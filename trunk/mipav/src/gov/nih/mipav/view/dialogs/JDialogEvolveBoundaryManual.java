package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlurSep;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;
import java.util.BitSet;

import javax.swing.*;


public class JDialogEvolveBoundaryManual extends JDialogBase {

    /**  */
    private static final long serialVersionUID = -2170631279185094655L;
    
    private JTextField textChangeX;
    
    /** DOCUMENT ME! */
    private int boundaryDir = AlgorithmSnake.OUT_DIR;

    /** DOCUMENT ME! */
    private JComboBox boundaryDirBox;

    /** DOCUMENT ME! */
    private int groupNum;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean removeOriginal;

    /** DOCUMENT ME! */
    private JCheckBox removeOriginalCheckBox;

    /** DOCUMENT ME! */
    private float scaleX = 0;

    /** DOCUMENT ME! */
    private VOI srcVOI;

    /** DOCUMENT ME! */
    private Color voiColor;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image.
     */
    public JDialogEvolveBoundaryManual(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        VOIs = im.getVOIs();

        int nVOI;

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (groupNum = 0; groupNum < nVOI; groupNum++) {

            if ((VOIs.VOIAt(groupNum).isActive() == true) && (VOIs.VOIAt(groupNum).getCurveType() == VOI.CONTOUR)) {
                break;
            }
        }

        if (groupNum == nVOI) {
            MipavUtil.displayError("VOI must be selected");
            dispose();

            return;
        }

        voiColor = VOIs.VOIAt(groupNum).getColor();
        srcVOI = VOIs.VOIAt(groupNum);
        image = im;
        init();
    }
    

    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image.
     */
    public JDialogEvolveBoundaryManual(Frame theParentFrame, ModelImage im, boolean separateThread) {
    	this(theParentFrame, im);
        setSeparateThread(separateThread);   	
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String tmpStr;

        if (source == OKButton) {

            removeOriginal = removeOriginalCheckBox.isSelected();

            tmpStr = textChangeX.getText();

            if (testParameter(tmpStr, 1, 50)) {
                scaleX = Float.valueOf(tmpStr).floatValue();
            } else {
                textChangeX.requestFocus();
                textChangeX.selectAll();

                return;
            }

            boundaryDir = boundaryDirBox.getSelectedIndex();

            VOIBaseVector curves = srcVOI.getCurves();
            VOIBase srcContour = null;
            for ( int i = 0; i < curves.size(); i++ )
            {
                if ( curves.elementAt(i).isActive() )
                {
                    srcContour = curves.elementAt(i);
                    evolveContour( srcContour );
                }
            }

            // Update frame
            ((ViewJFrameBase) parentFrame).updateImages(true);
            if ( voiManager != null )
            {
                voiManager.algorithmPerformed();
            }
            dispose();
        } else if (source == cancelButton) {
            dispose();
        }
        else if (source == helpButton) {
            MipavUtil.showHelp("10506");
        }
    }

    public void focusLost(FocusEvent event) {
        super.focusLost(event);
    }

    /**
     * @param  event  Event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        
    }
    
    private void evolveContour ( VOIBase srcContour )
    {
        if ( srcContour == null || (srcContour.size() == 0) )
        {
            return;
        }
        int slice = (int)srcContour.elementAt(0).Z;
        int size = image.getDataSize();
        if ( image.isColorImage() )
        {
            size /= 4;
        }
        BitSet mask = new BitSet( size );
        int[] extents = image.getExtents();
        srcContour.setMaskSlice( mask, extents[0], false, VOI.ADDITIVE ); 
        
        int[] extentsSlice = new int[]{extents[0],extents[1]};
        ModelImage maskImage = new ModelImage(ModelStorageBase.BOOLEAN, extentsSlice, "Binary Image");
        for (int i = 0; i < size; i++) {
            if ( boundaryDir == 0 )
            {
                maskImage.set(i, mask.get(i) );
            }
            else
            {
                maskImage.set(i, !mask.get(i) );
            }
        }

        float[] resolutions = image.getResolutions(slice);
        float sigmaX = scaleX * resolutions[0] / 4;
        float sigmaY = scaleX * resolutions[1] / 4;
        System.err.println( sigmaX + " " + sigmaY );

        // Make algorithm
        float[] sigmas = new float[]{sigmaX,sigmaY,0};
        AlgorithmGaussianBlurSep gaussianBlurSepAlgo = new AlgorithmGaussianBlurSep(maskImage, sigmas, true, true);
        gaussianBlurSepAlgo.run();
        

        final String name = JDialogBase.makeImageName(image.getImageName(), "_gblur");
        ModelImage resultImage = (ModelImage) maskImage.clone();
        resultImage.setImageName(name);

        if ( (resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
            // For 2D Dicom set secondary capture tags only for fileinfo(0)
            if (resultImage.getExtents().length == 2) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
            } else {
                for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                    ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                }
            }
        }
        try {
            resultImage.importData(0, gaussianBlurSepAlgo.getResultBuffer(), true);
        } catch (final IOException e) {
            resultImage.disposeLocal();
            MipavUtil.displayError("Algorithm Gausssian Blur importData: Image(s) Locked.");
            return;
        }
        // The algorithm has completed and produced a new image to
        // be displayed.
        if (resultImage.isColorImage()) {
            JDialogBase.updateFileInfo(image, resultImage);
        }
        resultImage.clearMask();
        
        if ( boundaryDir == 1 )
        {
            //Subtract the inverted and expanded mask from the original.
            for ( int i = 0; i < size; i++ )
            {
                if ( mask.get(i) == false )
                {
                    resultImage.set(i, false);
                }
                else if ( mask.get(i) && resultImage.getBoolean(i) )
                {
                    resultImage.set(i, false);
                }
                else if ( mask.get(i) )
                {
                    resultImage.set(i, true);
                }
            }
        }
        
        final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(resultImage);
        VOIExtractionAlgo.run();
        VOIVector resultVOIs = resultImage.getVOIs();
        for ( int i = 0; i < resultVOIs.size(); i++ )
        {
            // The algorithm has completed and produced a new image to be displayed.
            short sID = (short)(image.getVOIs().getUniqueID());
            String kName = srcContour.getClass().getName();
            int index = kName.lastIndexOf('.') + 1;
            kName = kName.substring(index);
            VOI resultVOI = new VOI(sID, kName + "_" + sID, srcContour.getType(), -1 );
            for ( int j = 0; j < resultVOIs.elementAt(i).getCurves().size(); j++ )
            {
                VOIBase kCurve = resultVOIs.elementAt(i).getCurves().elementAt(j);
                for ( int k = 0; k < kCurve.size(); k++ )
                {
                    kCurve.elementAt(k).Z = slice;
                }
                kCurve.update();
                resultVOI.importCurve( kCurve );
            }
            if (removeOriginal) {
                resultVOI.setColor(voiColor);
                image.getVOIs().removeElementAt(groupNum);
            }
            image.registerVOI( resultVOI );
        }
        resultImage.disposeLocal();
        maskImage.disposeLocal();
        mask = null;        
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Evolve Boundary");

        JPanel scalePanel = new JPanel(new GridLayout(4, 2));
        scalePanel.setForeground(Color.black);
        scalePanel.setBorder(buildTitledBorder("Contour Change"));

        int[] aiUnits = image.getUnitsOfMeasure();
        JLabel labelChangeX = new JLabel("Enter size change in " + FileInfoBase.getUnitsOfMeasureAbbrevStr(aiUnits[0]) + " units ");
        labelChangeX.setForeground(Color.black);
        labelChangeX.setFont(serif12);
        scalePanel.add(labelChangeX);

        textChangeX = new JTextField();
        textChangeX.setText("2.0");
        textChangeX.setFont(serif12);
        scalePanel.add(textChangeX);
        
        JLabel labelBoundaryDir = new JLabel("Move boundary "); // make & set a label
        labelBoundaryDir.setForeground(Color.black);
        labelBoundaryDir.setFont(serif12);
        scalePanel.add(labelBoundaryDir); // add kernel label

        boundaryDirBox = new JComboBox();
        boundaryDirBox.setFont(serif12);
        boundaryDirBox.setBackground(Color.white);
        boundaryDirBox.addItem("Outward");
        boundaryDirBox.addItem("Inward");
        scalePanel.add(boundaryDirBox);

        //JPanel imageVOIPanel = new JPanel(new GridLayout(1, 1));
        //imageVOIPanel.setForeground(Color.black);
        //imageVOIPanel.setBorder(buildTitledBorder("Evolve Boundary"));

        //ButtonGroup imageVOIGroup = new ButtonGroup();
        //singleSlice = new JRadioButton("Single slice", true);
        //singleSlice.setFont(serif12);
        //imageVOIGroup.add(singleSlice);
        //imageVOIPanel.add(singleSlice);

        removeOriginalCheckBox = new JCheckBox("Replace Original Contour");
        removeOriginalCheckBox.setFont(serif12);
        removeOriginalCheckBox.setForeground(Color.black);
        removeOriginalCheckBox.setSelected(false);
        scalePanel.add(removeOriginalCheckBox);


        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(scalePanel, gbc);
        //gbc.gridy++;
        //mainPanel.add(imageVOIPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }
}
