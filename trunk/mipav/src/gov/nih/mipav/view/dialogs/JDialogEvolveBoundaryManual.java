package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


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
    private float scaleY = 0;

    /** DOCUMENT ME! */
    private float scaleZ = 0;

    /** DOCUMENT ME! */
    private JRadioButton singleSlice;

    /** DOCUMENT ME! */
    private VOI srcVOI;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

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
        userInterface = ViewUserInterface.getReference();
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
        Object source = event.getSource();
    }
    
    private void evolveContour ( VOIBase srcContour )
    {

        if ( srcContour == null )
        {
            return;
        }
        VOIBase resultContour = srcContour.clone();

        Vector3f minPt = resultContour.getImageBoundingBox()[0];
        Vector3f maxPt = resultContour.getImageBoundingBox()[1];
        Vector3f minScanner = new Vector3f();
        Vector3f maxScanner = new Vector3f();
        MipavCoordinateSystems.fileToScanner(minPt, minScanner, image);
        MipavCoordinateSystems.fileToScanner(maxPt, maxScanner, image);
        float minX = Math.min( minScanner.X, maxScanner.X );
        float maxX = Math.max( minScanner.X, maxScanner.X );
        float minY = Math.min( minScanner.Y, maxScanner.Y );
        float maxY = Math.max( minScanner.Y, maxScanner.Y );
        float minZ = Math.min( minScanner.Z, maxScanner.Z );
        float maxZ = Math.max( minScanner.Z, maxScanner.Z );
        if ( boundaryDir == 1 )
        {
            scaleX *= -1;
        }
        float sX = ((maxX - minX) + scaleX ) / (maxX - minX);
        float sY = ((maxY - minY) + scaleX ) / (maxY - minY);
        float sZ = ((maxZ - minZ) + scaleX ) / (maxZ - minZ);
        Vector3f gcPt = resultContour.getGeometricCenter();
        float scale = Math.min( Math.abs(sX), Math.min( Math.abs(sY), Math.abs(sZ) ) );
        
        int size = resultContour.size();
        for (int i = 0; i < size; i++) {
            resultContour.elementAt(i).Sub(gcPt);
            resultContour.elementAt(i).Scale(scale, scale, 1);
            resultContour.elementAt(i).Add(gcPt);
        }
        resultContour.update();

        // The algorithm has completed and produced a new image to be displayed.
        short sID = (short)(image.getVOIs().getUniqueID());
        String kName = srcContour.getClass().getName();
        int index = kName.lastIndexOf('.') + 1;
        kName = kName.substring(index);
        VOI resultVOI = new VOI(sID, kName + "_" + sID, srcContour.getType(), -1 );
        resultVOI.importCurve( resultContour );

        if (removeOriginal) {
            resultVOI.setColor(voiColor);
            image.getVOIs().removeElementAt(groupNum);
        }

        image.registerVOI(resultVOI);
        srcContour.setActive(false);
        resultContour.setActive(false);
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


        JPanel imageVOIPanel = new JPanel(new GridLayout(3, 1));
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Evolve Boundary"));

        ButtonGroup imageVOIGroup = new ButtonGroup();
        singleSlice = new JRadioButton("Single slice", true);
        singleSlice.setFont(serif12);
        imageVOIGroup.add(singleSlice);
        imageVOIPanel.add(singleSlice);

        removeOriginalCheckBox = new JCheckBox("Replace Original Contour");
        removeOriginalCheckBox.setFont(serif12);
        removeOriginalCheckBox.setForeground(Color.black);
        removeOriginalCheckBox.setSelected(false);
        imageVOIPanel.add(removeOriginalCheckBox);


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
        gbc.gridy++;
        mainPanel.add(imageVOIPanel, gbc);

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
