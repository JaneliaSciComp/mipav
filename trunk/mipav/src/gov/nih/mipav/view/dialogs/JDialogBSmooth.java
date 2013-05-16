package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm for smoothing VOIs. The user can choose whether or not to remove
 * the original selected voi and whether or not to apply trimming to remove nearly collinear points. The user can choose
 * the number of interpolation points. In 2D all selected curves of a selected voi are smoothed. In 3D all selected
 * curves in all slices of a selected voi are smoothed. The original zslice information is only used so that the default
 * number of interpolated points comes from a contour in zslice. If the original voi is not removed, the new voi has a
 * new color. If the original voi is removed, the new voi has the same color.
 */
public class JDialogBSmooth extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2582311598077394632L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int defaultPts;

    /** DOCUMENT ME! */
    private int elementNum;

    /** DOCUMENT ME! */
    private int groupNum;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int nPts;

    /** DOCUMENT ME! */
    private boolean removeOriginal;

    /** DOCUMENT ME! */
    private JCheckBox removeOriginalCheckBox;

    /** DOCUMENT ME! */
    private AlgorithmBSmooth smoothAlgo;

    /** DOCUMENT ME! */
    private JTextField textInterpNPts;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private boolean trim;

    /** DOCUMENT ME! */
    private JCheckBox trimCheckBox;

    /** DOCUMENT ME! */
    private Color voiColor;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;
    
    private VOIManagerInterface voiManager;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     * @param  _zSlice         Z slice of image.
     */
    public JDialogBSmooth(Frame theParentFrame, ModelImage im, int _zSlice) {
        super(theParentFrame, false);

        int i;
        Vector<VOIBase> contours;
        int nVOI, nContours;
        float[] xPoints = null;
        float[] yPoints = null;
        float[] zPoints = null;

        image = im;
        VOIs = image.getVOIs();

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
        contours = VOIs.VOIAt(groupNum).getCurves();
        nContours = contours.size();

        for (elementNum = 0; elementNum < nContours; elementNum++) {

            if (((VOIContour) (contours.elementAt(elementNum))).isActive()) {
                break;
            }
        }

        if (elementNum == nContours) {

            // Don't think this should happen under normal operations
            dispose();

            return;
        }

        VOIBase activeContour = contours.elementAt(elementNum);
        int nPoints = activeContour.size();

        xPoints = new float[nPoints + 5];
        yPoints = new float[nPoints + 5];
        zPoints = new float[nPoints + 5];

        xPoints[0] = activeContour.elementAt(nPoints - 2).X;
        yPoints[0] = activeContour.elementAt(nPoints - 2).Y;
        zPoints[0] = activeContour.elementAt(nPoints - 2).Z;

        xPoints[1] = activeContour.elementAt(nPoints - 1).X;
        yPoints[1] = activeContour.elementAt(nPoints - 1).Y;
        zPoints[1] = activeContour.elementAt(nPoints - 1).Z;

        for (i = 0; i < nPoints; i++) {
            xPoints[i + 2] = activeContour.elementAt(i).X;
            yPoints[i + 2] = activeContour.elementAt(i).Y;
            zPoints[i + 2] = activeContour.elementAt(i).Z;
        }

        xPoints[nPoints + 2] = activeContour.elementAt(0).X;
        yPoints[nPoints + 2] = activeContour.elementAt(0).Y;
        zPoints[nPoints + 2] = activeContour.elementAt(0).Z;
        
        xPoints[nPoints + 3] = activeContour.elementAt(1).X;
        yPoints[nPoints + 3] = activeContour.elementAt(1).Y;
        zPoints[nPoints + 3] = activeContour.elementAt(1).Z;
        
        xPoints[nPoints + 4] = activeContour.elementAt(2).X;
        yPoints[nPoints + 4] = activeContour.elementAt(2).Y;
        zPoints[nPoints + 4] = activeContour.elementAt(2).Z;
        
        AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints, zPoints);
        defaultPts = Math.round(arcLength.getTotalArcLength() / 3);

        init();
    }
    
    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     * @param  _zSlice         Z slice of image.
     */
    public JDialogBSmooth(Frame theParentFrame, VOIManagerInterface voiManager, ModelImage im, int _zSlice) {
    	this(theParentFrame,im,_zSlice);
    	this.voiManager = voiManager;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String tmpStr;
        int i;

        if (source == OKButton) {

            removeOriginal = removeOriginalCheckBox.isSelected();

            trim = trimCheckBox.isSelected();

            tmpStr = textInterpNPts.getText();

            if (testParameter(tmpStr, 3.0, 1000000.0)) {
                nPts = Integer.valueOf(tmpStr).intValue();
            } else {
                textInterpNPts.requestFocus();
                textInterpNPts.selectAll();

                return;
            }

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                smoothAlgo = new AlgorithmBSmooth(image, VOIs.VOIAt(groupNum), nPts, trim);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                smoothAlgo.addListener(this);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (i = 0; i < imageFrames.size(); i++) {
                	if ( imageFrames.elementAt(i) instanceof ViewJFrameBase )
                	{
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    ((ViewJFrameBase) parentFrame).getUserInterface().unregisterFrame((Frame) (imageFrames.elementAt(i)));
                	}
                }

                // Start the thread as a low priority because we wish to still have user interface.
                if (smoothAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

                return;
            }

        } else if (source == cancelButton) {
            dispose();
        }
        else if (source == helpButton) {
            //MipavUtil.showHelp("Smooth01");
            MipavUtil.showWebHelp("Modifying_Contours#Smooth_VOI");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * algorithmPerformed - this method is required if the AlgorithmPerformed interface is implemented. It is called by
     * the algorithms when it has completed or failed to to complete, so that the dialog can be display the result image
     * and/or clean up.
     *
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        VOI resultVOI;
        int element;
        Vector<VOIBase> contours;
        int nContours;

        // ViewJFrameImage imageFrame = null;
        if (algorithm instanceof AlgorithmBSmooth) {

            if (smoothAlgo.isCompleted() == true) {

                // The algorithm has completed and produced a
                resultVOI = smoothAlgo.getResultVOI();

                if (removeOriginal) {
                    resultVOI.setColor(voiColor);
                    resultVOI.setAllActive(true);
                    contours = VOIs.VOIAt(groupNum).getCurves();

                    int resultIndex = 0;
                    nContours = contours.size();
                    for (element = nContours - 1; element >= 0; element--) {

                        if (((VOIContour) (contours.elementAt(element))).isActive()) {
                            VOIs.VOIAt(groupNum).removeCurve(contours.elementAt(element));

                            VOIs.VOIAt(groupNum).importCurve((VOIContour) resultVOI.getCurves().elementAt(resultIndex++));
                        }
                    }

                } else {
                    image.registerVOI(resultVOI);
                    // System.err.println("would have registered the new one here");
                }
            }

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
            	if ( imageFrames.elementAt(i) instanceof ViewJFrameBase )
            	{
            		((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
            		((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);
            		((ViewJFrameBase) parentFrame).getUserInterface().registerFrame((Frame) (imageFrames.elementAt(i)));
            	}
            }
        }

        // Update frame
        if ( voiManager != null )
        {
        	voiManager.updateDisplay();
        }
        dispose();
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Smooth VOI");

        JPanel imageVOIPanel = new JPanel(new GridLayout(2, 1));
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("VOI Options"));

        removeOriginalCheckBox = new JCheckBox("Replace Original Contour");
        removeOriginalCheckBox.setFont(serif12);
        removeOriginalCheckBox.setForeground(Color.black);
        removeOriginalCheckBox.setSelected(false);
        imageVOIPanel.add(removeOriginalCheckBox);

        trimCheckBox = new JCheckBox("Trim Collinear points");
        trimCheckBox.setFont(serif12);
        trimCheckBox.setForeground(Color.black);
        trimCheckBox.setSelected(false);
        imageVOIPanel.add(trimCheckBox);

        JPanel paramPanel = new JPanel(new GridLayout(1, 2));
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Algorithm parameters"));

        JLabel labelInterpNPts = new JLabel("Number of interpolation points ");
        labelInterpNPts.setForeground(Color.black);
        labelInterpNPts.setFont(serif12);
        paramPanel.add(labelInterpNPts);

        textInterpNPts = new JTextField();
        textInterpNPts.setText(String.valueOf(defaultPts));
        textInterpNPts.setFont(serif12);
        paramPanel.add(textInterpNPts);

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
        mainPanel.add(imageVOIPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(paramPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

}
