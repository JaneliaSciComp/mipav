package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to call the histogram algorithm. This dialog will not be visible because it does not require user input at
 * this time. It was made a dialog object because it may in the future require user input and to be consistent with the
 * dialog/algorithm paradigm. In should be noted, that the algorithms are executed in their own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @deprecated
 * @see JFrameHistogram
 */
public class JDialogHistogramLUT extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1863683975229524849L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean colocalizationEMFrame = false;

    /** DOCUMENT ME! */
    private boolean colocalizationRegFrame = false;

    /** DOCUMENT ME! */
    private ModelImage imageA = null; // source image

    /** DOCUMENT ME! */
    private ModelImage imageB = null; // source image

    /** DOCUMENT ME! */
    private ModelLUT LUTa;

    /** DOCUMENT ME! */
    private ModelLUT LUTb;

    /** false = apply algorithm only to VOI regions. */
    private ViewJComponentRegistration regComponent = null;

    /** DOCUMENT ME! */
    private boolean regionFlag = true; // true = apply algorithm to the whole image

    /** DOCUMENT ME! */
    private ModelRGB RGBa;

    /** DOCUMENT ME! */
    private ModelRGB RGBb;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new histogram dialog. Does not actually display dialog - constructDialog is called from ViewJFrameImage
     * if there are VOI regions in the image. Otherwise, histogramLUT is called directly.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image A
     * @param  imB             Source image B (can be null)
     * @param  _LUTa           LUT associated with image A.
     * @param  _LUTb           LUT associated with image B (can be null).
     * @deprecated
     */
    public JDialogHistogramLUT(Frame theParentFrame, ModelImage imA, ModelImage imB, ModelLUT _LUTa, ModelLUT _LUTb) {
        super(theParentFrame, true);
        imageA = imA;
        imageB = imB;
        LUTa = _LUTa;
        LUTb = _LUTb;
    }

    /**
     * Creates new histogram dialog. Does not actually display dialog - constructDialog is called from ViewJFrameImage
     * if there are VOI regions in the image. Otherwise, histogramLUT is called directly.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image A
     * @param  imB             Source image B (can be null)
     * @param  _RGBa           RGB LUT associated with image A.
     * @param  _RGBb           RGB LUT associated with image B (can be null).
     * @deprecated
     */
    public JDialogHistogramLUT(Frame theParentFrame, ModelImage imA, ModelImage imB, ModelRGB _RGBa, ModelRGB _RGBb) {
        super(theParentFrame, true);
        imageA = imA;
        imageB = imB;

        if (_RGBa == null) {
            int[] RGBExtents = new int[2];

            RGBExtents[0] = 4;
            RGBExtents[1] = 256;

            RGBa = new ModelRGB(RGBExtents);
        } else {
            RGBa = _RGBa;
        }

        if (_RGBb == null) {
            int[] RGBExtents = new int[2];

            RGBExtents[0] = 4;
            RGBExtents[1] = 256;

            RGBb = new ModelRGB(RGBExtents);
        } else {
            RGBb = _RGBb;
        }
        addWindowListener(this);

    }

    /**
     * Creates new histogram dialog. This is called from ViewJFrameRegistration.
     *
     * @param  theParentFrame  Parent frame
     * @param  _regComponent   Registration component.
     * @param  imA             Source image A
     * @param  imB             DOCUMENT ME!
     * @param  _LUTa           Source image B (can be null)
     * @param  _LUTb           RGB LUT associated with image A.
     * @deprecated
     */
    public JDialogHistogramLUT(Frame theParentFrame, ViewJComponentRegistration _regComponent, ModelImage imA,
                               ModelImage imB, ModelLUT _LUTa, ModelLUT _LUTb) {
        super(theParentFrame, true);
        imageA = imA;
        imageB = imB;
        LUTa = _LUTa;
        LUTb = _LUTb;
        regComponent = _regComponent;
        addWindowListener(this);
    }

    /**
     * Creates new histogram dialog. This is called from ViewJFrameRegistration.
     *
     * @param  theParentFrame  Parent frame
     * @param  _regComponent   Registration component.
     * @param  imA             Source image A
     * @param  imB             Source image B (can be null)
     * @param  _RGBa           RGB LUT associated with image A.
     * @param  _RGBb           RGB LUT associated with image B (can be null).
     * @deprecated
     */
    public JDialogHistogramLUT(Frame theParentFrame, ViewJComponentRegistration _regComponent, ModelImage imA,
                               ModelImage imB, ModelRGB _RGBa, ModelRGB _RGBb) {
        this(theParentFrame, imA, imB, _RGBa, _RGBb);
        regComponent = _regComponent;
        addWindowListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {

            if (wholeImage.isSelected()) {
                regionFlag = true;
            } else if (VOIRegions.isSelected()) {
                regionFlag = false;
            }

            dispose();
            histogramLUT(regionFlag);
        } else if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) { }

    /**
     * Creates a dialog to choose if histogram should be over all of image or just VOI regions.
     */
    public void constructDialog() {
        setTitle("Histogram");

        JPanel imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Histogram"));
        getContentPane().add(imageVOIPanel);

        ButtonGroup imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        imageVOIPanel.add(wholeImage, gbc);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Makes a histogram LUT by calling ViewJFrameHistoLUT.
     *
     * @param  entireFlag  if <code>true</code> calculate histogram for entire image
     */
    public void histogramLUT(boolean entireFlag) {

        if (imageA.isColorImage() == false) {
            ViewJFrameHistoLUT histoLUTFrame;

            if (colocalizationRegFrame) {
                histoLUTFrame = new ViewJFrameHistoLUT((ViewJFrameColocalizationRegression) parentFrame, imageA, null,
                                                       LUTa, null, true);
            } else if (colocalizationEMFrame) {
                histoLUTFrame = new ViewJFrameHistoLUT((ViewJFrameColocalizationEM) parentFrame, imageA, null, LUTa,
                                                       null, true);
            } else if (regComponent == null) {
                histoLUTFrame = new ViewJFrameHistoLUT(parentFrame, imageA, imageB, LUTa, LUTb, entireFlag);
            } else {
                histoLUTFrame = new ViewJFrameHistoLUT(regComponent, imageA, imageB, LUTa, LUTb, entireFlag);
            }

            imageA.addImageDisplayListener(histoLUTFrame);

            if (imageB != null) {
                imageB.addImageDisplayListener(histoLUTFrame);
            }
        } else { // type ARGB

            ViewJFrameHistoRGB histoRGBFrame;

            if ((imageB != null) && (imageB.isColorImage() == false)) {
                MipavUtil.displayError("imageB must also be of type ARGB:JDialogHistogramLUT");

                return;
            }

            if (regComponent == null) {
                histoRGBFrame = new ViewJFrameHistoRGB(imageA, imageB, RGBa, RGBb, entireFlag);
            } else {
                histoRGBFrame = new ViewJFrameHistoRGB(regComponent, imageA, imageB, RGBa, RGBb, entireFlag);
            }

            imageA.addImageDisplayListener(histoRGBFrame);

            if (imageB != null) {
                imageB.addImageDisplayListener(histoRGBFrame);
            }
        }
    }

    /**
     * if true parentFrame is a ViewJFrameColocalizationEM.
     *
     * @param  colocalizationEMFrame  DOCUMENT ME!
     */
    public void setColocalizationEMFrame(boolean colocalizationEMFrame) {
        this.colocalizationEMFrame = colocalizationEMFrame;
    }

    /**
     * if true parentFrame is a ViewJFrameColocalizationRegression.
     *
     * @param  colocalizationRegFrame  DOCUMENT ME!
     */
    public void setColocalizationRegFrame(boolean colocalizationRegFrame) {
        this.colocalizationRegFrame = colocalizationRegFrame;
    }
    

    /**
     * Unchanged.
     *
     * @param  event  WindowEvent
     */
    public void windowActivated(WindowEvent event) { 
    	System.err.println("ruida");
    	/*
    	if ( ViewUserInterface.getReference().getActiveImageFrame().getActiveImage() != imageA ) {
    		ViewUserInterface.getReference().setActiveImageFrame();
    		ViewUserInterface.getReference().getActiveImageFrame().getControls().setActiveImage(ViewJFrameImage.IMAGE_A);
    	}
    	*/
    }

    /**
     * Unchanged.
     *
     * @param  event  WindowEvent
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  WindowEvent
     */
    public void windowClosing(WindowEvent event) {
        cancelFlag = true;
        dispose();
    }

    /**
     * Unchanged.
     *
     * @param  event  WindowEvent
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  WindowEvent
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event WindowEvent
     */
    public void windowIconified(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  WindowEvent
     */
    public void windowOpened(WindowEvent event) { }


}
