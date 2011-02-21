package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). The algorithms are executed in their own
 * thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmGaussianBlur
 */
public class JDialogLevelSetDiffusion extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6530047904436344937L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    /** Flag indicating if slices should be blurred independently. */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int iters;

    /** DOCUMENT ME! */
    private float kValue;

    /** DOCUMENT ME! */
    private JLabel labelCorrected;

    /** DOCUMENT ME! */
    private JLabel labelGaussX;

    /** DOCUMENT ME! */
    private JLabel labelGaussY;

    /** DOCUMENT ME! */
    private JLabel labelGaussZ;

    /** DOCUMENT ME! */
    private JLabel labelIters;

    /** DOCUMENT ME! */
    private JLabel labelK;

    /** DOCUMENT ME! */
    private AlgorithmLevelSetDiffusion levelSetAlgo;

    /** DOCUMENT ME! */
    private float normFactor = 1; // normalization factor to adjust for resolution

    /** DOCUMENT ME! */
    private BitSet paintMask;

    /** DOCUMENT ME! */
    private JCheckBox resolutionCheckbox;

    /** DOCUMENT ME! */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private float scaleZ;

    /** DOCUMENT ME! */
    private JTextField textGaussX;

    /** DOCUMENT ME! */
    private JTextField textGaussY;

    /** DOCUMENT ME! */
    private JTextField textGaussZ;

    /** difference between x,y resolutions (in plane) and z resolution (between planes). */
    private JTextField textIters;

    /** DOCUMENT ME! */
    private JTextField textK;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for finding the level set.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogLevelSetDiffusion(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        userInterface = ViewUserInterface.getReference();
        image = im;

        ViewVOIVector VOIs = image.getVOIs();
        int nVOI = VOIs.size();

        if (nVOI == 0) {
            MipavUtil.displayError("Image must have a VOI for level set");

            return;
        }

        int i;

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }

        if (i == nVOI) {
            MipavUtil.displayError("The VOI must be a contour VOI");

            return;
        }

        init();
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
            tmpStr = textGaussX.getText();

            if (testParameter(tmpStr, 0.5, 5.0)) {
                scaleX = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussX.requestFocus();
                textGaussX.selectAll();

                return;
            }

            tmpStr = textGaussY.getText();

            if (testParameter(tmpStr, 0.5, 5.0)) {
                scaleY = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussY.requestFocus();
                textGaussY.selectAll();

                return;
            }

            tmpStr = textGaussZ.getText();

            if (testParameter(tmpStr, 0.0, 5.0)) {
                scaleZ = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussZ.requestFocus();
                textGaussZ.selectAll();

                return;
            }
            
            image25D = image25DCheckbox.isSelected();

            tmpStr = textIters.getText();

            if (testParameter(tmpStr, 1, 10000)) {
                iters = Integer.valueOf(tmpStr).intValue();
            } else {
                textIters.requestFocus();
                textIters.selectAll();

                return;
            }

            tmpStr = textK.getText();

            if (testParameter(tmpStr, 1, 100)) {
                kValue = Float.valueOf(tmpStr).floatValue();
            } else {
                textK.requestFocus();
                textK.selectAll();

                return;
            }

            // Apply normalization if requested!
            if (resolutionCheckbox.isSelected()) {
                scaleZ = scaleZ * normFactor;
            }

            if (image.getNDims() == 2) { // source image is 2D

                int[] destExtents = new int[2];
                destExtents[0] = image.getExtents()[0]; // X dim
                destExtents[1] = image.getExtents()[1]; // Y dim

                float[] sigmas = new float[2];
                sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
                sigmas[1] = scaleY;

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    levelSetAlgo = new AlgorithmLevelSetDiffusion(image, sigmas, iters, kValue, image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    levelSetAlgo.addListener(this);

                    
                    createProgressBar(image.getImageName(), levelSetAlgo);
                    
                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    // Start the thread as a low priority because we wish to still have user interface.
                    if (levelSetAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Diffusion: unable to allocate enough memory");

                    return;
                }
            } else if (image.getNDims() == 3) {
                int[] destExtents = new int[3];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2];

                float[] sigmas = new float[3];
                sigmas[0] = scaleX;
                sigmas[1] = scaleY;
                sigmas[2] = scaleZ; // normalized  - scaleZ * resolutionX/resolutionZ; !!!!!!!

                try {

                    // Make algorithm
                    levelSetAlgo = new AlgorithmLevelSetDiffusion(image, sigmas, iters, kValue, image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    levelSetAlgo.addListener(this);

                    createProgressBar(image.getImageName(), levelSetAlgo);
                    
                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (levelSetAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog diffusion: unable to allocate enough memory");

                    return;
                }
            }
        } else if (source == cancelButton) {
            dispose();
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        int i;

        if (algorithm instanceof AlgorithmLevelSetDiffusion) {

            if (levelSetAlgo.isCompleted()) {
                paintMask = ((ViewJFrameImage) parentFrame).getComponentImage().getPaintBitmap();

                for (i = 0; i < paintMask.size(); i++) {
                    paintMask.clear(i);
                }

                ((ViewJFrameImage) parentFrame).getComponentImage().setPaintMask(paintMask);

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            }
        }

        dispose();
    }

    /**
     * When the user clicks the mouse out of a text field, resets the neccessary variables.
     *
     * @param  event  event that triggers this function
     */
    public void focusLost(FocusEvent event) {
        Object source = event.getSource();
        float tempNum;

        if (source == textGaussZ) {

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(textGaussZ.getText()).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        }
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Changes scale based on resolution check box.
     *
     * @param  event  Event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        float tempNum;

        if (source == resolutionCheckbox) {

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(textGaussZ.getText()).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        } else if (source == image25DCheckbox) {
            if (image25DCheckbox.isSelected()) {
                resolutionCheckbox.setEnabled(false); 
                labelCorrected.setEnabled(false);
                labelGaussZ.setEnabled(false);
                textGaussZ.setEnabled(false);
            } else {
                resolutionCheckbox.setEnabled(true);
                labelCorrected.setEnabled(true);
                labelGaussZ.setEnabled(true);
                textGaussZ.setEnabled(true);
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Level Set Diffusion");
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel;
        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel scalePanel = new JPanel(new GridLayout(3, 2));
        scalePanel.setForeground(Color.black);
        scalePanel.setBorder(buildTitledBorder("Scale of the Gaussian"));
        mainPanel.add(scalePanel, gbc);

        labelGaussX = createLabel("X Dimension (0.5 - 5.0) ");
        scalePanel.add(labelGaussX);
        textGaussX = createTextField("0.5");
        scalePanel.add(textGaussX);

        labelGaussY = createLabel("Y Dimension (0.5 - 5.0) ");
        scalePanel.add(labelGaussY);
        textGaussY = createTextField("0.5");
        scalePanel.add(textGaussY);

        labelGaussZ = createLabel("Z Dimension (0.5 - 5.0) ");
        scalePanel.add(labelGaussZ);
        textGaussZ = createTextField("0.5");
        scalePanel.add(textGaussZ);

        gbc.gridx = 0;
        gbc.gridy = 1;

        JPanel resPanel = new JPanel(new BorderLayout());
        resPanel.setBorder(buildTitledBorder("Options"));
        resolutionCheckbox = new JCheckBox("Use image resolutions to normalize Z scale.");
        resolutionCheckbox.setFont(serif12);
        resPanel.add(resolutionCheckbox, BorderLayout.NORTH);
        resolutionCheckbox.setSelected(true);
        
        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        image25DCheckbox.setFont(serif12);
        resPanel.add(image25DCheckbox, BorderLayout.SOUTH);
        image25DCheckbox.setSelected(false);

        if (image.getNDims() >= 3) { // if the source image is 3D then allow
            resolutionCheckbox.setEnabled(true); // the user to indicate if it wishes to
            resolutionCheckbox.addItemListener(this); // use the correction factor
            image25DCheckbox.setEnabled(true);
            image25DCheckbox.addItemListener(this);
            textGaussZ.addFocusListener(this);
            textGaussZ.setEnabled(true);
        } else {
            resolutionCheckbox.setEnabled(false); // Image is only 2D, thus this checkbox
            image25DCheckbox.setEnabled(false);
            labelGaussZ.setEnabled(false); // is not relevent
            textGaussZ.setEnabled(false);
        }

        if (image.getNDims() >= 3) { // Source image is 3D, thus show correction factor

            int index = image.getExtents()[2] / 2;
            float xRes = image.getFileInfo(index).getResolutions()[0];
            float zRes = image.getFileInfo(index).getResolutions()[2];
            normFactor = xRes / zRes; // Calculate correction factor
            labelCorrected = new JLabel("      Corrected scale = " +
                                        String.valueOf(normFactor * Float.valueOf(textGaussZ.getText()).floatValue()));
            labelCorrected.setForeground(Color.black);
            labelCorrected.setFont(serif12);
            resPanel.add(labelCorrected, BorderLayout.CENTER);
        }

        mainPanel.add(resPanel, gbc);

        JPanel paramPanel = new JPanel(new GridLayout(2, 2));
        paramPanel.setBorder(buildTitledBorder("Algorithm parameters"));

        labelIters = createLabel("Iterations (1-10000)");

        textIters = createTextField("200");

        labelK = createLabel("k ( smaller  k  slows diffusion )");
        textK = createTextField("5");

        paramPanel.add(labelIters);
        paramPanel.add(textIters);
        paramPanel.add(labelK);
        paramPanel.add(textK);

        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(paramPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

    }

}
