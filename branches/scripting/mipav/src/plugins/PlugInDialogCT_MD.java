import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * JDialogBase class.
 *
 * <p>Note:</p>
 *
 * @version  July 12, 2002
 * @author   DOCUMENT ME!
 * @see      JDialogBase
 * @see      JDialogMedian
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogCT_MD.java $ $Revision: 10 $ $Date: 1/25/06 4:59p $</p>
 */
public class PlugInDialogCT_MD extends JDialogBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float correctionVal;

    /** DOCUMENT ME! */
    private PlugInAlgorithmCT_MD ctSegAlgo = null;

    /** DOCUMENT ME! */
    private int fatHVal;

    /** DOCUMENT ME! */
    private JTextField fatHValTF;

    /** DOCUMENT ME! */
    private int fatLVal;

    /** DOCUMENT ME! */
    private JTextField fatLValTF;

    /** DOCUMENT ME! */
    private int hdmHVal;

    /** DOCUMENT ME! */
    private JTextField hdmHValTF;

    /** DOCUMENT ME! */
    private int hdmLVal;

    /** DOCUMENT ME! */
    private JTextField hdmLValTF;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int ldmHVal;

    /** DOCUMENT ME! */
    private JTextField ldmHValTF;

    /** DOCUMENT ME! */
    private int ldmLVal;

    /** DOCUMENT ME! */
    private JTextField ldmLValTF;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for Median filtering using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogCT_MD(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);

        if ((im.getType() == ModelImage.BOOLEAN) || im.isColorImage()) {
            MipavUtil.displayError("Source Image must NOT be Boolean or Color");
            dispose();

            return;
        }

        image = im;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public PlugInDialogCT_MD(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;

        if ((im.getType() == ModelImage.BOOLEAN) || im.isColorImage()) {
            MipavUtil.displayError("Source Image must NOT be Boolean or Color");
            dispose();

            return;
        }

        image = im;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
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
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof PlugInAlgorithmCT_MD) {
            image.clearMask();

            if ((ctSegAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {
                    // resultImage.setImageName("Median: "+image.getImageName());

                    int[] dimExtentsLUT = new int[2];
                    dimExtentsLUT[0] = 4;
                    dimExtentsLUT[1] = 256;

                    ModelLUT LUTa = new ModelLUT(ModelLUT.COOLHOT, 256, dimExtentsLUT);
                    new ViewJFrameImage(resultImage, LUTa, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
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
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }
        }

        if (ctSegAlgo.isCompleted() == true) {

            if (userInterface.isScriptRecording()) {
                userInterface.getScriptDialog().append("Flow " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " " + correctionVal + "\n");
            }
        }

        dispose();

    } // end AlgorithmPerformed()

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }


    /**
     * Accessor that sets the correction value.
     *
     * @param  num  Value to set iterations to (should be between 1 and 20).
     */
    public void setCorrectionValue(float num) {
        correctionVal = num;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_CTseg");

        // stuff to do when working on 2-D images.
        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            try {

                // Make result image of Ubyte type
                resultImage = new ModelImage(ModelStorageBase.UBYTE, destExtents, name, userInterface);

                // ctSegAlgo = new PlugInAlgorithmFlowWrapFix(resultImage, image, iters, kernelSize, kernelShape,
                // stdDev, regionFlag);
                ctSegAlgo = new PlugInAlgorithmCT_MD(resultImage, image);

                System.out.println("Dialog fatL = " + fatLVal + " fatH = " + fatHVal);
                ctSegAlgo.fatL = fatLVal;
                ctSegAlgo.fatH = fatHVal;
                ctSegAlgo.ldmL = ldmLVal;
                ctSegAlgo.ldmH = ldmHVal;
                ctSegAlgo.hdmL = hdmLVal;
                ctSegAlgo.hdmH = hdmHVal;


                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                ctSegAlgo.addListener(this);
                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (ctSegAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    ctSegAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog median: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            try {

                // Make result image of float type
                resultImage = new ModelImage(ModelStorageBase.UBYTE, destExtents, name, userInterface);

                ctSegAlgo = new PlugInAlgorithmCT_MD(resultImage, image);
                ctSegAlgo.fatL = fatLVal;
                ctSegAlgo.fatH = fatHVal;
                ctSegAlgo.ldmL = ldmLVal;
                ctSegAlgo.ldmH = ldmHVal;
                ctSegAlgo.hdmL = hdmLVal;
                ctSegAlgo.hdmH = hdmHVal;

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                ctSegAlgo.addListener(this);
                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (ctSegAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    ctSegAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog median: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                return;
            }
        }
    } // end callAlgorithm()

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("CT_segmentation");

        JPanel inputPanel = new JPanel(new GridLayout(3, 3));
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Input parameters"));

        JLabel labelFat = new JLabel("Fat thresholds: ");
        labelFat.setForeground(Color.black);
        labelFat.setFont(serif12);
        inputPanel.add(labelFat);

        fatLValTF = new JTextField();
        fatLValTF.setText("-190");
        fatLValTF.setFont(serif12);
        inputPanel.add(fatLValTF);

        fatHValTF = new JTextField();
        fatHValTF.setText("-30");
        fatHValTF.setFont(serif12);
        inputPanel.add(fatHValTF);


        JLabel labelLDM = new JLabel("Low density muscle thresholds: ");
        labelLDM.setForeground(Color.black);
        labelLDM.setFont(serif12);
        inputPanel.add(labelLDM);

        ldmLValTF = new JTextField();
        ldmLValTF.setText("0");
        ldmLValTF.setFont(serif12);
        inputPanel.add(ldmLValTF);

        ldmHValTF = new JTextField();
        ldmHValTF.setText("30");
        ldmHValTF.setFont(serif12);
        inputPanel.add(ldmHValTF);

        JLabel labelHDM = new JLabel("High density muscle thresholds: ");
        labelHDM.setForeground(Color.black);
        labelHDM.setFont(serif12);
        inputPanel.add(labelHDM);

        hdmLValTF = new JTextField();
        hdmLValTF.setText("31");
        hdmLValTF.setFont(serif12);
        inputPanel.add(hdmLValTF);

        hdmHValTF = new JTextField();
        hdmHValTF.setText("100");
        hdmHValTF.setFont(serif12);
        inputPanel.add(hdmHValTF);

        getContentPane().add(inputPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()


    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;


        // verify iteration is within bounds
        tmpStr = fatLValTF.getText();

        if (testParameter(tmpStr, -4000, 4000)) {
            fatLVal = Integer.valueOf(tmpStr).intValue();
        } else {
            fatLValTF.requestFocus();
            fatLValTF.selectAll();

            return false;
        }

        tmpStr = fatHValTF.getText();

        if (testParameter(tmpStr, -4000, 4000)) {
            fatHVal = Integer.valueOf(tmpStr).intValue();
        } else {
            fatHValTF.requestFocus();
            fatHValTF.selectAll();

            return false;
        }

        tmpStr = ldmLValTF.getText();

        if (testParameter(tmpStr, -4000, 4000)) {
            ldmLVal = Integer.valueOf(tmpStr).intValue();
        } else {
            ldmLValTF.requestFocus();
            ldmLValTF.selectAll();

            return false;
        }

        tmpStr = ldmHValTF.getText();

        if (testParameter(tmpStr, -4000, 4000)) {
            ldmHVal = Integer.valueOf(tmpStr).intValue();
        } else {
            ldmHValTF.requestFocus();
            ldmHValTF.selectAll();

            return false;
        }

        tmpStr = hdmLValTF.getText();

        if (testParameter(tmpStr, -4000, 4000)) {
            hdmLVal = Integer.valueOf(tmpStr).intValue();
        } else {
            hdmLValTF.requestFocus();
            hdmLValTF.selectAll();

            return false;
        }

        tmpStr = hdmHValTF.getText();

        if (testParameter(tmpStr, -4000, 4000)) {
            hdmHVal = Integer.valueOf(tmpStr).intValue();
        } else {
            hdmHValTF.requestFocus();
            hdmHValTF.selectAll();

            return false;
        }

        return true;
    } // end setVariables()

}
