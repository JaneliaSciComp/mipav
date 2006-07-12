import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

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
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogOCT.java $ $Revision: 2 $ $Date: 1/25/06 4:59p $</p>
 */
public class PlugInDialogOCT extends JDialogBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JTextField noiseTF;

    /** DOCUMENT ME! */
    private int noiseVal;

    /** DOCUMENT ME! */
    private PlugInAlgorithmOCT thicknessAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for Median filtering using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogOCT(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);

        if ((im.getType() == ModelImage.BOOLEAN) || im.isColorImage()) {
            MipavUtil.displayError("Source Image must NOT be Boolean or Color");
            dispose();

            return;
        }

        image = im;
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public PlugInDialogOCT(ViewUserInterface UI, ModelImage im) {
        super();

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

        if (algorithm instanceof PlugInAlgorithmOCT) {
            image.clearMask();

            if (thicknessAlgo.isCompleted() == true) {
                // The algorithm has completed and produced a new image to be
                // displayed.

            }
        }

        // if (thicknessAlgo.isCompleted() == true) {
        // if (userInterface.isScriptRecording()) {
        // userInterface.getScriptDialog().append("Flow " +
        // userInterface.getScriptDialog().getVar(image.getImageName()) + " "
        // + correctionVal + "\n");
        // }
        // }
        dispose();

    } // end AlgorithmPerformed()

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        // stuff to do when working on 2-D images.
        if (image.getNDims() == 2) { // source image is 2D

            try {
                thicknessAlgo = new PlugInAlgorithmOCT((ModelImage) (image.clone()), noiseVal);

                // This is very important. Adding this object as a listener
                // allows the algorithm to
                // notify this object when it has completed or failed. See
                // algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed
                // interface
                thicknessAlgo.addListener(this);
                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to
                    // still have user interface work fast.
                    if (thicknessAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    thicknessAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Muanza: unable to allocate enough memory");

                return;
            }
        }

    } // end callAlgorithm()

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Calculate thickness");

        JPanel inputPanel = new JPanel(new GridLayout(3, 3));
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Input parameters"));

        JLabel labelNoise = new JLabel("Noise length: ");
        labelNoise.setForeground(Color.black);
        labelNoise.setFont(serif12);
        inputPanel.add(labelNoise);

        noiseTF = new JTextField();
        noiseTF.setText("1");
        noiseTF.setFont(serif12);
        inputPanel.add(noiseTF);

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
        tmpStr = noiseTF.getText();

        if (testParameter(tmpStr, 1, 50)) {
            noiseVal = Integer.valueOf(tmpStr).intValue();
        } else {
            noiseTF.requestFocus();
            noiseTF.selectAll();

            return false;
        }

        return true;
    } // end setVariables()

}
