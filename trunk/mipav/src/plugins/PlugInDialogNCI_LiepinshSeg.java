import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class PlugInDialogNCI_LiepinshSeg extends JDialogBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JTextField blueChannelInput;

    /** DOCUMENT ME! */
    private float blueChannelValue;

    /** DOCUMENT ME! */
    private PlugInAlgorithmNCI_LiepinshSeg eyeAlgo;

    /** DOCUMENT ME! */
    private ModelImage imageA = null; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Sets variables needed to call algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image
     */
    public PlugInDialogNCI_LiepinshSeg(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);

        if (imA.getVOIs().size() < 1) {
            MipavUtil.displayError("Image must have VOI for masking");

            return;
        }

        imageA = imA;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        imageA.clearMask();

        if (algorithm instanceof PlugInAlgorithmNCI_LiepinshSeg) {

            if ((eyeAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(imageA, resultImage);

                try {

                    // resultImage.setImageName("Compressed image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }
        }

        // Update frame
        if (parentFrame != null) {
            ((ViewJFrameBase) parentFrame).updateImages(true);
        }

        if (eyeAlgo.isCompleted() == true) {

            if (userInterface.isScriptRecording()) {
                userInterface.getScriptDialog().append("NCI " +
                                                       userInterface.getScriptDialog().getVar(imageA.getImageName()) +
                                                       " ");
                userInterface.getScriptDialog().putVar(resultImage.getImageName());
                userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                       "\n");
            }
        }

    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {

            // if (imageA.getType() == ModelStorageBase.ARGB) {
            resultImage = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), (imageA.getImageName() + "NCI-SEG"),
                                         userInterface);

            // get some important information from imageA and put it in
            // the result image
            resultImage.copyFileTypeInfo(imageA);

            // Make algorithm
            eyeAlgo = new PlugInAlgorithmNCI_LiepinshSeg(resultImage, imageA);
            eyeAlgo.setBlueChannelThreshold(blueChannelValue);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            eyeAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (eyeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                eyeAlgo.setActiveImage(isActiveImage);
                eyeAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog RGB to Gray: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Accessor that returns the result image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("NCI-Liepinsh");
        getContentPane().setLayout(new BorderLayout());

        JPanel blueChannelPanel = new JPanel();

        // make border
        // blueChannelPanel.setBorder(buildTitledBorder(""));
        getContentPane().add(blueChannelPanel, BorderLayout.CENTER);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        blueChannelPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;

        blueChannelPanel.add(Box.createHorizontalStrut(10));

        JLabel blueLabel = new JLabel("Blue Channel Threshold:");
        blueLabel.setFont(serif12);
        blueLabel.setForeground(Color.black);
        blueLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(blueLabel, gbc);
        blueChannelPanel.add(blueLabel);
        blueChannelPanel.add(Box.createHorizontalStrut(10));

        blueChannelInput = new JTextField("25", 4);
        blueChannelInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(blueChannelInput, false);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(blueChannelInput, gbc);
        blueChannelPanel.add(blueChannelInput);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        blueChannelValue = Float.parseFloat(blueChannelInput.getText());

        return true;
    }
}
