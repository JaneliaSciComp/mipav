package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Creates the dialog to pad blank images to an active image. Dialog asks the user how to pad the blank images, such as
 * padding to the front or back of the image, half to front and half to back of the image. It gives options to remove or
 * to cancel.
 *
 * @author  Ruida Cheng
 */
public class JDialogPadImages extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4451162492289524768L;

    /** DOCUMENT ME! */
    public static final int PAD_FRONT = 0;

    /** DOCUMENT ME! */
    public static final int PAD_BACK = 1;

    /** DOCUMENT ME! */
    public static final int PAD_HALF = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private int nSlices; // number of slices in image

    /** DOCUMENT ME! */
    private int paddedSlices; // number of slices in padded image

    /** DOCUMENT ME! */
    private ButtonGroup paddingGroup;

    /** DOCUMENT ME! */
    private int padMode = PAD_FRONT;

    /** DOCUMENT ME! */
    private AlgorithmPadWithSlices padSlicesAlgo;

    /** DOCUMENT ME! */
    private JRadioButton padToBack;

    /** DOCUMENT ME! */
    private JRadioButton padToFront;

    /** DOCUMENT ME! */
    private JRadioButton padToHalf;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogPadImages() { }

    /**
     * Creates new dialog for removing slices.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogPadImages(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im; // set the image from the arguments to an image in this class
        userInterface = ViewUserInterface.getReference();
        nSlices = image.getExtents()[2];
        paddedSlices = dimPowerOfTwo(nSlices);

        if (nSlices == paddedSlices) {
            MipavUtil.displayError(nSlices + " slices is already a power of 2");

            return;
        }

        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Pad")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("U4041");
        } else if (command.equals("Script")) {
            callAlgorithm();
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

        if (algorithm instanceof AlgorithmPadWithSlices) {

            if (displayLoc == NEW) {

                if ((padSlicesAlgo.isCompleted() == true) && (resultImage != null)) {

                    // The algorithm has completed and produced a new image to be displayed.
                    try {

                        // put the new image into a new frame
                        new ViewJFrameImage(resultImage, null, new Dimension(25, 32));
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Pad With Slices reports: out of memory; " +
                                               "unable to open a new frame");
                    }

                    if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                        Preferences.debug("\nHave padded from " + nSlices + " slices for " + image.getImageName() +
                                          " to " + paddedSlices + " slices for " + resultImage.getImageName() + "\n");
                    } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
                } else if ((padSlicesAlgo.isCompleted() == false) && (resultImage != null)) {

                    // algorithm failed but result image still has garbage
                    resultImage.disposeLocal(); // clean up memory
                    resultImage = null;
                    System.gc();
                }
                // last case is that algorithm failed, but no image was produced.
                // since there is no image, don't need to clean up anything!
            }

            if (displayLoc == REPLACE) {
                // need to clean up locks that were set during replace.

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {

                    // ( (Frame) ( imageFrames.elementAt( i ) ) ).setTitle( titles[i] );
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                // this won't really work until the notifyImageExtentsListeners has been
                // fully implemented.
                image.notifyImageExtentsListeners();

                if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                    Preferences.debug("\nHave padded from " + nSlices + " to " + paddedSlices + " slices for " +
                                      image.getImageName() + "\n");
                } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
            }

        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        padSlicesAlgo.finalize();
        padSlicesAlgo = null;
        dispose();
    }


    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }


    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * Accessor to set padMode.
     *
     * @param  padMode  int
     */
    public void setPadMode(int padMode) {
        this.padMode = padMode;
    }

    /**
     * Once all the necessary variables are set, call the Remove Slices algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        System.gc();

        int[] destExtents = null;

        if (displayLoc == NEW) {

            try {
                destExtents = new int[3];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = paddedSlices;

                // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
                resultImage = new ModelImage(image.getType(), destExtents, image.getImageName());


                // Make algorithm:
                padSlicesAlgo = new AlgorithmPadWithSlices(image, resultImage, padMode);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                padSlicesAlgo.addListener(this);

                createProgressBar(image.getImageName(), padSlicesAlgo);

                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (padSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    padSlicesAlgo.run();
                }
            } catch (OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                MipavUtil.displayError("Pad with Slices reports: unable to allocate enough memory");

                return;
            }

        } else if (displayLoc == REPLACE) {

            try {
                // No need to make new image space because the user has
                // choosen to replace the source image

                // Make algorithm:
                padSlicesAlgo = new AlgorithmPadWithSlices(image, padMode);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                padSlicesAlgo.addListener(this);

                createProgressBar(image.getImageName(), padSlicesAlgo);

                setVisible(false); // Hide dialog

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (padSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    padSlicesAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog PadImages: unable to allocate enough memory");

                return;
            }

        } // end if display is LOC or REPLACE


    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        nSlices = image.getExtents()[2];
        paddedSlices = dimPowerOfTwo(nSlices);

        if (nSlices == paddedSlices) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1),
                                         nSlices + " slices is already a power of 2");
        }

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setPadMode(scriptParameters.getParams().getInt("padding_mode"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("padding_mode", padMode));
    }

    /**
     * Calculate the dimension value to power of 2.
     *
     * @param   dim  dimension value.
     *
     * @return  value dimension value in power of 2
     */
    private int dimPowerOfTwo(int dim) {

        if (dim <= 4) {
            return 4;
        } else if (dim <= 8) {
            return 8;
        } else if (dim <= 16) {
            return 16;
        } else if (dim <= 32) {
            return 32;
        } else if (dim <= 64) {
            return 64;
        } else if (dim <= 128) {
            return 128;
        } else if (dim <= 256) {
            return 256;
        } else if (dim <= 512) {
            return 512;
        } else if (dim <= 1024) {
            return 1024;
        } else if (dim <= 2048) {
            return 2048;
        } else if (dim <= 4096) {
            return 4096;
        } else if (dim <= 8192) {
            return 8192;
        } else if (dim <= 16384) {
            return 16384;
        } else if (dim <= 32768) {
            return 32768;
        } else {
            return 65536;
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        JPanel mainPanel = new JPanel(new BorderLayout());

        setTitle("Padding images");

        JPanel infoPanel = new JPanel();
        infoPanel.setBorder(buildTitledBorder("Pad to Power of 2"));
        infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.Y_AXIS));

        String info = "Padding from " + nSlices + " slices to " + paddedSlices + " slices.";
        JLabel labelInfo = new JLabel(info);
        infoPanel.add(labelInfo);

        JPanel checkPanel = new JPanel();
        checkPanel.setLayout(new BoxLayout(checkPanel, BoxLayout.Y_AXIS));
        checkPanel.setBorder(buildTitledBorder("Pad"));

        paddingGroup = new ButtonGroup();

        padToFront = new JRadioButton("Pad to front", true);
        padToFront.setFont(serif12);
        paddingGroup.add(padToFront);
        checkPanel.add(padToFront);

        padToBack = new JRadioButton("Pad to back", true);
        padToBack.setFont(serif12);
        paddingGroup.add(padToBack);
        checkPanel.add(padToBack);

        padToHalf = new JRadioButton("Pad to half front and half back", true);
        padToHalf.setFont(serif12);
        paddingGroup.add(padToHalf);
        checkPanel.add(padToHalf);

        // destination goes in the left of the lower box
        JPanel destinationPanel = new JPanel();
        destinationPanel.setLayout(new BoxLayout(destinationPanel, BoxLayout.Y_AXIS));
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        mainPanel.add(infoPanel, BorderLayout.NORTH);
        mainPanel.add(checkPanel, BorderLayout.CENTER);
        mainPanel.add(destinationPanel, BorderLayout.SOUTH);

        JPanel buttonPanel = new JPanel(new FlowLayout());

        buttonPanel.add(buildButtons());
        OKButton.setText("Pad");
        helpButton.setVisible(true);

        mainDialogPanel.setLayout(new BorderLayout());
        mainDialogPanel.add(mainPanel); // put the main panel into the center of the dialog
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);
        getContentPane().add(mainDialogPanel);
        pack();
        setSize(300, 320);
        setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (padToFront.isSelected()) {
            padMode = PAD_FRONT;
        } else if (padToBack.isSelected()) {
            padMode = PAD_BACK;
        } else if (padToHalf.isSelected()) {
            padMode = PAD_HALF;
        }

        return true;
    }

}
