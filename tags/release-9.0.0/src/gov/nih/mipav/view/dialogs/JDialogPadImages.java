package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.Vector;

import javax.swing.*;


/**
 * Creates the dialog to pad blank images to an active image. Dialog asks the user how to pad the blank images, such as
 * padding to the front or back of the image, half to front and half to back of the image. It gives options to remove or
 * to cancel.
 * 
 * @author Ruida Cheng
 */
public class JDialogPadImages extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4451162492289524768L;

    /** Pad the image by adding blank slices to the front: */
    public static final int PAD_FRONT = 0;

    /** Pad the image by adding blank slices to the back: */
    public static final int PAD_BACK = 1;

    /** Pad the image by adding blank slices to the front and back: */
    public static final int PAD_HALF = 2;

    /** Flag indicating if a new image is to be generated or if the source image is to be replaced */
    private int displayLoc;

    /** source image */
    private ModelImage image;

    /** generate new image */
    private JRadioButton newImage;

    /** number of slices in image */
    private int nSlices;

    /** number of slices in padded image */
    private int paddedSlices;

    /** Padding mode, defaults to front. */
    private int padMode = JDialogPadImages.PAD_FRONT;

    /** Algorithm that adds slices to front/back of image. */
    private AlgorithmAddMargins padSlicesAlgo;

    /** Select padding to the back: */
    private JRadioButton padToBack;

    /** Select padding to the front: */
    private JRadioButton padToFront;

    /** Select padding to the front and back: */
    private JRadioButton padToHalf;

    /** Select replacing the current image */
    private JRadioButton replaceImage;

    /** result image */
    private ModelImage resultImage = null;

    /** Reference to the main user interface */
    private ViewUserInterface userInterface;

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogPadImages() {}

    /**
     * Creates new dialog for removing slices.
     * 
     * @param theParentFrame Parent frame
     * @param im Source image
     */
    public JDialogPadImages(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im; // set the image from the arguments to an image in this class
        userInterface = ViewUserInterface.getReference();
        nSlices = image.getExtents()[2];
        paddedSlices = MipavMath.nextPowerOfTwo(nSlices);

        if (nSlices == paddedSlices) {
            MipavUtil.displayError(nSlices + " slices is already a power of 2");

            return;
        }

        init();
    }

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("Pad")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("U4041");
            MipavUtil.showWebHelp("Pad");
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else { // else if (source == thresholdCheckbox)
            super.actionPerformed(event);
        }
    }

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmAddMargins) {

            if (displayLoc == JDialogBase.NEW) {

                if ( (padSlicesAlgo.isCompleted() == true) && (resultImage != null)) {

                    // The algorithm has completed and produced a new image to be displayed.
                    try {

                        // put the new image into a new frame
                        new ViewJFrameImage(resultImage, null, new Dimension(25, 32));
                    } catch (final OutOfMemoryError error) {
                        MipavUtil.displayError("Pad With Slices reports: out of memory; "
                                + "unable to open a new frame");
                    }

                    if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                        Preferences.debug("\nHave padded from " + nSlices + " slices for " + image.getImageName()
                                + " to " + paddedSlices + " slices for " + resultImage.getImageName() + "\n");
                    } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
                } else if ( (padSlicesAlgo.isCompleted() == false) && (resultImage != null)) {

                    // algorithm failed but result image still has garbage
                	
                    resultImage.disposeLocal(); // clean up memory
                    resultImage = null;
                    System.gc();
                }
                // last case is that algorithm failed, but no image was produced.
                // since there is no image, don't need to clean up anything!
            }

            if (displayLoc == JDialogBase.REPLACE) {
                // need to clean up locks that were set during replace.

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {

                    // ( (Frame) ( imageFrames.elementAt( i ) ) ).setTitle( titles[i] );
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if ( ((Frame) (imageFrames.elementAt(i))) != parentFrame) {
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
                    Preferences.debug("\nHave padded from " + nSlices + " to " + paddedSlices + " slices for "
                            + image.getImageName() + "\n");
                } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
            }

        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        setComplete(algorithm.isCompleted());
        padSlicesAlgo.finalize();
        padSlicesAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     * 
     * @return the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = JDialogBase.NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = JDialogBase.REPLACE;
    }

    /**
     * Accessor to set padMode.
     * 
     * @param padMode int
     */
    public void setPadMode(final int padMode) {
        this.padMode = padMode;
    }

    /**
     * Once all the necessary variables are set, call the Remove Slices algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        System.gc();

        int[] destExtents = null;

        final int[] marginX = new int[] {0, 0};
        final int[] marginY = new int[] {0, 0};
        final int[] marginZ = new int[] {0, 0};
        if (padMode == JDialogPadImages.PAD_FRONT) {
            marginZ[0] = Math.abs(paddedSlices - nSlices);
        } else if (padMode == JDialogPadImages.PAD_BACK) {
            marginZ[1] = Math.abs(paddedSlices - nSlices);
        } else {
            marginZ[0] = Math.abs(paddedSlices - nSlices) / 2;
            marginZ[1] = Math.abs(paddedSlices - nSlices) / 2;

            if ( (Math.abs(paddedSlices - nSlices) % 2) == 1) {
                marginZ[1]++;
            }
        }
        if (displayLoc == JDialogBase.NEW) {

            try {
                destExtents = new int[3];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = paddedSlices;

                resultImage = new ModelImage(image.getType(), destExtents, image.getImageName());
                resultImage.getMatrixHolder().replaceMatrices(image.getMatrixHolder().getMatrices());
                // preload this image with the minimum of the source image
                // resultImage.
                padSlicesAlgo = new AlgorithmAddMargins(image, resultImage, marginX, marginY, marginZ);

                // Listen to the algorithm so we get notified when it is succeeded or failed.
                // See algorithm performed event. caused by implementing AlgorithmedPerformed interface
                padSlicesAlgo.addListener(this);
                createProgressBar(image.getImageName(), padSlicesAlgo);

                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (padSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil
                                .displayError("AddMargins reports: A thread is already running on this object [addMarginsAlgo]");
                    }
                } else {

                    padSlicesAlgo.run();
                }
            } catch (final OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                MipavUtil.displayError("Pad with Slices reports: unable to allocate enough memory");

                return;
            }

        } else if (displayLoc == JDialogBase.REPLACE) {

            try {
            	padSlicesAlgo = new AlgorithmAddMargins(image, marginX, marginY, marginZ);

                padSlicesAlgo.addListener(this);
                createProgressBar(image.getImageName(), padSlicesAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                /*
                 * Vector imageFrames = image.getImageFrameVector();
                 * 
                 * titles = new String[imageFrames.size()]; for ( int i = 0; i < imageFrames.size(); i++ ) { titles[i] = (
                 * (Frame) ( imageFrames.elementAt( i ) ) ).getTitle(); ( (Frame) ( imageFrames.elementAt( i ) )
                 * ).setTitle( "Locked: " + titles[i] ); ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                 * userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) ); }
                 */

                if (isRunInSeparateThread()) {
                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (padSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    padSlicesAlgo.run();
                }
            } catch (final OutOfMemoryError x) {
                MipavUtil.displayError("Dialog PadImages: unable to allocate enough memory");

                return;
            }

        } // end if display is LOC or REPLACE

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == JDialogBase.NEW) {
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
        paddedSlices = MipavMath.nextPowerOfTwo(nSlices);

        if (nSlices == paddedSlices) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), nSlices
                    + " slices is already a power of 2");
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
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == JDialogBase.NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("padding_mode", padMode));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        final JPanel mainPanel = new JPanel(new BorderLayout());

        setTitle("Padding images");

        final JPanel infoPanel = new JPanel();
        infoPanel.setBorder(buildTitledBorder("Pad to Power of 2"));
        infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.Y_AXIS));

        final String info = "Padding from " + nSlices + " slices to " + paddedSlices + " slices.";
        final JLabel labelInfo = new JLabel(info);
        infoPanel.add(labelInfo);

        final JPanel checkPanel = new JPanel();
        checkPanel.setLayout(new BoxLayout(checkPanel, BoxLayout.Y_AXIS));
        checkPanel.setBorder(buildTitledBorder("Pad"));

        final ButtonGroup paddingGroup = new ButtonGroup();

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
        final JPanel destinationPanel = new JPanel();
        destinationPanel.setLayout(new BoxLayout(destinationPanel, BoxLayout.Y_AXIS));
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        final ButtonGroup destinationGroup = new ButtonGroup();
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

        final JPanel buttonPanel = new JPanel(new FlowLayout());

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
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (replaceImage.isSelected()) {
            displayLoc = JDialogBase.REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = JDialogBase.NEW;
        }

        if (padToFront.isSelected()) {
            padMode = JDialogPadImages.PAD_FRONT;
        } else if (padToBack.isSelected()) {
            padMode = JDialogPadImages.PAD_BACK;
        } else if (padToHalf.isSelected()) {
            padMode = JDialogPadImages.PAD_HALF;
        }

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Utilities.Slice tools");
            }

            public String getDescription() {
                return new String("Pads an Image");
            }

            public String getDescriptionLong() {
                return new String("Creates the dialog to pad blank images to an active image. " +
                		"Dialog asks the user how to pad the blank images, such as padding to " +
                		"the front or back of the image, half to front and half to back " +
                		"of the image. It gives options to remove or to cancel.");
            }

            public String getShortLabel() {
                return new String("Pad_power_of_2");
            }

            public String getLabel() {
                return new String("Pad Slices to power of 2");
            }

            public String getName() {
                return new String("Pad Slices to power of 2");
            }
        };
    }


	@Override
	public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterInt("padding_mode", 0));
            
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;

	}

	@Override
	public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;

	}

	@Override
	public String getOutputImageName(String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;

	}

	@Override
	public boolean isActionComplete() {
        return isComplete();
	}

}
