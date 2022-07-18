package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;


/**
 * Dialog to call the AlgorithmSwap34 to delete the current image and create a new image with the third and fourth
 * dimensions swapped. This dialog will not be visible because it does not require user input at this time. It was made
 * a dialog object because it may in the future require user input and to be consistent with the dialog/algorithm
 * paradigm. In should be noted, that the algorithms are executed in their own thread.** replaces image
 */
public class JDialogSwapDims extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8107279710815271509L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean doClose = true;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private String imageName;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private AlgorithmSwapDims swapDimsAlgo;

	private int swapType = AlgorithmSwapDims.SWAP34;
	
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSwapDims() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogSwapDims(ModelImage im) {
        super(false);
        image = im;
        imageName = image.getImageName();
        parentFrame = image.getParentFrame();
        doClose = false;
    }

    /**
     * Creates new dialog, but dialog is not visible.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogSwapDims(Frame theParentFrame, ModelImage im, int sw) {
        super(theParentFrame, false);
        image = im;
        imageName = image.getImageName();
		swapType = sw;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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

        if (algorithm instanceof AlgorithmSwapDims) {
            resultImage = swapDimsAlgo.getResultImage();

            if ((swapDimsAlgo.isCompleted() == true) && (resultImage != null)) {
                resultImage.setImageName(imageName);

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if ((parentFrame != null) && doClose) {
            ((ViewJFrameBase) parentFrame).getUserInterface().unregisterFrame(parentFrame);
            ((ViewJFrameBase) (parentFrame)).close();
        }

        swapDimsAlgo.finalize();
        swapDimsAlgo = null;
        dispose();
    }

    /**
     * Runs the algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            swapDimsAlgo = new AlgorithmSwapDims(image,swapType);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            swapDimsAlgo.addListener(this);

            createProgressBar(image.getImageName(), swapDimsAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                if (swapDimsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                swapDimsAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up image memory
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("JDialogSwap34: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        imageName = image.getImageName();
		
		swapType = scriptParameters.getParams().getInt("swap_type");
		
        setModal(false);
        doClose = false;
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(resultImage);
		
		scriptParameters.getParams().put(ParameterFactory.newParameter("swap_type", swapType));
    }
    
    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Utilities.4D tools");
            }

            public String getDescription() {
                return new String("Swaps either the 1st or 3rd dimensions with the 4th dimension of an image." +
                		"Note: for swap_type, input 1 to swap the 3rd and 4th dimensions, or " +
                		"input 2 to swap the 1st and 4th dimensions.");
            }

            public String getDescriptionLong() {
                return new String("Swaps either the 1st or 3rd dimensions with the 4th dimension of an image." +
                		"Note: for swap_type, input 1 to swap the 3rd and 4th dimensions, or " +
                		"input 2 to swap the 1st and 4th dimensions.");
            }

            public String getShortLabel() {
                return new String("SwapDims");
            }

            public String getLabel() {
                return new String("Swap dims");
            }

            public String getName() {
                return new String("Swap dims");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterInt("swap_type", 1));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
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

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                return getResultImage().getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }
}
