package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
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
public class JDialogSwap34 extends JDialogScriptableBase implements AlgorithmInterface {

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
    private AlgorithmSwap34 swap34Algo;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSwap34() { }

    /**
     * Creates new dialog, but dialog is not visible.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogSwap34(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        imageName = image.getImageName();
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogSwap34(ViewUserInterface UI, ModelImage im) {
        super(false);
        userInterface = UI;
        image = im;
        imageName = image.getImageName();
        parentFrame = image.getParentFrame();
        doClose = false;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calls run on the algorithm from the script parser.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
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
        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmSwap34) {
            resultImage = swap34Algo.getResultImage();

            if ((swap34Algo.isCompleted() == true) && (resultImage != null)) {
                resultImage.setImageName(imageName);

                try {
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
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

        swap34Algo.finalize();
        swap34Algo = null;
        dispose();
    }

    /**
     * Runs the algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            swap34Algo = new AlgorithmSwap34(image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            swap34Algo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                if (swap34Algo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    swap34Algo.setProgressBarVisible(false);
                }

                swap34Algo.run();
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
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();
        
        imageName = image.getImageName();
        setModal(false);
        doClose = false;
    }
   
    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
    }
}
