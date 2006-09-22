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


/**
 * Dialog to call the image rotate. This dialog will not be visible because it does not require user input at this time.
 * It was made a dialog object because it may in the future require user input and to be consistent with the
 * dialog/algorithm paradigm. Replaces image.
 *
 * @version  1.0 July 26, 2000
 * @author   Harman J. Singh
 */
public class JDialogRotate extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8408959889101739488L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean doClose = true;

    /** Source image for algorithm. */
    private ModelImage image = null;

    /** Result image after algorithm has completed. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private AlgorithmRotate rotateAlgo;

    /** Rotation axis. */
    private int rotateAxis;

    /** Needed to set frames to locked. */
    private String[] titles;

    /** User interface pointer. */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRotate() { }

    /**
     * Sets up variables but does not show dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     * @param  rotateAxis      Axis which image is to be rotated.
     */
    public JDialogRotate(Frame theParentFrame, ModelImage im, int rotateAxis) {
        super(theParentFrame, true);

        this.rotateAxis = rotateAxis;
        image = im;
        userInterface = ViewUserInterface.getReference();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Only called by the script parser; runs the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) { }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmRotate) {

            if (rotateAlgo.isCompleted() == true) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                resultImage = rotateAlgo.returnImage();

                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                Point pt;

                if (parentFrame != null) {
                    pt = ((ViewJFrameBase) parentFrame).getLocation();
                } else {
                    pt = new Point(Toolkit.getDefaultToolkit().getScreenSize().width / 2,
                                   Toolkit.getDefaultToolkit().getScreenSize().height / 2);
                }

                imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(pt.x, pt.y));

                // Not so sure about this.
                if (image.getLightBoxFrame() != null) {

                    try {
                        pt = image.getLightBoxFrame().getLocation();
                        image.getLightBoxFrame().close();
                        new ViewJFrameLightBox(imageFrame, "LightBox", resultImage,
                                               imageFrame.getComponentImage().getLUTa(),
                                               imageFrame.getComponentImage().getImageB(),
                                               imageFrame.getComponentImage().getLUTb(),
                                               imageFrame.getComponentImage().getResolutionX(),
                                               imageFrame.getComponentImage().getResolutionY(),
                                               new Dimension(pt.x, pt.y), imageFrame.getControls());
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                }
            } else {

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
            }

            if (rotateAlgo.isCompleted() == true) {
                insertScriptLine();

                if (doClose && !(ScriptRecorder.getReference().getRecorderStatus() == ScriptRecorder.RECORDING)) {

                    if (parentFrame != null) {
                        userInterface.unregisterFrame(parentFrame);
                        ((ViewJFrameBase) parentFrame).close();
                    } else {
                        ((ViewJFrameImage) image.getParentFrame()).close();
                    }
                } // if (doClose)

            } // if (rotateAlgo.isCompleted() == true)
        }

        System.gc();
        rotateAlgo.finalize();
        rotateAlgo = null;
    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {

            // Make algorithm
            rotateAlgo = new AlgorithmRotate(image, rotateAxis);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            rotateAlgo.addListener(this);

            createProgressBar(image.getImageName(), rotateAlgo);

            // Hide dialog
            setVisible(false);

            // These next lines set the titles in all frames where the source image is displayed to
            // "locked - " image name so as to indicate that the image is now read/write locked!
            // The image frames are disabled and then unregisted from the userinterface until the
            // algorithm has completed.
            Vector imageFrames = image.getImageFrameVector();
            titles = new String[imageFrames.size()];

            for (int i = 0; i < imageFrames.size(); i++) {
                titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
            }

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (rotateAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                if (!userInterface.isAppFrameVisible()) {
                    rotateAlgo.setProgressBarVisible(false);
                }

                rotateAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Rotate: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        doClose = false;
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        // TODO: why do we reg the parent frame?  shouldn't it already be registered?
        userInterface.regFrame(image.getParentFrame());

        String axisn = scriptParameters.getParams().getString("rotation_axis");
        int axis;

        if (axisn.equals("X180")) {
            axis = AlgorithmRotate.X_AXIS_180;
        } else if (axisn.equals("X+")) {
            axis = AlgorithmRotate.X_AXIS_PLUS;
        } else if (axisn.equals("X-")) {
            axis = AlgorithmRotate.X_AXIS_MINUS;
        } else if (axisn.equals("Y180")) {
            axis = AlgorithmRotate.Y_AXIS_180;
        } else if (axisn.equals("Y+")) {
            axis = AlgorithmRotate.Y_AXIS_PLUS;
        } else if (axisn.equals("Y-")) {
            axis = AlgorithmRotate.Y_AXIS_MINUS;
        } else if (axisn.equals("Z180")) {
            axis = AlgorithmRotate.Z_AXIS_180;
        } else if (axisn.equals("Z+")) {
            axis = AlgorithmRotate.Z_AXIS_PLUS;
        } else if (axisn.equals("Z-")) {
            axis = AlgorithmRotate.Z_AXIS_MINUS;
        } else {
            throw new ParameterException("rotation_axis", "Illegal axis parameter: " + axisn);
        }

        rotateAxis = axis;
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        AlgorithmParameters.storeImageInRecorder(getResultImage());

        String axis;

        switch (rotateAxis) {

            case AlgorithmRotate.X_AXIS_180:
                axis = "X180";
                break;

            case AlgorithmRotate.X_AXIS_PLUS:
                axis = "X+";
                break;

            case AlgorithmRotate.X_AXIS_MINUS:
                axis = "X-";
                break;

            case AlgorithmRotate.Y_AXIS_180:
                axis = "Y180";
                break;

            case AlgorithmRotate.Y_AXIS_PLUS:
                axis = "Y+";
                break;

            case AlgorithmRotate.Y_AXIS_MINUS:
                axis = "Y-";
                break;

            case AlgorithmRotate.Z_AXIS_180:
                axis = "Z180";
                break;

            case AlgorithmRotate.Z_AXIS_MINUS:
                axis = "Z-";
                break;

            case AlgorithmRotate.Z_AXIS_PLUS:
            default:
                axis = "Z+";
                break;
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("rotation_axis", axis));
    }
}
