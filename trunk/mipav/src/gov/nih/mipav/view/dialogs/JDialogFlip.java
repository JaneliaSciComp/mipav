package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;


/**
 * Dialog to call the image flip. This dialog will not be visible because it does not require user input at this time.
 * It was made a dialog object because it may in the future require user input and to be consistent with the
 * dialog/algorithm paradigm. In should be noted, that the algorithms are executed in their own thread.** replaces image
 *
 * @version  1.0 July 17, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogFlip extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5672670158596197276L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmFlip flipAlgo;

    /** DOCUMENT ME! */
    private int flipAxis;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFlip() { }

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     * @param  flipAxis        Axis which image is to be flipped.
     */
    public JDialogFlip(Frame theParentFrame, ModelImage im, int flipAxis) {
        super(theParentFrame, true);

        setForeground(Color.black);
        this.flipAxis = flipAxis;
        image = im;
        userInterface = ((ViewJFrameBase) parentFrame).getUserInterface();
    }

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present. This constructor is used by the script parser because it doesn't have the parent frame.
     *
     * @param  ui        User interface.
     * @param  im        Source image.
     * @param  flipAxis  Axis which image is to be flipped.
     */
    public JDialogFlip(ViewUserInterface ui, ModelImage im, int flipAxis) {
        super();

        setForeground(Color.black);
        parentFrame = im.getParentFrame();
        this.flipAxis = flipAxis;
        image = im;
        this.userInterface = ui;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Presently only the script function calls this method. When the script sends this dialog the action command, this
     * method calls run.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
    }

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

        if (algorithm instanceof AlgorithmFlip) {

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector imageFrames = image.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                    userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }
            }

            if (parentFrame != null) {
                userInterface.registerFrame(parentFrame);
            }

            image.notifyImageDisplayListeners(null, true);

            insertScriptLine(algorithm);
        }

        flipAlgo.finalize();
        flipAlgo = null;

    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            flipAlgo = new AlgorithmFlip(image, flipAxis);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            flipAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

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

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (flipAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                flipAlgo.setActiveImage(isActiveImage);

                //if (!userInterface.isAppFrameVisible()) {
                    flipAlgo.setProgressBarVisible(false);
                //}

                flipAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog VOI Extraction: unable to allocate enough memory");

            return;
        }
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("Flip " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (flipAxis == AlgorithmFlip.X_AXIS) {
                    userInterface.getScriptDialog().append("X\n");
                } else {
                    userInterface.getScriptDialog().append("Y\n");
                }
            }
        }
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        setForeground(Color.black);
        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        try {
            String axisn = parser.getNextString();

            if (axisn.equals("X")) {
                flipAxis = AlgorithmFlip.X_AXIS;
            } else if (axisn.equals("Y")) {
                flipAxis = AlgorithmFlip.Y_AXIS;
            } else {
                throw new Exception("Illegal axis parameter: " + axisn);
            }
        } catch (Exception e) {
            throw new IllegalArgumentException(e.getMessage());
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        callAlgorithm();
    }

}
