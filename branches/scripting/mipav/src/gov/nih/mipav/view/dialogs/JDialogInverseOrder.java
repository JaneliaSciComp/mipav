package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;


/**
 * Dialog to call the inverse the order of 3D or 4D dataset. This dialog will not be visible because it does not require
 * user input at this time. It was made a dialog object because it may in the future require user input and to be
 * consistent with the dialog/algorithm paradigm. In should be noted, that the algorithms are executed in their own
 * thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogInverseOrder extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 63020177076529331L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private AlgorithmInverseOrder orderAlgo;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogInverseOrder() { }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogInverseOrder(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);
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
    public JDialogInverseOrder(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = im.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
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
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        // call this whether or not successfully completed, because we need to unlock the image.
        if (algorithm instanceof AlgorithmInverseOrder) {

            // The algorithm has completed and produced a new image to be displayed.
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

            if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                Preferences.debug("\nHave inverted slice order on ");
                Preferences.debug(image.getImageName() + "\n");
            } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
        } // if (algorithm instanceof AlgorithmInverseOrder)

        insertScriptLine(algorithm);

        orderAlgo.finalize();
        orderAlgo = null;
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

                userInterface.getScriptDialog().append("InverseOrder " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       "\n");
            }
        }
    }

    /**
     * Locks the images, then runs the inverse slice order algorithm.
     */
    public void run() {

        try {
            System.gc();

            // Make algorithm
            orderAlgo = new AlgorithmInverseOrder(image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            orderAlgo.addListener(this);

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

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface.
                if (orderAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    orderAlgo.setProgressBarVisible(false);
                }

                orderAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Inverse order: unable to allocate enough memory");

            return;
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

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        setSeparateThread(false);
        run();
    }

    /**
     * Does nothing at the moment, no dialog is created.
     */
    private void init() { }
}
