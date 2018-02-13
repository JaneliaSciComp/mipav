package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;


/**
 * Dialog to call the randomize the order of 3D dataset. This dialog will not be visible because it does not require
 * user input at this time. It was made a dialog object because it may in the future require user input and to be
 * consistent with the dialog/algorithm paradigm. In should be noted, that the algorithms are executed in their own
 * thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogRandomizeSliceOrder extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8562091054505687054L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private AlgorithmRandSliceOrder orderAlgo;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRandomizeSliceOrder() { }

    /**
     * Run the algorithm.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogRandomizeSliceOrder(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
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
        int[] randomOrder;
        int i;

        // call this whether or not successfully completed, because we need to unlock the image.
        if (algorithm instanceof AlgorithmRandSliceOrder) {

            // The algorithm has completed and produced a new image to be displayed.
            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registered to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

            for (i = 0; i < imageFrames.size(); i++) {
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
            randomOrder = ((AlgorithmRandSliceOrder) algorithm).getRandomOrder();

            if (randomOrder != null) {
                userInterface.setDataText("\nThe new randomized slice ordering for ");
                userInterface.setDataText(image.getImageName() + " is: \n");

                for (i = 0; i < randomOrder.length; i++) {
                    userInterface.setDataText("\t" + (randomOrder[i]));

                    if (((i % 5) == 4) || (i == (randomOrder.length - 1))) {
                        userInterface.setDataText("\n");
                    }
                } // for (i = 0; i < randomOrder.length; i++)
            } // if (randomOrder != null)
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        

        orderAlgo.finalize();
        orderAlgo = null;
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
    }

    /**
     * Locks the images, then runs the inverse slice order algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            orderAlgo = new AlgorithmRandSliceOrder(image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            orderAlgo.addListener(this);

            createProgressBar(image.getImageName(), orderAlgo);
            
            // Hide dialog
            setVisible(false);

            // These next lines set the titles in all frames where the source image is displayed to
            // "locked - " image name so as to indicate that the image is now read/write locked!
            // The image frames are disabled and then unregisted from the userinterface until the
            // algorithm has completed.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
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
                orderAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog randomize slice order: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Does nothing at the moment, no dialog is created.
     */
    private void init() { }
}
