package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;


/**
 * Dialog to call the AlgorithmConvert4Dto3D to convert the current image from a 4D one to a 3D one This dialog will not
 * be visible because it does not require user input at this time. It was made a dialog object because it may in the
 * future require user input and to be consistent with the dialog/algorithm paradigm. In should be noted, that the
 * algorithms are executed in their own thread.** replaces image
 */
public class JDialogConvert4Dto3D extends JDialogScriptableBase implements AlgorithmInterface{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4079283618961189766L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmConvert4Dto3D convert4Dto3DAlgo;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogConvert4Dto3D() { }

    /**
     * Creates new dialog, but dialog is not visible.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogConvert4Dto3D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
 
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException{
        scriptParameters.storeInputImage(image);
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams(){
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
    }
    
    /**
     * Calls run on the algorithm from the script parser.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {}

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

        if (algorithm instanceof AlgorithmConvert4Dto3D) {

            // need to clean up locks that were set during replace.

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
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

            // this won't really work until the notifyImageExtentsListeners has been
            // fully implemented.
            image.notifyImageExtentsListeners();

        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        convert4Dto3DAlgo.finalize();
        convert4Dto3DAlgo = null;
        dispose();
    }

    /**
     * Runs the algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            convert4Dto3DAlgo = new AlgorithmConvert4Dto3D(image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            convert4Dto3DAlgo.addListener(this);

            createProgressBar(image.getImageName(), convert4Dto3DAlgo);
            
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

                if (convert4Dto3DAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
               
                convert4Dto3DAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("JDialogConvert4Dto3D: unable to allocate enough memory");

            return;
        }
    }
}
