import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.event.*;
import java.awt.*;

/**
 * 
 * 
 * @version 0.1 Nov 17, 1998
 * @author Matthew J. McAuliffe, Ph.D.
 * 
 */
public class PlugInDialogSCUP extends JDialogBase implements AlgorithmInterface {

    private PlugInAlgorithmSCUP scupAlgo;

    private ModelImage imageA = null; // source image

    private ModelImage resultImage = null;

    private ViewUserInterface userInterface;

    /**
     * Sets variables needed to call algorithm.
     * 
     * @param theParentFrame
     *            Parent frame
     * @param imA
     *            Source image
     */
    public PlugInDialogSCUP(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);
        imageA = imA;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        callAlgorithm();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm.
     * No actual dialog will appear but the set up info and result image will be
     * stored here.
     * 
     * @param UI
     *            The user interface, needed to create the image frame.
     * @param imA
     *            Source image.
     */
    public PlugInDialogSCUP(ViewUserInterface UI, ModelImage imA) {
        super();

        userInterface = UI;
        imageA = imA;
        callAlgorithm();
    }

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event
     *            Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {
            resultImage = new ModelImage(imageA.getType(), imageA.getExtents(), (imageA.getImageName() + "_scup"),
                    userInterface);

            resultImage.copyFileTypeInfo(imageA);

            // Make algorithm
            scupAlgo = new PlugInAlgorithmSCUP(resultImage, imageA);
            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed of failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            scupAlgo.addListener(this);
            // Hide dialog
            setVisible(false);

            if (runInSeparateThread) {
                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (scupAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                scupAlgo.setActiveImage(isActiveImage);
                scupAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("AlgorithmAbsoluteValue: unable to allocate enough memory");
            return;
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is
     * implemented. It is called by the algorithms when it has completed or
     * failed to to complete, so that the dialog can be display the result image
     * and/or clean up.
     * 
     * @param algorithm
     *            Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        imageA.clearMask();
        if (algorithm instanceof PlugInAlgorithmSCUP) {
            if (scupAlgo.isCompleted() == true) {
                // The algorithm has completed and produced a new image to be
                // displayed.

                updateFileInfo(imageA, resultImage);

                new ViewJFrameImage(resultImage, null);
            }
        }
    }
}
