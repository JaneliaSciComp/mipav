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
public class PlugInDialogNEISeg extends JDialogBase implements AlgorithmInterface {

    private PlugInAlgorithmNEISeg eyeAlgo;

    private ModelImage imageA = null; // source image

    private ModelImage resultImageRGB_AllRatios = null;

    private ModelImage resultImageHSB_Hue = null;

    private ModelImage resultImageRGB = null;

    private ModelImage resultImageFinal = null;

    private ViewUserInterface userInterface;

    /**
     * Sets variables needed to call algorithm.
     * 
     * @param theParentFrame
     *            Parent frame
     * @param imA
     *            Source image
     */
    public PlugInDialogNEISeg(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);
        if (imA.getVOIs().size() != 2) {
            MipavUtil.displayError("Two separate VOIs required");
            return;
        }

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
    public PlugInDialogNEISeg(ViewUserInterface UI, ModelImage imA) {
        super();

        if (imA.getVOIs().size() != 2) {
            MipavUtil.displayError("Two separate VOIs required");
            return;
        }

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

            resultImageRGB_AllRatios = new ModelImage(ModelImage.ARGB, imageA.getExtents(),
                    (imageA.getImageName() + "RGB_AllRatios"), userInterface);

            resultImageHSB_Hue = new ModelImage(ModelImage.ARGB, imageA.getExtents(),
                    (imageA.getImageName() + "HSB_Hue"), userInterface);

            resultImageRGB = new ModelImage(ModelImage.ARGB, imageA.getExtents(), (imageA.getImageName() + "RGB"),
                    userInterface);

            resultImageFinal = new ModelImage(ModelImage.ARGB, imageA.getExtents(),
                    (imageA.getImageName() + "Final (2 out of 3)"), userInterface);

            // get some important information from imageA and put it in
            // the result image
            resultImageRGB_AllRatios.copyFileTypeInfo(imageA);
            resultImageHSB_Hue.copyFileTypeInfo(imageA);
            resultImageRGB.copyFileTypeInfo(imageA);
            resultImageFinal.copyFileTypeInfo(imageA);

            // Make algorithm
            eyeAlgo = new PlugInAlgorithmNEISeg(resultImageRGB_AllRatios, resultImageHSB_Hue, resultImageRGB,
                    resultImageFinal, imageA);
            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed of failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            eyeAlgo.addListener(this);
            // Hide dialog
            setVisible(false);

            if (runInSeparateThread) {
                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (eyeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                eyeAlgo.setActiveImage(isActiveImage);
                eyeAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog RGB to Gray: unable to allocate enough memory");
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
        if (algorithm instanceof PlugInAlgorithmNEISeg) {
            if (eyeAlgo.isCompleted() == true) {
                // The algorithm has completed and produced a new image to be
                // displayed.

                updateFileInfo(imageA, resultImageRGB_AllRatios);
                updateFileInfo(imageA, resultImageHSB_Hue);
                updateFileInfo(imageA, resultImageRGB);
                updateFileInfo(imageA, resultImageFinal);

                new ViewJFrameImage(resultImageRGB_AllRatios, null, new Dimension(0, 100));
                new ViewJFrameImage(resultImageHSB_Hue, null, new Dimension(0, 200));
                new ViewJFrameImage(resultImageRGB, null, new Dimension(0, 400));
                new ViewJFrameImage(resultImageFinal, null, new Dimension(0, 600));

                // imageA.getParentFrame().setImageB(resultImage);
            }
        }
    }
}
