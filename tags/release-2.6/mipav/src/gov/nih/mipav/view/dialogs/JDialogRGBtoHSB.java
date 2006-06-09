package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;


/**
 * Dialog to separate RGB channels into grayscale Hue, Saturation, and Brightness channels.
 */
public class JDialogRGBtoHSB extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3386283808285677422L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmRGBtoHSB HSBAlgo;

    /** DOCUMENT ME! */
    private ModelImage imageA = null;

    /** DOCUMENT ME! */
    private ModelImage resultImageB = null; // result image

    /** DOCUMENT ME! */
    private ModelImage resultImageH = null; // result image

    /** DOCUMENT ME! */
    private ModelImage resultImageS = null; // result image

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRGBtoHSB() { }

    /**
     * Sets variables needed to call algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image
     */
    public JDialogRGBtoHSB(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);
        imageA = imA;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI   The user interface, needed to create the image frame.
     * @param  imA  Source image.
     */
    public JDialogRGBtoHSB(ViewUserInterface UI, ModelImage imA) {
        super();
        userInterface = UI;
        imageA = imA;
        parentFrame = imageA.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calls the algorithm from the script parser.
     *
     * @param  event  Event that triggers function.
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
        ViewJFrameImage imageFrameH = null;
        ViewJFrameImage imageFrameS = null;
        ViewJFrameImage imageFrameB = null;

        if (algorithm instanceof AlgorithmRGBtoHSB) {

            if ((HSBAlgo.isCompleted() == true) && (resultImageB != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(imageA, resultImageH);
                updateFileInfo(imageA, resultImageS);
                updateFileInfo(imageA, resultImageB);

                try {
                    imageFrameH = new ViewJFrameImage(resultImageH, null, new Dimension(610, 200));
                    imageFrameS = new ViewJFrameImage(resultImageS, null, new Dimension(650, 250));
                    imageFrameB = new ViewJFrameImage(resultImageB, null, new Dimension(690, 300));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImageH != null) {

                // algorithm failed but result image still has garbage
                resultImageH.disposeLocal(); // clean up memory
                resultImageH = null;

                if (resultImageS != null) {
                    resultImageS.disposeLocal(); // clean up memory
                    resultImageS = null;
                }

                if (resultImageB != null) {
                    resultImageB.disposeLocal(); // clean up memory
                    resultImageB = null;
                }

                System.gc();
            }
        }

        // Update frame
        if (parentFrame != null) {
            ((ViewJFrameBase) parentFrame).updateImages(true);
        }

        insertScriptLine(algorithm);

        HSBAlgo.finalize();
        HSBAlgo = null;
    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {

            if (imageA.getType() == ModelStorageBase.ARGB) {
                resultImageH = new ModelImage(ModelImage.UBYTE, imageA.getExtents(), "GrayHue", userInterface);
                resultImageS = new ModelImage(ModelImage.UBYTE, imageA.getExtents(), "GraySaturation", userInterface);
                resultImageB = new ModelImage(ModelImage.UBYTE, imageA.getExtents(), "GrayBrightness", userInterface);
            } else if (imageA.getType() == ModelStorageBase.ARGB_USHORT) {
                resultImageH = new ModelImage(ModelImage.USHORT, imageA.getExtents(), "GrayHue", userInterface);
                resultImageS = new ModelImage(ModelImage.USHORT, imageA.getExtents(), "GraySaturation", userInterface);
                resultImageB = new ModelImage(ModelImage.USHORT, imageA.getExtents(), "GrayBrightness", userInterface);
            } else if (imageA.getType() == ModelStorageBase.ARGB_FLOAT) {
                resultImageH = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), "GrayHue", userInterface);
                resultImageS = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), "GraySaturation", userInterface);
                resultImageB = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), "GrayBrightness", userInterface);
            }

            // Make algorithm
            HSBAlgo = new AlgorithmRGBtoHSB(resultImageH, resultImageS, resultImageB, imageA);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            HSBAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (HSBAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                HSBAlgo.setActiveImage(isActiveImage);

                if (!userInterface.isAppFrameVisible()) {
                    HSBAlgo.setProgressBarVisible(false);
                }

                HSBAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImageH != null) {
                resultImageH.disposeLocal(); // Clean up memory of result image
                resultImageH = null;
            }

            if (resultImageS != null) {
                resultImageS.disposeLocal(); // Clean up memory of result image
                resultImageS = null;
            }

            if (resultImageB != null) {
                resultImageB.disposeLocal(); // Clean up memory of result image
                resultImageB = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog RGB to HSB: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Accessor that returns the Brightness result image.
     *
     * @return  resultImageB
     */
    public ModelImage getResultImageB() {
        return resultImageB;
    }

    /**
     * Accessor that returns the Hue result image.
     *
     * @return  resultImageR
     */
    public ModelImage getResultImageH() {
        return resultImageH;
    }

    /**
     * Accessor that returns the Saturation result image.
     *
     * @return  resultImageG
     */
    public ModelImage getResultImageS() {
        return resultImageS;
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
                if (userInterface.getScriptDialog().getImgTableVar(imageA.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(imageA.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(imageA.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("RGBtoHSB " +
                                                       userInterface.getScriptDialog().getVar(imageA.getImageName()) +
                                                       " ");
                userInterface.getScriptDialog().putVar(resultImageH.getImageName());
                userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImageH.getImageName()) +
                                                       " ");
                userInterface.getScriptDialog().putVar(resultImageS.getImageName());
                userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImageS.getImageName()) +
                                                       " ");
                userInterface.getScriptDialog().putVar(resultImageB.getImageName());
                userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImageB.getImageName()) +
                                                       "\n");
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
        String imageHKey = null;
        String imageSKey = null;
        String imageBKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        imageA = im;
        userInterface = imageA.getUserInterface();
        parentFrame = imageA.getParentFrame();

        // the result image
        try {
            imageHKey = parser.getNextString();
            imageSKey = parser.getNextString();
            imageBKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        callAlgorithm();
        parser.putVariable(imageHKey, getResultImageH().getImageName());
        parser.putVariable(imageSKey, getResultImageS().getImageName());
        parser.putVariable(imageBKey, getResultImageB().getImageName());
    }
}
