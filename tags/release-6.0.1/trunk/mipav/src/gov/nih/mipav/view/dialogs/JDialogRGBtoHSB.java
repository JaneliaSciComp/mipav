package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;


/**
 * Dialog to separate RGB channels into grayscale Hue, Saturation, and Brightness channels.
 */
public class JDialogRGBtoHSB extends JDialogScriptableBase implements AlgorithmInterface {

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
        super(theParentFrame, false);
        imageA = imA;
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

        if (algorithm instanceof AlgorithmRGBtoHSB) {

            if ((HSBAlgo.isCompleted() == true) && (resultImageB != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(imageA, resultImageH);
                updateFileInfo(imageA, resultImageS);
                updateFileInfo(imageA, resultImageB);

                try {
                    new ViewJFrameImage(resultImageH, null, new Dimension(610, 200));
                    new ViewJFrameImage(resultImageS, null, new Dimension(650, 250));
                    new ViewJFrameImage(resultImageB, null, new Dimension(690, 300));
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

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        HSBAlgo.finalize();
        HSBAlgo = null;
    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {

            if (imageA.getType() == ModelStorageBase.ARGB) {
                resultImageH = new ModelImage(ModelImage.UBYTE, imageA.getExtents(), "GrayHue");
                resultImageS = new ModelImage(ModelImage.UBYTE, imageA.getExtents(), "GraySaturation");
                resultImageB = new ModelImage(ModelImage.UBYTE, imageA.getExtents(), "GrayBrightness");
            } else if (imageA.getType() == ModelStorageBase.ARGB_USHORT) {
                resultImageH = new ModelImage(ModelImage.USHORT, imageA.getExtents(), "GrayHue");
                resultImageS = new ModelImage(ModelImage.USHORT, imageA.getExtents(), "GraySaturation");
                resultImageB = new ModelImage(ModelImage.USHORT, imageA.getExtents(), "GrayBrightness");
            } else if (imageA.getType() == ModelStorageBase.ARGB_FLOAT) {
                resultImageH = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), "GrayHue");
                resultImageS = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), "GraySaturation");
                resultImageB = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), "GrayBrightness");
            }

            // Make algorithm
            HSBAlgo = new AlgorithmRGBtoHSB(resultImageH, resultImageS, resultImageB, imageA);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            HSBAlgo.addListener(this);

            createProgressBar(imageA.getImageName(), HSBAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (HSBAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
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
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImageH());
        AlgorithmParameters.storeImageInRunner(getResultImageS());
        AlgorithmParameters.storeImageInRunner(getResultImageB());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        imageA = scriptParameters.retrieveInputImage();
        parentFrame = imageA.getParentFrame();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(imageA);

        scriptParameters.storeImageInRecorder(getResultImageH());
        scriptParameters.storeImageInRecorder(getResultImageS());
        scriptParameters.storeImageInRecorder(getResultImageB());
    }
}
