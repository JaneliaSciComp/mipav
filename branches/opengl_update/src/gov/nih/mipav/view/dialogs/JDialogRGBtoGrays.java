package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;


/**
 * Dialog to call a algorithm to convert an RGB to a grayscale images. This dialog will not be visible because it does
 * not require user input at this time. It was made a dialog object because it may in the future require user input and
 * to be consistent with the dialog/algorithm paradigm. In should be noted, that the algorithms are executed in their
 * own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogRGBtoGrays extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 540686811347959955L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage imageA = null;

    /** DOCUMENT ME! */
    private ModelImage resultImageB = null; // result image

    /** DOCUMENT ME! */
    private ModelImage resultImageG = null; // result image

    /** DOCUMENT ME! */
    private ModelImage resultImageR = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmRGBtoGrays RGBAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRGBtoGrays() { }

    /**
     * Sets variables needed to call algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image
     */
    public JDialogRGBtoGrays(Frame theParentFrame, ModelImage imA) {
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

        if (algorithm instanceof AlgorithmRGBtoGrays) {

            if ((RGBAlgo.isCompleted() == true) && (resultImageB != null)) {
                // The algorithm has completed and produced a new image to be displayed.

            	if ((imageA.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM)  {
                	FileInfoDicom fileInfoBuffer; // buffer of type DICOM
                	for (int n = 0; n < imageA.getFileInfo().length; n++) {
                		fileInfoBuffer = (FileInfoDicom) imageA.getFileInfo(n).clone(); // copy into buffer
                		fileInfoBuffer.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                		fileInfoBuffer.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric
                		fileInfoBuffer.setDataType(resultImageR.getType());
                		resultImageR.setFileInfo(fileInfoBuffer, n);
                		fileInfoBuffer = (FileInfoDicom) imageA.getFileInfo(n).clone(); // copy into buffer
                		fileInfoBuffer.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                		fileInfoBuffer.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric
                		fileInfoBuffer.setDataType(resultImageG.getType());
                		resultImageG.setFileInfo(fileInfoBuffer, n);
                		fileInfoBuffer = (FileInfoDicom) imageA.getFileInfo(n).clone(); // copy into buffer
                		fileInfoBuffer.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                		fileInfoBuffer.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric
                		fileInfoBuffer.setDataType(resultImageB.getType());
                		resultImageB.setFileInfo(fileInfoBuffer, n);
                	}
                }
            	else {
	                updateFileInfo(imageA, resultImageR);
	                updateFileInfo(imageA, resultImageG);
	                updateFileInfo(imageA, resultImageB);
            	}

                try {
                    new ViewJFrameImage(resultImageR, null, new Dimension(610, 200));
                    new ViewJFrameImage(resultImageG, null, new Dimension(650, 250));
                    new ViewJFrameImage(resultImageB, null, new Dimension(690, 300));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImageR != null) {

                // algorithm failed but result image still has garbage
                resultImageR.disposeLocal(); // clean up memory
                resultImageR = null;

                if (resultImageG != null) {
                    resultImageG.disposeLocal(); // clean up memory
                    resultImageG = null;
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

        RGBAlgo.finalize();
        RGBAlgo = null;
    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {

            if (imageA.getType() == ModelStorageBase.ARGB) {
                resultImageR = new ModelImage(ModelImage.UBYTE, imageA.getExtents(), "GrayR");
                resultImageG = new ModelImage(ModelImage.UBYTE, imageA.getExtents(), "GrayG");
                resultImageB = new ModelImage(ModelImage.UBYTE, imageA.getExtents(), "GrayB");
            } else if (imageA.getType() == ModelStorageBase.ARGB_USHORT) {
                resultImageR = new ModelImage(ModelImage.USHORT, imageA.getExtents(), "GrayR");
                resultImageG = new ModelImage(ModelImage.USHORT, imageA.getExtents(), "GrayG");
                resultImageB = new ModelImage(ModelImage.USHORT, imageA.getExtents(), "GrayB");
            } else if (imageA.getType() == ModelStorageBase.ARGB_FLOAT) {
                resultImageR = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), "GrayR");
                resultImageG = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), "GrayG");
                resultImageB = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), "GrayB");
            }

            // Make algorithm
            RGBAlgo = new AlgorithmRGBtoGrays(resultImageR, resultImageG, resultImageB, imageA);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            RGBAlgo.addListener(this);

            createProgressBar(imageA.getImageName(), RGBAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (RGBAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                RGBAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImageR != null) {
                resultImageR.disposeLocal(); // Clean up memory of result image
                resultImageR = null;
            }

            if (resultImageG != null) {
                resultImageG.disposeLocal(); // Clean up memory of result image
                resultImageG = null;
            }

            if (resultImageB != null) {
                resultImageB.disposeLocal(); // Clean up memory of result image
                resultImageB = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog RGB to Grays: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Accessor that returns the Blue result image.
     *
     * @return  resultImageB
     */
    public ModelImage getResultImageB() {
        return resultImageB;
    }

    /**
     * Accessor that returns the Green result image.
     *
     * @return  resultImageG
     */
    public ModelImage getResultImageG() {
        return resultImageG;
    }

    /**
     * Accessor that returns the Red result image.
     *
     * @return  resultImageR
     */
    public ModelImage getResultImageR() {
        return resultImageR;
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImageR());
        AlgorithmParameters.storeImageInRunner(getResultImageG());
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

        scriptParameters.storeImageInRecorder(getResultImageR());
        scriptParameters.storeImageInRecorder(getResultImageG());
        scriptParameters.storeImageInRecorder(getResultImageB());
    }
}
