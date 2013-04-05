package gov.nih.mipav.model.algorithms;



import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;


/**
 * Will take Two 2D images (one Reference, one Adjusted) and use the Reference image's Resolutions to transform the
 * adjusted Image into a new size and resolution that the Reference Image can then be matched to, while preserving all
 * Field of Views (there is no loss of FOV).
 *
 * @author   Ben Link
 * @version  1.0
 */
public class AlgorithmMatchForReference extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** adjusted image. */
    private ModelImage adjImage;

    /** check for 2-dimensionality. */
    private boolean correctDims = true;

    /** was a new adjusted image created. */
    private boolean hasNewAdj = false;

    /** was a new reference image created. */
    private boolean hasNewRef = false;

    /** new adjusted image. */
    private ModelImage newAdjImage;

    /** new reference image. */
    private ModelImage newRefImage;

    /** reference image. */
    private ModelImage refImage;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.. takes in a reference and adjusted modelImage
     *
     * @param  rImage  reference image
     * @param  aImage  adjusted image
     */
    public AlgorithmMatchForReference(ModelImage rImage, ModelImage aImage) {
        this.refImage = rImage;
        this.adjImage = aImage;

        if (refImage.getNDims() != adjImage.getNDims()) {
            correctDims = false;
            setCompleted(false);
            notifyListeners(this);

            return;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {
        refImage = null;
        adjImage = null;
        newRefImage = null;
        newAdjImage = null;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * Gets the new adjusted image created.
     *
     * @return  new adj image
     */
    public ModelImage getNewAdjustedImage() {
        return this.newAdjImage;
    }

    /**
     * Gets the new reference image created.
     *
     * @return  new ref image
     */
    public ModelImage getNewReferenceImage() {
        return this.newRefImage;
    }

    /**
     * tells if new adjusted image was created during run().
     *
     * @return  if new was created
     */
    public boolean newAdjustedCreated() {
        return this.hasNewAdj;
    }

    /**
     * tells if new reference image was created during run().
     *
     * @return  if new was created
     */
    public boolean newReferenceCreated() {
        return this.hasNewRef;
    }

    /**
     * algorithm's run function....
     */
    public void runAlgorithm() {
        int units2D[] = new int[2];

        if (!correctDims) {
            return;
        }

        int newXDim, newYDim; // new X and Y dimensions... will only be used if larger

        // than the reference image's x and y dims

        int newZDim = 0;
        boolean adjNewZDim = false; // new ZDims will be used to make new adj image
        boolean refNewZDim = false; // new ZDims will be used to make new ref image
        boolean colorChanged = false; // was adj image's colortype changed (rgb->gray or gray->rgb)

        // check to see if the color types match up
        if (refImage.isColorImage() && !adjImage.isColorImage()) {

            // new adjusted image has NOT been created
            AlgorithmRGBConcat mathAlgo = null;

            // change adjImage to color
            ModelImage tempImage = new ModelImage(ModelImage.ARGB, adjImage.getExtents(),
                                                  JDialogBase.makeImageName(adjImage.getImageName(), "_rgb"));

            // get some important information from imageA and put it in
            // the result image
            FileInfoBase fInfoBase = null;

            for (int n = 0; n < adjImage.getFileInfo().length; n++) {
                fInfoBase = (FileInfoBase) (adjImage.getFileInfo(n).clone());
                fInfoBase.setDataType(tempImage.getType());
                tempImage.setFileInfo(fInfoBase, n);
            }

            // Make algorithm
            mathAlgo = new AlgorithmRGBConcat(adjImage, adjImage, adjImage, tempImage, true, true, 255.0f, true);
            mathAlgo.setRunningInSeparateThread(runningInSeparateThread);
            mathAlgo.run();

            newAdjImage = tempImage;
            adjImage = newAdjImage;
            newAdjImage.calcMinMax();
            mathAlgo.finalize();
            mathAlgo = null;
            hasNewAdj = true;
            colorChanged = true;
        } else if (!refImage.isColorImage() && adjImage.isColorImage()) {
            float redValue = 1.0f / 3.0f;
            float greenValue = 1.0f / 3.0f;
            float blueValue = 1.0f / 3.0f;
            float threshold = 1.0f;

            ModelImage tempImage = null;

            // image was not previously transformed
            if (adjImage.getType() == ModelStorageBase.ARGB) {
                tempImage = new ModelImage(ModelImage.UBYTE, adjImage.getExtents(), (adjImage.getImageName() + "Gray"));
            } else if (adjImage.getType() == ModelStorageBase.ARGB_USHORT) {
                tempImage = new ModelImage(ModelImage.USHORT, adjImage.getExtents(), (adjImage.getImageName() + "Gray"));
            } else if (adjImage.getType() == ModelStorageBase.ARGB_FLOAT) {
                tempImage = new ModelImage(ModelImage.FLOAT, adjImage.getExtents(), (adjImage.getImageName() + "Gray"));
            }

            // get some important information from imageA and put it in
            // the result image
            FileInfoBase fInfoBase = null;

            for (int n = 0; n < adjImage.getFileInfo().length; n++) {
                fInfoBase = (FileInfoBase) (adjImage.getFileInfo(n).clone());
                fInfoBase.setDataType(tempImage.getType());
                tempImage.setFileInfo(fInfoBase, n);
            }

            // Make algorithm
            AlgorithmRGBtoGray RGBAlgo = new AlgorithmRGBtoGray(tempImage, adjImage, redValue, greenValue, blueValue,
                                                                true, threshold, true);
            RGBAlgo.setRunningInSeparateThread(runningInSeparateThread);
            RGBAlgo.run();

            newAdjImage = tempImage;
            adjImage = newAdjImage;
            newAdjImage.calcMinMax();
            hasNewAdj = true;
            colorChanged = true;
        }

        int[] dims = new int[refImage.getNDims()];

        for (int i = 0; i < dims.length; i++) {
            dims[i] = refImage.getExtents()[i];
        }

        newXDim = adjImage.getExtents()[0];
        newYDim = adjImage.getExtents()[1];

        if (dims.length == 3) {
            newZDim = adjImage.getExtents()[2];
        }

        // first determine resolution differences
        float[] res = new float[refImage.getNDims()];

        for (int i = 0; i < res.length; i++) {
            res[i] = refImage.getFileInfo()[0].getResolutions()[i];
        }

        if (refImage.getNDims() == 3) {

            // check to see if there is a 2.5D mismatch (one but not the other)
            if ((refImage.getFileInfo()[0].getIs2_5D() && !adjImage.getFileInfo()[0].getIs2_5D()) ||
                    (!refImage.getFileInfo()[0].getIs2_5D() && adjImage.getFileInfo()[0].getIs2_5D())) {
                MipavUtil.displayError("Either both or neither image(s) must be 2.5D to load into Image(B)");
                setCompleted(false);

                return;
            } // Check to see if the resolutions are the same
            else if (res[2] == adjImage.getFileInfo()[0].getResolutions()[2]) {

                if (refImage.getExtents()[2] > adjImage.getExtents()[2]) {
                    newZDim = refImage.getExtents()[2];
                    adjNewZDim = true;
                } else if (refImage.getExtents()[2] < adjImage.getExtents()[2]) {
                    newZDim = adjImage.getExtents()[2];
                    refNewZDim = true;
                } else { }
            } else {
                // z resolutions are not the same

                newZDim = (int) ((adjImage.getFileInfo()[0].getResolutions()[2] / res[2]) * adjImage.getExtents()[2]);

                if (newZDim <= dims[2]) {
                    newZDim = dims[2];
                } else {
                    refNewZDim = true;
                }

                adjNewZDim = true;
            }
        }

        // if everything is the same, we dont need to change anything
        if ((res[0] == adjImage.getFileInfo()[0].getResolutions()[0]) &&
                (res[1] == adjImage.getFileInfo()[0].getResolutions()[1]) && (dims[0] == adjImage.getExtents()[0]) &&
                (dims[1] == adjImage.getExtents()[1]) && !hasNewAdj && !adjNewZDim && !refNewZDim) {
            setCompleted(true);

            return;
        }

        // otherwise check to see what new dimensions would be
        if (res[0] != adjImage.getFileInfo()[0].getResolutions()[0]) {
            newXDim = (int) ((adjImage.getFileInfo()[0].getResolutions()[0] / res[0]) * adjImage.getExtents()[0]);
        }

        if (res[1] != adjImage.getFileInfo()[0].getResolutions()[1]) {
            newYDim = (int) ((adjImage.getFileInfo()[0].getResolutions()[1] / res[1]) * adjImage.getExtents()[1]);
        }

        TransMatrix xfrmAdj = null;
        //Vector3f centerAdj = adjImage.getImageCentermm(false);
        xfrmAdj = new TransMatrix(res.length + 1);
        xfrmAdj.identity();
        //xfrmAdj.setTranslate(centerAdj.X, centerAdj.Y);
        //xfrmAdj.setTranslate(-centerAdj.X, -centerAdj.Y);

        TransMatrix xfrmRef;
        xfrmRef = new TransMatrix(res.length + 1);
        xfrmRef.identity();
        //xfrmRef.setTranslate(centerRef.X, centerRef.Y);
        //xfrmRef.setTranslate(-centerRef.X, -centerRef.Y);

        AlgorithmTransform trans = null;

        // now check to see the difference in dimensions (the new dimensions and the reference dimensions)
        // and always choose the larger field of view
        if (newXDim > dims[0]) {

            if (newYDim > dims[1]) {
                // use both of the new dimensions for the "bucket"
                // transform both reference and adjusted image

                // first transform the adjusted image
                if (adjNewZDim) {

                    // 3d
                    trans = new AlgorithmTransform(adjImage, xfrmAdj, AlgorithmTransform.TRILINEAR, res[0], res[1],
                                                   res[2], newXDim, newYDim, newZDim, 
                                                   false, false, false);
                } else {
                    units2D[0] = adjImage.getUnitsOfMeasure(0);
                    units2D[1] = adjImage.getUnitsOfMeasure(1);
                    trans = new AlgorithmTransform(adjImage, xfrmAdj, AlgorithmTransform.BILINEAR, res[0], res[1],
                                                   newXDim, newYDim, 
                                                   units2D,
                                                   false, false, false);
                }

                trans.setRunningInSeparateThread(runningInSeparateThread);
                trans.run();

                if (colorChanged) {
                    newAdjImage.disposeLocal();
                }

                newAdjImage = trans.getTransformedImage();
                newAdjImage.calcMinMax();
                trans.finalize();
                trans = null;

                // next transform the reference image
                if (refNewZDim) {
                    trans = new AlgorithmTransform(refImage, xfrmRef, AlgorithmTransform.TRILINEAR, res[0], res[1],
                                                   res[2], newXDim, newYDim, newZDim, 
                                                   false, false, false);
                } else {
                    units2D[0] = newAdjImage.getUnitsOfMeasure(0);
                    units2D[1] = newAdjImage.getUnitsOfMeasure(1);
                    trans = new AlgorithmTransform(refImage, xfrmRef, AlgorithmTransform.BILINEAR, res[0], res[1],
                                                   newXDim, newYDim, 
                                                   units2D,
                                                   false, false, false);
                }

                trans.setRunningInSeparateThread(runningInSeparateThread);
                trans.run();
                newRefImage = trans.getTransformedImage();
                newRefImage.calcMinMax();
                trans.finalize();
                trans = null;
                hasNewAdj = true;
                hasNewRef = true;
            } else {
                // use the newXdimension and the old Y dimension
                // transform both reference and adjusted image

                // first transform the adjusted image
                if (adjNewZDim) {

                    // 3d
                    trans = new AlgorithmTransform(adjImage, xfrmAdj, AlgorithmTransform.TRILINEAR, res[0], res[1],
                                                   res[2], newXDim, dims[1], newZDim, 
                                                   false, false, false);
                } else {
                    units2D[0] = newRefImage.getUnitsOfMeasure(0);
                    units2D[1] = newRefImage.getUnitsOfMeasure(1);
                    trans = new AlgorithmTransform(adjImage, xfrmAdj, AlgorithmTransform.BILINEAR, res[0], res[1],
                                                   newXDim, dims[1], 
                                                   units2D,
                                                   false, false, false);
                }

                trans.setRunningInSeparateThread(runningInSeparateThread);
                trans.run();

                if (colorChanged) {
                    newAdjImage.disposeLocal();
                }

                newAdjImage = trans.getTransformedImage();
                newAdjImage.calcMinMax();
                trans.finalize();
                trans = null;

                // next transform the reference image
                if (refNewZDim) {
                    trans = new AlgorithmTransform(refImage, xfrmRef, AlgorithmTransform.TRILINEAR, res[0], res[1],
                                                   res[2], newXDim, dims[1], newZDim, 
                                                   false, false, false);
                } else {
                    units2D[0] = newAdjImage.getUnitsOfMeasure(0);
                    units2D[1] = newAdjImage.getUnitsOfMeasure(1);
                    trans = new AlgorithmTransform(refImage, xfrmRef, AlgorithmTransform.BILINEAR, res[0], res[1],
                                                   newXDim, dims[1],  units2D,
                                                   false, false, false);
                }

                trans.setRunningInSeparateThread(runningInSeparateThread);
                trans.run();
                newRefImage = trans.getTransformedImage();
                newRefImage.calcMinMax();
                trans.finalize();
                trans = null;

                hasNewAdj = true;
                hasNewRef = true;
            }
        } else {

            if (newYDim > dims[1]) {
                // use the new Y dimension and the old X
                // transform both reference and adjusted image

                // first transform the adjusted image
                if (adjNewZDim) {

                    // 3d
                    trans = new AlgorithmTransform(adjImage, xfrmAdj, AlgorithmTransform.TRILINEAR, res[0], res[1],
                                                   res[2], dims[0], newYDim, newZDim, 
                                                   false, false, false);
                } else {
                    units2D[0] = adjImage.getUnitsOfMeasure(0);
                    units2D[1] = adjImage.getUnitsOfMeasure(1);
                    trans = new AlgorithmTransform(adjImage, xfrmAdj, AlgorithmTransform.BILINEAR, res[0], res[1],
                                                   dims[0], newYDim, 
                                                   units2D,
                                                   false, false, false);
                }

                trans.setRunningInSeparateThread(runningInSeparateThread);
                trans.run();

                if (colorChanged) {
                    newAdjImage.disposeLocal();
                }

                newAdjImage = trans.getTransformedImage();
                newAdjImage.calcMinMax();
                trans.finalize();
                trans = null;

                // next transform the reference image

                if (refNewZDim) {
                    trans = new AlgorithmTransform(refImage, xfrmRef, AlgorithmTransform.TRILINEAR, res[0], res[1],
                                                   res[2], dims[0], newYDim, newXDim, 
                                                   false, false, false);
                } else {
                    units2D[0] = refImage.getUnitsOfMeasure(0);
                    units2D[1] = refImage.getUnitsOfMeasure(1);
                    trans = new AlgorithmTransform(refImage, xfrmRef, AlgorithmTransform.BILINEAR, res[0], res[1],
                                                   dims[0], newYDim, 
                                                   units2D,
                                                   false, false, false);
                }

                trans.setRunningInSeparateThread(runningInSeparateThread);
                trans.run();
                newRefImage = trans.getTransformedImage();
                newRefImage.calcMinMax();
                trans.finalize();
                trans = null;

                hasNewAdj = true;
                hasNewRef = true;
            } else {

                // use the old X and Y dimensions (reference image will NOT change)
                // transform only the adjusted image
                if (adjNewZDim) {

                    trans = new AlgorithmTransform(adjImage, xfrmAdj, AlgorithmTransform.TRILINEAR, res[0], res[1],
                                                   res[2], dims[0], dims[1], newZDim, 
                                                   false, false, false);
                } else {
                    units2D[0] = adjImage.getUnitsOfMeasure(0);
                    units2D[1] = adjImage.getUnitsOfMeasure(1);
                    trans = new AlgorithmTransform(adjImage, xfrmAdj, AlgorithmTransform.BILINEAR, res[0], res[1],
                                                   dims[0], dims[1], 
                                                   units2D,
                                                   false, false, false);
                }

                trans.setRunningInSeparateThread(runningInSeparateThread);
                trans.run();

                if (colorChanged) {
                    newAdjImage.disposeLocal();
                }

                newAdjImage = trans.getTransformedImage();
                newAdjImage.calcMinMax();
                trans.finalize();
                trans = null;

                hasNewAdj = true;

                if (refNewZDim) {
                    trans = new AlgorithmTransform(refImage, xfrmRef, AlgorithmTransform.TRILINEAR, res[0], res[1],
                                                   res[2], dims[0], dims[1], newZDim,
                                                   false, false, false);
                    trans.setRunningInSeparateThread(runningInSeparateThread);
                    trans.run();

                    newRefImage = trans.getTransformedImage();
                    newRefImage.calcMinMax();
                    trans.finalize();
                    trans = null;
                    hasNewRef = true;
                } else {
                    hasNewRef = false;
                }
            }
        }

        setCompleted(true);
    }

}
