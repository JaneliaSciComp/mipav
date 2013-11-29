package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewJProgressBar;


/**
 * An Algorithm to rotate 3D or 4D dataset 90 or 180 degrees about X, Y, or Z axis. 2D Images can also be rotated. A new
 * rotated image with modified dimensions and resolutions created and can be accessed through returnImage().
 *
 * @version  1.0 July 25, 2000
 * @author   Harman J. Singh
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmRotate extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Rotate about the x axis 180 degrees. */
    public static final int X_AXIS_180 = 0;

    /** Rotate about the x axis 90 degrees. */
    public static final int X_AXIS_PLUS = 1;

    /** Rotate about the x axis -90 degrees (or 270 degrees). */
    public static final int X_AXIS_MINUS = 2;

    /** Rotate about the y axis 180 degrees. */
    public static final int Y_AXIS_180 = 3;

    /** Rotate about the y axis 90 degrees. */
    public static final int Y_AXIS_PLUS = 4;

    /** Rotate about the y axis -90 degrees (or 270 degrees). */
    public static final int Y_AXIS_MINUS = 5;

    /** Rotate about the z axis 180 degrees. */
    public static final int Z_AXIS_180 = 6;

    /** Rotate about the z axis 90 degrees. */
    public static final int Z_AXIS_PLUS = 7;

    /** Rotate about the z axis -90 degrees (or 270 degrees). */
    public static final int Z_AXIS_MINUS = 8;

    /* axisOrder and axisFlip changed to match the rotateAxis value: */
    private int[] axisOrder = { 0, 1, 2, 3 };
    private boolean[] axisFlip = { false, false, false, false };
    ViewJProgressBar progressBar;
    private boolean quiet  = false;

    //private TransMatrix rotMatrix = new TransMatrix(4);
    /**
     * Creates new algorithm for rotating. Sets the source image and axis parameter.
     *
     * @param  srcImg      Source image model.
     * @param  rotateMode  Rotate about which axis.
     */
    public AlgorithmRotate(ModelImage srcImg, int[] order, boolean[] flip ) {
        super(null, srcImg);
        for ( int i = 0; i < axisOrder.length; i++ )
        {
            this.axisOrder[i] = order[i];
            this.axisFlip[i] = flip[i];
        }
        //rotMatrix = null;
    }
    
    /**
     * Creates new algorithm for rotating. Sets the source image and axis parameter.
     *
     * @param  srcImg      Source image model.
     * @param  rotateMode  Rotate about which axis.
     */
    public AlgorithmRotate(ModelImage srcImg, int rotateMode) {
        super(null, srcImg);

        if ((rotateMode == Y_AXIS_180) || (rotateMode == Y_AXIS_PLUS) || (rotateMode == Y_AXIS_MINUS) ||
                (rotateMode == X_AXIS_180) || (rotateMode == X_AXIS_PLUS) || (rotateMode == X_AXIS_MINUS) ||
                (rotateMode == Z_AXIS_180) || (rotateMode == Z_AXIS_PLUS) || (rotateMode == Z_AXIS_MINUS)) {
            init(rotateMode);
        } else {
            init(Z_AXIS_PLUS); // default rotate mode
        }
    }

    /**
     * Creates new algorithm for rotating. Sets the source image and axis parameter.
     *
     * @param  srcImg      Source image model.
     * @param  rotateMode  Rotate about which axis.
     * @param  progress    Progress mode (see AlgorithmBase).
     */
    public AlgorithmRotate(ModelImage srcImg, int rotateMode, int progress) {
        this(srcImg, rotateMode);
        // progressMode = progress;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }
    
    /**
     * 
     * @param quiet
     */
    public void setQuiet(boolean quiet) {
        this.quiet = quiet;
    }

    /**
     * Returns the rotated image.
     *
     * @return  The rotated image.
     */
    public ModelImage returnImage() {
        return destImage;
    }

    /**
     * Runs the rotation algorithm. The algorithm is run in place so automatically replaces the source model image.
     * Resets image orientation, axis orientations, and start locations.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (!quiet) {
            progressBar = new ViewJProgressBar("Rotating image ",
                "Rotating image...", 0, 100, true);
        }
        calcInPlace();
        if (!quiet) {
            progressBar.dispose();
        }
    }


    private void init(int rotateAxis)
    {
        if (rotateAxis == X_AXIS_180) {
            axisFlip[1] = true;
            axisFlip[2] = true;
            //rotMatrix.setRotate(180, 0, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == X_AXIS_PLUS) {
            axisOrder[1] = 2;
            axisOrder[2] = 1;
            axisFlip[1] = true;
            //rotMatrix.setRotate(90, 0, 0, TransMatrix.DEGREES);
            // rotMatrix gives y' = -z, z' = y;
        } else if (rotateAxis == X_AXIS_MINUS) {
            axisOrder[1] = 2;
            axisOrder[2] = 1;
            axisFlip[2] = true;
            //rotMatrix.setRotate(-90, 0, 0, TransMatrix.DEGREES);
            // rotMatrix gives y' = z, z' = -y;
        } else if (rotateAxis == Y_AXIS_180) {
            axisFlip[0] = true;
            axisFlip[2] = true;
            //rotMatrix.setRotate(0, 180, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == Y_AXIS_PLUS) {
            axisOrder[0] = 2;
            axisOrder[2] = 0;
            axisFlip[2] = true;
            //rotMatrix.setRotate(0, 90, 0, TransMatrix.DEGREES);
            // rotMatrix gives x' = z, z' = -x
        } else if (rotateAxis == Y_AXIS_MINUS) {
            axisOrder[0] = 2;
            axisOrder[2] = 0;
            axisFlip[0] = true;
            //rotMatrix.setRotate(0, -90, 0, TransMatrix.DEGREES);
            // rotMatrix gives x' = -z, z' = x
        } else if (rotateAxis == Z_AXIS_180) {
            axisFlip[0] = true;
            axisFlip[1] = true;
            //rotMatrix.setRotate(0, 0, 180, TransMatrix.DEGREES);
        } else if (rotateAxis == Z_AXIS_PLUS) {
            axisOrder[0] = 1;
            axisOrder[1] = 0;
            axisFlip[0] = true;
            //rotMatrix.setRotate(0, 0, 90, TransMatrix.DEGREES);
        } else if (rotateAxis == Z_AXIS_MINUS) {
            axisOrder[0] = 1;
            axisOrder[1] = 0;
            axisFlip[1] = true;
            //rotMatrix.setRotate(0, 0, -90, TransMatrix.DEGREES);
        }
    }

    /**
     * Calculates the rotated image and replaces the source image with the rotated image.
     */
    private void calcInPlace() {
    	int startValue = 0;
    	int finalValue;
    	if (srcImage.getFileInfo(0).getFileFormat() != FileUtility.DICOM) {
    		finalValue = 100;
    	}
    	else {
    		finalValue = 90;
    	}
        destImage = srcImage.export( axisOrder, axisFlip, true, progressBar, startValue, finalValue );
        destImage.setImageName( srcImage.getImageName() );
        
        destImage.calcMinMax();
        if (srcImage.getFileInfo(0).getFileFormat() != FileUtility.DICOM) {
        	startValue = 100;
    		finalValue = 100;
    	}
    	else {
    		startValue = 90;
    		finalValue = 100;
    	}
        
        if ( !ModelImage.updateFileInfo( destImage, srcImage, axisOrder, axisFlip, progressBar,
        		startValue, finalValue) )
        {
            errorCleanUp("Algorithm Rotate: Out of memory", true);
            return;
        }
        
        destImage.releaseLock();
        setCompleted(true);
    }


}
