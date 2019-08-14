package gov.nih.mipav.model.algorithms.utilities;


import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;

/**
 * Algorithm to crop a tilted rectangle
 */
public class AlgorithmCropTilted extends AlgorithmBase { 
    private int x1Point;
    private int x2Point;
    private int x3Point;
    private int x4Point;
    
    private int y1Point;
    private int y2Point;
    private int y3Point;
    private int y4Point;
    
    private ModelImage resultImage;


    /**
     * Add or remove margins from the srcImage and store the results in the destImage.
     * @param srcImage original image
     * @param x1Point
     * @param y1Point
     * @param x2Point
     * @param y2Point
     * @param x3Point
     * @param y3Point
     * @param x4Point
     * @param y4Point
     */
    public AlgorithmCropTilted(ModelImage srcImage, int x1Point, int y1Point, int x2Point, int y2Point,
    		int x3Point, int y3Point, int x4Point, int y4Point) {
        super(null, srcImage);
        this.x1Point = x1Point;
        this.y1Point = y1Point;
        this.x2Point = x2Point;
        this.y2Point = y2Point;
        this.x3Point = x3Point;
        this.y3Point = y3Point;
        this.x4Point = x4Point;
        this.y4Point = y4Point;
    }
    
    public void runAlgorithm() {
    	double delx12 = x2Point - x1Point;
    	double dely12 = y2Point - y1Point;
    	double width = Math.sqrt(delx12*delx12 + dely12*dely12);
    	double delx23 = x3Point - x2Point;
    	double dely23 = y3Point - y2Point;
        double height = Math.sqrt(delx23*delx23 + dely23*dely23);
        
        double xcenter = (x1Point + x2Point + x3Point + x4Point)/4.0;
        double ycenter = (y1Point + y2Point + y3Point + y4Point)/4.0;
        System.out.println("xcenter = " + xcenter + " ycenter = " + ycenter);
        // Center in resolution space
        float xres = srcImage.getFileInfo()[0].getResolutions()[0];
        float yres = srcImage.getFileInfo()[0].getResolutions()[1];
        double ratio = (double)(y3Point - y4Point)/(double)(x3Point - x4Point);
        double theta = (180.0/Math.PI)*Math.atan(ratio);
        System.out.println("theta = " + theta);
        TransMatrix xfrm = new TransMatrix(3);
        xfrm.identity();
        xfrm.setTranslate(xres * xcenter,yres * ycenter);
        xfrm.setRotate(-theta);
        xfrm.setTranslate(-xres * xcenter,-yres * ycenter);
        int interp = AlgorithmTransform.BILINEAR;
        float oXres = xres;
        float oYres = yres;
        int oXdim = srcImage.getExtents()[0];;
        int oYdim = srcImage.getExtents()[1];
        int units[] = srcImage.getFileInfo()[0].getUnitsOfMeasure();
        boolean doVOI = true;
        boolean doClip = true;
        boolean doPad = false;
        boolean doRotateCenter = false;
        Vector3f center = new Vector3f(0.0f,0.0f,0.0f);
        AlgorithmTransform algoTrans = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oXdim, oYdim,
                                           units, doVOI, doClip, doPad, doRotateCenter, center);
        float fillValue = 0.0f;
        algoTrans.setFillValue(fillValue);
        boolean doUpdateOrigin = false;
        algoTrans.setUpdateOriginFlag(doUpdateOrigin);
        algoTrans.run();
        ModelImage rotatedImage = algoTrans.getTransformedImage();
        rotatedImage.calcMinMax();
        algoTrans.finalize();
        algoTrans = null;

        int xBounds[] = new int[2];
        int yBounds[] = new int[2];
        int zBounds[] = new int[2];
        xBounds[0] = (int)Math.floor((xcenter - width/2.0));
        if (xBounds[0] < 0) {
        	MipavUtil.displayError("Cannot have left < 0.0");
        	setCompleted(false);
        	return;
        }
        xBounds[1] = (int)Math.ceil((xcenter + width/2.0));
        if (xBounds[1] > srcImage.getExtents()[0] - 1) {
        	MipavUtil.displayError("Cannot have right > xDim - 1");
        	setCompleted(false);
        	return;
        }
        yBounds[0] = (int)Math.floor(ycenter - height/2.0);
        if (yBounds[0] < 0) {
        	MipavUtil.displayError("Cannot have top < 0");
        	setCompleted(false);
        	return;
        }
        yBounds[1] = (int)Math.ceil(ycenter + height/2.0);
        if (yBounds[1] > srcImage.getExtents()[1] - 1) {
        	MipavUtil.displayError("Cannot have bottom > yDim - 1");
        	setCompleted(false);
        	return;
        }
        
        int[] destExtents = null;

        if (srcImage.getNDims() == 2) {
            destExtents = new int[2];
            destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1;
            destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1;
        } else if (srcImage.getNDims() == 3) {
            destExtents = new int[3];
            destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1;
            destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1;
            destExtents[2] = Math.abs(zBounds[1] - zBounds[0]) + 1;

        } else if (srcImage.getNDims() == 4) {
            destExtents = new int[4];
            destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1;
            destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1;
            destExtents[2] = Math.abs(zBounds[1] - zBounds[0]) + 1;
            destExtents[3] = srcImage.getExtents()[3];
        } else {
            return;
        }

        // Make result image
        resultImage = new ModelImage(srcImage.getType(), destExtents,
                srcImage.getImageName() + "_crop");
        if (srcImage.getNDims() >= 2) {
            xBounds[0] *= -1;
            xBounds[1] = resultImage.getExtents()[0] - srcImage.getExtents()[0];
            yBounds[0] *= -1;
            yBounds[1] = resultImage.getExtents()[1] - srcImage.getExtents()[1];
        }
        if (srcImage.getNDims() >= 3) {
            zBounds[0] *= -1;
            zBounds[1] = resultImage.getExtents()[2] - srcImage.getExtents()[2];
        }
        AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(rotatedImage, resultImage, xBounds, yBounds, zBounds);
        cropAlgo.run();
        setCompleted(true);    
    }
    
    public ModelImage getResultImage() {
    	return resultImage;
    }

}
