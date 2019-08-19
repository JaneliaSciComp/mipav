package gov.nih.mipav.model.algorithms.utilities;


import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;

/**
 * Algorithm to crop a tilted rectangle
 */
public class AlgorithmCropTilted extends AlgorithmBase { 
    private double x1;
    private double x2;
    private double x3;
    private double x4;
    private double x5;
    private double x6;
    private double x7;
    private double x8;
    
    private double y1;
    private double y2;
    private double y3;
    private double y4;
    private double y5;
    private double y6;
    private double y7;
    private double y8;
    
    private double z1;
    private double z2;
    private double z3;
    private double z4;
    private double z5;
    private double z6;
    private double z7;
    private double z8;
    
    private ModelImage resultImage;
    
    /** used for setting rotation */
    public static final int DEGREES = 0;

    /** used for setting rotation */
    public static final int RADIANS = 1;
    
    private boolean run2D;


    /**
     * Rotate tilted rectangle to remove tilt and crop rectangle.
     * @param srcImage original image
     * @param x1
     * @param y1
     * @param x2
     * @param y2
     * @param x3
     * @param y3
     * @param x4
     * @param y4
     */
    public AlgorithmCropTilted(ModelImage srcImage, double x1, double y1, double x2, double y2,
    		double x3, double y3, double x4, double y4) {
        super(null, srcImage);
        this.x1 = x1;
        this.y1 = y1;
        this.x2 = x2;
        this.y2 = y2;
        this.x3 = x3;
        this.y3 = y3;
        this.x4 = x4;
        this.y4 = y4;
        run2D = true;
    }
    
    
    
    /**
     * Rotate tilted cuboid to remove tilt and crop cuboid.
     * @param srcImage original image
     * @param x1
     * @param y1
     * @param z1
     * @param x2
     * @param y2
     * @param z2
     * @param x3
     * @param y3
     * @param z3
     * @param x4
     * @param y4
     * @param z4
     * @param x5
     * @param y5
     * @param z5
     * @param x6
     * @param y6
     * @param z6
     * @param x7
     * @param y7
     * @param z7
     * @param x8
     * @param y8
     * @param z8
     */
    public AlgorithmCropTilted(ModelImage srcImage, double x1, double y1, double z1, double x2, double y2, double z2,
    		double x3, double y3, double z3, double x4, double y4, double z4, double x5, double y5, double z5, double x6,
    		double y6, double z6, double x7, double y7, double z7, double x8, double y8, double z8) {
        super(null, srcImage);
        this.x1 = x1;
        this.y1 = y1;
        this.z1 = z1;
        this.x2 = x2;
        this.y2 = y2;
        this.z2 = z2;
        this.x3 = x3;
        this.y3 = y3;
        this.z3 = z3;
        this.x4 = x4;
        this.y4 = y4;
        this.z4 = z4;
        this.x5 = x5;
        this.y5 = y5;
        this.z5 = z5;
        this.x6 = x6;
        this.y6 = y6;
        this.z6 = z6;
        this.x7 = x7;
        this.y7 = y7;
        this.z7 = z7;
        this.x8 = x8;
        this.y8 = y8;
        this.z8 = z8;
        run2D = false;
    }
    
    public void runAlgorithm() {
    	double delx12;
    	double dely12;
    	double delz12;
    	double width;
    	double height;
    	double depth = 0;
    	double delx23;
    	double dely23;
    	double delz23;
    	double delx15;
    	double dely15;
    	double delz15;
    	double xcenter;
    	double ycenter;
    	double zcenter = 0.0;
    	float xres = srcImage.getFileInfo()[0].getResolutions()[0];
    	float yres = srcImage.getFileInfo()[0].getResolutions()[1];
    	float zres;
    	double ratio;
    	double thetaX = 0.0;
    	double thetaY = 0.0;
    	double thetaZ;
    	TransMatrix xfrm;
    	int interp;
    	float oXres = xres;
    	float oYres = yres;
    	float oZres;
    	int oXdim = srcImage.getExtents()[0];;
        int oYdim = srcImage.getExtents()[1];
        int oZdim;
        int units[] = srcImage.getUnitsOfMeasure();
        int xunit = units[0];
        int yunit = units[1];
        int zunit;
        boolean doVOI = true;
        boolean doClip = true;
        boolean doPad = false;
        boolean doRotateCenter = false;
        Vector3f center = new Vector3f(0.0f,0.0f,0.0f);
        if ((xunit != yunit) && (xunit != Unit.UNKNOWN_MEASURE.getLegacyNum()) && (yunit != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
        	yres = (float)((Unit.getUnitFromLegacyNum(yunit)).
            		getConversionFactor(Unit.getUnitFromLegacyNum(xunit)) * yres);
        	units[1] = units[0];
        }
        AlgorithmTransform algoTrans;
        //i = 107 j = 71 k = 16 X = 108.40897670747673 Y = 71.0000014349817 Z = 17.982873916625977
		//i = 161 j = 71 k = 16 X = 155.17434463469482 Y = 71.0000014349817 Z = 12.936144828796387
		//i = 107 j = 186 k = 16 X = 108.40897670747673 Y = 185.9999958544973 Z = 17.982873916625977
		//i = 161 j = 186 k = 16 X = 155.17434463469482 Y = 185.9999958544973 Z = 12.936144828796387
		//i = 107 j = 71 k = 21 X = 121.78397553158895 Y = 71.0000014349817 Z = 22.313000857830048
		//i = 161 j = 71 k = 21 X = 168.54934345880704 Y = 71.0000014349817 Z = 17.266271770000458
		//i = 107 j = 186 k = 21 X = 121.78397553158895 Y = 185.9999958544973 Z = 22.313000857830048
		//i = 161 j = 186 k = 21 X = 168.54934345880704 Y = 185.9999958544973 Z = 17.266271770000458
        /*boolean test = true;
        if (test) {
        	// For tilted 30 degrees around y
        	x1 = 108.40897670747673;
        	y1 = 71.0000014349817;
        	z1 = 17.982873916625977;
        	x2 = 155.17434463469482;
        	y2 = 71.0000014349817;
        	z2 = 12.936144828796387;
        	x3 = 155.17434463469482;
        	y3 = 185.9999958544973;
        	z3 = 12.936144828796387;
        	x4 = 108.40897670747673;
        	y4 = 185.9999958544973;
        	z4 = 17.98287391662597;
        	x5 = 121.78397553158895;
        	y5 = 71.0000014349817;
        	z5 = 22.313000857830048;
        	x6 = 168.54934345880704;
        	y6 = 71.0000014349817;
        	z6 = 17.266271770000458;
        	x7 = 168.54934345880704;
        	y7 = 185.9999958544973;
        	z7 = 17.26627177000045;
            x8 = 121.78397553158895;
            y8 = 185.9999958544973;
            z8 = 22.313000857830048;
        }*/
    	
    	if (run2D) {
	        delx12 = x2 - x1;
	    	dely12 = y2 - y1;
	    	width = Math.sqrt(delx12*delx12*xres*xres + dely12*dely12*yres*yres)/xres;
	    	delx23 = x3 - x2;
	    	dely23 = y3 - y2;
	        height = Math.sqrt(delx23*delx23*xres*xres + dely23*dely23*yres*yres)/yres;
	        
	        xcenter = (x1 + x2 + x3 + x4)/4.0;
	        ycenter = (y1 + y2 + y3 + y4)/4.0;
	        System.out.println("xcenter = " + xcenter + " ycenter = " + ycenter);
	        // Center in resolution space
	        yres = srcImage.getFileInfo()[0].getResolutions()[1];
	        ratio = ((y3 - y4)*yres)/((x3 - x4)*xres);
	        thetaZ = (180.0/Math.PI)*Math.atan(ratio);
	        System.out.println("thetaZ = " + thetaZ);
	        xfrm = new TransMatrix(3);
	        xfrm.identity();
	        xfrm.setTranslate(xres * xcenter,yres * ycenter);
	        xfrm.setRotate(-thetaZ);
	        xfrm.setTranslate(-xres * xcenter,-yres * ycenter);
	        interp = AlgorithmTransform.BILINEAR;
	        
	        algoTrans = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oXdim, oYdim,
	                                           units, doVOI, doClip, doPad, doRotateCenter, center);
    	} // if (run2D)
    	else {
    		oZdim = srcImage.getExtents()[2];
    		zres = srcImage.getFileInfo()[0].getResolutions()[2];
    		zunit = units[2];
    		 if ((xunit != zunit) && (xunit != Unit.UNKNOWN_MEASURE.getLegacyNum()) && (zunit != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
    	        	zres = (float)((Unit.getUnitFromLegacyNum(zunit)).
    	            		getConversionFactor(Unit.getUnitFromLegacyNum(xunit)) * zres);
    	        	units[2] = units[0];
    	    }
	        oZres = zres;
    		delx12 = x2 - x1;
	    	dely12 = y2 - y1;
	    	delz12 = z2 - z1;
	    	width = Math.sqrt(delx12*delx12*xres*xres + dely12*dely12*yres*yres + delz12*delz12*zres*zres)/xres;
	    	System.out.println("width = " + width);
	    	delx23 = x3 - x2;
	    	dely23 = y3 - y2;
	    	delz23 = z3 - z2;
	        height = Math.sqrt(delx23*delx23*xres*xres + dely23*dely23*yres*yres + delz23*delz23*zres*zres)/yres;
	        System.out.println("height = " + height);
	        delx15 = x5 - x1;
	        dely15 = y5 - y1;
	        delz15 = z5 - z1;
	        depth = Math.sqrt(delx15*delx15*xres*xres + dely15*dely15*yres*yres + delz15*delz15*zres*zres)/zres;
	        System.out.println("depth = " + depth);
	        xcenter = (x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8)/8.0;
	        ycenter = (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8)/8.0;
	        zcenter = (z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8)/8.0;
	        System.out.println("xcenter = " + xcenter + " ycenter = " + ycenter + " zcenter = " + zcenter);
	        // Center in resolution space
	        ratio = ((y1 - y5)*yres)/((z1 - z5)*zres);
	        thetaX = (180.0/Math.PI)*Math.atan(ratio);
	        System.out.println("thetaX = " + thetaX);
	        ratio = ((x1 - x5)*xres)/((z1 - z5)*zres);
	        thetaY = (180.0/Math.PI)*Math.atan(ratio);
	        System.out.println("thetaY = " + thetaY);
	        ratio = ((y3 - y4)*yres)/((x3 - x4)*xres);
	        thetaZ = (180.0/Math.PI)*Math.atan(ratio);
	        System.out.println("thetaZ = " + thetaZ);
	        xfrm = new TransMatrix(4);
	        xfrm.identity();
	        xfrm.setTranslate(xres * xcenter, yres * ycenter, zres * zcenter);
	        xfrm.setRotate(-thetaX,thetaY,-thetaZ,DEGREES);
	        xfrm.setTranslate(-xres * xcenter, -yres * ycenter, -zres * zcenter);
	        interp = AlgorithmTransform.TRILINEAR;
	        
	        algoTrans = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim,
	                                           units, doVOI, doClip, doPad, doRotateCenter, center);	
    	}
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
        xBounds[0] = (int)Math.floor(xcenter - width/2.0);
        if (xBounds[0] < 0) {
        	MipavUtil.displayError("Cannot have left < 0.0");
        	setCompleted(false);
        	return;
        }
        xBounds[1] = (int)Math.ceil(xcenter + width/2.0);
        if (xBounds[1] > srcImage.getExtents()[0] - 1) {
        	System.out.println("right = " + xBounds[1]);
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
        if (!run2D) {
        	zBounds[0] = (int)Math.floor(zcenter - depth/2.0);
        	if (zBounds[0] < 0) {
        		MipavUtil.displayError("Cannot have front < 0");
        		setCompleted(false);
        		return;
        	}
        	zBounds[1] = (int)Math.ceil(zcenter + depth/2.0);
        	if (zBounds[1] > srcImage.getExtents()[2] - 1) {
        		MipavUtil.displayError("Cannot have back > zDim - 1");
        		setCompleted(false);
        		return;
        	}
        } // if (!run2D)
        
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
        resultImage.calcMinMax();
        setCompleted(true);    
    }
    
    public ModelImage getResultImage() {
    	return resultImage;
    }

}
