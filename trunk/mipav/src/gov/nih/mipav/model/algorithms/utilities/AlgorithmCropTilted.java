package gov.nih.mipav.model.algorithms.utilities;


import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewVOIVector;

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
    
    final int VERTICES_METHOD = 1;
    final int VOI_METHOD = 2;
    final int MASK_METHOD = 3;
    int method = VERTICES_METHOD;
    
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
    		double x3, double y3, double x4, double y4, int method) {
        super(null, srcImage);
        run2D = true;
        this.method = method;
        if (method == VERTICES_METHOD) {
        	this.x1 = x1;
            this.y1 = y1;
            this.x2 = x2;
            this.y2 = y2;
            this.x3 = x3;
            this.y3 = y3;
            this.x4 = x4;
            this.y4 = y4;
        }
        else if (method == MASK_METHOD) {
        	this.x1 = x1;
        	this.y1 = y1;
        }
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
    		double y6, double z6, double x7, double y7, double z7, double x8, double y8, double z8, int method) {
        super(null, srcImage);
        run2D = false;
        this.method = method;
        if (method == VERTICES_METHOD) {
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
        }
        else if (method == MASK_METHOD) {
        	this.x1 = x1;
	        this.y1 = y1;
	        this.z1 = z1;	
        }
        
    }
    
    public void runAlgorithm() {
    	double delx12;
    	double dely12;
    	double delz12;
    	double width = 0;
    	double height = 0;
    	double depth = 0;
    	double delx23;
    	double dely23;
    	double delz23;
    	double delx15;
    	double dely15;
    	double delz15;
    	double xcenter = 0.0;
    	double ycenter = 0.0;
    	double zcenter = 0.0;
    	float xres = srcImage.getFileInfo()[0].getResolutions()[0];
    	float yres = srcImage.getFileInfo()[0].getResolutions()[1];
    	double pixArea = xres * yres;
    	float zres;
    	double pixVolume;
    	double ratio;
    	double thetaX = 0.0;
    	double thetaY = 0.0;
    	double thetaZ = 0.0;
    	TransMatrix xfrm = null;
    	int interp;
    	float oXres = xres;
    	float oYres = yres;
    	float oZres;
    	int oXdim = srcImage.getExtents()[0];
        int oYdim = srcImage.getExtents()[1];
        int length = oXdim * oYdim;
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
        double term;
        // Moment of inertia or second moment of the area A with respect to the x axis
        double Ix = 0.0;
        // Moment of inertia about the y axis
        double Iy = 0.0;
        // Moment of inertia about the z axis
        double Iz = 0.0;
        // Product of inertia
        double Pxy = 0.0;
        double Pyz = 0.0;
        double Pzx = 0.0;
        double theta1;
        double theta2;
        double Iave;
        double Idiff;
        double Ixp1;
        double Ixp2;
        double R;
        double Imin;
        double Imax;
        int i;
        double exactTotalPixelCount;
        double diffx1;
        double diffx2;
        double diffy1;
        double diffy2;
        short buffer[];
        short value;
        int xLow;
        int xHigh;
        int yLow;
        int yHigh;
        int zLow;
        int zHigh;
        int nextXLow;
        int nextXHigh;
        int nextYLow;
        int nextYHigh;
        int nextZLow;
        int nextZHigh;
        boolean found;
        int nPts = 0;
        Vector<Float>xpos;
        Vector<Float>ypos;
        Vector<Float>zpos;
        BitSet havePos;
        int x;
        int y;
        int z;
        int index;
        double xdiff;
        double ydiff;
        double zdiff;
        int xStart;
        int yStart;
        int zStart;
        int volume;
        double a1;
        double b;
        double c;
        double d;
        double K1real[];
        double K2real[];
        double K2imag[];
        double K3real[];
        double K3imag[];
        int result[];
        CubicEquation ce;
        double dircosx1;
        double dircosy1;
        double dircosz1;
        double dircosx2;
        double dircosy2;
        double dircosz2;
        double dircosx3;
        double dircosy3;
        double dircosz3;
        double scalextoy;
        double scalextoz;
        // To develop and test code do create an untilted cuboid with a rectangle and VOI propagation to neighboring slices.
        // Record the voxel coordinates of the 8 edge voxels.
        // Then use AlgorithmTransform to create tilted x 30 degrees, tilted y 30 degrees, tilted z 30 degrees, 
        // tilted x 15 degrees, y 15 degrees, z 15 degrees.  Then for each of these reverse the angles of rotation
        // to get back to the original image.  When reversing the original rotation, use code like the following
        // in AlgorithmTransform.trilinear(double[], TransMatrix):
        //if (((i == 107) || (i == 161)) && ((j == 71) || (j == 186)) && ((k == 16) || (k == 21))) {
        //	System.out.println("i = " + i + " j = " + j + " k = " + k + " X = " + X + " Y = " + Y + " Z = " + Z); 
        //}
        // i are the untilted x coordinates, j are the untilted y coordinates, and k are the untilted z coordinates.
        // X are the tilted x coordinates, Y are the tilted y coordinates, and Z are the tilted Z coordinates.
        // You can then use code like below to feed in the 24 titled values:
        
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
        // Run AlgorithmCropTilted with these 24 values on a titled image to verify that tilted crop works correctly.
       
    	if (run2D) {
    		if (method == VERTICES_METHOD) {
		        delx12 = x2 - x1;
		    	dely12 = y2 - y1;
		    	width = Math.sqrt(delx12*delx12*xres*xres + dely12*dely12*yres*yres)/xres;
		    	System.out.println("width = " + width);
		    	delx23 = x3 - x2;
		    	dely23 = y3 - y2;
		        height = Math.sqrt(delx23*delx23*xres*xres + dely23*dely23*yres*yres)/yres;
		        System.out.println("height = " + height);
		        
		        xcenter = (x1 + x2 + x3 + x4)/4.0;
		        ycenter = (y1 + y2 + y3 + y4)/4.0;
		        System.out.println("xcenter = " + xcenter + " ycenter = " + ycenter);
		        // Center in resolution space
		        ratio = ((y3 - y4)*yres)/((x3 - x4)*xres);
		        thetaZ = (180.0/Math.PI)*Math.atan(ratio);
		        System.out.println("thetaZ = " + thetaZ);
		        xfrm = new TransMatrix(3);
		        xfrm.identity();
		        xfrm.setTranslate(xres * xcenter,yres * ycenter);
		        xfrm.setRotate(-thetaZ);
		        xfrm.setTranslate(-xres * xcenter,-yres * ycenter);
    		} // if (method == VERTICES_METHOD)
    		else if ((method == MASK_METHOD) || (method == VOI_METHOD)) {
    			if (method == MASK_METHOD) {
	    			if ((srcImage.getType() != ModelImage.BOOLEAN) && (srcImage.getType() != ModelImage.UBYTE) &&
	    	                (srcImage.getType() != ModelImage.BYTE) && (srcImage.getType() != ModelImage.USHORT) &&
	    	                (srcImage.getType() != ModelImage.SHORT)) {
	    	            displayError("Source Image must be Boolean, UByte, Byte, UShort, or Short");
	    	            setCompleted(false);
	
	    	            return;
	    	        }
	    			
	    			buffer = new short[length];
	    			try {
	                    srcImage.exportData(0, length, buffer);
	                } catch (IOException error) {
	                    displayError("Algorithm CropTilted: image bounds exceeded");
	                    setCompleted(false);
	
	                    return;
	                }
	    			xStart = (int)Math.round(x1);
	    			yStart = (int)Math.round(y1);
	                value = buffer[xStart + yStart*oXdim];
	                found = true;
	                xLow = Math.max(0,xStart-1);
	                xHigh = Math.min(oXdim-1,xStart+1);
	                yLow = Math.max(0,yStart-1);
	                yHigh = Math.min(oYdim-1,yStart+1);
	                nextXLow = xLow;
	                nextXHigh = xHigh;
	                nextYLow = yLow;
	                nextYHigh = yHigh;
	                xpos = new Vector<Float>();
	                ypos = new Vector<Float>();
	                xpos.add(xStart*xres);
	                ypos.add(yStart*yres);
	                havePos = new BitSet(length);
	                havePos.set(xStart + yStart*oXdim);
	                while (found) {
	                    found = false;
	                    xLow = nextXLow;
	                    xHigh = nextXHigh;
	                    yLow = nextYLow;
	                    yHigh = nextYHigh;
	                    for (y = yLow; y <= yHigh; y++) {
	                    	for (x = xLow; x <= xHigh; x++) {
	                    	    index = x + oXdim * y;
	                    	    if ((buffer[index] == value) && (!havePos.get(index)) && 
	                    	    (((x >= 1) && havePos.get(index-1)) || ((x < oXdim - 1) && havePos.get(index+1)) ||
	                    	     ((y >= 1) && havePos.get(index-oXdim)) || ((y < oYdim - 1) && havePos.get(index+oXdim)))) {
	                    	    	found = true;
	                    	    	havePos.set(index);
	                    	    	nPts++;
	                    	    	xpos.add(x*xres);
	                    	    	ypos.add(y*yres);
	                    	    	xcenter += x*xres;
	                    	    	ycenter += y*yres;
	                    	    	if ((x == xLow)  && (nextXLow > 0) && (nextXLow == xLow)) {
	                    	    		nextXLow = xLow-1;
	                    	    	}
	                    	    	if ((x == xHigh) && (nextXHigh < oXdim-1) && (nextXHigh == xHigh)) {
	                    	    		nextXHigh = xHigh+1;
	                    	    	}
	                    	    	if ((y == yLow)  && (nextYLow > 0) && (nextYLow == yLow)) {
	                    	    		nextYLow = yLow-1;
	                    	    	}
	                    	    	if ((y == yHigh) && (nextYHigh < oYdim-1) && (nextYHigh == yHigh)) {
	                    	    		nextYHigh = yHigh+1;
	                    	    	}
	                    	    }
	                    	}
	                    }
	                } // while (found)
	                xcenter = Math.abs(xcenter/nPts);
	                ycenter = Math.abs(ycenter/nPts);
	                System.out.println("xcenter = " + (xcenter/xres) + " ycenter = " + (ycenter/yres));
	                for (i = 0; i < nPts; i++) {
	                    xdiff = xpos.get(i) - xcenter;
	                    ydiff = ypos.get(i) - ycenter;
	                    Ix += ydiff*ydiff*pixArea;
	                    Iy += xdiff*xdiff*pixArea;
	                    Pxy += xdiff*ydiff*pixArea;
	                } // for (i = 0; i < nPts; i++)
    		    } // if (method == MASK_METHOD)
    		    else if (method == VOI_METHOD) {
	    			/**
	    			 * From statics it is known that any planar shape or closed curve possesses two principal axes 90 degrees
	    			 * apart intersecting at the centroid of the area.  (For certain areas such as a circle, there may be more
	    			 * than two principal axes about the centroid, but there are always at least two.)  The moment of inertia is
	    			 * maximum about one principal axis and minimum about the other principal axis.  Here the angle in degrees 
	    			 * with the principal axis having the smallest absolute angle is reported.  For a rectangle with the principal
	    			 * axes going through the centroid the moment of inertia Ix' = (1/12)*width*(height**3) and 
	    			 * Iy' = (1/12)*(height**3)*width.
	    			 * References: On the Computation of the Moments of a Polygon, with Some Applications by Soerjadi
	    			 * Vector Mechanics for Engineers Statics Ninth Edition by Beer, Johnston, Mazurek, Eisenberg
	    			 * Chapter 9 Distributed Forces: Moments of Inertia */
	    	        int nVOIs;
	    	        ViewVOIVector VOIs = srcImage.getVOIs();
	    	        nVOIs = VOIs.size();
	    	        int nBoundingVOIs = 0;
	    	        index = -1;
	    	        int nActiveBoundingVOIs = 0;
	    	        
	    	        for (i = 0; i < nVOIs; i++) {
	    	
	    	            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
	    	                nBoundingVOIs++;
	    	                if (nBoundingVOIs == 1) {
	    	                	index = i;
	    	                }
	    	                if (VOIs.VOIAt(i).isActive()) {
	    	                    nActiveBoundingVOIs++;	
	    	                    index = i;
	    	                }
	    	            }
	    	        } // for (i = 0; i < nVOIs; i++)
	    	        if (nBoundingVOIs == 0) {
	    	        	MipavUtil.displayError("Must have one contour or polyline VOI");
	    	        	setCompleted(false);
	    	        	return;
	    	        }
	    	        
	    	        if (nActiveBoundingVOIs > 1) {
	    	            MipavUtil.displayError("Cannot have more than one active bounding VOI");
	    	            setCompleted(false);
	    	            return;
	    	        }	
	    	        Vector<VOIBase> vArray = VOIs.VOIAt(index).getCurves();	
	    	        if (vArray == null) {
	    	        	MipavUtil.displayError("vArray is null");
	    	        	setCompleted(false);
	    	        	return;
	    	        }
	    	        if (vArray.size() == 0) {
	    	        	MipavUtil.displayError("No curves in VOI");
	    	        	setCompleted(false);
	    	        	return;
	    	        }
	    	        if (vArray.size() > 1) {
	    	            MipavUtil.displayError("More than 1 curve in VOI");
	    	            setCompleted(false);
	    	            return;
	    	        }
				    VOIBase v  = vArray.get(0);
				    nPts = v.size();
					float xPts[] = new float[nPts];
					float yPts[] = new float[nPts];
					float zPts[] = new float[nPts];
					v.exportArrays(xPts, yPts, zPts);
					for (i = 0; i < nPts; i++) {
						xPts[i] = xPts[i] * xres;
						yPts[i] = yPts[i] * yres;
					}
					exactTotalPixelCount = 0.0;
					for (i = 0; i < nPts-1; i++) {
			            term = (xPts[i]*yPts[i+1] - xPts[i+1]*yPts[i]);
			            exactTotalPixelCount += term;
			            xcenter += (xPts[i] + xPts[i+1]) * term;
			            ycenter += (yPts[i] + yPts[i+1]) * term;
			        }
			        term =  (xPts[nPts-1]*yPts[0] - xPts[0]*yPts[nPts-1]);
			        exactTotalPixelCount += term;
			        exactTotalPixelCount = 0.5*Math.abs(exactTotalPixelCount);
			        xcenter += (xPts[nPts-1] + xPts[0]) * term;
			        ycenter += (yPts[nPts-1] + yPts[0]) * term;
			        xcenter = Math.abs(xcenter/(6.0 * exactTotalPixelCount));
			        ycenter = Math.abs(ycenter/(6.0 * exactTotalPixelCount));
			        System.out.println("xcenter = " + (xcenter/xres) + " ycenter = " + (ycenter/yres));
			        
			        Ix = 0.0;
			        Iy = 0.0;
			        Pxy = 0.0;
			        for (i = 0; i < nPts-1; i++) {
			            diffx1 = xPts[i] - xcenter;
			            diffx2 = xPts[i+1] - xcenter;
			            diffy1 = yPts[i] - ycenter;
			            diffy2 = yPts[i+1] - ycenter;
			            term = (diffx1*diffy2 - diffx2*diffy1);
			            Ix += (diffy1*diffy1 + diffy1*diffy2 + diffy2*diffy2) * term;
			            Iy += (diffx1*diffx1 + diffx1*diffx2 + diffx2*diffx2) * term;
			            Pxy += (diffx1*(2.0*diffy1 + diffy2) + diffx2*(diffy1 + 2.0*diffy2)) * term;
			        }
			        diffx1 = xPts[nPts-1] - xcenter;
			        diffx2 = xPts[0] - xcenter;
			        diffy1 = yPts[nPts-1] - ycenter;
			        diffy2 = yPts[0] - ycenter;
			        term = (diffx1*diffy2 - diffx2*diffy1);
			        Ix += (diffy1*diffy1 + diffy1*diffy2 + diffy2*diffy2) * term;
			        Iy += (diffx1*diffx1 + diffx1*diffx2 + diffx2*diffx2) * term;
			        Pxy += (diffx1*(2.0*diffy1 + diffy2) + diffx2*(diffy1 + 2.0*diffy2)) * term;
			        Ix = Ix/12.0;
			        Iy = Iy/12.0;
			        Pxy = Pxy/24.0;
    		    } // else if (method == VOI_METHOD)
		        
		        theta1 = 0.5*Math.atan2(2.0*Pxy, Iy-Ix);
		        if (theta1 >= 0.0) {
		            theta2 = theta1 - Math.PI/2.0;
		        }
		        else {
		            theta2 = theta1 + Math.PI/2.0;
		        }
		        Iave = (Ix + Iy)/2.0;
		        Idiff = (Ix - Iy)/2.0;
		        Ixp1 = Iave + Idiff*Math.cos(2.0*theta1) - Pxy*Math.sin(2.0*theta1);
		        Preferences.debug("Ixp1 = " + Ixp1 + "\n", Preferences.DEBUG_ALGORITHM);
		        Ixp2 = Iave + Idiff*Math.cos(2.0*theta2) - Pxy*Math.sin(2.0*theta2);
		        Preferences.debug("Ixp2 = " + Ixp2 + "\n", Preferences.DEBUG_ALGORITHM);
		        // Double check
		        R = Math.sqrt(Idiff*Idiff + Pxy*Pxy);
		        Imin = Iave - R;
		        Preferences.debug("Imin = " + Imin + "\n", Preferences.DEBUG_ALGORITHM);
		        Imax = Iave + R;
		        Preferences.debug("Imax = " + Imax + "\n", Preferences.DEBUG_ALGORITHM);
		        if (Math.abs(theta1) < Math.abs(theta2)) {
		        //if (Ixp1 < Ixp2) {
		            thetaZ = theta1;
		            // Ix' = Ixp1 = (1/12)(width *xres)*((height * yres)**3)
		            // Iy' = Ixp2 = (1/12)((width * xres)**3) *(height * yres)
		            // Ixp1/Ixp2 = ((height *yres)**2)/((width * xres) ** 2)
		            // (height * yres)**2 = (Ixp1/Ixp2) * (width * xres)**2
		            // (height * yres) = sqrt(Ixp1/Ixp2) * (width * xres)
		            // Ixp1 = (1/12)*((Ixp1/Ixp2)**1.5) * (width * xres)**4
		            // (width * xres)**4 = 12*(Ixp2**1.5)/sqrt(Ixp1)
		            // width = ((12 * (Ixp2**1.5)/sqrt(Ixp1))**0.25)/xres
		            // height = (12 * Ixp2)/(((width * xres)**3)*yres)
		            width = Math.pow((12 * (Math.pow(Ixp2,1.5))/Math.sqrt(Ixp1)),0.25)/xres;
		            System.out.println("width = " + width);
		            height = (12 * Ixp2)/(Math.pow((width * xres),3)*yres);
		            System.out.println("height = " + height);
		            
		        }
		        else {
		            thetaZ = theta2;
		            // Ix' = Ixp2 = (1/12)(width *xres)*((height * yres)**3)
		            // Iy' = Ixp1 = (1/12)((width * xres)**3) *(height * yres)
		            // Ixp2/Ixp1 = ((height *yres)**2)/((width * xres) ** 2)
		            // (height * yres)**2 = (Ixp2/Ixp1) * (width * xres)**2
		            // (height * yres) = sqrt(Ixp2/Ixp1) * (width * xres)
		            // Ixp2 = (1/12)*((Ixp2/Ixp1)**1.5) * (width * xres)**4
		            // (width * xres)**4 = 12*(Ixp1**1.5)/sqrt(Ixp2)
		            // width = ((12 * (Ixp1**1.5)/sqrt(Ixp2))**0.25)/xres
		            // height = (12 * Ixp1)/(((width * xres)**3)*yres)
		            width = Math.pow((12 * (Math.pow(Ixp1,1.5))/Math.sqrt(Ixp2)),0.25)/xres;
		            System.out.println("width = " + width);
		            height = (12 * Ixp1)/(Math.pow((width * xres),3)*yres);
		            System.out.println("height = " + height);
		        }
		        thetaZ = (180.0/Math.PI)*thetaZ;
		        System.out.println("thetaZ = " + thetaZ);
		        xfrm = new TransMatrix(3);
		        xfrm.identity();
		        xfrm.setTranslate(xcenter,ycenter);
		        xfrm.setRotate(-thetaZ);
		        xfrm.setTranslate(-xcenter,-ycenter);
    		} // else if ((Method == MASK_MEATHOD) || (method == VOI_METHOD))
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
	        if (method == VERTICES_METHOD) {
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
	        } // if (method == VOI_VERTICES)
	        else if (method == MASK_METHOD) {
	        	if ((srcImage.getType() != ModelImage.BOOLEAN) && (srcImage.getType() != ModelImage.UBYTE) &&
    	                (srcImage.getType() != ModelImage.BYTE) && (srcImage.getType() != ModelImage.USHORT) &&
    	                (srcImage.getType() != ModelImage.SHORT)) {
    	            displayError("Source Image must be Boolean, UByte, Byte, UShort, or Short");
    	            setCompleted(false);

    	            return;
    	        }
    			
    			volume = length * oZdim;
    			pixVolume = pixArea * zres;
	        	buffer = new short[volume];
    			try {
                    srcImage.exportData(0, volume, buffer);
                } catch (IOException error) {
                    displayError("Algorithm CropTilted: image bounds exceeded");
                    setCompleted(false);

                    return;
                }
    			xStart = (int)Math.round(x1);
    			yStart = (int)Math.round(y1);
    			zStart = (int)Math.round(z1);
                value = buffer[xStart + yStart*oXdim + zStart*length];
                found = true;
                xLow = Math.max(0,xStart-1);
                xHigh = Math.min(oXdim-1,xStart+1);
                yLow = Math.max(0,yStart-1);
                yHigh = Math.min(oYdim-1,yStart+1);
                zLow = Math.max(0,zStart-1);
                zHigh = Math.min(oZdim-1,zStart+1);
                nextXLow = xLow;
                nextXHigh = xHigh;
                nextYLow = yLow;
                nextYHigh = yHigh;
                nextZLow = zLow;
                nextZHigh = zHigh;
                xpos = new Vector<Float>();
                ypos = new Vector<Float>();
                zpos = new Vector<Float>();
                xpos.add(xStart*xres);
                ypos.add(yStart*yres);
                zpos.add(zStart*zres);
                havePos = new BitSet(volume);
                havePos.set(xStart + yStart*oXdim + zStart*length);
                while (found) {
                    found = false;
                    xLow = nextXLow;
                    xHigh = nextXHigh;
                    yLow = nextYLow;
                    yHigh = nextYHigh;
                    zLow = nextZLow;
                    zHigh = nextZHigh;
                    for (z = zLow; z <= zHigh; z++) {
	                    for (y = yLow; y <= yHigh; y++) {
	                    	for (x = xLow; x <= xHigh; x++) {
	                    	    index = x + oXdim * y + length * z;
	                    	    if ((buffer[index] == value) && (!havePos.get(index)) && 
	                    	    (((x >= 1) && havePos.get(index-1)) || ((x < oXdim - 1) && havePos.get(index+1)) ||
	                    	     ((y >= 1) && havePos.get(index-oXdim)) || ((y < oYdim - 1) && havePos.get(index+oXdim)) ||
	                    	     ((z >= 1) && havePos.get(index-length)) || ((z < oZdim - 1) && havePos.get(index+length)))) {
	                    	    	found = true;
	                    	    	havePos.set(index);
	                    	    	nPts++;
	                    	    	xpos.add(x*xres);
	                    	    	ypos.add(y*yres);
	                    	    	zpos.add(z*zres);
	                    	    	xcenter += x*xres;
	                    	    	ycenter += y*yres;
	                    	    	zcenter += z*zres;
	                    	    	if ((x == xLow)  && (nextXLow > 0) && (nextXLow == xLow)) {
	                    	    		nextXLow = xLow-1;
	                    	    	}
	                    	    	if ((x == xHigh) && (nextXHigh < oXdim-1) && (nextXHigh == xHigh)) {
	                    	    		nextXHigh = xHigh+1;
	                    	    	}
	                    	    	if ((y == yLow)  && (nextYLow > 0) && (nextYLow == yLow)) {
	                    	    		nextYLow = yLow-1;
	                    	    	}
	                    	    	if ((y == yHigh) && (nextYHigh < oYdim-1) && (nextYHigh == yHigh)) {
	                    	    		nextYHigh = yHigh+1;
	                    	    	}
	                    	    	if ((z == zLow)  && (nextZLow > 0) && (nextZLow == zLow)) {
	                    	    		nextZLow = zLow-1;
	                    	    	}
	                    	    	if ((z == zHigh) && (nextZHigh < oZdim-1) && (nextZHigh == zHigh)) {
	                    	    		nextZHigh = zHigh+1;
	                    	    	}
	                    	    }
	                    	}
	                    }
                    }
                } // while (found)
                xcenter = Math.abs(xcenter/nPts);
                ycenter = Math.abs(ycenter/nPts);
                zcenter = Math.abs(zcenter/nPts);
                System.out.println("xcenter = " + (xcenter/xres) + " ycenter = " + (ycenter/yres) + " zcenter = " + (zcenter/zres));
                for (i = 0; i < nPts; i++) {
                    xdiff = xpos.get(i) - xcenter;
                    ydiff = ypos.get(i) - ycenter;
                    zdiff = zpos.get(i) - zcenter;
                    Ix += (ydiff*ydiff + zdiff*zdiff)*pixVolume;
                    Iy += (xdiff*xdiff + zdiff*zdiff)*pixVolume;
                    Iz += (xdiff*xdiff + ydiff*ydiff)*pixVolume;
                    Pxy += xdiff*ydiff*pixVolume;
                    Pyz += ydiff*zdiff*pixVolume;
                    Pzx += zdiff*xdiff*pixVolume;
                } // for (i = 0; i < nPts; i++)	
                // K**3 - (Ix + Iy + Iz)K**2 + (IxIy + IyIz + IzIx - Pxy**2 - Pyz**2 - Pzx**2)K
                // - (IxIyIz - IxPyz**2 - IyPzx**2 - IzPxy**2 - 2PxyPyzPzx) = 0
                // is a cubic equation in K yielding 3 positive real roots, K1, K2, and K3
                // which are the principal moments of inertia for the given body.
                a1 = 1.0;
                b = -(Ix + Iy + Iz);
                c = (Ix*Iy + Iy*Iz + Iz*Ix - Pxy*Pxy - Pyz*Pyz - Pzx*Pzx);
                d = -(Ix*Iy*Iz - Ix*Pyz*Pyz - Iy*Pzx*Pzx - Iz*Pxy*Pxy - 2.0*Pxy*Pyz*Pzx);
                K1real = new double[1];
                K2real = new double[1];
                K2imag = new double[1];
                K3real = new double[1];
                K3imag = new double[1];
                result = new int[1];
                ce = new CubicEquation(a1, b, c, d, K1real, K2real, K2imag, K3real, K3imag, result);
                ce.run();
                if (result[0] == 1) {
                	MipavUtil.displayError("Cubic equation for principal moments of inertia gives 2 complex conjugate values");
                	setCompleted(false);
                	return;
                }
                // (Ix - K)dircosx - Pxydircosy - Pzxdircosz = 0
                // -Pxydircosx + (Iy - K)dircosy - Pyzdircosz = 0
                // Multiplying the first equation by -Pyz, the second equation by Pzx, and adding the 2
                // dircosy = dircosx*((Ix - K)Pyz + PxyPzx]/((Iy - K)Pzx + PxyPyz)
                // dircosz = ((Ix - K)dircosx  - Pxydircosy)/Pzx = ((Ix - K)dircosx - Pxyscalextoydircosx)/Pzx
                // dircosx**2 + dircosy**2 + dircosz**2 = 1
                // For root1:
                scalextoy = ((Ix - K1real[0])*Pyz + Pxy*Pzx)/((Iy - K1real[0])*Pzx + Pxy*Pyz);
                scalextoz = ((Ix - K1real[0]) - Pxy*scalextoy)/Pzx;
                dircosx1 = 1/Math.sqrt(1 + scalextoy*scalextoy + scalextoz*scalextoz);
                dircosy1 = dircosx1 * scalextoy;
                dircosz1 = dircosx1 * scalextoz;
                scalextoy = ((Ix - K2real[0])*Pyz + Pxy*Pzx)/((Iy - K2real[0])*Pzx + Pxy*Pyz);
                scalextoz = ((Ix - K2real[0]) - Pxy*scalextoy)/Pzx;
                dircosx2 = 1/Math.sqrt(1 + scalextoy*scalextoy + scalextoz*scalextoz);
                dircosy2 = dircosx2 * scalextoy;
                dircosz2 = dircosx2 * scalextoz;
                scalextoy = ((Ix - K3real[0])*Pyz + Pxy*Pzx)/((Iy - K3real[0])*Pzx + Pxy*Pyz);
                scalextoz = ((Ix - K3real[0]) - Pxy*scalextoy)/Pzx;
                dircosx3 = 1/Math.sqrt(1 + scalextoy*scalextoy + scalextoz*scalextoz);
                dircosy3 = dircosx1 * scalextoy;
                dircosz3 = dircosx1 * scalextoz;
                // I have 9 directional cosines, but I don't know if these correspond to positive or negative angles.
	        } // else if (method == MASK_METHOD)
	        
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
