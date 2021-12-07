package gov.nih.mipav.model.algorithms.utilities;


import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;
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
        int nPts = 0;
        Vector<Float>xpos;
        Vector<Float>ypos;
        Vector<Float>zpos;
        int x;
        int y;
        int z;
        int index;
        double xdiff;
        double ydiff;
        double zdiff;
        double dircosx1;
        double dircosy1;
        double dircosz1;
        double dircosx2;
        double dircosy2;
        double dircosz2;
        double dircosx3;
        double dircosy3;
        double dircosz3;
        double xaxisdircosx;
        double xaxisdircosy;
        double xaxisdircosz;
        double yaxisdircosx;
        double yaxisdircosy;
        double yaxisdircosz;
        double zaxisdircosx;
        double zaxisdircosy;
        double zaxisdircosz;
        double I1;
        double I2;
        double I3;
        double Ixp;
        double Iyp;
        double Izp;
        double volume;
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
    			if (method == MASK_METHOD) {
    				setMask(srcImage.generateVOIMask());
    				BitSet mask = srcImage.getMask();
    				xpos = new Vector<Float>();
	                ypos = new Vector<Float>();
	    			for (y = 0; y < oYdim; y++) {
	    				for (x = 0; x < oXdim; x++) {
	    					index = x + y * oXdim;
	    					if (mask.get(index)) {
	    						nPts++;
	    						xpos.add(x*xres);
	    			            ypos.add(y*yres);
	    			            xcenter += x*xres;
                    	    	ycenter += y*yres;
	    					}
	    				}
	    			}
	                
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
		        xcenter = xcenter /xres;
		        ycenter = ycenter / yres;
    		} // else if ((Method == MASK_METHOD) || (method == VOI_METHOD))
	        interp = AlgorithmTransform.BILINEAR;
	        
	        algoTrans = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oXdim, oYdim,
	                                           units, doVOI, doClip, doPad, doRotateCenter, center);
    	} // if (run2D)
    	else { // run3D
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
    			pixVolume = pixArea * zres;
	        	
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
    	        setMask(srcImage.generateVOIMask());
				BitSet mask = srcImage.getMask();
				xpos = new Vector<Float>();
                ypos = new Vector<Float>();
                zpos = new Vector<Float>();
                for (z = 0; z < oZdim; z++) {
	    			for (y = 0; y < oYdim; y++) {
	    				for (x = 0; x < oXdim; x++) {
	    					index = x + y * oXdim + z * length;
	    					if (mask.get(index)) {
	    						nPts++;
	    						xpos.add(x*xres);
	    			            ypos.add(y*yres);
	    			            zpos.add(z*zres);
	    			            xcenter += x*xres;
	                	    	ycenter += y*yres;
	                	    	zcenter += z*zres;
	    					}
	    				}
	    			}
                }
                xcenter = xcenter/nPts;
                ycenter = ycenter/nPts;
                zcenter = zcenter/nPts;
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
                /*double a1 = 1.0;
                double b = -(Ix + Iy + Iz);
                double c = (Ix*Iy + Iy*Iz + Iz*Ix - Pxy*Pxy - Pyz*Pyz - Pzx*Pzx);
                double d = -(Ix*Iy*Iz - Ix*Pyz*Pyz - Iy*Pzx*Pzx - Iz*Pxy*Pxy - 2.0*Pxy*Pyz*Pzx);
                double K1real[] = new double[1];
                double K2real[] = new double[1];
                double K2imag[] = new double[1];
                double K3real[] = new double[1];
                double K3imag[] = new double[1];
                int result[] = new int[1];
                CubicEquation ce = new CubicEquation(a1, b, c, d, K1real, K2real, K2imag, K3real, K3imag, result);
                ce.run();
                if (result[0] == 1) {
                	MipavUtil.displayError("Cubic equation for principal moments of inertia gives 2 complex conjugate values");
                	setCompleted(false);
                	return;
                }
               
                System.out.println("K1real = " + K1real[0]);
                System.out.println("K2real = " + K2real[0]);
                System.out.println("K3real = " + K3real[0]);*/
                double arr[][] = new double[3][3];
                arr[0][0] = Ix;
                arr[0][1] = -Pxy;
                arr[0][2] = -Pzx;
                arr[1][0] = -Pxy;
                arr[1][1] = Iy;
                arr[1][2] = -Pyz;
                arr[2][0] = -Pzx;
                arr[2][1] = -Pyz;
                arr[2][2] = Iz;
                double eigenvalue[] = new double[3]; // Number of column dimensions
                double eigenvector[][] = new double[3][3]; // [number of row dimensions][number of column dimensions]
                // In EigenvalueDecomposition the columns represent the
                // eigenvectors
                Eigenvalue.decompose( arr, eigenvector, eigenvalue);
                dircosx1 = eigenvector[0][0];
                dircosy1 = eigenvector[0][1];
                dircosz1 = eigenvector[0][2];
                dircosx2 = eigenvector[1][0];
                dircosy2 = eigenvector[1][1];
                dircosz2 = eigenvector[1][2];
                dircosx3 = eigenvector[2][0];
                dircosy3 = eigenvector[2][1];
                dircosz3 = eigenvector[2][2];
                // The roots I1, I2, and I3 are the principal moments of inertia of the body.
                I1 = eigenvalue[0];
                I2 = eigenvalue[1];
                I3 = eigenvalue[2];
               
                
                if ((Math.abs(dircosx1) >= Math.abs(dircosx2)) && (Math.abs(dircosx1) >= Math.abs(dircosx3))) {
                	xaxisdircosx = dircosx1;
                	xaxisdircosy = dircosy1;
                	xaxisdircosz = dircosz1;
                	Ixp = I1;
                	if (Math.abs(dircosy2) >= Math.abs(dircosy3)) {
                		yaxisdircosx = dircosx2;
                		yaxisdircosy = dircosy2;
                		yaxisdircosz = dircosz2;
                		Iyp = I2;
                		zaxisdircosx = dircosx3;
                		zaxisdircosy = dircosy3;
                		zaxisdircosz = dircosz3;
                		Izp = I3;
                	}
                	else {
                		yaxisdircosx = dircosx3;
                		yaxisdircosy = dircosy3;
                		yaxisdircosz = dircosz3;
                		Iyp = I3;
                		zaxisdircosx = dircosx2;
                		zaxisdircosy = dircosy2;
                		zaxisdircosz = dircosz2;
                		Izp = I2;
                	}
                } // if ((Math.abs(dircosx1) >= Math.abs(dircosx2)) && (Math.abs(dircosx1) >= Math.abs(dircosx3)))
                else if (Math.abs(dircosx2) >= Math.abs(dircosx1)  && (Math.abs(dircosx2) >= Math.abs(dircosx3))) {
                	xaxisdircosx = dircosx2;
                	xaxisdircosy = dircosy2;
                	xaxisdircosz = dircosz2;
                	Ixp = I2;
                	if (Math.abs(dircosy1) >= Math.abs(dircosy3)) {
                		yaxisdircosx = dircosx1;
                		yaxisdircosy = dircosy1;
                		yaxisdircosz = dircosz1;
                		Iyp = I1;
                		zaxisdircosx = dircosx3;
                		zaxisdircosy = dircosy3;
                		zaxisdircosz = dircosz3;
                		Izp = I3;
                	}
                	else {
                		yaxisdircosx = dircosx3;
                		yaxisdircosy = dircosy3;
                		yaxisdircosz = dircosz3;
                		Iyp = I3;
                		zaxisdircosx = dircosx1;
                		zaxisdircosy = dircosy1;
                		zaxisdircosz = dircosz1;	
                		Izp = I1;
                	}
                } // else if (Math.abs(dircosx2) >= Math.abs(dircosx1)  && (Math.abs(dircosx2) >= Math.abs(dircosx3))) {
                else {
                	xaxisdircosx = dircosx3;
                	xaxisdircosy = dircosy3;
                	xaxisdircosz = dircosz3;
                	Ixp = I3;
                	if (Math.abs(dircosy1) >= Math.abs(dircosy2)) {
                		yaxisdircosx = dircosx1;
                		yaxisdircosy = dircosy1;
                		yaxisdircosz = dircosz1;
                		Iyp = I1;
                		zaxisdircosx = dircosx2;
                		zaxisdircosy = dircosy2;
                		zaxisdircosz = dircosz2;
                		Izp = I2;
                	}
                	else {
                		yaxisdircosx = dircosx2;
                		yaxisdircosy = dircosy2;
                		yaxisdircosz = dircosz2;
                		Iyp = I2;
                		zaxisdircosx = dircosx1;
                		zaxisdircosy = dircosy1;
                		zaxisdircosz = dircosz1;
                		Izp = I1;
                	}
                }
                System.out.println("Ixp = " + Ixp);
                System.out.println("Iyp = " + Iyp);
                System.out.println("Izp = " + Izp);
                System.out.println("xaxisdircosx = " + xaxisdircosx + " xaxisdircosy = " + xaxisdircosy + " xaxisdircosz = " + xaxisdircosz);
                System.out.println("yaxisdircosx = " + yaxisdircosx + " yaxisdircosy = " + yaxisdircosy + " yaxisdircosz = " + yaxisdircosz);
                System.out.println("zaxisdircosx = " + zaxisdircosx + " zaxisdircosy = " + zaxisdircosy + " zaxisdircosz = " + zaxisdircosz);
                // volume = width*xres*height*yres*depth*zres = nPts*xres*yres*zres;
                // Ixp, Iyp, and Izp are the principal moments of inertia of a rectangular prism
                // Ixp = (1.0/12.0)*(height*height*yres*yres + depth*depth*zres*zres)*volume
                // Iyp = (1.0/12.0)*(width*width*xres*xres + depth*depth*zres*zres)*volume
                // Izp = (1.0/12.0)*(width*width*xres*xres + height*height*yres*yres)*volume
                // Iyp - Ixp = (1.0/12.0)*(width*width*xres*xres - height*height*yres*yres)*volume
                // Iyp - Ixp + Izp = (1.0/12.0)*(2*width*width*xres*xres)*volume = (1.0/6.0)*(width*width*xres*xres)*volume
                // width*xres = sqrt((6.0/volume)*(Iyp - Ixp  + Izp))
                // width = sqrt((6.0/volume)*(Iyp - Ixp + Izp))/xres
                // Ixp - Iyp = (1.0/12.0)*(height*height*yres*yres  - width*width*xres*xres)*volume
                // Ixp - Iyp + Izp = (1.0/12.0)*(2.0*height*height*yres*yres)*volume = (1.0/6.0)*(height*height*yres*yres)*volume
                // height*yres = sqrt((6.0/volume)*(Ixp - Iyp + Izp))
                // height = sqrt((6.0/volume)*(Ixp - Iyp + Izp))/yres
                // Ixp - Izp = (1.0/12.0)*(depth*depth*zres*zres - width*width*xres*xres)*volume
                // Ixp - Izp + Iyp = (1.0/12.0)*(2*depth*depth*zres*zres)*volume = (1.0/6.0)(depth*depth*zres*zres)*volume
                // depth*zres = sqrt((6.0/volume)*(Ixp - Izp + Iyp))
                // depth = sqrt((6.0/volume)*(Ixp - Izp + Iyp))/zres
                volume = nPts * xres * yres * zres;
                width = Math.sqrt((6.0/volume)*(Iyp - Ixp + Izp))/xres;
                System.out.println("width = " + width);
                height = Math.sqrt((6.0/volume)*(Ixp - Iyp + Izp))/yres;
                System.out.println("height = " + height);
                depth = Math.sqrt((6.0/volume)*(Ixp - Izp + Iyp))/zres;
                System.out.println("depth = " + depth);
                
                /*double del = 0.1* Math.min(oXres,Math.min(oYres,oZres));
                // Find maximum width point
                int newx = (int)Math.round(xcenter/xres);
                int newy = (int)Math.round(ycenter/yres);
                int newz = (int)Math.round(zcenter/zres);
                double finalx1 = xcenter;
                double finaly1 = ycenter;
                double finalz1 = zcenter;
                index = newx + newy * oXdim + newz * length;
                i = 1;
                while (mask.get(index)) {
                	finalx1 = xcenter + (i-1) * del * xaxisdircosx;
                	finaly1 = ycenter + (i-1) * del * xaxisdircosy;
                	finalz1 = zcenter + (i-1) * del * xaxisdircosz;
                    newx = (int)Math.round((xcenter + i * del * xaxisdircosx)/xres);
                    newy = (int)Math.round((ycenter + i * del * xaxisdircosy)/yres);
                    newz = (int)Math.round((zcenter + i * del * xaxisdircosz)/zres);
                    index = newx + newy * oXdim + newz * length;
                    i++;
                }
                // Find minimum width point
                newx = (int)Math.round(xcenter/xres);
                newy = (int)Math.round(ycenter/yres);
                newz = (int)Math.round(zcenter/zres);
                double finalx2 = xcenter;
                double finaly2 = ycenter;
                double finalz2 = zcenter;
                index = newx + newy * oXdim + newz * length;
                i = 1;
                while (mask.get(index)) {
                	finalx2 = xcenter + (i-1) * del * -xaxisdircosx;
                	finaly2 = ycenter + (i-1) * del * -xaxisdircosy;
                	finalz2 = zcenter + (i-1) * del * -xaxisdircosz;
                    newx = (int)Math.round((xcenter + i * del * -xaxisdircosx)/xres);
                    newy = (int)Math.round((ycenter + i * del * -xaxisdircosy)/yres);
                    newz = (int)Math.round((zcenter + i * del * -xaxisdircosz)/zres);
                    index = newx + newy * oXdim + newz * length;
                    i++;
                }
                double distx = finalx2 - finalx1;
                double disty = finaly2 - finaly1;
                double distz = finalz2 - finalz1;
                width = Math.sqrt(distx*distx + disty*disty + distz*distz)/xres;
                System.out.println("x1 = " + (finalx1/xres) + " x2 = " + (finalx2/xres));
                System.out.println("width = " + width);
                
                // Find maximum height point
                newx = (int)Math.round(xcenter/xres);
                newy = (int)Math.round(ycenter/yres);
                newz = (int)Math.round(zcenter/zres);
                finalx1 = xcenter;
                finaly1 = ycenter;
                finalz1 = zcenter;
                index = newx + newy * oXdim + newz * length;
                i = 1;
                while (mask.get(index)) {
                	finalx1 = xcenter + (i-1) * del * yaxisdircosx;
                	finaly1 = ycenter + (i-1) * del * yaxisdircosy;
                	finalz1 = zcenter + (i-1) * del * yaxisdircosz;
                    newx = (int)Math.round((xcenter + i * del * yaxisdircosx)/xres);
                    newy = (int)Math.round((ycenter + i * del * yaxisdircosy)/yres);
                    newz = (int)Math.round((zcenter + i * del * yaxisdircosz)/zres);
                    index = newx + newy * oXdim + newz * length;
                    i++;
                }
                // Find minimum height point
                newx = (int)Math.round(xcenter/xres);
                newy = (int)Math.round(ycenter/yres);
                newz = (int)Math.round(zcenter/zres);
                finalx2 = xcenter;
                finaly2 = ycenter;
                finalz2 = zcenter;
                index = newx + newy * oXdim + newz * length;
                i = 1;
                while (mask.get(index)) {
                	finalx2 = xcenter + (i-1) * del * -yaxisdircosx;
                	finaly2 = ycenter + (i-1) * del * -yaxisdircosy;
                	finalz2 = zcenter + (i-1) * del * -yaxisdircosz;
                    newx = (int)Math.round((xcenter + i * del * -yaxisdircosx)/xres);
                    newy = (int)Math.round((ycenter + i * del * -yaxisdircosy)/yres);
                    newz = (int)Math.round((zcenter + i * del * -yaxisdircosz)/zres);
                    index = newx + newy * oXdim + newz * length;
                    i++;
                }
                distx = finalx2 - finalx1;
                disty = finaly2 - finaly1;
                distz = finalz2 - finalz1;
                System.out.println("y1 = " + (finaly1/yres) + " y2 = " + (finaly2/yres));
                height = Math.sqrt(distx*distx + disty*disty + distz*distz)/yres;
                System.out.println("height = " + height);
                
                // Find maximum depth point
                newx = (int)Math.round(xcenter/xres);
                newy = (int)Math.round(ycenter/yres);
                newz = (int)Math.round(zcenter/zres);
                finalx1 = xcenter;
                finaly1 = ycenter;
                finalz1 = zcenter;
                index = newx + newy * oXdim + newz * length;
                i = 1;
                while (mask.get(index)) {
                	finalx1 = xcenter + (i-1) * del * zaxisdircosx;
                	finaly1 = ycenter + (i-1) * del * zaxisdircosy;
                	finalz1 = zcenter + (i-1) * del * zaxisdircosz;
                    newx = (int)Math.round((xcenter + i * del * zaxisdircosx)/xres);
                    newy = (int)Math.round((ycenter + i * del * zaxisdircosy)/yres);
                    newz = (int)Math.round((zcenter + i * del * zaxisdircosz)/zres);
                    index = newx + newy * oXdim + newz * length;
                    i++;
                }
                // Find minimum depth point
                newx = (int)Math.round(xcenter/xres);
                newy = (int)Math.round(ycenter/yres);
                newz = (int)Math.round(zcenter/zres);
                finalx2 = xcenter;
                finaly2 = ycenter;
                finalz2 = zcenter;
                index = newx + newy * oXdim + newz * length;
                i = 1;
                while (mask.get(index)) {
                	finalx2 = xcenter + (i-1) * del * -zaxisdircosx;
                	finaly2 = ycenter + (i-1) * del * -zaxisdircosy;
                	finalz2 = zcenter + (i-1) * del * -zaxisdircosz;
                    newx = (int)Math.round((xcenter + i * del * -zaxisdircosx)/xres);
                    newy = (int)Math.round((ycenter + i * del * -zaxisdircosy)/yres);
                    newz = (int)Math.round((zcenter + i * del * -zaxisdircosz)/zres);
                    index = newx + newy * oXdim + newz * length;
                    i++;
                }
                distx = finalx2 - finalx1;
                disty = finaly2 - finaly1;
                distz = finalz2 - finalz1;
                depth = Math.sqrt(distx*distx + disty*disty + distz*distz)/zres;
                System.out.println("depth = " + depth);*/
                
                Vector3d[] row = new Vector3d[3];
                row[0] = new Vector3d(xaxisdircosx, xaxisdircosy, xaxisdircosz);
                row[1] = new Vector3d(yaxisdircosx, yaxisdircosy, yaxisdircosz);
                row[2] = new Vector3d(zaxisdircosx, zaxisdircosy, zaxisdircosz);
                Vector3d scale = new Vector3d();
                // Compute X scale factor and normalize first row.
                scale.X = row[0].length();

                // row[0] = *V3Scale(&row[0], 1.0);
                row[0].scale(1.0);
                
                // Now, compute Y scale and normalize 2nd row.
                scale.Y = row[1].length();
                row[1].scale(1.0);
                
                // Next, get Z scale and normalize 3rd row.
                scale.Z = row[2].length();
                row[2].scale(1.0);
                Vector3d pdum3 = new Vector3d();
                
                // At this point, the matrix (in rows[]) is orthonormal.
                // Check for a coordinate system flip.  If the determinant
                // is -1, then negate the matrix and the scaling factors.
                pdum3 = Vector3d.cross( row[1], row[2] );
                if (row[0].dot(pdum3) < 0) {
                    scale.neg();
                    for (i = 0; i < 3; i++) {
                        row[i].X *= -1;
                        row[i].Y *= -1;
                        row[i].Z *= -1;
                    }
                }
                
                thetaY = Math.asin(row[0].Z);

                if (Math.cos(thetaY) != 0) {
                    thetaX = -Math.atan2(row[1].Z, row[2].Z);
                    thetaZ = -Math.atan2(row[0].Y, row[0].X);
                } else {
                    thetaX = Math.atan2(row[2].Y, row[1].Y);
                    thetaZ = 0;
                }
                if (thetaX < -Math.PI/2.0) {
                	thetaX = thetaX + Math.PI;
                }
                else if (thetaX > Math.PI/2.0) {
                	thetaX = thetaX - Math.PI;
                }             
                if (thetaY < -Math.PI/2.0) {
                	thetaY = thetaY + Math.PI;
                }
                else if (thetaY > Math.PI/2.0) {
                	thetaY = thetaY - Math.PI;
                }
                if (thetaZ < -Math.PI/2.0) {
                	thetaZ = thetaZ + Math.PI;
                }
                else if (thetaZ > Math.PI/2.0) {
                	thetaZ = thetaZ - Math.PI;
                }
                
                System.out.println("thetaX = " + (180.0/Math.PI)*thetaX);
                System.out.println("thetaY = " + (180.0/Math.PI)*thetaY);
                System.out.println("thetaZ = " + (180.0/Math.PI)*thetaZ);
                
                xfrm = new TransMatrix(4);
		        xfrm.identity();
		        xfrm.setTranslate(xcenter, ycenter, zcenter);
		        xfrm.setRotate(thetaX,thetaY,thetaZ,RADIANS);
		        xfrm.setTranslate(-xcenter, -ycenter, -zcenter);
		        xcenter = xcenter /xres;
		        ycenter = ycenter / yres;
		        zcenter = zcenter /zres;
	        } // else if (method == MASK_METHOD)
	        
	        interp = AlgorithmTransform.TRILINEAR;
	        
	        algoTrans = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim,
	                                           units, doVOI, doClip, doPad, doRotateCenter, center);	
    	} // else run3D
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
