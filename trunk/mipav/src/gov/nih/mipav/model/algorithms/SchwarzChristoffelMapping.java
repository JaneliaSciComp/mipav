package gov.nih.mipav.model.algorithms;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.jama.ComplexLinearEquations;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentBase;
import gov.nih.mipav.view.ViewJComponentGraph;
import gov.nih.mipav.view.ViewJFrameGraph;

public class SchwarzChristoffelMapping extends AlgorithmBase implements MouseListener {
	
	// This is a port of portions of the Schwarz-Christoffel Toolbox from MATLAB to Java
	// with the kind permission of Professor Toby Driscoll.  The original code is:
	// Version 2.3   January 15, 2003
	// Copyright (c) 1994-2003 by Toby Driscoll (driscoll@math.udel.edu).

	// How much progress information to show during and after the solution to the parameter problem.
	//private boolean traceSolution = false;
	
	// Desired accuracy in the map.  This may not be met exactly.
	private double tolerance = 1.0E-8;
	
	// eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
	private double eps;
	
	private final Lock accessLock = new ReentrantLock();
	private final Condition canProcessMouseClick = accessLock.newCondition();
	private int xClick;
	private int yClick;
	
	// polygon vertices
	// w[i][0] x coordinate of ith vertex
	// w[i][1] y coordinate of ith vertex
	private double w[][];
	
	Vector<Double> linhx[][] = null;
	Vector<Double> linhy[][] = null;
	
	private double xSource[] = null;
	private double ySource[] = null;
	private int corners[] = null;
	private final int POLYGON_TO_RECTANGLE = 1;
	private final int POLYGON_TO_CIRCLE = 2;
	private final int CROSSRATIO_POLYGON_TO_CIRCLE = 3;
	private final int POLYGON_EXTERIOR_TO_CIRCLE = 4;
	private final int CROSSRATIO_POLYGON_TO_RECTANGLE = 5;
	private int algorithm = POLYGON_TO_RECTANGLE;
	private boolean setCenter;
	private double xCenter;
	private double yCenter;
	
	private double crsplit_neww[][] = null;
	private boolean crsplit_orig[] = null;
	
	private int crtriang_edge[][] = null;
	private int crtriang_triedge[][] = null;
	private int crtriang_edgetri[][] = null;
	
	private double crparam_beta[] = null;
	private double crparam_cr[] = null;
	private double crparam_qdata[][] = null;
	
	private double craffine_aff[][][] = null;
	private double craffine_wn[][] = null;
	
	private qlgraph crqgraph_Q = new qlgraph();
	
	private int crfixwc_quadnum;
	private double crfixwc_mt[][] = new double[4][2];
	private double crfixwc[] = new double[2];
	
	private boolean testRoutine = false;
	private boolean exterRoutine = false;
	
	public SchwarzChristoffelMapping() {
		
	}
	
	public SchwarzChristoffelMapping(ModelImage destImg, ModelImage srcImg, double w[][]) {
		super(destImg, srcImg);
		this.w = w;
	}
	
	public SchwarzChristoffelMapping(ModelImage destImg, ModelImage srcImg, double xSource[], double ySource[], int algorithm, boolean setCenter, double xCenter, double yCenter) {
		super(destImg, srcImg);
		this.xSource = xSource;
		this.ySource = ySource;
		this.algorithm = algorithm;
		this.setCenter = setCenter;
		this.xCenter = xCenter;
		this.yCenter = yCenter;
	}
	
	public SchwarzChristoffelMapping(ModelImage destImg, ModelImage srcImg, double xSource[], double ySource[], int corners[], int algorithm) {
		super(destImg, srcImg);
		this.xSource = xSource;
		this.ySource = ySource;
		this.corners = corners;
		this.algorithm = algorithm;
	}
	
	public void runAlgorithm() {
		// eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        eps = 1.0;
        double neweps = 1.0;


        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                eps = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)
        
        
        if (testRoutine) {
        	//testRectmap1();
        	//testRectmap2();
        	//testRectmap3();
            //testDiskmap1();
            //testCRDiskmap1();
        	//testDiskmap2();
        	//testCRDiskmap2();
        	//testDiskmap3();
        	//testCRDiskmap3();
        	//testDiskmap4();
        	// No testCRDiskmap4() because CRDisk cannot handle infinities in example.
        	//testDiskmap5();
        	//testCRDiskmap5();
            //testDiskmap6();
        	//testDiskmap7();
            //testCRDiskmap7();
        	//testExtermap1();
        	//testExtermap2();
        	//testCRRectmap1();
        	//testCRRectmap2();
        	testCRRectmap3();
            return;
        }
		if (algorithm == POLYGON_TO_RECTANGLE) {
			runPolygonToRectangle();
		}
		else if (algorithm == CROSSRATIO_POLYGON_TO_RECTANGLE) {
			runCrossRatioPolygonToRectangle();
		}
		else if (algorithm == POLYGON_TO_CIRCLE) {
			runPolygonToCircle();
		}
		else if (algorithm == CROSSRATIO_POLYGON_TO_CIRCLE) {
			runCrossRatioPolygonToCircle();
		}
		else if (algorithm == POLYGON_EXTERIOR_TO_CIRCLE) {
			runPolygonExteriorToCircle();
		}
	}
	
	public void setEps(double eps) {
		this.eps = eps;
	}
	
	public void runPolygonToCircle() {
		int i, j;
		int index;
		int cf;
		int xDimSource;
		int yDimSource;
		int sourceSlice;
		int xDimDest;
		int yDimDest;
		int destSlice;
		double srcBuffer[];
		double destBuffer[];
		double imageMin;
		int x;
		int y; 
		int c;
		double zp[][];
		double wp[][];
		double xp;
		double yp;
		int x0;
		int y0;
		int deltaX;
		int deltaY;
		double dx;
		double dy;
		double dx1;
		double dy1;
		int position;
		
		if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Polygon to circle ...");

        if (srcImage.isColorImage()) {
            cf = 4;
        } else {
            cf = 1;
        }
        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource;

        xDimDest = destImage.getExtents()[0];
        yDimDest = destImage.getExtents()[1];
        destSlice = xDimDest * yDimDest;
        srcBuffer = new double[cf * sourceSlice];

        try {
            srcImage.exportData(0, cf * sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }
        // Invert so y axis increases going up so can use ccw ordering
        double srcBuffer2[] = new double[cf *sourceSlice];
        for (y = 0; y < yDimSource; y++) {
        	for (x = 0; x < xDimSource; x++) {
        		if (cf == 1) {
        	        srcBuffer2[x + (yDimSource - 1 - y)*xDimSource]	= srcBuffer[ x + y*xDimSource];
        		}
        		else {
        		    for (c = 0; c < 4; c++) {
        		    	srcBuffer2[4*(x + (yDimSource - 1 - y)*xDimSource) + c]	= srcBuffer[4*(x + y*xDimSource) + c];	
        		    }
        		}
        	}
        }

        destBuffer = new double[cf * destSlice];

        if (!srcImage.isColorImage()) {
            imageMin = srcImage.getMin();

            for (i = 0; i < destSlice; i++) {
                destBuffer[i] = imageMin;
            }
        } // if (!srcImage.isColorImage())
        double w[][] = new double[xSource.length][2];
        for (i = 0; i < xSource.length; i++) {
        	w[i][0] = xSource[i];
        	// Invert so y axis increases going up so can use ccw ordering
        	w[i][1] = yDimSource-1-ySource[i];
        }
        scmap M = diskmap(w, null, tolerance, null, null);
        if (setCenter) {
        	double wc[] = new double[2];
        	wc[0] = xCenter;
        	wc[1] = yDimSource - 1 - yCenter;
        	M = center(M, wc, null);
        }
		for (i = 0; i < M.prevertex.length; i++) {
			System.out.println("prevertex["+i+"] = " + M.prevertex[i][0] + " " + M.prevertex[i][1]+"i");
		}
		System.out.println("center = " + M.center[0] + " " + (yDimSource - 1 - M.center[1])+"i");
		System.out.println("c = " + M.constant[0] + " " + M.constant[1]+"i");
		diskplot(M, null, null, 200, 140, null, yDimSource-1);
		double xcenterDest = (xDimDest-1.0)/2.0;
		double ycenterDest = (yDimDest-1.0)/2.0;
		double maxDistance = Math.min(xcenterDest,ycenterDest);
		//zp = new double[destSlice][2];
		boolean idx[] = new boolean[destSlice];
		j = 0;
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		double distX = (x-xcenterDest)/maxDistance;
        		double distY = (y-ycenterDest)/maxDistance;
        		if ((distX*distX + distY*distY) < 1.0) {
        			idx[index] = true;
        			j++;
        		}
        	}
        } // for (y = 0; y < yDimDest; y++)
        zp = new double[j][2];
        j = 0;
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		double distX = (x-xcenterDest)/maxDistance;
        		double distY = (y-ycenterDest)/maxDistance;
        		if ((distX*distX + distY*distY) < 1.0) {
        			zp[j][0] = distX;
        			zp[j][1] = distY;
        			j++;
        		}
        	}
        } // for (y = 0; y < yDimDest; y++)
        polygon p = M.poly;
        double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
        wp = dmap(zp, w, beta, M.prevertex, M.constant, M.qdata);
        
        j = 0;
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		if (idx[index]) {
        			xp = wp[j][0];
        			yp = wp[j][1];
        			j++;
					if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5)) {
		 	    	   if (xp <= 0) {
		                    x0 = 0;
		                    dx = 0;
		                    deltaX = 0;
		                } else if (xp >= (xDimSource - 1)) {
		                    x0 = xDimSource - 1;
		                    dx = 0;
		                    deltaX = 0;
		                } else {
		                    x0 = (int) xp;
		                    dx = xp - x0;
		                    deltaX = 1;
		                }
		
		                if (yp <= 0) {
		                    y0 = 0;
		                    dy = 0;
		                    deltaY = 0;
		                } else if (yp >= (yDimSource - 1)) {
		                    y0 = yDimSource - 1;
		                    dy = 0;
		                    deltaY = 0;
		                } else {
		                    y0 = (int) yp;
		                    dy = yp - y0;
		                    deltaY = xDimSource;
		                }
		
		                dx1 = 1 - dx;
		                dy1 = 1 - dy;
		
		                position = (y0 * xDimSource) + x0;
		
		                if (cf == 1) {
		                    destBuffer[index] = (dy1 * ( (dx1 * srcBuffer2[position]) + (dx * srcBuffer2[position + deltaX])))
		                        + (dy * ( (dx1 * srcBuffer2[position + deltaY]) + (dx * srcBuffer2[position + deltaY + deltaX])));   
		                }
		                else {
		                	 for (c = 0; c < 4; c++) {
		                         destBuffer[4*index+c] = (dy1 * ( (dx1 * srcBuffer2[4*position]+c) + (dx * srcBuffer2[4*(position + deltaX) + c])))
		                                 + (dy * ( (dx1 * srcBuffer2[4*(position + deltaY) + c]) + (dx * srcBuffer2[4*(position + deltaY + deltaX) + c])));
		                     } // for (c = 0; c < 4; c++)	
		                }
		 	       } // if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5))
        		}
        	}
		} // for (y = 0; y < yDimDest; y++)
		
	// Undo y axis inversion
    double destBuffer2[] = new double[cf * destSlice];
	for (y = 0; y < yDimDest; y++) {
    	for (x = 0; x < xDimDest; x++) {
    		if (cf == 1) {
    	        destBuffer2[x + (yDimDest - 1 - y)*xDimDest]	= destBuffer[ x + y*xDimDest];
    		}
    		else {
    		    for (c = 0; c < 4; c++) {
    		    	destBuffer2[4*(x + (yDimDest - 1 - y)*xDimDest) + c]	= destBuffer[4*(x + y*xDimDest) + c];
    		    }
    		}
    	}
    }
    

       
       try {
           destImage.importData(0, destBuffer2, true);
       } catch (IOException e) {
           MipavUtil.displayError("IOException " + e + " on destImage.importData");

           setCompleted(false);

           return;
       }

       setCompleted(true);

       return;
	}
	
	public void runCrossRatioPolygonToCircle() {
		int i, j;
		int index;
		int cf;
		int xDimSource;
		int yDimSource;
		int sourceSlice;
		int xDimDest;
		int yDimDest;
		int destSlice;
		double srcBuffer[];
		double destBuffer[];
		double imageMin;
		int x;
		int y; 
		int c;
		double zp[][];
		double wp[][];
		double xp;
		double yp;
		int x0;
		int y0;
		int deltaX;
		int deltaY;
		double dx;
		double dy;
		double dx1;
		double dy1;
		int position;
		
		if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Cross-ratio polygon to circle ...");

        if (srcImage.isColorImage()) {
            cf = 4;
        } else {
            cf = 1;
        }
        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource;

        xDimDest = destImage.getExtents()[0];
        yDimDest = destImage.getExtents()[1];
        destSlice = xDimDest * yDimDest;
        srcBuffer = new double[cf * sourceSlice];

        try {
            srcImage.exportData(0, cf * sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }
        // Invert so y axis increases going up so can use ccw ordering
        double srcBuffer2[] = new double[cf *sourceSlice];
        for (y = 0; y < yDimSource; y++) {
        	for (x = 0; x < xDimSource; x++) {
        		if (cf == 1) {
        	        srcBuffer2[x + (yDimSource - 1 - y)*xDimSource]	= srcBuffer[ x + y*xDimSource];
        		}
        		else {
        		    for (c = 0; c < 4; c++) {
        		    	srcBuffer2[4*(x + (yDimSource - 1 - y)*xDimSource) + c]	= srcBuffer[4*(x + y*xDimSource) + c];	
        		    }
        		}
        	}
        }

        destBuffer = new double[cf * destSlice];

        if (!srcImage.isColorImage()) {
            imageMin = srcImage.getMin();

            for (i = 0; i < destSlice; i++) {
                destBuffer[i] = imageMin;
            }
        } // if (!srcImage.isColorImage())
        double w[][] = new double[xSource.length][2];
        for (i = 0; i < xSource.length; i++) {
        	w[i][0] = xSource[i];
        	// Invert so y axis increases going up so can use ccw ordering
        	w[i][1] = yDimSource-1-ySource[i];
        }
        double xa[] = new double[w.length];
		double ya[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			xa[i] = w[i][0];
			ya[i] = w[i][1];
		}
		polygon poly = new polygon(xa, ya, null);
        scmap M = crdiskmap(poly, tolerance, null, null);
        if (setCenter) {
        	double wc[] = new double[2];
        	wc[0] = xCenter;
        	wc[1] = yDimSource - 1 - yCenter;
        	M = crdiskCenter(M, wc);
        }
		System.out.println("center = " + M.center[0] + " " + (yDimSource - 1 - M.center[1])+"i");
		boolean drawThetaToRadiusOne = true;
		crdiskplot(M, null, null, 200, 140, drawThetaToRadiusOne, null, yDimSource-1);
		double xcenterDest = (xDimDest-1.0)/2.0;
		double ycenterDest = (yDimDest-1.0)/2.0;
		double maxDistance = Math.min(xcenterDest,ycenterDest);
		boolean idx[] = new boolean[destSlice];
		j = 0;
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		double distX = (x-xcenterDest)/maxDistance;
        		double distY = (y-ycenterDest)/maxDistance;
        		if ((distX*distX + distY*distY) < 1.0) {
        			idx[index] = true;
        			j++;
        		}
        	}
        } // for (y = 0; y < yDimDest; y++)
        zp = new double[j][2];
        j = 0;
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		double distX = (x-xcenterDest)/maxDistance;
        		double distY = (y-ycenterDest)/maxDistance;
        		if ((distX*distX + distY*distY) < 1.0) {
        			zp[j][0] = distX;
        			zp[j][1] = distY;
        			j++;
        		}
        	}
        } // for (y = 0; y < yDimDest; y++)
        polygon p = M.poly;
        double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
        wp = crmap(zp,w,beta,M.crossratio,M.affine,M.center_fix_quadnum, M.center_fix_mt, M.qgraph, M.qdata);
        
        j = 0;
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		if (idx[index]) {
        			xp = wp[j][0];
        			yp = wp[j][1];
        			j++;
					if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5)) {
		 	    	   if (xp <= 0) {
		                    x0 = 0;
		                    dx = 0;
		                    deltaX = 0;
		                } else if (xp >= (xDimSource - 1)) {
		                    x0 = xDimSource - 1;
		                    dx = 0;
		                    deltaX = 0;
		                } else {
		                    x0 = (int) xp;
		                    dx = xp - x0;
		                    deltaX = 1;
		                }
		
		                if (yp <= 0) {
		                    y0 = 0;
		                    dy = 0;
		                    deltaY = 0;
		                } else if (yp >= (yDimSource - 1)) {
		                    y0 = yDimSource - 1;
		                    dy = 0;
		                    deltaY = 0;
		                } else {
		                    y0 = (int) yp;
		                    dy = yp - y0;
		                    deltaY = xDimSource;
		                }
		
		                dx1 = 1 - dx;
		                dy1 = 1 - dy;
		
		                position = (y0 * xDimSource) + x0;
		
		                if (cf == 1) {
		                    destBuffer[index] = (dy1 * ( (dx1 * srcBuffer2[position]) + (dx * srcBuffer2[position + deltaX])))
		                        + (dy * ( (dx1 * srcBuffer2[position + deltaY]) + (dx * srcBuffer2[position + deltaY + deltaX])));   
		                }
		                else {
		                	 for (c = 0; c < 4; c++) {
		                         destBuffer[4*index+c] = (dy1 * ( (dx1 * srcBuffer2[4*position]+c) + (dx * srcBuffer2[4*(position + deltaX) + c])))
		                                 + (dy * ( (dx1 * srcBuffer2[4*(position + deltaY) + c]) + (dx * srcBuffer2[4*(position + deltaY + deltaX) + c])));
		                     } // for (c = 0; c < 4; c++)	
		                }
		 	       } // if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5))
        		}
        	}
		} // for (y = 0; y < yDimDest; y++)
		
	// Undo y axis inversion
    double destBuffer2[] = new double[cf * destSlice];
	for (y = 0; y < yDimDest; y++) {
    	for (x = 0; x < xDimDest; x++) {
    		if (cf == 1) {
    	        destBuffer2[x + (yDimDest - 1 - y)*xDimDest]	= destBuffer[ x + y*xDimDest];
    		}
    		else {
    		    for (c = 0; c < 4; c++) {
    		    	destBuffer2[4*(x + (yDimDest - 1 - y)*xDimDest) + c]	= destBuffer[4*(x + y*xDimDest) + c];
    		    }
    		}
    	}
    }
    

       
       try {
           destImage.importData(0, destBuffer2, true);
       } catch (IOException e) {
           MipavUtil.displayError("IOException " + e + " on destImage.importData");

           setCompleted(false);

           return;
       }

       setCompleted(true);

       return;
	}
	
	public void runPolygonExteriorToCircle() {
		int i, j;
		int index;
		int cf;
		int xDimSource;
		int yDimSource;
		int sourceSlice;
		int xDimDest;
		int yDimDest;
		int destSlice;
		double srcBuffer[];
		double destBuffer[];
		double imageMin;
		int x;
		int y; 
		int c;
		double zp[][];
		double wp[][];
		double xp;
		double yp;
		int x0;
		int y0;
		int deltaX;
		int deltaY;
		double dx;
		double dy;
		double dx1;
		double dy1;
		int position;
		
		if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Polygon exterior to circle ...");

        if (srcImage.isColorImage()) {
            cf = 4;
        } else {
            cf = 1;
        }
        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource;

        xDimDest = destImage.getExtents()[0];
        yDimDest = destImage.getExtents()[1];
        destSlice = xDimDest * yDimDest;
        srcBuffer = new double[cf * sourceSlice];

        try {
            srcImage.exportData(0, cf * sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }
        // Invert so y axis increases going up so can use ccw ordering
        double srcBuffer2[] = new double[cf *sourceSlice];
        for (y = 0; y < yDimSource; y++) {
        	for (x = 0; x < xDimSource; x++) {
        		if (cf == 1) {
        	        srcBuffer2[x + (yDimSource - 1 - y)*xDimSource]	= srcBuffer[ x + y*xDimSource];
        		}
        		else {
        		    for (c = 0; c < 4; c++) {
        		    	srcBuffer2[4*(x + (yDimSource - 1 - y)*xDimSource) + c]	= srcBuffer[4*(x + y*xDimSource) + c];	
        		    }
        		}
        	}
        }

        destBuffer = new double[cf * destSlice];

        if (!srcImage.isColorImage()) {
            imageMin = srcImage.getMin();

            for (i = 0; i < destSlice; i++) {
                destBuffer[i] = imageMin;
            }
        } // if (!srcImage.isColorImage())
        double w[][] = new double[xSource.length][2];
        for (i = 0; i < xSource.length; i++) {
        	w[i][0] = xSource[i];
        	// Invert so y axis increases going up so can use ccw ordering
        	w[i][1] = yDimSource-1-ySource[i];
        }
        double wx[] = new double[w.length];
		double wy[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			wx[i] = w[i][0];
			wy[i] = w[i][1];
		}
		polygon poly = new polygon(wx, wy, null);
		double alpha[] = poly.angle;
        scmap M = extermap(w, alpha, tolerance, null, null);
		for (i = 0; i < M.prevertex.length; i++) {
			System.out.println("prevertex["+i+"] = " + M.prevertex[i][0] + " " + M.prevertex[i][1]+"i");
		}
		System.out.println("c = " + M.constant[0] + " " + M.constant[1]+"i");
		double theta[] = new double[1];
		exterplot(M, null, theta, 200, 140, null, yDimSource-1, null);
		double xcenterDest = (xDimDest-1.0)/2.0;
		double ycenterDest = (yDimDest-1.0)/2.0;
		double maxDistance = Math.min(xcenterDest,ycenterDest);
		boolean idx[] = new boolean[destSlice];
		j = 0;
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		double distX = (x-xcenterDest)/maxDistance;
        		double distY = (y-ycenterDest)/maxDistance;
        		if ((distX*distX + distY*distY) < 1.0) {
        			idx[index] = true;
        			j++;
        		}
        	}
        } // for (y = 0; y < yDimDest; y++)
        zp = new double[j][2];
        j = 0;
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		double distX = (x-xcenterDest)/maxDistance;
        		double distY = (y-ycenterDest)/maxDistance;
        		if ((distX*distX + distY*distY) < 1.0) {
        			zp[j][0] = distX;
        			zp[j][1] = distY;
        			j++;
        		}
        	}
        } // for (y = 0; y < yDimDest; y++)
        polygon p = M.poly;
		for (i = 0; i < p.vertex.length; i++) {
			w[i][0] = p.vertex[p.vertex.length - 1 - i][0];
			w[i][1] = p.vertex[p.vertex.length - 1 - i][1];
		}
		double beta[] = new double[p.angle.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = 1.0 - p.angle[p.angle.length - 1 - i];
		}
        wp = demap(zp, w, beta, M.prevertex, M.constant, M.qdata);
        
        j = 0;
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		if (idx[index]) {
        			xp = wp[j][0];
        			yp = wp[j][1];
        			j++;
					if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5)) {
		 	    	   if (xp <= 0) {
		                    x0 = 0;
		                    dx = 0;
		                    deltaX = 0;
		                } else if (xp >= (xDimSource - 1)) {
		                    x0 = xDimSource - 1;
		                    dx = 0;
		                    deltaX = 0;
		                } else {
		                    x0 = (int) xp;
		                    dx = xp - x0;
		                    deltaX = 1;
		                }
		
		                if (yp <= 0) {
		                    y0 = 0;
		                    dy = 0;
		                    deltaY = 0;
		                } else if (yp >= (yDimSource - 1)) {
		                    y0 = yDimSource - 1;
		                    dy = 0;
		                    deltaY = 0;
		                } else {
		                    y0 = (int) yp;
		                    dy = yp - y0;
		                    deltaY = xDimSource;
		                }
		
		                dx1 = 1 - dx;
		                dy1 = 1 - dy;
		
		                position = (y0 * xDimSource) + x0;
		
		                if (cf == 1) {
		                    destBuffer[index] = (dy1 * ( (dx1 * srcBuffer2[position]) + (dx * srcBuffer2[position + deltaX])))
		                        + (dy * ( (dx1 * srcBuffer2[position + deltaY]) + (dx * srcBuffer2[position + deltaY + deltaX])));   
		                }
		                else {
		                	 for (c = 0; c < 4; c++) {
		                         destBuffer[4*index+c] = (dy1 * ( (dx1 * srcBuffer2[4*position]+c) + (dx * srcBuffer2[4*(position + deltaX) + c])))
		                                 + (dy * ( (dx1 * srcBuffer2[4*(position + deltaY) + c]) + (dx * srcBuffer2[4*(position + deltaY + deltaX) + c])));
		                     } // for (c = 0; c < 4; c++)	
		                }
		 	       } // if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5))
        		}
        	}
		} // for (y = 0; y < yDimDest; y++)
		
	// Undo y axis inversion
    double destBuffer2[] = new double[cf * destSlice];
	for (y = 0; y < yDimDest; y++) {
    	for (x = 0; x < xDimDest; x++) {
    		if (cf == 1) {
    	        destBuffer2[x + (yDimDest - 1 - y)*xDimDest]	= destBuffer[ x + y*xDimDest];
    		}
    		else {
    		    for (c = 0; c < 4; c++) {
    		    	destBuffer2[4*(x + (yDimDest - 1 - y)*xDimDest) + c]	= destBuffer[4*(x + y*xDimDest) + c];
    		    }
    		}
    	}
    }
    

       
       try {
           destImage.importData(0, destBuffer2, true);
       } catch (IOException e) {
           MipavUtil.displayError("IOException " + e + " on destImage.importData");

           setCompleted(false);

           return;
       }

       setCompleted(true);

       return;
	}
	
	public void runPolygonToRectangle() {
		int i, j;
		int index;
		int cf;
		int xDimSource;
		int yDimSource;
		int sourceSlice;
		int xDimDest;
		int yDimDest;
		int destSlice;
		double srcBuffer[];
		double destBuffer[];
		double imageMin;
		int y;
		int x;
		double z[][];
	    int MCorners[];
	    double zr[][];
	    double wpinpoly[][];
		double zp[][];
		double xp;
		double yp;
		int x0;
		int y0;
		int deltaX;
		int deltaY;
		double dx;
		double dy;
		double dx1;
		double dy1;
		int position;
		int c;
		
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Polygon to rectangle ...");

        if (srcImage.isColorImage()) {
            cf = 4;
        } else {
            cf = 1;
        }
        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource;

        xDimDest = destImage.getExtents()[0];
        yDimDest = destImage.getExtents()[1];
        destSlice = xDimDest * yDimDest;
        srcBuffer = new double[cf * sourceSlice];

        try {
            srcImage.exportData(0, cf * sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }
        // Invert so y axis increases going up so can use ccw ordering
        double srcBuffer2[] = new double[cf *sourceSlice];
        for (y = 0; y < yDimSource; y++) {
        	for (x = 0; x < xDimSource; x++) {
        		if (cf == 1) {
        	        srcBuffer2[x + (yDimSource - 1 - y)*xDimSource]	= srcBuffer[ x + y*xDimSource];
        		}
        		else {
        		    for (c = 0; c < 4; c++) {
        		    	srcBuffer2[4*(x + (yDimSource - 1 - y)*xDimSource) + c]	= srcBuffer[4*(x + y*xDimSource) + c];	
        		    }
        		}
        	}
        }

        destBuffer = new double[cf * destSlice];

        if (!srcImage.isColorImage()) {
            imageMin = srcImage.getMin();

            for (i = 0; i < destSlice; i++) {
                destBuffer[i] = imageMin;
            }
        } // if (!srcImage.isColorImage())
        double w[][] = new double[xSource.length][2];
        for (i = 0; i < xSource.length; i++) {
        	w[i][0] = xSource[i];
        	// Invert so y axis increases going up so can use ccw ordering
        	w[i][1] = yDimSource-1-ySource[i];
        }
        scmap M = rectmap(w, corners, tolerance, null, null, null);
        rectplot(M, 10, 10, yDimSource-1);
        z = M.prevertex;
        MCorners = corners(M);
        double xMin = Double.MAX_VALUE;
        double xMax = -Double.MAX_VALUE;
        double yMin = Double.MAX_VALUE;
        double yMax = -Double.MAX_VALUE;
        zr = new double[MCorners.length][2];
        double xr[] = new double[MCorners.length];
        double yr[] = new double[MCorners.length];
        for (i = 0; i < MCorners.length; i++) {
            zr[i][0] = z[MCorners[i]][0];
            xr[i] = zr[i][0];
            zr[i][1] = z[MCorners[i]][1];
            yr[i] = zr[i][1];
            System.out.println("zr["+i+"] = " + zr[i][0] + " " + zr[i][1] + "i");
            if (zr[i][0] < xMin) {
            	xMin = zr[i][0];
            }
            if (zr[i][0] > xMax) {
            	xMax = zr[i][0];
            }
            if (zr[i][1] < yMin) {
            	yMin = zr[i][1];
            }
            if (zr[i][1] > yMax) {
            	yMax = zr[i][1];
            }
        }
        polygon pr = new polygon(xr, yr, null);
        double betar[] = new double[pr.angle.length];
        for (i = 0; i < pr.angle.length; i++) {
        	betar[i] = pr.angle[i] - 1.0;
        }
        double xSlope = (xMax - xMin)/(xDimDest - 1.0);
        double ySlope = (yMax - yMin)/(yDimDest - 1.0);
        zp = new double[destSlice][2];
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		zp[index][0] = x*xSlope + xMin;
        		zp[index][1] = y*ySlope + yMin;
        	}
        } // for (y = 0; y < yDimDest; y++)
        boolean indexout[] = new boolean[destSlice];
		boolean onvtx[][] = new boolean[w.length][destSlice];
		polygon p = M.poly;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		isinpoly(indexout, onvtx, zp, zr, betar, eps);
		int numinpoly = 0;
		for (i = 0; i < destSlice; i++) {
		    if (indexout[i]) {
		    	numinpoly++;
		    }
		} // for (i = 0; i < destSlice; i++)
		double zpinpoly[][] = new double[numinpoly][2];
		for (i = 0, j = 0; i < destSlice; i++) {
			if (indexout[i]) {
				zpinpoly[j][0] = zp[i][0];
				zpinpoly[j][1] = zp[i][1];
				j++;
			}
		} // for (i = 0, j = 0; i < destSlice; i++)
		wpinpoly = new double[numinpoly][2];
		rmap(wpinpoly, zpinpoly, w, beta, z, M.constant, M.stripL, M.qdata);
		j = 0;
		for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		if (indexout[index]) {
        			xp = wpinpoly[j][0];
        			yp = wpinpoly[j][1];
        			j++;
        			if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5)) {
         	    	   if (xp <= 0) {
                            x0 = 0;
                            dx = 0;
                            deltaX = 0;
                        } else if (xp >= (xDimSource - 1)) {
                            x0 = xDimSource - 1;
                            dx = 0;
                            deltaX = 0;
                        } else {
                            x0 = (int) xp;
                            dx = xp - x0;
                            deltaX = 1;
                        }

                        if (yp <= 0) {
                            y0 = 0;
                            dy = 0;
                            deltaY = 0;
                        } else if (yp >= (yDimSource - 1)) {
                            y0 = yDimSource - 1;
                            dy = 0;
                            deltaY = 0;
                        } else {
                            y0 = (int) yp;
                            dy = yp - y0;
                            deltaY = xDimSource;
                        }

                        dx1 = 1 - dx;
                        dy1 = 1 - dy;

                        position = (y0 * xDimSource) + x0;

                        if (cf == 1) {
                            destBuffer[index] = (dy1 * ( (dx1 * srcBuffer2[position]) + (dx * srcBuffer2[position + deltaX])))
                                + (dy * ( (dx1 * srcBuffer2[position + deltaY]) + (dx * srcBuffer2[position + deltaY + deltaX])));   
                        }
                        else {
                        	 for (c = 0; c < 4; c++) {
                                 destBuffer[4*index+c] = (dy1 * ( (dx1 * srcBuffer2[4*position]+c) + (dx * srcBuffer2[4*(position + deltaX) + c])))
                                         + (dy * ( (dx1 * srcBuffer2[4*(position + deltaY) + c]) + (dx * srcBuffer2[4*(position + deltaY + deltaX) + c])));
                             } // for (c = 0; c < 4; c++)	
                        }
         	       } // if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5))
        		}
        	}
		}
		
	// Undo y axis inversion
    double destBuffer2[] = new double[cf * destSlice];
	for (y = 0; y < yDimDest; y++) {
    	for (x = 0; x < xDimDest; x++) {
    		if (cf == 1) {
    	        destBuffer2[x + (yDimDest - 1 - y)*xDimDest]	= destBuffer[ x + y*xDimDest];
    		}
    		else {
    		    for (c = 0; c < 4; c++) {
    		    	destBuffer2[4*(x + (yDimDest - 1 - y)*xDimDest) + c]	= destBuffer[4*(x + y*xDimDest) + c];
    		    }
    		}
    	}
    }
    

       
       try {
           destImage.importData(0, destBuffer2, true);
       } catch (IOException e) {
           MipavUtil.displayError("IOException " + e + " on destImage.importData");

           setCompleted(false);

           return;
       }

       setCompleted(true);

       return;
	}
	
	public void runCrossRatioPolygonToRectangle() {
		int i, j;
		int index;
		int cf;
		int xDimSource;
		int yDimSource;
		int sourceSlice;
		int xDimDest;
		int yDimDest;
		int destSlice;
		double srcBuffer[];
		double destBuffer[];
		double imageMin;
		int y;
		int x;
	    double zr[][];
	    double wpinpoly[][][];
		double zp[][];
		double xp;
		double yp;
		int x0;
		int y0;
		int deltaX;
		int deltaY;
		double dx;
		double dy;
		double dx1;
		double dy1;
		int position;
		int c;
		
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Cross-ratio polygon to rectangle ...");

        if (srcImage.isColorImage()) {
            cf = 4;
        } else {
            cf = 1;
        }
        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource;

        xDimDest = destImage.getExtents()[0];
        yDimDest = destImage.getExtents()[1];
        destSlice = xDimDest * yDimDest;
        srcBuffer = new double[cf * sourceSlice];

        try {
            srcImage.exportData(0, cf * sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }
        // Invert so y axis increases going up so can use ccw ordering
        double srcBuffer2[] = new double[cf *sourceSlice];
        for (y = 0; y < yDimSource; y++) {
        	for (x = 0; x < xDimSource; x++) {
        		if (cf == 1) {
        	        srcBuffer2[x + (yDimSource - 1 - y)*xDimSource]	= srcBuffer[ x + y*xDimSource];
        		}
        		else {
        		    for (c = 0; c < 4; c++) {
        		    	srcBuffer2[4*(x + (yDimSource - 1 - y)*xDimSource) + c]	= srcBuffer[4*(x + y*xDimSource) + c];	
        		    }
        		}
        	}
        }

        destBuffer = new double[cf * destSlice];

        if (!srcImage.isColorImage()) {
            imageMin = srcImage.getMin();

            for (i = 0; i < destSlice; i++) {
                destBuffer[i] = imageMin;
            }
        } // if (!srcImage.isColorImage())
        double w[][] = new double[xSource.length][2];
        for (i = 0; i < xSource.length; i++) {
        	w[i][0] = xSource[i];
        	// Invert so y axis increases going up so can use ccw ordering
        	w[i][1] = yDimSource-1-ySource[i];
        }
        scmap M = crrectmap(w, corners, tolerance);
        double nre[] = new double[]{10.0};
        double nim[] = new double[]{10.0};
        crrectplot(M, nre, nim, yDimSource-1);
        double xMin = Double.MAX_VALUE;
        double xMax = -Double.MAX_VALUE;
        double yMin = Double.MAX_VALUE;
        double yMax = -Double.MAX_VALUE;
        zr = new double[corners.length][2];
        double xr[] = new double[corners.length];
        double yr[] = new double[corners.length];
        double wr[][] = M.rectpolygon.vertex;
        for (i = 0; i < corners.length; i++) {
            zr[i][0] = wr[corners[i]][0];
            xr[i] = zr[i][0];
            zr[i][1] = wr[corners[i]][1];
            yr[i] = zr[i][1];
            System.out.println("zr["+i+"] = " + zr[i][0] + " " + zr[i][1] + "i");
            if (zr[i][0] < xMin) {
            	xMin = zr[i][0];
            }
            if (zr[i][0] > xMax) {
            	xMax = zr[i][0];
            }
            if (zr[i][1] < yMin) {
            	yMin = zr[i][1];
            }
            if (zr[i][1] > yMax) {
            	yMax = zr[i][1];
            }
        }
        polygon pr = M.rectpolygon;
        double betar[] = new double[pr.angle.length];
        for (i = 0; i < pr.angle.length; i++) {
        	betar[i] = pr.angle[i] - 1.0;
        }
        double xSlope = (xMax - xMin)/(xDimDest - 1.0);
        double ySlope = (yMax - yMin)/(yDimDest - 1.0);
        zp = new double[destSlice][2];
        for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		zp[index][0] = x*xSlope + xMin;
        		zp[index][1] = y*ySlope + yMin;
        	}
        } // for (y = 0; y < yDimDest; y++)
        boolean indexout[] = new boolean[destSlice];
		boolean onvtx[][] = new boolean[w.length][destSlice];
		polygon p = M.poly;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		isinpoly(indexout, onvtx, zp, zr, betar, eps);
		int numinpoly = 0;
		for (i = 0; i < destSlice; i++) {
		    if (indexout[i]) {
		    	numinpoly++;
		    }
		} // for (i = 0; i < destSlice; i++)
		double zpinpoly[][] = new double[numinpoly][2];
		for (i = 0, j = 0; i < destSlice; i++) {
			if (indexout[i]) {
				zpinpoly[j][0] = zp[i][0];
				zpinpoly[j][1] = zp[i][1];
				j++;
			}
		} // for (i = 0, j = 0; i < destSlice; i++)
		wpinpoly = new double[numinpoly][1][2];
		//rmap(wpinpoly, zpinpoly, w, beta, z, M.constant, M.stripL, M.qdata);
		// Number of quadrature points per integration.
		// Approximately equals -log10(error).  Increase if plot
		// has false little zigzags in curves. 
		int nqpts = 5; 
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		double qdatr[][] = new double[nqpts][2*betar.length+2];
		scqdata(qdatr, betar, nqpts);
		double tolmap = Math.pow(10.0, -qdat.length);
        int zpindex[] = new int[numinpoly];
		onvtx = new boolean[wr.length][numinpoly];
		isinpoly2(zpindex, onvtx, zpinpoly, wr, betar, tolmap);
		int maxindex = -1;
		for (i = 0; i < numinpoly; i++) {
			if (zpindex[i] > maxindex) {
				maxindex = zpindex[i];
			}
		} // for (i = 0; i < numnew; i++)
		if (maxindex > 1) {
			MipavUtil.displayError("Too many values found at some points");
			System.exit(-1);
		}
		int wpqn[][] = new int[numinpoly][Math.max(1, maxindex)];
		double cr[] = M.crossratio;
		double aff[][][] = M.affine;
		double affr[][][] = M.rectaffine;
		qlgraph Q = M.qgraph;
        crrmap(wpinpoly, wpqn, zpinpoly, w, beta, wr, betar, cr, aff, affr,
        		Q, qdat, qdatr);
		j = 0;
		for (y = 0; y < yDimDest; y++) {
        	for (x = 0; x < xDimDest; x++) {
        		index = x + xDimDest*y;
        		if (indexout[index]) {
        			xp = wpinpoly[j][0][0];
        			yp = wpinpoly[j][0][1];
        			j++;
        			if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5)) {
         	    	   if (xp <= 0) {
                            x0 = 0;
                            dx = 0;
                            deltaX = 0;
                        } else if (xp >= (xDimSource - 1)) {
                            x0 = xDimSource - 1;
                            dx = 0;
                            deltaX = 0;
                        } else {
                            x0 = (int) xp;
                            dx = xp - x0;
                            deltaX = 1;
                        }

                        if (yp <= 0) {
                            y0 = 0;
                            dy = 0;
                            deltaY = 0;
                        } else if (yp >= (yDimSource - 1)) {
                            y0 = yDimSource - 1;
                            dy = 0;
                            deltaY = 0;
                        } else {
                            y0 = (int) yp;
                            dy = yp - y0;
                            deltaY = xDimSource;
                        }

                        dx1 = 1 - dx;
                        dy1 = 1 - dy;

                        position = (y0 * xDimSource) + x0;

                        if (cf == 1) {
                            destBuffer[index] = (dy1 * ( (dx1 * srcBuffer2[position]) + (dx * srcBuffer2[position + deltaX])))
                                + (dy * ( (dx1 * srcBuffer2[position + deltaY]) + (dx * srcBuffer2[position + deltaY + deltaX])));   
                        }
                        else {
                        	 for (c = 0; c < 4; c++) {
                                 destBuffer[4*index+c] = (dy1 * ( (dx1 * srcBuffer2[4*position]+c) + (dx * srcBuffer2[4*(position + deltaX) + c])))
                                         + (dy * ( (dx1 * srcBuffer2[4*(position + deltaY) + c]) + (dx * srcBuffer2[4*(position + deltaY + deltaX) + c])));
                             } // for (c = 0; c < 4; c++)	
                        }
         	       } // if ((xp > -0.5) && (xp < xDimSource - 0.5) && (yp > -0.5) && (yp < yDimSource - 0.5))
        		}
        	}
		}
		
	// Undo y axis inversion
    double destBuffer2[] = new double[cf * destSlice];
	for (y = 0; y < yDimDest; y++) {
    	for (x = 0; x < xDimDest; x++) {
    		if (cf == 1) {
    	        destBuffer2[x + (yDimDest - 1 - y)*xDimDest]	= destBuffer[ x + y*xDimDest];
    		}
    		else {
    		    for (c = 0; c < 4; c++) {
    		    	destBuffer2[4*(x + (yDimDest - 1 - y)*xDimDest) + c]	= destBuffer[4*(x + y*xDimDest) + c];
    		    }
    		}
    	}
    }
    

       
       try {
           destImage.importData(0, destBuffer2, true);
       } catch (IOException e) {
           MipavUtil.displayError("IOException " + e + " on destImage.importData");

           setCompleted(false);

           return;
       }

       setCompleted(true);

       return;
	}
	
	public void testRectmap1() {
		// Example from users guide
		// Test with
		// SchwarzChristoffelMapping sc = new SchwarzChristoffelMapping();
    	// sc.runAlgorithm();
		double w[][] = new double[6][2];
        w[0][0] = -5;
        w[0][1] = -1;
        w[1][0] = -5;
        w[1][1] = -3;
        w[2][0] = 5;
        w[2][1] = -3;
        w[3][0] = 5;
        w[3][1] = 1;
        w[4][0] = 5;
        w[4][1] = 3;
        w[5][0] = -5;
        w[5][1] = 3;
        int corner[] = new int[4];
        corner[0] = 0;
        corner[1] = 1;
        corner[2] = 3;
        corner[3] = 4;
        scmap M = rectmap(w, corner, tolerance, null, null, null);
        rectplot(M, 10, 10, Integer.MIN_VALUE);
	}
	
	public void testRectmap2() {
		// Example from Algorithm 843: Scwarz-Christoffel Toolbox for MATLAB
		//ans =
	    // 1.5708
		// 1.5708 +11.1798i
	    // -1.5708 +11.1798i
		// -1.5708
		int i;
		double w[][] = new double[6][2];
        w[0][0] = -1;
        w[0][1] = 1;
        w[1][0] = -1;
        w[1][1] = -1;
        w[2][0] = 2;
        w[2][1] = -1;
        w[3][0] = 2;
        w[3][1] = 0;
        w[4][0] = 0;
        w[4][1] = 0;
        w[5][0] = 0;
        w[5][1] = 1;
        int corner[] = new int[4];
        corner[0] = 0;
        corner[1] = 2;
        corner[2] = 3;
        corner[3] = 5;
        scmap M = rectmap(w, corner, tolerance, null, null, null);
        rectplot(M, 8, 4, Integer.MIN_VALUE);
        double zr[][] = rectangle(M);
        for (i = 0; i < zr.length; i++) {
        	if (zr[i][1] >= 0.0) {
                System.out.println("Corner " + i + " in the fundmemtal domain = " + zr[i][0] + " + " + zr[i][1]+"i");	
        	}
        	else {
        		System.out.println("Corner " + i + " in the fundmemtal domain = " + zr[i][0] + " " + zr[i][1]+"i");		
        	}
        }
	}
	
	public void testRectmap3() {
		int i;
	    double w[][] = new double[6][2];
	    w[0][0] = 4.0;
	    w[0][1] = 0.0;
	    w[1][0] = 0.0;
	    w[1][1] = 2.0;
	    w[2][0] = -2.0;
	    w[2][1] = 4.0;
	    w[3][0] = -3.0;
	    w[3][1] = 0.0;
	    w[4][0] = -3.0;
	    w[4][1] = -1.0;
	    w[5][0] = 2.0;
	    w[5][1] = -2.0;
	    double x[] = new double[6];
	    double y[] = new double[6];
	    for (i = 0; i < 6; i++) {
	    	x[i] = w[i][0];
	    	y[i] = w[i][1];
	    }
	    polygon poly = new polygon(x, y, null);
	    double beta[] = new double[poly.angle.length];
	    for (i = 0; i < poly.angle.length; i++) {
	    	beta[i] = poly.angle[i] - 1.0;
	    }
	    int corner[] = new int[4];
	    corner[0] = 0;
	    corner[1] = 1;
	    corner[2] = 2;
	    corner[3] = 3;
	    scmap M = rectmap(w, corner, tolerance, null, null, null);
	    double zp[][] = new double[4][2];
	    zp[0][0] = 1.5;
	    zp[0][1] = 0.0;
	    zp[1][0] = 1.4;
	    zp[1][1] = 3.0;
	    zp[2][0] = -0.6;
	    zp[2][1] = 1.0;
	    zp[3][0] = 1.0;
	    zp[3][1] = 0.0;
	    double wp[][] = new double[4][2];
	    
	    rmap(wp, zp, w, beta, M.prevertex, M.constant, M.stripL, M.qdata);
	    for (i = 0; i < 4; i++) {
	    	if (i == 0) {
	    		System.out.println("Expected forward result: 3.641550444027862 - 0.358449555972138i");
	    	}
	    	else if (i == 1) {
	    		System.out.println("Expected forward result: -0.005336970451055 + 1.9881355984422i");
	    	}
	    	else if (i == 2) {
	    		System.out.println("Expected forward result: -1.643459212104280 + 0.42859757726735i");
	    	}
	    	else if (i == 3) {
	    		System.out.println("Expected forward result: 1.646072527976422 - 1.929214505595285i");
	    	}
	    	System.out.println("Actual forward result: " + wp[i][0] + " " + wp[i][1] + "i");
	    }
	    double tol[][] = null;
	    double z0[][] = null;
	    int maxiter = 500;
	    double wpinverse[][] = rectevalinv(M, wp, tol, z0, maxiter);
	    for (i = 0; i < 4; i++) {
	    	if (i == 0) {
	    	    System.out.println("Expected inverse result: 1.5");
	    	}
	    	else if (i == 1) {
	    		System.out.println("Expected inverse result: 1.4 + 3i");
	    	}
	    	else if (i == 2) {
	    		System.out.println("Expected inverse result: -0.6 + 1.0i");
	    	}
	    	else if (i == 3) {
	    		System.out.println("Expected inverse result: 1.0");
	    	}
	    	System.out.println("Actual inverse result: " + wpinverse[i][0] + " " + wpinverse[i][1] + "i");
	    }
	}
	
	public void testDiskmap1() {
		int i;
		int j;
		double w[][] = new double[4][2];
		w[0][0] = 1;
		w[0][1] = 1;
		w[1][0] = -1;
		w[1][1] = 1;
		w[2][0] = -1;
		w[2][1] = -1;
		w[3][0] = 1;
		w[3][1] = -1;
		scmap M = diskmap(w, null, tolerance, null, null);
		double wc[] = new double[2];
		M = center(M, wc, null);
		// Points for unit circle
		int n = 180;
		double wcir[][] = new double[n][2];
		double xcir[] = new double[n];
		double ycir[] = new double[n];
		for (i = 0; i < n; i++) {
			wcir[i][0] = Math.cos(i*2.0*Math.PI/(n+1.0));
			wcir[i][1] = Math.sin(i*2.0*Math.PI/(n+1.0));
			xcir[i] = wcir[i][0];
			ycir[i] = wcir[i][1];
		}
		polygon poly = new polygon(xcir, ycir, null);
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		float xPointArray[] = new float[n+1];
		float yPointArray[] = new float[n+1];
		int nqpts = 5;
		double axlim[] = new double[4];
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, wcir, beta, false, axlim, Integer.MIN_VALUE, true, null);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
		double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		double X[][] = new double[201][9];
		double Y[][] = new double[201][9];
		double wp[][] = new double[201*9][2];
		double zp[][] = new double[201*9][2];
		for (i = 0; i < 201; i++) {
			for (j = 0; j < 9; j++) {
			    X[i][j] = -0.8 + 0.2*j;
			    Y[i][j] = -1.0 + 0.01*i;
			    wp[201*j + i][0] = X[i][j];
			    wp[201*j + i][1] = Y[i][j];
			}
		}
		int flag[] = null;
		int maxiter = 200;
		flag = diskevalinv(zp, M, wp, M.qdata, null, maxiter);
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		for (i = 0; i < 9; i++) {
			for (j = 0; j < 200; j++) {
				double posx1 = zp[201*i + j][0];
	    		double posy1 = zp[201*i + j][1];
	    		double posx2 = zp[201*i + j + 1][0];
	    		double posy2 = zp[201*i + j + 1][1];
	    		x1Vector.add(posx1);
	    		y1Vector.add(posy1);
	    		x2Vector.add(posx2);
	    		y2Vector.add(posy2);
	    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
			    graph.drawLine(g, x1, y1, x2, y2);	
			}
		}
		for (i = 0; i < 201; i++) {
			for (j = 0; j < 9; j++) {
			    wp[201*j + i][0] = Y[i][j];
			    wp[201*j + i][1] = X[i][j];
			}
		}
		flag = diskevalinv(zp, M, wp, M.qdata, null, maxiter);
		for (i = 0; i < 9; i++) {
			for (j = 0; j < 200; j++) {
				double posx1 = zp[201*i + j][0];
	    		double posy1 = zp[201*i + j][1];
	    		double posx2 = zp[201*i + j + 1][0];
	    		double posy2 = zp[201*i + j + 1][1];
	    		x1Vector.add(posx1);
	    		y1Vector.add(posy1);
	    		x2Vector.add(posx2);
	    		y2Vector.add(posy2);
	    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
			    graph.drawLine(g, x1, y1, x2, y2);	
			}
		}
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
	}
	
	public void testDiskmap2() {
		int i;
		double w[][] = new double[4][2];
		w[0][0] = -4;
		w[0][1] = -1;
		w[1][0] = 4;
		w[1][1] = -1;
		w[2][0] = 4;
		w[2][1] = 1;
		w[3][0] = -4;
		w[3][1] = 1;
		scmap M = diskmap(w, null, tolerance, null, null);
		double wc[] = new double[2];
		M = center(M, wc, null);
		double R[] = new double[4];
		for (i = 0; i < 4; i++) {
			R[i] = 0.2*(i+1);
		}
		double prevertex[][] = M.prevertex;
		double theta[] = new double[4];
		for (i = 0; i < 4; i++) {
			theta[i] = Math.atan2(prevertex[i][1], prevertex[i][0]);
		}
		diskplot(M, R, theta, 200, 140, null, Integer.MIN_VALUE);
	}
	
	public void testDiskmap3() {
		int i;
		double w[][] = new double[6][2];
		w[0][0] = 0.0;
		w[0][1] = 1.0;
		w[1][0] = -1.0;
		w[1][1] = 1.0;
		w[2][0] = -1.0;
		w[2][1] = -1.0;
		w[3][0] = 1.0;
		w[3][1] = -1.0;
		w[4][0] = 1.0;
		w[4][1] = 0.0;
		w[5][0] = 0.0;
		w[5][1] = 0.0;
		scmap M = diskmap(w, null, tolerance, null, null);
		for (i = 0; i < 6; i++) {
			System.out.println("prevertex["+i+"] = " + M.prevertex[i][0] + " " + M.prevertex[i][1]+"i");
		}
		System.out.println("center = " + M.center[0] + " " + M.center[1]+"i");
		System.out.println("c = " + M.constant[0] + " " + M.constant[1]+"i");
		diskplot(M, null, null, 200, 140, null, Integer.MIN_VALUE);
		double wc[] = new double[2];
		wc[0] = -0.5;
		wc[1] = -0.5;
		M = center(M, wc, null);
		for (i = 0; i < 6; i++) {
			System.out.println("prevertex["+i+"] = " + M.prevertex[i][0] + " " + M.prevertex[i][1]+"i");
		}
		System.out.println("center = " + M.center[0] + " " + M.center[1]+"i");
		System.out.println("c = " + M.constant[0] + " " + M.constant[1]+"i");
		diskplot(M, null, null, 200, 140, null, Integer.MIN_VALUE);
    }
	
	public void testDiskmap4() {
		int i, j, k;
		// Without z0 supplied to center MIPAV and MATLAB end when center calls dinvmap which calls scimapz0 which returns the
	    // error can't seem to choose starting points.  Add in z0 = 0.4 - 0.9i.
		// 2 accuracies from before setting center to zero and after setting center to zero in MATLAB and MIPAV agree.
		// MATLAB gives:
		// accuracy1 = 3.6940e-07
        // accuracy2 = 5.6571e-07
		// MIPAV gives:
		// Accuracy = 3.69396579067019E-7
	    // Accuracy = 5.657124293818956E-7
        // The center of MIPAV's diskplot looks like MATLAB's h = plot(f, 0.7:0.05:0.95, 0);.
		// In MATLAB the first length(h) = 6.
		// In MATLAB the 6 complex w lengths are 236, 265, 273, 259, 285, and 274;
		// By using the real(1/w) and the imag(1/w) in a plot a figure looking
		// like Figure 4.16 in Schwarz-Christoffel mapping results.
		// In MATLAB h = findobh(gca, 'colof', [001]) has a length(h) = 0 so
		// the bottom for loop never executes.  If the bottom for loop were to execute,
		// when (abs(w(1)) > abs(w(2)), w is to set w[2 1] and diff(w) is of length 1.
		// If abs(w1) <= abs(w(2)), then the length of w is unchanged and the number
		// of entries in diff(w) is equal to the number of entries in w - 1.
		// This would make linspace(0,2*diff(w),100) an illegal line.
		
		// The MATLAB code used is:
		/* w = [1+i, 1 + 2i, Inf, -.705 + .971i, Inf, -1-i, Inf, .705 - .971i, Inf];
		alpha = [2,1, -.3, 2, -.7, 2. -.3, 2, -.7];
		poly = polygon(w, alpha);
		z = [];
		c = [];
		qdata = [];
		[w,beta] = scfix('d', w, alpha-1);
		  poly = polygon(w,beta+1);             % in case polygon was altered
		 
		  [z,c,qdata] = dparam(w,beta);
		map.prevertex = z;
		map.constant = c;
		map.qdata = qdata;
		 

		% Find conformal center
		map.center = dmap(0,w,beta,map.prevertex,map.constant,map.qdata);

		 
		% Fill in apparent accuracy
		idx = find(~isinf(w));
		wf = w(idx);                % finite vertices
		 
		% Two columns hold endpoint indices for integrations
		idx = [idx(1:end) idx([2:end 1])];
		 
		% Always use center as the integration midpoint
		mid = zeros(length(idx),1);
		 
		% Do the integrations
		I = dquad(z(idx(:,1)),mid,idx(:,1),z,beta,qdata) - ...
		    dquad(z(idx(:,2)),mid,idx(:,2),z,beta,qdata);
		 
		map.accuracy = max(abs( c*I - diff(wf([1:end 1])) ));
		accuracy1 = map.accuracy

		wc = 0;
		% Find inverse image of wc under current map
		  z0 = [0.4-0.9i];
		  zc = dinvmap(wc,w,beta,z,map.constant,qdata, z0);
		 
		  % Use Moebius transform to reset prevertices
		  y = ((1-zc')/(1-zc))*(z-zc)./(1-zc'*z);
		  y(length(y)) = 1;         % force it to be exact
		  y = y./abs(y);
		  
		  % Recalculate constant
		  mid = mean(y(1:2));
		  I = dquad(y(1),mid,1,y,beta,qdata) - dquad(y(2),mid,2,y,beta,qdata);
		  c = diff(w(1:2))/I;
		  
		  % Assign new values
		  map.prevertex = y;
		  map.constant = c;
		  map.center = wc;
		  map.accuracy = []; 
		% Fill in apparent accuracy
		z = map.prevertex;
		idx = find(~isinf(w));
		wf = w(idx);                % finite vertices
		 
		% Two columns hold endpoint indices for integrations
		idx = [idx(1:end) idx([2:end 1])];
		 
		% Always use center as the integration midpoint
		mid = zeros(length(idx),1);
		 
		% Do the integrations
		I = dquad(z(idx(:,1)),mid,idx(:,1),z,beta,qdata) - ...
		    dquad(z(idx(:,2)),mid,idx(:,2),z,beta,qdata);
		 
		map.accuracy = max(abs( c*I - diff(wf([1:end 1])) ));
		accuracy2 = map.accuracy
		axis(5.5*[-1 1 -1 1]), hold on
		[h,r,theta] = dplot(w,beta,z,c, 0.7:0.05:0.95, 0);
		for n=1:length(h)
		w = (get(h(n),'xd') + i*get(h(n),'yd'));
		set(h(n),'xd',real(1./w),'yd',imag(1./w))
		end
		h = findobj(gca,'color',[0 0 1]);
		for n=1:length(h)
		w = get(h(n),'xd') + i*get(h(n),'yd');
		if abs(w(1)) > abs(w(2)), w=w([2 1]); end
		u1 = w(1) + linspace(0,2*diff(w),100);
		u2 = w(1) + linspace(2*diff(w),100*diff(w),100);
		plot(1./[u1 u2]);
		end
		delete(h)
		axis auto */

		double x[] = new double[]{1.0, 1.0, Double.POSITIVE_INFINITY, -.705, Double.POSITIVE_INFINITY, -1.0, Double.POSITIVE_INFINITY,
				.705, Double.POSITIVE_INFINITY};
		double y[] = new double[]{1.0, 2.0,0.0,.971,0,-1.0,0.0,-.971,0.0};
		double alpha[] = new double[]{2,1,-.3,2,-.7,2,-.3,2,-.7};
		//polygon p = new polygon(x, y, alpha);
		double w[][] = new double[9][2];
		for (i = 0; i < 9; i++) {
			w[i][0] = x[i];
			w[i][1] = y[i];
		}
		double wc[] = new double[2];
		double z0[][] = new double[1][2];
		z0[0][0] = 0.4;
		z0[0][1] = -0.9;
		scmap f = center(diskmap(w, alpha, tolerance, null, null), wc, z0);                                                                                                            
		double R[] = new double[6];
		for (i = 0; i < 6; i++) {
			R[i] = 0.70  + 0.05*i;
		}
		double theta[] = new double[1];
		theta[0] = 0.0;
		diskplot(f, R, theta, 20, 14, null, Integer.MIN_VALUE);
		double x2[] = new double[2];
		double y2[] = new double[2];
		double denom[] = new double[2];
		double wr[] = new double[2];
		double wi[] = new double[2];
		Vector<Double>uxr[] = new Vector[linhx.length];
		Vector<Double>uyr[] = new Vector[linhx.length];
		double axlim[] = new double[4];
		axlim[0] = Double.MAX_VALUE;
		axlim[1] = -Double.MAX_VALUE;
		axlim[2] = Double.MAX_VALUE;
		axlim[3] = -Double.MAX_VALUE;
		
		for (i = 0; i < linhx.length; i++) {
			uxr[i] = new Vector<Double>();
	    	uyr[i] = new Vector<Double>();
			for (j = 0; j < linhx[i][0].size(); j++) {
			    x2[0] = linhx[i][0].get(j);
			    y2[0] = linhy[i][0].get(j);
			    denom[0] = x2[0]*x2[0] + y2[0]*y2[0];
		    	wr[0] = x2[0]/denom[0];
		    	wi[0] = -y2[0]/denom[0];
		    	uxr[i].add(wr[0]);
		    	uyr[i].add(wi[0]);
	    	    if (wr[0] < axlim[0]) {
	    	    	axlim[0] = wr[0];
	    	    }
	    	    if (wr[0] > axlim[1]) {
	    	    	axlim[1] = wr[0];
	    	    }
	    	    if (wi[0] < axlim[2]) {
	    	    	axlim[2] = wi[0];
	    	    }
	    	    if (wi[0] > axlim[3]) {
	    	    	axlim[3] = wi[0];
	    	    }
			} // for (j = 0; j < linhx[i][0].size(); j++)
		} // for (i = 0; i < linhx.length; i++)
		
		
		float xPointArray[] = new float[]{(float)axlim[0], (float)axlim[1], (float)axlim[1], (float)axlim[0], (float)axlim[0]};
		float yPointArray[] = new float[]{(float)axlim[2], (float)axlim[2], (float)axlim[3], (float)axlim[3], (float)axlim[2]};
		ViewJFrameGraph pointGraph = new ViewJFrameGraph(xPointArray, yPointArray, "title", "labelX", "labelY", Color.BLUE);
		pointGraph.setVisible(true);
		ViewJComponentGraph graph = pointGraph.getGraph();
		graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);	
		Graphics g = pointGraph.getFrameGraphics();
		graph.paintComponent(g);
		Rectangle graphBounds = graph.getGraphBounds();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
        
        Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
        
        for (i = 0; i < uxr.length; i++) {
	        for (k = 0; k < uxr[i].size()-1; k++) {
	        	double posx1 = uxr[i].get(k);
	    		double posy1 = uyr[i].get(k);
	    		double posx2 = uxr[i].get(k+1);
	    		double posy2 = uyr[i].get(k+1);
	    		x1Vector.add(posx1);
	    		y1Vector.add(posy1);
	    		x2Vector.add(posx2);
	    		y2Vector.add(posy2);
	    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
			    int x2i =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
			    int y2i =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
			    y2i = -y2i + 2*graphBounds.y + graphBounds.height;
			    graph.drawLine(g, x1, y1, x2i, y2i);	
	        }
        }
        
        graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
	}
	
	public void testDiskmap5() {
		// From Table 1. in Algorithm 756: A MATLAB Toolbox for Schwarz-Christoffel Mapping
		// Error in NLConstrainedEngine during dparam call to dpfun
		// Abnormal termination because the latest search direction computed using subspace minimization
		// was not a descent direction (probably caused by a wrongly computed Jacobian)
		// Error because 4 prevertices are extremely close together in disk
		// (arg zk)?PI
	    // k = 1              0.00800451739
		// k = 2              0.606337224
		// k = 3              1.49999746
		// k = 4              1.49999860
		// k = 5              1.49999865
		// k = 6              1.5
		// k = 7              1.75
		// k = 8              2
		// Would have to use NLConstrainedEngineEP with DoubleDouble and a dpfun with DoubleDouble for this to work.
		int i;
		double w[][] = new double[8][2];
		w[0][0] = 3.2;
		w[0][1] = 2.4;
		w[1][0] = 0.8;
		w[1][1] = -0.4;
		w[2][0] = -0.8;
		w[2][1] = -0.4;
		w[3][0] = -2.8;
		w[3][1] = 2.0;
		w[4][0] = -2.8;
		w[4][1] = -2.0;
		w[5][0] = -0.8;
		w[5][1] = -0.8;
		w[6][0] = 0.8;
		w[6][1]= -0.8;
		w[7][0] = 3.2;
		w[7][1] = -2.0;
		scmap M = diskmap(w, null, tolerance, null, null);
		double diskang[] = new double[8];
		for (i = 0; i < 8; i++) {
		    diskang[i] = (Math.atan2(M.prevertex[i][1], M.prevertex[i][0]))/Math.PI;
		    System.out.println("diskang["+i+"] = " + diskang[i]);
		}
		double R[] = new double[]{10.0};
		double theta[] = new double[]{10.0};
		diskplot(M, R, theta, 200, 140, null, Integer.MIN_VALUE);
	}
	
	public void testDiskmap6() {
		// Example with 2 slits
		//p = polygon([1 0.6 1 1i -1 -1i -0.4i -1i]);
		//f = center( diskmap(p), 0 );
		//plot(f)
		//eval(f,[0 0.1])
		//ans =
		//-0.0000 + 0.0000i 0.0377 - 0.0547i
		//eval( diff(f), 0)
		//ans =
		//0.3896 - 0.5526i
		double w[][] = new double[8][2];
		w[0][0] = 1.0;
		w[0][1] = 0.0;
		w[1][0] = 0.6;
		w[1][1] = 0.0;
		w[2][0] = 1.0;
		w[2][1] = 0.0;
		w[3][0] = 0.0;
		w[3][1] = 1.0;
		w[4][0] = -1.0;
		w[4][1] = 0.0;
		w[5][0] = 0.0;
		w[5][1] = -1.0;
		w[6][0] = 0.0;
		w[6][1] = -0.4;
		w[7][0] = 0.0;
		w[7][1] = -1.0;
		scmap M = diskmap(w, null, tolerance, null, null);
		double wc[] = new double[2];
		M = center(M, wc, null);
		double R[] = new double[]{10.0};
		double theta[] = new double[]{10.0};
		diskplot(M, R, theta, 20, 14, null, Integer.MIN_VALUE);
		double wp[][] = new double[2][2];
		wp[0][0] = 0.0;
		wp[0][1] = 0.0;
		wp[1][0] = 0.1;
		wp[1][1] = 0.0;
		double zp[][] = diskeval(M, wp, tolerance);
		System.out.println("zp[0] = " + zp[0][0] + " " + zp[0][1]+"i");
		System.out.println("zp[1] = " + zp[1][0] + " " + zp[1][1]+"i");
		double zin[][] = new double[1][2];
		double fp[][] = diskevaldiff(M, zin);
		System.out.println("fp = " + fp[0][0] + " " + fp[0][1] + "i");
	}
	
	public void testDiskmap7() {
		int i;
	    double w[][] = new double[6][2];
	    w[0][0] = 4.0;
	    w[0][1] = 0.0;
	    w[1][0] = 0.0;
	    w[1][1] = 2.0;
	    w[2][0] = -2.0;
	    w[2][1] = 4.0;
	    w[3][0] = -3.0;
	    w[3][1] = 0.0;
	    w[4][0] = -3.0;
	    w[4][1] = -1.0;
	    w[5][0] = 2.0;
	    w[5][1] = -2.0;
	    scmap M = diskmap(w, null, tolerance, null, null);	
	    double sqrt2 = Math.sqrt(2.0);
	    double wp[][] = new double[4][2];
	    wp[0][0] = 0.5;
	    wp[0][1] = 0.5;
	    wp[1][0] = -0.9;
	    wp[1][1] = 0.0;
	    wp[2][0] = -0.8;
	    wp[2][1] = 0.3;
	    wp[3][0] = 1.0/sqrt2;
	    wp[3][1] = 1.0/sqrt2;
	    double zp[][] = diskeval(M, wp, tolerance);
	    for (i = 0; i < 4; i++) {
	    	if (i == 0) {
	    		System.out.println("Expected forward result: -2.301479291389453 + 0.891455618349974i");
	    	}
	    	else if (i == 1) {
	    	    System.out.println("Expected forward result: -2.959017053517382 - 0.004724964608807i");	
	    	}
	    	else if (i == 2) {
	    		System.out.println("Expected forward result: -2.920229222824237 + 0.110570172682907i");
	    	}
	    	else {
	    		System.out.println("Expected forward result: -2.699042997340806 + 1.203828010636846i");
	    	}
	    	System.out.println("Actual forward result = " + zp[i][0] + " " + zp[i][1] + "i");
	    }
	    double zpinverse[][] = new double[4][2];
	    int flag[] = null;
		int maxiter = 200;
		flag = diskevalinv(zpinverse, M, zp, M.qdata, null, maxiter);
		for (i = 0; i < 4; i++) {
			if (i == 0) {
				System.out.println("Expected inverse result: 0.5 + 0.5i");
			}
			else if (i == 1) {
				System.out.println("Expected inverse result: -0.9");
			}
			else if (i == 2) {
				System.out.println("Expected inverse result: -0.8 + 0.3i");
			}
			else if (i == 3) {
				double recipsqrt2 = 1.0/sqrt2;
				System.out.println("Expected inverse result = " + recipsqrt2 + " " + recipsqrt2 + "i");
			}
			System.out.println("Actual inverse result = " + zpinverse[i][0] + " " + zpinverse[i][1] + "i");
		}
	}
	
	public void testCRDiskmap1() {
		int i, j, k;
		double w[][] = new double[4][2];
		w[0][0] = 1;
		w[0][1] = 1;
		w[1][0] = -1;
		w[1][1] = 1;
		w[2][0] = -1;
		w[2][1] = -1;
		w[3][0] = 1;
		w[3][1] = -1;
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		scmap M = crdiskmap(poly, tolerance, null, null);
		double wc[] = new double[2];
		M = crdiskCenter(M, wc);
		// Points for unit circle
		int n = 180;
		double wcir[][] = new double[n][2];
		double xcir[] = new double[n];
		double ycir[] = new double[n];
		for (i = 0; i < n; i++) {
			wcir[i][0] = Math.cos(i*2.0*Math.PI/(n+1.0));
			wcir[i][1] = Math.sin(i*2.0*Math.PI/(n+1.0));
			xcir[i] = wcir[i][0];
			ycir[i] = wcir[i][1];
		}
		poly = new polygon(xcir, ycir, null);
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		float xPointArray[] = new float[n+1];
		float yPointArray[] = new float[n+1];
		int nqpts = 5;
		double axlim[] = new double[4];
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, wcir, beta, false, axlim, Integer.MIN_VALUE, true, null);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
		double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		double X[][] = new double[201][9];
		double Y[][] = new double[201][9];
		double wp[][] = new double[201*9][2];
		double zp[][] = new double[201*9][2];
		for (i = 0; i < 201; i++) {
			for (j = 0; j < 9; j++) {
			    X[i][j] = -0.8 + 0.2*j;
			    Y[i][j] = -1.0 + 0.01*i;
			    wp[201*j + i][0] = X[i][j];
			    wp[201*j + i][1] = Y[i][j];
			}
		}
		boolean indexout[] = new boolean[wp.length];
		boolean onvtx[][] = new boolean[w.length][wp.length];
		isinpoly(indexout, onvtx, wp, w, beta, eps);
		int numinpoly = 0;
		for (i = 0; i < wp.length; i++) {
		    if (indexout[i]) {
		    	numinpoly++;
		    }
		}
		System.out.println(numinpoly + " of 1809 points in wp used");
		double wpinpoly[][] = new double[numinpoly][2];
		for (i = 0, j = 0; i < wp.length; i++) {
		    if (indexout[i]) {
		    	wpinpoly[j][0] = wp[i][0];
		    	wpinpoly[j++][1] = wp[i][1];
		    }
		}
		zp = crdiskevalinv(M, wpinpoly, 1.0E-8);
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		k = 0;
		for (i = 0; i < 9; i++) {
			for (j = 0; j < 201; j++) {
				int index = 201*i + j;
				if ((j < 200) && indexout[index] && indexout[index+1]) {
					double posx1 = zp[k][0];
		    		double posy1 = zp[k][1];
		    		double posx2 = zp[k + 1][0];
		    		double posy2 = zp[k + 1][1];
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
				    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
				    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
				    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
				    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
				    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
				    graph.drawLine(g, x1, y1, x2, y2);
				}
				if (indexout[index]) {
					k++;
				}
			}
		}
		for (i = 0; i < 201; i++) {
			for (j = 0; j < 9; j++) {
			    wp[201*j + i][0] = Y[i][j];
			    wp[201*j + i][1] = X[i][j];
			}
		}
		indexout = new boolean[wp.length];
	    onvtx = new boolean[w.length][wp.length];
		isinpoly(indexout, onvtx, wp, w, beta, eps);
		numinpoly = 0;
		for (i = 0; i < wp.length; i++) {
		    if (indexout[i]) {
		    	numinpoly++;
		    }
		}
		System.out.println(numinpoly + " of 1809 points in wp used");
		wpinpoly = new double[numinpoly][2];
		for (i = 0, j = 0; i < wp.length; i++) {
		    if (indexout[i]) {
		    	wpinpoly[j][0] = wp[i][0];
		    	wpinpoly[j++][1] = wp[i][1];
		    }
		}
		zp = crdiskevalinv(M, wpinpoly, 1.0E-8);
		k = 0;
		for (i = 0; i < 9; i++) {
			for (j = 0; j < 201; j++) {
				int index = 201*i + j;
				if ((j < 200) && indexout[index] && indexout[index+1]) {
					double posx1 = zp[k][0];
		    		double posy1 = zp[k][1];
		    		double posx2 = zp[k + 1][0];
		    		double posy2 = zp[k + 1][1];
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
				    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
				    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
				    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
				    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
				    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
				    graph.drawLine(g, x1, y1, x2, y2);
				}
				if (indexout[index]) {
					k++;
				}
			}
		}
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
	}
	
	public void testCRDiskmap2() {
		int i;
		double w[][] = new double[4][2];
		w[0][0] = -4;
		w[0][1] = -1;
		w[1][0] = 4;
		w[1][1] = -1;
		w[2][0] = 4;
		w[2][1] = 1;
		w[3][0] = -4;
		w[3][1] = 1;
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		scmap M = crdiskmap(poly, tolerance, null, null);
		double wc[] = new double[2];
		M = crdiskCenter(M, wc);
		double R[] = new double[4];
		for (i = 0; i < 4; i++) {
			R[i] = 0.2*(i+1);
		}
		double theta[] = new double[4];
		// Use theta values from testDiskmap2
		theta[0] = 0.014939472942825261;
		theta[1] = -3.141592618638339;
		theta[2] = -3.1266531461319778;
		theta[3] = 0.0;
		boolean drawThetaToRadiusOne = true;
		crdiskplot(M, R, theta, 200, 140, drawThetaToRadiusOne, null, Integer.MIN_VALUE);
	}
	
	public void testCRDiskmap3() {
		// MATLAB AND MIPAV both show same kinks in drawing recentered at -0.5-0.5i
		//w = [0.0 + 1.0i; -1.0  + 1.0i; -1.0 - 1.0i; 1.0 - 1.0i; 1.0 + 0.0i; 0.0 + 0.0i];
		//beta = [-0.5; -0.5; -0.5; -0.5; -0.5; 0.5];

		//x = [0.0; -1.0; -1.0; 1.0; 1.0; 0.0];
		//y = [1.0; 1.0; -1.0; -1.0; 0.0; 0.0];
		//poly = polygon(x, y);
		//w = vertex(poly);
		//beta = angle(poly)-1;
		//[w, beta, cr, aff, Q, orig, qdata] = crparam(w, beta);
		//poly = polygon(w,beta+1);
		//M.crossratio = cr;
		//M.affine = aff;
		//M.qlgraph = Q;
		//M.qdata = qdata;
		//M.original = orig;

		//% Set conformal center as center of 1st triangle
		//T = Q.qlvert(1:3,1);
		//wc = mean(w(T));
		//wcfix = crfixwc(w,beta,cr,aff,Q,wc);
		//M.center = {wc,wcfix};
		//n = length(w);
		 
		//% Crossratios of target polygon
		//crtarget = crossrat(w,Q);
		 
		//% Actual crossratios
		//crimage = zeros(n-3,1);         % image vertex crossratios
		 
		//% Compute crossratio for each image quadrilateral
		//for k = 1:n-3
		  //prever = crembed(cr,Q,k);
		  //wq = -crquad(prever(Q.qlvert(:,k)),Q.qlvert(:,k),prever,beta,qdata);
		  //crimage(k) = (wq(2)-wq(1))*(wq(4)-wq(3))/((wq(3)-wq(2))*(wq(1)-wq(4)));
		//end
		 
		//% Compare them
		//acc = max(abs(crimage-crtarget));
		////R = 10;
		//theta = 10;
		//[H,R2,THETA] = crplot(w,beta,cr,aff,wcfix,Q,R,theta);
		//wc = -0.5-0.5i;
		//wcfix = crfixwc(w,beta,cr,M.affine,M.qlgraph,wc);
		//M.center = {wc,wcfix};
		//[H,R2,THETA] = crplot(w,beta,cr,aff,wcfix,Q,R,theta);

		int i;
		double w[][] = new double[6][2];
		w[0][0] = 0.0;
		w[0][1] = 1.0;
		w[1][0] = -1.0;
		w[1][1] = 1.0;
		w[2][0] = -1.0;
		w[2][1] = -1.0;
		w[3][0] = 1.0;
		w[3][1] = -1.0;
		w[4][0] = 1.0;
		w[4][1] = 0.0;
		w[5][0] = 0.0;
		w[5][1] = 0.0;
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		scmap M = crdiskmap(poly, tolerance, null, null);
		System.out.println("center = " + M.center[0] + " " + M.center[1]+"i");
		boolean drawThetaToRadiusOne = true;
		crdiskplot(M, null, null, 20, 12, drawThetaToRadiusOne, null, Integer.MIN_VALUE);
		double wc[] = new double[2];
		wc[0] = -0.5;
		wc[1] = -0.5;
		M = crdiskCenter(M, wc);
		System.out.println("center = " + M.center[0] + " " + M.center[1]+"i");
		drawThetaToRadiusOne = false;
		crdiskplot(M, null, null, 20, 12, drawThetaToRadiusOne, null, Integer.MIN_VALUE);
	}
	
	public void testCRDiskmap5() {
		// From Table 1. in Algorithm 756: A MATLAB Toolbox for Schwarz-Christoffel Mapping
		
		// (arg zk)?PI
	    // k = 1              0.00800451739
		// k = 2              0.606337224
		// k = 3              1.49999746
		// k = 4              1.49999860
		// k = 5              1.49999865
		// k = 6              1.5
		// k = 7              1.75
		// k = 8              2
		int i;
		double w[][] = new double[8][2];
		w[0][0] = 3.2;
		w[0][1] = 2.4;
		w[1][0] = 0.8;
		w[1][1] = -0.4;
		w[2][0] = -0.8;
		w[2][1] = -0.4;
		w[3][0] = -2.8;
		w[3][1] = 2.0;
		w[4][0] = -2.8;
		w[4][1] = -2.0;
		w[5][0] = -0.8;
		w[5][1] = -0.8;
		w[6][0] = 0.8;
		w[6][1]= -0.8;
		w[7][0] = 3.2;
		w[7][1] = -2.0;
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		scmap M = crdiskmap(poly, tolerance, null, null);
		boolean drawThetaToRadiusOne = false;
		crdiskplot(M, null, null, 200, 140, drawThetaToRadiusOne, null, Integer.MIN_VALUE);
	}
	
	public void testCRDiskmap7() {
		int i, j;
	    double w[][] = new double[6][2];
	    w[0][0] = 4.0;
	    w[0][1] = 0.0;
	    w[1][0] = 0.0;
	    w[1][1] = 2.0;
	    w[2][0] = -2.0;
	    w[2][1] = 4.0;
	    w[3][0] = -3.0;
	    w[3][1] = 0.0;
	    w[4][0] = -3.0;
	    w[4][1] = -1.0;
	    w[5][0] = 2.0;
	    w[5][1] = -2.0;
	    double x[] = new double[6];
	    double y[] = new double[6];
	    for (i = 0; i < 6; i++) {
	    	x[i] = w[i][0];
	    	y[i] = w[i][1];
	    }
	    polygon poly = new polygon(x, y, null);
	    double beta[] = new double[poly.angle.length];
	    for (i = 0; i < poly.angle.length; i++) {
	    	beta[i] = poly.angle[i] - 1.0;
	    }
	    scmap M = crdiskmap(poly, tolerance, null, null);	
	    double sqrt2 = Math.sqrt(2.0);
	    double wp[][] = new double[4][2];
	    wp[0][0] = 0.5;
	    wp[0][1] = 0.5;
	    wp[1][0] = -0.9;
	    wp[1][1] = 0.0;
	    wp[2][0] = -0.8;
	    wp[2][1] = 0.3;
	    wp[3][0] = 1.0/sqrt2;
	    wp[3][1] = 1.0/sqrt2;
	    poly = M.poly;
	    beta = new double[poly.angle.length];
	    for (i = 0; i < poly.angle.length; i++) {
	    	beta[i] = poly.angle[i] - 1.0;
	    }
	    w = new double[poly.vertex.length][2];
	    for (i = 0; i < poly.vertex.length; i++) {
	    	w[i][0] = poly.vertex[i][0];
	    	w[i][1] = poly.vertex[i][1];
	    }
	    double zp[][] = crmap(wp,w,beta,M.crossratio,M.affine,M.center_fix_quadnum, M.center_fix_mt, M.qgraph, M.qdata);
	    for (i = 0; i < 4; i++) {
	    	System.out.println("Actual forward result = " + zp[i][0] + " " + zp[i][1] + "i");
	    }
	    boolean indexout[] = new boolean[zp.length];
		boolean onvtx[][] = new boolean[w.length][zp.length];
		System.out.println("About to do isinpoly");
		isinpoly(indexout, onvtx, zp, w, beta, eps);
		int numinpoly = 0;
		for (i = 0; i < zp.length; i++) {
		    if (indexout[i]) {
		    	numinpoly++;
		    }
		}
		System.out.println(numinpoly + " of 4 points in zp used");
		double zpinpoly[][] = new double[numinpoly][2];
		int polyindex[] = new int[numinpoly];
		for (i = 0, j = 0; i < wp.length; i++) {
		    if (indexout[i]) {
		    	polyindex[j] = i;
		    	zpinpoly[j][0] = zp[i][0];
		    	zpinpoly[j++][1] = zp[i][1];
		    }
		}
		double zpinverse[][] = crdiskevalinv(M, zpinpoly, 1.0E-8);
		for (i = 0; i < numinpoly; i++) {
			if (polyindex[i] == 0) {
				System.out.println("Expected inverse result: 0.5 + 0.5i");
			}
			else if (polyindex[i] == 1) {
				System.out.println("Expected inverse result: -0.9");
			}
			else if (polyindex[i] == 2) {
				System.out.println("Expected inverse result: -0.8 + 0.3i");
			}
			else if (polyindex[i] == 3) {
				double recipsqrt2 = 1.0/sqrt2;
				System.out.println("Expected inverse result = " + recipsqrt2 + " " + recipsqrt2 + "i");
			}
			System.out.println("Actual inverse result = " + zpinverse[i][0] + " " + zpinverse[i][1] + "i");
		}
	}
	
	private void testExtermap1() {
		//p = i*polygon([-0.5,1-1.5i,-0.5,0.5+2i]);
		//f = extermap(p);
		//axis([-3.05 2.8 -2.3 2.5]), hold on
		//plot(f,(4:9)/10,0)
		int i;
		double w[][] = new double[4][2];
		w[0][0] = 0.0;
		w[0][1] = -0.5;
		w[1][0] = 1.5;
		w[1][1] = 1.0;
		w[2][0] = 0.0;
		w[2][1] = -0.5;
		w[3][0] = -2.0;
		w[3][1] = 0.5;
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		double alpha[] = poly.angle;
		scmap M = extermap(w, alpha, tolerance, null, null);
		double R[] = new double[]{4.0/10.0, 5.0/10.0, 6.0/10.0, 7.0/10.0, 8.0/10.0, 9.0/10.0};
		double theta[] =new double[1];
		double axis[] = new double[]{-3.05, 2.8, -2.3, 2.5};
		exterplot(M, R, theta, 20, 14, null, Integer.MIN_VALUE, axis);
	}
	
	private void testExtermap2() {
		int i;
	    double w[][] = new double[6][2];
	    w[0][0] = 4.0;
	    w[0][1] = 0.0;
	    w[1][0] = 0.0;
	    w[1][1] = 2.0;
	    w[2][0] = -2.0;
	    w[2][1] = 4.0;
	    w[3][0] = -3.0;
	    w[3][1] = 0.0;
	    w[4][0] = -3.0;
	    w[4][1] = -1.0;
	    w[5][0] = 2.0;
	    w[5][1] = -2.0;
	    double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		double alpha[] = poly.angle;
		scmap M = extermap(w, alpha, tolerance, null, null);
	    double sqrt2 = Math.sqrt(2.0);
	    double zp[][] = new double[4][2];
	    zp[0][0] = 0.5;
	    zp[0][1] = 0.5;
	    zp[1][0] = -0.9;
	    zp[1][1] = 0.0;
	    zp[2][0] = -0.8;
	    zp[2][1] = 0.3;
	    zp[3][0] = 1.0/sqrt2;
	    zp[3][1] = 1.0/sqrt2;	
	    double wi[][] = new double[poly.vertex.length][2];
		for (i = 0; i < poly.vertex.length; i++) {
			wi[i][0] = poly.vertex[poly.vertex.length - 1 - i][0];
			wi[i][1] = poly.vertex[poly.vertex.length - 1 - i][1];
		}
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = 1.0 - poly.angle[poly.angle.length - 1 - i];
		}
        double wp[][] = demap(zp, wi, beta, M.prevertex, M.constant, M.qdata);
        for (i = 0; i < wp.length; i++) {
        	if (i == 0) {
        		System.out.println("Expected forward result: 3.383320944105799 - 2.338017988574543i");
        	}
        	else if (i == 1) {
        		System.out.println("Expected forward result: -3.257095120413423 + 0.536117032603197i");
        	}
        	else if (i == 2) {
        		System.out.println("Expected forward result: -3.428512520416633 - 0.641446812228358i");
        	}
        	else if (i == 3) {
        		System.out.println("Expected forward result: 2.571844815094428 - 1.428155184905573i");
        	}
        	System.out.println("Actual forward result: " + wp[i][0] + " " + wp[i][1] + "i");
        }
		int maxiter = 200;
	    double wpinverse[][] = exteriorevalinv(M, wp, M.qdata, null, maxiter);
		for (i = 0; i < 4; i++) {
			if (i == 0) {
				System.out.println("Expected inverse result: 0.5 + 0.5i");
			}
			else if (i == 1) {
				System.out.println("Expected inverse result: -0.9");
			}
			else if (i == 2) {
				System.out.println("Expected inverse result: -0.8 + 0.3i");
			}
			else if (i == 3) {
				double recipsqrt2 = 1.0/sqrt2;
				System.out.println("Expected inverse result = " + recipsqrt2 + " " + recipsqrt2 + "i");
			}
			System.out.println("Actual inverse result = " + wpinverse[i][0] + " " + wpinverse[i][1] + "i");
		}
	}
	
	public void testCRRectmap1() {
		// Example from users guide
		// Test with
		// SchwarzChristoffelMapping sc = new SchwarzChristoffelMapping();
    	// sc.runAlgorithm();
		double w[][] = new double[6][2];
        w[0][0] = -5;
        w[0][1] = -1;
        w[1][0] = -5;
        w[1][1] = -3;
        w[2][0] = 5;
        w[2][1] = -3;
        w[3][0] = 5;
        w[3][1] = 1;
        w[4][0] = 5;
        w[4][1] = 3;
        w[5][0] = -5;
        w[5][1] = 3;
        int corner[] = new int[4];
        corner[0] = 0;
        corner[1] = 1;
        corner[2] = 3;
        corner[3] = 4;
        scmap M = crrectmap(w, corner, tolerance);
        double nre[] = new double[]{10.0};
        double nim[] = new double[]{10.0};
        crrectplot(M, nre, nim, Integer.MIN_VALUE);
	}
	
	public void testCRRectmap2() {
		// Example from Algorithm 843: Scwarz-Christoffel Toolbox for MATLAB
		//ans =
	    // 1.5708
		// 1.5708 +11.1798i
	    // -1.5708 +11.1798i
		// -1.5708
		int i;
		double w[][] = new double[6][2];
        w[0][0] = -1;
        w[0][1] = 1;
        w[1][0] = -1;
        w[1][1] = -1;
        w[2][0] = 2;
        w[2][1] = -1;
        w[3][0] = 2;
        w[3][1] = 0;
        w[4][0] = 0;
        w[4][1] = 0;
        w[5][0] = 0;
        w[5][1] = 1;
        int corner[] = new int[4];
        corner[0] = 0;
        corner[1] = 2;
        corner[2] = 3;
        corner[3] = 5;
        scmap M = crrectmap(w, corner, tolerance);
        double nre[] = new double[]{8};
        double nim[] = new double[]{4};
        crrectplot(M, nre, nim, Integer.MIN_VALUE);
	}
	
	public void testCRRectmap3() {
		int i, j;
	    double w[][] = new double[6][2];
	    w[0][0] = 4.0;
	    w[0][1] = 0.0;
	    w[1][0] = 0.0;
	    w[1][1] = 2.0;
	    w[2][0] = -2.0;
	    w[2][1] = 4.0;
	    w[3][0] = -3.0;
	    w[3][1] = 0.0;
	    w[4][0] = -3.0;
	    w[4][1] = -1.0;
	    w[5][0] = 2.0;
	    w[5][1] = -2.0;
	    double x[] = new double[6];
	    double y[] = new double[6];
	    for (i = 0; i < 6; i++) {
	    	x[i] = w[i][0];
	    	y[i] = w[i][1];
	    }
	    polygon poly = new polygon(x, y, null);
	    double beta[] = new double[poly.angle.length];
	    for (i = 0; i < poly.angle.length; i++) {
	    	beta[i] = poly.angle[i] - 1.0;
	    }
	    int corner[] = new int[4];
	    corner[0] = 0;
	    corner[1] = 1;
	    corner[2] = 2;
	    corner[3] = 3;
	    scmap M = crrectmap(w, corner, tolerance);
	    // Note that poly is changed by crrectmap
	    double zp[][] = new double[6][2];
	    zp[0][0] = 1.5;
	    zp[0][1] = 0.0;
	    zp[1][0] = 1.4;
	    zp[1][1] = 3.0;
	    zp[2][0] = -0.6;
	    zp[2][1] = 1.0;
	    zp[3][0] = 1.0;
	    zp[3][1] = 0.0;
	    zp[4][0] = 0.5;
	    zp[4][1] = 0.5;
	    zp[5][0] = 0.2;
	    zp[5][1] = 0.8;
	    boolean indexout[] = new boolean[zp.length];
		boolean onvtx[][] = new boolean[w.length][zp.length];
		poly = M.poly;
		w = new double[poly.vertex.length][2];
		for (i = 0; i < poly.vertex.length; i++) {
			w[i][0] = poly.vertex[i][0];
			w[i][1] = poly.vertex[i][1];
		}
		beta = new double[poly.angle.length];
	    for (i = 0; i < poly.angle.length; i++) {
	    	beta[i] = poly.angle[i] - 1.0;
	    }
		polygon pr = M.rectpolygon;
		double wr[][] = pr.vertex;
		double betar[] = new double[pr.angle.length];
		for (i = 0; i < pr.angle.length; i++) {
			betar[i] = pr.angle[i] - 1;
		}
		double zr[][] = new double[corner.length][2];
        for (i = 0; i < corner.length; i++) {
            zr[i][0] = wr[corner[i]][0];
            zr[i][1] = wr[corner[i]][1];
            System.out.println("zr["+i+"] = " + zr[i][0] + " " + zr[i][1]);
        }
        // zp points must be inside polygon pr to be transformed
        // zr[0] = 0.0 0.0
        // zr[1] = 0.9999999999999999 1.6653345369377348E-16
        // zr[2] = 0.9999999955356558 0.9826996089194511
        // zr[3] = -2.8639270038821607E-8 0.9826996041381473
	    isinpoly(indexout, onvtx, zp, zr, betar, eps);
		int numinpoly = 0;
		for (i = 0; i < zp.length; i++) {
		    if (indexout[i]) {
		    	numinpoly++;
		    }
		} // for (i = 0; i < zp.length; i++)
		System.out.println("numinpoly = " + numinpoly);
		double zpinpoly[][] = new double[numinpoly][2];
		int polyindex[] = new int[numinpoly];
		for (i = 0, j = 0; i < zp.length; i++) {
			if (indexout[i]) {
				zpinpoly[j][0] = zp[i][0];
				zpinpoly[j][1] = zp[i][1];
				polyindex[j] = i;
				j++;
			}
		} // for (i = 0, j = 0; i < zp.length; i++)
		for (i = 0; i < numinpoly; i++) {
			System.out.println("zpinpoly["+i+"] = " + zpinpoly[i][0] + " " + zpinpoly[i][1]);
		}
		double wpinpoly[][][] = new double[numinpoly][1][2];
		//rmap(wpinpoly, zpinpoly, w, beta, z, M.constant, M.stripL, M.qdata);
		// Number of quadrature points per integration.
		// Approximately equals -log10(error).  Increase if plot
		// has false little zigzags in curves. 
		int nqpts = 5; 
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		double qdatr[][] = new double[nqpts][2*betar.length+2];
		scqdata(qdatr, betar, nqpts);
		double tolmap = Math.pow(10.0, -qdat.length);
        int zpindex[] = new int[numinpoly];
		onvtx = new boolean[wr.length][numinpoly];
		isinpoly2(zpindex, onvtx, zpinpoly, wr, betar, tolmap);
		int maxindex = -1;
		for (i = 0; i < numinpoly; i++) {
			if (zpindex[i] > maxindex) {
				maxindex = zpindex[i];
			}
		} // for (i = 0; i < numnew; i++)
		if (maxindex > 1) {
			MipavUtil.displayError("Too many values found at some points");
			System.exit(-1);
		}
		int wpqn[][] = new int[numinpoly][Math.max(1, maxindex)];
		double cr[] = M.crossratio;
		double aff[][][] = M.affine;
		double affr[][][] = M.rectaffine;
		qlgraph Q = M.qgraph;
	    crrmap(wpinpoly, wpqn, zpinpoly, w, beta, wr, betar, cr, aff, affr,
        		Q, qdat, qdatr);
	    
	    for (i = 0; i < numinpoly; i++) {
	    	System.out.println("Actual forward result: " + wpinpoly[i][0][0] + " " + wpinpoly[i][0][1] + "i");
	    }
	    double wp2[][] = new double[numinpoly][2];
	    for (i = 0; i < numinpoly; i++) {
	    	wp2[i][0] = wpinpoly[i][0][0];
	    	wp2[i][1] = wpinpoly[i][0][1];
	    }
	    // Note that by switching the roles of w, beta, and aff with wr, betar, and affr,
	    // one inverts the map.
	    double wpinverse[][][] = new double[numinpoly][1][2];
	    crrmap(wpinverse, wpqn, wp2, wr, betar, w, beta, cr, affr, aff,
        		Q, qdat, qdatr);
	    for (i = 0; i < numinpoly; i++) {
	    	if (polyindex[i] == 0) {
	    	    System.out.println("Expected inverse result: 1.5");
	    	}
	    	else if (polyindex[i] == 1) {
	    		System.out.println("Expected inverse result: 1.4 + 3i");
	    	}
	    	else if (polyindex[i] == 2) {
	    		System.out.println("Expected inverse result: -0.6 + 1.0i");
	    	}
	    	else if (polyindex[i] == 3) {
	    		System.out.println("Expected inverse result: 1.0");
	    	}
	    	else if (polyindex[i] == 4) {
	    		System.out.println("Expected inverse 0.5 + 0.5i");
	    	}
	    	else if (polyindex[i] == 5) {
	    		System.out.println("Expected inverse 0.2 + 0.8i");
	    	}
	    	System.out.println("Actual inverse result: " + wpinverse[i][0][0] + " " + wpinverse[i][0][1] + "i");
	    }
	}
	
	public scmap crdiskmap(polygon poly, double tolerance, double cr[][], qlgraph Q) {
		// crdiskmap constructs a cross-ratio disk map object for the polygon p. The
		// parameter problem is solved using default options for the crossratios of
		// the prevertices.  If cr and Q are supplied, crdiskmap creates a diskmap
		// object having prevertex crossratios cr and quadrilateral graph Q.
		// Use crdiskmap instead of diskmap when the polygon has elongations, or 
		// when diskmap fails to converge.
		// Original MATLAB routine copyright 1998-2001 by Toby Driscoll
		int i, j;
		double wn[][];
		double betan[];
		
		// Get data for the low-level functions
		double w[][] = poly.vertex;
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		
		// Find prevertices if necessary
		if ((cr == null) || (cr.length == 0)) {
		    // Apply scfix to enforce solver rules
			// Solve parameter problem
			// Apply scfix to enforce solver rules
			wn = new double[w.length+2][2];
			betan = new double[w.length+2];
			// Number of vertices added by scfix
			int verticesAdded[] = new int[1];
			int initialVertices = w.length;
			for (i = 0; i < w.length; i++) {
				wn[i][0] = w[i][0];
				wn[i][1] = w[i][1];
				betan[i] = beta[i];
			}
			scfix(wn, betan, verticesAdded, null, "d", w, beta, null);
			double wn2[][];
			double betan2[];
			if ((verticesAdded[0] == 0) ||(verticesAdded[0] == 1)) {
			    wn2 = new double[initialVertices + verticesAdded[0]][2];
				betan2 = new double[initialVertices + verticesAdded[0]];
				for (i = 0; i < initialVertices + verticesAdded[0]; i++) {
					wn2[i][0] = wn[i][0];
					wn2[i][1] = wn[i][1];
					betan2[i] = betan[i];
				}
			}
			else {
				wn2 = wn;
				betan2 = betan;
			}
			// Standard solution
			// Return crsplit_neww
			// Return crparam_beta
			// Return crsplit_orig
			crparam(wn2, betan2, null, tolerance);
			// Remake polygon to reflect change in w
			double x[] = new double[crsplit_neww.length];
			double y[] = new double[crsplit_neww.length];
			double alpha[] = new double[crparam_beta.length];
			for (i = 0; i < crsplit_neww.length; i++) {
				x[i] = crsplit_neww[i][0];
				y[i] = crsplit_neww[i][1];
			}
			for (i = 0; i < crparam_beta.length; i++) {
				alpha[i] = crparam_beta[i] + 1.0;
			}
			poly = new polygon(x, y, alpha);
		} // if ((cr == null) || (cr.length == 0))
		scmap M = new scmap();
		M.crossratio = crparam_cr;
		M.affine = craffine_aff;
		M.qgraph = crqgraph_Q;
		M.qdata = crparam_qdata;
		M.original = crsplit_orig;
		M.poly = poly;
		
		// Set conformal center as center of 1st triangle
		int T[] = new int[3];
		double sum[] = new double[2];
		for (i = 0; i < 3; i++) {
		    T[i] = crqgraph_Q.qlvert[i][0];
		    sum[0] += crsplit_neww[T[i]][0];
		    sum[1] += crsplit_neww[T[i]][1];
		}
		double wc[] = new double[2];
		wc[0] = sum[0]/3.0;
		wc[1] = sum[1]/3.0;
		M.center = wc;
		crfixwc(crsplit_neww, crparam_beta, crparam_cr, craffine_aff, crqgraph_Q, wc);
		M.center_fix_quadnum = crfixwc_quadnum;
		for (i = 0; i < 4; i++) {
			for (j = 0; j < 2; j++) {
				M.center_fix_mt[i][j] = crfixwc_mt[i][j];
			}
		}
		
		// Now fill in true accuracy
		M.accuracy = crdiskAccuracy(M);
		return M;
	}
	
	private scmap crdiskCenter(scmap M, double wc[]) {
		// Conformal center of Schwarz_Christoffel disk map.
		// If wc is null crdiskCenter returns the conformal center (image of 0) of the
		// Schwarz-Christoffel crossratio disk map represented by M.
		// crdiskCenter(M, wc) computes a map conformally equivalent to M but with
		// conformal center wc (provided wc is inside the polygon of M), and
		// returns the new map.  If wc is empty, you will be asked to select it
		// graphically.
		// Original MATLAB center routine copyright 1998 by Toby Driscoll.
		int i, j;
		polygon p = M.poly;
		double cr[] = M.crossratio;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		crfixwc(w, beta, cr, M.affine, M.qgraph, wc);
		M.center[0] = wc[0];
		M.center[1] = wc[1];
		M.center_fix_quadnum = crfixwc_quadnum;
		for (i = 0; i < 4; i++) {
			for (j = 0; j < 2; j++) {
				M.center_fix_mt[i][j] = crfixwc_mt[i][j];
			}
		}
		return M;
	}
	
	private double crdiskAccuracy(scmap M) {
		// Apparent accuracy of Schwarz_christoffel cross-ratio disk map.
		// crdiskAccuracy estimates the accuracy of the Schwarz-christoffel disk map M.
		// The technique used is to compare the cross-ratios of the actual polygon image
		// with those of the target polygon, and return the maximum.
		// Original MATLAB accrayc routine copyright 1998 by Toby Driscoll.
		int i;
		double acc = 0.0;
		int k;
		double numre[] = new double[1];
		double numim[] = new double[1];
		double denomre[] = new double[1];
		double denomim[] = new double[1];
		double cre[] = new double[1];
		double cim[] = new double[1];
		double presentAcc;
		
		// Get data for low-level functions.
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		double cr[] = M.crossratio;
		double aff[][][] = M.affine;
		qlgraph Q = M.qgraph;
		double qdata[][] = M.qdata;
		
		int n = w.length;
		
		// Crossratios of target polygon
		double crtarget[][] = crossrat(w, Q);
		
		// Actual crossratios
		double crimage[][] = new double[n-3][2]; // image vertex crossratios
		
		// Compute crossratio for each image quadrilateral
		double z1[][] = new double[4][2];
		int sing1[] = new int[4];
		for (k = 0; k < n-3; k++) {
		    double prever[][] = crembed(cr, Q, k);
		    for (i = 0; i < 4; i++) {
		    	sing1[i] = Q.qlvert[i][k];
		    	z1[i][0] = prever[sing1[i]][0];
		    	z1[i][1] = prever[sing1[i]][1];
		    }
		    double wq[][] = crquad(z1, sing1, prever, beta, qdata);
		    for (i = 0; i < 4; i++) {
		    	wq[i][0] = -wq[i][0];
		    	wq[i][1] = -wq[i][1];
		    }
		    zmlt(wq[1][0] - wq[0][0], wq[1][1] - wq[0][1], wq[3][0] - wq[2][0], wq[3][1] - wq[2][1], numre, numim);
		    zmlt(wq[2][0] - wq[1][0], wq[2][1] - wq[1][1], wq[0][0] - wq[3][0], wq[0][1] - wq[3][1], denomre, denomim);
		    zdiv(numre[0], numim[0], denomre[0], denomim[0], cre, cim);
		    crimage[k][0] = cre[0];
		    crimage[k][1] = cim[0];
	    } // for (k = 0; k < n-3; k++)
		
		// Compare them
		for (i = 0; i < n-3; i++) {
			//System.out.println("crimage["+i+"]["+0+"] = " + crimage[i][0]);
			//System.out.println("crtarget["+i+"]["+0+"] = " + crtarget[i][0]);
			//System.out.println("crimage["+i+"]["+1+"] = " + crimage[i][1]);
			//System.out.println("crtarget["+i+"]["+1+"] = " + crtarget[i][1]);
		    presentAcc = zabs(crimage[i][0] - crtarget[i][0], crimage[i][1] - crtarget[i][1]);
		    if (presentAcc > acc) {
		    	acc = presentAcc;
		    }
		}
		
		System.out.println("Accuracy for cross-ratio disk map = " + acc);
		
		return acc;
	}
	public void crparam(double w[][], double beta[], double cr0[][], double tol) {
		// Crossratio parameter problem
		//  crparam solves the parameter problem associated with the crossratio formulation for the
		// polygon given by w and beta.  The polygon is first subdivided, then a Delaunay triangulation
		// and associated quadrilateral graph are found.  The nonlinear system of equations is solved
		// to find the prevertex crossratios cr.
		// The returned arguments are the subdivded polygon (w and beta), the crossratios (cr),
		// the afine transformation data computed by craffine (aff), the quadrilateral data
		// structure (Q), and the original-vertex flag vector returned by crsplit (orig).
		
		// If the output arguments are just [cr, aff, Q], the subdivision step is skipped.
		// This is likely to destroy accuracy if the polygon has elongations or pinches,
		// unless subdivision has already occurred.  
		
		// crparam uses cr0 as an initial guess in the nonlinear solution for cr.  Note that
		// cr0.length must be w.length - 3 after subdivision, so this parameter is useful only
		// when the subdivision step has been preprocessed.
		
		// Original MATLAB routine copyright 1998-2001 by Toby Driscoll.
		
		int i;
		int n = w.length;
		
		 // Check inputs
	    int err = sccheck("cr", w, beta, null);
	    if (err == -1) {
	    	return;
	    }
	    if (err == 1) {
	    	MipavUtil.displayError("Use scfix to make polygon obey requirements");
	    	return;
	    }
	    
	    int nqpts = (int)Math.max(Math.ceil(-Math.log10(tol)), 4);
	    
	    // Split
	    // Return crsplit_neww
	    // Return crsplit_orig
	    crsplit(w);
	    n = crsplit_neww.length;
	    crparam_beta = scangle(crsplit_neww);
	    crtriang(crsplit_neww);
	    crcdt(crsplit_neww);
	    
	    // Quadrilateral graph'
	    crqgraph(crsplit_neww);
	    
	    // Quadrature data
	    crparam_qdata = new double[nqpts][2*crparam_beta.length+2];
	    scqdata(crparam_qdata, crparam_beta, nqpts);
	    
	    // Find the crossratios to be sought
	    double target[][] = crossrat(crsplit_neww, crqgraph_Q);
	    
	    double z0[];
	    // Set up starting guess
	    if ((cr0 == null) || (cr0.length == 0)) {
	        z0 = new double[target.length];
	        for (i= 0; i < target.length; i++) {
	        	z0[i] = Math.log(zabs(target[i][0], target[i][1]));
	        }
	    }
	    else {
	    	z0 = new double[cr0.length];
	    	for (i= 0; i < cr0.length; i++) {
	        	z0[i] = Math.log(zabs(cr0[i][0], cr0[i][1]));
	        }
	    }
	    
	    // Solve nonlinear system of equations
	    crpfun fm = new crpfun(z0, n, crparam_beta, target, crqgraph_Q, crparam_qdata);
	    fm.driver();
		fm.dumpResults();
	    int exitStatus = fm.getExitStatus();
	    if (exitStatus < 0 ) {
	    	System.out.println("Error in NLConstrainedEngine during crparam call to crpfun");
	    	printExitStatus(exitStatus);
	    	System.exit(-1);
	    }
		double z[] = fm.getParameters();
		
		// Results
		crparam_cr = new double[z.length];
		for (i = 0; i < z.length; i++) {
			crparam_cr[i] = Math.exp(z[i]);
		}
		
		craffine(crsplit_neww, crparam_beta, crparam_cr, crqgraph_Q, tol);
	}
	
	private void crsplit(double w[][]) {
		// Split polygon edges to ensure good crossratios.
		// crsplit splits the edges of the polygon vertices w to attempt to ensure that subsequent
		// triangulation will produce quadrilaterals having reasonable crossratios.  The first step
		// is to "chop off" very sharp exterior corners.  Then, an iterative procedure subdivides
		// the long edges of narrow "channels."  This process is guaranteed to terminate, but not
		// necessarily to be optimal.
		
	    // crsplit also returns a logical vector crsplit_orig of the same length as crsplit_neww.
		// The 1 entries of crsplit_orig correspond to the original entries of w; i.e.
		// crsplit_neww(crsplit_orig) returns the original w.
		
		// crsplit uses Delaunay triangulations to do its job..  crsplit returns a cdt as computed
		// by cdtriang/crcdt.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		
		// First, isolate sharp corners.
		int i, j, k;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int n = w.length;
		double beta[] = scangle(w);
		boolean sharp[] = new boolean[n];
		for (i = 0; i < n; i++) {
			sharp[i] = (beta[i] < -0.75);
		}
		double neww[][][] = new double[3][n][2];
		for (i = 0; i < 3; i++) {
			for (j = 0; j < n; j++) {
				neww[i][j][0] = Double.NaN;
			}
		}
		for (i = 0; i < n; i++) {
			neww[1][i][0] = w[i][0];
			neww[1][i][1] = w[i][1];
		}
		double ang[] = new double[n];
    	boolean vis[] = new boolean[n];
	    for (j = 0; j < n; j++) {
	        if (sharp[j]) {
	        	// Find distance to nearest "visible" vertex
	        	int jm1 = (j-1+n)%n;
	        	int jp1 = (j+1)%n;
	        	for (i = 0; i < n; i++) {
	        		zdiv(w[i][0] - w[j][0], w[i][1] - w[j][1], w[jp1][0] - w[j][0], w[jp1][1] - w[j][1], cr, ci);
	        		double theta = Math.atan2(ci[0],  cr[0]) + 2.0*Math.PI;
	        		ang[i] = theta - 2.0*Math.PI*Math.floor(theta/(2.0*Math.PI));
	        	}
	        	for (i = 0; i < n; i++) {
	        		vis[i] = (ang[i] > 0) && (ang[i] < ang[jm1]);
	        	} 
	        	vis[(j-1+n)%n] = true;
	        	vis[(j+n)%n] = false;
	        	vis[(j+1+n)%n] = true;
	        	double mindist = Double.MAX_VALUE;
	        	for (i = 0; i < n; i++) {
	        		if (vis[i]) {
	        		    double dist = zabs(w[i][0] - w[j][0], w[i][1] - w[j][1]);
	        		    if (dist < mindist) {
	        		    	mindist = dist;
	        		    }
	        		}
	        	} // for (i = 0; i < n; i++)
	        	double result[] = sign(w[jm1][0] - w[j][0], w[jm1][1] - w[j][1]);
	        	neww[0][j][0] = w[j][0] + 0.5*mindist*result[0];
	        	neww[0][j][1] = w[j][1] + 0.5*mindist*result[1];
	        	result = sign(w[jp1][0] - w[j][0], w[jp1][1] - w[j][1]);
	        	neww[2][j][0] = w[j][0] + 0.5*mindist*result[0];
	        	neww[2][j][1] = w[j][1] + 0.5*mindist*result[1];
	        } // if (sharp[j])
	    } // for (j = 0; j < n; j++)
	    double wa[][] = new double[3*n][2];
	    for (j = 0; j < n; j++) {
	    	for (i = 0; i < 3; i++) {
	    		wa[i+3*j][0] = neww[i][j][0];
	    		wa[i+3*j][1] = neww[i][j][1];
	    	}
	    } // for (j = 0; j < n; j++)
	    boolean sharp2[][] = new boolean[3][n];
	    for (i = 0; i < n; i++) {
	    	sharp2[1][i] = sharp[i];
	    }
	    boolean sharp3[] = new boolean[3*n];
	    for (j = 0; j < n; j++) {
	    	for (i = 0; i < 3; i++) {
	    		sharp3[i+3*j] = sharp2[i][j];
	    	}
	    }
	    int sharpkeep = 0;
	    for (i = 0; i < 3*n; i++) {
	    	if ((!Double.isNaN(wa[i][0])) && (!Double.isNaN(wa[i][1]))) { 
	    		sharpkeep++;
	    	}
	    } // for (i = 0; i < 3*n; i++)
	    boolean sharp4[] = new boolean[sharpkeep];
	    for (i = 0, j = 0; i < 3*n; i++) {
	        if ((!Double.isNaN(wa[i][0])) && (!Double.isNaN(wa[i][1]))) {
	        	sharp4[j++] = sharp3[i];
	        }
	    }
	    boolean orig[][] = new boolean[3][n];
	    for (i = 0; i < n; i++) {
	    	orig[1][i] = true;
	    }
	    boolean orig2[] = new boolean[3*n];
	    for (j = 0; j < n; j++) {
	    	for (i = 0; i < 3; i++) {
	    		orig2[i+3*j] = orig[i][j];
	    	}
	    } // for (j = 0; j < n; j++)
	    crsplit_orig = new boolean[sharpkeep];
	    for (i = 0, j = 0; i < 3*n; i++) {
	    	if ((!Double.isNaN(wa[i][0])) && (!Double.isNaN(wa[i][1]))) {
	        	crsplit_orig[j++] = orig2[i];
	        }	
	    } // for (i = 0; i < 3*n; i++)
	    double wa2[][] = new double[sharpkeep][2];
	    for (i = 0, j = 0; i < 3*n; i++) {
	    	if ((!Double.isNaN(wa[i][0])) && (!Double.isNaN(wa[i][1]))) {
	    		wa2[j][0] = wa[i][0];
	    		wa2[j++][1] = wa[i][1];
	    	}
	    }
	    beta = scangle(wa2);
	    
	    // Triangulate polygon.  This is done to allow the use of geodesic distance.
	    crtriang(wa2);
	    crcdt(wa2);
	    // Build an adjacency matrix for the vertices in the triangulation
	    int maxEdgeValue = 0;
	    for (i = 0; i < 2; i++) {
	    	for (j = 0; j < crtriang_edge[0].length; j++) {
	    		if (crtriang_edge[i][j] > maxEdgeValue) {
	    			maxEdgeValue = crtriang_edge[i][j];
	    		}
	    	}
	    }
	    boolean V[][] = new boolean[maxEdgeValue+1][maxEdgeValue+1];
	    for (j = 0; j < crtriang_edge[0].length; j++) {
	    	V[crtriang_edge[0][j]][crtriang_edge[1][j]] = true;
	    	V[crtriang_edge[1][j]][crtriang_edge[0][j]] = true;
	    }
	    
	    // Begin iterative phase
	    boolean done = false;
	    while (!done) {
	        done = true;
	        n = wa2.length;
	        neww = new double[3][n][2];
			for (i = 0; i < 3; i++) {
				for (j = 0; j < n; j++) {
					neww[i][j][0] = Double.NaN;
				}
			}
			for (i = 0; i < n; i++) {
				neww[0][i][0] = wa2[i][0];
				neww[0][i][1] = wa2[i][1];
			}
			for (j = 0; j < n; j++) {
			    if (((j == n-1) && (!sharp4[n-1]) && (!sharp4[0])) || ((j != n-1)	&& (!sharp4[j]) && (!sharp4[j+1]))) {
			        // Vertices in sequence, mode n
			    	int jp1 = (j+1)%n;
			    	int jm1 = (j-1+n)%n;
			    	int jp2 = (jp1+1)%n;
			    	
			    	// All points other than j and jp1
			    	boolean mask[] = new boolean[n];
			    	for (i = 0; i < n; i++) {
			    		mask[i] = true;
			    	}
			    	mask[j] = false;
			    	mask[jp1] = false;
			    	// Points adjacent to j in triangulation
			    	boolean mask1[] = new boolean[n];
			    	for (i = 0; i < n; i++) {
			    		mask1[i] = V[i][j] & mask[i];
			    	}
			    	// Exclusion in the case of a slit (would appear as zero distance)
			    	if (Math.abs(beta[j]-1.0) < eps) {
			    		mask[jm1] = false;
			    	}
			    	int nummask1 = 0;
			    	for (i = 0; i < n; i++) {
			    		if (mask1[i]) {
			    			nummask1++;
			    		}
			    	}
			    	double w1[][] = new double[nummask1][2];
			    	for (i = 0, k = 0; i < n; i++) {
			    		if (mask1[i]) {
			    			w1[k][0] = wa2[i][0];
			    			w1[k][1] = wa2[i][1];
			    			k++;
			    		}
			    	}
			    	// Points adjacent to jp1, with slit exclusion
			    	boolean mask2[] = new boolean[n];
			    	for (i = 0; i < n; i++) {
			    		mask2[i] = V[i][jp1] & mask[i];
			    	}
			    	if (Math.abs(beta[jp1]-1) < eps) {
			    		mask2[jp2] = false;
			    	}
			    	int nummask2 = 0;
			    	for (i = 0; i < n; i++) {
			    		if (mask2[i]) {
			    			nummask2++;
			    		}
			    	}
			    	double w2[][] = new double[nummask2][2];
			    	for (i = 0, k = 0; i < n; i++) {
			    		if (mask2[i]) {
			    			w2[k][0] = wa2[i][0];
			    			w2[k][1] = wa2[i][1];
			    			k++;
			    		}
			    	}
			    	// Geodesic distance from adjacent points to segment [w[j], w[jp1]]
			    	double dist1[] = new double[w1.length];
			    	double dist2[] = new double[w2.length];
			    	double segment[][] = new double[2][2];
			    	segment[0][0] = wa2[j][0];
			    	segment[0][1] = wa2[j][1];
			    	segment[1][0] = wa2[jp1][0];
			    	segment[1][1] = wa2[jp1][1];
			    	crpsgd(dist1, dist2, segment, w1, w2);
			    	double dist[] = new double[dist1.length + dist2.length];
			    	double minDist = Double.MAX_VALUE;
			    	for (i = 0; i < dist1.length; i++) {
			    		dist[i] = dist1[i];
			    		if (dist[i] < minDist) {
			    			minDist = dist[i];
			    		}
			    	}
			    	for (i = 0; i < dist2.length; i++) {
			    		dist[i + dist1.length] = dist2[i];
			    		if (dist[i + dist1.length] < minDist) {
			    			minDist = dist[i + dist1.length];
			    		}
			    	}
			    	// If distance is too small, subdivide
			    	if (minDist/zabs(wa2[jp1][0] - wa2[j][0], wa2[jp1][1] - wa2[j][1]) < 1.0/(Math.sqrt(2.0) * 3.0)) {
			    		done = false;
			    		neww[1][j][0] = wa2[j][0] + (1.0/3.0)*(wa2[jp1][0] - wa2[j][0]);
			    		neww[1][j][1] = wa2[j][1] + (1.0/3.0)*(wa2[jp1][1] - wa2[j][1]);
			    		neww[2][j][0] = wa2[j][0] + (2.0/3.0)*(wa2[jp1][0] - wa2[j][0]);
			    		neww[2][j][1] = wa2[j][1] + (2.0/3.0)*(wa2[jp1][1] - wa2[j][1]);
			    	}
			    } //  if (((j == n-1) && (!sharp4[n-1]) && (!sharp4[0])) || ((j != n-1)	&& (!sharp4[j]) && (!sharp4[j+1])))
			} // for (j = 0; j < n; j++)
			
			// Update triangulation
			// First, update edge vertex numbers
			int renum[][] = new int[3][n];
			int numnumber = 0;
			for (j = 0; j < n; j++) {
				for (i = 0; i < 3; i++) {
					if ((!Double.isNaN(neww[i][j][0])) && (!Double.isNaN(neww[i][j][1]))) {
						renum[i][j] = numnumber++;
					}
					else {
						renum[i][j] = -1;
					}
				}
			}
			for (j = 0; j < crtriang_edge[0].length; j++) {
				for (i = 0; i < 2; i++) {
					crtriang_edge[i][j] = renum[0][crtriang_edge[i][j]];
				}
			} // for (j = 0; j < crtriang_edge[0].length; j++)
			// Now update triangles and edges
			int numnew = 0;
			for (i = 0; i < n; i++) {
				if ((!Double.isNaN(neww[1][i][0])) && (!Double.isNaN(neww[1][i][1]))) {
					numnew++;
				}
			}
			int newv[] = new int[numnew];
			for (i = 0, j = 0; i < n; i++) {
				if ((!Double.isNaN(neww[1][i][0])) && (!Double.isNaN(neww[1][i][1]))) {
					newv[j++] = i;
				}	
			}
			int newn = 0;
			for (i = 0; i < 3; i++) {
				for (j = 0; j < n; j++) {
					if ((!Double.isNaN(neww[i][j][0])) && (!Double.isNaN(neww[i][j][1]))) {
						newn++;
					}		
				}
			}
			
			for (i = 0; i < newv.length; i++) {
				j = newv[i];
				// Endpoints of newly split edge
				int idx[] = new int[2];
				idx[0] = renum[0][j];
				idx[1] = renum[0][(j+1)%n];
				// Find split edge
				int e = -1;
				for (k = 0; k < crtriang_edge[0].length && (e == -1); k++) {
				    if ((crtriang_edge[0][k] >= 0) && (crtriang_edge[0][k] == idx[0]) &&
				    	(crtriang_edge[1][k] >= 0) && (crtriang_edge[1][k] == idx[1])) {
				    	e = k;
				    }
				}
				if (e == -1) {
					for (k = 0; k < crtriang_edge[0].length && (e == -1); k++) {
					    if ((crtriang_edge[0][k] >= 0) && (crtriang_edge[0][k] == idx[1]) &&
					    	(crtriang_edge[1][k] >= 0) && (crtriang_edge[1][k] == idx[0])) {
					    	e = k;
					    }
					}	
				}
				// Triangle that edge e was in
				int t = crtriang_edgetri[0][e];
				// Replace e by new edge
				crtriang_edge[0][e] = idx[0];
				crtriang_edge[1][e] = idx[0] + 1;
				// Edges of t
				int te[] = new int[3];
				for (k = 0; k < 3; k++) {
					te[k] = crtriang_triedge[k][t];
				}
				// e's place among t's edges
				int m = -1;
				for (k = 0; (k < 3) & (m == -1); k++) {
					if (te[k] == e) {
						m = k;
					}
				}
				int ntri = crtriang_triedge[0].length;
				int nedge = crtriang_edge[0].length;
				// Find the edge in triangle t that is ccw from e
				int i2 = (m+1)%3;
				int e2 = te[i2];
				if ((crtriang_edge[0][e2] != idx[1]) && (crtriang_edge[1][e2] != idx[1])) {
					i2 = (m+2)%3;
					e2 = te[i2];
				}
				// 3rd vertex of t
				int vtx = -1;
				for (k = 0; (k < 2) && (vtx == -1); k++) {
				    if (crtriang_edge[k][e2] != idx[1]) {
				    	vtx = crtriang_edge[k][e2];
				    }
				}
				// New interior edges
				// Must grow number of columns of crtriang_edge by 4
			    int edge_temp[][] = new int[2][nedge];
			    for (k = 0; k < 2; k++) {
			    	for (m = 0; m < nedge; m++) {
			    		edge_temp[k][m] = crtriang_edge[k][m];
			    	}
			    }
			    crtriang_edge = new int[2][nedge+4];
			    for (k = 0; k < 2; k++) {
			    	for (m = 0; m < nedge; m++) {
			    		crtriang_edge[k][m] = edge_temp[k][m];
			    	}
			    }
				crtriang_edge[0][nedge] = idx[0] + 1;
				crtriang_edge[0][nedge+1] = idx[0] + 2;
				crtriang_edge[1][nedge] = vtx;
				crtriang_edge[1][nedge+1] = vtx;
				// New boundary edges
				crtriang_edge[0][nedge+2] = (idx[0]+1)%newn;
				crtriang_edge[0][nedge+3] = (idx[0]+2)%newn;
				crtriang_edge[1][nedge+2] = (idx[0]+2)%newn;
				crtriang_edge[1][nedge+3] = (idx[0]+3)%newn;
				// 2 new triangles and an old one replaced
				// Must grow number of columns of crtriang_triedge by 2
				int triedge_temp[][] = new int[3][ntri];
			    for (k = 0; k < 3; k++) {
			    	for (m = 0; m < ntri; m++) {
			    		triedge_temp[k][m] = crtriang_triedge[k][m];
			    	}
			    }
			    crtriang_triedge = new int[3][ntri+2];
			    for (k = 0; k < 3; k++) {
			    	for (m = 0; m < ntri; m++) {
			    		crtriang_triedge[k][m] = triedge_temp[k][m];
			    	}
			    }
				crtriang_triedge[0][ntri] = nedge + 2;
				crtriang_triedge[0][ntri+1] = nedge + 1;
				crtriang_triedge[1][ntri] = nedge + 1;
				crtriang_triedge[1][ntri+1] = nedge + 3;
			    crtriang_triedge[2][ntri] = nedge;
			    crtriang_triedge[2][ntri+1] = e2;
			    crtriang_triedge[i2][t] = nedge;
			    // New triangle memberships for new edges and e2
			    // Must grow number of columns of crtriang_edgetri by 4
			    int edgetri_temp[][] = new int[2][nedge];
			    for (k = 0; k < 2; k++) {
			    	for (m = 0; m < nedge; m++) {
			    		edgetri_temp[k][m] = crtriang_edgetri[k][m];
			    	}
			    }
			    crtriang_edgetri = new int[2][nedge+4];
			    for (k = 0; k < 2; k++) {
			    	for (m = 0; m < nedge; m++) {
			    		crtriang_edgetri[k][m] = edgetri_temp[k][m];
			    	}
			    }
			    crtriang_edgetri[0][nedge] = t;
			    crtriang_edgetri[0][nedge+1] = ntri;
			    crtriang_edgetri[1][nedge] = ntri;
			    crtriang_edgetri[1][nedge+1] = ntri+1;
			    crtriang_edgetri[0][nedge+2] = ntri;
			    crtriang_edgetri[0][nedge+3] = ntri+1;
			    crtriang_edgetri[1][nedge+2] = -1;
			    crtriang_edgetri[1][nedge+3] = -1;
			    for (k = 0; (k < 2); k++) {
			        if (crtriang_edgetri[k][e2] == t) {
			        	crtriang_edgetri[k][e2] = ntri+1;
			        }
			    }
			} // for (i = 0; i < newv.length; i++)
			
			wa2 = new double[3*n][2];
			for (j = 0; j < n; j++) {
		    	for (i = 0; i < 3; i++) {
		    		wa2[i+3*j][0] = neww[i][j][0];
		    		wa2[i+3*j][1] = neww[i][j][1];
		    	}
		    } // for (j = 0; j < n; j++)
			boolean sharp5[][] = new boolean[3][n];
			for (i = 0; i < n; i++) {
				sharp5[0][i] = sharp4[i];
			}
			boolean sharp6[] = new boolean[3*n];
			for (j = 0; j < n; j++) {
				for (i = 0; i < 3; i++) {
					sharp6[i+3*j] = sharp5[i][j];
				}
			}
			sharpkeep = 0;
		    for (i = 0; i < 3*n; i++) {
		    	if ((!Double.isNaN(wa2[i][0])) && (!Double.isNaN(wa2[i][1]))) { 
		    		sharpkeep++;
		    	}
		    } // for (i = 0; i < 3*n; i++)
		    sharp4 = new boolean[sharpkeep];
		    for (i = 0, j = 0; i < 3*n; i++) {
		        if ((!Double.isNaN(wa2[i][0])) && (!Double.isNaN(wa2[i][1]))) {
		        	sharp4[j++] = sharp6[i];
		        }
		    }
		    boolean orig4[][] = new boolean[3][n];
		    for (i = 0; i < n; i++) {
		    	orig4[0][i] = crsplit_orig[i];
		    }
		    boolean orig5[] = new boolean[3*n];
		    for (j = 0; j < n; j++) {
				for (i = 0; i < 3; i++) {
					orig5[i+3*j] = orig4[i][j];
				}
			}
		    crsplit_orig = new boolean[sharpkeep];
		    for (i = 0, j = 0; i < 3*n; i++) {
		        if ((!Double.isNaN(wa2[i][0])) && (!Double.isNaN(wa2[i][1]))) {
		        	crsplit_orig[j++] = orig5[i];
		        }
		    }
		    double wa3[][] = new double[sharpkeep][2];
		    for (i = 0, j = 0; i < 3*n; i++) {
		        if ((!Double.isNaN(wa2[i][0])) && (!Double.isNaN(wa2[i][1]))) {
		        	wa3[j][0] = wa2[i][0];
		        	wa3[j++][1] = wa2[i][1];
		        }
		    }
		    wa2 = new double[sharpkeep][2];
		    for (i = 0; i < sharpkeep; i++) {
		    	wa2[i][0] = wa3[i][0];
		    	wa2[i][1] = wa3[i][1];
		    }
		    beta = scangle(wa2);
		    
		    // Recompute CDT for geodesic distance
		    crcdt(wa2);
		    // Build an adjacency matrix for the vertices in the triangulation
		    maxEdgeValue = 0;
		    for (i = 0; i < 2; i++) {
		    	for (j = 0; j < crtriang_edge[0].length; j++) {
		    		if (crtriang_edge[i][j] > maxEdgeValue) {
		    			maxEdgeValue = crtriang_edge[i][j];
		    		}
		    	}
		    }
		    V = new boolean[maxEdgeValue+1][maxEdgeValue+1];
		    for (j = 0; j < crtriang_edge[0].length; j++) {
		    	V[crtriang_edge[0][j]][crtriang_edge[1][j]] = true;
		    	V[crtriang_edge[1][j]][crtriang_edge[0][j]] = true;
		    }
	    } // while (!done)
	    
	    crsplit_neww = new double[wa2.length][2];
	    for (i = 0; i < wa2.length; i++) {
	    	crsplit_neww[i][0] = wa2[i][0];
	    	crsplit_neww[i][1] = wa2[i][1];
	    }
	    
	    return;
	    
	} // crsplit
	
	private void crpsgd(double d1[], double d2[], double[][] segment, double pts1[][], double pts2[][]) {
	    // Geodesic distance from points to a line segment
		// crpsgd computes geodesic distance from two sets of points, pts1 and pts2, to a directed
		// segment.  "Geodesic distance" here means that the segment describes an "inside" (to the left
		// along the directed segment) and an "outside".  For points "inside", the distance is the usual
		// point-segment distance.  For points "outside", the distance for a point in {pts[j]} is the
		// distance to segment[j].  Typically the points in pts{j} are vertices of a polygon adjacent to
		// segment[j] in a constrained Delaunay triangulation, and one needs to know the polygon-interior
		// distance from each vertex to the polygon side segment.
		
		int i, j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		for (i = 0; i < d1.length; i++) {
			d1[i] = Double.POSITIVE_INFINITY;
		}
		for (i = 0; i < d2.length; i++) {
			d2[i] = Double.POSITIVE_INFINITY;
		}
		
		double diffseg[] = new double[2];
		diffseg[0] = segment[1][0] - segment[0][0];
		diffseg[1] = segment[1][1] - segment[0][1];
		double signdiff[] = sign(diffseg[0], diffseg[1]);
		zdiv(1.0, 0.0, signdiff[0], signdiff[1], cr, ci);
		double rot[] = new double[2];
		rot[0] = cr[0];
		rot[1] = ci[0];
		
		// For points to the left of the segment, use normal pt-seg distance.  For others,
		// use distance to the vertex it "belongs" to.
		double pts[][] = new double[pts1.length][2];
		for (i = 0; i < pts1.length; i++) {
			zmlt(pts1[i][0] - segment[0][0], pts1[i][1] - segment[0][1], rot[0], rot[1], cr, ci);
			pts[i][0] = cr[0];
			pts[i][1] = ci[0];
		}
		boolean mask[] = new boolean[pts1.length];
		int summask = 0;
		for (i = 0; i < pts1.length; i++) {
			if (pts[i][1] > eps) {
				mask[i] = true;
				summask++;
			}
		}
		if (summask > 0) {
		    double pts1mask[][] = new double[summask][2];
		    for (i = 0, j = 0; i < pts1.length; i++) {
		    	if (mask[i]) {
		    		pts1mask[j][0] = pts1[i][0];
		    		pts1mask[j][1] = pts1[i][1];
		    		j++;
		    	}
		    } //  for (i = 0, j = 0; i < pts1.length; i++)
		    double d1mask[] = crpsdist(segment, pts1mask);
		    for (i = 0, j = 0; i < pts1.length; i++) {
		    	if (mask[i]) {
		    		d1[i] = d1mask[j++];
		    	}
		    } // for (i = 0, j = 0; i < pts1.length; i++) 
		} // if (summask > 0)
		for (i = 0; i < pts1.length; i++) {
			if (!mask[i]) {
				d1[i] = zabs(pts1[i][0] - segment[0][0], pts1[i][1] - segment[0][1]);
			}
		} // for (i = 0; i < pts1.length; i++)
		
		pts = new double[pts2.length][2];
		for (i = 0; i < pts2.length; i++) {
			zmlt(pts2[i][0] - segment[0][0], pts2[i][1] - segment[0][1], rot[0], rot[1], cr, ci);
			pts[i][0] = cr[0];
			pts[i][1] = ci[0];
		}
		mask = new boolean[pts2.length];
		summask = 0;
		for (i = 0; i < pts2.length; i++) {
			if (pts[i][1] > eps) {
				mask[i] = true;
				summask++;
			}
		}
		if (summask > 0) {
		    double pts2mask[][] = new double[summask][2];
		    for (i = 0, j = 0; i < pts2.length; i++) {
		    	if (mask[i]) {
		    		pts2mask[j][0] = pts2[i][0];
		    		pts2mask[j][1] = pts2[i][1];
		    		j++;
		    	}
		    } //  for (i = 0, j = 0; i < pts2.length; i++)
		    double d2mask[] = crpsdist(segment, pts2mask);
		    for (i = 0, j = 0; i < pts2.length; i++) {
		    	if (mask[i]) {
		    		d2[i] = d2mask[j++];
		    	}
		    } // for (i = 0, j = 0; i < pts2.length; i++) 
		} // if (summask > 0)
		for (i = 0; i < pts2.length; i++) {
			if (!mask[i]) {
				d2[i] = zabs(pts2[i][0] - segment[1][0], pts2[i][1] - segment[1][1]);
			}
		} // for (i = 0; i < pts2.length; i++)
	} // crpsgd
	
	private void crtriang(double w[][]) {
	    // Triangulate a polygon.
		// crtriang triangulates a polygon at the n vertices w.  On exit,k edge is a 2 by (2*n-3)
		// matrix of edge endpoint indices (interior edges in columns 0:n-4), triedge is a 3 by
		// (n-2) matrix of triangle edge indices, and edgetri is a 2 by (2*n-3) matrix of
		// triangle membership indices for the edges.  If edge k is a member of just one triangle
		// (a boundary edge), then edgetri[1][k] = 0.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		
		// Uses a stack-based approach.  The recursive version is simpler but prone to cause
		// memory errors.
		
        int i, j, k, m;
        int enumb;
        double segment[][] = new double[2][2];
        double s[] = new double[2];
        double win[][] = new double[1][2];
        boolean onvtx[][];
        int e2[] = new int[2];
        double cr[] = new double[1];
        double ci[] = new double[1];
        double dw[][] = new double[2][2];
        int vis[];
        
        double W[][] = new double[w.length][2];
        for (i = 0; i < w.length; i++) {
            W[i][0] = w[i][0];
            W[i][1] = w[i][1];
        }
        int N = W.length;
        // Initialize outputs
        crtriang_edge = new int[2][2*N-3];
        crtriang_triedge = new int[3][N-2];
        crtriang_edgetri = new int[2][2*N-3];
        for (i = 0; i < 3; i++) {
        	for (j = 0; j < N-2; j++) {
        		crtriang_triedge[i][j] = -1;
        	}
        }
        for (i = 0; i < 2; i++) {
        	for (j = 0; j < 2*N-3; j++) {
        		crtriang_edgetri[i][j] = -1;
        		crtriang_edge[i][j] = -1;
        	}
        }
        
        // Enter the original boundary edges at the end of edge.
        for (i = 0; i < N; i++) {
        	crtriang_edge[0][N-3+i] = i;
        }
        for (i = 0; i < N-1; i++) {
        	crtriang_edge[1][N-3+i] = i+1;
        }
        crtriang_edge[1][2*N-4] = 0;
        
        // Each row is a (potential) stack entry, consisting of n entries of 0-1 indices.  These
        // indices describe a subdivision of the original polygon.  We preallocate memory to avoid
        // repeated resizing that could bog down.  To avoid hogging too much memory if n is large,
        // we will hope that the number of polygon subdivisions does not exceed 300.
        int maxstack = Math.min(N, 300);
        double stack[][] = new double[maxstack][N];
        for (i = 0; i < maxstack; i++) {
        	for (j = 0; j < N; j++) {
        		stack[i][j] = Double.NaN;
        	}
        }
        
        // Initialize stack
        for (i = 0; i < N; i++) {
        	stack[0][i] = 1;
        }
        int stackptr = 1;
        while (stackptr > 0) {
        	int numidx = 0;
        	for (i = 0; i < N; i++) {
        	    if (stack[stackptr-1][i] > 0) {
        	    	numidx++;
        	    }
        	}
        	int idx[] = new int[numidx];
        	for (i = 0, j = 0; i < N; i++) {
        		if (stack[stackptr-1][i] > 0) {
        		idx[j++] = i;	
        		}
        	} // for (i = 0, j = 0; i < n; i++)
        	stackptr = stackptr - 1;
        	double w2[][] = new double[numidx][2];
        	for (i = 0, j = 0; i < numidx; i++) {
        	    w2[j][0] = W[idx[i]][0];
        	    w2[j++][1] = W[idx[i]][1];
        	}
        	int n = w2.length;
        	if (n == 3) {
        		// Base case: polygon is a triangle
        		
        		// Assign the triangle a new number
        		int tnum = -1;
        		for (j = 0; j < N-2; j++) {
        			if ((crtriang_triedge[0][j] == -1) || (crtriang_triedge[1][j] == -1) || (crtriang_triedge[2][j] == -1)) {
        				tnum = j;
        				break;
        			}
        		} // for (j = 0; j < N-2; j++)
        		int e[][] = new int[2][3];
        		// Edge vertices
        		e[0][0] = idx[0];
        		e[1][0] = idx[1];
        		e[0][1] = idx[1];
        		e[1][1] = idx[2];
        		e[0][2] = idx[2];
        		e[1][2] = idx[0];
        		for (j = 0; j < 3; j++) {
        			if (e[1][j] < e[0][j]) {
        				int tmp = e[0][j];
        				e[0][j] = e[1][j];
        				e[1][j] = tmp;
        			}
        		} // for (j = 0; j < 3; j++)
        		int de[] = new int[3];
        		for (j = 0; j < 3; j++) {
        			de[j] = e[1][j] - e[0][j];
        		}
        		for (j = 0; j < 3; j++) {
        			// Find reference number of triangle edge j
        			if (de[j] == 1) {
        			    // Adjacent vertices
        				enumb = N-3 + e[0][j];
        			}
        			else if (de[j] == N-1) {
        				// Vertices are [0 N-1]
        				enumb = 2*N-4;
        			}
        			else {
        				// Find the edge number among the first N-3
        				enumb = -1;
        				for (i = 0; i < N-3 && (enumb == -1); i++) {
        					if ((crtriang_edge[0][i] == e[0][j]) && (crtriang_edge[1][i] == e[1][j])) {
        						enumb = i;
        					}
        				}
        			} // else
        			// Assign edge # to triangle and triangle # to edge
        			crtriang_triedge[j][tnum] = enumb;
        			if (enumb != -1) {
	        			if (crtriang_edgetri[0][enumb] == -1) {
	        				crtriang_edgetri[0][enumb] = tnum;
	        			}
	        			else if (crtriang_edgetri[1][enumb] == -1) {
	        				crtriang_edgetri[1][enumb] = tnum;
	        			}
        			} // if (enumb != -1)
        		} // for (j = 0; j < 3; j++)
        	} // if (n == 3)
        	else { // n > 3
        		// More than 3 vertices, so add an edge and subdivide
        		
        		// Start with sharpest outward point in polygon
        		double beta[] = scangle(w2);
        		double minbeta = Double.MAX_VALUE;
        		for (i = 0; i < beta.length; i++) {
        			if (beta[i] < minbeta) {
        				minbeta = beta[i];
        				j = i;
        			}
        		} // for (i = 0; i < beta.length; i++)
        		
        		// Trial triangle has vertices at j and its neighbors
        		int jm1 = (j+n-1)%n;
        		int jp1 = (j+1)%n;
        		boolean t[] = new boolean[n];
        		t[jm1] = true;
        		t[j] = true;
        		t[jp1] = true;
        		
        		// Determine which remaining vertices lie in trial triangle
        		double triangle[][] = new double[3][2];
        		triangle[0][0] = w2[jm1][0];
        		triangle[0][1] = w2[jm1][1];
        		triangle[1][0] = w2[j][0];
        		triangle[1][1] = w2[j][1];
        		triangle[2][0] = w2[jp1][0];
        		triangle[2][1] = w2[jp1][1];
        		boolean inside[] = new boolean[n];
        		double w2nott[][] = new double[n-3][2];
        		for (i = 0, m = 0; i < n; i++) {
        		    if (!t[i]) {
        		    	w2nott[m][0] = w2[i][0];
        		    	w2nott[m++][1] = w2[i][1];
        		    }
        		}
        		boolean insidenott[] = new boolean[n-3];
        		onvtx = new boolean[3][n-3];
        		isinpoly(insidenott, onvtx, w2nott, triangle, null, eps);
        		for (i = 0, m = 0; i < n; i++) {
        		    if (!t[i]) {
        		    	inside[i] = insidenott[m++];
        		    }
        		} // for (i = 0, m = 0; i < n; i++)
        		// Borderline cases: on a triangle edge.
        		for (k = 1; k <= 3; k++) {
        			double d[] = new double[n];
        			for (i = 0; i < n; i++) {
        				d[i] = 1.0;
        			}
        		    segment[0][0] = triangle[k%3][0];
        		    segment[0][1] = triangle[k%3][1];
        		    segment[1][0] = triangle[(k+1)%3][0];
        		    segment[1][1] = triangle[(k+1)%3][1];
        		    double dnott[] = crpsdist(segment, w2nott);
        		    for (i = 0, m = 0; i < n; i++) {
        		    	if (!t[i]) {
        		    		d[i] = dnott[m++];
        		    	}
        		    } // for (i = 0, m = 0; i < n; i++)
        		    int nump = 0;
        		    for (i = 0; i < n; i++) {
        		    	if (d[i] < eps) {
        		    		nump++;
        		    	}
        		    } // for (i = 0; i < n; i++)
        		    int p[] = new int[nump];
        		    for (i = 0, m = 0; i < n; i++) {
        		    	if (d[i] < eps) {
        		    	    p[m++] = i;	
        		    	}
        		    } // for (i = 0, m = 0; i < n; i++)
        		    for (i = 0; i < nump; i++) {
        		    	int pi = p[i];
        		    	inside[pi] = false;
        		        // If vertex is coincident with a triangle vertex, not "inside
        		    	if ((zabs(w2[pi][0] - triangle[0][0], w2[pi][1] - triangle[0][1]) > eps) &&
        		    			(zabs(w2[pi][0] - triangle[1][0], w2[pi][1] - triangle[1][1]) > eps) &&
        		    			(zabs(w2[pi][0] - triangle[2][0], w2[pi][1] - triangle[2][1]) > eps)) {
        		    		// Polygon slits/cracks need special care
        		    		// If inward perturbation goes into triangle, it's "inside"
        		    		int windex = (pi-1+n)%n;
        		    		double ang = Math.atan2(w2[windex][1] - w2[pi][1], w2[windex][0] - w2[pi][0]);
        		    		double ang2 = ang - Math.PI*(beta[pi] + 1.0)/2.0;
        		    		s[0] = Math.cos(ang2);
        		    		s[1] = Math.sin(ang2);
        		    		double absw = zabs(w2[pi][0], w2[pi][1]);
        		    		win[0][0] = w2[pi][0] + 1.0E-13*absw*s[0];
        		    		win[0][1] = w2[pi][1] + 1.0E-13*absw*s[1];
        		    		boolean indexout[] = new boolean[1];
        		    		onvtx = new boolean[3][1];
        		    		isinpoly(indexout, onvtx, win, triangle, null, eps);
        		    		if (indexout[0]) {
        		    			inside[pi] = true;
        		    		}
        		    	}
        		    }
        		} // for (k = 1; k <= 3; k++)
        		int numinside = 0;
        		for (i = 0; i < n; i++) {
        			if (inside[i]) {
        				numinside++;
        			}
        		}
        		int inside2[] = new int[numinside];
        		for (i = 0, m = 0; i < n; i++) {
        			if (inside[i]) {
        				inside2[m++] = i;
        			}
        		}
        		if (numinside == 0) {
        			// The trial triangle is OK; use its new edge
        			if (jp1 >= jm1) {
        				e2[0] = jm1;
        				e2[1] = jp1;
        			}
        			else {
        				e2[0] = jp1;
        				e2[1] = jm1;
        			}
        		} // if (numinside == 0)
        		else { // numinside > 0
        			// Edge must be drawn from w2[j] to a vertex inside t
        			
        			if (inside2.length > 1) {
        				// thanks to S. Vavasis for following
        				// Vertices must be "visible" to each other
        				// Find angle between forward side and connecting ray
        				int fwd[] = new int[numinside];
        				for (i = 0; i < numinside; i++) {
        					fwd[i] = (inside2[i]+1)%n;
        				}
        				double ang1[] = new double[numinside];
        				double ang2[] = new double[numinside];
        				for (i = 0; i < numinside; i++) {
        					zdiv(w2[j][0] - w2[inside2[i]][0], w2[j][1] - w2[inside2[i]][1], w2[fwd[i]][0] - w2[inside2[i]][0],
        							w2[fwd[i]][1] - w2[inside2[i]][1], cr, ci);
        					ang1[i] = Math.atan2(ci[0], cr[0])/Math.PI + 2.0;
        					ang1[i] = ang1[i] - 2.0*Math.floor(ang1[i]/2.0);
        					zdiv(w2[inside2[i]][0] - w2[j][0], w2[inside2[i]][1] - w2[j][1], w2[jp1][0] - w2[j][0],
        							w2[jp1][1] - w2[j][1], cr, ci);
        					ang2[i] = Math.atan2(ci[0], cr[0])/Math.PI + 2.0;
        					ang2[i] = ang2[i] - 2.0*Math.floor(ang2[i]/2.0);
        				} // for (i = 0; i < numinside; i++)
        				// Detect visibility by requiring ang to be less than interior angle
        				int numvis = 0;
        				for (i = 0; i < numinside; i++) {
        					if ((ang1[i] < beta[inside2[i]] + 1) && (ang2[i] < beta[j] + 1)) {
        						numvis++;
        					}
        				} // for (i = 0; i < numinside; i++)
        				vis = new int[numvis];
        				for (i = 0, m = 0; i < numinside; i++) {
        					if ((ang1[i] < beta[inside2[i]] + 1) && (ang2[i] < beta[j] + 1)) {
        						vis[m++] = i;
        					}
        				} // for (i = 0, m = 0; i < numinside; i++)
        				
        				// Find a line through w[j] outisde the polygon
        				dw[0][0] = w2[jp1][0] - w2[j][0];
        				dw[0][1] = w2[jp1][1] - w2[j][1];
        				dw[1][0] = w2[j][0] - w2[jm1][0];
        				dw[1][1] = w2[j][1] - w2[jm1][1];
        				zdiv(dw[1][0], dw[1][1], dw[0][0], dw[0][1], cr, ci);
        				double theta = Math.atan2(dw[0][1],  dw[0][0]) + Math.atan2(ci[0], cr[0])/2.0;
        				
        				// Find nearest visible point to that line
        				double wvis[][] = new double[numvis][2];
        				for (i = 0; i < numvis; i++) {
        					wvis[i][0] = w2[inside2[vis[i]]][0] - w2[j][0];
        					wvis[i][1] = w2[inside2[vis[i]]][1] - w2[j][1];
        				}
        				double D[] = new double[numvis];
        				double minD = Double.MAX_VALUE;
        				for (i = 0; i < numvis; i++) {
        					zmlt(wvis[i][0], wvis[i][1], Math.cos(theta), -Math.sin(theta), cr, ci);
        					D[i] = zabs(wvis[i][0] - cr[0]*Math.cos(theta), wvis[i][1] - cr[0]*Math.sin(theta));
        					if (D[i] < minD) {
        						minD= D[i];
        						k = i;
        					}
        				} // for (i = 0; i < numvis; i++)
        			} // if (inside2.length > 1)
        			else {
        				vis = new int[1];
        				vis[0] = 0;
        				k = 0;
        			}
        			if (j >= inside2[vis[k]]) {
        				e2[0] = inside2[vis[k]];
        				e2[1] = j;
        			}
        			else {
        				e2[0] = j;
        				e2[1] = inside2[vis[k]];
        			}
        		} // else numinside > 0
        		// Assign the next available edge number
        		enumb = -1;
        		for (j = 0; j < 2*N-3 && (enumb == -1); j++) {
        			if ((crtriang_edge[0][j] == -1) || (crtriang_edge[1][j] == -1)) {
        				enumb = j;
        			}
        		} // for (j = 0; j < 2*N-3 && (enumb == -1); j++)
        		crtriang_edge[0][enumb] = idx[e2[0]];
        		crtriang_edge[1][enumb] = idx[e2[1]];
        		
        		// Indices of subdivided pieces
        		int i1[] = new int[e2[1]-e2[0]+1];
        		for (i = 0; i < e2[1]-e2[0]+1; i++) {
        			i1[i] = i + e2[0];
        		}
        	    int i2[] = new int[n-e2[1]+e2[0]+1];
        	    for (i = 0; i < n-e2[1]; i++) {
        	    	i2[i] = e2[1] + i;
        	    }
        	    for (i = n-e2[1]; i < n-e2[1]+e2[0]+1; i++) {
        	    	i2[i] = i -(n -e2[1]);
        	    }
        	    
        	    // If stack will overflow, allocate a big chunk of memory
        	    if (stackptr > maxstack - 2) {
        	    	int addlen = Math.min(maxstack, N-maxstack);
        	    	int origLength = stack.length;
        	    	double tempstack[][] = new double[stack.length][N];
        	    	for (i = 0; i < stack.length; i++) {
        	    		for (j = 0; j < N; j++) {
        	    			tempstack[i][j] = stack[i][j];
        	    		}
        	    	}
        	    	stack = new double[origLength + addlen][N];
        	    	for (i = 0; i < origLength; i++) {
        	    		for (j = 0; j < N; j++) {
        	    			stack[i][j] = tempstack[i][j];
        	    		}
        	    	}
        	    	maxstack = maxstack + addlen;
        	    } // if (stackptr > maxstack - 2)
        	    
        	    // Put the two new pieces on the stack
        	    stackptr = stackptr+1;
        	    for (i = 0; i < N; i++) {
        	    	stack[stackptr-1][i] = 0;
        	    }
        	    for (i = 0; i < i1.length; i++) {
        	    	stack[stackptr-1][idx[i1[i]]] = 1;
        	    }
        	    stackptr = stackptr+1;
        	    for (i = 0; i < N; i++) {
        	    	stack[stackptr-1][i] = 0;
        	    }
        	    for (i = 0; i < i2.length; i++) {
        	    	stack[stackptr-1][idx[i2[i]]] = 1;
        	    }
        	} // else n > 3
        } // while (stackptr > 0)
	}

   private double[] crpsdist(double segment[][], double pts[][]) {
	   // Distance from point(s) to a line segment.
	   // crpsdist returns a vector the size of pts indicating the distance from each
	   // entry to the line segment described by seg.
	   
	   // Original MATLAB routine copyright 1997 bt Toby Driscoll.
	   int i;
	   double cr[] = new double[1];
	   double ci[] = new double[1];
	   
	   if ((pts == null) || (pts.length == 0)) {
		   return null;
	   }
	   
	   double d[] = new double[pts.length];
	   for (i = 0; i < pts.length; i++) {
		   d[i] = Double.POSITIVE_INFINITY;
	   }
	   
	   // Rotate to make segment equal [0, xmax].
	   double diff[] = new double[2];
	   diff[0] = segment[1][0] - segment[0][0];
	   diff[1] = segment[1][1] - segment[0][1];
	   double denom[] = sign(diff[0], diff[1]);
	   double pts2[][] = new double[pts.length][2];
	   for (i = 0; i < pts.length; i++) {
		   zdiv(pts[i][0] - segment[0][0], pts[i][1] - segment[0][1], denom[0], denom[1], cr, ci);
		   pts2[i][0] = cr[0];
		   pts2[i][1] = ci[0];
	   }
	   double xmax = zabs(diff[0], diff[1]);
	   
	   // Some points are closest to segment's interior.
	   for (i = 0; i < pts2.length; i++) {
		   if ((pts2[i][0] >= 0) && (pts2[i][0] <= xmax)) {
			   d[i] = Math.abs(pts2[i][1]);
		   }
	   }
	   
	   // The others are closest to an endpoint
	   for (i = 0; i < pts2.length; i++) {
		   if (pts2[i][0] < 0) {
			   d[i] = zabs(pts2[i][0], pts2[i][1]);
		   }
		   else if (pts2[i][0] > xmax) {
			   d[i] = zabs(pts2[i][0] - xmax, pts2[i][1]);
		   }
	   }
	   
	   return d;
   }
   
   private void crcdt(double w[][]) {
	   // Constrained Delaunay triangulation of a polygon.
	   // crcdt computes a constrained Delaunay triangulation of a polygon given any initial
	   // triangulation.  The parameters crtriang_edge, crtriang_triedge, and 
	   // crtriang_edgetri describe the triangulation as in polytri.
	   
	   // Original MATLAB routine copyright 1998 by Toby Driscoll.
	   
	   int i, j;
	   int t1[] = new int[3];
	   int t2[] = new int[3];
	   int e1[] = new int[6];
	   int e2[] = new int[6];
	   int e13[] =  new int[3];
	   int e23[] = new int[3];
	   double edgew[] = new double[2];
	   double diffw[] = new double[2];
	   double cr[] = new double[1];
	   double ci[] = new double[1];
	   double alpha[] = new double[4];
	    // crtriang_edge[0].length = 2*n-3 and crtriang_edgetri[i].length = 2n-3
	   int numedge = crtriang_edge[0].length;
	   boolean interior[] = new boolean[numedge];
	   for (i = 0; i < interior.length; i++) {
		   if (crtriang_edgetri[1][i] != -1) {
			   interior[i] = true;
		   }
	   } // for (i = 0; i < interior.length; i++)
	   // done marks that are known to be correct.
	   boolean done[] = new boolean[numedge];
	   for (i = 0; i < numedge; i++) {
		   done[i] = true;
	   }
	   int numnotdone =  0;
	   // Boundaries are fixed.
	   for (i = 0; i < numedge; i++) {
	       if (interior[i]) {
	    	   done[i] = false;
	    	   numnotdone++;;
	       }
	   }
	   int quadvtx[] = new int[4];
	   
	   while (numnotdone > 0) {
		   int e = -1;
		   for (i= 0; i < numedge && (e == -1); i++) {
			   if (!done[i]) {
			       e = i;   
			   }
		   } // for (i= 0; i < numedge && (e == -1); i++)
		   
		   // Get the two triangles in which edge e participates
		   for (i = 0; i < 3; i++) {
			   t1[i] = crtriang_triedge[i][crtriang_edgetri[0][e]];
			   t2[i] = crtriang_triedge[i][crtriang_edgetri[1][e]];
		   }
		   
		   // Find the quadrilateral of which e is the diagonal 
		   quadvtx[0] = crtriang_edge[0][e];
		   quadvtx[2] = crtriang_edge[1][e];
		   for (i = 0; i < 2; i++) {
			   for (j = 0; j < 3; j++) {
				   e1[i + 2*j] = crtriang_edge[i][t1[j]];
			   }
		   }
		   Arrays.sort(e1);
		   e13[0] = e1[0];
		   e13[1] = e1[2];
		   e13[2] = e1[4];
		   for (i = 0; i < 2; i++) {
			   for (j = 0; j < 3; j++) {
				   e2[i + 2*j] = crtriang_edge[i][t2[j]];
			   }
		   }
		   Arrays.sort(e2);
		   e23[0] = e2[0];
		   e23[1] = e2[2];
		   e23[2] = e2[4];
		   for (i = 0; i < 3; i++) {
			   if ((e13[i] != quadvtx[0]) && (e13[i] != quadvtx[2])) {
				   quadvtx[1] = e13[i];
			   }
			   if ((e23[i] != quadvtx[0]) && (e23[i] != quadvtx[2])) {
				   quadvtx[3] = e23[i];
			   }
		   }
		   
		   // Find the angles where diagonal meets quadrilateral
		   edgew[0] = w[crtriang_edge[1][e]][0] - w[crtriang_edge[0][e]][0];
		   edgew[1] = w[crtriang_edge[1][e]][1] - w[crtriang_edge[0][e]][1];
		   diffw[0] = w[quadvtx[1]][0] - w[quadvtx[0]][0];
		   diffw[1] = w[quadvtx[1]][1] - w[quadvtx[0]][1];
		   zdiv(edgew[0], edgew[1], diffw[0], diffw[1], cr, ci);
		   alpha[0] = Math.atan2(ci[0], cr[0]);
		   diffw[0] = w[quadvtx[3]][0] - w[quadvtx[0]][0];
		   diffw[1] = w[quadvtx[3]][1] - w[quadvtx[0]][1];
		   zdiv(diffw[0], diffw[1], edgew[0], edgew[1], cr, ci);
		   alpha[1] = Math.atan2(ci[0], cr[0]);
		   diffw[0] = w[quadvtx[3]][0] - w[quadvtx[2]][0];
		   diffw[1] = w[quadvtx[3]][1] - w[quadvtx[2]][1];
		   zdiv(-edgew[0], -edgew[1], diffw[0], diffw[1], cr, ci);
		   alpha[2] = Math.atan2(ci[0], cr[0]);
		   diffw[0] = w[quadvtx[2]][0] - w[quadvtx[1]][0];
		   diffw[1] = w[quadvtx[2]][1] - w[quadvtx[1]][1];
		   zdiv(diffw[0], diffw[1], edgew[0], edgew[1], cr, ci);
		   alpha[3] = Math.atan2(ci[0], cr[0]);
		   for (i = 0; i < 4; i++) {
			   alpha[i] = Math.abs(alpha[i]);
		   }
		   
		   // Flip if sum of angles is < PI
		   if ((alpha[0] + alpha[1] + alpha[2] + alpha[3]) < Math.PI) {
			
		       // Change endpts of edge e
			   crtriang_edge[0][e] = quadvtx[1];
			   crtriang_edge[1][e] = quadvtx[3];
			   // Find a quadrilateral side in each triangle
			   int foundt1 =-1;
			   int foundt2 = -1;
			   for (i = 0; i < 3; i++) {
				   if (t1[i] == e) {
					   foundt1 = i;
				   }
				   if (t2[i] == e) {
					   foundt2 = i;
				   }
			   } // for (i = 0; i < 3; i++)
			   int i1 = (foundt1 + 1)%3;
			   int i2 = (foundt2 + 1)%3;
			   // They must be opposites in the quadrilateral (no common endpts)
			   if ((crtriang_edge[0][t1[i1]] == crtriang_edge[0][t2[i2]]) ||
				   (crtriang_edge[0][t1[i1]] == crtriang_edge[1][t2[i2]]) ||
				   (crtriang_edge[1][t1[i1]] == crtriang_edge[0][t2[i2]]) ||
				   (crtriang_edge[1][t1[i1]] == crtriang_edge[1][t2[i2]])) {
				   i2 = (foundt2 + 2)%3;
			   }
			   // Change triangle/edge assignments
			   crtriang_triedge[i1][crtriang_edgetri[0][e]] = t2[i2];
			   int foundi = -1;
			   for (i = 0; i < 2 && (foundi == -1); i++) {
				   if (crtriang_edgetri[i][t2[i2]] == crtriang_edgetri[1][e]) {
					   foundi = i;
				   }
			   } //  for (i = 0; i < 2 && (foundi == -1); i++)
			   crtriang_edgetri[foundi][t2[i2]] = crtriang_edgetri[0][e];
			   crtriang_triedge[i2][crtriang_edgetri[1][e]] = t1[i1];
			   foundi = -1;
			   for (i = 0; i < 2 && (foundi == -1); i++) {
				   if (crtriang_edgetri[i][t1[i1]] == crtriang_edgetri[0][e]) {
					   foundi = i;
				   }
			   } //  for (i = 0; i < 2 && (foundi == -1); i++)
			   crtriang_edgetri[foundi][t1[i1]] = crtriang_edgetri[1][e];
			   
			   // The (non-bdy) edges of the triangles must be reconsidered
			   for (i = 0; i < 3; i++) {
				   done[t1[i]] = !interior[t1[i]];
				   done[t2[i]] = !interior[t2[i]];
			   }
		   } // if ((alpha[0] + alpha[1] + alpha[2] + alpha[3]) < Math.PI)
		   // Edge e is done, unless a neighbor resets it
		   done[e] = true;
		   numnotdone = 0;
		   for (i = 0; i < numedge; i++) {
			   if (!done[i]) {
				   numnotdone++;
			   }
		   }
	   } // while (numnotdone > 0)
	   return;
	   
   }
   
   private void crqgraph(double w[][]) {
       // Quadrilateral graph of a triangulation
	   // crqgraph constructs the "quadrilateral graph" of a polygon triangulation.  crtriang_triedge and
	   // crtriang_edgetri are as in crtriang.  On return, crqgraph_Q is a structure. Q.edge is 2 by (2n-3),
	   // each column being the endpoint indices of an edge of the triangulation.  The n-3 interior edges
	   // (diagonals) come first.  Q.qledge is 4 by (m-3), column k being the indices of the four edges
	   // (in counterclockwise order) of the quadrilateral of which edge k is a diagonal.  Q.qlvert is
	   // 4 by (n-3), with column k being the indices of the four vertices (in clockwise
	   // order) of that quadrilateral.  An endpoint of the diagonal of the quadrilateral will be the first
	   // entry of the column.  Q.adjacent is an (n-3) by (n-3) logical matrix; it indicates which 
	   // quadrilaterals share a common triangle.
	   
	   // Original MATLAB routine copyright 1998 by Toby Driscoll.
	   
	   int e;
	   int t1[] = new int[3];
	   int t2[] = new int[3];
	   int tmp[] = new int[3];
	   int i, j;
	   double we[][][] = new double[2][3][2];
	   double meanwe[][] = new double[3][2];
	   double ang[];
	   double sumang;
	   int t[];
	   double win[][] = new double[4][2];
	   // Get # of quadrilaterals
	   int N = crtriang_edge[0].length;
	   int n3 = (N+3)/2 - 3; // n3 = n - 3
	   
	   // Now construct the graph
	   int qlvert[][] = new int[4][n3];
	   int qledge[][] = new int[4][n3];
	   for (i = 0; i < 4; i++) {
		   for (j = 0; j < n3; j++) {
			   qlvert[i][j] = -1;
			   qledge[i][j] = -1;
		   }
	   }
	   boolean T[][] = new boolean[n3][n3];
	   int e1[] = new int[6];
	   int e2[] = new int[6];
	   int e13[] =  new int[3];
	   int e23[] = new int[3];
	   for (e = 0; e < n3; e++) {
	       // Find the triangles that include edge e
		   for (i = 0; i < 3; i++) {
			   t1[i] = crtriang_triedge[i][crtriang_edgetri[0][e]];
			   t2[i] = crtriang_triedge[i][crtriang_edgetri[1][e]];
		   } // for (i = 0; i < 3; i++)
			   
		   // Re-order triangles so that edge e is first
		   int i1 = -1;
		   for (i = 0; i < 3 && (i1 == -1); i++) {
			   if (t1[i] == e) {
				   i1 = i;
			   }
		   } // for (i = 0; i < 3 && (i1 == -1); i++)
		   for (i = i1; i < 3; i++) {
			   tmp[i-i1] = t1[i];
		   }
		   for (i = 0; i <= i1-1; i++) {
			   tmp[3-i1+i] = t1[i];
		   }
		   for (i = 0; i < 3; i++) {
			   t1[i] = tmp[i];
		   }
		   int i2 = -1;
		   for (i = 0; i < 3 && (i2 == -1); i++) {
			   if (t2[i] == e) {
				   i2 = i;
			   }
		   } // for (i = 0; i < 3 && (i2 == -1); i++)
		   for (i = i2; i < 3; i++) {
			   tmp[i-i2] = t2[i];
		   }
		   for (i = 0; i <= i2-1; i++) {
			   tmp[3-i2+i] = t2[i];
		   }
		   for (i = 0; i < 3; i++) {
			   t2[i] = tmp[i];
		   }
		   
		   // Ensure ccw ordering of edges
		   for (j = 0; j < 3; j++) {
		       for (i = 0; i < 2; i++) {
		    	   we[i][j][0] = w[crtriang_edge[i][t1[j]]][0];
		    	   we[i][j][1] = w[crtriang_edge[i][t1[j]]][1];
		       }
		   }
		   for (j = 0; j < 3; j++) {
			   meanwe[j][0]= (we[0][j][0] + we[1][j][0])/2.0;
			   meanwe[j][1]= (we[0][j][1] + we[1][j][1])/2.0;
		   }
		   ang = scangle(meanwe);
		   sumang = 0.0;
		   for (i = 0; i < 3; i++) {
			   sumang += ang[i];
		   }
		   if (sumang > 0.0) {
			   i = t1[2];
			   t1[2] = t1[1];
			   t1[1] = i;
		   }
		   for (j = 0; j < 3; j++) {
		       for (i = 0; i < 2; i++) {
		    	   we[i][j][0] = w[crtriang_edge[i][t2[j]]][0];
		    	   we[i][j][1] = w[crtriang_edge[i][t2[j]]][1];
		       }
		   }
		   for (j = 0; j < 3; j++) {
			   meanwe[j][0]= (we[0][j][0] + we[1][j][0])/2.0;
			   meanwe[j][1]= (we[0][j][1] + we[1][j][1])/2.0;
		   }
		   ang = scangle(meanwe);
		   sumang = 0.0;
		   for (i = 0; i < 3; i++) {
			   sumang += ang[i];
		   }
		   if (sumang > 0.0) {
			   i = t2[2];
			   t2[2] = t2[1];
			   t2[1] = i;
		   }
		   
		   // Read off quadrilateral edges
		   qledge[0][e] = t1[1];
		   qledge[1][e] = t1[2];
		   qledge[2][e] = t2[1];
		   qledge[3][e] = t2[2];
		   
		   // Any ql edge that is also a diagonal, is adjacent
		   int numt = 0;
		   for (i = 0; i < 4; i++) {
			   if (qledge[i][e] < n3) {
				   numt++;
			   }
		   }
		   t = new int[numt];
		   for (i = 0, j = 0; i < 4; i++) {
			   if (qledge[i][e] < n3) {
				   t[j++] = qledge[i][e];
			   }
		   }
		   for ( i = 0 ; i < numt; i++) {
			   T[t[i]][e] = true;
			   T[e][t[i]] = true;
		   }
		   
		   // Extract vertices
		   // An endpoint of e must be first (and third)
		   qlvert[0][e] = crtriang_edge[0][e];
		   qlvert[2][e] = crtriang_edge[1][e];
		   // Find unique vertices of t1 and t2 to get other ql vertices
		   for (i = 0; i < 2; i++) {
			   for (j = 0; j < 3; j++) {
				   e1[i + 2*j] = crtriang_edge[i][t1[j]];
			   }
		   }
		   Arrays.sort(e1);
		   e13[0] = e1[0];
		   e13[1] = e1[2];
		   e13[2] = e1[4];
		   for (i = 0; i < 2; i++) {
			   for (j = 0; j < 3; j++) {
				   e2[i + 2*j] = crtriang_edge[i][t2[j]];
			   }
		   }
		   Arrays.sort(e2);
		   e23[0] = e2[0];
		   e23[1] = e2[2];
		   e23[2] = e2[4];
		   for (i = 0; i < 3; i++) {
			   if ((e13[i] != qlvert[0][e]) && (e13[i] != qlvert[2][e])) {
				   qlvert[1][e] = e13[i];
			   }
			   if ((e23[i] != qlvert[0][e]) && (e23[i] != qlvert[2][e])) {
				   qlvert[3][e] = e23[i];
			   }
		   } // for (i = 0; i < 3; i++)
		   // Reverse ordering if necessary to clockwise
		   for (i = 0; i < 4; i++) {
			   win[i][0] = w[qlvert[i][e]][0];
			   win[i][1] = w[qlvert[i][e]][1];
		   }
		   ang = scangle(win);
		   sumang = 0.0;
		   for (i = 0; i < 4; i++) {
			   sumang += ang[i];
		   }
		   if (Math.abs(sumang - 2.0) > 1.0E-8) {
			   i= qlvert[3][e];
			   qlvert[3][e] = qlvert[1][e];
			   qlvert[1][e] = i;
		   }
	   } // for (e = 0; e < n3; e++)
	   
	   // Output form
	   crqgraph_Q.edge = crtriang_edge;
	   crqgraph_Q.qledge = qledge;
	   crqgraph_Q.qlvert = qlvert;
	   crqgraph_Q.adjacent = T;
   }
   
   private double[][] crossrat(double w[][], qlgraph Q) {
	   // Crossratios of a triangulated polygon.
	   // crossrat returns the n-3 crossratios of the n polygon vertices w defined by triangulation
	   // as given by the quadrilateral graph in crqgraph_q.
	   
	   // See also crtriang, crct, qlgraph
	   
	   // Original MATLAB routine copyright 1998 by Toby Driscoll
	   
	   int i, j;
	   double cre[] = new double[1];
	   double cim[] = new double[1];
	   double num[] = new double[2];
	   double denom[] = new double[2];
	   int ql[][] = Q.qlvert;
	   double wql[][][] = new double[4][ql[0].length][2];
	   for (i = 0; i < 4; i++) {
		   for (j = 0; j < ql[0].length; j++) {
			   wql[i][j][0] = w[ql[i][j]][0];
			   wql[i][j][1] = w[ql[i][j]][1];
		   }
	   } // for (i = 0; i < 4; i++)
	   double cr[][] = new double[ql[0].length][2];
	   for (i = 0; i < ql[0].length; i++) {
	       zmlt(wql[1][i][0]-wql[0][i][0], wql[1][i][1]- wql[0][i][1], wql[3][i][0]-wql[2][i][0],
	    		   wql[3][i][1]-wql[2][i][1], cre, cim);
	       num[0] = cre[0];
	       num[1] = cim[0];
	       zmlt(wql[2][i][0]-wql[1][i][0], wql[2][i][1]- wql[1][i][1], wql[0][i][0]-wql[3][i][0],
	    		   wql[0][i][1]-wql[3][i][1], cre, cim);
	       denom[0] = cre[0];
	       denom[1] = cim[0];
	       zdiv(num[0], num[1], denom[0], denom[1], cre, cim);
	       cr[i][0] = cre[0];
	       cr[i][1] = cim[0];
	   }
	   return cr;
   }
   
   public void craffine(double w[][], double beta[], double cr[], qlgraph Q, double tol) {
	    // Affine transformations for crossratio formulation.
	   // craffine computers an (n-3) by 2 array.  The cr formulation has n-3 implied "raw"
	   // conformal maps based on the S-C integral, each of which is an affine transformation
	   // away from the true target polygon.  The output array craffine_aff describes these 
	   // transformations.
	   
	   // Because of the overlapping nature of the quadrilaterals in the polygon's 
	   // decomposition, only one side of w need be specified in order to define these
	   // transformations uniquely.  If w is an n-vector of NaN's with at least 2 adjacent
	   // vertices filled in, craffine will also return the full polygon thus defined.  
	   // This is useful for computing maps to a rectangle, for example, where the aspect
	   // ratio -- hence the side lengths -- of the target w are not known in advance.
	   
	   // Original MATLAB routine copyright 1998 by Toby Driscoll.
	   
	   int i, j, k;
	   ComplexLinearEquations ce = new ComplexLinearEquations();
	   double cre[] = new double[1];
	   double cim[] = new double[1];
	   double cre2[] = new double[1];
	   double cim2[] = new double[1];
	   double oldv[][] = new double[4][2];
	   int oldidx[] = new int[4];
	   boolean done[]  = new boolean[4];
	   int n = beta.length;
	   if (Q == null) {
	       crqgraph(w);
	       Q = crqgraph_Q;
	   }
	   
	   int nqpts = (int)Math.max(4,Math.ceil(-Math.log10(tol)));
	   double qdat[][] = new double[nqpts][2*beta.length+2];
	   scqdata(qdat, beta, nqpts);
	   
	   craffine_wn = new double[n][2];
	   for (i = 0; i < n; i++) {
		   craffine_wn[i][0] = Double.NaN;
	   }
	   craffine_aff = new double[n-3][2][2];
	   for (i = 0; i < n-3; i++) {
		   for (j = 0; j < 2; j++) {
			   craffine_aff[i][j][0] = Double.NaN;
		   }
	   }
	   double rawimage[][][] = new double[4][n-3][2];
	   for (i = 0; i < 4; i++) {
		   for (j = 0; j < n-3; j++) {
			   rawimage[i][j][0] = 1.0;
		   }
	   }
	   
	   // Deduce the side we will start with
	   int sidenum = -1;
	   for (i = 0; i < n-1 && (sidenum == -1); i++) {
		   if ((!Double.isNaN(w[i][0])) && (!Double.isNaN(w[i][1])) && (!Double.isNaN(w[i+1][0])) && (!Double.isNaN(w[i+1][1]))) {
			   sidenum = i;
		   }
	   }
	   if (sidenum == -1) {
		   if ((!Double.isNaN(w[n-1][0])) && (!Double.isNaN(w[n-1][1])) && (!Double.isNaN(w[0][0])) && (!Double.isNaN(w[0][1]))) {
			   sidenum = n-1;
		   }   
	   }
	   if (sidenum == -1) {
		   MipavUtil.displayError("You must specify at least one side of the target polygon");
		   System.exit(-1);
	   }
	   int s[] = new int[2];
	   s[0] = sidenum;
	   s[1] = (sidenum+1)%n;
	   double side[][] = new double[2][2];
	   for (i = 0; i < 2; i++) {
		   for (j = 0; j < 2; j++) {
			   side[i][j] = w[s[i]][j];
		   }
	   }
	   
	   // Start by embedding for the quadrilateral containing the boundary edge given by sidenum
	   
	   // Which edge # is it?
	   int edgenum = -1;
	   for (i = 0; i < Q.edge[0].length && (edgenum == -1); i++) {
		   if ((Q.edge[0][i] == s[0]) && (Q.edge[1][i] == s[1])) {
			   edgenum = i;
		   }
	   }
	   for (i = 0; i < Q.edge[0].length && (edgenum == -1); i++) {
		   if ((Q.edge[1][i] == s[0]) && (Q.edge[0][i] == s[1])) {
			   edgenum = i;
		   }
	   }
	   // Which quadrilateral?
	   // Minimum column number which contains quadnum
	   int quadnum = -1;
	   for (j = 0; j < Q.qledge[0].length && (quadnum == -1); j++) {
		   for (i = 0; i < 4 && (quadnum == -1); i++) {
			   if (Q.qledge[i][j] == edgenum) {
				   quadnum = j;
			   }
		   }
	   } // for (j = 0; j < n-3 && (quadnum == -1); j++)
	   // Do the embedding and calculate the affine constants
	   double z[][] = crembed(cr, Q, quadnum);
	   int idx[] = new int[4];
	   for (i = 0; i < 4; i++) {
		   idx[i] = Q.qlvert[i][quadnum];
	   }
	   double z1[][] = new double[4][2];
	   for (i = 0; i < 4; i++) {
		   for (j = 0; j < 2; j++) {
			   z1[i][j] = z[idx[i]][j];
		   }
	   }
	   double v[][] = crquad(z1, idx, z, beta, qdat);
	   for (i = 0; i < 4; i++) {
		   for (j = 0; j < 2; j++) {
			   v[i][j] = -v[i][j];
		   }
	   }
	   for (i = 0; i < 4; i++) {
		   for (j = 0; j < 2; j++) {
			   craffine_wn[idx[i]][j] = v[i][j];
		   }
	   }
	   double wns[][] = new double[2][2];
	   for (i =  0; i < 2; i++) {
		   for (j = 0; j < 2; j++) {
			   wns[i][j] = craffine_wn[s[i]][j];
		   }
	   }
	   // 2 by 2 matrix is:
	   // wns[0]    1
	   // wns[1]    1
	   // det = wns[0] - wns[1]
	   // 2 by 2 matrix  inverse is 
	   // 1/det         -1/det
	   // -wns[1]/det   wns[0]/det
	   double det[] = new double[2];
	   det[0] = wns[0][0] - wns[1][0];
	   det[1] = wns[0][1] - wns[1][1];
	   zdiv(1.0, 0.0, det[0], det[1], cre, cim);
	   double a00[] = new double[2];
	   a00[0] = cre[0];
	   a00[1] = cim[0];
	   double a01[] = new double[2];
	   a01[0] = -cre[0];
	   a01[1] = -cim[0];
	   zdiv(-wns[1][0], -wns[1][1], det[0], det[1], cre, cim);
	   double a10[] = new double[2];
	   a10[0] = cre[0];
	   a10[1] = cim[0];
	   zdiv(wns[0][0], wns[0][1], det[0], det[1], cre, cim);
	   double a11[] = new double[2];
	   a11[0] = cre[0];
	   a11[1] = cim[0];
	   double y[][] = new double[2][2];
	   zmlt(a00[0], a00[1], side[0][0], side[0][1], cre, cim);
	   zmlt(a01[0], a01[1], side[1][0], side[1][1], cre2, cim2);
	   y[0][0] = cre[0] + cre2[0];
	   y[0][1] = cim[0] + cim2[0];
	   zmlt(a10[0], a10[1], side[0][0], side[0][1], cre, cim);
	   zmlt(a11[0], a11[1], side[1][0], side[1][1], cre2, cim2);
	   y[1][0] = cre[0] + cre2[0];
	   y[1][1] = cim[0] + cim2[0];
	   for (i = 0; i < 2; i++) {
		   for (j = 0; j < 2; j++) {
			   craffine_aff[quadnum][i][j] = y[i][j];
		   }
	   } // for (i = 0; i < 2; i++)
	   for (i = 0; i < 4; i++) {
		   zmlt(craffine_wn[idx[i]][0], craffine_wn[idx[i]][1], craffine_aff[quadnum][0][0], craffine_aff[quadnum][0][1], cre, cim);
		   craffine_wn[idx[i]][0] = cre[0] + craffine_aff[quadnum][1][0];
		   craffine_wn[idx[i]][1] = cim[0] + craffine_aff[quadnum][1][1];
	   }
	   
	   // Set up stack and begin
	   
	   // Keep track of "raw" quadrilateral images as they are found
	   for (i = 0; i < 4; i++) {
		   for (j = 0; j < 2; j++) {
			   rawimage[i][quadnum][j] = v[i][j];
		   }
	   } // for (i = 0; i < 4; i++)\
	   int stack[] = new int[n-3];
	   for (i = 0; i < n-3; i++) {
		   stack[i] = -1;
	   }
	   int m = 0;
	   for (i = 0; i < n-3; i++) {
		   if (Q.adjacent[i][quadnum] && (Double.isNaN(craffine_aff[i][0][0]) || Double.isNaN(craffine_aff[i][0][1]))) {
			  m++;
		   }
	   }
	   // Neighbors of quadnum
	   int newnbrs[] = new int[m];
	   for (i = 0, j = 0; i < n-3; i++) {
		   if (Q.adjacent[i][quadnum] && (Double.isNaN(craffine_aff[i][0][0]) || Double.isNaN(craffine_aff[i][0][1]))) {
			   newnbrs[j++] = i;
		   }
	   }
	   // Go on the stack first
	   for (i = 0; i < m; i++) {
		   stack[i] = newnbrs[i];
	   }
	   // Keep track of who put a diagonal on the stack
	   int origin[] = new int[n-3];
	   for (i = 0; i < n-3; i++) {
		   origin[i] = -1;
	   }
	   for (i = 0; i < m; i++) {
		   origin[i] = quadnum;
	   }
	   int stackptr = m-1;
	   while (stackptr >= 0) {
	       int q = stack[stackptr];
	       int oldq = origin[stackptr];
	       stackptr = stackptr-1;
	       
	       // Embedding & raw image
	       for (i = 0; i < 4; i++) {
	    	   idx[i] = Q.qlvert[i][q];
	       }
	       z = crembed(cr, Q, q);
	       for (i = 0; i < 4; i++) {
			   for (j = 0; j < 2; j++) {
				   z1[i][j] = z[idx[i]][j];
			   }
		   }
	       double newv[][] = crquad(z1, idx, z, beta, qdat);
	       for (i = 0; i < 4; i++) {
	    	   for (j = 0; j < 2; j++) {
	    		   newv[i][j] = -newv[i][j];
	    		   rawimage[i][q][j] = newv[i][j];
        	   }
	       }
	       
	       for (i = 0; i < 4; i++) {
	    	   for (j = 0; j < 2; j++) {
	    		   oldv[i][j] = rawimage[i][oldq][j];
	    	   }
	       }
	       for (i = 0; i < 4; i++) {
	    	   oldidx[i] = Q.qlvert[i][oldq];
	       }
	       
	       // Find new transformation by composing with the old one
	       int numfound = 0;
	       for (i = 0; i < 4; i++) {
	    	   for (j = 0; j < 4; j++) {
	    		   if (idx[i] == oldidx[j]) {
	    			   numfound++;
	    		   }
	    	   }
	       }
	       int ref[] = new int[numfound];
	       int oldref[] = new int[numfound];
	       k = 0;
	       for (i = 0; i < 4; i++) {
	    	   for (j = 0; j < 4; j++) {
	    		   if (idx[i] == oldidx[j]) {
	    			   ref[k] = i;
	    			   oldref[k++] = j;
	    		   }
	    	   }
	       } // for (i = 0; i < 4; i++)
	       for (i = 0; i < 4; i++) {
	    	   done[i] = false;
	       }
	       for (i = 0; i < 3; i++) {
	    	   done[ref[i]] = true;
	       }
	       double newvref[][] = new double[3][2];
	       for (i = 0; i < 3; i++) {
	    	   for (j = 0; j < 2; j++) {
	    		   newvref[i][j] = newv[ref[i]][j];
	    	   }
	       }
	       double A[][][] = new double[3][2][2];
	       for (i = 0; i < 3; i++) {
	    	   for (j = 0; j < 2; j++) {
	    		   A[i][0][j] = newvref[i][j];
	    	   }
	       }
	       for (i = 0; i < 3; i++) {
	    	   A[i][1][0] = 1.0;
	    	   A[i][1][1] = 0.0;
	       }
	       double B[][][] = new double[3][1][2];
	       for (i = 0; i < 3; i++) {
	    	   for (j = 0; j < 2; j++) {
	    		   B[i][0][j] = oldv[oldref[i]][j];
	    	   }
	       }
	       int ipiv[] = new int[Math.min(3, 2)];
	       int info[] = new int[1];
	       ce.zgetrf(3, 2, A, 3, ipiv, info);
	       if (info[0] > 0) {
	    	   MipavUtil.displayError("zgetrf generated a singular factor U[" + (info[0]-1) + "][" + (info[0]-1) + "]");
	    	   System.exit(-1);
	       }
	       ce.zgetrs('N', 2, 1, A, 3, ipiv, B, 3, info);
	       zmlt(B[0][0][0], B[0][0][1], craffine_aff[oldq][0][0], craffine_aff[oldq][0][1], cre, cim);
	       craffine_aff[q][0][0] = cre[0];
	       craffine_aff[q][0][1] = cim[0];
	       zmlt(B[1][0][0], B[1][0][1], craffine_aff[oldq][0][0], craffine_aff[oldq][0][1], cre, cim);
	       craffine_aff[q][1][0] = cre[0] + craffine_aff[oldq][1][0];
	       craffine_aff[q][1][1] = cim[0] + craffine_aff[oldq][1][1];
	       for (i = 0; i < 4; i++) {
	    	   if (!done[i]) {
	    		   zmlt(newv[i][0], newv[i][1], craffine_aff[q][0][0], craffine_aff[q][0][1], cre, cim);
	    		   craffine_wn[idx[i]][0] = cre[0] + craffine_aff[q][1][0];
	    		   craffine_wn[idx[i]][1] = cim[0] + craffine_aff[q][1][1];
	    	   }
	       } // for (i = 0; i < 4; i++)
	       
	       m = 0;
		   for (i = 0; i < n-3; i++) {
			   if (Q.adjacent[i][q] && (Double.isNaN(craffine_aff[i][0][0]) || Double.isNaN(craffine_aff[i][0][1]))) {
				  m++;
			   }
		   }
		   // Neighbors of quadnum
		   newnbrs = new int[m];
		   for (i = 0, j = 0; i < n-3; i++) {
			   if (Q.adjacent[i][q] && (Double.isNaN(craffine_aff[i][0][0]) || Double.isNaN(craffine_aff[i][0][1]))) {
				   newnbrs[j++] = i;
			   }
		   }
		   for (i = 0; i < m; i++) {
			   stack[stackptr + i + 1] = newnbrs[i];
			   origin[stackptr + i + 1] = q;
		   }
		   stackptr = stackptr + m;
	   } // while (stackptr >= 0)
   }
   
   private void crfixwc(double w[][], double beta[], double cr[], double aff[][][], qlgraph Q, double wc[]) {
       // Fix conformal center in crossratio formulation.
	   // The conformal center is defined as the image of zero from the disk.  The parameter problem solution
	   // obtained from crparam deliberately leaves this unspecified.  Before one can compute forward or 
	   // inverse maps, then, one must fix the conformal center.
	   
	   // crfixwc returns a vector containing the information that crmap and crinvmap need in order to place
	   // the conformal center at wc.  You must first run craffine to find aff.
	   
	   // If wc is null, you will be prompted for it graphically and it will be put in crfixwc_wc.
	   
	   // Original crfixwc MATLAB code copyright 1998 by Toby Driscoll.
	   
	   int i;
	   int n;
	   int quadnum;
	   double cre[] = new double[1];
	   double cim[] = new double[1];
	   
	   n = cr.length + 3;
	   
	   if ((wc == null) || (wc.length == 0)) {
		   MipavUtil.displayInfo("Click to set conformal center");
	   }
	   
	   // Find a quadrilateral containing wc
	   boolean indexout[] = new boolean[1];
	   boolean onvtx[][] = new boolean[4][1];
	   double wcin[][] = new double[1][2];
	   wcin[0][0] = wc[0];
	   wcin[0][1] = wc[1];
	   double win[][] = new double[4][2];
	   for (quadnum = 0; quadnum < n-3; quadnum++) {
		    for (i = 0; i < 4; i++) {
		    	win[i][0] = w[Q.qlvert[i][quadnum]][0];
		    	win[i][1] = w[Q.qlvert[i][quadnum]][1];
		    }
		    isinpoly(indexout, onvtx, wcin, win, null, 1.0E-4);
		    if (indexout[0]) {
		    	break;
		    }
	   } // for (quadnum = 0; quadnum < n-3; quadnum++)
	   
	   // Invert for that embedding
	   crfixwc_quadnum = quadnum;
	   double z[][] = crembed(cr, Q, quadnum);
	   double subaff[][] = new double[2][2];
	   for (i = 0; i < 2; i++) {
		   subaff[i][0] = aff[quadnum][i][0];
		   subaff[i][1] = aff[quadnum][i][1];
	   }
	   boolean ode = true;
	   boolean newton = true;
	   double tol = 1.0E-8;
	   int maxiter = 10;
	   double zc[][] = crimap0(wcin, z, beta, subaff, null, ode, newton, tol, maxiter);
	   
	   // Find the Moebius transform that goes form the original disk to this embedding.
	   // Always assume that original 1 maps to w[n].
	   zmlt(zc[0][0], -zc[0][1], z[n-1][0], z[n-1][1], cre, cim);
	   zdiv(1.0 - cre[0], -cim[0], z[n-1][0] - zc[0][0], z[n-1][1] - zc[0][1], cre, cim);
	   double a[] = sign(cre[0], cim[0]);
	   crfixwc_mt[0][0] = 1.0;
	   crfixwc_mt[0][1] = 0.0;
	   zmlt(zc[0][0], zc[0][1], a[0], a[1], cre, cim);
	   crfixwc_mt[1][0] = cre[0];
	   crfixwc_mt[1][1] = cim[0];
	   crfixwc_mt[2][0] = zc[0][0];
	   crfixwc_mt[2][1] = -zc[0][1];
	   crfixwc_mt[3][0] = a[0];
	   crfixwc_mt[3][1] = a[1];
   }
   
   private double[][] crimap0(double wp[][], double z[][], double beta[], double aff[][], double qdat[][],
		   boolean ode, boolean newton, double tol, int maxiter) {
	   // Single-embedding inverse map in crossratio formulation.
	   // crimap0 computes the inverse of wp under the map defined by the prevertex embedding z and the
	   // affine transformation aff[0:1].
	   
	   // For more information on the algorithm, see dinvmap.  However, there is no issue with starting
	   // points; the origin is always used as the starting point.
	   
	   // Original crimap0 MATLAB routine copyright 1998 by Toby Driscoll.
	   int i, j, k;
	   int m;
	   int n;
	   int lenwp;
	   double odetol;
	   double scale[][];
	   double cr[] = new double[1];
	   double ci[] = new double[1];
	   double logr;
	   double logi;
	   double realterm;
	   double imagterm;
	   double sum[] = new double[2];
	   double expterm;
	   double realexp;
	   double imagexp;
	   double F[][] = null;
	   double resid;
	   double maxresid;
	   
	   n = beta.length;
	   lenwp = wp.length;
	   double zp[][] = new double[lenwp][2];
	   
	   if (qdat == null) {
		    int nqpts = (int)Math.max(Math.ceil(-Math.log10(tol)), 2);
			qdat = new double[nqpts][2*beta.length+2];
			scqdata(qdat, beta, nqpts);	
	   }
	   
	   // Ignore points with beta == 0 in integration
	   boolean mask[] = new boolean[n];
	   int n2 = 0;
	   for (i = 0; i < n; i++) {
		   if (Math.abs(beta[i]) > eps) {
			   mask[i] = true;
			   n2++;
		   }
	   }
	   double beta2[] = new double[n2];
	   double z2[][] = new double[n2][2];
	   for (i = 0, j = 0; i < n; i++) {
		   if (Math.abs(beta[i]) > eps) {
			   beta2[j] = beta[i];
			   z2[j][0] = z[i][0];
			   z2[j][1] = z[i][1];
			   j++;
		   }
	   }
	   
	   // Use origin of disk as initial guess for inverse images
	   double w0[][] = new double[lenwp][2];
	   for (i = 0; i < lenwp; i++) {
		   w0[i][0] = aff[1][0];
		   w0[i][1] = aff[1][1];
	   }
	   
	   boolean done[] = new boolean[lenwp];
	   
	   if (ode) {
		   // Use relaxed ODE tol if improving with Newton
		   if (newton) {
			   odetol = Math.max(tol,  1.0E-2);
		   }
		   else {
			   odetol = tol;
		   }
		   double abstol = odetol;
		   double reltol = odetol;
		   
		   // Rescale dependent coordinates
		   scale = new double[lenwp][2];
		   for (i = 0; i < lenwp; i++) {
			   scale[i][0] = wp[i][0] - aff[1][0];
			   scale[i][1] = wp[i][1] - aff[1][1];
		   }
		   double z02[] = new double[2*lenwp];
		   double yarr[][] = new double[3][2*lenwp];
			for (i = 0; i < 2*lenwp; i++) {
				yarr[0][i] = 0.0;
			}
			double coef = 0.1;
			double t[] = new double[1];
			double relerr[] = new double[1];
			double abserr[] = new double[1];
			int iflag[] = new int[1];
			ODEModel modODE;
			double tout;
			while (true) {
				t[0] = 0;
				tout = 0.5;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < 2*lenwp; i++) {
				    z02[i] = 0.0;
				}
				modODE = new ODEModel(2*lenwp, z02, t, tout, relerr,
						abserr, iflag, scale, z2, beta2, aff[0]);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < 2*lenwp; i++) {
				yarr[1][i] = z02[i];
			}
			coef = 0.1;
			while (true) {
				t[0] = 0.5;
				tout = 1.0;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < 2*lenwp; i++) {
				    z02[i] = yarr[1][i];	
				}
				modODE = new ODEModel(2*lenwp, z02, t, tout, relerr,
						abserr, iflag, scale, z2, beta2, aff[0]);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < 2*lenwp; i++) {
				yarr[2][i] = z02[i];
			}
			m = yarr.length;
			//int leny = yarr[0].length;
			for (i = 0; i < lenwp ; i++) {
				zp[i][0] = yarr[2][i];
				zp[i][1] = yarr[2][i + lenwp];
			}
			double abszp[] = new double[lenwp];
			for (i = 0; i < lenwp; i++) {
				abszp[i] = zabs(zp[i][0],zp[i][1]);
			}
			for (i = 0; i < lenwp; i++) {
				if (abszp[i] > 1) {
					zp[i][0] = zp[i][0]/abszp[i];
					zp[i][1] = zp[i][1]/abszp[i];
				}
			}
	   } // if (ode)
	   
	   // Newton iterations
	   if (newton) {
		   double zn[][] = new double[lenwp][2];
		   if (ode) {
			   for (i = 0; i < lenwp; i++) {
				   zn[i][0] = zp[i][0];
				   zn[i][1] = zp[i][1];
			   }
		   } // if (ode)
	   
		   // The iteration
		   int numdone = 0;
		   k = 0;
		   while ((numdone < lenwp) && (k < 16)) {
		       int numnotdone = lenwp - numdone; 
		       double wpnotdone[][] = new double[numnotdone][2];
		       double znnotdone[][] = new double[numnotdone][2];
		       for (i = 0, j = 0; (i < lenwp) && (j < numnotdone); i++) {
		    	   if (!done[i]) {
		    		   wpnotdone[j][0] = wp[i][0];
		    		   wpnotdone[j][1] = wp[i][1];
		    		   znnotdone[j][0] = zn[i][0];
		    		   znnotdone[j][1] = zn[i][1];
		    		   j++;
		    	   }
		       } // for (i = 0, j = 0; (i < lenwp) && (j < numnotdone); i++)
		       double res[][] = crmap0(znnotdone, z, beta, aff, qdat);
		       F = new double[numnotdone][2];
		       for (i = 0; i < numnotdone; i++) {
		    	   F[i][0] = wpnotdone[i][0] - res[i][0];
		    	   F[i][1] = wpnotdone[i][1] - res[i][1];
		       }
		       double dF[][] = new double[numnotdone][2];
		       for (j = 0; j < numnotdone; j++) {
		    	   sum[0] = 0.0;
		    	   sum[1] = 0.0;
		           for (i = 0; i < n2; i++) {
		    	       zdiv(znnotdone[j][0], znnotdone[j][1], z2[i][0], z2[i][1], cr, ci);  
		    	       logr = Math.log(zabs(1.0 - cr[0], -ci[0]));
		    	       logi = Math.atan2(-ci[0], 1-cr[0]);
		    	       realterm = beta2[i] * logr;
		    	       imagterm = beta2[i] * logi;
		    	       sum[0] += realterm;
		    	       sum[1] += imagterm;
		    	   }
		           expterm = Math.exp(sum[0]);
		           realexp = expterm * Math.cos(sum[1]);
		           imagexp = expterm * Math.sin(sum[1]);
		           zmlt(aff[0][0], aff[0][1], realexp, imagexp, cr, ci);
		           dF[j][0] = cr[0];
		           dF[j][1] = ci[0];
		       } // for (j = 0; j < numnotdone; j++)
		       for (i = 0, j = 0; (i < lenwp) && (j < numnotdone); i++) {
		    	   if (!done[i])  {
		    		   zdiv(F[j][0], F[j][1], dF[j][0], dF[j][1], cr, ci); 
		    		   zn[i][0] = zn[i][0] + cr[0];
		    		   zn[i][1] = zn[i][1] + ci[0];
		    		   if (zabs(F[j][0], F[j][1]) < tol) {
		    			   done[i] = true;
		    			   numdone++;
		    		   }
		    		   j++;
		    	   }
		       } //  for (i = 0, j = 0; (i < lenwp) && (j < numnotdone); i++)
		       k = k+1;
		   } // while ((numdone < lenwp) && (k < 16))
		   maxresid = 0.0;
		   if ((F != null)) {
			   for (i = 0; i < F.length; i++) {
				   resid = zabs(F[i][0], F[i][1]);
			       if (resid > maxresid) {
			    	   maxresid = resid;
			       }
			   }
		   } // if ((F != null))
		   if (maxresid > tol) {
			   MipavUtil.displayWarning("Warning in crimap0: Solution may be inaccurate");
			   System.out.println("Maximum residual in crimap0 = " + maxresid);
		   }
		   for (i = 0; i < lenwp; i++) {
			   zp[i][0] = zn[i][0];
			   zp[i][1] = zn[i][1];
		   }
	   } // if (newton)
   
	   return zp;
   }
   
   private double[][] crmap0(double zp[][], double z[][], double beta[], double aff[][], double qdat[][]) {
	   // Single-embedding map in crossratio formulation.
	   // crmap0 computes the image of zp under the map defined by the single prevertex embedding
	   // z and the affine transformation aff[0:1]
	   
	   // In keeping with the cr approach, the interation is from the center from the disk.
	   // Results may not be accurate for points near crowded prevertices.  Instead one 
	   // should re-embed.
	   
	   // Original crmap0 MATLAB routine copyright 1998 by Toby Driscoll.
	   int n;
	   double qdat2[][];
	   int np;
	   int i;
	   int j;
	   int numdif;
	   int ir[];
	   int ic[];
	   int k;
	   double cr[] = new double[1];
	   double ci[] = new double[1];
	   
	   // Parse input and initialize
	   n = z.length;
	   if (qdat == null) {
		   qdat2 = new double[8][2*beta.length+2];
		   scqdata(qdat2, beta, 8);	   
	   }
	   else if (qdat.length == 1) {
		    int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 2);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	   
	   }
	   else {
		   qdat2 = qdat;
	   }
	   np = zp.length;
	   double wp[][] = new double[np][2];
	   for (i = 0; i < np; i++) {
		   wp[i][0] = zp[i][0];
		   wp[i][1] = zp[i][1];
	   }
	   
	   // Single out points that are essentially coincident with a prevertex
	   double dif[][] = new double[n][np];
	   numdif = 0;
	   for (i = 0; i < n; i++) {
		   for (j = 0; j < np; j++) {
			   if (zabs(z[i][0] - zp[j][0], z[i][1] - zp[j][1]) < 10.0*eps) {
				   numdif++;
			   }
		   }
       }
	   ir = new int[numdif];
	   ic = new int[numdif];
	   for (i = 0, k = 0; i < n; i++) {
		   for (j = 0; j < np; j++) {
			   if (zabs(z[i][0] - zp[j][0], z[i][1] - zp[j][1]) < 10.0*eps) {
				   ir[k] = i;
				   ic[k] = j;
				   k++;
			   }      
		   }
	   } // for (i = 0, k = 0; i < n; i++)
	   int sing[] = new int[np];
	   for (i = 0; i < np; i++) {
		   sing[i] = -1;
	   }
	   // Assign themn accurate Gauss-Jacobi quadrature
	   for (i = 0; i < numdif; i++) {
		   sing[ic[i]] = ir[i];
	   }
	   
	   double res[][] = crquad(zp, sing, z, beta, qdat2);
	   for (i = 0; i < np; i++) {
		   zmlt(-aff[0][0], -aff[0][1], res[i][0], res[i][1], cr, ci);
		   wp[i][0] = cr[0] + aff[1][0];
		   wp[i][1] = ci[0] + aff[1][1];
	   }
	   
	   return wp;
   }
   
   public scmap extermap(double w[][], double alpha[], double tolerance, double z[][], double c[]) {
	   // Schwarz-Christoffel exterior map object.
	   // expermap constructs a Schwarz-Christoffel exterior map object for the polygon whose
	   // vertices are given by w.  The parameter problem is solved by using default options
	   // for the prevertices and multiplicative constant.  If z is supplied, extermap creates
	   // a extermap object having the given prevertices Z (the multiplicative constant is 
	   // found automatically).  If z and alpha are supplied, extermap creates a map using the
	   // given prevertices and the interior polygonangles described by alpha.  The image polygon
	   // is deduced by computing S-C integrals assuming a multiplicative constant of 1 if c is
	   // not supplied.  Note that not every pairing of prevertices and angles produces a single-valued
	   // map;  you must have SUM((alpha-1)./z) equal to zero.  Also, z is given counterclockwise
	   // around the unit circle, but alpha should be clockwise with respect to the interior of the
	   // polygon
	   
	   // Original MATLAB extermap routine copyright 1998-2001 by Toby Driscoll.
	   int i;
		int nqpts;
		double qdata[][] = null;
		double wn[][] = null;
		double betan[] = null;
		double z0[][] = null;
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		double wflip[][] = null;
		double betaflip[] = null;
		double alphaflip[] = null;
		double cr[] = new double[1];
		double ci[] = new double[1];
		exterRoutine = true;
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, alpha);
		
		if ((z == null) || (z.length == 0)) {
			// Find prevertices
			// Apply scfix to enforce solver rules
			wflip = new double[w.length][2];
			for (i = 0; i < w.length; i++) {
				wflip[i][0] = w[w.length-1-i][0];
				wflip[i][1] = w[w.length-1-i][1];
			}
			betaflip = new double[poly.angle.length];
			for (i = 0; i < poly.angle.length; i++) {
				betaflip[i] = 1.0 - poly.angle[poly.angle.length-1-i];
			}
			wn = new double[w.length][2];
			betan = new double[w.length];
			// Number of vertices added by scfix
			for (i = 0; i < w.length; i++) {
				wn[i][0] = wflip[i][0];
				wn[i][1] = wflip[i][1];
				betan[i] = betaflip[i];
			}
			// No verticesw added
			scfix(wn, betan, null, null, "de", wflip, betaflip, null);
			for (i = 0; i < w.length; i++) {
				x[i] = wn[w.length-1-i][0];
				y[i] = wn[w.length-1-i][1];
			}
			alphaflip = new double[betan.length];
			for (i = 0; i < betan.length; i++) {
				alphaflip[i] = 1.0 - betan[betan.length-1-i];
			}
			poly = new polygon(x, y, alphaflip);
			c = new double[2];
			z = new double[wn.length][2];
			nqpts = Math.max((int)Math.ceil(-Math.log10(tolerance)), 2);
			qdata = new double[nqpts][2*betan.length+2];
			deparam(z, c, qdata, wn, betan, z0, tolerance);
		} // if ((z == null) || (z.length == 0))
		
		if ((qdata == null) || (qdata.length == 0)) {
		    // Base accuracy of quadrature on given options	
			nqpts = (int)Math.ceil(-Math.log10(tolerance));
			betan = new double[poly.angle.length];
			for (i = 0; i < poly.angle.length; i++) {
				betan[i] = 1.0 - poly.angle[poly.angle.length-1-i];
			}
			qdata = new double[nqpts][2*betan.length+2];
			scqdata(qdata, betan, nqpts);
		} // if ((qdata == null) || (qdata.length == 0))
		
		if ((c == null) || (c.length == 0)) {
		    // Find constant
			wn = new double[poly.vertex.length][2];
			for (i = 0; i < poly.vertex.length; i++) {
				wn[i][0] = poly.vertex[poly.vertex.length-1-i][0];
				wn[i][1] = poly.vertex[poly.vertex.length-1-i][1];
			}
			betan = new double[poly.angle.length];
			for (i = 0; i < poly.angle.length; i++) {
				betan[i] = 1.0 - poly.angle[poly.angle.length-1-i];
			}
			zdiv(z[1][0], z[1][1], z[0][1], z[0][0], cr, ci);
			double ang = Math.atan2(ci[0], cr[0])/2.0;
			zmlt(z[0][0], z[0][1], Math.cos(ang), Math.sin(ang), cr, ci);
			double mid[][] = new double[1][2];
			mid[0][0] = cr[0];
			mid[0][1] = ci[0];
			double z0in[][] = new double[1][2];
			z0in[0][0] = z[0][0];
			z0in[0][1] = z[0][1];
			int sing0[] = new int[1];
			sing0[0] = 0;
			double I1[][] = dequad(z0in, mid, sing0, z, betan, qdata);
			double z1in[][] = new double[1][2];
			z1in[0][0] = z[1][0];
			z1in[0][1] = z[1][1];
			int sing1[] = new int[1];
			sing1[0] = 1;
			double I2[][] = dequad(z1in, mid, sing1, z,  betan, qdata);
			double I[] = new double[2];
			I[0] = I1[0][0] - I2[0][0];
			I[1] = I1[0][1] - I2[0][1];
			double diffw[] = new double[2];
			diffw[0] = wn[1][0] - wn[0][0];
			diffw[1] = wn[1][1] - wn[0][1];
			zdiv(diffw[0], diffw[1], I[0], I[1], cr, ci);
			c = new double[2];
			c[0] = cr[0];
			c[1] = ci[0];
		} // if ((c == null) || (c.length == 0))
	    scmap map = new scmap();
	    map.prevertex = z;
		map.constant[0] = c[0];
		map.constant[1]= c[1];
		map.qdata = qdata;
		map.poly = poly;
		
		// Now fill in apparent accuracy
		map.accuracy = exterAccuracy(map);
	    return map;
   }
	
	public scmap diskmap(double w[][], double alpha[], double tolerance, double z[][], double c[]) {
		// Schwarz-Christoffel disk map object
		// diskmap constructs a Schwarz-Christoffel disk map object for the polygon
		// whose vertices are given by w.  The parameter problem is solved using
		// default options for the prevertices and the multiplicative constant.
		// If z is supplied diskmap creates a diskmap object having the given
		// prevertices z (the multiplicative constant is found automatically).
		// There is no checking to ensure that the prevertices are consistent with
		// the given polygon.  diskmap uses the supplied constant c when provided.
		// Based on original MATLAB routine copyright 1998-2001 by Toby Driscoll.
		int i;
		int nqpts;
		double qdata[][] = null;
		double wn[][] = null;
		double betan[] = null;
		double z0[][] = null;
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		double alpha2[] = null;
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, alpha);
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		if ((z == null) || (z.length == 0)) {
			// Solve parameter problem
			// Apply scfix to enforce solver rules
			wn = new double[w.length+2][2];
			betan = new double[w.length+2];
			// Number of vertices added by scfix
			int verticesAdded[] = new int[1];
			int initialVertices = w.length;
			for (i = 0; i < w.length; i++) {
				wn[i][0] = w[i][0];
				wn[i][1] = w[i][1];
				betan[i] = beta[i];
			}
			scfix(wn, betan, verticesAdded, null, "d", w, beta, null);
			double wn2[][];
			double betan2[];
			if ((verticesAdded[0] == 0) ||(verticesAdded[0] == 1)) {
			    wn2 = new double[initialVertices + verticesAdded[0]][2];
				betan2 = new double[initialVertices + verticesAdded[0]];
				for (i = 0; i < initialVertices + verticesAdded[0]; i++) {
					wn2[i][0] = wn[i][0];
					wn2[i][1] = wn[i][1];
					betan2[i] = betan[i];
				}
			}
			else {
				wn2 = wn;
				betan2 = betan;
			}
			double xn[] = new double[wn2.length];
			double yn[] = new double[wn2.length];
			for (i = 0; i < wn2.length; i++) {
				xn[i] = wn2[i][0];
				yn[i] = wn2[i][1];
			}
			alpha2 = new double[betan2.length];
			for (i = 0; i < betan2.length; i++) {
				alpha2[i] = betan2[i] + 1.0;
			}
			poly = new polygon(xn, yn, alpha2);
			c = new double[2];
			z = new double[wn2.length][2];
			nqpts = Math.max((int)Math.ceil(-Math.log10(tolerance)), 4);
			qdata = new double[nqpts][2*betan2.length+2];
			dparam(z, c, qdata, wn2, betan2, z0, tolerance);
		} // if ((z == null) || (z.length == 0))
		
		if ((qdata == null) || (qdata.length == 0)) {
		    // Base accuracy of quadrature on given options	
			nqpts = (int)Math.ceil(-Math.log10(tolerance));
			beta = new double[poly.angle.length];
			for (i = 0; i < poly.angle.length; i++) {
				beta[i] = poly.angle[i] - 1.0;
			}
			qdata = new double[nqpts][2*beta.length+2];
			scqdata(qdata, beta, nqpts);
		} // if ((qdata == null) || (qdata.length == 0))
		
		if ((c == null) || (c.length == 0)) {
		    // Find constant
			w = poly.vertex;
			beta = new double[poly.angle.length];
			for (i = 0; i < poly.angle.length; i++) {
				beta[i] = poly.angle[i] - 1.0;
			}
			boolean found = false;
			int idx = -1;
			for (i = 1; i < z.length && (!found); i++) {
			    if ((!Double.isInfinite(z[i][0])) && (!Double.isInfinite(z[i][1]))) {
			        idx = i+1;
			        found = true;
			    }
			}
			double mid[][] = new double[1][2];
			mid[0][0] = (z[0][0] + z[idx][0])/2.0;
			mid[0][1] = (z[0][1] + z[idx][1])/2.0;
			double z02[][] = new double[1][2];
			z02[0][0] = z[0][0];
			z02[0][1] = z[0][1];
			int sing0[] = new int[1];
			sing0[0] = 0;
			double zidx[][] = new double[1][2];
			zidx[0][0] = z[idx][0];
			zidx[0][1] = z[idx][1];
			int singidx[] = new int[1];
			singidx[0] = idx;
			double I1[][] = dquad(z02, mid, sing0, z, beta, qdata);
			double I2[][] = dquad(zidx, mid, singidx, z, beta, qdata);
		    double I[] = new double[2];
		    I[0] = I1[0][0] - I2[0][0];
		    I[1] = I1[0][1] - I2[0][1];
		    double diffw[] = new double[2];
		    diffw[0] = w[idx][0] - w[0][0];
		    diffw[1] = w[idx][1] - w[0][1];
		    double cr[] = new double[1];
		    double ci[] = new double[1];
		    zdiv(diffw[0], diffw[1], I[0], I[1], cr, ci);
		    c[0] = cr[0];
		    c[1] = ci[0];
		} // if ((c == null) || (c.length == 0))
		scmap map = new scmap();
		map.prevertex = z;
		map.constant[0] = c[0];
		map.constant[1]= c[1];
		map.qdata = qdata;
		map.poly = poly;
		
		// Find conformal center
		beta = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		double zp[][] = new double[1][2];
		double center[][] = dmap(zp, poly.vertex, beta,map.prevertex,map.constant,map.qdata);
		map.center[0] = center[0][0];
		map.center[1] = center[0][1];
		
		// Fill in apparent accuracy
		map.accuracy = diskAccuracy(map);
		return map;
	}
	
	private double exterAccuracy(scmap M) {
		// Apparent accuracy of Schwarz-Christoffel disk exterior map.
		// exterAccuracy estimates the accuracy of the Schwarz-Christoffel exterior
		// map M.  The technique used is to compare the differences between
		// successive finite vertices to the integral between the corresponding
		// prevertices, and return the maximum.
		
		// See also extermap.
		
		// Original MATLAB accuracy routine copyright 1998 by Toby Driscoll.
		
		// If an accuracy has been assigned, don't question it.
		int i, j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double acc;
		if (!Double.isNaN(M.accuracy)) {
			acc =  M.accuracy;
			return acc;
		}
		
		// Get data for low-level funcions
		polygon p = M.poly;
		double w[][] = new double[p.vertex.length][2];
		for (i = 0; i < p.vertex.length; i++) {
			w[i][0] = p.vertex[p.vertex.length - 1 - i][0];
			w[i][1] = p.vertex[p.vertex.length - 1 - i][1];
		}
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = 1.0 - p.angle[p.angle.length - 1 - i];
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		double qdata[][] = M.qdata;
		int n = w.length;
		
		// Test accuracy by integrating between consecutive finite prevertices, and
		// comparing to differences of vertices.
		int numidx = 0;
		for (i = 0; i < n; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				numidx++;
			}
		} // for (i = 0; i < n; i++)
		int idx[] = new int[numidx];
		double wf[][] = new double[numidx][2]; // finite vertices
		for (i = 0, j = 0; i < n; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				idx[j] = i;
				wf[j][0] = w[i][0];
				wf[j++][1] = w[i][1];
			}
		} // for (i = 0, j = 0; i < n; i++)
		
		// Two columns hold endpoint indices for integrations
		int idx2[][] = new int[idx.length][2];
		for (i = 0; i < idx.length; i++) {
			idx2[i][0] = idx[i];
			if (i < idx.length-1) {
				idx2[i][1] = idx[i+1];
			}
			else {
				idx2[i][1] = idx[0];
			}
		} // for (i = 0; i < idx.length; i++)
		
		double mid[][] = new double[numidx][2];
		for (i = 0; i < numidx; i++) {
			zdiv(z[idx2[i][1]][0], z[idx2[i][1]][1], z[idx2[i][0]][0], z[idx2[i][0]][1], cr, ci);
			double ang = Math.atan2(ci[0], cr[0]);
			double dtheta = ang - 2.0*Math.PI*Math.floor(ang/(2.0*Math.PI));
			zmlt(z[idx2[i][0]][0], z[idx2[i][0]][1], Math.cos(dtheta/2.0), Math.sin(dtheta/2.0), cr, ci);
			mid[i][0] = cr[0];
			mid[i][1] = ci[0];
		}
		
		// Do the integrations
		double zin[][] = new double[numidx][2];
		int sing[] = new int[numidx];
		for (i = 0; i < numidx; i++) {
			zin[i][0] = z[idx2[i][0]][0];
			zin[i][1] = z[idx2[i][0]][1];
			sing[i] = idx2[i][0];
		}
		double I1[][] = dequad(zin, mid, sing, z, beta, qdata);
		for (i = 0; i < numidx; i++) {
			zin[i][0] = z[idx2[i][1]][0];
			zin[i][1] = z[idx2[i][1]][1];
			sing[i] = idx2[i][1];
		}
		double I2[][] = dequad(zin, mid, sing, z, beta, qdata);
		double I[][] = new double[numidx][2];
		for (i = 0; i < numidx; i++) {
			I[i][0] = I1[i][0] - I2[i][0];
			I[i][1] = I1[i][1] - I2[i][1];
		}
		double diffwf[][] = new double[numidx][2];
		for (i = 0; i < numidx-1; i++) {
			diffwf[i][0] = wf[i+1][0] - wf[i][0];
			diffwf[i][1] = wf[i+1][1] - wf[i][1];
		}
		diffwf[numidx-1][0] = wf[0][0] - wf[numidx-1][0];
		diffwf[numidx-1][1] = wf[0][1] - wf[numidx-1][1];
		double cI[][] = new double[numidx][2];
		for (i = 0; i < numidx; i++) {
			zmlt(c[0],c[1],I[i][0],I[i][1],cr,ci);
			cI[i][0] = cr[0];
			cI[i][1] = ci[0];
		}
		acc = 0.0;
		for (i = 0; i < numidx; i++) {
			double currentAcc = zabs(cI[i][0] - diffwf[i][0],cI[i][1] - diffwf[i][1]);
			if (currentAcc > acc) {
				acc = currentAcc;
			}
		} // for (i = 0; i < numidx; i++)
		System.out.println("Accuracy = " + acc);
		return acc;
	}
	
	private double diskAccuracy(scmap M) {
	    // Apparent accuracy of Schwarz-Christoffel disk map.
		// diskAccuracy estimates the accuracy of the Schwarz-Christoffel disk
		// map M.  The technique used is to compare the differences between
		// successive finite vertices to the integral between the corresponding
		// prevertices, and return the maximum.
		
		// See also diskmap.
		
		// Original MATLAB accuracy routine copyright 1998 by Toby Driscoll.
		
		// If an accuracy has been assigned, don't question it.
		int i, j;
		double acc;
		if (!Double.isNaN(M.accuracy)) {
			acc =  M.accuracy;
			return acc;
		}
		
		// Get data for low-level funcions
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i]  - 1;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		double qdata[][] = M.qdata;
		
		// Test accuracy by integrating between consecutive finite prevertices, and
		// comparing to differences of vertices.
		int n = w.length;
		int numidx = 0;
		for (i = 0; i < n; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				numidx++;
			}
		} // for (i = 0; i < n; i++)
		int idx[] = new int[numidx];
		double wf[][] = new double[numidx][2]; // finite vertices
		for (i = 0, j = 0; i < n; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				idx[j] = i;
				wf[j][0] = w[i][0];
				wf[j++][1] = w[i][1];
			}
		} // for (i = 0, j = 0; i < n; i++)
		
		// Two columns hold endpoint indices for integrations
		int idx2[][] = new int[idx.length][2];
		for (i = 0; i < idx.length; i++) {
			idx2[i][0] = idx[i];
			if (i < idx.length-1) {
				idx2[i][1] = idx[i+1];
			}
			else {
				idx2[i][1] = idx[0];
			}
		} // for (i = 0; i < idx.length; i++)
		
		double mid[][] = new double[idx.length][2];
		
		// Do the integrations
		double z1[][] = new double[numidx][2];
		double z2[][] = new double[numidx][2];
		int i1[] = new int[numidx];
		int i2[] = new int[numidx];
		for (i = 0; i < numidx; i++) {
			z1[i][0] = z[idx2[i][0]][0];
			z1[i][1] = z[idx2[i][0]][1];
			z2[i][0] = z[idx2[i][1]][0];
			z2[i][1] = z[idx2[i][1]][1];
			i1[i] = idx2[i][0];
			i2[i] = idx2[i][1];
		}
		
		double I1[][] = dquad(z1,mid,i1,z,beta,qdata);
		double I2[][] = dquad(z2,mid,i2,z,beta,qdata);
		double I[][] = new double[numidx][2];
		for (i = 0; i < numidx; i++) {
			I[i][0] = I1[i][0] - I2[i][0];
			I[i][1] = I1[i][1] - I2[i][1];
		}
		
		double diffwf[][] = new double[numidx][2];
		for (i = 0; i < numidx-1; i++) {
			diffwf[i][0] = wf[i+1][0] - wf[i][0];
			diffwf[i][1] = wf[i+1][1] - wf[i][1];
		}
		diffwf[numidx-1][0] = wf[0][0] - wf[numidx-1][0];
		diffwf[numidx-1][1] = wf[0][1] - wf[numidx-1][1];
		double cI[][] = new double[numidx][2];
		double cr[] = new double[1];
		double ci[] = new double[1];
		for (i = 0; i < numidx; i++) {
			zmlt(c[0],c[1],I[i][0],I[i][1],cr,ci);
			cI[i][0] = cr[0];
			cI[i][1] = ci[0];
		}
		acc = 0.0;
		for (i = 0; i < numidx; i++) {
			double currentAcc = zabs(cI[i][0] - diffwf[i][0],cI[i][1] - diffwf[i][1]);
			if (currentAcc > acc) {
				acc = currentAcc;
			}
		} // for (i = 0; i < numidx; i++)
		System.out.println("Accuracy = " + acc);
		return acc;
	}
	
	private scmap center(scmap map, double wc[], double z0[][]) {
		// center (map, wc) computes a map conformally equivalent to map but with
		// the conformal center wc (provided wc is inside the polygon of map),
		// and returns the new map.  If wc is empty, you will be asked to select
		// it graphically.
		
		// See also diskmap.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		
		int i,j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		scmap mapout = new scmap();
		// Set center
		polygon p = map.poly;
		double qdata[][] = map.qdata;
		double qdataout[][] = new double[qdata.length][qdata[0].length];
		for (i = 0; i < qdata.length; i++) {
			for (j = 0; j < qdataout[0].length; j++) {
				qdataout[i][j] = qdata[i][j];
			}
		}
		double z[][] = map.prevertex;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		double x[] = new double[w.length];
		double yar[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			yar[i] = w[i][1];
		}
		double angleout[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			angleout[i] = p.angle[i];
		}
		polygon pout = new polygon(x,yar,angleout);
		
		if ((wc == null) || (wc.length == 0)) {
			
		}
		boolean indexout[] = new boolean[wc.length];
		boolean onvtx[][] = new boolean[w.length][wc.length];
		double wcin[][] = new double[1][2];
		wcin[0][0] = wc[0];
		wcin[0][1] = wc[1];
		isinpoly(indexout, onvtx, wcin, w, beta, eps);
		boolean haveInfinity = false;
		for (i = 0; i < w.length && (!haveInfinity); i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				haveInfinity = true;
			}
		}
		if ((!haveInfinity) && (!indexout[0])) {
			MipavUtil.displayError("Conformal center must be inside polygon");
			return null;
		}
		
		// Find inverse image of wc under current map
		double zc[][] = new double[wcin.length][2];
		int flag[] = null;
		boolean ode = true;
		boolean newton = true;
		double tol = 1.0E-8;
		int maxiter = 10;
		flag = dinvmap(zc, wcin, w, beta, z, map.constant, qdata, z0,
				ode, newton, tol, maxiter);
		
		// Use Moebius transform to reset prevertices
		double y[][] = new double[z.length][2];
		zdiv(1.0 - zc[0][0], zc[0][1], 1.0 - zc[0][0], -zc[0][1], cr, ci);
		double var[] = new double[2];
		var[0] = cr[0];
		var[1] = ci[0];
		double num[] = new double[2];
		for (i = 0; i < z.length; i++) {
			zmlt(var[0], var[1], z[i][0] - zc[0][0], z[i][1] - zc[0][1], cr, ci);
			num[0] = cr[0];
			num[1] = ci[0];
			zmlt(zc[0][0], -zc[0][1], z[i][0], z[i][1], cr, ci);
			zdiv(num[0], num[1], 1 - cr[0], -ci[0], cr, ci);
			y[i][0] = cr[0];
			y[i][1] = ci[0];
		} 
		// Force it to be exact.
		y[y.length-1][0] = 1;
		y[y.length-1][1] = 0;
		for (i = 0; i < y.length-1; i++) {
		    double yabs = zabs(y[i][0], y[i][1]);
		    y[i][0] = y[i][0]/yabs;
		    y[i][1] = y[i][1]/yabs;
		}
		
		// Recalculate constant
		double mid[][] = new double[1][2];
		mid[0][0] = (y[0][0] + y[1][0])/2.0;
		mid[0][1] = (y[0][1] + y[1][1])/2.0;
		double y0[][] = new double[1][2];
		y0[0][0] = y[0][0];
		y0[0][1] = y[0][1];
		int sing0[] = new int[1];
		sing0[0] = 0;
		double y1[][] = new double[1][2];
		y1[0][0] = y[1][0];
		y1[0][1] = y[1][1];
		int sing1[] = new int[1];
		sing1[0] = 1;
		double I1[][] = dquad(y0, mid, sing0, y, beta, qdata);
		double I2[][] = dquad(y1, mid, sing1, y, beta, qdata);
		double I[] = new double[2];
		I[0] = I1[0][0] - I2[0][0];
		I[1] = I1[0][1] - I2[0][1];
		double diffw[] = new double[2];
		diffw[0] = w[1][0] - w[0][0];
		diffw[1] = w[1][1] - w[0][1];
		zdiv(diffw[0], diffw[1], I[0], I[1], cr, ci);
		double c[] = new double[2];
		c[0] = cr[0];
		c[1] = ci[0];
		
		// Assign new values
		mapout.prevertex = y;
		mapout.constant = c;
		mapout.center = wc;
		mapout.poly = pout;
		mapout.qdata = qdataout;
		mapout.accuracy = diskAccuracy(mapout);
		return mapout;
	}
	
	private double[][] rectevalinv(scmap M, double wp[][], double tol[][], double z0[][], int maxiter) {
		// rectevalinv inverts Schwarz-Christoffel rectangle map at points.
		// rectevalinv evaluates the inverse of the Schwarz-Christoffel rectangle map M at the points
		// wp in the polygon.  rectevalinv attempts to give an answer accurate to tol.  If tol is
		// smaller than the accuracy of M, this is unlikely to be met.
		// If z0 is supplied, z0 are the given starting points.  z0 must be the
		// same size as wp or a complex scalar (to be expanded to that size).
		// It is used for the starting approximation to the inverse image of wp.
		// The starting guess need not be close to the correct answer; however, the
		// straight line segment between wp[k] and the forward image of z0[k] must
		// lie entirely inside the polygon, for each k.
		// Original MATLAB evalinv routine copyright 1998 by Toby Driscoll.
		int i;
		double z02[][] = null;
		double qdata[][] = M.qdata;
		double tolerance = M.accuracy;
		
		// Check inputs/supply defaults
		if ((tol != null) && (tol.length > 1)) {
			qdata = tol;
			tolerance = Math.pow(10.0, -qdata.length);
		} // if ((tol != null) && (tol.length > 1)
		
		if ((z0 != null) && (z0.length != 0)) {
		    if (z0.length == 1) {
		    	z02 = new double[wp.length][2];
		    	for (i = 0; i < wp.length; i++) {
		    		z02[i][0] = z0[0][0];
		    		z02[i][1] = z0[0][1];
		    	}
		    } // if (z0.length == 1)
		    else if (z0.length != wp.length) {
		    	MipavUtil.displayError("Argument z0 must be a complex scalar or the same size as wp");
		    	return null;
		    }
		    else {
		    	z02 = z0;
		    }
		} // if ((z0 != null) && (z0.length != 0))
		
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		double L[] = M.stripL;
		double zp[][] =  rinvmap(wp, w, beta, z, c, L, qdata, z02, true, true, tolerance, maxiter);
		return zp;
	}
	
	private double[][] exteriorevalinv(scmap M, double wp[][], double tol[][], double z0[][], int maxiter) {
		// Invert Schwarz-Christoffel exterior map at points.
		// diskevalinv evaluates the inverse of the Schwarz-Christoffel map M at the points
		// wp outside the polygon. diskevalinv attempts to give an answer accurate to tol.  If
		// tol is smaller than the accuracy of M, this is unlikely to be met.
		// If z0 is supplied, z0 are the given starting points.  z0 must be the
		// same size as wp or a complex scalar (to be expanded to that size).
		// It is used for the starting approximation to the inverse image of wp.
		// The starting guess need not be close to the correct answer; however, the
		// straight line segment between wp[k] and the forward image of z0[k] must
		// lie entirely outside the polygon, for each k. 
		// Original MATLAB evalinv routine copyright 1998 by Toby Driscoll.
		int i;
		double qdata[][] = M.qdata;
		double tolerance = M.accuracy;
		double z02[][] = null;
		polygon p = M.poly;
		double vertex[][] = p.vertex;
		double w[][] = new double[vertex.length][2];
		for (i = 0; i < vertex.length; i++) {
			w[i][0] = vertex[vertex.length-1-i][0];
			w[i][1] = vertex[vertex.length-1-i][1];
		}
		double beta[] = new double[p.angle.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = 1 - p.angle[p.angle.length-i-1];
		}
		
		// Check inputs/supply defaults
		if ((tol != null) && (tol.length > 1)) {
			qdata = tol;
			tolerance = Math.pow(10.0, -qdata.length);
		} // if ((tol != null) && (tol.length > 1))
		
		if ((z0 != null) && (z0.length != 0)) {
		    if (z0.length == 1) {
		    	z02 = new double[wp.length][2];
		    	for (i = 0; i < wp.length; i++) {
		    		z02[i][0] = z0[0][0];
		    		z02[i][1] = z0[0][1];
		    	}
		    } // if (z0.length == 1)
		    else if (z0.length != wp.length) {
		    	MipavUtil.displayError("Argument z0 must be a complex scalar or the same size as wp");
		    	return null;
		    }
		    else {
		    	z02 = z0;
		    }
		} // if ((z0 != null) && (z0.length != 0))
		
		double zp[][] = deinvmap(wp, w, beta, M.prevertex, M.constant, qdata, z02, true,  true, tolerance, maxiter);
		return zp;
	}
	
	private int[] diskevalinv(double zp[][], scmap M, double wp[][], double tol[][], double z0[][], int maxiter) {
		// Invert Schwarz-Christoffel disk map at points.
		// diskevalinv evaluates the inverse of the Schwarz-Christoffel map M at the points
		// wp in the polygon. diskevalinv attempts to give an answer accurate to tol.  If
		// tol is smaller than the accuracy of M, this is unlikely to be met.
		// If z0 is supplied, z0 are the given starting points.  z0 must be the
		// same size as wp or a complex scalar (to be expanded to that size).
		// It is used for the starting approximation to the inverse image of wp.
		// The starting guess need not be close to the correct answer; however, the
		// straight line segment between wp[k] and the forward image of z0[k] must
		// lie entirely inside the polygon, for each k. diskevalinv also returns a vector of 
		// indices where the methods was unable to produce a sufficiently small residual.
		// A warning was issued when this occurs.
		// Original MATLAB evalinv routine copyright 1998 by Toby Driscoll.
		int i;
		double qdata[][] = M.qdata;
		double tolerance = M.accuracy;
		double z02[][] = null;
		int flag[] = null;
		polygon p = M.poly;
		double vertex[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		
		// Check inputs/supply defaults
		if ((tol != null) && (tol.length > 1)) {
			qdata = tol;
			tolerance = Math.pow(10.0, -qdata.length);
		} // if ((tol != null) && (tol.length > 1))
		
		if ((z0 != null) && (z0.length != 0)) {
		    if (z0.length == 1) {
		    	z02 = new double[wp.length][2];
		    	for (i = 0; i < wp.length; i++) {
		    		z02[i][0] = z0[0][0];
		    		z02[i][1] = z0[0][1];
		    	}
		    } // if (z0.length == 1)
		    else if (z0.length != wp.length) {
		    	MipavUtil.displayError("Argument z0 must be a complex scalar or the same size as wp");
		    	return null;
		    }
		    else {
		    	z02 = z0;
		    }
		} // if ((z0 != null) && (z0.length != 0))
		
		flag = dinvmap(zp, wp, vertex, beta, M.prevertex, M.constant, qdata, z02, true,  true, tolerance, maxiter);
		return flag;
	}
	
	private double[][] crdiskevalinv(scmap M, double wp[][], double tol) {
		// Invert Schwarz-Christoffel crossratio disk map at points.
		// crdiskevalinv evaluates the inverse of the Schwarz-Christoffel map M
		// at the points wp in the polygon.  The routine attempts to give an
		// answer accurate to tol.  If tol is smaller than the accuracy of M,
		// this is unlikely to be met.
		
		// Original MATLAB evalinv routine copyright 1998 by Toby Driscoll.
		
		int i, j;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		double cr[] = M.crossratio;
		double aff[][][] = M.affine;
		int quadnum = M.center_fix_quadnum;
		double mt[][] = M.center_fix_mt;
		qlgraph Q = M.qgraph;
		double qdata[][] = M.qdata;
		
		double zp[][] = new double[wp.length][2];
		for (i = 0; i < wp.length; i++) {
			zp[i][0] = Double.NaN;
		}
		boolean idx[] = new boolean[wp.length];
		boolean onvtx[][] = new boolean[w.length][wp.length];
		isinpoly(idx, onvtx, wp, w, null, eps);
		int numidx = 0;
		for (i = 0; i < wp.length; i++) {
			if (idx[i]) {
				numidx++;
			}
		}
		double wpin[][] = new double[numidx][2];
		for (i = 0, j = 0; i < wp.length; i++) {
			if (idx[i]) {
				wpin[j][0] = wp[i][0];
				wpin[j++][1] = wp[i][1];
			}
		}
		boolean ode = true;
		boolean newton = true;
		int maxiter = 10;
		double zpout[][] = crinvmap(wpin, w, beta, cr, aff, quadnum, mt, Q, qdata, ode, newton, tol, maxiter);
		for (i = 0, j = 0; i < wp.length; i++) {
			if (idx[i]) {
				zp[i][0] = zpout[j][0];
				zp[i][1] = zpout[j++][1];
			}
		}
		return zp;
}
	
	private double[][] crinvmap(double wp[][], double w[][], double beta[], double cr[], double aff[][][],
			int quadnum, double mt[][], qlgraph Q, double qdat[][], boolean ode, boolean newton, double tol,
			int maxiter) {
		// S-C disk inverse map in crossratio formulation.
		// crinvmap computes the inverse of the disk map with the given conformal center.
		// You must first run crparam and crfixwc.
		
		// Original MATLAB crinvmap routine copyright 1998 by Toby Driscoll.
		
		int i, j;
		int n = w.length;
		int lenwp = wp.length;
		double zp[][] = new double[lenwp][2];
		double qdat2[][] = null;
		int q;
		double cre[] = new double[1];
		double cim[] = new double[1];
		double num[] = new double[2];
		double denom[] = new double[2];
		
		if ((qdat == null) || (qdat.length == 0)) {
			qdat2 = new double[8][2*beta.length+2];
			scqdata(qdat2, beta, 8);
		}
		else if (qdat.length == 1) {
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 2);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else {
			qdat2 = qdat;
		}
		
		// For each embedding, perform inverse maps for appropriate points
		int localQuadnum[] = new int[lenwp]; // keep track of embeddings
		for (i = 0; i < lenwp ;i++) {
		    localQuadnum[i] = -1;	
		}
		double win[][] = new double[4][2];
		double polytol = Math.pow(10.0, -qdat2.length);
		double affin[][] = new double[aff[0].length][2];
		for (q = 0; q < n-3; q++) {
		    int emptyQuadnum = 0;
		    for (i = 0; i < lenwp; i++) {
		    	if (localQuadnum[i] == -1) {
		    		emptyQuadnum++;
		    	}
 		    } // for (i = 0; i < lenwp; i++)
		    int idx[] = new int[emptyQuadnum];
		    for (i = 0, j = 0; i < lenwp; i++) {
		    	if (localQuadnum[i] == -1) {
		    	    idx[j++] = i;	
		    	}
		    } // for (i = 0, j = 0; i < lenwp; i++) 
		    double wpin[][] = new double[emptyQuadnum][2];
		    for (i = 0; i < emptyQuadnum; i++) {
		    	wpin[i][0] = wp[idx[i]][0];
		    	wpin[i][1] = wp[idx[i]][1];
		    }
		    for (i = 0; i < 4; i++) {
		    	win[i][0] = w[Q.qlvert[i][q]][0];
		    	win[i][1] = w[Q.qlvert[i][q]][1];
		    }
		    boolean mask[] = new boolean[emptyQuadnum];
			boolean onvtx[][] = new boolean[4][emptyQuadnum];
			isinpoly(mask, onvtx, wpin, win, null, polytol);
			int nummask = 0;
			for (i = 0; i < emptyQuadnum; i++) {
				if (mask[i]) {
					nummask++;
				}
			}
			if (nummask > 0) {
				int idx2[] = new int[nummask];
				for (i = 0, j = 0; i < emptyQuadnum; i++) {
					if (mask[i]) {
						idx2[j++] = idx[i];
					}
				}
				double z[][] = crembed(cr, Q, q);
				wpin = new double[nummask][2];
				for (i = 0; i < nummask; i++) {
					wpin[i][0] = wp[idx2[i]][0];
					wpin[i][1] = wp[idx2[i]][1];
				}
				for (i = 0; i < aff[0].length; i++) {
					affin[i][0] = aff[q][i][0];
					affin[i][1] = aff[q][i][1];
				}
				double zpout[][] = crimap0(wpin, z, beta, affin, qdat, ode, newton, tol, maxiter);
				for (i = 0; i < nummask; i++) {
				    zp[idx2[i]][0] = zpout[i][0];
				    zp[idx2[i]][1] = zpout[i][1];
				    localQuadnum[idx2[i]] = q;
				}
			} // if (nummask > 0)
			int fullQuadnum = 0;
			for (i = 0; i < lenwp; i++) {
				if (localQuadnum[i] >= 0) {
					fullQuadnum++;
				}
			}
			if (fullQuadnum == lenwp) {
				break;
			}
		} // for (q = 0; q < n-3; q++)
		
		// Convert from local embeddings to global one
		double zr[][][] = null;
		crgather(zp, localQuadnum, quadnum, cr, Q, zr);
		for (i = 0; i < zp.length; i++) {
		    zmlt(-mt[3][0], -mt[3][1], zp[i][0], zp[i][1], cre, cim);
		    num[0] = cre[0] + mt[1][0];
		    num[1] = cim[0] + mt[1][1];
		    zmlt(mt[2][0], mt[2][1], zp[i][0], zp[i][1], cre, cim);
		    denom[0] = cre[0] - mt[0][0];
		    denom[1] = cim[0] - mt[0][1];
		    zdiv(num[0], num[1], denom[0], denom[1], cre, cim);
		    zp[i][0] = cre[0];
		    zp[i][1] = cim[0];
		}
		
		return zp;
	}
	
	private void crgather(double u[][], int uquad[], int quadnum, double cr[], qlgraph Q, double zr[][][]) {
		// Convert points into a single embedding in CR formulation.
		// Each quadrilateral (crossratio) has an associated embedding of the prevertices.
		// These embeddings are linked by Moebius transformations, each of which is well-conditioned.
		// crgather assumes that the points of u are given in the embeddings described by uquad.
		// The Moebius transformations are applied recursively to map the points to their
		// representation in the single embedding quadnum.
		
		// Original MATLAB crgather routine copyright 1998 by Toby Driscoll.
		
		int i, ii, j, k;
		int qa;
		double cre[] = new double[1];
		double cim[] = new double[1];
		double num[] = new double[2];
		double denom[] = new double[2];
		int n3 = cr.length;
		if ((zr == null) || (zr.length == 0)) {
			// Initial call (nonrecursive)
			zr = new double[4][n3][2];
			for (i = 0; i < 4; i++) {
				for (j = 0; j < n3; j++) {
					zr[i][j][0] = Double.NaN;
				}
			}
		} // if ((zr == null) || (zr.length == 0))
		
		// Place the quadrilateral prevertices in a rectangle around the origin.
		int idx[] = new int[4];
		for (i = 0; i < 4; i++) {
			idx[i] = Q.qlvert[i][quadnum];
		}
		double r = cr[quadnum];
		double f1 = Math.sqrt(1.0/(r+1.0));
		double f2 = Math.sqrt(r/(r+1.0));
		zr[0][quadnum][0] = -f1;
		zr[0][quadnum][1] = -f2;
		zr[1][quadnum][0] = -f1;
		zr[1][quadnum][1] = f2;
		zr[2][quadnum][0] = f1;
		zr[2][quadnum][1] = f2;
		zr[3][quadnum][0] = f1;
		zr[3][quadnum][1] = -f2;
		
		// Recurse on neighbors to map into embedding quadnum
		boolean nbr[] = new boolean[n3];
		int numnbr = 0;
		for (i = 0; i < n3; i++) {
			nbr[i] = Q.adjacent[quadnum][i] && Double.isNaN(zr[0][i][0]);
			if (nbr[i]) {
				numnbr++;
			}
		}
		int q[] = new int[numnbr];
		for (i = 0, j = 0; i < n3; i++) {
			nbr[i] = Q.adjacent[quadnum][i] && Double.isNaN(zr[0][i][0]);
			if (nbr[i]) {
				q[j++] = i;
			}
		}
		int i1[] = new int[3];
		int i2[] = new int[3]; 
		double z[][] = new double[3][2];
		double w[][] = new double[3][2];
		boolean mask[] = new boolean[uquad.length];
		for (i = 0; i < q.length; i++) {
		    qa = q[i];
		    // First map into embedding qa
		    crgather(u, uquad, qa, cr, Q, zr);
		    // Find the 3 points in common with qa
		    int numcommon = 0;
		    for (ii = 0; ii < 4; ii++) {
		    	for (j = 0; j < 4; j++) {
		    		if (Q.qlvert[ii][quadnum] == Q.qlvert[j][qa]) {
		    			numcommon++;
		    		}
		    	}
		    }
		    if (numcommon != 3) {
		    	MipavUtil.displayError("Number of points in common with qa = " + qa + " was " + numcommon + " instead of the required 3");
		    	System.exit(-1);
		    }
		    k = 0;
		    for (ii = 0; ii < 4; ii++) {
		    	for (j = 0; j < 4; j++) {
		    		if (Q.qlvert[ii][quadnum] == Q.qlvert[j][qa]) {
		    			i1[k] = ii;
		    			i2[k++] = j;
		    		}
		    	}
		    }
		    // Map from qa to quadnum
		    for (ii = 0; ii < 3; ii++) {
		    	for (j = 0; j < 2; j++) {
		    	    z[ii][j] = zr[i2[ii]][qa][j];
		    	    w[ii][j] = zr[i1[ii]][quadnum][j];
		    	}
		    }
		    double mt[][] = moebius(z, w);
		    int nummask = 0;
		    for (ii = 0; ii < uquad.length; ii++) {
		    	mask[ii] = (uquad[ii] == qa);
		    	if (mask[ii]) {
		    		nummask++;
		    	}
		    }
		    if (nummask > 0) {
			    for (ii = 0; ii < mask.length; ii++) {
			    	if (mask[ii]) {
			    		zmlt(mt[1][0], mt[1][1], u[ii][0], u[ii][1], cre, cim);
			    		num[0] = cre[0] + mt[0][0];
			    		num[1] = cim[0] + mt[0][1];
			    		zmlt(mt[3][0], mt[3][1], u[ii][0], u[ii][1], cre, cim);
			    		denom[0] = cre[0] + mt[2][0];
			    		denom[1] = cim[0] + mt[2][1];
			    		zdiv(num[0], num[1], denom[0], denom[1], cre, cim);
			    		u[ii][0] = cre[0];
			    		u[ii][1] = cim[0];
			    		uquad[ii] = quadnum;
			    	} // if (mask[ii])
			    } // for (ii = 0; ii < mask.length; ii++)
		    } // if (nummask > 0)
		} // for (i = 0; i < q.length; i++)
	}
	
	private double[][] rinvmap(double wp[][], double w[][], double beta[], double z[][],
			double c[], double L[], double qdat[][], double z0[][], boolean ode, boolean newton,
			double tol, int maxiter) {
		// rinvmap computes the inverse of the Schwarz-Christoffel rectangle map (i.e., from the
		// polygon to the rectangle) at the points given in the vector wp.  The other arguments
		// are as in rparam. 
		
		// The default algorithm is to solve an ODE in order to obtain a fair
		// approximation for zp, and then improve zp with Newton iterations.
		// The 2 other possible algorithms are:
		// 1.) Use ODE only.
		// 2.) Use Newton only; take z0 as an initial guess.
		// The ODE solution at wp requires a vector z0 whose forward image w0
		// is such that for each j, the line segment connecting wp[j] and w0[j]
		// lies inside the polygon.  By default z0 is chosen by a fairly robust
		// automatic process.  Using the parameters ode and newton, you can choose
		// to use either an ODE solution or Newton solution exclusively.
		
		// rinvmap has two interpretations.  If the ODE solution is being used,
		// z0 overrides the automatic selection of initial points.  (This can
		// be handy in convex polygons, where the choice of z0 is trivial.)
		// Otherwise, z0 is taken as an initial guess to zp.  In either case,
		// if length(z0) == 1, the value z0 is used for all elements of wp;
		// otherwise, length(z0) should equal length(wp).
		
		// tol, error tolerance for solution (default 1e-8)
	    // maxiter, maximum number of Newton iterations (default 10)
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		
		int i, j, k, m;
		double qdat2[][] = null;
		double w0[][] = null;
		double zn[][] = null;
		double F[][] = null;
		double dF[][] = null;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int n = w.length;
		int corners[] = new int[4];
		int renum[] = new int[n];
		rcorners(w, beta, z, corners, renum);
		double rect[] = new double[4];
		double minreal = Double.MAX_VALUE;
		double maxreal = -Double.MAX_VALUE;
		double minimag = Double.MAX_VALUE;
		double maximag = -Double.MAX_VALUE;
		for (i = 0; i < 4; i++) {
			if (z[corners[i]][0] < minreal) {
			    minreal = z[corners[i]][0];	
			}
			if (z[corners[i]][0] > maxreal) {
				maxreal = z[corners[i]][0];
			}
			if (z[corners[i]][1] < minimag) {
			    minimag = z[corners[i]][1];	
			}
			if (z[corners[i]][1] > maximag) {
				maximag = z[corners[i]][1];
			}
		}
		rect[0] = minreal;
		rect[1] = maxreal;
		rect[2] = minimag;
		rect[3] = maximag;
		double K = -Double.MAX_VALUE;
		double Kp = -Double.MAX_VALUE;
		for (i = 0; i < z.length; i++) {
			if (z[i][0] > K) {
				K = z[i][0];
			}
			if (z[i][1] > Kp) {
				Kp = z[i][1];
			}
		} // for (i = 0; i < z.length; i++)
		
		double zs[][] = new double[z.length][2];
		double zsprime[][] = new double[z.length][2];
		r2strip(zs, zsprime, z, z, L[0]);
		for (i = 0; i < zs.length; i++) {
			// Put them *exactly* on edges
			zs[i][1] = Math.round(zs[i][1]);
		}
		
		double zp[][] = new double[wp.length][2];
		int lenwp = wp.length;
		
		if ((qdat == null) || (qdat.length == 0)) {
			qdat2 = new double[1][1];
			qdat2[0][0] = tol;
		}
		else if (qdat.length == 1) {
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 2);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else {
			qdat2 = qdat;
		}
		
		boolean done[] = new boolean[wp.length];
		// First, trap all points indistinguishable from vertices, or they will cause
		// trouble.
		// Modified 05/14/2007 to work around bug in matlab 2007a.
		for (j = 0; j < n; j++) {
			int numidx = 0;
			for (i = 0; i < lenwp; i++) {
				if (zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
					numidx++;
				}
			} // for (i = 0; i < lenwp; i++)
			int idx[] = new int[numidx];
			for (i = 0, k = 0; i < lenwp; i++) {
				if (zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
					idx[k++] = i;
				}
			} // for (i = 0, k = 0; i < lenwp; i++)
			for (i = 0; i < numidx; i++) {
				zp[idx[i]][0] = z[j][0];
				zp[idx[i]][1] = z[j][1];
				done[idx[i]] = true;
			} // for (i = 0; i < numidx; i++)
		} // for (j = 0; j < n; j++)
		int sumdone = 0;
		for (i = 0; i < done.length; i++) {
			if (done[i]) {
				sumdone++;
			}
		} // for (i = 0; i < done.length; i++)
		// lenwp contains number not done
		lenwp = lenwp - sumdone;
		if (lenwp == 0) {
			return null;
		}
		
		// ODE
		if (ode) {
			double z02[][];
			double w02[][];
			double z03[][];
			double w03[][];
			double wpnotdone[][] = new double[lenwp][2];
			double zpnotdone[][] = new double[lenwp][2];
			for (i = 0, j = 0; i < done.length; i++) {
				if (!done[i]) {
					wpnotdone[j][0] = wp[i][0];
					wpnotdone[j++][1] = wp[i][1];
				}
			} // for (i = 0, j = 0; i < done.length; i++)
			if ((z0 == null) || (z0.length == 0)) {
			  // Pick a value z0 (not a singularity) and compute the map there.	
				z03 = new double[lenwp][2];
				w03 = new double[lenwp][2];
				double Lenclose[][] = new double[1][L.length];
				for (i = 0; i < L.length; i++) {
					Lenclose[0][i] = L[i];
				}
				scimapz0(z03, w03, "r", wpnotdone, w, beta, z, c, Lenclose, qdat2);
			} // if ((z0 == null) || (z0.length == 0)) 
			else {
				w0 = new double[z0.length][2];
			    rmap(w0,z0,w,beta,z,c,L,qdat2);
			    if ((z0.length == 1) && (lenwp > 1)) {
			        z02 = new double[lenwp][2];
			        w02 = new double[lenwp][2];
			        for (i = 0; i < lenwp; i++) {
			        	z02[i][0] = z0[0][0];
			        	z02[i][1] = z0[0][1];
			        	w02[i][0] = w0[0][0];
			        	w02[i][1] = w0[0][1];
			        }
			    } // if ((z0.length == 1) && (lenwp > 1)
			    else {
			    	w02 = w0;
			    	z02 = z0;
			    }
			    w03 = new double[lenwp][2];
			    z03 = new double[lenwp][2];
			    for (i = 0, j = 0; i < done.length; i++) {
			    	if (!done[i]) {
			    		w03[j][0] = w02[i][0];
			    		w03[j][1] = w02[i][1];
			    		z03[j][0] = z02[i][0];
			    		z03[j++][1] = z02[i][1];
			    	}
			    } // for (i = 0, j = 0; i < done.length; i++)
			} // else
			
			// Use relaxed ODE tol if improving with Newton.
			double odetol;
			if (newton) {
			    odetol = Math.max(tol,  1.0E-3);
			}
			else {
				odetol = tol;
			}
			double abstol = odetol;
			double reltol = odetol;
			
			// Rescale dependent coordinate
			double scale[][] = new double[lenwp][2];
			for (i = 0; i < lenwp; i++) {
				scale[i][0] = wpnotdone[i][0] - w03[i][0];
				scale[i][1] = wpnotdone[i][1] - w03[i][1];
			}
			
			// Solve ODE
			double z04[] = new double[2*z03.length];
			for (i = 0; i < z03.length; i++) {
				z04[i] = z03[i][0];
				z04[i + z03.length] = z03[i][1];
			}
			//double tspan[] = new double[3];
			//tspan[0] = 0;
			//tspan[1] = 0.5;
			//tspan[2] = 1.0;
			//double t[] = new double[tspan.length];
			//double y[][] = new double[tspan.length][z04.length];
			//rimapfun(t, y, tspan, z04, abstol, reltol,scale, z, beta, c, zs, L);
			// Because ode23.m is copyrighted by MATLAB use a port of the
			// original FORTRAN ode.f code 
			double yarr[][] = new double[3][z04.length];
			for (i = 0; i < z04.length; i++) {
				yarr[0][i] = z04[i];
			}
			double coef = 0.1;
			double t[] = new double[1];
			double relerr[] = new double[1];
			double abserr[] = new double[1];
			int iflag[] = new int[1];
			ODERectModel modODE;
			double tout;
			while (true) {
				t[0] = 0;
				tout = 0.5;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z04.length; i++) {
				    z04[i] = yarr[0][i];
				}
				modODE = new ODERectModel(z04.length, z04, t, tout, relerr,
						abserr, iflag, scale, z, beta, c, zs, L);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < z04.length; i++) {
				yarr[1][i] = z04[i];
			}
			coef = 0.1;
			while (true) {
				t[0] = 0.5;
				tout = 1.0;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z04.length; i++) {
				    z04[i] = yarr[1][i];	
				}
				modODE = new ODERectModel(z04.length, z04, t, tout, relerr,
						abserr, iflag, scale, z, beta, c, zs, L);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < z04.length; i++) {
				yarr[2][i] = z04[i];
			}
			m = yarr.length;
			//int leny = yarr[0].length;
			for (i = 0, j = 0; i < done.length; i++) {
				if (!done[i]) {
				    zpnotdone[j][0] = yarr[m-1][j];
					zpnotdone[j][1] = yarr[m-1][j + lenwp];
					j++;
				}
			} // for (i = 0, j = 0; i < done.length; i++)
			zpnotdone = rectproject(zpnotdone, rect);
			for (i = 0, j = 0; i < done.length; i++) {
				if (!done[i]) {
					zp[i][0] = zpnotdone[j][0];
					zp[i][1] = zpnotdone[j][1];
					j++;
				}
			} // for (i = 0, j = 0; i < done.length; i++)
		} // if (ode)
		
		// Newton iterations
		if (newton) {
		    if (!ode) {
		        if ((z0.length == 1) & (lenwp > 1)) {
		        	zn = new double[lenwp][2];
		        	for (i = 0; i < lenwp; i++) {
		        		zn[i][0] = z0[0][0];
		        		zn[i][1] = z0[0][1];
		        	}
		        } // if ((z0.length == 1) & (lenwp > 1))
		        else {
		        	zn = new double[z0.length][2];
		        	for (i = 0; i < z0.length; i++) {
		        		zn[i][0] = z0[i][0];
		        		zn[i][1] = z0[i][1];
		        	}
		        } // else
		        for (i = 0; i < done.length; i++) {
		        	if (done[i]) {
		        		zn[i][0] = zp[i][0];
		        		zn[i][1] = zp[i][1];
		        	}
		        }
		    } // if (!ode)
		    else { // ode
		    	zn = new double[zp.length][2];
		    	for (i = 0; i < zp.length; i++) {
		    		zn[i][0] = zp[i][0];
		    		zn[i][1] = zp[i][1];
		    	}
		    } // else ode
		    
		    k = 0;
		    while ((sumdone < done.length) && (k < maxiter)) {
		        m = done.length - sumdone;
		        double znnotdone[][] = new double[m][2];
		        double wpnotdone[][] = new double[m][2];
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		znnotdone[j][0] = zn[i][0];
		        		znnotdone[j][1] = zn[i][1];
		        		wpnotdone[j][0] = wp[i][0];
		        		wpnotdone[j][1] = wp[i][1];
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        w0 = new double[znnotdone.length][2];
		        rmap(w0, znnotdone, w, beta, z, c, L, qdat2);
		        F = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	F[i][0] = wpnotdone[i][0] - w0[i][0];
		        	F[i][1] = wpnotdone[i][1] - w0[i][1];
		        }
		        dF = rderiv(znnotdone, z, beta, c, L, zs);
		        for (i = 0; i < m; i++) {
		        	zdiv(F[i][0], F[i][1], dF[i][0], dF[i][1], cr, ci);
		        	znnotdone[i][0] = znnotdone[i][0] + cr[0];
		        	znnotdone[i][1] = znnotdone[i][1] + ci[0];
		        }
		        znnotdone = rectproject(znnotdone, rect);
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		zn[i][0] = znnotdone[j][0];
		        		zn[i][1] = znnotdone[j++][1];
		        	}
		        }
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		if (zabs(F[j][0], F[j][1]) < tol) {
		        			done[i] = true;
		        			sumdone++;
		        		}
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        k = k + 1;
		    } //  while ((sumdone < done.length) && (k < maxiter))
		    double maxtol = 0.0;
		    if (F != null) {
			    for (i = 0; i < F.length; i++) {
			    	double currentTol = zabs(F[i][0], F[i][1]);
			    	if (currentTol > maxtol) {
			    		maxtol = currentTol;
			    	}
			    }
		    } // if (F != null)
		    if (maxtol > tol) {
		    	MipavUtil.displayWarning("Check solution: maximum residual = " + maxtol);
		    }
		    for (i = 0; i < zn.length; i++) {
		    	zp[i][0] = zn[i][0];
		    	zp[i][1] = zn[i][1];
		    }
		} // if (newton)
		
		return zp;
	}
	
	private double[][] rectproject(double zp[][], double rect[]) {
		// Project points into the rectangle
		int i;
		double zz[][] = new double[zp.length][2];
		for (i = 0; i < zp.length; i++) {
			zz[i][0] = Math.max(Math.min(zp[i][0], rect[1]), rect[0]);
			zz[i][1] = Math.max(Math.min(zp[i][1], rect[3]), rect[2]);
		}
		return zz;
	}
	
	class ODERectModel extends ODE {
		double scale[][];
		double z[][];
		double beta[];
		double c[];
		double zs[][];
		double L[];
		public ODERectModel(int neqn, double y[], double t[], double tout, double relerr[],
				double abserr[], int iflag[], double scale[][], double z[][], 
				double beta[], double c[], double zs[][], double L[]) {
			super(neqn, y, t, tout, relerr, abserr, iflag);
			this.scale = scale;
			this.z = z;
			this.beta = beta;
			this.c = c;
			this.zs = zs;
			this.L = L;
		}
		
		/**
		 * DOCUMENT ME!
		 */
		public void driver() {
			super.driver();
		}
		
		public void f(double x, double yy[], double yp[]) {
			int i;
			double cr[] = new double[1];
			double ci[] = new double[1];
			int lenyy = yy.length;
			int lenzp = lenyy/2;
			double zp[][] = new double[lenzp][2];
			for (i = 0; i < lenzp; i++) {
				zp[i][0] = yy[i];
			}
			for (i = lenzp; i < lenyy; i++) {
				zp[i-lenzp][1] = yy[i];
			}
			
			double fprime[][] = rderiv(zp, z, beta, c, L, zs);
			double f[][] = new double[scale.length][2];
			for (i = 0; i < scale.length; i++) {
				zdiv(scale[i][0], scale[i][1], fprime[i][0], fprime[i][1], cr, ci);
				f[i][0] = cr[0];
				f[i][1] = ci[0];
			}
			
			for (i = 0; i < f.length; i++) {
				yp[i] = f[i][0];
				yp[f.length + i] = f[i][1];
			}	
		}
	}
	
	private double[][] rderiv(double zp[][], double z[][], double beta[], double c[],
			double L[], double zs[][]) {
	    // Derivative of the rectangle map.
		// rderiv returns the derivative at the points of zp of the Schwarz-Christoffel
		// map defined by z, beta, c, and L.  If zs is supplied, it is assumed to be
		// the image of z on the intermediate strip.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		
		 int i, j;
		 double dG[][] = null;
		 double cr[] = new double[1];
		 double ci[] = new double[1];
		 int n = z.length;
		 if ((zs == null) || (zs.length == 0)) {
			 // Find prevertices on the strip
			 zs = new double[z.length][2];
			 double zsprime[][] = new double[z.length][2];
		     r2strip(zs, zsprime, z, z, L[0]);
			 for (i = 0; i < zs.length; i++) {
				// Put them *exactly* on edges
				zs[i][1] = Math.round(zs[i][1]);
			 }
		 } // if ((zs == null) || (zs.length == 0))
		 
		 // First compute map and derivative from rectangle to strip
		 double F[][] = new double[zp.length][2];
		 double dF[][] = new double[zp.length][2];
		 r2strip(F, dF, zp, z, L[0]);
		 
		 // Now compute derivative of map from strip to polygon
		 // Add in ends of strip
		 double diffimagz[] = new double[z.length];
		 for (i = 0; i < z.length-1; i++) {
			 diffimagz[i] = z[i+1][1] -z[i][1]; 
		 }
		 diffimagz[z.length-1] = z[0][1] - z[z.length-1][1];
		 int numends = 0;
		 for (i = 0; i < z.length; i++) {
			 if (diffimagz[i] != 0) {
				 numends++;
			 }
		 } // for (i = 0; i < z.length; i++)
		 int ends[] = new int[numends];
		 for (i = 0, j = 0; i < numends; i++) {
			 if (diffimagz[i] != 0) {
				 ends[j++] = i;
			 }
		 } // for (i = 0, j = 0; i < numends; i++)
		 double zs2[][] = new double[zs.length+2][2];
		 double bs[] = new double[zs.length+2];
		 for (i = 0; i <= ends[0]; i++) {
			 zs2[i][0] = zs[i][0];
			 zs2[i][1] = zs[i][1];
			 bs[i] = beta[i];
		 }
		 zs2[ends[0]+1][0] = Double.POSITIVE_INFINITY;
		 zs2[ends[0]+1][1] = 0;
		 bs[ends[0]+1] = 0;
		 for (i = ends[0]+1; i <= ends[1]; i++) {
			 zs2[i+1][0] = zs[i][0];
			 zs2[i+1][1] = zs[i][1];
			 bs[i+1] = beta[i];	 
		 }
		 zs2[ends[1]+2][0] = Double.NEGATIVE_INFINITY;
		 zs2[ends[1]+2][1] = 0;
		 bs[ends[1]+2] = 0;
		 for (i = ends[1]+1; i < n; i++) {
			 zs2[i+2][0] = zs[i][0];
			 zs2[i+2][1] = zs[i][1];
			 bs[i+2] = beta[i];	 
		 }
		 double c2[] = new double[]{1,0};
		 dG = stderiv(F, zs2, bs, c2, -1);
		 
		 // Put it together;
		 double fprime[][] = new double[zp.length][2];
		 for (i = 0; i < zp.length; i++) {
		     zmlt(c[0], c[1], dF[i][0], dF[i][1], cr, ci);
		     zmlt(cr[0], ci[0], dG[i][0], dG[i][1], cr, ci);
		     fprime[i][0] = cr[0];
		     fprime[i][1] = ci[0];
		 }
		 return fprime;
	}
	
	private double[][] deinvmap(double wp[][], double w[][],
			double beta[], double z[][], double c[], double qdat[][], double z0[][],
			boolean ode, boolean newton, double tol, int maxiter) {
	    // Schwarz-Christoffel disk exterior inverse map.	
	    // deinvmap computes the inverse of the Schwarz-Christoffel exterior map (i.e., from
	    // the exterior of a polygon to the disk) at the points given in vector wp.  The other
		// arguments are as in deparam.
		
		// The default algorithm is to solve an ODE in order to obtain a fair
		// approximation for zp, and then improve zp with Newton iterations.
		// The 2 other possible algorithms are:
		// 1.) Use ODE only.
		// 2.) Use Newton only; take z0 as an initial guess.
		// The ODE solution at wp requires a vector z0 whose forward image w0
		// is such that for each j, the line segment connecting wp[j] and w0[j]
		// lies inside the polygon.  By default z0 is chosen by a fairly robust
		// automatic process.  Using the parameters ode and newton, you can choose
		// to use either an ODE solution or Newton solution exclusively.
		
		// deinvmap has two interpretations.  If the ODE solution is being used,
		// z0 overrides the automatic selection of initial points.  (This can
		// be handy in convex polygons, where the choice of z0 is trivial.)
		// Otherwise, z0 is taken as an initial guess to zp.  In either case,
		// if length(z0) == 1, the value z0 is used for all elements of wp;
		// otherwise, length(z0) should equal length(wp).
		
		// tol, error tolerance for solution (default 1e-8)
	    // maxiter, maximum number of Newton iterations (default 10)
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j, k, m;
		double qdat2[][];
		double zn[][];
		double w0[][];
		double F[][] = null;
		double logreal;
		double logimag;
		double betamult[][][];
		double cr[] = new double[1];
		double ci[] = new double[1];
		double sumbeta[][];
		double expbeta[][];
		double znnotdonesq[][];
		double dF[][];
		
		int n = w.length;
	    double zp[][] = new double[wp.length][2];
		
		int lenwp = wp.length;
		
		if ((qdat == null) || (qdat.length == 0)) {
			qdat2 = new double[1][1];
			qdat2[0][0] = tol;
		}
		else if (qdat.length == 1) {
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 2);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else {
			qdat2 = qdat;
		}
		
		boolean done[] = new boolean[wp.length];
		// First, trap all points indistinguishable from vertices, or they will cause
		// trouble.
		// Modified 05/14/2007 to work around bug in matlab 2007a.
		for (j = 0; j < n; j++) {
			int numidx = 0;
			for (i = 0; i < lenwp; i++) {
				if (zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
					numidx++;
				}
			} // for (i = 0; i < lenwp; i++)
			int idx[] = new int[numidx];
			for (i = 0, k = 0; i < lenwp; i++) {
				if (zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
					idx[k++] = i;
				}
			} // for (i = 0, k = 0; i < lenwp; i++)
			for (i = 0; i < numidx; i++) {
				zp[idx[i]][0] = z[j][0];
				zp[idx[i]][1] = z[j][1];
				done[idx[i]] = true;
			} // for (i = 0; i < numidx; i++)
		} // for (j = 0; j < n; j++)
		int sumdone = 0;
		for (i = 0; i < done.length; i++) {
			if (done[i]) {
				sumdone++;
			}
		} // for (i = 0; i < done.length; i++)
		// lenwp contains number not done
		lenwp = lenwp - sumdone;
		if (lenwp == 0) {
			return null;
		}

		// ODE
		if (ode) {
			double z02[][];
			double w02[][];
			double z03[][];
			double w03[][];
			double wpnotdone[][] = new double[lenwp][2];
			for (i = 0, j = 0; i < done.length; i++) {
				if (!done[i]) {
					wpnotdone[j][0] = wp[i][0];
					wpnotdone[j++][1] = wp[i][1];
				}
			} // for (i = 0, j = 0; i < done.length; i++)
			if ((z0 == null) || (z0.length == 0)) {
			  // Pick a value z0 (not a singularity) and compute the map there.	
				z03 = new double[lenwp][2];
				w03 = new double[lenwp][2];
				scimapz0(z03, w03, "de", wpnotdone, w, beta, z, c, qdat2, null);
			} // if ((z0 == null) || (z0.length == 0)) 
			else {
			    w0 = demap(z0,w,beta,z,c,qdat2);
			    if ((z0.length == 1) && (lenwp > 1)) {
			        z02 = new double[lenwp][2];
			        w02 = new double[lenwp][2];
			        for (i = 0; i < lenwp; i++) {
			        	z02[i][0] = z0[0][0];
			        	z02[i][1] = z0[0][1];
			        	w02[i][0] = w0[0][0];
			        	w02[i][1] = w0[0][1];
			        }
			    } // if ((z0.length == 1) && (lenwp > 1)
			    else {
			    	w02 = w0;
			    	z02 = z0;
			    }
			    w03 = new double[lenwp][2];
			    z03 = new double[lenwp][2];
			    for (i = 0, j = 0; i < done.length; i++) {
			    	if (!done[i]) {
			    		w03[j][0] = w02[i][0];
			    		w03[j][1] = w02[i][1];
			    		z03[j][0] = z02[i][0];
			    		z03[j++][1] = z02[i][1];
			    	}
			    } // for (i = 0, j = 0; i < done.length; i++)
			} // else
			
			// Use relaxed ODE tol if improving with Newton.
			double odetol;
			if (newton) {
			    odetol = Math.max(tol,  1.0E-3);
			}
			else {
				odetol = tol;
			}
			double abstol = odetol;
			double reltol = odetol;
			
			// Rescale dependent coordinate
			double scale[][] = new double[lenwp][2];
			for (i = 0; i < lenwp; i++) {
				scale[i][0] = wpnotdone[i][0] - w03[i][0];
				scale[i][1] = wpnotdone[i][1] - w03[i][1];
			}
			
			// Solve ODE
			double z04[] = new double[2*z03.length];
			for (i = 0; i < z03.length; i++) {
				z04[i] = z03[i][0];
				z04[i + z03.length] = z03[i][1];
			}
			//double tspan[] = new double[3];
			//tspan[0] = 0;
			//tspan[1] = 0.5;
			//tspan[2] = 1.0;
			//double t[] = new double[tspan.length];
			//double y[][] = new double[tspan.length][z04.length];
			//deimapfun(t, y, tspan, z04, abstol, reltol,scale, z, beta, c);
			// Because ode233.m is copyrighted by MATLAB use a port of the
			// original FORTRAN ode.f code 
			double yarr[][] = new double[3][z04.length];
			for (i = 0; i < z04.length; i++) {
				yarr[0][i] = z04[i];
			}
			double coef = 0.1;
			double t[] = new double[1];
			double relerr[] = new double[1];
			double abserr[] = new double[1];
			int iflag[] = new int[1];
			ODEExtModel modODE;
			double tout;
			while (true) {
				t[0] = 0;
				tout = 0.5;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z04.length; i++) {
				    z04[i] = yarr[0][i];
				}
				modODE = new ODEExtModel(z04.length, z04, t, tout, relerr,
						abserr, iflag, scale, z, beta, c);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < z04.length; i++) {
				yarr[1][i] = z04[i];
			}
			coef = 0.1;
			while (true) {
				t[0] = 0.5;
				tout = 1.0;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z04.length; i++) {
				    z04[i] = yarr[1][i];	
				}
				modODE = new ODEExtModel(z04.length, z04, t, tout, relerr,
						abserr, iflag, scale, z, beta, c);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < z04.length; i++) {
				yarr[2][i] = z04[i];
			}
			m = yarr.length;
			//int leny = yarr[0].length;
			for (i = 0, j = 0; i < done.length; i++) {
				if (!done[i]) {
				    zp[i][0] = yarr[m-1][j];
					zp[i][1] = yarr[m-1][j + lenwp];
					j++;
				}
			} // for (i = 0, j = 0; i < done.length; i++)
			double abszp;
			for (i = 0; i < zp.length; i++) {
				abszp = zabs(zp[i][0], zp[i][1]);
				if (abszp > 1.0) {
					zp[i][0] = zp[i][0]/abszp;
					zp[i][1] = zp[i][1]/abszp;
				}
			} // for (i = 0; i < zp.length; i++)
		} // if (ode)
		
		// Newton iterations
		if (newton) {
		    if (!ode) {
		        if ((z0.length == 1) & (lenwp > 1)) {
		        	zn = new double[lenwp][2];
		        	for (i = 0; i < lenwp; i++) {
		        		zn[i][0] = z0[0][0];
		        		zn[i][1] = z0[0][1];
		        	}
		        } // if ((z0.length == 1) & (lenwp > 1))
		        else {
		        	zn = new double[z0.length][2];
		        	for (i = 0; i < z0.length; i++) {
		        		zn[i][0] = z0[i][0];
		        		zn[i][1] = z0[i][1];
		        	}
		        } // else
		        for (i = 0; i < done.length; i++) {
		        	if (done[i]) {
		        		zn[i][0] = zp[i][0];
		        		zn[i][1] = zp[i][1];
		        	}
		        }
		    } // if (!ode)
		    else { // ode
		    	zn = new double[zp.length][2];
		    	for (i = 0; i < zp.length; i++) {
		    		zn[i][0] = zp[i][0];
		    		zn[i][1] = zp[i][1];
		    	}
		    } // else ode
		    
		    k = 0;
		    while ((sumdone < done.length) && (k < maxiter)) {
		        m = done.length - sumdone;
		        double znnotdone[][] = new double[m][2];
		        double wpnotdone[][] = new double[m][2];
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		znnotdone[j][0] = zn[i][0];
		        		znnotdone[j][1] = zn[i][1];
		        		wpnotdone[j][0] = wp[i][0];
		        		wpnotdone[j][1] = wp[i][1];
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        w0 = demap(znnotdone, w, beta, z, c, qdat2);
		        F = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	F[i][0] = wpnotdone[i][0] - w0[i][0];
		        	F[i][1] = wpnotdone[i][1] - w0[i][1];
		        }
		        betamult = new double[n][m][2];
		        for (i = 0; i < n; i++) {
		        	for (j = 0; j < m; j++) {
		        		zdiv(znnotdone[j][0], znnotdone[j][1], z[i][0], z[i][1], cr, ci);
		        		logreal = Math.log(zabs(1.0 - cr[0], -ci[0]));
		        		logimag = Math.atan2(-ci[0], 1- cr[0]);
		        		betamult[i][j][0] = beta[i] * logreal;
		        		betamult[i][j][1] = beta[i] * logimag;
		        	}
		        } // for (i = 0; i < n; i++)
		        sumbeta = new double[m][2];
		        for (j = 0; j < m; j++) {
		            for (i = 0; i < n; i++) {
		            	sumbeta[j][0] += betamult[i][j][0];
		            	sumbeta[j][1] += betamult[i][j][1];
		            }
		        } // for (j = 0; j < m; j++)
		        expbeta = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	double var = Math.exp(sumbeta[i][0]);
		        	expbeta[i][0] = var*Math.cos(sumbeta[i][1]);
		        	expbeta[i][1] = var*Math.sin(sumbeta[i][1]);
		        } // for (i = 0; i < m; i++)
		        znnotdonesq = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	zmlt(znnotdone[i][0], znnotdone[i][1], znnotdone[i][0], znnotdone[i][1], cr, ci);
		        	znnotdonesq[i][0] = cr[0];
		        	znnotdonesq[i][1] = ci[0];
		        }
		        double cz[][] = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	zdiv(c[0], c[1], znnotdonesq[i][0], znnotdonesq[i][1], cr, ci);
		        	cz[i][0] = cr[0];
		        	cz[i][1] = ci[0];
		        }
		        dF = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	zmlt(cz[i][0], cz[i][1], expbeta[i][0], expbeta[i][1], cr, ci);
		        	dF[i][0] = cr[0];
		        	dF[i][1] = ci[0];
		        } // for (i = 0; i < m; i++)
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		zdiv(F[j][0], F[j][1], dF[j][0], dF[j][1], cr, ci);
		        		zn[i][0] = zn[i][0] + cr[0];
		        		zn[i][1] = zn[i][1] + ci[0];
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		if (zabs(F[j][0], F[j][1]) < tol) {
		        			done[i] = true;
		        			sumdone++;
		        		}
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        k = k + 1;
		    } //  while ((sumdone < done.length) && (k < maxiter))
		    double maxtol = 0.0;
		    if (F != null) {
			    for (i = 0; i < F.length; i++) {
			    	double currentTol = zabs(F[i][0], F[i][1]);
			    	if (currentTol > maxtol) {
			    		maxtol = currentTol;
			    	}
			    }
		    } // if (F != null)
		    if (maxtol > tol) {
		    	MipavUtil.displayWarning("Check solution: maximum residual = " + maxtol);
		    }
		    for (i = 0; i < zn.length; i++) {
		    	zp[i][0] = zn[i][0];
		    	zp[i][1] = zn[i][1];
		    }
		} // if (newton)
		
		return zp;
	}
	
	private int[] dinvmap(double zp[][], double wp[][], double w[][],
			double beta[], double z[][], double c[], double qdat[][], double z0[][],
			boolean ode, boolean newton, double tol, int maxiter) {
	    // Schwarz-Christoffel disk inverse map.	
	    // dinvmap computes the inverse of the Schwarz-Christoffel disk map (i.e., from
	    // a polygon to the disk) at the points given in vector wp.  The other
		// arguments are as in dparam.
		
		// The default algorithm is to solve an ODE in order to obtain a fair
		// approximation for zp, and then improve zp with Newton iterations.
		// The 2 other possible algorithms are:
		// 1.) Use ODE only.
		// 2.) Use Newton only; take z0 as an initial guess.
		// The ODE solution at wp requires a vector z0 whose forward image w0
		// is such that for each j, the line segment connecting wp[j] and w0[j]
		// lies inside the polygon.  By default z0 is chosen by a fairly robust
		// automatic process.  Using the parameters ode and newton, you can choose
		// to use either an ODE solution or Newton solution exclusively.
		
		// dinvmap has two interpretations.  If the ODE solution is being used,
		// z0 overrides the automatic selection of initial points.  (This can
		// be handy in convex polygons, where the choice of z0 is trivial.)
		// Otherwise, z0 is taken as an initial guess to zp.  In either case,
		// if length(z0) == 1, the value z0 is used for all elements of wp;
		// otherwise, length(z0) should equal length(wp).
		
		// tol, error tolerance for solution (default 1e-8)
	    // maxiter, maximum number of Newton iterations (default 10)
		
		// flag has a vector of indices where the method was unable to produce a
		// sufficiently small residual.  A warning is issued when this occurs.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int flag[] = null;
		int i, j, k, m;
		double qdat2[][];
		double zn[][];
		double w0[][];
		double F[][] = null;
		double logreal;
		double logimag;
		double betamult[][][];
		double cr[] = new double[1];
		double ci[] = new double[1];
		double sumbeta[][];
		double expbeta[][];
		double dF[][];
		
		int n = w.length;
		for (i = 0; i < wp.length; i++) {
			zp[i][0] = 0;
			zp[i][1] = 0;
		}
		int lenwp = wp.length;
		
		if ((qdat == null) || (qdat.length == 0)) {
			qdat2 = new double[1][1];
			qdat2[0][0] = tol;
		}
		else if (qdat.length == 1) {
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 2);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else {
			qdat2 = qdat;
		}
		
		boolean done[] = new boolean[wp.length];
		// First, trap all points indistinguishable from vertices, or they will cause
		// trouble.
		// Modified 05/14/2007 to work around bug in matlab 2007a.
		for (j = 0; j < n; j++) {
			int numidx = 0;
			for (i = 0; i < lenwp; i++) {
				if (zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
					numidx++;
				}
			} // for (i = 0; i < lenwp; i++)
			int idx[] = new int[numidx];
			for (i = 0, k = 0; i < lenwp; i++) {
				if (zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
					idx[k++] = i;
				}
			} // for (i = 0, k = 0; i < lenwp; i++)
			for (i = 0; i < numidx; i++) {
				zp[idx[i]][0] = z[j][0];
				zp[idx[i]][1] = z[j][1];
				done[idx[i]] = true;
			} // for (i = 0; i < numidx; i++)
		} // for (j = 0; j < n; j++)
		int sumdone = 0;
		for (i = 0; i < done.length; i++) {
			if (done[i]) {
				sumdone++;
			}
		} // for (i = 0; i < done.length; i++)
		// lenwp contains number not done
		lenwp = lenwp - sumdone;
		if (lenwp == 0) {
			return null;
		}

		// ODE
		if (ode) {
			double z02[][];
			double w02[][];
			double z03[][];
			double w03[][];
			double wpnotdone[][] = new double[lenwp][2];
			for (i = 0, j = 0; i < done.length; i++) {
				if (!done[i]) {
					wpnotdone[j][0] = wp[i][0];
					wpnotdone[j++][1] = wp[i][1];
				}
			} // for (i = 0, j = 0; i < done.length; i++)
			if ((z0 == null) || (z0.length == 0)) {
			  // Pick a value z0 (not a singularity) and compute the map there.	
				z03 = new double[lenwp][2];
				w03 = new double[lenwp][2];
				scimapz0(z03, w03, "d", wpnotdone, w, beta, z, c, qdat2, null);
			} // if ((z0 == null) || (z0.length == 0)) 
			else {
			    w0 = dmap(z0,w,beta,z,c,qdat2);
			    if ((z0.length == 1) && (lenwp > 1)) {
			        z02 = new double[lenwp][2];
			        w02 = new double[lenwp][2];
			        for (i = 0; i < lenwp; i++) {
			        	z02[i][0] = z0[0][0];
			        	z02[i][1] = z0[0][1];
			        	w02[i][0] = w0[0][0];
			        	w02[i][1] = w0[0][1];
			        }
			    } // if ((z0.length == 1) && (lenwp > 1)
			    else {
			    	w02 = w0;
			    	z02 = z0;
			    }
			    w03 = new double[lenwp][2];
			    z03 = new double[lenwp][2];
			    for (i = 0, j = 0; i < done.length; i++) {
			    	if (!done[i]) {
			    		w03[j][0] = w02[i][0];
			    		w03[j][1] = w02[i][1];
			    		z03[j][0] = z02[i][0];
			    		z03[j++][1] = z02[i][1];
			    	}
			    } // for (i = 0, j = 0; i < done.length; i++)
			} // else
			
			// Use relaxed ODE tol if improving with Newton.
			double odetol;
			if (newton) {
			    odetol = Math.max(tol,  1.0E-4);
			}
			else {
				odetol = tol;
			}
			double abstol = odetol;
			double reltol = odetol;
			
			// Rescale dependent coordinate
			double scale[][] = new double[lenwp][2];
			for (i = 0; i < lenwp; i++) {
				scale[i][0] = wpnotdone[i][0] - w03[i][0];
				scale[i][1] = wpnotdone[i][1] - w03[i][1];
			}
			
			// Solve ODE
			double z04[] = new double[2*z03.length];
			for (i = 0; i < z03.length; i++) {
				z04[i] = z03[i][0];
				z04[i + z03.length] = z03[i][1];
			}
			//double tspan[] = new double[3];
			//tspan[0] = 0;
			//tspan[1] = 0.5;
			//tspan[2] = 1.0;
			//double t[] = new double[tspan.length];
			//double y[][] = new double[tspan.length][z04.length];
			//dimapfun(t, y, tspan, z04, abstol, reltol,scale, z, beta, c);
			// Because ode113.m is copyrighted by MATLAB use a port of the
			// original FORTRAN ode.f code 
			double yarr[][] = new double[3][z04.length];
			for (i = 0; i < z04.length; i++) {
				yarr[0][i] = z04[i];
			}
			double coef = 0.1;
			double t[] = new double[1];
			double relerr[] = new double[1];
			double abserr[] = new double[1];
			int iflag[] = new int[1];
			ODEModel modODE;
			double tout;
			while (true) {
				t[0] = 0;
				tout = 0.5;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z04.length; i++) {
				    z04[i] = yarr[0][i];
				}
				modODE = new ODEModel(z04.length, z04, t, tout, relerr,
						abserr, iflag, scale, z, beta, c);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < z04.length; i++) {
				yarr[1][i] = z04[i];
			}
			coef = 0.1;
			while (true) {
				t[0] = 0.5;
				tout = 1.0;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z04.length; i++) {
				    z04[i] = yarr[1][i];	
				}
				modODE = new ODEModel(z04.length, z04, t, tout, relerr,
						abserr, iflag, scale, z, beta, c);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < z04.length; i++) {
				yarr[2][i] = z04[i];
			}
			m = yarr.length;
			//int leny = yarr[0].length;
			for (i = 0, j = 0; i < done.length; i++) {
				if (!done[i]) {
				    zp[i][0] = yarr[m-1][j];
					zp[i][1] = yarr[m-1][j + lenwp];
					j++;
				}
			} // for (i = 0, j = 0; i < done.length; i++)
			double abszp;
			for (i = 0; i < zp.length; i++) {
				abszp = zabs(zp[i][0], zp[i][1]);
				if (abszp > 1.0) {
					zp[i][0] = zp[i][0]/abszp;
					zp[i][1] = zp[i][1]/abszp;
				}
			} // for (i = 0; i < zp.length; i++)
		} // if (ode)
		
		// Newton iterations
		if (newton) {
		    if (!ode) {
		        if ((z0.length == 1) & (lenwp > 1)) {
		        	zn = new double[lenwp][2];
		        	for (i = 0; i < lenwp; i++) {
		        		zn[i][0] = z0[0][0];
		        		zn[i][1] = z0[0][1];
		        	}
		        } // if ((z0.length == 1) & (lenwp > 1))
		        else {
		        	zn = new double[z0.length][2];
		        	for (i = 0; i < z0.length; i++) {
		        		zn[i][0] = z0[i][0];
		        		zn[i][1] = z0[i][1];
		        	}
		        } // else
		        for (i = 0; i < done.length; i++) {
		        	if (done[i]) {
		        		zn[i][0] = zp[i][0];
		        		zn[i][1] = zp[i][1];
		        	}
		        }
		    } // if (!ode)
		    else { // ode
		    	zn = new double[zp.length][2];
		    	for (i = 0; i < zp.length; i++) {
		    		zn[i][0] = zp[i][0];
		    		zn[i][1] = zp[i][1];
		    	}
		    } // else ode
		    
		    k = 0;
		    while ((sumdone < done.length) && (k < maxiter)) {
		        m = done.length - sumdone;
		        double znnotdone[][] = new double[m][2];
		        double wpnotdone[][] = new double[m][2];
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		znnotdone[j][0] = zn[i][0];
		        		znnotdone[j][1] = zn[i][1];
		        		wpnotdone[j][0] = wp[i][0];
		        		wpnotdone[j][1] = wp[i][1];
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        w0 = dmap(znnotdone, w, beta, z, c, qdat2);
		        F = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	F[i][0] = wpnotdone[i][0] - w0[i][0];
		        	F[i][1] = wpnotdone[i][1] - w0[i][1];
		        }
		        betamult = new double[n][m][2];
		        for (i = 0; i < n; i++) {
		        	for (j = 0; j < m; j++) {
		        		zdiv(znnotdone[j][0], znnotdone[j][1], z[i][0], z[i][1], cr, ci);
		        		logreal = Math.log(zabs(1.0 - cr[0], -ci[0]));
		        		logimag = Math.atan2(-ci[0], 1- cr[0]);
		        		betamult[i][j][0] = beta[i] * logreal;
		        		betamult[i][j][1] = beta[i] * logimag;
		        	}
		        } // for (i = 0; i < n; i++)
		        sumbeta = new double[m][2];
		        for (j = 0; j < m; j++) {
		            for (i = 0; i < n; i++) {
		            	sumbeta[j][0] += betamult[i][j][0];
		            	sumbeta[j][1] += betamult[i][j][1];
		            }
		        } // for (j = 0; j < m; j++)
		        expbeta = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	double var = Math.exp(sumbeta[i][0]);
		        	expbeta[i][0] = var*Math.cos(sumbeta[i][1]);
		        	expbeta[i][1] = var*Math.sin(sumbeta[i][1]);
		        } // for (i = 0; i < m; i++)
		        dF = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	zmlt(c[0], c[1], expbeta[i][0], expbeta[i][1], cr, ci);
		        	dF[i][0] = cr[0];
		        	dF[i][1] = ci[0];
		        } // for (i = 0; i < m; i++)
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		zdiv(F[j][0], F[j][1], dF[j][0], dF[j][1], cr, ci);
		        		zn[i][0] = zn[i][0] + cr[0];
		        		zn[i][1] = zn[i][1] + ci[0];
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        for (i = 0; i < zn.length; i++) {
		        	double znabs = zabs(zn[i][0], zn[i][1]);
		        	if (znabs > 1) {
		        		zn[i][0] = zn[i][0]/znabs;
		        		zn[i][1] = zn[i][1]/znabs;
		        	}
		        }
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		if (zabs(F[j][0], F[j][1]) < tol) {
		        			done[i] = true;
		        			sumdone++;
		        		}
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        k = k + 1;
		    } //  while ((sumdone < done.length) && (k < maxiter))
		    double maxtol = 0.0;
		    if (F != null) {
			    for (i = 0; i < F.length; i++) {
			    	double currentTol = zabs(F[i][0], F[i][1]);
			    	if (currentTol > maxtol) {
			    		maxtol = currentTol;
			    	}
			    }
		    } // if (F != null)
		    if (maxtol > tol) {
		    	MipavUtil.displayWarning("Check solution: maximum residual = " + maxtol);
		    }
		    for (i = 0; i <zn.length; i++) {
		    	zp[i][0] = zn[i][0];
		    	zp[i][1] = zn[i][1];
		    }
		} // if (newton)
		int numnotdone = 0;
		for (i = 0; i < done.length; i++) {
			if (!done[i]) {
			    numnotdone++;	
			}
		} // for (i = 0; i < done.length; i++)
		flag = new int[numnotdone];
		for (i = 0, j = 0; i < done.length; i++) {
			if (!done[i]) {
				flag[j++] = i;
			}
		} // for (i = 0, j = 0; i < done.length; i++)
		return flag;
	}
	
	class ODEExtModel extends ODE {
		double scale[][];
		double z[][];
		double beta[];
		double c[];
		public ODEExtModel(int neqn, double y[], double t[], double tout, double relerr[],
				double abserr[], int iflag[], double scale[][], double z[][], 
				double beta[], double c[]) {
			super(neqn, y, t, tout, relerr, abserr, iflag);
			this.scale = scale;
			this.z = z;
			this.beta = beta;
			this.c = c;
			
		}
		
		/**
		 * DOCUMENT ME!
		 */
		public void driver() {
			super.driver();
		}
		
		public void f(double x, double yy[], double yp[]) {
			int i;
			double cr[] = new double[1];
			double ci[] = new double[1];
			int lenyy = yy.length;
			int lenzp = lenyy/2;
			double zp[][] = new double[lenzp][2];
			for (i = 0; i < lenzp; i++) {
				zp[i][0] = yy[i];
			}
			for (i = lenzp; i < lenyy; i++) {
				zp[i-lenzp][1] = yy[i];
			}
			
			double fprime[][] = dederiv(zp, z, beta, c);
			double f[][] = new double[scale.length][2];
			for (i = 0; i < scale.length; i++) {
				zdiv(scale[i][0], scale[i][1], fprime[i][0], fprime[i][1], cr, ci);
				f[i][0] = cr[0];
				f[i][1] = ci[0];
			}
			
			for (i = 0; i < f.length; i++) {
				yp[i] = f[i][0];
				yp[f.length + i] = f[i][1];
			}	
		}
	}
	
	class ODEModel extends ODE {
		double scale[][];
		double z[][];
		double beta[];
		double c[];
		public ODEModel(int neqn, double y[], double t[], double tout, double relerr[],
				double abserr[], int iflag[], double scale[][], double z[][], 
				double beta[], double c[]) {
			super(neqn, y, t, tout, relerr, abserr, iflag);
			this.scale = scale;
			this.z = z;
			this.beta = beta;
			this.c = c;
			
		}
		
		/**
		 * DOCUMENT ME!
		 */
		public void driver() {
			super.driver();
		}
		
		public void f(double x, double yy[], double yp[]) {
			int i;
			double cr[] = new double[1];
			double ci[] = new double[1];
			int lenyy = yy.length;
			int lenzp = lenyy/2;
			double zp[][] = new double[lenzp][2];
			for (i = 0; i < lenzp; i++) {
				zp[i][0] = yy[i];
			}
			for (i = lenzp; i < lenyy; i++) {
				zp[i-lenzp][1] = yy[i];
			}
			
			double fprime[][] = dderiv(zp, z, beta, c);
			double f[][] = new double[scale.length][2];
			for (i = 0; i < scale.length; i++) {
				zdiv(scale[i][0], scale[i][1], fprime[i][0], fprime[i][1], cr, ci);
				f[i][0] = cr[0];
				f[i][1] = ci[0];
			}
			
			for (i = 0; i < f.length; i++) {
				yp[i] = f[i][0];
				yp[f.length + i] = f[i][1];
			}	
		}
	}
	
	/*private void dimapfun(double t[], double y[][], double wp[], double yp[], double abstol, double reltol,
			double scale[][], double z[][], double beta[], double c[]) {
		// Used by dinvmap for the solution of an ODE.
		// t is the vector of output times
		// For each time the y values y1 to yn of the differential equations
		// in different columns
		// wp are the time span
		// yp are the intial conditions
		// zdot is a column vector dy1/dt, dy2/dt,...,dyn/dt
		// Original MATLAB dimapfun copyright 1998 by Toby Driscoll
		int i;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int lenyp = yp.length;
		int lenzp = lenyp/2;
		double zp[][] = new double[lenyp][2];
		for (i = 0; i < lenzp; i++) {
			zp[i][0] = yp[i];
		}
		for (i = lenzp; i < lenyp; i++) {
			zp[i][1] = yp[i];
		}
		
		double fprime[][] = dderiv(zp, z, beta, c);
		double f[][] = new double[scale.length][2];
		for (i = 0; i < scale.length; i++) {
			zdiv(scale[i][0], scale[i][1], fprime[i][0], fprime[i][1], cr, ci);
			f[i][0] = cr[0];
			f[i][1] = ci[0];
		}
		
		double zdot[] = new double[2*f.length];
		for (i = 0; i < f.length; i++) {
			zdot[i] = f[i][0];
			zdot[f.length + i] = f[i][1];
		}
	}*/
	
	private double[][] diskevaldiff(scmap M, double zp[][]) {
		// Derivative of the Schwarz-Christoffel disk map at points zp.
		
		// Original evaldiff MATLAB routne copyright 1998 by Toby Driscoll.
		int i;
		double z[][] = M.prevertex;
		double c[] = M.constant;
		polygon p = M.poly;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
		    beta[i] = p.angle[i] - 1.0;
		}
		double fp[][] = dderiv(zp, z, beta, c);
		return fp;
	}
	
	private double[][] dederiv(double zp[][], double z[][], double beta[], double c[]) {
		// Derivative of the exterior map.
		// dderiv returns the derivative at the points of zp of the 
		// Schwarz-Christoffel exterior map defined by z, beta, and c.
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i,j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double zprow[][] = new double[zp.length][2];
		for (i = 0; i < zp.length; i++) {
			zprow[i][0] = zp[i][0];
			zprow[i][1] = zp[i][1];
		}
		double fprime[][] = new double[zp.length][2];
		double beta2[] = new double[beta.length+1];
		for (i = 0; i < beta.length; i++) {
			beta2[i] = beta[i];
		}
		beta2[beta2.length-1] = -2;
		
		int npts = zp.length;
		double terms[] = new double[2];
		double logterms[] = new double[2];
		double betaterms[] = new double[2];
		double sumterms[][] = new double[npts][2];
		for (j = 0; j < npts; j++) {
		    for (i = 0; i < beta2.length; i++) {
		    	if (i < beta2.length-1) {
				    zdiv(zprow[j][0], zprow[j][1], z[i][0], z[i][1], cr, ci);
				    terms[0] = 1 - cr[0];
				    terms[1] = -ci[0];
		    	}
		    	else {
		    		terms[0] = zprow[j][0];
		    		terms[1] = zprow[j][1];
		    	}
				logterms[0] = Math.log(zabs(terms[0], terms[1]));
				logterms[1] = Math.atan2(terms[1],terms[0]);
				betaterms[0] = logterms[0] * beta2[i];
				betaterms[1] = logterms[1] * beta2[i];
				sumterms[j][0] += betaterms[0];
				sumterms[j][1] += betaterms[1];
			}
		}
		
		for (i = 0; i < npts; i++) {
			double expterm = Math.exp(sumterms[i][0]);
			zmlt(c[0], c[1],expterm*Math.cos(sumterms[i][1]), 
					expterm*Math.sin(sumterms[i][1]), cr, ci);
			fprime[i][0] = cr[0];
			fprime[i][1] = ci[0];
		}
	    
		return fprime;
	}
	
	private double[][] dderiv(double zp[][], double z[][], double beta[], double c[]) {
		// Derivative of the disk map.
		// dderiv returns the derivative at the points of zp of the 
		// Schwarz-Christoffel disk map defined by z, beta, and c.
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i,j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double zprow[][] = new double[zp.length][2];
		for (i = 0; i < zp.length; i++) {
			zprow[i][0] = zp[i][0];
			zprow[i][1] = zp[i][1];
		}
		double fprime[][] = new double[zp.length][2];
		
		int npts = zp.length;
		double terms[] = new double[2];
		double logterms[] = new double[2];
		double betaterms[] = new double[2];
		double sumterms[][] = new double[npts][2];
		for (j = 0; j < npts; j++) {
		    for (i = 0; i < beta.length; i++) {
				zdiv(zprow[j][0], zprow[j][1], z[i][0], z[i][1], cr, ci);
				terms[0] = 1 - cr[0];
				terms[1] = -ci[0];
				logterms[0] = Math.log(zabs(terms[0], terms[1]));
				logterms[1] = Math.atan2(terms[1],terms[0]);
				betaterms[0] = logterms[0] * beta[i];
				betaterms[1] = logterms[1] * beta[i];
				sumterms[j][0] += betaterms[0];
				sumterms[j][1] += betaterms[1];
			}
		}
		
		for (i = 0; i < npts; i++) {
			double expterm = Math.exp(sumterms[i][0]);
			zmlt(c[0], c[1],expterm*Math.cos(sumterms[i][1]), 
					expterm*Math.sin(sumterms[i][1]), cr, ci);
			fprime[i][0] = cr[0];
			fprime[i][1] = ci[0];
		}
	    
		return fprime;
	}
	
	private double[][] rectangle(scmap M) {
		// Return the corners of the rectangle in the fundamental domain.
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i;
		double zrp[][] = M.prevertex;
        int cor[] = corners(M);
        double zr[][] = new double[cor.length][2];
        for (i = 0; i < cor.length; i++) {
        	zr[i] = zrp[cor[i]];
        }
        return zr;
	}
	
	private int[] corners(scmap M) {
		// Indices of rectangle/generalized quadrilateral corners.
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j;
		double z[][] = M.prevertex;
		
		// Find extent of rectangle
		double K = -Double.MAX_VALUE;
		double Kp = -Double.MAX_VALUE;
		for (i = 0; i < z.length; i++) {
		    if (z[i][0] > K) {
		    	K = z[i][0];
		    }
		    if (z[i][1] > Kp) {
		    	Kp = z[i][1];
		    }
		} // for (i = 0; i < z.length; i++)
		
		// First corner is K  + 0i
		double zmat[][][] = new double[z.length][4][2];
		for (i = 0; i < z.length; i++) {
			for (j = 0; j < 4; j++) {
				zmat[i][j][0] = z[i][0];
				zmat[i][j][1] = z[i][1];
			}
		}
		double kmat[][][] = new double[z.length][4][2];
		for (i = 0; i < z.length; i++) {
			kmat[i][0][0] = K;
			kmat[i][0][1] = 0;
			kmat[i][1][0] = K;
			kmat[i][1][1] = Kp;
			kmat[i][2][0] = -K;
			kmat[i][2][1] = Kp;
			kmat[i][3][0] = -K;
			kmat[i][3][1] = 0;
		}
		double dif[][][] = new double[z.length][4][2];
		for (i = 0; i < z.length; i++) {
			for (j = 0; j < 4; j++) {
				dif[i][j][0] = zmat[i][j][0] - kmat[i][j][0];
				dif[i][j][1] = zmat[i][j][1] - kmat[i][j][1];
			} 
		}
		double absdif[][] = new double[z.length][4];
		for (i = 0; i < z.length; i++) {
			for (j = 0; j < 4; j++) {
				absdif[i][j] = zabs(dif[i][j][0], dif[i][j][1]);
			}
		}
		int corner[] = new int[4];
		for (j = 0; j < 4; j++) {
			double minVal = Double.MAX_VALUE;
			corner[j] = -1;
			for (i = 0; i < z.length; i++) {
				if (absdif[i][j] < minVal) {
					minVal = absdif[i][j];
					corner[j] = i;
				}
			}
		}
		return corner;
	}
	
	public void scimapz0(double z0[][], double w0[][], String prefix, double wp[][],
			double w[][], double beta[], double z[][], double c[], double qdat[][],
			double aux[][]) {
	    // scimapz0 returns starting points for computing inverses of 
		// Schwarz-Christoffel maps.
		
		// Each wp[j] (in the polygon plane) requires z0[j] (in the fundamental domain)
		// whose image w0[j] is such that the line segment from w0[j] to wp[j] lies in
		// the target (interior or exterior) region.  The algorithm here is to choose
		// z0[j] as a (weighted) average of successive pairs of adjacent prevertices.
		// The resulting w0[j] is on a polygon side.  Each choice is tested by looking
		// for intersections of the segment with (other) sides of the polygon.
		
		// After randomly trying 10 weights with such prevertex pairs, the routine gives
		// up.  Failures are pretty rare.  Slits are the most likely cause of trouble,
		// since  the intersection method doesn't know "which side" of the slit it's on.
		// In such a case you will have to supply starting points manually, perhaps by
		// a continuation method.
		
		// See also hpinvmap, dinvmap, deinvmap, rinvmap, stinvmap.
		
		// Original MATLAB routine copyright 1997 by Toby Driscoll.  Last updated 05/07/97.
		
		// P.S.  This file illustrates why the different domains in the SC toolbox
		// have mostly independent M-files.  The contingencies for the various
		// geometries become rather cumbersome.
		
		int i,j,k,p;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int n = w.length;
		boolean from_disk = false;
		boolean from_hp = false;
		boolean from_strip = false;
		boolean from_rect = false;
		double shape[][] = new double[wp.length][2];
		for (i = 0; i < wp.length; i++) {
			shape[i][0] = wp[i][0];
			shape[i][1] = wp[i][1];
			z0[i][0] = wp[i][0];
			z0[i][1] = wp[i][1];
			w0[i][0] = wp[i][0];
			w0[i][1] = wp[i][1];
		}
		if (prefix.substring(0,1).equalsIgnoreCase("d")) {
			from_disk = true;
		}
		else if (prefix.equalsIgnoreCase("hp")) {
			from_hp = true;
		}
		else if (prefix.equalsIgnoreCase("st")) {
			from_strip = true;
		}
		else if (prefix.equalsIgnoreCase("r")) {
			from_rect = true;
		}
		double w2[][];
		double z2[][];
		double beta2[];
		double qdat2[][];
		double argw[] = new double[n];
		int kinf = -1;
		// Calculate arguments of the directed polygon edges.
		if (from_strip) {
		    // Renumber to put left end of strip first
			int numinf = 0;
			for (i = 0; i < z.length; i++) {
				if (Double.isInfinite(z[i][0]) || (Double.isInfinite(z[i][1]))) {
					numinf++;
				}
			}
			int atinf[] = new int[numinf];
			for (i = 0, j = 0; i < z.length; i++) {
				if (Double.isInfinite(z[i][0]) || (Double.isInfinite(z[i][1]))) {
					atinf[j++] = i;
				}
			}
			int renum[] = new int[n];
			for (i = 0; i < n-atinf[0]; i++) {
				renum[i] = i+atinf[0];
			}
			for (i = n-atinf[0]; i < n; i++) {
				renum[i] = i-n+atinf[0];
			}
			w2 = new double[n][2];
			z2 = new double[n][2];
			beta2 = new double[n];
			qdat2 = new double[qdat.length][2*n+1];
			for (i = 0; i < n; i++) {
				w2[i][0] = w[renum[i]][0];
				w2[i][1] = w[renum[i]][1];
				z2[i][0] = z[renum[i]][0];
				z2[i][1] = z[renum[i]][1];
				if (Double.isInfinite(z2[i][0]) || Double.isInfinite(z2[i][1])) {
				    kinf = i;	
				}
				beta2[i] = beta[renum[i]];
				for (j = 0; j < qdat.length; j++) {
					qdat2[j][i] = qdat[j][renum[i]];
					qdat2[j][n+1+i] = qdat[j][n+1+renum[i]];
				}
			} // for (i = 0; i < n; i++)
			double diffw[] = new double[2];
			diffw[0] = w[2][0] - w[1][0];
			diffw[1] = w[2][1] - w[1][1];
			double argwp[] = new double[n];
			argwp[0] = Math.atan2(diffw[1], diffw[0]);
			for (i = 1; i < n-1; i++) {
				argwp[i] = argwp[i-1] - Math.PI*beta[i+1];
			}
			argwp[n-1] = argwp[n-2] - Math.PI*beta[0];
			argw[0] = argwp[n-1];
			for (i = 1; i < n; i++) {
				argw[i] = argwp[i-1];
			}
		} // if (from_strip)
		else {
			double diffw[] = new double[2];
			diffw[0] = w[1][0] - w[0][0];
			diffw[1] = w[1][1] - w[0][1];
			argw[0] = Math.atan2(diffw[1], diffw[0]);
			for (i = 1; i < n; i++) {
				argw[i] = argw[i-1] - Math.PI*beta[i];
			}
			w2 = w;
			z2 = z;
			beta2 = beta;
			qdat2 = qdat;
		} // else
		
		// Express each side in a form to allow detection of intersections.
		boolean infty[] = new boolean[n];
		for (i = 0; i < n; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				infty[i] = true;
			}
		} // for (i = 0; i < n; i++)
		int fwd[] = new int[n];
		for (i = 0; i < n-1; i++) {
			fwd[i] = i+1;
		}
		fwd[n-1] = 0;
		double anchor[][] = new double[n][2];
		for (i = 0; i < n; i++) {
			if (!infty[i]) {
				anchor[i][0] = w[i][0];
				anchor[i][1] = w[i][1];
			}
			else {
				anchor[i][0] = w[fwd[i]][0];
				anchor[i][1] = w[fwd[i]][1];  // Use the finite endpoint
			}
		} // for (i = 0; i < n; i++)
		double direcn[][] = new double[n][2];
		for (i = 0; i < n; i++) {
			direcn[i][0] = Math.cos(argw[i]);
			direcn[i][1] = Math.sin(argw[i]);
			if (infty[i]) {
				// reverse
				direcn[i][0] = -direcn[i][0];
				direcn[i][1] = -direcn[i][1];
			}
		} // for (i = 0; i < n; i++)
		double len[] = new double[n];
		for (i = 0; i < n; i++) {
			len[i] = zabs(w[fwd[i]][0] - w[i][0], w[fwd[i]][1] - w[i][1]);
		}
		
		double argz[] = new double[n];
		if (from_disk) {
			for (i = 0; i < n; i++) {
				argz[i] = Math.atan2(z[i][1], z[i][0]);
				if (argz[i] <= 0) {
					argz[i] = argz[i] + 2.0 * Math.PI;
				}
			}
		} // if (from_disk)
		
		double L[] = null;
		if (from_rect) {
			// Extra argument given
			L = qdat[0];
			qdat2 = aux;
		}
		
		double factor = 0.5;  // images of midpoints of preverts
		boolean done[] = new boolean[wp.length];
		int m = wp.length;
		double wpnotdone[][];
		int iter = 0;
		double tol;
		if (qdat2.length > 1) {
		    tol = 1000 * Math.pow(10.0, -qdat2.length);	
		}
		else {
			tol = qdat2[0][0];
		}
		
		double zbase[][] = new double[n][2];
		double wbase[][] = new double[n][2];
		for (i = 0; i < n; i++) {
			zbase[i][0] = Double.NaN;
			wbase[i][0] = Double.NaN;
		}
		int idx[] = null;
		boolean active[] = new boolean[done.length];
		int numactive;
		double A[][] = new double[2][2];
		double ACopy[][] = new double[2][2];
		double dif[] = new double[2];
		double Ainv[][] = new double[2][2];
		double s[] = new double[2];
		RandomNumberGen randomGen = new RandomNumberGen();
		while (m > 0) {  // while some not done
			// Choose a "basis point" on each side of the polygon.
			for (j = 0; j < n; j++) {
				if (from_disk) {
					if (j < n-1) {
						zbase[j][0] = Math.cos(factor*argz[j] + (1-factor)*argz[j+1]);
						zbase[j][1] = Math.sin(factor*argz[j] + (1-factor)*argz[j+1]);
					}
					else {
						zbase[j][0] = Math.cos(factor*argz[n-1] 
								+ (1-factor)*(2.0*Math.PI + argz[0]));
						zbase[j][1] = Math.sin(factor*argz[n-1] 
								+ (1-factor)*(2.0*Math.PI + argz[0]));
					}
				} // if (from_disk)
				else if (from_hp) {
					if (j < n-2) { // between two finite points
						zbase[j][0] = z2[j][0] + factor*(z2[j+1][0] - z2[j][0]);
						zbase[j][1] = z2[j][1] + factor*(z2[j+1][1] - z2[j][1]);
					}
					else if (j == n-2) { // between x[n-2] & Inf
						if (10.0 >= zabs(z2[n-2][0], z2[n-2][1])) {
							zbase[j][0] = 10.0/factor;
							zbase[j][1] = 0.0;
						}
						else { 
							zbase[j][0] = z2[n-2][0]/factor;
							zbase[j][1] = z2[n-2][1]/factor;
						}
					} // else if (j == n-2)
					else { // between -Inf and x[0]
						if (-10.0 <= zabs(z2[0][0],z2[0][1])) {
							zbase[j][0] = -10.0/factor;
						    zbase[j][1] = 0.0;	
						}
						else {
							zbase[j][0] = z2[0][0]/factor;
							zbase[j][1] = z2[0][1]/factor;
						}
					}
				} // else if (from_hp)
				else if (from_strip) {
				    if (j == 0) {
			    		zbase[j][0] = Math.min(-1, z2[1][0])/factor;
			    		zbase[j][1] = 0.0;
				    } // if (j == 0)
				    else if (j == kinf-1) {
				        zbase[j][0] = Math.max(1,z2[kinf-1][0])/factor;
				        zbase[j][1] = 0.0;
				    } // else if (j == kinf-1)
				    else if (j == kinf) {
				    	zbase[j][0] = Math.max(1,z2[kinf+1][0])/factor;
				    	zbase[j][1] = 1.0;
				    } // else if (j == kinf)
				    else if (j == n-1) {
				    	zbase[j][0] = Math.min(-1,z2[n-1][0])/factor;
				    	zbase[j][1] = 1.0;
				    } // else if (j == n-1)
				    else {
				    	zbase[j][0] = z2[j][0] + factor*(z2[j+1][0] - z2[j][0]);
				    	zbase[j][1] = z2[j][1] + factor*(z2[j+1][1] - z2[j][1]);
				    }
				} // else if (from_strip)
				else if (from_rect) {
					int rem = (j+1)%n;
					zbase[j][0] = z2[j][0] + factor * (z2[rem][0] - z2[j][0]);
					zbase[j][1] = z2[j][1] + factor * (z2[rem][1] - z2[j][1]);
					double maximagz2 = -Double.MAX_VALUE;
					for (i = 0; i < z2.length; i++) {
						if (z2[i][1] > maximagz2) {
							maximagz2 = z2[i][1];
						}
					}
					// Can't use 0 or iK' as basis points.
					if (zabs(zbase[j][0], zbase[j][1]) < 1.0E-4) {
						zbase[j][1] = zbase[j][1] + 0.2;
					}
					else if (zabs(zbase[j][0],zbase[j][1] - maximagz2) < 1.0E-4) {
						zbase[j][1] = zbase[j][1] - 0.2;
					}
				} // else if (from_rect)
				
				// Find images of all the z-plane basis points.
				if (from_rect) {
					double neww[][] = new double[1][2];
					double zpnew[][] = new double[1][2];
					zpnew[0][0] = zbase[j][0];
					zpnew[0][1] = zbase[j][1];
					rmap(neww, zpnew, w2, beta2, z2, c, L, qdat2);
					wbase[j][0] = neww[0][0];
					wbase[j][1] = neww[0][1];
				} // if (from_rect)
				else if (from_disk) {
					double zpnew[][] = new double[1][2];
					zpnew[0][0] = zbase[j][0];
					zpnew[0][1] = zbase[j][1];
					double neww[][] = dmap(zpnew, w2 , beta2, z2, c, qdat2);
					wbase[j][0] = neww[0][0];
					wbase[j][1] = neww[0][1];
				} // else if (from_disk)
				else if (from_hp) {
					double neww[][] = new double[1][2];
					double zpnew[][] = new double[1][2];
					zpnew[0][0] = zbase[j][0];
					zpnew[0][1] = zbase[j][1];
					hpmap(neww, zpnew, w2 , beta2, z2, c, qdat2);
					wbase[j][0] = neww[0][0];
					wbase[j][1] = neww[0][1];
				} // else if (from_hp)
				else if (from_strip) {
					double zpnew[][] = new double[1][2];
					zpnew[0][0] = zbase[j][0];
					zpnew[0][1] = zbase[j][1];
					double neww[][] = stmap(zpnew, w2 , beta2, z2, c, qdat2);
					wbase[j][0] = neww[0][0];
					wbase[j][1] = neww[0][1];
				} // else if (from_strip)
				
				// Project each base point exactly to the nearest polygon side.
				// This is needed to make intersection detection go smoothly in
				// borderline cases.
				zmlt(wbase[j][0] - anchor[j][0], wbase[j][1] - anchor[j][1],
						direcn[j][0], -direcn[j][1], cr, ci);
				double proj = cr[0];
				wbase[j][0] = anchor[j][0] + proj*direcn[j][0];
				wbase[j][1] = anchor[j][1] + proj*direcn[j][1];
			} // from (j = 0; j < n; j++)
			
			if ((idx == null) || (idx.length == 0)) {
				// First time through, assign nearest basis point to each image point.
				double dist[] =  new double[m];
				idx = new int[m];
				wpnotdone = new double[m][2];
				for (i = 0, j = 0; i < done.length; i++) {
					if (!done[i]) {
						wpnotdone[j][0] = wp[i][0];
						wpnotdone[j++][1] = wp[i][1];
					}
				}
				for (i = 0; i < m; i++) {
					dist[i] = Double.MAX_VALUE;
					idx[i] = -1;
					for (j = 0; j < n; j++) {
						double currentDist = zabs(wpnotdone[i][0] - wbase[j][0],
								wpnotdone[i][1] - wbase[j][1]);
					    
						if (currentDist < dist[i]) {
							dist[i] = currentDist;
							idx[i] = j;
						}
					}
				} // for (i = 0; i < m; i++)
			} // if ((idx == null) || (idx.length == 0))
			else {
				// Other times, just change those that failed.
				for (i = 0; i < done.length; i++) {
					if (!done[i]) {
						idx[i] = ((idx[i]+1)%n);
					}
				} // for (i = 0; i < done.length; i++)
			} // else
			for (i = 0; i < done.length; i++) {
				if (!done[i]) {
					z0[i][0] = zbase[idx[i]][0];
					z0[i][1] = zbase[idx[i]][1];
					w0[i][0] = wbase[idx[i]][0];
					w0[i][1] = wbase[idx[i]][1];
				}
			} // for (i = 0; i < done.length; i++)
			// Now, cycle thru basis points
			loopj: for (j = 0; j < n; j++) {
				// Those points who come from basis j and need checking.
				numactive = 0;
				for (i = 0; i < done.length; i++) {
				   active[i] = (idx[i] == j)	&& (!done[i]);
				   if (active[i]) {
					   numactive++;
				   }
				} // for (i = 0; i < done.length; i++)
				if (numactive > 0) {
					// Assume for now that it's good
					for (i = 0; i < done.length; i++) {
						if (active[i]) {
							done[i] = true;
						}
					} // for (i = 0; i < done.length; i++)
					// Test line segment for intersections with other sides.
					// We'll parameterize line segment and polygon side, compute
					// parameters at intersection, and check parameters at intersection.
					for (k = 0; k < n; k++) {
						if (k != j) {
						    A[0][0] = direcn[k][0];
						    ACopy[0][0] = A[0][0];
						    A[1][0] = direcn[k][1];
						    ACopy[1][0] = A[1][0];
						    for (p = 0; p < active.length; p++) {
						    	if (active[p]) {
						    	    dif[0] = w0[p][0] - wp[p][0];
						    	    dif[1] = w0[p][1] - wp[p][1];
						    	    A[0][1] = dif[0];
						    	    ACopy[0][1] = A[0][1];
						    	    A[1][1] = dif[1];
						    	    ACopy[1][1] = A[1][1];
						    	    // Get line segment and side parameters at intersection.
						    	    LinearEquations2 le2 = new LinearEquations2();
						    	    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
						    	    double anorm;
						    	    double rcond[] = new double[1];
						    	    double work[] = new double[2];
						    	    int iwork[] = new int[2];
						    	    int info[] = new int[1];
						    	    int ipiv[] = new int[2];
						    	    anorm = ge.dlange('1', 2, 2, ACopy, 2, work);
						    	    le2.dgetrf(2, 2, ACopy, 2, ipiv, info);
						    	    if (info[0] > 0) {
						    	    	MipavUtil.displayError("In dgetrf factor U is exactly singular");
						    	    }
						    	    le2.dgecon('1', 2, ACopy, 2, anorm, rcond, work, iwork, info);
						    	    // Reciprocal condition number of A in 1-norm.
						    	    if (rcond[0] < eps) {
						    	        // All 4 involved points are collinear
						    	    	zdiv(wp[p][0] - anchor[k][0], wp[p][1] - anchor[k][1],
						    	    			direcn[k][0], direcn[k][1], cr, ci);
						    	    	double wpx = cr[0];
						    	    	zdiv(w0[p][0] - anchor[k][0], w0[p][1] - anchor[k][1],
						    	    			direcn[k][0], direcn[k][1], cr, ci);
						    	    	double w0x = cr[0];
						    	    	if ((wpx*w0x < 0) || (((wpx-len[k])*(w0x-len[k])) < 0)) {
						    	    		// Intersection interior to segment: it's no good
						    	    		done[p] = false;
						    	    	}
						    	    } // if (rcond[0] < eps)
						    	    else {
						    	        dif[0] = w0[p][0] - anchor[k][0];
						    	        dif[1] = w0[p][1] - anchor[k][1];
						    	        double detA = A[0][0]*A[1][1] - A[0][1]*A[1][0];
						    	        Ainv[0][0] = A[1][1]/detA;
						    	        Ainv[0][1] = -A[0][1]/detA;
						    	        Ainv[1][0] = -A[1][0]/detA;
						    	        Ainv[1][1] = A[0][0]/detA;
						    	        s[0] = Ainv[0][0]*dif[0] + Ainv[0][1]*dif[1];
						    	        s[1] = Ainv[1][0]*dif[0] + Ainv[1][1]*dif[1];
						    	        // Intersection occurs interior to side? and segment?
						    	        if ((s[0] >= 0) && (s[0] <= len[k])) {
						    	            if (Math.abs(s[1] - 1) < tol) {
						    	                // Special case: wp[p] is on polygon side k
						    	            	z0[p][0] = zbase[k][0];
						    	            	z0[p][1] = zbase[k][1];
						    	            	w0[p][0] = wbase[k][0];
						    	            	w0[p][1] = wbase[k][1];
						    	            } // if (Math.abs(s[1] - 1) < tol)
						    	            else if (Math.abs(s[1]) < tol) {
						    	                // Happens when two sides are partially coincident
						    	            	// (slit)
						    	            	// Check against normal to that side
						    	            	zmlt(wp[p][0] - w0[p][0], -(wp[p][1] - w0[p][1]),
						    	            			-direcn[k][1], direcn[k][0], cr, ci);
						    	            	if (cr[0] > 0) {
						    	            		// Line segment came from "outside"
						    	            		done[p] = false;
						    	            	} // if (cr[0] > 0)
						    	            } // else if (Math.abs(s[1]) < tol)
					    	            	else if ((s[1] > 0) && (s[1] < 1)) {
					    	            	    // Intersection interior to segment:
					    	            		// it's no good
					    	            		done[p] = false;
					    	            	} // else if ((s[1] > 0) && (s[1] < 1))
						    	        } // if ((s[0] >= 0) && (s[0] <= len[k]))
						    	    } // else
						    	} // if (active[p])
						    } // for (p = 0; p < active.length; p++)
						} // if (k != j)
					} // for (k = 0; k < n; k++)
					
					// Short circuit if everything is finished
					m = 0;
					for (i= 0; i < done.length; i++) {
						if (!done[i]) {
							m++;
						}
					} // for (i= 0; i < done.length; i++)
					if (m == 0) {
					    break loopj;	
					} // if (m == 0)
				} // if (numactive > 0)
			} // for (j = 0; j < n; j++)
			if (iter > 2*n) {
				MipavUtil.displayError("In scimapz0 can't seem to choose starting points.  Supply them manually");
				System.exit(-1);
			}
			else {
				iter = iter + 1;
			}
			// Abandon midpoints
			factor = randomGen.genUniformRandomNum(0.0, 1.0);
		} // while (m > 0)
 	}
	
	public void hpmap(double wp[][], double zp[][], double w[][], double beta[], double z[][], 
			double c[], double qdat[][]) {
		// Schwarz-Christoffel half-plane map
		// hpmap computes the values of the Schwarz-Christoffel half-plane map at the points in
		// vector zp.  The polygon's vertices should be given in w and the arguments z, c, and
		// qdat should be computed by hpparam.  hpmap returns a vector wp the same size as zp.
		// Original MATLAB hpmap routine copyright 1998 by Toby Driscoll.
		int i, j;
		double betan[];
		double zn[][];
		int nqpts;
		double tol;
		double qdat2[][] = null;
		int p;
		double mid[][] = null;
		double cre[] = new double[1];
		double cim[] = new double[1];
		
		if ((zp == null) || (zp.length == 0)) {
			wp = null;
			return;
		}
		int n = w.length;
		betan = new double[n-1];
		zn = new double[n-1][2];
	    for (i = 0; i < n-1; i++) {
	    	betan[i] = beta[i];
	    	zn[i][0] = z[i][0];
	    	zn[i][1] = z[i][1];
	    }
		
		// Quadrature data and error tolerance
		if ((qdat == null) || (qdat.length == 0)) {
		    tol = 1.0E-8;
		    nqpts = 8;
		    qdat2 = new double[nqpts][2*betan.length+2];
			scqdata(qdat2, betan, nqpts);	
		}
		else if (qdat.length == 1) {
			tol = qdat[0][0];
			nqpts = Math.max((int)Math.ceil(-Math.log10(tol)), 8);
			qdat2 = new double[nqpts][2*betan.length+2];
		    scqdata(qdat2, betan, nqpts);
		}
		else {
			tol = Math.pow(10.0, -qdat.length);
			qdat2 = qdat;
		}
		
		p = zp.length;
		for (i = 0; i < p; i++) {
			wp[i][0] = 0.0;
			wp[i][1] = 0.0;
		}
		
		// For each point in zp, find nearest prevertex.
		double dist[] = new double[p];
		for (i = 0; i < p; i++) {
			dist[i] = Double.MAX_VALUE;
		}
		int sing[] = new int[p];  // indices of prevertices
		for (j = 0; j < p; j++) {
			for (i = 0; i < n; i++) {
				double currentDist = zabs(zp[j][0]-z[i][0],zp[j][1]-z[i][1]);
				if (currentDist < dist[j]) {
				    dist[j] = currentDist;
				    sing[j] = i;
				}
			}
		}
		
		// Screen out images of prevertices
		boolean vertex[] = new boolean[p];
		for (i = 0; i < p; i++) {
			vertex[i] = (dist[i] < tol);
		}
		for (i = 0; i < p; i++) {
			if (vertex[i]) {
				wp[i][0] = w[sing[i]][0];
				wp[i][1] = w[sing[i]][1];
			}
		}
		boolean zpinf[] = new boolean[p];
		for (i = 0; i < p; i++) {
			zpinf[i] = (Double.isInfinite(zp[i][0]) || Double.isInfinite(zp[i][1]));
		}
		for (i = 0; i < p; i++) {
			if (zpinf[i]) {
				wp[i][0] = w[n-1][0];
				wp[i][1] = w[n-1][1];
			}
		}
		for (i = 0; i < p; i++) {
			vertex[i] = vertex[i] || zpinf[i];
		}
		
		// "Bad" points are closest to a prevertex of infinity.
		int numinf = 0;
		for (i = 0; i < w.length; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				numinf++;
			}
		}
		int atinf[] = new int[numinf];
		for (i = 0, j = 0; i < w.length; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				atinf[j++] = i;
			}
		}
		boolean member[] = new boolean[p];
		for (i = 0; i < p; i++) {
			for (j = 0; j < numinf; j++) {
				if (sing[i] == atinf[j]) {
					member[i] = true;
				}
			}
		}
		boolean bad[] = new boolean[p];
		int numbad = 0;
		for (i = 0; i < p; i++) {
		    bad[i] = member[i] && (!vertex[i]);
		    if (bad[i]) {
		    	numbad++;
		    }
		}
		
		if (numbad > 0) {
			// Can't integrate starting at pre-infinity: which neighboring
			// prevertex to use;
			double direcn;
			mid = new double[numbad][2];
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					direcn = zp[i][0] - z[sing[i]][0];
					if (direcn >= 0) {
						sing[i] = sing[i] + 1;
					}
					else {
						sing[i] = sing[i] - 1;
					}
					// Midpoints of these integrations
					mid[j][0] = (z[sing[i]][0] + zp[i][0])/2;
					mid[j++][1] = (z[sing[i]][1] + zp[i][1])/2;
				}
			}
		} // if (numbad > 0)
		
		// za = the starting singularities
		double zs[][] = new double[p][2];
		double ws[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			j = sing[i];
			zs[i][0] = z[j][0];
			zs[i][1] = z[j][1];
			ws[i][0] = w[j][0];
			ws[i][1] = w[j][1];
		}
		
		// Compute the map directly at "normal" ppoints
		boolean normal[] = new boolean[p];
		int numnormal = 0;
		for (i = 0; i < p; i++) {
			normal[i] = (!bad[i]) && (!vertex[i]);
			if (normal[i]) {
				numnormal++;
			}
		}
		if (numnormal > 0) {
			double zsnormal[][] = new double[numnormal][2];
			double zpnormal[][] = new double[numnormal][2];
			int singnormal[] = new int[numnormal];
			for (i = 0, j = 0; i < p; i++) {
				if (normal[i]) {
					zsnormal[j][0] = zs[i][0];
					zsnormal[j][1] = zs[i][1];
					zpnormal[j][0] = zp[i][0];
					zpnormal[j][1] = zp[i][1];
					singnormal[j++] = sing[i];
				}
			}
			double I[][] = hpquad(zsnormal, zpnormal, singnormal, zn, betan, qdat);
			double cI[][] = new double[numnormal][2];
			for (i = 0; i < numnormal; i++) {
				zmlt(c[0], c[1], I[i][0], I[i][1], cre, cim);
				cI[i][0] = cre[0];
				cI[i][1] = cim[0];
			}
			for (i = 0, j = 0; i < p; i++) {
				if (normal[i]) {
					wp[i][0] = ws[i][0] + cI[j][0];
					wp[i][1] = ws[i][1] + cI[j++][1];
				}
			}
		} // if (numnormal > 0)
		
		// Compute map at "bad" points, in stages.  Stop at midpoint to avoid
		// integration where right endpoint is close to a singularity.
		if (numbad > 0) {
			double zsbad[][] = new double[numbad][2];
			int singbad[] = new int[numbad];
			double zpbad[][] = new double[numbad][2];
			int singnone[] = new int[numbad];
			for (i = 0; i < p; i++) {
				if (bad[i]) {
					zsbad[j][0] = zs[i][0];
					zsbad[j][1] = zs[i][1];
					singbad[j] = sing[i];
					zpbad[j][0] = zp[i][0];
					zpbad[j][1] = zp[i][1];
					singnone[j++] = -1;
				}
			}
			double I1[][] = hpquad(zsbad, mid, singbad, zn, betan, qdat);
			double I2[][] = hpquad(zpbad, mid, singnone, zn, betan, qdat);
			double I[][] = new double[numbad][2];
			for (i = 0; i < numbad; i++) {
				I[i][0] = I1[i][0] - I2[i][0];
				I[i][1] = I1[i][1] - I2[i][1];
			}
			double cI[][] = new double[numbad][2];
			for (i = 0; i < numbad; i++) {
				zmlt(c[0], c[1], I[i][0], I[i][1], cre, cim);
				cI[i][0] = cre[0];
				cI[i][1] = cim[0];
			}
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					wp[i][0] = ws[i][0] + cI[j][0];
					wp[i][1] = ws[i][1] + cI[j++][1];
				}
			}
		} // if (numbad > 0)
	}
	
	public double[][] hpquad(double z1[][], double z2[][], int sing1[], double z[][], double beta[], double qdat[][]) {
		// Numerical quadrature for the half-plane map.
		// z1, z2 are vectors of left and right endpoints. sing1 is a vector of
		// integer indices which label the singularities in z1. So if sing1[5] =
		// 3,
		// then z1[]5 = z[3],. A -1 means no singularity. A is the vector of
		// finite
		// singularities; beta is the vector of associated turning angles. qdata
		// is
		// quadrature data from scqdata.

		// hpquad integrates from a possible singularity at the left end to a
		// regular point at the right. If both endpoints are singularities,
		// you must break the integral into two pieces and make two calls, or
		// call hpquad(z1,z2,sing1,sing2,z,beta,qdat) and accept an automatic
		// choice.

		// The integral is subdivided, if necessary, so that no singularity
		// lies closer to the left endpoint than 1/2 the length of the
		// integration (sub)interval.

		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j, k, m;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int nqpts = qdat.length;
		// Note: Here n is the total number of *finite* singularities; i.e., the
		// number of terms in the product appearing in the integrand.
		int n = z.length;
		double bigz[][][] = new double[n][nqpts][2];
		for (i = 0; i < n; i++) {
			for (j = 0; j < nqpts; j++) {
				bigz[i][j][0] = z[i][0];
				bigz[i][j][1] = z[i][1];
			}
		}
		double bigbeta[][] = new double[beta.length][nqpts];
		for (i = 0; i < beta.length; i++) {
			for (j = 0; j < nqpts; j++) {
				bigbeta[i][j] = beta[i];
			}
		} // for (i = 0; i < beta.length; i++)
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		} // if ((sing1 == null) || (sing1.length == 0))
		double I[][] = new double[z1.length][2];
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numnontriv++;
			}
		} // for (i = 0; i < z1.length; i++)
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				nontriv[j++] = i;
			}
		} // for (i = 0, j = 0; i < z1.length; i++)

		double za[] = new double[2];
		double zb[] = new double[2];
		double nd[][] = new double[nqpts][2];
		double wt[][] = new double[nqpts][2];
		double terms[][][] = new double[n][nqpts][2];
		double zr[] = new double[2];
		double zl[] = new double[2];
		double logterms[] = new double[2];
		double prod[] = new double[2];
		double expSum[] = new double[2];
		double expTerm;
		double termsr;
		double termsi;

		for (i = 0; i < nontriv.length; i++) {
			k = nontriv[i];
			za[0] = z1[k][0];
			za[1] = z1[k][1];
			zb[0] = z2[k][0];
			zb[1] = z2[k][1];
			int sng = sing1[k];

			// Allowable integration step, based on nearest singularity.
			double dist = 1.0;
			double denom = zabs(zb[0] - za[0], zb[1] - za[1]);
			double minVal = Double.MAX_VALUE;
			double absDiff;
			for (j = 0; j <= sng - 1; j++) {
				absDiff = zabs(z[j][0] - za[0], z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}
			} // for (j = 0; j <=sng-1; j++)
			for (j = sng + 1; j <= n - 1; j++) {
				absDiff = zabs(z[j][0] - za[0], z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}
			} // for (j = sng+1; j <= n-1; j++)
			minVal = 2.0 * minVal / denom;
			if (minVal < dist) {
				dist = minVal;
			}
			zr[0] = za[0] + dist * (zb[0] - za[0]);
			zr[1] = za[1] + dist * (zb[1] - za[1]);
			// Adjust Gauss-Jacobi nodes and weights to interval.
			int ind = (sng + n + 1) % (n + 1);
			for (j = 0; j < nqpts; j++) {
				nd[j][0] = ((zr[0] - za[0]) * qdat[j][ind] + zr[0] + za[0]) / 2.0; // G-J
																					// nodes
				nd[j][1] = ((zr[1] - za[1]) * qdat[j][ind] + zr[1] + za[1]) / 2.0;
				wt[j][0] = ((zr[0] - za[0]) / 2.0) * qdat[j][ind + n + 1];
				wt[j][1] = ((zr[1] - za[1]) / 2.0) * qdat[j][ind + n + 1];// G-J
																			// weights
			} // for (j = 0; j < nqpts; j++)
			int zeroterms = 0;
			for (j = 0; j < n; j++) {
				for (m = 0; m < nqpts; m++) {
					terms[j][m][0] = nd[m][0] - bigz[j][m][0];
					terms[j][m][1] = nd[m][1] - bigz[j][m][1];
					if ((terms[j][m][0] == 0) && (terms[j][m][1] == 0)) {
						zeroterms++;
					}
				}
			} // for (j = 0; j < n; j++)
			if (zeroterms > 0) {
				// Endpoints are practically coincident
				I[k][0] = 0;
				I[k][1] = 0;
			} else {
				// Use Gauss-Jacobi on first subinterval, if necessary.
				if (sng >= 0) {
					double fac = zabs(zr[0] - za[0], zr[1] - za[1]) / 2.0;
					double fac2 = Math.pow(fac, beta[sng]);
					for (m = 0; m < nqpts; m++) {
						denom = zabs(terms[sng][m][0], terms[sng][m][1]);
						terms[sng][m][0] = terms[sng][m][0] / denom;
						terms[sng][m][1] = terms[sng][m][1] / denom;
						wt[m][0] = wt[m][0] * fac2;
						wt[m][1] = wt[m][1] * fac2;
					}
				} // if (sng >= 0)
				I[k][0] = 0;
				I[k][1] = 0;
				for (m = 0; m < nqpts; m++) {
					expSum[0] = 0;
					expSum[1] = 0;
					for (j = 0; j < n; j++) {
						logterms[0] = Math.log(zabs(terms[j][m][0], terms[j][m][1]));
						logterms[1] = Math.atan2(terms[j][m][1], terms[j][m][0]);
						prod[0] = logterms[0] * bigbeta[j][m];
						prod[1] = logterms[1] * bigbeta[j][m];
						expSum[0] += prod[0];
						expSum[1] += prod[1];
					} // for (j = 0; j < n; j++)
					expTerm = Math.exp(expSum[0]);
					zmlt(expTerm * Math.cos(expSum[1]), expTerm * Math.sin(expSum[1]), wt[m][0], wt[m][1], cr, ci);
					I[k][0] += cr[0];
					I[k][1] += ci[0];
				} // for (m = 0; m < nqpts; m++)
				while (dist < 1) {
					// Do regular Gaussian quad on other subintervals.
					zl[0] = zr[0];
					zl[1] = zr[1];
					dist = 1.0;
					minVal = Double.MAX_VALUE;
					denom = zabs(zl[0] - zb[0], zl[1] - zb[1]);
					for (j = 0; j < n; j++) {
						double num = zabs(z[j][0] - zl[0], z[j][1] - zl[1]);
						if (num < minVal) {
							minVal = num;
						}
					} // (j = 0; j < n; j++)
					minVal = 2.0 * minVal / denom;
					if (minVal < dist) {
						dist = minVal;
					}
					zr[0] = zl[0] + dist * (zb[0] - zl[0]);
					zr[1] = zl[1] + dist * (zb[1] - zl[1]);
					for (j = 0; j < nqpts; j++) {
						nd[j][0] = ((zr[0] - zl[0]) * qdat[j][n] + zr[0] + zl[0]) / 2.0;
						nd[j][1] = ((zr[1] - zl[1]) * qdat[j][n] + zr[1] + zl[1]) / 2.0;
						wt[j][0] = ((zr[0] - zl[0]) / 2.0) * qdat[j][2 * n + 1];
						wt[j][1] = ((zr[1] - zl[1]) / 2.0) * qdat[j][2 * n + 1];
					} // for (j = 0; j < nqpts; j++)
					for (m = 0; m < nqpts; m++) {
						expSum[0] = 0;
						expSum[1] = 0;
						for (j = 0; j < n; j++) {
							termsr = nd[m][0] - bigz[j][m][0];
							termsi = nd[m][1] - bigz[j][m][1];
							logterms[0] = Math.log(zabs(termsr, termsi));
							logterms[1] = Math.atan2(termsi, termsr);
							prod[0] = logterms[0] * bigbeta[j][m];
							prod[1] = logterms[1] * bigbeta[j][m];
							expSum[0] += prod[0];
							expSum[1] += prod[1];
						}
						expTerm = Math.exp(expSum[0]);
						zmlt(expTerm * Math.cos(expSum[1]), expTerm * Math.sin(expSum[1]), wt[m][0], wt[m][1], cr,
								ci);
						I[k][0] += cr[0];
						I[k][1] += ci[0];
					} // for (m = 0; m < nqpts; m++)
				} // while (dist < 1)
			} // else
		} // for (i = 0; i < nontriv.length; i++)
		return I;
	}
	
	public double[][] stmap(double zp[][], double w[][], double beta[], double z[][],
			double c[], double qdat[][]) {
		// Schwarz-Christoffel strip map.
		// stmap computes the values of the Schwarz-Christoffel strip map at the points
		// in vector zp.  The arguments w, beta, z, c, and qdat are as in vector zp.  stmap
		// returns a vector the same size as zp.
		// Original MATLAB stmap routine copyright 1998 by Toby Driscoll.
		double qdat2[][] = null;
		int i, j, k;
		double tol;
		int nqpts;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double mid1[][] = null;
		double mid2[][] = null;
		
		if ((zp == null) || (zp.length == 0)) {
			return null;
		}
		
		int n = w.length;
		
		// Quadrature data
		if ((qdat == null) || (qdat.length == 0)) {
		    nqpts = 8;
		    qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else if (qdat.length == 1) {
			nqpts = Math.max((int)Math.ceil(-Math.log10(qdat[0][0])), 8);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);
		}
		else {
			qdat2 = qdat;
		}
		tol = Math.pow(10.0,-qdat2.length);
		
		int p = zp.length;
		double wp[][] = new double[p][2];
		
		// For each point in zp, find nearest prevertex.
		double dist[] = new double[p];
		for (i = 0; i < p; i++) {
			dist[i] = Double.MAX_VALUE;
		}
		int sing[] = new int[p];  // indices of prevertices
		for (j = 0; j < p; j++) {
			for (i = 0; i < n; i++) {
				double currentDist = zabs(zp[j][0]-z[i][0],zp[j][1]-z[i][1]);
				if (currentDist < dist[j]) {
				    dist[j] = currentDist;
				    sing[j] = i;
				}
			}
		}
		
		// Screen out images of prevertices
		boolean vertex[] = new boolean[p];
		for (i = 0; i < p; i++) {
			vertex[i] = (dist[i] < tol);
		}
		for (i = 0; i < p; i++) {
			if (vertex[i]) {
				wp[i][0] = w[sing[i]][0];
				wp[i][1] = w[sing[i]][1];
			}
		}
		int numleftend = 0;
		for (i = 0; i < p; i++) {
			if (Double.isInfinite(zp[i][0]) && (zp[i][0] < 0.0)) {
				numleftend++;
			}
		}
		int leftend[] = new int[numleftend];
		for (i = 0, j = 0; i < p; i++) {
			if (Double.isInfinite(zp[i][0]) && (zp[i][0] < 0.0)) {
				leftend[j++] = i;
			}
		}
		for (i = 0, j = 0; i < n; i++) {
		    if (Double.isInfinite(z[i][0]) && (z[i][0] < 0.0) && (numleftend > j)) {
		    	wp[leftend[j]][0] = w[i][0];
		    	wp[leftend[j++]][1] = w[i][1];
		    }
		}
		int numrightend = 0;
		for (i = 0; i < p; i++) {
			if (Double.isInfinite(zp[i][0]) && (zp[i][0] > 0.0)) {
				numrightend++;
			}
		}
		int rightend[] = new int[numrightend];
		for (i = 0, j = 0; i < p; i++) {
			if (Double.isInfinite(zp[i][0]) && (zp[i][0] > 0.0)) {
				rightend[j++] = i;
			}
		}
		for (i = 0, j = 0; i < n; i++) {
		    if (Double.isInfinite(z[i][0]) && (z[i][0] > 0.0) && (numrightend > j)) {
		    	wp[rightend[j]][0] = w[i][0];
		    	wp[rightend[j++]][1] = w[i][1];
		    }
		}
		for (i = 0; i < numleftend; i++) {
			vertex[leftend[i]] = true;
		}
		for (i = 0; i < numrightend; i++) {
			vertex[rightend[i]] = true;
		}
		
		// "Bad" points are closest to a prevertex of inifinty
		int numatinf = 0;
		for (i = 0; i < n; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				numatinf++;
			}
		}
		int atinf[] = new int[numatinf];
		for (i = 0, j = 0; i < n; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				atinf[j++] = i;
			}
		}
		boolean member[] = new boolean[p];
		for (i = 0; i < p; i++) {
			for (j = 0; j < numatinf; j++) {
				if (sing[i] == atinf[j]) {
					member[i] = true;
				}
			}
		}
		boolean bad[] = new boolean[p];
		int numbad = 0;
		for (i = 0; i < p; i++) {
		    bad[i] = member[i] && (!vertex[i]);
		    if (bad[i]) {
		    	numbad++;
		    }
		}
		
		if (numbad > 0) {
			// We can't begin integrations at a preimage of infinity.  We will pick the
			// next-closest qualified prevertex.
			double zf[][] = new double[z.length][2];
			for (i = 0; i < z.length; i++) {
				zf[i][0] = z[i][0];
				zf[i][1] = z[i][1];
			}
			for (i = 0; i < n; i++) {
				if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
					zf[i][0] = Double.POSITIVE_INFINITY;
					zf[i][1] = 0.0;
				}
			}
			double tmp[] = new double[numbad];
			for (i = 0; i < numbad; i++) {
				tmp[i] = Double.MAX_VALUE;
			}
			int s[] = new int[numbad];  // indices of prevertices
			k = 0;
			for (j = 0; j < p; j++) {
				if (bad[j]) {
					for (i = 0; i < n; i++) {
						double currentTmp = zabs(zp[j][0]-zf[i][0],zp[j][1]-zf[i][1]);
						if (currentTmp < tmp[k]) {
						    tmp[k] = currentTmp;
						    s[k] = i;
						}
					}
					k++;
				} // if (bad[j])
			} // for (j = 0; j < p; j++)
			
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					sing[i] = s[j++];
				}
			}
			
			// Because we no longer integrate form the closest prevertex, we must go in
			// stages to maintain accuracy.
			mid1 = new double[numbad][2];
			for (i = 0; i <  numbad; i++) {
				mid1[i][0] = z[s[i]][0];
				mid1[i][1] = 0.5;
			}
			mid2 = new double[numbad][2];
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					mid2[j][0] = zp[i][0];
					mid2[j++][1] = 0.5;
				}
			}
		} // if (numbad > 0)
		else {
			bad = new boolean[p]; // all clear
		}
		
		// zs = the starting singularities
		double zs[][] = new double[p][2];
		double ws[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			zs[i][0] = z[sing[i]][0];
			zs[i][1] = z[sing[i]][1];
			// ws = map(zs)
			ws[i][0] = w[sing[i]][0];
			ws[i][1] = w[sing[i]][1];
		}
		
		// Compute the map directly at "normal" points
		boolean normal[] = new boolean[p];
		int numnormal = 0;
		for (i = 0; i < p; i++) {
			normal[i] = (!bad[i]) && (!vertex[i]);
			if (normal[i]) {
				numnormal++;
			}
		}
		if (numnormal > 0) {
			double zsnormal[][] = new double[numnormal][2];
			double zpnormal[][] = new double[numnormal][2];
			int singnormal[] = new int[numnormal];
			for (i = 0, j = 0; i < p; i++) {
			    if (normal[i]) {
			        zsnormal[j][0] = zs[i][0];
			        zsnormal[j][1] = zs[i][1];
			        zpnormal[j][0] = zp[i][0];
			        zpnormal[j][1] = zp[i][1];
			        singnormal[j++] = sing[i];
			    }
			}
			double I[][] = stquad(zsnormal, zpnormal, singnormal, z, beta, qdat2);
			for (i = 0, j = 0; i < p; i++) {
				if (normal[i]) {
				    zmlt(c[0], c[1], I[j][0], I[j][1], cr, ci);
				    j++;
				    wp[i][0] = ws[i][0] + cr[0];
				    wp[i][1] = ws[i][1] + ci[0];
				}
			}
		} // if (numnormal > 0)
		
		// Compute map at "bad" points in stages.
		if (numbad > 0) {
			double zsbad[][] = new double[numbad][2];
			int singbad[] = new int[numbad];
			int neg1bad[] = new int[numbad];
			for (i = 0; i < numbad; i++) {
				neg1bad[i] = -1;
			}
			double zpbad[][] = new double[numbad][2];
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					zsbad[j][0] = zs[i][0];
					zsbad[j][1] = zs[i][1];
					singbad[j] = sing[i];
					zpbad[j][0] = zp[i][0];
					zpbad[j++][1] = zp[i][1];
				}
			}
			double I1[][] = stquad(zsbad, mid1, singbad, z, beta, qdat2);
			double I2[][] = stquadh(mid1, mid2, neg1bad, z, beta, qdat2);
			double I3[][] = stquad(zpbad, mid2, neg1bad, z, beta, qdat2);
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
			        zmlt(c[0], c[1], I1[j][0] + I2[j][0] - I3[j][0], I1[j][1] + I2[j][1] - I3[j][1], cr, ci);
			        j++;
			        wp[i][0] = ws[i][0] + cr[0];
			        wp[i][1] = ws[i][1] + ci[0];
				}
			}
		} // if (numbad > 0)
		return wp;
	}
	
	private double[][] diskeval(scmap M, double zp[][], double tol) {
		// Evaluate the Schwarz-Christoffel disk map at points
		// diskeval evaulates the Schwarz-Christoffel map M at the points zp
		// in the unit disk.  diskeval atempts to five an answer accurate to 
		// tol.  If tol is less than the accuracy of M, this is unlikely
		// to be met.
		// Original MATLAB eval routine copyright 1998 by Toby Driscoll.
		int i, j;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		double wp[][] = new double[zp.length][2];
		for (i = 0; i < zp.length; i++) {
			wp[i][0] = Double.NaN;
		}
		boolean idx[] = new boolean[zp.length];
		int numidx = 0;
		for (i = 0; i < zp.length; i++) {
			if (zabs(zp[i][0], zp[i][1]) <= 1.0 + eps) {
				idx[i] = true;
				numidx++;
			}
		}
		double zpin[][] = new double[numidx][2];
		for (i = 0, j = 0; i < zp.length; i++) {
			if (idx[i]) {
				zpin[j][0] = zp[i][0];
				zpin[j++][1] = zp[i][1];
			}
		}
		double wpidx[][] = dmap(zpin, w, beta, M.prevertex, M.constant, M.qdata);
		for (i = 0, j = 0; i < zp.length; i++) {
			if (idx[i]) {
				wp[i][0] = wpidx[j][0];
				wp[i][1] = wpidx[j++][1];
			}
		}
		return wp;
	}
	
	private double[][] demap(double zp[][], double w[][], double beta[],
			double z[][], double c[], double qdat[][]) {
		// Schwarz-Christoffel disk map.
		// demap computes the values of the Schwarz-Christoffel exterior map at the
		// points in the vector zp.  The arguments w, beta, z, c, and qdat are
		// as in deparam.  demap returns a vector wp the same size as zp.
		
		// Original MATLAB demap routine copyright 1998 by Toby Driscoll.
		int i, j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double tol;
		if ((zp == null) || (zp.length == 0)) {
			return null;
		}
		
		int n = w.length;
		
		// Quadrature data and error tolerance
		if ((qdat == null) || (qdat.length == 0)) {
		    tol =- 1.0e-8;  
		    qdat = new double[8][2*beta.length+2];
			scqdata(qdat, beta, 8);
		}
		else {
			tol = Math.pow(10.0, -qdat.length);
		}
		//int shape = zp.length;
		int p = zp.length;
		double wp[][] = new double[p][2];
		
		// For each point in zp, find the nearest prevertex
		double dist[] = new double[p];
		int sing[] = new int[p]; // Indices of prevertices
		for (i = 0; i < p; i++) {
			dist[i] = Double.MAX_VALUE;
			sing[i] = -1;
			for (j = 0; j < n; j++) {
				double currentDist = zabs(zp[i][0] - z[j][0],zp[i][1] - z[j][1]);
				if (currentDist < dist[i]) {
					dist[i] = currentDist;
					sing[i] = j;
				}
			}
		} // for (i = 0; i < p; i++)
		
		// Screen out images of prevertices
		boolean vertex[] = new boolean[p];
		boolean zero[] = new boolean[p];
		for (i = 0; i < p; i++) {
			if (dist[i] < tol) {
				vertex[i] = true;
				wp[i][0] = w[sing[i]][0];
				wp[i][1] = w[sing[i]][1];
			}
			if (zabs(zp[i][0],zp[i][1]) < tol) {
			    zero[i] = true;
			    wp[i][0] = Double.POSITIVE_INFINITY;
			    wp[i][1] = 0.0;
			}
			vertex[i] = vertex[i] || zero[i];
		} // for (i  = 0; i < p; i++)
		
		// zs = the starting singularities
		double zs[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			zs[i][0] = z[sing[i]][0];
			zs[i][1] = z[sing[i]][1];
			if (!vertex[i]) {
				wp[i][0] = w[sing[i]][0];
				wp[i][1] = w[sing[i]][1];
			}
		}
		
		// Must be careful about the singularity at the origin, since the
		// quadrature routine doesn't pay attention to the right endpoint.
		
		// dist to sing at zero
		double abszp[] = new double[p];
		for (i = 0; i < p; i++) {
			abszp[i] = zabs(zp[i][0], zp[i][1]);
		}
		
		// Integrate for the rest, using vectorization with masking.
		// Unfinished cases
		boolean unf[] = new boolean[p];
		int numunf = 0;
		for (i = 0; i < p; i++) {
			unf[i] = !vertex[i];
			if (unf[i]) {
				numunf++;
			}
		}
		// Integration starting points
		double zold[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			zold[i][0] = zs[i][0];
			zold[i][1] = zs[i][1];
		}
		// Initial conditions
		double znew[][] = new double[p][2];
        for (i = 0; i < p; i++) {
        	znew[i][0] = zold[i][0];
        	znew[i][1] = zold[i][1];
        }
        dist = new double[p];
        for (i = 0; i < p; i++) {
        	dist[i] = 1.0;
        }
        
        while (numunf > 0) {
        	double znewunf[][] = new double[numunf][2];
    		double zoldunf[][] = new double[numunf][2];
    		double wpunf[][] = new double[numunf][2];
    		int singunf[] = new int[numunf];
        	// How far can the integration go?
        	for (i = 0, j = 0; i < p; i++) {
        		
        		if (unf[i]) {
        			double denom = zabs(zp[i][0] - zold[i][0], zp[i][1] - zold[i][1]);
        			dist[i] = Math.min(1.0, 2.0*abszp[i]/denom);
        			// New integration endpoints
        			znew[i][0] = zold[i][0] + dist[i] * (zp[i][0] - zold[i][0]);
        			znew[i][1] = zold[i][1] + dist[i] * (zp[i][1] - zold[i][1]);
        			znewunf[j][0] = znew[i][0];
        			znewunf[j][1] = znew[i][1];
        			zoldunf[j][0] = zold[i][0];
        			zoldunf[j][1] = zold[i][1];
        			wpunf[j][0] = wp[i][0];
        			wpunf[j][1] = wp[i][1];
        			singunf[j++] = sing[i];
        		}
        	} // for (i = 0, j = 0; i < p; i++)
        	double ans[][] = dequad(zoldunf, znewunf, singunf, z, beta, qdat);
        	for (i = 0, j = 0; i < p; i++) {
        		if (unf[i]) {
        			zmlt(c[0], c[1], ans[j][0], ans[j][1], cr, ci);
        			j++;
        			wp[i][0] = wp[i][0] + cr[0];
        			wp[i][1] = wp[i][1] + ci[0];
        		}
        	} // for (i = 0, j = 0; i < p; i++)
        	numunf = 0;
        	for (i = 0; i < p; i++) {
        	    unf[i] = (dist[i] < 1);
        	    if (unf[i]) {
        	    	numunf++;
        	    	zold[i][0] = znew[i][0];
        	    	zold[i][1] = znew[i][1];
        	    }
        	}
        	// Only first step can have endpoint singularity
        	for (i = 0; i < p; i++) {
        		sing[i] = -1;
        	}
        } // while (numunf > 0)
		
		return wp;
	}
	
	private double[][] dmap(double zp[][], double w[][], double beta[],
			double z[][], double c[], double qdat[][]) {
		// Schwarz-Christoffel disk map.
		// dmap computes the values of the Schwarz-Christoffel disk map at the
		// points in the vector zp.  The arguments w, beta, z, c, and qdat are
		// as in dparam.  dmap returns a vector wp the same size as zp.
		
		// Original MATLAB dmap routine copyright 1998 by Toby Driscoll.
		int i, j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double tol;
		if ((zp == null) || (zp.length == 0)) {
			return null;
		}
		
		int n = z.length;
		
		// Quadrature data and error tolerance
		if ((qdat == null) || (qdat.length == 0)) {
		    tol =- 1.0e-8;  
		    qdat = new double[8][2*beta.length+2];
			scqdata(qdat, beta, 8);
		}
		else {
			tol = Math.pow(10.0, -qdat.length);
		}
		//int shape = zp.length;
		int p = zp.length;
		double wp[][] = new double[p][2];
		
		// For each point in zp, find the nearest prevertex
		double dist[] = new double[p];
		int sing[] = new int[p]; // Indices of prevertices
		for (i = 0; i < p; i++) {
			dist[i] = Double.MAX_VALUE;
			sing[i] = -1;
			for (j = 0; j < n; j++) {
				double currentDist = zabs(zp[i][0] - z[j][0],zp[i][1] - z[j][1]);
				if (currentDist < dist[i]) {
					dist[i] = currentDist;
					sing[i] = j;
				}
			}
		} // for (i = 0; i < p; i++)
		
		// Screen out images of prevertices
		boolean vertex[] = new boolean[p];
		for (i = 0; i < p; i++) {
			if (dist[i] < tol) {
				vertex[i] = true;
				wp[i][0] = w[sing[i]][0];
				wp[i][1] = w[sing[i]][1];
			}
		} // for (i = 0; i < p; i++)
		
		// "Bad" points are closest to a prevertex of infinity.
		boolean atinf[] = new boolean[n];
		for (i = 0; i < n; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				atinf[i] = true;
			}
		} // for (i = 0; i < w.length; i++)
		
		boolean bad[] = new boolean[p];
		int numbad = 0;
		for (i = 0; i < p; i++) {
			if ((atinf[sing[i]]) && (!vertex[i])) {
				bad[i] = true;
				numbad++;
			}
		} // for (i = 0; i < p; i++)
		
		double wc[] = new double[2];
		if (numbad > 0) {
			// Can't integrate starting at pre-infinity: find conformal center to use
			// as integration basis.
			double z1[][] = new double[1][2];
			double z2[][] = new double[1][2];
			int sing1[] = new int[1];
			double I1[][];
			if ((!Double.isInfinite(w[n-2][0])) && (!Double.isInfinite(w[n-2][1]))) {
			    z1[0][0] = z[n-2][0];
			    z1[0][1] = z[n-2][1];   
			    sing1[0] = n-2;
			    I1 = dquad(z1,z2,sing1, z, beta, qdat);
			    zmlt(c[0], c[1], I1[0][0], I1[0][1], cr, ci);
			    wc[0] = w[n-2][0] + cr[0];
			    wc[1] = w[n-2][1] + ci[0];
			}
			else {
				z1[0][0] = z[n-1][0];
			    z1[0][1] = z[n-1][1];   
			    sing1[0] = n-1;
			    I1 = dquad(z1,z2,sing1, z, beta, qdat);
			    zmlt(c[0], c[1], I1[0][0], I1[0][1], cr, ci);
			    wc[0] = w[n-1][0] + cr[0];
			    wc[1] = w[n-1][1] + ci[0];	
			}
		} // if (numbad > 0)
		
		// zs = the starting singularities
		double zs[][] = new double[p][2];
		double ws[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			zs[i][0] = z[sing[i]][0];
			zs[i][1] = z[sing[i]][1];
			ws[i][0] = w[sing[i]][0];
			ws[i][1] = w[sing[i]][1];
		}
		
		// Compute the map directly at "normal" points.
		boolean normal[] = new boolean[p];
		int numnormal = 0;
		for (i = 0; i < p; i++) {
			normal[i] = (!bad[i]) && (!vertex[i]);
			if (normal[i]) {
				numnormal++;
			}
		}
		double I[][];
		if (numnormal > 0) {
		    double zsnormal[][] = new double[numnormal][2];
		    double zpnormal[][] = new double[numnormal][2];
		    int singnormal[] = new int[numnormal];
		    for (i = 0, j = 0; i < p; i++) {
		    	if (normal[i]) {
		    		zsnormal[j][0] = zs[i][0];
		    		zsnormal[j][1] = zs[i][1];
		    		zpnormal[j][0] = zp[i][0];
		    		zpnormal[j][1] = zp[i][1];
		    		singnormal[j++] = sing[i];
		    	}
		    } // for (i = 0, j = 0; i < p; i++)
		    I = dquad(zsnormal, zpnormal, singnormal, z,beta, qdat);
		    for (i = 0, j = 0; i < p; i++) {
		    	if (normal[i]) {
		    	    zmlt(c[0], c[1], I[j][0], I[j][1], cr, ci);
		    	    j++;
		    	    wp[i][0] = ws[i][0] + cr[0];
		    	    wp[i][1] = ws[i][1] + ci[0];
		    	}
		    } // for (i = 0, j = 0; i < p; i++)
		} // if (numnormal > 0)
		
		// Compute map at "bad" points, using conformal center as basis, to avoid
		// integration where right endpoint is too close to a singularity.
		if (numbad > 0) {
	        double zpbad[][] = new double[numbad][2];
	        for (i = 0, j = 0; i < p; i++) {
	        	if (bad[i]) {
	        		zpbad[j][0] = zp[i][0];
	        		zpbad[j++][1] = zp[i][1];
	        	}
	        }
	        double zerosbad[][] = new double[numbad][2];
	        int singbad[] = new int[numbad];
	        for (i = 0; i < numbad; i++) {
	        	singbad[i] = -1;
	        }
	        I = dquad(zpbad, zerosbad, singbad, z, beta, qdat);
	        for (i = 0, j = 0; i < p; i++) {
	        	if (bad[i]) {
	        		zmlt(c[0], c[1], I[j][0], I[j][1], cr, ci);
	        		j++;
	        		wp[i][0] = wc[0] - cr[0];
	        		wp[i][1] = wc[1] - ci[0];
	        	}
	        } // for (i = 0, j = 0; i < p; i++)
		} // if (numbad > 0) 
		return wp;
	}
	
	public scmap crrectmap(double w[][], int corner[], double tolerance) {
	    // Schwarz-Christoffel cross ratio-disk map object.
		// crrectmap constructs a Schwarz-Christoffel crossratio rectified map object
		// for the polygon p.  The parameter problem is solved, using default options,
		// for the crossratios of the prevertices on the disk.  Then you are required
		// to graphically assign angles of the rectified polygon.  These angles determine
		// the rectilinear polygon that is considered the canonical domain.
		// In this MIPAV application the rectilinear polygon will always be taken as
		// a rectangle with alphar having 0.5 at the 4 corners and alphar = 1 elsewhere.
		// Here alphhar is a vector having the same length as p.  Each element is an
		// interior angle of the rectified polygon, normalized by pi.  The only valid 
		// entries are {.5, 1, 1.5. 2}.  Use crrectmap instead of rectmap when the polygon
		// has multiple elongations, or when rectmap fails to converge.
		
		// Original crrectmap routine copyright 1998 by Toby Driscoll.
		int i, j, k;
		// New alphar
		double alphar[] = new double[w.length];
		for (i = 0; i < alphar.length; i++) {
			alphar[i] = 1.0;
		}
		for (i = 0; i < 4; i++) {
			alphar[corner[i]] = 0.5;
		}
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1;
		}
		
		// Find prevertex crossratios
		scmap M = crdiskmap(poly, tolerance, null, null);
		boolean orig[] = M.original;
		poly = M.poly;
		double wcr[][] = poly.vertex;
	    beta = new double[poly.angle.length];
	    for (i = 0; i < poly.angle.length; i++) {
	    	beta[i] = poly.angle[i] - 1.0;
	    }
	    double cr[] = M.crossratio;
	    double aff[][][] = new double[M.affine.length][M.affine[0].length][2];
	    for (i = 0; i < M.affine.length; i++) {
	    	for (j = 0; j < M.affine[0].length; j++) {
	    	    for (k = 0; k < 2; k++) {
	    	    	aff[i][j][k] = M.affine[i][j][k];
	    	    }
	    	}
	    }
	    qlgraph Q = M.qgraph;
	    // Convert alphas to betas, ecpanding if polygon was split
	    double betar[] = new double[wcr.length];
	    for (i = 0, j = 0; i < betar.length; i++) {
	    	if (orig[i]) {
	    	    betar[i] = alphar[j++] - 1.0;	
	    	}
	    }
	    double affr[][][] = new double[betar.length-3][2][2];
	    double wr[][] = new double[wcr.length][2];
	    // Here betar is just an input
	    crrect(wr, affr, wcr, beta, cr, aff, Q, betar, tolerance);
	    
	    alphar = new double[betar.length];
	    for (i = 0; i < betar.length; i++) {
	    	alphar[i] = betar[i] + 1.0;
	    }
	    double xr[] = new double[wr.length];
	    double yr[] = new double[wr.length];
	    for (i = 0; i < wr.length; i++) {
	    	xr[i] = wr[i][0];
	    	yr[i] = wr[i][1];
	    }
	    polygon polyr = new polygon(xr, yr, alphar);
	    M.rectpolygon = polyr;
	    M.rectaffine = affr;
	    int nqpts = (int)Math.ceil(-Math.log10(1.0E-8));
	    M.rectqdata = new double[nqpts][2*betar.length+2];
	    scqdata(M.rectqdata, betar, nqpts);
	    
		return M;
	}
	
	public void crrect(double wr[][], double affr[][][], double w[][],
			double beta[], double cr[], double aff[][][], qlgraph Q, double betar[], double tol) {
		// Here betar is just an input
		// Create a rectified map.
		// In this subset betar is already specified so no graphical
		// procedure takes place.
		// A rectified map is one between a generic polygon and a polygon having
		// all angles as multiples of pi/2.  Once the CR parameter problem is solved,
		// you can define a rectified polygon to map to by specifying the rectified
		// angles.  (Side lengths are not free variables; they are a consequence of
		// the original polygon and the rectified angles.)  There is no unique correct
		// choice of angles.  No automatic selection of the rectified angles is 
		// currently available.
		// 
		// Important note: The rectified polygon may not be embeddable in the plane.
		// That is, it may overlap itself. This may be difficult to see graphically,
		// but it does not affect crrect in any way.
		
		// This is a subset of the original MATLAB crrect 1998 routine copyright by Toby Driscoll.
		
		// betar was supplied just do computation
		int i, j, m;
		int n = w.length;
		//craffine(w, beta, cr, Q, tol) returns aff which is not used
		int k = -1;
		for (i = 0; i < betar.length && k == -1; i++) {
			if (betar[i] < 0) {
				k = i;
			}
		}
		for (i = 0; i < wr.length; i++) {
			wr[i][0] = Double.NaN;
			wr[i][1] = 0.0;
		}
		wr[k][0] = 0;
		wr[(k+1)%n][0] = 1;
		craffine(wr, betar, cr, Q, tol);
		for (i = 0; i < craffine_aff.length; i++) {
			for (j = 0; j < 2; j++) {
				for (m = 0; m < 2; m++) {
					affr[i][j][m] = craffine_aff[i][j][m];
				}
			}
		}
		for (i = 0; i < craffine_wn.length; i++) {
			for (j = 0; j < 2; j++) {
				wr[i][j] = craffine_wn[i][j];
			}
		}
		return;
	}
	
	public scmap rectmap(double w[][], int corner[], double tolerance,
			double z[][], double c[], double L[]) {
		// Schwarz-Christoffel rectangle map object
		// rectmap constructs a Schwarz-Christoffel rectangle map object for the polygon
		// whose vertices are given by w.  corner is a 4 integer vector containing the
		// indices of the vertices that are the images of the rectangle's corners.
		// These indices should be specified in counterclockwise order, and the first
		// two should be the endpoints of a long side of the rectangle.
		// If corner is null, rectmap requires you to choose the corners graphically.
		// If w, z, c, and L are given, rectmap constructs a rectmap using the
		// explicitly given prevertices z[][], constant c[0], and strip length L[0].
		// Original MATLAB routine copyright 1998-2001 by Toby Driscoll.
		int i;
		double qdata[][] = null;
		double wn[][] = null;
		double betan[] = null;
		int cornern[] = null;
		double z0[][] = null;
		//rparam(double z[][], double c[], double L[], double qdat[][], 
				//double w[][], double beta[], int cnr[], double z0[][], double tol)
		// z, c, L, and qdat are outputs
		// w, beta, cnr, z0, and tolerance are inputs
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		if ((z == null) || (z.length == 0)) {
			if ((corner == null) || (corner.length == 0)) {
				String msg[] = new String[2];
		        msg[0] = "Select the images of the corners of the rectangle";
		        msg[1] = "Go in counterclockwise order and select a long rectangle edge first";
		        corner = scselect(w, beta, 4, "Select corners", msg);
			} // if ((corner == null) || (corner.length == 0))
			
			// Apply scfix to enforce solver rules
			// For type "r" wn and betan will stay the same in length or increase by one
			wn = new double[w.length+1][2];
			betan = new double[w.length+1];
			cornern = new int[corner.length];
			// Number of vertices added by scfix
			int verticesAdded[] = new int[1];
			int initialVertices = w.length;
			scfix(wn, betan, verticesAdded, cornern, "r", w, beta, corner);
			double wn2[][];
			double betan2[];
			if (verticesAdded[0] == 0) {
			    wn2 = new double[initialVertices][2];
				betan2 = new double[initialVertices];
				for (i = 0; i < initialVertices; i++) {
					wn2[i][0] = wn[i][0];
					wn2[i][1] = wn[i][1];
					betan2[i] = betan[i];
				}
			}
			else {
				wn2 = wn;
				betan2 = betan;
			}
			double xn[] = new double[wn2.length];
			double yn[] = new double[wn2.length];
			for (i = 0; i < wn2.length; i++) {
				xn[i] = wn2[i][0];
				yn[i] = wn2[i][1];
			}
			double alpha[] = new double[betan2.length];
			for (i = 0; i < betan2.length; i++) {
				alpha[i] = betan2[i] + 1.0;
			}
			poly = new polygon(xn, yn, alpha);
		    L = new double[2];
			c = new double[2];
			z = new double[wn2.length][2];
			int nqpts = Math.max((int)Math.ceil(-Math.log10(tolerance)), 4);
			qdata = new double[nqpts][2*betan2.length+2];
			// Solve parameter problem (always necessary)
		    rparam(z, c, L, qdata, wn2, betan2, cornern, z0, tolerance );
		} // if ((z == null) || (z.length == 0))
		else {
			// Prevertices, etc. given.  Renumber to conform
			corner = new int[4];
			int renum[] = new int[w.length];
			rcorners(w, beta, z, corner, renum);
			int nqpts = (int)Math.ceil(-Math.log10(tolerance));
		    qdata = new double[nqpts][2*beta.length+2];
			scqdata(qdata, beta, nqpts);
			for (i = 0; i < w.length; i++) {
				x[i] = w[i][0];
				y[i] = w[i][1];
			}
			double  alpha[] = new double[beta.length];
			for (i = 0; i < beta.length; i++) {
				alpha[i] = beta[i] + 1.0;
			}
			poly = new polygon(x, y, alpha);
		} // else
		scmap M = new scmap();
		M.prevertex = z;
		M.constant[0] = c[0];
		M.constant[1]= c[1];
		M.stripL = L;
		M.qdata = qdata;
		M.poly = poly;
		
		// Now fill in apparent accuracy
		M.accuracy = rectAccuracy(M);
		return M;
	}
	
	public void crdiskplot(scmap M, double R[], double theta[], int num1draw, int num2draw, 
			boolean drawThetaToRadiusOne, double error[], int yInvert) {
	    // Visualize a Schwarz-Christoffel crossratio disk map.
		// crdiskplot plots the polygon associated with the Schwarz-Christoffel crossratio
		// disk map M and the images of circles and radii under the S-C
		// transformation.  If R.length == 1 and theta.length == 1, R[0] is the
		// number of evenly spaced circles and theta[0] is the number of
		// evenly spaced rays.  If R.length > 1 and theta.length > 1, then the
		// circles are plotted at radii given by the entries of R and rays at
		// the angles specified in theta.
		
		// tolerance normally defaults to max(1.0E-4, M.accuracy)
		
		// From original MATLAB plot routine copyright 1998 by Toby Driscoll
		int i;
		int nqpts;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double cr[] = M.crossratio;
		double aff[][][] = M.affine;
		int quadnum = M.center_fix_quadnum;
		double mt[][] = M.center_fix_mt;
		qlgraph Q = M.qgraph;
		if (R == null) {
			R = new double[1];
			R[0] = 10;
		}
		if (theta == null) {
			theta = new double[1];
			theta[0] = 10;
		}
		if (error != null) {
			nqpts = (int)Math.ceil(-Math.log10(error[0]));
		}
		else {
			nqpts = 5;
		}
		crplot(w, beta, cr, aff, quadnum, mt, Q, R, theta, nqpts, num1draw, num2draw, drawThetaToRadiusOne, yInvert);
		return;
	}
	
	public void exterplot(scmap M, double R[], double theta[], int num1draw, int num2draw, 
			double error[], int yInvert, double axis[]) {
		    // Visualize a Schwarz-Christoffel exterior map.
			// exterplot plots the polygon associated with the Schwarz-Christoffel
			// exterior map and the images of circles and radii under the S-C
			// transformation.  If R.length == 1 and theta.length == 1, R[0] is the
			// number of evenly spaced circles and theta[0] is the number of
			// evenly spaced rays.  If R.length > 1 and theta.length > 1, then the
			// circles are plotted at radii given by the entries of R and rays at
			// the angles specified in theta.
			
			// tolerance normally defaults to max(1.0E-4, M.accuracy)
			
			// From original MATLAB plot routine copyright 1998 by Toby Driscoll
			int i;
			int nqpts;
			polygon p = M.poly;
			double w[][] = new double[p.vertex.length][2];
			for (i = 0; i < p.vertex.length; i++) {
				w[i][0] = p.vertex[p.vertex.length - 1 - i][0];
				w[i][1] = p.vertex[p.vertex.length - 1 - i][1];
			}
			double beta[] = new double[p.angle.length];
			for (i = 0; i < beta.length; i++) {
				beta[i] = 1.0 - p.angle[p.angle.length - 1 - i];
			}
			double z[][] = M.prevertex;
			double c[] = M.constant;
			if (R == null) {
				R = new double[1];
				R[0] = 10;
			}
			if (theta == null) {
				theta = new double[1];
				theta[0] = 10;
			}
			if (error != null) {
				nqpts = (int)Math.ceil(-Math.log10(error[0]));
			}
			else {
				nqpts = 5;
			}
			deplot(w, beta, z, c, R, theta, nqpts, num1draw, num2draw, yInvert, axis);
			return;
		}
	
	
	
	public void diskplot(scmap M, double R[], double theta[], int num1draw, int num2draw, 
		double error[], int yInvert) {
	    // Visualize a Schwarz-Christoffel disk map.
		// diskplot plots the polygon associated with the Schwarz-Christoffel
		// disk map and the images of circles and radii under the S-C
		// transformation.  If R.length == 1 and theta.length == 1, R[0] is the
		// number of evenly spaced circles and theta[0] is the number of
		// evenly spaced rays.  If R.length > 1 and theta.length > 1, then the
		// circles are plotted at radii given by the entries of R and rays at
		// the angles specified in theta.
		
		// tolerance normally defaults to max(1.0E-4, M.accuracy)
		
		// From original MATLAB plot routine copyright 1998 by Toby Driscoll
		int i;
		int nqpts;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		if (R == null) {
			R = new double[1];
			R[0] = 10;
		}
		if (theta == null) {
			theta = new double[1];
			theta[0] = 10;
		}
		if (error != null) {
			nqpts = (int)Math.ceil(-Math.log10(error[0]));
		}
		else {
			nqpts = 5;
		}
		dplot(w, beta, z, c, R, theta, nqpts, num1draw, num2draw, yInvert);
		return;
	}
	
	private void crplot(double w[][], double beta[], double cr[], double aff[][][], int quadnum,
			double mt[][], qlgraph Q, double R[], double theta[], int nqpts, int num1draw, int num2draw, 
			boolean drawThetaToRadiusOne, int yInvert) {
		// Image of polar grid under disk map in crossratio form.
		// crplot will adaptively plot the images under the Schwarz-Christoffel crossratio
		// disk map of circles and rays in the unit disk. 
		// If R.length == 1 and theta.length == 1, R[0] is the
		// number of evenly spaced circles and theta[0] is the number of
		// evenly spaced rays.  If R.length > 1 and theta.length > 1, then the
		// circles are plotted at radii given by the entries of R and rays at
		// the angles specified in theta.
		
		// nqpts Number of quadrature points per integration.
		// Approximately equals -log10(error).  Increase if plot
	    // has false little zigzags in curves. 
		
		// Original MATLAB crplot routine copyright 1998 by Toby Driscoll.
		
		// In original code num1draw = 20 and num2draw = 14
		
		int i;
		int j;
		int m;
		int Rlength;
		int thetalength;
		double spacing;
		double R2[];
		double theta2[] = null;
		boolean drawTheta = true;
		int n = w.length;
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
        // Max allowed number of adaptive refinements made to meet other requirements 
        int maxrefn = 16;
        double axlim[] = new double[4];
        Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		Vector<Double>tpReal = new Vector<Double>();
		Vector<Double>tpImag = new Vector<Double>();
		Vector<Double>RpReal = new Vector<Double>();
		Vector<Double>RpImag = new Vector<Double>();
		Vector<Double>zpReal = new Vector<Double>();
		Vector<Double>zpImag = new Vector<Double>();
		Vector<Double>wpReal = new Vector<Double>();
		Vector<Double>wpImag = new Vector<Double>();
		Vector<Boolean>newlog = new Vector<Boolean>();
		double zp[][];
		double neww[][];
        
     // Empty arguments default to 10.
 		if ((R == null) || (R.length == 0)) {
 			R = new double[1];
 			R[0] = 10;
 		}
 		if ((theta == null) || (theta.length == 0)) {
 			theta = new double[1];
 			theta[0] = 10;
 		}
 		
 		// Integer arguments must be converted to specific values
 		if ((R.length == 1) && (R[0] == Math.round(R[0]))) {
 			Rlength = (int)R[0];
 			R2 = new double[Rlength];
 		    spacing = (1.0/(R[0] + 1.0));
 		    for (i = 1; i <= Rlength; i++) {
 		        R2[i-1] = i*spacing;	
 		    }
 		} // if ((R.length == 1) && (R[0] == Math.round(R[0])))
 		else {
 			R2 = new double[R.length];
 			for (i = 0; i < R.length; i++) {
 				R2[i] = R[i];
 			}
 		}
 		if ((theta.length == 1) && (theta[0] == 0.0)) {
 			drawTheta = false;
 		}
 		else if ((theta.length == 1) && (theta[0] == Math.round(theta[0]))) {
 			thetalength = (int)theta[0];
 			theta2 = new double[thetalength];
 		    spacing = ((2.0*Math.PI)/theta[0]);
 		    for (i = 0; i < thetalength; i++) {
 		        theta2[i] = i*spacing;	
 		    }
 		} // if ((theta.length == 1) && (theta[0] == Math.round(theta[0])))
 		else {
 			theta2 = new double[theta.length];
 			for (i = 0; i < theta.length; i++) {
 				theta2[i] = theta[i];
 			}
 		}
 		
 		float xPointArray[] = new float[n+1];
		float yPointArray[] = new float[n+1];
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, w, beta, false, axlim, yInvert, true, null);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
        double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		// Plot circles...
	    linhx = new Vector[R2.length][2];
		linhy = new Vector[R2.length][2];
		for (i = 0; i < R2.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		for (j = 0; j < R2.length; j++) {
		    // Start with evenly spaced theta
			tpReal.clear();
			tpImag.clear();
			for (i = 0; i < num1draw; i++) {
				tpReal.add(i*(2.0*Math.PI)/(num1draw-1.0));
				tpImag.add(0.0);
			}
			newlog.clear();
			for (i = 0; i < num1draw; i++) {
				newlog.add(true);
			}
			wpReal.clear();
			wpImag.clear();
			for (i = 0; i < num1draw; i++) {
				wpReal.add(Double.NaN);
				wpImag.add(0.0);
			}
			
			// The individual points will be shown as they are found.
			
			// Adaptive refinement to make curve smooth
			int iter = 0;
			while (iter < maxrefn) {
				int numnewlog = 0;
				for (i = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	numnewlog++;
				    }
				} // for (i = 0; i < newlog.size(); i++)
				if (numnewlog == 0) {
					break;
				}
				zp = new double[numnewlog][2];
				for (i = 0, m = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	zp[m][0] = R2[j]*Math.cos(tpReal.get(i));
				    	zp[m++][1] = R2[j]*Math.sin(tpReal.get(i));
				    }
				} // for (i = 0, m = 0; i < newlog.length; i++)
				neww = crmap(zp,w,beta,cr,aff,quadnum, mt, Q, qdat);
				for (i = 0, m = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
					    wpReal.set(i, neww[m][0]);
					    wpImag.set(i, neww[m++][1]);
					} 
				} // for (i = 0, m = 0; i < newlog.size(); i++)
				iter = iter + 1;
				
				linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (i = 0; i < wpReal.size(); i++) {
				    linhx[j][0].add(wpReal.get(i));
				    linhy[j][0].add(wpImag.get(i));
				}
				for (i = 0; i < zp.length; i++) {
					linhx[j][1].add(zp[i][0]);
				    linhy[j][1].add(zp[i][1]);	
				}
				
				// Add points to tp where necessary
				scpadapt(tpReal, tpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
			} // while (iter < maxrefn)
		} // for (j = 0; j < R2.length; j++)
		
		for (i = 0; i < R2.length; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		} // for (i = 0; i < R2.length; i++)
		
		// Plot radii
		if (drawTheta) {
			linhx = new Vector[theta2.length][2];
			linhy = new Vector[theta2.length][2];
			for (i = 0; i < theta2.length; i++) {
				for (j = 0; j < 2; j++) {
					linhx[i][j] = new Vector<Double>();
					linhy[i][j] = new Vector<Double>();
				}
			}
			for (j = 0; j < theta2.length; j++) {
				RpReal.clear();
				RpImag.clear();
				zpReal.clear();
				zpImag.clear();
				for (i = 0; i < num2draw; i++) {
					if (drawThetaToRadiusOne) {
					    RpReal.add((double)i/(double)(num2draw-1.0));
					}
					else {
						 RpReal.add((double)i/(double)(num2draw));	
					}
					RpImag.add(0.0);
					zpReal.add(RpReal.get(i)*Math.cos(theta2[j]));
					zpImag.add(RpReal.get(i)*Math.sin(theta2[j]));
				}
				newlog.clear();
				for (i = 0; i < num2draw; i++) {
					newlog.add(true);
				}
				wpReal.clear();
				wpImag.clear();
				for (i = 0; i < num2draw; i++) {
					wpReal.add(Double.NaN);
					wpImag.add(0.0);
				}
				
				// The individual points will be shown as they are found.
				
				// Adaptive refinement to make curve smooth
				int iter = 0;
				while (iter < maxrefn) {
					int numnewlog = 0;
					for (i = 0; i < newlog.size(); i++) {
					    if (newlog.get(i)) {
					    	numnewlog++;
					    }
					} // for (i = 0; i < newlog.size(); i++)
					if (numnewlog == 0) {
						break;
					}
					double zpnew[][] = new double[numnewlog][2];
					for (i = 0, m = 0; i < newlog.size(); i++) {
					    if (newlog.get(i)) {
					    	zpnew[m][0] = zpReal.get(i);
					    	zpnew[m++][1] = zpImag.get(i);
					    }
					} // for (i = 0, m = 0; i < newlog.length; i++)
					neww = crmap(zpnew, w, beta, cr, aff, quadnum, mt, Q, qdat);
					for (i = 0, m = 0; i < newlog.size(); i++) {
						if (newlog.get(i)) {
						    wpReal.set(i, neww[m][0]);
						    wpImag.set(i, neww[m++][1]);
						} 
					} // for (i = 0, m = 0; i < newlog.size(); i++)
					iter = iter + 1;
					
					linhx[j][0].clear();
					linhy[j][0].clear();
					linhx[j][1].clear();
					linhy[j][1].clear();
					// Update the points to show progress
					for (i = 0; i < wpReal.size(); i++) {
					    linhx[j][0].add(wpReal.get(i));
					    linhy[j][0].add(wpImag.get(i));
					    linhx[j][1].add(zpReal.get(i));
					    linhy[j][1].add(zpImag.get(i));
					}
					
					scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
				} // while (iter < maxrefn)
			} // for (j = 0; j < theta2.length; j++)
			
			for (i = 0; i < theta2.length; i++) {
			    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
			    	for (j = 0; j < linhx[i][0].size()-1; j++) {
			    		double posx1 = linhx[i][0].get(j);	
			    		double posy1 = linhy[i][0].get(j);
			    		double posx2 = linhx[i][0].get(j+1);
			    		double posy2 = linhy[i][0].get(j+1);
			    		x1Vector.add(posx1);
			    		y1Vector.add(posy1);
			    		x2Vector.add(posx2);
			    		y2Vector.add(posy2);
			    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
	    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
	    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
	    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
	    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
	    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
	    			    graph.drawLine(g, x1, y1, x2, y2);
			    	}
			    }
			} // for (i = 0; i < theta2.length; i++)
		} // if (drawTheta)
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
		return;
	}
	
	private double[][] crmap(double zp[][], double w[][], double beta[], double cr[], double aff[][][],
			int quadnum, double mt[][], qlgraph Q, double qdat[][]) {
		// Schwarz-Christoffel disk map in crossratio formulation.
		// crmap computes the values of the disk map at the points in the vector zp.  The arguments
		// are returned from crparam, craffine, and crfixwc.
		// crmap uses quadrature data intended to give an answer accurate to within roughly tol.
		
		// Original MATLAB crmap routine copyright 1998 by Toby Driscoll.
		int i, j;
		double qdat2[][];
		double cre[] = new double[1];
		double cim[] = new double[1];
		double num[] = new double[2];
		double denom[] = new double[2];
		double val;
		int idx[];
		double minVal;
		int q;
		int n = w.length;
		if (qdat == null) {
		   qdat2 = new double[8][2*beta.length+2];
		   scqdata(qdat2, beta, 8);	   
	    }
	    else if (qdat.length == 1) {
		    int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 4);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	   
	    }
	    else {
		   qdat2 = qdat;
	    }
		int p = zp.length;
		double wp[][] = new double[p][2];
		
		// Transform points into all embeddings, from quadnum and mt created in crfixwc
		double zl[][] = new double[p][2];
		for (i = 0; i < p; i++) {
		    zmlt(mt[0][0], mt[0][1], zp[i][0], zp[i][1], cre, cim);
		    num[0] = cre[0] + mt[1][0];
		    num[1] = cim[0] + mt[1][1];
		    zmlt(mt[2][0], mt[2][1], zp[i][0], zp[i][1], cre, cim);
		    denom[0] = cre[0] + mt[3][0];
		    denom[1] = cim[0] + mt[3][1];
		    zdiv(num[0], num[1], denom[0], denom[1], cre, cim);
		    zl[i][0] = cre[0];
		    zl[i][1] = cim[0];
		}
		// zl2 is cr.length by p
		double zl2[][][] = new double[cr.length][p][2];
		crspread(zl2, null, zl, quadnum, cr, Q);
		
		// Choose best embeddings based on proximity to origin
		// In testCrDiskmap3() saw in MIPAV
		// i = 0 val = 0.9999999999999996
	    // i = 1 val = 0.9999999999999994
		// i = 2 val = 0.9999999999999998
		// Saw in MATLAB:
		// 1.000000000000000
		// 1.000000000000000
		// 1.000000000000000
		// epsilon = 2.2204460e-16
		// So to select first actual row minimum require that the value + (10.0 * eps) be less than the minVal
		idx = new int[p];
		for (j = 0; j < p; j++) {
			minVal = Double.MAX_VALUE;
			for (i = 0; i < cr.length; i++) {
		        val = zabs(zl2[i][j][0], zl2[i][j][1]);
		        if (val + (10.0 * eps) < minVal) {
		    	    minVal = val;
		    	    idx[j] = i;
		        }
		    }
		}
		
		// Compute maps via embeddings
		int sameVal = 0;
		int idx2[] = new int[idx.length];
		for (i = 0; i < idx.length; i++) {
			idx2[i] = idx[i];
		}
		Arrays.sort(idx2);
		for (i = 0; i < p-1; i++) {
		    if (idx2[i] == idx2[i+1]) {
		        sameVal++;	
		    }
		}
		int unique = p - sameVal;
		int qa[] = new int[unique];
		qa[0] = idx2[0];
		for (i = 1, j = 1; i < p; i++) {
			if (idx2[i] != idx2[i-1]) {
				qa[j++] = idx2[i];
			}
		}
		for (q = 0; q < qa.length; q++) {
		    double z[][] = crembed(cr, Q, qa[q]);
		    boolean mask[] = new boolean[p];
		    int maskset = 0;
		    for (i = 0; i < p; i++) {
		    	if (idx[i] == qa[q]) {
		    		mask[i] = true;
		    		maskset++;
		    	}
		    }
		    double zl2in[][] = new double[maskset][2];
		    double affin[][] = new double[aff[0].length][2];
		    for (i = 0, j = 0; i < p; i++) {
		    	if (mask[i]) {
		    		zl2in[j][0] = zl2[qa[q]][i][0];
		    		zl2in[j++][1] = zl2[qa[q]][i][1];
		    	}
		    }
		    for (i = 0; i < aff[0].length; i++) {
		    	affin[i][0] = aff[qa[q]][i][0];
		    	affin[i][1] = aff[qa[q]][i][1];
		    }
		    double wpmask[][] = crmap0(zl2in, z, beta, affin, qdat2);
		    for (i = 0, j = 0; i < p; i++) {
		    	if (mask[i]) {
		    		wp[i][0] = wpmask[j][0];
		    		wp[i][1] = wpmask[j++][1];
		    	}
		    }
		} // for (q = 0; q < qa.length; q++)
		
		return wp;
	}
	
	private void crspread(double ul[][][], double dl[][][], double u[][], int quadnum, double cr[], qlgraph Q) {
	    // Transform points to every embedding in CR formulation.
		// Each quadrilateral has an associated embedding of the prevertices.  These embeddings
		// are linked by Moebius transformations, each of which is well-conditioned.
		
		// crspread assumes that the points of u are given in a single embedding, for quadrilateral quadnum.
		// The Moebius transformations are applied recursively so that ul(:,k) represents u[k] in all the 
		// embeddings.  Equivalently, us(qn,:) is the representation of U transpose in embedding number qn.
		
		// dl when present returns the derivatives of the composite transformations to the embeddings.
		// See also crparam, crgather, moebius.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j, k;
		double cre[] = new double[1];
		double cim[] = new double[1];
		double cre2[] = new double[1];
		double cim2[] = new double[1];
		double num[] = new double[2];
		double denom[] = new double[2];
		int n3 = cr.length;
		
		for (i = 0; i < n3; i++) {
			for (j = 0; j < u.length; j++) {
				ul[i][j][0] = 0.0;
				ul[i][j][1] = 0.0;
			}
		}
		
		for (i = 0; i < u.length; i++) {
			ul[quadnum][i][0] = u[i][0];
			ul[quadnum][i][1] = u[i][1];
		}
		
		if (dl != null) {
			for (i = 0; i < n3; i++) {
				for (j = 0; j < u.length; j++) {
					dl[i][j][0] = 0.0;
					dl[i][j][1] = 0.0;
				}
			}
			for (i = 0; i < u.length; i++) {
				dl[quadnum][i][0] = 1.0;
				dl[quadnum][i][1] = 0.0;
			}
		} // if (dl != null)
		
		// Place the quadrilateral prevertices in a rectangle around the origin.
		// We will store each rectangle so as to stably find the map between any
		// neighboring pair of embeddings.
		int idx[] = new int[4];
		for (i = 0; i < 4; i++) {
			idx[i] = Q.qlvert[i][quadnum];
		}
		double r = cr[quadnum];
		double f1 = Math.sqrt(1.0/(r + 1.0));
		double f2 = Math.sqrt(r/(r + 1.0));
		double zr[][][] = new double[4][n3][2];
		zr[0][quadnum][0] = -f1;
		zr[0][quadnum][1] = -f2;
		zr[1][quadnum][0] = -f1;
		zr[1][quadnum][1] = f2;
		zr[2][quadnum][0] = f1;
		zr[2][quadnum][1] = f2;
		zr[3][quadnum][0] = f1;
		zr[3][quadnum][1] = -f2;
		
		boolean done[] = new boolean[n3];
		done[quadnum] = true;
		int numnotdone = n3-1;
		// Neighbors of quadnum are available
		//  Q.adjacent is an (n-3) by (n-3) logical matrix; it indicates which 
		// quadrilaterals share a common triangle.
		boolean todo[] = new boolean[n3];
		for (i = 0; i < n3; i++) {
			todo[i] = Q.adjacent[i][quadnum];
		}
		
		double zin[][] = new double[3][2];
		double win[][] = new double[3][2];
		int i1[] = new int[3];
		int i2[] = new int[3];
		double ulq[][] = new double[u.length][2];
		double mtfac[] = null;
		double dlq[][] = null;
		if (dl != null) {
		    mtfac = new double[2];
		    dlq = new double[u.length][2];
	    }
		while (numnotdone > 0) {
		    // Pick an embedding
			int q = -1;
			for (i = 0; i < n3; i++) {
				if (todo[i]) {
					q = i;
					break;
				}
			} // for (i = 0; i < n3; i++)
			r = cr[q];
			// Quadrilateral prevertices for q
			f1 = Math.sqrt(1.0/(r+1.0));
			f2 = Math.sqrt(r/(r + 1.0));
			zr[0][q][0] = -f1;
			zr[0][q][1] = -f2;
			zr[1][q][0] = -f1;
			zr[1][q][1] = f2;
			zr[2][q][0] = f1;
			zr[2][q][1] = f2;
			zr[3][q][0] = f1;
			zr[3][q][1] = -f2;
			// Find a neighbor to quadrilateral q
			int qn = -1;
			for (i = 0; i < n3; i++) {
			    if (done[i] && Q.adjacent[i][q]) {
			    	qn = i;
			    	break;
			    }
			} // for (i = 0; i < n3; i++)
			// Find the 3 points in common between q and qn
			int numcommon = 0;
			for (i = 0; i < 4; i++) {
			    for (j = 0; j < 4; j++) {
			    	if (Q.qlvert[i][q] == Q.qlvert[j][qn]) {
			    	    numcommon++;	
			    	}
			    }
			} // for (i = 0; i < 4; i++)
			if (numcommon != 3) {
			    MipavUtil.displayError("In crspread number of points in common between q and qn =  " + numcommon + 
			    		" instead of the expected 3");
			    System.exit(-1);
			}
			k = 0;
			for (i = 0; i < 4; i++) {
			    for (j = 0; j < 4; j++) {
			    	if (Q.qlvert[i][q] == Q.qlvert[j][qn]) {
			    	    i1[k] = i;
			    	    i2[k++] = j;
			    	}
			    }
			} // for (i = 0; i < 4; i++)
			for (i = 0; i < 3; i++) {
				for (j = 0; j < 2; j++) {
					zin[i][j] = zr[i2[i]][qn][j];
					win[i][j] = zr[i1[i]][q][j];
				}
			} // for (i = 0; i < 3; i++)
			double mt[][] = moebius(zin, win);
			for (i = 0; i < u.length; i++) {
			    zmlt(mt[1][0], mt[1][1], ul[qn][i][0], ul[qn][i][1], cre, cim);	
			    num[0] = cre[0] + mt[0][0];
			    num[1] = cim[0] + mt[0][1];
			    zmlt(mt[3][0], mt[3][1], ul[qn][i][0], ul[qn][i][1], cre, cim);
			    denom[0] = cre[0] + mt[2][0];
			    denom[1] = cim[0] + mt[2][1];
			    zdiv(num[0], num[1], denom[0], denom[1], cre, cim);
			    ulq[i][0] = cre[0];
			    ulq[i][1] = cim[0];
			} // for (i = 0; i < u.length; i++)
			for (i = 0; i < u.length; i++) {
				ul[q][i][0] = ulq[i][0];
				ul[q][i][1] = ulq[i][1];
			}
			if (dl != null) {
			    zmlt(mt[1][0], mt[1][1], mt[2][0], mt[2][1], cre, cim);
			    zmlt(mt[0][0], mt[0][1], mt[3][0], mt[3][1], cre2, cim2);
			    mtfac[0] = cre[0] - cre2[0];
			    mtfac[1] = cim[0] - cim2[0];
			    for (i = 0; i < u.length; i++) {
			    	zmlt(dl[qn][i][0], dl[qn][i][1], mtfac[0], mtfac[1], cre, cim);
			    	num[0] = cre[0];
				    num[1] = cim[0];
				    zmlt(mt[3][0], mt[3][1], ul[qn][i][0], ul[qn][i][1], cre, cim);
				    denom[0] = cre[0] + mt[2][0];
				    denom[1] = cim[0] + mt[2][1];
				    zmlt(denom[0], denom[1], denom[0], denom[1], cre, cim);
				    denom[0] = cre[0];
				    denom[1] = cim[0];
				    zdiv(num[0], num[1], denom[0], denom[1], cre, cim);
				    dlq[i][0] = cre[0];
				    dlq[i][1] = cim[0];
			    } // for (i = 0; i < u.length; i++)
			    for (i = 0; i < u.length; i++) {
			    	dl[q][i][0] = dlq[i][0];
			    	dl[q][i][1] = dlq[i][1];
			    }
			} // if (dl != null)
			done[q] = true;
		    todo[q] = false;
		    numnotdone--;
		    // Neighbors of q can be done now
		    for (i = 0; i < n3; i++) {
		        todo[i] = todo[i] || (Q.adjacent[i][q] && !done[i]);	
		    }
		} // while (numnotdone > 0)
	} // crspread
	
	private double[][] moebius(double z[][], double w[][]) {
		// Moebius transformation parameters
		// moebius computes the coefficients of the Moebius transformation, taking
		// the 3-vector z to w, so that
		// w = (a[0]*z + a[1])./(a[2]*z + a[3]).
		// Infinities are allowed
		// Original MATLAB moebius routine copyright 1998 by Toby Driscoll.
		
		int i, j;
		double temp;
		double A[][] = new double[4][2];
		for (i = 0; i < 4; i++) {
			A[i][0] = Double.NaN;
		}
		
		boolean haveInfiniteW = false;
		boolean haveInfiniteZ = false;
		int infiniteW = -1;
		int infiniteZ = -1;
		int renum[] = new int[3];
		double z2[][] = new double[3][2];
		double w2[][] = new double[3][2];
		double t1[] = new double[2];
		double t2[] = new double[2];
		double cr[] = new double[1];
		double ci[] = new double[1];
		double cr2[] = new double[1];
		double ci2[] = new double[1];
		for (i = 0; i < 3; i++) {
			if (Double.isInfinite(w[i][0]) || (Double.isInfinite(w[i][1]))) {
				haveInfiniteW = true;
				infiniteW = i;
			}
			if (Double.isInfinite(z[i][0]) || (Double.isInfinite(z[i][1]))) {
				haveInfiniteZ = true;
				infiniteZ = i;
			}
		} // for (i = 0; i < 3; i++)
		
		if (haveInfiniteW) {
			// Make w[1] = Inf
			for (i = 0; i < 3; i++) {
				renum[i] = (infiniteW + 2 + i) % 3;
			}
			for (i = 0; i < 3; i++) {
			    for (j = 0; j < 2; j++) {
			    	z2[i][j] = z[renum[i]][j];
			    	if (Double.isInfinite(z2[i][j])) {
			    		infiniteZ = i;
			    	}
			    	w2[i][j] = w[renum[i]][j];
			    }
			} // for (i = 0; i < 3; i++) 
			if (!haveInfiniteZ) {
			    t1[0] = z2[1][0] - z2[0][0];
			    t1[1] = z2[1][1] - z2[0][1];
			    t2[0] = z2[1][0] - z2[2][0];
			    t2[1] = z2[1][1] - z2[2][1];
			}
			else if (infiniteZ == 1) {
				t1[0] = 1;
				t2[0] = 1;
			}
			else {
				// Will have to deal separately wiht w[1] = Inf and z[0] = Inf
				if (infiniteZ != 0) {
				    for (i = 0; i < 2; i++) {
				    	temp = z2[2][i];
				    	z2[2][i] = z2[0][i];
				    	z2[0][i] = temp;
				    	temp = w2[2][i];
				    	w2[2][i] = w2[0][i];
				    	w2[0][i] = temp;
				    }
				} // if (infiniteZ != 0)
				A[0][0] = w2[0][0];
				A[0][1] = w2[0][1];
				zmlt(w2[2][0], w2[2][1], z2[2][0] - z2[1][0], z2[2][1] - z2[1][1], cr, ci);
				zmlt(w2[0][0], w2[0][1], z2[2][0], z2[2][1], cr2, ci2);
				A[1][0] = cr[0] - cr2[0];
				A[1][1] = ci[0] - ci2[0];
				A[2][0] = 1.0;
				A[2][1] = 0.0;
				A[3][0] = -z2[1][0];
				A[3][1] = -z2[1][1];
			} // else 
		} // if (haveInfiniteW)
		
		else if (haveInfiniteZ) {
			// We already know no w is infinite
			// Make z[1] = Inf
			for (i = 0; i < 3; i++) {
				renum[i] = (infiniteZ + 2 + i) % 3;
			}
			for (i = 0; i < 3; i++) {
			    for (j = 0; j < 2; j++) {
			    	z2[i][j] = z[renum[i]][j];
			    	w2[i][j] = w[renum[i]][j];
			    }
			} // for (i = 0; i < 3; i++) 
			t1[0] = w2[1][0] - w2[2][0];
			t1[1] = w2[1][1] - w2[2][1];
			t2[0] = w2[1][0] - w2[0][0];
			t2[1] = w2[1][1] - w2[0][1];
		} // else if (haveInfiniteZ)
		
		else {
			// Everything finite
			zmlt(z[0][0] - z[1][0], z[0][1] - z[1][1], w[2][0] - w[1][0], w[2][1] - w[1][1], cr, ci);
			t1[0] = cr[0];
			t1[1] = ci[0];
			zmlt(z[1][0] - z[2][0], z[1][1] - z[2][1], w[1][0] - w[0][0], w[1][1] - w[0][1], cr, ci);
			t2[0] = cr[0];
			t2[1] = ci[0];
			for (i = 0; i < 3; i++) {
				for (j = 0; j < 2; j++) {
					w2[i][j] = w[i][j];
					z2[i][j] = z[i][j];
				}
			}
		} // else
		
		if (Double.isNaN(A[0][0])) {
		    zmlt(w2[0][0], w2[0][1], t1[0], t1[1], cr, ci);
		    zmlt(w2[2][0], w2[2][1], t2[0], t2[1], cr2, ci2);
		    A[0][0] = cr[0] - cr2[0];
		    A[0][1] = ci[0] - ci2[0];
		    zmlt(w2[2][0], w2[2][1], z2[0][0], z2[0][1], cr, ci);
		    zmlt(cr[0], ci[0], t2[0], t2[1], cr, ci);
		    zmlt(w2[0][0], w2[0][1], z2[2][0], z2[2][1], cr2, ci2);
		    zmlt(cr2[0], ci2[0], t1[0], t1[1], cr2, ci2);
		    A[1][0] = cr[0] - cr2[0];
		    A[1][1] = ci[0] - ci2[0];
		    A[2][0] = t1[0] - t2[0];
		    A[2][1] = t1[1] - t2[1];
		    zmlt(z2[0][0], z2[0][1], t2[0], t2[1], cr, ci);
		    zmlt(z2[2][0], z2[2][1], t1[0], t1[1], cr2, ci2);
		    A[3][0] = cr[0] - cr2[0];
		    A[3][1] = ci[0] - ci2[0];
		} // if (Double.isNaN(A[0][0]))
		
		return A;
	}
	
	private void deplot(double w[][], double beta[], double z[][], double c[],
			double R[], double theta[], int nqpts, int num1draw, int num2draw, int yInvert,
			double axis[]) {
		// Image of polar grid under Schwarz-Christoffel exterior map.
		// deplot will adaptively plot the images under the Schwarz-Christoffel 
		// exterior map of circles and rays in the unit disk. 
		// If R.length == 1 and theta.length == 1, R[0] is the
		// number of evenly spaced circles and theta[0] is the number of
		// evenly spaced rays.  If R.length > 1 and theta.length > 1, then the
		// circles are plotted at radii given by the entries of R and rays at
		// the angles specified in theta.
		
		// nqpts Number of quadrature points per integration.
		// Approximately equals -log10(error).  Increase if plot
	    // has false little zigzags in curves. 
		
		// Original MATLAB deplot routine copyright 1998 by Toby Driscoll.
		
		// In original code num1draw = 20 and num2draw = 14
		int i;
		int j;
		int m;
		int Rlength;
		int thetalength;
		double spacing;
		double R2[];
		double theta2[] = null;
		boolean drawTheta = true;
		int n = w.length;
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
        // Max allowed number of adaptive refinements made to meet other requirements 
        int maxrefn = 16;
		double axlim[] = new double[4];
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		Vector<Double>tpReal = new Vector<Double>();
		Vector<Double>tpImag = new Vector<Double>();
		Vector<Double>RpReal = new Vector<Double>();
		Vector<Double>RpImag = new Vector<Double>();
		Vector<Double>zpReal = new Vector<Double>();
		Vector<Double>zpImag = new Vector<Double>();
		Vector<Double>wpReal = new Vector<Double>();
		Vector<Double>wpImag = new Vector<Double>();
		Vector<Boolean>newlog = new Vector<Boolean>();
		double zp[][];
		double neww[][];
		// Empty arguments default to 10.
		if ((R == null) || (R.length == 0)) {
			R = new double[1];
			R[0] = 10;
		}
		if ((theta == null) || (theta.length == 0)) {
			theta = new double[1];
			theta[0] = 10;
		}
		
		// Integer arguments must be converted to specific values
		if ((R.length == 1) && (R[0] == Math.round(R[0]))) {
			Rlength = (int)R[0];
			R2 = new double[Rlength];
		    spacing = (0.75/(R[0] + 1.0));
		    for (i = 1; i <= Rlength; i++) {
		        R2[i-1] = .25 + i*spacing;	
		    }
		} // if ((R.length == 1) && (R[0] == Math.round(R[0])))
		else {
			R2 = new double[R.length];
			for (i = 0; i < R.length; i++) {
				R2[i] = R[i];
			}
		}
		if ((theta.length == 1) && (theta[0] == 0.0)) {
			drawTheta = false;
		}
		else if ((theta.length == 1) && (theta[0] == Math.round(theta[0]))) {
			thetalength = (int)theta[0];
			theta2 = new double[thetalength];
		    spacing = ((2.0*Math.PI)/theta[0]);
		    for (i = 0; i < thetalength; i++) {
		        theta2[i] = i*spacing;	
		    }
		} // if ((theta.length == 1) && (theta[0] == Math.round(theta[0])))
		else {
			theta2 = new double[theta.length];
			for (i = 0; i < theta.length; i++) {
				theta2[i] = theta[i];
			}
		}
		
		float xPointArray[] = new float[n+1];
		float yPointArray[] = new float[n+1];
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, w, beta, false, axlim, yInvert, true, axis);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
        double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		// Plot circles...
	    linhx = new Vector[R2.length][2];
		linhy = new Vector[R2.length][2];
		for (i = 0; i < R2.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		for (j = 0; j < R2.length; j++) {
		    // Start with evenly spaced theta
			tpReal.clear();
			tpImag.clear();
			for (i = 0; i < num1draw; i++) {
				tpReal.add(i*(2.0*Math.PI)/(num1draw-1.0));
				tpImag.add(0.0);
			}
			newlog.clear();
			for (i = 0; i < num1draw; i++) {
				newlog.add(true);
			}
			wpReal.clear();
			wpImag.clear();
			for (i = 0; i < num1draw; i++) {
				wpReal.add(Double.NaN);
				wpImag.add(0.0);
			}
			
			// The individual points will be shown as they are found.
			
			// Adaptive refinement to make curve smooth
			int iter = 0;
			while (iter < maxrefn) {
				int numnewlog = 0;
				for (i = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	numnewlog++;
				    }
				} // for (i = 0; i < newlog.size(); i++)
				if (numnewlog == 0) {
					break;
				}
				zp = new double[numnewlog][2];
				for (i = 0, m = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	zp[m][0] = R2[j]*Math.cos(tpReal.get(i));
				    	zp[m++][1] = R2[j]*Math.sin(tpReal.get(i));
				    }
				} // for (i = 0, m = 0; i < newlog.length; i++)
				neww = new double[numnewlog][2];
				neww = demap(zp,w,beta,z,c,qdat);
				for (i = 0, m = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
					    wpReal.set(i, neww[m][0]);
					    wpImag.set(i, neww[m++][1]);
					} 
				} // for (i = 0, m = 0; i < newlog.size(); i++)
				iter = iter + 1;
				
				linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (i = 0; i < wpReal.size(); i++) {
				    linhx[j][0].add(wpReal.get(i));
				    linhy[j][0].add(wpImag.get(i));
				}
				for (i = 0; i < zp.length; i++) {
					linhx[j][1].add(zp[i][0]);
				    linhy[j][1].add(zp[i][1]);	
				}
				
				// Add points to tp where necessary
				scpadapt(tpReal, tpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
			} // while (iter < maxrefn)
		} // for (j = 0; j < R2.length; j++)
		
		for (i = 0; i < R2.length; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		} // for (i = 0; i < R2.length; i++)
		
		// Plot radii
		if (drawTheta) {
			linhx = new Vector[theta2.length][2];
			linhy = new Vector[theta2.length][2];
			for (i = 0; i < theta2.length; i++) {
				for (j = 0; j < 2; j++) {
					linhx[i][j] = new Vector<Double>();
					linhy[i][j] = new Vector<Double>();
				}
			}
			for (j = 0; j < theta2.length; j++) {
				RpReal.clear();
				RpImag.clear();
				zpReal.clear();
				zpImag.clear();
				for (i = 0; i < num2draw; i++) {
					RpReal.add(i/(num2draw-1.0));
					RpImag.add(0.0);
					zpReal.add(RpReal.get(i)*Math.cos(theta2[j]));
					zpImag.add(RpReal.get(i)*Math.sin(theta2[j]));
				}
				newlog.clear();
				for (i = 0; i < num2draw; i++) {
					newlog.add(true);
				}
				wpReal.clear();
				wpImag.clear();
				for (i = 0; i < num2draw; i++) {
					wpReal.add(Double.NaN);
					wpImag.add(0.0);
				}
				
				// The individual points will be shown as they are found.
				
				// Adaptive refinement to make curve smooth
				int iter = 0;
				while (iter < maxrefn) {
					int numnewlog = 0;
					for (i = 0; i < newlog.size(); i++) {
					    if (newlog.get(i)) {
					    	numnewlog++;
					    }
					} // for (i = 0; i < newlog.size(); i++)
					if (numnewlog == 0) {
						break;
					}
					double zpnew[][] = new double[numnewlog][2];
					for (i = 0, m = 0; i < newlog.size(); i++) {
					    if (newlog.get(i)) {
					    	zpnew[m][0] = zpReal.get(i);
					    	zpnew[m++][1] = zpImag.get(i);
					    }
					} // for (i = 0, m = 0; i < newlog.length; i++)
					neww = demap(zpnew, w, beta, z, c, qdat);
					for (i = 0, m = 0; i < newlog.size(); i++) {
						if (newlog.get(i)) {
						    wpReal.set(i, neww[m][0]);
						    wpImag.set(i, neww[m++][1]);
						} 
					} // for (i = 0, m = 0; i < newlog.size(); i++)
					iter = iter + 1;
					
					linhx[j][0].clear();
					linhy[j][0].clear();
					linhx[j][1].clear();
					linhy[j][1].clear();
					// Update the points to show progress
					for (i = 0; i < wpReal.size(); i++) {
					    linhx[j][0].add(wpReal.get(i));
					    linhy[j][0].add(wpImag.get(i));
					    linhx[j][1].add(zpReal.get(i));
					    linhy[j][1].add(zpImag.get(i));
					}
					
					scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
				} // while (iter < maxrefn)
			} // for (j = 0; j < theta2.length; j++)
			
			for (i = 0; i < theta2.length; i++) {
			    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
			    	for (j = 0; j < linhx[i][0].size()-1; j++) {
			    		double posx1 = linhx[i][0].get(j);	
			    		double posy1 = linhy[i][0].get(j);
			    		double posx2 = linhx[i][0].get(j+1);
			    		double posy2 = linhy[i][0].get(j+1);
			    		x1Vector.add(posx1);
			    		y1Vector.add(posy1);
			    		x2Vector.add(posx2);
			    		y2Vector.add(posy2);
			    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
	    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
	    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
	    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
	    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
	    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
	    			    graph.drawLine(g, x1, y1, x2, y2);
			    	}
			    }
			} // for (i = 0; i < theta2.length; i++)
		} // if (drawTheta)
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
		return;
	}
	
	private void dplot(double w[][], double beta[], double z[][], double c[],
			double R[], double theta[], int nqpts, int num1draw, int num2draw, int yInvert) {
		// Image of polar grid under Schwarz-Christoffel disk map.
		// dplot will adaptively plot the images under the Schwarz-Christoffel 
		// disk map of circles and rays in the unit disk. 
		// If R.length == 1 and theta.length == 1, R[0] is the
		// number of evenly spaced circles and theta[0] is the number of
		// evenly spaced rays.  If R.length > 1 and theta.length > 1, then the
		// circles are plotted at radii given by the entries of R and rays at
		// the angles specified in theta.
		
		// nqpts Number of quadrature points per integration.
		// Approximately equals -log10(error).  Increase if plot
	    // has false little zigzags in curves. 
		
		// Original MATLAB dplot routine copyright 1998 by Toby Driscoll.
		
		// In original code num1draw = 20 and num2draw = 14
		
		int i;
		int j;
		int m;
		int Rlength;
		int thetalength;
		double spacing;
		double R2[];
		double theta2[] = null;
		boolean drawTheta = true;
		int n = w.length;
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
        // Max allowed number of adaptive refinements made to meet other requirements 
        int maxrefn = 16;
		double axlim[] = new double[4];
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		Vector<Double>tpReal = new Vector<Double>();
		Vector<Double>tpImag = new Vector<Double>();
		Vector<Double>RpReal = new Vector<Double>();
		Vector<Double>RpImag = new Vector<Double>();
		Vector<Double>zpReal = new Vector<Double>();
		Vector<Double>zpImag = new Vector<Double>();
		Vector<Double>wpReal = new Vector<Double>();
		Vector<Double>wpImag = new Vector<Double>();
		Vector<Boolean>newlog = new Vector<Boolean>();
		double zp[][];
		double neww[][];
		// Empty arguments default to 10.
		if ((R == null) || (R.length == 0)) {
			R = new double[1];
			R[0] = 10;
		}
		if ((theta == null) || (theta.length == 0)) {
			theta = new double[1];
			theta[0] = 10;
		}
		
		// Integer arguments must be converted to specific values
		if ((R.length == 1) && (R[0] == Math.round(R[0]))) {
			Rlength = (int)R[0];
			R2 = new double[Rlength];
		    spacing = (1.0/(R[0] + 1.0));
		    for (i = 1; i <= Rlength; i++) {
		        R2[i-1] = i*spacing;	
		    }
		} // if ((R.length == 1) && (R[0] == Math.round(R[0])))
		else {
			R2 = new double[R.length];
			for (i = 0; i < R.length; i++) {
				R2[i] = R[i];
			}
		}
		if ((theta.length == 1) && (theta[0] == 0.0)) {
			drawTheta = false;
		}
		else if ((theta.length == 1) && (theta[0] == Math.round(theta[0]))) {
			thetalength = (int)theta[0];
			theta2 = new double[thetalength];
		    spacing = ((2.0*Math.PI)/theta[0]);
		    for (i = 0; i < thetalength; i++) {
		        theta2[i] = i*spacing;	
		    }
		} // if ((theta.length == 1) && (theta[0] == Math.round(theta[0])))
		else {
			theta2 = new double[theta.length];
			for (i = 0; i < theta.length; i++) {
				theta2[i] = theta[i];
			}
		}
		int numinfinite = 0;
		// Put 2 finite points at every infinity in plotpoly
		for (i = 0; i < w.length; i++) {
		    if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1]))	{
		        numinfinite++;	
		    }
		}
		float xPointArray[] = new float[n+1+numinfinite];
		float yPointArray[] = new float[n+1+numinfinite];
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, w, beta, false, axlim, yInvert, true, null);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
        double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		// Plot circles...
	    linhx = new Vector[R2.length][2];
		linhy = new Vector[R2.length][2];
		for (i = 0; i < R2.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		for (j = 0; j < R2.length; j++) {
		    // Start with evenly spaced theta
			tpReal.clear();
			tpImag.clear();
			for (i = 0; i < num1draw; i++) {
				tpReal.add(i*(2.0*Math.PI)/(num1draw-1.0));
				tpImag.add(0.0);
			}
			newlog.clear();
			for (i = 0; i < num1draw; i++) {
				newlog.add(true);
			}
			wpReal.clear();
			wpImag.clear();
			for (i = 0; i < num1draw; i++) {
				wpReal.add(Double.NaN);
				wpImag.add(0.0);
			}
			
			// The individual points will be shown as they are found.
			
			// Adaptive refinement to make curve smooth
			int iter = 0;
			while (iter < maxrefn) {
				int numnewlog = 0;
				for (i = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	numnewlog++;
				    }
				} // for (i = 0; i < newlog.size(); i++)
				if (numnewlog == 0) {
					break;
				}
				zp = new double[numnewlog][2];
				for (i = 0, m = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	zp[m][0] = R2[j]*Math.cos(tpReal.get(i));
				    	zp[m++][1] = R2[j]*Math.sin(tpReal.get(i));
				    }
				} // for (i = 0, m = 0; i < newlog.length; i++)
				neww = new double[numnewlog][2];
				neww = dmap(zp,w,beta,z,c,qdat);
				for (i = 0, m = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
					    wpReal.set(i, neww[m][0]);
					    wpImag.set(i, neww[m++][1]);
					} 
				} // for (i = 0, m = 0; i < newlog.size(); i++)
				iter = iter + 1;
				
				linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (i = 0; i < wpReal.size(); i++) {
				    linhx[j][0].add(wpReal.get(i));
				    linhy[j][0].add(wpImag.get(i));
				}
				for (i = 0; i < zp.length; i++) {
					linhx[j][1].add(zp[i][0]);
				    linhy[j][1].add(zp[i][1]);	
				}
				
				// Add points to tp where necessary
				scpadapt(tpReal, tpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
			} // while (iter < maxrefn)
		} // for (j = 0; j < R2.length; j++)
		
		for (i = 0; i < R2.length; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		} // for (i = 0; i < R2.length; i++)
		
		// Plot radii
		if (drawTheta) {
			linhx = new Vector[theta2.length][2];
			linhy = new Vector[theta2.length][2];
			for (i = 0; i < theta2.length; i++) {
				for (j = 0; j < 2; j++) {
					linhx[i][j] = new Vector<Double>();
					linhy[i][j] = new Vector<Double>();
				}
			}
			for (j = 0; j < theta2.length; j++) {
				RpReal.clear();
				RpImag.clear();
				zpReal.clear();
				zpImag.clear();
				for (i = 0; i < num2draw; i++) {
					RpReal.add(i/(num2draw-1.0));
					RpImag.add(0.0);
					zpReal.add(RpReal.get(i)*Math.cos(theta2[j]));
					zpImag.add(RpReal.get(i)*Math.sin(theta2[j]));
				}
				newlog.clear();
				for (i = 0; i < num2draw; i++) {
					newlog.add(true);
				}
				wpReal.clear();
				wpImag.clear();
				for (i = 0; i < num2draw; i++) {
					wpReal.add(Double.NaN);
					wpImag.add(0.0);
				}
				
				// The individual points will be shown as they are found.
				
				// Adaptive refinement to make curve smooth
				int iter = 0;
				while (iter < maxrefn) {
					int numnewlog = 0;
					for (i = 0; i < newlog.size(); i++) {
					    if (newlog.get(i)) {
					    	numnewlog++;
					    }
					} // for (i = 0; i < newlog.size(); i++)
					if (numnewlog == 0) {
						break;
					}
					double zpnew[][] = new double[numnewlog][2];
					for (i = 0, m = 0; i < newlog.size(); i++) {
					    if (newlog.get(i)) {
					    	zpnew[m][0] = zpReal.get(i);
					    	zpnew[m++][1] = zpImag.get(i);
					    }
					} // for (i = 0, m = 0; i < newlog.length; i++)
					neww = dmap(zpnew, w, beta, z, c, qdat);
					for (i = 0, m = 0; i < newlog.size(); i++) {
						if (newlog.get(i)) {
						    wpReal.set(i, neww[m][0]);
						    wpImag.set(i, neww[m++][1]);
						} 
					} // for (i = 0, m = 0; i < newlog.size(); i++)
					iter = iter + 1;
					
					linhx[j][0].clear();
					linhy[j][0].clear();
					linhx[j][1].clear();
					linhy[j][1].clear();
					// Update the points to show progress
					for (i = 0; i < wpReal.size(); i++) {
					    linhx[j][0].add(wpReal.get(i));
					    linhy[j][0].add(wpImag.get(i));
					    linhx[j][1].add(zpReal.get(i));
					    linhy[j][1].add(zpImag.get(i));
					}
					
					scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
				} // while (iter < maxrefn)
			} // for (j = 0; j < theta2.length; j++)
			
			for (i = 0; i < theta2.length; i++) {
			    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
			    	for (j = 0; j < linhx[i][0].size()-1; j++) {
			    		double posx1 = linhx[i][0].get(j);	
			    		double posy1 = linhy[i][0].get(j);
			    		double posx2 = linhx[i][0].get(j+1);
			    		double posy2 = linhy[i][0].get(j+1);
			    		x1Vector.add(posx1);
			    		y1Vector.add(posy1);
			    		x2Vector.add(posx2);
			    		y2Vector.add(posy2);
			    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
	    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
	    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
	    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
	    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
	    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
	    			    graph.drawLine(g, x1, y1, x2, y2);
			    	}
			    }
			} // for (i = 0; i < theta2.length; i++)
		} // if (drawTheta)
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
		return;
	}
	
	public void crrectplot(scmap M, double nre[], double nim[], int yInvert) {
		// Image of the cartesian grid under Schwarz-Christoffel rectified map.
		// crrplot will adaptively plot the images under the Schwarz-Christoffel
		// rectified map of nre evenly spaced vertical and nim evenly spaced
		// horizontal line segments.
		// From 1998 MATLAB plot routine copyright by Toby Driscoll.
		int i;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		polygon pr = M.rectpolygon;
		double wr[][] = pr.vertex;
		double betar[] = new double[pr.angle.length];
		for (i = 0; i < pr.angle.length; i++) {
			betar[i] = pr.angle[i] - 1;
		}
		double cr[] = M.crossratio;
		double aff[][][] = M.affine;
		double affr[][][] = M.rectaffine;
		qlgraph Q = M.qgraph;
		crrplot(w, beta, wr, betar, cr, aff, affr, Q, nre, nim, yInvert);
	}
	
	public void rectplot(scmap M, int nre, int nim, int yInvert) {
	    // plot plots the polygon associated with the Schwarz-Christoffel
		// rectangle map M and the images of nre evenly spaced vertical 
	    // and nim horizontal line segments.  
		// The usual values are nre = nim = 10
		// Extracted from original routine by Toby Driscoll copyright 1998.
		int i;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		double L[] = M.stripL;
		rplot(w, beta, z, c, L, nre, nim, yInvert);
	}
	
	public void crrplot(double w[][], double beta[], double wr[][], double betar[], double cr[],
			double aff[][][], double affr[][][], qlgraph Q, double re[], double im[], int yInvert) {
	    // Image of cartesian grid under Schwarz-Christoffel rectified map.	
		// crrplot will adaptively plot the images under the Schwarz-Christoffel rectified
		// map of re evenly spaced vertical and im evenly spaced
		// horizontal lines.
		// Extracted from original routine by Toby Driscoll copyright 1998.
		int n = w.length;
		// Number of quadrature points per integration.
		// Approximately equals -log10(error).  Increase if plot
		// has false little zigzags in curves. 
		int nqpts = 5; 
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
        // Max allowed number of adaptive refinements made to meet other requirements 
        int maxrefn = 16;
		double axlim[] = new double[4];
		double tol;
		double minrealwr = Double.MAX_VALUE;
		double maxrealwr = -Double.MAX_VALUE;
		double minimagwr = Double.MAX_VALUE;
		double maximagwr = -Double.MAX_VALUE;
		int i;
		int j;
		int m;
		for (i = 0; i < wr.length; i++) {
			if (wr[i][0] < minrealwr) {
				minrealwr = wr[i][0];
			}
			if (wr[i][0] > maxrealwr) {
				maxrealwr = wr[i][0];
			}
			if (wr[i][1] < minimagwr) {
				minimagwr = wr[i][1];
			}
			if (wr[i][1] > maximagwr) {
				maximagwr = wr[i][1];
			}
		} // for (i = 0; i < wr.length; i++)
		double xlim[] = new double[]{minrealwr, maxrealwr};
		double ylim[] = new double[]{minimagwr, maximagwr};
		// Fnd the size of the rectified domain
		double siz = Math.max(maxrealwr - minrealwr, maximagwr - minimagwr);
		
		// Zero arguments default to 10
		if (((re == null) || (re.length == 0)) && ((im == null) || (im.length == 0))) {
		    re = new double[]{10.0};
		    im = new double[]{10.0};
		}
		
		// Integer arguments must be converted to specific values
		if ((re.length == 1) && (re[0] == Math.round(re[0]))) {
			if (re[0] < 1) {
			    re = null;
			}
			else {
			    m = (int)re[0];	
			    double spacing = (xlim[1] - xlim[0])/(m + 1.0);
			    re = new double[m];
			    for (i = 0; i < m; i++) {
			    	re[i] = (i+1)* spacing;
			    }
			} // else 
		} // if ((re.length == 1) && (re[0] == Math.round(re[0])))
		if ((im.length == 1) & (im[0] == Math.round(im[0]))) {
			if (im[0] < 1) {
				im = null;
			}
			else {
				m = (int)im[0];
				double spacing = (ylim[1] - ylim[0])/(m + 1.0);
				im = new double[m];
				for (i = 0; i < m; i++) {
					im[i] = (i+1)*spacing;
				}
			}
		} // if ((im.length == 1) & (im[0] == Math.round(im[0])))
		float xPointArray[] = new float[n+1];
		float yPointArray[] = new float[n+1];
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, w, beta, false, axlim, yInvert, true, null);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		double qdatr[][] = new double[nqpts][2*betar.length+2];
		scqdata(qdatr, betar, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
        double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		tol = Math.pow(10.0, -nqpts);
		Vector<Double> linhx[][] = new Vector[re.length][2];
		Vector<Double> linhy[][] = new Vector[re.length][2];
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		crrplot0("ver", re, minlen, maxlen, maxrefn, tol, w, beta, wr,
				betar, cr, aff, affr, Q, qdat, qdatr, axlim, siz, linhx, linhy);
		for (i = 0; i < re.length; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		}
		linhx = new Vector[im.length][2];
		linhy = new Vector[im.length][2];
		
		crrplot0("hor", im, minlen, maxlen, maxrefn, tol, w, beta, wr,
				betar, cr, aff, affr, Q, qdat, qdatr, axlim, siz, linhx, linhy);
		for (i = 0; i < im.length; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		}
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);

	}
	
	private void crrplot0(String direcn, double val[], double minlen, double maxlen, int maxrefn, double tol,
			double w[][], double beta[], double wr[][], double betar[], double cr[], double aff[][][],
			double affr[][][], qlgraph Q, double qdat[][], double qdatr[][], double axlim[], double siz,
			Vector<Double>linhx[][], Vector<Double>linhy[][]) {
	    // This is the routine that actually draws the curves corresponding to either vertical
		// or horizontal lines
		int i, j, k, m, p;
		int numcross;
		int cross[];
		int cross2[];
		int cross3[];
		int n = w.length;
		int numidentical;
		int identical[];
		int numnotidentical;
		int notidentical[];
		
		Vector<Double>zpReal = new Vector<Double>();
		Vector<Double>zpImag = new Vector<Double>();
		Vector<Double>wpReal[];
		Vector<Double>wpImag[];;
		double neww[][][];
		int newqn[][];
		Vector<Boolean>newlog = new Vector<Boolean>();
		for (i = 0; i < val.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		double d[] = new double[wr.length];
		double signd[] = new double[d.length+1];
		double diffd[] = new double[d.length];
		double absd[] = new double[d.length];
		double srtcross[];
		double srtcross2[];
		double interval[][];
		int idx[];
		double testint[][];
		double testedge[][];
		int index[];
		boolean onvtx[][];
		int index2[];
		boolean onvtx2[][];
		int maxindex;
		int maxindex2;
		int numcurves;
		double delta[];
		int n0[];
		int n0sum;
		double zp[];
		int lenzp;
		double wp[][][];
		int qn[][];
		int qnold[][];
		int iter;
		int numnew;
		double zpnew[][];
		double tolmap;
		int zpindex[];

		for (j = 0; j < val.length; j++) {
		    // Find intersections with rectified polygon sides
			if (direcn.equals("ver")) {
				for (i =  0; i < wr.length; i++) {
					d[i] = wr[i][0] - val[j];
					if (Double.isNaN(d[i])) {
						signd[i] = Double.NaN;
					}
					else if (d[i] > 0) {
						signd[i] = 1.0;
					}
					else if (d[i] == 0.0) {
						signd[i] = 0.0;
					}
					else {
						signd[i] = -1.0;
					}
				} // for (i =  0; i < wr.length; i++)
			    signd[signd.length-1] = signd[0];
			    numcross = 0;
			    for (i = 0; i < n; i++) {
			    	diffd[i] = signd[i+1] - signd[i];
			    	absd[i] = Math.abs(d[i]);
			    	if ((diffd[i] != 0) || (absd[i] < tol)) {
			    		numcross++;
			    	}
			    }
			    cross = new int[numcross];
			    for (i = 0, m = 0; i < n; i++) {
			    	if ((diffd[i] != 0) || (absd[i] < tol)) {
			    		cross[m++] = i;
			    	}	
			    }
			    ArrayList<indexValueItem> valueList = new ArrayList<indexValueItem>();
			    for (i = 0; i < numcross; i++) {
			    	valueList.add(new indexValueItem(i, wr[cross[i]][1]));
			    }
			    Collections.sort(valueList, new indexValueComparator());
			    srtcross = new double[numcross];
			    idx = new int[numcross];
			    for (i = 0; i < numcross; i++) {
			    	srtcross[i] = valueList.get(i).getValue();
			    	idx[i] = valueList.get(i).getIndex();
			    }
			} // if (direcn.equals("ver"))
			else { // direcn.equals("hor"))
				for (i =  0; i < wr.length; i++) {
					d[i] = wr[i][1] - val[j];
					if (Double.isNaN(d[i])) {
						signd[i] = Double.NaN;
					}
					else if (d[i] > 0) {
						signd[i] = 1.0;
					}
					else if (d[i] == 0.0) {
						signd[i] = 0.0;
					}
					else {
						signd[i] = -1.0;
					}
				} // for (i =  0; i < wr.length; i++)
			    signd[signd.length-1] = signd[0];
			    numcross = 0;
			    for (i = 0; i < n; i++) {
			    	diffd[i] = signd[i+1] - signd[i];
			    	absd[i] = Math.abs(d[i]);
			    	if ((diffd[i] != 0) || (absd[i] < tol)) {
			    		numcross++;
			    	}
			    }
			    cross = new int[numcross];
			    for (i = 0, m = 0; i < n; i++) {
			    	if ((diffd[i] != 0) || (absd[i] < tol)) {
			    		cross[m++] = i;
			    	}	
			    }
			    ArrayList<indexValueItem> valueList = new ArrayList<indexValueItem>();
			    for (i = 0; i < numcross; i++) {
			    	valueList.add(new indexValueItem(i, wr[cross[i]][0]));
			    }
			    Collections.sort(valueList, new indexValueComparator());
			    srtcross = new double[numcross];
			    idx = new int[numcross];
			    for (i = 0; i < numcross; i++) {
			    	srtcross[i] = valueList.get(i).getValue();
			    	idx[i] = valueList.get(i).getIndex();
			    }	
			} // else direnc.equals("hor"))
			cross2 = new int[numcross];
			for (i = 0; i < numcross; i++) {
				cross2[i] = cross[idx[i]];
			}
			
			// Remove (near-) duplicates
			numidentical = 0;
			numnotidentical = 1;
			for (i = 0; i < numcross-1; i++) {
			    if (Math.abs(srtcross[i+1] - srtcross[i]) < tol) {
			    	numidentical++;
			    }
			    else {
			    	numnotidentical++;
			    }
			}
			identical = new int[numidentical];
			notidentical = new int[numnotidentical];
			for (i = 0, m = 0, p = 0; i < numcross-1; i++) {
				if (Math.abs(srtcross[i+1] - srtcross[i]) < tol) {
			    	identical[m++] = i;
			    }
				else {
					notidentical[p++] = i;
				}
			}
			notidentical[p] = numcross-1;
			srtcross2 = new double[numnotidentical];
			cross3 = new int[numnotidentical];
			for (i = 0; i < numnotidentical; i++) {
				srtcross2[i] = srtcross[notidentical[i]];
				cross3[i] = cross2[notidentical[i]];
			}
			numcross = numnotidentical;
			
			// Set up intervals between crossings
			interval = new double[numcross-1][2];
			for (i = 0; i < numcross-1; i++) {
				interval[i][0] = srtcross2[i];
				interval[i][1] = srtcross2[i+1];
			}
			
			// Test points at ends and midpoints of intervals
			testint = new double[numcross-1][2];
			testedge = new double[numcross][2];
			if (direcn.equals("ver")) {
			    for (i = 0; i < numcross-1; i++) {
			    	testint[i][0] = val[j];
			    	testint[i][1] = (interval[i][0] + interval[i][1])/2.0;
			    }
			    for (i = 0; i < numcross; i++) {
			    	testedge[i][0] = val[j];
			    	testedge[i][1] = srtcross2[i];
			    }
			} // if (direcn.equals("ver"))
			else { // (direcn.equals("hor"))
				for (i = 0; i < numcross-1; i++) {
			    	testint[i][0] = (interval[i][0] + interval[i][1])/2.0;
			    	testint[i][1] = val[j];
			    }
			    for (i = 0; i < numcross; i++) {
			    	testedge[i][0] = srtcross2[i];
			    	testedge[i][1] = val[j];
			    }       	
			} // else (direcn.equals("hor"))
			
			// Use test points to determine number of curves
			index = new int[testint.length];
			onvtx = new boolean[wr.length][testint.length];
			isinpoly2(index, onvtx, testint, wr, betar, tol);
			maxindex = -1;
			for (i = 0; i < index.length; i++) {
				if (index[i] > maxindex) {
					maxindex = index[i];
				}
			}
			index2 = new int[testedge.length];
			onvtx2 = new boolean[wr.length][testedge.length];
			isinpoly2(index2, onvtx2, testedge, wr, betar, tol);
			maxindex2 = -1;
			for (i = 0; i < index2.length; i++) {
				if (index2[i] > maxindex2) {
					maxindex2 = index2[i];
				}
			}
			numcurves = Math.max(maxindex, maxindex2);
			
			// Put initial points in zp (string together intervals).
			delta = new double[numcross-1];
			for (i = 0; i < numcross-1; i++) {
				delta[i] = interval[i][1] - interval[i][0];
			}
			n0 = new int[numcross-1];
			n0sum = 0;
			for (i = 0; i < numcross-1; i++) {
				n0[i] = Math.max(5, (int)Math.ceil(16*delta[i]/siz));
				n0sum = n0sum + n0[i];
			}
			zp = new double[1 + n0sum];
			m = 0;
			zp[m++] = interval[0][0];
			for (k = 0; k < numcross-1; k++) {
				for (p = 1; p <= n0[k]; p++) {
				    zp[m++] = interval[0][k] + p * delta[k]/n0[k];
				}
			} // for (k = 0; k < numcross-1; k++)
			
			
			lenzp = zp.length;
			zpReal.clear();
			zpImag.clear();
			if (direcn.equals("ver")) {
			    for (i = 0; i < lenzp; i++) {
			    	zpReal.add(val[j]);
			    	zpImag.add(zp[i]);
			    }
			} // if (direcn.equals("ver"))
			else { // (direcn.equals("hor"))
				for (i = 0; i < lenzp; i++) {
					zpReal.add(zp[i]);
					zpImag.add(val[j]);
			    }	
			} // else (direcn.equals("hor"))
			
			// Prepare for iterative mapping
		    newlog.clear();
		    for (i = 0; i < lenzp; i++) {
		        newlog.add(true);
		    }
		    numnew = lenzp;
		    wpReal = new Vector[lenzp];
		    wpImag = new Vector[lenzp];
		    qn = new int[lenzp][numcurves];
		    for (i = 0; i < lenzp; i++) {
		    	wpReal[i] = new Vector<Double>();
		    	wpImag[i] = new Vector<Double>();
		    	for (m = 0; m < numcurves; m++) {
		    		wpReal[i].add(Double.NaN);
		    		wpImag[i].add(Double.NaN);
		    		qn[i][m] = -1;
		    	}
		    }
		    iter = 0;
		    
		    // Do the mapping and the plotting
		    while ((numnew > 0) && (iter < maxrefn)) {
		    	
		        zpnew = new double[numnew][2];
		        for (i = 0, m = 0; i < newlog.size(); i++) {
		        	if (newlog.get(i)) {
		        		zpnew[m][0] = zpReal.get(i);
		        		zpnew[m++][1] = zpImag.get(i);
		        	}
		        }
		        tolmap = Math.pow(10.0, -qdat.length);
		        zpindex = new int[numnew];
				onvtx = new boolean[wr.length][numnew];
				isinpoly2(zpindex, onvtx, zpnew, wr, betar, tolmap);
				maxindex = -1;
				for (i = 0; i < numnew; i++) {
					if (zpindex[i] > maxindex) {
						maxindex = zpindex[i];
					}
				} // for (i = 0; i < numnew; i++)
				if (maxindex > numcurves) {
					MipavUtil.displayError("Too many values found at some points");
					System.exit(-1);
				}
				neww = new double[numnew][Math.max(1, maxindex)][2];
				newqn = new int[numnew][Math.max(1, maxindex)];
		        crrmap(neww, newqn, zpnew, w, beta, wr, betar, cr, aff, affr,
		        		Q, qdat, qdatr);
		        // Incorporate new points
		        for (i = 0, k = 0; i < wpReal.length; i++) {
		        	if (newlog.get(i)) {
			        	for (m = 0; m < Math.max(1, maxindex); m++) {
			        		wpReal[i].set(m, neww[k][m][0]);
			        		wpImag[i].set(m, neww[k][m][1]);
			        	} 
			        	k++;
		        	} // if (newlog.get(i)
		        } // for (i = 0, k = 0; i < wp.length; i++)
		        if (numnew < newlog.size()) {
		            qnold = new int[qn.length][qn[0].length];
		            for (i = 0; i < qn.length; i++) {
		            	for (k = 0; k < qn[0].length; k++) {
		            		qnold[i][k] = qn[i][k];
		            	}
		            }
		            qn = new int[wpReal.length][wpReal[0].size()];
		            for (i = 0, m = 0; i < wpReal.length; i++) {
		            	if (newlog.get(i)) {
			            	for (k = 0; k < wpReal[0].size(); k++) {
			            		qn[i][k] = -1;
			            	}
		            	}
		            	else {
		            		for (k = 0; k < wpReal[0].size(); k++) {
			            		qn[i][k] = qnold[m][k];
			            	}
		            		m++;
		            	}
		            } // for (i = 0, m = 0; i < wp.length; i++)
		        } //  if (numnew < newlog.size())
		        for (i = 0, m = 0; i < wpReal.length; i++) {
	            	if (newlog.get(i)) {
		            	for (k = 0; k < wpReal[0].size(); k++) {
		            		qn[i][k] = newqn[m][k];
		            	}
		            	m++;
	            	}
	            } // for (i = 0, m = 0; i < wp.length; i++)
		        
		        // Sort the columns so as to make continuous curves
		        crrsort(wpReal, wpImag, qn, Q);
		        
		        linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (k = 0; k < wpReal[0].size(); k++) {
					for (i = 0; i < wpReal.length; i++) {
						linhx[j][0].add(wpReal[i].get(k));
						linhy[j][0].add(wpImag[i].get(k));
					}
				}
				for (i = 0; i < zpReal.get(i); i++) {
					linhx[j][1].add(zpReal.get(i));
					linhy[j][1].add(zpImag.get(i));
				}
				
				iter = iter + 1;
				// Add points to zp2 where necessary
				wp = scpadapt2(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
				wpReal = new Vector[wp.length];
				wpImag = new Vector[wp.length];
				for (i = 0; i < wp.length; i++) {
					wpReal[i] = new Vector<Double>();
					wpImag[i] = new Vector<Double>();
					for (m = 0; m < wp[0].length; m++) {
						wpReal[i].add(wp[i][m][0]);
						wpImag[i].add(wp[i][m][1]);
					}
				}
				numnew = 0;
				for (i = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
						numnew++;
					}
				}
		    } // while ((numnew > 0) && (iter < maxrefn))
		} // for (j = 0; j < val.length; j++)
		
	} // crrplot0
	
	private void crrsort(Vector<Double>wpReal[], Vector<Double>wpImag[], int qnum[][], qlgraph Q) {
		// For multiple-valued maps (self-overlapping source polygons), the
		// crrmap function returns columns for all images of each source
		// point.  This function sorts those columns for crrplot0 so that 
		// each represents a continuous curve.
		int i,j, k;
		boolean mask[][];
		int diffqnum[];
		int nump;
		int p[];
		int p0;
		boolean toggle;
		int numcol;
		int col[];
		double wptemp;
		int qnumtemp;
		int len = wpReal.length;
		int m = wpReal[0].size();
		// Q.adjacent is an (n-3) by (n-3) logical matrix; it indicates which 
		// quadrilaterals share a common triangle.
		int n3 = Q.adjacent[0].length;
		// adj is a matrix of quadrilateral adjacencies.  A dummy row and column of 
		// zeros are added to simulate the "null quadrilateral."  The diagonal is set
		// to ones to the quads self-adjacent.
		boolean adj[][] = new boolean[n3+1][n3+1];
		for (i = 0; i < n3; i++) {
			for (j = 0; j < n3; j++) {
			    adj[i][j] = Q.adjacent[i][j];	
			}
		}
		for (i = 0; i < n3; i++) {
			adj[i][i] = true;
		}
		
		// Make the unused positions come from the null quadrilateral
		mask = new boolean[qnum.length][qnum[0].length];
		for (i = 0; i < qnum.length; i++) {
			for (j = 0; j < qnum[0].length; j++) {
				if (qnum[i][j] == -1) {
					mask[i][j] = true;
					qnum[i][j] = n3;
				}
			}
		} // for (i = 0; i < qnum.length; i++)
		
		if (m > 1) {
		    // For each potential curve
			for (k = 0; k < m; k++) {
			    // Indices of changes in source quadrilateral
				diffqnum = new int[len-1];
				nump = 0;
				for (i = 0; i < len-1; i++) {
					diffqnum[i] = qnum[i+1][k] - qnum[i][k];
					if (diffqnum[i] != 0) {
						nump++;
					}
				} // for (i = 0; i < len-1; i++)
				p = new int[nump];
				for (i = 0, j = 0; i < len-1; i++) {
					if (diffqnum[i] != 0) {
					    p[j++] = i;	
					}
				} // for (i = 0, j = 0; i < len-1; i++)
				toggle = !Double.isNaN(wpReal[0].get(k));
				while ((p != null) && (p.length > 0)) {
				    p0 = p[0];
				    // If the change is not to a neighbor, swap columns
				    if (!adj[qnum[p0][k]][qnum[p0+1][k]]) {
				        // Look for a column that does use a neighbor
				    	numcol = 0;
				    	for (i = 0; i < qnum[0].length; i++) {
				    	    if (adj[qnum[p0][k]][qnum[p0+1][i]]) {
				    	        numcol++;	
				    	    }
				    	} // for (i = 0; i < qnum[0].length; i++)
				    	if (numcol > 0) {
				    		col = new int[numcol];
				    		for (i = 0, j = 0; i < qnum[0].length; i++) {
					    	    if (adj[qnum[p0][k]][qnum[p0+1][i]]) {
					    	        col[j++] = i;	
					    	    }
					    	} // for (i = 0, j = 0; i < qnum[0].length; i++)
				    		// Swap
				    		for (i = p0+1; i < len; i++) {
				    		    wptemp = wpReal[i].get(col[0]);
				    		    wpReal[i].set(col[0],wpReal[i].get(k));
				    		    wpReal[i].set(k,wptemp);
				    		    wptemp = wpImag[i].get(col[0]);
				    		    wpImag[i].set(col[0],wpImag[i].get(k));
				    		    wpImag[i].set(k,wptemp);
				    		    qnumtemp = qnum[i][col[0]];
				    		    qnum[i][col[0]] = qnum[i][k];
				    		    qnum[i][k] = qnumtemp;
				    		} // for (i = p0+1; i < len; i++)
				    	} // if (numcol > 0)
				    } // if (!adj[qnum[p0][k]][qnum[p0+1][k]])
				    if (toggle) {
				    	if (Double.isNaN(wpReal[p0+1].get(k))) {
				    		break;
				    	}
				    } // if (toggle)
				    else {
				    	if (!Double.isNaN(wpReal[p0+1].get(k))) {	
				    		toggle = true;
				    	}
				    }
				    // Move to next change in quads (must recompute)
				    diffqnum = new int[len-p0-1];
				    nump = 0;
				    for (i = 0; i < len-p0-1; i++) {
				        diffqnum[i] = qnum[i+p0+2][k] - qnum[i+p0+1][k];
				        if (diffqnum[i] != 0) {
							nump++;
						}
				    }
				    p = new int[nump];
				    for (i = 0, j = 0; i < len-p0-1; i++) {
				        if (diffqnum[i] != 0) {
							p[j++] = p0 + i + 1;
						}
				    }
				} // while ((p != null) && (p.length > 0))
			} // for (k = 0; k < m; k++)
		} // if (m > 1)
		
		// Change null quads back to NaNs
		for (i = 0; i < qnum.length; i++) {
			for (j = 0; j < qnum[0].length; j++) {
				mask[i][j] = (qnum[i][j] == n3);
				if (mask[i][j]) {
					qnum[i][j] = -1;
				}
			}
		}
	} // crrsort
	
	public void crrmap(double wp[][][], int qnum[][], double zp[][],
			double w[][], double beta[], double wr[][], double betar[], double cr[], double aff[][][],
			double affr[][][], qlgraph Q, double qdat[][], double qdatr[][]) {
	    // Schwarz-Christoffel rectified map in crossratio formulation.
		// crrmap computes the values of the rectified map from wr to w at the points in vector zp.
		// The arguments are returned from crparam, crrect, and scqdata.
		
		// It is possible that the rectified polygon lies on multiple Riemann sheets; i.e., 
		// overlaps itself.  If at least one point of zp lies in more than one covering,
		// then each row of output will contain all possible values for the map at the
		// corresponding point of z.  Unused entries will be assigned NaN.
		
		// crrmap also returns the indices of the quadrilaterals used to compute the maps,
		// and the intermediate points found in the disk, in those embeddings.
		
		// Note that by switching the roles of w, beta, and aff with wr, betar, and affr,
		// one inverts the map.
		
		// Original crrmap MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j, k, kk, m, q;
		int n = w.length;
		int p = zp.length;
		double qdat2[][] = null;
		double qdatr2[][] = null;
		double tol;
		int nqpts;
		int zpindex[];
		boolean onvtx[][];
		int maxindex;
		//int shape[] = null;
		int numcopies[] = null;
	    double up[][][] = null;
		boolean onvtxcolumn[];
		int numcol;
		int colfind[];
		int nv;
		int vtxnum[];
		//int minfind = -1;
		int qn[][];
		boolean allSame;
		int idxnum;
		int idx[];
		double zpidx[][];
		double wr4[][];
		boolean mask[];
		boolean onvtx2[][];
		int nummask;
		int idx2[];
		double z[][];
		double affrq[][];
		double affq[][];
		double up1[][];
		double wp1[][];
		double dif[][];
		boolean unique[];
		int numunique;
		int idx3[];
		double wp2[][];
		double up2[][];
		
		if ((qdat == null) || (qdat.length == 0)) {
			tol = 1.0E-8;
			qdat2 = new double[8][2*beta.length+2];
			scqdata(qdat2, beta, 8);
			qdatr2 = new double[8][2*betar.length+2];
			scqdata(qdatr2, betar, 8);
		}
		else if (qdat.length == 1) {
		    tol = qdat[0][0];
		    nqpts = (int)Math.max(Math.ceil(-Math.log10(tol)), 3);
		    qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);
			qdatr2 = new double[nqpts][2*betar.length+2];
			scqdata(qdatr2, betar, nqpts);
		}
		else {
			qdat2 = qdat;
			qdatr2 = qdatr;
			tol = Math.pow(10.0, -qdat.length);
		}
		
		// Look for multiple-sheet case
		zpindex = new int[p];
		onvtx = new boolean[wr.length][p];
		isinpoly2(zpindex, onvtx, zp, wr, betar, tol);
		maxindex = -1;
		for (i = 0; i < p; i++) {
			if (zpindex[i] > maxindex) {
				maxindex = zpindex[i];
			}
		} // for (i = 0; i < p; i++)
		if (maxindex == 0) {
		    // zp consists only of points outside the domain
			for (i = 0; i < p; i++) {
		        wp[i][0][0] = Double.NaN;
				wp[i][0][1] = 0;
			}
			return;
		} // if (maxindex == 0)
		//else if (maxindex == 1) {
	         //shape = new int[]{p};
		//}
		//else {
			//shape = new int[]{p, maxindex};
		//}
		numcopies = new int[p]; // # of copies found so far per point
		up = new double[p][Math.max(1, maxindex)][2];
		for (i = 0; i < p; i++) {
			for (j = 0; j < Math.max(1, maxindex); j++) {
				wp[i][j][0] = Double.NaN;
				wp[i][j][1] = 0.0;
				up[i][j][0] = Double.NaN;
				up[i][j][1] = 0.0;
				qnum[i][j] = -1; // where each point was mapped from
			}
		} // for (i = 0; i < p; i++)
		
		// First, deal with points coincident with vertices
		onvtxcolumn = new boolean[onvtx[0].length];
		numcol = 0;
		for (j = 0; j < onvtx[0].length; j++) {
			for (i = 0; i < onvtx.length; i++) {
				if (onvtx[i][j]) {
					onvtxcolumn[j] = true;
				}
			}
			if (onvtxcolumn[j]) {
				numcol++;
			}
		} // for (j = 0; j < onvtx[0].length; j++)
		colfind = new int[numcol];
		for (i = 0, j = 0; j < onvtx[0].length; j++) {
			if (onvtxcolumn[j]) {
			    colfind[i++] = j;	
			}
		}
		qn = new int[onvtx[0].length][onvtx.length];
		for (i = 0; i < onvtx[0].length; i++) {
			for (j = 0; j < onvtx.length; j++) {
				qn[i][j] = -1;
			}
		}
		for (kk = 0; kk < numcol; kk++) {
			k = colfind[kk];
			nv = 0;
			for (i = 0; i < onvtx.length; i++) {
				if (onvtx[i][k]) {
					 nv++;
				}
			}
			vtxnum = new int[nv];
			for (i = 0, m = 0; i < onvtx.length; i++) {
				if (onvtx[i][k]) {
					 vtxnum[m++] = i;
				}
			}
			for (i = 0; i < nv; i++) {
				wp[k][i][0] = w[vtxnum[i]][0];
				wp[k][i][1] = w[vtxnum[i]][1];
			} // for (i = 0; i < nv; i++)
			// qn created here is never used
			//for (j = 0; j < nv; j++) {
			    //minfind = -1;
			    //for (m = 0; m < Q.qlvert[0].length && (minfind == -1); m++) {
			    	//for (i = 0; i < 4 && (minfind == -1); i++) {
			    		//if (vtxnum[j] == Q.qlvert[i][m]) {
			    			//minfind = m;
			    		//}
			    	//}
			    //} // for (m = 0; m < Q.qlvert[0].length && (minfind == -1); m++)
			    //qn[k][j] = minfind;
			//} // for (j = 0; j < nv; j++)
			numcopies[k] = nv;
		} // for (kk = 0; kk < numcol; kk++)
		
		// For each embedding, compose inverse rectified & forwrward original maps
		// for points of zp in the quadrilateral
		for (q = 0; q < n-3; q++) {
		    // Short-circuit if all is done	
			allSame = true;
			for (i = 0; i < p && allSame; i++) {
				if (numcopies[i] != zpindex[i]) {
					allSame = false;
				}
			} // for (i = 0; i < p && allSame; i++) 
			if (allSame) {
				break;
			}
			
			// Still need to be mapped
			idxnum = 0;
			for (i = 0; i < p; i++) {
				if (numcopies[i] < zpindex[i]) {
					idxnum++;
				}
			}
			idx = new int[idxnum];
			for (i = 0, j = 0; i < p; i++) {
				if (numcopies[i] < zpindex[i]) {
					idx[j++] = i;
				}
			}
			
			// Those that are inside quadrilateral q
			mask = new boolean[idxnum];
			onvtx2 = new boolean[4][idxnum];
			zpidx = new double[idxnum][2];
			for (i = 0; i < idxnum; i++) {
				zpidx[i][0] = zp[idx[i]][0];
				zpidx[i][1] = zp[idx[i]][1];
			}
			wr4 = new double[4][2];
			for (i = 0; i < 4; i++) {
				wr4[i][0] = wr[Q.qlvert[i][q]][0]; 
				wr4[i][1] = wr[Q.qlvert[i][q]][1];
			}
			isinpoly(mask, onvtx2, zpidx, wr4, null, tol);
			
			// If a point was mapped from a neighboring quadrilateral, exclude it,
			// since it would be a repeat
			if (maxindex > 1) {
				qn = new int[maxindex][idxnum];
				
			     for (i = 0; i < idxnum; i++) {
			    	 for (j = 0; j < maxindex; j++) {
			    	    qn[j][i] = qnum[idx[i]][j];	 
			    	 }
			     }
			     for (i = 0; i < maxindex; i++) {
			    	 for (j = 0; j < idxnum; j++) {
			    		 if (qn[i][j] != -1) {
			    			 if (Q.adjacent[q][qn[i][j]]) {
			    				 qn[i][j] = 0;
			    			 }
			    		 }
			    	 }
			     }
			     for (j = 0; j < idxnum; j++) {
			    	 for (i = 0; i < maxindex; i++) {
			    		 if (qn[i][j] == 0) {
			    			 mask[j] = false;
			    		 }
			    	 }
			     }
			} // if (maxindex > 1)
			
			nummask = 0;
			for (i = 0; i < idxnum; i++) {
				if (mask[i]) {
					nummask++;
				}
			}
			
			if (nummask > 0) {
				// Proceed
				idx2 = new int[nummask];
				for (i = 0, j = 0; i < idxnum; i++) {
					if (mask[i]) {
					    idx2[j++] = idx[i];
					}
				}
				z = crembed(cr, Q, q);
				zpidx = new double[nummask][2];
				for (i = 0; i < nummask; i++) {
					zpidx[i][0] = zp[idx2[i]][0];
					zpidx[i][1] = zp[idx2[i]][1];
				}
				affrq = new double[affr[0].length][2];
				for (i = 0; i < affr[0].length; i++) {
					affrq[i][0] = affr[q][i][0];
					affrq[i][1] = affr[q][i][1];
				}
				boolean ode = true;
				boolean newton = true;
				int maxiter = 10;
				up1 = crimap0(zpidx, z, betar, affrq, qdatr, ode, newton, tol, maxiter);
				affq = new double[aff[0].length][2];
				for (i = 0; i < aff[0].length; i++) {
					affq[i][0] = aff[q][i][0];
					affq[i][1] = aff[q][i][1];
				}
				wp1 = crmap0(up1, z, beta, affq, qdat);
				// As a further check on repeats, compare values to previous ones
				// (Points may lie on diagonals and appear to be in non-neighboring
				// quadrilaterals)
				if (maxindex > 1) {
				    dif = new double[nummask][maxindex];
				    for (i = 0; i < nummask; i++) {
				    	for (j = 0; j < maxindex; j++) {
				    		dif[i][j] = zabs(wp1[i][0] - wp[idx2[i]][j][0], wp1[i][1] - wp[idx2[i]][j][1]);
				    	}
				    }
				    unique = new boolean[nummask];
				    numunique = 0;
				    for (i = 0; i < nummask; i++) {
				    	unique[i] = true;
				    	for (j = 0; j < maxindex; j++) {
				    	    if (dif[i][j] < 10.0*tol) {
				    	    	unique[i] = false;
				    	    }
				    	}
				    	if (unique[i]) {
				    		numunique++;
				    	}
				    } // for (i = 0; i < nummask; i++)
				    wp2 = new double[numunique][2];
				    up2 = new double[numunique][2];
				    idx3 = new int[numunique];
				    for (i = 0, j = 0; i < nummask; i++) {
				        if (unique[i]) {
				        	wp2[j][0] = wp1[i][0];
				        	wp2[j][1] = wp1[i][1];
				        	up2[j][0] = up1[i][0];
				        	up2[j][1] = up1[i][1];
				        	idx3[j++] = idx2[i];
				        }
				    }
				} // if (maxindex > 1)
				else {
					wp2 = wp1;
					up2 = up1;
					idx3 = idx2;
				}
				// Fill in new values
				for (i = 0; i < idx3.length; i++) {
					wp[idx3[i]][numcopies[idx3[i]]][0] = wp2[i][0];
					wp[idx3[i]][numcopies[idx3[i]]][1] = wp2[i][1];
					up[idx3[i]][numcopies[idx3[i]]][0] = up2[i][0];
					up[idx3[i]][numcopies[idx3[i]]][1] = up2[i][1];
					qnum[idx3[i]][numcopies[idx3[i]]] = q;
					numcopies[idx3[i]] = numcopies[idx3[i]] + 1;
				}
			} // if (nummask > 0)
		} // for (q = 0; q < n-3; q++)
	} // crrmap
	
	private void rplot(double w[][], double beta[], double z[][], double c[],
			double L[], int re, int im, int yInvert) {
		// Image of cartesian grid under Schwarz-Christoffel rectangle map.
		// rplot will adaptively plot the images under the Schwarz-Christoffel
		// rectangle map of re evenly spaced vertical and im evenly spaced
		// horizontal lines.
		// Extracted form original routine by Toby Driscoll copyright 1998.
		
		// Number of quadrature points per integration.
		// Approximately equals -log10(error).  Increase if plot
		// has false little zigzags in curves. 
		int nqpts = 5; 
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
        // Max allowed number of adaptive refinements made to meet other requirements 
        int maxrefn = 16;
        double axlim[] = new double[4];
		int i, j, m;
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		Vector<Double>zpReal = new Vector<Double>();
		Vector<Double>zpImag = new Vector<Double>();
		Vector<Double>wpReal = new Vector<Double>();
		Vector<Double>wpImag = new Vector<Double>();
		Vector<Boolean>newlog = new Vector<Boolean>();
		int n = w.length;
		int corners[] = new int[4];
		int renum[] = new int[w.length];
		rcorners(w, beta, z, corners, renum);
		double rect[][] = new double[4][2];
		for (i = 0; i < 4; i++) {
			rect[i][0] = z[corners[i]][0];
			rect[i][1] = z[corners[i]][1];
		}
		
		double Kp = rect[1][1];
		// double K[] = new double[2];
		// For testRectmap1 K[0] = 2.1555087314127523 K[1] = 0.0
		// K[0] = rect[0][0];
		//K[1] = rect[0][1];
	    double K = rect[0][0];
		double recoords[] = new double[re];
		double rexspacing = 2.0*K/(re+1);
		for (i = 1; i <= re; i++) {
			recoords[i-1] = -K + i*rexspacing;
		}
		double imcoords[] = new double[im];
		double imspacing = Kp/(im+1);
		for (i = 1; i <= im; i++) {
			imcoords[i-1] = i*imspacing;
		}
		float xPointArray[] = new float[n+1];
		float yPointArray[] = new float[n+1];
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, w, beta, false, axlim, yInvert, true, null);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
        double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		// Plot vertical lines
		Vector<Double> linhx[][] = new Vector[re][2];
		Vector<Double> linhy[][] = new Vector[re][2];
		for (i = 0; i < re; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		for (j = 0; j < re; j++) {
		    // Start evenly spaced
			zpReal.clear();
			zpImag.clear();
			for (i = 0; i < 15; i++) {
				zpReal.add(recoords[j]);
				zpImag.add(i*Kp/14.0);
			}
			newlog.clear();
			for (i = 0; i < 15; i++) {
				newlog.add(true);
			}
			wpReal.clear();
			wpImag.clear();
			for (i = 0; i < 15; i++) {
				wpReal.add(Double.NaN);
				wpImag.add(0.0);
			}
			
			// The individual points will be shown as they are found.
			
			// Adaptive refinement to make curve smooth
			int iter = 0;
			while (iter < maxrefn) {
				int numnewlog = 0;
				for (i = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	numnewlog++;
				    }
				} // for (i = 0; i < newlog.size(); i++)
				if (numnewlog == 0) {
					break;
				}
				double zpnew[][] = new double[numnewlog][2];
				for (i = 0, m = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	zpnew[m][0] = zpReal.get(i);
				    	zpnew[m++][1] = zpImag.get(i);
				    }
				} // for (i = 0, m = 0; i < newlog.length; i++)
				double neww[][] = new double[numnewlog][2];
				rmap(neww, zpnew, w, beta, z, c, L, qdat);
				for (i = 0, m = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
					    wpReal.set(i, neww[m][0]);
					    wpImag.set(i, neww[m++][1]);
					} 
				} // for (i = 0, m = 0; i < newlog.size(); i++)
				iter = iter + 1;
				
				linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (i = 0; i < wpReal.size(); i++) {
				    linhx[j][0].add(wpReal.get(i));
				    linhy[j][0].add(wpImag.get(i));
				    linhx[j][1].add(zpReal.get(i));
				    linhy[j][1].add(zpImag.get(i));
				}
				
				// Add points to zp where necessary
				scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
			} // while (iter < maxrefn)
		} // for (j = 0; j < re; j++)
		
		
		for (i = 0; i < re; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		}
		
		// Plot horizontal lines
		linhx = new Vector[im][2];
		linhy = new Vector[im][2];
		for (i = 0; i < im; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		for (j = 0; j < im; j++) {
		    // Start evenly spaced
			zpReal.clear();
			zpImag.clear();
			for (i = 0; i < 15; i++) {
				zpReal.add(-K + 2.0*i*K/14.0);
				zpImag.add(imcoords[j]);
			}
			newlog.clear();
			for (i = 0; i < 15; i++) {
				newlog.add(true);
			}
			wpReal.clear();
			wpImag.clear();
			for (i = 0; i < 15; i++) {
				wpReal.add(Double.NaN);
				wpImag.add(0.0);
			}
			
			// The individual points will be shown as they are found.
			
			// Adaptive refinement to make curve smooth
			int iter = 0;
			while (iter < maxrefn) {
				int numnewlog = 0;
				for (i = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	numnewlog++;
				    }
				} // for (i = 0; i < newlog.size(); i++)
				if (numnewlog == 0) {
					break;
				}
				double zpnew[][] = new double[numnewlog][2];
				for (i = 0, m = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	zpnew[m][0] = zpReal.get(i);
				    	zpnew[m++][1] = zpImag.get(i);
				    }
				} // for (i = 0, m = 0; i < newlog.length; i++)
				double neww[][] = new double[numnewlog][2];
				rmap(neww, zpnew, w, beta, z, c, L, qdat);
				for (i = 0, m = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
					    wpReal.set(i, neww[m][0]);
					    wpImag.set(i, neww[m++][1]);
					} 
				} // for (i = 0, m = 0; i < newlog.size(); i++)
				iter = iter + 1;
				
				linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (i = 0; i < wpReal.size(); i++) {
				    linhx[j][0].add(wpReal.get(i));
				    linhy[j][0].add(wpImag.get(i));
				    linhx[j][1].add(zpReal.get(i));
				    linhy[j][1].add(zpImag.get(i));
				}
				
				// Add points to zp where necessary
				scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
			} // while (iter < maxrefn)
		} // for (j = 0; j < im; j++)
		
		
		for (i = 0; i < im; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		}
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
	}
	
	public void scpadapt(Vector<Double> zpReal, Vector<Double> zpImag,
		Vector<Double> wpReal, Vector<Double> wpImag, Vector<Boolean> newlog,
			double minlen, double maxlen, double clip[]) {
		// This function looks for unacceptable nonsmoothness in the curve(s)
		// represented by wp.  At such points it adds in-between points to zp.
		// On return zp is the combined vector of points in order, wp has NaN's 
		// at the new points, and newlog is a 0-1 vector flagging the newly added 
		// points.
		
		// The algorithm is basically stolen from fplot.  If extrapolation of the
		// linear interpolation that the graphics will use results in an estimate
		// too far from reality, refinement is called for.
		
		// When the clip argument is used, the criteria do not apply for points
		// outside the clip region.  Why refine for invisible points?
		
		// wp can have multiple columns (curves).  Any curve needing refinement
		// causes it for all of them.
		
		// This should become the adaptive routine for all SC plotting.  Allowances
		// must be made for cases when the ends of zp need not be bounded (as with
		// the half-plane and strip).  Perhaps the newly added points should be
		// spaced exponentially rather than added algebraically.
		
		// Original MATLAB routine copyright 1997-2003 by Toby Driscoll.
		int i, j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int m = wpReal.size();
		double dwp[][] = new double[m-1][2];
		for (i = 0; i < m-1; i++) {
			dwp[i][0] = wpReal.get(i+1) - wpReal.get(i);
			dwp[i][1] = wpImag.get(i+1) - wpImag.get(i);
		}
		
		// Infinities at the ends of zp mean that we could go out forever.  However,
		// we will pick a finite bound.
		boolean linf = (Double.isInfinite(zpReal.get(0)) || Double.isInfinite(zpImag.get(0)));
		if (linf) {
			double absVal = zabs(zpReal.get(2) - zpReal.get(1), zpImag.get(2) - zpImag.get(1));
			double maxVal = Math.max(3, absVal * absVal);
			double signVal[] = sign(zpReal.get(1) - zpReal.get(2),
					                zpImag.get(1) - zpImag.get(2));
			zpReal.set(0, zpReal.get(1) + maxVal*signVal[0]);
			zpImag.set(0, zpImag.get(1) + maxVal*signVal[1]);
		} // if (linf)
		boolean rinf = (Double.isInfinite(zpReal.get(m-1)) || Double.isInfinite(zpImag.get(m-1)));
		if (rinf) {
			double absVal = zabs(zpReal.get(m-2) - zpReal.get(m-3), 
					zpImag.get(m-2) - zpImag.get(m-3));	
			double maxVal = Math.max(3, absVal * absVal);
			double signVal[] = sign(zpReal.get(m-2) - zpReal.get(m-3),
	                zpImag.get(m-2) - zpImag.get(m-3));
			zpReal.set(m-1, zpReal.get(m-2) + maxVal*signVal[0]);
			zpImag.set(m-1, zpImag.get(m-2) + maxVal*signVal[1]);
		} // if (rinf)
		
		// Sines of the turning angles at interior points
		double sines[] = new double[m-2];
		for (i = 0; i < m-2; i++) {
		    zmlt(dwp[i][0], dwp[i][1], dwp[i+1][0], -dwp[i+1][1], cr, ci);
		    double ang = Math.atan2(ci[0], cr[0]);
		    double sinang = Math.sin(ang);
		    sines[i] = Math.abs(sinang);
		} // for (i = 0; i < m-2; i++)
		
		// Distances from linear extrapolation to actual value.  Each interior
		// point has a column, with two rows being errors to either side.
		double absdwp[] = new double[m-1];
		for (i = 0; i < m-1; i++) {
			absdwp[i] = zabs(dwp[i][0], dwp[i][1]);
		}
		double err[][] = new double[m-2][2];
		for (i = 0; i < m-2; i++) {
			err[i][0] = absdwp[i]*sines[i];
			err[i][1] = absdwp[i+1]*sines[i];
		}
		
		
		// Forget about NaNs
		for (i = 0; i < m-2; i++) {
			for (j = 0; j < 2; j++) {
				if (Double.isNaN(err[i][j])) {
					err[i][j] = 0.0;
				}
			}
		}
		
		for (i = 0; i < absdwp.length; i++) {
			if (Double.isNaN(absdwp[i])) {
				absdwp[i] = 0;
			}
		}
		
		// Flag large errors
		boolean bad[][] = new boolean[m][2];
		for (i = 1; i < m-1; i++) {
			for (j = 0; j < 2; j++) {
				if (err[i-1][j] > maxlen/12.0) {
					bad[i][j] = true;
				}
			}
		}
		
		// Also flag if the segments themselves are too long.
		// However, exclude where segments are very short.
		boolean longv[] = new boolean[m-1];
		boolean shortv[] = new boolean[m-1];
		for (i = 0; i < m-1; i++) {
			if (absdwp[i] > maxlen) {
				longv[i] = true;
			}
			if (absdwp[i] < minlen) {
				shortv[i] = true;
			}
		} //  for (i = 0; i < m-1; i++)
		bad[0][0] = false;
		for (i = 1; i < m; i++) {
			bad[i][0] = (bad[i][0] || longv[i-1]) && (!shortv[i-1]);
		}
		for (i = 0; i < m-1; i++) {
			bad[i][1] = (bad[i][1] || longv[i]) && (!shortv[i]);
		}
		bad[m-1][1] = false;
		
		// Find clipped points
		double xp[] = new double[m];
		for (i = 0; i < m; i++) {
			xp[i] = wpReal.get(i);
		}
		double yp[] = new double[m];
		for (i = 0; i < m; i++) {
			yp[i] = wpImag.get(i);
		}
		boolean lohi[] = new boolean[m];
		boolean lfrt[] = new boolean[m];
		for (i = 0; i < m; i++) {
			lohi[i] = (yp[i] < clip[2]) || (yp[i] > clip[3]) || Double.isInfinite(xp[i]) ||
					   Double.isInfinite(yp[i]);
			lfrt[i] = (xp[i] < clip[0]) || (xp[i] > clip[1]) || Double.isInfinite(xp[i]) ||
					   Double.isInfinite(yp[i]);
		}
		
		// To be refined, you or a neighbor must be inside
		boolean inside[] = new boolean[m];
		for (i = 0; i < m; i++) {
			inside[i] = (!lohi[i]) && (!lfrt[i]);
		}
		
		for (i = 1; i < m; i++) {
			bad[i][0] = bad[i][0] && (inside[i] || inside[i-1]);
		}
		for (i = 0; i < m-1; i++) {
			bad[i][1] = bad[i][1] && (inside[i] || inside[i+1]);
		}
		
		// Exception: Extend the lines that bound the axes box.  If you cross one
		// horizontal and one vertical among those lines between adjacent points,
		// you must refine in-between.  Otherwise, you could miss valid inside
		// points, or get a line that crosses back into the axes box.
		boolean lh[] = new boolean[m];
		boolean lr[] = new boolean[m];
		for (i = 0; i < m; i++) {
			lh[i] = lohi[i] && (!lfrt[i]);
			lr[i] = lfrt[i] && (!lohi[i]);
		}
		boolean flag[] = new boolean[m-1];
		int numflag = 0;
		for (i = 0; i < m-1; i++) {
			flag[i] = (lh[i] && lr[i+1]) || (lr[i] && lh[i+1]);
			if (flag[i]) {
				numflag++;
			}
		} // for (i = 0; i < m-1; i++)
		if (numflag > 0) {
		    for (i = 0; i < m-1; i++) {
		    	if (flag[i]) {
		    		bad[i+1][0] = true;
		    		bad[i][1] = true;
		    	}
		    }
		} // if (numflag > 0)
		
		// Refinement
		double nans[][] = new double[m][2];
		for (i = 0; i < m; i++) {
			nans[i][0] = Double.NaN;
		}
		double zp2[][][] = new double[3][m][2];
		for (i = 0; i < m; i++) {
			zp2[0][i][0] = Double.NaN;
			zp2[1][i][0] = zpReal.get(i);
			zp2[1][i][1] = zpImag.get(i);
			zp2[2][i][0] = Double.NaN;
		}
		double new2[][] = new double[3][m];
		for (i = 0; i < m; i++) {
			new2[0][i] = Double.NaN;
			new2[2][i] = Double.NaN;
		}
		
		// Add new source points.  Always go one-third of the distance toward
		// the offending party.
		int numback = 0;
		for (i = 0; i < m; i++) {
			if (bad[i][0]) {
				numback++;
			}
		} // for (i = 0; i < m; i++)
		if (numback > 0) {
		    int back[] = new int[numback];
		    for (i = 0, j = 0; i < m; i++) {
		    	if (bad[i][0]) {
		    		back[j++] = i;
		    	}
		    } // for (i = 0, j = 0; i < m; i++)
		    for (i = 0; i < numback; i++) {
		    	j = back[i];
		    	zp2[0][j][0] = (zp2[1][j-1][0] + 2.0*zp2[1][j][0])/3.0;
		    	zp2[0][j][1] = (zp2[1][j-1][1] + 2.0*zp2[1][j][1])/3.0;
		    	new2[0][j] = 1;
		    }
		} // if (numback > 0)
		int numforw = 0;
		for (i = 0; i < m; i++) {
			if (bad[i][1]) {
				numforw++;
			}
		} // for (i = 0; i < m; i++)
		if (numforw > 0) {
		    int forw[] = new int[numforw];
		    for (i = 0, j = 0; i < m; i++) {
		    	if (bad[i][1]) {
		    		forw[j++] = i;
		    	}
		    } // for (i = 0, j = 0; i < m; i++)
		    for (i = 0; i < numforw; i++) {
		    	j = forw[i];
		    	zp2[2][j][0] = (zp2[1][j+1][0] + 2.0*zp2[1][j][0])/3.0;
		    	zp2[2][j][1] = (zp2[1][j+1][1] + 2.0*zp2[1][j][1])/3.0;
		    	new2[2][j] = 1;
		    }
		} // if (numforw > 0)
		
		// If we had infinities at the ends, put them back
		if (linf) {
			zp2[1][0][0] = Double.POSITIVE_INFINITY;
		}
		if (rinf) {
			zp2[1][m-1][0] = Double.POSITIVE_INFINITY;
		}
		
		// Restack zp into a vector, deleting NaNs
		zpReal.clear();
		zpImag.clear();
		newlog.clear();
		for (j = 0; j < m; j++) {
			for (i = 0; i < 3; i++) {
				if ((!Double.isNaN(zp2[i][j][0])) && (!Double.isNaN(zp2[i][j][1]))) {
				    zpReal.add(zp2[i][j][0]);
				    zpImag.add(zp2[i][j][1]);
				    if (new2[i][j] == 1) {
				        newlog.add(true);
				    }
				    else {
				    	newlog.add(false);
				    }
				}
			}
		} // for (j = 0; j < m; j++)
		
		// Update wp
		double wpold[][] =  new double[wpReal.size()][2];
		for (i = 0; i < wpReal.size(); i++) {
			wpold[i][0] = wpReal.get(i);
			wpold[i][1] = wpImag.get(i);
		}
		wpReal.clear();
		wpImag.clear();
		for (i = 0; i < zpReal.size(); i++) {
			wpReal.add(Double.NaN);
			wpImag.add(0.0);
		}
		for (i = 0, j = 0; i < zpReal.size(); i++) {
			if (!newlog.get(i)) {
				wpReal.set(i, wpold[j][0]);
				wpImag.set(i, wpold[j++][1]);
			}
		}
	}
	
	private double[][][] scpadapt2(Vector<Double> zpReal, Vector<Double> zpImag,
			Vector<Double> wpReal[], Vector<Double> wpImag[], Vector<Boolean> newlog,
				double minlen, double maxlen, double clip[]) {
			// This function looks for unacceptable nonsmoothness in the curve(s)
			// represented by wp.  At such points it adds in-between points to zp.
			// On return zp is the combined vector of points in order, wp has NaN's 
			// at the new points, and newlog is a 0-1 vector flagging the newly added 
			// points.
			
			// The algorithm is basically stolen from fplot.  If extrapolation of the
			// linear interpolation that the graphics will use results in an estimate
			// too far from reality, refinement is called for.
			
			// When the clip argument is used, the criteria do not apply for points
			// outside the clip region.  Why refine for invisible points?
			
			// wp can have multiple columns (curves).  Any curve needing refinement
			// causes it for all of them.
			
			// This should become the adaptive routine for all SC plotting.  Allowances
			// must be made for cases when the ends of zp need not be bounded (as with
			// the half-plane and strip).  Perhaps the newly added points should be
			// spaced exponentially rather than added algebraically.
			
			// Original MATLAB routine copyright 1997-2003 by Toby Driscoll.
			int i, j, k;
			double cr[] = new double[1];
			double ci[] = new double[1];
			int m = wpReal.length;
			double dwp[][][] = new double[m-1][wpReal[0].size()][2];
			for (i = 0; i < m-1; i++) {
				for (j = 0; j < wpReal[0].size(); j++) {
					dwp[i][j][0] = wpReal[i+1].get(j) - wpReal[i].get(j);
					dwp[i][j][1] = wpImag[i+1].get(j) - wpImag[i].get(j);
				}
			}
			
			// Infinities at the ends of zp mean that we could go out forever.  However,
			// we will pick a finite bound.
			boolean linf = (Double.isInfinite(zpReal.get(0)) || Double.isInfinite(zpImag.get(0)));
			if (linf) {
				double absVal = zabs(zpReal.get(2) - zpReal.get(1), zpImag.get(2) - zpImag.get(1));
				double maxVal = Math.max(3, absVal * absVal);
				double signVal[] = sign(zpReal.get(1) - zpReal.get(2),
						                zpImag.get(1) - zpImag.get(2));
				zpReal.set(0, zpReal.get(1) + maxVal*signVal[0]);
				zpImag.set(0, zpImag.get(1) + maxVal*signVal[1]);
			} // if (linf)
			boolean rinf = (Double.isInfinite(zpReal.get(m-1)) || Double.isInfinite(zpImag.get(m-1)));
			if (rinf) {
				double absVal = zabs(zpReal.get(m-2) - zpReal.get(m-3), 
						zpImag.get(m-2) - zpImag.get(m-3));	
				double maxVal = Math.max(3, absVal * absVal);
				double signVal[] = sign(zpReal.get(m-2) - zpReal.get(m-3),
		                zpImag.get(m-2) - zpImag.get(m-3));
				zpReal.set(m-1, zpReal.get(m-2) + maxVal*signVal[0]);
				zpImag.set(m-1, zpImag.get(m-2) + maxVal*signVal[1]);
			} // if (rinf)
			
			// Sines of the turning angles at interior points
			double sines[][] = new double[m-2][wpReal[0].size()];
			for (i = 0; i < m-2; i++) {
				for (j = 0; j < wpReal[0].size(); j++) {
				    zmlt(dwp[i][j][0], dwp[i][j][1], dwp[i+1][j][0], -dwp[i+1][j][1], cr, ci);
				    double ang = Math.atan2(ci[0], cr[0]);
				    double sinang = Math.sin(ang);
				    sines[i][j] = Math.abs(sinang);
				} // for (j = 0; j < wpReal[0].size(); j++)
			} // for (i = 0; i < m-2; i++)
			
			// Distances from linear extrapolation to actual value.  Each interior
			// point has a column, with two rows being errors to either side.
			double absdwp[][] = new double[m-1][wpReal[0].size()];
			for (i = 0; i < m-1; i++) {
				for (j = 0; j < wpReal[0].size(); j++) {
				    absdwp[i][j] = zabs(dwp[i][j][0], dwp[i][j][1]);
				}
			}
			double err[][] = new double[m-2][2];
			for (i = 0; i < m-2; i++) {
				err[i][0] = -Double.MAX_VALUE;
				err[i][1] = -Double.MAX_VALUE;
				for (j = 0; j < wpReal[0].size(); j++) {
					double val0 = absdwp[i][j]*sines[i][j];
					if (val0 > err[i][0]) {
						err[i][0] = val0;
					}
					double val1 = absdwp[i+1][j]*sines[i][j];
					if (val1 > err[i][1]) {
						err[i][1] = val1;
					}
				   
				}
			}
			
			
			// Forget about NaNs
			for (i = 0; i < m-2; i++) {
				for (j = 0; j < 2; j++) {
					if (Double.isNaN(err[i][j])) {
						err[i][j] = 0.0;
					}
				}
			}
			
			for (i = 0; i < absdwp.length; i++) {
				for (j = 0; j < absdwp[0].length; j++) {
					if (Double.isNaN(absdwp[i][j])) {
						absdwp[i][j] = 0;
					}
				}
			}
			
			// Flag large errors
			boolean bad[][] = new boolean[m][2];
			for (i = 1; i < m-1; i++) {
				for (j = 0; j < 2; j++) {
					if (err[i-1][j] > maxlen/12.0) {
						bad[i][j] = true;
					}
				}
			}
			
			// Also flag if the segments themselves are too long.
			// However, exclude where segments are very short.
			boolean longv[] = new boolean[m-1];
			boolean shortv[] = new boolean[m-1];
			for (i = 0; i < m-1; i++) {
				shortv[i] = true;
				for (j = 0; j < wpReal[0].size(); j++) {
					if (absdwp[i][j] > maxlen) {
						longv[i] = true;
					}
					if (absdwp[i][j] >= minlen) {
						shortv[i] = false;
					}
				} // for (j = 0; j < wpReal[0].size(); j++)
			} //  for (i = 0; i < m-1; i++)
			bad[0][0] = false;
			for (i = 1; i < m; i++) {
				bad[i][0] = (bad[i][0] || longv[i-1]) && (!shortv[i-1]);
			}
			for (i = 0; i < m-1; i++) {
				bad[i][1] = (bad[i][1] || longv[i]) && (!shortv[i]);
			}
			bad[m-1][1] = false;
			
			// Find clipped points
			double xp[][] = new double[m][wpReal[0].size()];
			for (i = 0; i < m; i++) {
				for (j = 0; j < wpReal[0].size(); j++) {
				    xp[i][j] = wpReal[i].get(j);
				}
			}
			double yp[][] = new double[m][wpReal[0].size()];
			for (i = 0; i < m; i++) {
				for (j = 0; j < wpReal[0].size(); j++) {
				    yp[i][j] = wpImag[i].get(j);
				}
			}
			boolean lohi[][] = new boolean[m][wpReal[0].size()];
			boolean lfrt[][] = new boolean[m][wpReal[0].size()];
			for (i = 0; i < m; i++) {
				for (j = 0; j < wpReal[0].size(); j++) {
					lohi[i][j] = (yp[i][j] < clip[2]) || (yp[i][j] > clip[3]) || Double.isInfinite(xp[i][j]) ||
							   Double.isInfinite(yp[i][j]);
					lfrt[i][j] = (xp[i][j] < clip[0]) || (xp[i][j] > clip[1]) || Double.isInfinite(xp[i][j]) ||
							   Double.isInfinite(yp[i][j]);
				}
			}
			
			// To be refined, you or a neighbor must be inside
			boolean inside[] = new boolean[m];
			for (i = 0; i < m; i++) {
				inside[i] = true;
				for (j = 0; j < wpReal[0].size(); j++) {
				    if (lohi[i][j] || lfrt[i][j]) {
				    	inside[i] = false;
				    }
				}
			}
			
			for (i = 1; i < m; i++) {
				bad[i][0] = bad[i][0] && (inside[i] || inside[i-1]);
			}
			for (i = 0; i < m-1; i++) {
				bad[i][1] = bad[i][1] && (inside[i] || inside[i+1]);
			}
			
			// Exception: Extend the lines that bound the axes box.  If you cross one
			// horizontal and one vertical among those lines between adjacent points,
			// you must refine in-between.  Otherwise, you could miss valid inside
			// points, or get a line that crosses back into the axes box.
			boolean lh[][] = new boolean[m][wpReal[0].size()];
			boolean lr[][] = new boolean[m][wpReal[0].size()];
			for (i = 0; i < m; i++) {
				for (j = 0; j < wpReal[0].size(); j++) {
				    lh[i][j] = lohi[i][j] && (!lfrt[i][j]);
				    lr[i][j] = lfrt[i][j] && (!lohi[i][j]);
				}
			}
			boolean flaginit[][] = new boolean[m-1][wpReal[0].size()];
			boolean flag[] = new boolean[m-1];
			int numflag = 0;
			for (i = 0; i < m-1; i++) {
				for (j = 0; j < wpReal[0].size(); j++) {
				    flaginit[i][j] = (lh[i][j] && lr[i+1][j]) || (lr[i][j] && lh[i+1][j]);
				    if (flaginit[i][j]) {
				    	flag[i] = true;
				    	numflag++;
				    }
				}
			} // for (i = 0; i < m-1; i++)
			if (numflag > 0) {
			    for (i = 0; i < m-1; i++) {
			    	if (flag[i]) {
			    		bad[i+1][0] = true;
			    		bad[i][1] = true;
			    	}
			    }
			} // if (numflag > 0)
			
			// Refinement
			double nans[][] = new double[m][2];
			for (i = 0; i < m; i++) {
				nans[i][0] = Double.NaN;
			}
			double zp2[][][] = new double[3][m][2];
			for (i = 0; i < m; i++) {
				zp2[0][i][0] = Double.NaN;
				zp2[1][i][0] = zpReal.get(i);
				zp2[1][i][1] = zpImag.get(i);
				zp2[2][i][0] = Double.NaN;
			}
			double new2[][] = new double[3][m];
			for (i = 0; i < m; i++) {
				new2[0][i] = Double.NaN;
				new2[2][i] = Double.NaN;
			}
			
			// Add new source points.  Always go one-third of the distance toward
			// the offending party.
			int numback = 0;
			for (i = 0; i < m; i++) {
				if (bad[i][0]) {
					numback++;
				}
			} // for (i = 0; i < m; i++)
			if (numback > 0) {
			    int back[] = new int[numback];
			    for (i = 0, j = 0; i < m; i++) {
			    	if (bad[i][0]) {
			    		back[j++] = i;
			    	}
			    } // for (i = 0, j = 0; i < m; i++)
			    for (i = 0; i < numback; i++) {
			    	j = back[i];
			    	zp2[0][j][0] = (zp2[1][j-1][0] + 2.0*zp2[1][j][0])/3.0;
			    	zp2[0][j][1] = (zp2[1][j-1][1] + 2.0*zp2[1][j][1])/3.0;
			    	new2[0][j] = 1;
			    }
			} // if (numback > 0)
			int numforw = 0;
			for (i = 0; i < m; i++) {
				if (bad[i][1]) {
					numforw++;
				}
			} // for (i = 0; i < m; i++)
			if (numforw > 0) {
			    int forw[] = new int[numforw];
			    for (i = 0, j = 0; i < m; i++) {
			    	if (bad[i][1]) {
			    		forw[j++] = i;
			    	}
			    } // for (i = 0, j = 0; i < m; i++)
			    for (i = 0; i < numforw; i++) {
			    	j = forw[i];
			    	zp2[2][j][0] = (zp2[1][j+1][0] + 2.0*zp2[1][j][0])/3.0;
			    	zp2[2][j][1] = (zp2[1][j+1][1] + 2.0*zp2[1][j][1])/3.0;
			    	new2[2][j] = 1;
			    }
			} // if (numforw > 0)
			
			// If we had infinities at the ends, put them back
			if (linf) {
				zp2[1][0][0] = Double.POSITIVE_INFINITY;
			}
			if (rinf) {
				zp2[1][m-1][0] = Double.POSITIVE_INFINITY;
			}
			
			// Restack zp into a vector, deleting NaNs
			zpReal.clear();
			zpImag.clear();
			newlog.clear();
			for (j = 0; j < m; j++) {
				for (i = 0; i < 3; i++) {
					if ((!Double.isNaN(zp2[i][j][0])) && (!Double.isNaN(zp2[i][j][1]))) {
					    zpReal.add(zp2[i][j][0]);
					    zpImag.add(zp2[i][j][1]);
					    if (new2[i][j] == 1) {
					        newlog.add(true);
					    }
					    else {
					    	newlog.add(false);
					    }
					}
				}
			} // for (j = 0; j < m; j++)
			
			// Update wp
			double wp[][][] = new double[zpReal.size()][wpReal[0].size()][2];
			for (i = 0; i < zpReal.size(); i++) {
				for (j = 0; j < wpReal[0].size(); j++) {
					wp[i][j][0] = Double.NaN;
				}
			}
			for (i = 0, k = 0; i < zpReal.size(); i++) {
				if (!newlog.get(i)) {
					for (j = 0; j < wpReal[0].size(); j++) {
					    wp[i][j][0] = wpReal[k].get(j);
					    wp[i][j][1] = wpImag[k].get(j);
					}
					k++;
				}
			}
			return wp;
		}
	
	private void rmap(double wp[][], double zp[][], double w[][], double beta[],
			double z[][], double c[], double L[], double qdat[][]) {
		// Schwarz-Christoffel rectangle map
		// rmap computes the values of the Schwarz-Christoffel rectangle map
		// at the points in the vector zp.  wp is the same size as zp.
		// The remaining arguments are in as rparam
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j;
		double qdat2[][] = null;
		if ((zp == null) || (zp.length == 0)) {
			wp = null;
			return;
		}
		int n = w.length;
		int corners[] = new int[4];
		int renum[] = new int[w.length];
		rcorners(w, beta, z, corners, renum);
		
		if (qdat == null) {
			qdat2 = new double[8][2*beta.length+2];
			scqdata(qdat2, beta, 8);		
		}
		else if ((qdat.length == 1) && (qdat[0].length == 1)) {
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 8);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else {
			qdat2 = qdat;
		}
		
		// Map prevertices to strip.
		double K = -Double.MAX_VALUE;
		for (i = 0; i < z.length; i++) {
			if (z[i][0] > K) {
				K = z[i][0];
			}
		}
		double Kp = -Double.MAX_VALUE;
		for (i = 0; i < z.length; i++) {
			if (z[i][1] > Kp) {
				Kp = z[i][1];
			}
		}
		double zs[][] = new double[z.length][2];
		double zsprime[][] = new double[z.length][2];
		r2strip(zs, zsprime, z, z, L[0]);
		for (i = 0; i < zs.length; i++) {
			// Put them *exactly* on edges
			zs[i][1] = Math.round(zs[i][1]);
		}
		
		// Add in ends of strip
		double diffzs[] = new double[n];
		int nonzerodiff = 0;
		for (i = 0; i < n-1; i++) {
			diffzs[i] = zs[i+1][1] - zs[i][1];
			if (diffzs[i] != 0) {
				nonzerodiff++;
			}
		}
		diffzs[n-1] = zs[0][1] - zs[n-1][1];
		if (diffzs[n-1] != 0) {
			nonzerodiff++;
		}
		int ends[] = new int[nonzerodiff];
		for (i = 0, j = 0; i < diffzs.length; i++) {
			if (diffzs[i] != 0) {
			    ends[j++] = i;	
			}
		}
		double zs2[][] = new double[n+2][2];
		double ws[][] = new double[n+2][2];
		double bs[] = new double[n+2];
		for (i = 0; i <= ends[0]; i++) {
			zs2[i][0] = zs[i][0];
			zs2[i][1] = zs[i][1];
			ws[i][0] = w[i][0];
			ws[i][1] = w[i][1];
			bs[i] = beta[i];
		}
		zs2[ends[0]+1][0] = Double.POSITIVE_INFINITY;
		zs2[ends[0]+1][1] = 0;
		ws[ends[0]+1][0] = Double.NaN;
		ws[ends[0]+1][1] = 0;
		bs[ends[0]+1] = 0;
		for (i = ends[0]+1; i <= ends[1]; i++) {
			zs2[i+1][0] = zs[i][0];
			zs2[i+1][1] = zs[i][1];
			ws[i+1][0] = w[i][0];
			ws[i+1][1] = w[i][1];
			bs[i+1] = beta[i];
		}
		zs2[ends[1]+2][0] = Double.NEGATIVE_INFINITY;
		zs2[ends[1]+2][1] = 0;
		ws[ends[1]+2][0] = Double.NaN;
		ws[ends[1]+2][1] = 0;
		bs[ends[1]+2] = 0;
		for (i = ends[1]+1; i < n; i++) {
			zs2[i+2][0] = zs[i][0];
			zs2[i+2][1] = zs[i][1];
			ws[i+2][0] = w[i][0];
			ws[i+2][1] = w[i][1];
			bs[i+2] = beta[i];
		}
		// Put dummy columns into qdat2 after ends[0] and ends[1] columns
		// qdat2 is nqpts rows by 2*n+2 columns
		// so we are expanding it to 2*n+6 columns with the insertion
		// of 2 more of column n and 2 more of column 2*n+1.
		double qdat3[][] = new double[qdat2.length][qdat2[0].length+4];
		for (i = 0; i < qdat2.length; i++) {
			for (j = 0; j <= ends[0]; j++) {
				qdat3[i][j] = qdat2[i][j];
				qdat3[i][j+2+n+1] = qdat2[i][j+n+1];
			}
			qdat3[i][ends[0]+1] = qdat2[i][n];
			qdat3[i][ends[0]+3+n+1] = qdat2[i][2*n+1];
			for (j = ends[0]+1; j <= ends[1]; j++) {
				qdat3[i][j+1] = qdat2[i][j];
				qdat3[i][j+3+n+1] = qdat2[i][j+n+1];
			}
			qdat3[i][ends[1]+2] = qdat2[i][n];
			qdat3[i][ends[1]+4+n+1] = qdat2[i][2*n+1];
			for (j = ends[1]+1; j <= n-1; j++) {
				qdat3[i][j+2] = qdat2[i][j];
				qdat3[i][j+4+n+1] = qdat2[i][j+n+1];
			}
			qdat3[i][n+2] = qdat2[i][n];
			qdat3[i][2*n+5] = qdat2[i][2*n+1];
		} // for (i = 0; i < qdat2.length; i++)
		
		for (i = 0; i < wp.length; i++) {
			wp[i][0] = 0;
			wp[i][1] = 0;
		}
		int p = zp.length;
		
		// Trap points which map to +/- Infinity on the strip.
		for (i = 0; i < p; i++) {
			if (zabs(zp[i][0],zp[i][1]) < 2.0*eps) {
				zp[i][0] = zp[i][0] + 100.0*eps;
			}
		}
		
		for (i = 0; i < p; i++) {
			if (zabs(zp[i][0],zp[i][1] - Kp) < 2.0*eps) {
				zp[i][1] = zp[i][1] - 100.0*eps*Kp;
			}
		}
		
		// Map from rectangle to strip.
		double yp[][] = new double[zp.length][2];
		double ypprime[][] = new double[zp.length][2];
		r2strip(yp, ypprime, zp, z, L[0]);
		
		// Now map from strip to polygon
		stmap(wp, yp, ws, bs, zs2, c, qdat3);
	}
	
	private void stmap(double wp[][], double zp[][], double w[][], double beta[],
			double z[][], double c[], double qdat[][]) {
		// Schwarz-Christoffel strip map.
		// stmap computes the values of the Schwarz-Christoffel strip map
		// at the points in the vector zp.  The arguments w, beta, z, c, and 
		// qdat are as in stparam.  stmap returns a vector wp the same size
		// as zp.
		// Original matlab routine copyright 1998 by Toby Driscoll.
		int i, j;
		double qdat2[][] = null;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double mid1[][] = null;
		double mid2[][] = null;
		
		if ((zp == null) || (zp.length == 0)) {
			wp = null;
			return;
		}
		
		int n = w.length;
		
		// Quadrature data;
		if (qdat == null) {
			qdat2 = new double[8][2*beta.length+2];
			scqdata(qdat2, beta, 8);		
		}
		else if ((qdat.length == 1) && (qdat[0].length == 1)) {
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 8);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else {
			qdat2 = qdat;
		}
		double tol = Math.pow(10.0, -qdat.length);
		
		//int shape = zp.length;
		double zprow[][] = new double[zp.length][2];
		for (i = 0; i < zp.length; i++) {
			zprow[i][0] = zp[i][0];
			zprow[i][1] = zp[i][1];
		}
		int p = zp.length;
		for (i = 0; i < wp.length; i++) {
			wp[i][0] = 0;
			wp[i][1] = 0;
		}
		
		// For each point in zp, find the nearest prevertex
		double dist[] = new double[p];
		int sing[] = new int[p];
		for (i = 0; i < p; i++) {
			dist[i] = Double.MAX_VALUE;
			sing[i] = -1;
			for (j = 0; j < n; j++) {
				double presentDist = zabs(zprow[i][0] - z[j][0], zprow[i][1] - z[j][1]);
				if (presentDist < dist[i]) {
					dist[i] = presentDist;
					sing[i] = j;
				}
			}
		} // for (i = 0; i < p; i++)
		
		// Screen out images of prevertices
		boolean vertex[] = new boolean[p];
		for (i = 0; i < p; i++) {
			if (dist[i] < tol) {
			    vertex[i] = true;
			    wp[i][0] = w[sing[i]][0];
			    wp[i][1] = w[sing[i]][1];
			}
		}
		
		boolean leftend[] = new boolean[p];
		for (i = 0; i < p; i++) {
			if ((Double.isInfinite(zp[i][0]) || Double.isInfinite(zp[i][1])) && (zp[i][0] < 0.0)) {
				leftend[i] = true;
			}
		}
		
		int windex = -1;
		for (i = 0; i < z.length && (windex == -1); i++) {
			if (Double.isInfinite(z[i][0]) && (z[i][0] < 0) && (z[i][1] == 0)) {
				windex = i;
			}
		}
		for (i = 0; i < p; i++) {
			if (leftend[i]) {
				wp[i][0] = w[windex][0];
				wp[i][1] = w[windex][1];
			}
		}
		
		boolean rightend[] = new boolean[p];
		for (i = 0; i < p; i++) {
			if ((Double.isInfinite(zp[i][0]) || Double.isInfinite(zp[i][1])) && (zp[i][0] > 0.0)) {
				rightend[i] = true;
			}
		}
		
		windex = -1;
		for (i = 0; i < z.length && (windex == -1); i++) {
			if (Double.isInfinite(z[i][0]) && (z[i][0] > 0) && (z[i][1] == 0)) {
				windex = i;
			}
		}
		for (i = 0; i < p; i++) {
			if (rightend[i]) {
				wp[i][0] = w[windex][0];
				wp[i][1] = w[windex][1];
			}
		}
		
		for (i = 0; i < p; i++) {
			vertex[i] = vertex[i] || leftend[i] || rightend[i];
		}
		
		// "Bad" points are closest to a prevertex of infinity.
		boolean atinf[] = new boolean[n];
		for (i = 0; i < n; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				// Infinite vertices
				atinf[i] = true;
			}
		}
		
		
		boolean bad[] = new boolean[p];
		int numbad = 0;
		for (i = 0; i < p; i++) {
			if ((atinf[sing[i]]) && (!vertex[i])) {
				bad[i] = true;
				numbad++;
			}
		}
		
		if (numbad > 0) {
			// we can't begin integrations at a preimage of infinity.  We
			// will pick the next-closest qualified prevertex
			double zf[][] = new double[z.length][2];
			for (i = 0; i < z.length; i++) {
				zf[i][0] = z[i][0];
				zf[i][1] = z[i][1];
			}
			for (i = 0; i < n; i++) {
				if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				    zf[i][0] = Double.POSITIVE_INFINITY;
				    zf[i][1] = 0.0;
				}
			}
			double zprowbad[][] = new double[numbad][2];
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					zprowbad[j][0] = zprow[i][0];
					zprowbad[j++][1] = zprow[i][1];
				}
			}
			double tmp[] = new double[numbad];
			int s[] = new int[numbad];
			for (i = 0; i < numbad; i++) {
				tmp[i] = Double.MAX_VALUE;
				s[i] = -1;
				for (j = 0; j < n; n++) {
					double presentTmp = zabs(zprowbad[i][0] - zf[j][0], zprowbad[i][1] - zf[j][1]);
					if (presentTmp < tmp[i]) {
						tmp[i] = presentTmp;
						s[i] = j;
					}
				}
			} // for (i = 0; i < p; i++)
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					sing[i] = s[j++];
				}
			} // for (i = 0, j = 0; i < p; i++)
			
			// Because we no longer integrate from the closest prevertex,  we
			// must go in stages to maintain accuracy.
			mid1 = new double[numbad][2];
			mid2 = new double[numbad][2];
			for (i = 0; i < numbad; i++) {
				mid1[i][0] = z[s[i]][0];
				mid1[i][1] = 0.5;
			}
			for (i = 0, j = 0; i < p; i++) {
			    if (bad[i]) {
			    	mid2[j][0] = zp[i][0];
			    	mid2[j++][1] = 0.5;
			    }
			} // for (i = 0, j = 0; i < p; i++)
		} // if (numbad > 0)
		else {
			// all clear
			bad = new boolean[p];
		}
		
		// zs = the starting singularities
		double zs[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			zs[i][0] = z[sing[i]][0];
			zs[i][1] = z[sing[i]][1];
		}
		// ws = map(zs)
		double ws[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			ws[i][0] = w[sing[i]][0];
			ws[i][1] = w[sing[i]][1];
		}
		
		// Compute the map directly at "normal" points.
		boolean normal[] = new boolean[p];
		int numnormal = 0;
		for (i = 0; i < p; i++) {
			normal[i] = (!bad[i]) && (!vertex[i]);
			if (normal[i]) {
				numnormal++;
			}
		} // for (i = 0; i < p; i++) 
		
		if (numnormal > 0) {
		    double zsnormal[][] = new double[numnormal][2];
		    double zpnormal[][] = new double[numnormal][2];
		    int singnormal[] = new int[numnormal];
		    for (i = 0, j = 0; i < p; i++) {
		        if (normal[i]) {
		        	zsnormal[j][0] = zs[i][0];
		        	zsnormal[j][1] = zs[i][1];
		        	zpnormal[j][0] = zp[i][0];
		        	zpnormal[j][1] = zp[i][1];
		        	singnormal[j++] = sing[i];
		        }
		    } // for (i = 0, j = 0; i < p; i++)
		    double I[][] = stquad(zsnormal, zpnormal, singnormal, z, beta, qdat2);
		    for (i = 0, j = 0; i < p; i++) {
		    	if (normal[i]) {
		    		zmlt(c[0], c[1], I[j][0], I[j][1], cr, ci);
		    		j++;
		    		wp[i][0] = ws[i][0] + cr[0];
		    		wp[i][1] = ws[i][1] + ci[0];
		    	}
		    } // for (i = 0, j = 0; i < p; i++)
		} // if (numnormal > 0)
		
		// Compute map at "bad" points in stages.
		if (numbad > 0) {
			double zsbad[][] = new double[numbad][2];
			double zpbad[][] = new double[numbad][2];
			int singbad[]= new int[numbad];
			int zerosbad[] = new int[numbad];
			for (i = 0, j = 0; i < p; i++) {
			    if (bad[i]) {
			    	zsbad[j][0] = zs[i][0];
			    	zsbad[j][1] = zs[i][1];
			    	zpbad[j][0] = zp[i][0];
			    	zpbad[j][1] = zp[i][1];
			    	singbad[j++] = sing[i];
			    }
			} // for (i = 0, j = 0; i < p; i++)
			double I1[][] = stquad(zsbad, mid1, singbad, z, beta, qdat2);
			double I2[][] = stquadh(mid1, mid2, zerosbad, z, beta, qdat2);
			double I3[][] = stquad(zpbad, mid2, zerosbad, z, beta, qdat2);
			for (i = 0, j = 0; i < p; i++) {
			    if (bad[i]) {
			    	zmlt(c[0], c[1], (I1[j][0] + I2[j][0] - I3[j][0]), 
			    			(I1[j][1] + I2[j][1] - I3[j][1]), cr, ci);
			    	j++;
			    	wp[i][0] = ws[i][0] + cr[0];
			    	wp[i][1] = ws[i][1] + ci[0];
			    }
			} // for (i = 0, j = 0; i < p; i++)
		} // if (numbad > 0)
	}
	
	public ViewJFrameGraph plotpoly(float xPointArray[], float yPointArray[],
			double w[][], double beta[], boolean addMarkerLabel, double axlim[], int yInvert,
			boolean closed, double axis[]) {
		// plotpoly plots the polygon whose vertices are in vector w
		// and whose turning angles are in beta.  Vertices at infinity
		// permitted, but there must be at least two consecutive finite
		// vertices somewhere in w.
		// If addMarkerLabel is true markers and numeric labels are put at vertices
		// Extracted from original MATLAB routine by Toby Driscoll copyright 1998.
		int n = w.length;
		int numfinite = 0;
		int numinfinite = 0;
		int i, j;
		double theta;
		double Rx[] = new double[2];
		double Ry[] = new double[2];
		double RR[] = new double[4];
		double lim[] = new double[4];
		double denom;
		double costheta;
		double sintheta;
		double minRR;
		boolean atinf[] = new boolean[w.length];
		for (i = 0; i < w.length; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				numfinite++;
			}
			else {
				numinfinite++;
				atinf[i] = true;
			}
		}
		double wf[][] = new double[numfinite][2];
		for (i = 0, j = 0; i < w.length; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				wf[j][0] = w[i][0];
				wf[j++][1] = w[i][1];
			}
		}
		
		double minrealwf = Double.MAX_VALUE;
		double maxrealwf = -Double.MAX_VALUE;
		double minimagwf = Double.MAX_VALUE;
		double maximagwf = -Double.MAX_VALUE;
		for (i = 0; i < wf.length; i++) {
			if (wf[i][0] < minrealwf) {
				minrealwf = wf[i][0];
			}
			if (wf[i][0] > maxrealwf) {
				maxrealwf = wf[i][0];
			}
			if (wf[i][1] < minimagwf) {
				minimagwf = wf[i][1];
			}
			if (wf[i][1] > maximagwf) {
				maximagwf = wf[i][1];
			}
		}
		if (axis != null) {
			lim[0] = axis[0];
			lim[1] = axis[1];
			lim[2] = axis[2];
			lim[3] = axis[3];
		}
		else {
			lim[0] = minrealwf;
			lim[1] = maxrealwf;
			lim[2] = minimagwf;
			lim[3] = maximagwf;
		}
		for (i = 0; i < 4; i++) {
			axlim[i] = lim[i];
		}
		double maxdiff = Math.max(lim[1] - lim[0], lim[3] - lim[2]);
		double fac = 0.6;
		if (numinfinite > 0) {
			fac = 0.7;
		}
		double meanlim01 = (lim[0] + lim[1])/2.0;
		double meanlim23 = (lim[2] + lim[3])/2.0;
		lim[0] = meanlim01 - fac * maxdiff;
		lim[1] = meanlim01 + fac * maxdiff;
		lim[2] = meanlim23 - fac * maxdiff;
		lim[3] = meanlim23 + fac * maxdiff;
		double R = Math.max(lim[1] - lim[0], lim[3] - lim[2]);
		int first = -1;
		for (i = 0; i < n-1 && (first == -1); i++) {
			if ((!atinf[i]) && (!atinf[i+1])) {
			    first = i;	
			}
		}
		if ((first == -1) && (!atinf[n-1]) && (!atinf[0])) {
			first = n-1;
		}
		if (first == -1) {
			MipavUtil.displayError("There must be two consecutive finite vertices.");
			return null;
		}
		int renum[] = new int[n];
		for (i = 0; i < n - first; i++) {
			renum[i] = first +i;
		}
		for (i = 0; i < first; i++) {
			renum[n-first+i] = i;
		}
		double wrenum[][] = new double[n][2];
		double betarenum[] = new double[n];
		boolean atinfrenum[] = new boolean[n];
		for (i = 0; i < n; i++) {
			wrenum[i][0] = w[renum[i]][0];
			wrenum[i][1] = w[renum[i]][1];
			betarenum[i] = beta[renum[i]];
			atinfrenum[i] = atinf[renum[i]];
		}
		
		// First edge
		//double edgeh[] = new double[n];
		int numberOfStrings;
		if (closed) {
			numberOfStrings = xPointArray.length-1;
		}
		else {
			numberOfStrings = xPointArray.length;
		}
		double lblh[][] = new double[numberOfStrings][2];
		for (i = 0; i < numberOfStrings; i++) {
			for (j = 0; j < 2; j++) {
				lblh[i][j] = Double.NaN;
			}
		}
		String lblhString[][] = new String[numberOfStrings][2];
		double lblhx[][] = new double[numberOfStrings][2];
		double lblhy[][] = new double[numberOfStrings][2];
		xPointArray[0] = (float)wrenum[0][0];
		yPointArray[0] = (float)wrenum[0][1];
		xPointArray[1] = (float)wrenum[1][0];
		yPointArray[1] = (float)wrenum[1][1];
		if (closed) {
		    xPointArray[xPointArray.length-1] = xPointArray[0];
		    yPointArray[xPointArray.length-1] = yPointArray[0];
		}
		
		double ang = Math.atan2(wrenum[1][1] - wrenum[0][1], wrenum[1][0] - wrenum[0][0]);
		
		// Remaining edges
		j = 2;
		int jext = 2;
		while (j <= n) {
		    int jp1 = (j%n)+1;
		    
		    // Draw marker/label
		    if (addMarkerLabel) {
		    	theta = ang;
		    	// May need to modify position of label
		    	if ((Math.abs(betarenum[j-2]-1) < 3.0*eps) ||
		    			(Math.abs(betarenum[jp1-1] - 1) < 3.0*eps)) {
		    		// Next to a slit, perturb label inside
		    		theta = theta + Math.PI - (betarenum[j-1]+ 1)*Math.PI/2.0;
		    	}
		    	else if (Math.abs(betarenum[j-1]) < 3.0*eps) {
		            // For a "trivial" vertex, put number outside
		    		theta = theta - Math.PI/2.0;
		    	}
		    	// Mark label; markers will be added last
		    	lblhx[jext-1][1] = wrenum[j-1][0] + 0.035*R*Math.cos(theta);
		    	lblhy[jext-1][1] = wrenum[j-1][1] + 0.035*R*Math.sin(theta);
		    	lblhString[jext-1][1] =String.valueOf(renum[j-1]);
		    } // if (addMarkerLabel)
		    
		    // Next edge
		    if (!atinfrenum[jp1-1]) {
		    	// Bounded edge; straightforward
		    	xPointArray[jext] = (float)wrenum[jp1-1][0];
		    	yPointArray[jext] = (float)wrenum[jp1-1][1];
		    	ang = ang - Math.PI*betarenum[j-1];
		    	j = j+1;
		    	jext = jext+1;
		    } // if (!atinfrenum[jp1-1])
		    else {
		        // Unbounded edge (first of two consecutive)
		    	ang = ang - Math.PI*betarenum[j-1];
		    	xPointArray[jext] = (float)(wrenum[j-1][0] + R*Math.cos(ang));
		    	yPointArray[jext] = (float)(wrenum[j-1][1] + R*Math.sin(ang));
		    	
		    	
		    	// Make first label outside axes box
		    	if (addMarkerLabel) {
		    		theta = ang;
		    		costheta = Math.cos(theta);
		    		if (costheta == 0) {
		    			denom = eps;
		    		}
		    		else {
		    			denom = costheta;
		    		}
		    		Rx[0] = (lim[0] - wrenum[j-1][0])/denom;
		    		Rx[1] = (lim[1] - wrenum[j-1][0])/denom;
		    		sintheta = Math.sin(theta);
		    		if (sintheta == 0) {
		    			denom = eps;
		    		}
		    		else {
		    			denom = sintheta;
		    		}
		    		Ry[0] = (lim[2] - wrenum[j-1][1])/denom;
		    		Ry[1] = (lim[3] - wrenum[j-1][1])/denom;
		    		RR[0] = Rx[0];
		    		RR[1] = Rx[1];
		    		RR[2] = Ry[0];
		    		RR[3] = Ry[1];
		    		minRR = Double.MAX_VALUE;
		    		for (i = 0; i <= 3; i++) {
		    		    if ((RR[i] > 0)	&& (RR[i] < minRR)) {
		    		    	minRR = RR[i];
		    		    }   		
		    		}
		    		if (minRR == Double.MAX_VALUE) {
		    			minRR = 0.0;
		    		}
		    		lblhx[jext][0] = wrenum[j-1][0] + (minRR + 0.07*R)*costheta;
		    		lblhy[jext][0] = wrenum[j-1][1] + (minRR + 0.07*R)*sintheta;
		    		lblhString[jext][0] = renum[j] + " (inf)";
		    	} // if (addMarkerLabel)
		    	
		    	// Second unbounded edge
		    	ang = ang - Math.PI*betarenum[jp1-1];
		    	xPointArray[jext+1] = (float)(wrenum[(j+1)%n][0] - R*Math.cos(ang));
		    	yPointArray[jext+1] = (float)(wrenum[(j+1)%n][1] - R*Math.sin(ang));
		    	xPointArray[jext+2] = (float)(wrenum[(j+1)%n][0]);
		    	yPointArray[jext+2] = (float)(wrenum[(j+1)%n][1]);
		    	if (addMarkerLabel) {
		    	    theta = ang + Math.PI;
		    	    costheta = Math.cos(theta);
		    		if (costheta == 0) {
		    			denom = eps;
		    		}
		    		else {
		    			denom = costheta;
		    		}
		    		Rx[0] = (lim[0] - wrenum[(j+1)%n][0])/denom;
		    		Rx[1] = (lim[1] - wrenum[(j+1)%n][0])/denom;
		    		sintheta = Math.sin(theta);
		    		if (sintheta == 0) {
		    			denom = eps;
		    		}
		    		else {
		    			denom = sintheta;
		    		}
		    		Ry[0] = (lim[2] - wrenum[(j+1)%n][1])/denom;
		    		Ry[1] = (lim[3] - wrenum[(j+1)%n][1])/denom;
		    		RR[0] = Rx[0];
		    		RR[1] = Rx[1];
		    		RR[2] = Ry[0];
		    		RR[3] = Ry[1];
		    		minRR = Double.MAX_VALUE;
		    		for (i = 0; i <= 3; i++) {
		    		    if ((RR[i] > 0)	&& (RR[i] < minRR)) {
		    		    	minRR = RR[i];
		    		    }   		
		    		}
		    		if (minRR == Double.MAX_VALUE) {
		    			minRR = 0.0;
		    		}
		    		lblhx[jext+1][1] = wrenum[(j+1)%n][0] + (minRR + 0.07*R)*costheta;
		    		lblhy[jext+1][1] = wrenum[(j+1)%n][1] + (minRR + 0.07*R)*sintheta;
		    		lblhString[jext+1][1] = renum[j] + " (inf)";

		    	} // if (addMarkerLabel)
		    	
		    	// We've done two
		    	j = j+2;
		    	jext = jext+3;
		    } // else
		} // while (j <= n)
		
		// Last item, label for first point
		if (addMarkerLabel) {
			theta = ang;
			if ((Math.abs(betarenum[n-1]-1) < 3.0*eps) || 
					(Math.abs(betarenum[0]) < 3.0*eps) || (Math.abs(betarenum[1]-1) < 3.0*eps)) {
				theta = theta + Math.PI - (betarenum[0] + 1)*Math.PI/2.0;
			}
			lblhx[0][1] = (int)Math.round(wrenum[0][0] + 0.035*R*Math.cos(theta));
			lblhy[0][1] = (int)Math.round(wrenum[0][1] + 0.035*R*Math.sin(theta));
			lblhString[0][1] = String.valueOf(renum[0]);
		} // if (addMarkerLabel)
		
		// Plot markers last, to keep them on top
		// Not needed with SHOW_POINTS_AND_LINES
		//if (addMarkerLabel) {
		   // for (j = 0; j < n; j++) {
		    	//if (!atinfrenum[j]) {
		    		//lblhx[j][0] = wrenum[j][0];
		    		//lblhy[j][0] = wrenum[j][1];
		    		//lblhString[j][0] = ".";
		    	//}
		   // }
		//} // if (addMarkerLabel)
		
		if (numinfinite > 0) {
			minrealwf = Double.MAX_VALUE;
			maxrealwf = -Double.MAX_VALUE;
			minimagwf = Double.MAX_VALUE;
			maximagwf = -Double.MAX_VALUE;
			for (i = 0; i < xPointArray.length; i++) {
				if (xPointArray[i] < minrealwf) {
					minrealwf = xPointArray[i];
				}
				if (xPointArray[i] > maxrealwf) {
					maxrealwf = xPointArray[i];
				}
				if (yPointArray[i] < minimagwf) {
					minimagwf = yPointArray[i];
				}
				if (yPointArray[i] > maximagwf) {
					maximagwf = yPointArray[i];
				}
			}
			axlim[0] = minrealwf;
			axlim[1] = maxrealwf;
			axlim[2] = minimagwf;
			axlim[3] = maximagwf;	
		} // if (numinfinite > 0)
		if (axis != null) {
			axlim[0] = axis[0];
			axlim[1] = axis[1];
			axlim[2] = axis[2];
			axlim[3] = axis[3];
		}
		else if (exterRoutine) {
			axlim[0] = 0;
			axlim[1] = destImage.getExtents()[0] - 1;
			axlim[2] = 0;
			axlim[3] = destImage.getExtents()[1] - 1;
		}
		
		ViewJFrameGraph pointGraph = new ViewJFrameGraph(xPointArray, yPointArray,
				"title", "labelX", "labelY", Color.BLUE);
		pointGraph.setVisible(true);
		ViewJComponentGraph graph = pointGraph.getGraph();
		if ((axis != null) || (exterRoutine)) {
		    graph.setDomain((float)axlim[0], (float)axlim[1]);	
		    graph.setRange((float)axlim[2], (float)axlim[3]);
		}
		if (yInvert != Integer.MIN_VALUE) {
			graph.setYInvert(yInvert);
		}
		if (addMarkerLabel) {
		    graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_POINTS_AND_LINES);
		}
		else {
			 graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);	
		}
		Graphics g = pointGraph.getFrameGraphics();
		graph.paintComponent(g);
		Rectangle graphBounds = graph.getGraphBounds();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
        for (i = 0; i < xPointArray.length-1; i++) {
        	for (j = 0; j < 2; j++) {
        		if (lblhString[i][j] != null) {
        			int posX =  (int)Math.round(graphBounds.x + xScale*(lblhx[i][j] - axlim[0]));
        			int posY =  (int)Math.round(graphBounds.y + yScale*(lblhy[i][j] - axlim[2]));
        			posY = -posY + 2*graphBounds.y + graphBounds.height;
        			graph.drawString(g, lblhString[i][j], posX, posY);
        		}
        	}
        }
        
		return pointGraph;
	}
	
	private double rectAccuracy(scmap M) {
		// Apparent accuracy of Schwarz-Christoffel rectangle map.
		// accuracy estimates the accuracy of the Schwarz-Christoffel rectangle map M.
		// The technique used is to compare the differences between successive finite
		// vertices to the integral between the corresponding prevertices, and return
		// the maximum.
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j;
		double acc;
		double cr[] = new double[1];
		double ci[] = new double[1];
		// If an accuracy has been assigned, don't question it.
		if (!Double.isNaN(M.accuracy)) {
			return M.accuracy;
		}
		// Get data for low-level functions
		polygon p = M.poly;
		double w[][] = p.vertex;
		int n = w.length;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double z[][] = M.prevertex;
		double c[] = new double[2];
		c[0] = M.constant[0];
		c[1] = M.constant[1];
		double L[] = M.stripL;
		double qdata[][] = M.qdata;
		
		// Remember to put first corner first
		int corner[] = new int[4];
		int renum[] = new int[n];
		rcorners(w, beta, z, corner, renum);
		
		// Map prevertices to strip
		double K = -Double.MAX_VALUE;
		for (i = 0; i < n; i++) {
			if (z[i][0] > K) {
				K = z[i][0];
			}
		}
		double Kp = -Double.MAX_VALUE;
		for (i = 0; i < n; i++) {
			if (z[i][1] > Kp) {
				Kp = z[i][1];
			}
		}
		double zs[][] = new double[z.length][2];
		double zsprime[][] = new double[z.length][2];
		r2strip(zs, zsprime, z, z, L[0]);
		for (i = 0; i < zs.length; i++) {
			// Put them *exactly* on edges
			zs[i][1] = Math.round(zs[i][1]);
		}
		
		// Integrate between consecutive finite pairs on bottom and top
		int numidxbot = 0;
		for (i = 0; i <=corner[2]-1; i++) {
			if (isFinite(w[i][0]) && isFinite(w[i][1])) {
				numidxbot++;
			}
		}
		int idxbot[] = new int[numidxbot];
		for (i = 0, j = 0; i <= corner[2] -1; i++) {
			if (isFinite(w[i][0]) && isFinite(w[i][1])) {
				idxbot[j++] = i;
			}
		}
		int numidxtop = 0;
		for (i = corner[2]; i <= n-1; i++) {
			if (isFinite(w[i][0]) && isFinite(w[i][1])) {
				numidxtop++;
			}	
		}
		int idxtop[] = new int[numidxtop];
		for (i = corner[2], j = 0; i <= n-1; i++) {
			if (isFinite(w[i][0]) && isFinite(w[i][1])) {
				idxtop[j++] = i;
			}	
		}
		// Two columns hold endpoint indices for integrations
		int idx[][] = new int[numidxbot+numidxtop-2][2];
		for (i = 0; i < numidxbot-1; i++) {
			idx[i][0] = idxbot[i];
			idx[i][1] = idxbot[i+1];
		}
		
		for (i = 0; i < numidxtop-1; i++) {
			idx[numidxbot-1+i][0] = idxtop[i];
			idx[numidxbot-1+i][1] = idxtop[i+1];
		}
		
		// Find midpoints.  Go into the interior of strip to avoid integrating
		// through skipped prevertices.
		double zz[][][] = new double[2][numidxbot+numidxtop-2][2];
		for (i = 0; i < numidxbot+numidxtop-2; i++) {
			for (j = 0; j < 2; j++) {
				zz[j][i][0] = zs[idx[i][j]][0];
				zz[j][i][1] = zs[idx[i][j]][1];
			}
		}
		double mid[][] = new double[numidxbot+numidxtop-2][2];
		for (j = 0; j < numidxbot+numidxtop-2; j++) {
			for (i = 0; i < 2; i++) {
				mid[j][0] += zz[i][j][0];
				mid[j][1] += zz[i][j][1];
			}
		}
		for (i = 0; i < numidxbot+numidxtop-2; i++) {
			mid[i][0] = mid[i][0]/2.0;
			mid[i][1] = 0.5;
		}
		
		// As a final check, integrate once across the strip
		double tmp = Double.MAX_VALUE;
		int k = -1;
		for (i = 0; i < numidxtop; i++) {
			if (Math.abs(zs[idxtop[i]][0] - zs[0][0]) < tmp) {
			    tmp = Math.abs(zs[idxtop[i]][0] - zs[0][0]);
			    k = i;
			}
		}
		int idx2[][] = new int[numidxbot+numidxtop-1][2];
		for (i = 0; i < numidxbot+numidxtop-2; i++) {
			for (j = 0; j < 2; j++) {
			    idx2[i][j] = idx[i][j];	
			}
		}
		idx2[numidxbot+numidxtop-2][0] = 0;
		idx2[numidxbot+numidxtop-2][1] = idxtop[k];
		double mid2[][] = new double[numidxbot+numidxtop-1][2];
		for (i = 0; i < numidxbot+numidxtop-2; i++) {
			mid2[i][0] = mid[i][0];
			mid2[i][1] = mid[i][1];
		}
		mid2[numidxbot+numidxtop-2][0] = (zs[0][0] + zs[idxtop[k]][0])/2.0;
		mid2[numidxbot+numidxtop-2][1] = (zs[0][1] + zs[idxtop[k]][1])/2.0;
		
		// Add in ends of strip
		int ends[] = new int[2];
		for (i = 0, j = 0; i < n-1 && j < 2; i++) {
			if (zs[i+1][1] != zs[i][1]) {
				ends[j++] = i;
			}
		}
		if (j < 2) {
			if (zs[n-1][1] != zs[0][1]) {
				ends[j++] = n-1;
			}
		}
		double zq[][] = new double[n+2][2];
		double bq[] = new double[n+2];
		double wq[][] = new double[n+2][2];
		for (i = 0; i <= ends[0]; i++) {
		    zq[i][0] = zs[i][0];
		    zq[i][1] = zs[i][1];
		    bq[i] = beta[i];
		    wq[i][0] = w[i][0];
		    wq[i][1] = w[i][1];
		}
		zq[ends[0]+1][0] = Double.POSITIVE_INFINITY;
		zq[ends[0]+1][1] = 0.0;
		bq[ends[0]+1] = 0.0;
		wq[ends[0]+1][0] = Double.NaN;
		wq[ends[0]+1][1] = 0.0;
		for (i = ends[0]+1; i <= ends[1]; i++) {
			zq[i+1][0] = zs[i][0];
			zq[i+1][1] = zs[i][1];
		    bq[i+1] = beta[i];
		    wq[i+1][0] = w[i][0];
		    wq[i+1][1] = w[i][1];
		}
		zq[ends[1]+2][0] = Double.NEGATIVE_INFINITY;
		zq[ends[1]+2][1] = 0.0;
		bq[ends[1]+2] = 0.0;
		wq[ends[1]+2][0] = Double.NaN;
		wq[ends[1]+2][1] = 0.0;
		for (i = ends[1]+1; i < n; i++) {
			zq[i+2][0] = zs[i][0];
			zq[i+2][1] = zs[i][1];
		    bq[i+2] = beta[i];
		    wq[i+2][0] = w[i][0];
		    wq[i+2][1] = w[i][1];	
		}
		// Extend qdat with useless columns at ends
		double qdata2[][] = new double[qdata.length][2*n+6];
		for (i = 0; i < qdata.length; i++) {
			for (j = 0; j <= ends[0]; j++) {
				qdata2[i][j] = qdata[i][j];
			}
			qdata2[i][ends[0]+1] = qdata[i][n];
			for (j = ends[0]+1; j <= ends[1]; j++) {
				qdata2[i][j+1] = qdata[i][j];
			}
			qdata2[i][ends[1]+2] = qdata[i][n];
		    for (j = ends[1]+1; j < n; j++) {
			   qdata2[i][j+2] = qdata[i][j];
		    }
		   qdata2[i][n+2] = qdata[i][n];
		   for (j = n+1; j <= ends[0]+n+1; j++) {
				qdata2[i][j+2] = qdata[i][j];
			}
		   qdata2[i][ends[0]+n+4] = qdata[i][2*n+1];
		   for (j = ends[0]+n+2; j <= ends[1]+n+1; j++) {
			   qdata2[i][j+3] = qdata[i][j];   
		   }
		   qdata2[i][ends[1]+n+5] = qdata[i][2*n+1];
		   for (j = ends[1]+n+2; j < 2*n+1; j++) {
			   qdata2[i][j+4] = qdata[i][j];   
		   }
		   qdata2[i][2*n+5] = qdata[i][2*n+1];
		}
		
		// Do the integrations
		double zleft[][] = new double[idx2.length][2];
		double zright[][] = new double[idx2.length][2];
		for (i = 0; i < idx2.length; i++) {
			zleft[i][0] = zs[idx2[i][0]][0];
			zleft[i][1] = zs[idx2[i][0]][1];
			zright[i][0] = zs[idx2[i][1]][0];
			zright[i][1] = zs[idx2[i][1]][1];
		}
		int idx3[][] = new int[idx2.length][2];
		for (i = 0; i < idx2.length; i++) {
			for (j = 0; j < 2; j++) {
				if (idx2[i][j] > ends[1]) {
					idx3[i][j] = idx2[i][j]+2;
				}
				else if (idx2[i][j] > ends[0]) {
					idx3[i][j] = idx2[i][j] +1;
				}
				else {
					idx3[i][j] = idx2[i][j];
				}
			}
		}
		int idxc0[] = new int[idx3.length];
		int idxc1[] = new int[idx3.length];
		for (i = 0; i < idx3.length; i++) {
			idxc0[i] = idx3[i][0];
			idxc1[i] = idx3[i][1];
		}
		
		double I1[][] = stquad(zleft, mid2, idxc0, zq, bq, qdata2);
		double I2[][] = stquad(zright, mid2, idxc1, zq, bq, qdata2);
		double I[][] = new double[I1.length][I1[0].length];
		for (i = 0; i < I1.length; i++) {
			for (j = 0; j < I1[0].length; j++) {
				I[i][j] = I1[i][j] - I2[i][j];
			}
		}
		//double diffw[][][] = new double[idx3[0].length-1][idx3.length][2];
		// idx3[0].length = 2
		double diffw[][] = new double[idx3.length][2];
		for (i = 0; i < idx3.length; i++) {
			diffw[i][0] = wq[idx3[i][1]][0] - wq[idx3[i][0]][0];
			diffw[i][1] = wq[idx3[i][1]][1] - wq[idx3[i][0]][1];
		}
		double absdiff[] = new double[idx3.length];
		for (i = 0; i < idx3.length; i++) {
			zmlt(c[0], c[1], I[i][0], I[i][1], cr, ci);
			absdiff[i] = zabs(cr[0] - diffw[i][0], ci[0] - diffw[i][1]);
		}
		acc = -Double.MAX_VALUE;
		for (i = 0; i < idx3.length; i++) {
			if (absdiff[i] > acc) {
				acc = absdiff[i];
			}
		}
		System.out.println("rectmap accuracy = " + acc);
		return acc;
	}
	
	public class scmap {
	    double prevertex[][];
	    double constant[] = new double[]{Double.NaN, Double.NaN};
	    double stripL[];
	    double qdata[][];
	    double rectqdata[][];
	    polygon poly;
	    polygon rectpolygon;
	    double center[] = new double[]{Double.NaN, Double.NaN};
	    int center_fix_quadnum;
	    double center_fix_mt[][] = new double[4][2];
	    double accuracy = Double.NaN;
	    String className = "rectmap";
	    qlgraph qgraph;
	    double crossratio[];
	    double affine[][][];
	    double rectaffine[][][];
	    boolean original[];
	    scmap() {
	    	
	    }
	}
	
	public class qlgraph {
		int edge[][];
		int qledge[][];
		int qlvert[][];
		boolean adjacent[][];
		qlgraph() {
			
		}
	}
	
	private void rcorners(double w[][], double beta[], double z[][], int corners[], int renum[]) {
		// w, beta, and z are input/output
		// w, beta, and z have the same length n
		// corners and renum are output
		// int corners[] = new int[4]
		// int renum[] = new int[w.length] = new int[n];
		// Find corners of rectangle whose map is represented by prevertices z
		// on the strip, then renumber w, beta, and z (and the corners) so that
		// corners[0] = 0.
		// Original MATLAB routine copyright 1998 by Toby Driscoll
		int left;
		int right;
		int top;
		int bot;
		int i, j;
		int offset = -1;
		int n = w.length;
		
		// Deduce corner locations
		double minrealz = Double.MAX_VALUE;
		double maxrealz = -Double.MAX_VALUE;
		double minimagz = Double.MAX_VALUE;
		double maximagz = -Double.MAX_VALUE;
		for (i = 0; i < n; i++) {
		    if (z[i][0] < minrealz) {
		    	minrealz = z[i][0];
		    }
		    if (z[i][0] > maxrealz) {
		    	maxrealz = z[i][0];
		    }
		    if (z[i][1] < minimagz) {
		    	minimagz = z[i][1];
		    }
		    if (z[i][1] > maximagz) {
		    	maximagz = z[i][1];
		    }
		} // for (i = 0; i < n; i++)
		for (i = 0, j = 0; i < n; i++) {
		    if (Math.abs(z[i][0] - minrealz) < eps) {
		    	left = 1;
		    }
		    else {
		    	left = 0;
		    }
		    if (Math.abs(z[i][0] - maxrealz) < eps) {
		    	right = 1;
		    }
		    else {
		    	right = 0;
		    }
		    if (Math.abs(z[i][1] - maximagz) < eps) {
		    	top = 1;
		    }
		    else {
		    	top = 0;
		    }
		    if (Math.abs(z[i][1] - minimagz) < eps) {
		    	bot = 1;
		    }
		    else {
		    	bot = 0;
		    }
		    if (left + right + top + bot == 2) {
		    	corners[j] = i;
		    	if (zabs(z[i][0] - maxrealz, z[i][1]) < eps) {
		    		offset = j;
		    	}
		    	j++;
		    }
		} // for (i = 0; i < n; i++)
		if (offset == -1) {
			MipavUtil.displayError("In rcorners none of 4 corners have abs(z-max(real(z))) < eps");
			System.exit(-1);
		}
		int tempcorners[] = new int[4];
		for (i = offset; i <= 3; i++) {
		    tempcorners[i-offset] = corners[i];	
		}
		for (i = 0; i <= offset-1; i++) {
			tempcorners[4-offset+i] = corners[i];
		}
		for (i = 0; i <= 3; i++) {
			corners[i] = tempcorners[i];
		}
		
		// Renumber vertices so that corners[0] = 0
		double wtemp[][] = new double[n][2];
		double betatemp[] = new double[n];
		double ztemp[][] = new double[n][2];
		for (i = corners[0]; i <= n-1; i++) {
			renum[i-corners[0]] = i;
			wtemp[i-corners[0]][0] = w[i][0];
			wtemp[i-corners[0]][1] = w[i][1];
			betatemp[i-corners[0]] = beta[i];
			ztemp[i-corners[0]][0] = z[i][0];
			ztemp[i-corners[0]][1] = z[i][1];
		}
		for (i = 0; i <= corners[0]-1; i++) {
			renum[n-corners[0]+i] = i;
			wtemp[n-corners[0]+i][0] = w[i][0];
			wtemp[n-corners[0]+i][1] = w[i][1];
			betatemp[n-corners[0]+i] = beta[i];
			ztemp[n-corners[0]+i][0] = z[i][0];
			ztemp[n-corners[0]+i][1] = z[i][1];
		}
		for (i = 0; i < n; i++) {
			w[i][0] = wtemp[i][0];
			w[i][1] = wtemp[i][1];
			beta[i] = betatemp[i];
			z[i][0] = ztemp[i][0];
			z[i][1] = ztemp[i][1];
		}
		for (i = 0; i < n; i++) {
			wtemp[i] = null;
			ztemp[i] = null;
		}
		wtemp = null;
		betatemp = null;
		ztemp = null;
		for (i = 0; i <= 3; i++) {
			corners[i] = ((tempcorners[i]-tempcorners[0] + n)%n);
		}
		tempcorners = null;
	}
	
	// Construct polygon object
	// x and y are 2 double[] vectors used to specify the vertices.
	// alpha if supplied manually specifies the interior angles at the vedrtices, divided by PI
	// polygon accepts unbounded polygons (vertices at infinity).  However, you must
	// supply alpha, and the vertices must be in counterclockwise order about the interior.
	// Ported from original MATLAB routine copyright 1998 by Toby Driscoll.
	public class polygon {
		double vertex[][];
		double angle[];
		//String className = "polygon";
		
		
		polygon(double x[], double y[], double alpha[]) {
			int n;
			// Vertices passed as two real vectors
			// If first point is repeated at the end, delete the second copy
			double xdiff = x[x.length-1] - x[0];
			double ydiff = y[x.length-1] - y[0];
			if (zabs(xdiff, ydiff) < 3.0*eps) {
			    n = x.length-1;	
			} // if (zabs(xdiff, ydiff) < 3.0*eps)
			else {
				n = x.length;
			}
			vertex = new double[n][2];
			if ((alpha != null) && (alpha.length == x.length)) {
				angle = new double[n];
			}
			for (int i = 0; i < n; i++) {
				vertex[i][0] = x[i];
				vertex[i][1] = y[i];
				if ((alpha != null) && (alpha.length == x.length)) {
					angle[i] = alpha[i];
				}
			}
			
			// Now compute angles if needed
			if (n > 0) {
			    double alpha2[] = new double[n];
			    boolean isccw[] = new boolean[1];
			    int index[] = new int[1];
			    angle(alpha2, isccw, index, vertex, angle);
			    if (!isccw[0]) {
			    	double vertextemp[][] = new double[n][2];
			    	for (int i = 0; i < n; i++) {
			    	    vertextemp[i][0] = vertex[n-1-i][0];
			    	    vertextemp[i][1] = vertex[n-1-i][1];
			    	}
			    	for (int i = 0; i < n; i++) {
			    		vertex[i][0] = vertextemp[i][0];
			    		vertex[i][1] = vertextemp[i][1];
			    	}
			    	for (int i = 0; i < n; i++) {
			    		vertextemp[i] = null;
			    	}
			    	vertextemp = null;
			    	double alphatemp[] = new double[n];
			    	for (int i = 0; i < n; i++) {
			    		alphatemp[i] = alpha2[n-1-i];
			    	}
			    	for (int i = 0; i < n; i++) {
			    		alpha2[i] = alphatemp[i];
			    	}
			    	alphatemp = null;
			    } // if (!isccw)
			    if (angle == null) {
			    	angle = new double[n];
			    }
			    for (int i = 0; i < n; i++) {
			    	angle[i] = alpha2[i];
			    }
			    if (Math.abs(index[0]) > 1) {
			    	MipavUtil.displayWarning("Polygon is multiple-sheeted");
			    }
			} // if (n > 0)
		} // polygon(double x[], double y[])
	} // private class polygon
	
	// Normalized interior angles of a polygon.
	// alpha contains the returned interior angles, normalized by PI, of the polygon
	// 0 < alpha[j] <= 2 if vertex j is finite, and -2 <= alpha[j] <= 0 if j is an infinite vertex.
	// (This is consistent with the definition as the angle swept from the exiting side through
	// the interior to the incoming side, with the vertices in counterclockwise order.)  It is
	// impossible to compute angles for an unbounded polygon; they must be supplied to the
	// polygon constructor to be well-defined.
	// Original MATLAB routine copyright 1998 by Toby Driscoll
	public void angle(double alpha[], boolean[] isccw, int index[], double w[][], double angle[]) {
		int i, j, k;
		double cr[] = new double[1];
		double ci[] = new double[1];
	    int n = w.length;
	    boolean mask[] = new boolean[n];
	    
	    if ((angle != null) && (angle.length > 0)) {
	    	// If the angles have been assigned, return them
	    	for (i = 0; i < angle.length; i++) {
	    		alpha[i] = angle[i];
	    	}
	    } // if ((angle != null) && (angle.length > 0))
	    else {
	        if (n == 0) {
	        	alpha = null;
	        	isccw = null;
	        	index = null;
	        	return;
	        }
	        
	        for (i = 0; i < n; i++) {
	        	if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
	        		System.err.println("Cannot compute angles for unbounded polygons");
	        		return;
	        	}
	        } // for (i = 0; i < w.length; i++)
	        
	        // Compute angles
	        double incoming[][] = new double[n][2];
	        incoming[0][0] = w[0][0] - w[n-1][0];
	        incoming[0][1] = w[0][1] - w[n-1][1];
	        for (i = 1; i < n; i++) {
	        	incoming[i][0] = w[i][0] - w[i-1][0];
	        	incoming[i][1] = w[i][1] - w[i-1][1];
	        }
	        double outgoing[][] = new double[n][2];
	        for (i = 0; i < n-1; i++) {
	        	outgoing[i][0] = incoming[i+1][0];
	        	outgoing[i][1] = incoming[i+1][1];
	        }
	        outgoing[n-1][0] = incoming[0][0];
	        outgoing[n-1][1] = incoming[0][1];
	        for (i = 0; i < n; i++) {
	            zmlt(-incoming[i][0], -incoming[i][1], outgoing[i][0], -outgoing[i][1], cr, ci);
	            double theta = Math.atan2(ci[0], cr[0]);
	            double tdiv = theta/Math.PI;
	            alpha[i] = tdiv - 2 * Math.floor(tdiv/2);
	        } // for (i = 0; i < n; i++)
	        
	        // It's ill-posed to determine locally the slits (inward-pointing) from 
	        // points (outward-pointing).  Check suspicious cases at the tips to see if
	        // they are interior to the rest of the polygon
	        int nummask = 0;
	        for (i = 0; i < n; i++) {
	        	if ((alpha[i] < 100.0  *eps) || ((2 - alpha[i]) < 100.0 * eps)) {
	        		mask[i] = true;
	        		nummask++;
	        	}
	        } // for (i = 0; i < n; i++)
	        if (nummask == n) {
	        	// This can happen if all vertices are collinear.
	        	for (i = 0; i < alpha.length; i++) {
	        		alpha[i] = 0;
	        	}
	        	isccw[0] = true; // irrelevant
	        	index[0] = 1; // irrelevant
	        	return;
	        } // if (nummask == n)
	        int numnotmask = n - nummask;
	        double wmask[][] = new double[nummask][2];
	        double wnotmask[][] = new double[numnotmask][2];
	        int fmask[] = new int[nummask];
	        for (i = 0, j = 0, k = 0; i < n; i++) {
	            if (mask[i]) {
	            	wmask[j][0] = w[i][0];
	            	wmask[j][1] = w[i][1];
	            	fmask[j++] = i;
	            }
	            else {
	            	wnotmask[k][0] = w[i][0];
	            	wnotmask[k++][1] = w[i][1];
	            }
	        } // for (i = 0, j = 0, k = 0; i < n; i++)
	        boolean slit[] = new boolean[wmask.length];
	        boolean onvtx[][] = new boolean[wnotmask.length][wmask.length];
	        isinpoly(slit, onvtx, wmask, wnotmask, null, eps);
	        for (i = 0; i < nummask; i++) {
	            if (slit[i]) {
	            	alpha[fmask[i]] = 2.0;
	            }
	            else {
	            	alpha[fmask[i]] = 0.0;
	            }
	        } // for (i = 0; i < nummask; i++)
	    } // else
	    
	    // Now test--if incorrect, assume the orientation is clockwise
	    double sindex = 0.0;
	    for (i = 0; i < alpha.length; i++) {
	        sindex += (alpha[i] - 1.0);	
	    }
	    sindex = sindex/2.0; // sindex should be integer
	    if (Math.abs(sindex - Math.round(sindex)) > 100.0*Math.sqrt(n)*eps) {
	    	// Try reversing the interpretation of a crack
	    	sindex = 0.0;
	    	for (i = 0; i < alpha.length; i++) {
	    		if ((alpha[i] < 2.0*eps) || ((2.0 - alpha[i]) < 2.0*eps)) {
	    			mask[i] = true;
	    		}
	    		else {
	    			mask[i] = false;
	    			alpha[i] = 2.0 - alpha[i];
	    		}
	    		sindex += (alpha[i] - 1.0);
	    	} // for (i = 0; i < alpha.length; i++)
	    	sindex = sindex/2.0; // should be integer
	    	// If still not OK, something is wrong
	    	if (Math.abs(sindex - Math.round(sindex)) > 100.0*Math.sqrt(n)*eps) {
	    		System.err.println("Invalid polygon");
	    		return;
	    	}
	    } // if (Math.abs(sindex - Math.round(sindex)) > 100.0*Math.sqrt(n)*eps)
	    
	    index[0] = (int)Math.round(sindex);
	    isccw[0] = (index[0] < 0);
	}
	
	// isinpoly identifies points inside a polygon
	// isinpoly returns a vector the size of z such that each nonzero corresponds to a point inside
	// the polygon defined by w and beta.
	// ontvx = new boolean[w.length][z.length]
	// More precisely, the value returned for a point is the winding number of the polygon about
	// that point.
	// The problem becomes ill-defined for points very near an edge or vertex.  isinpoly considers
	// points within roughly tol of the boundary to be "inside", and computes winding number for such points
	// as the number of conformal images the point ought to have
	// Original MATLAB routine copyright 1998 by Toby Driscoll.
	// Uses the argument principle, with some gymnastics for boundary points.
	private void isinpoly(boolean indexout[], boolean onvtx[][], double z[][], double w[][], double beta[], double tol) {
		int i, j, k, p;
		double cr[] = new double[1];
		double ci[] = new double[1];
	    if (beta == null) {
	    	beta = scangle(w);
	    }
	    int n = w.length;
	    double index[] = new double[z.length];
	    double zcopy[][] = new double[z.length][2];
	    for (i = 0; i < z.length; i++) {
	    	zcopy[i][0] = z[i][0];
	    	zcopy[i][1] = z[i][1];
	    }
	    double wcopy[][] = new double[w.length][2];
	    for (i = 0; i < w.length; i++) {
	    	wcopy[i][0] = w[i][0];
	    	wcopy[i][1] = w[i][1];
	    }
	    
	    
	    // Rescale to make differences relative
	    double diffw[][] = new double[n][2];
	    for (i = 0; i < n-1; i++) {
	        diffw[i][0] = w[i+1][0] - w[i][0];
	        diffw[i][1] = w[i+1][1] - w[i][1];
	    }
	    diffw[n-1][0] = w[0][0] - w[n-1][0];
	    diffw[n-1][1] = w[0][1] - w[n-1][1];
	    double absdiffw[] = new double[n];
	    double sum = 0.0;
	    boolean anygt = false;
	    for (i = 0; i < n; i++) {
	    	absdiffw[i] = zabs(diffw[i][0], diffw[i][1]);
	    	if (absdiffw[i] > eps) {
	    		anygt = true;
	    	}
	    	sum += absdiffw[i];
	    }
	    double scale = sum/n;
	    // Trivial case (e.g., a single or repeated point)
	    if (!anygt) {
	    	return;
	    }
	    
	    for (i = 0; i < n; i++) {
	    	wcopy[i][0] = w[i][0]/scale;
	    	wcopy[i][1] = w[i][1]/scale;
	    }
	    
	    for (i = 0; i < z.length; i++) {
	    	zcopy[i][0] = z[i][0]/scale;
	    	zcopy[i][1] = z[i][1]/scale;
	    }
	    
	    // Array of differences between each z and each w
	    int np = z.length;
	    double d[][][] = new double[n][np][2];
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    	    d[i][j][0] = wcopy[i][0] - zcopy[j][0];
	    	    d[i][j][1] = wcopy[i][1] - zcopy[j][1];
	    	}
	    } // for (i = 0; i < n; i++)
	    
	    // Avoid divides by zero
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    		if (zabs(d[i][j][0], d[i][j][1]) < eps) {
	    			d[i][j][0] = eps;
	    			d[i][j][1] = 0.0;
	    		}
	    	}
	    } // for (i = 0; i < n; i++)
	    
	    // Diffs of imag(log(w-z)) around the polygon
	    double ang[][] = new double[n][np];
	    for (i = 0; i < n-1; i++) {
	    	for (j = 0; j < np; j++) {
	    		zdiv(d[i+1][j][0], d[i+1][j][1], d[i][j][0], d[i][j][1], cr, ci);
	    		ang[i][j] = Math.atan2(ci[0], cr[0])/Math.PI;
	    	}	
	    } // for (i = 0; i < n-1; i++)
	    for (j = 0; j < np; j++) {
	    	zdiv(d[0][j][0], d[0][j][1], d[n-1][j][0], d[n-1][j][1], cr, ci);
	    	ang[i][j] = Math.atan2(ci[0], cr[0])/Math.PI;
	    }


	    
	    // Find boundary points (edge and vertex)
	    double wdiff[][] = new double[n][2];
	    for (i = 0; i < n-1; i++) {
	    	wdiff[i][0] = wcopy[i+1][0] - wcopy[i][0];
	    	wdiff[i][1] = wcopy[i+1][1] - wcopy[i][1];
	    }
	    wdiff[n-1][0] = wcopy[0][0] - wcopy[n-1][0];
	    wdiff[n-1][1] = wcopy[0][1] - wcopy[n-1][1];
	    double tangents[][] = new double[n][2];
	    for (i = 0; i < n; i++) {
	    	zdiv(wdiff[i][0], wdiff[i][1], zabs(wdiff[i][0], wdiff[i][1]), 0, cr, ci);
	    	tangents[i][0] = cr[0];
	    	tangents[i][1] = ci[0];
	    } // for (i = 0; i < n; i++)
	    
	    // If points are repeated (e.g. crowding), skip to new point
	    for (p = 0; p < n; p++) {
	    	if ((tangents[p][0] == 0) && (tangents[p][1] == 0)) {
	    		double v[][] = new double[2*n-p-1][2];
	    		for (i = p+1; i < n; i++) {
	    		    v[i-p-1][0] = wcopy[i][0];
	    		    v[i-p-1][1] = wcopy[i][1];
	    		}
	    		for (i = 0; i < n; i++) {
	    		    v[n-p-1+i][0] = wcopy[i][0];
	    		    v[n-p-1+i][1] = wcopy[i][1];
	    		}
	    		boolean found = false;
	    		for (i = 0; i < v.length && (!found); i++) {
	    			if ((v[i][0] != wcopy[p][0]) || (v[i][1] != wcopy[p][1])) {
	    			    found = true;
	    			    double vdiffreal = v[i][0] - wcopy[p][0];
	    			    double vdiffimag = v[i][1] - wcopy[p][1];
	    			    zdiv(vdiffreal, vdiffimag, zabs(vdiffreal, vdiffimag), 0, cr, ci);
	    			    tangents[p][0] = cr[0];
	    			    tangents[p][1] = ci[0];
	    			}
	    		}
	    	} // if ((tangents[p][0] == 0) && (tangents[p][1] == 0))
	    } // for (p = 0; p < n; p++)
	    
	    // Points which are close to an edge
	    boolean onbdy[][] = new boolean[n][np];
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    		zdiv(d[i][j][0], d[i][j][1],tangents[i][0], tangents[i][1], cr, ci);
	    		if (Math.abs(ci[0]) < 10.0 * tol) {
	    			onbdy[i][j] = true;
	    		}
	    	}
	    } // for (i = 0; i < n; i++)
	    
	    // Points which are essentially vertices
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    		if (zabs(d[i][j][0], d[i][j][1]) < tol) {
	    			onvtx[i][j] = true;
	    		}
	    	}
	    } // for (i = 0; i < n; i++)
	   
	    // Correction: points must be closed, finite edge segment
	    boolean onvtxshift[][] = new boolean[n][np];
	    for (i = 0; i < n-1; i++) {
	    	for (j = 0; j < np; j++) {
	    		onvtxshift[i][j] = onvtx[i+1][j];
	    	}
	    }
	    for (j = 0; j < np; j++) {
	    	onvtxshift[n-1][j] = onvtx[0][j];
	    }
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    		onbdy[i][j] = onbdy[i][j] && ((Math.abs(ang[i][j]) > 0.9)  || onvtx[i][j] || onvtxshift[i][j]);
	    	}
	    }
	    
	    // Truly interior points are easy: add up the args
	    boolean interior[] = new boolean[np];
	    boolean anyinterior = false;
	    for (j = 0; j < np; j++) {
	    	boolean found = false;
	    	interior[j] = true;
	    	for (i = 0; i < n && (!found); i++) {
	    		if (onbdy[i][j]) {
	    			found = true;
	    			interior[j] = false;
	    			
	    		}
	    	}
	    	if (interior[j]) {
	    		anyinterior = true;
	    	}
	    } //  for (j = 0; j < np; j++)
	    
	    if (anyinterior) {
	        for (j = 0; j < np; j++) {
	        	if (interior[j]) {
		        	sum = 0.0;
		        	for (i = 0; i < n; i++) {
		        		sum += ang[i][j];
		        	}
		        	sum = sum/2.0;
		        	index[j] = (int)Math.round(sum);
	        	}
	        }
	    } // if (anyinterior) 
	    
	    // Boundary points are tricky
	    for (k = 0; k < np; k++) {
	        if (!interior[k]) {
	            // Index wrt other parts of polygon
	            double S = 0.0;
	        	for (i = 0; i < n; i++) {
	        		if (!onbdy[i][k]) {
	        			S += ang[i][k];
	        		}
	        	}
	        	// We pretend a vertex point is on either adjacent side
	        	int numb = 0;
	        	for (i = 0; i < n; i++) {
	        		if (onvtx[i][k]) {
	        			numb++;
	        		}
	        	}
	        	double b[] = new double[numb];
	        	for (i = 0, j = 0; i < n; i++) {
	        	    if (onvtx[i][k]) {
	        	    	b[j++] = beta[i];
	        	    }
	        	}
	        	// Each edge membership counts as 1/2 winding number (either side)
	        	double augment = 0.0;
	        	for (i = 0; i < n; i++) {
	        		if (onbdy[i][k]) {
	        			augment += 1.0;
	        		}
	        		if (onvtx[i][k]) {
	        			augment -= 1.0;
	        		}
	        	}
	        	for (i = 0; i < b.length; i++) {
	        		augment -= b[i];
	        	}
	        	index[k] = Math.round(augment*sign(S) + S)/2.0;
	        } // if (!interior[k])
	    } // for (k = 0; k < np; k++)
	    
	    for (i = 0; i < index.length; i++) {
	    	if (index[i] != 0) {
	    		indexout[i] = true;
	    	}
	    }
	}
	
	// isinpoly identifies points inside a polygon
		// isinpoly returns a vector the size of z such that each nonzero corresponds to a point inside
		// the polygon defined by w and beta.
		// ontvx = new boolean[w.length][z.length]
		// More precisely, the value returned for a point is the winding number of the polygon about
		// that point.
		// The problem becomes ill-defined for points very near an edge or vertex.  isinpoly considers
		// points within roughly tol of the boundary to be "inside", and computes winding number for such points
		// as the number of conformal images the point ought to have
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		// Uses the argument principle, with some gymnastics for boundary points.
		private void isinpoly2(int indexout[], boolean onvtx[][], double z[][], double w[][], double beta[], double tol) {
			int i, j, k, p;
			double cr[] = new double[1];
			double ci[] = new double[1];
		    if (beta == null) {
		    	beta = scangle(w);
		    }
		    int n = w.length;
		    double index[] = new double[z.length];
		    double zcopy[][] = new double[z.length][2];
		    for (i = 0; i < z.length; i++) {
		    	zcopy[i][0] = z[i][0];
		    	zcopy[i][1] = z[i][1];
		    }
		    double wcopy[][] = new double[w.length][2];
		    for (i = 0; i < w.length; i++) {
		    	wcopy[i][0] = w[i][0];
		    	wcopy[i][1] = w[i][1];
		    }
		    
		    
		    // Rescale to make differences relative
		    double diffw[][] = new double[n][2];
		    for (i = 0; i < n-1; i++) {
		        diffw[i][0] = w[i+1][0] - w[i][0];
		        diffw[i][1] = w[i+1][1] - w[i][1];
		    }
		    diffw[n-1][0] = w[0][0] - w[n-1][0];
		    diffw[n-1][1] = w[0][1] - w[n-1][1];
		    double absdiffw[] = new double[n];
		    double sum = 0.0;
		    boolean anygt = false;
		    for (i = 0; i < n; i++) {
		    	absdiffw[i] = zabs(diffw[i][0], diffw[i][1]);
		    	if (absdiffw[i] > eps) {
		    		anygt = true;
		    	}
		    	sum += absdiffw[i];
		    }
		    double scale = sum/n;
		    // Trivial case (e.g., a single or repeated point)
		    if (!anygt) {
		    	return;
		    }
		    
		    for (i = 0; i < n; i++) {
		    	wcopy[i][0] = w[i][0]/scale;
		    	wcopy[i][1] = w[i][1]/scale;
		    }
		    
		    for (i = 0; i < z.length; i++) {
		    	zcopy[i][0] = z[i][0]/scale;
		    	zcopy[i][1] = z[i][1]/scale;
		    }
		    
		    // Array of differences between each z and each w
		    int np = z.length;
		    double d[][][] = new double[n][np][2];
		    for (i = 0; i < n; i++) {
		    	for (j = 0; j < np; j++) {
		    	    d[i][j][0] = wcopy[i][0] - zcopy[j][0];
		    	    d[i][j][1] = wcopy[i][1] - zcopy[j][1];
		    	}
		    } // for (i = 0; i < n; i++)
		    
		    // Avoid divides by zero
		    for (i = 0; i < n; i++) {
		    	for (j = 0; j < np; j++) {
		    		if (zabs(d[i][j][0], d[i][j][1]) < eps) {
		    			d[i][j][0] = eps;
		    			d[i][j][1] = 0.0;
		    		}
		    	}
		    } // for (i = 0; i < n; i++)
		    
		    // Diffs of imag(log(w-z)) around the polygon
		    double ang[][] = new double[n][np];
		    for (i = 0; i < n-1; i++) {
		    	for (j = 0; j < np; j++) {
		    		zdiv(d[i+1][j][0], d[i+1][j][1], d[i][j][0], d[i][j][1], cr, ci);
		    		ang[i][j] = Math.atan2(ci[0], cr[0])/Math.PI;
		    	}	
		    } // for (i = 0; i < n-1; i++)
		    for (j = 0; j < np; j++) {
		    	zdiv(d[0][j][0], d[0][j][1], d[n-1][j][0], d[n-1][j][1], cr, ci);
		    	ang[i][j] = Math.atan2(ci[0], cr[0])/Math.PI;
		    }
		    
		    // Find boundary points (edge and vertex)
		    double wdiff[][] = new double[n][2];
		    for (i = 0; i < n-1; i++) {
		    	wdiff[i][0] = wcopy[i+1][0] - wcopy[i][0];
		    	wdiff[i][1] = wcopy[i+1][1] - wcopy[i][1];
		    }
		    wdiff[n-1][0] = wcopy[0][0] - wcopy[n-1][0];
		    wdiff[n-1][1] = wcopy[0][1] - wcopy[n-1][1];
		    double tangents[][] = new double[n][2];
		    for (i = 0; i < n; i++) {
		    	zdiv(wdiff[i][0], wdiff[i][1], zabs(wdiff[i][0], wdiff[i][1]), 0, cr, ci);
		    	tangents[i][0] = cr[0];
		    	tangents[i][1] = ci[0];
		    } // for (i = 0; i < n; i++)
		    
		    // If points are repeated (e.g. crowding), skip to new point
		    for (p = 0; p < n; p++) {
		    	if ((tangents[p][0] == 0) && (tangents[p][1] == 0)) {
		    		double v[][] = new double[2*n-p-1][2];
		    		for (i = p+1; i < n; i++) {
		    		    v[i-p-1][0] = wcopy[i][0];
		    		    v[i-p-1][1] = wcopy[i][1];
		    		}
		    		for (i = 0; i < n; i++) {
		    		    v[n-p-1+i][0] = wcopy[i][0];
		    		    v[n-p-1+i][1] = wcopy[i][1];
		    		}
		    		boolean found = false;
		    		for (i = 0; i < v.length && (!found); i++) {
		    			if ((v[i][0] != wcopy[p][0]) || (v[i][1] != wcopy[p][1])) {
		    			    found = true;
		    			    double vdiffreal = v[i][0] - wcopy[p][0];
		    			    double vdiffimag = v[i][1] - wcopy[p][1];
		    			    zdiv(vdiffreal, vdiffimag, zabs(vdiffreal, vdiffimag), 0, cr, ci);
		    			    tangents[p][0] = cr[0];
		    			    tangents[p][1] = ci[0];
		    			}
		    		}
		    	} // if ((tangents[p][0] == 0) && (tangents[p][1] == 0))
		    } // for (p = 0; p < n; p++)
		    
		    // Points which are close to an edge
		    boolean onbdy[][] = new boolean[n][np];
		    for (i = 0; i < n; i++) {
		    	for (j = 0; j < np; j++) {
		    		zdiv(d[i][j][0], d[i][j][1],tangents[i][0], tangents[i][1], cr, ci);
		    		if (Math.abs(ci[0]) < 10.0 * tol) {
		    			onbdy[i][j] = true;
		    		}
		    	}
		    } // for (i = 0; i < n; i++)
		    // Points which are essentially vertices
		    for (i = 0; i < n; i++) {
		    	for (j = 0; j < np; j++) {
		    		if (zabs(d[i][j][0], d[i][j][1]) < tol) {
		    			onvtx[i][j] = true;
		    		}
		    	}
		    } // for (i = 0; i < n; i++)
		    // Correction: points must be closed, finite edge segment
		    boolean onvtxshift[][] = new boolean[n][np];
		    for (i = 0; i < n-1; i++) {
		    	for (j = 0; j < np; j++) {
		    		onvtxshift[i][j] = onvtx[i+1][j];
		    	}
		    }
		    for (j = 0; j < np; j++) {
		    	onvtxshift[n-1][j] = onvtx[0][j];
		    }
		    for (i = 0; i < n; i++) {
		    	for (j = 0; j < np; j++) {
		    		onbdy[i][j] = onbdy[i][j] && ((Math.abs(ang[i][j]) > 0.9)  || onvtx[i][j] || onvtxshift[i][j]);
		    	}
		    }
		    
		    // Truly interior points are easy: add up the args
		    boolean interior[] = new boolean[np];
		    boolean anyinterior = false;
		    for (j = 0; j < np; j++) {
		    	boolean found = false;
		    	interior[j] = true;
		    	for (i = 0; i < n && (!found); i++) {
		    		if (onbdy[i][j]) {
		    			found = true;
		    			interior[j] = false;
		    		}
		    	}
		    	if (interior[j]) {
		    		anyinterior = true;
		    	}
		    } //  for (j = 0; j < np; j++)
		    
		    if (anyinterior) {
		        for (j = 0; j < np; j++) {
		        	if (interior[j]) {
			        	sum = 0.0;
			        	for (i = 0; i < n; i++) {
			        		sum += ang[i][j];
			        	}
			        	sum = sum/2.0;
			        	index[j] = (int)Math.round(sum);
		        	}
		        }
		    } // if (anyinterior) 
		    
		    // Boundary points are tricky
		    for (k = 0; k < np; k++) {
		        if (!interior[k]) {
		            // Index wrt other parts of polygon
		            double S = 0.0;
		        	for (i = 0; i < n; i++) {
		        		if (!onbdy[i][k]) {
		        			S += ang[i][k];
		        		}
		        	}
		        	// We pretend a vertex point is on either adjacent side
		        	int numb = 0;
		        	for (i = 0; i < n; i++) {
		        		if (onvtx[i][k]) {
		        			numb++;
		        		}
		        	}
		        	double b[] = new double[numb];
		        	for (i = 0, j = 0; i < n; i++) {
		        	    if (onvtx[i][k]) {
		        	    	b[j++] = beta[i];
		        	    }
		        	}
		        	// Each edge membership counts as 1/2 winding number (either side)
		        	double augment = 0.0;
		        	for (i = 0; i < n; i++) {
		        		if (onbdy[i][k]) {
		        			augment += 1.0;
		        		}
		        		if (onvtx[i][k]) {
		        			augment -= 1.0;
		        		}
		        	}
		        	for (i = 0; i < b.length; i++) {
		        		augment -= b[i];
		        	}
		        	index[k] = Math.round(augment*sign(S) + S)/2.0;
		        } // if (!interior[k])
		    } // for (k = 0; k < np; k++)
		    
		    for (i = 0; i < index.length; i++) {
		        indexout[i] = (int)index[i];
		    }
		}
	
	// scangle computes the turning angles of the polygon whose vertices are specified in the vector w.
	// The turning angle of a vertex measures how much the heading changes at that vertex from the
	// incoming to the outgoing edge, normalized by pi.  For a finite vertex, it is equal in absolute
	// value to (exterior angle)/pi, with a negative sign for left turns and positive for right turns.
	// Thus the turn at a finite vertex is in (-1,1], with 1 meaning a slit.  At a infinite vertex the
	// turning angle is in the range [-3, -1] and is equal to the exterior angle of the two sides 
	// extended back from infinity, minus 2.  scangle cannot determine the angle at an infinite vertex
	// or its neighbors, and will return NaN's in those positions.
	// Original code copyright 1998 by Toby Driscoll.
	private double[] scangle(double w[][]) {
		int i;
		double beta[];
		double realPart;
		double imagPart;
		int n = w.length;
		if (n == 0) {
			beta = null;
			return beta;
		}
		// Cannot determine if at an infinite vertex or next to an infinite vertex
		boolean mask[] = new boolean[n];
		for (i = 0; i < n; i++) {
		    mask[i] =true;	
		}
		for (i = 0; i < n; i++) {
			if (i == 0) {
				if ((Double.isInfinite(w[n-1][0])) || (Double.isInfinite(w[n-1][1])) ||
				    (Double.isInfinite(w[i][0])) || (Double.isInfinite(w[i][1])) ||
					(Double.isInfinite(w[i+1][0])) || (Double.isInfinite(w[i+1][1]))) {
					mask[i] = false;
				}
			} // if (i == 0)
			else if (i == n-1) {
				if ((Double.isInfinite(w[i-1][0])) || (Double.isInfinite(w[i-1][1])) ||
				    (Double.isInfinite(w[i][0])) || (Double.isInfinite(w[i][1])) ||
					(Double.isInfinite(w[0][0])) || (Double.isInfinite(w[0][1]))) {
					mask[i] = false;
				}	
			}
			else {
				if ((Double.isInfinite(w[i-1][0])) || (Double.isInfinite(w[i-1][1])) ||
				    (Double.isInfinite(w[i][0])) || (Double.isInfinite(w[i][1])) ||
					(Double.isInfinite(w[i+1][0])) || (Double.isInfinite(w[i+1][1]))) {
					mask[i] = false;
				}
			}
		}
		double dw[][] = new double[n][2];
		dw[0][0] = w[0][0] - w[n-1][0];
		dw[0][1] = w[0][1] - w[n-1][1];
		for (i = 0; i < n-1; i++) {
			dw[i+1][0] = w[i+1][0] - w[i][0];
			dw[i+1][1] = w[i+1][1] - w[i][1];
		}
		double dwshift[][] = new double[n][2];
		for (i = 0; i < n-1; i++) {
			dwshift[i][0] = dw[i+1][0];
			dwshift[i][1] = dw[i+1][1];
		}
		dwshift[n-1][0] = dw[0][0];
		dwshift[n-1][1] = dw[0][1];
		beta = new double[w.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = Double.NaN;
		}
		for (i = 0; i < n; i++) {
			if (mask[i]) {
				realPart = (dw[i][0]*dwshift[i][0] + dw[i][1] * dwshift[i][1]);
				imagPart = (-dw[i][0]*dwshift[i][1] + dw[i][1] * dwshift[i][0]);
				beta[i] = Math.atan2(imagPart, realPart)/Math.PI;
				// It's ill-posed to tell a point (outward) from a slit (inward).
				// Since the latter is much more common and important, we'll be generous in
				// giving it the tie.
				if (Math.abs(beta[i]+1) < 1.0E-12) {
					beta[i] = 1.0;
				}
			}
		}
		return beta;
	}
	
	private void deparam(double z[][], double c[], double qdat[][], 
			double w[][], double beta[], double z0[][], double tol) {
		// Schwarz- Christoffel exterior parameter problem
		// deparam solves the Schwarz-Christoffel mapping problem with a disk as 
		// fundamental domain and the exterior of the polygon specified by w
		// as the target.  w must be a vector of vertices of the polygon, 
		// specified in clockwise order, and beta should be a vector of 
		// turning angles of the polygon.  If successful, deparam will return
		// z, a vector of pre-images of w; c, the multiplicative constant of the
		// conformal map; and qdat, an optional matrix of quadrature data used
		// by some of the other S-C routines.
		
		// Original MATLAB deparam routine copyright 1998 by Toby Driscoll.
		
		int i;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int n = w.length; // Number of vertices
		
		int err = sccheck("de", w, beta, null);
	    if (err == -1) {
	    	return;
	    }
	    if (err == 1) {
	    	MipavUtil.displayError("Use scfix to make polygon obey requirements");
	    	return;
	    }
	    
	    int nqpts = (int)Math.max(Math.ceil(-Math.log10(tol)),2);
	    scqdata(qdat, beta, nqpts);  // quadrature data
	    
	    if (n == 2) {
	    	// it's a slit
	    	z[0][0] = -1;
	    	z[0][1] = 0;
	    	z[1][0] = 1;
	    	z[1][1] = 0;
	    }
	    else { // n != 2
	    	// Set up normalized lengths for nonlinear equations
	    	double len[] = new double[n];
	    	len[0] = zabs(w[0][0] - w[n-1][0], w[0][1] - w[n-1][1]);
	    	for (i = 0; i < n-1; i++) {
	    		len[i+1]= zabs(w[i+1][0] - w[i][0], w[i+1][1] - w[i][1]);
	    	}
	    	double nmlen[] = new double[n-3];
	    	for (i = 0; i < n-3; i++) {
	    		nmlen[i] = len[i+2]/len[1];
	    	}
	    	
	    	// Set up initial guess
	    	double y0[] = null;
	    	if ((z0 == null) || (z0.length == 0)) {
	    	    y0 = new double[n-1];	
	    	}
	    	else {
	    		// Must have z0 with increasing angle so log is real
	    		// A negative number would give the log of the absolute value + i*PI.
	    		// Fix z0[n-1] = 1
	    		for (i = 0; i < n; i++) {
	    			zdiv(z0[i][0], z0[i][1], z0[n-1][0], z0[n-1][1], cr, ci);
	    		}
	    		double th[] = new double[n];
	    		for (i = 0; i < n; i++) {
	    			th[i] = Math.atan2(z0[i][1], z0[i][0]);
	    			if (th[i] <= 0) {
	    				th[i] = th[i] + 2.0*Math.PI;
	    			}
	    		} // for (i = 0; i < n; i++) 
	    		double dt[] = new double[n];
	    		dt[0] = th[0];
	    		if (dt[0] <= 0) {
	    			System.err.println("dt[0] <= 0 in deparam");
	    			System.exit(-1);
	    		}
	    		for (i = 1; i < n-2; i++) {
	    			dt[i] = th[i+1] - th[i];
	    			if (dt[i] <= 0) {
	    				System.err.println("dt["+i+"] <= 0 in deparam");
		    			System.exit(-1);	
	    			}
	    		}
	    		dt[n-1] = 2.0*Math.PI - th[n-2];
	    		if (dt[n-1] <= 0) {
	    			System.err.println("dt[n-1] <= 0 in deparam");
	    			System.exit(-1);
	    		}
	    		for (i = 0; i < n-1; i++) {
	    		    y0[i] = Math.log(dt[i]/dt[i+1]);	
	    		}
	    	}
	    	
	    	// Solve nonlinear system of equations
	    	depfun fm = new depfun(y0, n, beta, nmlen, qdat);
		    fm.driver();
			fm.dumpResults();
		    int exitStatus = fm.getExitStatus();
		    if (exitStatus < 0 ) {
		    	System.out.println("Error in NLConstrainedEngine during dparam call to depfun");
		    	printExitStatus(exitStatus);
		    	System.exit(-1);
		    }
			double y[] = fm.getParameters();
			// Convert y values to z
			double cumprod[] = new double[y.length+1];
			cumprod[0] = 1.0;
			for (i = 1; i <= y.length; i++) {
				cumprod[i] = cumprod[i-1] * Math.exp(-y[i-1]);
			}
			double cs[] = new double[y.length+1];
			cs[0] = cumprod[0];
			for (i = 1; i <= y.length; i++) {
				cs[i] = cs[i-1] + cumprod[i];
			}
			double theta[] = new double[n-1];
			for (i = 0; i < n-1; i++) {
				theta[i] = 2.0 * Math.PI * cs[i]/cs[n-1];
			}
			for (i = 0; i < n-1; i++) {
				z[i][0] = Math.cos(theta[i]);
				z[i][1] = Math.sin(theta[i]);
			}
			
			z[n-1][0] = 1.0;
			z[n-1][1] = 0.0;
	    } // else n != 2
	    
	    // Determine scaling constant
	    zdiv(z[1][0], z[1][1], z[0][0], z[0][1], cr, ci);
	    double ang = Math.atan2(ci[0], cr[0])/2.0;
	    zmlt(z[0][0], z[0][1], Math.cos(ang), Math.sin(ang), cr, ci);
	    double mid[][] = new double[1][2];
	    mid[0][0] = cr[0];
	    mid[0][1] = ci[0];
	    double wdiff[] = new double[2];
	    wdiff[0] = w[1][0] - w[0][0];
	    wdiff[1] = w[1][1] - w[0][1];
	    double zin[][] = new double[1][2];
	    zin[0][0] = z[0][0];
	    zin[0][1] = z[0][1];
	    int sing[] = new int[1];
	    sing[0] = 0;
	    double I1[][] = dequad(zin, mid, sing, z, beta, qdat);
	    zin[0][0] = z[1][0];
	    zin[0][1] = z[1][1];
	    sing[0] = 1;
	    double I2[][] = dequad(zin, mid, sing, z, beta, qdat);
	    double denom[] = new double[2];
	    denom[0] = I1[0][0] - I2[0][0];
	    denom[1] = I1[0][1] - I2[0][1];
	    zdiv(wdiff[0], wdiff[1], denom[0], denom[1], cr, ci);
	    c[0] = cr[0];
	    c[1] = ci[0];
	    return;
	}
	
	// Schwarz-Crhistoffel disk parameter problem.
	// dparam soves the Schwarz-Christoffel mapping parameter problem with the disk
	// as fundamental domain and the polygon specified by w as the target. w must be
	// a vector of the vertices of the polygon, specified in counterclockwise order,
	// and beta should be a vector of the turning angles of the polygon, see scangle
	// for details.  If successful, dparam will return z, a vector of the pre-images of w;
	// c, the multiplicative constant of the conformal map; and qdat, an optional
	// matrix of quadrature data used by some of the other S-C routines. z0 is an initial
	// guess for z.  dparam attempts to find an answer within tolerance tol.
	// Original MATLAB dparam copyright 1998-2001 by Toby Driscoll.
	private void dparam(double z[][], double c[], double qdat[][], 
			double w[][], double beta[], double z0[][], double tol) {
		int i, j, k;
		double cr[] = new double[1];
		double ci[] = new double[1];
	    int n = w.length; // number of vertices
	    
	    // Check input data
	    int err = sccheck("d", w, beta, null);
	    if (err == -1) {
	    	return;
	    }
	    if (err == 1) {
	    	MipavUtil.displayError("Use scfix to make polygon obey requirements");
	    	return;
	    }
	    
	    int nqpts = (int)Math.max(Math.ceil(-Math.log10(tol)),4);
	    scqdata(qdat, beta, nqpts);  // quadrature data
	    
	    boolean atinf[] = new boolean[beta.length];
	    for (i = 0; i < beta.length; i++) {
	    	if (beta[i] <= -1) {
	    		atinf[i] = true;
	    	}
	    } // for (i = 0; i < beta.length; i++)
	    
	    if (n == 3) {
	    	// Trivial solution
	    	z[0][0] = 0.0;
	    	z[0][1] = -1.0;
	    	double sqrt2 = Math.sqrt(2.0);
	    	z[1][0] = 1.0/sqrt2;
	    	z[1][1] = -1.0/sqrt2;
	    	z[2][0] = 1.0;
	    	z[2][1] = 0.0;
	    } // if (n == 3)
	    else {
	        // Set up normalized lengths for nonlinear equations
	    	
	    	// indices of left and right integration endpoints
	    	int numleft = 0;
	    	for (i = 0; i < n-2; i++) {
	    		if (!atinf[i]) {
	    			numleft++;
	    		}
	    	} // for (i = 0; i < n-2; i++)
	    	int left[] = new int[numleft];
	    	for (i = 0, j = 0; i < n-2; i++) {
	    		if (!atinf[i]) {
	    			left[j++] = i;
	    		}
	    	} // for (i = 0, j = 0; i < n-2; i++)
	    	int numright = 0;
	    	for (i = 1; i < n-1; i++) {
	    		if (!atinf[i]) {
	    			numright++;
	    		}
	    	} // for (i = 1; i < n-1; i++)
	    	int right[] = new int[numright];
	    	for (i = 1, j = 0; i < n-1; i++) {
	    	    if (!atinf[i]) {
	    	    	right[j++] = i;
	    	    }
	    	} // for (i = 1, j = 0; i < n-1; i++)
	    	// Assumes numright == numleft
	    	boolean cmplx[] = new boolean[numright];
	    	int numcmplx = 0;
	    	for (i = 0; i < numright; i++) {
	    		if ((right[i] - left[i]) == 2) {
	    			cmplx[i] = true;
	    			numcmplx++;
	    		}
	    	} // for (i = 0; i < numright; i++)
	    	// Normalize lengths by w[1] - w[0]
	    	double diffw[] = new double[2];
	    	diffw[0] = w[1][0] - w[0][0];
	    	diffw[1] = w[1][1] - w[0][1];
	    	double nmlen[][] = new double[numright][2];
	    	for (i = 0; i < numright; i++) {
	    	    zdiv(w[right[i]][0] - w[left[i]][0], w[right[i]][1] - w[left[i]][1], 
	    	    		diffw[0], diffw[1], cr, ci);
	    	    nmlen[i][0] = cr[0];
	    	    nmlen[i][1] = ci[0];
	    	} // for (i = 0; i < numright; i++)
	    	// abs value for finite ones; Re/Im or Infinite ones
	    	int numnotcmplx = numright-numcmplx;
	    	double nmlen2[] = new double[numnotcmplx + 2*numcmplx];
	    	for (i = 0, j = 0, k = 0; i < numright; i++) {
	    	    if (!cmplx[i]) {
	    	    	nmlen2[j++] = zabs(nmlen[i][0], nmlen[i][1]);
	    	    }
	    	    else {
	    	    	nmlen2[numnotcmplx + k] = nmlen[i][0];
	    	    	nmlen2[numnotcmplx + numcmplx + k] = nmlen[i][1];
	    	    	k++;
	    	    }
	    	} // for (i = 0, j = 0, k = 0; i < numright; i++)
	    	// First entry is useless (= 0)
	    	double nmlen3[] = new double[nmlen2.length-1];
	    	for (i = 0; i < nmlen3.length; i++) {
	    		nmlen3[i] = nmlen2[i+1];
	    	}
	    	
	    	// Set up initial guess
	    	double y0[] = null;
	    	if ((z0 == null) || (z0.length == 0)) {
	    		y0 = new double[n-3];
	    	}
	    	else {
	    		double z02[][] = new double[z0.length][2];
	    		for (i = 0; i < z0.length; i++) {
	    			zdiv(z0[i][0], z0[i][1], zabs(z0[i][0], z0[i][1]), 0.0, cr, ci);
	    			z02[i][0] = cr[0];
	    			z02[i][1] = ci[0];
	    		} // for (i = 0; i < z0.length; i++)
	    		// Moebius to make th(n-3:n-1) = [1,1.5,2]*PI
	    		// The MATLAB lines are:
	    		// Am = moebius(z0(n-2:n),[-1;-i;1]));
	    		// z0 = Am(z0);
	    		// The line Z0 = Am(z0); generates the error message:
	    		// Subscript indices must be either real positive integers or logicals.
	    	} // else
	    	// Solve nonlinear system of equations
		    dpfun fm = new dpfun(y0, n, beta, nmlen3, left, right, cmplx, qdat);
		    fm.driver();
			fm.dumpResults();
		    int exitStatus = fm.getExitStatus();
		    if (exitStatus < 0 ) {
		    	System.out.println("Error in NLConstrainedEngine during dparam call to dpfun");
		    	printExitStatus(exitStatus);
		    	System.exit(-1);
		    }
			double y[] = fm.getParameters();
			
			// Convert y values to z
			double cumprod[] = new double[y.length+1];
			cumprod[0] = 1.0;
			for (i = 1; i <= y.length; i++) {
				cumprod[i] = cumprod[i-1] * Math.exp(-y[i-1]);
			}
			double cs[] = new double[y.length+1];
			cs[0] = cumprod[0];
			for (i = 1; i <= y.length; i++) {
				cs[i] = cs[i-1] + cumprod[i];
			}
			double theta[] = new double[n-3];
			for (i = 0; i < n-3; i++) {
				theta[i] = Math.PI * cs[i]/cs[n-3];
			}
			for (i = 0; i < n-3; i++) {
				z[i][0] = Math.cos(theta[i]);
				z[i][1] = Math.sin(theta[i]);
			}
			z[n-3][0] = -1.0;
			z[n-3][1] = 0.0;
			z[n-2][0] = 0.0;
			z[n-2][1] = -1.0;
			z[n-1][0] = 1.0;
			z[n-1][1] = 0.0;
	    } // else
	    	    
	    // Determine scaling constant
	    double mid[][] = new double[1][2];
	    mid[0][0] = (z[0][0] + z[1][0])/2.0;
	    mid[0][1] = (z[0][1] + z[1][1])/2.0;
	    double z1[][] = new double[1][2];
	    z1[0][0] = z[1][0];
	    z1[0][1] = z[1][1];
	    int sing1[] = new int[1];
	    sing1[0] = 1;
	    double z02[][] = new double[1][2];
	    z02[0][0] = z[0][0];
	    z02[0][1] = z[0][1];
	    int sing0[] = new int[1];
	    sing0[0] = 0;
	    double I1[][] = dquad(z1, mid, sing1, z, beta, qdat);
	    double I2[][] = dquad(z02, mid, sing0, z, beta, qdat);
	    double denom[] = new double[2];
	    denom[0] = I1[0][0] - I2[0][0];
	    denom[1] = I1[0][1] - I2[0][1];
	    double num[] = new double[2];
	    num[0] = w[0][0] - w[1][0];
	    num[1] = w[0][1] - w[1][1];
	    zdiv(num[0], num[1], denom[0], denom[1], cr, ci);
	    c[0] = cr[0];
	    c[1] = ci[0];
	    return;
	}
	
	
	// Schwarz-Christoffel rectangle parameter problem
	// rparam solves the Schwarz-Christoffel parameter problem with a rectangle as a fundamental domain
	// and the interior of the specified polygon as the target.  
	// w must be a vector of the vertices of the polygon, specified in a counterclockwise order.
	// w is a complex number in the original MATLAB.
	// beta is a vector of the turning angles; see scangle
	// The interior angles of the polygon are alpha1*PI,..., alphan*PI.
	// (1 - alphak)*PI = betak*PI.
	// The angle betak*PI is the turning angle at vertex k.
	// cnr is a 4-component vector specifying the indices of the vertices which are which are the
	// images of the corners of the rectangle.  ** Be sure ** the first two entries describe the LONG
	// sides of the rectangle, and go in counterclockwise order.  If cnr is omitted, the user is 
	// requested to select these vertices using the mouse.
	// If successful, rparam will return z, a vector of prevertices;
	// c, the multiplicative constant of the conformal map;
	// L, a parameter related to aspect ratio;
	// and qdat, an optional matrix of quadrature data used by some of the other sc routines.
	// rparam uses z0 as an initial guess for z.  In this case, z0 represents the image of 
	// prevertices on the strip 0 <= Im z <= 1.  You can use r2strip to transform from the rectangle to 
	// the strip.
	// rparam attempts to find an answer within the tolerance tol.  (Also see scparopt.)
	// rparam uses a vector of control parameters.  See scparopt.
	// Original MATLAB code copyright 1998 by Toby Driscoll.
	// NESolve has been replaced the the MIPAV implementation of ELSUNC
	private void rparam(double z[][], double c[], double L[], double qdat[][], 
			double w[][], double beta[], int cnr[], double z0[][], double tol) {
		int i, j, k, m, p;
		double diffR;
		double diffI;
		double sum;
		int num;
		double mean;
		double sum1;
		double sum2;
		double len;
		double wid;
		double modest;
		int npoints;
		double spacing;
		double dx;
		int upper;
		double cr[] = new double[1];
		double ci[] = new double[1];
	    int n = w.length;  // number of vertices
	    
	    // cnr should be set by initial MIPAV dialog, so this should never happen.
	    // The polygon as specified by w and beta is drawn on the image and the
	    // user selects 4 vertices using the mouse.
	    if ((cnr == null) || (cnr.length == 0)) {
	    	String msg[] = new String[2];
	        msg[0] = "Select the images of the corners of the rectangle";
	        msg[1] = "Go in counerclockwise order and select a long rectangle edge first";
	        cnr = scselect(w, beta, 4, "Select corners", msg);
	    } // if ((cnr == null) || (cnr.length == 0))
	
	    // Renumber the vertices so that cnr[0] = 0.
	    int renum[] = new int[n];
	    for (i = cnr[0]; i < n; i++) {
	    	renum[i-cnr[0]] = i;
	    }
	    for (i = 0; i <= cnr[0]-1; i++) {
	    	renum[n-cnr[0]+i] = i;
	    }
	    double wcopy[][] = new double[n][2];
	    double betacopy[] = new double[n];
	    for (i = cnr[0]; i < n; i++) {
	        wcopy[i-cnr[0]][0] = w[i][0];
	        wcopy[i-cnr[0]][1] = w[i][1];
	        betacopy[i-cnr[0]] = beta[i];
	    }
	    for (i = 0; i <= cnr[0]-1; i++) {
	    	wcopy[i+n-cnr[0]][0] = w[i][0];
	    	wcopy[i+n-cnr[0]][1] = w[i][1];
	    	betacopy[i+n-cnr[0]] = beta[i];
	    }
	    
	    int offset = n - cnr[0]; // cnr - cnr[0] so no need to add one
	    int cnrcopy[] = new int[cnr.length];
	    for (i = 0; i < cnr.length; i++) {
	    	cnrcopy[i] = ((cnr[i] + offset) % n);
	    }
	    double z0copy[][] = null;
	    if (z0 != null) {
		    z0copy = new double[z0.length][2];
		    for (i = 0; i < z0.length; i++) {
		    	z0copy[i][0] = z0[i][0];
		    	z0copy[i][1] = z0[i][1];
		    }
		    
		    if (z0copy.length == 1) {
		    	tol = z0copy[0][0];
		    	z0copy = null;
		    }
	    } // if (z0 != null)
	    int nqpts = Math.max((int)Math.ceil(-Math.log10(tol)), 4);
	    scqdata(qdat, betacopy, nqpts);  // quadrature data
	    
	    // Check input data
	    int err = sccheck("r", wcopy, betacopy, cnrcopy);
	    if (err == -1) {
	    	return;
	    }
	    if (err == 1) {
	    	MipavUtil.displayError("Use scfix to make polygon obey requirements");
	    	return;
	    }
	    
	    boolean atinf[] = new boolean[n];
	    for (i = 0; i < n; i++) {
	    	atinf[i] = (betacopy[i] <= -1);
	    }
	    
	    if (z0copy == null) {
	        // Try to find a reasonable initial guess
	    	// side lengths
	    	double dw[]= new double[n];
	    	for (i = 0; i < n-1; i++) {
	    		diffR = wcopy[i+1][0] - wcopy[i][0];
	    		diffI = wcopy[i+1][1] - wcopy[i][1];
	    		dw[i] = zabs(diffR,diffI);
	    	} // for (i = 0; i < n-1; i++)
	    	diffR = wcopy[0][0] - wcopy[n-1][0];
	    	diffI = wcopy[0][1] - wcopy[n-1][1];
	    	dw[n-1] = zabs(diffR,diffI);
	        sum = 0.0;
	        num = 0;
	        for (i = 0; i < n; i++) {
	        	if ((!Double.isInfinite(dw[i])) && (!Double.isNaN(dw[i]))) {
	        		sum += dw[i];
	        		num++;
	        	}
	        } // for (i = 0; i < n; i++)
	        mean = sum/num;
	        for (i = 0; i < n; i++) {
	        	if (Double.isInfinite(dw[i]) || Double.isNaN(dw[i])) {
	        		dw[i] = mean;
	        	}
	        } // for (i = 0; i < n; i++)
	        // Estimate length and width (conformal modulus)
	        sum1 = 0.0;
	        sum2 = 0.0;
	        for (i = cnrcopy[0]; i <= cnrcopy[1]-1; i++) {
	        	sum1 += dw[i];
	        }
	        for (i = cnrcopy[2]; i <= cnrcopy[3]-1; i++) {
	        	sum2 += dw[i];
	        }
	        len = (sum1 + sum2)/2.0;
	        sum1 = 0.0;
	        sum2 = 0.0;
	        for (i = cnrcopy[1]; i <= cnrcopy[2]-1; i++) {
	        	sum1 += dw[i];
	        }
	        for (i = cnrcopy[3]; i <= n-1; i++) {
	        	sum2 += dw[i];
	        }
	        for (i = 0; i <= cnrcopy[0]-1; i++) {
	        	sum2 += dw[i];
	        }
	        wid = (sum1 + sum2)/2.0;
	        modest = Math.min(len/wid, 100);
	        // Evenly space prevertices to match this conformal modulus.
	        // z0[0] to z0[cnr[2]-1] has an imaginary part = 0 with increasing positive numbers
	        // z0[cnr[2]] to z0[n-1] has an imaginary part = 1 with decreasing positive numbers.
	        // Only dz[cnr[2]-1] has an imaginary part and this is not used in forming y0
	        // Initially dz fron 0 to cnrcopy[2] -1 is positive and dz from concopy[2] to n-2 is negative
	        // However, dz from cnrcopy[2] to n-1 is multiplied by negative one to become positive
	        // Therefore, all the dz used in forming y0 are positive real numbers.
	        // y0 is all real
	        npoints = cnrcopy[1] - cnrcopy[0] +1;
	        spacing = modest/(npoints - 1.0);
	        z0copy = new double[n][2];
	        for (i = 0; i < npoints-1; i++) {
	            z0copy[cnrcopy[0]+i][0] = i*spacing;	
	        }
	        z0copy[cnrcopy[1]][0] = modest;
	        dx = z0copy[cnrcopy[0]+1][0] - z0copy[cnrcopy[0]][0];
	        for (i = 1; i <= cnrcopy[0]-1; i++) {
	        	z0copy[cnrcopy[0]-i][0] = z0copy[cnrcopy[0]][0] - dx * i;
	        }
	        upper = cnrcopy[2] - cnrcopy[1] - 1;
	        for (i = 1; i <= upper; i++) {
	        	z0copy[cnrcopy[1]+i][0] = z0copy[cnrcopy[1]][0] + dx * i;
	        }
	        npoints = cnrcopy[3] - cnrcopy[2] + 1;
	        spacing = modest/(npoints - 1.0);
	        for (i = 0; i < npoints-1; i++) {
	        	z0copy[cnrcopy[3]-i][0] = i*spacing;
	        	z0copy[cnrcopy[3]-i][1] = 1;
	        }
	        z0copy[cnrcopy[2]][0] = modest; 
	        z0copy[cnrcopy[2]][1] = 1;
	        dx = z0copy[cnrcopy[3]-1][0] - z0copy[cnrcopy[3]][0];
	        for (i = 1; i <= n-1-cnrcopy[3]; i++) {
	        	z0copy[cnrcopy[3]+i][0] = z0copy[cnrcopy[3]][0] - dx*i;
	        	z0copy[cnrcopy[3]+i][1] = 1;
	        }
	    } // if (z0copy == null)
	    else {
	    	if (z0copy.length != n) {
	    		MipavUtil.displayError("Initial guess has wrong number of prevertices");
	    		return;
	    	}
	        z0copy = new double[n][2];
	    	for (i = cnrcopy[0]; i < n; i++) {
		        z0copy[i-cnrcopy[0]][0] = z0[i][0];
		        z0copy[i-cnrcopy[0]][1] = z0[i][1];
		    }
		    for (i = 0; i <= cnrcopy[0]-1; i++) {
		    	z0copy[i+n-cnrcopy[0]][0] = z0[i][0];
		    	z0copy[i+n-cnrcopy[0]][1] = z0[i][1];
		    }
		    for (i = 0; i < n; i++) {
		    	z0copy[i][1] = Math.round(z0copy[i][1]);
		    }
		    
            for (i = cnrcopy[0]; i <= cnrcopy[1]; i++) {
            	if (z0copy[i][1] == 0) {
            		MipavUtil.displayError("Initial guess has prevertices on wrong side of strip");
            		return;
            	}
            } // for (i = cnrcopy[0]; i <= cnrcopy[1]; i++)
            for (i = cnrcopy[2]; i <= cnrcopy[3]; i++) {
            	if (z0copy[i][1] == 0) {
            		MipavUtil.displayError("Initial guess has prevertices on wrong side of strip");
            		return;
            	}
            } // for (i = cnrcopy[2]; i <= cnrcopy[3]; i++)
	    } // else
	    
	    // Convert z0 to unconstrained vars
	    double y0[] = new double[n-3];
	    double dz[][] = new double[n-1][2];
	    for (i = 0; i < n-1; i++) {
	    	dz[i][0] = z0copy[i+1][0] - z0copy[i][0];
	    	dz[i][1] = z0copy[i+1][1] - z0copy[i][1];
	    }
	    for (i = cnrcopy[2]; i < n-1; i++) {
	    	dz[i][0] = -dz[i][0];
	    }
	    for (i = 0; i <= cnrcopy[1]-2; i++) {
	    	y0[i] = Math.log(dz[i][0]);
	    }
	    for (i = cnrcopy[2]-1; i <= cnrcopy[3]-3; i++) {
	    	y0[i] = Math.log(dz[i+2][0]);	
	    }
	    y0[cnrcopy[1]-1] = (Math.log(dz[cnrcopy[1]-1][0]) + Math.log(dz[cnrcopy[2]][0]))/2.0;
	    
	    
	    // Vertices on the "short" edges are transformed into the interval [-1,1],
	    // and then the Trefethen-style transformation is used.
	    L[0] = z0copy[cnrcopy[1]][0] - z0copy[cnrcopy[0]][0];
	    L[1] = z0copy[cnrcopy[1]][1] - z0copy[cnrcopy[0]][1];
	    double x[] = new double[cnrcopy[2]-cnrcopy[1]-1];
	    for (i = cnrcopy[1]+1; i <= cnrcopy[2]-1; i++) {
	    	x[i-cnrcopy[1]-1] = Math.exp(Math.PI*(L[0] - z0copy[i][0]))*Math.cos(Math.PI*(L[1] + z0copy[i][1]));
	    }
	    double dx2[] = new double[x.length+1];
	    if (x.length >= 1) {
		    dx2[0] = 1 - x[0];
		    for (i = 0; i < x.length-1; i++) {
		    	dx2[i+1] = x[i] - x[i+1];
		    }
		    dx2[x.length] = x[x.length-1] + 1;
	    }
	    else {
	    	dx2[0] = 2.0;
	    }
	    for (i = 0; i <= dx2.length-2; i++) {
	        y0[i+cnrcopy[1]] = Math.log(dx2[i]/dx2[i+1]);
	    }
	    x = new double[n-1-cnrcopy[3]];
	    for (i = 0; i < n-1-cnrcopy[3]; i++) {
	    	x[i] = Math.exp(Math.PI*z0copy[cnrcopy[3]+1+i][0])*Math.cos(Math.PI*z0copy[cnrcopy[3]+1+i][1]);
	    }
	    dx2 = new double[x.length+1];
	    if (x.length >= 1) {
		    dx2[0] = x[0]+1;
		    for (i = 0; i < x.length-1; i++) {
		    	dx2[i+1] = x[i+1] - x[i];
		    }
		    dx2[x.length] = 1 - x[x.length-1];
	    }
	    else {
	    	dx2[0] = 2.0;
	    }
	    for (i = 0; i <= dx2.length-2; i++) {
	    	y0[cnrcopy[3]-2+i] = Math.log(dx2[i]/dx2[i+1]);
	    }
	    
	    // Find prevertices (solve param problem)
	    
	    // Set up normalized lengths for nonlinear equations:
	    // indices of left and right integration endpoints
	    // Delete indices corresponding to vertices at infinity
	    int numleft = 0;
	    int numright = 0;
	    for (i = 0; i < n-2; i++) {
	    	if (!atinf[i]) {
	    		numleft++;
	    	}
	    	if (!atinf[i+1]) {
	    		numright++;
	    	}
	    }
	    if (atinf[n-2]) {
	    	numright++;
	    }
	    int left[] = new int[numleft];
	    int right[] = new int[numright];
	    int jl = 0;
	    int jr = 0;
	    for (i = 0; i < n-2; i++) {
	    	if (!atinf[i]) {
	    		left[jl++] = i;
	    	}
		    if (!atinf[i+1]) {
		    	right[jr++] = i+1;
		    }
	    }
	    if (atinf[n-2]) {
	    	right[jr++] = n-1;
	    }
	    if (left.length != right.length) {
	    	MipavUtil.displayError("left.length != right.length");
	    	return;
	    }
	    boolean cmplx[] = new boolean[left.length];
	    int numcmplx = 0;
	    for (i = 0; i < left.length; i++) {
	    	if ((right[i] - left[i]) == 2) {
	    		cmplx[i] = true;
	    		numcmplx++;
	    	}
	    }
	    // It's possible we replaced the last single condition by a complex one
	    if ((cmplx.length + numcmplx) > n-2) {
	    	cmplx[cmplx.length-1] = false;
	    }
	    // Normalize lengths by w[1] - w[0]
	    double wdenomreal = wcopy[1][0] - wcopy[0][0];
	    double wdenomimag = wcopy[1][1] - wcopy[0][1];
	    
	    // First entry is useless = 1.
	    double nmlen[][] = new double[left.length-1][2];
	    for (i = 1; i < left.length; i++) {
	    	double realpart = wcopy[right[i]][0] - wcopy[left[i]][0];
	    	double imagpart = wcopy[right[i]][1] - wcopy[left[i]][1];
	    	zdiv(realpart, imagpart, wdenomreal, wdenomimag, cr, ci);
	    	nmlen[i-1][0] = cr[0];
	    	nmlen[i-1][1] = ci[0];
	    	// Absolute value for finite ones
	    	if (!cmplx[i]) {
	    		nmlen[i-1][0] = zabs(nmlen[i-1][0],nmlen[i-1][1]);
	    		nmlen[i-1][1] = 0;
	    	}
	    } // for (i = 1; i < left.length; i++)
	    
	    // Solve nonlinear system of equations
	    rpfun fm = new rpfun(y0, n, betacopy, nmlen, left, right, cmplx, qdat, cnrcopy);
	    fm.driver();
		fm.dumpResults();
	    int exitStatus = fm.getExitStatus();
	    if (exitStatus < 0 ) {
	    	printExitStatus(exitStatus);
	    	return;
	    }
		double y[] = fm.getParameters();
		
		// Convert y values to z on a strip
		rptrnsfm(z, y,cnrcopy);
		int ends[] = new int[2];
		for (i = 0, j = 0; i < n-1; i++) {
			if (z[i][1] != z[i+1][1]) {
				ends[j++] = i;
			}
		}
		if (z[n-1][1] != z[0][1]) {
			ends[j] = n-1;
		}
		double zs[][] = new double[n+2][2];
		for (i = 0; i <= ends[0]; i++) {
			zs[i][0] = z[i][0];
			zs[i][1] = z[i][1];
		}
		zs[ends[0]+1][0] = Double.POSITIVE_INFINITY;
		zs[ends[0]+1][1] = 0.0;
		for (i = ends[0]+1; i <= ends[1]; i++) {
			zs[i+1][0] = z[i][0];
			zs[i+1][1] = z[i][1];
		}
		zs[ends[1]+2][0] = Double.NEGATIVE_INFINITY;
		zs[ends[1]+2][1] = 0;
		for (i = ends[1]+1; i < n; i++) {
			zs[i+2][0] = z[i][0];
			zs[i+2][1] = z[i][1];
		}
	    double bs[] = new double[n+2];
	    for (i = 0; i <= ends[0]; i++) {
	    	bs[i] = betacopy[i];
	    }
	    bs[ends[0]+1] = 0;
	    for (i = ends[0]+1; i <= ends[1]; i++) {
	    	bs[i+1] = betacopy[i];
	    }
	    bs[ends[1]+2] = 0;
	    for (i = ends[1]+1; i < n; i++) {
	    	bs[i+2] = betacopy[i];
	    }
	    double qs[][] = new double[qdat.length][qdat[0].length+4];
		for (i = 0; i < qdat.length; i++) {
			for (j = 0; j <= ends[0]; j++) {
				qs[i][j] = qdat[i][j];
				qs[i][j+2+n+1] = qdat[i][j+n+1];
			}
			qs[i][ends[0]+1] = qdat[i][n];
			qs[i][ends[0]+3+n+1] = qdat[i][2*n+1];
			for (j = ends[0]+1; j <= ends[1]; j++) {
				qs[i][j+1] = qdat[i][j];
				qs[i][j+3+n+1] = qdat[i][j+n+1];
			}
			qs[i][ends[1]+2] = qdat[i][n];
			qs[i][ends[1]+4+n+1] = qdat[i][2*n+1];
			for (j = ends[1]+1; j <= n-1; j++) {
				qs[i][j+2] = qdat[i][j];
				qs[i][j+4+n+1] = qdat[i][j+n+1];
			}
			qs[i][n+2] = qdat[i][n];
			qs[i][2*n+5] = qdat[i][2*n+1];
		} // for (i = 0; i < qdat.length; i++)
	    
		// Determine multiplicative constant
		double mid[][] = new double[1][2];
		mid[0][0] = (zs[0][0] + zs[1][0])/2.0;
		mid[0][1] = (zs[0][1] + zs[1][1])/2.0;
		int sing1[] = new int[1];
		sing1[0] = 1;
		double z1[][] = new double[1][2];
		z1[0][0] = zs[1][0];
		z1[0][1] = zs[1][1];
		double I1[][] = stquad(z1, mid, sing1, zs, bs, qs);
		sing1[0] = 0;
		z1[0][0] = zs[0][0];
		z1[0][1] = zs[0][1];
		double I2[][] = stquad(z1, mid, sing1, zs, bs, qs);
		double g[] = new double[2];
		g[0] = I1[0][0] - I2[0][0];
		g[1] = I1[0][1] - I2[0][1];
		double ar = w[0][0] - w[1][0];
		double ai = w[0][1] - w[1][1];
		zdiv(ar, ai, g[0], g[1], cr, ci);
		c[0] = cr[0];
		c[1] = ci[0];
		
		// Find prevertices on the rectangle
		
		// Find corners of the rectangle
		zs = new double[z.length][2];
		for (i = 0; i < z.length; i++) {
			zs[i][0] = z[i][0];
			zs[i][1] = z[i][1];
		}
		// In rptrnsfm z[cnrcopy[0]] is never changed from its initial value of zero
		// so zs[cnrcopy[0]][0] = 0 and zs[cnrcopy[0]][1] = 0;
		// so L is real
		L[0] = Math.max(zs[cnrcopy[1]][0], zs[cnrcopy[2]][0]) - zs[cnrcopy[0]][0];
		L[1] = -zs[cnrcopy[0]][1];
		double K[] = new double[1];
		double Kp[] = new double[1];
		ellipkkp(K, Kp, L[0]);
		double rect[][] = new double[4][2];
		rect[0][0] = K[0];
		rect[1][0] = K[0];
		rect[1][1] = Kp[0];
		rect[2][0] = -K[0];
		rect[2][1] = Kp[0];
		rect[3][0] = -K[0];
		boolean l[] = new boolean[n]; // on left side
		for (i = cnrcopy[2]; i <= cnrcopy[3]; i++) {
			l[i] = true;
		}
		boolean r[] = new boolean[n]; // on right side
		for (i = cnrcopy[0]; i <= cnrcopy[1]; i++) {
			r[i] = true;
 		}
		int tllength = 0;
		int trlength = 0;
		int bllength = 0;
		int brlength = 0;
		for (i = 0; i < zs.length; i++) {
			if ((zs[i][0] > L[0]) && (zs[i][1] > 0.0)) {
				// top-left side
				tllength++;
			}
			else if ((zs[i][0] > L[0]) && (zs[i][1] == 0.0)) {
				// top-right side
				trlength++;
			}
			else if ((zs[i][0] < 0.0) && (zs[i][1] > 0.0)) {
				// bottom-left side
				bllength++;
			}
			else if ((zs[i][0] < 0.0) && (zs[i][1] == 0.0)) {
				// bottom-right side
				brlength++;
			}
		} // for (i = 0; i < zs.length; i++)
		int tl[] = new int[tllength];
		int tr[] = new int[trlength];
		int bl[] = new int[bllength];
		int br[] = new int[brlength];
		for (i = 0, j = 0, k = 0, m = 0, p = 0; i < zs.length; i++) {
			if ((zs[i][0] > L[0]) && (zs[i][1] > 0.0)) {
				// top-left side
				tl[j++] = i;
			}
			else if ((zs[i][0] > L[0]) && (zs[i][1] == 0.0)) {
				// top-right side
				tr[k++] = i;
			}
			else if ((zs[i][0] < 0.0) && (zs[i][1] > 0.0)) {
				// bottom-left side
				bl[m++] = i;
			}
			else if ((zs[i][0] < 0.0) && (zs[i][1] == 0.0)) {
				// bottom-right side
				br[p++] = i;
			}
		} // for (i = 0, j = 0, k = 0, m = 0, p = 0; i < zs.length; i++)
		
		// Initial guesses
		//z = new double[zs.length][2];
		for (i = 0; i < z.length; i++) {
			z[i][0] = 0;
			z[i][1] = 0;
		}
		// Corners
		for (i = 0; i < 4; i++) {
		    z[cnrcopy[i]][0] = rect[i][0];
		    z[cnrcopy[i]][1] = rect[i][1];
		}
		// Left and right sides are simple
		int denom = cnrcopy[3] - cnrcopy[2];
		double realDiff = (rect[3][0] - rect[2][0])/denom;
		double imagDiff = (rect[3][1] - rect[2][1])/denom;
		for (i = cnrcopy[2]; i <= cnrcopy[3]; i++) {
			z[i][0] = rect[2][0] + (i - cnrcopy[2]) * realDiff;
			z[i][1] = rect[2][1] + (i - cnrcopy[2]) * imagDiff;
		}
		denom = cnrcopy[1] - cnrcopy[0];
		realDiff = (rect[1][0] - rect[0][0])/denom;
		imagDiff = (rect[1][1] - rect[0][1])/denom;
		for (i = cnrcopy[0]; i <= cnrcopy[1]; i++) {
			z[i][0] = rect[0][0] + (i - cnrcopy[0]) * realDiff;
			z[i][1] = rect[0][1] + (i - cnrcopy[0]) * imagDiff;
		}
		// Cluster the top and bottom guesses near 0.
		double h = K[0]/20.0;
		for (i = 0; i < tl.length; i++) {
			z[tl[i]][0] = -(i+1)*h;
			z[tl[i]][1] = Kp[0];
		}
		for (i = 0; i < tr.length; i++) {
			z[tr[i]][0] = h*(tr.length - i);
			z[tr[i]][1] = Kp[0];
		}
		for (i = 0; i < bl.length; i++) {
			z[bl[i]][0] = -h * (bl.length - i);
			z[bl[i]][1] = 0.0;
		}
		for (i = 0; i < br.length; i++) {
			z[br[i]][0] = (i+1)*h;
			z[br[i]][1] = 0.0;
		}
		
		// Newton iteration on "r2strip() - zs = 0"
		double zn[][] = new double[z.length][2];
		for (i = 0; i < z.length; i++) {
			zn[i][0] = z[i][0];
			zn[i][1] = z[i][1];
		}
		int maxiter = 50;
		boolean done[] = new boolean[zn.length];
		for (i = 0; i < 4; i++) {;
			done[cnrcopy[i]] = true;
		}
		k = 0;
		boolean alldone = true;
		int numnotdone = 0;
		for (i = 0; i < done.length; i++) {
			if (!done[i]) {
				alldone = false;
				numnotdone++;
			}
		} // for (i = 0; i < done.length; i++)
		double znnotdone[][] = new double[numnotdone][2];
		double zsnotdone[][] = new double[numnotdone][2];
		boolean lr[] = new boolean[numnotdone];
		boolean tbl[] = new boolean[numnotdone];
		boolean tlnotdone;
		boolean blnotdone;
		boolean tbr[] = new boolean[numnotdone];
		boolean trnotdone;
		boolean brnotdone;
		for (i = 0, j = 0; i < done.length; i++) {
		    if (!done[i]) {
		    	znnotdone[j][0] = zn[i][0];
		    	znnotdone[j][1] = zn[i][1];
		    	zsnotdone[j][0] = zs[i][0];
		    	zsnotdone[j][1] = zs[i][1];
		    	lr[j] = r[i] || l[i];
		    	tlnotdone = (zs[i][0] > L[0]) && (zs[i][1] > 0);
		    	blnotdone = (zs[i][0] < 0) && (zs[i][1] > 0);
		    	tbl[j] = tlnotdone || blnotdone;
		    	trnotdone = (zs[i][0] > L[0]) && (zs[i][1] == 0);
		    	brnotdone = (zs[i][0] < 0) && (zs[i][1] == 0);
		    	tbr[j++] = trnotdone || brnotdone;
		    } 
		} // for (i = 0, j = 0; i < done.length; i++)
		double zcnr[][] = new double[4][2];
		for (i = 0; i < 4; i++) {
			zcnr[i][0] = z[cnrcopy[i]][0];
			zcnr[i][1] = z[cnrcopy[i]][1];
		}
		double F[][] = new double[numnotdone][2];
		double Fabs;
		double dF[][] = new double[numnotdone][2];
		double step[][] = new double[numnotdone][2];
		double znew[][] = new double[numnotdone][2];
		double xn;
		while ((!alldone) && (k < maxiter)) {
		    r2strip(F, dF, znnotdone, zcnr, L[0]);
		    for (i = 0; i < numnotdone; i++) {
		    	F[i][0] = zsnotdone[i][0] - F[i][0];
		    	F[i][1] = zsnotdone[i][1] - F[i][1];
		    }
		    	
		    	// Adjust Newton step to stay exactly on rectangle boundary
		    for (i = 0; i < numnotdone; i++) {
		    	zdiv(F[i][0], F[i][1], dF[i][0], dF[i][1], cr, ci);
		    	step[i][0] = cr[0];
		    	step[i][1] = ci[0];
		    }
		    
		    for (i = 0; i < numnotdone; i++) {
		    	if (lr[i]) {
		    	    step[i][0] = 0;	
		    	}
		    	else {
		    		step[i][1] = 0;
		    	}
		    }
		    
		    	
		    	// Newton step
		    for (i = 0; i < numnotdone; i++) {
		    	znew[i][0] = znnotdone[i][0] + step[i][0];
		    	znew[i][1] = znnotdone[i][1] + step[i][1];
		    }
		    
		    	// Keep prevertices from moving too far (past boundaries)
		    	// Left/right sides capped in Im direction
		    for (i = 0; i < numnotdone; i++) {
		        if (lr[i]) {
		        	xn = Math.min(Math.max(znew[i][1], 0), Kp[0]);
		        	znew[i][1] = xn;
		        } // if (lr[i])
		    }
		        // Top/bottom-left sides capped in Re direction
		    for (i = 0; i < numnotdone; i++) {
		        if (tbl[i]) {
		        	xn = Math.min(Math.max(znew[i][0], -K[0]), -eps);
		        	znew[i][0] = xn;
		        }
		    }
		    
		        // Top/bottom-right sides capped in Re direction
		    for (i = 0; i < numnotdone; i++) {
		        if (tbr[i]) {
		            xn = Math.min(Math.max(znew[i][0], eps), K[0]);
		            znew[i][0] = xn;
		        } 
		    }
		        
		        //  Update
		    for (i = 0, j = 0; i < done.length; i++) {
		    	if (!done[i]) {
		            znnotdone[j][0] = znew[j][0];
		            znnotdone[j][1] = znew[j][1];
		            zn[i][0] = znew[j][0];
		            zn[i][1] = znew[j][1];
		    	}
		    } // for (i = 0, j = 0; i < done.length; i++)
		    
		    
		    alldone = true;
		    double znnotdone2[][] = new double[numnotdone][2];
		    numnotdone = 0;
		    for (i = 0, j = 0, m = 0; i < done.length; i++) {
		    	if (!done[i]) {
		    		Fabs = zabs(F[j][0], F[j][1]);
		    		if (Fabs < tol) {
		    			done[i] = true;
		    		}
		    		else {
		    			numnotdone++;
		    			alldone = false;
		    			znnotdone2[m][0] = znnotdone[j][0];
		    			znnotdone2[m++][1] = znnotdone[j][1];
		    		}
		    		j++;
		    	}
		    } // for (i = 0, j = 0, m = 0; i < done.length; i++)
		    if (!alldone) {
			    znnotdone = new double[numnotdone][2];
				zsnotdone = new double[numnotdone][2];
				lr = new boolean[numnotdone];
				tbl = new boolean[numnotdone];
				tbr = new boolean[numnotdone];
				for (i = 0, j = 0; i < done.length; i++) {
				    if (!done[i]) {
				    	znnotdone[j][0] = znnotdone2[j][0];
				    	znnotdone[j][1] = znnotdone2[j][1];
				    	zsnotdone[j][0] = zs[i][0];
				    	zsnotdone[j][1] = zs[i][1];
				    	lr[j] = r[i] || l[i];
				    	tlnotdone = (zs[i][0] > L[0]) && (zs[i][1] > 0);
				    	blnotdone = (zs[i][0] < 0) && (zs[i][1] > 0);
				    	tbl[j] = tlnotdone || blnotdone;
				    	trnotdone = (zs[i][0] > L[0]) && (zs[i][1] == 0);
				    	brnotdone = (zs[i][0] < 0) && (zs[i][1] == 0);
				    	tbr[j++] = trnotdone || brnotdone;
				    } 
				} // for (i = 0, j = 0; i < done.length; i++)
				F = new double[numnotdone][2];
				dF = new double[numnotdone][2];
		    } // if (!alldone)
		    k++;
		} // while ((!alldone) && (k < maxiter))
		
		boolean warning = false;
		for (i = 0; i < F.length && !warning; i++) {
			if (zabs(F[i][0], F[i][1]) > tol) {
				warning = true;
				MipavUtil.displayWarning("Could not converge to the rectangle prevertices");
			}
		} // for (i = 0; i < F.length && !warning; i++)
		
		for (i = 0; i < z.length; i++) {
			z[i][0] = zn[i][0];
			z[i][1] = zn[i][1];
		}
		
		// Undo renumbering
		double temp[][] = new double[z.length][2];
		for (i = 0; i < z.length; i++) {
			temp[renum[i]][0] = z[i][0];
			temp[renum[i]][1] = z[i][1];
		}
		for (i = 0; i < z.length; i++) {
			z[i][0] = temp[i][0];
			z[i][1] = temp[i][1];
		}
		
		return;
    }
	
	// Select one or more vertices in a polygon
	// The polygon given by w and beta is in the source image window.
	// The user selects m vertices using the mouse. If m <= 0, m defaults to 1.
	// On exit K is a vector of indices into W.
	// scselect provides an instructional message
	public int[] scselect(double w[][], double beta[], int m,  String titl, String msg[]) {
		int i, j;
		int nearestIndex;
		double nearestDistance;
		double xDiff;
		double yDiff;
		double nearestXDiff;
		double nearestYDiff;
		double distance;
		if (m <= 0) {
			m = 1;
		}
		int K[] = new int[m];
		int n = w.length;
		for (i = 0; i < n-1; i++) {
		    if ((Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) &&
		    	(Double.isInfinite(w[i+1][0]) || Double.isInfinite(w[i+1][1]))) {
		    	MipavUtil.displayError("Infinite vertices must not be adjacent");
		    	System.exit(-1);
		    }
		}
		if ((Double.isInfinite(w[n-1][0]) || Double.isInfinite(w[n-1][1])) &&
			(Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]))) {
			MipavUtil.displayError("Infinite vertices must not be adjacent");
	    	System.exit(-1);	
		}

        if (n < m) {
            MipavUtil.displayError("Number of vertices = " + n + " less than required " + m);

            System.exit(-1);
        }
		
		if (titl != null) {
		    System.out.println(titl);
		}
		if ((msg != null) && (msg.length > 0)) {
			for (i = 0; i < msg.length; i++) {
				System.out.println(msg[i]);
			}
		}
		
		// Begin selection(s)
		accessLock.lock();
		boolean addedPoint[] = new boolean[n];
		for (i = 0; i < m;) {
			try {
			    canProcessMouseClick.await();
			}
			catch (InterruptedException e) {
				e.printStackTrace();
			}
			nearestIndex = -1;
		    nearestDistance = Double.MAX_VALUE;
		    nearestXDiff = Double.MAX_VALUE;
		    nearestYDiff = Double.MAX_VALUE;
			for (j = 0; j < n; j++) {
			    xDiff = xClick - w[i][0];
			    yDiff = yClick - w[i][1];
			    distance = Math.sqrt(xDiff*xDiff + yDiff*yDiff);
			    if (distance < nearestDistance) {
			    	nearestIndex = j;
			    	nearestDistance = distance;
			    	nearestXDiff = Math.abs(xDiff);
			    	nearestYDiff = Math.abs(yDiff);
			    }
			} // for (j = 0; j < n; j++)
			if (nearestXDiff >= 3) {
				System.out.println("Cannot add vertex " + j + " when xDiff = " + nearestXDiff + " is >= 3");
				continue;
			}
			if (nearestYDiff >= 3) {
				System.out.println("Cannot add vertex " + j + " when yDiff = " + nearestYDiff + " is >= 3");
				continue;
			}
			if (addedPoint[j]) {
				System.out.println("Cannot add vertex " + j + " twice");
				continue;
			}
			else {
			    K[i] = nearestIndex;
			    addedPoint[nearestIndex] = true;
			    i++;
			}
		} // for (i = 0; i < m; i++)
		accessLock.unlock();
		return K;
	}
	
	
	
	public void mouseClicked(MouseEvent mouseEvent) {
		 int xS, yS;
	     ViewJComponentBase vBase= (ViewJComponentBase)srcImage.getParentFrame().getComponentImage();
		try {

			xS = Math.round((mouseEvent.getX() / (vBase.getZoomX() * vBase.getResolutionX())) - 0.5f);
            yS = Math.round((mouseEvent.getY() / (vBase.getZoomY() * vBase.getResolutionY())) - 0.5f);
            xClick = mouseEvent.getX();
            yClick = mouseEvent.getY();

            if ((xS < 0) || (xS >= srcImage.getExtents()[0]) || (yS < 0) || (yS >= srcImage.getExtents()[1])) {
                return;
            }

           
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: SchwarzChristoffelMapping.mouseClicked+"
            		+ "");

            return;
        }
		
	    canProcessMouseClick.signalAll();
	}
	
	public void mousePressed(MouseEvent event) {
		
	}
	
	public void mouseReleased(MouseEvent event) {
		
	}
	
	public void mouseEntered(MouseEvent event) {
		
	}
	
	public void mouseExited(MouseEvent event) {
		
	}
	
	// Map from rectangle to strip.
	// r2strip maps from a rectangle to the strip 0 <= Im z <= 1, with the function log(sn(z|m))/pi,
	// where sn is a Jacobi elliptic function and m = exp(-2*PI*L).  The prevertices of the map 
	// (in the rectangle domain) are given by z; only the corners of the rectangle defined by z
	// are used.
	
	// The derivative of the map is returned as yprime.
	
	// Note: The functionality is not parallel to hp2disk and disk2hp.
	
	// Original MATLAB routine copyright 1998 by Toby Driscoll.
	
	// The built-in ellipj accepts only real arguments.  The standard identity is not helpful
	// when m is near zero, because (1-m) loses digits of accuracy.
	private void r2strip(double yp[][], double yprime[][], double zp[][], double z[][], double L) {
	    int i;
	    double cr[] = new double[1];
	    double ci[] = new double[1];
		double K = z[0][0];
		//double Kp = z[0][1];
		for (i = 1; i < z.length; i++) {
			if (z[i][0] > K) {
				K = z[i][0];
			}
			//if (z[i][1] > Kp) {
				//Kp = z[i][1];
			//}
		} // for (i = 1; i < z.length; i++) 
		for (i = 0; i < zp.length; i++) {
			yp[i][0] = zp[i][0];
			yp[i][1] = zp[i][1];
			yprime[i][0] = zp[i][0];
			yprime[i][1] = zp[i][1];
		}
		
		double sn[][] = new double[zp.length][2];
		double cn[][] = new double[zp.length][2];
		double dn[][] = new double[zp.length][2];
		ellipjc(sn, cn, dn, zp, L, false);
		
		// Make sure everything is in the upper half-plane (fix roundoff)
		for (i = 0; i < sn.length; i++) {
			sn[i][1] = Math.max(0, sn[i][1]);
			yp[i][0] = Math.log(zabs(sn[i][0], sn[i][1]))/Math.PI;
			yp[i][1] = Math.atan2(sn[i][1], sn[i][0])/Math.PI;
			zmlt(cn[i][0], cn[i][1], dn[i][0], dn[i][1], cr, ci);
			zdiv(cr[0], ci[0], sn[i][0], sn[i][1], cr, ci);
			yprime[i][0] = cr[0]/Math.PI;
			yprime[i][1] = ci[0]/Math.PI;
		}
		
		// Make sure everything is in the strip (roundoff could put it outside).
		for (i = 0; i < yp.length; i++) {
			yp[i][1] = Math.min(1, Math.max(0, yp[i][1]));
		}
	
	}
	
	// Jacobi elliptic functions for complex argument
	// ellipjc returns the values of the Jacobi elliptic functions evaluated at complex argument U and
	// parameter m = exp(-2*PI*L), 0 < L < Infinity.  Recall that m = k^2, where k is the elliptic modulus.
	
	// u may be a vector: L must be a scalar.  The entries of U are expected to lie within the rectangle
	// |Re u| < K, 0 < Im u < Kp, where K and Kp are returned by ellipkkp.
	
	// Original MATLAB routine copyright 1999 by Toby Driscoll.
	
	// The built-in ellipj can't handle complex arguments, and standard transformations to handle this
	// would require ellipj called with parameter 1-m.  When m < eps (or is even close), this can't be
	// done accurately.
	
	// The algorithm is the descending Landen transformation, described in L Howell's PhD thesis from MIT.
	// Additional formulas come from Gradshteyn & Ryzhik, 5th ed., and Abramowitz & Stegun.
	private void ellipjc(double sn[][], double cn[][], double dn[][], double u[][], double L, boolean flag) {
		int i, j;
		int numhigh = 0;
		int high[];
		double m;
		double sinu[] = new double[2];
		double cosu[] = new double[2];
		double sincos[] = new double[2];
		double cr[] = new double[1];
		double ci[] = new double[1];
		double realPart;
		double imagPart;
		double cosSq[] = new double[2];
		double sinSq[] = new double[2];
		double kappa;
		double sqrt1m;
		double md4;
		double md42;
		double md43;
		double md44;
		double md45;
		double md46;
		double mu;
		double v[][] = new double[u.length][2];
		double sn1[][] = new double[u.length][2];
		double cn1[][] = new double[u.length][2];
		double dn1[][] = new double[u.length][2];
		double denom[] = new double[2];
		double sn1Sq[] = new double[2];
		double ucopy[][] = new double[u.length][2];
		
		for (i = 0; i < u.length; i++) {
			ucopy[i][0] = u[i][0];
			ucopy[i][1] = u[i][1];
		}
		
	    if (!flag) {
	    	// flag = false indicates we must check for and transform u in the upper half of the rectangle.
	    	double K[] = new double[1];
	    	double Kp[] = new double[1];
	    	ellipkkp(K, Kp, L);
	    	for (i = 0; i < ucopy.length; i++) {
	    		if (ucopy[i][1] > Kp[0]/2.0) {
	    			numhigh++;
	    		}
	    	} // for (i = 0; i < ucopy.length; i++)
	    	high = new int[numhigh];
	    	for (i = 0, j = 0; i < ucopy.length; i++) {
	    		if (ucopy[i][1] > Kp[0]/2.0) {
	    			ucopy[i][0] = -ucopy[i][0];
	    			ucopy[i][1] = Kp[0] - ucopy[i][1];
	    			high[j++] = i;
	    		}
	    	} // for (i = 0, j = 0; i < ucopy.length; i++)
	    	m = Math.exp(-2.0*Math.PI*L);
	    } // if (!flag)
	    else {
	    	// Recursive call - L is actually m.
	    	high = null;
	    	m = L;
	    }
	    
	    if (m < 4.0 * eps) {
	        for (i = 0; i < u.length; i++) {
	            sinu[0] = Math.sin(ucopy[i][0])*Math.cosh(ucopy[i][1]);
	            sinu[1] = Math.cos(ucopy[i][0])*Math.sinh(ucopy[i][1]);
	            cosu[0] = Math.cos(ucopy[i][0])*Math.cosh(ucopy[i][1]);
	            cosu[1] = -Math.sin(ucopy[i][0])*Math.sinh(ucopy[i][1]);
	            zmlt(sinu[0], sinu[1], cosu[0], cosu[1], cr, ci);
	            sincos[0] = cr[0];
	            sincos[1] = ci[0];
	            realPart = sincos[0] - ucopy[i][0];
	            imagPart = sincos[1] - ucopy[i][1];
	            zmlt(realPart, imagPart, cosu[0], cosu[1], cr, ci);
	            sn[i][0] = sinu[0] + (m/4.0)*cr[0];
	            sn[i][1] = sinu[1] + (m/4.0)*ci[0];
	            realPart = -sincos[0] + ucopy[i][0];
	            imagPart = -sincos[1] + ucopy[i][1];
	            zmlt(realPart, imagPart, sinu[0], sinu[1], cr, ci);
	            cn[i][0] = cosu[0] + (m/4.0)*cr[0];
	            cn[i][1] = cosu[1] + (m/4.0)*ci[0];
	            zmlt(cosu[0], cosu[1], cosu[0], cosu[1], cr, ci);
	            cosSq[0] = cr[0];
	            cosSq[1] = ci[0];
	            zmlt(sinu[0], sinu[1], sinu[0], sinu[1], cr, ci);
	            sinSq[0] = cr[0];
	            sinSq[1] = ci[0];
	            realPart = cosSq[0] - sinSq[0] - 1;
	            imagPart = cosSq[1] - sinSq[1];
	            dn[i][0] = 1.0 + (m/4.0)*realPart;
	            dn[i][1] = (m/4.0)*imagPart;
	        } // for (i = 0; i < u.length; i++)
	    } // if (m < 4.0 * eps)
	    else {
	    	if (m > 1.0e-3) {
	    		sqrt1m = Math.sqrt(1.0 - m);
	    	    kappa = (1 - sqrt1m)/(1.0 + sqrt1m);	
	    	}
	    	else {
	    	    md4 = (m/4.0);
	    	    md42 = md4 * md4;
	    	    md43 = md42 * md4;
	    	    md44 = md43 * md4;
	    	    md45 = md44 * md4;
	    	    md46 = md45 * md4;
	    	    kappa = 132*md46 + 42*md45 + 14*md44 + 5*md43 + 2*md42 + md4;
	    	}
	    	mu = kappa * kappa;
	    	for (i = 0; i < ucopy.length; i++) {
	    	    v[i][0] = ucopy[i][0]/(1.0 + kappa);
	    	    v[i][1] = ucopy[i][1]/(1.0 + kappa);
	    	} // for (i = 0; i < ucopy.length; i++)
	    	ellipjc(sn1, cn1, dn1, v, mu, true);
	    	for (i = 0; i < ucopy.length; i++) {
	    	    zmlt(sn1[i][0], sn1[i][1], sn1[i][0], sn1[i][1], cr, ci);
	    	    sn1Sq[0] = cr[0];
	    	    sn1Sq[1] = ci[0];
	    	    denom[0] = 1.0 + kappa*sn1Sq[0];
	    	    denom[1] = kappa*sn1Sq[1];
	    	    zdiv((1.0 + kappa)*sn1[i][0], (1.0 + kappa)*sn1[i][1], denom[0], denom[1], cr, ci);
	    	    sn[i][0] = cr[0];
	    	    sn[i][1] = ci[0];
	    	    zmlt(cn1[i][0], cn1[i][1],dn1[i][0], dn1[i][1], cr, ci);
	    	    zdiv(cr[0], ci[0], denom[0], denom[1], cr, ci);
	    	    cn[i][0] = cr[0];
	    	    cn[i][1] = ci[0];
	    	    zdiv((1.0 - kappa*sn1Sq[0]), -kappa*sn1Sq[1], denom[0], denom[1], cr, ci);
	    	    dn[i][0] = cr[0];
	    	    dn[i][1] = ci[0];
	    	} // for (i = 0; i < ucopy.length; i++)
	    } // else
	    
	    if (numhigh > 0) {
	    	double snh[] = new double[2];
			double cnh[] = new double[2];
			double dnh[] = new double[2];
		    double sqrtm = Math.sqrt(m);
		    for (i = 0; i < numhigh; i++) {
		        snh[0] = sn[high[i]][0];
		        snh[1] = sn[high[i]][1];
		        cnh[0] = cn[high[i]][0];
		        cnh[1] = cn[high[i]][1];
		        dnh[0] = dn[high[i]][0];
		        dnh[1] = dn[high[i]][1];
		        zdiv(-1.0, 0.0, sqrtm*snh[0], sqrtm*snh[1], cr, ci);
		        sn[high[i]][0] = cr[0];
		        sn[high[i]][1] = ci[0];
		        zdiv(-dnh[1], dnh[0], sqrtm*snh[0], sqrtm*snh[1], cr, ci);
		        cn[high[i]][0] = cr[0];
		        cn[high[i]][1] = ci[0];
		        zdiv(-cnh[1], cnh[0], snh[0], snh[1], cr, ci);
		        dn[high[i]][0] = cr[0];
		        dn[high[i]][1] = ci[0];
		    } // for (i = 0; i < numhigh; i++) 
	    } // if (numhigh > 0)
	}
	
	// ellipkkp is the complete elliptic integral of the first kind, with complement.
	// K[0] returns the value of the complete elliptic integral of the first kind, evaluated at
	// M = exp(-2*PI*L), 0 < L < Infinity/
	// Kp[0] returns the result for the complementary parameter 1-m, which is useful when m < eps.
	// Even when m < 1.0e-6, the built-in ellipke can lose digits of accuracy for Kp.
	// Recall that the elliptic modulus k is related to the parameter m by m = k^2.
	// Original MATLAB routine copyright 1999 by Toby Driscoll.
	
	// elipkkp uses the method of the arithmetic-geometric mean described in 17.6 of M. Abramowitz
	// and I. A. Stegun, "Handbook of Mathematical Functions," Dover, 1965.  Same method as in 
	// ellipke, only interchanging 1 and1-m to find Kp.
	private void ellipkkp(double K[], double Kp[], double L) {
	    double m;
	    double a0;
	    double b0;
	    double s0;
	    int i1;
	    double mm;
	    double a1 = 0.0;
	    double b1;
	    double c1;
	    double w1;
	    // When m = exp(-2*PI*L) is extremely small, use O(m) approximations.
	    if (L > 10.0) {
	    	K[0] = Math.PI/2.0;
	    	Kp[0] = Math.PI*L + Math.log(4.0);
	    	return;
	    } // if (L > 10.0)
	    
	    m = Math.exp(-2.0*Math.PI*L);
	    a0 = 1;
	    b0 = Math.sqrt(1.0-m);
	    s0 = m;
	    i1 = 0;
	    mm = 1.0;
	    while (mm > eps) {
	        a1 = (a0 + b0)/2.0;
	        b1 = Math.sqrt(a0*b0);
	        c1 = (a0-b0)/2.0;
	        i1 = i1 + 1;
	        w1 = Math.pow(2, i1)*c1*c1;
	        mm = w1;
	        s0 = s0 + w1;
	        a0 = a1;
	        b0 = b1;
	    } // while (mm > eps)
	    
	    K[0] = Math.PI/(2.0 * a1);
	    if (m == 1.0) {
	    	K[0] = K[0] * Double.POSITIVE_INFINITY;
	    }
	    
	    a0 = 1.0;
	    b0 = Math.sqrt(m);
	    s0 = 1.0 - m;
	    i1 = 0;
	    mm = 1.0;
	    while (mm > eps) {
	    	a1 = (a0 + b0)/2.0;
	    	b1 = Math.sqrt(a0*b0);
	    	c1 = (a0-b0)/2.0;
	    	i1 = i1 + 1;
	    	w1 = Math.pow(2, i1)*c1*c1;
	    	mm = w1;
	    	s0 = s0 + w1;
	    	a0 = a1;
	    	b0 = b1;
	    } // while (mm > eps)
	    
	    Kp[0] = Math.PI/(2.0 * a1);
	    if (m == 0) {
	    	Kp[0] = Kp[0] * Double.POSITIVE_INFINITY;
	    }
 	}
	
	public void printExitStatus(int exitStatus) {
		if (exitStatus == -1) {
            System.err.println("Abnormal termination because m < n or n <= 0 or m <= 0 or mdc < m or mdw < n*n + 5*n + 3*m + 6 or");
            System.err.println("maxit <= 0 or epsrel < 0 or epsabs < 0 or epsx < 0 or invalid starting point on entry");
        } 
        else if (exitStatus == -2) {
        	System.err.println("Abnormal termination because the number of iterations has exceeded the maximum allowed iterations");
        }
        else if (exitStatus == -3) {
        	System.err.println("Abnormal termination because the Hessian emanating from the 2nd order method is not positive definite");
        }
        else if (exitStatus == -4) {
        	System.err.println("Abnormal termination because the algorithm would like to use 2nd derivatives but is not allowed to do that");
        }
        else if (exitStatus == -5) {
        	System.err.println("Abnormal termination because an undamped step with Newtons method is a failure");
        }
        else if (exitStatus == -6) {
        	System.err.println("Abnormal termination because the latest search direction computed using subspace minimization");
        	System.err.println("was not a descent direction (probably caused by a wrongly computed Jacobian)");
        }
        else if (exitStatus == -7) {
        	System.err.println("Abnormal termination because there is only one feasible point");
        	System.err.println("namely X(I) = BL(I) = BU(I), I = 1,2,...,N");
        }
        else if (exitStatus == -8) {
        	System.err.println("Abnormal termination due to driver error");
        }
        else {
        	System.err.println("Exit status = " + exitStatus);
        }
	}
	
	class depfun extends NLConstrainedEngine {
		int n;
		double beta[];
		double nmlen[];
		double qdat[][];
		
		public depfun (double y0[], int n, double beta[], double nmlen[], double qdat[][]) {
    		// nPoints, params
    		super(y0.length, y0.length);
    		this.n = n;
    		this.beta = beta;
    		this.nmlen = nmlen;
    		this.qdat = qdat;
    		
    		bounds = 0; // bounds = 0 means unconstrained

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < y0.length; i++) {
				gues[i] = y0[i];
			}
    	}
    	
    	/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}
		
		/**
		 * Display results of displaying exponential fitting parameters.
		 */
		public void dumpResults() {
			Preferences
					.debug(" ******* Fit Elsunc Schwarz-Christoffel deparam ********* \n\n",
							Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters)
					+ "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared())
					+ "\n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < a.length; i++) {
			Preferences.debug("a"+i+" " + String.valueOf(a[i]) + "\n",
					Preferences.DEBUG_ALGORITHM);
			}
		}
    	
    	public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
    		int ctrl;
    		int i, j;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		double ints[][];
    		double F[] = null;
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					
					// Convert a (unconstrained variables) to z (prevertices)
					double cumprod[] = new double[a.length+1];
					cumprod[0] = 1;
					for (i = 1; i <= a.length; i++) {
						cumprod[i] = cumprod[i-1]*Math.exp(-a[i-1]);
					}
					double cs[] = new double[a.length+1];
					cs[0] = cumprod[0];
					for (i = 1; i <= a.length; i++) {
						cs[i] = cs[i-1] + cumprod[i];
					}
					double theta[] = new double[n-1];
					for (i = 0; i < n-1; i++) {
						theta[i] = 2.0*Math.PI*cs[i]/cs[n-1];
					}
					z = new double[n][2];
					for (i = 0; i < n-1; i++) {
						z[i][0] = Math.cos(theta[i]);
						z[i][1] = Math.sin(theta[i]);
					}
						
					z[n-1][0] = 1;
					z[n-1][1] = 0;
					
					// Compute the integrals
					double mid[][] = new double[n-2][2];
					for (i = 0; i < n-2; i++) {
						double ang = (theta[i] + theta[i+1])/2.0;
						mid[i][0] = Math.cos(ang);
						mid[i][1] = Math.sin(ang);
					}
					
					// We an use the same quadrature as for the interior map, because the
					// absolute value of the integrand on the unit circle is not affected
					// by the z^{-2} term.
					double z1[][] = new double[n-2][2];
					int sing1[] = new int[n-2];
					for (i = 0; i < n-2; i++) {
						z1[i][0] = z[i][0];
						z1[i][1] = z[i][1];
						sing1[i] = i;
					}
					I1 = dabsquad(z1, mid, sing1, z, beta, qdat);
					for (i = 0; i < n-2; i++) {
						z1[i][0] = z[i+1][0];
						z1[i][1] = z[i+1][1];
						sing1[i] = i+1;
					}
					I2 = dabsquad(z1, mid, sing1, z, beta, qdat);
					ints = new double[n-2][2];
					int numintszero = 0;
					for (i = 0; i < n-2; i++) {
						ints[i][0] = I1[i][0] + I2[i][0];
						ints[i][1] = I1[i][1] + I2[i][1];
						if ((ints[i][0] == 0) && (ints[i][1] == 0)) {
							numintszero++;
						}
					}
					
					if (numintszero > 0) {
						MipavUtil.displayWarning("Singularities were too crowded in practice.  Severe crowding.");
					}
					
					// Compute equation residual values.
					if (n > 3) {
					    F = new double[n-3];
					    for (i = 0; i < n-3; i++) {
					    	F[i] = zabs(ints[i+1][0], ints[i+1][1])/zabs(ints[0][0], ints[0][1]) - nmlen[i];
					    }
					} // if (n > 3)
					
					// Compute residue
					double sumr = 0.0;
					double sumi = 0.0;
					for (i = 0; i < n; i++) {
						zdiv(beta[i], 0, z[i][0], z[i][1], cr, ci);
						sumr += cr[0];
						sumi += ci[0];
					}
					zdiv(-sumr, -sumi, ints[0][0], ints[0][1], cr, ci);
					j = 0;
					if (F != null) {
						for (i = 0; i < F.length; i++) {
							residuals[j++] = F[i];
						}
					}
					residuals[j++] = cr[0];
					residuals[j] = ci[0];
				} // if ((ctrl == -1) || (ctrl == 1))

				// Calculate the Jacobian numerically
				else if (ctrl == 2) {
					ctrlMat[0] = 0;
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n",
						Preferences.DEBUG_ALGORITHM);
			}

			return;
    		
    	}
	}
	
	class dpfun extends NLConstrainedEngine {
		int n;
		double beta[];
		double nmlen[];
		int left[];
		int right[];
		boolean cmplx[];
		double qdat[][];
		
    	public dpfun (double y0[], int n, double beta[], double nmlen[], int left[],
    			int right[], boolean cmplx[], double qdat[][]) {
    		// nPoints, params
    		super(y0.length, y0.length);
    		this.n = n;
    		this.beta = beta;
    		this.nmlen = nmlen;
    		this.left = left;
    		this.right = right;
    		this.cmplx = cmplx;
    		this.qdat = qdat;
    		
    		bounds = 0; // bounds = 0 means unconstrained

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < y0.length; i++) {
				gues[i] = y0[i];
			}
    	}
    	
    	/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}
		
		/**
		 * Display results of displaying exponential fitting parameters.
		 */
		public void dumpResults() {
			Preferences
					.debug(" ******* Fit Elsunc Schwarz-Christoffel dparam ********* \n\n",
							Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters)
					+ "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared())
					+ "\n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < a.length; i++) {
			Preferences.debug("a"+i+" " + String.valueOf(a[i]) + "\n",
					Preferences.DEBUG_ALGORITHM);
			}
		}
    	
    	public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
    		int ctrl;
    		int i, j;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		double intsnotcmplx[][];
    		double intscmplx[][] = null;
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					
					// Convert a values to z (prevertices)
					double cumprod[] = new double[a.length+1];
					cumprod[0] = 1;
					for (i = 1; i <= a.length; i++) {
						cumprod[i] = cumprod[i-1]*Math.exp(-a[i-1]);
					}
					double cs[] = new double[a.length+1];
					cs[0] = cumprod[0];
					for (i = 1; i <= a.length; i++) {
						cs[i] = cs[i-1] + cumprod[i];
					}
					double theta[] = new double[n-3];
					for (i = 0; i < n-3; i++) {
						theta[i] = Math.PI*cs[i]/cs[cs.length-1];
					}
					z = new double[n][2];
					for (i = 0; i < n-3; i++) {
						z[i][0] = Math.cos(theta[i]);
						z[i][1] = Math.sin(theta[i]);
					}
					z[n-3][0] = -1;
					z[n-3][1] = 0;
					z[n-2][0] = 0;
					z[n-2][1] = -1;					
					z[n-1][0] = 1;
					z[n-1][1] = 0;
					
					// Compute the integrals
					double zleft[][] = new double[left.length][2];
					for (i = 0; i < left.length; i++) {
						zleft[i][0] = z[left[i]][0];
						zleft[i][1] = z[left[i]][1];	
					}
					double zright[][] = new double[right.length][2];
					for (i = 0; i < right.length; i++) {
						zright[i][0] = z[right[i]][0];
						zright[i][1] = z[right[i]][1];	
					}
					double angl[] = new double[left.length];
					for (i = 0; i < left.length; i++) {
						angl[i] = Math.atan2(zleft[i][1], zleft[i][0]);
					}
					double mid[][] = new double[left.length][2];
				    for (i = 0; i < left.length; i++) {
				    	zdiv(zright[i][0], zright[i][1], zleft[i][0], zleft[i][1], cr, ci);
				    	double ang = Math.atan2(ci[0],  cr[0]) + 2.0*Math.PI;
				    	double div = ang/(2.0*Math.PI);
				    	double fix;
				    	if (div >= 0) {
				    		fix = Math.floor(div);
				    	}
				    	else {
				    		fix = Math.ceil(div);
				    	}
				    	double rem = ang - 2.0*Math.PI*fix;
				    	double expvar = (angl[i] + rem/2.0);
				    	mid[i][0] = Math.cos(expvar);
				    	mid[i][1] = Math.sin(expvar);
				    }
				    // For integrals between nonadjacent singularities, choose 0
				    // as intermediate integration point.
				    int numcmplx = 0;
				    for (i = 0; i < cmplx.length; i++) {
				        if (cmplx[i]) {
				        	numcmplx++;
				        	mid[i][0] = 0;
				        	mid[i][1] = 0;
				        }
				    } // for (i = 0; i < cmplx.length; i++)
				    if (numcmplx > 0) {
				        cmplx[0] = true;	
				    }
				    double ints[][] = new double[left.length][2];
				    for (i = 0; i < left.length; i++) {
				    	ints[i][0] = Double.NaN;
				    }
				    numcmplx = 0;
				    for (i = 0; i < cmplx.length; i++) {
				        if (cmplx[i]) {
				        	numcmplx++;
				        }
				    } // for (i = 0; i < cmplx.length; i++)
				    int numnotcmplx = cmplx.length - numcmplx;
				    int numintszero = 0;
				    if (numnotcmplx > 0) {
					    double zleftnotcmplx[][] = new double[numnotcmplx][2];
					    double midnotcmplx[][] = new double[numnotcmplx][2];
					    int leftnotcmplx[] = new int[numnotcmplx];
					    double zrightnotcmplx[][] = new double[numnotcmplx][2];
					    int rightnotcmplx[] = new int[numnotcmplx];
					    for (i = 0, j = 0; i < cmplx.length; i++) {
					        if (!cmplx[i]) {
					        	zleftnotcmplx[j][0] = zleft[i][0];
					        	zleftnotcmplx[j][1] = zleft[i][1];
					        	midnotcmplx[j][0] = mid[i][0];
					        	midnotcmplx[j][1] = mid[i][1];
					        	leftnotcmplx[j] = left[i];
					        	zrightnotcmplx[j][0] = zright[i][0];
					        	zrightnotcmplx[j][1] = zright[i][1];
					        	rightnotcmplx[j++] = right[i];
					        }
					    } // for (i = 0, j = 0; i < cmplx.length; i++)
					    I1 = dabsquad(zleftnotcmplx, midnotcmplx, leftnotcmplx, z, beta, qdat);
					    I2 = dabsquad(zrightnotcmplx, midnotcmplx, rightnotcmplx, z, beta, qdat);
					    intsnotcmplx = new double[I1.length][I1[0].length];
					    for (i = 0, j = 0; i < cmplx.length; i++) {
					    	if (!cmplx[i]) {
						        intsnotcmplx[j][0] = I1[j][0] + I2[j][0];
						        intsnotcmplx[j][1] = I1[j][1] + I2[j][1];
						        ints[i][0] = intsnotcmplx[j][0];
						        ints[i][1] = intsnotcmplx[j++][1];
						        if ((ints[i][0] == 0) && (ints[i][1] == 0)) {
						        	numintszero++;
						        }
					    	} 
					    }
				    } // if (numnotcmplx > 0)
				    if (numcmplx > 0) {
					    double zleftcmplx[][] = new double[numcmplx][2];
					    double midcmplx[][] = new double[numcmplx][2];
					    int leftcmplx[] = new int[numcmplx];
					    double zrightcmplx[][] = new double[numcmplx][2];
					    int rightcmplx[] = new int[numcmplx];
					    for (i = 0, j = 0; i < cmplx.length; i++) {
					        if (cmplx[i]) {
					        	zleftcmplx[j][0] = zleft[i][0];
					        	zleftcmplx[j][1] = zleft[i][1];
					        	midcmplx[j][0] = mid[i][0];
					        	midcmplx[j][1] = mid[i][1];
					        	leftcmplx[j] = left[i];
					        	zrightcmplx[j][0] = zright[i][0];
					        	zrightcmplx[j][1] = zright[i][1];
					        	rightcmplx[j++] = right[i];
					        }
					    } // for (i = 0, j = 0; i < cmplx.length; i++)
					    I1 = dquad(zleftcmplx, midcmplx, leftcmplx, z, beta, qdat);
					    I2 = dquad(zrightcmplx, midcmplx, rightcmplx, z, beta, qdat);
					    intscmplx = new double[I1.length][I1[0].length];
					    
					    for (i = 0, j = 0; i < cmplx.length; i++) {
					    	if (cmplx[i]) {
						        intscmplx[j][0] = I1[j][0] - I2[j][0];
						        intscmplx[j][1] = I1[j][1] - I2[j][1];
						        ints[i][0] = intscmplx[j][0];
						        ints[i][1] = intscmplx[j++][1];
						        if ((ints[i][0] == 0) && (ints[i][1] == 0)) {
						        	numintszero++;
						        }
					    	} 
					    } // for (i = 0, j = 0; i < cmplx.length; i++)
				    } // if (numcmplx > 0)
				    if (numintszero > 0) {
				    	// Singularities were too crowded in practice
				    	MipavUtil.displayWarning("Severe crowding");
				    }
				    
				    // Compute nonlinear equation residual values.
				    cmplx[0] = false;
				    numcmplx = 0;
				    numnotcmplx = 0;
				    for (i = 0; i < cmplx.length; i++) {
				        if (cmplx[i]) {
				        	numcmplx++;
				        }
				        else {
				        	numnotcmplx++;
				        }
				    }
				    double F1[] = new double[numnotcmplx-1];
				    double absints0 = zabs(ints[0][0], ints[0][1]);
				    for (i = 1, j = 0; i < cmplx.length; i++) {
				    	if (!cmplx[i]) {
				            F1[j++] = ints[i][0]/absints0;
				    	}
				    } // for (i = 1, j = 0; i < cmplx.length; i++) 
				    double F2[][] = new double[numcmplx][2];
				    for (i = 1, j = 0; i < cmplx.length; i++) {
				    	if (cmplx[i]) {
				    		zdiv(ints[i][0], ints[i][1], ints[0][0], ints[0][1], cr, ci);
				    		F2[j][0] = cr[0];
				    		F2[j++][1] = ci[0];
				    	}
				    }
				    double F[] = new double[F1.length + 2*F2.length];
				    for (i = 0; i < F1.length; i++) {
				    	F[i] = F1[i];
				    }
				    for (i = 0; i < F2.length; i++) {
				    	F[F1.length + i] = F2[i][0];
				    	F[F1.length + F2.length + i] = F2[i][1];
				    }
				    for (i = 0; i < F.length; i++) {
				    	residuals[i] = F[i] - nmlen[i];
				    }
				} // if ((ctrl == -1) || (ctrl == 1))

				// Calculate the Jacobian numerically
				else if (ctrl == 2) {
					ctrlMat[0] = 0;
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n",
						Preferences.DEBUG_ALGORITHM);
			}

			return;
    		
    	}
    }
	
	private double[][] dequad(double z1[][], double z2[][], int sing1[], double z[][],
			double beta[], double qdat[][]) {
		// Numerical quadrature for the exterior map.
		// z1, z2 are vectors of left and right endpoints.  sing1 is a vector of
		// integer indices which label singularities in z1.  So if sking1[5] = 3,
		// then z1[5] = z[3].  A -1 means no singularity.  z is the vector of
		// prevertices (all singularities except the origin); beta is the vector
		// of associated turning angles.  qdat is quadrature data from scqdata.
		
		// dequad integrates from a possible singularity at the left end to a
		// regular point at the right.  If both endpoints are singularities, you
		// must break the integral into two pieces and make two calls.
		
		// The intergal is subdivided, if necessary, so that no singularity lies
		// closer to the left endpoint than 1/2 the length of the integration
		// (sub)interval.  But the singularity at the origin is NOT accounted
		// for in this decision.
		
		// Original MATLAB dequad routine copyright 1998 by Toby Driscoll.
		
		int i,j,k,p;
		double num;
		double za[] = new double[2];
		double zb[] = new double[2];
		double zr[] = new double[2];
		double zl[] = new double[2];
		int ind;
		double denom;
		int nqpts = qdat.length;
		int n = z.length;
		double nd[][] = new double[nqpts][2];
		double wt[][] = new double[nqpts][2];
		double bigz[][][] = new double[n][nqpts][2];
		double terms[][][] = new double[n][nqpts][2];
		double terms2[][][] = new double[n+1][nqpts][2];
		double cr[] = new double[1];
		double ci[] = new double[1];
		for (i = 0; i < n; i++) {
			for (j = 0; j < nqpts; j++) {
			    bigz[i][j][0] = z[i][0];
			    bigz[i][j][1] = z[i][1];
			}
		}
		double beta2[] = new double[beta.length+1];
		for (i = 0; i < beta.length; i++) {
			beta2[i] = beta[i];
		}
		beta2[beta.length] = -2.0;
		double bigbeta[][] = new double[beta2.length][nqpts];
		for (i = 0; i < beta2.length; i++) {
			for (j = 0; j < nqpts; j++) {
				bigbeta[i][j] = beta2[i];
			}
		}
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		}
		
		double I[][] = new double[z1.length][2];
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numnontriv++;
			}
		}
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				nontriv[j++] = i;
			}
		}
		for (p = 0; p < numnontriv; p++) {
		    k = nontriv[p];
		    za[0] = z1[k][0];
		    za[1] = z1[k][1];
		    zb[0] = z2[k][0];
		    zb[1] = z2[k][1];
		    int sng = sing1[k];
		    // Allowable integration step, based on nearest singularity.
		    double minNum = Double.MAX_VALUE;
		    for (i = 0; i <= sng-1; i++) {
		    	num = zabs(z[i][0] - za[0], z[i][1] - za[1]);
		    	if (num < minNum) {
		    		minNum = num;
		    	}
		    }
		    for (i = sng+1; i < n; i++) {
		    	num = zabs(z[i][0] - za[0], z[i][1] - za[1]);
		    	if (num < minNum) {
		    		minNum = num;
		    	}	
		    }
		    double dist = Math.min(1.0, 2.0*minNum/zabs(zb[0]-za[0], zb[1]-za[1]));
		    zr[0] = za[0] + dist * (zb[0] - za[0]);
		    zr[1] = za[1] + dist * (zb[1] - za[1]);
		    // Adjust Gauss-Jacobi nodes and weights to interval.
		    if (sng == -1) {
		        ind = n;	
		    }
		    else {
		    	ind = sng;
		    }
		    for (i = 0; i < nqpts; i++) {
		    	nd[i][0] = ((zr[0]-za[0])*qdat[i][ind] + zr[0] + za[0])/2.0; // nodes
		    	nd[i][1] = ((zr[1] - za[1])*qdat[i][ind] + zr[1] + za[1])/2.0;
		    	wt[i][0] = ((zr[0]-za[0])/2.0)*qdat[i][ind+n+1]; // weights
		    	wt[i][1] = ((zr[1]-za[1])/2.0)*qdat[i][ind+n+1];
		    }
		    int numtermszero = 0;
		    for (i = 0; i < n; i++) {
		    	for (j = 0; j < nqpts; j++) {
		    		zdiv(nd[j][0], nd[j][1], bigz[i][j][0], bigz[i][j][1], cr, ci);
		    		terms[i][j][0] = 1.0 - cr[0];
		    		terms[i][j][1] = -ci[0];
		    		if ((terms[i][j][0] == 0.0) && (terms[i][j][1] == 0.0)) {
		    			numtermszero++;
		    		}
		    	}
		    } // for (i = 0; i < n; i++)
		    if (numtermszero > 0) {
		    	// Endpoints are practically coincident
		    	I[k][0] = 0.0;
		    	I[k][1] = 0.0;
		    }
		    else {
		    	for (i = 0; i < n; i++) {
		    		for (j = 0; j < nqpts; j++) {
		    			terms2[i][j][0] = terms[i][j][0];
		    			terms2[i][j][1] = terms[i][j][1];
		    		}
		    	}
		    	for (j = 0; j < nqpts; j++) {
		    		terms2[n][j][0] = nd[j][0];
		    		terms2[n][j][1] = nd[j][1];
		    	}
		    	// Use Gauss-Jacobi on first subinterval, if necessary.
		    	if (sng >= 0) {
		    		for (i = 0; i < nqpts; i++) {
		    		    denom = zabs(terms2[sng][i][0], terms2[sng][i][1]);
		    		    terms2[sng][i][0] = terms2[sng][i][0]/denom;
		    		    terms2[sng][i][1] = terms2[sng][i][1]/denom;
		    		}
		    		double base = zabs(zr[0]-za[0],zr[1]-za[1])/2.0;
		    		double val = Math.pow(base, beta2[sng]);
		    		for (i = 0; i < nqpts; i++) {
		    			wt[i][0] = wt[i][0] * val;
		    			wt[i][1] = wt[i][1] * val;
		    		}
		    	} // if (sng >= 0)
		    	for (j = 0; j < nqpts; j++) {
		    		double sumr = 0.0;
		    		double sumi = 0.0;
		    	    for (i = 0; i < n+1; i++) {
		    	    	double logr = Math.log(zabs(terms2[i][j][0],terms2[i][j][1]));
		    	    	double logi = Math.atan2(terms2[i][j][1], terms2[i][j][0]);
		    	    	double prodr = logr * bigbeta[i][j];
		    	    	double prodi = logi * bigbeta[i][j];
		    	    	sumr += prodr;
		    	    	sumi += prodi;
		    	    }
		    	    double expterm = Math.exp(sumr);
		    	    double realexp = expterm * Math.cos(sumi);
		    	    double imagexp = expterm * Math.sin(sumi);
		    	    zmlt(realexp, imagexp, wt[j][0], wt[j][1], cr, ci);
		    	    I[k][0] += cr[0];
		    	    I[k][1] += ci[0];
		    	}
		    	while (dist < 1) {
		    		// Do regular Gaussian quad on other subintervals
		    		zl[0] = zr[0];
		    		zl[1] = zr[1];
		    		minNum = Double.MAX_VALUE;
		    		for (i = 0; i < n; i++) {
		    			num = zabs(z[i][0] - zl[0], z[i][1] - zl[1]);
		    			if (num < minNum) {
		    				minNum = num;
		    			}
		    		}
		    		dist = Math.min(1.0, 2.0*minNum/zabs(zl[0]-zb[0],zl[1]-zb[1]));
		    		zr[0] = zl[0] + dist * (zb[0] - zl[0]);
		    		zr[1] = zl[1] + dist * (zb[1] - zl[1]);
		    		for (i = 0; i < nqpts; i++) {
				    	nd[i][0] = ((zr[0]-zl[0])*qdat[i][n] + zr[0] + zl[0])/2.0; // nodes
				    	nd[i][1] = ((zr[1] - zl[1])*qdat[i][n] + zr[1] + zl[1])/2.0;
				    	wt[i][0] = ((zr[0]-zl[0])/2.0)*qdat[i][2*n+1]; // weights
				    	wt[i][1] = ((zr[1]-zl[1])/2.0)*qdat[i][2*n+1];
				    }
		    		for (i = 0; i < n; i++) {
				    	for (j = 0; j < nqpts; j++) {
				    		zdiv(nd[j][0], nd[j][1], bigz[i][j][0], bigz[i][j][1], cr, ci);
				    		terms[i][j][0] = 1.0 - cr[0];
				    		terms[i][j][1] = -ci[0];
				    	}
				    } // for (i = 0; i < n; i++)
		    		for (i = 0; i < n; i++) {
			    		for (j = 0; j < nqpts; j++) {
			    			terms2[i][j][0] = terms[i][j][0];
			    			terms2[i][j][1] = terms[i][j][1];
			    		}
			    	}
			    	for (j = 0; j < nqpts; j++) {
			    		terms2[n][j][0] = nd[j][0];
			    		terms2[n][j][1] = nd[j][1];
			    	}
			    	for (j = 0; j < nqpts; j++) {
			    		double sumr = 0.0;
			    		double sumi = 0.0;
			    	    for (i = 0; i < n+1; i++) {
			    	    	double logr = Math.log(zabs(terms2[i][j][0],terms2[i][j][1]));
			    	    	double logi = Math.atan2(terms2[i][j][1], terms2[i][j][0]);
			    	    	double prodr = logr * bigbeta[i][j];
			    	    	double prodi = logi * bigbeta[i][j];
			    	    	sumr += prodr;
			    	    	sumi += prodi;
			    	    }
			    	    double expterm = Math.exp(sumr);
			    	    double realexp = expterm * Math.cos(sumi);
			    	    double imagexp = expterm * Math.sin(sumi);
			    	    zmlt(realexp, imagexp, wt[j][0], wt[j][1], cr, ci);
			    	    I[k][0] += cr[0];
			    	    I[k][1] += ci[0];
			    	}
		    	} // while (dist < 1)
		    } // else
		} // for (p = 0; p < numnontriv; p++)
        return I;				
	}
	
	private double[][] dquad(double z1[][], double z2[][], int sing1[], double z[][],
			double beta[], double qdat[][]) {
		// Numerical quadrature for the disk map.
		// dquad performs the integration for the SC disk formula.  z1 and z2
		// are vectors of left and right endpoints.  If z1[k] = z[m], then 
		// sing1[k] should have the value m; otherwise, sing1[k] should be -1.
		
		// z and beta are prevertices and turning angles for the SC map.  qdat
		// is a matrix of quadrature data (see scqdata).
		
		// The integration is adaptive in the sense that members of z (with nonzero
		// beta) that are close to the left endpoints cause subdivision.  This is
		// NOT true of the singularities close to the right end.
		
		// Original MATLAB routine copyright 1998-2001 by Toby Driscoll.
		
		// z1,z2 are vectors of left and right endpoints.  sing1 is a vector of 
		// indices which label the singularities in z1.  So if sing1[5] = 3,
		// then z1[5] = z[3].  A -1 means no singularity.  z is the vector of
		// singularities; beta is the vector of associated turning angles. qdat
		// is quadrature data from scqdata.
		
		// Make sure z and beta are column vectors.
		
		// dquad integrates from a possible singularity at the left end to a
		// regular point at the right.  If both endpoints are singularities,
		// you must break the integral into two pieces and make two calls.
		
		// The integral is subdivided, if necessary, so that no singularity lies
		// closer  to the left endpoint than 1/2 the length of the integration
		// (sub) interval
		int i, j, k, m;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int nqpts = qdat.length;
		int n = z.length;
		double bigz[][][] = new double[n][nqpts][2];
		for (i = 0; i < n; i++) {
		    for (j = 0; j < nqpts; j++) {
		    	bigz[i][j][0] = z[i][0];
		    	bigz[i][j][1] = z[i][1];
		    }
		} // for (i = 0; i < n; i++)
		double bigbeta[][] = new double[beta.length][nqpts];
		for (i = 0; i < beta.length; i++) {
			for (j = 0; j < nqpts; j++) {
				bigbeta[i][j] = beta[i];
			}
		} // for (i = 0; i < beta.length; i++)
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		} // if ((sing1 == null) || (sing1.length == 0))
		double I[][] = new double[z1.length][2];
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numnontriv++;
			}
		} // for (i = 0; i < z1.length; i++)
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				nontriv[j++] = i;
			}
		} // for (i = 0, j = 0; i < z1.length; i++)
		
		double za[] = new double[2];
		double zb[] = new double[2];
		double nd[][] = new double[nqpts][2];
		double wt[][] = new double[nqpts][2];
		double terms[][][] = new double[n][nqpts][2];
		double zr[] = new double[2];
		double zl[] = new double[2];
		double logterms[] = new double[2];
		double prod[] = new double[2];
		double expSum[] = new double[2];
		double expTerm;
	
		for (i = 0; i < nontriv.length; i++) {
			k = nontriv[i];
			za[0] = z1[k][0];
			za[1] = z1[k][1];
			zb[0] = z2[k][0];
			zb[1] = z2[k][1];
			int sng = sing1[k];
			
			// Allowable integration step, based on nearest singularity.
			double dist = 1.0;
			double denom = zabs(zb[0]-za[0],zb[1]-za[1]);
			double minVal = Double.MAX_VALUE;
			double absDiff;
			for (j = 0; j <=sng-1; j++) {
				absDiff = zabs(z[j][0] - za[0],z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}
			} // for (j = 0; j <=sng-1; j++)
			for (j = sng+1; j <= n-1; j++) {
				absDiff = zabs(z[j][0] - za[0],z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}	
			} // for (j = sng+1; j <= n-1; j++)
			minVal = 2.0*minVal/denom;
			if (minVal < dist) {
				dist = minVal;
			}
			zr[0] = za[0] + dist * (zb[0] - za[0]);
			zr[1] = za[1] + dist * (zb[1] - za[1]);
			// Adjust Gauss-Jacobi nodes and weights to interval.
			int ind = (sng+n+1)%(n+1);
			for (j = 0; j < nqpts; j++) {
				nd[j][0] = ((zr[0] - za[0])*qdat[j][ind] + zr[0] + za[0])/2.0; // G-J nodes
				nd[j][1] = ((zr[1] - za[1])*qdat[j][ind] + zr[1] + za[1])/2.0;
				wt[j][0] = ((zr[0]-za[0])/2.0) * qdat[j][ind+n+1];
				wt[j][1] = ((zr[1]-za[1])/2.0) * qdat[j][ind+n+1];// G-J weights
			} // for (j = 0; j < nqpts; j++)
			int zeroterms = 0;
			for (j = 0; j < n; j++) {
				for (m = 0; m < nqpts; m++) {
				    zdiv(nd[m][0], nd[m][1], bigz[j][m][0], bigz[j][m][1],cr,ci);
				    terms[j][m][0] = 1 - cr[0];
				    terms[j][m][1] = -ci[0];
				    if ((terms[j][m][0] == 0) && (terms[j][m][1] == 0)) {
				    	zeroterms++;
				    }
				}
			} // for (j = 0; j < n; j++)
			if (zeroterms > 0) {
				// Endpoints are practically coincident
				I[k][0] = 0;
				I[k][1] = 0;
			}
			else {
			    // Use Gauss-Jacobi on first subinterval, if necessary.
				if (sng >= 0) {
					double fac = zabs(zr[0]-za[0],zr[1]-za[1])/2.0;
				    double fac2 = Math.pow(fac, beta[sng]);
					for (m = 0; m < nqpts; m++) {
					    denom = zabs(terms[sng][m][0], terms[sng][m][1]);
					    terms[sng][m][0] = terms[sng][m][0]/denom;
					    terms[sng][m][1] = terms[sng][m][1]/denom;
					    wt[m][0] = wt[m][0] * fac2;
					    wt[m][1] = wt[m][1] * fac2;
					}
				} // if (sng >= 0)
				I[k][0] = 0;
				I[k][1] = 0;
			    for (m = 0; m < nqpts; m++) {
			    	expSum[0] = 0;
			    	expSum[1] = 0;
			    	for (j = 0; j < n; j++) {
						logterms[0] = Math.log(zabs(terms[j][m][0],terms[j][m][1]));
						logterms[1] = Math.atan2(terms[j][m][1], terms[j][m][0]);
						prod[0] = logterms[0] * bigbeta[j][m];
						prod[1] = logterms[1] * bigbeta[j][m];
						expSum[0] += prod[0];
						expSum[1] += prod[1];
					} // for (j = 0; j < n; j++)
			    	expTerm = Math.exp(expSum[0]);
			    	zmlt(expTerm*Math.cos(expSum[1]), expTerm*Math.sin(expSum[1]),
			    			wt[m][0], wt[m][1], cr, ci);
			    	I[k][0] += cr[0];
			    	I[k][1] += ci[0];
				} // for (m = 0; m < nqpts; m++)
			    while (dist < 1) {
			        // Do regular Gaussian quad on other subintervals.
			    	zl[0] = zr[0];
			    	zl[1] = zr[1];
			    	dist = 1.0;
			    	minVal = Double.MAX_VALUE;
			    	denom = zabs(zl[0] - zb[0], zl[1] - zb[1]);
			    	for (j = 0; j < n; j++) {
			    	    double num = zabs(z[j][0] - zl[0], z[j][1] - zl[1]);
			    	    if (num < minVal) {
			    	    	minVal = num;
			    	    }
			    	} // (j = 0; j < n; j++)
			    	minVal = 2.0*minVal/denom;
			    	if (minVal < dist) {
			    		dist = minVal;
			    	}
			    	zr[0] = zl[0] + dist * (zb[0] - zl[0]);
			    	zr[1] = zl[1] + dist * (zb[1] - zl[1]);
			    	for (j = 0; j < nqpts; j++) {
						nd[j][0] = ((zr[0] - zl[0])*qdat[j][n] + zr[0] + zl[0])/2.0;
						nd[j][1] = ((zr[1] - zl[1])*qdat[j][n] + zr[1] + zl[1])/2.0;
						wt[j][0] = ((zr[0]-zl[0])/2.0) * qdat[j][2*n+1];
						wt[j][1] = ((zr[1]-zl[1])/2.0) * qdat[j][2*n+1];
					} // for (j = 0; j < nqpts; j++)
			    	//I(k) = I(k) + exp(sum(log(1 - nd(ones(n,1),:)./bigz).*bigbeta)) * wt
			    	for (m = 0; m < nqpts; m++) {
			    		expSum[0] = 0;
			    		expSum[1] = 0;
						for (j = 0; j < n; j++) {
						    zdiv(nd[m][0], nd[m][1], bigz[j][m][0], bigz[j][m][1],cr,ci);
						    logterms[0] = Math.log(zabs(1 - cr[0],-ci[0]));
						    logterms[1] = Math.atan2(-ci[0],1-cr[0]);
						    prod[0] = logterms[0] * bigbeta[j][m];
						    prod[1] = logterms[1] * bigbeta[j][m];
						    expSum[0] += prod[0];
						    expSum[1] += prod[1];
						}
						expTerm = Math.exp(expSum[0]);
						zmlt(expTerm*Math.cos(expSum[1]), expTerm*Math.sin(expSum[1]), wt[m][0], wt[m][1], cr, ci);
						I[k][0] += cr[0];
						I[k][1] += ci[0];
			    	} // for (m = 0; m < nqpts; m++)
			    } // while (dist < 1)
			} // else
		} // for (i = 0; i < nontriv.length; i++)
		return I;
	}
	
	private double[][] dabsquad(double z1[][], double z2[][], int sing1[], double z[][],
			double beta[], double qdat[][]) {
		// Numerical quadrature for side lengths in the disk map.
		
		// dabsquad is similar to dquad, but the integrand is replaced by its
		// absolute value and the path of integration is along the unit circle
		// rather than the straight line between endpoints.  This allows the use
		// of real rather than complex logarithms in the computations of powers, 
		// which can result in substantial savings.
		
		// z1,z2 are vectors of left and right endpoints.  sing1 is a vector of 
		// indices which label the singularities in z1.  So if sing1[5] = 3,
		// then z1[5] = z[3].  A -1 means no singularity.  z is the vector of
		// singularities; beta is the vector of associated turning angles. qdat
		// is quadrature data from scqdata.
		
		// Make sure z and beta are column vectors.
		
		// dabsquad integrates from a possible singularity at the left end to a
		// regular point at the right.  If both endpoints are singularities,
		// you must break the integral into two pieces and make two calls.  The
		// integration always takes along the smaller arc of the circle between
		// the endpoints.
		
		// The integral is subdivided, if necessary, so that no singularity lies
		// closer  to the left endpoint than 1/2 the length of the integration
		// (sub) interval
		
		// Original MATLAB routine copyright 1997 by Toby Driscoll.
		// Last updated 05/08/97.
		int i, j, k, m;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int nqpts = qdat.length;
		int n = z.length;
		double argz[] = new double[n];
		for (i = 0; i < n; i++) {
			argz[i] = Math.atan2(z[i][1], z[i][0]); 
		}
		double argz1[] = new double[z1.length];
		for (i = 0; i < z1.length; i++) {
			argz1[i] = Math.atan2(z1[i][1], z1[i][0]);
		}
		double argz2[] = new double[z2.length];
		for (i = 0; i < z2.length; i++) {
			argz2[i] = Math.atan2(z2[i][1], z2[i][0]);
		}
		double ang21[] = new double[z1.length];
		for (i = 0; i < z1.length; i++) {
			zdiv(z2[i][0], z2[i][1], z1[i][0], z1[i][1], cr, ci);
			ang21[i] = Math.atan2(ci[0], cr[0]);
		}
		// Modification needed if integration passes through -1
		for (i = 0; i < z1.length; i++) {
			if (((argz2[i] - argz1[i]) * ang21[i]) < 0.0) {
				argz2[i] = argz2[i] + 2.0*Math.PI*sign(ang21[i]);
			}
		} // for (i = 0; i < z1.length; i++)
		double bigargz[][] = new double[n][nqpts];
		for (i = 0; i < n; i++) {
		    for (j = 0; j < nqpts; j++) {
		    	bigargz[i][j] = argz[i];
		    }
		} // for (i = 0; i < n; i++)
		double bigbeta[][] = new double[beta.length][nqpts];
		for (i = 0; i < beta.length; i++) {
			for (j = 0; j < nqpts; j++) {
				bigbeta[i][j] = beta[i];
			}
		} // for (i = 0; i < beta.length; i++)
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		} // if ((sing1 == null) || (sing1.length == 0))
		
		
		double I[][] = new double[z1.length][2];
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numnontriv++;
			}
		} // for (i = 0; i < z1.length; i++)
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				nontriv[j++] = i;
			}
		} // for (i = 0, j = 0; i < z1.length; i++)
		
		double za[] = new double[2];
		double zb[] = new double[2];
		double nd[] = new double[nqpts];
		double wt[] = new double[nqpts];
		double theta[][] = new double[n][nqpts];
		double terms[][] = new double[n][nqpts];
		double logterms[] = new double[2];
		double prod[] = new double[2];
		double expSum[] = new double[2];
		double zl[] = new double[2];
		for (i = 0; i < nontriv.length; i++) {
			k = nontriv[i];
			double arga = argz1[k];
			double argb = argz2[k];
			za[0] = z1[k][0];
			za[1] = z1[k][1];
			zb[0] = z2[k][0];
			zb[1] = z2[k][1];
			int sng = sing1[k];
			
			// Allowable integration step, based on nearest singularity.
			double dist = 1.0;
			double denom = zabs(zb[0]-za[0],zb[1]-za[1]);
			double minVal = Double.MAX_VALUE;
			double absDiff;
			for (j = 0; j <=sng-1; j++) {
				absDiff = zabs(z[j][0] - za[0],z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}
			} // for (j = 0; j <=sng-1; j++)
			for (j = sng+1; j <= n-1; j++) {
				absDiff = zabs(z[j][0] - za[0],z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}	
			} // for (j = sng+1; j <= n-1; j++)
			minVal = 2.0*minVal/denom;
			if (minVal < dist) {
				dist = minVal;
			}
			double argr = arga + dist * (argb-arga);
			// Adjust Gauss-Jacobi nodes and weights to interval.
			int ind = (sng+n+1)%(n+1);
			for (j = 0; j < nqpts; j++) {
				nd[j] = ((argr - arga)*qdat[j][ind] + argr + arga)/2.0; // G-J nodes
				wt[j] = (Math.abs(argr-arga)/2.0) * qdat[j][ind+n+1]; // G-J weights
			} // for (j = 0; j < nqpts; j++)
			int zeroterms = 0;
			for (j = 0; j < n; j++) {
				for (m = 0; m < nqpts; m++) {
					double ang = nd[m] - bigargz[j][m] + 2.0*Math.PI;
					double div = ang/(2.0*Math.PI);
			    	double fix;
			    	if (div >= 0) {
			    		fix = Math.floor(div);
			    	}
			    	else {
			    		fix = Math.ceil(div);
			    	}
			    	theta[j][m] = ang - 2.0*Math.PI*fix;
			    	if (theta[j][m] > Math.PI) {
			    		theta[j][m] = 2.0*Math.PI - theta[j][m];
			    	}
			    	terms[j][m] = 2.0 * Math.sin(theta[j][m]/2.0);
			    	if (terms[j][m] == 0) {
			    		zeroterms++;
			    	}
				}
			} // for (j = 0; j < n; j++)
			if (zeroterms > 0) {
				// Endpoints are practically coincident
				I[k][0] = 0;
				I[k][1] = 0;
			}
			else {
			    // Use Gauss-Jacobi on first subinterval, if necessary.
				if (sng >= 0) {
					double fac = Math.abs(argr-arga)/2.0;
				    double fac2 = Math.pow(fac, beta[sng]);
					for (m = 0; m < nqpts; m++) {
					    denom = Math.abs(nd[m] - arga);
					    terms[sng][m] = terms[sng][m]/denom;
					    wt[m] = wt[m] * fac2;
					}
				} // if (sng >= 0)
				I[k][0] = 0;
				I[k][1] = 0;
			    for (m = 0; m < nqpts; m++) {
			    	expSum[0] = 0;
			    	expSum[1] = 0;
			    	for (j = 0; j < n; j++) {
						logterms[0] = Math.log(Math.abs(terms[j][m]));
						logterms[1] = Math.atan2(0, terms[j][m]);
						prod[0] = logterms[0] * bigbeta[j][m];
						prod[1] = logterms[1] * bigbeta[j][m];
						expSum[0] += prod[0];
						expSum[1] += prod[1];
					} // for (j = 0; j < n; j++)
			    	double expTerm = Math.exp(expSum[0]);
			    	I[k][0] += expTerm*Math.cos(expSum[1]) * wt[m];
			    	I[k][1] += expTerm*Math.sin(expSum[1]) * wt[m];
				} // for (m = 0; m < nqpts; m++)
			    while (dist < 1) {
			        // Do regular Gaussian quad on other subintervals.
			    	double argl = argr;
			    	zl[0] = Math.cos(argl);
			    	zl[1] = Math.sin(argl);
			    	dist = 1.0;
			    	minVal = Double.MAX_VALUE;
			    	denom = zabs(zl[0] - zb[0], zl[1] - zb[1]);
			    	for (j = 0; j < n; j++) {
			    	    double num = zabs(z[j][0] - zl[0], z[j][1] - zl[1]);
			    	    if (num < minVal) {
			    	    	minVal = num;
			    	    }
			    	} // (j = 0; j < n; j++)
			    	minVal = 2.0*minVal/denom;
			    	if (minVal < dist) {
			    		dist = minVal;
			    	}
			    	argr = argl + dist*(argb-argl);
			    	for (j = 0; j < nqpts; j++) {
						nd[j] = ((argr - argl)*qdat[j][n] + argr + argl)/2.0; 
						wt[j] = (Math.abs(argr-argl)/2.0) * qdat[j][2*n+1];
					} // for (j = 0; j < nqpts; j++)
			    	for (j = 0; j < n; j++) {
						for (m = 0; m < nqpts; m++) {
							double ang = nd[m] - bigargz[j][m] + 2.0*Math.PI;
							double div = ang/(2.0*Math.PI);
					    	double fix;
					    	if (div >= 0) {
					    		fix = Math.floor(div);
					    	}
					    	else {
					    		fix = Math.ceil(div);
					    	}
					    	theta[j][m] = ang - 2.0*Math.PI*fix;
					    	if (theta[j][m] > Math.PI) {
					    		theta[j][m] = 2.0*Math.PI - theta[j][m];
					    	}
						}
					} // for (j = 0; j < n; j++)
			    	for (m = 0; m < nqpts; m++) {
				    	expSum[0] = 0;
				    	expSum[1] = 0;
				    	for (j = 0; j < n; j++) {
				    		double term = 2.0*Math.sin(theta[j][m]/2.0);
							logterms[0] = Math.log(Math.abs(term));
							logterms[1] = Math.atan2(0, term);
							prod[0] = logterms[0] * bigbeta[j][m];
							prod[1] = logterms[1] * bigbeta[j][m];
							expSum[0] += prod[0];
							expSum[1] += prod[1];
						} // for (j = 0; j < n; j++)
				    	double expTerm = Math.exp(expSum[0]);
				    	I[k][0] += expTerm*Math.cos(expSum[1]) * wt[m];
				    	I[k][1] += expTerm*Math.sin(expSum[1]) * wt[m];
					} // for (m = 0; m < nqpts; m++)
			    } // while (dist < 1)
			} // else
		} // for (i = 0; i < nontriv.length; i++)
		return I;
	}
	
	class crpfun extends NLConstrainedEngine {
		int n;
		double beta[];
		double crtarget[][];
		qlgraph Q;
		double qdat[][];
		public crpfun(double x[], int n, double beta[], double crtarget[][], qlgraph Q, double qdat[][]) {
			// nPoints, params
			super(x.length, x.length);
			this.n = n;
			this.beta = beta;
			this.crtarget = crtarget;
			this.Q = Q;
			this.qdat = qdat;
			
			bounds = 0; // bounds = 0 means unconstrained

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < x.length; i++) {
				gues[i] = x[i];
			}
		}
		
		/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}
		
		/**
		 * Display results of displaying exponential fitting parameters.
		 */
		public void dumpResults() {
			Preferences
					.debug(" ******* Fit Elsunc Schwarz-Christoffel crparam ********* \n\n",
							Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters)
					+ "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared())
					+ "\n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < a.length; i++) {
			Preferences.debug("a"+i+" " + String.valueOf(a[i]) + "\n",
					Preferences.DEBUG_ALGORITHM);
			}
		}
		
		public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
    		int ctrl;
    		int i, k;
    		double prever[][];
    		double w[][];
    		double z1[][] = new double[4][2];
    		int sing1[] = new int[4];
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		double num[] = new double[2];
    		double denom[] = new double[2];
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					// prevertex crossratios
					double crprever[] = new double[a.length];
					for (i = 0; i < a.length; i++) {
						crprever[i] = Math.exp(a[i]);
					}
					// image vertex crossratios
					double crimage[][] = new double[n-3][2];
					
					// Compute crossratio for each image quadrilateral
					for (k = 0; k < n-3; k++) {
					    prever = crembed(crprever, Q, k);
					    for (i = 0; i < 4; i++) {
					    	z1[i][0] = prever[Q.qlvert[i][k]][0];
					    	z1[i][1] = prever[Q.qlvert[i][k]][1];
					    	sing1[i] = Q.qlvert[i][k];
					    }
					    w = crquad(z1, sing1, prever, beta, qdat);
					    for (i = 0; i < w.length; i++) {
					    	w[i][0] = -w[i][0];
					    	w[i][1] = -w[i][1];
					    }
					    zmlt(w[1][0] - w[0][0], w[1][1] - w[0][1], w[3][0] - w[2][0], w[3][1] - w[2][1], cr, ci);
					    num[0] = cr[0];
					    num[1] = ci[0];
					    zmlt(w[2][0] - w[1][0], w[2][1] - w[1][1], w[0][0] - w[3][0], w[0][1] - w[3][1], cr, ci);
					    denom[0] = cr[0];
					    denom[1] = ci[0];
					    zdiv(num[0], num[1], denom[0], denom[1], cr, ci);
					    crimage[k][0] = cr[0];
					    crimage[k][1] = ci[0];
					} // for (k = 0; k < n-3; k++)
					
					// Logarithmic scaling for residual
					for (i = 0; i < n-3; i++) {
						zdiv(crimage[i][0], crimage[i][1], crtarget[i][0], crtarget[i][1], cr, ci);
						residuals[i] = Math.log(zabs(cr[0], ci[0]));
					}
				} // if ((ctrl == -1) || (ctrl == 1))

				// Calculate the Jacobian numerically
				else if (ctrl == 2) {
					ctrlMat[0] = 0;
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n",
						Preferences.DEBUG_ALGORITHM);
			}

			return;
		}
	}
	
	private double[][] crembed(double cr[], qlgraph Q, int qnum) {
		// Embed prevertices for given crossratios
		// crembed embeds prevertices of given crossratios cr and quadrilateral graph
		// information Q (see qlgraph).  The embedding is constructed so that the 
		// prevertices of quadrilateral qnum are in a rectangle and do not crowd up
		// against each other or any other prevertices (as long as no crossratios are
		// very far from unity).  The resulting S-C map should be accurate for that
		// rectangle, including the four prevertices ofquadrilateral qnum.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		// This function was adapted from a C routine written by Stephen Vavasis.
		
		int i;
		int n = cr.length + 3;
		double z[][] = new double[n][2];
		int itmp[] = new int[4];
		double diff[] = new double[2];
		double cre[] = new double[1];
		double cim[] = new double[1];
		double fac[] = new double[2];
		double zmod[];
		
		// Place the quadrilateral spanned by the initial diagonal at a rectangle 
		// around the origin.
		double r = cr[qnum];
		double f1 = Math.sqrt(1.0/(r+1.0));
		double f2 = Math.sqrt(r/(r+1.0));
		// Quadrilateral vertices
		int idx[] = new int[4];
		for (i = 0; i < 4; i++) {
		    idx[i] = Q.qlvert[i][qnum];	
		}
		z[idx[0]][0] = -f1;
		z[idx[0]][1] = -f2;
		z[idx[1]][0] = -f1;
		z[idx[1]][1] = f2;
		z[idx[2]][0] = f1;
		z[idx[2]][1] = f2;
		z[idx[3]][0] = f1;
		z[idx[3]][1] = -f2;
		
		// Set up "already visited" lists
		boolean vtxdone[] = new boolean[n];
		for (i = 0; i < 4; i++) {
			vtxdone[idx[i]] = true;
		}
		boolean edgedone[] = new boolean[2*n-3];
		edgedone[qnum] = true;
		// Mark boundary edges as "done" so they will be ignored
		for (i = n-3; i < 2*n-3; i++) {
			edgedone[i] = true;
		}
		
		// Set up "ready to do" list.  There does not need to be a strict ordering as
		// in a queue or stack; any ready edge can go next.
		boolean edgetodo[] = new boolean[2*n-3];
		// The edges of the initial quadrilateral are ready to go
		for (i = 0; i < 4; i++) {
			edgetodo[Q.qledge[i][qnum]] = !edgedone[Q.qledge[i][qnum]];
		}
		int numedgetodo = 0;
		for (i = 0; i < 2*n-3; i++) {
			if (edgetodo[i]) {
				numedgetodo++;
			}
		}
		
		// Begin iteration
		while (numedgetodo > 0) {
			int e = -1;
			for (i = 0; i < 2*n-3 && (e == -1); i++) {
				if (edgetodo[i]) {
					e = i;
				}
			} // for (i = 0; i < 2*n-3 && (e == -1); i++) 
			for (i = 0; i < 4; i++) {
				idx[i] = Q.qlvert[i][e];
			}
			// If necessary, renumber so that z[idx[3]] is to be determined
			if (!vtxdone[idx[1]]) {
			    itmp[0] = idx[2];
			    itmp[1] = idx[3];
			    itmp[2] = idx[0];
			    itmp[3] = idx[1];
			    for (i = 0; i < 4; i++) {
			    	idx[i] = itmp[i];
			    }
			} // if (!vtxdone[idx[1]])
			
			// Work around divide by zero.  This will have no dire effects,
			// since there is no guarantee that the prevertices nt in qnum will be separated.
			diff[0] = z[idx[1]][0] - z[idx[0]][0];
			diff[1] = z[idx[1]][1] - z[idx[0]][1];
			
			                                                                                                   
			if (zabs(diff[0], diff[1]) < 5.0*eps) {
				z[idx[3]][0] = z[idx[1]][0];
				z[idx[3]][1] = z[idx[1]][1];
			}
			else {
				// Place z[idx[3]]
				diff[0] = z[idx[2]][0] - z[idx[1]][0];
				diff[1] = z[idx[2]][1] - z[idx[1]][1];
				zdiv(-cr[e]*diff[0], -cr[e]*diff[1], z[idx[1]][0] - z[idx[0]][0], z[idx[1]][1] - z[idx[0]][1], cre, cim);
				fac[0] = cre[0];
				fac[1] = cim[0];
				zmlt(fac[0], fac[1], z[idx[0]][0], z[idx[0]][1], cre, cim);
				zdiv(z[idx[2]][0] + cre[0], z[idx[2]][1] + cim[0], 1.0 + fac[0], fac[1], cre, cim);
				z[idx[3]][0] = cre[0];
				z[idx[3]][1] = cim[0];
				// Keep vertices exactly on the unit circle (fix roundoff)
				zmod = sign(z[idx[3]][0], z[idx[3]][1]);
				z[idx[3]][0] = zmod[0];
				z[idx[3]][1] = zmod[1];
			} // else
			
			vtxdone[idx[3]] = true;
			edgedone[e] = true;
			edgetodo[e] = false;
			// Ready to do neighboring edges that still have not been visited.
			for (i = 0; i < 4; i++) {
				edgetodo[Q.qledge[i][e]] = !edgedone[Q.qledge[i][e]];
			}
			numedgetodo = 0;
			for (i = 0; i < 2*n-3; i++) {
				if (edgetodo[i]) {
					numedgetodo++;
				}
			}
		} // while (numedgetodo > 0)
		return z;
	}
	
	private double[][] crquad(double z1[][], int sing1[], double z[][], double beta[], double qdat[][]) {
		// Numerical quadrature for the disk map, crossratio formulation
		
		// z1 is a vector of left endpoints; -1 is always the right endpoint.
		// sing1 is a vector of integer indices which label the singularities
		// in z1.  So if sing1[5] = 3, then z1[5] = z[3].  A -1 means no 
		// singularity.  z is the vector of prevertices; beta is the vector of
		// associated turning angles.  qdat is the quadrature data from scqdata.
		
		// The integral is subdivided, if necessary, so that no singularity
		// lies closer to the left endpoint than 1/2 the length of the
		// integration (sub)interval.
		
		// The conceptual differences between crquad and dquad are tiny.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		
		int i,j, k, m;
		double za[] = new double[2];
		double zb[] = new double[2];
		int sng;
		int qcol;
		int nqpts = qdat.length;
		double nd[][] = new double[nqpts][2];
		double wt[][] = new double[nqpts][2];
		double diff[] = new double[2];
		int n = z.length;
		boolean mask[] = new boolean[n];
		double bigz[][][] = new double[n][nqpts][2];
		double ndmask[] = new double[2];
		double bigzmask[] = new double[2];
		double cr[] = new double[1];
		double ci[] = new double[1];
		double h = 0.0;
		for (i = 0; i < n; i++) {
			for (j = 0; j < nqpts; j++) {
			    bigz[i][j][0] = z[i][0];
			    bigz[i][j][1] = z[i][1];
			}
		}
		double bigbeta[][] = new double[n][nqpts];
		for (i = 0; i < n; i++) {
			for (j = 0; j < nqpts; j++) {
				bigbeta[i][j] = beta[i];
			}
		}
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		}
		
		// Will ignore zero values of beta
		boolean ignore[] = new boolean[n];
		for (i = 0; i < n; i++) {
			if (Math.abs(beta[i]) < eps) {
				ignore[i] = true;
			}
		}
		double I[][] = new double[z1.length][2];
		double z1abs[] = new double[z1.length];
		for (i = 0; i < z1.length; i++) {
		    z1abs[i] = zabs(z1[i][0], z1[i][1]);
		}
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if (z1abs[i] > eps) {
				numnontriv++;
			}
		}
	    int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if (z1abs[i] > eps) {
				nontriv[j++] = i;
			}
		}
		for (k = 0; k < numnontriv; k++) {
		    za[0] = z1[nontriv[k]][0];
		    za[1] = z1[nontriv[k]][1];
		    sng = sing1[nontriv[k]];
		    for (i = 0; i < n; i++) {
		        mask[i] = !ignore[i];	
		    }
		    
		    // Integration subintervals are based on nearest singularity
		    if (sng >= 0) {
		    	mask[sng] = false;
		    }
		    double mindist = Double.MAX_VALUE;
		    for (i = 0; i < n; i++) {
		    	if (mask[i] ) {
		    		double dist = zabs(z[i][0]-za[0], z[i][1]-za[1]);
		    		if (dist < mindist) {
		    			mindist = dist;
		    		}
		    	}
		    } // for (i = 0; i < n; i++)
		    int panels = (int)Math.max(1, Math.ceil(-Math.log(mindist)/Math.log(2.0)));
		    if (sng >= 0) {
		    	mask[sng] = !ignore[sng];
		    }
		    
		    qcol = (sng+n+1)%(n+1);
		    int numkeep = 0;
		    for (i = 0; i < n; i++) {
		    	if (mask[i]) {
		    		numkeep++;
		    	}
		    }
		    int keep[] = new int[numkeep];
		    for (i = 0, j = 0; i < n; i++) {
		        if (mask[i]) {
		        	keep[j++] = i;
		        }
		    } // for (i = 0, j = 0; i < n; i++)
		    
		    // Adjust sng for ignored vertices
		    if (sng >= 0) {
		    	if (ignore[sng]) {
		    		sng = -1;
		    	}
		    	else {
		    		int sum = 0;
		    		for (i = 0; i <= sng; i++) {
		    			if (ignore[i]) {
		    				sum++;
		    			}
		    		}
		    		sng = sng - sum;
		    	}
		    } // if (sng >= 0)
		    
		    for (j = 1; j <= panels; j++) {
		        if (j == 1) {
		            h = Math.pow(2, (1-panels));	
		        }
		        else {
		        	h = h + Math.pow(2, (j-panels-1));
		        	za[0] = zb[0];
		        	za[1] = zb[1];
		        	qcol = n;
		        }
		        zb[0] = z1[nontriv[k]][0]*(1-h);
		        zb[1] = z1[nontriv[k]][1]*(1-h);
		        
		        // Adjust Gauss-Jacobi nodes and weights to interval
		        diff[0] = zb[0] - za[0];
		        diff[1] = zb[1] - za[1];
		        int numdiffndzero = 0;
		        for (i = 0; i < nqpts; i++) {
		        	// G-J nodes
		            nd[i][0] = (diff[0]*qdat[i][qcol] + zb[0] + za[0])/2.0;
		            nd[i][1] = (diff[1]*qdat[i][qcol] + zb[1] + za[1])/2.0;
		            if ((i >= 1) && (nd[i][0] == nd[i-1][0]) && (nd[i][1] == nd[i-1][1])) {
		            	numdiffndzero++;
		            }
		            // G-J weights
		            wt[i][0] = (diff[0]/2.0) * qdat[i][qcol+n+1];
		            wt[i][1] = (diff[1]/2.0) * qdat[i][qcol+n+1];
		        }
		       
		        double terms[][][] = new double[numkeep][nqpts][2];
		        int numtermszero = 0;
		        for (i = 0; i < numkeep; i++) {
		            for (m = 0; m < nqpts; m++) {
		            	ndmask[0] = nd[m][0];
		            	ndmask[1] = nd[m][1];
		            	bigzmask[0] = bigz[keep[i]][m][0];
		            	bigzmask[1] = bigz[keep[i]][m][1];
		            	zdiv(ndmask[0], ndmask[1], bigzmask[0], bigzmask[1], cr, ci);
		            	terms[i][m][0] = 1.0 - cr[0];
		            	terms[i][m][1] = -ci[0];
		            	if ((terms[i][m][0] == 0) && (terms[i][m][1] == 0)) {
		            		numtermszero++;
		            	}
		            }
		        } // for (i = 0; i < numkeep; i++)
		        // Check for coincident values indicating crowding (Should never happen!)
		        if ((numdiffndzero > 0) || (numtermszero > 0)) {
		        	MipavUtil.displayWarning("Prevertices are too crowded");
		        	I[nontriv[k]][0] = 0;
		        	I[nontriv[k]][1] = 0;
		        }
		        else {
		        	// Use Guass-Jacobi on first subinterval, if necessary.
		        	if ((sng >= 0) && (qcol < n)) {
		        		for (i = 0; i < nqpts; i++) {
		        			double absterm = zabs(terms[sng][i][0], terms[sng][i][1]);
		        			terms[sng][i][0] = terms[sng][i][0]/absterm;
		        			terms[sng][i][1] = terms[sng][i][1]/absterm;
		        			absterm = zabs(zb[0]-za[0],zb[1]-za[1]);
		        			double fac = Math.pow(absterm/2.0, beta[keep[sng]]);
		        			wt[i][0] = wt[i][0] * fac;
		        			wt[i][1] = wt[i][1] * fac;
		        		}
		        	} // if ((sng >= 0) && (qcol < n))
		        	double bigbetamask;
		        	double realsum[] = new double[nqpts];
		        	double imagsum[] = new double[nqpts];
		        	for (m = 0; m < nqpts; m++) {
		        	    for (i = 0; i < numkeep; i++) {
		        			bigbetamask = bigbeta[keep[i]][m];
		        			double reallog = Math.log(zabs(terms[i][m][0],terms[i][m][1]));
		        			double imaglog = Math.atan2(terms[i][m][1],terms[i][m][0]);
		        			double realmult = bigbetamask*reallog;
		        			double imagmult = bigbetamask*imaglog;
		        			realsum[m] += realmult;
		        			imagsum[m] += imagmult;
		        		} // for (i = 0; i < numkeep; i++)
		        	} // for (m = 0; m < nqpts; m++)
		        	double realexp[] = new double[nqpts];
		        	double imagexp[] = new double[nqpts];
		        	for (m = 0; m < nqpts; m++) {
		        		double expterm = Math.exp(realsum[m]);
		        		realexp[m] = expterm*Math.cos(imagsum[m]);
		        		imagexp[m] = expterm*Math.sin(imagsum[m]);
		        		zmlt(realexp[m],imagexp[m], wt[m][0], wt[m][1], cr, ci);
		        		I[nontriv[k]][0] += cr[0];
		        		I[nontriv[k]][1] += ci[0];
		        	}
		        }
		    } // for (j = 1; j <= panels; j++)
		} // for (k = 0; k < numnontriv; k++)
		return I;
	}
	
	class rpfun extends NLConstrainedEngine {
		int n;
		double beta[];
		double nmlen[][];
		int left[];
		int right[];
		boolean cmplx[];
		double qdat[][];
		int corners[];
		
    	public rpfun (double y0[], int n, double beta[], double nmlen[][], int left[],
    			int right[], boolean cmplx[], double qdat[][], int corners[]) {
    		// nPoints, params
    		super(y0.length, y0.length);
    		this.n = n;
    		this.beta = beta;
    		this.nmlen = nmlen;
    		this.left = left;
    		this.right = right;
    		this.cmplx = cmplx;
    		this.qdat = qdat;
    		this.corners = corners;
    		
    		bounds = 0; // bounds = 0 means unconstrained

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < y0.length; i++) {
				gues[i] = y0[i];
			}
    	}
    	
    	/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}
		
		/**
		 */
		public void dumpResults() {
			Preferences
					.debug(" ******* Fit Elsunc Schwarz-Christoffel rparam ********* \n\n",
							Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters)
					+ "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared())
					+ "\n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < a.length; i++) {
			Preferences.debug("a"+i+" " + String.valueOf(a[i]) + "\n",
					Preferences.DEBUG_ALGORITHM);
			}
		}
    	
    	public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
    		int ctrl;
    		int i, j, k;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		double I3[][];
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					
					// Transform a (unconstrained variables) to z (actual params)
					z = new double[a.length+3][2];
					rptrnsfm(z, a, corners);
					
					// Compute the integrals appearing in the nonlinear equations
					double zleft[][] = new double[left.length][2];
					for (i = 0; i < left.length; i++) {
						zleft[i][0] = z[left[i]][0];
						zleft[i][1] = z[left[i]][1];
					}
					double zright[][] = new double[right.length][2];
					for (i = 0; i < right.length; i++) {
						zright[i][0] = z[right[i]][0];
						zright[i][1] = z[right[i]][1];
					}
					
					// To use stquad, must put strip ends into z, beta
					double diffz[] = new double[z.length];
					int nonzerodiff = 0;
					for (i = 0; i < n-1; i++) {
						diffz[i] = z[i+1][1] - z[i][1];
						if (diffz[i] != 0) {
							nonzerodiff++;
						}
					}
					diffz[n-1] = z[0][1] - z[n-1][1];
					if (diffz[n-1] != 0) {
						nonzerodiff++;
					}
					int ends[] = new int[nonzerodiff];
					for (i = 0, j = 0; i < diffz.length; i++) {
						if (diffz[i] != 0) {
						    ends[j++] = i;	
						}
					}
					double z2[][] = new double[n+2][2];
					double beta2[] = new double[n+2];
					for (i = 0; i <= ends[0]; i++) {
						z2[i][0] = z[i][0];
						z2[i][1] = z[i][1];
						beta2[i] = beta[i];
					}
					z2[ends[0]+1][0] = Double.POSITIVE_INFINITY;
					z2[ends[0]+1][1] = 0;
					beta2[ends[0]+1] = 0;
					for (i = ends[0]+1; i <= ends[1]; i++) {
						z2[i+1][0] = z[i][0];
						z2[i+1][1] = z[i][1];
						beta2[i+1] = beta[i];
					}
					z2[ends[1]+2][0] = Double.NEGATIVE_INFINITY;
					z2[ends[1]+2][1] = 0;
					beta2[ends[1]+2] = 0;
					for (i = ends[1]+1; i < n; i++) {
						z2[i+2][0] = z[i][0];
						z2[i+2][1] = z[i][1];
						beta2[i+2] = beta[i];
					}
					// Put dummy columns into qdat after ends[0] and ends[1] columns
					// qdata is nqpts rows by 2*n+2 columns
					// so we are expanding it to 2*n+6 columns with the insertion
					// of 2 more of column n and 2 more of column 2*n+1.
					double qdat2[][] = new double[qdat.length][qdat[0].length+4];
					for (i = 0; i < qdat.length; i++) {
						for (j = 0; j <= ends[0]; j++) {
							qdat2[i][j] = qdat[i][j];
							qdat2[i][j+2+n+1] = qdat[i][j+n+1];
						}
						qdat2[i][ends[0]+1] = qdat[i][n];
						qdat2[i][ends[0]+3+n+1] = qdat[i][2*n+1];
						for (j = ends[0]+1; j <= ends[1]; j++) {
							qdat2[i][j+1] = qdat[i][j];
							qdat2[i][j+3+n+1] = qdat[i][j+n+1];
						}
						qdat2[i][ends[1]+2] = qdat[i][n];
						qdat2[i][ends[1]+4+n+1] = qdat[i][2*n+1];
						for (j = ends[1]+1; j <= n-1; j++) {
							qdat2[i][j+2] = qdat[i][j];
							qdat2[i][j+4+n+1] = qdat[i][j+n+1];
						}
						qdat2[i][n+2] = qdat[i][n];
						qdat2[i][2*n+5] = qdat[i][2*n+1];
					} // for (i = 0; i < qdat.length; i++)
					// Change singularity indices to reflect ends
					int left2[] = new int[left.length];
					for (i = 0; i < left.length; i++) {
						if (left[i] > ends[1]) {
							left2[i] = left[i] + 2;
						}
						else if (left[i] > ends[0]) {
							left2[i] = left[i] + 1;
						}
						else {
							left2[i] = left[i];
						}
					} // for (i = 0; i < left.length; i++)
					int right2[] = new int[right.length];
					for (i = 0; i < right.length; i++) {
						if (right[i] > ends[1]) {
							right2[i] = right[i] + 2;
						}
						else if (right[i] > ends[0]) {
							right2[i] = right[i] +1;
						}
						else {
							right2[i] = right[i];
						}
					} // for (i = 0; i < right.length; i++)
					
					double ints[][] = new double[zleft.length][2];
					int nums2 = 0;
					for (i = 0; i < left2.length; i++) {
						if ((right2[i] - left2[i] == 1) && (zleft[i][1] == zright[i][1])) {
							nums2++;
						}
					}
					int s2[] = new int[nums2];
					int nots2[] = new int[left2.length-nums2];
					for (i = 0, j = 0, k = 0; i < left2.length; i++) {
						if ((right2[i] - left2[i] == 1) && (zleft[i][1] == zright[i][1])) {
							s2[j++] = i;
						}
						else {
							nots2[k++] = i;
						}
					}
					double mid[][] = new double[s2.length][2];
					for (i = 0; i < s2.length; i++) {
						mid[i][0] = (zleft[s2[i]][0] + zright[s2[i]][0])/2.0;
						mid[i][1] = (zleft[s2[i]][1] + zright[s2[i]][1])/2.0;
					}
				    double zlefts2[][] = new double[s2.length][2];
				    int lefts2[] = new int[s2.length];
				    double zrights2[][] = new double[s2.length][2];
				    int rights2[] = new int[s2.length];
				    for (i = 0; i < s2.length; i++) {
				    	zlefts2[i][0] = zleft[s2[i]][0];
				    	zlefts2[i][1] = zleft[s2[i]][1];
				    	lefts2[i] = left2[s2[i]];
				    	zrights2[i][0] = zright[s2[i]][0];
				    	zrights2[i][1] = zright[s2[i]][1];
				    	rights2[i] = right2[s2[i]];
				    }
				    I1 = stquadh(zlefts2, mid, lefts2, z2, beta2, qdat2);
				    I2 = stquadh(zrights2, mid, rights2, z2, beta2, qdat2);
				    for (i = 0; i < s2.length; i++) {
				        ints[s2[i]][0] = I1[i][0] - I2[i][0];
				        ints[s2[i]][1] = I1[i][1] - I2[i][1];
				    }
				    
				    // Three-stage integrations
				    int lengthnots2 = zleft.length - s2.length;
				    double mid1[][] = new double[lengthnots2][2];
				    double mid2[][] = new double[lengthnots2][2];
				    for (i = 0; i < lengthnots2; i++) {
				    	mid1[i][0] = zleft[nots2[i]][0];
				    	mid1[i][1] = 0.5;
				    	mid2[i][0] = zright[nots2[i]][0];
				    	mid2[i][1] = 0.5;
				    }
				    double zleftns2[][] = new double[nots2.length][2];
				    int leftns2[] = new int[nots2.length];
				    double zrightns2[][] = new double[nots2.length][2];
				    int rightns2[] = new int[nots2.length];
				    for (i = 0; i < nots2.length; i++) {
				    	zleftns2[i][0] = zleft[nots2[i]][0];
				    	zleftns2[i][1] = zleft[nots2[i]][1];
				    	leftns2[i] = left2[nots2[i]];
				    	zrightns2[i][0] = zright[nots2[i]][0];
				    	zrightns2[i][1] = zright[nots2[i]][1];
				    	rightns2[i] = right2[nots2[i]];
				    }
				    int zero[] = new int[nots2.length];
				    I1 = stquad(zleftns2, mid1, leftns2, z2, beta2, qdat2);
				    I2 = stquadh(mid1, mid2, zero, z2, beta2, qdat2);
				    I3 = stquad(zrightns2, mid2, rightns2, z2, beta2, qdat2);
				    for (i = 0; i < nots2.length; i++) {
				    	ints[nots2[i]][0] = I1[i][0] + I2[i][0] - I3[i][0];
				    	ints[nots2[i]][1] = I1[i][1] + I2[i][1] - I3[i][1];
				    }
				    
				    boolean found = false;
				    for (i = 0; i < ints.length && (!found); i++) {
				    	if (((ints[i][0] == 0) && (ints[i][1] == 0)) || Double.isNaN(ints[i][0]) || Double.isNaN(ints[i][1])) {
				    		// Singularities were too crowded
				    		found = true;
				    		MipavUtil.displayWarning("Severe crowding");
				    	}
				    } // for (i = 0; i < ints.length && (!found); i++)
				    
				    // Compute nonlinear equation residual values
				    boolean cmplx2[] = new boolean[cmplx.length-1];
				    for (i = 0; i < cmplx.length-1; i++) {
				    	cmplx2[i] = cmplx[i+1];
				    }
				    int numcmplx = 0;
				    int numnotcmplx = 0;
				    for (i = 0; i < cmplx.length; i++) {
				    	if (cmplx[i]) {
				    		numcmplx++;
				    	}
				    	else {
				    		numnotcmplx++;
				    	}
				    } // for (i = 0; i < cmplx.length; i++)
				    double F[] = new double[numnotcmplx];
				    for (i = 0, j = 0; i < ints.length; i++) {
				    	if (!cmplx[i]) {
				    	    F[j++] = zabs(ints[i][0],ints[i][1]);	// F[0] = abs(ints[0])
				    	}
				    }
				    int numcmplx2 = 0;
				    int numnotcmplx2 = 0;
				    for (i = 0; i < cmplx2.length; i++) {
				    	if (cmplx2[i]) {
				    		numcmplx2++;
				    	}
				    	else {
				    		numnotcmplx2++;
				    	}
				    } // for (i = 0; i < cmplx2.length; i++)
				    double nmlennotcmplx2[] = new double[numnotcmplx2];
				    double nmlencmplx2[][] = new double[numcmplx2][2];
				    for (i = 0, j = 0, k = 0; i < nmlen.length; i++) {
				    	if (!cmplx2[i]) {
				    		nmlennotcmplx2[j++] = nmlen[i][0];
				    	}
				    	else {
				    		nmlencmplx2[k][0] = nmlen[i][0];
				    		nmlencmplx2[k++][1] = nmlen[i][1];
				    	}
				    } // for (i = 0, j = 0, k = 0; i < nmlen.length; i++)
				    double Fmod[] = new double[F.length-1];
				    for (i = 0; i < F.length-1; i++) {
				    	Fmod[i] = Math.log(F[i+1]/F[0]/nmlennotcmplx2[i]);
				    	residuals[i] = Fmod[i];
				    }
				    if (numcmplx > 0) {
				    	double intscmplx[][] = new double[numcmplx][2];
				    	for (i = 0, j = 0; i < cmplx.length; i++) {
					    	if (cmplx[i]) {
					    	    intscmplx[j][0] = ints[i][0];
					    	    intscmplx[j++][1] = ints[i][1];
					    	}
					    } // for (i = 0, j = 0; i < cmplx.length; i++)
				    	double realF2[] = new double[numcmplx];
				    	double imagF2[] = new double[numcmplx];
				    	for (i = 0; i < numcmplx; i++) {
				    	    zdiv(intscmplx[i][0], intscmplx[i][1], ints[0][0], ints[0][1], cr, ci);
				    	    zdiv(cr[0], ci[0], nmlencmplx2[i][0], nmlencmplx2[i][1], cr, ci);
				    	    realF2[i] = Math.log(zabs(cr[0],ci[0]));
				    	    imagF2[i] = Math.atan2(ci[0], cr[0]);
				    	} // for (i = 0; i < numcmplx; i++)
				    	for (i = 0; i < numcmplx; i++) {
			    	    	residuals[i+Fmod.length] = realF2[i];
			    	    	residuals[i+Fmod.length+numcmplx] = imagF2[i];
			    	    }
				    } // if (numcmplx > 0)
				    
				} // if ((ctrl == -1) || (ctrl == 1))

				// Calculate the Jacobian numerically
				else if (ctrl == 2) {
					ctrlMat[0] = 0;
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n",
						Preferences.DEBUG_ALGORITHM);
			}

			return;
    		
    	}
    }
	
			
	/**
     * complex divide c = a/b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    public void zdiv(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double bm, cc, cd, ca, cb;

        bm = 1.0 / zabs(br, bi);
        cc = br * bm;
        cd = bi * bm;
        ca = ( (ar * cc) + (ai * cd)) * bm;
        cb = ( (ai * cc) - (ar * cd)) * bm;
        cr[0] = ca;
        ci[0] = cb;

        return;
    }
    
    /**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     * 
     * @param zr double
     * @param zi double
     * 
     * @return double
     */
    public double zabs(final double zr, final double zi) {
        double u, v, q, s;
        u = Math.abs(zr);
        v = Math.abs(zi);
        s = u + v;

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        s = s * 1.0;

        if (s == 0.0) {
            return 0.0;
        } else if (u > v) {
            q = v / u;

            return (u * Math.sqrt(1.0 + (q * q)));
        } else {
            q = u / v;

            return (v * Math.sqrt(1.0 + (q * q)));
        }
    }
    
    /**
     * complex multiply c = a * b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    public void zmlt(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double ca, cb;

        ca = (ar * br) - (ai * bi);
        cb = (ar * bi) + (ai * br);
        cr[0] = ca;
        ci[0] = cb;

        return;
    }
    
    public double[][] stquadh(double z1[][], double z2[][], int sing1[], double z[][],
			double beta[], double qdat[][]) {
		// Original MATLAB stquadh routine copyright 1998 by Toby Driscoll.
		
		// stquadh applies the "1/2 rule" by assuming that the distance from the
		// integration interval to the nearest singularity is equal to the 
		// distance from the left endpoint to the nearest singularity.  This is
		// certainly true e.g. if one begins integration at the nearest
		// singularity to the target point.  However, it may be violated in
		// important circumstances, such as when one integrates from the 
		// next-nearest singularity (because the nearest maps to infinity), or when
		// one is integrating between adjacent prevertices (as in the param
		// problem).  The main difficulty is the possibility of singularities
		// from the "other side" of the strip intruding.
		
		// Here we assume the integration intervals are horizontal.  This
		// function recursively subdivides the interval until the "1/2 rule" is
		// satisfied for singularities "between" the endpoints.  Actually, we
		// use a more stringent "alpha rule", for alpha > 1/2, as this seems to
		// be necessary sometimes.
		
		// There must be no singularities *inside* the interval, of course.
		int i, j, k, kk;
		double za[][] = new double[1][2];
		double zb[][] = new double[1][2];
		int sng;
		double alf;
		double zmid[][] = new double[1][2];
		int n = z.length;
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		}
		double I[][] = new double[z1.length][2];
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numnontriv++;
			}
		}
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				nontriv[j++] = i;
			}	
		}
		
		
		for (kk = 0; kk < numnontriv; kk++) {
			k = nontriv[kk];
			za[0][0] = z1[k][0];
			za[0][1] = z1[k][1];
			zb[0][0] = z2[k][0];
			zb[0][1] = z2[k][1];
			sng = sing1[k];
			
			// alf == 1/2 means the "1/2 rule."  Better to be more strict.
			alf = .75;
			// Given integration length
			double d = zb[0][0] - za[0][0];
			
			// Compute horizontal position (signed) and vertical distance (positive)
			// from the singularities to the left endpoint.  If we are going from right,
			// to left, reverse the sense of horizontal.
			double sgnd = sign(d);
			double dx[] = new double[n];
			double dy[] = new double[n];
			for (i = 0; i < n; i++) {
				dx[i] = (z[i][0] - za[0][0]) * sgnd;
				dy[i] = Math.abs(z[i][1] - za[0][1]);
			}
			
			// We have to be concerned with singularities lying to the right (left if
			// d < 0) of the left integration endpoint.
			boolean toright[] = new boolean[n];
			for (i = 0; i < n; i++) {
				toright[i] = (dx[i] > 0) && (!Double.isInfinite(z[i][0])) && (!Double.isInfinite(z[i][1]));
			}
			// For points with small enough dx, the limitation is purely due to dy.  For
			// others it must be calculated.
			boolean active[] = new boolean[n];
			int numactive = 0;
			for (i = 0; i < n; i++) {
				active[i] = (dx[i] > dy[i]/alf) && toright[i];
				if (active[i]) {
					numactive++;
				}
			}
			// Make sure that the left endpoint won't be included
			if (sng >= 0) {
				active[sng] = false;
			}
			
			// For those active, find the integration length constraint.  This comes
			// from making the sing/right-endpoint distance equal to alf*L.
			double x[] = new double[numactive];
			double y[] = new double[numactive];
			for (i = 0, j = 0; i < n; i++) {
				if (active[i]) {
					x[j] = dx[i];
					y[j++] = dy[i];
				}
			}
			double L[] = new double[numactive];
			double alfSquared = alf * alf;
			double denom = 1.0 - alfSquared;
			for (i = 0; i < numactive; i++) {
				L[i] = (x[i] - Math.sqrt(alfSquared*x[i]*x[i] - denom*y[i]*y[i]))/denom;
			}
			
			// What if the maximum allowable integration length?
			double Lmin = Double.MAX_VALUE;
			for (i = 0; i < numactive; i++) {
				if (L[i] < Lmin) {
					Lmin = L[i];
				}
			}
			for (i = 0; i < n; i++) {
				if (toright[i] && (!active[i])) {
					double div = dy[i]/alf;
					if (div < Lmin) {
						Lmin = div;
					}
				}
			}
			
			if (Lmin < Math.abs(d)) {
			    // Apply stquad on the safe part and recurse on the rest
			    sgnd = sign(d);
				zmid[0][0] = za[0][0] + Lmin * sgnd;
				zmid[0][1] = za[0][1];
				int sng1[] = new int[]{sng};
				double I1[][] = stquad(za, zmid, sng1, z, beta, qdat);
				I[k][0] = I1[0][0];
				I[k][1] = I1[0][1];
				int sngm1[] = new int[]{-1};
				double I2[][] = stquadh(zmid, zb, sngm1, z, beta, qdat);
				I[k][0] = I[k][0] + I2[0][0];
				I[k][1] = I[k][1] + I2[0][1];
			} // if (Lmin < Math.abs(d))
			else {
				// No restriction
				int sng1[] = new int[]{sng};
				double I3[][] = stquad(za, zb, sng1, z, beta, qdat);
				I[k][0] = I3[0][0];
				I[k][1] = I3[0][1];
			}
		} // for (kk = 0; kk < numnontriv; kk++) 
		return I;
	}
	
	
	
	public double[][] stquad(double z1[][], double z2[][], int sing1[], double z[][],
			double beta[], double qdat[][]) {
		// z1, z2, are vectors of left and right endpoints.  sing1 is a vector of integer
		// indices which label the singularities in z1.  So if sing1[5] = 3, then z1[5] = 3.
		// A -1 means no singularity.  z is the vector of *all* singularities, including the
		// "ends" of the strip at +- Infinity.  beta is the vector of associated turning angles.
		// qdat is quadrature data from scqdata.  It should include all the beta values, even
		// though the ends are never used in this manner.
		
		// stquad integrates form a possible singularity at the left end to a regular point
		// at the right.  If both endpoints are singularities, you must break the integral into
		// two pieces and make two calls.
		
		// The integral is subdivided, if necessary, so that no singularity lies closer to the
		// left endpoint than 1/2 the length of the integration (sub)interval.
		
		// Original MATLAB stquad routine copyright 1998 by Toby Driscoll.
		
		int i, j, k, kk;
		double za[] = new double[2];
		double zb[] = new double[2];
		int sng;
		double zr[] = new double[2];
		int ind;
		double nd[][] = new double[qdat.length][2];
		double wt[][] = new double[qdat.length][2];
		double c[] = new double[]{1,0};
		double cr[] = new double[1];
		double ci[] = new double[1];
		double zl[] = new double[2];
		int n = z.length;
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		}
		double I[][] = new double[z1.length][2];
		
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numnontriv++;
			}
		}
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				nontriv[j++] = i;
			}	
		}
		
		
		for (kk = 0; kk < numnontriv; kk++) {
			k = nontriv[kk];
			za[0] = z1[k][0];
			za[1] = z1[k][1];
			zb[0] = z2[k][0];
			zb[1] = z2[k][1];
			sng = sing1[k];
			
			// Allowable integration step, based on nearest singularity.
			double dist;
			double denom = zabs(zb[0] - za[0], zb[1] - za[1]);
			double minVal = Double.MAX_VALUE;
			for (j = 0; j < n; j++) {
				if (j != sng) {
					double val = zabs(z[j][0] - za[0], z[j][1] - za[1]);
					if (val < minVal) {
						minVal = val;
					}
				}
			}
			dist = Math.min(1.0, 2.0*minVal/denom);
			zr[0] = za[0] + dist * (zb[0] - za[0]);
			zr[1] = za[1] + dist * (zb[1] - za[1]);
			ind = (sng+n+1)%(n+1);
			// Adjust Gauss-Jacobi nodes and weights to interval.
			for (i = 0; i < qdat.length; i++) {
				for (j = 0; j < 2; j++) {
			        nd[i][j] = ((zr[j] - za[j])*qdat[i][ind] + zr[j] + za[j])/2.0;  // G-J nodes
			        wt[i][j] = ((zr[j] - za[j])/2.0) * qdat[i][ind+n+1];  // G-J weights
				}
			} // for (i = 0; i < qdat.length; i++)
			boolean anydiffzero = false;
			if ((nd[0][0] == za[0]) && (nd[0][1] == za[1])) {
				anydiffzero = true;
			}
			for (i = 0; i < qdat.length-1 && (!anydiffzero); i++) {
				if ((nd[i+1][0] == nd[i][0]) && (nd[i+1][1] == nd[i][1])) {
					anydiffzero = true;
				}
			}
			if ((zr[0] == nd[qdat.length-1][0]) && (zr[1] == nd[qdat.length-1][1])) {
				anydiffzero = true;
			}
			if (anydiffzero) {
				// Endpoints are practically coincident
				I[k][0] = 0;
				I[k][1] = 0;
			}
			else {
				// Use Gauss-Jacobi on first subinterval, if necessary.
				if (sng >= 0) {
					double base = zabs(zr[0] - za[0], zr[1] - za[1])/2.0;
					double val = Math.pow(base, beta[sng]);
					for (i = 0; i < qdat.length; i++) {
						wt[i][0] = wt[i][0] * val;
						wt[i][1] = wt[i][1] * val;
					}
				} // if (sng >= 0)
				double sde[][] = stderiv(nd, z, beta, c, sng);
				I[k][0] = 0;
				I[k][1] = 0;
				for (i = 0; i < qdat.length; i++) {
					zmlt(sde[i][0], sde[i][1], wt[i][0], wt[i][1], cr, ci);
					I[k][0] += cr[0];
					I[k][1] += ci[0];
				}
				while ((dist < 1) && (!Double.isNaN(I[k][0])) && (!Double.isNaN(I[k][1]))) {
				    // Do regular Gaussian quad on other subintervals.
					zl[0] = zr[0];
					zl[1] = zr[1];
					denom = zabs(zl[0] - zb[0], zl[1] - zb[1]);
					minVal = Double.MAX_VALUE;
					for (i = 0; i < n; i++) {
						double val = zabs(z[i][0] - zl[0], z[i][1] - zl[1]);
						if (val < minVal) {
							minVal = val;
						}
					} // for (i = 0; i < n; i++)
					dist = Math.min(1.0, 2.0*minVal/denom);
					zr[0] = zl[0] + dist * (zb[0] - zl[0]);
					zr[1] = zl[1] + dist * (zb[1] - zl[1]);
					for (i = 0; i < qdat.length; i++) {
						for (j = 0; j < 2; j++) {
					        nd[i][j] = ((zr[j] - zl[j])*qdat[i][n] + zr[j] + zl[j])/2.0;  // G-J nodes
					        wt[i][j] = ((zr[j] - zl[j])/2.0) * qdat[i][2*n+1];  // G-J weights
						}
					} // for (i = 0; i < qdat.length; i++)
					sde = stderiv(nd, z, beta, c, -1);
					for (i = 0; i < qdat.length; i++) {
						zmlt(sde[i][0], sde[i][1], wt[i][0], wt[i][1], cr, ci);
						I[k][0] += cr[0];
						I[k][1] += ci[0];
					}
				} // while ((dist < 1) && (!Double.isNaN(I[k][0])) && (!Double.isNaN(I[k][1])))
			} // else
		} // for (kk = 0; kk < numnontriv; kk++)
		return I;
	}
	
	
	
	public double[][] stderiv(double zp[][], double z[][], double beta[], double c[], int j) {
		// Derivative of the strip map
		// stderiv returns the derivative at the points of zp of the Schwarz-Christoffel
		// strip map defined by z, beta, and c.
		
		// Original MATLAB stderiv routine copyright 1998 by Toby Driscoll.
		
		// If the fifth argument j >= 0, the terms corresponding to z[j] are normalized
		// by abs(zp-z[j]).  This is for Gauss-Jacobi quadrature.
		int i, k, m;
		double theta = 0.0;
		double z2[][] = null;
		double beta2[] = null;
		int n;
		double terms[][][];
		double cr[] = new double[1];
		double ci[] = new double[1];
		
		double log2 = 0.69314718055994531;
		int npts = zp.length;
		double fprime[][] = new double[npts][2];
		
		// Strip out infinite prevertices
		if (z.length == beta.length) {
		    int numinf = 0;
		    for (i  = 0; i < z.length; i++) {
		    	if (Double.isInfinite(z[i][0]) || Double.isInfinite(z[i][1])) {
		    		numinf++;
		    	}
		    }
		    int ends[] = new int[numinf];
		    for (i  = 0, k = 0; i < z.length; i++) {
		    	if (Double.isInfinite(z[i][0]) || Double.isInfinite(z[i][1])) {
		    		ends[k++] = i;
		    	}
		    }
		    theta = beta[ends[1]] - beta[ends[0]];
		    if (z[ends[0]][0] < 0) {
		    	theta = -theta;
		    }
		    z2 = new double[z.length - numinf][2];
		    beta2 = new double[beta.length - numinf];
		    for (i = 0, k = 0; i < z.length; i++) {
		    	boolean keep = true;
		    	for (m = 0; m < numinf  && keep; m++) {
		    	    if (ends[m] == i) {
		    	    	keep = false;
		    	    }
		    	} // for (m = 0; m < numinf  && keep; m++)
		    	if (keep) {
		    		z2[k][0] = z[i][0];
		    		z2[k][1] = z[i][1];
		    		beta2[k++] = beta[i];
		    	}
		    } // for (i = 0, k = 0; i < z.length; i++)
		    // Adjust singularity index if given 
		    int decrementj = 0;
		    if (j > ends[0]) {
		    	decrementj++;
		    }
		    if (j > ends[1]) {
		    	decrementj++;
		    }
		    j = j - decrementj;
		} // if (z.length == beta.length)
		else {
			MipavUtil.displayError("Vector of prevertices must include +/- Infinity entries in stderiv");
			System.exit(-1);
		}
		n = z2.length;
		
		terms = new double[n][npts][2];
		for (i = 0; i < n; i++) {
			for (k = 0; k < npts; k++) {
				terms[i][k][0] = (-Math.PI/2.0)*(zp[k][0] - z2[i][0]);
				terms[i][k][1] = (-Math.PI/2.0)*(zp[k][1] - z2[i][1]);
			}
		}
		boolean lower[] = new boolean[n];
		for (i = 0; i < n; i++) {
			lower[i] = (z2[i][1] == 0);
		}
		for (i = 0; i < n; i++) {
			if (lower[i]) {
				for (k = 0; k < npts; k++) {
					terms[i][k][0] = -terms[i][k][0];
					terms[i][k][1] = -terms[i][k][1];
				}
			}
		} // for (i = 0; i < n; i++)
		double rt[][] = new double[n][npts];
		for (i = 0; i < n; i++) {
			for (k = 0; k < npts; k++) {
				rt[i][k] = terms[i][k][0];			
			}
		}
		int numbig = 0;
		int numnotbig = 0;
		boolean big[][] = new boolean[n][npts];
		for (i = 0; i < n; i++) {
			for (k = 0; k < npts; k++) {
				big[i][k] = (Math.abs(rt[i][k]) > 40.0);
				if (big[i][k]) {
					numbig++;
				}
				else {
					numnotbig++;
				}
			}
		} // for (i = 0; i < n; i++)
		if (numnotbig > 0) {
		    for (i = 0; i < n; i++) {
		    	for (k = 0; k < npts; k++) {
		    		if (!big[i][k]) {
		    			// sinh(x + iy) = (sinhx cosy) + i(coshx siny)
		    			double logargr = Math.cosh(terms[i][k][0])*Math.sin(terms[i][k][1]);
		    			double logargi = -Math.sinh(terms[i][k][0])*Math.cos(terms[i][k][1]);
		    			terms[i][k][0] = Math.log(zabs(logargr, logargi));
		    			terms[i][k][1] = Math.atan2(logargi, logargr);
		    		}
		    	}
		    }
		} // if (numnotbig > 0)
		
		for (i = 0; i < n; i++) {
			for (k = 0; k < npts; k++) {
				if (big[i][k]) {
					double sgn = sign(rt[i][k]);
					terms[i][k][0] = sgn * terms[i][k][0] - log2;
					terms[i][k][1] = sgn * (terms[i][k][1] - Math.PI/2.0);
				}
			}
		} // for (i = 0; i < n; i++)
		if (j >= 0) {
			for (k = 0; k < npts; k++) {
				terms[j][k][0] = terms[j][k][0] - Math.log(zabs(zp[k][0]-z2[j][0], zp[k][1] - z2[j][1]));
			}
		} // if (j >= 0)
		double sum[][] = new double[npts][2];
		for (k = 0; k < npts; k++) {
			for (i = 0; i < n; i++) {
				sum[k][0] += (terms[i][k][0] * beta2[i]);
				sum[k][1] += (terms[i][k][1] * beta2[i]);
			}
		}
		double argr[] = new double[npts];
		double argi[] = new double[npts];
		for (i = 0; i < npts; i++) {
			argr[i] = (Math.PI/2.0)*theta*zp[i][0] + sum[i][0];
			argi[i] = (Math.PI/2.0)*theta*zp[i][1] + sum[i][1];
		}
		for (i = 0; i < npts; i++) {
			double expb = Math.exp(argr[i]);
			double expr = expb * Math.cos(argi[i]);
			double expi = expb * Math.sin(argi[i]);
			zmlt(c[0], c[1], expr, expi, cr, ci);
			fprime[i][0] = cr[0];
			fprime[i][1] = ci[0];
		}
		return fprime;
	}
	
	public double sign(double d) {
		if (d > 0.0) {
			return 1.0;
		}
		else if (d == 0.0) {
			return 0.0;
		}
		else {
			return -1.0;
		}
	}
	
	private double[] sign(double dReal, double dImag) {
		// x/abs(x) for complex numbers
		double realResult[] = new double[1];
		double imagResult[] = new double[1];
		double absd = zabs(dReal, dImag);
		zdiv(dReal, dImag, absd, 0, realResult, imagResult);
		double result[] = new double[]{realResult[0], imagResult[0]};
		return result;
	}
	
	private void rptrnsfm(double z[][], double y[], int cnr[]) {
		// rptrnsfm not intended for calling directly by the user
		// Transformation optimization vars to prevertices for rectangle problem
		// Original MATLAB code copyright 1997 by Toby Driscoll.  Last updated 05/06/97
		int i, j;
		int n = y.length+3;
		//double z[][] = new double[n][2];
		for (i = 0; i < z.length; i++) {
			for (j = 0; j < 2; j++) {
				z[i][j] = 0.0;
			}
		}
		
		// Fill interior of long edges first
		double cumsum = 0.0;
		for (i = cnr[0]; i <= cnr[1] - 2; i++) {
		    cumsum += Math.exp(y[i]);
		    z[i+1][0] = cumsum;
		    z[i+1][1] = 0;
		}
		cumsum = 0.0;
		for (i = cnr[3]-3; i >= cnr[2]-1; i--) {
			cumsum += Math.exp(y[i]);
		    z[i+2][0] = cumsum;
		    z[i+2][1] = 1.0;	
		}
		
		// Find L
		double xr[] = new double[2];
		xr[0] = z[cnr[1]-1][0];
		xr[1] = z[cnr[2]+1][0];
		double meanxr = (xr[0] + xr[1])/2.0;
		double diff = (xr[1] - xr[0])/2.0;
		double diffSquared = diff * diff;
		double yexp = Math.exp(2*y[cnr[1]-1]);
		double mag = diffSquared + yexp;
		double sqrtmag = Math.sqrt(mag);
		z[cnr[1]][0] = meanxr + sqrtmag;
		z[cnr[1]][1] = 0.0;
		z[cnr[2]][0] = z[cnr[1]][0];
		z[cnr[2]][1] = 1 + z[cnr[1]][1];
		z[cnr[3]][0] = 0.0;
		z[cnr[3]][1] = 1.0;
		
		// Now fill in "short edges"
		double cp[] = new double[Math.max(0, 1 + cnr[2] - 2 - cnr[1]) + 1];
		cp[0] = 1;
		for (i = 1; i < cnr[2] - cnr[1]; i++) {
			cp[i] = cp[i-1] * Math.exp(-(y[cnr[1] + i-1]));
		}
		double flipudcp[] = new double[cp.length];
		for (i = 0; i < cp.length; i++) {
			flipudcp[i] = cp[cp.length-1-i];
		}
		double cumsumflip[] = new double[cp.length];
		cumsumflip[0] = flipudcp[0];
		for (i = 1; i < cp.length; i++) {
			cumsumflip[i] = cumsumflip[i-1] + flipudcp[i];
		}
		double flipcumsum[] = new double[cp.length];
		for (i = 0; i < cp.length; i++) {
			flipcumsum[i] = cumsumflip[cp.length-i-1];
		}
		double cumsumcp[] = new double[cp.length];
		cumsumcp[0] = cp[0];
		for (i = 1; i < cp.length; i++) {
			cumsumcp[i] = cumsumcp[i-1] + cp[i];
		}
		double x[] = new double[cp.length+1];
		x[0] = -flipcumsum[0];
		for (i = 1; i < cp.length; i++) {
			x[i] = cumsumcp[i-1] - flipcumsum[i];
		}
		x[cp.length] = cumsumcp[cp.length-1];
		double x2[] = new double[x.length-2];
		for (i = 0; i < x2.length; i++) {
			x2[i] = x[i+1]/x[cp.length];
		}
		boolean mask[] = new boolean[x2.length];
		for (i = 0; i < x2.length; i++) {
			if (Math.abs(x2[i]) < eps) {
				mask[i] = true;
			}
		}
		double u[][] = new double[x2.length][2];
		for (i = 0; i < x2.length; i++) {
			u[i][0] = x2[i];
			u[i][1] = 0;
		}
		for (i = 0; i < x2.length; i++) {
			if (!mask[i]) {
				u[i][0] = Math.log(Math.abs(x2[i]))/Math.PI;
				u[i][1] = Math.atan2(0, x2[i])/Math.PI;
			}
			else {
				u[i][0] = -z[cnr[1]][0]/eps;
				u[i][1] = -z[cnr[1]][1]/eps;
			}
		} // for (i = 0; i < x2.length; i++)
		for (i = cnr[1]+1; i <= cnr[2]-1; i++) {
			z[i][0] = z[cnr[1]][0] - u[i-(cnr[1]+1)][0];
			z[i][1] = u[i-(cnr[1]+1)][1];
		}
				 
		cp = new double[Math.max(0, 1 + n-4-(cnr[3]-2))+1+(cnr[0]-1)+1];
		cp[0] = 1;
		for (i = 1; i <= n - cnr[3] - 1; i++) {
			cp[i] = cp[i-1]*Math.exp(-y[i+cnr[3]-3]);
		}
		for (i = n - cnr[3]; i < n -cnr[3] + cnr[0]; i++) {
			cp[i] = cp[i-1]*Math.exp(-y[i-n+cnr[3]]);
		}
		flipudcp = new double[cp.length];
		for (i = 0; i < cp.length; i++) {
			flipudcp[i] = cp[cp.length-1-i];
		}
		cumsumflip = new double[cp.length];
		cumsumflip[0] = flipudcp[0];
		for (i = 1; i < cp.length; i++) {
			cumsumflip[i] = cumsumflip[i-1] + flipudcp[i];
		}
		flipcumsum = new double[cp.length];
		for (i = 0; i < cp.length; i++) {
			flipcumsum[i] = cumsumflip[cp.length-i-1];
		}
		cumsumcp = new double[cp.length];
		cumsumcp[0] = cp[0];
		for (i = 1; i < cp.length; i++) {
			cumsumcp[i] = cumsumcp[i-1] + cp[i];
		}
		x = new double[cp.length+1];
		x[0] = -flipcumsum[0];
		for (i = 1; i < cp.length; i++) {
			x[i] = cumsumcp[i-1] - flipcumsum[i];
		}
		x[cp.length] = cumsumcp[cp.length-1];
		x2 = new double[x.length-2];
		for (i = 0; i < x2.length; i++) {
			x2[i] = x[i+1]/x[cp.length];
		}
		mask = new boolean[x2.length];
		for (i = 0; i < x2.length; i++) {
			if (Math.abs(x2[i]) < eps) {
				mask[i] = true;
			}
		}
		u = new double[x2.length][2];
		for (i = 0; i < x2.length; i++) {
			u[i][0] = x2[i];
			u[i][1] = 0;
		}
		for (i = 0; i < x2.length; i++) {
			if (!mask[i]) {
				u[i][0] = Math.log(Math.abs(x2[i]))/Math.PI;
				u[i][1] = Math.atan2(0, x2[i])/Math.PI;
			}
			else {
				u[i][0] = -z[cnr[1]][0]/eps;
				u[i][1] = -z[cnr[1]][1]/eps;
			}
		} // for (i = 0; i < x2.length; i++)
		for (i = 0; i < n-cnr[3]-1; i++) {
			z[i+cnr[3]+1][0] = u[i][0];
			z[i+cnr[3]+1][1] = u[i][1];
		}
		for (i = n-cnr[3]; i < n - cnr[3] + cnr[0]; i++) {
			z[i-n+cnr[3]][0] = u[i-1][0];
			z[i-n+cnr[3]][1] = u[i-1][1];
		}
		
		return;
	}
	
	// Gauss-Jacobi quadrature data for SC Toolbox.
	// scqdata returns a matrix of quadrature data suitable for other SC routines.  
	// beta is a vector of turning angles corresponding to *finite* singularities (prevertices and,
	// for exterior map, the origin).
	// nqpts is the number of quadrature points per subinterval, roughly equal to -log10(error).
	
	// All the SC routines call this routine as needed, and the work required is small, so you will
	// probably never have to call this routine directly.
	// Original MATLAB code copyright 1998 by Toby Driscoll.
	public void scqdata(double qdat[][], double beta[], int nqpts) {
		int i, j;
	    int n = beta.length;
	    double qnode[][] = new double[nqpts][n+1];
	    double qwght[][] = new double[nqpts][n+1];
	    double z[] = new double[nqpts];
	    double w[] = new double[nqpts];
	    for (j = 0; j < n; j++) {
	    	if (beta[j] > -1.0) {
	    	    gaussj(z, w, nqpts, 0, beta[j]);
	    	    for (i = 0; i < nqpts; i++) {
	    	    	qnode[i][j] = z[i];
	    	    	qwght[i][j] = w[i];
	    	    }
	    	}
	    } // for (j = 0; j < n; j++)
	    gaussj(z, w, nqpts, 0, 0);
	    for (i = 0; i < nqpts; i++) {
	    	qnode[i][n] = z[i];
	    	qwght[i][n] = w[i];
	    }
	    //qdat = new double[nqpts][2*n+2];
	    for (i = 0; i < nqpts; i++) {
	    	for (j = 0; j < n+1; j++) {
	    		qdat[i][j] = qnode[i][j];
	    		qdat[i][j+n+1] = qwght[i][j];
	    	}
	    }
	    return;
	}
	
	// gaussj returns nodes and weights for Gauss-Jacobi integration.  z and w are n-vectors such that the integral from
	// x = -1 to x = +1 of f(x)*((1-x)^alf)*((1+x)^bet)dx
	// is approximated by sum(f(z) .* w).
	// Original MATLAB code copyright 1997 by Toby Driscoll and last updated 04/11/97.
	// Uses the Lanczos iteration connection to orthogonal polynomials.
	// Borrows heavily from gaussj out of scpack FORTRAN.
	// Calculate coeffs a, b of Lanczos recurrence relation (closed form is known).
	// Break out n = 1 specially to avoid possible divide by zero.
	private void gaussj(double z[], double w[], int n,  double alf, double bet) {
		int i;
		double a[] = new double[n];
		double b[] = new double[n-1];
	    double apb = alf + bet;
	    a[0] = (bet-alf)/(apb+2);
	    double var = apb+2.0;
	    b[0] = Math.sqrt(4.0*(1.0+alf)*(1.0+bet)/ ((apb+3.0)*var*var));
	    for (i = 2; i <= n; i++) {
	    	a[i-1] = apb * (bet-alf) /((apb+2*i)*(apb+2*i-2));
	    }
	    for (i=2; i <= n-1; i++) {
	    	var = apb+2*i;
	    	var = var * var;
	    	b[i-1] = Math.sqrt(4.0*i*(i+alf)*(i+bet)*(i+apb)/((var-1)*var));
	    }
	    // Find eigvals/eigvecs of tridiag "Ritz" matrix
	    double eigenvector[][] = new double[n][n];
        double eigenvalue[] = new double[n];
	    if (n > 1) {
	        double sum[][] = new double[n][n];
	        for (i = 0; i < n; i++) {
	        	sum[i][i] = a[i];
	        }
	        for (i = 0; i < n-1; i++) {
	        	sum[i][i+1] = b[i];
	        	sum[i+1][i] = b[i];
	        }
	        // MATLAB puts eigenvalues in increasing order.
	        // Eigenvalue.decompose also puts eigenvalues in increasing order.
	        Eigenvalue.decompose(sum, eigenvector, eigenvalue);
	    } // if (n > 1)
	    else {
	    	eigenvector[0][0] = 1;
	    	eigenvalue[0] = a[0];
	    }
	    
	    // Compute normalization (integral of w(x))
	    double result1[] = new double[1];
	    Gamma gamma1 = new Gamma(alf+1, result1);
	    gamma1.run();
	    double result2[] = new double[1];
	    Gamma gamma2 = new Gamma(bet+1, result2);
	    gamma2.run();
	    double result3[] = new double[1];
	    Gamma gamma3 = new Gamma(apb+2, result3);
	    gamma3.run();
	    double c = Math.pow(2.0, apb+1.0)*result1[0]*result2[0]/result3[0];
	    
	    // Return the values
	    double prew[] = new double[n];
	    for (i = 0; i < n; i++) {
	    	prew[i] = c * eigenvector[0][i] * eigenvector[0][i];
	    }
	    ArrayList<indexValueItem> valueList = new ArrayList<indexValueItem>();
	    for (i = 0; i < n; i++) {
	    	valueList.add(new indexValueItem(i, eigenvalue[i]));
	    }
	    Collections.sort(valueList, new indexValueComparator());
	    for (i = 0; i < n; i++) {
	    	z[i] = eigenvalue[valueList.get(i).getIndex()];
	    	w[i] = prew[valueList.get(i).getIndex()];
	    }
	}
	
	// sccheck checks polygon inputs to the Schwarz-Christoffel functions.
	// sccheck is used by the xxparam functions to check the validity of 
	// inputs describing the polygon to be mapped.  type is a string consisting
	// of the prefix to param ("d", "dp", etc.).  If errors are found, execution 
	// will terminate.  Sometimes the trouble has to do with how the parameter
	// problem is posed, which imposes a few nonobvious constraints.  The function
	// scfix is provided to automatically fix such difficulties, by renumbering or
	// perhaps adding vertices.  sccheck output is 1 if the problem is rectifiable
	// by scfix, 2 if warning status only.
	// Original MATLAB code copyright 1998 by Toby Driscoll.
	public int sccheck(String type, double w[][], double beta[], int aux[]) {
		int i;
	    int err;
	    int sumb;
	    double sumbeta;
	    int n = w.length;
	    boolean atinf[] = new boolean[beta.length];
	    for (i = 0; i < beta.length; i++) {
	    	if (beta[i] <= -1.0) {
	    		atinf[i] = true;
	    	}
	    }
	    err = 0;
	    
	    // Universal truths
	    if (beta.length != n) {
	    	MipavUtil.displayError("Mismatched angles and vertices");
	    	err = -1;
	    	return err;
	    }
	    for (i = 0; i < n; i++) {
	    	if ((beta[i] > 1) || (beta[i] <-3)) {
	    		MipavUtil.displayError("beta["+i+"] = " + beta[i] + " not in the required [-3,1] range");
	    		err = -1;
	    		return err;
	    	}
	    } // for (i = 0; i < n; i++)
	    
	    // Infinite vertices
	    if ((!type.equalsIgnoreCase("de")) && (!type.equalsIgnoreCase("cr"))) {
	        for (i = 0; i < n; i++) {
	        	if ((!atinf[i]) && ((Double.isInfinite(w[i][0])) || (Double.isInfinite(w[i][1])))) {
	        	    MipavUtil.displayError("Infinite vertex " + i + " illegally has an angle > -1");
	        	    err = -1;
	        	    return err;
	        	}
	            if (atinf[i] && (!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
	        	    MipavUtil.displayError("Finite vertex " + i + " illegally has an angle <= -1");	
	        	    err = -1;
	        	    return err;
	        	}
	        } // for (i = 0; i < n; i++)
	        for (i = 0; i < n-1; i++) {
	        	if (atinf[i] && atinf[i+1]) {
	        		MipavUtil.displayError(i + " and " + (i+1) + " are unallowed adjacent infinite vertices");
	        		err = -1;
	        		return err;
	        	}
	        } // for (i = 0; i < n-1; i++)
	    } // if ((!type.equalsIgnoreCase("de")) && (!type.equalsIgnoreCase("cr")))
	    else {
	    	for (i = 0; i < n; i++) {
	    		if (atinf[i]) {
	    			MipavUtil.displayError("vertex " + i + " illegally has a beta <= -1");
	    			err = -1;
	    			return err;
	    		}
	    		if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
	    			MipavUtil.displayError(i + " is an illegal infinite vertex");
	    			err = -1;
	    			return err;
	    		}
	    	} // for (i = 0; i < n; i++)
	    } // else
	    
	    if (type.equalsIgnoreCase("de")) {
	    	sumb = 2;
	    }
	    else {
	    	sumb = -2;
	    }
	    
	    // Orientation conventions
	    sumbeta = 0.0;
	    for (i = 0; i < n; i++) {
	    	sumbeta += beta[i];
	    }
	    if (Math.abs(sumbeta + sumb) < 1.0E-9) {
	    	MipavUtil.displayError("Vertices were probably given in the wrong order");
	    	err = 1;
	    	return err;
	    }
	    else if (Math.abs(sumbeta - sumb) > 1.0E-9) {
	    	MipavUtil.displayWarning("Angles do not sum to " + sumb);
	    	err = 2;
	    	return err;
	    }
	    
	    // Some finer points
	    if (type.equalsIgnoreCase("hp") || type.equalsIgnoreCase("d")) {
	        if (n < 3) {
	            MipavUtil.displayError("Polygon must have at least 3 vertices");
	            err = -1;
	            return err;
	        }
	        else if (Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]) || Double.isInfinite(w[1][0]) ||
	        		Double.isInfinite(w[1][1]) || Double.isInfinite(w[n-2][0]) || Double.isInfinite(w[n-2][1])) {
	        	MipavUtil.displayError("Infinite vertices must not be at positions 0, 1, or n-2");
	        	err = 1;
	        	return err;
	        }
	        else if ((Math.abs(beta[n-1]) < eps) || (Math.abs(beta[n-1] - 1) < eps)) {
	        	MipavUtil.displayError("Sides adjacent to w[n-1] must not be collinear");
	        	err = 1;
	        	return err;
	        }
	    } // if (type.equalsIgnoreCase("hp") || type.equalsIgnoreCase("d"))
	    else if (type.equalsIgnoreCase("cr")) {
	        if (n < 4) {
	        	MipavUtil.displayError("Polygon must have at least 4 vertices");
	        	err = -1;
	        	return err;
	        }
	    } // else if (type.equalsIgnoreCase("cr"))
	    else if (type.equalsIgnoreCase("de")) {
	        if (n < 2) {
	        	MipavUtil.displayError("polygon must have at least 2 vertices");
	        	err = -1;
	        	return err;
	        }
	        else if (((beta[n-1] == 0) || (beta[n-1] == 1)) && (n > 2)) {
	        	MipavUtil.displayError("Sides adjacent to w[n-1] must not be collinear");
	        	err = 1;
	        	return err;
	        }
	    } // else if (type.equalsIgnoreCase("de"))
	    else if (type.equalsIgnoreCase("st")) {
	        if (n < 5) {
	        	MipavUtil.displayError("Polygon must have at least 5 vertices");
	        	err = -1;
	        	return err;
	        }
	        double wrenum[][] = new double[n][2];
	        double betarenum[] = new double[n];
	        for (i = aux[0]; i < n; i++) {
	        	wrenum[i - aux[0]][0] = w[i][0];
	        	wrenum[i - aux[0]][1] = w[i][1];
	        	betarenum[i - aux[0]] = beta[i];
	        }
	        for (i = 0; i <= aux[0]-1; i++) {
	        	wrenum[n - aux[0] + i][0] = w[i][0];
	        	wrenum[n - aux[0] + i][1] = w[i][1];
	        	betarenum[n- aux[0] + i] = beta[i];
	        }
	        int renum[] = new int[n];
	        for (i = aux[0]; i < n; i++) {
	        	renum[i-aux[0]] = i;
	        }
	        for (i = 0; i <= aux[0] - 1; i++) {
	        	renum[n - aux[0] + i] = i;
	        }
	        int k = -1;
	        for (i = 0; i < n; i++) {
	        	if (renum[i] == aux[1]) {
	        		k = i + 1;
	        	}
	        } // for (i = 0; i < n; i++)
	        renum = null;
	        if (atinf[1] || atinf[2] || atinf[n-1]) {
	        	MipavUtil.displayError("Vertices at (w[aux[0] + [1, 2, -1]) must be finite");
	        	err = 1;
	        	return err;
	        }
	        else if (k - 2 < 2) {
	        	MipavUtil.displayError("There must be at least 2 vertices between ends 0 and 1");
	        	err = 1;
	        	return err;
	        }
	        else if (k == n) {
	        	MipavUtil.displayError("There must be at least one vertex between ends 1 and 0");
	        	err = 1;
	        	return err;
	        }
	    } // else if (type.equalsIgnoreCase("st"))
	    else if (type.equalsIgnoreCase("r")) {
	        int corner[] = new int[aux.length];	
	        for (i = 0; i < aux.length; i++) {
	        	corner[i] = aux[i];
	        }
	        double wrenum[][] = new double[n][2];
	        double betarenum[] = new double[n];
	        for (i = aux[0]; i < n; i++) {
	        	wrenum[i - aux[0]][0] = w[i][0];
	        	wrenum[i - aux[0]][1] = w[i][1];
	        	betarenum[i - aux[0]] = beta[i];
	        }
	        for (i = 0; i <= aux[0]-1; i++) {
	        	wrenum[n - aux[0] + i][0] = w[i][0];
	        	wrenum[n - aux[0] + i][1] = w[i][1];
	        	betarenum[n- aux[0] + i] = beta[i];
	        }
	        int offset = n - corner[0]; // corner - corner[0] so no need to add one
	        int corner2[] = new int[corner.length];
		    for (i = 0; i < corner.length; i++) {
		    	corner2[i] = ((corner[i] + offset) % n);
		    }
		    if (n < 4) {
		    	MipavUtil.displayError("Polygon must have at least 4 vertices");
		    	err = -1;
		    	return err;
		    }
		    int sortcorner[] = new int[corner2.length];
		    for (i = 0; i < corner2.length; i++) {
		    	sortcorner[i] = corner2[i];
		    }
		    Arrays.sort(sortcorner);
		    for (i = 0; i < corner2.length; i++) {
		    	if (corner2[i] != sortcorner[i]) {
		    		// ccw order for y increasing going up
		    		// clockwise order for y increasing going down
		    		MipavUtil.displayError("Corners must be specified in clockwise order when y increases going down");
		    		//MipavUtil.displayError("Corners must be specified in ccw order");
		    		err = -1;
		    		return err;
		    	}
		    } // for (i = 0; i < corner.length; i++)
		    if (Double.isInfinite(wrenum[0][0]) || Double.isInfinite(wrenum[0][1])) {
		    	MipavUtil.displayError("Corner[0] must be finite");
		    	err = -1;
		    	return err;
		    }
		    if (Double.isInfinite(wrenum[1][0]) || Double.isInfinite(wrenum[1][1])) {
		        MipavUtil.displayError("Vertex corner[0] + 1 must be finite");
		        err = 1;
		        return err;
		    }
		    if ((Math.abs(betarenum[n-1]) < eps) || (Math.abs(betarenum[n-1] - 1) < eps)) {
	        	MipavUtil.displayError("Sides adjacent to w[corner[0]-1] must not be collinear");
	        	err = 1;
	        	return err;
	        }
	    } // else if (type.equalsIgnoreCase("r"))
	    return err;
	}
	
	public void scfix(double wn[][], double betan[], int verticesAdded[], int auxn[], String type, double w[][], double beta[], int aux[]) {
		// wn,betan, and auxn are output
		// w, beta, and type are input
		// aux is input
		// Fix polygon to meet Schwarz-Christoffel toolbox constraints.
		// scfix attempts to fix a problem in the given polygon that arises from the
		// posing of the parameter problem.  scfix is used when a call to xxparam results
		// in an error and so advises.  In this case the polygon as given violates some
		// fairly arbitrary constraint.  scifx remedies the situation by renumbering the
		// vertices, or, if necessary, adding a trivial (zero-turn) vertex.  type is one
		// of {"hp", "d", "de", "st", "r"}.  if an additional aux input and output argument
		// is given, it represents the indices of the strip ends or the rectangle corners.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll
		
		// You may wonder, why not let the xxparam functions call scfix automatically?  The 
		// trouble with that approach is that since a function can't modify its inputs in
		// the calling workspace, either the xxparam functions would have to return more
		// arguments, or the mapping and plotting functions also would have to detect and 
		// correct the problem every time they're called.  The problem is rare enough that this
		// method seems adequate.
		int i, j, k;
		int n = w.length;
		int renum[] = new int[n];
		for (i = 0; i < n; i++) {
			renum[i] = i;
		}
		int renumtemp[] = new int[n];
		
		// Orientation conventions
		int sumb;
		if (type.equalsIgnoreCase("de")) {
			sumb = 2;
		}
		else {
			sumb = -2;
		}
		double sumbeta = 0.0;
		for (i = 0; i < beta.length; i++) {
			sumbeta += beta[i];
		}
		if (Math.abs(sumbeta + sumb) < 1.0E-9) {
			// reverse order
			wn[0][0] = w[0][0];
			wn[0][1] = w[0][1];
			for (i = 1; i <= n-1; i++) {
				wn[i][0] = w[n-i][0];
				wn[i][1] = w[n-1][1];
			}
			for (i = 0; i < n; i++) {
				w[i][0] = wn[i][0];
				w[i][1] = wn[i][1];
			}
			
			beta = scangle(w);
			renumtemp[0] = renum[0];
			for (i = 1; i <= n-1; i++) {
				renumtemp[i] = renum[n-i];
			}
			for (i = 0; i < n; i++) {
				renum[i] = renumtemp[i];
			}
			if (aux != null) {
				int auxtemp[] = new int[n];
				auxtemp[0] = aux[0];
				for (i = 1; i <= n-1; i++) {
					auxtemp[i] = aux[n-i];
				}
				for (i = 0; i < n; i++) {
					aux[i] = auxtemp[i];
				}
				auxtemp = null;
			}
		} // if (Math.abs(sumbeta + sumb) < 1.0E-9)
		
		// Less obvious restrictions
		if (type.equalsIgnoreCase("hp") || type.equalsIgnoreCase("d")) {
		    int shift[] = new int[n];
		    for (i = 0; i < n-1; i++) {
		    	shift[i] = i+1;
		    }
		    shift[n-1] = 0;
		    // Remember, if necessary, to meet requirements:
		    // w([0,1,n-2]) finite && sides at w[n-1] not collinear
		    while (true) {
		    	boolean anyisinf = false;
		    	if (Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]) || Double.isInfinite(w[1][0]) ||
		    		Double.isInfinite(w[1][1]) || Double.isInfinite(w[n-2][0]) || Double.isInfinite(w[n-2][1])) {
		    		anyisinf = true;
		    	}
		    	if (!anyisinf && (Math.abs(beta[n-1]) >= eps) && (Math.abs(beta[n-1] - 1) >= eps)) {
		    		break;
		    	}
		        for (i = 0; i < n-1; i++) {
		        	renumtemp[i] = renum[i+1];
		        	wn[i][0] = w[i+1][0];
		        	wn[i][1] = w[i+1][1];
		        	betan[i] = beta[i+1];
		        }
		        renumtemp[n-1] = renum[0];
		        wn[n-1][0] = w[0][0];
		        wn[n-1][1] = w[0][1];
		        betan[n-1] = beta[0];
		        for (i = 0; i < n; i++) {
		        	renum[i] = renumtemp[i];
		        	w[i][0] = wn[i][0];
		        	w[i][1] = wn[i][1];
		        	beta[i] = betan[i];
		        }
		        if (renum[0] == 0) { // tried all orderings
		        	// First be sure beta[n-1] is no longer a problem.
		        	boolean betaproblem = true;
		        	for (i = 0; i < beta.length && betaproblem; i++) {
		        		if ((Math.abs(beta[i]-1.0) >= eps) && (Math.abs(beta[i]) >= eps)) {
		        			betaproblem = false;
		        		}
		        	} // for (i = 0; i < beta.length && betaproblem; i++)
		        	if (betaproblem) {
		        		System.err.println("Polygon has empty interior");
		        		return;
		        	}
		        	while ((Math.abs(beta[n-1]) < eps) || (Math.abs(beta[n-1] - 1) < eps)) {
		        		for (i = 0; i < n-1; i++) {
				        	wn[i][0] = w[i+1][0];
				        	wn[i][1] = w[i+1][1];
				        	betan[i] = beta[i+1];
				        }
				        wn[n-1][0] = w[0][0];
				        wn[n-1][1] = w[0][1];
				        betan[n-1] = beta[0];
				        for (i = 0; i < n; i++) {
				        	w[i][0] = wn[i][0];
				        	w[i][1] = wn[i][1];
				        	beta[i] = betan[i];
				        }	
		        	} // while ((Math.abs(beta[n-1]) < eps) || (Math.abs(beta[n-1] - 1) < eps))
		        	// Next, add one or two vertices as needed.
		        	if (Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]) || Double.isInfinite(w[1][0]) ||
		        			Double.isInfinite(w[1][1])) { 
		        	    scaddvtx(wn,betan,w,beta,0,null);
		        	    verticesAdded[0] = verticesAdded[0]+1;
		        	    w = new double[wn.length][2];
		        	    beta = new double[wn.length];
		        	    n = n+1;
		        	    for (i = 0; i < n; i++) {
		        	    	w[i][0] = wn[i][0];
		        	    	w[i][1] = wn[i][1];
		        	    	beta[i] = betan[i];
		        	    }
		        	} // if (Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]) || Double.isInfinite(w[1][0]) ||
		        	if (Double.isInfinite(w[n-2][0]) || Double.isInfinite(w[n-2][1])) {
		        		scaddvtx(wn,betan,w,beta,n-2,null);
		        		verticesAdded[0] = verticesAdded[0]+1;
		        		w = new double[wn.length][2];
			        	beta = new double[wn.length];
		        		n=n+1;
		        		for (i = 0; i < n; i++) {
		        	    	w[i][0] = wn[i][0];
		        	    	w[i][1] = wn[i][1];
		        	    	beta[i] = betan[i];
		        	    }
		        	} // if (Double.isInfinite(w[n-2][0]) || Double.isInfinite(w[n-2][1]))
		        	renum = new int[n];
		        	for (i = 0; i < n; i++) {
		        		renum[i] = i;
		        	}
		        	System.out.println("Warning: A vertex has been added");
		        	break;
		        } // if (renum[0] == 0)
		    } // while (true)
		} // if (type.equalsIgnoreCase("hp") || type.equalsIgnoreCase("d"))
		else if (type.equalsIgnoreCase("de")) {
			int shift[] = new int[n];
		    for (i = 0; i < n-1; i++) {
		    	shift[i] = i+1;
		    }
		    shift[n-1] = 0;
		    // Remember, if necessary, to ensure sides at w[n-1] not collinear
		    // (except if n==2, which is to be handled explicitly anyway)
		    while (true) {
		    	if (((Math.abs(beta[n-1]) >= eps) && (Math.abs(beta[n-1] - 1) >= eps)) || (n <= 2)) {
		    		break;
		    	}
		    	for (i = 0; i < n-1; i++) {
		        	renumtemp[i] = renum[i+1];
		        	wn[i][0] = w[i+1][0];
		        	wn[i][1] = w[i+1][1];
		        	betan[i] = beta[i+1];
		        }
		        renumtemp[n-1] = renum[0];
		        wn[n-1][0] = w[0][0];
		        wn[n-1][1] = w[0][1];
		        betan[n-1] = beta[0];
		        for (i = 0; i < n; i++) {
		        	renum[i] = renumtemp[i];
		        	w[i][0] = wn[i][0];
		        	w[i][1] = wn[i][1];
		        	beta[i] = betan[i];
		        }
		        if (renum[0] == 0) {
		        	wn = new double[2][2];
		        	betan = new double[2];
		        	for (i = 0, j = 0; i < beta.length; i++) {
		        		if (Math.abs(beta[i]) >= eps) {
		        		    wn[j][0] = w[i][0];
		        		    wn[j][1] = w[i][1];
		        		    betan[j++] = beta[i];
		        		}
		        	}
        		    renum = new int[2];
        		    renum[0] = 0;
        		    renum[1] = 1;
        		    n = 2;
        		    System.out.println("Polygon is a line segment); removing superfluous vertices");
        		    break;
		        } // if (renum[0] == 0)
		    } // while (true)
		} // else if (type.equalsIgnoreCase("de"))
		else if (type.equalsIgnoreCase("st")) {
			int ends[] = null;
			if ((aux != null) && (aux.length > 0)) {
			    ends = new int[aux.length];	
			    for (i = 0; i < aux.length; i++) {
			    	ends[i] = aux[i];
			    }
			} // if ((aux != null) && (aux.length > 0))
			else {
			    MipavUtil.displayInfo("Use mouse to select images of left and right ends of the strip.");
			    ends = scselect(w, beta, 2, null, null);
			}
			for (i = ends[0]; i < n; i++) {
				renum[i-ends[0]] = i;
				wn[i-ends[0]][0] = w[i][0];
				wn[i-ends[0]][1] = w[i][1];
				betan[i-ends[0]] = beta[i];
			}
			for (i = 0; i <= ends[0]-1; i++) {
				renum[n-ends[0]+i] = i;
				wn[n-ends[0]+i][0] = w[i][0];
				wn[n-ends[0]+i][1] = w[i][1];
				betan[n-ends[0]+i] = beta[i];
			}
			for (i = 0; i < n; i++) {
				w[i][0] = wn[i][0];
				w[i][1] = wn[i][1];
				beta[i] = betan[i];
			}
			for (k = 0; k < renum.length; k++) {
			    if (renum[k] == ends[1]) {
			    	break;
			    }
			} // for (k = 0; k < renum.length; k++)
			if (k < 3) {
				if (k < n-2) {
				    // Switch ends
					for (i = k; i < n; i++) {
						renum[i-k] = i;
						wn[i-k][0] = w[i][0];
						wn[i-k][1] = w[i][1];
						betan[i-k] = beta[i];
					}
					for (i = 0; i <= k-1; i++) {
						renum[n-k+i] = i;
						wn[n-k+i][0] = w[i][0];
						wn[n-k+i][1] = w[i][1];
						betan[n-k+i] = beta[i];
					}
					for (i = 0; i < n; i++) {
						w[i][0] = wn[i][0];
						w[i][1] = wn[i][1];
						beta[i] = betan[i];
					}
					for (k = 0; k < renum.length; k++) {
					    if (renum[k] == 0) {
					    	break;
					    }
					} // for (k = 0; k < renum.length; k++)
				} // if (k < n-2)
				else {
				    // Add one or two vertices.
					for (j = 1; j <= 4-(k+1); j++) {
					    scaddvtx(wn, betan, w, beta, j-1, null);
					    verticesAdded[0] = verticesAdded[0]+1;
					    w = new double[wn.length][2];
					    beta = new double[wn.length];
					    n = n+1;
					    for (i = 0; i < n; i++) {
					    	w[i][0] = wn[i][0];
					    	w[i][1] = wn[i][1];
					    	beta[i] = betan[i];
					    }
					    k = k+1;
					    System.out.println("Warning: A vertex has been added.");
					} // for (j = 1; j <= 4-(k+1); j++)
				} // else
			} // if (k < 3)
			
			if (k == n-1) {
				// Must add a vertex in any case
				 scaddvtx(wn, betan, w, beta, n-1, null);
				 verticesAdded[0] = verticesAdded[0]+1;
				 w = new double[wn.length][2];
			     beta = new double[wn.length];
			     n = n+1;
			     for (i = 0; i < n; i++) {
			    	 w[i][0] = wn[i][0];
			    	 w[i][1] = wn[i][1];
			    	 beta[i] = betan[i];
			     }
			     System.out.println("Warning: A vertex has been added.");
			} // if (k == n-1)
			
			if (Double.isInfinite(w[1][0]) || Double.isInfinite(w[1][1])) {
			    // Add two vertices
				for (j = 0; j <= 1; j++) {
					 scaddvtx(wn, betan, w, beta, j, null);
					 verticesAdded[0] = verticesAdded[0]+1;
					 w = new double[wn.length][2];
				     beta = new double[wn.length];
				     n = n+1;
				     k = k+1;
				     for (i = 0; i < n; i++) {
				    	 w[i][0] = wn[i][0];
				    	 w[i][1] = wn[i][1];
				    	 beta[i] = betan[i];
				     }
				     System.out.println("Warning: A vertex has been added.");	
				}
			} // if (Double.isInfinite(w[1][0]) || Double.isInfinite(w[1][1]))
			else if (Double.isInfinite(w[2][0]) || Double.isInfinite(w[2][1])) {
			    // Add one vertex.
				 scaddvtx(wn, betan, w, beta, 1, null);
				 verticesAdded[0] = verticesAdded[0]+1;
				 w = new double[wn.length][2];
			     beta = new double[wn.length];
			     n = n+1;
			     k = k+1;
			     for (i = 0; i < n; i++) {
			    	 w[i][0] = wn[i][0];
			    	 w[i][1] = wn[i][1];
			    	 beta[i] = betan[i];
			     }
			     System.out.println("Warning: A vertex has been added.");	
			} // else if (Double.isInfinite(w[2][0]) || Double.isInfinite(w[2][1]))
			else if (Double.isInfinite(w[n-1][0]) || Double.isInfinite(w[n-1][1])) {
			    // Add one vertex.
				 scaddvtx(wn, betan, w, beta, n-1, null);
				 verticesAdded[0] = verticesAdded[0]+1;
				 w = new double[wn.length][2];
			     beta = new double[wn.length];
			     n = n+1;
			     for (i = 0; i < n; i++) {
			    	 w[i][0] = wn[i][0];
			    	 w[i][1] = wn[i][1];
			    	 beta[i] = betan[i];
			     }
			     System.out.println("Warning: A vertex has been added.");	
			} // else if (Double.isInfinite(w[2][0]) || Double.isInfinite(w[2][1]))
			auxn[0] = 0;
			auxn[1] = k;
		} // else if (type.equalsIgnoreCase("st"))
		else if (type.equalsIgnoreCase("r")) {
		    int corner[] = new int[aux.length];
		    for (i = 0; i < aux.length; i++) {
		    	corner[i] = aux[i];
		    }
		    for (i = corner[0]; i < n; i++) {
				renum[i-corner[0]] = i;
				wn[i-corner[0]][0] = w[i][0];
				wn[i-corner[0]][1] = w[i][1];
				betan[i-corner[0]] = beta[i];
			}
			for (i = 0; i <= corner[0]-1; i++) {
				renum[n-corner[0]+i] = i;
				wn[n-corner[0]+i][0] = w[i][0];
				wn[n-corner[0]+i][1] = w[i][1];
				betan[n-corner[0]+i] = beta[i];
			}
			for (i = 0; i < n; i++) {
				w[i][0] = wn[i][0];
				w[i][1] = wn[i][1];
				beta[i] = betan[i];
			}
			int cornertemp[] = new int[corner.length];
			for (i = 0; i < corner.length; i++) {
				cornertemp[i] = (corner[i]-corner[0]+n) % n;
			} 
			for (i = 0; i < corner.length; i++) {
				corner[i] = cornertemp[i];
			}
			// Note: These problems are pretty rare.
			if ((Math.abs(beta[n-1]) <eps) || (Math.abs(beta[n-1]-1.0) < eps)) {
			    // Try swapping sides 1-2 and 3-4.
				if ((Math.abs(beta[corner[2]-1]) >= eps) && (Math.abs(beta[corner[2]-1]-1) >= eps) && (!Double.isInfinite(w[corner[2]][0]))
						&& (!Double.isInfinite(w[corner[2]][1]))) {
					for (i = corner[2]; i < n; i++) {
						renum[i-corner[2]] = i;
						wn[i-corner[2]][0] = w[i][0];
						wn[i-corner[2]][1] = w[i][1];
						betan[i-corner[2]] = beta[i];
					}
					for (i = 0; i <= corner[2]-1; i++) {
						renum[n-corner[2]+i] = i;
						wn[n-corner[2]+i][0] = w[i][0];
						wn[n-corner[2]+i][1] = w[i][1];
						betan[n-corner[2]+i] = beta[i];
					}
					for (i = 0; i < n; i++) {
						w[i][0] = wn[i][0];
						w[i][1] = wn[i][1];
						beta[i] = betan[i];
					}
					for (i = 0; i < corner.length; i++) {
						cornertemp[i] = (corner[i]-corner[2]+n) % n;
					} 
					Arrays.sort(cornertemp);
					for (i = 0; i < corner.length; i++) {
						corner[i] = cornertemp[i];
					}
				}
				else {
					System.err.println("Collinear sides make posing problem impossible");
					return;
				}
			} // if ((Math.abs(beta[n-1]) <eps) || (Math.abs(beta[n-1]-1.0) < eps))
			if (Double.isInfinite(w[1][0]) || Double.isInfinite(w[1][1])) {
				scaddvtx(wn, betan, w, beta, 0, null);
				verticesAdded[0] = verticesAdded[0]+1;
			    n = n+1;
			    for (i = 1; i <= 3; i++) {
			    	corner[i] = corner[i] + 1;
			    }
			} // if (Double.isInfinite(w[1][0]) || Double.isInfinite(w[1][1]))
			
			for (i = 0; i < corner.length; i++) {
				auxn[i] = corner[i];
			}
		} // else if (type.equalsIgnoreCase("r"))
	}
	
	private void scaddvtx(double wn[][], double betan[], double w[][], double beta[], int pos, double window[]) {
		// wn and betan are outputs of length n+1
		// w, beta, pos, and window are inputs. w and beta are of length n
		// Add a vertex to a polygon
		// scaddvtx adds a new vertex to the polygon described by w and beta immediately
		// after vertex pos.  If w[pos:pos+1] are finite, the new vertex is at the midpoint of an
		// edge; otherwise, the new vertex is a reasonable distance from its finite neighbor
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		double newv[] = new double[2];
		//int numadj;
		int i, j;
		int base = -1;
		boolean found;
		int rbase;
		double wreal;
		double wimag;
		int jrem;
		
		if (window == null) {
			window = new double[]{Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY,
					Double.POSITIVE_INFINITY};
		}
		int n = w.length;
		double ang[] = new double[n];
		double ang2[] = new double[n];
		if (pos < 0) {
			pos = n-1;
		}
		int pos1 = (pos+1)%n;
		if (isFinite(w[pos][0]) && isFinite(w[pos][1]) && isFinite(w[pos1][0]) &&
				isFinite(w[pos1][1])) {
			// Easy case
		    newv[0] = (w[pos][0] + w[pos1][0])/2.0;	
		    newv[1] = (w[pos][1] + w[pos1][1])/2.0;
		}
		else {
			// Messy case
			// Find a pair of adjacent finite vertices as a basis.
			//numadj = 0;
			found = false;
			for (i = 0; i < n-1 && (!found); i++) {
				if (isFinite(w[i][0]) && isFinite(w[i][1]) && isFinite(w[i+1][0]) &&
						isFinite(w[i+1][1])) {	
				    found = true;
				    base = i;
				}
			}
			if (!found) {
				if (isFinite(w[n-1][0]) && isFinite(w[n-1][1]) && isFinite(w[0][0]) &&
						isFinite(w[0][1])) {	
				    found = true;
				    base = n-1;
				}
			} // if (!found)
			rbase = (base+1) % n;
			wreal = w[rbase][0] - w[base][0];
			wimag = w[rbase][1] - w[base][1];
			ang[base] = Math.atan2(wimag, wreal);
			
			// Determine the absolute angle of side pos->pos1.
			boolean doloop = true;
			for (j = base+1; j < n; j++) {
			    jrem =( j-1 + n) % n;
			    ang[j] = ang[jrem] - Math.PI * beta[j];
			    if (j == pos) {
			    	doloop = false;
			    	break;
			    }
			} // for (j = base+1; j < n; j++)
			if (doloop) {
				for (j = 0; j <= base-1; j++) {
					jrem =( j-1 + n) % n;
				    ang[j] = ang[jrem] - Math.PI * beta[j];
				    if (j == pos) {
				    	break;	
				    }
				} // for (j = 0; j <= base-1; j++) 
			} // if (doloop)
			
			// Find a nice side length
			double len[] = new double[n];
			for (i = 0; i < n-1; i++) {
				len[i] = zabs(w[i+1][0] - w[i][0], w[i+1][1] - w[i][1]);
			}
			len[n-1] = zabs(w[0][0] - w[n-1][0], w[0][1] - w[n-1][1]);
			double sum = 0.0;
			int numfinite = 0;
			for (i = 0; i < n; i++) {
				if (isFinite(len[i])) {
					numfinite++;
					sum += len[i];
				}
			} // for (i = 0; i < n; i++)
			double avglen = sum/numfinite;
			
			double wbase[] = new double[2];
			double dir[] = new double[2];
			if (Double.isInfinite(w[pos][0]) || Double.isInfinite(w[pos][1])) {
			    wbase[0] = w[pos1][0];
			    wbase[1] = w[pos1][1];
			    dir[0] = Math.cos(ang[pos] + Math.PI);
			    dir[1] = Math.sin(ang[pos] + Math.PI);
			}
			else {
				wbase[0] = w[pos][0];
				wbase[1] = w[pos][1];
				dir[0] = Math.cos(ang[pos]);
				dir[1] = Math.sin(ang[pos]);
			}
			
			// Restrict point to a window (to help out graphics)
			newv[0] = wbase[0] + avglen*dir[0];
			newv[1] = wbase[1] + avglen*dir[1];
			while ((newv[0] < window[0]) || (newv[0] > window[1]) || (newv[1] < window[2]) || (newv[1] > window[3])) {
				avglen = avglen/2;
				newv[0] = wbase[0] + avglen*dir[0];
				newv[1] = wbase[1] + avglen*dir[1];
			}
		} // else
		
		for (i = 0; i <= pos; i++) {
			wn[i][0] = w[i][0];
			wn[i][1] = w[i][1];
			betan[i] = beta[i];
		}
		wn[pos+1][0] = newv[0];
		wn[pos+1][1] = newv[1];
		betan[pos+1] = 0;
		for (i = pos+1; i <= n-1; i++) {
			wn[i+1][0] = w[i][0];
			wn[i+1][1] = w[i][1];
			betan[i+1] = beta[i];
		}
	}
	
	private class indexValueComparator implements Comparator<indexValueItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final indexValueItem o1, final indexValueItem o2) {
            final double a = o1.getValue();
            final double b = o2.getValue();
            final int c = o1.getIndex();
            final int d = o2.getIndex();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else if (c < d) {
            	return -1;
            }
            else if (c > d) {
            	return 1;
            }
            else {
            	return 0;
            }
        }

    }
	
	private class indexValueItem {
		private int index;
		private double value;
		
		public indexValueItem(int index, double value) {
			this.value = value;
			this.index = index;
		}
		
		public int getIndex() {
			return index;
		}
		
		public double getValue() {
			return value;
		}
	}
	
	private static final boolean isFinite(double d) {
	    return (!Double.isInfinite(d) && !Double.isNaN(d));
	}
	
}