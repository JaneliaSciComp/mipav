package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.IOException;
import java.util.*;

public class AlgorithmActiveContoursWithoutEdges extends AlgorithmBase  {
	
	/*
	 * Reference: 1.) Active Contours Without Edges by Tony F. Chan and 
	 *                Luminita A. Vese, IEEE Transactions on Image Processing,
	 *                Vol. 10, No. 2, February, 2001, pp. 266- 277.
	 */
	
	// The length parameter which has a scaling role
	// If we have to detect all or as many objects as possible and of any size, then mu
	// should be small.  If we have to detect only larger objects (for example objects
	// formed by grouping), and to not detect smaller objects (like points, due to the
	// noise), then mu has to be larger.
	// In reference 1 mu varies from 3.3E-6 * 255 * 255 to 2 * 255 * 255
	private double mu;
	
	private double epsilon = 1.0;
	
	// The step space
	private double h = 1.0;
	
	// The time step
	// A value of delt = 0.01 was used in 1 example in reference 1.
	private double delt = 0.1;
	
	private double lambda1 = 1.0;
	
	private double lambda2 = 1.0;
	
	// Area parameter was set to 0.02 * 255 * 255 in 1 example in reference 1
	private double nu = 0.0;
	
	// One example in reference 1 had 1 iteration of reinitialization and 4
	// examples had 5 iterations of reinitialization.
	private int reinitializations = 0;

	
	public AlgorithmActiveContoursWithoutEdges(ModelImage srcImg, double mu, double delt,
			                                   double nu, int reinitializations) {
		super(null, srcImg);
		this.mu = mu;
		this.delt = delt;
		this.nu = nu;
		this.reinitializations = reinitializations;
	}
	
	public void runAlgorithm() {
		int i;
		int nVOI;
        ViewVOIVector VOIs;
        VOI cVOI;
        VOIContour contour;
        int xDim;
        int yDim;
        int sliceSize;
        double buffer[];
        boolean snear[] = new boolean[1];
		int i1[] = new int[1];
		int i2[] = new int[1];
		double phi[];
		double nextPhi[];
		double H[];
		int x;
		int y;
		int index;
		double c1NumCount;
	    double c1DenomCount;
		double c1;
		double c2NumCount;
		double c2DenomCount;
		double c2;
		boolean convergence;
		double phixplus;
		double phixminus;
		double phiyplus;
		double phiyminus;
		double delh;
		double hsquared;
		double phixdiff;
		double phiydiff;
        
        fireProgressStateChanged(srcImage.getImageName(), "Evolving the level set ...");
		
		VOIs = srcImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }

        cVOI = VOIs.VOIAt(i);
        contour = (VOIContour)cVOI.getCurves().elementAt(0);
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        buffer = new double[sliceSize];
        try {
        	srcImage.exportData(0, sliceSize, buffer);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, sliceize, buffer)");
        	setCompleted(false);
        	return;
        }
        
        phi = new double[sliceSize];
        nextPhi = new double[sliceSize];
        H = new double[sliceSize];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        	    index = x + y * xDim;
        	    phi[index] = contour.pinpol((double)x, (double)y, snear, i1, i2);
        	    H[index] = 0.5 * (1 + (2.0/Math.PI)*Math.atan(phi[index]/epsilon));
        	}
        }
        
        c1NumCount = 0.0;
        c1DenomCount = 0;
        c2NumCount = 0.0;
        c2DenomCount = 0;
        for (i = 0; i < sliceSize; i++) {
        	if (phi[i] >= 0.0) {
        		// On voi boundary or in voi
        		c1NumCount += (buffer[i]*H[i]);
        		c1DenomCount += H[i];
        	}
        	else {
        		// Outside voi boundary
        		c2NumCount += (buffer[i] * (1.0 - H[i]));
        		c2DenomCount += (1.0 - H[i]);
        	}
        } // for (i = 0; i < sliceSize; i++)
        c1 = c1NumCount/c1DenomCount;
        c2 = c2NumCount/c2DenomCount;
        
        convergence = false;
        while (!convergence) {
            for (y = 1; y < yDim - 1; y++) {
            	for (x = 1; x < xDim - 1; x++) {
            		index = x + y * xDim;
            		phixminus = phi[index] - phi[index-1];
            		phixplus = phi[index+1] - phi[index];
            		phiyminus = phi[index] - phi[index - xDim];
            		phiyplus = phi[index + xDim] - phi[index];
            		delh = (1.0/(Math.PI))*(epsilon/((epsilon*epsilon) + (phi[index]*phi[index])));
            		hsquared = h * h;
            		phixdiff = phi[index+1] - phi[index-1];
            		phiydiff = phi[index+xDim] - phi[index-xDim];
            	}
            }
        } // while (!convergence)
		
	}
}