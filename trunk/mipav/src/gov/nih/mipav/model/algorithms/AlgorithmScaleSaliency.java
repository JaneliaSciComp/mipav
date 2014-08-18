package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.Vector;

/**
 * Original source code in C and MATLAB is Copyright 1998-2004 by Timor Kadir Version 1.5
 * Kadir/Brady Feature detector (Scale Saliency) Code
 * For non-commercial use only
 * Ported by to Java by William Gandler
 * 
 * References:
 * 1.) Saliency, Scale, and Image Description Timor Kadir and Michael Brady.
       International Journal of Computer Vision. 45 (2):83-105, November 2001.
       
   2.) Scale, Saliency, and Scene Description Timor Kadir Department of Engineering Science
       Robotics Research Group, University of Oxford, Trinity Term, 2002.
       
   3.) An affine invariant salient region detector Timor Kadir, Andrew Zisserman, and 
       Michael Brady, European Conference on Computer Vision 2004. Pages 228 - 241.
 *
 */

public class AlgorithmScaleSaliency extends AlgorithmBase {
	
	private int startScale = 3;
	
    private int stopScale = 33;
    
    // Number of bins (set to 0 for Parzen window PDF estimation)
    private int nbins = 16;
    
    // sigma for Parzen window (if nbins = 0.  Only available on scalar data).
    private double sigma = 1.0;
    
    // Anti-aliased sampling (not available with Parzen windowing).
    private boolean antiAliasedSampling = false;
    
    // Threshold on saliency values
    private double wt = 0.5;
    
    // Threshold on inter-scale saliency
    private double yt = 0.0;
    
    private boolean fastplog = true;
    
    private static final int plogpres = 10000;
    
    private double plogpArr[] = null;
    
    private int xDim;
	private int yDim;
	private int sliceSize;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmScaleSaliency - default constructor.
     */
    public AlgorithmScaleSaliency() { }
    
    /**
     * 
     * @param srcImg Source image
     * @param startScale
     * @param stopScale
     * @param nbins Number of bins (set to 0 for Parzen window PDF estimation)
     * @param antiAliasedSampling (not available with Parzen windowing)
     * @param wt Threshold on saliency values
     * @param yt Threshold on inter-scale saliency
     */
    public AlgorithmScaleSaliency(ModelImage srcImg, int startScale, int stopScale, int nbins, double sigma,
    		                      boolean antiAliasedSampling, double wt, double yt) {
    	super(null, srcImg);
    	this.startScale = startScale;
    	this.stopScale = stopScale;
    	this.nbins = nbins;
    	this.antiAliasedSampling = antiAliasedSampling;
    	this.wt = wt;
    	this.yt = yt;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	
    	double imageMin;
    	double imageMax;
    	ModelImage quantizedImage = null;
    	AlgorithmChangeType algoChange;
    	boolean image25D = false;
    	int i;
    	double bestSaliency[][];
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Scale Saliency ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        imageMin = srcImage.getMin();
        imageMax = srcImage.getMax();
        // Quantize values to go from 0 to nbins-1
        if (nbins != 0) {
	    	quantizedImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "quantizedImage");
	    	algoChange = new AlgorithmChangeType(quantizedImage, srcImage, imageMin, imageMax, 0, nbins-1,image25D);
	    	algoChange.run();
	    	algoChange.finalize();
	    	algoChange = null;
        }
    	
    	if (fastplog) {
    	    for (i = 0; i < plogpres; i++) {
    	        plogpArr[i] = plogp((double)i/plogpres);	
    	    }
    	}
    	
    	if ((!srcImage.isColorImage()) && (!srcImage.isComplexImage())) {
    		// Input is scalar image
    		if (nbins != 0) {
    			// Don't use Parzen window
    			if (antiAliasedSampling) {
    				bestSaliency = calcSalScale1DAA(quantizedImage);
    			} // if (antiAliasedSampling)
    		} // if (nbins != 0)
    	} // if ((!srcImage.isColorImage()) && (!srcImage.isComplexImage()))
    } // runAlgorithm()
    
    private void AACirclePix(int pROIP[][], int pROIW[][]) {
    	int range;
    	int s;
    	Vector<Integer>pP;
    	Vector<Integer>pW;
    	int j;
    	int i;
    	double z;
    	double w;
    	
    	range = stopScale + 1;
    	pROIP = new int[stopScale+1][];
    	pROIW = new int[stopScale+1][];
    	pP = new Vector<Integer>();
    	pW = new Vector<Integer>();
    	for (s = startScale; s <= stopScale; s++) {
    	    for (j = -range; j <= range; j++) {
    	        for (i = -range; i <= range; i++) {
    	            z = Math.sqrt(i*i + j*j);
    	            w = ((1/(1.0 + Math.pow(z/s,42))) - (1/(1.0 + Math.pow(z/(s-1), 42))));
    	            if (w > 0.0001) {
    	            	pP.add(i+j*xDim);
    	            	pW.add((int)Math.round(w*1000));
    	            }
    	        }
    	    }
    	    pROIP[s] = new int[pP.size()];
    	    pROIW[s] = new int[pP.size()];
    	    for (j = 0; j < pP.size(); j++) {
    	    	pROIP[s][j] = pP.get(j);
    	    	pROIW[s][j] = pW.get(j);
    	    }
    	    pP.removeAllElements();
    	    pW.removeAllElements();
    	}
    }
    
    private double[][] calcSalScale1DAA(ModelImage image) {
        int histo1[];
        double histo2[];
        int pPNZ[];
        int s;
        int hs;
        int xStart;
        int xStop;
        int yStart;
        int yStop;
        double entropyArray[][] = null;
        double pEntropyArray[][] = null;
        double distArray[][] = null;
        double pDistArray[][] = null;
        double bestSaliency[][] = null;
        double pBestSaliency[][] = null;
        int peaks[];
        int ROIP[][] = null;
        int ROIW[][] = null;
        int lengths[];
        int sums[];
        int pROIP[][];
        int pROIW[][];
        int scale;
        int j;
        int range;
        Vector<Integer>pP;
    	Vector<Integer>pW;
    	int i;
    	double z;
    	double w;
        
        histo1 = new int[nbins];
        histo2 = new double[nbins];
        pPNZ = new int[nbins];
        
        s = 2*(stopScale + 1) + 1;
        hs = (s-1)/2;
        xStart = hs;
        xStop = xDim - hs;
        
        yStart = xStart;
        yStop = yDim - hs;
        
        // Setup entropy output array
        entropyArray = new double[1][stopScale+1];
        pEntropyArray = entropyArray;
        
        // Setup distance output array
        distArray = new double[1][stopScale+1];
        pDistArray = distArray;
        
        // Create output matrix of bestscales and their saliency values
        bestSaliency = new double[6][100000];
        pBestSaliency = bestSaliency;
        
        peaks = new int[stopScale];
        range = stopScale + 1;
    	ROIP = new int[stopScale+1][];
    	ROIW = new int[stopScale+1][];
    	pP = new Vector<Integer>();
    	pW = new Vector<Integer>();
    	for (s = startScale; s <= stopScale; s++) {
    	    for (j = -range; j <= range; j++) {
    	        for (i = -range; i <= range; i++) {
    	            z = Math.sqrt(i*i + j*j);
    	            w = ((1/(1.0 + Math.pow(z/s,42))) - (1/(1.0 + Math.pow(z/(s-1), 42))));
    	            if (w > 0.0001) {
    	            	pP.add(i+j*xDim);
    	            	pW.add((int)Math.round(w*1000));
    	            }
    	        }
    	    }
    	    ROIP[s] = new int[pP.size()];
    	    ROIW[s] = new int[pP.size()];
    	    for (j = 0; j < pP.size(); j++) {
    	    	ROIP[s][j] = pP.get(j);
    	    	ROIW[s][j] = pW.get(j);
    	    }
    	    pP.removeAllElements();
    	    pW.removeAllElements();
    	}
        lengths = new int[stopScale+1];
        sums = new int[stopScale+1];
        pROIP = new int[stopScale+1][];
        pROIW = new int[stopScale+1][];
        
        for (scale = startScale; scale <= stopScale; scale++) {
            lengths[scale] = ROIP[scale].length;
            pROIP[scale] = ROIP[scale];
            pROIW[scale] = ROIW[scale];
            for (j = 0; j < lengths[scale]; j++) {
                sums[scale] += pROIW[scale][j];	
            }
        } // for (scale = startScale; scale <= stopScale; scale++)
        
        return bestSaliency;
    } // private double[][] calcSalScale1DAA(ModelImage image)
    
    private double plogp(double p) {
        if (p == 0) {
        	return 0.0;
        }
        else {
        	return(p * Math.log(p));
        }
    } // private doble plogp(double p)
}