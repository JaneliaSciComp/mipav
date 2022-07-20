package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGrays;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

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
	
	private static final int NORMAL_MODE = 1;
	
	private static final int PARZEN_WINDOW_MODE = 2;
	
	private static final int ANTI_ALIASED_SAMPLING_MODE = 3;
	
	private int mode = NORMAL_MODE;
	
	// Minimum scale
	private int startScale = 3;
	
    // Maximum scale
	private int stopScale = 33;
    
    // Number of bins for histogram (set to 256 for Parzen window PDF estimation)
    private int nbins = 16;
    
    // sigma for Parzen window (has nbins = 256.  Only available on scalar data).
    private double sigma = 1.0;
    
    // Threshold on inter-scale saliency values (set between 0 and 2)
    // Setting wt high forces the selection of features that are more scale localized or isotropic.
    private double wt = 0.5;
    
    // Fraction of saliency maximum value used as threshold
    private double yt = 0.5;
    
    private boolean fastplog = false;
    
    private static final int plogpres = 10000;
    
    private double plogpArray[] = null;
    
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
     * @param mode
     * @param nbins Number of bins (has 256 for Parzen window PDF estimation)
     * @param sigma for Parzen window
     * @param wt Threshold on inter-scale saliency values
     * @param yt Fraction of saliency maximum value used as threshold
     */
    public AlgorithmScaleSaliency(ModelImage srcImg, int startScale, int stopScale, int mode, int nbins, double sigma,
    		                      double wt, double yt) {
    	super(null, srcImg);
    	this.startScale = startScale;
    	this.stopScale = stopScale;
    	this.mode = mode;
    	this.nbins = nbins;
    	this.sigma = sigma;
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
    	Vector<sixItems> bestSaliency = null;
    	Vector<sixItems> C = null;
    	short imageBuffer[] = null;
    	short imageBuffer2[] = null;
    	short imageBuffer3[] = null;
    	int colorsFound = 0;
    	AlgorithmRGBtoGrays RGBtoGraysAlgo;
    	ModelImage destImageR;
    	ModelImage destImageG;
    	ModelImage destImageB;
    	double realBuffer[] = null;
    	double imagBuffer[] = null;
    	ModelImage realImage = null;
    	ModelImage imagImage = null;
    	int maxCirclePoints;
    	int j;
    	int xcenter;
    	int ycenter;
    	int radius;
    	double theta;
    	int x;
    	int y;
    	int lastx;
    	int lasty;
    	VOI circleVOI = null;
    	Vector<Vector3f> circleV= null;
    	Vector3f pt[] = null;
    	VOIVector VOIs = new VOIVector();
    	
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
        if ((!srcImage.isColorImage()) && (!srcImage.isComplexImage())) {
	        // Quantize values to go from 0 to nbins-1
	    	quantizedImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "quantizedImage");
	    	algoChange = new AlgorithmChangeType(quantizedImage, srcImage, imageMin, imageMax, 0, nbins-1,image25D);
	    	algoChange.run();
	    	algoChange.finalize();
	    	algoChange = null;
	    	imageBuffer = new short[sliceSize];
	    	try {
	    		quantizedImage.exportData(0, sliceSize, imageBuffer);
	    	} catch (IOException e) {
	            MipavUtil.displayError("IOException " + e + " on quantizedImage.exportData");
	
	            setCompleted(false);
	
	            return;
	        }
	    	quantizedImage.disposeLocal();
	    	quantizedImage = null;
        } // if ((!srcImage.isColorImage()) && (!srcImage.isComplexImage()))
        else if (srcImage.isColorImage()) {
        	destImageR = new ModelImage(ModelStorageBase.FLOAT,srcImage.getExtents(),"redImage");
        	destImageG = new ModelImage(ModelStorageBase.FLOAT,srcImage.getExtents(),"greenImage");
        	destImageB = new ModelImage(ModelStorageBase.FLOAT,srcImage.getExtents(),"blueImage");
        	RGBtoGraysAlgo = new AlgorithmRGBtoGrays(destImageR, destImageG, destImageB, srcImage);
        	RGBtoGraysAlgo.run();
        	RGBtoGraysAlgo.finalize();
        	RGBtoGraysAlgo = null;
            if (destImageR.getMin() != destImageR.getMax()) {
                colorsFound++;
                // Quantize values to go from 0 to nbins-1
    	    	quantizedImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "quantizedImage");
    	    	algoChange = new AlgorithmChangeType(quantizedImage, destImageR, destImageR.getMin(), 
    	    			                             destImageR.getMax(), 0, nbins-1,image25D);
    	    	algoChange.run();
    	    	algoChange.finalize();
    	    	algoChange = null;
    	    	imageBuffer = new short[sliceSize];
    	    	try {
    	    		quantizedImage.exportData(0, sliceSize, imageBuffer);
    	    	} catch (IOException e) {
    	            MipavUtil.displayError("IOException " + e + " on quantizedImage.exportData");
    	
    	            setCompleted(false);
    	
    	            return;
    	        }
            } // if (destImageR.getMin() != destImageR.getMax())
            destImageR.disposeLocal();
            destImageR = null;
            if (destImageG.getMin() != destImageG.getMax()) {
                colorsFound++;
                // Quantize values to go from 0 to nbins-1
                if (quantizedImage == null) {
    	    	    quantizedImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "quantizedImage");
                }
    	    	algoChange = new AlgorithmChangeType(quantizedImage, destImageG, destImageG.getMin(), 
    	    			                             destImageG.getMax(), 0, nbins-1,image25D);
    	    	algoChange.run();
    	    	algoChange.finalize();
    	    	algoChange = null;
    	    	if (colorsFound == 1) {
    	    	    imageBuffer = new short[sliceSize];
	    	    	try {
	    	    		quantizedImage.exportData(0, sliceSize, imageBuffer);
	    	    	} catch (IOException e) {
	    	            MipavUtil.displayError("IOException " + e + " on quantizedImage.exportData");
	    	
	    	            setCompleted(false);
	    	
	    	            return;
	    	        }
    	    	} // if (colorsFound == 1)
    	    	else {
    	    		imageBuffer2 = new short[sliceSize];
	    	    	try {
	    	    		quantizedImage.exportData(0, sliceSize, imageBuffer2);
	    	    	} catch (IOException e) {
	    	            MipavUtil.displayError("IOException " + e + " on quantizedImage.exportData");
	    	
	    	            setCompleted(false);
	    	
	    	            return;
	    	        }	
    	    	}
            } // if (destImageG.getMin() != destImageG.getMax())
            destImageG.disposeLocal();
            destImageG = null;
            if (destImageB.getMin() != destImageB.getMax()) {
                colorsFound++;
                // Quantize values to go from 0 to nbins-1
                if (quantizedImage == null) {
    	    	    quantizedImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "quantizedImage");
                }
    	    	algoChange = new AlgorithmChangeType(quantizedImage, destImageB, destImageB.getMin(), 
    	    			                             destImageB.getMax(), 0, nbins-1,image25D);
    	    	algoChange.run();
    	    	algoChange.finalize();
    	    	algoChange = null;
    	    	if (colorsFound == 1) {
    	    	    imageBuffer = new short[sliceSize];
	    	    	try {
	    	    		quantizedImage.exportData(0, sliceSize, imageBuffer);
	    	    	} catch (IOException e) {
	    	            MipavUtil.displayError("IOException " + e + " on quantizedImage.exportData");
	    	
	    	            setCompleted(false);
	    	
	    	            return;
	    	        }
    	    	} // if (colorsFound == 1)
    	    	else if (colorsFound == 2) {
    	    		imageBuffer2 = new short[sliceSize];
	    	    	try {
	    	    		quantizedImage.exportData(0, sliceSize, imageBuffer2);
	    	    	} catch (IOException e) {
	    	            MipavUtil.displayError("IOException " + e + " on quantizedImage.exportData");
	    	
	    	            setCompleted(false);
	    	
	    	            return;
	    	        }	
    	    	} // else if (colorsFound == 2)
    	    	else {
    	    		imageBuffer3 = new short[sliceSize];
	    	    	try {
	    	    		quantizedImage.exportData(0, sliceSize, imageBuffer3);
	    	    	} catch (IOException e) {
	    	            MipavUtil.displayError("IOException " + e + " on quantizedImage.exportData");
	    	
	    	            setCompleted(false);
	    	
	    	            return;
	    	        }		
    	    	}
            } // if (destImageB.getMin() != destImageB.getMax())
            quantizedImage.disposeLocal();
	    	quantizedImage = null;
            destImageB.disposeLocal();
            destImageB = null;
        } // else if (srcImage.isColorImage())
        else if (srcImage.isComplexImage()) {
        	realBuffer = new double[sliceSize];
        	imagBuffer = new double[sliceSize];
        	try {
                srcImage.exportDComplexData(0, sliceSize, realBuffer, imagBuffer);
        	} catch (IOException e) {
	            MipavUtil.displayError("IOException " + e + " on srcImage.exportDComplexData");
		    	
	            setCompleted(false);
	
	            return;
	        }
        	realImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), "realImage");
        	try {
        		realImage.importData(0, realBuffer, true);
        	} catch (IOException e) {
	            MipavUtil.displayError("IOException " + e + " on realImage.importData");
		    	
	            setCompleted(false);
	
	            return;
	        }
        	// Quantize values to go from 0 to nbins-1
	    	quantizedImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "quantizedImage");
	    	algoChange = new AlgorithmChangeType(quantizedImage, realImage, realImage.getMin(), 
	    			                             realImage.getMax(), 0, nbins-1,image25D);
	    	algoChange.run();
	    	algoChange.finalize();
	    	algoChange = null;
	    	realImage.disposeLocal();
	    	realImage = null;
	    	realBuffer = null;
	    	imageBuffer = new short[sliceSize];
	    	try {
	    		quantizedImage.exportData(0, sliceSize, imageBuffer);
	    	} catch (IOException e) {
	            MipavUtil.displayError("IOException " + e + " on quantizedImage.exportData");
	
	            setCompleted(false);
	
	            return;
	        }
	    	
	    	imagImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), "imagImage");
        	try {
        		imagImage.importData(0, realBuffer, true);
        	} catch (IOException e) {
	            MipavUtil.displayError("IOException " + e + " on imagImage.importData");
		    	
	            setCompleted(false);
	
	            return;
	        }
        	// Quantize values to go from 0 to nbins-1
	    	algoChange = new AlgorithmChangeType(quantizedImage, imagImage, imagImage.getMin(), 
	    			                             imagImage.getMax(), 0, nbins-1,image25D);
	    	algoChange.run();
	    	algoChange.finalize();
	    	algoChange = null;
	    	imagImage.disposeLocal();
	    	imagImage = null;
	    	imagBuffer = null;
	    	imageBuffer2 = new short[sliceSize];
	    	try {
	    		quantizedImage.exportData(0, sliceSize, imageBuffer2);
	    	} catch (IOException e) {
	            MipavUtil.displayError("IOException " + e + " on quantizedImage.exportData");
	
	            setCompleted(false);
	
	            return;
	        }
	    	quantizedImage.disposeLocal();
	    	quantizedImage = null;
        } // else if (srcImage.isComplexImage())
    	
    	if (fastplog) {
    		plogpArray = new double[plogpres + 1];
    	    for (i = 0; i <= plogpres; i++) {
    	        plogpArray[i] = plogp((double)i/plogpres);	
    	    }
    	}
    	
    	if ((!srcImage.isColorImage()) && (!srcImage.isComplexImage())) {
    		// Input is scalar image
    			if (mode == ANTI_ALIASED_SAMPLING_MODE) {
    				bestSaliency = calcSalScale1DAA(imageBuffer);
    			} 
    			else if (mode == NORMAL_MODE){
    				bestSaliency = calcSalScale1D(imageBuffer);	
    			}
    		    else {
    			    bestSaliency = calcSalScale1DParzen(imageBuffer);
    		    }
    	} // if ((!srcImage.isColorImage()) && (!srcImage.isComplexImage()))
    	else if ((srcImage.isComplexImage()) || (colorsFound == 2)) {
        	bestSaliency = calcSalScale2D(imageBuffer, imageBuffer2);
        } // if (colorsFound == 2)
    	else if (colorsFound == 3) {
    		bestSaliency = calcSalScale3D(imageBuffer, imageBuffer2, imageBuffer3);	
    	}
    	
    	C = greedyCluster(bestSaliency);
    	fireProgressStateChanged(95);
    	
    	for (i = 0; i < C.size(); i++) {
    		circleVOI =  new VOI((short)i, "circleVOI"+ String.valueOf(i), VOI.CONTOUR, -1 );
    		circleV = null;
    		circleV = new Vector<Vector3f>();
    		xcenter = C.get(i).getPointx();
    		ycenter = C.get(i).getPointy();
    	    radius = C.get(i).getCircleRadius();
    	    maxCirclePoints = 24 * radius;
    	    lastx = -1;
    	    lasty = -1;
    	    for (j = 0; j < maxCirclePoints; j++) {
                theta = j * 2.0 * Math.PI/maxCirclePoints;
                x = (int)Math.round(xcenter + radius*Math.cos(theta));
                y = (int)Math.round(ycenter + radius*Math.sin(theta));
                if ((x >= 0) && (x < xDim) && (y >= 0) && (y < yDim) && ((x != lastx) || (y != lasty))) {
                    lastx = x;
                    lasty = y;
                    circleV.add(new Vector3f(x, y, 0.0f));
                }
    	    } // for (j = 0; j < maxCirclePoints; j++)
    	    pt = null;
    	    pt = new Vector3f[circleV.size()];
        	for (j = 0; j < circleV.size(); j++) {
        		pt[j] = circleV.elementAt(j);
        	}
        	circleVOI.importCurve(pt);
        	VOIs.add(circleVOI);
    	} // for (i = 0; i < C.size(); i++)
    	srcImage.setVOIs(VOIs);
    	fireProgressStateChanged(100);
    	setCompleted(true);
    	return;
    	
    } // runAlgorithm()
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale1D(short imageBuffer[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int circleRadius;
    	double entropy;
    	double interScaleSaliency;
    	double saliency;
    	int histo1[];
        double histo2[];
        int s;
        int hs;
        int xStart;
        int xStop;
        int yStart;
        int yStop;
        double entropyArray[] = null;
        double distArray[] = null;
        int i;
        int peaks[];
        int maxScale;
        int center;
        Vector<Integer> ROIPix[] = null;
        int scale;
        double radius2;
        int j;
        double z;
        double prevRadius2 = -1.0;
        int lengths[];
        double currentProgress;
        double dprogress;
        int pointx;
        int pointy;
        int x;
        int y;
        int index;
        double sum;
        double tempNormBin;
        int counts;
        int numScales = 0;
        double norm;
        
        histo1 = new int[nbins];
        histo2 = new double[nbins];
        
        s = 2*stopScale + 1;
        hs = (s-1)/2;
        xStart = hs;
        xStop = xDim - hs;
        
        yStart = xStart;
        yStop = yDim - hs;
        
        // Setup entropy output array
        entropyArray = new double[stopScale+1];
        
        // Setup distance output array
        distArray = new double[stopScale+1];
        
        peaks = new int[stopScale];
        maxScale = 2*stopScale + 1;
        center = (maxScale+1)/2;
        
        ROIPix = (Vector<Integer>[])new Vector[stopScale+1];
        for (s = startScale; s <=stopScale; s++) {
            ROIPix[s] = new Vector<Integer>();
            scale = 2*s + 1;
            radius2 = Math.pow((scale-1)/2, 2);
            for (j = 1; j <= maxScale; j++) {
            	for (i = 1; i <= maxScale; i++) {
            	    z= (i-center)*(i-center) + (j-center)*(j-center);
            	    if (z <= radius2 && z > prevRadius2) {
            	    	ROIPix[s].add(i - 1 + (j-1)*xDim);
            	    }
            	} // for (i = 1; i <= maxScale; i++)
            } // for (j = 1; j <= maxScale; j++)
            prevRadius2 = radius2;
        } // for (s = startScale; s <=stopScale; s++)
        
        lengths = new int[stopScale+1];
        
        for (scale = startScale; scale <= stopScale; scale++) {
        	lengths[scale] = ROIPix[scale].size();
        }
        
        currentProgress = 0.0;
        dprogress = 1.0/(double)(xStop - xStart);
        
        for (pointx = xStart; pointx < xStop; pointx++) {
        	currentProgress += dprogress;
            fireProgressStateChanged((int)Math.round(90.0*currentProgress));
            x = pointx - hs;
            for (pointy = yStart; pointy < yStop; pointy++) {
                y = pointy - hs;
                index = x + y * xDim;
                sum = 0.0;
                for (scale = startScale; scale <= stopScale; scale++) {
                    for (i = 0; i < lengths[scale]; i++) {
                    	histo1[imageBuffer[index + ROIPix[scale].get(i)]]++;
                    }
                    sum += lengths[scale];
                    entropyArray[scale] = 0.0;
                    distArray[scale] = 0.0;
                    for (j = 0; j < nbins; j++) {
                        if (histo1[j] > 0) {
                            tempNormBin = (double)histo1[j]/sum;
                            if (fastplog) {
                                entropyArray[scale] -= plogpArray[(int)(tempNormBin * plogpres)];
                            }
                            else {
                            	entropyArray[scale] -= plogp(tempNormBin);
                            }
                            distArray[scale] += Math.abs(tempNormBin - histo2[j]);
                            histo2[j] = tempNormBin;
                        } // if (histo1[j] > 0)
                        else {
                        	distArray[scale] += histo2[j];
                        	histo2[j] = 0;
                        }
                    } // for (j = 0; j < nbins; j++)
                } // for (scale = startScale; scale <= stopScale; scale++)
                counts = 0;
                distArray[startScale] = distArray[startScale+1];
                
                // Smooth the distArray
                for (scale = startScale+1; scale < stopScale; scale++) {
                	distArray[scale] = (distArray[scale-1] + distArray[scale] + distArray[scale+1])/3;
                    if ((entropyArray[scale] > entropyArray[scale-1]) &&
                        (entropyArray[scale] > entropyArray[scale+1])) {
                    	peaks[counts] = scale;
                    	counts++;
                    }	
                } // for (scale = startScale+1; scale < stopScale; scale++)
                
                // Assign the best (peaks) scales and global saliency for this x, y location
                if (counts > 0) {
                	for (i = numScales; i < numScales + counts; i++) {
                		circleRadius = peaks[i-numScales];
                		norm = (circleRadius*circleRadius)/(2.0*circleRadius - 1.0);
                		entropy = entropyArray[circleRadius];
                		interScaleSaliency = distArray[circleRadius] * norm;
                		saliency = entropy * interScaleSaliency;
                		bestSaliency.add(new sixItems(pointx, pointy, circleRadius, entropy, interScaleSaliency, saliency));
                	} // for (i = numScales; i < numScales + counts; i++)
                } // if (counts > 0)
                numScales += counts;
                for (j = 0; j < nbins; j++) {
                	histo1[j] = 0;
                	histo2[j] = 0;
                }
            } // for (pointy = yStart; pointy < yStop; pointy++)
        } // for (pointx = xStart; pointx < xStop; pointx++)
        fireProgressStateChanged(90);
    	
    	return bestSaliency;
    }
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale1DAA(short imageBuffer[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int circleRadius;
    	double entropy;
    	double interScaleSaliency;
    	double saliency;
        int histo1[];
        double histo2[];
        int s;
        int hs;
        int xStart;
        int xStop;
        int yStart;
        int yStop;
        double entropyArray[] = null;
        double distArray[] = null;
        int peaks[];
        Vector<Integer> ROIP[] = null;
        Vector<Integer> ROIW[] = null;
        int lengths[];
        int sums[];
        int scale;
        int j;
        int range;
    	int i;
    	double z;
    	double w;
    	double dprogress;
    	double currentProgress;
    	int x;
    	int y;
    	int index;
    	double sum;
    	double tempNormBin;
    	int counts;
    	int numScales = 0;
    	double norm;
    	
        
        histo1 = new int[nbins];
        histo2 = new double[nbins];
        
        s = 2*(stopScale + 1) + 1;
        hs = (s-1)/2;
        xStart = hs;
        xStop = xDim - hs;
        
        yStart = xStart;
        yStop = yDim - hs;
        
        // Setup entropy output array
        entropyArray = new double[stopScale+1];
        
        // Setup distance output array
        distArray = new double[stopScale+1];
        
        peaks = new int[stopScale];
        range = stopScale + 1;
    	ROIP = (Vector<Integer>[])new Vector[stopScale+1];
    	ROIW = (Vector<Integer>[])new Vector[stopScale+1];
    	
    	for (s = startScale; s <= stopScale; s++) {
    		ROIP[s] = new Vector<Integer>();
    		ROIW[s] = new Vector<Integer>();
    	    for (j = -range; j <= range; j++) {
    	        for (i = -range; i <= range; i++) {
    	            z = Math.sqrt(i*i + j*j);
    	            w = ((1/(1.0 + Math.pow(z/s,42))) - (1/(1.0 + Math.pow(z/(s-1), 42))));
    	            if (w > 0.0001) {
    	            	ROIP[s].add(i+j*xDim);
    	            	ROIW[s].add((int)Math.round(w*1000));
    	            }
    	        }
    	    }
    	}
        lengths = new int[stopScale+1];
        sums = new int[stopScale+1];
        
        for (scale = startScale; scale <= stopScale; scale++) {
            lengths[scale] = ROIP[scale].size();
            for (j = 0; j < lengths[scale]; j++) {
                sums[scale] += ROIW[scale].get(j);	
            }
        } // for (scale = startScale; scale <= stopScale; scale++)
        
        currentProgress = 0.0;
        dprogress = 1.0/(double)(xStop - xStart);
        
        for (x = xStart; x < xStop; x++) {
            currentProgress += dprogress;
            fireProgressStateChanged((int)Math.round(90.0*currentProgress));
            for (y = yStart; y < yStop; y++) {
                index = x + y * xDim;
                sum = 0.0;
                for (scale = startScale; scale <= stopScale; scale++) {
                    for (i = 0; i < lengths[scale] - 1; i++) {
                    	histo1[imageBuffer[index + ROIP[scale].get(i)]] += ROIW[scale].get(i);
                    } 
                    sum += sums[scale];
                    entropyArray[scale] = 0.0;
                    distArray[scale] = 0.0;
                    for (j = 0; j < nbins; j++) {
                        if (histo1[j] > 0) {
                            tempNormBin = (double)histo1[j]/sum;
                            if (fastplog) {
                            	entropyArray[scale] -= plogpArray[(int)(tempNormBin * plogpres)];
                            }
                            else {
                            	entropyArray[scale] -= plogp(tempNormBin);
                            }
                            distArray[scale] += Math.abs(tempNormBin - histo2[j]);
                            histo2[j] = tempNormBin;
                        } // if (histo1[j] > 0)
                        else {
                        	distArray[scale] += histo2[j];
                        	histo2[j] = 0;
                        }
                    } // for (j = 0; j < nbins; j++)
                } // for (scale = startScale; scale <= stopScale; scale++)
                counts = 0;
                distArray[startScale] = distArray[startScale+1];
                
                for (scale = startScale+1; scale < stopScale; scale++) {
                    distArray[scale] = (distArray[scale-1] + distArray[scale] + distArray[scale+1])/3;
                    if ((entropyArray[scale] > entropyArray[scale-1]) &&
                        (entropyArray[scale] > entropyArray[scale+1])) {
                    	peaks[counts] = scale;
                    	counts++;
                    }
                } // for (scale = startScale+1; scale < stopScale; scale++)
                
                // Assign the best (peaks) scales and global saliency for this x, y location
                if (counts > 0) {
                	for (i = numScales; i < numScales + counts; i++) {
                		circleRadius = peaks[i-numScales];
                		norm = circleRadius;
                		entropy = entropyArray[circleRadius];
                		interScaleSaliency = distArray[circleRadius] * norm;
                		saliency = entropy * interScaleSaliency;
                		bestSaliency.add(new sixItems(x, y, circleRadius, entropy, interScaleSaliency, saliency));
                	} // for (i = numScales; i < numScales + counts; i++)
                } // if (counts > 0)
                numScales += counts;
                for (j = 0; j < nbins; j++) {
                	histo1[j] = 0;
                	histo2[j] = 0;
                }
            } // for (y = yStart; y < yStop; y++)
        } // for (x = xStart; x < xStop; x++)
        fireProgressStateChanged(90);
        
        return bestSaliency;
    } // private double[][] calcSalScale1DAA(ModelImage image)
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale1DParzen(short imageBuffer[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int circleRadius;
    	double entropy;
    	double interScaleSaliency;
    	double saliency;
    	int histo1[];
    	double histo2[];
    	double histoSmooth[];
    	int s;
    	int hs;
    	int xStart;
    	int xStop;
    	int yStart;
    	int yStop;
    	int size;
    	int mean;
    	double gauss[];
    	int i;
    	double entropyArray[];
    	double distArray[];
    	int peaks[];
    	int maxScale;
        int center;
    	Vector<Integer> ROIPix[] = null;
        int scale;
        double radius2;
        int j;
        double z;
        double prevRadius2 = -1.0;
        int lengths[];
        double currentProgress;
        double dprogress;
        int pointx;
        int pointy;
        int x;
        int y;
        int index;
        double sum;
        int sind;
        int b;
        double temp;
        int istart;
        int istop;
        int k;
        double tempNormBin;
        int counts;
        int numScales = 0;
        double norm;
    	
    	nbins = 256;
    	histo1 = new int[nbins];
        histo2 = new double[nbins];
        histoSmooth = new double[nbins];
        
        s = 2*stopScale + 1;
        hs = (s-1)/2;
        xStart = hs;
        xStop = xDim - hs;
        
        yStart = xStart;
        yStop = yDim - hs;
        
        // Create smoothing filter for Parzen post smoothing
        size = Math.max((int)(3.0 * sigma), 1);
        mean = size/2;
        gauss = new double[size];
        
        for (i = 0; i < size; i++) {
        	gauss[i] = (1.0/(sigma*Math.sqrt(2.0*Math.PI))) * Math.exp(-Math.pow((i - mean + 1)/sigma,2));
        }
        
        // Setup entropy output array
        entropyArray = new double[stopScale+1];
        
        // Setup distance output array
        distArray = new double[stopScale+1];
        
        peaks = new int[stopScale];
        maxScale = 2*stopScale + 1;
        center = (maxScale+1)/2;
        ROIPix = (Vector<Integer>[])new Vector[stopScale+1];
        for (s = startScale; s <=stopScale; s++) {
            ROIPix[s] = new Vector<Integer>();
            scale = 2*s + 1;
            radius2 = Math.pow((scale-1)/2, 2);
            for (j = 1; j <= maxScale; j++) {
            	for (i = 1; i <= maxScale; i++) {
            	    z= (i-center)*(i-center) + (j-center)*(j-center);
            	    if (z <= radius2 && z > prevRadius2) {
            	    	ROIPix[s].add(i - 1 + (j-1)*xDim);
            	    }
            	} // for (i = 1; i <= maxScale; i++)
            } // for (j = 1; j <= maxScale; j++)
            prevRadius2 = radius2;
        } // for (s = startScale; s <=stopScale; s++)
        
        lengths = new int[stopScale+1];
        
        for (scale = startScale; scale <= stopScale; scale++) {
        	lengths[scale] = ROIPix[scale].size();
        }
        
        currentProgress = 0.0;
        dprogress = 1.0/(double)(xStop - xStart);
        
        for (pointx = xStart; pointx < xStop; pointx++) {
        	currentProgress += dprogress;
            fireProgressStateChanged((int)Math.round(90.0*currentProgress));
            x = pointx - hs;
            for (pointy = yStart; pointy < yStop; pointy++) {
                y = pointy - hs;
                index = x + y * xDim;
                sum = 0.0;
                for (scale = startScale; scale <= stopScale; scale++) {
                    sind = scale - 1;
                    for (i = 0; i < lengths[scale]; i++) {
                    	histo1[imageBuffer[index + ROIPix[scale].get(i)]]++;
                    }
                    for (b = 0; b < 256; b++) {
                    	temp = 0.0;
                    	istart = -Math.min(0,  b-mean);
                    	istop = Math.min(2*mean,  256-b+mean-1);
                    	for (i = istart; i < istop; i++) {
                    		temp += (double)histo1[b + i - mean]*gauss[i];
                    	}
                    	histoSmooth[b] = temp;
                    } // for (b = 0; b < 256; b++)
                    sum = 0.0;
                    for (k = 0; k < nbins; k++) {
                    	sum += histo1[k];
                    }
                    entropyArray[sind] = 0.0;
                    distArray[sind] = 0.0;
                    for (k = 0; k < nbins; k++) {
                    	tempNormBin = (double)histoSmooth[k]/sum;
                    	if (fastplog) {
                    		entropyArray[sind] -= plogpArray[(int)(tempNormBin * plogpres)];
                    	}
                    	else {
                    		entropyArray[sind] -= plogp(tempNormBin);
                    	}
                    	distArray[sind]+= Math.abs(tempNormBin - histo2[k]);
                    	histo2[k] = tempNormBin;
                    } // for (k = 0; k < nbins; k++)
                } // for (scale = startScale; scale <= stopScale; scale++)
                counts = 0;
                // Smooth the distArray
                for (scale = startScale+1; scale < stopScale; scale++) {
                	distArray[scale] = (distArray[scale-1] + distArray[scale] + distArray[scale+1])/3;
                }
                
                for (scale = startScale; scale <= stopScale-2; scale++) {
                    if ((entropyArray[scale] < entropyArray[scale+1]) &&
                        (entropyArray[scale+1] > entropyArray[scale+2])) {
                    	peaks[counts] = scale+1;
                    	counts++;
                    }
                } // for (scale = startScale; scale <= stopScale-2; scale++)
                // Assign the best (peaks) scales and global saliency for this x, y location
                if (counts > 0) {
                	for (i = numScales; i < numScales + counts; i++) {
                		circleRadius = peaks[i-numScales] + 1;
                		norm = (circleRadius*circleRadius)/(2.0*circleRadius - 1);
                		entropy = entropyArray[peaks[i-numScales]];
                		interScaleSaliency = distArray[peaks[i - numScales]] * norm;
                		saliency = entropy * interScaleSaliency;
                		bestSaliency.add(new sixItems(pointx, pointy, circleRadius, entropy, interScaleSaliency, saliency));
                	} // for (i = numScales; i < numScales + counts; i++)
                } // if (counts > 0)
                numScales += counts;
                for (k = 0; k < nbins; k++) {
                	histo1[k] = 0;
                	histoSmooth[k] = 0;
                	histo2[k] = 0;
                }
            } // for (pointy = yStart; pointy < yStop; pointy++)
        } // for (pointx = xStart; pointx < xStop; pointx++)
        fireProgressStateChanged(90);
    	
    	return bestSaliency;
    }
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale2D(short imageBuffer[], short imageBuffer2[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int circleRadius;
    	double entropy;
    	double interScaleSaliency;
    	double saliency;
    	int binmax = 0;
    	int histo1[];
    	double histo2[];
    	int pPNZ[];
    	int s;
    	int hs;
    	int xStart;
    	int xStop;
    	int yStart;
    	int yStop;
    	double entropyArray[] = null;
        double distArray[] = null;
        int i;
        int peaks[];
        int maxScale;
        int center;
        Vector<Integer> ROIPix[] = null;
        int scale;
        double radius2;
        int j;
        double z;
        double prevRadius2 = -1.0;
        int lengths[];
        double currentProgress;
        double dprogress;
        int pointx;
        int pointy;
        int x;
        int y;
        int index;
        int sum;
        int sind;
        double isum;
        int ind = 0;
        double r;
        double tempNormBin;
        int counts;
        int numScales = 0;
        double norm;
    	
    	binmax = nbins * nbins;
    	histo1 = new int[binmax];
    	histo2 = new double[binmax];
    	pPNZ = new int[binmax];
    	
    	s = 2*stopScale + 1;
        hs = (s-1)/2;
        xStart = hs;
        xStop = xDim - hs;
        
        yStart = xStart;
        yStop = yDim - hs;
        
        // Setup entropy output array
        entropyArray = new double[stopScale+1];
        
        // Setup distance output array
        distArray = new double[stopScale+1];
        
        peaks = new int[stopScale];
        maxScale = 2*stopScale + 1;
        center = (maxScale+1)/2;
        
        ROIPix = (Vector<Integer>[])new Vector[stopScale+1];
        for (s = startScale; s <=stopScale; s++) {
            ROIPix[s] = new Vector<Integer>();
            scale = 2*s + 1;
            radius2 = Math.pow((scale-1)/2, 2);
            for (j = 1; j <= maxScale; j++) {
            	for (i = 1; i <= maxScale; i++) {
            	    z= (i-center)*(i-center) + (j-center)*(j-center);
            	    if (z <= radius2 && z > prevRadius2) {
            	    	ROIPix[s].add(i - 1 + (j-1)*xDim);
            	    }
            	} // for (i = 1; i <= maxScale; i++)
            } // for (j = 1; j <= maxScale; j++)
            prevRadius2 = radius2;
        } // for (s = startScale; s <=stopScale; s++)
        
        lengths = new int[stopScale+1];
        
        for (scale = startScale; scale <= stopScale; scale++) {
        	lengths[scale] = ROIPix[scale].size();
        }
        
        currentProgress = 0.0;
        dprogress = 1.0/(double)(xStop - xStart);
        
        for (pointx = xStart; pointx < xStop; pointx++) {
        	currentProgress += dprogress;
            fireProgressStateChanged((int)Math.round(90.0*currentProgress));
            x = pointx - hs;
            for (pointy = yStart; pointy < yStop; pointy++) {
                y = pointy - hs;
                index = x + y * xDim;
                sum = 0;
                for (scale = startScale; scale <= stopScale; scale++) {
                    sind = scale - 1;
                    for (i = 0; i < lengths[scale]; i++) {
                    	histo1[imageBuffer[index + ROIPix[scale].get(i)] + imageBuffer2[index + ROIPix[scale].get(i)]*nbins]++;
                    }
                    sum += lengths[scale];
                    entropyArray[sind] = 0;
                    distArray[sind] = 0;
                    isum = 1.0/sum;
                    for (j = 0; j < ind; j++) {
                    	if (histo1[pPNZ[j]] == 0) {
                    	    // this optimized so that it only does this when histo2 > 0 (a bin is in the PNZ)
                    		r = histo2[pPNZ[j]];
                    		r += distArray[sind];
                    		histo2[pPNZ[j]] = 0;
                    		distArray[sind] = r;
                    	} // if (histo1[pPNZ[j]] == 0)
                    } // for (j = 0; j < ind; j++)
                    ind = 0;
                    for (j = 0; j < binmax; j++) {
                        if (histo1[j] != 0) {
                        	r = histo1[j];
                        	pPNZ[ind++] = j;
                        	tempNormBin = r * isum;
                        	if (fastplog) {
                        		entropyArray[sind] -= plogpArray[(int)(tempNormBin * plogpres)];
                        	}
                        	else {
                        		entropyArray[sind] -= plogp(tempNormBin);
                        	}
                        	distArray[sind] += Math.abs(tempNormBin - histo2[j]);
                        	histo2[j] = tempNormBin;
                        } // if (histo1[j] != 0)
                    } // for (j = 0; j < binmax; j++)
                } // for (scale = startScale; scale <= stopScale; scale++)
                counts = 0;
                // Smooth the distArray
                for (scale = startScale+1; scale < stopScale; scale++) {
                	distArray[scale] = (distArray[scale-1] + distArray[scale] + distArray[scale+1])/3;
                }
                
                for (scale = startScale; scale <= stopScale-2; scale++) {
                    if ((entropyArray[scale] < entropyArray[scale+1]) &&
                        (entropyArray[scale+1] > entropyArray[scale+2])) {
                    	peaks[counts] = scale+1;
                    	counts++;
                    }
                } // for (scale = startScale; scale <= stopScale-2; scale++)
                // Assign the best (peaks) scales and global saliency for this x, y location
                if (counts > 0) {
                	for (i = numScales; i < numScales + counts; i++) {
                		circleRadius = peaks[i-numScales] + 1;
                		norm = (circleRadius*circleRadius)/(2.0*circleRadius - 1);
                		entropy = entropyArray[peaks[i-numScales]];
                		interScaleSaliency = distArray[peaks[i - numScales]] * norm;
                		saliency = entropy * interScaleSaliency;
                		bestSaliency.add(new sixItems(pointx, pointy, circleRadius, entropy, interScaleSaliency, saliency));
                	} // for (i = numScales; i < numScales + counts; i++)
                } // if (counts > 0)
                numScales += counts;
                for (j = 0; j < binmax; j++) {
                	histo1[j] = 0;
                	histo2[j] = 0;
                }
            } // for (pointy = yStart; pointy < yStop; pointy++)
        } // for (pointx = xStart; pointx < xStop; pointx++)
        fireProgressStateChanged(90);
    	
    	return bestSaliency;
    }
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale3D(short imageBuffer[], short imageBuffer2[], short imageBuffer3[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int circleRadius;
    	double entropy;
    	double interScaleSaliency;
    	double saliency;
    	int binmax = 0;
    	int histo1[];
    	double histo2[];
    	int pPNZ[];
    	int s;
    	int hs;
    	int xStart;
    	int xStop;
    	int yStart;
    	int yStop;
    	double entropyArray[] = null;
        double distArray[] = null;
        int i;
        int peaks[];
        int maxScale;
        int center;
        Vector<Integer> ROIPix[] = null;
        int scale;
        double radius2;
        int j;
        double z;
        double prevRadius2 = -1.0;
        int lengths[];
        double currentProgress;
        double dprogress;
        int pointx;
        int pointy;
        int x;
        int y;
        int index;
        int sum;
        int ind = 0;
        int sind;
        double r;
        double tempNormBin;
        int counts;
        int numScales = 0;
        double norm;
        
        binmax = nbins * nbins * nbins;
    	histo1 = new int[binmax];
    	histo2 = new double[binmax];
    	pPNZ = new int[binmax];
    	
    	s = 2*stopScale + 1;
        hs = (s-1)/2;
        xStart = hs;
        xStop = xDim - hs;
        
        yStart = xStart;
        yStop = yDim - hs;
        
        // Setup entropy output array
        entropyArray = new double[stopScale+1];
        
        // Setup distance output array
        distArray = new double[stopScale+1];
        
        peaks = new int[stopScale];
        maxScale = 2*stopScale + 1;
        center = (maxScale+1)/2;
        
        ROIPix = (Vector<Integer>[])new Vector[stopScale+1];
        for (s = startScale; s <=stopScale; s++) {
            ROIPix[s] = new Vector<Integer>();
            scale = 2*s + 1;
            radius2 = Math.pow((scale-1)/2, 2);
            for (j = 1; j <= maxScale; j++) {
            	for (i = 1; i <= maxScale; i++) {
            	    z= (i-center)*(i-center) + (j-center)*(j-center);
            	    if (z <= radius2 && z > prevRadius2) {
            	    	ROIPix[s].add(i - 1 + (j-1)*xDim);
            	    }
            	} // for (i = 1; i <= maxScale; i++)
            } // for (j = 1; j <= maxScale; j++)
            prevRadius2 = radius2;
        } // for (s = startScale; s <=stopScale; s++)
        
        lengths = new int[stopScale+1];
        
        for (scale = startScale; scale <= stopScale; scale++) {
        	lengths[scale] = ROIPix[scale].size();
        }
        
        currentProgress = 0.0;
        dprogress = 1.0/(double)(xStop - xStart);
        
        for (pointx = xStart; pointx < xStop; pointx++) {
        	currentProgress += dprogress;
            fireProgressStateChanged((int)Math.round(90.0*currentProgress));
            x = pointx - hs;
            for (pointy = yStart; pointy < yStop; pointy++) {
                y = pointy - hs;
                index = x + y * xDim;
                sum = 0;
                ind = 0;
                for (scale = startScale; scale <= stopScale; scale++) {
                    sind = scale - 1;
                    for (i = 0; i < lengths[scale]; i++) {
                    	histo1[imageBuffer[index + ROIPix[scale].get(i)] + (imageBuffer2[index + ROIPix[scale].get(i)]
                    			+ imageBuffer3[index + ROIPix[scale].get(i)]*nbins)*nbins]++;
                    }
                    sum += lengths[scale];
                    entropyArray[sind] = 0;
                    distArray[sind] = 0;
                    for (j = 0; j < ind; j++) {
                    	if (histo1[pPNZ[j]] == 0) {
                    	    // this optimized so that it only does this when histo2 > 0 (a bin is in the PNZ)
                    		r = histo2[pPNZ[j]];
                    		r += distArray[sind];
                    		histo2[pPNZ[j]] = 0;
                    		distArray[sind] = r;
                    	} // if (histo1[pPNZ[j]] == 0)
                    } // for (j = 0; j < ind; j++)
                    ind = 0;
                    for (j = 0; j < binmax; j++) {
                        if (histo1[j] != 0) {
                        	r = histo1[j];
                        	pPNZ[ind++] = j;
                        	tempNormBin = r/sum;
                        	if (fastplog) {
                        		entropyArray[sind] -= plogpArray[(int)(tempNormBin * plogpres)];
                        	}
                        	else {
                        		entropyArray[sind] -= plogp(tempNormBin);
                        	}
                        	distArray[sind] += Math.abs(tempNormBin - histo2[j]);
                        	histo2[j] = tempNormBin;
                        } // if (histo1[j] != 0)
                    } // for (j = 0; j < binmax; j++)
                } // for (scale = startScale; scale <= stopScale; scale++)
                counts = 0;
                // Smooth the distArray
                for (scale = startScale+1; scale < stopScale; scale++) {
                	distArray[scale] = (distArray[scale-1] + distArray[scale] + distArray[scale+1])/3;
                }
                
                for (scale = startScale; scale <= stopScale-2; scale++) {
                    if ((entropyArray[scale] < entropyArray[scale+1]) &&
                        (entropyArray[scale+1] > entropyArray[scale+2])) {
                    	peaks[counts] = scale+1;
                    	counts++;
                    }
                } // for (scale = startScale; scale <= stopScale-2; scale++)
                // Assign the best (peaks) scales and global saliency for this x, y location
                if (counts > 0) {
                	for (i = numScales; i < numScales + counts; i++) {
                		circleRadius = peaks[i-numScales] + 1;
                		norm = (circleRadius*circleRadius)/(2.0*circleRadius - 1);
                		entropy = entropyArray[peaks[i-numScales]];
                		interScaleSaliency = distArray[peaks[i - numScales]] * norm;
                		saliency = entropy * interScaleSaliency;
                		bestSaliency.add(new sixItems(pointx, pointy, circleRadius, entropy, interScaleSaliency, saliency));
                	} // for (i = numScales; i < numScales + counts; i++)
                } // if (counts > 0)
                numScales += counts;
                for (j = 0; j < binmax; j++) {
                	histo1[j] = 0;
                	histo2[j] = 0;
                }
            } // for (pointy = yStart; pointy < yStop; pointy++)
        } // for (pointx = xStart; pointx < xStop; pointx++)
        fireProgressStateChanged(90);
        
    	return bestSaliency;
    }
    
   	private Vector<sixItems> greedyCluster(Vector<sixItems> Y) {
    	// Greedy clusterer for salient feature post-processing
   		// It starts with the highest saliency feature and works down removing any features
   		// which are too close in (xpoint, ypoint).  Here, close means within the support of
   		// the current feature, i.e., its diameter.
       	Vector<sixItems> C = new Vector<sixItems>();
       	int j;
       	sixItems Yi;
       	double dists[];
       	double diffx;
       	double diffy;
       	double maxSaliency;
       	double saliencyThreshold;
       	
       	// Sort in descending saliency
       	// Y[5] are all positive and must be put in descending order
       	Collections.sort(Y, new sixItemsComparator());
        maxSaliency = Y.get(0).getSaliency();
        saliencyThreshold = yt * maxSaliency;
       	for (j = Y.size() - 1; j >= 0; j--) {
       		if (Y.get(j).getSaliency() < saliencyThreshold) {
       			Y.remove(j);
       		}
       	}
       	
       	for (j = Y.size() - 1; j >= 0; j--) {
       		if (Y.get(j).getInterScaleSaliency() <= wt) {
       			Y.remove(j);
       		}
       	}
       	
       	while (Y.size() > 0) {
       	    Yi = Y.get(0);	
       	    C.add(Yi);
       	    dists = null;
       	    dists = new double [Y.size()];
       	    for (j = 0; j < Y.size(); j++) {
       	    	diffx = Y.get(j).getPointx() - Yi.getPointx();
       	    	diffy = Y.get(j).getPointy() - Yi.getPointy();
       	    	dists[j] = Math.sqrt(diffx*diffx + diffy*diffy);
       	    }
       	    for (j = Y.size() - 1; j >= 0; j--) {
       	    	if (dists[j] <= Yi.getCircleRadius()) {
       	    		Y.remove(j);
       	    	}
       	    }
       	} // while (Y.size() > 0)
       	
       	return C;
    }
    
    private class sixItemsComparator implements Comparator<sixItems> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final sixItems o1, final sixItems o2) {
        	// Put in descending order
            final double a = o1.getSaliency();
            final double b = o2.getSaliency();

            if (a > b) {
                return -1;
            } else if (a < b) {
                return 1;
            } else {
                return 0;
            }
        }

    }
    
    private class sixItems {
    	private int pointx;
    	private int pointy;
    	private int circleRadius;
    	private double entropy;
    	private double interScaleSaliency;
    	private double saliency;
    	
    	public sixItems(int pointx, int pointy,int circleRadius, double entropy, double interScaleSaliency, double saliency) {
    		this.pointx = pointx;
    		this.pointy = pointy;
    		this.circleRadius = circleRadius;
    		this.entropy = entropy;
    		this.interScaleSaliency = interScaleSaliency;
    		this.saliency = saliency;
    	}
    	
    	public int getPointx() {
    		return pointx;
    	}
    	
    	public int getPointy() {
    		return pointy;
    	}
    	
    	public int getCircleRadius() {
    		return circleRadius;
    	}
    	
    	@SuppressWarnings("unused")
		public double getEntropy() {
    		return entropy;
    	}
    	
    	public double getInterScaleSaliency() {
    		return interScaleSaliency;
    	}
    	
    	public double getSaliency() {
    		return saliency;
    	}
    }
    
    private double plogp(double p) {
        if (p == 0) {
        	return 0.0;
        }
        else {
        	return(p * Math.log(p));
        }
    } // private double plogp(double p)
}