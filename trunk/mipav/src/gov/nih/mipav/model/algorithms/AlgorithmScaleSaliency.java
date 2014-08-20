package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGrays;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Comparator;
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
    
    // Number of bins (set to 256 for Parzen window PDF estimation)
    private int nbins = 16;
    
    // sigma for Parzen window (has nbins = 256.  Only available on scalar data).
    private double sigma = 1.0;
    
    // Anti-aliased sampling (not available with Parzen windowing).
    private boolean antiAliasedSampling = false;
    
    // Threshold on saliency values
    private double wt = 0.5;
    
    // Threshold on inter-scale saliency
    private double yt = 0.0;
    
    private boolean fastplog = true;
    
    private static final int plogpres = 10000;
    
    private double plogpArray[] = null;
    
    private boolean doParzen = false;
    
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
     * @param doParzen
     * @param nbins Number of bins (has 256 for Parzen window PDF estimation)
     * @param antiAliasedSampling (not available with Parzen windowing)
     * @param wt Threshold on saliency values
     * @param yt Threshold on inter-scale saliency
     */
    public AlgorithmScaleSaliency(ModelImage srcImg, int startScale, int stopScale, boolean doParzen, int nbins, double sigma,
    		                      boolean antiAliasedSampling, double wt, double yt) {
    	super(null, srcImg);
    	this.startScale = startScale;
    	this.stopScale = stopScale;
    	this.doParzen = doParzen;
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
    	Vector<sixItems> bestSaliency = null;
    	Vector<Double>[] C = null;
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
    	    	quantizedImage.disposeLocal();
    	    	quantizedImage = null;
            } // if (destImageR.getMin() != destImageR.getMax())
            destImageR.disposeLocal();
            destImageR = null;
            if (destImageG.getMin() != destImageG.getMax()) {
                colorsFound++;
                // Quantize values to go from 0 to nbins-1
    	    	quantizedImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "quantizedImage");
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
    	    	quantizedImage.disposeLocal();
    	    	quantizedImage = null;
            } // if (destImageG.getMin() != destImageG.getMax())
            destImageG.disposeLocal();
            destImageG = null;
            if (destImageB.getMin() != destImageB.getMax()) {
                colorsFound++;
                // Quantize values to go from 0 to nbins-1
    	    	quantizedImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "quantizedImage");
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
    	    	quantizedImage.disposeLocal();
    	    	quantizedImage = null;
            } // if (destImageB.getMin() != destImageB.getMax())
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
	    	quantizedImage.disposeLocal();
	    	quantizedImage = null;
	    	
	    	imagImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), "imagImage");
        	try {
        		imagImage.importData(0, realBuffer, true);
        	} catch (IOException e) {
	            MipavUtil.displayError("IOException " + e + " on imagImage.importData");
		    	
	            setCompleted(false);
	
	            return;
	        }
        	// Quantize values to go from 0 to nbins-1
	    	quantizedImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "quantizedImage");
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
    	    for (i = 0; i < plogpres; i++) {
    	        plogpArray[i] = plogp((double)i/plogpres);	
    	    }
    	}
    	
    	if ((!srcImage.isColorImage()) && (!srcImage.isComplexImage())) {
    		// Input is scalar image
    		if (!doParzen) {
    			// Don't use Parzen window
    			if (antiAliasedSampling) {
    				bestSaliency = calcSalScale1DAA(imageBuffer);
    			} // if (antiAliasedSampling)
    			else {
    				bestSaliency = calcSalScale1D(imageBuffer);	
    			}
    		} // if (!doParzen)
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
    	
    } // runAlgorithm()
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale1D(short imageBuffer[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int bestScale;
    	double HD;
    	double WD;
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
            fireProgressStateChanged((int)Math.round(100.0/currentProgress));
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
                		bestScale = peaks[i-numScales];
                		norm = (bestScale*bestScale)/(2.0*bestScale - 1.0);
                		HD = entropyArray[bestScale];
                		WD = distArray[bestScale] * norm;
                		saliency = HD * WD;
                		bestSaliency.add(new sixItems(pointx, pointy, bestScale, HD, WD, saliency));
                	} // for (i = numScales; i < numScales + counts; i++)
                } // if (counts > 0)
                numScales += counts;
                for (j = 0; j < nbins; j++) {
                	histo1[j] = 0;
                	histo2[j] = 0;
                }
            } // for (pointy = yStart; pointy < yStop; pointy++)
        } // for (pointx = xStart; pointx < xStop; pointx++)
        fireProgressStateChanged(100);
    	
    	return bestSaliency;
    }
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale1DAA(short imageBuffer[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int bestScale;
    	double HD;
    	double WD;
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
            fireProgressStateChanged((int)Math.round(100.0/currentProgress));
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
                		bestScale = peaks[i-numScales];
                		norm = bestScale;
                		HD = entropyArray[bestScale];
                		WD = distArray[bestScale] * norm;
                		saliency = HD * WD;
                		bestSaliency.add(new sixItems(x, y, bestScale, HD, WD, saliency));
                	} // for (i = numScales; i < numScales + counts; i++)
                } // if (counts > 0)
                numScales += counts;
                for (j = 0; j < nbins; j++) {
                	histo1[j] = 0;
                	histo2[j] = 0;
                }
            } // for (y = yStart; y < yStop; y++)
        } // for (x = xStart; x < xStop; x++)
        fireProgressStateChanged(100);
        
        return bestSaliency;
    } // private double[][] calcSalScale1DAA(ModelImage image)
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale1DParzen(short imageBuffer[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int bestScale;
    	double HD;
    	double WD;
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
            fireProgressStateChanged((int)Math.round(100.0/currentProgress));
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
                		bestScale = peaks[i-numScales] + 1;
                		norm = (bestScale*bestScale)/(2.0*bestScale - 1);
                		HD = entropyArray[peaks[i-numScales]];
                		WD = distArray[peaks[i - numScales]] * norm;
                		saliency = HD * WD;
                		bestSaliency.add(new sixItems(pointx, pointy, bestScale, HD, WD, saliency));
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
        fireProgressStateChanged(100);
    	
    	return bestSaliency;
    }
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale2D(short imageBuffer[], short imageBuffer2[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int bestScale;
    	double HD;
    	double WD;
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
            fireProgressStateChanged((int)Math.round(100.0/currentProgress));
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
                		bestScale = peaks[i-numScales] + 1;
                		norm = (bestScale*bestScale)/(2.0*bestScale - 1);
                		HD = entropyArray[peaks[i-numScales]];
                		WD = distArray[peaks[i - numScales]] * norm;
                		saliency = HD * WD;
                		bestSaliency.add(new sixItems(pointx, pointy, bestScale, HD, WD, saliency));
                	} // for (i = numScales; i < numScales + counts; i++)
                } // if (counts > 0)
                numScales += counts;
                for (j = 0; j < binmax; j++) {
                	histo1[j] = 0;
                	histo2[j] = 0;
                }
            } // for (pointy = yStart; pointy < yStop; pointy++)
        } // for (pointx = xStart; pointx < xStop; pointx++)
        fireProgressStateChanged(100);
    	
    	return bestSaliency;
    }
    
    @SuppressWarnings("unchecked")
	private Vector<sixItems> calcSalScale3D(short imageBuffer[], short imageBuffer2[], short imageBuffer3[]) {
    	Vector<sixItems> bestSaliency = new Vector<sixItems>();
    	int bestScale;
    	double HD;
    	double WD;
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
        double isum;
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
            fireProgressStateChanged((int)Math.round(100.0/currentProgress));
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
                		bestScale = peaks[i-numScales] + 1;
                		norm = (bestScale*bestScale)/(2.0*bestScale - 1);
                		HD = entropyArray[peaks[i-numScales]];
                		WD = distArray[peaks[i - numScales]] * norm;
                		saliency = HD * WD;
                		bestSaliency.add(new sixItems(pointx, pointy, bestScale, HD, WD, saliency));
                	} // for (i = numScales; i < numScales + counts; i++)
                } // if (counts > 0)
                numScales += counts;
                for (j = 0; j < binmax; j++) {
                	histo1[j] = 0;
                	histo2[j] = 0;
                }
            } // for (pointy = yStart; pointy < yStop; pointy++)
        } // for (pointx = xStart; pointx < xStop; pointx++)
        fireProgressStateChanged(100);
        
    	return bestSaliency;
    }
    
    @SuppressWarnings("unchecked")
   	private Vector<Double>[] greedyCluster(Vector<sixItems> Y) {
    	// Greedy clusterer for salient feature post-processing
       	Vector<Double> C[] = (Vector<Double>[]) new Vector[3];
       	int j;
       	
       	for (j = 0; j < 3; j++) {
        	C[j] = new Vector<Double>();
        }
       	j = 1;
       	// Sort in ascending saliency
       	// Y[5] are all positive and must be put in descending order
       	
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

            if (a < b) {
                return 1;
            } else if (a > b) {
                return -1;
            } else {
                return 0;
            }
        }

    }
    
    private class sixItems {
    	private int pointx;
    	private int pointy;
    	private int bestScale;
    	private double HD;
    	private double WD;
    	private double saliency;
    	
    	public sixItems(int pointX, int pointy,int bestScale, double HD, double WD, double saliency) {
    		this.pointx = pointx;
    		this.pointy = pointy;
    		this.bestScale = bestScale;
    		this.HD = HD;
    		this.WD = WD;
    		this.saliency = saliency;
    	}
    	
    	public int getPointx() {
    		return pointx;
    	}
    	
    	public int getPointy() {
    		return pointy;
    	}
    	
    	public int getBestScale() {
    		return bestScale;
    	}
    	
    	public double getHD() {
    		return HD;
    	}
    	
    	public double getWD() {
    		return WD;
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
    } // private doble plogp(double p)
}