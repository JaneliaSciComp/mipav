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
    	Vector<Double>[] bestSaliency;
    	short imageBuffer[] = null;
    	
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
    	
    	if (fastplog) {
    	    for (i = 0; i < plogpres; i++) {
    	        plogpArray[i] = plogp((double)i/plogpres);	
    	    }
    	}
    	
    	if ((!srcImage.isColorImage()) && (!srcImage.isComplexImage())) {
    		// Input is scalar image
    		if (nbins != 0) {
    			// Don't use Parzen window
    			if (antiAliasedSampling) {
    				bestSaliency = calcSalScale1DAA(imageBuffer);
    			} // if (antiAliasedSampling)
    			else {
    				bestSaliency = calcSalScale1D(imageBuffer);	
    			}
    		} // if (nbins != 0)
    		else {
    			bestSaliency = calcSalScale1DParzen(imageBuffer);
    		}
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
    
    @SuppressWarnings("unchecked")
	private Vector<Double>[] calcSalScale1D(short imageBuffer[]) {
    	Vector<Double> bestSaliency[] = (Vector<Double>[]) new Vector[6];
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
        
        // Create output matrix of bestscales and their saliency values
        for (i = 0; i < 6; i++) {
        	bestSaliency[i] = new Vector<Double>();
        }
        
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
                	    bestSaliency[0].add(Double.valueOf(x));	// the x location
                	    bestSaliency[1].add(Double.valueOf(y)); // the y location
                	    bestSaliency[2].add(Double.valueOf(peaks[i-numScales])); // the best scale
                	    norm = Math.pow(bestSaliency[2].get(i),2)/(2*bestSaliency[2].get(i)-1);
                	    bestSaliency[3].add(entropyArray[peaks[i-numScales]]); // HD
                	    bestSaliency[4].add(distArray[peaks[i-numScales]] * norm); // WD
                	    // The saliency
                	    bestSaliency[5].add(norm * entropyArray[peaks[i-numScales]] * distArray[peaks[i-numScales]]);
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
	private Vector<Double>[] calcSalScale1DAA(short imageBuffer[]) {
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
		Vector<Double> bestSaliency[] = (Vector<Double>[]) new Vector[6];
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
        
        // Create output matrix of bestscales and their saliency values
        for (i = 0; i < 6; i++) {
        	bestSaliency[i] = new Vector<Double>();
        }
        
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
                	    bestSaliency[0].add(Double.valueOf(x));	// the x location
                	    bestSaliency[1].add(Double.valueOf(y)); // the y location
                	    bestSaliency[2].add(Double.valueOf(peaks[i-numScales])); // the best scale
                	    // TODO: fix scale normalization
                	    norm = peaks[i-numScales]; // pow(bestSaliency[2].get(i),2)/(2*bestSaliency[2].get(i)-1)
                	    bestSaliency[3].add(entropyArray[peaks[i-numScales]]); // HD
                	    bestSaliency[4].add(distArray[peaks[i-numScales]] * norm); // WD
                	    // The saliency
                	    bestSaliency[5].add(norm * entropyArray[peaks[i-numScales]] * distArray[peaks[i-numScales]]);
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
	private Vector<Double>[] calcSalScale1DParzen(short imageBuffer[]) {
    	Vector<Double> bestSaliency[] = (Vector<Double>[]) new Vector[6];
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
    	
    	nbins = 256;
    	histo1 = new int[nbins];
        histo2 = new double[nbins];
        histoSmooth = new double[nbins];
        
        s = 2*(stopScale + 1) + 1;
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
        
        // Create output matrix of bestscales and their saliency values
        for (i = 0; i < 6; i++) {
        	bestSaliency[i] = new Vector<Double>();
        }
        
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
                } // for (scale = startScale; scale <= stopScale; scale++)
            } // for (pointy = yStart; pointy < yStop; pointy++)
        } // for (pointx = xStart; pointx < xStop; pointx++)
    	
    	return bestSaliency;
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