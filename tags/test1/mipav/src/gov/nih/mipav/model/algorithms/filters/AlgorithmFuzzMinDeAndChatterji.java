package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.Preferences;

import java.io.*;

import java.util.*;


/**
   This algorithm performs minimization of fuzziness, a reduction of the amount of fuzziness.  This method is probably the first
   fuzzy approach to image enhancement and is also known in the literature as contrast intensification operator.  The main idea
   is to reduce the amount of image fuzziness.  The algorithm is as follows:
   1.) Gray-level fuzzification (u(g) denotes the degree of brightness)
        u(g) = 0.5*(1 - (srcThreshold - g)/(srcThreshold - srcMin)) for srcMin <= g <= srcThreshold
        u(g) = 0.5*(1 + (g - srcThreshold)/(srcMax - srcThreshold)) for srcThreshold <= g <= srcMax 
        
   2.) Contrast intensification
        u'(g) = 0.5*(1 - {sin(theta1*(1 - 2*u(g)))/sin(theta1)}**p1) for 0 <= u(g) <= 0.5
        u'(g) = 0.5*(1 + {sin(theta2*(2*u(g) - 1))}/sin(theta2)}**p2) for 0.5 <= u(g) <= 1.0
       
       Values below the crossover value will be decreased and values above the crossover value will be increased. 
   3.) New gray-levels by defuzzification 
       g' = enhancedThreshold - (enhancedThreshold - gmin)*(1 - 2*u'(g)) for 0 <= u'(g) <= 0.5
       g' = enhancedThreshold + (gmax - enhancedThreshold)*(2*u'(g) - 1) for 0.5 <= u'(g) <= 1.0
       gmax >= srcMax.
       gmin <= srcMin 
   
   References: 1.) Tizhoosh, H. R., "Fuzzy Image Enhancement: An Overview," in Fuzzy Techniques in Image Processing,
   E. Kerre and M. Nachtegael, eds., Springer-Verlag, New York, 2000, pp. 141-142.
   2.) De T.K. and Chatterji B.N.,"An approach to a generalized technique for image contrast enhancement using the
       concept of fuzzy set", Fuzzy Sets and Systems, Vol. 25, pp.145-158, 1998.
   3.) Fang N. and Cheng M.C.,"An automatic crossover point selection technique for image enhancement using fuzzy
       sets", Pattern Recognition Letters, Vol. 14, pp. 397-406, 1993.
 */
public class AlgorithmFuzzMinDeAndChatterji extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    
    //~ Instance fields ------------------------------------------------------------------------------------------------
	

    
    /** true means apply to entire image, false only region. */
    private boolean entireImage;

    /** Original image threshold */
    private double srcThreshold;
    
    /** Enhanced image threshold */
    private double enhancedThreshold;
    
    /** If true, ignore srcThreshold input and calculate best value */
    private boolean autoThreshold;
    
    /** Angular equivalence of black band, 0 <= theta1 <= PI/2 */
    private double theta1;
    
    /** Angular equivalence of white band, 0 <= theta2 <= PI/2 */
    private double theta2;
    
    /** Black region exponent, 0 <= p1 <= 1.0 */
    private double p1;
    
    /** White region exponent, 0 <= p2 <= 1.0 */
    private double p2;
    
    /** The new minimum level gmin <= srcMin */
    private double gmin;
    
    /** The new maximum level gmax >= srcMax */
    private double gmax;

    /** contains VOI. */
    private BitSet mask = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which changes are returned to the source image.
     *
     * @param  srcImg    Source image model.
     * @param  srcThreshold original image threshold
     * @param  autoThreshold If true, ignore srcThreshold and calculate best value
     * @param  enhancedThreshold enhanced image threshold
     * @param  theta1    angular equivalence of black band
     * @param  theta2    angular equivalence of white band
     * @param  p1        black region exponent
     * @param  p2        white region exponent
     * @param  gmin      The new minimum level gmin <= srcMin
     * @param  gmax      The new maximum level gmax >= srcMax
     * @param  maskFlag  Flag that indicates that the fuzzy minimization filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmFuzzMinDeAndChatterji(ModelImage srcImg, double srcThreshold, boolean autoThreshold, double enhancedThreshold,
    		double theta1, double theta2, double p1, double p2,
    		double gmin, double gmax, boolean maskFlag) {
        super(null, srcImg);

        entireImage = maskFlag;
        this.srcThreshold = srcThreshold;
        this.autoThreshold = autoThreshold;
        this.enhancedThreshold = enhancedThreshold;
        this.theta1 = theta1;
        this.theta2 = theta2;
        this.p1 = p1;
        this.p2 = p2;
        this.gmin = gmin;
        this.gmax = gmax;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor for images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg   Image model where result image is stored.
     * @param  srcImg    Source image model.
     * @param  srcThreshold original image threshold
     * @param  autoThreshold If true, ignore srcThreshold and calculate best value
     * @param  enhancedThreshold enhanced image threshold
     * @param  theta1    angular equivalence of black band
     * @param  theta2    angular equivalence of white band
     * @param  p1        black region exponent
     * @param  p2        white region exponent
     * @param  gmin      The new minimum level gmin <= srcMin
     * @param  gmax      The new maximum level gmax >= srcMax
     * @param  maskFlag  Flag that indicates that the fuzzy minimization filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmFuzzMinDeAndChatterji(ModelImage destImg, ModelImage srcImg, double srcThreshold, boolean autoThreshold, 
    		               double enhancedThreshold, double theta1, double theta2, double p1, double p2,
                           double gmin, double gmax, boolean maskFlag) {

        super(destImg, srcImg);

        entireImage = maskFlag;
        this.srcThreshold = srcThreshold;
        this.autoThreshold = autoThreshold;
        this.enhancedThreshold = enhancedThreshold;
        this.theta1 = theta1;
        this.theta2 = theta2;
        this.p1 = p1;
        this.p2 = p2;
        this.gmin = gmin;
        this.gmax = gmax;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        mask = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), " Fuzzy Minimization De and Chatterji...");
        
        if (destImage != null) { // if there exists a destination image
            calcStoreInDest();
            
        } else { // there is no image but the original source.
            calcStoreInPlace();
            
        }
    }


    /**
     * Fuzzy minimization filtering of the source image. Replaces the original image with the filtered image.
     */
    private void calcStoreInPlace() {

        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double[] resultBuffer;
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        int index;
        double minVal;
        double maxVal;
        int newType;
        double blackDenom;
        double whiteDenom;
        double sintheta1;
        double sintheta2;
        double sinnum;
        double expon;
        long frequency[] = null;
        double criterionFunction;
        double minCriterionFunction;
        long sum1;
        long sum2;
        double mean1;
        double mean2;
        double variance1;
        double variance2;
        int j;
        double diff;
        double zeta1;
        double zeta2;
        double FTg1;
        double FTg2;
        double sqrt2PI = Math.sqrt(2.0*Math.PI);;
        double std1;
        double std2;

        try {

           
            length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            if (srcImage.getNDims() > 2) {
            	length *= srcImage.getExtents()[2];
            	if (srcImage.getNDims() > 3) {
            		length *= srcImage.getExtents()[3];
            	}
            }

            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization De and Chatterji: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization De and Chatterji reports: Out of memory when creating image buffer", true);

            return;
        }
        
        
        for (i = 0; i < length; i++) {
        	if (entireImage || mask.get(i)) {
        	    if (!srcList.contains(buffer[i])) {
        	        srcList.add(buffer[i]);	
        	    }
        	}
        }
        Collections.sort(srcList);
        uniqueValues = srcList.size();
        resultBuffer = new double[uniqueValues];
        minVal = srcList.get(0);
        maxVal = srcList.get(uniqueValues-1);
        
        if (autoThreshold) {
            frequency = new long[uniqueValues];
            for (i = 0; i < length; i++) {
            	if (entireImage || mask.get(i)) {
            		index = Collections.binarySearch(srcList, buffer[i]);
            		frequency[index]++;
            	}
            } // for (i = 0; i < length; i++)
            
            minCriterionFunction = Double.MAX_VALUE;
            for (i = 1; i < uniqueValues - 1; i++) {
                sum1 = 0L;
                mean1 = 0.0;
                for (j = 0; j <= i; j++) {
                	sum1 += frequency[j];
                	mean1 += frequency[j]*srcList.get(j);
                }
                mean1 = mean1/sum1;
                variance1 = 0.0;
                for (j = 0; j <= i; j++) {
                	diff = srcList.get(j) - mean1;
                	variance1 += diff*diff*frequency[j];
                }
                variance1 = variance1/(sum1-1.0);
                std1 = Math.sqrt(variance1);
                zeta1 = 0.0;
                for (j = 0; j <= i; j++) {
                	diff = srcList.get(j) - mean1;
                	expon = Math.exp(-(diff*diff)/(2.0*variance1));
                	FTg1 = expon/(sqrt2PI*std1);
                	zeta1 += FTg1*sum1/frequency[j];
                }
                sum2 = 0L;
                mean2 = 0.0;
                for (j = i+1; j <= uniqueValues-1; j++) {
                	sum2 += frequency[j];
                	mean2 += frequency[j]*srcList.get(j);
                }
                mean2 = mean2/sum2;
                variance2 = 0.0;
                for (j = i+1; j <= uniqueValues-1; j++) {
                	diff = srcList.get(j) - mean2;
                	variance2 += diff*diff*frequency[j];
                }
                variance2 = variance2/(sum2-1.0);
                std2 = Math.sqrt(variance2);
                zeta2 = 0.0;
                for (j = i+1; j <= uniqueValues-1; j++) {
                	diff = srcList.get(j) - mean2;
                	expon = Math.exp(-(diff*diff)/(2.0*variance2));
                	FTg2 = expon/(sqrt2PI*std2);
                	zeta2 += FTg2*sum2/frequency[j];	
                }
                criterionFunction = zeta1 + zeta2;
                if (criterionFunction < minCriterionFunction) {
                	minCriterionFunction = criterionFunction;
                	srcThreshold = srcList.get(i);
                }
            } // for (i = 1; i < uniqueValues - 1; i++)
            Preferences.debug("Source threshold = " + srcThreshold + "\n");
        } // if (autoThreshold)
        blackDenom = srcThreshold - minVal;
        whiteDenom = maxVal - srcThreshold;
        sintheta1 = Math.sin(theta1);
        sintheta2 = Math.sin(theta2);
        
        for (i = 0; i < uniqueValues; i++) {
        	if (buffer[i] <= srcThreshold) {
        		resultBuffer[i] = 0.5*(1.0 - (srcThreshold - srcList.get(i))/blackDenom);
        	}
        	else {
        		resultBuffer[i] = 0.5*(1.0 + (srcList.get(i) - srcThreshold)/whiteDenom);
        	}
        	if (resultBuffer[i] <= 0.5) {
        	    sinnum = Math.sin(theta1*(1.0 - 2.0*resultBuffer[i]));
        	    expon = Math.pow(sinnum/sintheta1, p1);
        	    resultBuffer[i] = 0.5*(1.0 - expon);
        	}
        	else {
        	    sinnum = Math.sin(theta2*(2.0*resultBuffer[i] - 1.0));
        	    expon = Math.pow(sinnum/sintheta2, p2);
        	    resultBuffer[i] = 0.5*(1.0 + expon);
        	}
        	if (resultBuffer[i] <= 0.5) {
        	    resultBuffer[i] = enhancedThreshold - (enhancedThreshold - gmin)*(1.0 - 2.0*resultBuffer[i]);	
        	}
        	else {
        	    resultBuffer[i] = enhancedThreshold + (gmax - enhancedThreshold)*(2.0*resultBuffer[i] - 1.0);	
        	}
        } // for (i = 0; i < uniqueValues; i++)
        
        for (i = 0; i < length; i++) {
        	if (entireImage || mask.get(i)) {
        	    index = Collections.binarySearch(srcList, buffer[i]);
        	    buffer[i] = resultBuffer[index];
        	}
        }
        srcList.clear();
        
        newType = srcImage.getType();
        if (newType == ModelStorageBase.DOUBLE) {
        	
        }
        else if (newType == ModelStorageBase.FLOAT) {
        	if ((gmin < -Float.MAX_VALUE) || (gmax > Float.MAX_VALUE)) {
        		newType = ModelStorageBase.DOUBLE;
        	}
        }
        else if ((gmin < Long.MIN_VALUE) || (gmax > Long.MAX_VALUE)) {
        	newType = ModelStorageBase.DOUBLE;
        }
        else if (newType == ModelStorageBase.LONG) {
        	
        }
        else if (newType == ModelStorageBase.UINTEGER) {
        	if ((gmin < 0) || (gmax > 4294967295L)) {
        	    newType = ModelStorageBase.LONG;
        	}
        }
        else if ((gmin < Integer.MIN_VALUE) || (gmax > Integer.MAX_VALUE)) {
        	newType = ModelStorageBase.LONG;
        }
        else if (newType == ModelStorageBase.INTEGER) {
        	
        }
        else if (newType == ModelStorageBase.USHORT) {
        	if ((gmin < 0) || (gmax > 65535)) {
        		newType = ModelStorageBase.INTEGER;
        	}
        }
        else if ((gmin < -32768) || (gmax > 32767)) {
        	newType = ModelStorageBase.INTEGER;
        }
        else if (newType == ModelStorageBase.SHORT) {
        	
        }
        else if (newType == ModelStorageBase.UBYTE) {
        	if ((gmin < 0) || (gmax > 255)) {
        		newType = ModelStorageBase.SHORT;
        	}
        }
        else if (newType == ModelStorageBase.BYTE) {
        	if ((gmin < -128) || (gmax > 127)) {
        		newType = ModelStorageBase.SHORT;
        	}
        }
        
        if (srcImage.getType() != newType) {
        	AlgorithmChangeType convertType = new AlgorithmChangeType(srcImage, newType, srcImage.getMin(),
                    srcImage.getMax(), gmin, gmax, false);
            convertType.run();
            convertType = null;
        }
        
        if (threadStopped) {
            finalize();

            return;
        }

        try { // place buffer data into the image
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization De and Chatterji: Source image locked", true);

            return;
        }

        setCompleted(true);
    }

    

    /**
     * This function produces a new image that has had fuzzy minimization filtering
     */
    private void calcStoreInDest() {

        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double[] resultBuffer; 
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        double minVal;
        double maxVal;
        int index;
        double blackDenom;
        double whiteDenom;
        double sintheta1;
        double sintheta2;
        double sinnum;
        double expon;
        long frequency[] = null;
        double criterionFunction;
        double minCriterionFunction;
        long sum1;
        long sum2;
        double mean1;
        double mean2;
        double variance1;
        double variance2;
        int j;
        double diff;
        double zeta1;
        double zeta2;
        double FTg1;
        double FTg2;
        double sqrt2PI = Math.sqrt(2.0*Math.PI);;
        double std1;
        double std2;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Fuzzy Minimization De and Chatterji reports: destination image locked", false);

            return;
        }

        try {
        	
        	length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            if (srcImage.getNDims() > 2) {
            	length *= srcImage.getExtents()[2];
            	if (srcImage.getNDims() > 3) {
            		length *= srcImage.getExtents()[3];
            	}
            }

            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization De and Chatterji reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization De and Chatterji reports: out of memory", true);

            return;
        }
        
        for (i = 0; i < length; i++) {
        	if (entireImage || mask.get(i)) {
        	    if (!srcList.contains(buffer[i])) {
        	        srcList.add(buffer[i]);	
        	    }
        	}
        }
        Collections.sort(srcList);
        uniqueValues = srcList.size();
        resultBuffer = new double[uniqueValues];
        minVal = srcList.get(0);
        maxVal = srcList.get(uniqueValues-1);
        
        if (autoThreshold) {
            frequency = new long[uniqueValues];
            for (i = 0; i < length; i++) {
            	if (entireImage || mask.get(i)) {
            		index = Collections.binarySearch(srcList, buffer[i]);
            		frequency[index]++;
            	}
            } // for (i = 0; i < length; i++)
            
            minCriterionFunction = Double.MAX_VALUE;
            for (i = 1; i < uniqueValues - 1; i++) {
                sum1 = 0L;
                mean1 = 0.0;
                for (j = 0; j <= i; j++) {
                	sum1 += frequency[j];
                	mean1 += frequency[j]*srcList.get(j);
                }
                mean1 = mean1/sum1;
                variance1 = 0.0;
                for (j = 0; j <= i; j++) {
                	diff = srcList.get(j) - mean1;
                	variance1 += diff*diff*frequency[j];
                }
                variance1 = variance1/(sum1-1.0);
                std1 = Math.sqrt(variance1);
                zeta1 = 0.0;
                for (j = 0; j <= i; j++) {
                	diff = srcList.get(j) - mean1;
                	expon = Math.exp(-(diff*diff)/(2.0*variance1));
                	FTg1 = expon/(sqrt2PI*std1);
                	zeta1 += FTg1*sum1/frequency[j];
                }
                sum2 = 0L;
                mean2 = 0.0;
                for (j = i+1; j <= uniqueValues-1; j++) {
                	sum2 += frequency[j];
                	mean2 += frequency[j]*srcList.get(j);
                }
                mean2 = mean2/sum2;
                variance2 = 0.0;
                for (j = i+1; j <= uniqueValues-1; j++) {
                	diff = srcList.get(j) - mean2;
                	variance2 += diff*diff*frequency[j];
                }
                variance2 = variance2/(sum2-1.0);
                std2 = Math.sqrt(variance2);
                zeta2 = 0.0;
                for (j = i+1; j <= uniqueValues-1; j++) {
                	diff = srcList.get(j) - mean2;
                	expon = Math.exp(-(diff*diff)/(2.0*variance2));
                	FTg2 = expon/(sqrt2PI*std2);
                	zeta2 += FTg2*sum2/frequency[j];	
                }
                criterionFunction = zeta1 + zeta2;
                if (criterionFunction < minCriterionFunction) {
                	minCriterionFunction = criterionFunction;
                	srcThreshold = srcList.get(i);
                }
            } // for (i = 1; i < uniqueValues - 1; i++)
            Preferences.debug("Source threshold = " + srcThreshold + "\n");
        } // if (autoThreshold)
        blackDenom = srcThreshold - minVal;
        whiteDenom = maxVal - srcThreshold;
        sintheta1 = Math.sin(theta1);
        sintheta2 = Math.sin(theta2);
        
        for (i = 0; i < uniqueValues; i++) {
        	if (buffer[i] <= srcThreshold) {
        		resultBuffer[i] = 0.5*(1.0 - (srcThreshold - srcList.get(i))/blackDenom);
        	}
        	else {
        		resultBuffer[i] = 0.5*(1.0 + (srcList.get(i) - srcThreshold)/whiteDenom);
        	}
        	if (resultBuffer[i] <= 0.5) {
        	    sinnum = Math.sin(theta1*(1.0 - 2.0*resultBuffer[i]));
        	    expon = Math.pow(sinnum/sintheta1, p1);
        	    resultBuffer[i] = 0.5*(1.0 - expon);
        	}
        	else {
        	    sinnum = Math.sin(theta2*(2.0*resultBuffer[i] - 1.0));
        	    expon = Math.pow(sinnum/sintheta2, p2);
        	    resultBuffer[i] = 0.5*(1.0 + expon);
        	}
        	if (resultBuffer[i] <= 0.5) {
        	    resultBuffer[i] = enhancedThreshold - (enhancedThreshold - gmin)*(1.0 - 2.0*resultBuffer[i]);	
        	}
        	else {
        	    resultBuffer[i] = enhancedThreshold + (gmax - enhancedThreshold)*(2.0*resultBuffer[i] - 1.0);	
        	}
        } // for (i = 0; i < uniqueValues; i++)
        
        
        for (i = 0; i < length; i++) {
        	if (entireImage || mask.get(i)) {
        	    index = Collections.binarySearch(srcList, buffer[i]);
        	    buffer[i] = resultBuffer[index];
        	}
        }
        srcList.clear();
        
        
        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else
       
        if (threadStopped) {
            finalize();

            return;
        }

        try { // but now place buffer data into the image
            destImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization reports De and Chatterji: destination image still locked", true);

            return;
        }

        setCompleted(true);
    }

   
}
