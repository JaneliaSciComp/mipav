package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import gov.nih.mipav.view.*;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;
import Jama.*;


/**
 * This algorithm operates on 3D black and white images and on 2D and 3D color images. The user can select 1 or both of
 * 2 output images: The first image is a filtered 3D image containing only the first n independent components, where n is
 * selected by the user after viewing images of the first 10 (or the number that exist if less than 10) independent
 * components. The second image is a 2D image created by averaging together the images in the 3D image. Note that red,
 * green, and blue are treated as separate 2D planes.
 * Requirements:
 * 1.) The independent components must be statistically independent.
 * 2.) The independent components must have nongaussian distributions.
 * 3.) For simplicity, assume the unknown mixture matrix is square.  In other words, the number of 
 * mixtures equals the number of independent components.  If the number of independent components is
 * smaller than the number of mixtures, use principal component analysis to reduce the number of
 * mixtures to the number of principal components.  Reducing the number of dimensions can reduce noise and
 * prevent overlearning.
 * Filtering the data in the Fourier domain is allowed before performing independent component analysis since
 * applying the Fourier transform to the data does not change the mixing matrix.
 * References:
 * 1.) Independent Component Analysis by Aapo Hyvarinen, Juha Karhunen, and Erkki Oja, John-Wiley & Sons, Inc.,
 * 2001. 
 * From the text: "Another very useful thing to do is to reduce the dimension of the data by principal component
 * analysis.  This reduces noise and prevents overlearning.  It may also solve the problems with data that has a 
 * smaller number of independent components than mixtures."
 */
public class AlgorithmIndependentComponents extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage[] destImage = null;

    /** DOCUMENT ME! */
    private boolean haveColor;

    /** number of image planes present. */
    private int nPlanes;

    /** number of independent components to be retained.
     For SYMMETRIC_ORTHOGONALIZATION and MAXIMUM_LIKELIHOOD_ESTIMATION
         icNumber = nPlanes
                  = colorsRequested * number of source images for color
                  = number of source images for black and white
         icNumber can only be varied from 1 to nPlanes with DEFLATIONARY_ORTHOGONALIZATION */
    private int icNumber;
    
    private ModelImage[] srcImageArray = null;

    /** DOCUMENT ME! */
    private ModelImage srcImage;
    
    /* From the text Independent Component Analysis by Hyvarinen, Karhunen, and Oja;
     * "One must also choose the nonlinearity used in the algorithms.  It seems that the robust, nonpolynomial
     * nonlinearities are to be preferred in most applications.  The simplest thing to do is to just use the
     * tanh function as the nonlinearity g."  When testing 10 supergaussian independent components,
     * the best results from a statistical viewpoint were obtained when using the tanh nonlinearity with
     * any algorithm.
     * 
     * Kurtosis in the zero mean case is defined by the equation:
     * kurt(x) = E{x**4} - 3*[E{x**2}]**2
     * Normalized kurtosis = E{x**4}/{E{x**2}]**2 - 3
     * 
     * A slightly stronger property than uncorrelatedness is whiteness.  Whiteness of a zero-mean random
       vector means that components are uncorrelated and their variances equal unity.  Whitening is sometimes
       called sphering.  For whitened data E{x**2} = 1, and both kurtosis and normalized kurtosis reduce to
       E{x**4} - 3.
       
       Kurtosis is the simplest statistical quantity for indicating the nongaussianity of a random variable.
       The kurtosis of a gaussian distribution is zero.  Distributions having a negative kurtosis are said to
       be subgaussian or platykurtic.  If the kurtosis is positive, the respective distribution is called
       supergaussian or leptokurtic.  Subgaussian distributions tend to be flatter than gaussian distributions
       or multimodal.  The uniform distribution is a subgaussian distribution.  A typical supergaussian
       distribution has a sharper peak and longer tails than the gaussian distribution.  The Laplacian 
       distribution is a supergaussian distribution.
     * 
     * When the independent components are highly supergaussian, or when robustness is very important, g2(y) = 
     * y * exp(-y*y/2) may be better.
     * 
     * Using kurtosis and hence 1/4 the derivative of the normalized kurtosis = y**3 is well justified only if
     * the independent components are subgaussian and there are no outliers.
     */
    private int nonlinearFunction = 1; // g1(y) = tanh(a1*y)
                          // g2(y) = y*exp(-y*y/2)
                          // g3(y) = y*y*y
    
    private double a1 = 1.0; // 1.0 <= a1 <= 2.0
    
    private double endTolerance = 1.0E-6;
    
    private int icAlgorithm; // 1 = symmetric
                                   // 2 = deflationary
    /* From the text Independent Component Analysis by Hyvarinen, Karhunen, and Oja:
     * "Once choice is between estimating all the independent components in parallel, or just estimating a few of
     * them (possibly one-by-one).  This corresponds to choosing between symmetric and hierarchical decorrelation.
     * In most cases, symmetric decorrelation is recommended.  Deflation is mainly useful in cases where we want
     * to estimate only a very limited number of ICs, and other special cases.  The disadvantage with deflationary
     * orthogonalization is that the estimation errors in the components the are estimated first accumulate and
     * increase the errors in the later components."
     * 
     * The maximum likelihood estimation algorithm is a computationally optimized version of the gradient
     * algorithm.  It can use the tanh function to estimate both sub- and supergaussian independent components.
     * 
     * "In difficult real world problems, it is useful to apply several different ICA methods, because they 
     * may reveal different ICs from the data."
     */
    public final static int SYMMETRIC_ORTHOGONALIZATION = 1;
    
    public final static int DEFLATIONARY_ORTHOGONALIZATION = 2;
    
    public final static int MAXIMUM_LIKELIHOOD_ESTIMATION = 3;
    
    public final static int TANH_FUNCTION = 1;
    
    public final static int EXP_FUNCTION = 2;
    	
    public final static int CUBIC_FUNCTION = 3;
    
    private int maxIterations;
    
    private boolean redRequested;
    
    private boolean greenRequested;
    
    private boolean blueRequested;
    
    private boolean selfTest = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmIndependentComponents object.
     *
     * @param  destImg        image model where result image is to stored
     * @param  srcImg         source image model
     * @param  icNumber       icNumber is the number of independent component images to retain
     * @param  icAlgorithm
     * @param  nonlinearFunction
     * @param  a1
     * @param  endTolerance
     * @param maxIterations
     */
    public AlgorithmIndependentComponents(ModelImage[] destImg, ModelImage srcImg, int icNumber, int icAlgorithm,
    		int nonlinearFunction, double a1, double endTolerance, int maxIterations, boolean redRequested, boolean greenRequested,
    		boolean blueReuqested) {

        destImage = destImg; // Put results in destination image.
        srcImage = srcImg;
        this.icNumber = icNumber;
        this.icAlgorithm = icAlgorithm;
        this.nonlinearFunction = nonlinearFunction;
        this.a1 = a1;
        this.endTolerance = endTolerance;
        this.maxIterations = maxIterations;
        this.redRequested = redRequested;
        this.greenRequested = greenRequested;
        this.blueRequested = blueReuqested;
    }
    
    /**
     * Creates a new AlgorithmIndependentComponents object.
     *
     * @param  destImg        image model where result image is to stored
     * @param  srcImg         source image model
     * @param  icNumber       icNumber is the number of independent component images to retain
     * @param  icAlgorithm
     * @param  nonlinearFunction
     * @param  a1
     * @param  endTolerance
     * @param  maxIterations
     * @param  redRequested
     * @param  greenRequested
     * @param  blueRequested
     */
    public AlgorithmIndependentComponents(ModelImage[] destImg, ModelImage[] srcImgArray, int icNumber, int icAlgorithm,
    		int nonlinearFunction, double a1, double endTolerance, int maxIterations, boolean redRequested, boolean greenRequested,
    		boolean blueRequested) {

        destImage = destImg; // Put results in destination image.
        srcImageArray = srcImgArray;
        this.icNumber = icNumber;
        this.icAlgorithm = icAlgorithm;
        this.nonlinearFunction = nonlinearFunction;
        this.a1 = a1;
        this.endTolerance = endTolerance;
        this.maxIterations = maxIterations;
        this.redRequested = redRequested;
        this.greenRequested = greenRequested;
        this.blueRequested = blueRequested;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Start algorithm.
     */
    public void runAlgorithm() {

        if ((srcImageArray == null) && (srcImage == null)) {
            displayError("IComponent: Source Image is null");
            setCompleted(false);

            return;
        }

        if ((srcImageArray != null) && (srcImageArray[0].isColorImage())) {
        	haveColor = true;
        }
        else if ((srcImageArray != null) && (!(srcImageArray[0].isColorImage()))) {
        	haveColor = false;
        }
        else if (srcImage.isColorImage()) {
            haveColor = true;
        } else {
            haveColor = false;
        }

        if ((srcImageArray == null) && (srcImage.getNDims() != 3) && (!haveColor)) {
            displayError("IComponent: Black and white source image must be 3D");
            setCompleted(false);

            return;
        }

        
        if (srcImageArray != null) {
        	fireProgressStateChanged(srcImageArray[0].getImageName(), " Independent component ...");
        }
        else {
            fireProgressStateChanged(srcImage.getImageName(), "Independent component ...");
        }


        iComponent();

        if (threadStopped) {
            finalize();

            return;
        }
    }

    /**
     * Copy important file information to resultant image structure.
     *
     * @param  image        Source image.
     * @param  resultImage  Resultant image.
     */
    public void updateFileInfo(ModelImage image, ModelImage resultImage) {
        FileInfoBase[] fileInfo;

        if (resultImage.getNDims() == 2) {
            fileInfo = resultImage.getFileInfo();
            fileInfo[0].setModality(image.getFileInfo()[0].getModality());
            fileInfo[0].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
            fileInfo[0].setEndianess(image.getFileInfo()[0].getEndianess());
            fileInfo[0].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
            fileInfo[0].setResolutions(image.getFileInfo()[0].getResolutions());
            fileInfo[0].setExtents(resultImage.getExtents());
            fileInfo[0].setMax(resultImage.getMax());
            fileInfo[0].setMin(resultImage.getMin());
            fileInfo[0].setImageOrientation(image.getImageOrientation());
            fileInfo[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation());
            fileInfo[0].setOrigin(image.getFileInfo()[0].getOrigin());
            fileInfo[0].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
            fileInfo[0].setPhotometric(image.getFileInfo()[0].getPhotometric());
        } else if (resultImage.getNDims() == 3) {
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                int j = Math.min(i, image.getExtents()[2] - 1);
                fileInfo[i].setModality(image.getFileInfo()[j].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[j].getFileDirectory());
                fileInfo[i].setEndianess(image.getFileInfo()[j].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[j].getUnitsOfMeasure());
                fileInfo[i].setResolutions(image.getFileInfo()[j].getResolutions());
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(image.getImageOrientation());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[j].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[j].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[j].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[j].getPhotometric());
            }
        } else if (resultImage.getNDims() == 4) {
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < (resultImage.getExtents()[2] * resultImage.getExtents()[3]); i++) {
                fileInfo[i].setModality(image.getFileInfo()[i].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[i].getFileDirectory());
                fileInfo[i].setEndianess(image.getFileInfo()[i].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[i].getUnitsOfMeasure());
                fileInfo[i].setResolutions(image.getFileInfo()[i].getResolutions());
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(image.getImageOrientation());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[i].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[i].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[i].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[i].getPhotometric());
            }
        }

    }  

    

    /**
     * DOCUMENT ME!
     */
    private void iComponent() {
        double[] mean;
        int samples;
        int i, j, k, m;
        int z;
        int zDim;
        double[] values;
        double[] zvalues;
        int totalLength;
        double[][] covar;
        double[] x;
        int index;
        int p;
        double[] result;
        //int iNumber;
        //float vMin;
        //float vMax;
        //double typeMin;
        //double typeMax;
        //double a;
        //double b;
        //float rMin;
        //float rMax;
        int index2;
        int i2;
        int z2;
        double w[][];
        RandomNumberGen randomGen;
        double sumSquares;
        double norm;
        double wpTz;
        double wlast[];
        double wMaxDiff;
        double g;
        double gp;
        double ex;
        double E1[];
        double E2;
        double wpTwj;
        Matrix matW;
        Matrix matWWT;
        double WWT[][];
        double wslast[][];
        double B[][];
        Matrix matB;
        double Blast[][];
        double maxBDiff;
        double y[];
        double alpha[];
        double beta[];
        double Eygy[];
        double Egpy[];
        double ty;
        double EgyyT[][];
        double tk;
        double diagAlpha[][];
        Matrix matAlpha;
        Matrix matBeta;
        double prod[][];
        double BCBT[][];
        Matrix matC;
        int length;
        int iterations;
        int colorsRequested = 0;
        boolean colorArray[] = new boolean[4];
        int iCurrent;
        int i2Current;
        double temp[];
        double wstd[][];
        double wslaststd[][];
        double wlaststd[];
        double Bstd[][];
        double Blaststd[][];
        Matrix matScale;
        double scale[][];
        FFTUtility fft;
        double resultImag[] = null;
        double resultMag[] = null;
        double tag[] = null;

        if (haveColor) {
        	if (redRequested) {
        		colorsRequested++;
        		colorArray[1] = true;
        	}
        	if (greenRequested) {
        		colorsRequested++;
        		colorArray[2] =true;
        	}
        	if (blueRequested) {
        		colorsRequested++;
        		colorArray[3] = true;
        	}
            if (srcImageArray != null) {
        	    nPlanes = colorsRequested * srcImageArray.length;
        	    zDim = srcImageArray.length;
            }
            else if (srcImage.getNDims() == 2) {
                nPlanes = colorsRequested;
                zDim = 1;
            } else {
                zDim = srcImage.getExtents()[2];
                nPlanes = colorsRequested * zDim;
            }
        } // if (haveColor)
        else { // not color
        	if (srcImageArray != null) {
        	    nPlanes = srcImageArray.length;
        	    zDim = srcImageArray.length;
        	}
        	else {
                zDim = srcImage.getExtents()[2];
                nPlanes = zDim;
        	}
        } // else not color

        try {
            mean = new double[nPlanes];
        } catch (OutOfMemoryError e) {
            mean = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating mean");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < nPlanes; i++) {
            mean[i] = 0.0;
        }
        
        if (srcImageArray != null) {
        	samples = srcImageArray[0].getExtents()[0] * srcImageArray[0].getExtents()[1];
        }
        else {
            samples = srcImage.getExtents()[0] * srcImage.getExtents()[1];
        }

        if (haveColor) {
            totalLength = 4 * samples * zDim;
            length = 4 * samples;
        } else {
            totalLength = samples * zDim;
            length = samples;
        }

        fireProgressStateChanged("Exporting source data");

        try {
            values = new double[totalLength];
        } catch (OutOfMemoryError e) {
            values = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating values");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }
        
        if (srcImageArray != null) {
        	temp = new double[length];
            for (i = 0; i < srcImageArray.length; i++) {
            	try {
    	            srcImageArray[i].exportData(0, length, temp); // locks and releases lock
    	        } catch (IOException error) {
    	            displayError("Algorithm IComponent: Image(s) locked");
    	            setCompleted(false);
    	
    	            setThreadStopped(true);
    	
    	            return;
    	        }
    	        for (j = 0; j < length; j++) {
    	        	values[i*length + j] = temp[j];
    	        }
            }
            temp = null;
        }
        else {
	        try {
	            srcImage.exportData(0, totalLength, values); // locks and releases lock
	        } catch (IOException error) {
	            displayError("Algorithm IComponent: Image(s) locked");
	            setCompleted(false);
	
	            setThreadStopped(true);
	
	            return;
	        }
        }
        
        if (selfTest) {
        	resultImag = new double[length];
        	resultMag = new double[length];
        	tag = new double[length];
        	for (i = 0, j = 0, k = 0; j < length; j++, i++) {
        		// Because of the whitening step, the w and B matrices will not reflect the scaling values
        		// of the 2 components.  Must look at matW.times(matWh) and matB.times(matWh)
        		// No phase difference below:
        		values[j] = 0.75*Math.sin(j*Math.PI/100.0) + 0.25*Math.sin(3.0*j*Math.PI/100.0);
        		values[length+j] = 0.25*Math.sin(j*Math.PI/100.0) + 0.75*Math.sin(3.0*j*Math.PI/100.0);
        		//ic1 = 1.5*x1 - 0.5*x2
        		//ic2 = -0.5*x1 + 1.5*x2
        		// Normalizing:
        		// 0.948683298*x1 - 0.316227766*x2
        		// -0.3166227766*x1 + 0.948683298*x2
        		
        		// Now introduce a PI/4 phase in one sine.
        		//values[j] = 0.75*Math.sin(j*Math.PI/100.0) + 0.25*Math.sin((3.0*j*Math.PI/100.0) + 0.25*Math.PI);
        		//values[length+j] = 0.25*Math.sin(j*Math.PI/100.0) + 0.75*Math.sin((3.0*j*Math.PI/100.0) + 0.25*Math.PI);
        		
        		// Now make one sine 3.5 times the frequency of the other
        		//values[j] = 0.75*Math.sin(j*Math.PI/100.0) + 0.25*Math.sin(3.5*j*Math.PI/100.0);
        		//values[length+j] = 0.25*Math.sin(j*Math.PI/100.0) + 0.75*Math.sin(3.5*j*Math.PI/100.0);
        		
        		// Now make one sine PI times the frequency of the other
        		//values[j] = 0.75*Math.sin(j*Math.PI/100.0) + 0.25*Math.sin(Math.PI*j*Math.PI/100.0);
        		//values[length+j] = 0.25*Math.sin(j*Math.PI/100.0) + 0.75*Math.sin(Math.PI*j*Math.PI/100.0);
        		
        	}
        }

        fireProgressStateChanged(10);

        // Each element of the mean vector is the mean of 1 of the nPlanes
        fireProgressStateChanged("Calculating mean vector");

        if (haveColor) {

            for (z = 0; z < zDim; z++) {

                for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
                    iCurrent++;
                    for (j = 0; j < samples; j++) {
                        mean[iCurrent + (colorsRequested * z)] += values[(4 * z * samples) + (4 * j) + i];
                    }

                    mean[iCurrent + (colorsRequested * z)] /= samples;
                }
            }
        } // if haveColor
        else { // not color

            for (z = 0; z < zDim; z++) {

                for (j = 0; j < samples; j++) {
                    mean[z] += values[(z * samples) + j];
                }

                mean[z] /= samples;
            }
        } // else not color

        fireProgressStateChanged(20);

        if (threadStopped) {
            finalize();

            setCompleted(false);

            return;
        }
        
        // Set the values to zero mean
        fireProgressStateChanged("Making the variables zero mean");
        if (haveColor) {

            for (z = 0; z < zDim; z++) {

                for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
                    iCurrent++;
                    for (j = 0; j < samples; j++) {
                    	values[(4 * z * samples) + (4 * j) + i] -= mean[iCurrent + (colorsRequested * z)];
                    }

                }
            }
        } // if haveColor
        else { // not color

            for (z = 0; z < zDim; z++) {

                for (j = 0; j < samples; j++) {
                	values[(z * samples) + j] -= mean[z];
                }

            }
        } // else not color
        

        // The elements of the covar matrix contain the covariances between the nPlanes.
        // The covariance matrix is real and symmetric, so finding a set of nPlanes
        // orthonormal eigenvectors is always possible.
        fireProgressStateChanged("Calculating covariance matrix");

        try {
            covar = new double[nPlanes][nPlanes];
        } catch (OutOfMemoryError e) {
            covar = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating covar");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < nPlanes; i++) {

            for (j = 0; j < nPlanes; j++) {
                covar[i][j] = 0.0;
            }
        }

        try {
            x = new double[nPlanes];
        } catch (OutOfMemoryError e) {
            x = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating x");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        if (haveColor) {

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {

                    for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
                    	iCurrent++;
                        x[iCurrent + (colorsRequested * z)] = values[(4 * z * samples) + (4 * j) + i];
                    }
                }

                for (k = 0; k < nPlanes; k++) {

                    for (m = 0; m < nPlanes; m++) {
                        covar[k][m] += x[k] * x[m];
                    }
                }
            }
        } // if haveColor
        else { // not color

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {
                    x[z] = values[(z * samples) + j];
                }

                for (k = 0; k < nPlanes; k++) {

                    for (m = 0; m < nPlanes; m++) {
                        covar[k][m] += x[k] * x[m];
                    }
                }
            }
        } // else not color

        for (i = 0; i < nPlanes; i++) {

            for (j = 0; j < nPlanes; j++) {
                covar[i][j] = (covar[i][j] / samples);
            }
        }

        fireProgressStateChanged(30);

        if (threadStopped) {
            finalize();

            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Calculating eigenvalues and eigenvectors");

        double[] eigenvalue;
        double[][] V;
        double[] e1;
        double[][] D;
        Matrix matV;
        Matrix matD;
        Matrix matWh; // Whitening matrix
        double wh[][];
        try {
            eigenvalue = new double[nPlanes];
            D = new double[nPlanes][nPlanes];
            V = new double[nPlanes][nPlanes];
            e1 = new double[nPlanes];
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating eig");
            setCompleted(false);
            setThreadStopped(true);
            return;
        }
        // covar = V * (diagonal eigenvalues) * V'
        // In EigevalueDecomposition the columns of V represent the eigenvectors
        // Whitening matrix = v * 1/sqrt(diagonal eigenvalues) * V'
        Eigenvalue.decompose( covar, V, eigenvalue, e1 );
        for (i = 0; i < nPlanes; i++) {
        	D[i][i] = 1.0/Math.sqrt(eigenvalue[i]);
        }
        matV = new Matrix(V);
        matD = new Matrix(D);
        matWh = (matV.times(matD)).times(matV.transpose());  
        wh = matWh.getArray();
        try {
            zvalues = new double[totalLength];
        } catch (OutOfMemoryError e) {
            zvalues = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating zvalues");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }
        // A slightly stronger property than uncorrelatedness is whiteness.  Whiteness of a zero-mean random
        // vector means that components are uncorrelated and their variances equal unity.  Whitening is sometimes
        // called sphering.
        fireProgressStateChanged("Performing whitening");
        
        if (haveColor) {

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {

                    for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
                    	iCurrent++;
                    	index = iCurrent + (colorsRequested * z);
                    	for (z2 = 0; z2 < zDim; z2++) {
                    		for (i2Current = -1, i2 = 1; (i2 < 4) && (colorArray[i2]); i2++) {
                    			i2Current++;
                    			index2 = i2Current + (colorsRequested * z2);
                                zvalues[(4 * z * samples) + (4 * j) + i] += 
                                	wh[index][index2] * values[(4 * z2 * samples) + (4 * j) + i2];
                    		}
                    	}
                    }
                }
            }
        } // if haveColor
        else { // not color

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {
                	for (z2 = 0; z2 < zDim; z2++) {
                        zvalues[(z * samples) + j] += wh[z][z2] * values[(z2 * samples) + j];
                	}
                }
            }
        } // else not color
        
        for (i = 0; i < nPlanes; i++) {
        	for (j = 0; j < nPlanes; j++) {
        		Preferences.debug("wh["+i+"]["+j+"] = " +  wh[i][j] + "\n");
        	}
        }
        
        
        E1 = new double[nPlanes];
        randomGen = new RandomNumberGen();
        fireProgressStateChanged(40);
        if (icAlgorithm == DEFLATIONARY_ORTHOGONALIZATION) {
        	w = new double[icNumber][nPlanes];
        	wstd =  new double[icNumber][nPlanes];
        	wlast = new double[nPlanes];
        	wlaststd = new double[nPlanes];
        	result = new double[samples];
	        for (p = 0; p < icNumber; p++) {
	        	Preferences.debug("p = " + p + "\n");
	        	// Choose an initial value of unit norm for w[p], e.g., randomly
	        	sumSquares = 0.0;
	        	for (i = 0; i < nPlanes; i++) {
	        		w[p][i] = randomGen.genUniformRandomNum(-1.0, 1.0);
	        		sumSquares += w[p][i]*w[p][i];
	        	}
	        	norm = Math.sqrt(sumSquares);
	        	for (i = 0; i < nPlanes; i++) {
	        		w[p][i] = w[p][i]/norm;
	        		wlast[i] = w[p][i];
	        	}
	        	wMaxDiff = 1.0;
	        	iterations = 0;
	        	while ((wMaxDiff >= endTolerance) && (iterations < maxIterations)) {
	        		iterations++;
	        		fireProgressStateChanged("p = " + p + " iterations = " + iterations);
	        		fireProgressStateChanged(40 + (60*p/icNumber) + (60*iterations)/(maxIterations*icNumber));
	        		Preferences.debug("wMaxDiff = " + wMaxDiff + " at iteration " + iterations + "\n",
	        				Preferences.DEBUG_ALGORITHM);
	        	    for (i = 0; i < nPlanes; i++) {
	        	    	E1[i] = 0.0;
	        	    }
	        	    E2 = 0.0;
		        	// g1(y) = tanh(a1*y) where 1 <= a1 <= 2, often taken as a1 = 1.
		        	// g1'(y) = a1*(1 - tanh(a1*y)*tanh(a1*y))
		        	// g2(y) = y*exp(-y*y/2)
		        	// g2'(y) = (1 - y*y)exp(-y*y/2)
		        	// g3(y) = y*y*y
		        	// g3'(y) = 3*y*y
		        	// wp <- E{zg(wpTz)} - E{g'(wpTz)}wp
		        	if (haveColor) {
		
		                for (j = 0; j < samples; j++) {
		                    wpTz = 0;
		                    for (z = 0; z < zDim; z++) {
		
		                        for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
		                        	iCurrent++;
		                        	index = iCurrent + (colorsRequested * z);
		                        	wpTz += w[p][index]* zvalues[(4 * z * samples) + (4 * j) + i];
		                        	
		                        }
		                    } // for (z = 0; z < zDim; z++)
		                    if (nonlinearFunction == TANH_FUNCTION) {
		                    	g = Math.tanh(a1*wpTz);
		                    	gp = a1*(1.0 - g*g);
		                    }
		                    else if (nonlinearFunction == EXP_FUNCTION) {
		                    	ex = Math.exp(-wpTz*wpTz/2.0);
		                    	g = wpTz*ex;
		                    	gp = (1.0 - wpTz*wpTz)*ex;
		                    }
		                    else {
		                    	g = wpTz * wpTz * wpTz;
		                    	gp = 3.0 * wpTz * wpTz;
		                    }
		                    for (z = 0; z < zDim; z++) {
		                    	
		                        for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
		                        	iCurrent++;
		                        	index = iCurrent + (colorsRequested * z);
		                        	E1[index] += zvalues[(4 * z * samples) + (4 * j) + i] * g;
		                        }
		                    } // for (z = 0; z < zDim; z++)
		                    E2 += gp;
		                } // for (j = 0; j < samples; j++)
		            } // if haveColor
		            else { // not color
		
		                for (j = 0; j < samples; j++) {
		                    wpTz = 0;
		                    for (z = 0; z < zDim; z++) {
		                    	wpTz += w[p][z] * zvalues[(z * samples) + j];
		                    } // for (z = 0; z < zDim; z++)
		                    if (nonlinearFunction == TANH_FUNCTION) {
		                    	g = Math.tanh(a1*wpTz);
		                    	gp = a1*(1.0 - g*g);
		                    }
		                    else if (nonlinearFunction == EXP_FUNCTION) {
		                    	ex = Math.exp(-wpTz*wpTz/2.0);
		                    	g = wpTz*ex;
		                    	gp = (1.0 - wpTz*wpTz)*ex;
		                    }
		                    else {
		                    	g = wpTz * wpTz * wpTz;
		                    	gp = 3.0 * wpTz * wpTz;
		                    }
		                    for (z = 0; z < zDim; z++) {
		                    	E1[z] += zvalues[(z * samples) + j] * g;
		                    }
		                    E2 += gp;
		                } // for (j = 0; j < samples; j++)
		            } // else not color
		        	for (i = 0; i < nPlanes; i++) {
	                	E1[i] = E1[i]/samples;
	                }
	                E2 = E2/samples;
	                for (i = 0; i < nPlanes; i++) {
	                	w[p][i] = E1[i] - E2*w[p][i];
	                }
	                // Orthogonalize
	                for (j = 0; j < p; j++) {
	                    wpTwj = 0.0;
	                    for (i = 0; i < nPlanes; i++) {
	                    	wpTwj += w[p][i]*w[j][i];
	                    }
	                    for (i = 0; i < nPlanes; i++) {
	                    	w[p][i] = w[p][i] - wpTwj * w[j][i];
	                    }
	                } // for (j = 0; j < p; j++)
	                // Normalize
	                sumSquares = 0.0;
	                for (i = 0; i < nPlanes; i++) {
	            		sumSquares += w[p][i]*w[p][i];
	            	}
	            	norm = Math.sqrt(sumSquares);
	            	for (i = 0; i < nPlanes; i++) {
	            		w[p][i] = w[p][i]/norm;
	            	}
	            	// Make sure the signs are the same as on the last iteration
	            	// by always making the first element positive
	            	if (w[p][0] < 0) {
	            		for (i = 0; i < nPlanes; i++) {
	            			wstd[p][i] = -w[p][i];
	            		}
	            	}
	            	else {
	            	    for (i = 0; i < nPlanes; i++) {
	            		    wstd[p][i] = w[p][i];		
	            	    }
	            	}
	            	if (wlast[0] < 0) {
	            		for (i = 0; i < nPlanes; i++) {
	            			wlaststd[i] = -wlast[i];
	            		}
	            	}
	            	else {
	            		for (i = 0; i < nPlanes; i++) {
	            			wlaststd[i] = wlast[i];
	            		}
	            	}
		        	wMaxDiff = 0;
		        	for (i = 0; i < nPlanes; i++) {
		        		if (Math.abs(wstd[p][i] - wlaststd[i]) > wMaxDiff) {
		        		    wMaxDiff = Math.abs(wstd[p][i] - wlaststd[i]);	
		        		}
		        		wlast[i] = w[p][i];
		        	}
	        	} // while ((wMaxDiff >= endTolerance)&& (iterations < maxIterations))
	        	// y[p] = w[p][i] * z
	        	if (haveColor) {
		        	for (j = 0; j < samples; j++) {
	                    result[j] = 0;
	                    for (z = 0; z < zDim; z++) {
	
	                        for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
	                        	iCurrent++;
	                        	index = iCurrent + (colorsRequested * z);
	                        	result[j] += w[p][index]* zvalues[(4 * z * samples) + (4 * j) + i];
	                        }
	                    } // for (z = 0; z < zDim; z++)
		        	} // for (j = 0; j < samples; j++)
	        	} // if (haveColor)
	        	else {
	        		for (j = 0; j < samples; j++) {
	                    result[j] = 0;
	                    for (z = 0; z < zDim; z++) {
	                    	result[j] += w[p][z] * zvalues[(z * samples) + j];
	                    } // for (z = 0; z < zDim; z++)
	        		} // for (j = 0; j < samples; j++)
	        	}
	        	try {
	                destImage[p].importData(0, result, true);
	            } catch (IOException error) {
	                displayError("AlgorithmPrincipalComponents: IOException on averaged destination image import data");

	                setCompleted(false);

	                return;
	            }
	            if (selfTest) {
	            	Preferences.debug("Largest FFT coefficients in component " + p + "\n");
	            	for (i = 0; i < length; i++) {
	            		resultImag[i] = 0.0;
	            	}
	                fft = new FFTUtility(result, resultImag, 1, length, 1, -1, FFTUtility.FFT);	
	                fft.run();
	                for (i = 0; i < length; i++) {
	                	resultMag[i] = Math.sqrt(result[i]*result[i] + resultImag[i]*resultImag[i]);
	                }
	                for (i = 0; i < length; i++) {
	                	tag[i] = i;
	                }
	                fft.sortg(resultMag, length, tag);
	                for (i = length-1; i > length-100; i--) {
	                	Preferences.debug("Mag = " + resultMag[i] + " Index = " + Math.round(tag[i]) + " Real = " + result[(int)Math.round(tag[i])] + " Imag = " +
	                			                                                 resultImag[(int)Math.round(tag[i])] + "\n");
	                }
	            }
	        } // for (p = 0; p < icNumber; p++)
	        for (p = 0; p < icNumber; p++) {
	        	for (i = 0; i < nPlanes; i++) {
	        		Preferences.debug(" w["+p+"]["+i+"] = " + w[p][i] + "\n", Preferences.DEBUG_ALGORITHM);
	        	}
        	}
	        matW = new Matrix(w);
        	matScale = matW.times(matWh);
        	scale = matScale.getArray();
        	for (p = 0; p < nPlanes; p++) {
	        	for (i = 0; i < nPlanes; i++) {
	        		Preferences.debug(" scale["+p+"]["+i+"] = " + scale[p][i] + "\n", Preferences.DEBUG_ALGORITHM);
	        	}
        	}
        } // if (icAlgorithm == DEFLATIONARY_ORTHOGONALIZATION)
        else if (icAlgorithm == SYMMETRIC_ORTHOGONALIZATION){
        	w = new double[nPlanes][nPlanes];
        	wstd = new double[nPlanes][nPlanes];
        	result = new double[samples];
        	for (p = 0; p < nPlanes; p++) {
	        	// Choose an initial value of unit norm for w[p], e.g., randomly
	        	sumSquares = 0.0;
	        	for (i = 0; i < nPlanes; i++) {
	        		w[p][i] = randomGen.genUniformRandomNum(-1.0, 1.0);
	        		sumSquares += w[p][i]*w[p][i];
	        	}
	        	norm = Math.sqrt(sumSquares);
	        	for (i = 0; i < nPlanes; i++) {
	        		w[p][i] = w[p][i]/norm;
	        	}
        	} // for (p = 0; p < nPlanes; p++)
        	matW = new Matrix(w);
        	matWWT = matW.times(matW.transpose());
        	WWT = matWWT.getArray();
        	try {
                eigenvalue = new double[nPlanes];
                D = new double[nPlanes][nPlanes];
                V = new double[nPlanes][nPlanes];
                e1 = new double[nPlanes];
            } catch (OutOfMemoryError e) {
                System.gc();
                displayError("Algorithm Independent component: Out of memory allocating eig");
                setCompleted(false);
                setThreadStopped(true);
                return;
            }
            // WWT = V * (diagonal eigenvalues) * V'
            // In EigevalueDecomposition the columns of V represent the eigenvectors
            // WWT**(-1/2) = v * 1/sqrt(diagonal eigenvalues) * V'
            Eigenvalue.decompose( WWT, V, eigenvalue, e1 );
            for (i = 0; i < nPlanes; i++) {
            	D[i][i] = 1.0/Math.sqrt(eigenvalue[i]);
            }
            matV = new Matrix(V);
            matD = new Matrix(D);
            matWh = (matV.times(matD)).times(matV.transpose());  
            matW = matWh.times(matW);
            w = matW.getArray();
            wMaxDiff = 1.0;
            wslast = new double[nPlanes][nPlanes];
            wslaststd = new double[nPlanes][nPlanes];
            for (p = 0 ; p < nPlanes; p++) {
            	for (i = 0; i < nPlanes; i++) {
            		wslast[p][i] = w[p][i];
            	}
            }
            iterations = 0;
        	while ((wMaxDiff >= endTolerance) && (iterations < maxIterations)) {
        		iterations++;
        		fireProgressStateChanged("iterations = " + iterations);
        		fireProgressStateChanged(40 + (60*iterations)/maxIterations);
        		Preferences.debug("wMaxDiff = " + wMaxDiff + " at iteration " + iterations + "\n",
        				Preferences.DEBUG_ALGORITHM);
        		for (p = 0; p < nPlanes; p++) {
        			for (i = 0; i < nPlanes; i++) {
	        	    	E1[i] = 0.0;
	        	    }
	        	    E2 = 0.0;
		        	// g1(y) = tanh(a1*y) where 1 <= a1 <= 2, often taken as a1 = 1.
		        	// g1'(y) = a1*(1 - tanh(a1*y)*tanh(a1*y))
		        	// g2(y) = y*exp(-y*y/2)
		        	// g2'(y) = (1 - y*y)exp(-y*y/2)
		        	// g3(y) = y*y*y
		        	// g3'(y) = 3*y*y
		        	// wp <- E{zg(wpTz)} - E{g'(wpTz)}wp
		        	if (haveColor) {
		
		                for (j = 0; j < samples; j++) {
		                    wpTz = 0;
		                    for (z = 0; z < zDim; z++) {
		
		                        for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
		                        	iCurrent++;
		                        	index = iCurrent + (colorsRequested * z);
		                        	wpTz += w[p][index]* zvalues[(4 * z * samples) + (4 * j) + i];
		                        	
		                        }
		                    } // for (z = 0; z < zDim; z++)
		                    if (nonlinearFunction == TANH_FUNCTION) {
		                    	g = Math.tanh(a1*wpTz);
		                    	gp = a1*(1.0 - g*g);
		                    }
		                    else if (nonlinearFunction == EXP_FUNCTION) {
		                    	ex = Math.exp(-wpTz*wpTz/2.0);
		                    	g = wpTz*ex;
		                    	gp = (1.0 - wpTz*wpTz)*ex;
		                    }
		                    else {
		                    	g = wpTz * wpTz * wpTz;
		                    	gp = 3.0 * wpTz * wpTz;
		                    }
		                    for (z = 0; z < zDim; z++) {
		                    	
		                        for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
		                        	iCurrent++;
		                        	index = iCurrent + (colorsRequested * z);
		                        	E1[index] += zvalues[(4 * z * samples) + (4 * j) + i] * g;
		                        }
		                    } // for (z = 0; z < zDim; z++)
		                    E2 += gp;
		                } // for (j = 0; j < samples; j++)
		            } // if haveColor
		            else { // not color
		
		                for (j = 0; j < samples; j++) {
		                    wpTz = 0;
		                    for (z = 0; z < zDim; z++) {
		                    	wpTz += w[p][z] * zvalues[(z * samples) + j];
		                    } // for (z = 0; z < zDim; z++)
		                    if (nonlinearFunction == TANH_FUNCTION) {
		                    	g = Math.tanh(a1*wpTz);
		                    	gp = a1*(1.0 - g*g);
		                    }
		                    else if (nonlinearFunction == EXP_FUNCTION) {
		                    	ex = Math.exp(-wpTz*wpTz/2.0);
		                    	g = wpTz*ex;
		                    	gp = (1.0 - wpTz*wpTz)*ex;
		                    }
		                    else {
		                    	g = wpTz * wpTz * wpTz;
		                    	gp = 3.0 * wpTz * wpTz;
		                    }
		                    for (z = 0; z < zDim; z++) {
		                    	E1[z] += zvalues[(z * samples) + j] * g;
		                    }
		                    E2 += gp;
		                } // for (j = 0; j < samples; j++)
		            } // else not color
		        	for (i = 0; i < nPlanes; i++) {
	                	E1[i] = E1[i]/samples;
	                }
	                E2 = E2/samples;
	                for (i = 0; i < nPlanes; i++) {
	                	w[p][i] = E1[i] - E2*w[p][i];
	                }	
        		} // for (p = 0; p < nPlanes; p++)
        		matW = new Matrix(w);
            	matWWT = matW.times(matW.transpose());
            	WWT = matWWT.getArray();
            	// WWT = V * (diagonal eigenvalues) * V'
                // In EigevalueDecomposition the columns of V represent the eigenvectors
                // WWT**(-1/2) = v * 1/sqrt(diagonal eigenvalues) * V'
                Eigenvalue.decompose( WWT, V, eigenvalue, e1 );
                for (i = 0; i < nPlanes; i++) {
                	D[i][i] = 1.0/Math.sqrt(eigenvalue[i]);
                }
                matV = new Matrix(V);
                matD = new Matrix(D);
                matWh = (matV.times(matD)).times(matV.transpose());  
                matW = matWh.times(matW);
                w = matW.getArray();
        		wMaxDiff = 0;
        		for (p = 0; p < nPlanes; p++) {
        		    if (w[p][0] < 0) {
    		    	    for (i = 0; i < nPlanes; i++) {
    		    		    wstd[p][i] = -w[p][i];
    		    	    }
        		    }
        		    else {
        		    	for (i = 0; i < nPlanes; i++) {
        		    		wstd[p][i] = w[p][i];
        		    	}
        		    }
        		    if (wslast[p][0] < 0) {
    		    	    for (i = 0; i < nPlanes; i++) {
    		    		    wslaststd[p][i] = -wslast[p][i];
    		    	    }
        		    }
        		    else {
        		    	for (i = 0; i < nPlanes; i++) {
        		    		wslaststd[p][i] = wslast[p][i];
        		    	}
        		    }
        		}
        		for (p = 0; p < nPlanes; p++) {
		        	for (i = 0; i < nPlanes; i++) {
		        		if (Math.abs(wstd[p][i] - wslaststd[p][i]) > wMaxDiff) {
		        		    wMaxDiff = Math.abs(wstd[p][i] - wslaststd[p][i]);	
		        		}
		        		wslast[p][i] = w[p][i];
		        	}
        		}
        	} // while ((wMaxDiff >= endTolerance) && (iterations < maxIterations))
        	for (p = 0; p < nPlanes; p++) {
	        	if (haveColor) {
		        	for (j = 0; j < samples; j++) {
	                    result[j] = 0;
	                    for (z = 0; z < zDim; z++) {
	
	                        for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
	                        	iCurrent++;
	                        	index = iCurrent + (colorsRequested * z);
	                        	result[j] += w[p][index]* zvalues[(4 * z * samples) + (4 * j) + i];
	                        }
	                    } // for (z = 0; z < zDim; z++)
		        	} // for (j = 0; j < samples; j++)
	        	} // if (haveColor)
	        	else {
	        		for (j = 0; j < samples; j++) {
	                    result[j] = 0;
	                    for (z = 0; z < zDim; z++) {
	                    	result[j] += w[p][z] * zvalues[(z * samples) + j];
	                    } // for (z = 0; z < zDim; z++)
	        		} // for (j = 0; j < samples; j++)
	        	}
	        	try {
	                destImage[p].importData(0, result, true);
	            } catch (IOException error) {
	                displayError("AlgorithmPrincipalComponents: IOException on averaged destination image import data");
	
	                setCompleted(false);
	
	                return;
	            }
	            if (selfTest) {
	            	Preferences.debug("Largest FFT coefficients in component " + p + "\n");
	            	for (i = 0; i < length; i++) {
	            		resultImag[i] = 0.0;
	            	}
	                fft = new FFTUtility(result, resultImag, 1, length, 1, -1, FFTUtility.FFT);	
	                fft.run();
	                for (i = 0; i < length; i++) {
	                	resultMag[i] = Math.sqrt(result[i]*result[i] + resultImag[i]*resultImag[i]);
	                }
	                for (i = 0; i < length; i++) {
	                	tag[i] = i;
	                }
	                fft.sortg(resultMag, length, tag);
	                for (i = length-1; i > length-100; i--) {
	                	Preferences.debug("Mag = " + resultMag[i] + " Index = " + Math.round(tag[i]) + " Real = " + result[(int)Math.round(tag[i])] + " Imag = " +
	                			                                                 resultImag[(int)Math.round(tag[i])] + "\n");
	                }
	            }
        	} // for (p = 0; p < nPlanes; p++)
        	for (p = 0; p < nPlanes; p++) {
	        	for (i = 0; i < nPlanes; i++) {
	        		Preferences.debug(" w["+p+"]["+i+"] = " + w[p][i] + "\n", Preferences.DEBUG_ALGORITHM);
	        	}
        	}
        	matW = new Matrix(w);
        	matScale = matW.times(matWh);
        	scale = matScale.getArray();
        	for (p = 0; p < nPlanes; p++) {
	        	for (i = 0; i < nPlanes; i++) {
	        		Preferences.debug(" scale["+p+"]["+i+"] = " + scale[p][i] + "\n", Preferences.DEBUG_ALGORITHM);
	        	}
        	}
        } // else if (icAlgorithm == SYMMETRIC_ORTHOGONALIZATION)
        else { // icAlgorithm == MAXIMUM_LIKELIHOOD_ESTIMATION
        	// Table 9.2 has not whitening but notes that in practice, whitening combined with PCA may often
        	// be useful.  Under 9.4 examples the text states "Here, we use whitened data.  This is not strictly
        	// necessary, but the algorithms converge much better with whitened data.
        	// Compute the correlation matrix
        	result = new double[samples];
        	for (i = 0; i < nPlanes; i++) {

                for (j = 0; j < nPlanes; j++) {
                    covar[i][j] = 0.0;
                }
            }
        	
        	if (haveColor) {

                for (j = 0; j < samples; j++) {

                    for (z = 0; z < zDim; z++) {

                        for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
                        	iCurrent++;
                            x[iCurrent + (colorsRequested * z)] = zvalues[(4 * z * samples) + (4 * j) + i];
                        }
                    }

                    for (k = 0; k < nPlanes; k++) {

                        for (m = 0; m < nPlanes; m++) {
                            covar[k][m] += x[k] * x[m];
                        }
                    }
                }
            } // if haveColor
            else { // not color

                for (j = 0; j < samples; j++) {

                    for (z = 0; z < zDim; z++) {
                        x[z] = zvalues[(z * samples) + j];
                    }

                    for (k = 0; k < nPlanes; k++) {

                        for (m = 0; m < nPlanes; m++) {
                            covar[k][m] += x[k] * x[m];
                        }
                    }
                }
            } // else not color

            for (i = 0; i < nPlanes; i++) {

                for (j = 0; j < nPlanes; j++) {
                    covar[i][j] = (covar[i][j] / samples);
                }
            }
            
            // Choose an initial (e.g, random) separating matrix B.
            // Under 9.4 examples the text states "The algorithms were always initialized so that
            // B was the identity matrix.
            B = new double[nPlanes][nPlanes];
            Bstd = new double[nPlanes][nPlanes];
            Blast = new double[nPlanes][nPlanes];
            Blaststd = new double[nPlanes][nPlanes];
            for (i = 0; i < nPlanes; i++) {
            	B[i][i] = 1.0;
            	Blast[i][i] = 1.0;
            }
            y = new double[nPlanes];
            alpha = new double[nPlanes];
            beta = new double[nPlanes];
            Eygy = new double[nPlanes];
            Egpy = new double[nPlanes];
            EgyyT = new double[nPlanes][nPlanes];
            diagAlpha = new double[nPlanes][nPlanes];
            D = new double[nPlanes][nPlanes];
            maxBDiff = 1.0;
            // The nonlinear function g is taken as the tanh function.
            // g'(y) = 1.0 - tanh(y)*tanh(y)
            iterations = 0;
            while ((maxBDiff >= endTolerance) && (iterations < maxIterations)) {
            	iterations++;
            	fireProgressStateChanged("iterations = " + iterations);
        		fireProgressStateChanged(40 + (60*iterations)/maxIterations);
        		Preferences.debug("maxBDiff = " + maxBDiff + " at iteration " + iterations + "\n",
        				Preferences.DEBUG_ALGORITHM);
            	for (i = 0; i < nPlanes; i++) {
        			Eygy[i] = 0.0;
        			Egpy[i] = 0.0;
        			for (j = 0; j < nPlanes; j++) {
        				EgyyT[i][j] = 0.0;
        			}
        		}
            	if (haveColor) {
	                for (j = 0; j < samples; j++) {
	                    for (z = 0; z < zDim; z++) {
	                        for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
	                        	iCurrent++;
	                        	index = iCurrent + (colorsRequested * z);
	                        	y[index] = 0;
	                        	for (z2 = 0; z2 < zDim; z2++) {
	                        		for (i2Current = -1, i2 = 1; (i2 < 4) && (colorArray[i2]); i2++) {
	                        			i2Current++;
	                        			index2 = i2Current + (colorsRequested * z2);
	                        			y[index] += B[index][index2] * zvalues[(4 * z2 * samples) + (4 * j) + i2];
	                        		} // for (i2 = 1; i2 < 4; i2++)
	                        	} // for (z2 = 0; z2 < zDim; z2++)
	                        	ty = Math.tanh(y[index]);
	                        	Eygy[index] += y[index]*ty;
	                        	Egpy[index] += (1.0 - ty * ty);
	                        } // for (i = 1;i < 4; i++)
	                    } // for (z = 0; z < zDim; z++)
	                    for (k = 0; k < nPlanes; k++) {
	                    	tk = Math.tanh(y[k]);
	                    	for (m = 0; m < nPlanes; m++) {
	                    		EgyyT[k][m] += tk * y[m];
	                    	}
	                    }
	                } // for (j = 0; j < samples; j++)
	            } // if haveColor
	            else { // not color
	                for (j = 0; j < samples; j++) {
	                    for (z = 0; z < zDim; z++) {
	                    	y[z] = 0;
	                    	for (z2 = 0; z2 < zDim; z2++) {
	                    		y[z] += B[z][z2] * zvalues[(z2 * samples) + j];
	                    	} // for (z2 = 0; z2 < zDim; z2++)
	                    	ty = Math.tanh(y[z]);
                        	Eygy[z] += y[z]*ty;
                        	Egpy[z] += (1.0 - ty * ty);
	                    } // for (z = 0; z < zDim; z++)
	                    for (k = 0; k < nPlanes; k++) {
	                    	tk = Math.tanh(y[k]);
	                    	for (m = 0; m < nPlanes; m++) {
	                    		EgyyT[k][m] += tk * y[m];
	                    	}
	                    }
	                } // for (j = 0; j < samples; j++)
	            } // else not color
            	for (i = 0; i < nPlanes; i++) {
            		Eygy[i] = Eygy[i]/samples;
            		beta[i] = -Eygy[i];
            		Egpy[i] = Egpy[i]/samples;
            		alpha[i] = -1.0/(beta[i] + Egpy[i]);
            		for (j = 0; j < nPlanes; j++) {
            			EgyyT[i][j] = EgyyT[i][j]/samples;
            		}
            	}
            	// Update the separating matrix B
            	for (i = 0; i < nPlanes; i++) {
            		diagAlpha[i][i] = alpha[i];
            		EgyyT[i][i] = EgyyT[i][i] + beta[i];
            	}
            	matAlpha = new Matrix(diagAlpha);
            	matBeta = new Matrix(EgyyT);
            	matB = new Matrix(B);
            	prod = ((matAlpha.times(matBeta)).times(matB)).getArray();
            	for (i = 0; i < nPlanes; i++) {
            		for (j = 0; j < nPlanes; j++) {
            			B[i][j] = B[i][j] + prod[i][j];
            		}
            	}
            	// Decorrelate and normalize
            	matB = new Matrix(B);
            	matC = new Matrix(covar);
            	BCBT = ((matB.times(matC)).times(matB.transpose())).getArray();
            	// BCBT = V * (diagonal eigenvalues) * V'
                // In EigevalueDecomposition the columns of V represent the eigenvectors
                // BCBT**(-1/2) = v * 1/sqrt(diagonal eigenvalues) * V'
                Eigenvalue.decompose(BCBT, V, eigenvalue, e1 );
                for (i = 0; i < nPlanes; i++) {
                	D[i][i] = 1.0/Math.sqrt(eigenvalue[i]);
                }
                matV = new Matrix(V);
                matD = new Matrix(D);
                matWh = (matV.times(matD)).times(matV.transpose()); 
                B = (matWh.times(matB)).getArray();
            	maxBDiff = 0.0;
            	// Make signs the same as on previous iteration by always making the first element positive
            	for (i = 0; i < nPlanes; i++) {
            		if (B[i][0] < 0.0) {
            			for (j = 0; j < nPlanes; j++) {
            				Bstd[i][j] = -B[i][j];
            			}
            		}
            		else {
            			for (j = 0; j < nPlanes; j++) {
            				Bstd[i][j] = B[i][j];
            			}
            		}
            		if (Blast[i][0] < 0.0) {
            			for (j = 0; j < nPlanes; j++) {
            				Blaststd[i][j] = -Blast[i][j];
            			}
            		}
            		else {
            			for (j = 0; j < nPlanes; j++) {
            				Blaststd[i][j] = Blast[i][j];
            			}
            		}
            	}
            	for (i = 0; i < nPlanes; i++) {
            		for (j = 0; j < nPlanes; j++) {
            			if (Math.abs(Bstd[i][j] - Blaststd[i][j]) > maxBDiff) {
            				maxBDiff = Math.abs(Bstd[i][j] - Blaststd[i][j]);
            			}
            			Blast[i][j] = B[i][j];
            		}
            	}
            } // while ((maxBDiff >= endTolerance) && (iterations < maxIterations))
            for (p = 0; p < nPlanes; p++) {
	        	if (haveColor) {
		        	for (j = 0; j < samples; j++) {
	                    result[j] = 0;
	                    for (z = 0; z < zDim; z++) {
	
	                        for (iCurrent = -1, i = 1; (i < 4) && (colorArray[i]); i++) {
	                        	iCurrent++;
	                        	index = iCurrent + (colorsRequested * z);
	                        	result[j] += B[p][index]* zvalues[(4 * z * samples) + (4 * j) + i];
	                        }
	                    } // for (z = 0; z < zDim; z++)
		        	} // for (j = 0; j < samples; j++)
	        	} // if (haveColor)
	        	else {
	        		for (j = 0; j < samples; j++) {
	                    result[j] = 0;
	                    for (z = 0; z < zDim; z++) {
	                    	result[j] += B[p][z] * zvalues[(z * samples) + j];
	                    } // for (z = 0; z < zDim; z++)
	        		} // for (j = 0; j < samples; z++)
	        	}
	        	try {
	                destImage[p].importData(0, result, true);
	            } catch (IOException error) {
	                displayError("AlgorithmPrincipalComponents: IOException on averaged destination image import data");
	
	                setCompleted(false);
	
	                return;
	            }
	            
	            if (selfTest) {
	            	Preferences.debug("Largest FFT coefficients in component " + p + "\n");
	            	for (i = 0; i < length; i++) {
	            		resultImag[i] = 0.0;
	            	}
	                fft = new FFTUtility(result, resultImag, 1, length, 1, -1, FFTUtility.FFT);	
	                fft.run();
	                for (i = 0; i < length; i++) {
	                	resultMag[i] = Math.sqrt(result[i]*result[i] + resultImag[i]*resultImag[i]);
	                }
	                for (i = 0; i < length; i++) {
	                	tag[i] = i;
	                }
	                fft.sortg(resultMag, length, tag);
	                for (i = length-1; i > length-100; i--) {
	                	Preferences.debug("Mag = " + resultMag[i] + " Index = " + Math.round(tag[i]) + " Real = " + result[(int)Math.round(tag[i])] + " Imag = " +
	                			                                                 resultImag[(int)Math.round(tag[i])] + "\n");
	                }
	            }
        	} // for (p = 0; p < nPlanes; p++)
            for (p = 0; p < nPlanes; p++) {
            	for (i = 0; i < nPlanes; i++) {
            		Preferences.debug("B["+p+"]["+i+"] = " + B[p][i] + "\n",Preferences.DEBUG_ALGORITHM);
            	}
            }
            matB = new Matrix(B);
        	matScale = matB.times(matWh);
        	scale = matScale.getArray();
        	for (p = 0; p < nPlanes; p++) {
	        	for (i = 0; i < nPlanes; i++) {
	        		Preferences.debug(" scale["+p+"]["+i+"] = " + scale[p][i] + "\n", Preferences.DEBUG_ALGORITHM);
	        	}
        	}
        } // else icAlgorithm == MAXIMUM_LIKELIHOOD_ESTIMATION

        fireProgressStateChanged(100);
        setCompleted(true);
    }

}
