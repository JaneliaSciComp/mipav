package gov.nih.mipav.model.algorithms;


import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;

/**
 * This routines except the SLOW routines are ported from FORTRAN to Java from:
 *     ALGORITHM 749, COLLECTED ALGORITHMS FROM ACM.
*      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
*      VOL. 21, NO. 4, December, 1995, P.  372--378.
*
* This file contains 4 files separated by lines of the form
*         C*** filename
*
* The filenames in this file are:
*
* driver.f             driverd.f            fct.f               
* fctd.f                                                        
*  driver.f and fct.f are float versions and driverd.f and fctd.f are double versions. 
* <p>
* From <a href="http://www.acm.org/publications/policies/softwarecrnotice/">the ACM website</a>:
* 
* <pre>
* ACM Software License Agreement
* 
*  All software, both binary and source published by the Association for Computing Machinery (hereafter, Software) is copyrighted by the Association (hereafter, ACM) and ownership of all right, title and interest in and to the Software remains with ACM. By using or copying the Software, User agrees to abide by the terms of this Agreement.
*  Noncommercial Use
* 
*  The ACM grants to you (hereafter, User) a royalty-free, nonexclusive right to execute, copy, modify and distribute both the binary and source code solely for academic, research and other similar noncommercial uses, subject to the following conditions:
* 
*  1. User acknowledges that the Software is still in the development stage and that it is being supplied &quot;as is,&quot; without any support services from ACM. Neither ACM nor the author makes any representations or warranties, express or implied, including, without limitation, any representations or warranties of the merchantability or fitness for any particular purpose, or that the application of the software, will not infringe on any patents or other proprietary rights of others.
*  2. ACM shall not be held liable for direct, indirect, incidental or consequential damages arising from any claim by User or any third party with respect to uses allowed under this Agreement, or from any use of the Software.
*  3. User agrees to fully indemnify and hold harmless ACM and/or the author(s) of the original work from and against any and all claims, demands, suits, losses, damages, costs and expenses arising out of the User's use of the Software, including, without limitation, arising out of the User's modification of the Software.
*  4. User may modify the Software and distribute that modified work to third parties provided that: (a) if posted separately, it clearly acknowledges that it contains material copyrighted by ACM (b) no charge is associated with such copies, (c) User agrees to notify ACM and the Author(s) of the distribution, and (d) User clearly notifies secondary users that such modified work is not the original Software.
*  5. User agrees that ACM, the authors of the original work and others may enjoy a royalty-free, non-exclusive license to use, copy, modify and redistribute these modifications to the Software made by the User and distributed to third parties as a derivative work under this agreement.
*  6. This agreement will terminate immediately upon User's breach of, or non-compliance with, any of its terms. User may be held liable for any copyright infringement or the infringement of any other proprietary rights in the Software that is caused or facilitated by the User's failure to abide by the terms of this agreement.
*  7. This agreement will be construed and enforced in accordance with the law of the state of New York applicable to contracts performed entirely within the State. The parties irrevocably consent to the exclusive jurisdiction of the state or federal courts located in the City of New York for all disputes concerning this agreement. 
* 
*  Commercial Use
* 
*  Any User wishing to make a commercial use of the Software must contact ACM at permissions@acm.org to arrange an appropriate license. Commercial use includes (1) integrating or incorporating all or part of the source code into a product for sale or license by, or on behalf of, User to third parties, or (2) distribution of the binary or source code to third parties for use with a commercial product sold or licensed by, or on behalf of, User.
* 
*  Revised 6/98
* </pre>
* 
* 
* SLOW routines adapted from:
* DCT and IDCT - listing 1
 * Copyright (c) 2001 Emil Mikulic.
 * http://unix4lyfe.org/dct/
 *
 * Feel free to do whatever you like with this code.
 * Feel free to credit me.
 */
 

public class DiscreteCosineTransform extends AlgorithmBase {

    // ~ Static fields/initializers
    // -----------------------
	
	private int constructionMethod;
    public static final int CONSTRUCTION_NONE = 0;
    public static final int GAUSSIAN = 2;
    public static final int BUTTERWORTH = 3;
    public static final int CHEBYSHEV_TYPE_I = 5;
    public static final int CHEBYSHEV_TYPE_II = 6;
    
    private int filterType;
    private double f1;
    private double f2;
	public static final int LOWPASS = 1;
    public static final int HIGHPASS = 2;
    public static final int BANDPASS = 3;
    public static final int BANDSTOP = 4;
    private int filterOrder;
    private double epsilon;  // maximum ripple in Chebyshev filters

    private int transformLength;
    
    public final static int randomData = 1;
    
    public final static int sinusoidalData = 2;
    
    // testDataType must be either randomData or sinusoidalData
    private int testDataType;
    
    private boolean printData;
    
    private final int MAXPOW = 20; // Parameter which determines the maximum transform length
    // permitted.  Maximum length = 2**MAXPOW.
    
    private final double RT2INV = 1.0/Math.sqrt(2.0);
    
    // Can only use a fast algorithm with a power of 2
    // Otherwise use a slow version using the basic formula.
    private boolean fastAlgorithm = true;
    
    // In COMMON FCTLEN
    private int LENGTH = 0; // The transform length for which the array C of cosine coefficients
                            // is currently set up.  Used by FCT and IFCT to determine whether 
                            // transformLength has changed.
    private int IPOW = 0;   // The base-2 logarithm of LENGTH.  Used in FCT and IFCT to determine
                            // the number of stages of butterflies or summations.
    private int dims[];
    
    private int xDim;
    
    private int yDim;
    
    private ModelImage transformImage;
    private ModelImage inverseImage;
    
    public DiscreteCosineTransform() {
		
	}
    
    public DiscreteCosineTransform(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg, int constructionMethod,
    		int filterType, double f1, double f2, int filterOrder, double epsilon) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
		this.constructionMethod = constructionMethod;
		this.filterType = filterType;
		this.f1 = f1;
		this.f2 = f2;
		this.filterOrder = filterOrder;
		this.epsilon = epsilon;
	}

    public DiscreteCosineTransform(boolean fastAlgorithm, int dims[], int testDataType, boolean printData, int filterType,
    		double filterVal1, double filterVal2) {
    	this.dims = dims;
    	this.fastAlgorithm = fastAlgorithm;
        this.testDataType = testDataType;
        this.printData = printData;
        if (dims.length == 1) {
        	transformLength = dims[0];
        }
        else {
        	xDim = dims[0];
        	yDim = dims[1];
        	transformLength = xDim * yDim;
        }
    }
    
    public void runAlgorithm() {
		int zDim;
		double doubleBuffer[];
		int xTest;
		int yTest;
		int length;
		int z;
		xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        dims = new int[2];
        dims[0] = xDim;
        dims[1] = yDim;
        length = xDim * yDim;
        doubleBuffer = new double[length];
        zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
         
        xTest = xDim;
        while ((xTest % 2) == 0) {
        	xTest = xTest/2;
        }
        if (xTest != 1) {
        	fastAlgorithm = false;
        }
        yTest = yDim;
        while ((yTest % 2) == 0) {
        	yTest = yTest/2;
        }
        if (yTest != 1) {
        	fastAlgorithm = false;
        }
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, doubleBuffer); // locks and releases lock
            } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Discrete Cosine Transform: Image(s) locked", true);

                return;
            }
        	if (fastAlgorithm) {
        		FCT2D(doubleBuffer, xDim, yDim);
        	}
        	else {
        		SLOW2DFCT(doubleBuffer, xDim, yDim);
        	}
        	try {
                transformImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Discrete Cosine Transform: Image(s) locked", true);

                return;
             }
        	if (constructionMethod != CONSTRUCTION_NONE) {
        		if (constructionMethod == GAUSSIAN) {
                    makeGaussianFilter(doubleBuffer, f1);
                }
        		else if (constructionMethod == BUTTERWORTH) {
        			makeButterworthFilter(doubleBuffer, f1, f2);
        		}
        		else if (constructionMethod == CHEBYSHEV_TYPE_I) {
        		    makeChebyshevTypeIFilter(doubleBuffer, f1, f2);	
        		}
        		else if (constructionMethod == CHEBYSHEV_TYPE_II) {
        		    makeChebyshevTypeIIFilter(doubleBuffer, f1, f2);	
        		}
        	}
        	// Inverse transform
        	if (fastAlgorithm) {
        		IFCT2D(doubleBuffer, xDim, yDim);
        	}
        	else {
        		SLOW2DIFCT(doubleBuffer, xDim, yDim);
        	}
        	try {
                inverseImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Discrete Cosine Transform: Image(s) locked", true);

                return;
             }
        }
        transformImage.calcMinMax();
        inverseImage.calcMinMax();
        setCompleted(true);
        return;
	}
    
    public void runTest() {
    	int I; // For counter used to address each array element in turn.
    	double F[]; // Data array passed to the subroutines FCT and IFCT
    	double C[]; // Array of cosine coefficients passed to FCT and IFCT.
    	double FSAVE[]; // Array to save the original data so it can be compared to the recovered data.
    	double DCTSAV[]; // Array to save the DCT coefficients so that they can be printed out later.
    	int IX[] = new int [1]; // The three seeds for the random number generator.
    	int IY[] = new int[1];
    	int IZ[] = new int[1];
    	int IFAULT[] = new int[1]; // Fault indicator set by subroutines FCT and IFCT.
    	double SUM; // Temporary variable to hold running sum during ms error
    	            // calculation
    	double TEMP; // Temporary variable used during ms error calculation to
    	             // hold error between original and recovered data point.
    	double TMSERR; // Mean square error between original and recovered data.
    	if ((testDataType != randomData) && (testDataType != sinusoidalData)) {
    		MipavUtil.displayError("testDataType must be randomData or sinusoidalData");
    		return;	
    	}
    	
    	// Initialize seeds for random number generator
    	IX [0]= 1234;
    	IY[0] = 42;
    	IZ[0] = 1000;
    	
    	// Initialize data and save it for later
    	F = new double[transformLength];
    	C = new double[transformLength];
    	FSAVE = new double[transformLength];
    	DCTSAV = new double[transformLength];
    	
    	for (I = 0; I < transformLength; I++) {
    	    if (testDataType == randomData) {
    	        F[I] = RANDOM(IX, IY, IZ);	
    	    }
    	    else if (testDataType == sinusoidalData) {
    	        F[I] = 10.0*Math.sin(2.0*Math.PI*(I+1)/(double)transformLength);	
    	    }
    	    FSAVE[I] = F[I];
    	} // for (I = 0; I < transformLength; I++)
    	
    	// Perform the forward cosine transform
    	System.out.println("Calling FCT");
    	Preferences.debug("Calling FCT\n", Preferences.DEBUG_ALGORITHM);
    	if (fastAlgorithm && (dims.length == 2)) {
    		FCT2D(F, xDim, yDim);
    	}
    	else if (fastAlgorithm) {
    	   FCT(F, C, transformLength, IFAULT);
    	}
    	else if (dims.length == 2) {
    		SLOW2DFCT(F, xDim, yDim);
    	}
    	else {
    		SLOWFCT(F, transformLength);
    	}
    	
    	// Save transformed data for later
    	
    	for (I = 0; I < transformLength; I++) {
    		DCTSAV[I] = F[I];
    	}
    	
    	System.out.println("FCT has been set. IFAULT[0] = " + IFAULT[0]);
    	Preferences.debug("FCT has been set. IFAULT[0] = " + IFAULT[0] + "\n", Preferences.DEBUG_ALGORITHM);
    	if (IFAULT[0] == 0) {
    		System.out.println("Succesful completion of FCT");
    		Preferences.debug("Succesful completion of FCT\n", Preferences.DEBUG_ALGORITHM);
    	}
    	else if (IFAULT[0] == 1) {
    		System.out.println("Error -- transformLength is <= 1");
    		Preferences.debug("Error -- transformLength is <= 1\n", Preferences.DEBUG_ALGORITHM);
    	}
    	else if (IFAULT[0] == 2) {
    		System.out.println("Error -- transformLength = " + transformLength + " is greater than 2**MAXPOW = " + 
    	                         Math.pow(2, MAXPOW));
    		Preferences.debug("Error -- transformLength = " + transformLength + " is greater than 2**MAXPOW = " + 
    	                         Math.pow(2, MAXPOW) + "\n", Preferences.DEBUG_ALGORITHM);
    	}
    	else if (IFAULT[0] == 3) {
    		System.out.println("Error -- transformLength is not a power of two.");
    		Preferences.debug("Error -- transformLength is not a power of two.\n", Preferences.DEBUG_ALGORITHM);
    	}
    	
    	// Perform the inverse cosine transform
    	
    	System.out.println("Calling IFCT");
    	Preferences.debug("Calling IFCT\n", Preferences.DEBUG_ALGORITHM);
    	if (fastAlgorithm && (dims.length == 2)) {
    		IFCT2D(F, xDim, yDim);
    	}
    	else if (fastAlgorithm) {
    	    IFCT(F, C, transformLength, IFAULT);
    	}
    	else if (dims.length == 2) {
    		SLOW2DIFCT(F, xDim, yDim);
    	}
    	else {
    		SLOWIFCT(F, transformLength);
    	}
    	System.out.println("IFCT has been set. IFAULT[0] = " + IFAULT[0]);
    	Preferences.debug("IFCT has been set. IFAULT[0] = " + IFAULT[0] + "\n", Preferences.DEBUG_ALGORITHM);
    	if (IFAULT[0] == 0) {
    		System.out.println("Succesful completion of IFCT");
    		Preferences.debug("Succesful completion of IFCT\n", Preferences.DEBUG_ALGORITHM);
    	}
    	else if (IFAULT[0] == 1) {
    		System.out.println("Error -- transformLength is <= 1");
    		Preferences.debug("Error -- transformLength is <= 1\n", Preferences.DEBUG_ALGORITHM);
    	}
    	else if (IFAULT[0] == 2) {
    		System.out.println("Error -- transformLength = " + transformLength + " is greater than 2**MAXPOW = " + 
    	                         Math.pow(2, MAXPOW));
    		Preferences.debug("Error -- transformLength = " + transformLength + " is greater than 2**MAXPOW = " + 
    	                         Math.pow(2, MAXPOW) + "\n", Preferences.DEBUG_ALGORITHM);
    	}
    	else if (IFAULT[0] == 3) {
    		System.out.println("Error -- transformLength is not a power of two.");
    		Preferences.debug("Error -- transformLength is not a power of two.\n", Preferences.DEBUG_ALGORITHM);
    	}
    	
    	// Calculate mean-square error between original and recovered data
    	SUM = 0.0;
    	for (I = 0; I < transformLength; I++) {
    		TEMP = F[I] - FSAVE[I];
    		SUM = SUM + TEMP*TEMP;
    	}
    	TMSERR = SUM/(double)(transformLength-1);
    	System.out.println("Mean square error = " + TMSERR);
    	Preferences.debug("Mean square error = " + TMSERR + "\n", Preferences.DEBUG_ALGORITHM);
    	
    	if (printData) {
    		for (I = 0; I < transformLength; I++) {
    			System.out.println("I = " + (I+1) + "  Original data " + FSAVE[I] + 
    					"  After FCT " + DCTSAV[I] + "  After IFCT = " + F[I]);
    			Preferences.debug("I = " + (I+1) + "  Original data " + FSAVE[I] + 
    					"  After FCT " + DCTSAV[I] + "  After IFCT = " + F[I] + "\n", Preferences.DEBUG_ALGORITHM);
    		}
    	}
    } 
    
    public double RANDOM(int IX[],int IY[], int IZ[]) {
           double r;
           double rfloor;
           double ans;
    //     THIS IS THE WICHMANN & HILL RANDOM NUMBER GENERATOR
    //     ALGORITHM AS-183, APPLIED STATISTICS VOL 31 NO 2, PP 188-190
    
          IX[0] = 171 * (IX[0] % 177) - 2  * (IX[0] /177);
          IY[0] = 172 * (IY[0] % 176) - 35 * (IY[0]/176);
          IZ[0] = 170 * (IZ[0] % 178) - 63 * (IZ[0]/178);
    
          if (IX[0] < 0) IX[0] = IX[0] + 30269;
          if (IY[0] < 0) IY[0] = IY[0] + 30307;
          if (IZ[0] < 0) IZ[0] = IZ[0] + 30323;
    
          r = (double)IX[0]/ 30269.0 + (double)IY[0] / 30307.0 + (double)IZ[0] / 30323.0;
          rfloor = Math.floor(r);
          ans = r - rfloor;
          return ans;
    }
    
    private void FCT2D(double F[], int xDim, int yDim) {
    	int x;
    	int y;
    	int IFAULT[] = new int[1];
        // F Array containing data to be transformed.  On exit, it contains
        // the transformed elements.
    	double FROW[] = new double[xDim];
    	double CROW[] = new double[xDim];
    	
    	// Transform on rows
    	for (y = 0; y < yDim; y++) {
    	    for (x = 0; x < xDim; x++) {
    	    	FROW[x] = F[x + y * xDim];
    	    }
            LENGTH = 0;
    	    FCT(FROW, CROW, xDim, IFAULT);
    	    for (x = 0; x < xDim; x++) {
    	    	F[x + y * xDim] = FROW[x];
    	    }
    	}
    	
    	
    	double FCOL[] = new double[yDim];
    	double CCOL[] = new double[yDim];
    	// Transform on columns
    	for (x = 0; x < xDim; x++) {
    	    for (y = 0; y < yDim; y++) {
    	    	FCOL[y] = F[x + y * xDim];
    	    }
    	    LENGTH = 0;
    	    FCT(FCOL, CCOL, yDim, IFAULT);
    	    for (y = 0; y < yDim; y++) {
    	    	F[x + y * xDim] = FCOL[y];
    	    }
    	}
    	
    	
    }
    
    private void IFCT2D(double F[], int xDim, int yDim) {
    	int x;
    	int y;
    	int IFAULT[] = new int[1];
        // F Array containing data to be transformed.  On exit, it contains
        // the transformed elements.
    	
    	double FROW[] = new double[xDim];
    	double CROW[] = new double[xDim];
    	// Transform on rows
    	for (y = 0; y < yDim; y++) {
    	    for (x = 0; x < xDim; x++) {
    	    	FROW[x] = F[x + y * xDim];
    	    }
    	    LENGTH = 0;
    	    IFCT(FROW, CROW, xDim, IFAULT);
    	    for (x = 0; x < xDim; x++) {
    	    	F[x + y * xDim] = FROW[x];
    	    }
    	}
    	
    	double FCOL[] = new double[yDim];
    	double CCOL[] = new double[yDim];
    	// Transform on columns
    	for (x = 0; x < xDim; x++) {
    	    for (y = 0; y < yDim; y++) {
    	    	FCOL[y] = F[x + y * xDim];
    	    }
    	    LENGTH = 0;
    	    IFCT(FCOL, CCOL, yDim, IFAULT);
    	    for (y = 0; y < yDim; y++) {
    	    	F[x + y * xDim] = FCOL[y];
    	    }
    	}
    }
    
    private void FCT(double F[], double C[], int transformLength, int IFAULT[]) {
    	// Fast discrete cosine transform
    	// Arguments:
    	// F      Array containing the data to be transformed.  On exit, it contains
    	//        the transformed sequence.
    	// C      Array containing cosine coefficients as previously calculated by a 
    	//        call to INIFCT.  When this is the first call to FCT or IFCT for the
    	//        current value of transformLength, this array will be initialized by a 
    	//        call to INIFCT.
    	// transformLength  Length the the array F.  transformLength must be a power of two.
    	// IFAULT on exit:
    	//        = 0, if there are no faults.
    	//        = 1, if transformLength <= 1.
    	//        = 2, if N > 2**MAXPOW.  (See subroutine INIFCT below).
    	//        = 3, if 2 <= tranformLength <= 2**MAXPOW but transformLength is not a power of two.
    	// Internal variables:
    	double CFAC; // Cosine multiplicative factor used in butterfly.
    	int I;
    	int IBASE; // Index of base of butterfly.
    	int IBUTFY; // Butterfly counter
    	int IGROUP; // Loop counter for groups of butterflies.
    	int II1; // Index to first element of butterfly.
    	int II2; // Index to second element of butterfly.
        int INCR; // Index increment to move from one group of butterflies to the next.
        int INDEX;
        int ISTAGE; // Counter for stages during butterfly or summation processing.
        int ISTEP; // Counter for steps within a thread during summation processing.
        int ISTPSZ; // Size of step within a thread during summation processing.
    	int IWNGSP; // Width (wingspan) of butterfly.
    	int NGRPS; // Number of groups of butterflies in current stage.
    	int NSTEPS; // Number of steps in current thread during summation processing.
    	int NTHRDS; // Number of summation threads in current summation stage.
    	int NV2; // Set to transformLength/2.
    	double TEMP; // Temporary variable used during butterfly processing.
    	int THREAD;
    	double TWOVN; // Set to 2.0/transformLength.
    	
    	// Check for valid transform size
    	IFAULT[0] = 0;
    	if ((transformLength != LENGTH) || (transformLength <= 1)) {
    		INIFCT(C, transformLength, IFAULT);
    	}
    	if (IFAULT[0] != 0) return;
    	
    	NV2 = transformLength/2;
    	
    	// Scramble the data in F into odd-even ordering
    	SCRAMB(F,transformLength);
    	
    	// Do the butterflies
    	
    	NGRPS = 1;
    	IWNGSP = NV2;
    	INCR = transformLength;
    	for (ISTAGE = IPOW; ISTAGE >= 1; ISTAGE--) {
    	    for (IBUTFY = 1; IBUTFY <= IWNGSP; IBUTFY++) {
    	        CFAC = C[IWNGSP + IBUTFY - 1];
    	        IBASE = 0;
    	        for (IGROUP = 1; IGROUP <= NGRPS; IGROUP++) {
    	            II1 = IBASE + IBUTFY;
    	            II2 = II1 + IWNGSP;
    	            TEMP = F[II2-1];
    	            F[II2-1] = CFAC * (F[II1-1] - TEMP);
    	            F[II1-1] = F[II1-1] + TEMP;
    	            IBASE = IBASE + INCR;
    	        } // for (IGROUP = 1; IGROUP <= NGRPS; IGROUP++)
    	    } // for (IBUTFY = 1; IBUTFY <= IWNGSP; IBUTFY++)
    	    INCR = INCR/2;
    	    IWNGSP = IWNGSP / 2;
    	    NGRPS = NGRPS + NGRPS;
    	} // for (ISTAGE = IPOW; ISTAGE >= 1; ISTAGE--)
    	
    	// Bit-reverse order array F
    	BITREV(F, transformLength);
    	
    	// Do the sums
    	NTHRDS = transformLength/4;
    	ISTPSZ = NV2;
    	NSTEPS = 2;
    	for (ISTAGE = IPOW-1; ISTAGE >= 1; ISTAGE--) {
    	    for (THREAD = 1; THREAD <= NTHRDS; THREAD++) {
    	        INDEX = NTHRDS + THREAD;
    	        for (ISTEP = 1; ISTEP <= NSTEPS-1; ISTEP++) {
    	            F[INDEX-1] = F[INDEX-1] + F[INDEX + ISTPSZ - 1]	;
    	            INDEX = INDEX + ISTPSZ;
    	        } // for (ISTEP = 1; ISTEP <= NSTEPS-1; ISTEP++)
    	    } // for (THREAD = 1; THREAD <= NTHRDS; THREAD++)
    	    NSTEPS = NSTEPS + NSTEPS;
    	    NTHRDS = NTHRDS/2;
    	    ISTPSZ = ISTPSZ/2;
    	} // for (ISTAGE = IPOW-1; ISTAGE >= 1; ISTAGE--)
    	
    	// Scale the result
    	TWOVN = 2.0/(double)transformLength;
    	for (I = 0; I < transformLength; I++) {
    		F[I] = F[I] * TWOVN;
    	}
    	F[0] = F[0] * RT2INV;
    	return;
    }
    
    private void IFCT(double F[], double C[], int transformLength, int IFAULT[]) {
    	// Fast inverse discrete transform
    	
    	// Arguments:
    	// F  Array containing the data that is to be inverse transformed.
    	//    On exit, it contains the inverse transformed data.
    	// C, transformLength, IFAULT  As for subroutine FCT.
    	
    	// Internal variables:
    	// As for subroutine FCT.
    	double TEMP;
    	double CFAC;
    	int NV2;
    	int NTHRDS;
    	int ISTPSZ;
    	int NSTEPS;
    	int ISTAGE;
    	int THREAD;
    	int INDEX;
    	int NGRPS;
    	int IWNGSP;
    	int INCR;
    	int IBUTFY;
    	int IBASE;
    	int IGROUP;
    	int II1;
    	int II2;
    	int ISTEP;
    	
    	// Check transform length and initialize if necessary
    	
    	IFAULT[0] = 0;
    	if ((transformLength != LENGTH) || (transformLength <= 1)) {
    		INIFCT(C, transformLength, IFAULT);
    	}
    	if (IFAULT[0] != 0) {
    		return;
    	}
    	
    	NV2 = transformLength/2;
    	
    	F[0] = F[0] * RT2INV;
    	
    	// Do the sums
    	NTHRDS = 1;
    	ISTPSZ = 2;
    	NSTEPS = NV2;
    	for (ISTAGE = 1; ISTAGE <= IPOW-1; ISTAGE++) {
    	    for (THREAD = 1; THREAD <= NTHRDS; THREAD++) {
    	        INDEX = transformLength - THREAD + 1;	
    	        for (ISTEP = 1; ISTEP <= NSTEPS-1; ISTEP++) {
    	        	F[INDEX-1] = F[INDEX-1] + F[INDEX - ISTPSZ-1];
    	        	INDEX = INDEX - ISTPSZ;
    	        } // for (ISTEP = 1; ISTEP <= NSTEPS-1; ISTEP++)
    	    } // for (THREAD = 1; THREAD <= NTHRDS; THREAD++)
    	    NSTEPS = NSTEPS/2;
    	    NTHRDS = NTHRDS + NTHRDS;
    	    ISTPSZ = ISTPSZ + ISTPSZ;
    	} // for (ISTAGE = 1; ISTAGE <= IPOW-1; ISTAGE++)
    	
    	// Bit-reverse order array F
    	
    	BITREV(F, transformLength);
    	
    	// Do the butterflies
    	NGRPS = NV2;
    	IWNGSP = 1;
    	INCR = 2;
    	for (ISTAGE = 1; ISTAGE <= IPOW; ISTAGE++) {
    	    for (IBUTFY = 1; IBUTFY <= IWNGSP; IBUTFY++) {
    	        CFAC = C[IWNGSP + IBUTFY - 1];
    	        IBASE = 0;
    	        for (IGROUP = 1; IGROUP <= NGRPS; IGROUP++) {
    	        	II1 = IBASE + IBUTFY;
    	        	II2 = II1 + IWNGSP;
    	        	TEMP = CFAC * F[II2-1];
    	        	F[II2-1] = F[II1-1] - TEMP;
    	        	F[II1-1] = F[II1-1] + TEMP;
    	        	IBASE = IBASE + INCR;
    	        } // for (IGROUP = 1; IGROUP <= NGRPS; IGROUP++)
    	    } // for (IBUTFY = 1; IBUTFY <= IWNGSP; IBUTFY++)
    	    INCR = INCR + INCR;
    	    IWNGSP = IWNGSP + IWNGSP;
    	    NGRPS = NGRPS/2;
    	} // for (ISTAGE = 1; ISTAGE <= IPOW; ISTAGE++)
    	
    	// Unscramble from odd-even ordering to natural ordering
    	USCRAM(F, transformLength);
    	return;
    	
    }
    
    private void BITREV(double F[], int transformLength) {
    	// Rearranges the data in the array F into bit-reverse order.
    	
    	// Arguments:
    	// F  Array containing the data to be rearranged into bit-reversed
    	//    order.
    	// transformLength Length of the array F.  N must be a power of two.
    	
    	// Internal variables:
    	int I;
    	int J;
    	int NV2;
    	int M;
    	double TEMP;
    	
    	if (transformLength <= 2) {
    		return;
    	}
    	NV2 = transformLength/2;
    	J = 1;
    	for (I = 1; I <= transformLength; I++) {
    	    if (I < J) {
    	    	TEMP = F[J-1];
    	    	F[J-1] = F[I-1];
    	    	F[I-1] = TEMP;
    	    }
    	    M = NV2;
    	    while( J > M) {
    	    	J = J - M;
    	    	M = (M + 1)/2;
    	    }
    	    J = J + M;
    	} // for (I = 1; I <= transformLength; I++)
    	return;
    }
    
    private void SCRAMB(double F[], int transformLength) {
    	// Scrambles the data in the array F into odd-even order, i.e.
    	// with the odd-indexed elements in the first half of the array
    	// (in natural order), and the even elements in the second half
    	// (in reverse order).
    	
    	// Arguments:
    	// F Array containing the data to be rearranged into odd-even order.
    	// transformLength Length of array F.  transfomrLength must be a power of 2.
    	
    	// Internal variables:
    	int I; // Loop counter for swaps in top half of array.
    	int II1; // Index to first array element in swap.
    	int II2; // Index to second array element in swap.
    	int NV2; // Set equal to transformLength/2.
    	int NV4; // Set equal to transformLength/4.
    	double TEMP; // Temporary variable used while swapping array elements
    	double F2[];
    	
    	NV2 = transformLength/2;
    	NV4 = transformLength/4;
    	BITREV(F, transformLength);
    	BITREV(F, NV2);
    	F2 = new double[NV2];
    	for (I = 0; I < NV2; I++) {
    	    F2[I] = F[NV2+I];	
    	}
    	BITREV(F2, NV2);
    	for (I = 0; I < NV2; I++) {
    	    F[NV2+I] = F2[I];	
    	}
    	
    	II1 = transformLength - 1;
    	II2 = NV2;
    	for (I = 0; I < NV4; I++) {
    		TEMP = F[II1];
    		F[II1] = F[II2];
    		F[II2] = TEMP;
    		II1 = II1-1;
    		II2 = II2 + 1;
    	} // for (I = 0; I < NV4; I++)
    	return;
    }
    
    private void USCRAM(double F[], int transformLength) {
        // Unscrambles the data in array F from odd-even to natural order,
    	// i.e. performs the inverse of the operation performed by 
    	// subroutine SCRAMB.
    	
    	// Arguments:
    	// F  AZrray containing the data to be unscrambled.
    	// transformLength Length of the array F.  transformLength must be a power of 2.
    	
    	// Internal variables:
    	// As for subroutine SCRAMB.
    	int I; // Loop counter for swaps in top half of array.
    	int II1; // Index to first array element in swap.
    	int II2; // Index to second array element in swap.
    	int NV2; // Set equal to transformLength/2.
    	int NV4; // Set equal to transformLength/4.
    	double TEMP; // Temporary variable used while swapping array elements
    	double F2[];
    	
    	NV2 = transformLength/2;
    	NV4 = transformLength/4;
    	II1 = transformLength-1;
    	II2 = NV2;
    	for (I = 0; I < NV4; I++) {
    	    TEMP = F[II1];	
    	    F[II1] = F[II2];
    	    F[II2] = TEMP;
    	    II1 = II1-1;
    	    II2 = II2+1;
    	} // for (I = 0; I < NV4; I++)
    	BITREV(F,NV2);
    	F2 = new double[NV2];
    	for (I = 0; I < NV2; I++) {
    	    F2[I] = F[NV2+I];	
    	}
    	BITREV(F2, NV2);
    	for (I = 0; I < NV2; I++) {
    	    F[NV2+I] = F2[I];	
    	}
    	BITREV(F,transformLength);
    	return;
    }
    
    private void INIFCT(double C[], int transformLength, int IFAULT[]) {
        // Initializes the array C of cosine coefficients as required for
    	// calls to subroutines FCT and IFCT.  Also checks the validity of
    	// the transform size transformLength.  Called whenever subroutines
    	// FCT or IFCT receive a new transform size.  Never called explicitly
    	// by the user.
    	
    	// Arguments:
    	// C     Array to hold the cosine coefficients.
    	// transformLength Length of the array C, i.e. size of transform desired.
    	// IFAULT As described in subroutine FCT.
    	
    	// Internal variables:
    	int I; // Loop counter to index array elements while loading top 
    	       // half of C array, and while taking cosines of the array
    	       // values.
    	int IFAC; // Multiplication factor used while filling bottom half of
    	          // C array.
    	int IGROUP; // Loop counter for groups of C array coefficients in
    	            // bottom half.
    	int II; // Temporary variable to hold powers of two while the 
    	        // transform length is being checked.
    	int ITEM; // Loop counter to count items withina group of C array
    	          // coefficients.
    	int K; // Loop counter counting successive powers of two during
    	       // transform length checking.
    	int NITEMS; // Number of times within current group of C array coefficients.
    	int NV2; // Set to transformLength/2.
    	
    	// Check for valid transform size
    	Preferences.debug("INIFCT tranformLength = " + transformLength + "\n", Preferences.DEBUG_ALGORITHM);
    	IFAULT[0] = 0;
    	if (transformLength <= 1) {
    		IFAULT[0] = 1;
    		return;
    	}
    	II = 1;
    	L1:
    	{
    		for (K = 1; K <= MAXPOW; K++) {
    			II = II + II;
    			if (II == transformLength) {
    				break L1;
    			}
    			if (II > transformLength) {
    				IFAULT[0] = 3;
    				return;
    			}
    		} // for (K = 1; K <= MAXPOW; K++)
    		IFAULT[0] = 2;
    		return;
    	} // L1
    	
    	// If we reach this point, transform length is valid
    	IPOW = K;
    	LENGTH = transformLength;
    	NV2 = transformLength/2;
    	
    	// Put values into top half of C array
    	for (I = 0; I < NV2; I++) {
    		C[NV2+I] = (double)(4*I+1);
    	}
    	
    	// Copy scaled values from top half of C array to bottom half.
    	
    	NITEMS = 1;
    	IFAC = NV2;
    	for (IGROUP = 1; IGROUP <= IPOW - 1; IGROUP++) {
    		for (ITEM = 1; ITEM <= NITEMS; ITEM++) {
    	        C[NITEMS + ITEM - 1] = (double)IFAC * C[NV2 + ITEM - 1];
    		}
    		NITEMS = NITEMS + NITEMS;
    		IFAC = IFAC/2;
    	}
    	
    	// Take cosine of each element of C array
    	
    	for (I = 1; I < transformLength; I++) {
    		C[I] = 1.0/(2.0*Math.cos(C[I] * Math.PI/(double)(transformLength+transformLength)));
    	}
    	return;
    }
    
    private void SLOWFCT(double F[], int transformLength) {
    	int i;
    	int j;
    	double tmult = Math.PI/(2.0 * transformLength);
    	double onert = 1.0/Math.sqrt(transformLength);
    	double twort = Math.sqrt(2.0) * onert;
    	double sum[] = new double[transformLength];
    	for (j = 0; j < transformLength; j++) {
    		sum[0] += F[j];
    	}
    	for (i = 1; i < transformLength; i++) {
    		for (j = 0; j < transformLength; j++) {
    			sum[i] += F[j] * Math.cos((2*j + 1)*i*tmult);
    		}
    	}
    	F[0] = onert*sum[0];
    	for (i = 1; i < transformLength; i++) {
    		F[i] = twort * sum[i];
    	}
    }
    
    private void SLOW2DFCT(double F[], int xDim, int yDim) {
    	double xmult = Math.PI/(2.0 * xDim);
    	double ymult = Math.PI/(2.0 * yDim);
    	int x;
    	int y;
    	int u;
    	int v;
    	int uv = 0;
    	int xy;
    	double sum[] = new double[xDim * yDim];
    	double cy;
    	double scale = 2.0/Math.sqrt(xDim * yDim);
    	for (v = 0; v < yDim; v++) {
    		for (u = 0; u < xDim; u++) {
    			uv = u + v * xDim;
    			for (y = 0; y < yDim; y++) {
    				cy = Math.cos((2*y+1)*v*ymult);
    				for (x = 0; x < xDim; x++) {
    				    xy = x + y * xDim;
    				    sum[uv] += F[xy] * Math.cos((2*x+1)*u*xmult)*cy;
    				}
    			}
    			if (u == 0) {
    				sum[uv] *= RT2INV;
    			}
    			if (v == 0) {
    				sum[uv] *= RT2INV;
    			}
    			sum[uv] *= scale;
    		} // for (u = 0; u < xDim; u++)
    	} // for (v = 0; v < yDim; v++)
    	
    	for (v = 0; v < yDim; v++) {
    		for (u = 0; u < xDim; u++) {
    			uv = u + v * xDim;
    			F[uv] = sum[uv];
    		}
    	}
    }
    
    private void SLOWIFCT(double F[], int transformLength) {
    	int i;
    	int j;
    	double tmult = Math.PI/(2.0 * transformLength);
    	double onert = 1.0/Math.sqrt(transformLength);
    	double twort = Math.sqrt(2.0) * onert;
    	double sum[] = new double[transformLength];
    	for (i = 0; i < transformLength; i++) {
    		sum[i] = F[0]*RT2INV;
    	}
    	for (i = 0; i < transformLength; i++) {
    		for (j = 1; j < transformLength; j++) {
    			sum[i] += F[j] * Math.cos((2*i+1)*j*tmult);
    		}
    	}
    	
    	for (i = 0; i < transformLength; i++) {
    		F[i] = twort * sum[i];
    	}
    	
    }
    
    private void SLOW2DIFCT(double F[], int xDim, int yDim) {
    	double umult = Math.PI/(2.0 * xDim);
    	double vmult = Math.PI/(2.0 * yDim);
    	int x;
    	int y;
    	int u;
    	int v;
    	int uv;
    	int xy;
    	double sum[] = new double[xDim * yDim];
    	double cv;
    	double scale = 2.0/Math.sqrt(xDim * yDim);
    	double prod;
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			xy = x + y * xDim;
    			for (v = 0; v < yDim; v++) {
    				cv = Math.cos((2*y+1)*v*vmult);
    				for (u = 0; u < xDim; u++) {
    					uv = u + v * xDim;
    				    prod = F[uv]*Math.cos((2*x+1)*u*umult)*cv;	
    				    if ((u == 0) && (v == 0)) {
    				    	sum[xy] += 0.5 * prod;
    				    }
    				    else if ((u == 0) || (v == 0)) {
    				    	sum[xy] += RT2INV * prod;
    				    }
    				    else {
    				    	sum[xy] += prod;
    				    }
    				} // for (u = 0; u < xDim; u++) 
    			} // for (v = 0; v < yDim; v++)
    			sum[xy] *= scale;
    		} // for (x = 0; x < xDim; x++)
    	}
    	
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			xy = x + y * xDim;
    			F[xy] = sum[xy];
    		}
    	}
    }
    
    
    
    private void makeGaussianFilter(double buffer[], double rmsFreq) {
        double xexpDenom, yexpDenom;
        int x, y, pos;
        double coeff;
        double xnorm, ynorm;

        xnorm = (xDim-1)*(xDim-1);
        ynorm = (yDim-1)*(yDim-1);

        xexpDenom = 2.0 * rmsFreq * rmsFreq * xnorm;
        yexpDenom = 2.0 * rmsFreq * rmsFreq * ynorm;


            

        if (filterType == LOWPASS) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    coeff = (Math.exp(-x * x / xexpDenom) *
                                         Math.exp(-y * y / yexpDenom));
                    buffer[pos] *= coeff;
                }
            }
        } // end of if (filterType == LOWPASS)
        else if (filterType == HIGHPASS) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    coeff =  (1.0 -
                                     (Math.exp(-x * x / xexpDenom) *
                                          Math.exp(-y * y / yexpDenom)));
                    buffer[pos] *= coeff;
                }
            }
        } // end of if (filterType == HIGHPASS)
        
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  fr1  DOCUMENT ME!
     * @param  fr2  DOCUMENT ME!
     */
    private void makeButterworthFilter(double buffer[], double fr1, double fr2) {
        int x, y, pos;
        double distsq, width, centersq, coeff, num, xnorm, ynorm;
        xnorm = (xDim-1)*(xDim-1);
        ynorm = (yDim-1)*(yDim-1);


            if (filterType == LOWPASS) {
                for (y = 0; y <= (yDim - 1); y++) {
                    for (x = 0; x <= (xDim - 1); x++) {
                        pos = (y * xDim) + x;
                        distsq = (x * x / xnorm) + (y * y / ynorm);
                        coeff = (1.0 / (1.0 + Math.pow(distsq / (fr1 * fr1), filterOrder)));
                        buffer[pos] *= coeff;
                    }
                }
            } // end of if (filterType == LOWPASS)
            else if (filterType == HIGHPASS) {
                for (y = 0; y <= (yDim - 1); y++) {
                    for (x = 0; x <= (xDim - 1); x++) {
                        pos = (y * xDim) + x;
                        distsq = (x * x / xnorm) + (y * y / ynorm);
                        coeff = (1.0 / (1.0 + Math.pow((fr1 * fr1) / distsq, filterOrder)));
                        buffer[pos] *= coeff;
                    }
                }
            } // end of if (filterType == HIGHPASS)
            else if (filterType == BANDPASS) {
                width = fr2 - fr1;
                centersq = fr1 * fr2;
                for (y = 0; y <= (yDim - 1); y++) {
                    for (x = 0; x <= (xDim - 1); x++) {
                        pos =  (y * xDim) + x;
                        distsq = (x  * x / xnorm) + (y  * y / ynorm);
                        num = Math.pow(Math.sqrt(distsq) * width, 2.0 * filterOrder);
                        coeff =  (num / (num + Math.pow((distsq - centersq), 2.0 * filterOrder)));
                        buffer[pos] *= coeff;
                    }
                }
            } // end of if (filterType == BANDPASS)
            else if (filterType == BANDSTOP) {
                width = fr2 - fr1;
                centersq = fr1 * fr2;
                for (y = 0; y <= (yDim - 1); y++) {
                    for (x = 0; x <= (xDim - 1); x++) {
                        pos = (y * xDim) + x;
                        distsq = (x * x / xnorm) + (y * y / ynorm);
                        num = Math.pow((distsq - centersq), 2.0 * filterOrder);
                        coeff = (num / (num + Math.pow(Math.sqrt(distsq) * width, 2.0 * filterOrder)));
                        buffer[pos] *= coeff;
                    }
                }
            } // else if (filterType == BANDSTOP)
        
    }
    
    private double Chebyshev(int order, double w) {
    	double wSquared;
    	double wCubed;
    	double wFourth;
    	double wFifth;
    	double wSixth;
    	double wSeventh;
    	double wEighth;
    	double wNinth;
    	double wTenth;
    	double wEleventh;
    	double wTwelfth;
    	double wThirteenth;
    	switch (order) {
    	case 0:
    		return 1;
    	case 1:
    		return w;
    	case 2:
    		return 2.0*w*w - 1;
    	case 3:
    		return 4.0*w*w*w - 3.0*w;
    	case 4:
    		wSquared = w * w;
    		return 8.0*wSquared*wSquared - 8.0*wSquared + 1.0;
    	case 5:
    		wCubed = w * w * w;
    		return 16.0*wCubed*w*w - 20.0*wCubed + 5.0*w;
    	case 6:
    		wSquared = w * w;
    		wFourth = wSquared * wSquared;
    		return 32.0*wFourth*wSquared -48.0*wFourth + 18.0*wSquared - 1.0;
    	case 7:
    		wSquared = w*w;
    		wCubed = wSquared*w;
    		wFifth = wCubed*wSquared;
    		return 64.0*wFifth*wSquared - 112.0*wFifth + 56.0*wCubed - 7.0*w;
    	case 8:
    		wSquared = w*w;
    		wFourth = wSquared*wSquared;
    		wSixth = wFourth*wSquared;
    		return 128.0*wFourth*wFourth - 256.0*wSixth + 160.0*wFourth -32.0*wSquared + 1.0;
    	case 9:
    		wSquared = w*w;
    		wCubed = wSquared*w;
    		wFifth = wCubed*wSquared;
    		wSeventh = wFifth*wSquared;
    		return 256.0*wSeventh*wSquared - 576.0*wSeventh + 432.0*wFifth - 120.0*wCubed + 9.0*w;
    	case 10:
    		wSquared = w*w;
    		wFourth = wSquared*wSquared;
    		wSixth = wFourth*wSquared;
    		wEighth = wFourth*wFourth;
    		return 512.0*wEighth*wSquared - 1280.0*wEighth + 1120.0*wSixth - 400.0*wFourth + 50.0*wSquared - 1.0;
    	case 11:
    		wSquared = w*w;
    		wCubed = wSquared*w;
    		wFifth = wCubed*wSquared;
    		wSeventh = wFifth*wSquared;
    		wNinth = wSeventh*wSquared;
    		return 1024.0*wNinth*wSquared - 2816.0*wNinth + 2816.0*wSeventh - 1232.0*wFifth + 220.0*wCubed - 11.0*w;
    	case 12:
    		wSquared = w*w;
    		wFourth = wSquared*wSquared;
    		wSixth = wFourth*wSquared;
    		wEighth = wFourth*wFourth;
    		wTenth = wEighth*wSquared;
    		return 2048.0*wSixth*wSixth - 6144.0*wTenth + 6912.0*wEighth - 3584.0*wSixth + 840.0*wFourth - 72.0*wSquared + 1.0;
    	case 13:
    		wSquared = w*w;
    		wCubed = wSquared*w;
    		wFifth = wCubed*wSquared;
    		wSeventh = wFifth*wSquared;
    		wNinth = wSeventh*wSquared;
    		wEleventh = wNinth*wSquared;
    		return 4096.0*wEleventh*wSquared - 13312.0*wEleventh + 16640.0*wNinth - 9984.0*wSeventh + 2912.0*wFifth
    				- 364.0*wCubed + 13.0;
    	case 14:
    		wSquared = w*w;
    		wFourth = wSquared*wSquared;
    		wSixth = wFourth*wSquared;
    		wEighth = wFourth*wFourth;
    		wTenth = wEighth*wSquared;
    		wTwelfth = wSixth * wSixth;
    		return 8192.0*wTwelfth*wSquared - 28672.0*wTwelfth + 39424.0*wTenth - 26880.0*wEighth + 9408.0*wSixth
    				- 1568.0*wFourth + 98.0*wSquared - 1.0;
    	case 15:
    		wSquared = w*w;
    		wCubed = wSquared*w;
    		wFifth = wCubed*wSquared;
    		wSeventh = wFifth*wSquared;
    		wNinth = wSeventh*wSquared;
    		wEleventh = wNinth*wSquared;
    		wThirteenth = wEleventh*wSquared;
    		return 16384.0*wThirteenth*wSquared - 61440.0*wThirteenth + 92160.0*wEleventh - 70400.0*wNinth + 28800.0*wSeventh
    				- 6048.0*wFifth + 560.0*wCubed - 15.0*w;
    	default:
    		MipavUtil.displayError("No Chebyshev polynomial returned");
    		return Double.NaN;
    	}
    	 
    }
    
    private void makeChebyshevTypeIFilter(double buffer[], double fr1, double fr2) {
    	// Lowpass filter has ripples in the passband but no ripples in the stopband.
    	int x, y, pos;
        double distsq, coeff, xnorm, ynorm;
        
        double epsilonSquared = epsilon*epsilon;
        double ratio;
        double Tn;
        
     
        xnorm = (xDim-1)*(xDim-1);
        ynorm = (yDim-1)*(yDim-1);

        if (filterType == LOWPASS) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    distsq = (x * x / xnorm) + (y * y / ynorm);
                    ratio = Math.sqrt(distsq)/fr1;
                    Tn = Chebyshev(filterOrder, ratio);
                    coeff = (1.0 / (1.0 + epsilonSquared*Tn*Tn));
                    buffer[pos] *= coeff;
                }
            }
        } // end of if (filterType == LOWPASS)
        else if (filterType == HIGHPASS) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    distsq = (x * x / xnorm) + (y * y / ynorm);
                    ratio = fr1/Math.sqrt(distsq);
                    Tn = Chebyshev(filterOrder, ratio);
                    coeff = (float) (1.0 / (1.0 + epsilonSquared*Tn*Tn));
                    buffer[pos] *= coeff;
                }
            }
        } // else if (filterType == HIGHPASS)	
        else if (filterType == BANDPASS) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    distsq = (x * x / xnorm) + (y * y / ynorm);
                    ratio = Math.abs(fr1*fr2 - distsq)/((fr2 - fr1)*Math.sqrt(distsq));
                    Tn = Chebyshev(filterOrder, ratio);
                    coeff = (float) (1.0 / (1.0 + epsilonSquared*Tn*Tn));
                    buffer[pos] *= coeff;
                }
            }
        } // else if (filterType == BANDPASS)
        else if (filterType == BANDSTOP) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    distsq = (x * x / xnorm) + (y * y / ynorm);
                    ratio = ((fr2 - fr1)*Math.sqrt(distsq))/Math.abs(fr1*fr2 - distsq);
                    Tn = Chebyshev(filterOrder, ratio);
                    coeff = (float) (1.0 / (1.0 + epsilonSquared*Tn*Tn));
                    buffer[pos] *= coeff;
                }
            }
        } // else if (filterType == BANDSTOP)
        
    }
    
    private void makeChebyshevTypeIIFilter(double buffer[], double fr1, double fr2) {
    	// Lowpass filter has no ripples in the passband but has ripples in the stopband
    	// fr1 end of pass band only works for 2.0 * PI * fr1 > 1.0
    	// fr2 start of stop band
    	// fr2 > fr1
    	int x, y, pos;
        double distsq, coeff, xnorm, ynorm;
        
        double ratio;
        double Tn;
        double Tn2;
        double product;
        double TnSquared;
        
        Tn2 = Chebyshev(filterOrder, 2.0 * Math.PI * fr1); // Only works if 2.0 * PI * fr1 > 1.0
        product = epsilon * epsilon * Tn2 * Tn2;
        
        xnorm = (xDim-1)*(xDim-1);
        ynorm = (yDim-1)*(yDim-1);

        if (filterType == LOWPASS) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    distsq = (x * x / xnorm) + (y * y / ynorm);
                    if (distsq != 0.0) {
                        ratio = fr1/Math.sqrt(distsq);
                    }
                    else {
                    	distsq = (0.1 * 0.1 /xnorm) + (0.1 * 0.1/ynorm);
                    	ratio = fr1/Math.sqrt(distsq);
                    }
                    Tn = Chebyshev(filterOrder, ratio);
                    TnSquared = Tn*Tn;
                    coeff = (TnSquared / (TnSquared + product));
                    buffer[pos] *= coeff;
                }
            }
        } // end of if (filterType == LOWPASS)
        else if (filterType == HIGHPASS) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    distsq = (x * x / xnorm) + (y * y / ynorm);
                    ratio = Math.sqrt(distsq)/fr1;
                    Tn = Chebyshev(filterOrder, ratio);
                    TnSquared = Tn*Tn;
                    coeff = (TnSquared / (TnSquared + product));
                    buffer[pos] *= coeff;
                }
            }
        } // else if (filterType == HIGHPASS)
        else if (filterType == BANDPASS) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    distsq = (x * x / xnorm) + (y * y / ynorm);
                    ratio = ((fr2 - fr1)*Math.sqrt(distsq))/Math.abs(fr1*fr2 - distsq);
                    Tn = Chebyshev(filterOrder, ratio);
                    TnSquared = Tn*Tn;
                    coeff = (TnSquared / (TnSquared + product));
                    buffer[pos] *= coeff;
                }
            }
        } // else if (filterType == BANDPASS)
        else if (filterType == BANDSTOP) {
            for (y = 0; y <= (yDim - 1); y++) {
                for (x = 0; x <= (xDim - 1); x++) {
                    pos = (y * xDim) + x;
                    distsq = (x * x / xnorm) + (y * y / ynorm);
                    if (distsq != 0) {
                        ratio = Math.abs(fr1*fr2 - distsq)/((fr2 - fr1)*Math.sqrt(distsq));
                    }
                    else {
                    	distsq = (0.1 * 0.1 / xnorm) + (0.1 * 0.1 / ynorm);	
                    	ratio = Math.abs(fr1*fr2 - distsq)/((fr2 - fr1)*Math.sqrt(distsq));
                    }
                    Tn = Chebyshev(filterOrder, ratio);
                    TnSquared = Tn*Tn;
                    coeff = (TnSquared / (TnSquared + product));
                    buffer[pos] *= coeff;
                }
            }
        } // else if (filterType == BANDSTOP)
        
    }

}