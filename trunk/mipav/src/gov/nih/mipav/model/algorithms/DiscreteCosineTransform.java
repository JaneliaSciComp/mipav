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
	
	private final int FILTER_NONE = 0;
	private final int FILTER_SOFT = 1;
	private final int FILTER_NN_GARROTE = 2;
	private final int FILTER_HARD = 3;
	private final int FILTER_GREATER = 4;
	private final int FILTER_LESS = 5;
	private final int FILTER_THRESHOLD_FIRM = 6;
	private int filterType;
	private double filterVal1;
	private double filterVal2;

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
    
    public DiscreteCosineTransform(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg, int filterType,
    		double filterVal1, double filterVal2) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
		this.filterType = filterType;
		this.filterVal1 = filterVal1;
		this.filterVal2 = filterVal2;
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
        this.filterType = filterType;
		this.filterVal1 = filterVal1;
		this.filterVal2 = filterVal2;
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
        	if (filterType != FILTER_NONE) {
        		filter(doubleBuffer, filterType,filterVal1,filterVal2);	
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
    
    private void filter(double data[], int filterType, double filterVal1, double filterVal2) {
    	if (filterType == FILTER_SOFT) {
    		soft(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_NN_GARROTE) {
    		nn_garrote(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_HARD) {
    		hard(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_GREATER) {
    		greater(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_LESS) {
    		less(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_THRESHOLD_FIRM) {
    		threshold_firm(data, filterVal1, filterVal2);
    	}
    	return;
    }
    
    public double[] soft(double data[], double value, double substitute) {
    	// Default substitute = 0
    	int i;
    	double magnitude[] = new double[data.length];
    	double thresholded[] = new double[data.length];
    	for (i = 0; i < data.length; i++) {
    		if (data[i] == 0.0) {
    			thresholded[i] = 0.0;
    		}
    		else {
    		    magnitude[i] = Math.abs(data[i]);
    		    thresholded[i] = (1.0 - value/magnitude[i]);
    		    if (thresholded[i] < 0.0) {
    		    	thresholded[i] = 0.0;
    		    }
    		    thresholded[i] = data[i] * thresholded[i];
    		}
    	}
    	
    	if (substitute == 0) {
    		return thresholded;
    	}
    	else {
    		for (i = 0; i < thresholded.length; i++) {
    			if (magnitude[i] < value) {
    				thresholded[i] = substitute;
    			}
    		}
    		return thresholded;
    	}
    }
    
    public double[] nn_garrote(double data[], double value, double substitute) {
        // Non-negative Garrote
    	// Default substitute = 0
    	int i;
    	double magnitude[] = new double[data.length];
    	double valueSquared = value * value;
    	double thresholded[] = new double[data.length];
    	for (i = 0; i < data.length; i++) {
    		if (data[i] == 0.0) {
    			thresholded[i] = 0.0;
    		}
    		else {
    		    magnitude[i] = Math.abs(data[i]);
    		    thresholded[i] = (1.0 - valueSquared/(magnitude[i] * magnitude[i]));
    		    if (thresholded[i] < 0.0) {
    		    	thresholded[i] = 0.0;
    		    }
    		    thresholded[i] = data[i] * thresholded[i];
    		}
    	}
    	
    	if (substitute == 0) {
    		return thresholded;
    	}
    	else {
    		for (i = 0; i < thresholded.length; i++) {
    			if (magnitude[i] < value) {
    				thresholded[i] = substitute;
    			}
    		}
    		return thresholded;
    	}
    }
    
    public double[] hard(double data[], double value, double substitute) {
    	// default substitute = 0.0
    	double data2[] = data.clone();
    	int i;
        for (i = 0; i < data2.length; i++) {
            if (Math.abs(data2[i]) < value) {
            	data2[i] = substitute;
            }
        }
        return data2;
    }
    
    public double[][] hard(double data[][], double value, double substitute) {
    	// default substitute = 0.0
    	double data2[][] = data.clone();
    	int i,j;
        for (i = 0; i < data2.length; i++) {
        	for (j = 0; j < data2[i].length; j++) {
	            if (Math.abs(data2[i][j]) < value) {
	            	data2[i][j] = substitute;
	            }
        	}
        }
        return data2;
    }
    
    public double[] greater(double data[], double value, double substitute) {
    	double data2[] = data.clone();
        // default substitute = 0.0
        // greater thresholding only supports real data
    	int i;
    	for (i = 0; i  < data2.length; i++) {
    		if (data2[i] < value) {
    			data2[i] = substitute;
    		}
    	}
        return data2;
    }
    
    public double[] less(double data[], double value, double substitute) {
    	// default substitute = 0.0
        // less thresholding only supports real data
    	double data2[] = data.clone();
    	int i;
    	for (i = 0; i < data2.length; i++) {
    		if (data2[i] > value) {
    			data2[i] = substitute;
    		}
    	}
    	return data2;
    }
    
    public double[] threshold_firm(double data[], double value_low, double value_high) {
        // Firm threshold.

        // The approach is intermediate between soft and hard thresholding [1]_. It
        // behaves the same as soft-thresholding for values below `value_low` and
        // the same as hard-thresholding for values above `thresh_high`.  For
        // intermediate values, the thresholded value is in between that corresponding
        // to soft or hard thresholding.

        // Parameters
        // ----------
        // data : array-like
        //    The data to threshold.  This can be either real or complex-valued.
        // value_low : float
        //    Any values smaller then `value_low` will be set to zero.
        // value_high : float
        //    Any values larger than `value_high` will not be modified.

        // Notes
        // -----
        // This thresholding technique is also known as semi-soft thresholding [2]_.

        // For each value, `x`, in `data`. This function computes::

        //    if np.abs(x) <= value_low:
        //        return 0
        //    elif np.abs(x) > value_high:
        //        return x
        //    elif value_low < np.abs(x) and np.abs(x) <= value_high:
        //        return x * value_high * (1 - value_low/x)/(value_high - value_low)

        // ``firm`` is a continuous function (like soft thresholding), but is
        // unbiased for large values (like hard thresholding).

        // If ``value_high == value_low`` this function becomes hard-thresholding.
        // If ``value_high`` is infinity, this function becomes soft-thresholding.

        // Returns
        // -------
        // val_new : array-like
        //    The values after firm thresholding at the specified thresholds.

        // See Also
        // --------
        // threshold

        // References
        // ----------
        // .. [1] H.-Y. Gao and A.G. Bruce. Waveshrink with firm shrinkage.
        //    Statistica Sinica, Vol. 7, pp. 855-874, 1997.
        // .. [2] A. Bruce and H-Y. Gao. WaveShrink: Shrinkage Functions and
        //    Thresholds. Proc. SPIE 2569, Wavelet Applications in Signal and
        //    Image Processing III, 1995.
        //    DOI:10.1117/12.217582
        int i;

        if (value_low < 0) {
            MipavUtil.displayError("value_low must be non-negative.");
            return null;
        }

        if (value_high < value_low) {
            MipavUtil.displayError("value_high must be greater than or equal to value_low.");
            return null;
        }

        
        double magnitude[] = new double[data.length];
        double thresholded[] = new double[data.length];
        double vdiff = value_high - value_low;
        for (i = 0; i < data.length; i++) {
        	if (data[i] == 0.0) {
        		thresholded[i] = 0.0;
        	}
        	else {
        	    magnitude[i] = Math.abs(data[i]);
        	    thresholded[i] = value_high * (1 - value_low/magnitude[i]) / vdiff;
        	    if (thresholded[i] < 0.0) {
        	    	thresholded[i] = 0.0;
        	    }
        	    thresholded[i] = data[i] * thresholded[i];
        	}
        }

        // restore hard-thresholding behavior for values > value_high
        for (i = 0; i < magnitude.length; i++) {
        	if (magnitude[i] > value_high) {
        		thresholded[i] = data[i];
        	}
        }
        return thresholded;
    }

}