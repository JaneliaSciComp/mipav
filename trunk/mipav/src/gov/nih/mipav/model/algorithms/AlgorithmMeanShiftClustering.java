package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Random;
import java.util.Vector;

import Jama.Matrix;
import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/**
 * The java code is ported from C++ code downloaded from http://coewww.rutgers.edu/riul/research/code.html.
 * The relevant web page section says:
 * Adaptive mean shift based clustering
   C++ code implementing an (approximate) mean shift procedure with variable bandwith (in high dimensions). 
   The algorithm is described in Mean shift based clustering in high dimensions: A texture classification example.
   For comments, please contact Bogdan Georgescu or Ilan Shimshoni.
 * 
 * The original paper was written by Bogdan Georgescu, Ilan Shimshoni, and P. Meer in the proceedings of ICCV 2003.
 * The original files were fams.cpp and fams.h.
 * 
 * This code was ported by William Gandler.
 */

public class AlgorithmMeanShiftClustering extends AlgorithmBase {
	public static final int EVERY_POINT = 1;
	public static final int SELECT_ON_JUMP = 2;
	public static final int SELECT_PERCENT = 3;
	// Find KL
	// Number of points on which test is run
	private static final int FAMS_FKL_NEL = 500;
	// Number of times on which same test is run
	private static final int FAMS_FKL_TIMES = 10;
	
	// FAMS main algorithm
	// Maximum valid K
	private static final int FAMS_MAX_K = 70;
	// Maximum valid L
	private static final int FAMS_MAX_L = 500;
	// first hash table block size
	private static final int FAMS_BLOCKSIZE = 4096;
	// second hash table block size
	private static final int FAMS_BLOCKSIZE2 = 256;
	// maximum MS iterations
	private static final int FAMS_MAXITER = 100;
	// weight power
	private static final double FAMS_ALPHA = 1.0;
	
	//private static final int Bs = FAMS_BLOCKSIZE/sizeof(fams_hash_entry);
	private static final int Bs = FAMS_BLOCKSIZE/14;
	//private static final int Bs2 = FAMS_BLOCKSIZE2/sizeof(fams_hash_entry2);
	private static final int Bs2 = FAMS_BLOCKSIZE2/12;
	
	private static float FAMS_FLOAT_SHIFT = 100000.0f;
	// K, L are LSH parameters.  If either K or L is not greater than zero, the LSH data structure is not built
	// and the linear algorithm is run.
	private int K;
	private int L;
	// k_neigh is the number of neighbors used in the consruction of pilot density
	private int k_neigh;
	// input_directory and data_file_name provide the location of the input data file.
	private String data_file_name;
	private String input_directory;
	// jump and percent are two ways to choose from which points to start the mean shift procedure.
	// The default is to perform the procedure on every point.
	// jump means once every jump a point is selected.  If jump is 5 then the points 1,6,11,16,...
	// are chosen.  percent means that (percent*n)/100 (n the number of points) are chosen at
	// random with replacement.  Obviously, each run of the program will yield a different set.
	// Only 1 of EVERY_POINT, SELECT_ON_JUMP, or SELECT_PERCENT can be chosen.
	private int choosePoints;
	// jump must be at least 1
	private int jump = 1;
	// Need 0.0 <= percent <= 1.0
	private double percent = 0.0;
	// if fixedWidth is true, the user runs the fixed bandwidth mean shift procedure.
	// The width * d (d is the dimension of the data) is the distance under the L1 norm
	// used as the fixed bandwidth.
	private boolean fixedWidth;
	private float width = -1;
	// If findOptimalKL is true, the program automatically computes the optimal K and L.
	// The optimal K is searched between Kmin and K (the first parameter) with step Kjump.
	// The optimal L is searched between 1 and L (the second parameter)
	// epsilon represents the allowed error (in experiments epsilon = 0.05)
	private boolean findOptimalKL;
	private float epsilon;
	private int Kmin;
	private int Kjump;
	
	private boolean noLSH;
	
	private RandomAccessFile raFile;
	/** byte array for int * */
    private final byte[] byteIntBuffer = new byte[4];
    /** byte array for float * */
    private final byte[] byteFloatBuffer = new byte[4];
    private boolean endianess;
	
	// interval of input data
	private float minVal;
	private float maxVal;
	
	// input points
	private famsPoint points[];
	private int data[][];
	private int nPoints;
	private int nDims;
	private int dataSize;
	// temp work
	double rr[];
	
	// selected points on which mean shift is run
	private int psel[];
	private int nsel;
	private int modes[];
	private int hmodes[];
	private int npm;
	
	// hash table data
	private int M;
	private int M2;
	private int hashCoeffs[];
	
	// alg_params
	private int K_;
	private int L_;
	
	// temporary
	private boolean t_cut_res[][] = new boolean[FAMS_MAX_L][FAMS_MAX_K];
	private boolean t_old_cut_res[][] = new boolean[FAMS_MAX_L][FAMS_MAX_K];
	private int t_m[] = new int[FAMS_MAX_L];
	private int t_m2[] = new int[FAMS_MAX_L];
	private int nnres1;
	private int nnres2;
	
	private long tt1;
	private Random srand;
	
	/**
	 * File formats:
		Files are all ASCII files.
		The input file has a one line header with two
		number_of_points dimension
		Then the input points are given as lines including d numbers.

		The pilot process produces the neighborhood size for each point
		If the input file is for example d040_1S.txt and the number of neighbors is k
		the pilot file will be pilot_d040_1S_k.txt
		If the file exists the program will assume that it was created by a
		previous run and use the information 
		in it and will not rerun the pilot creation procedure.
		This enables the users to provide their own neighborhood sizes.

		The output files are for example:
		The result of MS on the selected points is in out_data_file_name.txt
		The result of joined modes is in modes_data_file_name.txt

		Example:
		We are given a file d040_1S.txt
		fams 30 46 200 d040_1S ./ -f 0.05 10 2
		Finds K,L and optionally runs mean shift on all points.
		To run mean shift on same data with K=24 and L=35
		fams 24 35 200 d040_1S ./

		NOTES:

		- Internally the program scales the data between 0 and 2^16-1, therefore
		  the precision is limited by range_of_data/2^16 

		- Running mean shift on all the points is not necessary when only the modes
		  are needed

		- In the file fams.h there are several constants that influence the speed and
		  accuracy of the program. For example you can set the bandwidth on which two 
		  modes are joined or the number of trials on which the test is run when 
		  finding K and L.
    */
	
	public AlgorithmMeanShiftClustering(int K, int L, int k_neigh, String data_file_name, String input_directory,
			int choosePoints, int jump, double percent, boolean fixedWidth, float width, boolean findOptimalKL, float epsilon, int Kmin, int Kjump) {
	    this.K = K;
	    this.L = L;
	    this.k_neigh = k_neigh;
	    this.data_file_name = data_file_name;
	    this.input_directory = input_directory;
	    this.choosePoints = choosePoints;
	    this.percent = percent;
	    this.jump = jump;
	    this.fixedWidth = fixedWidth;
	    this.width = width;
	    this.findOptimalKL = findOptimalKL;
	    this.epsilon = epsilon;
	    this.Kmin = Kmin;
	    this.Kjump = Kjump;
	}
	
	public void runAlgorithm() {
	    String fdata_file_name;
	    String pilot_file_name;
	    int Lmax = 0;
	    int Kmax = 0;
	    String firstLine;
	    String values[];
	    int numValues;
	    int presentValues;
	    float pttemp[];
	    String dataLine;
	    int i;
	    int j;
	    int k;
	    float deltaVal;
	    float findEpsilon;
	    boolean adaptive;
	    int hWidth;
	    //float scores[];
	    float scores[][];
	    int Lcrt;
	    int Kcrt;
	    int nBest;
	    int LBest[];
	    int KBest[];
	    int ntimes;
	    int is;
	    long run_times[];
	    int iBest;
	    long timeBest;
	    fams_hash_entry HT[][];
	    int hs[];
	    fams_hash_entry2 HT2[][];
	    int hs2[];
	    fams_cut cuts[][];
	    boolean cut_res[];
	    int hjump[];
	    int m;
	    int m2;
	    int hwd;
	    
	    noLSH = (K <= 0) || (L <= 0);
	    // input_directory should end with File.separator
	    fdata_file_name = input_directory + data_file_name;
	    if (choosePoints == SELECT_ON_JUMP) {
	    	if (jump < 1) {
	    		jump = 1;
	    	}
	    }
	    else if (choosePoints == SELECT_PERCENT) {
	    	if ((percent < 0) || (percent > 1)) {
	    		percent = 0;
	    	}
	    }
	    
	    if (findOptimalKL) {
	    	Lmax = L;
	    	Kmax = K;
	    }
	    
	    // load points
	    int nsel = 0;
	    int npm = 0;
	    hashCoeffs = null;
	    
	    tt1 = System.currentTimeMillis();
	    srand = new Random(tt1);
	    
	    nnres1 = 0;
	    nnres2 = 0;
	    try {
	    	raFile = new RandomAccessFile(fdata_file_name, "r");
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on new RandomAccessFile(fdata_file_name, \"r\")"); 
	    	setCompleted(false);
	    	return;
	    }
	    
	    do {
		    try {
		    	firstLine = raFile.readLine();
		    }
		    catch (IOException e) {
		    	MipavUtil.displayError("IOException " + e + " firstLine = raFile.readLine)"); 
		    	setCompleted(false);
		    	return;
		    }
	    } while ((firstLine == null) || (firstLine.isEmpty()));
	    values = retrieveValues(firstLine);
	    if (values == null) {
	    	MipavUtil.displayError("No values found in first line");
	    	setCompleted(false);
	    	return;
	    }
	    if (values.length != 2) {
	    	MipavUtil.displayError("First line has " + values.length + " values instead of the expected 2");
	    	setCompleted(false);
	    	return;
	    }
	    try {
	        nPoints = Integer.parseInt(values[0]);
	    }
	    catch (NumberFormatException e) {
	    	MipavUtil.displayError("NumberFormatException " + e + " on nPoints = Integer.parseInt(values[0])");
	    	setCompleted(false);
	    	return;
	    }
	    if (nPoints < 1) {
	    	MipavUtil.displayError("nPoints = " + nPoints);
	    	setCompleted(false);
	    	return;
	    }
	    try {
	    	nDims= Integer.parseInt(values[1]);
	    }
	    catch (NumberFormatException e) {
	    	MipavUtil.displayError("NumberFormatException " + e + " on nDims = Integer.parseInt(values[1])");
	    	setCompleted(false);
	    	return;
	    }
	    if (nDims < 1) {
	    	MipavUtil.displayError("nDims = " + nDims);
	    	setCompleted(false);
	    	return;
	    }
	    numValues = nPoints * nDims;
	    presentValues = 0;
	    // Allocate data
	    pttemp = new float[numValues];
	    while (presentValues < numValues) {
	    	try {
		    	dataLine = raFile.readLine();
		    }
		    catch (IOException e) {
		    	MipavUtil.displayError("IOException " + e + " dataLine = raFile.readLine)"); 
		    	setCompleted(false);
		    	return;
		    }
	    	 values = retrieveValues(dataLine);
	    	 if (values != null) {
	    		 for (i = 0; i < values.length && presentValues < numValues; i++) {
	    			 try {
	    				 pttemp[presentValues++] = Float.parseFloat(values[i]);
	    			 }
	    			 catch (NumberFormatException e) {
	    				 MipavUtil.displayError("NumberFormatException " + e + " pttemp["+presentValues+"++] = "+
	    			     "Float.parseFloat(values["+i+"])");
	    			     setCompleted(false);
	    			    return;	 
	    			 }
	    		 } // for (i = 0; i < values.length; i++)
	    	 } // if (values != null)
	    } // while (presentValues < numValues)
	    try {
	    	raFile.close();
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on raFile.close()");
	    	setCompleted(false);
	    	return;
	    }
	    
	    // Allocate and convert to integer
	    for (i = 0, minVal = pttemp[0], maxVal = pttemp[0]; i < numValues; i++) {
	    	if (minVal > pttemp[i]) {
	    		minVal = pttemp[i];
	    	}
	    	else if (maxVal < pttemp[i]) {
	    		maxVal = pttemp[i];
	    	}
	    } // for (i = 0, minVal = pttemp[0], maxVal = pttemp[0]; i < numValues; i++)
	    data = new int[nPoints][nDims];
	    rr = new double[nDims];
	    deltaVal = maxVal - minVal;
	    if (deltaVal == 0) {
	    	deltaVal = 1.0f;
	    }
	    for (i = 0, j = 0, k = 0; i < numValues; i++) {
	    	data[j][k] = (int)(65535.0 * (pttemp[i] - minVal)/deltaVal);
	    	k++;
	    	if (k == nDims) {
	    		k = 0;
	    		j++;
	    	}
	    }
	    pttemp = null;
	    dataSize = nDims;
	    
	    points = new famsPoint[nPoints];
	    for (i = 0; i < nPoints; i++) {
	    	points[i] = new famsPoint();
	    	points[i].setData(data[i]);
	    	points[i].setUsedFlag(0);
	    }
	    
	    adaptive = !fixedWidth;
        
        if (fixedWidth) {
        	 hWidth = (int)(65535.0*width/(maxVal - minVal));
        }
        else {
        	hWidth = 0;
        }
	    
	    if (findOptimalKL) {
	        findEpsilon = epsilon + 1;
	        
	        // select points on which test is run
	        selectMSPoints(FAMS_FKL_NEL*100.0/nPoints, 0);
	        
	        // Compute bandwidths for selected points
	        computeRealBandwidths(hWidth);
	        
	        // Start finding the correct L for each K
	        //scores = new float[FAMS_FKL_TIMES*FAMS_MAX_L];
	        LBest = new int[FAMS_MAX_K];
	        KBest = new int[FAMS_MAX_K];
	        
	        Lcrt = Lmax; 
	        scores = new float[FAMS_FKL_TIMES][Lcrt];
	        Preferences.debug("About to find valid pairs\n", Preferences.DEBUG_ALGORITHM);
	        for (Kcrt = Kmax, nBest = 0; Kcrt >= Kmin; Kcrt -= Kjump, nBest++) {
	            // Do iterations for crt K and L = 1...Lcrt
	        	for (ntimes = 0; ntimes < FAMS_FKL_TIMES; ntimes++) {
	        	    doFindKLIteration(Kcrt, Lcrt, scores[ntimes]);	
	        	} // for (ntimes = 0; ntimes < FAMS_FKL_TIMES; ntimes++)
	        	
	        	// Get correct for this K
	        	KBest[nBest] = Kcrt;
	        	LBest[nBest] = -1;
	        	for (is = 0; (LBest[nBest] == -1) && (is < Lcrt); is++) {
	        	    // Find max on this column
	        		for (ntimes = 1; ntimes < FAMS_FKL_TIMES; ntimes++) {
	        		    if (scores[0][is] < scores[ntimes][is]) {
	        		    	scores[0][is] = scores[ntimes][is];
	        		    }
	        		} // for (ntimes = 1; ntimes < FAMS_FKL_TIMES; ntimes++)
	        		if (scores[0][is] < findEpsilon) {
	        			LBest[nBest] = is+1;
	        		}
	        	} // for (is = 0; (LBest[nBest] == -1) && (is < Lcrt); is++)
	        	
	        	// Update Lcrt to reduce running time
	        	if (LBest[nBest] > 0) {
	        		Lcrt = LBest[nBest] + 2;
	        	}
	        } // for (Kcrt = Kmax, nBest = 0; Kcrt >= Kmin; Kcrt -= Kjump, nBest++)
	        
	        // Start finding the pair with the best running time
	        run_times = new long[FAMS_FKL_TIMES];
	        timeBest = -1;
	        iBest = -1;
	        Preferences.debug("About to select the best pair\n", Preferences.DEBUG_ALGORITHM);
	        for (i = 0; i < nBest; i++) {
	        	if (LBest[i] <= 0) {
	        		continue;
	        	}
	        	for (ntimes = 0; ntimes < FAMS_FKL_TIMES; ntimes++) {
	        	    run_times[ntimes] = doFindKLIteration(KBest[i], LBest[i], scores[ntimes]);	
	        	} // for (ntimes = 0; ntimes < FAMS_FKL_TIMES; ntimes++)
	        	bgSort(run_times, FAMS_FKL_TIMES);
	        	if ((timeBest == -1) || (timeBest > run_times[FAMS_FKL_TIMES/2])) {
	        		iBest = i;
	        		timeBest = run_times[FAMS_FKL_TIMES/2];
	        	}
	        } // for (i = 0; i < nBest; i++)
	        K = KBest[iBest];
	        L = LBest[iBest];
	        Preferences.debug("KBest = " + KBest[iBest] + " LBest = " + LBest[iBest] + "\n");
	        Preferences.debug("time = " + run_times[FAMS_FKL_TIMES] + "\n");
	    } // if (findOptimalKL)
	    pilot_file_name = input_directory+"pilot_"+String.valueOf(k_neigh)+"_"+data_file_name;
	    
	    K_ = K;
	    L_ = L;
	    selectMSPoints(percent, jump);
	    
	    // Allocate memory for the hash table
	    M = getPrime(3*nPoints*L_/Bs);
	    M2 = getPrime(nsel*20*3/Bs2);
	    HT = new fams_hash_entry[M][Bs];
		for (i = 0; i < M; i++) {
			for (j = 0; j < Bs; j++) {
				HT[i][j] = new fams_hash_entry();
			}
		}
		hs = new int[M];
		HT2 = new fams_hash_entry2[M2][Bs2];
		for (i = 0; i < M2; i++) {
			for (j = 0; j < Bs2; j++) {
				HT2[i][j] = new fams_hash_entry2();
			}
		}
		hs2 = new int[M2];
		
		// Build partitions
		cuts = new fams_cut[L][FAMS_MAX_K];
		for (i = 0; i < L; i++) {
			for (j = 0; j < FAMS_MAX_K; j++) {
				cuts[i][j] = new fams_cut();
			}
		}
		cut_res = new boolean[FAMS_MAX_K];
		makeCuts(cuts);
		
		initHash(K_+L_);
		
		// Insert data into partitions
		hjump = new int[1];
		for (j = 0; j < nPoints; j++) {
		    for (i = 0; i < L_; i++) {
		        evalCutRes(points[j], cuts[i], cut_res);
		        m = hashFunction(cut_res, 0, i, K_, M, hjump);
		        m2 = hashFunction(cut_res,1,i,K_-1, 0, null);
		        addDataToHash(HT, hs, points[j], m, Bs, M, i, m2, hjump[0]);
		    } // for (i = 0; i < L_; i++)
		} // for (j = 0; j < nPoints; j++)
		
		// Compute pilot if necessary
		if (adaptive) {
			computePilot(HT, hs, cuts, pilot_file_name);
		}
		else {
			// fixed bandwidth
		    hwd = (int)(hWidth * nDims);
		    for (i = 0; i < nPoints; i++) {
		    	points[i].window = hwd;
		    	points[i].weightdp2 = 1;
		    }
		}
		
		doFAMS(HT, hs, cuts, HT2, hs2);
	}
	
	// perform FAMS starting from a subset of the data points.
	private void doFAMS(fams_hash_entry HT[][],int hs[], fams_cut cuts[][],
	     fams_hash_entry2 HT2[][], int hs2[])
	{
	   int i;
	   int j;
	   int k;
	   int jj;
	   int oldMean[];
	   int crtMean[];
	   famsPoint currentpt;
	   int sol[];
	   int who;
	   int tMode[][];
	   int newH;
	   int crtH[];
	   int myPt;
	   int iter;
	   fams_res_cont res = new fams_res_cont(nPoints);
	   for (i = 0; i < M2; i++) {
		   for (j = 0; j < Bs2; j++) {
			   HT2[i][j].whichCut = 0;
			   for (k = 0; k < HT2[i][j].dp[0].length; k++) {
				   HT2[i][j].dp[k] = null;
			   }
			   HT2[i][j].dp = null;
		   }
	   }
	   for (i = 0; i < M2; i++) {
		   hs2[i] = 0;
	   }
	   oldMean = new int[nDims];
	   crtMean = new int[nDims];
	   
	   tMode = new int[nPoints][];
	   Preferences.debug(" Start MS iterations", Preferences.DEBUG_ALGORITHM);
	   myPt = nsel/10;
	   /*for(jj=0; jj<nsel; jj++)
	   {
	      if((jj%myPt)==0)
	         //bgLog(".");
	      who = psel[jj];
	      currentpt = points[who];
	      for (i = 0; i < dataSize; i++) {
	    	  crtMean[i] = currentpt.data[i];
	      }
	      hmodes[jj] = currentpt.window;
	      tMode[jj]= 1;
	      for(iter=0; notEq(oldMean,crtMean) && (iter<FAMS_MAXITER); iter++)
	      {
	         if(!noLSH)
	         {
	            if((sol=getNearestNeighbours2H(crtMean,HT,hs,cuts,
	               res,tMode[jj],
	               HT2,hs2)) != null)
	            {
	               if(sol == (unsigned short*)1)
	               {
	                  tMode[jj] = &modes_[jj*d_];
	                  memcpy(tMode[jj], crtMean, dataSize_);
	               }
	               else
	               {
	                  tMode[jj] = &modes_[jj*d_];
	                  memcpy(tMode[jj], sol, dataSize_);
	                  break;
	               }
	            }
	         }
	         memcpy(oldMean, crtMean, dataSize_);
	         if(!(newH=DoMeanShiftAdaptiveIteration(res,oldMean,crtMean)))
	         {
	            memcpy(crtMean, oldMean, dataSize_);
	            break;
	         }
	         *crtH=newH;
	      }
	      if(tMode[jj]==(unsigned short*)1)
	      {
	         tMode[jj] = &modes_[jj*d_];
	         memcpy(tMode[jj], crtMean, dataSize_);
	      }
	   }
	   delete [] oldMean;
	   delete [] crtMean;
	   delete [] tMode;
	   bgLog("done.\n");*/
	}
	
	private int[] getNearestNeighbours2H(int who[], fams_hash_entry HT[][], int hs[],
		       fams_cut cuts[][], fams_res_cont res,
		       int solution[], fams_hash_entry2 HT2[][], int hs2[])
		{
		   int i;
		   /*for(i=0; i<L_; i++)
		   {
		      evalCutRes(who,cuts[i],t_cut_res[i]);
		      t_m[i] = HashFunction(t_cut_res_[i],i,K_,M_,&t_hjump_[i]);
		      t_m2_[i] = HashFunction(&t_cut_res_[i][1],i,K_-1);
		   }
		   if(FAMS_DO_SPEEDUP)
		   {
		      int hjump2;
		      int hf = HashFunction(t_m_,0,L_,M2_,&hjump2);
		      int hf2 = HashFunction(&t_m_[L_/2-1],0,L_/2);
		      unsigned short *old_sol =  FindInHash(HT2,hs2,hf,hf2,M2_,hjump2);
		      if(old_sol!= NULL && old_sol != (unsigned short*)1)
		         return old_sol;
		      
		      if(old_sol == NULL)
		         InsertIntoHash(HT2,hs2,hf,hf2,solution,M2_,hjump2);
		   }
		   if(memcmp(t_m_,t_old_m_,sizeof(int)*L_)==0)
		   {
		      return NULL;
		   }
		   memcpy(t_old_m_,t_m_,sizeof(int)*L_);
		   res.clear();
		   nnres2_++;
		   for(i=0; i<L_; i++)
		      AddDataToRes(HT,hs,res,t_m_[i],Bs,M_,i,nnres2_,t_m2_[i],t_hjump_[i]);
           */
		   return null;
		}

	
	private boolean notEq(int in_d1[], int in_d2[])
	{
		   for(int in_i=0; in_i<nDims; in_i++)
		      if(in_d1[in_i] != in_d2[in_i])
		         return true;
		   return false;
		}

	
	// compute the pilot h_i's for the data points
	private void computePilot(fams_hash_entry HT[][],int hs[], fams_cut cuts[][], String pilot_file_name)
	{
	   final int win_j = 10,max_win=7000;
	   int i,j;
	   int nn;
	   int wjd = (int)(win_j*nDims);
	   int num_l[] = new int[1000];
	   fams_res_cont res = new fams_res_cont(nPoints);
	   if(!loadBandwidths(pilot_file_name))
	   {
	      Preferences.debug("compute bandwidths...", Preferences.DEBUG_ALGORITHM);
	      for(j=0; j<nPoints; j++)
	      { 
	         int numn=0;
	         int numns[] = new int[max_win/win_j];
	         int nel;
	         if(noLSH)
	         {
	            nel = nPoints;
	            for(i=0; i<nel; i++)
	            {
	               famsPoint pt = points[i];
	               nn = distL1(points[j],pt) / wjd;
	               if(nn <max_win/win_j)
	                  numns[nn]++;	
	            }
	         }
	         else
	         {
	            getNearestNeighbours(points[j],HT,hs,cuts,res,0,num_l);
	            nel = res.nel;      
	            for(i=0; i<nel; i++)
	            {
	               famsPoint pt = res.vec[i];
	               nn = distL1(points[j],pt) / wjd;
	               if(nn <max_win/win_j)
	                  numns[nn]++;	
	            }
	         }
	         for(nn=0; nn<max_win/win_j; nn++)
	         {
	            numn+=numns[nn];
	            if(numn>k_neigh)
	            {
	               break;
	            }
	         }	
	         points[j].window=(nn+1)*wjd;
	      }
	      saveBandwidths(pilot_file_name);
	   }
	   else
	      Preferences.debug("load bandwidths...",Preferences.DEBUG_ALGORITHM);
	   for(j=0; j<nPoints; j++){
	      points[j].weightdp2 = (float) Math.pow(FAMS_FLOAT_SHIFT/points[j].window, (nDims+2)*FAMS_ALPHA);
//	      points_[j].weightdp2_ = (float) (1.0/pow(points_[j].window_, (d_+2)*FAMS_ALPHA));
	   }
	}
	
	private boolean loadBandwidths(String fn)
	{
        int n;
        int i;
        float bw;
        float deltaVal;
		try {
	    	raFile = new RandomAccessFile(fn, "r");
	    }
	    catch (IOException e) { 
	    	return false;
	    }
		try {
			n = getInt(endianess);
		}
		catch (IOException e) {
			return false;
		}
		if (n != nPoints) {
			try {
				raFile.close();
				return false;
			}
			catch (IOException e) {
				return false;
			}
		} // if (n != nPoints)
		deltaVal = maxVal - minVal;
		for (i = 0; i < nPoints; i++) {
		    try {
		    	bw = getFloat(endianess);
		    	 points[i].window = (int) (65535.0*(bw)/deltaVal);
		    }
		    catch (IOException e) {
		    	try {
		    		raFile.close();
		    		return false;
		    	}
		    	catch (IOException e2) {
		    		return false;
		    	}
		    }
		} // for (i = 0; i < nPoints; i++)
		try {
			raFile.close();
		}
		catch (IOException e) {
			return false;
		}
		return true;
	}
	
	private void saveBandwidths(String fn)
	{
		int i;
		float bw;
		float deltaVal;
		try {
	    	raFile = new RandomAccessFile(fn, "rw");
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("Error in saveBandwidths raFile = new RandomAccessFile");
	    	return;
	    }
		try {
			raFile.setLength(0);
		}
		catch (IOException e) {
	    	MipavUtil.displayError("Error in saveBandwidths raFile.setLength(0)");
	    	return;
	    }
		try {
			writeInt(nPoints,endianess);
		}
		catch (IOException e) {
	    	MipavUtil.displayError("Error in saveBandwidths writeInt");
	    	return;
	    }
		deltaVal = maxVal - minVal;
		for (i = 0; i < nPoints; i++) {
			bw = (float) (points[i].window*deltaVal/65535.0);
			try {
				writeFloat(bw, endianess);
			}
			catch (IOException e) {
		    	MipavUtil.displayError("Error in saveBandwidths writeFloat");
		    	return;
		    }
		} // for (i = 0; i < nPoints; i++)
		try {
			raFile.close();
		}
		catch (IOException e) {
	    	MipavUtil.displayError("Error in saveBandwidths raFile.close()");
	    	return;
	    }
		return;
	}


	/**
     * Reads four signed bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the integer read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final int getInt(final boolean bigEndian) throws IOException {

        raFile.readFully(byteIntBuffer);

        if (bigEndian) {
            return ( ( (byteIntBuffer[0] & 0xff) << 24) | ( (byteIntBuffer[1] & 0xff) << 16)
                    | ( (byteIntBuffer[2] & 0xff) << 8) | (byteIntBuffer[3] & 0xff)); // Big Endian
        } else {
            return ( ( (byteIntBuffer[3] & 0xff) << 24) | ( (byteIntBuffer[2] & 0xff) << 16)
                    | ( (byteIntBuffer[1] & 0xff) << 8) | (byteIntBuffer[0] & 0xff));
        }
    }
    
    /**
     * Reads four unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the float read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final float getFloat(final boolean bigEndian) throws IOException {

        raFile.readFully(byteFloatBuffer);

        int tmpInt;

        if (bigEndian) {
            tmpInt = ( ( (byteFloatBuffer[0] & 0xff) << 24) | ( (byteFloatBuffer[1] & 0xff) << 16)
                    | ( (byteFloatBuffer[2] & 0xff) << 8) | (byteFloatBuffer[3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = ( ( (byteFloatBuffer[3] & 0xff) << 24) | ( (byteFloatBuffer[2] & 0xff) << 16)
                    | ( (byteFloatBuffer[1] & 0xff) << 8) | (byteFloatBuffer[0] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }
    
    /**
     * Writes an int as four bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeInt(final int data, final boolean bigEndian) throws IOException {

        if (bigEndian) {
            byteIntBuffer[0] = (byte) (data >>> 24);
            byteIntBuffer[1] = (byte) (data >>> 16);
            byteIntBuffer[2] = (byte) (data >>> 8);
            byteIntBuffer[3] = (byte) (data & 0xff);
        } else {
            byteIntBuffer[0] = (byte) (data & 0xff);
            byteIntBuffer[1] = (byte) (data >>> 8);
            byteIntBuffer[2] = (byte) (data >>> 16);
            byteIntBuffer[3] = (byte) (data >>> 24);
        }

        raFile.write(byteIntBuffer);
    }
    
    /**
     * Writes a float as four bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeFloat(final float data, final boolean bigEndian) throws IOException {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        writeInt(tmpInt, bigEndian);
    }
	
	private void bgSort(long ra[], int nVec)
	{
	   int l;
	   int ir;
	   int i;
	   int j;
	   long n;
	   n = nVec;
	   long rra;
	   
	   if (n<2)
	      return;
	   l = (int)(n>>1)+1;
	   ir = (int)n;
	   for (;;)
	   {
	      if (l>1)
	      {
	         rra = ra[(--l)-1];
	      }
	      else
	      {
	         rra = ra[ir-1];
	         ra[ir-1] = ra[1-1];
	         if (--ir==1)
	         {
	            ra[1-1] = rra;
	            break;
	         }
	      }
	      i = l;
	      j = l+l;
	      while (j<=ir)
	      {
	         if (j<ir && ra[j-1]<ra[j+1-1])
	            j++;
	         if (rra<ra[j-1])
	         {
	            ra[i-1] = ra[j-1];
	            i = j;
	            j <<= 1;
	         }
	         else
	            j = ir+1;
	      }
	      ra[i-1] = rra;
	   }

	}
	
	// Choose a subset of points on which to perform the mean shift operation
	private void selectMSPoints(double percent, int jump) {
	    int i;
	    int tsel;
	    
	    if (percent > 0.0) {
	        tsel = (int)(nPoints * percent /100.0);
	        if (tsel != nsel) {
	        	cleanSelected();
	        	nsel = tsel;
	        	psel = new int[nsel];
	        	modes = new int[nsel * nDims];
	        	hmodes = new int[nsel];
	        }
	        for (i = 0; i < nsel; i++) {
	        	psel[i] = Math.min(nPoints-1, (int)(srand.nextDouble()*nPoints));
	        }
	    }
	    else {
	        tsel = (int)Math.ceil(((double)nPoints)/((double)jump));
	        if (tsel != nsel) {
	        	cleanSelected();
	        	nsel = tsel;
	        	psel = new int[nsel];
	        	modes = new int[nsel * nDims];
	        	hmodes = new int[nsel];
	        }
	        for (i = 0; i < nsel; i++) {
	        	psel[i] = i * jump;
	        }
	    }
	}
	
	// Compute real bandwidths for selected points
	private void computeRealBandwidths(int h) {
	    final int win_j = 10;
	    final int max_win = 7000;
	    int i, j;
	    int nn;
	    int wjd;
	    int who;
	    int numn;
	    int numns[];
	    
	    wjd = (int) (win_j * nDims);
	    if (h == 0) {
	    	for (j = 0; j < nsel; j++) {
	    		who = psel[j];
	    		numn = 0;
	    		numns = new int[max_win/win_j];
	    		for (i = 0; i < nPoints; i++) {
	    		    nn = distL1(points[who], points[i])/wjd;
	    		    if (nn < max_win/win_j) {
	    		    	numns[nn]++;
	    		    }
	    		} // for (i = 0; i < nPoints; i++)
	    		for (nn = 0; nn < max_win/win_j; nn++) {
	    		    numn += numns[nn];
	    		    if (numn > k_neigh) {
	    		    	break;
	    		    }
	    		} // for (nn = 0; nn < max_win/win_j; nn++)
	    		points[who].setWindow((nn+1)*win_j);
	    	} // for (j = 0; j < nsel; j++)
	    } // if (h == 0)
	    else {
	    	for (j = 0; j < nsel; j++) {
	    		who = psel[j];
	    		points[who].setWindow(h);
	    	}
	    } // else
	}
	
	private long doFindKLIteration(int K, int L, float scores[]) {
		int i;
		int j;
		fams_hash_entry HT[][];
	    int hs[];
	    fams_cut cuts[][];
	    boolean cut_res[];
	    long t1;
	    long t2;
	    long timeElapsed;
	    int hjump[] = new int[1];
	    int m;
	    int m2;
	    
	    K_ = K;
	    L_ = L;
		
		// Allocate memory for the hash table
		M = getPrime(3*nPoints*L_/Bs);
		HT = new fams_hash_entry[M][Bs];
		for (i = 0; i < M; i++) {
			for (j = 0; j < Bs; j++) {
				HT[i][j] = new fams_hash_entry();
			}
		}
		hs = new int[M];
		initHash(K_+L_);
		
		// Build partitions
		cuts = new fams_cut[L_][FAMS_MAX_K];
		for (i = 0; i < L_; i++) {
			for (j = 0; j < FAMS_MAX_K; j++) {
				cuts[i][j] = new fams_cut();
			}
		}
		cut_res = new boolean[FAMS_MAX_K];
		makeCuts(cuts);
		
		// Insert data into partitions
		for (j = 0; j < nPoints; j++) {
		    for (i = 0; i < L_; i++) {
		        evalCutRes(points[j], cuts[i],cut_res);
		        m = hashFunction(cut_res, 0, i, K_, M, hjump);
		        m2 = hashFunction(cut_res, 1, i, K_ - 1, 0, null);
		        addDataToHash(HT, hs, points[j], m, Bs, M, i, m2, hjump[0]);
		    } // for (j = 0; j < nPoints; j++)
		} // for (j = 0; j < nPoints; j++)
		
		// Compute scores
		t1 = System.currentTimeMillis();
		computeScores(HT, hs, cuts, scores);
		t2 = System.currentTimeMillis();
		timeElapsed = t2 - t1;
		
		// Clean
		for (i = 0; i < L_; i++) {
			cuts[i] = null;
		}
		cuts = null;
		hs = null;
		for (i = 0; i < M; i++) {
			for (j = 0; j < Bs; j++) {
				HT[i][j].getPt().setData(null);
				HT[i][j].setPt(null);
			}
		}
		for (i = 0; i < M; i++) {
			HT[i] = null;
		}
		HT = null;
		return timeElapsed;
	}
	
	// compute the pilot h_i's for the data points
	private void computeScores(fams_hash_entry HT[][], int hs[], fams_cut cuts[][], float scores[])
	{
	   final int win_j = 10,max_win=7000;
	   int i,j,who;
	   int nn;
	   int wjd = (int)(win_j*nDims);
	   int num_l[] = new int[1000];
	   for (i = 0; i < L_; i++) {
		   scores[i] = 0.0f;
	   }
	
	   fams_res_cont res = new fams_res_cont(nPoints);
	   for(j=0; j<nsel; j++)
	   { 
	      who = psel[j];
	      int numn;
	      int nl=0;
	      int numns[] = new int[max_win/win_j];
	      int nel;
	      if(noLSH)
	      {
	         nel = nPoints;
	         num_l[L_]=nPoints+1;
	         for(i=0; i<nel; i++)
	         {
	            famsPoint pt = points[i];
	            nn = distL1(points[who],pt) / wjd;
	            if(nn <max_win/win_j)
	               numns[nn]++;	
	            if(i == (num_l[nl]-1))
	            {
	               numn=0;
	               for(nn=0; nn<max_win/win_j; nn++)
	               {
	                  if(numn>k_neigh)
	                     break;
	               }
	               for(; (num_l[nl]-1) == i; nl++)
	                  scores[nl] += (float) (((nn+1.0)*win_j)/points[who].window);
	            }
	         }
	      }
	      else
	      {
	         getNearestNeighbours(points[who],HT,hs,cuts,res,0,num_l);
	         nel = res.getNel();
	         num_l[L_] = nPoints+1;
	         for(i=0; i<nel; i++)
	         {
	            famsPoint pt = res.getVec()[i];
	            nn = distL1(points[who],pt) / wjd;
	            if(nn <max_win/win_j)
	               numns[nn]++;
	            if(i == (num_l[nl]-1))
	            {
	               numn=0;
	               for(nn=0; nn<max_win/win_j; nn++)
	               {
	                  numn+=numns[nn];
	                  if(numn>k_neigh)
	                     break;
	               }
	               for(; (num_l[nl]-1) == i; nl++)
	                  scores[nl] += (float) (((nn+1.0)*win_j)/points[who].window);
	            }
	         }
	      }
	      numn=0;
	      for(nn=0; nn<max_win/win_j; nn++)
	      {
	         numn+=numns[nn];
	         if(numn>k_neigh)
	            break;
	      }	
	   }
	   for(j=0; j<L_; j++)
	      scores[j]/=nsel;

	}
	
	//perform an LSH query

	private void getNearestNeighbours(famsPoint who, fams_hash_entry HT[][], int hs[],
	     fams_cut cuts[][], fams_res_cont res, int print,int num_l[])
	{
	   int i;
	   for(i=0; i<L_; i++)
	      evalCutRes(who,cuts[i],t_cut_res[i]);
	   if(!compareCutRes(t_cut_res,t_old_cut_res))
	   {
	      return;
	   }
	   for (i = 0; i < t_old_cut_res.length; i++) {
		   t_old_cut_res[i] = t_cut_res[i];
	   }
	   res.clear();
	   nnres1++;
	   for(i=0; i<L_; i++)
	   {
	      int hjump[] = new int[1];
	      int m = hashFunction(t_cut_res[i],0,i,K_,M,hjump);
	      int m2 = hashFunction(t_cut_res[i],1,i,K_-1, 0, null);
	      addDataToRes(HT,hs,res,m,Bs,M,i,nnres1,m2,hjump[0]);
	      num_l[i] = res.getNel();
	   }
	}
	
	// Perform a query to one partition and retreive all the points in the cell 

	private void addDataToRes(fams_hash_entry HT[][],int hs[],fams_res_cont res,int where,
	        int Bs,int M,int which, int nnres, int which2, int hjump)
	{
	   int uu;
	   for(;;where = (where+hjump)%M)
	   {
	      for(uu=0; uu<hs[where]; uu++)
	      {
	         if(HT[where][uu].getWhichCut() == which && HT[where][uu].getWhich2() == which2)
	         {
	            if((HT[where][uu].getPt()).getUsedFlag()!=nnres)
	            {
	               res.push_back(HT[where][uu].getPt());
	               (HT[where][uu].getPt()).setUsedFlag(nnres);
	            }
	         }
	      }
	      if(hs[where] < Bs)
	         break;
	   }
	}


	//Compare a pair of L binary vectors
	private boolean compareCutRes(boolean in_cr1[][], boolean in_cr2[][])
	{
	   for (int in_i=0; in_i<L_; in_i++)
	      for (int in_j=0; in_j<K_; in_j++)
	         if (in_cr1[in_i][in_j] != in_cr2[in_i][in_j])
	            return true;
//	      if (memcmp(in_cr1[in_i], in_cr2[in_i], (K_*sizeof(int))) != 0)
//	         return 1;
	   return false;	
	}
	
	// Add a point to the LSH hash table using double hashing 
	private void addDataToHash(fams_hash_entry HT[][],int hs[],famsPoint pt,int where,int Bs,int M,int which,int which2,
	                      int hjump)
	{
	   int nw=0;
	   for(;;where = (where+hjump)%M)
	   {
	      nw++;
	      if(nw > M)
	      {
	         Preferences.debug("LSH hash table overflow exiting\n", Preferences.DEBUG_ALGORITHM);
	         System.exit(-1);
	      }
	      if(hs[where] == Bs)
	         continue;
	      HT[where][hs[where]].setPt(pt);
	      HT[where][hs[where]].setWhichCut((short)which);
	      HT[where][hs[where]].setWhich2(which2);
	      hs[where]++;
	      break;
	   }
	}

	
	/* compute the hash key and and the double hash key if needed 
	It is possible to give M the the size of the hash table and 
	hjump for the double hash key 
	*/

	private int hashFunction(boolean cutVals[], int offset, int whichPartition, int kk,int M,int hjump[])
	{
	   int i;
	   int res = whichPartition;
	   for(i=0; i<kk; i++)
	   {
		  if (cutVals[i+offset]) {
	          res += hashCoeffs[i];
		  }
	   }
	   if(M > 0)
	   {
	      res = Math.abs(res);
	      if(hjump != null)
	         hjump[0] = (res%(M-1))+1;
	      res = res%M;
	   }
	   return res;
	}

	
	//Produce the boolean vector of a data point with a partition 
	private void evalCutRes(famsPoint in_pt, fams_cut in_part[],boolean in_cut_res[])
	{
	   for(int in_i=0; in_i<K_; in_i++)
	      in_cut_res[in_i] =  in_pt.getData()[in_part[in_i].getWhich()] >= in_part[in_i].getWhere();
	}
	
	//Produce the boolean vector of a ms data point with a partition 
	private void evalCutRes(int in_dat[], fams_cut[] in_part, boolean in_cut_res[])
	{
	   for(int in_i=0; in_i<K_; in_i++)
	      in_cut_res[in_i] =  in_dat[in_part[in_i].which] >= in_part[in_i].where;
	}
	
	private void makeCuts(fams_cut cuts[][])
	{
	   int i;
	   for(i=0; i<L_; i++)
	      makeCutL(cuts[i]);
	}
	
	// data-driven uniform partition

	private void makeCutL(fams_cut cut[])
	{
	   int n1 = (int)Math.floor(K_/(1.0*nDims));
	   int i,j;
	   int ncu=0;
	   int w;
	   int which[];
	   int wh;
	   for(i=0; i<nDims; i++)
	   {
	      for(j=0; j<n1; j++)
	      {
	         cut[ncu].setWhich(i);
	         w = Math.min((int)(srand.nextDouble()*nPoints),nPoints-1);
	         cut[ncu].setWhere(points[w].getData()[i]);
	         ncu++;
	      }
	   }
	   /*
	   int *which = new int[d_];
	   for (i=0; i<d_; i++)
	      which[i]=i;
	   int wh, ndleft, itmp;
	   ndleft = d_;
	   for (i=0; i<(K_-ncu); i++)
	   {
	      wh = MyMin((int)(drand48()*ndleft),ndleft-1);
	      itmp = which[wh+i];
	      which[wh+i] = which[i];
	      which[i] = itmp;
	      ndleft--;
	   }

	   for(i=0 ; ncu < K_; ncu++, i++)
	   {
	      w = MyMin((int)(drand48()*n_),n_-1);
	      cut[ncu].which_ = which[i];
	      cut[ncu].where_ = points_[w].data_[which[i]];
	   }
	   delete [] which;
	   */
	   which = new int[nDims];
	   for (; ncu<K_; )
	   {
	      wh = Math.min((int)(srand.nextDouble()*nDims),nDims-1);
	      if(which[wh]!=0)
	         continue;
	      which[wh]=1;
	      w = Math.min((int)(srand.nextDouble()*nPoints), nPoints-1);
	      cut[ncu].setWhich(wh);
	      cut[ncu].setWhere(points[w].getData()[wh]);
	      ncu++;
	   }
	   which = null;
	}


	
	private void initHash(int nk)
	{  
	   long t1;
	   Random rand;
	   hashCoeffs = null;
	   hashCoeffs = new int[nk];
	   t1 = System.currentTimeMillis();
	   rand = new Random(t1);
	   for(int i=0; i<nk; i++)
	      hashCoeffs[i] = rand.nextInt(Integer.MAX_VALUE);
	}

	
	// return a prime number greater than minp
	private int getPrime(int minp)
	{	
	   int i,j;
	   for(i=minp%2==0? minp+1 :minp; ; i+=2)
	   {
	      int sqt = (int)Math.sqrt(i);
	      if(i % 2==0)
	         continue;
	      for(j=3; j<sqt; j+=2)
	      {
	         if(i % j == 0)
	            break;
	      }
	      if(j >= sqt)
	         return i;
	   }
	};
	
	private int distL1(famsPoint inPt1, famsPoint inPt2) {
		int ini;
		int inres = 0;
		for (ini = 0; ini < nDims; ini++) {
			inres += Math.abs(inPt1.data[ini] - inPt2.data[ini]);
		}
		return inres;
	}
	
	private void cleanSelected() {
		if (nsel > 0) {
			psel = null;
			modes = null;
			hmodes = null;
			nsel = 0;
		}
	}
	
	private String[] retrieveValues(String inString) {
        String outString[] = null;
        int i;
        int numValues = 0;
        Vector<Integer> firstValue = new Vector<Integer>();
        Vector<Integer> lastValue = new Vector<Integer>();

        if ((inString != null) && (!inString.isEmpty())) {
        	for (i = 0; i < inString.length(); i++) {
        	    if ((inString.charAt(i) > 0x20) &&	((i == 0) || (inString.charAt(i-1) <= 0x20))) {
        	    	numValues++;
        	    	firstValue.add(i);
        	    	if (i == inString.length() - 1) {
        	    		lastValue.add(i);
        	    	}
        	    }
        	    else if ((inString.charAt(i) <= 0x20) && (inString.charAt(i-1) > 0x20)) {
        	    	lastValue.add(i-1);
        	    }
        	    else if ((i == inString.length() - 1) && (inString.charAt(i) > 0x20)) {
        	    	lastValue.add(i);
        	    }
        	}
        	outString = new String[numValues];
        	char[] val = new char[inString.length()];
            for (i = 0; i < inString.length(); i++) {
            	val[i] = inString.charAt(i);
            }
        	for (i = 0; i < numValues; i++) {
        	    outString[i] = new String(val, firstValue.get(i), lastValue.get(i) - firstValue.get(i) + 1);    
        	} // for (i = 0; i < numValues; i++)
            
            return outString;
            
        } // if ((inString != null) && (!inString.isEmpty()))
        else {
            return null;
        }

    }
	
	private class famsPoint {
		private int data[];
		private int usedFlag;
		private int window;
		float weightdp2;
		
		public famsPoint() {
			
		}
		
		public famsPoint(famsPoint d2) {
			usedFlag = d2.usedFlag;
			weightdp2 = d2.weightdp2;
			window = d2.window;
			data = d2.data;
		}
		
		public void setData(int data[]) {
			this.data = data;
		}
		
		public void setUsedFlag(int usedFlag) {
			this.usedFlag = usedFlag;
		}
		
		public void setWindow(int window) {
			this.window = window;
		}
		
		public void setWeightdp2(float weightdp2) {
			this.weightdp2 = weightdp2;
		}
		
		public int[] getData() {
			return data;
		}
		
		public int getUsedFlag() {
			return usedFlag;
		}
		
		public int getWindow() {
			return window;
		}
		
		public float getWeightdp2() {
			return weightdp2;
		}
	}
	
    private class fams_hash_entry {
		  private short whichCut;
		  private int which2;
		  private famsPoint pt;
		  
		  public fams_hash_entry() {
		      
		  }
		  
		  public void setWhichCut(short whichCut) {
			  this.whichCut = whichCut;
		  }
		  
		  public void setWhich2(int which2) {
			  this.which2 = which2;
		  }
		  
		  public void setPt(famsPoint pt) {
			  this.pt = pt;
		  }
		  
		  public short getWhichCut() {
			  return whichCut;
		  }
		  
		  public int getWhich2() {
			  return which2;
		  }
		  
		  public famsPoint getPt() {
			  return pt;
		  }
	}
    
    private class fams_hash_entry2{
    	  int whichCut;
    	  int dp[][];
    	  
    	  public fams_hash_entry2() {
    		  
    	  }
    	  
    	  public void setWhichCut(int whichCut) {
    		  this.whichCut = whichCut;
    	  }
    	  
    	  public void setDp(int dp[][]) {
    		  this.dp = dp;
    	  }
    	  
    	  public int getWhichCut() {
    		  return whichCut;
    	  }
    	  
    	  public int[][] getDp() {
    		  return dp;
    	  }
    }
    
    private class fams_cut {
    	private int which;
	    private int where;
	    
	    public fams_cut() {
	    	
	    }
	    
	    public void setWhich(int which) {
	    	this.which = which;
	    }
	    
	    public void setWhere(int where) {
	    	this.where = where;
	    }
	    
	    public int getWhich() {
	    	return which;
	    }
	    
	    public int getWhere() {
	    	return where;
	    }
    }
    
    private class fams_res_cont{
    	    private int nel;
    		private famsPoint vec[];
    		public fams_res_cont() {
    			nel = 0;
    			vec = new famsPoint[1];
    		}
    		public fams_res_cont(int n){
    			nel=0;
    			vec = new famsPoint[n];
    		};
    		
    		public int getNel() {
    			return nel;
    		}
    		
    		public famsPoint[] getVec() {
    			return vec;
    		}
    		
    		public void clear() {
    		    nel = 0;	
    		}
    		
    		public void push_back(famsPoint in_el)
    		   {vec[nel++] = in_el;};
    }
	
}