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
	
	//private static final int Bs = FAMS_BLOCKSIZE/sizeof(fams_hash_entry);
	private static final int Bs = FAMS_BLOCKSIZE/14;
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
	
	// interval of input data
	private float minVal;
	private float maxVal;
	
	// input points
	private famsPoint points[];
	private int data[][];
	private boolean hasPoints;
	private int nPoints;
	private int nDims;
	private int dataSize;
	// temp work
	double rr[];
	
	// selected points on which mean shift is run
	private int psel[];
	private int nsel;
	private int modes[];
	private long hmodes[];
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
	    int Lmax = 0;
	    int Kmax = 0;
	    RandomAccessFile raFile;
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
	    float scores[];
	    int Lcrt;
	    int Kcrt;
	    int nBest;
	    int LBest[];
	    int KBest[];
	    int ntimes;
	    int is;
	    
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
	    hasPoints = false;
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
	    hasPoints = true;
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
	    
	    if (findOptimalKL) {
	        findEpsilon = epsilon;
	        adaptive = !fixedWidth;
	        
	        if (fixedWidth) {
	        	 hWidth = (int)(65535.0*width/(maxVal - minVal));
	        }
	        else {
	        	hWidth = 0;
	        }
	        findEpsilon = epsilon + 1;
	        
	        // select points on which test is run
	        selectMSPoints(FAMS_FKL_NEL*100.0/nPoints, 0);
	        
	        // Compute bandwidths for selected points
	        computeRealBandwidths(hWidth);
	        
	        // Start finding the correct L for each K
	        scores = new float[FAMS_FKL_TIMES*FAMS_MAX_L];
	        LBest = new int[FAMS_MAX_K];
	        KBest = new int[FAMS_MAX_K];
	        
	        Lcrt = Lmax;
	        Preferences.debug("About to find valid pairs\n", Preferences.DEBUG_ALGORITHM);
	        for (Kcrt = Kmax, nBest = 0; Kcrt >= Kmin; Kcrt -= Kjump, nBest++) {
	            // Do iterations for crt K and L = 1...Lcrt
	        	for (ntimes = 0; ntimes < FAMS_FKL_TIMES; ntimes++) {
	        		
	        	}
	        } // for (Kcrt = Kmax, nBest = 0; Kcrt >= Kmin; Kcrt -= Kjump, nBest++)
	    } // if (findOptimalKL)
	}
	
	// Choose a subset of points on which to perform the mean shift operation
	private void selectMSPoints(double percent, int jump) {
	    int i;
	    int tsel;
	    
	    if (!hasPoints) {
	    	return;
	    }
	    
	    if (percent > 0.0) {
	        tsel = (int)(nPoints * percent /100.0);
	        if (tsel != nsel) {
	        	cleanSelected();
	        	nsel = tsel;
	        	psel = new int[nsel];
	        	modes = new int[nsel * nDims];
	        	hmodes = new long[nsel];
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
	        	hmodes = new long[nsel];
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
		hs = new int[M];
		initHash(K_+L_);
		
		// Build partitions
		cuts = new fams_cut[L_][FAMS_MAX_K];
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