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
	private int hashCoeffs[];
	
	// temporary
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
	    int Kmax;
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
	
}