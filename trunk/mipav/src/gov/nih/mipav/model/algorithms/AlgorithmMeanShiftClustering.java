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
	// The width * d (d is tne dimension of the data) is the distance under the L1 norm
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
	
	// input points
	private boolean hasPoints;
	
	// selected points on which mean shift is run
	private int nsel;
	private int npm;
	
	// hash table data
	private int hashCoeffs[];
	
	// temporary
	private int nnres1;
	private int nnres2;
	
	private long tt1;
	Random srand;
	
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
	    int Lmax;
	    int Kmax;
	    
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
	}
	
}