package gov.nih.mipav.model.algorithms;



import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import java.io.*;
import java.util.*;

public class StochasticForests extends AlgorithmBase {
	// Note that while Random Forests is the name usually applied to this type
	// of algorithm, Random Forests(tm) is a trademark of Leo Breiman and Adele Cutler and is 
	// licensed exclusively to Salford Systems for the commercial release of the software.
	// Our trademarks also include RF(tm), RandomForests(tm), RandomForest(tm) and Random Forest(tm).
	
	// This is a port from C++ to Java of the ranger package of version 0.9.7 of A Fast Implementation
	// of Random Forests.  The date of the original code is 3/29/2018.  The authors of the original code
	// are Marvin N. Wright, Stefan Wager, and Philipp Probst.  The maintainer of the original code is
	// Marvin N. Wright at cran@wrig.de.  The license of the C++ core of version 0.9.7 is a MIT license. 
	//(The R package which is not used in this port is still a GPL3 license.)
	
	// Copyright <2014-2018> <Marvin N. Wright>

	// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
	// associated documentation files (the "Software"), to deal in the Software without restriction, 
	// including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
	// and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
	// subject to the following conditions:

	// The above copyright notice and this permission notice shall be included in all copies or substantial
	// portions of the Software.

	// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
	// NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
	// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
	// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
	
	// ### Introduction
	// ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, 
	// particularly suited for high dimensional data. Classification, regression, probability estimation
	// and survival forests are supported. Classification and regression forests are implemented as in
	// the original Random Forest (Breiman 2001), survival forests as in Random Survival Forests
	// (Ishwaran et al. 2008). For probability estimation forests see Malley et al. (2012). 
	
	// ### References
	// 1.) Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High
	// Dimensional Data in C++ and R. Journal of Statistical Software 77:1-17.
	// http://dx.doi.org/10.18637/jss.v077.i01.
	// 2.) Schmid, M., Wright, M. N. & Ziegler, A. (2016). On the use of Harrell's C for clinical risk
	// prediction via random survival forests. Expert Systems with Applications 63:450-459. 
	// http://dx.doi.org/10.1016/j.eswa.2016.07.018.
	// 3.) Wright, M. N., Dankowski, T. & Ziegler, A. (2017). Unbiased split variable selection for
	// random survival forests using maximally selected rank statistics. Statistics in Medicine. 
	// http://dx.doi.org/10.1002/sim.7212.
	// 4.) Breiman, L. (2001). Random forests. Machine learning 45:5-32.
	// 5.) Ishwaran, H., Kogalur, U. B., Blackstone, E. H., & Lauer, M. S. (2008). 
	// Random survival forests. The Annals of Applied Statistics 2:841-860.
	// 6.) Malley, J. D., Kruppa, J., Dasgupta, A., Malley, K. G., & Ziegler, A. (2012). 
	// Probability machines: consistent probability estimation using nonparametric learning machines. 
	// Methods of Information in Medicine 51:74-81.

    // Tree types, probability is not selected by ID
	private enum TreeType {
		TREE_CLASSIFICATION,
		TREE_REGRESSION,
	    TREE_SURVIVAL,
	    TREE_PROBABILITY
	};
	
	// Memory modes
	private enum MemoryMode {
		MEM_DOUBLE,
		MEM_FLOAT,
		MEM_CHAR
	};
	
	// Mask and offset to store 2 bit values in bytes
	static final int mask[] = new int[]{192,48,12,3};
	static final int offset[] = new int[]{6,4,2,0};
	
	// Variable importance
	private enum ImportanceMode {
		IMP_NONE,
		IMP_GINI,
		IMP_PERM_BREIMAN,
		IMP_PERM_LIAW,
		IMP_PERM_RAW,
		IMP_GINI_CORRECTED
	};
	
	// Split mode
	private enum SplitRule {
		LOGRANK,
		AUC,
		AUC_IGNORE_TIES,
		MAXSTAT,
		EXTRATREES
	};
	
	// Prediction type
	private enum PredictionType {
		RESPONSE,
		TERMINALNODES
	};
	
	// Default values
	final int DEFAULT_NUM_TREE = 500;
	final int DEFAULT_NUM_THREADS = 0;
	final ImportanceMode DEFAULT_IMPORTANCE_MODE = ImportanceMode.IMP_NONE;
	
	final int DEFAULT_MIN_NODE_SIZE_CLASSIFICATION = 1;
	final int DEFAULT_MIN_NODE_SIZE_REGRESSION = 5;
	final int DEFAULT_MIN_NODE_SIZE_SURVIVAL = 3;
	final int DEFAULT_MIN_NODE_SIZE_PROBABILITY = 10;

	final SplitRule DEFAULT_SPLITRULE = SplitRule.LOGRANK;
	final double DEFAULT_ALPHA = 0.5;
	final double DEFAULT_MINPROP = 0.1;

	final PredictionType DEFAULT_PREDICTIONTYPE = PredictionType.RESPONSE;
	final int DEFAULT_NUM_RANDOM_SPLITS = 1;

	//const std::vector<double> DEFAULT_SAMPLE_FRACTION = std::vector<double>({1});

	// Interval to print progress in seconds
	final double STATUS_INTERVAL = 30.0;

	// Threshold for q value split method switch
	final double Q_THRESHOLD = 0.02;
	
	private class Data {
		protected Vector<String> variable_names;
		protected int num_rows = 0;
		protected int num_rows_rounded = 0;
		protected int num_cols = 0;
		
		protected char snp_data[] = null;
		protected int num_cols_no_snp = 0;
		protected boolean externalData = true;
		
		protected int index_data[] = null;
		protected Vector<Vector<Double>> unique_data_values = null;
		protected int max_num_unique_values = 0;
		int i;
		
		// Variable to not split at (only dependent_varID for non-survival trees)
		protected Vector<Integer> no_split_variables = null;
		
		// For each varID true if ordered
	    protected Vector<Boolean> is_ordered_variable = null;
	    
	    // Permuted samples for corrected impurity importance
	    protected Vector<Integer> permuted_sampleIDs = null;
	    
	    public void dispose() {
	    	index_data = null;
	    }
	    
	    public int getVariableID(String variable_name) {
	         for (i = 0; i < variable_names.size(); i++) {
	        	 if(variable_names.get(i).equals(variable_name)) {
	        		 return i;
	        	 }
	         } // for (i = 0; i < variable_names.size(); i++)
	         return -1;
	    }
	    
	    public void addSnpData(char snp_data[], int num_cols_snp) {
	    	num_cols = num_cols_no_snp + num_cols_snp;
	    	num_rows_rounded = roundToNextMultiple(num_rows, 4);
	    	this.snp_data = snp_data;
	    }
	    
	    // #nocov start
	    /*public boolean loadFromFile(String filename) {
	    	boolean result;
	    	
	    	// Open input file
	    	File file = new File(filename);
	    	BufferedReader input_file;
	    	try {
	    	    input_file = new BufferedReader(new FileReader(file));
	    	}
	    	catch (FileNotFoundException e) {
	    		MipavUtil.displayError("Could not find file " + filename);
	    		return false;
	    	}
	    	
	    	// Count number of rows
	    	int line_count = 0;
	    	String line;
	        while (true) {
	        	try {
	    		    line = input_file.readLine();
	        	}
	    		catch (IOException e) {
	    			MipavUtil.displayError("IO exception on readLine of " + filename);
	    			return false;
	    		}
	        	if (line != null) {
	        		line_count++;
	        	}
	        	else {
	        		break;
	        	}
	    	} // while (true)
	        num_rows = line_count-1;
	        try {
	    	    input_file.close();
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IO exception on close of " + filename);
	        }
	        try {
	    	    input_file = new BufferedReader(new FileReader(file));
	    	}
	    	catch (FileNotFoundException e) {
	    		MipavUtil.displayError("Could not find file " + filename);
	    		return false;
	    	}
	        
	        // Check if comma, semicolon, or whitespace separated
	        String header_line;
	        try {
	            header_line = input_file.readLine();
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IO exception reading header line of " + filename);
	        	return false;
	        }
	        
	        // Find out if comma, semicolon, or whitespace separated and call appropriate method
	        if (header_line.indexOf(",") != -1) {
	        	result = loadFromFileOther(input_file, header_line, ",");
	        }
	        else if (header_line.indexOf(";") != -1) {
	        	result = loadFromFileOther(input_file, header_line, ";");	
	        }
	        else {
	        	result = loadFromFileWhitespace(input_file, header_line);
	        }
	        
	        externalData = false;
	        try {
	    	    input_file.close();
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IO exception on close of " + filename);
	        }
	        return result;
	    } // loadFromFile
	    
	    public boolean loadFromFileWhitespace(BufferedReader input_file, String header_line) {
	    	// Read header
	    	String[] header_tokens;
	    	header_tokens = header_line.split(" ");
	    	for (i = 0; i < header_tokens.length; i++) {
	    		variable_names.add(header_tokens[i]);
	    	}
	    	num_cols = variable_names.size();
	    	num_cols_no_snp = num_cols;
	    	
	    	// Read body
	    	boolean error = false;
	    	String line;
	    	int row = 0;
	    	while (true) {
	    		try {
	    	        line = input_file.readLine();
	    		}
	    		catch (IOException e) {
	    			MipavUtil.displayError("IO exception on readLine from input_file");
	    			return false;
	    		}
	    		if (line == null) {
	    			break;
	    		}
	    		String tokens[];
	    		tokens = line.split(" ");
	    		for (i = 0; i < tokens.length; i++) {
	    			double dValue = Double.valueOf(tokens[i]).doubleValue();
	    		}
	    	} // while true
	    } */
	    
	    
	} // private class Data
	
	private int roundToNextMultiple(int value, int multiple) {
		if (multiple == 0) {
			return value;
		}
		
		int remainder = value % multiple;
		if (remainder == 0) {
			return value;
		}
		return value + multiple - remainder;
	}

	
	public void runAlgorithm() {
		
	}
}