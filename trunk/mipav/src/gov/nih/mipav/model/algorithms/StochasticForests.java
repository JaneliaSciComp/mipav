package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.*;
import javafx.util.Pair;

import java.io.*;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

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
	
	public static int RAND_MAX = 32767;
	
	public static double logprop;
	
	private boolean testUtility = true;
	
	private TreeType treetype;
	
	private boolean probability = false;
	
	private boolean verbose_out = false;
	
	private String dependent_variable_name = null;
	
	private MemoryMode memory_mode;
	
	private String input_file = null;
	
	private int mtry;
	
	private String output_prefix = null;
	
	private int num_trees;
	
	private long seed;
	
	private int num_threads;
	
	private String load_forest_filename = null;
	
	private ImportanceMode importance_mode;
	
	private int min_node_size;
	
	private String split_select_weights_file = null;
	
	private Vector<String> always_split_variable_names;
	
	private String status_variable_name = null;
	
	private boolean sample_with_replacement;
	
	private Vector<String> unordered_variable_names;
	
	private boolean memory_saving_splitting;
	
	private SplitRule splitrule;
	
	private String case_weights_file = null;
	
	private boolean predict_all;
	
	private double sample_fraction;
	
	private double alpha;
	
	private double minprop;
	
	private boolean holdout;
	
	private PredictionType prediction_type;
	
	private int num_random_splits;
	
	private boolean write;
	
	private abstract class Data {
		protected Vector<String> variable_names;
		protected int num_rows = 0;
		protected int num_rows_rounded = 0;
		protected int num_cols = 0;
		
		protected char snp_data[] = null;
		protected int num_cols_no_snp = 0;
		protected boolean externalData = true;
		
		protected int index_data[] = null;
		protected Vector<Vector<Double>> unique_data_values = new Vector<Vector<Double>>();
		protected int max_num_unique_values = 0;
		int i;
		
		// Variable to not split at (only dependent_varID for non-survival trees)
		protected Vector<Integer> no_split_variables = new Vector<Integer>();
		
		// For each varID true if ordered
	    protected Vector<Boolean> is_ordered_variable = new Vector<Boolean>();
	    
	    // Permuted samples for corrected impurity importance
	    protected Vector<Integer> permuted_sampleIDs = new Vector<Integer>();
	    
	    public Data() {
	    	
	    }
	    
	    public Data(Vector<String>variable_names, int num_rows, int num_cols) {
	    	this.variable_names = variable_names;
	    	this.num_rows = num_rows;
	    	this.num_cols = num_cols;
	    	this.num_cols_no_snp = num_cols;
	    }
	    
	    public void dispose() {
	    	index_data = null;
	    }
	    
	    public abstract double get(int row, int column);
	    
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
	    public boolean loadFromFile(String filename) {
	    	boolean result;
	    	
	    	// Open input file
	    	File file = new File(filename);
	    	BufferedReader input_file;
	    	try {
	    	    input_file = new BufferedReader(new FileReader(file));
	    	}
	    	catch (FileNotFoundException e) {
	    		MipavUtil.displayError("Could not find file " + filename);
	    		return true;
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
	    			return true;
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
	        	return true;
	        }
	        try {
	    	    input_file = new BufferedReader(new FileReader(file));
	    	}
	    	catch (FileNotFoundException e) {
	    		MipavUtil.displayError("Could not find file " + filename);
	    		return true;
	    	}
	        
	        // Check if comma, semicolon, or whitespace separated
	        String header_line;
	        try {
	            header_line = input_file.readLine();
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IO exception reading header line of " + filename);
	        	return true;
	        }
	        
	        // Find out if comma, semicolon, or whitespace separated and call appropriate method
	        if (header_line.indexOf(",") != -1) {
	        	result = loadFromFileOther(input_file, header_line, ",");
	        }
	        else if (header_line.indexOf(";") != -1) {
	        	result = loadFromFileOther(input_file, header_line, ";");	
	        }
	        else {
	        	result = loadFromFileOther(input_file, header_line, " ");
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
	    
	    // Use instead of loadFromFileWhitespace by using separator = " ".
	    public boolean loadFromFileOther(BufferedReader input_file, String header_line, String separator) {
	    	// Read header
	    	String[] header_tokens;
	    	header_tokens = header_line.split(separator);
	    	for (i = 0; i < header_tokens.length; i++) {
	    		variable_names.add(header_tokens[i]);
	    	}
	    	num_cols = variable_names.size();
	    	num_cols_no_snp = num_cols;
	    	
	    	// Read body
	    	reserveMemory();
	    	boolean error[] = new boolean[]{false};
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
	    		int column = 0;
	    		tokens = line.split(separator);
	    		for (i = 0; i < tokens.length; i++) {
	    			double dValue = Double.valueOf(tokens[i]).doubleValue();
	    			set(column, row, dValue, error);
                    column++;
	    		}
	    		if (separator.equals(" ")) {
		    		if (column > num_cols) {
		    			MipavUtil.displayError("Too many columns in a row");
		    			return false;
		    		}
		    		else if (column < num_cols) {
		    			MipavUtil.displayError("Too few columns in a row.  Are all values numeric?");
		    			return false;
		    		}
	    		} // if (separator.equals(" "))
	    		row++;
	    	} // while true
	    	num_rows = row;
	    	return error[0];
	    }
	    
	    public void getAllValues(Vector<Double> all_values, Vector<Integer> sampleIDs,
	    		int varID) {
	    	// All values for varID (no duplicates) for given sampleIDs
	    	if (getUnpermutedVarID(varID) < num_cols_no_snp) {
	    	    if (all_values.size() < sampleIDs.size()) {
	    	        all_values.setSize(sampleIDs.size());	
	    	    }
	    	    for (i = 0; i < sampleIDs.size(); i++) {
	    	    	all_values.add(get(sampleIDs.get(i),varID));
	    	    }
	    	    all_values.sort(null);
	    	    for (i = all_values.size()-1; i >= 1; i--) {
	    	    	if (all_values.get(i) == all_values.get(i-1)) {
	    	    		all_values.removeElementAt(i);
	    	    	}
	    	    }
	    	} // if (getUnpermutedVarID(varID) < num_cols_no_snp)
	    	else {
	    		// If GWA data just use 0, 1, 2
	    		all_values.clear();
	    		all_values.add(0.0);
	    		all_values.add(1.0);
	    		all_values.add(2.0);
	    	}
	    }
	    
	    public void getMinMaxValues(double min[], double max[], Vector<Integer> sampleIDs, int varID) {
	        if (sampleIDs.size() > 0) {
	        	min[0] = get(sampleIDs.get(0), varID);
	        	max[0] = min[0];
	        }
	        for ( i = 1; i < sampleIDs.size(); i++) {
	        	double value = get(sampleIDs.get(i), varID);
	        	if (value < min[0]) {
	        		min[0] = value;
	        	}
	        	if (value > max[0]) {
	        		max[0] = value;
	        	}
	        }
	    }
	    
	    public void sort() {
	    	// Reserve memory
	    	index_data = new int[num_cols_no_snp * num_rows];
	    	
	    	// For all columns, get unique values and save index for each observation
	    	for (int col = 0; col < num_cols_no_snp; col++) {
	    		// Get all unique values
	    		Vector<Double>unique_values = new Vector<Double>();
	    		if (unique_values.size() < num_rows) {
	    			unique_values.setSize(num_rows);
	    		}
	    		for (int row = 0; row < num_rows; row++) {
	    			unique_values.add(row,get(row, col));
	    		}
	    		unique_values.sort(null);
	    		for (i = unique_values.size()-1; i >= 1; i--) {
	    	    	if (unique_values.get(i) == unique_values.get(i-1)) {
	    	    		unique_values.removeElementAt(i);
	    	    	}
	    	    }
	    		
	    		// Get index of unique value
	    		for (int row = 0; row < num_rows; row++) {
	    			int idx;
	    			for (idx = 0; idx < unique_values.size(); idx++) {
	    				if (unique_values.get(idx) >= get(row,col)) {
	    					break;
	    				}
	    			}
	    			index_data[col * num_rows + row] = idx;
	    		} // for (int row = 0; row < num_rows; row++)
	    		
	    		// Save unique values
	    	    unique_data_values.add(unique_values);
	    	    if (unique_values.size() > max_num_unique_values) {
	    	    	max_num_unique_values = unique_values.size();
	    	    }
	    	} // for (int col = 0; col < num_cols_no_snp; col++) {
	    } // public void sort()
	    
	    public abstract void reserveMemory();
	    
	    public abstract void set(int col, int row, double value, boolean error[]);
	    
	    public int getUnpermutedVarID(int varID) {
	        if (varID >= num_cols) {
	          varID -= num_cols;

	          for (i = 0; i < no_split_variables.size(); i++) {
	            if (varID >= no_split_variables.get(i)) {
	              ++varID;
	            }
	          }
	        }
	        return varID;
	      }
	    
	    public int getPermutedSampleID(int sampleID) {
	        return permuted_sampleIDs.get(sampleID);
	    }

	    public int getIndex(int row, int col) {
	        // Use permuted data for corrected impurity importance
	        if (col >= num_cols) {
	          col = getUnpermutedVarID(col);
	          row = getPermutedSampleID(row);
	        }

	        if (col < num_cols_no_snp) {
	          return index_data[col * num_rows + row];
	        } else {
	          // Get data out of snp storage. -1 because of GenABEL coding.
	          int idx = (col - num_cols_no_snp) * num_rows_rounded + row;
	          int result = (((snp_data[idx / 4] & mask[idx % 4]) >> offset[idx % 4]) - 1);

	          // TODO: Better way to treat missing values?
	          if (result > 2) {
	            return 0;
	          } else {
	            return result;
	          }
	        }
	      }

	    public double getUniqueDataValue(int varID, int index) {
	        // Use permuted data for corrected impurity importance
	        if (varID >= num_cols) {
	          varID = getUnpermutedVarID(varID);
	        }

	        if (varID < num_cols_no_snp) {
	          return unique_data_values.get(varID).get(index);
	        } else {
	          // For GWAS data the index is the value
	          return (index);
	        }
	      }

	    public int getNumUniqueDataValues(int varID) {
	        // Use permuted data for corrected impurity importance
	        if (varID >= num_cols) {
	          varID = getUnpermutedVarID(varID);
	        }

	        if (varID < num_cols_no_snp) {
	          return unique_data_values.get(varID).size();
	        } else {
	          // For GWAS data 0,1,2
	          return (3);
	        }
	      }
	    
	    public Vector<String> getVariableNames() {
	    	return variable_names;
	    }
	    
	    public int getNumCols() {
	    	return num_cols;
	    }
	    
	    public int getNumRows() {
	    	return num_rows;
	    }
	    
	    public int getMaxNumUniqueValues() {
	        if (snp_data == null || max_num_unique_values > 3) {
	          // If no snp data or one variable with more than 3 unique values, return that value
	          return max_num_unique_values;
	        } else {
	          // If snp data and no variable with more than 3 unique values, return 3
	          return 3;
	        }
	    }

	    public Vector<Integer> getNoSplitVariables() {
	        return no_split_variables;
	    }
	    
	    public void addNoSplitVariable(int varID) {
	        no_split_variables.add(varID);
	        no_split_variables.sort(null);
	    }
	    
	    public Vector<Boolean> getIsOrderedVariable() {
	        return is_ordered_variable;
	    }

	    // Original name setIsOrderedVariable
	    public void setIsOrderedVariableString(Vector<String> unordered_variable_names) {
	        if (is_ordered_variable.size() > num_cols) {
	        	for (i = is_ordered_variable.size() - 1; i >= num_cols; i++) {
	        		is_ordered_variable.remove(i);
	        	}
	        }
	        else if (is_ordered_variable.size() < num_cols) {
	        	for (i = is_ordered_variable.size(); i < num_cols; i++) {
	        		is_ordered_variable.add(i, true);
	        	}
	        }
	        for (i = 0; i < unordered_variable_names.size(); i++) {
	          int varID = getVariableID(unordered_variable_names.get(i));
	          is_ordered_variable.add(varID, false);
	        }
	    }
	    
	    public void setIsOrderedVariable(Vector<Boolean> is_ordered_variable) {
	        this.is_ordered_variable = is_ordered_variable;
	    }

	    public boolean isOrderedVariable(int varID) {
	        // Use permuted data for corrected impurity importance
	        if (varID >= num_cols) {
	          varID = getUnpermutedVarID(varID);
	        }
	        return is_ordered_variable.get(varID);
	    }
	    
	    public void permuteSampleIDs(Random random) {
	        permuted_sampleIDs.clear();
	        for (i = 0; i < num_rows; i++) {
	        	permuted_sampleIDs.add(i);
	        }
	        shuffle(permuted_sampleIDs, random);
	    }


	}; // private class Data
	
	private class DataDouble extends Data {
	    private double data[] = null;
	    
	    public DataDouble() {
	    	super();
	    }
	    
	    public DataDouble(double data[], Vector<String> variable_names, int num_rows,
	    		int num_cols) {
	    	super(variable_names, num_rows, num_cols);
	    	this.data = data;
	    }
	    
	    public void dispose() {
	    	if (!externalData) {
	    		data = null;
	    	}
	    }
	    
	    public void reserveMemory() {
	    	data = new double[num_cols * num_rows];
	    }
	    
	    public void set(int col, int row, double value, boolean error[]) {
	        data[col * num_rows + row] = value;
	    }
	    
	    public double get(int row, int col) {
	        // Use permuted data for corrected impurity importance
	        if (col >= num_cols) {
	          col = getUnpermutedVarID(col);
	          row = getPermutedSampleID(row);
	        }

	        if (col < num_cols_no_snp) {
	          return data[col * num_rows + row];
	        } else {
	          // Get data out of snp storage. -1 because of GenABEL coding.
	          int idx = (col - num_cols_no_snp) * num_rows_rounded + row;
	          double result = (((snp_data[idx / 4] & mask[idx % 4]) >> offset[idx % 4]) - 1);
	          return result;
	        }
	      }

	} // private class DataDouble extends Data
	
	private class DataFloat extends Data {
	    private float data[] = null;
	    
	    public DataFloat() {
	    	super();
	    }
	    
	    public DataFloat(double data_double[], Vector<String> variable_names, int num_rows,
	    		int num_cols) {
	    	super(variable_names, num_rows, num_cols);
	    	reserveMemory();
	    	for (int i = 0; i < num_cols; i++) {
	    		for (int j = 0; j < num_rows; j++) {
	    			data[i * num_rows + j] = (float) data_double[i * num_rows + j];
	    		}
	    	}
	    }
	    
	    public void dispose() {
	    	if (!externalData) {
	    		data = null;
	    	}
	    }
	    
	    public void reserveMemory() {
	    	data = new float[num_cols * num_rows];
	    }
	    
	    public void set(int col, int row, double value, boolean error[]) {
	        data[col * num_rows + row] = (float)value;
	    }
	    
	    public double get(int row, int col) {
	        // Use permuted data for corrected impurity importance
	        if (col >= num_cols) {
	          col = getUnpermutedVarID(col);
	          row = getPermutedSampleID(row);
	        }

	        if (col < num_cols_no_snp) {
	          return data[col * num_rows + row];
	        } else {
	          // Get data out of snp storage. -1 because of GenABEL coding.
	          int idx = (col - num_cols_no_snp) * num_rows_rounded + row;
	          double result = (((snp_data[idx / 4] & mask[idx % 4]) >> offset[idx % 4]) - 1);
	          return result;
	        }
	      }

	} // private class DataFloat extends Data
	
	private class DataChar extends Data {
	    private char data[] = null;
	    
	    public DataChar() {
	    	super();
	    }
	    
	    public DataChar(double data_double[], Vector<String> variable_names, int num_rows,
	    		int num_cols, boolean error[]) {
	    	super(variable_names, num_rows, num_cols);
	    	reserveMemory();
	    	
	    	// Save data and report errors
	    	for (int i = 0; i < num_cols; i++) {
	    		for (int j = 0; j < num_rows; j++) {
	    			double value = data_double[i * num_rows + j];
	    			if ((value > Character.MAX_VALUE) || (value < Character.MIN_VALUE)) {
	    				error[0] = true;
	    			}
	    			if (Math.floor(value) != Math.ceil(value)) {
	    				error[0] = true;
	    			}
	    			data[i * num_rows + j] = (char)value;
	    		}
	    	}
	    }
	    
	    public void dispose() {
	    	if (!externalData) {
	    		data = null;
	    	}
	    }
	    
	    public void reserveMemory() {
	    	data = new char[num_cols * num_rows];
	    }
	    
	    public void set(int col, int row, double value, boolean error[]) {
	    	if ((value > Character.MAX_VALUE) || (value < Character.MIN_VALUE)) {
				error[0] = true;
			}
			if (Math.floor(value) != Math.ceil(value)) {
				error[0] = true;
			}
	        data[col * num_rows + row] = (char)value;
	    }
	    
	    public double get(int row, int col) {
	        // Use permuted data for corrected impurity importance
	        if (col >= num_cols) {
	          col = getUnpermutedVarID(col);
	          row = getPermutedSampleID(row);
	        }

	        if (col < num_cols_no_snp) {
	          return data[col * num_rows + row];
	        } else {
	          // Get data out of snp storage. -1 because of GenABEL coding.
	          int idx = (col - num_cols_no_snp) * num_rows_rounded + row;
	          double result = (((snp_data[idx / 4] & mask[idx % 4]) >> offset[idx % 4]) - 1);
	          return result;
	        }
	      }

	} // private class DataFloat extends Data
	
	private class SparseMatrix {
		int num_rows;
		int num_cols;
		int num_nonzero_values;
		// sparse matrix with 3 rows and num_nonzero_values columns, row, column, and value rows
		double sm[][];
		
		public SparseMatrix() {
			super();
		}
		
		public SparseMatrix(int num_rows, int num_cols) {
		    super();
		    this.num_rows = num_rows;
		    this.num_cols = num_cols;
		}
		
		public double coeff(int row, int col) {
			for (int i = 0; i < num_nonzero_values; i++) {
				if ((sm[0][i] == row) && (sm[1][i] == col)) {
					return sm[2][i];
				}
			}
			return Double.NaN;
		}
		
		// coeffRef assumes the row and column value already exist.
		// If they do not, use insert
		public void insert(int row, int col, double value) {
			int i, j;
			for (i = 0; i < num_nonzero_values; i++) {
				if ((sm[0][i] == row) && (sm[1][i] == col)) {
					sm[2][i] = value;
					return;
				}
			}
			double smtemp[][] = new double[3][num_nonzero_values];
			for (i = 0; i < 3; i++) {
				for (j = 0; j <num_nonzero_values; j++) {
					smtemp[i][j] = sm[i][j];
				}
			}
			sm[0] = null;
			sm[1] = null;
			sm[2] = null;
			sm = null;
			num_nonzero_values = num_nonzero_values + 1;
			sm = new double[3][num_nonzero_values];
			for (i = 0; i < 3; i++) {
			    for (j = 0; j < num_nonzero_values-1; j++) {
			    	sm[i][j] = smtemp[i][j];
			    }
			}
			sm[0][num_nonzero_values-1] = row;
			sm[1][num_nonzero_values-1] = col;
			sm[2][num_nonzero_values-1] = value;
			smtemp[0] = null;
			smtemp[1] = null;
			smtemp[2] = null;
			smtemp = null;
		}
	}
	
	private class DataSparse extends Data {
		private SparseMatrix data = null;
		
		public DataSparse() {
			super();
		}
		
		public DataSparse(SparseMatrix data, Vector<String> variable_names, int num_rows,
				int num_cols) {
			super(variable_names, num_rows, num_cols);
			this.data = data;
		}
		
		public void dispose() {
		    if (!externalData) {
		        data = null;
		    }
		}
		
		public double get(int row, int col) {
			return data.coeff(row, col);
		}
		
		public void reserveMemory() {
			data = new SparseMatrix(num_rows, num_cols);
		}
		
		public void set(int col, int row, double value, boolean error[]) {
			// coeffRef assumes that the (row, col) position already exists
			// Otherwise use insert
			//data.coeffRef(row,col,value);
			data.insert(row,col,value);
		}
	} // private class DataSparse
	
	private Vector<Integer >order(Vector<Double> x, boolean decreasing) {
		int i;
		int n = x.size();
		ArrayList<indexValueItem> ivList = new ArrayList<indexValueItem>();
		  for (i = 0; i < n; i++) {
		      ivList.add(new indexValueItem(i, x.get(i))); 
		  }
		  if (decreasing) {
			  Collections.sort(ivList, new indexValueDescendingComparator());
		  }
		  else {
			  Collections.sort(ivList, new indexValueComparator());
		  }
		  Vector<Integer> indices = new Vector<Integer>();
		  for (i = 0; i < n; i++) {
		      indices.add(ivList.get(i).getIndex());  
		  }
          return indices;
	}
	
	/**
	 * Sample ranks starting from 1. Ties are given the average rank.
	 * @param values Values to rank
	 * @return Ranks of input values
	 */
	Vector<Double> rank(Vector<Double> values) {
	  int num_values = values.size();

	// Order
	  Vector<Integer> indices = order(values, false);

	// Compute ranks, start at 1
	  Vector<Double> ranks = new Vector<Double>();
	  ranks.setSize(num_values);
	  int reps = 1;
	  for (int i = 0; i < num_values; i += reps) {

	    // Find number of replications
	    reps = 1;
	    while (i + reps < num_values && values.get(indices.get(i)).equals(values.get(indices.get(i + reps)))) {
	      ++reps;
	    }

	    // Assign rank to all replications
	    for (int j = 0; j < reps; ++j)
	      ranks.set(indices.get(i + j),  (2 * (double) i + (double) reps - 1) / 2 + 1);
	  }

	  return ranks;
	}

	
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
	
	private void shuffle(Vector<Integer> v, Random random)
	{
	    int index, temp;
	    for (int i = v.size() - 1; i > 0; i--)
	    {
	        index = random.nextInt(i + 1);
	        temp = v.get(index);
	        v.set(index,v.get(i));
	        v.set(i,temp);
	    }
	}
	
	private void equalSplit(Vector<Integer> result, int start, int end, int num_parts) {
		
		// Return range if only 1 part
		if (num_parts == 1) {
			result.add(start);
			result.add(end+1);
			return;
		}
		
		// Return vector from start to end+1 if more parts than elements
		if (num_parts > end - start + 1) {
			for (int i = start; i <= end + 1; i++) {
				result.add(i);
			}
			return;
		}
		
		int length = (end - start + 1);
	    int part_length_short = length / num_parts;
		int part_length_long = (int) Math.ceil(length / ((double) num_parts));
		int cut_pos = length % num_parts;

		// Add long ranges
		for (int i = start; i < start + cut_pos * part_length_long; i = i + part_length_long) {
		    result.add(i);
		}

		// Add short ranges
		for (int i = start + cut_pos * part_length_long; i <= end + 1; i = i + part_length_short) {
		    result.add(i);
		}

	} // private void equalSplit
	
	void loadDoubleVectorFromFile(Vector<Double> result, String filename) { // #nocov start
      String line;
      int i;
	  // Open input file
	  File file = new File(filename);
  	  BufferedReader input_file;
  	  try {
  	      input_file = new BufferedReader(new FileReader(file));
  	  }
  	  catch (FileNotFoundException e) {
  		  MipavUtil.displayError("Could not find file " + filename);
  		  return;
  	  }

	  // Read the first line, ignore the rest
  	  try {
           line = input_file.readLine();
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on readLine from input_file");
		  return;
	  }
  	  try {
  		  input_file.close();
  	  }
  	  catch (IOException e) {
  		  MipavUtil.displayError("IO exception on close from input_file");
  		  return;
  	  }
	  if (line == null) {
		  return;
	  }
	  String tokens[];
	  tokens = line.split(" ");
	  for (i = 0; i < tokens.length; i++) {
		  double dValue = Double.valueOf(tokens[i]).doubleValue();
		  result.add(dValue);
	  }
	} // #nocov end
	
	private void drawWithoutReplacementSkip(Vector<Integer> result, Random random, int max,
	    Vector<Integer> skip, int num_samples) {
	  if (num_samples < max / 10) {
	    drawWithoutReplacementSimple(result, random, max, skip, num_samples);
	  } else {
	    //drawWithoutReplacementKnuth(result, random, max, skip, num_samples);
	    drawWithoutReplacementFisherYates(result, random, max, skip, num_samples);
	  }
	}

	
	private void drawWithoutReplacementSimple(Vector<Integer> result, Random random, int max,
		    Vector<Integer> skip, int num_samples) {
      int i, j;
      result.ensureCapacity(num_samples);
	  // Set all to not selected
	  Vector<Boolean> temp = new Vector<Boolean>();
	  for (i = 0; i < max; i++) {
	      temp.add(i, false);
	  }

	  for (i = 0; i < num_samples; ++i) {
	    int draw;
	    do {
	      draw = random.nextInt(max - skip.size());
	      for (j = 0; j < skip.size(); j++) {
	        if (draw >= skip.get(j)) {
	          ++draw;
	        }
	      }
	    } while (temp.get(draw));
	    temp.set(draw,true);
	    result.add(draw);
	  }
	}

	private void drawWithoutReplacementFisherYates(Vector<Integer> result, Random random,
	    int max, Vector<Integer> skip, int num_samples) {
      int i, j;
      int temp;
	  // Create indices
	  result.clear();
	  for (i = 0; i < max; i++) {
		  result.add(i);
	  }

	  // Skip indices
	  for (i = 0; i < skip.size(); ++i) {
	    result.removeElementAt((skip.get(skip.size() - 1 - i)).intValue());
	  }

	  // Draw without replacement using Fisher Yates algorithm
	  for (i = 0; i < num_samples; ++i) {
	    j = (int)(i + random.nextDouble() * (max - skip.size() - i));
	    temp = result.get(i);
	    result.set(i,result.get(j));
	    result.set(j,temp);
	  }

	  result.setSize(num_samples);
	}
	
	private void drawWithoutReplacementWeighted(Vector<Integer> result, Random random, Vector<Integer> indices,
			int num_samples, Vector<Double> weights) {
		int i, j;
		int draw = 0;
		double sum_of_weight = 0;
		double rand;
		
		result.ensureCapacity(num_samples);
		// Set all to not selected
		Vector<Boolean> temp = new Vector<Boolean>();
		for (i = 0; i < indices.size(); i++) {
			temp.add(false);
		}
		
		for (i = 0; i < weights.size(); i++) {
			sum_of_weight = sum_of_weight + weights.get(i);
		}
		
		for (i = 0; i < num_samples; i++) {
		    do {
		        rand = sum_of_weight * random.nextDouble();
		        for (j = 0; j < weights.size(); j++) {
		            if (rand < weights.get(j)) {
		            	draw = j;
		            	break;
		            }
		            else {
		            	rand = rand - weights.get(j);
		            }
		        }
		    } while(temp.get(draw));
		    temp.set(draw, true);
		    result.add(indices.get(draw));
		}
	}
	
	private void drawWithoutReplacementWeighted(Vector<Integer> result, Random random, int max_index, 
			int num_samples, Vector<Double> weights) {
		int i, j;
		int draw = 0;
		double sum_of_weight = 0;
		double rand;
		
		result.ensureCapacity(num_samples);
		// Set all to not selected
		Vector<Boolean> temp = new Vector<Boolean>();
		for (i = 0; i < max_index+1; i++) {
			temp.add(false);
		}
		
		for (i = 0; i < weights.size(); i++) {
			sum_of_weight = sum_of_weight + weights.get(i);
		}
		
		for (i = 0; i < num_samples; i++) {
		    do {
		        rand = sum_of_weight * random.nextDouble();
		        for (j = 0; j < weights.size(); j++) {
		            if (rand < weights.get(j)) {
		            	draw = j;
		            	break;
		            }
		            else {
		            	rand = rand - weights.get(j);
		            }
		        }
		    } while(temp.get(draw));
		    temp.set(draw, true);
		    result.add(draw);
		}	
	}
	
	/**
	 * Returns the most frequent class index of a vector with counts for the classes. Returns a random class if counts are equal.
	 * @param class_count Vector with class counts
	 * @param random_number_generator Random number generator
	 * @return Most frequent class index. Out of range index if all 0.
	 */
	private int mostFrequentClass(Vector<Integer> class_count, Random random) {
	  Vector<Integer> major_classes = new Vector<Integer>();

	// Find maximum count
	  int max_count = 0;
	  for (int i = 0; i < class_count.size(); ++i) {
	    int count = class_count.get(i);
	    if (count > max_count) {
	      max_count = count;
	      major_classes.clear();
	      major_classes.add(i);
	    } else if (count == max_count) {
	      major_classes.add(i);
	    }
	  }

	  if (max_count == 0) {
	    return class_count.size();
	  } else if (major_classes.size() == 1) {
	    return major_classes.get(0);
	  } else {
	    // Choose randomly
	    return major_classes.get(random.nextInt(major_classes.size()));
	  }
	}
	
	/**
	 * Returns the most frequent class index of a vector with counts for the classes. Returns a random class if counts are equal.
	 * @param class_count Vector with class counts
	 * @param random_number_generator Random number generator
	 * @return Most frequent class index. Out of range index if all 0.
	 */
	private int mostFrequentDClass(Vector<Double> class_count, Random random) {
	  Vector<Integer> major_classes = new Vector<Integer>();

	// Find maximum count
	  double max_count = 0;
	  for (int i = 0; i < class_count.size(); ++i) {
	    double count = class_count.get(i);
	    if (count > max_count) {
	      max_count = count;
	      major_classes.clear();
	      major_classes.add(i);
	    } else if (count == max_count) {
	      major_classes.add(i);
	    }
	  }

	  if (max_count == 0) {
	    return class_count.size();
	  } else if (major_classes.size() == 1) {
	    return major_classes.get(0);
	  } else {
	    // Choose randomly
	    return major_classes.get(random.nextInt(major_classes.size()));
	  }
	}

	
	private double mostFrequentValue(HashMap<Double, Integer> class_count, Random random) {
		int i;
		int value;
		double key;
		int select;
		Vector<Double> major_classes = new Vector<Double>();
		// Find maximum count;
		int max_count = 0;
		Object keys[] = class_count.keySet().toArray();
		Object values[] = class_count.values().toArray();
		for (i = 0; i < class_count.size(); i++) {
			if (values[i] == null) {
				value = 0;
			}
			else {
		        value = (int)values[i];	
			}
			if (keys[i] == null) {
				key = Double.NaN;
			}
			else {
		        key = (double)keys[i];
			}
		    if (value > max_count) {
		    	max_count = value;
		    	major_classes.clear();
		    	if (!Double.isNaN(key)) {
		    	    major_classes.add(key);
		    	}
		    }
		    else if (value == max_count) {
		    	if (!Double.isNaN(key)) {
		    	    major_classes.add(key);
		    	}
		    }
		}
		
		if (major_classes.size() == 1) {
			return major_classes.get(0);
		}
		else {
			// Choose randomly
			select = random.nextInt(major_classes.size());
			return major_classes.get(select);
		}
	}
	
	private double computeConcordanceIndex(Data data, Vector<Double> sum_chf, 
			int dependent_varID, int status_varID, Vector<Integer> sample_IDs) {
		int i, j;
	    // Compute concordance index
		double concordance = 0.0;
		double permissible = 0.0;
		for (i = 0; i < sum_chf.size(); i++) {
			int sample_i = i;
			if (!sample_IDs.isEmpty()) {
				sample_i = sample_IDs.get(i);
			}
			double time_i = data.get(sample_i, dependent_varID);
			double status_i = data.get(sample_i,  status_varID);
		
			for ( j = i + 1; j < sum_chf.size(); ++j) {
			      int sample_j = j;
			      if (!sample_IDs.isEmpty()) {
			        sample_j = sample_IDs.get(j);
			      }
			      double time_j = data.get(sample_j, dependent_varID);
			      double status_j = data.get(sample_j, status_varID);
	
			      if (time_i < time_j && status_i == 0) {
			        continue;
			      }
			      if (time_j < time_i && status_j == 0) {
			        continue;
			      }
			      if (time_i == time_j && status_i == status_j) {
			        continue;
			      }
	
			      permissible += 1;
	
			      if (time_i < time_j && sum_chf.get(i) > sum_chf.get(j)) {
			        concordance += 1;
			      } else if (time_j < time_i && sum_chf.get(j) > sum_chf.get(i)) {
			        concordance += 1;
			      } else if (sum_chf.get(i) == sum_chf.get(j)) {
			        concordance += 0.5;
			      }
	
			    }
		  }

		  return (concordance / permissible);

	}
	
	private String uintToString(int number) {
		return String.valueOf(number);
	}
	
	private String beautifyTime(int seconds) { // #nocov start
		  String result;

		  // Add seconds, minutes, hours, days if larger than zero
		  int out_seconds = seconds % 60;
		  result = uintToString(out_seconds) + " seconds";
		  int out_minutes = (seconds / 60) % 60;
		  if (seconds / 60 == 0) {
		    return result;
		  } else if (out_minutes == 1) {
		    result = "1 minute, " + result;
		  } else {
		    result = uintToString(out_minutes) + " minutes, " + result;
		  }
		  int out_hours = (seconds / 3600) % 24;
		  if (seconds / 3600 == 0) {
		    return result;
		  } else if (out_hours == 1) {
		    result = "1 hour, " + result;
		  } else {
		    result = uintToString(out_hours) + " hours, " + result;
		  }
		  int out_days = (seconds / 86400);
		  if (out_days == 0) {
		    return result;
		  } else if (out_days == 1) {
		    result = "1 day, " + result;
		  } else {
		    result = uintToString(out_days) + " days, " + result;
		  }
		  return result;
    } // #nocov end
	
	private void splitString(Vector<String> result, String input, String split_string) { // #nocov start
        int i;
		if (input == null) {
			return;
		}
		String tokens[];
        tokens = input.split(split_string);
        for (i = 0; i < tokens.length; i++) {
        	result.add(tokens[i]);
        }
    } // #nocov end

	private void shuffleAndSplit(Vector<Integer> first_part, Vector<Integer> second_part, 
			int n_all, int n_first, Random random) {
          int i;
		  // Reserve space
		  first_part.setSize(n_all);

		  // Fill with 0..n_all-1 and shuffle
		  
		  for (i = 0; i < n_all; i++) {
			  first_part.set(i, i);
		  }
		  shuffle(first_part, random);

		  // Copy to second part
		  second_part.setSize(n_all - n_first);
		  for (i = n_first; i < first_part.size(); i++) {
			  second_part.set(i - n_first, first_part.get(i));
		  }

		  // Resize first part
		  first_part.setSize(n_first);
	}
	
	private void shuffleAndSplitAppend(Vector<Integer> first_part, Vector<Integer> second_part, 
			int n_all, int n_first, Vector<Integer> mapping, Random random) {
		  int i, j;
		  // Old end is start position for new data
		  int first_old_size = first_part.size();
		  int second_old_size = second_part.size();

		  // Reserve space
		  first_part.setSize(first_old_size + n_all);
		  int first_start_pos = first_old_size;

		  // Fill with 0..n_all-1 and shuffle
		  Vector<Integer>fp2 = new Vector<Integer>();
		  for (i = first_start_pos; i < first_part.size(); i++) {
			  fp2.add(i - first_start_pos);
		  }
		  shuffle(fp2, random);
		  for (i = first_start_pos; i < first_part.size(); i++) {
			  first_part.set(i, fp2.get(i-first_start_pos));
		  }

		  // Mapping
		  for (j = first_start_pos; j != first_part.size(); ++j) {
		    first_part.set(j, mapping.get(first_part.get(j)));
		  }

		  // Copy to second part
		  second_part.setSize(second_part.size() + n_all - n_first);
		  int second_start_pos = second_old_size;
		  for (i = first_start_pos + n_first; i < first_part.size(); i++) {
			  second_part.set(i - first_start_pos - n_first + second_start_pos, 
					  first_part.get(i));
		  }

		  // Resize first part
		  first_part.setSize(first_old_size + n_first);
    }

	private String checkUnorderedVariables(Data data, Vector<String> unordered_variable_names) { // #nocov start
		int i;  
		int num_rows = data.getNumRows();
		  Vector<Integer> sampleIDs = new Vector<Integer>();
		  for (i = 0; i < num_rows; i++) {
			  sampleIDs.add(i);
		  }

		  // Check for all unordered variables
		  for (i = 0; i < unordered_variable_names.size(); i++) {
			String variable_name = unordered_variable_names.get(i);
		    int varID = data.getVariableID(variable_name);
		    Vector<Double> all_values = new Vector<Double>();
		    data.getAllValues(all_values, sampleIDs, varID);

		    // Check level count
		    int max_level_count = 8 * 4 - 1;
		    if (all_values.size() > max_level_count) {
		      return "Too many levels in unordered categorical variable " + variable_name + ". Only "
		          + uintToString(max_level_count) + " levels allowed on this system.";
		    }

		    // Check positive integers
		    if (!checkPositiveIntegers(all_values)) {
		      return "Not all values in unordered categorical variable " + variable_name + " are positive integers.";
		    }
		  }
		  return "";
    } // #nocov end
	
	private boolean checkPositiveIntegers(Vector<Double> all_values) { // #nocov start
		  int i;
		  for (i = 0; i < all_values.size(); i++) {
			double value = all_values.get(i);
		    if (value < 1 || !(Math.floor(value) == value)) {
		      return false;
		    }
		  }
		  return true;
    } // #nocov end
	
	private double maxstatPValueLau92(double b, double minprop, double maxprop) {

		  if (b < 1) {
		    return 1.0;
		  }

		  // Compute only once (minprop/maxprop don't change during runtime)
		  logprop = Math.log((maxprop * (1 - minprop)) / ((1 - maxprop) * minprop));

		  double db = dstdnorm(b);
		  double p = 4 * db / b + db * (b - 1 / b) * logprop;

		  if (p > 0) {
		    return p;
		  } else {
		    return 0;
		  }
    }

	
	private double maxstatPValueLau94(double b, double minprop, double maxprop, int N, Vector<Integer> m) {

		  double D = 0;
		  for (int i = 0; i < m.size() - 1; ++i) {
		    double m1 = m.get(i);
		    double m2 = m.get(i + 1);

		    double t = Math.sqrt(1.0 - m1 * (N - m2) / ((N - m1) * m2));
		    D += 1 / Math.PI * Math.exp(-b * b / 2) * (t - (b * b / 4 - 1) * (t * t * t) / 6);
		  }

		  return 2 * (1 - pstdnorm(b)) + D;
    }
	
	private double maxstatPValueUnadjusted(double b) {
	  return 2 * pstdnorm(-b);
	}

	private double dstdnorm(double x) {
	  return Math.exp(-0.5 * x * x) / Math.sqrt(2 * Math.PI);
	}

	private double pstdnorm(double x) {
	  return 0.5 * (1 + erf(x / Math.sqrt(2.0)));
	}
	
	
	// Error function erf(x) from Computation of Special Functions by 
	// Shanjie Zhang and Jianming Jin. pp. 622-623.
	private double erf(double x) {
		double eps = 1.0E-15;
		double x2, er, r, c0, err;
		int k;
		x2 = x * x;
		if (Math.abs(x) < 3.5) {
			er = 1.0;
			r = 1.0;
			for (k = 1; k <= 50; k++) {
				r = r*x2/(k + 0.5);
				er = er + r;
				if (Math.abs(r) <= Math.abs(er)*eps) {
					break;
				}
			} // for (k = 1; k <= 50; k++)
			c0 = 2.0/Math.sqrt(Math.PI) * x * Math.exp(-x2);
			err = c0 * er;
		} // if (Math.abs(x) < 3.5)
		else {
			er = 1.0;
			r = 1.0;
			for (k = 1; k <= 12; k++) {
				r = -r*(k - 0.5)/x2;
				er = er + r;
			}
			c0 = Math.exp(-x2)/(Math.abs(x) * Math.sqrt(Math.PI));
			err = 1.0 - c0 * er;
			if (x < 0.0) err = -err;
		} // else
		return err;
	}

	private Vector<Double> adjustPvalues(Vector<Double> unadjusted_pvalues) {
		  int i;
		  int idx, idx_last;
		  int num_pvalues = unadjusted_pvalues.size();
		  Vector<Double>adjusted_pvalues = new Vector<Double>();
		  for (i = 0; i < num_pvalues; i++) {
			  adjusted_pvalues.add(0.0);
		  }

		  // Get order of p-values
		  ArrayList<indexValueItem> ivList = new ArrayList<indexValueItem>();
		  for (i = 0; i < unadjusted_pvalues.size(); i++) {
		      ivList.add(new indexValueItem(i, unadjusted_pvalues.get(i))); 
		  }
		  Collections.sort(ivList, new indexValueDescendingComparator());
		  Vector<Integer> indices = new Vector<Integer>();
		  for (i = 0; i < unadjusted_pvalues.size(); i++) {
		      indices.add(ivList.get(i).getIndex());  
		  }

		  // Compute adjusted p-values
		  adjusted_pvalues.set(indices.get(0),unadjusted_pvalues.get(indices.get(0)));
		  for (i = 1; i < indices.size(); ++i) {
		    idx = indices.get(i);
		    idx_last = indices.get(i - 1);

		    adjusted_pvalues.set(idx,Math.min(adjusted_pvalues.get(idx_last),
		        (double) num_pvalues / (double) (num_pvalues - i) * unadjusted_pvalues.get(idx)));
		  }
		  return adjusted_pvalues;
    }
	
	private class indexValueComparator implements Comparator<indexValueItem> {
		 // Sort in ascending order
		 public int compare(final indexValueItem o1, final indexValueItem o2) {
	            final double a = o1.getValue();
	            final double b = o2.getValue();

	            if (a < b) {
	                return -1;
	            } else if (a > b) {
	                return 1;
	            } else {
	                return 0;
	            }
	        }	
	}
	
	private class indexValueDescendingComparator implements Comparator<indexValueItem> {
		 // Sort in descending order
		 public int compare(final indexValueItem o1, final indexValueItem o2) {
	            final double a = o1.getValue();
	            final double b = o2.getValue();

	            if (a < b) {
	                return 1;
	            } else if (a > b) {
	                return -1;
	            } else {
	                return 0;
	            }
	        }	
	}
	
	private class indexValueItem {

        /** DOCUMENT ME! */
        private final int index;

        /** DOCUMENT ME! */
        private final double value;

        /**
         * Creates a new indexValueItem object.
         * 
         * @param index
         * @param value
         */
        public indexValueItem(final int index, final double value) {
            this.index = index;
            this.value = value;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getIndex() {
            return index;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getValue() {
            return value;
        }

    }
	
	
	private Vector<Double> logrankScores(Vector<Double> time, Vector<Double> status) {
		  int i, j;
		  int n = time.size();
		  Vector<Double>scores = new Vector<Double>();
		  scores.setSize(n);

		  // Get order of timepoints
		  ArrayList<indexValueItem> ivList = new ArrayList<indexValueItem>();
		  for (i = 0; i < n; i++) {
		      ivList.add(new indexValueItem(i, time.get(i))); 
		  }
		  Collections.sort(ivList, new indexValueComparator());
		  Vector<Integer> indices = new Vector<Integer>();
		  for (i = 0; i < n; i++) {
		      indices.add(ivList.get(i).getIndex());  
		  }

		  // Compute scores
		  double cumsum = 0;
		  int last_unique = -1;
		  for (i = 0; i < n; ++i) {

		    // Continue if next value is the same
		    if (i < n - 1 && time.get(indices.get(i)).equals(time.get(indices.get(i + 1)))) {
		      continue;
		    }

		    // Compute sum and scores for all non-unique values in a row
		    for (j = last_unique + 1; j <= i; ++j) {
		      cumsum += status.get(indices.get(j)) / (n - i);
		    }
		    for (j = last_unique + 1; j <= i; ++j) {
		      scores.set(indices.get(j),status.get(indices.get(j)) - cumsum);
		    }

		    // Save last computed value
		    last_unique = i;
		  }

		  return scores;
    }

	private void maxstat(Vector<Double> scores, Vector<Double> x, Vector<Integer> indices,
			double best_maxstat[], double best_split_value[], double minprop, double maxprop) {
	  int n = x.size();

	  double sum_all_scores = 0;
	  for (int i = 0; i < n; ++i) {
	    sum_all_scores += scores.get(indices.get(i));
	  }

	  // Compute sum of differences from mean for variance
	  double mean_scores = sum_all_scores / n;
	  double sum_mean_diff = 0;
	  for (int i = 0; i < n; ++i) {
	    sum_mean_diff += (scores.get(i) - mean_scores) * (scores.get(i) - mean_scores);
	  }

	  // Get smallest and largest split to consider, -1 for compatibility with R maxstat
	  int minsplit = 0;
	  if (n * minprop > 1) {
	    minsplit = (int)(n * minprop - 1);
	  }
	  int maxsplit = (int)(n * maxprop - 1);

	  // For all unique x-values
	  best_maxstat[0] = -1;
	  best_split_value[0] = -1;
	  double sum_scores = 0;
	  int n_left = 0;
	  for (int i = 0; i <= maxsplit; ++i) {

	    sum_scores += scores.get(indices.get(i));
	    n_left++;

	    // Dont consider splits smaller than minsplit for splitting (but count)
	    if (i < minsplit) {
	      continue;
	    }

	    // Consider only unique values
	    if (i < n - 1 && x.get(indices.get(i)).equals(x.get(indices.get(i + 1)))) {
	      continue;
	    }

	    // If value is largest possible value, stop
	    if (x.get(indices.get(i)).equals(x.get(indices.get(n - 1)))) {
	      break;
	    }

	    double S = sum_scores;
	    double E = (double) n_left / (double) n * sum_all_scores;
	    double V = (double) n_left * (double) (n - n_left) / (double) (n * (n - 1)) * sum_mean_diff;
	    double T = Math.abs((S - E) / Math.sqrt(V));

	    if (T > best_maxstat[0]) {
	      best_maxstat[0] = T;

	      // Use mid-point split if possible
	      if (i < n - 1) {
	        best_split_value[0] = (x.get(indices.get(i)) + x.get(indices.get(i + 1))) / 2;
	      } else {
	        best_split_value[0] = x.get(indices.get(i));
	      }
	    }
	  }
	}
	
	private Vector<Integer> numSamplesLeftOfCutpoint(Vector<Double> x, Vector<Integer> indices) {
	  Vector<Integer> num_samples_left = new Vector<Integer>();

	  for (int i = 0; i < x.size(); ++i) {
	    if (i == 0) {
	      num_samples_left.add(1);
	    } else if (x.get(indices.get(i)).equals(x.get(indices.get(i - 1)))) {
	    	int value = num_samples_left.lastElement();
	    	num_samples_left.set(num_samples_left.size() - 1,value+1);
	    } else {
	      int value = num_samples_left.lastElement();
	      num_samples_left.add(value + 1);
	    }
	  }

	  return num_samples_left;
	}
	
	/**
	 * Write a 1d vector to filestream. First the size is written, then all vector elements.
	 * @param vector Vector of type T to save
	 * @param file ofstream object to write to.
	 */
	private void saveVector1D(Vector<Integer> vector, BufferedWriter bw) {
	  // Save length
	  int i;
	  int length = vector.size();
	  try {
	      bw.write(String.valueOf(length)+"\n");
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on bw.write(String.valueOf(length))");
		  return;
	  }
	  for (i = 0; i < length; i++) {
		  try {
		      bw.write(String.valueOf(vector.get(i))+"\n");
		  }
		  catch (IOException e) {
			  MipavUtil.displayError("IO exception on bw.write(String.valueOf(vector.get(i)))");
			  return;
		  }
	  }
	}
	
	/**
	 * Write a 1d vector to filestream. First the size is written, then all vector elements.
	 * @param vector Vector of type T to save
	 * @param file ofstream object to write to.
	 */
	private void saveBVector1D(Vector<Boolean> vector, BufferedWriter bw) {
	  // Save length
	  int i;
	  int length = vector.size();
	  try {
	      bw.write(String.valueOf(length)+"\n");
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on bw.write(String.valueOf(length))");
		  return;
	  }
	  for (i = 0; i < length; i++) {
		  try {
		      bw.write(String.valueOf(vector.get(i))+"\n");
		  }
		  catch (IOException e) {
			  MipavUtil.displayError("IO exception on bw.write(String.valueOf(vector.get(i)))");
			  return;
		  }
	  }
	}
	
	/**
	 * Read a 1d vector written by saveVector1D() from filestream.
	 * @param result Result vector with elements of type T.
	 * @param file ifstream object to read from.
	 */
	private void readVector1D(Vector<Integer> result, BufferedReader br) {
	  int i;
	  int value;
	  // Read length
	  int length;
	  String line;
	  try {
          line = br.readLine();
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on br.readLine()");
		  return;
	  }
	  length = Integer.valueOf(line).intValue();
	  for (i = 0; i < length; i++) {
		  try {
	          line = br.readLine();
		  }
		  catch (IOException e) {
			  MipavUtil.displayError("IO exception on br.readLine()");
			  return;
		  }
		  value = Integer.valueOf(line).intValue();
		  result.add(value);
	  }
	}
	
	/**
	 * Read a 1d vector written by saveVector1D() from filestream.
	 * @param result Result vector with elements of type T.
	 * @param file ifstream object to read from.
	 */
	private void readBVector1D(Vector<Boolean> result, BufferedReader br) {
	  int i;
	  boolean value;
	  // Read length
	  int length;
	  String line;
	  try {
          line = br.readLine();
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on br.readLine()");
		  return;
	  }
	  length = Integer.valueOf(line).intValue();
	  for (i = 0; i < length; i++) {
		  try {
	          line = br.readLine();
		  }
		  catch (IOException e) {
			  MipavUtil.displayError("IO exception on br.readLine()");
			  return;
		  }
		  value = Boolean.valueOf(line).booleanValue();
		  result.add(value);
	  }
	}

	/**
	 * Write a 1d vector to filestream. First the size is written, then all vector elements.
	 * @param vector Vector of type T to save
	 * @param file ofstream object to write to.
	 */
	private void saveDVector1D(Vector<Double> vector, BufferedWriter bw) {
	  // Save length
	  int i;
	  int length = vector.size();
	  try {
	      bw.write(String.valueOf(length)+"\n");
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on bw.write(String.valueOf(length))");
		  return;
	  }
	  for (i = 0; i < length; i++) {
		  try {
		      bw.write(String.valueOf(vector.get(i))+"\n");
		  }
		  catch (IOException e) {
			  MipavUtil.displayError("IO exception on bw.write(String.valueOf(vector.get(i)))");
			  return;
		  }
	  }
	}
	
	/**
	 * Read a 1d vector written by saveVector1D() from filestream.
	 * @param result Result vector with elements of type T.
	 * @param file ifstream object to read from.
	 */
	private void readDVector1D(Vector<Double> result, BufferedReader br) {
	  int i;
	  double value;
	  // Read length
	  int length;
	  String line;
	  try {
          line = br.readLine();
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on br.readLine()");
		  return;
	  }
	  length = Integer.valueOf(line).intValue();
	  for (i = 0; i < length; i++) {
		  try {
	          line = br.readLine();
		  }
		  catch (IOException e) {
			  MipavUtil.displayError("IO exception on br.readLine()");
			  return;
		  }
		  value = Double.valueOf(line).doubleValue();
		  result.add(value);
	  }
	}
	
	/**
	 * Write a 2d vector to filestream. First the size of the first dim is written as size_t, then for all inner vectors the size and elements.
	 * @param vector Vector of vectors of type T to write to file.
	 * @param file ofstream object to write to.
	 */
	private void saveVector2D(Vector<Vector<Integer>> vector, BufferedWriter bw) {
	  int i;
	  // Save length of first dim
	  int length = vector.size();
	  try {
	      bw.write(String.valueOf(length)+"\n");
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on bw.write(String.valueOf(length))");
		  return;
	  }

	  // Save outer vector
	  Vector<Integer> inner_vector = new Vector<Integer>();
	  for (i = 0; i < vector.size(); i++) {
	    // Save inner vector
		inner_vector = vector.get(i);
	    saveVector1D(inner_vector, bw);
	  }
	}
	
	/**
	 * Read a 2d vector written by saveVector2D() from filestream.
	 * @param result Result vector of vectors with elements of type T.
	 * @param file ifstream object to read from.
	 */
	private void readVector2D(Vector<Vector<Integer>> result, BufferedReader br) {
	  int i;
	  // Read length of first dim
	  int length;
	  String line;
	  try {
          line = br.readLine();
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on br.readLine()");
		  return;
	  }
	  length = Integer.valueOf(line).intValue();
	  for (i = 0; i < length; i++) {
	      result.add(new Vector<Integer>());  
	  }

	  // Read outer vector
	  for (i = 0; i < length; ++i) {
	    // Read inner vector
	    readVector1D(result.get(i), br);
	  }
	}

	/**
	 * Write a 2d vector to filestream. First the size of the first dim is written as size_t, then for all inner vectors the size and elements.
	 * @param vector Vector of vectors of type T to write to file.
	 * @param file ofstream object to write to.
	 */
	private void saveDVector2D(Vector<Vector<Double>> vector, BufferedWriter bw) {
	  int i;
	  // Save length of first dim
	  int length = vector.size();
	  try {
	      bw.write(String.valueOf(length)+"\n");
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on bw.write(String.valueOf(length))");
		  return;
	  }

	  // Save outer vector
	  Vector<Double> inner_vector = new Vector<Double>();
	  for (i = 0; i < vector.size(); i++) {
	    // Save inner vector
		inner_vector = vector.get(i);
	    saveDVector1D(inner_vector, bw);
	  }
	}
	
	/**
	 * Read a 2d vector written by saveVector2D() from filestream.
	 * @param result Result vector of vectors with elements of type T.
	 * @param file ifstream object to read from.
	 */
	private void readDVector2D(Vector<Vector<Double>> result, BufferedReader br) {
	  int i;
	  // Read length of first dim
	  int length;
	  String line;
	  try {
          line = br.readLine();
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IO exception on br.readLine()");
		  return;
	  }
	  length = Integer.valueOf(line).intValue();
	  for (i = 0; i < length; i++) {
	      result.add(new Vector<Double>());  
	  }

	  // Read outer vector
	  for (i = 0; i < length; ++i) {
	    // Read inner vector
	    readDVector1D(result.get(i), br);
	  }
	}
	
	private boolean equalVectorInteger(Vector<Integer> v1, Vector<Integer> v2, String str) {
		// v1 is true answer, v2 is test answer
		int i;
	    if (v1.size() != v2.size()) {
	        System.out.println("In " + str + " correct size is " + v1.size());
	        System.out.println("Test size is " + v2.size());
	        return false;
	    }
	    for (i = 0; i < v1.size(); i++) {
	    	if (!v1.get(i).equals(v2.get(i))) {
	    		System.out.println("In " + str + " correct element " + i + " is " + v1.get(i));
	    		System.out.println("Actual test element at " + i + " is " + v2.get(i));
	    		return false;
	    	}
	    }
	    System.out.println(str + " passed");
	    return true;
	}
	
	private boolean equalVectorVectorInteger(Vector<Vector<Integer>> v1, Vector<Vector<Integer>> v2, String str) {
		// v1 is true answer, v2 is test answer
		int i, j;
		Vector<Integer> p1 = new Vector<Integer>();
		Vector<Integer> p2 = new Vector<Integer>();
	    if (v1.size() != v2.size()) {
	        System.out.println("In " + str + " correct size is " + v1.size());
	        System.out.println("Test size is " + v2.size());
	        return false;
	    }
	    for (i = 0; i < v1.size(); i++) {
	    	if (v1.get(i).size() != v2.get(i).size()) {
	    		System.out.println("In " + str + " v1.get("+i+").size() is " + v1.get(i).size());
	    		System.out.println("Actual test element v2.get("+i+").size() is " + v2.get(i).size());
	    		return false;
	    	}
	    	p1 = v1.get(i);
	    	p2 = v2.get(i);
	    	for (j = 0; j < p1.size(); j++) {
		    	if (!p1.get(j).equals(p2.get(j))) {
		    		System.out.println("In " + str + " correct element " + i + "," + j + " is " + p1.get(j));
		    		System.out.println("Actual test element at " + i + "," + j + " is " + p2.get(j));
		    		return false;
		    	}
		    }
	    }
	    System.out.println(str + " passed");
	    return true;
	}
	
	private boolean equalVectorDouble(Vector<Double> v1, Vector<Double> v2, String str) {
		// v1 is true answer, v2 is test answer
		int i;
	    if (v1.size() != v2.size()) {
	        System.out.println("In " + str + " correct size is " + v1.size());
	        System.out.println("Test size is " + v2.size());
	        return false;
	    }
	    for (i = 0; i < v1.size(); i++) {
	    	if (!v1.get(i).equals(v2.get(i))) {
	    		System.out.println("In " + str + " correct element " + i + " is " + v1.get(i));
	    		System.out.println("Actual test element at " + i + " is " + v2.get(i));
	    		return false;
	    	}
	    }
	    System.out.println(str + " passed");
	    return true;
	}
	
	private boolean equalVectorVectorDouble(Vector<Vector<Double>> v1, Vector<Vector<Double>> v2, String str) {
		// v1 is true answer, v2 is test answer
		int i, j;
		Vector<Double> p1 = new Vector<Double>();
		Vector<Double> p2 = new Vector<Double>();
	    if (v1.size() != v2.size()) {
	        System.out.println("In " + str + " correct size is " + v1.size());
	        System.out.println("Test size is " + v2.size());
	        return false;
	    }
	    for (i = 0; i < v1.size(); i++) {
	    	if (v1.get(i).size() != v2.get(i).size()) {
	    		System.out.println("In " + str + " v1.get("+i+").size() is " + v1.get(i).size());
	    		System.out.println("Actual test element v2.get("+i+").size() is " + v2.get(i).size());
	    		return false;
	    	}
	    	p1 = v1.get(i);
	    	p2 = v2.get(i);
	    	for (j = 0; j < p1.size(); j++) {
		    	if (!p1.get(j).equals(p2.get(j))) {
		    		System.out.println("In " + str + " correct element " + i + "," + j + " is " + p1.get(j));
		    		System.out.println("Actual test element at " + i + "," + j + " is " + p2.get(j));
		    		return false;
		    	}
		    }
	    }
	    System.out.println(str + " passed");
	    return true;
	}
	
	private boolean expectNear(double val1, double val2, double error, String str) {
		if ((val2 < val1 - error) || (val2 > val1 + error)) {
			System.out.println("In " + str + " " + val2 + " is more than " +
		                   error + " away from expected " + val1);
			return false;
		}
		return true;
	}
	
	private abstract class Tree {
		protected Random random;
	    protected int dependent_varID;
	    protected int mtry;
	    
	    // Number of samples (all samples, not only inbag for this tree)
	    protected int num_samples;
	    
	    // Number of OOB samples
	    protected int num_samples_oob;
	    
	    // Minimum node size to split, like in original RF nodes of 
	    // smaller size can be produced
	    protected int min_node_size;
	    
	    // Weight vector for selecting possible split variables, one
	    // weight between 0 (never select) and 1 (always select) for each
	    // variable
	    // Deterministic variables are always selected
	    protected Vector<Integer> deterministic_varIDs;
	    protected Vector<Integer> split_select_varIDs;
	    protected Vector<Double> split_select_weights;
	    
	    // Bootstrap weights
	    protected Vector<Double> case_weights;
	    
	    // Splitting variable for each node
	    protected Vector<Integer> split_varIDs = new Vector<Integer>();
	    
	    // Value to split at for each node, for now only binary split
	    // For terminal nodes the prediction value is saved here
	    protected Vector<Double> split_values = new Vector<Double>();
	    
	    // Vector of left and right child node IDs, 0 for no child
	    protected Vector<Vector<Integer>> child_nodeIDs = new Vector<Vector<Integer>>();
	    
	    // For each node a vector with IDs of samples in node
	    protected Vector<Vector<Integer>> sampleIDs = new Vector<Vector<Integer>>();
	    
	    // IDs of OOB inidividuals, sorted
	    protected Vector<Integer> oob_sampleIDs;
	    
	    // Holdout mode
	    protected boolean holdout;
	    
	    // Inbag counts
	    protected boolean keep_inbag;
	    protected Vector<Integer> inbag_counts = new Vector<Integer>();
	    
	    // Pointer to original data
	    protected Data data;
	    
	    // Variable importance for all variables
	    protected Vector<Double> variable_importance;
	    protected ImportanceMode importance_mode;
	    
	    // When growing here the OOB set is used
	    // Terminal nodeIDs for prediction samples
	    protected Vector<Integer> prediction_terminal_nodeIDs = new Vector<Integer>();
	    
	    protected boolean sample_with_replacement;
	    protected Vector<Double> sample_fraction;
	    
	    protected boolean memory_saving_splitting;
	    protected SplitRule splitrule;
	    protected double alpha;
	    protected double minprop;
	    protected int num_random_splits;
	    
	    public Tree() {
	    	dependent_varID = 0;
	    	mtry = 0;
	    	num_samples = 0;
	    	num_samples_oob = 0;
	    	min_node_size = 0;
	    	deterministic_varIDs = new Vector<Integer>();
	    	split_select_varIDs = new Vector<Integer>();
	    	split_select_weights = new Vector<Double>();
	    	case_weights = new Vector<Double>();
	    	oob_sampleIDs = new Vector<Integer>();
	    	holdout = false;
	    	keep_inbag = false;
	    	data = null;
	    	variable_importance = new Vector<Double>();
	    	importance_mode = DEFAULT_IMPORTANCE_MODE;
	    	sample_with_replacement = true;
	    	sample_fraction = new Vector<Double>();
	    	memory_saving_splitting = false;
	    	splitrule = DEFAULT_SPLITRULE;
	    	alpha = DEFAULT_ALPHA;
	    	minprop = DEFAULT_MINPROP;
	    	num_random_splits = DEFAULT_NUM_RANDOM_SPLITS;
	    }
	    
	    public Tree(Vector<Vector<Integer>> child_nodeIDs, Vector<Integer> split_varIDs, Vector<Double> split_values) {
	    	dependent_varID = 0;
	    	mtry = 0;
	    	num_samples = 0;
	    	num_samples_oob = 0;
	    	min_node_size = 0;
	    	deterministic_varIDs = new Vector<Integer>();
	    	split_select_varIDs = new Vector<Integer>();
	    	split_select_weights = new Vector<Double>();
	    	case_weights = new Vector<Double>();
	    	this.split_varIDs.addAll(split_varIDs);
	    	this.split_values.addAll(split_values);
	    	this.child_nodeIDs.addAll(child_nodeIDs);
	    	oob_sampleIDs = new Vector<Integer>();
	    	holdout = false;
	    	keep_inbag = false;
	    	data = null;
	    	variable_importance = new Vector<Double>();
	    	importance_mode = DEFAULT_IMPORTANCE_MODE;
	    	sample_with_replacement = true;
	    	sample_fraction = new Vector<Double>();
	    	memory_saving_splitting = false;
	    	splitrule = DEFAULT_SPLITRULE;
	    	alpha = DEFAULT_ALPHA;
	    	minprop = DEFAULT_MINPROP;
	    	num_random_splits = DEFAULT_NUM_RANDOM_SPLITS;	
	    }
	    
	    public void dispose() {
	    	
	    }
	    
	    public void init(Data data, int mtry, int dependent_varID, int num_samples, long seed,
	    		Vector<Integer> deterministic_varIDs, Vector<Integer> split_select_varIDs, 
	    		Vector<Double> split_select_weights, ImportanceMode importance_mode, int min_node_size,
	    		boolean sample_with_replacement, boolean memory_saving_splitting, SplitRule splitrule,
	    		Vector<Double> case_weights, boolean keep_inbag, Vector<Double> sample_fraction,
	    		double alpha, double minprop, boolean holdout, int num_random_splits) {
	    	this.data = data;
	    	this.mtry = mtry;
	    	this.dependent_varID = dependent_varID;
	    	this.num_samples = num_samples;
	    	this.memory_saving_splitting = memory_saving_splitting;
	    	
	    	// Create root node, assign bootstrap sample and oob samples
	    	child_nodeIDs.add(new Vector<Integer>());
	    	child_nodeIDs.add(new Vector<Integer>());
	    	createEmptyNode();
	    	
	    	// Initialize the random number generator and set seed
	    	random = new Random(seed);
	    	
	    	this.deterministic_varIDs = deterministic_varIDs;
	    	this.split_select_varIDs = split_select_varIDs;
	    	this.split_select_weights = split_select_weights;
	    	this.importance_mode = importance_mode;
	    	this.min_node_size = min_node_size;
	    	this.sample_with_replacement = sample_with_replacement;
	    	this.splitrule = splitrule;
	    	this.case_weights = case_weights;
	    	this.keep_inbag = keep_inbag;
	    	this.sample_fraction = sample_fraction;
	    	this.holdout = holdout;
	    	this.alpha = alpha;
	    	this.minprop = minprop;
	    	this.num_random_splits = num_random_splits;
	    }
	    
	    public void grow(Vector<Double> variable_importance) {
	        // Allow memory for tree growing
	    	allocateMemory();
	    	this.variable_importance = variable_importance;
	    	
	    	// Bootstrap, dependent if weighted or not and with or without replacement
	    	if (!case_weights.isEmpty()) {
	    		if (sample_with_replacement) {
	    			bootstrapWeighted();
	    		}
	    		else {
	    			bootstrapWithoutReplacementWeighted();
	    		}
	    	}
	    	else if (sample_fraction.size() > 1) {
	    		if (sample_with_replacement) {
	    			bootstrapClassWise();
	    		}
	    		else {
	    			bootstrapWithoutReplacementClassWise();
	    		}
	    	}
	    	else {
	    		if (sample_with_replacement) {
	    			bootstrap();
	    		}
	    		else {
	    			bootstrapWithoutReplacement();
	    		}
	    	}
	    	
	    	// While not all nodes terminal, split next node
	    	int num_open_nodes = 1;
	    	int i = 0;
	    	while (num_open_nodes > 0) {
	    		boolean is_terminal_node = splitNode(i);
	    		if (is_terminal_node) {
	    			--num_open_nodes;
	    		}
	    		else {
	    			++num_open_nodes;
	    		}
	    		++i;
	    	}
	    	
	    	// Delete sampleID vector to save memory
	    	sampleIDs.clear();
	    	cleanUpInternal();	
	    }
	    
	    public void predict(Data prediction_data, boolean oob_prediction) {
	    	int i;
	    	int num_samples_predict;
	    	if (oob_prediction) {
	    		num_samples_predict = num_samples_oob;
	    	}
	    	else {
	    		num_samples_predict = prediction_data.getNumRows();
	    	}
	    	
	    	int insize = prediction_terminal_nodeIDs.size();
	    	prediction_terminal_nodeIDs.setSize(num_samples_predict);
	    	for (i = insize; i < num_samples_predict; i++) {
	    		prediction_terminal_nodeIDs.set(i, 0);
	    	}
	    	
	    	// For each sample start in root, drop down the tree and return final value
	    	for (i = 0; i < num_samples_predict; ++i) {
	    		int sample_idx;
	    		if (oob_prediction) {
	    			sample_idx = oob_sampleIDs.get(i);
	    		}
	    		else {
	    			sample_idx = i;
	    		}
	    		int nodeID = 0;
	    		while (true) {
	    			
	    			// Break if terminal node
	    			if (child_nodeIDs.get(0).get(nodeID) == 0 && child_nodeIDs.get(1).get(nodeID) == 0) {
	    				break;
	    			}
	    			
	    			// Move to child
	    			int split_varID = split_varIDs.get(nodeID);
	    			
	    			double value = prediction_data.get(sample_idx, split_varID);
	    			if (prediction_data.isOrderedVariable(split_varID)) {
	    				if (value <= split_values.get(nodeID)) {
	    					// Move to left child
	    					nodeID = child_nodeIDs.get(0).get(nodeID);
	    				}
	    				else {
	    					// Move to right child
	    					nodeID = child_nodeIDs.get(1).get(nodeID);
	    				}
	    			}
	    			else {
	    				int factorID = (int)Math.floor(value) - 1;
	    				int splitID = (int)Math.floor(split_values.get(nodeID));
	    				
	    				// Left if 0 found at position factorID
	    				if ((splitID & (1 << factorID)) == 0) {
	    					// Move to left child
	    					nodeID = child_nodeIDs.get(0).get(nodeID);
	    				}
	    				else {
	    					// Move to right child
	    					nodeID = child_nodeIDs.get(1).get(nodeID);
	    				}
	    			}
	    		}
	    		
	    		prediction_terminal_nodeIDs.set(i, nodeID);
	    	}
	    }
	    
	    public void computePermutationImportance(Vector<Double> forest_importance,
	    		Vector<Double> forest_variance) {
	    	int i,j;
	    	int num_independent_variables = data.getNumCols() - data.getNoSplitVariables().size();
	    	
	    	// Compute normal prediction accuracy for each tree.  Predictions already computed.
	    	double accuracy_normal = computePredictionAccuracyInternal();
	    	
	    	prediction_terminal_nodeIDs.clear();
	    	prediction_terminal_nodeIDs.setSize(num_samples_oob);
	    	for (i = 0; i < num_samples_oob; i++) {
	    		prediction_terminal_nodeIDs.set(i, 0);
	    	}
	    	
	    	// Reserve space for permutation, initialize with oob_sampleIDs
	    	Vector<Integer> permutations = new Vector<Integer>();
	    	permutations.addAll(oob_sampleIDs);
	    	
	    	// Randomly permute for all independent variables
	    	for (i = 0; i < num_independent_variables; ++i) {
	    	
	            // Skip no split variables
	    		int varID = i;
	    	    for (j = 0; j < data.getNoSplitVariables().size(); j++) {
	    	    	int skip = data.getNoSplitVariables().get(j);
	    	    	if (varID >= skip) {
	    	    		++varID;
	    	    	}
	    	    }
	    	    
	    	    // Permute and compute prediction accuracy again for this permutation and save difference
	    	    permuteAndPredictOobSamples(varID, permutations);
	    	    double accuracy_permuted = computePredictionAccuracyInternal();
	    	    double accuracy_difference = accuracy_normal - accuracy_permuted;
	    	    double val = forest_importance.get(i);
	    	    forest_importance.set(i, val + accuracy_difference);
	    	    
	    	    // Compute variance
	    	    if (importance_mode == ImportanceMode.IMP_PERM_BREIMAN) {
	    	    	val = forest_variance.get(i);
	    	    	forest_variance.set(i, val + accuracy_difference * accuracy_difference);
	    	    }
	    	    else if (importance_mode == ImportanceMode.IMP_PERM_LIAW) {
	    	    	val = forest_variance.get(i);
	    	    	forest_variance.set(i, val + accuracy_difference * accuracy_difference * num_samples_oob);	
	    	    }
	    	}
	    }
	    
	    public void appendToFile(BufferedWriter bw) {
	    	
	    	// Save general fields
	    	saveVector2D(child_nodeIDs, bw);
	    	saveVector1D(split_varIDs, bw);
	    	saveDVector1D(split_values, bw);
	    	
	    	// Call special functions for subclasses to save special fields\
	    	appendToFileInternal(bw);
	    }
	    
	    public void createPossibleSplitVarSubset(Vector<Integer> result) {
	    	int i;
	    	int num_vars  =  data.getNumCols();
	    	
	    	// Fro corrected Gini importance add dummy variables
	    	if (importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
	    		num_vars += data.getNumCols() - data.getNoSplitVariables().size();
	    	}
	    	
	    	// Always use deterministic variables
	    	for (i = 0; i < deterministic_varIDs.size(); i++) {
	    		result.add(deterministic_varIDs.get(i));
	    	}
	    	
	    	// Randomly add non-deterministic variables (according to weights if needed)
	    	if (split_select_weights.isEmpty()) {
	    		drawWithoutReplacementSkip(result, random, num_vars, data.getNoSplitVariables(), mtry);
	    	}
	    	else {
	    		// No corrected Gini importance supported for weighted splitting
	    		int num_draws = mtry - result.size();
	    		drawWithoutReplacementWeighted(result, random, split_select_varIDs, num_draws, split_select_weights);
	    	}
	    }
	    
	    public boolean splitNode(int nodeID) {
	    	int i;
	    	// Select random subset of variables to possibly split at
	    	Vector<Integer> possible_split_varIDs = new Vector<Integer>();
	    	createPossibleSplitVarSubset(possible_split_varIDs);
	    	
	    	// Call subclass mthod, sets split_varIds and split_values
	    	boolean stop = splitNodeInternal(nodeID, possible_split_varIDs);
	    	if (stop) {
	    	    // Terminal node
	    		return true;
	    	}
	    	
	    	int split_varID = split_varIDs.get(nodeID);
	    	double split_value = split_values.get(nodeID);
	    	
	    	// Save non-permuted variable for prediction
	    	split_varIDs.set(nodeID, data.getUnpermutedVarID(split_varID));
	    	
	    	// Create child nodes
	    	int left_child_nodeID = sampleIDs.size();
	    	child_nodeIDs.get(0).set(nodeID, left_child_nodeID);
	    	createEmptyNode();
	    	
	    	int right_child_nodeID = sampleIDs.size();
	    	child_nodeIDs.get(1).set(nodeID, right_child_nodeID);
	    	createEmptyNode();
	    	
	    	// For each sample in node, assign to left or right child
	    	if (data.isOrderedVariable(split_varID)) {
	    		// Ordered: left is <= splitval and right is > splitval
	    		for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
	    			int sampleID = sampleIDs.get(nodeID).get(i);
	    			if (data.get(sampleID, split_varID) <= split_value) {
	    				sampleIDs.get(left_child_nodeID).add(sampleID);
	    			}
	    			else {
	    				sampleIDs.get(right_child_nodeID).add(sampleID);
	    			}
	    		}
	    	}
	    	else {
	    		// Unordered: if bit at position is 1 -> right, 0 -> left
	    		for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
	    			int sampleID = sampleIDs.get(nodeID).get(i);
	    			double level = data.get(sampleID, split_varID);
	    			int factorID = (int)Math.floor(level) - 1;
	    			int splitID  = (int)Math.floor(split_value);
	    			
	    			// Left if 0 found at position factorID
	    			if ((splitID & (1 << factorID)) == 0) {
	    			    sampleIDs.get(left_child_nodeID).add(sampleID);	
	    			}
	    			else {
	    				sampleIDs.get(right_child_nodeID).add(sampleID);
	    			}
	    		}
	    	}
	    	
	    	// No terminal node
	    	return false;
	    }
	    
	    public void createEmptyNode() {
	    	split_varIDs.add(0);
	    	split_values.add(0.0);
	    	child_nodeIDs.get(0).add(0);
	    	child_nodeIDs.get(1).add(0);
	    	sampleIDs.add(new Vector<Integer>());
	    	createEmptyNodeInternal();
	    }
	    
	    public int dropDownSamplePermuted(int permuted_varID, int sampleID, int permuted_sampleID) {
	    	
	    	// Start in root and drop down
	    	int nodeID = 0;
	    	while(child_nodeIDs.get(0).get(nodeID) != 0 || child_nodeIDs.get(1).get(nodeID) != 0) {
	    		
	    		// Permute if variable is permutation variable
	    		int split_varID = split_varIDs.get(nodeID);
	    		int sampleID_final = sampleID;
	    		if (split_varID == permuted_varID) {
	    			sampleID_final = permuted_sampleID;
	    		}
	    		
	    		// Move to child
	    		double value = data.get(sampleID_final, split_varID);
	    		if (data.isOrderedVariable(split_varID)) {
	    			if (value <= split_values.get(nodeID)) {
	    				// Move to left child
	    				nodeID = child_nodeIDs.get(0).get(nodeID);
	    			}
	    			else {
	    				// Move to right child
	    				nodeID = child_nodeIDs.get(1).get(nodeID);
	    			}
	    		}
	    		else {
	    			int factorID = (int)Math.floor(value) - 1;
	    			int splitID = (int)Math.floor(split_values.get(nodeID));
	    		
	    		// Left if 0 found at position factorID
	    		if ((splitID & (1 << factorID)) == 0) {
	    			// Move to left child
	    			nodeID = child_nodeIDs.get(0).get(nodeID);
	    		}
	    		else {
	    			// Move to right child
	    			nodeID = child_nodeIDs.get(1).get(nodeID);
	    		}
	    	  }
	    	}
	    	return nodeID;
	    }
	    public void permuteAndPredictOobSamples(int permuted_varID, Vector<Integer> permutations) {
	    	// Permute OOB sample
	        Collections.shuffle(permutations);
	        
	        // For each sample, drop down the tree and add prediction
	        for (int i = 0; i < num_samples_oob; ++i) {
	        	int nodeID = dropDownSamplePermuted(permuted_varID, oob_sampleIDs.get(i),
	        			permutations.get(i));
	        	prediction_terminal_nodeIDs.set(i, nodeID);
	        }
	    }
	    
	    public void bootstrap() {
	    	int draw;
	    	// Use fraction (default 63.21%) of the samples
	    	int num_samples_inbag = (int)(num_samples * sample_fraction.get(0));
	    	
	    	// Reserve space, reserve a little more to be sure
	    	sampleIDs.get(0).ensureCapacity(num_samples_inbag);
	    	oob_sampleIDs.ensureCapacity((int)(num_samples * (Math.exp(-(sample_fraction.get(0)))+0.1)));
	    	
	    	Random random = new Random();
	    	
	    	// Start with all samples OOB
	    	 int insize = inbag_counts.size();
		     inbag_counts.setSize(num_samples);
		     for (int i = insize; i < num_samples; i++) {
		         inbag_counts.set(i, 0);
		     }
		     
		     // Draw num_samples with replacement (num_samples inbag out of n)
		     // as inbag and mark as not OOB
		     for (int s = 0; s < num_samples_inbag; ++s) {
		         draw = random.nextInt(num_samples);
		         sampleIDs.get(0).add(draw);
		         int draw_count = inbag_counts.get(draw);
		         inbag_counts.set(draw, draw_count+1);
		     }
		     
		     // Save OOB examples
		     for (int s = 0; s < inbag_counts.size(); ++s) {
		    	 if (inbag_counts.get(s) == 0) {
		    		 oob_sampleIDs.add(s);
		    	 }
		     }
		     num_samples_oob = oob_sampleIDs.size();
		     
		     if (!keep_inbag) {
		    	 inbag_counts.clear();
		     }
	    }
	    
	    public void bootstrapWeighted() {
	    	double rand;
	    	int draw = 0;
	    	int draw_count;
	    
	    	// Use fraction (default 63.21%) of the samples
	    	int num_samples_inbag = (int) (num_samples * sample_fraction.get(0));
	    	
	    	// Reserve space, reserve a little more to be safe)
	    	sampleIDs.get(0).ensureCapacity(num_samples_inbag);
	    	oob_sampleIDs.ensureCapacity((int)(num_samples * (Math.exp(-(sample_fraction.get(0)))+0.1)));
	    	
	    	double sum_of_weights = 0.0;
	    	for (int i = 0; i < case_weights.size(); i++) {
	    		sum_of_weights += case_weights.get(i);
	    	}
	        Vector<Double> weighted_dist = new Vector<Double>();
	        for (int i = 0; i < case_weights.size(); i++) {
	        	weighted_dist.add(case_weights.get(i)/sum_of_weights);
	        }
	        
	        // Start with all samples OOB
	        int insize = inbag_counts.size();
	        inbag_counts.setSize(num_samples);
	        for (int i = insize; i < num_samples; i++) {
	        	inbag_counts.set(i, 0);
	        }
	        
	        Random random = new Random();
	        // Draw num_samples with replacement (n out of n) as inbag and mark as not OOB
	        for (int s = 0; s < num_samples_inbag; ++s) {
	        	rand = random.nextDouble();	
	        	for (int j = 0; j < weighted_dist.size(); j++) {
		            if (rand < weighted_dist.get(j)) {
		            	draw = j;
		            	break;
		            }
		            else {
		            	rand = rand - weighted_dist.get(j);
		            }
		        }
	        	sampleIDs.get(0).add(draw);
		        draw_count = inbag_counts.get(draw);
		        inbag_counts.set(draw, draw_count+1);
	        } // for (int s = 0; s < num_samples_inbag; ++s)
	        
	        // Save OOB samples.  In holdout mode these are the case with 0 weight.
	        if (holdout) {
	        	for (int s = 0; s < case_weights.size(); ++s) {
	        		if (case_weights.get(s) == 0.0) {
	        			oob_sampleIDs.add(s);
	        		}
	        	}
	        }
	        else {
	        	for (int s = 0; s < inbag_counts.size(); ++s) {
	        		if (inbag_counts.get(s) == 0) {
	        			oob_sampleIDs.add(s);
	        		}
	        	}
	        }
	        num_samples_oob = oob_sampleIDs.size();
	        
	        if (!keep_inbag) {
	        	inbag_counts.clear();
	        }
	    }
	    
	    public void bootstrapWithoutReplacement() {
	    	int i;
	    	// Use fraction (default 63.21%) of the samples
	    	int num_samples_inbag = (int) (num_samples * sample_fraction.get(0));
	    	shuffleAndSplit(sampleIDs.get(0), oob_sampleIDs, num_samples, num_samples_inbag, random);
	    	num_samples_oob = oob_sampleIDs.size();
	    	
	    	if (keep_inbag) {
	    		// All observations are 0 or 1 times inbag
	    		int insize = inbag_counts.size();
	    		inbag_counts.setSize(num_samples);
	    		for (i = insize; i < num_samples; i++) {
	    			inbag_counts.set(i, 1);
	    		}
	    		for (i = 0; i < oob_sampleIDs.size(); i++) {
	    			int index = oob_sampleIDs.get(i);
	    			inbag_counts.set(index, 0);
	    		}
	    	}
	    }
	    
	    public void bootstrapWithoutReplacementWeighted() {
	    	int i;
	    	// Use fraction (default 63.21%) of the samples
	    	int num_samples_inbag = (int) (num_samples * sample_fraction.get(0));
	    	drawWithoutReplacementWeighted(sampleIDs.get(0), random, num_samples-1, num_samples_inbag,
	    			case_weights);
	    	
	    	// All Observations are 0 or 1 times inbag
	    	int insize = inbag_counts.size();
	        inbag_counts.setSize(num_samples);
	        for (i = insize; i < num_samples; i++) {
	        	inbag_counts.set(i, 0);
	        }
	    	
	    	for (i = 0; i < sampleIDs.get(0).size(); i++) {
	    		int sampleID = sampleIDs.get(0).get(i);
	    		inbag_counts.set(sampleID, 1);
	    	}
	    	
	    	// Save OOB samples.  In holdout mode these are the cases with 0 weight.
	    	if (holdout) {
	    		for (int s = 0; s < case_weights.size(); ++s) {
	    			if (case_weights.get(s) == 0.0) {
	    				oob_sampleIDs.add(s);
	    			}
	    		}
	    	}
	    	else {
	    		for (int s = 0; s < inbag_counts.size(); ++s) {
	    			if (inbag_counts.get(s) == 0) {
	    				oob_sampleIDs.addElement(s);
	    			}
	    		}
	    	}
	    	num_samples_oob = oob_sampleIDs.size();
	    	
	    	if (!keep_inbag) {
	    		inbag_counts.clear();
	    	}
	    }
	    
	    public abstract void allocateMemory();
	    
	    public abstract void appendToFileInternal(BufferedWriter bw);
	    
	    public Vector<Vector<Integer>> getChildNodeIDs() {
	    	return child_nodeIDs;
	    }
	    
	    public Vector<Double> getSplitValues() {
	    	return split_values;
	    }
	    
	    public Vector<Integer> getSplitVarIDs() {
	    	return split_varIDs;
	    }
	    
	    public Vector<Integer> getOobSampleIDs() {
	    	return oob_sampleIDs;
	    }
	    
	    public int getNumSamplesOob() {
	    	return num_samples_oob;
	    }
	    
	    public Vector<Integer> getInbagCounts() {
	    	return inbag_counts;
	    }
	    
	    protected abstract boolean splitNodeInternal(int nodeID, Vector<Integer> possible_split_varIDs);
	    
	    protected abstract void createEmptyNodeInternal();
	    
	    protected abstract double computePredictionAccuracyInternal();
	    
	    protected abstract void bootstrapClassWise();
	    protected abstract void bootstrapWithoutReplacementClassWise();
	    
	    protected abstract void cleanUpInternal();
	} 
	
	private class TreeClassification extends Tree {
		
		// Classes of the dependent variable and classIDs for responses
		Vector<Double> class_values;
		Vector<Integer> response_classIDs;
		Vector<Vector<Integer>> sampleIDs_per_class;
		
		// Splitting weights
		Vector<Double> class_weights;
		
		int counter[];
		int counter_per_class[];
		
		public TreeClassification(Vector<Double> class_values, Vector<Integer> response_classIDs, 
				Vector<Vector<Integer>> sampleIDs_per_class, Vector<Double> class_weights) {
			super();
			this.class_values = new Vector<Double>();
			this.class_values.addAll(class_values);
			this.response_classIDs = new Vector<Integer>();
			this.response_classIDs.addAll(response_classIDs);
			this.sampleIDs_per_class = new Vector<Vector<Integer>>();
			this.sampleIDs_per_class.addAll(sampleIDs_per_class);
			this.class_weights = new Vector<Double>();
			this.class_weights.addAll(class_weights);
			counter = null;
			counter_per_class = null;
		}
		
		public TreeClassification(Vector<Vector<Integer>> child_nodeIDs, Vector<Integer> split_varIDs,
				Vector<Double> split_values, Vector<Double> class_values, Vector<Integer> response_classIDs) {
			super(child_nodeIDs, split_varIDs, split_values);
			this.class_values = new Vector<Double>();
			this.class_values.addAll(class_values);
			this.response_classIDs = new Vector<Integer>();
			this.response_classIDs.addAll(response_classIDs);
			sampleIDs_per_class = new Vector<Vector<Integer>>();
			class_weights = new Vector<Double>();
			counter = null;
			counter_per_class = null;
		}
		
		public double getPrediction(int sampleID) {
		    int terminal_nodeID = prediction_terminal_nodeIDs.get(sampleID);
		    return split_values.get(terminal_nodeID);
		  }

		
		public int getPredictionTerminalNodeID(int sampleID)  {
		    return prediction_terminal_nodeIDs.get(sampleID);
		  }

		
		public void allocateMemory() {
			// Init counters if not in memory efficient mode
			if (!memory_saving_splitting) {
				int num_classes = class_values.size();
				int max_num_splits = data.getMaxNumUniqueValues();
				
				// Use number of random splits for extra trees
				if (splitrule == SplitRule.EXTRATREES && num_random_splits > max_num_splits) {
				    max_num_splits = num_random_splits;
				}
				
				counter = new int[max_num_splits];	
				counter_per_class = new int[num_classes * max_num_splits];
			}
		}
		
		public double estimate(int nodeID) {
			int i;
			// Count classes over samples in node and return class with maximum count
			Vector<Double> class_count = new Vector<Double>();
			for (i = 0; i < class_values.size(); i++) {
				class_count.add(0.0);
			}
			
			for (i = 0; i < sampleIDs.get(nodeID).size(); ++i) {
				int value = response_classIDs.get(sampleIDs.get(nodeID).get(i));
				double val  = class_count.get(value);
				class_count.set(value, val + class_weights.get(value));
			}
				
			if (sampleIDs.get(nodeID).size() > 0) {
				int result_classID = mostFrequentDClass(class_count, random);
				return class_values.get(result_classID);
			}
			else {
				MipavUtil.displayError("Error: Empty node");
				return Double.NaN; 
			}
		}
		
		public void appendToFileInternal(BufferedWriter bw) {
			// Empty on purpose
		}
		
		public boolean splitNodeInternal(int nodeID, Vector<Integer> possible_split_varIDs) {
		
			// Check node size, stop if maximum reached
			if (sampleIDs.get(nodeID).size() <= min_node_size) {
				split_values.set(nodeID, estimate(nodeID));
				return true;
			}
			
			// Check if node is pure and set split_valueto estimate and stop if pure
			boolean pure = true;
			double pure_value = 0.0;
			for (int i = 0; i < sampleIDs.get(nodeID).size(); ++i) {
				double value = data.get(sampleIDs.get(nodeID).get(i), dependent_varID);
				if (i != 0 && value != pure_value) {
					pure = false;
					break;
				}
				pure_value = value;
			}
			if (pure) {
				split_values.set(nodeID, pure_value);
				return true;
			}
			
			// Find best split, stop if no decrease of impurity
			boolean stop;
			if (splitrule == SplitRule.EXTRATREES) {
				stop = findBestSplitExtraTrees(nodeID, possible_split_varIDs);
			}
			else {
				stop = findBestSplit(nodeID, possible_split_varIDs);
			}
			
			if (stop) {
				split_values.set(nodeID, estimate(nodeID));
				return true;
			}
			
			return false;
		}
		
		public void createEmptyNodeInternal() {
			// Empty on purpose
		}
		
		public double computePredictionAccuracyInternal() {
			int num_predictions = prediction_terminal_nodeIDs.size();
			int num_missclassifications = 0;
			for (int i = 0; i < num_predictions; ++i) {
				int terminal_nodeID = prediction_terminal_nodeIDs.get(i);
				double predicted_value = split_values.get(terminal_nodeID);
				double real_value = data.get(oob_sampleIDs.get(i), dependent_varID);
				if (predicted_value != real_value) {
					++num_missclassifications;
				}
			}
			return (1.0 - (double)num_missclassifications/(double) num_predictions);
		}
		
		public boolean findBestSplit(int nodeID, Vector<Integer> possible_split_varIDs) {
			int num_samples_node = sampleIDs.get(nodeID).size();
			int num_classes = class_values.size();
			double best_decrease[] = new double[]{-1.0};
			int best_varID[] = new int[]{0};
			double best_value[] = new double[]{0.0};
			
			int class_counts[] = new int[num_classes];
			// Compute overall class counts
			for (int i = 0; i < num_samples_node; ++i) {
				int sampleID = sampleIDs.get(nodeID).get(i);
				int sample_classID = response_classIDs.get(sampleID);
				++class_counts[sample_classID];
			}
			
			// For all possible split variables
			for (int j = 0; j <possible_split_varIDs.size(); j++) {
				int varID = possible_split_varIDs.get(j);
				// Find best split value, if ordered consider all values as split values,
				// else all 2-partitions
				if (data.isOrderedVariable(varID)) {
					
					// Use memory saving method if option set
					if (memory_saving_splitting) {
					    findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node,
							best_value, best_varID, best_decrease);
				    }
				    else {
					    // Use faster method for both cases
					    double q = (double) num_samples_node / (double) data.getNumUniqueDataValues(varID);
					    if (q < Q_THRESHOLD) {
						    findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node,
								    best_value, best_varID, best_decrease);
					    }
					    else {
						    findBestSplitValueLargeQ(nodeID, varID, num_classes, class_counts, num_samples_node,
								    best_value, best_varID, best_decrease);
					    }
					}
				}
				else {
					findBestSplitValueUnordered(nodeID, varID, num_classes, class_counts, num_samples_node,
							best_value, best_varID, best_decrease);
				}
			}
			
			class_counts = null;
			
			// Stop if no good split found
			if (best_decrease[0] < 0) {
				return true;
			}
			
			// Save best values
			split_varIDs.set(nodeID, best_varID[0]);
			split_values.set(nodeID, best_value[0]);
			
			// Compute gini index for this node andto variable importance if needed
			if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
				addGiniImportance(nodeID, best_varID[0], best_decrease[0]);
			}
			return false;
		}
		
		public void findBestSplitValueSmallQ(int nodeID, int varID, int num_classes, int class_counts[], 
				int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
			int i, j;
		    // Create possible split values
			Vector<Double> possible_split_values = new Vector<Double>();
			data.getAllValues(possible_split_values, sampleIDs.get(nodeID), varID);
			
			// Try next variable if all equal for this
			if (possible_split_values.size() < 2) {
				return;
			}
			
			// Initialize with 0, if not in memory efficient mode, use pre-allocated space
			// -1 because no split possible at largest value
			int num_splits = possible_split_values.size() - 1;
			int class_counts_right[];
			int n_right[];
			if (memory_saving_splitting) {
				class_counts_right = new int[num_splits * num_classes];
				n_right = new int[num_splits];
			}
			else {
				class_counts_right = counter_per_class;
				n_right = counter;
			    for (i = 0; i < num_splits * num_classes; i++) {
			    	class_counts_right[i] = 0;
			    }
			    for (i = 0; i < num_splits; i++) {
			    	n_right[i] = 0;
			    }
			}
			
			// Count samples in right child per class and possible split
			for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
				int sampleID = sampleIDs.get(nodeID).get(j);
				double value = data.get(sampleID, varID);
				int sample_classID = response_classIDs.get(sampleID);
				
				// Count sample until split_value reached
				for (i = 0; i < num_splits; ++i) {
					if (value > possible_split_values.get(i)) {
						++n_right[i];
						++class_counts_right[i * num_classes + sample_classID];
					}
					else {
						break;
					}
				}
			}
			
			// Compute decrease of impurity for each possible split
			for (i = 0; i < num_splits; ++i) {
				
				// Stop if one child empty
				int n_left = num_samples_node - n_right[i];
				if (n_left == 0 || n_right[i] == 0) {
					continue;
				}
				
				// Sum of squares
				double sum_left = 0.0;
				double sum_right = 0.0;
				for (j = 0; j < num_classes; ++j) {
					int class_count_right = class_counts_right[i * num_classes + j];
					int class_count_left = class_counts[j] - class_count_right;
					
					sum_right += (class_weights.get(j) * class_count_right * class_count_right);
					sum_left += (class_weights.get(j) * class_count_left * class_count_left);
				}
				
				// Decrease of impurity
			    double decrease = sum_left / (double) n_left + sum_right / (double) n_right[i];

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      best_value[0] = (possible_split_values.get(i) + possible_split_values.get(i + 1)) / 2;
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;

			      // Use smaller value if average is numerically the same as the larger value
			      if (best_value[0] == possible_split_values.get(i + 1)) {
			        best_value[0] = possible_split_values.get(i);
			      }
			    }
			  }

			  if (memory_saving_splitting) {
			    class_counts_right = null;
			    n_right = null;
			  }
		}
		
		public void findBestSplitValueLargeQ(int nodeID, int varID, int num_classes, int class_counts[],
			    int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
              int i,j;
			  // Set counters to 0
			  int num_unique = data.getNumUniqueDataValues(varID);
			  for (i = 0; i < num_unique * num_classes; i++) {
				  counter_per_class[i] = 0;
			  }
			  for (i = 0; i < num_unique; i++) {
				  counter[i] = 0;
			  }

			  // Count values
			  for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
				int sampleID = sampleIDs.get(nodeID).get(j);
			    int index = data.getIndex(sampleID, varID);
			    int classID = response_classIDs.get(sampleID);

			    ++counter[index];
			    ++counter_per_class[index * num_classes + classID];
			  }

			  int n_left = 0;
			  int class_counts_left[] = new int[num_classes];

			  // Compute decrease of impurity for each split
			  for (i = 0; i < num_unique - 1; ++i) {

			    // Stop if nothing here
			    if (counter[i] == 0) {
			      continue;
			    }

			    n_left += counter[i];

			    // Stop if right child empty
			    int n_right = num_samples_node - n_left;
			    if (n_right == 0) {
			      break;
			    }

			    // Sum of squares
			    double sum_left = 0;
			    double sum_right = 0;
			    for (j = 0; j < num_classes; ++j) {
			      class_counts_left[j] += counter_per_class[i * num_classes + j];
			      int class_count_right = class_counts[j] - class_counts_left[j];

			      sum_left += (class_weights.get(j) * class_counts_left[j] * class_counts_left[j]);
			      sum_right += (class_weights.get(j) * class_count_right * class_count_right);
			    }

			    // Decrease of impurity
			    double decrease = sum_right / (double) n_right + sum_left / (double) n_left;

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      // Find next value in this node
			      j = i + 1;
			      while (j < num_unique && counter[j] == 0) {
			        ++j;
			      }

			      // Use mid-point split
			      best_value[0] = (data.getUniqueDataValue(varID, i) + data.getUniqueDataValue(varID, j)) / 2;
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;

			      // Use smaller value if average is numerically the same as the larger value
			      if (best_value[0] == data.getUniqueDataValue(varID, j)) {
			        best_value[0] = data.getUniqueDataValue(varID, i);
			      }
			    }
			  }

			  class_counts_left = null;
	    }

		 
		public void findBestSplitValueUnordered(int nodeID, int varID, int num_classes,
			    int class_counts[], int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
              int j;
			  // Create possible split values
			  Vector<Double> factor_levels = new Vector<Double>();
			  data.getAllValues(factor_levels, sampleIDs.get(nodeID), varID);

			  // Try next variable if all equal for this
			  if (factor_levels.size() < 2) {
			    return;
			  }

			  // Number of possible splits is 2^num_levels
			  int num_splits = (1 << factor_levels.size());

			  // Compute decrease of impurity for each possible split
			  // Split where all left (0) or all right (1) are excluded
			  // The second half of numbers is just left/right switched the first half -> Exclude second half
			  for (int local_splitID = 1; local_splitID < num_splits / 2; ++local_splitID) {

			    // Compute overall splitID by shifting local factorIDs to global positions
			    int splitID = 0;
			    for (j = 0; j < factor_levels.size(); ++j) {
			      if ((local_splitID & (1 << j)) != 0) {
			        double level = factor_levels.get(j);
			        int factorID = (int)Math.floor(level) - 1;
			        splitID = splitID | (1 << factorID);
			      }
			    }

			    // Initialize
			    int class_counts_right[] = new int[num_classes];
			    int n_right = 0;

			    // Count classes in left and right child
			    for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
			      int sampleID = sampleIDs.get(nodeID).get(j);
			      int sample_classID = response_classIDs.get(sampleID);
			      double value = data.get(sampleID, varID);
			      int factorID = (int)Math.floor(value) - 1;

			      // If in right child, count
			      // In right child, if bitwise splitID at position factorID is 1
			      if ((splitID & (1 << factorID)) != 0) {
			        ++n_right;
			        ++class_counts_right[sample_classID];
			      }
			    }
			    int n_left = num_samples_node - n_right;

			    // Sum of squares
			    double sum_left = 0;
			    double sum_right = 0;
			    for (j = 0; j < num_classes; ++j) {
			      int class_count_right = class_counts_right[j];
			      int class_count_left = class_counts[j] - class_count_right;

			      sum_right += (class_weights.get(j) * class_count_right * class_count_right);
			      sum_left += (class_weights.get(j) * class_count_left * class_count_left);
			    }

			    // Decrease of impurity
			    double decrease = sum_left / (double) n_left + sum_right / (double) n_right;

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      best_value[0] = splitID;
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;
			    }

			    class_counts_right = null;
			  }
			}

		
		public boolean findBestSplitExtraTrees(int nodeID, Vector<Integer> possible_split_varIDs) {
              int i;
			  int num_samples_node = sampleIDs.get(nodeID).size();
			  int num_classes = class_values.size();
			  double best_decrease[] = new double[]{-1.0};
			  int best_varID[] = new int[]{0};
			  double best_value[] = new double[]{0.0};

			  int class_counts[] = new int[num_classes];
			  // Compute overall class counts
			  for (i = 0; i < num_samples_node; ++i) {
			    int sampleID = sampleIDs.get(nodeID).get(i);
			    int sample_classID = response_classIDs.get(sampleID);
			    ++class_counts[sample_classID];
			  }

			  // For all possible split variables
		      for (i = 0; i < possible_split_varIDs.size(); i++) {
		    	int varID = possible_split_varIDs.get(i);
			    // Find best split value, if ordered consider all values as split values, else all 2-partitions
			    if (data.isOrderedVariable(varID)) {
			      findBestSplitValueExtraTrees(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
			          best_decrease);
			    } else {
			      findBestSplitValueExtraTreesUnordered(nodeID, varID, num_classes, class_counts, num_samples_node, best_value,
			          best_varID, best_decrease);
			    }
			  }

			  class_counts = null;

			  // Stop if no good split found
			  if (best_decrease[0] < 0) {
			    return true;
			  }

			  // Save best values
			  split_varIDs.set(nodeID, best_varID[0]);
			  split_values.set(nodeID, best_value[0]);

			  // Compute gini index for this node and to variable importance if needed
			  if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
			    addGiniImportance(nodeID, best_varID[0], best_decrease[0]);
			  }
			  return false;
			}

		public void findBestSplitValueExtraTrees(int nodeID, int varID, int num_classes,
			    int class_counts[], int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
              int i, j;
			  // Get min/max values of covariate in node
			  double min[] = new double[1];
			  double max[] = new double[1];
			  data.getMinMaxValues(min, max, sampleIDs.get(nodeID), varID);

			  // Try next variable if all equal for this
			  if (min[0] == max[0]) {
			    return;
			  }

			  // Create possible split values: Draw randomly between min and max
			  Vector<Double> possible_split_values = new Vector<Double>();
			  possible_split_values.ensureCapacity(num_random_splits);
			  Random random = new Random();
			  for (i = 0; i < num_random_splits; ++i) {
				double rand = (max[0] - min[0])*random.nextDouble() + min[0];
			    possible_split_values.add(rand);
			  }

			  // Initialize with 0, if not in memory efficient mode, use pre-allocated space
			  int num_splits = possible_split_values.size();
			  int class_counts_right[];
			  int n_right[];
			  if (memory_saving_splitting) {
			    class_counts_right = new int[num_splits * num_classes];
			    n_right = new int[num_splits];
			  } else {
			    class_counts_right = counter_per_class;
			    n_right = counter;
			    for (i = 0; i < num_splits * num_classes; i++) {
			    	class_counts_right[i] = 0;
			    }
			    for (i = 0; i < num_splits; i++) {
			    	n_right[i] = 0;
			    }
			  }

			  // Count samples in right child per class and possible split
		      for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
		    	int sampleID = sampleIDs.get(nodeID).get(j);
			    double value = data.get(sampleID, varID);
			    int sample_classID = response_classIDs.get(sampleID);

			    // Count samples until split_value reached
			    for (i = 0; i < num_splits; ++i) {
			      if (value > possible_split_values.get(i)) {
			        ++n_right[i];
			        ++class_counts_right[i * num_classes + sample_classID];
			      } else {
			        break;
			      }
			    }
			  }

			  // Compute decrease of impurity for each possible split
			  for (i = 0; i < num_splits; ++i) {

			    // Stop if one child empty
			    int n_left = num_samples_node - n_right[i];
			    if (n_left == 0 || n_right[i] == 0) {
			      continue;
			    }

			    // Sum of squares
			    double sum_left = 0;
			    double sum_right = 0;
			    for (j = 0; j < num_classes; ++j) {
			      int class_count_right = class_counts_right[i * num_classes + j];
			      int class_count_left = class_counts[j] - class_count_right;

			      sum_right += (class_weights.get(j) * class_count_right * class_count_right);
			      sum_left += (class_weights.get(j) * class_count_left * class_count_left);
			    }

			    // Decrease of impurity
			    double decrease = sum_left / (double) n_left + sum_right / (double) n_right[i];

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      best_value[0] = possible_split_values.get(i);
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;
			    }
			  }

			  if (memory_saving_splitting) {
			    class_counts_right = null;
			    n_right = null;
			  }
			}

		public void findBestSplitValueExtraTreesUnordered(int nodeID, int varID, int num_classes,
			    int class_counts[], int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
              int i, j;
			  int num_unique_values = data.getNumUniqueDataValues(varID);

			  // Get all factor indices in node
			  Vector<Boolean> factor_in_node = new Vector<Boolean>();
			  for (i = 0; i < num_unique_values; i++) {
				  factor_in_node.add(false);
			  }
		      for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
		    	int sampleID = sampleIDs.get(nodeID).get(i);
			    int index = data.getIndex(sampleID, varID);
			    factor_in_node.set(index, true);
			  }

			  // Vector of indices in and out of node
			  Vector<Integer> indices_in_node = new Vector<Integer>();
			  Vector<Integer> indices_out_node = new Vector<Integer>();
			  indices_in_node.ensureCapacity(num_unique_values);
			  indices_out_node.ensureCapacity(num_unique_values);
			  for (i = 0; i < num_unique_values; ++i) {
			    if (factor_in_node.get(i)) {
			      indices_in_node.add(i);
			    } else {
			      indices_out_node.add(i);
			    }
			  }

			  // Generate num_random_splits splits
			  Random random = new Random();
			  for (i = 0; i < num_random_splits; ++i) {
			    Vector<Integer> split_subset = new Vector<Integer>();
			    split_subset.ensureCapacity(num_unique_values);

			    // Draw random subsets, sample all partitions with equal probability
			    if (indices_in_node.size() > 1) {
			      int num_partitions = (2 << (indices_in_node.size() - 1)) - 2; // 2^n-2 (don't allow full or empty)
			      int splitID_in_node =  1 + random.nextInt(num_partitions);
			      for (j = 0; j < indices_in_node.size(); ++j) {
			        if ((splitID_in_node & (1 << j)) > 0) {
			          split_subset.add(indices_in_node.get(j));
			        }
			      }
			    }
			    if (indices_out_node.size() > 1) {
			      int num_partitions = (2 << (indices_out_node.size() - 1)) - 1; // 2^n-1 (allow full or empty)
			      int splitID_out_node = random.nextInt(num_partitions+1);
			      for (j = 0; j < indices_out_node.size(); ++j) {
			        if ((splitID_out_node & (1 << j)) > 0) {
			          split_subset.add(indices_out_node.get(j));
			        }
			      }
			    }

			    // Assign union of the two subsets to right child
			    int splitID = 0;
			    for (j = 0; j < split_subset.size(); j++) {
			      int idx = split_subset.get(j);
			      splitID |= 1 << idx;
			    }

			    // Initialize
			    int class_counts_right[] = new int[num_classes];
			    int n_right = 0;

			    // Count classes in left and right child
			    for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
			      int sampleID = sampleIDs.get(nodeID).get(j);
			      int sample_classID = response_classIDs.get(sampleID);
			      double value = data.get(sampleID, varID);
			      int factorID = (int)Math.floor(value) - 1;

			      // If in right child, count
			      // In right child, if bitwise splitID at position factorID is 1
			      if ((splitID & (1 << factorID)) != 0) {
			        ++n_right;
			        ++class_counts_right[sample_classID];
			      }
			    }
			    int n_left = num_samples_node - n_right;

			    // Sum of squares
			    double sum_left = 0;
			    double sum_right = 0;
			    for (j = 0; j < num_classes; ++j) {
			      int class_count_right = class_counts_right[j];
			      int class_count_left = class_counts[j] - class_count_right;

			      sum_right += (class_weights.get(j) * class_count_right * class_count_right);
			      sum_left += (class_weights.get(j) * class_count_left * class_count_left);
			    }

			    // Decrease of impurity
			    double decrease = sum_left / (double) n_left + sum_right / (double) n_right;

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      best_value[0] = splitID;
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;
			    }

			    class_counts_right = null;
			  }
			}

		
		public void addGiniImportance(int nodeID, int varID, double decrease) {
              int i;
			  Vector<Integer> class_counts = new Vector<Integer>();
			  for (i = 0; i < class_values.size(); i++) {
				  class_counts.add(0);
			  }

			  for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
				int sampleID = sampleIDs.get(nodeID).get(i);
			    int sample_classID = response_classIDs.get(sampleID);
			    int val = class_counts.get(sample_classID);
			    class_counts.set(sample_classID, val+1);
			  }
			  double sum_node = 0;
			  for (i = 0; i < class_counts.size(); i++) {
				int class_count = class_counts.get(i);
			    sum_node += class_count * class_count;
			  }
			  double best_gini = decrease - sum_node / (double) sampleIDs.get(nodeID).size();

			  // No variable importance for no split variables
			  int tempvarID = data.getUnpermutedVarID(varID);
			  for (i = 0; i < data.getNoSplitVariables().size(); i++) {
				int skip = data.getNoSplitVariables().get(i);
			    if (tempvarID >= skip) {
			      --tempvarID;
			    }
			  }

			  // Subtract if corrected importance and permuted variable, else add
			  double dval;
			  if (importance_mode == ImportanceMode.IMP_GINI_CORRECTED && varID >= data.getNumCols()) {
				 dval = variable_importance.get(tempvarID);
				 variable_importance.set(tempvarID, dval - best_gini);
			  } else {
			    dval = variable_importance.get(tempvarID);
				variable_importance.set(tempvarID, dval + best_gini);
			  }
			}

		public void bootstrapClassWise() {
			  int i, j;
			  // Number of samples is sum of sample fraction * number of samples
			  int num_samples_inbag = 0;
			  double sum_sample_fraction = 0;
			  for (i = 0; i < sample_fraction.size(); i++) {
				double s = sample_fraction.get(i);
			    num_samples_inbag += (int) (num_samples * s);
			    sum_sample_fraction += s;
			  }

			  // Reserve space, reserve a little more to be save)
			  sampleIDs.get(0).ensureCapacity(num_samples_inbag);
			  oob_sampleIDs.ensureCapacity((int)(num_samples * (Math.exp(-sum_sample_fraction) + 0.1)));

			  // Start with all samples OOB
			  int insize = inbag_counts.size();
			  inbag_counts.setSize(num_samples);
			  for (i = insize; i < num_samples; i++) {
				  inbag_counts.set(i, 0);
			  }

			  // Draw samples for each class
			  Random random = new Random();
			  for (i = 0; i < sample_fraction.size(); ++i) {
			    // Draw samples of class with replacement as inbag and mark as not OOB
			    int num_samples_class = sampleIDs_per_class.get(i).size();
			    int num_samples_inbag_class = (int)(num_samples * sample_fraction.get(i));
			    for (j = 0; j < num_samples_inbag_class; ++j) {
			      int rand = random.nextInt(num_samples_class);
			      int draw = sampleIDs_per_class.get(i).get(rand);
			      sampleIDs.get(0).add(draw);
			      int val = inbag_counts.get(draw);
			      inbag_counts.set(draw, val+1);
			    }
			  }

			  // Save OOB samples
			  for (j = 0; j < inbag_counts.size(); ++j) {
			    if (inbag_counts.get(j) == 0) {
			      oob_sampleIDs.add(j);
			    }
			  }
			  num_samples_oob = oob_sampleIDs.size();

			  if (!keep_inbag) {
			    inbag_counts.clear();
			  }
			}

		public void bootstrapWithoutReplacementClassWise() {
			  int i;
			  // Number of samples is sum of sample fraction * number of samples
			  //int num_samples_inbag = 0;
			  //double sum_sample_fraction = 0;
			  //for (i = 0; i < sample_fraction.size(); i++) {
				//double s = sample_fraction.get(i);
			    //num_samples_inbag += (int) (num_samples * s);
			    //sum_sample_fraction += s;
			  //}

			  // Draw samples for each class
			  for (i = 0; i < sample_fraction.size(); ++i) {
			    int num_samples_class = sampleIDs_per_class.get(i).size();
			    int num_samples_inbag_class = (int) (num_samples * sample_fraction.get(i));

			    shuffleAndSplitAppend(sampleIDs.get(0), oob_sampleIDs, num_samples_class, num_samples_inbag_class,
			        sampleIDs_per_class.get(i), random);
			  }

			  if (keep_inbag) {
			    // All observation are 0 or 1 times inbag
				int insize = inbag_counts.size();
				inbag_counts.setSize(num_samples);
				for (i = insize; i < num_samples; i++) {
					inbag_counts.set(i, 1);
				}
			    for (i = 0; i < oob_sampleIDs.size(); i++) {
			      inbag_counts.set(oob_sampleIDs.get(i), 0);
			    }
			  }
			}

		
		public void cleanUpInternal() {
			if (counter != null) {
				counter = null;
			}
			if (counter_per_class != null) {
				counter_per_class = null;
			}
		}
	}  // private class TreeClassification
	
	private class TreeProbability extends Tree {
		// Classes of the dependent variable and classIDs for responses
		Vector<Double> class_values;
		Vector<Integer> response_classIDs;
		Vector<Vector<Integer>> sampleIDs_per_class;
		
		// Class counts in terminal nodes.  Empty for non-terminal nodes
		Vector<Vector<Double>> terminal_class_counts;
		
		// Splitting weights
		Vector<Double> class_weights;
		
		int counter[];
		int counter_per_class[];
		
		public TreeProbability(Vector<Double> class_values, Vector<Integer> response_classIDs, 
				Vector<Vector<Integer>> sampleIDs_per_class, Vector<Double> class_weights) {
			super();
			this.class_values = new Vector<Double>();
			this.class_values.addAll(class_values);
			this.response_classIDs = new Vector<Integer>();
			this.response_classIDs.addAll(response_classIDs);
			this.sampleIDs_per_class = new Vector<Vector<Integer>>();
			this.sampleIDs_per_class.addAll(sampleIDs_per_class);
			this.class_weights = new Vector<Double>();
			this.class_weights.addAll(class_weights);
			counter = null;
			counter_per_class = null;
		}
		
		public TreeProbability(Vector<Vector<Integer>> child_nodeIDs, Vector<Integer> split_varIDs,
				Vector<Double> split_values, Vector<Double> class_values, Vector<Integer> response_classIDs,
				Vector<Vector<Double>> terminal_class_counts) {
			super(child_nodeIDs, split_varIDs, split_values);
			this.class_values = new Vector<Double>();
			this.class_values.addAll(class_values);
			this.response_classIDs = new Vector<Integer>();
			this.response_classIDs.addAll(response_classIDs);
			sampleIDs_per_class = new Vector<Vector<Integer>>();
			this.terminal_class_counts = new Vector<Vector<Double>>();
			this.terminal_class_counts.addAll(terminal_class_counts);
			class_weights = new Vector<Double>();
			counter = null;
			counter_per_class = null;
		}
		
		public void cleanUpInternal() {
			if (counter != null) {
				counter = null;
			}
			if (counter_per_class != null) {
				counter_per_class = null;
			}
		}
		
		public Vector<Double> getPrediction(int sampleID) {
			int terminal_nodeID = prediction_terminal_nodeIDs.get(sampleID);
			return terminal_class_counts.get(terminal_nodeID);
		}
		
		public int getPredictionTerminalNodeID(int sampleID) {
			return prediction_terminal_nodeIDs.get(sampleID);
		}
		
		public Vector<Vector<Double>> getTerminalClassCounts() {
			return terminal_class_counts;
		}
		
		public void allocateMemory() {
			// Init counters if not in memory efficient mode
			if (!memory_saving_splitting) {
				int num_classes = class_values.size();
				int max_num_splits = data.getMaxNumUniqueValues();
				
				// Use number of random splits for extra trees
				if (splitrule == SplitRule.EXTRATREES && num_random_splits > max_num_splits) {
				    max_num_splits = num_random_splits;
				}
				
				counter = new int[max_num_splits];	
				counter_per_class = new int[num_classes * max_num_splits];
			}
		}
		
		public void addToTerminalNodes(int nodeID) {
              int i;
			  int num_samples_in_node = sampleIDs.get(nodeID).size();
			  int insize = terminal_class_counts.get(nodeID).size();
			  terminal_class_counts.get(nodeID).setSize(class_values.size());
			  for (i = insize; i < class_values.size(); i++) {
				  terminal_class_counts.get(nodeID).set(i, 0.0);
			  }
			  

			  // Compute counts
			  double val;
			  for (i = 0; i < num_samples_in_node; ++i) {
			    int node_sampleID = sampleIDs.get(nodeID).get(i);
			    int classID = response_classIDs.get(node_sampleID);
			    val = terminal_class_counts.get(nodeID).get(classID);
			    terminal_class_counts.get(nodeID).set(classID, val + 1);
			  }

			  // Compute fractions
			  for (i = 0; i < terminal_class_counts.get(nodeID).size(); ++i) {
				val = terminal_class_counts.get(nodeID).get(i);
				terminal_class_counts.get(nodeID).set(i, val/num_samples_in_node);
			  }
			}
		
		public void appendToFileInternal(BufferedWriter bw) { 

			  // Add Terminal node class counts
			  // Convert to vector without empty elements and save
			  Vector<Integer> terminal_nodes = new Vector<Integer>();
			  Vector<Vector<Double>> terminal_class_counts_vector = new Vector<Vector<Double>>();
			  for (int i = 0; i < terminal_class_counts.size(); ++i) {
			    if (!terminal_class_counts.get(i).isEmpty()) {
			      terminal_nodes.add(i);
			      terminal_class_counts_vector.add(terminal_class_counts.get(i));
			    }
			  }
			  saveVector1D(terminal_nodes, bw);
			  saveDVector2D(terminal_class_counts_vector, bw);
			} 

		public boolean splitNodeInternal(int nodeID, Vector<Integer> possible_split_varIDs) {

			  // Check node size, stop if maximum reached
			  if (sampleIDs.get(nodeID).size() <= min_node_size) {
			    addToTerminalNodes(nodeID);
			    return true;
			  }

			  // Check if node is pure and set split_value to estimate and stop if pure
			  boolean pure = true;
			  double pure_value = 0.0;
			  for (int i = 0; i < sampleIDs.get(nodeID).size(); ++i) {
			    double value = data.get(sampleIDs.get(nodeID).get(i), dependent_varID);
			    if (i != 0 && value != pure_value) {
			      pure = false;
			      break;
			    }
			    pure_value = value;
			  }
			  if (pure) {
			    addToTerminalNodes(nodeID);
			    return true;
			  }

			  // Find best split, stop if no decrease of impurity
			  boolean stop;
			  if (splitrule == SplitRule.EXTRATREES) {
			    stop = findBestSplitExtraTrees(nodeID, possible_split_varIDs);
			  } else {
			    stop = findBestSplit(nodeID, possible_split_varIDs);
			  }

			  if (stop) {
			    addToTerminalNodes(nodeID);
			    return true;
			  }

			  return false;
			}

		public void createEmptyNodeInternal() {
			  terminal_class_counts.add(new Vector<Double>());
			}
	
		public double computePredictionAccuracyInternal() {

			  int num_predictions = prediction_terminal_nodeIDs.size();
			  double sum_of_squares = 0;
			  for (int i = 0; i < num_predictions; ++i) {
			    int sampleID = oob_sampleIDs.get(i);
			    int real_classID = response_classIDs.get(sampleID);
			    int terminal_nodeID = prediction_terminal_nodeIDs.get(i);
			    double predicted_value = terminal_class_counts.get(terminal_nodeID).get(real_classID);
			    sum_of_squares += (1 - predicted_value) * (1 - predicted_value);
			  }
			  return (1.0 - sum_of_squares / (double) num_predictions);
			}
		
		public boolean findBestSplit(int nodeID, Vector<Integer> possible_split_varIDs) {
			int num_samples_node = sampleIDs.get(nodeID).size();
			int num_classes = class_values.size();
			double best_decrease[] = new double[]{-1.0};
			int best_varID[] = new int[]{0};
			double best_value[] = new double[]{0.0};
			
			int class_counts[] = new int[num_classes];
			// Compute overall class counts
			for (int i = 0; i < num_samples_node; ++i) {
				int sampleID = sampleIDs.get(nodeID).get(i);
				int sample_classID = response_classIDs.get(sampleID);
				++class_counts[sample_classID];
			}
			
			// For all possible split variables
			for (int j = 0; j <possible_split_varIDs.size(); j++) {
				int varID = possible_split_varIDs.get(j);
				// Find best split value, if ordered consider all values as split values,
				// else all 2-partitions
				if (data.isOrderedVariable(varID)) {
					
					// Use memory saving method if option set
					if (memory_saving_splitting) {
					    findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node,
							best_value, best_varID, best_decrease);
				    }
				    else {
					    // Use faster method for both cases
					    double q = (double) num_samples_node / (double) data.getNumUniqueDataValues(varID);
					    if (q < Q_THRESHOLD) {
						    findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node,
								    best_value, best_varID, best_decrease);
					    }
					    else {
						    findBestSplitValueLargeQ(nodeID, varID, num_classes, class_counts, num_samples_node,
								    best_value, best_varID, best_decrease);
					    }
					}
				}
				else {
					findBestSplitValueUnordered(nodeID, varID, num_classes, class_counts, num_samples_node,
							best_value, best_varID, best_decrease);
				}
			}
			
			class_counts = null;
			
			// Stop if no good split found
			if (best_decrease[0] < 0) {
				return true;
			}
			
			// Save best values
			split_varIDs.set(nodeID, best_varID[0]);
			split_values.set(nodeID, best_value[0]);
			
			// Compute gini index for this node andto variable importance if needed
			if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
				addImpurityImportance(nodeID, best_varID[0], best_decrease[0]);
			}
			return false;
		}
		
		public void findBestSplitValueSmallQ(int nodeID, int varID, int num_classes, int class_counts[], 
				int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
			int i, j;
		    // Create possible split values
			Vector<Double> possible_split_values = new Vector<Double>();
			data.getAllValues(possible_split_values, sampleIDs.get(nodeID), varID);
			
			// Try next variable if all equal for this
			if (possible_split_values.size() < 2) {
				return;
			}
			
			// Initialize with 0, if not in memory efficient mode, use pre-allocated space
			// -1 because no split possible at largest value
			int num_splits = possible_split_values.size() - 1;
			int class_counts_right[];
			int n_right[];
			if (memory_saving_splitting) {
				class_counts_right = new int[num_splits * num_classes];
				n_right = new int[num_splits];
			}
			else {
				class_counts_right = counter_per_class;
				n_right = counter;
			    for (i = 0; i < num_splits * num_classes; i++) {
			    	class_counts_right[i] = 0;
			    }
			    for (i = 0; i < num_splits; i++) {
			    	n_right[i] = 0;
			    }
			}
			
			// Count samples in right child per class and possible split
			for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
				int sampleID = sampleIDs.get(nodeID).get(j);
				double value = data.get(sampleID, varID);
				int sample_classID = response_classIDs.get(sampleID);
				
				// Count sample until split_value reached
				for (i = 0; i < num_splits; ++i) {
					if (value > possible_split_values.get(i)) {
						++n_right[i];
						++class_counts_right[i * num_classes + sample_classID];
					}
					else {
						break;
					}
				}
			}
			
			// Compute decrease of impurity for each possible split
			for (i = 0; i < num_splits; ++i) {
				
				// Stop if one child empty
				int n_left = num_samples_node - n_right[i];
				if (n_left == 0 || n_right[i] == 0) {
					continue;
				}
				
				// Sum of squares
				double sum_left = 0.0;
				double sum_right = 0.0;
				for (j = 0; j < num_classes; ++j) {
					int class_count_right = class_counts_right[i * num_classes + j];
					int class_count_left = class_counts[j] - class_count_right;
					
					sum_right += (class_weights.get(j) * class_count_right * class_count_right);
					sum_left += (class_weights.get(j) * class_count_left * class_count_left);
				}
				
				// Decrease of impurity
			    double decrease = sum_left / (double) n_left + sum_right / (double) n_right[i];

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      best_value[0] = (possible_split_values.get(i) + possible_split_values.get(i + 1)) / 2;
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;

			      // Use smaller value if average is numerically the same as the larger value
			      if (best_value[0] == possible_split_values.get(i + 1)) {
			        best_value[0] = possible_split_values.get(i);
			      }
			    }
			  }

			  if (memory_saving_splitting) {
			    class_counts_right = null;
			    n_right = null;
			  }
		}
		
		public void findBestSplitValueLargeQ(int nodeID, int varID, int num_classes, int class_counts[],
			    int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
              int i,j;
			  // Set counters to 0
			  int num_unique = data.getNumUniqueDataValues(varID);
			  for (i = 0; i < num_unique * num_classes; i++) {
				  counter_per_class[i] = 0;
			  }
			  for (i = 0; i < num_unique; i++) {
				  counter[i] = 0;
			  }

			  // Count values
			  for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
				int sampleID = sampleIDs.get(nodeID).get(j);
			    int index = data.getIndex(sampleID, varID);
			    int classID = response_classIDs.get(sampleID);

			    ++counter[index];
			    ++counter_per_class[index * num_classes + classID];
			  }

			  int n_left = 0;
			  int class_counts_left[] = new int[num_classes];

			  // Compute decrease of impurity for each split
			  for (i = 0; i < num_unique - 1; ++i) {

			    // Stop if nothing here
			    if (counter[i] == 0) {
			      continue;
			    }

			    n_left += counter[i];

			    // Stop if right child empty
			    int n_right = num_samples_node - n_left;
			    if (n_right == 0) {
			      break;
			    }

			    // Sum of squares
			    double sum_left = 0;
			    double sum_right = 0;
			    for (j = 0; j < num_classes; ++j) {
			      class_counts_left[j] += counter_per_class[i * num_classes + j];
			      int class_count_right = class_counts[j] - class_counts_left[j];

			      sum_left += (class_weights.get(j) * class_counts_left[j] * class_counts_left[j]);
			      sum_right += (class_weights.get(j) * class_count_right * class_count_right);
			    }

			    // Decrease of impurity
			    double decrease = sum_right / (double) n_right + sum_left / (double) n_left;

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      // Find next value in this node
			      j = i + 1;
			      while (j < num_unique && counter[j] == 0) {
			        ++j;
			      }

			      // Use mid-point split
			      best_value[0] = (data.getUniqueDataValue(varID, i) + data.getUniqueDataValue(varID, j)) / 2;
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;

			      // Use smaller value if average is numerically the same as the larger value
			      if (best_value[0] == data.getUniqueDataValue(varID, j)) {
			        best_value[0] = data.getUniqueDataValue(varID, i);
			      }
			    }
			  }

			  class_counts_left = null;
	    }
		
		public void findBestSplitValueUnordered(int nodeID, int varID, int num_classes,
			    int class_counts[], int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
              int j;
			  // Create possible split values
			  Vector<Double> factor_levels = new Vector<Double>();
			  data.getAllValues(factor_levels, sampleIDs.get(nodeID), varID);

			  // Try next variable if all equal for this
			  if (factor_levels.size() < 2) {
			    return;
			  }

			  // Number of possible splits is 2^num_levels
			  int num_splits = (1 << factor_levels.size());

			  // Compute decrease of impurity for each possible split
			  // Split where all left (0) or all right (1) are excluded
			  // The second half of numbers is just left/right switched the first half -> Exclude second half
			  for (int local_splitID = 1; local_splitID < num_splits / 2; ++local_splitID) {

			    // Compute overall splitID by shifting local factorIDs to global positions
			    int splitID = 0;
			    for (j = 0; j < factor_levels.size(); ++j) {
			      if ((local_splitID & (1 << j)) != 0) {
			        double level = factor_levels.get(j);
			        int factorID = (int)Math.floor(level) - 1;
			        splitID = splitID | (1 << factorID);
			      }
			    }

			    // Initialize
			    int class_counts_right[] = new int[num_classes];
			    int n_right = 0;

			    // Count classes in left and right child
			    for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
			      int sampleID = sampleIDs.get(nodeID).get(j);
			      int sample_classID = response_classIDs.get(sampleID);
			      double value = data.get(sampleID, varID);
			      int factorID = (int)Math.floor(value) - 1;

			      // If in right child, count
			      // In right child, if bitwise splitID at position factorID is 1
			      if ((splitID & (1 << factorID)) != 0) {
			        ++n_right;
			        ++class_counts_right[sample_classID];
			      }
			    }
			    int n_left = num_samples_node - n_right;

			    // Sum of squares
			    double sum_left = 0;
			    double sum_right = 0;
			    for (j = 0; j < num_classes; ++j) {
			      int class_count_right = class_counts_right[j];
			      int class_count_left = class_counts[j] - class_count_right;

			      sum_right += (class_weights.get(j) * class_count_right * class_count_right);
			      sum_left += (class_weights.get(j) * class_count_left * class_count_left);
			    }

			    // Decrease of impurity
			    double decrease = sum_left / (double) n_left + sum_right / (double) n_right;

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      best_value[0] = splitID;
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;
			    }

			    class_counts_right = null;
			  }
			}
		
		public boolean findBestSplitExtraTrees(int nodeID, Vector<Integer> possible_split_varIDs) {
            int i;
			  int num_samples_node = sampleIDs.get(nodeID).size();
			  int num_classes = class_values.size();
			  double best_decrease[] = new double[]{-1.0};
			  int best_varID[] = new int[]{0};
			  double best_value[] = new double[]{0.0};

			  int class_counts[] = new int[num_classes];
			  // Compute overall class counts
			  for (i = 0; i < num_samples_node; ++i) {
			    int sampleID = sampleIDs.get(nodeID).get(i);
			    int sample_classID = response_classIDs.get(sampleID);
			    ++class_counts[sample_classID];
			  }

			  // For all possible split variables
		      for (i = 0; i < possible_split_varIDs.size(); i++) {
		    	int varID = possible_split_varIDs.get(i);
			    // Find best split value, if ordered consider all values as split values, else all 2-partitions
			    if (data.isOrderedVariable(varID)) {
			      findBestSplitValueExtraTrees(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
			          best_decrease);
			    } else {
			      findBestSplitValueExtraTreesUnordered(nodeID, varID, num_classes, class_counts, num_samples_node, best_value,
			          best_varID, best_decrease);
			    }
			  }

			  class_counts = null;

			  // Stop if no good split found
			  if (best_decrease[0] < 0) {
			    return true;
			  }

			  // Save best values
			  split_varIDs.set(nodeID, best_varID[0]);
			  split_values.set(nodeID, best_value[0]);

			  // Compute gini index for this node and to variable importance if needed
			  if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
			    addImpurityImportance(nodeID, best_varID[0], best_decrease[0]);
			  }
			  return false;
			}
		
		public void findBestSplitValueExtraTrees(int nodeID, int varID, int num_classes,
			    int class_counts[], int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
              int i, j;
			  // Get min/max values of covariate in node
			  double min[] = new double[1];
			  double max[] = new double[1];
			  data.getMinMaxValues(min, max, sampleIDs.get(nodeID), varID);

			  // Try next variable if all equal for this
			  if (min[0] == max[0]) {
			    return;
			  }

			  // Create possible split values: Draw randomly between min and max
			  Vector<Double> possible_split_values = new Vector<Double>();
			  possible_split_values.ensureCapacity(num_random_splits);
			  Random random = new Random();
			  for (i = 0; i < num_random_splits; ++i) {
				double rand = (max[0] - min[0])*random.nextDouble() + min[0];
			    possible_split_values.add(rand);
			  }

			  // Initialize with 0, if not in memory efficient mode, use pre-allocated space
			  int num_splits = possible_split_values.size();
			  int class_counts_right[];
			  int n_right[];
			  if (memory_saving_splitting) {
			    class_counts_right = new int[num_splits * num_classes];
			    n_right = new int[num_splits];
			  } else {
			    class_counts_right = counter_per_class;
			    n_right = counter;
			    for (i = 0; i < num_splits * num_classes; i++) {
			    	class_counts_right[i] = 0;
			    }
			    for (i = 0; i < num_splits; i++) {
			    	n_right[i] = 0;
			    }
			  }

			  // Count samples in right child per class and possible split
		      for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
		    	int sampleID = sampleIDs.get(nodeID).get(j);
			    double value = data.get(sampleID, varID);
			    int sample_classID = response_classIDs.get(sampleID);

			    // Count samples until split_value reached
			    for (i = 0; i < num_splits; ++i) {
			      if (value > possible_split_values.get(i)) {
			        ++n_right[i];
			        ++class_counts_right[i * num_classes + sample_classID];
			      } else {
			        break;
			      }
			    }
			  }

			  // Compute decrease of impurity for each possible split
			  for (i = 0; i < num_splits; ++i) {

			    // Stop if one child empty
			    int n_left = num_samples_node - n_right[i];
			    if (n_left == 0 || n_right[i] == 0) {
			      continue;
			    }

			    // Sum of squares
			    double sum_left = 0;
			    double sum_right = 0;
			    for (j = 0; j < num_classes; ++j) {
			      int class_count_right = class_counts_right[i * num_classes + j];
			      int class_count_left = class_counts[j] - class_count_right;

			      sum_right += (class_weights.get(j) * class_count_right * class_count_right);
			      sum_left += (class_weights.get(j) * class_count_left * class_count_left);
			    }

			    // Decrease of impurity
			    double decrease = sum_left / (double) n_left + sum_right / (double) n_right[i];

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      best_value[0] = possible_split_values.get(i);
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;
			    }
			  }

			  if (memory_saving_splitting) {
			    class_counts_right = null;
			    n_right = null;
			  }
			}
		
		public void findBestSplitValueExtraTreesUnordered(int nodeID, int varID, int num_classes,
			    int class_counts[], int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
              int i, j;
			  int num_unique_values = data.getNumUniqueDataValues(varID);

			  // Get all factor indices in node
			  Vector<Boolean> factor_in_node = new Vector<Boolean>();
			  for (i = 0; i < num_unique_values; i++) {
				  factor_in_node.add(false);
			  }
		      for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
		    	int sampleID = sampleIDs.get(nodeID).get(i);
			    int index = data.getIndex(sampleID, varID);
			    factor_in_node.set(index, true);
			  }

			  // Vector of indices in and out of node
			  Vector<Integer> indices_in_node = new Vector<Integer>();
			  Vector<Integer> indices_out_node = new Vector<Integer>();
			  indices_in_node.ensureCapacity(num_unique_values);
			  indices_out_node.ensureCapacity(num_unique_values);
			  for (i = 0; i < num_unique_values; ++i) {
			    if (factor_in_node.get(i)) {
			      indices_in_node.add(i);
			    } else {
			      indices_out_node.add(i);
			    }
			  }

			  // Generate num_random_splits splits
			  Random random = new Random();
			  for (i = 0; i < num_random_splits; ++i) {
			    Vector<Integer> split_subset = new Vector<Integer>();
			    split_subset.ensureCapacity(num_unique_values);

			    // Draw random subsets, sample all partitions with equal probability
			    if (indices_in_node.size() > 1) {
			      int num_partitions = (2 << (indices_in_node.size() - 1)) - 2; // 2^n-2 (don't allow full or empty)
			      int splitID_in_node =  1 + random.nextInt(num_partitions);
			      for (j = 0; j < indices_in_node.size(); ++j) {
			        if ((splitID_in_node & (1 << j)) > 0) {
			          split_subset.add(indices_in_node.get(j));
			        }
			      }
			    }
			    if (indices_out_node.size() > 1) {
			      int num_partitions = (2 << (indices_out_node.size() - 1)) - 1; // 2^n-1 (allow full or empty)
			      int splitID_out_node = random.nextInt(num_partitions+1);
			      for (j = 0; j < indices_out_node.size(); ++j) {
			        if ((splitID_out_node & (1 << j)) > 0) {
			          split_subset.add(indices_out_node.get(j));
			        }
			      }
			    }

			    // Assign union of the two subsets to right child
			    int splitID = 0;
			    for (j = 0; j < split_subset.size(); j++) {
			      int idx = split_subset.get(j);
			      splitID |= 1 << idx;
			    }

			    // Initialize
			    int class_counts_right[] = new int[num_classes];
			    int n_right = 0;

			    // Count classes in left and right child
			    for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
			      int sampleID = sampleIDs.get(nodeID).get(j);
			      int sample_classID = response_classIDs.get(sampleID);
			      double value = data.get(sampleID, varID);
			      int factorID = (int)Math.floor(value) - 1;

			      // If in right child, count
			      // In right child, if bitwise splitID at position factorID is 1
			      if ((splitID & (1 << factorID)) != 0) {
			        ++n_right;
			        ++class_counts_right[sample_classID];
			      }
			    }
			    int n_left = num_samples_node - n_right;

			    // Sum of squares
			    double sum_left = 0;
			    double sum_right = 0;
			    for (j = 0; j < num_classes; ++j) {
			      int class_count_right = class_counts_right[j];
			      int class_count_left = class_counts[j] - class_count_right;

			      sum_right += (class_weights.get(j) * class_count_right * class_count_right);
			      sum_left += (class_weights.get(j) * class_count_left * class_count_left);
			    }

			    // Decrease of impurity
			    double decrease = sum_left / (double) n_left + sum_right / (double) n_right;

			    // If better than before, use this
			    if (decrease > best_decrease[0]) {
			      best_value[0] = splitID;
			      best_varID[0] = varID;
			      best_decrease[0] = decrease;
			    }

			    class_counts_right = null;
			  }
			}
		
		public void addImpurityImportance(int nodeID, int varID, double decrease) {
            int i;
			  Vector<Integer> class_counts = new Vector<Integer>();
			  for (i = 0; i < class_values.size(); i++) {
				  class_counts.add(0);
			  }

			  for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
				int sampleID = sampleIDs.get(nodeID).get(i);
			    int sample_classID = response_classIDs.get(sampleID);
			    int val = class_counts.get(sample_classID);
			    class_counts.set(sample_classID, val+1);
			  }
			  double sum_node = 0;
			  for (i = 0; i < class_counts.size(); i++) {
				int class_count = class_counts.get(i);
			    sum_node += class_count * class_count;
			  }
			  double best_gini = decrease - sum_node / (double) sampleIDs.get(nodeID).size();

			  // No variable importance for no split variables
			  int tempvarID = data.getUnpermutedVarID(varID);
			  for (i = 0; i < data.getNoSplitVariables().size(); i++) {
				int skip = data.getNoSplitVariables().get(i);
			    if (tempvarID >= skip) {
			      --tempvarID;
			    }
			  }

			  // Subtract if corrected importance and permuted variable, else add
			  double dval;
			  if (importance_mode == ImportanceMode.IMP_GINI_CORRECTED && varID >= data.getNumCols()) {
				 dval = variable_importance.get(tempvarID);
				 variable_importance.set(tempvarID, dval - best_gini);
			  } else {
			    dval = variable_importance.get(tempvarID);
				variable_importance.set(tempvarID, dval + best_gini);
			  }
			}
		
		public void bootstrapClassWise() {
			  int i, j;
			  // Number of samples is sum of sample fraction * number of samples
			  int num_samples_inbag = 0;
			  double sum_sample_fraction = 0;
			  for (i = 0; i < sample_fraction.size(); i++) {
				double s = sample_fraction.get(i);
			    num_samples_inbag += (int) (num_samples * s);
			    sum_sample_fraction += s;
			  }

			  // Reserve space, reserve a little more to be save)
			  sampleIDs.get(0).ensureCapacity(num_samples_inbag);
			  oob_sampleIDs.ensureCapacity((int)(num_samples * (Math.exp(-sum_sample_fraction) + 0.1)));

			  // Start with all samples OOB
			  int insize = inbag_counts.size();
			  inbag_counts.setSize(num_samples);
			  for (i = insize; i < num_samples; i++) {
				  inbag_counts.set(i, 0);
			  }

			  // Draw samples for each class
			  Random random = new Random();
			  for (i = 0; i < sample_fraction.size(); ++i) {
			    // Draw samples of class with replacement as inbag and mark as not OOB
			    int num_samples_class = sampleIDs_per_class.get(i).size();
			    int num_samples_inbag_class = (int)(num_samples * sample_fraction.get(i));
			    for (j = 0; j < num_samples_inbag_class; ++j) {
			      int rand = random.nextInt(num_samples_class);
			      int draw = sampleIDs_per_class.get(i).get(rand);
			      sampleIDs.get(0).add(draw);
			      int val = inbag_counts.get(draw);
			      inbag_counts.set(draw, val+1);
			    }
			  }

			  // Save OOB samples
			  for (j = 0; j < inbag_counts.size(); ++j) {
			    if (inbag_counts.get(j) == 0) {
			      oob_sampleIDs.add(j);
			    }
			  }
			  num_samples_oob = oob_sampleIDs.size();

			  if (!keep_inbag) {
			    inbag_counts.clear();
			  }
			}
		
		public void bootstrapWithoutReplacementClassWise() {
			  int i;
			  // Number of samples is sum of sample fraction * number of samples
			  //int num_samples_inbag = 0;
			  //double sum_sample_fraction = 0;
			  //for (i = 0; i < sample_fraction.size(); i++) {
				//double s = sample_fraction.get(i);
			    //num_samples_inbag += (int) (num_samples * s);
			    //sum_sample_fraction += s;
			  //}

			  // Draw samples for each class
			  for (i = 0; i < sample_fraction.size(); ++i) {
			    int num_samples_class = sampleIDs_per_class.get(i).size();
			    int num_samples_inbag_class = (int) (num_samples * sample_fraction.get(i));

			    shuffleAndSplitAppend(sampleIDs.get(0), oob_sampleIDs, num_samples_class, num_samples_inbag_class,
			        sampleIDs_per_class.get(i), random);
			  }

			  if (keep_inbag) {
			    // All observation are 0 or 1 times inbag
				int insize = inbag_counts.size();
				inbag_counts.setSize(num_samples);
				for (i = insize; i < num_samples; i++) {
					inbag_counts.set(i,1);
				}
			    for (i = 0; i < oob_sampleIDs.size(); i++) {
			      inbag_counts.set(oob_sampleIDs.get(i), 0);
			    }
			  }
			}

	} // private class TreeProbability
	
	private class TreeRegression extends Tree {
		private int counter[];
		private double sums[];
		
		public TreeRegression() {
			super();
			counter = null;
			sums = null;
		}
		
		public TreeRegression(Vector<Vector<Integer>> child_nodeIDs, Vector<Integer> split_varIDs, Vector<Double> split_values) {
			super(child_nodeIDs, split_varIDs, split_values);
			counter = null;
			sums = null;
		}
		
		public int getPredictionTerminalNodeID(int sampleID) {
		    return prediction_terminal_nodeIDs.get(sampleID);
		}

		public double getPrediction(int sampleID) {
		    int terminal_nodeID = prediction_terminal_nodeIDs.get(sampleID);
		    return (split_values.get(terminal_nodeID));
		}

		public void allocateMemory() {
			// Init counters if not in memory efficient mode
			  if (!memory_saving_splitting) {
			    int max_num_splits = data.getMaxNumUniqueValues();

			    // Use number of random splits for extratrees
			    if (splitrule == SplitRule.EXTRATREES && num_random_splits > max_num_splits) {
			      max_num_splits = num_random_splits;
			    }

			    counter = new int[max_num_splits];
			    sums = new double[max_num_splits];
			  }
	
		}
		
		public double estimate(int nodeID) {

			// Mean of responses of samples in node
			  double sum_responses_in_node = 0;
			  int num_samples_in_node = sampleIDs.get(nodeID).size();
			  for (int i = 0; i < sampleIDs.get(nodeID).size(); ++i) {
			    sum_responses_in_node += data.get(sampleIDs.get(nodeID).get(i), dependent_varID);
			  }
			  return (sum_responses_in_node / (double) num_samples_in_node);
			}
		
		public void appendToFileInternal(BufferedWriter bw) {
			// Empty on purpose
	    } 

		public boolean splitNodeInternal(int nodeID, Vector<Integer> possible_split_varIDs) {

			  // Check node size, stop if maximum reached
			  if (sampleIDs.get(nodeID).size() <= min_node_size) {
			    split_values.set(nodeID, estimate(nodeID));
			    return true;
			  }

			  // Check if node is pure and set split_value to estimate and stop if pure
			  boolean pure = true;
			  double pure_value = 0;
			  for (int i = 0; i < sampleIDs.get(nodeID).size(); ++i) {
			    double value = data.get(sampleIDs.get(nodeID).get(i), dependent_varID);
			    if (i != 0 && value != pure_value) {
			      pure = false;
			      break;
			    }
			    pure_value = value;
			  }
			  if (pure) {
			    split_values.set(nodeID, pure_value);
			    return true;
			  }

			  // Find best split, stop if no decrease of impurity
			  boolean stop;
			  if (splitrule == SplitRule.MAXSTAT) {
			    stop = findBestSplitMaxstat(nodeID, possible_split_varIDs);
			  } else if (splitrule == SplitRule.EXTRATREES) {
			    stop = findBestSplitExtraTrees(nodeID, possible_split_varIDs);
			  } else {
			    stop = findBestSplit(nodeID, possible_split_varIDs);
			  }

			  if (stop) {
			    split_values.set(nodeID, estimate(nodeID));
			    return true;
			  }

			  return false;
		}
		
        public void createEmptyNodeInternal() {
        	// Empty on purpose
        }
        
        public double computePredictionAccuracyInternal() {

        	  int num_predictions = prediction_terminal_nodeIDs.size();
        	  double sum_of_squares = 0;
        	  for (int i = 0; i < num_predictions; ++i) {
        	    int terminal_nodeID = prediction_terminal_nodeIDs.get(i);
        	    double predicted_value = split_values.get(terminal_nodeID);
        	    double real_value = data.get(oob_sampleIDs.get(i), dependent_varID);
        	    if (predicted_value != real_value) {
        	      sum_of_squares += (predicted_value - real_value) * (predicted_value - real_value);
        	    }
        	  }
        	  return (1.0 - sum_of_squares / (double) num_predictions);
        	}
        
        public boolean findBestSplit(int nodeID, Vector<Integer> possible_split_varIDs) {
              int i;
        	  int num_samples_node = sampleIDs.get(nodeID).size();
        	  double best_decrease[] = new double[]{-1.0};
        	  int best_varID[] = new int[]{0};
        	  double best_value[] = new double[]{0.0};

        	  // Compute sum of responses in node
        	  double sum_node = 0;
        	  for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
        		int sampleID = sampleIDs.get(nodeID).get(i);
        	    sum_node += data.get(sampleID, dependent_varID);
        	  }

        	  // For all possible split variables
        	  for (i = 0; i < possible_split_varIDs.size(); i++) {
        		 int varID = possible_split_varIDs.get(i);

        	    // Find best split value, if ordered consider all values as split values, else all 2-partitions
        	    if (data.isOrderedVariable(varID)) {

        	      // Use memory saving method if option set
        	      if (memory_saving_splitting) {
        	        findBestSplitValueSmallQ(nodeID, varID, sum_node, num_samples_node, best_value, best_varID, best_decrease);
        	      } else {
        	        // Use faster method for both cases
        	        double q = (double) num_samples_node / (double) data.getNumUniqueDataValues(varID);
        	        if (q < Q_THRESHOLD) {
        	          findBestSplitValueSmallQ(nodeID, varID, sum_node, num_samples_node, best_value, best_varID, best_decrease);
        	        } else {
        	          findBestSplitValueLargeQ(nodeID, varID, sum_node, num_samples_node, best_value, best_varID, best_decrease);
        	        }
        	      }
        	    } else {
        	      findBestSplitValueUnordered(nodeID, varID, sum_node, num_samples_node, best_value, best_varID, best_decrease);
        	    }
        	  }

        	// Stop if no good split found
        	  if (best_decrease[0] < 0) {
        	    return true;
        	  }

        	// Save best values
        	  split_varIDs.set(nodeID, best_varID[0]);
        	  split_values.set(nodeID, best_value[0]);

        	// Compute decrease of impurity for this node and add to variable importance if needed
        	  if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
        	    addImpurityImportance(nodeID, best_varID[0], best_decrease[0]);
        	  }
        	  return false;
        	}

        public void findBestSplitValueSmallQ(int nodeID, int varID, double sum_node, int num_samples_node,
        	    double best_value[], int best_varID[], double best_decrease[]) {
              int i, j;
        	  // Create possible split values
        	  Vector<Double> possible_split_values = new Vector<Double>();
        	  data.getAllValues(possible_split_values, sampleIDs.get(nodeID), varID);

        	  // Try next variable if all equal for this
        	  if (possible_split_values.size() < 2) {
        	    return;
        	  }

        	  // Initialize with 0 if not in memory efficient mode, use pre-allocated space
        	  // -1 because no split possible at largest value
        	  int num_splits = possible_split_values.size() - 1;
        	  double sums_right[];
        	  int n_right[];
        	  if (memory_saving_splitting) {
        	    sums_right = new double[num_splits];
        	    n_right = new int[num_splits];
        	  } else {
        	    sums_right = sums;
        	    n_right = counter;
        	    for (i = 0; i < num_splits; i++) {
        	    	sums_right[i] = 0.0;
        	    	n_right[i] = 0;
        	    }
        	  }

        	  // Sum in right child and possible split
        	  for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
        		int sampleID = sampleIDs.get(nodeID).get(j);
        	    double value = data.get(sampleID, varID);
        	    double response = data.get(sampleID, dependent_varID);

        	    // Count samples until split_value reached
        	    for (i = 0; i < num_splits; ++i) {
        	      if (value > possible_split_values.get(i)) {
        	        ++n_right[i];
        	        sums_right[i] += response;
        	      } else {
        	        break;
        	      }
        	    }
        	  }

        	  // Compute decrease of impurity for each possible split
        	  for (i = 0; i < num_splits; ++i) {

        	    // Stop if one child empty
        	    int n_left = num_samples_node - n_right[i];
        	    if (n_left == 0 || n_right[i] == 0) {
        	      continue;
        	    }

        	    double sum_right = sums_right[i];
        	    double sum_left = sum_node - sum_right;
        	    double decrease = sum_left * sum_left / (double) n_left + sum_right * sum_right / (double) n_right[i];

        	    // If better than before, use this
        	    if (decrease > best_decrease[0]) {
        	      best_value[0] = (possible_split_values.get(i) + possible_split_values.get(i + 1)) / 2;
        	      best_varID[0] = varID;
        	      best_decrease[0] = decrease;

        	      // Use smaller value if average is numerically the same as the larger value
        	      if (best_value[0] == possible_split_values.get(i + 1)) {
        	        best_value[0] = possible_split_values.get(i);
        	      }
        	    }
        	  }

        	  if (memory_saving_splitting) {
        	    sums_right = null;
        	    n_right = null;
        	  }
        	}

        public void findBestSplitValueLargeQ(int nodeID, int varID, double sum_node, int num_samples_node,
        	    double best_value[], int best_varID[], double best_decrease[]) {
              int i,j;
        	  // Set counters to 0
        	  int num_unique = data.getNumUniqueDataValues(varID);
        	  for (i = 0; i < num_unique; i++) {
        		  counter[i] = 0;
        		  sums[i] = 0.0;
        	  }

              for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
            	int sampleID = sampleIDs.get(nodeID).get(i);
        	    int index = data.getIndex(sampleID, varID);

        	    sums[index] += data.get(sampleID, dependent_varID);
        	    ++counter[index];
        	  }

        	  int n_left = 0;
        	  double sum_left = 0;

        	  // Compute decrease of impurity for each split
        	  for (i = 0; i < num_unique - 1; ++i) {

        	    // Stop if nothing here
        	    if (counter[i] == 0) {
        	      continue;
        	    }

        	    n_left += counter[i];
        	    sum_left += sums[i];

        	    // Stop if right child empty
        	    int n_right = num_samples_node - n_left;
        	    if (n_right == 0) {
        	      break;
        	    }

        	    double sum_right = sum_node - sum_left;
        	    double decrease = sum_left * sum_left / (double) n_left + sum_right * sum_right / (double) n_right;

        	    // If better than before, use this
        	    if (decrease > best_decrease[0]) {
        	      // Find next value in this node
        	      j = i + 1;
        	      while (j < num_unique && counter[j] == 0) {
        	        ++j;
        	      }

        	      // Use mid-point split
        	      best_value[0] = (data.getUniqueDataValue(varID, i) + data.getUniqueDataValue(varID, j)) / 2;
        	      best_varID[0] = varID;
        	      best_decrease[0] = decrease;

        	      // Use smaller value if average is numerically the same as the larger value
        	      if (best_value[0] == data.getUniqueDataValue(varID, j)) {
        	        best_value[0] = data.getUniqueDataValue(varID, i);
        	      }
        	    }
        	  }
        	}

        public void findBestSplitValueUnordered(int nodeID, int varID, double sum_node, int num_samples_node,
        	    double best_value[], int best_varID[], double best_decrease[]) {
              int j;
        	// Create possible split values
        	  Vector<Double> factor_levels = new Vector<Double>();
        	  data.getAllValues(factor_levels, sampleIDs.get(nodeID), varID);

        	// Try next variable if all equal for this
        	  if (factor_levels.size() < 2) {
        	    return;
        	  }

        	// Number of possible splits is 2^num_levels
        	  int num_splits = (1 << factor_levels.size());

        	// Compute decrease of impurity for each possible split
        	// Split where all left (0) or all right (1) are excluded
        	// The second half of numbers is just left/right switched the first half -> Exclude second half
        	  for (int local_splitID = 1; local_splitID < num_splits / 2; ++local_splitID) {

        	    // Compute overall splitID by shifting local factorIDs to global positions
        	    int splitID = 0;
        	    for (j = 0; j < factor_levels.size(); ++j) {
        	      if ((local_splitID & (1 << j)) != 0) {
        	        double level = factor_levels.get(j);
        	        int factorID = (int)Math.floor(level) - 1;
        	        splitID = splitID | (1 << factorID);
        	      }
        	    }

        	    // Initialize
        	    double sum_right = 0;
        	    int n_right = 0;

        	    // Sum in right child
        	    for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
        	      int sampleID = sampleIDs.get(nodeID).get(j);
        	      double response = data.get(sampleID, dependent_varID);
        	      double value = data.get(sampleID, varID);
        	      int factorID = (int)Math.floor(value) - 1;

        	      // If in right child, count
        	      // In right child, if bitwise splitID at position factorID is 1
        	      if ((splitID & (1 << factorID)) != 0) {
        	        ++n_right;
        	        sum_right += response;
        	      }
        	    }
        	    int n_left = num_samples_node - n_right;

        	    // Sum of squares
        	    double sum_left = sum_node - sum_right;
        	    double decrease = sum_left * sum_left / (double) n_left + sum_right * sum_right / (double) n_right;

        	    // If better than before, use this
        	    if (decrease > best_decrease[0]) {
        	      best_value[0] = splitID;
        	      best_varID[0] = varID;
        	      best_decrease[0] = decrease;
        	    }
        	  }
        	}

        public boolean findBestSplitMaxstat(int nodeID, Vector<Integer> possible_split_varIDs) {
              int i,j;
        	  int num_samples_node = sampleIDs.get(nodeID).size();

        	  // Compute ranks
        	  Vector<Double> response = new Vector<Double>();
        	  response.ensureCapacity(num_samples_node);
        	  for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
        		int sampleID = sampleIDs.get(nodeID).get(i);
        	    response.add(data.get(sampleID, dependent_varID));
        	  }
        	  Vector<Double> ranks = new Vector<Double>();
        	  ranks = rank(response);

        	  // Save split stats
        	  Vector<Double> pvalues = new Vector<Double>();
        	  pvalues.ensureCapacity(possible_split_varIDs.size());
        	  Vector<Double> values = new Vector<Double>();
        	  values.ensureCapacity(possible_split_varIDs.size());
        	  Vector<Integer> candidate_varIDs = new Vector<Integer>(); // Change from Vector<Double>
        	  candidate_varIDs.ensureCapacity(possible_split_varIDs.size());

        	  // Compute p-values
              for (i = 0; i < possible_split_varIDs.size(); i++) {
            	 int varID = possible_split_varIDs.get(i);

        	    // Get all observations
        	    Vector<Double> x = new Vector<Double>();
        	    x.ensureCapacity(num_samples_node);
        	    for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
        	      int sampleID = sampleIDs.get(nodeID).get(j);
        	      x.add(data.get(sampleID, varID));
        	    }

        	    // Order by x
        	    Vector<Integer> indices = order(x, false);
        	    //std::vector<size_t> indices = orderInData(data, sampleIDs[nodeID], varID, false);

        	    // Compute maximally selected rank statistics
        	    double best_maxstat[] = new double[1];
        	    double best_split_value[] = new double[1];
        	    maxstat(ranks, x, indices, best_maxstat, best_split_value, minprop, 1 - minprop);
        	    //maxstatInData(scores, data, sampleIDs[nodeID], varID, indices, best_maxstat, best_split_value, minprop, 1 - minprop);

        	    if (best_maxstat[0] > -1) {
        	      // Compute number of samples left of cutpoints
        	      Vector<Integer> num_samples_left = numSamplesLeftOfCutpoint(x, indices);
        	      //std::vector<size_t> num_samples_left = numSamplesLeftOfCutpointInData(data, sampleIDs[nodeID], varID, indices);

        	      // Compute p-values
        	      double pvalue_lau92 = maxstatPValueLau92(best_maxstat[0], minprop, 1 - minprop);
        	      double pvalue_lau94 = maxstatPValueLau94(best_maxstat[0], minprop, 1 - minprop, num_samples_node, num_samples_left);

        	      // Use minimum of Lau92 and Lau94
        	      double pvalue = Math.min(pvalue_lau92, pvalue_lau94);

        	      // Save split stats
        	      pvalues.add(pvalue);
        	      values.add(best_split_value[0]);
        	      candidate_varIDs.add(varID);
        	    }
        	  }

        	  double adjusted_best_pvalue = Double.MAX_VALUE;
        	  int best_varID = 0;
        	  double best_value = 0;

        	  if (pvalues.size() > 0) {
        	    // Adjust p-values with Benjamini/Hochberg
        	    Vector<Double> adjusted_pvalues = adjustPvalues(pvalues);

        	    // Use smallest p-value
        	    double min_pvalue = Double.MAX_VALUE;
        	    for (i = 0; i < pvalues.size(); ++i) {
        	      if (pvalues.get(i) < min_pvalue) {
        	        min_pvalue = pvalues.get(i);
        	        best_varID = candidate_varIDs.get(i);
        	        best_value = values.get(i);
        	        adjusted_best_pvalue = adjusted_pvalues.get(i);
        	      }
        	    }
        	  }

        	  // Stop if no good split found (this is terminal node).
        	  if (adjusted_best_pvalue > alpha) {
        	    return true;
        	  } else {
        	    // If not terminal node save best values
        	    split_varIDs.set(nodeID, best_varID);
        	    split_values.set(nodeID, best_value);
        	    return false;
        	  }
        	}

        public boolean findBestSplitExtraTrees(int nodeID, Vector<Integer> possible_split_varIDs) {
              int i;
        	  int num_samples_node = sampleIDs.get(nodeID).size();
        	  double best_decrease[] = new double[]{-1.0};
        	  int best_varID[] = new int[]{0};
        	  double best_value[] = new double[]{0.0};

        	  // Compute sum of responses in node
        	  double sum_node = 0;
              for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
            	int sampleID = sampleIDs.get(nodeID).get(i);
        	    sum_node += data.get(sampleID, dependent_varID);
        	  }

        	  // For all possible split variables
        	  for (i = 0; i < possible_split_varIDs.size(); i++) {
        		int varID = possible_split_varIDs.get(i);

        	    // Find best split value, if ordered consider all values as split values, else all 2-partitions
        	    if (data.isOrderedVariable(varID)) {
        	      findBestSplitValueExtraTrees(nodeID, varID, sum_node, num_samples_node, best_value, best_varID, best_decrease);
        	    } else {
        	      findBestSplitValueExtraTreesUnordered(nodeID, varID, sum_node, num_samples_node, best_value, best_varID,
        	          best_decrease);
        	    }
        	  }

        	  // Stop if no good split found
        	  if (best_decrease[0] < 0) {
        	    return true;
        	  }

        	  // Save best values
        	  split_varIDs.set(nodeID, best_varID[0]);
        	  split_values.set(nodeID, best_value[0]);

        	  // Compute decrease of impurity for this node and add to variable importance if needed
        	  if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
        	    addImpurityImportance(nodeID, best_varID[0], best_decrease[0]);
        	  }
        	  return false;
        	}

        	public void findBestSplitValueExtraTrees(int nodeID, int varID, double sum_node, int num_samples_node,
        	    double best_value[], int best_varID[], double best_decrease[]) {
              int i, j;
        	  // Get min/max values of covariate in node
        	  double min[] = new double[1];
        	  double max[] = new double[1];
        	  data.getMinMaxValues(min, max, sampleIDs.get(nodeID), varID);

        	  // Try next variable if all equal for this
        	  if (min[0] == max[0]) {
        	    return;
        	  }

        	  // Create possible split values: Draw randomly between min and max
        	  Vector<Double> possible_split_values = new Vector<Double>();
        	  possible_split_values.ensureCapacity(num_random_splits);
        	  Random random = new Random();
        	  for (i = 0; i < num_random_splits; ++i) {
        		double dval = (max[0] - min[0])*random.nextDouble() + min[0];
        	    possible_split_values.add(dval);
        	  }

        	  // Initialize with 0m if not in memory efficient mode, use pre-allocated space
        	  int num_splits = possible_split_values.size();
        	  double sums_right[];
        	  int n_right[];
        	  if (memory_saving_splitting) {
        	    sums_right = new double[num_splits];
        	    n_right = new int[num_splits];
        	  } else {
        	    sums_right = sums;
        	    n_right = counter;
        	    for (i = 0; i < num_splits; i++) {
        	    	sums_right[i] = 0.0;
        	    	n_right[i] = 0;
        	    }
        	  }

        	  // Sum in right child and possible split
        	  for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
        		 int sampleID = sampleIDs.get(nodeID).get(j);
        	    double value = data.get(sampleID, varID);
        	    double response = data.get(sampleID, dependent_varID);

        	    // Count samples until split_value reached
        	    for (i = 0; i < num_splits; ++i) {
        	      if (value > possible_split_values.get(i)) {
        	        ++n_right[i];
        	        sums_right[i] += response;
        	      } else {
        	        break;
        	      }
        	    }
        	  }

        	  // Compute decrease of impurity for each possible split
        	  for (i = 0; i < num_splits; ++i) {

        	    // Stop if one child empty
        	    int n_left = num_samples_node - n_right[i];
        	    if (n_left == 0 || n_right[i] == 0) {
        	      continue;
        	    }

        	    double sum_right = sums_right[i];
        	    double sum_left = sum_node - sum_right;
        	    double decrease = sum_left * sum_left / (double) n_left + sum_right * sum_right / (double) n_right[i];

        	    // If better than before, use this
        	    if (decrease > best_decrease[0]) {
        	      best_value[0] = possible_split_values.get(i);
        	      best_varID[0] = varID;
        	      best_decrease[0] = decrease;
        	    }
        	  }

        	  if (memory_saving_splitting) {
        	    sums_right = null;
        	    n_right = null;
        	  }
        	}

        	public void findBestSplitValueExtraTreesUnordered(int nodeID, int varID, double sum_node,
        		  int num_samples_node, double best_value[], int best_varID[], double best_decrease[]) {
                  int i, j;
        		  int num_unique_values = data.getNumUniqueDataValues(varID);

        		  // Get all factor indices in node
        		  Vector<Boolean> factor_in_node = new Vector<Boolean>();
        		  for (i = 0; i < num_unique_values; i++) {
        			  factor_in_node.add(false);
        		  }
        		  for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
        			int sampleID = sampleIDs.get(nodeID).get(i);
        		    int index = data.getIndex(sampleID, varID);
        		    factor_in_node.set(index, true);
        		  }

        		  // Vector of indices in and out of node
        		  Vector<Integer> indices_in_node = new Vector<Integer>();
        		  Vector<Integer> indices_out_node = new Vector<Integer>();
        		  indices_in_node.ensureCapacity(num_unique_values);
        		  indices_out_node.ensureCapacity(num_unique_values);
        		  for (i = 0; i < num_unique_values; ++i) {
        		    if (factor_in_node.get(i)) {
        		      indices_in_node.add(i);
        		    } else {
        		      indices_out_node.add(i);
        		    }
        		  }

        		  // Generate num_random_splits splits
        		  Random random = new Random();
        		  for (i = 0; i < num_random_splits; ++i) {
        		    Vector<Integer> split_subset = new Vector<Integer>();
        		    split_subset.ensureCapacity(num_unique_values);

        		    // Draw random subsets, sample all partitions with equal probability
        		    if (indices_in_node.size() > 1) {
        		      int num_partitions = (2 << (indices_in_node.size() - 1)) - 2; // 2^n-2 (don't allow full or empty)
        		      int splitID_in_node = random.nextInt(num_partitions) + 1;
        		      for (j = 0; j < indices_in_node.size(); ++j) {
        		        if ((splitID_in_node & (1 << j)) > 0) {
        		          split_subset.add(indices_in_node.get(j));
        		        }
        		      }
        		    }
        		    if (indices_out_node.size() > 1) {
        		      int num_partitions = (2 << (indices_out_node.size() - 1)) - 1; // 2^n-1 (allow full or empty)
        		      int splitID_out_node = random.nextInt(num_partitions+1);
        		      for (j = 0; j < indices_out_node.size(); ++j) {
        		        if ((splitID_out_node & (1 << j)) > 0) {
        		          split_subset.add(indices_out_node.get(j));
        		        }
        		      }
        		    }

        		    // Assign union of the two subsets to right child
        		    int splitID = 0;
        		     for (j = 0; j < split_subset.size(); j++) {
        		      int idx = split_subset.get(j);
        		      splitID |= 1 << idx;
        		    }

        		    // Initialize
        		    double sum_right = 0;
        		    int n_right = 0;

        		    // Sum in right child
        		    for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
        		      int sampleID = sampleIDs.get(nodeID).get(j);
        		      double response = data.get(sampleID, dependent_varID);
        		      double value = data.get(sampleID, varID);
        		      int factorID = (int)Math.floor(value) - 1;

        		      // If in right child, count
        		      // In right child, if bitwise splitID at position factorID is 1
        		      if ((splitID & (1 << factorID)) != 0) {
        		        ++n_right;
        		        sum_right += response;
        		      }
        		    }
        		    int n_left = num_samples_node - n_right;

        		    // Sum of squares
        		    double sum_left = sum_node - sum_right;
        		    double decrease = sum_left * sum_left / (double) n_left + sum_right * sum_right / (double) n_right;

        		    // If better than before, use this
        		    if (decrease > best_decrease[0]) {
        		      best_value[0] = splitID;
        		      best_varID[0] = varID;
        		      best_decrease[0] = decrease;
        		    }
        		  }
        		}
        
        	public void addImpurityImportance(int nodeID, int varID, double decrease) {
                  int i;
        		  double sum_node = 0;
        		  for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
        			int sampleID = sampleIDs.get(nodeID).get(i);
        		    sum_node += data.get(sampleID, dependent_varID);
        		  }
        		  double best_decrease = decrease - sum_node * sum_node / (double) sampleIDs.get(nodeID).size();

        		  // No variable importance for no split variables
        		  int tempvarID = data.getUnpermutedVarID(varID);
        		  for ( i = 0; i < data.getNoSplitVariables().size(); i++) {
        			int skip = data.getNoSplitVariables().get(i);
        		    if (tempvarID >= skip) {
        		      --tempvarID;
        		    }
        		  }

        		  // Subtract if corrected importance and permuted variable, else add
        		  double val = variable_importance.get(tempvarID);
        		  if (importance_mode == ImportanceMode.IMP_GINI_CORRECTED && varID >= data.getNumCols()) {
        			variable_importance.set(tempvarID, val - best_decrease);
        		  } else {
          			variable_importance.set(tempvarID, val + best_decrease);
        		  }
        		}

        	public void bootstrapClassWise() {
        		
        	}
        	
        	public void bootstrapWithoutReplacementClassWise() {
        		
        	}
        	
        public void cleanUpInternal() {
			if (counter != null) {
				counter = null;
			}
			if (sums != null) {
				sums = null;
			}
		}
	} // private class TreeRegression
	
	private class TreeSurvival extends Tree {
		private int status_varID;
		
		// Unique time points for all individuals (not only this bootstrap), sorted
		private Vector<Double> unique_timepoints;
		private int num_timepoints;
		private Vector<Integer> response_timepointIDs;
		
		// For all terminal nodes CHF for all unique timepoints.  For other nodes empty vector.
		private Vector<Vector<Double>> chf;
		
		// Fields to save to while tree growing
		private int num_deaths[];
		private int num_samples_at_risk[];
		
		public TreeSurvival(Vector<Double> unique_timepoints, int status_varID, Vector<Integer> response_timepoints) {
			super();
			this.status_varID = status_varID;
			this.unique_timepoints = new Vector<Double>();
			this.unique_timepoints.addAll(unique_timepoints);
			this.response_timepointIDs = new Vector<Integer>();
			this.response_timepointIDs.addAll(response_timepointIDs);
			num_deaths = null;
			num_samples_at_risk = null;
			num_timepoints = this.unique_timepoints.size();
		}
		
		public TreeSurvival(Vector<Vector<Integer>> child_nodeIDs, Vector<Integer> split_varIDs, Vector<Double> split_values,
				Vector<Vector<Double>> chf, Vector<Double> unique_timepoints, Vector<Integer> response_timepointIDs) {
			super(child_nodeIDs, split_varIDs, split_values);
			status_varID = 0;
			this.unique_timepoints = new Vector<Double>();
			this.unique_timepoints.addAll(unique_timepoints);
			this.response_timepointIDs = new Vector<Integer>();
			this.response_timepointIDs.addAll(response_timepointIDs);
			this.chf = new Vector<Vector<Double>>();
			this.chf.addAll(chf);
			num_deaths = null;
			num_samples_at_risk = null;
			num_timepoints = this.unique_timepoints.size();
		}
		
		public Vector<Vector<Double>> getChf() {
			return chf;
		}
		
		public Vector<Double> getPrediction(int sampleID) {
			int terminal_nodeID = prediction_terminal_nodeIDs.get(sampleID);
			return chf.get(terminal_nodeID);
		}
		
		public int getPredictionTerminalNodeID(int sampleID) {
			return prediction_terminal_nodeIDs.get(sampleID);
		}
		
		public void allocateMemory() {
			  // Number of deaths and samples at risk for each timepoint
			  num_deaths = new int[num_timepoints];
			  num_samples_at_risk = new int[num_timepoints];
		}
		
		public void appendToFileInternal(BufferedWriter bw) {

			  // Convert to vector without empty elements and save
			  Vector<Integer> terminal_nodes = new Vector<Integer>();
			  Vector<Vector<Double>> chf_vector = new Vector<Vector<Double>>();
			  for (int i = 0; i < chf.size(); ++i) {
			    if (!chf.get(i).isEmpty()) {
			      terminal_nodes.add(i);
			      chf_vector.add(chf.get(i));
			    }
			  }
			  saveVector1D(terminal_nodes, bw);
			  saveDVector2D(chf_vector, bw);
		} 

		public void createEmptyNodeInternal() {
			  chf.add(new Vector<Double>());
		}

		public void computeSurvival(int nodeID) {
			  Vector<Double> chf_temp = new Vector<Double>();
			  chf_temp.ensureCapacity(num_timepoints);
			  double chf_value = 0;
			  for (int i = 0; i < num_timepoints; ++i) {
			    if (num_samples_at_risk[i] != 0) {
			      chf_value += (double) num_deaths[i] / (double) num_samples_at_risk[i];
			    }
			    chf_temp.add(chf_value);
			  }
			  chf.set(nodeID, chf_temp);
		}
		
		public double computePredictionAccuracyInternal() {

			  // Compute summed chf for samples
			  Vector<Double> sum_chf = new Vector<Double>();
			  for (int i = 0; i < prediction_terminal_nodeIDs.size(); ++i) {
			    int terminal_nodeID = prediction_terminal_nodeIDs.get(i);
			    double accumulate = 0.0;
			    for (int j = 0; j < chf.get(terminal_nodeID).size(); j++) {
			    	accumulate += chf.get(terminal_nodeID).get(j);
			    }
			    sum_chf.add(accumulate);
		      }

			  // Return concordance index
			  return computeConcordanceIndex(data, sum_chf, dependent_varID, status_varID, oob_sampleIDs);
		}

		public boolean splitNodeInternal(int nodeID, Vector<Integer> possible_split_varIDs) {

			  if (splitrule == SplitRule.MAXSTAT) {
			    return findBestSplitMaxstat(nodeID, possible_split_varIDs);
			  } else if (splitrule == SplitRule.EXTRATREES) {
			    return findBestSplitExtraTrees(nodeID, possible_split_varIDs);
			  } else {
			    return findBestSplit(nodeID, possible_split_varIDs);
			  }
		}

		public boolean findBestSplit(int nodeID, Vector<Integer> possible_split_varIDs) {
              int i;
			  double best_decrease[] = new double[]{-1.0};
			  int num_samples_node = sampleIDs.get(nodeID).size();
			  int best_varID[] = new int[]{0};
			  double best_value[] = new double[]{0.0};

			  computeDeathCounts(nodeID);

			  // Stop early if no split posssible
			  if (num_samples_node >= 2 * min_node_size) {

			    // For all possible split variables
			    for (i = 0; i < possible_split_varIDs.size(); i++) {
			    	int varID = possible_split_varIDs.get(i);

			      // Find best split value, if ordered consider all values as split values, else all 2-partitions
			      if (data.isOrderedVariable(varID)) {
			        if (splitrule == SplitRule.LOGRANK) {
			          findBestSplitValueLogRank(nodeID, varID, best_value, best_varID, best_decrease);
			        } else if (splitrule == SplitRule.AUC || splitrule == SplitRule.AUC_IGNORE_TIES) {
			          findBestSplitValueAUC(nodeID, varID, best_value, best_varID, best_decrease);
			        }
			      } else {
			        findBestSplitValueLogRankUnordered(nodeID, varID, best_value, best_varID, best_decrease);
			      }

			    }
			  }

			  // Stop and save CHF if no good split found (this is terminal node).
			  if (best_decrease[0] < 0) {
			    computeSurvival(nodeID);
			    return true;
			  } else {
			    // If not terminal node save best values
			    split_varIDs.set(nodeID, best_varID[0]);
			    split_values.set(nodeID, best_value[0]);

			    // Compute decrease of impurity for this node and add to variable importance if needed
			    if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
			      addImpurityImportance(nodeID, best_varID[0], best_decrease[0]);
			    }

			    return false;
			  }
		}

		public boolean findBestSplitMaxstat(int nodeID, Vector<Integer> possible_split_varIDs) {
              int i,j;
			  int num_samples_node = sampleIDs.get(nodeID).size();

			  // Check node size, stop if maximum reached
			  if (num_samples_node <= min_node_size) {
			    computeDeathCounts(nodeID);
			    computeSurvival(nodeID);
			    return true;
			  }

			  // Compute scores
			  Vector<Double> time = new Vector<Double>();
			  time.ensureCapacity(num_samples_node);
			  Vector<Double> status = new Vector<Double>();
			  status.ensureCapacity(num_samples_node);
			  for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
				int sampleID = sampleIDs.get(nodeID).get(i);
			    time.add(data.get(sampleID, dependent_varID));
			    status.add(data.get(sampleID, status_varID));
			  }
			  Vector<Double> scores = logrankScores(time, status);
			  //std::vector<double> scores = logrankScoresData(data, dependent_varID, status_varID, sampleIDs[nodeID]);

			  // Save split stats
			  Vector<Double> pvalues = new Vector<Double>();
			  pvalues.ensureCapacity(possible_split_varIDs.size());
			  Vector<Double> values = new Vector<Double>();
			  values.ensureCapacity(possible_split_varIDs.size());
			  Vector<Integer> candidate_varIDs = new Vector<Integer>(); // Change from Vector<Double>
			  candidate_varIDs.ensureCapacity(possible_split_varIDs.size());

			  // Compute p-values
			  for (i = 0; i < possible_split_varIDs.size(); i++) {
				int varID = possible_split_varIDs.get(i);

			    // Get all observations
			    Vector<Double> x = new Vector<Double>();
			    x.ensureCapacity(num_samples_node);
			    for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
			      int sampleID = sampleIDs.get(nodeID).get(j);
			      x.add(data.get(sampleID, varID));
			    }

			    // Order by x
			    Vector<Integer> indices = order(x, false);
			    //std::vector<size_t> indices = orderInData(data, sampleIDs[nodeID], varID, false);

			    // Compute maximally selected rank statistics
			    double best_maxstat[] = new double[1];
			    double best_split_value[] = new double[1];
			    maxstat(scores, x, indices, best_maxstat, best_split_value, minprop, 1 - minprop);
			    //maxstatInData(scores, data, sampleIDs[nodeID], varID, indices, best_maxstat, best_split_value, minprop, 1 - minprop);

			    if (best_maxstat[0] > -1) {
			      // Compute number of samples left of cutpoints
			      Vector<Integer> num_samples_left = numSamplesLeftOfCutpoint(x, indices);
			      //std::vector<size_t> num_samples_left = numSamplesLeftOfCutpointInData(data, sampleIDs[nodeID], varID, indices);

			      // Remove largest cutpoint (all observations left)
			      num_samples_left.removeElementAt(num_samples_left.size()-1);

			      // Use unadjusted p-value if only 1 split point
			      double pvalue;
			      if (num_samples_left.size() == 1) {
			        pvalue = maxstatPValueUnadjusted(best_maxstat[0]);
			      } else {
			        // Compute p-values
			        double pvalue_lau92 = maxstatPValueLau92(best_maxstat[0], minprop, 1 - minprop);
			        double pvalue_lau94 = maxstatPValueLau94(best_maxstat[0], minprop, 1 - minprop, num_samples_node,
			            num_samples_left);

			        // Use minimum of Lau92 and Lau94
			        pvalue = Math.min(pvalue_lau92, pvalue_lau94);
			      }

			      // Save split stats
			      pvalues.add(pvalue);
			      values.add(best_split_value[0]);
			      candidate_varIDs.add(varID);
			    }
			  }

			  double adjusted_best_pvalue = Double.MAX_VALUE;
			  int best_varID = 0;
			  double best_value = 0;

			  if (pvalues.size() > 0) {
			    // Adjust p-values with Benjamini/Hochberg
			    Vector<Double> adjusted_pvalues = adjustPvalues(pvalues);

			    double min_pvalue = Double.MAX_VALUE;
			    for (i = 0; i < pvalues.size(); ++i) {
			      if (pvalues.get(i) < min_pvalue) {
			        min_pvalue = pvalues.get(i);
			        best_varID = candidate_varIDs.get(i);
			        best_value = values.get(i);
			        adjusted_best_pvalue = adjusted_pvalues.get(i);
			      }
			    }
			  }

			  // Stop and save CHF if no good split found (this is terminal node).
			  if (adjusted_best_pvalue > alpha) {
			    computeDeathCounts(nodeID);
			    computeSurvival(nodeID);
			    return true;
			  } else {
			    // If not terminal node save best values
			    split_varIDs.set(nodeID, best_varID);
			    split_values.set(nodeID, best_value);
			    return false;
			  }
		}
		
		public void computeDeathCounts(int nodeID) {
              int i;
			  // Initialize
			  for (i = 0; i < num_timepoints; ++i) {
			    num_deaths[i] = 0;
			    num_samples_at_risk[i] = 0;
			  }

		      for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
		    	int sampleID = sampleIDs.get(nodeID).get(i);
			    double survival_time = data.get(sampleID, dependent_varID);

			    int t = 0;
			    while (t < num_timepoints && unique_timepoints.get(t) < survival_time) {
			      ++num_samples_at_risk[t];
			      ++t;
			    }

			    // Now t is the survival time, add to at risk and to death if death
			    if (t < num_timepoints) {
			      ++num_samples_at_risk[t];
			      if (data.get(sampleID, status_varID) == 1) {
			        ++num_deaths[t];
			      }
			    }
			  }
		}
		
		public void computeChildDeathCounts(int nodeID, int varID, Vector<Double> possible_split_values,
			    int num_samples_right_child[], int delta_samples_at_risk_right_child[], int num_deaths_right_child[],
			    int num_splits) {
              int i,j;
			  // Count deaths in right child per timepoint and possible split
		      for (j = 0; j < sampleIDs.get(nodeID).size(); j++) {
		    	int sampleID = sampleIDs.get(nodeID).get(j);
			    double value = data.get(sampleID, varID);
			    int survival_timeID = response_timepointIDs.get(sampleID);

			    // Count deaths until split_value reached
			    for (i = 0; i < num_splits; ++i) {

			      if (value > possible_split_values.get(i)) {
			        ++num_samples_right_child[i];
			        ++delta_samples_at_risk_right_child[i * num_timepoints + survival_timeID];
			        if (data.get(sampleID, status_varID) == 1) {
			          ++num_deaths_right_child[i * num_timepoints + survival_timeID];
			        }
			      } else {
			        break;
			      }
			    }
			  }
		}

		public void findBestSplitValueLogRank(int nodeID, int varID, double best_value[], int best_varID[],
			    double best_logrank[]) {
              int i;
			  // Create possible split values
			  Vector<Double> possible_split_values = new Vector<Double>();
			  data.getAllValues(possible_split_values, sampleIDs.get(nodeID), varID);

			  // Try next variable if all equal for this
			  if (possible_split_values.size() < 2) {
			    return;
			  }

			  // -1 because no split possible at largest value
			  int num_splits = possible_split_values.size() - 1;

			  // Initialize
			  int num_deaths_right_child[] = new int[num_splits * num_timepoints];
			  int delta_samples_at_risk_right_child[] = new int[num_splits * num_timepoints];
			  int num_samples_right_child[] = new int[num_splits];

			  computeChildDeathCounts(nodeID, varID, possible_split_values, num_samples_right_child,
			      delta_samples_at_risk_right_child, num_deaths_right_child, num_splits);

			  // Compute logrank test for all splits and use best
			  for (i = 0; i < num_splits; ++i) {
			    double numerator = 0;
			    double denominator_squared = 0;

			    // Stop if minimal node size reached
			    int num_samples_left_child = sampleIDs.get(nodeID).size() - num_samples_right_child[i];
			    if (num_samples_right_child[i] < min_node_size || num_samples_left_child < min_node_size) {
			      continue;
			    }

			    // Compute logrank test statistic for this split
			    int num_samples_at_risk_right_child = num_samples_right_child[i];
			    for (int t = 0; t < num_timepoints; ++t) {
			      if (num_samples_at_risk[t] < 2 || num_samples_at_risk_right_child < 1) {
			        break;
			      }

			      if (num_deaths[t] > 0) {
			        // Numerator and demoninator for log-rank test, notation from Ishwaran et al.
			        double di = (double) num_deaths[t];
			        double di1 = (double) num_deaths_right_child[i * num_timepoints + t];
			        double Yi = (double) num_samples_at_risk[t];
			        double Yi1 = (double) num_samples_at_risk_right_child;
			        numerator += di1 - Yi1 * (di / Yi);
			        denominator_squared += (Yi1 / Yi) * (1.0 - Yi1 / Yi) * ((Yi - di) / (Yi - 1)) * di;
			      }

			      // Reduce number of samples at risk for next timepoint
			      num_samples_at_risk_right_child -= delta_samples_at_risk_right_child[i * num_timepoints + t];

			    }
			    double logrank = -1;
			    if (denominator_squared != 0) {
			      logrank = Math.abs(numerator / Math.sqrt(denominator_squared));
			    }

			    if (logrank > best_logrank[0]) {
			      best_value[0] = (possible_split_values.get(i) + possible_split_values.get(i + 1)) / 2;
			      best_varID[0] = varID;
			      best_logrank[0] = logrank;

			      // Use smaller value if average is numerically the same as the larger value
			      if (best_value[0] == possible_split_values.get(i + 1)) {
			        best_value[0] = possible_split_values.get(i);
			      }
			    }
			  }

			  num_deaths_right_child = null;
			  delta_samples_at_risk_right_child = null;
			  num_samples_right_child = null;
		}

		public void findBestSplitValueLogRankUnordered(int nodeID, int varID, double best_value[],
			    int best_varID[], double best_logrank[]) {
              int i,j;
			  // Create possible split values
			  Vector<Double> factor_levels = new Vector<Double>();
			  data.getAllValues(factor_levels, sampleIDs.get(nodeID), varID);

			  // Try next variable if all equal for this
			  if (factor_levels.size() < 2) {
			    return;
			  }

			  // Number of possible splits is 2^num_levels
			  int num_splits = (1 << factor_levels.size());

			  // Compute logrank test statistic for each possible split
			  // Split where all left (0) or all right (1) are excluded
			  // The second half of numbers is just left/right switched the first half -> Exclude second half
			  for (int local_splitID = 1; local_splitID < num_splits / 2; ++local_splitID) {

			    // Compute overall splitID by shifting local factorIDs to global positions
			    int splitID = 0;
			    for (j = 0; j < factor_levels.size(); ++j) {
			      if ((local_splitID & (1 << j)) != 0) {
			        double level = factor_levels.get(j);
			        int factorID = (int)Math.floor(level) - 1;
			        splitID = splitID | (1 << factorID);
			      }
			    }

			    // Initialize
			    int num_deaths_right_child[] = new int[num_timepoints];
			    int delta_samples_at_risk_right_child[] = new int[num_timepoints];
			    int num_samples_right_child = 0;
			    double numerator = 0;
			    double denominator_squared = 0;

			    // Count deaths in right child per timepoint
			    for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
			      int sampleID = sampleIDs.get(nodeID).get(i);
			      int survival_timeID = response_timepointIDs.get(sampleID);
			      double value = data.get(sampleID, varID);
			      int factorID = (int)Math.floor(value) - 1;

			      // If in right child, count
			      // In right child, if bitwise splitID at position factorID is 1
			      if ((splitID & (1 << factorID)) != 0) {
			        ++num_samples_right_child;
			        ++delta_samples_at_risk_right_child[survival_timeID];
			        if (data.get(sampleID, status_varID) == 1) {
			          ++num_deaths_right_child[survival_timeID];
			        }
			      }

			    }

			    // Stop if minimal node size reached
			    int num_samples_left_child = sampleIDs.get(nodeID).size() - num_samples_right_child;
			    if (num_samples_right_child < min_node_size || num_samples_left_child < min_node_size) {
			      num_deaths_right_child = null;
			      delta_samples_at_risk_right_child = null;
			      continue;
			    }

			    // Compute logrank test statistic for this split
			    int num_samples_at_risk_right_child = num_samples_right_child;
			    for (int t = 0; t < num_timepoints; ++t) {
			      if (num_samples_at_risk[t] < 2 || num_samples_at_risk_right_child < 1) {
			        break;
			      }

			      if (num_deaths[t] > 0) {
			        // Numerator and demoninator for log-rank test, notation from Ishwaran et al.
			        double di = (double) num_deaths[t];
			        double di1 = (double) num_deaths_right_child[t];
			        double Yi = (double) num_samples_at_risk[t];
			        double Yi1 = (double) num_samples_at_risk_right_child;
			        numerator += di1 - Yi1 * (di / Yi);
			        denominator_squared += (Yi1 / Yi) * (1.0 - Yi1 / Yi) * ((Yi - di) / (Yi - 1)) * di;
			      }

			      // Reduce number of samples at risk for next timepoint
			      num_samples_at_risk_right_child -= delta_samples_at_risk_right_child[t];
			    }
			    double logrank = -1;
			    if (denominator_squared != 0) {
			      logrank = Math.abs(numerator / Math.sqrt(denominator_squared));
			    }

			    if (logrank > best_logrank[0]) {
			      best_value[0] = splitID;
			      best_varID[0] = varID;
			      best_logrank[0] = logrank;
			    }

			    num_deaths_right_child = null;
			    delta_samples_at_risk_right_child = null;
			  }

		}

		public void findBestSplitValueAUC(int nodeID, int varID, double best_value[], int best_varID[],
			    double best_auc[]) {

			  // Create possible split values
			  Vector<Double> possible_split_values = new Vector<Double>();
			  data.getAllValues(possible_split_values, sampleIDs.get(nodeID), varID);

			  // Try next variable if all equal for this
			  if (possible_split_values.size() < 2) {
			    return;
			  }

			  int num_node_samples = sampleIDs.get(nodeID).size();
			  int num_splits = possible_split_values.size() - 1;
			  int num_possible_pairs = num_node_samples * (num_node_samples - 1) / 2;

			  // Initialize
			  double num_count[] = new double[num_splits];
			  double num_total[] = new double[num_splits];
			  int num_samples_left_child[] = new int[num_splits];
			  for (int i = 0; i < num_splits; ++i) {
			    num_count[i] = num_possible_pairs;
			    num_total[i] = num_possible_pairs;
			    num_samples_left_child[i] = 0;
			  }

			  // For all pairs
			  for (int k = 0; k < num_node_samples; ++k) {
			    int sample_k = sampleIDs.get(nodeID).get(k);
			    double time_k = data.get(sample_k, dependent_varID);
			    double status_k = data.get(sample_k, status_varID);
			    double value_k = data.get(sample_k, varID);

			    // Count samples in left node
			    for (int i = 0; i < num_splits; ++i) {
			      double split_value = possible_split_values.get(i);
			      if (value_k <= split_value) {
			        ++num_samples_left_child[i];
			      }
			    }

			    for (int l = k + 1; l < num_node_samples; ++l) {
			      int sample_l = sampleIDs.get(nodeID).get(l);
			      double time_l = data.get(sample_l, dependent_varID);
			      double status_l = data.get(sample_l, status_varID);
			      double value_l = data.get(sample_l, varID);

			      // Compute split
			      computeAucSplit(time_k, time_l, status_k, status_l, value_k, value_l, num_splits, possible_split_values,
			          num_count, num_total);
			    }
			  }

			  for (int i = 0; i < num_splits; ++i) {
			    // Do not consider this split point if fewer than min_node_size samples in one node
			    int num_samples_right_child = num_node_samples - num_samples_left_child[i];
			    if (num_samples_left_child[i] < min_node_size || num_samples_right_child < min_node_size) {
			      continue;
			    } else {
			      double auc = Math.abs((num_count[i] / 2) / num_total[i] - 0.5);
			      if (auc > best_auc[0]) {
			        best_value[0] = (possible_split_values.get(i) + possible_split_values.get(i + 1)) / 2;
			        best_varID[0] = varID;
			        best_auc[0] = auc;

			        // Use smaller value if average is numerically the same as the larger value
			        if (best_value[0] == possible_split_values.get(i + 1)) {
			          best_value[0] = possible_split_values.get(i);
			        }
			      }
			    }
			  }

			  // Clean up
			  num_count = null;
			  num_total = null;
			  num_samples_left_child = null;
		}
		
		public void computeAucSplit(double time_k, double time_l, double status_k, double status_l, double value_k,
			    double value_l, int num_splits, Vector<Double> possible_split_values, double num_count[],
			    double num_total[]) {

			  boolean ignore_pair = false;
			  boolean do_nothing = false;

			  double value_smaller = 0;
			  double value_larger = 0;
			  double status_smaller = 0;

			  if (time_k < time_l) {
			    value_smaller = value_k;
			    value_larger = value_l;
			    status_smaller = status_k;
			  } else if (time_l < time_k) {
			    value_smaller = value_l;
			    value_larger = value_k;
			    status_smaller = status_l;
			  } else {
			    // Tie in survival time
			    if (status_k == 0 || status_l == 0) {
			      ignore_pair = true;
			    } else {
			      if (splitrule == SplitRule.AUC_IGNORE_TIES) {
			        ignore_pair = true;
			      } else {
			        if (value_k == value_l) {
			          // Tie in survival time and in covariate
			          ignore_pair = true;
			        } else {
			          // Tie in survival time in covariate
			          do_nothing = true;
			        }
			      }
			    }
			  }

			  // Do not count if smaller time censored
			  if (status_smaller == 0) {
			    ignore_pair = true;
			  }

			  if (ignore_pair) {
			    for (int i = 0; i < num_splits; ++i) {
			      --num_count[i];
			      --num_total[i];
			    }
			  } else if (do_nothing) {
			    // Do nothing
			  } else {
			    for (int i = 0; i < num_splits; ++i) {
			      double split_value = possible_split_values.get(i);

			      if (value_smaller <= split_value && value_larger > split_value) {
			        ++num_count[i];
			      } else if (value_smaller > split_value && value_larger <= split_value) {
			        --num_count[i];
			      } else if (value_smaller <= split_value && value_larger <= split_value) {
			        break;
			      }
			    }
			  }

		}

		public boolean findBestSplitExtraTrees(int nodeID, Vector<Integer> possible_split_varIDs) {
              int i;
			  double best_decrease[] = new double[]{-1.0};
			  int num_samples_node = sampleIDs.get(nodeID).size();
			  int best_varID[] = new int[]{0};
			  double best_value[] = new double[]{0.0};

			  computeDeathCounts(nodeID);

			  // Stop early if no split posssible
			  if (num_samples_node >= 2 * min_node_size) {

			    // For all possible split variables
			    for (i = 0; i < possible_split_varIDs.size(); i++) {
			      int varID = possible_split_varIDs.get(i);

			      // Find best split value, if ordered consider all values as split values, else all 2-partitions
			      if (data.isOrderedVariable(varID)) {
			        findBestSplitValueExtraTrees(nodeID, varID, best_value, best_varID, best_decrease);
			      } else {
			        findBestSplitValueExtraTreesUnordered(nodeID, varID, best_value, best_varID, best_decrease);
			      }

			    }
			  }

			  // Stop and save CHF if no good split found (this is terminal node).
			  if (best_decrease[0] < 0) {
			    computeSurvival(nodeID);
			    return true;
			  } else {
			    // If not terminal node save best values
			    split_varIDs.set(nodeID, best_varID[0]);
			    split_values.set(nodeID, best_value[0]);

			    // Compute decrease of impurity for this node and add to variable importance if needed
			    if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
			      addImpurityImportance(nodeID, best_varID[0], best_decrease[0]);
			    }

			    return false;
			  }
		}

		public void findBestSplitValueExtraTrees(int nodeID, int varID, double best_value[], int best_varID[],
			    double best_logrank[]) {

			  // Get min/max values of covariate in node
			  double min[] = new double[1];
			  double max[] = new double[1];
			  data.getMinMaxValues(min, max, sampleIDs.get(nodeID), varID);

			  // Try next variable if all equal for this
			  if (min[0] == max[0]) {
			    return;
			  }

			  // Create possible split values: Draw randomly between min and max
			  Vector<Double> possible_split_values = new Vector<Double>();
			  Random random = new Random();
			  possible_split_values.ensureCapacity(num_random_splits);
			  for (int i = 0; i < num_random_splits; ++i) {
				double dval = (max[0] - min[0])*random.nextDouble() + min[0];
			    possible_split_values.add(dval);
			  }

			  int num_splits = possible_split_values.size();

			  // Initialize
			  int num_deaths_right_child[] = new int[num_splits * num_timepoints];
			  int delta_samples_at_risk_right_child[] = new int[num_splits * num_timepoints];
			  int num_samples_right_child[] = new int[num_splits];

			  computeChildDeathCounts(nodeID, varID, possible_split_values, num_samples_right_child,
			      delta_samples_at_risk_right_child, num_deaths_right_child, num_splits);

			  // Compute logrank test for all splits and use best
			  for (int i = 0; i < num_splits; ++i) {
			    double numerator = 0;
			    double denominator_squared = 0;

			    // Stop if minimal node size reached
			    int num_samples_left_child = sampleIDs.get(nodeID).size() - num_samples_right_child[i];
			    if (num_samples_right_child[i] < min_node_size || num_samples_left_child < min_node_size) {
			      continue;
			    }

			    // Compute logrank test statistic for this split
			    int num_samples_at_risk_right_child = num_samples_right_child[i];
			    for (int t = 0; t < num_timepoints; ++t) {
			      if (num_samples_at_risk[t] < 2 || num_samples_at_risk_right_child < 1) {
			        break;
			      }

			      if (num_deaths[t] > 0) {
			        // Numerator and demoninator for log-rank test, notation from Ishwaran et al.
			        double di = (double) num_deaths[t];
			        double di1 = (double) num_deaths_right_child[i * num_timepoints + t];
			        double Yi = (double) num_samples_at_risk[t];
			        double Yi1 = (double) num_samples_at_risk_right_child;
			        numerator += di1 - Yi1 * (di / Yi);
			        denominator_squared += (Yi1 / Yi) * (1.0 - Yi1 / Yi) * ((Yi - di) / (Yi - 1)) * di;
			      }

			      // Reduce number of samples at risk for next timepoint
			      num_samples_at_risk_right_child -= delta_samples_at_risk_right_child[i * num_timepoints + t];

			    }
			    double logrank = -1;
			    if (denominator_squared != 0) {
			      logrank = Math.abs(numerator / Math.sqrt(denominator_squared));
			    }

			    if (logrank > best_logrank[0]) {
			      best_value[0] = possible_split_values.get(i);
			      best_varID[0] = varID;
			      best_logrank[0] = logrank;
			    }
			  }

			  num_deaths_right_child = null;
			  delta_samples_at_risk_right_child = null;
			  num_samples_right_child = null;
		}
 
		public void findBestSplitValueExtraTreesUnordered(int nodeID, int varID, double best_value[],
			    int best_varID[], double best_logrank[]) {
              int i;
			  int num_unique_values = data.getNumUniqueDataValues(varID);

			  // Get all factor indices in node
			  Vector<Boolean> factor_in_node = new Vector<Boolean>();
			  for (i = 0; i < num_unique_values; i++) {
				  factor_in_node.add(false);
			  }
			  for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
				int sampleID = sampleIDs.get(nodeID).get(i);
			    int index = data.getIndex(sampleID, varID);
			    factor_in_node.set(index, true);
			  }

			  // Vector of indices in and out of node
			  Vector<Integer> indices_in_node = new Vector<Integer>();
			  Vector<Integer> indices_out_node = new Vector<Integer>();
			  indices_in_node.ensureCapacity(num_unique_values);
			  indices_out_node.ensureCapacity(num_unique_values);
			  for (i = 0; i < num_unique_values; ++i) {
			    if (factor_in_node.get(i)) {
			      indices_in_node.add(i);
			    } else {
			      indices_out_node.add(i);
			    }
			  }

			  Random random = new Random();
			  // Generate num_random_splits splits
			  for (i = 0; i < num_random_splits; ++i) {
			    Vector<Integer> split_subset = new Vector<Integer>();
			    split_subset.ensureCapacity(num_unique_values);

			    // Draw random subsets, sample all partitions with equal probability
			    if (indices_in_node.size() > 1) {
			      int num_partitions = (2 << (indices_in_node.size() - 1)) - 2; // 2^n-2 (don't allow full or empty)
			      int splitID_in_node = random.nextInt(num_partitions) + 1;
			      for (int j = 0; j < indices_in_node.size(); ++j) {
			        if ((splitID_in_node & (1 << j)) > 0) {
			          split_subset.add(indices_in_node.get(j));
			        }
			      }
			    }
			    if (indices_out_node.size() > 1) {
			      int num_partitions = (2 << (indices_out_node.size() - 1)) - 1; // 2^n-1 (allow full or empty)
			      int splitID_out_node = random.nextInt(num_partitions+1);
			      for (int j = 0; j < indices_out_node.size(); ++j) {
			        if ((splitID_out_node & (1 << j)) > 0) {
			          split_subset.add(indices_out_node.get(j));
			        }
			      }
			    }

			    // Assign union of the two subsets to right child
			    int splitID = 0;
			    for (i = 0; i < split_subset.size(); i++) {
			      int idx = split_subset.get(i);
			      splitID |= 1 << idx;
			    }

			    // Initialize
			    int num_deaths_right_child[] = new int[num_timepoints];
			    int delta_samples_at_risk_right_child[] = new int[num_timepoints];
			    int num_samples_right_child = 0;
			    double numerator = 0;
			    double denominator_squared = 0;

			    // Count deaths in right child per timepoint
			    for (i = 0; i < sampleIDs.get(nodeID).size(); i++) {
			      int sampleID = sampleIDs.get(nodeID).get(i);
			      int survival_timeID = response_timepointIDs.get(sampleID);
			      double value = data.get(sampleID, varID);
			      int factorID = (int)Math.floor(value) - 1;

			      // If in right child, count
			      // In right child, if bitwise splitID at position factorID is 1
			      if ((splitID & (1 << factorID)) != 0) {
			        ++num_samples_right_child;
			        ++delta_samples_at_risk_right_child[survival_timeID];
			        if (data.get(sampleID, status_varID) == 1) {
			          ++num_deaths_right_child[survival_timeID];
			        }
			      }

			    }

			    // Stop if minimal node size reached
			    int num_samples_left_child = sampleIDs.get(nodeID).size() - num_samples_right_child;
			    if (num_samples_right_child < min_node_size || num_samples_left_child < min_node_size) {
			      num_deaths_right_child = null;
			      delta_samples_at_risk_right_child = null;
			      continue;
			    }

			    // Compute logrank test statistic for this split
			    int num_samples_at_risk_right_child = num_samples_right_child;
			    for (int t = 0; t < num_timepoints; ++t) {
			      if (num_samples_at_risk[t] < 2 || num_samples_at_risk_right_child < 1) {
			        break;
			      }

			      if (num_deaths[t] > 0) {
			        // Numerator and demoninator for log-rank test, notation from Ishwaran et al.
			        double di = (double) num_deaths[t];
			        double di1 = (double) num_deaths_right_child[t];
			        double Yi = (double) num_samples_at_risk[t];
			        double Yi1 = (double) num_samples_at_risk_right_child;
			        numerator += di1 - Yi1 * (di / Yi);
			        denominator_squared += (Yi1 / Yi) * (1.0 - Yi1 / Yi) * ((Yi - di) / (Yi - 1)) * di;
			      }

			      // Reduce number of samples at risk for next timepoint
			      num_samples_at_risk_right_child -= delta_samples_at_risk_right_child[t];
			    }
			    double logrank = -1;
			    if (denominator_squared != 0) {
			      logrank = Math.abs(numerator / Math.sqrt(denominator_squared));
			    }

			    if (logrank > best_logrank[0]) {
			      best_value[0] = splitID;
			      best_varID[0] = varID;
			      best_logrank[0] = logrank;
			    }

			    num_deaths_right_child = null;
			    delta_samples_at_risk_right_child = null;
			  }
		}
		
		public void addImpurityImportance(int nodeID, int varID, double decrease) {
              int i;
			  // No variable importance for no split variables
			  int tempvarID = data.getUnpermutedVarID(varID); 
			  for (i = 0; i < data.getNoSplitVariables().size(); i++) {
				int skip = data.getNoSplitVariables().get(i);
			    if (tempvarID >= skip) {
			      --tempvarID;
			    }
			  }

			  // Subtract if corrected importance and permuted variable, else add
			  double val = variable_importance.get(tempvarID);
			  if (importance_mode == ImportanceMode.IMP_GINI_CORRECTED && varID >= data.getNumCols()) {
				  variable_importance.set(tempvarID, val - decrease);
			  } else {
			    variable_importance.set(tempvarID, val + decrease);
			  }
		}

		public void bootstrapClassWise() {
    		
    	}
    	
    	public void bootstrapWithoutReplacementClassWise() {
    		
    	}
    	
    	public void cleanUpInternal() {
    		num_deaths = null;
    		num_samples_at_risk = null;
    	}
	} // private class TreeSurvival
	
	private abstract class Forest {
		protected Random random;
		// Verbose output stream, cout if verbose == true, logfile if not
		//protected BufferedWriter verbose_out;
		protected boolean verbose_out = false;
		protected int num_trees;
		protected int mtry;
		protected int min_node_size;
		protected int num_variables;
		protected int num_independent_variables;
		protected long seed;
		protected int dependent_varID;
		protected int num_samples;
		protected boolean prediction_mode;
		protected MemoryMode memory_mode;
		protected boolean sample_with_replacement;
		protected boolean memory_saving_splitting;
		protected SplitRule splitrule;
		protected boolean predict_all;
		protected boolean keep_inbag;
		protected Vector<Double> sample_fraction;
		protected boolean holdout;
		protected PredictionType prediction_type;
		protected int num_random_splits;
		
		// MAXSTAT splitrule
		protected double alpha;
		protected double minprop;
		
		// Multithreading
		protected int num_threads;
		protected Vector<Integer> thread_ranges;
		
		protected Vector<Tree> trees;
		protected Data data;
		
		protected Vector<Vector<Vector<Double>>> predictions;
		protected double overall_prediction_error;

		// Weight vector for selecting possible split variables, one weight between 0 (never select)
		// and 1 (always select) for each variable
		// Deterministic variables are always selected
		protected Vector<Integer> deterministic_varIDs;
		protected Vector<Integer> split_select_varIDs;
		protected Vector<Vector<Double>> split_select_weights;
		
		// Bootstrap weights
		protected Vector<Double> case_weights;
		
		protected String output_prefix;
		protected ImportanceMode importance_mode;
		
		// Variable importance for all variables in forest
		protected Vector<Double> variable_importance;
		
		// Computation progress (finished trees)
		protected int progress;
		
		protected final Lock mutex = new ReentrantLock();
		private final Condition condition_variable = mutex.newCondition();
		
		protected abstract void growInternal();
		// Predict using existing tree from file and data as prediction data
		protected abstract void allocatePredictMemory();
		protected abstract void predictInternal(int sample_idx);
		protected abstract void computePredictionErrorInternal();
		protected abstract void loadFromFileInternal(BufferedReader br);
		public abstract void initInternal(String status_variable_name);
		public abstract void writeOutputInternal();
		public abstract void writeConfusionFile();
		public abstract void writePredictionFile();
		public abstract void saveToFileInternal(BufferedWriter bw);
		
		public Forest() {
			verbose_out = false;
			num_trees = DEFAULT_NUM_TREE;
			mtry = 0;
			min_node_size = 0;
			num_variables = 0;
			num_independent_variables = 0;
			seed = 0;
			dependent_varID = 0;
			num_samples = 0;
			prediction_mode = false;
			memory_mode = MemoryMode.MEM_DOUBLE;
			sample_with_replacement = true;
			memory_saving_splitting = false;
			splitrule = DEFAULT_SPLITRULE;
			predict_all = false;
			keep_inbag = false;
			sample_fraction = new Vector<Double>();
			sample_fraction.add(1.0);
			holdout = false;
			prediction_type = DEFAULT_PREDICTIONTYPE;
			num_random_splits = DEFAULT_NUM_RANDOM_SPLITS;
			alpha = DEFAULT_ALPHA;
			minprop = DEFAULT_MINPROP;
			num_threads = DEFAULT_NUM_THREADS;
			data = null;
			overall_prediction_error = 0.0;
			importance_mode = DEFAULT_IMPORTANCE_MODE;
			progress = 0;
		}
		
		public Vector<Vector<Vector<Integer>>> getChildNodeIDs() {
		    Vector<Vector<Vector<Integer>>> result = new Vector<Vector<Vector<Integer>>>();
		    for (int i = 0; i < trees.size(); i++) {
		      Tree tree = trees.get(i);
		      result.add(tree.getChildNodeIDs());
		    }
		    return result;
		}
		
		public Vector<Vector<Integer>> getSplitVarIDs() {
		    Vector<Vector<Integer>> result = new Vector<Vector<Integer>>();
		    for (int i = 0; i < trees.size(); i++) {
		        Tree tree = trees.get(i);
		        result.add(tree.getSplitVarIDs());
		    }
		    return result;
		  }
		
		
		  public Vector<Vector<Double>> getSplitValues() {
		    Vector<Vector<Double>> result = new Vector<Vector<Double>>();
		    for (int i = 0; i < trees.size(); i++) {
		        Tree tree = trees.get(i);
		        result.add(tree.getSplitValues());
		    }
		    return result;
		  }
		  
		  public Vector<Double> getVariableImportance() {
		    return variable_importance;
		  }
		  
		  public double getOverallPredictionError() {
		    return overall_prediction_error;
		  }
		  
		  public Vector<Vector<Vector<Double>>> getPredictions() {
		    return predictions;
		  }
		  
		  public int getDependentVarId() {
		    return dependent_varID;
		  }
		  
		  public int getNumTrees() {
		    return num_trees;
		  }
		  
		  public int getMtry() {
		    return mtry;
		  }
		  
		  public int getMinNodeSize() {
		    return min_node_size;
		  }
		  
		  public int getNumIndependentVariables() {
		    return num_independent_variables;
		  }

		  public Vector<Boolean> getIsOrderedVariable() {
		    return data.getIsOrderedVariable();
		  }

		  public Vector<Vector<Integer>> getInbagCounts() {
		    Vector<Vector<Integer>> result = new Vector<Vector<Integer>>();
		    for (int i = 0; i < trees.size(); i++) {
		        Tree tree = trees.get(i);
		        result.add(tree.getInbagCounts());
		    }
		    return result;
		  }

		
		public void dispose() {
			for (int i = trees.size()-1; i >= 0; i--) {
				Tree tree = trees.get(i);
				tree = null;
			}
		}
		
		public void initCpp(String dependent_variable_name, MemoryMode memory_mode, String input_file, int mtry,
			    String output_prefix, int num_trees, boolean verbose_out, long seed, int num_threads,
			    String load_forest_filename, ImportanceMode importance_mode, int min_node_size,
			    String split_select_weights_file, Vector<String> always_split_variable_names,
			    String status_variable_name, boolean sample_with_replacement, Vector<String> unordered_variable_names,
			    boolean memory_saving_splitting, SplitRule splitrule, String case_weights_file, boolean predict_all,
			    double sample_fraction, double alpha, double minprop, boolean holdout, PredictionType prediction_type,
			    int num_random_splits) {

			  this.verbose_out = verbose_out;
			  int i;

			  // Initialize data with memmode
			  switch (memory_mode) {
			  case MEM_DOUBLE:
			    data = new DataDouble();
			    break;
			  case MEM_FLOAT:
			    data = new DataFloat();
			    break;
			  case MEM_CHAR:
			    data = new DataChar();
			    break;
			  }

			  // Load data
			  if (verbose_out) Preferences.debug("Loading input file: " + input_file + ".\n", Preferences.DEBUG_ALGORITHM);
			  boolean rounding_error = data.loadFromFile(input_file);
			  if (rounding_error && verbose_out) {
			    Preferences.debug("Warning: Rounding or Integer overflow occurred. Use FLOAT or DOUBLE precision to avoid this.\n",Preferences.DEBUG_ALGORITHM);
			  }

			  // Set prediction mode
			  boolean prediction_mode = false;
			  if (!load_forest_filename.isEmpty()) {
			    prediction_mode = true;
			  }

			  // Sample fraction to vector
			  Vector<Double> sample_fraction_vector = new Vector<Double>();
			  sample_fraction_vector.add(sample_fraction);

			  // Call other init function
			  init(dependent_variable_name, memory_mode, data, mtry, output_prefix, num_trees, seed, num_threads, importance_mode,
			      min_node_size, status_variable_name, prediction_mode, sample_with_replacement, unordered_variable_names,
			      memory_saving_splitting, splitrule, predict_all, sample_fraction_vector, alpha, minprop, holdout, prediction_type,
			      num_random_splits);

			  if (prediction_mode) {
			    loadFromFile(load_forest_filename);
			  }
			  // Set variables to be always considered for splitting
			  if (!always_split_variable_names.isEmpty()) {
			    setAlwaysSplitVariables(always_split_variable_names);
			  }

			  // TODO: Read 2d weights for tree-wise split select weights
			  // Load split select weights from file
			  if (!split_select_weights_file.isEmpty()) {
			    Vector<Vector<Double>>split_select_weights = new Vector<Vector<Double>>();
			    split_select_weights.add(new Vector<Double>());
			    loadDoubleVectorFromFile(split_select_weights.get(0), split_select_weights_file);
			    if (split_select_weights.get(0).size() != num_variables - 1) {
			      MipavUtil.displayError("Number of split select weights is not equal to number of independent variables.");
			      System.exit(-1);
			    }
			    setSplitWeightVector(split_select_weights);
			  }

			  // Load case weights from file
			  if (!case_weights_file.isEmpty()) {
			    loadDoubleVectorFromFile(case_weights, case_weights_file);
			    if (case_weights.size() != num_samples - 1) {
			      MipavUtil.displayError("Number of case weights is not equal to number of samples.");
			      System.exit(-1);
			    }
			  }

			  // Sample from non-zero weights in holdout mode
			  if (holdout && !case_weights.isEmpty()) {
			    int nonzero_weights = 0;
			    for (i = 0; i < case_weights.size(); i++) {
			      double weight = case_weights.get(i);
			      if (weight > 0) {
			        ++nonzero_weights;
			      }
			    }
			    double sf = this.sample_fraction.get(0);
			    this.sample_fraction.set(0, sf * ((double) nonzero_weights / (double) num_samples));
			  }

			  // Check if all catvars are coded in integers starting at 1
			  if (!unordered_variable_names.isEmpty()) {
			    String error_message = checkUnorderedVariables(data, unordered_variable_names);
			    if (!error_message.isEmpty()) {
			      MipavUtil.displayError(error_message);
			      System.exit(-1);
			    }
			  }
		}
		

	public void initR(String dependent_variable_name, Data input_data, int mtry, int num_trees,
	    boolean verbose_out, long seed, int num_threads, ImportanceMode importance_mode, int min_node_size,
	    Vector<Vector<Double>> split_select_weights, Vector<String> always_split_variable_names,
	    String status_variable_name, boolean prediction_mode, boolean sample_with_replacement,
	    Vector<String> unordered_variable_names, boolean memory_saving_splitting, SplitRule splitrule,
	    Vector<Double> case_weights, boolean predict_all, boolean keep_inbag, Vector<Double> sample_fraction,
	    double alpha, double minprop, boolean holdout, PredictionType prediction_type, int num_random_splits) {
	
	  this.verbose_out = verbose_out;
	
	  // Call other init function
	  init(dependent_variable_name, MemoryMode.MEM_DOUBLE, input_data, mtry, "", num_trees, seed, num_threads, importance_mode,
	      min_node_size, status_variable_name, prediction_mode, sample_with_replacement, unordered_variable_names,
	      memory_saving_splitting, splitrule, predict_all, sample_fraction, alpha, minprop, holdout, prediction_type,
	      num_random_splits);
	
	  // Set variables to be always considered for splitting
	  if (!always_split_variable_names.isEmpty()) {
	    setAlwaysSplitVariables(always_split_variable_names);
	  }
	
	  // Set split select weights
	  if (!split_select_weights.isEmpty()) {
	    setSplitWeightVector(split_select_weights);
	  }
	
	  // Set case weights
	  if (!case_weights.isEmpty()) {
	    if (case_weights.size() != num_samples) {
	      MipavUtil.displayError("Number of case weights not equal to number of samples.");
	      System.exit(-1);
	    }
	    this.case_weights = case_weights;
	  }
	
	  // Keep inbag counts
	  this.keep_inbag = keep_inbag;
	}


		
		public void init(String dependent_variable_name, MemoryMode memory_mode, Data input_data, int mtry,
			    String output_prefix, int num_trees, long seed, int num_threads, ImportanceMode importance_mode,
			    int min_node_size, String status_variable_name, boolean prediction_mode, boolean sample_with_replacement,
			    Vector<String> unordered_variable_names, boolean memory_saving_splitting, SplitRule splitrule,
			    boolean predict_all, Vector<Double> sample_fraction, double alpha, double minprop, boolean holdout,
			    PredictionType prediction_type, int num_random_splits) {

			  // Initialize data with memmode
			  this.data = input_data;

			  // Initialize random number generator and set seed
			  if (seed == 0) {
			    random = new Random();
			  } else {
			    random = new Random(seed);
			  }

			  // Set number of threads
			  if (num_threads == DEFAULT_NUM_THREADS) {
			    this.num_threads = ThreadUtil.getAvailableCores();
			  } else {
			    this.num_threads = num_threads;
			  }

			  // Set member variables
			  this.num_trees = num_trees;
			  this.mtry = mtry;
			  this.seed = seed;
			  this.output_prefix = output_prefix;
			  this.importance_mode = importance_mode;
			  this.min_node_size = min_node_size;
			  this.memory_mode = memory_mode;
			  this.prediction_mode = prediction_mode;
			  this.sample_with_replacement = sample_with_replacement;
			  this.memory_saving_splitting = memory_saving_splitting;
			  this.splitrule = splitrule;
			  this.predict_all = predict_all;
			  this.sample_fraction = sample_fraction;
			  this.holdout = holdout;
			  this.alpha = alpha;
			  this.minprop = minprop;
			  this.prediction_type = prediction_type;
			  this.num_random_splits = num_random_splits;

			  // Set number of samples and variables
			  num_samples = data.getNumRows();
			  num_variables = data.getNumCols();

			  // Convert dependent variable name to ID
			  if (!prediction_mode && !dependent_variable_name.isEmpty()) {
			    dependent_varID = data.getVariableID(dependent_variable_name);
			  }

			  // Set unordered factor variables
			  if (!prediction_mode) {
			    data.setIsOrderedVariableString(unordered_variable_names);
			  }

			  data.addNoSplitVariable(dependent_varID);

			  initInternal(status_variable_name);

			  num_independent_variables = num_variables - data.getNoSplitVariables().size();

			  // Init split select weights
			  split_select_weights.add(new Vector<Double>());

			  // Check if mtry is in valid range
			  if (this.mtry > num_variables - 1) {
			    MipavUtil.displayError("mtry can not be larger than number of variables-1 in data.");
			    System.exit(-1);
			  }

			  // Check if any observations samples
			  if ((int) num_samples * sample_fraction.get(0) < 1) {
			    MipavUtil.displayError("sample_fraction too small, no observations sampled.");
			    System.exit(-1);
			  }

			  // Permute samples for corrected Gini importance
			  if (importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
			    data.permuteSampleIDs(random);
			  }

		}

		public void run(boolean verbose) {

			  if (prediction_mode) {
			    if (verbose && verbose_out) {
			      Preferences.debug("Predicting ..\n",Preferences.DEBUG_ALGORITHM);
			    }
			    predict();
			  } else {
			    if (verbose && verbose_out) {
			      Preferences.debug("Growing trees ..\n",Preferences.DEBUG_ALGORITHM);
			    }

			    grow();

			    if (verbose && verbose_out) {
			      Preferences.debug("Computing prediction error ..\n",Preferences.DEBUG_ALGORITHM);
			    }
			    computePredictionError();

			    if (importance_mode == ImportanceMode.IMP_PERM_BREIMAN || importance_mode == ImportanceMode.IMP_PERM_LIAW ||
			    		importance_mode == ImportanceMode.IMP_PERM_RAW) {
			      if (verbose && verbose_out) {
			        Preferences.debug("Computing permutation variable importance ..\n",Preferences.DEBUG_ALGORITHM);
			      }
			      computePermutationImportance();
			    }
			  }
		}
		
		public void writeOutput() {

			  writeOutputInternal();
			  if (verbose_out) {
			    Preferences.debug("\nDependent variable name:           " + data.getVariableNames().get(dependent_varID) + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Dependent variable ID:             " + dependent_varID + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Number of trees:                   " + num_trees  + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Sample size:                       " + num_samples + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Number of independent variables:   " + num_independent_variables + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Mtry:                              " + mtry + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Target node size:                  " + min_node_size + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Variable importance mode:          " + importance_mode + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Memory mode:                       " + memory_mode + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Seed:                              " + seed + "\n", Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("Number of threads:                 " + num_threads+ "\n\n", Preferences.DEBUG_ALGORITHM);
			  }

			  if (prediction_mode) {
			    writePredictionFile();
			  } else {
			    if (verbose_out) {
			      Preferences.debug("Overall OOB prediction error:      " + overall_prediction_error + "\n\n", Preferences.DEBUG_ALGORITHM);
			    }

			    if (!split_select_weights.isEmpty() & !split_select_weights.get(0).isEmpty()) {
			      if (verbose_out) {
			          Preferences.debug(
			          "Warning: Split select weights used. Variable importance measures are only comparable for variables with equal weights.\n",
			           Preferences.DEBUG_ALGORITHM);
			      }
			    }

			    if (importance_mode != ImportanceMode.IMP_NONE) {
			      writeImportanceFile();
			    }

			    writeConfusionFile();
			  }
		}

		public void writeImportanceFile() {
              int i,j;
			  // Open importance file for writing
			  String filename = output_prefix + ".importance";
			  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in writeImportanceFile");
				  return;
			  }
	          BufferedWriter bw = new BufferedWriter(fw);

			  // Write importance to file
			  for (i = 0; i < variable_importance.size(); ++i) {
			    int varID = i;
			    for (j = 0; j < data.getNoSplitVariables().size(); j++) {
			      int skip = data.getNoSplitVariables().get(j);
			      if (varID >= skip) {
			        ++varID;
			      }
			    }
			    String variable_name = data.getVariableNames().get(varID);
			    try {
			    	bw.write(variable_name + ": " + variable_importance.get(i) + "\n");
			    }
			    catch (IOException e) {
			    	MipavUtil.displayError("IO exception on bw.write for writeImportanceFile");
			    	return;
			    }
			  }

			  try {
			      bw.close();
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on bw.close for writeImportanceFile");
				  return;
			  }
			  if (verbose_out) Preferences.debug("Saved variable importance to file " + filename + ".\n", Preferences.DEBUG_ALGORITHM);
		}

		public void saveToFile() {

			  // Open file for writing
			  String filename = output_prefix + ".forest";
			  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in saveToFile");
				  return;
			  }
	          BufferedWriter bw = new BufferedWriter(fw);

			  // Write dependent_varID
	          try {
			      bw.write(String.valueOf(dependent_varID) + "\n");
	          }
              catch(IOException e) {
            	  MipavUtil.displayError("IO exception on bw.write of dependent_varID in saveToFile");
            	  return;
              }
			  // Write num_trees
	          try {
	        	  bw.write(String.valueOf(num_trees) + "\n");
	          }
	          catch(IOException e) {
	        	  MipavUtil.displayError("IO exception on bw.write of num_trees in saveToFile");
	        	  return;
	          }

			  // Write is_ordered_variable
			  saveBVector1D(data.getIsOrderedVariable(), bw);

			  saveToFileInternal(bw);

			  // Write tree data for each tree
			  for (int i = 0; i < trees.size(); i++) {
				Tree tree = trees.get(i);
			    tree.appendToFile(bw);
			  }

			  // Close file
			  try {
			      bw.close();
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception onbw.close in saveToFile");
				  return;
			  }
			  if (verbose_out) Preferences.debug("Saved forest to file " + filename + ".\n", Preferences.DEBUG_ALGORITHM);
		}

		public void grow() {
              int i,j;
              int insize;
			  // Create thread ranges
			  equalSplit(thread_ranges, 0, num_trees - 1, num_threads);

			  // Call special grow functions of subclasses. There trees must be created.
			  growInternal();

			  // Init trees, create a seed for each tree, based on main seed
			  for (i = 0; i < num_trees; ++i) {
			    long tree_seed;
			    if (seed == 0) {
			      tree_seed = random.nextLong();
			    } else {
			      tree_seed = (i + 1) * seed;
			    }

			    // Get split select weights for tree
			    Vector<Double> tree_split_select_weights = new Vector<Double>();
			    if (split_select_weights.size() > 1) {
			      tree_split_select_weights = split_select_weights.get(i);
			    } else {
			      tree_split_select_weights = split_select_weights.get(0);
			    }

			    trees.get(i).init(data, mtry, dependent_varID, num_samples, tree_seed, deterministic_varIDs, split_select_varIDs,
			        tree_split_select_weights, importance_mode, min_node_size, sample_with_replacement, memory_saving_splitting,
			        splitrule, case_weights, keep_inbag, sample_fraction, alpha, minprop, holdout, num_random_splits);
			  }

			// Init variable importance
			  insize = variable_importance.size();
			  variable_importance.setSize(num_independent_variables);
			  for (i = insize; i < num_independent_variables; i++) {
				  variable_importance.set(i, 0.0);
			  }

			  progress = 0;

			// Initailize importance per thread
			  Vector<Vector<Double>> variable_importance_threads = new Vector<Vector<Double>>();
			  for (i = 0; i < num_threads; i++) {
				  variable_importance_threads.add(new Vector<Double>());
			  }

			  for (i = 0; i < num_threads; ++i) {
			    if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
			      insize = variable_importance_threads.get(i).size();
			      variable_importance_threads.get(i).setSize(num_independent_variables);
			      for (j = insize; j < num_independent_variables; j++) {
			    	  variable_importance_threads.get(i).set(j, 0.0);
			      }
			    }
			  }
			  ExecutorService executorService = Executors.newCachedThreadPool();
			  showProgress("Growing trees..", num_trees);
			  for (i = 0; i < num_threads; i++) {
				  executorService.execute(new growTreesInThread(i, variable_importance_threads.get(i)));
			  }
			  executorService.shutdown();
			  try {
				  boolean tasksEnded = executorService.awaitTermination(10, TimeUnit.MINUTES);
				  if (!tasksEnded) {
					  MipavUtil.displayError("Timed out waiting for growTreesInThread to finish");
					  System.exit(-1);
				  }
			  }
			  catch (InterruptedException ex) {
				  ex.printStackTrace();
			  }

			  // Sum thread importances
			  if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
				insize = variable_importance.size();
				variable_importance.setSize(num_independent_variables);
				for (i = insize; i < num_independent_variables; i++) {
					variable_importance.set(i, 0.0);
				}
			    for (i = 0; i < num_independent_variables; ++i) {
			      for (j = 0; j < num_threads; ++j) {
			    	double val = variable_importance.get(i);
			    	variable_importance.set(i, val + variable_importance_threads.get(j).get(i));
			      }
			    }
			    variable_importance_threads.clear();
			  }

			// Divide importance by number of trees
			  if (importance_mode == ImportanceMode.IMP_GINI || importance_mode == ImportanceMode.IMP_GINI_CORRECTED) {
			    for (i = 0; i < variable_importance.size(); i++) {
			     double v = variable_importance.get(i);
			     variable_importance.set(i, v/num_trees);
			    }
			  }
		}
		
		public void predict() {
              int i;
			  // Predict trees in multiple threads and join the threads with the main thread
			  progress = 0;

			  // Predict
			  ExecutorService executorService = Executors.newCachedThreadPool();
			  showProgress("Predicting..", num_trees);
			  for (i = 0; i < num_threads; i++) {
				  executorService.execute(new predictTreesInThread(i, data, false));	  
			  }
		
			  executorService.shutdown();
			  try {
				  boolean tasksEnded = executorService.awaitTermination(10, TimeUnit.MINUTES);
				  if (!tasksEnded) {
					  MipavUtil.displayError("Timed out waiting for predictTreesInThread to finish");
					  System.exit(-1);
				  }
			  }
			  catch (InterruptedException ex) {
				  ex.printStackTrace();
			  }

			  // Aggregate predictions
			  allocatePredictMemory();
			  progress = 0;
			  executorService = Executors.newCachedThreadPool();
			  showProgress("Aggregating predictions..", num_samples);
			  for (i = 0; i < num_threads; ++i) {
				  executorService.execute(new predictInternalInThread(i));
			  }
			  executorService.shutdown();
			  try {
				  boolean tasksEnded = executorService.awaitTermination(10, TimeUnit.MINUTES);
				  if (!tasksEnded) {
					  MipavUtil.displayError("Timed out waiting for predictInternalInThread to finish");
					  System.exit(-1);
				  }
			  }
			  catch (InterruptedException ex) {
				  ex.printStackTrace();
			  }

			
		}

		public void computePredictionError() {

			 // Predict trees in multiple threads
			
			  progress = 0;
			  ExecutorService executorService = Executors.newCachedThreadPool();
			  showProgress("Computing prediction error..", num_trees);
			  for (int i = 0; i < num_threads; ++i) {
				  executorService.execute(new predictTreesInThread(i, data, true));
			  }
			  executorService.shutdown();
			  try {
				  boolean tasksEnded = executorService.awaitTermination(10, TimeUnit.MINUTES);
				  if (!tasksEnded) {
					  MipavUtil.displayError("Timed out waiting for predictTreesInThread to finish");
					  System.exit(-1);
				  }
			  }
			  catch (InterruptedException ex) {
				  ex.printStackTrace();
			  }
			
			  // Call special function for subclasses
			  computePredictionErrorInternal();
		}
		
		public void computePermutationImportance() {
              int i,j;
              int insize;
              double val;
			  // Compute tree permutation importance in multiple threads
			
			  progress = 0;
			
			// Initialize importance and variance
			  Vector<Vector<Double>> variable_importance_threads = new Vector<Vector<Double>>();
			  Vector<Vector<Double>> variance_threads = new Vector<Vector<Double>>();
			  for (i = 0; i < num_threads; i++) {
				  variable_importance_threads.add(new Vector<Double>());
				  variance_threads.add(new Vector<Double>());
			  }

			// Compute importance
			  ExecutorService executorService = Executors.newCachedThreadPool();
			  showProgress("Computing permutation importance..", num_trees);
			  for (i = 0; i < num_threads; ++i) {
				for (j = 0; j < num_independent_variables; j++) {
					variable_importance_threads.get(i).add(0.0);
				}
			    if (importance_mode == ImportanceMode.IMP_PERM_BREIMAN || importance_mode == ImportanceMode.IMP_PERM_LIAW) {
			      for (j = 0; j < num_independent_variables; j++) {
			    	  variance_threads.get(i).add(0.0);
			      }
			    }
			    executorService.execute(new computeTreePermutationImportanceInThread(i, variable_importance_threads.get(i),
			    		variance_threads.get(i)));
			  }
			  
			  executorService.shutdown();
			  try {
				  boolean tasksEnded = executorService.awaitTermination(10, TimeUnit.MINUTES);
				  if (!tasksEnded) {
					  MipavUtil.displayError("Timed out waiting for predictTreesInThread to finish");
					  System.exit(-1);
				  }
			  }
			  catch (InterruptedException ex) {
				  ex.printStackTrace();
			  }

			  // Sum thread importances
			  insize = variable_importance.size();
			  variable_importance.setSize(num_independent_variables);
			  for (i = insize; i < num_independent_variables; i++) {
				  variable_importance.set(i, 0.0);
			  }
			  for (i = 0; i < num_independent_variables; ++i) {
			    for (j = 0; j < num_threads; ++j) {
			      val = variable_importance.get(i);
			      variable_importance.set(i, val + variable_importance_threads.get(j).get(i));
			    }
			  }
			  variable_importance_threads.clear();

			// Sum thread variances
			  Vector<Double> variance = new Vector<Double>();
			  for (i = 0; i < num_independent_variables; i++) {
				  variance.add(0.0);
			  }
			  if (importance_mode == ImportanceMode.IMP_PERM_BREIMAN || importance_mode == ImportanceMode.IMP_PERM_LIAW) {
			    for (i = 0; i < num_independent_variables; ++i) {
			      for (j = 0; j < num_threads; ++j) {
			    	val = variance.get(i);  
			        variance.set(i, val + variance_threads.get(j).get(i));
			      }
			    }
			    variance_threads.clear();
			  }

			  for (i = 0; i < variable_importance.size(); ++i) {
				val = variable_importance.get(i);
			    variable_importance.set(i, val/num_trees);

			    // Normalize by variance for scaled permutation importance
			    if (importance_mode == ImportanceMode.IMP_PERM_BREIMAN || importance_mode == ImportanceMode.IMP_PERM_LIAW) {
			      if (variance.get(i) != 0) {
			    	val = variance.get(i) / num_trees - variable_importance.get(i) * variable_importance.get(i);
			    	variance.set(i, val);
			        val = variable_importance.get(i);
			        variable_importance.set(i, val/Math.sqrt(variance.get(i)/num_trees));
			      }
			    }
			  }
			}

		public class growTreesInThread implements Runnable {
			  private int thread_idx;
			  private Vector<Double> variable_importance;
			  public growTreesInThread(int thread_idx, Vector<Double> variable_importance) {
			      this.thread_idx = thread_idx;
			      this.variable_importance = variable_importance;
			  }
			  
			  public void run() {
				  if (thread_ranges.size() > thread_idx + 1) {
				    for (int i = thread_ranges.get(thread_idx); i < thread_ranges.get(thread_idx + 1); ++i) {
				      trees.get(i).grow(variable_importance);
	
				      // Check for user interrupt
	
				      // Increase progress by 1 tree
				      try {
				          mutex.lock();
				          ++progress;
				      }
				      finally {
				          mutex.unlock();
				      }
				      condition_variable.signalAll();
				    }
				  }
			  }
		}
		
		public class predictTreesInThread implements Runnable {
			private int thread_idx;
			private Data prediction_data;
			private boolean oob_prediction;
			public predictTreesInThread(int thread_idx, Data prediction_data, boolean oob_prediction) {
				this.thread_idx = thread_idx;
				this.prediction_data = prediction_data;
				this.oob_prediction = oob_prediction;
			}
			
			public void run() {
				  if (thread_ranges.size() > thread_idx + 1) {
				    for (int i = thread_ranges.get(thread_idx); i < thread_ranges.get(thread_idx + 1); ++i) {
				      trees.get(i).predict(prediction_data, oob_prediction);
	
				      // Check for user interrupt
				
				      // Increase progress by 1 tree
				      try {
				         mutex.lock();
				         ++progress;
				      }
				      finally {
				          mutex.unlock();
				      }
				      condition_variable.signalAll();
				    }
				  }
			   }
		}

		public class predictInternalInThread implements Runnable {
			private int thread_idx;
			public predictInternalInThread(int thread_idx) {
				this.thread_idx = thread_idx;
			}
			
			public void run() {
			  // Create thread ranges
			  Vector<Integer> predict_ranges = new Vector<Integer>();
			  equalSplit(predict_ranges, 0, num_samples - 1, num_threads);

			  if (predict_ranges.size() > thread_idx + 1) {
			    for (int i = predict_ranges.get(thread_idx); i < predict_ranges.get(thread_idx + 1); ++i) {
			      predictInternal(i);

			      // Check for user interrupt

			      // Increase progress by 1 tree
			      try {
			          mutex.lock();
			          ++progress;
			      }
			      finally {
			          mutex.unlock();
			      }
			      condition_variable.signalAll();
			    }
			  }
			}
		}

		public class computeTreePermutationImportanceInThread implements Runnable {
			private int thread_idx;
			private Vector<Double> importance;
			private Vector<Double> variance;
			public computeTreePermutationImportanceInThread(int thread_idx, Vector<Double> importance,
					Vector<Double> variance) {
				this.thread_idx = thread_idx;
				this.importance = importance;
				this.variance = variance;
			}
			
			public void run() {
			  if (thread_ranges.size() > thread_idx + 1) {
			    for (int i = thread_ranges.get(thread_idx); i < thread_ranges.get(thread_idx + 1); ++i) {
			      trees.get(i).computePermutationImportance(importance, variance);

			      // Check for user interrupt

			      // Increase progress by 1 tree
			      try {
			          mutex.lock();
			          ++progress;
			      }
			      finally {
			          mutex.unlock();
			      }
			      condition_variable.signalAll();
			    }
			  }
			}
		}

		public void loadFromFile(String filename) {
			  if (verbose_out) Preferences.debug("Loading forest from file " + filename + ".\n", Preferences.DEBUG_ALGORITHM);

		    // Open input file
	    	File file = new File(filename);
	    	BufferedReader input_file;
	    	try {
	    	    input_file = new BufferedReader(new FileReader(file));
	    	}
	    	catch (FileNotFoundException e) {
	    		MipavUtil.displayError("Could not find file " + filename);
	    	  	return;
	    	}
			  
			// Read dependent_varID and num_trees
	    	String line;
	  	    try {
	              line = input_file.readLine();
	  	    }
	  	    catch (IOException e) {
	  		    MipavUtil.displayError("IO exception on input_file.readLine()");
	  		    return;
	  	    }
	  	    dependent_varID = Integer.valueOf(line).intValue();
	  	    try {
                line = input_file.readLine();
  	        }
  	        catch (IOException e) {
  		       MipavUtil.displayError("IO exception on input_file.readLine()");
  		       return;
  	       }
  	       num_trees = Integer.valueOf(line).intValue();

			// Read is_ordered_variable
			  readBVector1D(data.getIsOrderedVariable(), input_file);

			// Read tree data. This is different for tree types -> virtual function
			  loadFromFileInternal(input_file);

			  try {
			      input_file.close();
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on input_file.close() in loadFromFile");
			  }

			// Create thread ranges
			  equalSplit(thread_ranges, 0, num_trees - 1, num_threads);
		}
		
		public void setSplitWeightVector(Vector<Vector<Double>> split_select_weights) {
            int i,j,k;
            int insize;
			// Size should be 1 x num_independent_variables or num_trees x num_independent_variables
			  if (split_select_weights.size() != 1 && split_select_weights.size() != num_trees) {
			    MipavUtil.displayError("Size of split select weights not equal to 1 or number of trees.");
			    System.exit(-1);
			  }

			// Reserve space
			  if (split_select_weights.size() == 1) {
			    this.split_select_weights.get(0).setSize(num_independent_variables);
			  } else {
			    this.split_select_weights.clear();
			    for (i = 0; i < num_trees; i++) {
			        Vector<Double>vec = new Vector<Double>();
			        for (j = 0; j < num_independent_variables; j++) {
			        	vec.add(0.0);
			        }
			        split_select_weights.add(vec);
			    }
			    
			  }
			  insize = this.split_select_varIDs.size();
			  this.split_select_varIDs.setSize(num_independent_variables);
			  for (i = insize; i < num_independent_variables; i++) {
				  this.split_select_varIDs.set(i, 0);
			  }
			  deterministic_varIDs.ensureCapacity(num_independent_variables);

			// Split up in deterministic and weighted variables, ignore zero weights
			  for (i = 0; i < split_select_weights.size(); ++i) {

			    // Size should be 1 x num_independent_variables or num_trees x num_independent_variables
			    if (split_select_weights.get(i).size() != num_independent_variables) {
			      MipavUtil.displayError("Number of split select weights not equal to number of independent variables.");
			      System.exit(-1);
			    }

			    for (j = 0; j < split_select_weights.get(i).size(); ++j) {
			      double weight = split_select_weights.get(i).get(j);

			      if (i == 0) {
			        int varID = j;
			        for (k = 0; k < data.getNoSplitVariables().size(); k++) {
			          int skip = data.getNoSplitVariables().get(k);
			          if (varID >= skip) {
			            ++varID;
			          }
			        }

			        if (weight == 1) {
			          deterministic_varIDs.add(varID);
			        } else if (weight < 1 && weight > 0) {
			          this.split_select_varIDs.set(j, varID);
			          this.split_select_weights.get(i).set(j, weight);
			        } else if (weight < 0 || weight > 1) {
			          MipavUtil.displayError("One or more split select weights not in range [0,1].");
			          System.exit(-1);
			        }

			      } else {
			        if (weight < 1 && weight > 0) {
			          this.split_select_weights.get(i).set(j, weight);
			        } else if (weight < 0 || weight > 1) {
			          MipavUtil.displayError("One or more split select weights not in range [0,1].");
			          System.exit(-1);
			        }
			      }
			    }
			  }

			  if (deterministic_varIDs.size() > this.mtry) {
			    MipavUtil.displayError("Number of ones in split select weights cannot be larger than mtry.");
			    System.exit(-1);
			  }
			  if (deterministic_varIDs.size() + split_select_varIDs.size() < mtry) {
			    MipavUtil.displayError("Too many zeros in split select weights. Need at least mtry variables to split at.");
			    System.exit(-1);
			  }
		}

		public void setAlwaysSplitVariables(Vector<String> always_split_variable_names) {

			  deterministic_varIDs.ensureCapacity(num_independent_variables);

			  for (int i = 0; i < always_split_variable_names.size(); i++) {
				String variable_name = always_split_variable_names.get(i);
			    int varID = data.getVariableID(variable_name);
			    deterministic_varIDs.add(varID);
			  }

			  if (deterministic_varIDs.size() + this.mtry > num_independent_variables) {
			    MipavUtil.displayError(
			        "Number of variables to be always considered for splitting plus mtry cannot be larger than number of independent variables.");
			    System.exit(-1);
			  }
		}
		
		public void showProgress(String operation, int max_progress) {

			  Instant start_time = Instant.now();
			  Instant last_time = Instant.now();
			  mutex.lock();

			// Wait for message from threads and show output if enough time elapsed
			  while (progress < max_progress) {
				try {
			        condition_variable.await();
				}
				catch (InterruptedException exception) {
					Thread.currentThread().interrupt();
				}
			    Instant present_time = Instant.now();
			    double elapsed_time = Duration.between(last_time, present_time).toMillis()/1000.0;

			    // Check for user interrupt

			    if (progress > 0 && elapsed_time > STATUS_INTERVAL) {
			      double relative_progress = (double) progress / (double) max_progress;
			      present_time = Instant.now();
			      double time_from_start = Duration.between(start_time, present_time).toMillis()/1000.0;
			      int remaining_time = (int)Math.round((1 / relative_progress - 1) * time_from_start);
			      if (verbose_out) {
			        System.out.println(operation + " Progress: " + (int)Math.round(100 * relative_progress)
			                     + "%. Estimated remaining time: " + beautifyTime(remaining_time) + ".");
			      }
			      last_time = Instant.now();
			    }
			  }
			  mutex.unlock();
		}


	} // private abstract Forest
	
	private class ForestClassification extends Forest {
		// Classes of the dependent variable and classIDs for responses
		  Vector<Double> class_values;
		  Vector<Integer> response_classIDs;
		  Vector<Vector<Integer>> sampleIDs_per_class;

		  // Splitting weights
		  Vector<Double> class_weights;

		  // Table with classifications and true classes
		  Map<Pair<Double, Double>, Integer> classification_table;
		  
		  public Vector<Double> getClassValues() {
			    return class_values;
		  }

		  public void setClassWeights(Vector<Double> class_weights) {
			    this.class_weights = class_weights;
	      }
		  
		  public ForestClassification() {
			  super();
		  }
		  
		  public void loadForest(int dependent_varID, int num_trees,
				    Vector<Vector<Vector<Integer>> > forest_child_nodeIDs,
				    Vector<Vector<Integer>> forest_split_varIDs, Vector<Vector<Double>> forest_split_values,
				    Vector<Double> class_values, Vector<Boolean> is_ordered_variable) {

				  this.dependent_varID = dependent_varID;
				  this.num_trees = num_trees;
				  this.class_values = class_values;
				  data.setIsOrderedVariable(is_ordered_variable);

				  // Create trees
				  trees.ensureCapacity(num_trees);
				  for (int i = 0; i < num_trees; ++i) {
				    Tree tree = new TreeClassification(forest_child_nodeIDs.get(i), forest_split_varIDs.get(i), 
				    		forest_split_values.get(i), this.class_values, response_classIDs);
				    trees.add(tree);
				  }

				  // Create thread ranges
				  equalSplit(thread_ranges, 0, num_trees - 1, num_threads);
		  }

		  public void initInternal(String status_variable_name) {
              int i;
              int insize;
			  // If mtry not set, use floored square root of number of independent variables.
			  if (mtry == 0) {
			    int temp = (int)Math.sqrt((double) (num_variables - 1));
			    mtry = Math.max(1, temp);
			  }

			  // Set minimal node size
			  if (min_node_size == 0) {
			    min_node_size = DEFAULT_MIN_NODE_SIZE_CLASSIFICATION;
			  }

			  // Create class_values and response_classIDs
			  if (!prediction_mode) {
			    for (i = 0; i < num_samples; ++i) {
			      double value = data.get(i, dependent_varID);

			      // If classID is already in class_values, use ID. Else create a new one.
			      int classID;
			      for (classID = 0; classID < class_values.size(); classID++) {
			    	  if (class_values.get(classID) == value) {
			    		  break;
			    	  }
			      }
			      if (classID == class_values.size()) {
			        class_values.add(value);
			      }
			      response_classIDs.add(classID);
			    }
			  }

			  // Create sampleIDs_per_class if required
			  if (sample_fraction.size() > 1) {
				insize = sampleIDs_per_class.size();
				sampleIDs_per_class.setSize(sample_fraction.size());
				for (i = insize; i < sample_fraction.size(); i++) {
					sampleIDs_per_class.set(i, new Vector<Integer>());
				}
			    for (i = 0; i < sampleIDs_per_class.size(); i++) {
			      sampleIDs_per_class.get(i).ensureCapacity(num_samples);
			    }
			    for (i = 0; i < num_samples; ++i) {
			      int classID = response_classIDs.get(i);
			      sampleIDs_per_class.get(classID).add(i);
			    }
			  }

			  // Set class weights all to 1
			  class_weights = new Vector<Double>();
			  for (i = 0; i < class_values.size(); i++) {
				  class_weights.add(1.0);
			  }

			  // Sort data if memory saving mode
			  if (!memory_saving_splitting) {
			    data.sort();
			  }

		}
		  
		public void growInternal() {
			  trees.ensureCapacity(num_trees);
			  for (int i = 0; i < num_trees; ++i) {
			    trees.add(new TreeClassification(class_values, response_classIDs, sampleIDs_per_class, class_weights));
			  }
		}

		public void allocatePredictMemory() {
			  int i, j, k;
			  int num_prediction_samples = data.getNumRows();
			  if (predict_all || prediction_type == PredictionType.TERMINALNODES) {
				predictions = new Vector<Vector<Vector<Double>>>();
				for (i = 0; i < 1; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < num_prediction_samples; j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < num_trees; k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				}
			    
			  } else {
				  predictions = new Vector<Vector<Vector<Double>>>();
					for (i = 0; i < 1; i++) {
						Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
						for (j = 0; j < 1; j++) {
							Vector<Double> vec = new Vector<Double>();
							for (k = 0; k < num_prediction_samples; k++) {
								vec.add(0.0);
							}
							vec2.add(vec);
						}
						predictions.add(vec2);
					}
			  }
		}
		
		public void predictInternal(int sample_idx) {
			  if (predict_all || prediction_type == PredictionType.TERMINALNODES) {
			    // Get all tree predictions
			    for (int tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
			      if (prediction_type == PredictionType.TERMINALNODES) {
			        predictions.get(0).get(sample_idx).set(tree_idx, (double)
			        ((TreeClassification) trees.get(tree_idx)).getPredictionTerminalNodeID(sample_idx));
			      } else {
			        predictions.get(0).get(sample_idx).set(tree_idx,
			        		((TreeClassification) trees.get(tree_idx)).getPrediction(sample_idx));
			      }
			    }
			  } else {
			    // Count classes over trees and save class with maximum count
			    HashMap<Double, Integer> class_count = new HashMap<Double,Integer>();
			    for (int tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
			      double value = ((TreeClassification) trees.get(tree_idx)).getPrediction(sample_idx);
			      int map_value;
			      if (class_count.get(value) == null) {
			    	  map_value = 0;
			      }
			      else {
			          map_value = class_count.get(value);
			      }
			      class_count.put(value, map_value + 1);
			    }
			    predictions.get(0).get(0).set(sample_idx, mostFrequentValue(class_count, random));
			  }
		}

		public void computePredictionErrorInternal() {
              int i, j, k;
              int intValue;
			  // Class counts for samples
			  Vector<HashMap<Double, Integer>> class_counts = new Vector<HashMap<Double, Integer>>();
			  class_counts.ensureCapacity(num_samples);
			  for (i = 0; i < num_samples; ++i) {
			    class_counts.add(new HashMap<Double, Integer>());
			  }

			  // For each tree loop over OOB samples and count classes
			  for (int tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
			    for (int sample_idx = 0; sample_idx < trees.get(tree_idx).getNumSamplesOob(); ++sample_idx) {
			      int sampleID = trees.get(tree_idx).getOobSampleIDs().get(sample_idx);
			      double value = ((TreeClassification) trees.get(tree_idx)).getPrediction(sample_idx);
			      if (class_counts.get(sampleID).get(value) == null) {
			    	  intValue = 0;
			      }
			      else {
			    	  intValue = class_counts.get(sampleID).get(value);
			      }
			      class_counts.get(sampleID).put(value, intValue+1);
			    }
			  }

			  // Compute majority vote for each sample
			  predictions = new Vector<Vector<Vector<Double>>>();
			  for (i = 0; i < 1; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < 1; j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < num_samples; k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				}
			  for (i = 0; i < num_samples; ++i) {
			    if (!class_counts.get(i).isEmpty()) {
			      predictions.get(0).get(0).set(i, mostFrequentValue(class_counts.get(i), random));
			    } else {
			      predictions.get(0).get(0).set(i, Double.NaN);
			    }
			  }

			  // Compare predictions with true data
			  int num_missclassifications = 0;
			  int num_predictions = 0;
			  for (i = 0; i < predictions.get(0).get(0).size(); ++i) {
			    double predicted_value = predictions.get(0).get(0).get(i);
			    if (!Double.isNaN(predicted_value)) {
			      ++num_predictions;
			      double real_value = data.get(i, dependent_varID);
			      if (predicted_value != real_value) {
			        ++num_missclassifications;
			      }
			      Pair<Double, Double> pair = new Pair<Double, Double>(real_value, predicted_value);
			      if (classification_table.get(pair) == null) {
			    	  intValue = 0;
			      }
			      else {
			    	  intValue = classification_table.get(pair);
			      }
			      classification_table.put(pair, intValue+1);
			    }
			  }
			  overall_prediction_error = (double) num_missclassifications / (double) num_predictions;
		}

	    public void writeOutputInternal() {
			  if (verbose_out) {
			    Preferences.debug("Tree type:                         " + "Classification\n", Preferences.DEBUG_ALGORITHM);
			  }
		}
 
	    public void writeConfusionFile() {
              int i, j;
	    	  // Open confusion file for writing
	    	  String filename = output_prefix + ".confusion";
	    	  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in writeConfusionFile");
				  return;
			  }
	          BufferedWriter bw = new BufferedWriter(fw);
	          
	    	  // Write confusion to file
	          try {
		    	  bw.write("Overall OOB prediction error (Fraction missclassified): " + 
		    	            String.valueOf(overall_prediction_error) + "\n");
		    	  bw.write("\n");
		    	  bw.write("Class specific prediction errors:" + "\n");
		    	  bw.write("           ");
		    	  for (i = 0; i < class_values.size(); i++) {
		    		  double class_value = class_values.get(i);
		    	      bw.write("     " + class_value);
		    	  }
		    	  bw.write("\n");
		    	  for (i = 0; i < class_values.size(); i++) {
		    		double predicted_value = class_values.get(i);
		    	    bw.write("predicted " + String.valueOf(predicted_value) + "     ");
		    	    for (j = 0; j < class_values.size(); j++) {
		    	      double real_value = class_values.get(j);
		    	      Pair<Double, Double> pair = new Pair<Double, Double>(real_value, predicted_value);
		    	      int value = classification_table.get(pair);
		    	      bw.write(String.valueOf(value));
		    	      if (value < 10) {
		    	        bw.write("     ");
		    	      } else if (value < 100) {
		    	        bw.write("    ");
		    	      } else if (value < 1000) {
		    	        bw.write("   ");
		    	      } else if (value < 10000) {
		    	        bw.write("  ");
		    	      } else if (value < 100000) {
		    	        bw.write(" ");
		    	      }
		    	    }
		    	    bw.write("\n");
		    	  }
	          }
	          catch (IOException e) {
	        	  MipavUtil.displayError("IO exception on bw.write in writeConfusionFile()");
	        	  System.exit(-1);
	          }

	    	  try {
	              bw.close();
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on bw.close in writeConfusionFile");
	    		  System.exit(-1);
	    	  }
	    	  if (verbose_out) {
	    		  Preferences.debug("Saved confusion matrix to file " + filename + "." + "\n", Preferences.DEBUG_ALGORITHM);
	    	  }
	    }

	    public void writePredictionFile() {

	    	  // Open prediction file for writing
	    	  String filename = output_prefix + ".prediction";
	    	  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in writePredictionFile");
				  return;
			  }
	          BufferedWriter bw = new BufferedWriter(fw);

	    	  // Write
	          try {
		    	  bw.write("Predictions: " + "\n");
		    	  if (predict_all) {
		    	    for (int k = 0; k < num_trees; ++k) {
		    	      bw.write("Tree " + String.valueOf(k) + ":" + "\n");
		    	      for (int i = 0; i < predictions.size(); ++i) {
		    	        for (int j = 0; j < predictions.get(i).size(); ++j) {
		    	          bw.write(String.valueOf(predictions.get(i).get(j).get(k)) + "\n");
		    	        }
		    	      }
		    	      bw.write("\n");
		    	    }
		    	  } else {
		    	    for (int i = 0; i < predictions.size(); ++i) {
		    	      for (int j = 0; j < predictions.get(i).size(); ++j) {
		    	        for (int k = 0; k < predictions.get(i).get(j).size(); ++k) {
		    	          bw.write(String.valueOf(predictions.get(i).get(j).get(k)) + "\n");
		    	        }
		    	      }
		    	    }
		    	  }
	          }
	          catch (IOException e) {
	        	  MipavUtil.displayError("IO exception on bw.write in writePredictionFile");
	        	  System.exit(-1);
	          }
	    	  
	    	  try {
	            bw.close();
	    	  }
	    	  catch (IOException e) {
	        	  MipavUtil.displayError("IO exception on bw.close in writePredictionFile");
	        	  System.exit(-1);
	          }

	    	  if (verbose_out) {
	    		  Preferences.debug("Saved predictions to file " + filename + "." + "\n", Preferences.DEBUG_ALGORITHM);
	    	  }
	    }
	    
	    public void saveToFileInternal(BufferedWriter bw) {

	    	  // Write num_variables
	    	  try {
		    	  bw.write(String.valueOf(num_variables) + "\n");
	
		    	  // Write treetype
		    	  bw.write("TREE_CLASSIFICATION" + "\n");
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on bw.write in saveToFileInternal");
	    		  System.exit(-1);
	    	  }

	    	  // Write class_values
	    	  saveDVector1D(class_values, bw);
	   }

	   public void loadFromFileInternal(BufferedReader br) {
		      int i,j;
              String line = null;
	    	  // Read number of variables
	    	  int num_variables_saved = 0;
	    	  TreeType treetype = null;
	    	  try {
		    	  line = br.readLine();
		    	  num_variables_saved = Integer.valueOf(line).intValue();
	
		    	  // Read treetype
		    	  line = br.readLine();
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on br.readLine in loadFromFileInternal");
	    		  System.exit(-1);
	    	  }
	    	  if (line.equals("TREE_CLASSIFICATION")) {
	    		  treetype = TreeType.TREE_CLASSIFICATION;
	    	  }
	    	  else if (line.equals("TREE_REGRESSION")) {
	    		  treetype = TreeType.TREE_REGRESSION;
	    	  }
	    	  else if (line.equals("TREE_SURVIVAL")) {
	    		  treetype = TreeType.TREE_SURVIVAL;
	    	  }
	    	  else if (line.equals("TREE_PROBABILITY")) {
	    		  treetype = TreeType.TREE_PROBABILITY;
	    	  }
	    	  if (treetype != TreeType.TREE_CLASSIFICATION) {
	    	    MipavUtil.displayError("Wrong treetype. Loaded file is not a classification forest.");
	    	    System.exit(-1);
	    	  }

	    	  // Read class_values
	    	  readDVector1D(class_values, br);

	    	  for (i = 0; i < num_trees; ++i) {

	    	    // Read data
	    	    Vector<Vector<Integer>> child_nodeIDs = new Vector<Vector<Integer>>();
	    	    readVector2D(child_nodeIDs, br);
	    	    Vector<Integer> split_varIDs = new Vector<Integer>();
	    	    readVector1D(split_varIDs, br);
	    	    Vector<Double> split_values = new Vector<Double>();
	    	    readDVector1D(split_values, br);

	    	    // If dependent variable not in test data, change variable IDs accordingly
	    	    if (num_variables_saved > num_variables) {
	    	      for (j = 0; j < split_varIDs.size(); j++) {
	    	    	int varID = split_varIDs.get(j);
	    	        if (varID >= dependent_varID) {
	    	          --varID;
	    	        }
	    	      }
	    	    }

	    	    // Create tree
	    	    Tree tree = new TreeClassification(child_nodeIDs, split_varIDs, split_values, class_values, response_classIDs);
	    	    trees.add(tree);
	    	  }
	    	}

	} // private class ForestClassification
	

	private class ForestProbability extends Forest {
		  // Classes of the dependent variable and classIDs for responses
		  protected Vector<Double> class_values;
		  protected Vector<Integer> response_classIDs;
		  protected Vector<Vector<Integer>> sampleIDs_per_class;

		  // Splitting weights
		  protected Vector<Double> class_weights;

		  // Table with classifications and true classes
		  protected HashMap<Pair<Double, Double>, Integer> classification_table;
		  
		  public Vector<Vector<Vector<Double>>> getTerminalClassCounts() {
			    int i;
			    Vector<Vector<Vector<Double>>> result = new Vector<Vector<Vector<Double>>>();
			    result.ensureCapacity(num_trees);
			    for (i = 0; i < trees.size(); i++) {
			      Tree tree = trees.get(i);
			      TreeProbability temp = (TreeProbability) tree;
			      result.add(temp.getTerminalClassCounts());
			    }
			    return result;
			}

		  public Vector<Double> getClassValues() {
		    return class_values;
		  }

		  public void setClassWeights(Vector<Double> class_weights) {
		    this.class_weights = class_weights;
		  }

	      public ForestProbability() {
	    	  super();
	      }
	      
	      public void loadForest(int dependent_varID, int num_trees,
	    		    Vector<Vector<Vector<Integer>> > forest_child_nodeIDs,
	    		    Vector<Vector<Integer>> forest_split_varIDs, Vector<Vector<Double>> forest_split_values,
	    		    Vector<Double> class_values, Vector<Vector<Vector<Double>>> forest_terminal_class_counts,
	    		    Vector<Boolean> is_ordered_variable) {

	    		  this.dependent_varID = dependent_varID;
	    		  this.num_trees = num_trees;
	    		  this.class_values = class_values;
	    		  data.setIsOrderedVariable(is_ordered_variable);

	    		  // Create trees
	    		  trees.ensureCapacity(num_trees);
	    		  for (int i = 0; i < num_trees; ++i) {
	    		    Tree tree = new TreeProbability(forest_child_nodeIDs.get(i), forest_split_varIDs.get(i), forest_split_values.get(i),
	    		        this.class_values, response_classIDs, forest_terminal_class_counts.get(i));
	    		    trees.add(tree);
	    		  }

	    		  // Create thread ranges
	    		  equalSplit(thread_ranges, 0, num_trees - 1, num_threads);
	      }
	      
	      public void initInternal(String status_variable_name) {
              int i;
              int classID;
              int insize;
	    	  // If mtry not set, use floored square root of number of independent variables.
	    	  if (mtry == 0) {
	    	    int temp = (int)Math.sqrt((double) (num_variables - 1));
	    	    mtry = Math.max(1, temp);
	    	  }

	    	  // Set minimal node size
	    	  if (min_node_size == 0) {
	    	    min_node_size = DEFAULT_MIN_NODE_SIZE_PROBABILITY;
	    	  }

	    	  // Create class_values and response_classIDs
	    	  if (!prediction_mode) {
	    	    for (i = 0; i < num_samples; ++i) {
	    	      double value = data.get(i, dependent_varID);

	    	      // If classID is already in class_values, use ID. Else create a new one.
	    	      for (classID = 0; classID < class_values.size(); classID++) {
	    	    	  if (class_values.get(classID)  == value) {
	    	    		 break;
	    	    	  }
	    	      }
	    	      if (classID == class_values.size()) {
	    	        class_values.add(value);
	    	      }
	    	      response_classIDs.add(classID);
	    	    }
	    	  }

	    	  // Create sampleIDs_per_class if required
	    	  if (sample_fraction.size() > 1) {
	    		insize = sampleIDs_per_class.size();
	    		sampleIDs_per_class.setSize(sample_fraction.size());
	    		for ( i = insize; i < sample_fraction.size(); i++) {
	    			sampleIDs_per_class.set(i, new Vector<Integer>());
	    		}
	    	    for (i = 0; i < sampleIDs_per_class.size(); i++) {
	    	      sampleIDs_per_class.get(i).ensureCapacity(num_samples);
	    	    }
	    	    for (i = 0; i < num_samples; ++i) {
	    	      classID = response_classIDs.get(i);
	    	      sampleIDs_per_class.get(classID).add(i);
	    	    }
	    	  }

	    	  // Set class weights all to 1
	    	  class_weights = new Vector<Double>();
	    	  for (i = 0; i < class_values.size(); i++) {
	    		  class_weights.add(1.0);
	    	  }

	    	  // Sort data if memory saving mode
	    	  if (!memory_saving_splitting) {
	    	    data.sort();
	    	  }
	    }

	      public void growInternal() {
	    	  trees.ensureCapacity(num_trees);
	    	  for (int i = 0; i < num_trees; ++i) {
	    	    trees.add(new TreeProbability(class_values, response_classIDs, sampleIDs_per_class, class_weights));
	    	  }
	      }
	      
	      public void allocatePredictMemory() {
	    	  int i, j, k;
	    	  int num_prediction_samples = data.getNumRows();
	    	  if (predict_all) {
	    		  predictions = new Vector<Vector<Vector<Double>>>();
				  for (i = 0; i < num_prediction_samples; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < class_values.size(); j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < num_trees; k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				  }
	    	  } else if (prediction_type == PredictionType.TERMINALNODES) {
	    		  predictions = new Vector<Vector<Vector<Double>>>();
				  for (i = 0; i < 1; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < num_prediction_samples; j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < num_trees; k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				  }
	    	  } else {
	    		  predictions = new Vector<Vector<Vector<Double>>>();
				  for (i = 0; i < 1; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < num_prediction_samples; j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < class_values.size(); k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				  }
	    	  }
	      }

	      public void predictInternal(int sample_idx) {
	    	  double value;
	    	  // For each sample compute proportions in each tree
	    	  for (int tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
	    	    if (predict_all) {
	    	      Vector<Double> counts = ((TreeProbability) trees.get(tree_idx)).getPrediction(sample_idx);

	    	      for (int class_idx = 0; class_idx < counts.size(); ++class_idx) {
	    	    	value = predictions.get(sample_idx).get(class_idx).get(tree_idx);
	    	    	predictions.get(sample_idx).get(class_idx).set(tree_idx, value + counts.get(class_idx));
	    	      }
	    	    } else if (prediction_type == PredictionType.TERMINALNODES) {
	    	      value = predictions.get(0).get(sample_idx).set(tree_idx,(double)
	    	      ((TreeProbability) trees.get(tree_idx)).getPredictionTerminalNodeID(
	    	          sample_idx));
	    	    } else {
	    	      Vector<Double> counts = ((TreeProbability) trees.get(tree_idx)).getPrediction(sample_idx);

	    	      for (int class_idx = 0; class_idx < counts.size(); ++class_idx) {
	    	    	value = predictions.get(0).get(sample_idx).get(class_idx);
	    	        predictions.get(0).get(sample_idx).set(class_idx, value + counts.get(class_idx));
	    	      }
	    	    }
	    	  }

	    	  // Average over trees
	    	  if (!predict_all && prediction_type != PredictionType.TERMINALNODES) {
	    	    for (int class_idx = 0; class_idx < predictions.get(0).get(sample_idx).size(); ++class_idx) {
	    	      value = predictions.get(0).get(sample_idx).get(class_idx);
	    	      predictions.get(0).get(sample_idx).set(class_idx, value/num_trees);
	    	    }
	    	  }
	      }
	      
	      public void computePredictionErrorInternal() {
              int i, j, k;
              double value;
              int intValue;
	    	// For each sample sum over trees where sample is OOB
	    	  Vector<Integer> samples_oob_count = new Vector<Integer>();
	    	  for (i = 0; i < num_samples; i++) {
	    		  samples_oob_count.add(0);
	    	  }
	    	  predictions = new Vector<Vector<Vector<Double>>>();
			  for (i = 0; i < 1; i++) {
				Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
				for (j = 0; j < num_samples; j++) {
					Vector<Double> vec = new Vector<Double>();
					for (k = 0; k < class_values.size(); k++) {
						vec.add(0.0);
					}
					vec2.add(vec);
				}
				predictions.add(vec2);
			  }

	    	  for (int tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
	    	    for (int sample_idx = 0; sample_idx < trees.get(tree_idx).getNumSamplesOob(); ++sample_idx) {
	    	      int sampleID = trees.get(tree_idx).getOobSampleIDs().get(sample_idx);
	    	      Vector<Double> counts = ((TreeProbability) trees.get(tree_idx)).getPrediction(sample_idx);

	    	      for (int class_idx = 0; class_idx < counts.size(); ++class_idx) {
	    	    	value = predictions.get(0).get(sampleID).get(class_idx);
	    	        predictions.get(0).get(sampleID).set(class_idx, value +  counts.get(class_idx));
	    	      }
	    	      intValue = samples_oob_count.get(sampleID);
	    	      samples_oob_count.set(sampleID, intValue+1);
	    	    }
	    	  }

	    	// MSE with predicted probability and true data
	    	  int num_predictions = 0;
	    	  for (i = 0; i < predictions.get(0).size(); ++i) {
	    	    if (samples_oob_count.get(i) > 0) {
	    	      ++num_predictions;
	    	      for (j = 0; j < predictions.get(0).get(i).size(); ++j) {
	    	    	value = predictions.get(0).get(i).get(j);
	    	        predictions.get(0).get(i).set(j, value/(double) samples_oob_count.get(i));
	    	      }
	    	      int real_classID = response_classIDs.get(i);
	    	      double predicted_value = predictions.get(0).get(i).get(real_classID);
	    	      overall_prediction_error += (1 - predicted_value) * (1 - predicted_value);
	    	    } else {
	    	      for (j = 0; j < predictions.get(0).get(i).size(); ++j) {
	    	        predictions.get(0).get(i).set(j, Double.NaN);
	    	      }
	    	    }
	    	  }

	    	  overall_prediction_error /= (double) num_predictions;
	     }

	     public void writeOutputInternal() {
	    	  if (verbose_out) {
	    	    Preferences.debug("Tree type:                         " + "Probability estimation" + "\n",
	    	    		Preferences.DEBUG_ALGORITHM);
	    	  }
	     }

	     public void writeConfusionFile() {

	    	  // Open confusion file for writing
	    	  String filename = output_prefix + ".confusion";
	    	  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in writeConfusionFile");
				  return;
			  }
	          BufferedWriter bw = new BufferedWriter(fw);
	    	  

	    	  // Write confusion to file
	          try {
	    	      bw.write("Overall OOB prediction error (MSE): " + String.valueOf(overall_prediction_error) + "\n");
	          }
	          catch (IOException e) {
	        	  MipavUtil.displayError("IO exception in bw.write in writeConfusionFile");
	        	  System.exit(-1);
	          }

	    	  try {
	              bw.close();
	    	  }
	    	  catch (IOException e) {
	        	  MipavUtil.displayError("IO exception in bw.close in writeConfusionFile");
	        	  System.exit(-1);
	          }
	    	  if (verbose_out) {
	    		  Preferences.debug("Saved prediction error to file " + filename + "." + "\n",
	    				  Preferences.DEBUG_ALGORITHM);
	    	  }
	    }

	     public void writePredictionFile() {
              int i, j, k;
	    	  // Open prediction file for writing
	    	  String filename = output_prefix + ".prediction";
	    	  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in writePredictionFile");
				  return;
			  }
			  BufferedWriter bw = new BufferedWriter(fw);

	    	  // Write
			  try {
		    	  bw.write("Class predictions, one sample per row." + "\n");
		    	  for (i = 0; i < class_values.size(); i++) {
		    		double class_value = class_values.get(i);
		    	    bw.write(String.valueOf(class_value) + " ");
		    	  }
		    	  bw.write("\n\n");
	
		    	  if (predict_all) {
		    	    for (k = 0; k < num_trees; ++k) {
		    	      bw.write("Tree " + String.valueOf(k) + ":" + "\n");
		    	      for (i = 0; i < predictions.size(); ++i) {
		    	        for (j = 0; j < predictions.get(i).size(); ++j) {
		    	           bw.write(String.valueOf(predictions.get(i).get(j).get(k)) + " ");
		    	        }
		    	        bw.write("\n");
		    	      }
		    	      bw.write("\n");
		    	    }
		    	  } else {
		    	    for (i = 0; i < predictions.size(); ++i) {
		    	      for (j = 0; j < predictions.get(i).size(); ++j) {
		    	        for (k = 0; k < predictions.get(i).get(j).size(); ++k) {
		    	        	bw.write(String.valueOf(predictions.get(i).get(j).get(k)) + " ");
		    	        }
		    	        bw.write("\n");
		    	      }
		    	    }
		    	  }
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception in bw.write in writePredictionFile");
				  System.exit(-1);
			  }
	    	  
	    	  try {
			      bw.close();
	    	  }
	    	  catch (IOException e) {
				  MipavUtil.displayError("IO exception in bw.close in writePredictionFile");
				  System.exit(-1);
			  }

	    	  if (verbose_out) {
	    		  Preferences.debug("Saved predictions to file " + filename + "." + "\n", Preferences.DEBUG_ALGORITHM);
	    	  }
	    }
	     
	     public void saveToFileInternal(BufferedWriter bw) {

	    	// Write num_variables
	    	 try {
		    	  bw.write(String.valueOf(num_variables) + "\n");
	
		    	  // Write treetype
		    	  bw.write("TREE_PROBABILITY" + "\n");
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on bw.write in saveToFileInternal");
	    		  System.exit(-1);
	    	  }

	    	  // Write class_values
	    	  saveDVector1D(class_values, bw);
	    }

	    public void loadFromFileInternal(BufferedReader br) {
	    	int i,j;
            String line = null;
	    	  // Read number of variables
	    	  int num_variables_saved = 0;
	    	  TreeType treetype = null;
	    	  try {
		    	  line = br.readLine();
		    	  num_variables_saved = Integer.valueOf(line).intValue();
	
		    	  // Read treetype
		    	  line = br.readLine();
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on br.readLine in loadFromFileInternal");
	    		  System.exit(-1);
	    	  }
	    	  if (line.equals("TREE_CLASSIFICATION")) {
	    		  treetype = TreeType.TREE_CLASSIFICATION;
	    	  }
	    	  else if (line.equals("TREE_REGRESSION")) {
	    		  treetype = TreeType.TREE_REGRESSION;
	    	  }
	    	  else if (line.equals("TREE_SURVIVAL")) {
	    		  treetype = TreeType.TREE_SURVIVAL;
	    	  }
	    	  else if (line.equals("TREE_PROBABILITY")) {
	    		  treetype = TreeType.TREE_PROBABILITY;
	    	  }
	    	  if (treetype != TreeType.TREE_PROBABILITY) {
	    	    MipavUtil.displayError("Wrong treetype. Loaded file is not a classification forest.");
	    	    System.exit(-1);
	    	  }

	    	  // Read class_values
	    	  readDVector1D(class_values, br);

	    	  for (i = 0; i < num_trees; ++i) {

	    	    // Read data
	    	    Vector<Vector<Integer>> child_nodeIDs = new Vector<Vector<Integer>>();
	    	    readVector2D(child_nodeIDs, br);
	    	    Vector<Integer> split_varIDs = new Vector<Integer>();
	    	    readVector1D(split_varIDs, br);
	    	    Vector<Double> split_values = new Vector<Double>();
	    	    readDVector1D(split_values, br);
	    	    
	    	    // Read Terminal node class counts
	    	    Vector<Integer> terminal_nodes = new Vector<Integer>();
	    	    readVector1D(terminal_nodes, br);
	    	    Vector<Vector<Double>> terminal_class_counts_vector = new Vector<Vector<Double>>();
	    	    readDVector2D(terminal_class_counts_vector, br);

	    	    // Convert Terminal node class counts to vector with empty elemtents for non-terminal nodes
	    	    Vector<Vector<Double>> terminal_class_counts = new Vector<Vector<Double>>();
	    	    for (j = 0; j < child_nodeIDs.get(0).size(); j++) {
	    	    	terminal_class_counts.add(new Vector<Double>());
	    	    }
	    	    for (j = 0; j < terminal_nodes.size(); ++j) {
	    	      terminal_class_counts.set(terminal_nodes.get(j), terminal_class_counts_vector.get(j));
	    	    }

	    	    // If dependent variable not in test data, change variable IDs accordingly
	    	    if (num_variables_saved > num_variables) {
	    	      for (j = 0; j < split_varIDs.size(); j++) {
	    	    	int varID = split_varIDs.get(j);
	    	        if (varID >= dependent_varID) {
	    	          --varID;
	    	        }
	    	      }
	    	    }

	    	    // Create tree
	    	    Tree tree = new TreeProbability(child_nodeIDs, split_varIDs, split_values, class_values, response_classIDs,
	    	    		terminal_class_counts);
	    	    trees.add(tree);
	    	  }
	    	
	    	}

	} // private class ForestProbability
	
	private class ForestRegression extends Forest {
		
		public ForestRegression() {
			super();
		}
		
		public void loadForest(int dependent_varID, int num_trees,
			    Vector<Vector<Vector<Integer>> > forest_child_nodeIDs,
			    Vector<Vector<Integer>> forest_split_varIDs, Vector<Vector<Double>> forest_split_values,
			    Vector<Boolean> is_ordered_variable) {

			  this.dependent_varID = dependent_varID;
			  this.num_trees = num_trees;
			  data.setIsOrderedVariable(is_ordered_variable);

			  // Create trees
			  trees.ensureCapacity(num_trees);
			  for (int i = 0; i < num_trees; ++i) {
			    Tree tree = new TreeRegression(forest_child_nodeIDs.get(i), forest_split_varIDs.get(i), forest_split_values.get(i));
			    trees.add(tree);
			  }

			  // Create thread ranges
			  equalSplit(thread_ranges, 0, num_trees - 1, num_threads);
		}
		
		public void initInternal(String status_variable_name) {

			  // If mtry not set, use floored square root of number of independent variables
			  if (mtry == 0) {
			    int temp = (int)Math.sqrt((double) (num_variables - 1));
			    mtry = Math.max(1, temp);
			  }

			  // Set minimal node size
			  if (min_node_size == 0) {
			    min_node_size = DEFAULT_MIN_NODE_SIZE_REGRESSION;
			  }

			  // Sort data if memory saving mode
			  if (!memory_saving_splitting) {
			    data.sort();
			  }
		}

		public void growInternal() {
			  trees.ensureCapacity(num_trees);
			  for (int i = 0; i < num_trees; ++i) {
			    trees.add(new TreeRegression());
			  }
		}

		public void allocatePredictMemory() {
			  int i, j, k;
			  int num_prediction_samples = data.getNumRows();
			  if (predict_all || prediction_type == PredictionType.TERMINALNODES) {
				  predictions = new Vector<Vector<Vector<Double>>>();
				  for (i = 0; i < 1; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < num_prediction_samples; j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < num_trees; k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				  }
			  } else {
				  predictions = new Vector<Vector<Vector<Double>>>();
				  for (i = 0; i < 1; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < 1; j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < num_prediction_samples; k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				 }
			  }
		}

		public void predictInternal(int sample_idx) {
			  if (predict_all || prediction_type == PredictionType.TERMINALNODES) {
			    // Get all tree predictions
			    for (int tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
			      if (prediction_type == PredictionType.TERMINALNODES) {
			        predictions.get(0).get(sample_idx).set(tree_idx, (double)
                      ((TreeRegression) trees.get(tree_idx)).getPredictionTerminalNodeID(sample_idx));
			      } else {
			        predictions.get(0).get(sample_idx).set(tree_idx,
			        		((TreeRegression) trees.get(tree_idx)).getPrediction(sample_idx));
			        
			      }
			    }
			  } else {
			    // Mean over trees
			    double prediction_sum = 0;
			    for (int tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
			      prediction_sum += ((TreeRegression) trees.get(tree_idx)).getPrediction(sample_idx);
			    }
			    predictions.get(0).get(0).set(sample_idx, prediction_sum / num_trees);
			  }
		}

		public void computePredictionErrorInternal() {
            int i, j, k;
            double var;
            int intValue;
			// For each sample sum over trees where sample is OOB
			  Vector<Integer> samples_oob_count = new Vector<Integer>();
			  predictions = new Vector<Vector<Vector<Double>>>();
			  for (i = 0; i < 1; i++) {
				Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
				for (j = 0; j < 1; j++) {
					Vector<Double> vec = new Vector<Double>();
					for (k = 0; k < num_samples; k++) {
						vec.add(0.0);
					}
					vec2.add(vec);
				}
				predictions.add(vec2);
			 }
			  for (i = 0; i < num_samples; i++) {
				  samples_oob_count.add(0);
			  }
			  for (int tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
			    for (int sample_idx = 0; sample_idx < trees.get(tree_idx).getNumSamplesOob(); ++sample_idx) {
			      int sampleID = trees.get(tree_idx).getOobSampleIDs().get(sample_idx);
			      double value = ((TreeRegression) trees.get(tree_idx)).getPrediction(sample_idx);

			      var = predictions.get(0).get(0).get(sampleID);
			      predictions.get(0).get(0).set(sampleID, var + value);
			      if (samples_oob_count.get(sampleID) == null) {
			    	  intValue = 0;
			      }
			      else {
			          intValue = samples_oob_count.get(sampleID);
			      }
			      samples_oob_count.set(sampleID, intValue+1);
			    }
			  }

			// MSE with predictions and true data
			  int num_predictions = 0;
			  for (i = 0; i < predictions.get(0).get(0).size(); ++i) {
			    if (samples_oob_count.get(i) > 0) {
			      ++num_predictions;
			      var = predictions.get(0).get(0).get(i);
			      predictions.get(0).get(0).set(i, var/ (double) samples_oob_count.get(i));
			      double predicted_value = predictions.get(0).get(0).get(i);
			      double real_value = data.get(i, dependent_varID);
			      overall_prediction_error += (predicted_value - real_value) * (predicted_value - real_value);
			    } else {
			      predictions.get(0).get(0).set(i, Double.NaN);
			    }
			  }

			  overall_prediction_error /= (double) num_predictions;
		}

			// #nocov start
		public void writeOutputInternal() {
			  if (verbose_out) {
			    Preferences.debug("Tree type:                         " + "Regression" + "\n",
			    		Preferences.DEBUG_ALGORITHM);
			  }
		}
		
		public void writeConfusionFile() {
			// Open confusion file for writing
	    	  String filename = output_prefix + ".confusion";
	    	  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in writeConfusionFile");
				  return;
			  }
	          BufferedWriter bw = new BufferedWriter(fw);
	    	  

	    	  // Write confusion to file
	          try {
	    	      bw.write("Overall OOB prediction error (MSE): " + String.valueOf(overall_prediction_error) + "\n");
	          }
	          catch (IOException e) {
	        	  MipavUtil.displayError("IO exception in bw.write in writeConfusionFile");
	        	  System.exit(-1);
	          }

	    	  try {
	              bw.close();
	    	  }
	    	  catch (IOException e) {
	        	  MipavUtil.displayError("IO exception in bw.close in writeConfusionFile");
	        	  System.exit(-1);
	          }
	    	  if (verbose_out) {
	    		  Preferences.debug("Saved prediction error to file " + filename + "." + "\n",
	    				  Preferences.DEBUG_ALGORITHM);
	    	  }
			
		}

		public void writePredictionFile() {
			int i, j, k;
	    	  // Open prediction file for writing
	    	  String filename = output_prefix + ".prediction";
	    	  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in writePredictionFile");
				  return;
			  }
			  BufferedWriter bw = new BufferedWriter(fw);

	    	  // Write
			  try {
		    	  bw.write("Predictions: " + "\n");
		    	  
		    	  if (predict_all) {
		    	    for (k = 0; k < num_trees; ++k) {
		    	      bw.write("Tree " + String.valueOf(k) + ":" + "\n");
		    	      for (i = 0; i < predictions.size(); ++i) {
		    	        for (j = 0; j < predictions.get(i).size(); ++j) {
		    	           bw.write(String.valueOf(predictions.get(i).get(j).get(k)) + "\n");
		    	        }
		    	      }
		    	      bw.write("\n");
		    	    }
		    	  } else {
		    	    for (i = 0; i < predictions.size(); ++i) {
		    	      for (j = 0; j < predictions.get(i).size(); ++j) {
		    	        for (k = 0; k < predictions.get(i).get(j).size(); ++k) {
		    	        	bw.write(String.valueOf(predictions.get(i).get(j).get(k)) + "\n");
		    	        }
		    	      }
		    	    }
		    	  }
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception in bw.write in writePredictionFile");
				  System.exit(-1);
			  }
	    	  
	    	  try {
			      bw.close();
	    	  }
	    	  catch (IOException e) {
				  MipavUtil.displayError("IO exception in bw.close in writePredictionFile");
				  System.exit(-1);
			  }

	    	  if (verbose_out) {
	    		  Preferences.debug("Saved predictions to file " + filename + "." + "\n", Preferences.DEBUG_ALGORITHM);
	    	  }
			
		}

		public void saveToFileInternal(BufferedWriter bw) {
			// Write num_variables
	    	 try {
		    	  bw.write(String.valueOf(num_variables) + "\n");
	
		    	  // Write treetype
		    	  bw.write("TREE_REGRESSION" + "\n");
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on bw.write in saveToFileInternal");
	    		  System.exit(-1);
	    	  }

		}
		
		public void loadFromFileInternal(BufferedReader br) {
			int i,j;
            String line = null;
	    	  // Read number of variables
	    	  int num_variables_saved = 0;
	    	  TreeType treetype = null;
	    	  try {
		    	  line = br.readLine();
		    	  num_variables_saved = Integer.valueOf(line).intValue();
	
		    	  // Read treetype
		    	  line = br.readLine();
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on br.readLine in loadFromFileInternal");
	    		  System.exit(-1);
	    	  }
	    	  if (line.equals("TREE_CLASSIFICATION")) {
	    		  treetype = TreeType.TREE_CLASSIFICATION;
	    	  }
	    	  else if (line.equals("TREE_REGRESSION")) {
	    		  treetype = TreeType.TREE_REGRESSION;
	    	  }
	    	  else if (line.equals("TREE_SURVIVAL")) {
	    		  treetype = TreeType.TREE_SURVIVAL;
	    	  }
	    	  else if (line.equals("TREE_PROBABILITY")) {
	    		  treetype = TreeType.TREE_PROBABILITY;
	    	  }
	    	  if (treetype != TreeType.TREE_REGRESSION) {
	    	    MipavUtil.displayError("Wrong treetype. Loaded file is not a classification forest.");
	    	    System.exit(-1);
	    	  }


	    	  for (i = 0; i < num_trees; ++i) {

	    	    // Read data
	    	    Vector<Vector<Integer>> child_nodeIDs = new Vector<Vector<Integer>>();
	    	    readVector2D(child_nodeIDs, br);
	    	    Vector<Integer> split_varIDs = new Vector<Integer>();
	    	    readVector1D(split_varIDs, br);
	    	    Vector<Double> split_values = new Vector<Double>();
	    	    readDVector1D(split_values, br);

	    	    // If dependent variable not in test data, change variable IDs accordingly
	    	    if (num_variables_saved > num_variables) {
	    	      for (j = 0; j < split_varIDs.size(); j++) {
	    	    	int varID = split_varIDs.get(j);
	    	        if (varID >= dependent_varID) {
	    	          --varID;
	    	        }
	    	      }
	    	    }

	    	    // Create tree
	    	    Tree tree = new TreeRegression(child_nodeIDs, split_varIDs, split_values);
	    	    trees.add(tree);
	    	  }
			
			}

	} // private class ForestRegression
	
	private class ForestSurvival extends Forest {
		  private int status_varID;
		  private Vector<Double> unique_timepoints;
		  private Vector<Integer> response_timepointIDs;
		  
		  public Vector<Vector<Vector<Double>>>getChf() {
			    int i;
			    Vector<Vector<Vector<Double>>> result = new Vector<Vector<Vector<Double>>>();
			    result.ensureCapacity(num_trees);
			    for (i = 0; i < trees.size(); i++) {
			      Tree tree = trees.get(i);
			      TreeSurvival temp = (TreeSurvival) tree;
			      result.add(temp.getChf());
			    }
			    return result;
		  }
		  
		  public int getStatusVarId() {
	          return status_varID;
	      }
		  
		  public Vector<Double> getUniqueTimepoints() {
			    return unique_timepoints;
		  }

          public ForestSurvival() {
        	  super();
        	  status_varID = 0;
        	  response_timepointIDs = new Vector<Integer>();
          }
          
          public void loadForest(int dependent_varID, int num_trees,
        		    Vector<Vector<Vector<Integer>> > forest_child_nodeIDs,
        		    Vector<Vector<Integer>> forest_split_varIDs, Vector<Vector<Double>> forest_split_values,
        		    int status_varID, Vector<Vector<Vector<Double>> > forest_chf,
        		    Vector<Double> unique_timepoints, Vector<Boolean> is_ordered_variable) {

        		  this.dependent_varID = dependent_varID;
        		  this.status_varID = status_varID;
        		  this.num_trees = num_trees;
        		  this.unique_timepoints = unique_timepoints;
        		  data.setIsOrderedVariable(is_ordered_variable);

        		  // Create trees
        		  trees.ensureCapacity(num_trees);
        		  for (int i = 0; i < num_trees; ++i) {
        		    Tree tree = new TreeSurvival(forest_child_nodeIDs.get(i), forest_split_varIDs.get(i), forest_split_values.get(i),
        		        forest_chf.get(i), this.unique_timepoints, response_timepointIDs);
        		    trees.add(tree);
        		  }

        		  // Create thread ranges
        		  equalSplit(thread_ranges, 0, num_trees - 1, num_threads);
          }

          public void initInternal(String status_variable_name) {
              int i;
        	  // Convert status variable name to ID
        	  if (!prediction_mode && !status_variable_name.isEmpty()) {
        	    status_varID = data.getVariableID(status_variable_name);
        	  }

        	  data.addNoSplitVariable(status_varID);

        	  // If mtry not set, use floored square root of number of independent variables.
        	  if (mtry == 0) {
        	    int temp = (int)Math.ceil(Math.sqrt((double) (num_variables - 2)));
        	    mtry = Math.max(1, temp);
        	  }

        	  // Set minimal node size
        	  if (min_node_size == 0) {
        	    min_node_size = DEFAULT_MIN_NODE_SIZE_SURVIVAL;
        	  }

        	  // Create unique timepoints
        	  HashSet<Double> unique_timepoint_set = new HashSet<Double>();
        	  for (i = 0; i < num_samples; ++i) {
        	    unique_timepoint_set.add(data.get(i, dependent_varID));
        	  }
        	  unique_timepoints.ensureCapacity(unique_timepoint_set.size());
        	  Double unique_timepoint_array[] = (Double [])unique_timepoint_set.toArray();
        	  for (i = 0; i < unique_timepoint_array.length; i++) {
        		Double t = unique_timepoint_array[i];
        	    unique_timepoints.add(t);
        	  }

        	  // Create response_timepointIDs
        	  if (!prediction_mode) {
        	    for (i = 0; i < num_samples; ++i) {
        	      double value = data.get(i, dependent_varID);

        	      // If timepoint is already in unique_timepoints, use ID. Else create a new one.
        	      int timepointID;
        	      for (timepointID = 0; timepointID < unique_timepoints.size(); timepointID++) {
        	    	  if (unique_timepoints.get(timepointID) == value) {
        	    		  break;
        	    	  }
        	      }
        	      response_timepointIDs.add(timepointID);
        	    }
        	  }

        	  // Sort data if extratrees and not memory saving mode
        	  if (splitrule == SplitRule.EXTRATREES && !memory_saving_splitting) {
        	    data.sort();
        	  }
        }
          
         public void growInternal() {
        	  trees.ensureCapacity(num_trees);
        	  for (int i = 0; i < num_trees; ++i) {
        	    trees.add(new TreeSurvival(unique_timepoints, status_varID, response_timepointIDs));
        	  }
         }

         public void allocatePredictMemory() {
        	  int i, j, k;
        	  int num_prediction_samples = data.getNumRows();
        	  int num_timepoints = unique_timepoints.size();
        	  if (predict_all) {
        		  predictions = new Vector<Vector<Vector<Double>>>();
				  for (i = 0; i < num_prediction_samples; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < num_timepoints; j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < num_trees; k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				  }
        	  } else if (prediction_type == PredictionType.TERMINALNODES) {
        		  predictions = new Vector<Vector<Vector<Double>>>();
				  for (i = 0; i < 1; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < num_prediction_samples; j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < num_trees; k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				  }
        	  } else {
        		  predictions = new Vector<Vector<Vector<Double>>>();
				  for (i = 0; i < 1; i++) {
					Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
					for (j = 0; j < num_prediction_samples; j++) {
						Vector<Double> vec = new Vector<Double>();
						for (k = 0; k < num_timepoints; k++) {
							vec.add(0.0);
						}
						vec2.add(vec);
					}
					predictions.add(vec2);
				  }
        	  }
         }

         public void predictInternal(int sample_idx) {
        	  // For each timepoint sum over trees
        	  if (predict_all) {
        	    for (int j = 0; j < unique_timepoints.size(); ++j) {
        	      for (int k = 0; k < num_trees; ++k) {
        	        predictions.get(sample_idx).get(j).set(k, ((TreeSurvival) trees.get(k)).getPrediction(sample_idx).get(j));
        	      }
        	    }
        	  } else if (prediction_type == PredictionType.TERMINALNODES) {
        	    for (int k = 0; k < num_trees; ++k) {
        	      predictions.get(0).get(sample_idx).set(k, (double)((TreeSurvival) trees.get(k)).getPredictionTerminalNodeID(sample_idx));
        	    }
        	  } else {
        	    for (int j = 0; j < unique_timepoints.size(); ++j) {
        	      double sample_time_prediction = 0;
        	      for (int k = 0; k < num_trees; ++k) {
        	        sample_time_prediction += ((TreeSurvival) trees.get(k)).getPrediction(sample_idx).get(j);
        	      }
        	      predictions.get(0).get(sample_idx).set(j ,sample_time_prediction / num_trees);
        	    }
        	  }
        }
         
         public void computePredictionErrorInternal() {
              int i,j,k;
              double value;
              int intValue;
        	  int num_timepoints = unique_timepoints.size();

        	  // For each sample sum over trees where sample is OOB
        	  Vector<Integer> samples_oob_count = new Vector<Integer>();
        	  for (i = 0; i < num_samples; i++) {
        		  samples_oob_count.add(0);
        	  }
        	  
        	  predictions = new Vector<Vector<Vector<Double>>>();
			  for (i = 0; i < 1; i++) {
				Vector<Vector<Double>> vec2 = new Vector<Vector<Double>>();
				for (j = 0; j < num_samples; j++) {
					Vector<Double> vec = new Vector<Double>();
					for (k = 0; k < num_timepoints; k++) {
						vec.add(0.0);
					}
					vec2.add(vec);
				}
				predictions.add(vec2);
			 }

        	  for (int tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
        	    for (int sample_idx = 0; sample_idx < trees.get(tree_idx).getNumSamplesOob(); ++sample_idx) {
        	      int sampleID = trees.get(tree_idx).getOobSampleIDs().get(sample_idx);
        	      Vector<Double> tree_sample_chf = ((TreeSurvival) trees.get(tree_idx)).getPrediction(sample_idx);

        	      for (int time_idx = 0; time_idx < tree_sample_chf.size(); ++time_idx) {
        	    	value = predictions.get(0).get(sampleID).get(time_idx);
        	        predictions.get(0).get(sampleID).set(time_idx, value + tree_sample_chf.get(time_idx));
        	      }
        	      intValue = samples_oob_count.get(sampleID);
        	      samples_oob_count.set(sampleID, intValue+1);
        	    }
        	  }

        	  // Divide sample predictions by number of trees where sample is oob and compute summed chf for samples
        	  Vector<Double> sum_chf = new Vector<Double>();
        	  sum_chf.ensureCapacity(predictions.get(0).size());
        	  Vector<Integer> oob_sampleIDs = new Vector<Integer>();
        	  oob_sampleIDs.ensureCapacity(predictions.get(0).size());
        	  for (i = 0; i < predictions.get(0).size(); ++i) {
        	    if (samples_oob_count.get(i) > 0) {
        	      double sum = 0;
        	      for (j = 0; j < predictions.get(0).get(i).size(); ++j) {
        	    	value = predictions.get(0).get(i).get(j);
        	        predictions.get(0).get(i).set(j, value/ samples_oob_count.get(i));
        	        sum += predictions.get(0).get(i).get(j);
        	      }
        	      sum_chf.add(sum);
        	      oob_sampleIDs.add(i);
        	    }
        	  }

        	  // Use all samples which are OOB at least once
        	  overall_prediction_error = 1 - computeConcordanceIndex(data, sum_chf, dependent_varID, status_varID, oob_sampleIDs);
         }

         public void writeOutputInternal() {
        	  if (verbose_out) {
        	    Preferences.debug("Tree type:                         " + "Survival" + "\n", Preferences.DEBUG_ALGORITHM);
        	    Preferences.debug("Status variable name:              " + data.getVariableNames().get(status_varID) + "\n",
        	    		Preferences.DEBUG_ALGORITHM);
        	    Preferences.debug("Status variable ID:                " + status_varID + "\n", Preferences.DEBUG_ALGORITHM);
        	  }
         }
 
         public void writeConfusionFile() {

        	  // Open confusion file for writing
        	  String filename = output_prefix + ".confusion";
        	  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in writeConfusionFile");
				  return;
			  }
	          BufferedWriter bw = new BufferedWriter(fw);

        	  // Write confusion to file
	          try {
        	      bw.write("Overall OOB prediction error (1 - C): " + String.valueOf(overall_prediction_error) + "\n");
	          }
	          catch (IOException e) {
	        	  MipavUtil.displayError("IO exception on bw.write in writeConfusionFile");
	        	  System.exit(-1);;
	          }

        	  try {
	              bw.close();
        	  }
        	  catch (IOException e) {
	        	  MipavUtil.displayError("IO exception on bw.close() in writeConfusionFile");
	        	  System.exit(-1);;
	          }
        	  
        	  if (verbose_out) {
        		  Preferences.debug("Saved prediction error to file " + filename + "." + "\n", Preferences.DEBUG_ALGORITHM);
        	  }

        }

         public void writePredictionFile() {
              int i, j, k;
        	  // Open prediction file for writing
        	  String filename = output_prefix + ".prediction";
        	  File outfile = new File(filename);
			  FileWriter fw;
			  try {
			      fw = new FileWriter(outfile);
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on fw = new FileWriter(outfile) in writePredictionFile");
				  return;
			  }
			  BufferedWriter bw = new BufferedWriter(fw);

        	  // Write
			  try {
	        	  bw.write("Unique timepoints: " + "\n");
	        	  for (i = 0; i < unique_timepoints.size(); i++) {
	        		double timepoint = unique_timepoints.get(i);
	        	    bw.write(String.valueOf(timepoint) + " ");
	        	  }
	        	  bw.write("\n\n");
	
	        	  bw.write("Cumulative hazard function, one row per sample: " + "\n");
	        	  if (predict_all) {
	        	    for (k = 0; k < num_trees; ++k) {
	        	      bw.write("Tree " + String.valueOf(k) + ":" + "\n");
	        	      for (i = 0; i < predictions.size(); ++i) {
	        	        for (j = 0; j < predictions.get(i).size(); ++j) {
	        	          bw.write(String.valueOf(predictions.get(i).get(j).get(k)) + " ");
	        	        }
	        	        bw.write("\n");
	        	      }
	        	      bw.write("\n");
	        	    }
	        	  } else {
	        	    for (i = 0; i < predictions.size(); ++i) {
	        	      for (j = 0; j < predictions.get(i).size(); ++j) {
	        	        for (k = 0; k < predictions.get(i).get(j).size(); ++k) {
	        	        	 bw.write(String.valueOf(predictions.get(i).get(j).get(k)) + " ");
	        	        }
	        	        bw.write("\n");
	        	      }
	        	    }
	        	  }
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on bw.write in writePredictionFile");
				  System.exit(-1);
			  }
			  
			  try {
                  bw.close();
			  }
			  catch (IOException e) {
				  MipavUtil.displayError("IO exception on bw.close() in writePredictionFile");
				  System.exit(-1);
			  }
			  
        	  if (verbose_out) {
        		  Preferences.debug("Saved predictions to file " + filename + "." + "\n",Preferences.DEBUG_ALGORITHM);
        	  }
        }

        public void saveToFileInternal(BufferedWriter bw) {
        	// Write num_variables
	    	 try {
		    	  bw.write(String.valueOf(num_variables) + "\n");
	
		    	  // Write treetype
		    	  bw.write("TREE_SURVIVAL" + "\n");
		    	  
		    	// Write status_varID
		    	  bw.write(String.valueOf(status_varID) + "\n");
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on bw.write in saveToFileInternal");
	    		  System.exit(-1);
	    	  }
        	  
        	  // Write unique timepoints
        	  saveDVector1D(unique_timepoints, bw);
        }

        public void loadFromFileInternal(BufferedReader br) {
        	  int i,j;
        	  int varID;
              String line = null;
	    	  // Read number of variables
	    	  int num_variables_saved = 0;
	    	  TreeType treetype = null;
	    	  try {
		    	  line = br.readLine();
		    	  num_variables_saved = Integer.valueOf(line).intValue();
	
		    	  // Read treetype
		    	  line = br.readLine();
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on br.readLine in loadFromFileInternal");
	    		  System.exit(-1);
	    	  }
	    	  if (line.equals("TREE_CLASSIFICATION")) {
	    		  treetype = TreeType.TREE_CLASSIFICATION;
	    	  }
	    	  else if (line.equals("TREE_REGRESSION")) {
	    		  treetype = TreeType.TREE_REGRESSION;
	    	  }
	    	  else if (line.equals("TREE_SURVIVAL")) {
	    		  treetype = TreeType.TREE_SURVIVAL;
	    	  }
	    	  else if (line.equals("TREE_PROBABILITY")) {
	    		  treetype = TreeType.TREE_PROBABILITY;
	    	  }
	    	  if (treetype != TreeType.TREE_SURVIVAL) {
	    	    MipavUtil.displayError("Wrong treetype. Loaded file is not a classification forest.");
	    	    System.exit(-1);
	    	  }

        	  // Read status_varID
	    	  try {
		    	  line = br.readLine();
		    	  status_varID = Integer.valueOf(line).intValue();
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IO exception on br.readLine in loadFromFileInternal");
	    		  System.exit(-1);
	    	  }

        	  // Read unique timepoints
        	  unique_timepoints.clear();
        	  readDVector1D(unique_timepoints, br);

        	  for (i = 0; i < num_trees; ++i) {

        		// Read data
  	    	    Vector<Vector<Integer>> child_nodeIDs = new Vector<Vector<Integer>>();
  	    	    readVector2D(child_nodeIDs, br);
  	    	    Vector<Integer> split_varIDs = new Vector<Integer>();
  	    	    readVector1D(split_varIDs, br);
  	    	    Vector<Double> split_values = new Vector<Double>();
  	    	    readDVector1D(split_values, br);

        	    // Read chf
        	    Vector<Integer> terminal_nodes = new Vector<Integer>();
        	    readVector1D(terminal_nodes, br);
        	    Vector<Vector<Double>> chf_vector = new Vector<Vector<Double>>();
        	    readDVector2D(chf_vector, br);

        	    // Convert chf to vector with empty elements for non-terminal nodes
        	    Vector<Vector<Double>> chf = new Vector<Vector<Double>>();
        	    for (j = 0; j < child_nodeIDs.get(0).size(); j++) {
        	    	chf.add(new Vector<Double>());
        	    }
        	    for (j = 0; j < terminal_nodes.size(); ++j) {
        	      chf.set(terminal_nodes.get(j), chf_vector.get(j));
        	    }

        	    // If dependent variable not in test data, change variable IDs accordingly
        	    if (num_variables_saved > num_variables) {
        	      for (j = 0; j < split_varIDs.size(); j++) {
        	    	varID = split_varIDs.get(j);
        	        if (varID >= dependent_varID) {
        	          --varID;
        	        }
        	      }
        	    }
        	    if (num_variables_saved > num_variables + 1) {
        	      for (j = 0; j < split_varIDs.size(); j++) {
        	    	varID = split_varIDs.get(j);
        	        if (varID >= status_varID) {
        	          --varID;
        	        }
        	      }
        	    }

        	    // Create tree
        	    Tree tree = new TreeSurvival(child_nodeIDs, split_varIDs, split_values, chf, unique_timepoints,
        	        response_timepointIDs);
        	    trees.add(tree);
        	  }
        	}

	} // private class ForestSurvival
	
	
	public StochasticForests() {
		
	}
	
	public StochasticForests(TreeType treetype, boolean probability, boolean verbose_out, String dependent_variable_name,
			MemoryMode memory_mode, String input_file, int mtry, String output_prefix, int num_trees, long seed,
			int num_threads, String load_forest_filename, ImportanceMode importance_mode, int min_node_size,
			String split_select_weights_file, Vector<String> always_split_variable_names, String status_variable_name,
			boolean sample_with_replacement, Vector<String> unordered_variable_names, boolean memory_saving_splitting,
			SplitRule splitrule, String case_weights_file, boolean predict_all, double sample_fraction, double alpha,
			double minprop, boolean holdout, PredictionType prediction_type, int num_random_splits, boolean write) {
		this.treetype = treetype;
		this.probability = probability;
		this.verbose_out = verbose_out;
		this.dependent_variable_name = dependent_variable_name;
		this.memory_mode = memory_mode;
		this.input_file = input_file;
		this.mtry = mtry;
		this.output_prefix = output_prefix;
		this.num_trees = num_trees;
		this.seed = seed;
		this.num_threads = num_threads;
		this.load_forest_filename = load_forest_filename;
		this.importance_mode = importance_mode;
		this.min_node_size = min_node_size;
		this.split_select_weights_file = split_select_weights_file;
		this.always_split_variable_names = always_split_variable_names;
		this.status_variable_name = status_variable_name;
		this.sample_with_replacement = sample_with_replacement;
		this.unordered_variable_names = unordered_variable_names;
		this.memory_saving_splitting = memory_saving_splitting;
		this.splitrule = splitrule;
		this.case_weights_file = case_weights_file;
		this.predict_all = predict_all;
		this.sample_fraction = sample_fraction;
		this.alpha = alpha;
		this.minprop = minprop;
		this.holdout = holdout;
		this.prediction_type = prediction_type;
		this.num_random_splits = num_random_splits;
		this.write = write;
	}


	public void runAlgorithm() {
	   if (testUtility) {
		   Random random = new Random();
		   int i, j;
		   Vector<Integer>test = new Vector<Integer>();
		   // Split 0..9 in 1 part
		   String str = "Test equalSplit, onePart";
		   Vector<Integer>answer = new Vector<Integer>();
		   answer.addAll(Arrays.asList(new Integer[]{0,10}));
		   equalSplit(test, 0, 9, 1);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 0..7 in 4 parts
		   str = "Test equalSplit, perfectSplit0";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{0,2,4,6,8}));
		   equalSplit(test, 0, 7, 4);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 2..7 in 2 parts
		   str = "Test equalSplit, perfectSplit2";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{2,5,8}));
		   equalSplit(test, 2, 7, 2);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 13..24 in 3 parts
		   str = "Test equalSplit, perfectSplit13";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{13,17,21,25}));
		   equalSplit(test, 13, 24, 3);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 0..6 in 4 parts
		   str = "Test equalSplit, nonPerfectSplit0";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{0,2,4,6,7}));
		   equalSplit(test, 0, 6, 4);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 2..12 in 5 parts
		   str = "Test equalSplit, nonPerfectSplit2";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{2,5,7,9,11,13}));
		   equalSplit(test, 2, 12, 5);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 15..19 in 2 parts
		   str = "Test equalSplit, nonPerfectSplit15";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{15,18,20}));
		   equalSplit(test, 15, 19, 2);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 30..35 in 1 part
		   str = "Test equalSplit, nonPerfectSplit30";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{30,36}));
		   equalSplit(test, 30, 35, 1);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 0..2 in 6 parts
		   // Result should only contain 3 parts
		   str = "Test equalSplit, moreParts1";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{0,1,2,3}));
		   equalSplit(test, 0, 2, 6);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 0..2 in 4 parts
		   // Result should only contain 3 parts
		   str = "Test equalSplit, moreParts2";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{0,1,2,3}));
		   equalSplit(test, 0, 2, 4);
		   equalVectorInteger(answer, test, str);
		   
		   // Split 0..2 in 3 parts
		   // Result should contain only 3 parts
		   str = "Test equalSplit, moreParts3";
		   test.clear();
		   answer.clear();
		   answer.addAll(Arrays.asList(new Integer[]{0,1,2,3}));
		   equalSplit(test, 0, 2, 3);
		   equalVectorInteger(answer, test, str);
		   
		   str = "Test readWrite1D, int1";
		   File outfile = new File("testfile1d");
		   FileWriter fw;
		   try {
		       fw = new FileWriter(outfile);
		   }
		   catch (IOException e) {
			   MipavUtil.displayError("IO exception on fw = new FileWriter(outfile)");
			   return;
		   }
           BufferedWriter bw = new BufferedWriter(fw);
		   Vector<Integer> expect = new Vector<Integer>();
		   expect.addAll(Arrays.asList(new Integer[]{1,2,3,4,5,6}));
		   saveVector1D(expect, bw);
		   try {
		       bw.close();
		   }
		   catch (IOException e) {
			   MipavUtil.displayError("IO exception on bw.close()");
			   return;
		   }
		   
		   File file = new File("testfile1d");
	  	   BufferedReader infile;
	  	   try {
	  	       infile = new BufferedReader(new FileReader(file));
	  	   }
	  	   catch (FileNotFoundException e) {
	  		   MipavUtil.displayError("Could not find file testfile1d");
	  		   return;
	  	   }
	  	   test.clear();
	  	   readVector1D(test, infile);
	  	   try {
	  		   infile.close();
	  	   }
	  	   catch (IOException e) {
	  		   MipavUtil.displayError("IO exception on infile.close()");
	  		   return;
	  	   }
	  	   equalVectorInteger(expect, test, str);
	  	   
	  	   str = "Test readWrite1D, double1";
		   outfile = new File("testfile1d");
		   try {
		       fw = new FileWriter(outfile);
		   }
		   catch (IOException e) {
			   MipavUtil.displayError("IO exception on fw = new FileWriter(outfile)");
			   return;
		   }
           bw = new BufferedWriter(fw);
		   Vector<Double> Dexpect = new Vector<Double>();
		   Dexpect.addAll(Arrays.asList(new Double[]{1.5,4.5,10.2,0.0,-5.9}));
		   saveDVector1D(Dexpect, bw);
		   try {
		       bw.close();
		   }
		   catch (IOException e) {
			   MipavUtil.displayError("IO exception on bw.close()");
			   return;
		   }
		   
		   file = new File("testfile1d");
	  	   try {
	  	       infile = new BufferedReader(new FileReader(file));
	  	   }
	  	   catch (FileNotFoundException e) {
	  		   MipavUtil.displayError("Could not find file testfile1d");
	  		   return;
	  	   }
	  	   Vector<Double> Dtest = new Vector<Double>();
	  	   readDVector1D(Dtest, infile);
	  	   try {
	  		   infile.close();
	  	   }
	  	   catch (IOException e) {
	  		   MipavUtil.displayError("IO exception on infile.close()");
	  		   return;
	  	   }
	  	   equalVectorDouble(Dexpect, Dtest, str);
	  	   
	  	   str = "Test readWrite2D, int1";
	  	   outfile = new File("testfile2d");
		   try {
		       fw = new FileWriter(outfile);
		   }
		   catch (IOException e) {
			   MipavUtil.displayError("IO exception on fw = new FileWriter(outfile)");
			   return;
		   }
           bw = new BufferedWriter(fw);
           Vector<Integer> expect1 = new Vector<Integer>();
           expect1.addAll(Arrays.asList(new Integer[]{1,2,3,4,5,6}));
           Vector<Integer> expect2 = new Vector<Integer>();
           expect2.addAll(Arrays.asList(new Integer[]{7,8,9,10}));
           Vector<Integer> expect3 = new Vector<Integer>();
           expect3.addAll(Arrays.asList(new Integer[]{11,12,13,14,15}));
           Vector<Vector<Integer>> expectAll = new Vector<Vector<Integer>>();
           expectAll.add(expect1);
           expectAll.add(expect2);
           expectAll.add(expect3);
           saveVector2D(expectAll, bw);
           try {
        	   bw.close();
           }
           catch (IOException e) {
        	   MipavUtil.displayError("IO exception on bw.close()");
        	   return;
           }
           

		   file = new File("testfile2d");
	  	   try {
	  	       infile = new BufferedReader(new FileReader(file));
	  	   }
	  	   catch (FileNotFoundException e) {
	  		   MipavUtil.displayError("Could not find file testfile2d");
	  		   return;
	  	   }
	  	   Vector<Vector<Integer>> testAll = new Vector<Vector<Integer>>();
	  	   readVector2D(testAll, infile);
	  	   try {
	  		   infile.close();
	  	   }
	  	   catch(IOException e) {
	  		   MipavUtil.displayError("IO exception on infile.close()");
	  		   return;
	  	   }
	  	   equalVectorVectorInteger(expectAll, testAll, str);
	  	   
	  	 str = "Test readWrite2D, double1";
	  	 outfile = new File("testfile2d");
	     try {
		     fw = new FileWriter(outfile);
		 }
		 catch (IOException e) {
			 MipavUtil.displayError("IO exception on fw = new FileWriter(outfile)");
			 return;
		 }
         bw = new BufferedWriter(fw);
         Vector<Double> Dexpect1 = new Vector<Double>();
         Dexpect1.addAll(Arrays.asList(new Double[]{1.1,2.4,3.0,4.3,5.9,6.7}));
         Vector<Double> Dexpect2 = new Vector<Double>();
         Dexpect2.addAll(Arrays.asList(new Double[]{7.2,8.1,9.0,10.1}));
         Vector<Double> Dexpect3 = new Vector<Double>();
         Dexpect3.addAll(Arrays.asList(new Double[]{11.3,12.4,13.2,14.7,15.8}));
         Vector<Vector<Double>> DexpectAll = new Vector<Vector<Double>>();
         DexpectAll.add(Dexpect1);
         DexpectAll.add(Dexpect2);
         DexpectAll.add(Dexpect3);
         saveDVector2D(DexpectAll, bw);
         try {
      	   bw.close();
         }
         catch (IOException e) {
      	   MipavUtil.displayError("IO exception on bw.close()");
      	   return;
         }
         

	     file = new File("testfile2d");
  	     try {
  	         infile = new BufferedReader(new FileReader(file));
  	     }
  	     catch (FileNotFoundException e) {
  		     MipavUtil.displayError("Could not find file testfile2d");
  		     return;
  	     }
  	     Vector<Vector<Double>> DtestAll = new Vector<Vector<Double>>();
  	     readDVector2D(DtestAll, infile);
  	     try {
  		     infile.close();
  	     }
  	     catch(IOException e) {
  		     MipavUtil.displayError("IO exception on infile.close()");
  		     return;
  	     }
  	     equalVectorVectorDouble(DexpectAll, DtestAll, str);
  	     
  	     str = "Test drawWithoutReplacementSkip, small_small1";
  	     Vector<Integer>result = new Vector<Integer>();
  	     HashMap<Integer, Integer> counts = new HashMap<Integer, Integer>();
  	     int max = 9;
  	     Vector<Integer> skip = new Vector<Integer>();
  	     skip.add(7);
  	     int num_samples = 4;
  	     int num_replicates = 10000;
  	     int expected_count = num_samples * num_replicates / max;
  	     
  	     for (i = 0; i < num_replicates; i++) {
  	    	 result.clear();
  	    	 drawWithoutReplacementSkip(result, random, max + 1, skip, num_samples);
  	    	 for (j = 0; j < result.size(); j++) {
  	    	     int idx = result.get(j);
  	    	     if (counts.get(idx) == null) {
  	    	    	 counts.put(idx, 1);
  	    	     }
  	    	     else {
  	    	         counts.put(idx, counts.get(idx)+1);
  	    	     }
  	    	 }
  	     } 
  	     
  	     // Check if counts are expected +-5%
  	    Object countsObject[] = counts.values().toArray();
		boolean success = true;
		boolean near;
		int value;
  	     for (i = 0; i < counts.size(); i++) {
  	    	 if (countsObject[i] == null) {
  	    		 value = 0;
  	    	 }
  	    	 else {
  	    		 value = (int)countsObject[i];
  	    	 }
  	    	 near = expectNear(expected_count, value, expected_count * 0.05, str);
  	    	 if (!near) {
  	    		 success = false;
  	    	 }
  	     }
  	     if (counts.get(skip.get(0)) != null) {
  	    	 System.out.println("In " + str + " counts.get(skip.get(0)) = " + 
  	                     counts.get(skip.get(0)) + " instead of null for zero count");
  	    	 success = false;
  	     }
  	     if (success) {
  	    	 System.out.println(str + " passed");
  	     }
  	     
  	     str = "Test drawWithoutReplacementSkip, small_small2";
	     result.clear();
	     counts.clear();
	     max = 9;
	     skip.clear();
	     skip.add(0);
	     num_samples = 4;
	     num_replicates = 10000;
	     expected_count = num_samples * num_replicates / max;
	     
	     for (i = 0; i < num_replicates; i++) {
	    	 result.clear();
	    	 drawWithoutReplacementSkip(result, random, max + 1, skip, num_samples);
	    	 for (j = 0; j < result.size(); j++) {
	    	     int idx = result.get(j);
	    	     if (counts.get(idx) == null) {
	    	    	 counts.put(idx, 1);
	    	     }
	    	     else {
	    	         counts.put(idx, counts.get(idx)+1);
	    	     }
	    	 }
	     } 
	     
	     // Check if counts are expected +-5%
	     countsObject = counts.values().toArray();
		 success = true;
	     for (i = 0; i < counts.size(); i++) {
	    	 if (countsObject[i] == null) {
  	    		 value = 0;
  	    	 }
  	    	 else {
  	    		 value = (int)countsObject[i];
  	    	 }
	    	 near = expectNear(expected_count, value, expected_count * 0.05, str);
	    	 if (!near) {
	    		 success = false;
	    	 }
	     }
	     if (counts.get(skip.get(0)) != null) {
	    	 System.out.println("In " + str + " counts.get(skip.get(0)) = " + 
	                     counts.get(skip.get(0)) + " instead of null for zero count");
	    	 success = false;
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test drawWithoutReplacementSkip, small_small3";
	     result.clear();
	     counts.clear();
	     max = 9;
	     skip.clear();
	     skip.add(9);
	     num_samples = 4;
	     num_replicates = 10000;
	     expected_count = num_samples * num_replicates / max;
	     
	     for (i = 0; i < num_replicates; i++) {
	    	 result.clear();
	    	 drawWithoutReplacementSkip(result, random, max + 1, skip, num_samples);
	    	 for (j = 0; j < result.size(); j++) {
	    	     int idx = result.get(j);
	    	     if (counts.get(idx) == null) {
	    	    	 counts.put(idx, 1);
	    	     }
	    	     else {
	    	         counts.put(idx, counts.get(idx)+1);
	    	     }
	    	 }
	     } 
	     
	     // Check if counts are expected +-5%
	     countsObject = counts.values().toArray();
		 success = true;
	     for (i = 0; i < counts.size(); i++) {
	    	 if (countsObject[i] == null) {
  	    		 value = 0;
  	    	 }
  	    	 else {
  	    		 value = (int)countsObject[i];
  	    	 }

	    	 near = expectNear(expected_count, value, expected_count * 0.05, str);
	    	 if (!near) {
	    		 success = false;
	    	 }
	     }
	     if (counts.get(skip.get(0)) != null) {
	    	 System.out.println("In " + str + " counts.get(skip.get(0)) = " + 
	                     counts.get(skip.get(0)) + " instead of null for zero count");
	    	 success = false;
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test drawWithoutReplacementSkip, small_large1";
	     result.clear();
	     counts.clear();
	     max = 1000;
	     skip.clear();
	     skip.add(7);
	     num_samples = 50;
	     num_replicates = 100000;
	     expected_count = num_samples * num_replicates / max;
	     
	     for (i = 0; i < num_replicates; i++) {
	    	 result.clear();
	    	 drawWithoutReplacementSkip(result, random, max + 1, skip, num_samples);
	    	 for (j = 0; j < result.size(); j++) {
	    	     int idx = result.get(j);
	    	     if (counts.get(idx) == null) {
	    	    	 counts.put(idx, 1);
	    	     }
	    	     else {
	    	         counts.put(idx, counts.get(idx)+1);
	    	     }
	    	 }
	     } 
	     
	     // Check if counts are expected +-10%
	     countsObject = counts.values().toArray();
		 success = true;
	     for (i = 0; i < counts.size(); i++) {
	    	 if (countsObject[i] == null) {
  	    		 value = 0;
  	    	 }
  	    	 else {
  	    		 value = (int)countsObject[i];
  	    	 }
	    	 near = expectNear(expected_count, value, expected_count * 0.1, str);
	    	 if (!near) {
	    		 success = false;
	    	 }
	     }
	     if (counts.get(skip.get(0)) != null) {
	    	 System.out.println("In " + str + " counts.get(skip.get(0)) = " + 
	                     counts.get(skip.get(0)) + " instead of null for zero count");
	    	 success = false;
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test drawWithoutReplacementSkip, large_large1";
	     result.clear();
	     counts.clear();
	     max = 1000;
	     skip.clear();
	     skip.add(7);
	     num_samples = 500;
	     num_replicates = 10000;
	     expected_count = num_samples * num_replicates / max;
	     
	     for (i = 0; i < num_replicates; i++) {
	    	 result.clear();
	    	 drawWithoutReplacementSkip(result, random, max + 1, skip, num_samples);
	    	 for (j = 0; j < result.size(); j++) {
	    	     int idx = result.get(j);
	    	     if (counts.get(idx) == null) {
	    	    	 counts.put(idx, 1);
	    	     }
	    	     else {
	    	         counts.put(idx, counts.get(idx)+1);
	    	     }
	    	 }
	     } 
	     
	     // Check if counts are expected +- 5%
	     countsObject = counts.values().toArray();
		 success = true;
	     for (i = 0; i < counts.size(); i++) {
	    	 if (countsObject[i] == null) {
  	    		 value = 0;
  	    	 }
  	    	 else {
  	    		 value = (int)countsObject[i];
  	    	 }
	    	 near = expectNear(expected_count, value, expected_count * 0.05, str);
	    	 if (!near) {
	    		 success = false;
	    	 }
	     }
	     if (counts.get(skip.get(0)) != null) {
	    	 System.out.println("In " + str + " counts.get(skip.get(0)) = " + 
	                     counts.get(skip.get(0)) + " instead of null for zero count");
	    	 success = false;
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test mostFrequentClassCount, notEqual1";
	     Vector<Integer>class_count = new Vector<Integer>();
	     class_count.addAll(Arrays.asList(new Integer[]{0,4,7,3,2,1,8}));
	     int ans = mostFrequentClass(class_count, random);
	     if (ans != 6) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 6");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test mostFrequentClassCount, notEqual2";
	     class_count.clear();
	     class_count.addAll(Arrays.asList(new Integer[]{5,4,3,2,1}));
	     ans = mostFrequentClass(class_count, random);
	     if (ans != 0) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 0");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test mostFrequentClassCount, equal1";
	     class_count.clear();
	     class_count.addAll(Arrays.asList(new Integer[]{5,5,5,5}));
	     ans = mostFrequentClass(class_count, random);
	     if ((ans < 0) || (ans > 3)) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 0 to 3");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test mostFrequentClassCount, equal2";
	     class_count.clear();
	     class_count.addAll(Arrays.asList(new Integer[]{4,5,5,4}));
	     ans = mostFrequentClass(class_count, random);
	     if ((ans < 1) || (ans > 2)) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 1 to 2");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test mostFrequentValue, notEqual1";
	     HashMap <Double, Integer> class_count2 = new HashMap<Double, Integer>();
	     class_count2.put(1.0,5);
	     class_count2.put(2.0,7);
	     class_count2.put(3.0,10);
	     double dans = mostFrequentValue(class_count2, random);
	     if (dans != 3.0) {
	    	 System.out.println("In " + str + " answer = " + dans + " instead of the correct 3.0");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test mostFrequentValue, notEqual2";
	     class_count2.clear();
	     class_count2.put(10.1,15);
	     class_count2.put(2.5,12);
	     class_count2.put(30.0,10);
	     dans = mostFrequentValue(class_count2, random);
	     if (dans != 10.1) {
	    	 System.out.println("In " + str + " answer = " + dans + " instead of the correct 10.1");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test mostFrequentValue, equal1";
	     class_count2.clear();
	     class_count2.put(1.0,10);
	     class_count2.put(2.0,15);
	     class_count2.put(3.0,15);
	     dans = mostFrequentValue(class_count2, random);
	     if ((dans != 2.0) && (dans != 3.0)) {
	    	 System.out.println("In " + str + " answer = " + dans + " instead of the correct 2.0 or 3.0");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test mostFrequentValue, equal2";
	     class_count2.clear();
	     class_count2.put(10.0,30);
	     class_count2.put(11.0,30);
	     class_count2.put(15.0,29);
	     dans = mostFrequentValue(class_count2, random);
	     if ((dans != 10.0) && (dans != 11.0)) {
	    	 System.out.println("In " + str + " answer = " + dans + " instead of the correct 10.0 or 11.0");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test mostFrequentValue, equal3";
	     class_count2.clear();
	     class_count2.put(3.0,10);
	     class_count2.put(5.0,500);
	     class_count2.put(6.0,500);
	     dans = mostFrequentValue(class_count2, random);
	     if ((dans != 5.0) && (dans != 6.0)) {
	    	 System.out.println("In " + str + " answer = " + dans + " instead of the correct 5.0 or 6.0");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test beautifyTime, seconds1";
	     String res = beautifyTime(0);
	     if (!res.equals("0 seconds")) {
	    	 System.out.println("In " + str + " answer = " + res + " instead of the correct 0 seconds");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test beautifyTime, seconds2";
	     res = beautifyTime(30);
	     if (!res.equals("30 seconds")) {
	    	 System.out.println("In " + str + " answer = " + res + " instead of the correct 30 seconds");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test beautifyTime, minutes1";
	     res = beautifyTime(60);
	     if (!res.equals("1 minute, 0 seconds")) {
	    	 System.out.println("In " + str + " answer = " + res + 
	    			 " instead of the correct 1 minute, 0 seconds");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test beautifyTime, minutes2";
	     res = beautifyTime(2317);
	     if (!res.equals("38 minutes, 37 seconds")) {
	    	 System.out.println("In " + str + " answer = " + res + 
	    			 " instead of the correct 38 minutes, 37 seconds");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test beautifyTime, hours1";
	     res = beautifyTime(3600);
	     if (!res.equals("1 hour, 0 minutes, 0 seconds")) {
	    	 System.out.println("In " + str + " answer = " + res + 
	    			 " instead of the correct 1 hour, 0 minutes, 0 seconds");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test beautifyTime, hours2";
	     res = beautifyTime(13498);
	     if (!res.equals("3 hours, 44 minutes, 58 seconds")) {
	    	 System.out.println("In " + str + " answer = " + res + 
	    			 " instead of the correct 3 hours, 44 minutes, 58 seconds");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test beautifyTime, days1";
	     res = beautifyTime(86400);
	     if (!res.equals("1 day, 0 hours, 0 minutes, 0 seconds")) {
	    	 System.out.println("In " + str + " answer = " + res + 
	    			 " instead of the correct 1 day, 0 hours, 0 minutes, 0 seconds");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test beautifyTime, days2";
	     res = beautifyTime(287345);
	     if (!res.equals("3 days, 7 hours, 49 minutes, 5 seconds")) {
	    	 System.out.println("In " + str + " answer = " + res + 
	    			 " instead of the correct 3 days, 7 hours, 49 minutes, 5 seconds");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test0";
	     ans = roundToNextMultiple(0, 4);
	     if (ans != 0) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 0");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test1";
	     ans = roundToNextMultiple(1, 4);
	     if (ans != 4) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 4");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test2";
	     ans = roundToNextMultiple(2, 4);
	     if (ans != 4) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 4");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test3";
	     ans = roundToNextMultiple(3, 4);
	     if (ans != 4) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 4");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test4";
	     ans = roundToNextMultiple(4, 4);
	     if (ans != 4) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 4");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test5";
	     ans = roundToNextMultiple(5, 4);
	     if (ans != 8) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 8");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test6";
	     ans = roundToNextMultiple(6, 4);
	     if (ans != 8) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 8");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test7";
	     ans = roundToNextMultiple(7, 4);
	     if (ans != 8) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 8");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test8";
	     ans = roundToNextMultiple(8, 4);
	     if (ans != 8) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 8");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test roundToNextMultiple, test9";
	     ans = roundToNextMultiple(9, 4);
	     if (ans != 12) {
	    	 System.out.println("In " + str + " answer = " + ans + " instead of the correct 12");
	     }
	     else {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test splitString, test1";
	     String test_string = "abc,def,ghi";
	     Vector<String> splitted_string = new Vector<String>();
	     splitString(splitted_string, test_string, ",");
	     Vector<String>expect_string = new Vector<String>();
	     expect_string.addAll(Arrays.asList(new String[]{"abc","def","ghi"}));
	     if (splitted_string.size() != expect_string.size()) {
	    	 System.out.println("In " + str + " splitted_string.size() = " + splitted_string.size() +
	    			 " instead of the correct " + expect_string.size());
	     }
	     success = true;
	     for (i = 0; i < splitted_string.size(); i++) {
	    	 if (!splitted_string.get(i).equals(expect_string.get(i))) {
	    		 System.out.println(" In " + str + " for i = " + i + " have " + splitted_string.get(i) +
	    				 " instead of the correct " + expect_string.get(i));
	    		 success = false;
	    	 }
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test splitString, test2";
	     test_string = "abc";
	     splitted_string.clear();
	     splitString(splitted_string, test_string, ",");
	     expect_string.clear();
	     expect_string.addAll(Arrays.asList(new String[]{"abc"}));
	     if (splitted_string.size() != expect_string.size()) {
	    	 System.out.println("In " + str + " splitted_string.size() = " + splitted_string.size() +
	    			 " instead of the correct " + expect_string.size());
	     }
	     success = true;
	     for (i = 0; i < splitted_string.size(); i++) {
	    	 if (!splitted_string.get(i).equals(expect_string.get(i))) {
	    		 System.out.println(" In " + str + " for i = " + i + " have " + splitted_string.get(i) +
	    				 " instead of the correct " + expect_string.get(i));
	    		 success = false;
	    	 }
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test splitString, test3";
	     test_string = "a-b-c";
	     splitted_string.clear();
	     splitString(splitted_string, test_string, "-");
	     expect_string.clear();
	     expect_string.addAll(Arrays.asList(new String[]{"a","b","c"}));
	     if (splitted_string.size() != expect_string.size()) {
	    	 System.out.println("In " + str + " splitted_string.size() = " + splitted_string.size() +
	    			 " instead of the correct " + expect_string.size());
	     }
	     success = true;
	     for (i = 0; i < splitted_string.size(); i++) {
	    	 if (!splitted_string.get(i).equals(expect_string.get(i))) {
	    		 System.out.println(" In " + str + " for i = " + i + " have " + splitted_string.get(i) +
	    				 " instead of the correct " + expect_string.get(i));
	    		 success = false;
	    	 }
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test splitString, test4";
	     test_string = "";
	     splitted_string.clear();
	     splitString(splitted_string, test_string, ",");
	     expect_string.clear();
	     if ((test_string.length() == 0) && (expect_string.size() == 0)) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test shuffleAndSplit, test1";
	     Vector<Integer> first_part = new Vector<Integer>();
	     Vector<Integer> second_part = new Vector<Integer>();
	     shuffleAndSplit(first_part, second_part, 10, 3, random);
	     success = true;
	     if (first_part.size() != 3) {
	    	 System.out.println("In " + str + " first_part.size() = " + first_part.size() +
	    			 " instead of the correct 3");
	    	 success = false;
	     }
	     if (second_part.size() != 7) {
	    	 System.out.println("In " + str + " second_part.size() = " + second_part.size() +
	    			 " instead of the correct 7");
	    	 success = false;	 
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test shuffleAndSplit, test2";
	     first_part.clear();
	     second_part.clear();
	     shuffleAndSplit(first_part, second_part, 100, 63, random);
	     success = true;
	     if (first_part.size() != 63) {
	    	 System.out.println("In " + str + " first_part.size() = " + first_part.size() +
	    			 " instead of the correct 63");
	    	 success = false;
	     }
	     if (second_part.size() != 37) {
	    	 System.out.println("In " + str + " second_part.size() = " + second_part.size() +
	    			 " instead of the correct 37");
	    	 success = false;	 
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test shuffleAndSplit, test3";
	     first_part.clear();
	     second_part.clear();
	     shuffleAndSplit(first_part, second_part, 1, 1, random);
	     success = true;
	     if (first_part.size() != 1) {
	    	 System.out.println("In " + str + " first_part.size() = " + first_part.size() +
	    			 " instead of the correct 1");
	    	 success = false;
	     }
	     if (second_part.size() != 0) {
	    	 System.out.println("In " + str + " second_part.size() = " + second_part.size() +
	    			 " instead of the correct 0");
	    	 success = false;	 
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test shuffleAndSplit, test4";
	     first_part.clear();
	     second_part.clear();
	     shuffleAndSplit(first_part, second_part, 3, 0, random);
	     success = true;
	     if (first_part.size() != 0) {
	    	 System.out.println("In " + str + " first_part.size() = " + first_part.size() +
	    			 " instead of the correct 0");
	    	 success = false;
	     }
	     if (second_part.size() != 3) {
	    	 System.out.println("In " + str + " second_part.size() = " + second_part.size() +
	    			 " instead of the correct 3");
	    	 success = false;	 
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test maxstatPValueLau92, test1";
	     Vector<Double> p_expect = new Vector<Double>();
	     p_expect.addAll(Arrays.asList(new Double[]{1.0, 0.967882898076573, 0.819678995766699,
	    	      0.463872768757117, 0.189802453892004, 0.0578438845691903, 0.0133240079314344, 
	    	      0.00233924318507284, 0.00031467775847682, 3.25492795226314e-05, 2.59527010078785e-06,
	    	      1.59801511710768e-07, 7.6090999589879e-09, 2.80479710245055e-10, 8.01032048074225e-12,
	    	      1.77366479130538e-13, 3.04652951223938e-15, 4.06114941874027e-17,
	    	      4.20307813816918e-19, 3.37831711514353e-21 }));
	     
	     // Create sequence 0.5..10
	     Vector<Double> test_b = new Vector<Double>();
	     double val = 0.5;
	     for (i = 0; i < 20; i++) {
	         test_b.add(val);
	         val = val + 0.5;
	     }
	     
	     // Compute approximation
	     double minprop = 0.1;
	     Vector<Double>p = new Vector<Double>();
	     for (i = 0; i < test_b.size(); i++) {
	    	 p.add(maxstatPValueLau92(test_b.get(i), minprop, 1 - minprop));
	     }
	     
	     // Compare with expectation
	     success = true;
	     for (i = 0; i < p.size(); i++) {
	         if (p.get(i) < p_expect.get(i) - Math.abs(p_expect.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " p.get("+i+") is too low");
	        	 success = false;
	         }
	         else if (p.get(i) > p_expect.get(i) + Math.abs(p_expect.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " p.get("+i+") is too high");
	        	 success = false;
	         }
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test maxstatPValueLau94, test1";
	     Vector<Integer> m = new Vector<Integer>();
	     m.addAll(Arrays.asList(new Integer[]{0, 3, 7, 8, 12, 15, 17, 18, 21, 25, 
	    		 27, 30, 31, 35, 36, 39, 44, 46 }));
	     p_expect.clear();
	     p_expect.addAll(Arrays.asList(new Double[]{3.24640516569147, 2.10411448384791, 1.07190625979408,
	    	      0.426066236447909, 0.131558039703021, 0.0314590549699589, 0.00581093458428213,
	    	      0.000826972741261553, 9.03968770946711e-05, 7.55926672751076e-06, 4.80774063093186e-07,
	    	      2.30447718702542e-08, 8.19448285148733e-10, 2.09519635709089e-11, 3.56382497736666e-13,
	    	      3.02490041849519e-15, -4.80261133649249e-17, -1.6110288577566e-18,
	    	      -2.62248821317204e-20, -2.84175352170915e-22 }));
	     
	     // test_b has sequence 0.5..10
	     
	     // Compute approximation
	     minprop = 0.1;
	     int N = 50;
	     p.clear();
	     for (i = 0; i < test_b.size(); i++) {
	    	 p.add(maxstatPValueLau94(test_b.get(i), minprop, 1 - minprop, N, m));
	     }
	     
	     // Compare with expectation
	     success = true;
	     for (i = 0; i < p.size(); i++) {
	         if (p.get(i) < p_expect.get(i) - Math.abs(p_expect.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " p.get("+i+") is too low");
	        	 success = false;
	         }
	         else if (p.get(i) > p_expect.get(i) + Math.abs(p_expect.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " p.get("+i+") is too high");
	        	 success = false;
	         }
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test dstnorm, test1";
	     Vector<Double> expectDst = new Vector<Double>();
	     expectDst.addAll(Arrays.asList(new Double[]{0.000133830225764885, 0.00087268269504576,
	    	      0.00443184841193801, 0.0175283004935685, 0.0539909665131881, 0.129517595665892,
	    	      0.241970724519143, 0.3520653267643, 0.398942280401433, 0.3520653267643, 0.241970724519143,
	    	      0.129517595665892, 0.0539909665131881, 0.0175283004935685, 0.00443184841193801,
	    	      0.00087268269504576, 0.000133830225764885 }));
	     
	     // Create sequence -4, 4, by 0.5
	     Vector<Double> test_x = new Vector<Double>();
	     val = -4.0;
	     for (i = 0; i < 17; i++) {
	    	 test_x.add(val);
	    	 val = val + 0.5;
	     }
	     
	     // Compute density
	     Vector<Double> density = new Vector<Double>();
	     for (i = 0; i < test_x.size(); i++) {
	    	 density.add(dstdnorm(test_x.get(i)));
	     }
	     
	     // Compare with expectation
	     success = true;
	     for (i = 0; i < density.size(); i++) {
	    	 if (density.get(i) < expectDst.get(i) - Math.abs(expectDst.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " density.get("+i+") is too low");
	        	 success = false;
	         }
	         else if (density.get(i) > expectDst.get(i) + Math.abs(expectDst.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " density.get("+i+") is too high");
	        	 success = false;
	         }	 
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test pstdnorm, test1";
	     Vector<Double> expectPst = new Vector<Double>();
	     expectPst.addAll(Arrays.asList(new Double[]{3.16712418331199e-05, 0.000232629079035525,
	    	      0.00134989803163009, 0.00620966532577613, 0.0227501319481792, 0.0668072012688581,
	    	      0.158655253931457, 0.308537538725987, 0.5, 0.691462461274013, 0.841344746068543,
	    	      0.933192798731142, 0.977249868051821, 0.993790334674224, 0.99865010196837,
	    	      0.999767370920964, 0.999968328758167 }));
	     
	     // Use test_x -4,4 by 0.5 sequence
	  // Compute distribution
	     Vector<Double> dist = new Vector<Double>();
	     for (i = 0; i < test_x.size(); i++) {
	    	 dist.add(pstdnorm(test_x.get(i)));
	     }
	     
	     // Compare with expectation
	     success = true;
	     for (i = 0; i < dist.size(); i++) {
	    	 if (dist.get(i) < expectPst.get(i) - Math.abs(expectPst.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " dist.get("+i+") is too low");
	        	 success = false;
	         }
	         else if (dist.get(i) > expectPst.get(i) + Math.abs(expectPst.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " dist.get("+i+") is too high");
	        	 success = false;
	         }	 
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test adjustPvalues, test1";
	     p.clear();
	     p.addAll(Arrays.asList(new Double[]{0.575155046407955, 0.817953853056094, 0.119065433061771,
	    	      0.943789021783783, 0.897129975905795, 0.086333312789587, 0.644858724286655, 
	    	      0.205849377808347, 0.492175460488491, 0.655841439284439, 0.220921371984431,
	    	      0.718986362304244, 0.688588400057994, 0.911867952434092, 0.578319462345744,
	    	      0.0739515289321684, 0.618589434095213, 0.0492273640304203, 0.483080935233713,
	    	      0.636362004751351, 0.285600042559897, 0.827448656690798, 0.304889487308354, 
	    	      0.466068200259841, 0.531945286062773, 0.189079625396729, 0.000124148072892801,
	    	      0.00161395372907077, 0.0626223946736039, 2.10159024072429e-05, 0.000611494315340942,
	    	      0.0068319089314331, 9.81478332360736e-05, 0.000105260958830543, 0.000132598801923585,
	    	      0.000225455732762676, 0.0003795380309271, 0.00330242962313738, 0.00705922565893261,
	    	      0.00880512860101142, 0.0211501681388388, 0.00523699658057458, 0.0828110328240387,
	    	      2.35405350797934e-07, 2.57684187404011e-05, 0.0605329775053071, 0.00940103983967277,
	    	      0.0112979808322889, 0.000156850331255678, 0.00353834177611007 }));
	     
	     Vector<Double> expectP = new Vector<Double>();
	     expectP.addAll(Arrays.asList(new Double[]{0.741435208135569, 0.880264528394466, 0.212616844753163,
	    	      0.943789021783783, 0.930477502483768, 0.159876505165902, 0.762606324749347,
	    	      0.343082296347245, 0.683577028456238, 0.762606324749347, 0.356324793523276,
	    	      0.798873735893604, 0.782486818247721, 0.930477502483768, 0.741435208135569,
	    	      0.147903057864337, 0.762606324749347, 0.11188037279641, 0.683577028456238,
	    	      0.762606324749347, 0.446250066499839, 0.880264528394466, 0.461953768649021,
	    	      0.683577028456238, 0.718844981165909, 0.325999354132291, 0.000947134299454175,
	    	      0.00672480720446156, 0.130463322236675, 0.000429473645673352, 0.0027795196151861,
	    	      0.0207624284086253, 0.000947134299454175, 0.000947134299454175, 0.000947134299454175,
	    	      0.00125253184868153, 0.0018976901546355, 0.0126369349146788, 0.0207624284086253,
	    	      0.0244586905583651, 0.0503575431877115, 0.0174566552685819, 0.159251986200075,
	    	      1.17702675398967e-05, 0.000429473645673352, 0.130463322236675, 0.0247395785254547,
	    	      0.0282449520807222, 0.000980314570347985, 0.0126369349146788 }));
	     
	     // Adjist p-values
	     Vector<Double> adjusted = adjustPvalues(p);
	     
	     // Compare with expectation
	     success = true;
	     for (i = 0; i < p.size(); i++) {
	    	 if (adjusted.get(i) < expectP.get(i) - Math.abs(expectP.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " adjusted.get("+i+") is too low");
	        	 success = false;
	         }
	         else if (adjusted.get(i) > expectP.get(i) + Math.abs(expectP.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " adjusted.get("+i+") is too high");
	        	 success = false;
	         }	 
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test order, test1";
	     Vector<Double> x = new Vector<Double>();
	     x.addAll(Arrays.asList(new Double[]{0.287577520124614, 0.788305135443807, 0.4089769218117,
	    	      0.883017404004931, 0.940467284293845, 0.0455564993899316, 0.528105488047004,
	    	      0.892419044394046, 0.551435014465824, 0.456614735303447, 0.956833345349878,
	    	      0.453334156190977, 0.677570635452867, 0.572633401956409, 0.102924682665616,
	    	      0.899824970401824, 0.24608773435466, 0.0420595335308462, 0.327920719282702,
	    	      0.954503649147227, 0.889539316063747, 0.6928034061566, 0.640506813768297,
	    	      0.994269776623696, 0.655705799115822, 0.708530468167737, 0.544066024711356,
	    	      0.59414202044718, 0.28915973729454, 0.147113647311926, 0.963024232536554, 
	    	      0.902299045119435, 0.690705278422683, 0.795467417687178, 0.0246136845089495, 
	    	      0.477795971091837, 0.758459537522867, 0.216407935833558, 0.318181007634848,
	    	      0.231625785352662, 0.142800022382289, 0.414546335814521, 0.413724326295778,
	    	      0.368845450924709, 0.152444747742265, 0.13880606344901, 0.233034099452198,
	    	      0.465962450252846, 0.265972640365362, 0.857827715342864}));
	     Vector<Integer> expect_inc = new Vector<Integer>();
	     expect_inc.addAll(Arrays.asList(new Integer[]{34, 17, 5, 14, 45, 40, 29, 44, 37, 39, 46, 16, 48, 0,
	    	      28, 38, 18, 43, 2, 42, 41, 11, 9, 47, 35, 6, 26, 8, 13, 27, 22, 24, 12, 32, 21, 25,
	    	      36, 1, 33, 49, 3, 20, 7, 15, 31, 4, 19, 10, 30, 23 }));
	     Vector<Integer> expect_dec = new Vector<Integer>();
	     expect_dec.addAll(Arrays.asList(new Integer[]{23, 30, 10, 19, 4, 31, 15, 7, 20, 3, 49, 33, 1, 36, 25,
	    	      21, 32, 12, 24, 22, 27, 13, 8, 26, 6, 35, 47, 9, 11, 41, 42, 2, 43, 18, 38, 28, 0, 48, 16,
	    	      46, 39, 37, 44, 29, 40, 45, 14, 5, 17, 34 }));
	     
	     // Order
	     Vector<Integer> inc = order(x, false);
	     Vector<Integer> dec = order(x, true);
	     
	     equalVectorInteger(expect_inc, inc, str + " increasing");
	     equalVectorInteger(expect_dec, dec, str + " decreasing");
	     
	     str = "Test rank, test1";
	     x.clear();
	     x.addAll(Arrays.asList(new Double[]{1.0, 3.0, 2.0}));
	     Vector<Double> expectRank = new Vector<Double>();
	     expectRank.addAll(Arrays.asList(new Double[]{1.0, 3.0, 2.0}));
	     
	     // Order
	     Vector<Double> ranks = rank(x);
	     equalVectorDouble(expectRank, ranks, str);
	     
	     str = "Test rank, test2";
	     x.clear();
	     x.addAll(Arrays.asList(new Double[]{1.5, 3.2, 1.1, 2.2}));
	     expectRank.clear();
	     expectRank.addAll(Arrays.asList(new Double[]{2.0,4.0,1.0,3.0}));
	     
	     // Order
	     ranks = rank(x);
	     equalVectorDouble(expectRank, ranks, str);
	     
	     str = "Test rank, test3";
	     x.clear();
	     x.addAll(Arrays.asList(new Double[]{4.0, 8.0, 5.0, 9.0, 9.0, 1.0, 6.0, 9.0, 6.0, 5.0, 10.0,
	    		 5.0, 7.0, 6.0, 2.0, 9.0, 3.0, 1.0, 4.0, 10.0, 9.0, 7.0, 7.0, 10.0, 7.0, 7.0, 6.0, 
	    		 6.0, 4.0, 2.0, 10.0, 9.0, 7.0, 8.0, 1.0, 5.0, 8.0, 3.0, 4.0, 3.0, 2.0, 5.0, 5.0, 
	    		 4.0, 2.0, 2.0, 3.0, 5.0, 3.0, 9.0 }));
	     expectRank.clear();
	     expectRank.addAll(Arrays.asList(new Double[]{16.0, 38.0, 22.0, 43.0, 43.0, 2.0, 28.0, 43.0,
	    		 28.0, 22.0, 48.5, 22.0, 33.5, 28.0, 6.0, 43.0, 11.0, 2.0, 16.0, 48.5, 43.0, 33.5,
	    		 33.5, 48.5, 33.5, 33.5, 28.0, 28.0, 16.0, 6.0, 48.5, 43.0, 33.5, 38.0, 2.0, 22.0,
	    		 38.0, 11.0, 16.0, 11.0, 6.0, 22.0, 22.0, 16.0, 6.0, 6.0, 11.0, 22.0, 11.0, 43.0 }));
	     
	     // Order
	     ranks = rank(x);
	     equalVectorDouble(expectRank, ranks, str);
	     
	     str = "Test logrankScores, test1";
	     Vector<Double> time = new Vector<Double>();
	     time.addAll(Arrays.asList(new Double[]{72.0, 411.0, 228.0, 126.0, 118.0, 10.0, 82.0, 110.0, 
	    		  314.0, 100.0, 42.0, 8.0, 144.0, 25.0, 11.0, 30.0, 384.0, 4.0, 54.0, 13.0, 123.0,
	    		  97.0, 153.0, 59.0, 117.0, 16.0, 151.0, 22.0, 56.0, 21.0, 18.0, 139.0, 20.0, 31.0,
	    		  52.0, 287.0, 18.0, 51.0, 122.0, 27.0, 54.0, 7.0, 63.0, 392.0, 10.0, 8.0, 92.0, 35.0,
	    		  117.0, 132.0, 12.0, 162.0, 3.0, 95.0, 177.0, 162.0, 216.0, 553.0, 278.0, 12.0,
	    		  260.0, 200.0, 156.0, 182.0, 143.0, 105.0, 103.0, 250.0, 100.0, 999.0, 112.0, 87.0,
	    		  231.0, 242.0, 991.0, 111.0, 1.0, 587.0, 389.0, 33.0, 25.0, 357.0, 467.0, 201.0, 1.0,
	    		  30.0, 44.0, 283.0, 15.0, 25.0, 103.0, 21.0, 13.0, 87.0, 2.0, 20.0, 7.0, 24.0, 99.0,
	    		  8.0, 99.0, 61.0, 25.0, 95.0, 80.0, 51.0, 29.0, 24.0, 18.0, 83.0, 31.0, 51.0, 90.0,
	    		  52.0, 73.0, 8.0, 36.0, 48.0, 7.0, 140.0, 186.0, 84.0, 19.0, 45.0, 80.0, 52.0, 164.0,
	    		  19.0, 53.0, 15.0, 43.0, 340.0, 133.0, 111.0, 231.0, 378.0, 49.0}));
	     Vector<Double> status = new Vector<Double>();
	     status.addAll(Arrays.asList(new Double[]{1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0,
	    		  1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0}));
	     Vector<Double> expectlr = new Vector<Double>();
	     expectlr.addAll(Arrays.asList(new Double[]{0.338662090461661, -2.02010051669164, -0.705694192881291,
	    	      -0.108471626927477, -0.0608255690715493, 0.891953239598987, 0.280039238335736, 
	    	      0.0692894105115154, -1.20022317181429, -0.87187197712046, 0.578363772221272,
	    	      0.908082271857051, -0.26880467541842, -0.315498831477494, 0.883823158298174,
	    	      0.64336274345551, -1.58557670716783, 0.962905226680006, 0.407180963376637, 0.850487509989263,
	    	      -1.08408138302504, -0.817639172887656, -0.328519470427333, 0.38033411652979,
	    	      -0.0380982963442766, 0.824772803240074, -0.298216440124302, 0.733731937753275,
	    	      0.393847630043303, 0.743165900017426, 0.798224130673702, -0.185428442042187,
	    	      0.761857488802473, 0.622086147710829, 0.446317265670834, -1.12330009489122,
	    	      0.798224130673702, 0.484291949215137, -0.0840813830250378, 0.674400158421496,
	    	      0.407180963376637, 0.939828303603083, 0.352746597503915, -1.85343385002497,
	    	      0.891953239598987, 0.908082271857051, 0.216259132197089, 0.600463894321395,
	    	      -0.0380982963442766, -0.133471626927477, 0.867294232678339, -0.426436137093999,
	    	      0.970424023672487, 0.182360827112344, -0.496633181428975, -0.426436137093999,
	    	      -0.660239647426745, -2.47010051669164, -0.985204856795979, 0.867294232678339,
	    	      -0.922704856795979, -0.575094719890513, -0.359769470427333, -1.49663318142897,
	    	      -0.240233246846991, 0.0892894105115153, 0.10889725364877, -0.863881327384215, 
	    	      0.12812802287954, -4.30343385002497, 0.00634614810016787, -0.751218392921895,
	    	      -1.75569419288129, -0.808325771828659, -3.30343385002497, 0.0276227438448486,
	    	      0.985294117647059, -2.80343385002497, -1.71057670716783, 0.611333459538786,
	    	      0.684501168522506, -1.37446559605672, -2.22010051669164, -0.61676138655718,
	    	      0.985294117647059, 0.64336274345551, 0.555764180801456, -1.05187152346265,
	    	      0.833393492895246, 0.684501168522506, -0.89110274635123, 0.743165900017426,
	    	      0.850487509989263, 0.248781607078105, 0.977886710239651, 0.761857488802473,
	    	      0.939828303603083, 0.714501168522506, 0.146646541398058, 0.908082271857051,
	    	      0.146646541398058, 0.366635486392803, 0.684501168522506, 0.182360827112344,
	    	      0.294964611470064, 0.484291949215137, 0.664196076788843, 0.714501168522506,
	    	      0.798224130673702, -0.719960761664264, 0.622086147710829, 0.484291949215137,
	    	      0.23265257482004, 0.446317265670834, 0.324376376175947, 0.908082271857051,
	    	      0.589474883332384, 0.532642020951149, 0.939828303603083, -0.212455469069214,
	    	      -0.535094719890513, 0.264654622951121, 0.780206112655684, 0.544269927927893,
	    	      0.294964611470064, 0.446317265670834, -0.460918895714689, 0.780206112655684,
	    	      0.433496752850321, 0.833393492895246, 0.567127817165093, -1.28355650514763,
	    	      -0.159112652568502, 0.0276227438448486, -0.75569419288129, -1.47446559605672,
	    	      0.520877315068796}));
	     
	     // Order
	     Vector<Double> scores = logrankScores(time, status);
	     // Compare with expectation
	     success = true;
	     for (i = 0; i < time.size(); i++) {
	    	 if (scores.get(i) < expectlr.get(i) - Math.abs(expectlr.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " scores.get("+i+") is too low");
	        	 success = false;
	         }
	         else if (scores.get(i) > expectlr.get(i) + Math.abs(expectlr.get(i) * 0.05)) {
	        	 System.out.println("In " + str + " scores.get("+i+") is too high");
	        	 success = false;
	         }	 
	     }
	     if (success) {
	    	 System.out.println(str + " passed");
	     }
	     
	     str = "Test maxstat, trt";
	     time.clear();
	     time.addAll(Arrays.asList(new Double[]{72.0, 411.0, 228.0, 126.0, 118.0, 10.0, 82.0, 110.0,
	    		  314.0, 100.0, 42.0, 8.0, 144.0, 25.0, 11.0, 30.0, 384.0, 4.0, 54.0, 13.0, 123.0,
	    		  97.0, 153.0, 59.0, 117.0, 16.0, 151.0, 22.0, 56.0, 21.0, 18.0, 139.0, 20.0, 31.0, 
	    		  52.0, 287.0, 18.0, 51.0, 122.0, 27.0, 54.0, 7.0, 63.0, 392.0, 10.0, 8.0, 92.0, 35.0, 
	    		  117.0, 132.0, 12.0, 162.0, 3.0, 95.0, 177.0, 162.0, 216.0, 553.0, 278.0, 12.0, 260.0,
	    		  200.0, 156.0, 182.0, 143.0, 105.0, 103.0, 250.0, 100.0, 999.0, 112.0, 87.0, 231.0, 
	    		  242.0, 991.0, 111.0, 1.0, 587.0, 389.0, 33.0, 25.0, 357.0, 467.0, 201.0, 1.0, 30.0,
	    		  44.0, 283.0, 15.0, 25.0, 103.0, 21.0, 13.0, 87.0, 2.0, 20.0, 7.0, 24.0, 99.0, 8.0, 
	    		  99.0, 61.0, 25.0, 95.0, 80.0, 51.0, 29.0, 24.0, 18.0, 83.0, 31.0, 51.0, 90.0, 52.0,
	    		  73.0, 8.0, 36.0, 48.0, 7.0, 140.0, 186.0, 84.0, 19.0, 45.0, 80.0, 52.0, 164.0, 19.0, 
	    		  53.0, 15.0, 43.0, 340.0, 133.0, 111.0, 231.0, 378.0, 49.0 }));
	     status.clear();
	     status.addAll(Arrays.asList(new Double[]{1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 
	    		  1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 }));
	     x.clear();
	     x.addAll(Arrays.asList(new Double[]{1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
	    		  1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
	    		  2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
	    		  2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
	    		  2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0 }));
	     
	     double expect_maxstat = 0.095071135385996;
	     double expect_split = 1.5;

	     // Order
	     Vector<Integer> indices = order(x, false);

	     // Scores
	     scores.clear();
	     scores = logrankScores(time, status);

	     double best_maxstat[] = new double[1];
	     double best_split_value[] = new double[1];
	     maxstat(scores, x, indices, best_maxstat, best_split_value, 0.1, 0.9);

	     // Compare with expectation
         success = true;
         if (best_maxstat[0] < expect_maxstat - Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too low");
        	 success = false;
         }
         else if (best_maxstat[0] > expect_maxstat + Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too high");
        	 success = false;	 
         }
         if (best_split_value[0] < expect_split - Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best_split_value[0] is too low");
        	 success = false;
         }
         else if (best_split_value[0] > expect_split + Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best split_value[0] is too high");
        	 success = false;
         }
         if (success) {
	    	 System.out.println(str + " passed");
	     }
         
         str = "Test maxstat, celltype";
         // time and status same as in last example
         x.clear();
         x.addAll(Arrays.asList(new Double[]{1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
        		  1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
        		  2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0,
        		  3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0,
        		  4.0, 4.0, 4.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
        		  1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
        		  2.0, 2.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
        		  3.0, 3.0, 3.0, 3.0, 3.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0 }));
         
         expect_maxstat = 3.44263138354529;
         expect_split = 1.5;

         // Order
         indices.clear();
         indices = order(x, false);

         // Scores
         scores.clear();
         scores = logrankScores(time, status);

         maxstat(scores, x, indices, best_maxstat, best_split_value, 0.1, 0.9);

         // Compare with expectation
         success = true;
         if (best_maxstat[0] < expect_maxstat - Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too low");
        	 success = false;
         }
         else if (best_maxstat[0] > expect_maxstat + Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too high");
        	 success = false;	 
         }
         if (best_split_value[0] < expect_split - Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best_split_value[0] is too low");
        	 success = false;
         }
         else if (best_split_value[0] > expect_split + Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best split_value[0] is too high");
        	 success = false;
         }
         if (success) {
	    	 System.out.println(str + " passed");
	     }
         
         str = "Test maxstat, karno";
         // time and status same as before
         x.clear();
         x.addAll(Arrays.asList(new Double[]{60.0, 70.0, 60.0, 60.0, 70.0, 20.0, 40.0, 80.0, 50.0, 70.0,
        		  60.0, 40.0, 30.0, 80.0, 70.0, 60.0, 60.0, 40.0, 80.0, 60.0, 40.0, 60.0, 60.0, 30.0,
        		  80.0, 30.0, 50.0, 60.0, 80.0, 40.0, 20.0, 80.0, 30.0, 75.0, 70.0, 60.0, 30.0, 60.0,
        		  80.0, 60.0, 70.0, 50.0, 50.0, 40.0, 40.0, 20.0, 70.0, 40.0, 80.0, 80.0, 50.0, 80.0,
        		  30.0, 80.0, 50.0, 80.0, 50.0, 70.0, 60.0, 40.0, 80.0, 80.0, 70.0, 90.0, 90.0, 80.0,
        		  80.0, 70.0, 60.0, 90.0, 80.0, 80.0, 50.0, 50.0, 70.0, 70.0, 20.0, 60.0, 90.0, 30.0,
        		  20.0, 70.0, 90.0, 80.0, 50.0, 70.0, 60.0, 90.0, 50.0, 30.0, 70.0, 20.0, 30.0, 60.0,
        		  40.0, 30.0, 20.0, 60.0, 70.0, 80.0, 85.0, 70.0, 70.0, 70.0, 50.0, 30.0, 40.0, 40.0,
        		  40.0, 99.0, 80.0, 60.0, 60.0, 60.0, 60.0, 50.0, 70.0, 10.0, 40.0, 70.0, 90.0, 80.0,
        		  50.0, 40.0, 40.0, 60.0, 70.0, 30.0, 60.0, 30.0, 60.0, 80.0, 75.0, 60.0, 70.0, 80.0, 30.0 }));
         
         expect_maxstat = 4.61806159115936;
         expect_split = 45;
         
         // Order
         indices.clear();
         indices = order(x, false);

         // Scores
         scores.clear();
         scores = logrankScores(time, status);

         maxstat(scores, x, indices, best_maxstat, best_split_value, 0.1, 0.9);

         // Compare with expectation
         success = true;
         if (best_maxstat[0] < expect_maxstat - Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too low");
        	 success = false;
         }
         else if (best_maxstat[0] > expect_maxstat + Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too high");
        	 success = false;	 
         }
         if (best_split_value[0] < expect_split - Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best_split_value[0] is too low");
        	 success = false;
         }
         else if (best_split_value[0] > expect_split + Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best split_value[0] is too high");
        	 success = false;
         }
         if (success) {
	    	 System.out.println(str + " passed");
	     }
         
         
         str = "Test maxstat, diagtime";
         // time and status same as before
         x.clear();
         x.addAll(Arrays.asList(new Double[]{7.0, 5.0, 3.0, 9.0, 11.0, 5.0, 10.0, 29.0, 18.0, 6.0, 4.0,
        		  58.0, 4.0, 9.0, 11.0, 3.0, 9.0, 2.0, 4.0, 4.0, 3.0, 5.0, 14.0, 2.0, 3.0, 4.0, 12.0, 
        		  4.0, 12.0, 2.0, 15.0, 2.0, 5.0, 3.0, 2.0, 25.0, 4.0, 1.0, 28.0, 8.0, 1.0, 7.0, 11.0,
        		  4.0, 23.0, 19.0, 10.0, 6.0, 2.0, 5.0, 4.0, 5.0, 3.0, 4.0, 16.0, 5.0, 15.0, 2.0, 12.0,
        		  12.0, 5.0, 12.0, 2.0, 2.0, 8.0, 11.0, 5.0, 8.0, 13.0, 12.0, 6.0, 3.0, 8.0, 1.0, 7.0,
        		  3.0, 21.0, 3.0, 2.0, 6.0, 36.0, 13.0, 2.0, 28.0, 7.0, 11.0, 13.0, 2.0, 13.0, 2.0, 22.0, 
        		  4.0, 2.0, 2.0, 36.0, 9.0, 11.0, 8.0, 3.0, 2.0, 4.0, 2.0, 2.0, 1.0, 17.0, 87.0, 8.0, 2.0,
        		  5.0, 3.0, 3.0, 5.0, 22.0, 3.0, 3.0, 5.0, 8.0, 4.0, 4.0, 3.0, 3.0, 4.0, 10.0, 3.0, 4.0,
        	      4.0, 15.0, 4.0, 12.0, 5.0, 11.0, 10.0, 1.0, 5.0, 18.0, 4.0, 3.0 }));
         
         expect_maxstat = 0.800489478294775;
         expect_split = 3.5;
         
         // Order
         indices.clear();
         indices = order(x, false);

         // Scores
         scores.clear();
         scores = logrankScores(time, status);

         maxstat(scores, x, indices, best_maxstat, best_split_value, 0.1, 0.9);

         // Compare with expectation
         success = true;
         if (best_maxstat[0] < expect_maxstat - Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too low");
        	 success = false;
         }
         else if (best_maxstat[0] > expect_maxstat + Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too high");
        	 success = false;	 
         }
         if (best_split_value[0] < expect_split - Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best_split_value[0] is too low");
        	 success = false;
         }
         else if (best_split_value[0] > expect_split + Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best split_value[0] is too high");
        	 success = false;
         }
         if (success) {
	    	 System.out.println(str + " passed");
	     }

         str = "Test maxstat, age";
         // time and status same as before
         x.clear();
         x.addAll(Arrays.asList(new Double[]{69.0, 64.0, 38.0, 63.0, 65.0, 49.0, 69.0, 68.0, 43.0,
        		  70.0, 81.0, 63.0, 63.0, 52.0, 48.0, 61.0, 42.0, 35.0, 63.0, 56.0, 55.0, 67.0, 63.0,
        		  65.0, 46.0, 53.0, 69.0, 68.0, 43.0, 55.0, 42.0, 64.0, 65.0, 65.0, 55.0, 66.0, 60.0,
        		  67.0, 53.0, 62.0, 67.0, 72.0, 48.0, 68.0, 67.0, 61.0, 60.0, 62.0, 38.0, 50.0, 63.0,
        		  64.0, 43.0, 34.0, 66.0, 62.0, 52.0, 47.0, 63.0, 68.0, 45.0, 41.0, 66.0, 62.0, 60.0,
        		  66.0, 38.0, 53.0, 37.0, 54.0, 60.0, 48.0, 52.0, 70.0, 50.0, 62.0, 65.0, 58.0, 62.0,
        		  64.0, 63.0, 58.0, 64.0, 52.0, 35.0, 63.0, 70.0, 51.0, 40.0, 69.0, 36.0, 71.0, 62.0,
        		  60.0, 44.0, 54.0, 66.0, 49.0, 72.0, 68.0, 62.0, 71.0, 70.0, 61.0, 71.0, 59.0, 67.0,
        		  60.0, 69.0, 57.0, 39.0, 62.0, 50.0, 43.0, 70.0, 66.0, 61.0, 81.0, 58.0, 63.0, 60.0, 
        		  62.0, 42.0, 69.0, 63.0, 45.0, 68.0, 39.0, 66.0, 63.0, 49.0, 64.0, 65.0, 64.0, 67.0,
        		  65.0, 37.0 }));
         
         expect_maxstat = 1.7992993341166;
         expect_split = 58.5;
         
         // Order
         indices.clear();
         indices = order(x, false);

         // Scores
         scores.clear();
         scores = logrankScores(time, status);

         maxstat(scores, x, indices, best_maxstat, best_split_value, 0.1, 0.9);

         // Compare with expectation
         success = true;
         if (best_maxstat[0] < expect_maxstat - Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too low");
        	 success = false;
         }
         else if (best_maxstat[0] > expect_maxstat + Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too high");
        	 success = false;	 
         }
         if (best_split_value[0] < expect_split - Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best_split_value[0] is too low");
        	 success = false;
         }
         else if (best_split_value[0] > expect_split + Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best split_value[0] is too high");
        	 success = false;
         }
         if (success) {
	    	 System.out.println(str + " passed");
	     }

		 str = "Test maxstat, prior";
		 // time and status same as before
		 x.clear();
		 x.addAll(Arrays.asList(new Double[]{0.0, 10.0, 0.0, 10.0, 10.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0,
				  10.0, 0.0, 10.0, 10.0, 0.0, 0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 10.0, 0.0, 0.0, 10.0, 0.0,
				  0.0, 10.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
				  0.0, 10.0, 10.0, 0.0, 0.0, 0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0,
				  10.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.0, 10.0, 10.0, 0.0, 0.0, 10.0, 0.0, 
				  10.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.0, 0.0, 0.0, 10.0, 0.0, 10.0, 0.0,
			      10.0, 0.0, 0.0, 0.0, 10.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.0,
			      0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.0, 
			      0.0, 0.0, 0.0, 0.0, 10.0, 10.0, 0.0, 0.0, 10.0, 10.0, 0.0, 0.0, 10.0, 0.0, 0.0 }));
		 
		 expect_maxstat = 0.7158562778385;
		 expect_split = 5;

		 // Order
         indices.clear();
         indices = order(x, false);

         // Scores
         scores.clear();
         scores = logrankScores(time, status);

         maxstat(scores, x, indices, best_maxstat, best_split_value, 0.1, 0.9);

         // Compare with expectation
         success = true;
         if (best_maxstat[0] < expect_maxstat - Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too low");
        	 success = false;
         }
         else if (best_maxstat[0] > expect_maxstat + Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too high");
        	 success = false;	 
         }
         if (best_split_value[0] < expect_split - Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best_split_value[0] is too low");
        	 success = false;
         }
         else if (best_split_value[0] > expect_split + Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best split_value[0] is too high");
        	 success = false;
         }
         if (success) {
	    	 System.out.println(str + " passed");
	     }
         
         str = "Test maxstat, regression";
         Vector<Double>y = new Vector<Double>();
         y.addAll(Arrays.asList(new Double[]{5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.9, 5.4, 4.8, 4.8,
        		    4.3, 5.8, 5.7, 5.4, 5.1, 5.7, 5.1, 5.4, 5.1, 4.6, 5.1, 4.8, 5.0,
        		    5.0, 5.2, 5.2, 4.7, 4.8, 5.4, 5.2, 5.5, 4.9, 5.0, 5.5, 4.9, 4.4,
        		    5.1, 5.0, 4.5, 4.4, 5.0, 5.1, 4.8, 5.1, 4.6, 5.3, 5.0, 7.0, 6.4, 6.9,
        		    5.5, 6.5, 5.7, 6.3, 4.9, 6.6, 5.2, 5.0, 5.9, 6.0, 6.1, 5.6, 6.7,
        		    5.6, 5.8, 6.2, 5.6, 5.9, 6.1, 6.3, 6.1, 6.4, 6.6, 6.8, 6.7, 6.0,
        		    5.7, 5.5, 5.5, 5.8, 6.0, 5.4, 6.0, 6.7, 6.3, 5.6, 5.5, 5.5, 6.1,
        		    5.8, 5.0, 5.6, 5.7, 5.7, 6.2, 5.1, 5.7, 6.3, 5.8, 7.1, 6.3, 6.5,
        		    7.6, 4.9, 7.3, 6.7, 7.2, 6.5, 6.4, 6.8, 5.7, 5.8, 6.4, 6.5, 7.7,
        		    7.7, 6.0, 6.9, 5.6, 7.7, 6.3, 6.7, 7.2, 6.2, 6.1, 6.4, 7.2, 7.4,
        		    7.9, 6.4, 6.3, 6.1, 7.7, 6.3, 6.4, 6.0, 6.9, 6.7, 6.9, 5.8, 6.8,
        		    6.7, 6.7, 6.3, 6.5, 6.2, 5.9 }));
         x.clear();
         x.addAll(Arrays.asList(new Double[]{3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1, 3.7, 3.4, 3.0,
        		    3.0, 4.0, 4.4, 3.9, 3.5, 3.8, 3.8, 3.4, 3.7, 3.6, 3.3, 3.4, 3.0, 3.4,
        		    3.5, 3.4, 3.2, 3.1, 3.4, 4.1, 4.2, 3.1, 3.2, 3.5, 3.6, 3.0, 3.4,
        		    3.5, 2.3, 3.2, 3.5, 3.8, 3.0, 3.8, 3.2, 3.7, 3.3, 3.2, 3.2, 3.1,
        		    2.3, 2.8, 2.8, 3.3, 2.4, 2.9, 2.7, 2.0, 3.0, 2.2, 2.9, 2.9, 3.1,
        		    3.0, 2.7, 2.2, 2.5, 3.2, 2.8, 2.5, 2.8, 2.9, 3.0, 2.8, 3.0, 2.9, 2.6,
        		    2.4, 2.4, 2.7, 2.7, 3.0, 3.4, 3.1, 2.3, 3.0, 2.5, 2.6, 3.0, 2.6, 2.3,
        		    2.7, 3.0, 2.9, 2.9, 2.5, 2.8, 3.3, 2.7, 3.0, 2.9, 3.0, 3.0, 2.5, 2.9,
        		    2.5, 3.6, 3.2, 2.7, 3.0, 2.5, 2.8, 3.2, 3.0, 3.8, 2.6, 2.2, 3.2,
        		    2.8, 2.8, 2.7, 3.3, 3.2, 2.8, 3.0, 2.8, 3.0, 2.8, 3.8, 2.8, 2.8,
        		    2.6, 3.0, 3.4, 3.1, 3.0, 3.1, 3.1, 3.1, 2.7, 3.2, 3.3, 3.0, 2.5, 3.0,
        		    3.4, 3.0 }));
         
         expect_maxstat = 3.73535181856697;
         expect_split = 3.35;

         // Order x
         indices.clear();
         indices = order(x, false);

         // Rank scores
         scores.clear();
         scores = rank(y);

         maxstat(scores, x, indices, best_maxstat, best_split_value, 0.1, 0.9);

         // Compare with expectation
         success = true;
         if (best_maxstat[0] < expect_maxstat - Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too low");
        	 success = false;
         }
         else if (best_maxstat[0] > expect_maxstat + Math.abs(expect_maxstat * 0.05)) {
        	 System.out.println("In " + str + " best_maxstat[0] is too high");
        	 success = false;	 
         }
         if (best_split_value[0] < expect_split - Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best_split_value[0] is too low");
        	 success = false;
         }
         else if (best_split_value[0] > expect_split + Math.abs(expect_split * 0.05)) {
        	 System.out.println("In " + str + " best split_value[0] is too high");
        	 success = false;
         }
         if (success) {
	    	 System.out.println(str + " passed");
	     }
         
         str = "Test numSamplesLeftOfCutpoint, test1";
         x.clear();
         x.addAll(Arrays.asList(new Double[]{69.0, 64.0, 38.0, 63.0, 65.0, 49.0, 69.0, 68.0, 43.0, 70.0,
        		  81.0, 63.0, 63.0, 52.0, 48.0, 61.0, 42.0, 35.0, 63.0, 56.0, 55.0, 67.0, 63.0, 65.0, 
        		  46.0, 53.0, 69.0, 68.0, 43.0, 55.0, 42.0, 64.0, 65.0, 65.0, 55.0, 66.0, 60.0, 67.0,
        		  53.0, 62.0, 67.0, 72.0, 48.0, 68.0, 67.0, 61.0, 60.0, 62.0, 38.0, 50.0, 63.0, 64.0,
        		  43.0, 34.0, 66.0, 62.0, 52.0, 47.0, 63.0, 68.0, 45.0, 41.0, 66.0, 62.0, 60.0, 66.0,
        		  38.0, 53.0, 37.0, 54.0, 60.0, 48.0, 52.0, 70.0, 50.0, 62.0, 65.0, 58.0, 62.0, 64.0,
        		  63.0, 58.0, 64.0, 52.0, 35.0, 63.0, 70.0, 51.0, 40.0, 69.0, 36.0, 71.0, 62.0, 60.0,
        		  44.0, 54.0, 66.0, 49.0, 72.0, 68.0, 62.0, 71.0, 70.0, 61.0, 71.0, 59.0, 67.0, 60.0, 
        		  69.0, 57.0, 39.0, 62.0, 50.0, 43.0, 70.0, 66.0, 61.0, 81.0, 58.0, 63.0, 60.0, 62.0,
        		  42.0, 69.0, 63.0, 45.0, 68.0, 39.0, 66.0, 63.0, 49.0, 64.0, 65.0, 64.0, 67.0, 65.0, 37.0 }));
         Vector<Integer>expectnum = new Vector<Integer>();
         expectnum.addAll(Arrays.asList(new Integer[]{1, 3, 4, 6, 9, 11, 12, 13, 16, 20, 21, 23, 24, 25,
        		 28, 31, 34, 35, 39, 42, 44, 47, 48, 49, 52, 53, 60, 64, 74, 86, 93, 100, 107, 113, 119,
        		 125, 130, 133, 135, 137 }));
         
         // Order
         indices.clear();
         indices = order(x, false);

         m.clear();
         m = numSamplesLeftOfCutpoint(x, indices);
         
         // Compare with expectation
         equalVectorInteger(expectnum, m, str);

		 return;
	   } // if (testUtility)
	   
	   Forest forest = null;
	   try {

	     // Create forest object
	     switch (treetype) {
	     case TREE_CLASSIFICATION:
	       if (probability) {
	         forest = new ForestProbability();
	       } else {
	         forest = new ForestClassification();
	       }
	       break;
	     case TREE_REGRESSION:
	       forest = new ForestRegression();
	       break;
	     case TREE_SURVIVAL:
	       forest = new ForestSurvival();
	       break;
	     case TREE_PROBABILITY:
	       forest = new ForestProbability();
	       break;
	     }

	     if (verbose_out) {
	    	 Preferences.debug("Starting Stochastic Forests\n", Preferences.DEBUG_ALGORITHM);
	     }

	     forest.initCpp(dependent_variable_name, memory_mode, input_file, mtry,
	         output_prefix, num_trees, verbose_out, seed, num_threads,
	         load_forest_filename, importance_mode, min_node_size, split_select_weights_file,
	         always_split_variable_names, status_variable_name, sample_with_replacement, unordered_variable_names,
	         memory_saving_splitting, splitrule, case_weights_file, predict_all, sample_fraction,
	         alpha, minprop, holdout, prediction_type, num_random_splits);

	     forest.run(true);
	     if (write) {
	       forest.saveToFile();
	     }
	     forest.writeOutput();
	     if (verbose_out) {
	    	 Preferences.debug("Finished Stochastic Forests\n", Preferences.DEBUG_ALGORITHM);
	     }

	     forest.dispose();
	     forest = null;
	   } catch (Exception e) {
	     System.err.println("Error: " + e.getMessage() + " Stochastic Forests will EXIT now.\n");
	     forest.dispose();
	     setCompleted(false);
	     return;
	   }

	   setCompleted(true);

	} // runAlgorithm
}