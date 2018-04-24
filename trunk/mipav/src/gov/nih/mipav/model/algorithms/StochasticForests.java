package gov.nih.mipav.model.algorithms;

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
	
	public static int RAND_MAX = 32767;
	
	public static double logprop;
	
	private boolean testCode = true;
	
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
	    
	    public void permuteSampleIDs() {
	        permuted_sampleIDs.clear();
	        for (i = 0; i < num_rows; i++) {
	        	permuted_sampleIDs.add(i);
	        }
	        shuffle(permuted_sampleIDs);
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
	
	private void shuffle(Vector<Integer> v)
	{
	    int index, temp;
	    Random random = new Random();
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
	
	private void drawWithoutReplacementSkip(Vector<Integer> result, int max,
	    Vector<Integer> skip, int num_samples) {
	  if (num_samples < max / 10) {
	    drawWithoutReplacementSimple(result, max, skip, num_samples);
	  } else {
	    //drawWithoutReplacementKnuth(result, max, skip, num_samples);
	    drawWithoutReplacementFisherYates(result, max, skip, num_samples);
	  }
	}

	
	private void drawWithoutReplacementSimple(Vector<Integer> result, int max,
		    Vector<Integer> skip, int num_samples) {
      int i, j;

	  // Set all to not selected
	  Vector<Boolean> temp = new Vector<Boolean>();
	  for (i = 0; i < max; i++) {
	      temp.add(i, false);
	  }

	  Random random = new Random();
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

	private void drawWithoutReplacementFisherYates(Vector<Integer> result,
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
	  Random random = new Random();
	  for (i = 0; i < num_samples; ++i) {
	    j = (int)Math.round(i + random.nextDouble() * (max - skip.size() - i));
	    temp = result.get(i);
	    result.set(i,result.get(j));
	    result.set(j,temp);
	  }

	  result.setSize(num_samples);
	}
	
	private void drawWithoutReplacementWeighted(Vector<Integer> result, Vector<Integer> indices,
			int num_samples, Vector<Double> weights) {
		int i, j;
		int draw = 0;
		double sum_of_weight = 0;
		double rand;
		
		// Set all to not selected
		Vector<Boolean> temp = new Vector<Boolean>();
		for (i = 0; i < indices.size(); i++) {
			temp.add(false);
		}
		
		Random random = new Random();
		
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
	
	private void drawWithoutReplacementWeighted(Vector<Integer> result, int max_index, 
			int num_samples, Vector<Double> weights) {
		int i, j;
		int draw = 0;
		double sum_of_weight = 0;
		double rand;
		
		// Set all to not selected
		Vector<Boolean> temp = new Vector<Boolean>();
		for (i = 0; i < max_index+1; i++) {
			temp.add(false);
		}
		
		Random random = new Random();
		
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
	
	private double mostFrequentValue(HashMap<Double, Integer> class_count) {
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
		    value = (int)values[i];	
		    key = (double)keys[i];
		    if (value > max_count) {
		    	max_count = value;
		    	major_classes.clear();
		    	major_classes.add(key);
		    }
		    else if (value == max_count) {
		    	major_classes.add(key);
		    }
		}
		
		if (major_classes.size() == 1) {
			return major_classes.get(0);
		}
		else {
			// Choose randomly
			Random random = new Random();
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
			int n_all, int n_first) {
          int i;
		  // Reserve space
		  first_part.setSize(n_all);

		  // Fill with 0..n_all-1 and shuffle
		  
		  for (i = 0; i < n_all; i++) {
			  first_part.set(i, i);
		  }
		  shuffle(first_part);

		  // Copy to second part
		  second_part.setSize(n_all - n_first);
		  for (i = n_first; i < first_part.size(); i++) {
			  second_part.set(i - n_first, first_part.get(i));
		  }

		  // Resize first part
		  first_part.setSize(n_first);
	}
	
	private void shuffleAndSplitAppend(Vector<Integer> first_part, Vector<Integer> second_part, 
			int n_all, int n_first, Vector<Integer> mapping) {
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
		  shuffle(fp2);
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
		    if (i < n - 1 && time.get(indices.get(i)) == time.get(indices.get(i + 1))) {
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
	    if (i < n - 1 && x.get(indices.get(i)) == x.get(indices.get(i + 1))) {
	      continue;
	    }

	    // If value is largest possible value, stop
	    if (x.get(indices.get(i)) == x.get(indices.get(n - 1))) {
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
	  num_samples_left.setSize(x.size());

	  for (int i = 0; i < x.size(); ++i) {
	    if (i == 0) {
	      num_samples_left.add(1);
	    } else if (x.get(indices.get(i)) == x.get(indices.get(i - 1))) {
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


	
	private boolean equalVectorInteger(Vector<Integer> v1, Vector<Integer> v2, String str) {
		// v1 is true answer, v2 is test answer
		int i;
	    if (v1.size() != v2.size()) {
	        System.out.println("In " + str + " correct size is " + v1.size());
	        System.out.println("Test size is " + v2.size());
	        return false;
	    }
	    for (i = 0; i < v1.size(); i++) {
	    	if (v1.get(i) != v2.get(i)) {
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
		    	if (p1.get(i) != p2.get(i)) {
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
	
	public StochasticForests() {
		
	}


	public void runAlgorithm() {
	   if (testCode) {
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
           expect3.addAll(Arrays.asList(new Integer[]{11,121,3,14,15}));
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
		   return;
	   }
	}
}