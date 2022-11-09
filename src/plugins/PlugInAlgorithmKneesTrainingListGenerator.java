import gov.nih.mipav.model.algorithms.*;
import java.io.*;
import java.util.*;

/*
 * The class converts the 2D-volumetric approach axial, sagittal and coronal MRI and CED png slices (or MRI png slices alone)
 * into a file list.  HED uses the list to train HED deep learning model. 
 * 
 * Training case only.   
 * 
 * @author Ruida Cheng
 */
public class PlugInAlgorithmKneesTrainingListGenerator extends AlgorithmBase {

    
    /** image repository input directory. */
    private String inputDir;
		
	// hash table to hold the image directory full path
	private Hashtable<String, Hashtable<String, Vector<String>>> keyImageVector = new Hashtable<String, Hashtable<String, Vector<String>>>();
	
	// hash table to hold the voi directory full path
	private Hashtable<String, Hashtable<String, Vector<String>>> keyImageVOIVector = new Hashtable<String, Hashtable<String, Vector<String>>>();
	
	
    /**
     * Algorithm for brain subcortical registration
     * @param _inputDir    image repository input directory
     * @param _outputDir    saved images ( registered, comparison, and report) directory. 
     * @param _parentDialog    reference to parent dialog, for file info saving. 
     */
    public PlugInAlgorithmKneesTrainingListGenerator(String _inputDir, PlugInDialogKneesTrainingListGenerator _parentDialog) {
    	inputDir = _inputDir;
    }
    
    /**
     * Processing comamnd line, construction image instances, then do registration and comparison. 
     */
	public void runAlgorithm() {
		
		long startTime = System.currentTimeMillis();
		
		File fileDir = new File(inputDir);
		traverse_train_folds(fileDir);
		
		sortAll();
		
		writePngPairAll();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");
	
		System.gc();
		System.out.println("PlugIn Knees femur segmentation training list generation finish running.  MIPAV Quits. ");
		System.exit(0);
	}
    
	/**
	 * Write the axial.lst, sagittal.lst and coronal.lst for each fold and each orientation.   Those lists will be
	 * used to train the HNN models.   
	 */
	private void writePngPairAll() {
		
		Set<String> key_foldNumber = keyImageVector.keySet();
		String fullpath;
		Vector<String> image_path_vector;
		Vector<String> voi_path_vector;
		Hashtable<String, Vector<String>> image_foldTable;
		Set<String> key_orientation;
		Hashtable<String, Vector<String>> voi_foldTable;
		int index;
		String upper_oneLevel;
		String upper_twoLevel;
		int i, len;
		String imageName, voiName;
		
		for (String foldKey : key_foldNumber) {
			
			image_foldTable = keyImageVector.get(foldKey);
			key_orientation = image_foldTable.keySet();
			
			voi_foldTable = keyImageVOIVector.get(foldKey);
			
			for (String orientation_key : key_orientation ) {
				
				image_path_vector = image_foldTable.get(orientation_key);
				
				if ( image_path_vector.size() >= 1 ) {
					
					image_path_vector = image_foldTable.get(orientation_key);
					voi_path_vector = voi_foldTable.get(orientation_key);
					
					fullpath = image_path_vector.get(0);
					index = fullpath.lastIndexOf(File.separator);
					upper_oneLevel = fullpath.substring(0, index);
					index = upper_oneLevel.lastIndexOf(File.separator);
					upper_twoLevel = upper_oneLevel.substring(0, index);
					
					System.err.println("upper_twoLevel = " + upper_twoLevel);
			
					try {
						PrintWriter fileWriter = new PrintWriter(
								new FileWriter(upper_twoLevel + File.separator + orientation_key + ".lst")); 
						len = image_path_vector.size();
	
						for (i = 0; i < len; i++) {
							imageName = image_path_vector.get(i);
							voiName = voi_path_vector.get(i);
							fileWriter.write(imageName + " " + voiName + "\n");
						}
						fileWriter.close();
						System.err.println("text write finish");
					} catch (IOException e) {
						e.printStackTrace();
					}
				 
				}
				
			}
			
		}
		
	
		
		
	}
	
	/**
	 * Sort the full path tables for images and vois.  
	 * Sort the image and voi pairs in sorted order. 
	 */
	public void sortAll() {
		
		Set<String> key_foldNumber = keyImageVector.keySet();
		for (String foldKey : key_foldNumber) {
			
			Hashtable<String, Vector<String>> image_foldTable = keyImageVector.get(foldKey);
			Set<String> key_orientation = image_foldTable.keySet();
			
			Hashtable<String, Vector<String>> voi_foldTable = keyImageVOIVector.get(foldKey);
			
			for (String orientation_key : key_orientation ) {
				System.err.println("orientation_key = " + orientation_key);
				Vector<String> image_path_vector = image_foldTable.get(orientation_key);
				Vector<String> voi_path_vector = voi_foldTable.get(orientation_key);
				
				int j;
				int len = image_path_vector.size();
				System.err.println("len = " + len);
				String imageName;
				String voiName;
				int start, end;
				Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
				Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
				int index;
				int max_index = 0;

				for ( j = 0; j < len; j++ ) {
					imageName = image_path_vector.get(j);
					start = imageName.lastIndexOf(File.separator) + 1;
					end = imageName.lastIndexOf(".");
					index = Integer.valueOf(imageName.substring(start+6, end));
					if ( index > max_index ) {
						max_index = index;
					}
				}
				
				
				for (j = 0; j < len; j++) {
					imageName = image_path_vector.get(j);
					if ( imageName != null ) {
						start = imageName.lastIndexOf(File.separator) + 1;
						end = imageName.lastIndexOf(".");
						index = Integer.valueOf(imageName.substring(start+6, end));
						imageNameTable.put(index, imageName);
					}
				}
				
				len = max_index;
				image_path_vector.clear();
				for (j = 0; j < len; j++) {
					imageName = imageNameTable.get(j);
					if (imageName != null) {
						System.err.println("imageName = " + imageName);
						image_path_vector.add(imageName);
					}
				}

				len = voi_path_vector.size();
				for (j = 0; j < len; j++) {
					voiName = voi_path_vector.get(j);
					start = voiName.lastIndexOf(File.separator) + 1;
					end = voiName.lastIndexOf(".");
					index = Integer.valueOf(voiName.substring(start+4, end));
					imageVOITable.put(index, voiName);
				}

				len = max_index;
				voi_path_vector.clear();
				for (j = 0; j < len; j++) {
					voiName = imageVOITable.get(j);
					if (voiName != null) {
						System.err.println("voiName = " + voiName);
						voi_path_vector.add(voiName);
					}
				}

				
			}
			
			
		}
		
	}
	
	/**
	 * Traverse the specified 10-fold training directory
	 * @param dir
	 */
	private void traverse_train_folds(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder(new File(dir, children[i]), children[i]);
			}
		}

	}
	
	/**
	 * Traverse the each fold directory
	 * @param dir  directory 
	 * @param foldNumber   fold hash ID, i.e. fold1, fold2, fold3, ... etc. 
	 */
	private void processDir_folder(File dir, String foldNumber) {
		
		if ( dir.isDirectory() ) {
			
			String[] children = dir.list();
			
			keyImageVector.put(foldNumber, new Hashtable<String, Vector<String>>());
			keyImageVOIVector.put(foldNumber, new Hashtable<String, Vector<String>>());
			
			for ( int i = 0; i < children.length; i++ ) {
				processDir_orientation(new File(dir, children[i]), children[i], foldNumber);	
			}
		}
	
	}
	
	/**
	 * Under each fold, traverse the orientation directories, i.e. axial, coronal, sagittal directories.
	 * @param dir   direcotry
	 * @param orientation  orientation keyword or hashID
	 * @param foldNumber   fold keyword or hashID
	 */
	private void processDir_orientation(File dir, String orientation, String foldNumber) {
		
		if ( dir.isDirectory() ) {
			String[] children = dir.list();
			
			keyImageVector.get(foldNumber).put(orientation, new Vector<String>());
			keyImageVOIVector.get(foldNumber).put(orientation, new Vector<String>());
			
			for ( int i = 0; i < children.length; i++ ) {	
				process_file(new File(dir, children[i]), orientation, foldNumber);
			}
			
		}
		
	}
	
	/**
	 * Read the base directory for images and vois png files. Recording the full path. 
	 * @param dir  directory
	 * @param orientation   orientatino hash ID
	 * @param foldNumber    fold hash ID
	 */
	private void process_file(File dir, String orientation, String foldNumber) {
		
		String dirName = dir.toString();
		
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".png")) {
			// System.err.println("foldNumber = " + foldNumber  + " orientation = " + orientation + " path = " + dirName.toString());
			keyImageVector.get(foldNumber).get(orientation).add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".png")) {
			System.err.println("foldNumber = " + foldNumber  + " orientation = " + orientation + " path = " + dirName.toString());
			keyImageVOIVector.get(foldNumber).get(orientation).add(dir.toString());
		}
	}
	
	
      
      
	
}


