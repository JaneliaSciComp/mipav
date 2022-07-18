import gov.nih.mipav.model.file.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.*;


/**
 * This class generates the knees VOI contours from the deep learning (HNN) MRI energy maps.
 * 
 * @author Ruida Cheng
 * 
 */
public class PlugInAlgorithmSK10_MRI_map_nopre extends AlgorithmBase {

	/** image repository input directory. */
    private String inputDirImage = "/home/ruida/Knee_2010_challenge/train_img_sk10_femur_cart/axial";
    
    private String inputDirMap = "/home/ruida/Knee_2010_challenge/train_img_sk10_femur_cart/test_out/fold10";

    /** saved images and report output directory. */
    private String outputDir;
    
    /** Reference to the brain subcortical plugin dialog. */
    private PluginDialogSK10_MRI_map_nopre parentDialog;
   

	// image table to the HNN prediction probablity maps strings 
	Hashtable<String, Vector<String>> imageTable = new Hashtable<String, Vector<String>>();
	
	// mask table to hold the HNN prediction probablity map images. 
	Hashtable<String, Vector<ModelImage>> maskTable = new Hashtable<String, Vector<ModelImage>>();

	// mask image table to hold the HNN prediction probablity maps name
	Hashtable<String, Vector<String>> maskImageTable = new Hashtable<String, Vector<String>>();

	// image table to hold the testing images path strings
	Hashtable<String, String> nameTableImages = new Hashtable<String, String>();
	
	// image table to hold the testing images. 
	Hashtable<String, ModelImage> keyImagesOrientation = new Hashtable<String, ModelImage>();
	
	
	
    /**
     * Algorithm for brain subcortical registration
     * @param _inputDir    image repository input directory
     * @param _outputDir    saved images ( registered, comparison, and report) directory. 
     * @param _parentDialog    reference to parent dialog, for file info saving. 
     */
    public PlugInAlgorithmSK10_MRI_map_nopre(String _inputDirImage, String _inputDirMap, String _outputDir, 
    		PluginDialogSK10_MRI_map_nopre _parentDialog) {
    	inputDirImage = _inputDirImage;
    	inputDirMap = _inputDirMap;
    	outputDir = _outputDir;
    	parentDialog = _parentDialog;
    }
    
    /**
     * Processing comamnd line, construction image instances, then do registration and comparison. 
     */
	public void runAlgorithm() {
		long startTime = System.currentTimeMillis();
		
		readKeyImageDir();
		
		sortImageTable();
		
		readFile();
		
		generateContours_MRI();
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();
		
		
	}

	/**
	 * Generate the VOI contours from MRI prediction only.   As one ablation experiment
	 * in the MRM paper. 
	 */
	private void generateContours_MRI() {
		
		Set<String> keys = maskTable.keySet();
		
		
		for (String key : keys) {
		
			System.err.println("key = " + key);
			
				
				Vector<ModelImage> imageVec = maskTable.get(key);

				ModelImage srcImage = keyImagesOrientation.get(key);
				float[] srcResol = srcImage.getResolutions(0);
				
				int[] units = srcImage.getUnitsOfMeasure();


				String directory = outputDir + File.separator + key + File.separator;

				File dirFile = new File(directory);
				if (!dirFile.isDirectory()) {
					dirFile.mkdir();
				}

				VOI voiNewFinal = new VOI((short) 0, "voi_result_" + key);
				int[] extents = new int[2];
				int[] srcExtents = srcImage.getExtents();

				int[] blankExts = new int[2];
				blankExts[0] = srcExtents[0];
				blankExts[1] = srcExtents[1];


				int imageDim = srcExtents[2];

				for (int j = 0; j < imageDim; j++) {

					ModelImage image = imageVec.get(j);
					
					ModelImage blankImage = new ModelImage(image.getType(), blankExts, "_blank");
					int[] imageExt = image.getExtents();
					int imageSize = imageExt[0] * imageExt[1];
					int[] maskBuffer = new int[imageSize];

					try {
						image.exportData(0, imageSize, maskBuffer);
						
						blankImage.importData(0, maskBuffer, true);
						FileInfoBase fileInfo = blankImage.getFileInfo()[0];
						fileInfo.setResolutions(srcResol);
						fileInfo.setUnitsOfMeasure(units);
					} catch (IOException e) {
						e.printStackTrace();
					}

				

					extents = blankImage.getExtents();
					String name = "binaryMask_" + blankImage.getImageName();
					
					ModelImage resultImage = new ModelImage(ModelImage.BOOLEAN, extents, name);
					float[] thresholds = new float[2];
					thresholds[0] = 240;
					thresholds[1] = 255;
					float fillValue = 0f;
					boolean isInverse = false;
					boolean regionFlag = true;
					int outputType = 1;

					AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(resultImage, blankImage, thresholds, fillValue, outputType, regionFlag, isInverse);
					thresholdAlgo.run();

					
					resultImage.calcMinMax();
					AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(resultImage);
					VOIExtractionAlgo.run();

					VOIVector v = resultImage.getVOIs();
					if (v.size() == 0)
						continue;

					// for femur part seg
					if (resultImage.getVOIs() != null) {

						for (int idx = 0; idx < resultImage.getVOIs().size(); idx++) {
							VOIBaseVector current_va = resultImage.getVOIs().VOIAt(idx).getCurves();
							VOIBase current_v = current_va.get(0);
							VOIBase vTemp = (VOIBase) current_v.clone();
							int nPtsCurrent = current_v.size();


							float[] xPts = new float[nPtsCurrent];
							float[] yPts = new float[nPtsCurrent];
							float[] zPts = new float[nPtsCurrent];

							current_v.exportArrays(xPts, yPts, zPts);

							for (int k = 0; k < nPtsCurrent; k++) {
								zPts[k] = j;
							}

							vTemp.importArrays(xPts, yPts, zPts, nPtsCurrent);
							voiNewFinal.importCurve(vTemp);
						}
						
					}
                  
					// thresholdAlgo.finalize();
					thresholdAlgo = null;
					// idObjectsAlgo2D.finalize();
					// idObjectsAlgo2D = null;
					
					// VOIExtractionAlgo.finalize();
					VOIExtractionAlgo = null;
					
					maskBuffer = null;
					// maxMaskImage.disposeLocal(false);
					// maxMaskImage = null;
					// maskBuffer_ced = null;
					maskBuffer = null;
					// blankImage.disposeLocal(false);
					blankImage = null;
					// blankImage_ced.disposeLocal(false);
					// blankImage_ced = null;
					
					// mathAlgo.finalize();
					// mathAlgo = null;
					
					// resultImage.disposeLocal(false);
					resultImage = null;
					
				} // end for j loop

				voiNewFinal.update();
				// save VOI contours

				int[] newExtents = new int[3];
				newExtents[0] = extents[0];
				newExtents[1] = extents[1];
				newExtents[2] = imageVec.size();

				srcImage.registerVOI(voiNewFinal);
				// for patella part only, femur don't need this
				// smoothVOI30(srcImage, srcImage);
				try {

					System.err.println("directory = " + directory);

					FileVOI fileVOI = new FileVOI("voi_" + key + ".xml",directory, srcImage);
					fileVOI.writeVOI(voiNewFinal, true);

					// srcImage.saveImage(directory, "image_" + i + ".xml", FileUtility.XML, false);
					System.err.println("source image name = " + srcImage.getImageName());
					srcImage.saveImage(directory,srcImage.getImageName() , FileUtility.XML, false);
					srcImage.disposeLocal(false);
					srcImage = null;
					fileVOI.finalize();
					fileVOI = null;

				} catch (IOException e) {
					e.printStackTrace();
				}

				imageVec.clear();
				imageVec = null;
				newExtents = null;
				System.err.println("done dispose");
			
				
			
		}
		
	}
	
	/**
	 * Read testing images and the HNN predcited probability maps. 
	 */
	public void readFile() {

		try {
			
			int index;
		
			Set<String> keys = nameTableImages.keySet();

			for (String key : keys) {
				System.err.println("axial key = " + key);
				String dir = nameTableImages.get(key);
				if (dir == null)
					continue;
				// System.err.println("dir = " + dir);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					
					keyImagesOrientation.put(key, image);
					keyImagesOrientation.get(key).setImageName(key);
					
				}
				keyImageIO = null;
			}
	        
			keys = maskImageTable.keySet();
	        for(String key : keys ){
	        	  
					Vector<String> stringVec = maskImageTable.get(key);
					Vector<ModelImage> imageVec = new Vector<ModelImage>();

					for (int j = 0; j < stringVec.size(); j++) {
						String dir = stringVec.get(j);
						index = dir.lastIndexOf(File.separator);
						String directory = new String(dir.substring(0, index + 1));
						String fileName = new String(dir.substring(index + 1, dir.length()));
						System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
						FileIO keyImageIO = new FileIO();
						imageVec.add(keyImageIO.readImage(fileName, directory));
					}
					System.err.println("axial key = " + key);
					maskTable.put(key, imageVec);
        	   
			}
			
			
		
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	
	/** 
	 * Sort the testing image directory in accending order. 
	 */
	private void sortImageTable() {
		
		Set<String> keys = imageTable.keySet();
		for (String key : keys ) {
				Vector<String> stringVec = imageTable.get(key);
				Vector<String> newStringVect = new Vector<String>();
				Hashtable<Integer, String> hash = new Hashtable<Integer, String>(); 
				for ( int j = 0; j < stringVec.size(); j++ ) {
					String fullName = stringVec.get(j);
					int stopIndex = fullName.lastIndexOf("-");
					String subName = fullName.substring(0, stopIndex-1);
					stopIndex = subName.lastIndexOf("-");
					int startIdx = fullName.lastIndexOf("_");
					String numString = fullName.substring(startIdx+1, stopIndex);
					
					int num = Integer.valueOf(numString);
					
					hash.put(num, fullName);
				}
				
				for ( int j = 0; j < 1000; j++ ) {
					String name = hash.get(j);
					if ( name != null ) {
						newStringVect.add(name);
						System.err.println("name = " + name);
					}
				}
				System.err.println("test key = " + key);
				maskImageTable.put(key, newStringVect);
		}
		// pause();
		System.err.println(" finish inti read dir");
		
	}

    
	/**
	 * Parsing 3D images directory and the HNN generated probablity maps directory
	 */
	private void readKeyImageDir() {
		
		File fileDir_image = new File(inputDirImage);
		traverse_image_folder(fileDir_image, null);
		
		File fileDir_map = new File(inputDirMap);
		traverse_folder_map(fileDir_map);
	}

	
	/**
	 * Traverse the test image folder
	 */
	private void traverse_image_folder(File dir, String hashID) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				read_image_name(new File(dir, children[i]));
			}
		}

	}

	/**
	 * Grab the test image in mipav .xml file format.   
	 * @param dir  image sub-directory name
	 */
	private void read_image_name(File dir) {

		String dirName = dir.toString();

		String lowerCaseName;
		String fileName;
		String hashID;

		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		lowerCaseName = dirName.toLowerCase();
		fileName = lowerCaseName.substring(begin, end);
		begin = fileName.lastIndexOf("_") + 1;
		end = fileName.lastIndexOf(".");

		hashID = fileName.substring(begin, end);

		if (fileName.startsWith("image") && fileName.endsWith("xml") && 
				!fileName.contains("ced") && !fileName.contains("mask")) {
			// System.err.println("hashID = " + hashID + "   fileName = " + fileName );
			nameTableImages.put(hashID, dirName);
		}


	}

	/*
	 * Traverse the HNN generated probability map directory
	 */
	private void traverse_folder_map(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_map(new File(dir, children[i]), i);
			}
		}
	}
	
	/**
	 * Traverse the HNN subdirectories. 
	 * @param dir    directory name
	 * @param index    map index
	 */
	private void traverse_folder_map(File dir, int index) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder_map(new File(dir, children[i]), index);
			}
		}

	}

	/**
	 * HNN generates multi-scaled prediction output.  We use fuse layer generated output as the energy map
	 * to inference the VOI contour. 
	 * @param dir  sub-directory name. 
	 * @param index    probablity map index
	 */
	private void processDir_folder_map(File dir, int index) {
		
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".png")
				&& dirName.contains("fuse")) {
			System.err.println("file = " + dir.toString());
			String fullName = dir.toString();
			int idxEnd = fullName.lastIndexOf("/");
			String subName = fullName.substring(0, idxEnd);
			int idxStart = subName.lastIndexOf("/");
			String numString = fullName.substring(idxStart+1, idxEnd);
			String hashIndex = numString;
			System.err.println(numString);
			if ( imageTable.get(hashIndex) == null ) {
				imageTable.put(hashIndex, new Vector<String>());
			}
		    imageTable.get(hashIndex).add(dir.toString());
		}
	}
	
    
	
}

