import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileRaw;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;

/**
 * 
 * @author pandyan
 * 
 * This is the algorithm for the DTI Save raw Volumes Plug-In
 * 
 * References: This algorithm was developed in concert with Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 *
 */
public class PlugInAlgorithmDTISaveRawVolumes extends AlgorithmBase {
	
	/** list file abs path **/
	private String listFileAbsPath;
	
	/** parent dir **/
	private String parentDir;
	
	/** boolean for if is xyz **/
	private boolean isXYZ;
	
	/** boolean for if is nifti **/
	private boolean isNIFTI;

	/** BufferedReader **/
	private BufferedReader bReader = null;
	
	/** x dimension **/
	private int xDim;
	
	/** y dimension **/
	private int yDim;
	
	/** z dimension **/
	private int zDim;
	
	/** t dimension **/
	private int tDim;
	
	/** x resolution **/
	private float xRes;
	
	/** y resolution **/
	private float yRes;
	
	/** z resolution **/
	private float zRes;
	
	/** format of images **/
	private String format;
	
	/** path filename **/
	private String pathFilename;
	
	/** study name **/
	private String studyName;

	/** path to pathfile **/
	private String pathFileAbsPath;
	
	/** dest image **/
	private ModelImage destImage;
	
	/** extents **/
	private int[] sliceExtents,volExtents;
	
	/** array list of path strings **/
	private ArrayList<String> pathsAL;
	
	
	/**
	 * empty constructor
	 */
	public PlugInAlgorithmDTISaveRawVolumes() {
		
	}
	
	/**
	 * constructor
	 * @param listFileAbsPath
	 * @param isXYZ
	 * @param isNIFTI
	 */
	public PlugInAlgorithmDTISaveRawVolumes(String listFileAbsPath, boolean isXYZ, boolean isNIFTI) {
		this.listFileAbsPath = listFileAbsPath;
		this.parentDir = listFileAbsPath.substring(0, listFileAbsPath.lastIndexOf(File.separator));
		this.isXYZ = isXYZ;
		this.isNIFTI = isNIFTI;
	}

	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		fireProgressStateChanged(5);
		boolean success = false;
		success = readListFile();
		if(success) {
			success = readPathFile();
		}else {
			MipavUtil.displayError("Error reading list file");
			setCompleted(true);
            return;
		}
		if (success) {
			if(format.equalsIgnoreCase("float")) {
				if(isXYZ) {
					success = saveXYZRaw();
				}else {
					success = saveXYRRaw();
				}
				if(success) {
					setCompleted(true);
		            return;
				}else {
					MipavUtil.displayError("Error saving files");
					setCompleted(true);
		            return;
				}
				
			}else {
				MipavUtil.displayError("This algorithm only works on RAW FLOAT images");
				setCompleted(true);
	            return;
			}
		}else {
			MipavUtil.displayError("Error reading path file");
			setCompleted(true);
            return;
		}

	}
	
	
	/**
	 * read list file
	 * @return success
	 */
	public boolean readListFile() {
		System.out.println("Reading in list file..." + listFileAbsPath);
		File listFile = new File(listFileAbsPath);
		fireProgressStateChanged("Reading in list file..." + listFileAbsPath);
		fireProgressStateChanged(10);
		try {
            bReader = new BufferedReader(new FileReader(listFile));
            String lineString = null;
            while((lineString = bReader.readLine()) != null) {
            	if(lineString.startsWith("<original_columns>")) {
            		String columnsStr = lineString.substring(lineString.indexOf("<original_columns>") + 18, lineString.indexOf("</original_columns>")).trim();
            		xDim = Integer.parseInt(columnsStr);
            	}else if(lineString.startsWith("<original_rows>")) {
            		String rowsStr = lineString.substring(lineString.indexOf("<original_rows>") + 15, lineString.indexOf("</original_rows>")).trim();
            		yDim = Integer.parseInt(rowsStr);
            	}else if(lineString.startsWith("<slice>")) {
            		String sliceStr = lineString.substring(lineString.indexOf("<slice>") + 7, lineString.indexOf("</slice>")).trim();
            		zDim = Integer.parseInt(sliceStr);
            	}else if(lineString.startsWith("<nim>")) {
            		String nimStr = lineString.substring(lineString.indexOf("<nim>") + 5, lineString.indexOf("</nim>")).trim();
            		tDim = Integer.parseInt(nimStr);
            	}else if(lineString.startsWith("<rawimageformat>")) {
            		format = lineString.substring(lineString.indexOf("<rawimageformat>") + 16, lineString.indexOf("</rawimageformat>")).trim();
            	}else if(lineString.startsWith("<raw_image_path_filename>")) {
            		pathFilename = lineString.substring(lineString.indexOf("<raw_image_path_filename>") + 25, lineString.indexOf("</raw_image_path_filename>")).trim();
            		pathFileAbsPath = parentDir + File.separator + pathFilename;
            		studyName = pathFilename.substring(0, pathFilename.indexOf(".path"));
            	}else if(lineString.startsWith("<x_field_of_view>")) {
            		String xFOVStr = lineString.substring(lineString.indexOf("<x_field_of_view>") + 17, lineString.indexOf("</x_field_of_view>")).trim(); 
            		float xFOV = Float.parseFloat(xFOVStr);
            		xRes = xFOV/xDim;
            	}else if(lineString.startsWith("<y_field_of_view>")) {
            		String yFOVStr = lineString.substring(lineString.indexOf("<y_field_of_view>") + 17, lineString.indexOf("</y_field_of_view>")).trim(); 
            		float yFOV = Float.parseFloat(yFOVStr);
            		yRes = yFOV/yDim;
            	}else if(lineString.startsWith("<slice_thickness>")) {
            		String zResStr = lineString.substring(lineString.indexOf("<slice_thickness>") + 17, lineString.indexOf("</slice_thickness>")).trim(); 
            		zRes = Float.parseFloat(zResStr);
            	}
            }
           

            bReader.close();
            bReader = null;
            return true;
		}catch(Exception e) {
			e.printStackTrace();
			return false;
		}
	}
	
	
	
	
	
	/**
	 * read path file
	 * @return success
	 */
	public boolean readPathFile() {
		System.out.println("Reading in path file..." + pathFileAbsPath);
		File pathFile = new File(pathFileAbsPath);
		String[] paths = new String[zDim * tDim];
		try {
            bReader = new BufferedReader(new FileReader(pathFile));
            String lineString = null;

            for(int i=0;i<(zDim * tDim);i++) {
            	lineString = bReader.readLine();
            	String pathString = parentDir + File.separator + lineString;
            	paths[i] = pathString;
    		}
            
		}catch (Exception e) {
			e.printStackTrace();
			return false;
		}

		
		pathsAL = new ArrayList<String>(Arrays.asList(paths));
		
		return true;
	}
	
	

	
	
	
	
	
	/**
	 * saveXYZRaw
	 * reads raw in x,y,z format and saves as 3d vols
	 * @return success
	 */
	public boolean saveXYZRaw() {
		long begTime = System.currentTimeMillis();
		int type = ModelStorageBase.FLOAT;

		FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
		FileRaw fileRaw = null;
		float[] buffer;	
		buffer = new float[xDim * yDim];
		String dir;
		String filename;
		FileInfoImageXML fileInfo;
		
		volExtents = new int[3];
		volExtents[0] = xDim;
		volExtents[1] = yDim;
		volExtents[2] = zDim;
		 
		sliceExtents = new int[3];
		sliceExtents[0] = xDim;
		sliceExtents[1] = yDim;

		
		float[] volResols = new float[3];
        int[] volUnits = new int[3];
        float[] sliceResols = new float[2];
        int[] sliceUnits = new int[2];
        sliceResols[0] = xRes;
        sliceUnits[0] = Unit.MILLIMETERS.getLegacyNum();
        sliceResols[1] = yRes;
        sliceUnits[1] = Unit.MILLIMETERS.getLegacyNum();
        float[] startLocs = new float[3];
		String saveDir = parentDir + File.separator + studyName + "_NIFTI-xyz" + File.separator;
		new File(saveDir).mkdirs();
		String saveFilename  = "";
		
		fireProgressStateChanged(15);
		try {
			for(int i=0;i<tDim;i++) {
				
				//slice counter
				int slice=0;
				String currVolString = "";
				int currVol = i + 1;
				if(currVol < 10) {
					currVolString = "00" + currVol;
				}else if(currVol >= 10 && currVol < 100) {
					currVolString = "0" + currVol;
				}else {
					currVolString = String.valueOf(currVol);
				}
				fireProgressStateChanged("Processing raw files...Vol " + currVolString + "/" + tDim);
				fireProgressStateChanged(15 + (i * 80 /tDim));
				System.out.println("Processing Vol " + currVolString + "/" + tDim);
				destImage = new ModelImage(ModelStorageBase.FLOAT, volExtents, studyName);
				if(isNIFTI) {
					saveFilename = studyName + "_vol" + currVolString + ".nii";
				}else {
					saveFilename = studyName + "_vol" + currVolString + ".nrrd";
				}
				for(int k=i;k<pathsAL.size();k=k+tDim) {
					String path = (String)pathsAL.get(k);
					dir = path.substring(0,path.lastIndexOf(File.separator)) + File.separator;
					filename = path.substring(path.lastIndexOf(File.separator) + 1, path.length());
					fileInfo = new FileInfoImageXML(filename, dir, FileUtility.RAW);
					fileInfo.setDataType(type);
				    fileInfo.setExtents(sliceExtents);
				    fileInfo.setUnitsOfMeasure(sliceUnits);
				    fileInfo.setResolutions(sliceResols);
				    fileInfo.setEndianess(FileBase.BIG_ENDIAN);
				    fileInfo.setOffset(0);
				    fileRaw = new FileRaw(filename, dir, fileInfo, FileBase.READ);

					//read in raw slice data into buffer
					fileRaw.readImage(buffer, fileInfo.getOffset(), type);

					//import buffer into destImage
	                destImage.importData((buffer.length * slice), buffer, false);
	                
	                //clone file info and set dest image slice file info
					destImage.setFileInfo((FileInfoBase) (fileInfo.clone()), slice);

					//set up additional info needed for file info
					volResols[0] = xRes;
					volUnits[0] = Unit.MILLIMETERS.getLegacyNum();
			        startLocs[0] = 0;
			        volResols[1] = yRes;
			        volUnits[1] = Unit.MILLIMETERS.getLegacyNum();
			        startLocs[1] = 0;
			        volResols[2] = zRes;
			        volUnits[2] = Unit.MILLIMETERS.getLegacyNum();
			        startLocs[2] = 0;
			        destImage.getFileInfo(slice).setResolutions(volResols);
	                destImage.getFileInfo(slice).setUnitsOfMeasure(volUnits);
	                destImage.getFileInfo(slice).setExtents(volExtents);
	                destImage.getFileInfo(slice).setOrigin(startLocs);
	                
	                
	                
	                //write to nifti only if we are at the last slice in the vol
	                if(slice == zDim - 1) {
	                	destImage.calcMinMax();
	                	FileWriteOptions opts = new FileWriteOptions(true);
	                    opts.setFileType(FileUtility.NIFTI);
	                    opts.setFileDirectory(saveDir);
	                    opts.setFileName(saveFilename);
	                    opts.setBeginSlice(0);
	                    opts.setEndSlice(zDim - 1);
	                    opts.setOptionsSet(true);
	                    fileIO.writeImage(destImage, opts);
	                    
	                	destImage.disposeLocal();
	                	destImage = null;
	                }

	                slice++;
				}
			}
			
		}catch (Exception e) {
			e.printStackTrace();
			return false;
		}

		
		long endTime = System.currentTimeMillis();
        long diffTime = endTime - begTime;
        float seconds = ((float) diffTime) / 1000;
        
		fireProgressStateChanged(95);
		System.out.println("** Algorithm took " + seconds + " seconds \n");
		
		return true;
	}
	
	
	
	
	
	
	
	
	
	
	
	
	/**
	 * saveXYRRaw
	 * reads raw in x,y,z format and saves as 3d vols
	 * @return success
	 */
	public boolean saveXYRRaw() {
		long begTime = System.currentTimeMillis();
		int type = ModelStorageBase.FLOAT;

		FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
		FileRaw fileRaw = null;
		float[] buffer;	
		buffer = new float[xDim * yDim];
		String dir;
		String filename;
		FileInfoImageXML fileInfo;
		
		volExtents = new int[3];
		volExtents[0] = xDim;
		volExtents[1] = yDim;
		volExtents[2] = tDim;
		 
		sliceExtents = new int[3];
		sliceExtents[0] = xDim;
		sliceExtents[1] = yDim;

		
		float[] volResols = new float[3];
        int[] volUnits = new int[3];
        float[] sliceResols = new float[2];
        int[] sliceUnits = new int[2];
        sliceResols[0] = xRes;
        sliceUnits[0] = Unit.MILLIMETERS.getLegacyNum();
        sliceResols[1] = xRes;
        sliceUnits[1] = Unit.MILLIMETERS.getLegacyNum();
        float[] startLocs = new float[3];
		String saveDir = parentDir + File.separator + studyName + "_NIFTI-xyr" + File.separator;
		new File(saveDir).mkdirs();
		String saveFilename  = "";
		
		fireProgressStateChanged(15);
		try {
			for(int i=0;i<zDim;i++) {
				//vol counter
				int volSlice=0;
			
				String currVolString = "";
				int currVol = i + 1;
				if(currVol < 10) {
					currVolString = "00" + currVol;
				}else if(currVol >= 10 && currVol < 100) {
					currVolString = "0" + currVol;
				}else {
					currVolString = String.valueOf(currVol);
				}
				fireProgressStateChanged("Processing raw files...Slice " + currVolString + "/" + zDim);
				fireProgressStateChanged(15 + (i * 80 /zDim));
				System.out.println("Processing Slice " + currVolString + "/" + zDim);
				destImage = new ModelImage(ModelStorageBase.FLOAT, volExtents, studyName);
				if(isNIFTI) {
					saveFilename = studyName + "_slice" + currVolString + ".nii";
				}else {
					saveFilename = studyName + "_slice" + currVolString + ".nrrd";
				}
				for(int k=i*tDim;k<((i*tDim)+tDim);k++) {
					String path = (String)pathsAL.get(k);
					dir = path.substring(0,path.lastIndexOf(File.separator)) + File.separator;
					filename = path.substring(path.lastIndexOf(File.separator) + 1, path.length());
					fileInfo = new FileInfoImageXML(filename, dir, FileUtility.RAW);
					fileInfo.setDataType(type);
				    fileInfo.setExtents(sliceExtents);
				    fileInfo.setUnitsOfMeasure(sliceUnits);
				    fileInfo.setResolutions(sliceResols);
				    fileInfo.setEndianess(FileBase.BIG_ENDIAN);
				    fileInfo.setOffset(0);
				    fileRaw = new FileRaw(filename, dir, fileInfo, FileBase.READ);

					//read in raw slice data into buffer
					fileRaw.readImage(buffer, fileInfo.getOffset(), type);

					//import buffer into destImage
	                destImage.importData((buffer.length * volSlice), buffer, false);
	                
	                //clone file info and set dest image slice file info
					destImage.setFileInfo((FileInfoBase) (fileInfo.clone()), volSlice);

					//set up additional info needed for file info
					volResols[0] = xRes;
					volUnits[0] = Unit.MILLIMETERS.getLegacyNum();
			        startLocs[0] = 0;
			        volResols[1] = yRes;
			        volUnits[1] = Unit.MILLIMETERS.getLegacyNum();
			        startLocs[1] = 0;
			        volResols[2] = 0;
			        volUnits[2] = Unit.MILLIMETERS.getLegacyNum();
			        startLocs[2] = 0;
			        destImage.getFileInfo(volSlice).setResolutions(volResols);
	                destImage.getFileInfo(volSlice).setUnitsOfMeasure(volUnits);
	                destImage.getFileInfo(volSlice).setExtents(volExtents);
	                destImage.getFileInfo(volSlice).setOrigin(startLocs);

	                //write to nifti only if we are at the last slice in the vol
	                if(volSlice == tDim - 1) {
	                	destImage.calcMinMax();
	                	FileWriteOptions opts = new FileWriteOptions(true);
	                    opts.setFileType(FileUtility.NIFTI);
	                    opts.setFileDirectory(saveDir);
	                    opts.setFileName(saveFilename);
	                    opts.setBeginSlice(0);
	                    opts.setEndSlice(tDim - 1);
	                    opts.setOptionsSet(true);
	                    fileIO.writeImage(destImage, opts);
	                    
	                	destImage.disposeLocal();
	                	destImage = null;
	                }

	                volSlice++;
				}
			}
			
		}catch (Exception e) {
			e.printStackTrace();
			return false;
		}

		
		long endTime = System.currentTimeMillis();
        long diffTime = endTime - begTime;
        float seconds = ((float) diffTime) / 1000;
        
		fireProgressStateChanged(95);
		System.out.println("** Algorithm took " + seconds + " seconds \n");
		
		return true;
	}

}
