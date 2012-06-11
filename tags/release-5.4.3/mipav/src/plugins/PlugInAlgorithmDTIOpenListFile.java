import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileRaw;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;


/**
 * This is the algorithm for the DTI Open List File Plug-In
 * This plugin opens a 4D DTI dataset by reading in the list file
 * @author pandyan
 * 
 * 
 * References: This algorithm was developed in concert with Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 *
 */
public class PlugInAlgorithmDTIOpenListFile extends AlgorithmBase {

	/** path to list file **/
	private String listFileAbsPath;
	
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
	
	/** parent dir **/
	private String parentDir;
	
	/** path to pathfile **/
	private String pathFileAbsPath;
	
	/** dest image **/
	private ModelImage destImage;
	
	/** extents for dest image **/
	private int[] extents;
	
	/** endianness **/
	private boolean endianness = FileBase.LITTLE_ENDIAN;;
	
	
	ArrayList<String> pathsAL;
	

	
	


	/**
	 * empty constructor
	 *
	 */
	public PlugInAlgorithmDTIOpenListFile() {
		
	}
	
	/**
	 * constructor
	 * @param absPath
	 * @param parentDir
	 */
	public PlugInAlgorithmDTIOpenListFile(String absPath,String parentDir) {
		this.listFileAbsPath = absPath;
		this.parentDir = parentDir;
	}
	
	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		fireProgressStateChanged("Reading list file");
        fireProgressStateChanged(5);
		boolean success = false;
		success = readListFile();
		if(success) {
			success = readPathFile();
		}
		if (success) {
			if(format.equalsIgnoreCase("dicom")) {
				fireProgressStateChanged("Reading dicom files");
				success = openDicom();
			} else if(format.equalsIgnoreCase("float")) {
				fireProgressStateChanged("Reading raw files");
				success = openRaw();
			}
			if (success) {
				setCompleted(true);
			}
		}
	}
	
	/**
	 * read list file
	 *
	 */
	public boolean readListFile() {
		File listFile = new File(listFileAbsPath);
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
            	}else if(lineString.startsWith("<endian_raw_in>")) {
            		String endiannessStr = lineString.substring(lineString.indexOf("<endian_raw_in>") + 15, lineString.indexOf("</endian_raw_in>")).trim(); 
            		if(endiannessStr.equalsIgnoreCase("BIG")) {
            			endianness = FileBase.BIG_ENDIAN;
            		}
            	}
            }
            extents = new int[4];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
            extents[3] = tDim;
            bReader.close();
            bReader = null;
            return true;
		}catch(Exception e) {
			e.printStackTrace();
			return false;
		}
	}
	
	
	/**
	 * reads raw and exports to 4d dataset
	 * @return
	 */
	public boolean openRaw() {
		long begTime = System.currentTimeMillis();
		fireProgressStateChanged(15);
		int length;
		float[] buffer;	
		length = xDim * yDim;
		int[] ext2D = new int[2];
		ext2D[0] = xDim;
		ext2D[1] = yDim;
		buffer = new float[length];
		int[] unitsOfMeas2D = new int[2];
		unitsOfMeas2D[0] = Unit.MILLIMETERS.getLegacyNum();
		unitsOfMeas2D[1] = Unit.MILLIMETERS.getLegacyNum();
		float[] res2D = new float[2];
		res2D[0] = xRes;
		res2D[1] = yRes;
		String dir;
		String filename;
		FileInfoImageXML fileInfo = null;
		float[] resols = new float[4];
		resols[0] = xRes;
		resols[1] = yRes;
		resols[2] = zRes;
		resols[3] = 0;
        int[] units = new int[4];
        units[0] = Unit.MILLIMETERS.getLegacyNum();
        units[1] = Unit.MILLIMETERS.getLegacyNum();
        units[2] = Unit.MILLIMETERS.getLegacyNum();
        units[3] = Unit.MILLIMETERS.getLegacyNum();
        float[] startLocs = new float[4];
        startLocs[0] = 0;
        startLocs[1] = 0;
        startLocs[2] = 0;
        startLocs[3] = 0;
		int j=0;
		FileRaw fileRaw = null;
		fireProgressStateChanged(20);
		try {
			for(int i=0;i<tDim;i++) {
				int currVol = i + 1;
				int totVols = tDim;
				fireProgressStateChanged("Reading raw files...Vol " + currVol + "/" + totVols);
				fireProgressStateChanged(20 + (i * 75 /tDim));
				for(int k=i;k<pathsAL.size();k=k+tDim) {
					String path = (String)pathsAL.get(k);
					dir = path.substring(0,path.lastIndexOf(File.separator)) + File.separator;
					filename = path.substring(path.lastIndexOf(File.separator) + 1, path.length());
					fileInfo = new FileInfoImageXML(filename, dir, FileUtility.RAW);
					fileInfo.setDataType(ModelStorageBase.FLOAT);
					fileInfo.setExtents(ext2D);
					fileInfo.setUnitsOfMeasure(unitsOfMeas2D);
					fileInfo.setResolutions(res2D);
					fileInfo.setEndianess(endianness);
					fileInfo.setOffset(0);
					fileRaw = new FileRaw(filename, dir, fileInfo, FileBase.READ);
					fileRaw.readImage(buffer, 0, ModelStorageBase.FLOAT);
					if(j == 0) {
						destImage = new ModelImage(ModelStorageBase.FLOAT, extents, studyName);
					}
					destImage.importData(length * j, buffer, false);
					destImage.setFileInfo((FileInfoBase) (fileInfo.clone()), j);
			        destImage.getFileInfo(j).setResolutions(resols);
	                destImage.getFileInfo(j).setUnitsOfMeasure(units);
	                destImage.getFileInfo(j).setExtents(extents);
	                destImage.getFileInfo(j).setOrigin(startLocs);

					j++;
					
				}
			}
		}catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		destImage.calcMinMax();
		long endTime = System.currentTimeMillis();
        long diffTime = endTime - begTime;
        float seconds = ((float) diffTime) / 1000;
		fireProgressStateChanged(95);
		System.out.println("** Algorithm took " + seconds + " seconds \n");
		return true;
	}

	

	
	/**
	 * reads dicom and exports to 4d dataest
	 * @return
	 */
	public boolean openDicom() {
		long begTime = System.currentTimeMillis();
		fireProgressStateChanged(15);
		int length;
		short[] buffer;	
		length = xDim * yDim;	
		buffer = new short[length];
		String dir;
		String filename;
		FileInfoBase fileInfo = null;
		float[] resols = new float[4];
        int[] units = new int[4];
        float[] startLocs = new float[4];
		int j=0;
		FileDicom fileDicom = null;
		FileInfoDicom refFileInfo = null;
		fireProgressStateChanged(20);
		
		try {
			for(int i=0;i<tDim;i++) {
				int currVol = i + 1;
				int totVols = tDim;
				fireProgressStateChanged("Reading dicom files...Vol " + currVol + "/" + totVols);
				fireProgressStateChanged(20 + (i * 75 /tDim));
				for(int k=i;k<pathsAL.size();k=k+tDim) {
					String path = (String)pathsAL.get(k);
					dir = path.substring(0,path.lastIndexOf(File.separator)) + File.separator;
					filename = path.substring(path.lastIndexOf(File.separator) + 1, path.length());
					if(j == 0) {
						fileDicom = new FileDicom(filename, dir);
						fileDicom.setQuiet(true);
						fileDicom.readHeader(true);
						refFileInfo = (FileInfoDicom) fileDicom.getFileInfo();
						fileInfo = fileDicom.getFileInfo();
					}else {
						
						fileDicom.setFileName(filename, dir, refFileInfo);
						fileDicom.readHeader(true);
						fileInfo = fileDicom.getFileInfo();
					}
					fileDicom.readImage(buffer, ModelStorageBase.SHORT, 0);
					if(j == 0) {
						destImage = new ModelImage(ModelStorageBase.SHORT, extents, studyName);
					}
					destImage.importData(length * j, buffer, false);
					destImage.setFileInfo((FileInfoBase) (fileInfo.clone()), j);
					for (int m = 0; m< 3; m++) {
			            resols[m] = fileInfo.getResolutions()[m];
			            units[m] = fileInfo.getUnitsOfMeasure()[m];
			            startLocs[m] = fileInfo.getOrigin(m);
			        }
					resols[3] = 0;
			        units[3] = Unit.MILLIMETERS.getLegacyNum();
			        startLocs[3] = 0;
			        
			        destImage.getFileInfo(j).setResolutions(resols);
	                destImage.getFileInfo(j).setUnitsOfMeasure(units);
	                destImage.getFileInfo(j).setExtents(extents);
	                destImage.getFileInfo(j).setOrigin(startLocs);

					j++;
					
				}
			}
		}catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		destImage.calcMinMax();
		long endTime = System.currentTimeMillis();
        long diffTime = endTime - begTime;
        float seconds = ((float) diffTime) / 1000;
		fireProgressStateChanged(95);
		System.out.println("** Algorithm took " + seconds + " seconds \n");
		
		return true;
	}
	
	
	
	
	/**
	 * 
	 * @return
	 */
	public boolean readPathFile() {
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
	 * get dest image
	 */
	public ModelImage getDestImage() {
		return destImage;
	}
	
	
	

}
