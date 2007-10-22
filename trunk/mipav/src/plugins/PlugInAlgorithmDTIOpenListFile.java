import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;


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
        fireProgressStateChanged(10);
		boolean success = false;
		success = readListFile();
		if (success) {
			if(format.equalsIgnoreCase("dicom")) {
				fireProgressStateChanged("Reading dicom files");
				success = openDicom();
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
		fireProgressStateChanged(20);
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
	 * reads dicom and exports to 4d dataest
	 * @return
	 */
	public boolean openDicom() {
		long begTime = System.currentTimeMillis();
		fireProgressStateChanged(30);
		File pathFile = new File(pathFileAbsPath);
		String[] paths = new String[zDim * tDim];
		
		FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
		
		
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

		
		ArrayList pathsAL = new ArrayList(Arrays.asList(paths));
		
		int length;
		float[] buffer;	
		length = xDim * yDim;	
		buffer = new float[length];
		ModelImage volume;
		String dir;
		String filename;
		FileInfoBase[] fileInfo;
		float[] resols = new float[4];
        int[] units = new int[4];
        float[] startLocs = new float[4];
		int j=0;
		fireProgressStateChanged(35);
		try {
			for(int i=0;i<tDim;i++) {
				long begVolTime = System.currentTimeMillis();
				int currVol = i + 1;
				int totVols = tDim;
				fireProgressStateChanged("Reading dicom files...Vol " + currVol + "/" + totVols);
				fireProgressStateChanged(35 + (i * 60 /tDim));
				System.out.println("i is " + i);
				for(int k=i;k<pathsAL.size();k=k+tDim) {
					//fireProgressStateChanged(35 + (i * 60 /t));
					System.out.println("k is  " + k);
					String path = (String)pathsAL.get(k);
					dir = path.substring(0,path.lastIndexOf(File.separator)) + File.separator;
					filename = path.substring(path.lastIndexOf(File.separator) + 1, path.length());
						long begVolReadTime = System.currentTimeMillis();
					volume = fileIO.readImage(filename,dir,false,null);
						long endVolReadTime = System.currentTimeMillis();
						long diffVolReadTime = endVolReadTime - begVolReadTime;
						float volReadSeconds = ((float) diffVolReadTime) / 1000;
						System.out.println("******** reading in image took " + volReadSeconds + " seconds \n");

						
						long begVolExportTime = System.currentTimeMillis();
					volume.exportData(0, length, buffer);
						long endVolExportTime = System.currentTimeMillis();
						long diffVolExportTime = endVolExportTime - begVolExportTime;
						float volExportSeconds = ((float) diffVolExportTime) / 1000;
						System.out.println("******** exporting data buffer took " + volExportSeconds + " seconds \n");
					if(j == 0) {
						destImage = new ModelImage(volume.getType(), extents, studyName);
					}
					fileInfo = volume.getFileInfo();
						long begVolImportTime = System.currentTimeMillis();
					destImage.importData(length * j, buffer, true);
						long endVolImportTime = System.currentTimeMillis();
						long diffVolImportTime = endVolImportTime - begVolImportTime;
						float volImportSeconds = ((float) diffVolImportTime) / 1000;
						System.out.println("******** importing data buffer took " + volImportSeconds + " seconds \n");
					destImage.setFileInfo((FileInfoBase) (fileInfo[0].clone()), j);
					for (int m = 0; m< 3; m++) {
			            resols[m] = fileInfo[0].getResolutions()[m];
			            units[m] = fileInfo[0].getUnitsOfMeasure()[m];
			            startLocs[m] = fileInfo[0].getOrigin(m);
			        }
					resols[3] = 0;
			        units[3] = FileInfoBase.MILLIMETERS;
			        startLocs[3] = 0;
			        
			        destImage.getFileInfo(j).setResolutions(resols);
	                destImage.getFileInfo(j).setUnitsOfMeasure(units);
	                destImage.getFileInfo(j).setExtents(extents);
	                destImage.getFileInfo(j).setOrigin(startLocs);
	                volume.disposeLocal();
					volume = null;
					j++;
					
				}
				long endVolTime = System.currentTimeMillis();
		        long diffVolTime = endVolTime - begVolTime;
		        float volSeconds = ((float) diffVolTime) / 1000;
		        System.out.println("******** volume took " + volSeconds + " seconds \n");
			}
		}catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		
		
	
		//delete MODEL IMAGES!
		
		
		
		
		
		
		
		//fireProgressStateChanged(95);
		
		


		
		//import the data to 4d datset and copy over file info
		/*
		fireProgressStateChanged("Importing data to 4D dataset");
		
		for(int i=0;i<volumes.length;i++) {
			fireProgressStateChanged(65 + (i * 30 /volumes.length));
			try {
				fileInfo = volumes[i].getFileInfo();
				volumes[i].exportData(0, length, buffer);
				destImage.importData(length * i, buffer, true);
				for(int k=0;k<fileInfo.length;k++) {
					destImage.setFileInfo((FileInfoBase) (fileInfo[k].clone()), slice);
					
					for (int m = 0; m< 3; m++) {
			            resols[m] = fileInfo[k].getResolutions()[m];
			            units[m] = fileInfo[k].getUnitsOfMeasure()[m];
			            startLocs[m] = fileInfo[k].getOrigin(m);
			        }
					resols[3] = 0;
			        units[3] = FileInfoBase.MILLIMETERS;
			        startLocs[3] = 0;
			        
			        destImage.getFileInfo(slice).setResolutions(resols);
	                destImage.getFileInfo(slice).setUnitsOfMeasure(units);
	                destImage.getFileInfo(slice).setExtents(extents);
	                destImage.getFileInfo(slice).setOrigin(startLocs);

					slice++;
				}
				volumes[i].disposeLocal();
				volumes[i] = null;
			}catch(Exception e) {
				e.printStackTrace();
				return false;
			}
		}
		
		*/
		
		long endTime = System.currentTimeMillis();
        long diffTime = endTime - begTime;
        float seconds = ((float) diffTime) / 1000;
        
		fireProgressStateChanged(95);
		System.out.println("** Algorithm took " + seconds + " seconds \n");
		
		return true;
	}

	
	/**
	 * get dest image
	 */
	public ModelImage getDestImage() {
		return destImage;
	}
	
	
	

}
