import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import javax.swing.JTextArea;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileIO;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileInfoPARREC;
import gov.nih.mipav.model.file.FilePARREC;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.Preferences;

/**
 * @author pandyan
 * 
 * This algorithm is responsible for the creation of a sorted path file, list file, and b-matrix file
 * 
 * References: This algorithm was developed in concert with Okan Irfanoglu, Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 *
 */
public class PlugInAlgorithmDTIParRecSortingProcess extends AlgorithmBase {
	
	
	/** file name **/
	private String fileName;
	
	/** file dir **/
	private String fileDir;
	
	/** path to gradient file (only necessary for version 4 of Par/Rec) **/
	private String gradientFilePath;
	
	/** FileParRec handle **/
	private FilePARREC fileParRec;
	
	/** ModelImage of 4D dataset **/
	private ModelImage image4D;
	
	/** handle to test area **/
	private JTextArea outputTextArea;
	
	/** flag indicating if method was successful.* */
    private boolean success = true;
    
    /** This is the name of the study.* */
    private String studyName;
    
    /** x dim **/
    private String originalColumnsString;
    
    /** y dim **/
    private String originalRowsString;
    
    /** z dim **/
    private String numSlicesString;
    
    /** t dim **/
    private String nimString;
    
    /** phase encoding...hard coded to vertical **/
    private String phaseEncodingString = "vertical";
    
    /** horizontal FOV **/
    private String horizontalFOVString;
    
    /** vertical FOV **/
    private String verticalFOVString;
    
    /** raw image format...hardcoded to float since we will be outputting slices in float format **/
    private String rawImageFormatString = "float";
    
    /** List file name **/
    private String listFileName;
    
    /** B Matrix file name **/
    private String bmatrixFileName;
    
    /** Path File name **/
    private String pathFileName;
    
    /** vol parameters **/
    private HashMap<String,String> volParameters;

	
    /** slice gap String **/
    private String sliceGapString;
    
    /** sliec thickness **/
    private String sliceThicknessString;
    
    /** image orientation **/
    private String imageOrientationString;
    
    /** path for imageSlicesDir **/
    private String imageSlicesDirPath;
    
    /** relative path for slices dir for outputting to path file **/
    private String relativeImageSlicesDirPath;
    
    /** arraylist of unsorted path strings to image slices **/
    private ArrayList<String> unsortedPathsArrayList = new ArrayList<String>();
    
    /** xDIm of 4d image **/
    private int xDim;
    
    /** yDim of 4d image **/
    private int yDim;
    
    /** zDim of 4d image **/
    private int zDim;
    
    /** tDim of 4d image **/
    private int tDim;
    
    /**Philips puts in one volume as the average of all the DWIs. This
	volume will have a non-zero B value and 0 in the gradients
	Once we find this volume, exclude it....
    this int represents which volume index to exclude**/
    private int avgVolIndex = -1;
    
    /** boolean if proc dir is created **/
    private boolean isProcDirCreated;
    
    /** boolean identifying if datset is older than version 4.1 **/
    private boolean isOldVersion = false;
    
    /** this counter is used for version 4 of Par/Rec...nim is calculated from gradient file **/
    private int nimCounter = 0;
    
    /** arraylist of gradients for version 4 of Par/Rec **/
    private  ArrayList<String[]> oldVersionGradientsAL;
    
    /** file info Par/Rec **/
	private FileInfoPARREC fileInfoPR;
    
	
	
	/** constructor
	 * 
	 */
	public PlugInAlgorithmDTIParRecSortingProcess(String fileName, String fileDir, String gradientFilePath, JTextArea outputTextArea) {
		this.fileName = fileName;
		this.studyName = fileName.substring(0, fileName.indexOf("."));
		this.fileDir = fileDir;
		this.gradientFilePath = gradientFilePath;
		this.listFileName = studyName + ".list";
		this.bmatrixFileName = studyName + ".BMTXT";
		this.pathFileName = studyName + ".path";
		this.outputTextArea = outputTextArea;
	}
	

	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		long begTime = System.currentTimeMillis();

        Preferences.debug("** Beginning Algorithm v1.0\n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("** Beginning Algorithm v1.0\n");
        }
		fileParRec = new FilePARREC(fileName, fileDir + File.separator);

		

		//read in 4d par/rec data
		try {
			image4D = fileParRec.readImage(false);
		}catch (Exception e) {
			e.printStackTrace();
			finalize();
            setCompleted(true);
            return;
		}
		

		
		//obtain list file data
		success = obtainListFileData();
		if (success == false) {
            finalize();
            setCompleted(true);
            return;
        }
		
		
		//create proc dir
        Preferences.debug("* Creating proc dir and imageSlices dir \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("* Creating proc dir and imageSlices dir \n");
        }
        success = createProcDirAndImageSlicesDir();
        if (success == false) {
            finalize();
            setCompleted(true);
            return;
        }
        isProcDirCreated = true;
        
        

        // create list file
        Preferences.debug("* Creating list file \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("* Creating list file \n");
        }
        success = createListFile();
        if (success == false) {
            finalize();
            setCompleted(true);
            return;
        }
        
        

        //create b matrix file
        Preferences.debug("* Creating b-matrix file \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("* Creating b-matrix file \n");
        }
        success = createBMatrixFile();
        if (success == false) {
            finalize();
            setCompleted(true);
            return;
        }
        
        
        
        //write out image slices
        Preferences.debug("* Writing out image slices", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("* Writing out image slices");
        }
        success = writeImageSlices();
        if (success == false) {
            finalize();
            setCompleted(true);
            return;
        }
        
        
        
        //create path file
        Preferences.debug("\n* Creating path file \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("\n* Creating path file \n");
        }
        success = createPathFile();
        if (success == false) {
            finalize();
            setCompleted(true);
            return;
        }
        
        
        //ending algorithm
        Preferences.debug("** Ending Algorithm \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("** Ending Algorithm \n");
        }
        finalize();
        long endTime = System.currentTimeMillis();
        long diffTime = endTime - begTime;
        float seconds = ((float) diffTime) / 1000;
        Preferences.debug("** Algorithm took " + seconds + " seconds \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("** Algorithm took " + seconds + " seconds \n");
        }
        
        
        image4D.disposeLocal();
    	image4D = null;

        setCompleted(true);
	}
	
	
	
	
	
	
	
	
	
	/**
	 * This method obtains list file data mostly from the 1st slice
	 * 
	 * @return
	 */
	public boolean obtainListFileData() {
		try{

			fileInfoPR = (FileInfoPARREC)image4D.getFileInfo(0);

			int extents[] = image4D.getExtents();
			originalColumnsString = String.valueOf(extents[0]);
			originalRowsString = String.valueOf(extents[1]);
			numSlicesString = String.valueOf(extents[2]);
			volParameters = fileInfoPR.getVolParameters();
			String fovString = (String)volParameters.get("scn_fov");
			String[] fovs = fovString.trim().split("\\s+");
			horizontalFOVString = fovs[0];
			verticalFOVString = fovs[2];
			//slices = fileInfoPR.getSlices();
			//get some list file info from 1st slice 
			FileInfoPARREC firstFileInfo = (FileInfoPARREC)image4D.getFileInfo(0);
			String slice = firstFileInfo.getSliceInfo();
	        String[] values = slice.split("\\s+");
	        //slice gap
	        sliceGapString = values[23];
			//slice thickness
	        sliceThicknessString = values[22];
	        //slice orientation
	        int sliceOrientation = (Integer.valueOf(values[25])).intValue();
	        if(sliceOrientation == 1) {
	        	imageOrientationString = "axial";
	        }else if(sliceOrientation == 2) {
	        	imageOrientationString = "sagittal";
	        }else if(sliceOrientation == 3) {
	        	imageOrientationString = "coronal";
	        }
	        
	        //Philips puts in one volume as the average of all the DWIs. This
			//volume will have a non-zero B value and 0 in the gradients
			//Once we find this volume, exclude
	        avgVolIndex = -1;
	        
	        for(int i = 0,vol=0; i < (extents[2] * extents[3]);i=i+extents[2],vol++) {
				FileInfoPARREC fileInfoPR = (FileInfoPARREC)image4D.getFileInfo(i);
	            slice = fileInfoPR.getSliceInfo();
				values = slice.split("\\s+");
				float bValue = (Float.valueOf(values[33])).floatValue();
				float grad1 = (Float.valueOf(values[45])).floatValue();
				float grad2 = (Float.valueOf(values[46])).floatValue();
				float grad3 = (Float.valueOf(values[47])).floatValue();
				
				if(bValue != 0 && grad1 == 0 && grad2 == 0 && grad3 == 0) {
					avgVolIndex = vol;	
				}
			}
			if(avgVolIndex != -1) {
				nimString = String.valueOf(extents[3]-1);
			}else {
				nimString = String.valueOf(extents[3]);
			}    
		} catch(ArrayIndexOutOfBoundsException e) {
			//this means that we the Par/Rec version is an older one
			//so first check if gradient file was supplied...if so, get nimString
			//by the number of lines in that file....if no gradient file was supplied...exit
			//and complain!  
			//also...might as well just read the gradient file at this point
			if(gradientFilePath == null) {
				Preferences.debug("! ERROR: A valid gradient file must be supplied for this dataset....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
				outputTextArea.append("! ERROR: A valid gradient file must be supplied for this dataset....exiting algorithm \n");
				return false;
			}else {
				boolean success = readGradientFile();
				if(success == false) {
		            Preferences.debug("! ERROR: Error in reading gradient file....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

		            if (outputTextArea != null) {
		                outputTextArea.append("! ERROR: Error in reading gradient file....exiting algorithm \n");
		            }
					return false;
				}
				else {

					isOldVersion = true;
					//Philips puts in one volume as the average of all the DWIs. This
					//volume will have a non-zero B value and 0 in the gradients
					//Once we find this volume, exclude
			        avgVolIndex = -1;
			        int extents[] = image4D.getExtents();
			        int k = 0;
			        for(int i = 0,vol=0; i < (extents[2] * extents[3]);i=i+extents[2],vol++) {
						FileInfoPARREC fileInfoPR = (FileInfoPARREC)image4D.getFileInfo(i);
			            String slice = fileInfoPR.getSliceInfo();
						String[] values = slice.split("\\s+");
						float bValue = (Float.valueOf(values[33])).floatValue();
						if(bValue != 0) {
							String[] gradStrings = oldVersionGradientsAL.get(k);
							float grad1 = (Float.valueOf(gradStrings[0])).floatValue();
							float grad2 = (Float.valueOf(gradStrings[1])).floatValue();
							float grad3 = (Float.valueOf(gradStrings[2])).floatValue();
							if(grad1 == 0 && grad2 == 0 && grad3 == 0) {
								avgVolIndex = vol;	
							}
							k++;
						}
						
						
					}
					if(avgVolIndex != -1) {
						nimString = String.valueOf(extents[3]-1);
					}else {
						nimString = String.valueOf(extents[3]);
					}   
					
				}
				
			}
			
			
		}
		catch(Exception e) {
			Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Obtaining list file data falied....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: Obtaining list file data failed....exiting algorithm \n");
            }
			return false;
		}
		
		return true;
	}
	
	
	
	
	public boolean readGradientFile() {

        try {
            String str;
            FileInputStream fis = new FileInputStream(gradientFilePath);
            BufferedReader d = new BufferedReader(new InputStreamReader(fis));
            oldVersionGradientsAL = new ArrayList<String[]>();
            while ((str = d.readLine()) != null) {
                str = str.trim();
                String[] arr = str.split("\\s+");
                oldVersionGradientsAL.add(arr);
                nimCounter++;
            }
            fis.close();
        }
        catch(Exception e) {
        	return false;
        }
        
        return true;
	}
	
	
	
	/**
     * this method creates the proc dir in which the list file, path file, and b-matrix file go.
     *
     * @return  boolean success
     */
    public boolean createProcDirAndImageSlicesDir() {

        // create parallel proc dir to study path that will hold list file, path file, and b matrix file
        File procDir = new File(fileDir + "_proc");

        if (!procDir.isDirectory()) {
            boolean success = new File(fileDir + "_proc").mkdir();

            if (!success) {
                Preferences.debug("! ERROR: Creation of proc directory failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

                if (outputTextArea != null) {
                    outputTextArea.append("! ERROR: Creation of proc directory failed....exiting algorithm \n");
                }
                return false;
            }
        } else {

            // we should delete all the files in the procDir if there are any
            File[] listFiles = procDir.listFiles();

            if (listFiles.length > 0) {

                for (int i = 0; i < listFiles.length; i++) {
                    String name = listFiles[i].getName();

                    if (name.equals(studyName + ".path") || name.equals(studyName + ".list") ||
                            name.equals(studyName + ".BMTXT")) {
                        listFiles[i].delete();
                    }
                }
            }
        }
        
        Preferences.debug(" - proc dir created : " + fileDir + "_proc \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append(" - proc dir created : " + fileDir + "_proc \n");
        }
        
        //create image slices dir
        imageSlicesDirPath = fileDir + "_proc" + File.separator + studyName + "_slices";
        relativeImageSlicesDirPath = "./" + studyName + "_slices/";
        File imageSlicesDir = new File(imageSlicesDirPath);
        if(!imageSlicesDir.isDirectory()) {
        	boolean success = imageSlicesDir.mkdir();
        	 if (!success) {
                 Preferences.debug("! ERROR: Creation of image slices directory failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

                 if (outputTextArea != null) {
                     outputTextArea.append("! ERROR: Creation of image slices directory failed....exiting algorithm \n");
                 }
                 return false;
             }
        }else {
        	//we should delete all the fils under the imageSlicesDir
            File[] listFiles = imageSlicesDir.listFiles();
            if (listFiles.length > 0) {
                for (int i = 0; i < listFiles.length; i++) {
                	listFiles[i].delete();
                }
            }
        }
        Preferences.debug(" - imageSlices dir created : " + fileDir + File.separator + "imageSlices \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append(" - imageSlices dir created : " + fileDir + File.separator + "imageSlices \n");
        }


        return true;
    }
    
    
    
    /**
     * This method creates the list file.
     *
     * @return  boolean success
     */
    public boolean createListFile() {
    	
    	try {
            File listFile = new File(fileDir + "_proc" + File.separator + listFileName);
            FileOutputStream outputStream = new FileOutputStream(listFile);
            PrintStream printStream = new PrintStream(outputStream);

            printStream.println("<!-- DTI initialization file -->");
            printStream.println("<!-- do not remove the above comment line -->");
            printStream.println();
            printStream.println("<!-- NUMBER OF COLUMNS -->");
            printStream.println("<original_columns>" + originalColumnsString + "</original_columns>");
            printStream.println();
            printStream.println("<!-- NUMBER OF ROWS -->");
            printStream.println("<original_rows>" + originalRowsString + "</original_rows>");
            printStream.println();
            printStream.println("<!-- NUMBER OF SLICES -->");
            printStream.println("<slice>" + numSlicesString + "</slice>");
            printStream.println();
            printStream.println("<!-- NUMBER OF BMATRICES -->");
            printStream.println("<nim>" + nimString + "</nim>");
            printStream.println();
            printStream.println("<!-- ORIENTATION OF PHASE ENCODING (vertical, horizontal) -->");
            printStream.println("<phase_encode_direction>" + phaseEncodingString + "</phase_encode_direction>");
            printStream.println();
            printStream.println("<!-- HORIZONTAL FIELD OF VIEW (in mm) -->");
            printStream.println("<x_field_of_view>" + horizontalFOVString + "</x_field_of_view>");
            printStream.println();
            printStream.println("<!-- VERTICAL FIELD OF VIEW (in mm) -->");
            printStream.println("<y_field_of_view>" + verticalFOVString + "</y_field_of_view>");
            printStream.println();
            printStream.println("<!-- FORMAT OF RAW IMAGES (integer, float, dicom, dummy) -->");
            printStream.println("<rawimageformat>" + rawImageFormatString + "</rawimageformat>");
            printStream.println();
            printStream.println("<!-- NAME OF BMATRIX FILE -->");
            printStream.println("<bmatrixfile>" + bmatrixFileName + "</bmatrixfile>");
            printStream.println();
            printStream.println("<!-- GAP BETWEEN SLICES (in mm. Write 0 for contiguous slices) -->");
            printStream.println("<slice_gap>" + sliceGapString + "</slice_gap>");
            printStream.println();
            printStream.println("<!-- SLICE THICKNESS (in mm) -->");
            printStream.println("<slice_thickness>" + sliceThicknessString + "</slice_thickness>");
            printStream.println();
            printStream.println("<!-- IMAGE PLANE (axial,coronal,sagittal) -->");
            printStream.println("<image_plane>" + imageOrientationString + "</image_plane>");
            printStream.println();
            printStream.println("<!-- NAME OF FILE CONTAINING PATH OF RAW IMAGES -->");
            printStream.println("<raw_image_path_filename>" + pathFileName + "</raw_image_path_filename>");

            outputStream.close();

    	}catch(Exception e) {
    		Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Creation of list file failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: Creation of list file failed....exiting algorithm \n");
            }
            return false;
    	}
    	
    	Preferences.debug(" - list file created : " + fileDir + "_proc" + File.separator + listFileName + " \n", Preferences.DEBUG_ALGORITHM);

    	if (outputTextArea != null) {
    		outputTextArea.append(" - list file created : " + fileDir + "_proc" + File.separator + listFileName + " \n");
    	}
    	return true;
    }
    
    
    
    /**
     * This method creates the B-Matrix file
     * 
     * @return
     */
    public boolean createBMatrixFile() {
    	int extents[] = image4D.getExtents();
    	String slice;
    	String[] values;
    	
    	try {
            StringBuffer sb;
            int padLength;
            File bMatrixFile = new File(fileDir + "_proc" + File.separator + bmatrixFileName);
            FileOutputStream outputStream = new FileOutputStream(bMatrixFile);
            PrintStream printStream = new PrintStream(outputStream);
            float x = 0;
            float y = 0;
            float z = 0;
            float b;
            int k = 0;
            for(int i = 0,vol=0; i < (extents[2] * extents[3]);i=i+extents[2],vol++) {
				FileInfoPARREC fileInfoPR = (FileInfoPARREC)image4D.getFileInfo(i);
	    		if(vol!=avgVolIndex) {
					slice = fileInfoPR.getSliceInfo();
					values = slice.split("\\s+");
					b = (Float.valueOf(values[33])).floatValue();
					if(!isOldVersion) {
						x = (Float.valueOf(values[45])).floatValue();
						y = (Float.valueOf(values[46])).floatValue();
						z = (Float.valueOf(values[47])).floatValue();
					}else {
						if(b != 0) {
							String[] grads = oldVersionGradientsAL.get(k);
							x = Float.valueOf(grads[0]).floatValue();
							y = Float.valueOf(grads[1]).floatValue();
							z = Float.valueOf(grads[2]).floatValue();
							k++;
						}
						
					}

					// For Par/Rec datasets, Philips uses another coordinate frame
					// formula for bmtxt values is :
		            // bzz 2bxz -2byz bxx -2bxy byy

					float _bxx = b * x * x;
	
	                if (Math.abs(_bxx) == 0) {
	                    _bxx = Math.abs(_bxx);
	                }
	
	                float _2bxy = -2.0f * b * x * y;
	
	                if (Math.abs(_2bxy) == 0) {
	                   _2bxy = Math.abs(_2bxy);
	                }
	
	                float _2bxz = 2.0f * b * x * z;
	
	                if (Math.abs(_2bxz) == 0) {
	                    _2bxz = Math.abs(_2bxz);
	                }
	
	                float _byy = b * y * y;
	
	                if (Math.abs(_byy) == 0) {
	                    _byy = Math.abs(_byy);
	                }
	
	                float _2byz = -2.0f * b * y * z;
	
	                if (Math.abs(_2byz) == 0) {
	                    _2byz = Math.abs(_2byz);
	                }
	
	                float _bzz = b * z * z;
	
	                if (Math.abs(_bzz) == 0) {
	                    _bzz = Math.abs(_bzz);
	                }
	
	
	                // following is for 1.4 compliant
	                // otherwise, it would be : printStream.printf("%16f", b*x*x);
	                
	                
	                String _bzz_string = String.valueOf(_bzz);
	                int _bzz_stringLength = _bzz_string.length();
	                sb = new StringBuffer(16);
	                padLength = 16 - _bzz_stringLength;
	                for (int j = 0; j < padLength; j++) {
	                    sb.insert(j, " ");
	                }
	                sb.insert(padLength, _bzz_string);
	                printStream.print(sb.toString());
	                
	                
	                
	                String _2bxz_string = String.valueOf(_2bxz);
	                int _2bxz_stringLength = _2bxz_string.length();
	                sb = new StringBuffer(16);
	                padLength = 16 - _2bxz_stringLength;
	                for (int j = 0; j < padLength; j++) {
	                    sb.insert(j, " ");
	                }
	                sb.insert(padLength, _2bxz_string);
	                printStream.print(sb.toString());
	                
	                
	                
	                String _2byz_string = String.valueOf(_2byz);
	                int _2byz_stringLength = _2byz_string.length();
	                sb = new StringBuffer(16);
	                padLength = 16 - _2byz_stringLength;
	                for (int j = 0; j < padLength; j++) {
	                    sb.insert(j, " ");
	                }
	                sb.insert(padLength, _2byz_string);
	                printStream.print(sb.toString());
	                

	                
	                String _bxx_string = String.valueOf(_bxx);
	                int _bxx_stringLength = _bxx_string.length();
	                sb = new StringBuffer(16);
	                padLength = 16 - _bxx_stringLength;
	                for (int j = 0; j < padLength; j++) {
	                    sb.insert(j, " ");
	                }
	                sb.insert(padLength, _bxx_string);
	                printStream.print(sb.toString());
	
	
	                
	                
	                
	                
	                
	                String _2bxy_string = String.valueOf(_2bxy);
	                int _2bxy_stringLength = _2bxy_string.length();
	                sb = new StringBuffer(16);
	                padLength = 16 - _2bxy_stringLength;
	                for (int j = 0; j < padLength; j++) {
	                    sb.insert(j, " ");
	                }
	                sb.insert(padLength, _2bxy_string);
	                printStream.print(sb.toString());
	
	
	               
	
	
	                String _byy_string = String.valueOf(_byy);
	                int _byy_stringLength = _byy_string.length();
	                sb = new StringBuffer(16);
	                padLength = 16 - _byy_stringLength;
	                for (int j = 0; j < padLength; j++) {
	                    sb.insert(j, " ");
	                }
	                sb.insert(padLength, _byy_string);
	                printStream.print(sb.toString());

	                printStream.println();
	               
	    		}
	    	}
	    	outputStream.close();
    	}catch(Exception e) {
    		Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Creation of bmatrix file failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: Creation of bmatrix file failed....exiting algorithm \n");
            }
    		return false;
    	}
    	
    	
    	Preferences.debug(" - b-matrix file created : " + fileDir + "_proc" + File.separator + bmatrixFileName + " \n", Preferences.DEBUG_ALGORITHM);

    	if (outputTextArea != null) {
    		outputTextArea.append(" - b-matrix file created : " + fileDir + "_proc" + File.separator + bmatrixFileName + " \n");
    	}
    	
    	return true;
    }
    
    
    
    
    
    
    
    /**
     * This method creates the path file
     * 
     * @return
     */
    public boolean createPathFile() {
    	try {
            File listFile = new File(fileDir + "_proc" + File.separator + pathFileName);
            FileOutputStream outputStream = new FileOutputStream(listFile);
            PrintStream printStream = new PrintStream(outputStream);
            
            for(int i=0;i<zDim;i++) {   	
            	for(int j=i;j<unsortedPathsArrayList.size();j=j+zDim) {
            		printStream.println(((String)unsortedPathsArrayList.get(j)) + ".raw");
            	}
            }
            outputStream.close();
            
    	}catch(Exception e) {
    		Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Creation of path file failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: Creation of path file failed....exiting algorithm \n");
            }
    		return false;
    	}
    	Preferences.debug(" - path file created : " + fileDir + "_proc" + File.separator + pathFileName+ " \n", Preferences.DEBUG_ALGORITHM);

    	if (outputTextArea != null) {
    		outputTextArea.append(" - path file created : " + fileDir + "_proc" + File.separator + pathFileName + " \n");
    	}
    	
    	return true;
    }

    
    /**
     * This method writes out the image slices as raw float
     * 
     * @return
     */
    public boolean writeImageSlices() {
    	//When writing out the image slices, the floating point value should be determined by:
    	//# === PIXEL VALUES =============================================================
    	//	#  PV = pixel value in REC file, FP = floating point value, DV = displayed value on console
    	//	#  RS = rescale slope,           RI = rescale intercept,    SS = scale slope
    	//	#  DV = PV * RS + RI             FP = DV / (RS * SS)
    	
    	
    	//get some of the values for eqautions above from 1st slice 
    	FileInfoPARREC firstFileInfo = (FileInfoPARREC)image4D.getFileInfo(0);
		String slice = firstFileInfo.getSliceInfo();
        String[] values = slice.split("\\s+");
    	float RS = Float.valueOf(values[12]).floatValue();
    	float RI = Float.valueOf(values[11]).floatValue();
    	float SS = Float.valueOf(values[13]).floatValue();
    	float PV;
    	float DV;
    	float FP;
    	xDim = image4D.getExtents()[0];
    	yDim = image4D.getExtents()[1];
    	zDim = image4D.getExtents()[2];
    	tDim = image4D.getExtents()[3];
    	int volLength = xDim * yDim * zDim;
    	int sliceLength = xDim * yDim;
    	int[] sliceExtents = new int[2];
    	sliceExtents[0] = xDim;
		sliceExtents[1] = yDim;
        float[] volBuffer = new float[volLength];
        float[] sliceBuffer = new float[sliceLength];
        FileInfoImageXML fileInfoXML;
        String dir = fileDir + "_proc" + File.separator + studyName + "_slices" + File.separator;
		String filename;
		int[] sliceUnits = new int[2];
		sliceUnits[0] = Unit.MILLIMETERS.getLegacyNum();
		sliceUnits[1] = Unit.MILLIMETERS.getLegacyNum();
		float[] sliceResols = new float[2];
		float xRes = Float.valueOf(horizontalFOVString)/xDim;
		float yRes = Float.valueOf(verticalFOVString)/yDim;
		sliceResols[0] = xRes;
		sliceResols[1] = yRes;
		int type = ModelStorageBase.FLOAT;
		FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
		ModelImage sliceImage = new ModelImage(ModelStorageBase.FLOAT, sliceExtents, studyName);
		
		int image4DLength = xDim * yDim * zDim * tDim;
		float[] image4DBuffer = new float[image4DLength];
		try {
			image4D.exportData(0, image4DLength, image4DBuffer);
		}catch(Exception e) {
			e.printStackTrace();
			return false;
		}
		
		int volStart = 0;
		int filenameVol = 0;
    	for(int volIndex=0;volIndex<tDim;volIndex++) {
    		
    		
    		if(volIndex != avgVolIndex) {
    	        try {
    	        	//extract the volume buffer from the 4D buffer
    	        	for(int i=volStart,j=0;i<volLength;i++,j++) {
    	        		volBuffer[j] = image4DBuffer[i];
    	        	}
    	        	int index = 0;
    	        	for(int sliceIndex=0;sliceIndex<zDim;sliceIndex++) {
    	        		for(int i=0;i<sliceBuffer.length;i++) {
    	        			//do calculations to rescale
    	        			PV = volBuffer[index];
    	        			DV = PV * RS + RI;
    	        			FP = DV / (RS * SS);
    	        			sliceBuffer[i] = FP;
    	        			index++;
    	        		}
    	        		//write out slice...we will write out starting with 1...so add 1
    	        		int volInt = filenameVol + 1;
    	        		String volString;
    	        		if(volInt < 10) {
    	        			volString = "0" + volInt;
    	        		}else {
    	        			volString = String.valueOf(volInt);
    	        		}
    	        		filename = "vol" + volString + "slice" + (sliceIndex + 1);
    	        		String pathString = relativeImageSlicesDirPath + filename;
    	        		unsortedPathsArrayList.add(pathString);
    	        		
    	        		fileInfoXML = new FileInfoImageXML(filename, dir, FileUtility.RAW);
    	        		fileInfoXML.setDataType(type);
    	        		fileInfoXML.setExtents(sliceExtents);
    	        		fileInfoXML.setUnitsOfMeasure(sliceUnits);
    	        		fileInfoXML.setResolutions(sliceResols);
    	        		fileInfoXML.setEndianess(FileBase.LITTLE_ENDIAN);
    	        		fileInfoXML.setOffset(0);

    	                sliceImage.importData(0, sliceBuffer, false);
    	                sliceImage.setFileInfo(fileInfoXML,0);
    	                
    	                FileWriteOptions opts = new FileWriteOptions(true);
	                    opts.setFileType(FileUtility.RAW);
	                    opts.setFileDirectory(dir);
	                    opts.setFileName(filename);
	                    opts.setBeginSlice(0);
	                    opts.setEndSlice(0);
	                    opts.setOptionsSet(true);
	                    fileIO.writeImage(sliceImage, opts);
    	        	}
    	        	filenameVol++;
    	        }
    	        catch (Exception e) {
    	        	Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
    	            Preferences.debug("! ERROR: Writing of image slices failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

    	            if (outputTextArea != null) {
    	                outputTextArea.append("! ERROR: " + e.toString() + "\n");
    	                outputTextArea.append("! ERROR: Writing of image slices failed....exiting algorithm \n");
    	            }
    	            return false;
    	        }
    		}

    		volStart = volStart + volLength;
    		Preferences.debug(".", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append(".");
            }
    	}
    	sliceImage.disposeLocal();
    	sliceImage = null;

    	return true;
    }
    
    
    /**
     * this method cleans up the proc dir if success is false and it sets the lists to null.
     */
    public void finalize() {

        // delete any of the files that were created if success is false
        if (success == false) {

            if (isProcDirCreated) {
                Preferences.debug("! Deleting .list, .path, and .BMTXT (if created)  from proc dir \n", Preferences.DEBUG_ALGORITHM);

                if (outputTextArea != null) {
                    outputTextArea.append("! Deleting .list, .path, and .BMTXT (if created)  from proc dir \n");
                }


                File procDir = new File(fileDir + "_proc");
                File[] files = procDir.listFiles();

                if (files.length > 0) {

                    for (int i = 0; i < files.length; i++) {
                        String name = files[i].getName();

                        if (name.equals(pathFileName) || name.equals(listFileName) ||
                                name.equals(bmatrixFileName)) {
                            files[i].delete();
                        }
                    }
                }

                procDir.delete();
            }
        }

        success = true;
    } 
    
    
    
}
