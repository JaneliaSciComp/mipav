package gov.nih.mipav.model.algorithms.DiffusionTensorImaging;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import javax.swing.JTextArea;
import javax.swing.JTextField;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR35D;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileIO;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileInfoPARREC;
import gov.nih.mipav.model.file.FilePARREC;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJProgressBar;

public class AlgorithmDTICreateListFile extends AlgorithmBase implements AlgorithmInterface{

	
	
	/** This is the dir name of the study.* */
    private String studyName;

    /** This is the full path of the study.* */
    private String studyPath;
    
    /** par rec file name **/
	private String prFileName;
	
	/** par rec file dir **/
	private String prFileDir;
    
    /** This is the full path to the gradient file.* */
    private String gradientFilePath;
	
    /** This is the full path to the b matrix file.* */
    private String bmtxtFilePath;
    
    /** boolean if dataset is interelaved **/
    private boolean isInterleaved;
    
    /** TextArea of main dialog for text output.* */
    private JTextArea outputTextArea;
    
    /** handle to dwi data textfield of main dialog **/
    private JTextField dwiPathTextField;
    
    /** This is an ordered map of series number and seriesFileInfoTreeSet.* */
    private TreeMap<Integer,TreeSet<String[]>> seriesFileInfoTreeMap; 
    
    /** This is an ordered list of files per series number.* */
    private TreeSet<String[]> seriesFileInfoTreeSet;

    /** boolean indicating if we are dealing with dicom or par/rec **/
    private boolean isDICOM;
    
    /** List file name **/
    private String listFileName;
    
    /** B Matrix file name **/
    private String bmatrixFileName;
    
    /** Path File name **/
    private String pathFileName;
    
    /** flag indicating if method was successful.* */
    private boolean success = true;
    
    /** total image slices.* */
    private int totalImageSlices = 0;

    /** total num of volumes in study.* */
    private int totalNumVolumes = 0;
    
    /** boolean telling if dataset is eDTI.* */
    private boolean isEDTI = false;

    /** boolean telling if datset is GE.* */
    private boolean isGE = false;

    /** boolean telling if datset is SIEMENS.* */
    private boolean isSiemens = false;
    
    /** first fileinfo dicom...used for list file info.* */
    private FileInfoDicom firstFileInfoDicom;
    
    /** int telling software version if datset is GE.* */
    private int geSoftwareVersion = -1;
    
    /** This is the image filter needed to select the correct dicom images.* */
    private ViewImageFileFilter imageFilter;
    
    /** Array that holds the dicom tag info* */
    private String[] dicomInfo;
    
    /** boolean if proc dir was created.* */
    private boolean isProcDirCreated = false;
    
    /** this is the number of image slices per volume.* */
    private int numSlicesPerVolume;
    
    /** this is the number of gradient quantities that is obtained by reading first line of gradient file.* */
    private int nim;
    
    /** this is the gradient matrix that is populated when reading the gradient file.* */
    private float[][] direction;
    
    /** this is an array of b-values that for each volume.* */
    private ArrayList<Float> bValuesArrayList = new ArrayList<Float>();
    
    /** FileParRec handle **/
	private FilePARREC fileParRec;
	
	/** file info Par/Rec **/
	private FileInfoPARREC fileInfoPR;
	
	/** ModelImage of 4D par/rec dataset **/
	private ModelImage image4D;
	
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
    
    /** slice gap String **/
    private String sliceGapString;
    
    /** sliec thickness **/
    private String sliceThicknessString;
    
    /** image orientation **/
    private String imageOrientationString;
    
    /** vol parameters **/
    private HashMap<String, String> volParameters; 

    
    /**Philips puts in one volume as the average of all the DWIs. This
	volume will have a non-zero B value and 0 in the gradients
	Once we find this volume, exclude it....
    this int represents which volume index to exclude**/
    private int avgVolIndex = -1;
    
    /** boolean identifying if par/rec datset is older than version 4.1 **/
    private boolean isOldVersion = false;
    
    /** arraylist of gradients for version 4 of Par/Rec **/
    private  ArrayList<String[]> oldVersionGradientsAL;
    
    /** this counter is used for version 4 of Par/Rec...nim is calculated from gradient file **/
    private int nimCounter = 0;
    
    /** path for imageSlicesDir **/
    private String imageSlicesDirPath;
    
    /** relative path for slices dir for outputting to path file **/
    private String relativeImageSlicesDirPath;
    
    /** raw image format...hardcoded to float since we will be outputting par/rec slices in float format **/
    private String rawImageFormatString = "float";
    
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
    
    /** boolean for performing registration **/
    private boolean performRegistration;
    
    /** handle to AlgorithmRegOAR35D **/
    private AlgorithmRegOAR35D reg35;
    
    /** handle to AlgorithmRegOAR3D **/
    private AlgorithmRegOAR3D reg3;
    
    /** registration option parameters **/
    private int cost, DOF, interp, interp2, registerTo, maxIterations, numMinima;
    
    /** registration option parameters **/
    private float rotateBegin, rotateEnd, coarseRate, fineRate, rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;
    
    /** registration option parameters **/
    private boolean doGraph = false;
    
    /** registration option parameters **/
    private boolean doSubsample, doMultiThread, fastMode, maxOfMinResol;
    
    /** path to dicom B0 for registration purposes **/
    private String dicomB0VolumePath;
    
    /** model images **/
    private ModelImage refImage, matchImage, resultImage;
    
    /** file IO **/
    private FileIO fileIO;
    
    /** study name for registered data set **/
    private String registeredStudyName;
    
    /** handle to AlgorithmTransform **/
    private AlgorithmTransform transform;
    
    /** parameter for AlgorithmTransform **/
    private int xdimA;
    
    /** parameter for AlgorithmTransform **/
    private int ydimA;
    
    /** parameter for AlgorithmTransform **/
    private int zdimA;
    
    /** parameter for AlgorithmTransform **/
    private float xresA;
    
    /** parameter for AlgorithmTransform **/
    private float yresA;
    
    /** parameter for AlgorithmTransform **/
    private float zresA;
    
    /** registered B0 parent folder **/
    private String registeredDicomB0VolumePathParentFolder;
    
    
    /**
     * constructor for dicom
     *
     * @param  studyPath         
     * @param  studyName         
     * @param  gradientFilePath  
     * @param  bmtxtFilePath     
     * @param  outputTextArea   
     */
    public AlgorithmDTICreateListFile(String studyPath, String studyName, String gradientFilePath, String bmtxtFilePath, JTextArea outputTextArea, boolean isInterleaved, boolean performRegsitration) {
        this.studyPath = studyPath;
        this.studyName = studyName;
        this.gradientFilePath = gradientFilePath;
        this.bmtxtFilePath = bmtxtFilePath;
        this.outputTextArea = outputTextArea;
        this.isInterleaved = isInterleaved;
        this.listFileName = studyName + ".list";
		this.bmatrixFileName = studyName + ".BMTXT";
		this.pathFileName = studyName + ".path";
		this.performRegistration = performRegsitration;
        seriesFileInfoTreeMap = new TreeMap<Integer,TreeSet<String[]>>();
        isDICOM = true;
    }
    
    
    
    
    /**
     * constructor for dicom if registration is to be done first
     *
     * @param  studyPath         
     * @param  studyName         
     * @param  gradientFilePath  
     * @param  bmtxtFilePath     
     * @param  outputTextArea   
     */
    public AlgorithmDTICreateListFile(String studyPath, String studyName, String dicomB0VolumePath, String gradientFilePath,
    		String bmtxtFilePath, JTextArea outputTextArea, boolean isInterleaved, boolean performRegsitration, int cost,
    		int DOF, int interp, int interp2, float rotateBeginX, float rotateEndX, float coarseRateX, float fineRateX,
    		float rotateBeginY, float rotateEndY, float coarseRateY, float fineRateY, float rotateBeginZ, float rotateEndZ,
    		float coarseRateZ, float fineRateZ, boolean maxOfMinResol, boolean doSubsample, boolean doMultiThread,
    		boolean fastMode, int bracketBound, int maxIterations, int numMinima, JTextField dwiPathTextField) {
        this.studyPath = studyPath;
        this.studyName = studyName;
        this.dicomB0VolumePath = dicomB0VolumePath;
        this.gradientFilePath = gradientFilePath;
        this.bmtxtFilePath = bmtxtFilePath;
        this.outputTextArea = outputTextArea;
        this.isInterleaved = isInterleaved;
        this.listFileName = studyName + ".list";
		this.bmatrixFileName = studyName + ".BMTXT";
		this.pathFileName = studyName + ".path";
		this.performRegistration = performRegsitration;
		this.cost = cost;
		this.DOF = DOF;
		this.interp = interp;
		this.interp2 = interp2;
		this.rotateBeginX = rotateBeginX;
		this.rotateEndX = rotateEndX;
		this.coarseRateX = coarseRateX;
		this.fineRateX = fineRateX;
		this.rotateBeginY = rotateBeginY;
		this.rotateEndY = rotateEndY;
		this.coarseRateY = coarseRateY;
		this.fineRateY = fineRateY;
		this.rotateBeginZ = rotateBeginZ;
		this.rotateEndZ = rotateEndZ;
		this.coarseRateZ = coarseRateZ;
		this.fineRateZ = fineRateZ;
		this.maxOfMinResol = maxOfMinResol;
		this.doSubsample = doSubsample;
		this.doMultiThread = doMultiThread;
		this.fastMode = fastMode;
		this.maxIterations = maxIterations;
		this.numMinima = numMinima;
		this.dwiPathTextField = dwiPathTextField;
		registeredStudyName = studyName + "_registered";
		fileIO = new FileIO();
        seriesFileInfoTreeMap = new TreeMap<Integer,TreeSet<String[]>>();
        isDICOM = true;
    }
    
    
    /** constructor for par/rec
	 * 
	 */
	public AlgorithmDTICreateListFile(String fileName, String fileDir, String gradientFilePath, String bmtxtFilePath, JTextArea outputTextArea, boolean performRegsitration) {
		this.prFileName = fileName;
		this.studyName = fileName.substring(0, fileName.indexOf("."));
		this.prFileDir = fileDir;
		this.gradientFilePath = gradientFilePath;
		this.bmtxtFilePath = bmtxtFilePath;
		this.listFileName = studyName + ".list";
		this.bmatrixFileName = studyName + ".BMTXT";
		this.pathFileName = studyName + ".path";
		this.outputTextArea = outputTextArea;
		this.performRegistration = performRegsitration;
		isDICOM = false;
	}
	
	
	
	/** constructor for par/rec if registration is to be done first
	 * 
	 */
	public AlgorithmDTICreateListFile(String fileName, String fileDir, String gradientFilePath, String bmtxtFilePath, 
			JTextArea outputTextArea, boolean performRegsitration, int cost, int DOF, int interp, int interp2,
			int registerTo, float rotateBegin, float rotateEnd, float coarseRate, float fineRate, boolean doSubsample, 
			boolean doMultiThread, boolean fastMode, int bracketBound, int maxIterations, int numMinima,
			JTextField dwiPathTextField) {
		this.prFileName = fileName;
		this.studyName = fileName.substring(0, fileName.indexOf("."));
		this.prFileDir = fileDir;
		this.gradientFilePath = gradientFilePath;
		this.bmtxtFilePath = bmtxtFilePath;
		this.listFileName = studyName + ".list";
		this.bmatrixFileName = studyName + ".BMTXT";
		this.pathFileName = studyName + ".path";
		this.outputTextArea = outputTextArea;
		this.performRegistration = performRegsitration;
		this.cost = cost;
		this.DOF = DOF;
		this.interp = interp;
		this.interp2 = interp2;
		this.registerTo = registerTo;
		this.rotateBegin = rotateBegin;
		this.rotateEnd = rotateEnd;
		this.coarseRate = coarseRate;
		this.fineRate = fineRate;
		this.doSubsample = doSubsample;
		this.doMultiThread = doMultiThread;
		this.fastMode = fastMode;
		this.maxIterations = maxIterations;
		this.numMinima = numMinima;
		this.dwiPathTextField = dwiPathTextField;
		
		isDICOM = false;
	}
	
	
	
	
	
	
	
	
	
	
	public void runAlgorithm() {
		long begTime = System.currentTimeMillis();


        Preferences.debug("** Beginning Algorithm \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append("** Beginning Algorithm \n");
        }

        //System.out.println("** Beginning Algorithm \n");
        
        
		if(isDICOM) {
			success = true;
			if(performRegistration) {
				success = performRegistrationDICOM();
			}
			if(success) {
				runAlgorithmDICOM();
			}
		}else {
			success = true;
			if(performRegistration) {
				success = performRegistrationPARREC();
			}
			if(success) {
				runAlgorithmPARREC();
			}
		}
		
		long endTime = System.currentTimeMillis();
        long diffTime = endTime - begTime;
        float seconds = ((float) diffTime) / 1000;

        Preferences.debug("** Algorithm took " + seconds + " seconds \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append("** Algorithm took " + seconds + " seconds \n");
        }

        //System.out.println("** Algorithm took " + seconds + " seconds \n");
        
        if(refImage != null) {
        	refImage.disposeLocal();
        	refImage = null;
        }
        if(resultImage != null) {
        	resultImage.disposeLocal();
        	resultImage = null;
        }
        if(matchImage != null) {
        	matchImage.disposeLocal();
        	matchImage = null;
        }
		
		setCompleted(true);
	}
	
	/**
	 * regsitration of PAR REC DWI dataset
	 *
	 */
	public boolean performRegistrationPARREC() {
		 Preferences.debug("** Registering DWI dataset...\n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append("** Registering DWI dataset...\n");
        }

        //System.out.println("** Registering DWI dataset...\n");
        
		fileParRec = new FilePARREC(prFileName, prFileDir + File.separator);
		//read in 4d par/rec data
		try {
			image4D = fileParRec.readImage(false);
		}catch (Exception e) {
			e.printStackTrace();
			success = false;
			finalize();
            setCompleted(true);
            return false;
		}
		fileInfoPR = (FileInfoPARREC)image4D.getFileInfo(0);
		int extents[] = image4D.getExtents();
		String[] values;
		int bZeroIndex = -1;
		String slice;
		
		//find B0 volume index
		for (int i = 0,vol=0; i < (extents[2] * extents[3]);i=i+extents[2],vol++) {
            FileInfoPARREC fileInfoPR = (FileInfoPARREC)image4D.getFileInfo(i);
            slice = fileInfoPR.getSliceInfo();
            values = slice.split("\\s+");
			float bValue = (Float.valueOf(values[33])).floatValue();

			if(bValue == 0) {
				bZeroIndex = vol;
				break;
			}
        }
		
		
		
		//now we have the volume index to register other volumes to....call algorithm to register
		int refImageNum = bZeroIndex;
		
		reg35 = new AlgorithmRegOAR35D(image4D, cost, DOF, interp, interp2, registerTo, refImageNum, rotateBegin,
                rotateEnd, coarseRate, fineRate, doGraph, doSubsample, fastMode,
                maxIterations, numMinima);
		
		
		reg35.setRunningInSeparateThread(isRunningInSeparateThread());
		reg35.addListener(this);
		ViewJProgressBar progressBar = new ViewJProgressBar("", " ...", 0, 100, true);
        progressBar.setSeparateThread(isRunningInSeparateThread());
        reg35.addProgressChangeListener(progressBar);
        reg35.setProgressValues(0, 100);
		reg35.run();
		

		try {
			String[] splits = prFileName.split("\\.");
			String prFileNameRegistered = splits[0] + "_registered." + splits[1];

			FileWriteOptions opts = new FileWriteOptions(true);
	        opts.setFileType(FileUtility.PARREC);
	        opts.setFileDirectory(prFileDir + File.separator);
	        opts.setFileName(prFileNameRegistered);
	        opts.setBeginSlice(0);
	        opts.setEndSlice(extents[2]-1);
	        opts.setTimeSlice(0);
	        opts.setEndTime(extents[3]-1);
	        opts.setOptionsSet(true);
	        FileIO fileIO = new FileIO();
	        fileIO.writeImage(image4D, opts);
	        //now set prFileName to this registered image for continuation of algorithm
	        prFileName = prFileNameRegistered;
	        dwiPathTextField.setText(prFileDir + File.separator + prFileName);
	        studyName = studyName + "_registered";
	        listFileName = studyName + ".list";
			bmatrixFileName = studyName + ".BMTXT";
			pathFileName = studyName + ".path";
		}catch (Exception e) {
			e.printStackTrace();
        	Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Writing of registered image failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: Writing registered image failed....exiting algorithm \n");
            }

        }
		
		
		
		
		image4D.disposeLocal();
		image4D = null;
		
		Preferences.debug(" - Registration complete \n", Preferences.DEBUG_ALGORITHM);

	    if (outputTextArea != null) {
	    	outputTextArea.append(" - Registration complete \n");
	    }

	    //System.out.println(" - Registration complete \n");
		
		return true;
	}
	
	
	/**
	 * regsitration of DICOM DWI dataset
	 *
	 */
	public boolean performRegistrationDICOM() {
		Preferences.debug("** Registering DWI dataset", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append("** Registering DWI dataset");
        }

        System.out.println("** Registering DWI dataset");
		
		
		//load up reference B0 volume
		int index = dicomB0VolumePath.lastIndexOf(File.separator);
		String fileDir = dicomB0VolumePath.substring(0, index) + File.separator;
		String fileName = dicomB0VolumePath.substring(index+1, dicomB0VolumePath.length());
		refImage = fileIO.readImage(fileName, fileDir, true, null);
		
		//create parallel directory to studyDir for the studyDir_registered directory
		File registeredDir = new File(studyPath + "_registered");
		if (!registeredDir.isDirectory()) {
			 try {
				 success = registeredDir.mkdir();
			 }catch(Exception e) {
				 e.printStackTrace();
			 }
	         if (!success) {
	        	 Preferences.debug("! ERROR: Creation of registered study dir directory failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

	             if (outputTextArea != null) {
	            	 outputTextArea.append("! ERROR: Creation of registered study dir directory failed....exiting algorithm \n");
	             }
	             return false;
	         }
	     } else {
	         //we should delete everything in this dir first
	    	 deleteAllFiles(registeredDir);
	    	 try {
				 success = registeredDir.mkdir();
			 }catch(Exception e) {
				 e.printStackTrace();
			 }  
	     }
	    //now create all the subdirectories    
	    File studyPathDir = new File(studyPath);
	    createSubDirsInRegisteredDir(studyPathDir);
	    
	    
	    //ready to start registration

	    //ok....first thing we can do is save the B0 volume over since this is the reference volume
	    String dicomB0VolumePathParentFolder = dicomB0VolumePath.substring(0, dicomB0VolumePath.lastIndexOf(File.separator));
	    //replace the studyname with the registeredStudyName and then save here
	    registeredDicomB0VolumePathParentFolder = dicomB0VolumePathParentFolder.replace(studyName, registeredStudyName);
	    String fName = registeredDicomB0VolumePathParentFolder.substring(registeredDicomB0VolumePathParentFolder.lastIndexOf(File.separator)+1,registeredDicomB0VolumePathParentFolder.length());
	    int[] extents = refImage.getExtents();
	    FileWriteOptions opts = new FileWriteOptions(true);
        opts.setFileType(FileUtility.DICOM);
        opts.setFileDirectory(registeredDicomB0VolumePathParentFolder + File.separator);
        opts.setFileName(fName);
        opts.setBeginSlice(0);
        opts.setEndSlice(extents[2]-1);
        opts.setOptionsSet(true);
        FileIO fileIO = new FileIO();
        fileIO.writeImage(refImage, opts);
        
        //now ready to register all the non-reference volumes to the reference volume and then save in the
        //registered folder
        xdimA = refImage.getExtents()[0];
        ydimA = refImage.getExtents()[1];
        zdimA = refImage.getExtents()[2];
        xresA = refImage.getFileInfo(0).getResolutions()[0];
        yresA = refImage.getFileInfo(0).getResolutions()[1];
        zresA = refImage.getFileInfo(0).getResolutions()[2];
		
		//register!
        registerAndSaveVolumes(studyPathDir);
	    

	    //last steps....rename the studyPath and studyName to the registered one
		studyPath = studyPath + "_registered";
		studyName = studyName + "_registered";
		dwiPathTextField.setText(studyPath);
		
		Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("\n");
        }
		return true;
	}
	
	
	
	public boolean registerAndSaveVolumes(File dir) {
		if(dir.isDirectory()) {
			File[] files = dir.listFiles();
			for(int i = 0; i < files.length; i++) {
				if(files[i].isDirectory()) {
					registerAndSaveVolumes(files[i]);
				}else {
					//load the input image
					String path = files[i].getAbsolutePath();
					String matchImagePathParentFolder = path.substring(0, path.lastIndexOf(File.separator));
					String temp = matchImagePathParentFolder.replace(studyName, registeredStudyName);
					if(temp.equals(registeredDicomB0VolumePathParentFolder)) {
						//this means that this is the ref image...obviusly we dont want to register the ref image to itself
						break;
					}
					Preferences.debug(".", Preferences.DEBUG_ALGORITHM);

                    if (outputTextArea != null) {
                        outputTextArea.append(".");
                    }

                    System.out.print(".");
                    
					int index = path.lastIndexOf(File.separator);
					String fileDir = path.substring(0, index) + File.separator;
					String fileName = path.substring(index+1, path.length());
					matchImage = fileIO.readImage(fileName, fileDir, true, null);
					//call algorithm to register
					reg3 = new AlgorithmRegOAR3D(refImage, matchImage, cost, DOF, interp, rotateBeginX, rotateEndX,
							coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
							rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, doMultiThread,
							fastMode, maxIterations, numMinima);
					reg3.setRunningInSeparateThread(isRunningInSeparateThread());
					reg3.addListener(this);
					ViewJProgressBar progressBar = new ViewJProgressBar("", " ...", 0, 100, true);
			        progressBar.setSeparateThread(isRunningInSeparateThread());
			        reg3.addProgressChangeListener(progressBar);
			        reg3.setProgressValues(0, 100);
					reg3.run();
					TransMatrix finalMatrix = reg3.getTransform();
					transform = new AlgorithmTransform(matchImage, finalMatrix, interp2, xresA, yresA, zresA, xdimA, ydimA, zdimA, true, false, false);
					transform.setUpdateOriginFlag(true);
					transform.run();
					resultImage = transform.getTransformedImage();
					transform.finalize();
					resultImage.calcMinMax();
					//now we need to save the result image in the appropriate place in the registered directory
				    //replace the studyname with the registeredStudyName and then save here
				    String registeredMatchImagePathParentFolder = matchImagePathParentFolder.replace(studyName, registeredStudyName);
				    String fName = registeredMatchImagePathParentFolder.substring(registeredMatchImagePathParentFolder.lastIndexOf(File.separator)+1,registeredMatchImagePathParentFolder.length());
					resultImage.setImageName(fName);

				    int[] extents = refImage.getExtents();
				    FileWriteOptions opts = new FileWriteOptions(true);
			        opts.setFileType(FileUtility.DICOM);
			        opts.setFileDirectory(registeredMatchImagePathParentFolder + File.separator);
			        opts.setFileName(fName);
			        opts.setBeginSlice(0);
			        opts.setEndSlice(extents[2]-1);
			        opts.setOptionsSet(true);
			        opts.setRecalculateInstanceNumber(false);
			        FileIO fileIO = new FileIO();
			        fileIO.writeImage(resultImage, opts);


			        resultImage.disposeLocal();
			        matchImage.disposeLocal();
					resultImage = null;
					matchImage = null;
					//since we only need the first image slice to create the ModelImage, we can break now
					break;
				}
			}

		}
		return true;
	}
	
	
	
	

	public boolean createSubDirsInRegisteredDir(File dir) {
		
		if(dir.isDirectory())  {
			File[] files = dir.listFiles();
			
			for(int i = 0; i < files.length; i++) {
	          if(files[i].isDirectory()) {
	        	  String path = files[i].getAbsolutePath();
	        	  String regPath = path.replace(studyName, registeredStudyName);
	        	  File newDir = new File(regPath);
	        	  newDir.mkdir();
	        	  createSubDirsInRegisteredDir(files[i]);
	          }
	        }
			
		}
		
		return true;
	}
	
	public boolean deleteAllFiles(File dir) {
		if(!dir.exists()) {
            return true;
        }
        boolean res = true;
        if(dir.isDirectory()) {
            File[] files = dir.listFiles();
            for(int i = 0; i < files.length; i++) {
                res &= deleteAllFiles(files[i]);
            }
            res = dir.delete();//Delete dir itself
        } else {
            res = dir.delete();
        }
        return res;
	}
	
	
	/**
	 *  run algorithm for dicom
	 *
	 */
	public void runAlgorithmDICOM() {
		    

	        //remove last slash from study path if it has it
	        if(String.valueOf(studyPath.charAt(studyPath.length() - 1)).equals(File.separator)) {
	        	studyPath = studyPath.substring(0,studyPath.length() - 1);
		    }
	        
	        Preferences.debug("* The study path is " + studyPath + " \n", Preferences.DEBUG_ALGORITHM);

	        if (outputTextArea != null) {
	            outputTextArea.append("* The study path is " + studyPath + " \n");
	        }

	        //System.out.println("* The study path is " + studyPath + " \n");
	        
	        
	        
	        if(isInterleaved) {
	        	Preferences.debug("* Interleaved dataset \n", Preferences.DEBUG_ALGORITHM);

	            if (outputTextArea != null) {
	                outputTextArea.append("* Interleaved dataset \n");
	            }

	            //System.out.println("* Interleaved dataset \n");
	        }
	        
	        
	        if(isInterleaved && bmtxtFilePath.equals("")) {
	        	//this means that no b-matrix file was provided along with this  interleaved dataset
	            Preferences.debug("* For interleaved datasets, b-matrix file is required \n", Preferences.DEBUG_ALGORITHM);

	            if (outputTextArea != null) {
	                outputTextArea.append("* For interleaved datasets, b-matrix file is required \n");
	            }

	            //System.out.println("* For interleaved datasets, b-matrix file is required \n");
	            success = false;
	            finalize();
	            setCompleted(true);
	            return;
	        }


	        // first create a File object based upon the study path
	        File studyPathRoot = new File(studyPath);


	        // parse the directory
	        Preferences.debug("* Parsing", Preferences.DEBUG_ALGORITHM);

	        if (outputTextArea != null) {
	            outputTextArea.append("* Parsing");
	        }

	        System.out.print("* Parsing");


	        try {
	            success = parse(studyPathRoot);
	        } catch (OutOfMemoryError e) {
	        	if (outputTextArea != null) {
	                outputTextArea.append("! ERROR: out of memory....exiting algorithm \n");
	            }
	            //System.out.println("! ERROR: " + e.getCause().toString() + "\n");
	            //System.out.println("! ERROR: out of memory....exiting algorithm \n");
	            success = false;
	            finalize();
	            setCompleted(true);
	            return;
	        } catch (IOException e) {
	        	if (outputTextArea != null) {
	                outputTextArea.append("! ERROR: IOException....exiting algorithm \n");
	            }
	            //System.out.println("! ERROR: " + e.toString() + "\n");
	            //System.out.println("! ERROR: IOException....exiting algorithm \n");
	            success = false;
	            finalize();
	            setCompleted(true);
	            return;
	        }

	        if (success == false) {
	            finalize();
	            setCompleted(true);
	            return;
	        }

	        System.gc();

	        Preferences.debug("\n* Number of image slices in study dir is " + totalImageSlices + " \n",
	                          Preferences.DEBUG_ALGORITHM);

	        if (outputTextArea != null) {
	            outputTextArea.append("\n* Number of image slices in study dir is " + totalImageSlices + " \n");
	        }

	        //System.out.println("\n\n* Number of image slices in study dir is " + totalImageSlices + " \n");


	        if (totalImageSlices == 0) {
	        	success = false;
	            finalize();
	            setCompleted(true);
	            return;
	        }


	        // set initial info
	        success = setInitialInfo();

	        if (success == false) {
	            finalize();
	            setCompleted(true);
	            return;
	        }


	        // create proc dir
	        Preferences.debug("* Creating proc dir \n", Preferences.DEBUG_ALGORITHM);

	        if (outputTextArea != null) {
	            outputTextArea.append("* Creating proc dir \n");
	        }

	        //System.out.println("* Creating proc dir \n");
	        success = createProcDirDICOM();

	        if (success == false) {
	            finalize();
	            setCompleted(true);
	            return;
	        }


	        // create path file
	        Preferences.debug("* Creating path file \n", Preferences.DEBUG_ALGORITHM);

	        if (outputTextArea != null) {
	            outputTextArea.append("* Creating path file \n");
	        }

	        //System.out.println("* Creating path file \n");
	        if(isInterleaved) {
	        	success = createPathFileForInterleavedDICOM();
	        }else {
	        	success = createPathFileDICOM();
	        }

	        if (success == false) {
	            finalize();
	            setCompleted(true);
	            return;
	        }


	        // create list file
	        Preferences.debug("* Creating list file \n", Preferences.DEBUG_ALGORITHM);

	        if (outputTextArea != null) {
	            outputTextArea.append("* Creating list file \n");
	        }

	        //System.out.println("* Creating list file \n");
	        success = createListFileDICOM();

	        if (success == false) {
	            finalize();
	            setCompleted(true);
	            return;
	        }
	 

	        if (!gradientFilePath.equals("")) {
	            // this means user provided gradient file...so...

	            // read gradient file
	            Preferences.debug("* Reading gradient file \n", Preferences.DEBUG_ALGORITHM);

	            if (outputTextArea != null) {
	                outputTextArea.append("* Reading gradient file \n");
	            }

	            //System.out.println("* Reading gradient file \n");
	            success = readGradientFileDICOM();

	            if (success == false) {
	                finalize();
	                setCompleted(true);
	                return;
	            }

	            // obtain b-values
	            Preferences.debug("* Obtaining b-values \n", Preferences.DEBUG_ALGORITHM);

	            if (outputTextArea != null) {
	                outputTextArea.append("* Obtaining b-values \n");
	            }

	            //System.out.println("* Obtaining b-values \n");
	            success = obtainBValuesDICOM();

	            if (success == false) {
	                finalize();
	                setCompleted(true);
	                return;
	            }

	            // create b matrix file
	            Preferences.debug("* Creating b-matrix file \n", Preferences.DEBUG_ALGORITHM);

	            if (outputTextArea != null) {
	                outputTextArea.append("* Creating b-matrix file \n");
	            }

	            //System.out.println("\n* Creating b-matrix file \n");
	            success = createBMatrixFileDICOM();

	            if (success == false) {
	                finalize();
	                setCompleted(true);
	                return;
	            }
	        } else {
	            // this means that the bmtxt file was provided instead...so...

	            // copy and rename b-matrix file
	            Preferences.debug("* b-matrix file provided \n", Preferences.DEBUG_ALGORITHM);

	            if (outputTextArea != null) {
	                outputTextArea.append("* b-matrix file provided \n");
	            }

	            //System.out.println("* b-matrix file provided \n");
	            success = copyBMatrixFile();

	            if (success == false) {
	                finalize();
	                setCompleted(true);
	                return;
	            }
	        }

	        Preferences.debug("** Ending Algorithm \n", Preferences.DEBUG_ALGORITHM);

	        if (outputTextArea != null) {
	            outputTextArea.append("** Ending Algorithm \n");
	        }

	        //System.out.println("** Ending Algorithm \n");

	        finalize();


	        


	}
	
	
	
	/**
	 * run algorithm for par/rec
	 *
	 */
	public void runAlgorithmPARREC() {
		fileParRec = new FilePARREC(prFileName, prFileDir + File.separator);

		

		//read in 4d par/rec data
		try {
			image4D = fileParRec.readImage(false);
		}catch (Exception e) {
			e.printStackTrace();
			success = false;
			finalize();
            setCompleted(true);
            return;
		}
		

		//create proc dir
        Preferences.debug("* Creating proc dir and imageSlices dir \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("* Creating proc dir and imageSlices dir \n");
        }
        success = createProcDirAndImageSlicesDirParRec();
        if (success == false) {
            finalize();
            setCompleted(true);
            return;
        }
        isProcDirCreated = true;
        

        //obtain list file data
		success = obtainListFileDataParRec();
		if (success == false) {
            finalize();
            setCompleted(true);
            return;
        }
        
        
        // create list file
        Preferences.debug("* Creating list file \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append("* Creating list file \n");
        }
        success = createListFileParRec();
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
        success = createBMatrixFileParRec();
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
        success = writeImageSlicesParRec();
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
        success = createPathFileParRec();
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

        image4D.disposeLocal();
    	image4D = null;

        
	}
	
	
	
	
	
	
	
	//--------methods for dicom ---------------------------------------------------------------------------------------
	
	
	
	 /**
     * this method sets initial info about the study.
     *
     * @return  boolean success
     */
    public boolean setInitialInfo() {


        // checking to see if it eDTI
        // first check private tag 0019,109C....if its not null, determine if it is eDTI from there
        // if it is null, check public tag 0008,103E and determine if it is eDTI from there
        String privateTag0019109C = null;
        String publicTag0008103E = null;

        try {

            if (firstFileInfoDicom.getTagTable().getValue("0019,109C") != null) {
                privateTag0019109C = (String) firstFileInfoDicom.getTagTable().getValue("0019,109C");
            }

            if ((privateTag0019109C != null) && (privateTag0019109C.toLowerCase().indexOf("edti") != -1)) {
                isEDTI = true;
                Preferences.debug("* eDTI data set \n", Preferences.DEBUG_ALGORITHM);

                if (outputTextArea != null) {
                    outputTextArea.append("* eDTI data set \n");
                }

                //System.out.println("* eDTI data set \n");
            } else {
                Preferences.debug("* DTI data set \n", Preferences.DEBUG_ALGORITHM);

                if (outputTextArea != null) {
                    outputTextArea.append("* DTI data set \n");
                }

                //System.out.println("* DTI data set \n");
            }
        } catch (NullPointerException e) {
            publicTag0008103E = (String) firstFileInfoDicom.getTagTable().getValue("0008,103E");

            if ((publicTag0008103E != null) && (publicTag0008103E.toLowerCase().indexOf("edti") != -1)) {
                isEDTI = true;
                Preferences.debug("* eDTI data set \n", Preferences.DEBUG_ALGORITHM);

                if (outputTextArea != null) {
                    outputTextArea.append("* eDTI data set \n");
                }

                //System.out.println("* eDTI data set \n");
            } else {
                Preferences.debug("* DTI data set \n", Preferences.DEBUG_ALGORITHM);

                if (outputTextArea != null) {
                    outputTextArea.append("* DTI data set \n");
                }

                //System.out.println("* DTI data set \n");
            }
        }


        // checking to see if it is GE or SIEMENS
        String manufacturer = ((String) firstFileInfoDicom.getTagTable().getValue("0008,0070")).trim();

        if (manufacturer.toLowerCase().startsWith("ge")) {
            isGE = true;
            isSiemens = false;
            Preferences.debug("* Manufacturer : GE \n", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("* Manufacturer : GE \n");
            }

            //System.out.println("* Manufacturer : GE \n");

            // getting software version for GE if regular DTI and not eDTI
            if (!isEDTI) {
                String softwareVersion = ((String) firstFileInfoDicom.getTagTable().getValue("0018,1020")).trim();

                try {
                    geSoftwareVersion = new Integer(softwareVersion).intValue();
                    Preferences.debug("* Software Version : " + geSoftwareVersion + " \n", Preferences.DEBUG_ALGORITHM);

                    if (outputTextArea != null) {
                        outputTextArea.append("* Software Version : " + geSoftwareVersion + " \n");
                    }

                    //System.out.println("* Software Version : " + geSoftwareVersion + " \n");
                } catch (NumberFormatException e) {
                	//software version might be in a format like "12, blah, kdfhasdukfblah"....extract the first number (12)
                	StringBuffer versionSB = new StringBuffer();
            		for(int i=0;i<softwareVersion.length();i++) {
            			char c = softwareVersion.charAt(i);
            			if(Character.isDigit(c)) {
            				versionSB.append(c);
            			}
            			else {
            				break;
            			}
            		}
            		if(versionSB.length() > 0) {
            			geSoftwareVersion = new Integer(versionSB.toString().trim()).intValue();
            			Preferences.debug("* Software Version : " + geSoftwareVersion + " \n", Preferences.DEBUG_ALGORITHM);

                        if (outputTextArea != null) {
                            outputTextArea.append("* Software Version : " + geSoftwareVersion + " \n");
                        }

                        //System.out.println("* Software Version : " + geSoftwareVersion + " \n");
            		}
            		else {
            			Preferences.debug("! ERROR: Unable to determine software version for GE dataset....exiting algorithm \n",
                                Preferences.DEBUG_ALGORITHM);

            			if (outputTextArea != null) {
            					outputTextArea.append("! ERROR: Unable to determine software version for GE dataset....exiting algorithm \n");
            			}

            			//System.out.println("! ERROR: Unable to determine software version for GE dataset....exiting algorithm \n");

            			return false;
            		}
                }
            }
        } else {
            isSiemens = true;
            isGE = false;
            Preferences.debug("* Manufacturer : SIEMENS \n", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("* Manufacturer : SIEMENS \n");
            }

            //System.out.println("* Manufacturer : SIEMENS \n");
        }

        return true;
    }
	
	
	
    /**
     * this method parses the study dir sort into lists using series number that is available in dicom tag. then based
     * on the filename, get vol number (for example mipav_dicom_dti_series11_volume10005.dcm), and sort within lists
     * using vol number and instance number that comes from dicom tag.
     *
     * @param   file 
     *
     * @return  boolean success
     *
     * @throws  IOException     
     * @throws  OutOfMemoryError 
     */
    public boolean parse(File file) throws IOException, OutOfMemoryError {
        imageFilter = new ViewImageFileFilter(new String[] { ".dcm", ".DCM", ".ima", ".IMA" });

        File[] children = file.listFiles();
        FileDicom imageFile = null;

        try {

            for (int i = 0; i < children.length; i++) {

                if (isThreadStopped()) {
                    return false;
                }

                if (children[i].isDirectory()) {
                    parse(children[i]);
                } else if (!children[i].isDirectory()) {

                    try {

                        if (imageFile == null) {
                            imageFile = new FileDicom(children[i].getName(),
                                                      children[i].getParent() + File.separatorChar);
                        } else {
                            imageFile.setFileName(children[i], firstFileInfoDicom);
                        }

                        if (imageFilter.accept(children[i])) {
                            success = imageFile.readHeader(true);
                        } else {
                            imageFile.finalize();
                            imageFile = null;

                            continue;
                        }
                    } catch (IOException error) {
                        /*error.printStackTrace();
                        System.gc();
                        i--;

                        continue;*/
                    	error.printStackTrace();
                    	throw error;

                    }

                    FileInfoDicom fileInfoDicom = (FileInfoDicom) imageFile.getFileInfo();

                    if (totalImageSlices == 0) {

                        // we need the first file info dicom handle so we can use it for the list file
                        firstFileInfoDicom = fileInfoDicom;
                    }


                    // get relevant info from fileinfodicom and place it in array that is then sorted by
                    // instance number and vol in its appropriate series list
                    dicomInfo = new String[8];

                    String instanceNo = ((String) fileInfoDicom.getTagTable().getValue("0020,0013")).trim();

                    if (instanceNo.trim().equals("")) {
                        Preferences.debug("! ERROR: instance number dicom field is empty.....exiting algorithm \n",
                                          Preferences.DEBUG_ALGORITHM);

                        if (outputTextArea != null) {
                            outputTextArea.append("! ERROR: instance number dicom field is empty.....exiting algorithm \n");
                        }

                        //System.out.println("! ERROR: instance number dicom field is empty.....exiting algorithm \n");

                        return false;
                    }


                    TransMatrix matrix = null;
                    float[] tPt = new float[3];
                    matrix = fileInfoDicom.getPatientOrientation();

                    if (matrix != null) {

                        /* transform the x location, y location, and z location, found
                         * from the Image Position tag, by the matrix.  The tPt array now has the three numbers arranged
                         * as if this image had been transformed.  The third place in the array holds the number that
                         * the axis is being sliced along. xlocation, ylocation, zlocation are from the DICOM tag
                         * 0020,0032 patient location.....in other words tPt[2] is the slice location;
                         */
                        matrix.transform(fileInfoDicom.xLocation, fileInfoDicom.yLocation, fileInfoDicom.zLocation,
                                         tPt);

                    } else {
                        Preferences.debug("! ERROR: unable to obtain image orientation matrix.....exiting algorithm \n",
                                          Preferences.DEBUG_ALGORITHM);

                        if (outputTextArea != null) {
                            outputTextArea.append("! ERROR: unable to obtain image orientation matrix.....exiting algorithm \n");
                        }

                        //System.out.println("! ERROR: unable to obtain image orientation matrix.....exiting algorithm \n");

                        return false;
                    }

                    String absPath = fileInfoDicom.getFileDirectory();
                    String fileName = fileInfoDicom.getFileName();
                    String publicTag00180024 = ((String) fileInfoDicom.getTagTable().getValue("0018,0024"));
                    String privateTag00431039 = null;
                    String privateTag001910B9 = null;

                    try {

                        if (fileInfoDicom.getTagTable().getValue("0043,1039") != null) {
                            privateTag00431039 = (String) fileInfoDicom.getTagTable().getValue("0043,1039");
                        }
                    } catch (NullPointerException e) {
                        privateTag00431039 = null;
                    }


                    try {

                        if (fileInfoDicom.getTagTable().getValue("0019,10B9") != null) {
                            privateTag001910B9 = (String) fileInfoDicom.getTagTable().getValue("0019,10B9");
                        }
                    } catch (NullPointerException e) {
                        privateTag001910B9 = null;
                    }
                    
                    
                    
                    String imageSlice = ((String) fileInfoDicom.getTagTable().getValue("0020,1041")).trim();
					if(imageSlice.trim().equals("")) {
						Preferences.debug("! ERROR: image slice dicom field is empty.....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
						if (outputTextArea != null) {
							outputTextArea.append("! ERROR: image slice dicom field is empty.....exiting algorithm \n");
						}
						return false;
					}

                    dicomInfo[0] = instanceNo;
                    dicomInfo[1] = String.valueOf(tPt[2]);
                    dicomInfo[2] = absPath;
                    dicomInfo[3] = fileName;
                    dicomInfo[4] = publicTag00180024;
                    dicomInfo[5] = privateTag00431039;
                    dicomInfo[6] = privateTag001910B9;
                    dicomInfo[7] = imageSlice;
                    

                    String seriesNumber_String = ((String) fileInfoDicom.getTagTable().getValue("0020,0011")).trim();


                    if ((seriesNumber_String == null) || seriesNumber_String.equals("")) {
                        seriesNumber_String = "0";
                    }

                    Integer seriesNumber = new Integer(seriesNumber_String);
                    if(isInterleaved) {
	                    if (seriesFileInfoTreeMap.get(seriesNumber) == null) {
	                        seriesFileInfoTreeSet = new TreeSet<String[]>(new InstanceNumberVolComparator());
	                        seriesFileInfoTreeSet.add(dicomInfo);
	                        seriesFileInfoTreeMap.put(seriesNumber, seriesFileInfoTreeSet);
	                    } else {
	                        seriesFileInfoTreeSet = (TreeSet<String[]>) seriesFileInfoTreeMap.get(seriesNumber);
	                        seriesFileInfoTreeSet.add(dicomInfo);
	                        seriesFileInfoTreeMap.put(seriesNumber, seriesFileInfoTreeSet);
	                    }
                    }
                    else {
                    	if (seriesFileInfoTreeMap.get(seriesNumber) == null) {
    						seriesFileInfoTreeSet = new TreeSet<String[]>(new InstanceNumberComparator());
    						seriesFileInfoTreeSet.add(dicomInfo);
    						seriesFileInfoTreeMap.put(seriesNumber, seriesFileInfoTreeSet);
    					} else {
    						seriesFileInfoTreeSet = (TreeSet<String[]>) seriesFileInfoTreeMap.get(seriesNumber);
    						seriesFileInfoTreeSet.add(dicomInfo);
    						seriesFileInfoTreeMap.put(seriesNumber, seriesFileInfoTreeSet);
    					}
                    }

                    dicomInfo = null;
                    fileInfoDicom = null;

                    if ((totalImageSlices % 100) == 0) {
                        Preferences.debug(".", Preferences.DEBUG_ALGORITHM);

                        if (outputTextArea != null) {
                            outputTextArea.append(".");
                        }

                        System.out.print(".");
                    }

                    if ((totalImageSlices % 500) == 0) {
                        System.gc();
                    }

                    totalImageSlices++;
                }
            }
        } finally {
        	if(imageFile != null) {
        		imageFile.finalize();
        		imageFile = null;
        	}
        }

        return true;
    }
	
	

    /**
     * this method creates the proc dir in which the list file, path file, and b-matrix file go.
     *
     * @return  boolean success
     */
    public boolean createProcDirDICOM() {

        // create parallel proc dir to study path that will hold list file, path
        // file, and b matrix file
        File procDir = new File(studyPath + "_proc");

        if (!procDir.isDirectory()) {
            boolean success = new File(studyPath + "_proc").mkdir();

            if (!success) {
                Preferences.debug("! ERROR: Creation of proc directory failed....exiting algorithm \n",
                                  Preferences.DEBUG_ALGORITHM);

                if (outputTextArea != null) {
                    outputTextArea.append("! ERROR: Creation of proc directory failed....exiting algorithm \n");
                }

                //System.out.println("! ERROR: Creation of proc directory failed....exiting algorithm \n");

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

        Preferences.debug(" - proc dir created : " + studyPath + "_proc \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append(" - proc dir created : " + studyPath + "_proc \n");
        }

        //System.out.println(" - proc dir created : " + studyPath + "_proc \n");
        isProcDirCreated = true;

        return true;
    }
	

    
    /**
	 * This method creates the path file for interleaverd datasets
	 * 
	 * @return boolean success
	 */
	public boolean createPathFileForInterleavedDICOM() {

		try {
			File pathFile = new File(studyPath + "_proc" + File.separator + studyName + ".path");
			FileOutputStream outputStream = new FileOutputStream(pathFile);
			PrintStream printStream = new PrintStream(outputStream);
			Set<Integer> ketSet = seriesFileInfoTreeMap.keySet();
			Iterator<Integer> iter = ketSet.iterator();
			ArrayList<Integer> numSlicesCheckList = new ArrayList<Integer>();
			while (iter.hasNext()) {
				TreeSet<String[]> seriesFITS = (TreeSet<String[]>) seriesFileInfoTreeMap.get(iter.next());
				Iterator<String[]> iter2 = seriesFITS.iterator();
				// lets get the first element and remember its imageSlice
				String imageSlice = ((String) (((String[]) seriesFITS.first())[7])).trim();

				// now we need to figure out how many slices are in each
				// vol...do this by
				// finding at what value the counter is when it is equal to the
				// first one since
				// the imageSlice wraps around...this represents the next
				// vol...so the num of slices
				// in each vol is 1 less that
				int counter = 1;
				int sliceNum = 0;
				while (iter2.hasNext()) {
					String[] arr = (String[]) iter2.next();
					String imgSlice = ((String) arr[7]).trim();
					//this is to just get total num volumes
					if (imgSlice.equals(imageSlice)) {
						totalNumVolumes = totalNumVolumes + 1;
					}
					//this is to compare the image slices...but we dont want to get the first one
					if (imgSlice.equals(imageSlice) && counter != 1) {
						
						numSlicesCheckList.add(new Integer(counter - 1 - sliceNum));
						sliceNum = counter - 1;
					}
					++counter;
				}
			}
			numSlicesPerVolume = ((Integer)numSlicesCheckList.get(0)).intValue();
			for(int i=1;i<numSlicesCheckList.size();i++) {
				if(((Integer)numSlicesCheckList.get(0)).intValue() != numSlicesPerVolume) {
					Preferences.debug("! ERROR: There are not equal number of image slices in all volumes of this study....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
					if (outputTextArea != null) {
						outputTextArea.append("! ERROR: There are not equal number of image slices in all volumes of this study....exiting algorithm \n");
					}
					return false;
				}
			}
			
			//ok....we have equal # of image slices in all volumes of the study
			Object[] keyArray = seriesFileInfoTreeMap.keySet().toArray();
			String relPath;
			for(int i=0;i<numSlicesPerVolume;i++) {
				for(int k=0;k<keyArray.length;k++) {
					Object[] fidArr = ((TreeSet<String[]>) seriesFileInfoTreeMap.get(keyArray[k])).toArray();
					int numVols = fidArr.length / numSlicesPerVolume;
					String absPath = (String)((String[]) fidArr[i])[2];
					relPath = new String(".." + File.separator + studyName +absPath.substring(absPath.lastIndexOf(studyName) + studyName.length(), absPath.length()));
					printStream.println(relPath + (String)((String[]) fidArr[i])[3]);
					for (int j = 1; j < numVols; j++) {
						absPath = (String)((String[]) fidArr[i + (numSlicesPerVolume * j)])[2];
						relPath = new String(".." + File.separator + studyName + absPath.substring(absPath.lastIndexOf(studyName) + studyName.length(), absPath.length()));
						printStream.println(relPath + (String)((String[]) fidArr[i + (numSlicesPerVolume * j)])[3]);
					}
					
				}
			}
			outputStream.close();
			Preferences.debug(" - path file created : " + studyPath + "_proc" + File.separator + studyName + ".path" + " \n", Preferences.DEBUG_ALGORITHM);
			if (outputTextArea != null) {
				outputTextArea.append(" - path file created : " + studyPath + "_proc" + File.separator + studyName + ".path" + " \n");
			}
			return true;
		}
		catch(Exception e) {
			Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("! ERROR: Creation of path file failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
			if (outputTextArea != null) {
				outputTextArea.append("! ERROR: " + e.toString() + "\n");
				outputTextArea.append("! ERROR: Creation of path file failed....exiting algorithm \n");
			}
			e.printStackTrace();
			return false;
		}
	}
    

	
	/**
     * This method creates the path file.
     *
     * @return  boolean success
     */
    public boolean createPathFileDICOM() {

        try {
        	pathFileName = studyName + ".path";
            File pathFile = new File(studyPath + "_proc" + File.separator + pathFileName);
            FileOutputStream outputStream = new FileOutputStream(pathFile);
            PrintStream printStream = new PrintStream(outputStream);
            Set<Integer> ketSet = seriesFileInfoTreeMap.keySet();
            Iterator<Integer> iter = ketSet.iterator();
            ArrayList<Integer> numSlicesCheckList = new ArrayList<Integer>();

            while (iter.hasNext()) {
                TreeSet<String[]> seriesFITS = (TreeSet<String[]>) seriesFileInfoTreeMap.get(iter.next());
                Iterator<String[]> iter2 = seriesFITS.iterator();

                // lets get the first element and remember its slice location
                String sliceLocation = ((String) (((String[]) seriesFITS.first())[1])).trim();

                // now we need to figure out how many slices are in each
                // vol...do this by
                // finding at what value the counter is when it is equal to the
                // first one since
                // the sliceLocation wraps around...this represents the next
                // vol...so the num of slices
                // in each vol is 1 less that
                int counter = 1;
                int sliceNum = 0;

                while (iter2.hasNext()) {
                    String[] arr = (String[]) iter2.next();
                    //String imgSlice = ((String) arr[1]).trim();
                    String imgSlice = ((String) arr[7]).trim();

                    // this is to just get total num volumes
                    if (imgSlice.equals(sliceLocation)) {
                        totalNumVolumes = totalNumVolumes + 1;
                    }

                    // this is to compare the image slices...but we dont want to get the first one
                    if (imgSlice.equals(sliceLocation) && (counter != 1)) {

                        numSlicesCheckList.add(new Integer(counter - 1 - sliceNum));
                        sliceNum = counter - 1;
                    }

                    ++counter;
                }
            }

            numSlicesPerVolume = ((Integer) numSlicesCheckList.get(0)).intValue();

            for (int i = 1; i < numSlicesCheckList.size(); i++) {

                if (((Integer) numSlicesCheckList.get(0)).intValue() != numSlicesPerVolume) {
                    Preferences.debug("! ERROR: There are not equal number of image slices in all volumes of this study....exiting algorithm \n",
                                      Preferences.DEBUG_ALGORITHM);

                    if (outputTextArea != null) {
                        outputTextArea.append("! ERROR: There are not equal number of image slices in all volumes of this study....exiting algorithm \n");
                    }

                    //System.out.println("! ERROR: There are not equal number of image slices in all volumes of this study....exiting algorithm \n");

                    return false;
                }
            }

            // ok....we have equal # of image slices in all volumes of the study
            Object[] keyArray = seriesFileInfoTreeMap.keySet().toArray();
            String relPath;

            for (int i = 0; i < numSlicesPerVolume; i++) {

                for (int k = 0; k < keyArray.length; k++) {
                    Object[] fidArr = ((TreeSet<String[]>) seriesFileInfoTreeMap.get(keyArray[k])).toArray();
                    int numVols = fidArr.length / numSlicesPerVolume;
                    String absPath = (String) ((String[]) fidArr[i])[2];
                    relPath = new String(".." + File.separator + studyName +
                                         absPath.substring(absPath.lastIndexOf(studyName) + studyName.length(),
                                                           absPath.length()));
                    printStream.println(relPath + (String) ((String[]) fidArr[i])[3]);

                    for (int j = 1; j < numVols; j++) {
                        absPath = (String) ((String[]) fidArr[i + (numSlicesPerVolume * j)])[2];
                        relPath = new String(".." + File.separator + studyName +
                                             absPath.substring(absPath.lastIndexOf(studyName) + studyName.length(),
                                                               absPath.length()));
                        printStream.println(relPath + (String) ((String[]) fidArr[i + (numSlicesPerVolume * j)])[3]);
                    }

                }
            }

            outputStream.close();
            Preferences.debug(" - path file created : " + studyPath + "_proc" + File.separator + studyName + ".path" +
                              " \n", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append(" - path file created : " + studyPath + "_proc" + File.separator + studyName +
                                      ".path" + " \n");
            }

            //System.out.println(" - path file created : " + studyPath + "_proc" + File.separator + studyName + ".path" +
                               //" \n");

            return true;
        } catch (Exception e) {
            Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Creation of path file failed....exiting algorithm \n",
                              Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: Creation of path file failed....exiting algorithm \n");
            }

            //System.out.println("! ERROR: " + e.toString() + "\n");
            //System.out.println("! ERROR: Creation of path file failed....exiting algorithm \n");
            e.printStackTrace();

            return false;
        }
    }
    

    
    /**
     * This method creates the list file.
     *
     * @return  boolean success
     */
    public boolean createListFileDICOM() {
        // we can get the info for the list file from just 1 of the imageSlices' FileInfoDicom TreeSet firstTS =
        // (TreeSet)seriesFileInfoTreeMap.get(seriesFileInfoTreeMap.firstKey()); FileInfoDicom fileInfoDicom =
        // (FileInfoDicom)firstTS.first();

        Short originalColumns = (Short) (firstFileInfoDicom.getTagTable().getValue("0028,0011"));
        originalColumnsString = originalColumns.toString();

        Short originalRows = (Short) (firstFileInfoDicom.getTagTable().getValue("0028,0010"));
        originalRowsString = originalRows.toString();

        String dir = ((String) (firstFileInfoDicom.getTagTable().getValue("0018,1312"))).trim();


        if (dir.equalsIgnoreCase("col")) {
            phaseEncodingString = "vertical";
        } else if (dir.equalsIgnoreCase("row")) {
        	phaseEncodingString = "horizontal";
        }


        sliceThicknessString = ((String) (firstFileInfoDicom.getTagTable().getValue("0018,0050"))).trim();
        float sliceTh = new Float(sliceThicknessString.trim()).floatValue();

        sliceGapString = ((String) (firstFileInfoDicom.getTagTable().getValue("0018,0088"))).trim();
        float sliceGp = new Float(sliceGapString.trim()).floatValue();
        
        sliceGp = sliceTh - sliceGp;
        sliceGapString = String.valueOf(sliceGp);

        String fieldOfView = (String) (firstFileInfoDicom.getTagTable().getValue("0018,1100"));

        if ((fieldOfView == null) || fieldOfView.trim().equals("")) {

            // get pixel space in x direction
            String xyPixelSpacingString = ((String) (firstFileInfoDicom.getTagTable().getValue("0028,0030"))).trim();
            int index = xyPixelSpacingString.indexOf("\\");
            String xPixelSpacingString = xyPixelSpacingString.substring(index+1, xyPixelSpacingString.length());
            float xPixelSpacing = new Float(xPixelSpacingString).floatValue();
            float fieldOfViewFloat = xPixelSpacing * originalColumns.shortValue();
            fieldOfView = String.valueOf(fieldOfViewFloat);
        }


        // hard coded for now
        String imagePlane = "axial";

        // hard coded for now
        String rawImageFormat = "dicom";

        String pathFilename = studyName + ".path";

        String bmtrixFilename = studyName + ".BMTXT";

        nimString = String.valueOf(totalNumVolumes);
        


        try {
        	listFileName = studyName + ".list";
            File listFile = new File(studyPath + "_proc" + File.separator + listFileName);
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
            printStream.println("<slice>" + numSlicesPerVolume + "</slice>");
            printStream.println();
            printStream.println("<!-- NUMBER OF BMATRICES -->");
            printStream.println("<nim>" + nimString + "</nim>");
            printStream.println();
            printStream.println("<!-- ORIENTATION OF PHASE ENCODING (vertical, horizontal) -->");
            printStream.println("<phase_encode_direction>" + phaseEncodingString + "</phase_encode_direction>");
            printStream.println();
            printStream.println("<!-- HORIZONTAL FIELD OF VIEW (in mm) -->");
            printStream.println("<x_field_of_view>" + fieldOfView + "</x_field_of_view>");
            printStream.println();
            printStream.println("<!-- VERTICAL FIELD OF VIEW (in mm) -->");
            printStream.println("<y_field_of_view>" + fieldOfView + "</y_field_of_view>");
            printStream.println();
            printStream.println("<!-- FORMAT OF RAW IMAGES (integer, float, dicom, dummy) -->");
            printStream.println("<rawimageformat>" + rawImageFormat + "</rawimageformat>");
            printStream.println();
            printStream.println("<!-- NAME OF BMATRIX FILE -->");
            printStream.println("<bmatrixfile>" + bmtrixFilename + "</bmatrixfile>");
            printStream.println();
            printStream.println("<!-- GAP BETWEEN SLICES (in mm. Write 0 for contiguous slices) -->");
            printStream.println("<slice_gap>" + sliceGapString + "</slice_gap>");
            printStream.println();
            printStream.println("<!-- SLICE THICKNESS (in mm) -->");
            printStream.println("<slice_thickness>" + sliceThicknessString + "</slice_thickness>");
            printStream.println();
            printStream.println("<!-- IMAGE PLANE (axial,coronal,sagittal) -->");
            printStream.println("<image_plane>" + imagePlane + "</image_plane>");
            printStream.println();
            printStream.println("<!-- NAME OF FILE CONTAINING PATH OF RAW IMAGES -->");
            printStream.println("<raw_image_path_filename>" + pathFilename + "</raw_image_path_filename>");

            outputStream.close();
        } catch (Exception e) {
            Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Creation of list file failed....exiting algorithm \n",
                              Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: Creation of list file failed....exiting algorithm \n");
            }

            //System.out.println("! ERROR: " + e.toString() + "\n");
            //System.out.println("! ERROR: Creation of list file failed....exiting algorithm \n");

            return false;
        }

        Preferences.debug(" - list file created : " + studyPath + "_proc" + File.separator + studyName + ".list" +
                          " \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append(" - list file created : " + studyPath + "_proc" + File.separator + studyName +
                                  ".list" + " \n");
        }

        //System.out.println(" - list file created : " + studyPath + "_proc" + File.separator + studyName + ".list" +
                           //" \n");

        return true;
    }
    
    

    /**
     * This method reads in the gradient file gradient file can be in 2 differetn ways....for eDTI, there will be a
     * number on first line followed by correct number of gradients if it is DTI, there will be no number and just 7
     * linies of gradients....this needs to be duplicated as many times as there are series.
     *
     * @return  boolean success
     */
    public boolean readGradientFileDICOM() {

        try {
            String str;
            FileInputStream fis = new FileInputStream(gradientFilePath);
            BufferedReader d = new BufferedReader(new InputStreamReader(fis));
            String firstLine = d.readLine();
            String[] firstLineSplits = firstLine.split("\\s+");
            ArrayList<Float> arrList = new ArrayList<Float>(); 
            ArrayList<Float> arrList2 = new ArrayList<Float>();

            for (int i = 0; i < firstLineSplits.length; i++) {

                if (!(firstLineSplits[i].trim().equals(""))) {
                    arrList.add(new Float(firstLineSplits[i]));
                }
            }

            if (arrList.size() == 1) {

                // this means the first line is just 1 number...indicating eDTI grad file
                nim = ((Float) arrList.get(0)).intValue();
                direction = new float[nim][3];

                while ((str = d.readLine()) != null) {
                    str = str.trim();

                    String[] arr = str.split("\\s+");

                    for (int i = 0; i < arr.length; i++) {

                        if (!(arr[i].trim().equals(""))) {
                            arrList2.add(new Float(arr[i]));
                        }
                    }
                }


                for (int j = 0, k = 0; j < nim; j++, k = k + 3) {
                    direction[j][0] = ((Float) arrList2.get(k)).floatValue();
                    direction[j][1] = ((Float) arrList2.get(k + 1)).floatValue();
                    direction[j][2] = ((Float) arrList2.get(k + 2)).floatValue();
                }
            } else {

                // this means it is for reg DTI grad file
                nim = totalNumVolumes;

                direction = new float[nim][3];

                while ((str = d.readLine()) != null) {
                    str = str.trim();

                    String[] arr = str.split("\\s+");

                    for (int i = 0; i < arr.length; i++) {

                        if (!(arr[i].trim().equals(""))) {
                            arrList.add(new Float(arr[i]));
                        }
                    }
                }

                
                //we want to handle the case of gradient directions that have
                //7 lines in the gradient file as well if number of lines in
                //gradient file match up with totalNumVOlumes
                
                
                if(arrList.size() == 7 || (arrList.size() == nim*3)) {
                	if(arrList.size() == 7) {
		                for (int j = 0, k = 0; j < 7; j++, k = k + 3) {
		                    direction[j][0] = ((Float) arrList.get(k)).floatValue();
		                    direction[j][1] = ((Float) arrList.get(k + 1)).floatValue();
		                    direction[j][2] = ((Float) arrList.get(k + 2)).floatValue();
		                }
		
		
		                // we got the first 7 lines for the direction already...so start on the 8th
		                int counter2 = 0;
		
		                for (int i = 8; i <= totalNumVolumes; i++) {
		
		                    direction[i - 1][0] = direction[counter2][0];
		                    direction[i - 1][1] = direction[counter2][1];
		                    direction[i - 1][2] = direction[counter2][2];
		
		                    if ((i % 7) == 0) {
		
		                        // reset counter
		                        counter2 = 0;
		                    } else {
		                        counter2++;
		                    }
		                }
                	}else {
                		//this is the case if num lines in gradient file match up with total num volumes
                		for (int j = 0, k = 0; j < totalNumVolumes; j++, k = k + 3) {
		                    direction[j][0] = ((Float) arrList.get(k)).floatValue();
		                    direction[j][1] = ((Float) arrList.get(k + 1)).floatValue();
		                    direction[j][2] = ((Float) arrList.get(k + 2)).floatValue();
		                }
                	}
                }else {
                    Preferences.debug("! ERROR: Number of line entries in gradient file do not match up with number of volumes in dataset...exiting algorithm \n",
                                      Preferences.DEBUG_ALGORITHM);

                    if (outputTextArea != null) {
                        outputTextArea.append("! ERROR: Number of line entries in gradient file do not match up with number of volumes in dataset...exiting algorithm \n");
                    }

                    //System.out.println("! ERROR: Number of line entries in gradient file do not match up with number of volumes in dataset...exiting algorithm \n");

                    return false;
                	
                }
            }

            fis.close();
        } catch (Exception e) {
            Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: reading of gradient file failed....exiting algorithm \n",
                              Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: reading of gradient file failed....exiting algorithm \n");
            }

            //System.out.println("! ERROR: " + e.toString() + "\n");
            //System.out.println("! ERROR: reading of gradient file failed....exiting algorithm \n");

            return false;
        }

        Preferences.debug(" - gradient file read \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append(" - gradient file read \n");
        }

        //System.out.println(" - gradient file read \n");

        return true;
    }
    
    

    /**
     * This method obtains the b-values for each volume from either the public tag or the private tag.
     *
     * @return  boolean success
     */
    public boolean obtainBValuesDICOM() {
        // get b-values for each volume set

        // For DTI: 1. If Siemens, then extract b-value from public tag 0018,0024...the b-value is the number after ep_b
        // and before #   2. If GE and if Software Version is <= 8, then extract b-value from private tag
        // 0019,10B9...the b-value is the number after ep_b 3. If GE and if Software Version is > 8, then extract
        // b-value from private tag 0043,1039....the b-value is the first number in the string

        // For eDTI: 1. If Siemens, then extract b-value from public tag 0018,0024...the b-value is the number after
        // ep_b and before #   2. If GE, then extract b-value from private tag 0043,1039....the b-value is the first
        // number in the string

        Set<Integer> ketSet = seriesFileInfoTreeMap.keySet();
        Iterator<Integer> iter = ketSet.iterator();

        Preferences.debug(" - b-values :\n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append(" - b-values :\n");
        }

        //System.out.println(" - b-values :\n");

        while (iter.hasNext()) {
            TreeSet<String[]> seriesFITS = (TreeSet<String[]>) seriesFileInfoTreeMap.get(iter.next());
            int seriesFITSSize = seriesFITS.size();
            int numVols = seriesFITSSize / numSlicesPerVolume;
            Object[] fidArr = seriesFITS.toArray();
            String bValueLongString_pubTag = "";
            String bValueLongString_privTag_0043 = "";
            String bValueString_privTag_001910B9 = "";
            String bValueString;
            Float bValue;
            int poundIndex = -1;

            for (int k = 0; k < numVols; k++) {

                if (isSiemens) {
                    bValueLongString_pubTag = (String) (((String[]) fidArr[numSlicesPerVolume * k])[4]);

                    int length = 0;
                    int index = -1;

                    if ((bValueLongString_pubTag != null) && (!bValueLongString_pubTag.trim().equals(""))) {
                        String ep_b = "ep_b";
                        length = ep_b.length();
                        index = bValueLongString_pubTag.indexOf(ep_b);
                        poundIndex = bValueLongString_pubTag.indexOf('#');

                        if (index != -1) {
                            index = index + length;

                            if ((poundIndex != -1) && (poundIndex > index)) {
                                bValueString = (bValueLongString_pubTag.substring(index, poundIndex)).trim();
                            } else {
                                bValueString = (bValueLongString_pubTag.substring(index,
                                                                                  bValueLongString_pubTag.length()))
                                                   .trim();
                            }

                            if (bValueString.equals("") || (bValueString == null)) {
                                Preferences.debug("! ERROR: No b-value found in private tag 0018,0024....exiting algorithm \n",
                                                  Preferences.DEBUG_ALGORITHM);

                                if (outputTextArea != null) {
                                    outputTextArea.append("! ERROR: No b-value found in private tag 0018,0024....exiting algorithm \n");
                                }

                                //System.out.println("! ERROR: No b-value found in private tag 0018,0024....exiting algorithm \n");

                                return false;
                            }

                            bValue = new Float(bValueString);
                            bValuesArrayList.add(bValue);

                            continue;
                        } else {
                            Preferences.debug("! ERROR: No b-value found in private tag 0018,0024....if dataset is interleaved, b-matrix file must be provided....exiting algorithm \n",
                                              Preferences.DEBUG_ALGORITHM);

                            if (outputTextArea != null) {
                                outputTextArea.append("! ERROR: No b-value found in private tag 0018,0024....if dataset is interleaved, b-matrix file must be provided....exiting algorithm \n");
                            }

                            //System.out.println("! ERROR: No b-value found in private tag 0018,0024....if dataset is interleaved, b-matrix file must be provided....exiting algorithm \n");

                            return false;
                        }
                    }
                }

                if (isGE) {

                    if (!isEDTI && (geSoftwareVersion <= 8)) {

                        if (((String[]) fidArr[numSlicesPerVolume * k])[6] != null) {
                            bValueString_privTag_001910B9 = (String) (((String[]) fidArr[numSlicesPerVolume * k])[6]);
                            bValueString = bValueString_privTag_001910B9.trim();

                            if (bValueString.equals("") || (bValueString == null)) {
                                Preferences.debug("! ERROR: No b-value found in private tag 0019,10B9....exiting algorithm \n",
                                                  Preferences.DEBUG_ALGORITHM);

                                if (outputTextArea != null) {
                                    outputTextArea.append("! ERROR: No b-value found in private tag 0019,10B9....exiting algorithm \n");
                                }

                                //System.out.println("! ERROR: No b-value found in private tag 0019,10B9....exiting algorithm \n");

                                return false;
                            }

                            bValue = new Float(bValueString);
                            bValuesArrayList.add(bValue);

                            continue;
                        } else {
                            Preferences.debug("! ERROR: No b-value found in private tag 0019,10B9....exiting algorithm \n",
                                              Preferences.DEBUG_ALGORITHM);

                            if (outputTextArea != null) {
                                outputTextArea.append("! ERROR: No b-value found in private tag 0019,10B9....exiting algorithm \n");
                            }

                            //System.out.println("! ERROR: No b-value found in private tag 0019,10B9....exiting algorithm \n");

                            return false;
                        }
                    } else {

                        if (((String[]) fidArr[numSlicesPerVolume * k])[5] != null) {
                            bValueLongString_privTag_0043 = (String) (((String[]) fidArr[numSlicesPerVolume * k])[5]);
                        } else {
                            Preferences.debug("! ERROR: No b-value found in private tag 0043,1039....exiting algorithm \n",
                                              Preferences.DEBUG_ALGORITHM);

                            if (outputTextArea != null) {
                                outputTextArea.append("! ERROR: No b-value found in private tag 0043,1039....exiting algorithm \n");
                            }

                            //System.out.println("! ERROR: No b-value found in private tag 0043,1039....exiting algorithm \n");

                            return false;
                        }

                        if ((bValueLongString_privTag_0043 != null) &&
                                (!bValueLongString_privTag_0043.trim().equals(""))) {
                            int index_privTag = bValueLongString_privTag_0043.indexOf("\\");

                            if (index_privTag != -1) {
                                bValueString = (bValueLongString_privTag_0043.substring(0, index_privTag)).trim();
                                bValue = new Float(bValueString);
                                bValuesArrayList.add(bValue);

                                continue;
                            } else {
                                Preferences.debug("! ERROR: No b-value found in private tag 0043,1039....exiting algorithm \n",
                                                  Preferences.DEBUG_ALGORITHM);

                                if (outputTextArea != null) {
                                    outputTextArea.append("! ERROR: No b-value found in private tag 0043,1039....exiting algorithm \n");
                                }

                                //System.out.println("! ERROR: No b-value found in private tag 0043,1039....exiting algorithm \n");

                                return false;
                            }
                        } else {
                            Preferences.debug("! ERROR: No b-value found in private tag 0043,1039....exiting algorithm \n",
                                              Preferences.DEBUG_ALGORITHM);

                            if (outputTextArea != null) {
                                outputTextArea.append("! ERROR: No b-value found in private tag 0043,1039....exiting algorithm \n");
                            }

                            //System.out.println("! ERROR: No b-value found in private tag 0043,1039....exiting algorithm \n");

                            return false;
                        }
                    }
                }
            }
        }

        // lets output the b-values to screen:
        Preferences.debug(" - [  ", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append(" - [  ");
        }

        System.out.print(" - [  ");

        for (int i = 0; i < bValuesArrayList.size(); i++) {
            float b = ((Float) bValuesArrayList.get(i)).floatValue();
            Preferences.debug(String.valueOf(b) + "   ", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append(String.valueOf(b) + "   ");
            }

            System.out.print(String.valueOf(b) + "   ");
        }

        Preferences.debug("] \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append("] \n");
        }

        System.out.print("] \n");

        // check that num of b-values and num vols in gradient file are same
        if (bValuesArrayList.size() != direction.length) {
            Preferences.debug("! ERROR: the num of b values obtained, " + bValuesArrayList.size() +
                              ", and the number of vols in the gradient file, " + direction.length +
                              ", are not the same....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: the num of b values obtained, " + bValuesArrayList.size() +
                                      ", and the number of vols in the gradient file, " + direction.length +
                                      ", are not the same....exiting algorithm \n");
            }

            //System.out.println("! ERROR: the num of b values obtained, " + bValuesArrayList.size() +
                               //", and the number of vols in the gradient file, " + direction.length +
                               //", are not the same....exiting algorithm \n");

            return false;
        }

        return true;
    }
    
	

    /**
     * This method creates the b-matrix file.
     *
     * @return  boolean success
     */
    public boolean createBMatrixFileDICOM() {

        try {
            StringBuffer sb;
            int padLength;
            bmatrixFileName = studyName + ".BMTXT";
            File bMatrixFile = new File(studyPath + "_proc" + File.separator + bmatrixFileName);
            
            FileOutputStream outputStream = new FileOutputStream(bMatrixFile);
            PrintStream printStream = new PrintStream(outputStream);

            // formula for bmtxt values is :
            // bxx 2bxy 2bxz byy 2byz bzz
            // x, y, and z values come from the direction[][]
            for (int i = 0; i < bValuesArrayList.size(); i++) {
                float b = ((Float) bValuesArrayList.get(i)).floatValue();
                float x = direction[i][0];
                float y = direction[i][1];
                float z = direction[i][2];

                float _bxx = b * x * x;

                if (Math.abs(_bxx) == 0) {
                    _bxx = Math.abs(_bxx);
                }

                float _2bxy = 2 * b * x * y;

                if (Math.abs(_2bxy) == 0) {
                    _2bxy = Math.abs(_2bxy);
                }

                float _2bxz = 2 * b * x * z;

                if (Math.abs(_2bxz) == 0) {
                    _2bxz = Math.abs(_2bxz);
                }

                float _byy = b * y * y;

                if (Math.abs(_byy) == 0) {
                    _byy = Math.abs(_byy);
                }

                float _2byz = 2 * b * y * z;

                if (Math.abs(_2byz) == 0) {
                    _2byz = Math.abs(_2byz);
                }

                float _bzz = b * z * z;

                if (Math.abs(_bzz) == 0) {
                    _bzz = Math.abs(_bzz);
                }


                // following is for 1.4 compliant
                // otherwise, it would be : printStream.printf("%16f", b*x*x);
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


                String _2bxz_string = String.valueOf(_2bxz);
                int _2bxz_stringLength = _2bxz_string.length();
                sb = new StringBuffer(16);
                padLength = 16 - _2bxz_stringLength;

                for (int j = 0; j < padLength; j++) {
                    sb.insert(j, " ");
                }

                sb.insert(padLength, _2bxz_string);
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


                String _2byz_string = String.valueOf(_2byz);
                int _2byz_stringLength = _2byz_string.length();
                sb = new StringBuffer(16);
                padLength = 16 - _2byz_stringLength;

                for (int j = 0; j < padLength; j++) {
                    sb.insert(j, " ");
                }

                sb.insert(padLength, _2byz_string);
                printStream.print(sb.toString());


                String _bzz_string = String.valueOf(_bzz);
                int _bzz_stringLength = _bzz_string.length();
                sb = new StringBuffer(16);
                padLength = 16 - _bzz_stringLength;

                for (int j = 0; j < padLength; j++) {
                    sb.insert(j, " ");
                }

                sb.insert(padLength, _bzz_string);
                printStream.print(sb.toString());

                printStream.println();

            }

            outputStream.close();
        } catch (Exception e) {
            Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Creation of b-matrix file failed....exiting algorithm \n",
                              Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: Creation of b-matrix file failed....exiting algorithm \n");
            }

            //System.out.println("! ERROR: " + e.toString() + "\n");
            //System.out.println("! ERROR: Creation of b-matrix file failed....exiting algorithm \n");

            return false;
        }

        Preferences.debug(" - b-matrix file created : " + studyPath + "_proc" + File.separator + studyName + ".BMTXT" +
                          " \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append(" - b-matrix file created : " + studyPath + "_proc" + File.separator + studyName +
                                  ".BMTXT" + " \n");
        }

        //System.out.println(" - b-matrix file created : " + studyPath + "_proc" + File.separator + studyName + ".BMTXT" +
                           //" \n");

        return true;
    }
    
    
    
    
    
    //------methods for par/rec ----------------------------------------------------------------------------------
    
    
    
    /**
	 * This method obtains list file data mostly from the 1st slice
	 * 
	 * @return
	 */
	public boolean obtainListFileDataParRec() {
		try{
			int extents[] = image4D.getExtents();
			originalColumnsString = String.valueOf(extents[0]);
			originalRowsString = String.valueOf(extents[1]);
			numSlicesString = String.valueOf(extents[2]);
			if(fileInfoPR == null) {
				fileInfoPR = (FileInfoPARREC)image4D.getFileInfo(0);
			}
			volParameters = fileInfoPR.getVolParameters();
			String fovString = (String)volParameters.get("scn_fov");
			String[] fovs = fovString.trim().split("\\s+");
			horizontalFOVString = fovs[0];
			verticalFOVString = fovs[2];
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
			//by the number of lines in that file....if no gradient file was supplied,
			//this means the b-matrix file was provided...so copy 
			//also...might as well just read the gradient file at this point
			if((gradientFilePath == null || gradientFilePath.trim().equals("")) &&  (bmtxtFilePath == null || bmtxtFilePath.trim().equals(""))) {
				 
				Preferences.debug("! ERROR: A valid gradient file  or bmatrix file must be supplied for this dataset....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
				outputTextArea.append("! ERROR: A valid gradient file or bmatrix file must be supplied for this dataset....exiting algorithm \n");
				return false;

				
			}else {
				if(gradientFilePath == null || gradientFilePath.trim().equals("")) {
					//this means that the bmtxt file was provided instead...so...

		            // copy and rename b-matrix file
		            Preferences.debug("* b-matrix file provided \n", Preferences.DEBUG_ALGORITHM);

		            if (outputTextArea != null) {
		                outputTextArea.append("* b-matrix file provided \n");
		            }

		            //System.out.println("* b-matrix file provided \n");
		            success = copyBMatrixFile();

		            if (success == false) {
		                finalize();
		                setCompleted(true);
		                return false;
		            }
				}else {
					boolean success = readGradientFileParRec();
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
    
    
    /**
     * this method reads the gradient file
     * 
     * @return
     */
	public boolean readGradientFileParRec() {

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
    public boolean createProcDirAndImageSlicesDirParRec() {

        // create parallel proc dir to study path that will hold list file, path file, and b matrix file
        File procDir = new File(prFileDir + "_proc");

        if (!procDir.isDirectory()) {
            boolean success = new File(prFileDir + "_proc").mkdir();

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
        
        Preferences.debug(" - proc dir created : " + prFileDir + "_proc \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append(" - proc dir created : " + prFileDir + "_proc \n");
        }
        
        //create image slices dir
        imageSlicesDirPath = prFileDir + "_proc" + File.separator + studyName + "_slices";
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
        Preferences.debug(" - imageSlices dir created : " + prFileDir + "_proc" + File.separator + studyName + "_slices \n", Preferences.DEBUG_ALGORITHM);
        if (outputTextArea != null) {
            outputTextArea.append(" - imageSlices dir created : " + prFileDir + "_proc" + File.separator + studyName + "_slices \n");
        }


        return true;
    }
	
	
	
    /**
     * This method creates the list file.
     *
     * @return  boolean success
     */
    public boolean createListFileParRec() {
    	
    	try {
            File listFile = new File(prFileDir + "_proc" + File.separator + listFileName);
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
    	
    	Preferences.debug(" - list file created : " + prFileDir + "_proc" + File.separator + listFileName + " \n", Preferences.DEBUG_ALGORITHM);

    	if (outputTextArea != null) {
    		outputTextArea.append(" - list file created : " + prFileDir + "_proc" + File.separator + listFileName + " \n");
    	}
    	return true;
    }
	
	
    
    /**
     * This method creates the B-Matrix file
     * 
     * @return
     */
    public boolean createBMatrixFileParRec() {
    	int extents[] = image4D.getExtents();
    	String slice;
    	String[] values;
    	
    	try {
            StringBuffer sb;
            int padLength;
            File bMatrixFile = new File(prFileDir + "_proc" + File.separator + bmatrixFileName);
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
    	
    	
    	Preferences.debug(" - b-matrix file created : " + prFileDir + "_proc" + File.separator + bmatrixFileName + " \n", Preferences.DEBUG_ALGORITHM);

    	if (outputTextArea != null) {
    		outputTextArea.append(" - b-matrix file created : " + prFileDir + "_proc" + File.separator + bmatrixFileName + " \n");
    	}
    	
    	return true;
    }
    
	
    
    
    /**
     * This method writes out the image slices as raw float
     * 
     * @return
     */
    public boolean writeImageSlicesParRec() {
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
        String dir = prFileDir + "_proc" + File.separator + studyName + "_slices" + File.separator;
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
     * This method creates the path file
     * 
     * @return
     */
    public boolean createPathFileParRec() {
    	try {
            File listFile = new File(prFileDir + "_proc" + File.separator + pathFileName);
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
    	Preferences.debug(" - path file created : " + prFileDir + "_proc" + File.separator + pathFileName+ " \n", Preferences.DEBUG_ALGORITHM);

    	if (outputTextArea != null) {
    		outputTextArea.append(" - path file created : " + prFileDir + "_proc" + File.separator + pathFileName + " \n");
    	}
    	
    	return true;
    }
    
    
    
    
    
	//-------for both par/rec and dicom ------------------------------------------------------------------------
	
	
    //------------metods used by dicom and par-rec ------------------------------------------------------------
    
    
    
    
    /**
     * this method cleans up the proc dir if success is false and it sets the lists to null.
     */
    public void finalize() {

        // delete any of the files that were created if success is false
        if (success == false) {

            if (isProcDirCreated) {
                Preferences.debug("! Deleting .list, .path, and .BMTXT (if created)  from proc dir \n",
                                  Preferences.DEBUG_ALGORITHM);

                if (outputTextArea != null) {
                    outputTextArea.append("! Deleting .list, .path, and .BMTXT (if created)  from proc dir \n");
                }

                System.out.println("! Deleting .list, .path, and .BMTXT (if created)  from proc dir \n");
                File procDir;
                if(isDICOM) {
                	procDir = new File(studyPath + "_proc");
                }else {
                	procDir = new File(prFileDir + "_proc");
                }
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
        if(isDICOM) {
        	seriesFileInfoTreeSet = null;
        	seriesFileInfoTreeMap = null;
        }
        success = true;
    }
    
    
    /**
     * This method copies the optional b matrix file that user provided, and copies it to proc dir and renames it to the
     * correct naming syntax.
     *
     * @return boolean
     */
    public boolean copyBMatrixFile() {
        File from = new File(bmtxtFilePath);
        String pathString;
        if(isDICOM) {
        	pathString = studyPath;
        }else {
        	pathString = prFileDir;
        }
        File to = new File(pathString + "_proc" + File.separator + studyName + ".BMTXT");
        Writer out = null;
        FileInputStream fis = null;
        FileOutputStream fos = null;

        try {
            String str;
            fis = new FileInputStream(from);
            fos = new FileOutputStream(to);
            out = new OutputStreamWriter(fos);

            BufferedReader d = new BufferedReader(new InputStreamReader(fis));

            while ((str = d.readLine()) != null) {
                out.write(str + "\n");
                out.flush();
            }
        } catch (Exception e) {
            Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: copying of b-matrix to proc dir failed....exiting algorithm \n",
                              Preferences.DEBUG_ALGORITHM);

            if (outputTextArea != null) {
                outputTextArea.append("! ERROR: " + e.toString() + "\n");
                outputTextArea.append("! ERROR: copying of b-matrix to proc dir failed....exiting algorithm \n");
            }

            //System.out.println("! ERROR: " + e.toString() + "\n");
            //System.out.println("! ERROR: copying of b-matrix to proc dir failed....exiting algorithm \n");

            return false;
        } finally {

            try {

                if (out != null) {
                    out.close();
                }

                if (fos != null) {
                    fos.close();
                }

                if (fis != null) {
                    fis.close();
                }
            } catch (IOException e) { }

        }

        Preferences.debug(" - b-matrix file copied and renamed to : " + pathString + "_proc" + File.separator +
                          studyName + ".BMTXT" + " \n", Preferences.DEBUG_ALGORITHM);

        if (outputTextArea != null) {
            outputTextArea.append(" - b-matrix file copied and renamed to  : " + pathString + "_proc" + File.separator +
                                  studyName + ".BMTXT" + " \n");
        }

       //System.out.println(" - b-matrix file copied and renamed to  : " + pathString + "_proc" + File.separator +
                           //studyName + ".BMTXT" + " \n");

        return true;
    }
    
    
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	
    }
    
    
    
    //---------INNER CLASSES -------------------------------------------------------------------------------------

    //~ Inner Classes --------------------------------------------------------------------------------------------------
    /**
     * This inner class is used to sort the list by instance number and vol. the vol is determined by the filename
     */
    private class InstanceNumberVolComparator implements Comparator<Object> {

        /**
         *
         * @param   oA  
         * @param   oB  
         *
         * @return  int
         */
        public int compare(Object oA, Object oB) {
            String[] aA = (String[]) oA;
            String[] aB = (String[]) oB;
            String instancNoA_String = ((String) aA[0]).trim();

            if (instancNoA_String == null) {
                instancNoA_String = "";
            }

            String instancNoB_String = ((String) aB[0]).trim();

            if (instancNoB_String == null) {
                instancNoB_String = "";
            }

            int instanceNoA = new Integer(instancNoA_String).intValue();
            int instanceNoB = new Integer(instancNoB_String).intValue();


            String filenameA = ((String) aA[3]).trim();
            String filenameB = ((String) aB[3]).trim();

            String volStringA = filenameA.substring(filenameA.lastIndexOf("volume") + 6, filenameA.lastIndexOf('.'));
            int volNumberA = new Integer(volStringA).intValue();

            String volStringB = filenameB.substring(filenameB.lastIndexOf("volume") + 6, filenameB.lastIndexOf('.'));
            int volNumberB = new Integer(volStringB).intValue();


            if (volNumberA == volNumberB) {

                if (instanceNoA < instanceNoB) {
                    return -1;
                }

                if (instanceNoA > instanceNoB) {
                    return 1;
                }
            }

            if (volNumberA < volNumberB) {
                return -1;
            }

            if (volNumberA > volNumberB) {
                return 1;
            }

            // this should never happen
            return -1;
        }
    }


    
    /**
	 * This inner class is used to sort
	 * the list by instance number
	 */
	private class InstanceNumberComparator implements Comparator<Object> {
		public int compare(Object oA, Object oB) {
			String[] aA = (String[]) oA;
			String[] aB = (String[]) oB;
			String instancNoA_String = ((String)aA[0]).trim();
			if (instancNoA_String == null) {
				instancNoA_String = "";
			}
			String instancNoB_String = ((String)aB[0]).trim();
			if (instancNoB_String == null) {
				instancNoB_String = "";
			}
			if ((!instancNoA_String.equals("")) && (!instancNoB_String.equals(""))) {
				int instanceNoA = new Integer(instancNoA_String).intValue();
				int instanceNoB = new Integer(instancNoB_String).intValue();
				if (instanceNoA < instanceNoB) {
					return -1;
				}
				if (instanceNoA > instanceNoB) {
					return 1;
				}
			}
			return 0;
		}
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

}
