import gov.nih.mipav.model.algorithms.AlgorithmBase;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.*;
import java.util.regex.*;


public class PlugInAlgorithmBrainSubcortical extends AlgorithmBase {

    /** ImageA instance. */
	private BrainSubcorticalInstance imageAreader;
    
	/** ImageB instance. */
	private BrainSubcorticalInstance imageBreader;
    
	/** Brain subcortical instances vector. */
    private Vector<BrainSubcorticalInstance> instances = new  Vector<BrainSubcorticalInstance>();
    
    /** Case number directory. */
    private Vector<String> caseVector = new Vector<String>();
    
    /** image repository input directory. */
    private String inputDir;

    /** saved images and report output directory. */
    private String outputDir;
    
    /** case comparison directory. */
    private String caseCompareDir;
    
    /** vector to record which subsections to do registration. */
    private Vector<Integer> regSection;
    
    /** Reference to the brain subcortical plugin dialog. */
    private PlugInDialogBrainSubcortical parentDialog;
    
    /** Vector to record which two cases to compare. */ 
    private Vector<caseComparison> cases = new Vector<caseComparison>();
    
    /**
     * Algorithm for brain subcortical registration
     * @param _inputDir    image repository input directory
     * @param _outputDir    saved images ( registered, comparison, and report) directory. 
     * @param _caseCompareDir   case comparison directory.
     * @param _parentDialog    reference to parent dialog, for file info saving. 
     * @param _regSection    vector to flag which subsection to do registration. 
     */
    public PlugInAlgorithmBrainSubcortical(String _inputDir, String _outputDir, String _caseCompareDir, PlugInDialogBrainSubcortical _parentDialog, Vector<Integer> _regSection ) {
    	inputDir = _inputDir;
    	outputDir = _outputDir;
    	caseCompareDir = _caseCompareDir;
    	parentDialog = _parentDialog;
    	regSection = _regSection;
    }
    
    /**
     * Processing comamnd line, construction image instances, then do registration and comparison. 
     */
	public void runAlgorithm() {
		
		processInputDir();
    	processCaseComparison();
        readInstances();
		
		for (int i = 0; i < cases.size(); i++) {

			try {
				imageAreader = getCase(cases.get(i).firstCase);
				imageBreader = getCase(cases.get(i).secondCase);

				// step0. validate registered subsection
				validate();

				// Step1. load the image file
				loadImages();

				// Step2. threshold the image into sub-cortical structures
				threshold();

				// Step3. Image registration on the related sub structures.
				registration();

				// Step4. Save threshold images and registered images.
				saveImages();

				// Step5. Comparison
				doComparison();

				// Step6. Static data reporting
				printReport();

				// Step 7. dispose memory
				dispose();

			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		System.gc();
		System.out.println("PlugIn Brain Subcortical Analysis Finish Running.  MIPAV Quits. ");
		System.exit(0);
	}
    
    /**
     * The brain MRI image datasets are provided by Francois.   This method reads the directory, creates each case 
     * as instance. 
     * @return
     */
    private boolean readInstances() {
    	int start; 
    	int end;
    	int index;
    	String numberRegex = "[0123456789]{1,}";
    	Pattern p = Pattern.compile(numberRegex);
    	for ( int i = 0; i < caseVector.size(); i++ ) {
    		String dir = caseVector.get(i);
    		
    		index = dir.lastIndexOf(File.separator);
    		String directory = new String(dir.substring(0, index+1));
    		String fileName = new String(dir.substring(index+1, dir.length()));
    		
    		Matcher m = p.matcher(dir);
    		if ( m.find() ) {
    			start = m.start();
    			end = m.end();
    			int caseNumber = Integer.valueOf(dir.substring(start, end));	
    			BrainSubcorticalInstance temp = new BrainSubcorticalInstance(caseNumber, fileName, directory, regSection, parentDialog); 
    			instances.add(temp);
    		
    		}
    	}
    	
    	return true;
    }
    
    /**
     * For each case, find the corresponding aseg.mgz file. 
     * @param dir
     */
    private void processDir(File dir) {

        // System.out.print( (dir.isDirectory() ? "[D] : " : "[F] : "));
        // System.out.println(dir);
    	String dirName = dir.toString();
    	int begin = dirName.lastIndexOf(File.separator)+1;
    	int end = dirName.length();
    	// System.err.println(dirName.substring(begin, end));
        if ( dirName.substring(begin, end).equals("aseg.mgz")) {
        	caseVector.add(dir.toString());
        }

    }

    /** 
     * Recursively searching 'aseg.mgz' file for each case. 
     * @param dir   Brain subcortical images repository directory. 
     */
    private void traverse(File dir) {

        processDir(dir);

        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
            	// System.err.println(dir.getAbsolutePath() + children[i]);
                traverse(new File(dir, children[i]));
            }
        }

    }
    
    /** 
     * Load images for the two cases.
     */
    private void loadImages() {
    	imageAreader.readImage();
    	imageBreader.readImage();
    	
    }
    
    
    /**
     * Search the case with the given caseNumber
     * @param caseNumber  case number
     * @return  Brain subcortical instance
     */
    private BrainSubcorticalInstance getCase(int caseNumber) {
    	for ( int i = 0; i < instances.size(); i++ ) {
    		if ( instances.get(i).caseNumber == caseNumber ) {
    			return instances.get(i);
    		}
    	}
    	return null;
    }
    
   
	/**
	 * Validate which subsection to do registration. 
	 */
	public void validate() {
		imageAreader.validateRegSubSection();
		imageBreader.validateRegSubSection();
	}
	
	/**
	 * Dispose memory. 
	 */
	public void dispose() {
		imageAreader.disposeLocal();
		imageBreader.disposeLocal();
	}
    
    /**
     * print statistic report with given output directory
     */
    public void printReport() {
    	imageAreader.printReport(outputDir + File.separator + imageAreader.getCaseNumber() + "vs" + imageBreader.getCaseNumber());	
    }
    
    /**
     * Compare the original single out subsection with the corresponding registered image subsection. 
     */
    public void doComparison() {
    	imageAreader.originImageThresholdBinary();
    	imageAreader.registeredImageThresholdBinary();   
        imageAreader.statisticsDataGeneration(outputDir + File.separator + imageAreader.getCaseNumber() + "vs" + imageBreader.getCaseNumber());
    }
    
    /**
     * Save each theshold subsection images, also save the registered images. 
     */
    public void saveImages() {
    	imageAreader.saveImages();
    	imageBreader.saveImages();
    	imageAreader.saveRegisteredImages(outputDir + File.separator + imageAreader.getCaseNumber() + "vs" + imageBreader.getCaseNumber());
    	
    }
    
    /**
     * Register the theshold subsection between the two given datasets. 
     */
    public void registration() {
    	imageAreader.registration(imageBreader);
    }
    

    /** 
     * threshold subsections
     */
    public void threshold() {
    	imageAreader.threshold();
    	imageBreader.threshold();
    }

    /**
     * process from command line input directory. 
     */
    public void processInputDir() {
    	File fileDir = new File(inputDir);
        System.err.println(inputDir);
        traverse(fileDir);
    }
    
    
    /**
     * Reading the case comparison file, and set up the comparison case number. 
     */
    public void processCaseComparison() {
    	try {
    		FileReader vsDirAbsPath = new FileReader(caseCompareDir);
    		BufferedReader inputStream = new BufferedReader(vsDirAbsPath);
    		String line = null;
    		String []tokens;
    		int firstCase;
    		int secondCase;
    		while ( ( line = inputStream.readLine() ) != null ) {
    			 tokens = line.split("\\s+");
    			 firstCase = Integer.valueOf(tokens[0]);
    			 secondCase = Integer.valueOf(tokens[2]);
    			 cases.add(new caseComparison(firstCase, secondCase));
    		}
    		
    		
    	} catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    
	
}



/**
* Brain subcortical analysis is running from the command line.   User types command line such as,
* >mipav -p PlugInBrainSubcorticalRegistration -inDir F:\BrainMRIData -outDir F:\BrainMRIData  
* -Reg LH LA RP	
* 
* -p indicates running as a plugin
* -inDir specify the input brain MRI images repository directory. 
* -outDir specify the output directory for registered images, color RGB comparison images, and statics report. 
* -Reg flag indicates which subcortical sections to do registration. 
*  
*/
class BrainSubcorticalInstance implements AlgorithmInterface {
	
	/** Brain subcortical image name, aseg.mgz */
	public String fileName = new String();
	
	/** Brain subcortical image directory */
	public String directory= new String();
	
	/** Brain subcortical case number */
	public int caseNumber;

	/** Brain subcortical image reference. */
	private ModelImage myImage;
	
	/** Vector array to indicates which subsections to do registration. */
	private Vector<Integer> regSection;
	
	/** Vector to store the statistics data for current registered subsections. */
    public 	Vector<StatisticsData> myData= new Vector<StatisticsData>();

    /** Same data-type, binary, or unsigned byte mask. */
   private int outputType = 0;

   /** Flags to indicates which subsection to do registration. */
   private static boolean flagLeftHippocampus = false;
   private static boolean flagRightHippocampus = false;

   private static boolean flagLeftAmygdala = false;
   private static boolean flagRightAmygdala = false;

   private static boolean flagLeftCaudate = false;
   private static boolean flagRightCaudate = false;

   private static boolean flagLeftPutamen = false;
   private static boolean flagRightPutamen = false;

   private static boolean flagLeftGlobusPallidus = false;
   private static boolean flagRightGlobusPallidus = false;

   private static boolean flagLeftThalamus = false;
   private static boolean flagRightThalamus = false;
   
   /** threshold constants value of color labeling.  */ 
   private static float LeftHippocampus = 17.0f;
   private static float RightHippocampus = 53.0f;

   private static float LeftAmygdala = 18.0f;
   private static float RightAmygdala = 54.0f;

   private static float LeftCaudate = 11.0f;
   private static float RightCaudate = 50.0f;

   private static float LeftPutamen = 12.0f;
   private static float RightPutamen = 51.0f;

   private static float LeftGlobusPallidus = 13.0f;
   private static float RightGlobusPallidus = 52.0f;

   private static float LeftThalamus = 10.0f;
   private static float RightThalamus = 49.0f;

   /** threshold algorithm to single out each subsection. */	
   private AlgorithmThresholdDual algorThreshold_LeftHippocampus;
   private 	AlgorithmThresholdDual algorThreshold_RightHippocampus;
	 
   private 	AlgorithmThresholdDual algorThreshold_LeftAmygdala;
   private 	AlgorithmThresholdDual algorThreshold_RightAmygdala;

   private AlgorithmThresholdDual algorThreshold_LeftCaudate;
   private AlgorithmThresholdDual algorThreshold_RightCaudate;

   private AlgorithmThresholdDual algorThreshold_LeftPutamen;
   private AlgorithmThresholdDual algorThreshold_RightPutamen;

   private AlgorithmThresholdDual algorThreshold_LeftGlobusPallidus;
   private AlgorithmThresholdDual algorThreshold_RightGlobusPallidus;

   private AlgorithmThresholdDual algorThreshold_LeftThalamus;
   private AlgorithmThresholdDual algorThreshold_RightThalamus;
	
   /** registration algorithm for each subsections. */
   private AlgorithmRegOAR3D regLeftHippocampus;
   private AlgorithmRegOAR3D regRightHippocampus;
   
   private AlgorithmRegOAR3D regLeftAmygdala;
   private AlgorithmRegOAR3D regRightAmygdala;

   private AlgorithmRegOAR3D regLeftCaudate;
   private AlgorithmRegOAR3D regRightCaudate;

   private AlgorithmRegOAR3D regLeftPutamen;
   private AlgorithmRegOAR3D regRightPutamen;

   private AlgorithmRegOAR3D regLeftGlobusPallidus;
   private AlgorithmRegOAR3D regRightGlobusPallidus;

   private AlgorithmRegOAR3D regLeftThalamus;
   private AlgorithmRegOAR3D regRightThalamus;
   
   
   /** model images for each subsection after thresholding. */ 
   public ModelImage image_LeftHippocampus;
   public ModelImage image_RightHippocampus;
	 
   public ModelImage image_LeftAmygdala;
   public ModelImage image_RightAmygdala;

   public ModelImage image_LeftCaudate;
   public ModelImage image_RightCaudate;

   public ModelImage image_LeftPutamen;
   public ModelImage image_RightPutamen;

   public ModelImage image_LeftGlobusPallidus;
   public ModelImage image_RightGlobusPallidus;

   public ModelImage image_LeftThalamus;
   public ModelImage image_RightThalamus;
	

   /** registered resulting images for each subsections. */
   public ModelImage regImage_LeftHippocampus;
   public ModelImage regImage_RightHippocampus;
	 
   public ModelImage regImage_LeftAmygdala;
   public ModelImage regImage_RightAmygdala;

   public ModelImage regImage_LeftCaudate;
   public ModelImage regImage_RightCaudate;

   public ModelImage regImage_LeftPutamen;
   public ModelImage regImage_RightPutamen;

   public ModelImage regImage_LeftGlobusPallidus;
   public ModelImage regImage_RightGlobusPallidus;

   public ModelImage regImage_LeftThalamus;
   public ModelImage regImage_RightThalamus;

   /** Parent dialog reference. */
   public PlugInDialogBrainSubcortical parentDialog;
   
   /** 2nd dataset instance */
   public BrainSubcorticalInstance instanceB;

   
   /** images comparison result images for the two datasets. */ 
   public ModelImage image_LeftHippocampus_binary;
   public ModelImage image_RightHippocampus_binary;
   
   public ModelImage image_LeftAmygdala_binary;
   public ModelImage image_RightAmygdala_binary;

   public ModelImage image_LeftCaudate_binary;
   public ModelImage image_RightCaudate_binary;

   public ModelImage image_LeftPutamen_binary;
   public ModelImage image_RightPutamen_binary;

   public ModelImage image_LeftGlobusPallidus_binary;
   public ModelImage image_RightGlobusPallidus_binary;

   public ModelImage image_LeftThalamus_binary;
   public ModelImage image_RightThalamus_binary;   
   
   /** Threshold alogirthm reference of each subsection */ 
   private AlgorithmThresholdDual algorThreshold_LeftHippocampus_binary;
   private 	AlgorithmThresholdDual algorThreshold_RightHippocampus_binary;
   
   private 	AlgorithmThresholdDual algorThreshold_LeftAmygdala_binary;
   private 	AlgorithmThresholdDual algorThreshold_RightAmygdala_binary;

   private AlgorithmThresholdDual algorThreshold_LeftCaudate_binary;
   private AlgorithmThresholdDual algorThreshold_RightCaudate_binary;

   private AlgorithmThresholdDual algorThreshold_LeftPutamen_binary;
   private AlgorithmThresholdDual algorThreshold_RightPutamen_binary;

   private AlgorithmThresholdDual algorThreshold_LeftGlobusPallidus_binary;
   private AlgorithmThresholdDual algorThreshold_RightGlobusPallidus_binary;

   private AlgorithmThresholdDual algorThreshold_LeftThalamus_binary;
   private AlgorithmThresholdDual algorThreshold_RightThalamus_binary;

   /** Binary images for each subsection */ 
   public ModelImage regImage_LeftHippocampus_binary;
   public ModelImage regImage_RightHippocampus_binary;
   
   public ModelImage regImage_LeftAmygdala_binary;
   public ModelImage regImage_RightAmygdala_binary;

   public ModelImage regImage_LeftCaudate_binary;
   public ModelImage regImage_RightCaudate_binary;

   public ModelImage regImage_LeftPutamen_binary;
   public ModelImage regImage_RightPutamen_binary;

   public ModelImage regImage_LeftGlobusPallidus_binary;
   public ModelImage regImage_RightGlobusPallidus_binary;

   public ModelImage regImage_LeftThalamus_binary;
   public ModelImage regImage_RightThalamus_binary;   
   
   /** Binary image for the registered subsection. */
   private AlgorithmThresholdDual algorThreshold_LeftHippocampus_reg_binary;
   private 	AlgorithmThresholdDual algorThreshold_RightHippocampus_reg_binary;
   
   private 	AlgorithmThresholdDual algorThreshold_LeftAmygdala_reg_binary;
   private 	AlgorithmThresholdDual algorThreshold_RightAmygdala_reg_binary;

   private AlgorithmThresholdDual algorThreshold_LeftCaudate_reg_binary;
   private AlgorithmThresholdDual algorThreshold_RightCaudate_reg_binary;

   private AlgorithmThresholdDual algorThreshold_LeftPutamen_reg_binary;
   private AlgorithmThresholdDual algorThreshold_RightPutamen_reg_binary;

   private AlgorithmThresholdDual algorThreshold_LeftGlobusPallidus_reg_binary;
   private AlgorithmThresholdDual algorThreshold_RightGlobusPallidus_reg_binary;

   private AlgorithmThresholdDual algorThreshold_LeftThalamus_reg_binary;
   private AlgorithmThresholdDual algorThreshold_RightThalamus_reg_binary;
   
   /*
	 * Left Hippocampus  ---   LH
	 * Right Hippocampus ---   RH
	 * Left Amygdala     ---   LA
	 * Right Amygdala    ---   RA
	 * Left Candate      ---   LC
	 * Right Candate     ---   RC
	 * Left Putamen      ---   LP
	 * Right Putaman     ---   RP
	 * Left globus pallidess  --- LG
	 * Right globus pallidees --- RG
	 * Left Thalomus     ---   LT
	 * Right Thalomus    ---   RT 
	 */
   public static int LH = 0; 
   public static int RH = 1; 
   public static int LA = 2; 
   public static int RA = 3; 
   public static int LC = 4; 
   public static int RC = 5; 
   public static int LP = 6; 
   public static int RP = 7; 
   public static int LG = 8; 
   public static int RG = 9; 
   public static int LT = 10; 
   public static int RT = 11;
  
   /** Constructor for the single out brain subcortical instance. 
    * 
    *  Each instance is the reference to one brain MRI images.  Each instance also 
    *  contains the reference of the 2nd brain MRI image dataset for comparison. 
    */  
	 public BrainSubcorticalInstance(int _caseNumber, String _fileName, String _directory, Vector<Integer> _regSection, PlugInDialogBrainSubcortical parent) {
		 	caseNumber = _caseNumber;
		 	fileName = _fileName;
	        directory = _directory;
	        regSection = _regSection;
	        parentDialog = parent;
	 }

	 /** 
	  * Validate which subsections need to do registration. 
	  */
	 public void validateRegSubSection() {
		 int size = regSection.size();
		 int value;
		 for ( int i = 0; i < size; i++ ) {
			 value = (Integer)(regSection.get(i));
			 
			 if( value == LH ) {
				 flagLeftHippocampus = true;
			 } else if ( value == RH ) {
				 flagRightHippocampus = true;
			 } else if ( value == LA ) {
				 flagLeftAmygdala = true;
			 } else if ( value == RA ) {
				 flagRightAmygdala = true;
			 } else if ( value == LC ) {
				 flagLeftCaudate = true;
			 } else if ( value == RC ) {
				 flagRightCaudate = true;
			 } else if ( value == LP ) {
				 flagLeftPutamen = true;
			 } else if ( value == RP ) {
				 flagRightPutamen = true;
			 } else if ( value == LG ) {
				 flagLeftGlobusPallidus = true;
			 } else if ( value == RG ) {
				 flagRightGlobusPallidus = true;
			 } else if ( value == LT ) {
				 flagLeftThalamus = true;
			 } else if ( value == RT ) {
				 flagRightThalamus = true;
			 }
			 
		 }
	 }
	 
	 /** 
	  * Clean memory 
	  */
	public void disposeLocal() {

		// image original
		if ( image_LeftHippocampus != null ) {
			image_LeftHippocampus.disposeLocal();
			image_LeftHippocampus = null;
		}
		
		if ( image_RightHippocampus != null ) {
			image_RightHippocampus.disposeLocal();
			image_RightHippocampus = null;
		}

		if ( image_LeftAmygdala != null ) {
			image_LeftAmygdala.disposeLocal();
			image_LeftAmygdala = null;
		}
		
		if ( image_RightAmygdala != null ) {
			image_RightAmygdala.disposeLocal();
			image_RightAmygdala = null;
		}
		
		if ( image_LeftCaudate != null ) {
			image_LeftCaudate.disposeLocal();
			image_LeftCaudate = null;
		}
		
		if ( image_RightCaudate != null ) {
			image_RightCaudate.disposeLocal();
			image_RightCaudate = null;
		}

		if ( image_LeftPutamen != null ) {
			image_LeftPutamen.disposeLocal();
			image_LeftPutamen = null;
		}
		
		if ( image_RightPutamen != null ) {
			image_RightPutamen.disposeLocal();
			image_RightPutamen = null;
		}

		if ( image_LeftGlobusPallidus != null ) {
			image_LeftGlobusPallidus.disposeLocal();
			image_LeftGlobusPallidus = null;
		}
		
		if ( image_RightGlobusPallidus != null ) {
			image_RightGlobusPallidus.disposeLocal();
			image_RightGlobusPallidus = null;
		}
		
		if ( image_LeftThalamus != null ) {
			image_LeftThalamus.disposeLocal();
			image_LeftThalamus = null;
		}
		
		if ( image_RightThalamus != null ) {
			image_RightThalamus.disposeLocal();
			image_RightThalamus = null;
		}

		// registered image
		if ( regImage_LeftHippocampus != null ) {
			regImage_LeftHippocampus.disposeLocal();
			regImage_LeftHippocampus = null;
		}
		
		if ( regImage_RightHippocampus != null ) {
			regImage_RightHippocampus.disposeLocal();
			regImage_RightHippocampus = null;
		}

		if ( regImage_LeftAmygdala != null ) {
			regImage_LeftAmygdala.disposeLocal();
			regImage_LeftAmygdala = null;
		}
		
		if ( regImage_RightAmygdala != null ) {
			regImage_RightAmygdala.disposeLocal();
			regImage_RightAmygdala = null;
		}

		if ( regImage_LeftCaudate != null ) {
			regImage_LeftCaudate.disposeLocal();
			regImage_LeftCaudate = null;
		}

		if ( regImage_RightCaudate != null ) {
			regImage_RightCaudate.disposeLocal();
			regImage_RightCaudate = null;
		}

		
		if( regImage_LeftPutamen != null ) {
			regImage_LeftPutamen.disposeLocal();
			regImage_LeftPutamen = null;
		}
		
		if ( regImage_RightPutamen != null ) {
			regImage_RightPutamen.disposeLocal();
			regImage_RightPutamen = null;
		}

		if ( regImage_LeftGlobusPallidus != null ) {
			regImage_LeftGlobusPallidus.disposeLocal();
			regImage_LeftGlobusPallidus = null;
		}
		
		if ( regImage_RightGlobusPallidus != null ) {
			regImage_RightGlobusPallidus.disposeLocal();
			regImage_RightGlobusPallidus = null;
		}
		
		if ( regImage_LeftThalamus != null ) {
			regImage_LeftThalamus.disposeLocal();
			regImage_LeftThalamus = null;
		}
		
		if ( regImage_RightThalamus != null ) {
			regImage_RightThalamus.disposeLocal();
			regImage_RightThalamus = null;
		}

		// image in binary
		if ( image_LeftHippocampus_binary != null ) {
			image_LeftHippocampus_binary.disposeLocal();
			image_LeftHippocampus_binary = null;
		}
		
		if ( image_RightHippocampus_binary != null ) {
			image_RightHippocampus_binary.disposeLocal();
			image_RightHippocampus_binary = null;
		}

		if ( image_LeftAmygdala_binary != null ) {
			image_LeftAmygdala_binary.disposeLocal();
			image_LeftAmygdala_binary = null;
		}
		
		if ( image_RightAmygdala_binary != null ) {
			image_RightAmygdala_binary.disposeLocal();
			image_RightAmygdala_binary = null;
		}

		if ( image_LeftCaudate_binary != null ) {
			image_LeftCaudate_binary.disposeLocal();
			image_LeftCaudate_binary = null;
		}
		
		if ( image_RightCaudate_binary != null ) {
			image_RightCaudate_binary.disposeLocal();
			image_RightCaudate_binary = null;
		}

        if ( image_LeftPutamen_binary != null ) {		
        	image_LeftPutamen_binary.disposeLocal();
        	image_LeftPutamen_binary = null;
        }
		
        if ( image_RightPutamen_binary != null ) {
        	image_RightPutamen_binary.disposeLocal();
        	image_RightPutamen_binary = null;
        }
        
        if ( image_LeftGlobusPallidus_binary != null ) {
        	image_LeftGlobusPallidus_binary.disposeLocal();
        	image_LeftGlobusPallidus_binary = null;
        }
        
        if ( image_RightGlobusPallidus_binary != null ) {
        	image_RightGlobusPallidus_binary.disposeLocal();
        	image_RightGlobusPallidus_binary = null;
        }

        if ( image_LeftThalamus_binary != null ) {		
        	image_LeftThalamus_binary.disposeLocal();
        	image_LeftThalamus_binary = null;
        }
		
        if ( image_RightThalamus_binary != null ) {
        	image_RightThalamus_binary.disposeLocal();
        	image_RightThalamus_binary = null;
        }

		// Registered image in binary
        if ( regImage_LeftHippocampus_binary != null ) {
        	regImage_LeftHippocampus_binary.disposeLocal();
        	regImage_LeftHippocampus_binary = null;
        }
		
        if ( regImage_RightHippocampus_binary != null ) {
        	regImage_RightHippocampus_binary.disposeLocal();
        	regImage_RightHippocampus_binary = null;
        }
        
        if ( regImage_LeftAmygdala_binary != null ) {
        	regImage_LeftAmygdala_binary.disposeLocal();
        	regImage_LeftAmygdala_binary = null;
        }
        
        if ( regImage_RightAmygdala_binary != null ) {
        	regImage_RightAmygdala_binary.disposeLocal();
        	regImage_RightAmygdala_binary = null;
        }

        if ( regImage_LeftCaudate_binary != null ) {
        	regImage_LeftCaudate_binary.disposeLocal();
        	regImage_LeftCaudate_binary = null;
        }
		
        if ( regImage_RightCaudate_binary != null ) {
        	regImage_RightCaudate_binary.disposeLocal();
        	regImage_RightCaudate_binary = null;
        }

        if ( regImage_LeftPutamen_binary != null ) {
        	regImage_LeftPutamen_binary.disposeLocal();
        	regImage_LeftPutamen_binary = null;
        }
		
        if ( regImage_RightPutamen_binary != null ) {
        	regImage_RightPutamen_binary.disposeLocal();
        	regImage_RightPutamen_binary = null;
        }

        if ( regImage_LeftGlobusPallidus_binary != null ) {
        	regImage_LeftGlobusPallidus_binary.disposeLocal();
        	regImage_LeftGlobusPallidus_binary = null;
        }
		
        if ( regImage_RightGlobusPallidus_binary != null ) {
        	regImage_RightGlobusPallidus_binary.disposeLocal();
        	regImage_RightGlobusPallidus_binary = null;
        }

        if ( regImage_LeftThalamus_binary != null ) {
        	regImage_LeftThalamus_binary.disposeLocal();
        	regImage_LeftThalamus_binary = null;
        }
		
        if ( regImage_RightThalamus_binary != null ) {
        	regImage_RightThalamus_binary.disposeLocal();
        	regImage_RightThalamus_binary = null;
        }

        if ( myImage != null ) {
        	myImage.disposeLocal();
        	myImage = null;
        }
         
		myData.clear();
		
		System.gc();

	}
	
	/** 
	 * Thredhold the brain subcortical image to each subsection. 
	 */
	 public void threshold() {
	
		 float[] thresholds = new float[2];

	       

	        float fillValue = 0;
	        boolean isInverse = false;
	        boolean regionFlag = true;


		try {

			float thres1 = LeftHippocampus; // lowerThres;
			float thres2 = LeftHippocampus; // upperThres;

			// Left Hippocampus
			if (flagLeftHippocampus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftHippocampus = (ModelImage) myImage.clone();
				image_LeftHippocampus.setType(myImage.getType());
				image_LeftHippocampus.setImageName(myImage.getImageName()
						+ "_leftHippocampus");
				algorThreshold_LeftHippocampus = new AlgorithmThresholdDual(
						image_LeftHippocampus, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_LeftHippocampus.addListener(this);
				algorThreshold_LeftHippocampus.run();
			}

			// Right Hippocampus ****
			thres1 = RightHippocampus; // lowerThres;
			thres2 = RightHippocampus; // upperThres;

			if (flagRightHippocampus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightHippocampus = (ModelImage) myImage.clone();
				image_RightHippocampus.setType(myImage.getType());
				image_RightHippocampus.setImageName(myImage.getImageName()
						+ "_rightHippocampus");
				algorThreshold_RightHippocampus = new AlgorithmThresholdDual(
						image_RightHippocampus, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_RightHippocampus.addListener(this);
				algorThreshold_RightHippocampus.run();
			}

			// Left Amygdala ***
			thres1 = LeftAmygdala; // lowerThres;
			thres2 = LeftAmygdala; // upperThres;
			if (flagLeftAmygdala) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftAmygdala = (ModelImage) myImage.clone();
				image_LeftAmygdala.setType(myImage.getType());
				image_LeftAmygdala.setImageName(myImage.getImageName()
						+ "_LeftAmygdala");
				algorThreshold_LeftAmygdala = new AlgorithmThresholdDual(
						image_LeftAmygdala, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_LeftAmygdala.addListener(this);
				algorThreshold_LeftAmygdala.run();
			}

			// Right Amygdala ***
			thres1 = RightAmygdala; // lowerThres;
			thres2 = RightAmygdala; // upperThres;

			if (flagRightAmygdala) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightAmygdala = (ModelImage) myImage.clone();
				image_RightAmygdala.setType(myImage.getType());
				image_RightAmygdala.setImageName(myImage.getImageName()
						+ "_RightAmygdala");
				algorThreshold_RightAmygdala = new AlgorithmThresholdDual(
						image_RightAmygdala, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_RightAmygdala.addListener(this);
				algorThreshold_RightAmygdala.run();
			}

			// Left Caudate ***
			thres1 = LeftCaudate; // lowerThres;
			thres2 = LeftCaudate; // upperThres;

			if (flagLeftCaudate) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftCaudate = (ModelImage) myImage.clone();
				image_LeftCaudate.setType(myImage.getType());
				image_LeftCaudate.setImageName(myImage.getImageName()
						+ "_LeftCaudate");
				algorThreshold_LeftCaudate = new AlgorithmThresholdDual(
						image_LeftCaudate, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_LeftCaudate.addListener(this);
				algorThreshold_LeftCaudate.run();
			}

			// RightCaudate ***
			thres1 = RightCaudate; // lowerThres;
			thres2 = RightCaudate; // upperThres;

			if (flagRightCaudate) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightCaudate = (ModelImage) myImage.clone();
				image_RightCaudate.setType(myImage.getType());
				image_RightCaudate.setImageName(myImage.getImageName()
						+ "_RightCaudate");
				algorThreshold_RightCaudate = new AlgorithmThresholdDual(
						image_RightCaudate, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_RightCaudate.addListener(this);
				algorThreshold_RightCaudate.run();
			}

			// Left Putamen ***
			thres1 = LeftPutamen; // lowerThres;
			thres2 = LeftPutamen; // upperThres;

			if (flagLeftPutamen) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftPutamen = (ModelImage) myImage.clone();
				image_LeftPutamen.setType(myImage.getType());
				image_LeftPutamen.setImageName(myImage.getImageName()
						+ "_LeftPutamen");
				algorThreshold_LeftPutamen = new AlgorithmThresholdDual(
						image_LeftPutamen, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_LeftPutamen.addListener(this);
				algorThreshold_LeftPutamen.run();
			}

			// Right Putamen ***
			thres1 = RightPutamen; // lowerThres;
			thres2 = RightPutamen; // upperThres;
			if (flagRightPutamen) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightPutamen = (ModelImage) myImage.clone();
				image_RightPutamen.setType(myImage.getType());
				image_RightPutamen.setImageName(myImage.getImageName()
						+ "_RightPutamen");
				algorThreshold_RightPutamen = new AlgorithmThresholdDual(
						image_RightPutamen, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_RightPutamen.addListener(this);
				algorThreshold_RightPutamen.run();
			}
			// Left GlobusPallidus ***
			thres1 = LeftGlobusPallidus; // lowerThres;
			thres2 = LeftGlobusPallidus; // upperThres;

			if (flagLeftGlobusPallidus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftGlobusPallidus = (ModelImage) myImage.clone();
				image_LeftGlobusPallidus.setType(myImage.getType());
				image_LeftGlobusPallidus.setImageName(myImage.getImageName()
						+ "_LeftGlobusPallidus");
				algorThreshold_LeftGlobusPallidus = new AlgorithmThresholdDual(
						image_LeftGlobusPallidus, myImage, thresholds,
						fillValue, outputType, regionFlag, isInverse);
				algorThreshold_LeftGlobusPallidus.addListener(this);
				algorThreshold_LeftGlobusPallidus.run();
			}

			// Right GlobusPallidus ***
			thres1 = RightGlobusPallidus; // lowerThres;
			thres2 = RightGlobusPallidus; // upperThres;

			if (flagRightGlobusPallidus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightGlobusPallidus = (ModelImage) myImage.clone();
				image_RightGlobusPallidus.setType(myImage.getType());
				image_RightGlobusPallidus.setImageName(myImage.getImageName()
						+ "_RightGlobusPallidus");
				algorThreshold_RightGlobusPallidus = new AlgorithmThresholdDual(
						image_RightGlobusPallidus, myImage, thresholds,
						fillValue, outputType, regionFlag, isInverse);
				algorThreshold_RightGlobusPallidus.addListener(this);
				algorThreshold_RightGlobusPallidus.run();
			}

			// Left Thalamus ***
			thres1 = LeftThalamus; // lowerThres;
			thres2 = LeftThalamus; // upperThres;

			if (flagLeftThalamus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftThalamus = (ModelImage) myImage.clone();
				image_LeftThalamus.setType(myImage.getType());
				image_LeftThalamus.setImageName(myImage.getImageName()
						+ "_LeftThalamus");
				algorThreshold_LeftThalamus = new AlgorithmThresholdDual(
						image_LeftThalamus, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_LeftThalamus.addListener(this);
				algorThreshold_LeftThalamus.run();
			}

			// Right Thalamus ***
			thres1 = RightThalamus; // lowerThres;
			thres2 = RightThalamus; // upperThres;

			if (flagRightThalamus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightThalamus = (ModelImage) myImage.clone();
				image_RightThalamus.setType(myImage.getType());
				image_RightThalamus.setImageName(myImage.getImageName()
						+ "_RightThalamus");
				algorThreshold_RightThalamus = new AlgorithmThresholdDual(
						image_RightThalamus, myImage, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_RightThalamus.addListener(this);
				algorThreshold_RightThalamus.run();
			}

		} catch (OutOfMemoryError x) {
	            MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

	            return;
	        }
	 }
	 
	 /**
	  * Register each subsection with the 2nd dataset.
	  * @param _instanceB   reference to the 2nd dataset. 
	  */
	public void registration(BrainSubcorticalInstance _instanceB) {
		int cost = 1;
		int DOF = 6;
		int interp = 0;
		float rotateBeginX = 10.0f;
		float rotateEndX = -10.0f;
		float coarseRateX = 10.0f;
		float fineRateX = 3.0f;
		float rotateBeginY = 10.0f;
		float rotateEndY = -10.0f;
		float coarseRateY = 10.0f;
		float fineRateY = 3.0f;
		float rotateBeginZ = 10.0f;
		float rotateEndZ = -10.0f;
		float coarseRateZ = 10.0f;
		float fineRateZ = 3.0f;
		boolean maxOfMinResol = true;
		boolean doSubsample = true;
		boolean doMultiThread = true;
		boolean fastMode = true;
		int maxIterations = 2;
		int numMinima = 3;
		boolean doJTEM = false;

		instanceB = _instanceB;

		if (flagLeftHippocampus) {
			
			regImage_LeftHippocampus = (ModelImage)image_LeftHippocampus.clone();
			/*
			int imageOrient = image_LeftHippocampus.getFileInfo(0).getImageOrientation();
			int []axisOrient = image_LeftHippocampus.getFileInfo(0).getAxisOrientation();
			
			FileInfoMGH fileInfo = new FileInfoMGH(image_LeftHippocampus.getImageName(), null, FileUtility.MGH);
			
            fileInfo.setImageOrientation(imageOrient);
			fileInfo.setAxisOrientation(axisOrient);
			// fileInfo.setOrigin(newOrigin);
			FileInfoMGH[] fileInfos = {fileInfo};
			regImage_LeftHippocampus.setFileInfo(fileInfos);
			*/
			regLeftHippocampus = new AlgorithmRegOAR3D(image_LeftHippocampus,
					instanceB.image_LeftHippocampus, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regLeftHippocampus.setJTEM(doJTEM);
			regLeftHippocampus.addListener(this);
			regLeftHippocampus.run();
		}

		if (flagRightHippocampus) {
			regRightHippocampus = new AlgorithmRegOAR3D(image_RightHippocampus,
					instanceB.image_RightHippocampus, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regRightHippocampus.setJTEM(doJTEM);
			regRightHippocampus.addListener(this);
			regRightHippocampus.run();
		}

		if (flagLeftAmygdala) {
			regLeftAmygdala = new AlgorithmRegOAR3D(image_LeftAmygdala,
					instanceB.image_LeftAmygdala, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regLeftAmygdala.setJTEM(doJTEM);
			regLeftAmygdala.addListener(this);
			regLeftAmygdala.run();
		}

		if (flagRightAmygdala) {
			regRightAmygdala = new AlgorithmRegOAR3D(image_RightAmygdala,
					instanceB.image_RightAmygdala, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regRightAmygdala.setJTEM(doJTEM);
			regRightAmygdala.addListener(this);
			regRightAmygdala.run();
		}

		if (flagLeftCaudate) {
			regLeftCaudate = new AlgorithmRegOAR3D(image_LeftCaudate,
					instanceB.image_LeftCaudate, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regLeftCaudate.setJTEM(doJTEM);
			regLeftCaudate.addListener(this);
			regLeftCaudate.run();
		}

		if (flagRightCaudate) {
			regRightCaudate = new AlgorithmRegOAR3D(image_RightCaudate,
					instanceB.image_RightCaudate, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regRightCaudate.setJTEM(doJTEM);
			regRightCaudate.addListener(this);
			regRightCaudate.run();
		}

		if (flagLeftPutamen) {
			regLeftPutamen = new AlgorithmRegOAR3D(image_LeftPutamen,
					instanceB.image_LeftPutamen, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regLeftPutamen.setJTEM(doJTEM);
			regLeftPutamen.addListener(this);
			regLeftPutamen.run();
		}

		if (flagRightPutamen) {
			regRightPutamen = new AlgorithmRegOAR3D(image_RightPutamen,
					instanceB.image_RightPutamen, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regRightPutamen.setJTEM(doJTEM);
			regRightPutamen.addListener(this);
			regRightPutamen.run();
		}

		if (flagLeftGlobusPallidus) {
			regLeftGlobusPallidus = new AlgorithmRegOAR3D(
					image_LeftGlobusPallidus,
					instanceB.image_LeftGlobusPallidus, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regLeftGlobusPallidus.setJTEM(doJTEM);
			regLeftGlobusPallidus.addListener(this);
			regLeftGlobusPallidus.run();
		}

		if (flagRightGlobusPallidus) {
			regRightGlobusPallidus = new AlgorithmRegOAR3D(
					image_RightGlobusPallidus,
					instanceB.image_RightGlobusPallidus, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regRightGlobusPallidus.setJTEM(doJTEM);
			regRightGlobusPallidus.addListener(this);
			regRightGlobusPallidus.run();
		}

		if (flagLeftThalamus) {
			regLeftThalamus = new AlgorithmRegOAR3D(image_LeftThalamus,
					instanceB.image_LeftThalamus, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regLeftThalamus.setJTEM(doJTEM);
			regLeftThalamus.addListener(this);
			regLeftThalamus.run();
		}

		if (flagRightThalamus) {
			regRightThalamus = new AlgorithmRegOAR3D(image_RightThalamus,
					instanceB.image_RightThalamus, cost, DOF, interp,
					rotateBeginX, rotateEndX, coarseRateX, fineRateX,
					rotateBeginY, rotateEndY, coarseRateY, fineRateY,
					rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
					maxOfMinResol, doSubsample, doMultiThread, fastMode,
					maxIterations, numMinima);
			regRightThalamus.setJTEM(doJTEM);
			regRightThalamus.addListener(this);
			regRightThalamus.run();
		}

	}
	 
	 
	/*
	 * Read brain MRI image from given file name and directory.
	 */
	    public void readImage() {
	    	FileIO fileIO = null;
	    	// boolean multiFile = false;
	    	// FileInfoBase fileInfo = null;
	    	// System.err.println("fileName = " + fileName);
	    	  try {
	              fileIO = new FileIO();
	              // fileIO.setRawImageInfo(rawInfo);
	              // read absolute path
	              myImage = fileIO.readImage(fileName, directory);
	              // new ViewJFrameImage(myImage);
	          } catch (OutOfMemoryError e) {
	              MipavUtil.displayError("Out of memory!");
	          }

	    	
	    	
	    }
	    
	    /**
	     * Save the registred image to the output directory specified from the command line. 
	     * @param rootDirectory  output directory 
	     */
	public void saveRegisteredImages(String rootDirectory) {
		String fileName;
		int fileType;
		File rootDir = new File(rootDirectory);
		if (!rootDir.isDirectory()) {
			rootDir.mkdir();
		}
		String regdir = rootDirectory + File.separator + "Registered"
				+ File.separator;
		File fileDirectory = new File(regdir);
		if (!fileDirectory.isDirectory()) {
			fileDirectory.mkdir();
		}

		if (flagLeftHippocampus) {
			fileName = regImage_LeftHippocampus.getImageFileName() + ".mgz";
			fileType = regImage_LeftHippocampus.getType();
			regImage_LeftHippocampus
					.saveImage(regdir, fileName, fileType, true);
		}

		if (flagRightHippocampus) {
			fileName = regImage_RightHippocampus.getImageFileName() + ".mgz";
			fileType = regImage_RightHippocampus.getType();
			regImage_RightHippocampus.saveImage(regdir, fileName, fileType,
					true);
		}

		if (flagLeftAmygdala) {
			fileName = regImage_LeftAmygdala.getImageFileName() + ".mgz";
			fileType = regImage_LeftAmygdala.getType();
			regImage_LeftAmygdala.saveImage(regdir, fileName, fileType, true);
		}

		if (flagRightAmygdala) {
			fileName = regImage_RightAmygdala.getImageFileName() + ".mgz";
			fileType = regImage_RightAmygdala.getType();
			regImage_RightAmygdala.saveImage(regdir, fileName, fileType, true);
		}

		if (flagLeftCaudate) {
			fileName = regImage_LeftCaudate.getImageFileName() + ".mgz";
			fileType = regImage_LeftCaudate.getType();
			regImage_LeftCaudate.saveImage(regdir, fileName, fileType, true);
		}

		if (flagRightCaudate) {
			fileName = regImage_RightCaudate.getImageFileName() + ".mgz";
			fileType = regImage_RightCaudate.getType();
			regImage_RightCaudate.saveImage(regdir, fileName, fileType, true);
		}

		if (flagLeftPutamen) {
			fileName = regImage_LeftPutamen.getImageFileName() + ".mgz";
			fileType = regImage_LeftPutamen.getType();
			regImage_LeftPutamen.saveImage(regdir, fileName, fileType, true);
		}

		if (flagRightPutamen) {
			fileName = regImage_RightPutamen.getImageFileName() + ".mgz";
			fileType = regImage_RightPutamen.getType();
			regImage_RightPutamen.saveImage(regdir, fileName, fileType, true);
		}

		if (flagLeftGlobusPallidus) {
			fileName = regImage_LeftGlobusPallidus.getImageFileName() + ".mgz";
			fileType = regImage_LeftGlobusPallidus.getType();
			regImage_LeftGlobusPallidus.saveImage(regdir, fileName, fileType,
					true);
		}

		if (flagRightGlobusPallidus) {
			fileName = regImage_RightGlobusPallidus.getImageFileName() + ".mgz";
			fileType = regImage_RightGlobusPallidus.getType();
			regImage_RightGlobusPallidus.saveImage(regdir, fileName, fileType,
					true);
		}

		if (flagLeftThalamus) {
			fileName = regImage_LeftThalamus.getImageFileName() + ".mgz";
			fileType = regImage_LeftThalamus.getType();
			regImage_LeftThalamus.saveImage(regdir, fileName, fileType, true);
		}

		if (flagRightThalamus) {
			fileName = regImage_RightThalamus.getImageFileName() + ".mgz";
			fileType = regImage_RightThalamus.getType();
			regImage_RightThalamus.saveImage(regdir, fileName, fileType, true);
		}
	}
	    
	    
	/** 
	 * Save each subsection images after threshold process.    
	 */
	public void saveImages() {

		String fileDir = directory + File.separator + "Subcortical"
				+ File.separator;
		int fileType;
		String fileName;

		File fileDirectory = new File(fileDir);
		if (!fileDirectory.isDirectory()) {
			fileDirectory.mkdir();
		}

		if (flagLeftHippocampus) {
			fileName = image_LeftHippocampus.getImageFileName() + ".mgz";
			fileType = image_LeftHippocampus.getType();
			image_LeftHippocampus.saveImage(fileDir, fileName, fileType, true);
		}

		if (flagRightHippocampus) {
			fileName = image_RightHippocampus.getImageFileName() + ".mgz";
			fileType = image_RightHippocampus.getType();
			image_RightHippocampus.saveImage(fileDir, fileName, fileType, true);
		}

		if (flagLeftAmygdala) {
			fileName = image_LeftAmygdala.getImageFileName() + ".mgz";
			fileType = image_LeftAmygdala.getType();
			image_LeftAmygdala.saveImage(fileDir, fileName, fileType, true);
		}

		if (flagRightAmygdala) {
			fileName = image_RightAmygdala.getImageFileName() + ".mgz";
			fileType = image_RightAmygdala.getType();
			image_RightAmygdala.saveImage(fileDir, fileName, fileType, true);
		}

		if (flagLeftCaudate) {
			fileName = image_LeftCaudate.getImageFileName() + ".mgz";
			fileType = image_LeftCaudate.getType();
			image_LeftCaudate.saveImage(fileDir, fileName, fileType, true);
		}

		if (flagRightCaudate) {
			fileName = image_RightCaudate.getImageFileName() + ".mgz";
			fileType = image_RightCaudate.getType();
			image_RightCaudate.saveImage(fileDir, fileName, fileType, true);
		}

		if (flagLeftPutamen) {
			fileName = image_LeftPutamen.getImageFileName() + ".mgz";
			fileType = image_LeftPutamen.getType();
			image_LeftPutamen.saveImage(fileDir, fileName, fileType, true);
		}

		if (flagRightPutamen) {
			fileName = image_RightPutamen.getImageFileName() + ".mgz";
			fileType = image_RightPutamen.getType();
			image_RightPutamen.saveImage(fileDir, fileName, fileType, true);
		}

		if (flagLeftGlobusPallidus) {
			fileName = image_LeftGlobusPallidus.getImageFileName() + ".mgz";
			fileType = image_LeftGlobusPallidus.getType();
			image_LeftGlobusPallidus.saveImage(fileDir, fileName, fileType,
					true);
		}

		if (flagRightGlobusPallidus) {
			fileName = image_RightGlobusPallidus.getImageFileName() + ".mgz";
			fileType = image_RightGlobusPallidus.getType();
			image_RightGlobusPallidus.saveImage(fileDir, fileName, fileType,
					true);
		}

		if (flagLeftThalamus) {
			fileName = image_LeftThalamus.getImageFileName() + ".mgz";
			fileType = image_LeftThalamus.getType();
			image_LeftThalamus.saveImage(fileDir, fileName, fileType, true);
		}

		if (flagRightThalamus) {
			fileName = image_RightThalamus.getImageFileName() + ".mgz";
			fileType = image_RightThalamus.getType();
			image_RightThalamus.saveImage(fileDir, fileName, fileType, true);
		}
	}
	
	    /**
	     * Algorithm performed method, used to clean the memory. 
	     * @see algorithm 
	     */
	    public void algorithmPerformed(AlgorithmBase algorithm) {
	    	
	    	int interp2 = 0;
	    	float fillValue = 0.0f;
	    	
	    	if ( algorithm instanceof AlgorithmRegOAR3D ) {
	   
	    		if ( regLeftHippocampus != null && regLeftHippocampus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftHippocampus.getTransform();	
	    			
	    			final int xdimA = image_LeftHippocampus.getExtents()[0];
                    final int ydimA = image_LeftHippocampus.getExtents()[1];
                    final int zdimA = image_LeftHippocampus.getExtents()[2];
                    final float xresA = image_LeftHippocampus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftHippocampus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftHippocampus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftHippocampus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftHippocampus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftHippocampus = transform.getTransformedImage();
                    regImage_LeftHippocampus.calcMinMax();
                    regImage_LeftHippocampus.setImageName(name);
                    regImage_LeftHippocampus.setType(myImage.getType());
                    
                    regImage_LeftHippocampus.copyFileTypeInfo(image_LeftHippocampus);
                    // new ViewJFrameImage(regImage_LeftHippocampus);
                    /// regImage_LeftHippocampus.copyFileTypeInfo(image_LeftHippocampus);
                    
                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regLeftHippocampus != null) {
                    	regLeftHippocampus.finalize();
                    	regLeftHippocampus = null;
                    }
               
	    			
	    		}
	    		else if ( regRightHippocampus != null && regRightHippocampus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightHippocampus.getTransform();	
	    			
	    			final int xdimA = image_RightHippocampus.getExtents()[0];
                    final int ydimA = image_RightHippocampus.getExtents()[1];
                    final int zdimA = image_RightHippocampus.getExtents()[2];
                    final float xresA = image_RightHippocampus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightHippocampus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightHippocampus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightHippocampus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightHippocampus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightHippocampus = transform.getTransformedImage();
                    regImage_RightHippocampus.calcMinMax();
                    regImage_RightHippocampus.setImageName(name);
                    regImage_RightHippocampus.setType(myImage.getType());
                    
                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regRightHippocampus != null) {
                    	regRightHippocampus.finalize();
                    	regRightHippocampus = null;
                    }
                    
	    		}
	    		else if ( regLeftAmygdala != null && regLeftAmygdala.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftAmygdala.getTransform();	
	    			
	    			final int xdimA = image_LeftAmygdala.getExtents()[0];
                    final int ydimA = image_LeftAmygdala.getExtents()[1];
                    final int zdimA = image_LeftAmygdala.getExtents()[2];
                    final float xresA = image_LeftAmygdala.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftAmygdala.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftAmygdala.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftAmygdala.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftAmygdala, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftAmygdala = transform.getTransformedImage();
                    regImage_LeftAmygdala.calcMinMax();
                    regImage_LeftAmygdala.setImageName(name);
                    regImage_LeftAmygdala.setType(myImage.getType());
                    
                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regLeftAmygdala != null) {
                    	regLeftAmygdala.finalize();
                    	regLeftAmygdala = null;
                    }
                
	    		}
	    		else if ( regRightAmygdala != null && regRightAmygdala.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightAmygdala.getTransform();	
	    			
	    			final int xdimA = image_RightAmygdala.getExtents()[0];
                    final int ydimA = image_RightAmygdala.getExtents()[1];
                    final int zdimA = image_RightAmygdala.getExtents()[2];
                    final float xresA = image_RightAmygdala.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightAmygdala.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightAmygdala.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightAmygdala.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightAmygdala, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightAmygdala = transform.getTransformedImage();
                    regImage_RightAmygdala.calcMinMax();
                    regImage_RightAmygdala.setImageName(name);
                    regImage_RightAmygdala.setType(myImage.getType());
                    
                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regRightAmygdala != null) {
                    	regRightAmygdala.finalize();
                    	regRightAmygdala = null;
                    }
	    		}
	    		else if ( regLeftCaudate != null && regLeftCaudate.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftCaudate.getTransform();	
	    			
	    			final int xdimA = image_LeftCaudate.getExtents()[0];
                    final int ydimA = image_LeftCaudate.getExtents()[1];
                    final int zdimA = image_LeftCaudate.getExtents()[2];
                    final float xresA = image_LeftCaudate.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftCaudate.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftCaudate.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftCaudate.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftCaudate, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftCaudate = transform.getTransformedImage();
                    regImage_LeftCaudate.calcMinMax();
                    regImage_LeftCaudate.setImageName(name);
                    regImage_LeftCaudate.setType(myImage.getType());

                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regLeftCaudate != null) {
                    	regLeftCaudate.finalize();
                    	regLeftCaudate = null;
                    }
                    
	    		}
	    		else if ( regRightCaudate != null && regRightCaudate.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightCaudate.getTransform();	
	    			
	    			final int xdimA = image_RightCaudate.getExtents()[0];
                    final int ydimA = image_RightCaudate.getExtents()[1];
                    final int zdimA = image_RightCaudate.getExtents()[2];
                    final float xresA = image_RightCaudate.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightCaudate.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightCaudate.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightCaudate.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightCaudate, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightCaudate = transform.getTransformedImage();
                    regImage_RightCaudate.calcMinMax();
                    regImage_RightCaudate.setImageName(name);
                    regImage_RightCaudate.setType(myImage.getType());

                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regRightCaudate != null) {
                    	regRightCaudate.finalize();
                    	regRightCaudate = null;
                    }
                 
	    		}
	    		else if ( regLeftPutamen != null && regLeftPutamen.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftPutamen.getTransform();	
	    			
	    			final int xdimA = image_LeftPutamen.getExtents()[0];
                    final int ydimA = image_LeftPutamen.getExtents()[1];
                    final int zdimA = image_LeftPutamen.getExtents()[2];
                    final float xresA = image_LeftPutamen.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftPutamen.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftPutamen.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftPutamen.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftPutamen, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftPutamen = transform.getTransformedImage();
                    regImage_LeftPutamen.calcMinMax();
                    regImage_LeftPutamen.setImageName(name);
                    regImage_LeftPutamen.setType(myImage.getType());
                    
                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regLeftPutamen != null) {
                    	regLeftPutamen.finalize();
                    	regLeftPutamen = null;
                    }
                
	    		}
	    		else if ( regRightPutamen != null && regRightPutamen.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightPutamen.getTransform();	
	    			
	    			final int xdimA = image_RightPutamen.getExtents()[0];
                    final int ydimA = image_RightPutamen.getExtents()[1];
                    final int zdimA = image_RightPutamen.getExtents()[2];
                    final float xresA = image_RightPutamen.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightPutamen.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightPutamen.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightPutamen.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightPutamen, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightPutamen = transform.getTransformedImage();
                    regImage_RightPutamen.calcMinMax();
                    regImage_RightPutamen.setImageName(name);
                    regImage_RightPutamen.setType(myImage.getType());

                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regRightPutamen != null) {
                    	regRightPutamen.finalize();
                    	regRightPutamen = null;
                    }
                    
	    		}
	    		else if ( regLeftGlobusPallidus != null && regLeftGlobusPallidus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftGlobusPallidus.getTransform();	
	    			
	    			final int xdimA = image_LeftGlobusPallidus.getExtents()[0];
                    final int ydimA = image_LeftGlobusPallidus.getExtents()[1];
                    final int zdimA = image_LeftGlobusPallidus.getExtents()[2];
                    final float xresA = image_LeftGlobusPallidus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftGlobusPallidus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftGlobusPallidus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftGlobusPallidus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftGlobusPallidus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftGlobusPallidus = transform.getTransformedImage();
                    regImage_LeftGlobusPallidus.calcMinMax();
                    regImage_LeftGlobusPallidus.setImageName(name);
                    regImage_LeftGlobusPallidus.setType(myImage.getType());
                    
                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regLeftGlobusPallidus != null) {
                    	regLeftGlobusPallidus.finalize();
                    	regLeftGlobusPallidus = null;
                    }
	    			
	    		}
	    		else if ( regRightGlobusPallidus != null && regRightGlobusPallidus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightGlobusPallidus.getTransform();	
	    			
	    			final int xdimA = image_RightGlobusPallidus.getExtents()[0];
                    final int ydimA = image_RightGlobusPallidus.getExtents()[1];
                    final int zdimA = image_RightGlobusPallidus.getExtents()[2];
                    final float xresA = image_RightGlobusPallidus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightGlobusPallidus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightGlobusPallidus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightGlobusPallidus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightGlobusPallidus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightGlobusPallidus = transform.getTransformedImage();
                    regImage_RightGlobusPallidus.calcMinMax();
                    regImage_RightGlobusPallidus.setImageName(name);
                    regImage_RightGlobusPallidus.setType(myImage.getType());
                    
                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regRightGlobusPallidus != null) {
                    	regRightGlobusPallidus.finalize();
                    	regRightGlobusPallidus = null;
                    }
	    			
	    		}
	    		else if ( regLeftThalamus != null && regLeftThalamus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regLeftThalamus.getTransform();	
	    			
	    			final int xdimA = image_LeftThalamus.getExtents()[0];
                    final int ydimA = image_LeftThalamus.getExtents()[1];
                    final int zdimA = image_LeftThalamus.getExtents()[2];
                    final float xresA = image_LeftThalamus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_LeftThalamus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_LeftThalamus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_LeftThalamus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_LeftThalamus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_LeftThalamus = transform.getTransformedImage();
                    regImage_LeftThalamus.calcMinMax();
                    regImage_LeftThalamus.setImageName(name);
                    regImage_LeftThalamus.setType(myImage.getType());

                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regLeftThalamus != null) {
                    	regLeftThalamus.finalize();
                    	regLeftThalamus = null;
                    }
	    			
	    		}
	    		else if ( regRightThalamus != null && regRightThalamus.isCompleted() ) {
	    			final TransMatrix finalMatrix = regRightThalamus.getTransform();	
	    			
	    			final int xdimA = image_RightThalamus.getExtents()[0];
                    final int ydimA = image_RightThalamus.getExtents()[1];
                    final int zdimA = image_RightThalamus.getExtents()[2];
                    final float xresA = image_RightThalamus.getFileInfo(0).getResolutions()[0];
                    final float yresA = image_RightThalamus.getFileInfo(0).getResolutions()[1];
                    final float zresA = image_RightThalamus.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(instanceB.image_RightThalamus.getImageName(), "_register");

                    AlgorithmTransform transform = new AlgorithmTransform(instanceB.image_RightThalamus, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);
                    transform.run();
                    regImage_RightThalamus = transform.getTransformedImage();
                    regImage_RightThalamus.calcMinMax();
                    regImage_RightThalamus.setImageName(name);
                    regImage_RightThalamus.setType(myImage.getType());
                                        
                    if (transform != null) {
                        transform.finalize();
                        transform = null;
                    }
                    
                    if (regRightThalamus != null) {
                    	regRightThalamus.finalize();
                    	regRightThalamus = null;
                    }
                  
	    		}
	    		
	    	}
	    	else if (algorithm instanceof AlgorithmThresholdDual) {
	        	
	    		// image threshold of color label
	            if (algorThreshold_LeftHippocampus != null && (algorThreshold_LeftHippocampus.isCompleted() == true)) {
	            	image_LeftHippocampus.clearMask();

	                try {
	                	image_LeftHippocampus.calcMinMax();
	                	image_LeftHippocampus.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil.displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftHippocampus.finalize();
		            algorThreshold_LeftHippocampus = null;
		     
	            }
	            else if (algorThreshold_RightHippocampus != null && (algorThreshold_RightHippocampus.isCompleted() == true)) {
	            	image_RightHippocampus.clearMask();
	             
	                try {
	                	image_RightHippocampus.calcMinMax();
	                	image_RightHippocampus.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightHippocampus.finalize();
		            algorThreshold_RightHippocampus = null;
	            }
	            else if (algorThreshold_LeftAmygdala != null && (algorThreshold_LeftAmygdala.isCompleted() == true)) {
	            	image_LeftAmygdala.clearMask();
	               
	                try {
	                	image_LeftAmygdala.calcMinMax();
	                	image_LeftAmygdala.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftAmygdala.finalize();
		            algorThreshold_LeftAmygdala = null;
	            }
	            else if (algorThreshold_RightAmygdala != null && (algorThreshold_RightAmygdala.isCompleted() == true)) {
	            	image_RightAmygdala.clearMask();

	                try {
	                	image_RightAmygdala.calcMinMax();
	                	image_RightAmygdala.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightAmygdala.finalize();
		            algorThreshold_RightAmygdala = null;
		           
	            }
	            else if (algorThreshold_LeftCaudate != null && (algorThreshold_LeftCaudate.isCompleted() == true)) {
	            	image_LeftCaudate.clearMask();
	              
	                try {
	                	image_LeftCaudate.calcMinMax();
	                	image_LeftCaudate.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftCaudate.finalize();
		            algorThreshold_LeftCaudate = null;
		    
	            }
	            else if (algorThreshold_RightCaudate != null && (algorThreshold_RightCaudate.isCompleted() == true)) {
	            	image_RightCaudate.clearMask();
	               
	                try {
	                	image_RightCaudate.calcMinMax();
	                	image_RightCaudate.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightCaudate.finalize();
		            algorThreshold_RightCaudate = null;
		            
	            }
	            else if (algorThreshold_LeftPutamen != null && (algorThreshold_LeftPutamen.isCompleted() == true)) {
	            	image_LeftPutamen.clearMask();
	               
	                try {
	                	image_LeftPutamen.calcMinMax();
	                	image_LeftPutamen.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftPutamen.finalize();
		            algorThreshold_LeftPutamen = null;
		            
	            }
	            else if (algorThreshold_RightPutamen != null && (algorThreshold_RightPutamen.isCompleted() == true)) {
	            	image_RightPutamen.clearMask();
	                
	                try {
	                	image_RightPutamen.calcMinMax();
	                	image_RightPutamen.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightPutamen.finalize();
		            algorThreshold_RightPutamen = null;
	            }
	            else if (algorThreshold_LeftGlobusPallidus != null && (algorThreshold_LeftGlobusPallidus.isCompleted() == true)) {
	            	image_LeftGlobusPallidus.clearMask();
	                
	                try {
	                	image_LeftGlobusPallidus.calcMinMax();
	                	image_LeftGlobusPallidus.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftGlobusPallidus.finalize();
		            algorThreshold_LeftGlobusPallidus = null;
	            }
	            else if (algorThreshold_RightGlobusPallidus != null && (algorThreshold_RightGlobusPallidus.isCompleted() == true)) {
	            	image_RightGlobusPallidus.clearMask();
	               
	                try {
	                	image_RightGlobusPallidus.calcMinMax();
	                	image_RightGlobusPallidus.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightGlobusPallidus.finalize();
		            algorThreshold_RightGlobusPallidus = null;
		        
	            }
	            else if (algorThreshold_LeftThalamus != null && (algorThreshold_LeftThalamus.isCompleted() == true)) {
	            	image_LeftThalamus.clearMask();
	                
	                try {
	                	image_LeftThalamus.calcMinMax();
	                	image_LeftThalamus.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftThalamus.finalize();
		            algorThreshold_LeftThalamus = null;
		        
	            }
	            else if (algorThreshold_RightThalamus != null && (algorThreshold_RightThalamus.isCompleted() == true)) {
	            	image_RightThalamus.clearMask();
	                
	                try {
	                	image_RightThalamus.calcMinMax();
	                	image_RightThalamus.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightThalamus.finalize();
		            algorThreshold_RightThalamus = null;
	            }
	            
	            // binary threshold on origin and registered images. 
	            if (algorThreshold_LeftHippocampus_binary != null && (algorThreshold_LeftHippocampus_binary.isCompleted() == true)) {
	            	image_LeftHippocampus_binary.clearMask();

	                try {
	                	image_LeftHippocampus_binary.calcMinMax();
	                	image_LeftHippocampus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftHippocampus_binary.finalize();
		            algorThreshold_LeftHippocampus_binary = null;
		   
	            } else if (algorThreshold_RightHippocampus_binary != null && (algorThreshold_RightHippocampus_binary.isCompleted() == true)) {
	            	image_RightHippocampus_binary.clearMask();
	                
	            	try {
	                	image_RightHippocampus_binary.calcMinMax();
	                	image_RightHippocampus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightHippocampus_binary.finalize();
		            algorThreshold_RightHippocampus_binary = null;
		        } else if (algorThreshold_LeftAmygdala_binary != null && (algorThreshold_LeftAmygdala_binary.isCompleted() == true)) {
	            	image_LeftAmygdala_binary.clearMask();
	                
	            	try {
	                	image_LeftAmygdala_binary.calcMinMax();
	                	image_LeftAmygdala_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftAmygdala_binary.finalize();
		            algorThreshold_LeftAmygdala_binary = null;
		        } else if (algorThreshold_RightAmygdala_binary != null && (algorThreshold_RightAmygdala_binary.isCompleted() == true)) {
	            	image_RightAmygdala_binary.clearMask();
	                
	            	try {
	                	image_RightAmygdala_binary.calcMinMax();
	                	image_RightAmygdala_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightAmygdala_binary.finalize();
		            algorThreshold_RightAmygdala_binary = null;
		        } else if (algorThreshold_LeftCaudate_binary != null && (algorThreshold_LeftCaudate_binary.isCompleted() == true)) {
	            	image_LeftCaudate_binary.clearMask();

	                try {
	                	image_LeftCaudate_binary.calcMinMax();
	                	image_LeftCaudate_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftCaudate_binary.finalize();
		            algorThreshold_LeftCaudate_binary = null;
		        } else if (algorThreshold_RightCaudate_binary != null && (algorThreshold_RightCaudate_binary.isCompleted() == true)) {
	            	image_RightCaudate_binary.clearMask();
	               
	                try {
	                	image_RightCaudate_binary.calcMinMax();
	                	image_RightCaudate_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightCaudate_binary.finalize();
		            algorThreshold_RightCaudate_binary = null;
		        } else if (algorThreshold_LeftPutamen_binary != null && (algorThreshold_LeftPutamen_binary.isCompleted() == true)) {
	            	image_LeftPutamen_binary.clearMask();
	                
	                try {
	                	image_LeftPutamen_binary.calcMinMax();
	                	image_LeftPutamen_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftPutamen_binary.finalize();
		            algorThreshold_LeftPutamen_binary = null;
		        } else if (algorThreshold_RightPutamen_binary != null && (algorThreshold_RightPutamen_binary.isCompleted() == true)) {
	            	image_RightPutamen_binary.clearMask();

	                try {
	                	image_RightPutamen_binary.calcMinMax();
	                	image_RightPutamen_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightPutamen_binary.finalize();
		            algorThreshold_RightPutamen_binary = null;
		        } else if (algorThreshold_LeftGlobusPallidus_binary != null && (algorThreshold_LeftGlobusPallidus_binary.isCompleted() == true)) {
	            	image_LeftGlobusPallidus_binary.clearMask();
	                try {
	                	image_LeftGlobusPallidus_binary.calcMinMax();
	                	image_LeftGlobusPallidus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftGlobusPallidus_binary.finalize();
		            algorThreshold_LeftGlobusPallidus_binary = null;
		        } else if (algorThreshold_RightGlobusPallidus_binary != null && (algorThreshold_RightGlobusPallidus_binary.isCompleted() == true)) {
	            	image_RightGlobusPallidus_binary.clearMask();
	               
	                try {
	                	image_RightGlobusPallidus_binary.calcMinMax();
	                	image_RightGlobusPallidus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightGlobusPallidus_binary.finalize();
		            algorThreshold_RightGlobusPallidus_binary = null;
		            System.gc();
	            } else if (algorThreshold_LeftThalamus_binary != null && (algorThreshold_LeftThalamus_binary.isCompleted() == true)) {
	            	image_LeftThalamus_binary.clearMask();
	               
	                try {
	                	image_LeftThalamus_binary.calcMinMax();
	                	image_LeftThalamus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftThalamus_binary.finalize();
		            algorThreshold_LeftThalamus_binary = null;
		        } else if (algorThreshold_RightThalamus_binary != null && (algorThreshold_RightThalamus_binary.isCompleted() == true)) {
	            	image_RightThalamus_binary.clearMask();
	                
	                try {
	                	image_RightThalamus_binary.calcMinMax();
	                	image_RightThalamus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightThalamus_binary.finalize();
		            algorThreshold_RightThalamus_binary = null;
		        } 
	            
	            // registered image binary threshold
	            if (algorThreshold_LeftHippocampus_reg_binary != null && (algorThreshold_LeftHippocampus_reg_binary.isCompleted() == true)) {
	            	regImage_LeftHippocampus_binary.clearMask();
	                
	                try {
	                	regImage_LeftHippocampus_binary.calcMinMax();
	                	regImage_LeftHippocampus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftHippocampus_reg_binary.finalize();
	                algorThreshold_LeftHippocampus_reg_binary = null;
		        } else if (algorThreshold_RightHippocampus_reg_binary != null && (algorThreshold_RightHippocampus_reg_binary.isCompleted() == true)) {
	            	regImage_RightHippocampus_binary.clearMask();
	                
	                try {
	                	regImage_RightHippocampus_binary.calcMinMax();
	                	regImage_RightHippocampus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightHippocampus_reg_binary.finalize();
	                algorThreshold_RightHippocampus_reg_binary = null;
		        } else if (algorThreshold_LeftAmygdala_reg_binary != null && (algorThreshold_LeftAmygdala_reg_binary.isCompleted() == true)) {
	            	regImage_LeftAmygdala_binary.clearMask();
	                
	                try {
	                	regImage_LeftAmygdala_binary.calcMinMax();
	                	regImage_LeftAmygdala_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftAmygdala_reg_binary.finalize();
	                algorThreshold_LeftAmygdala_reg_binary = null;
		        } else if (algorThreshold_RightAmygdala_reg_binary != null && (algorThreshold_RightAmygdala_reg_binary.isCompleted() == true)) {
	            	regImage_RightAmygdala_binary.clearMask();
	                
	                try {
	                	regImage_RightAmygdala_binary.calcMinMax();
	                	regImage_RightAmygdala_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightAmygdala_reg_binary.finalize();
	                algorThreshold_RightAmygdala_reg_binary = null;
		        } else if (algorThreshold_LeftCaudate_reg_binary != null && (algorThreshold_LeftCaudate_reg_binary.isCompleted() == true)) {
	            	regImage_LeftCaudate_binary.clearMask();
	                
	                try {
	                	regImage_LeftCaudate_binary.calcMinMax();
	                	regImage_LeftCaudate_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftCaudate_reg_binary.finalize();
	                algorThreshold_LeftCaudate_reg_binary = null;
		        } else if (algorThreshold_RightCaudate_reg_binary != null && (algorThreshold_RightCaudate_reg_binary.isCompleted() == true)) {
	            	regImage_RightCaudate_binary.clearMask();
	                

	                try {
	                	regImage_RightCaudate_binary.calcMinMax();
	                	regImage_RightCaudate_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightCaudate_reg_binary.finalize();
	                algorThreshold_RightCaudate_reg_binary = null;
		        }  else if (algorThreshold_LeftPutamen_reg_binary != null && (algorThreshold_LeftPutamen_reg_binary.isCompleted() == true)) {
	            	regImage_LeftPutamen_binary.clearMask();
	               
	                try {
	                	regImage_LeftPutamen_binary.calcMinMax();
	                	regImage_LeftPutamen_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftPutamen_reg_binary.finalize();
	                algorThreshold_LeftPutamen_reg_binary = null;
		        } else if (algorThreshold_RightPutamen_reg_binary != null && (algorThreshold_RightPutamen_reg_binary.isCompleted() == true)) {
	            	regImage_RightPutamen_binary.clearMask();
	                

	                try {
	                	regImage_RightPutamen_binary.calcMinMax();
	                	regImage_RightPutamen_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightPutamen_reg_binary.finalize();
	                algorThreshold_RightPutamen_reg_binary = null;
		        } else if (algorThreshold_LeftGlobusPallidus_reg_binary != null && (algorThreshold_LeftGlobusPallidus_reg_binary.isCompleted() == true)) {
	            	regImage_LeftGlobusPallidus_binary.clearMask();
	                
	                try {
	                	regImage_LeftGlobusPallidus_binary.calcMinMax();
	                	regImage_LeftGlobusPallidus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftGlobusPallidus_reg_binary.finalize();
	                algorThreshold_LeftGlobusPallidus_reg_binary = null;
		        } else if (algorThreshold_RightGlobusPallidus_reg_binary != null && (algorThreshold_RightGlobusPallidus_reg_binary.isCompleted() == true)) {
	            	regImage_RightGlobusPallidus_binary.clearMask();
	                
	                try {
	                	regImage_RightGlobusPallidus_binary.calcMinMax();
	                	regImage_RightGlobusPallidus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_RightGlobusPallidus_reg_binary.finalize();
	                algorThreshold_RightGlobusPallidus_reg_binary = null;
		        } else if (algorThreshold_LeftThalamus_reg_binary != null && (algorThreshold_LeftThalamus_reg_binary.isCompleted() == true)) {
	            	regImage_LeftThalamus_binary.clearMask();
	                
	                try {
	                	regImage_LeftThalamus_binary.calcMinMax();
	                	regImage_LeftThalamus_binary.notifyImageDisplayListeners(null, true);
	                } catch (OutOfMemoryError error) {
	                    MipavUtil
	                    .displayError("Out of memory: unable to open new frame");
	                }
	                algorThreshold_LeftThalamus_reg_binary.finalize();
	                algorThreshold_LeftThalamus_reg_binary = null;
		            System.gc();
	            } 
	            
	            
	        }

	    }

	/**
	 * Threshold the image origin and registered image to binary image for comparison. 
	 */
	public void originImageThresholdBinary() {

		float[] thresholds = new float[2];
		float fillValue = 0;
		boolean isInverse = false;
		boolean regionFlag = true;
		int outputType = 1;

		try {

			float thres1 = 1.0f; // lowerThres;
			float thres2 = LeftHippocampus; // upperThres;

			thresholds[0] = thres1;
			thresholds[1] = thres2;

			// Left Hippocampus ***
			if (flagLeftHippocampus) {
				image_LeftHippocampus_binary = (ModelImage) image_LeftHippocampus
						.clone();
				image_LeftHippocampus_binary.setType(ModelStorageBase.BOOLEAN);
				image_LeftHippocampus_binary.setImageName(myImage
						.getImageName() + "_leftHippocampus_binary");
				algorThreshold_LeftHippocampus_binary = new AlgorithmThresholdDual(
						image_LeftHippocampus_binary, image_LeftHippocampus,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_LeftHippocampus_binary.addListener(this);
				algorThreshold_LeftHippocampus_binary.run();
			}

			// Right Hippocampus ****
			thres1 = 1.0f; // lowerThres;
			thres2 = RightHippocampus; // upperThres;

			if (flagRightHippocampus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightHippocampus_binary = (ModelImage) image_RightHippocampus
						.clone();
				image_RightHippocampus_binary.setType(ModelStorageBase.BOOLEAN);
				image_RightHippocampus_binary.setImageName(myImage
						.getImageName() + "_rightHippocampus_binary");
				algorThreshold_RightHippocampus_binary = new AlgorithmThresholdDual(
						image_RightHippocampus_binary, image_RightHippocampus,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_RightHippocampus_binary
						.addListener(this);
				algorThreshold_RightHippocampus_binary.run();
			}
			// Left Amygdala ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftAmygdala; // upperThres;

			if (flagLeftAmygdala) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftAmygdala_binary = (ModelImage) image_LeftAmygdala
						.clone();
				image_LeftAmygdala_binary.setType(ModelStorageBase.BOOLEAN);
				image_LeftAmygdala_binary.setImageName(myImage.getImageName()
						+ "_LeftAmygdala_binary");
				algorThreshold_LeftAmygdala_binary = new AlgorithmThresholdDual(
						image_LeftAmygdala_binary, image_LeftAmygdala,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_LeftAmygdala_binary.addListener(this);
				algorThreshold_LeftAmygdala_binary.run();
			}

			// Right Amygdala ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightAmygdala; // upperThres;

			if (flagRightAmygdala) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightAmygdala_binary = (ModelImage) image_RightAmygdala
						.clone();
				image_RightAmygdala_binary.setType(ModelStorageBase.BOOLEAN);
				image_RightAmygdala_binary.setImageName(myImage.getImageName()
						+ "_RightAmygdala_binary");
				algorThreshold_RightAmygdala_binary = new AlgorithmThresholdDual(
						image_RightAmygdala_binary, image_RightAmygdala,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_RightAmygdala_binary.addListener(this);
				algorThreshold_RightAmygdala_binary.run();
			}

			// Left Caudate ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftCaudate; // upperThres;

			if (flagLeftCaudate) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftCaudate_binary = (ModelImage) image_LeftCaudate
						.clone();
				image_LeftCaudate_binary.setType(ModelStorageBase.BOOLEAN);
				image_LeftCaudate_binary.setImageName(myImage.getImageName()
						+ "_LeftCaudate_binary");
				algorThreshold_LeftCaudate_binary = new AlgorithmThresholdDual(
						image_LeftCaudate_binary, image_LeftCaudate,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_LeftCaudate_binary.addListener(this);
				algorThreshold_LeftCaudate_binary.run();
			}

			// RightCaudate ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightCaudate; // upperThres;

			if (flagRightCaudate) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightCaudate_binary = (ModelImage) image_RightCaudate
						.clone();
				image_RightCaudate_binary.setType(ModelStorageBase.BOOLEAN);
				image_RightCaudate_binary.setImageName(myImage.getImageName()
						+ "_RightCaudate_binary");
				algorThreshold_RightCaudate_binary = new AlgorithmThresholdDual(
						image_RightCaudate_binary, image_RightCaudate,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_RightCaudate_binary.addListener(this);
				algorThreshold_RightCaudate_binary.run();
			}

			// Left Putamen ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftPutamen; // upperThres;

			if (flagLeftPutamen) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftPutamen_binary = (ModelImage) image_LeftPutamen
						.clone();
				image_LeftPutamen_binary.setType(ModelStorageBase.BOOLEAN);
				image_LeftPutamen_binary.setImageName(myImage.getImageName()
						+ "_LeftPutamen_binary");
				algorThreshold_LeftPutamen_binary = new AlgorithmThresholdDual(
						image_LeftPutamen_binary, image_LeftPutamen,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_LeftPutamen_binary.addListener(this);
				algorThreshold_LeftPutamen_binary.run();
			}

			// Right Putamen ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightPutamen; // upperThres;

			if (flagRightPutamen) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;

				image_RightPutamen_binary = (ModelImage) image_RightPutamen
						.clone();
				image_RightPutamen_binary.setType(ModelStorageBase.BOOLEAN);
				image_RightPutamen_binary.setImageName(myImage.getImageName()
						+ "_RightPutamen_binary");
				algorThreshold_RightPutamen_binary = new AlgorithmThresholdDual(
						image_RightPutamen_binary, image_RightPutamen,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_RightPutamen_binary.addListener(this);
				algorThreshold_RightPutamen_binary.run();
			}

			// Left GlobusPallidus ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftGlobusPallidus; // upperThres;

			if (flagLeftGlobusPallidus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftGlobusPallidus_binary = (ModelImage) image_LeftGlobusPallidus
						.clone();
				image_LeftGlobusPallidus_binary
						.setType(ModelStorageBase.BOOLEAN);
				image_LeftGlobusPallidus_binary.setImageName(myImage
						.getImageName() + "_LeftGlobusPallidus_binary");
				algorThreshold_LeftGlobusPallidus_binary = new AlgorithmThresholdDual(
						image_LeftGlobusPallidus_binary,
						image_LeftGlobusPallidus, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_LeftGlobusPallidus_binary
						.addListener(this);
				algorThreshold_LeftGlobusPallidus_binary.run();
			}

			// Right GlobusPallidus ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightGlobusPallidus; // upperThres;

			if (flagRightGlobusPallidus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightGlobusPallidus_binary = (ModelImage) image_RightGlobusPallidus
						.clone();
				image_RightGlobusPallidus_binary
						.setType(ModelStorageBase.BOOLEAN);
				image_RightGlobusPallidus_binary.setImageName(myImage
						.getImageName() + "_RightGlobusPallidus_binary");
				algorThreshold_RightGlobusPallidus_binary = new AlgorithmThresholdDual(
						image_RightGlobusPallidus_binary,
						image_RightGlobusPallidus, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_RightGlobusPallidus_binary
						.addListener(this);
				algorThreshold_RightGlobusPallidus_binary.run();
			}
			// Left Thalamus ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftThalamus; // upperThres;

			if (flagLeftThalamus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_LeftThalamus_binary = (ModelImage) image_LeftThalamus
						.clone();
				image_LeftThalamus_binary.setType(ModelStorageBase.BOOLEAN);
				image_LeftThalamus_binary.setImageName(myImage.getImageName()
						+ "_LeftThalamus_binary");
				algorThreshold_LeftThalamus_binary = new AlgorithmThresholdDual(
						image_LeftThalamus_binary, image_LeftThalamus,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_LeftThalamus_binary.addListener(this);
				algorThreshold_LeftThalamus_binary.run();
			}

			// Right Thalamus ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightThalamus; // upperThres;

			if (flagRightThalamus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				image_RightThalamus_binary = (ModelImage) image_RightThalamus
						.clone();
				image_RightThalamus_binary.setType(ModelStorageBase.BOOLEAN);
				image_RightThalamus_binary.setImageName(myImage.getImageName()
						+ "_RightThalamus_binar");
				algorThreshold_RightThalamus_binary = new AlgorithmThresholdDual(
						image_RightThalamus_binary, image_RightThalamus,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_RightThalamus_binary.addListener(this);
				algorThreshold_RightThalamus_binary.run();
			}
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog threshold: unable to allocate enough memory");

			return;
		}

	}
	
	/** 
	 * Convert the registered image to binary image. 	    
	 */
	public void registeredImageThresholdBinary() {

		float[] thresholds = new float[2];
		float fillValue = 0;
		boolean isInverse = false;
		boolean regionFlag = true;
		int outputType = 1;

		try {

			float thres1 = 1.0f; // lowerThres;
			float thres2 = LeftHippocampus; // upperThres;

			if (flagLeftHippocampus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;

				// / Left Hippocampus ***
				regImage_LeftHippocampus_binary = (ModelImage) regImage_LeftHippocampus
						.clone();
				regImage_LeftHippocampus_binary
						.setType(ModelStorageBase.BOOLEAN);
				regImage_LeftHippocampus_binary
						.setImageName(regImage_LeftHippocampus.getImageName()
								+ "_binary");
				algorThreshold_LeftHippocampus_reg_binary = new AlgorithmThresholdDual(
						regImage_LeftHippocampus_binary,
						regImage_LeftHippocampus, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_LeftHippocampus_reg_binary
						.addListener(this);
				algorThreshold_LeftHippocampus_reg_binary.run();
			}

			// / Right Hippocampus ****
			thres1 = 1.0f; // lowerThres;
			thres2 = RightHippocampus; // upperThres;

			if (flagRightHippocampus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_RightHippocampus_binary = (ModelImage) regImage_RightHippocampus
						.clone();
				regImage_RightHippocampus_binary
						.setType(ModelStorageBase.BOOLEAN);
				regImage_RightHippocampus_binary
						.setImageName(regImage_RightHippocampus.getImageName()
								+ "_binary");
				algorThreshold_RightHippocampus_reg_binary = new AlgorithmThresholdDual(
						regImage_RightHippocampus_binary,
						regImage_RightHippocampus, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_RightHippocampus_reg_binary
						.addListener(this);
				algorThreshold_RightHippocampus_reg_binary.run();
			}

			// / Left Amygdala ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftAmygdala; // upperThres;

			if (flagLeftAmygdala) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_LeftAmygdala_binary = (ModelImage) regImage_LeftAmygdala
						.clone();
				regImage_LeftAmygdala_binary.setType(ModelStorageBase.BOOLEAN);
				regImage_LeftAmygdala_binary.setImageName(regImage_LeftAmygdala
						.getImageName() + "_binary");
				algorThreshold_LeftAmygdala_reg_binary = new AlgorithmThresholdDual(
						regImage_LeftAmygdala_binary, regImage_LeftAmygdala,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_LeftAmygdala_reg_binary
						.addListener(this);
				algorThreshold_LeftAmygdala_reg_binary.run();
			}

			// / Right Amygdala ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightAmygdala; // upperThres;

			if (flagRightAmygdala) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_RightAmygdala_binary = (ModelImage) regImage_RightAmygdala
						.clone();
				regImage_RightAmygdala_binary.setType(ModelStorageBase.BOOLEAN);
				regImage_RightAmygdala_binary
						.setImageName(regImage_RightAmygdala.getImageName()
								+ "_binary");
				algorThreshold_RightAmygdala_reg_binary = new AlgorithmThresholdDual(
						regImage_RightAmygdala_binary, regImage_RightAmygdala,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_RightAmygdala_reg_binary
						.addListener(this);
				algorThreshold_RightAmygdala_reg_binary.run();
			}

			// / Left Caudate ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftCaudate; // upperThres;

			if (flagLeftCaudate) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_LeftCaudate_binary = (ModelImage) regImage_LeftCaudate
						.clone();
				regImage_LeftCaudate_binary.setType(ModelStorageBase.BOOLEAN);
				regImage_LeftCaudate_binary.setImageName(regImage_LeftCaudate
						.getImageName() + "_binary");
				algorThreshold_LeftCaudate_reg_binary = new AlgorithmThresholdDual(
						regImage_LeftCaudate_binary, regImage_LeftCaudate,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_LeftCaudate_reg_binary.addListener(this);
				algorThreshold_LeftCaudate_reg_binary.run();
			}
			// / RightCaudate ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightCaudate; // upperThres;

			if (flagRightCaudate) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_RightCaudate_binary = (ModelImage) regImage_RightCaudate
						.clone();
				regImage_RightCaudate_binary.setType(ModelStorageBase.BOOLEAN);
				regImage_RightCaudate_binary.setImageName(regImage_RightCaudate
						.getImageName() + "_binary");
				algorThreshold_RightCaudate_reg_binary = new AlgorithmThresholdDual(
						regImage_RightCaudate_binary, regImage_RightCaudate,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_RightCaudate_reg_binary
						.addListener(this);
				algorThreshold_RightCaudate_reg_binary.run();
			}

			// / Left Putamen ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftPutamen; // upperThres;

			if (flagLeftPutamen) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_LeftPutamen_binary = (ModelImage) regImage_LeftPutamen
						.clone();
				regImage_LeftPutamen_binary.setType(ModelStorageBase.BOOLEAN);
				regImage_LeftPutamen_binary.setImageName(regImage_LeftPutamen
						.getImageName() + "_binary");
				algorThreshold_LeftPutamen_reg_binary = new AlgorithmThresholdDual(
						regImage_LeftPutamen_binary, regImage_LeftPutamen,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_LeftPutamen_reg_binary.addListener(this);
				algorThreshold_LeftPutamen_reg_binary.run();
			}

			// / Right Putamen ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightPutamen; // upperThres;
			if (flagRightPutamen) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_RightPutamen_binary = (ModelImage) regImage_RightPutamen
						.clone();
				regImage_RightPutamen_binary.setType(ModelStorageBase.BOOLEAN);
				regImage_RightPutamen_binary.setImageName(regImage_RightPutamen
						.getImageName() + "_binary");
				algorThreshold_RightPutamen_reg_binary = new AlgorithmThresholdDual(
						regImage_RightPutamen_binary, regImage_RightPutamen,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_RightPutamen_reg_binary
						.addListener(this);
				algorThreshold_RightPutamen_reg_binary.run();
			}

			// / Left GlobusPallidus ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftGlobusPallidus; // upperThres;

			if (flagLeftGlobusPallidus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_LeftGlobusPallidus_binary = (ModelImage) regImage_LeftGlobusPallidus
						.clone();
				regImage_LeftGlobusPallidus_binary
						.setType(ModelStorageBase.BOOLEAN);
				regImage_LeftGlobusPallidus_binary
						.setImageName(regImage_LeftGlobusPallidus
								.getImageName() + "_binary");
				algorThreshold_LeftGlobusPallidus_reg_binary = new AlgorithmThresholdDual(
						regImage_LeftGlobusPallidus_binary,
						regImage_LeftGlobusPallidus, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_LeftGlobusPallidus_reg_binary
						.addListener(this);
				algorThreshold_LeftGlobusPallidus_reg_binary.run();
			}

			// / Right GlobusPallidus ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightGlobusPallidus; // upperThres;

			if (flagRightGlobusPallidus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_RightGlobusPallidus_binary = (ModelImage) regImage_RightGlobusPallidus
						.clone();
				regImage_RightGlobusPallidus_binary
						.setType(ModelStorageBase.BOOLEAN);
				regImage_RightGlobusPallidus_binary
						.setImageName(regImage_RightGlobusPallidus
								.getImageName() + "_binary");
				algorThreshold_RightGlobusPallidus_reg_binary = new AlgorithmThresholdDual(
						regImage_RightGlobusPallidus_binary,
						regImage_RightGlobusPallidus, thresholds, fillValue,
						outputType, regionFlag, isInverse);
				algorThreshold_RightGlobusPallidus_reg_binary
						.addListener(this);
				algorThreshold_RightGlobusPallidus_reg_binary.run();
			}

			// / Left Thalamus ***
			thres1 = 1.0f; // lowerThres;
			thres2 = LeftThalamus; // upperThres;

			if (flagLeftThalamus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_LeftThalamus_binary = (ModelImage) regImage_LeftThalamus
						.clone();
				regImage_LeftThalamus_binary.setType(ModelStorageBase.BOOLEAN);
				regImage_LeftThalamus_binary.setImageName(regImage_LeftThalamus
						.getImageName() + "_binary");
				algorThreshold_LeftThalamus_reg_binary = new AlgorithmThresholdDual(
						regImage_LeftThalamus_binary, regImage_LeftThalamus,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_LeftThalamus_reg_binary
						.addListener(this);
				algorThreshold_LeftThalamus_reg_binary.run();
			}

			// Right Thalamus ***
			thres1 = 1.0f; // lowerThres;
			thres2 = RightThalamus; // upperThres;

			if (flagRightThalamus) {
				thresholds[0] = thres1;
				thresholds[1] = thres2;
				regImage_RightThalamus_binary = (ModelImage) regImage_RightThalamus
						.clone();
				regImage_RightThalamus_binary.setType(ModelStorageBase.BOOLEAN);
				regImage_RightThalamus_binary.setImageName(myImage
						.getImageName() + "_RightThalamus_binar");
				algorThreshold_RightThalamus_reg_binary = new AlgorithmThresholdDual(
						regImage_RightThalamus_binary, regImage_RightThalamus,
						thresholds, fillValue, outputType, regionFlag,
						isInverse);
				algorThreshold_RightThalamus_reg_binary
						.addListener(this);
				algorThreshold_RightThalamus_reg_binary.run();
			}

		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog threshold: unable to allocate enough memory");

			return;

		}
	}
	
	/** 
	 * Generate the statistics report. 
	 * @param rootDirectory
	 */
	public void statisticsDataGeneration(String rootDirectory) {

		File rootDir = new File(rootDirectory);
		if (!rootDir.isDirectory()) {
			rootDir.mkdir();
		}
		if (flagLeftHippocampus) {
			compare(image_LeftHippocampus_binary,
					regImage_LeftHippocampus_binary, "LeftHippocampus",
					rootDirectory);
		}

		if (flagRightHippocampus) {
			compare(image_RightHippocampus_binary,
					regImage_RightHippocampus_binary, "RightHippocampus",
					rootDirectory);
		}

		if (flagLeftAmygdala) {
			compare(image_LeftAmygdala_binary, regImage_LeftAmygdala_binary,
					"LeftAmygdala", rootDirectory);
		}

		if (flagRightAmygdala) {
			compare(image_RightAmygdala_binary, regImage_RightAmygdala_binary,
					"RightAmygdala", rootDirectory);
		}

		if (flagLeftCaudate) {
			compare(image_LeftCaudate_binary, regImage_LeftCaudate_binary,
					"LeftCaudate", rootDirectory);
		}

		if (flagRightCaudate) {
			compare(image_RightCaudate_binary, regImage_RightCaudate_binary,
					"RightCaudate", rootDirectory);
		}

		if (flagLeftPutamen) {
			compare(image_LeftPutamen_binary, regImage_LeftPutamen_binary,
					"LeftPutamen", rootDirectory);
		}

		if (flagRightPutamen) {
			compare(image_RightPutamen_binary, regImage_RightPutamen_binary,
					"RightPutamen", rootDirectory);
		}

		if (flagLeftGlobusPallidus) {
			compare(image_LeftGlobusPallidus_binary,
					regImage_LeftGlobusPallidus_binary, "LeftGlobusPallidus",
					rootDirectory);
		}

		if (flagRightGlobusPallidus) {
			compare(image_RightGlobusPallidus_binary,
					regImage_RightGlobusPallidus_binary, "RightGlobusPallidus",
					rootDirectory);
		}

		if (flagLeftThalamus) {
			compare(image_LeftThalamus_binary, regImage_LeftThalamus_binary,
					"LeftThalamus", rootDirectory);
		}
		if (flagRightThalamus) {
			compare(image_RightThalamus_binary, regImage_RightThalamus_binary,
					"RightThalamus", rootDirectory);
		}
	}
	
	/** 
	 * Generate the statistic data. 
	 * @param srcImage    source image
	 * @param targetImage   target image
	 * @param sectionName   compared section name
	 * @param rootDirectory  root directory
	 */
	public void compare(ModelImage srcImage, ModelImage targetImage, String sectionName, String rootDirectory) {
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int zDim = srcImage.getExtents()[2];
		
		int volSize = xDim * yDim * zDim;
		
		int[] sourceBuffer = new int[volSize];
		int[] targetBuffer = new int[volSize];
		
	    float[] buffer;
	    
	    int length = 4 * volSize;
		int L1 = 0, L2 = 0, L1andL2 = 0;
	    
		
	    ModelImage destImage = new ModelImage(ModelStorageBase.ARGB, srcImage.getExtents(), sectionName+"_comparedRGB");
		/*
	    FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[destImage.getExtents()[2]];
	    for (int i = 0; i < fileInfoBases.length; i++) {
	    	 fileInfoBases[i] = new FileInfoImageXML(destImage.getImageName(), null, FileUtility.XML);
	            fileInfoBases[i].setEndianess(srcImage.getFileInfo()[0].getEndianess());
	            fileInfoBases[i].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
	            fileInfoBases[i].setResolutions(srcImage.getFileInfo()[0].getResolutions());
	            fileInfoBases[i].setExtents(srcImage.getExtents());
	            fileInfoBases[i].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation());
	            fileInfoBases[i].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
	            fileInfoBases[i].setOrigin(srcImage.getFileInfo()[0].getOrigin());
	            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);       
	    }
	    destImage.setFileInfo(fileInfoBases);
        */
	    for (int i = 0; i < srcImage.getExtents()[2]; i++) {
	    
		    boolean endianness = srcImage.getFileInfo(i).getEndianess();
	        int modality = srcImage.getFileInfo(i).getModality();
	        float[] res = srcImage.getFileInfo(i).getResolutions();
	        int[] exts = srcImage.getFileInfo(i).getExtents();
	        int[] units = srcImage.getFileInfo(i).getUnitsOfMeasure();
			int imageOrient = srcImage.getFileInfo(i).getImageOrientation();
			int []axisOrient = srcImage.getFileInfo(i).getAxisOrientation();
			float[] origin = srcImage.getFileInfo(i).getOrigin();
			// System.err.println("origin[0] = " + origin[0] + "origin[1] = " + origin[1] + "origin[2] = " + origin[2]);
			
			
			FileInfoImageXML fileInfo = new FileInfoImageXML(sectionName+"_comparedRGB", null, FileUtility.XML);
			/// fileInfo.setDataType(ModelStorageBase.FLOAT);    
			fileInfo.setEndianess(endianness);
	        fileInfo.setExtents(exts);
	        fileInfo.setModality(modality);
	        fileInfo.setResolutions(res);
	        fileInfo.setUnitsOfMeasure(units);
	        fileInfo.setImageOrientation(imageOrient);
			fileInfo.setAxisOrientation(axisOrient);
			fileInfo.setOrigin(origin);
        
		 
			destImage.setFileInfo(fileInfo, i);
         }
        
		
        // FileInfoImageXML[] fileInfos = {fileInfo};
        // destImage.setFileInfo(fileInfos);
	    
		try {
			
			srcImage.exportData(0, volSize, sourceBuffer);
			targetImage.exportData(0, volSize, targetBuffer);
		
			buffer = new float[length]; 
			
			for ( int i = 0, j = 0; i < volSize; i++, j+=4 ) {
				if ( sourceBuffer[i] == 0 && targetBuffer[i] == 0 ) {
					buffer[j] = 0; buffer[j+1] = 0;buffer[j+2] = 0;buffer[j+3] = 0;
				} else if ( sourceBuffer[i] == 1 && targetBuffer[i] == 1 ) {
					// overlappaed region
					L1andL2++;
					L1++;
					L2++;
					buffer[j] = 0; buffer[j+1] = 255;buffer[j+2] = 0;buffer[j+3] = 0;
				} else if ( sourceBuffer[i] == 1 && targetBuffer[i] != 1 ) {
					// belong to srcImage,  Green color
					L1++;
					buffer[j] = 0; buffer[j+1] = 0;buffer[j+2] = 255;buffer[j+3] = 0;
				} else if ( sourceBuffer[i] != 1 && targetBuffer[i] == 1 ) {
					// belong to target Image,  Blue color
					L2++;
					buffer[j] = 0; buffer[j+1] = 0;buffer[j+2] = 0;buffer[j+3] = 255;			
				}
			}
			
			destImage.importData(0, buffer, false);
			destImage.calcMinMax();
			// new ViewJFrameImage(destImage);
			saveComparedImage(destImage, rootDirectory);
			
			// Overlap:            		   V(L1 & L2 ) 
			//           O(L1, L2) = -----------------------  * 100% 
			//                           (V(L1) + V(L2))/2
			
			double overlapInPercent =   L1andL2 / (( L1 + L2) / 2d) * 100;
			
			// Difference:        		| V(L1) - V(L2) |
			//           D(L1, L2) = ------------------------  * 100%
			//                         (V(L1) + V(L2)) / 2
			
			double differenceInPercent = Math.abs(L1 - L2) /  ( (L1 + L2) / 2d ) * 100;
			
			myData.add(new StatisticsData(sectionName, overlapInPercent, differenceInPercent));
			
			// destImage.disposeLocal();
			// destImage = null;
			
		} catch ( IOException e ) {
			MipavUtil.displayError("IOException on srcImage.exportData(0, volSize, sourceBuffer).");
		}
	}

	/** 
	 * Get the case number
	 * @return  case number. 
	 */
	public int getCaseNumber() {
		return caseNumber;
	}
	
	/** 
	 * Print report in PDF file format. 
	 * @param rootDirectory
	 */
	public void printReport(String rootDirectory) {

	   File rootDir = new File(rootDirectory);
  	   if ( !rootDir.isDirectory()) {
  		   rootDir.mkdir();
  	   }
		
		String fileDir = rootDirectory + File.separator + "StatisticReport"
				+ File.separator;

		File fileDirectory = new File(fileDir);
		if (!fileDirectory.isDirectory()) {
			fileDirectory.mkdir();
		}

		try {
			FileWriter writer = new FileWriter(fileDir + "report.csv");
			
			writer.append(" ");
			writer.append(',');
			writer.append("Overlap");
			writer.append(',');
			writer.append("Difference");
			writer.append('\n');
			
			for (int i = 0; i < myData.size(); i++) {

				StatisticsData data = myData.get(i);

				writer.append(data.sectionName);
				writer.append(',');
				writer.append(String.valueOf(data.overlap) + "%");
				writer.append(',');
				writer.append(String.valueOf(data.difference) + "%");
				writer.append('\n');

			}
			
			writer.flush();
		    writer.close();
			
			/*
			Document document = new Document();
			PdfWriter.getInstance(document, new FileOutputStream(fileDir
					+ "reportPDFTable.pdf"));
			document.open();

			// Amygdala(Am), Caudate(Ca), Hippocampus(Hp), Pallidum(Pa),
			// Putaman(Pu), Thalamus(Th)

			PdfPTable table = new PdfPTable(3);

			// Add the title lines
			table.addCell(" ");
			table.addCell("Overlap");
			table.addCell("Difference");

			for (int i = 0; i < myData.size(); i++) {

				StatisticsData data = myData.get(i);

				table.addCell(data.sectionName);
				table.addCell(String.valueOf(data.overlap) + "%");
				table.addCell(String.valueOf(data.difference) + "%");

			}

			document.add(table);
			document.close();
			*/
			
		} catch (Exception e) {
			e.printStackTrace();
		}

	}
	
	/** 
	 * Save the color RGB comparison images. 
	 * @param comparedImage  color image for comparison. 
	 * @param rootDirectory output directory specified from command line. 
	 */
	  public void saveComparedImage(ModelImage comparedImage, String rootDirectory) {
	    	 
	    	String fileDir = rootDirectory + File.separator + "StatisticComparison" + File.separator;
	    	
	    	File fileDirectory = new File(fileDir);
			if (!fileDirectory.isDirectory()) {
				fileDirectory.mkdir();
			}
			
	    	String fileName = comparedImage.getImageFileName();
	    	int fileType = comparedImage.getType();
	    	
	    	// parentDialog.updateFileInfo(myImage, comparedImage);
	    	
	    	comparedImage.saveImage(fileDir, fileName, fileType, true);
	  }

	  /** 
	   * Class for statistics data.     
	   * @author Ruida
	   *
	   */
	  class StatisticsData {
		  
		  String sectionName;
		  double overlap;
		  double difference;
		  
		  public StatisticsData(String _sectionName, double _overlap, double _difference) {
			  sectionName = _sectionName;
			  overlap = _overlap;
			  difference = _difference;
		  }
		  
	  }
	  
}


/**
 * Class to record which two cases to compare
 * @author ruida
 */
class caseComparison {
	
	public int firstCase;
	public int secondCase;
	
	public caseComparison(int _firstCase, int _secondCase) {
		firstCase = _firstCase;
		secondCase = _secondCase;
	}
}