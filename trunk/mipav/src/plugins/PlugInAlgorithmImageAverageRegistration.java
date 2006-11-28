import java.io.File;
import java.util.ArrayList;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.model.structures.ModelStorageBase;

/**
 * @author pandyan
 * 
 * This class is the algorithm that loops through the
 * src images and calls the AlgorithmRegOAR3D algorithm
 * to register each image....it then averages all the registered
 * images to produce a final registered image
 *
 */
public class PlugInAlgorithmImageAverageRegistration extends AlgorithmBase{
	
	/** ModelImages*/
    private ModelImage sourceImage, targetImage, registeredImage, resultImage, bufferImage;
    /** ArrayList for each registered image */
    private ArrayList registeredImages = new ArrayList();
    /** ints needed for algorithm */
    private int cost, interp, interp2, DOF;
    /** floats needed for algorithm..x direction*/
    private float rotateBeginX, rotateEndX, coarseRateX, fineRateX;
    /** floats needed for algorithm..y direction*/
    private float rotateBeginY, rotateEndY, coarseRateY, fineRateY;
    /** floats needed for algorithm..z direction*/
    private float rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;
    /** boolean of maxOfMin Resol */
    private boolean maxOfMinResol;
    /** boolean that decides if target imgae should be used in final avcerage calculation*/
    boolean includeTargetImageinCalc;
    /** boolean for subsample! */
    private boolean doSubsample = true; 
    /** Dboolean for fast mode */
    private boolean fastMode = false;
    /** default ints */
    private int maxIterations = 2, bracketBound = 10, numMinima = 3;
    /** handle to the registration algorithm */
	private AlgorithmRegOAR3D algReg3D;
	/** handle to AlgorithmImageCalculator */
	private AlgorithmImageCalculator algCalc;
	/** handle to progress bar */
	private ViewJProgressBar progressBar;
	/** handle to AlgorithmImageMath */
	private AlgorithmImageMath algMath;
	/** List of source filenames */
	private ArrayList srcFilenamesArrList;
	/** boolean indicating target color value */
	private boolean doColor;
	/** boolean indicating whether to save intermediate registered images */
	private boolean saveIntermediateRegImages;
	/** default directory **/
	private String directory;
	/** handle to ViewUserInterface */
	private ViewUserInterface UI;
	
	public PlugInAlgorithmImageAverageRegistration(ArrayList srcFilenamesArrList, ModelImage targetImage, int cost, int DOF, 
												   int interp, int interp2, float rotateBeginX, float rotateEndX, float coarseRateX,
												   float fineRateX, float rotateBeginY, float rotateEndY,float coarseRateY,
												   float fineRateY, float rotateBeginZ, float rotateEndZ,float coarseRateZ,
												   float fineRateZ, boolean maxOfMinResol, boolean includeTargetImageinCalc,
												   boolean saveIntermediateRegImages, boolean doColor) {
												   
		this.srcFilenamesArrList = srcFilenamesArrList;
		this.targetImage = targetImage;
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
		this.fineRateZ= fineRateZ;
		this.maxOfMinResol = maxOfMinResol;
		this.includeTargetImageinCalc = includeTargetImageinCalc;
		this.saveIntermediateRegImages = saveIntermediateRegImages;
		this.doColor = doColor;
		
		UI = ViewUserInterface.getReference();
		directory = UI.getDefaultDirectory();
		
	}
	
	
	/**
	 * runAlgorithm
	 */
	public void runAlgorithm() {
		//read in each src image and then run algorithm
		int targetNumDims = targetImage.getNDims();
		Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
		Preferences.debug("*** Beginning Image Average Registration ***\n",Preferences.DEBUG_ALGORITHM);
		Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
		Preferences.debug("*** Target image name is " + targetImage.getImageFileName() + "\n",Preferences.DEBUG_ALGORITHM);
		Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
		Preferences.debug("*** Number of source images is " + srcFilenamesArrList.size() + "\n",Preferences.DEBUG_ALGORITHM);
		for(int i=0;i<srcFilenamesArrList.size();i++) {
			//get the directory and the filename
			String fullPath = (String)srcFilenamesArrList.get(i);
			int sepIndex = fullPath.lastIndexOf(File.separatorChar);
			String directory = fullPath.substring(0, sepIndex + 1);
			String filename = fullPath.substring(sepIndex + 1, fullPath.length());
			Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
			Preferences.debug("*** Source image name(" + (i+1) + ") is " + filename + "\n",Preferences.DEBUG_ALGORITHM);
			//first check to see if this image is an undefined type because
			//if it is, we do not want the undefined dialog to pop up 
			//We will just ignore this image in the average algorithm...but we will log it
			FileIO fileIO = new FileIO();
			int fileType = fileIO.getFileType(filename, directory, false); 
			if(fileType == FileUtility.ERROR) {
				//log error in obtaining file type
				Preferences.debug("  * Error in obtaining file type of " + filename + "\n",Preferences.DEBUG_ALGORITHM);
				Preferences.debug("  * " + filename + " will be ignored \n",Preferences.DEBUG_ALGORITHM);
				continue;
			}
			else if(fileType == FileUtility.UNDEFINED) {
				//log that this filetype is undefined
				Preferences.debug("  * Undefined file type for " + filename + "\n",Preferences.DEBUG_ALGORITHM);
				Preferences.debug("  * " + filename + " will be ignored \n",Preferences.DEBUG_ALGORITHM);
				continue;
			}
			else {
				//read in the image
				sourceImage = fileIO.readImage(filename, directory, false, null); 
				if (sourceImage == null) {
                    //log problem that there was an error reading in the file
					Preferences.debug("  * Error reading in file for " + filename + "\n",Preferences.DEBUG_ALGORITHM);
					Preferences.debug("  * " + filename + " will be ignored \n",Preferences.DEBUG_ALGORITHM);
                    continue;
                }
				if(sourceImage.isColorImage() != doColor) {
					//log issue that this source image and the target image are not the same color properties	
					Preferences.debug("  * Color type of " + filename + " does not match target image \n",Preferences.DEBUG_ALGORITHM);
					Preferences.debug("  * " + filename + " will be ignored \n",Preferences.DEBUG_ALGORITHM);
					if(sourceImage != null) {
		            	 sourceImage.disposeLocal();
		            	 sourceImage = null;
		             }
					continue;
				}
				if(sourceImage.getNDims() != targetNumDims) {
					//log issue that this source image and the target image are not the same dimensions
					Preferences.debug("  * Dimensions of " + filename + " do not match target image \n",Preferences.DEBUG_ALGORITHM);
					Preferences.debug("  * " + filename + " will be ignored in registration calculation \n",Preferences.DEBUG_ALGORITHM);
					if(sourceImage != null) {
		            	 sourceImage.disposeLocal();
		            	 sourceImage = null;
		             }
					continue;
				}
				
				Preferences.debug("  * " + filename + " read in successfully \n",Preferences.DEBUG_ALGORITHM);
				Preferences.debug("  * Beginning registration of "  + filename + " to " + targetImage.getImageFileName() + "\n",Preferences.DEBUG_ALGORITHM);
				
				AlgorithmTransform transform = null;
				algReg3D = new AlgorithmRegOAR3D(targetImage, sourceImage, cost, DOF, interp, rotateBeginX, rotateEndX,
		                coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY,
		                rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol,
		                doSubsample, fastMode, bracketBound, maxIterations, numMinima);
				
				progressBar = new ViewJProgressBar(sourceImage.getImageName(), "", 0, 100, true);
		        progressBar.setSeparateThread(true);
		        algReg3D.addProgressChangeListener(progressBar);
		        algReg3D.setProgressValues(0, 100);
				
		        algReg3D.run();
				
				TransMatrix finalMatrix = algReg3D.getTransform();
				 
				 int xdimA = targetImage.getExtents()[0];
	             int ydimA = targetImage.getExtents()[1];
	             int zdimA = targetImage.getExtents()[2];
	             float xresA = targetImage.getFileInfo(0).getResolutions()[0];
	             float yresA = targetImage.getFileInfo(0).getResolutions()[1];
	             float zresA = targetImage.getFileInfo(0).getResolutions()[2];
				
	             transform = new AlgorithmTransform(sourceImage, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
	                     ydimA, zdimA, false, false, false);
	             
	             transform.setUpdateOriginFlag(true);
	             transform.run();
	             registeredImage = transform.getTransformedImage();
	             transform.finalize();
	             registeredImage.calcMinMax();
	             
	             //add registered imgage to the array
	             registeredImages.add(registeredImage);
	             
	             Preferences.debug("*** " + filename + " registered successfully \n",Preferences.DEBUG_ALGORITHM);
	             
	             //this is where we will save the intermediate registered image if user checked the checkbox
	             if(saveIntermediateRegImages) {
	            	String imageName = registeredImage.getImageFileName() + ".xml";
	            	FileWriteOptions options = new FileWriteOptions(imageName,directory,true);
	            	if(registeredImage.getExtents().length == 3) {
	            		options.setBeginSlice(0);
	         			options.setEndSlice(registeredImage.getExtents()[2] - 1);
	         		}
	         		else if(registeredImage.getExtents().length == 4) {
	         			options.setBeginSlice(0);
	         			options.setEndSlice(registeredImage.getExtents()[2] - 1);
	         			options.setBeginTime(0);
	         			options.setEndTime(registeredImage.getExtents()[3] - 1);
	         		}
	            	options.setOptionsSet(true);
	            	FileIO io = new FileIO();
	            	io.writeImage(registeredImage, options);
	    			Preferences.debug("*** Intermediate registered image saved to " + directory + imageName + "\n",Preferences.DEBUG_ALGORITHM);
	             }

	             if(sourceImage != null) {
	            	 sourceImage.disposeLocal();
	            	 sourceImage = null;
	             }	
			}	
		}
		
		if(registeredImages.size() == 0) {
			//no registered images
			Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
			Preferences.debug("*** No images were registered \n",Preferences.DEBUG_ALGORITHM);
			return;
		}
		
		
		//if user only supplied 1 source image and doesnt want to average the target image...just return the registered image
		if(registeredImages.size() == 1 && !includeTargetImageinCalc) {
			resultImage = (ModelImage)registeredImages.get(0);
			resultImage.setImageName("Final Registered Image");
			return;
		}
		
		

		Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
		Preferences.debug("*** Beginning image averaging \n",Preferences.DEBUG_ALGORITHM);
		
		
		//if user wants to average in the target image also, we need to add the target image to the list of Registered Images
		if(includeTargetImageinCalc) {
			Preferences.debug("  * Target image will be included in averaging \n",Preferences.DEBUG_ALGORITHM);
			registeredImages.add(targetImage);
		}
		
		Preferences.debug("  * Total number of images being averaged is " + registeredImages.size() + "\n",Preferences.DEBUG_ALGORITHM);
		

		//the extents size and type are from the target image
		int[] extents = targetImage.getExtents();
		int srcType = targetImage.getType();
		
		double size = new Integer(registeredImages.size()).doubleValue();
		double imag = 0.0;
		
		
		//now we need to average (add all and then divide) all the registered images
		bufferImage = new ModelImage(ModelStorageBase.FLOAT,extents,"Buffer Image");
		for(int k=0;k<registeredImages.size();k++) {
			algCalc = new AlgorithmImageCalculator(bufferImage,(ModelImage)registeredImages.get(k),bufferImage,AlgorithmImageCalculator.AVERAGE,
					AlgorithmImageCalculator.PROMOTE,true,"");
			algCalc.run();
			bufferImage = algCalc.getDestImage();		
		}
		resultImage = new ModelImage(srcType,extents,"Final Registered Image");
		algMath = new AlgorithmImageMath(resultImage,bufferImage,AlgorithmImageMath.DIVIDE,size,imag,AlgorithmImageCalculator.PROMOTE,true);
		algMath.run();
		resultImage = algMath.getDestImage();
		
		Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
		Preferences.debug("*** Image averaging complete \n",Preferences.DEBUG_ALGORITHM);
		
		for(int k=0;k<registeredImages.size();k++) {
			if ((ModelImage)registeredImages.get(k) != null) {
				((ModelImage)registeredImages.get(k)).disposeLocal();
				registeredImages.set(k,null);
			}
		}
		if(targetImage != null) {
			targetImage.disposeLocal();
			targetImage = null;
		}
		
		if(bufferImage != null) {
			bufferImage.disposeLocal();
			bufferImage = null;
		}
		

	}


	/**
	 * getResultImage
	 * returns the resultImage
	 * @return ModelImage
	 */
	public ModelImage getResultImage() {
		return resultImage;
	}

	
	
	

}
