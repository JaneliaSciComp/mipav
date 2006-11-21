import java.util.ArrayList;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.ViewJProgressBar;
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
    /** ArrayList of src Images */
    private ArrayList srcImages;
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
	
	public PlugInAlgorithmImageAverageRegistration(ArrayList srcImages, ModelImage targetImage, int cost, int DOF, 
												   int interp, int interp2, float rotateBeginX, float rotateEndX, float coarseRateX,
												   float fineRateX, float rotateBeginY, float rotateEndY,float coarseRateY,
												   float fineRateY, float rotateBeginZ, float rotateEndZ,float coarseRateZ,
												   float fineRateZ, boolean maxOfMinResol, boolean includeTargetImageinCalc) {
												   
		this.srcImages = srcImages;
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
		
	}
	
	
	/**
	 * runAlgorithm
	 */
	public void runAlgorithm() {
		//rin algorithm for each src image
		for(int i=0;i<srcImages.size();i++) {
			AlgorithmTransform transform = null;
			sourceImage =(ModelImage)srcImages.get(i);
			algReg3D = new AlgorithmRegOAR3D(targetImage, sourceImage, cost, DOF, interp, rotateBeginX, rotateEndX,
	                coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY,
	                rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol,
	                doSubsample, fastMode, bracketBound, maxIterations, numMinima);
			
			progressBar = new ViewJProgressBar(sourceImage.getImageName(), "...", 0, 100, true);
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
             
             //add registered imgae to the array
             registeredImages.add(registeredImage);
		}
		
		
		//if user only supplied 1 source image and doesnt want to average the target image...just return the registered image
		if(srcImages.size() == 1 && !includeTargetImageinCalc) {
			resultImage = registeredImage;
			return;
		}
		
		
		
		//if user wants to average in the target image also, we need to add the target image to the list of Registered Images
		if(includeTargetImageinCalc) {
			registeredImages.add(targetImage);
		}
		

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
