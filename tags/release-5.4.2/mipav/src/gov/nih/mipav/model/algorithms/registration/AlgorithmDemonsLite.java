package gov.nih.mipav.model.algorithms.registration;

//import edu.jhmi.rad.Mipav.dialogs.*;
//import edu.jhmi.rad.Mipav.methods.*;
//import edu.jhmi.rad.Mipav.utilities.*;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.io.*;

 /**
 *
 *   Algorithm for non-linear registration with the DEMONS algorithm.
 *   *  (Demons variant from Tom Vercauteren's 2009 paper, 
 *   "Diffeomorphic demons: Efficient non-parametric image registration by
 *   Tom Vercauteren, Xavier Pennec, Aymeric Perchant, and Nicholas Ayache,
 *   NeuroImage, 45, 2009, S61-S72.)
 *
 *	@version    Feb 2005
 *	@author     Pierre-Louis Bazin
 *  @see 		JDialogDemons
 *		
 *
*/
public class AlgorithmDemonsLite extends AlgorithmBase {

    // Fuzzy images require 1 image for each class
    // Hard images 1 image with assigned clusters
    private ModelImage srcImage;
    private ModelImage targetImage;
    private ModelImage[] destImage;
	private String output;

	private int nix,niy,niz;
	private float rix,riy,riz;
    private int ntx,nty,ntz;
	private float rtx,rty,rtz;
    
    // algorithm parameters
	private DemonsRegistrationLite algorithm;
	private float smoothing;
	private float scale;
	private int levels;
	private int	Ni;
	private int	regType;
	
    // Test images for 2D registration
    private ModelImage circleImage;
    private ModelImage cImage;
	
    /**
    *	Constructor for 3D images in which changes are placed in a predetermined destination image.
    *   @param destImg      Image model where result image is to stored.
    *   @param srcImg       Source image model.
    */
	public AlgorithmDemonsLite(ModelImage srcImg, ModelImage targetImg, 
									int levels_, int Ni_, float smooth_, float scale_,
									String reg_, 
									String out_) {
			
		super(null, srcImg);
		srcImage = srcImg;
        targetImage = targetImg;  // Put results in destination image.
        output = out_;
        
		if (reg_.equals("diffusion-like")) {
			regType = DemonsRegistrationLite.DIFFUSION;
			//MipavUtil.displayMessage("regularization: diffusion-likee \n");
		} else {
			regType = DemonsRegistrationLite.FLUID;
			//MipavUtil.displayMessage("regularization: fluid-likee \n");
		}
		
		levels = levels_;
		Ni = Ni_;
        smoothing = smooth_;
		scale = scale_;
	}

    /**
    *	Prepares this class for destruction.
    */
	public void finalize(){
        targetImage = null;
	    destImage   = null;
	    srcImage    = null;
        super.finalize();
        System.gc();
	}
	
	/** for output of results */
	public ModelImage[] getResultImages() { return destImage; }

    /**
    *   Starts the algorithm.
    */
	public void runAlgorithm() {
        int nDims;
        boolean testImages = false;
        
        if (testImages) {
            generateTestImages();
            return;
        }

        if (srcImage  == null) {
            displayError("Source Image is null");
            return;
        }
        if (targetImage  == null) {
            displayError("Target Image is null");
            return;
        }
		// start the timer to compute the elapsed time
        setStartTime();

		fireProgressStateChanged(srcImage.getImageName(), "Extracting data ...");
		fireProgressStateChanged(10);
		
		// init dimensions
        nDims = srcImage.getNDims();
		nix = srcImage.getExtents()[0];
		niy = srcImage.getExtents()[1];
        if (nDims > 2) {
		    niz = srcImage.getExtents()[2];
        }
        else {
            niz = 1;
        }

		ntx = targetImage.getExtents()[0];
		nty = targetImage.getExtents()[1];
        if (nDims > 2) {
		    ntz = targetImage.getExtents()[2];
        }
        else {
            ntz = 1;
        }
   
		float[] image; 
		float[] target;
		try {
			// retrieve all data: for each volume
			image = new float[nix*niy*niz];
			srcImage.exportData(0, nix*niy*niz, image); // locks and releases lock
			target = new float[ntx*nty*ntz];
			targetImage.exportData(0, ntx*nty*ntz, target); // locks and releases lock
		} catch (IOException error) {
			image = null;
			target = null;
			errorCleanUp("Algorithm: source image locked", true);
			return;
		} catch (OutOfMemoryError e){
			image = null;
			target = null;
			errorCleanUp("Algorithm: Out of memory creating process buffer", true);
			return;
		}

		// resolution
		rix = srcImage.getFileInfo()[0].getResolutions()[0];
		riy = srcImage.getFileInfo()[0].getResolutions()[1];
        if (nDims > 2) {
		    riz = srcImage.getFileInfo()[0].getResolutions()[2];
        }
        else {
            riz = 1.0f;
        }
		
		rtx = targetImage.getFileInfo()[0].getResolutions()[0];
		rty = targetImage.getFileInfo()[0].getResolutions()[1];
        if (nDims > 2) {
		    rtz = targetImage.getFileInfo()[0].getResolutions()[2];
        }
        else {
            rtz = 1.0f;
        }
		
		
		// main algorithm
		fireProgressStateChanged(srcImage.getImageName(), "Main computations ...");
		fireProgressStateChanged(40);

		algorithm = new DemonsRegistrationLite(image, target, 
											nix, niy, niz, rix, riy, riz, 
											ntx, nty, ntz, rtx, rty, rtz,
											smoothing, scale, levels, Ni, 
											regType, null);
		
		algorithm.runGaussianPyramid();
		
		// output
		try {
			fireProgressStateChanged(srcImage.getImageName(), "Output ...");
			fireProgressStateChanged(80);
			
			if (output.equals("transformed_image")) {
				int[] destExtents = targetImage.getExtents();
				
				// create the output array
				destImage = new ModelImage[1];
			
				destImage[0] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
														JDialogBase.makeImageName(srcImage.getImageName(), "_DemonsLite_reg"));				

				destImage[0].importData(0, algorithm.exportTransformedImage(), true);
			} else if  (output.equals("transformation_field")) {
				int[] destExtents = new int[nDims+1];
				for (int n=0;n<nDims;n++) destExtents[n] = targetImage.getExtents()[n];
				destExtents[nDims] = nDims;
				
				// create the output array
				destImage = new ModelImage[1];
			
				destImage[0] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
														JDialogBase.makeImageName(srcImage.getImageName(), "_DemonsLite_vec"));				

				destImage[0].importData(0, algorithm.exportTransformField(), true);
			} else if  (output.equals("all_images")) {
				// create the output array
				destImage = new ModelImage[2];
			
				// transformed image
				int[] destExtents = targetImage.getExtents();
				
				destImage[0] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
														JDialogBase.makeImageName(srcImage.getImageName(), "_DemonsLite_reg"));				

				destImage[0].importData(0, algorithm.exportTransformedImage(), true);
				
				// transformation
				destExtents = new int[nDims+1];
				for (int n=0;n<nDims;n++) destExtents[n] = targetImage.getExtents()[n];
				destExtents[nDims] = nDims;
				
				destImage[1] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
														JDialogBase.makeImageName(srcImage.getImageName(), "_DemonsLite_vec"));				

				destImage[1].importData(0, algorithm.exportTransformField(), true);
			}
		} catch (OutOfMemoryError e) {
			errorCleanUp("DemonsLite: Out of memory creating hard classification", true);
			System.err.println(e.getMessage());
			finalize();
			
			setCompleted(false);
			return;
		} catch (IOException error) {
			errorCleanUp("DemonsLite: export problem to destImage[]", true);
			System.err.println(error.getMessage());
			finalize();
			
			setCompleted(false);
			return;
		}
		algorithm.finalize();
		algorithm = null;

		fireProgressStateChanged(90);
		
		setCompleted(true);
		threadStopped = false;
       // compute the elapsed time
        computeElapsedTime();
    } // end runAlgorithm()
    
    /**
     * A test of the diffeomorphic demons is to transform a circle image to a letter C image.
     *
     */
    public void generateTestImages() {
        int x, y;
        int xDist;
        int yDist;
        int yDistSquared;
        int radius = 128;
        int yoff;
        int radiusSquared = radius * radius;
        int extents[] = new int[2];
        int innerRad = 64;
        int innerRadSquared = innerRad * innerRad;
        int halfCOpen = (int)Math.round(innerRad/2.5);
        extents[0] = 512;
        extents[1] = 512;
        int sliceSize = extents[0] * extents[1];
        float buffer[] = new float[sliceSize];
        for (y = 0; y < extents[1]; y ++) {
            yoff = y * extents[0];
            yDist = y - 256;
            yDistSquared = yDist * yDist;
            for (x = 0; x < extents[0]; x++) {
                xDist = x - 256;
                if ((xDist*xDist + yDistSquared) <= radiusSquared) {
                    buffer[x + yoff] = 100.0f;
                }
            }
        }
        circleImage = new ModelImage(ModelStorageBase.FLOAT, extents, "circleImage");
        try {
            circleImage.importData(0, buffer, true);
        }
        catch (IOException e) {
            displayError("IOException on circleImage.importData");
            return;
        }
        new ViewJFrameImage(circleImage);
        for (y = 0; y < extents[1]; y ++) {
            yoff = y * extents[0];
            yDist = y - 256;
            yDistSquared = yDist * yDist;
            for (x = 0; x < extents[0]; x++) {
                xDist = x - 256;
                if ((xDist*xDist + yDistSquared) <= innerRadSquared) {
                    buffer[x + yoff] = 0.0f;
                }
                else if ((x > 256) && (Math.abs(y - 256) < halfCOpen)) {
                    buffer[x + yoff] = 0.0f;
                }
            }
        }
        cImage = new ModelImage(ModelStorageBase.FLOAT, extents, "cImage");
        try {
            cImage.importData(0, buffer, true);
        }
        catch (IOException e) {
            displayError("IOException on cImage.importData");
            return;
        }
        new ViewJFrameImage(cImage);
        return;
    } // public void generateTestImages

}
