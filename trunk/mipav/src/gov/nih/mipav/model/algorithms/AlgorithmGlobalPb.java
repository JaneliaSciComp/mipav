package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;

/**
 * Compute globalPb, the globalized probability of boundary of an image
 * Reference: Contour Detection and Hierarchical Image Segmentation by Pablo Arbelaez, Michael Maire,
 * Charless Fowlkes, and Jitendra Malik, IEEE Transactions on Pattern Analysis and Machine Intelligence,
 * Volume 33, Issue 5, August, 2010, pp. 898-916.
 * 
 * BSR package code written in MATLAB and C++ is ported to Java with the permission of 
 * Pablo Andres Arbelaez Escalante.
 */

public class AlgorithmGlobalPb extends AlgorithmBase {
	
	// Resizing factor in (0, 1], to speed up eigenvector
	private double rsz = 1.0;
	
	private int xDim;
	
	private int yDim;
	
	private int sliceSize;
	
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmGlobalPb - default constructor.
     */
    public AlgorithmGlobalPb() { }
    
    /**
     * 
     * @param destImg
     * @param srcImg
     */
    public AlgorithmGlobalPb(ModelImage destImg, ModelImage srcImg) {
    	super(destImg, srcImg);
    }
    
    /**
     * Original globalPb.m by Pablo Arbelaez on December, 2010
     */
    public void runAlgorithm() {
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        ModelImage inputImage;
      	AlgorithmChangeType changeTypeAlgo;
      	FileInfoBase[] fileInfo;
      	double weights[] = new double[13];
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        if ((rsz <= 0.0) || (rsz > 1.0)) {
        	MipavUtil.displayError("Resizing factor rsz out of range (0, 1]");
        	setCompleted(false);
        	return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Global Pb ...");
        
        if (srcImage.isColorImage()) {
        	inputImage = new ModelImage(ModelStorageBase.ARGB_FLOAT,
      				srcImage.getExtents(), "changeTypeImage");
        }
        else {
        	inputImage = new ModelImage(ModelStorageBase.DOUBLE,
      				srcImage.getExtents(), "changeTypeImage");
        }
  		inputImage.getFileInfo(0).setEndianess(FileBase.LITTLE_ENDIAN);
  		changeTypeAlgo = new AlgorithmChangeType(inputImage,
				srcImage, srcImage.getMin(),
				srcImage.getMax(), 0.0, 1.0, image25D);
  		changeTypeAlgo.run();
  		changeTypeAlgo.finalize();
  		changeTypeAlgo = null;
  		fileInfo = inputImage.getFileInfo();
        fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(inputImage.getExtents());
        fileInfo[0].setMax(inputImage.getMax());
        fileInfo[0].setMin(inputImage.getMin());
        fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        
        // Default feature weights
        if (srcImage.isColorImage()) {
        	weights[0] = 0.0;
        	weights[1] = 0.0;
        	weights[2] = 0.0039;
        	weights[3] = 0.0050;
        	weights[4] = 0.0058;
        	weights[5] = 0.0069;
        	weights[6] = 0.0040;
        	weights[7] = 0.0044;
        	weights[8] = 0.0049;
        	weights[9] = 0.0024;
        	weights[10] = 0.0027;
        	weights[11] = 0.0170;
        	weights[12] = 0.0074;
        }
        else {
        	weights[0] = 0.0;
        	weights[1] = 0.0;
        	weights[2] = 0.0054;
        	weights[3] = 0.0;
        	weights[4] = 0.0;
        	weights[5] = 0.0;
        	weights[6] = 0.0;
        	weights[7] = 0.0;
        	weights[8] = 0.0;
        	weights[9] = 0.0048;
        	weights[10] = 0.0049;
        	weights[11] = 0.0264;
        	weights[12] = 0.0090;
        }
        
        // mPb
        multiscalePb(inputImage);
        
        setCompleted(true);
        return;
    }
    
    /**
     * Compute local contour cues of an image
     * gradinets by Michael Maire
     * Original MATLAB code by Pablo Arbelaez December, 2010
     * @param im
     */
    private void multiscalePb(ModelImage im) {
        double weights[] = new double[12];
        if (im.isColorImage()) {
            weights[0] = 0.0146;
            weights[1] = 0.0145;
            weights[2] = 0.0163;
            weights[3] = 0.0210;
            weights[4] = 0.0243;
            weights[5] = 0.0287;
            weights[6] = 0.0166;
            weights[7] = 0.0185;
            weights[8] = 0.0204;
            weights[9] = 0.0101;
            weights[10] = 0.0111;
            weights[11] = 0.0141;
        }
        else {
        	weights[0] = 0.0245;
        	weights[1] = 0.0220;
        	weights[2] = 0.0233;
        	weights[3] = 0.0;
        	weights[4] = 0.0;
        	weights[5] = 0.0;
        	weights[6] = 0.0;
        	weights[7] = 0.0;
        	weights[8] = 0.0;
        	weights[9] = 0.0208;
        	weights[10] = 0.0210;
        	weights[11] = 0.0229;
        }
    }
}
