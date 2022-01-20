package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import java.io.IOException;
import java.util.BitSet;



/**
 * The Standard Deviation Threshold works by first having an active VOI in which the standard deviation and other
 * statistics are calculated. The dialog allows the user to enter the number of standard deviations and the values
 * outside the range for the thersholding process. The theresholding can be done on the whole image or alternatively,
 * on other VOIS that are not active that might be on the image. The destination can either be a new image or paint in
 * existing image except if the src image is a color image.
 * 
 * @author pandyan
 */
public class AlgorithmStandardDeviationThreshold extends AlgorithmBase {


	/** max intensities for threshold **/
	private float maxIntensity, maxIntensityR, maxIntensityG, maxIntensityB;
	
	/** min intensities for threshold **/
	private float minIntensity, minIntensityR, minIntensityG, minIntensityB;
	
	/** src image, result image **/
	private ModelImage srcImage, resultImage;
	
	/** values outside intensities for threshold **/
	private float valuesOutside, valuesOutsideR, valuesOutsideG, valuesOutsideB;
	
	/** dimensions and length **/
	private int xDim, yDim, zDim, length;
	
	/** num pixels in threshold **/
	private int numPixels = 0, numPixelsR = 0, numPixelsG = 0, numPixelsB = 0;
	
	/** total volume in threshold **/
	private float totalVolume,totalArea, totalVolumeR, totalVolumeG, totalVolumeB, totalAreaR, totalAreaG, totalAreaB, sumIntensities, sumIntensitiesR, sumIntensitiesG, sumIntensitiesB;
	
	/** resolutions **/
	private float xRes,yRes,zRes;
	
	/** bitset for src image **/
	private BitSet bitset;
	
	/** flags for destination, threshold, inverse, and color image **/
	private boolean newImageDestination, wholeImageThreshold, inverseThreshold, isColorImage;

    /** bitset of vois mask **/
    private BitSet mask;
	
	
	
    /**
     * constructor for grey scale image
     * @param srcImage
     * @param minIntensity
     * @param maxIntensity
     * @param valuesOutside
     * @param newImageDestination
     * @param wholeImageThreshold
     * @param inverseThreshold
     */
	public AlgorithmStandardDeviationThreshold(ModelImage srcImage, float minIntensity, float maxIntensity, float valuesOutside, boolean newImageDestination, boolean wholeImageThreshold, boolean inverseThreshold) {
		this.srcImage = srcImage;
		bitset = srcImage.getParentFrame().getComponentImage().getPaintBitmap();
		this.minIntensity = minIntensity;
		this.maxIntensity = maxIntensity;
		this.valuesOutside = valuesOutside;
		this.newImageDestination = newImageDestination;
		this.wholeImageThreshold = wholeImageThreshold;
		this.inverseThreshold = inverseThreshold;
		isColorImage = false;
	}
	
	/**
	 * constructor for color image
	 * @param srcImage
	 * @param minIntensityR
	 * @param minIntensityG
	 * @param minIntensityB
	 * @param maxIntensityR
	 * @param maxIntensityG
	 * @param maxIntensityB
	 * @param valuesOutsideR
	 * @param valuesOutsideG
	 * @param valuesOutsideB
	 * @param newImageDestination
	 * @param wholeImageThreshold
	 * @param inverseThreshold
	 */
	public AlgorithmStandardDeviationThreshold(ModelImage srcImage, float minIntensityR, float minIntensityG, float minIntensityB, float maxIntensityR, float maxIntensityG, float maxIntensityB, float valuesOutsideR, float valuesOutsideG, float valuesOutsideB, boolean newImageDestination, boolean wholeImageThreshold, boolean inverseThreshold) {
		this.srcImage = srcImage;
		bitset = srcImage.getParentFrame().getComponentImage().getPaintBitmap();
		this.minIntensityR = minIntensityR;
		this.minIntensityG = minIntensityG;
		this.minIntensityB = minIntensityB;
		this.maxIntensityR = maxIntensityR;
		this.maxIntensityG = maxIntensityG;
		this.maxIntensityB = maxIntensityB;
		this.valuesOutsideR = valuesOutsideR;
		this.valuesOutsideG = valuesOutsideG;
		this.valuesOutsideB = valuesOutsideB;
		this.newImageDestination = newImageDestination;
		this.wholeImageThreshold = wholeImageThreshold;
		this.inverseThreshold = inverseThreshold;
		isColorImage = true;
	}
	
	
	

	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		if (srcImage.getNDims() == 2) {
			 xDim = srcImage.getExtents()[0];
		     yDim = srcImage.getExtents()[1];
		     length = xDim * yDim;
		     xRes = srcImage.getFileInfo(0).getResolutions()[0];
	         yRes = srcImage.getFileInfo(0).getResolutions()[1];
		 }else if(srcImage.getNDims() == 3) {
			 xDim = srcImage.getExtents()[0];
		     yDim = srcImage.getExtents()[1];
		     zDim = srcImage.getExtents()[2];
		     length = xDim * yDim * zDim;
		     xRes = srcImage.getFileInfo(0).getResolutions()[0];
	         yRes = srcImage.getFileInfo(0).getResolutions()[1];
	         zRes = srcImage.getFileInfo(0).getResolutions()[2];
		 }
		 float[] resultImgBuffer = null;
		 float[] srcImgBuffer = null;
		
		if(!wholeImageThreshold) {
			//this means we do thresholding only on VOIs
			mask = srcImage.generateVOIMask();
			if(!isColorImage) {
				srcImgBuffer = new float[length];
				 try {
					 srcImage.exportData(0, length, srcImgBuffer);
				 }catch(IOException e) {
					 
				 }
				 resultImgBuffer = new float[length];
				 for(int i=0;i<srcImgBuffer.length;i++) {
					 float val = srcImgBuffer[i];
					 if(mask.get(i) == true) {
						 mask.clear(i);
						 if(val >= minIntensity && val <= maxIntensity) {
							 if(!inverseThreshold) {
								 resultImgBuffer[i] = val;
								 numPixels = numPixels + 1;
								 sumIntensities = sumIntensities + val;
								 if(!newImageDestination) {
									 bitset.set(i);
								 }
							 }else {
								 resultImgBuffer[i] = valuesOutside;
							 }
							 
						 } else {
							 if(!inverseThreshold) {
								 resultImgBuffer[i] = valuesOutside;
							 }else {
								 resultImgBuffer[i] = val;
								 numPixels = numPixels + 1;
								 sumIntensities = sumIntensities + val;
								 if(!newImageDestination) {
									 bitset.set(i);
								 } 
							 }
						 }
					 }else {
						 //do nothing
					 }
				 }
			}else {
				//color image
				srcImgBuffer = new float[length * 4];
				 try {
					 srcImage.exportData(0, length * 4, srcImgBuffer);
				 }catch(IOException e) {
					 
				 }
				 resultImgBuffer = new float[length * 4];
				 for(int i=0;i<srcImgBuffer.length;i=i+4) {
					 float valR = srcImgBuffer[i+1];
					 float valG = srcImgBuffer[i+2];
					 float valB = srcImgBuffer[i+3];
					 if(mask.get(i/4) == true) {
						 mask.clear(i/4);
						 if(valR >= minIntensityR && valR <= maxIntensityR) {
							if(!inverseThreshold) {
								resultImgBuffer[i+1] = valR;
								numPixelsR = numPixelsR + 1;
								sumIntensitiesR = sumIntensitiesR + valR;
							}else {
								resultImgBuffer[i+1] = valuesOutsideR;
							}
						 }else {
							 if(!inverseThreshold) {
								 resultImgBuffer[i+1] = valuesOutsideR;
							 }else {
								 resultImgBuffer[i+1] = valR;
								 numPixelsR = numPixelsR + 1;
								 sumIntensitiesR = sumIntensitiesR + valR;
							 }
						 }
						 if(valG >= minIntensityG && valG <= maxIntensityG) {
							if(!inverseThreshold) {
								resultImgBuffer[i+2] = valG;
								numPixelsG = numPixelsG + 1;
								sumIntensitiesG = sumIntensitiesG + valG;
							}else {
								resultImgBuffer[i+2] = valuesOutsideG;
							} 
						 }else {
							 if(!inverseThreshold) {
								 resultImgBuffer[i+2] = valuesOutsideG;
							 }else {
								 resultImgBuffer[i+2] = valG;
								 numPixelsG = numPixelsG + 1;
								 sumIntensitiesG = sumIntensitiesG + valG;
							 } 
						 }
						 if(valB >= minIntensityB && valB <= maxIntensityB) {
							if(!inverseThreshold) {
								resultImgBuffer[i+3] = valB;
								numPixelsB = numPixelsB + 1;
								sumIntensitiesB = sumIntensitiesB + valB;
							}else {
								resultImgBuffer[i+3] = valuesOutsideB;
							} 
						 }else {
							 if(!inverseThreshold) {
								 resultImgBuffer[i+3] = valuesOutsideB;
							 }else {
								 resultImgBuffer[i+3] = valB;
								 numPixelsB = numPixelsB + 1;
								 sumIntensitiesB = sumIntensitiesB + valB;
							 } 
						 }
					 }
				 }
				 
			}
	

		}else {
			 if(!isColorImage) {
				 srcImgBuffer = new float[length];
				 try {
					 srcImage.exportData(0, length, srcImgBuffer);
				 }catch(IOException e) {
					 
				 }
				 resultImgBuffer = new float[length];
				 for(int i=0;i<srcImgBuffer.length;i++) {
					 float val = srcImgBuffer[i];
					 if(val >= minIntensity && val <= maxIntensity) {
						 if(!inverseThreshold) {
							 resultImgBuffer[i] = val;
							 numPixels = numPixels + 1;
							 sumIntensities = sumIntensities + val;
							 if(!newImageDestination) {
								 bitset.set(i);
							 }
						 }else {
							 resultImgBuffer[i] = valuesOutside;
						 }
						 
					 } else {
						 if(!inverseThreshold) {
							 resultImgBuffer[i] = valuesOutside;
						 }else {
							 resultImgBuffer[i] = val;
							 numPixels = numPixels + 1;
							 sumIntensities = sumIntensities + val;
							 if(!newImageDestination) {
								 bitset.set(i);
							 } 
						 }
					 }
				 }
			 }else {
				 srcImgBuffer = new float[length * 4];
				 try {
					 srcImage.exportData(0, length * 4, srcImgBuffer);
				 }catch(IOException e) {
					 
				 }
				 resultImgBuffer = new float[length * 4];
				 for(int i=0;i<srcImgBuffer.length;i=i+4) {
					 float valR = srcImgBuffer[i+1];
					 float valG = srcImgBuffer[i+2];
					 float valB = srcImgBuffer[i+3];
					 if(valR >= minIntensityR && valR <= maxIntensityR) {
						if(!inverseThreshold) {
							resultImgBuffer[i+1] = valR;
							numPixelsR = numPixelsR + 1;
							sumIntensitiesR = sumIntensitiesR + valR;
						}else {
							resultImgBuffer[i+1] = valuesOutsideR;
						}
					 }else {
						 if(!inverseThreshold) {
							 resultImgBuffer[i+1] = valuesOutsideR;
						 }else {
							 resultImgBuffer[i+1] = valR;
							 numPixelsR = numPixelsR + 1;
							 sumIntensitiesR = sumIntensitiesR + valR;
						 }
					 }
					 if(valG >= minIntensityG && valG <= maxIntensityG) {
						if(!inverseThreshold) {
							resultImgBuffer[i+2] = valG;
							numPixelsG = numPixelsG + 1;
							sumIntensitiesG = sumIntensitiesG + valG;
						}else {
							resultImgBuffer[i+2] = valuesOutsideG;
						} 
					 }else {
						 if(!inverseThreshold) {
							 resultImgBuffer[i+2] = valuesOutsideG;
						 }else {
							 resultImgBuffer[i+2] = valG;
							 numPixelsG = numPixelsG + 1;
							 sumIntensitiesG = sumIntensitiesG + valG;
						 } 
					 }
					 if(valB >= minIntensityB && valB <= maxIntensityB) {
						if(!inverseThreshold) {
							resultImgBuffer[i+3] = valB;
							numPixelsB = numPixelsB + 1;
							sumIntensitiesB = sumIntensitiesB + valB;
						}else {
							resultImgBuffer[i+3] = valuesOutsideB;
						} 
					 }else {
						 if(!inverseThreshold) {
							 resultImgBuffer[i+3] = valuesOutsideB;
						 }else {
							 resultImgBuffer[i+3] = valB;
							 numPixelsB = numPixelsB + 1;
							 sumIntensitiesB = sumIntensitiesB + valB;
						 } 
					 }
					 
				 }
			 }
		}//end else !wholeImageThreshold
		
		if(newImageDestination) {
			 resultImage = (ModelImage) srcImage.clone();
	         resultImage.setImageName(srcImage.getImageFileName() + "_stdDevThreshold");
	         resultImage.resetVOIs();
			 try {
				 resultImage.importData(0, resultImgBuffer, true);
			 } catch(IOException e) {
				 
			 }
		 }
		 if (srcImage.getNDims() == 2) {
			if(!isColorImage) {
				totalArea =  numPixels * xRes * yRes;
			}else {
				totalAreaR =  numPixelsR* xRes * yRes;
				totalAreaG =  numPixelsG * xRes * yRes;
				totalAreaB =  numPixelsB * xRes * yRes;
			}
		 }else if(srcImage.getNDims() == 3) {
			 if(!isColorImage) {
				 totalVolume = numPixels * xRes * yRes * zRes;
			 }else {
				 totalVolumeR = numPixelsR * xRes * yRes * zRes;
				 totalVolumeG = numPixelsG * xRes * yRes * zRes;
				 totalVolumeB = numPixelsB * xRes * yRes * zRes;
			 }
		 }
		
		 setCompleted(true);
	}

	
	/**
	 * getResultImage
	 * @return
	 */
	public ModelImage getResultImage() {
		return resultImage;
	}

	
	/**
	 * getTotalArea
	 * @return
	 */
	public float getTotalArea() {
		return totalArea;
	}

	/**
	 * getTotalVolume
	 * @return
	 */
	public float getTotalVolume() {
		return totalVolume;
	}

	/**
	 * getNumPixels
	 * @return
	 */
	public int getNumPixels() {
		return numPixels;
	}

	/**
	 * getNumPixelsB
	 * @return
	 */
	public int getNumPixelsB() {
		return numPixelsB;
	}

	/**
	 * getNumPixelsG
	 * @return
	 */
	public int getNumPixelsG() {
		return numPixelsG;
	}

	/**
	 * getNumPixelsR
	 * @return
	 */
	public int getNumPixelsR() {
		return numPixelsR;
	}

	/**
	 * getTotalAreaB
	 * @return
	 */
	public float getTotalAreaB() {
		return totalAreaB;
	}

	/**
	 * getTotalAreaG
	 * @return
	 */
	public float getTotalAreaG() {
		return totalAreaG;
	}

	/**
	 * getTotalAreaR
	 * @return
	 */
	public float getTotalAreaR() {
		return totalAreaR;
	}

	/**
	 * getTotalVolumeB
	 * @return
	 */
	public float getTotalVolumeB() {
		return totalVolumeB;
	}

	/**
	 * getTotalVolumeG
	 * @return
	 */
	public float getTotalVolumeG() {
		return totalVolumeG;
	}

	/**
	 * getTotalVolumeR
	 * @return
	 */
	public float getTotalVolumeR() {
		return totalVolumeR;
	}

	/**
	 * get sum intensities
	 * @return
	 */
	public float getSumIntensities() {
		return sumIntensities;
	}

	/**
	 * get sum intensities B
	 * @return
	 */
	public float getSumIntensitiesB() {
		return sumIntensitiesB;
	}

	/**
	 * get sum intensities G
	 * @return
	 */
	public float getSumIntensitiesG() {
		return sumIntensitiesG;
	}

	/**
	 * get sum intensities  R
	 * @return
	 */
	public float getSumIntensitiesR() {
		return sumIntensitiesR;
	}
	
	
	
	
	
	

}
