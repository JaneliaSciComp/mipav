package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;

public class AlgorithmStandardDeviationThreshold extends AlgorithmBase {


	/** DOCUMENT ME **/
	private float maxIntensity;
	
	/** DOCUMENT ME **/
	private float minIntensity;
	
	/** DOCUMENT ME **/
	private ModelImage srcImage, resultImage;
	
	/** DOCUMENT ME **/
	private float valuesOutside;
	
	/** DOCUMENT ME **/
	private int xDim, yDim, zDim, length;
	
	/** DOCUMENT ME **/
	private int numPixels = 0;
	
	/** DOCUMENT ME **/
	private float totalVolume,totalArea;
	
	/** DOCUMENT ME **/
	private float xRes,yRes,zRes;
	
	
	
	/**
	 * 
	 * @param srcImage
	 * @param minIntensity
	 * @param maxIntensity
	 */
	public AlgorithmStandardDeviationThreshold(ModelImage srcImage, float minIntensity, float maxIntensity, float valuesOutside) {
		this.srcImage = srcImage;
		this.minIntensity = minIntensity;
		this.maxIntensity = maxIntensity;
		this.valuesOutside = valuesOutside;
	}

	/**
	 * 
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
		 float[] srcImgBuffer = new float[length];
		 try {
			 srcImage.exportData(0, length, srcImgBuffer);
		 }catch(IOException e) {
			 
		 }
		 float[] resultImgBuffer = new float[length];
		 for(int i=0;i<srcImgBuffer.length;i++) {
			 float val = srcImgBuffer[i];
			 if(val >= minIntensity && val <= maxIntensity) {
				 resultImgBuffer[i] = val;
				 numPixels = numPixels + 1;
			 } else {
				 resultImgBuffer[i] = valuesOutside;
			 }
		 }
		 resultImage = (ModelImage) srcImage.clone();
         resultImage.setImageName(srcImage.getImageFileName() + "_stdDevThreshold");
         resultImage.resetVOIs();
		 try {
			 resultImage.importData(0, resultImgBuffer, true);
		 } catch(IOException e) {
			 
		 }
		 if (srcImage.getNDims() == 2) {
			totalArea =  numPixels * xRes * yRes;
		 }else if(srcImage.getNDims() == 3) {
			totalVolume = numPixels * xRes * yRes * zRes;
		 }
		 
		 
		
		 setCompleted(true);
	}

	
	/**
	 * 
	 * @return
	 */
	public ModelImage getResultImage() {
		return resultImage;
	}

	
	/**
	 * 
	 * @return
	 */
	public float getTotalArea() {
		return totalArea;
	}

	public float getTotalVolume() {
		return totalVolume;
	}
	
	
	
	

}
