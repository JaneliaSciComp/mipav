package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.GaussianKernelFactory;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmSeparableConvolver;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.util.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;

import java.io.IOException;
import java.util.Arrays;

/**
 * Calculates the gradient magnitude of an image at a scale defined by the user (using separable convolutions). Adapted
 * from AlgorithmGradientMagnitude. This version should be faster but uses significantly more memory.
 *
 * <p>Produces equivalent result images to AlgorithmGradientMagnitude (aside from some rounding error that seems to be
 * at most 10^-4)</p>
 *
 * @version  0.1 July 31, 2003
 * @author   Evan McCreedy
 * @see      AlgorithmGradientMagnitude
 */
public class AlgorithmGradientMagnitudeSep extends AlgorithmBase {

	//~ Instance fields ------------------------------------------------------------------------------------------------

	/** DOCUMENT ME! */
	private boolean blue = true;

	/**
	 * Flag, if true, indicates that the whole image should be processed. If false only process the image over the mask
	 * areas.
	 */
	private boolean entireImage;

	/** DOCUMENT ME! */
	private boolean green = true;

	private Kernel gaussianKernel;


	/** Flags indicate which color channel to process. True indicates the channel should be processed. */
	private boolean red = true;

	/** Standard deviations of the gaussian used to calculate the kernels. */
	private float[] sigmas;

	private float[] outputBuffer;
	
	private boolean normalized;
	private boolean directionNeeded;
	private float[] xDerivativeDirections;
	private float[] yDerivativeDirections;

	//~ Constructors ---------------------------------------------------------------------------------------------------

	/**
	 * Creates a new AlgorithmGradientMagnitudeSep object.
	 *
	 * @param  srcImg    source image model
	 * @param  sigmas    Gaussian standard deviations in each dimension
	 * @param  maskFlag  Flag, if true, indicates that the gradient magnitude will be calculated for the whole image
	 * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
	 *                   images disregard this flag.
	 */
	public AlgorithmGradientMagnitudeSep(ModelImage srcImg, float[] sigmas,
			boolean maskFlag, boolean img25D) {
		super(null, srcImg);

		image25D = img25D;
		if(image25D){
			this.sigmas = new float[]{sigmas[0], sigmas[1]};
		}else{
			this.sigmas = sigmas;
		}
		entireImage = maskFlag;

		if (entireImage == false) {
			mask = srcImage.generateVOIMask();
		}
	}

	//~ Methods --------------------------------------------------------------------------------------------------------

	public void beforeExecute() {
		fireProgressStateChanged(minProgressValue, null, "Calculating gradient magnitude ... ");
	}

	public void execute() {
		if(threadStopped){
			return;
		}
		boolean color = false;
		int cFactor = 1;
		if (srcImage.isColorImage()) {
			color = true;
			cFactor = 4;
		}

		int imgLength = cFactor
				* MipavUtil.calculateImageSize(srcImage.getExtents());

		float[] buffer = new float[imgLength];
		try {
			srcImage.exportData(0, imgLength, buffer);
		} catch (IOException e) {
			errorCleanUp(
					"Algorithm Gradient Magnitude exportData: Image(s) locked.",
					false);
			fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
			return;
		}

		if(threadStopped){
			return;
		}
		
		// Calculate the x derivatives of the image.
		int progressFrom = minProgressValue;
		int progressTo = 0;
		
		if(srcImage.is2DImage()){
			progressTo = (int)((maxProgressValue - minProgressValue) * 0.3)-1;
		}else{
			progressTo = (int)((maxProgressValue - minProgressValue) * 0.25)-1;
		}
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.X_DERIVATIVE_KERNEL);
		gaussianKernel = gkf.createKernel();

		float[] xDerivativeBuffer = calculateDerivativeImage(buffer, color,
				gaussianKernel.getData(), progressFrom, progressTo, false);

		if(threadStopped){
			return;
		}

		// Calculate the y derivatives of the image.
		progressFrom = progressTo+1;
		if(srcImage.is2DImage() || !is3DKernel()){
			progressTo = progressFrom + (int)((maxProgressValue - minProgressValue) * 0.3)-1;
		}else{
			progressTo = progressFrom + (int)((maxProgressValue - minProgressValue) * 0.25)-1;
		}
		gkf.setKernelType(GaussianKernelFactory.Y_DERIVATIVE_KERNEL);
		gaussianKernel = gkf.createKernel();
		float[] yDerivativeBuffer = calculateDerivativeImage(buffer, color,
				gaussianKernel.getData(), progressFrom, progressTo, false);

		if(threadStopped){
			return;
		}

		// Calculate the z derivative of the image.
		if (srcImage.is3DImage() && is3DKernel()) {
			progressFrom = progressTo+1;
			progressTo = progressFrom + (int)((maxProgressValue - minProgressValue) * 0.25)-1;

			gkf.setKernelType(GaussianKernelFactory.Z_DERIVATIVE_KERNEL);
			gaussianKernel = gkf.createKernel();

			float[] zDerivativeBuffer = calculateDerivativeImage(buffer, color,
					gaussianKernel.getData(), progressFrom, progressTo, true);

			if (threadStopped) {
				return;
			}
			
			// Calculate the gradient magnitude of the image.
			progressFrom = progressTo+1;
			progressTo = progressFrom + (int)((maxProgressValue - minProgressValue) * 0.25)-1;
			progress = progressFrom;
			outputBuffer = magnitude(buffer, cFactor, xDerivativeBuffer,
					yDerivativeBuffer, zDerivativeBuffer, progressFrom, progressTo);
			xDerivativeBuffer = null;
			yDerivativeBuffer = null;
			zDerivativeBuffer = null;
			return;
		}
		progressFrom = progressTo+1;
		progressTo = progressFrom + (int)((maxProgressValue - minProgressValue) * 0.3);
		progress = progressFrom;
		// Calculate the gradient magnitude of the image.
		outputBuffer = magnitude(buffer, cFactor, xDerivativeBuffer,
				yDerivativeBuffer, null, progressFrom, progressTo);
		xDerivativeBuffer = null;
		yDerivativeBuffer = null;
	}

	public void afterExecute() {
		fireProgressStateChanged(maxProgressValue);
		setCompleted(true);
	}

	private float[] calculateDerivativeImage(float[] imgData, boolean color,
			float[][] kernelData, int progressFrom, int progressTo, boolean sourcBufferChangeable) {
		AlgorithmSeparableConvolver convolver = new AlgorithmSeparableConvolver(
				imgData, srcImage.getExtents(), kernelData, color, Preferences.isMultiThreadingEnabled(),
				sourcBufferChangeable);
		convolver.setNumberOfThreads(Preferences.getNumberOfThreads());
		convolver.setProgressValues(generateProgressValues(progressFrom,
				progressTo));
		linkProgressToAlgorithm(convolver);
		convolver.setRunningInSeparateThread(runningInSeparateThread);
		if (!entireImage) {
			convolver.setMask(mask);
		}

		if (color) {
			convolver.setColorChannels(red, green, blue);
		}

		convolver.run();
		delinkProgressToAlgorithm(convolver);
		if(threadStopped){
			return null;
		}
		return convolver.getOutputBuffer();
	}

	private float[] magnitude(float[] buffer, int cFactor,
			float[] xDerivatives, float[] yDerivatives, float[] zDerivatives,
			int progressFrom, int progressTo) {
		float[] resultBuffer = null;
		if(directionNeeded){
			xDerivativeDirections = xDerivatives;
			yDerivativeDirections = yDerivatives;
			
			if(zDerivatives != null){
				resultBuffer = zDerivatives;
			}else{
				resultBuffer = new float[xDerivatives.length];
			}
		}else{
			resultBuffer = xDerivatives;
		}
		int start = calculateValidStartZIndex() * srcImage.getSliceSize() * cFactor;
		int end = calculateValidEndZIndex() * srcImage.getSliceSize() * cFactor;
		if(start > 0){
			Arrays.fill(resultBuffer, 0, start, 0);
		}
		if(end < xDerivatives.length){
			Arrays.fill(resultBuffer, end, xDerivatives.length, 0);
		}
		progressStep = (end-start) / (progressTo - progressFrom);
		for (int i = start; i < end && !threadStopped; i += cFactor) {
			if(i % progressStep == 0){
				makeProgress(1);
				fireProgressStateChanged((int)progress);
			}
			if (entireImage || mask.get(i / cFactor)) {
				if (cFactor == 4) {
					resultBuffer[i] = buffer[i];
					if (red) {
						if(zDerivatives == null){
							resultBuffer[i + 1] = (float) Math
							.sqrt((xDerivatives[i + 1] * xDerivatives[i + 1])
									+ (yDerivatives[i + 1] * yDerivatives[i + 1]));
						}else{
						resultBuffer[i + 1] = (float) Math
								.sqrt((xDerivatives[i + 1] * xDerivatives[i + 1])
										+ (yDerivatives[i + 1] * yDerivatives[i + 1])
										+ (zDerivatives[i + 1] * zDerivatives[i + 1]));
						}
						if(directionNeeded && normalized){
							xDerivatives[i+1] /= resultBuffer[i+1];
							yDerivatives[i+1] /= resultBuffer[i+1];
						}
					} else {
						resultBuffer[i + 1] = buffer[i + 1];
					}
					if (green) {
						if(zDerivatives == null){
							resultBuffer[i + 2] = (float) Math
							.sqrt((xDerivatives[i + 2] * xDerivatives[i + 2])
									+ (yDerivatives[i + 2] * yDerivatives[i + 2]));
							
						}else{
						resultBuffer[i + 2] = (float) Math
								.sqrt((xDerivatives[i + 2] * xDerivatives[i + 2])
										+ (yDerivatives[i + 2] * yDerivatives[i + 2])
										+ (zDerivatives[i + 2] * zDerivatives[i + 2]));
						}
						if(directionNeeded && normalized){
							xDerivatives[i+2] /= resultBuffer[i+2];
							yDerivatives[i+2] /= resultBuffer[i+2];
						}
					} else {
						resultBuffer[i + 2] = buffer[i + 2];
					}
					if (blue) {
						if(zDerivatives == null){
							resultBuffer[i + 3] = (float) Math.sqrt((xDerivatives[i + 3] * xDerivatives[i + 3])
									+ (yDerivatives[i + 3] * yDerivatives[i + 3]));
							
						}else{
							resultBuffer[i + 3] = (float) Math.sqrt((xDerivatives[i + 3] * xDerivatives[i + 3])
									+ (yDerivatives[i + 3] * yDerivatives[i + 3])
									+ (zDerivatives[i + 3] * zDerivatives[i + 3]));
						}
						if(directionNeeded && normalized){
							xDerivatives[i+3] /= resultBuffer[i+3];
							yDerivatives[i+3] /= resultBuffer[i+3];
						}
					} else {
						resultBuffer[i + 3] = buffer[i + 3];
					}
				} else {
					if(zDerivatives == null){
						resultBuffer[i] = (float) Math.sqrt((xDerivatives[i] * xDerivatives[i])
								+ (yDerivatives[i] * yDerivatives[i]));
					}else{
						resultBuffer[i] = (float) Math.sqrt((xDerivatives[i] * xDerivatives[i])
									+ (yDerivatives[i] * yDerivatives[i])
									+ (zDerivatives[i] * zDerivatives[i]));
					}
					if(directionNeeded && normalized){
						xDerivatives[i] /= resultBuffer[i];
						yDerivatives[i] /= resultBuffer[i];
					}
				}
			}
		}
		return resultBuffer;
	}

	/**
	 * Prepares this class for destruction.
	 */
	public void finalize() {
		sigmas = null;
		destImage = null;
		srcImage = null;
		super.finalize();
	}

	/**
	 * Starts the program.
	 */
	public void runAlgorithm() {
		beforeExecute();
		execute();
		afterExecute();
	}

	/**
	 * Sets the flag for the blue channel.
	 *
	 * @param  flag  if set to true then the blue channel is processed.
	 */
	public void setBlue(boolean flag) {
		blue = flag;
	}

	/**
	 * Sets the flag for the green channel.
	 *
	 * @param  flag  if set to true then the green channel is processed.
	 */
	public void setGreen(boolean flag) {
		green = flag;
	}

	/**
	 * Sets the flag for the red channel.
	 *
	 * @param  flag  if set to true then the red channel is processed.
	 */
	public void setRed(boolean flag) {
		red = flag;
	}

	public float[] getResultBuffer() {
		return outputBuffer;
	}

	public float[] getXDerivativeDirections() {
		return xDerivativeDirections;
	}

	public float[] getYDerivativeDirections() {
		return yDerivativeDirections;
	}

	public boolean isNormalized() {
		return normalized;
	}

	public void setNormalized(boolean normalized) {
		this.normalized = normalized;
	}

	public boolean isDirectionNeeded() {
		return directionNeeded;
	}

	public void setDirectionNeeded(boolean directionNeeded) {
		this.directionNeeded = directionNeeded;
	}
	
	private boolean is3DKernel(){
		if(sigmas.length > 2){
			return true;
		}
		return false;
	}
	public int calculateValidStartZIndex(){
		if(srcImage.is3DImage() && is3DKernel()){
			return (gaussianKernel.getExtents()[2]+1)/2;
		}
		return 0;
	}
	
	public int calculateValidEndZIndex(){
		if(srcImage.is3DImage() && is3DKernel()){
			return srcImage.getExtents()[2]-(gaussianKernel.getExtents()[2]+1)/2;
		}
		if(srcImage.is3DImage()){
			return srcImage.getExtents()[2];
		}
		
		return 1;
	}
}
