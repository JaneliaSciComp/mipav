package gov.nih.mipav.model;

import gov.nih.mipav.model.algorithms.GenerateGaussian;

import java.util.logging.Level;
import java.util.logging.Logger;

public class GaussianKernelFactory implements KernelFactory {
	public static final int BLUR_KERNEL = 0;
	public static final int X_DERIVATIVE_KERNEL = 1;
	public static final int Y_DERIVATIVE_KERNEL = 2;
	public static final int Z_DERIVATIVE_KERNEL = 3;
	
	private Logger logger;
	private float[] sigmas;
	private int[] extents;
	private int kernelType;
	private int derivativeOrder = 1;
	private int extentScale = 8;
	
	public int getKernelType() {
		return kernelType;
	}

	public void setKernelType(int kernelType) {
		this.kernelType = kernelType;
	}

	public void setDerivativeOrder(int derivativeOrder) {
		this.derivativeOrder = derivativeOrder;
	}
	
	public void setExtentScale(int extentScale) {
		this.extentScale = extentScale;
	}

	private GaussianKernelFactory(float[] sigmas){
		logger = Logger.getLogger(getClass().getName());
		this.kernelType = BLUR_KERNEL;
		this.sigmas = sigmas;
		extents = new int[sigmas.length];
		for(int i = 0; i < sigmas.length; i++){
			extents[i] = determineExtent(sigmas[i]);
		}
	}
	
	public static GaussianKernelFactory getInstance(float[] sigmas){
		return new GaussianKernelFactory(sigmas);
	}
	 
	public Kernel createKernel(){
		if(kernelType == BLUR_KERNEL){
			return createBlurKernel();
		}else if(kernelType == X_DERIVATIVE_KERNEL){
			return createXDerivativeKernel();
		}else if(kernelType == Y_DERIVATIVE_KERNEL){
			return createYDerivativeKernel();
		}else if(kernelType == Z_DERIVATIVE_KERNEL){
			return createZDerivativeKernel();
		}else{
			return null;
		}
	}
	
	public Kernel createBlurKernel() {
		if(sigmas == null){
			logger.log(Level.SEVERE, "The sigmas used to create kernel are null.");
		}
		GaussianKernel gaussianKernel = new GaussianKernel();
		int xkDim = determineExtent(sigmas[0]);
		int[] kExtents = new int[1];
		kExtents[0] = xkDim;
		float[] lsigmas = new float[1];
		lsigmas[0] = sigmas[0];
		int[] derivativeOrder = new int[1];
		derivativeOrder[0] = 0;
		float[] gxData = new float[xkDim];
        GenerateGaussian g = new GenerateGaussian(gxData, kExtents, lsigmas, derivativeOrder);
        g.calc(false);
        
		lsigmas[0] = sigmas[1];
        int ykDim = determineExtent(sigmas[1]);
		kExtents[0] = ykDim;
		derivativeOrder[0] = 0;
		float[] gyData = new float[ykDim];
        g = new GenerateGaussian(gyData, kExtents, lsigmas, derivativeOrder);
        g.calc(false);
        
        if (sigmas.length > 2) {
			lsigmas[0] = sigmas[2];
			int zkDim = determineExtent(sigmas[2]);
			kExtents[0] = zkDim;
			derivativeOrder[0] = 0;
			float[] gzData = new float[zkDim];
			g = new GenerateGaussian(gzData, kExtents, lsigmas, derivativeOrder);
			g.calc(false);

			gaussianKernel.setData(new float[][]{gxData, gyData, gzData});
		}else{
			gaussianKernel.setData(new float[][]{gxData, gyData});
		}
        
		return gaussianKernel;
	}
	
	public Kernel createXDerivativeKernel() {
		if(sigmas == null){
			logger.log(Level.SEVERE, "The sigmas used to create kernel are null.");
		}
		GaussianKernel gaussianKernel = new GaussianKernel();
		int[] kExtents = new int[1];
		float[] lsigmas = new float[1];
		int[] derivativeOrder = new int[1];
		int xkDim = determineExtent(sigmas[0]);
		kExtents[0] = xkDim;
		lsigmas[0] = sigmas[0];
        float[] gxDerivativeData = new float[xkDim];
        derivativeOrder[0] = this.derivativeOrder;
        GenerateGaussian g = new GenerateGaussian(gxDerivativeData, kExtents, lsigmas, derivativeOrder);
        g.calc(false);
        
		lsigmas[0] = sigmas[1];
        int ykDim = determineExtent(sigmas[1]);
		kExtents[0] = ykDim;
		derivativeOrder[0] = 0;
		float[] gyData = new float[ykDim];
        g = new GenerateGaussian(gyData, kExtents, lsigmas, derivativeOrder);
        g.calc(false);
        
        if (sigmas.length > 2) {
			lsigmas[0] = sigmas[2];
			int zkDim = determineExtent(sigmas[2]);
			kExtents[0] = zkDim;
			derivativeOrder[0] = 0;
			float[] gzData = new float[zkDim];
			g = new GenerateGaussian(gzData, kExtents, lsigmas, derivativeOrder);
			g.calc(false);

			gaussianKernel.setData(new float[][]{gxDerivativeData, gyData, gzData});
		}else{
			gaussianKernel.setData(new float[][]{gxDerivativeData, gyData});
		}
        
		return gaussianKernel;
	}
	
	public Kernel createYDerivativeKernel() {
		if(sigmas == null){
			logger.log(Level.SEVERE, "The sigmas used to create kernel are null.");
		}
		GaussianKernel gaussianKernel = new GaussianKernel();
		int xkDim = determineExtent(sigmas[0]);
		int[] kExtents = new int[1];
		kExtents[0] = xkDim;
		float[] lsigmas = new float[1];
		lsigmas[0] = sigmas[0];
		int[] derivativeOrder = new int[1];
		derivativeOrder[0] = 0;
		float[] gxData = new float[xkDim];
        GenerateGaussian g = new GenerateGaussian(gxData, kExtents, lsigmas, derivativeOrder);
        g.calc(false);
        
		lsigmas[0] = sigmas[1];
        int ykDim = determineExtent(sigmas[1]);
		kExtents[0] = ykDim;
        float[] gyDerivativeData = new float[ykDim];
        derivativeOrder[0] = this.derivativeOrder;
        g = new GenerateGaussian(gyDerivativeData, kExtents, lsigmas, derivativeOrder);
        g.calc(false);
        
        if (sigmas.length > 2) {
			lsigmas[0] = sigmas[2];
			int zkDim = determineExtent(sigmas[2]);
			kExtents[0] = zkDim;
			derivativeOrder[0] = 0;
			float[] gzData = new float[zkDim];
			g = new GenerateGaussian(gzData, kExtents, lsigmas, derivativeOrder);
			g.calc(false);

			gaussianKernel.setData(new float[][]{gxData, gyDerivativeData, gzData});
		}else{
			gaussianKernel.setData(new float[][]{gxData, gyDerivativeData});
		}
        
		return gaussianKernel;
	}
	
	public Kernel createZDerivativeKernel() {
		if(sigmas == null){
			logger.log(Level.SEVERE, "The sigmas used to create kernel are null.");
		}
		GaussianKernel gaussianKernel = new GaussianKernel();
		int xkDim = determineExtent(sigmas[0]);
		int[] kExtents = new int[1];
		kExtents[0] = xkDim;
		float[] lsigmas = new float[1];
		lsigmas[0] = sigmas[0];
		int[] derivativeOrder = new int[1];
		derivativeOrder[0] = 0;
		float[] gxData = new float[xkDim];
        GenerateGaussian g = new GenerateGaussian(gxData, kExtents, lsigmas, derivativeOrder);
        g.calc(false);
        
		lsigmas[0] = sigmas[1];
        int ykDim = determineExtent(sigmas[1]);
		kExtents[0] = ykDim;
		derivativeOrder[0] = 0;
		float[] gyData = new float[ykDim];
        g = new GenerateGaussian(gyData, kExtents, lsigmas, derivativeOrder);
        g.calc(false);
        
		lsigmas[0] = sigmas[2];
		int zkDim = determineExtent(sigmas[2]);
		kExtents[0] = zkDim;
		float[] gzDerivativeData = new float[zkDim];
		derivativeOrder[0] = this.derivativeOrder;
		g = new GenerateGaussian(gzDerivativeData, kExtents, lsigmas,
				derivativeOrder);
		g.calc(false);
		gaussianKernel.setData(new float[][] { gxData, gyData, gzDerivativeData });

		return gaussianKernel;
	}
	
	private int determineExtent(float sigma){
		int extent = Math.round(extentScale * sigma);
		if(extent % 2 == 0){
			extent++;
		}
		
		if(extent < 3){
			extent = 3;
		}
		return extent;
	}
}
