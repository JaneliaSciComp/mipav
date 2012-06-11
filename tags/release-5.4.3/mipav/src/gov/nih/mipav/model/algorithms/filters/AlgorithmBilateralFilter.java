package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;
import gov.nih.mipav.view.ViewJProgressBar;


/**
 * Bilateral filtering smooths an image or VOI region of the image while preserving edges with 2 Gaussian functions.
 * One is a Gaussian at a user defined spatial scale (sigma - standard deviation) and the second is a Gaussian at a
 * user defined intensity scale.  The spatial Gaussian weighs pixels with smaller spatial separations more heavily and 
 * the intensity Gaussian weighs pixels with similar intensities more heavily.  
 * 
 * RGB values are converted to CIELab values and the color distance is found using the CIE76 distance metric in 
 * CIELab space.  The convolution is performed in CIELab space.  Then CIELab is converted back to RGB.  In contrast
 * to standard Gaussian spatial smoothing, bilateral filtering produces no phantom colors along edges in color images,
 * and reduces phantom colors where they appear in the original image.
 
 * <p>1D Gaussian = (1/sqrt(2*PI*sigma*sigma))*exp(-x*x/(2*sigma*sigma));</p>
 * 
 * The three coordinates of CIELAB represent the lightness of the color(L* = 0 yields black and L* = 100 indicates diffuse 
 * white; specular white may be higher), its position between red/magenta and green(a*, negative values indicate green
 * while positive values indicate magenta) and its position between yellow and blue(b*, negative values indicate blue 
 * and positive values indicate yellow).  The asterisk(*) after L, a, and b are part of the full name, since they represent 
 * L*, a*, and b*, to distinguish them from Hunter's L, a, and b.
 * 
 * The L* coordinate ranges from 0 to 100.  The possible range of a* and b* coordinates depends on the color space that one
 * is converting from.  
 * R = 0, G = 0, B = 0 => L* = 0, a* = 0, b* = 0
 * R = 255, G = 0, B = 0 => L* = 53.2, a* = 80.1, b* = 67.22
 * R = 0, G = 255, B = 0 => L* = 87.7, a* = -86.2, b* = 83.2
 * R = 0, G = 0, B = 255 => L* = 32.3, a* = 79.2, b* = -107.9
 * R = 255, G = 255, B = 0 => L* = 97.1, a* = -21.6, b* = 94.5
 * R = 255, G = 0, B = 255 => L* = 60.3, a* = 98.3, b* = -60.8
 * R = 0, G = 255, B = 255 => L* = 91.1, a* = -48.1, b* = -14.1
 * R = 255, G = 255, B = 255 => L* = 100.0, a* = 0.00525, b* = -0.0104
 * so the range of a* equals about the range of b* and the range of a* equals about twice the range of L*.
 * The simplest distance metric delta E is CIE76 = sqrt((L2* - L1*)**2 + (a2* - a1*)**2 + (b2* - b1*)**2)
 * 
 * XW, YW, and ZW (also called XN, YN, ZN or X0, Y0, Z0) are reference white tristimulus values - typically the white
 * of a perfectly reflecting diffuser under CIE standard D65 illumination(defined by x = 0.3127 and y = 0.3291 in the
 * CIE chromatcity diagram).  The 2 degrees, D65 reference tristimulus values are: XN = 95.047, YN = 100.000, and ZN = 108.883.
 * 
 * References:
 * 1.) C. Tomasi and R. Manduchi, "Bilateral Filtering for Gray and Color Images", Proceedings of the 1998 IEEE International
 * Conference on Computer Vision, Bombay, India.
 * 2.)Sylvain Paris, Pierre Kornprobst, Jack Tumblin, and Fredo Durand, "A Gentle Introduction to Bilateral Filtering and its
 * Applications", SIGGRAPH 2007.
 * 3.) http://www.easyrgb.com has XYZ -> RGB, RGB -> XYZ, XYZ -> CIEL*ab, CIEL*ab -> XYZ, and
 *     XYZ(Tristimulus) Reference values of a perfect reflecting diffuser.
 *
 * @version  0.1 February 5, 2009
 * @author   William Gandler
 * @see      GenerateGaussian
 * @see      AlgorithmConvolver
 */
public class AlgorithmBilateralFilter extends AlgorithmBase implements AlgorithmInterface{

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Storage location of the Gaussian kernel. */
    private float[] GaussData;

    /** Dimensionality of the kernel. */
    private int[] kExtents;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;
    
    /** units of intensity range; it is multiplied by the intensity range to create intensitySigma */
    private float intensityFraction;
    
    // intensityGaussianDenom = 2.0 * intensitySigma * intensitySigma
    private double intensityGaussianDenom;
    
    /* Assigned to srcImage if replace image, assigned to destImage if new image */
    private ModelImage targetImage = null;
    
    private ModelImage cieLabImage = null;
    
    private double imageMax;
    
    // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
    private double scaleMax = 255.0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmGaussianBlur object.
     *
     * @param  srcImg    DOCUMENT ME!
     * @param  sigmas    DOCUMENT ME!
     * @param  intensityFraction
     * @param  maskFlag  DOCUMENT ME!
     * @param  img25D    DOCUMENT ME!
     */
    public AlgorithmBilateralFilter(ModelImage srcImg, float[] sigmas, float intensityFraction, boolean maskFlag, boolean img25D) {
        this(null, srcImg, sigmas, intensityFraction, maskFlag, img25D);
    }

    /**
     * Constructor which sets the source and destination images, the minimum and maximum progress value.
     *
     * @param  destImg   the destination image
     * @param  srcImg    the source image
     * @param  sigmas    the sigmas
     * @param  intensityFraction units of intensity range; it is multiplied by the intensity range to create intensitySigma
     * @param  maskFlag  the mask flag
     * @param  img25D    the 2.5D indicator
     */
    public AlgorithmBilateralFilter(ModelImage destImg, ModelImage srcImg, float[] sigmas, float intensityFraction, boolean maskFlag,
                                 boolean img25D) {
        super(destImg, srcImg);

        this.sigmas = sigmas;
        this.intensityFraction = intensityFraction;
        entireImage = maskFlag;
        image25D = img25D;

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        GaussData = null;
        destImage = null;
        srcImage = null;
        kExtents = null;
        sigmas = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        AlgorithmConvolver convolver;
        double intensitySigma;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
//		 Test to calc speed of srcImage.calcMinMax()
//		long startTimeMM = System.nanoTime();
//		for (int m = 0; m < 1200; m++){ srcImage.calcMinMax(); }
//		System.out.println("Time consumed: MIN MAX " + (System.nanoTime()-startTimeMM));

        if (srcImage.getNDims() == 2) {
            makeKernels2D();
        } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
            makeKernels3D();
        } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
            makeKernels2D();
        } else if (srcImage.getNDims() == 4) {
            makeKernels3D();
        }

        if (threadStopped) {
            setCompleted(false);
            destImage.releaseLock();

            return;
        }
        
        long startTime = System.nanoTime();
        fireProgressStateChanged(0, srcImage.getImageName(), "Bilateral filter on image ...");
        
        if (destImage == null) {
            targetImage = srcImage;
        }
        else {
            targetImage = destImage;
        }
        
        srcImage.calcMinMax();
        imageMax = (float)srcImage.getMax();
        if (srcImage.isColorImage()) {
            scaleMax = Math.max(255.0, imageMax);
            cieLabImage = new ModelImage(ModelImage.ARGB_FLOAT, srcImage.getExtents(),srcImage.getImageName() + "cieLab");
            convertRGBtoCIELab();
        } // if (targetImage.isColorImage())
        else {
            intensitySigma = (intensityFraction * (srcImage.getMax() - srcImage.getMin()));
            intensityGaussianDenom = (2.0  * intensitySigma * intensitySigma);
        }

        if (srcImage.isColorImage()) {
            convolver = new AlgorithmConvolver(cieLabImage, GaussData, kExtents,entireImage, image25D, 
                    intensityGaussianDenom);    
        }
        else {
            convolver = new AlgorithmConvolver(srcImage, GaussData, kExtents,entireImage, image25D, 
                                                              intensityGaussianDenom);
        }
        convolver.setMinProgressValue(0);
        convolver.setMaxProgressValue(100);
		linkProgressToAlgorithm(convolver);
		convolver.addListener(this);
		if (!entireImage) {
			convolver.setMask(mask);
		}

        convolver.run();
        if (cieLabImage != null) {
            cieLabImage.disposeLocal();
            cieLabImage = null;
        }

        setCompleted(true);
        //System.out.println("Time consumed GB: " + (System.nanoTime()-startTime));
    }
    
    /**
     * Creates 2D Gaussian kernels for the blurring process. The kernel size is always odd and proportional (8X) to the
     * standard deviation of the Gaussian.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 0;
        derivOrder[1] = 0;

        xkDim = Math.round(8 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        GaussData = new float[xkDim * ykDim];

        GenerateGaussian Gauss = new GenerateGaussian(GaussData, kExtents, sigmas, derivOrder);
        Gauss.calc(false);
        Gauss.finalize();
        Gauss = null;
    }

    /**
     * Creates 3D Gaussian kernels for the blurring process. The kernel size is always odd and proportional (8X) to the
     * standard deviation of the Gaussian.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(8 * sigmas[0]);
        // System.out.println("Sigma 0 = " + sigmas[0]);
        // System.out.println("Sigma 1 = " + sigmas[1]);
        // System.out.println("Sigma 2 = " + sigmas[2]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(8 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        kExtents[2] = zkDim;

        GaussData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gauss = new GenerateGaussian(GaussData, kExtents, sigmas, derivOrder);
        Gauss.calc(false);
        Gauss.finalize();
        Gauss = null;
    }

    public void algorithmPerformed(AlgorithmBase algorithm){
    	if(!algorithm.isCompleted()){
    		finalize();
    		return;
    	}
    	if (algorithm instanceof AlgorithmConvolver) {
			AlgorithmConvolver convolver = (AlgorithmConvolver) algorithm;
            if (srcImage.isColorImage()) {
                convertCIELabtoRGB(convolver.getOutputBuffer());
            }
			try {
				targetImage.importData(0, convolver.getOutputBuffer(), true);
			} catch (IOException error) {
				errorCleanUp("Algorithm Gaussian Blur: Image(s) locked",
						false);

				return;
			}
			this.setCompleted(true);
		}
    }
    
    
    private void convertRGBtoCIELab() {
        // Observer = 2 degrees, Illuminant = D65
        double XN = 95.047;
        double YN = 100.000;
        double ZN = 108.883;
        int i;
        double varR, varG, varB;
        double X, Y, Z;
        double varX, varY, varZ;
        float L, a, b;
        int length = 4 * targetImage.getSliceSize();
        float buffer[] = new float[length];
        int t;
        int z;
        int tDim = 1;
        int zDim = 1;
        float minL = Float.MAX_VALUE;
        float maxL = -Float.MAX_VALUE;
        float mina = Float.MAX_VALUE;
        float maxa = -Float.MAX_VALUE;
        float minb = Float.MAX_VALUE;
        float maxb = -Float.MAX_VALUE;
        float LRange;
        float aRange;
        float bRange;
        double maxDistance;
        double intensitySigma;
        if (srcImage.getNDims() >= 3) {
            tDim = srcImage.getExtents()[2];
        }
        if (srcImage.getNDims() >= 4) {
            tDim = srcImage.getExtents()[3];
        }
        
        for (t = 0; (t < tDim) && !threadStopped; t++) {
            for (z = 0; (z < zDim) && !threadStopped; z++) {
                try {
                    srcImage.exportData((t * zDim + z) * length, length, buffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm Bilateral Filter: Image(s) locked");
                    setCompleted(false);
                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
                    srcImage.releaseLock();

                    return;
                }
                for (i = 0; i < buffer.length; i += 4) {
                    varR = buffer[i+1]/scaleMax;
                    varG = buffer[i+2]/scaleMax;
                    varB = buffer[i+3]/scaleMax;
                    
                    if (varR <= 0.04045) {
                        varR = varR/12.92;
                    }
                    else {
                        varR = Math.pow((varR + 0.055)/1.055, 2.4);
                    }
                    if (varG <= 0.04045) {
                        varG = varG/12.92;
                    }
                    else {
                        varG = Math.pow((varG + 0.055)/1.055, 2.4);
                    }
                    if (varB <= 0.04045) {
                        varB = varB/12.92;
                    }
                    else {
                        varB = Math.pow((varB + 0.055)/1.055, 2.4);
                    }
                    
                    varR = 100.0 * varR;
                    varG = 100.0 * varG;
                    varB = 100.0 * varB;
                    
                    // Observer = 2 degrees, Illuminant = D65
                    X = 0.4124*varR + 0.3576*varG + 0.1805*varB;
                    Y = 0.2126*varR + 0.7152*varG + 0.0722*varB;
                    Z = 0.0193*varR + 0.1192*varG + 0.9505*varB;
                    
                    varX = X/ XN;
                    varY = Y/ YN;
                    varZ = Z/ ZN;
                    
                    if (varX > 0.008856) {
                        varX = Math.pow(varX, 1.0/3.0);
                    }
                    else {
                        varX = (7.787 * varX) + (16.0/116.0);
                    }
                    if (varY > 0.008856) {
                        varY = Math.pow(varY, 1.0/3.0);
                    }
                    else {
                        varY = (7.787 * varY) + (16.0/116.0);
                    }
                    if (varZ > 0.008856) {
                        varZ = Math.pow(varZ, 1.0/3.0);
                    }
                    else {
                        varZ = (7.787 * varZ) + (16.0/116.0);
                    }
                    
                    L = (float)((116.0 * varY) - 16.0);
                    a = (float)(500.0 * (varX - varY));
                    b = (float)(200.0 * (varY - varZ));
                    
                    if (L < minL) {
                        minL = L;
                    }
                    if (L > maxL) {
                        maxL = L;
                    }
                    if (a < mina) {
                        mina = a;
                    }
                    if (a > maxa) {
                        maxa = a;
                    }
                    if (b < minb) {
                        minb = b;
                    }
                    if (b > maxb) {
                        maxb = b;
                    }
                    
                    buffer[i+1] = L;
                    buffer[i+2] = a;
                    buffer[i+3] = b;
                } // for (i = 0; i < buffer.length; i += 4)
                try {
                    cieLabImage.importData((t * zDim + z) * length, buffer, false);
                } catch (IOException error) {
                    errorCleanUp("Algorithm Bilateral Filter: Image(s) locked", false);

                    return;
                }
            } // for (z = 0; (z < zDim) && !threadStopped; z++)
        } // for (t = 0; (t < tDim) && !threadStopped; t++)
        LRange = maxL - minL;
        aRange = maxa - mina;
        bRange = maxb - minb;
        maxDistance = (float)Math.sqrt(LRange*LRange + aRange*aRange + bRange*bRange);
        intensitySigma = intensityFraction * maxDistance;
        intensityGaussianDenom = (2.0 * intensitySigma * intensitySigma);
    } // private void convertRGBtoCIELab()
    
    private void convertCIELabtoRGB(float buffer[]) {
        // Observer = 2 degrees, Illuminant = D65
        double XN = 95.047;
        double YN = 100.000;
        double ZN = 108.883;
        int i;
        double varX, varY, varZ;
        double L, a, b;
        double varX3, varY3, varZ3;
        double X, Y, Z;
        double varR, varG, varB;
        double R, G, B;
        for (i = 0; i < buffer.length; i += 4) {
            L = (double)buffer[i+1];
            a = (double)buffer[i+2];
            b = (double)buffer[i+3];
            
            varY = (L + 16.0)/116.0;
            varX = a/500.0 + varY;
            varZ = varY - b/200.0;
            
            varX3 = Math.pow(varX, 3.0);
            if (varX3 > 0.008856) {
                varX = varX3;
            }
            else {
                varX = (varX - 16.0/116.0)/7.787;
            }
            varY3 = Math.pow(varY, 3.0);
            if (varY3 > 0.008856) {
                varY = varY3;
            }
            else {
                varY = (varY - 16.0/116.0)/7.787;
            }
            varZ3 = Math.pow(varZ, 3.0);
            if (varZ3 > 0.008856) {
                varZ = varZ3;
            }
            else {
                varZ = (varZ - 16.0/116.0)/7.787;
            }
            
            X = XN * varX;
            Y = YN * varY;
            Z = ZN * varZ;
            
            varX = X / 100.0;
            varY = Y / 100.0;
            varZ = Z / 100.0;
            
            varR = 3.2406 * varX - 1.5372 * varY - 0.4986 * varZ;
            varG = -0.9689 * varX + 1.8758 * varY + 0.0415 * varZ;
            varB = 0.0557 * varX - 0.2040 * varY + 1.0570 * varZ;
            
            if (varR > 0.0031308) {
                varR = 1.055 * (Math.pow(varR, 1.0/2.4)) - 0.055;
            }
            else {
                varR = 12.92 * varR;
            }
            if (varG > 0.0031308) {
                varG = 1.055 * (Math.pow(varG, 1.0/2.4)) - 0.055;
            }
            else {
                varG = 12.92 * varG;
            }
            if (varB > 0.0031308) {
                varB = 1.055 * (Math.pow(varB, 1.0/2.4)) - 0.055;
            }
            else {
                varB = 12.92 * varB;
            }
            
            R = scaleMax * varR;
            G = scaleMax * varG;
            B = scaleMax * varB;
            
            buffer[i+1] = (float)R;
            buffer[i+2] = (float)G;
            buffer[i+3] = (float)B;
        } // for (i = 0; i < buffer.length; i += 4)
    } // private void convertCIELabtoRGB(float buffer[])
}

