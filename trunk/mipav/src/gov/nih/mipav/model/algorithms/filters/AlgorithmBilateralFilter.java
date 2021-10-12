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
 * The routine bilateralFilter is obtained from the modification of code in OpenCl code in bilateral_filter-dispatch.cpp 
 * with the following license:
 * M///////////////////////////////////////////////////////////////////////////////////////
//
//  IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
//
//  By downloading, copying, installing or using the software you agree to this license.
//  If you do not agree to this license, do not download, install,
//  copy or use the software.
//
//
//                           License Agreement
//                For Open Source Computer Vision Library
//
// Copyright (C) 2000-2008, 2018, Intel Corporation, all rights reserved.
// Copyright (C) 2009, Willow Garage Inc., all rights reserved.
// Copyright (C) 2014-2015, Itseez Inc., all rights reserved.
// Third party copyrights are property of their respective owners.
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
//   * Redistribution's of source code must retain the above copyright notice,
//     this list of conditions and the following disclaimer.
//
//   * Redistribution's in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//
//   * The name of the copyright holders may not be used to endorse or promote products
//     derived from this software without specific prior written permission.
//
// This software is provided by the copyright holders and contributors "as is" and
// any express or implied warranties, including, but not limited to, the implied
// warranties of merchantability and fitness for a particular purpose are disclaimed.
// In no event shall the Intel Corporation or contributors be liable for any direct,
// indirect, incidental, special, exemplary, or consequential damages
// (including, but not limited to, procurement of substitute goods or services;
// loss of use, data, or profits; or business interruption) however caused
// and on any theory of liability, whether in contract, strict liability,
// or tort (including negligence or otherwise) arising in any way out of
// the use of this software, even if advised of the possibility of such damage.
//
//M

 * 
 *
 * @version  0.1 February 5, 2009
 * @author   William Gandler
 * @see      GenerateGaussian
 * @see      AlgorithmConvolver
 */
public class AlgorithmBilateralFilter extends AlgorithmBase implements AlgorithmInterface{

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private final int BORDER_CONSTANT = 0; // iiiiii|abcdefgh|iiiiiii with some specified i
	private final int BORDER_REPLICATE = 1; // aaaaaa|abcdefgh|hhhhhhh
	private final int BORDER_REFLECT = 2; // fedcba|abcdefgh|hgfedcb
	private final int BORDER_WRAP = 3; // cdefgh|abcdefgh|abcdefg
	private final int BORDER_REFLECT_101 = 4; // gfedcb|abcdefgh|gfedcba
	private final int BORDER_DEFAULT = BORDER_REFLECT_101;

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
    
    private boolean useProgressBar = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    public AlgorithmBilateralFilter() {
    	
    }

    /**
     * Creates a new AlgorithmBilateralFilter object.
     *
     * @param  srcImg    DOCUMENT ME!
     * @param  sigmas    DOCUMENT ME!
     * @param  intensityFraction
     * @param  maskFlag  DOCUMENT ME!
     * @param  img25D    DOCUMENT ME!
     * @param  useProgressBar
     */
    public AlgorithmBilateralFilter(ModelImage srcImg, float[] sigmas, float intensityFraction, boolean maskFlag, boolean img25D,
    		boolean useProgressBar) {
        this(null, srcImg, sigmas, intensityFraction, maskFlag, img25D, useProgressBar);
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
     * @param  useProgressBar
     */
    public AlgorithmBilateralFilter(ModelImage destImg, ModelImage srcImg, float[] sigmas, float intensityFraction, boolean maskFlag,
                                 boolean img25D, boolean useProgressBar) {
        super(destImg, srcImg);

        this.sigmas = sigmas;
        this.intensityFraction = intensityFraction;
        entireImage = maskFlag;
        image25D = img25D;
        this.useProgressBar = useProgressBar;
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
        
        //long startTime = System.nanoTime();
        if (useProgressBar) {
            fireProgressStateChanged(0, srcImage.getImageName(), "Bilateral filter on image ...");
        }
        
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
        if (useProgressBar) {
	        convolver.setMinProgressValue(0);
	        convolver.setMaxProgressValue(100);
			linkProgressToAlgorithm(convolver);
        }
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
            zDim = srcImage.getExtents()[2];
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
            if (R < 0) {
            	R = 0;
            }
            if (R > scaleMax) {
            	R = scaleMax;
            }
            G = scaleMax * varG;
            if (G < 0) {
            	G = 0;
            }
            if (G > scaleMax) {
            	G = scaleMax;
            }
            B = scaleMax * varB;
            if (B < 0) {
            	B = 0;
            }
            if (B > scaleMax) {
            	B = scaleMax;
            }
            
            buffer[i+1] = (float)R;
            buffer[i+2] = (float)G;
            buffer[i+3] = (float)B;
        } // for (i = 0; i < buffer.length; i += 4)
    } // private void convertCIELabtoRGB(float buffer[])
    
    public double[][] bilateralFilter(double src[][], int channels, int d,
            double sigmaColor, double sigmaSpace,
            int borderType ) {
    	double dst[][];
    	if (src == null) {
    		System.err.println("src == null in bilateralFilter");
    		return null;
    	}
    	
    	if (src[0] == null) {
    		System.err.println("src[0] == null in bilateralFilter");
    		return null;
    	}
    	
    	dst = new double[src.length][src[0].length];
    	
        int i, j, radius, y, x;
        double minValSrc= Double.MAX_VALUE, maxValSrc= -Double.MAX_VALUE;
        double r2;
        double totalWeight;
        double totalSum;
        double totalSumR;
        double totalSumG;
        double totalSumB;
        double weight;
        double valueDiff;
        double valueDiffR;
        double valueDiffG;
        double valueDiffB;
        int c;
        double srcChannel[][];
  
        if ((channels != 1) && (channels != 3)) {
        	System.err.println("channels = " + channels + " bilateralFilter");
        	return null;
        }
        
        if( sigmaColor <= 0 )
            sigmaColor = 1;
        if( sigmaSpace <= 0 )
            sigmaSpace = 1;

        double gauss_color_coeff = -0.5/(sigmaColor*sigmaColor);
        double gauss_space_coeff = -0.5/(sigmaSpace*sigmaSpace);

        if( d <= 0 )
            radius = (int)Math.round(sigmaSpace*1.5);
        else
            radius = d/2;
        radius = Math.max(radius, 1);
        d = radius*2 + 1;
        // compute the min/max range for the input image (even if multichannel)
        for (i = 0; i < src.length; i++) {
        	for (j = 0; j < src[i].length; j++) {
	        	if (src[i][j] < minValSrc) {
	        		minValSrc = src[i][j];
	        	}
	        	if (src[i][j] > maxValSrc) {
	        		maxValSrc = src[i][j];
	        	}
        	}
        }
       // epsilon = D1MACH(4)
       // Machine epsilon is the smallest positive epsilon such that
       // (1.0 + epsilon) != 1.0.
       // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
       // epsilon = 2.2204460e-16
       // epsilon is called the largest relative spacing
       double epsilon = 1.0;
       double neweps = 1.0;

       while (true) {

           if (1.0 == (1.0 + neweps)) {
               break;
           } else {
               epsilon = neweps;
               neweps = neweps / 2.0;
           }
       } // while(true)
        	
        if (Math.abs(minValSrc - maxValSrc) < epsilon) {
            for (i = 0; i < src.length; i++) {
            	for (j = 0; j < src[i].length; i++) {
            		dst[i][j] = src[i][j];
            	}
            }
            
            return dst;
        }
        
        // temporary copy of the image with borders for easy processing
        double temp[][] = null;
        if (channels == 1) {
            temp = copyMakeBorder( src, radius, radius, radius, radius, borderType, 0.0);
        }
        else {
        	temp = new double[src.length + 6*radius][src[0].length + 6*radius];
        	srcChannel = new double[src.length][src[0].length/3];
        	for (c = 0; c < 3; c++) {
        	    for (y = 0; y < src.length; y++) {
        	    	for (x = 0; x < src[0].length/3; x++) {
        	    		srcChannel[y][x] = src[y][3*x + c];
        	    	}
        	    }
        	    double tempChannel[][] = copyMakeBorder(srcChannel, radius, radius, radius, radius, borderType, 0.0);
        	    for (y = 0; y < tempChannel.length; y++) {
        	    	for (x = 0; x < tempChannel[0].length; x++) {
        	    		temp[y][3*x+c] =  tempChannel[y][x];
        	    	}
        	    }
        	}
        }
        
     // initialize space-related bilateral filter coefficients
     double space_weight[][] = new double[d][d];
     for (i = -radius; i <= radius; i++) {
    	 for (j = -radius; j <= radius; j++) {
    	     r2 = i*i + j*j;
    	     space_weight[i + radius][j + radius] = Math.exp(-r2 * gauss_space_coeff);
    	 }
     }
     
     if (channels == 1) {
    	 for (y = 0; y < src.length; y++) {
    		 for (x = 0; x < src[0].length; x++) {
    		     totalWeight = 0.0;
    		     totalSum = 0.0;
    		     for (i = -radius; i <= radius; i++) {
    		    	 for (j = -radius; j <= radius; j++) {
    		    		 valueDiff = temp[y + radius][x + radius] - temp[y + radius + i][x + radius + j];
    		    		 weight = space_weight[i + radius][j + radius] * Math.exp(valueDiff * valueDiff * gauss_color_coeff);
    		    		 totalWeight += weight;
    		    		 totalSum += weight * temp[y + radius + i][x + radius + j];
    		    	 }
    		     }
    		     dst[y][x] = totalSum/totalWeight;
    		 }
    	 }
     } // if (channels == 1)
     else if (channels == 3) {
    	 for (y = 0; y < src.length; y++) {
    		 for (x = 0; x < src[0].length/3; x++) {
    		     totalWeight = 0.0;
    		     totalSumR = 0.0;
    		     totalSumG = 0.0;
    		     totalSumB = 0.0;
    		     for (i = -radius; i <= radius; i++) {
    		    	 for (j = -radius; j <= radius; j++) {
    		    		 valueDiffR = temp[y + radius][3*(x + radius)] - temp[y + radius + i][3*(x + radius + j)];
    		    		 valueDiffG = temp[y + radius][3*(x + radius) + 1] - temp[y + radius + i][3*(x + radius + j) + 1];
    		    		 valueDiffB = temp[y + radius][3*(x + radius) + 2] - temp[y + radius + i][3*(x + radius + j) + 2];
    		    		 valueDiff = Math.abs(valueDiffR) + Math.abs(valueDiffG) + Math.abs(valueDiffB);
    		    		 weight = space_weight[i + radius][j + radius] * Math.exp(valueDiff * valueDiff * gauss_color_coeff);
    		    		 totalWeight += weight;
    		    		 totalSumR += weight * temp[y + radius + i][3*(x + radius + j)];
    		    		 totalSumG += weight * temp[y + radius + i][3*(x + radius + j) + 1];
    		    		 totalSumB += weight * temp[y + radius + i][3*(x + radius + j) + 2];
    		    	 }
    		     }
    		     dst[y][3*x] = totalSumR/totalWeight;
    		     dst[y][3*x+1] = totalSumG/totalWeight;
    		     dst[y][3*x+2] = totalSumB/totalWeight;
    		 }
    	 }	 
     }

    	
    	return dst;
    }
        
    private double[][] copyMakeBorder(double src[][], int top, int bottom, int left, int right, int borderType, double borderValue) {
    	int i,j;
    	double dst[][] = new double[src.length + top + bottom][src[0].length + left + right];
    	for (i = 0; i < src.length; i++) {
    		for (j = 0; j < src[0].length; j++) {
    			dst[i+top][j+left] = src[i][j];
    		}
    	}
    	
    	for (i = 0; i < top; i++) {
    		for (j = 0; j < left; j++) {
    		   switch (borderType) {
    		   case BORDER_CONSTANT:
    			   dst[i][j] = borderValue;
    			   break;
    		   case BORDER_REPLICATE:
    			   dst[i][j] = src[0][0];
    			   break;
    		   case BORDER_REFLECT:
    			   dst[i][j] = src[top - 1 - i][left - 1 - j];
    			   break;
    		   case BORDER_REFLECT_101:
     			   dst[i][j] = src[top - i][left - j];
     			   break;
    		   }
    		  
 		   }
    		
    		for (j = left; j < src[0].length + left; j++) {
    			switch (borderType) {
    			case BORDER_CONSTANT:
    				dst[i][j] = borderValue;
    				break;
    			case BORDER_REPLICATE:
    				dst[i][j] = src[0][j - left];
    				break;
    			case BORDER_REFLECT:
    				dst[i][j] = src[top - 1 - i][j - left];
    				break;
    			case BORDER_REFLECT_101:
    				dst[i][j] = src[top - i][j - left];
    				break;
    			}
    		}
    		
    		for (j = src[0].length + left; j < src[0].length + left + right; j++) {
    		    switch(borderType) {
    		    case BORDER_CONSTANT:
     			   dst[i][j] = borderValue;
     			   break;
    		    case BORDER_REPLICATE:
    		    	dst[i][j] = src[0][src[0].length-1];
    		    	break;
    		    case BORDER_REFLECT:
    		    	dst[i][j] = src[top - 1 - i][2*src[0].length + left - 1 - j];
    		    	break;
    		    case BORDER_REFLECT_101:
    		    	dst[i][j] = src[top - i][2*src[0].length + left - 2 - j];
    		    	break;
    		    }
    		}
    	}
    	
    	for (i = top + src.length; i < top + src.length + bottom; i++) {
            for (j = 0; j < left; j++) {
    		    switch (borderType) {
    		    case BORDER_CONSTANT:
     			   dst[i][j] = borderValue;
     			   break;
    		    case BORDER_REPLICATE:
    		    	dst[i][j] = src[src.length-1][0];
    		    	break;
    		    case BORDER_REFLECT:
    		    	dst[i][j] = src[2*src.length + top - 1 - i][left - 1 - j];
    		    	break;
    		    case BORDER_REFLECT_101:
    		    	dst[i][j] = src[2*src.length + top - 2 - i][left - j];
    		    	break;
    		    }
    		}
            
            for (j = left; j < src[0].length + left; j++) {
    			switch (borderType) {
    			case BORDER_CONSTANT:
    				dst[i][j] = borderValue;
    				break;
    			case BORDER_REPLICATE:
    				dst[i][j] = src[src.length-1][j - left];
    				break;
    			case BORDER_REFLECT:
    				dst[i][j] = src[2*src.length + top - 1 - i][j - left];
    				break;
    			case BORDER_REFLECT_101:
    				dst[i][j] = src[2*src.length + top - 2 - i][j - left];
    				break;
    			}
    		}
    		
    		for (j = src[0].length + left; j < src[0].length + left + right; j++) {
    		    switch(borderType) {
    		    case BORDER_CONSTANT:
     			   dst[i][j] = borderValue;
     			   break;
    		    case BORDER_REPLICATE:
    		    	dst[i][j] = src[src.length-1][src[0].length-1];
    		    	break;
    		    case BORDER_REFLECT:
    		        dst[i][j] = src[2*src.length + top - 1 - i][2*src[0].length + left - 1 - j];
    		        break;
    		    case BORDER_REFLECT_101:
    		        dst[i][j] = src[2*src.length + top - 2 - i][2*src[0].length + left - 2 - j];
    		        break;
    		    }
    		}	
    	}
    	
    	for (i = top; i < src.length + top; i++) {
    		for (j = 0; j < left; j++) {
    			switch(borderType) {
    			case BORDER_CONSTANT:
    				dst[i][j] = borderValue;
    				break;
    			case BORDER_REPLICATE:
    				dst[i][j] = src[i - top][0];
    				break;
    			case BORDER_REFLECT:
    				dst[i][j] = src[i - top][left - 1 - j];
    				break; 
    			case BORDER_REFLECT_101:
    				dst[i][j] = src[i - top][left - j];
    				break;  
    			}
    		}
    		
    		for (j = src[0].length + left; j < src[0].length + left + right; j++) {
    		    switch(borderType) {
    		    case BORDER_CONSTANT:
     			   dst[i][j] = borderValue;
     			   break;
    		    case BORDER_REPLICATE:
    		    	dst[i][j] = src[i - top][src[0].length-1];
    		    	break;
    		    case BORDER_REFLECT:
    		        dst[i][j] = src[i - top][2*src[0].length + left - 1 - j];
    		        break;
    		    case BORDER_REFLECT_101:
    		        dst[i][j] = src[i - top][2*src[0].length + left - 2 - j];
    		        break;
    		    }
    		}	
    	}
    	
    	
    	return dst;
    }

}

