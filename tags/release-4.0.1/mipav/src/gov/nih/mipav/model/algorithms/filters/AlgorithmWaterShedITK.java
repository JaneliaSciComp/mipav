package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import InsightToolkit.*;

import java.io.*;


/**
 * Implementation of a WaterShed segmenation using functionality provided by the InsightToolkit library (www.itk.org).
 *
 * <p>The application of this algorithm segment an image or VOI region of the image with a WaterShed filter at a user
 * defined scale (conductance, level, iterations, threshold). The first step in the image processing pipeline is diffusion 
 * of the color input image using an aniso tropic diffusion filter. </p>
 *
 * <p>1D Gaussian = (1/sqrt(2*PI*sigma*sigma))*exp(-x*x/(2*sigma*sigma));</p>
 *
 * <p>Advantages to convolving the Gaussian function to blur an image include:</p>
 *
 * <p>1. Structure will not be added to the image. 2. Can be analytically calculated, as well as the Fourier Transform
 * of the Gaussian. 3. By varying the SD a Gaussian scale-space can easily be constructed.</p>
 */
public class AlgorithmWaterShedITK extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flags indicate which color channel to process. True indicates the channel should be processed. */
    private boolean[] abProcessChannel = new boolean[] {
                                             false, // alpha
                                             true, // red
                                             true, // green
                                             true // blue
                                         };

    /** Dimensionality of the kernel. */
    private int[] kExtents;

    private float conductance;
    private int iterations;
    private float threshold;
    private float level;
    private float timeStep;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmWaterShedITK object.
     *
     * @param  srcImg  source image model
     * @param  params  WaterShed parameters
     * @param  img25D  Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                 images disregard this flag.
     */
    public AlgorithmWaterShedITK(ModelImage srcImg, float[] params, boolean img25D) {
        super(null, srcImg);
        image25D = img25D;
        conductance = params[0];
        iterations = (int)params[1];
        threshold = params[2];
        level = params[3];
        timeStep = params[4];
    }

    /**
     * Creates a new AlgorithmWaterShedITK object.
     *
     * @param  destImg  image model where result image is to stored
     * @param  srcImg   source image model
     * @param  params   WaterShed parameters
     * @param  img25D   Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                  images disregard this flag. 
     */
    public AlgorithmWaterShedITK(ModelImage destImg, ModelImage srcImg, float[] params, boolean img25D) {
        super(destImg, srcImg);
        image25D = img25D;
        conductance = params[0];
        iterations = (int)params[1];
        threshold = params[2];
        level = params[3];
        timeStep = params[4];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        kExtents = null;
        abProcessChannel = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        if ((srcImage.getNDims() == 3) && image25D) {
            fireProgressStateChanged(0, null, "WaterShed slices ...");
        } else {
            fireProgressStateChanged(0, null, "WaterShed ...");
        }

        
        // 2D or 2.5D
        if ((2 == srcImage.getNDims()) || ((3 == srcImage.getNDims()) && image25D)) {

            
            
            // Color 2D
            if ((2 == srcImage.getNDims()) && srcImage.isColorImage()) {
                  
                // store result in target image
                if (null != destImage) {

                    try {
                        destImage.setLock(ModelStorageBase.RW_LOCKED);
                    } catch (IOException error) {
                        errorCleanUp("WahterShed: Image(s) locked", false);

                        return;
                    }

                    for (int iChannel = 0; iChannel < 4; iChannel++) {
                        itkImageF2 kImageSrcITK = InsightToolkitSupport.itkCreateImageColor2D(srcImage, iChannel);
                        
                        // filter channel and write result to target image
                        if (abProcessChannel[iChannel]) {
                        	  itkImageUL2 resultImage = perform2DFiltering(kImageSrcITK);
                              InsightToolkitSupport.itkTransferImageColor2D(kImageSrcITK, resultImage, mask,
                                      srcImage, iChannel);
                        }
                        // just copy channel from source to target image
                        else {
                        	itkImageUL2 resultImage = perform2DFiltering(kImageSrcITK);
                        	InsightToolkitSupport.itkTransferImageColor2D(kImageSrcITK, resultImage, mask,
                                      srcImage, iChannel);
                        }
                    }

                    destImage.releaseLock();

                }

                // store result back into the source image
                else {

                    for (int iChannel = 0; iChannel < 4; iChannel++) {

                        if (abProcessChannel[iChannel]) {
                            itkImageF2 kImageSrcITK = InsightToolkitSupport.itkCreateImageColor2D(srcImage, iChannel);
                            itkImageUL2 resultImage = perform2DFiltering(kImageSrcITK);
                            InsightToolkitSupport.itkTransferImageColor2D(kImageSrcITK, resultImage, mask,
                                                                          srcImage, iChannel);
                        }
                    }
                } 
            }
             
                
            // Single channel 2D
            else if ((2 == srcImage.getNDims()) && !srcImage.isColorImage()) {
                itkImageF2 kImageSrcITK = InsightToolkitSupport.itkCreateImageSingle2D(srcImage);
                itkImageUL2 resultImage = perform2DFiltering(kImageSrcITK);
                
                // store result in target image
                if (null != destImage) {

                    try {
                        destImage.setLock(ModelStorageBase.RW_LOCKED);
                    } catch (IOException error) {
                        errorCleanUp("GaussianBlurITK: Image(s) locked", false);

                        return;
                    }

                    InsightToolkitSupport.itkTransferImageSingle2D(kImageSrcITK, resultImage, mask,
                                                                   destImage);

                    destImage.releaseLock();
                }

                // store result back in source image
                else {
                    InsightToolkitSupport.itkTransferImageSingle2D(kImageSrcITK, resultImage, mask,
                                                                   srcImage);
                }
            }

            // Color 2.5D
            else if (image25D && srcImage.isColorImage()) {
                
                // store result in target image
                if (null != destImage) {

                    try {
                        destImage.setLock(ModelStorageBase.RW_LOCKED);
                    } catch (IOException error) {
                        errorCleanUp("GaussianBlurITK: Image(s) locked", false);

                        return;
                    }

                    int iNumSlices = srcImage.getExtents()[2];

                    for (int iSlice = 0; iSlice < iNumSlices; iSlice++) {

                        fireProgressStateChanged(((float) (iSlice + 1) / iNumSlices), null, null);
                       
                        if (threadStopped) {

                            finalize();

                            return;
                        }

                        for (int iChannel = 0; iChannel < 4; iChannel++) {
                            itkImageF2 kImageSrcITK = InsightToolkitSupport.itkCreateImageColorSlice(srcImage, iSlice,
                                                                                                     iChannel);

                            // filter channel and write result to target image
                            if (abProcessChannel[iChannel]) {
                            	itkImageUL2 resultImage = perform2DFiltering(kImageSrcITK);

                                InsightToolkitSupport.itkTransferImageColorSlice(kImageSrcITK, resultImage,
                                                                                 mask, destImage, iSlice, iChannel);
                            }
                            // just copy channel from source to target image
                            else {
                            	itkImageUL2 resultImage = perform2DFiltering(kImageSrcITK);
                                InsightToolkitSupport.itkTransferImageColorSlice(kImageSrcITK, resultImage, mask,
                                                                                 destImage, iSlice, iChannel);
                            }
                        }
                    }

                    destImage.releaseLock();
                }

                // store result back into the source image
                else {
                    int iNumSlices = srcImage.getExtents()[2];

                    for (int iSlice = 0; iSlice < iNumSlices; iSlice++) {

                        fireProgressStateChanged(((float) (iSlice + 1) / iNumSlices), null, null);
                        
                        if (threadStopped) {

                            finalize();

                            return;
                        }

                        for (int iChannel = 0; iChannel < 4; iChannel++) {

                            if (abProcessChannel[iChannel]) {
                                itkImageF2 kImageSrcITK = InsightToolkitSupport.itkCreateImageColorSlice(srcImage,
                                                                                                         iSlice,
                                                                                                         iChannel);
                                itkImageUL2 resultImage = perform2DFiltering(kImageSrcITK);

                                InsightToolkitSupport.itkTransferImageColorSlice(kImageSrcITK, resultImage,
                                                                                 mask, srcImage, iSlice, iChannel);
                            }
                        }
                    }
                } 
            }

            // Single channel 2.5D
            else if (image25D && !srcImage.isColorImage()) {

                // store result in target image
                if (null != destImage) {

                    try {
                        destImage.setLock(ModelStorageBase.RW_LOCKED);
                    } catch (IOException error) {
                        errorCleanUp("GaussianBlurITK: Image(s) locked", false);

                        return;
                    }

                    int iNumSlices = srcImage.getExtents()[2];

                    for (int iSlice = 0; iSlice < iNumSlices; iSlice++) {

                        fireProgressStateChanged(((float) (iSlice + 1) / iNumSlices), null, null);
                        

                        if (threadStopped) {

                            finalize();

                            return;
                        }

                        itkImageF2 kImageSrcITK = InsightToolkitSupport.itkCreateImageSingle2D(srcImage);
                        itkImageUL2 resultImage = perform2DFiltering(kImageSrcITK);
                        InsightToolkitSupport.itkTransferImageSingleSlice(kImageSrcITK, resultImage, mask,
                                                                          destImage, iSlice);
                    }

                    destImage.releaseLock();
                }

                // store result back into the source image
                else {

                    int iNumSlices = srcImage.getExtents()[2];

                    for (int iSlice = 0; iSlice < iNumSlices; iSlice++) {

                        fireProgressStateChanged(((float) (iSlice + 1) / iNumSlices), null, null);
                        

                        if (threadStopped) {

                            finalize();

                            return;
                        }

                        itkImageF2 kImageSrcITK = InsightToolkitSupport.itkCreateImageSingle2D(srcImage);
                        itkImageUL2 resultImage = perform2DFiltering(kImageSrcITK);
                        InsightToolkitSupport.itkTransferImageSingleSlice(kImageSrcITK, resultImage, mask,
                                                                          srcImage, iSlice);
                    }

                }
            }
        }

        // 3D
        else if ((3 == srcImage.getNDims()) && !image25D) {

           

            // Color 3D
            if (srcImage.isColorImage()) {
                
                // store result in target image
                if (null != destImage) {

                    try {
                        destImage.setLock(ModelStorageBase.RW_LOCKED);
                    } catch (IOException error) {
                        errorCleanUp("GaussianBlurITK: Image(s) locked", false);

                        return;
                    }

                    for (int iChannel = 0; iChannel < 4; iChannel++) {
                        itkImageF3 kImageSrcITK = InsightToolkitSupport.itkCreateImageColor3D(srcImage, iChannel);

                        // filter channel and write result to target image
                        if (abProcessChannel[iChannel]) {
                        	itkImageUL3 resultImage = perform3DFiltering(kImageSrcITK);

                            InsightToolkitSupport.itkTransferImageColor3D(kImageSrcITK, resultImage, mask,
                                                                          destImage, iChannel);
                        }
                        // just copy channel from source to target image
                        else {
                        	itkImageUL3 resultImage = perform3DFiltering(kImageSrcITK);
                            InsightToolkitSupport.itkTransferImageColor3D(kImageSrcITK, resultImage, mask, destImage,
                                                                          iChannel);
                        }
                    }

                    destImage.releaseLock();
                   
                }

                // store result back into the source image
                else {

                    for (int iChannel = 0; iChannel < 4; iChannel++) {

                        if (abProcessChannel[iChannel]) {
                            itkImageF3 kImageSrcITK = InsightToolkitSupport.itkCreateImageColor3D(srcImage, iChannel);
                            itkImageUL3 resultImage = perform3DFiltering(kImageSrcITK);
                            InsightToolkitSupport.itkTransferImageColor3D(kImageSrcITK, resultImage, mask,
                                                                          srcImage, iChannel);
                        }
                    }
                }           
            }

            // Single channel 3D
            else {
            	itkImageF3 kImageSrcITK = InsightToolkitSupport.itkCreateImageSingle3D(srcImage);
            	itkImageUL3 resultImage = perform3DFiltering(kImageSrcITK);
               
                // store result in target image
                if (null != destImage) {

                    try {
                        destImage.setLock(ModelStorageBase.RW_LOCKED);
                    } catch (IOException error) {
                        errorCleanUp("GaussianBlurITK: Image(s) locked", false);

                        return;
                    }

                    InsightToolkitSupport.itkTransferImageSingle3D(kImageSrcITK, resultImage, mask,
                                                                   destImage);

                    destImage.releaseLock();
                }

                // store result back in source image
                else {
                    InsightToolkitSupport.itkTransferImageSingle3D(kImageSrcITK, resultImage, mask,
                                                                   srcImage);
                }
            }
        }

        // unsupported
        else {
            displayError("Algorithm WaterShed ITK: unsupported resolution");
            finalize();

            return;
        }

        
        setCompleted(true);
    }
    
    /**
     * Apply the watershed segmentation algorithm with 3D image.
     * @param kImageSrcITK  itk image source file
     * @return watershed.GetOutput()   watershed algorithm output image file. 
     */
    public itkImageUL3 perform3DFiltering(itkImageF3 kImageSrcITK ) {
    	  itkGradientAnisotropicDiffusionImageFilterF3F3_Pointer diffusion = 
              itkGradientAnisotropicDiffusionImageFilterF3F3.itkGradientAnisotropicDiffusionImageFilterF3F3_New();

            diffusion.SetInput( kImageSrcITK );
            diffusion.SetTimeStep( timeStep );
            diffusion.SetConductanceParameter( conductance );
            diffusion.SetNumberOfIterations(  iterations );

            itkGradientMagnitudeImageFilterF3F3_Pointer gradient = 
              itkGradientMagnitudeImageFilterF3F3.itkGradientMagnitudeImageFilterF3F3_New();

            gradient.SetInput(diffusion.GetOutput());

            itkWatershedImageFilterF3_Pointer watershed = 
              itkWatershedImageFilterF3.itkWatershedImageFilterF3_New();

            watershed.SetInput( gradient.GetOutput() );
            watershed.SetThreshold( threshold );
            watershed.SetLevel( level );
            watershed.Update();
    	return watershed.GetOutput();
    }

    /**
     * Apply the watershed segmentation algorithm with 2D image.
     * @param kImageSrcITK  itk image source file
     * @return watershed.GetOutput()   watershed algorithm output image file. 
     */    
    public itkImageUL2 perform2DFiltering(itkImageF2 kImageSrcITK ) {
    	 itkGradientAnisotropicDiffusionImageFilterF2F2_Pointer diffusion = 
             itkGradientAnisotropicDiffusionImageFilterF2F2.itkGradientAnisotropicDiffusionImageFilterF2F2_New();

           diffusion.SetInput( kImageSrcITK );
           diffusion.SetTimeStep( timeStep );
           diffusion.SetConductanceParameter( conductance );
           diffusion.SetNumberOfIterations(  iterations );

           itkGradientMagnitudeImageFilterF2F2_Pointer gradient = 
             itkGradientMagnitudeImageFilterF2F2.itkGradientMagnitudeImageFilterF2F2_New();

           gradient.SetInput(diffusion.GetOutput());

           itkWatershedImageFilterF2_Pointer watershed = 
             itkWatershedImageFilterF2.itkWatershedImageFilterF2_New();

           watershed.SetInput( gradient.GetOutput() );
           watershed.SetThreshold( threshold );
           watershed.SetLevel( level );
           watershed.Update();
           return watershed.GetOutput();
    }
    
    /**
     * Sets the flag for the blue channel.
     *
     * @param  flag  if set to true then the blue channel is processed.
     */
    public void setBlue(boolean flag) {
        abProcessChannel[3] = flag;
    }

    /**
     * Sets the flag for the green channel.
     *
     * @param  flag  if set to true then the green channel is processed.
     */
    public void setGreen(boolean flag) {
        abProcessChannel[2] = flag;
    }

    /**
     * Sets the flag for the red channel.
     *
     * @param  flag  if set to true then the red channel is processed.
     */
    public void setRed(boolean flag) {
        abProcessChannel[1] = flag;
    }
}
