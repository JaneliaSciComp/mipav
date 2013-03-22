package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.itk.*;

import InsightToolkit.*;

import java.io.*;


/**
 * <p>Implementation of a filter using functionality provided by the
 * InsightToolkit library (www.itk.org).</p>
 *
 * <p>Takes a filter object created using reflection, with parameters already
 * set, and executes it.</p>
 *
 */
public class AlgorithmItkFilter extends AlgorithmBase {

    //~ Instance fields ----------------------------------------------------------------------------

    /** Flags indicate which color channel to process. True indicates the
     * channel should be processed. */
    protected boolean[] m_abProcessChannel = new boolean[] {
                                             false, // alpha
                                             true, // red
                                             true, // green
                                             true // blue
                                         };

    /** 
     * itk filter container 
     */
    protected PItkFilter m_itkFilter = null;

    /**
     * Progress window constant
     */
    private static final float PROG_PRE = 0.1f;
    /**
     * Progress window constant
     */
    private static final float PROG_POST = 0.9f;

    //~ Constructors -------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmItkFilter object.
     *
     * @param  destImg  image model where result image is to stored. May be null.
     * @param  srcImg   source image model
     * @param  filter   itk filter object.
     */
    public AlgorithmItkFilter(ModelImage destImg, ModelImage srcImg, PItkFilter filter) {
        super(destImg, srcImg);

        m_itkFilter = filter;
        
        if (srcImg.getNDims() == 3 && m_itkFilter.getNDims() == 2) {
            image25D = true;
        } else {
            image25D = false;
        }
    }

    //~ Methods ------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        m_itkFilter = null;
        m_abProcessChannel = null;
        super.finalize();
    }

    /** helper method for runAlgorithm */
    protected itkImageBase2 filterInputOutput2D(PItkImage2 kImageSrcITK) {
        AutoItkLoader.invokeMethod("SetInput", m_itkFilter.filter(), 
                                   null, kImageSrcITK.img());
        AutoItkLoader.invokeMethod("Update", m_itkFilter.filter());
        return (itkImageBase2)AutoItkLoader.invokeMethod("GetOutput", m_itkFilter.filter());
    }

    /** helper method for runAlgorithm */
    protected itkImageBase3 filterInputOutput3D(PItkImage3 kImageSrcITK) {
        AutoItkLoader.invokeMethod("SetInput", m_itkFilter.filter(), 
                                   null, kImageSrcITK.img());
        AutoItkLoader.invokeMethod("Update", m_itkFilter.filter());
        return (itkImageBase3)AutoItkLoader.invokeMethod("GetOutput", m_itkFilter.filter());
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
            fireProgressStateChanged(0, null, "Filtering slices ...");
        } else {
            fireProgressStateChanged(0, null, "Filtering ...");
        }

        

        // Make note of the sample spacing.  The ITK image created will
        // already have the sample spacing applied to it, but Gaussian
        // is to be applied in sample space not in real space.
        // float[] afResolutions = srcImage.getFileInfo(0).getResolutions();

        // 2D or 2.5D
        if ((2 == srcImage.getNDims()) || ((3 == srcImage.getNDims()) && image25D)) {

            // Color 2D
            if ((2 == srcImage.getNDims()) && srcImage.isColorImage()) {

                // store result in target image
                if (null != destImage) {

                    try {
                        destImage.setLock(ModelStorageBase.RW_LOCKED);
                    } catch (IOException error) {
                        errorCleanUp("ItkFilter: Image(s) locked", false);

                        return;
                    }

                    for (int iChannel = 0; iChannel < 4; iChannel++) {
                        PItkImage2 kImageSrcITK = InsightToolkitSupport.itkCreateImageColor2D(srcImage, iChannel);

                        // filter channel and write result to target image
                        if (m_abProcessChannel[iChannel]) {
                            itkImageBase2 kImageDestITK = filterInputOutput2D(kImageSrcITK);
                            if (kImageDestITK != null) {
                                InsightToolkitSupport.itkTransferImageColor2D(kImageSrcITK.img(), 
                                                                              kImageDestITK, mask,
                                                                              destImage, iChannel);
                            }
                        }
                        // just copy channel from source to target image
                        else {
                            InsightToolkitSupport.itkTransferImageColor2D(kImageSrcITK.img(), kImageSrcITK.img(), mask, destImage,
                                                                          iChannel);
                        }
                        fireProgressStateChanged((float)(iChannel + 1)/4, null, null);
                    }
                    destImage.calcMinMax();
                    destImage.releaseLock();

                }

                // store result back into the source image
                else {

                    for (int iChannel = 0; iChannel < 4; iChannel++) {

                        if (m_abProcessChannel[iChannel]) {
                            PItkImage2 kImageSrcITK = InsightToolkitSupport.itkCreateImageColor2D(srcImage, iChannel);
                            itkImageBase2 kImageDestITK = filterInputOutput2D(kImageSrcITK);
                            if (kImageDestITK != null) {
                                InsightToolkitSupport.itkTransferImageColor2D(kImageSrcITK.img(), 
                                                                              kImageDestITK, mask,
                                                                              srcImage, iChannel);
                            }
                        }
                        fireProgressStateChanged((float)(iChannel + 1)/4, null, null);
                    }
                    srcImage.calcMinMax();
                }
            }

            // Single channel 2D
            else if ((2 == srcImage.getNDims()) && !srcImage.isColorImage()) {
                PItkImage2 kImageSrcITK = InsightToolkitSupport.itkCreateImageSingle2D(srcImage);
                fireProgressStateChanged(PROG_PRE, null, null);
                itkImageBase2 kImageDestITK = filterInputOutput2D(kImageSrcITK);
                fireProgressStateChanged(PROG_POST, null, null);

                if (kImageDestITK != null) {
                    // store result in target image
                    if (null != destImage) {

                        try {
                            destImage.setLock(ModelStorageBase.RW_LOCKED);
                        } catch (IOException error) {
                            errorCleanUp("ItkFilter: Image(s) locked", false);

                            return;
                        }

                        InsightToolkitSupport.itkTransferImageSingle2D(kImageSrcITK.img(), 
                                                                       kImageDestITK, mask,
                                                                       destImage);

                        destImage.calcMinMax();
                        destImage.releaseLock();
                    }

                    // store result back in source image
                    else {
                        InsightToolkitSupport.itkTransferImageSingle2D(kImageSrcITK.img(), 
                                                                       kImageDestITK, mask,
                                                                       srcImage);
                        srcImage.calcMinMax();
                    }
                }
            }

            // Color 2.5D
            else if (image25D && srcImage.isColorImage()) {

                // store result in target image
                if (null != destImage) {

                    try {
                        destImage.setLock(ModelStorageBase.RW_LOCKED);
                    } catch (IOException error) {
                        errorCleanUp("ItkFilter: Image(s) locked", false);

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
                            PItkImage2 kImageSrcITK = InsightToolkitSupport.itkCreateImageColorSlice(srcImage, iSlice,
                                                                                                     iChannel);

                            // filter channel and write result to target image
                            if (m_abProcessChannel[iChannel]) {
                                itkImageBase2 kImageDestITK = filterInputOutput2D(kImageSrcITK);
                                if (kImageDestITK != null) {
                                    InsightToolkitSupport.itkTransferImageColorSlice(kImageSrcITK.img(),
                                                                                     kImageDestITK, mask,
                                                                                     destImage, iSlice, iChannel);
                                }
                            }
                            // just copy channel from source to target image
                            else {
                                InsightToolkitSupport.itkTransferImageColorSlice(kImageSrcITK.img(), 
                                                                                 kImageSrcITK.img(), mask,
                                                                                 destImage, iSlice, iChannel);
                            }
                        }
                    }

                    destImage.calcMinMax();
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

                            if (m_abProcessChannel[iChannel]) {
                                PItkImage2 kImageSrcITK = InsightToolkitSupport.itkCreateImageColorSlice(srcImage,
                                                                                                         iSlice,
                                                                                                         iChannel);
                                itkImageBase2 kImageDestITK = filterInputOutput2D(kImageSrcITK);
                                if (kImageDestITK != null) {
                                    InsightToolkitSupport.itkTransferImageColorSlice(kImageSrcITK.img(), 
                                                                                     kImageDestITK, mask, 
                                                                                     srcImage, iSlice, iChannel);
                                }
                            }
                        }
                    }
                    srcImage.calcMinMax();
               }
            }

            // Single channel 2.5D
            else if (image25D && !srcImage.isColorImage()) {

                // store result in target image
                if (null != destImage) {

                    try {
                        destImage.setLock(ModelStorageBase.RW_LOCKED);
                    } catch (IOException error) {
                        errorCleanUp("ItkFilter: Image(s) locked", false);

                        return;
                    }

                    int iNumSlices = srcImage.getExtents()[2];

                    for (int iSlice = 0; iSlice < iNumSlices; iSlice++) {

                        fireProgressStateChanged(((float) (iSlice + 1) / iNumSlices), null, null);
                        

                        if (threadStopped) {

                            finalize();

                            return;
                        }

                        PItkImage2 kImageSrcITK = InsightToolkitSupport.itkCreateImageSingleSlice(srcImage, iSlice);
                        itkImageBase2 kImageDestITK = filterInputOutput2D(kImageSrcITK);
                        if (kImageDestITK != null) {
                            InsightToolkitSupport.itkTransferImageSingleSlice(kImageSrcITK.img(), 
                                                                              kImageDestITK, mask,
                                                                              destImage, iSlice);
                        }
                    }

                    destImage.calcMinMax();
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

                        PItkImage2 kImageSrcITK = InsightToolkitSupport.itkCreateImageSingleSlice(srcImage, iSlice);
                        itkImageBase2 kImageDestITK = filterInputOutput2D(kImageSrcITK);
                        
                        if (kImageDestITK != null) {
                            InsightToolkitSupport.itkTransferImageSingleSlice(kImageSrcITK.img(), 
                                                                              kImageDestITK, mask,
                                                                              srcImage, iSlice);
                        }
                    }
                    srcImage.calcMinMax();
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
                        errorCleanUp("ItkFilter: Image(s) locked", false);

                        return;
                    }

                    for (int iChannel = 0; iChannel < 4; iChannel++) {
                        PItkImage3 kImageSrcITK = InsightToolkitSupport.itkCreateImageColor3D(srcImage, iChannel);

                        // filter channel and write result to target image
                        if (m_abProcessChannel[iChannel]) {
                            itkImageBase3 kImageDestITK = filterInputOutput3D(kImageSrcITK);
                            
                            if (kImageDestITK != null) {
                                InsightToolkitSupport.itkTransferImageColor3D(kImageSrcITK.img(),
                                                                              kImageDestITK, mask,
                                                                              destImage, iChannel);
                            }
                        }
                        // just copy channel from source to target image
                        else {
                            InsightToolkitSupport.itkTransferImageColor3D(kImageSrcITK.img(), 
                                                                          kImageSrcITK.img(), mask, destImage,
                                                                          iChannel);
                        }
                        fireProgressStateChanged((float)(iChannel + 1)/4, null, null);
                    }

                    destImage.calcMinMax();
                    destImage.releaseLock();

                }

                // store result back into the source image
                else {

                    for (int iChannel = 0; iChannel < 4; iChannel++) {

                        if (m_abProcessChannel[iChannel]) {
                            PItkImage3 kImageSrcITK = InsightToolkitSupport.itkCreateImageColor3D(srcImage, iChannel);
                            itkImageBase3 kImageDestITK = filterInputOutput3D(kImageSrcITK);
                            
                            if (kImageDestITK != null) {
                                InsightToolkitSupport.itkTransferImageColor3D(kImageSrcITK.img(), 
                                                                              kImageDestITK, mask,
                                                                              srcImage, iChannel);
                            }
                        }
                        fireProgressStateChanged((float)(iChannel + 1)/4, null, null);
                    }
                    srcImage.calcMinMax();
                }

            }

            // Single channel 3D
            else {
                PItkImage3 kImageSrcITK = InsightToolkitSupport.itkCreateImageSingle3D(srcImage);
                fireProgressStateChanged(PROG_PRE, null, null);
                itkImageBase3 kImageDestITK = filterInputOutput3D(kImageSrcITK);
                fireProgressStateChanged(PROG_POST, null, null);
                            
                if (kImageDestITK != null) {
                    // store result in target image
                    if (null != destImage) {

                        try {
                            destImage.setLock(ModelStorageBase.RW_LOCKED);
                        } catch (IOException error) {
                            errorCleanUp("ItkFilter: Image(s) locked", false);

                            return;
                        }

                        InsightToolkitSupport.itkTransferImageSingle3D(kImageSrcITK.img(), 
                                                                       kImageDestITK, mask,
                                                                       destImage);
                        
                        destImage.calcMinMax();
                        destImage.releaseLock();
                    }

                    // store result back in source image
                    else {
                        InsightToolkitSupport.itkTransferImageSingle3D(kImageSrcITK.img(),
                                                                       kImageDestITK, mask,
                                                                       srcImage);
                        srcImage.calcMinMax();
                    }
                }
            }

        }

        // unsupported
        else {
            displayError("Algorithm Itk Filter: unsupported resolution");
            finalize();

            return;
        }

        
        setCompleted(true);
    }

    /**
     * Sets the flag for the blue channel.
     *
     * @param  flag  if set to true then the blue channel is processed.
     */
    public void setBlue(boolean flag) {
        m_abProcessChannel[3] = flag;
    }

    /**
     * Sets the flag for the green channel.
     *
     * @param  flag  if set to true then the green channel is processed.
     */
    public void setGreen(boolean flag) {
        m_abProcessChannel[2] = flag;
    }

    /**
     * Sets the flag for the red channel.
     *
     * @param  flag  if set to true then the red channel is processed.
     */
    public void setRed(boolean flag) {
        m_abProcessChannel[1] = flag;
    }
}
