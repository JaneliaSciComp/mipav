package gov.nih.mipav.model.structures;


import gov.nih.mipav.view.*;

import InsightToolkit.*;

import java.io.*;

import java.util.*;


/**
 * This class provides support methods, mostly static, for use when interfacing to the InsightToolkit Java classes.
 */
public class InsightToolkitSupport {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * Class initializer for all class (static) variables.
     */
    static {

        // Attempt to access a simple class in the InsightToolkit, but
        // setup to catch java.lang.UnsatisfiedLinkError in case the
        // native C++ runtime library for the toolkit cannot be found.
        try {
            itkImageF2.itkImageF2_New();
            bLibraryPresent = true;
        } catch (java.lang.UnsatisfiedLinkError error) {
            Preferences.debug("Insight toolkit error = " + error + "\n");
            bLibraryPresent = false;
        }
    }

    /**
     * This flag is set if the native C++ runtlime library is present which means the methods of the InsightToolkit
     * package can be used.
     */
    private static boolean bLibraryPresent;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Query whether InsightToolkit C++ runtime library is present. If not, then the InsightToolkit java classes cannot
     * be used and any access to them will cause the java.lang.UnsatisfiedLinkError exception to be thrown.
     *
     * @return  boolean True if the InsightToolkit C++ runtime library is present.
     */
    public static boolean isLibraryPresent() {
        return bLibraryPresent;
    }

    /**
     * Create a 2D ITK single channel image with properties and values from the specified channel in color model image.
     *
     * @param   kModelImage  ModelImage Contains image with properties and values to use.
     * @param   iChannel     int Index of the color channel to use (0=alpha, 1=red, 2=green, 3=blue).
     *
     * @return  itkImageF2 New instance created with copied image properties and values.
     */
    public static itkImageF2 itkCreateImageColor2D(ModelImage kModelImage, int iChannel) {

        // If not a color image, then call method which handles the
        // single channel image.
        if (!kModelImage.isColorImage()) {
            return itkCreateImageSingle2D(kModelImage);
        }

        float[] afValues = null;
        itkImageF2 kImageITK = null;

        try {

            // Compute the size of the buffer to store the values extracted
            // for the particular channel.
            int iLength = kModelImage.getSliceSize();

            // Allocate memory for one channel of 2D image and extract
            // the data values into it.
            afValues = new float[iLength];

            // Make sure the channel index is valid for a color image.
            if (iChannel > 3) {
                iChannel = 3;
            } else if (iChannel < 0) {
                iChannel = 0;
            }

            // Extract the color channel of values into buffer
            kModelImage.exportRGBData(iChannel, 0, iLength, afValues);

            // Create ITK 2D image for this single channel of values
            kImageITK = itkCreateImage2D(kModelImage, afValues);
        } catch (IOException error) {
            MipavUtil.displayError("InsightToolkitSupport: image(s) locked");
            kImageITK = null;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("InsightToolkitSupport: out of memory");
            kImageITK = null;
        }

        afValues = null;

        return kImageITK;
    }

    /**
     * Create a 3D ITK single channel image with properties and values from the specified channel in color model image.
     *
     * @param   kModelImage  ModelImage Contains image with properties and values to use.
     * @param   iChannel     int Index of the color channel to use (0=alpha, 1=red, 2=green, 3=blue).
     *
     * @return  itkImageF3 New instance created with copied image properties and values.
     */
    public static itkImageF3 itkCreateImageColor3D(ModelImage kModelImage, int iChannel) {

        // If not a color image, then call method which handles the
        // single channel image.
        if (!kModelImage.isColorImage()) {
            return itkCreateImageSingle3D(kModelImage);
        }

        float[] afValues = null;
        itkImageF3 kImageITK = null;

        try {

            // Compute the size of the buffer to store the values extracted
            // for the particular channel.
            int iLength = kModelImage.getSliceSize() * kModelImage.getExtents()[2];

            // Allocate memory for one channel of 3D image and extract
            // the data values into it.
            afValues = new float[iLength];

            // Make sure the channel index is valid for a color image.
            if (iChannel > 3) {
                iChannel = 3;
            } else if (iChannel < 0) {
                iChannel = 0;
            }

            // Extract the color channel of values into buffer
            kModelImage.exportRGBData(iChannel, 0, iLength, afValues);

            // Create ITK 3D image for this single channel of values
            kImageITK = itkCreateImage3D(kModelImage, afValues);
        } catch (IOException error) {
            MipavUtil.displayError("InsightToolkitSupport: image(s) locked");
            kImageITK = null;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("InsightToolkitSupport: out of memory");
            kImageITK = null;
        }

        afValues = null;

        return kImageITK;
    }

    /**
     * Create a 2D ITK single channel image with properties and values from the specified slice and channel in color
     * model image.
     *
     * @param   kModelImage  ModelImage Contains image with properties and values to use.
     * @param   iSlice       int Index of the slice in the 3D image to use.
     * @param   iChannel     int Index of the color channel to use (0=alpha, 1=red, 2=green, 3=blue).
     *
     * @return  itkImageF2 New instance created with copied image properties and values.
     */
    public static itkImageF2 itkCreateImageColorSlice(ModelImage kModelImage, int iSlice, int iChannel) {

        // If not a color image, then call method which handles the
        // single channel image.
        if (!kModelImage.isColorImage()) {
            return itkCreateImageSingleSlice(kModelImage, iSlice);
        }

        float[] afValues = null;
        itkImageF2 kImageITK = null;

        try {

            // Compute the size of the buffer to store the values extracted
            // for the particular channel.
            int iLength = kModelImage.getSliceSize();

            // Allocate memory for one channel of 2D image and extract
            // the data values into it.
            afValues = new float[iLength];

            // Make sure the channel index is valid for a color image.
            if (iChannel > 3) {
                iChannel = 3;
            } else if (iChannel < 0) {
                iChannel = 0;
            }

            // Extract the color channel of values into buffer
            kModelImage.exportRGBData(iChannel, iSlice * iLength * 4, iLength, afValues);

            // Create ITK 2D image for this single channel of values
            kImageITK = itkCreateImage2D(kModelImage, afValues);
        } catch (IOException error) {
            MipavUtil.displayError("InsightToolkitSupport: image(s) locked");
            kImageITK = null;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("InsightToolkitSupport: out of memory");
            kImageITK = null;
        }

        afValues = null;

        return kImageITK;
    }

    /**
     * Create a 2D ITK single channel image with properties and values from the specified single channel model image.
     *
     * @param   kModelImage  ModelImage Contains image with properties and values to use.
     *
     * @return  itkImageF2 New instance created with copied image properties and values.
     */
    public static itkImageF2 itkCreateImageSingle2D(ModelImage kModelImage) {

        float[] afValues = null;
        itkImageF2 kImageITK = null;

        try {

            // Compute the size of the buffer to store the values extracted
            // for the particular channel.
            int iLength = kModelImage.getSliceSize();

            // Allocate memory for one channel of 2D image and extract
            // the data values into it.
            afValues = new float[iLength];

            // Extract the single channel of values into buffer
            kModelImage.exportData(0, iLength, afValues);

            // Create ITK 2D image for this single channel of values
            kImageITK = itkCreateImage2D(kModelImage, afValues);
        } catch (IOException error) {
            MipavUtil.displayError("InsightToolkitSupport: image(s) locked");
            kImageITK = null;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("InsightToolkitSupport: out of memory");
            kImageITK = null;
        }

        afValues = null;

        return kImageITK;
    }

    /**
     * Create a 3D ITK single channel image with properties and values from the specified single channel model image.
     *
     * @param   kModelImage  ModelImage Contains image with properties and values to use.
     *
     * @return  itkImageF3 New instance created with copied image properties and values.
     */
    public static itkImageF3 itkCreateImageSingle3D(ModelImage kModelImage) {

        float[] afValues = null;
        itkImageF3 kImageITK = null;

        try {

            // Compute the size of the buffer to store the values extracted
            // for the particular channel.
            int iLength = kModelImage.getSliceSize() * kModelImage.getExtents()[2];

            // Allocate memory for one channel of 3D image and extract
            // the data values into it.
            afValues = new float[iLength];

            // Extract the single channel of values into buffer
            kModelImage.exportData(0, iLength, afValues);

            // Create ITK 3D image for this single channel of values
            kImageITK = itkCreateImage3D(kModelImage, afValues);
        } catch (IOException error) {
            MipavUtil.displayError("InsightToolkitSupport: image(s) locked");
            kImageITK = null;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("InsightToolkitSupport: out of memory");
            kImageITK = null;
        }

        afValues = null;

        return kImageITK;
    }

    /**
     * Create a 2D ITK single channel image with properties and values from the specified slice in the single channel 3D
     * model image.
     *
     * @param   kModelImage  ModelImage Contains image with properties and values to use.
     * @param   iSlice       int Index of the slice in the 3D image to use.
     *
     * @return  itkImageF2 New instance created with copied image properties and values.
     */
    public static itkImageF2 itkCreateImageSingleSlice(ModelImage kModelImage, int iSlice) {

        float[] afValues = null;
        itkImageF2 kImageITK = null;

        try {

            // Compute the size of the buffer to store the values extracted
            // for the particular channel.
            int iLength = kModelImage.getSliceSize();

            // Allocate memory for one channel of 2D image and extract
            // the data values into it.
            afValues = new float[iLength];

            // Extract the single channel of values into buffer
            kModelImage.exportData(iSlice * iLength, iLength, afValues);

            // Create ITK 2D image for this single channel of values
            kImageITK = itkCreateImage2D(kModelImage, afValues);
        } catch (IOException error) {
            MipavUtil.displayError("InsightToolkitSupport: image(s) locked");
            kImageITK = null;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("InsightToolkitSupport: out of memory");
            kImageITK = null;
        }

        afValues = null;

        return kImageITK;
    }

    /**
     * Transfers the pixels values from either of two 2D ITK single channel images into the specified color channel in
     * the 2D model image. Pixels are tranferred selected from either ITK image depending on the specification of the
     * mask image. The dimensions of the two ITK images, the model image, and the mask image (if defined) must be
     * compatible.
     *
     * @param  kImageITK0   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iChannel     int Index of the color channel (0=alpha, 1=red, 2=green, 3=blue) into which values will be
     *                      transferred in the model image.
     */
    public static void itkTransferImageColor2D(itkImageF2 kImageITK0, itkImageF2 kImageITK1, BitSet kMask,
                                               ModelImage kModelImage, int iChannel) {

        // If not a color image, then call method which handles the
        // single channel image.
        if (!kModelImage.isColorImage()) {
            itkTransferImageSingle2D(kImageITK0, kImageITK1, kMask, kModelImage);

            return;
        }

        itkTransferImage2D(kImageITK0, kImageITK1, kMask, kModelImage, iChannel, 4);
    }

    public static void itkTransferImageColor2D(itkImageF2 kImageITK0, itkImageUL2 kImageITK1, BitSet kMask,
    		ModelImage kModelImage, int iChannel) {
    	
    	//If not a color image, then call method which handles the
    	//single channel image.
    	if (!kModelImage.isColorImage()) {
    		itkTransferImageSingle2D(kImageITK0, kImageITK1, kMask, kModelImage);
    		
    		return;
    	}
    	
    	itkTransferImage2D(kImageITK0, kImageITK1, kMask, kModelImage, iChannel, 4);
    }

    
    /**
     * Transfers the pixels values from either of two 3D ITK single channel images into the specified color channel in
     * the 3D model image. Pixels are tranferred selected from either ITK image depending on the specification of the
     * mask image. The dimensions of the two ITK images, the model image, and the mask image (if defined) must be
     * compatible.
     *
     * @param  kImageITK0   itkImageF3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageF3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iChannel     int Index of the color channel (0=alpha, 1=red, 2=green, 3=blue) into which values will be
     *                      transferred in the model image.
     */
    public static void itkTransferImageColor3D(itkImageF3 kImageITK0, itkImageF3 kImageITK1, BitSet kMask,
                                               ModelImage kModelImage, int iChannel) {

        // If not a color image, then call method which handles the
        // single channel image.
        if (!kModelImage.isColorImage()) {
            itkTransferImageSingle3D(kImageITK0, kImageITK1, kMask, kModelImage);

            return;
        }

        itkTransferImage3D(kImageITK0, kImageITK1, kMask, kModelImage, iChannel, 4);
    }
    
    /**
     * Transfers the pixels values from either of two 3D ITK single channel images into the specified color channel in
     * the 3D model image. Pixels are tranferred selected from either ITK image depending on the specification of the
     * mask image. The dimensions of the two ITK images, the model image, and the mask image (if defined) must be
     * compatible.
     *
     * @param  kImageITK0   itkImageF3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageUL3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iChannel     int Index of the color channel (0=alpha, 1=red, 2=green, 3=blue) into which values will be
     *                      transferred in the model image.
     */
    public static void itkTransferImageColor3D(itkImageF3 kImageITK0,
			itkImageUL3 kImageITK1, BitSet kMask, ModelImage kModelImage,
			int iChannel) {

		// If not a color image, then call method which handles the
		// single channel image.
		if (!kModelImage.isColorImage()) {
			itkTransferImageSingle3D(kImageITK0, kImageITK1, kMask, kModelImage);

			return;
		}

		itkTransferImage3D(kImageITK0, kImageITK1, kMask, kModelImage, iChannel, 4);
    }

    /**
     * Transfers the pixels values from either of two 2D ITK single channel images into the specified slice and color
     * channel of the 3D model image. Pixels are tranferred selected from either ITK image depending on the
     * specification of the mask image. The dimensions of the two ITK images, the model image, and the mask image (if
     * defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iSlice       int Index of the slice in the 3D model image into values will be transferred in the model
     *                      image.
     * @param  iChannel     int Index of the color channel (0=alpha, 1=red, 2=green, 3=blue) into which values will be
     *                      transferred in the model image.
     */
    public static void itkTransferImageColorSlice(itkImageF2 kImageITK0, itkImageF2 kImageITK1, BitSet kMask,
                                                  ModelImage kModelImage, int iSlice, int iChannel) {

        // If not a color image, then call method which handles the
        // single channel image.
        if (!kModelImage.isColorImage()) {
            itkTransferImageSingleSlice(kImageITK0, kImageITK1, kMask, kModelImage, iSlice);

            return;
        }

        itkTransferImage2D(kImageITK0, kImageITK1, kMask, kModelImage,
                           (kModelImage.getSliceSize() * iSlice * 4) + iChannel, 4);
    }

    /**
     * Transfers the pixels values from either of two 2D ITK single channel images into the specified slice and color
     * channel of the 3D model image. Pixels are tranferred selected from either ITK image depending on the
     * specification of the mask image. The dimensions of the two ITK images, the model image, and the mask image (if
     * defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageUL2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iSlice       int Index of the slice in the 3D model image into values will be transferred in the model
     *                      image.
     * @param  iChannel     int Index of the color channel (0=alpha, 1=red, 2=green, 3=blue) into which values will be
     *                      transferred in the model image.
     */
    public static void itkTransferImageColorSlice(itkImageF2 kImageITK0, itkImageUL2 kImageITK1, BitSet kMask, ModelImage kModelImage,
			int iSlice, int iChannel) {

		// If not a color image, then call method which handles the
		// single channel image.
		if (!kModelImage.isColorImage()) {
			itkTransferImageSingleSlice(kImageITK0, kImageITK1, kMask,
					kModelImage, iSlice);

			return;
		}

		itkTransferImage2D(kImageITK0, kImageITK1, kMask, kModelImage,
				(kModelImage.getSliceSize() * iSlice * 4) + iChannel, 4);
	}
    
    /**
     * Transfers the pixels values from either of two 2D ITK single channel images into the specified single channel 2D
     * model image. Pixels are tranferred selected from either ITK image depending on the specification of the mask
     * image. The dimensions of the two ITK images, the model image, and the mask image (if defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     */
    public static void itkTransferImageSingle2D(itkImageF2 kImageITK0, itkImageF2 kImageITK1, BitSet kMask,
                                                ModelImage kModelImage) {

        itkTransferImage2D(kImageITK0, kImageITK1, kMask, kModelImage, 0, 1);
    }

    public static void itkTransferImageSingle2D(itkImageF2 kImageITK0, itkImageUL2 kImageITK1, BitSet kMask,
            ModelImage kModelImage) {

         itkTransferImage2D(kImageITK0, kImageITK1, kMask, kModelImage, 0, 1);
    }

    
    /**
     * Transfers the pixels values from either of two 3D ITK single channel images into the specified single channel 3D
     * model image. Pixels are tranferred selected from either ITK image depending on the specification of the mask
     * image. The dimensions of the two ITK images, the model image, and the mask image (if defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageF3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     */
    public static void itkTransferImageSingle3D(itkImageF3 kImageITK0, itkImageF3 kImageITK1, BitSet kMask,
                                                ModelImage kModelImage) {

        itkTransferImage3D(kImageITK0, kImageITK1, kMask, kModelImage, 0, 1);
    }
    
    public static void itkTransferImageSingle3D(itkImageF3 kImageITK0, itkImageUL3 kImageITK1, BitSet kMask,
            ModelImage kModelImage) {
 
       itkTransferImage3D(kImageITK0, kImageITK1, kMask, kModelImage, 0, 1);
}

    /**
     * Transfers the pixels values from either of two 2D ITK single channel images into the specified slice in the
     * single channel 3D model image. Pixels are tranferred selected from either ITK image depending on the
     * specification of the mask image. The dimensions of the two ITK images, the model image, and the mask image (if
     * defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iSlice       int Index of the slice in the 3D model image into values will be transferred in the model
     *                      image.
     */
    public static void itkTransferImageSingleSlice(itkImageF2 kImageITK0, itkImageF2 kImageITK1, BitSet kMask,
                                                   ModelImage kModelImage, int iSlice) {

        itkTransferImage2D(kImageITK0, kImageITK1, kMask, kModelImage, kModelImage.getSliceSize() * iSlice, 1);
    }
    
    /**
     * Transfers the pixels values from either of two 2D ITK single channel images into the specified slice in the
     * single channel 3D model image. Pixels are tranferred selected from either ITK image depending on the
     * specification of the mask image. The dimensions of the two ITK images, the model image, and the mask image (if
     * defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageUL2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iSlice       int Index of the slice in the 3D model image into values will be transferred in the model
     *                      image.
     */
    public static void itkTransferImageSingleSlice(itkImageF2 kImageITK0, itkImageUL2 kImageITK1, BitSet kMask,
            ModelImage kModelImage, int iSlice) {

     itkTransferImage2D(kImageITK0, kImageITK1, kMask, kModelImage, kModelImage.getSliceSize() * iSlice, 1);
    }

    /**
     * Create a 2D ITK single channel image with properties and values from the specified single channel model image.
     * The values are either from a 2D model image or a slice from a 3D model image. The pixels values have already been
     * extracted into a linear array. The properties from the input model image that are used include the image size and
     * spacing between samples.
     *
     * @param   kModelImage  ModelImage Contains image with properties and values to use.
     * @param   afValues     float[] Linear array of pixel values from the input model image. The array is ordered such
     *                       that the x coordinate increments the fastest and the y coordinate increments the slowest.
     *
     * @return  itkImageF2 New instance created with copied image properties and values.
     */
    private static itkImageF2 itkCreateImage2D(ModelImage kModelImage, float[] afValues) {

        try {

            // Create instance of ITK 2D image of floating point values
            itkImageF2_Pointer kImageITK = itkImageF2.itkImageF2_New();
            int iSizeX = kModelImage.getExtents()[0];
            int iSizeY = kModelImage.getExtents()[1];

            // Set the ITK image size
            itkImageRegion2 kImageRegion = new itkImageRegion2();
            itkIndex2 kImageStart = new itkIndex2();
            kImageStart.Fill(0);
            kImageRegion.SetIndex(kImageStart);

            itkSize2 kImageSize = new itkSize2();
            kImageSize.SetElement(0, iSizeX);
            kImageSize.SetElement(1, iSizeY);
            kImageRegion.SetSize(kImageSize);
            kImageITK.SetRegions(kImageRegion);

            // Set ITK image spacing
            float[] afResolutions = kModelImage.getFileInfo(0).getResolutions();
            SWIGTYPE_p_float afSwigResolutions = SwigExtras.new_FArray(2);
            SwigExtras.FArray_setitem(afSwigResolutions, 0, afResolutions[0]);
            SwigExtras.FArray_setitem(afSwigResolutions, 1, afResolutions[1]);
            kImageITK.SetSpacing(afSwigResolutions);
            SwigExtras.delete_FArray(afSwigResolutions);

            // Allocate the memory for the ITK image
            kImageITK.Allocate();

            // Copy pixel values from the ModelImage to the ITK image.
            itkIndex2 iIndexITK = new itkIndex2();
            int iIndex = 0;

            for (int iY = 0; iY < iSizeY; iY++) {
                iIndexITK.SetElement(1, iY);

                for (int iX = 0; iX < iSizeX; iX++) {
                    iIndexITK.SetElement(0, iX);

                    kImageITK.SetPixel(iIndexITK, afValues[iIndex++]);
                }
            }

            return kImageITK.GetPointer();
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("InsightToolkitSupport: out of memory");

            return null;
        }
    }

    /**
     * Create a 3D ITK single channel image with properties and values from the specified single channel model image.
     * The pixels values have already been extracted into a linear array. The properties from the input model image that
     * are used include the image size and spacing between samples.
     *
     * @param   kModelImage  ModelImage Contains image with properties and values to use.
     * @param   afValues     float[] Linear array of pixel values from the input model image. The array is ordered such
     *                       that the x coordinate increments the fastest and the z coordinate increments the slowest.
     *
     * @return  itkImageF3 New instance created with copied image properties and values.
     */
    private static itkImageF3 itkCreateImage3D(ModelImage kModelImage, float[] afValues) {

        try {

            // Create instance of ITK 3D image of floating point values
            itkImageF3_Pointer kImageITK = itkImageF3.itkImageF3_New();
            int iSizeX = kModelImage.getExtents()[0];
            int iSizeY = kModelImage.getExtents()[1];
            int iSizeZ = kModelImage.getExtents()[2];

            // Set the ITK image size
            itkImageRegion3 kImageRegion = new itkImageRegion3();
            itkIndex3 kImageStart = new itkIndex3();
            kImageStart.Fill(0);
            kImageRegion.SetIndex(kImageStart);

            itkSize3 kImageSize = new itkSize3();
            kImageSize.SetElement(0, iSizeX);
            kImageSize.SetElement(1, iSizeY);
            kImageSize.SetElement(2, iSizeZ);
            kImageRegion.SetSize(kImageSize);
            kImageITK.SetRegions(kImageRegion);

            // Set ITK image spacing
            float[] afResolutions = kModelImage.getFileInfo(0).getResolutions();
            SWIGTYPE_p_float afSwigResolutions = SwigExtras.new_FArray(3);
            SwigExtras.FArray_setitem(afSwigResolutions, 0, afResolutions[0]);
            SwigExtras.FArray_setitem(afSwigResolutions, 1, afResolutions[1]);
            SwigExtras.FArray_setitem(afSwigResolutions, 2, afResolutions[2]);
            kImageITK.SetSpacing(afSwigResolutions);
            SwigExtras.delete_FArray(afSwigResolutions);

            // Allocate the memory for the ITK image
            kImageITK.Allocate();

            // Copy pixel values from the ModelImage to the ITK image.
            itkIndex3 iIndexITK = new itkIndex3();
            int iIndex = 0;

            for (int iZ = 0; iZ < iSizeZ; iZ++) {
                iIndexITK.SetElement(2, iZ);

                for (int iY = 0; iY < iSizeY; iY++) {
                    iIndexITK.SetElement(1, iY);

                    for (int iX = 0; iX < iSizeX; iX++) {
                        iIndexITK.SetElement(0, iX);

                        kImageITK.SetPixel(iIndexITK, afValues[iIndex++]);
                    }
                }
            }

            return kImageITK.GetPointer();
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("InsightToolkitSupport: out of memory");

            return null;
        }
    }

    /**
     * Transfers the pixels values from either of two 2D ITK single channel images into either a 2D model image or the
     * specified slice of a 3D model image. Pixels are tranferred selected from either ITK image depending on the
     * specification of the mask image. The dimensions of the two ITK images, the model image, and the mask image (if
     * defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iStart       int Starting index into the model image linear array of values for the corresponding pixel
     *                      at the start of the ITK image.
     * @param  iStride      int Increment to advance to the next pixel for the same channel in the model image. The
     *                      integral-truncated result of the starting index (iStart) divided by this increment is the
     *                      starting index into the mask image (if defined) for the corresponding pixel at the start of
     *                      the ITK image.
     */
    private static void itkTransferImage2D(itkImageF2 kImageITK0, itkImageF2 kImageITK1, BitSet kMask,
                                           ModelImage kModelImage, int iStart, int iStride) {

        // Note the dimensions of each axis.
        int iSizeX = kModelImage.getExtents()[0];
        int iSizeY = kModelImage.getExtents()[1];

        // Copy pixel values from the ModelImage to the ITK image.
        itkIndex2 iIndexITK = new itkIndex2();
        int iIndex = iStart;
        int iIndexMask = iStart / iStride;

        for (int iY = 0; iY < iSizeY; iY++) {
            iIndexITK.SetElement(1, iY);

            for (int iX = 0; iX < iSizeX; iX++) {
                iIndexITK.SetElement(0, iX);

                if ((null == kMask) || kMask.get(iIndexMask)) {
                    kModelImage.set(iIndex, kImageITK1.GetPixel(iIndexITK));
                } else {
                    kModelImage.set(iIndex, kImageITK0.GetPixel(iIndexITK));
                }

                iIndex += iStride;
                iIndexMask++;
            }
        }
    }

    /**
     * Transfers the pixels values from either of two 2D ITK single channel images into either a 2D model image or the
     * specified slice of a 3D model image. Pixels are tranferred selected from either ITK image depending on the
     * specification of the mask image. The dimensions of the two ITK images, the model image, and the mask image (if
     * defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageUL2 2D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iStart       int Starting index into the model image linear array of values for the corresponding pixel
     *                      at the start of the ITK image.
     * @param  iStride      int Increment to advance to the next pixel for the same channel in the model image. The
     *                      integral-truncated result of the starting index (iStart) divided by this increment is the
     *                      starting index into the mask image (if defined) for the corresponding pixel at the start of
     *                      the ITK image.
     */
    private static void itkTransferImage2D(itkImageF2 kImageITK0, itkImageUL2 kImageITK1, BitSet kMask,
            ModelImage kModelImage, int iStart, int iStride) {

		// Note the dimensions of each axis.
		int iSizeX = kModelImage.getExtents()[0];
		int iSizeY = kModelImage.getExtents()[1];
		
		// Copy pixel values from the ModelImage to the ITK image.
		itkIndex2 iIndexITK = new itkIndex2();
		int iIndex = iStart;
		int iIndexMask = iStart / iStride;
		
		for (int iY = 0; iY < iSizeY; iY++) {
			iIndexITK.SetElement(1, iY);
			
			for (int iX = 0; iX < iSizeX; iX++) {
				iIndexITK.SetElement(0, iX);
				
				if ((null == kMask) || kMask.get(iIndexMask)) {
				kModelImage.set(iIndex, kImageITK1.GetPixel(iIndexITK));
				} else {
				kModelImage.set(iIndex, kImageITK0.GetPixel(iIndexITK));
				}
				
				iIndex += iStride;
				iIndexMask++;
			}
		}
}    
    
    /**
     * Transfers the pixels values from either of two 3D ITK single channel images into the specified 3D model image.
     * Pixels are tranferred selected from either ITK image depending on the specification of the mask image. The
     * dimensions of the two ITK images, the model image, and the mask image (if defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageF3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iStart       int Starting index into the model image linear array of values for the corresponding pixel
     *                      at the start of the ITK image.
     * @param  iStride      int Increment to advance to the next pixel for the same channel in the model image. The
     *                      integral-truncated result of the starting index (iStart) divided by this increment is the
     *                      starting index into the mask image (if defined) for the corresponding pixel at the start of
     *                      the ITK image.
     */
    private static void itkTransferImage3D(itkImageF3 kImageITK0, itkImageF3 kImageITK1, BitSet kMask,
                                           ModelImage kModelImage, int iStart, int iStride) {

        // Note the dimensions of each axis.
        int iSizeX = kModelImage.getExtents()[0];
        int iSizeY = kModelImage.getExtents()[1];
        int iSizeZ = kModelImage.getExtents()[2];

        // Copy pixel values from the ModelImage to the ITK image.
        itkIndex3 iIndexITK = new itkIndex3();
        int iIndex = iStart;
        int iIndexMask = iStart / iStride;

        for (int iZ = 0; iZ < iSizeZ; iZ++) {
            iIndexITK.SetElement(2, iZ);

            for (int iY = 0; iY < iSizeY; iY++) {
                iIndexITK.SetElement(1, iY);

                for (int iX = 0; iX < iSizeX; iX++) {
                    iIndexITK.SetElement(0, iX);

                    if ((null == kMask) || kMask.get(iIndexMask)) {
                        kModelImage.set(iIndex, kImageITK1.GetPixel(iIndexITK));
                    } else {
                        kModelImage.set(iIndex, kImageITK0.GetPixel(iIndexITK));
                    }

                    iIndex += iStride;
                    iIndexMask++;
                }
            }
        }
    }

    
    /**
     * Transfers the pixels values from either of two 3D ITK single channel images into the specified 3D model image.
     * Pixels are tranferred selected from either ITK image depending on the specification of the mask image. The
     * dimensions of the two ITK images, the model image, and the mask image (if defined) must be compatible.
     *
     * @param  kImageITK0   itkImageF3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask image if that mask value for the corresponding pixel is not set. This image is ignored
     *                      if the input mask image is not defined.
     * @param  kImageITK1   itkImageUL3 3D ITK single channel image with values to copy for pixels defined in the input
     *                      mask if that mask value for the corresponding pixel is set. Pixels are tranferred from this
     *                      image only if the input mask image is not defined.
     * @param  kMask        BitSet Boolean image which contains set values indicating pixels to be transferred from
     *                      input kImageITK1; otherwise unset values indicate pixels to be transferred from input
     *                      kImageITK0. This image does not have to be defined (i.e., set to null) which means all
     *                      pixels are tranferred from input kImageITK1.
     * @param  kModelImage  ModelImage Image into which values will be transferred.
     * @param  iStart       int Starting index into the model image linear array of values for the corresponding pixel
     *                      at the start of the ITK image.
     * @param  iStride      int Increment to advance to the next pixel for the same channel in the model image. The
     *                      integral-truncated result of the starting index (iStart) divided by this increment is the
     *                      starting index into the mask image (if defined) for the corresponding pixel at the start of
     *                      the ITK image.
     */
    private static void itkTransferImage3D(itkImageF3 kImageITK0, itkImageUL3 kImageITK1, BitSet kMask,
                                           ModelImage kModelImage, int iStart, int iStride) {

        // Note the dimensions of each axis.
        int iSizeX = kModelImage.getExtents()[0];
        int iSizeY = kModelImage.getExtents()[1];
        int iSizeZ = kModelImage.getExtents()[2];

        // Copy pixel values from the ModelImage to the ITK image.
        itkIndex3 iIndexITK = new itkIndex3();
        int iIndex = iStart;
        int iIndexMask = iStart / iStride;

        for (int iZ = 0; iZ < iSizeZ; iZ++) {
            iIndexITK.SetElement(2, iZ);

            for (int iY = 0; iY < iSizeY; iY++) {
                iIndexITK.SetElement(1, iY);

                for (int iX = 0; iX < iSizeX; iX++) {
                    iIndexITK.SetElement(0, iX);

                    if ((null == kMask) || kMask.get(iIndexMask)) {
                        kModelImage.set(iIndex, kImageITK1.GetPixel(iIndexITK));
                    } else {
                        kModelImage.set(iIndex, kImageITK0.GetPixel(iIndexITK));
                    }

                    iIndex += iStride;
                    iIndexMask++;
                }
            }
        }
    }
    
    
}
