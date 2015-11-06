package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
     This algorithm changes the saturation of a color image while keeping the hue and intensity constant.
     Intensity = (R + G + B)/3
     Hue = theta if B <= G
         = 360 - theta if B > G
     with
     theta = arccos{0.5[(R - G) + (R - B)]/sqrt[(R - G)**2 + (R - B)*(G - B)]}
     Saturation = 1 - 3 * min(R, G, B)/(R + G + B)
     Derived from quaternion formula:
     Radial dilation = ((a + 2)/2)*(r*i + g*j + b*k) + (a/2)*u*(ri + g*j + b*k)*u
     where u is a pure unit quaternion.
     For radial dilation around the gray line where r = g = b, set u = (i + j + k)/sqrt(3)
     Radial dilation around gray line = ((a + 2)/2)*(r*i + g*j + b*k)
                       + (a/2)*((i + j + k)/sqrt(3))*(r*i + g*j + b*k)*((i + j + k)/sqrt(3))
     which expands space outward for a > 0 or moves it out from the r = g = b line
     and compresses space for a < 0 or moves it in towards the r = g = b line.
     Using a = -1 sets r = g = b at every pixel.
     Using a = -2 complements the image in an opposing color sense.
     
     If need be, decrease the magnitude of a applied to a particular pixel so that the 3 equations
     can be satisfied for legal values of r, g, and b.
     References:
     1.) Multivariate Image Processing, Christopher Collet, Jocelyn Chanussot, and Kacem Chehdi, editors,
     Chapter 13, Hypercomplex Models and Processing of Vector Images by S. J. Sangwine, T. A. Ell, and
     N. Le Bihan, Year 2010, Publisher Wiley, pp. 407-436.
     2.) "Hypercomplex color affine filters" by T. A. Ell, IEEE International Conference on Image Processing
     (ICIP 2000), vol. II, Vancouver, Canada, Institute of Electrical and Electronics Engineers,
     pp. 792-795, September 16-19, 2007.
 */
public class AlgorithmColorSaturation extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    // Scale factor which increases saturation for a > 0 and decreases saturation for a < 0
	private float a;
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmColorSaturation - default constructor.
     */
    public AlgorithmColorSaturation() { }

    /**
     * AlgorithmColorSaturation.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  a        Scale factor which increases saturation for a > 0 and decreases saturation for a < 0
     */
    public AlgorithmColorSaturation(ModelImage destImg, ModelImage srcImg, float a) {
        super(destImg, srcImg);
        this.a = a;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        int xDim;
        int yDim;
        int zDim;
        int tDim;
        int z;
        int t;
        int tStart;
        int start;
        int sliceSize;
        int volSize;
        int index;

        float[] srcBuffer;
        float[] destBuffer;
        float imageMin;
        float imageMax;
        float rOld;
        float gOld;
        float bOld;
        float rNew;
        float gNew;
        float bNew;
        float aMod;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Changing color saturation ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;

        srcBuffer = new float[4 * sliceSize];
        destBuffer = new float[4 * sliceSize];
        imageMin = 0.0f;
        if (srcImage.getType()  == ModelStorageBase.ARGB) {
            imageMax = 255.0f;	
        }
        else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
        	imageMax = 65535.0f;
        }
        else {
        	imageMax = Float.MAX_VALUE;
        }
        zDim = 1;
        tDim = 1;
        if (srcImage.getNDims() > 2) {
        	tDim = srcImage.getExtents()[2];
        }
        if (srcImage.getNDims() > 3) {
        	zDim = srcImage.getExtents()[3];
        }
        volSize = sliceSize * zDim;
        
        for (t = 0; t < tDim; t++) {
            tStart = 4 * t * volSize;
            for (z = 0; z < zDim; z++) {
                start = tStart + 4 * z * sliceSize;	
            
		        try {
		            srcImage.exportData(start, 4 * sliceSize, srcBuffer);
		        } catch (IOException e) {
		            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
		
		            setCompleted(false);
		
		            return;
		        }
		        
		        for (index = 0; index < sliceSize; index++) {
		        	aMod = a;
	                rOld = srcBuffer[4 * index + 1];
	                gOld = srcBuffer[4 * index + 2];
	                bOld = srcBuffer[4 * index + 3];
	                
	                rNew = (float)((2.0 * aMod * rOld + 3.0 * rOld - aMod * gOld - aMod * bOld )/3.0);
	                if (rNew < imageMin) {
	                	rNew = imageMin;
	                	aMod = (float)(3.0*(imageMin - rOld)/(2.0 * rOld - gOld - bOld));
	                }
	                if (rNew > imageMax) {
	                	rNew = imageMax;
	                	aMod = (float)(3.0*(imageMax - rOld)/(2.0 * rOld - gOld - bOld));
	                }
	                
	                gNew = (float)((2.0 * aMod * gOld + 3.0 * gOld - aMod * rOld - aMod * bOld)/3.0);
	                if (gNew < imageMin) {
	                	gNew = imageMin;
	                	aMod = (float)(3.0*(imageMin - gOld)/(2.0 * gOld - rOld - bOld));
	                	rNew = (float)((2.0 * aMod * rOld + 3.0 * rOld - aMod * gOld - aMod * bOld )/3.0);
	                }
	                if (gNew > imageMax) {
	                	gNew = imageMax;
	                	aMod = (float)(3.0*(imageMax - gOld)/(2.0 * gOld - rOld - bOld));
	                	rNew = (float)((2.0 * aMod * rOld + 3.0 * rOld - aMod * gOld - aMod * bOld )/3.0);
	                }
	                
	                bNew = (float)((2.0 * aMod * bOld + 3.0 * bOld - aMod * rOld - aMod * gOld)/3.0);
	                if (bNew < imageMin) {
	                	bNew = imageMin;
	                	aMod = (float)(3.0*(imageMin - bOld)/(2.0 * bOld - rOld - gOld));
	                	rNew = (float)((2.0 * aMod * rOld + 3.0 * rOld - aMod * gOld - aMod * bOld )/3.0);
	                	gNew = (float)((2.0 * aMod * gOld + 3.0 * gOld - aMod * rOld - aMod * bOld)/3.0);
	                }
	                if (bNew > imageMax) {
	                	bNew = imageMax;
	                	aMod = (float)(3.0*(imageMax - bOld)/(2.0 * bOld - rOld - gOld));
	                	rNew = (float)((2.0 * aMod * rOld + 3.0 * rOld - aMod * gOld - aMod * bOld )/3.0);
	                	gNew = (float)((2.0 * aMod * gOld + 3.0 * gOld - aMod * rOld - aMod * bOld)/3.0);
	                }
	                
	                destBuffer[4 * index + 1] = rNew;
	                destBuffer[4 * index + 2] = gNew; 
	                destBuffer[4 * index + 3] = bNew;
	            } // for (index = 0; index < sliceSize; index++) 
		        try {
		            destImage.importData(start, destBuffer, false);
		        } catch (IOException e) {
		            MipavUtil.displayError("IOException " + e + " on destImage.importData");
		
		            setCompleted(false);
		
		            return;
		        }
            } // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)

        
        destImage.calcMinMax();
        setCompleted(true);

        return;
    }
}
