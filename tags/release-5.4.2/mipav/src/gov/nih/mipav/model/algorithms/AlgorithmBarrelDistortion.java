package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Corrects barrel and/or pin cushion distortion for 2D images.  
 * rsrc = a * rdest**4 + b * rdest**3 + c * rdest**2 + d * rdest
 * rsrc and rdest are specified in units of the min((xDim-1)/2, (yDim-1)/2)
 * 
 * Reference 1.) Correcting Barrel Distortion by Helmut Dersch at
 * http://www.all-in-one.ee/~dersch/barrel/barrel.html
 * Quoting from this reference:
 * How to determine suitable Parameters:

   The correcting function is a third order polynomial. It relates the distance of a pixel from the center of the source image
   (rsrc) to the corresponding distance in the corrected image (rdest) :
   rsrc = ( a * rdest3 + b * rdest2 + c * rdest + d ) * rdest
   The parameter d describes the linear scaling of the image. Using d=1, and a=b=c=0 leaves the image as it is. 
   Choosing other d-values scales the image by that amount. a,b and c distort the image. Using negative values shifts distant
   points away from the center. This counteracts barrel distortion, and is the basis for the above corrections. Using positive
   values shifts distant points towards the center. This counteracts pincussion distortions. Correcting using 'a' affects only
   the outermost pixels of the image,  while 'b' correction is more uniform. Finally, you may correct pincussion and barrel
   distortions in the same image: If the outer regions exhibit barrel distortions, and the inner parts pincussion, you should
   use negative 'a' and positive 'b' values. If you do not want to scale the image, you should set d so that a +b + c + d = 1.

   In most cases, you will get quite satisfactory results by using just one parameter, like the 'b'-parameter in the above examples.
   These examples may also serve as a guide to how large these values should be, ie around 0.1 if the distortion is quite visible,
   or around 0.01 if it is very small. Simply optimize this starting value until you like your image. 
   
   Reference 2.) Lens correction model by From PanoTools.org Wiki at
   http://wiki.panotools.org/Lens_correction_model
   Quoting from this reference:
   Usual values for a, b, and c are below 1.0, in most cases below 0.01.
   The a and c parameters control more complex forms of distortion.  In most cases it will be enough to optimize for the b
   parameter only, which is good at correcting normal barrel distortion and pincushion distortion.
 * 
 */
public class AlgorithmBarrelDistortion extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float a;

    /** DOCUMENT ME! */
    private float b;
    
    private float c;
    
    private float d;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmBarrelDistortion - default constructor.
     */
    public AlgorithmBarrelDistortion() { }

    /**
     * AlgorithmBarrelDistortion.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  a    
     * @param  b
     * @param  c
     * @param  d
     */
    public AlgorithmBarrelDistortion(ModelImage destImg, ModelImage srcImg, float a, float b, float c, float d) {
        super(destImg, srcImg);
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
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

        int sliceSize;

        int i, j;
        int index, index1;
        double ang;

        float[] srcBuffer;
        float[] destBuffer;
        double ySrc;
        double xSrc;
        float imageMin;
        int xBase;
        float delX;
        int yBase;
        float delY;
        int sIndex;
        int cf;
        double xcen;
        double ycen;
        double xoff;
        double yoff;
        double rdest;
        double rdest2;
        double rdest3;
        double rdest4;
        double rsrc;
        double normDist;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Correcting barrel distortion ...");

        if (srcImage.isColorImage()) {
            cf = 4;
        } else {
            cf = 1;
        }

        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        xcen = (xDim - 1.0)/2.0;
        ycen = (yDim - 1.0)/2.0;
        normDist = Math.min(xcen, ycen);

        srcBuffer = new float[cf * sliceSize];

        try {
            srcImage.exportData(0, cf * sliceSize, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }

        destBuffer = new float[cf * sliceSize];

        if (!srcImage.isColorImage()) {
            imageMin = (float) srcImage.getMin();

            for (i = 0; i < sliceSize; i++) {
                destBuffer[i] = imageMin;
            }
        } // if (!srcImage.isColorImage())

        

        for (j = 0; j < yDim; j++) {
            fireProgressStateChanged(100 * j / yDim);
            index1 = j * xDim;
            yoff = (j - ycen)/normDist;

            for (i = 0; i < xDim; i++) {
                xoff = (i - xcen)/normDist;
                rdest2 = xoff*xoff + yoff*yoff;
                rdest = Math.sqrt(rdest2);
                rdest3 = rdest2 * rdest;
                rdest4 = rdest2 * rdest2;
                rsrc = a * rdest4 + b * rdest3 + c * rdest2 + d * rdest;
                rsrc = normDist * rsrc;
                ang = Math.atan2(yoff,xoff);
                xSrc = xcen + (rsrc * Math.cos(ang));
                ySrc = ycen + (rsrc * Math.sin(ang));

                // Use bilinear interpolation to find the contributions from the
                // 4 nearest neighbors in the original space
                if ((xSrc >= 0.0) && ((xSrc) <= (xDim - 1)) && (ySrc >= 0.0) && (ySrc <= (yDim - 1))) {
                    xBase = (int) Math.floor(xSrc);
                    delX = (float) (xSrc - xBase);
                    yBase = (int) Math.floor(ySrc);
                    delY = (float) (ySrc - yBase);
                    index = index1 + i;
                    sIndex = (yBase * xDim) + xBase;

                    if (srcImage.isColorImage()) {
                        destBuffer[(4 * index) + 1] = (1 - delX) * (1 - delY) * srcBuffer[(4 * sIndex) + 1];
                        destBuffer[(4 * index) + 2] = (1 - delX) * (1 - delY) * srcBuffer[(4 * sIndex) + 2];
                        destBuffer[(4 * index) + 3] = (1 - delX) * (1 - delY) * srcBuffer[(4 * sIndex) + 3];

                        if (xSrc < (xDim - 1)) {
                            destBuffer[(4 * index) + 1] += delX * (1 - delY) * srcBuffer[(4 * sIndex) + 1];
                            destBuffer[(4 * index) + 2] += delX * (1 - delY) * srcBuffer[(4 * sIndex) + 2];
                            destBuffer[(4 * index) + 3] += delX * (1 - delY) * srcBuffer[(4 * sIndex) + 3];
                        }

                        if (ySrc < (yDim - 1)) {
                            destBuffer[(4 * index) + 1] += (1 - delX) * delY *
                                                               srcBuffer[(4 * (sIndex + xDim)) + 1];
                            destBuffer[(4 * index) + 2] += (1 - delX) * delY *
                                                               srcBuffer[(4 * (sIndex + xDim)) + 2];
                            destBuffer[(4 * index) + 3] += (1 - delX) * delY *
                                                               srcBuffer[(4 * (sIndex + xDim)) + 3];
                        }

                        if ((xSrc < (xDim - 1)) && (ySrc < (yDim - 1))) {
                            destBuffer[(4 * index) + 1] += delX * delY * srcBuffer[(4 * (sIndex + xDim + 1)) + 1];
                            destBuffer[(4 * index) + 2] += delX * delY * srcBuffer[(4 * (sIndex + xDim + 1)) + 2];
                            destBuffer[(4 * index) + 3] += delX * delY * srcBuffer[(4 * (sIndex + xDim + 1)) + 3];
                        }
                    } // if (srcImage.isColorImage())
                    else { // black and white image
                        destBuffer[index] = (1 - delX) * (1 - delY) * srcBuffer[sIndex];

                        if (xSrc < (xDim - 1)) {
                            destBuffer[index] += delX * (1 - delY) * srcBuffer[sIndex + 1];
                        }

                        if (ySrc < (yDim - 1)) {
                            destBuffer[index] += (1 - delX) * delY * srcBuffer[sIndex + xDim];
                        }

                        if ((xSrc < (xDim - 1)) && (ySrc < (yDim - 1))) {
                            destBuffer[index] += delX * delY * srcBuffer[sIndex + xDim + 1];
                        }
                    } // else black and white image
                }
            }
        } // for (j = 0; j < yDimDest; j++)

        try {
            destImage.importData(0, destBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData");

            setCompleted(false);

            return;
        }

        setCompleted(true);

        return;
    }
}
