import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * This shows how to extend the AlgorithmBase class.
 *
 * <p>Supports the segmentation CT scans: Fat: -190 to -30 Low density muscle: 0 to 30 High density muscle: 31 to 100 If
 * you have any questions, please drop me a line. ===== Matthew J. Delmonico, MS, MPH Graduate Research Assistant,
 * Exercise Physiology 2132 HHP Building University of Maryland College Park, MD 20742 (301) 405-2569 (301) 793-0567
 * (cell)</p>
 *
 * @version  July 12, 2002
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmCT_MD.java $ $Revision: 13 $ $Date: 11/16/05 5:11p $</p>
 */
public class PlugInAlgorithmCT_MD extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public int fatH = -30;

    /** DOCUMENT ME! */
    public int fatL = -190;

    /** DOCUMENT ME! */
    public int hdmH = 100;

    /** DOCUMENT ME! */
    public int hdmL = 31;

    /** DOCUMENT ME! */
    public int ldmH = 30;

    /** DOCUMENT ME! */
    public int ldmL = 0;
    
    /**The ending units */
    public String resultUnit;
    
    /**Whether to calculate slice volumes for a 3D image*/
    public boolean doVolume;
    
    /**Whether to calculate slice area for a 3D image*/
    public boolean doArea;


    /** DOCUMENT ME! */
    private boolean entireImage = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for 3D images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg  Image model where result image is to stored.
     * @param  srcImg   Source image model.
     */
    public PlugInAlgorithmCT_MD(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
        resultUnit = (Unit.getUnitFromLegacyNum(srcImage.getUnitsOfMeasure()[0])).toString();
        doVolume = true;
        doArea = false;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }


    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (destImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (destImage != null) { // if there exists a destination image

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDest3D();
            }
        }
    }

    /**
     * This function produces a new image that has been median filtered and places filtered image in the destination
     * image.
     */
    private void calcStoreInDest2D() {

        int length; // total number of data-elements (pixels) in image
        int resultUnitLoc;
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        String unitStr;

        try {

            // image length is length in 2 dims
            length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            resultUnitLoc = (Unit.getUnit(resultUnit)).getLegacyNum();
            unitStr = (Unit.getUnitFromLegacyNum(resultUnitLoc)).getAbbrev();
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            unitStr = "unknown";
            errorCleanUp("Algorithm CT_MD reports: source image locked", true);
            
            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            unitStr = "unknown";
            errorCleanUp("Algorithm CT_MD reports: out of memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length


        // Fat:  -190 to -30
        // Low density muscle:  0 to 30
        // High density muscle:  31 to 100
        BitSet mask = null;

        if (srcImage.getVOIs().size() > 0) {
            mask = srcImage.generateVOIMask();
            entireImage = false;
        }

        int fat = 0;
        int ldMuscle = 0;
        int hdMuscle = 0;

        for (int i = 0; (i < length) && !threadStopped; i++) {

            if ((((i) % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) (i) / (length - 1) * 100));
            }

            if ((entireImage == true) || mask.get(i)) {

                if ((buffer[i] >= fatL) && (buffer[i] <= fatH)) {
                    destImage.set(i, 20);
                    fat++;
                } else if ((buffer[i] >= ldmL) && (buffer[i] <= ldmH)) {
                    destImage.set(i, 40);
                    ldMuscle++;
                } else if ((buffer[i] >= hdmL) && (buffer[i] <= hdmH)) {
                    destImage.set(i, 60);
                    hdMuscle++;
                } else {
                    destImage.set(i, 0);
                    // buffer[i] = (float)srcImage.getMin();
                }
            }
        }

        // destImage.releaseLock();
        
        if (threadStopped) {
            finalize();

            return;
        }

        //perform any necessary conversion
        int res0Unit = srcImage.getUnitsOfMeasure(0);
        int res1Unit = srcImage.getUnitsOfMeasure(1);
        
        float xRes = (float) (srcImage.getResolutions(0)[0] * ModelImage.getConversionFactor(resultUnitLoc, res0Unit));
        float yRes = (float) (srcImage.getResolutions(0)[1] * ModelImage.getConversionFactor(resultUnitLoc, res1Unit));
        
        float area = xRes * yRes;

        ViewUserInterface.getReference().getMessageFrame().append("Number of Fat pixels = " + fat,
                                                                  ViewJFrameMessage.DATA);
        ViewUserInterface.getReference().getMessageFrame().append("  Area = " + (fat * area) + " "+unitStr+"^2\n",
                                                                  ViewJFrameMessage.DATA);

        ViewUserInterface.getReference().getMessageFrame().append("Number of LDM pixels = " + ldMuscle,
                                                                  ViewJFrameMessage.DATA);
        ViewUserInterface.getReference().getMessageFrame().append("  Area = " + (ldMuscle * area) + " "+unitStr+"^2\n",
                                                                  ViewJFrameMessage.DATA);

        ViewUserInterface.getReference().getMessageFrame().append("Number of HDM pixels = " + hdMuscle,
                                                                  ViewJFrameMessage.DATA);
        ViewUserInterface.getReference().getMessageFrame().append("  Area = " + (hdMuscle * area) + " "+unitStr+"^2\n",
                                                                  ViewJFrameMessage.DATA);

        destImage.calcMinMax();
        setCompleted(true);
    }

    /**
     * This function produces a new volume image that has been median filtered. Image can be filtered by filtering each
     * slice individually, or by filtering using a kernel-volume.
     */
    private void calcStoreInDest3D() {

        int totLength, imgLength;
        float[] buffer;
        
       //perform any necessary conversion
        int resultUnitLoc = (Unit.getUnit(resultUnit)).getLegacyNum();
        int res0Unit = srcImage.getUnitsOfMeasure(0);
        int res1Unit = srcImage.getUnitsOfMeasure(1);
        int res2Unit = srcImage.getUnitsOfMeasure(2);
        
        float xRes = (float) (srcImage.getResolutions(0)[0] * ModelImage.getConversionFactor(resultUnitLoc, res0Unit));
        float yRes = (float) (srcImage.getResolutions(0)[1] * ModelImage.getConversionFactor(resultUnitLoc, res1Unit));
        float zRes = (float) (srcImage.getResolutions(0)[2] * ModelImage.getConversionFactor(resultUnitLoc, res2Unit));
        
        float area = xRes * yRes;

        float vol = xRes * yRes * zRes;

        String unitStr = (Unit.getUnitFromLegacyNum(resultUnitLoc)).getAbbrev();
        
        try {

            // image totLength is totLength in 3 dims
            imgLength = srcImage.getSliceSize();
            totLength = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[totLength];
            srcImage.exportData(0, totLength, buffer); // locks and releases lock
            fireProgressStateChanged("Processing image ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm CT_MD: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm CT_MD: Out of memory creating process buffer", true);

            return;
        }

        int totFat = 0;
        int totLdMuscle = 0;
        int totHdMuscle = 0;


        for (int i = 0; (i < srcImage.getExtents()[2]) && !threadStopped; i++) {
            int fat = 0;
            int ldMuscle = 0;
            int hdMuscle = 0;

            fireProgressStateChanged(Math.round((float) (i) / (srcImage.getExtents()[2] - 1) * 100));

            for (int j = 0; (j < imgLength) && !threadStopped; j++) {

                // System.out.println(" j = " + j);
                int index = (i * imgLength) + j;

                if ((buffer[index] >= fatL) && (buffer[index] <= fatH)) {
                    destImage.set(index, 60);
                    totFat++;
                    fat++;
                } else if ((buffer[index] >= ldmL) && (buffer[index] <= ldmH)) {
                    destImage.set(index, 120);
                    totLdMuscle++;
                    ldMuscle++;
                } else if ((buffer[index] >= hdmL) && (buffer[index] <= hdmH)) {
                    destImage.set(index, 200);
                    totHdMuscle++;
                    hdMuscle++;
                } else {
                    destImage.set(index, 0);
                    // buffer[i] = -1024;
                }
            }

            ViewUserInterface.getReference().getMessageFrame().append("\n\n ***************** Slice " + i +
                                                                      " totals ***************\n",
                                                                      ViewJFrameMessage.DATA);
            ViewUserInterface.getReference().getMessageFrame().append("Number of fat pixels = " + fat,
                                                                      ViewJFrameMessage.DATA);
            if(doArea) {
	            ViewUserInterface.getReference().getMessageFrame().append("  Area = " + (fat * area) + " "+unitStr+"^2",
	                                                                      ViewJFrameMessage.DATA);
            }
            if(doVolume) {
            	ViewUserInterface.getReference().getMessageFrame().append("  Volume = " + (fat * vol) + " "+unitStr+"^3",
                        ViewJFrameMessage.DATA);
            }
            ViewUserInterface.getReference().getMessageFrame().append("\n", ViewJFrameMessage.DATA);
            
            
            ViewUserInterface.getReference().getMessageFrame().append("Number of LDM pixels = " + ldMuscle,
                                                                      ViewJFrameMessage.DATA);
            if(doArea) {
	            ViewUserInterface.getReference().getMessageFrame().append("  Area = " + (ldMuscle * area) + " "+unitStr+"^2",
	                                                                      ViewJFrameMessage.DATA);
            }
            if(doVolume) {
            	ViewUserInterface.getReference().getMessageFrame().append("  Volume = " + (ldMuscle * vol) + " "+unitStr+"^3",
                        ViewJFrameMessage.DATA);
            }
            ViewUserInterface.getReference().getMessageFrame().append("\n", ViewJFrameMessage.DATA);

            ViewUserInterface.getReference().getMessageFrame().append("Number of HDM pixels = " + hdMuscle,
                                                                      ViewJFrameMessage.DATA);
            if(doArea) {
	            ViewUserInterface.getReference().getMessageFrame().append("  Area = " + (hdMuscle * area) + " "+unitStr+"^2",
	                                                                      ViewJFrameMessage.DATA);
            }
            if(doVolume) {
            	ViewUserInterface.getReference().getMessageFrame().append("  Volume = " + (hdMuscle * vol) + " "+unitStr+"^3",
                        ViewJFrameMessage.DATA);
            }
            ViewUserInterface.getReference().getMessageFrame().append("\n", ViewJFrameMessage.DATA);
        }

        destImage.releaseLock();

        if (threadStopped) {
            finalize();

            return;
        }

        ViewUserInterface.getReference().getMessageFrame().append("\n ************************ Totals ********************\n",
                                                                  ViewJFrameMessage.DATA);
        ViewUserInterface.getReference().getMessageFrame().append("Number of totFat pixels = " + totFat,
                                                                  ViewJFrameMessage.DATA);
        ViewUserInterface.getReference().getMessageFrame().append("  Volume = " + (totFat * vol) + " "+unitStr+"^3\n",
                                                                  ViewJFrameMessage.DATA);

        ViewUserInterface.getReference().getMessageFrame().append("Number of LDM pixels = " + totLdMuscle,
                                                                  ViewJFrameMessage.DATA);
        ViewUserInterface.getReference().getMessageFrame().append("  Volume = " + (totLdMuscle * vol) + " "+unitStr+"^3\n",
                                                                  ViewJFrameMessage.DATA);

        ViewUserInterface.getReference().getMessageFrame().append("Number of HDM pixels = " + totHdMuscle,
                                                                  ViewJFrameMessage.DATA);
        ViewUserInterface.getReference().getMessageFrame().append("  Volume = " + (totHdMuscle * vol) + " "+unitStr+"^3\n",
                                                                  ViewJFrameMessage.DATA);

        destImage.calcMinMax();

        setCompleted(true);
    }
}
