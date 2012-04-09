package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 *
 * @author   William Gandler
 * @version  1.0
 * If all the values in a slice are the same, the slice is marked as a blank slice.  The
 * number of consecutive blank slices is counted.  If the consecutive blank slices are at
 * the beginning of the image, they are all replaced by the first nonblank slice.  If
 * th consecutive blank slices are at the end of the image, the are all replaced by the 
 * last nonblank slice.  Otherwise, the consecutive blank slices are replaced by different
 * weightings of the surrounding nonblank slices.  For 1 consecutive blank slice simply 
 * an average of the 2 surrounding slices.  For 2 consecutive blank slices the first 
 * blank slice is 2/3 * bottomNonBlank + 1/3 * topNonBlank and the second blank slice is
 * 1/3 * bottomNonBlank  + 2/3 * topNonBlank.
 */
public class AlgorithmReplaceBlankSlicesWithAverages extends AlgorithmBase {

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new AlgorithmReplaceSlice object.
     *
     * @param  srcImage    DOCUMENT ME!
     */
    public AlgorithmReplaceBlankSlicesWithAverages(ModelImage srcImage) {
        super(null, srcImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Actually runs the algorithm.
     *
     * @todo  Implement this gov.nih.mipav.model.algorithms.AlgorithmBase method
     */
    public void runAlgorithm() {

        int sliceSize = srcImage.getExtents()[0] * srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int z;
        float red, green, blue, gray;
        float real, imaginary;
        boolean blank;
        int i;
        int consecutiveBlank = 0;
        int zRep;
        float sliceData2[] = null;
        float sliceDataAvg[] = null;
        int sl;
        float firstPart;
        float secondPart;

        if (srcImage.isColorImage()) {
            sliceSize *= 4;
        }
        else if (srcImage.isComplexImage()) {
        	sliceSize *= 2;
        }

        float[] sliceData = new float[sliceSize];
        
        fireProgressStateChanged(srcImage.getImageName(), "Replacing blanks with averages...");
        fireProgressStateChanged(0);
        

        for (z = 0; z < zDim; z++) {
            fireProgressStateChanged(100 * z/zDim);
            try {
                srcImage.exportData(z*sliceSize, sliceSize, sliceData);
            } catch (Exception ex) {
                MipavUtil.displayError("Error exporting data from slice " + (z+1));
                setCompleted(false);
                return;
            }
            if (srcImage.isColorImage()) {
                red = sliceData[1];
                green = sliceData[2];
                blue = sliceData[3];
                blank = true;
                for (i = 4; i < sliceSize && blank; i+=4) {
                    if ((sliceData[i + 1] != red)  || (sliceData[i + 2] != green) ||
                        (sliceData[i + 3] != blue)) {
                        blank = false;
                    }
                }
            } // if (srcImage.isColorImage())
            else if (srcImage.isComplexImage()) {
                real = sliceData[0];
                imaginary = sliceData[1];
                blank = true;
                for (i = 2; i < sliceSize && blank; i += 2) {
                	if ((sliceData[i] != real) || (sliceData[i+1] != imaginary)) {
                		blank = false;
                	}
                }
            } // else if (srcImage.isComplexImage())
            else { // black and white
                gray = sliceData[0];
                blank = true;
                for (i = 1; i < sliceSize && blank; i++) {
                    if (sliceData[i] != gray) {
                        blank = false;
                    }
                }
            } // else black and white
            
            if (blank) {
                Preferences.debug("Slice " + (z+1) + " was blank\n", Preferences.DEBUG_ALGORITHM);
                consecutiveBlank++;
                if (z == (zDim - 1)) {
                    try {
                        srcImage.exportData((zDim-1-consecutiveBlank)*sliceSize, sliceSize, sliceData);
                    } catch (Exception ex) {
                        MipavUtil.displayError("Error exporting data from slice " +
                                               (zDim-consecutiveBlank));
                        
                        setCompleted(false);
                        return;
                    } 
                    
                    for (zRep = (zDim - consecutiveBlank); zRep <= zDim - 1; zRep++) {
                        try {
                            srcImage.importData((zRep * sliceSize), sliceData, false);
                        } catch (Exception ex) {
                            MipavUtil.displayError("Error replacing slice " + (zRep+1) + " in source image");
                            
                            setCompleted(false);
                            return;
                        }    
                    } // for (zRep = (zDim - consecutiveBlank); zRep <= zDim - 1; zRep++)
                } // if (z == (zDim - 1))
            } // if (blank)
            else { // not blank
                if (consecutiveBlank > 0) {
                    if ((z - consecutiveBlank) == 0) {
                        try {
                            srcImage.exportData(z*sliceSize, sliceSize, sliceData);
                        } catch (Exception ex) {
                            MipavUtil.displayError("Error exporting data from slice " + (z+1));
                            
                            setCompleted(false);
                            return;
                        }
                        for (zRep = 0; zRep < consecutiveBlank; zRep++) {
                            try {
                                srcImage.importData((zRep * sliceSize), sliceData, false);
                            } catch (Exception ex) {
                                MipavUtil.displayError("Error replacing slice " + (zRep+1) + " in source image");
                                
                                setCompleted(false);
                                return;
                            }        
                        } // for (zRep = 0; zRep < consecutiveBlank; zRep++)
                    } // if ((z - consecutiveBlank) == 0)
                    else { // z - consecutiveBlank > 0
                        if (sliceData2 == null) {
                            sliceData2 = new float[sliceSize];
                        }
                        if ((consecutiveBlank > 1) && (sliceDataAvg == null)) {
                            sliceDataAvg = new float[sliceSize];
                        }
                        try {
                            srcImage.exportData((z-consecutiveBlank-1)*sliceSize, sliceSize, sliceData);
                        } catch (Exception ex) {
                            MipavUtil.displayError("Error exporting data from slice " + (z-consecutiveBlank));
                            
                            setCompleted(false);
                            return;
                        }
                        try {
                            srcImage.exportData(z*sliceSize, sliceSize, sliceData2);
                        } catch (Exception ex) {
                            MipavUtil.displayError("Error exporting data from slice " + (z+1));
                            
                            setCompleted(false);
                            return;
                        }
                        if (consecutiveBlank == 1) {
                            for (i = 0; i < sliceSize; i++) {
                                sliceData[i] = (sliceData[i] + sliceData2[i])/2.0f;
                            }
                            try {
                                srcImage.importData((z-1)*sliceSize, sliceData, false);
                            }
                            catch (Exception ex) {
                                MipavUtil.displayError("Error replacing slice " + z + " in source image");
                                
                                setCompleted(false);
                                return;
                            }
                        } // if (consecutiveBlank == 1) 
                        else { // consecutiveBlank > 1
                            for (zRep = z - consecutiveBlank, sl = 1; zRep <= z - 1; zRep++, sl++) {
                                firstPart = ((float)(consecutiveBlank-sl+1))/(consecutiveBlank+1);
                                secondPart = ((float)sl)/(consecutiveBlank+1);
                                for (i = 0; i < sliceSize; i++) {
                                    sliceDataAvg[i] =  firstPart*sliceData[i] + secondPart*sliceData2[i]; 
                                }
                                try {
                                    srcImage.importData(zRep*sliceSize, sliceDataAvg, false);
                                }
                                catch (Exception ex) {
                                    MipavUtil.displayError("Error replacing slice " + (zRep+1) + " in source image");
                                    
                                    setCompleted(false);
                                    return;
                                }
                            } // for (zRep = z - consecutiveBlank; zRep <= z - 1; zRep++)
                        } // else consecutiveBlank > 1
                    } // else z - consecutiveBlank > 0
                    consecutiveBlank = 0;
                } // if (consecutiveBlank > 0)
            } // else not blank
        } // for (z = 0; z < zDim; z++)

        srcImage.calcMinMax();
        
        setCompleted(true);

        return;
    }

}
