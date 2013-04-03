package gov.nih.mipav.model.algorithms.utilities;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.util.MipavCoordinateSystems;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;

import java.io.*;
import java.util.*;

/**
 * 
 * @author ilb
 *
 */
    
    
public class AlgorithmTiltCorrection extends AlgorithmBase {
    
    private boolean nonHelical;
    
    private double tiltAngle;
    
    public AlgorithmTiltCorrection(ModelImage destImg, ModelImage srcImg, boolean nonHelical, double tiltAngle) {
        super(destImg, srcImg);
        this.nonHelical = nonHelical;
        this.tiltAngle = tiltAngle;
    }
    
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        if (nonHelical) {
            nonHelicalCorrection();
        }
        else {
            helicalCorrection();
        }
    }
    
    private void nonHelicalCorrection() {
        int factor = 1;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int length;
        double buffer[];
        int z;
        ModelImage sliceImage;
        int sliceExtents[] = new int[2];
        float firstLocation;
        Object value;
        String s;
        boolean useDICOM = false;
        float sliceLocation;
        float xRes = srcImage.getFileInfo()[0].getResolutions()[0];
        float yRes = srcImage.getFileInfo()[0].getResolutions()[1];
        float zRes = srcImage.getFileInfo()[0].getResolutions()[2];
        double tiltRadians;
        double tiltTangent;
        TransMatrix xfrm;
        double Tx = 0.0;
        double Ty = 0.0;
        AlgorithmTransform algoTrans;
        int interp = AlgorithmTransform.BILINEAR;
        int units[] = new int[2];
        boolean doVOI = false;
        boolean doClip = true;
        boolean doPad = false;
        boolean doRotateCenter = true;
        Vector3f center = null;
        float fillValue = (float)srcImage.getMin();
        boolean doUpdateOrigin = true;
        ModelImage resultImage;
        ModelImage finalImage;
        
        fireProgressStateChanged(srcImage.getImageName(), "Tilt correcting image ...");
        
        if (destImage != null) {
            finalImage = destImage;
        }
        else {
            finalImage = srcImage;
        }
        
        if (srcImage.isColorImage()) {
            factor = 4;
        } else if (srcImage.isComplexImage()) {
            factor = 2;
        }
        
        units[0] = srcImage.getFileInfo()[0].getUnitsOfMeasure()[0];
        units[1] = srcImage.getFileInfo()[0].getUnitsOfMeasure()[1];
        
        length = factor * sliceSize;
        
        try {
            buffer = new double[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Tilt Correction: Out of memory", true);

            return;
        }
        
        sliceExtents[0] = xDim;
        sliceExtents[1] = yDim;
        sliceImage = new ModelImage(srcImage.getDataType(), sliceExtents, srcImage.getImageName() + "_slice2D");
        sliceImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        center = sliceImage.getImageCentermm(false);
        
        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
            useDICOM = true;
            for (z = 0; z < zDim; z++) {
                if (((FileInfoDicom) (srcImage.getFileInfo(z))).getTagTable().getValue("0020,1041") != null) {
                    value = ((FileInfoDicom) (srcImage.getFileInfo(z))).getTagTable().getValue("0020,1041");
                    s = ((String) value).trim();
                    
                    try {
                        sliceLocation = Float.valueOf(s).floatValue();
                    } catch (NumberFormatException e) {
                        useDICOM = false;
                    }
                } // if (((FileInfoDicom) (srcImage.getFileInfo(z))).getTagTable().getValue("0020,1041") != null)
                else {
                    useDICOM = false;
                }
            } // for (z = 0; z < zDim; z++)
        } // if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM)
        
        if (useDICOM) {
            value = ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().getValue("0020,1041");
            s = ((String) value).trim();
            firstLocation = Float.valueOf(s).floatValue();
        }
        else {
            firstLocation = 0.0f;
        }
        
        tiltRadians = (Math.PI/180.0) * tiltAngle;
        tiltTangent = Math.tan(tiltRadians);
        
        xfrm = new TransMatrix(3);
        xfrm.identity();
        
        for (z = 0; z < zDim; z++) {
            
            fireProgressStateChanged((100 * z)/(zDim - 1));
            
            try {
                srcImage.exportData(z * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Tilt Correction: Image(s) locked", true);

                return;
            }
            
            // A y offset for every slice except the first
            if (z != 0) {
                try {
                    sliceImage.importData(0, buffer, true);
                } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Algorithm Tilt Correction: Image(s) locked", true);

                    return;
                }
                
                if (useDICOM) {
                    value = ((FileInfoDicom) (srcImage.getFileInfo(z))).getTagTable().getValue("0020,1041");
                    s = ((String) value).trim();
                    sliceLocation = Float.valueOf(s).floatValue();    
                }
                else {
                    sliceLocation = z * zRes;
                }
                
                Ty = tiltTangent * (sliceLocation - firstLocation);
                if (tiltAngle > 0.0) {
                    Ty = -Ty;
                }
                xfrm.setTranslate(Tx, Ty);
                algoTrans = new AlgorithmTransform(sliceImage, xfrm, interp, xRes, yRes, xDim, yDim, units, doVOI, doClip,
                        doPad, doRotateCenter, center);
                algoTrans.setFillValue(fillValue);
                algoTrans.setUpdateOriginFlag(doUpdateOrigin);
                algoTrans.run();
                resultImage = algoTrans.getTransformedImage();
                if (algoTrans != null) {
                    algoTrans.disposeLocal();
                    algoTrans = null;
                }
                
                try {
                    resultImage.exportData(0, length, buffer); // locks and releases lock
                } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Algorithm Tilt Correction: Image(s) locked", true);

                    return;
                }
                
                if (resultImage != null) {
                    resultImage.disposeLocal();
                    resultImage = null;
                }
            } // if (z != 0)
            
            try {
                finalImage.importData(z*length, buffer, false);
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Tilt Correction: Image(s) locked", true);

                return;
            }
            
            if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                // Set the gantry/detector tilt angle to "0.0"
                ((FileInfoDicom) (finalImage.getFileInfo(z))).getTagTable().setValue("0018,1120",
                    new String("0.0"), 3);
            }
            
        } // for (z = 0; z < zDim; z++)
        
        if (sliceImage != null) {
            sliceImage.disposeLocal();
            sliceImage = null;
        }
        
        finalImage.calcMinMax();
        setCompleted(true);
        return;
        
    }
    
    private void helicalCorrection() {
        
    }
}