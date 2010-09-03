package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Extracts slices from a mosaic in a 2D image
 *
 * @version  1.0 September 2, 2010
 * @author   William Gandler
 */
public class AlgorithmMosaicToSlices extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Source image */
    private ModelImage srcImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmMosaicToSlices object.
     *
     * @param  srcIm  source image model
     * @param  dest    destination image
     */
    public AlgorithmMosaicToSlices(ModelImage srcIm, ModelImage dest) {
        super(dest, srcIm);
        srcImage = srcIm; 
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;
        super.finalize();
    }

    /**
     * Accessor that returns the result image.
     *
     * @return  Result image.
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int subXDim;
    	int subYDim;
    	int cFactor = 1;
    	double buffer[] = null;
    	double subBuffer[] = null;
    	int length;
    	int subLength;
    	int sliceNum;
    	int x;
    	int y;
    	int xs;
    	int ys;
    	int index;
    	int subIndex;
    	int c;
    	FileInfoDicom[] fileInfoDicom;
    	int i;
    	double slLoc;
        int RLIndex;
        int APIndex;
        int ISIndex;
        boolean increaseRes;
        double sliceResolution = 1.0;
        float resolutions[] = new float[3];
        FileInfoBase[] fileInfo;
        if (srcImage == null) {
        	displayError("Source Image is null");
            setCompleted(false);

            return;
        }
        
        if (destImage == null) {
        	displayError("Destination Image is null");
            setCompleted(false);

            return;
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        
        if (srcImage.isComplexImage()) {
        	cFactor = 2;
        }
        else if (srcImage.isColorImage()) {
        	cFactor = 4;
        }
        
        length = cFactor * xDim * yDim;
        buffer = new double[length];
        
        try {
        	srcImage.exportData(0, length, buffer);
        }
        catch (IOException error) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("AlgorithmMosaicToSlices. srcImage locked", true);

            return;
        } 
        
        subXDim = destImage.getExtents()[0];
        subYDim = destImage.getExtents()[1];
        subLength = cFactor * subXDim * subYDim;
        subBuffer = new double[subLength];
        sliceNum = 0;
        for (y = 0; (y + subYDim - 1) < yDim; y += subYDim) {
        	for (x = 0; (x + subXDim - 1) < xDim; x += subXDim) {
        	    for (ys = 0; ys < subYDim; ys++) {
        	    	for (xs = 0; xs <  subXDim; xs++) {
        	    	    for (c = 0; c < cFactor; c++) {
        	    	    	subIndex = c + cFactor*(xs + ys*subXDim);
        	    	    	index = c + cFactor*(x + xs + (y + ys)*xDim);
        	    	    	subBuffer[subIndex] = buffer[index];
        	    	    } // for (c = 0; c < cFactor; c++)
        	    	} // for (xs = 0; xs <  subXDim; xs++)
        	    } // for (ys = 0; ys < subYDim; ys++)
        	    try {
        	    	destImage.importData(sliceNum*subLength, subBuffer, false);
        	    }
        	    catch (IOException error) {
                    buffer = null;
                    destImage.disposeLocal(); // Clean up memory of result image
                    destImage = null;
                    errorCleanUp("AlgorithmMosaicToSlices destImage locked", true);

                    return;
                }
        	    sliceNum++;
        	}
        }
        destImage.calcMinMax();
        if (srcImage.getFileInfo()[0] instanceof FileInfoDicom) {
        	FileInfoDicom dicomInfo = (FileInfoDicom) srcImage.getFileInfo(0);
        	FileDicomTagTable tagTable = dicomInfo.getTagTable();
        	if (tagTable.getValue("0018,0088") != null) {
        		String sliceGapString = ((String) (dicomInfo.getTagTable().getValue("0018,0088"))).trim();
                sliceResolution = new Double(sliceGapString.trim()).doubleValue();
        	}
        	resolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
        	resolutions[1] = srcImage.getFileInfo(0).getResolutions()[1];
        	resolutions[2] = (float)sliceResolution;
            fileInfoDicom = new FileInfoDicom[destImage.getExtents()[2]];
            final float[] imageOrg = srcImage.getFileInfo(0).getOrigin();
            final double dicomOrigin[] = new double[imageOrg.length];

            for (int k = 0; k < imageOrg.length; k++) {
                dicomOrigin[k] = imageOrg[k];
            }
            TransMatrix matrix = dicomInfo.getPatientOrientation();
            if (matrix != null) {
                final TransMatrix transposeMatrix = new TransMatrix(4);
            	for (i = 0; i < 4; i++) {
            		for (int j = 0; j < 4; j ++) {
            			transposeMatrix.set(i, j, matrix.get(j, i));
            		}
            	}
            	matrix = null;
            	matrix = transposeMatrix;
            }
            else {
            	matrix = srcImage.getMatrix();
            }
            RLIndex = 0;
            APIndex = 1;
            ISIndex = 2;
            increaseRes = true;
            for (i = 0; i <= 2; i++) {
            	if (srcImage.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_R2L_TYPE) {
            		RLIndex = i;
                } else if (srcImage.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_L2R_TYPE) {
            		RLIndex = i;
            		if (i == 2) {
            		    increaseRes = false;
            		}
                } else if (srcImage.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_A2P_TYPE) {
            		APIndex = i;
                } else if (srcImage.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_P2A_TYPE) {
            		APIndex = i;
            		if (i == 2) {
            			increaseRes = false;
            		}
                } else if (srcImage.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_I2S_TYPE) {
            		ISIndex = i;
                } else if (srcImage.getFileInfo()[0].getAxisOrientation()[i] == FileInfoBase.ORI_S2I_TYPE) {
            		ISIndex = i;
            		if (i == 2) {
            			increaseRes = false;
            		}
            	}
            }
      
            slLoc = dicomOrigin[2];

            for (i = 0; (i < destImage.getExtents()[2]) && !threadStopped; i++) {
                fileInfoDicom[i] = (FileInfoDicom) (dicomInfo.clone());
                fileInfoDicom[i].getTagTable().setValue("0028,0011", new Short((short) subXDim), 2); // columns
                fileInfoDicom[i].getTagTable().setValue("0028,0010", new Short((short) subYDim), 2); // rows
                fileInfoDicom[i].setExtents(destImage.getExtents());
                fileInfoDicom[i].setResolutions(resolutions);
                fileInfoDicom[i].getTagTable().setValue("0020,0013", Short.toString((short) (i + 1)),
                		                                 Short.toString((short) (i + 1)).length()); // instance number
                
                // Add code to modify the slice location attribute (0020, 1041) VR = DS = decimal string
                fileInfoDicom[i].getTagTable().setValue("0020,1041", Double.toString(slLoc),
                                  Double.toString(slLoc).length());
                if (increaseRes) {
                	slLoc += sliceResolution;
                } else {
                	slLoc -= sliceResolution;
                }
                
                final String tmpStr = new String(Float.toString((float) dicomOrigin[RLIndex]) + "\\"
                        + Float.toString((float) dicomOrigin[APIndex]) + "\\" 
                        + Float.toString((float) dicomOrigin[ISIndex]));

                fileInfoDicom[i].getTagTable().setValue("0020,0032", tmpStr, tmpStr.length());
                
                dicomOrigin[RLIndex] += matrix.get(0, 2)*sliceResolution;
                dicomOrigin[APIndex] += matrix.get(1, 2)*sliceResolution;
                dicomOrigin[ISIndex] += matrix.get(2, 2)*sliceResolution;

            }

            destImage.setFileInfo(fileInfoDicom);
            fileInfoDicom = null;
        } // if (srcImage.getFileInfo()[0] instanceof FileInfoDicom)
        else {
            fileInfo = destImage.getFileInfo();
            resolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
            resolutions[1] = srcImage.getFileInfo(0).getResolutions()[1];
            resolutions[2] = 1.0f;

            for (i = 0; (i < (destImage.getExtents()[2]) && !threadStopped); i++) {
                fileInfo[i].setModality(srcImage.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setEndianess(srcImage.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setExtents(destImage.getExtents());
                fileInfo[i].setMax(destImage.getMax());
                fileInfo[i].setMin(destImage.getMin());
                fileInfo[i].setImageOrientation(srcImage.getImageOrientation());
                fileInfo[i].setPixelPadValue(srcImage.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(srcImage.getFileInfo()[0].getPhotometric());
                fileInfo[i].setAxisOrientation(srcImage.getAxisOrientation());
                fileInfo[i].setOrigin(srcImage.getOrigin());
            }
            fileInfo = null;
        }
        setCompleted(true);
        
    }
    
}
