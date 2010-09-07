package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;
import Jama.*;

import java.io.*;


/**
 * Extracts slices from a mosaic in a 2D image
 * Followed DICOM orientation for mosaic from http://nipy.sourceforge.net/dipy/theory/dicom_mosaic.html and
 * http://nipy.sourceforge.net/dipy/theory/dicom_orientation.html#dicom-affines-reloaded.
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
        int numberOfImagesInMosaic;
        double xOrient[] = new double[3];
        double yOrient[] = new double[3];
        double Q[][] = new double[3][2];
        Matrix matQ;
        double rc[][] = new double[2][1];
        Matrix matRC;
        Matrix matDicom;
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
        numberOfImagesInMosaic = destImage.getExtents()[2];
        subLength = cFactor * subXDim * subYDim;
        subBuffer = new double[subLength];
        sliceNum = 0;
        for (y = 0; ((y + subYDim - 1) < yDim) && (sliceNum < numberOfImagesInMosaic); y += subYDim) {
        	for (x = 0; ((x + subXDim - 1) < xDim) && (sliceNum < numberOfImagesInMosaic); x += subXDim) {
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
            
            // DICOM (20,32) is incorrect for mosaics.  The value in this field gives where the
            // origin of an image the size of the mosaic would have been had such an image been
            // collected.  This puts the origin outside of the scanner.
            
            // Define a flipped version of 'ImageOrientationPatient (20,37)', F, that has flipped columns.
            // Thus if the vector of 6 values in 'ImageOrientationPatient', are i1,i2,i3,i4,i5,i6, then F =
            // [i4 i1]
            // [i5 i2]
            // [i6 i3]
            // Now the first column of F contains what the DICOM docs call the 'column(Y) direction cosine',
            // and second column contains the 'row(X) direction cosine.'  We prefer to think of these as 
            // (respectively) the row index direction cosine.
            
            // We can think of the affine A as the (3,3) component, RS, and a (3,1) translation vector t.
            // RS can in turn be thought of as the dot product of a (3,3) rotation matrix R and a scaling
            // matrix S, where S = diag(s) and s a is (3,) vector of voxel sizes.  t is a (3,1) translation
            // vector, defining the coordinate in millimeters of the first voxel in the voxel volume(the
            // voxel given by the voxel_array[0,0,0]).
            
            // In the case of the mosaic, we have the first two columns of R from the F - the left/right
            // flipped version of the ImageOrientationPatient DICOM field described in DICOM affines again.
            // To make a full rotation matrix, we can generate the last column from the cross product of the
            // first two.  However, Siemens defines, in its private CSA header, a SliceNormalVector which gives
            // the third column, but possibly with a z flip, so that R is orthogonal, but not a rotation 
            // matrix (it has a determinant of < 0).
            
            // The first two values of s(s1,s2) are given by the PixelSpacing field.  We get s3(the slice
            // scaling value) from SpacingBetweenSlices.
            
            // The SPM DICOM conversion code has a comment saying that mosaic DICOM images have an incorrect
            // ImagePositionPatient field.  The ImagePositionPatient field gives the t vector.  The comments
            // imply that Siemens has derived ImagePositionPatient from the (correct) position of the center
            // of the first slice (once the mosaic has been unpacked), but has then adjusted the vector to 
            // point to the top left voxel, where the slice size used for this adjustment is the size of the
            // mosaic, before it has been unpacked.  Let's call the correct position in millimeters of the
            // center of the first slice c = [cx,cy,cz].  We have derived the RS matrix from the calculations
            // above.  The unpacked (eventual, real) slice dimensions are (rdrows, rdcols) and the mosaic
            // dimensions are (mdrows,mdcols).  The ImagePositionPatient  vector i resulted from:
            
            //           [-(mdrows - 1)/2] 
            // i = c + RS[-(mdcols - 1)/2]
            //           [       0       ]
            
            // To correct the faulty translation, we reverse it, and add the correct translation for the unpacked
            // slice size (rdrows, rdcols), giving the true image position t:
            
            //             [-(mdrows - 1)/2]       [-(rdrows - 1)/2]
            // t = i - (RS [-(mdcols - 1)/2]) + (RS[-(rdcols - 1)/2])
            //             [       0       ]       [       0       ]
            
            // Because of the final zero in the voxel translations, this simplifies to:
            
            
            // t = i + Q[(mdrows - rdrows)/2]
            //          [(mdcols - rdcols)/2]
            
            // where:
            
            //      [r11*s1 r12*s2]
            //  Q = [r21*s1 r22*s2]
            //      [r31*s1 r32*s2]
            
            if (tagTable.getValue("0020,0037") != null) {
            	String orientation = (String) tagTable.getValue("0020,0037");
                if (orientation != null) {

	                int index1, index2, index3, index4, index5;
	                int notSet = -1;
	                index1 = index2 = index3 = index4 = notSet = index5 = notSet;
	
	                for (i = 0; i < orientation.length(); i++) {
	
	                    if (orientation.charAt(i) == '\\') {
	
	                        if (index1 == notSet) {
	                            index1 = i;
	                        } else if (index2 == notSet) {
	                            index2 = i;
	                        } else if (index3 == notSet) {
	                            index3 = i;
	                        } else if (index4 == notSet) {
	                            index4 = i;
	                        } else {
	                            index5 = i;
	                        }
	                    }
	                }
	
	                xOrient[0] = Double.valueOf(orientation.substring(0, index1)).doubleValue();
	                xOrient[1] = Double.valueOf(orientation.substring(index1 + 1, index2)).doubleValue();
	                xOrient[2] = Double.valueOf(orientation.substring(index2 + 1, index3)).doubleValue();
	                yOrient[0] = Double.valueOf(orientation.substring(index3 + 1, index4)).doubleValue();
	                yOrient[1] = Double.valueOf(orientation.substring(index4 + 1, index5)).doubleValue();
	                yOrient[2] = Double.valueOf(orientation.substring(index5 + 1)).doubleValue();
	                Q[0][0] = yOrient[0] * srcImage.getFileInfo(0).getResolution(0);
	                Q[1][0] = yOrient[1] * srcImage.getFileInfo(0).getResolution(0);
	                Q[2][0] = yOrient[2] * srcImage.getFileInfo(0).getResolution(0);
	                Q[0][1] = xOrient[0] * srcImage.getFileInfo(0).getResolution(1);
	                Q[1][1] = xOrient[1] * srcImage.getFileInfo(0).getResolution(1);
	                Q[2][1] = xOrient[2] * srcImage.getFileInfo(0).getResolution(1);
	                matQ = new Matrix(Q);
	                rc[0][0] = (yDim - subYDim)/2.0;
	                rc[1][0] = (xDim - subXDim)/2.0;
	                matRC = new Matrix(rc);
	                matDicom = matQ.times(matRC);
	                dicomOrigin[0] = imageOrg[0] + matDicom.get(0, 0);
	                dicomOrigin[1] = imageOrg[1] + matDicom.get(1, 0);
	                dicomOrigin[2] = imageOrg[2] + matDicom.get(2, 0);
                } // if (orientation != null) 
            } // if (tagTable.getValue("0020,0037") != null)
            else {
	            dicomOrigin[0] = imageOrg[0]/(xDim/subXDim);
	            dicomOrigin[1] = imageOrg[1]/(yDim/subYDim);
	            if (imageOrg.length >= 3) {
	                dicomOrigin[2] = imageOrg[2];
	            }
            } // else

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
                for (int k = 0; k < 3; k++) {
                    fileInfoDicom[i].setOrigin((float)dicomOrigin[k],k);
                }
                
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
