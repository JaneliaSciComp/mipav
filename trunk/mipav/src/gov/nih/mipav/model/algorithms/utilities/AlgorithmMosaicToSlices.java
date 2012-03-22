package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import Jama.*;

import java.io.*;

/**
 * Algorithm requires input of 2D or 3D mosaics. Based on the dimension of the mosaic image, the 
 * algorithm converts 2D mosaics to 3D slices and 3D mosaics to 4D slices (note: slice numbers 
 * are specified by the user).
 *  
 *
 * @version  0.2 July 25, 2011
 * @author   Beth Tyrie
 * @version  0.1 September 2, 2010
 * @author   William Gandler
 * @see      
 */


public class AlgorithmMosaicToSlices extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Source image */
    private ModelImage srcImage;
    
    private DTIParameters orgImDTIparams, newImDTIparams;

    private FileInfoDicom srcDicomInfo;

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
        orgImDTIparams = srcImage.getDTIParameters();
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
        int zDim;
        int subXDim;
        int subYDim;
        int subZDim;
        int compSubZDim;
        int subTDim;
        int cFactor = 1;
        double buffer[] = null;
        double subBuffer[] = null;
        int length;
        int subLength;
        int sliceNum;
        int bufferSliceCount;
        int x;
        int y;
        int z;
        int xs;
        int ys;
        int zs;
        int orgIndex;
        int subIndex;
        int c;
        FileInfoDicom[] fileInfoDicom;
        int i;
        int t;
        double slLoc;
        int RLIndex;
        int APIndex;
        int ISIndex;
        boolean increaseRes;
        double sliceResolution = 1.0;
        float resolutions[];
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
            displayError("Original Image is null");
            setCompleted(false);

            return;
        }
        
        if (destImage == null) {
            displayError("sub Image is null");
            setCompleted(false);

            return;
        }
        
        if (srcImage.isComplexImage()) {
            cFactor = 2;
            System.err.println( "cFactor:" +cFactor );
        }
        else if (srcImage.isColorImage()) {
            cFactor = 4;
        }
        
    if (srcImage.is2DImage()) {
       resolutions= new float[3];
            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];
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
            if (orgImDTIparams != null){
                newImDTIparams = new DTIParameters(1); 
                newImDTIparams.setbValues(orgImDTIparams.getbValues());
                newImDTIparams.setGradients(orgImDTIparams.getGradients());
                newImDTIparams.setbMatrixVals(orgImDTIparams.getbMatrixVals());
                
            } 
            subLength = cFactor * subXDim * subYDim;
            subBuffer = new double[subLength];
            sliceNum = 0;
            
            for (y = 0; ((y + subYDim - 1) < yDim) && (sliceNum < numberOfImagesInMosaic); y += subYDim) {
                for (x = 0; ((x + subXDim - 1) < xDim) && (sliceNum < numberOfImagesInMosaic); x += subXDim) {
                    for (ys = 0; ys < subYDim; ys++) {
                        for (xs = 0; xs <  subXDim; xs++) {
                            for (c = 0; c < cFactor; c++) {
                                subIndex = c + cFactor*(xs + ys*subXDim);
                                orgIndex = c + cFactor*(x + xs + (y + ys)*xDim);
                                subBuffer[subIndex] = buffer[orgIndex];
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
                        errorCleanUp("AlgorithmMosaicToSlices. destImage locked", true);

                        return;
                    }
                    sliceNum++;
                }
            }   
            
     
    
    }
            else { 
                resolutions= new float[4];
                    xDim = srcImage.getExtents()[0];
                    yDim = srcImage.getExtents()[1];
                    zDim= srcImage.getExtents()[2];
                    
                    length = cFactor * xDim * yDim * zDim;
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
                    subZDim = destImage.getExtents()[2]; //User specified Z-Dim
                    compSubZDim = (yDim/(subYDim))*((xDim/subXDim)); //Sets for loop Z-Dim based on subX and subY dims  
                    subTDim = destImage.getExtents()[3];
                    if (orgImDTIparams != null){
                        newImDTIparams = new DTIParameters(subTDim); 
                        newImDTIparams.setbValues(orgImDTIparams.getbValues());
                        newImDTIparams.setGradients(orgImDTIparams.getGradients());
                        newImDTIparams.setbMatrixVals(orgImDTIparams.getbMatrixVals());
                        
                    }                    
                    subLength = cFactor * subXDim * subYDim * compSubZDim;
                    subBuffer = new double[subLength/compSubZDim]; //Creates buffer for each z-slice
                    sliceNum = 0;
                    zs = 0;// Keeps track of Z-Dim to be used in orgIndex equation
                    bufferSliceCount = -1;
                    for (z = 0; z < subTDim; z++) {  
                        sliceNum=0;
                        for (y = 0; ((y + subYDim - 1) < yDim) && (sliceNum < compSubZDim); y += subYDim) {
                            for (x = 0; ((x + subXDim - 1) < xDim) && (sliceNum < compSubZDim); x += subXDim) {
                                for (ys = 0; ys < subYDim; ys++) {
                                    for (xs = 0; xs <  subXDim; xs++) {
                                          for (c = 0; c < cFactor; c++) {
                                              subIndex = c + cFactor*(xs+ys*subXDim);
                                              orgIndex = (c + cFactor*(x + xs + (y + ys)*xDim))+(((subLength-1)*zs)+zs);
                                              subBuffer[subIndex] = buffer[orgIndex];       
                                        } // for (c = 0; c < cFactor; c++)
                                    }// for (xs = 0; xs <  subXDim; xs++)
                                }// for (ys = 0; ys < subYDim; ys++)
                                
                               if (sliceNum <subZDim){ 
                                   bufferSliceCount++;// Keeps track of user specified Z-Dim
                                try {
                                    destImage.importData((bufferSliceCount*(subLength/compSubZDim)), subBuffer, false);
                                    }
                                catch (IOException error) {
                                    buffer = null;
                                    destImage.disposeLocal(); // Clean up memory of result image
                                    destImage = null;
                                    errorCleanUp("AlgorithmMosaicToSlicess destImage locked", true);

                                    return;
                                     }
                               }
                               else{
                               }
                               sliceNum++;                               
                            }    
                        }  
                        zs++; 
                     
                    }
                      
             } 
     
                           
                  destImage.calcMinMax();
                  fileInfo = srcImage.getFileInfo();
                  
                  if (srcImage.getFileInfo()[0] instanceof FileInfoDicom) {
                      //4-D Destination dicom images
                      if (srcImage.is3DImage()) {
                          FileInfoBase destFileInfo[] = null;
                          int numInfos = destImage.getExtents()[3]*destImage.getExtents()[2];
                          FileInfoDicom oldDicomInfo = null;
                          int j;
                          destFileInfo = new FileInfoBase[numInfos];
                          int sliceCounter = 0; //Keeps track of every slice to populate tag


                     // Most efficient way of creating DICOM tags for 4-D. Uses pointers based on srcimage dicom tags    
                     for (t = 0; t < destImage.getExtents()[3]; t++) {
                         for (z = 0; z <destImage.getExtents()[2] ; z++) {
                             j = (t*destImage.getExtents()[2]) + z;
                             oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo(t);

                             if (z == 0) {
                                 destFileInfo[j] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                                 oldDicomInfo.getFileFormat());
                                 ((FileInfoDicom)destFileInfo[j]).setVr_type(oldDicomInfo.getVr_type());     
                             }
                             else {
                                 destFileInfo[j] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                                 oldDicomInfo.getFileFormat(), (FileInfoDicom) destFileInfo[t*(destImage.getExtents()[2])]);
                                 
                                 ((FileInfoDicom)destFileInfo[j]).setVr_type(oldDicomInfo.getVr_type()); 
          
                                
                             }
                              
                              FileDicomTagTable newTagTable = ((FileInfoDicom) destFileInfo[j]).getTagTable();
                              if (newTagTable.getValue("0018,0088") != null) {
                                  String sliceGapString = ((String) ((FileInfoDicom) destFileInfo[j]).getTagTable().getValue("0018,0088")).trim();
                                  sliceResolution = new Double(sliceGapString.trim()).doubleValue();
                              }                    
                                  fireProgressStateChanged((((100 * (t*2)))/(destImage.getExtents()[2]+1)));
                                  resolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
                                  resolutions[1] = srcImage.getFileInfo(0).getResolutions()[1];
                                  resolutions[2] = srcImage.getFileInfo(0).getResolutions()[2];
                                  resolutions[3] = (float)sliceResolution;
                                  //resolutions[4] = (float)sliceResolution;
                                  destFileInfo[sliceCounter].setResolutions(resolutions);
                                  destFileInfo[sliceCounter].setExtents(destImage.getExtents());
                                  destFileInfo[sliceCounter].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[0], 0);
                                  destFileInfo[sliceCounter].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[1], 1);
                                  destFileInfo[sliceCounter].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[2], 2);
                                  destFileInfo[sliceCounter].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation());                              
                                  ((FileInfoDicom) destFileInfo[j]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(t));
                                  ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0011", new Short((short) subXDim), 2); // columns
                                  ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0010", new Short((short) subYDim), 2); // rows                 
                                  ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0020,0013", Short.toString((short) (t + 1)),
                                                                           Short.toString((short) (t + 1)).length()); // instance number
                                  ((FileInfoDicom) destFileInfo[j]).getTagTable().removeTag("0019,100A");// Removes NumberofImages in Mosaic Tag
                                  sliceCounter++;  
                                                           
                         }                         
                         
                     }
                                          
                      destImage.setFileInfo(destFileInfo);
                  }
                      //3-D Destination dicom images
                      else{
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
                      
                      System.out.println("3D");
                      if (tagTable.getValue("0020,0037") != null) {
                          String orientation = (String) tagTable.getValue("0020,0037");
                          System.out.println("orientation: " +orientation);
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
                              System.out.println("xOrient[0]" +xOrient[0]);
                              System.out.println("xOrient[1]" +xOrient[1]);
                              System.out.println("xOrient[2]" +xOrient[2]);
                              System.out.println("yOrient[0]" +yOrient[0]);
                              System.out.println("yOrient[1]" +yOrient[1]);
                              System.out.println("yOrient[2]" +yOrient[2]);
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
                              System.out.println("dicomOrgin[0]" +dicomOrigin[0]);
                              dicomOrigin[1] = imageOrg[1] + matDicom.get(1, 0);
                              System.out.println("dicomOrgin[1]" +dicomOrigin[1]);
                              dicomOrigin[2] = imageOrg[2] + matDicom.get(2, 0);
                              System.out.println("dicomOrgin[2]" +dicomOrigin[2]);
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
                              for (int j1 = 0; j1 < 4; j1 ++) {
                                  transposeMatrix.set(i, j1, matrix.get(j1, i));
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
                          fireProgressStateChanged((100 * i)/destImage.getExtents()[2]);
                          fileInfoDicom[i] = new FileInfoDicom(dicomInfo.getFileName(), dicomInfo.getFileDirectory(),
                                  dicomInfo.getFileFormat());
                          ((FileInfoDicom)fileInfoDicom[i]).setVr_type(dicomInfo.getVr_type());
                          ((FileInfoDicom) fileInfoDicom[i]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(0));
                          fileInfoDicom[i].getTagTable().setValue("0028,0011", new Short((short) subXDim), 2); // columns
                          fileInfoDicom[i].getTagTable().setValue("0028,0010", new Short((short) subYDim), 2); // rows
                          fileInfoDicom[i].setExtents(destImage.getExtents());
                          fileInfoDicom[i].setResolutions(resolutions);
                          fileInfoDicom[i].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[0], 0);
                          fileInfoDicom[i].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[1], 1);
                          fileInfoDicom[i].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[2], 2);
                          fileInfoDicom[i].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation()); 
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

 }
               // Non-Dicom Images
                  else {
                      fileInfo = destImage.getFileInfo();
                      
                      if (srcImage.is2DImage()) {
                      resolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
                      resolutions[1] = srcImage.getFileInfo(0).getResolutions()[1];
                      resolutions[2] = 1.0f;
                      }
                      else{
                          resolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
                          resolutions[1] = srcImage.getFileInfo(0).getResolutions()[1];
                          resolutions[2] = 1.0f;
                          resolutions[3] = 1.0f;
                      }

                      for (int k = 0; (k < (destImage.getExtents()[2]) && !threadStopped); k++) {
                          fireProgressStateChanged((100 * k)/destImage.getExtents()[2]);
                          fileInfo[k].setModality(srcImage.getFileInfo()[0].getModality());
                          fileInfo[k].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
                          fileInfo[k].setEndianess(srcImage.getFileInfo()[0].getEndianess());
                          fileInfo[k].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                          fileInfo[k].setResolutions(resolutions);
                          fileInfo[k].setExtents(destImage.getExtents());
                          fileInfo[k].setMax(destImage.getMax());
                          fileInfo[k].setMin(destImage.getMin());
                          fileInfo[k].setImageOrientation(srcImage.getImageOrientation());
                          fileInfo[k].setPixelPadValue(srcImage.getFileInfo()[0].getPixelPadValue());
                          fileInfo[k].setPhotometric(srcImage.getFileInfo()[0].getPhotometric());
                          fileInfo[k].setAxisOrientation(srcImage.getAxisOrientation());
                          fileInfo[k].setOrigin(srcImage.getOrigin());
                      }
                      fileInfo = null;
                  }
                  destImage.calcMinMax();
                  
                  
                  
                  if (newImDTIparams != null){
                      destImage.setDTIParameters(newImDTIparams);
                  }
                  
                  setCompleted(true);
                  
              }
              
  }

// *****************************************NOTE*****************************************************************
// DICOM (20,32) is incorrect for mosaics.  The value in this field gives where the
// origin of an image the size of the mosaic would have been had such an image been
// collected.  This puts the origin outside of the scanner.

// Define a flipped version of 'ImageOrientationPatient (20,37)', F, that has flipped columns.
// Thus if the vector of 6 values in 'ImageOrientationPatient', are j,i2,i3,i4,i5,i6, then F =
// [i4 j]
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

/*destImage.calcMinMax();
fileInfo = srcImage.getFileInfo();
if (srcImage.getFileInfo()[0] instanceof FileInfoDicom) {
    //4-D Destination dicom images
    if (srcImage.is3DImage()) {
        FileInfoBase destFileInfo[] = null;
        int numInfos = destImage.getExtents()[3]*destImage.getExtents()[2];
        FileInfoDicom oldDicomInfo = null;
        FileDicomTagTable[] childTagTables = null;
        int j;

    destFileInfo = new FileInfoBase[numInfos];
    oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo(0);
    childTagTables = new FileDicomTagTable[numInfos - 1];
    int sliceCounter = 0; //Keeps track of every slice to populate tag

   // Most efficient way of creating DICOM tags for 4-D. Uses pointers based on srcimage dicom tags    
   for (t = 0; t < destImage.getExtents()[3]; t++) {
       for (z = 0; z <destImage.getExtents()[2] ; z++) {
           j = (t*destImage.getExtents()[2]) + z;
           if (j == 0) {
               // create a new reference file info
               destFileInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                               oldDicomInfo.getFileFormat());
               ((FileInfoDicom)destFileInfo[0]).setVr_type(oldDicomInfo.getVr_type());     
           }
           else {

               // all other slices are children of the first file info..
               destFileInfo[j] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                               oldDicomInfo.getFileFormat(), (FileInfoDicom) destFileInfo[0]);
               
               ((FileInfoDicom)destFileInfo[j]).setVr_type(oldDicomInfo.getVr_type()); 
               childTagTables[j - 1] = ((FileInfoDicom) destFileInfo[j]).getTagTable();
           }
            
            FileDicomTagTable newTagTable = ((FileInfoDicom) destFileInfo[t]).getTagTable();
            if (newTagTable.getValue("0018,0088") != null) {
                String sliceGapString = ((String) ((FileInfoDicom) destFileInfo[t]).getTagTable().getValue("0018,0088")).trim();
                sliceResolution = new Double(sliceGapString.trim()).doubleValue();
            }

                      
                fireProgressStateChanged((((100 * (t*2)))/(destImage.getExtents()[2]+1)));
                resolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
                resolutions[1] = srcImage.getFileInfo(0).getResolutions()[1];
                resolutions[2] = 1.0f;
                resolutions[3] = 1.0f;
                resolutions[4] = 1;
                destFileInfo[sliceCounter].setResolutions(resolutions);
                destFileInfo[sliceCounter].setExtents(destImage.getExtents());
                ((FileInfoDicom) destFileInfo[t]).getTagTable().setValue("0028,0011", new Short((short) subXDim), 2); // columns
                ((FileInfoDicom) destFileInfo[t]).getTagTable().setValue("0028,0010", new Short((short) subYDim), 2); // rows                 
                ((FileInfoDicom) destFileInfo[t]).getTagTable().setValue("0020,0013", Short.toString((short) (t + 1)),
                                                         Short.toString((short) (t + 1)).length()); // instance number
                ((FileInfoDicom) destFileInfo[t]).getTagTable().importTags((FileInfoDicom) fileInfo[t]);
                ((FileInfoDicom) destFileInfo[t]).getTagTable().removeTag("0019,100A");// Removes NumberofImages in Mosaic Tag
                 
                 sliceCounter++;  
        }
       
   }
   
    ((FileInfoDicom) destFileInfo[0]).getTagTable().attachChildTagTables(childTagTables);
    destImage.setFileInfo(destFileInfo);
}*/