package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Quantify image data based on a mask image. At present calculates the number of voxels, volume, total
 * intensity , mean intensity, and standard deviation for an object defined in a mask image.
 *
 * @version  0.1 Feb 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmQuantify extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * The mask image. The range of the image [0, positive integer value] where zero is the background and a positive
     * number indicates that is part of an object that has the same positive integer number.
     */
    private ModelImage maskImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmQuantify object.
     *
     * @param  srcImg   image model where result image is to stored
     * @param  maskImg  source image model
     */
    public AlgorithmQuantify(ModelImage srcImg, ModelImage maskImg) {

        super(null, srcImg);
        maskImage = maskImg; // Put results in destination image.
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }
    
    /**
     * Starts the program
     */
    public void runAlgorithm() {
        int count[];
        int zEnd;
        int tEnd;
        int z;
        int t;
        int sliceSize;
        int imgLength;
        int volSize = 1;
        int cf;
        float buffer[];
        int offset;
        int i;
        float total[][] = null;
        float mean[][] = null;
        float stdDev[][] = null;
        float diff;
        int numObjects;
        short objID;
        int imageSize;
        
        if ((srcImage == null) || (maskImage == null)) {
            displayError("Source and/or Mask Image is null");
            setCompleted(false);
            return;
        }
        
        zEnd = 1;
        tEnd = 1;
        if (srcImage.getNDims() >= 4) {
            tEnd = srcImage.getExtents()[3];
        }
        if (srcImage.getNDims() >= 3) {
            zEnd = srcImage.getExtents()[2];
        }
        if (srcImage.isColorImage()) {
            cf = 4;
        }
        else if ((srcImage.getType() == ModelStorageBase.COMPLEX) || (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
            cf = 2;
        }
        else {
            cf = 1;
        }
        
        numObjects = (int)maskImage.getMax();
        count = new int[numObjects];
        
        imageSize = srcImage.getExtents()[0];
        for (i = 1; i < srcImage.getNDims(); i++) {
            imageSize *= srcImage.getExtents()[i];
        }
        if (cf == 2) {
            for (i = 0; i < imageSize; i++) {
                objID = maskImage.getShort(i);
                if (objID != 0) {
                    count[objID-1]++;
                }
            }
            showRegionInfo(count);
            setCompleted(true);
            return;
        }
        
        total = new float[numObjects][cf];
        mean = new float[numObjects][cf];
        stdDev = new float[numObjects][cf];
        sliceSize = srcImage.getExtents()[0] * srcImage.getExtents()[1];
        imgLength = cf * sliceSize;
        if (srcImage.getNDims() >= 3) {
            volSize = sliceSize * srcImage.getExtents()[2];
        }
        buffer = new float[imgLength];
        for (t = 0; t < tEnd; t++) {
            for (z = 0; z < zEnd; z++) {
                offset = t * volSize + z * sliceSize;
                try {
                  srcImage.exportData(cf * offset, imgLength, buffer); // locks and releases
                                                                                                  // lock
              } catch (IOException error) {
                  MipavUtil.displayError("AlgorithmQuantify: Image(s) locked");
                  setCompleted(false);
                  return;
              } 
              if (cf == 4) {
                  for (i = 0; i < sliceSize; i++) {
                      objID = maskImage.getShort(offset+i);
                      if (objID != 0) {
                          count[objID-1]++;
                          total[objID-1][0] += buffer[4*i + 1];
                          total[objID-1][1] += buffer[4*i + 2];
                          total[objID-1][2] += buffer[4*i + 3];
                      } // if (objID != 0)
                  } // for (i = 0; i < sliceSize; i++)
              } // if (cf == 4)
              else {
                  for (i = 0; i < sliceSize; i++) {
                      objID = maskImage.getShort(offset+i);
                      if (objID != 0) {
                          count[objID-1]++;
                          total[objID-1][0] += buffer[i];
                      } // if (objID != 0)
                  } // for (i = 0; i < sliceSize; i++)        
              }
          } // for (z = 0; z < zEnd; z++)
      } // for (t = 0; t < tEnd; t++)
        
      for (i = 0; i < numObjects; i++) {
          mean[i][0] = total[i][0]/count[i];
          if (cf == 4) {
              mean[i][1] = total[i][1]/count[i];
              mean[i][2] = total[i][2]/count[i];
          }
      } // for (i = 0; i < numObjects; i++)
     
      for (t = 0; t < tEnd; t++) {
          for (z = 0; z < zEnd; z++) {
              offset = t * volSize + z * sliceSize;
              try {
                srcImage.exportData(cf * offset, imgLength, buffer); // locks and releases
                                                                                                // lock
              } catch (IOException error) {
                  MipavUtil.displayError("AlgorithmQuantify: Image(s) locked");
                  setCompleted(false);
                  return;
              } 
              if (cf == 4) {
                  for (i = 0; i < sliceSize; i++) {
                      objID = maskImage.getShort(offset+i);
                      if (objID != 0) {
                          diff = buffer[4*i + 1] - mean[objID-1][0];
                          stdDev[objID-1][0] += diff * diff;
                          diff = buffer[4*i + 2] - mean[objID-1][1];
                          stdDev[objID-1][1] += diff * diff;
                          diff = buffer[4*i + 3] - mean[objID-1][2];
                          stdDev[objID-1][2] += diff * diff;
                      } // if (objID != 0)
                  } // for (i = 0; i < sliceSize; i++)
              } // if (cf == 4)
              else {
                  for (i = 0; i < sliceSize; i++) {
                      objID = maskImage.getShort(offset+i);
                      if (objID != 0) {
                          diff = buffer[i] - mean[objID-1][0];
                          stdDev[objID-1][0] += diff * diff;
                      } // if (objID != 0)
                  } // for (i = 0; i < sliceSize; i++)        
              }
          } // for (z = 0; z < zEnd; z++)
      } // for (t = 0; t < tEnd; t++)
      
      for (i = 0; i < numObjects; i++) {
          stdDev[i][0] = (float)Math.sqrt(stdDev[i][0]/count[i]);
          if (cf == 4) {
              stdDev[i][1] = (float)Math.sqrt(stdDev[i][1]/count[i]);
              stdDev[i][2] = (float)Math.sqrt(stdDev[i][2]/count[i]);
          }
      } // for (i = 0; i < numObjects; i++)
    

      showRegionInfo(count, total, mean, stdDev);
      setCompleted(true);
    }
    
    /**
     * Display statistics about the grown region.
     *
     * @param  count       Number of pixels (voxels)
     * @param  total       Sum of pixel intensities
     * @param  mean        Average pixel intensity
     * @param  stdDev      Standard deviation of pixel intensities
     */
    public void showRegionInfo(int count[], float total[][], float mean[][], float stdDev[][]) {
        float volume;
        float area;
        int pad;
        int i, j;
        String areaString[];
        String volumeString[];
        int numObjects = count.length;

        ViewUserInterface.getReference().setDataText("\n Output from image quantify based on mask. ");
        try {
            String str = new String();
            if (srcImage.getNDims() == 2) {
                areaString = new String[numObjects];
                str = srcImage.getFileInfo(0).getAreaUnitsOfMeasureStr();
                for (i = 0; i < numObjects; i++) {
                    area = count[i] * srcImage.getResolutions(0)[0] * srcImage.getResolutions(0)[1];
                    areaString[i] = String.valueOf(area) + str;
                    if (areaString[i].length() < 20) {
                        pad = 20 - areaString[i].length();
                        for (j = 0; j < pad; j++) {
                            areaString[i] = areaString[i].concat(" ");
                        }   
                    }
                } // for (i = 0; i < numObjects; i++)

                if (total[0].length == 1) {
                    ViewUserInterface.getReference().setDataText("\nObject\tpixels\t\tarea");
                    ViewUserInterface.getReference().setDataText("\t\ttotal intensity\t\tmean intensity\t\tstandard deviation\n");
                    for (i = 0; i < numObjects; i++) {
                        ViewUserInterface.getReference().setDataText(String.valueOf(i+1) + "\t" + count[i] + "\t\t" + areaString[i]);
                        ViewUserInterface.getReference().setDataText("\t" + total[i][0] + "\t\t" + mean[i][0] + "\t\t" + stdDev[i][0] + "\n");
                    }
                }
                else {
                    ViewUserInterface.getReference().setDataText("\n");
                    for (i = 0; i < numObjects; i++) {
                        ViewUserInterface.getReference().setDataText("Object\tpixels\t\tarea\n");
                        ViewUserInterface.getReference().setDataText(String.valueOf(i+1) + "\t" + count[i] + "\t\t" + areaString[i] + "\n");
                        ViewUserInterface.getReference().setDataText("\ttotal intensity\t\tmean intensity\t\tstandard deviation\n");
                        ViewUserInterface.getReference().setDataText("red\t" + total[i][0] + "\t\t" + mean[i][0] + "\t\t" + stdDev[i][0] + "\n"); 
                        ViewUserInterface.getReference().setDataText("green\t" + total[i][1] + "\t\t" + mean[i][1] + "\t\t" + stdDev[i][1] + "\n");
                        ViewUserInterface.getReference().setDataText("blue\t" + total[i][2] + "\t\t" + mean[i][2] + "\t\t" + stdDev[i][2] + "\n");  
                    }
                }

            } else {
                volumeString = new String[numObjects];
                str = srcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                for (i = 0; i < numObjects; i++) {
                    volume = count[i] * srcImage.getResolutions(0)[0] * srcImage.getResolutions(0)[1] *
                             srcImage.getResolutions(0)[2];

                    volumeString[i] = String.valueOf(volume) + str;
                    if (volumeString[i].length() < 20) {
                        pad = 20 - volumeString[i].length();
                        for (j = 0; j < pad; j++) {
                            volumeString[i] = volumeString[i].concat(" ");
                        }   
                    }
                }

                if (total[0].length == 1) {
                    ViewUserInterface.getReference().setDataText("\nObject\tpixels\t\tvolume");
                    ViewUserInterface.getReference().setDataText("\t\ttotal intensity\t\tmean intensity\t\tstandard deviation\n");
                    for (i = 0; i < numObjects; i++) {
                        ViewUserInterface.getReference().setDataText(String.valueOf(i+1) + "\t" + count[i] + "\t\t" + volumeString[i]);
                        ViewUserInterface.getReference().setDataText("\t" + total[i][0] + "\t\t" + mean[i][0] + "\t\t" + stdDev[i][0] + "\n");
                    }
                }
                else {
                    ViewUserInterface.getReference().setDataText("\n");
                    for (i = 0; i < numObjects; i++) {
                        ViewUserInterface.getReference().setDataText("Object\tpixels\t\tvolume\n");
                        ViewUserInterface.getReference().setDataText(String.valueOf(i+1) + "\t" + count[i] + "\t\t" + volumeString[i] + "\n");
                        ViewUserInterface.getReference().setDataText("\ttotal intensity\t\tmean intensity\t\tstandard deviation\n");
                        ViewUserInterface.getReference().setDataText("red\t" + total[i][0] + "\t\t" + mean[i][0] + "\t\t" + stdDev[i][0] + "\n"); 
                        ViewUserInterface.getReference().setDataText("green\t" + total[i][1] + "\t\t" + mean[i][1] + "\t\t" + stdDev[i][1] + "\n");
                        ViewUserInterface.getReference().setDataText("blue\t" + total[i][2] + "\t\t" + mean[i][2] + "\t\t" + stdDev[i][2] + "\n");  
                    }
                }
            }
           
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: AlgorithmQuantify.showRegionInfo");
        }
    }
    
    /**
     * Display statistics about the grown region.
     *
     * @param  count       Number of pixels (voxels)
     */
    public void showRegionInfo(int count[]) {
        float volume;
        float area;
        int pad;
        int i, j;
        String areaString[];
        String volumeString[];
        int numObjects = count.length;

        ViewUserInterface.getReference().setDataText("\n Output from image quantify based on mask. ");
        try {
            String str = new String();
            if (srcImage.getNDims() == 2) {
                areaString = new String[numObjects];
                str = srcImage.getFileInfo(0).getAreaUnitsOfMeasureStr();
                for (i = 0; i < numObjects; i++) {
                    area = count[i] * srcImage.getResolutions(0)[0] * srcImage.getResolutions(0)[1];
                    areaString[i] = String.valueOf(area) + str;
                    if (areaString[i].length() < 20) {
                        pad = 20 - areaString[i].length();
                        for (j = 0; j < pad; j++) {
                            areaString[i] = areaString[i].concat(" ");
                        }   
                    }
                } // for (i = 0; i < numObjects; i++)

                ViewUserInterface.getReference().setDataText("\nObject\tpixels\t\tarea\n");
                    for (i = 0; i < numObjects; i++) {
                        ViewUserInterface.getReference().setDataText(String.valueOf(i+1) + "\t" + count[i] + "\t\t" + areaString[i]);
                    }
            } else {
                volumeString = new String[numObjects];
                str = srcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                for (i = 0; i < numObjects; i++) {
                    volume = count[i] * srcImage.getResolutions(0)[0] * srcImage.getResolutions(0)[1] *
                             srcImage.getResolutions(0)[2];

                    volumeString[i] = String.valueOf(volume) + str;
                    if (volumeString[i].length() < 20) {
                        pad = 20 - volumeString[i].length();
                        for (j = 0; j < pad; j++) {
                            volumeString[i] = volumeString[i].concat(" ");
                        }   
                    }
                }

                ViewUserInterface.getReference().setDataText("\nObject\tpixels\t\tvolume\n");
                for (i = 0; i < numObjects; i++) {
                    ViewUserInterface.getReference().setDataText(String.valueOf(i+1) + "\t" + count[i] + "\t\t" + volumeString[i]);
                }
            }
           
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: AlgorithmQuantify.showRegionInfo");
        }
    }

}
