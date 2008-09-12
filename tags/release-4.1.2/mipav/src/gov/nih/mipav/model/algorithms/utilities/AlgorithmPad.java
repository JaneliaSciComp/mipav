package gov.nih.mipav.model.algorithms.utilities;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import java.io.*;
import java.util.*;


/**
 * Pads 2D and 3D and 4D images using the supplied parameters. 
 *
 * @version  1.0 June 14, 2007
 * @author   Mayur Joshi
 */
public class AlgorithmPad extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag for color or grayscale image. */
    private boolean RGBImage;

    /** xBounds indicating the number of pixels to be padded on left and the total length of X dimension */
    private int[] x = null;

    /** yBounds indicating the number of pixels to be padded on top and the total length of Y dimension */
    private int[] y = null;

    /** xBounds indicating the number of slices to be padded in front and the total length of Z dimension*/
    private int[] z = null;
    
    /** Value to be padded. */
    private int pad;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new algorithm to pad an image with a pad value using specified parameters. Stores in srcImg.
     *
     * @param  srcImg    source image model
     * @param  _x        xBounds: x[0] = Padding on left and x[1] = Padded X dimension
     * @param  _y        yBounds: y[0] = Padding on top and y[1] = Padded Y dimension
     * @param  _z        zBounds: z[0] = Padded slices in front and z[1] = Padded Z dimension
     */
    public AlgorithmPad(ModelImage srcImg, int _pad, int[] _x, int[] _y, int[] _z) {
        super(null, srcImg);
        pad = _pad;
        x = _x;
        y = _y;
        z = _z;
    }

    /**
     * Creates new algorithm to pad an image with a pad value using specified parameters. Stores in destImg.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  _x        xBounds: x[0] = Padding on left and x[1] = Padded X dimension
     * @param  _y        yBounds: y[0] = Padding on top and y[1] = Padded Y dimension
     * @param  _z        zBounds: z[0] = Padded slices in front and z[1] = Padded Z dimension
     */
    public AlgorithmPad(ModelImage destImg, ModelImage srcImg, int _pad, int[] _x, int[] _y, int[] _z) {

        super(destImg, srcImg);
        pad = _pad;
        x = _x;
        y = _y;
        z = _z;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        x = null;
        y = null;
        z = null;
        super.finalize();
    }

    /**
     * Method to return the source image if the result replaces source image.
     *
     * @return  ModelImage
     */
    public ModelImage getSrcImage() {
        return srcImage;
    }
    
    /**
     * Return the result image (or null if not created yet).
     *
     * @return  the padded image
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (srcImage.isColorImage()) {
            RGBImage = true;
        } else {
            RGBImage = false;
        }

        

        if (destImage != null) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() >= 3) {
                calcStoreInDest34D();
            } 
        } // if (destImage != null)
        else { // destImage == null

            if (srcImage.getNDims() == 2) {
                calcStoreInPlace2D();
            } else if (srcImage.getNDims() >= 3) {
                calcStoreInPlace34D();
            } 

        } // else destImage == null

        if (destImage != null) {
            updateFileInfoData();
        }

        if (threadStopped) {
            finalize();
        }
    }


    /**
     * This function produces a new 2D image that has been padded. 
     */
    private void calcStoreInDest2D() {

        int i;
        int j = 0;
        int index = 0;
        int length;
        float[] buffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int[] dimExtents;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            displayError(" Pad: Image(s) locked");
            buffer = null;
            setCompleted(false);

            return;
        } 
        
        try {
            dimExtents = new int[2];
            dimExtents[0] = Math.abs(x[1]);
            dimExtents[1] = Math.abs(y[1]);

            if (RGBImage) {
                length = 4 * xDim * yDim;
            } else {
                length = xDim * yDim;
            }

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Padding image ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Pad Image: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Pad Image:  Out of memory", true);

            return;
        }

        if (RGBImage) {
            length = 4 * dimExtents[0] * dimExtents[1];
        } else {
            length = dimExtents[0] * dimExtents[1];
        }

        int mod = 	length / 100; // mod is 1 percent of length

        if (RGBImage) {
        	
        	int offset = (4 * ((y[0] * x[1]) + x[0]));
        	        	
        	for (i = 0; i < offset; i++){
        		
        		if ((i % mod) == 0) {
    				fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
    			}
        		
        		if (i % 4 == 0) {
        			destImage.set(i, buffer[0]);
        		} else {
        			destImage.set(i, pad);
        		}
        	}
        	        	
        	for (i = 0; i < yDim; i++) {
        		
        		index = offset;
        		
        		if (i != (yDim - 1)) {
        			offset = offset + (4 * x[1]);
        		}
        		
        		        		        		        		
        		for (j = 0; j < (4 * xDim); j++) {
        			
        			if ((index % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) index / (length - 1) * 100));
        			}
        			
        			destImage.set(index, buffer[(4 * i * xDim) + j]);
        			index = index + 1;
        		}
        		
        		for ( j = index; j < offset; j++) {
        			
        			if ((j % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) j / (length - 1) * 100));
        			}
        			
        			if (j % 4 == 0) {
            			destImage.set(j, buffer[0]);
            		} else {
            			destImage.set(j, pad);
            		}
        			index = j;
        		}
        	}
        	
        	for (i = (index + 1); i < length; i++) {
        		
        		if ((i % mod) == 0) {
    				fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
    			}
        		
        		if (i % 4 == 0) {
        			destImage.set(i, buffer[0]);
        		} else {
        			destImage.set(i, pad);
        		}
        	}
        } // end of if (RGBImage)
        else { // not ARGB or ARGB_USHORT  or ARGB_FLOAT
        	
        	int offset = (y[0] * x[1]) + x[0];
        	
        	for (i = 0; i < yDim; i++) {
        		index = offset + (i * x[1]);
        		
        		for (j = 0; j < xDim; j++) {
        			
        			if ((index % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) index / (length - 1) * 100));
        			}
        			
        			destImage.set(index, buffer[(xDim * i) + j]);
        			index = index + 1;
        		}
        	}
        } // not ARGB or ARGB_USHORT or ARGB_FLOAT

        if (threadStopped) {
            buffer = null;

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();

        setCompleted(true);
    }

    /**
     * This function produces a new 3D image that has been padded.
     */
    private void calcStoreInDest34D() {
        int i, j, k, offset, index;
        int length;
        int start = 0;
        int paddedLength;
        int paddedVolume;
        int paddedTimeSet;
        float[] buffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = 1;
        if (srcImage.getNDims() > 3) {
            tDim = srcImage.getExtents()[3];
        }
        int[] dimExtents;
        int t;
        int tOffset;
        int volLength;
                
        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp(" Pad Image: Image(s) locked", true);

            return;
        }

        dimExtents = new int[3];
        dimExtents[0] = Math.abs(x[1]);
        dimExtents[1] = Math.abs(y[1]);
        dimExtents[2] = Math.abs(z[1]); 
        

        if (RGBImage) {
            paddedLength = 4 * dimExtents[0] * dimExtents[1];
        } else {
            paddedLength = dimExtents[0] * dimExtents[1];
        }

        paddedVolume = paddedLength * dimExtents[2];
        paddedTimeSet = paddedVolume * tDim;

        int mod = paddedVolume / 100; // mod is 1 percent of length

        if (RGBImage) {
            volLength = 4 * xDim * yDim * zDim;
        } else {
            volLength = xDim * yDim * zDim;
        }
        length = volLength * tDim;

        try {
            buffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Crop Image: Out of memory", true);

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Padding image ...");


        try {
              srcImage.exportData(0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Pad Image: Image(s) locked", true);

                return;
            }

           
        if (RGBImage) {
            for (t = 0; t < tDim; t++) {
        	    tOffset = t * paddedVolume;    	
            	offset = tOffset + (4 * ((z[0] * x[1] * y[1]) + (y[0] * x[1]) + x[0]));
            	
            	for (i = tOffset; i < offset; i++){
            		
            		if ((i % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
        			}
            		
            		if (i % 4 == 0) {
            			destImage.set(i, buffer[0]);
            		} else {
            			destImage.set(i, pad);
            		}
            	}
            	
            	for (k = 0; k < zDim; k++) {
            		
            		start = offset + (4 * k * x[1] * y[1]);
            		
            		for (i = 0; i < yDim; i++) {
                		
                		index = start;
                		start = start + (4 * x[1]);
                		        		        		        		
                		for (j = 0; j < (4 * xDim); j++) {
                			
                			if ((index % mod) == 0) {
                				fireProgressStateChanged(Math.round((float) index / (length - 1) * 100));
                			}
                			
                			destImage.set(index, buffer[(4 * t * xDim * yDim * zDim) + (4 * k * xDim * yDim) +
                                                        (4 * i * xDim) + j]);
                			index = index + 1;
                		}
                		
                		for ( j = (index + 1); j < start; j++) {
                			
                			if ((j % mod) == 0) {
                				fireProgressStateChanged(Math.round((float) j / (length - 1) * 100));
                			}
                			
                			if (j % 4 == 0) {
                    			destImage.set(i, buffer[0]);
                    		} else {
                    			destImage.set(i, pad);
                    		}
                		}
                			
                	}
            		
            	}
            	
            	for (i = start; i < (t+1)*volLength; i++) {
            		
            		if ((i % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
        			}
            		
            		if (i % 4 == 0) {
            			destImage.set(i, buffer[0]);
            		} else {
            			destImage.set(i, pad);
            		}
            	}
            } // for (t = 0; t < tDim; t++)
                
        } // if (RGBImage)
        else { // not ARGB or ARGB_USHORT or ARGB_FLOAT
        	for (t = 0; t < tDim; t++) { 
                tOffset = t * paddedVolume;
                offset = tOffset + (z[0] * x[1] * y[1]) + (y[0] * x[1]) + x[0];
                
                for (k = 0; k < zDim; k++) {
                	start = offset + (k * x[1] * y[1]);
                	for (i = 0; i < yDim; i++) {
                    	index = start + (i * x[1]);
                    	            		
                    	for (j = 0; j < xDim; j++) {
                    		
                    		if ((index % mod) == 0) {
                				fireProgressStateChanged(Math.round((float) index / (length - 1) * 100));
                			}
                    		
                    		destImage.set(index, buffer[(t * xDim * yDim * zDim) +(k * xDim * yDim) + (xDim * i) + j]);
                    		index = index + 1;
                    	}
                    }
                	
                }
            } // for (t = 0; t < tDim; t++)
        } // not ARGB or ARGB_USHORT or ARGB_FLOAT
        
        buffer = null;

        if (threadStopped) {
            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();

        setCompleted(true);
    }

   

    /**
     * This function pads srcImage Must use getSrcImage after running.
     */
    private void calcStoreInPlace2D() {

        int i, j;
        int length;
        int index = 0;
        float[] buffer;
        float[] destBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int[] dimExtents;
        FileInfoBase[] fInfoBase;
        int dataType = srcImage.getType();
        String imageName = srcImage.getImageName();
        float[] resols;
        int[] axisOrient;
        int nDims = 2;
        int[] direct = new int[nDims];
        float[] imgOriginLPS;
        float[] originImgOrd = new float[3];
        float[] newImgOriginLPS = new float[3];
        String value;


        try {
            dimExtents = new int[2];
            dimExtents[0] = Math.abs(x[1]);
            dimExtents[1] = Math.abs(y[1]);

            if (RGBImage) {
                length = 4 * xDim * yDim;
            } else {
                length = xDim * yDim;
            }

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Padding image ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Pad Image: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Pad Image:  Out of memory", true);

            return;
        }
        
        fInfoBase = new FileInfoBase[1];
        fInfoBase[0] = (FileInfoBase) (srcImage.getFileInfo(0).clone());
        fInfoBase[0].setExtents(dimExtents);


        resols = fInfoBase[0].getResolutions();
        axisOrient = fInfoBase[0].getAxisOrientation();
        
        Vector mats = srcImage.getMatrixHolder().getMatrices();

        for (i = 0; i < nDims; i++) {

            if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                direct[i] = 1;
            } else {
                direct[i] = -1;
            }
        }

        imgOriginLPS = fInfoBase[0].getOrigin();
        originImgOrd = originLPS2Img(imgOriginLPS, srcImage);
        originImgOrd[0] = originImgOrd[0] + (direct[0] * x[0] * resols[0]);
        originImgOrd[1] = originImgOrd[1] + (direct[1] * y[0] * resols[1]);
        newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
        fInfoBase[0].setOrigin(newImgOriginLPS);
        fInfoBase[0].setResolutions(resols);


        if (srcImage.getParentFrame() != null) {
            srcImage.getParentFrame().close();
        }

        srcImage.disposeLocal();
        srcImage = null;


        if (RGBImage) {
            length = 4 * dimExtents[0] * dimExtents[1];
        } else {
            length = dimExtents[0] * dimExtents[1];
        }

        int mod = length / 100; // mod is 1 percent of length

        try {
            destBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            destBuffer = null;
            errorCleanUp("Algorithm Pad Image:  Out of memory", true);

            return;
        }
                
        if (RGBImage) {
        	
        	int offset = (4 * ((y[0] * x[1]) + x[0]));
        	
        	for (i = 0; i < offset; i++){
        		
        		if ((i % mod) == 0) {
    				fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
    			}
        		
        		if (i % 4 == 0) {
        			destBuffer[i] = buffer[0];
        		} else {
        			destBuffer[i] = pad;
        		}
        	}
        	        	
        	for (i = 0; i < yDim; i++) {
        		
        		index = offset;
        		
        		if (i != (yDim -1)) {
        			offset = offset + (4 * x[1]);
        		}
        		
        		        		        		        		
        		for (j = 0; j < (4 * xDim); j++) {
        			
        			if ((index % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) index / (length - 1) * 100));
        			}
        			
        			destBuffer[index] = buffer[(4 * i * xDim) + j];
        			index = index + 1;
        		}
        		
        		for ( j = index; j < offset; j++) {
        			
        			if ((j % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) j / (length - 1) * 100));
        			}
        			
        			if (j % 4 == 0) {
            			destBuffer[j] = buffer[0];
            		} else {
            			destBuffer[j] = pad;
            		}
        			index = j;
        		}
        	}
        	
        	for (i = (index + 1); i < length; i++) {
        		
        		if ((i % mod) == 0) {
    				fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
    			}
        		
        		if (i % 4 == 0) {
        			destBuffer[i] = buffer[0];
        		} else {
        			destBuffer[i] = pad;
        		}
        	}
        } // end of if (RGBImage)
        else { // not ARGB or ARGB_USHORT or ARGB_FLOAT
        	
        	Arrays.fill(destBuffer, (float) pad);
        	
        	int offset = (y[0] * x[1]) + x[0];
        	
        	for (i = 0; i < yDim; i++) {
        		index = offset + (i * x[1]);
        		for (j = 0; j < xDim; j++) {
        			
        			if ((index % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) index / (length - 1) * 100));
        			}
        			
        			destBuffer[index] = buffer[(xDim * i) + j];
        			index = index + 1;
        		}
        	}
        } // not ARGB or ARGB_USHORT or ARGB_FLOAT

        if (threadStopped) {
            buffer = null;

            return;
        }

        buffer = null;

        srcImage = new ModelImage(dataType, dimExtents, imageName);
        srcImage.setFileInfo(fInfoBase[0], 0);
        srcImage.getMatrixHolder().replaceMatrices(mats);

        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
            ((FileInfoDicom) (srcImage.getFileInfo(0))).setSecondaryCaptureTags();
            ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0028,0011",
                                                                               new Short((short) dimExtents[0]), 2); // columns
            ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0028,0010",
                                                                               new Short((short) dimExtents[1]), 2); // rows
            ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0020,0013", Short.toString((short)
                                                                                                           (1)),
                                                                               Short.toString((short) (1)).length()); // instance number


            newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
            value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                    Float.toString(newImgOriginLPS[2]);
            
            ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0020,0032", value, value.length());

            value = String.valueOf(originImgOrd[2]);
            ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0020,1041", value, value.length());
        } // if ( ( srcImage.getFileInfo()[0] ).getFileFormat() == FileUtility.DICOM )

        try {
            srcImage.importData(0, destBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm RGBtoGray: Output Image(s) locked");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * This function pads srcImage Must use getSrcImage after running.
     */
    private void calcStoreInPlace34D() {

        int i,j, k, offset, n, length;
        int index = 0;
        int row_start = 0;
        int paddedLength;
        int paddedVolume;
        int paddedTimeSet;
        float[] buffer;
        float[] destBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = 1;
        if (srcImage.getNDims() > 3) {
            tDim = srcImage.getExtents()[3];
        }
        int[] destExtents;
        int nDims;
        FileInfoBase[] fileInfo;
        int start = 0;
        int dataType = srcImage.getType();
        String imageName = srcImage.getImageName();
        float[] resols;
        int[] axisOrient;
        int[] direct;
        float[] imgOriginLPS;
        float[] originImgOrd = new float[3];
        float[] newImgOriginLPS = new float[3];
        float startPos;
        String value;
        int slc;
        FileInfoDicom dicomInfoBuffer;
        int t;
        int tOffset;


        destExtents = new int[3];
        destExtents[0] = Math.abs(x[1]);
        destExtents[1] = Math.abs(y[1]);
        destExtents[2] = Math.abs(z[1]);
        nDims = 3;
        resols = new float[3];
        resols[2] = srcImage.getFileInfo(0).getResolutions()[2];
        resols[0] = srcImage.getFileInfo(0).getResolutions()[0];
        resols[1] = srcImage.getFileInfo(0).getResolutions()[1];
        direct = new int[nDims];

        if (RGBImage) {
            paddedLength = 4 * destExtents[0] * destExtents[1];
        } else {
            paddedLength = destExtents[0] * destExtents[1];
        }

        paddedVolume = paddedLength * destExtents[2];
        paddedTimeSet = paddedVolume * tDim;

        int mod = paddedTimeSet / 100; // mod is 1 percent of length

        if (RGBImage) {
            length = 4 * xDim * yDim * zDim * tDim;
        } else {
            length = xDim * yDim * zDim * tDim;
        }

        try {
            buffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Pad Image: Out of memory", true);

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Padding image ...");


        try {
        	srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
        	buffer = null;
        	errorCleanUp("Algorithm Pad Image: Image(s) locked", true);
        	return;
        }
        
        int numInfos = z[1] * tDim;
        
        fileInfo = new FileInfoBase[numInfos];
        
        if (srcImage.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
        	FileInfoDicom oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo(0);
        	FileDicomTagTable[] childTagTables = new FileDicomTagTable[numInfos - 1];
        
        
        	// Create all the new file infos (reference and children) and fill them with the tags from the old
        	// file infos. 
            for (t = 0; t < tDim; t++) {
            	for(i = 0; i < z[1]; i++) {
            	    index = t * z[1] + i;
            		if (index == 0) {
            			// Create a reference file info
            			fileInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                            							oldDicomInfo.getFileFormat());
            		} else {
            			fileInfo[index] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
            										oldDicomInfo.getFileFormat(), (FileInfoDicom) fileInfo[0]);
            			childTagTables[index - 1] = ((FileInfoDicom) fileInfo[index]).getTagTable();
           			}
            		
            		if (z[1] > i) {
            			
            			if (i < z[0]) {
            				((FileInfoDicom) fileInfo[index]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(t*srcImage.getExtents()[2]));
            			} else if ((i < srcImage.getExtents()[2]) && (i >= z[0])) {
            				((FileInfoDicom) fileInfo[index]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(t*srcImage.getExtents()[2] + i-z[0]));
            			} else {
            				((FileInfoDicom) fileInfo[index]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(t*srcImage.getExtents()[2] + srcImage.getExtents()[2] - 1));
            			}
            	   	}
            	}
            }
        	
        	((FileInfoDicom) fileInfo[0]).getTagTable().attachChildTagTables(childTagTables);
        	
        } else {
            for (t = 0; t < tDim; t++) {
            	for (i = 0; i < z[1]; i++) {
            		index = t * z[1] + i;
            		if (index == 0) {
            			fileInfo[index] = (FileInfoBase) srcImage.getFileInfo(0).clone();
            		}
            		
            		if (z[1] > i) {
            			if (i < z[0]) {
            				fileInfo[index] = (FileInfoBase) srcImage.getFileInfo(t*srcImage.getExtents()[2]).clone();
            			} else if ((i < srcImage.getExtents()[2]) && (i >= z[0])) {
            				fileInfo[index] = (FileInfoBase) srcImage.getFileInfo(t*srcImage.getExtents()[2] + i).clone();
            			} else {
            				fileInfo[index] = (FileInfoBase) srcImage.getFileInfo(t*srcImage.getExtents()[2]+srcImage.getExtents()[2]-1).clone();
            			}
            		}
            	}
            }
        }
        
        FileInfoBase.copyCoreInfo(srcImage.getFileInfo(), fileInfo);
        
        axisOrient = fileInfo[0].getAxisOrientation();
        
        Vector mats = srcImage.getMatrixHolder().getMatrices();

        for (i = 0; i < nDims; i++) {

            if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                direct[i] = 1;
            } else {
                direct[i] = -1;
            }
        }

        for (n = 0; n < numInfos; n++) {
            imgOriginLPS = fileInfo[n].getOrigin();
            originImgOrd = originLPS2Img(imgOriginLPS, srcImage);
            originImgOrd[0] = originImgOrd[0] + (direct[0] * x[0] * resols[0]);
            originImgOrd[1] = originImgOrd[1] + (direct[1] * y[0] * resols[1]);
            newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
            fileInfo[n].setOrigin(newImgOriginLPS);
            fileInfo[n].setResolutions(resols);
        }

        if (srcImage.getParentFrame() != null) {
            srcImage.getParentFrame().close();
        }

        srcImage.disposeLocal();
        srcImage = null;

        try {
            destBuffer = new float[paddedTimeSet];
        } catch (OutOfMemoryError e) {
            buffer = null;
            destBuffer = null;
            errorCleanUp("Algorithm Pad Image:  Out of memory", true);

            return;
        }

        if (RGBImage) {
        	
        	for (t = 0; t < tDim; t++) {
                tOffset = t * paddedVolume;
                offset = tOffset + (4 * ((z[0] * x[1] * y[1]) + (y[0] * x[1]) + x[0]));
            	
            	for (i = tOffset; i < offset; i++){
            		
            		if ((i % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) i / (paddedTimeSet - 1) * 100));
        			}
            		
            		if (i % 4 == 0) {
            			destBuffer[i] = buffer[0];
            		} else {
            			destBuffer[i] = pad;
            		}
            	}
            	
            	start = offset;
            	
            	for (k = 0; k < zDim; k++) {
            		
            		row_start = start;
            		
            		for (i = 0; i < yDim; i++) {
                		
                		index = row_start;
                		
                		if (i != (yDim - 1)) {
                			row_start = row_start + (4 * x[1]);
                		}
                		
                		
                		            		           		        		        		        		
                		for (j = 0; j < (4 * xDim); j++) {
                			
                			if ((index % mod) == 0) {
                				fireProgressStateChanged(Math.round((float) index / (paddedTimeSet - 1) * 100));
                			}
                			
                			destBuffer[index] = buffer[(4 * t * xDim * yDim * zDim) + (4 * k * xDim * yDim)
                                                        + (4 * i * xDim) + j];
                			index = index + 1;
                		}
                		if (i == (yDim - 1) && k != (zDim -1)) {
                			
                			start = offset + (4 * (k + 1) * x[1] * y[1]);
                			
                			for ( j = index; j < start; j++) {
                				
                				if ((j % mod) == 0) {
                    				fireProgressStateChanged(Math.round((float) j / (paddedTimeSet - 1) * 100));
                    			}
                			
                				if (j % 4 == 0) {
                					destBuffer[j] = buffer[0];
                				} else {
                					destBuffer[j] = pad;
                				}
                				index = j;
                			}
                		} else {
                			
                			for ( j = index; j < row_start; j++) {
                    			
                				if ((j % mod) == 0) {
                    				fireProgressStateChanged(Math.round((float) j / (paddedTimeSet - 1) * 100));
                    			}
                				
                				if (j % 4 == 0) {
                					destBuffer[j] = buffer[0];
                				} else {
                					destBuffer[j] = pad;
                				}
                				index = j;
                			}
                		}
                			
            		}
                }
            	
            	for (i = (index + 1); i < (t+1)*paddedVolume; i++) {
            		
            		if ((i % mod) == 0) {
        				fireProgressStateChanged(Math.round((float) i / (paddedTimeSet - 1) * 100));
        			}
            		
            		if (i % 4 == 0) {
            			destBuffer[i] = buffer[0];
            		} else {
            			destBuffer[i] = pad;
            		}
            	}
            } // for (t = 0; t < tDim; t++)
        	
        } // if (RGBImage)
        else { // not ARGB or ARGB_USHORT or ARGB_FLOAT
        	
        	Arrays.fill(destBuffer, (float) pad);
            for (t = 0; t < tDim; t++) {
        	    tOffset = t * paddedVolume;
            	offset = tOffset + (z[0] * x[1] * y[1]) + (y[0] * x[1]) + x[0];
                
                for (k = 0; k < zDim; k++) {
                	start = offset + (k * x[1] * y[1]);
                	for (i = 0; i < yDim; i++) {
                    	index = start + (i * x[1]);
                    	            		
                    	for (j = 0; j < xDim; j++) {
                    		
                    		if ((index % mod) == 0) {
                				fireProgressStateChanged(Math.round((float) index / (paddedTimeSet - 1) * 100));
                			}
                    		
                    		destBuffer[index] = buffer[(t * xDim * yDim * zDim) + (k * xDim * yDim) + (xDim * i) + j];
                    		index = index + 1;
                    	}
                    }
                	
                }
            } // for (t = 0; t < tDim; t++)
        	
        } // not ARGB or ARGB_USHORT or ARGB_FLOAT
       
        buffer = null;

        if (threadStopped) {
            return;
        }
        
        srcImage = new ModelImage(dataType, destExtents, imageName);

        for (n = 0; n < numInfos; n++) {
            srcImage.setFileInfo(fileInfo[n], n);
        }

        srcImage.getMatrixHolder().replaceMatrices(mats);

        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

            if (nDims == 2) {
                ((FileInfoDicom) (srcImage.getFileInfo(0))).setSecondaryCaptureTags();
                ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0028,0011",
                                                                                   new Short((short) destExtents[0]),
                                                                                   2); // columns
                ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0028,0010",
                                                                                   new Short((short) destExtents[1]),
                                                                                   2); // rows
                ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0020,0013",
                                                                                   Short.toString((short) (1)),
                                                                                   Short.toString((short) (1)).length()); // instance number


                startPos = originImgOrd[2];
                originImgOrd[2] = startPos + (direct[2] * z[0] * resols[2]);
                newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
                value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                        Float.toString(newImgOriginLPS[2]);

                ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0020,0032", value, value.length());

                value = String.valueOf(originImgOrd[2]);
                ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().setValue("0020,1041", value, value.length());
            } // if (nDims = 2)
            else { // nDims >= 3
                for (t = 0; t < tDim; t++) {
                    tOffset = t * srcImage.getExtents()[2];
                    for (n = 0, slc = (n + z[0]); slc < z[1]; n++, slc++) {
    
                        ((FileInfoDicom) (srcImage.getFileInfo(tOffset+n))).setSecondaryCaptureTags();
                        ((FileInfoDicom) (srcImage.getFileInfo(tOffset+n))).getTagTable().setValue("0028,0011",
                                                                                           new Short((short) destExtents[0]),
                                                                                           2); // columns
                        ((FileInfoDicom) (srcImage.getFileInfo(tOffset+n))).getTagTable().setValue("0028,0010",
                                                                                           new Short((short) destExtents[1]),
                                                                                           2); // rows
                        ((FileInfoDicom) (srcImage.getFileInfo(tOffset+n))).getTagTable().setValue("0020,0013",
                                                                                           Short.toString((short) (n + 1)),
                                                                                           Short.toString((short) (n + 1)).length()); // instance number
                        //dicomInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(slc - z[0]);
                        dicomInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(tOffset+n);
                        // change the slice number ("0020,0013"):
                        // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                        value = Integer.toString(slc - (int) z[0] + 1);
                        dicomInfoBuffer.getTagTable().setValue("0020,0013", value, value.length());
    
                        // change the image start position ("0020, 0032")
                        startPos = originImgOrd[2];
                        originImgOrd[2] = startPos + (direct[2] * z[0] * resols[2]);
                        newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
                        value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                                Float.toString(newImgOriginLPS[2]);
                        dicomInfoBuffer.getTagTable().setValue("0020,0032", value, value.length());
    
                        // readjust the slice location ("0020,1041")
                        value = String.valueOf(originImgOrd[2]);
                        dicomInfoBuffer.getTagTable().setValue("0020,1041", value, value.length());
                    } // for ( n = 0, slc = z[0]; slc <= z[1]; n++, slc++ )
                } // for (t = 0; t < tDim; t++)
            } // else nDims >= 3
        } // if ( ( srcImage.getFileInfo()[0] ).getFileFormat() == FileUtility.DICOM )


        try {
            srcImage.importData(0, destBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Pad Image: Output Image(s) locked");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Switch origin order from image order to LPS order.
     *
     * @param   origImg  Image origin!
     * @param   img      Image!
     *
     * @return  origLPS   Origin in LPS order!
     */
    private float[] originImg2LPS(float[] origImg, ModelImage img) {
        float[] origLPS = new float[3];
        TransMatrix img2LPS = img.getMatrix();

        for (int i = 0; i < 3; i++) { // i's are the rows

            for (int j = 0; j < 3; j++) { // j's are the columns

                if (img2LPS.Get(i, j) != 0) {
                    origLPS[i] = origImg[j];
                }
            }
        }

        return origLPS;
    }

    /**
     * Switch origin order from LPS order to Img order.
     *
     * @param   origLPS  Origin in LPS order!
     * @param   img      Image!
     *
     * @return  origImg	 Origin in image order!
     */
    private float[] originLPS2Img(float[] origLPS, ModelImage img) {
        float[] origImg = new float[3];
        TransMatrix LPS2img = img.getMatrix().clone();
        LPS2img.Inverse();

        for (int i = 0; i < 3; i++) { // i's are the rows

            for (int j = 0; j < 3; j++) { // j's are the columns

                if (LPS2img.Get(i, j) != 0) {
                    origImg[i] = origLPS[j];
                }
            }
        }

        return origImg;
    }

    /**
     * Update special case DICOM format tags.
     */
    private void updateDICOM() {
        int n;
        float startPos;
        float[] originImgOrd = new float[3];
        float[] newImgOriginLPS = new float[3];
        float[] resols;
        String value;
        int slc;
        int[] destExtents = destImage.getExtents();
        int[] direct = new int[3];
        FileInfoDicom dicomInfoBuffer;
        int tDim = 1;
        if (destImage.getNDims() > 3) {
            tDim = destImage.getExtents()[3];
        }
        int t;
        int tOffset;

        dicomInfoBuffer = (FileInfoDicom) destImage.getFileInfo(0);
        destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());


        float[] origins = new float[3];


        originImgOrd = dicomInfoBuffer.getOrigin();

        for (int i = 0; i < originImgOrd.length; i++) {
            origins[i] = originImgOrd[i];
        }

        resols = srcImage.getFileInfo(0).getResolutions();

        int nDims = destImage.getNDims();
        int[] axisOrient = destImage.getFileInfo()[0].getAxisOrientation();

        for (int i = 0; i < Math.min(3,nDims); i++) {

            if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                direct[i] = 1;
            } else {
                direct[i] = -1;
            }
        }

        if ((destImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

            if ((destImage.getNDims() == 2) || (z[0] == z[1])) {
                ((FileInfoDicom) (destImage.getFileInfo(0))).setSecondaryCaptureTags();
                ((FileInfoDicom) (destImage.getFileInfo(0))).getTagTable().setValue("0028,0011",
                                                                                    new Short((short) destExtents[0]),
                                                                                    2); // columns
                ((FileInfoDicom) (destImage.getFileInfo(0))).getTagTable().setValue("0028,0010",
                                                                                    new Short((short) destExtents[1]),
                                                                                    2); // rows
                ((FileInfoDicom) (destImage.getFileInfo(0))).getTagTable().setValue("0020,0013",
                                                                                    Short.toString((short) (1)),
                                                                                    Short.toString((short) (1)).length()); // instance number

                // int imgOrient = ((FileInfoDicom) (destImage.getFileInfo(0))).getImageOrientation();

                startPos = origins[2];
                origins[2] = startPos + (direct[2] * z[0] * resols[2]);
                newImgOriginLPS = originImg2LPS(origins, destImage);
                value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                        Float.toString(newImgOriginLPS[2]);

                ((FileInfoDicom) (destImage.getFileInfo(0))).getTagTable().setValue("0020,0032", value, value.length());

                value = String.valueOf(originImgOrd[2]);
                ((FileInfoDicom) (destImage.getFileInfo(0))).getTagTable().setValue("0020,1041", value, value.length());
            } else if (destImage.getNDims() == 3) {
                // int imgOrient = dicomInfoBuffer.getImageOrientation();

                Vector3f originVOI = new Vector3f(x[0], y[0], z[0]);
                Vector3f originLPS = new Vector3f();

                startPos = origins[2];

                // System.err.println("Original file origin: " + originVOI);
                for (n = 0, slc = (n + z[0]); slc < z[1]; n++, slc++) {

                    ((FileInfoDicom) (destImage.getFileInfo(n))).setSecondaryCaptureTags();
                    ((FileInfoDicom) (destImage.getFileInfo(n))).getTagTable().setValue("0028,0011",
                                                                                        new Short((short) destExtents[0]),
                                                                                        2); // columns
                    ((FileInfoDicom) (destImage.getFileInfo(n))).getTagTable().setValue("0028,0010",
                                                                                        new Short((short) destExtents[1]),
                                                                                        2); // rows
                    ((FileInfoDicom) (destImage.getFileInfo(n))).getTagTable().setValue("0020,0013",
                                                                                        Short.toString((short) (n + 1)),
                                                                                        Short.toString((short) (n + 1)).length()); // instance number
                    dicomInfoBuffer = (FileInfoDicom) destImage.getFileInfo(n);

                    // change the slice number ("0020,0013"):
                    // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                    value = Integer.toString(slc - (int) z[0] + 1);
                    dicomInfoBuffer.getTagTable().setValue("0020,0013", value, value.length());

                    // change the image start position ("0020, 0032")

                    originVOI.Z = slc;
                    MipavCoordinateSystems.fileToScanner(originVOI, originLPS, srcImage);

                    value = Float.toString(originLPS.X) + "\\" + Float.toString(originLPS.Y) + "\\" +
                            Float.toString(originLPS.Z);

                    dicomInfoBuffer.getTagTable().setValue("0020,0032", value, value.length());

                    // readjust the slice location ("0020,1041")
                    value = String.valueOf(originImgOrd[2]);
                    dicomInfoBuffer.getTagTable().setValue("0020,1041", value, value.length());
                } 
            } else if (destImage.getNDims() == 4) {
                // int imgOrient = dicomInfoBuffer.getImageOrientation();

                Vector3f originVOI = new Vector3f(x[0], y[0], z[0]);
                Vector3f originLPS = new Vector3f();

                startPos = origins[2];
                for (t = 0; t < tDim; t++) {
                    tOffset = t * z[1];
                    // System.err.println("Original file origin: " + originVOI);
                    for (n = 0, slc = (n + z[0]); slc < z[1]; n++, slc++) {
    
                        ((FileInfoDicom) (destImage.getFileInfo(tOffset+n))).setSecondaryCaptureTags();
                        ((FileInfoDicom) (destImage.getFileInfo(tOffset+n))).getTagTable().setValue("0028,0011",
                                                                                            new Short((short) destExtents[0]),
                                                                                            2); // columns
                        ((FileInfoDicom) (destImage.getFileInfo(tOffset+n))).getTagTable().setValue("0028,0010",
                                                                                            new Short((short) destExtents[1]),
                                                                                            2); // rows
                        ((FileInfoDicom) (destImage.getFileInfo(tOffset+n))).getTagTable().setValue("0020,0013",
                                                                                            Short.toString((short) (n + 1)),
                                                                                            Short.toString((short) (n + 1)).length()); // instance number
                        dicomInfoBuffer = (FileInfoDicom) destImage.getFileInfo(tOffset+n);
    
                        // change the slice number ("0020,0013"):
                        // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                        value = Integer.toString(slc - (int) z[0] + 1);
                        dicomInfoBuffer.getTagTable().setValue("0020,0013", value, value.length());
    
                        // change the image start position ("0020, 0032")
    
                        originVOI.Z = slc;
                        MipavCoordinateSystems.fileToScanner(originVOI, originLPS, srcImage);
    
                        value = Float.toString(originLPS.X) + "\\" + Float.toString(originLPS.Y) + "\\" +
                                Float.toString(originLPS.Z);
    
                        dicomInfoBuffer.getTagTable().setValue("0020,0032", value, value.length());
    
                        // readjust the slice location ("0020,1041")
                        value = String.valueOf(originImgOrd[2]);
                        dicomInfoBuffer.getTagTable().setValue("0020,1041", value, value.length());
                    } 
                }
                
            }
        }
    }

    /**
     * Updates important image attributes (start locations, orientation ) for the new padded file by modifying the
     * fileinfo fo the new ( destination ) image.
     */
    private void updateFileInfoData() {
        int i;
        int numInfos;
        float[] resols;
        float[] resolsTmp;
        float[] imgOriginLPS;
        float[] originImgOrd = new float[3];
        float[] newImgOriginLPS = new float[3];
        int[] axisOrient;
        int[] extentsTmp;
        FileInfoBase[] fileInfo;
        FileInfoBase fileInfoBuffer;
        int nDims = destImage.getNDims();
        int[] direct = new int[nDims];
        int zDim = 1;
        if (destImage.getNDims() > 2) {
            zDim = destImage.getExtents()[2];
        }
        int tDim = 1;
        if (destImage.getNDims() > 3) {
            tDim = destImage.getExtents()[3];
        }
        int t;
        int index;
        
        numInfos = zDim * tDim;
        
        fileInfo = new FileInfoBase[numInfos];
        
        if (srcImage.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
        	FileInfoDicom oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo(0);
        	FileDicomTagTable[] childTagTables = new FileDicomTagTable[numInfos - 1];
        
        
        	// Create all the new file infos (reference and children) and fill them with the tags from the old
        	// file infos.
            for (t = 0; t < tDim; t++) {
            	for(i = 0; i < zDim; i++) {
            	    index = t * zDim + i;
            		if (index == 0) {
            			// Create a reference file info
            			fileInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                            							oldDicomInfo.getFileFormat());
            		} else {
            			fileInfo[index] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
            										oldDicomInfo.getFileFormat(), (FileInfoDicom) fileInfo[0]);
            			childTagTables[index - 1] = ((FileInfoDicom) fileInfo[index]).getTagTable();
           			}
            		
            		if ((zDim > i) && (nDims > 2)) {
            			
            			if (i < z[0]) {
            				((FileInfoDicom) fileInfo[index]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(t*srcImage.getExtents()[2]));
            			} else if ((i < srcImage.getExtents()[2]) && (i >= z[0])) {
            				((FileInfoDicom) fileInfo[index]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(t*srcImage.getExtents()[2] + i-z[0]));
            			} else {
            				((FileInfoDicom) fileInfo[index]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(t*srcImage.getExtents()[2] + srcImage.getExtents()[2] - 1));
            			}
            	   	}
            	}
            } // for (t = 0; t < tDim; t++)
        	
        	((FileInfoDicom) fileInfo[0]).getTagTable().attachChildTagTables(childTagTables);
        	
        } else {
            for (t = 0; t < tDim; t++) {
            	for (i = 0; i < zDim; i++) {
            		index = t*zDim + i;
            		if (index == 0) {
            			fileInfo[index] = (FileInfoBase) srcImage.getFileInfo(0).clone();
            		}
            		
            		if ((zDim > i) && (nDims > 2)) {
            			if (i < z[0]) {
            				fileInfo[index] = (FileInfoBase) srcImage.getFileInfo(t*srcImage.getExtents()[2]).clone();
            			} else if ((i < srcImage.getExtents()[2]) && (i >= z[0])) {
            				fileInfo[index] = (FileInfoBase) srcImage.getFileInfo(t*srcImage.getExtents()[2]+i).clone();
            			} else {
            				fileInfo[index] = (FileInfoBase) srcImage.getFileInfo(t*srcImage.getExtents()[2] + srcImage.getExtents()[2]-1).clone();
            			}
            		}
            	}
            }
        }
        
        FileInfoBase.copyCoreInfo(srcImage.getFileInfo(), fileInfo);
        
        destImage.setFileInfo(fileInfo);
        
        if (nDims == 2) {
        	destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());

            // Copies the source's image file info.
            //destImage.setFileInfo((FileInfoBase) (srcImage.getFileInfo()[start].clone()), 0);
            fileInfoBuffer = (FileInfoBase) destImage.getFileInfo(0);
            resols = fileInfoBuffer.getResolutions();
            axisOrient = fileInfoBuffer.getAxisOrientation();

            for (i = 0; i < nDims; i++) {

                if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                    direct[i] = 1;
                } else {
                    direct[i] = -1;
                }
            }

            imgOriginLPS = fileInfoBuffer.getOrigin();

            originImgOrd = originLPS2Img(imgOriginLPS, destImage);
            originImgOrd[0] = originImgOrd[0] - (direct[0] * x[0] * resols[0]);
            originImgOrd[1] = originImgOrd[1] - (direct[1] * y[0] * resols[1]);
            newImgOriginLPS = originImg2LPS(originImgOrd, destImage);

            fileInfoBuffer.setOrigin(newImgOriginLPS);

            if (srcImage.getNDims() == 3) {
                resolsTmp = new float[2];
                resolsTmp[0] = resols[0];
                resolsTmp[1] = resols[1];
                fileInfoBuffer.setResolutions(resolsTmp);
                extentsTmp = new int[2];
                extentsTmp[0] = destImage.getExtents()[0];
                extentsTmp[1] = destImage.getExtents()[1];
                fileInfoBuffer.setExtents(extentsTmp);
            } else {
                fileInfoBuffer.setResolutions(resols);
                fileInfoBuffer.setExtents(destImage.getExtents());
            }
        } else if (nDims == 3) {
        	destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices()); 

        	fileInfoBuffer = destImage.getFileInfo(0);
            resols = fileInfoBuffer.getResolutions();
            axisOrient = fileInfoBuffer.getAxisOrientation();


            if (srcImage.getNDims() == 4) { // Reallocate resolutions since we are going from 4D to 3D
                resolsTmp = new float[3];
                resolsTmp[0] = resols[0];
                resolsTmp[1] = resols[1];
                resolsTmp[2] = resols[3];

                extentsTmp = new int[3];
                extentsTmp[0] = destImage.getExtents()[0];
                extentsTmp[1] = destImage.getExtents()[1];
                extentsTmp[2] = destImage.getExtents()[3];
            } else {
                resolsTmp = resols;
                extentsTmp = destImage.getExtents();
            }

            for (i = 0; i < nDims; i++) {

                if ((axisOrient[i] == FileInfoBase.ORI_R2L_TYPE) || (axisOrient[i] == FileInfoBase.ORI_A2P_TYPE) ||
                        (axisOrient[i] == FileInfoBase.ORI_I2S_TYPE) ||
                        (axisOrient[i] == FileInfoBase.ORI_UNKNOWN_TYPE)) {
                    direct[i] = 1;
                } else {
                    direct[i] = -1;
                }
            }

            Vector3f fileOriginVOI = new Vector3f(x[0], y[0], z[0]);
            Vector3f lpsOriginVOI = new Vector3f();
            MipavCoordinateSystems.fileToScanner(fileOriginVOI, lpsOriginVOI, destImage);
 
            int orientation = destImage.getImageOrientation();

            if ((orientation == FileInfoBase.AXIAL) || (orientation == FileInfoBase.UNKNOWN_ORIENT)) {
                originImgOrd[0] = lpsOriginVOI.X;
                originImgOrd[1] = lpsOriginVOI.Y;
                originImgOrd[2] = lpsOriginVOI.Z;
            } else if (orientation == FileInfoBase.SAGITTAL) {
                originImgOrd[0] = lpsOriginVOI.Y;
                originImgOrd[1] = lpsOriginVOI.Z;
                originImgOrd[2] = lpsOriginVOI.X;
            } else if (orientation == FileInfoBase.CORONAL) {
                originImgOrd[0] = lpsOriginVOI.X;
                originImgOrd[1] = lpsOriginVOI.Z;
                originImgOrd[2] = lpsOriginVOI.Y;
            }


            for (int s = 0; s < destImage.getExtents()[2]; s++) { // for each slice
                fileInfoBuffer = destImage.getFileInfo(s);

                destImage.getFileInfo(s).setExtents(extentsTmp);
                destImage.getFileInfo(s).setResolutions(resolsTmp);
                fileInfoBuffer.setOrigin(originImgOrd);
            }
        } else if (nDims == 4) {
            destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices()); 

            fileInfoBuffer = destImage.getFileInfo(0);
            resols = fileInfoBuffer.getResolutions();
            axisOrient = fileInfoBuffer.getAxisOrientation();

            for (i = 0; i < 3; i++) {

                if ((axisOrient[i] == FileInfoBase.ORI_R2L_TYPE) || (axisOrient[i] == FileInfoBase.ORI_A2P_TYPE) ||
                        (axisOrient[i] == FileInfoBase.ORI_I2S_TYPE) ||
                        (axisOrient[i] == FileInfoBase.ORI_UNKNOWN_TYPE)) {
                    direct[i] = 1;
                } else {
                    direct[i] = -1;
                }
            }

            Vector3f fileOriginVOI = new Vector3f(x[0], y[0], z[0]);
            Vector3f lpsOriginVOI = new Vector3f();
            MipavCoordinateSystems.fileToScanner(fileOriginVOI, lpsOriginVOI, destImage);
 
            int orientation = destImage.getImageOrientation();

            if ((orientation == FileInfoBase.AXIAL) || (orientation == FileInfoBase.UNKNOWN_ORIENT)) {
                originImgOrd[0] = lpsOriginVOI.X;
                originImgOrd[1] = lpsOriginVOI.Y;
                originImgOrd[2] = lpsOriginVOI.Z;
            } else if (orientation == FileInfoBase.SAGITTAL) {
                originImgOrd[0] = lpsOriginVOI.Y;
                originImgOrd[1] = lpsOriginVOI.Z;
                originImgOrd[2] = lpsOriginVOI.X;
            } else if (orientation == FileInfoBase.CORONAL) {
                originImgOrd[0] = lpsOriginVOI.X;
                originImgOrd[1] = lpsOriginVOI.Z;
                originImgOrd[2] = lpsOriginVOI.Y;
            }


            for (int s = 0; s < destImage.getExtents()[2]*destImage.getExtents()[3]; s++) { // for each slice
                fileInfoBuffer = destImage.getFileInfo(s);

                destImage.getFileInfo(s).setExtents(destImage.getExtents());
                destImage.getFileInfo(s).setResolutions(resols);
                fileInfoBuffer.setOrigin(originImgOrd);
            }    
        }

        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
            updateDICOM();
        }

    }

}

