package gov.nih.mipav.model.algorithms.utilities;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * Flips 2D, 3D or 4D grays scale or color dataset about X, Y, or Z axis (when applicable) when AlgorithmFlip.IMAGE is
 * passed to the constructor. An option is given to flip all VOIs at this time. When AlgorithmFlip.VOI is passed, only
 * the selected VOI is flipped about the specified axis.
 *
 * @version  1.0 July 14, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */

public class AlgorithmFlip extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Flip along Y axis. */
    public static final int Y_AXIS = 0;

    /** Flip along X axis. */
    public static final int X_AXIS = 1;

    /** Flip along Z axis. */
    public static final int Z_AXIS = 2;

    /** Image and all VOIs should be flipped. */
    public static final int IMAGE_AND_VOI = 2;

    /** Denotes image should be flipped without VOI. */
    public static final int IMAGE = 0;

    /** Denotes selected VOI should be flipped. */
    public static final int VOI_TYPE = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Axis to flip along. */
    private int flipAxis = Y_AXIS;

    /** Type of object to flip. */
    private int flipObject;
    
    /** Whether orientation and origin should change with flipping. */
    private boolean changeOrientationOrigin;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Flips 2D, 3D or 4D grays scale or color dataset about X or Y axis.
     *
     * @param  srcImg      source image model
     * @param  flipMode    flip about which axis
     * @param  flipObject  DOCUMENT ME!
     * @param  changeOrientationOrigin
     */
    public AlgorithmFlip(ModelImage srcImg, int flipMode, int flipObject, boolean changeOrientationOrigin) {
        super(null, srcImg);
        this.flipObject = flipObject;
        this.changeOrientationOrigin = changeOrientationOrigin;

        if ((flipMode == Y_AXIS) || (flipMode == X_AXIS) || (flipMode == Z_AXIS)) {
            flipAxis = flipMode;
        } else {
            flipAxis = Y_AXIS;
        }
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }


    /**
     * Runs the flip algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (srcImage.getNDims() == 2) {
            calcInPlace(1);
        } else if (srcImage.getNDims() == 3) {
            calcInPlace(srcImage.getExtents()[2]);
        } else if (srcImage.getNDims() == 4) {
            calcInPlace(srcImage.getExtents()[2] * srcImage.getExtents()[3]);
        }
    }

    /**
     * Generates the flipped image and replaces the source image with the flippeded image.
     *
     * @param  nImages  Number of images to be flipped. If 2D image then nImage = 1, if 3D or 4D image where each image
     *                  is to processed independently then nImages equals the number of images in the volume.
     */
    private void calcInPlace(int nImages) {
        int slice;
        float[] sliceBuffer;
        float[] sliceBufferTemp = null;
        int buffFactor;
        boolean logMagDisplay = Preferences.is(Preferences.PREF_LOGMAG_DISPLAY);
        boolean evenNumberZSlices = true;
        int i;
        int j;
        float loc;

        try {

            if (srcImage.isColorImage()) {
                buffFactor = 4;
            } else if ((srcImage.getType() == ModelStorageBase.COMPLEX) ||
                           (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
                buffFactor = 2;
                logMagDisplay = srcImage.getLogMagDisplay();
            } else {
                buffFactor = 1;
            }

            slice = buffFactor * srcImage.getSliceSize();
            sliceBuffer = new float[slice];
            fireProgressStateChanged(srcImage.getImageName(), "Flipping image ...");
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm Flip: Out of memory");
            setCompleted(false);

            return;
        }
        
        if (srcImage.getNDims() >= 3) {
            if (srcImage.getExtents()[2]/2 * 2 != srcImage.getExtents()[2]) {
               evenNumberZSlices = false;    
            }
        }

        int mod = nImages / 10; // mod is 10 percent of length

        if (mod == 0) {

            // since % mod gives a divide by zero error for mod = 0
            mod = 1;
        }

        /* axisOrder is always the default: (no coordinate axis remapping) */
        int[] axisOrder = { 0, 1, 2 };

        /* axisFlip depends on flipAxis: */
        boolean[] axisFlip = { false, false, false };
        int index = 2;

        if (flipAxis == Y_AXIS) {
            index = 0;
        } else if (flipAxis == X_AXIS) {
            index = 1;
        }

        axisFlip[index] = true;
        
        FileInfoBase[] fileInfo = srcImage.getFileInfo();

        int tDim = 1;
        int volume = 1;
        int zDim = 1;
        int xDim = 1;
        int yDim = 1;

        if (srcImage.getNDims() > 1) {
            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];
        }

        if (srcImage.getNDims() == 4) {
            zDim = srcImage.getExtents()[2];
            tDim = srcImage.getExtents()[3];
            volume = slice * zDim;
        } else if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }

        if ((flipObject == AlgorithmFlip.IMAGE) || (flipObject == AlgorithmFlip.IMAGE_AND_VOI)) {


            /* If flipping the z-axis, then loop 1/2 the times and swap the z slices... */
            if (index == 2) {
                zDim /= 2;
                sliceBufferTemp = new float[slice];
            }

            /* For each slice: */
            for (int t = 0; (t < tDim) && !threadStopped; t++) {

                for (int z = 0; (z < zDim) && !threadStopped; z++) {

                    if ((nImages > 1) && ((((t * zDim) + z) % mod) == 0)) {
                        fireProgressStateChanged(Math.round((float) ((t * zDim) + z) / (nImages - 1) * 100));
                    }

                    try {
                        srcImage.export(axisOrder, axisFlip, t, z, sliceBuffer, false);

                        if (index == 2) {
                            int zTemp = (srcImage.getExtents()[2] - 1 - z);
                            srcImage.export(axisOrder, axisFlip, t, zTemp, sliceBufferTemp, false);
                            srcImage.importData((t * volume) + (zTemp * slice), sliceBufferTemp, false);
                        }

                        srcImage.importData((t * volume) + (z * slice), sliceBuffer, false);
                    } catch (IOException error) {
                        displayError("AlgorithmSubset reports: Destination image already locked.");
                        setCompleted(false);

                        return;
                    }
                }
            }

            if (threadStopped) {
                sliceBuffer = null;
                sliceBufferTemp = null;
                finalize();

                return;
            }

            if (buffFactor == 2) {
                srcImage.setLogMagDisplay(logMagDisplay);
                srcImage.calcMinMaxMag(logMagDisplay);
            }

            /* Update FileInfo for mapping into DICOM space: */
            if (changeOrientationOrigin && srcImage.getNDims() > 2) {
                
                float origin[] = new float[3];
                Vector3f position;
                Vector3f out;
                if (index == 0) {
                	position = new Vector3f(fileInfo[0].getExtents()[0] - 1, 0, 0);
                }
                else if (index == 1) {
                	position =  new Vector3f(0, fileInfo[0].getExtents()[1] - 1, 0);
                }
                else {
                	position = new Vector3f(0, 0, fileInfo[0].getExtents()[2] - 1);
                }
                out = new Vector3f(position);
                MipavCoordinateSystems.fileToScanner(position, out, srcImage);
                origin[0] = out.X;
                origin[1] = out.Y;
                origin[2] = out.Z;
            	
                int orient = fileInfo[0].getAxisOrientation(index);
    
                orient = FileInfoBase.oppositeOrient(orient);
    
                for (i = 0; i < fileInfo.length; i++) {
                    fileInfo[i].setAxisOrientation(orient, index);
                    for (j = 0; j < 3; j++) {
                    	// set change for each slice later on
                		if ((fileInfo[0].getAxisOrientation()[j] == FileInfoBase.ORI_R2L_TYPE) ||
                			(fileInfo[0].getAxisOrientation()[j] == FileInfoBase.ORI_L2R_TYPE)) {
                	        fileInfo[i].setOrigin(origin[0],j);
                		}
                		else if ((fileInfo[0].getAxisOrientation()[j] == FileInfoBase.ORI_A2P_TYPE) ||
                		        (fileInfo[0].getAxisOrientation()[j] == FileInfoBase.ORI_P2A_TYPE)) {
                		    fileInfo[i].setOrigin(origin[1], j);
                		}
                		else {
                		    fileInfo[i].setOrigin(origin[2], j);        	
                	    }
                	}
                	

                }
                
                if ( (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                        || (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
                	TransMatrix dicomMatrix = null;
                	dicomMatrix = srcImage.getMatrix().clone();
                	
                	for (j = 0; j < 3; j++) {
                        dicomMatrix.set(j, index, -dicomMatrix.get(j, index));
                    }
                	dicomMatrix.setTransformID(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL);
                	srcImage.getMatrixHolder().clearMatrices();
                	srcImage.getMatrixHolder().addMatrix(dicomMatrix);
                } // if ( (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                
                if (fileInfo[0] instanceof FileInfoNIFTI) {
                    MatrixHolder matHolder = null;
                    matHolder = srcImage.getMatrixHolder();

                    if (matHolder != null) {
                        LinkedHashMap<String, TransMatrix> matrixMap = matHolder.getMatrixMap();
                        Iterator<String> iter = matrixMap.keySet().iterator();
                        String nextKey = null;
                        
                        TransMatrix tempMatrix = null;
                        
                        while (iter.hasNext()) {
                            nextKey = iter.next();
                            tempMatrix = matrixMap.get(nextKey);
                            if (tempMatrix.isNIFTI()) { 
                                for (j = 0; j < 3; j++) {
                                    tempMatrix.set(j, index, -tempMatrix.get(j, index));
                                }
                                //The qoffset_{x,y,z} values are explicitly LR, PA, IS, as the
                                //transformations always describe resulting coordinates in LPI (sign
                                //and order) orientation. The sign of those coordinates corresponds
                                //to LPI being the negative directions.
                                // However, our displayed matrixQ and  matrixS have been reworked
                                // to display R-L, A-P, and I-S
                                loc = tempMatrix.get(index, 3);
                                orient = srcImage.getFileInfo(0).getAxisOrientation(index);
                            	if ((orient == FileInfoBase.ORI_R2L_TYPE) || 
                                        (orient == FileInfoBase.ORI_A2P_TYPE) || 
                                        (orient == FileInfoBase.ORI_I2S_TYPE)) {
                                	loc = loc - ((srcImage.getFileInfo(0).getExtents()[index] - 1) * srcImage.getFileInfo(0).getResolutions()[index]);
                                }
                            	else {
                            		loc = loc + ((srcImage.getFileInfo(0).getExtents()[index] - 1) * srcImage.getFileInfo(0).getResolutions()[index]);	
                            	}
                                tempMatrix.set(index, 3, loc);
                                
                                if (tempMatrix.isQform()) {
                                    if (srcImage.getNDims() == 3) {
                                        for (i = 0; i < srcImage.getExtents()[2]; i++) {
                                            ((FileInfoNIFTI)fileInfo[i]).setMatrixQ(tempMatrix);
                                        }
                                    }
                                    else if (srcImage.getNDims() == 4) {
                                        for (i = 0; i < srcImage.getExtents()[2]*srcImage.getExtents()[3]; i++) {
                                            ((FileInfoNIFTI)fileInfo[i]).setMatrixQ(tempMatrix);    
                                        }
                                    }
                                } // if (tempMatrix.isQform())
                                else { // tempMatrix is sform
                                    if (srcImage.getNDims() == 3) {
                                        for (i = 0; i < srcImage.getExtents()[2]; i++) {
                                            ((FileInfoNIFTI)fileInfo[i]).setMatrixS(tempMatrix);
                                        }
                                    }
                                    else if (srcImage.getNDims() == 4) {
                                        for (i = 0; i < srcImage.getExtents()[2]*srcImage.getExtents()[3]; i++) {
                                            ((FileInfoNIFTI)fileInfo[i]).setMatrixS(tempMatrix);    
                                        }
                                    }    
                                } // else tempMatrix is sform
                            }
                        }
                    } // if (matHolder != null)    
                } // if (fileInfo[0] instanceof FileInfoNIFTI)
                
                if (fileInfo[0] instanceof FileInfoMGH) {
                	TransMatrix matrix = ((FileInfoMGH)fileInfo[0]).getMatrix();
                	for (j = 0; j < 4; j++) {
	                	if ((orient == FileInfoBase.ORI_R2L_TYPE) || (orient == FileInfoBase.ORI_L2R_TYPE)) {
	                	    matrix.set(0, j, -matrix.get(0, j));	
	                	}
	                	else if ((orient == FileInfoBase.ORI_A2P_TYPE) || (orient == FileInfoBase.ORI_P2A_TYPE)) {
	                	    matrix.set(1, j, -matrix.get(1, j));	
	                	}
	                	else {
	                	    matrix.set(2, j, -matrix.get(2, j));
	                	}
                	}
                	
                	
                	for (i = 0; i < fileInfo.length; i++) {
                		((FileInfoMGH)fileInfo[i]).setMatrix(matrix);
                	}
                }
                
                for (i = 0; i < fileInfo.length; i++) {
                	if (( orient == FileInfoBase.ORI_R2L_TYPE) || (orient == FileInfoBase.ORI_P2A_TYPE)
                            || (orient == FileInfoBase.ORI_I2S_TYPE) || (orient == FileInfoBase.ORI_UNKNOWN_TYPE)) {
                        position = new Vector3f(0, 0, i);
                    } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                        position = new Vector3f(0, 0, -i);
                    }
                	out = new Vector3f(position);
                    MipavCoordinateSystems.fileToScanner(position, out, srcImage);
                    origin = new float[3];
                    origin[0] = out.X;
                    origin[1] = out.Y;
                    origin[2] = out.Z;
                    for (j = 0; j < 3; j++) {
                		if ((fileInfo[0].getAxisOrientation()[j] == FileInfoBase.ORI_R2L_TYPE) ||
                			(fileInfo[0].getAxisOrientation()[j] == FileInfoBase.ORI_L2R_TYPE)) {
                	        fileInfo[i].setOrigin(origin[0],j);
                		}
                		else if ((fileInfo[0].getAxisOrientation()[j] == FileInfoBase.ORI_A2P_TYPE) ||
                		        (fileInfo[0].getAxisOrientation()[j] == FileInfoBase.ORI_P2A_TYPE)) {
                		    fileInfo[i].setOrigin(origin[1], j);
                		}
                		else {
                		    fileInfo[i].setOrigin(origin[2], j);        	
                	    }
                	}
                }
            } // if (changeOrientationOrigin && srcImage.getNDims() > 2)

            if (flipObject == AlgorithmFlip.IMAGE_AND_VOI) {
                VOIVector vec = srcImage.getVOIs();
                Iterator<VOI> vecIter = vec.iterator();

                while (vecIter.hasNext()) {
                    VOI nextVoi = (VOI) vecIter.next();
                    for (i = 0; i < nextVoi.getCurves().size(); i++ )
                    {
                        VOIBase kVOI = nextVoi.getCurves().get(i);
                        for (j = 0; j < kVOI.size(); j++ )
                        {
                            if (flipAxis == X_AXIS) {
                                kVOI.elementAt(i).Y = yDim - kVOI.elementAt(i).Y;
                            }
                            if ( flipAxis == Y_AXIS ) {
                                kVOI.elementAt(i).X = xDim - kVOI.elementAt(i).X;                                
                            }
                            if ( flipAxis == Z_AXIS && (srcImage.getNDims() > 2) ) {
                                //kVOI.elementAt(i).Z = zDim - kVOI.elementAt(i).Z;                
                                int z = (int)kVOI.elementAt(i).Z, direction = (z >= (zDim / 2)) ? -1 : 1;
                                int distance, scope;
                                if (!evenNumberZSlices) {
                                    distance = Math.abs(z - (zDim / 2));
                                    scope = 2 * distance;
                                }
                                else if (evenNumberZSlices && z < zDim/2) {
                                    distance = (zDim/2 - 1 - z);
                                    scope = 2 * distance + 1;
                                }
                                else {
                                    distance = z - zDim/2;
                                    scope = 2 * distance + 1;
                                }                                                              
                                kVOI.elementAt(i).Z = kVOI.elementAt(i).Z +  + (direction * scope);                
                            }
                        }
                    }
                }
            }
            
        } else if (flipObject == AlgorithmFlip.VOI_TYPE) {
            ViewVOIVector vec = srcImage.getVOIs();
            Iterator<VOI> vecIter = vec.iterator();
            
            while (vecIter.hasNext()) {
                VOI nextVoi = (VOI) vecIter.next();
                if (nextVoi.isActive()) {
                    for (i = 0; i < nextVoi.getCurves().size(); i++) {
                        VOIBase base = nextVoi.getCurves().get(i);
                        Iterator<Vector3f> itr = base.iterator();

                        while (itr.hasNext()) {
                            Vector3f point = (Vector3f) itr.next();

                            if (flipAxis == X_AXIS) {
                                point.Y = -point.Y + yDim;
                            }
                            if (flipAxis == Y_AXIS) {
                                point.X = -point.X + xDim;
                            }
                            if (flipAxis ==  Z_AXIS) {
                                int z = (int)point.Z, direction = (z >= (zDim / 2)) ? -1 : 1;
                                int distance, scope;
                                if (!evenNumberZSlices) {
                                    distance = Math.abs(z - (zDim / 2));
                                    scope = 2 * distance;
                                }
                                else if (evenNumberZSlices && z < zDim/2) {
                                    distance = (zDim/2 - 1 - z);
                                    scope = 2 * distance + 1;
                                }
                                else {
                                    distance = z - zDim/2;
                                    scope = 2 * distance + 1;
                                }                                                              
                                point.Z = point.Z +  + (direction * scope);
                            }
                        }
                        base.update();
                    }
                }
            }
        }

        setCompleted(true);
    }
    
    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private class ShapeHolder {

        /** DOCUMENT ME! */
        private boolean isConstructed;

        /** DOCUMENT ME! */
        private ArrayList<Object> shapeList;

        /** DOCUMENT ME! */
        private ArrayList<Integer> sliceList;

        /**
         * Creates a new ShapeHolder object.
         */
        private ShapeHolder() {
            shapeList = new ArrayList<Object>();
            sliceList = new ArrayList<Integer>();
            isConstructed = true;
        }

        /**
         * Creates a new ShapeHolder object.
         *
         * @param  shapeArr     DOCUMENT ME!
         * @param  sliceLocArr  DOCUMENT ME!
         */
        private ShapeHolder(Object[] shapeArr, int[] sliceLocArr) {
            this();

            if (shapeArr.length == sliceLocArr.length) {

                for (int i = 0; i < shapeArr.length; i++) {
                    shapeList.add(shapeArr[i]);
                    sliceList.add(new Integer(sliceLocArr[i]));
                }
            } else {
                isConstructed = false;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param   shape  DOCUMENT ME!
         * @param   slice  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private boolean addShape(Object shape, int slice) {

            if ((shapeList.size() == sliceList.size()) && isConstructed) {
                shapeList.add(shape);
                sliceList.add(new Integer(slice));

                return true;
            } else {
                return false;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param   index  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private Object getShape(int index) {
            return shapeList.get(index);
        }


        /**
         * DOCUMENT ME!
         *
         * @param   slice  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private Object[] getShapesAtSlice(int slice) {
            ArrayList<Object> tempShapes = new ArrayList<Object>();

            for (int i = 0; i < sliceList.size(); i++) {

                if (getSlice(i) == slice) {
                    tempShapes.add(getShape(i));
                }
            }

            Object[] shapesAtSlice = new Object[tempShapes.size()];

            for (int i = 0; i < tempShapes.size(); i++) {
                shapesAtSlice[i] = tempShapes.get(i);
            }

            return shapesAtSlice;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   index  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private int getSlice(int index) {
            Integer tempInt = ((Integer) sliceList.get(index));

            return tempInt.intValue();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private boolean isConstructed() {
            return isConstructed;
        }
    }


}
