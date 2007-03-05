package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;

import java.awt.Polygon;
import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeMap;


/**
 * Flips 2D, 3D or 4D grays scale or color dataset about X, Y, or Z axis (when applicable) when AlgorithmFlip.IMAGE is passed to
 * the constructor.  An option is given to flip all VOIs at this time.  When 
 * AlgorithmFlip.VOI is passed, only the selected VOI is flipped about the specified axis.
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
    
    /** Image and all VOIs should be flipped */
    public static final int IMAGE_AND_VOI = 2;
    
    /** Denotes image should be flipped without VOI */
    public static final int IMAGE = 0;
    
    /** Denotes selected VOI should be flipped */
    public static final int VOI = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Axis to flip along. */
    private int flipAxis = Y_AXIS;
    
    /** Type of object to flip */
    private int flipObject;
    
    /** Whether all VOIs shouold be flipped when the image is flipped */
    
    private boolean flipVoiWithImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    
    /**
     * Flips 2D, 3D or 4D grays scale or color dataset about X or Y axis.
     *
     * @param  srcImg    source image model
     * @param  flipMode  flip about which axis
     */
    public AlgorithmFlip(ModelImage srcImg, int flipMode, int flipObject) {
        super(null, srcImg);
        this.flipObject = flipObject;
        if ((flipMode == Y_AXIS) || (flipMode == X_AXIS) || (flipMode == Z_AXIS)) {
            flipAxis = flipMode;
        } else {
            flipAxis = Y_AXIS;
        }
        flipVoiWithImage = false;
    }

    /**
     * Flips 2D, 3D or 4D grays scale or color dataset about X or Y axis.
     *
     * @param  srcImg    source image model
     * @param  flipMode  flip about which axis
     * @param  progress  mode of progress bar (see AlgorithmBase)
     */
    public AlgorithmFlip(ModelImage srcImg, int flipMode, int progress, int flipObject) {
        this(srcImg, flipMode, flipObject);
      //  progressMode = progress;
    }
    
    

    //~ Methods --------------------------------------------------------------------------------------------------------

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

        constructLog();

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
        boolean logMagDisplay = false;

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
        int tDim = 1;
        int volume = 1;
        int zDim = 1;
        int xDim = 1;
        int yDim = 1;
        if(srcImage.getNDims() > 1) {
            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];
        }
        if (srcImage.getNDims() == 4) {
            zDim = srcImage.getExtents()[2];
            tDim = srcImage.getExtents()[3];
            volume = slice * zDim;
        }
        else if (srcImage.getNDims() == 3) {            
            zDim = srcImage.getExtents()[2];
        }
        if(flipObject == AlgorithmFlip.IMAGE || flipObject == AlgorithmFlip.IMAGE_AND_VOI)
        {
            
    
            /* If flipping the z-axis, then loop 1/2 the times and swap the z slices... */
            if ( index == 2 )
            {
                zDim /= 2;
                sliceBufferTemp = new float[slice];
            }
    
            /* For each slice: */
            for (int t = 0; (t < tDim) && !threadStopped; t++) {
                for (int z = 0; (z < zDim) && !threadStopped; z++) {
                    if ((nImages > 1) && (((t * zDim + z) % mod) == 0)) {
                        fireProgressStateChanged(Math.round((float) (t * zDim + z) / (nImages - 1) * 100));
                    }
    
                    try {
                        srcImage.export( axisOrder, axisFlip, t, z, sliceBuffer);
                        if ( index == 2 )
                        {
                            int zTemp = (srcImage.getExtents()[2] - 1 - z);
                            srcImage.export( axisOrder, axisFlip, t, zTemp, sliceBufferTemp);
                            srcImage.importData(t * volume + zTemp * slice, sliceBufferTemp, false);
                        }
                        srcImage.importData(t * volume + z * slice, sliceBuffer, false);
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
            FileInfoBase[] fileInfo = srcImage.getFileInfo();
            float loc = fileInfo[0].getOrigin(index);
            int orient = fileInfo[0].getAxisOrientation(index);
    
            if ( loc > 0.0f ) {
                loc = loc - ((fileInfo[0].getExtents()[index] - 1) * fileInfo[0].getResolutions()[index]);
            } else {
                loc = loc + ((fileInfo[0].getExtents()[index] - 1) * fileInfo[0].getResolutions()[index]);
            }
            orient = FileInfoBase.oppositeOrient(orient);
            for (int i = 0; i < fileInfo.length; i++) {
                fileInfo[i].setAxisOrientation(orient, index);
                fileInfo[i].setOrigin(loc, index);
                if ( index == 2 )
                {
                    fileInfo[i].setOrigin( loc + (fileInfo[0].getResolutions()[index] * i), index);
                }
            }
            if(flipObject == AlgorithmFlip.IMAGE_AND_VOI) {
                VOIVector vec = srcImage.getVOIs();
                Iterator vecIter = vec.iterator();
                if(flipAxis == X_AXIS) {
                    while(vecIter.hasNext()) {              
                        VOI nextVoi = (VOI)vecIter.next();
                        for(int i=0; i<zDim; i++) {
                            Polygon[] polyList = nextVoi.exportPolygons(i);
                            nextVoi.removeCurves(i);
                            for(int j=0; j<polyList.length; j++) {
                                Polygon poly = polyList[j];
                                int[] points = poly.ypoints;
                                for(int k=0; k<points.length; k++) {
                                    points[k] = -points[k] + yDim;                                
                                }
                                nextVoi.importPolygon(poly, i); 
                            }
                        }
                    }
                }
                if(flipAxis == Y_AXIS) {
                    while(vecIter.hasNext()) {              
                        VOI nextVoi = (VOI)vecIter.next();
                        for(int i=0; i<zDim; i++) {
                            Polygon[] polyList = nextVoi.exportPolygons(i);
                            nextVoi.removeCurves(i);
                            for(int j=0; j<polyList.length; j++) {
                                Polygon poly = polyList[j];
                                int[] points = poly.xpoints;
                                for(int k=0; k<points.length; k++) {
                                    points[k] = -points[k] + xDim;                                
                                }
                                nextVoi.importPolygon(poly, i); 
                            }
                        }
                    }
                }
                if(flipAxis == Z_AXIS && srcImage.getNDims() > 2)
                {
                    while(vecIter.hasNext()) {              
                        VOI nextVoi = (VOI)vecIter.next();
                        ArrayList[] polyArray = new ArrayList[zDim];
                        for(int i=0; i<zDim; i++)
                            polyArray[i] = new ArrayList();
                        for(int i=0; i<zDim; i++) {
                            Polygon[] polyList = nextVoi.exportPolygons(i);
                            nextVoi.removeCurves(i);
                            for(int j=0; j<polyList.length; j++) {
                                Polygon poly = polyList[j];  
                                polyArray[-i + zDim].add(poly);   
                            }    
                        }
                        for(int i=0; i<zDim; i++) {
                            ArrayList tempArray = polyArray[i];
                            for(int j=0; j<tempArray.size(); j++) {
                                nextVoi.importPolygon((Polygon)tempArray.get(j), i);
                            }
                        }
                    }
                }
            }
        }
        else if(flipObject == AlgorithmFlip.VOI)
        {
            boolean activeVoi = false;
            VOIVector vec = srcImage.getVOIs();
            Iterator vecIter = vec.iterator();
            if(flipAxis == X_AXIS) {
                while(vecIter.hasNext()) {              
                    VOI nextVoi = (VOI)vecIter.next();
                    if(nextVoi.isActive())
                    {
                        activeVoi = true;
                        for(int i=0; i<zDim; i++) {
                            VOIBase base = nextVoi.getActiveContour(i);
                            if(base != null)
                            {
                                Iterator itr = base.iterator();
                                while(itr.hasNext())
                                {
                                    Point3Df point = (Point3Df)itr.next();
                                    point.y = -point.y + yDim;
                                }
                            }
                        }
                    }
                }
            }
            if(flipAxis == Y_AXIS) {
                while(vecIter.hasNext()) { 
                    VOI nextVoi = (VOI)vecIter.next();
                    if(nextVoi.isActive())
                    {
                        activeVoi = true;
                        for(int i=0; i<zDim; i++) {
                            VOIBase base = nextVoi.getActiveContour(i);
                            if(base != null)
                            {
                                Iterator itr = base.iterator();
                                while(itr.hasNext())
                                {
                                    Point3Df point = (Point3Df)itr.next();
                                    point.x = -point.x + xDim;
                                }
                            }
                        }
                    }
                }
            }
            if(flipAxis == Z_AXIS && srcImage.getNDims() > 2)
            {
                while(vecIter.hasNext()) {              
                    VOI nextVoi = (VOI)vecIter.next();
                    if(nextVoi.isActive())
                    {
                        activeVoi = true;
                        for(int i=0; i<zDim; i++) {
                            VOIBase base = nextVoi.getActiveContour(i);
                            if(base != null)
                            {
                                Iterator itr = base.iterator();
                                while(itr.hasNext())
                                {
                                    Point3Df point = (Point3Df)itr.next();
                                    point.z = -point.z + zDim;
                                }
                            }
                        }
                    }
                }
            }
            if(!activeVoi) {
                MipavUtil.displayError("Select a VOI to flip.");
            }
        }
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {

        if (flipAxis == Y_AXIS) {
            historyString = new String("Flip(Y_AXIS)\n");
        } else if (flipAxis == X_AXIS) {
            historyString = new String("Flip(X_AXIS)\n");
        } else {
            historyString = new String("Flip(Z_AXIS)\n");
        }
        if(flipObject == IMAGE) {
        	historyString = historyString+"Object(IMAGE)\n";
       	} else if(flipObject == VOI) {
       		historyString = historyString+"Object(VOI)\n";			
       	} else {
       		historyString = historyString+"Object(IMAGE_AND_VOI)\n";
       	}
    }

}
