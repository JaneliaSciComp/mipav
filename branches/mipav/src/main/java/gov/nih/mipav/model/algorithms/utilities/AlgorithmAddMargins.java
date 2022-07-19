package gov.nih.mipav.model.algorithms.utilities;


import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

/**
 * Algorithm to add or remove margins around the image. Margins are defined in the screen-space coordinates.
 * marginX is screen-space left,right.
 * marginY is screen-space top,bottom.
 * marginZ is screen-space front,back.
 * Margins may be positive to add to the image, or negative to crop the image.
 */
public class AlgorithmAddMargins extends AlgorithmBase {


    /** marginX indicating the number of pixels to be padded on left and the right of X dimension */
    private int[] marginX = new int[]{0,0};

    /** marginY indicating the number of pixels to be padded on top and the bottom of Y dimension */
    private int[] marginY = new int[]{0,0};

    /** marginZ indicating the number of slices to be padded in front and the back of Z dimension*/
    private int[] marginZ = new int[]{0,0};

    /** marginT indicating the number of slices to be padded in time at the start and end of T dimension*/
    private int[] marginT = new int[]{0,0};

    private double[] marginColor = new double[3];


    /**
     * Add or remove margins from the srcImage and store the results in the destImage.
     * @param srcImage original image
     * @param destImage output modified image
     * @param x margin in the screen-space x-direction to add or remove [left,right]
     * @param y margin in the screen-space y-direction to add or remove [top,bottom]
     * @param z margin in the screen-space z-direction to add or remove [front,back]
     */
    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, int[] x, int[] y, int[] z) {
        super(destImage, srcImage);
        marginX = x.clone();
        marginY = y.clone();
        marginZ = z.clone();
        
        if (srcImage.isColorImage()){
            marginColor[0] = srcImage.getMinR();
            marginColor[1] = srcImage.getMinG();
            marginColor[2] = srcImage.getMinB();
        }
        else if (srcImage.isComplexImage()) {
        	marginColor[0] = srcImage.getMin();
        	marginColor[1] = 0.0;
        }
        else {
            marginColor[0] = srcImage.getMin();
        }
    }

    /**
     * Add or remove margins from the srcImage.
     * @param srcImage original image modified by this algorithm.
     * @param x margin in the screen-space x-direction to add or remove [left,right]
     * @param y margin in the screen-space y-direction to add or remove [top,bottom]
     * @param z margin in the screen-space z-direction to add or remove [front,back]
     */
    public AlgorithmAddMargins(ModelImage srcImage, int[] x, int[] y, int[] z) {
        this( srcImage, null, x, y, z );
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        marginX = null;
        marginY = null;
        marginZ = null;
        marginColor = null;
        super.finalize();
    }

    /**
     * Returns the source image.
     * @return source image, may be modified.
     */
    public ModelImage getSrcImage() {
        return srcImage;
    }

    /**
     * Runs the add image margins algorithm.
     */
    public void runAlgorithm() {       
        if (destImage != null) {
            calcStoreInDest();
        } else {
            calcStoreInPlace();
        }
        setCompleted(true);
    }
    
    /**
     * Sets the value for the added margins, for either grayscale or color images.
     * @param value the image values to fill in the added margins.
     */
    public void setPadValue( float[] value )
    {
        for ( int i = 0; i < Math.min( value.length, marginColor.length ); i++ )
        {
            marginColor[i] = value[i];
        }
    }
    
    /**
     * Adds or removes margins in the 4th dimensions.
     * @param tMargin margins in time to add or remove from the image.
     */
    public void setTMargins( int[] tMargin )
    {
        for ( int i = 0; i < Math.min( tMargin.length, marginT.length ); i++ )
        {
            marginT[i] = tMargin[i];
        }
    }


    /**
     * Adds image margins and stores result in destImage.
     */
    private void calcStoreInDest() {
    	int iFactor = 1;
    	if (destImage.isColorImage()) {
    		iFactor = 4;
    	} else if (destImage.isComplexImage()) {
    	    iFactor = 2;	
    	}

        int zDimSrc = ( srcImage.getNDims() > 2 ) ? srcImage.getExtents()[2] : 1;
        int yDimSrc = ( srcImage.getNDims() > 1 ) ? srcImage.getExtents()[1] : 1;
        int xDimSrc = ( srcImage.getNDims() > 0 ) ? srcImage.getExtents()[0] : 1;
        
        int tDim = ( destImage.getNDims() > 3 ) ? destImage.getExtents()[3] : 1;
        int zDim = ( destImage.getNDims() > 2 ) ? destImage.getExtents()[2] : 1;
        int yDim = ( destImage.getNDims() > 1 ) ? destImage.getExtents()[1] : 1;
        int xDim = ( destImage.getNDims() > 0 ) ? destImage.getExtents()[0] : 1;

        int xShiftSrc = -1 * marginX[0]; 
        int yShiftSrc = -1 * marginY[0]; 
        int zShiftSrc = -1 * marginZ[0]; 
        int tShiftSrc = -1 * marginT[0]; 

        int leftBound = ( marginX[0] < 0 ) ? 0 : marginX[0];
        int rightBound = 1;
        if ( destImage.getNDims() > 0 )
        {
            rightBound = ( marginX[1] < 0 ) ? destImage.getExtents()[0] : srcImage.getExtents()[0] + marginX[0];
        }
        
        int topBound = ( marginY[0] < 0 ) ? 0 : marginY[0];
        int bottomBound = 1;
        if ( destImage.getNDims() > 1 )
        {
            bottomBound = ( marginY[1] < 0 ) ? destImage.getExtents()[1] : srcImage.getExtents()[1] + marginY[0];
        }
        
        int frontBound = ( marginZ[0] < 0 ) ? 0 : marginZ[0];
        int backBound = 1;
        if ( destImage.getNDims() > 2 )
        {
            backBound = ( marginZ[1] < 0 ) ? destImage.getExtents()[2] : srcImage.getExtents()[2] + marginZ[0];
        }

        
        int timeStartBound = ( marginT[0] < 0 ) ? 0 : marginT[0];
        int timeEndBound = 1;
        if ( destImage.getNDims() > 3 )
        {
            timeEndBound = ( marginT[1] < 0 ) ? destImage.getExtents()[3] : srcImage.getExtents()[3] + marginT[0];
        }
                
        for ( int t = 0; t < tDim; t++ )
        {
            for ( int z = 0; z < zDim; z++ )
            {
                for ( int y = 0; y < yDim; y++ )
                {
                    for ( int x = 0; x < xDim; x++ )
                    {
                        int destIndex = t * (zDim * yDim * xDim) + z * (yDim * xDim) + y * xDim + x;
                        if ( (x >= leftBound) && (x < rightBound) &&
                             (y >= topBound) && (y < bottomBound) &&
                             (z >= frontBound) && (z < backBound) &&
                             (t >= timeStartBound) && (t < timeEndBound)    )
                        {
                            int srcIndex = (t+tShiftSrc) * (zDimSrc * yDimSrc * xDimSrc) + 
                            (z+zShiftSrc) * (yDimSrc * xDimSrc) + 
                            (y+yShiftSrc) * xDimSrc + (x + xShiftSrc);
                            if ( iFactor == 1 )
                            {
                                destImage.set( destIndex, srcImage.get(srcIndex) );
                            }
                            else if (iFactor == 2) {
                            	destImage.set(destIndex * 2, srcImage.get(srcIndex * 2));
                            	destImage.set(destIndex * 2 + 1, srcImage.get(srcIndex * 2 + 1));
                            }
                            else
                            {
                                destImage.set( destIndex * 4,  srcImage.get(srcIndex * 4) );
                                destImage.set( destIndex * 4 + 1,  srcImage.get(srcIndex * 4 + 1) );
                                destImage.set( destIndex * 4 + 2,  srcImage.get(srcIndex * 4 + 2) );
                                destImage.set( destIndex * 4 + 3,  srcImage.get(srcIndex * 4 + 3) );
                            }     
                        }
                        else
                        {
                            if ( iFactor == 1 )
                            {
                                destImage.set( destIndex,  marginColor[0] );
                            }
                            else if (iFactor == 2) {
                            	destImage.set(destIndex * 2, marginColor[0]);
                            	destImage.set(destIndex * 2 + 1, marginColor[1]);
                            }
                            else
                            {
                                destImage.set( destIndex * 4,  1 );
                                destImage.set( destIndex * 4 + 1,  marginColor[0] );
                                destImage.set( destIndex * 4 + 2,  marginColor[1] );
                                destImage.set( destIndex * 4 + 3,  marginColor[2] );
                            }                            
                        }
                    }
                }
            }
        }
        destImage.calcMinMax();
        updateFileInfo( srcImage, destImage );
        destImage.setVOIs( srcImage.getVOIsCopy() );

        if ( destImage.getVOIs() != null )
        {
            for ( int i = 0; i < destImage.getVOIs().size(); i++ )
            {
                Vector<VOIBase> curves = destImage.getVOIs().elementAt(i).getCurves();
                for ( int j = 0; j < curves.size(); j++ )
                {
                    VOIBase contour = curves.elementAt(j);
                    for ( int k = 0; k < contour.size(); k++ )
                    {
                        Vector3f kIn = contour.elementAt(k);
                        kIn.X = Math.min( xDim, Math.max( 0, kIn.X - xShiftSrc ) );
                        kIn.Y = Math.min( yDim, Math.max( 0, kIn.Y - yShiftSrc ) );
                        kIn.Z = Math.min( zDim - 1, Math.max( 0, kIn.Z - zShiftSrc ) );
                    }
                    contour.update();
                }
            }
        }

    }

    /**
     * Adds image margins and stores result in srcImage Must use getSrcImage after running this routine.
     */
    private void calcStoreInPlace() 
    {
        int[] destExtents = new int[srcImage.getNDims()];
        for ( int i = 0; i < srcImage.getNDims(); i++ )
        {
            destExtents[i] = srcImage.getExtents()[i];
        }
        if ( destExtents.length > 0 )
        {
            destExtents[0] += (marginX[0] + marginX[1]);
        }
        if ( destExtents.length > 1 )
        {
            destExtents[1] += (marginY[0] + marginY[1]);
        }
        if ( destExtents.length > 2 )
        {
            destExtents[2] += (marginZ[0] + marginZ[1]);
        }
        destImage = new ModelImage(srcImage.getType(), destExtents, srcImage.getImageName() );
        calcStoreInDest();
        
        if (srcImage.getParentFrame() != null)
        {
            srcImage.getParentFrame().close();
        }
        srcImage.disposeLocal();
        srcImage = null;
        
        srcImage = destImage;
    }

    /**
     * Updates the fileInfo values for the kWrite image, uses the fileInfo values from kRead.
     * @param kRead image to read fileInfo values from
     * @param kWrite image to write fileInfo values to.
     */
    private void updateFileInfo(ModelImage kRead, ModelImage kWrite)
    {
        int[] marginVector = new int[3];
        marginVector[0] = marginX[0];
        marginVector[1] = marginY[0];
        marginVector[2] = marginZ[0];

        if (kRead.getNDims() == 2) {
            float[] newOrigin = calculateNewOrigin(kRead, marginVector);

            // FILE INFO: add the file info    (if the original is a DICOM image, do a special file info...)
            fireProgressStateChanged("Updating File Info...");

            if ((kRead.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                FileInfoDicom fileInfoBuffer; // buffer of type DICOM
                fileInfoBuffer = (FileInfoDicom) kRead.getFileInfo(0).clone(); // copy into buffer
                fileInfoBuffer.setExtents(kWrite.getExtents());

                String stringForDicom = Float.toString(newOrigin[0]) + "\\" + Float.toString(newOrigin[1]) +
                "\\" + Float.toString(newOrigin[2]);
                fileInfoBuffer.getTagTable().setValue("0020,0032", stringForDicom, stringForDicom.length());
                fileInfoBuffer.setOrigin(newOrigin);
                kWrite.setFileInfo(fileInfoBuffer, 0);

                // set image rows ("0028,0010")
                stringForDicom = String.valueOf(kWrite.getExtents()[1]);
                fileInfoBuffer.getTagTable().setValue("0028,0010", stringForDicom);

                // set image columns ("0028,0011")
                stringForDicom = String.valueOf(kWrite.getExtents()[0]);
                fileInfoBuffer.getTagTable().setValue("0028,0011", stringForDicom);

            } else { // not a DICOM image,
                FileInfoBase fileInfoBuffer; // buffer of any old type
                fileInfoBuffer = (FileInfoBase) kRead.getFileInfo(0).clone();
                fileInfoBuffer.setOrigin(newOrigin);
                fileInfoBuffer.setExtents(kWrite.getExtents()); // SET extents for the destination
                kWrite.setFileInfo(fileInfoBuffer, 0);
            }
        }
        else
        {

            int srcT = (kRead.getNDims() == 4) ? kRead.getExtents()[3] : 1;
            int destT = (kWrite.getNDims() == 4) ? kRead.getExtents()[3] : 1;
            
            int srcDepth = (kRead.getNDims() == 3) ? kRead.getExtents()[2] : 1;
            int destDepth = (kWrite.getNDims() == 3) ? kWrite.getExtents()[2] : 1;

            FileInfoDicom[] fileInfoDicomBuffer = null; // buffer of type DICOM
            FileInfoBase[] fileInfoBuffer = null; // buffer of any old type
            if ((kRead.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                fileInfoDicomBuffer = new FileInfoDicom[destDepth * destT];
            } else {
                fileInfoBuffer = new FileInfoBase[destDepth * destT];
            }
                        
            float[] newOrigin = calculateNewOrigin(kRead, marginVector);
            float delta = kRead.getFileInfo()[0].getResolutions()[2];
            int axisOrient = kRead.getFileInfo()[0].getAxisOrientation(2);
            if ((axisOrient != FileInfoBase.ORI_A2P_TYPE) && (axisOrient != FileInfoBase.ORI_R2L_TYPE) &&
                    (axisOrient != FileInfoBase.ORI_I2S_TYPE)) {
                delta = -delta;
            }
            float startLoc = newOrigin[2];

            // insert margin values into the blank slices
            int tRead = 0;
            for (int tWrite = 0; (tWrite < destT) && !threadStopped; tWrite++) {
                int zRead = 0;
                for (int zWrite = 0; (zWrite < destDepth) && !threadStopped; zWrite++) {

                    if ((destT == 1)) {
                        fireProgressStateChanged(Math.round((float) (zWrite) / destDepth * 100));
                    }

                    // DICOM
                    if ((kRead.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        FileInfoDicom kCopy = (FileInfoDicom) kRead.getFileInfo(tRead*srcDepth + zRead).clone();
                        kCopy.setExtents(kWrite.getExtents());

                        // change the slice number ("0020,0013"):
                        // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                        // Reset the image (slice) number with the new number ordering
                        String stringForDicom = Integer.toString(zWrite + 1);
                        kCopy.getTagTable().setValue("0020,0013", stringForDicom, stringForDicom.length());

                        newOrigin[2] = startLoc + (delta * zWrite);

                        stringForDicom = Float.toString(newOrigin[0]) + "\\" + Float.toString(newOrigin[1]) + "\\" + Float.toString(newOrigin[2]);
                        kCopy.getTagTable().setValue("0020,0032", stringForDicom, stringForDicom.length());
                        kCopy.setOrigin(newOrigin);

                        // readjust the slice location ("0020,1041")
                        stringForDicom = String.valueOf(newOrigin[2]);
                        kCopy.getTagTable().setValue("0020,1041", stringForDicom, stringForDicom.length());

                        // set image columns ("0028,0011")
                        kCopy.getTagTable().setValue("0028,0010", new Short((short) kCopy.getExtents()[1]), 2);
                        kCopy.getTagTable().setValue("0028,0011", new Short((short) kCopy.getExtents()[0]), 2);
                        
                        
                        fileInfoDicomBuffer[tWrite*destDepth + zWrite] = kCopy;
                    } else { // NOT DICOM
                        FileInfoBase kCopy = (FileInfoBase) kRead.getFileInfo((tRead*srcDepth) + zRead).clone();
                        kCopy.setOrigin(newOrigin);
                        kCopy.setExtents(kWrite.getExtents());
                        fileInfoBuffer[tWrite*destDepth + zWrite] = kCopy;
                    }

                    if (!((zWrite < marginZ[0]) || (zWrite >= (srcDepth + marginZ[0] - 1)))) {

                        /* While the destImage slice offset is outside the range of the srcImage image,
                         * dont update the srcImage counter.  This way: For new slices before the start of the
                         * original image set, copy the first FileInfoBuffer and modify. For new slices that
                         * correspond to an existing image, copy that FileInfoBuffer and modify. For new slices
                         * after the end of the original image set, copy the last FileInfoBuffer and modify.
                         */
                        zRead++; // goto the next slice in the source image
                    }
                }

                if (!((tWrite < marginT[0]) || (tWrite >= (srcT + marginT[0] - 1)))) {
                    tRead++;
                }
            }

            for (int t = 0; t < destT; t++) {
                for (int Z = 0; Z < destDepth; Z++) {
                    if (fileInfoBuffer == null) {
                        kWrite.setFileInfo(fileInfoDicomBuffer[(t * destDepth) + Z], ((t * destDepth) + Z));
                    } else {
                        kWrite.setFileInfo(fileInfoBuffer[(t * destDepth) + Z], ((t * destDepth) + Z));
                    }
                }
            }
        }
    }
    

    /**
     * Calculates the origin values for the modified image, based on the current origin values and the added margins.
     * @param srcImg original image
     * @param margin margins added or removed from the image
     * @return new origins.
     */
    private float[] calculateNewOrigin(ModelImage srcImg, int[] margin) {

        FileInfoBase fileInfoBuffer = srcImg.getFileInfo()[0];

        float[] origin;
        
        if ((fileInfoBuffer.getFileFormat() == FileUtility.DICOM) && 
           (((FileInfoDicom)fileInfoBuffer).getTagTable().getValue("0020,0032") != null)) {
            FileInfoDicom fileDicom = (FileInfoDicom) fileInfoBuffer;
            origin = convertIntoFloat(fileDicom.parseTagValue("0020,0032"));
        } else {
            origin = fileInfoBuffer.getOrigin().clone();
        }

        for (int i = 0; i < Math.min( 3,srcImg.getNDims() ); i++) {
            int axisOrient = fileInfoBuffer.getAxisOrientation(i);

            if ((axisOrient == FileInfoBase.ORI_A2P_TYPE) || (axisOrient == FileInfoBase.ORI_R2L_TYPE) ||
                    (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                origin[i] = origin[i] - (srcImg.getResolutions(0)[i] * margin[i]);
            } else {
                origin[i] = origin[i] + (srcImg.getResolutions(0)[i] * margin[i]);
            }
        }

        return origin;
    }

}
