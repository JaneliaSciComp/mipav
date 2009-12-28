package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

/**
 * Algorithm to place an image into the center of a larger image, as if the image was given margins or a border around
 * its outside.
 */
public class AlgorithmAddMargins extends AlgorithmBase {


    /** xBounds indicating the number of pixels to be padded on left and the total length of X dimension */
    private int[] marginX = new int[]{0,0};

    /** yBounds indicating the number of pixels to be padded on top and the total length of Y dimension */
    private int[] marginY = new int[]{0,0};

    /** xBounds indicating the number of slices to be padded in front and the total length of Z dimension*/
    private int[] marginZ = new int[]{0,0};

    private double[] marginColor = new double[3];


    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, int[] x, int[] y, int[] z) {
        super(destImage, srcImage);
        marginX = x.clone();
        marginY = y.clone();
        marginZ = z.clone();
        
        if ( !srcImage.isColorImage() )
        {
            marginColor[0] = srcImage.getMin();
        }
        else
        {
            marginColor[0] = srcImage.getMinR();
            marginColor[1] = srcImage.getMinG();
            marginColor[2] = srcImage.getMinB();
        }
    }

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
     * Accessor returns srcImage.
     *
     * @return  DOCUMENT ME!
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



    
    public void setPadValue( float[] value )
    {
        for ( int i = 0; i < Math.min( value.length, marginColor.length ); i++ )
        {
            marginColor[i] = value[i];
        }
    }


    /**
     * Adds image margins and stores result in destImage.
     */
    private void calcStoreInDest() {
        int iColorFactor = destImage.isColorImage() ? 4 : 1;

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
                             (z >= frontBound) && (z < backBound)    )
                        {
                            int srcIndex = t * (zDimSrc * yDimSrc * xDimSrc) + 
                            (z+zShiftSrc) * (yDimSrc * xDimSrc) + 
                            (y+yShiftSrc) * xDimSrc + (x + xShiftSrc);
                            if ( iColorFactor == 1 )
                            {
                                destImage.set( destIndex, srcImage.get(srcIndex) );
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
                            if ( iColorFactor == 1 )
                            {
                                destImage.set( destIndex,  marginColor[0] );
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
                stringForDicom = String.valueOf(kWrite.getExtents()[0]);
                fileInfoBuffer.getTagTable().setValue("0028,0010", stringForDicom);

                // set image columns ("0028,0011")
                stringForDicom = String.valueOf(kWrite.getExtents()[1]);
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

            int tDim = (kRead.getNDims() == 4) ? kRead.getExtents()[3] : 1;
            // FILE INFO: add the file info for 3D images
            if ((tDim == 1)) {
                fireProgressStateChanged("Updating File Info...");
                // int fillLength = Math.round((float)z/destDepth); int piece = (1 - fillLength);
            }

            int srcDepth = kRead.getExtents()[2];
            int destDepth = kWrite.getExtents()[2];

            FileInfoDicom[] fileInfoDicomBuffer = null; // buffer of type DICOM
            FileInfoBase[] fileInfoBuffer = null; // buffer of any old type
            if ((kRead.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                fileInfoDicomBuffer = new FileInfoDicom[destDepth * tDim];
            } else {
                fileInfoBuffer = new FileInfoBase[destDepth * tDim];
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
            for (int t = 0; (t < tDim) && !threadStopped; t++) {
                int z = 0;
                for (int Z = 0; (Z < destDepth) && !threadStopped; Z++) {

                    if ((tDim == 1)) {
                        fireProgressStateChanged(Math.round((float) (Z) / destDepth * 100));
                    }

                    // DICOM
                    if ((kRead.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        fileInfoDicomBuffer[Z] = (FileInfoDicom) kRead.getFileInfo(z).clone();

                        fileInfoDicomBuffer[Z].setExtents(kWrite.getExtents()); // modify extents to use the extents
                        // of destImage img

                        // change the slice number ("0020,0013"):
                        // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                        // Reset the image (slice) number with the new number ordering
                        String stringForDicom = Integer.toString(Z + 1);
                        fileInfoDicomBuffer[Z].getTagTable().setValue("0020,0013", stringForDicom,
                                stringForDicom.length());

                        newOrigin[2] = startLoc + (delta * Z);

                        stringForDicom = Float.toString(newOrigin[0]) + "\\" +
                        Float.toString(newOrigin[1]) + "\\" +
                        Float.toString(newOrigin[2]);
                        fileInfoDicomBuffer[Z].getTagTable().setValue("0020,0032", stringForDicom,
                                stringForDicom.length());
                        fileInfoDicomBuffer[Z].setOrigin(newOrigin);

                        // readjust the slice location ("0020,1041")
                        stringForDicom = String.valueOf(newOrigin[2]);
                            fileInfoDicomBuffer[Z].getTagTable().setValue("0020,1041", stringForDicom,
                                stringForDicom.length());

                        // set image columns ("0028,0011")
                        // stringForDicom = String.valueOf(destImage.getExtents()[0]);
                        // fileInfoBuffer.setValue("0028,0011", stringForDicom);
                        fileInfoDicomBuffer[Z].getTagTable().setValue("0028,0010",
                                new Short((short) fileInfoDicomBuffer[Z].getExtents()[1]),
                                2);
                        fileInfoDicomBuffer[Z].getTagTable().setValue("0028,0011",
                                new Short((short) fileInfoDicomBuffer[Z].getExtents()[0]),
                                2);
                    } else { // NOT DICOM
                        fileInfoBuffer[Z] = (FileInfoBase) kRead.getFileInfo((t * srcDepth) + z).clone();
                        fileInfoBuffer[Z].setOrigin(newOrigin);
                        fileInfoBuffer[Z].setExtents(kWrite.getExtents());
                    }

                    if (!((Z < marginZ[0]) || (Z >= (srcDepth + marginZ[0] - 1)))) {

                        /* While the destImage slice offset is outside the range of the srcImage image,
                         * dont update the srcImage counter.  This way: For new slices before the start of the
                         * original image set, copy the first FileInfoBuffer and modify. For new slices that
                         * correspond to an existing image, copy that FileInfoBuffer and modify. For new slices
                         * after the end of the original image set, copy the last FileInfoBuffer and modify.
                         */
                        z++; // goto the next slice in the source image
                    }
                }
            }

            for (int t = 0; t < tDim; t++) {
                for (int Z = 0; Z < destDepth; Z++) {
                    if (fileInfoBuffer == null) {
                        kWrite.setFileInfo(fileInfoDicomBuffer[(t * destDepth) + Z], ((t * destDepth) + Z));
                    } else {
                        kWrite.setFileInfo(fileInfoBuffer[(t * destDepth) + Z], ((t * destDepth) + Z));
                    }
                }
            } // for (t = 0; t < tDim; t++)
        }
    }
    
    /**
     * Switch origin order from LPS order to Img order.
     *
     * @param   srcImg  DOCUMENT ME!
     * @param   margin  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
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

        for (int i = 0; i < Math.min(3, srcImg.getNDims()); i++) {
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
