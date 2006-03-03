package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.file.*;

import java.io.*;


/**
 *      Swaps third and fourth dimensions in 4D dataset.  The current image is deleted and
 *      a new one with swapped third and fourth dimensions is created.
 */
public class AlgorithmSwap34 extends AlgorithmBase {

    /**
     *   Constructs new algorithm and sets source.
     *   @param srcImg   source image model
     */
    public AlgorithmSwap34( ModelImage srcImg ) {
        super( null, srcImg );
    }

    /**
     *   Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     *   Constructs a string of the contruction parameters and
     *   outputs the string to the messsage frame if the logging
     *   procedure is turned on.
     */
    private void constructLog() {
        historyString = new String( "Swap34()\n" );
    }

    /**
     *   Starts the program.
     */
    public void runAlgorithm() {
        if ( srcImage == null ) {
            displayError( "Source Image is null" );
            return;
        }
        constructLog();
        swap34();
    }

    /**
     *   Swaps the image third and fourth dimensions and replaces the source image
     *   with the swapped dimension image.
     */
    private void swap34() {

        int xy, index, t, z;
        int length;
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        int sliceSize;
        int colorFactor;
        FileInfoBase fileInfo[], fileInfoR[];
        int[] extents;
        int[] units;
        float[] resolutions;
        int tempi;
        float tempf;
        int bufferType;
        ViewUserInterface ui;
        int modality;
        String fileDir;
        int dataType;
        boolean endianess;
        double min, max;
        Short pixelPadValue;
        short photometric;
        int[] axis;
        int imageOrientation;
        float[] startLocs;
        float sliceSpacing;
        TransMatrix xfrm;

        int transformID;

        try {
            if ( srcImage.isColorImage() ) {
                colorFactor = 4;
            } else {
                colorFactor = 1;
            }

            sliceSize = xDim * yDim * colorFactor;
            length = colorFactor * xDim * yDim * zDim * tDim;
            buffer = new float[length];
            resultBuffer = new float[length];
            buildProgressBar( srcImage.getImageName(), "Swapping Dimensions 3-4 ...", 0, 100 );
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            resultBuffer = null;
            System.gc();
            displayError( "AlgorithmSwap34: Out of memory" );
            setCompleted( false );
            disposeProgressBar();
            return;
        }

        int mod = length / 100; // mod is 1 percent of length
        initProgressBar();

        try {
            srcImage.exportData( 0, length, buffer );
        } catch ( IOException error ) {
            displayError( "AlgorithmSwap34: Image locked" );
            buffer = null;
            resultBuffer = null;
            setCompleted( false );
            disposeProgressBar();
            return;
        }

        // The third and fourth dimensions are swapped in going from buffer to resultBuffer.
        // xy iterates through the first and second dimensions in one combined step.
        for ( t = 0; t < tDim && !threadStopped; t++ ) {
            for ( z = 0; z < zDim && !threadStopped; z++ ) {
                for ( xy = 0; xy < sliceSize && !threadStopped; xy++ ) {
                    index = ( t * zDim * sliceSize + z * sliceSize + xy );
                    if ( index % mod == 0 && isProgressBarVisible() ) {
                        progressBar.updateValue( Math.round( (float) ( index ) / ( length - 1 ) * 100 ), activeImage );
                    }

                    resultBuffer[z * tDim * sliceSize + t * sliceSize + xy] = buffer[index];
                }
            }
        }
        if ( threadStopped ) {
            buffer = null;
            resultBuffer = null;
            finalize();
            return;
        }

        // The third and fourth dimensions are swapped in extents, units, and resolutions.
        extents = new int[] { xDim, yDim, tDim, zDim };
        fileInfo = srcImage.getFileInfo();
        xfrm = srcImage.getMatrix();
        units = fileInfo[0].getUnitsOfMeasure();
        tempi = units[2];
        units[2] = units[3];
        units[3] = tempi;
        resolutions = fileInfo[0].getResolutions();
        tempf = resolutions[2];
        resolutions[2] = resolutions[3];
        resolutions[3] = tempf;
        bufferType = srcImage.getType();
        ui = srcImage.getUserInterface();
        modality = fileInfo[0].getModality();
        fileDir = fileInfo[0].getFileDirectory();
        dataType = fileInfo[0].getDataType();
        endianess = fileInfo[0].getEndianess();
        max = fileInfo[0].getMax();
        min = fileInfo[0].getMin();
        pixelPadValue = fileInfo[0].getPixelPadValue();
        photometric = fileInfo[0].getPhotometric();
        axis = fileInfo[0].getAxisOrientation();
        imageOrientation = fileInfo[0].getImageOrientation();
        sliceSpacing = fileInfo[0].getSliceSpacing();

        transformID = fileInfo[0].getTransformID();

        String name = JDialogBase.makeImageName( srcImage.getImageName(), "_result" );

        srcImage = null;
        destImage = new ModelImage( bufferType, extents, name, ui );
        fileInfoR = destImage.getFileInfo();
        for ( int i = 0; i < zDim * tDim; i++ ) {
            startLocs = fileInfo[i].getOrigin();
            tempf = startLocs[2];
            startLocs[2] = startLocs[3];
            startLocs[3] = tempf;
            fileInfoR[i].setOrigin( startLocs );
            fileInfoR[i].setImageOrientation( imageOrientation );
            fileInfoR[i].setAxisOrientation( axis );
            fileInfoR[i].setModality( modality );
            fileInfoR[i].setFileDirectory( fileDir );
            fileInfoR[i].setDataType( dataType );
            fileInfoR[i].setEndianess( endianess );
            fileInfoR[i].setUnitsOfMeasure( units );
            fileInfoR[i].setResolutions( resolutions );
            fileInfoR[i].setSliceSpacing( sliceSpacing );
            fileInfoR[i].setExtents( extents );
            fileInfoR[i].setMax( max );
            fileInfoR[i].setMin( min );
            fileInfoR[i].setPixelPadValue( pixelPadValue );
            fileInfoR[i].setPhotometric( photometric );
            fileInfoR[i].setTransformID(transformID);
            if ( fileInfoR[i] instanceof FileInfoXML ) {
                ( (FileInfoImageXML) ( fileInfoR[i] ) ).setMatrix( xfrm );
            }

        }

        destImage.setMatrix( xfrm );
        try {
            destImage.importData( 0, resultBuffer, true );
        } catch ( IOException error ) {
            displayError( "AlgorithmSwap34: Image(s) locked" );
            setCompleted( false );
            disposeProgressBar();
            return;
        } catch ( OutOfMemoryError e ) {
            System.gc();
            displayError( "AlgorithmSwap34: Out of memory" );
            setCompleted( false );
            disposeProgressBar();
            return;
        }

        disposeProgressBar();
        setCompleted( true );
    }

    /**
     *    Returns result image.
     *    @return destImage
     */
    public ModelImage getResultImage() {
        return destImage;
    }

}
