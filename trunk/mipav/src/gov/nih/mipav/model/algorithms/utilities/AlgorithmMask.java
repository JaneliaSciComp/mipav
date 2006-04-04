package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.io.*;
import java.util.*;


/**
 *  Allow the user to fill a VOI or fill everything outside the
 *  VOI with a specific fill value.
 *
 *		@version 0.1 Feb 11, 1998
 *		@author Matthew J. McAuliffe, Ph.D.
 *
 */
public class AlgorithmMask extends AlgorithmBase {

    /**
     *   Set the mask areas to this value.
     */
    private float imageFill = 0.0f;
    private float imageFillR = 0.0f;
    private float imageFillG = 0.0f;
    private float imageFillB = 0.0f;

    /**
     *    if true then fill inside the VOI. If false then fill outside
     */
    private boolean polarity = true;

    /**
     *
     *   @param destImg  image model where result image is to stored
     *   @param srcImg   source image model
     *   @param fill     value used to fill a region
     *   @param polarity flag indicating fill location, true = fill inside; false fill outside
     *   @param useVOI   use the VOI to define the mask area else it will use the painted area to define the mask.
     */
    public AlgorithmMask( ModelImage destImg, ModelImage srcImg, float fill, boolean polarity, boolean useVOI ) {

        super( destImg, srcImg );
        imageFill = fill;
        this.polarity = polarity;

        if (useVOI == true)
           mask = srcImage.generateVOIMask();
    }

    /**
     *
     *   @param srcImg   source image model
     *   @param fill     value used to fill a region
     *   @param polarity flag indicating fill location, true = fill inside; false fill outside
     *   @param useVOI   use the VOI to define the mask area else it will use the painted area to define the mask.
     */
    public AlgorithmMask( ModelImage srcImg, float fill, boolean polarity, boolean useVOI ) {

        super( null, srcImg );
        imageFill = fill;
        this.polarity = polarity;

        if (useVOI == true)
          mask = srcImage.generateVOIMask();
    }

    /**
     *
     *   @param destImg  image model where result image is to stored
     *   @param srcImg   source image model
     *   @param fillR    red value used to fill a region
     *   @param fillG    green value used to fill a region
     *   @param fillB    blue value used to fill a region
     *   @param polarity flag indicating fill location, true = fill inside; false fill outside
     *   @param useVOI   use the VOI to define the mask area else it will use the painted area to define the mask.
     */
    public AlgorithmMask( ModelImage destImg, ModelImage srcImg, float fillR,
                          float fillG, float fillB, boolean polarity, boolean useVOI ) {

        super( destImg, srcImg );
        imageFillR = fillR;
        imageFillG = fillG;
        imageFillB = fillB;
        this.polarity = polarity;

        if (useVOI == true)
           mask = srcImage.generateVOIMask();
    }

    /**
     *
     *   @param srcImg   source image model
     *   @param fillR    red value used to fill a region
     *   @param fillG    green value used to fill a region
     *   @param fillB    blue value used to fill a region
     *   @param polarity flag indicating fill location, true = fill inside; false fill outside
     *   @param useVOI   use the VOI to define the mask area else it will use the painted area to define the mask.
     */
    public AlgorithmMask( ModelImage srcImg, float fillR, float fillG, float fillB,
                          boolean polarity, boolean useVOI ) {

        super( null, srcImg );
        imageFillR = fillR;
        imageFillG = fillG;
        imageFillB = fillB;
        this.polarity = polarity;

        if (useVOI == true)
          mask = srcImage.generateVOIMask();
    }


    /**
     *
     *   @param srcImg   source image model
     *   @param _fillColor     color used to fill a region
     *   @param polarity flag indicating fill location, true = fill inside; false fill outside
     *   @param useVOI   use the VOI to define the mask area else it will use the painted area to define the mask.
     */
    public AlgorithmMask( ModelImage srcImg, Color _fillColor, boolean polarity, boolean useVOI ) {

        super( null, srcImg );
        this.polarity = polarity;

        if (useVOI == true)
          mask = srcImage.generateVOIMask();
    }

    /**
     *   Prepares this class for destruction
     */
    public void finalize() {
        super.finalize();
    }

    /**
     *   Constructs a string of the contruction parameters and outputs the
     *   string to the messsage frame if the logging procedure is turned on.
     */
    private void constructLog() {
        historyString = new String( "Mask(" + String.valueOf( imageFill ) + String.valueOf( polarity ) + ")\n" );

    }

    /**
     *   Starts the program
     */
    public void runAlgorithm() {

        if ( srcImage == null ) {
            displayError( "Source Image is null" );
            return;
        }

        constructLog();

        if (srcImage.isColorImage()) {
            if (destImage != null) {
                if (srcImage.getNDims() == 2) {
                    calcStoreInDest2DRGB();
                } else if (srcImage.getNDims() > 2) {
                    calcStoreInDest3DRGB();
                }
                destImage.setMatrix(srcImage.getMatrix());
            } else {
                if (srcImage.getNDims() == 2) {
                    calcInPlace2DRGB();
                } else if (srcImage.getNDims() > 2) {
                    calcInPlace3DRGB();
                }
            }

        } // if (srcImage.isColorImage())
        else { // !colorImage
            if (destImage != null) {
                if (srcImage.getNDims() == 2) {
                    calcStoreInDest2D();
                } else if (srcImage.getNDims() > 2) {
                    calcStoreInDest3D();
                }
                destImage.setMatrix(srcImage.getMatrix());
            } else {
                if (srcImage.getNDims() == 2) {
                    calcInPlace2D();
                } else if (srcImage.getNDims() > 2) {
                    calcInPlace3D();
                }
            }
        } // else !colorImage
    }

    /**
     *   Fills VOI of source image with fill value
     */
    private void calcInPlace2D() {

        int i;
        int length;
        float[] buffer;
System.out.println("calcInPlace2D");
        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportData( 0, length, buffer ); // locks and releases lock
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        int mod = length / 100; //mod is 1 percent of length

        initProgressBar();

        for ( i = 0; i < length && !threadStopped; i++ ) {
            if ( i % mod == 0 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round( (float) i / ( length - 1 ) * 100 ), activeImage );
            }

            if ( mask.get( i ) == true && polarity == true ) {
                buffer[i] = imageFill;
            } else if ( mask.get( i ) == false && polarity == false ) {
                buffer[i] = imageFill;
            }
        }

        if ( threadStopped ) {
            finalize();
            return;
        }

        try {
            //srcImage.reallocate(srcImage.getType());
            srcImage.importData( 0, buffer, true );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        }

        disposeProgressBar();
        setCompleted( true );
    }

    /**
     *   Fills VOI of the source image with fill value
     */
    private void calcInPlace3D() {

        int i, z;
        int length, offset;
        float[] buffer;

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        initProgressBar();

        for ( z = 0; z < srcImage.getExtents()[2]; z++ ) {

            try {
                srcImage.exportData( z * length, length, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            offset = z * length;
            if ( pBarVisible == true && progressBar != null ) {
                progressBar.updateValue( Math.round( (float) z / ( srcImage.getExtents()[2] - 1 ) * 100 ), activeImage );
            }
            for ( i = 0; i < length && !threadStopped; i++ ) {

                if ( mask.get( offset + i ) == true && polarity == true ) {
                    buffer[i] = imageFill;
                } else if ( mask.get( offset + i ) == false && polarity == false ) {
                    buffer[i] = imageFill;
                }
            }

            if ( threadStopped ) {
                finalize();
                return;
            }

            try {
                srcImage.importData( offset, buffer, false );
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }
        }

        srcImage.calcMinMax();
        disposeProgressBar();
        setCompleted( true );
    }

    /**
     *   Fills new image/VOI with new fill value;
     */
    private void calcStoreInDest2D() {

        int i;
        int length;
        float[] buffer;

        try {
            destImage.setLock( ModelStorageBase.RW_LOCKED );
        } catch ( IOException error ) {
            displayError( "Algorithm Mask: Image(s) locked" );
            setCompleted( false );
            return;
        }

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportData( 0, length, buffer ); // locks and releases lock
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        int mod = length / 100; //mod is 1 percent of length

        initProgressBar();

        for ( i = 0; i < length && !threadStopped; i++ ) {
            if ( i % mod == 0 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round( (float) i / ( length - 1 ) * 100 ), activeImage );
            }

            if ( mask.get( i ) == true && polarity == false ) {
                destImage.set( i, buffer[i] );
            } else if ( mask.get( i ) == true && polarity == true ) {
                destImage.set( i, imageFill );
            } else if ( mask.get( i ) == false && polarity == false ) {
                destImage.set( i, imageFill );
            } else {
                destImage.set( i, buffer[i] );
            }
        }
        if ( threadStopped ) {
            finalize();
            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        //destImage.notifyImageDisplayListeners(null, true);
        setCompleted( true );
        disposeProgressBar();
    }

    /**
     *   Fills new image/VOI with new fill value;
     */
    private void calcStoreInDest3D() {

        int i, z;
        int length, offset;
        float[] buffer;

        try {
            destImage.setLock( ModelStorageBase.RW_LOCKED );
        } catch ( IOException error ) {
            displayError( "Algorithm Mask: Image(s) locked" );
            setCompleted( false );
            return;
        }

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        initProgressBar();

        for ( z = 0; z < srcImage.getExtents()[2] && !threadStopped; z++ ) {
            try {
                srcImage.exportData( z * length, length, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            offset = z * length;
            if ( pBarVisible == true && progressBar != null ) {
                progressBar.updateValue( Math.round( (float) z / ( srcImage.getExtents()[2] - 1 ) * 100 ), activeImage );
            }

            for ( i = 0; i < length && !threadStopped; i++ ) {
                if ( mask.get( offset + i ) == true && polarity == false ) {
                    destImage.set( offset + i, buffer[i] );
                } else if ( mask.get( offset + i ) == true && polarity == true ) {
                    destImage.set( offset + i, imageFill );
                } else if ( mask.get( offset + i ) == false && polarity == false ) {
                    destImage.set( offset + i, imageFill );
                } else {
                    destImage.set( offset + i, buffer[i] );
                }
            }
        }
        if ( threadStopped ) {
            finalize();
            return;
        }
        destImage.calcMinMax();
        destImage.releaseLock();
        //destImage.notifyImageDisplayListeners(null, true);
        disposeProgressBar();
        setCompleted( true );
    }

    /**
     *   Fills VOI of source image with fill value
     */
    private void calcInPlace2DRGB() {

        int i;
        int length;
        float[] buffer;

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportRGBData(1, 0, length, buffer ); // locks and releases lock
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        int mod = length / 100; //mod is 1 percent of length

        initProgressBar();

        for ( i = 0; i < length && !threadStopped; i++ ) {
            if ( i % mod == 0 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round( (float) i / ( length - 1 ) * 33 ), activeImage );
            }

            if ( mask.get( i ) == true && polarity == true ) {
                buffer[i] = imageFillR;
            } else if ( mask.get( i ) == false && polarity == false ) {
                buffer[i] = imageFillR;
            }
        }

        if ( threadStopped ) {
            finalize();
            return;
        }

        try {
            //srcImage.reallocate(srcImage.getType());
            srcImage.importRGBData(1, 0, buffer, false );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        }

        try {
            srcImage.exportRGBData(2, 0, length, buffer ); // locks and releases lock
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        for ( i = 0; i < length && !threadStopped; i++ ) {
            if ( i % mod == 0 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round(33 +  (float) i / ( length - 1 ) * 33 ), activeImage );
            }

            if ( mask.get( i ) == true && polarity == true ) {
                buffer[i] = imageFillG;
            } else if ( mask.get( i ) == false && polarity == false ) {
                buffer[i] = imageFillG;
            }
        }

        if ( threadStopped ) {
            finalize();
            return;
        }

        try {
            //srcImage.reallocate(srcImage.getType());
            srcImage.importRGBData(2, 0, buffer, false );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        }

        try {
            srcImage.exportRGBData(3, 0, length, buffer ); // locks and releases lock
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        for ( i = 0; i < length && !threadStopped; i++ ) {
            if ( i % mod == 0 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round(67 + (float) i / ( length - 1 ) * 33 ), activeImage );
            }

            if ( mask.get( i ) == true && polarity == true ) {
                buffer[i] = imageFillB;
            } else if ( mask.get( i ) == false && polarity == false ) {
                buffer[i] = imageFillB;
            }
        }

        if ( threadStopped ) {
            finalize();
            return;
        }

        try {
            //srcImage.reallocate(srcImage.getType());
            srcImage.importRGBData(3, 0, buffer, true );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        }

        disposeProgressBar();
        setCompleted( true );
    }

    /**
     *   Fills VOI of the source image with fill value
     */
    private void calcInPlace3DRGB() {

        int i, z;
        int length, offset;
        float[] buffer;

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        initProgressBar();

        for ( z = 0; z < srcImage.getExtents()[2]; z++ ) {

            try {
                srcImage.exportRGBData(1, 4 * z * length, length, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            offset = z * length;
            if ( pBarVisible == true && progressBar != null ) {
                progressBar.updateValue( Math.round( (float) (3*z) /(3* ( srcImage.getExtents()[2] - 1 )+2) * 100 ), activeImage );
            }
            for ( i = 0; i < length && !threadStopped; i++ ) {

                if ( mask.get( offset + i ) == true && polarity == true ) {
                    buffer[i] = imageFillR;
                } else if ( mask.get( offset + i ) == false && polarity == false ) {
                    buffer[i] = imageFillR;
                }
            }

            if ( threadStopped ) {
                finalize();
                return;
            }

            try {
                //srcImage.reallocate(srcImage.getType());
                srcImage.importRGBData(1, 4 * offset, buffer, false );
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            try {
                srcImage.exportRGBData(2, 4 * z * length, length, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            if ( pBarVisible == true && progressBar != null ) {
                progressBar.updateValue( Math.round( (float) (3*z+1) /(3* ( srcImage.getExtents()[2] - 1 )+2) * 100 ), activeImage );
            }
            for ( i = 0; i < length && !threadStopped; i++ ) {

                if ( mask.get( offset + i ) == true && polarity == true ) {
                    buffer[i] = imageFillG;
                } else if ( mask.get( offset + i ) == false && polarity == false ) {
                    buffer[i] = imageFillG;
                }
            }

            if ( threadStopped ) {
                finalize();
                return;
            }

            try {
                //srcImage.reallocate(srcImage.getType());
                srcImage.importRGBData(2, 4 * offset, buffer, false );
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            try {
                srcImage.exportRGBData(3, 4 * z * length, length, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            if ( pBarVisible == true && progressBar != null ) {
                progressBar.updateValue( Math.round( (float) (3*z+2) /(3* ( srcImage.getExtents()[2] - 1 )+2) * 100 ), activeImage );
            }
            for ( i = 0; i < length && !threadStopped; i++ ) {

                if ( mask.get( offset + i ) == true && polarity == true ) {
                    buffer[i] = imageFillB;
                } else if ( mask.get( offset + i ) == false && polarity == false ) {
                    buffer[i] = imageFillB;
                }
            }

            if ( threadStopped ) {
                finalize();
                return;
            }

            try {
                //srcImage.reallocate(srcImage.getType());
                srcImage.importRGBData(3, 4 * offset, buffer, false );
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

        }

        srcImage.calcMinMax();
        disposeProgressBar();
        setCompleted( true );
    }

    /**
     *   Fills new image/VOI with new fill value;
     */
    private void calcStoreInDest2DRGB() {

        int i;
        int length;
        float[] buffer;


        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportRGBData(1, 0, length, buffer ); // locks and releases lock
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        int mod = length / 100; //mod is 1 percent of length

        initProgressBar();

        for ( i = 0; i < length && !threadStopped; i++ ) {
            if ( i % mod == 0 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round( (float) i / ( length - 1 ) * 33 ), activeImage );
            }

            if ( mask.get( i ) == true && polarity == true ) {
                buffer[i] = imageFillR;
            } else if ( mask.get( i ) == false && polarity == false ) {
                buffer[i] = imageFillR;
            }
        }

        if ( threadStopped ) {
            finalize();
            return;
        }

        try {
            destImage.importRGBData(1, 0, buffer, false );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        }

        try {
            srcImage.exportRGBData(2, 0, length, buffer ); // locks and releases lock
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        for ( i = 0; i < length && !threadStopped; i++ ) {
            if ( i % mod == 0 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round(33 +  (float) i / ( length - 1 ) * 33 ), activeImage );
            }

            if ( mask.get( i ) == true && polarity == true ) {
                buffer[i] = imageFillG;
            } else if ( mask.get( i ) == false && polarity == false ) {
                buffer[i] = imageFillG;
            }
        }

        if ( threadStopped ) {
            finalize();
            return;
        }

        try {
            destImage.importRGBData(2, 0, buffer, false );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        }

        try {
            srcImage.exportRGBData(3, 0, length, buffer ); // locks and releases lock
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        for ( i = 0; i < length && !threadStopped; i++ ) {
            if ( i % mod == 0 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round(67 + (float) i / ( length - 1 ) * 33 ), activeImage );
            }

            if ( mask.get( i ) == true && polarity == true ) {
                buffer[i] = imageFillB;
            } else if ( mask.get( i ) == false && polarity == false ) {
                buffer[i] = imageFillB;
            }
        }

        if ( threadStopped ) {
            finalize();
            return;
        }

        try {
            destImage.importRGBData(3, 0, buffer, true );
        } catch ( IOException error ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Image(s) locked", true );
            return;
        }

        disposeProgressBar();
        setCompleted( true );
    }

    /**
     *   Fills new image/VOI with new fill value;
     */
    private void calcStoreInDest3DRGB() {

        int i, z;
        int length, offset;
        float[] buffer;



        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        initProgressBar();

        for ( z = 0; z < srcImage.getExtents()[2]; z++ ) {

            try {
                srcImage.exportRGBData(1, 4 * z * length, length, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            offset = z * length;
            if ( pBarVisible == true && progressBar != null ) {
                progressBar.updateValue( Math.round( (float) (3*z) /(3* ( srcImage.getExtents()[2] - 1 )+2) * 100 ), activeImage );
            }
            for ( i = 0; i < length && !threadStopped; i++ ) {

                if ( mask.get( offset + i ) == true && polarity == true ) {
                    buffer[i] = imageFillR;
                } else if ( mask.get( offset + i ) == false && polarity == false ) {
                    buffer[i] = imageFillR;
                }
            }

            if ( threadStopped ) {
                finalize();
                return;
            }

            try {
                destImage.importRGBData(1, 4 * offset, buffer, false );
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            try {
                srcImage.exportRGBData(2, 4 * z * length, length, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            if ( pBarVisible == true && progressBar != null ) {
                progressBar.updateValue( Math.round( (float) (3*z+1) /(3* ( srcImage.getExtents()[2] - 1 )+2) * 100 ), activeImage );
            }
            for ( i = 0; i < length && !threadStopped; i++ ) {

                if ( mask.get( offset + i ) == true && polarity == true ) {
                    buffer[i] = imageFillG;
                } else if ( mask.get( offset + i ) == false && polarity == false ) {
                    buffer[i] = imageFillG;
                }
            }

            if ( threadStopped ) {
                finalize();
                return;
            }

            try {
                destImage.importRGBData(2, 4 * offset, buffer, false );
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            try {
                srcImage.exportRGBData(3, 4 * z * length, length, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

            if ( pBarVisible == true && progressBar != null ) {
                progressBar.updateValue( Math.round( (float) (3*z+2) /(3* ( srcImage.getExtents()[2] - 1 )+2) * 100 ), activeImage );
            }
            for ( i = 0; i < length && !threadStopped; i++ ) {

                if ( mask.get( offset + i ) == true && polarity == true ) {
                    buffer[i] = imageFillB;
                } else if ( mask.get( offset + i ) == false && polarity == false ) {
                    buffer[i] = imageFillB;
                }
            }

            if ( threadStopped ) {
                finalize();
                return;
            }

            try {
                destImage.importRGBData(3, 4 * offset, buffer, false );
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }

        }

        destImage.calcMinMax();
        disposeProgressBar();
        setCompleted( true );

    }

    /**
     *   Fills VOI of the source image with fill value
     *   @param mask
     *   @param fillValue value to be placed in the image where the mask is true
     *   @param tSlice    indicates which volume should be painted (tSlice = 4th dimension)
     */
    public void calcInPlace25D( BitSet mask, float fillValue, int tSlice ) {
    	calcInPlace25D(mask, fillValue, tSlice, null);
    }

    /**
     *   Fills VOI of the source image with fill value
     *   @param mask
     *   @param fillValue value to be placed in the image where the mask is true
     *   @param tSlice    indicates which volume should be painted (tSlice = 4th dimension)
     *   @param intensityLockVector Vector containing Integer objects that represent the intensity
     *   values that are not mutable
     */
    public void calcInPlace25D( BitSet mask, float fillValue, int tSlice, Vector intensityLockVector ) {

        int i, z, end = 1;
        int imgLength, volLength = 0, offset;
        float[] buffer;
        float[] bufferI;
        boolean logMagDisplay;
        float mag, norm;
        
        int lockedIntensities[] = null;

        if (intensityLockVector != null)
        {
        	lockedIntensities = new int[intensityLockVector.size()];
        	
        	for (i = 0; i < intensityLockVector.size(); i++)
        	{
        		try
        		{
	        		Integer integerObj = (Integer) intensityLockVector.elementAt(i);
	        		
	        		if (integerObj != null)
	        		{
	        			lockedIntensities[i] = integerObj.intValue();
	        		}
        		}
        		catch (Exception e)
        		{
        			continue;
        		}
        	}
        }

        if ( srcImage.getType() != ModelStorageBase.COMPLEX ) {
            try {

                imgLength = srcImage.getSliceSize();
                buffer = new float[imgLength];
                buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
            } catch ( OutOfMemoryError e ) {
                buffer = null;
                bufferI = null;
                errorCleanUp( "Algorithm Mask: Out of memory", true );
                return;
            }

            initProgressBar();
            MipavUtil.centerOnScreen(progressBar);

            if ( srcImage.getNDims() == 4 ) {
                end = srcImage.getExtents()[2];
                volLength = imgLength * srcImage.getExtents()[2];
            } else if ( srcImage.getNDims() == 3 ) {
                end = srcImage.getExtents()[2];
                volLength = imgLength * srcImage.getExtents()[2];
                tSlice = 0;
            } else if ( srcImage.getNDims() == 2 ) {
                end = 1;
            } else {
                errorCleanUp( "Algorithm mask - dimension not supported", false );
                return;
            }

            for ( z = 0; z < end && !threadStopped; z++ ) {
                try {
                    srcImage.exportData( tSlice * volLength + z * imgLength, imgLength, buffer ); // locks and releases lock
                } catch ( IOException error ) {
                    buffer = null;
                    bufferI = null;
                    errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                    return;
                }
                if ( srcImage.getNDims() == 3 && isProgressBarVisible() ) {
                    progressBar.updateValue( Math.round( (float) z / ( srcImage.getExtents()[2] - 1 ) * 100 ),
                            activeImage );
                }
                offset = z * imgLength;
                int mod = imgLength / 10;
                for ( i = 0; i < imgLength && !threadStopped; i++ ) {
                    if ( srcImage.getNDims() == 2 && ( i % mod == 0 ) && isProgressBarVisible() ) {
                        progressBar.updateValue( Math.round( (float) i / ( imgLength - 1 ) ) * 100, activeImage );
                    }
                    if ( (mask.get( offset + i ) == true && polarity == true) ||
                    	 (mask.get( offset + i ) == false && polarity == false) ) {
                    	
                    	boolean locked = false;
                    	if (lockedIntensities != null)
                    	{
                    		for (int j = 0; j < lockedIntensities.length; j++)
                    		{
                    			if (buffer[i] == lockedIntensities[j])
                    			{
                    				locked = true;
                    			}
                    		}
                    	}
                    	
                    	if (locked == false)
                    	{
                    		buffer[i] = fillValue;
                    	}
                    }
                }

                try {
                    srcImage.importData( tSlice * volLength + z * imgLength, buffer, false );
                } catch ( IOException error ) {
                    buffer = null;
                    bufferI = null;
                    errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                    return;
                }
            }

            srcImage.calcMinMax();
        } // if (srcImage.getType() != ModelStorageBase.COMPLEX)
        else { // COMPLEX
            // For complex numbers don't use a fill value - just nearly zero the
            // painted areas.
            logMagDisplay = srcImage.getLogMagDisplay();
            try {
                imgLength = srcImage.getSliceSize();
                buffer = new float[imgLength];
                bufferI = new float[imgLength];
                buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
            } catch ( OutOfMemoryError e ) {
                buffer = null;
                bufferI = null;
                errorCleanUp( "Algorithm Mask: Out of memory", true );
                return;
            }

            initProgressBar();

            if ( srcImage.getNDims() == 4 ) {
                end = srcImage.getExtents()[2];
                volLength = imgLength * srcImage.getExtents()[2];
            } else if ( srcImage.getNDims() == 3 ) {
                end = srcImage.getExtents()[2];
                volLength = imgLength * srcImage.getExtents()[2];
                tSlice = 0;
            } else if ( srcImage.getNDims() == 2 ) {
                end = 1;
            } else {
                errorCleanUp( "Algorithm mask - dimension not supported", false );
                return;
            }

            for ( z = 0; z < end && !threadStopped; z++ ) {
                try {
                    srcImage.exportComplexData( 2 * ( tSlice * volLength + z * imgLength ), imgLength, buffer, bufferI ); // locks and releases lock
                } catch ( IOException error ) {
                    errorCleanUp( "Algorithm Mask: Image(s) locked", false );
                    return;
                }
                if ( srcImage.getNDims() == 3 && isProgressBarVisible() ) {
                    progressBar.updateValue( Math.round( (float) z / ( srcImage.getExtents()[2] - 1 ) * 100 ),
                            activeImage );
                }
                offset = z * imgLength;
                int mod = imgLength / 10;
                for ( i = 0; i < imgLength && !threadStopped; i++ ) {
                    if ( srcImage.getNDims() == 2 && ( i % mod == 0 ) && isProgressBarVisible() ) {
                        progressBar.updateValue( Math.round( (float) i / ( imgLength - 1 ) ) * 100, activeImage );
                    }
                    if ( mask.get( offset + i ) == true && polarity == true ) {
                        // Must preserve phase information so make values 1000 times the
                        // minimum float instead of zero
                        mag = (float) ( Math.sqrt( buffer[i] * buffer[i] + bufferI[i] * bufferI[i] ) );
                        if ( mag > 1000.0f ) {
                            norm = 1000.0f * Float.MIN_VALUE / mag;
                            buffer[i] = buffer[i] * norm;
                            bufferI[i] = bufferI[i] * norm;
                        }
                    } else if ( mask.get( offset + i ) == false && polarity == false ) {
                        mag = (float) ( Math.sqrt( buffer[i] * buffer[i] + bufferI[i] * bufferI[i] ) );
                        if ( mag > 1000.0f ) {
                            norm = 1000.0f * Float.MIN_VALUE / mag;
                            buffer[i] = buffer[i] * norm;
                            bufferI[i] = bufferI[i] * norm;
                        }
                    }
                }

                try {
                    srcImage.importComplexData( 2 * ( tSlice * volLength + z * imgLength ), buffer, bufferI, false,
                            logMagDisplay );
                } catch ( IOException error ) {
                    buffer = null;
                    bufferI = null;
                    errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                    return;
                }
            }
            srcImage.calcMinMaxMag( logMagDisplay );
        } // else COMPLEX
        disposeProgressBar();
        if ( threadStopped ) {
            finalize();
            return;
        }
        setCompleted( true );
    }
    
    /**
     *   Fills VOI of the source image with fill value
     *   @param mask
     *   @param fillValue value to be placed in the image where the mask is true
     *   @param tSlice    indicates which volume should be painted (tSlice = 4th dimension)
     */
    public void calcInPlace25DShortMask( BitSet mask, float fillValue, int tSlice ) {
    	calcInPlace25DShortMask(mask, fillValue, tSlice, null);
    }


    /**
     *   Fills VOI of the source image with fill value
     *   @param mask
     *   @param fillValue value to be placed in the image where the mask is true
     *   @param tSlice    indicates which volume should be painted (tSlice = 4th dimension)
     */
    public void calcInPlace25DShortMask( BitSet mask, float fillValue, int tSlice, Vector intensityLockVector ) {

        int i, z, end = 1;
        int imgLength, volLength = 0, offset;
        float[] buffer;
        float[] bufferI;
        boolean logMagDisplay;
        float mag, norm;
        
        int lockedIntensities[] = null;

        if (intensityLockVector != null)
        {
        	lockedIntensities = new int[intensityLockVector.size()];
        	
        	for (i = 0; i < intensityLockVector.size(); i++)
        	{
        		try
        		{
	        		Integer integerObj = (Integer) intensityLockVector.elementAt(i);
	        		
	        		if (integerObj != null)
	        		{
	        			lockedIntensities[i] = integerObj.intValue();
	        		}
        		}
        		catch (Exception e)
        		{
        			continue;
        		}
        	}
        }

        if ( srcImage.getType() != ModelStorageBase.COMPLEX ) {
            try {

                imgLength = srcImage.getSliceSize();
                buffer = new float[imgLength];
                buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
            } catch ( OutOfMemoryError e ) {
                buffer = null;
                bufferI = null;
                errorCleanUp( "Algorithm Mask: Out of memory", true );
                return;
            }

            initProgressBar();

            if ( srcImage.getNDims() == 4 ) {
                end = srcImage.getExtents()[2];
                volLength = imgLength * srcImage.getExtents()[2];
            } else if ( srcImage.getNDims() == 3 ) {
                end = srcImage.getExtents()[2];
                volLength = imgLength * srcImage.getExtents()[2];
                tSlice = 0;
            } else if ( srcImage.getNDims() == 2 ) {
                end = 1;
            } else {
                errorCleanUp( "Algorithm mask - dimension not supported", false );
                return;
            }

            for ( z = 0; z < end && !threadStopped; z++ ) {
                try {
                    srcImage.exportData( tSlice * volLength + z * imgLength, imgLength, buffer ); // locks and releases lock
                } catch ( IOException error ) {
                    buffer = null;
                    bufferI = null;
                    errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                    return;
                }
                if ( srcImage.getNDims() == 3 && isProgressBarVisible() ) {
                    progressBar.updateValue( Math.round( (float) z / ( srcImage.getExtents()[2] - 1 ) * 100 ),
                            activeImage );
                }
                offset = z * imgLength;
                int mod = imgLength / 10;
                for ( i = 0; i < imgLength && !threadStopped; i++ ) {
                    if ( srcImage.getNDims() == 2 && ( i % mod == 0 ) && isProgressBarVisible() ) {
                        progressBar.updateValue( Math.round( (float) i / ( imgLength - 1 ) ) * 100, activeImage );
                    }
                    if ( (mask.get( offset + i ) == true && polarity == true) ||
                    	 (mask.get( offset + i ) == false && polarity == false) ) {
                    	
                    	boolean locked = false;
                    	if (lockedIntensities != null)
                    	{
                    		for (int j = 0; j < lockedIntensities.length; j++)
                    		{
                    			if (buffer[i] == lockedIntensities[j])
                    			{
                    				locked = true;
                    			}
                    		}
                    	}
                    	
                    	if (locked == false)
                    	{
                    		buffer[i] = fillValue;
                    	}
                    } else if ( mask.get( offset + i ) == false && polarity == true ) {
                        buffer[i] = 0;
                    }
                }

                try {
                    srcImage.importData( tSlice * volLength + z * imgLength, buffer, false );
                } catch ( IOException error ) {
                    buffer = null;
                    bufferI = null;
                    errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                    return;
                }
            }

            srcImage.calcMinMax();
        } // if (srcImage.getType() != ModelStorageBase.COMPLEX)
        else { // COMPLEX
            // For complex numbers don't use a fill value - just nearly zero the
            // painted areas.
            logMagDisplay = srcImage.getLogMagDisplay();
            try {
                imgLength = srcImage.getSliceSize();
                buffer = new float[imgLength];
                bufferI = new float[imgLength];
                buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
            } catch ( OutOfMemoryError e ) {
                buffer = null;
                bufferI = null;
                errorCleanUp( "Algorithm Mask: Out of memory", true );
                return;
            }

            initProgressBar();

            if ( srcImage.getNDims() == 4 ) {
                end = srcImage.getExtents()[2];
                volLength = imgLength * srcImage.getExtents()[2];
            } else if ( srcImage.getNDims() == 3 ) {
                end = srcImage.getExtents()[2];
                volLength = imgLength * srcImage.getExtents()[2];
                tSlice = 0;
            } else if ( srcImage.getNDims() == 2 ) {
                end = 1;
            } else {
                errorCleanUp( "Algorithm mask - dimension not supported", false );
                return;
            }

            for ( z = 0; z < end && !threadStopped; z++ ) {
                try {
                    srcImage.exportComplexData( 2 * ( tSlice * volLength + z * imgLength ), imgLength, buffer, bufferI ); // locks and releases lock
                } catch ( IOException error ) {
                    errorCleanUp( "Algorithm Mask: Image(s) locked", false );
                    return;
                }
                if ( srcImage.getNDims() == 3 && isProgressBarVisible() ) {
                    progressBar.updateValue( Math.round( (float) z / ( srcImage.getExtents()[2] - 1 ) * 100 ),
                            activeImage );
                }
                offset = z * imgLength;
                int mod = imgLength / 10;
                for ( i = 0; i < imgLength && !threadStopped; i++ ) {
                    if ( srcImage.getNDims() == 2 && ( i % mod == 0 ) && isProgressBarVisible() ) {
                        progressBar.updateValue( Math.round( (float) i / ( imgLength - 1 ) ) * 100, activeImage );
                    }
                    if ( mask.get( offset + i ) == true && polarity == true ) {
                        // Must preserve phase information so make values 1000 times the
                        // minimum float instead of zero
                        mag = (float) ( Math.sqrt( buffer[i] * buffer[i] + bufferI[i] * bufferI[i] ) );
                        if ( mag > 1000.0f ) {
                            norm = 1000.0f * Float.MIN_VALUE / mag;
                            buffer[i] = buffer[i] * norm;
                            bufferI[i] = bufferI[i] * norm;
                        }
                    } else if ( mask.get( offset + i ) == false && polarity == false ) {
                        mag = (float) ( Math.sqrt( buffer[i] * buffer[i] + bufferI[i] * bufferI[i] ) );
                        if ( mag > 1000.0f ) {
                            norm = 1000.0f * Float.MIN_VALUE / mag;
                            buffer[i] = buffer[i] * norm;
                            bufferI[i] = bufferI[i] * norm;
                        }
                    }
                }

                try {
                    srcImage.importComplexData( 2 * ( tSlice * volLength + z * imgLength ), buffer, bufferI, false,
                            logMagDisplay );
                } catch ( IOException error ) {
                    buffer = null;
                    bufferI = null;
                    errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                    return;
                }
            }
            srcImage.calcMinMaxMag( logMagDisplay );
        } // else COMPLEX
        disposeProgressBar();
        if ( threadStopped ) {
            finalize();
            return;
        }
        setCompleted( true );
    }
    

    /**
     *   Fills VOI of the color source image with fill color
     *   @param mask
     *   @param fillColor color to be placed in the image where the mask is true
     *   @param tSlice    indicates which volume should be painted (tSlice = 4th dimension)
     */
    public void calcInPlace25DC( BitSet mask, Color fillColor, int tSlice) {

        int i, j, z, end = 1;
        int imgLength, volLength = 0, offset;
        int paintLength;
        byte[] buffer;
        polarity = true;
        byte red, green, blue;

        red = (byte) Math.round( fillColor.getRed() );
        green = (byte) Math.round( fillColor.getGreen() );
        blue = (byte) Math.round( fillColor.getBlue() );
        
        try {
            paintLength = srcImage.getSliceSize();
            imgLength = 4 * paintLength;
            buffer = new byte[imgLength];
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        initProgressBar();

        if ( srcImage.getNDims() == 4 ) {
            end = srcImage.getExtents()[2];
            volLength = imgLength * srcImage.getExtents()[2];
        } else if ( srcImage.getNDims() == 3 ) {
            end = srcImage.getExtents()[2];
            volLength = imgLength * srcImage.getExtents()[2];
            tSlice = 0;
        } else if ( srcImage.getNDims() == 2 ) {
            end = 1;
        } else {
            errorCleanUp( "Algorithm mask - dimension not supported", false );
            return;
        }

        for ( z = 0; z < end && !threadStopped; z++ ) {
            try {
                srcImage.exportData( tSlice * volLength + z * imgLength, imgLength, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                errorCleanUp( "Algorithm Mask: Image(s) locked", false );
                return;
            }
            if ( srcImage.getNDims() == 3 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round( (float) z / ( srcImage.getExtents()[2] - 1 ) * 100 ), activeImage );
            }
            offset = z * paintLength;
            int mod = imgLength / 10;
            for ( i = 0, j = 0; i < imgLength && !threadStopped; i = i + 4, j++ ) {
                if ( srcImage.getNDims() == 2 && ( i % mod == 0 ) && isProgressBarVisible() ) {
                    progressBar.updateValue( Math.round( (float) i / ( imgLength - 1 ) ) * 100, activeImage );
                }
                if ( (mask.get( offset + j ) == true && polarity == true) ||
                	 (mask.get( offset + j ) == false && polarity == false) ) {
            		buffer[i] = (byte) 255;
                    buffer[i + 1] = red;
                    buffer[i + 2] = green;
                    buffer[i + 3] = blue;
                }
            }

            try {
                srcImage.importData( tSlice * volLength + z * imgLength, buffer, false );
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }
        }

        if ( threadStopped ) {
            finalize();
            return;
        }

        srcImage.calcMinMax();
        disposeProgressBar();
        setCompleted( true );
    }

    /**
     *   Fills VOI of the color source image with fill color
     *   @param mask
     *   @param fillColor color to be placed in the image where the mask is true
     *   @param tSlice    indicates which volume should be painted (tSlice = 4th dimension)
     */
    public void calcInPlace25DCShortMask( BitSet mask, Color fillColor, int tSlice ) {

        int i, j, z, end = 1;
        int imgLength, volLength = 0, offset;
        int paintLength;
        byte[] buffer;
        polarity = true;
        byte red, green, blue;

        red = (byte) Math.round( fillColor.getRed() );
        green = (byte) Math.round( fillColor.getGreen() );
        blue = (byte) Math.round( fillColor.getBlue() );

        try {
            paintLength = srcImage.getSliceSize();
            imgLength = 4 * paintLength;
            buffer = new byte[imgLength];
            buildProgressBar( srcImage.getImageName(), "Masking ...", 0, 100 );
        } catch ( OutOfMemoryError e ) {
            buffer = null;
            errorCleanUp( "Algorithm Mask: Out of memory", true );
            return;
        }

        initProgressBar();

        if ( srcImage.getNDims() == 4 ) {
            end = srcImage.getExtents()[2];
            volLength = imgLength * srcImage.getExtents()[2];
        } else if ( srcImage.getNDims() == 3 ) {
            end = srcImage.getExtents()[2];
            volLength = imgLength * srcImage.getExtents()[2];
            tSlice = 0;
        } else if ( srcImage.getNDims() == 2 ) {
            end = 1;
        } else {
            errorCleanUp( "Algorithm mask - dimension not supported", false );
            return;
        }

        for ( z = 0; z < end && !threadStopped; z++ ) {
            try {
                srcImage.exportData( tSlice * volLength + z * imgLength, imgLength, buffer ); // locks and releases lock
            } catch ( IOException error ) {
                errorCleanUp( "Algorithm Mask: Image(s) locked", false );
                return;
            }
            if ( srcImage.getNDims() == 3 && isProgressBarVisible() ) {
                progressBar.updateValue( Math.round( (float) z / ( srcImage.getExtents()[2] - 1 ) * 100 ), activeImage );
            }
            offset = z * paintLength;
            int mod = imgLength / 10;
            for ( i = 0, j = 0; i < imgLength && !threadStopped; i = i + 4, j++ ) {
                if ( srcImage.getNDims() == 2 && ( i % mod == 0 ) && isProgressBarVisible() ) {
                    progressBar.updateValue( Math.round( (float) i / ( imgLength - 1 ) ) * 100, activeImage );
                }
                if ( mask.get( offset + j ) == true && polarity == true ) {
                    buffer[i] = (byte) 255;
                    buffer[i + 1] = red;
                    buffer[i + 2] = green;
                    buffer[i + 3] = blue;
                } else if ( mask.get( offset + j ) == false && polarity == true ) {
                    buffer[i] = (byte) 255;
                    buffer[i + 1] = 0;
                    buffer[i + 2] = 0;
                    buffer[i + 3] = 0;
                } else if ( mask.get( offset + j ) == false && polarity == false ) {
                    buffer[i] = (byte) 255;
                    buffer[i + 1] = red;
                    buffer[i + 2] = green;
                    buffer[i + 3] = blue;
                }
            }

            try {
                srcImage.importData( tSlice * volLength + z * imgLength, buffer, false );
            } catch ( IOException error ) {
                buffer = null;
                errorCleanUp( "Algorithm Mask: Image(s) locked", true );
                return;
            }
        }

        if ( threadStopped ) {
            finalize();
            return;
        }

        srcImage.calcMinMax();
        disposeProgressBar();
        setCompleted( true );
    }

}
