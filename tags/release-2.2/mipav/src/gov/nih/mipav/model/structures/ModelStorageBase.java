package gov.nih.mipav.model.structures;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.MipavUtil;

import java.io.*;
import java.util.*;
import java.lang.*;


/**
 *   N-dimensional buffer (physically stored as a 1D buffer)
 *
 *		@version 1.0 Sept 1, 1998
 *		@author Matthew J. McAuliffe
 */
public class ModelStorageBase extends ModelSerialCloneable {

    public static final int UNLOCKED = 0;
    public static final int RW_LOCKED = 1;
    public static final int W_LOCKED = 2; // locked from writing

    public static final int NEAREST = 0;
    public static final int LINEAR = 1;
    public static final int TALYOR = 2;

    /****** buffer types ******/
    public static final int BOOLEAN = 0;
    public static final int BYTE = 1;
    public static final int UBYTE = 2;
    public static final int SHORT = 3;
    public static final int USHORT = 4;
    public static final int INTEGER = 5;
    public static final int UINTEGER = 14;
    public static final int LONG = 6;
    public static final int FLOAT = 7;
    public static final int DOUBLE = 8;
    public static final int ARGB = 9; // 4 * UBYTE(8  bits)  = 4  bytes
    public static final int ARGB_USHORT = 10; // 4 * USHORT(16 bits) = 8 bytes
    public static final int ARGB_FLOAT = 11; // 4 * FLOAT(32 bits)  = 16 bytes
    public static final int COMPLEX = 12; // 2 * FLOAT(32 bits)  = 8  bytes
    public static final int DCOMPLEX = 13; // 2 * DOUBLE(64 bits) = 16 bytes

    public static final String BOOLEAN_STRING = "Boolean";
    public static final String BYTE_STRING = "Byte";
    public static final String UBYTE_STRING = "Unsigned Byte";
    public static final String SHORT_STRING = "Short";
    public static final String USHORT_STRING = "Unsigned Short";
    public static final String INTEGER_STRING = "Integer";
    public static final String LONG_STRING = "Long";
    public static final String FLOAT_STRING = "Float";
    public static final String DOUBLE_STRING = "Double";
    public static final String ARGB_STRING = "ARGB"; // 4 * UBYTE(8  bits)  = 4  bytes
    public static final String ARGB_USHORT_STRING = "ARGB Ushort"; // 4 * USHORT(16 bits) = 8 bytes
    public static final String ARGB_FLOAT_STRING = "ARGB Float"; // 4 * FLOAT(32 bits)  = 16 bytes
    public static final String COMPLEX_STRING = "Complex"; // 2 * FLOAT(32 bits)  = 8  bytes
    public static final String DCOMPLEX_STRING = "Complex Double"; // 2 * DOUBLE(64 bits) = 16 bytes
    public static final String UINTEGER_STRING = "Unsigned Integer";

    public static final String[] bufferTypeStr = {
        BOOLEAN_STRING, BYTE_STRING, UBYTE_STRING, SHORT_STRING,
        USHORT_STRING, INTEGER_STRING, LONG_STRING, FLOAT_STRING, DOUBLE_STRING, ARGB_STRING, ARGB_USHORT_STRING,
        ARGB_FLOAT_STRING, COMPLEX_STRING, DCOMPLEX_STRING, UINTEGER_STRING };

    /**
     *	information about the project which the image is a part of
     *	(or null if the image was not opened as part of a project).
     */
    protected FileInfoProject projectInfo;

    /**
     *  each image slice has a information that describes aspects
     *  of each slice of an image.
     */
    protected FileInfoBase[] fileInfo;

    /** storage location of image data */
    private BufferBase data;

    /** type of image buffer (i.e. BOOLEAN, BYTE, UBYTE, SHORT ...) */
    private int bufferType;

    /** number of dimensions that represent the image */
    private int nDims;

    /** locking status of the image. Default = UNLOCKED */
    private int lockStatus = UNLOCKED;

    /** */
    private int writeLockCount = 0;

    /**
     *   bounds of the image where <ul>
     *                <li>dimExtents[0] = x dimension</li>
     *                <li>dimExtents[1] = y dimension</li>
     *                <li>dimExtents[2] = z dimension (typically)</li>
     *                <li>dimExtents[3] = fourth dimension (ie time)...</li>
     *   </ul>
     */
    private int[] dimExtents;

    /**
     *   bounds of the image where<ul>
     *                <li>dimOriginalExtents[0] = x dimension</li>
     *                <li>dimOriginalExtents[1] = y dimension</li>
     *                <li>dimOriginalExtents[2] = z dimension (typically)</li>
     *                <li>dimOriginalExtents[3] = fourth dimension (ie time)...</li>
     *   </ul>
     */
    private int[] dimOriginalExtents;

    /*
     * This are variables used by the FFT filter algorithm
     *
     */
    private int originalKernelDimension;
    private int originalFilterConstruction;
    private int originalButterworthOrder;
    private float originalMinimum;
    private float originalMaximum;
    private boolean originalCropCheckbox;
    private boolean originalDoCrop;
    private int[] originalStart;
    private int[] originalEnd;
    private boolean haveWindowed;
    private int filterType = 2;
    private float freq1 = 0.4f;
    private float freq2 = 0.7f;

    /** total buffer length */
    private int dataSize;

    /** lastMin and lastMax store the last min and max values for an image.
     *   these are used as a trigger for resetting the transfer function.
     */
    protected double lastMin, lastMax;

    /** minimum and maximum image intensity */
    private double min, max;

    /** minimum and maximum nonzero intensity */
    private double nonZeroMin, nonZeroMax;

    /** smallest magnitude negative and positive */
    private double smallestMagnitudeNegative, smallestMagnitudePositive;

    /** minimum and maximum image RGB */
    private double minR, maxR, minG, maxG, minB, maxB;

    /** minimum and maximum nonzero image RGB */
    private double nonZeroMinR, nonZeroMaxR,
            nonZeroMinG, nonZeroMaxG,
            nonZeroMinB, nonZeroMaxB;

    /** smallest magnitude negative and positive */
    private double smallestMagnitudeNegativeR, smallestMagnitudePositiveR,
            smallestMagnitudeNegativeG, smallestMagnitudePositiveG,
            smallestMagnitudeNegativeB, smallestMagnitudePositiveB;

    /**
     *   minimum and maximum image intensity with log magnitude operation removed
     */
    private double noLogMin, noLogMax, noLogMinNonZero;

    /**
     *  boolean telling if log magnitude display is used in complex image
     */
    private boolean logMagDisp;

    /**
     *  boolean telling if 3D images are processed 1 slice at a time
     */
    private boolean image25D;

    // boolean telling if unequal dimensions are allowed in the FFT image
    private boolean unequalDim;

    /** default constructor */
    public ModelStorageBase() {
        data = null;
    }

    /**
     * allocates buffer memory of the specified type
     * @param type         type of buffer to allocate
     * @param dimExtents   extents of the buffer in each dimension (multipleid
     *                     together produces the size of the buffer to be allocated
     */
    public ModelStorageBase( int type, int[] dimExtents ) {
        construct( type, dimExtents );
    }

    /**
     * allocates buffer memory of the specified type
     * @param type         type of buffer to allocate
     * @param dimExtents   extents of the buffer in each dimension (multiplied
     *                     together produces the size of the buffer to be allocated
     */
    protected void construct( int type, int[] dimExtents ) {

        this.bufferType = type;
        this.dimExtents = (int[]) dimExtents.clone();
        this.nDims = dimExtents.length;

        // compute the dataSize based on the extents
        computeDataSize();

        // allocate memory in the data buffer and do some initialization
        // based on dataSize
        allocateData();

    }  // end construct()

    /**
     *   Allocates data based on the data type and dataSize.  Since this can be
     *   used to reallocate data, first set the data to null and do some garbage
     *   collecting.
     */
    protected void allocateData() {
        int i;

        // check to see if data is already allocated
        if ( this.data != null ) {
            this.data = null;
            System.gc();
        }

        // allocate memory in the data buffer and do some initialization
        // based on dataSize
        switch ( this.bufferType ) {
        case BOOLEAN:
            // since the dataSize computed for boolean may not actually
            // be the number used for allocation of the buffer,
            // go ahead and recompute the boolean datasize manually
            for ( this.dataSize = 1, i = 0; i < this.nDims; i++ ) {
                this.dataSize *= this.dimExtents[i];
            }
            try {
                this.data = new BufferBoolean( this.dataSize );

                // Reset dataSize to reflect change in buffer size
                // because BitSet structure may increase buffer.
                this.dataSize = ( (BufferBoolean) ( this.data ) ).dataArray.size();
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate boolean data" );
                throw ( error );
            }
            break;

        case BYTE:
            try {
                this.data = new BufferByte( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate byte data" );
                throw ( error );
            }
            break;

        case UBYTE:
            try {
                this.data = new BufferUByte( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate unsigned byte data" );
                throw ( error );
            }
            break;

        case SHORT:
            try {
                this.data = new BufferShort( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate short data" );
                throw ( error );
            }
            break;

        case USHORT:
            try {
                this.data = new BufferUShort( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate unsigned short data" );
                throw ( error );
            }
            break;

        case INTEGER:
            try {
                this.data = new BufferInt( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate integer data buffer" );
                throw ( error );
            }
            break;

        case UINTEGER:
            try {
                this.data = new BufferUInt( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate unsigned integer data buffer" );
                throw ( error );
            }
            break;

        case LONG:
            try {
                this.data = new BufferLong( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate long data buffer" );
                throw ( error );
            }
            break;

        case FLOAT:
            try {
                this.data = new BufferFloat( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate float data buffer" );
                throw ( error );
            }
            break;

        case COMPLEX:
            try {
                this.data = new BufferFloat( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate complex data buffer" );
                throw ( error );
            }
            break;

        case DOUBLE:
            try {
                this.data = new BufferDouble( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate double data buffer" );
                throw ( error );
            }
            break;

        case DCOMPLEX:
            try {
                this.data = new BufferDouble( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate double complex data buffer" );
                throw ( error );
            }
            break;

        case ARGB:
            try {
                this.data = new BufferUByte( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate ARGB-UByte data buffer" );
                throw ( error );
            }
            // set alphas to 255 - full on
            for ( i = 0; i < this.dataSize; i += 4 ) {
                setUByte( i, (byte) 255 );
            }

            break;

        case ARGB_USHORT:
            try {
                this.data = new BufferUShort( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate ARGB-UShort data buffer" );
                throw ( error );
            }
            // set alphas to 65535 - full on
            for ( i = 0; i < this.dataSize; i += 4 ) {
                setUShort( i, 65535 );
            }

            break;

        case ARGB_FLOAT:
            try {
                this.data = new BufferFloat( this.dataSize );
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate ARGB- float data buffer" );
                throw ( error );
            }

            // set alphas to 1.0 - full on
            for ( i = 0; i < this.dataSize; i += 4 ) {
                set( i, 1.0f );
            }
            break;

        default: {
            MipavUtil.displayError( "ModelStorageArray: Unknown data type - memory not allocated" );
        }
        }

    }  // end allocateData()

    /**
     *   Recomputes the datasize based on the type of buffer.  If the datasize has
     *   changed, then the data array needs to be reallocated. So this method will
     *   also reconstruct the data array in this case. This method
     *   must be called if the extents of an buffer have been changed.
     *
     *   WARNING:  This will clear any existing data if the dataSize is changed.
     */
    public void recomputeDataSize() {
        int oldDataSize = this.dataSize;

        this.computeDataSize();

        if ( this.dataSize != oldDataSize ) {
            allocateData();
        }

    } // end recomputeDataSize()

    /**
     *   Computes the datasize based on the type of buffer.
     */
    protected void computeDataSize() {
        int i;

        switch ( bufferType ) {
        case BOOLEAN:
            for ( this.dataSize = 1, i = 0; i < this.nDims; i++ ) {
                this.dataSize *= dimExtents[i];
            }
            try {
                // create a temporary buffer to get this dataSize)
                BufferBase tmpdata = new BufferBoolean( this.dataSize );

                // Reset dataSize to reflect change in buffer size
                // because BitSet structure may increase buffer.
                dataSize = ( (BufferBoolean) ( tmpdata ) ).dataArray.size();

                // clean up the temporary buffer
                tmpdata = null;
                System.gc();
            } catch ( OutOfMemoryError error ) {
                disposeLocal();
                MipavUtil.displayError( "ImageModel: Unable to allocate boolean data" );
                throw ( error );
            }
            break;

        case BYTE:
        case UBYTE:
        case SHORT:
        case USHORT:
        case INTEGER:
        case UINTEGER:
        case LONG:
        case FLOAT:
        case DOUBLE:
            for ( this.dataSize = 1, i = 0; i < this.nDims; i++ ) {
                this.dataSize *= dimExtents[i];
            }
            break;

        case COMPLEX:
        case DCOMPLEX:
            for ( this.dataSize = 1, i = 0; i < this.nDims; i++ ) {
                this.dataSize *= dimExtents[i];
            }
            this.dataSize *= 2;
            break;

        case ARGB:
        case ARGB_USHORT:
        case ARGB_FLOAT:
            for ( this.dataSize = 1, i = 0; i < this.nDims; i++ ) {
                this.dataSize *= dimExtents[i];
            }
            this.dataSize *= 4;
            break;

        default: {
            MipavUtil.displayError( "ModelStorageArray: Unknown data type - dataSize not computed" );
        }
        } // end switch

    }  // end computeDataSize()

    /**
     *   convertToFloat - disposes of old data and constructs a new buffer of the user specific type
     *   if the image in NOT locked
     *   @exception IOException    throws an exception if the image model is locked.
     */
    public synchronized void convertToFloat()
        throws IOException {

        if ( lockStatus == UNLOCKED ) {
            // disposeLocal(); // delete old memory and reallocate
            // data = null;
            // System.gc();
            if ( getType() != FLOAT ) {
                int i;

                for ( this.dataSize = 1, i = 0; i < this.nDims; i++ ) {
                    this.dataSize *= dimExtents[i];
                }
                float[] imgBuf = new float[dataSize];

                exportData( 0, dataSize, imgBuf );

                data = null;
                System.gc();
                construct( FLOAT, dimExtents );
                importData( 0, imgBuf, true );
            } else {
                return;
            }
        } else {
            throw new IOException( "ModelStorageBase: convertToFloat: Image locked !" );
        }
    }

    /**
     *   disposes of old data and constructs a new buffer of the user specific type
     *   if the image in NOT locked
     *   @param type   type of new buffer
     *   @exception IOException    throws an exception if the image model is locked.
     */
    public synchronized void reallocate( int type )
        throws IOException {
        if ( lockStatus == UNLOCKED ) {
            // disposeLocal(); // delete old memory and reallocate
            data = null;
            System.gc();
            construct( type, dimExtents );
        } else {
            throw new IOException( "ModelStorageBase: Reallocate: Image locked !" );
        }
    }

    /**
     *   reallocate - disposes of old data and constructs a new buffer of the user specific type
     *   if the image in NOT locked
     *   @param type   type of new buffer
     *   @param dimExtents   extents of the buffer in each dimension (multiplied
     *                     together produces the size of the buffer to be allocated
     *   @exception IOException    throws an exception if the image model is locked.
     */
    public synchronized void reallocate( int type, int[] dimExtents )
        throws IOException {

        if ( lockStatus == UNLOCKED ) {
            // disposeLocal(); // delete old memory and reallocate
            data = null;
            System.gc();
            construct( type, dimExtents );
        } else {
            throw new IOException( "ModelStorageBase: Reallocate: Image locked !" );
        }
    }

    /**
     *   reallocate - disposes of old data and constructs a new buffer of the user specific type
     *   if the image in NOT locked
     *   @param dimExtents   extents of the buffer in each dimension (multipleid
     *                     together produces the size of the buffer to be allocated
     *   @exception IOException    throws an exception if the image model is locked.
     */
    public synchronized void reallocate( int[] dimExtents )
        throws IOException {

        if ( lockStatus == UNLOCKED ) {
            // disposeLocal(); // delete old memory and reallocate
            data = null;
            System.gc();
            construct( bufferType, dimExtents );
        } else {
            throw new IOException( "ModelStorageBase: Reallocate: Image locked !" );
        }
    }

    /**
     *   Calls disposeLocal of this class to ensure this class nulls the
     *   references to global class variables so that memory will be
     *   recovered
     */
    protected void finalize()
        throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /** Dispose of memory and call the garbage collector */
    public void disposeLocal() {
        int i;
        if ( data != null ) {
            try {
                data.finalize();
            } catch ( Throwable t ) {}
        }

        data = null;

        projectInfo = null;
        if ( fileInfo != null ) {
            for ( i = 0; i < fileInfo.length; i++ ) {
                if ( fileInfo[i] != null ) {
                    fileInfo[i].finalize();
                    fileInfo[i] = null;
                }
            }
        }
        fileInfo = null;

        dimExtents = null;
        dimOriginalExtents = null;
    }

    /** Copies the object. */
    public Object clone() {
        try {
            setLock( ModelStorageBase.W_LOCKED );
        } catch ( IOException error ) {
            MipavUtil.displayError( "" + error );
            return null;
        }
        Object obj = super.clone();

        releaseLock();
        return ( obj );

    }

    /****************************************************************************/

    /******************************* Accessors **********************************/

    /****************************************************************************/

    /**
     * Accessor that returns the project information for this image
     * @return           FileInfoProject projectInfo structure
     */
    public FileInfoProject getProjectInfo() {
        return projectInfo;
    }

    /**
     * Accessor that returns the fileInfo of a specific image slice
     * @return           FileInfoBase fileInfo structure
     */
    public FileInfoBase[] getFileInfo() {
        return fileInfo;
    }

    /**
     * Accessor that returns the fileInfo of a specific image slice
     * @param i          index that indicates image slice
     * @return           FileInfoBase fileInfo structure
     */
    public FileInfoBase getFileInfo( int i ) {
        return fileInfo[i];
    }

    /**
     * Accessor that returns the image type
     * @return       int indicating image type
     */
    public final int getType() {
        return bufferType;
    }

    /**
     * Accessor that returns the image type
     * @return       int indicating image type
     */
    public final String getTypeString() {
        if ( bufferType == ModelStorageBase.BOOLEAN ) {
            return BOOLEAN_STRING;
        } else if ( bufferType == ModelStorageBase.BYTE ) {
            return BYTE_STRING;
        } else if ( bufferType == ModelStorageBase.UBYTE ) {
            return UBYTE_STRING;
        } else if ( bufferType == ModelStorageBase.SHORT ) {
            return SHORT_STRING;
        } else if ( bufferType == ModelStorageBase.USHORT ) {
            return USHORT_STRING;
        } else if ( bufferType == ModelStorageBase.INTEGER ) {
            return INTEGER_STRING;
        } else if ( bufferType == ModelStorageBase.UINTEGER ) {
            return UINTEGER_STRING;
        } else if ( bufferType == ModelStorageBase.LONG ) {
            return LONG_STRING;
        } else if ( bufferType == ModelStorageBase.FLOAT ) {
            return FLOAT_STRING;
        } else if ( bufferType == ModelStorageBase.DOUBLE ) {
            return DOUBLE_STRING;
        } else if ( bufferType == ModelStorageBase.ARGB ) {
            return ARGB_STRING;
        } else if ( bufferType == ModelStorageBase.ARGB_USHORT ) {
            return ARGB_USHORT_STRING;
        } else if ( bufferType == ModelStorageBase.ARGB_FLOAT ) {
            return ARGB_FLOAT_STRING;
        } else {
            return "Unknown";
        }
    }

    /**
     * Accessor that returns the dimensionality of the image
     * @return       int indicating the number of dimensions
     */
    public final int getNDims() {
        return nDims;
    }

    /**
     * Accessor that returns the extents of the image
     * @return       array of ints indicating the extents in each dimension
     */
    public final int[] getExtents() {
        return dimExtents;
    }

    /**
     * Get the nuber of pixels in a slice of the image.
     * @return  the number of pixels in a slice
     */
    public final int getSliceSize() {
        return dimExtents[0] * dimExtents[1];
    }

    /**
     * Accessor that returns the original extents of the image
     * @return       array of ints indicating the original extents in each dimension
     */
    public final int[] getOriginalExtents() {
        return dimOriginalExtents;
    }

    /**
     * returns kernel diameter chosen on forward FFT
     * @return       int showing original kernel diameter chosen on forward FFT
     */
    public final int getOriginalKernelDimension() {
        return originalKernelDimension;
    }

    /**
     * returns type of filter - low, high, bandpass, or bandstop
     * @return filterType
     */
    public final int getFilterType() {
        return filterType;
    }

    /**
     * returns frequency 1 of filter
     * @return freq1
     */
    public final float getFreq1() {
        return freq1;
    }

    /**
     * returns frequency 2 of filter
     * @return freq2
     */
    public final float getFreq2() {
        return freq2;
    }

    /**
     * returns integer telling filter construction method
     * @return     int showing original filter construction method
     */

    public final int getOriginalFilterConstruction() {
        return originalFilterConstruction;
    }

    /**
     * returns integer telling Butterworth order
     * @return     int showing Butterworth filter order;
     */
    public final int getOriginalButterworthOrder() {
        return originalButterworthOrder;
    }

    /** */
    public final float getOriginalMinimum() {
        return originalMinimum;
    }

    /** */
    public final float getOriginalMaximum() {
        return originalMaximum;
    }

    /** */
    public final boolean getOriginalCropCheckbox() {
        return originalCropCheckbox;
    }

    /** */
    public final boolean getOriginalDoCrop() {
        return originalDoCrop;
    }

    /** */
    public final int[] getOriginalStart() {
        return originalStart;
    }

    /** */
    public final int[] getOriginalEnd() {
        return originalEnd;
    }

    /** */
    public final boolean getHaveWindowed() {
        return haveWindowed;
    }

    /**
     * Accessor that returns the total size(length) of the data array
     * @return       int indicating the number of data points in the dataArray
     */
    public final int getSize() {
        return dataSize;
    }

    /**
     * Accessor that returns the minimum value in the dataArray
     * @return       double indicating minimum value in the dataArray
     */
    public double getMin() {
        return min;
    }

    /**
     * Accessor that returns the maximum value in the dataArray
     * @return       double indicating maximum value in the dataArray
     */
    public double getMax() {
        return max;
    }

    /**
     * Accessor that returns the minimum nonzero value in the dataArray
     * @return       double indicating minimum nonzero value in the dataArray
     *               NaN if no nonzero value present
     */
    public double getNonZeroMin() {
        return nonZeroMin;
    }

    /**
     * Accessor that returns the smallest magnitude negative value in the dataArray
     * @return       double indicating maximum nonzero value in the dataArray
     *               NaN if no nonzero value present
     */
    public double getNonZeroMax() {
        return nonZeroMax;
    }

    /**
     * Accessor that returns the smallest magnitude negative value in the dataArray
     * @return       double indicating smallest magnitude negative value in the dataArray
     *               NaN if no negative value is present
     */
    public double getSmallestMagnitudeNegative() {
        return smallestMagnitudeNegative;
    }

    /**
     * Accessor that returns the smallest magnitude positive value in the dataArray
     * @return       double indicating smallest magnitude positive value in the dataArray
     *               NaN if no positive value is present
     */
    public double getSmallestMagnitudePositive() {
        return smallestMagnitudePositive;
    }

    /**
     *  Accessor that returns the minimum red value in the dataArray
     * @return       double indicating minimum red value in the dataArray
     */
    public double getMinR() {
        return minR;
    }

    /**
     * Accessor that returns the maximum red value in the dataArray
     * @return       double indicating maximum red value in the dataArray
     */
    public double getMaxR() {
        return maxR;
    }

    /**
     *  Accessor that returns the minimum nonzero red value in the dataArray
     * @return       double indicating minimum nonzero red value in the dataArray
     *               NaN if no nonzero red value present
     */
    public double getNonZeroMinR() {
        return nonZeroMinR;
    }

    /**
     * Accessor that returns the maximum nonzero red value in the dataArray
     * @return       double indicating maximum nonzero red value in the dataArray
     *               NaN if no nonzero red value present
     */
    public double getNonZeroMaxR() {
        return nonZeroMaxR;
    }

    /**
     * Accessor that returns the smallest magnitude negative red value in the dataArray
     * @return       double indicating smallest magnitude negative red value in the dataArray
     *               NaN if no negative red value is present
     */
    public double getSmallestMagnitudeNegativeR() {
        return smallestMagnitudeNegativeR;
    }

    /**
     * Accessor that returns the smallest magnitude positive red value in the dataArray
     * @return       double indicating smallest magnitude positive red value in the dataArray
     *               NaN if no positive red value is present
     */
    public double getSmallestMagnitudePositiveR() {
        return smallestMagnitudePositiveR;
    }

    /**
     * getMinG    -   Accessor that returns the minimum green value in the dataArray
     * @return       double indicating minimum green value in the dataArray
     */
    public double getMinG() {
        return minG;
    }

    /**
     * Accessor that returns the maximum green value in the dataArray
     * @return       double indicating maximum green value in the dataArray
     */
    public double getMaxG() {
        return maxG;
    }

    /**
     *  Accessor that returns the minimum nonzero green value in the dataArray
     * @return       double indicating minimum nonzero green value in the dataArray
     *               NaN if no nonzero green value present
     */
    public double getNonZeroMinG() {
        return nonZeroMinG;
    }

    /**
     * Accessor that returns the maximum nonzero green value in the dataArray
     * @return       double indicating maximum nonzero green value in the dataArray
     *               NaN if no nonzero green value present
     */
    public double getNonZeroMaxG() {
        return nonZeroMaxG;
    }

    /**
     * Accessor that returns the smallest magnitude negative green value in the dataArray
     * @return       double indicating smallest magnitude negative green value in the dataArray
     *               NaN if no negative green value is present
     */
    public double getSmallestMagnitudeNegativeG() {
        return smallestMagnitudeNegativeG;
    }

    /**
     * Accessor that returns the smallest magnitude positive green value in the dataArray
     * @return       double indicating smallest magnitude positive green value in the dataArray
     *               NaN if no positive green value is present
     */
    public double getSmallestMagnitudePositiveG() {
        return smallestMagnitudePositiveG;
    }

    /**
     * Accessor that returns the minimum blue value in the dataArray
     * @return       double indicating minimum blue value in the dataArray
     */
    public double getMinB() {
        return minB;
    }

    /**
     *  Accessor that returns the maximum blue value in the dataArray
     * @return       double indicating maximum blue value in the dataArray
     */
    public double getMaxB() {
        return maxB;
    }

    /**
     *  Accessor that returns the minimum nonzero blue value in the dataArray
     * @return       double indicating minimum nonzero blue value in the dataArray
     *               NaN if no nonzero blue value present
     */
    public double getNonZeroMinB() {
        return nonZeroMinB;
    }

    /**
     * Accessor that returns the maximum nonzero blue value in the dataArray
     * @return       double indicating maximum nonzero blue value in the dataArray
     *               NaN if no nonzero blue value present
     */
    public double getNonZeroMaxB() {
        return nonZeroMaxB;
    }

    /**
     * Accessor that returns the smallest magnitude negative blue value in the dataArray
     * @return       double indicating smallest magnitude negative blue value in the dataArray
     *               NaN if no negative blue value is present
     */
    public double getSmallestMagnitudeNegativeB() {
        return smallestMagnitudeNegativeB;
    }

    /**
     * Accessor that returns the smallest magnitude positive blue value in the dataArray
     * @return       double indicating smallest magnitude positive blue value in the dataArray
     *               NaN if no positive blue value is present
     */
    public double getSmallestMagnitudePositiveB() {
        return smallestMagnitudePositiveB;
    }

    /**
     * Accessor that returns the minimum value without log processing in the dataArray
     * @return       double indicating minimum value in the dataArray
     */
    public double getNoLogMin() {
        return noLogMin;
    }

    /**
     * Accessor that returns the minimum nonzero value without log processing in the dataArray
     * @return       double indicating minimum value in the dataArray
     */
    public double getNoLogMinNonZero() {
        return noLogMinNonZero;
    }

    /**
     * Accessor that returns the maximum value without log processing in the dataArray
     * @return       double indicating maximum value in the dataArray
     */
    public double getNoLogMax() {
        return noLogMax;
    }

    /**
     * Accessor that returns the boolean indicating if log
     * magnitude displays are used in complex images
     * @return            boolean telling if log display is used
     */
    public boolean getLogMagDisplay() {
        return logMagDisp;
    }

    /**
     * Accessor that returns the boolean indicating if 3D
     * images are processed one slice at a time
     * @return boolean telling if slice by slice processing occurs in 3D
     */
    public boolean getImage25D() {
        return image25D;
    }

    /**
     * Accessor that returns the boolean indicating if unequal
     * dimensions are allowed in complex images
     * @return            boolean telling if unequal dimensions are allowed
     */
    public boolean getUnequalDim() {
        return unequalDim;
    }

    /**
     * Accessor that returns the lock status of the image
     * @return             lock status
     */
    public final int getLockStatus() {
        return lockStatus;
    }

    /**
     *   sets the dimExtents for this structure.
     */
    public void setExtents( int[] dims ) {
        this.dimExtents = (int[]) dims.clone();
        this.nDims = dims.length;
    } // end setExtents()

    /**
     * sets original dimensionality of the images
     * @param dims int[]  dimensionality for x,y, and z ... dimensions
     */
    public void setOriginalExtents( int[] dims ) {
        dimOriginalExtents = dims;
    }

    /**
     * @param kDim
     */
    public void setOriginalKernelDimension( int kDim ) {
        originalKernelDimension = kDim;
    }

    /**
     * @param filterConstruction
     */
    public void setOriginalFilterConstruction( int filterConstruction ) {
        originalFilterConstruction = filterConstruction;
    }

    /**
     * @param butterworthOrder
     */
    public void setOriginalButterworthOrder( int butterworthOrder ) {
        originalButterworthOrder = butterworthOrder;
    }

    /**
     *
     * @param _filterType int
     */
    public void setFilterType( int _filterType ) {
        filterType = _filterType;
    }

    /**
     *
     * @param _freq1 float
     */
    public void setFreq1( float _freq1 ) {
        freq1 = _freq1;
    }

    /**
     *
     * @param _freq2 float
     */
    public void setFreq2( float _freq2 ) {
        freq2 = _freq2;
    }

    /**
     *
     * @param minimum float
     */
    public void setOriginalMinimum( float minimum ) {
        originalMinimum = minimum;
    }

    /**
     *
     * @param maximum float
     */
    public void setOriginalMaximum( float maximum ) {
        originalMaximum = maximum;
    }

    /**
     *
     * @param cropCheckbox boolean
     */
    public void setOriginalCropCheckbox( boolean cropCheckbox ) {
        originalCropCheckbox = cropCheckbox;
    }

    /**
     *
     * @param doCrop boolean
     */
    public void setOriginalDoCrop( boolean doCrop ) {
        originalDoCrop = doCrop;
    }

    /**
     *
     * @param start int[]
     */
    public void setOriginalStart( int[] start ) {
        originalStart = start;
    }

    /**
     *
     * @param end int[]
     */
    public void setOriginalEnd( int[] end ) {
        originalEnd = end;
    }

    /**
     *
     * @param window boolean
     */
    public void setHaveWindowed( boolean window ) {
        haveWindowed = window;
    }

    /**
     * Accessor that sets the minimum value in the dataArray
     * @param _min   double indicating minimum value in the dataArray
     */
    public void setMin( double _min ) {
        min = _min;
    }

    /**
     * Accessor that sets the maximum value in the dataArray
     * @param _max  double indicating maximum value in the dataArray
     */
    public void setMax( double _max ) {
        max = _max;
    }

    /**
     *  Accessor that sets the boolean telling if
     *                    log magnitude display is used in a complex image.
     *@param logMagDisplay
     */
    public void setLogMagDisplay( boolean logMagDisplay ) {
        logMagDisp = logMagDisplay;
    }

    /**
     *  Accessor that sets the boolean telling if 3D images are processed one slice
     *  at a time
     *  @param _image25D
     */
    public void setImage25D( boolean _image25D ) {
        image25D = _image25D;
    }

    /**
     *  Accessor that sets the boolean telling if
     *  unequal dimesnions are allowed in a complex image.
     *  @param unequalDimension
     */
    public void setUnequalDim( boolean unequalDimension ) {
        unequalDim = unequalDimension;
    }

    /**
     * Sets the lockFlag to protect data. When the flag is set
     * no other processes can read or write the data
     */
    public final synchronized void setLock( int lockType )
        throws IOException {

        if ( lockStatus == RW_LOCKED ) {
            throw new IOException( "ModelStorageBase: Image locked !" );
        } else if ( lockStatus == UNLOCKED && lockType == W_LOCKED ) {
            writeLockCount++;
            lockStatus = lockType;
        } else if ( lockStatus == W_LOCKED && lockType == W_LOCKED ) {
            writeLockCount++;
        } else if ( lockStatus == W_LOCKED && lockType == RW_LOCKED ) {
            throw new IOException( "ModelStorageBase: Image locked !" );
        } else if ( lockStatus == UNLOCKED && lockType == RW_LOCKED ) {
            lockStatus = lockType;
        }
    }

    /**
     * Releases the lock so that other proceses can read or
     * write the data
     */
    public final synchronized void releaseLock() {

        if ( lockStatus == W_LOCKED ) {
            writeLockCount--;
            if ( writeLockCount <= 0 ) {
                lockStatus = UNLOCKED;
                return;
            }
        } else if ( lockStatus == RW_LOCKED ) {
            lockStatus = UNLOCKED;
            writeLockCount = 0; // Just make sure its zero.
        }
    }

    /**
     * Sets the lockFlag to protect data. When the flag is set
     * no other processes can read or write the data
     */
    public void setLock()
        throws IOException {

        if ( lockStatus == UNLOCKED ) {
            lockStatus = RW_LOCKED;
        } else {
            throw new IOException( "ModelStorageBase: Image locked !" );
        }
    }

    /** calculates the min and max values for the image array  */
    public void calcMinMax() {

        // before computing the new min, max, save the last values.
        lastMin = min;
        lastMax = max;

        min = Double.POSITIVE_INFINITY;
        max = Double.NEGATIVE_INFINITY;

        int i;
        double value;

        if ( bufferType == BOOLEAN ) {
            min = 0;
            max = 1;
        } else if ( bufferType != ARGB && bufferType != ARGB_USHORT && bufferType != ARGB_FLOAT ) {
            for ( i = 0; i < dataSize; i++ ) {
                value = data.getDouble( i );
                if ( value > max ) {
                    max = value;
                }
                if ( value < min ) {
                    min = value;
                }
            }
        } else { // color
            minR = Double.POSITIVE_INFINITY;
            maxR = Double.NEGATIVE_INFINITY;
            minG = Double.POSITIVE_INFINITY;
            maxG = Double.NEGATIVE_INFINITY;
            minB = Double.POSITIVE_INFINITY;
            maxB = Double.NEGATIVE_INFINITY;
            for ( i = 0; i < dataSize; i += 4 ) {
                value = data.getDouble( i + 1 );
                if ( value > maxR ) {
                    maxR = value;
                }
                if ( value < minR ) {
                    minR = value;
                }

                value = data.getDouble( i + 2 );
                if ( value > maxG ) {
                    maxG = value;
                }
                if ( value < minG ) {
                    minG = value;
                }

                value = data.getDouble( i + 3 );
                if ( value > maxB ) {
                    maxB = value;
                }
                if ( value < minB ) {
                    minB = value;
                }
            } // for(i= 0; i < dataSize; i+=4)
            min = Math.min( minR, Math.min( minG, minB ) );
            max = Math.max( maxR, Math.max( maxG, maxB ) );
        } // else color

    }

    /** calculates the min and max nonzero values for the image array  */
    public void calcMinMaxNonZero() {
        boolean foundNonZeroMin;
        boolean foundNonZeroMax;
        boolean foundSmallestMagnitudeNegative;
        boolean foundSmallestMagnitudePositive;
        boolean foundNonZeroMinR;
        boolean foundNonZeroMaxR;
        boolean foundSmallestMagnitudeNegativeR;
        boolean foundSmallestMagnitudePositiveR;
        boolean foundNonZeroMinG;
        boolean foundNonZeroMaxG;
        boolean foundSmallestMagnitudeNegativeG;
        boolean foundSmallestMagnitudePositiveG;
        boolean foundNonZeroMinB;
        boolean foundNonZeroMaxB;
        boolean foundSmallestMagnitudeNegativeB;
        boolean foundSmallestMagnitudePositiveB;

        nonZeroMin = Double.POSITIVE_INFINITY;
        nonZeroMax = -Double.POSITIVE_INFINITY;
        smallestMagnitudeNegative = -Double.POSITIVE_INFINITY;
        smallestMagnitudePositive = Double.POSITIVE_INFINITY;
        nonZeroMinR = Double.POSITIVE_INFINITY;
        nonZeroMaxR = -Double.POSITIVE_INFINITY;
        smallestMagnitudeNegativeR = -Double.POSITIVE_INFINITY;
        smallestMagnitudePositiveR = Double.POSITIVE_INFINITY;
        nonZeroMinG = Double.POSITIVE_INFINITY;
        nonZeroMaxG = -Double.POSITIVE_INFINITY;
        smallestMagnitudeNegativeG = -Double.POSITIVE_INFINITY;
        smallestMagnitudePositiveG = Double.POSITIVE_INFINITY;
        nonZeroMinB = Double.POSITIVE_INFINITY;
        nonZeroMaxB = -Double.POSITIVE_INFINITY;
        smallestMagnitudeNegativeB = -Double.POSITIVE_INFINITY;
        smallestMagnitudePositiveB = Double.POSITIVE_INFINITY;

        int i;
        double value;

        if ( bufferType == BOOLEAN ) {
            nonZeroMin = 1;
            nonZeroMax = 1;
            smallestMagnitudeNegative = Double.NaN;
            smallestMagnitudePositive = 1;
        } else if ( bufferType != ARGB && bufferType != ARGB_USHORT && bufferType != ARGB_FLOAT ) {
            foundNonZeroMin = false;
            foundNonZeroMax = false;
            foundSmallestMagnitudeNegative = false;
            foundSmallestMagnitudePositive = false;
            for ( i = 0; i < dataSize; i++ ) {
                value = data.getDouble( i );
                if ( value != 0.0 ) {
                    if ( value > nonZeroMax ) {
                        nonZeroMax = value;
                        foundNonZeroMax = true;
                    }
                    if ( value < nonZeroMin ) {
                        nonZeroMin = value;
                        foundNonZeroMin = true;
                    }
                    if ( ( value > 0.0 ) && ( value < smallestMagnitudePositive ) ) {
                        smallestMagnitudePositive = value;
                        foundSmallestMagnitudePositive = true;
                    }
                    if ( ( value < 0.0 ) && ( value > smallestMagnitudeNegative ) ) {
                        smallestMagnitudeNegative = value;
                        foundSmallestMagnitudeNegative = true;
                    }
                } // if (value != 0.0)
            } // for(i= 0; i < dataSize; i++)
            if ( !foundNonZeroMin ) {
                nonZeroMin = Double.NaN;
            }
            if ( !foundNonZeroMax ) {
                nonZeroMax = Double.NaN;
            }
            if ( !foundSmallestMagnitudePositive ) {
                smallestMagnitudePositive = Double.NaN;
            }
            if ( !foundSmallestMagnitudeNegative ) {
                smallestMagnitudeNegative = Double.NaN;
            }
        } // else if (bufferType != ARGB &&...)
        else { // color
            foundNonZeroMinR = false;
            foundNonZeroMaxR = false;
            foundSmallestMagnitudeNegativeR = false;
            foundSmallestMagnitudePositiveR = false;
            foundNonZeroMinG = false;
            foundNonZeroMaxG = false;
            foundSmallestMagnitudeNegativeG = false;
            foundSmallestMagnitudePositiveG = false;
            foundNonZeroMinB = false;
            foundNonZeroMaxB = false;
            foundSmallestMagnitudeNegativeB = false;
            foundSmallestMagnitudePositiveB = false;
            for ( i = 0; i < dataSize; i += 4 ) {
                value = Math.abs( data.getDouble( i + 1 ) );
                if ( value != 0.0 ) {
                    if ( value > nonZeroMaxR ) {
                        nonZeroMaxR = value;
                        foundNonZeroMaxR = true;
                    }
                    if ( value < nonZeroMinR ) {
                        nonZeroMinR = value;
                        foundNonZeroMinR = true;
                    }
                    if ( ( value > 0.0 ) && ( value < smallestMagnitudePositiveR ) ) {
                        smallestMagnitudePositiveR = value;
                        foundSmallestMagnitudePositiveR = true;
                    }
                    if ( ( value < 0.0 ) && ( value > smallestMagnitudeNegativeR ) ) {
                        smallestMagnitudeNegativeR = value;
                        foundSmallestMagnitudeNegativeR = true;
                    }
                } // if (value != 0.0)

                value = Math.abs( data.getDouble( i + 2 ) );
                if ( value != 0.0 ) {
                    if ( value > nonZeroMaxG ) {
                        nonZeroMaxG = value;
                        foundNonZeroMaxG = true;
                    }
                    if ( value < nonZeroMinG ) {
                        nonZeroMinG = value;
                        foundNonZeroMinG = true;
                    }
                    if ( ( value > 0.0 ) && ( value < smallestMagnitudePositiveG ) ) {
                        smallestMagnitudePositiveG = value;
                        foundSmallestMagnitudePositiveG = true;
                    }
                    if ( ( value < 0.0 ) && ( value > smallestMagnitudeNegativeG ) ) {
                        smallestMagnitudeNegativeG = value;
                        foundSmallestMagnitudeNegativeG = true;
                    }
                } // if (value != 0.0)

                value = Math.abs( data.getDouble( i + 3 ) );
                if ( value != 0.0 ) {
                    if ( value > nonZeroMaxB ) {
                        nonZeroMaxB = value;
                        foundNonZeroMaxB = true;
                    }
                    if ( value < nonZeroMinB ) {
                        nonZeroMinB = value;
                        foundNonZeroMinB = true;
                    }
                    if ( ( value > 0.0 ) && ( value < smallestMagnitudePositiveB ) ) {
                        smallestMagnitudePositiveB = value;
                        foundSmallestMagnitudePositiveB = true;
                    }
                    if ( ( value < 0.0 ) && ( value > smallestMagnitudeNegativeB ) ) {
                        smallestMagnitudeNegativeB = value;
                        foundSmallestMagnitudeNegativeB = true;
                    }
                } // if (value != 0.0)
            } // for(i= 0; i < dataSize; i+=4)
            if ( !foundNonZeroMinR ) {
                nonZeroMinR = Double.NaN;
            }
            if ( !foundNonZeroMaxR ) {
                nonZeroMaxR = Double.NaN;
            }
            if ( !foundSmallestMagnitudePositiveR ) {
                smallestMagnitudePositiveR = Double.NaN;
            }
            if ( !foundSmallestMagnitudeNegativeR ) {
                smallestMagnitudeNegativeR = Double.NaN;
            }
            if ( !foundNonZeroMinG ) {
                nonZeroMinG = Double.NaN;
            }
            if ( !foundNonZeroMaxG ) {
                nonZeroMaxG = Double.NaN;
            }
            if ( !foundSmallestMagnitudePositiveG ) {
                smallestMagnitudePositiveG = Double.NaN;
            }
            if ( !foundSmallestMagnitudeNegativeG ) {
                smallestMagnitudeNegativeG = Double.NaN;
            }
            if ( !foundNonZeroMinB ) {
                nonZeroMinB = Double.NaN;
            }
            if ( !foundNonZeroMaxB ) {
                nonZeroMaxB = Double.NaN;
            }
            if ( !foundSmallestMagnitudePositiveB ) {
                smallestMagnitudePositiveB = Double.NaN;
            }
            if ( !foundSmallestMagnitudeNegativeB ) {
                smallestMagnitudeNegativeB = Double.NaN;
            }
        } // else color

    }

    /**
     *  calculates the min and max magnitude values for the image array
     *  @param logMagDisplay if true calculate min and max for log10 of 1 + magnitude
     */
    public void calcMinMaxMag( boolean logMagDisplay ) {
        min = Double.POSITIVE_INFINITY;
        max = Double.NEGATIVE_INFINITY;
        noLogMin = Double.POSITIVE_INFINITY;
        noLogMinNonZero = Double.POSITIVE_INFINITY;
        noLogMax = Double.NEGATIVE_INFINITY;

        int i;
        double valuer, valuei, value;

        for ( i = 0; i < dataSize; i = i + 2 ) {
            valuer = data.getDouble( i );
            valuei = data.getDouble( i + 1 );
            value = java.lang.Math.sqrt( valuer * valuer + valuei * valuei );
            if ( logMagDisplay ) {
                if ( value > noLogMax ) {
                    noLogMax = value;
                }
                if ( value < noLogMin ) {
                    noLogMin = value;
                }
                if ( ( value > 0 ) && ( value < noLogMinNonZero ) ) {
                    noLogMinNonZero = value;
                }
                value = 0.4342944819 * java.lang.Math.log( 1 + value );
            }
            if ( value > max ) {
                max = value;
            }
            if ( value < min ) {
                min = value;
            }
        }

        for ( i = 0; i < fileInfo.length; i++ ) {
            fileInfo[i].setMin( getMin() );
            fileInfo[i].setMax( getMax() );
        }

    }

    /**
     * Accessor that sets the project information for this image
     * @param pInfo structure
     */
    public void setProjectInfo( FileInfoProject pInfo ) {
        if ( pInfo != null ) {
            projectInfo = pInfo;
        }
    }

    /**
     * Accessor that sets the entire fileInfo array of the image
     * @param           fInfo structure
     */
    public void setFileInfo( FileInfoBase[] fInfo ) {
        if ( fInfo != null ) {
            fileInfo = fInfo;
        }
    }

    /**
     * Accessor that sets the fileInfo class for the image
     * @param fInfo      fileInfo structure.
     */
    public void setFileInfo( FileInfoBase fInfo, int i ) {
        fileInfo[i] = fInfo;
    }

    /*****************************************************************************/

    /********************************* Number ***********************************/

    /*****************************************************************************/

    /**
     *   color function to get data where bounds checking is performed
     *   @param position   position in one dimensional array
     *   @param color
     *   @return           returns value if position in data array range
     */
    public final Number getC( int position, int color ) {
        if ( ( position >= 0 ) && ( ( 4 * position + color ) < dataSize ) ) {
            return ( data.get( 4 * position + color ) );
        }
        return ( new Byte( (byte) 0 ) );
    }

    /**
     *   function to get data where bounds checking is performed
     *   @param position   position in one dimensional array
     *   @return           returns value if position in data array range
     */
    public final Number get( int position ) {
        if ( ( position >= 0 ) && ( position < dataSize ) ) {
            return ( data.get( position ) );
        }
        return ( new Byte( (byte) 0 ) );
    }

    /**
     *   2D get data fuction where bounds checking is performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           returns value if position in data array range
     */
    public final Number get( int x, int y ) {
        int position;

        if ( nDims == 2 ) {
            position = y * dimExtents[0] + x;
            if ( ( position >= 0 ) && ( position < dataSize ) ) {
                return data.get( position );
            }
            return ( new Byte( (byte) 0 ) );
        }
        return ( new Byte( (byte) 0 ) );
    }

    /**
     *   3D get data fuction where bounds checking is performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           returns value if position in data array range
     */
    public final Number get( int x, int y, int z ) {
        int position;

        if ( nDims == 3 ) {
            position = z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x;
            if ( ( position >= 0 ) && ( position < dataSize ) ) {
                return data.get( position );
            }
            return ( new Byte( (byte) 0 ) );
        }
        return ( new Byte( (byte) 0 ) );
    }

    /**
     *   4D get data fuction where bounds checking is performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate (ie. multi-modality images or time)
     *   @return           returns true if position in data array range
     */
    public final Number get( int x, int y, int z, int b ) {
        int position;

        if ( nDims == 4 ) {
            position = b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                    + y * dimExtents[0] + x;
            if ( ( position >= 0 ) && ( position < dataSize ) ) {
                return data.get( position );
            }
            return ( new Byte( (byte) 0 ) );
        }
        return ( new Byte( (byte) 0 ) );
    }

    /**
     *   nD get data fuction where bounds checking is performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final Number get( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        if ( nDims == dimensions ) {
            for ( i = dimensions - 1; i > 0; i-- ) {
                location += ( position[i] * dimExtents[i] );
            }
            location += position[0];
            if ( ( location >= 0 ) && ( location < dataSize ) ) {
                return data.get( location );
            }
            return ( new Byte( (byte) 0 ) );
        }
        return ( new Byte( (byte) 0 ) );
    }

    /**
     *   version of get that performs bi-linear interpoloation.
     *                     Note - does perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final Number getBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        float dx, dy;
        float x1, x2;
        int intX, intY;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        if ( ( position >= 0 ) && ( position < dataSize - xDim - 1 ) ) {
            x1 = ( 1 - dx ) * ( data.get( position ) ).floatValue() + dx * ( data.get( position + 1 ) ).floatValue();

            x2 = ( 1 - dx ) * ( data.get( position + xDim ) ).floatValue()
                    + dx * ( data.get( position + xDim + 1 ) ).floatValue();

            return ( new Float( ( 1 - dy ) * x1 + dy * x2 ) );
        } else {
            return ( new Byte( (byte) 0 ) );
        }
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                     <b>Note - does perform bounds checking</b>
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final Number getTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        if ( ( position1 >= 0 ) && ( position1 < dataSize - xDim * yDim - 1 ) && ( position2 >= 0 )
                && ( position2 < dataSize - xDim * yDim - 1 ) ) {
            a1 = ( 1 - dx ) * data.get( position1 ).floatValue() + dx * data.get( position1 + 1 ).floatValue();
            a2 = ( 1 - dx ) * data.get( position1 + xDim ).floatValue()
                    + dx * data.get( position1 + xDim + 1 ).floatValue();
            b1 = ( 1 - dy ) * a1 + dy * a2;

            a1 = ( 1 - dx ) * data.get( position2 ).floatValue() + dx * data.get( position2 + 1 ).floatValue();
            a2 = ( 1 - dx ) * data.get( position2 + xDim ).floatValue()
                    + dx * data.get( position2 + xDim + 1 ).floatValue();
            b2 = ( 1 - dy ) * a1 + dy * a2;

            return ( new Float( ( 1 - dz ) * b1 + dz * b2 ) );
        } else {
            return ( new Byte( (byte) 0 ) );
        }
    }

    /**
     *   export data into values array WITHOUT locking
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final void exportDataNoLock( int start, int length, Number[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.get( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data into values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final void exportData( int start, int length, Number[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.get( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportSliceXY( int slice, Number[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXZ( int slice, Number[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.get( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZY( int slice, Number[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.get( slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   color set that does perform bounds checking
     *   @param position   position in one dimensional array
     *   @param color
     *   @param value
     *   @return           returns value if position in data array range
     */
    public final boolean setC( int position, int color, Number value ) {
        if ( ( position >= 0 ) && ( ( 4 * position + color ) < dataSize ) ) {
            data.set( ( 4 * position + color ), value );
            return true;
        }
        return false;
    }

    /**
     *   set that does perform bounds checking
     *   @param position   position in one dimensional array
     *   @param value
     *   @return           returns value if position in data array range
     */
    public final boolean set( int position, Number value ) {
        if ( ( position >= 0 ) && ( position < dataSize ) ) {
            data.set( position, value );
            return true;
        }
        return false;
    }

    /**
     *   2D get data fuction where bounds checking is performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param value      set data array to this value
     *   @return           returns true if position in data array range
     */
    public final boolean set( int x, int y, Number value ) {
        int position;

        if ( nDims == 2 ) {
            position = y * dimExtents[0] + x;
            if ( ( position >= 0 ) && ( position < dataSize ) ) {
                data.set( position, value );
                return true;
            }
            return false;
        }
        return false;
    }

    /**
     *   3D set data fuction where bounds checking is performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param value      set data array to this value
     *   @return           returns true if position in data array range
     */
    public final boolean set( int x, int y, int z, Number value ) {
        int position;

        if ( nDims == 3 ) {
            position = z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x;
            if ( ( position >= 0 ) && ( position < dataSize ) ) {
                data.set( position, value );
                return true;
            }
            return false;
        }
        return false;
    }

    /**
     *   3D color set data fuction where bounds checking is performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param c          A,R,G, or B
     *   @param value      set data array to this value
     *   @return           returns true if position in data array range
     */
    public final boolean setC( int x, int y, int z, int c, Number value ) {
        int position;

        if ( nDims == 3 ) {
            position = 4 * ( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) + c;
            if ( ( position >= 0 ) && ( position < dataSize ) ) {
                data.set( position, value );
                return true;
            }
            return false;
        }
        return false;
    }

    /**
     *   4D get data fuction where bounds checking is performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate (ie. multi modality images or time)
     *   @param value      set data array to this value
     *   @return           returns true if position in data array range
     */
    public final boolean set( int x, int y, int z, int b, Number value ) {
        int position;

        if ( nDims == 4 ) {
            position = b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                    + y * dimExtents[0] + x;
            if ( ( position >= 0 ) && ( position < dataSize ) ) {
                data.set( position, value );
                return true;
            }
            return false;
        }
        return false;
    }

    /**
     *   nD set data fuction where bounds checking is performed
     *   @param position   array
     *   @param value    single value array to return data
     *   @return           returns true if position in data array range
     */
    public final boolean set( int[] position, Number value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        if ( nDims == dimensions ) {
            for ( i = dimensions - 1; i > 0; i-- ) {
                location += ( position[i] * dimExtents[i] );
            }
            location += position[0];
            if ( ( location >= 0 ) && ( location < dataSize ) ) {
                data.set( location, value );
                return true;
            }
            return false;
        }
        return false;
    }

    /**
     *   import Number data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importData( int start, Number[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.set( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /*****************************************************************************/

    /********************************* Boolean ***********************************/

    /*****************************************************************************/

    /*
     *   version of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final boolean getBoolean( int position ) {
        return ( data.getBoolean( position ) );
    }

    /**
     *   2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value
     */
    public final boolean getBoolean( int x, int y ) {
        return ( data.getBoolean( y * dimExtents[0] + x ) );
    }

    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value
     */
    public final boolean getBoolean( int x, int y, int z ) {
        return ( data.getBoolean( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value
     */
    public final boolean getBoolean( int x, int y, int z, int b ) {
        return ( data.getBoolean(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x ) );
    }

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final boolean getBoolean( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getBoolean( location ) );
    }

    /**
     *   export data into values array WITHOUT locking
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataNoLock( int start, int length, BitSet values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.size() ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                if ( data.getBoolean( i ) ) {
                    values.set( j );
                } else {
                    values.clear( j );
                }
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data into values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportData( int start, int length, BitSet values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.size() ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                if ( data.getBoolean( i ) ) {
                    values.set( j );
                } else {
                    values.clear( j );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportSliceXY( int slice, BitSet values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXZ( int slice, BitSet values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    if ( data.getBoolean( i ) ) {
                        values.set( j );
                    } else {
                        values.clear( j );
                    }
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZY( int slice, BitSet values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    if ( data.getBoolean( i ) ) {
                        values.set( j );
                    } else {
                        values.clear( j );
                    }
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set boolean methods *************************/

    /*
     *   version of set that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final void set( int position, boolean value ) {
        data.setBoolean( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void set( int x, int y, boolean value ) {
        data.setBoolean( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void set( int x, int y, int z, boolean value ) {
        data.setBoolean( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void set( int x, int y, int z, int b, boolean value ) {
        data.setBoolean(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void set( int[] position, boolean value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setBoolean( location, value );
    }

    /**
     *   import boolean (BitSet) data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importData( int start, BitSet values, boolean mmFlag )
        throws IOException {

        int length = values.size();
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setBoolean( ptr, values.get( i ) );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /*****************************************************************************/

    /********************************** Byte *************************************/

    /*****************************************************************************/

    /*
     *   version of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     *   @return            value at that position in the data array
     */
    public final byte getByte( int position ) {
        return ( data.getByte( position ) );
    }

    /**
     *   2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value at that position in the data array
     */
    public final byte getByte( int x, int y ) {
        return ( data.getByte( y * dimExtents[0] + x ) );
    }

    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final byte getByte( int x, int y, int z ) {
        return ( data.getByte( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final byte getByte( int x, int y, int z, int b ) {
        return ( data.getByte(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x ) );
    }

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final byte getByte( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getByte( location ) );
    }

    /**
     *   version of get that performs bi-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final byte getByteBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( 1 - dx ) * data.getByte( position ) + dx * data.getByte( position + 1 );
        x2 = ( 1 - dx ) * data.getByte( position + xDim ) + dx * data.getByte( position + xDim + 1 );

        return (byte) ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                     <b>Note - does NOT perform bounds checking</b>
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final byte getByteTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        a1 = ( 1 - dx ) * data.getByte( position1 ) + dx * data.getByte( position1 + 1 );
        a2 = ( 1 - dx ) * data.getByte( position1 + xDim ) + dx * data.getByte( position1 + xDim + 1 );
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * data.getByte( position2 ) + dx * data.getByte( position2 + 1 );
        a2 = ( 1 - dx ) * data.getByte( position2 + xDim ) + dx * data.getByte( position2 + xDim + 1 );
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return (byte) ( ( 1 - dz ) * b1 + dz * b2 );
    }

    /**
     *   export data into values array WITHOUT locking
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataNoLock( int start, int length, byte[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getByte( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data into values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportData( int start, int length, byte[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getByte( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportSliceXY( int slice, byte[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXZ( int slice, byte[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getByte( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZY( int slice, byte[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getByte( slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set byte methods *************************/

    /**
     *   version of set that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final void set( int position, byte value ) {
        data.setByte( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void set( int x, int y, byte value ) {
        data.setByte( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void set( int x, int y, int z, byte value ) {
        data.setByte( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void set( int x, int y, int z, int b, byte value ) {
        data.setByte(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void set( int[] position, byte value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setByte( location, value );
    }

    /**
     *   import boolean data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importData( int start, boolean[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setBoolean( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /**
     *   import byte data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importData( int start, byte[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setByte( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /*****************************************************************************/

    /************************* Unsigned Byte *************************************/

    /*****************************************************************************/

    /**
     *   version of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final short getUByte( int position ) {
        return ( data.getUByte( position ) );
    }

    /**
     *    2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value at that position in the data array
     */
    public final short getUByte( int x, int y ) {
        return ( data.getUByte( y * dimExtents[0] + x ) );
    }

    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final short getUByte( int x, int y, int z ) {
        return ( data.getUByte( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /*
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     *
     public final short getUByte(int x, int y, int z, int b) {
     return (data.getUByte(b*(dimExtents[0]*dimExtents[1]*dimExtents[2]) +
     z*(dimExtents[0]*dimExtents[1]) +
     y*dimExtents[0] +
     x));
     }
     */

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final short getUByte( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getUByte( location ) );
    }

    /**
     *   version of get that performs bi-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final short getUByteBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( 1 - dx ) * data.getUByte( position ) + dx * data.getUByte( position + 1 );
        x2 = ( 1 - dx ) * data.getUByte( position + xDim ) + dx * data.getUByte( position + xDim + 1 );

        return (short) ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                    <b> Note - does NOT perform bounds checking</b>
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final short getUByteTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;
        a1 = ( 1 - dx ) * data.getByte( position1 ) + dx * data.getByte( position1 + 1 );
        a2 = ( 1 - dx ) * data.getByte( position1 + xDim ) + dx * data.getByte( position1 + xDim + 1 );
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * data.getByte( position2 ) + dx * data.getByte( position2 + 1 );
        a2 = ( 1 - dx ) * data.getByte( position2 + xDim ) + dx * data.getByte( position2 + xDim + 1 );
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return (short) ( ( 1 - dz ) * b1 + dz * b2 );
    }

    /**
     *   export data into values array WITHOUT locking
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUDataNoLock( int start, int length, short[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getUByte( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data into values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUData( int start, int length, short[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getUByte( i );
            }

            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportUSliceXY( int slice, short[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUSliceXZ( int slice, short[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getUByte( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUSliceZY( int slice, short[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getUByte( slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set unsigned byte methods *************************/

    /**
     *   version of set that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final void setUByte( int position, short value ) {
        data.setUByte( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void setUByte( int x, int y, short value ) {
        data.setUByte( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void setUByte( int x, int y, int z, short value ) {
        data.setUByte( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void setUByte( int x, int y, int z, int b, short value ) {
        data.setUByte(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void setUByte( int[] position, short value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setUByte( location, value );
    }

    /**
     *   import short data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importUData( int start, short[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setUByte( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /*****************************************************************************/

    /********************************** Short ************************************/

    /*****************************************************************************/

    /**
     *   version of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final short getShort( int position ) {
        return ( data.getShort( position ) );
    }

    /**
     *   2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value at that position in the data array
     */
    public final short getShort( int x, int y ) {
        return ( data.getShort( y * dimExtents[0] + x ) );
    }

    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final short getShort( int x, int y, int z ) {
        return ( data.getShort( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final short getShort( int x, int y, int z, int b ) {
        return ( data.getShort(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x ) );
    }

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final short getShort( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getShort( location ) );
    }

    /**
     *   version of get that performs bi-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final short getShortBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( 1 - dx ) * data.getShort( position ) + dx * data.getShort( position + 1 );
        x2 = ( 1 - dx ) * data.getShort( position + xDim ) + dx * data.getShort( position + xDim + 1 );

        return (short) ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                     <b>Note - does NOT perform bounds checking</b>
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final short getShortTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        a1 = ( 1 - dx ) * data.getShort( position1 ) + dx * data.getShort( position1 + 1 );
        a2 = ( 1 - dx ) * data.getShort( position1 + xDim ) + dx * data.getShort( position1 + xDim + 1 );
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * data.getShort( position2 ) + dx * data.getShort( position2 + 1 );
        a2 = ( 1 - dx ) * data.getShort( position2 + xDim ) + dx * data.getShort( position2 + xDim + 1 );
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return (short) ( ( 1 - dz ) * b1 + dz * b2 );
    }

    /**
     *   export data into values array WITHOUT locking
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataNoLock( int start, int length, short[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getShort( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data into values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportData( int start, int length, short[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getShort( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportSliceXY( int slice, short[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXZ( int slice, short[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getShort( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZY( int slice, short[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getShort( slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set short methods *************************/

    /**
     *   version of set that does NOT perform bounds checking
     *   @param position  position in one dimensional array
     */
    public final void setShort( int position, short value ) {
        data.setShort( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void setShort( int x, int y, short value ) {
        data.setShort( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void set( int x, int y, int z, short value ) {
        data.setShort( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void set( int x, int y, int z, int b, short value ) {
        data.setShort(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void set( int[] position, short value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setShort( location, value );
    }

    /**
     *   import short data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importData( int start, short[] values, boolean mmFlag )
        throws IOException {

        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setShort( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /*****************************************************************************/

    /************************* Unsigned Short ************************************/

    /*****************************************************************************/

    /**
     *   version of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     *   @return           value at that position in the data array
     */
    public final int getUShort( int position ) {
        return ( data.getUShort( position ) );
    }

    /**
     *   2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value at that position in the data array
     */
    public final int getUShort( int x, int y ) {
        return ( data.getUShort( y * dimExtents[0] + x ) );
    }

    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final int getUShort( int x, int y, int z ) {
        return ( data.getUShort( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final int getUShort( int x, int y, int z, int b ) {
        return ( data.getUShort(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x ) );
    }

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final int getUShort( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getUShort( location ) );
    }

    /**
     *   version of get that performs bi-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final int getUShortBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( 1 - dx ) * data.getUShort( position ) + dx * data.getUShort( position + 1 );
        x2 = ( 1 - dx ) * data.getUShort( position + xDim ) + dx * data.getUShort( position + xDim + 1 );

        return (int) ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final int getUShortTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        a1 = ( 1 - dx ) * data.getUShort( position1 ) + dx * data.getUShort( position1 + 1 );
        a2 = ( 1 - dx ) * data.getUShort( position1 + xDim ) + dx * data.getUShort( position1 + xDim + 1 );
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * data.getUShort( position2 ) + dx * data.getUShort( position2 + 1 );
        a2 = ( 1 - dx ) * data.getUShort( position2 + xDim ) + dx * data.getUShort( position2 + xDim + 1 );
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return (int) ( ( 1 - dz ) * b1 + dz * b2 );
    }

    /**
     *   export data into values array WITHOUT LOCKING
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUDataNoLock( int start, int length, int[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getUShort( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data in values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUData( int start, int length, int[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getUShort( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportUSliceXY( int slice, int[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUSliceXZ( int slice, int[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getUShort( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUSliceZY( int slice, int[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getUShort( slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set unsigned short methods ***********************/

    /**
     *   version of set that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final void setUShort( int position, int value ) {
        data.setUShort( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void setUShort( int x, int y, int value ) {
        data.setUShort( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void setUShort( int x, int y, int z, int value ) {
        data.setUShort( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void setUShort( int x, int y, int z, int b, int value ) {
        data.setUShort(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void setUShort( int[] position, int value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setUShort( location, value );
    }

    /**
     *   import int data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importUData( int start, int[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setUShort( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /*****************************************************************************/

    /********************************** Int ************************************/

    /*****************************************************************************/

    /**
     *   version of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     *   @return           value at that position in the data array
     */
    public final int getInt( int position ) {
        return ( data.getInt( position ) );
    }

    /**
     *   2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value at that position in the data array
     */
    public final int getInt( int x, int y ) {
        return ( data.getInt( y * dimExtents[0] + x ) );
    }

    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final int getInt( int x, int y, int z ) {
        return ( data.getInt( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final int getInt( int x, int y, int z, int b ) {
        return ( data.getInt(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x ) );
    }

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final int getInt( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getInt( location ) );
    }

    /**
     *   version of get that performs bi-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final int getIntBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( 1 - dx ) * data.getInt( position ) + dx * data.getInt( position + 1 );
        x2 = ( 1 - dx ) * data.getInt( position + xDim ) + dx * data.getInt( position + xDim + 1 );

        return (int) ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final int getIntTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        a1 = ( 1 - dx ) * data.getInt( position1 ) + dx * data.getInt( position1 + 1 );
        a2 = ( 1 - dx ) * data.getInt( position1 + xDim ) + dx * data.getInt( position1 + xDim + 1 );
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * data.getShort( position2 ) + dx * data.getShort( position2 + 1 );
        a2 = ( 1 - dx ) * data.getShort( position2 + xDim ) + dx * data.getShort( position2 + xDim + 1 );
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return (int) ( ( 1 - dz ) * b1 + dz * b2 );
    }

    /**
     *   export data into values array WITHOUT locking
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataNoLock( int start, int length, int[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getInt( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   exportData      - export data into values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportData( int start, int length, int[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getInt( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportSliceXY( int slice, int[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXZ( int slice, int[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getInt( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZY( int slice, int[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getInt( slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set int methods *************************/

    /**
     *   version of set that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final void set( int position, int value ) {
        data.setInt( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void set( int x, int y, int value ) {
        data.setInt( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void set( int x, int y, int z, int value ) {
        data.setInt( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void set( int x, int y, int z, int b, int value ) {
        data.setInt(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void set( int[] position, int value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setInt( location, value );
    }

    /**
     *   import integer data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importData( int start, int[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setInt( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /*****************************************************************************/

    /************************* Unsigned Integer ************************************/

    /*****************************************************************************/

    /**
     *   version of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     *   @return           value at that position in the data array
     */
    public final long getUInt( int position ) {
        return ( data.getUInt( position ) );
    }

    /**
     *   2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value at that position in the data array
     */
    public final long getUInt( int x, int y ) {
        return ( data.getUInt( y * dimExtents[0] + x ) );
    }

    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final long getUInt( int x, int y, int z ) {
        return ( data.getUInt( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final long getUInt( int x, int y, int z, int b ) {
        return ( data.getUInt(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x ) );
    }

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final long getUInt( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getUInt( location ) );
    }

    /**
     *   version of get that performs bi-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final long getUIntBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( 1 - dx ) * data.getUInt( position ) + dx * data.getUInt( position + 1 );
        x2 = ( 1 - dx ) * data.getUInt( position + xDim ) + dx * data.getUInt( position + xDim + 1 );

        return (long) ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final long getUIntTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        a1 = ( 1 - dx ) * data.getUInt( position1 ) + dx * data.getUInt( position1 + 1 );
        a2 = ( 1 - dx ) * data.getUInt( position1 + xDim ) + dx * data.getUInt( position1 + xDim + 1 );
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * data.getUInt( position2 ) + dx * data.getUInt( position2 + 1 );
        a2 = ( 1 - dx ) * data.getUInt( position2 + xDim ) + dx * data.getUInt( position2 + xDim + 1 );
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return (long) ( ( 1 - dz ) * b1 + dz * b2 );
    }

    /**
     *   export data into values array WITHOUT LOCKING
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUDataNoLock( int start, int length, long[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getUInt( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data in values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUData( int start, int length, long[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getUInt( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportUSliceXY( int slice, long[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUSliceXZ( int slice, long[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getUInt( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportUSliceZY( int slice, long[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getUInt( slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set unsigned short methods ***********************/

    /**
     *   version of set that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final void setUInt( int position, long value ) {
        data.setUInt( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void setUInt( int x, int y, long value ) {
        data.setUInt( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void setUInt( int x, int y, int z, long value ) {
        data.setUInt( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void setUInt( int x, int y, int z, int b, long value ) {
        data.setUInt(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void setUInt( int[] position, long value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setUInt( location, value );
    }

    /**
     *   import int data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importUData( int start, long[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setUInt( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /*****************************************************************************/

    /********************************** Long ************************************/

    /*****************************************************************************/

    /**
     *   version of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final long getLong( int position ) {
        return ( data.getLong( position ) );
    }

    /**
     *   2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value at that position in the data array
     */
    public final long getLong( int x, int y ) {
        return ( data.getLong( y * dimExtents[0] + x ) );
    }

    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final long getLong( int x, int y, int z ) {
        return ( data.getLong( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final long getLong( int x, int y, int z, int b ) {
        return ( data.getLong(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x ) );
    }

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final long getLong( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getLong( location ) );
    }

    /**
     *    version of get that performs bi-linear interpoloation.
     *                     <b>Note - does NOT perform bounds checking</b>
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final long getLongBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( 1 - dx ) * data.getLong( position ) + dx * data.getLong( position + 1 );
        x2 = ( 1 - dx ) * data.getLong( position + xDim ) + dx * data.getLong( position + xDim + 1 );

        return (long) ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                     <b>Note - does NOT perform bounds checking</b>
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final long getLongTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        a1 = ( 1 - dx ) * data.getLong( position1 ) + dx * data.getLong( position1 + 1 );
        a2 = ( 1 - dx ) * data.getLong( position1 + xDim ) + dx * data.getLong( position1 + xDim + 1 );
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * data.getLong( position2 ) + dx * data.getLong( position2 + 1 );
        a2 = ( 1 - dx ) * data.getLong( position2 + xDim ) + dx * data.getLong( position2 + xDim + 1 );
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return (long) ( ( 1 - dz ) * b1 + dz * b2 );
    }

    /**
     *   export data into values array WITHOUT locking
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataNoLock( int start, int length, long[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getLong( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data into values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportData( int start, int length, long[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getLong( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportSliceXY( int slice, long[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXZ( int slice, long[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getLong( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZY( int slice, long[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getLong( slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set long methods *************************/

    /**
     *   version of set that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final void set( int position, long value ) {
        data.setLong( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void set( int x, int y, long value ) {
        data.setLong( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void set( int x, int y, int z, long value ) {
        data.setLong( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void set( int x, int y, int z, int b, long value ) {
        data.setLong(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void set( int[] position, long value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setLong( location, value );
    }

    /**
     *   import long data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importData( int start, long[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setLong( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /*****************************************************************************/

    /********************************** Float ************************************/

    /*****************************************************************************/

    /**
     *   gversion of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final float getFloat( int position ) {
        return ( data.getFloat( position ) );
    }

    /**
     *   2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value at that position in the data array
     */
    public final float getFloat( int x, int y ) {
        return ( data.getFloat( y * dimExtents[0] + x ) );
    }

    /**
     *   2D color get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param c          color index
     *   @return           value at that position in the data array
     */
    public final float getFloatC( int x, int y, int c ) {
        return ( data.getFloat(4 * ( y * dimExtents[0] + x ) + c) );
    }


    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final float getFloat( int x, int y, int z ) {
        return ( data.getFloat( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /**
     *   3D color get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param c
     *   @return           value at that position in the data array
     */
    public final float getFloatC( int x, int y, int z, int c ) {
        return ( data.getFloat( 4 * ( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) ) + c );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param t          t coordinate
     *   @return           value at that position in the data array
     */
    public final float getFloat( int x, int y, int z, int t ) {
        return ( data.getFloat(
                t * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x ) );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param t          t coordinate
     *   @param c          color value
     *   @return           value at that position in the data array
     */
    public final float getFloatC( int x, int y, int z, int t, int c ) {
        return ( data.getFloat(
                4
                        * ( t * ( dimExtents[0] * dimExtents[1] * dimExtents[2] )
                                + z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) )
                                        + c );
    }

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final float getFloat( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getFloat( location ) );
    }

    /**
     *   version of get that performs bi-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final float getFloatBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( 1 - dx ) * data.getFloat( position ) + dx * data.getFloat( position + 1 );
        x2 = ( 1 - dx ) * data.getFloat( position + xDim ) + dx * data.getFloat( position + xDim + 1 );

        return (float) ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final float getFloatTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        a1 = ( 1 - dx ) * data.getFloat( position1 ) + dx * data.getFloat( position1 + 1 );
        a2 = ( 1 - dx ) * data.getFloat( position1 + xDim ) + dx * data.getFloat( position1 + xDim + 1 );
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * data.getFloat( position2 ) + dx * data.getFloat( position2 + 1 );
        a2 = ( 1 - dx ) * data.getFloat( position2 + xDim ) + dx * data.getFloat( position2 + xDim + 1 );
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return (float) ( ( 1 - dz ) * b1 + dz * b2 );
    }

    /**
     * Get a value using tri-linear interpoloation.
     * Note - DOES perform bounds checking (both that x,y,z are valid and that the interp indicies don't cause exceptions).
     * @param  x         x coordinate
     * @param  y         y coordinate
     * @param  z         z coordinate
     * @return           interpolated value if in bounds, 0 if not.
     */
    public final float getFloatTriLinearBounds( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        if ( ( x >= 0 ) && ( y >= 0 ) && ( z >= 0 ) && ( x < xDim ) && ( y < yDim ) && ( z < dimExtents[2] ) && ( position1 >= 0 ) && ( position1 < dataSize - xDim * yDim - 1 ) && ( position2 >= 0 ) && ( position2 < dataSize - xDim * yDim - 1 ) ) {
            a1 = ( 1 - dx ) * data.getFloat( position1 ) + dx * data.getFloat( position1 + 1 );
            a2 = ( 1 - dx ) * data.getFloat( position1 + xDim ) + dx * data.getFloat( position1 + xDim + 1 );
            b1 = ( 1 - dy ) * a1 + dy * a2;

            a1 = ( 1 - dx ) * data.getFloat( position2 ) + dx * data.getFloat( position2 + 1 );
            a2 = ( 1 - dx ) * data.getFloat( position2 + xDim ) + dx * data.getFloat( position2 + xDim + 1 );
            b2 = ( 1 - dy ) * a1 + dy * a2;

            return (float) ( ( 1 - dz ) * b1 + dz * b2 );
        } else {
            return 0;
        }
    }

    /**
     *   export data in values array WITHOUT using locking.
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataNoLock( int start, int length, float[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getFloat( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data in values array WITHOUT using locking.
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBDataNoLock( int offset, int start, int length,
            float[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + 4 * length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i += 4, j++ ) {
                values[j] = data.getFloat( i + offset );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data to values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportData( int start, int length, float[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getFloat( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );

    }

    /**
     *   export data to values array
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportData( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( y = 0, j = 0; y < length1; y++ ) {
                for ( x = 0; x <= length0 - 1; x++, j++ ) {
                    i = start + x + y * length0;
                    values[j] = data.getFloat( i );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );

    }

    /**
     *   export data to values array reversing the second dimension
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataR1( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( y = length1 - 1, j = 0; y >= 0; y-- ) {
                for ( x = 0; x <= length0 - 1; x++, j++ ) {
                    i = start + x + y * length0;
                    values[j] = data.getFloat( i );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );

    }

    /**
     *   export data to values array reversing the first dimension
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataR0( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( y = 0, j = 0; y <= length1 - 1; y++ ) {
                for ( x = length0 - 1; x >= 0; x--, j++ ) {
                    i = start + x + y * length0;
                    values[j] = data.getFloat( i );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );

    }

    /**
     *   export RGB data to values array reversing the first dimension
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBDataR0( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( y = 0, j = 0; y <= length1 - 1; y++ ) {
                for ( x = length0 - 1; x >= 0; x -= 4, j += 4 ) {
                    i = start + x + y * length0;
                    values[j + 3] = data.getFloat( i );
                    values[j + 2] = data.getFloat( i - 1 );
                    values[j + 1] = data.getFloat( i - 2 );
                    values[j] = data.getFloat( i - 3 );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );

    }

    /**
     *   export data to values array reversing the first and second dimensions
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataR0R1( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( y = length1 - 1, j = 0; y >= 0; y-- ) {
                for ( x = length0 - 1; x >= 0; x--, j++ ) {
                    i = start + x + y * length0;
                    values[j] = data.getFloat( i );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );

    }

    /**
     *   export RGB data to values array reversing the first and second dimensions
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBDataR0R1( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( y = length1 - 1, j = 0; y >= 0; y-- ) {
                for ( x = length0 - 1; x >= 0; x -= 4, j += 4 ) {
                    i = start + x + y * length0;
                    values[j + 3] = data.getFloat( i );
                    values[j + 2] = data.getFloat( i - 1 );
                    values[j + 1] = data.getFloat( i - 2 );
                    values[j] = data.getFloat( i - 3 );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );

    }

    /**
     *   export data to values array interchanging first and second dimensions
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataXD( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( x = 0, j = 0; x <= length0 - 1; x++ ) {
                for ( y = 0; y <= length1 - 1; y++, j++ ) {
                    i = start + y * length0 + x;
                    values[j] = data.getFloat( i );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data to values array interchanging first and second dimensions
     *   read in original second dimension in reverse order
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataXDR1( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( x = 0, j = 0; x <= length0 - 1; x++ ) {
                for ( y = length1 - 1; y >= 0; y--, j++ ) {
                    i = start + y * length0 + x;
                    values[j] = data.getFloat( i );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data to values array interchanging first and second dimensions
     *   read in original first dimension in reverse order
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataXDR0( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( x = length0 - 1, j = 0; x >= 0; x-- ) {
                for ( y = 0; y <= length1 - 1; y++, j++ ) {
                    i = start + y * length0 + x;
                    values[j] = data.getFloat( i );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data to values array interchanging first and second dimensions
     *   read in both dimensions in the reverse direction
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of data to be copied from data array
     *   @param length1    length of second dimension of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataXDR0R1( int start, int length0, int length1,
            float[] values )
        throws IOException {
        int i, j;
        int x, y;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( x = length0 - 1, j = 0; x >= 0; x-- ) {
                for ( y = length1 - 1; y >= 0; y--, j++ ) {
                    i = start + y * length0 + x;
                    values[j] = data.getFloat( i );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data in values array
     *   @param offset     correct offset for RED, GREEN, or BLUE component to be exported
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBData( int offset, int start, int length,
            byte[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + 4 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start + offset, j = 0; j < length; i += 4, j++ ) {
                values[j] = data.getByte( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data in values array
     *   @param offset     correct offset for RED, GREEN, or BLUE component to be exported
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBData( int offset, int start, int length,
            short[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + 4 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start + offset, j = 0; j < length; i += 4, j++ ) {
                values[j] = data.getShort( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data in values array
     *   @param offset     correct offset for RED, GREEN, or BLUE component to be exported
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBData( int offset, int start, int length,
            float[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + 4 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start + offset, j = 0; j < length; i += 4, j++ ) {
                values[j] = data.getFloat( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data to valuesr and valuesi arrays
     *   @param start      indicates starting position in data array
     *   @param length     length of complex data (in 2 float units) to be copied from data array
     *   @param valuesR    array where real data is to be deposited
     *   @param valuesI    array where imaginary data is to be deposited
     */
    public final synchronized void exportComplexData( int start, int length, float[] valuesR, float[] valuesI )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= valuesR.length )
                && ( length <= valuesI.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i += 2, j++ ) {
                valuesR[j] = data.getFloat( i );
                valuesI[j] = data.getFloat( i + 1 );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );

    }

    /**
     *   export data to valuesr and valuesi arrays
     *   @param start      indicates starting position in data array
     *   @param length     length of complex data (in 2 float units) to be copied from data array
     *   @param valuesR    array where real data is to be deposited
     *   @param valuesI    array where imaginary data is to be deposited
     */
    public final synchronized void exportDComplexData( int start, int length, double[] valuesR, double[] valuesI )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= valuesR.length )
                && ( length <= valuesI.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i += 2, j++ ) {
                valuesR[j] = data.getDouble( i );
                valuesI[j] = data.getDouble( i + 1 );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );

    }

    /**
     *   export magnitude data to values array
     *   @param start      indicates starting position in data array
     *   @param length     length of magnitude data to be copied from data array
     *   @param values    array where magnitude data is to be deposited
     */
    public final synchronized void exportMagData( int start, int length, float[] values )
        throws IOException {
        int i, j;
        double real, imaginary;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i += 2, j++ ) {
                real = (double) data.getFloat( i );
                imaginary = (double) data.getFloat( i + 1 );
                values[j] = (float) Math.sqrt( real * real + imaginary * imaginary );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export magnitude data to values array reversing second dimension
     *   @param start      indicates starting position in data array
     *   @param length0     length of first dimension of magnitude data to be copied from data array
     *   @param length1     length of second dimension of magnitude data to be copied from data array
     *   @param values    array where magnitude data is to be deposited
     *   @param logMagDisplay if true take log10 of 1 + value
     */
    public final synchronized void exportMagDataR1( int start, int length0, int length1, float[] values,
            boolean logMagDisplay )
        throws IOException {
        int i, j;
        int x, y;
        double real, imaginary;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( y = length1 - 1, j = 0; y >= 0; y-- ) {
                for ( x = 0; x <= length0 - 1; x++, j++ ) {
                    i = start + 2 * x + 2 * y * length0;
                    real = (double) data.getFloat( i );
                    imaginary = (double) data.getFloat( i + 1 );
                    values[j] = (float) Math.sqrt( real * real + imaginary * imaginary );
                }
            }
            releaseLock();
            if ( logMagDisplay ) {
                for ( i = 0; i < length; i++ ) {
                    // log10(x) = loge(x)/loge(10)
                    values[i] = (float) ( 0.4342944819 * java.lang.Math.log( (double) ( 1.0 + values[i] ) ) );
                }
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export magnitude data to values array reversing first dimension
     *   @param start      indicates starting position in data array
     *   @param length0    length of first dimension of magnitude data to be copied from data array
     *   @param length1    lenght of second dimension of magnitude data to be copied from data array
     *   @param values    array where magnitude data is to be deposited
     *   @param logMagDisplay if true take log10 of 1 + value
     */
    public final synchronized void exportMagDataR0( int start, int length0, int length1, float[] values,
            boolean logMagDisplay )
        throws IOException {
        int i, j;
        int x, y;
        double real, imaginary;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( y = 0, j = 0; y <= length1 - 1; y++ ) {
                for ( x = length0 - 1; x >= 0; x--, j++ ) {
                    i = start + 2 * x + 2 * y * length0;
                    real = (double) data.getFloat( i );
                    imaginary = (double) data.getFloat( i + 1 );
                    values[j] = (float) Math.sqrt( real * real + imaginary * imaginary );
                }
            }
            releaseLock();
            if ( logMagDisplay ) {
                for ( i = 0; i < length; i++ ) {
                    // log10(x) = loge(x)/loge(10)
                    values[i] = (float) ( 0.4342944819 * java.lang.Math.log( (double) ( 1.0 + values[i] ) ) );
                }
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export magnitude data to values array reversing first and second dimensions
     *   @param start     indicates starting position in data array
     *   @param length0   length of first dimension of magnitude data to be copied from data array
     *   @param length1   length of second dimension of magnitude data to be copied from data array
     *   @param values    array where magnitude data is to be deposited
     *   @param logMagDisplay if true take the log10 of 1 + value
     */
    public final synchronized void exportMagDataR0R1( int start, int length0, int length1, float[] values,
            boolean logMagDisplay )
        throws IOException {
        int i, j;
        int x, y;
        double real, imaginary;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( y = length1 - 1, j = 0; y >= 0; y-- ) {
                for ( x = length0 - 1; x >= 0; x--, j++ ) {
                    i = start + 2 * x + 2 * y * length0;
                    real = (double) data.getFloat( i );
                    imaginary = (double) data.getFloat( i + 1 );
                    values[j] = (float) Math.sqrt( real * real + imaginary * imaginary );
                }
            }
            releaseLock();
            if ( logMagDisplay ) {
                for ( i = 0; i < length; i++ ) {
                    // log10(x) = loge(x)/loge(10)
                    values[i] = (float) ( 0.4342944819 * java.lang.Math.log( (double) ( 1.0 + values[i] ) ) );
                }
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export magnitude data to values array interchanging dimensions
     *   @param start      indicates starting position in data array
     *   @param length0     length of first dimension of magnitude data to be copied from data array
     *   @param length1     length of second dimension of magnitude data to be copied from data array
     *   @param values    array where magnitude data is to be deposited
     *   @param logMagDisplay if true take log10 of 1 + value
     */
    public final synchronized void exportMagDataXD( int start, int length0, int length1, float[] values,
            boolean logMagDisplay )
        throws IOException {
        int i, j;
        int x, y;
        double real, imaginary;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( x = 0, j = 0; x <= length0 - 1; x++ ) {
                for ( y = 0; y <= length1 - 1; y++, j++ ) {
                    i = start + 2 * x + 2 * y * length0;
                    real = (double) data.getFloat( i );
                    imaginary = (double) data.getFloat( i + 1 );
                    values[j] = (float) Math.sqrt( real * real + imaginary * imaginary );
                }
            }
            releaseLock();
            if ( logMagDisplay ) {
                for ( i = 0; i < length; i++ ) {
                    // log10(x) = loge(x)/loge(10)
                    values[i] = (float) ( 0.4342944819 * java.lang.Math.log( (double) ( 1.0 + values[i] ) ) );
                }
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export magnitude data to values array interchanging dimensions
     *   read in original second dimension in reverse direction
     *   @param start      indicates starting position in data array
     *   @param length0     length of first dimension of magnitude data to be copied from data array
     *   @param length1     length of second dimension of magnitude data to be copied from data array
     *   @param values    array where magnitude data is to be deposited
     *   @param logMagDisplay if true take log10 of 1 + value
     */
    public final synchronized void exportMagDataXDR1( int start, int length0, int length1, float[] values,
            boolean logMagDisplay )
        throws IOException {
        int i, j;
        int x, y;
        double real, imaginary;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( x = 0, j = 0; x <= length0 - 1; x++ ) {
                for ( y = length1 - 1; y >= 0; y--, j++ ) {
                    i = start + 2 * x + 2 * y * length0;
                    real = (double) data.getFloat( i );
                    imaginary = (double) data.getFloat( i + 1 );
                    values[j] = (float) Math.sqrt( real * real + imaginary * imaginary );
                }
            }
            releaseLock();
            if ( logMagDisplay ) {
                for ( i = 0; i < length; i++ ) {
                    // log10(x) = loge(x)/loge(10)
                    values[i] = (float) ( 0.4342944819 * java.lang.Math.log( (double) ( 1.0 + values[i] ) ) );
                }
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export magnitude data to values array interchanging dimensions
     *   read in original first dimension in reverse direction
     *   @param start      indicates starting position in data array
     *   @param length0     length of first dimension of magnitude data to be copied from data array
     *   @param length1     length of second dimension of magnitude data to be copied from data array
     *   @param values    array where magnitude data is to be deposited
     *   @param logMagDisplay if true take log10 of 1 + value
     */
    public final synchronized void exportMagDataXDR0( int start, int length0, int length1, float[] values,
            boolean logMagDisplay )
        throws IOException {
        int i, j;
        int x, y;
        double real, imaginary;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( x = length0 - 1, j = 0; x >= 0; x-- ) {
                for ( y = 0; y <= length1 - 1; y++, j++ ) {
                    i = start + 2 * x + 2 * y * length0;
                    real = (double) data.getFloat( i );
                    imaginary = (double) data.getFloat( i + 1 );
                    values[j] = (float) Math.sqrt( real * real + imaginary * imaginary );
                }
            }
            releaseLock();
            if ( logMagDisplay ) {
                for ( i = 0; i < length; i++ ) {
                    // log10(x) = loge(x)/loge(10)
                    values[i] = (float) ( 0.4342944819 * java.lang.Math.log( (double) ( 1.0 + values[i] ) ) );
                }
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export magnitude data to values array interchanging dimensions
     *   read in both dimensions in the reverse direction
     *   @param start      indicates starting position in data array
     *   @param length0     length of first dimension of magnitude data to be copied from data array
     *   @param length1     length of second dimension of magnitude data to be copied from data array
     *   @param values    array where magnitude data is to be deposited
     *   @param logMagDisplay if true take log10 of 1 + value
     */
    public final synchronized void exportMagDataXDR0R1( int start, int length0, int length1, float[] values,
            boolean logMagDisplay )
        throws IOException {
        int i, j;
        int x, y;
        double real, imaginary;
        int length = length0 * length1;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( x = length0 - 1, j = 0; x >= 0; x-- ) {
                for ( y = length1 - 1; y >= 0; y--, j++ ) {
                    i = start + 2 * x + 2 * y * length0;
                    real = (double) data.getFloat( i );
                    imaginary = (double) data.getFloat( i + 1 );
                    values[j] = (float) Math.sqrt( real * real + imaginary * imaginary );
                }
            }
            releaseLock();
            if ( logMagDisplay ) {
                for ( i = 0; i < length; i++ ) {
                    // log10(x) = loge(x)/loge(10)
                    values[i] = (float) ( 0.4342944819 * java.lang.Math.log( (double) ( 1.0 + values[i] ) ) );
                }
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export magnitude data to values array
     *   @param start      indicates starting position in data array
     *   @param length     length of magnitude data to be copied from data array
     *   @param values    array where magnitude data is to be deposited
     */
    public final synchronized void exportDMagData( int start, int length, double[] values )
        throws IOException {
        int i, j;
        double real, imaginary;

        if ( ( start >= 0 ) && ( start + 2 * length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i += 2, j++ ) {
                real = data.getDouble( i );
                imaginary = data.getDouble( i + 1 );
                values[j] = Math.sqrt( real * real + imaginary * imaginary );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportSliceXY( int slice, float[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XY slice magnitude data into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     *   @param logMagDisplay if true display log10 of 1 + data
     */
    public final void exportComplexSliceXY( int slice, float[] values, boolean logMagDisplay )
        throws IOException {

        int i;
        int length = dimExtents[0] * dimExtents[1];

        exportMagData( 2 * slice * length, length, values );

        if ( logMagDisplay ) {
            for ( i = 0; i < length; i++ ) {
                // log10(x) = loge(x)/loge(10)
                values[i] = (float) ( 0.4342944819 * java.lang.Math.log( (double) ( 1.0 + values[i] ) ) );
            }
        }
    }

    /**
     *   export XZ slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     *   @param logMagDisplay if true display log10 of 1 + value
     */
    public final synchronized void exportComplexSliceXZ( int tSlice, int slice, float[] values,
            boolean logMagDisplay )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;
        double real, imaginary, mag;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {
            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 2;

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 2 + slice * xDim * 2;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim * 2; k += 2 ) {
                    real = data.getFloat( k );
                    imaginary = data.getFloat( k + 1 );
                    if ( logMagDisplay == true ) {
                        mag = Math.sqrt( real * real + imaginary * imaginary );
                        values[i] = (float) ( 0.4342944819 * Math.log( ( 1.0 + mag ) ) );
                    } else {
                        values[i] = (float) Math.sqrt( real * real + imaginary * imaginary );
                    }
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportComplexSliceZY( int tSlice, int slice, float[] values,
            boolean logMagDisplay )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;
        double real, imaginary, mag;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 2;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 2;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            slice = slice * 2;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    real = data.getFloat( start + slice + k * stride );
                    imaginary = data.getFloat( start + slice + k * stride + 1 );
                    if ( logMagDisplay == true ) {
                        mag = Math.sqrt( real * real + imaginary * imaginary );
                        values[i] = (float) ( 0.4342944819 * Math.log( ( 1.0 + mag ) ) );
                    } else {
                        values[i] = (float) Math.sqrt( real * real + imaginary * imaginary );
                    }
                    i++;
                }
                slice += xDim * 2;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XZ slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXZ( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getFloat( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XZ slice into values array with Z dimension read in reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXZR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice * xDim + stride * ( zDim - 1 );

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getFloat( k );
                    i++;
                }
                start -= stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XZ slice into values array with x dimension read in reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXRZ( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice * xDim + xDim - 1;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k > start - xDim; k-- ) {
                    values[i] = data.getFloat( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XZ slice into values array with both dimensions read in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXRZR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice * xDim + stride * ( zDim - 1 ) + xDim
                    - 1;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k > start - xDim; k-- ) {
                    values[i] = data.getFloat( k );
                    i++;
                }
                start -= stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZX slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZX( int tSlice, int slice, float[] values )
        throws IOException {
        int i;
        int x, z;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice * xDim;

            for ( x = 0; x < xDim; x++ ) {
                for ( z = 0; z < zDim; z++, i++ ) {
                    values[i] = data.getFloat( start + z * stride + x );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZX slice into values array with X read in reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZXR( int tSlice, int slice, float[] values )
        throws IOException {
        int i;
        int x, z;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice * xDim;

            for ( x = xDim - 1; x >= 0; x-- ) {
                for ( z = 0; z < zDim; z++, i++ ) {
                    values[i] = data.getFloat( start + z * stride + x );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZX slice into values array with Z dimension read in reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZRX( int tSlice, int slice, float[] values )
        throws IOException {
        int i;
        int x, z;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice * xDim;

            for ( x = 0; x < xDim; x++ ) {
                for ( z = zDim - 1; z >= 0; z--, i++ ) {
                    values[i] = data.getFloat( start + z * stride + x );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZX slice into values array wit both dimensions read in reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZRXR( int tSlice, int slice, float[] values )
        throws IOException {
        int i;
        int x, z;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice * xDim;

            for ( x = xDim - 1; x >= 0; x-- ) {
                for ( z = zDim - 1; z >= 0; z--, i++ ) {
                    values[i] = data.getFloat( start + z * stride + x );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XZ slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceXZ( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * xDim * 4;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim * 4; k += 4 ) {
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XZ slice into values array reading the z dimension in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceXZR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * xDim * 4
                    + ( zDim - 1 ) * stride;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim * 4; k += 4 ) {
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
                start -= stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XZ slice into values array reading the x dimension in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceXRZ( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * xDim * 4;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start + ( xDim - 1 ) * 4; k >= start; k -= 4 ) {
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XZ slice into values array reading both dimensions in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceXRZR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * xDim * 4
                    + ( zDim - 1 ) * stride;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start + ( xDim - 1 ) * 4; k >= start; k -= 4 ) {
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
                start -= stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZX slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceZX( int tSlice, int slice, float[] values )
        throws IOException {
        int i, x, z, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * xDim * 4;

            for ( x = 0; x < xDim; x++ ) {
                for ( z = 0; z < zDim; z++ ) {
                    k = start + z * stride + 4 * x;
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
            }

            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZX slice into values array reading the x dimension in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceZXR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, x, z, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * xDim * 4;

            for ( x = xDim - 1; x >= 0; x-- ) {
                for ( z = 0; z < zDim; z++ ) {
                    k = start + z * stride + 4 * x;
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
            }

            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZX slice into values array reading the z dimension in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceZRX( int tSlice, int slice, float[] values )
        throws IOException {
        int i, x, z, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * xDim * 4;

            for ( x = 0; x < xDim; x++ ) {
                for ( z = zDim - 1; z >= 0; z-- ) {
                    k = start + z * stride + 4 * x;
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
            }

            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZX slice into values array reading both dimensions in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceZRXR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, x, z, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;

            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * xDim * 4;

            for ( x = xDim - 1; x >= 0; x-- ) {
                for ( z = zDim - 1; z >= 0; z-- ) {
                    k = start + z * stride + 4 * x;
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
            }

            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZY( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2];

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getFloat( start + slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array with y dimension reversed
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZYR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];

            stride = dimExtents[0] * dimExtents[1];
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + ( yDim - 1 ) * xDim;

            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getFloat( start + slice + k * stride );
                    i++;
                }
                slice -= xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array with z dimension reversed
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZRY( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2];

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = zDim - 1; k >= 0; k-- ) {
                    values[i] = data.getFloat( start + slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array with both dimensions read in reverse
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZRYR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];

            stride = dimExtents[0] * dimExtents[1];
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + ( yDim - 1 ) * xDim;

            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = zDim - 1; k >= 0; k-- ) {
                    values[i] = data.getFloat( start + slice + k * stride );
                    i++;
                }
                slice -= xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export YZ slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceYZ( int tSlice, int slice, float[] values )
        throws IOException {
        int i;
        int y, z;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;

            for ( z = 0; z < zDim; z++ ) {
                for ( y = 0; y < yDim; y++, i++ ) {
                    values[i] = data.getFloat( start + y * xDim + z * stride );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export YZ slice into values array reading the z dimension in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceYZR( int tSlice, int slice, float[] values )
        throws IOException {
        int i;
        int y, z;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;

            for ( z = zDim - 1; z >= 0; z-- ) {
                for ( y = 0; y < yDim; y++, i++ ) {
                    values[i] = data.getFloat( start + y * xDim + z * stride );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export YZ slice into values array reading the y dimension in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceYRZ( int tSlice, int slice, float[] values )
        throws IOException {
        int i;
        int y, z;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;

            for ( z = 0; z < zDim; z++ ) {
                for ( y = yDim - 1; y >= 0; y--, i++ ) {
                    values[i] = data.getFloat( start + y * xDim + z * stride );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export YZ slice into values array reading both dimensions in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceYRZR( int tSlice, int slice, float[] values )
        throws IOException {
        int i;
        int y, z;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] + slice;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;

            for ( z = zDim - 1; z >= 0; z-- ) {
                for ( y = yDim - 1; y >= 0; y--, i++ ) {
                    values[i] = data.getFloat( start + y * xDim + z * stride );
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceZY( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            slice = slice * 4;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getFloat( start + slice + k * stride );
                    values[i + 1] = data.getFloat( start + slice + k * stride + 1 );
                    values[i + 2] = data.getFloat( start + slice + k * stride + 2 );
                    values[i + 3] = data.getFloat( start + slice + k * stride + 3 );
                    i += 4;
                }
                slice += xDim * 4;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array reading the y dimension in the reverese direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceZYR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];

            stride = dimExtents[0] * dimExtents[1] * 4;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + 4 * slice + ( yDim - 1 ) * xDim * 4;

            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getFloat( start + k * stride );
                    values[i + 1] = data.getFloat( start + k * stride + 1 );
                    values[i + 2] = data.getFloat( start + k * stride + 2 );
                    values[i + 3] = data.getFloat( start + k * stride + 3 );
                    i += 4;
                }
                start -= xDim * 4;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array reading the z dimension in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceZRY( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1] * 4;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            slice = slice * 4;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = zDim - 1; k >= 0; k-- ) {
                    values[i] = data.getFloat( start + slice + k * stride );
                    values[i + 1] = data.getFloat( start + slice + k * stride + 1 );
                    values[i + 2] = data.getFloat( start + slice + k * stride + 2 );
                    values[i + 3] = data.getFloat( start + slice + k * stride + 3 );
                    i += 4;
                }
                slice += xDim * 4;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array reading both dimensions in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceZRYR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];

            stride = dimExtents[0] * dimExtents[1] * 4;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + 4 * slice + ( yDim - 1 ) * xDim * 4;

            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = zDim - 1; k >= 0; k-- ) {
                    values[i] = data.getFloat( start + k * stride );
                    values[i + 1] = data.getFloat( start + k * stride + 1 );
                    values[i + 2] = data.getFloat( start + k * stride + 2 );
                    values[i + 3] = data.getFloat( start + k * stride + 3 );
                    i += 4;
                }
                start -= xDim * 4;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export YZ slice into values array
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceYZ( int tSlice, int slice, float[] values )
        throws IOException {
        int i, y, z, k;
        int xDim, yDim, zDim;
        int stride;
        int yStride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            yStride = dimExtents[0] * 4;
            stride = dimExtents[0] * dimExtents[1] * 4;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * 4;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;

            for ( z = 0; z < zDim; z++ ) {
                for ( y = 0; y < yDim; y++ ) {
                    k = start + y * yStride + z * stride;
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export YZ slice into values array reading the z dimension in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceYZR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, y, z, k;
        int xDim, yDim, zDim;
        int stride;
        int yStride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            yStride = dimExtents[0] * 4;
            stride = dimExtents[0] * dimExtents[1] * 4;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * 4;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;

            for ( z = zDim - 1; z >= 0; z-- ) {
                for ( y = 0; y < yDim; y++ ) {
                    k = start + y * yStride + z * stride;
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export YZ slice into values array reading the y dimension in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceYRZ( int tSlice, int slice, float[] values )
        throws IOException {
        int i, y, z, k;
        int xDim, yDim, zDim;
        int stride;
        int yStride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            yStride = dimExtents[0] * 4;
            stride = dimExtents[0] * dimExtents[1] * 4;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * 4;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;

            for ( z = 0; z < zDim; z++ ) {
                for ( y = yDim - 1; y >= 0; y-- ) {
                    k = start + y * yStride + z * stride;
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export YZ slice into values array reeading both directions in the reverse direction
     *   @param tSlice     indicates time slice of data to be exported
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportRGBSliceYRZR( int tSlice, int slice, float[] values )
        throws IOException {
        int i, y, z, k;
        int xDim, yDim, zDim;
        int stride;
        int yStride;
        int start;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            yStride = dimExtents[0] * 4;
            stride = dimExtents[0] * dimExtents[1] * 4;
            start = tSlice * dimExtents[0] * dimExtents[1] * dimExtents[2] * 4 + slice * 4;

            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;

            for ( z = zDim - 1; z >= 0; z-- ) {
                for ( y = yDim - 1; y >= 0; y-- ) {
                    k = start + y * yStride + z * stride;
                    values[i] = data.getFloat( k );
                    values[i + 1] = data.getFloat( k + 1 );
                    values[i + 2] = data.getFloat( k + 2 );
                    values[i + 3] = data.getFloat( k + 3 );
                    i += 4;
                }
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set float methods *************************/

    /**
     *   color version of setC that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     *   @param c
     *   @param value
     */
    public final void setC( int position, int c, float value ) {
        data.setFloat( 4 * position + c, value );
    }

    /**
     *   version of set that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final void set( int position, float value ) {
        data.setFloat( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void set( int x, int y, float value ) {
        data.setFloat( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void set( int x, int y, int z, float value ) {
        data.setFloat( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   2D fast color version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param c
     *   @param value
     */
    public final void setC( int x, int y, int c, float value ) {
        data.setFloat( 4 * ( y * dimExtents[0] + x ) + c, value );
    }

    /**
     *   3D fast color version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param c
     *   @param value
     */
    public final void setC( int x, int y, int z, int c, float value ) {
        data.setFloat( 4 * ( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) + c, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void set( int x, int y, int z, int b, float value ) {
        data.setFloat(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void set( int[] position, float value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setFloat( location, value );
    }

    /**
     *   sets the type of data
     *   @param type the type of data
     */
    public final void setType( int type ) {
        bufferType = type;
    }

    /**
     *   import float data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importData( int start, float[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {
            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setFloat( ptr, values[i] );
            }

            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /**
     *   import float data into data array
     *   @param color      color planes are interleaved, so color offsets the appropriate interleave interval
     *   @param alphaIndexStart      indicates starting position in data array which points to the alpha value
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importRGBData( int color, int alphaIndexStart, float[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( bufferType != ARGB && bufferType != ARGB_USHORT && bufferType != ARGB_FLOAT ) { // not a color image
            return; // so don't do anything
        }

        if ( ( alphaIndexStart >= 0 ) && ( alphaIndexStart + length <= dataSize ) ) {
            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = alphaIndexStart + color;

            for ( int i = 0; i < length; i++, ptr += 4 ) {
                data.setFloat( ptr, values[i] );
            }

            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /**
     *   import Complex data (in 2 float units) into data array
     *   @param start      indicates starting position in data array
     *   @param valuesR    array where real data is to be acquired
     *   @param valuesI    array where imaginary data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max magnitude values for the image
     *   @param logMagDisplay whether or not min and max are calculated for log10 of 1 + magnitude
     *                     array
     */
    public final synchronized void importComplexData( int start, float[] valuesR, float[] valuesI,
            boolean mmFlag, boolean logMagDisplay )
        throws IOException {
        int length = valuesR.length;
        int lengthi = valuesI.length;
        int ptr;

        if ( ( length == lengthi ) && ( start >= 0 ) && ( start + 2 * length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr += 2 ) {
                data.setFloat( ptr, valuesR[i] );
                data.setFloat( ptr + 1, valuesI[i] );
            }
            setLogMagDisplay( logMagDisplay );
            if ( mmFlag ) {
                calcMinMaxMag( logMagDisplay );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /**
     *   import Complex data (in 2 double units) into data array
     *   @param start      indicates starting position in data array
     *   @param valuesR    array where real data is to be acquired
     *   @param valuesI    array where imaginary data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max magnitude values for the image
     *   @param logMagDisplay whether or not min and max are calculated for log10 of 1 + magnitude
     *                     array
     */
    public final synchronized void importDComplexData( int start, double[] valuesR, double[] valuesI,
            boolean mmFlag, boolean logMagDisplay )
        throws IOException {
        int length = valuesR.length;
        int lengthi = valuesI.length;
        int ptr;

        if ( ( length == lengthi ) && ( start >= 0 ) && ( start + 2 * length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr += 2 ) {
                data.setDouble( ptr, valuesR[i] );
                data.setDouble( ptr + 1, valuesI[i] );
            }
            setLogMagDisplay( logMagDisplay );
            if ( mmFlag ) {
                calcMinMaxMag( logMagDisplay );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /******************************************************************************/

    /********************************** Double ************************************/

    /******************************************************************************/

    /**
     *   version of get that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final double getDouble( int position ) {
        return ( data.getDouble( position ) );
    }

    /**
     *   2D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @return           value at that position in the data array
     */
    public final double getDouble( int x, int y ) {
        return ( data.getDouble( y * dimExtents[0] + x ) );
    }

    /**
     *   3D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value at that position in the data array
     */
    public final double getDouble( int x, int y, int z ) {
        return ( data.getDouble( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x ) );
    }

    /**
     *   4D get data fuction where bounds checking is NOT performed
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @return           value
     */
    public final double getDouble( int x, int y, int z, int b ) {
        return ( data.getDouble(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x ) );
    }

    /**
     *   nD get data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @return           returns true if position in data array range
     */
    public final double getDouble( int[] position ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        return ( data.getDouble( location ) );
    }

    /**
     *   version of get that performs bi-linear interpoloation.
     *                     Note - does NOT perform bounds checking
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     */
    public final double getDoubleBiLinear( float x, float y ) {

        int xDim = dimExtents[0];
        int position;
        int intX, intY;
        double dx, dy;
        double x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = intY * xDim + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( 1 - dx ) * data.getDouble( position ) + dx * data.getDouble( position + 1 );
        x2 = ( 1 - dx ) * data.getDouble( position + xDim ) + dx * data.getDouble( position + xDim + 1 );

        return ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   version of get that performs tri-linear interpoloation.
     *                     <b>Note - does NOT perform bounds checking</b>
     *   @param  x         x coordinate
     *   @param  y         y coordinate
     *   @param  z         z coordinate
     */
    public final double getDoubleTriLinear( float x, float y, float z ) {

        int xDim = dimExtents[0];
        int yDim = dimExtents[1];
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        double dx, dy, dz;
        double a1, a2;
        double b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = intZ * imageSize + intY * xDim + intX;
        position2 = position1 + imageSize;

        a1 = ( 1 - dx ) * data.getDouble( position1 ) + dx * data.getDouble( position1 + 1 );
        a2 = ( 1 - dx ) * data.getDouble( position1 + xDim ) + dx * data.getDouble( position1 + xDim + 1 );
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * data.getDouble( position2 ) + dx * data.getDouble( position2 + 1 );
        a2 = ( 1 - dx ) * data.getDouble( position2 + xDim ) + dx * data.getDouble( position2 + xDim + 1 );
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return ( ( 1 - dz ) * b1 + dz * b2 );
    }

    /**
     *   export data into values array WITHOUT locking
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportDataNoLock( int start, int length, double[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getDouble( i );
            }
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export data in values array
     *   @param start      indicates starting position in data array
     *   @param length     length of data to be copied from data array
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportData( int start, int length, double[] values )
        throws IOException {
        int i, j;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) && ( length <= values.length ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            for ( i = start, j = 0; j < length; i++, j++ ) {
                values[j] = data.getDouble( i );
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export XY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final void exportSliceXY( int slice, double[] values )
        throws IOException {
        int length = dimExtents[0] * dimExtents[1];

        exportData( slice * length, length, values );
    }

    /**
     *   export XZ slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceXZ( int slice, double[] values )
        throws IOException {
        int i, j, k;
        int start;
        int xDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[1] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            zDim = dimExtents[2];
            i = 0;
            start = slice * xDim;

            for ( j = 0; j < zDim; j++ ) {
                for ( k = start; k < start + xDim; k++ ) {
                    values[i] = data.getDouble( k );
                    i++;
                }
                start += stride;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /**
     *   export ZY slice into values array
     *   @param slice      indicates slice of data to be exported
     *   @param values     array where data is to be deposited
     */
    public final synchronized void exportSliceZY( int slice, double[] values )
        throws IOException {
        int i, j, k;
        int xDim, yDim, zDim;
        int stride;

        if ( ( slice >= 0 ) && ( slice < dimExtents[0] ) ) {

            try {
                setLock( W_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            stride = dimExtents[0] * dimExtents[1];
            xDim = dimExtents[0];
            yDim = dimExtents[1];
            zDim = dimExtents[2];
            i = 0;
            for ( j = 0; j < yDim; j++ ) {
                for ( k = 0; k < zDim; k++ ) {
                    values[i] = data.getDouble( slice + k * stride );
                    i++;
                }
                slice += xDim;
            }
            releaseLock();
            return;
        }
        throw new IOException( "Export data error - bounds incorrect" );
    }

    /************************** Set double methods *************************/

    /**
     *   version of set that does NOT perform bounds checking
     *   @param position   position in one dimensional array
     */
    public final void set( int position, double value ) {
        data.setDouble( position, value );
    }

    /**
     *   2D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     */
    public final void set( int x, int y, double value ) {
        data.setDouble( y * dimExtents[0] + x, value );
    }

    /**
     *   3D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     */
    public final void set( int x, int y, int z, double value ) {
        data.setDouble( z * ( dimExtents[0] * dimExtents[1] ) + y * dimExtents[0] + x, value );
    }

    /**
     *   4D fast version of set that does NOT perform bounds checking
     *   @param x          x coordinate
     *   @param y          y coordinate
     *   @param z          z coordinate
     *   @param b          b coordinate
     */
    public final void set( int x, int y, int z, int b, double value ) {
        data.setDouble(
                b * ( dimExtents[0] * dimExtents[1] * dimExtents[2] ) + z * ( dimExtents[0] * dimExtents[1] )
                + y * dimExtents[0] + x,
                value );
    }

    /**
     *   nD set data fuction where bounds checking is NOT performed
     *   @param position   array of coordinate values
     *   @param value      data that will be stored in the data array
     */
    public final void set( int[] position, double value ) {
        int i;
        int location = 0;
        int dimensions = position.length;

        for ( i = dimensions - 1; i > 0; i-- ) {
            location += ( position[i] * dimExtents[i] );
        }
        location += position[0];
        data.setDouble( location, value );
    }

    /**
     *   import double data into data array
     *   @param start      indicates starting position in data array
     *   @param values     array where data is to be acquired
     *   @param mmFlag     whether or not to calculate min and max values for the image array
     */
    public final synchronized void importData( int start, double[] values, boolean mmFlag )
        throws IOException {
        int length = values.length;
        int ptr;

        if ( ( start >= 0 ) && ( start + length <= dataSize ) ) {

            try {
                setLock( RW_LOCKED );
            } catch ( IOException error ) {
                throw error;
            }

            ptr = start;
            for ( int i = 0; i < length; i++, ptr++ ) {
                data.setDouble( ptr, values[i] );
            }
            if ( mmFlag ) {
                calcMinMax();
            }
            releaseLock();
            return;
        }
        throw new IOException( "Import data error: bounds incorrect" );
    }

    /**
     *   Returns the string for a particular buffer type.
     *   @param    type int representing the buffer type (see the static definitions)
     *   @return   string representing the buffer type
     */
    public static String getBufferTypeStr( int type ) {

        try {
            return ModelStorageBase.bufferTypeStr[type];
        } catch ( ArrayIndexOutOfBoundsException ae ) {}

        return "";

    } // end getBufferTypeStr()
}
