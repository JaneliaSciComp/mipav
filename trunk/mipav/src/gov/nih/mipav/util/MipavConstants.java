package gov.nih.mipav.util;

public class MipavConstants {
    public static final Unit UNKNOW_UNIT = new Unit("unk", "unknown");
    /**
     * Space units
     */
    public static final Unit MILES = new Unit("mi", "miles");
    public static final Unit KILOMETERS = new Unit("km", "kilometers");
    public static final Unit INCHES = new Unit("in", "inches");
    public static final Unit METERS = new Unit("m", "meters");
    public static final Unit CENTIMETERS = new Unit("cm", "centimeters");
    public static final Unit MILLIMETERS = new Unit("mm", "millimeters");
    public static final Unit MICROMETERS = new Unit("um", "micrometers");
    public static final Unit NANOMETERS = new Unit("nm", "nanometers");
    public static final Unit ANGSTROMS = new Unit("A", "angstroms");
    
    /**
     * Time units.
     */
    public static final Unit HOURS = new Unit("hr", "hours");
    public static final Unit MINUTES = new Unit("min", "minutes");
    public static final Unit SECONDS = new Unit("sec", "seconds");
    public static final Unit MILLISECONDS = new Unit("msec", "milliseconds");
    public static final Unit MICROSECONDS = new Unit("usec", "microseconds");
    public static final Unit NANOSECONDS = new Unit("nsec", "nanoseconds");

    /**
     * Frequency units.
     */
    public static final Unit HERTZ = new Unit("hz", "hertz");
    
    public static final Unit PART_PER_MILLION = new Unit("ppm", "part per million");
    public static final Unit RADIANS_PER_SECOND = new Unit("rads", "radians per second");
    
    /**
     * Data Type Constants
     */
    /**
     * 1 bit per voxel
     */
    public static final DataType BOOLEAN = new DataType(0, "Boolean");
    
    /**
     * 1 byte per voxel
     */
    public static final DataType BYTE = new DataType(1, "Byte");
    
    /**
     * 1 byte per voxel
     */
    public static final DataType UBTYPE = new DataType(2, "Usigned Byte");
    
    /**
     * 2 bytes per voxel
     */
    public static final DataType SHORT = new DataType(3, "Short");
    
    /**
     * 2 bytes per voxel
     */
    public static final DataType USHORT= new DataType(4, "Unsigned Short");
    
    /**
     * 4 bytes per voxel
     */
    public static final DataType INTEGER = new DataType(5, "Integer");
    
    /**
     * 8 bytes per voxel
     */
    public static final DataType LONG = new DataType(6, "Long");
    
    /**
     * 4 bytes per voxel
     */
    public static final DataType FLOAT = new DataType(7, "Float");
    
    /**
     * 8 bytes per voxel
     */
    public static final DataType DOUBLE = new DataType(8, "Double");
    
    /**
     * Each voxel consists of 4 channels, A=alpha, R=red, G=green, B=blue.
     * Each channel is represented by a unsigned byte value. 4 bytes per voxel.
     */
    public static final DataType ARGB = new DataType(9, "ARGB");
    
    /**
     * Each voxel consists of 4 channels, A=alpha, R=red, G=green, B=blue.
     * Each channel is represented by a unsigned short value. 8 bytes per voxel.
     */
    public static final DataType ARGB_USHORT = new DataType(10, "ARGB Ushort");
    
    /**
     * Each voxel consists of 4 channels, A=alpha, R=red, G=green, B=blue.
     * Each channel is represented by a float value. 16 bytes per voxel.
     */
    public static final DataType ARGB_FLOAT = new DataType(11, "ARGB Float");
    
    /**
     * 8 bytes per voxel.
     */
    public static final DataType COMPLEX = new DataType(12, "Complex");
    
    /**
     * 16 bytes per voxel.
     */
    public static final DataType DCOMPLEX = new DataType(13, "Complex Double");
    
    /**
     * 4 bytes per voxel
     */
    public static final DataType UINTEGER = new DataType(14, "Unsigned Integer");

    /**
     * Constants for xy plane, yz plane and zx plane.
     */
    public static final int SLICE_XY = 0;
    public static final int SLICE_YZ = 1;
    public static final int SLICE_ZX = 2;
}
