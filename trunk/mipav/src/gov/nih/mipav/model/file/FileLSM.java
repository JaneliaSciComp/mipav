package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.JamaMatrix;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * Reads Zeiss LSM 510 image files - an extension of the TIFF format. However, ordinary TIFF format supports both Little
 * Endian(0x4949 or II) byte order and Big Endian (0x4d4d or MM) byte order. LSM supports only Little Endian byte order.
 * NEW_SUBFILE_TYPE can be either 0 for images or 1 for thumbnails. The reader will skip over thumbnails and not include
 * them in the read image. COMPRESSION can be either 1 for no compression or 5 for LZW compression.
 *
 * <p>One big difference between standard TIFF and the LSM format is the inclusion of the private CZ_LSMINFO tag. The
 * pixel resolutions must be obtained from the VoxelSizeX, VoxelSizeY, and VoxelSizeZ fields in this private tag. This
 * private tag appears only in the first image directory, so fileInfo.set operations must cycle thru all the slices
 * using the values obtained when reading the first slice.</p>
 *
 * <p>The x and y resolution units are always be in meters. LSM data may be 1, 2, or 3 channels. 1 data is black and
 * white. Both 2 and 3 channels are RGB although in 2 channels only 2 of the 3 colors are used. LSM format uses either 8
 * bit unsigned data, 12 bit unsigned data, or 32 bit floats. Ths 12 bit unsigned data is stored in 16 bit shorts. If 2
 * or 3 channels use 12 bit unsigned data, then ARGB_USHORT format must be used, a format not found in ordinary TIFF. If
 * a LSM file has 4 or more channels, then create another dimension in which to put each spectrum. The color map values
 * are scaled to go only to 255.0 since a value of 65535 was observed in the unscaled map.</p>
 *
 * <p>Ordinary TIFF uses the SAMPLE_FORMAT FIELD to specify how to interpret data with 1 = unsigned integer data, 2 =
 * two's complement signed integer data, 3 = IEEE floating point data, and 4 = undefined data format. The default is 1,
 * unsigned integer data. LSM files do not include the SAMPLE_FORMAT field, so if only TIFF rules were used the LSM 32
 * bit floating point numbers would be interpreted as unsigned integers.</p>
 *
 * <p>The LSM release 6.0 documentation does not completely conform with the observed files. Table 16 with the
 * CZ-Private tag shows a 488 byte structure, but at least some LSM files have values of 512 bytes in the
 * s32StructureSize field. u32OffsetTimeStamps appears to point to a time stamps structure, but this structure contains
 * the ascii such as Ch1-T1, Ch2-T2, and Ch3-T3 which does not appear in the table 22 structure for time stamp
 * information. u32OffsetNextRecording appears to point to the start of a new structure, but it is not a second file
 * header.</p>
 *
 * @see  FileIO
 * @see  FileInfoLSM
 * @see  FileRaw
 */

public class FileLSM extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** TIFF Types. */
    private static final int BYTE = 1; // 8  bit unsigned

    /** DOCUMENT ME! */
    private static final int ASCII = 2; // 7  bit ASCII

    /** DOCUMENT ME! */
    private static final int SHORT = 3; // 16 bit unsigned

    /** DOCUMENT ME! */
    private static final int LONG = 4; // 32 bit unsigned    ****** 4 bytes !!!!

    /** DOCUMENT ME! */
    private static final int RATIONAL = 5; // 2  longs 1st numerator

    /** 2nd denom. */
    private static final int SBYTE = 6; // 8 bit signed

    /** DOCUMENT ME! */
    private static final int UNDEFINED = 7; // 8 bit undefined

    /** DOCUMENT ME! */
    private static final int SSHORT = 8; // 16 bit signed

    /** DOCUMENT ME! */
    private static final int SLONG = 9; // 32 bit signed

    /** DOCUMENT ME! */
    private static final int SRATIONAL = 10; //

    /** DOCUMENT ME! */
    private static final int FLOAT = 11; // single precision 4 byte IEEE format

    /** DOCUMENT ME! */
    private static final int DOUBLE = 12; // double precision 8 byte IEEE format

    /** TIFF Tags. */
    private static final int NEW_SUBFILE_TYPE = 254;

    /** DOCUMENT ME! */
    private static final int IMAGE_WIDTH = 256;

    /** DOCUMENT ME! */
    private static final int IMAGE_LENGTH = 257;

    /** DOCUMENT ME! */
    private static final int BITS_PER_SAMPLE = 258;

    /** DOCUMENT ME! */
    private static final int COMPRESSION = 259; // 1=no compression, 2=modified huffman,

    /** 3 = CCITT-T4, 4 = CCITT-T6, 5 = LZW, 32773 = packbits. */
    private static final int PHOTO_INTERP = 262;

    /** DOCUMENT ME! */
    private static final int IMAGE_DESCRIPTION = 270;

    /** DOCUMENT ME! */
    private static final int STRIP_OFFSETS = 273;

    /** DOCUMENT ME! */
    private static final int SAMPLES_PER_PIXEL = 277;

    /** DOCUMENT ME! */
    private static final int ROWS_PER_STRIP = 278;

    /** DOCUMENT ME! */
    private static final int STRIP_BYTE_COUNTS = 279;

    /** DOCUMENT ME! */
    private static final int MIN_SAMPLE_VALUE = 280;

    /** DOCUMENT ME! */
    private static final int MAX_SAMPLE_VALUE = 281;

    /** DOCUMENT ME! */
    private static final int XRESOLUTION = 282;

    /** DOCUMENT ME! */
    private static final int YRESOLUTION = 283;

    /** DOCUMENT ME! */
    private static final int PLANAR_CONFIG = 284;

    /** DOCUMENT ME! */
    private static final int RESOLUTION_UNIT = 296;

    /** DOCUMENT ME! */
    private static final int SOFTWARE = 305;

    /** DOCUMENT ME! */
    private static final int DATE_TIME = 306;

    /** DOCUMENT ME! */
    private static final int PREDICTOR = 317;

    /** DOCUMENT ME! */
    private static final int COLOR_MAP = 320;

    /** DOCUMENT ME! */
    private static final int TILE_WIDTH = 322;

    /** DOCUMENT ME! */
    private static final int TILE_LENGTH = 323;

    /** DOCUMENT ME! */
    private static final int TILE_OFFSETS = 324;

    /** DOCUMENT ME! */
    private static final int TILE_BYTE_COUNTS = 325;

    /** DOCUMENT ME! */
    private static final int SAMPLE_FORMAT = 339;

    /** DOCUMENT ME! */
    private static final int CZ_LSMINFO = 34412;

    /** Values for entries in scan information. */
    private static final int SUBBLOCK_RECORDING = 0x10000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_LASERS = 0x30000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_LASER = 0x50000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_TRACKS = 0x20000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_TRACK = 0x40000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_DETECTION_CHANNELS = 0x60000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_DETECTION_CHANNEL = 0x70000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_ILLUMINATION_CHANNELS = 0x80000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_ILLUMINATION_CHANNEL = 0x90000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_BEAM_SPLITTERS = 0xA0000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_BEAM_SPLITTER = 0xB0000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_DATA_CHANNELS = 0xC0000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_DATA_CHANNEL = 0xD0000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_TIMERS = 0x11000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_TIMER = 0x12000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_MARKERS = 0x13000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_MARKER = 0x14000000;

    /** DOCUMENT ME! */
    private static final int SUBBLOCK_END = 0xFFFFFFFF;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_NAME = 0x10000001;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_DESCRIPTION = 0x10000002;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_NOTES = 0x10000003;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_OBJECTIVE = 0x10000004;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_PROCESSING_SUMMARY = 0x10000005;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_SPECIAL_SCAN_MODE = 0x10000006;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_SCAN_TYPE = 0x10000007;

    /** DOCUMENT ME! */
    private static final int OLEDB_RECORDING_ENTRY_SCAN_MODE = 0x10000008;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_NUMBER_OF_STACKS = 0x10000009;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_LINES_PER_PLANE = 0x1000000A;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_SAMPLES_PER_LINE = 0x1000000B;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_PLANES_PER_VOLUME = 0x1000000C;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_IMAGES_WIDTH = 0x1000000D;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_IMAGES_HEIGHT = 0x1000000E;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_IMAGES_NUMBER_PLANES = 0x1000000F;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_IMAGES_NUMBER_STACKS = 0x10000010;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_IMAGES_NUMBER_CHANNELS = 0x10000011;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_LINSCAN_XY_SIZE = 0x10000012;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_SCAN_DIRECTION = 0x10000013;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_TIME_SERIES = 0x10000014;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_ORIGINAL_SCAN_DATA = 0x10000015;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_ZOOM_X = 0x10000016;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_ZOOM_Y = 0x10000017;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_ZOOM_Z = 0x10000018;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_SAMPLE_0X = 0x10000019;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_SAMPLE_0Y = 0x1000001A;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_SAMPLE_0Z = 0x1000001B;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_SAMPLE_SPACING = 0x1000001C;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_LINE_SPACING = 0x1000001D;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_PLANE_SPACING = 0x1000001E;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_PLANE_WIDTH = 0x1000001F;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_PLANE_HEIGHT = 0x10000020;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_VOLUME_DEPTH = 0x10000021;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_ROTATION = 0x10000034;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_NUTATION = 0x10000023;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_PRECESSION = 0x10000035;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_SAMPLE_0TIME = 0x10000036;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_START_SCAN_TRIGGER_IN = 0x10000037;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_START_SCAN_TRIGGER_OUT = 0x10000038;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_START_SCAN_EVENT = 0x10000039;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_START_SCAN_TIME = 0x10000040;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_STOP_SCAN_TRIGGER_IN = 0x10000041;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_STOP_SCAN_TRIGGER_OUT = 0x10000042;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_STOP_SCAN_EVENT = 0x10000043;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_STOP_SCAN_TIME = 0x10000044;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_USE_ROIS = 0x10000045;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_USE_REDUCED_MEMORY_ROIS = 0x10000046;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_USER = 0x10000047;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_USE_BCCORRECTION = 0x10000048;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_POSITION_BCCORRECTION1 = 0x10000049;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_POSITION_BCCORRECTION2 = 0x10000050;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_INTERPOLATIONY = 0x10000051;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_CAMERA_BINNING = 0x10000052;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_CAMERA_SUPERSAMPLING = 0x10000053;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_CAMERA_FRAME_WIDTH = 0x10000054;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_CAMERA_FRAME_HEIGHT = 0x10000055;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_CAMERA_OFFSETX = 0x10000056;

    /** DOCUMENT ME! */
    private static final int RECORDING_ENTRY_CAMERA_OFFSETY = 0x10000057;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_MULTIPLEX_TYPE = 0x40000001;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_MULTIPLEX_ORDER = 0x40000002;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_SAMPLING_MODE = 0x40000003;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_SAMPLING_METHOD = 0x40000004;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_SAMPLING_NUMBER = 0x40000005;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ACQUIRE = 0x40000006;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_SAMPLE_OBSERVATION_TIME = 0x40000007;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_TIME_BETWEEN_STACKS = 0x4000000B;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_NAME = 0x4000000C;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_COLLIMATOR1_NAME = 0x4000000D;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_COLLIMATOR1_POSITION = 0x4000000E;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_COLLIMATOR2_NAME = 0x4000000F;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_COLLIMATOR2_POSITION = 0x40000010;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_IS_BLEACH_TRACK = 0x40000011;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_IS_BLEACH_AFTER_SCAN_NUMBER = 0x40000012;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_BLEACH_SCAN_NUMBER = 0x40000013;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_TRIGGER_IN = 0x40000014;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_TRIGGER_OUT = 0x40000015;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_IS_RATIO_TRACK = 0x40000016;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_BLEACH_COUNT = 0x40000017;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_SPI_CENTER_WAVELENGTH = 0x40000018;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_PIXEL_TIME = 0x40000019;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ID_CONDENSOR_FRONTLENS = 0x40000020;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_CONDENSOR_FRONTLENS = 0x40000021;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ID_FIELD_STOP = 0x40000022;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_FIELD_STOP_VALUE = 0x40000023;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ID_CONDENSOR_APERTURE = 0x40000024;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_CONDENSOR_APERTURE = 0x40000025;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ID_CONDENSOR_REVOLVER = 0x40000026;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_CONDENSOR_FILTER = 0x40000027;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ID_TRANSMISSION_FILTER1 = 0x40000028;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ID_TRANSMISSION1 = 0x40000029;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ID_TRANSMISSION_FILTER2 = 0x40000030;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ID_TRANSMISSION2 = 0x40000031;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_REPEAT_BLEACH = 0x40000032;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_ENABLE_SPOT_BLEACH_POS = 0x40000033;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_SPOT_BLEACH_POSX = 0x40000034;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_SPOT_BLEACH_POSY = 0x40000035;

    /** DOCUMENT ME! */
    private static final int TRACK_ENTRY_BLEACH_POSITION_Z = 0x40000036;

    /** DOCUMENT ME! */
    private static final int LASER_ENTRY_NAME = 0x50000001;

    /** DOCUMENT ME! */
    private static final int LASER_ENTRY_ACQUIRE = 0x50000002;

    /** DOCUMENT ME! */
    private static final int LASER_ENTRY_POWER = 0x50000003;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_INTEGRATION_MODE = 0x70000001;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_SPECIAL_MODE = 0x70000002;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_DETECTOR_GAIN_FIRST = 0x70000003;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_DETECTOR_GAIN_LAST = 0x70000004;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_AMPLIFIER_GAIN_FIRST = 0x70000005;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_AMPLIFIER_GAIN_LAST = 0x70000006;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_AMPLIFIER_OFFS_FIRST = 0x70000007;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_AMPLIFIER_OFFS_LAST = 0x70000008;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_PINHOLE_DIAMETER = 0x70000009;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_COUNTING_TRIGGER = 0x7000000A;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_ENTRY_ACQUIRE = 0x7000000B;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_POINT_DETECTOR_NAME = 0x7000000C;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_AMPLIFIER_NAME = 0x7000000D;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_PINHOLE_NAME = 0x7000000E;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_FILTER_SET_NAME = 0x7000000F;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_FILTER_NAME = 0x70000010;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_INTEGRATOR_NAME = 0x70000013;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_DETECTION_CHANNEL_NAME = 0x70000014;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_DETECTOR_GAIN_BC1 = 0x70000015;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_DETECTOR_GAIN_BC2 = 0x70000016;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_AMPLIFIER_GAIN_BC1 = 0x70000017;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_AMPLIFIER_GAIN_BC2 = 0x70000018;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_AMPLIFIER_OFFSET_BC1 = 0x70000019;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_AMPLIFIER_OFFSET_BC2 = 0x70000020;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_SPECTRAL_SCAN_CHANNELS = 0x70000021;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_SPI_WAVELENGTH_START = 0x70000022;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_SPI_WAVELENGTH_END = 0x70000023;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_DYE_NAME = 0x70000026;

    /** DOCUMENT ME! */
    private static final int DETCHANNEL_DYE_FOLDER = 0x70000027;

    /** DOCUMENT ME! */
    private static final int ILLUMCHANNEL_ENTRY_NAME = 0x90000001;

    /** DOCUMENT ME! */
    private static final int ILLUMCHANNEL_ENTRY_POWER = 0x90000002;

    /** DOCUMENT ME! */
    private static final int ILLUMCHANNEL_ENTRY_WAVELENGTH = 0x90000003;

    /** DOCUMENT ME! */
    private static final int ILLUMCHANNEL_ENTRY_ACQUIRE = 0x90000004;

    /** DOCUMENT ME! */
    private static final int ILLUMCHANNEL_DETCHANNEL_NAME = 0x90000005;

    /** DOCUMENT ME! */
    private static final int ILLUMCHANNEL_POWER_BC1 = 0x90000006;

    /** DOCUMENT ME! */
    private static final int ILLUMCHANNEL_POWER_BC2 = 0x90000007;

    /** DOCUMENT ME! */
    private static final int BEAMSPLITTER_ENTRY_FILTER_SET = 0xB0000001;

    /** DOCUMENT ME! */
    private static final int BEAMSPLITTER_ENTRY_FILTER = 0xB0000002;

    /** DOCUMENT ME! */
    private static final int BEAMSPLITTER_ENTRY_NAME = 0xB0000003;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_NAME = 0xD0000001;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_COLOR = 0xD0000004;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_SAMPLETYPE = 0xD0000005;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_BITSPERSAMPLE = 0xD0000006;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_TYPE = 0xD0000007;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_TRACK1 = 0xD0000008;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_TRACK2 = 0xD0000009;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_CHANNEL1 = 0xD000000A;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_CHANNEL2 = 0xD000000B;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_CONST1 = 0xD000000C;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_CONST2 = 0xD000000D;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_CONST3 = 0xD000000E;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_CONST4 = 0xD000000F;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_CONST5 = 0xD0000010;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_CONST6 = 0xD0000011;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES1 = 0xD0000012;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES2 = 0xD0000013;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_DYE_NAME = 0xD0000014;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_DYE_FOLDER = 0xD0000015;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_SPECTRUM = 0xD0000016;

    /** DOCUMENT ME! */
    private static final int DATACHANNEL_ENTRY_ACQUIRE = 0xD0000017;

    /** DOCUMENT ME! */
    private static final int TIMER_ENTRY_NAME = 0x12000001;

    /** DOCUMENT ME! */
    private static final int TIMER_ENTRY_DESCRIPTION = 0x12000002;

    /** DOCUMENT ME! */
    private static final int TIMER_ENTRY_INTERVAL = 0x12000003;

    /** DOCUMENT ME! */
    private static final int TIMER_ENTRY_TRIGGER_IN = 0x12000004;

    /** DOCUMENT ME! */
    private static final int TIMER_ENTRY_TRIGGER_OUT = 0x12000005;

    /** DOCUMENT ME! */
    private static final int TIMER_ENTRY_ACTIVATION_TIME = 0x12000006;

    /** DOCUMENT ME! */
    private static final int TIMER_ENTRY_ACTIVATION_NUMBER = 0x12000007;

    /** DOCUMENT ME! */
    private static final int MARKER_ENTRY_NAME = 0x14000001;

    /** DOCUMENT ME! */
    private static final int MARKER_ENTRY_DESCRIPTION = 0x14000002;

    /** DOCUMENT ME! */
    private static final int MARKER_ENTRY_TRIGGER_IN = 0x14000003;

    /** DOCUMENT ME! */
    private static final int MARKER_ENTRY_TRIGGER_OUT = 0x14000004;

    /** Values for types in scan information. */
    private static final int TYPE_SUBBLOCK = 0; // start of end of a subblock

    /** DOCUMENT ME! */
    private static final int TYPE_LONG = 4; // 32 bit signed integer

    /** DOCUMENT ME! */
    private static final int TYPE_RATIONAL = 5; // 64 bit floating point

    /** DOCUMENT ME! */
    private static final int TYPE_ASCII = 2; // zero terminated string

    /** Bleached ROI shapes. */
    private static final int RECTANGLE = 18;

    /** DOCUMENT ME! */
    private static final int ELLIPSE = 19;

    /** DOCUMENT ME! */
    private static final int CLOSED_POLYLINE = 20;

    /** DOCUMENT ME! */
    private static final int CLOSED_BEZIER = 22;

    /** DOCUMENT ME! */
    private static final int CIRCLE = 24;

    /** Types of drawing elements. */
    private static final int VECTOR_OVERLAY = 1;

    /** DOCUMENT ME! */
    private static final int ROI = 2;

    /** DOCUMENT ME! */
    private static final int BLEACH_ROI = 3;

    /** DOCUMENT ME! */
    private static final int MEANOFROIS_OVERLAY = 4;

    /** DOCUMENT ME! */
    private static final int TOPOLSOLINE_OVERLAY = 5;

    /** DOCUMENT ME! */
    private static final int TOPOPROFILE_OVERLAY = 6;

    /** DOCUMENT ME! */
    private static final int LINESCAN_OVERLAY = 7;

    /** Types of LUTS. */
    private static final int INPUT_LUT = 1;

    /** DOCUMENT ME! */
    private static final int OUTPUT_LUT = 2;

    /** EchoTech Tiff TAGS. */
    private static final int ZRESOLUTION = 65000;

    /** DOCUMENT ME! */
    private static final int TRESOLUTION = 65001;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int bleachedROIShape = -1;

    /** DOCUMENT ME! */
    private double[] bleachKnotX = null;

    /** DOCUMENT ME! */
    private double[] bleachKnotY = null;

    /** DOCUMENT ME! */
    private int[] blueArray = null;

    /** DOCUMENT ME! */
    private int blueOffset = 2;

    /** DOCUMENT ME! */
    private byte[] byteBuffer = null;

    /** DOCUMENT ME! */
    private int[] channelDataTypes;

    /** DOCUMENT ME! */
    private String[] channelNames;

    /** DOCUMENT ME! */
    private boolean chunky = true;

    /** DOCUMENT ME! */
    private int czChannels;

    /** DOCUMENT ME! */
    private int czDataType;

    /** DOCUMENT ME! */
    private int czDataType2;

    /** DOCUMENT ME! */
    private int czDimT = 1;

    /** DOCUMENT ME! */
    private int czDimX;

    /** DOCUMENT ME! */
    private int czDimY;

    /** DOCUMENT ME! */
    private int czDimZ = 1;

    /** DOCUMENT ME! */
    private int czScanType;

    /** DOCUMENT ME! */
    private int czSpectralScan;

    @SuppressWarnings("unchecked")
    private Vector<Index>[] dataOffsets = new Vector[8000];

    /** DOCUMENT ME! */
    private byte[] dateTime;

    /** DOCUMENT ME! */
    private byte[] decomp = null;
    
    private int dimensionM;
    
    private int dimensionP;
    
    private int dimensionsReserved[] = new int[16];

    /** DOCUMENT ME! */
    private double displayAspectTime = 0.0;

    /** DOCUMENT ME! */
    private double displayAspectX = 0.0;

    /** DOCUMENT ME! */
    private double displayAspectY = 0.0;

    /** DOCUMENT ME! */
    private double displayAspectZ = 0.0;

    /** DOCUMENT ME! */
    private boolean doTile = false; // true if tiles are used

    /** DOCUMENT ME! */
    private int drawingElement;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private String[] eventDescription;

    /** DOCUMENT ME! */
    private double[] eventTime;

    /** DOCUMENT ME! */
    private int[] eventType;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private int fileDataType;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoLSM fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private int firstSliceAfterBleach = -1;

    /** DOCUMENT ME! */
    private int[] greenArray = null;

    /** DOCUMENT ME! */
    private int greenOffset = 1;

    /** DOCUMENT ME! */
    private boolean haveBleachedBezier = false;

    /** DOCUMENT ME! */
    private boolean haveBleachedCircle = false;

    /** DOCUMENT ME! */
    private boolean haveBleachedCirclePerim3 = false;

    /** DOCUMENT ME! */
    private boolean haveBleachedEllipse = false;

    /** DOCUMENT ME! */
    private boolean haveBleachedPolyline = false;

    /** DOCUMENT ME! */
    private boolean haveBleachedRectangle = false;

    /** DOCUMENT ME! */
    private int[] IFDoffsets = new int[8192];

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private byte[] imageDescription;

    /** DOCUMENT ME! */
    private int imageSlice = 0;

    /** DOCUMENT ME! */
    private float[][] img3DMultiBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols;
    
    private int internalUse1;

    /** DOCUMENT ME! */
    private double[] knotX = null;

    /** DOCUMENT ME! */
    private double[] knotY = null;

    /** DOCUMENT ME! */
    private int lastSliceBeforeBleach = -1;

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private int lutKind;

    /** DOCUMENT ME! */
    private boolean lzwCompression = false;

    /** DOCUMENT ME! */
    private TIFFLZWDecoder lzwDecoder = null; // for decoding LZW compressed images

    /** DOCUMENT ME! */
    private boolean manySpectrums = false; // true only if 4 or more spectrums per pixel

    /** DOCUMENT ME! */
    private int mono;

    /** DOCUMENT ME! */
    private int numberColors = 0;

    /** DOCUMENT ME! */
    private double objectiveSphereCorrection = -1.0;
    
    private int offsetAcquisitionParameters;

    /** DOCUMENT ME! */
    private int offsetBleachRoi;

    /** DOCUMENT ME! */
    private int offsetChannelColors;

    /** DOCUMENT ME! */
    private int offsetChannelDataTypes;

    /** DOCUMENT ME! */
    private int offsetChannelFactors;

    /** DOCUMENT ME! */
    private int offsetChannelWavelength;
    
    private int offsetCharacteristics;

    /** DOCUMENT ME! */
    private int offsetEventList;

    /** DOCUMENT ME! */
    private int offsetInputLut;

    /** DOCUMENT ME! */
    private int offsetKsData;

    /** DOCUMENT ME! */
    private int offsetLinescanOverlay;

    /** DOCUMENT ME! */
    private int offsetMeanOfRoisOverlay;

    /** DOCUMENT ME! */
    // private int offsetNextRecording;

    /** DOCUMENT ME! */
    private int offsetOutputLut;

    /** DOCUMENT ME! */
    private int offsetRoi;
    
    private int offsetPalette;
    
    private int offsetPositions;

    /** DOCUMENT ME! */
    private int offsetScanInformation;
    
    private int offsetTilePositions;

    /** DOCUMENT ME! */
    private int offsetTimeStamps;

    /** DOCUMENT ME! */
    private int offsetTopolsolineOverlay;

    /** DOCUMENT ME! */
    private int offsetTopoProfileOverlay;

    /** DOCUMENT ME! */
    private int offsetUnmixParameters;

    /** DOCUMENT ME! */
    private int offsetVectorOverlay;
    
    /** The x-offset of the center of the image in meter relative to the optical axis.
     * For LSM images the x-direction is the direction of the x-scanner.
     * In releases prior to 4.0 the entry was not used and the value 0 was written instead.
     */
    private double originX =0.0;
    
    /** The y-offset of the center of the image in meter relative to the optical axis.
     * For LSM images the y-direction is the direction of the y-scanner.
     * In releases prior to 4.0 the entry was not used and the value 0 was written instead.
     */
    private double originY = 0.0;


    /** DOCUMENT ME! */
    private boolean packBit = false; // true if the read data file has pack bit compression

    /** DOCUMENT ME! */
    private VOI photoBleachedVOI;

    /** DOCUMENT ME! */
    private int predictor = 1; // predictor for lzw decoding

    /** DOCUMENT ME! */
    private int[] redArray = null;

    /** Default ordering is red, green, blue. */
    private int redOffset = 0;
    
    private int reserved[] = new int[9];

    /** DOCUMENT ME! */
    private int rowsPerStrip = 0;

    /** private short nDirEntries;. */
    private int samplesPerPixel = 1;

    /** DOCUMENT ME! */
    //private String[] scanString = null;

    /** DOCUMENT ME! */
    private int secondAddress;

    /** DOCUMENT ME! */
    private int secondImage = 0; // address of TIFF header of second image if present: 0 if not present

    /** DOCUMENT ME! */
    private byte[] software;

    /** DOCUMENT ME! */
    private int spectrumNumber = 1;

    /** DOCUMENT ME! */
    private String str;

    /** DOCUMENT ME! */
    private boolean thumbNail = false;

    /** DOCUMENT ME! */
    private long thumbnailOffset = 0;

    /** DOCUMENT ME! */
    private int thumbNailX;

    /** DOCUMENT ME! */
    private int thumbNailY;

    /** DOCUMENT ME! */
    private int[] tileByteCounts;

    /** DOCUMENT ME! */
    private int tileByteNumber;

    /** DOCUMENT ME! */
    private int tileLength = 0;

    /** DOCUMENT ME! */
    private int tileMaxByteCount = 0;

    /** DOCUMENT ME! */
    private int tileOffsetNumber;

    /** DOCUMENT ME! */
    private long[] tileOffsets;

    /** DOCUMENT ME! */
    private int tilesAcross;

    /** DOCUMENT ME! */
    private int tilesDown;

    /** DOCUMENT ME! */
    private int tilesPerImage;

    /** DOCUMENT ME! */
    private int tilesPerSlice;

    /** DOCUMENT ME! */
    private long[] tileTemp;

    /** DOCUMENT ME! */
    private int tileWidth;
    
    private double timeDifferenceX;
    
    private double timeDifferenceY;
    
    private double timeDifferenceZ;

    /** DOCUMENT ME! */
    private double timeInterval;

    /** DOCUMENT ME! */
    private double[] timeStamp;

    /** DOCUMENT ME! */
    private int toolbarFlags;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private int[] unitsOfMeasure;

    /** DOCUMENT ME! */
    private double voxelSizeX;

    /** DOCUMENT ME! */
    private double voxelSizeY;

    /** DOCUMENT ME! */
    private double voxelSizeZ;

    /** DOCUMENT ME! */
    private int wavelengthNumber;

    /** DOCUMENT ME! */
    private double[] wavelengths = null;

    /** DOCUMENT ME! */
    private int xDim = 0;

    /** DOCUMENT ME! */
    private int yDim = 0;

    /** DOCUMENT ME! */
    private int zDim = 1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * LSM reader/writer constructor.
     *
     * @param      fileName       file name
     * @param      fileDir        file directory
     * @param      secondAddress  the position within the image file to begin reading from
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileLSM(String fileName, String fileDir, int secondAddress) throws IOException {

        UI = ViewUserInterface.getReference();
        this.fileName = fileName;
        this.fileDir = fileDir;
        this.secondAddress = secondAddress;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        int i;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        file = null;
        image = null;
        bleachKnotX = null;
        bleachKnotY = null;
        blueArray = null;
        byteBuffer = null;
        channelDataTypes = null;
        if (channelNames != null) {
            for (i = 0; i < channelNames.length; i++) {
            channelNames[i] = null;
            }
            channelNames = null;
        }
        if (dataOffsets != null) {
            for (i = 0; i < dataOffsets.length; i++) {
                if (dataOffsets[i] != null) {
                    dataOffsets[i].removeAllElements();
                    dataOffsets[i] = null;
                }
            }
            dataOffsets = null;
        }
        dateTime = null;
        decomp = null;
        if (eventDescription != null) {
            for (i = 0; i < eventDescription.length; i++) {
                eventDescription[i] = null;
            }
            eventDescription = null;
        }
        eventTime = null;
        eventType = null;
        greenArray = null;
        IFDoffsets = null;
        imageDescription = null;
        if (img3DMultiBuffer != null) {
            for (i = 0; i < img3DMultiBuffer.length; i++) {
            img3DMultiBuffer[i] = null;
            }
            img3DMultiBuffer = null;
        }
        imgBuffer = null;
        imgResols = null;
        knotX = null;
        knotY = null;
        LUT = null;
        lzwDecoder = null;
        redArray = null;
        //scanString = null;
        software = null;
        str = null;
        tileByteCounts = null;
        tileOffsets = null;
        tileTemp = null;
        timeStamp = null;
        unitsOfMeasure = null;
        wavelengths = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Accessor that returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }

    /**
     * Accessor that returns the image buffer.
     *
     * @return  buffer of image.
     */
    public float[][] getImage3DMultiBuffer() {
        return img3DMultiBuffer;
    }

    /**
     * Accessor that returns the image buffer.
     *
     * @return  buffer of image.
     */
    public float[] getImageBuffer() {
        return imgBuffer;
    }

    /**
     * Rreturns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * Accessor that returns the address of the TIF header of the second image if present Returns zero if not present.
     *
     * @return  secondImage
     */
    public int getSecondImage() {
        return secondImage;
    }

    /**
     * Reads the Tiff header which indicates endianess, the TIFF magic number, and the offset in bytes of the first IFD.
     * It then reads all the IFDs. This method then opens a Model of an image and imports the the images one slice at a
     * time. Image slices are separated by an IFD.
     *
     * @param      multiFile  <code>true</code> if a set of files each containing a separate 2D image is present <code>
     *                        false</code> if one file with either a 2D image or a stack of 2D images
     * @param      one        DOCUMENT ME!
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean multiFile, boolean one) throws IOException {
        int[] imgExtents;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");

            raFile.seek(secondAddress);

            short byteOrder = raFile.readShort();

            if (byteOrder == 0x4949) {
                endianess = FileBase.LITTLE_ENDIAN;
            } else {
                raFile.close();
                throw new IOException("LSM Read Header: Error - first 2 bytes are an illegal " + byteOrder);
            }

            int magicTIFFNumber = getUnsignedShort(endianess);

            if (magicTIFFNumber != 42) {
                raFile.close();
                throw new IOException("Tiff Read Header: Error - Invalid Magic number = " + magicTIFFNumber);
            }

            long saveLoc = raFile.getFilePointer();

            fileInfo = new FileInfoLSM(fileName, fileDir, FileUtility.LSM); // dummy fileInfo
            fileInfo.setEndianess(endianess);

            imageSlice = 0;
            IFDoffsets[imageSlice] = getInt(endianess);

            boolean moreIFDs = true;
            imgResols = new float[5];

            Preferences.debug("\n ************** FileTiff.openIFD: Opening = " + fileName + "\n", Preferences.DEBUG_FILEIO);
            tileOffsetNumber = 0;
            tileByteNumber = 0;

            while (moreIFDs) { // Find number of images!!
                raFile.seek(IFDoffsets[imageSlice]);
                moreIFDs = openIFD(fileInfo);
            }

            Preferences.debug("Just past init IFD read", Preferences.DEBUG_FILEIO);

            if (doTile) {
                tilesPerSlice = tilesAcross * tilesDown;
                imageSlice = tilesPerImage / tilesPerSlice;
            } // if (doTile)
            else if (lzwCompression && !chunky) {

                // set the tile width to the xDim for use in LZW Decoder
                tileWidth = xDim;

                if (rowsPerStrip != 0) {
                    tileLength = rowsPerStrip;
                    // System.err.println("Setting tileLength to : " + rowsPerStrip);
                } else if (tileLength == 0) {

                    // System.err.println("Setting tileLength to : " + yDim);
                    tileLength = yDim;
                }

                // for testing:
                int numVectors = -1;
                int numPerVector = dataOffsets[0].size();

                do {
                    numVectors++;
                } while (dataOffsets[numVectors] != null);

                tileOffsets = new long[numVectors * numPerVector];
                tileByteCounts = new int[tileOffsets.length];
                tileMaxByteCount = 0;

                int index = 0;

                for (int i = 0; i < numVectors; i++) {

                    for (int j = 0; j < numPerVector; j++, index++) {
                        tileOffsets[index] = (long) ((Index) (dataOffsets[i].elementAt(j))).index;
                        tileByteCounts[index] = (int) ((Index) (dataOffsets[i].elementAt(j))).byteCount;

                    }
                }

                for (int i = 0; i < tileOffsets.length; i++) {

                    if (tileByteCounts[i] > tileMaxByteCount) {
                        tileMaxByteCount = tileByteCounts[i];
                    }
                    // System.err.println("Tile offset: " + tileOffsets[i] + " with bytecount: " + tileByteCounts[i]);
                }
                // System.err.println("TileMaxByteCount: " + tileMaxByteCount);

                // tileByteCounts[numVectors * 3 - 1] = (int) ( (Index) (dataOffsets[numVectors-1].elementAt(3))).index
                // -   tileOffsets[numVectors * 3 - 1]; System.err.println("Last tile byte count: " +
                // tileByteCounts[numVectors * 3 -1]);

                tilesAcross = (xDim + tileWidth - 1) / tileWidth;
                tilesDown = (yDim + tileLength - 1) / tileLength;

                tilesPerSlice = tilesAcross * tilesDown * numPerVector;
            }

            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;

            Preferences.debug("imageSlice = " + imageSlice, Preferences.DEBUG_FILEIO);

            if (manySpectrums) {

                if (imageSlice > 1) {
                    imgExtents = new int[4];
                    imgExtents[0] = xDim;
                    imgExtents[1] = yDim;
                    imgExtents[2] = spectrumNumber;
                    imgExtents[3] = imageSlice;
                    unitsOfMeasure = new int[4];
                    unitsOfMeasure[0] = Unit.MICROMETERS.getLegacyNum();
                    unitsOfMeasure[1] = Unit.MICROMETERS.getLegacyNum();
                    unitsOfMeasure[2] = Unit.UNKNOWN_MEASURE.getLegacyNum();

                    if (czDimZ > 1) {
                        unitsOfMeasure[3] = Unit.MICROMETERS.getLegacyNum();
                    } else {
                        unitsOfMeasure[3] = Unit.SECONDS.getLegacyNum();
                    }
                } else {
                    imgExtents = new int[3];
                    imgExtents[0] = xDim;
                    imgExtents[1] = yDim;
                    imgExtents[2] = spectrumNumber;
                    unitsOfMeasure = new int[3];
                    unitsOfMeasure[0] = Unit.MICROMETERS.getLegacyNum();
                    unitsOfMeasure[1] = Unit.MICROMETERS.getLegacyNum();
                    unitsOfMeasure[2] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                }
            } // if (manySpectrums)
            else { // no more than 3 spectrums per image

                if ((czDimZ > 1) && (czDimT > 1)) {
                    imgExtents = new int[4];
                    imgExtents[0] = czDimX;
                    imgExtents[1] = czDimY;
                    imgExtents[2] = czDimZ;
                    imgExtents[3] = czDimT;
                    unitsOfMeasure = new int[4];
                    unitsOfMeasure[0] = Unit.MICROMETERS.getLegacyNum();
                    unitsOfMeasure[1] = Unit.MICROMETERS.getLegacyNum();
                    unitsOfMeasure[2] = Unit.MICROMETERS.getLegacyNum();
                    unitsOfMeasure[3] = Unit.SECONDS.getLegacyNum();
                } else if (imageSlice > 1) {
                    imgExtents = new int[3];
                    imgExtents[0] = xDim;
                    imgExtents[1] = yDim;
                    imgExtents[2] = imageSlice;
                    unitsOfMeasure = new int[3];
                    unitsOfMeasure[0] = Unit.MICROMETERS.getLegacyNum();
                    unitsOfMeasure[1] = Unit.MICROMETERS.getLegacyNum();

                    if (czDimZ > 1) {
                        unitsOfMeasure[2] = Unit.MICROMETERS.getLegacyNum();
                    } else {
                        unitsOfMeasure[2] = Unit.SECONDS.getLegacyNum();
                    }
                } else {
                    imgExtents = new int[2];
                    imgExtents[0] = xDim;
                    imgExtents[1] = yDim;
                    unitsOfMeasure = new int[2];
                    unitsOfMeasure[0] = Unit.MICROMETERS.getLegacyNum();
                    unitsOfMeasure[1] = Unit.MICROMETERS.getLegacyNum();
                }
            } // no more than 3 specrums more image

            if (imgExtents.length == 2) {
                zDim = 1;
            } else {
                zDim = imgExtents[2];
            }

            fileInfo.setExtents(imgExtents);


            fileDataType = fileInfo.getDataType();

            if (multiFile == false) {

                if (one) {
                    image = new ModelImage(fileDataType, new int[] { imgExtents[0], imgExtents[1] },
                                           fileInfo.getFileName());
                } else {
                    image = new ModelImage(fileDataType, imgExtents, fileInfo.getFileName());
                }
            }

            imageSlice = 0;
            raFile.seek(saveLoc);

            moreIFDs = true;

            int i = 0;
            tileOffsetNumber = 0;
            tileByteNumber = 0;

            while (moreIFDs) {
                fileInfo = new FileInfoLSM(fileName, fileDir, FileUtility.LSM);
                fileInfo.setExtents(imgExtents);
                fileInfo.setUnitsOfMeasure(unitsOfMeasure);
                fileInfo.setDataType(fileDataType);
                fileInfo.setChannels(czChannels);
                fileInfo.setTimeDim(czDimT);
                fileInfo.setLSMDataType(czDataType);
                fileInfo.setScanType(czScanType);
                fileInfo.setSpectralScan(czSpectralScan);
                fileInfo.setLSMDataType2(czDataType2);
                fileInfo.setTimeStamp(timeStamp);
                fileInfo.setEventTime(eventTime);
                fileInfo.setEventType(eventType);
                fileInfo.setEventDescription(eventDescription);

                if (displayAspectX > 0.0) {
                    fileInfo.setDisplayAspectX(displayAspectX);
                }

                if (displayAspectY > 0.0) {
                    fileInfo.setDisplayAspectY(displayAspectY);
                }

                if (displayAspectZ > 0.0) {
                    fileInfo.setDisplayAspectZ(displayAspectZ);
                }

                if (displayAspectTime > 0.0) {
                    fileInfo.setDisplayAspectTime(displayAspectTime);
                }

                fileInfo.setObjectiveSphereCorrection(objectiveSphereCorrection);

                if ((wavelengths != null) && (wavelengths.length >= 1) && (wavelengths[0] > 0.0)) {
                    fileInfo.setWavelengths(wavelengths);
                }

                fileInfo.setRedArray(redArray);
                fileInfo.setGreenArray(greenArray);
                fileInfo.setBlueArray(blueArray);
                fileInfo.setChannelNames(channelNames);
                fileInfo.setMono(mono);
                fileInfo.setLastSliceBeforeBleach(lastSliceBeforeBleach);
                fileInfo.setFirstSliceAfterBleach(firstSliceAfterBleach);
                fileInfo.setBleachedROIShape(bleachedROIShape);
                fileInfo.setKnotX(bleachKnotX);
                fileInfo.setKnotY(bleachKnotY);
                raFile.seek(IFDoffsets[imageSlice]);
                moreIFDs = openIFD(fileInfo);

                // Set the resolutions
                fileInfo.setResolutions(imgResols);

                if (!thumbNail) {

                    if ((multiFile == false) && (one == false)) {
                        image.setFileInfo(fileInfo, i);
                    }

                    i++;
                } // if (!thumbNail)
            }

            if (manySpectrums) {

                for (i = imageSlice; i < (imageSlice * spectrumNumber); i++) {
                    image.setFileInfo(fileInfo, i);
                }
            } // if (manySpectrums)

            if (doTile) {
                imageSlice = tilesPerImage / tilesPerSlice;
            }

            if (lzwCompression) {

                if (chunky) {
                    lzwDecoder = new TIFFLZWDecoder(tileWidth, predictor, samplesPerPixel);
                } else {
                    lzwDecoder = new TIFFLZWDecoder(tileWidth, predictor, 1);
                }
                // System.err.println("Created LZW Decoder");
            }

            if (haveBleachedRectangle && (bleachKnotX[0] >= 0) && (bleachKnotX[0] <= (imgExtents[0] - 1)) &&
                    (bleachKnotY[0] >= 0) && (bleachKnotY[0] <= (imgExtents[1] - 1)) && (bleachKnotX[1] >= 0) &&
                    (bleachKnotX[1] <= (imgExtents[0] - 1)) && (bleachKnotY[1] >= 0) &&
                    (bleachKnotY[1] <= (imgExtents[1] - 1))) {

                int reply = JOptionPane.showConfirmDialog(UI.getMainFrame(), "Do you want to load the bleached ROI?",
                                                          "Load Bleached ROI", JOptionPane.YES_NO_OPTION,
                                                          JOptionPane.QUESTION_MESSAGE);

                if (reply == JOptionPane.YES_OPTION) {
                    short id = 0;
                    // 0.0f for red hue indicating photobleached VOI in FRAP

                    photoBleachedVOI = new VOI(id, "Photobleached", VOI.CONTOUR, 0.0f);

                    int[] x = new int[4];
                    int[] y = new int[4];
                    int[] z = new int[4];
                    x[0] = (int) Math.round(bleachKnotX[0]);
                    y[0] = (int) Math.round(bleachKnotY[0]);
                    x[1] = (int) Math.round(bleachKnotX[1]);
                    y[1] = (int) Math.round(bleachKnotY[0]);
                    x[2] = (int) Math.round(bleachKnotX[1]);
                    y[2] = (int) Math.round(bleachKnotY[1]);
                    x[3] = (int) Math.round(bleachKnotX[0]);
                    y[3] = (int) Math.round(bleachKnotY[1]);

                    for (i = 0; i < zDim; i++) {
                        z[0] = z[1] = z[2] = z[3] = i;
                        photoBleachedVOI.importCurve(x, y, z);
                    }

                    image.registerVOI(photoBleachedVOI);
                }
            } // haveBleachedRectangle
            else if (haveBleachedPolyline) {
                int nPts = bleachKnotX.length;

                if (bleachKnotY.length == nPts) {
                    boolean doPolyline = true;

                    for (i = 0; i < nPts; i++) {

                        if ((bleachKnotX[i] < 0) || (bleachKnotX[i] > (imgExtents[0] - 1)) || (bleachKnotY[i] < 0) ||
                                (bleachKnotY[i] > (imgExtents[1] - 1))) {
                            doPolyline = false;
                        }
                    } // for (i = 0; i < nPts; i++)

                    if (doPolyline) {
                        int reply = JOptionPane.showConfirmDialog(UI.getMainFrame(),
                                                                  "Do you want to load the bleached ROI?",
                                                                  "Load Bleached ROI", JOptionPane.YES_NO_OPTION,
                                                                  JOptionPane.QUESTION_MESSAGE);

                        if (reply == JOptionPane.YES_OPTION) {
                            short id = 0;
                            // 0.0f for red hue indicating photobleached VOI in FRAP

                            photoBleachedVOI = new VOI(id, "Photobleached", VOI.CONTOUR, 0.0f);

                            int[] x = new int[nPts];
                            int[] y = new int[nPts];
                            int[] z = new int[nPts];

                            for (i = 0; i < nPts; i++) {
                                x[i] = (int) Math.round(bleachKnotX[i]);
                                y[i] = (int) Math.round(bleachKnotY[i]);
                            }

                            for (i = 0; i < zDim; i++) {
                                for (int j = 0; j < nPts; j++) {
                                    z[j] = i;
                                }
                                photoBleachedVOI.importCurve(x, y, z);
                            }

                            image.registerVOI(photoBleachedVOI);
                        }
                    } // if (doPolyline)
                } // if (bleachKnotY.length == nPts)
            } // else if (haveBleachedPolyline)
            else if (haveBleachedCircle && (bleachKnotX[0] >= 0) && (bleachKnotX[0] <= (imgExtents[0] - 1)) &&
                         (bleachKnotY[0] >= 0) && (bleachKnotY[0] <= (imgExtents[1] - 1)) && (bleachKnotX[1] >= 0) &&
                         (bleachKnotX[1] <= (imgExtents[0] - 1)) && (bleachKnotY[1] >= 0) &&
                         (bleachKnotY[1] <= (imgExtents[1] - 1))) {
                double deltaX = bleachKnotX[1] - bleachKnotX[0];
                double deltaY = bleachKnotY[1] - bleachKnotY[0];
                double radius = Math.sqrt((deltaX * deltaX) + (deltaY * deltaY));

                if (((bleachKnotX[0] - radius) >= 0) && ((bleachKnotX[0] + radius) <= (imgExtents[0] - 1)) &&
                        ((bleachKnotY[0] - radius) >= 0) && ((bleachKnotY[0] + radius) <= (imgExtents[1] - 1))) {
                    int reply = JOptionPane.showConfirmDialog(UI.getMainFrame(),
                                                              "Do you want to load the bleached ROI?",
                                                              "Load Bleached ROI", JOptionPane.YES_NO_OPTION,
                                                              JOptionPane.QUESTION_MESSAGE);

                    if (reply == JOptionPane.YES_OPTION) {
                        short id = 0;
                        // 0.0f for red hue indicating photobleached VOI in FRAP

                        photoBleachedVOI = new VOI(id, "Photobleached", VOI.CONTOUR, 0.0f);

                        int[] x = new int[40];
                        int[] y = new int[40];
                        int[] z = new int[40];

                        for (i = 0; i < 40; i++) {
                            x[i] = (int) Math.round(bleachKnotX[0] + (radius * Math.cos(2.0 * Math.PI * i / 40.0)));
                            y[i] = (int) Math.round(bleachKnotY[0] + (radius * Math.sin(2.0 * Math.PI * i / 40.0)));
                        }

                        for (i = 0; i < zDim; i++) {
                            for (int j = 0; j < 40; j++) {
                                z[j] = i;
                            }
                            photoBleachedVOI.importCurve(x, y, z);
                        }

                        image.registerVOI(photoBleachedVOI);
                    }
                }
            } // haveBleachedCircle
            else if (haveBleachedCirclePerim3 && (bleachKnotX[0] >= 0) && (bleachKnotX[0] <= (imgExtents[0] - 1)) &&
                         (bleachKnotY[0] >= 0) && (bleachKnotY[0] <= (imgExtents[1] - 1)) && (bleachKnotX[1] >= 0) &&
                         (bleachKnotX[1] <= (imgExtents[0] - 1)) && (bleachKnotY[1] >= 0) &&
                         (bleachKnotY[1] <= (imgExtents[1] - 1)) && (bleachKnotX[2] >= 0) &&
                         (bleachKnotX[2] <= (imgExtents[0] - 1)) && (bleachKnotY[2] >= 0) &&
                         (bleachKnotY[2] <= (imgExtents[1] - 1))) {

                // Let the center be at a,b
                // (x - a)**2 + (y - b)**2 = r**2
                // (x0 - a)**2 + (y0 - b)**2 = r**2
                // (x1 - a)**2 + (y1 - b)**2 = r**2
                // (x2 - a)**2 + (y2 - b)**2 = r**2
                // x0**2 - 2ax0 + a**2 + y0**2 - 2by0 + b**2 =
                // x1**2 - 2ax1 + a**2 + y1**2 - 2by1 + b**2
                // x0**2 + y0**2 - x1**2 - y1**2 = 2a(x0-x1) + 2b(y0-y1)
                // x0**2 + y0**2 - x2**2 - y2**2 = 2a(x0-x2) + 2b(y0-y2)
                // If (y0 != y1) && (y0 != y2), then multiply the above equation by
                // -(y0-y1)/(y0-y2)
                double x0 = bleachKnotX[0];
                double x1 = bleachKnotX[1];
                double x2 = bleachKnotX[2];
                double y0 = bleachKnotY[0];
                double y1 = bleachKnotY[1];
                double y2 = bleachKnotY[2];
                double radius = 0.0;
                double a = 0.0;
                double b = 0.0;
                double num, denom;

                if ((y0 != y1) && (y0 != y2)) {
                    num = (x0 * x0) + (y0 * y0) - (x1 * x1) - (y1 * y1) -
                          ((y0 - y1) * ((x0 * x0) + (y0 * y0) - (x2 * x2) - (y2 * y2)) / (y0 - y2));
                    denom = (2 * (x0 - x1)) - (2 * (y0 - y1) * (x0 - x2) / (y0 - y2));
                    a = num / denom;
                    num = (x0 * x0) + (y0 * y0) - (x1 * x1) - (y1 * y1) - (2 * a * (x0 - x1));
                    denom = 2 * (y0 - y1);
                    b = num / denom;
                    radius = Math.sqrt(((x0 - a) * (x0 - a)) + ((y0 - b) * (y0 - b)));
                }
                // else if (x0 != x1) && (x0 != x2), then multiply the above equation by
                // -(x0-x1)/(x0-x2)
                else if ((x0 != x1) && (x0 != x2)) {
                    num = (x0 * x0) + (y0 * y0) - (x1 * x1) - (y1 * y1) -
                          ((x0 - x1) * ((x0 * x0) + (y0 * y0) - (x2 * x2) - (y2 * y2)) / (x0 - x2));
                    denom = (2 * (y0 - y1)) - (2 * (x0 - x1) * (y0 - y2) / (x0 - x2));
                    b = num / denom;
                    num = (x0 * x0) + (y0 * y0) - (x1 * x1) - (y1 * y1) - (2 * b * (y0 - y1));
                    denom = 2 * (x0 - x1);
                    a = num / denom;
                    radius = Math.sqrt(((x0 - a) * (x0 - a)) + ((y0 - b) * (y0 - b)));
                } else if (x0 == x1) {
                    b = (y0 + y1) / 2.0;
                    num = (x0 * x0) + (y0 * y0) - (x2 * x2) - (y2 * y2) - (2 * b * (y0 - y2));
                    denom = 2 * (x0 - x2);
                    a = num / denom;
                    radius = Math.sqrt(((x0 - a) * (x0 - a)) + ((y0 - b) * (y0 - b)));
                } else if (x0 == x2) {
                    b = (y0 + y2) / 2.0;
                    num = (x0 * x0) + (y0 * y0) - (x1 * x1) - (y1 * y1) - (2 * b * (y0 - y1));
                    denom = 2 * (x0 - x1);
                    a = num / denom;
                    radius = Math.sqrt(((x0 - a) * (x0 - a)) + ((y0 - b) * (y0 - b)));
                }

                if (((a - radius) >= 0) && ((a + radius) <= (imgExtents[0] - 1)) && ((b - radius) >= 0) &&
                        ((b + radius) <= (imgExtents[1] - 1))) {
                    int reply = JOptionPane.showConfirmDialog(UI.getMainFrame(),
                                                              "Do you want to load the bleached ROI?",
                                                              "Load Bleached ROI", JOptionPane.YES_NO_OPTION,
                                                              JOptionPane.QUESTION_MESSAGE);

                    if (reply == JOptionPane.YES_OPTION) {
                        short id = 0;
                        // 0.0f for red hue indicating photobleached VOI in FRAP

                        photoBleachedVOI = new VOI(id, "Photobleached", VOI.CONTOUR, 0.0f);

                        int[] x = new int[40];
                        int[] y = new int[40];
                        int[] z = new int[40];

                        for (i = 0; i < 40; i++) {
                            x[i] = (int) Math.round(a + (radius * Math.cos(2.0 * Math.PI * i / 40.0)));
                            y[i] = (int) Math.round(b + (radius * Math.sin(2.0 * Math.PI * i / 40.0)));
                        }

                        for (i = 0; i < zDim; i++) {
                            for (int j = 0; j < 40; j++) {
                                z[j] = i;
                            }
                            photoBleachedVOI.importCurve(x, y, z);
                        }

                        image.registerVOI(photoBleachedVOI);
                    }
                }
            } // haveBleachedCirclePerim3
            else if (haveBleachedEllipse && (bleachKnotX[0] >= 0) && (bleachKnotX[0] <= (imgExtents[0] - 1)) &&
                         (bleachKnotY[0] >= 0) && (bleachKnotY[0] <= (imgExtents[1] - 1)) && (bleachKnotX[1] >= 0) &&
                         (bleachKnotX[1] <= (imgExtents[0] - 1)) && (bleachKnotY[1] >= 0) &&
                         (bleachKnotY[1] <= (imgExtents[1] - 1)) && (bleachKnotX[2] >= 0) &&
                         (bleachKnotX[2] <= (imgExtents[0] - 1)) && (bleachKnotY[2] >= 0) &&
                         (bleachKnotY[2] <= (imgExtents[1] - 1)) && (bleachKnotX[3] >= 0) &&
                         (bleachKnotX[3] <= (imgExtents[0] - 1)) && (bleachKnotY[3] >= 0) &&
                         (bleachKnotY[3] <= (imgExtents[1] - 1))) {
                int reply = JOptionPane.showConfirmDialog(UI.getMainFrame(), "Do you want to load the bleached ROI?",
                                                          "Load Bleached ROI", JOptionPane.YES_NO_OPTION,
                                                          JOptionPane.QUESTION_MESSAGE);

                if (reply == JOptionPane.YES_OPTION) {
                    short id = 0;
                    // 0.0f for red hue indicating photobleached VOI in FRAP

                    photoBleachedVOI = new VOI(id, "Photobleached", VOI.CONTOUR, 0.0f);

                    double x0 = bleachKnotX[0];
                    double x1 = bleachKnotX[1];
                    double x2 = bleachKnotX[2];
                    double x3 = bleachKnotX[3];
                    double y0 = bleachKnotY[0];
                    double y1 = bleachKnotY[1];
                    double y2 = bleachKnotY[2];
                    double y3 = bleachKnotY[3];

                    // Major axis has length 2*a
                    // Minor axis has length 2*b
                    // Knot0 and knot2 are the two cut points of the ellipse with one
                    // of the axes and knot1 and knot3 are the cut points with the
                    // other axis
                    double axis1;
                    double axis2;
                    double theta;
                    double costh;
                    double sinth;
                    axis1 = Math.sqrt(((x0 - x2) * (x0 - x2)) + ((y0 - y2) * (y0 - y2)));
                    axis2 = Math.sqrt(((x1 - x3) * (x1 - x3)) + ((y1 - y3) * (y1 - y3)));

                    double a, b;

                    if (axis1 >= axis2) {
                        a = 0.5 * axis1;
                        b = 0.5 * axis2;

                        if (x0 >= x2) {
                            theta = Math.atan2((x0 - x2), (y0 - y2));
                        } else {
                            theta = Math.atan2((x2 - x0), (y2 - y0));
                        }
                    } else {
                        a = 0.5 * axis2;
                        b = 0.5 * axis1;

                        if (x1 >= x3) {
                            theta = Math.atan2((x1 - x3), (y1 - y3));
                        } else {
                            theta = Math.atan2((x3 - x1), (y3 - y1));
                        }
                    }

                    costh = Math.cos(theta);
                    sinth = Math.sin(theta);

                    // Find the center of the ellipse;
                    double xc, yc;
                    xc = 0.5 * (x0 + x2);
                    yc = 0.5 * (y0 + y2);

                    int[] x = new int[40];
                    int[] y = new int[40];
                    int[] z = new int[40];
                    double rp;
                    double costhp;
                    double sinthp;
                    double xp;
                    double yp;
                    double num;
                    double denom;

                    for (i = 0; i < 40; i++) {

                        // rp and thethap refer to an ellipse with its center at
                        // (0,0) and its main longer axis along the x axis
                        // The original ellipse is transformed into this centered
                        // and aligned ellipse by subtracting xcenter, ycenter from
                        // the x,y coordinates to place the center at (0,0) and the
                        // by rotating by the negative of the angle theta formed between the
                        // x axis and the major axis.
                        // xp = (x - xc)costh + (y - yc)sinth
                        // yp = -(x - xc)sinth + (y - yc)costh
                        // xp = x*costh + y*sinth - xc*costh - yc*sinth
                        // yp = -x*sinth + y*costh + xc*sinth - yc*costh (eq 1)
                        // Multiply equation (1) by -sinth/costh
                        // -sinth*yp/costh = x*sinth*sinth/costh - y*sinth
                        // -xc*sinth*sinth/costh + yc*sinth
                        // x(costh + sinth*sinth/costh) = xp - sinth*yp/costh
                        // + xc*costh + xc*sinth*sinth/costh
                        // Multiply equation (1) by costh/sinth
                        // costh*yp/sinth = -x*costh + y*costh*costh/sinth
                        // + xc*costh - yc*costh*costh/sinth
                        // y(sinth + costh*costh/sinth) = xp + costh*yp/sinth
                        // + yc*sinth + yc*costh*costh/sinth
                        costhp = Math.cos(2.0 * Math.PI * i / 40.0);
                        sinthp = Math.sin(2.0 * Math.PI * i / 40.0);
                        rp = Math.sqrt(b * b * a * a / ((b * b * costhp * costhp) + (a * a * sinthp * sinthp)));
                        xp = rp * costhp;
                        yp = rp * sinthp;

                        if ((costh != 0.0) && (sinth != 0.0)) {
                            num = xp - (sinth * yp / costh) + (xc * costh) + (xc * sinth * sinth / costh);
                            denom = costh + (sinth * sinth / costh);
                            x[i] = (int) ((num / denom) + 0.5);
                            num = xp + (costh * yp / sinth) + (yc * sinth) + (yc * costh * costh / sinth);
                            denom = sinth + (costh * costh / sinth);
                            y[i] = (int) ((num / denom) + 0.5);
                        } else if (sinth == 0.0) {
                            x[i] = (int) ((xp / costh) + xc + 0.5);
                            y[i] = (int) ((yp / costh) + yc + 0.5);
                        } else if (costh == 0.0) {
                            x[i] = (int) ((-yp / sinth) + xc + 0.5);
                            y[i] = (int) ((xp / sinth) + yc + 0.5);
                        }
                    }

                    for (i = 0; i < zDim; i++) {
                        for (int j = 0; j < 40; j++) {
                            z[j] = i;
                        }
                        photoBleachedVOI.importCurve(x, y, z);
                    }

                    image.registerVOI(photoBleachedVOI);
                }
            } // haveBleachedEllipse
            else if (haveBleachedBezier) {

                // These knots are the endpoints of cubic Bezier curve segments.
                // The 2 control points for Bezier curve segments are calculated
                // with the assumption of equidistant parameterization of the curve.
                // Believe this means C1 and C2 continuity - continuity of the first
                // and second derivatives
                // Let pn-2, pn-1, pn be the final control points of 1 Bezier segment
                // Let p0', p1', p2' be the first control points of the next
                // Bezier segment
                // p0' = pn C0 continutiy
                // p1' = pn + (pn - pn-1) C1 continuity
                // p2' = pn-2 + 4(pn - pn-1) C2 continuity
                // For a cubic Bezier with control points P1, P2, P3, and P4
                // with P1 and P4 the endpoints
                // Q(t) = (1-t)**3P1 + 3t(1-t)**2P2 + 3t**2(1-t)P3 + t**3P4
                // for 0 <= t <= 1
                int nPts = bleachKnotX.length;

                if (bleachKnotY.length == nPts) {
                    boolean doBezier = true;

                    for (i = 0; i < nPts; i++) {

                        if ((bleachKnotX[i] < 0) || (bleachKnotX[i] > (imgExtents[0] - 1)) || (bleachKnotY[i] < 0) ||
                                (bleachKnotY[i] > (imgExtents[1] - 1))) {
                            doBezier = false;
                        }
                    } // for (i = 0; i < nPts; i++)

                    if (doBezier) {
                        int reply = JOptionPane.showConfirmDialog(UI.getMainFrame(),
                                                                  "Do you want to load the bleached ROI?",
                                                                  "Load Bleached ROI", JOptionPane.YES_NO_OPTION,
                                                                  JOptionPane.QUESTION_MESSAGE);

                        if (reply == JOptionPane.YES_OPTION) {
                            short id = 0;
                            // 0.0f for red hue indicating photobleached VOI in FRAP

                            photoBleachedVOI = new VOI(id, "Photobleached", VOI.CONTOUR, 0.0f);

                            // Find the x values of the control points
                            JamaMatrix A = new JamaMatrix(2 * nPts, 2 * nPts);
                            JamaMatrix b = new JamaMatrix(2 * nPts, 1);

                            // p0Ax = 2p0x - pn-1Bx
                            // p0Bx = pn-1Ax + 4p0x - 4pn-1Bx
                            // p0Ax + pn-1Bx = 2p0x
                            // p0Bx - pn-1Ax + 4pn-1Bx = 4p0x
                            A.set(0, 0, 1);
                            A.set(0, (2 * nPts) - 1, 1);
                            b.set(0, 0, 2 * bleachKnotX[0]);
                            A.set(1, 1, 1);
                            A.set(1, (2 * nPts) - 2, -1);
                            A.set(1, (2 * nPts) - 1, 4);
                            b.set(1, 0, 4 * bleachKnotX[0]);

                            // p1Ax = 2p1x - p0Bx
                            // p1Bx = p0Ax + 4p1x - 4p0Bx
                            // p0Bx + p1Ax = 2p1x
                            // -p0Ax + 4p0Bx + p1Bx = 4p1x
                            // pi-1Bx + piAx = 2pix
                            // -pi-1Ax + 4pi-1Bx + piBx = 4pix
                            for (i = 1; i <= (nPts - 1); i++) {
                                A.set(2 * i, (2 * i) - 1, 1);
                                A.set(2 * i, 2 * i, 1);
                                b.set(2 * i, 0, 2 * bleachKnotX[i]);
                                A.set((2 * i) + 1, (2 * i) - 2, -1);
                                A.set((2 * i) + 1, (2 * i) - 1, 4);
                                A.set((2 * i) + 1, (2 * i) + 1, 1);
                                b.set((2 * i) + 1, 0, 4 * bleachKnotX[i]);
                            }

                            JamaMatrix controlX = A.solve(b);
                            b.set(0, 0, 2 * bleachKnotY[0]);
                            b.set(1, 0, 4 * bleachKnotY[0]);

                            for (i = 1; i <= (nPts - 1); i++) {
                                b.set(2 * i, 0, 2 * bleachKnotY[i]);
                                b.set((2 * i) + 1, 0, 4 * bleachKnotY[i]);
                            }

                            JamaMatrix controlY = A.solve(b);
                            int[] x = new int[10 * nPts];
                            int[] y = new int[10 * nPts];
                            int[] z = new int[10 * nPts];
                            double t;

                            for (i = 0; i < nPts; i++) {
                                double p1x = bleachKnotX[i];
                                double p1y = bleachKnotY[i];
                                double p2x = controlX.get(2 * i, 0);
                                double p2y = controlY.get(2 * i, 0);
                                double p3x = controlX.get((2 * i) + 1, 0);
                                double p3y = controlY.get((2 * i) + 1, 0);
                                double p4x, p4y;

                                if (i == (nPts - 1)) {
                                    p4x = bleachKnotX[0];
                                    p4y = bleachKnotY[0];
                                } else {
                                    p4x = bleachKnotX[i + 1];
                                    p4y = bleachKnotY[i + 1];
                                }

                                for (int j = 0; j < 10; j++) {
                                    t = 0.1 * j;
                                    x[(10 * i) + j] = (int) (((1 - t) * (1 - t) * (1 - t) * p1x) +
                                                             (3 * t * (1 - t) * (1 - t) * p2x) +
                                                             (3 * t * t * (1 - t) * p3x) + (t * t * t * p4x) + 0.5);
                                    y[(10 * i) + j] = (int) (((1 - t) * (1 - t) * (1 - t) * p1y) +
                                                             (3 * t * (1 - t) * (1 - t) * p2y) +
                                                             (3 * t * t * (1 - t) * p3y) + (t * t * t * p4y) + 0.5);
                                }
                            }

                            for (i = 0; i < zDim; i++) {
                                for (int j = 0; j < (10 * nPts); j++) {
                                    z[j] = i;
                                }
                                photoBleachedVOI.importCurve(x, y, z);
                            }

                            image.registerVOI(photoBleachedVOI);
                        }
                    } // if (doBezier)
                } // if (bleachKnotY.length == nPts)
            } // else if (haveBleachedBezier)

            int bufferSize;

            if (manySpectrums) {
                bufferSize = xDim * yDim * spectrumNumber;
            } else if (ModelImage.isColorImage(fileInfo.getDataType())) {
                bufferSize = xDim * yDim * 4;
            } else {
                bufferSize = xDim * yDim;

            }

            if (multiFile && (imgExtents.length == 3)) {

                if (img3DMultiBuffer == null) {
                    img3DMultiBuffer = new float[imageSlice][bufferSize];
                }
            } else if (imgBuffer == null) {
                imgBuffer = new float[bufferSize];
            }

            if (one) {
                imageSlice = 1;
                image.setFileInfo(fileInfo, 0);
            }


            for (i = 0; i < imageSlice; i++) {

                try {

                    if (multiFile && (imgExtents.length == 3)) {

                        if (doTile || lzwCompression) {
                            readTileBuffer(i, img3DMultiBuffer[i]);
                        } else {
                            readBuffer(i, img3DMultiBuffer[i]); // Slice a time;
                        }
                    } else {

                        if (doTile || lzwCompression) {
                            readTileBuffer(i, imgBuffer);
                        } else {

                            if (one && (imgExtents.length > 2)) {
                                readBuffer(imgExtents[2] / 2, imgBuffer);
                            } else {
                                readBuffer(i, imgBuffer); // Slice a time;
                            }
                        }
                    } // else not (multiFile && (imgExtents.length == 3))
                } catch (IOException error) {
                    throw new IOException("FileLSM: readImage: " + error);
                }

                if (multiFile == false) {
                    image.importData(i * bufferSize, imgBuffer, false);
                }
            }

            raFile.close();

        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            byteBuffer = null;
            System.gc();
            throw error;
        }

        if (multiFile == false) {
            image.calcMinMax();

            if (LUT != null) {
                LUT.resetTransferLine((float) image.getMin(), (float) image.getMax());
            }
        } // if (multiFile == false)

        return image;
    }

    /**
     * Accessor to set the file name (used when reading TIFF multiFile).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    /**
     * Reads and decodes IFDs (Image File Directory).
     *
     * @param      fileInfo  DOCUMENT ME!
     *
     * @return     DOCUMENT ME!
     *
     * @exception  IOException  if there is an error reading the file
     */
    private boolean openIFD(FileInfoLSM fileInfo) throws IOException {
        int i;
        int i1;
        int tag;
        int type;
        int count;
        int ecount;
        int[] valueArray = new int[1000];
        int value_offset;
        int nDirEntries;
        long numerator, denominator;
        float valueFloat = 0.0f;
        double valueDouble = 0.0;
        boolean haveXDim = false;
        int xDimTemp = 0;
        boolean haveYDim = false;
        int yDimTemp = 0;
        boolean haveByte = false;
        int[] byteTemp = new int[1000];
        int byteCountTemp = 0;
        boolean haveOffset = false;
        long[] offsetTemp = new long[1000];
        int offsetCountTemp = 0;
        boolean haveSamples = false;
        int samplesPerPixelTemp = 0;
        boolean havePlanar = false;
        boolean chunkyTemp = false;
        boolean haveBitsPerSample = false;
        int dataTypeTemp = 0;

        fileInfo.setEndianess(endianess);
        nDirEntries = getUnsignedShort(endianess);

        if (nDirEntries <= 0) {
            throw new IOException("First 2 IFD bytes are an illegal " + nDirEntries);
        }

        Preferences.debug("\nOpenIFD: Entries = " + nDirEntries + "\n", Preferences.DEBUG_FILEIO);

        for (i = 0; i < nDirEntries; i++) {

            tag = getUnsignedShort(endianess);

            if (tag == 0) {
                throw new IOException("Tiff Zero Tag Error");
            }

            type = getUnsignedShort(endianess);
            count = getInt(endianess);

            if ((type == SHORT) && (count == 1)) {
                valueArray[0] = getUnsignedShort(endianess);
                getUnsignedShort(endianess);
            } else if ((type == SHORT) && (count == 2)) {
                valueArray[0] = getUnsignedShort(endianess);
                valueArray[1] = getUnsignedShort(endianess);
            } else if ((type == SHORT) && (count >= 3)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 1000)); i1++) {
                    valueArray[i1] = getUnsignedShort(endianess);
                }

                raFile.seek(saveLocus);
            } else if (((type == LONG) || (type == SLONG)) && (count == 1)) {
                valueArray[0] = getInt(endianess);
            } else if (((type == LONG) || (type == SLONG)) && (count >= 2)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 1000)); i1++) {
                    valueArray[i1] = getInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == RATIONAL) || (type == SRATIONAL)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < (2 * count)) && (i1 < 1000)); i1 = i1 + 2) {
                    valueArray[i1] = getInt(endianess);
                    valueArray[i1 + 1] = getInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == DOUBLE) && (count == 1)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);
                valueDouble = getDouble(endianess);
                raFile.seek(saveLocus);
            } else if ((type == DOUBLE) && (count > 1)) {

                // Ignore these EchoTech fields for now
                value_offset = getInt(endianess);
            } else if ((type == BYTE) && (tag == CZ_LSMINFO)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);
                readCZPrivateTag();
                raFile.seek(saveLocus);
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 1)) {
                valueArray[0] = raFile.readUnsignedByte();
                raFile.readUnsignedByte();
                raFile.readUnsignedByte();
                raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 2)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                raFile.readUnsignedByte();
                raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 3)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                valueArray[2] = raFile.readUnsignedByte();
                raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 4)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                valueArray[2] = raFile.readUnsignedByte();
                valueArray[3] = raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count > 4)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 1000)); i1++) {
                    valueArray[i1] = raFile.readUnsignedByte();
                }

                raFile.seek(saveLocus);
            } else if ((type == SBYTE) && (count == 1)) {
                valueArray[0] = raFile.readByte();
                raFile.readByte();
                raFile.readByte();
                raFile.readByte();
            } else if ((type == SBYTE) && (count == 2)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                raFile.readByte();
                raFile.readByte();
            } else if ((type == SBYTE) && (count == 3)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                valueArray[2] = raFile.readByte();
                raFile.readByte();
            } else if ((type == SBYTE) && (count == 4)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                valueArray[2] = raFile.readByte();
                valueArray[3] = raFile.readByte();
            } else if ((type == SBYTE) && (count > 4)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 1000)); i1++) {
                    valueArray[i1] = raFile.readByte();
                }

                raFile.seek(saveLocus);
            } else if ((type == SSHORT) && (count == 1)) {
                valueArray[0] = getSignedShort(endianess);
                getSignedShort(endianess);
            } else if ((type == SSHORT) && (count == 2)) {
                valueArray[0] = getSignedShort(endianess);
                valueArray[1] = getSignedShort(endianess);
            } else if ((type == SSHORT) && (count >= 3)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 1000)); i1++) {
                    valueArray[i1] = getSignedShort(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == FLOAT) && (count == 1)) {
                valueFloat = getFloat(endianess);
            } else if ((type == FLOAT) && (count > 1)) {

                // Ignore these fields for now
                value_offset = getInt(endianess);
            } else {
                Preferences.debug("\nOpenIFD: Unknown field type = " + type + " Tag = " + tag + "\n",
                		Preferences.DEBUG_FILEIO);
                throw new IOException("OpenIFD: Unknown field type = " + type + " Tag = " + tag);
            }

            Preferences.debug("\nFileLSM.openIFD: Tag = " + tag + "\n", Preferences.DEBUG_FILEIO);

            switch (type) {

                case BYTE:
                    Preferences.debug("FileLSM.openIFD: Type = BYTE  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case ASCII:
                    Preferences.debug("FileLSM.openIFD: Type = ASCII  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SHORT:
                    Preferences.debug("FileLSM.openIFD: Type = SHORT  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case LONG:
                    Preferences.debug("FileTiff.openIFD: Type = LONG  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case RATIONAL:
                    Preferences.debug("FileTiff.openIFD: Type = RATIONAL  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SBYTE:
                    Preferences.debug("FileTiff.openIFD: Type = SBYTE  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case UNDEFINED:
                    Preferences.debug("FileTiff.openIFD: Type = UNDEFINED  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SSHORT:
                    Preferences.debug("FileTiff.openIFD: Type = SSHORT  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SLONG:
                    Preferences.debug("FileTiff.openIFD: Type = SLONG  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SRATIONAL:
                    Preferences.debug("FileTiff.openIFD: Type = SRATIONAL  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case FLOAT:
                    Preferences.debug("FileTiff.openIFD: Type = FLOAT  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case DOUBLE:
                    Preferences.debug("FileTiff.openIFD: Type = DOUBLE  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;
            }

            if ((type == RATIONAL) || (type == SRATIONAL)) {
                ecount = 2 * count;
            } else {
                ecount = count;
            }

            if ((type != DOUBLE) && (type != FLOAT)) {

                for (i1 = 0; ((i1 < ecount) && (i1 < 1000)); i1++) {
                    Preferences.debug("FileTiff.openIFD: value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n",
                    		Preferences.DEBUG_FILEIO);
                }
            } else if ((type == DOUBLE) && (count == 1)) {
                Preferences.debug("FIleTiff.openIFD: value = " + valueDouble + "\n", Preferences.DEBUG_FILEIO);
            } else if ((type == FLOAT) && (count == 1)) {
                Preferences.debug("fileTiff.openIFD: value = " + valueFloat + "\n", Preferences.DEBUG_FILEIO);
            }

            switch (tag) {

                case NEW_SUBFILE_TYPE:
                    if (type != LONG) {
                        throw new IOException("NEW_SUBFILE_TYPE has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("NEW_SUBFILE_TYPE has illegal count = " + count + "\n");
                    }

                    Preferences.debug("FileTiff.openIFD: tag = NEW_SUBTYPE_FILE\n", Preferences.DEBUG_FILEIO);
                    if ((valueArray[0] & 0x01) == 0x01) {
                        Preferences.debug("Image is a reduced resolution version of another " +
                                          "image in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        thumbNail = true;
                    } else {
                        Preferences.debug("Image is not a reduced resolution version of another " +
                                          "image in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        thumbNail = false;
                    }

                    if ((valueArray[0] & 0x02) == 0x02) {
                        Preferences.debug("Image is a single page of a multi-page image\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Image is not a single page of a multi-page image\n", Preferences.DEBUG_FILEIO);
                    }

                    if ((valueArray[0] & 0x04) == 0x04) {
                        Preferences.debug("Images defines a transparency mask for another image " +
                                          "in this TIFF file\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Images does not define a transparency mask for another image " +
                                          "in this TIFF file\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case IMAGE_WIDTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("IMAGE_WIDTH has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("IMAGE_WIDTH has illegal count = " + count + "\n");
                    }

                    haveXDim = true;
                    xDimTemp = valueArray[0];
                    Preferences.debug("FileTiff.openIFD: Image_Width = " + xDimTemp + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case IMAGE_LENGTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("IMAGE_LENGTH has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("IMAGE_LENGTH has illegal COUNT = " + count + "\n");
                    }

                    haveYDim = true;
                    yDimTemp = valueArray[0];
                    Preferences.debug("FileTiff.openIFD: Image_Length = " + yDimTemp + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case BITS_PER_SAMPLE:
                    if (type != SHORT) {
                        throw new IOException("BITS_PER_SAMPLE has illegal type = " + type + "\n");
                    }

                    haveBitsPerSample = true;
                    if (count == 1) {
                        Preferences.debug("FileTiff.openIFD: BitsPerSample = " + valueArray[0] + "\n",
                        		Preferences.DEBUG_FILEIO);

                        switch (valueArray[0]) {

                            case 8:
                                dataTypeTemp = ModelStorageBase.UBYTE;
                                break;

                            case 16:
                                dataTypeTemp = ModelStorageBase.USHORT;
                                break;

                            case 32:
                                dataTypeTemp = ModelStorageBase.FLOAT;
                                break;

                            default:
                                throw new IOException("TIFF Tag BitsPerSample has illegal value = " + valueArray[0]);
                        }
                    } else if ((count == 2) || (count == 3)) {
                        Preferences.debug("FileTiff.openIFD: BitsPerSample\n", Preferences.DEBUG_FILEIO);

                        for (i1 = 0; i1 < count; i1++) {
                            Preferences.debug("Value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n", Preferences.DEBUG_FILEIO);
                        }

                        if (valueArray[0] == 8) {
                            dataTypeTemp = ModelStorageBase.ARGB;
                        } else if (valueArray[0] == 16) {
                            dataTypeTemp = ModelStorageBase.ARGB_USHORT;
                        } else if (valueArray[0] == 32) {
                            dataTypeTemp = ModelStorageBase.ARGB_FLOAT;
                        }
                    } else {
                        Preferences.debug("FileTiff.openIFD: BitsPerSample\n", Preferences.DEBUG_FILEIO);

                        for (i1 = 0; i1 < count; i1++) {
                            Preferences.debug("Value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n", Preferences.DEBUG_FILEIO);
                        }

                        if (valueArray[0] == 8) {
                            dataTypeTemp = ModelStorageBase.UBYTE;
                        } else if (valueArray[0] == 16) {
                            dataTypeTemp = ModelStorageBase.USHORT;
                        } else if (valueArray[0] == 32) {
                            dataTypeTemp = ModelStorageBase.FLOAT;
                        }

                        manySpectrums = true;
                        spectrumNumber = count;
                        Preferences.debug("BITS_PER_SAMPLE shows " + spectrumNumber + " spectrums\n",
                        		Preferences.DEBUG_FILEIO);
                    }

                    break;

                case ROWS_PER_STRIP:

                    // System.err.println("found rows per strip");
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("ROWS_PER-STRIP has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("ROWS_PER_STRIP has illegal count = " + count + "\n");
                    }

                    rowsPerStrip = valueArray[0];
                    Preferences.debug("ROWS_PER_STRIP = " + valueArray[0] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case STRIP_OFFSETS:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("STRIP_OFFSETS has illegal type = " + type + "\n");
                    }

                    haveOffset = true;
                    if (count == 1) {
                        Preferences.debug("FileTiff.openIFD: Strip_offset = " + valueArray[0] + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        offsetCountTemp = 1;
                        offsetTemp[0] = valueArray[0] & 0XFFFFFFFFL;
                    } else if (count > 1) {
                        Preferences.debug("FileTiff.openIFD: Strip_offset\n", Preferences.DEBUG_FILEIO);
                        offsetCountTemp = count;

                        for (i1 = 0; i1 < count; i1++) {

                            // System.err.println("Strip byte count: " + valueArray[i1]);
                            Preferences.debug("Value[" + (i1 + 1) + "] = " + (valueArray[i1] & 0xFFFFFFFFL) + "\n",
                            		Preferences.DEBUG_FILEIO);
                            offsetTemp[i1] = valueArray[i1] & 0xFFFFFFFFL;
                        }
                    }

                    break;

                case STRIP_BYTE_COUNTS:

                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("STRIP_BYTE_COUNTS has illegal type = " + type + "\n");
                    }

                    haveByte = true;
                    if (count == 1) {
                        Preferences.debug("FileTiff.openIFD: Strip byte counts = " + valueArray[0] + "\n",
                        		Preferences.DEBUG_FILEIO);
                        byteCountTemp = 1;
                        byteTemp[0] = valueArray[0];
                    } else if (count > 1) {
                        Preferences.debug("FileTiff.openIFD. Strip byte counts\n", Preferences.DEBUG_FILEIO);
                        byteCountTemp = count;

                        for (i1 = 0; i1 < count; i1++) {
                            Preferences.debug("Value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n",
                            		Preferences.DEBUG_FILEIO);
                            byteTemp[i1] = valueArray[i1];
                        }
                    }

                    break;

                case PHOTO_INTERP:
                    if (type != SHORT) {
                        throw new IOException("PHOTO_INTERP has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("PHOTO_INTERP has illegal count = " + count + "\n");
                    }

                    if (valueArray[0] > 4) {
                        throw new IOException("PHOTO_INTERP has illegal value = " + valueArray[0] + "\n");
                    }

                    Preferences.debug("FileTiff.openIFD: PhotoInterp= " + valueArray[0] + "\n", Preferences.DEBUG_FILEIO);
                    if (valueArray[0] == 1) { // Black is zero
                        fileInfo.setPhotometric((short) 1);
                        Preferences.debug("FileTiff.openIFD: PhotoInterp = Black is zero\n" +
                                          "For bilevel and grayscale images\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 0) { // white is zero
                        fileInfo.setPhotometric((short) 0);
                        Preferences.debug("FileTiff.openIFD: PhotoInterp = White is zero\n" +
                                          "For bilevel and grayscale images\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 2) { // Color RGB
                        fileInfo.setPhotometric((short) 2);
                        Preferences.debug("FileTiff.openIFD: PhotoInterp = RGB\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 3) { // Color Indexed
                        fileInfo.setPhotometric((short) 3);
                        Preferences.debug("FileTiff.openIFD: PhotoInterp = Palette color\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 4) { // Transparency Mask
                        fileInfo.setPhotometric((short) 4);
                        Preferences.debug("FileTiff.openIFD: PhotoInterp = Transparency Mask\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case SAMPLES_PER_PIXEL:
                    if (type != SHORT) {
                        throw new IOException("SAMPLES_PER_PIXEL has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("SAMPLES_PER_PIXEL has illegal count = " + count + "\n");
                    }

                    haveSamples = true;
                    Preferences.debug("FileTiff.openIFD: samplesPerPixel = " + valueArray[0] + "\n",
                    		Preferences.DEBUG_FILEIO);

                    samplesPerPixelTemp = valueArray[0];
                    break;

                case PLANAR_CONFIG:
                    if (type != SHORT) {
                        throw new IOException("PLANAR_CONFIG has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("PLANAR_CONFIG has illegal count = " + count + "\n");
                    }

                    if ((valueArray[0] != 1) && (valueArray[0] != 2)) {
                        throw new IOException("PLANAR_CONFIG has illegal value = " + valueArray[0] + "\n");
                    }

                    havePlanar = true;
                    if (valueArray[0] == 1) { // can be black and white or color
                        chunkyTemp = true;
                        Preferences.debug("FileTiff.openIFD: planar config = chunky \n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 2) { // Color RGB
                        chunkyTemp = false;
                        Preferences.debug("FileTiff.openIFD: planar config = RRRRR, GGGG, BBBB. \n",
                        		Preferences.DEBUG_FILEIO);
                    }

                    break;

                case COMPRESSION:
                    if (type != SHORT) {
                        throw new IOException("COMPRESSION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("COMPRESSION has illegal count = " + count + "\n");
                    }

                    if ((valueArray[0] != 1) && (valueArray[0] != 2) && (valueArray[0] != 5) &&
                            (valueArray[0] != 32773)) {
                        Preferences.debug("COMPRESSION has illegal value = " + valueArray[0] + "\n",
                        		Preferences.DEBUG_FILEIO);
                        throw new IOException("COMPRESSION has illegal value = " + valueArray[0] + "\n");
                    }

                    if (valueArray[0] == 2) {
                        Preferences.debug("Modified Huffman run length encoding is not supported\n", 
                        		Preferences.DEBUG_FILEIO);
                        throw new IOException("Modified Huffman run length encoding is not supported\n");
                    } else if (valueArray[0] == 1) {
                        packBit = false;
                        Preferences.debug("FileTiff.openIFD: compression = no compression\n ", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 32773) {
                        packBit = true;
                        Preferences.debug("FileTiff.openIFD: compression = packed bit\n ", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 5) {
                        packBit = false;
                        lzwCompression = true;
                        Preferences.debug("FileTiff.openIFD: compression = LZW\n ", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case COLOR_MAP:
                    if (type != SHORT) {
                        throw new IOException("COLOR_MAP has illegal type = " + type + "\n");
                    }

                    if ((count % 6) != 0) {
                        throw new IOException("COLOR_MAP has illegal count = " + count + "\n");
                    }

                    // Already read LUT -  only same LUT in every file of multiFile and only one
                    // LUT for a multiImage file for now.
                    if ((count == 768) && (LUT == null)) {
                        int[] extents = new int[2];
                        extents[0] = 4;
                        extents[1] = 256;

                        float valueMax = valueArray[0];

                        for (i1 = 1; i1 < count; i1++) {

                            if (valueArray[i1] > valueMax) {
                                valueMax = valueArray[i1];
                            }
                        }

                        if (valueMax > 255.0) {
                            float scale = 255.0f / valueMax;

                            for (i1 = 0; i1 < count; i1++) {
                                valueArray[i1] *= scale;
                            }
                        }

                        LUT = new ModelLUT(ModelLUT.GRAY, 256, extents);

                        for (i1 = 0; i1 < 256; i1++) {
                            LUT.set(0, i1, 1.0f);
                            LUT.set(1, i1, valueArray[i1]);
                            LUT.set(2, i1, valueArray[i1 + 256]);
                            LUT.set(3, i1, valueArray[i1 + 512]);
                        }

                        LUT.makeIndexedLUT(null);
                    }

                    break;

                case RESOLUTION_UNIT:
                    if (type != SHORT) {
                        throw new IOException("RESOLUTION_UNIT has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("RESOLUTION_UNIT has illegal count = " + count + "\n");
                    } else if ((valueArray[0] < 1) || (valueArray[0] > 17)) {
                        throw new IOException("RESOLUTION_UNIT has illegal value = " + valueArray[0] + "\n");
                    }

                    if (valueArray[0] == Unit.MILLIMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = MILLIMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.UNKNOWN_MEASURE.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = UNKNOWN\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.INCHES.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.INCHES.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.INCHES.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = INCHES\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MILS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MILS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MILS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = MILS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.CENTIMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.CENTIMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.CENTIMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = CENTIMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.ANGSTROMS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.ANGSTROMS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.ANGSTROMS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = ANGSTROMS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.NANOMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = NANOMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MICROMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = MICROMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.METERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.METERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.METERS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = METERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.KILOMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.KILOMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.KILOMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = KILOMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MILES.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MILES.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MILES.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = MILES\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.NANOSEC.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.NANOSEC.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.NANOSEC.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = NANOSEC\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MICROSEC.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MICROSEC.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MICROSEC.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = MICROSEC\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MILLISEC.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = MILLISEC\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.SECONDS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.SECONDS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.SECONDS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = SECONDS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MINUTES.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MINUTES.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MINUTES.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = MINUTES\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.HOURS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.HOURS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.HOURS.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = HOURS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.HZ.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.HZ.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.HZ.getLegacyNum(), 1);
                        Preferences.debug("FileTiff.openIFD: Resolution Unit = HERTZ\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case XRESOLUTION:
                    if (type != RATIONAL) {
                        throw new IOException("XRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("XRESOLUTION has illegal count = " + count + "\n");
                    }

                    numerator = valueArray[0];
                    denominator = valueArray[1];
                    imgResols[0] = (float) numerator / denominator;
                    imgResols[0] = 1 / imgResols[0];
                    Preferences.debug("FileTiff.openIFD: X Resolution = " + imgResols[0] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case YRESOLUTION:
                    if (type != RATIONAL) {
                        throw new IOException("YRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("YRESOLUTION has illegal count = " + count + "\n");
                    }

                    numerator = valueArray[0];
                    denominator = valueArray[1];
                    imgResols[1] = (float) numerator / denominator;
                    imgResols[1] = 1 / imgResols[1];
                    Preferences.debug("FileTiff.openIFD: Y Resolution = " + imgResols[1] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case ZRESOLUTION:
                    if (type != DOUBLE) {
                        throw new IOException("ZRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("ZRESOLUTION has illegal count = " + count + "\n");
                    }

                    imgResols[2] = (float) valueDouble;
                    Preferences.debug("FileTiff.openIFD: Z Resolution = " + imgResols[2] + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
                    break;

                case TRESOLUTION:
                    if (type != DOUBLE) {
                        throw new IOException("TRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("TRESOLUTION has illegal count = " + count + "\n");
                    }

                    imgResols[3] = (float) valueDouble;
                    Preferences.debug("FileTiff.openIFD: T Resolution = " + imgResols[3] + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 3);
                    break;

                case TILE_WIDTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("TILE_WIDTH has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("TILE_WIDTH has illegal count = " + count + "\n");
                    }

                    doTile = true;
                    tileWidth = valueArray[0];
                    Preferences.debug("FileTiff.openIFD: tileWidth = " + tileWidth + "\n", Preferences.DEBUG_FILEIO);
                    tilesAcross = (xDim + tileWidth - 1) / tileWidth;
                    Preferences.debug("FileTiff.openIFD: tilesAcross = " + tilesAcross + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case TILE_LENGTH:

                    // System.err.println("TILE LENGTH");
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("TILE_LENGTH has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("TILE_LENGTH has illegal count = " + count + "\n");
                    }

                    tileLength = valueArray[0];
                    Preferences.debug("FileTiff.openIFD: tileLength = " + tileLength + "\n", Preferences.DEBUG_FILEIO);
                    tilesDown = (yDim + tileLength - 1) / tileLength;
                    Preferences.debug("FileTiff.openIFD: tilesDown = " + tilesDown + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case TILE_OFFSETS:
                    if (type != LONG) {
                        throw new IOException("TILE_OFFSETS has illegal type = " + type + "\n");
                    }

                    if (chunky) {
                        tilesPerImage = count;
                    } else {
                        tilesPerImage = count / samplesPerPixel;
                    }

                    Preferences.debug("FileTiff.openIFD: tilesPerImage = " + tilesPerImage + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("FileTiff.openIFD: tileOffsets are above\n", Preferences.DEBUG_FILEIO);
                    if (tileOffsetNumber == 0) {
                        tileOffsets = new long[count];

                        for (i1 = 0; i1 < count; i1++) {
                            tileOffsets[i1] = valueArray[i1] & 0XFFFFFFFFL;
                        }

                        tileOffsetNumber = count;
                    } // if (tileOffsetNumber == 0)
                    else {
                        tileTemp = new long[tileOffsetNumber];

                        for (i1 = 0; i1 < tileOffsetNumber; i1++) {
                            tileTemp[i1] = tileOffsets[i1];
                        }

                        tileOffsets = new long[tileOffsetNumber + count];

                        for (i1 = 0; i1 < tileOffsetNumber; i1++) {
                            tileOffsets[i1] = tileTemp[i1];
                        }

                        for (i1 = 0; i1 < count; i1++) {
                            tileOffsets[i1 + count] = valueArray[i1] & 0XFFFFFFFFL;
                        }

                        tileOffsetNumber += count;
                    } // else for tileOffsetNumber != 0

                    break;

                case TILE_BYTE_COUNTS:

                    // System.err.println("Got tile byte count");
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("TILE_BYTE_COUNTS has illegal type = " + type + "\n");
                    }

                    if (chunky) {

                        if (tilesPerImage != count) {
                            throw new IOException("Count fields do not agree in TILE_OFFSETS and TILE_BYTE_COUNTS");
                        }
                    } else {

                        if ((tilesPerImage * samplesPerPixel) != count) {
                            throw new IOException("Count fields do not agree in TILE_OFFSETS and TILE_BYTE_COUNTS");
                        }
                    }

                    Preferences.debug("FileTiff.openIFD: tileByteCounts are above\n", Preferences.DEBUG_FILEIO);
                    if (tileByteNumber == 0) {
                        tileByteCounts = new int[count];

                        for (i1 = 0; i1 < count; i1++) {
                            tileByteCounts[i1] = valueArray[i1];
                        }

                        tileByteNumber = count;
                    } else {
                        tileTemp = new long[tileByteNumber];

                        for (i1 = 0; i1 < tileByteNumber; i1++) {
                            tileTemp[i1] = tileByteCounts[i1];
                        }

                        tileByteCounts = new int[tileByteNumber + count];

                        for (i1 = 0; i1 < tileByteNumber; i1++) {
                            tileByteCounts[i1] = (int)tileTemp[i1];
                        }

                        for (i1 = 0; i1 < count; i1++) {
                            tileByteCounts[i1 + count] = valueArray[i1];
                        }

                        tileByteNumber += count;
                    } // else for tileByteNumber != 0

                    break;

                case IMAGE_DESCRIPTION:
                    if (type != ASCII) {
                        throw new IOException("IMAGE_DESCRIPTION has illegal type = " + type + "\n");
                    }

                    imageDescription = new byte[count];
                    for (i1 = 0; i1 < count; i1++) {
                        imageDescription[i1] = (byte) valueArray[i1];
                    }

                    str = new String(imageDescription);
                    fileInfo.setImageDescription(str);
                    Preferences.debug("FileTiff.openIFD: imageDescription = " + str + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case MIN_SAMPLE_VALUE:
                    if (type != SHORT) {
                        throw new IOException("MIN_SAMPLE_VALUE has illegal type = " + type + "\n");
                    }

                    for (i1 = 0; i1 < count; i1++) {
                        Preferences.debug("FileTiff.openIFD: minSampleValue[" + i1 + "] = " + valueArray[i1] + "\n",
                        		Preferences.DEBUG_FILEIO);
                    }

                    break;

                case MAX_SAMPLE_VALUE:
                    if (type != SHORT) {
                        throw new IOException("MAX_SAMPLE_VALUE has illegal type = " + type + "\n");
                    }

                    for (i1 = 0; i1 < count; i1++) {
                        Preferences.debug("FileTiff.openIFD: maxSampleValue[" + i1 + "] = " + valueArray[i1] + "\n",
                        		Preferences.DEBUG_FILEIO);
                    }

                    break;

                case SOFTWARE:
                    if (type != ASCII) {
                        throw new IOException("SOFTWARE has illegal type = " + type + "\n");
                    }

                    software = new byte[count];
                    for (i1 = 0; i1 < count; i1++) {
                        software[i1] = (byte) valueArray[i1];
                    }

                    str = new String(software);
                    Preferences.debug("FileTiff.openIFD: software = " + str + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case DATE_TIME:
                    if (type != ASCII) {
                        throw new IOException("DATE_TIME has illegal type = " + type + "\n");
                    }

                    if (count != 20) {
                        throw new IOException("DATE_TIME has illegal count = " + count + "\n");
                    }

                    dateTime = new byte[20];
                    for (i1 = 0; i1 < count; i1++) {
                        dateTime[i1] = (byte) valueArray[i1];
                    }

                    str = new String(dateTime);
                    Preferences.debug("FileTiff.openIFD: dateTime = " + str + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SAMPLE_FORMAT:
                    if (type != SHORT) {
                        throw new IOException("SAMPLE_FORMAT has illegal type = " + type + "\n");
                    }

                    for (i1 = 0; i1 < count; i1++) {
                        Preferences.debug("FileTiff.openIFD: sampleFormat[ " + i1 + "] = " + valueArray[i1] + "\n",
                        		Preferences.DEBUG_FILEIO);

                        if (valueArray[i1] == 1) {
                            Preferences.debug("FileTiff.openIFD: unsigned integer data\n", Preferences.DEBUG_FILEIO);
                        } else if (valueArray[i1] == 2) {
                            Preferences.debug("FileTiff.openIFD: two's complement signed integer data\n",
                            		Preferences.DEBUG_FILEIO);
                        } else if (valueArray[i1] == 3) {
                            Preferences.debug("FileTiff.openIFD: IEEE floating point data\n", Preferences.DEBUG_FILEIO);
                        } else if (valueArray[i1] == 4) {
                            Preferences.debug("FileTiff.openIFD: undefined data format\n", Preferences.DEBUG_FILEIO);
                        }
                    }

                    break;

                case PREDICTOR:
                    if (type != SHORT) {
                        throw new IOException("PREDICTOR has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("PREDICTOR has illegal count = " + count + "\n");
                    }

                    if (valueArray[0] == 1) {
                        predictor = 1;
                        Preferences.debug("PREDICTOR = 1 for no prediction scheme used\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 2) {
                        predictor = 2;
                        Preferences.debug("PREDICTOR = 2 for horizontal differencing\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("PREDICTOR = " + valueArray[0] + ", an illegal value", Preferences.DEBUG_FILEIO);
                    }

                    break;

                default:
                    break;
            }
        }

        if (!thumbNail) {

            // Ignore thumbnail images
            if (haveXDim) {
                xDim = xDimTemp;
            }

            if (haveYDim) {
                yDim = yDimTemp;
            }

            if (haveOffset) {
                dataOffsets[imageSlice] = new Vector<Index>();

                for (i1 = 0; i1 < offsetCountTemp; i1++) {
                    dataOffsets[imageSlice].addElement(new Index(offsetTemp[i1]));
                }
            } // if (haveOffset)

            if (haveByte) {

                for (i1 = 0; i1 < byteCountTemp; i1++) {
                    ((Index) (dataOffsets[imageSlice].elementAt(i1))).byteCount = byteTemp[i1];
                }
            } // if (haveByte)

            if (haveSamples) {
                samplesPerPixel = samplesPerPixelTemp;
            }

            if (havePlanar) {
                chunky = chunkyTemp;
            }

            if (haveBitsPerSample) {
                fileInfo.setDataType(dataTypeTemp);
            }

            imageSlice++;
        } // if (!thumbNail)
        else { // thumbnail image
            thumbnailOffset = offsetTemp[0];
            Preferences.debug("thumbnail offset: " + thumbnailOffset + "\n", Preferences.DEBUG_FILEIO);
        }

        IFDoffsets[imageSlice] = getInt(endianess);
        Preferences.debug("\nFileLSM.openIFD: Ref. to next imageSlice = " + IFDoffsets[imageSlice] + "\n",
        		Preferences.DEBUG_FILEIO);

        if (IFDoffsets[imageSlice] == 0) {
            return false; // Done reading images
        }

        return true; // Read more IFDs (ie. images)
    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   offset into the file stored in the dataOffset array
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readBuffer(int slice, float[] buffer) throws IOException {
        int i, j, a;
        int iCount, iNext;
        int b1, b2, b3, b4;
        int tmpInt;
        long progress, progressLength, mod;

        // long pointer;
        int idx = 0;
        int nBytes;
        int nLength;
        int planarRGB = 0; // Use this for planar RGB where you must read a stripsPerImage

        // number of red strips, followed by a stripsPerImage number of
        // green strips, followed by a stripsPerImage number of blue strips.

        int nIndex = dataOffsets[slice].size();
        int stripsPerImage = (nIndex + 1) / 3; // used for planar RGB

        // The below line is used appears in FileTiff, but cannot be used in FileLSM because in 2
        // channel data 1 of the 3 strips has 0 bytes.
        // if (nBytes == 0)  {nBytes = buffer.length; };
        i = 0;

        for (a = 0; a < nIndex; a++, idx++) {

            try {
                raFile.seek(((Index) (dataOffsets[slice].elementAt(idx))).index);
                nBytes = ((Index) (dataOffsets[slice].elementAt(idx))).byteCount;

                switch (fileInfo.getDataType()) {

                    case ModelStorageBase.BOOLEAN:
                        nLength = 8 * ((buffer.length + 63) >> 6); // new BitSet(size) = new long[(size+63)>>6];
                        byteBuffer = new byte[nLength];
                        raFile.read(byteBuffer, 0, nLength);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j++, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[i] = byteBuffer[j >> 3] & (1 << (7-(j % 8)));
                        }

                        break;

                    case ModelStorageBase.BYTE:
                        if (packBit == false) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[buffer.length];
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 10;


                            for (j = 0; j < nBytes; j++, i++) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = byteBuffer[j];
                            }
                        } // if (packBit == false)
                        else if (packBit == true) {
                            byteBuffer = new byte[nBytes];
                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j] & 0x80) == 0) {
                                    iCount = byteBuffer[j] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j];
                                    }
                                } // end of if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j];
                                    }

                                    j++;
                                } // end of else for compressed data bytes
                            } // end of while (j < nBytes)
                        } // end of else if (packBit == true)

                        break;

                    case ModelStorageBase.UBYTE:
                        if (packBit == false) {

                            if (manySpectrums) {
                                byteBuffer = new byte[nBytes];
                            } else {

                                if (byteBuffer == null) {
                                    byteBuffer = new byte[buffer.length];
                                }
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 100;


                            for (j = 0; j < nBytes; j++, i++) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = byteBuffer[j] & 0xff;
                            }
                        } // if (packBit == false)
                        else if (packBit == true) {
                            byteBuffer = new byte[nBytes];
                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j] & 0x80) == 0) {
                                    iCount = byteBuffer[j] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j] & 0xff;
                                    }
                                } // end of if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j] & 0xff;
                                    }

                                    j++;
                                } // end of else for compressed data bytes
                            } // end of while (j < nBytes)
                        } // end of else if (packBit == true)

                        break;

                    case ModelStorageBase.SHORT:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[2 * buffer.length];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 2, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);

                            if (endianess) {
                                buffer[i] = (short) ((b1 << 8) + b2);
                            } else {
                                buffer[i] = (short) ((b2 << 8) + b1);
                            }

                        }

                        break;

                    case ModelStorageBase.USHORT:
                        if (manySpectrums) {
                            byteBuffer = new byte[nBytes];
                        } // if (manySpectrums)
                        else { // not manySectrums

                            if (byteBuffer == null) {
                                byteBuffer = new byte[2 * buffer.length];
                            }
                        } // else not manySpectrums

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 2, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);

                            if (endianess) {
                                buffer[i] = ((b1 << 8) + b2);
                            } else {
                                buffer[i] = ((b2 << 8) + b1);
                            }
                        }

                        break;

                    case ModelStorageBase.INTEGER:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[4 * buffer.length];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 4, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);

                            if (endianess) {
                                buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                            } else {
                                buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                            }
                        }

                        break;

                    case ModelStorageBase.FLOAT:
                        if (manySpectrums) {
                            byteBuffer = new byte[nBytes];
                        } else {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[4 * buffer.length];
                            }
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 4, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);

                            if (endianess) {
                                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                            } else {
                                tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                            }

                            buffer[i] = Float.intBitsToFloat(tmpInt);

                        } // for (j =0; j < nBytes; j+=4, i++ )

                        break;

                    case ModelStorageBase.ARGB:
                        if (chunky == true) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[buffer.length];
                            }

                            raFile.read(byteBuffer, 0, nBytes);

                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 10;


                            // For the moment I compress RGB images to unsigned bytes.
                            for (j = 0; j < nBytes; j += 3, i += 4) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = 255;
                                buffer[i + 1] = getUnsignedByte(byteBuffer, j + redOffset);
                                buffer[i + 2] = getUnsignedByte(byteBuffer, j + greenOffset);
                                buffer[i + 3] = getUnsignedByte(byteBuffer, j + blueOffset);
                            }
                        } // if (chunky == true)
                        else { // planar RGB configuration

                            if (planarRGB < stripsPerImage) {

                                if (byteBuffer == null) {
                                    byteBuffer = new byte[buffer.length];
                                }

                                raFile.read(byteBuffer, 0, nBytes);

                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                // For the moment I compress RGB images to unsigned bytes
                                for (j = 0; j < nBytes; j++, i += 4) {

                                    if ((((i / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) + progress) /
                                                                                progressLength * 100));
                                    }

                                    buffer[i] = 255;

                                    if (redOffset == 0) {
                                        buffer[i + 1] = getUnsignedByte(byteBuffer, j);
                                    } else if (greenOffset == 0) {
                                        buffer[i + 2] = getUnsignedByte(byteBuffer, j);
                                    } else {
                                        buffer[i + 3] = getUnsignedByte(byteBuffer, j);
                                    }
                                }

                                planarRGB++;

                                if (planarRGB == stripsPerImage) {
                                    i = 0;
                                }
                            } // end of if (planarRGB < stripsPerImage)
                            else if (planarRGB < (2 * stripsPerImage)) {
                                raFile.read(byteBuffer, 0, nBytes);

                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j++, i += 4) {

                                    if ((((i / 3) + (buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) + (buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    if (greenOffset == 1) {
                                        buffer[i + 2] = getUnsignedByte(byteBuffer, j);
                                    } else if (redOffset == 1) {
                                        buffer[i + 1] = getUnsignedByte(byteBuffer, j);
                                    } else {
                                        buffer[i + 3] = getUnsignedByte(byteBuffer, j);
                                    }
                                }

                                planarRGB++;

                                if (planarRGB == (2 * stripsPerImage)) {
                                    i = 0;
                                }
                            } // end of else if (planarRGB < 2*stripsPerImage)
                            else { // planarRGB >= 2*stripsPerImage
                                raFile.read(byteBuffer, 0, nBytes);

                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j++, i += 4) {

                                    if ((((i / 3) + (2 * buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) +
                                                                                     (2 * buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    if (blueOffset == 2) {
                                        buffer[i + 3] = getUnsignedByte(byteBuffer, j);
                                    } else if (redOffset == 2) {
                                        buffer[i + 1] = getUnsignedByte(byteBuffer, j);
                                    } else {
                                        buffer[i + 2] = getUnsignedByte(byteBuffer, j);
                                    }
                                }

                                planarRGB++;
                            } // end of else for planarRGB >= 2*StripsPerImage
                        } // end of else for planar RGB configuration

                        break;

                    case ModelStorageBase.ARGB_USHORT:
                        if (chunky == true) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[2 * buffer.length];
                            }

                            raFile.read(byteBuffer, 0, nBytes);

                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 10;


                            // For the moment I compress RGB images to unsigned bytes.
                            for (j = 0; j < nBytes; j += 6, i += 4) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = 65535;
                                b1 = getUnsignedByte(byteBuffer, j + (2 * redOffset));
                                b2 = getUnsignedByte(byteBuffer, j + (2 * redOffset) + 1);

                                if (endianess) {
                                    buffer[i + 1] = ((b1 << 8) + b2);
                                } else {
                                    buffer[i + 1] = ((b2 << 8) + b1);
                                }

                                b1 = getUnsignedByte(byteBuffer, j + (2 * greenOffset));
                                b2 = getUnsignedByte(byteBuffer, j + (2 * greenOffset) + 1);

                                if (endianess) {
                                    buffer[i + 2] = ((b1 << 8) + b2);
                                } else {
                                    buffer[i + 2] = ((b2 << 8) + b1);
                                }

                                b1 = getUnsignedByte(byteBuffer, j + (2 * blueOffset));
                                b2 = getUnsignedByte(byteBuffer, j + (2 * blueOffset) + 1);

                                if (endianess) {
                                    buffer[i + 3] = ((b1 << 8) + b2);
                                } else {
                                    buffer[i + 3] = ((b2 << 8) + b1);
                                }

                            }
                        } // if (chunky == true)
                        else { // planar RGB configuration

                            if (planarRGB < stripsPerImage) {

                                if (byteBuffer == null) {
                                    byteBuffer = new byte[2 * buffer.length];
                                }

                                raFile.read(byteBuffer, 0, nBytes);

                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                // For the moment I compress RGB images to unsigned bytes
                                for (j = 0; j < nBytes; j += 2, i += 4) {

                                    if ((((i / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) + progress) /
                                                                                progressLength * 100));
                                    }

                                    buffer[i] = 65535;
                                    b1 = getUnsignedByte(byteBuffer, j);
                                    b2 = getUnsignedByte(byteBuffer, j + 1);

                                    if (redOffset == 0) {

                                        if (endianess) {
                                            buffer[i + 1] = ((b1 << 8) + b2);
                                        } else {
                                            buffer[i + 1] = ((b2 << 8) + b1);
                                        }
                                    } // if (redOffset == 0)
                                    else if (greenOffset == 0) {

                                        if (endianess) {
                                            buffer[i + 2] = ((b1 << 8) + b2);
                                        } else {
                                            buffer[i + 2] = ((b2 << 8) + b1);
                                        }
                                    } // else if (greenOffset == 0)
                                    else { // blueOffset == 0

                                        if (endianess) {
                                            buffer[i + 3] = ((b1 << 8) + b2);
                                        } else {
                                            buffer[i + 3] = ((b2 << 8) + b1);
                                        }
                                    } // else blueOffset == 0
                                }

                                planarRGB++;

                                if (planarRGB == stripsPerImage) {
                                    i = 0;
                                }
                            } // end of if (planarRGB < stripsPerImage)
                            else if (planarRGB < (2 * stripsPerImage)) {
                                raFile.read(byteBuffer, 0, nBytes);

                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j += 2, i += 4) {

                                    if ((((i / 3) + (buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) + (buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    b1 = getUnsignedByte(byteBuffer, j);
                                    b2 = getUnsignedByte(byteBuffer, j + 1);

                                    if (greenOffset == 1) {

                                        if (endianess) {
                                            buffer[i + 2] = ((b1 << 8) + b2);
                                        } else {
                                            buffer[i + 2] = ((b2 << 8) + b1);
                                        }
                                    } // if (greenOffset == 1)
                                    else if (redOffset == 1) {

                                        if (endianess) {
                                            buffer[i + 1] = ((b1 << 8) + b2);
                                        } else {
                                            buffer[i + 1] = ((b2 << 8) + b1);
                                        }
                                    } // else if (redOffset == 1)
                                    else { // blueOffset == 1

                                        if (endianess) {
                                            buffer[i + 3] = ((b1 << 8) + b2);
                                        } else {
                                            buffer[i + 3] = ((b2 << 8) + b1);
                                        }

                                    } // else blueOffset == 1
                                }

                                planarRGB++;

                                if (planarRGB == (2 * stripsPerImage)) {
                                    i = 0;
                                }
                            } // end of else if (planarRGB < 2*stripsPerImage)
                            else { // planarRGB >= 2*stripsPerImage
                                raFile.read(byteBuffer, 0, nBytes);

                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j += 2, i += 4) {

                                    if ((((i / 3) + (2 * buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) +
                                                                                     (2 * buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    b1 = getUnsignedByte(byteBuffer, j);
                                    b2 = getUnsignedByte(byteBuffer, j + 1);

                                    if (blueOffset == 2) {

                                        if (endianess) {
                                            buffer[i + 3] = ((b1 << 8) + b2);
                                        } else {
                                            buffer[i + 3] = ((b2 << 8) + b1);
                                        }
                                    } // if (blueOffset == 2)
                                    else if (redOffset == 2) {

                                        if (endianess) {
                                            buffer[i + 1] = ((b1 << 8) + b2);
                                        } else {
                                            buffer[i + 1] = ((b2 << 8) + b1);
                                        }
                                    } // else if (redOffset == 2)
                                    else { // greenOffset == 2

                                        if (endianess) {
                                            buffer[i + 2] = ((b1 << 8) + b2);
                                        } else {
                                            buffer[i + 2] = ((b2 << 8) + b1);
                                        }
                                    } // else greenOffset == 2
                                }

                                planarRGB++;
                            } // end of else for planarRGB >= 2*StripsPerImage
                        } // end of else for planar RGB configuration

                        break;
                }
            } catch (OutOfMemoryError error) {
                System.gc();
                throw error;
            }
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void readCZPrivateTag() throws IOException {
        int magicNumber;
        int structureSize;
        int timeStampBytes;
        int timeStampNumber = 0;
        boolean haveTimeStamps = false;
        @SuppressWarnings("unused")
        int allEventsBytes;
        int thisEventBytes;
        int eventNumber;
        byte tempByte;
        byte[] description;
        int channelColorsBytes;
        int numberNames;
        int colorsOffset;
        int namesOffset;
        int colorValue;
        int i, j;
        long startCZ;
        int startPos, endPos;
        boolean found;

        startCZ = raFile.getFilePointer();
        magicNumber = getInt(endianess);

        if (magicNumber == 0x00300494C) {
            Preferences.debug("Magic number corresponds to version 1.3\n", Preferences.DEBUG_FILEIO);
        } else if (magicNumber == 0x00400494C) {
            Preferences.debug("Magic number corresponds to version 1.5 to 6.0\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("Illegal magic number = " + magicNumber + "\n", Preferences.DEBUG_FILEIO);
            throw new IOException("Illegal magic number = " + magicNumber);
        }

        structureSize = getInt(endianess);
        Preferences.debug("The number of bytes in the CZ private tag is = " + structureSize + "\n", Preferences.DEBUG_FILEIO);
        czDimX = getInt(endianess);
        Preferences.debug("Intensity values in x direction = " + czDimX + "\n", Preferences.DEBUG_FILEIO);
        czDimY = getInt(endianess);
        Preferences.debug("Intensity values in y direction = " + czDimY + "\n", Preferences.DEBUG_FILEIO);
        czDimZ = getInt(endianess);
        Preferences.debug("Intensity values in z direction = " + czDimZ + "\n", Preferences.DEBUG_FILEIO);
        czChannels = getInt(endianess);
        Preferences.debug("Number of channels = " + czChannels + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setChannels(czChannels);
        czDimT = getInt(endianess);
        Preferences.debug("Intensity values in time direction = " + czDimT + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setTimeDim(czDimT);
        czDataType = getInt(endianess);

        if (czDataType == 0) {
            Preferences.debug("Different CZ bit numbers for different channels\n", Preferences.DEBUG_FILEIO);
        } else if (czDataType == 1) {
            Preferences.debug("CZ data is 8-bit unsigned integer\n", Preferences.DEBUG_FILEIO);
        } else if (czDataType == 2) {
            Preferences.debug("CZ data is 12-bit unsigned integer\n", Preferences.DEBUG_FILEIO);
        } else if (czDataType == 5) {
            Preferences.debug("CZ data is 32 bit float\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("Illegal CZ data type = " + czDataType + "\n", Preferences.DEBUG_FILEIO);
        }

        fileInfo.setLSMDataType(czDataType);
        thumbNailX = getInt(endianess);
        Preferences.debug("Width in pixels of a thumbnail = " + thumbNailX + "\n", Preferences.DEBUG_FILEIO);
        thumbNailY = getInt(endianess);
        Preferences.debug("Height in pixels of a thumbnail = " + thumbNailY + "\n", Preferences.DEBUG_FILEIO);
        voxelSizeX = getDouble(endianess);

        // Change from meters to micrometers
        imgResols[0] = (float) (1.0E6 * voxelSizeX);
        Preferences.debug("Voxel Size X in micrometers = " + imgResols[0] + "\n", Preferences.DEBUG_FILEIO);
        voxelSizeY = getDouble(endianess);
        imgResols[1] = (float) (1.0E6 * voxelSizeY);
        Preferences.debug("Voxel Size Y in micrometers = " + imgResols[1] + "\n", Preferences.DEBUG_FILEIO);
        voxelSizeZ = getDouble(endianess);

        if (czDimZ > 1) {
            imgResols[2] = (float) (1.0E6 * voxelSizeZ);
            Preferences.debug("Voxel Size Z in micrometers = " + imgResols[2] + "\n", Preferences.DEBUG_FILEIO);
        }
        
        originX = getDouble(endianess);
        Preferences.debug("The x-offset of the center of the image in meters relative to the optical axis = " + originX + "\n",
                          Preferences.DEBUG_FILEIO);
        fileInfo.setOriginX(originX);
        
        originY = getDouble(endianess);
        Preferences.debug("The y-offset of the center of the image in meters relative to the optical axis = " + originY + "\n",
                          Preferences.DEBUG_FILEIO);
        fileInfo.setOriginY(originY);

        raFile.seek(startCZ + 88L);
        czScanType = getUnsignedShort(endianess);

        if (czScanType == 0) {
            Preferences.debug("Normal x-y-z-scan\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 1) {
            Preferences.debug("Z-Scan (x-z-plane)\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 2) {
            Preferences.debug("Line scan\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 3) {
            Preferences.debug("Time series x-y\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 4) {
            Preferences.debug("Time series x-z\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 5) {
            Preferences.debug("Time series - Mean of ROIs\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 6) {
            Preferences.debug("Time series x-y-z\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 7) {
            Preferences.debug("Spline scan\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 8) {
            Preferences.debug("Spline plane x-z\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 9) {
            Preferences.debug("Time series spline plane x-z\n", Preferences.DEBUG_FILEIO);
        } else if (czScanType == 10) {
            Preferences.debug("Point mode\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("Illegal scan type = " + czScanType + "\n", Preferences.DEBUG_FILEIO);
        }

        fileInfo.setScanType(czScanType);

        czSpectralScan = getUnsignedShort(endianess);

        if (czSpectralScan == 0) {
            Preferences.debug("No spectral scan\n", Preferences.DEBUG_FILEIO);
        } else if (czSpectralScan == 1) {
            Preferences.debug("Image has been acquired in spectral scan mode with a ", Preferences.DEBUG_FILEIO);
            Preferences.debug("LSM 510 META or LSM 710 QUASAR detector\n", Preferences.DEBUG_FILEIO);
        }

        fileInfo.setSpectralScan(czSpectralScan);

        czDataType2 = getInt(endianess);

        if (czDataType2 == 0) {
            Preferences.debug("Scan data\n", Preferences.DEBUG_FILEIO);
        } else if (czDataType2 == 1) {
            Preferences.debug("Calculated data\n", Preferences.DEBUG_FILEIO);
        } else if (czDataType2 == 2) {
            Preferences.debug("3D reconstruction\n", Preferences.DEBUG_FILEIO);
        } else if (czDataType2 == 3) {
            Preferences.debug("Topography height map\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("Data type2 has illegal value = " + czDataType2 + "\n", Preferences.DEBUG_FILEIO);
        }

        fileInfo.setLSMDataType2(czDataType2);

        offsetVectorOverlay = getInt(endianess);

        if (offsetVectorOverlay != 0) {
            Preferences.debug("Vector overlay is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetVectorOverlay);
            drawingElement = VECTOR_OVERLAY;
            readDrawingElement();
            raFile.seek(saveLocus);
        }

        offsetInputLut = getInt(endianess);

        if (offsetInputLut != 0) {
            Preferences.debug("Input LUT is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetInputLut);
            lutKind = INPUT_LUT;
            readLut();
            raFile.seek(saveLocus);
        }

        offsetOutputLut = getInt(endianess);

        if (offsetOutputLut != 0) {
            Preferences.debug("Output LUT is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetOutputLut);
            lutKind = OUTPUT_LUT;
            readLut();
            raFile.seek(saveLocus);
        }

        offsetChannelColors = getInt(endianess);

        if (offsetChannelColors != 0) {
            Preferences.debug("List of channel colors and names is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetChannelColors);
            channelColorsBytes = getInt(endianess);
            numberColors = getInt(endianess);

            if (numberColors != czChannels) {
                Preferences.debug("Number of channels = " + czChannels + ", but number of colors = " + numberColors +
                                  "\n", Preferences.DEBUG_FILEIO);
            }

            numberNames = getInt(endianess);

            if (numberNames != czChannels) {
                Preferences.debug("Number of channels = " + czChannels + ", but number of names = " + numberNames +
                                  "\n", Preferences.DEBUG_FILEIO);
            }

            colorsOffset = getInt(endianess);

            long saveLocus2 = raFile.getFilePointer();
            raFile.seek(offsetChannelColors + colorsOffset);
            redArray = new int[numberColors];
            greenArray = new int[numberColors];
            blueArray = new int[numberColors];

            for (i = 0; i < numberColors; i++) {
                colorValue = getInt(endianess);
                redArray[i] = 0x000000ff & colorValue;
                greenArray[i] = 0x000000ff & (colorValue >> 8);
                blueArray[i] = 0x000000ff & (colorValue >> 16);
                Preferences.debug("Channel # " + i + " red = " + redArray[i] + " green = " + greenArray[i] +
                                  " blue = " + blueArray[i] + "\n", Preferences.DEBUG_FILEIO);

                if (i == 0) {

                    if ((greenArray[0] > 0) && (redArray[0] == 0) && (blueArray[0] == 0)) {
                        greenOffset = 0;
                        redOffset = -1;
                    } else if ((blueArray[0] > 0) && (redArray[0] == 0) && (greenArray[0] == 0)) {
                        blueOffset = 0;
                        redOffset = -1;
                    }
                } // if (i == 0)
                else if (i == 1) {

                    if ((redArray[1] > 0) && (greenArray[1] == 0) && (blueArray[1] == 0)) {
                        redOffset = 1;

                        if (greenOffset != 0) {
                            greenOffset = -1;
                        }
                    } else if ((blueArray[1] > 0) && (redArray[1] == 0) && (greenArray[1] == 0)) {
                        blueOffset = 1;

                        if (greenOffset != 0) {
                            greenOffset = -1;
                        }
                    }
                } // else if (i == 1)
                else if (i == 2) {

                    if ((redArray[2] > 0) && (greenArray[2] == 0) && (blueArray[2] == 0)) {
                        redOffset = 2;
                    }

                    if ((greenArray[2] > 0) && (redArray[2] == 0) && (blueArray[2] == 0)) {
                        greenOffset = 2;
                    }
                } // else if (i == 2)
            }

            fileInfo.setRedArray(redArray);
            fileInfo.setGreenArray(greenArray);
            fileInfo.setBlueArray(blueArray);
            raFile.seek(saveLocus2);
            namesOffset = getInt(endianess);

            long saveLocus3 = raFile.getFilePointer();
            raFile.seek(offsetChannelColors + namesOffset);
            description = new byte[channelColorsBytes - namesOffset];
            channelNames = new String[numberNames];

            for (i = 0; i < description.length; i++) {
                description[i] = raFile.readByte();
            }

            startPos = 4;
            endPos = 5;

            for (i = 0; i < numberNames; i++) {
                found = false;

                for (j = startPos; (j < description.length) && (!found); j++) {

                    if (description[j] == 0) {
                        endPos = j;
                        found = true;
                    }
                }

                channelNames[i] = new String(description, startPos, endPos - startPos);

                if (channelNames[i] != null) {
                    Preferences.debug("Channel # " + i + " Name = " + channelNames[i] + "\n", Preferences.DEBUG_FILEIO);
                }

                startPos = endPos + 5;
            }

            fileInfo.setChannelNames(channelNames);
            raFile.seek(saveLocus3);
            mono = getInt(endianess);

            if (mono == 0) {
                Preferences.debug("The mono button was not pressed\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("The mono button was pressed\n", Preferences.DEBUG_FILEIO);
            }

            fileInfo.setMono(mono);
            raFile.seek(saveLocus);
        }

        timeInterval = getDouble(endianess);
        Preferences.debug("Time interval for time series = " + timeInterval + "\n", Preferences.DEBUG_FILEIO);

        if (timeInterval != 0.0) {

            if (czDimZ == 1) {
                imgResols[2] = (float) timeInterval;
            } else {
                imgResols[3] = (float) timeInterval;
            }

            if (czDimT > 1) {
                fileInfo.setTimeInterval(timeInterval);
            }
        }

        offsetChannelDataTypes = getInt(endianess);

        if (offsetChannelDataTypes != 0) {
            Preferences.debug("Channel data types array is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetChannelDataTypes);
            channelDataTypes = new int[czChannels];

            for (i = 0; i < czChannels; i++) {
                channelDataTypes[i] = getInt(endianess);

                if (channelDataTypes[i] == 1) {
                    Preferences.debug("Channel # " + i + " has 8-bit unsigned integers\n", Preferences.DEBUG_FILEIO);
                } else if (channelDataTypes[i] == 2) {
                    Preferences.debug("Channel # " + i + " has 12-bit unsigned integers\n", Preferences.DEBUG_FILEIO);
                } else if (channelDataTypes[i] == 5) {
                    Preferences.debug("Channel # " + i + " has 32-bit floats\n", Preferences.DEBUG_FILEIO);
                } else {
                    Preferences.debug("Channel # " + i + " has illegal data type = " + channelDataTypes[i] + "\n",
                    		Preferences.DEBUG_FILEIO);
                    throw new IOException("Channel # " + i + " has illegal data type");
                }
            } // for (i = 0; i < czChannels; i++)

            fileInfo.setChannelDataTypes(channelDataTypes);
            raFile.seek(saveLocus);
        } // if (offsetChannelDataTypes != 0)

        offsetScanInformation = getInt(endianess);

        if (offsetScanInformation != 0) {
            Preferences.debug("Information of the device settings used to scan the image is present\n", 
            		Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetScanInformation);
            readScanInformation();
            raFile.seek(saveLocus);
        }

        offsetKsData = getInt(endianess);

        if (offsetKsData != 0) {
            Preferences.debug("Zeiss Vision Ks-3d specific data is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetKsData);
            readKsData();
            raFile.seek(saveLocus);
        }

        offsetTimeStamps = getInt(endianess);

        if (offsetTimeStamps != 0) {
            Preferences.debug("Time stamp structure is present\n", Preferences.DEBUG_FILEIO);
            haveTimeStamps = true;

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetTimeStamps);
            timeStampBytes = getInt(endianess);
            Preferences.debug("Number of bytes of in time stamp block = " + timeStampBytes + "\n", Preferences.DEBUG_FILEIO);
            timeStampNumber = getInt(endianess);
            Preferences.debug("Time stamp number = " + timeStampNumber + "\n", Preferences.DEBUG_FILEIO);
            timeStamp = new double[timeStampNumber];

            for (i = 0; i < timeStampNumber; i++) {
                timeStamp[i] = getDouble(endianess);
                Preferences.debug("Time stamp # " + (i + 1) + " = " + timeStamp[i] + "\n", Preferences.DEBUG_FILEIO);
            }

            fileInfo.setTimeStamp(timeStamp);
            raFile.seek(saveLocus);
        } // if (offsetTimeStamps != 0)

        offsetEventList = getInt(endianess);

        if (offsetEventList != 0) {
            Preferences.debug("Event list structure is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetEventList);
            allEventsBytes = getInt(endianess);
            eventNumber = getInt(endianess);
            eventTime = new double[eventNumber];
            eventType = new int[eventNumber];
            description = new byte[1000];
            eventDescription = new String[eventNumber];

            for (i = 0; i < eventNumber; i++) {
                long eventStart = raFile.getFilePointer();
                thisEventBytes = getInt(endianess);
                Preferences.debug("Event # " + (i + 1) + " has " + thisEventBytes + " bytes\n", Preferences.DEBUG_FILEIO);
                eventTime[i] = getDouble(endianess);
                eventType[i] = getInt(endianess);
                tempByte = 1;

                for (j = 0; j < description.length; j++) {
                    description[j] = 0;
                }

                j = 0;

                while (tempByte != 0) {
                    tempByte = raFile.readByte();
                    description[j++] = tempByte;
                }

                eventDescription[i] = new String(description, 0, j - 1);
                Preferences.debug("Event # " + (i + 1) + " time = " + eventTime[i] + "\n", Preferences.DEBUG_FILEIO);

                if (eventType[i] == 0) {
                    Preferences.debug("Event # " + (i + 1) + " type = experimental annotation\n", Preferences.DEBUG_FILEIO);
                } else if (eventType[i] == 1) {
                    Preferences.debug("Event # " + (i + 1) + " type = time interval has changed\n", Preferences.DEBUG_FILEIO);
                } else if (eventType[i] == 2) {
                    Preferences.debug("Event # " + (i + 1) + " type = start of a bleach operation\n", Preferences.DEBUG_FILEIO);

                    // Find last slice before start of bleach operation
                    if (haveTimeStamps) {
                        found = false;

                        for (j = 0; (j < timeStampNumber) && (!found); j++) {

                            if (timeStamp[j] > eventTime[i]) {
                                lastSliceBeforeBleach = j - 1;
                                fileInfo.setLastSliceBeforeBleach(lastSliceBeforeBleach);
                                found = true;
                            }
                        }
                    } // if (haveTimeStamps)
                } else if (eventType[i] == 3) {
                    Preferences.debug("Event # " + (i + 1) + " type = end of a bleach operation\n", Preferences.DEBUG_FILEIO);

                    // Find first slice after end of bleach operation
                    if (haveTimeStamps) {
                        found = false;

                        for (j = timeStampNumber - 1; (j >= 0) && (!found); j--) {

                            if (timeStamp[j] < eventTime[i]) {
                                firstSliceAfterBleach = j + 1;
                                fileInfo.setFirstSliceAfterBleach(firstSliceAfterBleach);
                                found = true;
                            }
                        }
                    } // if (haveTimeStamps)
                } else if (eventType[i] == 4) {
                    Preferences.debug("Event # " + (i + 1) + " type = trigger signal was detected\n", Preferences.DEBUG_FILEIO);
                } else {
                    Preferences.debug("Event # " + (i + 1) + " illegal type = " + eventType[i] + "\n", Preferences.DEBUG_FILEIO);
                    throw new IOException("Event # " + (i + 1) + " has illegal event type");
                }

                if (eventDescription[i] != null) {
                    Preferences.debug("Event # " + (i + 1) + " description:\n" + eventDescription[i] + "\n",
                    		Preferences.DEBUG_FILEIO);
                }

                if (i < (eventNumber - 1)) {
                    raFile.seek(eventStart + thisEventBytes);
                }
            }

            fileInfo.setEventTime(eventTime);
            fileInfo.setEventType(eventType);
            fileInfo.setEventDescription(eventDescription);
            raFile.seek(saveLocus);
        } // if (offsetEventList != 0)

        offsetRoi = getInt(endianess);

        if (offsetRoi != 0) {
            Preferences.debug("List of ROIs is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetRoi);
            drawingElement = ROI;
            readDrawingElement();
            raFile.seek(saveLocus);
        }

        offsetBleachRoi = getInt(endianess);

        if (offsetBleachRoi != 0) {
            Preferences.debug("Description of the bleach region is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetBleachRoi);
            drawingElement = BLEACH_ROI;
            readDrawingElement();
            raFile.seek(saveLocus);
        }

        secondImage = getInt(endianess);

        if (secondImage != 0) {
            Preferences.debug("A second image is stored in this file\n", Preferences.DEBUG_FILEIO);
        }

        displayAspectX = getDouble(endianess);
        Preferences.debug("Zoom factor for image display in the x-direction = " + displayAspectX + "\n",
        		Preferences.DEBUG_FILEIO);

        if (displayAspectX > 0.0) {
            fileInfo.setDisplayAspectX(displayAspectX);
        }

        displayAspectY = getDouble(endianess);
        Preferences.debug("Zoom factor for image display in the y-direction = " + displayAspectY + "\n",
        		Preferences.DEBUG_FILEIO);

        if (displayAspectY > 0.0) {
            fileInfo.setDisplayAspectY(displayAspectY);
        }

        displayAspectZ = getDouble(endianess);
        Preferences.debug("Zoom factor for image display in the z-direction = " + displayAspectZ + "\n", 
        		Preferences.DEBUG_FILEIO);

        if (displayAspectZ > 0.0) {
            fileInfo.setDisplayAspectZ(displayAspectZ);
        }

        displayAspectTime = getDouble(endianess);
        Preferences.debug("Zoom factor for image display in the time-direction = " + displayAspectTime + "\n",
        		Preferences.DEBUG_FILEIO);

        if (displayAspectTime > 0.0) {
            fileInfo.setDisplayAspectTime(displayAspectTime);
        }

        offsetMeanOfRoisOverlay = getInt(endianess);

        if (offsetMeanOfRoisOverlay != 0) {
            Preferences.debug("Description of the vector overlay with the ROIs used\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("during a scan in the Mean of ROIs mode is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetMeanOfRoisOverlay);
            drawingElement = MEANOFROIS_OVERLAY;
            readDrawingElement();
            raFile.seek(saveLocus);
        }

        offsetTopolsolineOverlay = getInt(endianess);

        if (offsetTopolsolineOverlay != 0) {
            Preferences.debug("Description of the vector overlay for the topography\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("-iso-lines and height display with the profile\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("selection line is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetTopolsolineOverlay);
            drawingElement = TOPOLSOLINE_OVERLAY;
            readDrawingElement();
            raFile.seek(saveLocus);
        }

        offsetTopoProfileOverlay = getInt(endianess);

        if (offsetTopoProfileOverlay != 0) {
            Preferences.debug("Description of the vector overlay for the topography\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("-profile display is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetTopoProfileOverlay);
            drawingElement = TOPOPROFILE_OVERLAY;
            readDrawingElement();
            raFile.seek(saveLocus);
        }

        offsetLinescanOverlay = getInt(endianess);

        if (offsetLinescanOverlay != 0) {
            Preferences.debug("Description of the vector overlay for the line\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("scan line selection with the selected line or\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("Bezier curve\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetLinescanOverlay);
            drawingElement = LINESCAN_OVERLAY;
            readDrawingElement();
            raFile.seek(saveLocus);
        }

        toolbarFlags = getInt(endianess);
        Preferences.debug("toolbarFlags = " + toolbarFlags + "\n", Preferences.DEBUG_FILEIO);

        offsetChannelWavelength = getInt(endianess);

        if (offsetChannelWavelength != 0) {
            Preferences.debug("Channel wavelength is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetChannelWavelength);
            wavelengthNumber = getInt(endianess);
            wavelengths = new double[2 * wavelengthNumber];

            for (i = 0; i < wavelengthNumber; i++) {
                wavelengths[2 * i] = getDouble(endianess);
                wavelengths[(2 * i) + 1] = getDouble(endianess);
                Preferences.debug("Start wavelength[" + (i + 1) + "] = " + wavelengths[2 * i] + "\n",
                		Preferences.DEBUG_FILEIO);
                Preferences.debug("End wavelength[" + (i + 1) + "] = " + wavelengths[(2 * i) + 1] + "\n",
                		Preferences.DEBUG_FILEIO);
            }

            if ((wavelengthNumber > 0) && (wavelengths[0] > 0.0)) {
                fileInfo.setWavelengths(wavelengths);
            }

            raFile.seek(saveLocus);
        }

        offsetChannelFactors = getInt(endianess);

        if (offsetChannelFactors != 0) {
            Preferences.debug("Offset to memory block with scaling factor, offset,\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("and unit for each image channel is present\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetChannelFactors);
            readOffsetChannelFactors();
            raFile.seek(saveLocus);
        }

        objectiveSphereCorrection = getDouble(endianess);
        Preferences.debug("Objective sphere correction = " + objectiveSphereCorrection + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setObjectiveSphereCorrection(objectiveSphereCorrection);

        offsetUnmixParameters = getInt(endianess);

        if (offsetUnmixParameters != 0) {
            Preferences.debug("Offset to the parameters for linear unmixing\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("that have been used to generate the image data\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("from scan data of the spectral detector\n", Preferences.DEBUG_FILEIO);

            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetUnmixParameters);
            readOffsetUnmixParameters();
            raFile.seek(saveLocus);
        }
        
        offsetAcquisitionParameters = getInt(endianess);
        
        if (offsetAcquisitionParameters != 0) {
            Preferences.debug("Offset to a block with acquisition parameters for\n", Preferences.DEBUG_FILEIO);  
            Preferences.debug("support of the re-use function of the LSM 5/7 program\n", Preferences.DEBUG_FILEIO);
            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetAcquisitionParameters);
            readOffsetAcquisitionParameters();
            raFile.seek(saveLocus);
        }
        
        offsetCharacteristics = getInt(endianess);
        
        if (offsetCharacteristics != 0) {
            Preferences.debug("Offset to a block with user specified properties\n", Preferences.DEBUG_FILEIO);
            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetCharacteristics);
            readOffsetCharacteristics();
            raFile.seek(saveLocus);
        }
        
        offsetPalette = getInt(endianess);
        
        if (offsetPalette != 0) {
            Preferences.debug("Offset to a block with detailed color palette properties\n", Preferences.DEBUG_FILEIO);
            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetPalette);
            readOffsetPalette();
            raFile.seek(saveLocus);
        }
        
        timeDifferenceX = getDouble(endianess);
        Preferences.debug("The time difference for the acquisition of adjacent pixels in x-direction in seconds = " +
                          timeDifferenceX + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setTimeDifferenceX(timeDifferenceX);
        
        timeDifferenceY = getDouble(endianess);
        Preferences.debug("The time difference for the acquisition of adjacent pixels in y-direction in seconds = " +
                          timeDifferenceY + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setTimeDifferenceY(timeDifferenceY);
        
        timeDifferenceZ = getDouble(endianess);
        Preferences.debug("The time difference for the acquisition of adjacent pixels in z-direction in seconds = " +
                          timeDifferenceZ + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setTimeDifferenceZ(timeDifferenceZ);
        
        internalUse1 = getInt(endianess);
        Preferences.debug("Internal use 1 = " + internalUse1 + "\n", Preferences.DEBUG_FILEIO);
        
        dimensionP = getInt(endianess);
        Preferences.debug("Number of intensity values in position-direction = " + dimensionP + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setDimensionP(dimensionP);
        
        dimensionM = getInt(endianess);
        Preferences.debug("Number of intensity values in tile (mosaic) direction = " + dimensionM + "\n", 
                          Preferences.DEBUG_FILEIO);
        fileInfo.setDimensionM(dimensionM);
        
        for (i = 0; i < 16; i++) {
            dimensionsReserved[i] = getInt(endianess);
            if (dimensionsReserved[i] == 0) {
                Preferences.debug("dimensionReserved["+i+"] = 0 as expected\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("dimensionsReserved["+i+"] = " + dimensionsReserved[i] + " instead of the expected 0\n",
                                  Preferences.DEBUG_FILEIO);
            } 
        }
        
        offsetTilePositions = getInt(endianess);
        
        if (offsetTilePositions != 0) {
            Preferences.debug("Offset to a block with the positions of the tiles\n", Preferences.DEBUG_FILEIO);
            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetTilePositions);
            readOffsetTilePositions();
            raFile.seek(saveLocus);
        }
        
        for (i = 0; i < 9; i++) {
            reserved[i] = getInt(endianess);
            if (reserved[i] == 0) {
                Preferences.debug("reserved["+i+"] = 0 as expected\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("reserved["+i+"] = " + reserved[i] + " instead of the expected 0\n");
            }
        }
        
       offsetPositions = getInt(endianess);
        
        if (offsetPositions != 0) {
            Preferences.debug("Offset to a block with the positions of the acquisition regions\n", Preferences.DEBUG_FILEIO);
            long saveLocus = raFile.getFilePointer();
            raFile.seek(offsetPositions);
            readOffsetPositions();
            raFile.seek(saveLocus);
        }
        
    }


    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void readDrawingElement() throws IOException {
        int i, j;
        int numberDrawingElements;
        int blockSize;
        int lineWidth;
        int measure;
        int reserved1;
        int reserved2;
        int color;
        int redValue;
        int greenValue;
        int blueValue;
        int disabled;
        int knotWidth;
        int catchArea;
        int fontHeight;
        int fontWidth;
        int fontEscapement;
        int fontOrientation;
        int fontWeight;
        int fontItalic;
        int fontUnderline;
        int fontStrikeOut;
        int fontCharSet;
        int fontOutPrecision;
        int fontClipPrecision;
        int fontQuality;
        int fontPitchAndFamily;
        int fontPitch;
        int fontFamily;
        char[] fName = new char[32];
        String fontFaceName;
        int[] reserved7 = new int[7];
        int elementType;
        long drawingElementBlockStart;
        int drawingElementBlockSize;
        double startPointX;
        double startPointY;
        int valid;
        int enabled;
        int notMoveable;
        int[] reserved8 = new int[8];
        int numberKnots;
        double pointOriginX;
        double pointOriginY;
        char[] cText;
        String sText;
        long currentPos;
        int remainingBlockBytes;
        int numChars;
        byte[] measureFlags = new byte[10];
        numberDrawingElements = getInt(endianess);

        String drawString = null;
        String drawString2 = null;

        switch (drawingElement) {

            case VECTOR_OVERLAY:
                drawString = "Vector overlay ";
                drawString2 = " vector overlay ";
                break;

            case ROI:
                drawString = "Scan ROI ";
                drawString2 = " scan ROI ";
                break;

            case BLEACH_ROI:
                drawString = "Bleach ROI ";
                drawString2 = " bleach ROI ";
                break;

            case MEANOFROIS_OVERLAY:
                drawString = "Mean ROI ";
                drawString2 = " mean ROI ";
                break;

            case TOPOLSOLINE_OVERLAY:
                drawString = "Profile selection line ";
                drawString2 = " profile selection line ";
                break;

            case TOPOPROFILE_OVERLAY:
                drawString = "Topography overlay ";
                drawString2 = " topography overlay ";
                break;

            case LINESCAN_OVERLAY:
                drawString = "Line scan overlay ";
                drawString2 = " line scan overlay ";
                break;
        }

        if (numberDrawingElements == 1) {
            Preferences.debug(drawString + "has " + numberDrawingElements + " drawing element in the list\n",
            		Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug(drawString + "has " + numberDrawingElements + " drawing elements in the list\n", 
            		Preferences.DEBUG_FILEIO);
        }

        blockSize = getInt(endianess);
        Preferences.debug(drawString + "block size is " + blockSize + " bytes\n", Preferences.DEBUG_FILEIO);
        lineWidth = getInt(endianess);
        Preferences.debug("The most recently used line width was " + lineWidth + "\n", Preferences.DEBUG_FILEIO);
        measure = getInt(endianess);

        if (measure == 0) {
            Preferences.debug("The measure button was not checked in\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("The measure button was checked in\n", Preferences.DEBUG_FILEIO);
        }

        reserved1 = getInt(endianess);

        if (reserved1 == 0) {
            Preferences.debug("reserved1 == 0 as expected\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("reserved1 == " + reserved1 + " instead of the expected 0\n", Preferences.DEBUG_FILEIO);
        }

        reserved2 = getInt(endianess);

        if (reserved2 == 0) {
            Preferences.debug("reserved2 == 0 as expected\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("reserved2 == " + reserved1 + " instead of the expected 0\n", Preferences.DEBUG_FILEIO);
        }

        color = getInt(endianess);
        redValue = color & 0x000000ff;
        greenValue = (color >> 8) & 0x000000ff;
        blueValue = (color >> 16) & 0x000000ff;
        Preferences.debug("The most recently used color had red = " + redValue + " green = " + greenValue + " blue = " +
                          blueValue + "\n", Preferences.DEBUG_FILEIO);
        disabled = getInt(endianess);

        if (disabled == 0) {
            Preferences.debug("The overlay was not enabled in the editor\n", Preferences.DEBUG_FILEIO);
        } else if (disabled == 1) {
            Preferences.debug("The overlay was enabled in the editor\n", Preferences.DEBUG_FILEIO);
        }

        knotWidth = getInt(endianess);
        Preferences.debug("knotWidth = " + knotWidth + "\n", Preferences.DEBUG_FILEIO);
        catchArea = getInt(endianess);
        Preferences.debug("The size of the mouse catch area was " + catchArea + "\n", Preferences.DEBUG_FILEIO);
        fontHeight = getInt(endianess);
        Preferences.debug("The font height in logical units was " + fontHeight + "\n", Preferences.DEBUG_FILEIO);
        fontWidth = getInt(endianess);

        if (fontWidth == 0) {
            Preferences.debug("The default font width was used\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("The font width in logical units was " + fontWidth + "\n", Preferences.DEBUG_FILEIO);
        }

        fontEscapement = getInt(endianess);
        Preferences.debug("Angle of each text line = " + (fontEscapement / 10.0f) + "\n", Preferences.DEBUG_FILEIO);
        fontOrientation = getInt(endianess);
        Preferences.debug("Angle of each character's base line = " + (fontOrientation / 10.0f) + "\n",
        		Preferences.DEBUG_FILEIO);
        fontWeight = getInt(endianess);

        if (fontWeight == 0) {
            Preferences.debug("The default font weight was used\n", Preferences.DEBUG_FILEIO);
        } else if (fontWeight == 400) {
            Preferences.debug("The font weight was 400, the value for normal\n", Preferences.DEBUG_FILEIO);
        } else if (fontWeight == 700) {
            Preferences.debug("The font weight was 700, the value for bold\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("The font weight was = " + fontWeight + "\n", Preferences.DEBUG_FILEIO);
        }

        fontItalic = getInt(endianess);

        if (fontItalic == 0) {
            Preferences.debug("An italic font was not specified\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("An italic font was specified\n", Preferences.DEBUG_FILEIO);
        }

        fontUnderline = getInt(endianess);

        if (fontUnderline == 0) {
            Preferences.debug("An underlined font was not specified\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("An underlined font was specified\n", Preferences.DEBUG_FILEIO);
        }

        fontStrikeOut = getInt(endianess);

        if (fontStrikeOut == 0) {
            Preferences.debug("A strikeout font was not specified\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("A strikeout font was specified\n", Preferences.DEBUG_FILEIO);
        }

        fontCharSet = getInt(endianess);

        if (fontCharSet == 0) {
            Preferences.debug("FontCharSet = 0 as expected\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("FontCharSet = " + fontCharSet + " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
        }

        fontOutPrecision = getInt(endianess);

        if (fontOutPrecision == 0) {
            Preferences.debug("FontOutPrecision = 0 as expected\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("FontOutPrecision = " + fontOutPrecision + " instead of the expected zero\n",
            		Preferences.DEBUG_FILEIO);
        }

        fontClipPrecision = getInt(endianess);

        if (fontClipPrecision == 0) {
            Preferences.debug("FontClipPrecision = 0 as expected\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("FontClipPrecision = " + fontClipPrecision + " instead of the expected zero\n",
            		Preferences.DEBUG_FILEIO);
        }

        fontQuality = getInt(endianess);

        if (fontQuality == 0) {
            Preferences.debug("FontQuality = 0 as expected\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("FontQuality = " + fontQuality + " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
        }

        fontPitchAndFamily = getInt(endianess);
        fontPitch = fontPitchAndFamily & 0x00000003;

        if (fontPitch == 0) {
            Preferences.debug("The system default pitch was used\n", Preferences.DEBUG_FILEIO);
        } else if (fontPitch == 1) {
            Preferences.debug("A fixed pitch was used\n", Preferences.DEBUG_FILEIO);
        } else if (fontPitch == 2) {
            Preferences.debug("A variable pitch was used\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("FontPitch = 3\n", Preferences.DEBUG_FILEIO);
        }

        fontFamily = (0x000000f0 & fontPitchAndFamily) >> 4;

        if (fontFamily == 0) {
            Preferences.debug("Font family is a don't care\n", Preferences.DEBUG_FILEIO);
        } else if (fontFamily == 1) {
            Preferences.debug("Font family is a variable stroke width, serified\n", Preferences.DEBUG_FILEIO);
        } else if (fontFamily == 2) {
            Preferences.debug("Font family was a variable stroke width, " + "sans-serified\n", Preferences.DEBUG_FILEIO);
        } else if (fontFamily == 3) {
            Preferences.debug("Font family was a constant stroke width, " + "serified or sans-serified\n", Preferences.DEBUG_FILEIO);
        } else if (fontFamily == 4) {
            Preferences.debug("Font family was cursive\n", Preferences.DEBUG_FILEIO);
        } else if (fontFamily == 5) {
            Preferences.debug("Font family was old English\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("Font family = " + fontFamily + "\n", Preferences.DEBUG_FILEIO);
        }

        for (i = 0; i < 32; i++) {
            fName[i] = (char) getUnsignedShort(endianess);
        }

        fontFaceName = new String(fName);
        fontFaceName = fontFaceName.trim();
        Preferences.debug("Font face name = " + fontFaceName + "\n", Preferences.DEBUG_FILEIO);
        raFile.read(measureFlags);

        for (i = 0; i < 10; i++) {

            if (measureFlags[i] != 0) {

                switch (i) {

                    case 0:
                        Preferences.debug("Closed polyline has flags for ", Preferences.DEBUG_FILEIO);
                        break;

                    case 1:
                        Preferences.debug("Open polyline has flags for ", Preferences.DEBUG_FILEIO);
                        break;

                    case 2:
                        Preferences.debug("Closed Bezier curve has flags for ", Preferences.DEBUG_FILEIO);
                        break;

                    case 3:
                        Preferences.debug("Open Bezier curve has flags for ", Preferences.DEBUG_FILEIO);
                        break;

                    case 4:
                        Preferences.debug("Arrow with closed tip has flags for ", Preferences.DEBUG_FILEIO);
                        break;

                    case 5:
                        Preferences.debug("Arrow with open tip has flags for ", Preferences.DEBUG_FILEIO);
                        break;

                    case 6:
                        Preferences.debug("Ellipse has flags for ", Preferences.DEBUG_FILEIO);
                        break;

                    case 7:
                        Preferences.debug("Circle has flags for ", Preferences.DEBUG_FILEIO);
                        break;

                    case 8:
                        Preferences.debug("Rectangle has flags for ", Preferences.DEBUG_FILEIO);
                        break;

                    case 9:
                        Preferences.debug("Line has flags for ", Preferences.DEBUG_FILEIO);
                        break;
                }

                if ((measureFlags[i] & 0x02) != 0) {
                    Preferences.debug("circumference ", Preferences.DEBUG_FILEIO);
                }

                if ((measureFlags[i] & 0x04) != 0) {
                    Preferences.debug("area ", Preferences.DEBUG_FILEIO);
                }

                if ((measureFlags[i] & 0x08) != 0) {
                    Preferences.debug("radius ", Preferences.DEBUG_FILEIO);
                }

                if ((measureFlags[i] & 0x10) != 0) {
                    Preferences.debug("angle ", Preferences.DEBUG_FILEIO);
                }

                if ((measureFlags[i] & 0x20) != 0) {
                    Preferences.debug("distance x ", Preferences.DEBUG_FILEIO);
                }

                if ((measureFlags[i] & 0x40) != 0) {
                    Preferences.debug("distance y ", Preferences.DEBUG_FILEIO);
                }

                Preferences.debug("\n", Preferences.DEBUG_FILEIO);
            } // if (measureFlags[i] != 0)
        } // for (i = 0; i < 10; i++)

        // Read 2 bytes so that 10 measureFlag bytes + 2 bytes + 28 reserved bytes =
        // the 40 reserved bytes of version 2.0.
        getSignedShort(endianess);

        for (i = 0; i < 7; i++) {
            reserved7[i] = getInt(endianess);

            if (reserved7[i] == 0) {
                Preferences.debug("Reserved field " + (i + 1) + " of 7 is an expected zero\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("Reserved field " + (i + 1) + " of 7 = " + reserved7[i] +
                                  " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
            }
        }

        drawingElementBlockStart = raFile.getFilePointer();

        for (i = 0; i < numberDrawingElements; i++) {
            Preferences.debug("Reading information for" + drawString2 + (i + 1) + " of " + numberDrawingElements +
                              "\n", Preferences.DEBUG_FILEIO);
            elementType = getInt(endianess);

            switch (elementType) {

                case 13:
                    Preferences.debug(drawString + (i + 1) + " is text\n", Preferences.DEBUG_FILEIO);
                    break;

                case 14:
                    Preferences.debug(drawString + (i + 1) + " is a line\n", Preferences.DEBUG_FILEIO);
                    break;

                case 15:
                    Preferences.debug(drawString + (i + 1) + " is a horizontal or vertical " +
                                      "scale bar with displayed length\n", Preferences.DEBUG_FILEIO);
                    break;

                case 16:
                    Preferences.debug(drawString + (i + 1) + " is a line and a two line arrow\n", Preferences.DEBUG_FILEIO);
                    break;

                case 17:
                    Preferences.debug(drawString + (i + 1) + " is a line and a three line arrow\n", Preferences.DEBUG_FILEIO);
                    break;

                case 18:
                    Preferences.debug(drawString + (i + 1) + " is a rectangle\n", Preferences.DEBUG_FILEIO);
                    if (drawingElement == BLEACH_ROI) {
                        bleachedROIShape = RECTANGLE;
                        fileInfo.setBleachedROIShape(bleachedROIShape);
                    }

                    break;

                case 19:
                    Preferences.debug(drawString + (i + 1) + " is an ellipse\n", Preferences.DEBUG_FILEIO);
                    if (drawingElement == BLEACH_ROI) {
                        bleachedROIShape = ELLIPSE;
                        fileInfo.setBleachedROIShape(bleachedROIShape);
                    }

                    break;

                case 20:
                    Preferences.debug(drawString + (i + 1) + " is a closed polyline\n", Preferences.DEBUG_FILEIO);
                    if (drawingElement == BLEACH_ROI) {
                        bleachedROIShape = CLOSED_POLYLINE;
                        fileInfo.setBleachedROIShape(bleachedROIShape);
                    }

                    break;

                case 21:
                    Preferences.debug(drawString + (i + 1) + " is an open polyline\n", Preferences.DEBUG_FILEIO);
                    break;

                case 22:
                    Preferences.debug(drawString + (i + 1) + " is a closed bezier spline curve\n", Preferences.DEBUG_FILEIO);
                    if (drawingElement == BLEACH_ROI) {
                        bleachedROIShape = CLOSED_BEZIER;
                        fileInfo.setBleachedROIShape(bleachedROIShape);
                    }

                    break;

                case 23:
                    Preferences.debug(drawString + (i + 1) + " is an open bezier spline curve\n", Preferences.DEBUG_FILEIO);
                    break;

                case 24:
                    Preferences.debug(drawString + (i + 1) + " is a circle\n", Preferences.DEBUG_FILEIO);
                    if (drawingElement == BLEACH_ROI) {
                        bleachedROIShape = CIRCLE;
                        fileInfo.setBleachedROIShape(bleachedROIShape);
                    }

                    break;

                case 25:
                    Preferences.debug(drawString + (i + 1) + " is a rectangle with color palette\n", Preferences.DEBUG_FILEIO);
                    break;

                case 26:
                    Preferences.debug(drawString + (i + 1) + " is a open polyline with arrow tip\n", Preferences.DEBUG_FILEIO);
                    break;

                case 27:
                    Preferences.debug(drawString + (i + 1) + " is an open Bezier spline curve with arrow tip\n",
                    		Preferences.DEBUG_FILEIO);
                    break;

                case 28:
                    Preferences.debug(drawString + (i + 1) + " is two connected lines for angle measurement\n",
                    		Preferences.DEBUG_FILEIO);
                    break;

                case 29:
                    Preferences.debug(drawString + (i + 1) + " is a circle defined by 3 points on the perimeter\n",
                    		Preferences.DEBUG_FILEIO);
                    if (drawingElement == BLEACH_ROI) {
                        bleachedROIShape = CIRCLE;
                        fileInfo.setBleachedROIShape(bleachedROIShape);
                    }

                    break;

                default:
                    Preferences.debug(drawString + (i + 1) + " = " + elementType + "\n", Preferences.DEBUG_FILEIO);
            } // switch(elementType)

            drawingElementBlockSize = getInt(endianess);
            Preferences.debug(drawString + (i + 1) + " has a block size = " + drawingElementBlockSize + " bytes\n",
            		Preferences.DEBUG_FILEIO);
            drawingElementBlockStart = drawingElementBlockStart + drawingElementBlockSize;
            lineWidth = getInt(endianess);
            Preferences.debug(drawString + (i + 1) + " has line width = " + lineWidth + "\n", Preferences.DEBUG_FILEIO);
            measure = getInt(endianess);

            if (measure == 0) {
                Preferences.debug(drawString + (i + 1) + " has the display of " +
                                  "measured characteristics switched off\n", Preferences.DEBUG_FILEIO);
            } else if (measure == 1) {
                Preferences.debug(drawString + (i + 1) + " has the display of " +
                                  "default measured characteristics switched on\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " has the display of ", Preferences.DEBUG_FILEIO);

                if ((measure & 0x02) != 0) {
                    Preferences.debug("circumference ", Preferences.DEBUG_FILEIO);
                }

                if ((measure & 0x04) != 0) {
                    Preferences.debug("area ", Preferences.DEBUG_FILEIO);
                }

                if ((measure & 0x08) != 0) {
                    Preferences.debug("radius ", Preferences.DEBUG_FILEIO);
                }

                if ((measure & 0x10) != 0) {
                    Preferences.debug("angle ", Preferences.DEBUG_FILEIO);
                }

                if ((measure & 0x20) != 0) {
                    Preferences.debug("distance x ", Preferences.DEBUG_FILEIO);
                }

                if ((measure & 0x40) != 0) {
                    Preferences.debug("distance y ", Preferences.DEBUG_FILEIO);
                }

                Preferences.debug("\n", Preferences.DEBUG_FILEIO);
            }

            startPointX = getDouble(endianess);
            Preferences.debug(drawString + (i + 1) + " has a horizontal starting " + "point = " + startPointX + "\n",
            		Preferences.DEBUG_FILEIO);
            startPointY = getDouble(endianess);
            Preferences.debug(drawString + (i + 1) + " has a vertical starting " + "point = " + startPointY + "\n",
            		Preferences.DEBUG_FILEIO);
            color = getInt(endianess);
            redValue = color & 0x000000ff;
            greenValue = (color >> 8) & 0x000000ff;
            blueValue = (color >> 16) & 0x000000ff;
            Preferences.debug(drawString + (i + 1) + " had red = " + redValue + " green = " + greenValue + " blue = " +
                              blueValue + "\n", Preferences.DEBUG_FILEIO);
            valid = getInt(endianess);

            if (valid == 0) {
                Preferences.debug(drawString + (i + 1) + " editing was unexpectedly not completed\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " editing was completed as expected\n", Preferences.DEBUG_FILEIO);
            }

            knotWidth = getInt(endianess);
            Preferences.debug(drawString + (i + 1) + " knotWidth = " + knotWidth + "\n", Preferences.DEBUG_FILEIO);
            catchArea = getInt(endianess);
            Preferences.debug(drawString + (i + 1) + " The size of the mouse catch area was " + catchArea + "\n",
            		Preferences.DEBUG_FILEIO);
            fontHeight = getInt(endianess);
            Preferences.debug(drawString + (i + 1) + " The font height in logical units was " + fontHeight + "\n",
            		Preferences.DEBUG_FILEIO);
            fontWidth = getInt(endianess);

            if (fontWidth == 0) {
                Preferences.debug(drawString + (i + 1) + " The default font width was used\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " The font width in logical units was " + fontWidth + "\n",
                		Preferences.DEBUG_FILEIO);
            }

            fontEscapement = getInt(endianess);
            Preferences.debug(drawString + (i + 1) + " Angle of each text line = " + (fontEscapement / 10.0f) + "\n", 
            		Preferences.DEBUG_FILEIO);
            fontOrientation = getInt(endianess);
            Preferences.debug(drawString + (i + 1) + " Angle of each character's base line = " +
                              (fontOrientation / 10.0f) + "\n", Preferences.DEBUG_FILEIO);
            fontWeight = getInt(endianess);

            if (fontWeight == 0) {
                Preferences.debug(drawString + (i + 1) + " The default font weight was used\n", Preferences.DEBUG_FILEIO);
            } else if (fontWeight == 400) {
                Preferences.debug(drawString + (i + 1) + " The font weight was 400, the value for normal\n", Preferences.DEBUG_FILEIO);
            } else if (fontWeight == 700) {
                Preferences.debug(drawString + (i + 1) + " The font weight was 700, the value for bold\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " The font weight was = " + fontWeight + "\n", Preferences.DEBUG_FILEIO);
            }

            fontItalic = getInt(endianess);

            if (fontItalic == 0) {
                Preferences.debug(drawString + (i + 1) + " An italic font was not specified\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " An italic font was specified\n", Preferences.DEBUG_FILEIO);
            }

            fontUnderline = getInt(endianess);

            if (fontUnderline == 0) {
                Preferences.debug(drawString + (i + 1) + " An underlined font was not specified\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " An underlined font was specified\n", Preferences.DEBUG_FILEIO);
            }

            fontStrikeOut = getInt(endianess);

            if (fontStrikeOut == 0) {
                Preferences.debug(drawString + (i + 1) + " A strikeout font was not specified\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " A strikeout font was specified\n", Preferences.DEBUG_FILEIO);
            }

            fontCharSet = getInt(endianess);

            if (fontCharSet == 0) {
                Preferences.debug(drawString + (i + 1) + " FontCharSet = 0 as expected\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " FontCharSet = " + fontCharSet +
                                  " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
            }

            fontOutPrecision = getInt(endianess);

            if (fontOutPrecision == 0) {
                Preferences.debug(drawString + (i + 1) + " FontOutPrecision = 0 as expected\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " FontOutPrecision = " + fontOutPrecision +
                                  " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
            }

            fontClipPrecision = getInt(endianess);

            if (fontClipPrecision == 0) {
                Preferences.debug(drawString + (i + 1) + " FontClipPrecision = 0 as expected\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " FontClipPrecision = " + fontClipPrecision +
                                  " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
            }

            fontQuality = getInt(endianess);

            if (fontQuality == 0) {
                Preferences.debug(drawString + (i + 1) + " FontQuality = 0 as expected\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " FontQuality = " + fontQuality +
                                  " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
            }

            fontPitchAndFamily = getInt(endianess);
            fontPitch = fontPitchAndFamily & 0x00000003;

            if (fontPitch == 0) {
                Preferences.debug(drawString + (i + 1) + " The system default pitch was used\n", Preferences.DEBUG_FILEIO);
            } else if (fontPitch == 1) {
                Preferences.debug(drawString + (i + 1) + " A fixed pitch was used\n", Preferences.DEBUG_FILEIO);
            } else if (fontPitch == 2) {
                Preferences.debug(drawString + (i + 1) + " A variable pitch was used\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " FontPitch = 3\n", Preferences.DEBUG_FILEIO);
            }

            fontFamily = (0x000000f0 & fontPitchAndFamily) >> 4;

            if (fontFamily == 0) {
                Preferences.debug(drawString + (i + 1) + " Font family is a don't care\n", Preferences.DEBUG_FILEIO);
            } else if (fontFamily == 1) {
                Preferences.debug(drawString + (i + 1) + " Font family is a variable stroke width, serified\n",
                		Preferences.DEBUG_FILEIO);
            } else if (fontFamily == 2) {
                Preferences.debug(drawString + (i + 1) + " Font family was a variable stroke width, " +
                                  "sans-serified\n", Preferences.DEBUG_FILEIO);
            } else if (fontFamily == 3) {
                Preferences.debug(drawString + (i + 1) + " Font family was a constant stroke width, " +
                                  "serified or sans-serified\n", Preferences.DEBUG_FILEIO);
            } else if (fontFamily == 4) {
                Preferences.debug(drawString + (i + 1) + " Font family was cursive\n", Preferences.DEBUG_FILEIO);
            } else if (fontFamily == 5) {
                Preferences.debug(drawString + (i + 1) + " Font family was old English\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " Font family = " + fontFamily + "\n", Preferences.DEBUG_FILEIO);
            }

            for (j = 0; j < 32; j++) {
                fName[j] = (char) getUnsignedShort(endianess);
            }

            fontFaceName = new String(fName);
            fontFaceName = fontFaceName.trim();
            Preferences.debug(drawString + (i + 1) + " Font face name = " + fontFaceName + "\n", Preferences.DEBUG_FILEIO);
            enabled = getInt(endianess);

            if (enabled == 0) {
                Preferences.debug(drawString + (i + 1) + " is disabled\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(drawString + (i + 1) + " is enabled\n", Preferences.DEBUG_FILEIO);
            }

            notMoveable = getInt(endianess);

            if (notMoveable != 0) {
                Preferences.debug(drawString + (i + 1) + " cannot be moved\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("with the editor\n", Preferences.DEBUG_FILEIO);
            }

            for (j = 0; j < 8; j++) {
                reserved8[j] = getInt(endianess);

                if (reserved8[j] == 0) {
                    Preferences.debug("Reserved field " + (j + 1) + " of 8 is an expected zero\n", Preferences.DEBUG_FILEIO);
                } else {
                    Preferences.debug("Reserved field " + (j + 1) + " of 8 = " + reserved8[j] +
                                      " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
                }
            }

            switch (elementType) {

                case 13: // text
                    pointOriginX = getDouble(endianess);
                    Preferences.debug(drawString + "text " + (i + 1) + " has left text start = " + pointOriginX + "\n",
                    		Preferences.DEBUG_FILEIO);
                    pointOriginY = getDouble(endianess);
                    Preferences.debug(drawString + "text " + (i + 1) +
                                      " has upper border of text bounding rectangle at = " + pointOriginY + "\n", 
                                      Preferences.DEBUG_FILEIO);
                    currentPos = raFile.getFilePointer();
                    remainingBlockBytes = (int) (drawingElementBlockStart - currentPos);
                    numChars = remainingBlockBytes / 2;
                    cText = new char[numChars];
                    for (j = 0; j < numChars; j++) {
                        cText[j] = (char) getUnsignedShort(endianess);
                    }

                    sText = new String(cText);
                    sText = sText.trim();
                    Preferences.debug(drawString + "text " + (i + 1) + " = " + sText + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case 14: // line
                    numberKnots = getInt(endianess);
                    if (numberKnots == 2) {
                        Preferences.debug(drawString + "line " + (i + 1) + " has the required 2 knots\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "line " + (i + 1) + " has " + numberKnots +
                                          " knots instead of the required 2\n", Preferences.DEBUG_FILEIO);
                    }

                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    knotX[0] = getDouble(endianess);
                    Preferences.debug(drawString + "line " + (i + 1) + " has knot1X = " + knotX[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[0] = getDouble(endianess);
                    Preferences.debug(drawString + "line " + (i + 1) + " has knot1Y = " + knotY[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotX[1] = getDouble(endianess);
                    Preferences.debug(drawString + "line " + (i + 1) + " has knot2X = " + knotX[1] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[1] = getDouble(endianess);
                    Preferences.debug(drawString + "line " + (i + 1) + " has knot2Y = " + knotY[1] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case 15: // scale bar
                    numberKnots = getInt(endianess);
                    if (numberKnots == 2) {
                        Preferences.debug(drawString + "scale bar " + (i + 1) + " has the required 2 knots\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "scale bar " + (i + 1) + " has " + numberKnots +
                                          " knots instead of the required 2\n", Preferences.DEBUG_FILEIO);
                    }

                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    knotX[0] = getDouble(endianess);
                    Preferences.debug(drawString + "scale bar " + (i + 1) + " has knot1X = " + knotX[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[0] = getDouble(endianess);
                    Preferences.debug(drawString + "scale bar " + (i + 1) + " has knot1Y = " + knotY[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotX[1] = getDouble(endianess);
                    Preferences.debug(drawString + "scale bar " + (i + 1) + " has knot2X = " + knotX[1] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[1] = getDouble(endianess);
                    Preferences.debug(drawString + "scale bar " + (i + 1) + " has knot2Y = " + knotY[1] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case 16: // open arrow
                    numberKnots = getInt(endianess);
                    if (numberKnots == 2) {
                        Preferences.debug(drawString + "open arrow " + (i + 1) + " has the required 2 knots\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "open arrow " + (i + 1) + " has " + numberKnots +
                                          " knots instead of the required 2\n", Preferences.DEBUG_FILEIO);
                    }

                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    knotX[0] = getDouble(endianess);
                    Preferences.debug(drawString + "open arrow " + (i + 1) + " has knot1X = " + knotX[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[0] = getDouble(endianess);
                    Preferences.debug(drawString + "open arrow " + (i + 1) + " has knot1Y = " + knotY[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotX[1] = getDouble(endianess);
                    Preferences.debug(drawString + "open arrow " + (i + 1) + " has knot2X = " + knotX[1] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[1] = getDouble(endianess);
                    Preferences.debug(drawString + "open arrow " + (i + 1) + " has knot2Y = " + knotY[1] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case 17: // closed arrow
                    numberKnots = getInt(endianess);
                    if (numberKnots == 2) {
                        Preferences.debug(drawString + "closed arrow " + (i + 1) + " has the required 2 knots\n",
                        		Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "closed arrow " + (i + 1) + " has " + numberKnots +
                                          " knots instead of the required 2\n", Preferences.DEBUG_FILEIO);
                    }

                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    knotX[0] = getDouble(endianess);
                    Preferences.debug(drawString + "closed arrow " + (i + 1) + " has knot1X = " + knotX[0] + "\n",
                    		Preferences.DEBUG_FILEIO);
                    knotY[0] = getDouble(endianess);
                    Preferences.debug(drawString + "closed arrow " + (i + 1) + " has knot1Y = " + knotY[0] + "\n", 
                    		Preferences.DEBUG_FILEIO);
                    knotX[1] = getDouble(endianess);
                    Preferences.debug(drawString + "closed arrow " + (i + 1) + " has knot2X = " + knotX[1] + "\n",
                    		Preferences.DEBUG_FILEIO);
                    knotY[1] = getDouble(endianess);
                    Preferences.debug(drawString + "closed arrow " + (i + 1) + " has knot2Y = " + knotY[1] + "\n",
                    		Preferences.DEBUG_FILEIO);
                    break;

                case 18: // rectangle
                    numberKnots = getInt(endianess);
                    if (numberKnots == 2) {
                        Preferences.debug(drawString + "rectangle " + (i + 1) + " has the required 2 knots\n",
                        		Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "rectangle " + (i + 1) + " has " + numberKnots +
                                          " knots instead of the required 2\n", Preferences.DEBUG_FILEIO);
                    }

                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    knotX[0] = getDouble(endianess);
                    Preferences.debug(drawString + "rectangle " + (i + 1) + " has knot1X = " + knotX[0] + "\n",
                    		Preferences.DEBUG_FILEIO);
                    knotY[0] = getDouble(endianess);
                    Preferences.debug(drawString + "rectangle " + (i + 1) + " has knot1Y = " + knotY[0] + "\n",
                    		Preferences.DEBUG_FILEIO);
                    knotX[1] = getDouble(endianess);
                    Preferences.debug(drawString + "rectangle " + (i + 1) + " has knot2X = " + knotX[1] + "\n",
                    		Preferences.DEBUG_FILEIO);
                    knotY[1] = getDouble(endianess);
                    Preferences.debug(drawString + "rectangle " + (i + 1) + " has knot2Y = " + knotY[1] + "\n",
                    		Preferences.DEBUG_FILEIO);
                    if (drawingElement == BLEACH_ROI) {
                        bleachKnotX = new double[numberKnots];
                        bleachKnotY = new double[numberKnots];

                        for (j = 0; j < knotX.length; j++) {
                            bleachKnotX[j] = knotX[j];
                            bleachKnotY[j] = knotY[j];
                        }

                        fileInfo.setKnotX(bleachKnotX);
                        fileInfo.setKnotY(bleachKnotY);
                        haveBleachedRectangle = true;
                    } // if (drawingElement == BLEACH_ROI)

                    break;

                case 19: // ellipse
                    numberKnots = getInt(endianess);
                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    if (numberKnots == 4) {
                        Preferences.debug(drawString + "ellipse " + (i + 1) + " has the required 4 knots\n", 
                        		Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "ellipse " + (i + 1) + " has " + numberKnots +
                                          " knots instead of the required 4\n", Preferences.DEBUG_FILEIO);
                    }

                    for (j = 0; j < numberKnots; j++) {
                        knotX[j] = getDouble(endianess);
                        Preferences.debug(drawString + "ellipse " + (i + 1) + " has knot" + (j + 1) + "X = " +
                                          knotX[j] + "\n", Preferences.DEBUG_FILEIO);
                        knotY[j] = getDouble(endianess);
                        Preferences.debug(drawString + "ellipse " + (i + 1) + " has knot" + (j + 1) + "Y = " +
                                          knotY[j] + "\n", Preferences.DEBUG_FILEIO);
                    } // for (j = 0; j < numberKnots; j++)

                    if (drawingElement == BLEACH_ROI) {
                        bleachKnotX = new double[numberKnots];
                        bleachKnotY = new double[numberKnots];

                        for (j = 0; j < knotX.length; j++) {
                            bleachKnotX[j] = knotX[j];
                            bleachKnotY[j] = knotY[j];
                        }

                        fileInfo.setKnotX(bleachKnotX);
                        fileInfo.setKnotY(bleachKnotY);

                        if (numberKnots >= 4) {
                            haveBleachedEllipse = true;
                        }
                    } // if (drawingElement == BLEACH_ROI)

                    break;

                case 20: // closed polyline
                    numberKnots = getInt(endianess);
                    Preferences.debug(drawString + "closed polyline " + (i + 1) + " has " + numberKnots +
                                      " verticies\n", Preferences.DEBUG_FILEIO);
                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    for (j = 0; j < numberKnots; j++) {
                        knotX[j] = getDouble(endianess);
                        Preferences.debug(drawString + "closed polyline " + (i + 1) + " has vertex" + (j + 1) + "X = " +
                                          knotX[j] + "\n", Preferences.DEBUG_FILEIO);
                        knotY[j] = getDouble(endianess);
                        Preferences.debug(drawString + "closed polyline " + (i + 1) + " has vertex" + (j + 1) + "Y = " +
                                          knotY[j] + "\n", Preferences.DEBUG_FILEIO);
                    } // for (j = 0; j < numberKnots; j++)

                    if (drawingElement == BLEACH_ROI) {
                        bleachKnotX = new double[numberKnots];
                        bleachKnotY = new double[numberKnots];

                        for (j = 0; j < knotX.length; j++) {
                            bleachKnotX[j] = knotX[j];
                            bleachKnotY[j] = knotY[j];
                        }

                        fileInfo.setKnotX(bleachKnotX);
                        fileInfo.setKnotY(bleachKnotY);
                        haveBleachedPolyline = true;
                    } // if (drawingElement == BLEACH_ROI)

                    break;

                case 21: // open polyline
                    numberKnots = getInt(endianess);
                    Preferences.debug(drawString + "open polyline " + (i + 1) + " has " + numberKnots + " verticies\n",
                    		Preferences.DEBUG_FILEIO);
                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    for (j = 0; j < numberKnots; j++) {
                        knotX[j] = getDouble(endianess);
                        Preferences.debug(drawString + "open polyline " + (i + 1) + " has vertex" + (j + 1) + "X = " +
                                          knotX[j] + "\n", Preferences.DEBUG_FILEIO);
                        knotY[j] = getDouble(endianess);
                        Preferences.debug(drawString + "open polyline " + (i + 1) + " has vertex" + (j + 1) + "Y = " +
                                          knotY[j] + "\n", Preferences.DEBUG_FILEIO);
                    } // for (j = 0; j < numberKnots; j++)

                    break;

                case 22: // closed Bezier spline curve
                    numberKnots = getInt(endianess);
                    Preferences.debug(drawString + "closed Bezier spline curve " + (i + 1) + " has " + numberKnots +
                                      " knots\n", Preferences.DEBUG_FILEIO);
                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    for (j = 0; j < numberKnots; j++) {
                        knotX[j] = getDouble(endianess);
                        Preferences.debug(drawString + "closed Bezier spline curve " + (i + 1) + " has knot" + (j + 1) +
                                          "X = " + knotX[j] + "\n", Preferences.DEBUG_FILEIO);
                        knotY[j] = getDouble(endianess);
                        Preferences.debug(drawString + "closed Bezier spline curve " + (i + 1) + " has knot" + (j + 1) +
                                          "Y = " + knotY[j] + "\n", Preferences.DEBUG_FILEIO);
                    } // for (j = 0; j < numberKnots; j++)

                    if (drawingElement == BLEACH_ROI) {
                        bleachKnotX = new double[numberKnots];
                        bleachKnotY = new double[numberKnots];

                        for (j = 0; j < knotX.length; j++) {
                            bleachKnotX[j] = knotX[j];
                            bleachKnotY[j] = knotY[j];
                        }

                        fileInfo.setKnotX(bleachKnotX);
                        fileInfo.setKnotY(bleachKnotY);
                        haveBleachedBezier = true;
                    } // if (drawingElement == BLEACH_ROI)

                    break;

                case 23: // open Bezier spline curve
                    numberKnots = getInt(endianess);
                    Preferences.debug(drawString + "open Bezier spline curve " + (i + 1) + " has " + numberKnots +
                                      " knots\n", Preferences.DEBUG_FILEIO);
                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    for (j = 0; j < numberKnots; j++) {
                        knotX[j] = getDouble(endianess);
                        Preferences.debug(drawString + "open Bezier spline curve " + (i + 1) + " has knot" + (j + 1) +
                                          "X = " + knotX[j] + "\n", Preferences.DEBUG_FILEIO);
                        knotY[j] = getDouble(endianess);
                        Preferences.debug(drawString + "open Bezier spline curve " + (i + 1) + " has knot" + (j + 1) +
                                          "Y = " + knotY[j] + "\n", Preferences.DEBUG_FILEIO);
                    } // for (j = 0; j < numberKnots; j++)

                    break;

                case 24: // circle

                    // The first point is the center and the second point is
                    // on the perimiter.
                    numberKnots = getInt(endianess);
                    if (numberKnots == 2) {
                        Preferences.debug(drawString + "circle " + (i + 1) + " has the required 2 knots\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "circle " + (i + 1) + " has " + numberKnots +
                                          " knots instead of the required 2\n", Preferences.DEBUG_FILEIO);
                    }

                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    knotX[0] = getDouble(endianess);
                    Preferences.debug(drawString + "circle " + (i + 1) + " has knot1X = " + knotX[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[0] = getDouble(endianess);
                    Preferences.debug(drawString + "circle " + (i + 1) + " has knot1Y = " + knotY[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotX[1] = getDouble(endianess);
                    Preferences.debug(drawString + "circle " + (i + 1) + " has knot2X = " + knotX[1] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[1] = getDouble(endianess);
                    Preferences.debug(drawString + "circle " + (i + 1) + " has knot2Y = " + knotY[1] + "\n", Preferences.DEBUG_FILEIO);
                    if (drawingElement == BLEACH_ROI) {
                        bleachKnotX = new double[numberKnots];
                        bleachKnotY = new double[numberKnots];

                        for (j = 0; j < knotX.length; j++) {
                            bleachKnotX[j] = knotX[j];
                            bleachKnotY[j] = knotY[j];
                        }

                        fileInfo.setKnotX(bleachKnotX);
                        fileInfo.setKnotY(bleachKnotY);
                        haveBleachedCircle = true;
                    } // if (drawingElement == BLEACH_ROI)

                    break;

                case 25: // rectangle with color palette
                    numberKnots = getInt(endianess);
                    if (numberKnots == 2) {
                        Preferences.debug(drawString + "rectangle with color palette " + (i + 1) +
                                          " has the required 2 knots\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "rectangle with color palette " + (i + 1) + " has " +
                                          numberKnots + " knots instead of the required 2\n", Preferences.DEBUG_FILEIO);
                    }

                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    knotX[0] = getDouble(endianess);
                    Preferences.debug(drawString + "rectangle with color palette " + (i + 1) + " has knot1X = " +
                                      knotX[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[0] = getDouble(endianess);
                    Preferences.debug(drawString + "rectangle with color palette " + (i + 1) + " has knot1Y = " +
                                      knotY[0] + "\n", Preferences.DEBUG_FILEIO);
                    knotX[1] = getDouble(endianess);
                    Preferences.debug(drawString + "rectangle with color palette " + (i + 1) + " has knot2X = " +
                                      knotX[1] + "\n", Preferences.DEBUG_FILEIO);
                    knotY[1] = getDouble(endianess);
                    Preferences.debug(drawString + "rectangle with color palette " + (i + 1) + " has knot2Y = " +
                                      knotY[1] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case 26: // open polyline with arrow tip
                    numberKnots = getInt(endianess);
                    Preferences.debug(drawString + "open polyline with arrow tip " + (i + 1) + " has " + numberKnots +
                                      " verticies\n", Preferences.DEBUG_FILEIO);
                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    for (j = 0; j < numberKnots; j++) {
                        knotX[j] = getDouble(endianess);
                        Preferences.debug(drawString + "open polyline with arrow tip " + (i + 1) + " has vertex" +
                                          (j + 1) + "X = " + knotX[j] + "\n", Preferences.DEBUG_FILEIO);
                        knotY[j] = getDouble(endianess);
                        Preferences.debug(drawString + "open polyline with arrow tip " + (i + 1) + " has vertex" +
                                          (j + 1) + "Y = " + knotY[j] + "\n", Preferences.DEBUG_FILEIO);
                    } // for (j = 0; j < numberKnots; j++)

                    break;

                case 27: // open Bezier spline curve with arrow tip
                    numberKnots = getInt(endianess);
                    Preferences.debug(drawString + "open Bezier spline curve with arrow tip " + (i + 1) + " has " +
                                      numberKnots + " knots\n", Preferences.DEBUG_FILEIO);
                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    for (j = 0; j < numberKnots; j++) {
                        knotX[j] = getDouble(endianess);
                        Preferences.debug(drawString + "open Bezier spline curve with arrow tip " + (i + 1) +
                                          " has knot" + (j + 1) + "X = " + knotX[j] + "\n", Preferences.DEBUG_FILEIO);
                        knotY[j] = getDouble(endianess);
                        Preferences.debug(drawString + "open Bezier spline curve with arrow tip " + (i + 1) +
                                          " has knot" + (j + 1) + "Y = " + knotY[j] + "\n", Preferences.DEBUG_FILEIO);
                    } // for (j = 0; j < numberKnots; j++)

                    break;

                case 28: // Two connected lines for angle measurement
                    numberKnots = getInt(endianess);
                    if (numberKnots == 3) {
                        Preferences.debug(drawString + "two connected lines for angle measurement " + (i + 1) +
                                          " has the required 3 knots\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "two connected lines for angle measurement " + (i + 1) +
                                          " has " + numberKnots + " knots instead of the required 3\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    for (j = 0; j < numberKnots; j++) {
                        knotX[j] = getDouble(endianess);
                        Preferences.debug(drawString + "two connected lines for angle measurement " + (i + 1) +
                                          " has knot" + (j + 1) + "X = " + knotX[j] + "\n", Preferences.DEBUG_FILEIO);
                        knotY[j] = getDouble(endianess);
                        Preferences.debug(drawString + "two connected lines for angle measurement " + (i + 1) +
                                          " has knot" + (j + 1) + "Y = " + knotY[j] + "\n", Preferences.DEBUG_FILEIO);
                    } // for (j = 0; j < numberKnots; j++)

                    break;

                case 29: // circle with 3 perimiter points
                    numberKnots = getInt(endianess);
                    if (numberKnots == 3) {
                        Preferences.debug(drawString + "circle " + (i + 1) + " has the required 3 knots\n",
                        		Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug(drawString + "circle " + (i + 1) + " has " + numberKnots +
                                          " knots instead of the required 3\n", Preferences.DEBUG_FILEIO);
                    }

                    knotX = new double[numberKnots];
                    knotY = new double[numberKnots];
                    for (j = 0; j < numberKnots; j++) {
                        knotX[j] = getDouble(endianess);
                        Preferences.debug(drawString + "circle " + (i + 1) + " has knot" + (j + 1) + "X = " + knotX[j] +
                                          "\n", Preferences.DEBUG_FILEIO);
                        knotY[j] = getDouble(endianess);
                        Preferences.debug(drawString + "circle " + (i + 1) + " has knot" + (j + 1) + "Y = " + knotY[j] +
                                          "\n", Preferences.DEBUG_FILEIO);
                    } // for (j = 0; j < numberKnots; j++)

                    if (drawingElement == BLEACH_ROI) {
                        bleachKnotX = new double[numberKnots];
                        bleachKnotY = new double[numberKnots];

                        for (j = 0; j < knotX.length; j++) {
                            bleachKnotX[j] = knotX[j];
                            bleachKnotY[j] = knotY[j];
                        }

                        fileInfo.setKnotX(bleachKnotX);
                        fileInfo.setKnotY(bleachKnotY);
                        haveBleachedCirclePerim3 = true;
                    } // if (drawingElement == BLEACH_ROI)

                    break;
            }

            if (i < (numberDrawingElements - 1)) {
                raFile.seek(drawingElementBlockStart);
            }
        } // for (i = 0; i < numberDrawingElements; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void readKsData() { }


    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void readLut() throws IOException {
        int blockSize;
        int subBlockNumber;
        int lutChannels;
        int lutType;
        int advanced;
        int currentChannel;
        int[] reserved = new int[9];
        int lutEnd;
        int i, j, k;
        int subBlockType;
        int subBlockSize;
        String subBlockString = null;
        String lutString = null;
        long subBlockStart;
        double[] gamma;
        double[] brightness;
        double[] contrast;
        double[] startX;
        double[] startY;
        double[] endX;
        double[] endY;
        short[][] lutRes;
        int knotNumber;
        double[][] lutKnotX;
        double[][] lutKnotY;

        switch (lutKind) {

            case INPUT_LUT:
                lutString = "Input LUT ";
                break;

            case OUTPUT_LUT:
                lutString = "Output LUT ";
                break;
        }

        blockSize = getInt(endianess);
        Preferences.debug(lutString + "block size = " + blockSize + " bytes\n", Preferences.DEBUG_FILEIO);
        subBlockNumber = getInt(endianess);
        Preferences.debug(lutString + "has " + subBlockNumber + " sub blocks\n", Preferences.DEBUG_FILEIO);
        lutChannels = getInt(endianess);

        if (lutKind == INPUT_LUT) {

            if (lutChannels == czChannels) {
                Preferences.debug(lutString + "handles " + lutChannels +
                                  " channels, the same as the number of data channels\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(lutString + "handles " + lutChannels + " channels, but the number expected is " +
                                  czChannels + ",\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("the number of data channels\n", Preferences.DEBUG_FILEIO);
            }
        } else if (lutKind == OUTPUT_LUT) {

            if (lutChannels == 3) {
                Preferences.debug(lutString + "handles 3 channels as expected\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(lutString + "handles " + lutChannels + " channels, but the number expected is 3\n",
                		Preferences.DEBUG_FILEIO);
            }
        }

        lutType = getInt(endianess);

        switch (lutType) {

            case 0:
                Preferences.debug(lutString + "is normal type\n", Preferences.DEBUG_FILEIO);
                break;

            case 1:
                Preferences.debug(lutString + "is original type\n", Preferences.DEBUG_FILEIO);
                break;

            case 2:
                Preferences.debug(lutString + "is ramp type\n", Preferences.DEBUG_FILEIO);
                break;

            case 3:
                Preferences.debug(lutString + "is polyline type\n", Preferences.DEBUG_FILEIO);
                break;

            case 4:
                Preferences.debug(lutString + "is cubic spline type\n", Preferences.DEBUG_FILEIO);
                break;

            case 5:
                Preferences.debug(lutString + "is exponential type\n", Preferences.DEBUG_FILEIO);
                break;

            default:
                Preferences.debug(lutString + "has lutType = " + lutType + "\n", Preferences.DEBUG_FILEIO);
        }

        advanced = getInt(endianess);

        if (lutKind == INPUT_LUT) {

            if (advanced == 0) {
                Preferences.debug(lutString + "did not have More/Simple button pressed\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug(lutString + "did have More/Simple button pressed\n", Preferences.DEBUG_FILEIO);
            }
        }

        currentChannel = getInt(endianess);

        if (currentChannel == -1) {
            Preferences.debug(lutString + "selected all channels for modification in the LUT editor\n", 
            		Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug(lutString + " most recently selected channel " + currentChannel +
                              " for modification in the LUT editor\n", Preferences.DEBUG_FILEIO);
        }

        for (i = 0; i < 9; i++) {
            reserved[i] = getInt(endianess);

            if (reserved[i] == 0) {
                Preferences.debug("Reserved word " + (i + 1) + " of 9 equals 0 as expected\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("Reserved word " + (i + 1) + " of 9 equals " + reserved[i] +
                                  " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
            }
        }

        for (i = 0; i < subBlockNumber; i++) {
            subBlockStart = raFile.getFilePointer();
            subBlockType = getInt(endianess);

            switch (subBlockType) {

                case 1:
                    Preferences.debug(lutString + "SUBBLOCK_GAMMA\n", Preferences.DEBUG_FILEIO);
                    subBlockString = "SUBBLOCK_GAMMA";
                    break;

                case 2:
                    Preferences.debug(lutString + "SUBBLOCK_BRIGHTNESS\n", Preferences.DEBUG_FILEIO);
                    subBlockString = "SUBBLOCK_BRIGHTNESS";
                    break;

                case 3:
                    Preferences.debug(lutString + "SUBBLOCK_CONTRAST\n", Preferences.DEBUG_FILEIO);
                    subBlockString = "SUBBLOCK_CONTRAST";
                    break;

                case 4:
                    Preferences.debug(lutString + "SUBBLOCK_RAMP\n", Preferences.DEBUG_FILEIO);
                    subBlockString = "SUBBLOCK_RAMP";
                    break;

                case 5:
                    Preferences.debug(lutString + "SUBBLOCK_KNOTS\n", Preferences.DEBUG_FILEIO);
                    subBlockString = "SUBBLOCK_KNOTS";
                    break;

                case 6:
                    Preferences.debug(lutString + "SUBBLOCK_PALETTE_12_TO_12\n", Preferences.DEBUG_FILEIO);
                    subBlockString = "SUBBLOCK_PALETTE_12_TO_12";
                    break;

                default:
                    Preferences.debug(lutString + "SUBBLOCK = " + subBlockType + "\n", Preferences.DEBUG_FILEIO);
                    subBlockString = "SUBBLOCK_UNKNOWN";
            }

            subBlockSize = getInt(endianess);
            Preferences.debug(lutString + subBlockString + " has " + subBlockSize + " bytes\n", Preferences.DEBUG_FILEIO);

            switch (subBlockType) {

                case 1: // SUBBLOCK_GAMMA
                    gamma = new double[lutChannels];
                    for (j = 0; j < lutChannels; j++) {
                        gamma[j] = getDouble(endianess);
                        Preferences.debug(lutString + "channel " + (j + 1) + " has exponent " + gamma[j] + "\n",
                        		Preferences.DEBUG_FILEIO);
                    }

                    break;

                case 2: // SUBBLOCK_BRIGHTNESS
                    brightness = new double[lutChannels];
                    for (j = 0; j < lutChannels; j++) {
                        brightness[j] = getDouble(endianess);
                        Preferences.debug(lutString + "channel " + (j + 1) + " has brightness " + brightness[j] + "\n",
                        		Preferences.DEBUG_FILEIO);
                    }

                    break;

                case 3: // SUBBLOCK_CONTRAST
                    contrast = new double[lutChannels];
                    for (j = 0; j < lutChannels; j++) {
                        contrast[j] = getDouble(endianess);
                        Preferences.debug(lutString + "channel " + (j + 1) + " has contrast " + contrast[j] + "\n",
                        		Preferences.DEBUG_FILEIO);
                    }

                    break;

                case 4: // SUBBLOCK_RAMP
                    startX = new double[lutChannels];
                    startY = new double[lutChannels];
                    endX = new double[lutChannels];
                    endY = new double[lutChannels];
                    for (j = 0; j < lutChannels; j++) {
                        startX[j] = getDouble(endianess);
                        startY[j] = getDouble(endianess);
                        endX[j] = getDouble(endianess);
                        endY[j] = getDouble(endianess);
                        Preferences.debug(lutString + "channel " + (j + 1) + " start = (" + startX[j] + "," +
                                          startY[j] + ")" + " end = (" + endX[j] + "," + endY[j] + ")\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case 5: // SUBBLOCK_KNOTS

                    // Each of knotX and knotY is 8 bytes
                    if (lutChannels > 0) {
                        knotNumber = (subBlockSize - 8) / (16 * lutChannels);
                        lutKnotX = new double[lutChannels][knotNumber];
                        lutKnotY = new double[lutChannels][knotNumber];

                        for (j = 0; j < lutChannels; j++) {

                            for (k = 0; k < knotNumber; k++) {
                                lutKnotX[j][k] = getDouble(endianess);
                                lutKnotY[j][k] = getDouble(endianess);
                                Preferences.debug(lutString + "channel " + (j + 1) + " knot " + (k + 1) + " = (" +
                                                  lutKnotX[j][k] + "," + lutKnotY[j][k] + ")\n", Preferences.DEBUG_FILEIO);
                            }
                        }
                    } // if (lutChannels > 0)

                    break;

                case 6: // SUBBLOCK_PALETTE_12_TO_12
                    lutRes = new short[lutChannels][4096];
                    for (j = 0; j < lutChannels; j++) {

                        for (k = 0; k < 4096; k++) {
                            lutRes[j][k] = (short) getSignedShort(endianess);
                        }
                    }

                    break;
            }

            if (i < (subBlockNumber - 1)) {
                raFile.seek(subBlockStart + subBlockSize);
            }
        } // for (i = 0; i < subBlockNumber; i++)

        // The last word should be SUBBLOCK_LIST_END = 0
        lutEnd = getInt(endianess);

        if (lutEnd == 0) {
            Preferences.debug(lutString + "ended with expected SUBBLOCK_LIST_END\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug(lutString + "ended with " + lutEnd + " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
        }

        return;

    }


    private void readOffsetAcquisitionParameters() throws IOException {
        int i, j;
        int index = 0;
        int level = 0;
        int entry;
        int scanType;
        int scanSize;
        String pad;
        String[] tempString = new String[10000];
        int nLONG;
        int nRATIONAL;
        byte[] description;
        int endPos;
        boolean foundEnd;
        int startPos;
        boolean foundStart;
        int intValue;
        double doubleValue;
        int redValue;
        int greenValue;
        int blueValue;
        boolean doFirst = true;

        do {
            entry = getInt(endianess);
            scanType = getInt(endianess);
            scanSize = getInt(endianess);
            if (doFirst) {
                doFirst = false;
                Preferences.debug("entry = " + entry + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("scanType = " + scanType + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("scanSize = " + scanSize + "\n", Preferences.DEBUG_FILEIO);
            }

            if ((entry != SUBBLOCK_END) && (scanType == TYPE_SUBBLOCK)) {
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                level++;

                switch (entry) {

                    case SUBBLOCK_RECORDING:
                        tempString[index++] = pad + "SUBBLOCK_RECORDING";
                        break;

                    case SUBBLOCK_LASERS:
                        tempString[index++] = pad + "SUBBLOCK_LASERS";
                        break;

                    case SUBBLOCK_LASER:
                        tempString[index++] = pad + "SUBBLOCK_LASER";
                        break;

                    case SUBBLOCK_TRACKS:
                        tempString[index++] = pad + "SUBBLOCK_TRACKS";
                        break;

                    case SUBBLOCK_TRACK:
                        tempString[index++] = pad + "SUBBLOCK_TRACK";
                        break;

                    case SUBBLOCK_DETECTION_CHANNELS:
                        tempString[index++] = pad + "SUBBLOCK_DETECTION_CHANNELS";
                        break;

                    case SUBBLOCK_DETECTION_CHANNEL:
                        tempString[index++] = pad + "SUBBLOCK_DETECTION_CHANNEL";
                        break;

                    case SUBBLOCK_ILLUMINATION_CHANNELS:
                        tempString[index++] = pad + "SUBBLOCK_ILLUMINATION_CHANNELS";
                        break;

                    case SUBBLOCK_ILLUMINATION_CHANNEL:
                        tempString[index++] = pad + "SUBBLOCK_ILLUMINATION_CHANNEL";
                        break;

                    case SUBBLOCK_BEAM_SPLITTERS:
                        tempString[index++] = pad + "SUBBLOCK_BEAM_SPLITTERS";
                        break;

                    case SUBBLOCK_BEAM_SPLITTER:
                        tempString[index++] = pad + "SUBBLOCK_BEAM_SPLITTER";
                        break;

                    case SUBBLOCK_DATA_CHANNELS:
                        tempString[index++] = pad + "SUBBLOCK_DATA_CHANNELS";
                        break;

                    case SUBBLOCK_DATA_CHANNEL:
                        tempString[index++] = pad + "SUBBLOCK_DATA_CHANNEL";
                        break;

                    case SUBBLOCK_TIMERS:
                        tempString[index++] = pad + "SUBBLOCK_TIMERS";
                        break;

                    case SUBBLOCK_TIMER:
                        tempString[index++] = pad + "SUBBLOCK_TIMER";
                        break;

                    case SUBBLOCK_MARKERS:
                        tempString[index++] = pad + "SUBBLOCK_MARKERS";
                        break;

                    case SUBBLOCK_MARKER:
                        tempString[index++] = pad + "SUBBLOCK_MARKER";
                        break;
                } // switch (entry)
            } else if ((entry == SUBBLOCK_END) && (scanType == TYPE_SUBBLOCK)) {
                level--;
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                tempString[index++] = pad + "SUBBLOCK_END";
            } else if (scanType != TYPE_SUBBLOCK) {
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                switch (entry) {

                    case RECORDING_ENTRY_NAME:
                        tempString[index++] = pad + "RECORDING_ENTRY_NAME";
                        break;

                    case RECORDING_ENTRY_DESCRIPTION:
                        tempString[index++] = pad + "RECORDING_ENTRY_DESCRIPTION";
                        break;

                    case RECORDING_ENTRY_NOTES:
                        tempString[index++] = pad + "RECORDING_ENTRY_NOTES";
                        break;

                    case RECORDING_ENTRY_OBJECTIVE:
                        tempString[index++] = pad + "RECORDING_ENTRY_OBJECTIVE";
                        break;

                    case RECORDING_ENTRY_PROCESSING_SUMMARY:
                        tempString[index++] = pad + "RECORDING_ENTRY_PROCESSING_SUMMARY";
                        break;

                    case RECORDING_ENTRY_SPECIAL_SCAN_MODE:
                        tempString[index++] = pad + "RECORDING_ENTRY_SPECIAL_SCAN_MODE";
                        break;

                    case RECORDING_ENTRY_SCAN_TYPE:
                        tempString[index++] = pad + "RECORDING_ENTRY_SCAN_TYPE";
                        break;

                    case OLEDB_RECORDING_ENTRY_SCAN_MODE:
                        tempString[index++] = pad + "OLEDB_RECORDING_ENTRY_SCAN_MODE";
                        break;

                    case RECORDING_ENTRY_NUMBER_OF_STACKS:
                        tempString[index++] = pad + "RECORDING_ENTRY_NUMBER_OF_STACKS";
                        break;

                    case RECORDING_ENTRY_LINES_PER_PLANE:
                        tempString[index++] = pad + "RECORDING_ENTRY_LINES_PER_PLANE";
                        break;

                    case RECORDING_ENTRY_SAMPLES_PER_LINE:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLES_PER_LINE";
                        break;

                    case RECORDING_ENTRY_PLANES_PER_VOLUME:
                        tempString[index++] = pad + "RECORDING_ENTRY_PLANES_PER_VOLUME";
                        break;

                    case RECORDING_ENTRY_IMAGES_WIDTH:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_WIDTH";
                        break;

                    case RECORDING_ENTRY_IMAGES_HEIGHT:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_HEIGHT";
                        break;

                    case RECORDING_ENTRY_IMAGES_NUMBER_PLANES:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_NUMBER_PLANES";
                        break;

                    case RECORDING_ENTRY_IMAGES_NUMBER_STACKS:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_NUMBER_STACKS";
                        break;

                    case RECORDING_ENTRY_IMAGES_NUMBER_CHANNELS:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_NUMBER_CHANNELS";
                        break;

                    case RECORDING_ENTRY_LINSCAN_XY_SIZE:
                        tempString[index++] = pad + "RECORDING_ENTRY_LINSCAN_XY_SIZE";
                        break;

                    case RECORDING_ENTRY_SCAN_DIRECTION:
                        tempString[index++] = pad + "RECORDING_ENTRY_SCAN_DIRECTION";
                        break;

                    case RECORDING_ENTRY_TIME_SERIES:
                        tempString[index++] = pad + "RECORDING_ENTRY_TIME_SERIES";
                        break;

                    case RECORDING_ENTRY_ORIGINAL_SCAN_DATA:
                        tempString[index++] = pad + "RECORDING_ENTRY_ORIGINAL_SCAN_DATA";
                        break;

                    case RECORDING_ENTRY_ZOOM_X:
                        tempString[index++] = pad + "RECORDING_ENTRY_ZOOM_X";
                        break;

                    case RECORDING_ENTRY_ZOOM_Y:
                        tempString[index++] = pad + "RECORDING_ENTRY_ZOOM_Y";
                        break;

                    case RECORDING_ENTRY_ZOOM_Z:
                        tempString[index++] = pad + "RECORDING_ENTRY_ZOOM_Z";
                        break;

                    case RECORDING_ENTRY_SAMPLE_0X:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_0X";
                        break;

                    case RECORDING_ENTRY_SAMPLE_0Y:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_0Y";
                        break;

                    case RECORDING_ENTRY_SAMPLE_0Z:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_0Z";
                        break;

                    case RECORDING_ENTRY_SAMPLE_SPACING:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_SPACING";
                        break;

                    case RECORDING_ENTRY_LINE_SPACING:
                        tempString[index++] = pad + "RECORDING_ENTRY_LINE_SPACING";
                        break;

                    case RECORDING_ENTRY_PLANE_SPACING:
                        tempString[index++] = pad + "RECORDING_ENTRY_PLANE_SPACING";
                        break;

                    case RECORDING_ENTRY_PLANE_WIDTH:
                        tempString[index++] = pad + "RECORDING_ENTRY_PLANE_WIDTH";
                        break;

                    case RECORDING_ENTRY_PLANE_HEIGHT:
                        tempString[index++] = pad + "RECORDING_ENTRY_PLANE_HEIGHT";
                        break;

                    case RECORDING_ENTRY_VOLUME_DEPTH:
                        tempString[index++] = pad + "RECORDING_ENTRY_VOLUME_DEPTH";
                        break;

                    case RECORDING_ENTRY_ROTATION:
                        tempString[index++] = pad + "RECORDING_ENTRY_ROTATION";
                        break;

                    case RECORDING_ENTRY_NUTATION:
                        tempString[index++] = pad + "RECORDING_ENTRY_NUTATION";
                        break;

                    case RECORDING_ENTRY_PRECESSION:
                        tempString[index++] = pad + "RECORDING_ENTRY_PRECESSION";
                        break;

                    case RECORDING_ENTRY_SAMPLE_0TIME:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_0TIME";
                        break;

                    case RECORDING_ENTRY_START_SCAN_TRIGGER_IN:
                        tempString[index++] = pad + "RECORDING_ENTRY_START_SCAN_TRIGGER_IN";
                        break;

                    case RECORDING_ENTRY_START_SCAN_TRIGGER_OUT:
                        tempString[index++] = pad + "RECORDING_ENTRY_START_SCAN_TRIGGER_OUT";
                        break;

                    case RECORDING_ENTRY_START_SCAN_EVENT:
                        tempString[index++] = pad + "RECORDING_ENTRY_START_SCAN_EVENT";
                        break;

                    case RECORDING_ENTRY_START_SCAN_TIME:
                        tempString[index++] = pad + "RECORDING_ENTRY_START_SCAN_TIME";
                        break;

                    case RECORDING_ENTRY_STOP_SCAN_TRIGGER_IN:
                        tempString[index++] = pad + "RECORDING_ENTRY_STOP_SCAN_TRIGGER_IN";
                        break;

                    case RECORDING_ENTRY_STOP_SCAN_TRIGGER_OUT:
                        tempString[index++] = pad + "RECORDING_ENTRY_STOP_SCAN_TRIGGER_OUT";
                        break;

                    case RECORDING_ENTRY_STOP_SCAN_EVENT:
                        tempString[index++] = pad + "RECORDING_ENTRY_STOP_SCAN_EVENT";
                        break;

                    case RECORDING_ENTRY_STOP_SCAN_TIME:
                        tempString[index++] = pad + "RECORDING_ENTRY_STOP_SCAN_TIME";
                        break;

                    case RECORDING_ENTRY_USE_ROIS:
                        tempString[index++] = pad + "RECORDING_ENTRY_USE_ROIS";
                        break;

                    case RECORDING_ENTRY_USE_REDUCED_MEMORY_ROIS:
                        tempString[index++] = pad + "RECORDING_ENTRY_USE_REDUCED_MEMORY_ROIS";
                        break;

                    case RECORDING_ENTRY_USER:
                        tempString[index++] = pad + "RECORDING_ENTRY_USER";
                        break;

                    case RECORDING_ENTRY_USE_BCCORRECTION:
                        tempString[index++] = pad + "RECORDING_ENTRY_USE_BCCORRECTION";
                        break;

                    case RECORDING_ENTRY_POSITION_BCCORRECTION1:
                        tempString[index++] = pad + "RECORDING_ENTRY_POSITION_BCCORRECTION1";
                        break;

                    case RECORDING_ENTRY_POSITION_BCCORRECTION2:
                        tempString[index++] = pad + "RECORDING_ENTRY_POSITION_BCCORRECTION2";
                        break;

                    case RECORDING_ENTRY_INTERPOLATIONY:
                        tempString[index++] = pad + "RECORDING_ENTRY_INTERPOLATIONY";
                        break;

                    case RECORDING_ENTRY_CAMERA_BINNING:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_BINNING";
                        break;

                    case RECORDING_ENTRY_CAMERA_SUPERSAMPLING:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_SUPERSAMPLING";
                        break;

                    case RECORDING_ENTRY_CAMERA_FRAME_WIDTH:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_FRAME_WIDTH";
                        break;

                    case RECORDING_ENTRY_CAMERA_FRAME_HEIGHT:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_FRAME_HEIGHT";
                        break;

                    case RECORDING_ENTRY_CAMERA_OFFSETX:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_OFFSETX";
                        break;

                    case RECORDING_ENTRY_CAMERA_OFFSETY:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_OFFSETY";
                        break;

                    case TRACK_ENTRY_MULTIPLEX_TYPE:
                        tempString[index++] = pad + "TRACK_ENTRY_MULTIPLEX_TYPE";
                        break;

                    case TRACK_ENTRY_MULTIPLEX_ORDER:
                        tempString[index++] = pad + "TRACK_ENTRY_MULTIPLEX_ORDER";
                        break;

                    case TRACK_ENTRY_SAMPLING_MODE:
                        tempString[index++] = pad + "TRACK_ENTRY_SAMPLING_MODE";
                        break;

                    case TRACK_ENTRY_SAMPLING_METHOD:
                        tempString[index++] = pad + "TRACK_ENTRY_SAMPLING_METHOD";
                        break;

                    case TRACK_ENTRY_SAMPLING_NUMBER:
                        tempString[index++] = pad + "TRACK_ENTRY_SAMPLING_NUMBER";
                        break;

                    case TRACK_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "TRACK_ENTRY_ACQUIRE";
                        break;

                    case TRACK_ENTRY_SAMPLE_OBSERVATION_TIME:
                        tempString[index++] = pad + "TRACK_ENTRY_SAMPLE_OBSERVATION_TIME";
                        break;

                    case TRACK_ENTRY_TIME_BETWEEN_STACKS:
                        tempString[index++] = pad + "TRACK_ENTRY_TIME_BETWEEN_STACKS";
                        break;

                    case TRACK_ENTRY_NAME:
                        tempString[index++] = pad + "TRACK_ENTRY_NAME";
                        break;

                    case TRACK_ENTRY_COLLIMATOR1_NAME:
                        tempString[index++] = pad + "TRACK_ENTRY_COLLIMATOR1_NAME";
                        break;

                    case TRACK_ENTRY_COLLIMATOR1_POSITION:
                        tempString[index++] = pad + "TRACK_ENTRY_COLLIMATOR1_POSITION";
                        break;

                    case TRACK_ENTRY_COLLIMATOR2_NAME:
                        tempString[index++] = pad + "TRACK_ENTRY_COLLIMATOR2_NAME";
                        break;

                    case TRACK_ENTRY_COLLIMATOR2_POSITION:
                        tempString[index++] = pad + "TRACK_ENTRY_COLLIMATOR2_POSITION";
                        break;

                    case TRACK_ENTRY_IS_BLEACH_TRACK:
                        tempString[index++] = pad + "TRACK_ENTRY_IS_BLEACH_TRACK";
                        break;

                    case TRACK_ENTRY_IS_BLEACH_AFTER_SCAN_NUMBER:
                        tempString[index++] = pad + "TRACK_ENTRY_IS_BLEACH_AFTER_SCAN_NUMBER";
                        break;

                    case TRACK_ENTRY_BLEACH_SCAN_NUMBER:
                        tempString[index++] = pad + "TRACK_ENTRY_BLEACH_SCAN_NUMBER";
                        break;

                    case TRACK_ENTRY_TRIGGER_IN:
                        tempString[index++] = pad + "TRACK_ENTRY_TRIGGER_IN";
                        break;

                    case TRACK_ENTRY_TRIGGER_OUT:
                        tempString[index++] = pad + "TRACK_ENTRY_TRIGGER_OUT";
                        break;

                    case TRACK_ENTRY_IS_RATIO_TRACK:
                        tempString[index++] = pad + "TRACK_ENTRY_IS_RATIO_TRACK";
                        break;

                    case TRACK_ENTRY_BLEACH_COUNT:
                        tempString[index++] = pad + "TRACK_ENTRY_BLEACH_COUNT";
                        break;

                    case TRACK_ENTRY_SPI_CENTER_WAVELENGTH:
                        tempString[index++] = pad + "TRACK_ENTRY_SPI_CENTER_WAVELENGTH";
                        break;

                    case TRACK_ENTRY_PIXEL_TIME:
                        tempString[index++] = pad + "TRACK_ENTRY_PIXEL_TIME";
                        break;

                    case TRACK_ENTRY_ID_CONDENSOR_FRONTLENS:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_CONDENSOR_FRONTLENS";
                        break;

                    case TRACK_ENTRY_CONDENSOR_FRONTLENS:
                        tempString[index++] = pad + "TRACK_ENTRY_CONDENSOR_FRONTLENS";
                        break;

                    case TRACK_ENTRY_ID_FIELD_STOP:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_FIELD_STOP";
                        break;

                    case TRACK_ENTRY_FIELD_STOP_VALUE:
                        tempString[index++] = pad + "TRACK_ENTRY_FIELD_STOP_VALUE";
                        break;

                    case TRACK_ENTRY_ID_CONDENSOR_APERTURE:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_CONDENSOR_APERTURE";
                        break;

                    case TRACK_ENTRY_CONDENSOR_APERTURE:
                        tempString[index++] = pad + "TRACK_ENTRY_CONDENSOR_APERTURE";
                        break;

                    case TRACK_ENTRY_ID_CONDENSOR_REVOLVER:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_CONDENSOR_REVOLVER";
                        break;

                    case TRACK_ENTRY_CONDENSOR_FILTER:
                        tempString[index++] = pad + "TRACK_ENTRY_CONDENSOR_FILTER";
                        break;

                    case TRACK_ENTRY_ID_TRANSMISSION_FILTER1:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_TRANSMISSION_FILTER1";
                        break;

                    case TRACK_ENTRY_ID_TRANSMISSION1:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_TRANSMISSION1";
                        break;

                    case TRACK_ENTRY_ID_TRANSMISSION_FILTER2:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_TRANSMISSION_FILTER2";
                        break;

                    case TRACK_ENTRY_ID_TRANSMISSION2:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_TRANSMISSION2";
                        break;

                    case TRACK_ENTRY_REPEAT_BLEACH:
                        tempString[index++] = pad + "TRACK_ENTRY_REPEAT_BLEACH";
                        break;

                    case TRACK_ENTRY_ENABLE_SPOT_BLEACH_POS:
                        tempString[index++] = pad + "TRACK_ENTRY_ENABLE_SPOT_BLEACH_POS";
                        break;

                    case TRACK_ENTRY_SPOT_BLEACH_POSX:
                        tempString[index++] = pad + "TRACK_ENTRY_SPOT_BLEACH_POSX";
                        break;

                    case TRACK_ENTRY_SPOT_BLEACH_POSY:
                        tempString[index++] = pad + "TRACK_ENTRY_SPOT_BLEACH_POSY";
                        break;

                    case TRACK_ENTRY_BLEACH_POSITION_Z:
                        tempString[index++] = pad + "TRACK_ENTRY_BLEACH_POSITION_Z";
                        break;

                    case LASER_ENTRY_NAME:
                        tempString[index++] = pad + "LASER_ENTRY_NAME";
                        break;

                    case LASER_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "LASER_ENTRY_ACQUIRE";
                        break;

                    case LASER_ENTRY_POWER:
                        tempString[index++] = pad + "LASER_ENTRY_POWER";
                        break;

                    case DETCHANNEL_ENTRY_INTEGRATION_MODE:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_INTEGRATION_MODE";
                        break;

                    case DETCHANNEL_ENTRY_SPECIAL_MODE:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_SPECIAL_MODE";
                        break;

                    case DETCHANNEL_ENTRY_DETECTOR_GAIN_FIRST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_DETECTOR_GAIN_FIRST";
                        break;

                    case DETCHANNEL_ENTRY_DETECTOR_GAIN_LAST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_DETECTOR_GAIN_LAST";
                        break;

                    case DETCHANNEL_ENTRY_AMPLIFIER_GAIN_FIRST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_AMPLIFIER_GAIN_FIRST";
                        break;

                    case DETCHANNEL_ENTRY_AMPLIFIER_GAIN_LAST:
                        tempString[index++] = pad + "DETCHANEL_ENTRY_AMPLIFIER_GAIN_LAST";
                        break;

                    case DETCHANNEL_ENTRY_AMPLIFIER_OFFS_FIRST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_AMPLIFIER_OFFS_FIRST";
                        break;

                    case DETCHANNEL_ENTRY_AMPLIFIER_OFFS_LAST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_AMPLIFIER_OFFS_LAST";
                        break;

                    case DETCHANNEL_ENTRY_PINHOLE_DIAMETER:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_PINHOLE_DIAMETER";
                        break;

                    case DETCHANNEL_ENTRY_COUNTING_TRIGGER:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_COUNTING_TRIGGER";
                        break;

                    case DETCHANNEL_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_ACQUIRE";
                        break;

                    case DETCHANNEL_POINT_DETECTOR_NAME:
                        tempString[index++] = pad + "DETCHANNEL_POINT_DETECTOR_NAME";
                        break;

                    case DETCHANNEL_AMPLIFIER_NAME:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_NAME";
                        break;

                    case DETCHANNEL_PINHOLE_NAME:
                        tempString[index++] = pad + "DETCHANNEL_PINHOLE_NAME";
                        break;

                    case DETCHANNEL_FILTER_SET_NAME:
                        tempString[index++] = pad + "DETCHANNEL_FILTER_SET_NAME";
                        break;

                    case DETCHANNEL_FILTER_NAME:
                        tempString[index++] = pad + "DETCHANNEL_FILTER_NAME";
                        break;

                    case DETCHANNEL_INTEGRATOR_NAME:
                        tempString[index++] = pad + "DETCHANNEL_INTEGRATOR_NAME";
                        break;

                    case DETCHANNEL_DETECTION_CHANNEL_NAME:
                        tempString[index++] = pad + "DETCHANNEL_DETECTION_CHANNEL_NAME";
                        break;

                    case DETCHANNEL_DETECTOR_GAIN_BC1:
                        tempString[index++] = pad + "DETCHANNEL_DETECTOR_GAIN_BC1";
                        break;

                    case DETCHANNEL_DETECTOR_GAIN_BC2:
                        tempString[index++] = pad + "DETCHANNEL_DETECTOR_GAIN_BC2";
                        break;

                    case DETCHANNEL_AMPLIFIER_GAIN_BC1:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_GAIN_BC1";
                        break;

                    case DETCHANNEL_AMPLIFIER_GAIN_BC2:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_GAIN_BC2";
                        break;

                    case DETCHANNEL_AMPLIFIER_OFFSET_BC1:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_OFFSET_BC1";
                        break;

                    case DETCHANNEL_AMPLIFIER_OFFSET_BC2:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_OFFSET_BC2";
                        break;

                    case DETCHANNEL_SPECTRAL_SCAN_CHANNELS:
                        tempString[index++] = pad + "DETCHANNEL_SPECTRAL_SCAN_CHANNELS";
                        break;

                    case DETCHANNEL_SPI_WAVELENGTH_START:
                        tempString[index++] = pad + "DETCHANNEL_SPI_WAVELENGTH_START";
                        break;

                    case DETCHANNEL_SPI_WAVELENGTH_END:
                        tempString[index++] = pad + "DETCHANNEL_SPI_WAVELENGTH_END";
                        break;

                    case DETCHANNEL_DYE_NAME:
                        tempString[index++] = pad + "DETCHANNEL_DYE_NAME";
                        break;

                    case DETCHANNEL_DYE_FOLDER:
                        tempString[index++] = pad + "DETCHANNEL_DYE_FOLDER";
                        break;

                    case ILLUMCHANNEL_ENTRY_NAME:
                        tempString[index++] = pad + "ILLUMCHANNEL_ENTRY_NAME";
                        break;

                    case ILLUMCHANNEL_ENTRY_POWER:
                        tempString[index++] = pad + "ILLUMCHANNEL_ENTRY_POWER";
                        break;

                    case ILLUMCHANNEL_ENTRY_WAVELENGTH:
                        tempString[index++] = pad + "ILLUMCHANNEL_ENTRY_WAVELENGTH";
                        break;

                    case ILLUMCHANNEL_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "ILLUMCHANNEL_ENTRY_ACQUIRE";
                        break;

                    case ILLUMCHANNEL_DETCHANNEL_NAME:
                        tempString[index++] = pad + "ILLUMCHANNEL_DETCHANNEL_NAME";
                        break;

                    case ILLUMCHANNEL_POWER_BC1:
                        tempString[index++] = pad + "ILLUMCHANNEL_POWER_BC1";
                        break;

                    case ILLUMCHANNEL_POWER_BC2:
                        tempString[index++] = pad + "ILLUMCHANNEL_POWER_BC2";
                        break;

                    case BEAMSPLITTER_ENTRY_FILTER_SET:
                        tempString[index++] = pad + "BEAMSPLITTER_ENTRY_FILTER_SET";
                        break;

                    case BEAMSPLITTER_ENTRY_FILTER:
                        tempString[index++] = pad + "BEAMSPLITTER_ENTRY_FILTER";
                        break;

                    case BEAMSPLITTER_ENTRY_NAME:
                        tempString[index++] = pad + "BEAMSPLITTER_ENTRY_NAME";
                        break;

                    case DATACHANNEL_ENTRY_NAME:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_NAME";
                        break;

                    case DATACHANNEL_ENTRY_COLOR:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_COLOR";
                        break;

                    case DATACHANNEL_ENTRY_SAMPLETYPE:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_SAMPLETYPE";
                        break;

                    case DATACHANNEL_ENTRY_BITSPERSAMPLE:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_BITSPERSAMPLE";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_TYPE:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_TYPE";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_TRACK1:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_TRACK1";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CHANNEL1:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CHANNEL1";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_TRACK2:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_TRACK2";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CHANNEL2:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CHANNEL2";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST1:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST1";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST2:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST2";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST3:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST3";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST4:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST4";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST5:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST5";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST6:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST6";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES1:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES1";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES2:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES2";
                        break;

                    case DATACHANNEL_ENTRY_DYE_NAME:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_DYE_NAME";
                        break;

                    case DATACHANNEL_ENTRY_DYE_FOLDER:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_DYE_FOLDER";
                        break;

                    case DATACHANNEL_ENTRY_SPECTRUM:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_SPECTRUM";
                        break;

                    case DATACHANNEL_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_ACQUIRE";
                        break;

                    case TIMER_ENTRY_NAME:
                        tempString[index++] = pad + "TIMER_ENTRY_NAME";
                        break;

                    case TIMER_ENTRY_DESCRIPTION:
                        tempString[index++] = pad + "TIMER_ENTRY_DESCRIPTION";
                        break;

                    case TIMER_ENTRY_INTERVAL:
                        tempString[index++] = pad + "TIMER_ENTRY_INTERVAL";
                        break;

                    case TIMER_ENTRY_TRIGGER_IN:
                        tempString[index++] = pad + "TIMER_ENTRY_TRIGGER_IN";
                        break;

                    case TIMER_ENTRY_TRIGGER_OUT:
                        tempString[index++] = pad + "TIMER_ENTRY_TRIGGER_OUT";
                        break;

                    case TIMER_ENTRY_ACTIVATION_TIME:
                        tempString[index++] = pad + "TIMER_ENTRY_ACTIVATION_TIME";
                        break;

                    case TIMER_ENTRY_ACTIVATION_NUMBER:
                        tempString[index++] = pad + "TIMER_ENTRY_ACTIVATION_NUMBER";
                        break;

                    case MARKER_ENTRY_NAME:
                        tempString[index++] = pad + "MARKER_ENTRY_NAME";
                        break;

                    case MARKER_ENTRY_DESCRIPTION:
                        tempString[index++] = pad + "MARKER_ENTRY_DESCRIPTION";
                        break;

                    case MARKER_ENTRY_TRIGGER_IN:
                        tempString[index++] = pad + "MARKER_ENTRY_TRIGGER_IN";
                        break;

                    case MARKER_ENTRY_TRIGGER_OUT:
                        tempString[index++] = pad + "MARKER_ENTRY_TRIGGER_OUT";
                        break;

                    default:
                        // long longEntry = (long)entry;
                        // longEntry = 0x00000000ffffffffL & longEntry;
                        // System.out.println("entry = " + Long.toString(longEntry,16));
                } // switch(entry)
            }

            if (scanType == TYPE_LONG) {
                nLONG = scanSize / 4;
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                for (i = 0; i < nLONG; i++) {
                    intValue = getInt(endianess);

                    switch (entry) {

                        case RECORDING_ENTRY_START_SCAN_EVENT:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Button (normal operation)";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Trigger slow - Scanner off" +
                                                          ", PMT high voltage off, reaction 300 msec";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "Trigger normal - Scanner " +
                                                          "off, PMT high voltage on, reaction 30 msec";
                                    break;

                                case 3:
                                    tempString[index++] = pad + "Trigger fast - Scanner on," +
                                                          " PMT high voltage on, reaction 5 msec";
                                    break;

                                case 4:
                                    tempString[index++] = pad + "Start time";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case RECORDING_ENTRY_STOP_SCAN_EVENT:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Button (normal operation button)";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Trigger";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "End time";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case RECORDING_ENTRY_USE_ROIS:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "ROIs should not be used for the scan process";
                                    break;

                                default:
                                    tempString[index++] = pad + "ROIs should be used for the scan process";
                            }

                            break;

                        case RECORDING_ENTRY_USE_REDUCED_MEMORY_ROIS:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Scan memory was provided for the whole plane";
                                    break;

                                default:
                                    tempString[index++] = pad + "Scan memory was provided " +
                                                          "only for the ROI bounding rectangle";
                            }

                            break;

                        case RECORDING_ENTRY_INTERPOLATIONY:
                            switch (intValue) {

                                case 1:
                                    tempString[index++] = pad + "No interpolation was done";
                                    break;

                                default:
                                    tempString[index++] = pad + "Scan 1 out of every " + String.valueOf(intValue) +
                                                          " lines";
                            }

                            break;

                        case RECORDING_ENTRY_CAMERA_SUPERSAMPLING:
                            switch (intValue) {

                                case 0:
                                case 1:
                                    tempString[index++] = pad + "No technique was used " +
                                                          "to enlarge the number of pixels";
                                    break;

                                default:
                                    tempString[index++] = pad + "Horizontal and vertical " +
                                                          "enlargement of pixel number by factor of " +
                                                          String.valueOf(intValue);
                            }

                            break;

                        case TRACK_ENTRY_MULTIPLEX_TYPE:
                            switch (intValue) {

                                case 2:
                                    tempString[index++] = pad + "A switch to the next track is done afer a stack";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "A switch to the next track is done after a plane";
                                    break;

                                case 0:
                                    tempString[index++] = pad + "A switch to the next track is done after a line";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case TRACK_ENTRY_SAMPLING_MODE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Sample";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Line-Average";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "Frame-Average";
                                    break;

                                case 3:
                                    tempString[index++] = pad + "Integration mode";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case TRACK_ENTRY_SAMPLING_METHOD:
                            switch (intValue) {

                                case 1:
                                    tempString[index++] = pad + "Mean";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "Sum";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case TRACK_ENTRY_ACQUIRE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "The track was not used during scan";
                                    break;

                                default:
                                    tempString[index++] = pad + "The track was used during scan";
                            }

                            break;

                        case TRACK_ENTRY_IS_BLEACH_TRACK:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "The track setting does not specify bleach parameters";
                                    break;

                                default:
                                    tempString[index++] = pad + "The track setting does specify bleach parameters " +
                                                          "and has no active detection channels";
                            }

                            break;

                        case TRACK_ENTRY_CONDENSOR_FRONTLENS:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "No condensor front lens was in the beam path";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "The condensor front lens was in the beam path";
                                    break;
                            }

                            break;

                        case LASER_ENTRY_ACQUIRE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "The laser was off";
                                    break;

                                default:
                                    tempString[index++] = pad + "The laser was on";
                            }

                            break;

                        case DETCHANNEL_ENTRY_INTEGRATION_MODE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Integration mode";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Photon counting mode";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case DETCHANNEL_ENTRY_ACQUIRE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Detection channel was not used during scan";
                                    break;

                                default:
                                    tempString[index++] = pad + "Detection channel was used during scan";
                            }

                            break;

                        case ILLUMCHANNEL_ENTRY_ACQUIRE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "The attenuator was disabled";
                                    break;

                                default:
                                    tempString[index++] = pad + "The attenuator was enabled";
                            }

                            break;

                        case DATACHANNEL_ENTRY_COLOR:
                            redValue = intValue & 0x000000ff;
                            greenValue = (intValue >> 8) & 0x000000ff;
                            blueValue = (intValue >> 16) & 0x000000ff;
                            tempString[index++] = pad + "red = " + String.valueOf(redValue);
                            tempString[index++] = pad + "green = " + String.valueOf(greenValue);
                            tempString[index++] = pad + "blue = " + String.valueOf(blueValue);
                            break;

                        case DATACHANNEL_ENTRY_SAMPLETYPE:
                            switch (intValue) {

                                case 1:
                                    tempString[index++] = pad + "8 bit channel";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "12 bit channel";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case DATACHANNEL_ENTRY_BITSPERSAMPLE:
                            switch (intValue) {

                                case 8:
                                    tempString[index++] = pad + "8 bit channel";
                                    break;

                                case 12:
                                    tempString[index++] = pad + "12 bit channel";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case DATACHANNEL_ENTRY_RATIO_TYPE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "No online calculation - " +
                                                          "the data channel receives raw scan data";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Online ratio: " + "(S1 + C1)/(S2 + C2)*C3+C4";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "Online subtraction: " + "(S1*C1 - S2*C3)/C2 + C4";
                                    break;

                                case 3:
                                    tempString[index++] = pad + "Online ratio: " + "(S1-S2*C1)/(S1+S2*C2)*C3+C4";
                                    break;

                                case 4:
                                    tempString[index++] = pad + "Online hill function: " + "C3*(C4/C5)*(S1-C1)/(C2-S2)";
                                    break;

                                case 5:
                                    tempString[index++] = pad + "Online reference ratio: " + "(S1-S2+C1)/(S2-C2)*C3+C4";
                                    break;

                                case 6:
                                    tempString[index++] = pad + "Online linear unmixing";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        default:
                            tempString[index++] = pad + String.valueOf(intValue);
                    }
                }
            } // if (scanType == TYPE_LONG)
            else if (scanType == TYPE_RATIONAL) {
                nRATIONAL = scanSize / 8;
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                for (i = 0; i < nRATIONAL; i++) {
                    doubleValue = getDouble(endianess);
                    tempString[index++] = pad + String.valueOf(doubleValue);
                }
            } // else if (scanType == TYPE_RATIONAL) {
            else if ((scanType == TYPE_ASCII) && (scanSize > 0)) {
                description = new byte[scanSize];

                for (i = 0; i < scanSize; i++) {
                    description[i] = raFile.readByte();
                }

                endPos = 0;
                foundEnd = false;

                for (j = scanSize - 1; (j >= 0) && (!foundEnd); j--) {

                    if (description[j] > 0x20) {
                        endPos = j;
                        foundEnd = true;
                    }
                }

                startPos = 0;
                foundStart = false;

                for (j = 0; (j < endPos) && (!foundStart); j++) {

                    if (description[j] > 0x20) {
                        startPos = j;
                        foundStart = true;
                    }
                }

                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                if (endPos > 0) {
                    tempString[index++] = pad + new String(description, startPos, endPos - startPos + 1);
                }
            }
        } while (level > 0);

        for (i = 0; i < (index - 1); i++) {
            Preferences.debug(tempString[i] + "\n", Preferences.DEBUG_FILEIO);
        }    
    }
    
    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void readOffsetChannelFactors() throws IOException {
        int bytesToFollow;
        int nChannels;
        int i;
        double[] factor;
        double[] offset;
        int[] unit;
        int[][] reserved;

        bytesToFollow = getInt(endianess);
        Preferences.debug("Bytes to follow in offset channel factors = " + bytesToFollow + "\n", Preferences.DEBUG_FILEIO);
        nChannels = bytesToFollow / 32;
        factor = new double[nChannels];
        offset = new double[nChannels];
        unit = new int[nChannels];
        reserved = new int[nChannels][3];

        for (i = 0; i < nChannels; i++) {
            factor[i] = getDouble(endianess);
            Preferences.debug("Factor[" + (i + 1) + "] = " + factor[i] + " in offsetChannelFactors\n", Preferences.DEBUG_FILEIO);
            offset[i] = getDouble(endianess);
            Preferences.debug("Offset[" + (i + 1) + "] = " + offset[i] + " in offsetChannelFactors\n", Preferences.DEBUG_FILEIO);
            unit[i] = getInt(endianess);
            Preferences.debug("Unit[" + (i + 1) + "] = " + unit[i] + " in offsetChannelFactors\n", Preferences.DEBUG_FILEIO);
            reserved[i][0] = getInt(endianess);
            reserved[i][1] = getInt(endianess);
            reserved[i][2] = getInt(endianess);
        }

    }
    
    private void readOffsetCharacteristics() throws IOException {
        
    }
    
    private void readOffsetPalette() throws IOException {
        
    }
    
    private void readOffsetPositions() throws IOException {
        int i;
        int positions;
        int positionX1[];
        int positionY1[];
        int positionX2[];
        int positionY2[];
        
        positions = readInt(endianess);
        Preferences.debug("The number of acquisition regions for which position information is stored = " 
                           + positions + "\n", Preferences.DEBUG_FILEIO);
        positionX1 = new int[positions];
        positionY1 = new int[positions];
        positionX2 = new int[positions];
        positionY2 = new int[positions];
        for (i = 0; i < positions; i++) {
            Preferences.debug("For region " + (i+1) + ": \n", Preferences.DEBUG_FILEIO);
            positionX1[i] = getInt(endianess);
            Preferences.debug("Position X1 = " + positionX1[i] + " meters\n", Preferences.DEBUG_FILEIO);
            positionY1[i] = getInt(endianess);
            Preferences.debug("Position Y1 = " + positionY1[i] + " meters\n", Preferences.DEBUG_FILEIO);
            positionX2[i] = getInt(endianess);
            Preferences.debug("Position X2 = " + positionX2[i] + " meters\n", Preferences.DEBUG_FILEIO);
            positionY2[i] = getInt(endianess);
            Preferences.debug("Position Y2 = " + positionY2[i] + " meters\n", Preferences.DEBUG_FILEIO);
        }
    }
    
    private void readOffsetTilePositions() throws IOException {
        int i;
        int positions;
        int positionX1[];
        int positionY1[];
        int positionX2[];
        int positionY2[];
        
        positions = readInt(endianess);
        Preferences.debug("The number of tiles for which position information is stored = " 
                           + positions + "\n", Preferences.DEBUG_FILEIO);
        positionX1 = new int[positions];
        positionY1 = new int[positions];
        positionX2 = new int[positions];
        positionY2 = new int[positions];
        for (i = 0; i < positions; i++) {
            Preferences.debug("For tile " + (i+1) + ": \n", Preferences.DEBUG_FILEIO);
            positionX1[i] = getInt(endianess);
            Preferences.debug("Position X1 = " + positionX1[i] + " meters\n", Preferences.DEBUG_FILEIO);
            positionY1[i] = getInt(endianess);
            Preferences.debug("Position Y1 = " + positionY1[i] + " meters\n", Preferences.DEBUG_FILEIO);
            positionX2[i] = getInt(endianess);
            Preferences.debug("Position X2 = " + positionX2[i] + " meters\n", Preferences.DEBUG_FILEIO);
            positionY2[i] = getInt(endianess);
            Preferences.debug("Position Y2 = " + positionY2[i] + " meters\n", Preferences.DEBUG_FILEIO);
        }
        
    }

    /**
     * DOCUMENT ME!
     */
    private void readOffsetUnmixParameters() { }


    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void readScanInformation() throws IOException {
        int i, j;
        int index = 0;
        int level = 0;
        int entry;
        int scanType;
        int scanSize;
        String pad;
        String[] tempString = new String[10000];
        int nLONG;
        int nRATIONAL;
        byte[] description;
        int endPos;
        boolean foundEnd;
        int startPos;
        boolean foundStart;
        int intValue;
        double doubleValue;
        int redValue;
        int greenValue;
        int blueValue;

        do {
            entry = getInt(endianess);
            scanType = getInt(endianess);
            scanSize = getInt(endianess);

            if ((entry != SUBBLOCK_END) && (scanType == TYPE_SUBBLOCK)) {
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                level++;

                switch (entry) {

                    case SUBBLOCK_RECORDING:
                        tempString[index++] = pad + "SUBBLOCK_RECORDING";
                        break;

                    case SUBBLOCK_LASERS:
                        tempString[index++] = pad + "SUBBLOCK_LASERS";
                        break;

                    case SUBBLOCK_LASER:
                        tempString[index++] = pad + "SUBBLOCK_LASER";
                        break;

                    case SUBBLOCK_TRACKS:
                        tempString[index++] = pad + "SUBBLOCK_TRACKS";
                        break;

                    case SUBBLOCK_TRACK:
                        tempString[index++] = pad + "SUBBLOCK_TRACK";
                        break;

                    case SUBBLOCK_DETECTION_CHANNELS:
                        tempString[index++] = pad + "SUBBLOCK_DETECTION_CHANNELS";
                        break;

                    case SUBBLOCK_DETECTION_CHANNEL:
                        tempString[index++] = pad + "SUBBLOCK_DETECTION_CHANNEL";
                        break;

                    case SUBBLOCK_ILLUMINATION_CHANNELS:
                        tempString[index++] = pad + "SUBBLOCK_ILLUMINATION_CHANNELS";
                        break;

                    case SUBBLOCK_ILLUMINATION_CHANNEL:
                        tempString[index++] = pad + "SUBBLOCK_ILLUMINATION_CHANNEL";
                        break;

                    case SUBBLOCK_BEAM_SPLITTERS:
                        tempString[index++] = pad + "SUBBLOCK_BEAM_SPLITTERS";
                        break;

                    case SUBBLOCK_BEAM_SPLITTER:
                        tempString[index++] = pad + "SUBBLOCK_BEAM_SPLITTER";
                        break;

                    case SUBBLOCK_DATA_CHANNELS:
                        tempString[index++] = pad + "SUBBLOCK_DATA_CHANNELS";
                        break;

                    case SUBBLOCK_DATA_CHANNEL:
                        tempString[index++] = pad + "SUBBLOCK_DATA_CHANNEL";
                        break;

                    case SUBBLOCK_TIMERS:
                        tempString[index++] = pad + "SUBBLOCK_TIMERS";
                        break;

                    case SUBBLOCK_TIMER:
                        tempString[index++] = pad + "SUBBLOCK_TIMER";
                        break;

                    case SUBBLOCK_MARKERS:
                        tempString[index++] = pad + "SUBBLOCK_MARKERS";
                        break;

                    case SUBBLOCK_MARKER:
                        tempString[index++] = pad + "SUBBLOCK_MARKER";
                        break;
                } // switch (entry)
            } else if ((entry == SUBBLOCK_END) && (scanType == TYPE_SUBBLOCK)) {
                level--;
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                tempString[index++] = pad + "SUBBLOCK_END";
            } else if (scanType != TYPE_SUBBLOCK) {
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                switch (entry) {

                    case RECORDING_ENTRY_NAME:
                        tempString[index++] = pad + "RECORDING_ENTRY_NAME";
                        break;

                    case RECORDING_ENTRY_DESCRIPTION:
                        tempString[index++] = pad + "RECORDING_ENTRY_DESCRIPTION";
                        break;

                    case RECORDING_ENTRY_NOTES:
                        tempString[index++] = pad + "RECORDING_ENTRY_NOTES";
                        break;

                    case RECORDING_ENTRY_OBJECTIVE:
                        tempString[index++] = pad + "RECORDING_ENTRY_OBJECTIVE";
                        break;

                    case RECORDING_ENTRY_PROCESSING_SUMMARY:
                        tempString[index++] = pad + "RECORDING_ENTRY_PROCESSING_SUMMARY";
                        break;

                    case RECORDING_ENTRY_SPECIAL_SCAN_MODE:
                        tempString[index++] = pad + "RECORDING_ENTRY_SPECIAL_SCAN_MODE";
                        break;

                    case RECORDING_ENTRY_SCAN_TYPE:
                        tempString[index++] = pad + "RECORDING_ENTRY_SCAN_TYPE";
                        break;

                    case OLEDB_RECORDING_ENTRY_SCAN_MODE:
                        tempString[index++] = pad + "OLEDB_RECORDING_ENTRY_SCAN_MODE";
                        break;

                    case RECORDING_ENTRY_NUMBER_OF_STACKS:
                        tempString[index++] = pad + "RECORDING_ENTRY_NUMBER_OF_STACKS";
                        break;

                    case RECORDING_ENTRY_LINES_PER_PLANE:
                        tempString[index++] = pad + "RECORDING_ENTRY_LINES_PER_PLANE";
                        break;

                    case RECORDING_ENTRY_SAMPLES_PER_LINE:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLES_PER_LINE";
                        break;

                    case RECORDING_ENTRY_PLANES_PER_VOLUME:
                        tempString[index++] = pad + "RECORDING_ENTRY_PLANES_PER_VOLUME";
                        break;

                    case RECORDING_ENTRY_IMAGES_WIDTH:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_WIDTH";
                        break;

                    case RECORDING_ENTRY_IMAGES_HEIGHT:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_HEIGHT";
                        break;

                    case RECORDING_ENTRY_IMAGES_NUMBER_PLANES:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_NUMBER_PLANES";
                        break;

                    case RECORDING_ENTRY_IMAGES_NUMBER_STACKS:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_NUMBER_STACKS";
                        break;

                    case RECORDING_ENTRY_IMAGES_NUMBER_CHANNELS:
                        tempString[index++] = pad + "RECORDING_ENTRY_IMAGES_NUMBER_CHANNELS";
                        break;

                    case RECORDING_ENTRY_LINSCAN_XY_SIZE:
                        tempString[index++] = pad + "RECORDING_ENTRY_LINSCAN_XY_SIZE";
                        break;

                    case RECORDING_ENTRY_SCAN_DIRECTION:
                        tempString[index++] = pad + "RECORDING_ENTRY_SCAN_DIRECTION";
                        break;

                    case RECORDING_ENTRY_TIME_SERIES:
                        tempString[index++] = pad + "RECORDING_ENTRY_TIME_SERIES";
                        break;

                    case RECORDING_ENTRY_ORIGINAL_SCAN_DATA:
                        tempString[index++] = pad + "RECORDING_ENTRY_ORIGINAL_SCAN_DATA";
                        break;

                    case RECORDING_ENTRY_ZOOM_X:
                        tempString[index++] = pad + "RECORDING_ENTRY_ZOOM_X";
                        break;

                    case RECORDING_ENTRY_ZOOM_Y:
                        tempString[index++] = pad + "RECORDING_ENTRY_ZOOM_Y";
                        break;

                    case RECORDING_ENTRY_ZOOM_Z:
                        tempString[index++] = pad + "RECORDING_ENTRY_ZOOM_Z";
                        break;

                    case RECORDING_ENTRY_SAMPLE_0X:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_0X";
                        break;

                    case RECORDING_ENTRY_SAMPLE_0Y:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_0Y";
                        break;

                    case RECORDING_ENTRY_SAMPLE_0Z:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_0Z";
                        break;

                    case RECORDING_ENTRY_SAMPLE_SPACING:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_SPACING";
                        break;

                    case RECORDING_ENTRY_LINE_SPACING:
                        tempString[index++] = pad + "RECORDING_ENTRY_LINE_SPACING";
                        break;

                    case RECORDING_ENTRY_PLANE_SPACING:
                        tempString[index++] = pad + "RECORDING_ENTRY_PLANE_SPACING";
                        break;

                    case RECORDING_ENTRY_PLANE_WIDTH:
                        tempString[index++] = pad + "RECORDING_ENTRY_PLANE_WIDTH";
                        break;

                    case RECORDING_ENTRY_PLANE_HEIGHT:
                        tempString[index++] = pad + "RECORDING_ENTRY_PLANE_HEIGHT";
                        break;

                    case RECORDING_ENTRY_VOLUME_DEPTH:
                        tempString[index++] = pad + "RECORDING_ENTRY_VOLUME_DEPTH";
                        break;

                    case RECORDING_ENTRY_ROTATION:
                        tempString[index++] = pad + "RECORDING_ENTRY_ROTATION";
                        break;

                    case RECORDING_ENTRY_NUTATION:
                        tempString[index++] = pad + "RECORDING_ENTRY_NUTATION";
                        break;

                    case RECORDING_ENTRY_PRECESSION:
                        tempString[index++] = pad + "RECORDING_ENTRY_PRECESSION";
                        break;

                    case RECORDING_ENTRY_SAMPLE_0TIME:
                        tempString[index++] = pad + "RECORDING_ENTRY_SAMPLE_0TIME";
                        break;

                    case RECORDING_ENTRY_START_SCAN_TRIGGER_IN:
                        tempString[index++] = pad + "RECORDING_ENTRY_START_SCAN_TRIGGER_IN";
                        break;

                    case RECORDING_ENTRY_START_SCAN_TRIGGER_OUT:
                        tempString[index++] = pad + "RECORDING_ENTRY_START_SCAN_TRIGGER_OUT";
                        break;

                    case RECORDING_ENTRY_START_SCAN_EVENT:
                        tempString[index++] = pad + "RECORDING_ENTRY_START_SCAN_EVENT";
                        break;

                    case RECORDING_ENTRY_START_SCAN_TIME:
                        tempString[index++] = pad + "RECORDING_ENTRY_START_SCAN_TIME";
                        break;

                    case RECORDING_ENTRY_STOP_SCAN_TRIGGER_IN:
                        tempString[index++] = pad + "RECORDING_ENTRY_STOP_SCAN_TRIGGER_IN";
                        break;

                    case RECORDING_ENTRY_STOP_SCAN_TRIGGER_OUT:
                        tempString[index++] = pad + "RECORDING_ENTRY_STOP_SCAN_TRIGGER_OUT";
                        break;

                    case RECORDING_ENTRY_STOP_SCAN_EVENT:
                        tempString[index++] = pad + "RECORDING_ENTRY_STOP_SCAN_EVENT";
                        break;

                    case RECORDING_ENTRY_STOP_SCAN_TIME:
                        tempString[index++] = pad + "RECORDING_ENTRY_STOP_SCAN_TIME";
                        break;

                    case RECORDING_ENTRY_USE_ROIS:
                        tempString[index++] = pad + "RECORDING_ENTRY_USE_ROIS";
                        break;

                    case RECORDING_ENTRY_USE_REDUCED_MEMORY_ROIS:
                        tempString[index++] = pad + "RECORDING_ENTRY_USE_REDUCED_MEMORY_ROIS";
                        break;

                    case RECORDING_ENTRY_USER:
                        tempString[index++] = pad + "RECORDING_ENTRY_USER";
                        break;

                    case RECORDING_ENTRY_USE_BCCORRECTION:
                        tempString[index++] = pad + "RECORDING_ENTRY_USE_BCCORRECTION";
                        break;

                    case RECORDING_ENTRY_POSITION_BCCORRECTION1:
                        tempString[index++] = pad + "RECORDING_ENTRY_POSITION_BCCORRECTION1";
                        break;

                    case RECORDING_ENTRY_POSITION_BCCORRECTION2:
                        tempString[index++] = pad + "RECORDING_ENTRY_POSITION_BCCORRECTION2";
                        break;

                    case RECORDING_ENTRY_INTERPOLATIONY:
                        tempString[index++] = pad + "RECORDING_ENTRY_INTERPOLATIONY";
                        break;

                    case RECORDING_ENTRY_CAMERA_BINNING:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_BINNING";
                        break;

                    case RECORDING_ENTRY_CAMERA_SUPERSAMPLING:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_SUPERSAMPLING";
                        break;

                    case RECORDING_ENTRY_CAMERA_FRAME_WIDTH:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_FRAME_WIDTH";
                        break;

                    case RECORDING_ENTRY_CAMERA_FRAME_HEIGHT:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_FRAME_HEIGHT";
                        break;

                    case RECORDING_ENTRY_CAMERA_OFFSETX:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_OFFSETX";
                        break;

                    case RECORDING_ENTRY_CAMERA_OFFSETY:
                        tempString[index++] = pad + "RECORDING_ENTRY_CAMERA_OFFSETY";
                        break;

                    case TRACK_ENTRY_MULTIPLEX_TYPE:
                        tempString[index++] = pad + "TRACK_ENTRY_MULTIPLEX_TYPE";
                        break;

                    case TRACK_ENTRY_MULTIPLEX_ORDER:
                        tempString[index++] = pad + "TRACK_ENTRY_MULTIPLEX_ORDER";
                        break;

                    case TRACK_ENTRY_SAMPLING_MODE:
                        tempString[index++] = pad + "TRACK_ENTRY_SAMPLING_MODE";
                        break;

                    case TRACK_ENTRY_SAMPLING_METHOD:
                        tempString[index++] = pad + "TRACK_ENTRY_SAMPLING_METHOD";
                        break;

                    case TRACK_ENTRY_SAMPLING_NUMBER:
                        tempString[index++] = pad + "TRACK_ENTRY_SAMPLING_NUMBER";
                        break;

                    case TRACK_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "TRACK_ENTRY_ACQUIRE";
                        break;

                    case TRACK_ENTRY_SAMPLE_OBSERVATION_TIME:
                        tempString[index++] = pad + "TRACK_ENTRY_SAMPLE_OBSERVATION_TIME";
                        break;

                    case TRACK_ENTRY_TIME_BETWEEN_STACKS:
                        tempString[index++] = pad + "TRACK_ENTRY_TIME_BETWEEN_STACKS";
                        break;

                    case TRACK_ENTRY_NAME:
                        tempString[index++] = pad + "TRACK_ENTRY_NAME";
                        break;

                    case TRACK_ENTRY_COLLIMATOR1_NAME:
                        tempString[index++] = pad + "TRACK_ENTRY_COLLIMATOR1_NAME";
                        break;

                    case TRACK_ENTRY_COLLIMATOR1_POSITION:
                        tempString[index++] = pad + "TRACK_ENTRY_COLLIMATOR1_POSITION";
                        break;

                    case TRACK_ENTRY_COLLIMATOR2_NAME:
                        tempString[index++] = pad + "TRACK_ENTRY_COLLIMATOR2_NAME";
                        break;

                    case TRACK_ENTRY_COLLIMATOR2_POSITION:
                        tempString[index++] = pad + "TRACK_ENTRY_COLLIMATOR2_POSITION";
                        break;

                    case TRACK_ENTRY_IS_BLEACH_TRACK:
                        tempString[index++] = pad + "TRACK_ENTRY_IS_BLEACH_TRACK";
                        break;

                    case TRACK_ENTRY_IS_BLEACH_AFTER_SCAN_NUMBER:
                        tempString[index++] = pad + "TRACK_ENTRY_IS_BLEACH_AFTER_SCAN_NUMBER";
                        break;

                    case TRACK_ENTRY_BLEACH_SCAN_NUMBER:
                        tempString[index++] = pad + "TRACK_ENTRY_BLEACH_SCAN_NUMBER";
                        break;

                    case TRACK_ENTRY_TRIGGER_IN:
                        tempString[index++] = pad + "TRACK_ENTRY_TRIGGER_IN";
                        break;

                    case TRACK_ENTRY_TRIGGER_OUT:
                        tempString[index++] = pad + "TRACK_ENTRY_TRIGGER_OUT";
                        break;

                    case TRACK_ENTRY_IS_RATIO_TRACK:
                        tempString[index++] = pad + "TRACK_ENTRY_IS_RATIO_TRACK";
                        break;

                    case TRACK_ENTRY_BLEACH_COUNT:
                        tempString[index++] = pad + "TRACK_ENTRY_BLEACH_COUNT";
                        break;

                    case TRACK_ENTRY_SPI_CENTER_WAVELENGTH:
                        tempString[index++] = pad + "TRACK_ENTRY_SPI_CENTER_WAVELENGTH";
                        break;

                    case TRACK_ENTRY_PIXEL_TIME:
                        tempString[index++] = pad + "TRACK_ENTRY_PIXEL_TIME";
                        break;

                    case TRACK_ENTRY_ID_CONDENSOR_FRONTLENS:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_CONDENSOR_FRONTLENS";
                        break;

                    case TRACK_ENTRY_CONDENSOR_FRONTLENS:
                        tempString[index++] = pad + "TRACK_ENTRY_CONDENSOR_FRONTLENS";
                        break;

                    case TRACK_ENTRY_ID_FIELD_STOP:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_FIELD_STOP";
                        break;

                    case TRACK_ENTRY_FIELD_STOP_VALUE:
                        tempString[index++] = pad + "TRACK_ENTRY_FIELD_STOP_VALUE";
                        break;

                    case TRACK_ENTRY_ID_CONDENSOR_APERTURE:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_CONDENSOR_APERTURE";
                        break;

                    case TRACK_ENTRY_CONDENSOR_APERTURE:
                        tempString[index++] = pad + "TRACK_ENTRY_CONDENSOR_APERTURE";
                        break;

                    case TRACK_ENTRY_ID_CONDENSOR_REVOLVER:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_CONDENSOR_REVOLVER";
                        break;

                    case TRACK_ENTRY_CONDENSOR_FILTER:
                        tempString[index++] = pad + "TRACK_ENTRY_CONDENSOR_FILTER";
                        break;

                    case TRACK_ENTRY_ID_TRANSMISSION_FILTER1:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_TRANSMISSION_FILTER1";
                        break;

                    case TRACK_ENTRY_ID_TRANSMISSION1:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_TRANSMISSION1";
                        break;

                    case TRACK_ENTRY_ID_TRANSMISSION_FILTER2:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_TRANSMISSION_FILTER2";
                        break;

                    case TRACK_ENTRY_ID_TRANSMISSION2:
                        tempString[index++] = pad + "TRACK_ENTRY_ID_TRANSMISSION2";
                        break;

                    case TRACK_ENTRY_REPEAT_BLEACH:
                        tempString[index++] = pad + "TRACK_ENTRY_REPEAT_BLEACH";
                        break;

                    case TRACK_ENTRY_ENABLE_SPOT_BLEACH_POS:
                        tempString[index++] = pad + "TRACK_ENTRY_ENABLE_SPOT_BLEACH_POS";
                        break;

                    case TRACK_ENTRY_SPOT_BLEACH_POSX:
                        tempString[index++] = pad + "TRACK_ENTRY_SPOT_BLEACH_POSX";
                        break;

                    case TRACK_ENTRY_SPOT_BLEACH_POSY:
                        tempString[index++] = pad + "TRACK_ENTRY_SPOT_BLEACH_POSY";
                        break;

                    case TRACK_ENTRY_BLEACH_POSITION_Z:
                        tempString[index++] = pad + "TRACK_ENTRY_BLEACH_POSITION_Z";
                        break;

                    case LASER_ENTRY_NAME:
                        tempString[index++] = pad + "LASER_ENTRY_NAME";
                        break;

                    case LASER_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "LASER_ENTRY_ACQUIRE";
                        break;

                    case LASER_ENTRY_POWER:
                        tempString[index++] = pad + "LASER_ENTRY_POWER";
                        break;

                    case DETCHANNEL_ENTRY_INTEGRATION_MODE:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_INTEGRATION_MODE";
                        break;

                    case DETCHANNEL_ENTRY_SPECIAL_MODE:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_SPECIAL_MODE";
                        break;

                    case DETCHANNEL_ENTRY_DETECTOR_GAIN_FIRST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_DETECTOR_GAIN_FIRST";
                        break;

                    case DETCHANNEL_ENTRY_DETECTOR_GAIN_LAST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_DETECTOR_GAIN_LAST";
                        break;

                    case DETCHANNEL_ENTRY_AMPLIFIER_GAIN_FIRST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_AMPLIFIER_GAIN_FIRST";
                        break;

                    case DETCHANNEL_ENTRY_AMPLIFIER_GAIN_LAST:
                        tempString[index++] = pad + "DETCHANEL_ENTRY_AMPLIFIER_GAIN_LAST";
                        break;

                    case DETCHANNEL_ENTRY_AMPLIFIER_OFFS_FIRST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_AMPLIFIER_OFFS_FIRST";
                        break;

                    case DETCHANNEL_ENTRY_AMPLIFIER_OFFS_LAST:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_AMPLIFIER_OFFS_LAST";
                        break;

                    case DETCHANNEL_ENTRY_PINHOLE_DIAMETER:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_PINHOLE_DIAMETER";
                        break;

                    case DETCHANNEL_ENTRY_COUNTING_TRIGGER:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_COUNTING_TRIGGER";
                        break;

                    case DETCHANNEL_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "DETCHANNEL_ENTRY_ACQUIRE";
                        break;

                    case DETCHANNEL_POINT_DETECTOR_NAME:
                        tempString[index++] = pad + "DETCHANNEL_POINT_DETECTOR_NAME";
                        break;

                    case DETCHANNEL_AMPLIFIER_NAME:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_NAME";
                        break;

                    case DETCHANNEL_PINHOLE_NAME:
                        tempString[index++] = pad + "DETCHANNEL_PINHOLE_NAME";
                        break;

                    case DETCHANNEL_FILTER_SET_NAME:
                        tempString[index++] = pad + "DETCHANNEL_FILTER_SET_NAME";
                        break;

                    case DETCHANNEL_FILTER_NAME:
                        tempString[index++] = pad + "DETCHANNEL_FILTER_NAME";
                        break;

                    case DETCHANNEL_INTEGRATOR_NAME:
                        tempString[index++] = pad + "DETCHANNEL_INTEGRATOR_NAME";
                        break;

                    case DETCHANNEL_DETECTION_CHANNEL_NAME:
                        tempString[index++] = pad + "DETCHANNEL_DETECTION_CHANNEL_NAME";
                        break;

                    case DETCHANNEL_DETECTOR_GAIN_BC1:
                        tempString[index++] = pad + "DETCHANNEL_DETECTOR_GAIN_BC1";
                        break;

                    case DETCHANNEL_DETECTOR_GAIN_BC2:
                        tempString[index++] = pad + "DETCHANNEL_DETECTOR_GAIN_BC2";
                        break;

                    case DETCHANNEL_AMPLIFIER_GAIN_BC1:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_GAIN_BC1";
                        break;

                    case DETCHANNEL_AMPLIFIER_GAIN_BC2:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_GAIN_BC2";
                        break;

                    case DETCHANNEL_AMPLIFIER_OFFSET_BC1:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_OFFSET_BC1";
                        break;

                    case DETCHANNEL_AMPLIFIER_OFFSET_BC2:
                        tempString[index++] = pad + "DETCHANNEL_AMPLIFIER_OFFSET_BC2";
                        break;

                    case DETCHANNEL_SPECTRAL_SCAN_CHANNELS:
                        tempString[index++] = pad + "DETCHANNEL_SPECTRAL_SCAN_CHANNELS";
                        break;

                    case DETCHANNEL_SPI_WAVELENGTH_START:
                        tempString[index++] = pad + "DETCHANNEL_SPI_WAVELENGTH_START";
                        break;

                    case DETCHANNEL_SPI_WAVELENGTH_END:
                        tempString[index++] = pad + "DETCHANNEL_SPI_WAVELENGTH_END";
                        break;

                    case DETCHANNEL_DYE_NAME:
                        tempString[index++] = pad + "DETCHANNEL_DYE_NAME";
                        break;

                    case DETCHANNEL_DYE_FOLDER:
                        tempString[index++] = pad + "DETCHANNEL_DYE_FOLDER";
                        break;

                    case ILLUMCHANNEL_ENTRY_NAME:
                        tempString[index++] = pad + "ILLUMCHANNEL_ENTRY_NAME";
                        break;

                    case ILLUMCHANNEL_ENTRY_POWER:
                        tempString[index++] = pad + "ILLUMCHANNEL_ENTRY_POWER";
                        break;

                    case ILLUMCHANNEL_ENTRY_WAVELENGTH:
                        tempString[index++] = pad + "ILLUMCHANNEL_ENTRY_WAVELENGTH";
                        break;

                    case ILLUMCHANNEL_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "ILLUMCHANNEL_ENTRY_ACQUIRE";
                        break;

                    case ILLUMCHANNEL_DETCHANNEL_NAME:
                        tempString[index++] = pad + "ILLUMCHANNEL_DETCHANNEL_NAME";
                        break;

                    case ILLUMCHANNEL_POWER_BC1:
                        tempString[index++] = pad + "ILLUMCHANNEL_POWER_BC1";
                        break;

                    case ILLUMCHANNEL_POWER_BC2:
                        tempString[index++] = pad + "ILLUMCHANNEL_POWER_BC2";
                        break;

                    case BEAMSPLITTER_ENTRY_FILTER_SET:
                        tempString[index++] = pad + "BEAMSPLITTER_ENTRY_FILTER_SET";
                        break;

                    case BEAMSPLITTER_ENTRY_FILTER:
                        tempString[index++] = pad + "BEAMSPLITTER_ENTRY_FILTER";
                        break;

                    case BEAMSPLITTER_ENTRY_NAME:
                        tempString[index++] = pad + "BEAMSPLITTER_ENTRY_NAME";
                        break;

                    case DATACHANNEL_ENTRY_NAME:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_NAME";
                        break;

                    case DATACHANNEL_ENTRY_COLOR:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_COLOR";
                        break;

                    case DATACHANNEL_ENTRY_SAMPLETYPE:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_SAMPLETYPE";
                        break;

                    case DATACHANNEL_ENTRY_BITSPERSAMPLE:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_BITSPERSAMPLE";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_TYPE:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_TYPE";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_TRACK1:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_TRACK1";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CHANNEL1:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CHANNEL1";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_TRACK2:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_TRACK2";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CHANNEL2:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CHANNEL2";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST1:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST1";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST2:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST2";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST3:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST3";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST4:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST4";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST5:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST5";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_CONST6:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_CONST6";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES1:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES1";
                        break;

                    case DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES2:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_RATIO_FIRST_IMAGES2";
                        break;

                    case DATACHANNEL_ENTRY_DYE_NAME:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_DYE_NAME";
                        break;

                    case DATACHANNEL_ENTRY_DYE_FOLDER:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_DYE_FOLDER";
                        break;

                    case DATACHANNEL_ENTRY_SPECTRUM:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_SPECTRUM";
                        break;

                    case DATACHANNEL_ENTRY_ACQUIRE:
                        tempString[index++] = pad + "DATACHANNEL_ENTRY_ACQUIRE";
                        break;

                    case TIMER_ENTRY_NAME:
                        tempString[index++] = pad + "TIMER_ENTRY_NAME";
                        break;

                    case TIMER_ENTRY_DESCRIPTION:
                        tempString[index++] = pad + "TIMER_ENTRY_DESCRIPTION";
                        break;

                    case TIMER_ENTRY_INTERVAL:
                        tempString[index++] = pad + "TIMER_ENTRY_INTERVAL";
                        break;

                    case TIMER_ENTRY_TRIGGER_IN:
                        tempString[index++] = pad + "TIMER_ENTRY_TRIGGER_IN";
                        break;

                    case TIMER_ENTRY_TRIGGER_OUT:
                        tempString[index++] = pad + "TIMER_ENTRY_TRIGGER_OUT";
                        break;

                    case TIMER_ENTRY_ACTIVATION_TIME:
                        tempString[index++] = pad + "TIMER_ENTRY_ACTIVATION_TIME";
                        break;

                    case TIMER_ENTRY_ACTIVATION_NUMBER:
                        tempString[index++] = pad + "TIMER_ENTRY_ACTIVATION_NUMBER";
                        break;

                    case MARKER_ENTRY_NAME:
                        tempString[index++] = pad + "MARKER_ENTRY_NAME";
                        break;

                    case MARKER_ENTRY_DESCRIPTION:
                        tempString[index++] = pad + "MARKER_ENTRY_DESCRIPTION";
                        break;

                    case MARKER_ENTRY_TRIGGER_IN:
                        tempString[index++] = pad + "MARKER_ENTRY_TRIGGER_IN";
                        break;

                    case MARKER_ENTRY_TRIGGER_OUT:
                        tempString[index++] = pad + "MARKER_ENTRY_TRIGGER_OUT";
                        break;

                    default:
                        // long longEntry = (long)entry;
                        // longEntry = 0x00000000ffffffffL & longEntry;
                        // System.out.println("entry = " + Long.toString(longEntry,16));
                } // switch(entry)
            }

            if (scanType == TYPE_LONG) {
                nLONG = scanSize / 4;
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                for (i = 0; i < nLONG; i++) {
                    intValue = getInt(endianess);

                    switch (entry) {

                        case RECORDING_ENTRY_START_SCAN_EVENT:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Button (normal operation)";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Trigger slow - Scanner off" +
                                                          ", PMT high voltage off, reaction 300 msec";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "Trigger normal - Scanner " +
                                                          "off, PMT high voltage on, reaction 30 msec";
                                    break;

                                case 3:
                                    tempString[index++] = pad + "Trigger fast - Scanner on," +
                                                          " PMT high voltage on, reaction 5 msec";
                                    break;

                                case 4:
                                    tempString[index++] = pad + "Start time";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case RECORDING_ENTRY_STOP_SCAN_EVENT:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Button (normal operation button)";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Trigger";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "End time";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case RECORDING_ENTRY_USE_ROIS:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "ROIs should not be used for the scan process";
                                    break;

                                default:
                                    tempString[index++] = pad + "ROIs should be used for the scan process";
                            }

                            break;

                        case RECORDING_ENTRY_USE_REDUCED_MEMORY_ROIS:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Scan memory was provided for the whole plane";
                                    break;

                                default:
                                    tempString[index++] = pad + "Scan memory was provided " +
                                                          "only for the ROI bounding rectangle";
                            }

                            break;

                        case RECORDING_ENTRY_INTERPOLATIONY:
                            switch (intValue) {

                                case 1:
                                    tempString[index++] = pad + "No interpolation was done";
                                    break;

                                default:
                                    tempString[index++] = pad + "Scan 1 out of every " + String.valueOf(intValue) +
                                                          " lines";
                            }

                            break;

                        case RECORDING_ENTRY_CAMERA_SUPERSAMPLING:
                            switch (intValue) {

                                case 0:
                                case 1:
                                    tempString[index++] = pad + "No technique was used " +
                                                          "to enlarge the number of pixels";
                                    break;

                                default:
                                    tempString[index++] = pad + "Horizontal and vertical " +
                                                          "enlargement of pixel number by factor of " +
                                                          String.valueOf(intValue);
                            }

                            break;

                        case TRACK_ENTRY_MULTIPLEX_TYPE:
                            switch (intValue) {

                                case 2:
                                    tempString[index++] = pad + "A switch to the next track is done afer a stack";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "A switch to the next track is done after a plane";
                                    break;

                                case 0:
                                    tempString[index++] = pad + "A switch to the next track is done after a line";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case TRACK_ENTRY_SAMPLING_MODE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Sample";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Line-Average";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "Frame-Average";
                                    break;

                                case 3:
                                    tempString[index++] = pad + "Integration mode";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case TRACK_ENTRY_SAMPLING_METHOD:
                            switch (intValue) {

                                case 1:
                                    tempString[index++] = pad + "Mean";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "Sum";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case TRACK_ENTRY_ACQUIRE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "The track was not used during scan";
                                    break;

                                default:
                                    tempString[index++] = pad + "The track was used during scan";
                            }

                            break;

                        case TRACK_ENTRY_IS_BLEACH_TRACK:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "The track setting does not specify bleach parameters";
                                    break;

                                default:
                                    tempString[index++] = pad + "The track setting does specify bleach parameters " +
                                                          "and has no active detection channels";
                            }

                            break;

                        case TRACK_ENTRY_CONDENSOR_FRONTLENS:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "No condensor front lens was in the beam path";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "The condensor front lens was in the beam path";
                                    break;
                            }

                            break;

                        case LASER_ENTRY_ACQUIRE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "The laser was off";
                                    break;

                                default:
                                    tempString[index++] = pad + "The laser was on";
                            }

                            break;

                        case DETCHANNEL_ENTRY_INTEGRATION_MODE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Integration mode";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Photon counting mode";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case DETCHANNEL_ENTRY_ACQUIRE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "Detection channel was not used during scan";
                                    break;

                                default:
                                    tempString[index++] = pad + "Detection channel was used during scan";
                            }

                            break;

                        case ILLUMCHANNEL_ENTRY_ACQUIRE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "The attenuator was disabled";
                                    break;

                                default:
                                    tempString[index++] = pad + "The attenuator was enabled";
                            }

                            break;

                        case DATACHANNEL_ENTRY_COLOR:
                            redValue = intValue & 0x000000ff;
                            greenValue = (intValue >> 8) & 0x000000ff;
                            blueValue = (intValue >> 16) & 0x000000ff;
                            tempString[index++] = pad + "red = " + String.valueOf(redValue);
                            tempString[index++] = pad + "green = " + String.valueOf(greenValue);
                            tempString[index++] = pad + "blue = " + String.valueOf(blueValue);
                            break;

                        case DATACHANNEL_ENTRY_SAMPLETYPE:
                            switch (intValue) {

                                case 1:
                                    tempString[index++] = pad + "8 bit channel";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "12 bit channel";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case DATACHANNEL_ENTRY_BITSPERSAMPLE:
                            switch (intValue) {

                                case 8:
                                    tempString[index++] = pad + "8 bit channel";
                                    break;

                                case 12:
                                    tempString[index++] = pad + "12 bit channel";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        case DATACHANNEL_ENTRY_RATIO_TYPE:
                            switch (intValue) {

                                case 0:
                                    tempString[index++] = pad + "No online calculation - " +
                                                          "the data channel receives raw scan data";
                                    break;

                                case 1:
                                    tempString[index++] = pad + "Online ratio: " + "(S1 + C1)/(S2 + C2)*C3+C4";
                                    break;

                                case 2:
                                    tempString[index++] = pad + "Online subtraction: " + "(S1*C1 - S2*C3)/C2 + C4";
                                    break;

                                case 3:
                                    tempString[index++] = pad + "Online ratio: " + "(S1-S2*C1)/(S1+S2*C2)*C3+C4";
                                    break;

                                case 4:
                                    tempString[index++] = pad + "Online hill function: " + "C3*(C4/C5)*(S1-C1)/(C2-S2)";
                                    break;

                                case 5:
                                    tempString[index++] = pad + "Online reference ratio: " + "(S1-S2+C1)/(S2-C2)*C3+C4";
                                    break;

                                case 6:
                                    tempString[index++] = pad + "Online linear unmixing";
                                    break;

                                default:
                                    tempString[index++] = pad + String.valueOf(intValue);
                            }

                            break;

                        default:
                            tempString[index++] = pad + String.valueOf(intValue);
                    }
                }
            } // if (scanType == TYPE_LONG)
            else if (scanType == TYPE_RATIONAL) {
                nRATIONAL = scanSize / 8;
                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                for (i = 0; i < nRATIONAL; i++) {
                    doubleValue = getDouble(endianess);
                    tempString[index++] = pad + String.valueOf(doubleValue);
                }
            } // else if (scanType == TYPE_RATIONAL) {
            else if ((scanType == TYPE_ASCII) && (scanSize > 0)) {
                description = new byte[scanSize];

                for (i = 0; i < scanSize; i++) {
                    description[i] = raFile.readByte();
                }

                endPos = 0;
                foundEnd = false;

                for (j = scanSize - 1; (j >= 0) && (!foundEnd); j--) {

                    if (description[j] > 0x20) {
                        endPos = j;
                        foundEnd = true;
                    }
                }

                startPos = 0;
                foundStart = false;

                for (j = 0; (j < endPos) && (!foundStart); j++) {

                    if (description[j] > 0x20) {
                        startPos = j;
                        foundStart = true;
                    }
                }

                pad = new String("");

                for (i = 0; i < level; i++) {
                    pad = pad + "    ";
                }

                if (endPos > 0) {
                    tempString[index++] = pad + new String(description, startPos, endPos - startPos + 1);
                }
            }
        } while (level > 0);

        for (i = 0; i < (index - 1); i++) {
            Preferences.debug(tempString[i] + "\n", Preferences.DEBUG_FILEIO);
        }
    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   image slice
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readTileBuffer(int slice, float[] buffer) throws IOException {
        int a, i, j;
        int iCount, iNext;
        int b1, b2, b3, b4;
        long progress, progressLength, mod;
        int nBytes;
        int nLength;
        int xTile, yTile;
        int x, y;

        int counter = 0;

        i = 0;
        xTile = 0;
        yTile = 0;
        x = 0;
        y = 0;

        for (a = 0; a < tilesPerSlice; a++) {

            try {
                raFile.seek(tileOffsets[(slice * tilesPerSlice) + a]);
                nBytes = tileByteCounts[(slice * tilesPerSlice) + a];

                switch (fileInfo.getDataType()) {

                    case ModelStorageBase.BOOLEAN:
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        nLength = 8 * ((nBytes + 63) >> 6); // new BitSet(size) = new long[(size+63)>>6];

                        if (lzwCompression) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[tileMaxByteCount];
                            }

                            raFile.read(byteBuffer, 0, nLength);

                            if (decomp == null) {
                                decomp = new byte[tileWidth * tileLength];
                            }

                            lzwDecoder.decode(byteBuffer, decomp, tileLength);

                            for (j = 0; j < decomp.length; j++) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    buffer[x + (y * xDim)] = decomp[j >> 3] & (1 << (7-(j % 8)));
                                    i++;

                                }

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            }
                        } else {
                            byteBuffer = new byte[nLength];
                            raFile.read(byteBuffer, 0, nLength);

                            for (j = 0; j < nBytes; j++) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    buffer[x + (y * xDim)] = byteBuffer[j >> 3] & (1 << (7-(j % 8)));
                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j++)
                        }

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.BYTE:
                        if (packBit == false) {

                            if (byteBuffer == null) {

                                if (lzwCompression) {
                                    byteBuffer = new byte[tileMaxByteCount];
                                } else {
                                    byteBuffer = new byte[nBytes];
                                }
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;


                            if (lzwCompression) {

                                if (decomp == null) {
                                    decomp = new byte[tileWidth * tileLength];
                                }

                                lzwDecoder.decode(byteBuffer, decomp, tileLength);

                                for (j = 0; j < decomp.length; j++) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[x + (y * xDim)] = decomp[j] & 0xff;
                                        i++;

                                    }

                                    x++;

                                    if (x == ((xTile + 1) * tileWidth)) {
                                        x = xTile * tileWidth;
                                        y++;
                                    }
                                }
                            } else {

                                for (j = 0; j < nBytes; j++) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[x + (y * xDim)] = byteBuffer[j];
                                        i++;
                                    } // if ((x < xDim) && (y < yDim))

                                    x++;

                                    if (x == ((xTile + 1) * tileWidth)) {
                                        x = xTile * tileWidth;
                                        y++;
                                    }
                                } // for (j = 0; j < nBytes; j++)
                            }

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // if (packBit == false)
                        else if (packBit == true) {
                            byteBuffer = new byte[nBytes];
                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j] & 0x80) == 0) {
                                    iCount = byteBuffer[j] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[x + (y * xDim)] = byteBuffer[j];
                                            i++;
                                        } // if ((x < xDim) && (y < yDim))

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        } // if (x == (xTile+1)*tileWidth)
                                    } // for (iNext = 0; iNext < iCount; iNext++,j++)
                                } // end of if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[x + (y * xDim)] = byteBuffer[j];
                                        } // if ((x < xDim) && (y < yDim))

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        } // if (x == (xTile+1)*tileWidth)
                                    } // for (iNext = 0; iNext < iCount; iNext++,i++)

                                    j++;
                                } // end of else for compressed data bytes
                            } // end of while (j < nBytes)

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // else if (packBit == true)

                        break;

                    case ModelStorageBase.UBYTE:
                        if (packBit == false) {

                            if (byteBuffer == null) {

                                if (lzwCompression) {
                                    byteBuffer = new byte[tileMaxByteCount];
                                } else {
                                    byteBuffer = new byte[nBytes];
                                }

                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;


                            if (lzwCompression) {

                                if (decomp == null) {
                                    decomp = new byte[tileWidth * tileLength];
                                }

                                lzwDecoder.decode(byteBuffer, decomp, tileLength);

                                for (j = 0; j < decomp.length; j++) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[x + (y * xDim)] = decomp[j] & 0xff;
                                        i++;

                                    }

                                    x++;

                                    if (x == ((xTile + 1) * tileWidth)) {
                                        x = xTile * tileWidth;
                                        y++;
                                    }
                                }
                            } else {

                                for (j = 0; j < nBytes; j++) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[x + (y * xDim)] = byteBuffer[j] & 0xff;
                                        i++;
                                    } // if ((x < xDim) && (y < yDim))

                                    x++;

                                    if (x == ((xTile + 1) * tileWidth)) {
                                        x = xTile * tileWidth;
                                        y++;
                                    }
                                } // for (j = 0; j < nBytes; j++)
                            }

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // if (packBit == false)
                        else if (packBit == true) {
                            byteBuffer = new byte[nBytes];
                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j] & 0x80) == 0) {
                                    iCount = byteBuffer[j] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[x + (y * xDim)] = byteBuffer[j] & 0xff;
                                            i++;
                                        } // if ((x < xDim) && (y < yDim))

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        } // if (x == (xTile+1)*tileWidth)
                                    } // for (iNext = 0; iNext < iCount; iNext++,j++)
                                } // if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[x + (y * xDim)] = byteBuffer[j] & 0xff;
                                        } // if ((x < xDim) && (y < yDim))

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        } // if (x == (xTile+1)*tileWidth)
                                    } // for (iNext = 0; iNext < iCount; iNext++,i++)

                                    j++;
                                } // else for compressed data bytes
                            } // while (j < nBytes)

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // else if (packBit == true)

                        break;

                    case ModelStorageBase.SHORT:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[nBytes];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        for (j = 0; j < nBytes; j += 2) {

                            if ((x < xDim) && (y < yDim)) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                b1 = getUnsignedByte(byteBuffer, j);
                                b2 = getUnsignedByte(byteBuffer, j + 1);

                                if (endianess) {
                                    buffer[x + (y * xDim)] = (short) ((b1 << 8) + b2);
                                } else {
                                    buffer[x + (y * xDim)] = (short) ((b2 << 8) + b1);
                                }

                                i++;
                            } // if ((x < xDim) && (y < yDim))

                            x++;

                            if (x == ((xTile + 1) * tileWidth)) {
                                x = xTile * tileWidth;
                                y++;
                            }
                        } // for (j = 0; j < nBytes; j+= 2)

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.USHORT:
                        if (byteBuffer == null) {

                            if (lzwCompression) {
                                byteBuffer = new byte[tileMaxByteCount];
                            } else {
                                byteBuffer = new byte[nBytes];
                            }
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;


                        if (lzwCompression) {

                            if (decomp == null) {
                                decomp = new byte[tileWidth * tileLength * 3];
                            }

                            lzwDecoder.decode(byteBuffer, decomp, tileLength);

                            for (j = 0; j < decomp.length; j += 2) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(decomp, j);
                                    b2 = getUnsignedByte(decomp, j + 1);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = ((b1 << 8) + b2);
                                    } else {
                                        buffer[x + (y * xDim)] = ((b2 << 8) + b1);
                                    }
                                }

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            }
                        } else {

                            for (j = 0; j < nBytes; j += 2) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(byteBuffer, j);
                                    b2 = getUnsignedByte(byteBuffer, j + 1);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = ((b1 << 8) + b2);
                                    } else {
                                        buffer[x + (y * xDim)] = ((b2 << 8) + b1);
                                    }

                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j+= 2)
                        }

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.INTEGER:
                        if (byteBuffer == null) {

                            if (lzwCompression) {
                                byteBuffer = new byte[tileMaxByteCount];
                            } else {
                                byteBuffer = new byte[nBytes];
                            }
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;


                        if (lzwCompression) {

                            if (decomp == null) {
                                decomp = new byte[tileWidth * tileLength * 3];
                            }

                            lzwDecoder.decode(byteBuffer, decomp, tileLength);

                            for (j = 0; j < decomp.length; j += 4) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(decomp, j);
                                    b2 = getUnsignedByte(decomp, j + 1);
                                    b3 = getUnsignedByte(decomp, j + 2);
                                    b4 = getUnsignedByte(decomp, j + 3);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                                    } else {
                                        buffer[x + (y * xDim)] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                                    }

                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < decomp.length; j+= 4)

                        } else {

                            for (j = 0; j < nBytes; j += 4) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(byteBuffer, j);
                                    b2 = getUnsignedByte(byteBuffer, j + 1);
                                    b3 = getUnsignedByte(byteBuffer, j + 2);
                                    b4 = getUnsignedByte(byteBuffer, j + 3);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                                    } else {
                                        buffer[x + (y * xDim)] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                                    }

                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j+= 4)
                        }

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.UINTEGER:
                        if (byteBuffer == null) {

                            if (lzwCompression) {
                                byteBuffer = new byte[tileMaxByteCount];
                            } else {
                                byteBuffer = new byte[nBytes];
                            }
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;


                        if (lzwCompression) {

                            if (decomp == null) {
                                decomp = new byte[tileWidth * tileLength * 3];
                            }

                            lzwDecoder.decode(byteBuffer, decomp, tileLength);

                            for (j = 0; j < decomp.length; j += 4) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(decomp, j);
                                    b2 = getUnsignedByte(decomp, j + 1);
                                    b3 = getUnsignedByte(decomp, j + 2);
                                    b4 = getUnsignedByte(decomp, j + 3);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = (((long) b1 << 24) | ((long) b2 << 16) |
                                                                      ((long) b3 << 8) | (long) b4); // Big Endian
                                    } else {
                                        buffer[x + (y * xDim)] = (((long) b4 << 24) | ((long) b3 << 16) |
                                                                      ((long) b2 << 8) | (long) b1); // Little Endian
                                    }

                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < decomp.length; j+= 4)

                        } else {

                            for (j = 0; j < nBytes; j += 4) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(byteBuffer, j);
                                    b2 = getUnsignedByte(byteBuffer, j + 1);
                                    b3 = getUnsignedByte(byteBuffer, j + 2);
                                    b4 = getUnsignedByte(byteBuffer, j + 3);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = (((long) b1 << 24) | ((long) b2 << 16) |
                                                                      ((long) b3 << 8) | (long) b4); // Big Endian
                                    } else {
                                        buffer[x + (y * xDim)] = (((long) b4 << 24) | ((long) b3 << 16) |
                                                                      ((long) b2 << 8) | (long) b1); // Little Endian
                                    }

                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j+= 4)
                        }

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.ARGB:
                        if (chunky == true) {

                            if (byteBuffer == null) {

                                if (lzwCompression) {
                                    byteBuffer = new byte[tileMaxByteCount];
                                } else {
                                    byteBuffer = new byte[nBytes];
                                }
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;


                            if (lzwCompression) {
                                // System.err.println("Read " + nBytes + " from raFile");

                                if (decomp == null) {
                                    decomp = new byte[tileWidth * tileLength * 3];
                                }

                                lzwDecoder.decode(byteBuffer, decomp, tileLength);
                                // System.err.println("Decoded byte length: " + decomp.length);

                                for (j = 0; j < decomp.length; j += 3) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[4 * (x + (y * xDim))] = 255;
                                        buffer[(4 * (x + (y * xDim))) + 1] = getUnsignedByte(decomp, j + redOffset);
                                        buffer[(4 * (x + (y * xDim))) + 2] = getUnsignedByte(decomp, j + greenOffset);
                                        buffer[(4 * (x + (y * xDim))) + 3] = getUnsignedByte(decomp, j + blueOffset);
                                        i++;

                                    }

                                    x++;

                                    if (x == ((xTile + 1) * tileWidth)) {
                                        x = xTile * tileWidth;
                                        y++;
                                    }
                                }
                            } else {

                                for (j = 0; j < nBytes; j += 3) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[4 * (x + (y * xDim))] = 255;
                                        buffer[(4 * (x + (y * xDim))) + 1] = getUnsignedByte(byteBuffer, j + redOffset);
                                        buffer[(4 * (x + (y * xDim))) + 2] = getUnsignedByte(byteBuffer,
                                                                                             j + greenOffset);
                                        buffer[(4 * (x + (y * xDim))) + 3] = getUnsignedByte(byteBuffer,
                                                                                             j + blueOffset);
                                        i++;
                                    } // if ((x < xDim) && (y < yDim))

                                    x++;

                                    if (x == ((xTile + 1) * tileWidth)) {
                                        x = xTile * tileWidth;
                                        y++;
                                    }
                                } // for (j = 0; j < nBytes; j+= 3)
                            }

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // if (chunky == true)
                        else {

                            if (byteBuffer == null) {

                                if (lzwCompression) {
                                    byteBuffer = new byte[tileMaxByteCount];
                                } else {
                                    byteBuffer = new byte[nBytes];
                                }
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim * 3;
                            progressLength = imageSlice * xDim * yDim * 3;
                            mod = progressLength / 100;


                            if (lzwCompression) {
                                // System.err.println("Read " + nBytes + " from raFile");

                                if (decomp == null) {
                                    decomp = new byte[tileWidth * tileLength];
                                }

                                lzwDecoder.decode(byteBuffer, decomp, tileLength);
                                // System.err.println("Decoded byte length: " + decomp.length);

                                // System.err.println("A is: " + a);
                                // System.err.println("Decomp length is: " + decomp.length);
                                // System.err.println("I is: " + i);


                                for (j = 0; j < decomp.length; j++, i += 4, counter++) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((counter + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (counter + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = 255;
                                        buffer[i + a + 1] = getUnsignedByte(decomp, j);
                                    }
                                }

                                i = 0;

                            }

                        }

                        break;
                } // switch(fileInfo.getDataType())
            } // try
            catch (OutOfMemoryError error) {
                System.gc();
                throw error;
            }
        } // for (i = 0; i < tilesPerSlice; i++)
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Simple class to store image offsets and bytes located at the offset.
     */
    private class Index {

        /** DOCUMENT ME! */
        public int byteCount = 0;

        /** DOCUMENT ME! */
        public long index = 0;

        /**
         * Creates a new Index object.
         *
         * @param  _index  DOCUMENT ME!
         */
        public Index(long _index) {
            index = _index;
        }
    }

}
