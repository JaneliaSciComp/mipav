package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.zip.*;

import WildMagic.LibFoundation.Mathematics.Matrix4f;


/**
 * The class reads MGH files. The 4 by 4 matrix in MGH transforms x,y,z indexes to (right, anterior, superior)
 * coordinates where +x = Right, +y = Anterior, +z = Superior. In MIPAV the 4 by 4 matrix does not imply the axis
 * orientations.
 *
 * <p>The MGH definition is: [right] = [xr yr zr] [ xsize * x] + [rorigin] [anterior] = [xa ya za] [ ysize * y] + [aorigin]
 * [superior] = [xs ys zs] [ zsize * z] + [sorigin] Now in going to MIPAV a change must occur. MGH has L->R and P->A while
 * MIPAV uses R->L in all axes and A->P in axes 0 and 1 and P->A in axis 2, so this would cause xr, yr, zr, rorigin,
 * xa, ya, za, and aorigin to be multiplied by -1. cr, ca, and cs refer to the center point of the scanner coordinate system.</p>
 *
 * <p>A 284 byte header always precedes the data buffer In version 1 if the goodRASFlag <= 0, then only the first 30
 * bytes are read. If the goodRASFlag > 0, then the first 90 bytes are read</p>
 *
 * <p>Immediately after the data buffer other optional data structures may be found: They are: Recovery time in
 * milliseconds float Flip angle in radians float Echo time in milliseconds float Inversion time in millseconds float
 * Field of view in millimeters float Comment about fov field from Nick Schmansky: The FoV field should be ignored. In
 * discussing this field with Bruce Fischl, and looking more closely at the code, it appears to be a field with a long
 * and inconsistent usage history (in how it is set and where the data originates). We do not rely on it in our
 * binaries. Lastly, tags including the Talairach transform file name and a list of commands used to create this
 * data(provenance info) may be present.</p>
 *
 * <p>I am not reading the contents of the Talairach transform file for 3 reasons: 1.) This information is probably not
 * very useful. 2.) Every sample file that I obtained from MGH gave the same name: talairach.xfm. If this name is always
 * used, then it is impossible to tell which .mgh file it corresponds to. 3.) The Talairach transform file is a clear
 * ascii file, so it can be easily read with any number of common programs.</p>
 *
 * @see  FileIO
 * @see  FileInfoMGH
 * @see  FileRaw
 */

public class FileMGH extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int MRI_UCHAR = 0;

    /** DOCUMENT ME! */
    private static final int MRI_INT = 1;

    /** DOCUMENT ME! */
    private static final int MRI_LONG = 2;

    /** DOCUMENT ME! */
    private static final int MRI_FLOAT = 3;

    /** DOCUMENT ME! */
    private static final int MRI_SHORT = 4;

    /** DOCUMENT ME! */
    private static final int MRI_BITMAP = 5;

    /** DOCUMENT ME! */
    private static final int MRI_TENSOR = 6;

    /** DOCUMENT ME! */
    private static final int TAG_OLD_COLORTABLE = 1;

    /** DOCUMENT ME! */
    private static final int TAG_OLD_USEREALRAS = 2;

    /** DOCUMENT ME! */
    private static final int TAG_CMDLINE = 3;

    /** DOCUMENT ME! */
    private static final int TAG_OLD_SURF_GEOM = 20;

    /** DOCUMENT ME! */
    private static final int TAG_OLD_MGH_XFORM = 30;

    /** DOCUMENT ME! */
    private static final int TAG_MGH_XFORM = 31;

    /** DOCUMENT ME! */
    private static final int MAX_CMDS = 1000;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */


    String[] cmdlines = new String[MAX_CMDS];

    /** DOCUMENT ME! */
    int ncmds = 0;

    /** DOCUMENT ME! */
    private int[] axisOrientation;

    /** DOCUMENT ME! */
    private int bytesPerValue;

    /** DOCUMENT ME! */
    private float ca;

    /** DOCUMENT ME! */
    private float cr;

    /** DOCUMENT ME! */
    private float cs;

    /** DOCUMENT ME! */
    private int dataType = ModelStorageBase.UBYTE;

    /** DOCUMENT ME! */
    private int depth;

    /** DOCUMENT ME! */
    private int dof;

    /** DOCUMENT ME! */
    private int[] extents;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoMGH fileInfo = null;

    /** DOCUMENT ME! */
    private long fileLength;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private FileInputStream fis;

    /** DOCUMENT ME! */
    private float flipAngle;

    /** DOCUMENT ME! */
    private float fov;

    /** DOCUMENT ME! */
    private short goodRASFlag;

    /** DOCUMENT ME! */
    private boolean gunzip;

    /** DOCUMENT ME! */
    private int height;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private TransMatrix matrix = new TransMatrix(4);

    /** DOCUMENT ME! */
    private int mghType;

    /** DOCUMENT ME! */
    private int nFrames;

    /** DOCUMENT ME! */
    private long optionalStructuresLocation;

    /** DOCUMENT ME! */
    private float[] origin;

    /** DOCUMENT ME! */
    private float[] resolutions = new float[3];

    /** DOCUMENT ME! */
    private float te;

    /** DOCUMENT ME! */
    private float ti;

    /** DOCUMENT ME! */
    private float tr;

    /** Present version number is 1. */
    private int version;

    /** DOCUMENT ME! */
    private int width;

    /** DOCUMENT ME! */
    private float xa;

    /** DOCUMENT ME! */
    private float xr;

    /** DOCUMENT ME! */
    private float xs;

    /** DOCUMENT ME! */
    private float ya;

    /** DOCUMENT ME! */
    private float yr;

    /** DOCUMENT ME! */
    private float ys;

    /** DOCUMENT ME! */
    private float za;

    /** DOCUMENT ME! */
    private float zr;

    /** DOCUMENT ME! */
    private float zs;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileMGH(String fName, String fDir) {
        fileName = fName;
        fileDir = fDir;
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
        if (cmdlines != null) {
            for (i = 0; i < cmdlines.length; i++) {
                cmdlines[i] = null;
            }
            cmdlines = null;
        }
        axisOrientation = null;
        extents = null;
        matrix = null;
        origin = null;
        resolutions = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Returns the FileInfoMGH read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoMGH getFileInfo() {
        return fileInfo;
    }

    /**
     * Reads the MGH header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoMGH
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {
        int j;
        int index;
        boolean endianess;
        long bytesLeft;
        int tag;
        long pLen;
        String transformFileName = null;
        int i;
        boolean okay;
        String str;
        byte[] buf;

        index = fileName.lastIndexOf(".");

        if ((fileName.substring(index + 1).equalsIgnoreCase("mgz")) ||
                (fileName.substring(index + 1).equalsIgnoreCase("gz"))) {
            gunzip = true;
        } else {
            gunzip = false;
        }

        file = new File(fileDir + fileName);

        if (gunzip) {
            int totalBytesRead = 0;
            fireProgressStateChanged("Uncompressing GZIP file ...");

            fis = new FileInputStream(file);

            GZIPInputStream gzin = new GZIPInputStream(new BufferedInputStream(fis));

            if (fileName.substring(index + 1).equalsIgnoreCase("mgz")) {
                fileName = fileName.substring(0, index).concat(".mgh");
            } else {
                fileName = fileName.substring(0, index);
            }

            String uncompressedName = fileDir + fileName;
            FileOutputStream out = new FileOutputStream(uncompressedName);
            byte[] buffer = new byte[256];

            while (true) {
                int bytesRead = gzin.read(buffer);

                if (bytesRead == -1) {
                    break;
                }

                totalBytesRead += bytesRead;
                out.write(buffer, 0, bytesRead);
            }

            out.close();
            file = new File(uncompressedName);
            fileInfo.setFileName(fileName);
        } // if (gunzip)

        raFile = new RandomAccessFile(file, "r");
        fileLength = raFile.length();

        // MGH file are always BIG_ENDIAN
        endianess = BIG_ENDIAN;
        fileInfo.setEndianess(BIG_ENDIAN);

        // Current value of version number is 1
        version = getInt(endianess); // 0
        fileInfo.setVersion(version);

        // First dimension of the image buffer
        width = getInt(endianess); // 4

        // Second dimension of the image buffer
        height = getInt(endianess); // 8

        // Third dimension of the image buffer
        depth = getInt(endianess); // 12

        // Fourth dimension of the image buffer
        nFrames = getInt(endianess); // 16

        if (nFrames > 1) {
            extents = new int[4];
            extents[0] = width;
            extents[1] = height;
            extents[2] = depth;
            extents[3] = nFrames;
        } // if (nFrames > 1)
        else if (depth > 1) {
            extents = new int[3];
            extents[0] = width;
            extents[1] = height;
            extents[2] = depth;
        } else {
            extents = new int[2];
            extents[0] = width;
            extents[1] = height;
        }

        fileInfo.setExtents(extents);

        mghType = getInt(endianess); // 20

        switch (mghType) {

            case MRI_UCHAR:
                dataType = ModelStorageBase.UBYTE;
                bytesPerValue = 1;
                break;

            case MRI_SHORT:
                dataType = ModelStorageBase.SHORT;
                bytesPerValue = 2;
                break;

            case MRI_INT:
                dataType = ModelStorageBase.INTEGER;
                bytesPerValue = 4;
                break;

            case MRI_LONG:
                dataType = ModelStorageBase.LONG;
                bytesPerValue = 8;
                break;

            case MRI_FLOAT:
                dataType = ModelStorageBase.FLOAT;
                bytesPerValue = 4;
                break;

            case MRI_TENSOR:
                dataType = ModelStorageBase.FLOAT;
                bytesPerValue = 4;
                if (nFrames != 9) {
                    nFrames = 9;
                    extents = new int[4];
                    extents[0] = width;
                    extents[1] = height;
                    extents[2] = depth;
                    extents[3] = nFrames;
                    fileInfo.setExtents(extents);
                } // if (nFrames != 9)

                break;

            case MRI_BITMAP:
                Preferences.debug("Cannot handle type = MRI_BITMAP", Preferences.DEBUG_FILEIO);

                return false;
        } // switch (mghType)
        // raFile.close();

        fileInfo.setDataType(dataType);

        // Degrees of freedom
        dof = getInt(endianess); // 24
        fileInfo.setDOF(dof);

        goodRASFlag = (short) getSignedShort(endianess); // 28

        // The x, y, and z variables define the rotational part
        // of the affine transform.
        // The "c_ras" values define where the volume center
        // sits in the RAS coordinate system.  That is, c_r,
        // c_a, c_s are the RAS coordinate values of a voxel
        // point (width/2, height/2, depth/2).  The convention
        // used is that the center of a voxel corresponds to
        // the integer voxel coordinate position.
        if (goodRASFlag > 0) {
            resolutions[0] = getFloat(endianess); // 30
            resolutions[1] = getFloat(endianess); // 34
            resolutions[2] = getFloat(endianess); // 38
            xr = getFloat(endianess); // 42
            xa = getFloat(endianess); // 46
            xs = getFloat(endianess); // 50
            yr = getFloat(endianess); // 54
            ya = getFloat(endianess); // 58
            ys = getFloat(endianess); // 62
            zr = getFloat(endianess); // 66
            za = getFloat(endianess); // 70
            zs = getFloat(endianess); // 74
            cr = getFloat(endianess); // 78
            ca = getFloat(endianess); // 82
            cs = getFloat(endianess); // 86
        } // if (goodRASFlag > 0)
        else {

            // Default coronal orientation with z axis to the left,
            // y axis inferior, and z axis anterior
            resolutions[0] = 1.0f;
            resolutions[1] = 1.0f;
            resolutions[2] = 1.0f;
            xr = -1.0f;
            xa = 0.0f;
            xs = 0.0f;
            yr = 0.0f;
            ya = 0.0f;
            ys = -1.0f;
            zr = 0.0f;
            za = 1.0f;
            zs = 0.0f;
            cr = 0.0f;
            ca = 0.0f;
            cs = 0.0f;
        }

        fileInfo.setResolutions(resolutions);
        matrix.set(0, 0, (double) -xr * resolutions[0]);
        matrix.set(0, 1, (double) -yr * resolutions[1]);
        matrix.set(0, 2, (double) -zr * resolutions[2]);

        matrix.set(1, 0, (double) -xa * resolutions[0]);
        matrix.set(1, 1, (double) -ya * resolutions[1]);
        matrix.set(1, 2, (double) -za * resolutions[2]);

        matrix.set(2, 0, (double) xs * resolutions[0]);
        matrix.set(2, 1, (double) ys * resolutions[1]);
        matrix.set(2, 2, (double) zs * resolutions[2]);


        axisOrientation = getAxisOrientation(matrix);
        Preferences.debug("axisOrientation = " + axisOrientation[0] + "  " + axisOrientation[1] + "  " +
                          axisOrientation[2] + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setAxisOrientation(axisOrientation);

        if ((axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE) || (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
            fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
        } else if ((axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) ||
                       (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
            fileInfo.setImageOrientation(FileInfoBase.CORONAL);
        } else {
            fileInfo.setImageOrientation(FileInfoBase.AXIAL);
        }

        origin = new float[3];

        for (j = 0; j < 2; j++) {

            if (axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE) {
                origin[j] = -(width / 2.0f) * resolutions[0];
            } else if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE) {
                origin[j] = (width / 2.0f) * resolutions[0];
            } else if (axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE) {
                origin[j] = -(height / 2.0f) * resolutions[1];
            } else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE) {
                origin[j] = (height / 2.0f) * resolutions[1];
            } else if (axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE) {
                origin[j] = -(depth / 2.0f) * resolutions[2];
            } else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE) {
                origin[j] = (depth / 2.0f) * resolutions[2];
            }
        }
        if (axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE) {
            origin[2] = -(width / 2.0f) * resolutions[0];
        } else if (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE) {
            origin[2] = (width / 2.0f) * resolutions[0];
        } else if (axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) {
            origin[2] = (height / 2.0f) * resolutions[1];
        } else if (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE) {
            origin[2] = -(height / 2.0f) * resolutions[1];
        } else if (axisOrientation[2] == FileInfoBase.ORI_I2S_TYPE) {
            origin[2] = -(depth / 2.0f) * resolutions[2];
        } else if (axisOrientation[2] == FileInfoBase.ORI_S2I_TYPE) {
            origin[2] = (depth / 2.0f) * resolutions[2];
        }

        fileInfo.setOrigin(origin);
        matrix.set(0, 3, (double) origin[0]);
        matrix.set(1, 3, (double) origin[1]);
        matrix.set(2, 3, (double) origin[2]);
        Preferences.debug("matrix = \n" + matrix + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setMatrix(matrix);

        fileInfo.setLeftCenter(-cr);
        fileInfo.setPosteriorCenter(-ca);
        fileInfo.setSuperiorCenter(cs);

        optionalStructuresLocation = 284 + (bytesPerValue * width * height * depth * nFrames);

        if (fileLength >= (optionalStructuresLocation + 4)) {
            raFile.seek(optionalStructuresLocation);

            // Recovery time in milliseconds
            tr = getFloat(endianess);
            fileInfo.setTR(tr);
        }

        if (fileLength >= (optionalStructuresLocation + 8)) {

            // Flip angle in radians
            flipAngle = getFloat(endianess);
            fileInfo.setFlipAngle(flipAngle);
        }

        if (fileLength >= (optionalStructuresLocation + 12)) {

            // Echo time in milliseconds
            te = getFloat(endianess);
            fileInfo.setTE(te);
        }

        if (fileLength >= (optionalStructuresLocation + 16)) {

            // Inversion time in milliseconds
            ti = getFloat(endianess);
            fileInfo.setTI(ti);
        }

        if (fileLength >= (optionalStructuresLocation + 20)) {

            // Field of view in millimeters
            fov = getFloat(endianess);
            fileInfo.setFOV(fov);
        }

        bytesLeft = fileLength - (optionalStructuresLocation + 20);

        while (bytesLeft >= 4) {
            tag = getInt(endianess);

            switch (tag) {

                case 0:
                    Preferences.debug("Tag = 0 signalling end of file read\n", Preferences.DEBUG_FILEIO);
                    break;

                case TAG_OLD_COLORTABLE:
                    Preferences.debug("TAG_OLD_COLORTABLE\n", Preferences.DEBUG_FILEIO);
                    break;

                case TAG_OLD_USEREALRAS:
                    Preferences.debug("TAG_OLD_USEREALRAS\n", Preferences.DEBUG_FILEIO);
                    break;

                case TAG_CMDLINE:
                    Preferences.debug("TAG_CMDLINE\n", Preferences.DEBUG_FILEIO);
                    break;

                case TAG_OLD_SURF_GEOM:
                    Preferences.debug("TAG_OLD_SURF_GEOM\n", Preferences.DEBUG_FILEIO);
                    break;

                case TAG_OLD_MGH_XFORM:
                    Preferences.debug("TAG_OLD_MGH_XFORM\n", Preferences.DEBUG_FILEIO);
                    break;

                case TAG_MGH_XFORM:
                    Preferences.debug("TAG_MGH_XFORM\n", Preferences.DEBUG_FILEIO);
                    break;

                default:
                    Preferences.debug("Tag = " + tag + "\n", Preferences.DEBUG_FILEIO);
            }

            bytesLeft -= 4;

            if ((bytesLeft > 0) && (tag != 0)) {

                switch (tag) {

                    case TAG_OLD_MGH_XFORM:
                        pLen = getInt(endianess) - 1;
                        bytesLeft -= 4;
                        break;

                    case TAG_OLD_SURF_GEOM:
                    case TAG_OLD_USEREALRAS:
                    case TAG_OLD_COLORTABLE:
                        pLen = 0;
                        break;

                    default:
                        pLen = getLong(endianess);
                        bytesLeft -= 8;
                } // switch (tag)

                switch (tag) {

                    case TAG_OLD_MGH_XFORM:
                    case TAG_MGH_XFORM:
                        okay = true;
                        for (i = 0; (i < pLen) && okay; i++) {

                            if (i == 0) {
                                transformFileName = getString(1);
                                bytesLeft -= 1;
                            } else {
                                str = getString(1);
                                bytesLeft -= 1;

                                if (str.charAt(0) == '\n') {
                                    okay = false;
                                } else {
                                    transformFileName = transformFileName.concat(str);
                                }
                            }
                        } // for (i = 0; i < pLen && okay; i++)

                        Preferences.debug("Transform file name = " + transformFileName + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setTransformFileName(transformFileName);
                        break;

                    case TAG_CMDLINE:
                        if (ncmds < MAX_CMDS) {
                            cmdlines[ncmds] = getString((int) pLen);
                            bytesLeft -= pLen;
                            Preferences.debug("cmdlines[" + ncmds + "] = " + cmdlines[ncmds] + "\n", Preferences.DEBUG_FILEIO);
                            ncmds++;
                        } else {
                            Preferences.debug("Number of comands exceeds 1000/n", Preferences.DEBUG_FILEIO);
                        }

                        break;

                    default:
                        buf = new byte[(int) pLen];
                        raFile.read(buf);
                        bytesLeft -= pLen;
                        buf = null;
                } // switch (tag)
            } // if ((bytesLeft > 0) && (tag != 0))
        } // while (bytesLeft >= 4)

        if (ncmds > 0) {
            fileInfo.setCmdlines(cmdlines);
        }

        raFile.close();

        return true; // If it got this far, it has successfully read in the header
    }


    /**
     * Reads a MGH image file by reading the header then making a FileRaw to read the image for all filenames in the
     * file list. Only the one file directory (currently) supported.
     *
     * @param      one  flag indicating one image of a 3D dataset should be read in.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     The image.
     *
     * @see        FileRaw
     */
    public ModelImage readImage(boolean one) throws IOException, OutOfMemoryError {
        long offset;

        fileInfo = new FileInfoMGH(fileName, fileDir, FileUtility.MGH);

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException(" MGH header file error"));
        }

        int[] extents = null;

        try {

            if (one) {
                extents = new int[fileInfo.getExtents().length];

                for (int i = 0; i < extents.length; i++) {
                    extents[i] = fileInfo.getExtents()[i];
                }

                image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                                       fileInfo.getFileName());
            } else {
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        attachFileInfo(fileInfo, image);
        updateorigins(image.getFileInfo());
        image.setMatrix(matrix);

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

            offset = 284L;

            if (one) {

                if (fileInfo.getExtents().length > 2) {
                    offset = offset + getOffset();
                }
            }

            rawFile.readImage(image, offset);

            if (one) {
                fileInfo.setExtents(extents);
            }

        } catch (IOException error) {
            throw new IOException("FileMGH: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return image;
    }

    /**
     * Reads a MGH image file by reading the header then making a FileRaw to read the file. Image data is left in
     * buffer. If the fileInfo cannot be found, the header will be located and read first. Image is not 'flipped', and
     * neither units of measure nor orientation are set.
     *
     * @param      buffer  Image buffer to store image data into.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public void readImage(float[] buffer) throws IOException, OutOfMemoryError {
        long offset;

        if (fileInfo == null) { // if no file info yet, make it.
            fileInfo = new FileInfoMGH(fileName, fileDir, FileUtility.MGH);

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
                throw (new IOException("Cannot read image because of MGH header file error"));
            }
        }

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

            offset = 284L;

            rawFile.readImage(buffer, offset, dataType);
            rawFile.raFile.close();
        } catch (IOException error) {
            throw new IOException("FileMGH: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return;
    }

    /**
     * Writes a MGH or MGZ format type image.
     *
     * @param      image    Image model of data to write.
     * @param      options  options such as starting and ending slices and times
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        int index;
        boolean gzip;
        int zBegin;
        int zEnd;
        int tBegin;
        int tEnd;
        boolean endianess;
        byte[] byteBuffer;
        int sliceSize;
        int z;
        int t;
        int zDim;
        int numberSlices;
        int count;
        int j;
        short[] shortBuffer;
        int[] intBuffer;
        float[] floatBuffer;
        long[] longBuffer;
        int tmpInt;

        index = fileName.lastIndexOf(".");

        if ((fileName.substring(index + 1).equalsIgnoreCase("mgz")) ||
                (fileName.substring(index + 1).equalsIgnoreCase("gz"))) {
            gzip = true;

        } else {
            gzip = false;
        }

        zBegin = options.getBeginSlice();
        zEnd = options.getEndSlice();

        if (image.getNDims() == 4) {
            tBegin = options.getBeginTime();
            tEnd = options.getEndTime();
        } else {
            tBegin = 0;
            tEnd = 0;
        }

        if (gzip) {
            file = new File(fileDir + fileName.substring(0, index + 1) + "mgh");
        } else {
            file = new File(fileDir + fileName);
        }

        raFile = new RandomAccessFile(file, "rw");

        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        raFile.setLength(0);

        // MGH file are always BIG_ENDIAN
        endianess = BIG_ENDIAN;
        extents = image.getExtents();
        sliceSize = extents[0] * extents[1];
        dataType = image.getFileInfo(0).getDataType();
        resolutions = image.getFileInfo(0).getResolutions();
        FileInfoBase fileInfo = image.getFileInfo()[0];
        if (fileInfo instanceof FileInfoMGH) {
        	matrix = ((FileInfoMGH)fileInfo).getMatrix();
        }
        else {
            matrix = image.getMatrix();
        }

        if (matrix == null) {
            matrix = new TransMatrix(4);
        }

        // Current value of version number is 1
        writeInt(1, endianess); // 0

        // First dimension of the image buffer
        writeInt(extents[0], endianess); // 4

        // Second dimension of the image buffer
        writeInt(extents[1], endianess); // 8

        // Third dimension of the image buffer
        writeInt(zEnd - zBegin + 1, endianess); // 12

        // Fourth dimension of the image buffer
        writeInt(tEnd - tBegin + 1, endianess); // 16

        // Type of data
        switch (dataType) {

            case ModelStorageBase.UBYTE:
                mghType = MRI_UCHAR;
                break;

            case ModelStorageBase.BYTE:
            case ModelStorageBase.SHORT:
                mghType = MRI_SHORT;
                break;

            case ModelStorageBase.USHORT:
            case ModelStorageBase.INTEGER:
                mghType = MRI_INT;
                break;

            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.LONG:
                mghType = MRI_LONG;
                break;

            case ModelStorageBase.FLOAT:
                mghType = MRI_FLOAT;
                break;

            default:
                Preferences.debug("Cannot handle type = " + image.getFileInfo(0).getDataType() + "\n",
                		Preferences.DEBUG_FILEIO);
                throw (new IOException("Cannot write MGH image with data type = " + image.getFileInfo(0).getDataType()));
        } // switch (mghType)

        writeInt(mghType, endianess); // 20

        // Degrees of freedom
        writeInt(0, endianess); // 24

        // Good RAS flag
        writeShort((short) 1, endianess); // 28

        // The x, y, and z variables define the rotational part
        // of the affine transform.
        xr = -(float) matrix.get(0, 0) / resolutions[0];
        yr = -(float) matrix.get(0, 1) / resolutions[1];
        zr = -(float) matrix.get(0, 2) / resolutions[2];

        xa = -(float) matrix.get(1, 0) / resolutions[0];
        ya = -(float) matrix.get(1, 1) / resolutions[1];
        za = -(float) matrix.get(1, 2) / resolutions[2];

        xs = (float) matrix.get(2, 0) / resolutions[0];
        ys = (float) matrix.get(2, 1) / resolutions[1];
        zs = (float) matrix.get(2, 2) / resolutions[2];

        writeFloat(resolutions[0], endianess); // 30
        writeFloat(resolutions[1], endianess); // 34
        writeFloat(resolutions[2], endianess); // 38
        writeFloat(xr, endianess); // 42
        writeFloat(xa, endianess); // 46
        writeFloat(xs, endianess); // 50
        writeFloat(yr, endianess); // 54
        writeFloat(ya, endianess); // 58
        writeFloat(ys, endianess); // 62
        writeFloat(zr, endianess); // 66
        writeFloat(za, endianess); // 70
        writeFloat(zs, endianess); // 74

        // Since in readMGH the origin value is such as to
        // put the center at 0,0,0
        // cr
        writeFloat(0.0f, endianess); // 78

        // ca
        writeFloat(0.0f, endianess); // 82

        // cs
        writeFloat(0.0f, endianess); // 86

        // Fill out the other 194 bytes of the 284 byte header
        byteBuffer = new byte[194];
        raFile.write(byteBuffer);

        zDim = image.getExtents()[2];
        numberSlices = (tEnd - tBegin + 1) * (zEnd - zBegin + 1);
        count = 0;

        switch (dataType) {

            case ModelStorageBase.UBYTE:
                byteBuffer = new byte[sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);

                        image.exportSliceXY((t * zDim) + z, byteBuffer);
                        raFile.write(byteBuffer);
                    }
                }

                break;

            case ModelStorageBase.BYTE:
            case ModelStorageBase.SHORT:
                shortBuffer = new short[sliceSize];
                byteBuffer = new byte[2 * sliceSize];
                for (t = 0; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);

                        image.exportSliceXY((t * zDim) + z, shortBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
                            byteBuffer[(2 * j) + 1] = (byte) (shortBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.USHORT:
            case ModelStorageBase.INTEGER:
                intBuffer = new int[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);

                        image.exportSliceXY((t * zDim) + z, intBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[4 * j] = (byte) (intBuffer[j] >>> 24);
                            byteBuffer[(4 * j) + 1] = (byte) (intBuffer[j] >>> 16);
                            byteBuffer[(4 * j) + 2] = (byte) (intBuffer[j] >>> 8);
                            byteBuffer[(4 * j) + 3] = (byte) (intBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.LONG:
                longBuffer = new long[sliceSize];
                byteBuffer = new byte[8 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);

                        image.exportSliceXY((t * zDim) + z, longBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[8 * j] = (byte) (longBuffer[j] >>> 56);
                            byteBuffer[(8 * j) + 1] = (byte) (longBuffer[j] >>> 48);
                            byteBuffer[(8 * j) + 2] = (byte) (longBuffer[j] >>> 40);
                            byteBuffer[(8 * j) + 3] = (byte) (longBuffer[j] >>> 32);
                            byteBuffer[(8 * j) + 4] = (byte) (longBuffer[j] >>> 24);
                            byteBuffer[(8 * j) + 5] = (byte) (longBuffer[j] >>> 16);
                            byteBuffer[(8 * j) + 6] = (byte) (longBuffer[j] >>> 8);
                            byteBuffer[(8 * j) + 7] = (byte) (longBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.FLOAT:
                floatBuffer = new float[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);

                        image.exportSliceXY((t * zDim) + z, floatBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);
                            byteBuffer[4 * j] = (byte) (tmpInt >>> 24);
                            byteBuffer[(4 * j) + 1] = (byte) (tmpInt >>> 16);
                            byteBuffer[(4 * j) + 2] = (byte) (tmpInt >>> 8);
                            byteBuffer[(4 * j) + 3] = (byte) (tmpInt);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;


        } // switch(dataType)

        raFile.close();

        if (gzip) {
            fireProgressStateChanged("Compressing GZIP file ...");

            GZIPOutputStream out = new GZIPOutputStream(new FileOutputStream(fileDir + fileName));

            FileInputStream in = new FileInputStream(fileDir + fileName.substring(0, index + 1) + "mgh");

            byteBuffer = new byte[1024];

            int len;

            while ((len = in.read(byteBuffer)) > 0) {
                out.write(byteBuffer, 0, len);
            }

            in.close();
            out.finish();
            out.close();
        } // if (gzip)

    } // writeImage

    /**
     * DOCUMENT ME!
     *
     * @param  fileInfo  -- a NIFTI file Info that has already been read
     * @param  image     -- a ModelImage that the fileInfo needs to be attached to
     */
    private void attachFileInfo(FileInfoMGH fileInfo, ModelImage image) {

        int[] extents = fileInfo.getExtents();

        if (image.getNDims() == 2) {
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image

            for (int i = 0; i < extents[2]; i++) {
                FileInfoMGH newFileInfo = (FileInfoMGH) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image

            for (int i = 0; i < (extents[2] * extents[3]); i++) {
                FileInfoMGH newFileInfo = (FileInfoMGH) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    } // end updateUnitsOfMeasure()


    /**
     * Return the 3 axis orientation codes that correspond to the closest standard anatomical orientation of the (i,j,k)
     * axes.
     *
     * @param   mat  4x4 matrix that transforms (i,j,k) indexes to x,y,z coordinates where +x =Left, +y = Posterior, +z
     *               = Superior Only the upper-left 3x3 corner of the matrix is used This routine finds the permutation
     *               of (x,y,z) which has the smallest angle to the (i,j,k) axes directions, which are columns of the
     *               input matrix Errors: The codes returned will be zero.
     *
     * @return  DOCUMENT ME!
     */
    public static int[] getAxisOrientation(TransMatrix mat) {
        int[] axisOrientation = new int[3];
        //double[][] array;
        double xi, xj, xk, yi, yj, yk, zi, zj, zk, val;
        TransMatrix Q;
        double detQ;
        double vbest;
        int ibest, jbest, kbest, pbest, qbest, rbest;
        int i, j, k, p, q, r;
        TransMatrix P;
        double detP;
        TransMatrix M = new TransMatrix(mat.getDim());

        //array = mat.getMatrix(0, 2, 0, 2).getArray();
        xi = mat.get(0, 0);
        xj = mat.get(0, 1);
        xk = mat.get(0, 2);
        yi = mat.get(1, 0);
        yj = mat.get(1, 1);
        yk = mat.get(1, 2);
        zi = mat.get(2, 0);
        zj = mat.get(2, 1);
        zk = mat.get(2, 2);

        int izero = 0;
        int jzero = 0;
        int kzero = 0;
        int xzero = 0;
        int yzero = 0;
        int zzero = 0;

        if (xi == 0.0) {
            izero++;
            xzero++;
        }

        if (yi == 0.0) {
            izero++;
            yzero++;
        }

        if (zi == 0.0) {
            izero++;
            zzero++;
        }

        if (xj == 0.0) {
            jzero++;
            xzero++;
        }

        if (yj == 0.0) {
            jzero++;
            yzero++;
        }

        if (zj == 0.0) {
            jzero++;
            zzero++;
        }

        if (xk == 0.0) {
            kzero++;
            xzero++;
        }

        if (yk == 0.0) {
            kzero++;
            yzero++;
        }

        if (zk == 0.0) {
            kzero++;
            zzero++;
        }

        if ((izero == 2) && (jzero == 2) && (kzero == 2) && (xzero == 2) && (yzero == 2) && (zzero == 2)) {

            if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
            }

            return axisOrientation;
        } // if ((izero == 2) && (jzero == 2) && (kzero == 2) && (xzero == 2) && (yzero == 2) && (zzero == 2))

        // Normalize column vectors to get unit vectors along each ijk-axis

        // Normalize i axis
        val = Math.sqrt((xi * xi) + (yi * yi) + (zi * zi));

        if (val == 0.0) {
            MipavUtil.displayError("xi = yi = zi = 0 in getAxisOrientation");

            return null;
        }

        xi /= val;
        yi /= val;
        zi /= val;

        // Normalize j axis
        val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj));

        if (val == 0.0) {
            MipavUtil.displayError("xj = yj = zj = 0 in getAxisOrientation");

            return null;
        }

        xj /= val;
        yj /= val;
        zj /= val;

        // Orthogonalize j axis to i axis, if needed
        val = (xi * xj) + (yi * yj) + (zi * zj); // dot product between i and j

        if (Math.abs(val) > 1.0e-4) {
            xj -= val * xi;
            yj -= val * yi;
            zj -= val * zi;
            val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj)); // Must renormalize

            if (val == 0.0) {
                MipavUtil.displayError("j was parallel to i in getAxisOrientation");

                return null;
            }

            xj /= val;
            yj /= val;
            zj /= val;
        }

        // Normalize k axis; if it is zero, make it the cross product i x j
        val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

        if (val == 0.0) {
            xk = (yi * zj) - (zi * yj);
            yk = (zi * xj) - (zj * xi);
            zk = (xi * yj) - (yi * xj);
        } else {
            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to i
        val = (xi * xk) + (yi * yk) + (zi * zk); // dot product between i and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xi;
            yk -= val * yi;
            zk -= val * zi;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to i");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to j
        val = (xj * xk) + (yj * yk) + (zj * zk); // dot product between j and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xj;
            yk -= val * yj;
            zk -= val * zj;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to j");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }
        
        if ( ( (Math.abs(xi) > 0.9) || (Math.abs(yi) > 0.9) || (Math.abs(zi) > 0.9))
                && ( (Math.abs(xj) > 0.9) || (Math.abs(yj) > 0.9) || (Math.abs(zj) > 0.9))
                && ( (Math.abs(xk) > 0.9) || (Math.abs(yk) > 0.9) || (Math.abs(zk) > 0.9))) {

            if (Math.abs(xi) < 0.9) {
                xi = 0;
            }

            if (Math.abs(yi) < 0.9) {
                yi = 0;
            }

            if (Math.abs(zi) < 0.9) {
                zi = 0;
            }

            if (Math.abs(xj) < 0.9) {
                xj = 0;
            }

            if (Math.abs(yj) < 0.9) {
                yj = 0;
            }

            if (Math.abs(zj) < 0.9) {
                zj = 0;
            }

            if (Math.abs(xk) < 0.9) {
                xk = 0;
            }

            if (Math.abs(yk) < 0.9) {
                yk = 0;
            }

            if (Math.abs(zk) < 0.9) {
                zk = 0;
            }

            if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
            }

            return axisOrientation;
        }

        mat.set(0, 0, xi);
        mat.set(0, 1, xj);
        mat.set(0, 2, xk);
        mat.set(1, 0, yi);
        mat.set(1, 1, yj);
        mat.set(1, 2, yk);
        mat.set(2, 0, zi);
        mat.set(2, 1, zj);
        mat.set(2, 2, zk);

        // At this point, Q is the rotation matrix from the (i,j,k) to the (x,y,z) axes
        Q = new TransMatrix(mat);
        P = new TransMatrix(mat);
        detQ = Q.determinant();

        if (detQ == 0.0) {
            MipavUtil.displayError("detQ == 0.0 in getAxisOrientation");

            return null;
        }

        // Build and test all possible +1/-1 coordinate permutation matrices P;
        // then find the P such that the rotation matrix M=PQ is closest to the
        // identity, in the sense of M having the smallest total rotation angle

        // Despite the formidable looking 6 nested loops, there are
        // only 3*3*3*2*2*2 = 216 passes, which will run very quickly
        vbest = -Double.MAX_VALUE;
        pbest = 1;
        qbest = 1;
        rbest = 1;
        ibest = 1;
        jbest = 2;
        kbest = 3;

        for (i = 1; i <= 3; i++) { // i = column number to use for row #1

            for (j = 1; j <= 3; j++) { // j = column number to use for row #2

                if (i == j) {
                    continue;
                }

                for (k = 1; k <= 3; k++) { // k = column number to use for row #3

                    if ((i == k) || (j == k)) {
                        continue;
                    }

//                     mat.set(0, 0, 0.0);
//                     mat.set(0, 1, 0.0);
//                     mat.set(0, 2, 0.0);
//                     mat.set(1, 0, 0.0);
//                     mat.set(1, 1, 0.0);
//                     mat.set(1, 2, 0.0);
//                     mat.set(2, 0, 0.0);
//                     mat.set(2, 1, 0.0);
//                     mat.set(2, 2, 0.0);
                    P.makeZero();


                    for (p = -1; p <= 1; p += 2) { // p,q,r are -1 or +1 and go into rows #1,2,3

                        for (q = -1; q <= 1; q += 2) {

                            for (r = -1; r <= 1; r += 2) {
                                P.set(0, i - 1, p);
                                P.set(1, j - 1, q);
                                P.set(2, k - 1, r);
                                detP = P.determinant();

                                // sign of permutation doesn't match sign of Q
                                if ((detP * detQ) <= 0.0) {
                                    continue;
                                }
                                
                                M.copy( Matrix4f.mult(P, Q) );

                                // angle of M rotation = 2.0*acos(0.5*sqrt(1.0+trace(M)))
                                // we want largest trace(M) == smallest angle == M nearest to I
                                val = M.get(0, 0) + M.get(1, 1) + M.get(2, 2); // trace

                                if (val > vbest) {
                                    vbest = val;
                                    ibest = i;
                                    jbest = j;
                                    kbest = k;
                                    pbest = p;
                                    qbest = q;
                                    rbest = r;
                                }
                            }
                        }
                    }
                }
            }
        }

        // At this point ibest is 1 or 2 or 3; pbest is -1 or +1; etc.

        // The matrix P that corresponds is the best permutation approximation
        // to Q-inverse; that is, P (approximately) takes (x,y,z) coordinates
        // to the (i,j,k) axes

        // For example, the first row of P (which contains pbest in column ibest)
        // determines the way the i axis points relative to the anatomical
        // (x,y,z) axes.  If ibest is 2, then the i axis is along the yaxis,
        // which is direction P2A (if pbest < 0) or A2P (if pbest > 0).

        // So, using ibest and pbest, we can assign the output code for
        // the i axis.  The same also applies for the j and k axes.

        switch (ibest * pbest) {

            case -1:
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (jbest * qbest) {

            case -1:
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (kbest * rbest) {

            case -1:
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        return axisOrientation;
    }
    
    

    /**
     * Helper method to calculate the offset for getting only the middle NIFTI image slice from the 3D file.
     *
     * @return  offset
     */
    private int getOffset() {
        int offset = width * height * depth / 2;

        switch (dataType) {

            case ModelStorageBase.UBYTE:
                break;

            case ModelStorageBase.SHORT:
                offset *= 2;
                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.FLOAT:
                offset *= 4;
                break;

            case ModelStorageBase.LONG:
                offset *= 8;
                break;


        }

        return offset;
    }

    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     *
     * @param  fileInfo  DOCUMENT ME!
     */
    private void updateorigins(FileInfoBase[] fileInfo) {
        int axisOrient;

        float[] origin = (float[]) (fileInfo[0].getOrigin().clone());
        float[] resolutions = fileInfo[0].getResolutions();

        if (image.getNDims() == 3) {

            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setOrigin(origin);
                axisOrient = fileInfo[i].getAxisOrientation(2);

                if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                        (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                    origin[2] += resolutions[2];
                } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                    origin[2] -= resolutions[2];
                }
            }
        } else if (image.getNDims() == 4) {
            float tmp = origin[2];

            for (int i = 0; i < image.getExtents()[3]; i++) {

                for (int j = 0; j < image.getExtents()[2]; j++) {
                    fileInfo[(i * image.getExtents()[2]) + j].setOrigin(origin);
                    axisOrient = fileInfo[i].getAxisOrientation(2);

                    if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                            (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                        origin[2] += resolutions[2];
                    } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                        origin[2] -= resolutions[2];
                    }
                }

                origin[3] += resolutions[3];
                origin[2] = tmp;
            }
        }
        /*else if (image.getNDims() == 5) {
         * fileInfo = image.getFileInfo(); for (int i = 0;    i <    image.getExtents()[2] * image.getExtents()[3] *
         * image.getExtents()[4];    i++) { fileInfo[i].setorigins(startLocs); startLocs[4] += resolutions[4]; }  }*/
    }


}
