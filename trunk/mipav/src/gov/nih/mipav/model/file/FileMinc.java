package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 * The MINC file format is built on top of NetCDF. It has a small header that holds certain image information: position
 * and step data for each of the dimensions, real minimum and maximum values, and history.
 *
 * <P>For more information, go to <A HREF="http://www.bic.mni.mcgill.ca/software/minc">the MINC homepage</A> or <A
 * HREF="http://www.unidata.ucar.edu/packages/netcdf/guidec/guidec-18.html">the NetCDF File Format Specification</A>.
 * </P>
 *
 * @version  1.0 July 1, 2000
 * @see      FileIO
 * @see      FileRaw
 * @see      FileRawChunk
 */
public class FileMinc extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * The location, in bytes, of the portion of the minc file being written directly after the header. Does not include
     * data added to the header by the dynamically generated history attribute or dicom-exported tags.
     */
    private static final int DEFAULT_NON_HEADER_START_LOCATION = 2228;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Table of tags extracted from a dicom file info to be written to the minc file. Generated in writeHeader(), then
     * used again later in writeImage() to output placeholder values outside of the header.
     */
    private Hashtable dicomConvertedTagTable;

    /**
     * The endianess of the image being written or read.
     *
     * <p>TODO: this variable either needs to be local to the read/write methods or consistently used globally. As it
     * stands right now, it is used in both ways and confuses things greatly.</p>
     */
    private boolean endianess;

    /** The directory containing the minc file being written out or read in. */
    private String fileDir;

    /** The name of the minc file to be read in or written out. */
    private String fileName;

    /** The location, in bytes, of the image data in the minc file being written out. */
    private int imgBegin = 0;

    /** The size of the image data that will be written out, in bytes. */
    private int imgSize = 0;

    /**
     * TODO: THIS VARIABLE MUST BE REMOVED! It is used as if local in places and global in others. A number of methods
     * change it, causing subtle side effects which must be carefully managed.
     */
    private int location = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * MINC reader/writer constructor.
     *
     * @param      fName  File name.
     * @param      fDir   File directory.
     *
     * @exception  IOException  if there is an error constructing the files
     */
    public FileMinc(String fName, String fDir) throws IOException {
        fileName = fName;
        fileDir = fDir;

        try {

            try {
                raFile = new RandomAccessFile(new File(fileDir + fileName), "rw");
            } catch (IOException e) {
                raFile = new RandomAccessFile(new File(fileDir + fileName), "r");
            }

        } catch (OutOfMemoryError e) {
            throw new IOException("Out of memory in FileMinc constructor.");
        }
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Reads in all the tags available in the file and stores them in FileInfoMinc.
     *
     * <P>Format:<BR>
     * header := magic numrecs dim_array gatt_array var_array<BR>
     * magic := 'C' 'D' 'F' '\001'<BR>
     * numrecs := non negative integer<BR>
     * dim_array := NC_DIMENSION nelems [dim...]<BR>
     * gatt_array := att_array<BR>
     * att_array := NC_ATTRIBUTE nelems [attr...]<BR>
     * var_array := NC_VARIABLE nelems [var...]<BR>
     * nelems is # of elements in following array.<BR>
     * dim := name dim_length<BR>
     * dim is the extents; the name is xspace, yspace, or zspace.<BR>
     * attr := name nc_type nelems [values]<BR>
     * name is the name of the attribute; nc_type is the type (integer, char, etc.); values are the values of this
     * attribute. It seems that attributes are mostly of type char, so the entire attribute is just a string.<BR>
     * var := name nelems [dimid...] vatt_array nc_type vsize begin<BR>
     * name is the name of the variable; don't know what dimid is; vatt_array is an array of attributes for this
     * variable; nc_type is the type of the variable; vsize is the size of the variable; begin is where in the file the
     * variable data begins.</P>
     *
     * @return     <code>true</code> confirms a successful read.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileInfoMinc
     */
    public FileInfoMinc readHeader() throws IOException {
        String attrString;
        FileInfoMinc fileInfo = new FileInfoMinc(fileName, fileDir, FileBase.MINC);

        location = 0;
        raFile.seek(0);
        endianess = FileBase.BIG_ENDIAN;
        fileInfo.setEndianess(endianess);

        String magic = getString(4);

        // Every MINC image starts with the CDF tag
        if (!magic.equals("CDF" + '\001')) {
            Preferences.debug("4 byte magic tag = " + magic + " instead of CDF\001\n", Preferences.DEBUG_FILEIO);
            throw new IOException("No 'CDF' tag at beginning of file");
        }

        location += 4;
        fileInfo.numrecs = getInt(endianess);
        location += 4;

        int next = getInt(endianess);

        // Dimension part of header
        if (next == 167772160) {
            endianess = FileBase.LITTLE_ENDIAN;
            fileInfo.setEndianess(endianess);
            raFile.seek(4);
            fileInfo.numrecs = getInt(endianess);
            location = 8;
            next = getInt(endianess);
        }

        if (endianess == FileBase.BIG_ENDIAN) {
            Preferences.debug("Big endian\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("Little endian\n", Preferences.DEBUG_FILEIO);
        }

        Preferences.debug("numrecs = " + fileInfo.numrecs + "\n", Preferences.DEBUG_FILEIO);

        if (next == FileInfoMinc.NC_DIMENSION) {
            Preferences.debug("10 for NC_DIMENSION found\n", Preferences.DEBUG_FILEIO);
            next = getInt(endianess); // nelems
            Preferences.debug("NC_DIMENSION is followed with number of elements = " + next + "\n",
                              Preferences.DEBUG_FILEIO);
            fileInfo.createDimArray(next);
            location += 8;

            int[] extents = new int[fileInfo.getDimArray().length];

            for (int i = 0; i < fileInfo.getDimArray().length; i++) {
                int len = getInt(endianess);
                Preferences.debug("length of dim[" + i + "] string name = " + len + "\n", Preferences.DEBUG_FILEIO);

                String name = getString(len);
                Preferences.debug("dim[" + i + "] string name = " + name + "\n", Preferences.DEBUG_FILEIO);
                location += 4 + len;
                padding();
                len = getInt(endianess);
                Preferences.debug("dim[" + i + "] length = " + len + "\n", Preferences.DEBUG_FILEIO);

                // goes z y x
                extents[fileInfo.getDimArray().length - 1 - i] = len;
                Preferences.debug("extents[" + (fileInfo.getDimArray().length - 1 - i) + "] = " + len + "\n",
                                  Preferences.DEBUG_FILEIO);
                location += 4;
                fileInfo.addDimElem(name, len, i);
            }

            fileInfo.setExtents(extents);
        }
        // ABSENT - two zeros in a row
        else if (next == 0) {
            Preferences.debug("0 for absence of NC_DIMENSION tag read\n", Preferences.DEBUG_FILEIO);
            next = getInt(endianess);

            if (next == 0) {
                Preferences.debug("0 for number of NC_DIMENSION elements read\n", Preferences.DEBUG_FILEIO);
                location += 8;
            } else {
                Preferences.debug("ABSENCE of NC_DIMENSION tag erroneously " + "followed with number of elements = " +
                                  next + "\n", Preferences.DEBUG_FILEIO);
                throw new IOException("MINC header corrupted");
            }
        } else {
            Preferences.debug("Should have NC_DIMENSION = 10 or ABSENCE = 0, " + "but instead have = " + next + "\n",
                              Preferences.DEBUG_FILEIO);
            throw new IOException("MINC header corrupted");
        }

        next = getInt(endianess);

        // Global attribute part of file; usually history, usually characters
        if (next == FileInfoMinc.NC_ATTRIBUTE) {
            Preferences.debug("12 for NC_ATTRIBUTE found\n", Preferences.DEBUG_FILEIO);
            next = getInt(endianess);
            Preferences.debug("NC_ATTRIBUTE is followed with " + " number of elements = " + next + "\n",
                              Preferences.DEBUG_FILEIO);
            fileInfo.createGattArray(next);
            location += 8;

            for (int i = 0; i < fileInfo.getGattArray().length; i++) {
                attrString = new String();

                int len = getInt(endianess);
                Preferences.debug("length of attr[" + i + "] string name = " + len + "\n", Preferences.DEBUG_FILEIO);

                String name = getString(len);
                Preferences.debug("attr[" + i + "] string name = " + name + "\n", Preferences.DEBUG_FILEIO);
                location += 4 + len;
                padding();

                int type = getInt(endianess);

                switch (type) {

                    case 1:
                        Preferences.debug("attr[" + i + "] nc_type = 1 for NC_BYTE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 2:
                        Preferences.debug("attr[" + i + "] nc_type = 2 for NC_CHAR\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 3:
                        Preferences.debug("attr[" + i + "] nc_type = 3 for NC_SHORT\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 4:
                        Preferences.debug("attr[" + i + "] nc_type = 4 for NC_INT\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 5:
                        Preferences.debug("attr[" + i + "] nc_type = 5 for NC_FLOAT\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 6:
                        Preferences.debug("attr[" + i + "] nc_type = 6 for NC_DOUBLE\n", Preferences.DEBUG_FILEIO);
                        break;

                    default:
                        Preferences.debug("attr[" + i + "] nc_type illegally = " + type + "\n",
                                          Preferences.DEBUG_FILEIO);
                }

                location += 4;
                len = getInt(endianess);
                Preferences.debug("attr[" + i + "] length = " + len + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.addGattElem(name, type, len, i);
                location += 4;

                for (int j = 0; j < len; j++) {
                    Object value = getNextElem(type, endianess);

                    switch (type) {

                        case 1:
                            Preferences.debug("attr[" + i + "][" + j + "] = " + ((Byte) (value)).byteValue() + "\n",
                                              Preferences.DEBUG_FILEIO);
                            break;

                        case 2:
                            attrString += ((Character) (value)).charValue();
                            break;

                        case 3:
                            Preferences.debug("attr[" + i + "][" + j + "] = " + ((Short) (value)).shortValue() + "\n",
                                              Preferences.DEBUG_FILEIO);
                            break;

                        case 4:
                            Preferences.debug("attr[" + i + "][" + j + "] = " + ((Integer) (value)).intValue() + "\n",
                                              Preferences.DEBUG_FILEIO);
                            break;

                        case 5:
                            Preferences.debug("attr[" + i + "][" + j + "] = " + ((Float) (value)).floatValue() + "\n",
                                              Preferences.DEBUG_FILEIO);
                            break;

                        case 6:
                            Preferences.debug("attr[" + i + "][" + j + "] = " + ((Double) (value)).doubleValue() + "\n",
                                              Preferences.DEBUG_FILEIO);
                            break;
                    }

                    fileInfo.addAttValue(fileInfo.getGattElem(i), value, j);
                }

                if ((type == 2) && (attrString != null)) {
                    Preferences.debug("attr[" + i + "] = " + attrString.trim() + "\n", Preferences.DEBUG_FILEIO);
                }

                padding();

            }
        }
        // ABSENT - two zeros
        else if (next == 0) {
            Preferences.debug("0 for absence of NC_ATTRIBUTE tag read\n", Preferences.DEBUG_FILEIO);
            next = getInt(endianess);

            if (next == 0) {
                Preferences.debug("0 for number of NC_ATTRIBUTE elements read\n", Preferences.DEBUG_FILEIO);
                location += 8;
            } else {
                Preferences.debug("ABSENCE of NC_ATTRIBUTE tag erroneously " + "followed by number of elements = " +
                                  next + "\n", Preferences.DEBUG_FILEIO);
                throw new IOException("MINC header corrupted");
            }
        } else {
            Preferences.debug("Should have NC_ATTRIBUTE = 12 or ABSENCE = 0, " + "but instead have = " + next + "\n",
                              Preferences.DEBUG_FILEIO);
            throw new IOException("MINC header corrupted");
        }

        next = getInt(endianess);

        // Variable part of header
        if (next == FileInfoMinc.NC_VARIABLE) {
            Preferences.debug("11 for NC_VARIABLE found\n", Preferences.DEBUG_FILEIO);
            next = getInt(endianess);
            Preferences.debug("NC_VARIABLE is followed with " + " number of elements = " + next + "\n",
                              Preferences.DEBUG_FILEIO);
            fileInfo.createVarArray(next);
            location += 8;

            for (int i = 0; i < fileInfo.getVarArray().length; i++) {
                int len = getInt(endianess);
                Preferences.debug("length of var[" + i + "] string name = " + len + "\n", Preferences.DEBUG_FILEIO);

                String name = getString(len);
                Preferences.debug("var[" + i + "] string name = " + name + "\n", Preferences.DEBUG_FILEIO);
                location += 4 + len;
                padding();
                len = getInt(endianess);
                Preferences.debug("var[" + i + "] has dimid.length = " + len + "\n", Preferences.DEBUG_FILEIO);
                location += 4;

                int[] dimid = new int[len];

                for (int j = 0; j < len; j++) {
                    dimid[j] = getInt(endianess);
                    Preferences.debug("var[" + i + "] dimid[" + j + "] = " + dimid[j] + "\n", Preferences.DEBUG_FILEIO);
                    location += 4;
                }

                fileInfo.addVarElem(name, len, dimid, i);
                next = getInt(endianess);
                location += 4;

                // get the attribute array for this variable
                getVattArray(i, fileInfo);

                int type = getInt(endianess);

                switch (type) {

                    case 1:
                        Preferences.debug("var[" + i + "] nc_type = 1 for NC_BYTE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 2:
                        Preferences.debug("var[" + i + "] nc_type = 2 for NC_CHAR\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 3:
                        Preferences.debug("var[" + i + "] nc_type = 3 for NC_SHORT\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 4:
                        Preferences.debug("var[" + i + "] nc_type = 4 for NC_INT\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 5:
                        Preferences.debug("var[" + i + "] nc_type = 5 for NC_FLOAT\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 6:
                        Preferences.debug("var[" + i + "] nc_type = 6 for NC_DOUBLE\n", Preferences.DEBUG_FILEIO);
                        break;

                    default:
                        Preferences.debug("var[" + i + "] nc_type illegally = " + type + "\n",
                                          Preferences.DEBUG_FILEIO);
                }

                int size = getInt(endianess);
                Preferences.debug("var[" + i + "] vsize = " + size + "\n", Preferences.DEBUG_FILEIO);

                int begin = getInt(endianess);
                Preferences.debug("var[" + i + "] begin = " + begin + "\n", Preferences.DEBUG_FILEIO);
                location += 12;
                fileInfo.getVarElem(i).setOther(type, size, begin);
            }
        }
        // ABSENT - two zeros
        else if (next == 0) {
            Preferences.debug("0 for absence of NC_VARIABLE tag read\n", Preferences.DEBUG_FILEIO);
            next = getInt(endianess);

            if (next == 0) {
                Preferences.debug("0 for number of NC_VARIABLE elements read\n", Preferences.DEBUG_FILEIO);
                location += 8;
            } else {
                Preferences.debug("ABSENCE of NC_VARIABLE tag erroneously " + "followed by number of elements = " +
                                  next + "\n", Preferences.DEBUG_FILEIO);

                throw new IOException("MINC header corrupted");
            }
        } else {
            Preferences.debug("Should have NC_VARIABLE = 11 or ABSENCE = 0, " + "but instead have = " + next + "\n",
                              Preferences.DEBUG_FILEIO);
            throw new IOException("MINC header corrupted");
        }

        return fileInfo;
    }

    /**
     * Reads a MINC image file and stores the data in file info.
     *
     * @param      one  Flag indicating if only one image should be read in
     *
     * @return     The image.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public ModelImage readImage(boolean one) throws IOException {
        FileInfoMinc fileInfo = null;
        ViewJProgressBar progressBar = null;

        progressBar = new ViewJProgressBar(ViewUserInterface.getReference().getProgressBarPrefix() + fileName,
                                           ViewUserInterface.getReference().getProgressBarPrefix() + "image(s) ...", 0,
                                           100, false, null, null);
        progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);
        progressBar.setVisible(!one);
        progressBar.updateValue(5, true);

        try {
            fileInfo = readHeader();
        } catch (IOException error) {
            raFile.close();
            throw error;
        }

        progressBar.updateValue(10, true);


        float[] buffer;
        int[] extents = null;
        ModelImage image = null;
        FileRaw rawFile;

        try {

            if (one) {
                extents = new int[fileInfo.getExtents().length];

                for (int i = 0; i < extents.length; i++) {
                    extents[i] = fileInfo.getExtents()[i];
                }

                image = new ModelImage(ModelImage.FLOAT, new int[] { extents[0], extents[1] }, fileName);
            } else {
                image = new ModelImage(ModelImage.FLOAT, fileInfo.getExtents(), fileName);
            }

            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, false, FileBase.READ);

            buffer = new float[fileInfo.getExtents()[0] * fileInfo.getExtents()[1]];
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
            }

            image = null;
            buffer = null;
            rawFile = null;
            System.gc();
            throw (error);
        }

        // set image orientation depending on which variable was read in firs
        if (fileInfo.getDimElem(0).name.equals("zspace")) {
            fileInfo.setImageOrientation(FileInfoBase.AXIAL);
        } else if (fileInfo.getDimElem(0).name.equals("xspace")) {
            fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
        } else if (fileInfo.getDimElem(0).name.equals("yspace")) {
            fileInfo.setImageOrientation(FileInfoBase.CORONAL);
        }

        fileInfo.setImportantImageInfo();
        fileInfo.setResolutions(fileInfo.getImageOrientation());
        fileInfo.setUnits();

        // ModelImage image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileName);
        // for each variable, get its corresponding data - possibly after image
        for (int i = 0; i < fileInfo.getVarArray().length; i++) {

            // at image tag, construct image
            if (fileInfo.getVarElem(i).name.equals("image")) {

                try {
                    location += fileInfo.getVarElem(i).vsize;
                    padding();
                } catch (OutOfMemoryError error) {

                    if (image != null) {
                        image.disposeLocal();
                    }

                    image = null;
                    buffer = null;
                    rawFile = null;
                    System.gc();
                    throw (error);
                }
            } else if ((fileInfo.getVarElem(i).begin + fileInfo.getVarElem(i).vsize) <= raFile.length()) {

                // at all other tags both before & after image
                raFile.seek(fileInfo.getVarElem(i).begin);

                // set value of variable to data read in
                location = fileInfo.getVarElem(i).begin;

                while (location < (fileInfo.getVarElem(i).vsize + fileInfo.getVarElem(i).begin)) {
                    fileInfo.getVarElem(i).setValue(getNextElem(fileInfo.getVarElem(i).nc_type, endianess));
                }

            }
        }

        progressBar.updateValue(15, true);
        raFile.close();

        if ((fileInfo.vmax == -1) && (fileInfo.vmin == -1)) {
            fileInfo.vmax = image.getMax();
            fileInfo.vmin = image.getMin();
        }

        double[] rescaleIntercept = null;
        double[] rescaleSlope = null;

        if (image.getNDims() == 2) {
            rescaleIntercept = new double[1];
            rescaleSlope = new double[1];
            fileInfo.calculateRescaleIntercept(rescaleIntercept, rescaleSlope);
        } else if (image.getNDims() == 3) {
            rescaleIntercept = new double[image.getExtents()[2]];
            rescaleSlope = new double[image.getExtents()[2]];
            fileInfo.calculateRescaleIntercept(rescaleIntercept, rescaleSlope);
        }


        if (image.getNDims() == 2) {
            fileInfo.setRescaleIntercept(rescaleIntercept[0]);
            fileInfo.setRescaleSlope(rescaleSlope[0]);
            fileInfo.setStartLocations(((FileInfoMinc) fileInfo).getConvertStartLocationsToDICOM(0));
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image

            FileInfoMinc fileInfo0 = (FileInfoMinc) fileInfo.clone();

            for (int k = 0; k < image.getExtents()[2]; k++) {
                progressBar.updateValueImmed(Math.round(15 + ((float) k / image.getExtents()[2] * 10)));
                fileInfo.setRescaleIntercept(rescaleIntercept[k]);
                fileInfo.setRescaleSlope(rescaleSlope[k]);
                fileInfo.setStartLocations(fileInfo0.getConvertStartLocationsToDICOM(k));
                image.setFileInfo((FileInfoMinc) fileInfo.clone(), k); // Set the array of fileInfos in ModelImage
            }
        }

        progressBar.updateValue(25, true);


        try {
            rawFile.setImageFile(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

            int imgTypeFac = 1;

            if ((fileInfo.getDataType() == ModelStorageBase.BYTE) ||
                    (fileInfo.getDataType() == ModelStorageBase.UBYTE)) {
                imgTypeFac = 1;
            } else if ((fileInfo.getDataType() == ModelStorageBase.SHORT) ||
                           (fileInfo.getDataType() == ModelStorageBase.USHORT)) {
                imgTypeFac = 2;
            } else if ((fileInfo.getDataType() == ModelStorageBase.FLOAT) ||
                           (fileInfo.getDataType() == ModelStorageBase.INTEGER)) {
                imgTypeFac = 4;
            } else if ((fileInfo.getDataType() == ModelStorageBase.DOUBLE) ||
                           (fileInfo.getDataType() == ModelStorageBase.LONG)) {
                imgTypeFac = 8;
            }

            double slope, intercept;
            slope = 1.0;
            intercept = 0.0;

            int nImgs = 1;

            if (fileInfo.getExtents().length == 3) {
                nImgs = fileInfo.getExtents()[2];
            }

            if (one) {
                nImgs = 1;
            }

            for (int n = 0; n < nImgs; n++) {
                progressBar.updateValueImmed(Math.round(25 + ((float) n / nImgs * 75)));

                // ***** Read image slice
                if (one) {

                    if (fileInfo.getExtents().length == 3) {
                        rawFile.readImage(buffer,
                                          fileInfo.getOffset() + (fileInfo.getExtents()[2] / 2 * buffer.length * imgTypeFac),
                                          fileInfo.getDataType());
                    } else {
                        rawFile.readImage(buffer, fileInfo.getOffset(), fileInfo.getDataType());
                    }
                } else {
                    rawFile.readImage(buffer, fileInfo.getOffset() + (n * buffer.length * imgTypeFac),
                                      fileInfo.getDataType());
                }

                slope = image.getFileInfo(n).getRescaleSlope();
                intercept = image.getFileInfo(n).getRescaleIntercept();

                for (int m = 0; m < buffer.length; m++) {
                    buffer[m] = (float) ((buffer[m] * slope) + intercept);
                }

                image.importData(n * buffer.length, buffer, false);
            }

            rawFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("FileMinc: " + error);
            raFile.close();
            rawFile.close();

            if (image != null) {
                image.disposeLocal();
            }

            image = null;
            buffer = null;
            rawFile = null;
            System.gc();
            throw (error);
        }

        if (fileInfo.getExtents().length == 3) {
            progressBar.updateValueImmed(100);
            progressBar.dispose();
        }

        buffer = null;

        return image;
    }


    /**
     * Writes a MINC format type image. Calls the appropriate header method.
     *
     * @param      _image   Image model where the data is stored.
     * @param      options  Information about how to write this file.
     *
     * @exception  IOException  if there is an error writing the file
     *
     * @see        FileInfoMinc
     * @see        FileMinc
     */
    public void writeImage(ModelImage _image, FileWriteOptions options) throws IOException {
        ViewJProgressBar progressBar = new ViewJProgressBar("Saving " + fileName, "Writing header", 0, 100, false, null,
                                                            null);
        progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);
        progressBar.setVisible(true);
        progressBar.updateValue(5, true);

        raFile.setLength(0);

        ModelImage image = null;
        progressBar.updateValue(10, options.isActiveImage());

        try {
            image = new ModelImage(_image.getFileInfo()[0].getDataType(), _image.getFileInfo()[0].getExtents(),
                                   _image.getImageFileName());

            image.copyFileTypeInfo(_image);

            if (!options.isSaveAs() || (_image.getFileInfo(0).getFileFormat() == FileBase.MINC)) {
                FileInfoMinc fileInfo;
                fileInfo = (FileInfoMinc) _image.getFileInfo(0);

                int length;
                writeHeader(fileInfo);
                progressBar.updateValue(15, options.isActiveImage());

                // for each variable, get its corresponding data - possibly after image
                for (int i = 0; i < fileInfo.getVarArray().length; i++) {

                    // at image tag, construct image
                    if (fileInfo.getVarElem(i).name.equals("image")) {
                        FileRawChunk rawChunkFile;
                        rawChunkFile = new FileRawChunk(raFile, fileInfo);
                        progressBar.setMessage("Saving image(s) ...");
                        length = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];

                        float[] data = new float[length];

                        if (image.getNDims() == 3) {

                            for (int j = 0; j < image.getExtents()[2]; j++) {
                                _image.exportData(j * length, length, data);
                                progressBar.updateValue(15 +
                                                        Math.round((float) j / (fileInfo.getExtents()[2] - 1) * 35),
                                                        options.isActiveImage());


                                // Why don't we recalc min and max per image slice.
                                // What happens if the image changes ?
                                double slope = _image.getFileInfo(j).getRescaleSlope();
                                double intercept = _image.getFileInfo(j).getRescaleIntercept();

                                for (int k = 0; k < length; k++) {
                                    data[k] = (float) ((data[k] - intercept) / slope);
                                }

                                image.importData(j * length, data, false);
                            }

                            for (int j = 0; j < image.getExtents()[2]; j++) {
                                rawChunkFile.writeImage(image, j * length, (j + 1) * length, j);
                                progressBar.updateValue(50 +
                                                        Math.round((float) j / (fileInfo.getExtents()[2] - 1) * 50),
                                                        options.isActiveImage());
                            }

                            progressBar.updateValue(100, options.isActiveImage());
                            progressBar.dispose();
                        } else {
                            rawChunkFile.writeImage(image, 0, image.getExtents()[0] * image.getExtents()[1], 0);
                        }

                        progressBar.updateValue(100, options.isActiveImage());
                        progressBar.dispose();
                        location = fileInfo.getVarElem(i).vsize;
                        writePadding();
                    } else {

                        // at all other tags both before & after image
                        location = fileInfo.getVarElem(i).begin;
                        raFile.seek(fileInfo.getVarElem(i).begin);

                        for (int j = 0; j < fileInfo.getVarElem(i).values.size(); j++) {
                            writeNextElem(fileInfo.getVarElem(i).values.elementAt(j), fileInfo.getVarElem(i).nc_type,
                                          fileInfo.getEndianess());
                        }

                        writePadding();

                        while (location < (fileInfo.getVarElem(i).begin + fileInfo.getVarElem(i).vsize)) {
                            location++;
                            raFile.write((byte) 0);
                        }
                    }
                }
            } else {
                int nImages = options.getEndSlice() - options.getBeginSlice() + 1;
                FileInfoBase fileInfo = _image.getFileInfo(0);
                int[] extents = fileInfo.getExtents();

                // int[] newexts;
                double[] mins = null;
                double[] maxs = null;

                writeHeader(fileInfo, options);

                // placeholder for location pointed to by rootvariable NC_VARIABLE
                writeInt(0, endianess);

                // placeholders for location pointed to by {x,y,z}space NC_VARIABLEs
                for (int j = 0; j < fileInfo.getExtents().length; j++) {
                    writeDouble(0, endianess);
                }

                progressBar.updateValueImmed(2);
                fileInfo.setExtents(extents); // reset extents to proper value

                FileRawChunk rawChunkFile;
                rawChunkFile = new FileRawChunk(raFile, fileInfo);

                int bufferSize = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];

                progressBar.setMessage("Rescaling data");
                progressBar.updateValue(10, options.isActiveImage());

                float[] data = new float[bufferSize];
                mins = new double[nImages];
                maxs = new double[nImages];

                double vmin, vmax; // volume min and max
                double intercept, slope;
                double smin, smax; // slice min and max

                if (fileInfo.getFileFormat() == FileBase.MINC) {

                    // Valid_range see line  823 in FileInfoMinc!!!!!!!.
                    vmin = ((FileInfoMinc) (fileInfo)).vmin;
                    vmax = ((FileInfoMinc) (fileInfo)).vmax;
                } else {
                    vmin = _image.getMin();
                    vmax = _image.getMax();
                }

                int count = 1;
                int jp;

                for (int j = options.getBeginSlice(); j <= options.getEndSlice(); j++) {
                    jp = j - options.getBeginSlice();
                    progressBar.updateValue(10 + Math.round((float) count / (nImages - 1) * 40),
                                            options.isActiveImage());
                    _image.exportData(j * bufferSize, bufferSize, data);
                    smin = Double.MAX_VALUE;
                    smax = -Double.MAX_VALUE;

                    // calculate min max values per slice
                    for (int k = 0; k < data.length; k++) {

                        if (data[k] < smin) {
                            smin = data[k];
                        }

                        if (data[k] > smax) {
                            smax = data[k];
                        }
                    }

                    mins[jp] = smin;
                    maxs[jp] = smax;

                    double divisor = vmax - vmin;

                    if (divisor == 0) {
                        divisor = 1;
                    }

                    slope = (smax - smin) / divisor;
                    intercept = smin - (slope * vmin);

                    for (int k = 0; k < data.length; k++) {
                        data[k] = (float) ((data[k] - intercept) / slope);
                    }

                    image.importData(jp * bufferSize, data, false);
                    count++;
                }

                image.getFileInfo(0).setEndianess(FileBase.BIG_ENDIAN);
                progressBar.setMessage("Saving image(s) ...");
                count = 1;

                for (int j = options.getBeginSlice(); j <= options.getEndSlice(); j++) {
                    jp = j - options.getBeginSlice();

                    // System.out.println(" j = " + j);
                    rawChunkFile.writeImage(image, jp * bufferSize, (jp + 1) * bufferSize, jp);
                    progressBar.updateValue(50 + Math.round((float) count / (nImages - 1) * 50),
                                            options.isActiveImage());
                    count++;
                }

                location = imgBegin + imgSize; // important for proper alignment of min and max values of image
                writePadding();

                int m = 0;

                for (int j = options.getBeginSlice(); j <= options.getEndSlice(); j++) {
                    writeDouble(maxs[m++], FileBase.BIG_ENDIAN);
                }

                m = 0;

                for (int j = options.getBeginSlice(); j <= options.getEndSlice(); j++) {
                    writeDouble(mins[m++], FileBase.BIG_ENDIAN);
                }

                // write out placeholders values pointed to by NC_VARIABLEs of extracted dicom tag groups (if any)
                Enumeration groupEnum = dicomConvertedTagTable.keys();
                while (groupEnum.hasMoreElements()) {
                    groupEnum.nextElement();
                    writeInt(0, FileBase.BIG_ENDIAN);
                }

                progressBar.updateValue(100, options.isActiveImage());
            }
        } catch (OutOfMemoryError e) {
            raFile.close();
            progressBar.dispose();

            if (image != null) {
                image.disposeLocal();
            }

            throw new IOException("Out of memory in FileMinc writer.");
        }

        raFile.close();
        progressBar.dispose();
        image.disposeLocal();
        image = null;
    }

    /**
     * Extracts any Dicom tags from a given FileInfoBase (if the file info is dicom) and puts them into a Hashtable.
     *
     * @param   fileInfo  the file info to extract dicom tags from
     *
     * @return  Hashtable keyed on tag group, containing Hashtables keyed on tag element, containing the tag values.
     *          Returns an empty Hashtable if the file info is not dicom.
     */
    private static Hashtable extractDicomTags(FileInfoBase fileInfo) {
        Hashtable tagTable = new Hashtable();

        if (fileInfo instanceof FileInfoDicom) {
            FileInfoDicom dicomInfo = (FileInfoDicom) fileInfo;
            Hashtable dicomTags = dicomInfo.getTagsList();
            Enumeration tagKeyEnum = dicomTags.keys();

            while (tagKeyEnum.hasMoreElements()) {
                FileDicomKey key = (FileDicomKey) tagKeyEnum.nextElement();
                FileDicomTag tag = (FileDicomTag) dicomTags.get(key);

                String group = key.getGroup();
                String element = key.getElement();

                String valueStr = getDicomValueAsString(key, tag);

                if (valueStr != null) {
                    Preferences.debug("exported:\t" + group + "," + element + "\t" + valueStr + "\n",
                                      Preferences.DEBUG_FILEIO);

                    if (!tagTable.containsKey(group)) {
                        tagTable.put(group, new Hashtable());
                    }

                    ((Hashtable) tagTable.get(group)).put(element, valueStr);
                }
            }
        }

        return tagTable;
    }

    /**
     * Returns the value of a dicom tag, represented as a string. Null tag values are ignored, as are array value types.
     *
     * @param   key  The dicom tag key (contains the tag group and element strings). TODO: maybe remove this parameter.
     * @param   tag  The dicom tag (including its value).
     *
     * @return  the value of the given dicom tag (null if the value is null or is an array (which we don't support at
     *          the moment))
     */
    private static String getDicomValueAsString(FileDicomKey key, FileDicomTag tag) {
        String group = key.getGroup();
        String element = key.getElement();

        // note: don't parse the dicom value into something more human-readable, just put it into the minc
        Object value = tag.getValue(false);
        if (value == null) {

            // Preferences.debug("skipping null tag:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);
            return null;
        }

        String valueStr = null;
        if (value instanceof Integer) {
            valueStr = value.toString();
            Preferences.debug("Int:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);
        } else if (value instanceof Short) {
            valueStr = value.toString();
            Preferences.debug("Short:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);
        } else if (value instanceof String) {
            valueStr = value.toString();
            Preferences.debug("String:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);
        } else if (value instanceof Float) {
            valueStr = value.toString();
            Preferences.debug("Float:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);
        } else if (value instanceof Double) {
            valueStr = value.toString();
            Preferences.debug("Double:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);
        } else if (value instanceof Byte[]) {
            Preferences.debug("Byte[] - skipped:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);

            // Byte[] array = (Byte[]) value;
            // valueStr = new String();
            // for (int i = 0; i < (array.length - 1); i++) {
            // valueStr += array[i].toString();
            // }
        } else if (value instanceof Short[]) {
            Preferences.debug("Short[] - skipped:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);

            // Short[] array = (Short[]) value;
            // valueStr = new String();
            // for (int i = 0; i < array.length; i++) {
            // valueStr += array[i].toString();
            // }
        } else if (value instanceof Integer[]) {
            Preferences.debug("Int[] - skipped:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);

            // Integer[] array = (Integer[]) value;
            // valueStr = new String();
            // for (int i = 0; i < array.length; i++) {
            // valueStr += array[i].toString();
            // }
        } else if (value instanceof Float[]) {
            Preferences.debug("Float[] - skipped:\t" + group + "," + element + "\n", Preferences.DEBUG_FILEIO);

            // Float[] array = (Float[]) value;
            // valueStr = new String();
            // for (int i = 0; i < array.length; i++) {
            // valueStr += array[i].toString();
            // }
        }

        return valueStr;
    }

    /**
     * Gets the next element, switching on the type.
     *
     * @param   type       Short, byte, float, etc, defined in FileInfoMinc.
     * @param   endianess  Endianess, FileBase.BIG_ENDIAN or FileBase.LITTLE_ENDIAN.
     *
     * @return  The element read in.
     *
     * @throws  IOException  if an error is encountered reading from the file
     */
    private Object getNextElem(int type, boolean endianess) throws IOException {
        Object value = null;

        switch (type) {

            case FileInfoMinc.NC_BYTE:
                value = new Byte(raFile.readByte());
                location++;
                break;

            case FileInfoMinc.NC_CHAR:
                value = new Character((char) raFile.readByte());
                location++;
                break;

            case FileInfoMinc.NC_SHORT:
                value = new Short((short) getUnsignedShort(endianess));
                location += 2;
                break;

            case FileInfoMinc.NC_INT:
                value = new Integer(getInt(endianess));
                location += 4;
                break;

            case FileInfoMinc.NC_FLOAT:
                value = new Float(getFloat(endianess));
                location += 4;
                break;

            case FileInfoMinc.NC_DOUBLE:
                value = new Double(getDouble(endianess));
                location += 8;
                break;

            default:
                MipavUtil.displayError("Invalid type in FileMinc.getNextElement");
        }

        return value;
    }

    /**
     * Returns the amount of padding needed (to the nearest 4 byte boundary) after a variable of length <code>
     * size</code>.
     *
     * @param   size  The length of the variable.
     *
     * @return  The amount of padding needed for that variable.
     */
    private int getPadding(int size) {
        int write = 0;

        while ((size % 4) != 0) {
            size++;
            write++;
        }

        return write;
    }

    /**
     * Get the size, in bytes, of the data which would be written to the header to store a set of dicom tags.
     *
     * @param   tagTable  hashtable of exported dicom tags. Hashtable inside of a Hashtable ultimately storing Strings
     *                    (<code>tagValue = table[tagGroup][tagElement]</code>).
     *
     * @return  the size of the data which will be written to the header for the exported dicom tags
     */
    private int getSizeOfExportedDicomTags(Hashtable tagTable) {

        // figure out the amount to adjust START3D by due to dicom-exported tags
        int exportedTagsSize = 0;
        Enumeration groupEnum = tagTable.keys();
        while (groupEnum.hasMoreElements()) {
            String group = (String) groupEnum.nextElement();

            // writeName("dicom_0x" + group, 0, endianess);
            exportedTagsSize += getSizeOfWrittenName("dicom_0x" + group, 0);

            // writeInt(1, endianess);
            exportedTagsSize += 4;

            // writeInt(0, endianess);
            exportedTagsSize += 4;

            // writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess);
            exportedTagsSize += 4;

            // writeInt(1, endianess); -- number of subvars (actual number doesn't matter for this size calc
            exportedTagsSize += 4;

            // ...calc the size of the attributes elsewhere...
            Enumeration elemEnum = ((Hashtable) tagTable.get(group)).keys();
            while (elemEnum.hasMoreElements()) {
                String element = (String) elemEnum.nextElement();
                String value = (String) ((Hashtable) tagTable.get(group)).get(element);

                // writeName("el_0x" + element, 0, endianess);
                exportedTagsSize += getSizeOfWrittenName("el_0x" + element, 0);

                // writeInt(FileInfoMinc.NC_CHAR, endianess);
                exportedTagsSize += 4;

                // writeName(value, 1, endianess);
                exportedTagsSize += getSizeOfWrittenName(value, 1);
            }

            // writeInt(FileInfoMinc.NC_INT, endianess); -- type of placeholder value pointed to within non-header
            // portion of file
            exportedTagsSize += 4;

            // writeInt(4, endianess); -- size (in bytes) of placeholder value pointed to within non-header portion of
            // file
            exportedTagsSize += 4;

            // writeInt(imgBegin + imgSize + pad + (8 * nImages) + (8 * nImages), endianess); -- beginning location of
            // placeholder value pointed to within non-header portion of file
            exportedTagsSize += 4;
        }

        return exportedTagsSize;
    }

    /**
     * Returns the number of bytes that would be written to disk by calling <code>writeName()</code> on a given string.
     *
     * @param   string     the string which would be passed to <code>writeName()</code>
     * @param   addedByte  the number of bytes of explicit padding to put after the string (see the second param of
     *                     <code>writeName()</code>)
     *
     * @return  the number of bytes that would be written to disk if the given string was passed to <code>
     *          writeName()</code>
     *
     * @see     #writeName(String, int, boolean)
     */
    private int getSizeOfWrittenName(String string, int addedByte) {
        return string.length() + getPadding(string.length() + addedByte) + 4 + addedByte;
    }

    /**
     * Gets the attribute array within a variable.
     *
     * @param   index     Index into the variable array; i.e., which variable this is.
     * @param   fileInfo  The file info to fill with data from the Vatt array read in from the minc file
     *
     * @throws  IOException  If an error is encountered while reading from the file
     */
    private void getVattArray(int index, FileInfoMinc fileInfo) throws IOException {
        String attrString;
        int next = getInt(endianess);
        Preferences.debug("var[" + index + "] vatt_array has " + next + " elements\n", Preferences.DEBUG_FILEIO);
        fileInfo.getVarElem(index).createVattArray(next);
        location += 4;

        // get the attributes - same as gattArray...
        for (int i = 0; i < next; i++) {
            attrString = new String();

            int len = getInt(endianess);
            Preferences.debug("var[" + index + "] vatt_array[" + i + "] string name length = " + len + "\n",
                              Preferences.DEBUG_FILEIO);

            String name = getString(len);
            Preferences.debug("var[" + index + "] vatt_array[" + i + "] string name = " + name + "\n",
                              Preferences.DEBUG_FILEIO);
            location += 4 + len;
            padding();

            int type = getInt(endianess);

            switch (type) {

                case 1:
                    Preferences.debug("var[" + index + "] vatt_array[" + i + "] type = 1 for NC_BYTE\n",
                                      Preferences.DEBUG_FILEIO);
                    break;

                case 2:
                    Preferences.debug("var[" + index + "] vatt_array[" + i + "] type = 2 for NC_CHAR\n",
                                      Preferences.DEBUG_FILEIO);
                    break;

                case 3:
                    Preferences.debug("var[" + index + "] vatt_array[" + i + "] type = 3 for NC_SHORT\n",
                                      Preferences.DEBUG_FILEIO);
                    break;

                case 4:
                    Preferences.debug("var[" + index + "] vatt_array[" + i + "] type = 4 for NC_INT\n",
                                      Preferences.DEBUG_FILEIO);
                    break;

                case 5:
                    Preferences.debug("var[" + index + "] vatt_array[" + i + "] type = 5 for NC_FLOAT\n",
                                      Preferences.DEBUG_FILEIO);
                    break;

                case 6:
                    Preferences.debug("var[" + index + "] vatt_array[" + i + "] type = 6 for NC_DOUBLE\n",
                                      Preferences.DEBUG_FILEIO);
                    break;

                default:
                    Preferences.debug("var[" + index + "] vatt_array[" + i + "] type illegally = " + type + "\n",
                                      Preferences.DEBUG_FILEIO);
            }

            location += 4;
            len = getInt(endianess);
            Preferences.debug("var[" + index + "] vatt_array[" + i + "] length = " + len + "\n",
                              Preferences.DEBUG_FILEIO);

            // add this attribute to the variable's attribute array
            fileInfo.getVarElem(index).addVattElem(name, type, len, i);
            location += 4;

            for (int j = 0; j < len; j++) {
                Object value = getNextElem(type, endianess);

                switch (type) {

                    case 1:
                        Preferences.debug("var[" + index + "] vatt_array[" + i + "][" + j + "] = " +
                                          ((Byte) (value)).byteValue() + "\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 2:
                        attrString += ((Character) (value)).charValue();
                        break;

                    case 3:
                        Preferences.debug("var[" + index + "] vatt_array[" + i + "][" + j + "] = " +
                                          ((Short) (value)).shortValue() + "\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 4:
                        Preferences.debug("var[" + index + "] vatt_array[" + i + "][" + j + "] = " +
                                          ((Integer) (value)).intValue() + "\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 5:
                        Preferences.debug("var[" + index + "] vatt_array[" + i + "][" + j + "] = " +
                                          ((Float) (value)).floatValue() + "\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 6:
                        Preferences.debug("var[" + index + "] vatt_array[" + i + "][" + j + "] = " +
                                          ((Double) (value)).doubleValue() + "\n", Preferences.DEBUG_FILEIO);
                        break;
                }

                fileInfo.getVarElem(index).addVattValue(fileInfo.getVarElem(index).getVattElem(i), value, j);
            }

            if ((type == 2) && (attrString != null)) {
                Preferences.debug("var[" + index + "] vatt_array[" + i + "] = " + attrString.trim() + "\n",
                                  Preferences.DEBUG_FILEIO);
            }

            padding();
        }

    }

    /**
     * Pads to the nearest 4 byte boundary. Everything in MINC files is padded this way.
     *
     * @throws  IOException  If an error is encountered while reading from the file
     */
    private void padding() throws IOException {

        while ((location % 4) != 0) {
            raFile.readByte();
            location++;
        }
    }

    /**
     * Writes the dicom-extracted tags contained in <code>tagTable</code> to disk.
     *
     * @param   tagTable         a tag-group keyed table, containing a tag-element keyed table of string values
     * @param   beginningOffset  the location, in bytes, directly after the data section pointed to by the previous
     *                           NC_VARIABLE section (usually image-min)
     *
     * @throws  IOException  If an error is encountered while writing to the file
     *
     * @see     #extractDicomTags(FileInfoBase)
     */
    private void writeDicomTagsToHeader(Hashtable tagTable, int beginningOffset) throws IOException {
        Enumeration groupEnum = tagTable.keys();
        int i = 0;
        while (groupEnum.hasMoreElements()) {
            String group = (String) groupEnum.nextElement();


            writeName("dicom_0x" + group, 0, endianess);

            writeInt(1, endianess);
            writeInt(0, endianess);

            writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess);

            Enumeration elementEnum = ((Hashtable) tagTable.get(group)).keys();
            writeInt(((Hashtable) tagTable.get(group)).size(), endianess);

            while (elementEnum.hasMoreElements()) {
                String element = (String) elementEnum.nextElement();
                String elementValue = (String) ((Hashtable) tagTable.get(group)).get(element);

                writeName("el_0x" + element, 0, endianess);
                writeInt(FileInfoMinc.NC_CHAR, endianess);
                writeName(elementValue, 1, endianess);
            }

            writeInt(FileInfoMinc.NC_INT, endianess);
            writeInt(4, endianess);
            writeInt(beginningOffset + (i * 4), endianess);

            i++;
        }
    }

    /**
     * Writes a header for MINC to MINC. This is much easier than the other-format-to-MINC writeHeader; in this case, we
     * already have all the information we need to write. Therefore, the process is just to go through the dimArray,
     * gattArray, and varArray and write out the variables.
     *
     * @param   fileInfo  File info needed to write the header.
     *
     * @throws  IOException  If an error is encountered while writing to the file
     */
    private void writeHeader(FileInfoMinc fileInfo) throws IOException {
        boolean endianess = fileInfo.getEndianess();
        raFile.writeBytes("CDF" + '\001'); // write the magic number
        writeInt(fileInfo.numrecs, endianess); // write the numrecs (usually 0)

        if (fileInfo.getDimArray() != null) { // write all dimension variables
            writeInt(FileInfoMinc.NC_DIMENSION, endianess);
            writeInt(fileInfo.getDimArray().length, endianess);

            for (int i = 0; i < fileInfo.getDimArray().length; i++) {
                writeName(fileInfo.getDimElem(i).name, 0, endianess);
                writeInt(fileInfo.getDimElem(i).length, endianess);
            }
        } else {
            writeInt(0, endianess);
            writeInt(0, endianess);
        }

        if (fileInfo.getGattArray() != null) { // write all global attribute variables
            writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess);
            writeInt(fileInfo.getGattArray().length, endianess);

            for (int i = 0; i < fileInfo.getGattArray().length; i++) {
                Preferences.debug("writing global att: " + fileInfo.getGattElem(i).name + "\n",
                                  Preferences.DEBUG_FILEIO);
                writeName(fileInfo.getGattElem(i).name, 0, endianess);
                writeInt(fileInfo.getGattElem(i).nc_type, endianess);
                writeInt(fileInfo.getGattElem(i).values.length, endianess);
                writeValuesArray(fileInfo.getGattElem(i).values, fileInfo.getGattElem(i).nc_type, endianess);
            }
        } else {
            writeInt(0, endianess);
            writeInt(0, endianess);
        }

        if (fileInfo.getVarArray() != null) { // write the variables
            writeInt(FileInfoMinc.NC_VARIABLE, endianess);
            writeInt(fileInfo.getVarArray().length, endianess);

            for (int i = 0; i < fileInfo.getVarArray().length; i++) {
                Preferences.debug("writing var: " + fileInfo.getVarElem(i).name + "\n", Preferences.DEBUG_FILEIO);
                writeName(fileInfo.getVarElem(i).name, 0, endianess); // write name
                writeInt(fileInfo.getVarElem(i).dimid.length, endianess); // write dim id

                for (int j = 0; j < fileInfo.getVarElem(i).dimid.length; j++) {
                    writeInt(fileInfo.getVarElem(i).dimid[j], endianess);
                }

                writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess);
                writeInt(fileInfo.getVarElem(i).vattArray.length, endianess);

                for (int j = 0; j < fileInfo.getVarElem(i).vattArray.length; j++) { // write attribute array
                    Preferences.debug("writing var att: " + fileInfo.getVarElem(i).vattArray[j].name + "\n",
                                      Preferences.DEBUG_FILEIO);
                    writeName(fileInfo.getVarElem(i).vattArray[j].name, 0, endianess);
                    writeInt(fileInfo.getVarElem(i).vattArray[j].nc_type, endianess);
                    writeInt(fileInfo.getVarElem(i).vattArray[j].values.length, endianess);
                    writeValuesArray(fileInfo.getVarElem(i).vattArray[j].values,
                                     fileInfo.getVarElem(i).vattArray[j].nc_type, endianess);
                }

                writeInt(fileInfo.getVarElem(i).nc_type, endianess);
                writeInt(fileInfo.getVarElem(i).vsize, endianess);
                writeInt(fileInfo.getVarElem(i).begin, endianess);
            }
        } else {
            writeInt(0, endianess);
            writeInt(0, endianess);
        }

    }

    /**
     * Writes the header for non-MINC to MINC. MINC has a very structured format. Many of the things written out I don't
     * understand, but they seem to be consistent across the MINC files we've seen (e.g., varid, vartype, and version
     * attributes). The header begins with a dimension array with a specific ordering of the spaces which dictates
     * orientation. Then there's a global attribute array, which in our case is just the history of how this file was
     * created. Then there's a variable array. Each variable contains an attribute array, a type, a size, and a begin
     * location. The actual value of the variable is written at its begin location, after all other header info is
     * written. So the rootvariable's value, for example, is written after the info for image-min, which is the last
     * variable in the array. It seems that conventionally the image-max and image-min variables are after the image
     * variable; thus, the values of those variables are written after the "value of the image" which is the actual
     * image data. Consequently, this function exits just before the image is to be written, even though the image-max
     * and image-min values have yet to be written.
     *
     * <p>This is a public method because it is called from FileIO. Previously it was called from writeImage, but this
     * would create a file even if the dialog was cancelled. Obviously that is undesirable behavior.</p>
     *
     * @param   fileInfo  Info to use when writing the header.
     * @param   options   The structure that returns important information about the image to be written.
     *
     * @throws  IOException  If an error is encountered while writing to the file
     */
    private void writeHeader(FileInfoBase fileInfo, FileWriteOptions options) throws IOException {
        int currentNonHeaderStartLocation = DEFAULT_NON_HEADER_START_LOCATION;

        int nImages = options.getEndSlice() - options.getBeginSlice() + 1;

        endianess = FileBase.BIG_ENDIAN; // seems to always be big endian;
                                         // at least, that's what Display reads
        raFile.writeBytes("CDF" + '\001'); // indicates NetCDF
        writeInt(0, endianess); // number of records, don't know what means

        writeInt(FileInfoMinc.NC_DIMENSION, endianess); // dimension array
        writeInt(fileInfo.getExtents().length, endianess); // number of dimensions

        // orientation of the image.
        switch (options.getAxisOrientation()[2]) {

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                writeName("xspace", 0, endianess);
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                writeName("yspace", 0, endianess);
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
            default:
                writeName("zspace", 0, endianess);
        }

        writeInt(nImages, endianess);

        switch (options.getAxisOrientation()[1]) {

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                writeName("xspace", 0, endianess);
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                writeName("yspace", 0, endianess);
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                writeName("zspace", 0, endianess);
                break;

            default:
                writeName("yspace", 0, endianess);
        }

        writeInt(fileInfo.getExtents()[1], endianess);

        switch (options.getAxisOrientation()[0]) {

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                writeName("xspace", 0, endianess);
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                writeName("yspace", 0, endianess);
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                writeName("zspace", 0, endianess);
                break;

            default:
                writeName("xspace", 0, endianess);
        }

        writeInt(fileInfo.getExtents()[0], endianess);


        writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess); // global attribute array
        writeInt(1, endianess); // one attribute: history
        writeName("history", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);

        Date d = new Date();
        String s = "" + d + " >> File created by MIPAV from " + fileInfo.getFileName() + "\n";
        writeInt(s.length(), endianess);
        raFile.writeBytes(s);
        location = s.length();
        writePadding();

        // expand the starting point for the non-header data downward to compensate for data we have written to the
        // history attribute
        currentNonHeaderStartLocation += location;

        writeInt(FileInfoMinc.NC_VARIABLE, endianess); // variable array

        // we need to export any and all dicom tags early so that we can figure out the number of NC_VARIABLEs and the
        // amount to adjust START3D by
        dicomConvertedTagTable = extractDicomTags(fileInfo);

        // adding exported dicom tags moves the start of the non-header portion of the file downwards
        currentNonHeaderStartLocation += getSizeOfExportedDicomTags(dicomConvertedTagTable);

        // the number of NC_VARIABLE entries (7 for basic image info hardcoded below + any dicom-exported tag groups)
        writeInt(7 + dicomConvertedTagTable.size(), endianess);

        writeName("rootvariable", 0, endianess); // always in MINC files (not sure what it means)
        writeInt(0, endianess);
        writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess); // attribute array within variable
        writeInt(5, endianess); // five attributes
        writeName("varid", 0, endianess); // attribute 1
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC standard variable", 1, endianess); // always the value
        writeName("vartype", 0, endianess); // attribute 2
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("group________", 1, endianess); // this is a "group" variable (not sure what it means)
        writeName("version", 0, endianess); // attribute 3
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC Version    1.0", 1, endianess); // always the value
        writeName("parent", 0, endianess); // attribute 4: no parent, but null character written - don't know why
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeInt(1, endianess);
        raFile.writeByte((byte) 0); // null character
        location = 1;
        writePadding(); // pad to four byte boundary
        writeName("children", 0, endianess); // attribute 5
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("image", 1, endianess);

        writeInt(FileInfoMinc.NC_INT, endianess); // type of variable
        writeInt(4, endianess); // size of variable

        writeInt(currentNonHeaderStartLocation, endianess);

        double xSpace = options.getXSpace();
        double ySpace = options.getYSpace(), zSpace = 1;

        if (fileInfo.getExtents().length == 3) {
            zSpace = options.getZSpace();
        }

        if (fileInfo.getExtents().length == 3) {

            switch (options.getAxisOrientation()[2]) {

                case FileInfoBase.ORI_L2R_TYPE:
                    writeSpace(xSpace, options.getXStart(), "xspace", endianess, true);
                    break;

                case FileInfoBase.ORI_R2L_TYPE:
                    writeSpace(xSpace, options.getXStart(), "xspace", endianess, false);
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                    writeSpace(ySpace, options.getYStart(), "yspace", endianess, false);
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    writeSpace(ySpace, options.getYStart(), "yspace", endianess, true);
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                    writeSpace(zSpace, options.getZStart(), "zspace", endianess, true);
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    writeSpace(zSpace, options.getZStart(), "zspace", endianess, false);
                    break;

                default:
                    writeSpace(zSpace, options.getZStart(), "zspace", endianess, true);
            }

            writeInt(FileInfoMinc.NC_DOUBLE, endianess);
            writeInt(8, endianess);
            writeInt(currentNonHeaderStartLocation + 4, endianess);

            switch (options.getAxisOrientation()[1]) {

                case FileInfoBase.ORI_L2R_TYPE:
                    writeSpace(xSpace, options.getXStart(), "xspace", endianess, true);
                    break;

                case FileInfoBase.ORI_R2L_TYPE:
                    writeSpace(xSpace, options.getXStart(), "xspace", endianess, false);
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                    writeSpace(ySpace, options.getYStart(), "yspace", endianess, false);
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    writeSpace(ySpace, options.getYStart(), "yspace", endianess, true);
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                    writeSpace(zSpace, options.getZStart(), "zspace", endianess, true);
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    writeSpace(zSpace, options.getZStart(), "zspace", endianess, false);
                    break;

                default:
                    writeSpace(ySpace, options.getYStart(), "yspace", endianess, true);
            }

            writeInt(FileInfoMinc.NC_DOUBLE, endianess);
            writeInt(8, endianess);
            writeInt(currentNonHeaderStartLocation + 12, endianess);

            switch (options.getAxisOrientation()[0]) {

                case FileInfoBase.ORI_L2R_TYPE:
                    writeSpace(xSpace, options.getXStart(), "xspace", endianess, true);
                    break;

                case FileInfoBase.ORI_R2L_TYPE:
                    writeSpace(xSpace, options.getXStart(), "xspace", endianess, false);
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                    writeSpace(ySpace, options.getYStart(), "yspace", endianess, false);
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    writeSpace(ySpace, options.getYStart(), "yspace", endianess, true);
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                    writeSpace(zSpace, options.getZStart(), "zspace", endianess, true);
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    writeSpace(zSpace, options.getZStart(), "zspace", endianess, false);
                    break;

                default:
                    writeSpace(xSpace, options.getXStart(), "xspace", endianess, true);
            }

            writeInt(FileInfoMinc.NC_DOUBLE, endianess);
            writeInt(8, endianess);
            writeInt(currentNonHeaderStartLocation + 20, endianess);
        }

        writeName("image", 0, endianess);
        writeInt(fileInfo.getExtents().length, endianess);

        for (int k = 0; k < fileInfo.getExtents().length; k++) {
            writeInt(k, endianess);
        }

        writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess);
        writeInt(9, endianess);
        writeName("parent", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("rootvariable", 1, endianess);
        writeName("varid", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC standard variable", 1, endianess);
        writeName("vartype", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("group________", 1, endianess);
        writeName("version", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC Version    1.0", 1, endianess);
        writeName("complete", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("true_", 1, endianess);
        writeName("signtype", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);

        if ((fileInfo.getDataType() == ModelStorageBase.UBYTE) || (fileInfo.getDataType() == ModelStorageBase.USHORT) ||
                (fileInfo.getDataType() == ModelStorageBase.UINTEGER)) {
            writeName("unsigned", 1, endianess);
        } else {
            writeName("signed__", 1, endianess);
        }

        writeName("valid_range", 0, endianess);
        writeInt(FileInfoMinc.NC_DOUBLE, endianess);
        writeInt(2, endianess);

        if (fileInfo.getFileFormat() == FileBase.MINC) {
            writeDouble(((FileInfoMinc) (fileInfo)).vmin, endianess);
            writeDouble(((FileInfoMinc) (fileInfo)).vmax, endianess);
        } else {
            writeDouble(fileInfo.getMin(), endianess);
            writeDouble(fileInfo.getMax(), endianess);
        }

        writeName("image-max", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("--->image-max", 1, endianess);
        writeName("image-min", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("--->image-min", 1, endianess);


        int size = 1;

        switch (fileInfo.getDataType()) {

            case ModelStorageBase.UBYTE:
            case ModelStorageBase.BYTE:
                writeInt(FileInfoMinc.NC_BYTE, endianess);
                size = 1;
                break;

            case ModelStorageBase.USHORT:
            case ModelStorageBase.SHORT:
                writeInt(FileInfoMinc.NC_SHORT, endianess);
                size = 2;
                break;

            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.INTEGER:
                writeInt(FileInfoMinc.NC_INT, endianess);
                size = 4;
                break;

            case ModelStorageBase.FLOAT:
                writeInt(FileInfoMinc.NC_FLOAT, endianess);
                size = 4;
                break;

            case ModelStorageBase.DOUBLE:
                writeInt(FileInfoMinc.NC_DOUBLE, endianess);
                size = 8;
                break;

            default:
                throw new IOException("Undefined type in FileMinc.writeHeader");
        }

        if (fileInfo.getExtents().length == 3) {
            writeInt(fileInfo.getExtents()[0] * fileInfo.getExtents()[1] * nImages * size, endianess);
            writeInt(currentNonHeaderStartLocation + 28, endianess);
            imgBegin = currentNonHeaderStartLocation + 28;
            imgSize = fileInfo.getExtents()[0] * fileInfo.getExtents()[1] * nImages * size;
        }


        int pad = getPadding(imgBegin + imgSize);

        writeName("image-max", 0, endianess);
        writeInt(1, endianess);
        writeInt(0, endianess);
        writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess);
        writeInt(5, endianess);
        writeName("varid", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC standard variable", 1, endianess);
        writeName("vartype", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("var_attribute", 1, endianess);
        writeName("version", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC Version    1.0", 1, endianess);
        writeName("_FillValue", 0, endianess);
        writeInt(FileInfoMinc.NC_DOUBLE, endianess);
        writeInt(1, endianess);
        writeDouble(1, endianess);
        writeName("parent", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("image", 1, endianess);
        writeInt(FileInfoMinc.NC_DOUBLE, endianess);

        writeInt(8 * nImages, endianess);
        writeInt(imgBegin + imgSize + pad, endianess);

        writeName("image-min", 0, endianess);
        writeInt(1, endianess);
        writeInt(0, endianess);
        writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess);
        writeInt(5, endianess);
        writeName("varid", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC standard variable", 1, endianess);
        writeName("vartype", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("var_attribute", 1, endianess);
        writeName("version", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC Version    1.0", 1, endianess);
        writeName("_FillValue", 0, endianess);
        writeInt(FileInfoMinc.NC_DOUBLE, endianess);
        writeInt(1, endianess);
        writeDouble(0, endianess);
        writeName("parent", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("image", 1, endianess);
        writeInt(FileInfoMinc.NC_DOUBLE, endianess);

        writeInt(8 * nImages, endianess);
        writeInt(imgBegin + imgSize + pad + (8 * nImages), endianess);

        writeDicomTagsToHeader(dicomConvertedTagTable, imgBegin + imgSize + pad + (8 * nImages) + (8 * nImages));
    }

    /**
     * Write a string name by writing the length of the string plus padding length, writing the string, and writing the
     * padding.
     *
     * @param   name       Value to write.
     * @param   add        Length of padding to add.
     * @param   endianess  Endianess, FileBase.BIG_ENDIAN or FileBase.LITTLE_ENDIAN.
     *
     * @throws  IOException  If an error is encountered while writing to the file
     */
    private void writeName(String name, int add, boolean endianess) throws IOException {
        location = 0;
        writeInt(name.length() + add, endianess);
        raFile.writeBytes(name);

        if (add == 1) {
            raFile.write((byte) 0);
        }

        location += name.length() + add;
        writePadding();
    }

    /**
     * Writes the next element, switching on the type.
     *
     * @param   value      The value to write out.
     * @param   type       Short, byte, float, etc, defined in FileInfoMinc.
     * @param   endianess  Endianess, FileBase.BIG_ENDIAN or FileBase.LITTLE_ENDIAN.
     *
     * @throws  IOException  If an error is encountered while writing to the file
     */
    private void writeNextElem(Object value, int type, boolean endianess) throws IOException {

        switch (type) {

            case FileInfoMinc.NC_BYTE:
                raFile.write(((Byte) value).byteValue());
                location++;
                break;

            case FileInfoMinc.NC_CHAR:
                raFile.writeBytes(String.valueOf(((Character) value).charValue()));
                location++;
                break;

            case FileInfoMinc.NC_SHORT:
                writeShort(((Short) value).shortValue(), endianess);
                location += 2;
                break;

            case FileInfoMinc.NC_INT:
                writeInt(((Integer) value).intValue(), endianess);
                location += 4;
                break;

            case FileInfoMinc.NC_FLOAT:
                writeFloat(((Float) value).floatValue(), endianess);
                location += 4;
                break;

            case FileInfoMinc.NC_DOUBLE:
                writeDouble(((Double) value).doubleValue(), endianess);
                location += 8;
                break;

            default:
                MipavUtil.displayError("Invalid type in FileMinc.writeNextElement");
        }
    }


    /**
     * Pads to the nearest 4 byte boundary. Everything in MINC files is padded this way.
     *
     * @throws  IOException  If an error is encountered while writing to the file
     */
    private void writePadding() throws IOException {

        while ((location % 4) != 0) {
            raFile.write((byte) 0);
            location++;
        }
    }

    /**
     * Writes the "space", as in xspace, yspace, zspace. The space gives information on that dimension, such as length
     * (what we call extents), step, and start. Start tells the real talirach value of where the space starts, so -50
     * would be something like 50 inches away from the center of the brain to the right, depending on which space we're
     * in. Step is the step you take per slice from the start, so the third slice in would be start + 3*step.
     *
     * @param   step       Step, see above.
     * @param   start      Start, see above.
     * @param   space      The string label used for the space ('xspace', 'yspace', or 'zspace')
     * @param   endianess  Endianess, FileBase.BIG_ENDIAN or FileBase.LITTLE_ENDIAN.
     * @param   isNormal   No longer used to determine the space direction comment. Consider removal.
     *
     * @throws  IOException  If an error is encountered while writing to the file
     */
    private void writeSpace(double step, double start, String space, boolean endianess, boolean isNormal)
            throws IOException {
        writeName(space, 0, endianess);
        writeInt(0, endianess);
        writeInt(FileInfoMinc.NC_ATTRIBUTE, endianess);
        writeInt(9, endianess);
        writeName("varid", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC standard variable", 1, endianess);
        writeName("vartype", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("dimension____", 1, endianess);
        writeName("version", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("MINC Version    1.0", 1, endianess);
        writeName("comments", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);

        if (space.equals("xspace")) {
            writeName("X increases from patient left to right", 1, endianess);
        } else if (space.equals("yspace")) {
            writeName("Y increases from patient posterior to anterior", 1, endianess);
        } else if (space.equals("zspace")) {
            writeName("Z increases from patient inferior to superior", 1, endianess);
        }

        writeName("spacing", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("regular__", 1, endianess);
        writeName("alignment", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("centre", 1, endianess);
        writeName("step", 0, endianess);
        writeInt(FileInfoMinc.NC_DOUBLE, endianess);
        writeInt(1, endianess);
        writeDouble(step, endianess);
        writeName("start", 0, endianess);
        writeInt(FileInfoMinc.NC_DOUBLE, endianess);
        writeInt(1, endianess);
        writeDouble(start, endianess);
        writeName("units", 0, endianess);
        writeInt(FileInfoMinc.NC_CHAR, endianess);
        writeName("mm", 1, endianess); // should get this from file, but don't know what other measurements are -inches
                                       // "in"?

    }

    /**
     * Writes the array, switching on the type.
     *
     * @param   values     Array to write.
     * @param   type       Short, byte, float, etc, defined in FileInfoMinc.
     * @param   endianess  Endianess, FileBase.BIG_ENDIAN or FileBase.LITTLE_ENDIAN.
     *
     * @throws  IOException  If an error is encountered while writing to the file
     */
    private void writeValuesArray(Object[] values, int type, boolean endianess) throws IOException {

        int i;

        switch (type) {

            case FileInfoMinc.NC_BYTE:
                for (i = 0; i < values.length; i++) {
                    raFile.write(((Byte) values[i]).byteValue());
                }

                location = values.length;
                writePadding();
                break;

            case FileInfoMinc.NC_CHAR:

                String s = "";
                for (i = 0; i < values.length; i++) {
                    s += ((Character) values[i]).charValue();
                }

                raFile.writeBytes(s);
                location = values.length;
                writePadding();
                break;

            case FileInfoMinc.NC_SHORT:
                for (i = 0; i < values.length; i++) {
                    writeShort(((Short) values[i]).shortValue(), endianess);
                }

                location = values.length * 2;
                writePadding();
                break;

            case FileInfoMinc.NC_INT:
                for (i = 0; i < values.length; i++) {
                    writeInt(((Integer) values[i]).intValue(), endianess);
                }

                break;

            case FileInfoMinc.NC_FLOAT:
                for (i = 0; i < values.length; i++) {
                    writeFloat(((Float) values[i]).floatValue(), endianess);
                }

                break;

            case FileInfoMinc.NC_DOUBLE:
                for (i = 0; i < values.length; i++) {
                    writeDouble(((Double) values[i]).doubleValue(), endianess);
                }

                break;

            default:
                MipavUtil.displayError("Invalid type in FileMinc.writeValuesArray");
        }
    }
}
