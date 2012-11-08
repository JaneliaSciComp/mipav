package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Hashtable;


/**
 *
 * @version  0.1 May 27, 2009
 * @author   Andrew Morse
 * @see      FileIO
 * 
 */

public class FileSiemensText extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The extensions of SiemensText file. */
    public static final String[] EXTENSIONS = { ".hdr", ".img" };

    /** SiemensText file format define use to indicate undefined image data type. */
    public static final short DT_UNKNOWN = 0;

    /** SiemensText file format define use to indicate Byte (8-bits) data type */
    public static final short DT_BYTE = 1;

    /** SiemensText file format define use to indicate 2-byte integer - Intel style. */
    public static final short DT_2INTEGERI = 2;
    
    /** SiemensText file format define use to indicate 4-byte integer - Intel style. */
    public static final short DT_4INTEGERI = 3;

    /** SiemensText file format define use to indicate 2-byte float - Intel style. */
    public static final short DT_2FLOAT = 4;
    
    /** SiemensText file format define use to indicate 4-byte float - Sun style. */
    public static final short DT_4FLOAT = 5;

    /** SiemensText file format define use to indicate 2-byte integer - Sun style. */
    public static final short DT_2INTEGERS = 2;
    
    /** SiemensText file format define use to indicate 4-byte integer - Sun style. */
    public static final short DT_4INTEGERS = 3;





    //~ Instance fields ------------------------------------------------------------------------------------------------

    
    Hashtable<String, String> storage = new Hashtable<String,String>();
    
    /** File directory of the image. */
    private String fileDir;

    /** Reference to the header file (*.hdr). */
    private File fileHeader;
    
    /** File name of the image. */
    private String fileName;
    
    /** Reference to the file info. for an SiemensText header */
    private FileInfoSiemensText fileInfo = null;

    /** The image read in from the file. */
    private ModelImage image;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileSiemensText(String fName, String fDir) {
        String fileDataName;
        File fileData;
        int index = fName.length();

        for (int i = fName.length() - 1; i >= 0; i--) {

            if (fName.charAt(i) == '.') {
                index = i;

                break;
            }
        }
        if (fName.substring(index).equalsIgnoreCase(".HDR")) {
            fileDataName = fName.substring(0, index);
            fileData = new File(fDir + fileDataName);
            if (fileData.exists()) {
                fName = fileDataName;
            }
            else {
                fileDataName = fName.substring(0, index) + ".IMG";
                fileData = new File(fDir + fileDataName); 
                if (fileData.exists()) {
                    fName = fileDataName;
                }
            }
        }
        fileName = fName;
        fileDir = fDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        fileName = null;
        fileDir = null;
        fileInfo = null;
        image = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    

    /**
     * Returns the complete list of file names according to given file name.
     *
     * @param   absolutePath  one file name of SiemensText.
     *
     * @return  the complete list of file names.
     */
    public static String[] getCompleteFileNameList(String absolutePath) {
        String[] completeFileNameList = new String[2];

        if (isHeaderFile(absolutePath)) {
            completeFileNameList[0] = absolutePath;

            completeFileNameList[1] = absolutePath.substring(0, absolutePath.lastIndexOf(".")) + EXTENSIONS[1];
        } else if (isImageFile(absolutePath)) {
            completeFileNameList[1] = absolutePath;

            completeFileNameList[0] = absolutePath.substring(0, absolutePath.lastIndexOf(".")) + EXTENSIONS[0];
        } else {
            completeFileNameList = null;
        }

        return completeFileNameList;
    }

    /**
     * Returns the header file (ends in .hdr) as a string.
     *
     * @param   fileNames  DOCUMENT ME!
     *
     * @return  The header file (ends in .hdr)
     */
    public static String getHeaderFile(String[] fileNames) {

        if ((fileNames == null) || (fileNames.length != 2)) {
            return null;
        }

        for (int i = 0; i < fileNames.length; i++) {

            if (FileUtility.getExtension(fileNames[i]).equals(EXTENSIONS[0])) {
                return fileNames[i];
            }
        }

        return null;
    }

    /**
     * Returns the image file list.
     *
     * @param   fileNames  DOCUMENT ME!
     *
     * @return  the image file list.
     */
    public static String[] getImageFiles(String[] fileNames) {

        if (fileNames == null) {
            return null;
        }

        String[] result = new String[1];

        for (int i = 0; i < fileNames.length; i++) {

            if (FileUtility.getExtension(fileNames[i]).equals(EXTENSIONS[1])) {
                result[0] = fileNames[i];

                return result;
            }
        }

        return null;
    }


    /**
     * Return true if the file specified by absolutePath is header file of SiemensText.
     *
     * @param   absolutePath  the file name including path information.
     *
     * @return  true if the specified file is header file.
     */
    public static boolean isHeaderFile(String absolutePath) {
        String fileName = FileUtility.getFileName(absolutePath);
        String extension = FileUtility.getExtension(fileName);

        if (extension.equalsIgnoreCase(EXTENSIONS[0])) {
            return true;
        }

        return false;
    }

    /**
     * Return true if the file specified by absolutePath is image file of SiemensText.
     *
     * @param   absolutePath  the file name including path information.
     *
     * @return  true if the specified file is image file.
     */
    public static boolean isImageFile(String absolutePath) {
        String fileName = FileUtility.getFileName(absolutePath);
        String extension = FileUtility.getExtension(fileName);

        if (extension.equalsIgnoreCase(EXTENSIONS[1])) {
            return true;
        } else if (extension.length() == 0) { // Support for image file with no extension.
            return true;
        }

        return false;
    }
    
    public static boolean isSiemensText(String absolutePath) {
        String fileName = FileUtility.getFileName(absolutePath);

        int index = fileName.indexOf(".");

        if (index >= 0) {
            if(fileName.substring(index).equalsIgnoreCase(".img.hdr"))
            		return true;
        }


        return false;
    }

    /**
     * Returns the FileInfoSiemensText read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoSiemensText getFileInfo() {
        return fileInfo;
    }

    /**
     * Returns the header file.
     *
     * @return  DOCUMENT ME!
     */
    public String getHeaderFile() {

        if ((fileNames == null) || (fileNames.length != 2)) {
            return null;
        }

        for (int i = 0; i < fileNames.length; i++) {

            if (FileUtility.getExtension(fileNames[i]).equals(EXTENSIONS[0])) {
                return fileNames[i];
            }
        }

        return null;
    }

    /**
     * Returns the image file list.
     *
     * @return  the image file list.
     */
    public String[] getImageFiles() {

        if ((fileNames == null) || (fileNames.length != 2)) {
            return null;
        }

        String[] result = new String[1];

        for (int i = 0; i < fileNames.length; i++) {

            if (FileUtility.getExtension(fileNames[i]).equals(EXTENSIONS[1])) {
                result[0] = fileNames[i];

                return result;
            }
        }

        return null;
    }

    /**
     * Reads the SiemensText header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoSiemensText
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {
        int i;
        int index;
        String fileHeaderName;
        String bufferImageHeader;
        index = fileName.length();

        for (i = fileName.length() - 1; i >= 0; i--) {

            if (fileName.charAt(i) == '.') {
                index = i;

                break;
            }
        }

        fileHeaderName = fileName.substring(0) + ".hdr";
        Preferences.debug(" fileHeaderName = " + fileHeaderName + "\n", Preferences.DEBUG_FILEIO);
        fileHeader = new File(fileDir + fileHeaderName);

        if (fileHeader.exists() == false) {
            Preferences.debug(fileDir + fileHeaderName + " cannot be found.\n", Preferences.DEBUG_FILEIO);
            fileHeaderName = fileName.substring(0, index) + ".HDR";
            Preferences.debug("fileHeaderName = " + fileHeaderName + "\n", Preferences.DEBUG_FILEIO);
            fileHeader = new File(fileDir + fileHeaderName);

            if (fileHeader.exists() == false) {
                Preferences.debug(fileDir + fileHeaderName + " cannot be found.\n", Preferences.DEBUG_FILEIO);

                return false;
            }
        }

        // Required to read in multi-file SiemensText images. i.e. a set of 3D images to make a 4D dataset
        // The files should all have the same prefix. fooR_001.img, fooR_002.img etc.
        if (fileInfo == null) { // if the file info does not yet exist: make it
            Preferences.debug("fileInfo is null\n", Preferences.DEBUG_FILEIO);
            fileInfo = new FileInfoSiemensText(imageFileName, fileDir, FileUtility.SIEMENSTEXT);

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) { // Why 3/20/2001
                throw (new IOException(" SiemensText header file error"));
            }
        }

        try {
            raFile = new RandomAccessFile(fileHeader, "r");
        } catch (FileNotFoundException e) {
            Preferences.debug("raFile = new RandomAccessFile(fileHeader, r) gave " + "FileNotFoundException " + e +
            		"\n", Preferences.DEBUG_FILEIO);
            throw new IOException("Error on raFile = new RandomAccessFile(fileHeader,r)");
        }
    	int loc = 0;
        try {
        	bufferImageHeader = raFile.readLine();
        	String key;
        	String data;

        	while (bufferImageHeader != null)
        	{
        		loc = bufferImageHeader.indexOf(' ');
        		if (bufferImageHeader.charAt(0) != '#' && loc != -1){
        			key = bufferImageHeader.substring(0, loc);
        			data = bufferImageHeader.substring(loc + 1);
        			
        			storage.put(key, data);
        			
        		}
        			
        		
        		bufferImageHeader = raFile.readLine();
        	}
            
        } catch (IOException e) {
            Preferences.debug("raFile.read(bufferImageHeader gave IOException " + e + "\n", Preferences.DEBUG_FILEIO);
            throw new IOException(" Error on raFile.read(bufferImageHeader)");
        }

        try {
            raFile.close();
        } catch (IOException e) {
            Preferences.debug("raFile.close() gave IOException " + e + "\n", Preferences.DEBUG_FILEIO);
            throw new IOException(" Error on raFile.close()");
        }

        

        // The following reads in certain tags.  In some cases, it returns false and exits out of readHeader
        // if the information is wrong.
        fileInfo.setDataType(Short.parseShort((String)storage.get("data_type")));
        Preferences.debug("Data type name = " + fileInfo.getDataTypeName() + "\n", Preferences.DEBUG_FILEIO);


        int dims = Integer.parseInt((String) storage.get("number_of_dimensions")); // number of Dimensions should = 4

        // SiemensText dims = 3
        Preferences.debug("FileSiemensText:readHeader. Number of dimensions = " + dims + "\n", Preferences.DEBUG_FILEIO);
        int[] extents = new int[dims];
        
        extents[0] = Integer.parseInt((String) storage.get("x_dimension"));
        extents[1] = Integer.parseInt((String) storage.get("y_dimension"));
        
        if (dims == 3)
        	extents[2] = Integer.parseInt((String) storage.get("z_dimension"));

        fileInfo.setExtents(extents);

        
        Preferences.debug("FileSiemensText:readHeader. Data type = " + fileInfo.getDataTypeCode() + "\n", 
        		Preferences.DEBUG_FILEIO);

        switch (fileInfo.getDataTypeCode()) { // Set the dataType in ModelStorage based on this tag

            case FileInfoSiemensText.DT_UNKNOWN:
                return false;

            case FileInfoSiemensText.DT_BYTE:
            	fileInfo.setEndianess(BIG_ENDIAN);
                fileInfo.setDataType(ModelStorageBase.UBYTE);
                break;

            case FileInfoSiemensText.DT_2INTEGERI:
            	fileInfo.setEndianess(LITTLE_ENDIAN);
                fileInfo.setDataType(ModelStorageBase.SHORT);
                break;

            case FileInfoSiemensText.DT_2INTEGERS:
            	fileInfo.setEndianess(BIG_ENDIAN);
                fileInfo.setDataType(ModelStorageBase.SHORT);
                break;

            case FileInfoSiemensText.DT_4FLOATS:
            	fileInfo.setEndianess(BIG_ENDIAN);
                fileInfo.setDataType(ModelStorageBase.FLOAT);
                break;

            case FileInfoSiemensText.DT_4INTEGERI:
            	fileInfo.setEndianess(LITTLE_ENDIAN);
                fileInfo.setDataType(ModelStorageBase.INTEGER);
                break;

            case FileInfoSiemensText.DT_4INTEGERS:
            	fileInfo.setEndianess(BIG_ENDIAN);
                fileInfo.setDataType(ModelStorageBase.INTEGER);
                break;

            case FileInfoSiemensText.DT_4FLOATI:
            	fileInfo.setEndianess(LITTLE_ENDIAN);
                fileInfo.setDataType(ModelStorageBase.FLOAT);
                break;

            default:
                return false;
        }

        Preferences.debug("FileSiemensText:readHeader. bits per pixel = " + fileInfo.getBitPix() + "\n", 
        		Preferences.DEBUG_FILEIO);

        fileInfo.setDim((short) dims);

        int numDims = Integer.parseInt((String) storage.get("number_of_dimensions"));
        float[] resolutions = new float[numDims];
        resolutions[0] = 1;
        resolutions[1] = 1;
        
        if (numDims == 3)
        	resolutions[2] = 1;
        


        fileInfo.setResolutions(resolutions);


        fileInfo.setCalMin(Float.parseFloat((String) storage.get("minimum")));
        fileInfo.setCalMax(Float.parseFloat((String) storage.get("maximum")));


        // update the fileInfo modality based on the description
        // if the description contains something other than modality, then
        // the modality will be set to unknown.
        switch (Integer.parseInt((String) storage.get("modality"))){
        
    	case -1:
    		fileInfo.setModality(FileInfoBase.UNKNOWN_MODALITY);
    		break;
    		
    	case 0:
    		fileInfo.setModality(FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY);
    		break;
    		
    	case 1:
    		fileInfo.setModality(FileInfoBase.COMPUTED_TOMOGRAPHY);
    		break;
    		
    	case 2:
    		fileInfo.setModality(FileInfoBase.SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY);
    		break;
    		
    	default:
    		fileInfo.setModality(FileInfoBase.UNKNOWN_MODALITY);
    		
        		
        }


        fileInfo.setPatientID((String) storage.get("subject_identifier"));
        fileInfo.setExperimentDateandTime((String) storage.get("scan_time"));


        return true; // If it got this far, it has successfully read in the header
    }

    /**
     * Reads an SiemensText image file by reading the header then making a FileRaw to read the image for all filenames in
     * the file list. Only the one file directory (currently) supported.
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
        fileInfo = new FileInfoSiemensText(fileName, fileDir, FileUtility.SIEMENSTEXT);

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException(" SiemensText header file error"));
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



        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
            if (image.getType() == ModelStorageBase.BOOLEAN) {
                rawFile.setMinimumBitsMinus1(7);
                rawFile.setShiftToDivide(3);
            }
            int zdim = Integer.parseInt((String) storage.get("z_dimension"));
            linkProgress(rawFile);
            float[] buffer = new float[Integer.parseInt((String) storage.get("x_dimension"))*Integer.parseInt((String) storage.get("y_dimension"))*
                                       zdim];
            
            if (fileInfo.getDataType() == ModelImage.UBYTE) {
                rawFile.readImage(buffer, 0L, ModelImage.UBYTE);
            } else if (fileInfo.getDataType() == ModelImage.SHORT) {
                rawFile.readImage(buffer, 0L, ModelImage.SHORT);
            } else if (fileInfo.getDataType() == ModelImage.FLOAT) {
                rawFile.readImage(buffer, 0L, ModelImage.FLOAT);
            } else if (fileInfo.getDataType() == ModelImage.INTEGER) {
                rawFile.readImage(buffer, 0L, ModelImage.INTEGER);
            }

            image.importData(0, buffer, true);
            if (one) {
                fileInfo.setExtents(extents);
            }
            for (int i = 0; i < zdim; i++){
            	image.setFileInfo(fileInfo, i);
            	
            }
            
        } catch (IOException error) {
            throw new IOException("FileSiemensText: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        
        return image;
    }

    /**
     * Reads an SiemensText image file by reading the header then making a FileRaw to read the file. Image data is left in
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

        if (fileInfo == null) { // if no file info yet, make it.
            fileInfo = new FileInfoSiemensText(fileName, fileDir, FileUtility.SIEMENSTEXT);

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
                throw (new IOException("Cannot read image because of SiemensText header file error"));
            }
        }

        // if vox units defines the units of measure, then use that instead
        // updateUnitsOfMeasure(fileInfo);
        int units = (Unit.getUnit(fileInfo.getVoxUnits())).getLegacyNum();

        if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
        } else {
            fileInfo.setUnitsOfMeasure(units, 0);
            fileInfo.setUnitsOfMeasure(units, 1);
        }


        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
            if (image.getType() == ModelStorageBase.BOOLEAN) {
                rawFile.setMinimumBitsMinus1(7);
                rawFile.setShiftToDivide(3);
            }

            long offset = 0L;
            rawFile.readImage(buffer, offset, fileInfo.getDataType());
            rawFile.raFile.close();


        } catch (IOException error) {
            error.printStackTrace();
            throw new IOException("FileSiemensText: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return;
    }


}