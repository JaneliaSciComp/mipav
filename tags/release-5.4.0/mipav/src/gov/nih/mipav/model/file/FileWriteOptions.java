package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.util.BitSet;
import java.util.HashMap;


/**
 * This structure contains information to direct file writing.
 * 
 * @version 0.1 June 21, 2001
 * @author Neva Cherniavsky
 * @see FileIO
 */
public class FileWriteOptions {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean runningInSeparateThread = false;

    /** DOCUMENT ME! */
    private float alphaBlend;

    /** DOCUMENT ME! */
    private int AVICompression = -1;

    /** DOCUMENT ME! */
    private int[] axisOrientation;

    /** 3D options. */
    private int beginSlice = 0;

    /** 4D options. */
    private int beginTimePeriod = 0;

    /** DOCUMENT ME! */
    private int blue;

    /** DOCUMENT ME! */
    private boolean defaults = true;

    /** DOCUMENT ME! */
    private int endSlice = 0;

    /** DOCUMENT ME! */
    private int endTimePeriod = 0;

    /** DOCUMENT ME! */
    private int fileDigitNumber = 2;

    /** DOCUMENT ME! */
    private String fileDirectory;

    /** All file options. */
    private String fileName;

    /** DOCUMENT ME! */
    private int fileStartNumber = 0;

    /** DOCUMENT ME! */
    private int fileType = FileUtility.UNDEFINED;

    /** DOCUMENT ME! */
    private int green;

    /** DOCUMENT ME! */
    private ModelImage imageB;

    /** AVI option. */
    private boolean isAVI = false;

    /** For Scripts. */
    private boolean isScript = false;

    /** DOCUMENT ME! */
    private ModelLUT LUTa;

    /** DOCUMENT ME! */
    private ModelLUT LUTb;

    /** DOCUMENT ME! */
    private int microSecPerFrame = 100000;

    /** DOCUMENT ME! */
    private float mjpegQuality = .8f;

    /** DOCUMENT ME! */
    private boolean multiFile = false;
    
    /**Used for stamping DICOM images with MIPAV reference. */
    private boolean doStamp = true;
    
    /**Whether dicom files are saved in enhanced format. */
    private boolean doEnhanced;
    
    private String niftiExtension = null;

    /** DOCUMENT ME! */
    private float opacity;

    /** DOCUMENT ME! */
    private boolean optionsSet = false;

    /** DOCUMENT ME! */
    private boolean packBitEnable = false;

    /** DOCUMENT ME! */
    private BitSet paintBitmap;

    // DICOM option(s)
    /** recalculate the Instance Number (0020,0013). Default <code>true</code> */
    private boolean recalculateInstanceNumber = true;

    /** DOCUMENT ME! */
    private int red;

    /** DOCUMENT ME! */
    private ModelRGB RGBTa;

    /** DOCUMENT ME! */
    private ModelRGB RGBTb;

    /** DOCUMENT ME! */
    private boolean saveAs = false;

    /**
     * To save the image file in a subdirectory of image-name (without extension) when true. Implementation elsewhere,
     * obviously.
     */
    private boolean saveInSubdirectory = false;

    /** DOCUMENT ME! */
    private int timeSlice = 0;

    /** TIFF options. */
    private boolean writePackBit = false;

    /** XML option. */
    private String xmlLinkedFilename;

    /** DOCUMENT ME! */
    private float xSpace;

    /** DOCUMENT ME! */
    private float xStart;

    /** DOCUMENT ME! */
    private float ySpace;

    /** DOCUMENT ME! */
    private float yStart;

    /** DOCUMENT ME! */
    private float zSpace;

    /** DOCUMENT ME! */
    private float zStart;


    /** DOCUMENT ME! */
    private float tSpace;

    /** DOCUMENT ME! */
    private float tStart;

    /**
     * Tells FileIO whether or not to insert the saved image into the Quicklist (recently used image list) after
     * successful saving
     */
    private boolean putInQuicklist = true;

    /** Tells the FileBase (FileImageXML only now) to write only the header (.xml) if true */
    private boolean writeHeaderOnly = false;

    private NDARWriteData ndarData = null;
    
    private HashMap<?,?> pSetsHashMap = null;
    
    private boolean isGzip = false;
    
    private boolean isZip = false;
    
    private boolean isBz2zip = false;

    

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructs an options structure with the given saveAs value. The file name and directory will need to be set
     * later.
     * 
     * @param saveAs <code>true</code> indicates this is a file with a new file name.
     */
    public FileWriteOptions(boolean saveAs) {
        this.saveAs = saveAs;
    }

    /**
     * Constructs an options structure with the given file name, directory, and saveAs values. The image is written to
     * this file name and directory, under the preference of saveAs.
     * 
     * @param fileName File name to save the image at.
     * @param directory Directory to save the image at.
     * @param saveAs <code>true</code> indicates this is a file with a new file name.
     */
    public FileWriteOptions(String fileName, String directory, boolean saveAs) {
        this.fileName = fileName;
        this.fileDirectory = directory;
        this.saveAs = saveAs;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        this.LUTa = null;
        this.LUTb = null;
        this.RGBTa = null;
        this.RGBTb = null;
        this.paintBitmap = null;
        this.imageB = null;
    }

    /**
     * Whether or not to put the file into the recently used image list after saving
     * 
     * @return if the image should be put into the quicklist
     */
    public boolean doPutInQuicklist() {
        return this.putInQuicklist;
    }

    /**
     * Sets the flag for putting the image into the recently used image list
     * 
     * @param doPut true = do put into quicklist
     */
    public void doPutInQuicklist(boolean doPut) {
        this.putInQuicklist = doPut;
    }
    
    /**
     * Whether to save dicom files in enhanced format.
     * 
     * @return
     */
    public boolean doEnhanced() {
        return this.doEnhanced;
    }
    
    /**
     * Whether or not to stamp DICOM images.
     * 
     * @return
     */
    public boolean doStamp() {
    	return this.doStamp;
    }
    
    /**
     * Sets the flag for saving dicom files in enhanced format.
     * 
     * @param doSaveEnhancedDicom
     */
    public void doEnhanced(boolean doSaveEnhancedDicom) {
        this.doEnhanced = doSaveEnhancedDicom; 
    }
    
    /**
     * Sets the flag for stamping DICOM images with a reference to MIPAV as a secondary processor.
     * 
     * @param doStamp
     */
    public void doStamp(boolean doStamp) {
    	this.doStamp = doStamp;
    }

    /**
     * Determines whether to write only the header (for now, .xml files) and no raw file...for NDAR SRB transfer
     * 
     * @return write header only
     */
    public boolean writeHeaderOnly() {
        return this.writeHeaderOnly;
    }

    /**
     * Sets the options to write only the .xml header if true
     * 
     * @param headerOnly write header only flag
     */
    public void setWriteHeaderOnly(boolean headerOnly) {
        this.writeHeaderOnly = headerOnly;
    }
    
    
    
    /**
     * gets the psets hashmap
     * @return
     */
    public HashMap<?,?> getPSetsHashMap() {
		return pSetsHashMap;
	}

    /**
     * sets the pset hasmap
     * @param setsHashMap
     */
	public void setPSetsHashMap(HashMap<?,?> setsHashMap) {
		pSetsHashMap = setsHashMap;
	}

	/**
     * Sets the NDAR Data that will be used to populate XML header fields
     * 
     * @param data NDAR data generated from JDialogNDAR
     */
    public void setNDARData(NDARWriteData data) {
        this.ndarData = data;
    }

    /**
     * Retrieves the NDAR data for use in populating XML fields
     * 
     * @return the ndar data
     */
    public NDARWriteData getNDARData() {
        return this.ndarData;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public float getAlphaBlend() {
        return this.alphaBlend;
    }

    /**
     * Gets the AVI compression.
     * 
     * @return AVICompression
     */
    public int getAVICompression() {
        return AVICompression;
    }

    /**
     * Returns the orientations of the 3 axes.
     * 
     * @return DOCUMENT ME!
     */
    public int[] getAxisOrientation() {
        return axisOrientation;
    }

    /**
     * Accessor that returns the slice to begin saving at.
     * 
     * @return The first slice to save at.
     */
    public int getBeginSlice() {
        return beginSlice;
    }

    /**
     * Accessor that returns the time slice to begin saving at.
     * 
     * @return The first time slice to save at.
     */
    public int getBeginTime() {
        return beginTimePeriod;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getBlue() {
        return this.blue;
    }

    /**
     * Accessor that returns the number of digits to use when appending to the multi file save (i.e., if you want
     * test00.tif to be the first file, 0 is the start number and 2 is the digit number).
     * 
     * @return The number of digits to append to the multi file save.
     */
    public int getDigitNumber() {
        return fileDigitNumber;
    }

    /**
     * Accessor that returns the slice to stop saving at.
     * 
     * @return The last slice to save at.
     */
    public int getEndSlice() {
        return endSlice;
    }

    /**
     * Accessor that returns the time slice to stop saving at.
     * 
     * @return The last time slice to save at.
     */
    public int getEndTime() {
        return endTimePeriod;
    }

    /**
     * Accessor that returns the directory to be written to.
     * 
     * @return The directory to write this image to.
     */
    public String getFileDirectory() {
        return fileDirectory;
    }

    /**
     * Accessor that returns the file name to be written to.
     * 
     * @return The file name to write this image to.
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * Accessor to get what type of file this is, such as analyze, raw, DICOM, etc.
     * 
     * @return The file type of this image.
     * 
     * @see FileBase
     */
    public int getFileType() {
        return fileType;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getGreen() {
        return this.green;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public ModelImage getImageB() {
        return this.imageB;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public ModelLUT getLUTa() {
        return this.LUTa;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public ModelLUT getLUTb() {
        return this.LUTb;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getMicroSecPerFrame() {
        return this.microSecPerFrame;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public float getMJPEGQuality() {
        return this.mjpegQuality;
    }
    
    public String getNIFTIExtension() {
    	return this.niftiExtension;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public float getOpacity() {
        return this.opacity;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public BitSet getPaintBitmap() {
        return this.paintBitmap;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getRed() {
        return this.red;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public ModelRGB getRGBTa() {
        return this.RGBTa;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public ModelRGB getRGBTb() {
        return this.RGBTb;
    }

    /**
     * Accessor that returns the number to append to the multi file save (i.e., if you want test00.tif to be the first
     * file, 0 is the start number and 2 is the digit number).
     * 
     * @return The number to append to the multi file save.
     */
    public int getStartNumber() {
        return fileStartNumber;
    }

    /**
     * Accessor that returns the time slice to save at; for TIFF, because TIFF only saves to 3D.
     * 
     * @return The time slice to save at.
     */
    public int getTimeSlice() {
        return timeSlice;
    }

    /**
     * Gets the filename for the linked file for the XML header.
     * 
     * @return xml linked filename
     */
    public String getXMLLinkedFilename() {
        return this.xmlLinkedFilename;
    }

    /**
     * Accessor that returns value of X Space text field.
     * 
     * @return Value typed in field.
     */
    public float getXSpace() {
        return xSpace;
    }

    /**
     * Accessor that returns value of X Start text field.
     * 
     * @return Value typed in field.
     */
    public float getXStart() {
        return xStart;
    }

    /**
     * Accessor that returns value of Y Space text field.
     * 
     * @return Value typed in field.
     */
    public float getYSpace() {
        return ySpace;
    }

    /**
     * Accessor that returns value of Y Start text field.
     * 
     * @return Value typed in field.
     */
    public float getYStart() {
        return yStart;
    }

    /**
     * Accessor that returns value of Z Space text field.
     * 
     * @return Value typed in field.
     */
    public float getZSpace() {
        return zSpace;
    }

    /**
     * Accessor that returns value of Z Start text field.
     * 
     * @return Value typed in field.
     */
    public float getZStart() {
        return zStart;
    }

    
    /**
     * Accessor that returns value of T Space text field.
     * 
     * @return Value typed in field.
     */
    public float getTSpace() {
        return tSpace;
    }

    /**
     * Accessor that returns value of T Start text field.
     * 
     * @return Value typed in field.
     */
    public float getTStart() {
        return tStart;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean isRunningInSeparateThread() {
        return this.runningInSeparateThread;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean isAVI() {
        return this.isAVI;
    }

    /**
     * Accessor that returns whether or not this is the default save. So if a user just hits the "OK" button in the
     * dialog without changing anything, it is default. However, if something needs to be recorded, all necessary
     * information is recorded.
     * 
     * @return <code>true</code> indicates default, <code>false</code> otherwise.
     */
    public boolean isDefault() {
        return defaults;
    }

    /**
     * Accessor that returns whether or not the Instance Number (0020,0013) should be recalculated before saving.
     * 
     * @return <code>true</code> indicates recalculate the Instance number, <code>false</code> don't modify the
     *         Instance Number.
     */
    public boolean isInstanceNumberRecalculated() {
        return recalculateInstanceNumber;
    }

    /**
     * Accessor that returns whether or not this is an image to be stored as multiple files. (Used primarily for TIFF
     * images).
     * 
     * @return <code>true</code> indicates multiple file save, <code>false</code> otherwise.
     */
    public boolean isMultiFile() {
        return multiFile;
    }

    /**
     * Accessor that returns whether or not it is possible to write with packed bits compression (only possible with
     * TIFF byte images).
     * 
     * @return <code>true</code> indicates possible, <code>false</code> otherwise.
     */
    public boolean isPackBitEnabled() {
        return packBitEnable;
    }

    /**
     * Accessor that returns whether or not this is a Save or SaveAs operation.
     * 
     * @return <code>true</code> indicates save as, <code>false</code> save.
     */
    public boolean isSaveAs() {
        return saveAs;
    }

    /**
     * Accessor that returns whether or not the save should be into a subdirectory of imagename (without extension).
     * 
     * @return <code>true</code> indicates save should be into a subdirectory as, <code>false</code> save.
     */
    public boolean isSaveInSubdirectory() {
        return saveInSubdirectory;
    }

    /**
     * Checks to see if this is being run from a script.
     * 
     * @return is it a script?
     */
    public boolean isScript() {
        return isScript;
    }

    /**
     * Accessor that returns whether all of the options have been previously set (usually by the script parser).
     * 
     * @return <code>true</code> indicates the options are already set, <code>false</code> otherwise.
     */
    public boolean isSet() {
        return optionsSet;
    }

    /**
     * Accessor that returns whether or not to write with packed bits compression (only possible with TIFF byte images).
     * 
     * @return <code>true</code> indicates w/compression, <code>false</code> otherwise.
     */
    public boolean isWritePackBit() {
        return writePackBit;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param isActive DOCUMENT ME!
     */
    public void setRunningInSeparateThread(boolean isActive) {
        this.runningInSeparateThread = isActive;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param alphaBlend DOCUMENT ME!
     */
    public void setAlphaBlend(float alphaBlend) {
        this.alphaBlend = alphaBlend;
    }

    /**
     * Accessor that sets the AVI compression.
     * 
     * @param AVICompression DOCUMENT ME!
     */
    public void setAVICompression(int AVICompression) {
        this.AVICompression = AVICompression;
    }

    /**
     * Accessor that sets the orientations of the 3 axes.
     * 
     * @param ori DOCUMENT ME!
     */
    public void setAxisOrientation(int[] ori) {
        axisOrientation = ori;
    }

    /**
     * Accessor that sets the slice to begin writing at (3D).
     * 
     * @param slice First slice to begin writing at.
     */
    public void setBeginSlice(int slice) {
        beginSlice = slice;
    }

    /**
     * Accessor that sets the time slice to begin writing at (4D).
     * 
     * @param slice Time slice to begin writing at.
     */
    public void setBeginTime(int slice) {
        beginTimePeriod = slice;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param blue DOCUMENT ME!
     */
    public void setBlue(int blue) {
        this.blue = blue;
    }

    /**
     * Accessor that sets the "default" flag.
     * 
     * @param flag <code>true</code> indicates default options, <code>false</code> otherwise.
     */
    public void setDefault(boolean flag) {
        defaults = flag;
    }

    /**
     * /** Accessor that sets the number of digits to use when appending to the multi file save (i.e., if you want
     * test00.tif to be the first file, 0 is the start number and 2 is the digit number).
     * 
     * @param digit Number of digits to write out.
     */
    public void setDigitNumber(int digit) {
        fileDigitNumber = digit;
    }

    /**
     * Accessor that sets the slice to stop writing at (3D).
     * 
     * @param slice Last slice to stop writing at.
     */
    public void setEndSlice(int slice) {
        endSlice = slice;
    }

    /**
     * Accessor that sets the time slice to stop writing at (4D).
     * 
     * @param slice Time slice to stop writing at.
     */
    public void setEndTime(int slice) {
        endTimePeriod = slice;
    }

    /**
     * Accessor that sets the file directory.
     * 
     * @param dir File directory to write to.
     */
    public void setFileDirectory(String dir) {
        fileDirectory = dir;
    }

    /**
     * Accessor that sets the file name to write out.
     * 
     * @param name File name to write to.
     */
    public void setFileName(String name) {
        fileName = name;
    }

    /**
     * Accessor that sets the file type (analyze, raw, TIFF, DICOM, etc.).
     * 
     * @param type File type to set to.
     */
    public void setFileType(int type) {
        fileType = type;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param green DOCUMENT ME!
     */
    public void setGreen(int green) {
        this.green = green;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param imageB DOCUMENT ME!
     */
    public void setImageB(ModelImage imageB) {
        this.imageB = imageB;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param isAVI DOCUMENT ME!
     */
    public void setIsAVI(boolean isAVI) {
        this.isAVI = isAVI;
    }

    /**
     * Accessor that sets the options to be run in a script.
     * 
     * @param isScript whether the options are being used as part of a script
     */
    public void setIsScript(boolean isScript) {
        this.isScript = isScript;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param LUTa DOCUMENT ME!
     */
    public void setLUTa(ModelLUT LUTa) {
        this.LUTa = LUTa;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param LUTb DOCUMENT ME!
     */
    public void setLUTb(ModelLUT LUTb) {
        this.LUTb = LUTb;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param microSec DOCUMENT ME!
     */
    public void setMicroSecPerFrame(int microSec) {
        this.microSecPerFrame = microSec;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param mjpegQuality DOCUMENT ME!
     */
    public void setMJPEGQuality(float mjpegQuality) {
        this.mjpegQuality = mjpegQuality;
    }

    /**
     * Accessor that sets whether or not to write the image as multiple files, one 2D slice per file. Used primarily for
     * TIFF images.
     * 
     * @param flag <code>true</code> indicates write this as multiple files, <code>false</code> otherwise.
     */
    public void setMultiFile(boolean flag) {
        multiFile = flag;
    }
    
    /**
     * Accessor that sets whether or not nifti should be written as a combined .nii file or
     * as separate .hdr and .img files.
     * @param niftiExtension
     */
    public void setNIFTIExtension(String niftiExtension) {
    	this.niftiExtension = niftiExtension;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param opacity DOCUMENT ME!
     */
    public void setOpacity(float opacity) {
        this.opacity = opacity;
    }

    /**
     * Accessor that sets whether all of the options have been previously set (usually by the script parser).
     * 
     * @param set <code>true</code> indicates the options are already set, <code>false</code> otherwise.
     */
    public void setOptionsSet(boolean set) {
        optionsSet = set;
    }

    /**
     * Accessor that sets whether it is possible to write with packed bits compression. It is only an options for TIFF
     * files that are byte images.
     * 
     * @param flag <code>true</code> indicates it is possible to write with packed bits compression, <code>
     *               false</code>
     *            otherwise.
     */
    public void setPackBitEnabled(boolean flag) {
        packBitEnable = flag;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param paintBitmap DOCUMENT ME!
     */
    public void setPaintBitmap(BitSet paintBitmap) {
        this.paintBitmap = paintBitmap;
    }

    /**
     * Accessor that sets whether or not the Instance Number (0020,0013) should be recalculated before saving.
     * 
     * @param flag DOCUMENT ME!
     */
    public void setRecalculateInstanceNumber(boolean flag) {
        recalculateInstanceNumber = flag;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param red DOCUMENT ME!
     */
    public void setRed(int red) {
        this.red = red;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param RGBTa DOCUMENT ME!
     */
    public void setRGBTa(ModelRGB RGBTa) {
        this.RGBTa = RGBTa;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param RGBTb DOCUMENT ME!
     */
    public void setRGBTb(ModelRGB RGBTb) {
        this.RGBTb = RGBTb;
    }

    /**
     * Accessor that sets the "save as" flag.
     * 
     * @param flag <code>true</code> indicates save as, <code>false</code> indicates regular save.
     */
    public void setSaveAs(boolean flag) {
        saveAs = flag;
    }

    /**
     * Accessor that sets the "save as" flag.
     * 
     * @param flag <code>true</code> indicates image should be saved in a subdirectory (imagename without extension)
     *            <code>false</code> indicates regular save.
     */
    public void setSaveInSubdirectory(boolean flag) {
        saveInSubdirectory = flag;
    }

    /**
     * Accessor that sets the number to append to the multi file save (i.e., if you want test00.tif to be the first
     * file, 0 is the start number and 2 is the digit number).
     * 
     * @param num Start number to write out.
     */
    public void setStartNumber(int num) {
        fileStartNumber = num;
    }

    /**
     * Accessor that sets the time slice to record (4D TIFF). This would have the same functionality as setting the
     * begin time slice and end time slice to the same number. With TIFF images, we do not allow the user to specify a
     * range of time slices, so we will save a 4D image to a 3D TIFF but not to a 4D TIFF.
     * 
     * @param slice Time slice to write.
     */
    public void setTimeSlice(int slice) {
        timeSlice = slice;
    }

    /**
     * Accessor that sets whether to write with packed bits compression. Only possible for TIFF files that are byte
     * images.
     * 
     * @param flag <code>true</code> indicates write with packed bits compression, <code>false</code> otherwise.
     */
    public void setWritePackBit(boolean flag) {
        writePackBit = flag;
    }

    /**
     * Accessor that sets the xml linked filename (for XML files).
     * 
     * @param xmlfilename the linked xml filename
     */
    public void setXMLLinkedFilename(String xmlfilename) {
        xmlLinkedFilename = xmlfilename;
    }

    /**
     * Accessor that sets x space (used for MINC images).
     * 
     * @param value Value to set the x space to.
     * 
     * @see gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setXSpace(float value) {
        xSpace = value;
    }

    /**
     * Accessor that sets x start (used for MINC images).
     * 
     * @param value Value to set the x start to.
     * 
     * @see gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setXStart(float value) {
        xStart = value;
    }

    /**
     * Accessor that sets y space (used for MINC images).
     * 
     * @param value Value to set the y space to.
     * 
     * @see gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setYSpace(float value) {
        ySpace = value;
    }

    /**
     * Accessor that sets y start (used for MINC images).
     * 
     * @param value Value to set the y start to.
     * 
     * @see gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setYStart(float value) {
        yStart = value;
    }

    /**
     * Accessor that sets z space (used for MINC images).
     * 
     * @param value Value to set the z space to.
     * 
     * @see gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setZSpace(float value) {
        zSpace = value;
    }

    /**
     * Accessor that sets z start (used for MINC images).
     * 
     * @param value Value to set the z start to.
     * 
     * @see gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setZStart(float value) {
        zStart = value;
    }
    
    /**
     * Accessor that sets t space (used for MINC images).
     * 
     * @param value Value to set the t space to.
     * 
     * @see gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setTSpace(float value) {
        tSpace = value;
    }
    
    /**
     * Accessor that sets t start (used for MINC images).
     * 
     * @param value Value to set the t start to.
     * 
     * @see gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setTStart(float value) {
        tStart = value;
    }

    

    public boolean isGzip() {
		return isGzip;
	}

	public void setGzip(boolean isGzip) {
		this.isGzip = isGzip;
	}
	
	

	public boolean isZip() {
		return isZip;
	}

	public void setZip(boolean isZip) {
		this.isZip = isZip;
	}

	public boolean isBz2zip() {
		return isBz2zip;
	}

	public void setBz2zip(boolean isBz2zip) {
		this.isBz2zip = isBz2zip;
	}

	/**
     * Shows these options as a string, useful for debugging.
     * 
     * @return A string containing the saved file write options.
     */
    public String toString() {
        String s = "";
        s += "All File options:\n";
        s += "\tFile name: " + fileName + "\n\tFile directory: " + fileDirectory + "\n\tFile type: " + fileType
                + "\n\tSave as: " + saveAs + "\n\tOptions set: " + optionsSet + "\n\tDefaults: " + defaults + "\n\n";
        s += "3D options:\n";
        s += "\tBegin slice: " + beginSlice + "\n\tEnd slice: " + endSlice + "\n\n";
        s += "4D options:\n";
        s += "\tBegin time: " + beginTimePeriod + "\n\tEnd time: " + endTimePeriod + "\n\tTime Slice: " + timeSlice
                + "\n\n";
        s += "TIFF options:\n";
        s += "\tPack Bit Enabled: " + packBitEnable + "\n\tWrite Pack Bit: " + writePackBit + "\n\tMulti file: "
                + multiFile + "\n\tStart #: " + fileStartNumber + "\n\t# digits: " + fileDigitNumber + "\n\n";
        s += "MINC options:\n";
        s += "\tX Start: " + xStart + "\n\tY Start: " + yStart + "\n\tZ Start: " + zStart + "\n\tX Space: " + xSpace
                + "\n\tY Space: " + ySpace + "\n\tZ Space: " + zSpace + "\n\n";
        s += "DICOM options:\n";
        s += "\tRecalculate Image Instance: " + recalculateInstanceNumber + "\n";

        return s;
    }

    
}
