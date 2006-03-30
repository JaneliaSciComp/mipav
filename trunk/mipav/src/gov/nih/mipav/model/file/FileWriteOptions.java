package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;

import java.util.*;

/**
 * This structure contains information to direct file writing.
 *
 * @version    0.1 June 21, 2001
 * @author     Neva Cherniavsky
 * @see        FileIO
 */
public class FileWriteOptions {

	// All file options
	private	String	fileName;
	private	String	fileDirectory;
	private int		fileType		= FileBase.UNDEFINED;
	private	boolean	saveAs			= false;

	/**
	 To save the image file in a subdirectory of image-name (without extension) when true.
	 Implementation elsewhere, obviously.
	*/
	private boolean saveInSubdirectory = false;
	private	boolean	optionsSet		= false;
	private	boolean	defaults		= true;

	// 3D options
	private	int		beginSlice		= 0;
	private	int		endSlice		= 0;

	// 4D options
	private	int		beginTimePeriod = 0;
	private	int		endTimePeriod	= 0;

	// TIFF options
	private	boolean	writePackBit	= false;
	private boolean packBitEnable	= false;
	private	boolean	multiFile		= false;
	private	int		timeSlice		= 0;
	private	int		fileStartNumber = 0;
	private	int		fileDigitNumber = 2;

	// MINC options
	private boolean isInfToSup;
	private boolean isPosToAnt;
	private boolean isLeftToRight;
	private float	xStart;
	private float	yStart;
	private float	zStart;
	private float	xSpace;
	private float	ySpace;
	private float	zSpace;
	private int		orientation;
    private int[]   axisOrientation;

	// XML option
	private String  xmlLinkedFilename;

	// AVI option
        private boolean isAVI = false;
    private int AVICompression = -1;
    private BitSet paintBitmap;
    private int red;
    private int green;
    private int blue;
    private ModelLUT LUTa;
    private ModelLUT LUTb;
    private ModelRGB RGBTa;
    private ModelRGB RGBTb;
    private float opacity;
    private float alphaBlend;
    private ModelImage imageB;
    private int microSecPerFrame = 100000;
    private float mjpegQuality = .8f;
    private boolean activeImage = false;


        // DICOM option(s)
        /** recalculate the Instance Number (0020,0013).  Default <code>true</code> */
        private boolean recalculateInstanceNumber = true;

        //For Scripts
        private boolean isScript = false;

	/**
	*	Constructs an options structure with the given saveAs value.  The
	*	file name and directory will need to be set later.
	*	@param saveAs	<code>true</code> indicates this is a file with a new file name.
	*/
    public FileWriteOptions(boolean saveAs) {
    	this.saveAs = saveAs;
    }

    public void setActiveImage(boolean isActive) {
        this.activeImage = isActive;
    }

    public boolean isActiveImage() {
        return this.activeImage;
    }

	/**
	*	Constructs an options structure with the given file name, directory, and saveAs values.  The
	*	image is written to this file name and directory, under the preference of saveAs.
	*	@param fileName	File name to save the image at.
	*	@param directory	Directory to save the image at.
	*	@param saveAs	<code>true</code> indicates this is a file with a new file name.
	*/
    public FileWriteOptions(String fileName, String directory, boolean saveAs) {
    	this.fileName = fileName;
    	this.fileDirectory = directory;
    	this.saveAs = saveAs;
    }


    public int getRed() { return this.red; }
    public int getGreen() { return this.green; }
    public int getBlue() { return this.blue; }
    public BitSet getPaintBitmap() { return this.paintBitmap; }
    public ModelLUT getLUTa() { return this.LUTa; }
    public ModelLUT getLUTb() { return this.LUTb; }
    public ModelRGB getRGBTa() { return this.RGBTa; }
    public ModelRGB getRGBTb() { return this.RGBTb; }
    public float getOpacity() { return this.opacity; }
    public float getAlphaBlend() { return this.alphaBlend; }
    public ModelImage getImageB() { return this.imageB; }
    public int getMicroSecPerFrame() { return this.microSecPerFrame; }
    public float getMJPEGQuality() { return this.mjpegQuality; }


    public void disposeLocal() {
        this.LUTa = null;
        this.LUTb = null;
        this.RGBTa = null;
        this.RGBTb = null;
        this.paintBitmap = null;
        this.imageB = null;
    }

    /**
    *	Accessor to get what type of file this is, such as analyze, raw, DICOM, etc.
    *	@return 	The file type of this image.
    *	@see		FileBase
    */
    public int		getFileType()	{return fileType;}

    /**
    *	Accessor that returns whether or not this is a Save or SaveAs operation.
    *	@return		<code>true</code> indicates save as, <code>false</code> save.
    */
    public boolean	isSaveAs()		{return saveAs;}

    /**
     *  Accessor that returns whether or not the Instance Number (0020,0013) should be recalculated before saving.
     *  @return	<code>true</code> indicates recalculate the Instance number,
     *          <code>false</code> don't modify the Instance Number.
     */
    public boolean      isInstanceNumberRecalculated()   {return recalculateInstanceNumber;}

    /**
    *	Accessor that returns whether or not the save should be into a subdirectory
    *   of imagename (without extension).
    *	@return		<code>true</code> indicates save should be into a subdirectory as,
    *               <code>false</code> save.
    */
    public boolean	isSaveInSubdirectory()		{return saveInSubdirectory;}

    /**
    *	Accessor that returns whether or not this is the default save.  So if a user just
    *	hits the "OK" button in the dialog without changing anything, it is default.
    *	However, if something needs to be recorded, all necessary information is recorded.
    *	@return		<code>true</code> indicates default, <code>false</code> otherwise.
    */
    public boolean	isDefault()		{return defaults;}

    /**
    *	Accessor that returns whether or not to write with packed bits compression (only
    *	possible with TIFF byte images).
    *	@return		<code>true</code> indicates w/compression, <code>false</code> otherwise.
    */
    public boolean	isWritePackBit(){return writePackBit;}

    /**
    *	Accessor that returns whether or not it is possible to write with packed bits compression (only
    *	possible with TIFF byte images).
    *	@return		<code>true</code> indicates possible, <code>false</code> otherwise.
    */
    public boolean	isPackBitEnabled(){return packBitEnable;}

    /**
    *	Accessor that returns whether or not this is an image to be stored as multiple files.
    *	(Used primarily for TIFF images).
    *	@return		<code>true</code> indicates multiple file save, <code>false</code> otherwise.
    */
    public boolean	isMultiFile()	{return multiFile;}

    /**
    *	Accessor that returns the slice to begin saving at.
    *	@return		The first slice to save at.
    */
    public int		getBeginSlice()	{return beginSlice;}

    /**
    *	Accessor that returns the slice to stop saving at.
    *	@return		The last slice to save at.
    */
    public int		getEndSlice()	{return endSlice;}

    /**
    *	Accessor that returns the time slice to begin saving at.
    *	@return		The first time slice to save at.
    */
    public int		getBeginTime()	{return beginTimePeriod;}

    /**
    *	Accessor that returns the time slice to stop saving at.
    *	@return		The last time slice to save at.
    */
    public int		getEndTime()	{return endTimePeriod;}

    /**
    *	Accessor that returns the time slice to save at; for TIFF, because TIFF only saves to 3D.
    *	@return		The time slice to save at.
    */
    public int		getTimeSlice()	{return timeSlice;}

    /**
    *	Accessor that returns the number to append to the multi file save (i.e., if you want
    *	test00.tif to be the first file, 0 is the start number and 2 is the digit number).
    *	@return		The number to append to the multi file save.
    */
    public int		getStartNumber(){return fileStartNumber;}

    /**
    *	Accessor that returns the number of digits to use when appending to the multi file save
    *	(i.e., if you want test00.tif to be the first file, 0 is the start number and 2 is the digit number).
    *	@return		The number of digits to append to the multi file save.
    */
    public int		getDigitNumber(){return fileDigitNumber;}

    /**
    *	Accessor that returns whether all of the options have been previously set (usually by the
    *	script parser).
    *	@return		<code>true</code> indicates the options are already set, <code>false</code> otherwise.
    */
    public boolean	isSet()			{return	optionsSet;}

    /**
    *	Accessor that returns the file name to be written to.
    *	@return		The file name to write this image to.
    */
    public String	getFileName()	{return fileName;}

    /**
    *	Accessor that returns the directory to be written to.
    *	@return		The directory to write this image to.
    */
    public String	getFileDirectory(){return fileDirectory;}

    /**
    *   Accessor that returns value of X Start text field.
    *   @return     Value typed in field.
    */
    public float getXStart()		{return xStart;}

    /**
    *   Accessor that returns value of Y Start text field.
    *   @return     Value typed in field.
    */
    public float getYStart()		{return yStart;}

    /**
    *   Accessor that returns value of Z Start text field.
    *   @return     Value typed in field.
    */
    public float getZStart()		{return zStart;}

    /**
    *   Accessor that returns value of X Space text field.
    *   @return     Value typed in field.
    */
    public float getXSpace()		{return xSpace;}

    /**
    *   Accessor that returns value of Y Space text field.
    *   @return     Value typed in field.
    */
    public float getYSpace()		{return ySpace;}

    /**
    *   Accessor that returns value of Z Space text field.
    *   @return     Value typed in field.
    */
    public float getZSpace()		{return zSpace;}

    /**
    *   Checks if this is the default MINC orientation or not.
    *   @return      <code>true</code> represents left to right,
    *                <code>false</code> represents right to left.
    */
    public boolean isLeftToRight()	{return isLeftToRight;}

    /**
    *   Checks if this is the default MINC orientation or not.
    *   @return      <code>true</code> represents posterior to anterior,
    *                <code>false</code> represents anterior to posterior.
    */
    public boolean isPosToAnt() 	{return isPosToAnt;}

    /**
    *   Checks if this is the default MINC orientation or not.
    *   @return      <code>true</code> represents inferior to superior,
    *                <code>false</code> represents superior to inferior.
    */
    public boolean isInfToSup() 	{return isInfToSup;}

    /**
     * Checks to see if this is being run from a script
     * @return is it a script?
     */
    public boolean isScript() { return isScript; }

    /**
    *  Gets the AVI compression
    *  @return AVICompression
    */
    public int getAVICompression() {return AVICompression; }

    /**
    *   Gets the orientation selected, axial, coronal, or sagittal, in MINC file.
    *   @return         Integer representing the orientation.
    */
    public int getOrientation() 	{return orientation;}
    
    /**
     * Returns the orientations of the 3 axes
     * @return
     */
    public int[] getAxisOrientation() {return axisOrientation;}

    /**
    *   Gets the filename for the linked file for the XML header
    *   @return  xml linked filename
    */
    public String getXMLLinkedFilename()      { return this.xmlLinkedFilename; }

    /**
    *	Accessor that sets the file type (analyze, raw, TIFF, DICOM, etc.)
    *	@param type		File type to set to.
    */
    public void setFileType(int type)			{fileType	= type;}

    /**
    *	Accessor that sets the "save as" flag.
    *	@param flag		<code>true</code> indicates save as, <code>false</code> indicates regular save.
    */
    public void setSaveAs(boolean flag)	{
        saveAs 	= flag;
    }

    /**
     *  Accessor that sets whether or not the Instance Number (0020,0013)
     *  should be recalculated before saving.
     *  @return	<code>true</code> indicates recalculate the Instance number,
     *          <code>false</code> don't modify the Instance Number.
     */
    public void setRecalculateInstanceNumber(boolean flag) {
        recalculateInstanceNumber 	= flag;
    }

    /**
    *	Accessor that sets the "save as" flag.
    *	@param flag		<code>true</code> indicates image should be saved in a subdirectory (imagename without extension)
    *                   <code>false</code> indicates regular save.
    */
    public void setSaveInSubdirectory(boolean flag)			{saveInSubdirectory 	= flag;}

    /**
    *	Accessor that sets the "default" flag.
    *	@param flag		<code>true</code> indicates default options, <code>false</code> otherwise.
    */
    public void setDefault(boolean flag)		{defaults	= flag;}

    /**
    *	Accessor that sets whether to write with packed bits compression.  Only possible for
    *	TIFF files that are byte images.
    *	@param flag		<code>true</code> indicates write with packed bits compression, <code>false</code> otherwise.
    */
    public void setWritePackBit(boolean flag)	{writePackBit = flag;}

    /**
    *	Accessor that sets whether it is possible to write with packed bits compression.
    *	It is only an options for TIFF files that are byte images.
    *	@param flag		<code>true</code> indicates it is possible to write with packed bits compression, <code>false</code> otherwise.
    */
    public void setPackBitEnabled(boolean flag)	{packBitEnable = flag;}

    /**
    *	Accessor that sets whether or not to write the image as multiple files, one 2D slice per file.
    *	Used primarily for TIFF images.
    *	@param flag		<code>true</code> indicates write this as multiple files, <code>false</code> otherwise.
    */
    public void setMultiFile(boolean flag)		{multiFile 	= flag;}

    /**
    *	Accessor that sets the slice to begin writing at (3D).
    *	@param slice	First slice to begin writing at.
    */
    public void setBeginSlice(int slice)		{beginSlice	= slice;}

    /**
    *	Accessor that sets the slice to stop writing at (3D).
    *	@param slice	Last slice to stop writing at.
    */
    public void setEndSlice(int slice)			{endSlice	= slice;}

    /**
    *	Accessor that sets the time slice to begin writing at (4D).
    *	@param slice	Time slice to begin writing at.
    */
    public void setBeginTime(int slice)			{beginTimePeriod = slice;}

    /**
    *	Accessor that sets the time slice to stop writing at (4D).
    *	@param slice	Time slice to stop writing at.
    */
    public void setEndTime(int slice)			{endTimePeriod = slice;}

    /**
    *	Accessor that sets the time slice to record (4D TIFF).  This would have the same
    *	functionality as setting the begin time slice and end time slice to the same number.
    *	With TIFF images, we do not allow the user to specify a range of time slices, so we will
    *	save a 4D image to a 3D TIFF but not to a 4D TIFF.
    *	@param slice	Time slice to write.
    */
    public void setTimeSlice(int slice)			{timeSlice	= slice;}

    /**
    *	Accessor that sets the number to append to the multi file save (i.e., if you want
    *	test00.tif to be the first file, 0 is the start number and 2 is the digit number).
    *	@param num		Start number to write out.
    */
    public void setStartNumber(int num)			{fileStartNumber = num;}

    /**
    /**
    *	Accessor that sets the number of digits to use when appending to the multi file save
    *	(i.e., if you want test00.tif to be the first file, 0 is the start number and 2 is the digit number).
    *	@param digit	Number of digits to write out.
    */
    public void setDigitNumber(int digit)		{fileDigitNumber = digit;}

    /**
     *	Accessor that sets whether all of the options have been previously set (usually by the
     *	script parser).
     *	@param set  <code>true</code> indicates the options are already set, <code>false</code> otherwise.
     */
    public void	setOptionsSet(boolean set)		{optionsSet = set;}

    /**
    *	Accessor that sets the file name to write out.
    *	@param name		File name to write to.
    */
    public void setFileName(String name)		{fileName	= name;}

    /**
    *	Accessor that sets the file directory.
    *	@param dir		File directory to write to.
    */
    public void	setFileDirectory(String dir)	{fileDirectory = dir;}

    /**
     *	Accessor that sets x start (used for MINC images).
     *	@param value	Value to set the x start to.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setXStart(float value)			{xStart 	= value;}

    /**
     *	Accessor that sets y start (used for MINC images).
     *	@param value	Value to set the y start to.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setYStart(float value)			{yStart 	= value;}

    /**
     *	Accessor that sets z start (used for MINC images).
     *	@param value	Value to set the z start to.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setZStart(float value)			{zStart 	= value;}

    /**
     *	Accessor that sets x space (used for MINC images).
     *	@param value	Value to set the x space to.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setXSpace(float value)			{xSpace 	= value;}

    /**
     *	Accessor that sets y space (used for MINC images).
     *	@param value	Value to set the y space to.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setYSpace(float value)			{ySpace 	= value;}

    /**
     *	Accessor that sets z space (used for MINC images).
     *	@param value	Value to set the z space to.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setZSpace(float value)			{zSpace 	= value;}

    /**
     *	Accessor that sets whether or not the axis is increasing from left to right.
     *	@param flag		<code>true</code> indicates left to right, <code>false</code> right to left.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setLeftToRight(boolean flag)	{isLeftToRight = flag;}

    /**
     *	Accessor that sets whether or not the axis is increasing from posterior to anterior.
     *	@param flag		<code>true</code> indicates posterior to anterior, <code>false</code> anterior to posterior.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setPosToAnt(boolean flag)		{isPosToAnt = flag;}

    /**
     *	Accessor that sets whether or not the axis is increasing from inferior to superior.
     *	@param flag		<code>true</code> indicates inferior to superior, <code>false</code> superior to inferior.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setInfToSup(boolean flag)		{isInfToSup = flag;}

    /**
     *	Accessor that sets the orientation (axial, coronal, or sagittal).  Used for MINC.
     *	@param value	The defined orientation, in ModelImage.
     *	@see	gov.nih.mipav.view.dialogs.JDialogSaveMinc
     */
    public void setOrientation(int value)		{orientation = value;}
    
    /**
     * Accessor that sets the orientations of the 3 axes
     * @param ori
     */
    public void setAxisOrientation (int ori[]) {axisOrientation = ori;}

    /**
     *  Accessor that sets the xml linked filename (for XML files)
     *  @param xmlfilename  the linked xml filename
     */
    public void setXMLLinkedFilename(String xmlfilename)    { xmlLinkedFilename = xmlfilename; }

    /**
     * Accessor that sets the options to be run in a script
     * @param isScript  whether the options are being used as part of a script
     */
    public void setIsScript(boolean isScript) {
      this.isScript = isScript;
    }

    /**
    *  Accessor that sets the AVI compression
    *  @param AVICompression
    */
    public void setAVICompression(int AVICompression) {
        this.AVICompression = AVICompression;
    }

    public void setIsAVI(boolean isAVI) {
      this.isAVI = isAVI;
    }
    public boolean isAVI() { return this.isAVI; }


    public void setPaintBitmap(BitSet paintBitmap) { this.paintBitmap = paintBitmap; }
    public void setRed(int red) { this.red = red; }
    public void setGreen(int green) { this.green = green; }
    public void setBlue(int blue) { this.blue = blue; }
    public void setLUTa(ModelLUT LUTa) { this.LUTa = LUTa; }
    public void setLUTb(ModelLUT LUTb) { this.LUTb = LUTb; }
    public void setRGBTa(ModelRGB RGBTa) { this.RGBTa = RGBTa; }
    public void setRGBTb(ModelRGB RGBTb) { this.RGBTb = RGBTb; }
    public void setOpacity(float opacity) { this.opacity = opacity; }
    public void setAlphaBlend(float alphaBlend) { this.alphaBlend = alphaBlend; }
    public void setImageB(ModelImage imageB) { this.imageB = imageB; }
    public void setMicroSecPerFrame(int microSec) { this.microSecPerFrame = microSec; }
    public void setMJPEGQuality(float mjpegQuality) { this.mjpegQuality = mjpegQuality; }

	/**
	*	Shows these options as a string, useful for debugging.
	*	@return		A string containing the saved file write options.
	*/
	public String toString() {
		String s="";
		s += "All File options:\n";
		s += "\tFile name: " + fileName + "\n\tFile directory: " + fileDirectory +
			 "\n\tFile type: " + fileType + "\n\tSave as: " + saveAs + "\n\tOptions set: " +
			  optionsSet + "\n\tDefaults: " + defaults + "\n\n";
		s += "3D options:\n";
		s += "\tBegin slice: " + beginSlice + "\n\tEnd slice: " + endSlice + "\n\n";
		s += "4D options:\n";
		s += "\tBegin time: " + beginTimePeriod + "\n\tEnd time: " + endTimePeriod + "\n\tTime Slice: " + timeSlice + "\n\n";
		s += "TIFF options:\n";
		s += "\tPack Bit Enabled: " + packBitEnable + "\n\tWrite Pack Bit: " + writePackBit +
			 "\n\tMulti file: " + multiFile + "\n\tStart #: " + fileStartNumber +
                         "\n\t# digits: " + fileDigitNumber + "\n\n";
		s += "MINC options:\n";
		s += "\tX Start: " + xStart + "\n\tY Start: " + yStart + "\n\tZ Start: " + zStart +
                        "\n\tX Space: " + xSpace + "\n\tY Space: " + ySpace + "\n\tZ Space: " + zSpace +
                        "\n\tOrientation: " + orientation +
                        "\n\tLeft to Right: " + isLeftToRight +
                        "\n\tPosterior to Anterior: " + isPosToAnt +
                        "\n\tInferior to Superior: " + isInfToSup + "\n\n";
                s += "DICOM options:\n";
                s += "\tRecalculate Image Instance: " + recalculateInstanceNumber +"\n";

		return s;
    }

}
