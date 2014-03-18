package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.Preferences;

import java.io.*;
import java.util.ArrayList;


/**
 * Reads a BRUKER file by first reading in the d3proc header file, second the reco header file, third the acqp file int the 
 * same directory or up one or two two parent directories, and finally the 2dseq binary file.
 */

public class FileBRUKER extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoBRUKER fileInfo;
    
    private FileInfoBRUKER fileInfoCopy;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private boolean foundEOF = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int imageSlice;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[5];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** The preferred image name, used if inversion time exists. */
    private String prefImageName = null;
    
    private int numVolumes = -1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * BRUKER reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileBRUKER(String fileName, String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
        
        File f = new File(fileDir + fileName); //file separator already included
        String tryFileDir = null;
        if(!f.exists()) {  //since BRUKER is not a specific file, the user may enter one directory to high,
                           //this method does a search by depth to 3 in looking for the d3proc file
            tryFileDir = searchChildDir(new File(fileDir));
        }
        
        if(tryFileDir != null) {
            this.fileDir = tryFileDir;
        }
    }
    
    /**
     * Utility method for searching for the d3proc file that indicates a BRUKER file has been found,
     * note method is recursive to a maximum level of 3.
     */
    private String searchChildDir(File currentDir) {
        System.out.println("Searching: "+currentDir.getAbsolutePath()+File.separator +fileName);
        if(!new File(currentDir.getAbsolutePath() + File.separator + fileName).exists()) {
            ArrayList<String> subDir = childDir(currentDir);
            for(String dir : subDir) {
                if(new File(dir + File.separator + fileName).exists()) {
                    System.out.println("Returning "+dir+File.separator);
                    return dir+File.separator;
                }
            }
            for(String dir : subDir) {
                String str = searchChildDir(new File(dir));
                System.out.println("Returning "+str);
                return str;
            }
        }
        return currentDir.getAbsolutePath()+File.separator;
    }

    /**
     * Returns an ArrayList of all the names of subdirectories of the 
     * directory denoted by <code>currentDir</code>.
     */
    private ArrayList<String> childDir(File currentDir) {
        ArrayList<String> subDir = new ArrayList<String>();
        String[] fileList = currentDir.list();
        for(int i=0; i<fileList.length; i++) {
            if(new File(currentDir.getAbsolutePath() + File.separator + fileList[i]).isDirectory()) {
                subDir.add(currentDir.getAbsolutePath() + File.separator + fileList[i]);
                System.out.println("Adding "+currentDir.getAbsolutePath() + File.separator + fileList[i]);
            }
        }
        
        return subDir;
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
        fileInfoCopy = null;
        file = null;
        image = null;
        imgResols = null;
        LUT = null;
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
     * Rreturns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }
    
    /**
     * Reads the optional method file that may be part of the Bruker scan.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void readMethod() throws IOException {
        String lineString = null;
        String[] parseString;
        boolean okay;
        int numVars;
        int numFound;
        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        lineString = readLine();

        while (lineString != null) {
            parseString = parse(lineString);

            if (parseString[0].equalsIgnoreCase("##$PVM_InversionTime")) {

                if (parseString.length == 2) {

                    try {
                        Double invTime = Double.valueOf(parseString[1]);
                        fileInfo.setInversionTime(invTime.doubleValue());
                        prefImageName = "img:invTime:"+invTime.intValue();
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Inversion time for "+fileName+" could not be read.", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$ACQ_slice_sepn_mode has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_DwBMat")) {
            	okay = true;
            	if (parseString.length == 6) {
	                if (parseString[1].equals("(")) {
	                	Preferences.debug("For PVM_DwBMat parseString[1] == '(' as expected\n", Preferences.DEBUG_FILEIO);
	                }
	                else
	                {
	                	Preferences.debug("For PVM_DwBMat parseString[1] unexpectedly == " + parseString[1] + "\n", Preferences.DEBUG_FILEIO);
	                	okay = false;
	                }
	                if (okay) {
		                if (parseString[2].endsWith(",")) {
		                    numVolumes = Integer.valueOf(parseString[2].substring(0,parseString[2].length()-1));
		                    Preferences.debug("For PVM_DwBMat numVolumes = " + numVolumes + "\n", Preferences.DEBUG_FILEIO);
		                }
		                else {
		                	Preferences.debug("For PVM_DwBMat parseString[2] unexpectedly == " + parseString[2] + "\n", Preferences.DEBUG_FILEIO);
		                	okay = false;
		                }
	                }
	                if (okay) {
		                if (parseString[3].equals("3,")) {
		                    Preferences.debug("For PVM_DwBMat parseString[3] equals '3,', as expected\n", Preferences.DEBUG_FILEIO);			
		                }
		                else {
		                	Preferences.debug("For PVM_DwBMat parseString[3] unexpectedly == " + parseString[3] + "\n",
		                			          Preferences.DEBUG_FILEIO);
		                	okay = false;
		                }
	                }
	                if (okay) {
	                	if (parseString[4].equals("3")) {
		                    Preferences.debug("For PVM_DwBMat parseString[4] equals 3, as expected\n", Preferences.DEBUG_FILEIO);			
		                }
		                else {
		                	Preferences.debug("For PVM_DwBMat parseString[4] unexpectedly == " + parseString[4] + "\n",
		                			          Preferences.DEBUG_FILEIO);
		                	okay = false;
		                }	
	                }
	                if (okay) {
	                    if (parseString[5].equals(")")) {
	                        Preferences.debug("For PVM_DwMat parseString[5] == ')' as expected\n", Preferences.DEBUG_FILEIO);	
	                    }
	                    else {
	                    	Preferences.debug("For PVM_DwBMat parseString[5] unexpectedly == " + parseString[5] + "\n", Preferences.DEBUG_FILEIO);
		                	okay = false;	
	                    }
	                }
            	}
            	else {
            		Preferences.debug("For PVM_DwBMat parseString.length unexpectedly == " + parseString.length + "\n",
            				          Preferences.DEBUG_FILEIO);
            		okay = false;
            	}
            	if (okay) {
                    numVars = 9 * numVolumes;
            		numFound = 0;
            		
            	}
            } // else if (parseString[0].equalsIgnoreCase("##$PVM_DwBMat"))

            lineString = readLine();
        } // while (lineString != null)

        raFile.close();
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void readacqp() throws IOException {
        String lineString = null;
        String[] parseString;
        String sliceSeparationMode = null;
        float sliceSeparation = -1.0f;
        float sliceThickness;
        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        lineString = readLine();

        while (lineString != null) {
            parseString = parse(lineString);

            if (parseString[0].equalsIgnoreCase("##$ACQ_slice_sepn_mode")) {

                if (parseString.length == 2) {

                    if (parseString[1].equalsIgnoreCase("Var_Angle")) {
                        sliceSeparationMode = "Variable angle";
                        fileInfo.setSliceSeparationMode("Variable angle");
                    } else if (parseString[1].equalsIgnoreCase("Var_Parallel")) {
                        sliceSeparationMode = "Variable parallel";
                        fileInfo.setSliceSeparationMode("Variable parallel");
                    } else if (parseString[1].equalsIgnoreCase("Equidistant")) {
                        sliceSeparationMode = "Equidistant";
                        fileInfo.setSliceSeparationMode("Equidistant");
                    } else if (parseString[1].equalsIgnoreCase("Contiguous")) {
                        sliceSeparationMode = "Contiguous";
                        fileInfo.setSliceSeparationMode("Contiguous");
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$ACQ_slice_sepn_mode has parseString with length = " + parseString.length);
                }

            } else if (parseString[0].equalsIgnoreCase("##$ACQ_slice_sepn")) {
                lineString = readLine();
                parseString = parse(lineString);
                sliceSeparation = Float.valueOf(parseString[0]).floatValue();
            } else if (parseString[0].equalsIgnoreCase("##$ACQ_slice_thick")) {

                if (parseString.length == 2) {
                    sliceThickness = Float.valueOf(parseString[1]).floatValue();
                    fileInfo.setSliceThickness(sliceThickness);
                } else {
                    raFile.close();
                    throw new IOException("##$ACQ_slice_thick has parseString with length = " + parseString.length);
                }
            }

            lineString = readLine();
        } // while (lineString != null)

        raFile.close();

        if ((sliceSeparation > 0.0f) && (fileInfo.getRecoSize() == 2) &&
                ((sliceSeparationMode == "Equidistant") || (sliceSeparationMode == "Contiguous"))) {
            imgResols = fileInfo.getResolutions();
            imgResols[2] = sliceSeparation;
            fileInfo.setResolutions(imgResols);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
            fileInfo.setHaveZResol(true);
        }

    }


    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void readd3proc() throws IOException {

        // This reads d3proc header file of the BRUKER file format
        String lineString = null;
        String[] parseString;
        int dataType;
        int xDim = 0;
        int yDim = 0;
        int zDim = 0;
        int tDim = 0;
        int[] imageExtents;

        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        fileInfo = new FileInfoBRUKER(fileName, fileDir, FileUtility.BRUKER); // dummy fileInfo
        lineString = readLine();

        while (lineString != null) {
            parseString = parse(lineString);

            if (parseString[0].equalsIgnoreCase("##$DATTYPE")) {

                if (parseString.length == 2) {

                    if (parseString[1].equals("ip_byte")) { // Educated guess
                        fileInfo.setDataType(ModelImage.BYTE);
                    } else if (parseString[1].equals("ip_short")) { // From actual data.
                        fileInfo.setDataType(ModelImage.SHORT);
                    } else if ((parseString[1].equals("ip_integer")) ||
                    		   (parseString[1].equals("ip_int"))) { // Educated guess
                        fileInfo.setDataType(ModelImage.INTEGER);
                    } else {
                        dataType = Integer.valueOf(parseString[1]).intValue();

                        if (dataType == 2) {
                            fileInfo.setDataType(ModelImage.UBYTE);
                        } else if (dataType == 3) {
                            fileInfo.setDataType(ModelImage.SHORT);
                        } else if (dataType == 5) {
                            fileInfo.setDataType(ModelImage.INTEGER);
                        }
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$DATTYPE has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$IM_SIX")) {

                if (parseString.length == 2) {
                    xDim = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$IM_SIX has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$IM_SIY")) {

                if (parseString.length == 2) {
                    yDim = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$IM_SIY has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$IM_SIZ")) {

                if (parseString.length == 2) {
                    zDim = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$IM_SIZ has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$IM_SIT")) {

                if (parseString.length == 2) {
                    tDim = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$IM_SIT has parseString with length = " + parseString.length);
                }
            }

            lineString = readLine();
        } // while (lineString != null)

        raFile.close();

        if ((xDim > 1) && (yDim > 1) && (zDim > 1) && (tDim > 1)) {
            imageExtents = new int[4];
            imageExtents[0] = xDim;
            imageExtents[1] = yDim;
            imageExtents[2] = zDim;
            imageExtents[3] = tDim;
            fileInfo.setExtents(imageExtents);
        } else if ((xDim > 1) && (yDim > 1) && (zDim > 1)) {
            imageExtents = new int[3];
            imageExtents[0] = xDim;
            imageExtents[1] = yDim;
            imageExtents[2] = zDim;
            fileInfo.setExtents(imageExtents);
        } else if ((xDim > 1) && (yDim > 1)) {
            imageExtents = new int[2];
            imageExtents[0] = xDim;
            imageExtents[1] = yDim;
            fileInfo.setExtents(imageExtents);
        }

    }

    /**
     * This method reads one slice from the file into byteBuffer and then transfers into the float array imgBuffer.
     *
     * @param      one  DOCUMENT ME!
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean one) throws IOException {
        int i;
        int[] imageExtents;
        float[] imgBuffer = null;
        int length;
        int middleSlice = 0;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            imageExtents = fileInfo.getExtents();
            length = imageExtents[0] * imageExtents[1];

            if (imageExtents.length == 4) {
                imageSlice = imageExtents[2] * imageExtents[3];
            } else if (imageExtents.length == 3) {
                imageSlice = imageExtents[2];
            } else {
                imageSlice = 1;
            }

            if (one) {
                imageSlice = 1;
                imageExtents = new int[2];
                imageExtents[0] = fileInfo.getExtents()[0];
                imageExtents[1] = fileInfo.getExtents()[1];

                if (imageExtents.length > 3) {
                    middleSlice = imageExtents[2] / 2;
                }
            }

            imgBuffer = new float[length];

            image = new ModelImage(fileInfo.getDataType(), imageExtents, fileName);
            if(prefImageName != null) {
                image.setImageName(prefImageName);
            }

            for (i = 0; i < imageSlice; i++) {

                try {

                    if (one) {
                        readBuffer(middleSlice, imgBuffer);
                    } else {
                        readBuffer(i, imgBuffer); // Slice a time
                    }

                    fileInfoCopy = (FileInfoBRUKER)fileInfo.clone();
                    image.setFileInfo(fileInfoCopy, i);
                } catch (IOException error) {
                    throw new IOException("File BRUKER: read: " + error);
                }

                image.importData(i * length, imgBuffer, false);
            }

            raFile.close();

        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            imgBuffer = null;
            System.gc();
            throw error;
        }

        return image;

    }

    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void readreco() throws IOException {

        // This reads the reco file of the BRUKER file format
        String lineString = null;
        String[] parseString;

        float[] fov = null;
        int[] recoSize = null;
        int i;
        float temp;

        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        lineString = readLine();

        while (lineString != null) {
            parseString = parse(lineString);

            if (parseString[0].equalsIgnoreCase("##$RECO_wordtype")) {

                if (parseString.length == 2) {

                    if (parseString[1].equalsIgnoreCase("_8BIT_UNSGN_INT")) {
                        fileInfo.setDataType(ModelImage.UBYTE);
                    } else if (parseString[1].equalsIgnoreCase("_16BIT_SGN_INT")) {
                        fileInfo.setDataType(ModelImage.SHORT);
                    } else if (parseString[1].equalsIgnoreCase("_32_BIT_SGN_INT")) {
                        fileInfo.setDataType(ModelImage.INTEGER);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$RECO_wordtype has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$RECO_byte_order")) {

                if (parseString.length == 2) {

                    if (parseString[1].equalsIgnoreCase("bigEndian")) {
                        fileInfo.setEndianess(FileBase.BIG_ENDIAN);
                    } else if (parseString[1].equalsIgnoreCase("littleEndian")) {
                        fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$RECO_wordtype has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$RECO_size")) {
                lineString = readLine();
                parseString = parse(lineString);
                fileInfo.setRecoSize(parseString.length);
                
                recoSize = new int[parseString.length];
                for (i = 0; i < parseString.length; i++) {
                	recoSize[i] = Integer.valueOf(parseString[i]).intValue();
                }
            } else if (parseString[0].equalsIgnoreCase("##$RECO_fov")) {
                lineString = readLine();
                parseString = parse(lineString);
                fov = new float[parseString.length];

                for (i = 0; i < parseString.length; i++) {
                    fov[i] = Float.valueOf(parseString[i]).floatValue();
                    fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), i);
                }


                if (parseString.length > 2) {
                    fileInfo.setHaveZResol(true);
                }
            }

            lineString = readLine();
        } // while (lineString != null)
        
        if (fov != null) {
        	if ((recoSize != null) && (fov.length >= 2) && (recoSize.length >= 2) && (recoSize[0] != recoSize[1]) &&
        	    (recoSize[0] == fileInfo.getExtents()[1]) && (recoSize[1] == fileInfo.getExtents()[0])) {
        		  temp = fov[0];
        		  fov[0] = fov[1];
        		  fov[1] = temp;
        	}
        	for (i = 0; i < fov.length; i++) {
        		imgResols[i] = fov[i] * 10.0f / fileInfo.getExtents()[i];
        	}
        	fileInfo.setResolutions(imgResols);
        } // if (fov != null)

        raFile.close();
    }

    /**
     * Accessor to set the file directoyr (used for reading BRUKER files).
     *
     * @param  fDir  file directory of image to read.
     */
    public void setFileDir(String fDir) {
        fileDir = fDir;
    }

    /**
     * Accessor to set the file name (used for reading BRUKER files).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   inString  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String[] parse(String inString) {
        String[] tmpString = new String[50];
        String[] outString;
        int i;
        int sNum = 0;
        int firstEl = 0;

        for (i = 0; i < inString.length(); i++) {

            if ((inString.charAt(i) <= 0x20) || (inString.charAt(i) == '=')) {

                if (firstEl != i) {
                    tmpString[sNum++] = inString.substring(firstEl, i);
                }

                firstEl = i + 1;
            }
        }

        if (firstEl != i) {
            tmpString[sNum++] = inString.substring(firstEl, i);
        }

        if (sNum == 0) {
            outString = new String[1];
            outString[0] = inString;
        } else {
            outString = new String[sNum];

            for (i = 0; i < (sNum); i++) {
                outString[i] = tmpString[i];
            }
        }

        return outString;

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
        int i, j;
        int b1, b2, b3, b4;
        long progress, progressLength, mod;

        // long pointer;
        int nBytes;
        byte[] byteBuffer;

        i = 0;

        try {

            switch (fileInfo.getDataType()) {

                case ModelStorageBase.UBYTE:
                    nBytes = buffer.length;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * imageSlice;
                    mod = progressLength / 10;

                    for (j = 0; j < nBytes; j++, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[i] = byteBuffer[j] & 0xff;
                    }
                    break;

                case ModelStorageBase.SHORT:
                    nBytes = 2 * buffer.length;
                    byteBuffer = new byte[nBytes];
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

                        if (fileInfo.getEndianess()) {
                            buffer[i] = (short) ((b1 << 8) + b2);
                        } else {
                            buffer[i] = (short) ((b2 << 8) + b1);
                        }

                    }

                    break;

                case ModelStorageBase.INTEGER:
                    nBytes = 4 * buffer.length;
                    byteBuffer = new byte[nBytes];
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

                        if (fileInfo.getEndianess()) {
                            buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                        } else {
                            buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                        }
                    }

                    break;
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

    }

    /**
     * Reads lines of the file until a nonnull String results or the end of the file is reached.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString = null;

        while ((tempString == null) && (raFile.getFilePointer() < (raFile.length() - 1)) && (!foundEOF)) {

            try {
                tempString = raFile.readLine();
            } catch (EOFException error) {
                tempString = null;
                foundEOF = true;
            } catch (IOException error) {
                throw (error);
            }


            if (tempString != null) {

                if (tempString.length() == 0) {
                    tempString = null;
                }
            }
        } // while

        return tempString;
    }

    /**
     * This convenience method is needed to establish the location of the 2dseq file
     */
    public String getFileDir() {
        return fileDir;
    }

}
