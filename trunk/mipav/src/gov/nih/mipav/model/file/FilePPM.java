package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Vector;

public class FilePPM extends FileBase {
	// Reads a color image
	
	/** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoPPM fileInfo;
    
    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
   // private ViewJProgressBar progressBar;
    
    /**
     * File Portable Pixmap Utilities reader/writer constructor.
     *
     * @param  fName  file name
     * @param  fDir   file directory
     */
    public FilePPM(String fName, String fDir) {
        fileName = fName;
        fileDir = fDir;
    }
    
    public ModelImage readImage() throws IOException {
        int i;
        int[] extents;
        String lineString;
        int ASCII_FORMAT = 1;
        int BINARY_FORMAT = 2;
        int format = BINARY_FORMAT;
        String[] values = null;
        boolean haveMagicNumber = false;
        boolean haveXDim = false;
        boolean haveYDim = false;
        boolean haveZDim = false;
        boolean haveMaxValue = false;
        int numValues = 0;
        int currentValue = 0;
        int xDim = 0;
        int yDim = 0;
        int zDim = 0;
        int length;
        int maxValue = 255;
        int imageType = ModelStorageBase.ARGB;
        FileRaw rawFile;
        int valuesRead;
        short sbuf[] = null;
        int ibuf[] = null;
        long preReadPosition = 0;
        int nonWhiteBytesRead = 0;

        fileInfo = new FileInfoPPM(fileName, fileDir, FileUtility.PPM);
        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");

        fileInfo.setEndianess(BIG_ENDIAN);
        
        while (!haveMaxValue) {
        	preReadPosition = raFile.getFilePointer();
            lineString = readLine();
            if (lineString != null) {
                nonWhiteBytesRead = lineString.length();
            	values = retrieveValues(lineString);
            	if (values != null) {
            		currentValue = 0;
	            	numValues = values.length;
	        	    if ((!haveMagicNumber) && (currentValue < numValues)) {
	        	        if (values[currentValue].equals("P3")) {
	        	        	currentValue++;
	        	        	haveMagicNumber = true;
	        	        	format = ASCII_FORMAT;
	        	        	Preferences.debug("Magic number = P3 for ASCII\n", Preferences.DEBUG_FILEIO);		
	        	        }
	        	        else if (values[currentValue].equals("P6")) {
	        	        	currentValue++;
	        	        	haveMagicNumber = true;
	        	        	format = BINARY_FORMAT;
	        	        	Preferences.debug("Magic number = P6 for BINARY\n", Preferences.DEBUG_FILEIO);	
	        	        }
	        	        else {
	        	        	raFile.close();
	        	        	throw new IOException("Magic number is an illegal " + values[currentValue]);
	        	        }
	        	    } // if ((!haveMagicNumber) && (currentValue < numValues))
	        	    if ((!haveXDim) && (currentValue < numValues)) {
	        	    	xDim = Integer.parseInt(values[currentValue]);
	        	    	currentValue++;
	        	    	haveXDim = true;
	        	    	Preferences.debug("xDim = " + xDim + "\n", Preferences.DEBUG_FILEIO);
	        	    	if ((!haveYDim) && (currentValue < numValues)) {
		        	    	yDim = Integer.parseInt(values[currentValue]);
		        	    	currentValue++;
		        	    	haveYDim = true;
		        	    	Preferences.debug("yDim = " + yDim + "\n", Preferences.DEBUG_FILEIO);
		        	    	if ((!haveZDim) && (currentValue < numValues)) {
			        	    	zDim = Integer.parseInt(values[currentValue]);
			        	    	currentValue++;
			        	    	haveZDim = true;
			        	    	Preferences.debug("zDim = " + zDim + "\n", Preferences.DEBUG_FILEIO);
			        	    }
		        	    }
	        	    }
	        	    if ((!haveMaxValue) && (currentValue < numValues)) {
	        	    	maxValue = Integer.parseInt(values[currentValue]);
	        	    	currentValue++;
	        	    	haveMaxValue = true;
	        	    	Preferences.debug("Maximum value = " + maxValue + "\n", Preferences.DEBUG_FILEIO);
	        	    }
            	} // if (values != null)
            } // if (lineString != null)
        } // while (!haveMaxValue)
        if (!haveZDim) {
            extents = new int[2];
            extents[0] = xDim;
            extents[1] = yDim;
        }
        else {
        	extents = new int[3];
        	extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
        }
        fileInfo.setExtents(extents);
        length = xDim * yDim;
        if (haveZDim) {
        	length = length * zDim;
        }
        if (maxValue <= 255) {
        	imageType = ModelStorageBase.ARGB;
        	if (raFile.getFilePointer() + length > raFile.length()) {
        		// Handle case where no new line after maxValue.
        		// Then just set position based on 1 white space character after the maximum value.
        		raFile.seek(preReadPosition + nonWhiteBytesRead + 1);
        	}
        }
        else if (maxValue <= 65535) {
        	imageType = ModelStorageBase.ARGB_USHORT;
        }
        fileInfo.setDataType(imageType);
        image = new ModelImage(imageType, extents, fileInfo.getFileName());
        if (format == BINARY_FORMAT) {
        	
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
            linkProgress(rawFile);
            rawFile.readImage(image, raFile.getFilePointer());
        }
        else { // format == ASCII_FORMAT
            valuesRead = 0;
            if (imageType == ModelStorageBase.ARGB) {
            	sbuf = new short[4*length];
            }
            else if (imageType == ModelStorageBase.ARGB_USHORT) {
            	ibuf = new int[4*length];
            }
            while (valuesRead < 4*length) {
            	lineString = readLine();
                if (lineString != null) {
                	values = retrieveValues(lineString);
	                if (values != null) {
	                	numValues = values.length;
	                	if (imageType == ModelStorageBase.ARGB) {
	                        for (i = 0; (i < numValues) && (valuesRead + i < 4*length); i+=3) {
	                        	sbuf[valuesRead + i] = 0;
	                        	sbuf[valuesRead + i + 1] = Short.parseShort(values[i]);
	                        	sbuf[valuesRead + i + 2] = Short.parseShort(values[i+1]);
	                        	sbuf[valuesRead + i + 3] = Short.parseShort(values[i+2]);
	                        }
	                    }
	                    else if (imageType == ModelStorageBase.ARGB_USHORT) {
	                    	for (i = 0; (i < numValues) && (valuesRead + i < 4*length); i+=3) {
	                    		ibuf[valuesRead + i] = 0;
	                        	ibuf[valuesRead + i + 1] = Integer.parseInt(values[i]);
	                        	ibuf[valuesRead + i + 2] = Integer.parseInt(values[i+1]);
	                        	ibuf[valuesRead + i + 3] = Integer.parseInt(values[i+2]);
	                        }	
	                    }
	                	valuesRead = valuesRead + 4*numValues/3;
                	} // if (values != null)
                } // if (lineString != null)
            } // while (valuesRead < 4*length)
            if (imageType == ModelStorageBase.ARGB) {	
                image.importData(0, sbuf, true);
            }
            else if (imageType == ModelStorageBase.ARGB_USHORT) {
            	image.importData(0, ibuf, true);	
            }
        } // else format == ASCII_FORMAT
        raFile.close();
        
        return image;
    }
    
    /**
     * readLine() - reads a line of the file and strips comments indicated by the # symbol.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString;
        String retString;
        int index;

        try {
            tempString = raFile.readLine();
        } catch (IOException error) {
            throw (error);
        }

        if (tempString == null) {
            return null;
        }

        index = tempString.indexOf("#");

        if (index != -1) {
            retString = tempString.substring(0, index);

            return retString.trim();
        } else {
            return tempString.trim();
        }
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param   inString  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String[] retrieveValues(String inString) {
        String outString[] = null;
        int i;
        int numValues = 0;
        Vector<Integer> firstValue = new Vector<Integer>();
        Vector<Integer> lastValue = new Vector<Integer>();

        if ((inString != null) && (!inString.isEmpty())) {
        	numValues = 1;
        	firstValue.add(0);
        	if (inString.length() == 1) {
        		lastValue.add(0);
        	}
        	else {
	        	for (i = 1; i < inString.length(); i++) {
	        	    if ((inString.charAt(i) > 0x20) &&	(inString.charAt(i-1) <= 0x20)) {
	        	    	numValues++;
	        	    	firstValue.add(i);
	        	    	if (i == inString.length() - 1) {
	        	    		lastValue.add(i);
	        	    	}
	        	    }
	        	    else if ((inString.charAt(i) <= 0x20) && (inString.charAt(i-1) > 0x20)) {
	        	    	lastValue.add(i-1);
	        	    }
	        	    else if ((i == inString.length() - 1) && (inString.charAt(i) > 0x20)) {
	        	    	lastValue.add(i);
	        	    }
	        	}
        	}
        	outString = new String[numValues];
        	char[] val = new char[inString.length()];
            for (i = 0; i < inString.length(); i++) {
            	val[i] = inString.charAt(i);
            }
        	for (i = 0; i < numValues; i++) {
        	    outString[i] = new String(val, firstValue.get(i), lastValue.get(i) - firstValue.get(i) + 1);    
        	} // for (i = 0; i < numValues; i++)
            
            return outString;
            
        } // if ((inString != null) && (!inString.isEmpty()))
        else {
            return null;
        }

    }
}