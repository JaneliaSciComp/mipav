package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * HistoLUT reader/writer. This classes incorporates the ability to write/read ModelLUT and ModelRGB information in two
 * ways. The first saves the LUT arrays (not available with ModelRGB), and the second saves the transfer and RGB
 * functions. The LUT transfer Line is saved by default (and likewise opened) when the saveLUT method is called.
 *
 * <p>NOTE: Reading a LUT does not return a ModelLUT. Rather, it updates the arrays and transfer functions in a provided
 * ModelLUT.</p>
 *
 * @version  May 14, 2002
 * @author   Lynne M. Pusanik
 * @see      ModelLUT
 * @see      ModelRGB
 * @see      gov.nih.mipav.view.ViewJFrameHistoLUT
 * @see      gov.nih.mipav.view.ViewJFrameHistoRGB
 */
public class FileHistoLUT extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Tag marking the beginning of a LUT data file. */
    private static final String lutTag = "<LUT>";

    /** Tag marking the beginning of a transfer function file. */
    private static final String funcTag = "<Transfer Functions>";

    /** Tag marking the end of a LUT or transfer file. */
    private static final String endTag = "End";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The file directory containing the data files. */
    private String fileDir;

    /** The file name of the transfer function data file. */
    private String funcFileName;

    /** The (grayscale) LUT to save. */
    private ModelLUT lut = null;

    /** The file name of the LUT data file. */
    private String lutFileName;

    /** The opacity transfer function, saved if non-null. */
    private TransferFunction opacityFunction;

    /** The color image histogram data to save. */
    private ModelRGB rgb = null;

    /** Whether to use the LUT or the RGB model. */
    private boolean useLUT = true;

    /** ModelImage to use for reference */
    private ModelImage img = null;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * LUT reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the files
     */
    public FileHistoLUT(String fileName, String fileDir) throws IOException {
        int index = fileName.lastIndexOf("."); // remove extension
        String fileNameBase = null;
        
        if(index != -1){
        	fileNameBase = fileName.substring(0, index);
        }
        else{
        	fileNameBase = fileName;
        }


        // initialize class variables
        this.lutFileName = fileNameBase + ".lut";
        this.fileDir = fileDir;

        this.funcFileName = new String(fileNameBase + ".fun");
    }

    /**
     * LUT reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     * @param      lut       the ModelLUT
     *
     * @exception  IOException  if there is an error making the files, or if lut is null
     */
    public FileHistoLUT(String fileName, String fileDir, ModelLUT lut) throws IOException {
        this(fileName, fileDir);
        setLUT(lut);
    }

    /**
     * RGB reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     * @param      rgb       the ModelRGB
     *
     * @exception  IOException  if there is an error making the files, or if rgb is null
     */
    public FileHistoLUT(String fileName, String fileDir, ModelRGB rgb) throws IOException {
        this(fileName, fileDir);
        setRGB(rgb);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * This method reads the transfer and RGB functions associated with a LUT.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readFunctions() throws IOException {

        if (useLUT) {
            readFunctions(lut);
        } else {
            readFunctions(rgb);
        }
    } // end readFunctions()

    /**
     * This method reads the transfer for a grayscale image and the RGB functions that define the LUT.
     *
     * @param      lut  The ModelLUT that these functions are read to
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readFunctions(ModelLUT lut) throws IOException {
        useLUT = false;

        // make sure that file is set to the funcFile
        if (raFile != null) {
            raFile.close();
        }

        File file = new File(fileDir + File.separator + funcFileName);

        if (!file.exists()) {
            throw new IOException("LUT Functions file does not exist.");
        }

        raFile = new RandomAccessFile(file, "r");

        String tagStr = raFile.readLine();

        if (tagStr == null) {
            raFile.close();
            throw new IOException("Error reading LUT functions. Functions file has bad format.");
        }

        tagStr.trim();

        if (!funcTag.equals(tagStr)) {
            raFile.close();
            throw new IOException("Error reading LUT Functions. Bad Tag in functions file.");
        }

        if (lut == null) {
            raFile.close();
            throw new IOException("Error reading LUT functions.  Null LUT.");
        }

        String tag2 = raFile.readLine();

        while (!tag2.equals(endTag)) {

            if (tag2.equals("Transfer")) {
                useLUT = true;
                readTransferFunction(raFile, lut.getTransferFunction(), "Transfer");
            } else if (tag2.equals("Alpha")) {
                readTransferFunction(raFile, lut.getAlphaFunction(), "Alpha");
            } else if (tag2.equals("Red")) {
                readTransferFunction(raFile, lut.getRedFunction(),"Red");
            } else if (tag2.equals("Green")) {
                readTransferFunction(raFile, lut.getGreenFunction(),"Green");
            } else if (tag2.equals("Blue")) {
                readTransferFunction(raFile, lut.getBlueFunction(),"Blue");
            } else if (tag2.equals("Opacity")) {

                if (opacityFunction != null) {
                    readTransferFunction(raFile, opacityFunction, "Opacity");
                }
            }

            // get the next tag
            tag2 = raFile.readLine();
        } // end while

        raFile.close();

        // set the data in the lut
        if (useLUT) {
            int height = lut.getExtents()[1];
            lut.makeLUT(height);
            lut.makeIndexedLUT(null);
        }

    } // end readFunctions()

    /**
     * This method reads the transfers for a RGB image.
     *
     * @param      rgb  The ModelRGB that these functions are read to
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readFunctions(ModelRGB rgb) throws IOException {
        useLUT = false;

        // make sure that file is set to the funcFile
        if (raFile != null) {
            raFile.close();
        }

        File file = new File(fileDir + File.separator + funcFileName);

        if (!file.exists()) {

            if (raFile != null) {
                raFile.close();
            }

            throw new IOException("RGB Functions file does not exist.");
        }

        raFile = new RandomAccessFile(file, "r");

        String tagStr = raFile.readLine();

        if (tagStr == null) {
            raFile.close();
            throw new IOException("Error reading RGB functions. Functions file has bad format.");
        }

        tagStr.trim();

        if (!funcTag.equals(tagStr)) {
            raFile.close();
            throw new IOException("Error reading RGB Functions. Bad Tag in functions file.");
        }

        if (rgb == null) {
            raFile.close();
            throw new IOException("Error reading RGB functions.  Null RGB.");
        }

        String tag2 = raFile.readLine();

        while (!tag2.equals(endTag)) {

            if (tag2.equals("Transfer")) {

                // this cannot be set in an rgb, so just ignore it and continue in loop
                continue;
            } else if (tag2.equals("Alpha")) {

                // this cannot be set in an rgb, so just ignore it and continue in loop
                continue;
            } else if (tag2.equals("Red")) {
                readTransferFunction(raFile, rgb.getRedFunction(), "Red");
            } else if (tag2.equals("Green")) {
                readTransferFunction(raFile, rgb.getGreenFunction(),"Green");
            } else if (tag2.equals("Blue")) {
                readTransferFunction(raFile, rgb.getBlueFunction(), "Blue");
            }

            // get the next tag
            tag2 = raFile.readLine();
        } // end while

        raFile.close();

        // set the data in the rgb
        int height = rgb.getExtents()[1];
        rgb.makeRGB(height);
        rgb.makeIndexedRGB();

    } // end readFunctions()

    /**
     * This method reads a LUT file and its associated transfer function.
     *
     * @param      quietMode  if true indicates that warnings should not be displayed.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readLUTandTransferFunction(boolean quietMode) throws IOException {

        if (useLUT) {
        	readLUTandTransferFunction(lut, quietMode);
        } else {
            readFunctions(rgb);
        }
    }

    /**
     * This method reads a LUT file and its associated transfer function.
     *
     * @param      lut        The ModelLUT that the LUT data gets applied to
     * @param      quietMode  if true indicates that warnings should not be displayed.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readLUTandTransferFunction(ModelLUT lut, boolean quietMode) throws IOException {
        String s;
        int height = 0;
        float[] fields;
        float[] alpha;
        float[] red;
        float[] blue;
        float[] green;

        // read functions first (otherwise they may overwrite points on histogram)
        // since it's possible that the functions file won't exist, but that there
        // is a LUT -- don't stop processing if this throws an exception
        try {
            readFunctions(lut);
        } catch (IOException e) {

            if (!quietMode) {
                MipavUtil.displayError("Unable to read transfer functions: " + e.getMessage());
            }
        }

        // now read the LUT
        try {
            raFile = new RandomAccessFile(new File(fileDir + File.separator + lutFileName), "r");
        } catch (FileNotFoundException e) {
            throw new IOException(e.getMessage());
        }

        String tagStr = raFile.readLine();

        if (tagStr == null) {
            raFile.close();
            throw new IOException("Error reading LUT. LUT file has bad format.");
        }

        tagStr.trim();

        if (!lutTag.equals(tagStr)) {
            raFile.close();
            throw new IOException("Error reading LUT. Bad Tag in LUT file.");
        }

        height = Integer.valueOf(readLine(raFile)).intValue();

        if (height != lut.getExtents()[1]) {
            raFile.close();
            throw new IOException("Error reading LUT.  This LUT is wrong size for this image.");
        }

        // allocate memory for arrays
        try {
            alpha = new float[height];
            red = new float[height];
            blue = new float[height];
            green = new float[height];
        } catch (OutOfMemoryError error) {
            raFile.close();
            throw new IOException("Error reading LUT. Out of memory.");
        }

        // read the data into the arrays
        for (int i = 0; i < height; i++) {
            int j = 1;
            s = raFile.readLine();

            try {
                fields = parseString(s, 5);
                alpha[i] = fields[j];
                j++;
                red[i] = fields[j];
                j++;
                green[i] = fields[j];
                j++;
                blue[i] = fields[j];
                j++;
            } catch (Exception e) {
                raFile.close();
                throw new IOException(e.getMessage());
            }
        }

        raFile.close();

        // set the data in the lut
        for (int i = 0; i < height; i++) {
            lut.set(0, i, alpha[i]); // set the alpha level
            lut.set(1, i, red[i]); // set red channel
            lut.set(2, i, green[i]); // set green channel
            lut.set(3, i, blue[i]); // set blue channel
        }

        lut.makeIndexedLUT(null);

    } // end readLUT()


    /**
     * Reads in a transfer function from a file.
     *
     * @param   file   the file to read from
     * @param   funct  the function to add points to
     * @param   functName  the transfer function name
     * @throws  IOException  if there is a problem reading from the file
     */
    private void readTransferFunction(RandomAccessFile file, TransferFunction funct, String functName) throws IOException {
        String s;
        float[] fields;
        float[] x;
        float[] y;
        float[] z;

        // read info from this data section
        int nPts = Integer.valueOf(readLine(file)).intValue();

        try {
            x = new float[nPts];
            y = new float[nPts];
            z = new float[nPts];
        } catch (OutOfMemoryError error) {
            file.close();
            throw new IOException("Error reading LUT functions. Out of memory.");
        }

        for (int i = 0; i < nPts; i++) {
            int j = 0;
            s = file.readLine();

            try {
                fields = parseString(s, 3);
                x[i] = fields[j];
                j++;
                y[i] = fields[j];
                j++;
                z[i] = fields[j];
                j++;
            } catch (Exception e) {
                file.close();
                throw new IOException(e.getMessage());
            }
        }
       
        if( functName.equals("Transfer")) {
        	float min, max, diff;    
        	boolean keepImgRef = true;
        	if(img == null) {
        	    img = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage();
        	    keepImgRef = false;
        	}
        	min = (float)img.getMin();
        	max = (float)img.getMax();
        	if(!keepImgRef) {
        	    img = null;
        	}
        	diff = max - min;
        	
        	// remap the xfer function from 0->1 to min->max
            for (int i = 0; i < nPts; i++) {
                x[i] = (x[i] * diff) + min;
            }
            System.err.println("min = " + min + " max = " + max + " diff = " + diff);               
            
        }

        // import data into function
        funct.importArrays(x, y, nPts);
        
    }

    
    /**
     * Accessor to set the LUT.
     *
     * @param      lut  the ModelLUT
     *
     * @exception  IOException  if lut is null
     */
    public void setLUT(ModelLUT lut) throws IOException {

        // make sure lut exists
        if (lut == null) {
            throw new IOException("FileHistoLUT: LUT is null.");
        }

        // initialize lut
        this.lut = lut;
        this.useLUT = true; // using lut, not rgb
    }

    /**
     * @param img the img to set
     */
    public void setImg(ModelImage img) {
        this.img = img;
    }

    /**
     * Sets the opacity function, which indicates that we want it to be saved / read in with the rest of the transfer
     * functions.
     *
     * @param  opacFunction  the (grayscale) opacity function
     */
    public void setOpacityFunction(TransferFunction opacFunction) {
        opacityFunction = opacFunction;
    }

    /**
     * Accessor to set the RGB.
     *
     * @param      rgb  the ModelRGB
     *
     * @exception  IOException  if rgb is null
     */
    public void setRGB(ModelRGB rgb) throws IOException {

        // make sure rgb exists
        if (rgb == null) {
            throw new IOException("FileHistoLUT: RGB is null.");
        }

        // initialize rgb
        this.rgb = rgb;
        this.useLUT = false; // using rgb, not lut
    }

    /**
     * Writes the default ModelLUT or ModelRGB to a file.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void write() throws IOException {

        if (useLUT) {
            writeLUT(lut);
        } else {
            writeFunctions();
        }
    }

    /**
     * Writes the default ModelLUT or ModelRGB to a file. Also writes the functions for a ModelLUT.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeAll() throws IOException {

        if (useLUT) {
            writeLUT(lut);
        }

        writeFunctions();
    } // end writeAll()

    /**
     * Writes the transfer and RGB functions to a file.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeFunctions() throws IOException {

        if (useLUT && (lut == null)) {
            throw new IOException("Error writing LUT transfer functions. LUT is null.");
        } else if (!useLUT && (rgb == null)) {
            throw new IOException("Error writing RGB transfer functions. RGB is null.");
        }

        // make sure that file is set to the funcFile
        if (raFile != null) {
            raFile.close();
        }

        File file = new File(fileDir + File.separator + funcFileName);
        raFile = new RandomAccessFile(file, "rw");

        if (file.exists() == true) {
            raFile.close();
            file.delete();
            file = new File(fileDir + File.separator + funcFileName);
            raFile = new RandomAccessFile(file, "rw");
        }

        raFile.writeBytes(funcTag + "\r\n");

        // get the transfer function -- only exists for lut
        if (useLUT) {
            writeTransferFunction(raFile, lut.getTransferFunction(), "Transfer");
            writeTransferFunction(raFile, lut.getAlphaFunction(), "Alpha");
        }

        if (useLUT) {
            writeTransferFunction(raFile, lut.getRedFunction(), "Red");
            writeTransferFunction(raFile, lut.getGreenFunction(), "Green");
            writeTransferFunction(raFile, lut.getBlueFunction(), "Blue");
        } else {
            writeTransferFunction(raFile, rgb.getRedFunction(), "Red");
            writeTransferFunction(raFile, rgb.getGreenFunction(), "Green");
            writeTransferFunction(raFile, rgb.getBlueFunction(), "Blue");
        }

        if (opacityFunction != null) {
            writeTransferFunction(raFile, opacityFunction, "Opacity");
        }

        raFile.writeBytes(endTag + "\r\n");
        raFile.close();
    }

    /**
     * Writes the default ModelLUT or ModelRGB to a file. Also writes the functions for a ModelLUT.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeLUTandTransferFunction() throws IOException {
    	if (useLUT) {
            writeLUT(lut);
        }

        writeFunctions();
    }

    /**
     * Writes a ModelLUT to a file.
     *
     * @param      lut  ModelLUT to write
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeLUT(ModelLUT lut) throws IOException {
        int height;
        String s;

        if (lut == null) {
            throw new IOException("Error writing LUT. LUT is null.");
        }

        // make sure that file is set to the file
        if (raFile != null) {
            raFile.close();
        }

        File file = new File(fileDir + File.separator + lutFileName);
        raFile = new RandomAccessFile(file, "rw");

        if (file.exists() == true) {
            raFile.close();
            file.delete();
            file = new File(fileDir + File.separator + lutFileName);
            raFile = new RandomAccessFile(file, "rw");
        }

        raFile.writeBytes(lutTag + "\r\n");

        height = lut.getExtents()[1];
        raFile.writeBytes(height + "\t\t# Size of LUT Arrays\r\n");

        for (int index = 0; index < height; index++) { // for loop that writes the index number followed
            s = Integer.toString(index); // by the pertaining ARGB values for each line
            raFile.writeBytes(s + "\t" + Float.toString(lut.getFloat(0, index)) + "\t" + // A
                              Float.toString(lut.getFloat(1, index)) + "\t" + // R
                              Float.toString(lut.getFloat(2, index)) + "\t" + // G
                              Float.toString(lut.getFloat(3, index)) + "\r\n"); // B
        }

        raFile.close();

    } // end writeLUT()

    /**
     * Write out a transfer function to a file.
     *
     * @param   file   the file to write the function to
     * @param   funct  the transfer function
     * @param   name   the label to apply to the function data
     *
     * @throws  IOException  if there is a problem writing out to the file
     */
    public void writeTransferFunction(RandomAccessFile file, TransferFunction funct, String name) throws IOException {
        int nPts = funct.size();

        float[] x = new float[nPts];
        float[] y = new float[nPts];
        float[] z = new float[nPts];

        funct.exportArrays(x, y);

        file.writeBytes(name + "\r\n");
        file.writeBytes(Integer.toString(nPts) + "\t\t# Number of Points\r\n");

        if( name.equals("Transfer")) {
        	float min, max, diff;
        	boolean keepImgRef = true;
            if(img == null) {
                img = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage();
                keepImgRef = false;
            }
        	min = (float)img.getMin();
        	max = (float)img.getMax();
        	if(!keepImgRef) {
        	    img = null;
        	}
        	diff = max - min;
        	
        	// remap the xfer function from min->max to 0->1 
            for (int i = 0; i < nPts; i++) {
                x[i] = (x[i] / diff) - min;
            }
        }
   
        
        for (int i = 0; i < nPts; i++) {
            file.writeBytes(Float.toString(x[i]) + "\t" + Float.toString(y[i]) + "\t" + Float.toString(z[i]) + "\r\n");
        }
    }

    /**
     * Parses a string and returns an array of floats.
     *
     * @param      s    string to be parsed
     * @param      len  length of desired array
     *
     * @return     array of floats
     *
     * @exception  Exception  if there is a problem reading in the float values
     */
    private static float[] parseString(String s, int len) throws Exception {
        StringTokenizer str;
        float[] array;

        str = new StringTokenizer(s, "\t", false);

        if (len <= 0) {
            len = str.countTokens();
        }

        array = new float[len];

        int i = 0;

        while (str.hasMoreTokens()) {

            try {
                array[i] = (Float.valueOf(str.nextToken())).floatValue();
            } catch (NumberFormatException e) {
                array[i] = Float.NaN;
            }

            i++;
        }

        return array;
    }

    /**
     * Reads a line of the file and strips comments indicated by the # symbol.
     *
     * @param      file  the file to read from
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private static String readLine(RandomAccessFile file) throws IOException {
        String tempString;
        int index;

        try {
            tempString = file.readLine();
        } catch (IOException error) {
            throw (error);
        }

        if (tempString == null) {
            return null;
        }

        index = tempString.indexOf("#");

        if (index != -1) {
            tempString = tempString.substring(0, index - 1);
        }

        return tempString.trim();
    }


} // end class FileHistoLUT
