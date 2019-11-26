package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmFlip;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;


/**
 
 */
public class FileJSON extends FileBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int[] imgExtents;

    /** DOCUMENT ME! */
    int nDimensions = 2;

    /** DOCUMENT ME! */
    int sourceType = ModelStorageBase.FLOAT;

    /** DOCUMENT ME! */
    private int BLANK = 0;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoJSON fileInfo;

    private FileInfoJSON fileInfoCopy;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private boolean haveBlank = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private int numberSlices; // 1 for 2D, zDim for 3D, and zDim * tDim for 4D

    private boolean isColorPlanar2D = false;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * JSON reader/writer constructor.
     * 
     * @param fileName file name
     * @param fileDir file directory
     * 
     * @exception IOException if there is an error making the file
     */
    public FileJSON(final String fileName, final String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

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
        imgExtents = null;
        LUT = null;
        
        try {
            super.finalize();
        } catch (final Throwable er) {}
    }

    /**
     * returns LUT if defined.
     * 
     * @return the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * reads the FITS file header and data.
     * 
     * @exception IOException if there is an error reading the file
     * 
     * @return DOCUMENT ME!
     */
    public ModelImage readImage() throws IOException {
        int i = 0;
        int j = 0;
        String s, firstS, subS;
        String dateString;
        String jobNameString;
        String timeString;
        int numApos;
        int aposLoc[];
        boolean haveTime;
        boolean readAgain;
        int count = 0;
        int bitsPerPixel;

        // data value = (FITS_value) X BSCALE + BZERO
        double BSCALE = 1.0;
        double BZERO = 0.0;
        long offset;
        float[] imgBuffer;
        double[] imgDBuffer;
        int bufferSize;
        // Be able to handle dimensions of length 1 in read in as long
        // as no more than 4 dimensions greater than length 1 are present
        final float[] imgResols = new float[] {1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f};
        final double[] scale = new double[] {1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
        // reference pixel position along axis
        final double[] crpix = new double[6];
        // coordinate of reference pixel
        final double[] crval = new double[6];
        final boolean[] haveCrpix = new boolean[6];
        final boolean[] haveCrval = new boolean[6];
        float minResol;
        float maxResol;
        int dimNumber;
        int[] reducedExtents;
        double[] reducedScale;
        double[] reducedCrpix;
        double[] reducedCrval;
        boolean[] reducedHaveCrpix;
        boolean[] reducedHaveCrval;
        float focalRatio;
        double origin[];
        TransMatrix matrix;
        char c;
        boolean inInfoStructure = false;

        try {

            file = new File(fileDir + fileName);

            endianess = FileBase.BIG_ENDIAN; // true
            fileInfo = new FileInfoJSON(fileName, fileDir, FileUtility.JSON);
            fileInfo.setEndianess(endianess);

            raFile = new RandomAccessFile(file, "r");
            long fileLength = raFile.length();

            readAgain = true;

            while (readAgain) {
            	if (raFile.getFilePointer() >= (fileLength - 1)) {
                    break;
                }
                s = readLine();
                count++;
                if ((s.startsWith("\"info\":"))  && (s.endsWith("{"))) {
                    inInfoStructure = true;
                    MipavUtil.displayError("Found info");
                    System.exit(-1);
                    continue;
                }
                
            } // while(readAgain) looping for first required keyword of SIMPLE

           

            image.calcMinMax();
            raFile.close();

            return image;
        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw new IOException();
        }
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

        if (tempString == null){
            return null;
        }

      
        return tempString.trim();
    }

}
