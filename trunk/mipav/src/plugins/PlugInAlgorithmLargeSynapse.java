import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.AlgorithmBase;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.io.IOException;

import java.awt.*;


/**
 * This is simple plugin that sweeps the images with sets of 16 parallel lines to find all red, blue, green synapses.
 * The image will be contained in 1 large 8 bit color TIFF file made from source files described below:
 * Source file names are of the form AT_jcTCs153_-0006_p0c2.tif
 * 0006 is the plane number.  There are 27 files in each plane, composed of 3 colors * 9 tiles.
 * p0 - p8 are the tiles,
 * p0 upper left
 * p1 upper middle
 * p2 upper right
 * p3 second row left
 * p4 second row middle
 * p5 2nd row right
 * p6 bottom left
 * p7 bottom center
 * p8 bottom right
 * Each tile has a 20% overlap for alignment purposes.  20 % is from the distance of the microscopic stage.
 * It's quite far from exact in terms of exact pixel number.  Alignment is required.
 * Also the different planes must be aligned with each other.  Planes are translated and rotated so that p0
 * in 1 plane may become p4 in another plane.
 * Each individual file is 1388 by 1040 pixels.
 * c1-c3 are colors with
 * c1 green channel
 * c2 red channel
 * c3 a longer red wavelength appearing as blue in the image
 * Images are unsigned short with only 12 bits being used.
 * Files erroneously say resolution is .006666 by .006666 inches.
 * Actually the pixel size is 100 nm., while the optical resolution is 200 nm.
 * Now around a couple of hundred planes will be used, but wish to increase it to up to 2-3 thousand
 * with 100 nm. distance.  Pixel size will then be 100 nm. in all three directions.
 * However, the large 8 bit color files I receive will have all the registration performed.
 * Note that Java can only index arrays with signed integers, so it can only go from 0 to 2**31 - 1 = 2,147,483,647 =
 * 2.147E9.
 * JC wishes to use files with 5000 X 5000 X 5000 pixels = 1.25E11 pixels.  Since ARGB has 4 bytes per pixel, this
 * requires 5E11 bytes.
 *
 * @see  PlugInGeneric
 */
public class PlugInAlgorithmLargeSynapse extends AlgorithmBase {
    
    private PlugInDialogLargeSynapse largeSynapseDialog;
    
    private String inputFileName;
    
    private String directory;
    
    private FileTiff tiffFile;
    
    /** Length of a processed square within a slice */
    private int xyProcessLength;
    
    /** Overlap length in a processed square in a slice */
    private int xyOverlapLength;
    
    /** Height of a processed volume across slices */
    private int zProcessLength;
    
    /** Overlap of processed volume heights across slices */
    private int zOverlapLength;
    
    /** Minimum number of red pixels along line */
    private int redMin;
    
    /** Maximum number of pixels along line */
    private int redMax;
    
    /** Minimum number of green pixels along line */
    private int greenMin;
    
    /** Maximum number of pixels along line */
    private int greenMax;
    
    /** Minimum number of blue pixels along line within slice*/
    private int blueMinXY;
    
    /** Maximum number of blue pixels along line within slice*/
    private int blueMaxXY;
    
    /** Minimum number of blue pixels along line between slices*/
    private int blueMinZ;
    
    /** Maximum number of blue pixels along line between slices*/
    private int blueMaxZ;
    
    /** Minimum intensity values */
    private int redIntensity;
    
    private int redBrightIntensity;
    
    private int greenIntensity;
    
    private int greenBrightIntensity;
    
    private int blueIntensity;
    
    private int blueBrightIntensity;
    
    private boolean histoInfo;
    
    private final byte NONE = 0;
    private final byte RED = 1;
    private final byte GREEN = 2;
    private final byte BLUE = 3;
    private final byte BRIGHT_RED = 4;
    private final byte BRIGHT_GREEN = 5;
    private final byte BRIGHT_BLUE = 6;
    
    /**
     * 
     * @param directory
     * @param inputFileName
     * @param xyProcessLength
     * @param xyOverlapLength
     * @param zProcessLength
     * @param zOverlapLength
     * @param redMin
     * @param redMax
     * @param greenMin
     * @param greenMax
     * @param blueMinXY
     * @param blueMaxXY
     * @param blueMinZ
     * @param blueMaxZ
     * @param redIntensity
     * @param greenIntensity
     * @param blueIntensity
     * @param redBrightIntensity
     * @param greenBrightIntensity
     * @param blueBrightIntensity
     * @param histoInfo
     */
    public PlugInAlgorithmLargeSynapse(String directory, String inputFileName, int xyProcessLength,
            int xyOverlapLength, int zProcessLength, int zOverlapLength, int redMin, int redMax,
            int greenMin, int greenMax, int blueMinXY, int blueMaxXY, int blueMinZ, int blueMaxZ,
            int redIntensity, int greenIntensity, int blueIntensity, int redBrightIntensity,
            int greenBrightIntensity, int blueBrightIntensity, boolean histoInfo) {
        this.directory = directory;
        this.inputFileName = inputFileName;
        this.xyProcessLength = xyProcessLength;
        this.xyOverlapLength = xyOverlapLength;
        this.zProcessLength = zProcessLength;
        this.zOverlapLength = zOverlapLength;
        this.redMin = redMin;
        this.redMax = redMax;
        this.greenMin = greenMin;
        this.greenMax = greenMax;
        this.blueMinXY = blueMinXY;
        this.blueMaxXY = blueMaxXY;
        this.blueMinZ = blueMinZ;
        this.blueMaxZ = blueMaxZ;
        this.redIntensity = redIntensity;
        this.greenIntensity = greenIntensity;
        this.blueIntensity = blueIntensity;
        this.redBrightIntensity = redBrightIntensity;
        this.greenBrightIntensity = greenBrightIntensity;
        this.blueBrightIntensity = blueBrightIntensity;
        this.histoInfo = histoInfo;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    
    
    /**
     * Runs the plugin.
     *
     */
    public void runAlgorithm() {
        int xstart;
        int ystart;
        int zstart;
        int xDim;
        int yDim;
        int zDim;
        int imgExtents[];
        int zIncrement;
        int xyIncrement;
        // Number of bytes processed in each slice each iteration
        int processSquare;
        // Number of bytes in a processed volume
        int processVolume;
        // In each processed volume all bytes are classified as NONE, RED, GREEN, BLUE, BRIGHT_RED,
        // BRIGHT_GREEN, or BRIGHT_BLUE
        byte buffer[];
        short redBuffer[];
        short greenBuffer[];
        short blueBuffer[];
        // The x length used in the current processed volume
        int xLength;
        // The y length used in the current processed volume
        int yLength;
        // The z length used in the current processed volume
        int zLength;
        int fileNumber = 0;
        int hyphenIndex;
        int lastUnderlineIndex;
        String planeString;
        String baseString;
        int planeDigits = 0;
        int firstPlaneNumber;
        int i;
        boolean found;
        int planeNumber;
        int digitLength;
        int leadingZeros;
        String testName;
        File testFile;
        int lastPlaneNumber;
        int tile;
        int xLower;
        int yLower;
        String tileString;
        int z;
        String fileName;
        int zPos;
        int red;
        int green;
        int blue;
        int pos;
        int zOffset;
        ModelImage redImage;
        ModelImage greenImage;
        ModelImage blueImage;
        ModelImage p0Image;
        ModelImage p3Image;
        int tileSequence[] = new int[9];
        int tileSequenceIndex;
        int tileNumber;
        
        
        try {
            tiffFile = new FileTiff(inputFileName, directory);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on tiffFile = new FileTiff(inputFileName, directory");
            return;
        }
        tiffFile.setSynapseIntensities(redIntensity, greenIntensity, blueIntensity, redBrightIntensity,
                                       greenBrightIntensity, blueBrightIntensity);
        try {
            imgExtents = tiffFile.getExtents();
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on imgExtents = tiffFile.getExtents()");
            return;
        }
        
        xDim = imgExtents[0];
        Preferences.debug("\nxDim = " + xDim + "\n");
        yDim = imgExtents[1];
        Preferences.debug("yDim = " + yDim + "\n");
        zDim = imgExtents[2];
        Preferences.debug("zDim = " + zDim + "\n");
        
       
        zIncrement = zProcessLength - zOverlapLength;
        xyIncrement = xyProcessLength - xyOverlapLength;
        processSquare = xyProcessLength * xyProcessLength;
        processVolume = processSquare * zProcessLength;
        buffer = new byte[processVolume];
        
        for (zstart = 0; zstart < zDim; zstart += zIncrement) {
            fireProgressStateChanged(100 * zstart/ zDim);
            Preferences.debug("main loop zstart = " + zstart + "\n");
            zLength = Math.min(zProcessLength, zDim - zstart);
            for (ystart = 0; ystart < yDim; ystart += xyIncrement) {
                Preferences.debug("main loop ystart = " + ystart + "\n");
                yLength = Math.min(xyProcessLength, yDim - ystart);
                for (xstart = 0; xstart < xDim; xstart += xyIncrement) {
                    Preferences.debug("main loop xstart = " + xstart + "\n");
                    xLength = Math.min(xyProcessLength, xDim - xstart);
                    
                    try {
                        tiffFile.readSynapseBuffer(buffer, xstart, xLength, ystart, yLength, zstart,
                                                   zLength);
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on tiffFile.readSynapseBuffer");
                        return;
                    }
                        
                        
                } // for (xstart = 0; xstart < xDim; xstart += xyIncrement)
            } // for (ystart = 0; ystart < yDim; ystart += xyIncrement)
        } // for (zstart = 0; zstart < zDim; zstart += zIncrement)
        
        return;
    }
}
