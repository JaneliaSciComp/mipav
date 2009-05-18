import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.AlgorithmBase;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.io.IOException;

import java.awt.*;


/**
 * This is simple plugin that sweeps the images with sets of 16 parallel lines to find all red, blue, green synapses.
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
        int x;
        int y;
        int z;
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
        // The x length used in the current processed volume
        int xLength;
        // The y length used in the current processed volume
        int yLength;
        // The z length used in the current processed volume
        int zLength;
        
            
        try {
            tiffFile = new FileTiff(inputFileName, directory);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on tiffFile = new FileTiff(inputFileName, directory");
            return;
        }
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
        for (z = 0; z < zDim; z += zIncrement) {
            Preferences.debug("main loop z = " + z + "\n");
            zLength = Math.min(zProcessLength, zDim - 1 - z + 1);
            for (y = 0; y < yDim; y += xyIncrement) {
                fireProgressStateChanged(100 * (y + z*yDim)/(yDim * zDim));
                Preferences.debug("main loop y = " + y + "\n");
                yLength = Math.min(xyProcessLength, yDim - 1 - y + 1);
                for (x = 0; x < xDim; x += xyIncrement) {
                    Preferences.debug("main loop x = " + x + "\n");
                    xLength = Math.min(xyProcessLength, xDim - 1 - x + 1);
                    try {
                        buffer = tiffFile.readSynapseBuffer(x, xLength, y, yLength, z, zLength,
                                 redIntensity, greenIntensity, blueIntensity,
                                 redBrightIntensity, greenBrightIntensity, blueIntensity);
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on tiffFile.readSynapseBuffer");
                        return;
                    }
                } // for (x = 0; x < xDim; x += xyIncrement)
            } // for (y = 0; y < yDim; y += xyIncrement)
        } // for (z = 0; z < zDim; z += zIncrement)
        
        return;
    }
}
