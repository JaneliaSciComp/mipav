import gov.nih.mipav.model.structures.*;

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
public class PlugInLargeSynapse implements PlugInGeneric {
    
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

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface.
     *
     
     */
    public void run() {

        largeSynapseDialog = new PlugInDialogLargeSynapse(false, this);
    }
    
    /**
     * Runs the plugin.
     *
     */
    public void runPlugin() {
        if (largeSynapseDialog.isSuccessfulExit()) {
            directory = largeSynapseDialog.getDirectory();
            Preferences.debug("directory = " + directory + "\n");
            inputFileName = largeSynapseDialog.getInputFileName();
            Preferences.debug("input fileName = " + inputFileName + "\n");
            xyProcessLength = largeSynapseDialog.getXYProcessLength();
            Preferences.debug("xyProcessLength = " + xyProcessLength + "\n");
            xyOverlapLength = largeSynapseDialog.getXYOverlapLength();
            Preferences.debug("xyOverlapLength = " + xyOverlapLength + "\n");
            zProcessLength = largeSynapseDialog.getZProcessLength();
            Preferences.debug("zProcessLength = " + zProcessLength + "\n");
            zOverlapLength = largeSynapseDialog.getZOverlapLength();
            Preferences.debug("zOverlapLength = " + zOverlapLength + "\n");
            redMin = largeSynapseDialog.getRedMin();
            Preferences.debug("redMin = " + redMin + "\n");
            redMax = largeSynapseDialog.getRedMax();
            Preferences.debug("redMax = " + redMax + "\n");
            greenMin = largeSynapseDialog.getGreenMin();
            Preferences.debug("greenMin = " + greenMin + "\n");
            greenMax = largeSynapseDialog.getGreenMax();
            Preferences.debug("greenMax = " + greenMax + "\n");
            blueMinXY = largeSynapseDialog.getBlueMinXY();
            Preferences.debug("blueMinXY = " + blueMinXY + "\n");
            blueMaxXY = largeSynapseDialog.getBlueMaxXY();
            Preferences.debug("blueMaxXY = " + blueMaxXY + "\n");
            blueMinZ = largeSynapseDialog.getBlueMinZ();
            Preferences.debug("blueMinZ = " + blueMinZ + "\n");
            blueMaxZ = largeSynapseDialog.getBlueMaxZ();
            Preferences.debug("blueMaxZ = " + blueMaxZ + "\n");
            redIntensity = largeSynapseDialog.getRedIntensity();
            Preferences.debug("redIntensity = " + redIntensity + "\n");
            redBrightIntensity = largeSynapseDialog.getRedBrightIntensity();
            Preferences.debug("redBrightIntensity = " + redBrightIntensity + "\n");
            greenIntensity = largeSynapseDialog.getGreenIntensity();
            Preferences.debug("greenIntensity = " + greenIntensity + "\n");
            greenBrightIntensity = largeSynapseDialog.getGreenBrightIntensity();
            Preferences.debug("greenBrightIntensity = " + greenBrightIntensity + "\n");
            blueIntensity = largeSynapseDialog.getBlueIntensity();
            Preferences.debug("blueIntensity = " + blueIntensity + "\n");
            blueBrightIntensity = largeSynapseDialog.getBlueBrightIntensity();
            Preferences.debug("blueBrightIntensity = " + blueBrightIntensity + "\n");
            histoInfo = largeSynapseDialog.getHistoInfo();
            Preferences.debug("histoInfo = " + histoInfo + "\n");
            try {
                tiffFile = new FileTiff(inputFileName, directory);
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException on tiffFile = new FileTiff(inputFileName, directory");
            }
        } // if (largeSyanpseDialog.isSuccessfulExit())
        else {
            // Do nothing since individual error is already displayed
        }
        return;
    }
}
