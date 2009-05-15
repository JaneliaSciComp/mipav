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
    
    private int redMin;
    
    private int redMax;
    
    private int greenMin;
    
    private int greenMax;
    
    private int blueMinXY;
    
    private int blueMaxXY;
    
    private int blueMinZ;
    
    private int blueMaxZ;

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
