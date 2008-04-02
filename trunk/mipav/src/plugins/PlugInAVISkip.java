import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;

import java.io.File;
import java.io.IOException;

import java.util.Vector;


/**
 * Converts cheshire overlays in the given file to VOIs.
 *
 * @see  PlugInAlgorithm
 */

// This is a Generic type of PlugIn which does not require a source image to run.
public class PlugInAVISkip implements PlugInGeneric {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------


    /** Dialog for this plugin. */
    private PlugInDialogAVISkip aviSkipDialog;
    
    private String inputFileName;
    
    private String directory;
    
    private String outputFileName;
    
    private float captureTime;
    
    private float skipTime;
    
    private FileAvi aviFile;
    
    private int microSecPerFrame;
    
    private float secPerFrame;
    
    // rate/scale = samples/second
    private int rate;
    
    private int scale;
    
    private float samplesPerSecond;
    
    int framesToCapture;
    
    int framesToSkip;
    
    boolean AVIF_HASINDEX;
    
    boolean AVIF_MUSTUSEINDEX;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. Run method creates dialog which obtains
     * directory, input file name, output file name, capture time in seconds, and skip time in seconds.
     *
     * @see  ViewUserInterface
     * @see  ModelImage
     * @see  ViewJFrameImage
     */
    public void run() {
        aviSkipDialog = new PlugInDialogAVISkip(false, this);    
    }

    /**
     * Runs the plugin.
     */

    public void runPlugin() {

        if (aviSkipDialog.isSuccessfulExit()) {
            directory = aviSkipDialog.getDirectory();
            Preferences.debug("directory = " + directory + "\n");
            inputFileName = aviSkipDialog.getInputFileName();
            Preferences.debug("input fileName = " + inputFileName + "\n");
            outputFileName = aviSkipDialog.getOutputFileName();
            Preferences.debug("output fileName = " + outputFileName + "\n");
            captureTime = aviSkipDialog.getCaptureTime();
            Preferences.debug("capture time = " + captureTime + "\n");
            skipTime = aviSkipDialog.getSkipTime();
            Preferences.debug("skip time = " + skipTime + "\n");
            try {
                aviFile = new FileAvi(inputFileName, directory);
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException on aviFile = new FileAvi(inputFileName, directory");
                return;
            }
            try {
                aviFile.readHeader();
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException on aviFile.readHeader()");
                return;
            }
            
            microSecPerFrame = aviFile.getMicroSecPerFrame();
            Preferences.debug("PlugInAVISkip: microseconds per frame = " + microSecPerFrame + "\n");
            secPerFrame = 1.0E-6F * microSecPerFrame;
            rate = aviFile.getRate();
            scale = aviFile.getScale();
            samplesPerSecond = (float)rate/(float)scale;
            Preferences.debug("Samples per second = " + samplesPerSecond + "\n");
            if (Math.abs(((1.0/samplesPerSecond) - secPerFrame)/secPerFrame) < 0.01) {
                Preferences.debug("Frame times from 1.0E-6*microSecPerFrame and scale/rate match\n");
            }
            else {
                MipavUtil.displayError("Frame times from 1.0E-6*microSecPerFrame and scale/rate don't match");
                return;
            }
            framesToCapture = Math.max(1, Math.round(captureTime/secPerFrame));
            Preferences.debug("Frames to capture = " + framesToCapture + "\n");
            framesToSkip = Math.round(skipTime/secPerFrame);
            Preferences.debug("Frames to skip = " + framesToSkip + "\n");
            AVIF_HASINDEX = aviFile.getHasIndex();
            if (AVIF_HASINDEX) {
                Preferences.debug("The avi file has an index file that will allow quick skipping\n");
                AVIF_MUSTUSEINDEX = aviFile.getMustUseIndex();
                if (AVIF_MUSTUSEINDEX) {
                    Preferences.debug("The avi file index must be used for any file read\n");
                }
                else {
                    Preferences.debug("AVIF_MUSTUSEINDEX = false so the avi fle index need not be used for file reads\n");
                    Preferences.debug("AVIF_MUSTUSEINDEX will be changed from false to true so that the\n" +
                                      "index is used for quick skipping\n");
                }
            }
            else {
                Preferences.debug("The avi file has no index file so skipping will be slower\n");
            }
            aviFile.setOutputFileName(outputFileName);
            aviFile.setFramesToCapture(framesToCapture);
            aviFile.setFramesToSkip(framesToSkip);
            try {
                aviFile.readWriteImage();
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException on aviFile.readWriteImage()");
                return;
            }
            Preferences.debug("Completed aviFile.readWriteImage()\n");
        } else {
            // Do nothing since individual error is already displayed.
        }
        return;
    }
}
