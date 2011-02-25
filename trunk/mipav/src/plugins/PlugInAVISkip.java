import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.io.IOException;


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
            aviFile.setOutputFileName(outputFileName);
            aviFile.setCaptureTime(captureTime);
            aviFile.setSkipTime(skipTime);
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
