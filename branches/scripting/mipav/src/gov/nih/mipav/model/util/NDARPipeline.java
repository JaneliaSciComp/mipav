package gov.nih.mipav.model.util;


import gov.nih.mipav.model.algorithms.AlgorithmScriptParser;
import gov.nih.mipav.model.dicomcomm.DICOM_Receiver;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.srb.*;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.srb.*;

import java.io.File;

import java.util.Vector;


/**
 * The implementation of a pipeline, which observes the DICOM receiver. Once the DICOM receiver
 * recieved dicom files, this pipeline will be initiated. The script file will executed on these
 * dicom files and upload to the srb server(birn).
 * 
 * If you want to use this pipeline, you have to follow the following procedures:
 * 
 * 1) Constructs a NDARPipeline object.
 * 2) Call setup function to set up the parameters of pipeline.
 * 3) install the observable object.
 * 
 * For example:
 * 
 *      NDARPipeline pipeline = new NDARPipeline();
 *      if(pipeline.setup()){
 *          
 *           // Retrieve or create the observable object o.
 *            
 *           pipeline.install(o);
 *      }
 */
public class NDARPipeline implements Observer {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The observable object */
    private Observable observedObject;

    /** The script file name which will be executed when the obervable object's status has changed. */
    private String scriptFileName;

    /** The destination directory where the files will be uploaded to */
    private String targetSRBDir;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new NDARPipeline object.
     */
    public NDARPipeline() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets up the parameters of this pipeline.
     *
     * @param  o  the observable object.
     */
    
    public boolean setup(){
        JDialogSetupPipeline pipelineDialog = new JDialogSetupPipeline("Setup the NDAR pipeline");
        if (!pipelineDialog.isCancelled()) {
            scriptFileName = pipelineDialog.getScriptFileName();
            targetSRBDir = pipelineDialog.getTargetSRBDir();
            return true;
        }
        return false;
    }
    
    /**
     * Installs the observable object.
     * @param o  the observable object.
     */
    public void install(Observable o) {
        observedObject = o;
        observedObject.addObserver(this);
    }

    /**
     * Sets the script file which will be executed on the files.
     *
     * @param  scriptFileName  the script file name.
     */
    public void setScriptFileName(String scriptFileName) {
        this.scriptFileName = scriptFileName;
    }

    /**
     * Sets the destination directory where the files will be uploaded on the srb server.
     *
     * @param  targetDir  the destination directory on srb server.
     */
    public void setTargetDir(String targetDir) {
        this.targetSRBDir = targetDir;
    }

    /**
     * Uninstall the observable object.
     */
    public void uninstall() {
        if (observedObject != null) {
            observedObject.deleteObserver(this);
        }
    }

    /**
     * The implementation of the Observer interface.
     *
     * @param  o    the observed object.
     * @param  arg  the input argument object.
     */
    public void update(Observable o, Object arg) {
        if ((o == null) || (arg == null)) {
            return;
        }

        if (o instanceof DICOM_Receiver) {

            // read in image from disk
            Vector imageFiles = (Vector) arg;
            if (imageFiles.size() > 1) {
                ViewUserInterface.getReference().openImageFrame((String) imageFiles.elementAt(0), true);
            } else {
                ViewUserInterface.getReference().openImageFrame((String) imageFiles.elementAt(0), false);
            }
            // System.out.println("opened image: " + (String)imageFiles.elementAt(0));

            // process image using specified script
            if (scriptFileName == null) {
                MipavUtil.displayError("A pre-upload script must be specified.");
                return;
            }

            String scriptDir, scriptName;
            int index = scriptFileName.lastIndexOf(File.separator);
            if (index == -1) {
                scriptDir = ".";
                scriptName = scriptFileName;
            } else {
                scriptDir = scriptFileName.substring(0, index + 1);
                scriptName = scriptFileName.substring(index + 1);
            }
            // System.out.println("Script file: dir = " + scriptDir + " name = " + scriptName);

            AlgorithmScriptParser scriptParser = new AlgorithmScriptParser(scriptName, scriptDir);

            // sanity checks on the script contents
            int numImages = scriptParser.preParse();
            if (numImages > 0) {
                MipavUtil.displayError("The pre-upload script must be an active mode script (no OpenImage or LoadImage commands).\nNumber of images found: " +
                                       numImages);
                return;
            }

            int numActiveImages = scriptParser.preParseActiveImages();
            if (numActiveImages != 1) {
                MipavUtil.displayError("The pre-upload script must use exactly one active image.\nNumber of images found: " +
                                       numActiveImages);
                return;
            }

            // run the pre-upload script
            scriptParser.preSetupActiveImages();
            scriptParser.run();

            if (!scriptParser.isCompleted()) {
                MipavUtil.displayError("Pre-upload script failed.  Skipping header anonymization and image upload.");
                return;
            }

            // anonymize all tags in the image
            ModelImage resultImage = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage();
            boolean[] anonFields = new boolean[FileInfoDicom.anonymizeTagIDs.length];
            for (int i = 0; i < anonFields.length; i++) {
                anonFields[i] = true;
            }
            resultImage.anonymize(anonFields, true);
            resultImage.notifyImageDisplayListeners();
            resultImage.getParentFrame().setTitle();

            // upload image
            SRBFileTransferer transferer = new SRBFileTransferer();
            transferer.saveToSRB(resultImage, targetSRBDir);
        }
    }
}
