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
 * DOCUMENT ME!
 */
public class NDARPipeline implements Observer {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Observable observedObject;

    /** DOCUMENT ME! */
    private String scriptFileName;

    /** DOCUMENT ME! */
    private String targetSRBDir;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new NDARPipeline object.
     */
    public NDARPipeline() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  o  DOCUMENT ME!
     */
    public void install(Observable o) {
        JDialogSetupPipeline pipelineDialog = new JDialogSetupPipeline("Setup the NDAR pipeline");
        if (!pipelineDialog.isCancelled()) {
            scriptFileName = pipelineDialog.getScriptFileName();
            targetSRBDir = pipelineDialog.getTargetSRBDir();
            observedObject = o;
            observedObject.addObserver(this);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scriptFileName  DOCUMENT ME!
     */
    public void setScriptFileName(String scriptFileName) {
        this.scriptFileName = scriptFileName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  targetDir  DOCUMENT ME!
     */
    public void setTargetDir(String targetDir) {
        this.targetSRBDir = targetDir;
    }

    /**
     * DOCUMENT ME!
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

            // TODO: make the anonymizing scriptable, add it to the ndar script
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
