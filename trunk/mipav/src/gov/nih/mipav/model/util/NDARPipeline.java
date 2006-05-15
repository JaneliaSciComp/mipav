package gov.nih.mipav.model.util;

import java.util.Vector;
import java.io.File;

import edu.sdsc.grid.io.srb.*;
import edu.sdsc.grid.io.local.*;
import edu.sdsc.grid.io.*;
import gov.nih.mipav.model.dicomcomm.DICOM_Receiver;
import gov.nih.mipav.view.srb.*;
import gov.nih.mipav.model.srb.*;

public class NDARPipeline implements Observer {
    private String targetSRBDir;
    private String scriptFileName;
    private Observable observedObject;
    public NDARPipeline(){
        
    }
    
    public void setTargetDir(String targetDir){
        this.targetSRBDir = targetDir;
    }
    
    public void setScriptFileName(String scriptFileName){
        this.scriptFileName = scriptFileName;
    }
    
    public void install(Observable o){
        JDialogSetupPipeline pipelineDialog = new JDialogSetupPipeline("Setup the NDAR pipeline");
        if(!pipelineDialog.isCancelled()){
            scriptFileName = pipelineDialog.getScriptFileName();
            targetSRBDir = pipelineDialog.getTargetSRBDir();
            observedObject = o;
            observedObject.addObserver(this);
        }
    }
    
    public void uninstall(){
        if(observedObject != null){
            observedObject.deleteObserver(this);
        }
    }
    /**
     * The implementation of the Observer interface.
     * @param o    the observed object.
     * @param arg  the input argument object.
     */
    public void update(Observable o, Object arg){
        if(o == null || arg == null){
            return;
        }
        if(o instanceof DICOM_Receiver){
            DICOM_Receiver receiver = (DICOM_Receiver)o;
            String baseDir = receiver.getDefaultStorageDir();
            SRBFile targetDir = new SRBFile(JDialogLoginSRB.srbFileSystem, targetSRBDir);
            GeneralFile[] sourceFiles = SRBFileTransferer.createFiles(new LocalFileSystem(), (Vector)arg);
            GeneralFile[] targetFiles = SRBFileTransferer.createTargetFiles(targetDir, new LocalFile(new File(baseDir)), sourceFiles);
            SRBFileTransferer transferer = new SRBFileTransferer();
            transferer.setThreadSeperated(true);
            transferer.setSourceFiles(sourceFiles);
            transferer.setTargetFiles(targetFiles);
            new Thread(transferer).start();
        }
    }

}
