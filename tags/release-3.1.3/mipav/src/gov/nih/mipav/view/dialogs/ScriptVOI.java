package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.VOI;

import java.io.File;


public class ScriptVOI {
    private String voiName;

    private String voiFileLocation;

    private boolean isOpenedByDialog;
    
    public ScriptVOI(VOI voi, String imageName, String imageFileLocation) {
        this.voiName = voi.getName();
        // voi.setActive(true);
        String defaultFileLocation = FileUtility.getFileDirectory(imageFileLocation) + "defaultVOIs_" + imageName + File.separator;
        String fileName = defaultFileLocation + voiName + ".xml";

        isOpenedByDialog = false;
        
        if (new File(fileName).exists()) {
            // System.out.println(fileName);
        } else {
            System.out.println(fileName + " does not exist on file system");
            
            // TODO: need to find a way to add the unsaved VOI to this list in the model...
            //unSavedVOIs.add(voi);
        }
    }

    public ScriptVOI(String voiName, String voiFileLocation) {
        this.voiName = voiName;
        this.voiFileLocation = voiFileLocation;
        isOpenedByDialog = true;
    }

    public String toString() {
    	
    	return this.getVoiName();      	
    }

    public boolean isOpenedByDialog() {
    	return this.isOpenedByDialog;
    }
    
    public String getVoiFileLocation() {
        return voiFileLocation;
    }

    public void setVoiFileLocation(String voiFileLocation) {
        this.voiFileLocation = voiFileLocation;
    }

    public String getVoiName() {
        return voiName;
    }

    public void setVoiName(String voiName) {
        this.voiName = voiName;
    }
}
