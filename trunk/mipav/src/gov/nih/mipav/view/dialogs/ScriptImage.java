package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;


public class ScriptImage {
    private boolean isImageOpenedByScriptDialog;
    
    private String imageName;

    private String fileLocation;

    private boolean isMultiFile = false;
    

    private ScriptVOI[] scriptVOIs;

    public ScriptImage(ModelImage modelImage) {
        isImageOpenedByScriptDialog = false;
        
        imageName = modelImage.getImageName();
        fileLocation = modelImage.getImageDirectory() + modelImage.getImageFileName();
        isMultiFile = modelImage.getFileInfo()[0].getMultiFile();
        Object[] vois = modelImage.getVOIs().toArray();
        scriptVOIs = new ScriptVOI[vois.length];

        for (int i = 0; i < vois.length; i++) {
            scriptVOIs[i] = new ScriptVOI((VOI) vois[i], imageName, fileLocation);
        }

    }
    
    public ScriptImage(String imageName, String fileLocation, boolean isMulti) {
        isImageOpenedByScriptDialog = true;
        
        this.imageName = imageName;
        this.fileLocation = fileLocation;
        this.isMultiFile = isMulti;
    }

    public String toString() {
    	return getImageName();
    }

    public String getFileLocation() {
        return fileLocation;
    }

    public void setFileLocation(String fileLocation) {
        this.fileLocation = fileLocation;
    }

    public String getImageName() {
        return imageName;
    }

    public void setImageName(String imageName) {
        this.imageName = imageName;
    }

    public ScriptVOI[] getScriptVOIs() {
        return scriptVOIs;
    }

    public void setScriptVOIs(ScriptVOI[] scriptVOIs) {
        this.scriptVOIs = scriptVOIs;
    }

    public ScriptVOI getScriptVOI(String name) {

        ScriptVOI[] voiArr = getScriptVOIs();
        for (int i = 0; i < voiArr.length; i++) {
        	System.err.println("Checking against: " + voiArr[i].getVoiName());
            if (voiArr[i].getVoiName().equalsIgnoreCase(name)) {
                return voiArr[i];
            }
        }
        return null;
    }

    public void addScriptVOI(ScriptVOI scriptVOI) {

        ScriptVOI[] oldVOIs = getScriptVOIs();
        ScriptVOI[] newScriptVOIs = new ScriptVOI[oldVOIs.length + 1];
        for (int i = 0; i < oldVOIs.length; i++) {
            newScriptVOIs[i] = oldVOIs[i];
        }
        newScriptVOIs[newScriptVOIs.length - 1] = scriptVOI;
        setScriptVOIs(newScriptVOIs);
    }
    
    public boolean isOpenedByScript() {
    	return this.isImageOpenedByScriptDialog;
    }
    
    public boolean isMultiFile() {
    	return this.isMultiFile;
    }
    
}
