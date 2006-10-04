package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;


public class ScriptImage {
    private boolean isImageOpenedByScriptDialog;
    
    private String imageName;

    private String fileLocation;

    private int[] extents;

    private ScriptVOI[] scriptVOIs;

    public ScriptImage(ModelImage modelImage) {
        isImageOpenedByScriptDialog = false;
        
        imageName = modelImage.getImageName();
        fileLocation = modelImage.getImageDirectory() + modelImage.getImageFileName();
        Object[] vois = modelImage.getVOIs().toArray();
        scriptVOIs = new ScriptVOI[vois.length];

        int[] x = new int[2];
        int[] y = new int[2];
        int[] z = new int[2];

        for (int i = 0; i < vois.length; i++) {
            scriptVOIs[i] = new ScriptVOI((VOI) vois[i], imageName, fileLocation);
            ((VOI) vois[i]).getBounds(x, y, z);
        }

        setExtents(modelImage.getExtents());
    }
    
    public ScriptImage(String imageName, String fileLocation, int[] extents, VOIVector VOIs) {
        isImageOpenedByScriptDialog = true;
        
        this.imageName = imageName;
        this.fileLocation = fileLocation;
        Object[] vois = VOIs.toArray();
        scriptVOIs = new ScriptVOI[vois.length];

        int[] x = new int[2];
        int[] y = new int[2];
        int[] z = new int[2];

        for (int i = 0; i < vois.length; i++) {
            scriptVOIs[i] = new ScriptVOI((VOI) vois[i], imageName, fileLocation);
            ((VOI) vois[i]).getBounds(x, y, z);
        }

        setExtents(extents);
    }

    public String toString() {
        if (isImageOpenedByScriptDialog) {
            return getFileLocation();
        } else {
            return getImageName();
        }
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

    public int[] getExtents() {
        return extents;
    }

    public void setExtents(int[] extents) {
        this.extents = extents;
    }
}
