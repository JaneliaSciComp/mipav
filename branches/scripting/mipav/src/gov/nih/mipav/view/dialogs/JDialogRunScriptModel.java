package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;
import java.util.*;


/**
 * @author Nathan Pollack -- Contractor (SSAI)
 * @version 0.1 May 24, 2006
 * @see JDialogRunScriptController
 * @see JDialogRunScriptView
 */
public class JDialogRunScriptModel extends Observable {
    private Vector availableImageList;

    private String[] scriptImageVars;

    private int[] numberOfRequiredVOIsForScriptImages;

    private String scriptFile;

    public JDialogRunScriptModel() {
        availableImageList = new Vector(0);
    }

    public void addVOI(String voiName, String voiLocation, int scriptModelImageVectorIndex) {
        ScriptImage scriptImage = (ScriptImage) this.getAvailableImageList().get(scriptModelImageVectorIndex);
        scriptImage.addScriptVOI(new ScriptVOI(voiName, voiLocation));
        setChanged();
        notifyObservers();
    }

    public void addToAvailableImageList(String imageName, String imageLocation, int[] extents, VOIVector VOIs) {
        availableImageList.add(new ScriptImage(imageName, imageLocation, extents, VOIs));
        setChanged();
        notifyObservers();
    }
    
    public void addToAvailableImageList(ModelImage updateItem) {
        if (updateItem instanceof ModelImage) {
            availableImageList.add(new ScriptImage((ModelImage) updateItem));
            setChanged();
            notifyObservers();
        }
    }

    public boolean isImage(String name) {
        if (getScriptImage(name) != null) {
            return true;
        }
        return false;
    }

    public ScriptImage getScriptImage(String name) {
        ScriptImage image = null;
        Object[] imageListArr = getAvailableImageList().toArray();
        for (int i = 0; i < imageListArr.length; i++ ) {
            if (((ScriptImage) imageListArr[i]).getImageName().equalsIgnoreCase(name) ||
                    ((ScriptImage) imageListArr[i]).getFileLocation().equalsIgnoreCase(name)) {
                return (ScriptImage) imageListArr[i];
            }
        }
        return image;
    }

    public Vector getAvailableImageList() {
        return availableImageList;
    }

    public String[] getScriptImageVars() {
        return scriptImageVars;
    }

    public void setScriptImageVars(String[] imagePlaceHolders) {
        this.scriptImageVars = imagePlaceHolders;
    }

    public int[] getNumberOfRequiredVOIsForScriptImages() {
        return numberOfRequiredVOIsForScriptImages;
    }

    public void setNumberOfRequiredVOIsForScriptImages(int[] numberOfVOIs) {
        this.numberOfRequiredVOIsForScriptImages = numberOfVOIs;
    }

    public String getScriptFile() {
        return scriptFile;
    }

    public void setScriptFile(String scriptFile) {
        this.scriptFile = scriptFile;
    }
    
    /*
     * public Vector getUnSavedVOIs() { return unSavedVOIs; }
     * 
     * public void setUnSavedVOIs(Vector unSavedVOIs) { this.unSavedVOIs = unSavedVOIs; }
     */
}
