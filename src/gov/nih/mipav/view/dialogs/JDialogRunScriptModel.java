package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import java.util.*;


/**
 * @author   Nathan Pollack -- Contractor (SSAI)
 * @version  0.1 May 24, 2006
 * @see      JDialogRunScriptController
 * @see      JDialogRunScriptView
 */
public class JDialogRunScriptModel extends Observable {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Vector<ScriptImage> availableImageList;

    /** DOCUMENT ME! */
    private int[] numberOfRequiredVOIsForScriptImages;

    /** DOCUMENT ME! */
    private String scriptFile;

    /** DOCUMENT ME! */
    private String[] scriptImageActions;

    /** DOCUMENT ME! */
    private String[] scriptImageLabels;

    /** DOCUMENT ME! */
    private String[] scriptImageVars;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogRunScriptModel object.
     */
    public JDialogRunScriptModel() {
        availableImageList = new Vector<ScriptImage>(0);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  updateItem  DOCUMENT ME!
     */
    public void addToAvailableImageList(ModelImage updateItem) {
    	availableImageList.add(new ScriptImage(updateItem));
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageName      DOCUMENT ME!
     * @param  imageLocation  DOCUMENT ME!
     * @param  extents        DOCUMENT ME!
     * @param  VOIs           DOCUMENT ME!
     */
    public void addToAvailableImageList(String imageName, String imageLocation, boolean isMulti) {
        availableImageList.add(new ScriptImage(imageName, imageLocation, isMulti));
    }

    /**
     * DOCUMENT ME!
     *
     * @param  voiName                      DOCUMENT ME!
     * @param  voiLocation                  DOCUMENT ME!
     * @param  scriptModelImageVectorIndex  DOCUMENT ME!
     */
    public void addVOI(String voiName, String voiLocation, int scriptModelImageVectorIndex) {
        ScriptImage scriptImage = (ScriptImage) this.getAvailableImageList().get(scriptModelImageVectorIndex);
        scriptImage.addScriptVOI(new ScriptVOI(voiName, voiLocation));
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector<ScriptImage> getAvailableImageList() {
        return availableImageList;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getNumberOfRequiredVOIsForScriptImages() {
        return numberOfRequiredVOIsForScriptImages;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getScriptFile() {
        return scriptFile;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   name  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ScriptImage getScriptImage(String name) {
        ScriptImage image = null;
        Object[] imageListArr = getAvailableImageList().toArray();

        for (int i = 0; i < imageListArr.length; i++) {

            if (((ScriptImage) imageListArr[i]).getImageName().equalsIgnoreCase(name) ||
                    ((ScriptImage) imageListArr[i]).getFileLocation().equalsIgnoreCase(name)) {
                return (ScriptImage) imageListArr[i];
            }
        }

        return image;
    }

    public int getImageIndex(String name) {
        Object[] imageListArr = getAvailableImageList().toArray();

        for (int i = 0; i < imageListArr.length; i++) {

            if (((ScriptImage) imageListArr[i]).getImageName().equalsIgnoreCase(name) ||
                    ((ScriptImage) imageListArr[i]).getFileLocation().equalsIgnoreCase(name)) {
                return i;
            }
        }
    	
    	return 0;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getScriptImageActions() {
        return scriptImageActions;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getScriptImageLabels() {
        return scriptImageLabels;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getScriptImageVars() {
        return scriptImageVars;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   name  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isImage(String name) {

        if (getScriptImage(name) != null) {
            return true;
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  numberOfVOIs  DOCUMENT ME!
     */
    public void setNumberOfRequiredVOIsForScriptImages(int[] numberOfVOIs) {
        this.numberOfRequiredVOIsForScriptImages = numberOfVOIs;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scriptFile  DOCUMENT ME!
     */
    public void setScriptFile(String scriptFile) {
        this.scriptFile = scriptFile;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageActions  DOCUMENT ME!
     */
    public void setScriptImageVarActions(String[] imageActions) {
        this.scriptImageActions = imageActions;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageLabels  DOCUMENT ME!
     */
    public void setScriptImageVarLabels(String[] imageLabels) {
        this.scriptImageLabels = imageLabels;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imagePlaceHolders  DOCUMENT ME!
     */
    public void setScriptImageVars(String[] imagePlaceHolders) {
        this.scriptImageVars = imagePlaceHolders;
    }
}
