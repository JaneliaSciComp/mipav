package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.structures.*;
import java.util.*;

/** @author Nathan Pollack -- Contractor (SSAI)
* @version 0.1 May 24, 2006
* @see JDialogRunScriptController
* @see JDialogRunScriptView
*/

public class JDialogRunScriptModel extends Observable{

    private Vector imageList;
    private String[] imagePlaceHolders;
    private int[] numberOfVOIs;
    private String scriptFile;
    private Vector unSavedVOIs;

    
    public JDialogRunScriptModel() {
        imageList = new Vector(0);
        unSavedVOIs = new Vector(0);
    }

    public void addVOI(String voiName, String voiLocation, int scriptModelImageVectorIndex){
        ScriptModelImage scriptImage = (ScriptModelImage)this.getImageList().get(scriptModelImageVectorIndex);
        scriptImage.addScriptVOI(new ScriptVOI(voiName,voiLocation));
        setChanged();
        notifyObservers(); 
   }
    
    
    
     public void updateVector(Object updateItem){
        if (updateItem instanceof ModelImage){
             imageList.add(new ScriptModelImage((ModelImage)updateItem));
            setChanged();
            notifyObservers(); 
        }
    }
     
     public boolean isImage(String name){
         if (getScriptModelImage(name) != null) return true;
         return false;
     }
     
     
     public ScriptModelImage getScriptModelImage(String name){
         ScriptModelImage image = null;
         Object[] imageListArr = getImageList().toArray();
         for (int i=0;i<imageListArr.length;i++){
             if (((ScriptModelImage)imageListArr[i]).imageName.equalsIgnoreCase(name))
                 return (ScriptModelImage)imageListArr[i];
         }        
         return image;
     }
 
    public Vector getImageList() {
        return imageList;
    }


   
    
    public String[] getImagePlaceHolders() {
        return imagePlaceHolders;
    }


    public void setImagePlaceHolders(String[] imagePlaceHolders) {
        this.imagePlaceHolders = imagePlaceHolders;
    }


    public int[] getNumberOfVOIs() {
        return numberOfVOIs;
    }


    public void setNumberOfVOIs(int[] numberOfVOIs) {
        this.numberOfVOIs = numberOfVOIs;
    }


    public String getScriptFile() {
        return scriptFile;
    }


    public void setScriptFile(String scriptFile) {
        this.scriptFile = scriptFile;
    }
    
    
    
    class ScriptModelImage{
        String imageName;
        String fileLocation;
        int[] extents;
        int zDim;
        
        ScriptVOI[] scriptVOIs;
       
        ScriptModelImage(ModelImage modelImage){
            this.imageName = modelImage.getImageName();
            this.fileLocation = modelImage.getFilePath();            
            Object[] vois = modelImage.getVOIs().toArray();
            scriptVOIs = new ScriptVOI[vois.length];            
            
            int[] x = new int[2];
            int[] y = new int[2];
            int[] z = new int[2];
            
            if (modelImage.getExtents().length > 2)
            zDim = modelImage.getExtents()[2];            
            
            for (int i=0;i<vois.length;i++){
                scriptVOIs[i] = new ScriptVOI((VOI)vois[i], imageName, fileLocation);
                ((VOI)vois[i]).getBounds(x,y,z);
            }
            
            setExtents(modelImage.getExtents());
            modelImage = null;
        }
        
        public String toString(){
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
        
        public ScriptVOI getScriptVOI(String name){
            
            ScriptVOI[] voiArr = getScriptVOIs();
            for (int i=0;i<voiArr.length;i++){
                if (voiArr[i].voiName.equalsIgnoreCase(name))
                    return voiArr[i];
            }                
            return null;
        }
        
        
        public void addScriptVOI(ScriptVOI scriptVOI){
             
            ScriptVOI[] oldVOIs = getScriptVOIs();
            ScriptVOI[] newScriptVOIs = new ScriptVOI[oldVOIs.length+1];
            for (int i=0;i<oldVOIs.length;i++){
                newScriptVOIs[i] = oldVOIs[i];  
            }
            newScriptVOIs[newScriptVOIs.length-1] = scriptVOI;
            setScriptVOIs(newScriptVOIs);
       }

        public int[] getExtents() {
            return extents;
        }

        public void setExtents(int[] extents) {
            this.extents = extents;
        }

        public int getZDim() {
            return zDim;
        }

        public void setZDim(int dim) {
            zDim = dim;
        }
    }//ScriptModelImage
    
    
    class ScriptVOI{
        String voiName;
        String voiFileLocation;
        
        ScriptVOI(VOI voi, String imageName, String imageFileLocation){
            this.voiName = voi.getName(); 
            //voi.setActive(true);
            
            
            
            String defaultFileLocation = imageFileLocation.substring(0,imageFileLocation.lastIndexOf(java.io.File.separator)+1) + "defaultVOIs_" + imageName + java.io.File.separator;
            String fileName = defaultFileLocation + voiName + ".xml";
          
            if (new java.io.File(fileName).exists()){
                //System.out.println(fileName);
                }else{
                    System.out.println(fileName + " does not exist on file system");
                    unSavedVOIs.add(voi);
                }
        }
        
        ScriptVOI(String voiName, String voiFileLocation){
            this.voiName = voiName;
            this.voiFileLocation = voiFileLocation;            
        }
        
        public String toString(){
            return this.voiName;
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
        
    }//ScriptVOI


    public void makeVoiActive(String voiName){
        for (int i=0;i<unSavedVOIs.capacity();i++){
            if (((VOI)unSavedVOIs.get(i)).getName().equalsIgnoreCase(voiName)){
                ((VOI)unSavedVOIs.get(i)).setActive(true);
            }
        }        
    }
    
   /* public Vector getUnSavedVOIs() {
        return unSavedVOIs;
    }

    public void setUnSavedVOIs(Vector unSavedVOIs) {
        this.unSavedVOIs = unSavedVOIs;
    }*/
     
    
    

}//class