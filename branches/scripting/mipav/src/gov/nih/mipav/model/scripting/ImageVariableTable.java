package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.ViewUserInterface;

import java.util.Enumeration;
import java.util.Hashtable;


/**
 * <p>
 * Stores image names, which are referenced by placeholder variable strings (e.g., '$image1').  Every image name is
 * unique (enforced by ViewUserInterface), so each image name should map to exactly one image placeholder in this 
 * table (and vice-versa).
 * </p>
 * <p>
 * USAGE NOTE: A new instance of this class should be created each time a new script is recorded or executed.
 * </p>
 * 
 * @see ScriptRecorder
 */
public class ImageVariableTable extends Hashtable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4108175711902767830L;

    /** The base image placeholder variable string prefix.  Should be prepended to a number for each image used in a script. */
    protected static final String imageVariablePrefix = "$image";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The number to use for the next image placeholder variable to be added to the table.  Starts at 1. */
    protected int currentImageNumber = 1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ImageVariableTable object.
     */
    public ImageVariableTable() {
        super();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Finds an image name in the table and changes it to a new value with the same variable placeholder (does nothing if the value is not in the table).
     * 
     * @param  oldName  The old image name to replace.
     * @param  newName  The new image name to give to the variable.
     */
    public void changeImageName(String oldName, String newName) {
        String imageVar = getImageVariable(oldName);
        if (imageVar != null) {
            removeImageVariable(imageVar);
            storeImageVariable(imageVar, newName);
        }
    }

    /**
     * Returns the image associated with a image placeholder variable.
     *
     * @param   imageVar  An image placeholder variable (e.g., '$image1').
     *
     * @return  The associated image.
     */
    public ModelImage getImage(String imageVar) {
        return ViewUserInterface.getReference().getRegisteredImageByName(getImageName(imageVar));
    }

    /**
     * Retrieves the image name linked to an image variable.
     *
     * @param   imageVar  The image variable to look up.
     *
     * @return  The image name associated with the given image variable.
     */
    public String getImageName(String imageVar) {
        return (String) super.get(imageVar);
    }
    
    /**
     * Returns the image variable that an image name is associated with.
     * @param   imageName  The name of the image to look for in the image table.
     * @return  The image placeholder variable associated with the given image name, or <code>null</code> if none was found.
     */
    public String getImageVariable(String imageName) {
        Enumeration keys = keys();
        
        while (keys.hasMoreElements()) {
            String curImageVar = (String) keys.nextElement();
            String curImageName = getImageName(curImageVar);
            
            if (curImageName.equals(imageName)) {
                return curImageVar;
            }
        }
        
        // did not find the image name in the table
        return null;
    }

    /**
     * Checks to see if an image variable has been added to the table.
     *
     * @param   imageVar  The image variable to check for.
     *
     * @return  <code>True</code> if the variable has been added to the table, <code>false</code> otherwise.
     */
    public boolean isImageVariableSet(String imageVar) {
        return super.containsKey(imageVar);
    }

    /**
     * Removes an image variable and its image name from the table.
     *
     * @param  varName  The name of the image variable to remove.
     */
    public void removeImageVariable(String varName) {
        super.remove(varName);
    }

    /**
     * Stores a new image name in the variable table, giving it the next available image placeholder variable name.
     *
     * @param   imageName  The name of the image to add to the table.
     *
     * @return  The image placeholder variable assigned to the image name (e.g., '$image2').
     * 
     * @see     gov.nih.mipav.model.structures.ModelImage#getImageName()
     */
    public String storeImageName(String imageName) {
        String imageVar = null;

        if (!super.containsValue(imageName)) {
            imageVar = imageVariablePrefix + currentImageNumber;
            super.put(imageVar, imageName);
            currentImageNumber++;
        } else {
            imageVar = getImageVariable(imageName);
        }

        return imageVar;
    }
    
    /**
     * Associates an image (through its name) with an image placholder variable.
     *
     * @param  imageVar   The name of the variable (e.g., '$image1').
     * @param  imageName  The image name.
     */
    public void storeImageVariable(String imageVar, String imageName) {
        super.put(imageVar, imageName);
    }
}
