package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.ViewUserInterface;

import java.util.Hashtable;


/**
 * A table used to store variables and their values.  The main use at the moment is to store image names as values and assign them image placeholder variable names (e.g., '$image1'), which can then be used in scripts.
 */
public class VariableTable extends Hashtable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4108175711902767830L;

    /** The reference to the only VariableTable which should ever be instantiated. */
    protected static VariableTable singletonReference = null;

    /** The base image placeholder variable string prefix.  Should be prepended to a number for each image used in a script. */
    protected static final String imageVariablePrefix = "$image";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The number to use for the next image placeholder variable to be added to the table.  Starts at 1. */
    protected int currentImageNumber = 1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new VariableTable object.
     */
    protected VariableTable() {
        super();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns a reference to the variable table singleton.
     *
     * @return  A reference to the variable table.
     */
    public static final VariableTable getReference() {

        if (singletonReference == null) {
            singletonReference = new VariableTable();
        }

        return singletonReference;
    }

    /**
     * Returns the image associated with a image placeholder variable.
     *
     * @param   varName  An image placeholder variable (e.g., '$image1').
     *
     * @return  The associated image.
     */
    public ModelImage getImage(String varName) {
        return ViewUserInterface.getReference().getRegisteredImageByName(interpolate(varName));
    }

    /**
     * Interpolates and returns the value of a given variable.
     *
     * @param   varName  The name of the variable to interpolate.
     *
     * @return  The variable's value.
     */
    public String interpolate(String varName) {
        return (String) super.get(varName);
    }

    /**
     * Checks to see if a variable has been added to the variable table.
     *
     * @param   varName  The name of the variable to check for.
     *
     * @return  <code>True</code> if the variable has been added to the table, <code>false</code> otherwise.
     */
    public boolean isVariableSet(String varName) {
        return super.containsKey(varName);
    }

    /**
     * Removes a variable and its value from the variable table.
     *
     * @param  varName  The name of the variable to remove.
     */
    public void removeVariable(String varName) {
        super.remove(varName);
    }

    /**
     * Stores a new image name in the variable table, giving it the next available image placeholder variable name.
     *
     * @param   imageName  The name of the image to add to the table.
     *
     * @return  The image placeholder variable assigned to the newly added image (e.g., '$image2').
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

            for (int i = 1; i <= currentImageNumber; i++) {

                if (imageName.equals(interpolate(imageVariablePrefix + i))) {
                    imageVar = imageVariablePrefix + i;
                }
            }
        }

        return imageVar;
    }

    /**
     * Stores a variable name and its value in the variable table.
     *
     * @param  varName  The name of the variable.
     * @param  value    The variable value.
     */
    public void storeVariable(String varName, String value) {
        super.put(varName, value);
    }
}
