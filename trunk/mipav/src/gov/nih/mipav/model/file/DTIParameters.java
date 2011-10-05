package gov.nih.mipav.model.file;


/**
 * Object to store parameters in the ModelImage acquired from DWI image file or ImageInfo DTI tab
 * 
 * @version 0.1 September 27, 2011
 * @author Beth Tyrie
 * @see
 */

public class DTIParameters {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private float[] bValues;

    private float[][] gradients;

    private int numVolumes;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    public DTIParameters(int numVolumes) {
        this.numVolumes = numVolumes;
    }

    // Getters and Setters
    public float[] getbValues() {
        return bValues;
    }

    public void setbValues(float[] bValues) {
        this.bValues = bValues;
    }

    public float[][] getGradients() {
        return gradients;
    }

    public void setGradients(float[][] gradients) {
        this.gradients = gradients;
    }

    public int getNumVolumes() {
        return numVolumes;
    }

    public void setNumVolumes(int numVolumes) {
        this.numVolumes = numVolumes;
    }

}
