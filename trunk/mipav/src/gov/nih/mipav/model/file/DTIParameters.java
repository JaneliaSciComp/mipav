package gov.nih.mipav.model.file;

import java.io.Serializable;

    
/**
 * Object to store parameters in the ModelImage acquired from DWI image file or ImageInfo DTI tab
 * 
 * @version 0.1 September 27, 2011
 * @author Beth Tyrie
 * @see
 */

public class DTIParameters implements Serializable {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private float[] bValues;

    private float[][] gradients;
    
    private float[][] bMatrixVals;
    
    private int numVolumes;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    public DTIParameters(int numVolumes) {
        this.numVolumes = numVolumes;
    }
    
    /**
     * Copy constructor, copies the entire input DTIParameters into this DTIParameters object.
     * @param params
     */
    public DTIParameters(DTIParameters params) {
        this.numVolumes = params.numVolumes;
    	if ( params.bValues != null )
    	{
    		this.bValues = new float[numVolumes];
    	}

    	if ( params.gradients != null )
    	{
    		this.gradients = new float[numVolumes][3];
    	}
    	
    	if ( params.bMatrixVals != null )
    	{
    		this.bMatrixVals = new float[numVolumes][6];
    	}
        for ( int i = 0; i < numVolumes; i++ )
        {   
        	if ( params.bValues != null )
        	{
        		this.bValues[i] = params.bValues[i];
        	}

        	if ( params.gradients != null )
        	{
        		for ( int j = 0; j < 3; j++ )
        		{
        			this.gradients[i][j] = params.gradients[i][j];
        		}
        	}

        	if ( params.bMatrixVals != null )
        	{
        		for ( int j = 0; j < 6; j++ )
        		{
        			this.bMatrixVals[i][j] = params.bMatrixVals[i][j];
        		}
        	}
        }
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
    
    public float[][] getbMatrixVals() {
        return bMatrixVals;
    }

    public void setbMatrixVals (float[][] bMatrixVals) {
        this.bMatrixVals = bMatrixVals;
    }

    public int getNumVolumes() {
        return numVolumes;
    }

    public void setNumVolumes(int numVolumes) {
        this.numVolumes = numVolumes;
    }

}
