package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.util.*;

public class AlgorithmEllipticFourierDescriptors extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------

    /** The voi selected by the user. */
    private VOI activeVOI;

    /** Number of coefficients. */
    private int coefficients;

    /** The resultant polygon and the evolution has completed. */
    private VOI resultVOI;

    /** Source image. */
    private ModelImage srcImage;
    
//~ Constructors ---------------------------------------------------------------------------------------------------
    
    public AlgorithmEllipticFourierDescriptors() {
       
    }

    /**
     * Creates a new AlgorithmEllipticFourierDescriptors object.
     *
     * @param  srcImg     2D or 3D source image
     * @param  activeVOI  the selected voi
     * @param  coefficients       number of coefficients
     */
    public AlgorithmEllipticFourierDescriptors(ModelImage srcImg, VOI activeVOI, int coefficients) {

        srcImage = srcImg;
        this.activeVOI = activeVOI;
        this.coefficients = coefficients;
    }
    
    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Accessor that returns a voi that is a smoothed version of the original.
     *
     * @return  resultVOI
     */
    public VOI getResultVOI() {
        return resultVOI;
    }
    
    /**
     * Starts the smooth algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        } else {
        	
        }
    }

}