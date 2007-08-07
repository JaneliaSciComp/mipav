package gov.nih.mipav.model.provenance;


import java.util.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.model.structures.*;


/**
 * Class that stores matrices (owned by ModelImage) with accessor functions to the matrix map/adding and changing
 * matrices.
 *
 * @author  linkb
 */
public class ProvenanceHolder extends Vector<ProvenanceEntry> {

    //~ Instance fields ------------------------------------------------------------------------------------------------
 
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     *
     * @param  nDims  dimensions of image
     */
    public ProvenanceHolder() {
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Copies the object.
     *
     * @return  Object A copy of the file info.
     */
    public Object clone() {
        Object base = super.clone();

        return (base);
    }
    
}
