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
    
    public void sort() {
    	Collections.sort(this, new ProvenanceComparator());
    }
    
    /**
     * Copies the object.
     *
     * @return  Object A copy of the file info.
     */
    public Object clone() {
        Object base = super.clone();

        return (base);
    }
    
    private class ProvenanceComparator implements Comparator {

        /**
         * DOCUMENT ME!
         *
         * @param   o1  DOCUMENT ME!
         * @param   o2  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int compare(Object o1, Object o2) {
            long a = ((ProvenanceEntry) o1).getTimeStamp();
            long b = ((ProvenanceEntry) o2).getTimeStamp();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }
    
}
