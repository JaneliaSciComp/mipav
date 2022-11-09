package gov.nih.mipav.model.structures;


import java.util.*;
import gov.nih.mipav.view.MipavUtil;


/**
 * Class that stores matrices (owned by ModelImage) with accessor functions to the matrix map/adding and changing
 * matrices.
 *
 * @author  linkb
 */
public class MatrixHolder extends ModelSerialCloneable {

    //~ Instance fields ------------------------------------------------------------------------------------------------

	private static final long serialVersionUID = 6886161945452841455L;


	/** The composite matrix formed by multiplying (in reverse order) all stored matrices. */
    private TransMatrix compositeMatrix = null;


    /** Linked hash map that will store all of the images associated matrices. */
    private LinkedHashMap<String, TransMatrix> matrixMap;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     *
     * @param  nDims  dimensions of image
     */
    public MatrixHolder(int nDims) {
        this.matrixMap = new LinkedHashMap<String, TransMatrix>();


        // go ahead and set the composite matrix
        // this way JDialogImageInfo will always have one to display

        if (nDims == 2) {
            compositeMatrix = new TransMatrix(3, TransMatrix.TRANSFORM_COMPOSITE);
        } else if (nDims >= 3) {
            compositeMatrix = new TransMatrix(4, TransMatrix.TRANSFORM_COMPOSITE);
        } //else {
        	//throw new IllegalArgumentException("Dims must be 2 or 3");
        //}
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds a matrix to the map. matrices will be keyed by their type and sequential numbering
     *
     * @param  mat  DOCUMENT ME!
     */
    public void addMatrix(TransMatrix mat) {

        int tID = mat.getTransformID();

        // do not allow adding of Composite type
        if (tID == TransMatrix.TRANSFORM_COMPOSITE) {
        	MipavUtil.displayWarning("Composite matrices can not be added, they are generated dynamically");
            return;
        }

        String key = TransMatrix.getTransformIDStr(mat.getTransformID());

        if (tID != TransMatrix.TRANSFORM_SCANNER_ANATOMICAL) {

            // check to see how many of this type of matrix there is in the linkedhashmap
            int index = 0;

            while (matrixMap.containsKey(new String(TransMatrix.getTransformIDStr(mat.getTransformID()) + index))) {
                index++;
            }

            key += index;

        }

        matrixMap.put(key, mat);

    }

    /**
     * Returns up to two composite TransMatrices that are NIFTI specific
     * @return 0,1, or 2 TransMatrices (related to NIFTI)
     */
    public TransMatrix [] getNIFTICompositeMatrices() {
    	TransMatrix [] composites = new TransMatrix[2];
    	
    	 Iterator<String> iter = matrixMap.keySet().iterator();
         String nextKey = null;
         
         TransMatrix tempMatrix = null;
         
         int idx = 0;
         while (iter.hasNext()) {
         	nextKey = iter.next();
         	tempMatrix = matrixMap.get(nextKey);
         	if (tempMatrix.isNIFTI() && idx < 2) {
         		composites[idx] = new TransMatrix(getCompositeMatrix(true));
         		composites[idx].mult(tempMatrix);
         		composites[idx].setIsNIFTI(true);
         		if (tempMatrix.isQform()) {
         			composites[idx].setIsQform(true);
         		}
         		idx++;
         	}
         }
    	
    	return composites;
    }
    

    /**
     * Removes all matrices from the map.
     */
    public void clearMatrices() {
        matrixMap.clear();
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
    
    /**
     * Determines if the map contains a matrix of the given type (checks for String.contains()).
     *
     * @param   type  transformID to check
     *
     * @return  whether the map contains this type of transform
     */
    public boolean containsType(int type) {

        Iterator<String> iter = matrixMap.keySet().iterator();
        String nextKey = null;
        while (iter.hasNext()) {
        	nextKey = iter.next();
            if (nextKey.startsWith(TransMatrix.getTransformIDStr(type))) {
                return true;
            }
        }
        return false;
    }

    /**
     * Gets the composite (dynamically built) matrix made by multiplying in forward order the image's matrices.
     *
     * @param   useDICOM  whether to include the scanner anatomical matrix (if available)
     *
     * @return  the composite TransMatrix
     */
    public TransMatrix getCompositeMatrix(boolean useDICOM) {

        // here we will dynamically create the composite matrix, with or without DICOM (scanner anatomical) if present

        compositeMatrix.identity();

        Set<String> keySet = matrixMap.keySet();
        String[] keys = keySet.toArray(new String[] { });
        TransMatrix tempMatrix = null;

        for (int i = 0; i < keys.length; i++) {
            tempMatrix = matrixMap.get(keys[i]);
                     
            //if the composite matrix is not the same size as the matrix stored, change the composite matrix to be 
            //   the correct size
            if (compositeMatrix.getDim() != tempMatrix.getDim()) {
            	compositeMatrix = new TransMatrix(tempMatrix.getDim(), TransMatrix.TRANSFORM_COMPOSITE);
            	compositeMatrix.identity();
            }
            
            //do not include a nifti associated matrices (matrices loaded w\ a NIFTI image)
            if (((tempMatrix.getTransformID() != TransMatrix.TRANSFORM_SCANNER_ANATOMICAL) || useDICOM)
            		&& (!tempMatrix.isNIFTI())) {
                compositeMatrix.mult(tempMatrix);
                
            }
        }

        // System.err.println("Composite Matrix: " + compositeMatrix);


        return compositeMatrix;
    }

    /**
     * Accessor that gets a matrix based on the key (for the linkedhashmap)
     * @param key key to the matrix
     * @return the matrix associated with the key, null otherwise
     */
    public TransMatrix getMatrix(Object key) {
    	TransMatrix mat = null;
    	try {
    		mat = matrixMap.get(key);
    	} catch (Exception e) {
    	}
    	
    	
    	return mat;
    }
    
    /**
     * Gets a cloned copy of the image's matrices stored in a vector.
     *
     * @return  Vector of image's matrices
     */
    public Vector<TransMatrix> getMatrices() {
        Vector<TransMatrix> tempV = new Vector<TransMatrix>();

        Iterator<String> iter = matrixMap.keySet().iterator();
        TransMatrix tempM = null;

        while (iter.hasNext()) {
            tempM = new TransMatrix(matrixMap.get(iter.next()));

            // System.err.println("getting matrix from src image, type: " + tempM.getTransformID());
            tempV.add(tempM);
        }

        return tempV;
    }

    /**
     * Gets the matrix map.
     *
     * @return  the matrix map
     */
    public LinkedHashMap<String, TransMatrix> getMatrixMap() {
        return matrixMap;
    }

    /**
     * Safe method for removing matrices from the image
     *
     * @param  key  DOCUMENT ME!
     */
    public void removeMatrix(Object key) {
        matrixMap.remove(key);
    }

    /**
     * Helper function for XML type images which may have several associated matrices.
     *
     * @param  matrixVector  Vector of matrices to replace the current matrices in the map
     */
    public void replaceMatrices(Vector<TransMatrix> matrixVector) {
        // remove all first
        clearMatrices();

        TransMatrix tMatrix = null;

        // add each to the map
        for (int i = 0; i < matrixVector.size(); i++) {
            tMatrix = (TransMatrix) matrixVector.elementAt(i);
            addMatrix(tMatrix);
        }
    }

    /**
     * Replaces the matrix at the given key (or inserts if that key is not present, which should not happen).
     *
     * @param  key        the key for the matrix
     * @param  newMatrix  the new matrix
     */
    public void replaceMatrix(String key, TransMatrix newMatrix) {
        
        if (key == null) {
            System.err.println("key for the matrix is null, aborting replaceMatrix()");
            return;
        }

        if (key.indexOf(TransMatrix.getTransformIDStr(newMatrix.getTransformID())) == -1) {

            // we are replacing via key something of the wrong type.  key and tID do not match
            System.err.println("Replacing wrong type, aborting replaceMatrix()");

            return;
        }

        matrixMap.put(key, newMatrix);
    }
    
    public String toString() {
    	String desc = new String("MatrixHolder: ");
    	desc += "\n\tNumber of matrices: " + matrixMap.size();
    	 Iterator<String> iter = matrixMap.keySet().iterator();

         int count = 0;
         while (iter.hasNext()) {
        	 desc+= "\n\t\t" + count + ": " + matrixMap.get(iter.next());
         }
    	
    	
    	return desc;
    }
    
}
