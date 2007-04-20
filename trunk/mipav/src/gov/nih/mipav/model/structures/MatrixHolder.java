package gov.nih.mipav.model.structures;

import gov.nih.mipav.view.MipavUtil;

import java.util.*;


/**
 * Class that stores matrices (owned by ModelImage) with accessor functions to the matrix map/adding and changing matrices
 * @author linkb
 *
 */
public class MatrixHolder {


    /** Linked hash map that will store all of the images associated matrices */
    private LinkedHashMap matrixMap;
	
    /** The composite matrix formed by multiplying (in reverse order) all stored matrices*/
    private TransMatrix compositeMatrix = null;
       

    /**
     * Default constructor
     * @param nDims dimensions of image
     */
	public MatrixHolder(int nDims) {
        this.matrixMap = new LinkedHashMap();
        
        
        //go ahead and set the composite matrix 
        //this way JDialogImageInfo will always have one to display
        
        if (nDims == 2) {
        	compositeMatrix = new TransMatrix(3, TransMatrix.TRANSFORM_COMPOSITE);
        } else {
        	compositeMatrix = new TransMatrix(4, TransMatrix.TRANSFORM_COMPOSITE);
        }
	}

	
	/**
	 * Removes all matrices from the map
	 *
	 */
	public void clearMatrices() {
		matrixMap.clear();
	}
	
	/**
	 * Adds a matrix to the map.  matrices will be keyed by their type and sequential numbering
	 * @param mat
	 */
	public void addMatrix(TransMatrix mat) {
		
		int tID = mat.getTransformID();
				
		//do not allow adding of Composite type
		if (tID == TransMatrix.TRANSFORM_COMPOSITE) {
			return;
		}
		
		String key = TransMatrix.getTransformIDStr(mat.getTransformID());
		
		if (tID != TransMatrix.TRANSFORM_SCANNER_ANATOMICAL &&
				tID != TransMatrix.TRANSFORM_TALAIRACH_TOURNOUX) {
			//check to see how many of this type of matrix there is in the linkedhashmap
			int index = 0;
			while(matrixMap.containsKey(new String(TransMatrix.getTransformIDStr(mat.getTransformID()) + index))) {
				index++;
			}
			key += index;
			
		} 
				
		matrixMap.put(key, mat);
		
	}
	
	/**
	 * Gets the composite (dynamically built) matrix made by multiplying in reverse order the image's matrices
	 * @param useDICOM whether to include the scanner anatomical matrix (if avaiable)
	 * @return the composite TransMatrix
	 */
	public TransMatrix getCompositeMatrix(boolean useDICOM) {
		
		//here we will dynamically create the composite matrix, with or without DICOM (scanner anatomical) if present
		
		compositeMatrix.identity();
		
		Set keySet = matrixMap.keySet();
		Object [] keys = keySet.toArray();
		TransMatrix tempMatrix = null;
		
		for (int i = keys.length-1; i >=0; i--) {
			tempMatrix = (TransMatrix)matrixMap.get(keys[i]);
			if (tempMatrix.getTransformID() != TransMatrix.TRANSFORM_SCANNER_ANATOMICAL 
					|| useDICOM) {
				compositeMatrix.setMatrix((tempMatrix).times(compositeMatrix).getArray());
			}
		}
		
		//System.err.println("Composite Matrix: " + compositeMatrix);
		
		
		return compositeMatrix;
	}
	
	/**
	 * Replaces the matrix at the given key (or inserts if that key is not present, which should not happen)
	 * @param key the key for the matrix
	 * @param newMatrix the new matrix
	 */
	public void replaceMatrix(Object key, TransMatrix newMatrix) {
		if (((String)key).indexOf(TransMatrix.getTransformIDStr(newMatrix.getTransformID())) == -1) {
			//we are replacing via key something of the wrong type.  key and tID do not match
			System.err.println("Replacing wrong type, aborting replaceMatrix()");
			return;
		}
		
		System.err.println("replaceMatrix before: total #: " + matrixMap.keySet().size());
		
		if (matrixMap.containsKey(key)) {
			System.err.println("putting it in @ same key... replacing");
		}
		
		
		
		matrixMap.put(key, newMatrix);
		System.err.println("replaceMatrix after: total #: " + matrixMap.keySet().size());
	}
	
	/**
	 * Safe method for removing matrices from the image (will not allow default/composite to be removed)
	 * @param key
	 */
	public void removeMatrix(Object key) {
		matrixMap.remove(key);
	}
	
	/** 
	 * Gets a cloned copy of the image's matrices stored in a vector
	 * @return Vector of image's matrices
	 */
	public Vector getMatrices() {
		Vector tempV = new Vector();
		
		Iterator iter = matrixMap.keySet().iterator();
		TransMatrix tempM = null;
		
		while(iter.hasNext()) {
			tempM = (TransMatrix)((TransMatrix)matrixMap.get(iter.next())).clone();
			//System.err.println("getting matrix from src image, type: " + tempM.getTransformID());
			tempV.add(tempM);
		}
		
		return tempV;
	}
	
	/**
	 * Helper function for XML type images which may have several associated matrices
	 * @param matrixVector Vector of matrices to replace the current matrices in the map
	 */
	public void replaceMatrices(Vector matrixVector) {
		//remove all first
		clearMatrices();
		
		TransMatrix tMatrix = null;
		//add each to the map
		for (int i = 0; i < matrixVector.size(); i++) {
			tMatrix = (TransMatrix)matrixVector.elementAt(i);
			addMatrix(tMatrix);			
		}
	}
	
	/**
	 * Determines if the map contains a matrix of the given type (checks for String.contains())
	 * @param type transformID to check
	 * @return whether the map contains this type of transform
	 */
	public boolean containsType(int type) {
		
		Iterator iter = matrixMap.keySet().iterator();
		
		while(iter.hasNext()) {
			if ( ((String)iter.next()).indexOf(TransMatrix.getTransformIDStr(type)) != -1) {
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Gets the matrix map
	 * @return the matrix map
	 */
	public LinkedHashMap getMatrixMap() {
		return matrixMap;
	}
}