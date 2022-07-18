package gov.nih.mipav.model.algorithms.registration;

import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.TransMatrixd;

/**
 * Helper class to make it easy to store the necessary information about a minimum. Stores the "point", or vector at
 * which the minimum was reached; the "cost", or value of the cost function at that minimum; and the matrix, which
 * was the true input into the cost function and represents the transformation that gives the minimum cost of
 * differences between the images. Implements Comparable, so that a list of MatrixListItems can be sorted using
 * Java's sort.
 */
public class MatrixListItem implements Comparable<MatrixListItem> {

    /** Cost of function at this minimum. */
    protected double cost;

    /** Matrix with the best transformation divided by half. Might be null. */
    protected TransMatrix halfMatrix;
    
    /** Matrix with the best transformation divided by half. Might be null. */
    protected TransMatrixd halfMatrixd;

    /** Rotations, translations, scales, and skews that make up transformation. */
    protected double[] initial;

    /** Matrix that gives best transformation. */
    protected TransMatrix matrix = null;
    
    /** Matrix that gives best transformation. */
    protected TransMatrixd matrixd = null;

    /**
     * Matrix with the best transformation's z rot and xy translations. Might be
     * null.
     */
    protected TransMatrix midsagMatrix;
    
    /**
     * Matrix with the best transformation's z rot and xy translations. Might be
     * null.
     */
    protected TransMatrixd midsagMatrixd;
    
    

    /**
     * Creates new minimum object, setting the data and copying the point array
     * explicitly.
     * 
     * @param _cost
     *            Cost of this minimum.
     * @param _matrix
     *            Matrix that gives best transformation.
     * @param _initial
     *            Rotations, translations, scales, and skews that make up
     *            transformation.
     */
    public MatrixListItem(double _cost, TransMatrix _matrix, double[] _initial) {
        this.cost = _cost;
        this.matrix = _matrix;
        initial = new double[_initial.length];

        for (int i = 0; i < initial.length; i++) {
            initial[i] = _initial[i];
        }
    }
    
    /**
     * Creates new minimum object, setting the data and copying the point array
     * explicitly.
     * 
     * @param _cost
     *            Cost of this minimum.
     * @param _matrix
     *            Matrix that gives best transformation.
     * @param _initial
     *            Rotations, translations, scales, and skews that make up
     *            transformation.
     */
    public MatrixListItem(double _cost, TransMatrixd _matrix, double[] _initial) {
        this.cost = _cost;
        this.matrixd = _matrix;
        initial = new double[_initial.length];

        for (int i = 0; i < initial.length; i++) {
            initial[i] = _initial[i];
        }
    }

    /**
     * Necessary to implement so that list may be sorted. Returns -1 if this
     * cost is less than the parameter's cost; 1 if this cost is greater than
     * the parameter's cost; and 0 if they are equal.
     * 
     * @param o
     *            MatrixListItem to compare to.
     * 
     * @return -1 if this is less than, 1 if greater than, 0 if equal.
     */
    public int compareTo(MatrixListItem o) {

        if (cost < o.cost) {
            return -1;
        } else if (cost > o.cost) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Creates string of this object with just first 6DOF and cost.
     * 
     * @return Readable string representation of this object.
     */
    public String toAbridgedString() {
        String s = "";
        s += "Cost of " + cost + " at:\n";

        for (int i = 0; i < 3; i++) {
            s += " Rotations : ";
            s += initial[i] + " ";
            s += "\n";
        }

        for (int i = 3; i < 6; i++) {
            s += " Translations : ";
            s += initial[i] + " ";
            s += "\n";
        }

        return s;
    }

    /**
     * Creates readable string of this object, including cost, matrix, and point
     * with its meanings.
     * 
     * @return Readable string representation of this object.
     */
    public String toString() {
        StringBuffer sb = new StringBuffer("");
        sb.append("Cost of " + cost + " at:\n");
        if (matrix != null) {
            sb.append(matrix.toString());
        }
        if (matrixd != null) {
        	sb.append(matrixd.toString());
        }
        sb.append("\n");
        sb.append("Point:\n");
        if (initial.length == 7) {
            sb.append(" Rotation : ");
            sb.append(initial[0]);
            sb.append("\n");

            for (int i = 1; i < 3; i++) {
                sb.append(" Translations : ");
                sb.append(initial[i] + " ");
                sb.append("\n");
            }

            for (int i = 3; i < 5; i++) {
                sb.append(" Zooms : ");
                sb.append(initial[i] + " ");
                sb.append("\n");
            }

            for (int i = 5; i < 7; i++) {
                sb.append(" Skews : ");
                sb.append(initial[i] + " ");
                sb.append("\n");
            }
        } else {
            for (int i = 0; i < 3; i++) {
                sb.append(" Rotations : ");
                sb.append(initial[i] + " ");
                sb.append("\n");
            }

            for (int i = 3; i < 6; i++) {
                sb.append(" Translations : ");
                sb.append(initial[i] + " ");
                sb.append("\n");
            }

            for (int i = 6; i < 9; i++) {
                sb.append(" Zooms : ");
                sb.append(initial[i] + " ");
                sb.append("\n");
            }

            for (int i = 9; i < 12; i++) {
                sb.append(" Skews : ");
                sb.append(initial[i] + " ");
                sb.append("\n");
            }

        }
        return sb.toString();
    }

    /**
     * Returns the cost.
     * @return
     */
    public double getCost(){
        return cost;
    }
}
