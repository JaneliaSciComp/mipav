package gov.nih.mipav.util;

import Jama.Matrix;

public class MatrixUtil {

    /**
     * Reverse order rows.
     * @return flipped Matrix
     */
    public static Matrix flipud(Matrix a) {
        int m = a.getRowDimension();
        int n = a.getColumnDimension();

        Matrix X = new Matrix(m, n);
        double[][] C = X.getArray();
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                C[m-1-i][j] = a.get(i,j);
            }
        }
        return X;
    }

    /** Extract a matrix of size r,c starting at the top left */
    public static Matrix extract(int r, int c, Matrix a) {
        Matrix X = new Matrix(r, c);
        double[][] C = X.getArray();
        for (int i = 0; i < r; i++) {
            for (int j = 0; j < c; j++) {
                C[i][j] = a.get(i,j);
            }
        }
        return X;
    }
}
