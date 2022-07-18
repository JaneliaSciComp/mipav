package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import Jama.*;

import gov.nih.mipav.view.*;

import java.util.*;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;


/**
 * This code fits data points to a 3D ellipsoid. The part of the code up to the derivation of v[0] through v[9] for the
 * 3D ellipsoid model comes from the port of MATLAB code provided by Lecturer Qingde Li Department of Computer Science,
 * University of Hull, Hull HU6 7RX, UK [q.li@hull.ac.uk] Reference: "Least Squares Ellipsoid Specific Fitting" by
 * Qingde Li and John G. Griffiths, Proceedings of the Geometric Modeling and Processing 2004, IEEE Computer Society.
 *
 * <p>The second part of this code that derives the length of the three axes from the ellipsoid equation is based on
 * equations found in the article "Compound Extraction and Fitting Method for Detecting Cardiac Ventricle in SPECT Data"
 * by Timothy S. Newman and Hong Yi, 15th International Conference on Pattern Recognition, Vol. 4, 2000, pp. 328-331.
 * </p>
 */
public class AlgorithmEllipsoidFit {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    double[] axis = new double[3];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmEllipsoidFit object.
     *
     * @param  kPoints  a vector of points, each vector element is of type Point3f
     */
    public AlgorithmEllipsoidFit(Vector<Vector3f> kPoints) {
        double[][] S11 = new double[6][6];
        double[][] S12 = new double[6][4];
        double[][] S22 = new double[4][4];
        double[][] C = new double[6][6];
        int i;
        C[0][0] = -1.0;
        C[1][1] = -1.0;
        C[2][2] = -1.0;
        C[3][3] = -4.0;
        C[4][4] = -4.0;
        C[5][5] = -4.0;
        C[0][1] = 1;
        C[1][0] = 1;
        C[0][2] = 1;
        C[2][0] = 1;
        C[1][2] = 1;
        C[2][1] = 1;

        for (i = 0; i < kPoints.size(); i++) {
            Vector3f kPoint = (Vector3f) kPoints.get(i);
            float fX = kPoint.X;
            float fY = kPoint.Y;
            float fZ = kPoint.Z;
            float fX2 = fX * fX;
            float fY2 = fY * fY;
            float fZ2 = fZ * fZ;
            float fXY = fX * fY;
            float fXZ = fX * fZ;
            float fYZ = fY * fZ;
            float fX3 = fX * fX2;
            float fXY2 = fX * fY2;
            float fXZ2 = fX * fZ2;
            float fX2Y = fX * fXY;
            float fX2Z = fX * fXZ;
            float fXYZ = fX * fY * fZ;
            float fY3 = fY * fY2;
            float fYZ2 = fY * fZ2;
            float fY2Z = fY * fYZ;
            float fZ3 = fZ * fZ2;
            float fX4 = fX * fX3;
            float fX2Y2 = fX * fXY2;
            float fX2Z2 = fX * fXZ2;
            float fX3Y = fX * fX2Y;
            float fX3Z = fX * fX2Z;
            float fX2YZ = fX * fXYZ;
            float fY4 = fY * fY3;
            float fY2Z2 = fY * fYZ2;
            float fXY3 = fX * fY3;
            float fXY2Z = fX * fY2Z;
            float fY3Z = fY * fY2Z;
            float fZ4 = fZ * fZ3;
            float fXYZ2 = fX * fYZ2;
            float fXZ3 = fX * fZ3;
            float fYZ3 = fY * fZ3;

            S11[0][0] += fX4;
            S11[0][1] += fX2Y2;
            S11[0][2] += fX2Z2;
            S11[0][3] += fX2YZ;

            // S11[0][3] += 2.0*fX2YZ;
            S11[0][4] += fX3Z;

            // S11[0][4] += 2.0*fX3Z;
            S11[0][5] += fX3Y;

            // S11[0][5] += 2.0*fX3Y;
            S11[1][1] += fY4;
            S11[1][2] += fY2Z2;
            S11[1][3] += fY3Z;

            // S11[1][3] += 2.0*fY3Z;
            S11[1][4] += fXY2Z;

            // S11[1][4] += 2.0*fXY2Z;
            S11[1][5] += fXY3;

            // S11[1][5] += 2.0*fXY3;
            S11[2][2] += fZ4;
            S11[2][3] += fYZ3;

            // S11[2][3] += 2.0*fYZ3;
            S11[2][4] += fXZ3;

            // S11[2][4] += 2.0*fXZ3;
            S11[2][5] += fXYZ2;
            // S11[2][5] += 2.0*fXYZ2;
            // S11[3][3] += 4.0*fY2Z2;
            // S11[3][4] += 4.0*fXYZ2;
            // S11[3][5] += 4.0*fXY2Z;
            // S11[4][4] += 4.0*fX2Z2;
            // S11[4][5] += 4.0*fX2YZ;
            // S11[5][5] += 4.0*fX2Y2;

            S12[0][0] += fX3;

            // S12[0][0] += 2.0*fX3;
            S12[0][1] += fX2Y;

            // S12[0][1] += 2.0*fX2Y;
            S12[0][2] += fX2Z;

            // s12[0][2] += 2.0*fX2Z;
            S12[0][3] += fX2;
            S12[1][0] += fXY2;

            // S12[1][0] += 2.0*fXY2;
            S12[1][1] += fY3;

            // S12[1][1] += 2.0*fY3;
            S12[1][2] += fY2Z;

            // S12[1][2] += 2.0*fY2Z;
            S12[1][3] += fY2;
            S12[2][0] += fXZ2;

            // S12[2][0] = 2.0*fXZ2;
            S12[2][1] += fYZ2;

            // S12[2][1] += 2.0*fYZ2;
            S12[2][2] += fZ3;

            // S12[2][2] += 2.0*fZ3;
            S12[2][3] += fZ2;
            S12[3][0] += fXYZ;

            // S12[3][0] += 4.0*fXYZ;
            // S12[3][1] += 4.0*fY2Z;
            // S12[3][2] += 4.0*fYZ2;
            S12[3][3] += fYZ;

            // S12[3][3] += 2.0*fYZ;
            // S12[4][0] += 4.0*fX2Z;
            // S12[4][1] += 4.0*fXYZ;
            // S12[4][2] += 4.0*fXZ2;
            S12[4][3] += fXZ;

            // S12[4][3] += 2.0*fXZ;
            // S12[5][0] += 4.0*fX2Y;
            // S12[5][1] += 4.0*fXY2;
            // S12[5][2] += 4.0*fXYZ;
            S12[5][3] += fXY;
            // S12[5][3] += 2.0*fXY;

            // S22[0][0] += 4.0*fX2;
            // S22[0][1] += 4.0*fXY;
            // S22[0][2] += 4.0*fXZ;
            S22[0][3] += fX;

            // S22[0][3] += 2.0*fX;
            // S22[1][1] += 4.0*fY2;
            // S22[1][2] += 4.0*fYZ;
            S22[1][3] += fY;

            // S22[1][3] += 2.0*fY;
            // S22[2][2] += 4.0*fZ2;
            S22[2][3] += fZ;
            // S22[2][3] += 2.0*fZ;
            // S22[3][3] += 1;
        }

        S11[0][3] *= 2.0;
        S11[0][4] *= 2.0;
        S11[0][5] *= 2.0;
        S11[1][3] *= 2.0;
        S11[1][4] *= 2.0;
        S11[1][5] *= 2.0;
        S11[2][3] *= 2.0;
        S11[2][4] *= 2.0;
        S11[2][5] *= 2.0;
        S11[3][3] = 4.0 * S11[1][2];
        S11[3][4] = 2.0 * S11[2][5];
        S11[3][5] = 2.0 * S11[1][4];
        S11[4][4] = 4.0 * S11[0][2];
        S11[4][5] = 2.0 * S11[0][3];
        S11[5][5] = 4.0 * S11[0][1];
        S11[1][0] = S11[0][1];
        S11[2][0] = S11[0][2];
        S11[3][0] = S11[0][3];
        S11[4][0] = S11[0][4];
        S11[5][0] = S11[0][5];
        S11[2][1] = S11[1][2];
        S11[3][1] = S11[1][3];
        S11[4][1] = S11[1][4];
        S11[5][1] = S11[1][5];
        S11[3][2] = S11[2][3];
        S11[4][2] = S11[2][4];
        S11[5][2] = S11[2][5];
        S11[4][3] = S11[3][4];
        S11[5][3] = S11[3][5];
        S11[5][4] = S11[4][5];

        S12[0][0] *= 2.0;
        S12[0][1] *= 2.0;
        S12[0][2] *= 2.0;
        S12[1][0] *= 2.0;
        S12[1][1] *= 2.0;
        S12[1][2] *= 2.0;
        S12[2][0] *= 2.0;
        S12[2][1] *= 2.0;
        S12[2][2] *= 2.0;
        S12[3][0] *= 4.0;
        S12[3][1] = 2.0 * S12[1][2];
        S12[3][2] = 2.0 * S12[2][1];
        S12[3][3] *= 2.0;
        S12[4][0] = 2.0 * S12[0][2];
        S12[4][1] = S12[3][0];
        S12[4][2] = 2.0 * S12[2][0];
        S12[4][3] *= 2.0;
        S12[5][0] = 2.0 * S12[0][1];
        S12[5][1] = 2.0 * S12[1][0];
        S12[5][2] = S12[3][0];
        S12[5][3] *= 2.0;

        S22[0][0] = 4.0 * S12[0][3];
        S22[0][1] = 2.0 * S12[5][3];
        S22[0][2] = 2.0 * S12[4][3];
        S22[0][3] *= 2.0;
        S22[1][1] = 4.0 * S12[1][3];
        S22[1][2] = 2.0 * S12[3][3];
        S22[1][3] *= 2.0;
        S22[2][2] = 4.0 * S12[2][3];
        S22[2][3] *= 2.0;
        S22[3][3] = kPoints.size();
        S22[1][0] = S22[0][1];
        S22[2][0] = S22[0][2];
        S22[3][0] = S22[0][3];
        S22[2][1] = S22[1][2];
        S22[3][1] = S22[1][3];
        S22[3][2] = S22[2][3];

        Matrix matD = new Matrix(S22);

        // Generate inv(S22)
        matD = matD.inverse();

        Matrix matE = new Matrix(S12);

        // Generate inv(S22)*S12'
        matD = matD.times(matE.transpose());

        // Generate S12*inv(S22)*S12'
        matE = matE.times(matD);

        Matrix matA = new Matrix(S11);

        // Generate A = S11 - S12*inv(S22)*S12'
        matA = matA.minus(matE);

        Matrix matC = new Matrix(C);

        // Generate CA = inv(C)*A;
        Matrix matCA = (matC.inverse()).times(matA);

        double[] eigenvalue = new double[matCA.getColumnDimension()];
        double[][] eigenvector = new double[matCA.getRowDimension()][matCA.getColumnDimension()];
        double temp;
        double[] tempCol = new double[6];
        int m, n, index;
        // In EigenvalueDecomposition the columns represent the
        // eigenvectors
        Eigenvalue.decompose( matCA.getArray(), eigenvector, eigenvalue);

        // Arrange the eigenvalues and corresponding eigenvectors
        // in descending order so that e0 >= e1 >= e2
        // Only the largest eigenvalue should be positive
        for (m = 0; m < 6; m++) {
            index = m;

            for (n = m + 1; n < 6; n++) {

                if (eigenvalue[n] > eigenvalue[index]) {
                    index = n;
                }
            } // for (m = m+1; n < 6; n++)

            if (index != m) {
                temp = eigenvalue[m];
                eigenvalue[m] = eigenvalue[index];
                eigenvalue[index] = temp;

                for (n = 0; n < 6; n++) {
                    tempCol[n] = eigenvector[n][m];
                    eigenvector[n][m] = eigenvector[n][index];
                    eigenvector[n][index] = tempCol[n];
                }
            } // if (index != m)
        } // for (m = 0; m < 6; m++)

        // Place the eigenvector corresponding to the positive eigenvalue
        // in v1
        double[][] v1 = new double[6][1];

        for (i = 0; i < 6; i++) {
            v1[i][0] = eigenvector[i][0];
            Preferences.debug("Eigenvalue[" + (i + 1) + "] = " + eigenvalue[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        Matrix matV1 = new Matrix(v1);
        Matrix matV2 = matD.times(matV1);

        // V2 = -inv(S22)*S12'*v1
        matV2 = matV2.times(-1.0);

        double[][] v2 = matV2.getArray();
        double[] v = new double[10];

        for (i = 0; i < 6; i++) {
            v[i] = v1[i][0];
        }

        for (i = 0; i < 4; i++) {
            v[i + 6] = v2[i][0];
        }

        // The ellipsoid equation is
        // v[0]*x**2 + v[1]*y**2 + v[2]*z**2 + 2*v[3]*y*z + 2*v[4]*x*z + 2*v[5]*x*y
        // + 2*v[6]*x + 2*v[7]*y + 2*v[8]*z + v[9] = 0
        // Now put in the form of the equation:
        // a[0]*x**2 + a[1]*y**2 + a[2]*z**2 + a[3]*x*y + a[4]*x*z + a[5]*y*z
        // + a[6]*x + a[7]*y + a[8]*z = 1
        double[] a = new double[9];
        a[0] = -v[0] / v[9];
        a[1] = -v[1] / v[9];
        a[2] = -v[2] / v[9];
        a[3] = -2.0 * v[5] / v[9];
        a[4] = -2.0 * v[4] / v[9];
        a[5] = -2.0 * v[3] / v[9];
        a[6] = -2.0 * v[6] / v[9];
        a[7] = -2.0 * v[7] / v[9];
        a[8] = -2.0 * v[8] / v[9];

        // -2a[0]x0 - a[3]y0 - a[4]z0 = a[6]
        // -a[3]x0 - 2a[1]y0 - a[5]z0 = a[7]
        // -a[4]x0 - a[5]y0 - 2a[2]z0 = a[8]
        double[][] denom = new double[3][3];
        denom[0][0] = -2.0 * a[0];
        denom[0][1] = -a[3];
        denom[0][2] = -a[4];
        denom[1][0] = -a[3];
        denom[1][1] = -2.0 * a[1];
        denom[1][2] = -a[5];
        denom[2][0] = -a[4];
        denom[2][1] = -a[5];
        denom[2][2] = -2.0 * a[2];

        Matrix matDenom = new Matrix(denom);
        double denomDet = matDenom.det();
        double[][] num1 = new double[3][3];
        num1[0][0] = a[6];
        num1[0][1] = -a[3];
        num1[0][2] = -a[4];
        num1[1][0] = a[7];
        num1[1][1] = -2.0 * a[1];
        num1[1][2] = -a[5];
        num1[2][0] = a[8];
        num1[2][1] = -a[5];
        num1[2][2] = -2.0 * a[2];

        Matrix matNum = new Matrix(num1);
        double num1Det = matNum.det();
        double x0 = num1Det / denomDet;
        double[][] num2 = new double[3][3];
        num2[0][0] = -2.0 * a[0];
        num2[0][1] = a[6];
        num2[0][2] = -a[4];
        num2[1][0] = -a[3];
        num2[1][1] = a[7];
        num2[1][2] = -a[5];
        num2[2][0] = -a[4];
        num2[2][1] = a[8];
        num2[2][2] = -2.0 * a[2];
        matNum = new Matrix(num2);

        double num2Det = matNum.det();
        double y0 = num2Det / denomDet;
        double[][] num3 = new double[3][3];
        num3[0][0] = -2.0 * a[0];
        num3[0][1] = -a[3];
        num3[0][2] = a[6];
        num3[1][0] = -a[3];
        num3[1][1] = -2.0 * a[1];
        num3[1][2] = a[7];
        num3[2][0] = -a[4];
        num3[2][1] = -a[5];
        num3[2][2] = a[8];
        matNum = new Matrix(num3);

        double num3Det = matNum.det();
        double z0 = num3Det / denomDet;
        double e = (a[0] * x0 * x0) + (a[1] * y0 * y0) + (a[2] * z0 * z0);
        double dp = 1.0 / (1.0 + (a[3] * x0 * y0) + (a[4] * x0 * z0) + (a[5] * y0 * z0) + e);
        double[][] G1 = new double[3][3];
        G1[0][0] = a[0] * dp;
        G1[1][1] = a[1] * dp;
        G1[2][2] = a[2] * dp;
        G1[0][1] = 0.5 * a[3] * dp;
        G1[0][2] = 0.5 * a[4] * dp;
        G1[1][0] = G1[0][1];
        G1[1][2] = 0.5 * a[5] * dp;
        G1[2][0] = G1[0][2];
        G1[2][1] = G1[1][2];

        Matrix matG1 = new Matrix(G1);
        eigenvalue = new double[matG1.getColumnDimension()];
        eigenvector = new double[matG1.getRowDimension()][matCA.getColumnDimension()];
        // In EigenvalueDecomposition the columns represent the
        // eigenvectors
        Eigenvalue.decompose( matCA.getArray(), eigenvector, eigenvalue);

        // Arrange the eigenvalues and corresponding eigenvectors
        // in descending order so that e0 >= e1 >= e2
        for (m = 0; m < 3; m++) {
            index = m;

            for (n = m + 1; n < 3; n++) {

                if (eigenvalue[n] > eigenvalue[index]) {
                    index = n;
                }
            } // for (m = m+1; n < 3; n++)

            if (index != m) {
                temp = eigenvalue[m];
                eigenvalue[m] = eigenvalue[index];
                eigenvalue[index] = temp;

                for (n = 0; n < 3; n++) {
                    tempCol[n] = eigenvector[n][m];
                    eigenvector[n][m] = eigenvector[n][index];
                    eigenvector[n][index] = tempCol[n];
                }
            } // if (index != m)
        } // for (m = 0; m < 3; m++)

        // The eigenvalues of G1 are 1/a**2, 1/b**2, and 1/c**2, where a, b, and c
        // are the half-lengths of the principal axes.
        axis[0] = 2.0 / Math.sqrt(eigenvalue[2]);
        axis[1] = 2.0 / Math.sqrt(eigenvalue[1]);
        axis[2] = 2.0 / Math.sqrt(eigenvalue[0]);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double[] getAxes() {
        return axis;
    }
}
