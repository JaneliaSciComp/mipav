package gov.nih.mipav.model.algorithms;


import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.NumericalAnalysis.Eigenf;

import java.util.*;


/**
 * Approximate a set of points by a quadric surface. The surface is implicitly represented as the quadratic equation
 *
 * <pre>
     0 = C[0] + C[1]*X + C[2]*Y + C[3]*Z + C[4]*X^2 + C[5]*Y^2
         + C[6]*Z^2 + C[7]*X*Y + C[8]*X*Z + C[9]*Y*Z
 * </pre>
 *
 * <p>The coefficients are estimated using a least-squares algorithm. There is one degree of freedom in the
 * coefficients. It is removed by requiring the vector C = (C[0],...,C[9]) to be unit length. The least-square problem
 * is to minimize E(C) = C^t M C with M = (sum_i V_i)(sum_i V_i)^t and</p>
 *
 * <pre>
     V = (1, X, Y, Z, X^2, Y^2, Z^2, X*Y, X*Z, Y*Z)
 * </pre>
 *
 * <p>Minimum value is the smallest eigenvalue of M and C is a corresponding unit length eigenvector.<br>
 * <br>
 * </p>
 *
 * <p>The quadratic equation can be factored into P^T A P + B^T P + K = 0 where P = (X,Y,Z), K = C[0], B =
 * (C[1],C[2],C[3]), and A is a 3x3 symmetric matrix with A00 = C[4], A11 = C[5], A22 = C[6], A01 = C[7]/2, A02 =
 * C[8]/2, and A12 = C[9]/2. Matrix A = R^T D R where R is orthogonal and D is diagonal (using an eigendecomposition).
 * Define V = R P = (v0,v1,v2), E = R B = (e0,e1,e2), D = diag(d0,d1,d2), and f = K to obtain</p>
 *
 * <pre>
     d0 v0^2 + d1 v1^2 + d2 v^2 + e0 v0 + e1 v1 + e2 v2 + f = 0
 * </pre>
 *
 * <p>The characterization of the surface depends on the signs of the d_i. For the MRI brain segmentation, the signs d_i
 * are all positive, thereby producing an ellipsoid.<br>
 * <br>
 * </p>
 *
 * <p>The quadratic equation can finally be factored into</p>
 *
 * <pre>
     (P-G)^T R^T D R (P-G) = L
 * </pre>
 *
 * <p>where G is the center of the surface, R is the orientation matrix, D is a diagonal matrix, and L is a constant.
 * </p>
 */

public class AlgorithmQuadraticFit {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** the coeffficients of the approximating quadratic equation. */
    protected float[] m_afCoeff;

    /** DOCUMENT ME! */
    protected float[] m_afDiagonal;

    /** DOCUMENT ME! */
    protected float m_fConstant;

    /**
     * The center G, orientation R, diagonal matrix D, and constant L in the factorization (P-G)^T R^T D R (P-G) = L.
     */
    protected Vector3f m_kCenter;

    /** DOCUMENT ME! */
    protected Matrix3f m_kOrient;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Fit a set of 3D points with a quadric surface. The various representations of the surface can be accessed by
     * member functions.
     *
     * @param  kPoints  a vector of points, each vector element is of type Point3f
     */
    public AlgorithmQuadraticFit(Vector<Vector3f> kPoints) {
        Eigenf kES = new Eigenf(10);
        int iRow, iCol;

        for (iRow = 0; iRow < 10; iRow++) {

            for (iCol = 0; iCol < 10; iCol++) {
                kES.SetData(iRow, iCol, 0.0f);
            }
        }

        for (int i = 0; i < kPoints.size(); i++) {
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

            kES.AddToData(0, 1, fX);
            kES.AddToData(0, 2, fY);
            kES.AddToData(0, 3, fZ);
            kES.AddToData(0, 4, fX2);
            kES.AddToData(0, 5, fY2);
            kES.AddToData(0, 6, fZ2);
            kES.AddToData(0, 7, fXY);
            kES.AddToData(0, 8, fXZ);
            kES.AddToData(0, 9, fYZ);
            kES.AddToData(1, 4, fX3);
            kES.AddToData(1, 5, fXY2);
            kES.AddToData(1, 6, fXZ2);
            kES.AddToData(1, 7, fX2Y);
            kES.AddToData(1, 8, fX2Z);
            kES.AddToData(1, 9, fXYZ);
            kES.AddToData(2, 5, fY3);
            kES.AddToData(2, 6, fYZ2);
            kES.AddToData(2, 9, fY2Z);
            kES.AddToData(3, 6, fZ3);
            kES.AddToData(4, 4, fX4);
            kES.AddToData(4, 5, fX2Y2);
            kES.AddToData(4, 6, fX2Z2);
            kES.AddToData(4, 7, fX3Y);
            kES.AddToData(4, 8, fX3Z);
            kES.AddToData(4, 9, fX2YZ);
            kES.AddToData(5, 5, fY4);
            kES.AddToData(5, 6, fY2Z2);
            kES.AddToData(5, 7, fXY3);
            kES.AddToData(5, 8, fXY2Z);
            kES.AddToData(5, 9, fY3Z);
            kES.AddToData(6, 6, fZ4);
            kES.AddToData(6, 7, fXYZ2);
            kES.AddToData(6, 8, fXZ3);
            kES.AddToData(6, 9, fYZ3);
            kES.AddToData(9, 9, fY2Z2);

        }

        kES.SetData(0, 0, (float) kPoints.size());
        kES.SetData(1, 1, kES.GetData(0, 4));
        kES.SetData(1, 2, kES.GetData(0, 7));
        kES.SetData(1, 3, kES.GetData(0, 8));
        kES.SetData(2, 2, kES.GetData(0, 5));
        kES.SetData(2, 3, kES.GetData(0, 9));
        kES.SetData(2, 4, kES.GetData(1, 7));
        kES.SetData(2, 7, kES.GetData(1, 5));
        kES.SetData(2, 8, kES.GetData(1, 9));
        kES.SetData(3, 3, kES.GetData(0, 6));
        kES.SetData(3, 4, kES.GetData(1, 8));
        kES.SetData(3, 5, kES.GetData(2, 9));
        kES.SetData(3, 7, kES.GetData(1, 9));
        kES.SetData(3, 8, kES.GetData(1, 6));
        kES.SetData(3, 9, kES.GetData(2, 6));
        kES.SetData(7, 7, kES.GetData(4, 5));
        kES.SetData(7, 8, kES.GetData(4, 9));
        kES.SetData(7, 9, kES.GetData(5, 8));
        kES.SetData(8, 8, kES.GetData(4, 6));
        kES.SetData(8, 9, kES.GetData(6, 7));
        kES.SetData(9, 9, kES.GetData(5, 6));

        for (iRow = 0; iRow < 10; iRow++) {

            for (iCol = 0; iCol < iRow; iCol++) {
                kES.SetData(iRow, iCol, kES.GetData(iCol, iRow));
             }
        }

        kES.IncrSortEigenStuff();

        // compute the coefficients of the quadratic equation
        m_afCoeff = new float[10];

        for (iRow = 0; iRow < 10; iRow++) {
            m_afCoeff[iRow] = (float) kES.GetEigenvector(iRow, 0);
        }

        m_kCenter = new Vector3f();
        m_kOrient = new Matrix3f();
        m_afDiagonal = new float[3];

        // Extract the center C and constant L so that (X-C)^T*A*(X-C) = L
        // where X = (x,y,z) is 3x1, A is symmetric 3x3, and L is a constant.
        Matrix3f kA = new Matrix3f(m_afCoeff[4], 0.5f * m_afCoeff[7], 0.5f * m_afCoeff[8], 0.5f * m_afCoeff[7],
                                   m_afCoeff[5], 0.5f * m_afCoeff[9], 0.5f * m_afCoeff[8], 0.5f * m_afCoeff[9],
                                   m_afCoeff[6]);
        Vector3f kB = new Vector3f(m_afCoeff[1], m_afCoeff[2], m_afCoeff[3]);
        Matrix3f kInvA = Matrix3f.inverse(kA);
        kInvA.mult(kB, m_kCenter);
        m_kCenter.scale(-0.5f);
        kA.mult(m_kCenter, kB);
        m_fConstant = (m_kCenter.X * kB.X) + (m_kCenter.Y * kB.Y) + (m_kCenter.Z * kB.Z) - m_afCoeff[0];

        // factor A = R*D*R^T where R is a rotation and D is diagonal
        kES = new Eigenf(3);
        kES.SetData(0, 0, kA.M00);
        kES.SetData(0, 1, kA.M01);
        kES.SetData(0, 2, kA.M02);
        kES.SetData(1, 0, kA.M10);
        kES.SetData(1, 1, kA.M11);
        kES.SetData(1, 2, kA.M12);
        kES.SetData(2, 0, kA.M20);
        kES.SetData(2, 1, kA.M21);
        kES.SetData(2, 2, kA.M22);
        kES.IncrSortEigenStuff();

        // the orientation is determined by the eigenvectors
        m_kOrient.M00 = (float) kES.GetEigenvector(0, 0);
        m_kOrient.M01 = (float) kES.GetEigenvector(0, 1);
        m_kOrient.M02 = (float) kES.GetEigenvector(0, 2);
        m_kOrient.M10 = (float) kES.GetEigenvector(1, 0);
        m_kOrient.M11 = (float) kES.GetEigenvector(1, 1);
        m_kOrient.M12 = (float) kES.GetEigenvector(1, 2);
        m_kOrient.M20 = (float) kES.GetEigenvector(2, 0);
        m_kOrient.M21 = (float) kES.GetEigenvector(2, 1);
        m_kOrient.M22 = (float) kES.GetEigenvector(2, 2);

        // get the diagonal components
        m_afDiagonal[0] = kES.GetEigenvalue(0);
        m_afDiagonal[1] = kES.GetEigenvalue(1);
        m_afDiagonal[2] = kES.GetEigenvalue(2);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Get the center of the quadric surface. This is the vector G in the factorization (P-G)^T R^T D R (P-G) = L.
     *
     * @return  the center point
     */
    public final Vector3f getCenter() {
        return m_kCenter;
    }

    /**
     * Get the coefficients of the quadratic equation that represents the approximating quadric surface.
     *
     * @param   i  the coefficient index (0 <= i < 10)
     *
     * @return  the value of the coefficient
     */
    public final float getCoefficient(int i) {

        // assert:  0 <= i < 10
        return m_afCoeff[i];
    }

    /**
     * Get the constant term for the factored quadratic equation. This is the L term in (P-G)^T R^T D R (P-G) = L.
     *
     * @return  the constant term
     */
    public final float getConstant() {
        return m_fConstant;
    }

    /**
     * Get a diagonal entry for the quadric surface. This is a diagonal term for the matrix D in the factorization A =
     * R^T D R where A is the coefficient matrix for the quadratic term in the equation.
     *
     * @param   i  the index of the diagonal (0 <= i < 3)
     *
     * @return  the diagonal entry
     */
    public final float getDiagonal(int i) {

        // assert:  0 <= i < 3
        return m_afDiagonal[i];
    }

    /**
     * Get the orientation of the quadric surface. This is the matrix R in the factorization A = R^T D R where A is the
     * coefficient matrix for the quadratic term in the equation.
     *
     * @return  the orientation matrix
     */
    public final Matrix3f getOrient() {
        return m_kOrient;
    }
}
