package gov.nih.mipav.model.algorithms;


/**
 * A numerical eigensolver for symmetric matrices. The code is a minor variation of that found in Numerical Recipes in
 * C.
 */

public class AlgorithmEigensolver {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The matrix for eigensolving. It must be symmetric. */
    protected double[][] m_aafMat;

    /** Storage for the eigenvalues of the matrix. */
    protected double[] m_afDiag;

    /** Temporary storage for the reduction to a diagonal matrix. */
    protected double[] m_afSubd;

    /** The matrix is m_iSize-by-m_iSize. */
    protected int m_iSize;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create an eigensolver object. The matrices that the solver processes must be square and symmetric.
     *
     * @param  iSize  the number of rows and of columns of the matrix
     */
    public AlgorithmEigensolver(int iSize) {
        m_iSize = iSize; // should be 2 or larger
        m_aafMat = new double[m_iSize][m_iSize];
        m_afDiag = new double[m_iSize];
        m_afSubd = new double[m_iSize];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Get the i-th eigenvalue of the matrix.
     *
     * @param   i  the index of the desired eigenvalue (0 <= i < iSize)
     *
     * @return  the eigenvalue
     */
    public double getEigenvalue(int i) {
        return m_afDiag[i];
    }

    /**
     * Get the row-th coordinate of the col-th eigenvector.
     *
     * @param   iRow  a component of the col-th eigenvector
     * @param   iCol  the index for the column that stores the eigenvector
     *
     * @return  the component of the eigenvector
     */
    public double getEigenvector(int iRow, int iCol) {
        return m_aafMat[iRow][iCol];
    }

    /**
     * Get the matrix value in the specified row and column.
     *
     * @param   iRow  the row index (0 <= iRow < iSize)
     * @param   iCol  the column index (0 <= iCol < iSize)
     *
     * @return  the value of the matrix in that row and column
     */
    public double getMatrix(int iRow, int iCol) {
        return m_aafMat[iRow][iCol];
    }

    /**
     * Set the matrix value in the specified row and column.
     *
     * @param  iRow    the row index (0 <= iRow < iSize)
     * @param  iCol    the column index (0 <= iCol < iSize)
     * @param  fValue  the value for the matrix in that row and column
     */
    public void setMatrix(int iRow, int iCol, double fValue) {
        m_aafMat[iRow][iCol] = fValue;
    }

    /**
     * Solve for the eigenvalues and eigenvectors of the matrix. This function should be called only after the matrix
     * entries are set and the matrix is ensured to be symmetric. The eigenvalues are stored in increasing order and may
     * be accessed by getEigenvalue(i). The corresponding eigenvectors are stored as columns of a matrix. The
     * eigenvector for the i-th eigenvalue is stored in the i-th column of the matrix. Eigenvectors are accessed by
     * getEigenvector(iRow,iCol). The eigenvectors are normalized to unit length.
     */
    public void solve() {
        makeTridiagonal();
        reduceQLAlgorithm();
        increasingSort();
    }

    /**
     * Add a value to the matrix value in the specified row and column.
     *
     * @param  iRow    the row index (0 <= iRow < iSize)
     * @param  iCol    the column index (0 <= iCol < iSize)
     * @param  fValue  the value to add to the matrix value in that row and column
     */
    public void updateMatrix(int iRow, int iCol, double fValue) {
        m_aafMat[iRow][iCol] += fValue;
    }

    /**
     * Sort the eigenvalues and eigenvectors in ascending order.
     */
    protected void increasingSort() {

        // sort eigenvalues in increasing order, e[0] <= ... <= e[iSize-1]
        for (int i0 = 0, i1; i0 <= (m_iSize - 2); i0++) {

            // locate minimum eigenvalue
            i1 = i0;

            double fMin = m_afDiag[i1];
            int i2;

            for (i2 = i0 + 1; i2 < m_iSize; i2++) {

                if (m_afDiag[i2] < fMin) {
                    i1 = i2;
                    fMin = m_afDiag[i1];
                }
            }

            if (i1 != i0) {

                // swap eigenvalues
                m_afDiag[i1] = m_afDiag[i0];
                m_afDiag[i0] = fMin;

                // swap eigenvectors
                for (i2 = 0; i2 < m_iSize; i2++) {
                    double fTmp = m_aafMat[i2][i0];
                    m_aafMat[i2][i0] = m_aafMat[i2][i1];
                    m_aafMat[i2][i1] = fTmp;
                }
            }
        }
    }

    /**
     * Householder reduction to convert a symmetric matrix to a tridiagonal matrix. This code is a minor modification of
     * that from Numerical Recipes in C.
     */
    protected void makeTridiagonal() {
        int i0, i1, i2, i3;

        for (i0 = m_iSize - 1, i3 = m_iSize - 2; i0 >= 1; i0--, i3--) {
            double fH = 0.0, fScale = 0.0;

            if (i3 > 0) {

                for (i2 = 0; i2 <= i3; i2++) {
                    fScale += Math.abs(m_aafMat[i0][i2]);
                }

                if (fScale == 0.0) {
                    m_afSubd[i0] = m_aafMat[i0][i3];
                } else {
                    double fInvScale = 1.0 / fScale;

                    for (i2 = 0; i2 <= i3; i2++) {
                        m_aafMat[i0][i2] *= fInvScale;
                        fH += m_aafMat[i0][i2] * m_aafMat[i0][i2];
                    }

                    double fF = m_aafMat[i0][i3];
                    double fG = Math.sqrt(fH);

                    if (fF > 0.0) {
                        fG = -fG;
                    }

                    m_afSubd[i0] = fScale * fG;
                    fH -= fF * fG;
                    m_aafMat[i0][i3] = fF - fG;
                    fF = 0.0f;

                    double fInvH = 1.0 / fH;

                    for (i1 = 0; i1 <= i3; i1++) {
                        m_aafMat[i1][i0] = m_aafMat[i0][i1] * fInvH;
                        fG = 0.0;

                        for (i2 = 0; i2 <= i1; i2++) {
                            fG += m_aafMat[i1][i2] * m_aafMat[i0][i2];
                        }

                        for (i2 = i1 + 1; i2 <= i3; i2++) {
                            fG += m_aafMat[i2][i1] * m_aafMat[i0][i2];
                        }

                        m_afSubd[i1] = fG * fInvH;
                        fF += m_afSubd[i1] * m_aafMat[i0][i1];
                    }

                    double fHalfFdivH = 0.5 * fF * fInvH;

                    for (i1 = 0; i1 <= i3; i1++) {
                        fF = m_aafMat[i0][i1];
                        fG = m_afSubd[i1] - (fHalfFdivH * fF);
                        m_afSubd[i1] = fG;

                        for (i2 = 0; i2 <= i1; i2++) {
                            m_aafMat[i1][i2] -= (fF * m_afSubd[i2]) + (fG * m_aafMat[i0][i2]);
                        }
                    }
                }
            } else {
                m_afSubd[i0] = m_aafMat[i0][i3];
            }

            m_afDiag[i0] = fH;
        }

        m_afDiag[0] = 0.0;
        m_afSubd[0] = 0.0;

        for (i0 = 0, i3 = -1; i0 <= (m_iSize - 1); i0++, i3++) {

            if (m_afDiag[i0] != 0.0) {

                for (i1 = 0; i1 <= i3; i1++) {
                    double fSum = 0.0;

                    for (i2 = 0; i2 <= i3; i2++) {
                        fSum += m_aafMat[i0][i2] * m_aafMat[i2][i1];
                    }

                    for (i2 = 0; i2 <= i3; i2++) {
                        m_aafMat[i2][i1] -= fSum * m_aafMat[i2][i0];
                    }
                }
            }

            m_afDiag[i0] = m_aafMat[i0][i0];
            m_aafMat[i0][i0] = 1.0;

            for (i1 = 0; i1 <= i3; i1++) {
                m_aafMat[i1][i0] = 0.0;
                m_aafMat[i0][i1] = 0.0;
            }
        }

        // re-ordering for use by QLAlgorithm
        for (i0 = 1, i3 = 0; i0 < m_iSize; i0++, i3++) {
            m_afSubd[i3] = m_afSubd[i0];
        }

        m_afSubd[m_iSize - 1] = 0.0;
    }

    /**
     * Iteratively reduce the tridiagonal matrix obtained from Householder reduction to a diagonal matrix. This code is
     * a minor modification of that from Numerical Recipes in C.
     *
     * @return  DOCUMENT ME!
     */
    protected boolean reduceQLAlgorithm() {
        int iMaxIter = 32;

        for (int i0 = 0; i0 < m_iSize; i0++) {
            int i1;

            for (i1 = 0; i1 < iMaxIter; i1++) {
                int i2;

                for (i2 = i0; i2 <= (m_iSize - 2); i2++) {
                    double fTmp = Math.abs(m_afDiag[i2]) + Math.abs(m_afDiag[i2 + 1]);

                    if ((Math.abs(m_afSubd[i2]) + fTmp) == fTmp) {
                        break;
                    }
                }

                if (i2 == i0) {
                    break;
                }

                double fG = (m_afDiag[i0 + 1] - m_afDiag[i0]) / (2.0 * m_afSubd[i0]);
                double fR = Math.sqrt((fG * fG) + 1.0);

                if (fG < 0.0) {
                    fG = m_afDiag[i2] - m_afDiag[i0] + (m_afSubd[i0] / (fG - fR));
                } else {
                    fG = m_afDiag[i2] - m_afDiag[i0] + (m_afSubd[i0] / (fG + fR));
                }

                double fSin = 1.0, fCos = 1.0, fP = 0.0;

                for (int i3 = i2 - 1; i3 >= i0; i3--) {
                    double fF = fSin * m_afSubd[i3];
                    double fB = fCos * m_afSubd[i3];

                    if (Math.abs(fF) >= Math.abs(fG)) {
                        fCos = fG / fF;
                        fR = Math.sqrt((fCos * fCos) + 1.0);
                        m_afSubd[i3 + 1] = fF * fR;
                        fSin = 1.0 / fR;
                        fCos *= fSin;
                    } else {
                        fSin = fF / fG;
                        fR = Math.sqrt((fSin * fSin) + 1.0);
                        m_afSubd[i3 + 1] = fG * fR;
                        fCos = 1.0 / fR;
                        fSin *= fCos;
                    }

                    fG = m_afDiag[i3 + 1] - fP;
                    fR = ((m_afDiag[i3] - fG) * fSin) + (2.0 * fB * fCos);
                    fP = fSin * fR;
                    m_afDiag[i3 + 1] = fG + fP;
                    fG = (fCos * fR) - fB;

                    for (int i4 = 0; i4 < m_iSize; i4++) {
                        fF = m_aafMat[i4][i3 + 1];
                        m_aafMat[i4][i3 + 1] = (fSin * m_aafMat[i4][i3]) + (fCos * fF);
                        m_aafMat[i4][i3] = (fCos * m_aafMat[i4][i3]) - (fSin * fF);
                    }
                }

                m_afDiag[i0] -= fP;
                m_afSubd[i0] = fG;
                m_afSubd[i2] = 0.0;
            }

            if (i1 == iMaxIter) {
                return false;
            }
        }

        return true;
    }
}
