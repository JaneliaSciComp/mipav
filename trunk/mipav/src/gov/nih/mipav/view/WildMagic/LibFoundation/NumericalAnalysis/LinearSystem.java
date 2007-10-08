// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.  The license is available for reading at
// either of the locations:
//     http://www.gnu.org/copyleft/lgpl.html
//     http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibFoundation.NumericalAnalysis;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

public class LinearSystem
{
    // construction
    public LinearSystem () {}

    // 2x2 and 3x3 systems (avoids overhead of Gaussian elimination)
    public static boolean Solve2 (final float aafA[][], final float afB[], float afX[])
    {
        float fDet = aafA[0][0]*aafA[1][1]-aafA[0][1]*aafA[1][0];
        if (Math.abs(fDet) < Mathf.ZERO_TOLERANCE)
        {
            return false;
        }

        float fInvDet = (1.0f)/fDet;
        afX[0] = (aafA[1][1]*afB[0]-aafA[0][1]*afB[1])*fInvDet;
        afX[1] = (aafA[0][0]*afB[1]-aafA[1][0]*afB[0])*fInvDet;
        return true;
    }

    public static boolean Solve3 (final float aafA[][], final float afB[], float afX[])
    {
        float[][] aafAInv = new float[3][3];
        aafAInv[0][0] = aafA[1][1]*aafA[2][2]-aafA[1][2]*aafA[2][1];
        aafAInv[0][1] = aafA[0][2]*aafA[2][1]-aafA[0][1]*aafA[2][2];
        aafAInv[0][2] = aafA[0][1]*aafA[1][2]-aafA[0][2]*aafA[1][1];
        aafAInv[1][0] = aafA[1][2]*aafA[2][0]-aafA[1][0]*aafA[2][2];
        aafAInv[1][1] = aafA[0][0]*aafA[2][2]-aafA[0][2]*aafA[2][0];
        aafAInv[1][2] = aafA[0][2]*aafA[1][0]-aafA[0][0]*aafA[1][2];
        aafAInv[2][0] = aafA[1][0]*aafA[2][1]-aafA[1][1]*aafA[2][0];
        aafAInv[2][1] = aafA[0][1]*aafA[2][0]-aafA[0][0]*aafA[2][1];
        aafAInv[2][2] = aafA[0][0]*aafA[1][1]-aafA[0][1]*aafA[1][0];
        float fDet = aafA[0][0]*aafAInv[0][0] + aafA[0][1]*aafAInv[1][0] +
            aafA[0][2]*aafAInv[2][0];

        if (Math.abs(fDet) < Mathf.ZERO_TOLERANCE)
        {
            return false;
        }

        float fInvDet = ((float)1.0)/fDet;
        for (int iRow = 0; iRow < 3; iRow++)
        {
            for (int iCol = 0; iCol < 3; iCol++)
            {
                aafAInv[iRow][iCol] *= fInvDet;
            }
        }

        afX[0] = aafAInv[0][0]*afB[0]+aafAInv[0][1]*afB[1]+aafAInv[0][2]*afB[2];
        afX[1] = aafAInv[1][0]*afB[0]+aafAInv[1][1]*afB[1]+aafAInv[1][2]*afB[2];
        afX[2] = aafAInv[2][0]*afB[0]+aafAInv[2][1]*afB[1]+aafAInv[2][2]*afB[2];
        return true;
    }


    // Input:
    //     A[iSize][iSize], entries are A[row][col]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     InvA[iSize][iSize], inverse matrix
    public static boolean Inverse (final GMatrixf rkA, GMatrixf rkInvA)
    {
        // computations are performed in-place
        assert(rkA.GetRows() == rkA.GetColumns());
        int iSize = rkInvA.GetRows();
        rkInvA = new GMatrixf(rkA);

        int[] aiColIndex = new int[iSize];
        int[] aiRowIndex = new int[iSize];
        boolean[] abPivoted = new boolean[iSize];
        for ( int i = 0; i < iSize; i++ )
        {
            abPivoted[i] = false;
        }
        int i1, i2, iRow = 0, iCol = 0;
        float fSave;

        // elimination by full pivoting
        for (int i0 = 0; i0 < iSize; i0++)
        {
            // search matrix (excluding pivoted rows) for maximum absolute entry
            float fMax = 0.0f;
            for (i1 = 0; i1 < iSize; i1++)
            {
                if (!abPivoted[i1])
                {
                    for (i2 = 0; i2 < iSize; i2++)
                    {
                        if (!abPivoted[i2])
                        {
                            float fAbs = Math.abs(rkInvA.Get(i1,i2));
                            if (fAbs > fMax)
                            {
                                fMax = fAbs;
                                iRow = i1;
                                iCol = i2;
                            }
                        }
                    }
                }
            }

            if (fMax == (float)0.0)
            {
                // matrix is not invertible
                aiColIndex = null;
                aiRowIndex = null;
                abPivoted = null;
                return false;
            }

            abPivoted[iCol] = true;

            // swap rows so that A[iCol][iCol] contains the pivot entry
            if (iRow != iCol)
            {
                rkInvA.SwapRows(iRow,iCol);
            }

            // keep track of the permutations of the rows
            aiRowIndex[i0] = iRow;
            aiColIndex[i0] = iCol;

            // scale the row so that the pivot entry is 1
            float fInv = ((float)1.0)/rkInvA.Get(iCol,iCol);
            rkInvA.Set(iCol,iCol, (float)1.0);
            for (i2 = 0; i2 < iSize; i2++)
            {
                rkInvA.Set(iCol,i2, rkInvA.Get(iCol,i2)*fInv);
            }

            // zero out the pivot column locations in the other rows
            for (i1 = 0; i1 < iSize; i1++)
            {
                if (i1 != iCol)
                {
                    fSave = rkInvA.Get(i1,iCol);
                    rkInvA.Set(i1,iCol, (float)0.0);
                    for (i2 = 0; i2 < iSize; i2++)
                    {
                        rkInvA.Set(i1,i2, rkInvA.Get(i1,i2) - rkInvA.Get(iCol,i2)*fSave);
                    }
                }
            }
        }

        // reorder rows so that A[][] stores the inverse of the original matrix
        for (i1 = iSize-1; i1 >= 0; i1--)
        {
            if (aiRowIndex[i1] != aiColIndex[i1])
            {
                for (i2 = 0; i2 < iSize; i2++)
                {
                    fSave = rkInvA.Get(i2,aiRowIndex[i1]);
                    rkInvA.Set(i2,aiRowIndex[i1], rkInvA.Get(i2,aiColIndex[i1]));
                    rkInvA.Set(i2,aiColIndex[i1], fSave);
                }
            }
        }

        aiColIndex = null;
        aiRowIndex = null;
        abPivoted = null;
        return true;
    }


    // Input:
    //     A[iSize][iSize] coefficient matrix, entries are A[row][col]
    //     B[iSize] vector, entries are B[row]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     X[iSize] is solution X to AX = B
    public static boolean Solve (final GMatrixf rkA, final float[] afB, float[] afX)
    {
        // computations are performed in-place
        int iSize = rkA.GetColumns();
        GMatrixf kInvA = new GMatrixf(rkA);
        for ( int i = 0; i < iSize; i++ )
        {
            afX[i] = afB[i];
        }

        int[] aiColIndex = new int[iSize];
        int[] aiRowIndex = new int[iSize];
        boolean[] abPivoted = new boolean[iSize];
        for ( int i = 0; i < iSize; i++ )
        {
            abPivoted[i] = false;
        }

        int i1, i2, iRow = 0, iCol = 0;
        float fSave;

        // elimination by full pivoting
        for (int i0 = 0; i0 < iSize; i0++)
        {
            // search matrix (excluding pivoted rows) for maximum absolute entry
            float fMax = 0.0f;
            for (i1 = 0; i1 < iSize; i1++)
            {
                if (!abPivoted[i1])
                {
                    for (i2 = 0; i2 < iSize; i2++)
                    {
                        if (!abPivoted[i2])
                        {
                            float fAbs = Math.abs(kInvA.Get(i1,i2));
                            if (fAbs > fMax)
                            {
                                fMax = fAbs;
                                iRow = i1;
                                iCol = i2;
                            }
                        }
                    }
                }
            }

            if (fMax == (float)0.0)
            {
                // matrix is not invertible
                aiColIndex = null;
                aiRowIndex = null;
                abPivoted = null;
                return false;
            }

            abPivoted[iCol] = true;

            // swap rows so that A[iCol][iCol] contains the pivot entry
            if (iRow != iCol)
            {
                kInvA.SwapRows(iRow,iCol);

                fSave = afX[iRow];
                afX[iRow] = afX[iCol];
                afX[iCol] = fSave;
            }

            // keep track of the permutations of the rows
            aiRowIndex[i0] = iRow;
            aiColIndex[i0] = iCol;

            // scale the row so that the pivot entry is 1
            float fInv = ((float)1.0)/kInvA.Get(iCol,iCol);
            kInvA.Set(iCol,iCol, (float)1.0);
            for (i2 = 0; i2 < iSize; i2++)
            {
                kInvA.Set(iCol,i2, kInvA.Get(iCol,i2)*fInv);
            }
            afX[iCol] *= fInv;

            // zero out the pivot column locations in the other rows
            for (i1 = 0; i1 < iSize; i1++)
            {
                if (i1 != iCol)
                {
                    fSave = kInvA.Get(i1,iCol);
                    kInvA.Set(i1,iCol, (float)0.0);
                    for (i2 = 0; i2 < iSize; i2++)
                        kInvA.Set(i1,i2, kInvA.Get(i1,i2) - kInvA.Get(iCol,i2)*fSave);
                    afX[i1] -= afX[iCol]*fSave;
                }
            }
        }

        // reorder rows so that A[][] stores the inverse of the original matrix
        for (i1 = iSize-1; i1 >= 0; i1--)
        {
            if (aiRowIndex[i1] != aiColIndex[i1])
            {
                for (i2 = 0; i2 < iSize; i2++)
                {
                    fSave = kInvA.Get(i2,aiRowIndex[i1]);
                    kInvA.Set(i2,aiRowIndex[i1], kInvA.Get(i2,aiColIndex[i1]));
                    kInvA.Set(i2,aiColIndex[i1], fSave);
                }
            }
        }

        aiColIndex = null;
        aiRowIndex = null;
        abPivoted = null;
        return true;
    }


    // Input:
    //     Matrix is tridiagonal.
    //     Lower diagonal A[iSize-1]
    //     Main  diagonal B[iSize]
    //     Upper diagonal C[iSize-1]
    //     Right-hand side R[iSize]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     U[iSize] is solution
    //     public boolean SolveTri (int iSize, float* afA, float* afB, float* afC, float* afR,
    //         float* afU);

    // Input:
    //     Matrix is tridiagonal.
    //     Lower diagonal is finalant, A
    //     Main  diagonal is finalant, B
    //     Upper diagonal is finalant, C
    //     Right-hand side Rr[iSize]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     U[iSize] is solution
    //     public boolean SolveFinalTri (int iSize, float fA, float fB, float fC, float* afR,
    //         float* afU);

    // Solution using the conjugate gradient method.
    // Input:
    //    A[iSize][iSize] symmetrix matrix, entries are A[row][col]
    //    B[iSize] vector, entries are B[row]
    // Output:
    //    X[iSize] is the solution x to Ax = B
    //     public boolean SolveSymmetricCG (final GMatrixf rkA, final float* afB,
    //         float* afX);

    // Conjugate gradient method for sparse, symmetric matrices.
    // Input:
    //    The nonzero entries of the symmetrix matrix A are stored in a map
    //    whose keys are pairs (i,j) and whose values are real numbers.  The
    //    pair (i,j) is the location of the value in the array.  Only one of
    //    (i,j) and (j,i) should be stored since A is symmetric.  The code
    //    assumes this is how you set up A.  The column vector B is stored as
    //    an array of contiguous values.
    // Output:
    //    X[iSize] is the solution x to Ax = B
    //     typedef std::map<std::pair<int,int>,float> SparseMatrix;
    //     public boolean SolveSymmetricCG (int iSize, final SparseMatrix& rkA,
    //         final float* afB, float* afX);

    // solve banded matrix systems
    // Input:
    //     A, a banded matrix
    //     B[iSize] vector, entries are B[row]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     X[iSize] is solution X to AX = B
    //     public boolean SolveBanded (final BandedMatrix<float>& rkA, final float* afB,
    //         float* afX);

    // invert a banded matrix
    // Input:
    //     A, a banded matrix
    // Output:
    //     return value is TRUE if the inverse exists, FALSE otherwise
    //     InvA, the inverse of A
    //     public boolean Invert (final BandedMatrix<float>& rkA, GMatrixf rkInvA);

        // support for the conjugate gradient method for standard arrays
//     private float Dot (int iSize, final float* afU, final float* afV);
//     private void Multiply (final GMatrixf rkA, final float* afX, float* afProd);
//     private void UpdateX (int iSize, float* afX, float fAlpha, final float* afP);
//     private void UpdateR (int iSize, float* afR, float fAlpha, final float* afW);
//     private void UpdateP (int iSize, float* afP, float fBeta, final float* afR);

    // support for the conjugate gradient method for sparse arrays
    //     private void Multiply (int iSize, final SparseMatrix& rkA, final float* afX,
    //         float* afProd);

    // support for banded matrices
    //     private boolean ForwardEliminate (int iReduceRow, BandedMatrix<float>& rkA,
    //         float* afB);
    //     private boolean ForwardEliminate (int iReduceRow, BandedMatrix<float>& rkA,
    //         GMatrixf rkB);
    //     private void BackwardEliminate (int iReduceRow, BandedMatrix<float>& rkA,
    //         GMatrixf rkB);
};
