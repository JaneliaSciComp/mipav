package gov.nih.mipav.view.renderer.surfaceview.brainflattenerview;

import java.util.*;

/**
 * Sparse matrix implemented with a hash map.  All undefined entries are zero.
 */
public class MjSparseMatrix
{
    private static final float ms_fTolerance = 1e-06f;

    // Used to reference a single element in the matrix.
    public static class Index
    {
        public Index (int iRow, int iCol)
        {
            m_iRow = iRow;
            m_iCol = iCol;
        }

        // override for Object method used for hashing
        public int hashCode ()
        {
            return (m_iRow << 16) | m_iCol;
        }

        // override for Object method used for hashing
        public boolean equals (Object kObject)
        {
            Index kIndex = (Index)kObject;
            return m_iRow == kIndex.m_iRow && m_iCol == kIndex.m_iCol;
        }

        public int m_iRow;
        public int m_iCol;
    }

    // Matrix of undefined dimensions.  Initially all elements are zero.
    public MjSparseMatrix()
    {
    }

    // Get the element value at the specified row,col.
    public float getElement (int iRow, int iCol)
    {
        m_kIndexKeyTmp.m_iRow = iRow;
        m_kIndexKeyTmp.m_iCol = iCol;
        Float kValue = (Float)m_kMap.get(m_kIndexKeyTmp);
        return ( null == kValue ) ? 0.0f : kValue.floatValue();
    }

    // Set the element value at the specified row,col.
    public void setElement (int iRow, int iCol, float fValue)
    {
        m_kMap.put(new Index(iRow,iCol),new Float(fValue));
    }

    // Return iterator to non-zero elements in the matrix.
    // Each entry in the iterator is of Map.Entry type where
    // the key is of Index type and the value is of Float type.
    public Iterator iterator()
    {
        return m_kMap.entrySet().iterator();
    }

    // return number of non-zero entries in the matrix
    public int size()
    {
        return m_kMap.size();
    }

    // conjugate gradient methods
    private static float dot (int iSize, float [] afU, float [] afV)
    {
        float fDot = 0.0f;
        for (int i = 0; i < iSize; i++)
            fDot += afU[i]*afV[i];
        return fDot;
    }
    private void multiply (int iSize, float [] afX, float [] afProd)
    {
        for (int i=0; i < iSize; i++)
            afProd[i] = 0.0f;

        Iterator kIter = m_kMap.entrySet().iterator();
        while ( kIter.hasNext() )
        {
            Map.Entry kEntry = (Map.Entry)kIter.next();
            Index kIndex = (Index)kEntry.getKey();
            Float kValue = (Float)kEntry.getValue();
            int i = kIndex.m_iRow;
            int j = kIndex.m_iCol;
            float fValue = kValue.floatValue();
            afProd[i] += fValue*afX[j];
            if ( i != j )
                afProd[j] += fValue*afX[i];
        }
    }
    private static void updateX (int iSize, float [] afX, float fAlpha,
                                 float [] afP)
    {
        for (int i = 0; i < iSize; i++)
            afX[i] += fAlpha*afP[i];
    }
    private static void updateR (int iSize, float [] afR, float fAlpha,
                                 float [] afW)
    {
        for (int i = 0; i < iSize; i++)
            afR[i] -= fAlpha*afW[i];
    }
    private static void updateP (int iSize, float [] afP, float fBeta,
                                 float [] afR)
    {
        for (int i = 0; i < iSize; i++)
            afP[i] = afR[i] + fBeta*afP[i];
    }

    // Conjugate gradient method for sparse, symmetric matrices.
    // Input:
    //    The nonzero entries of the symmetrix matrix A are stored in a map
    //    whose keys are pairs (i,j) and whose values are real numbers.  The
    //    pair (i,j) is the location of the value in the array.  Only one of
    //    (i,j) and (j,i) should be stored since A is symmetric.  The code
    //    assumes this is how you set up A.  The column vector B is stored as
    //    an array of contiguous valuess.  If B itself is sparse, another
    //    solver can be implemented which uses a map to store B.
    // Output:
    //    B[iSize] is the solution x to Ax = B
    boolean solveSymmetricCG (int iSize, float [] afB, float [] afX)
    {
        // based on the algorithm in "Matrix Computations"
        // by Golum and Van Loan
        float [] afR = new float [iSize];
        float [] afP = new float [iSize];
        float [] afW = new float [iSize];

        // first iteration
        for (int i=0; i < iSize; i++)
        {
            afX[i] = 0.0f;
            afR[i] = afB[i];
            afP[i] = afR[i];
        }
        float fRho0 = dot(iSize,afR,afR);
        multiply(iSize,afP,afW);
        float fAlpha = fRho0/dot(iSize,afP,afW);
        updateX(iSize,afX,fAlpha,afP);
        updateR(iSize,afR,fAlpha,afW);
        float fRho1 = dot(iSize,afR,afR);

        // remaining iterations
        final int iMax = 1024;
        int i;
        for (i = 1; i < iMax; i++)
        {
            float fRoot0 = (float)Math.sqrt(fRho1);
            float fNorm = dot(iSize,afB,afB);
            float fRoot1 = (float)Math.sqrt(fNorm);
            if ( fRoot0 <= ms_fTolerance*fRoot1 )
                break;

            float fBeta = fRho1/fRho0;
            updateP(iSize,afP,fBeta,afR);
            multiply(iSize,afP,afW);
            fAlpha = fRho1/dot(iSize,afP,afW);
            updateX(iSize,afX,fAlpha,afP);
            updateR(iSize,afR,fAlpha,afW);
            fRho0 = fRho1;
            fRho1 = dot(iSize,afR,afR);
        }

        return i < iMax;
    }


    // Mapping of Index(row,col)
    private HashMap m_kMap = new HashMap();

    // Instantiate once instead of each time a value key (Index) is needed.
    private Index m_kIndexKeyTmp = new Index(-1,-1);
}
