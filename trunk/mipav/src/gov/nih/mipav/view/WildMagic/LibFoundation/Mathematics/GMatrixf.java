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

/** Matrix operations are applied on the left.  For example, given a matrix M
 *and a vector V, matrix-times-vector is M*V.  That is, V is treated as a
 * column vector.  Some graphics APIs use V*M where V is treated as a row
 * vector.  In this context the "M" matrix is really a transpose of the M as
 * represented in Wild Magic.  Similarly, to apply two matrix operations M0
 * and M1, in that order, you compute M1*M0 so that the transform of a vector
 * is (M1*M0)*V = M1*(M0*V).  Some graphics APIs use M0*M1, but again these
 * matrices are the transpose of those as represented in Wild Magic.  You
 * must therefore be careful about how you interface the transformation code
 * with graphics APIS.
 *
 * Matrices are stored in row-major order, matrix[row][col].
 */

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

public class GMatrixf
{
    // construction and destruction
    public GMatrixf ()
    {
        SetSize(0,0);
    }

    public GMatrixf (int iRows, int iCols)
    {
        SetSize(iRows,iCols);
    }

    public GMatrixf (int iRows, int iCols, final float[] afEntry)
    {
        SetMatrix(iRows,iCols,afEntry);
    }

    public GMatrixf (int iRows, int iCols, final float[][] aafMatrix)
    {
        SetMatrix(iRows,iCols,aafMatrix);
    }

    public GMatrixf (GMatrixf rkM)
    {
        SetMatrix(rkM.GetRows(),rkM.GetColumns(), rkM.GetData());
    }

    public void finalize ()
    {
        Deallocate();
    }

    // member access
    public void SetSize (int iRows, int iCols)
    {
        Deallocate();
        if (iRows > 0 && iCols > 0)
        {
            m_iRows = iRows;
            m_iCols = iCols;
            m_iQuantity = m_iRows*m_iCols;
            Allocate(true);
        }
        else
        {
            m_iRows = 0;
            m_iCols = 0;
            m_iQuantity = 0;
            m_afData = null;
        }
    }

    public int GetRows ()
    {
        return m_iRows;
    }

    public int GetColumns ()
    {
        return m_iCols;
    }

    public int GetQuantity ()
    {
        return m_iQuantity;
    }

    public void Set( int iRow, int iCol, float fValue )
    {
        assert(0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols);
        m_afData[iRow* m_iCols + iCol] = fValue;
    }

    public float Get( int iRow, int iCol )
    {
        assert(0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols);
        return m_afData[iRow* m_iCols + iCol];
    }

    public float[] GetData(  )
    {
        return m_afData;
    }

//     operator final float[] () final;
//     operator float[] ();
//     final float[] operator[] (int iRow) final;
//     float[] operator[] (int iRow);
    public void SwapRows (int iRow0, int iRow1)
    {
        assert(0 <= iRow0 && iRow0 < m_iRows && 0 <= iRow1 && iRow1 < m_iRows);
        for ( int i = 0; i < m_iCols; i++ )
        {
            float fSave = m_afData[iRow0 * m_iCols + i];
            m_afData[iRow0 * m_iCols + i] = m_afData[iRow1 * m_iCols + i];
            m_afData[iRow1 * m_iCols + i] = fSave;
        }
    }

//     Real operator() (int iRow, int iCol) final;
//     Real& operator() (int iRow, int iCol);
    public void SetRow (int iRow, final GVectorf rkV)
    {
        assert((0 <= iRow && iRow < m_iRows) && (rkV.GetSize() == m_iCols));
        for (int iCol = 0; iCol < m_iCols; iCol++)
        {
            m_afData[iRow * m_iCols + iCol] = rkV.Get(iCol);
        }
    }

    public GVectorf GetRow (int iRow)
    {
        assert(0 <= iRow && iRow < m_iRows);
        GVectorf kV = new GVectorf(m_iCols);
        for (int iCol = 0; iCol < m_iCols; iCol++)
        {
            kV.Set(iCol, m_afData[iRow* m_iCols + iCol] );
        }
        return kV;
    }

    public void SetColumn (int iCol, final GVectorf rkV)
    {
        assert((0 <= iCol && iCol < m_iCols) && (rkV.GetSize() == m_iRows));
        for (int iRow = 0; iRow < m_iRows; iRow++)
        {
            m_afData[iRow* m_iCols + iCol] = rkV.Get(iRow);
        }
    }

    public GVectorf GetColumn (int iCol)
    {
        assert(0 <= iCol && iCol < m_iCols);
        GVectorf kV = new GVectorf(m_iRows);
        for (int iRow = 0; iRow < m_iRows; iRow++)
        {
            kV.Set(iRow, m_afData[iRow* m_iCols + iCol]);
        }
        return kV;
    }

    public void SetMatrix (int iRows, int iCols, final float[] afEntry)
    {
        Deallocate();
        if (iRows > 0 && iCols > 0)
        {
            m_iRows = iRows;
            m_iCols = iCols;
            m_iQuantity = m_iRows*m_iCols;
            Allocate(false);
            for ( int i = 0; i < m_iQuantity; i++ )
            {
                m_afData[i] = afEntry[i];
            }
        }
        else
        {
            m_iRows = 0;
            m_iCols = 0;
            m_iQuantity = 0;
            m_afData = null;
        }
    }

    public void SetMatrix (int iRows, int iCols, final float[][] aafMatrix)
    {
        Deallocate();
        if (iRows > 0 && iCols > 0)
        {
            m_iRows = iRows;
            m_iCols = iCols;
            m_iQuantity = m_iRows*m_iCols;
            Allocate(false);
            for (int iRow = 0; iRow < m_iRows; iRow++)
            {
                for (int iCol = 0; iCol < m_iCols; iCol++)
                {
                    m_afData[iRow* m_iCols + iCol] = aafMatrix[iRow][iCol];
                }
            }
        }
        else
        {
            m_iRows = 0;
            m_iCols = 0;
            m_iQuantity = 0;
            m_afData = null;
        }
    }

//     public void GetColumnMajor (float[] afCMajor)
//     {
//         for (int iRow = 0, i = 0; iRow < m_iRows; iRow++)
//         {
//             for (int iCol = 0; iCol < m_iCols; iCol++)
//             {
//                 afCMajor[i++] = m_aafEntry[iCol][iRow];
//             }
//         }
//     }


    // assignment
//     GMatrixf& operator= (final GMatrixf& rkM);

    // comparison
//     bool operator== (final GMatrixf& rkM) final;
//     bool operator!= (final GMatrixf& rkM) final;
//     bool operator<  (final GMatrixf& rkM) final;
//     bool operator<= (final GMatrixf& rkM) final;
//     bool operator>  (final GMatrixf& rkM) final;
//     bool operator>= (final GMatrixf& rkM) final;

    // arithmetic operations
//     GMatrixf operator+ (final GMatrixf& rkM) final;
//     GMatrixf operator- (final GMatrixf& rkM) final;
//     GMatrixf operator* (final GMatrixf& rkM) final;
//     GMatrixf operator* (Real fScalar) final;
//     GMatrixf operator/ (Real fScalar) final;
//     GMatrixf operator- () final;

    // arithmetic updates
//     GMatrixf& operator+= (final GMatrixf& rkM);
//     GMatrixf& operator-= (final GMatrixf& rkM);
//     GMatrixf& operator*= (Real fScalar);
//     GMatrixf& operator/= (Real fScalar);

    // matrix products
//     public GMatrixf Transpose () final;  // M^T
//     public GMatrixf TransposeTimes (final GMatrixf& rkM) final;  // this^T * M
//     public GMatrixf TimesTranspose (final GMatrixf& rkM) final;  // this * M^T

    // matrix-vector operations
//     public GVector<Real> operator* (final GVector<Real>& rkV) final;  // M * v
//     public float QForm (final GVector<Real>& rkU, final GVector<Real>& rkV)
//         final;  // u^T*M*v

    /** Inversion.  The matrix must be square.  The function returns true
     * whenever the matrix is square and invertible.
     */
//     public boolean GetInverse (GMatrixf<Real>& rkInverse) final;

    /** Support for allocation and deallocation.  The allocation call requires
     * m_iRows, m_iCols, and m_iQuantity to have already been correctly
     * initialized.
     */
    private void Allocate (boolean bSetToZero)
    {
        m_afData = new float[m_iQuantity];
        if ( bSetToZero )
        {
            for ( int i = 0; i < m_iQuantity; i++ )
            {
                m_afData[i] = 0.0f;
            }
        }
    }

    private void Deallocate ()
    {
        m_afData = null;
    }


    /** support for comparisons */
//     private int CompareArrays (final GMatrixf& rkM) final;

    /** rows, columns, and size: */
    private int m_iRows, m_iCols, m_iQuantity;

    /** the matrix is stored in row-major form as a 1-dimensional array */
    private float[] m_afData = null;
};

// c * M
// template <class Real>
// GMatrixf<Real> operator* (Real fScalar, final GMatrixf<Real>& rkM);

// // v^T * M
// template <class Real>
// GVector<Real> operator* (final GVector<Real>& rkV, final GMatrixf<Real>& rkM);
