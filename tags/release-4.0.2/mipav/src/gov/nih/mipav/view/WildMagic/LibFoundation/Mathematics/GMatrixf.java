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

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

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
public class GMatrixf
{
    /** Construct a general matrix of size 0,0.  */
    public GMatrixf ()
    {
        SetSize(0,0);
    }

    /** Construct a general matrix of size iRows,iCols.
     * @param iRows, number of rows in the matrix.
     * @param iCols, number of columns in the matrix.
     */
    public GMatrixf (int iRows, int iCols)
    {
        SetSize(iRows,iCols);
    }

    /** Construct a general matrix of size iRows,iCols. Copy the
     * afEntry array into the new matrix.
     * @param iRows, number of rows in the matrix.
     * @param iCols, number of columns in the matrix.
     * @param afEntry, matrix values.
     */
    public GMatrixf (int iRows, int iCols, final float[] afEntry)
    {
        SetMatrix(iRows,iCols,afEntry);
    }

    /** Construct a general matrix of size iRows,iCols. Copy the
     * afEntry array into the new matrix.
     * @param iRows, number of rows in the matrix.
     * @param iCols, number of columns in the matrix.
     * @param afEntry, matrix values.
     */
    public GMatrixf (int iRows, int iCols, final float[][] aafMatrix)
    {
        SetMatrix(iRows,iCols,aafMatrix);
    }

    /** Construct a general matrix that is the copy of the input matrix.
     * @param rkM, input matrix.
     */
    public GMatrixf (GMatrixf rkM)
    {
        SetMatrix(rkM.GetRows(),rkM.GetColumns(), rkM.GetData());
    }

    /** Deallocate the matrix. */
    public void dispose ()
    {
        Deallocate();
    }

    /** Set the matrix size.
     * @param iRows, number of rows in the matrix.
     * @param iCols, number of columns in the matrix.
     */
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

    /** Get the number of rows in the matrix.
     * @return the number of rows in the matrix.
     */
    public int GetRows ()
    {
        return m_iRows;
    }

    /** Get the number of columns in the matrix.
     * @return the number of rows in the matrix.
     */
    public int GetColumns ()
    {
        return m_iCols;
    }

    /** Get the size of the matrix rows*columns.
     * @return the size of the matrix rows*columns.
     */
    public int GetQuantity ()
    {
        return m_iQuantity;
    }

    /** Set the value of the matrix at the location iRow, iCol.
     * @param iRow, row position.
     * @param iCol, column position.
     * @param fValue, new value.
     */
    public void Set( int iRow, int iCol, float fValue )
    {
        assert(0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols);
        m_afData[iRow* m_iCols + iCol] = fValue;
    }

    /** Get the value of the matrix at the location iRow, iCol.
     * @param iRow, row position.
     * @param iCol, column position.
     * @return the matrix value  at the location iRow, iCol.
     */
    public float Get( int iRow, int iCol )
    {
        assert(0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols);
        return m_afData[iRow* m_iCols + iCol];
    }

    /** Get the matrix values in float[] format.
     * @return the matrix values.
     */
    public float[] GetData(  )
    {
        return m_afData;
    }

    /** 
     * Swap the iRow0 and iRow1 in the matrix.
     * @param iRow0, first row to swap.
     * @param iRow1, second row to swap.
     */
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

    /** Set the value of the matrix for the iRow th row.
     * @param iRow, row to set.
     * @param rkV general vector containing the new values.
     */
    public void SetRow (int iRow, final GVectorf rkV)
    {
        assert((0 <= iRow && iRow < m_iRows) && (rkV.GetSize() == m_iCols));
        for (int iCol = 0; iCol < m_iCols; iCol++)
        {
            m_afData[iRow * m_iCols + iCol] = rkV.Get(iCol);
        }
    }

    /** Get the value of the matrix for the iRow th row.
     * @param iRow, row to get.
     * @return a general vector containing the row values.
     */
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

    /** Set the value of the matrix for the iCol th column.
     * @param iCol, column to set.
     * @param rkV general vector containing the new values.
     */
    public void SetColumn (int iCol, final GVectorf rkV)
    {
        assert((0 <= iCol && iCol < m_iCols) && (rkV.GetSize() == m_iRows));
        for (int iRow = 0; iRow < m_iRows; iRow++)
        {
            m_afData[iRow* m_iCols + iCol] = rkV.Get(iRow);
        }
    }

    /** Get the value of the matrix for the iCol th column.
     * @param iCol, column to get.
     * @return a general vector containing the column values.
     */
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

    /** Construct a general matrix of size iRows,iCols. Copy the
     * afEntry array into the new matrix.
     * @param iRows, number of rows in the matrix.
     * @param iCols, number of columns in the matrix.
     * @param afEntry, matrix values.
     */
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

    /** Support for allocation and deallocation.  The allocation call requires
     * m_iRows, m_iCols, and m_iQuantity to have already been correctly
     * initialized.
     * @param bSetToZero, when true set the matrix to all 0-values.
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

    /** Delete the matrix data. */
    private void Deallocate ()
    {
        m_afData = null;
    }


    /** rows, columns, and size: */
    private int m_iRows, m_iCols, m_iQuantity;

    /** the matrix is stored in row-major form as a 1-dimensional array */
    private float[] m_afData = null;
};
