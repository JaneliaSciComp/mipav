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
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

/** Matrix operations are applied on the left.  For example, given a matrix M
 * and a vector V, matrix-times-vector is M*V.  That is, V is treated as a
 * column vector.  Some graphics APIs use V*M where V is treated as a row
 * vector.  In this context the "M" matrix is really a transpose of the M as
 * represented in Wild Magic.  Similarly, to apply two matrix operations M0
 * and M1, in that order, you compute M1*M0 so that the transform of a vector
 * is (M1*M0)*V = M1*(M0*V).  Some graphics APIs use M0*M1, but again these
 * matrices are the transpose of those as represented in Wild Magic.  You
 * must therefore be careful about how you interface the transformation code
 * with graphics APIS.
 *
 * For memory organization it might seem natural to chose Real[N][N] for the
 * matrix storage, but this can be a problem on a platform/console that
 * chooses to store the data in column-major rather than row-major format.
 * To avoid potential portability problems, the matrix is stored as Real[N*N]
 * and organized in row-major order.  That is, the entry of the matrix in row
 * r (0 <= r < N) and column c (0 <= c < N) is stored at index i = c+N*r
 * (0 <= i < N*N).
 */
public class Matrix4f
{
    /** Zero matrix: */
    public static final Matrix4f ZERO =
        new Matrix4f(
                     0.0f,0.0f,0.0f,0.0f,
                     0.0f,0.0f,0.0f,0.0f,
                     0.0f,0.0f,0.0f,0.0f,
                     0.0f,0.0f,0.0f,0.0f);

    /** Identity matrix: */
    public static final Matrix4f IDENTITY = 
        new Matrix4f(
                     1.0f,0.0f,0.0f,0.0f,
                     0.0f,1.0f,0.0f,0.0f,
                     0.0f,0.0f,1.0f,0.0f,
                     0.0f,0.0f,0.0f,1.0f);


    /** Create the zero matrix.
     */
    public Matrix4f ()
    {
        MakeZero();
    }

    /** If bZero is true, create the zero matrix.  Otherwise, create the
     * identity matrix.
     * @param bZero, when true create the zero matrix, othersize create identity.
     */
    public Matrix4f (boolean bZero)
    {
        if (bZero)
        {
            MakeZero();
        }
        else
        {
            MakeIdentity();
        }
    }
    

    /** copy constructor
     * @param rkM, matrix to copy
     */
    public Matrix4f (Matrix4f rkM)
    {
        m_afEntry[ 0] = rkM.m_afEntry[ 0];
        m_afEntry[ 1] = rkM.m_afEntry[ 1];
        m_afEntry[ 2] = rkM.m_afEntry[ 2];
        m_afEntry[ 3] = rkM.m_afEntry[ 3];
        m_afEntry[ 4] = rkM.m_afEntry[ 4];
        m_afEntry[ 5] = rkM.m_afEntry[ 5];
        m_afEntry[ 6] = rkM.m_afEntry[ 6];
        m_afEntry[ 7] = rkM.m_afEntry[ 7];
        m_afEntry[ 8] = rkM.m_afEntry[ 8];
        m_afEntry[ 9] = rkM.m_afEntry[ 9];
        m_afEntry[10] = rkM.m_afEntry[10];
        m_afEntry[11] = rkM.m_afEntry[11];
        m_afEntry[12] = rkM.m_afEntry[12];
        m_afEntry[13] = rkM.m_afEntry[13];
        m_afEntry[14] = rkM.m_afEntry[14];
        m_afEntry[15] = rkM.m_afEntry[15];
    }


    /** input Mrc is in row r, column c.
     * @param fM00, matrix[0] entry
     * @param fM01, matrix[1] entry
     * @param fM02, matrix[2] entry
     * @param fM03, matrix[3] entry
     * @param fM10, matrix[4] entry
     * @param fM11, matrix[5] entry
     * @param fM12, matrix[6] entry
     * @param fM13, matrix[7] entry
     * @param fM20, matrix[8] entry
     * @param fM21, matrix[9] entry
     * @param fM22, matrix[10] entry
     * @param fM23, matrix[11] entry
     * @param fM30, matrix[12] entry
     * @param fM31, matrix[13] entry
     * @param fM32, matrix[14] entry
     * @param fM33, matrix[15] entry
     */
    public Matrix4f (float fM00, float fM01, float fM02, float fM03,
                     float fM10, float fM11, float fM12, float fM13,
                     float fM20, float fM21, float fM22, float fM23,
                     float fM30, float fM31, float fM32, float fM33)
    {
        m_afEntry[ 0] = fM00;
        m_afEntry[ 1] = fM01;
        m_afEntry[ 2] = fM02;
        m_afEntry[ 3] = fM03;
        m_afEntry[ 4] = fM10;
        m_afEntry[ 5] = fM11;
        m_afEntry[ 6] = fM12;
        m_afEntry[ 7] = fM13;
        m_afEntry[ 8] = fM20;
        m_afEntry[ 9] = fM21;
        m_afEntry[10] = fM22;
        m_afEntry[11] = fM23;
        m_afEntry[12] = fM30;
        m_afEntry[13] = fM31;
        m_afEntry[14] = fM32;
        m_afEntry[15] = fM33;
    }


    /** Create a matrix from an array of numbers.  The input array is
     * interpreted based on the Boolean input as
     *   true:  entry[0..15]={m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,
     *                        m23,m30,m31,m32,m33} [row major]
     * false: entry[0..15]={m00,m10,m20,m30,m01,m11,m21,m31,m02,m12,m22,
     *                        m32,m03,m13,m23,m33} [col major]
     * @param afEntry, array of values to put in matrix
     * @param bRowMajor, when true copy in row major order.
     */
    public Matrix4f (float[] afEntry, boolean bRowMajor)
    {
        if (bRowMajor)
        {
            m_afEntry[ 0] = afEntry[ 0];
            m_afEntry[ 1] = afEntry[ 1];
            m_afEntry[ 2] = afEntry[ 2];
            m_afEntry[ 3] = afEntry[ 3];
            m_afEntry[ 4] = afEntry[ 4];
            m_afEntry[ 5] = afEntry[ 5];
            m_afEntry[ 6] = afEntry[ 6];
            m_afEntry[ 7] = afEntry[ 7];
            m_afEntry[ 8] = afEntry[ 8];
            m_afEntry[ 9] = afEntry[ 9];
            m_afEntry[10] = afEntry[10];
            m_afEntry[11] = afEntry[11];
            m_afEntry[12] = afEntry[12];
            m_afEntry[13] = afEntry[13];
            m_afEntry[14] = afEntry[14];
            m_afEntry[15] = afEntry[15];
        }
        else
        {
            m_afEntry[ 0] = afEntry[ 0];
            m_afEntry[ 1] = afEntry[ 4];
            m_afEntry[ 2] = afEntry[ 8];
            m_afEntry[ 3] = afEntry[12];
            m_afEntry[ 4] = afEntry[ 1];
            m_afEntry[ 5] = afEntry[ 5];
            m_afEntry[ 6] = afEntry[ 9];
            m_afEntry[ 7] = afEntry[13];
            m_afEntry[ 8] = afEntry[ 2];
            m_afEntry[ 9] = afEntry[ 6];
            m_afEntry[10] = afEntry[10];
            m_afEntry[11] = afEntry[14];
            m_afEntry[12] = afEntry[ 3];
            m_afEntry[13] = afEntry[ 7];
            m_afEntry[14] = afEntry[11];
            m_afEntry[15] = afEntry[15];
        }
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        m_afEntry = null;
    }

    /** make this the zero matrix */
    public void MakeZero ()
    {
        m_afEntry[ 0] = (float)0.0;
        m_afEntry[ 1] = (float)0.0;
        m_afEntry[ 2] = (float)0.0;
        m_afEntry[ 3] = (float)0.0;
        m_afEntry[ 4] = (float)0.0;
        m_afEntry[ 5] = (float)0.0;
        m_afEntry[ 6] = (float)0.0;
        m_afEntry[ 7] = (float)0.0;
        m_afEntry[ 8] = (float)0.0;
        m_afEntry[ 9] = (float)0.0;
        m_afEntry[10] = (float)0.0;
        m_afEntry[11] = (float)0.0;
        m_afEntry[12] = (float)0.0;
        m_afEntry[13] = (float)0.0;
        m_afEntry[14] = (float)0.0;
        m_afEntry[15] = (float)0.0;
    }

    /** make this the identity matrix */
    public void MakeIdentity ()
    {
        m_afEntry[ 0] = (float)1.0;
        m_afEntry[ 1] = (float)0.0;
        m_afEntry[ 2] = (float)0.0;
        m_afEntry[ 3] = (float)0.0;
        m_afEntry[ 4] = (float)0.0;
        m_afEntry[ 5] = (float)1.0;
        m_afEntry[ 6] = (float)0.0;
        m_afEntry[ 7] = (float)0.0;
        m_afEntry[ 8] = (float)0.0;
        m_afEntry[ 9] = (float)0.0;
        m_afEntry[10] = (float)1.0;
        m_afEntry[11] = (float)0.0;
        m_afEntry[12] = (float)0.0;
        m_afEntry[13] = (float)0.0;
        m_afEntry[14] = (float)0.0;
        m_afEntry[15] = (float)1.0;
    }

    /** Set member access
     * @param iRow, row to set
     * @param iCol, column to set
     * @param fValue, new value
     */
    public void SetData( int iRow, int iCol, float fValue )
    {
        m_afEntry[iRow*4 + iCol] = fValue;
    }

    /** Get member access
     * @param iRow, row to get
     * @param iCol, column to get
     * @return value at m_afEntry[iRow*4 + iCol];
     */
    public float GetData( int iRow, int iCol )
    {
        return m_afEntry[iRow*4 + iCol];
    }

    /** Set member access
     * @param iRow, row to set
     * @param rkV, new row vector values
     */
    public void SetRow (int iRow, Vector4f rkV)
    {
        int i0 = 4*iRow, i1 = i0+1, i2 = i1+1, i3 = i2+1;
        m_afEntry[i0] = rkV.X();
        m_afEntry[i1] = rkV.Y();
        m_afEntry[i2] = rkV.Z();
        m_afEntry[i3] = rkV.W();
    }

    /** Get member access
     * @param iRow, row to get
     * @return row vector values
     */
    public Vector4f GetRow (int iRow)
    {
        int i0 = 4*iRow, i1 = i0+1, i2 = i1+1, i3 = i2+1;
        return new Vector4f(m_afEntry[i0],m_afEntry[i1],m_afEntry[i2],
                            m_afEntry[i3]);
    }

    /** Set member access
     * @param iCol, column to set
     * @param rkV, new column vector values
     */
    public void SetColumn (int iCol, Vector4f rkV)
    {
        m_afEntry[iCol] = rkV.X();
        m_afEntry[iCol+4] = rkV.Y();
        m_afEntry[iCol+8] = rkV.Z();
        m_afEntry[iCol+12] = rkV.W();
    }

    /** Get member access
     * @param iCol, column to get
     * @return column vector values
     */
    public Vector4f GetColumn (int iCol)
    {
        return new Vector4f(m_afEntry[iCol],m_afEntry[iCol+4],m_afEntry[iCol+8],
                            m_afEntry[iCol+12]);
    }

    /** Get member access. Copies matrix into input array.
     * @param afData, copy matrix into array.
     */
    public void GetData (float[] afData)
    {
        afData[ 0] = m_afEntry[ 0];
        afData[ 1] = m_afEntry[ 1];
        afData[ 2] = m_afEntry[ 2];
        afData[ 3] = m_afEntry[ 3];
        afData[ 4] = m_afEntry[ 4];
        afData[ 5] = m_afEntry[ 5];
        afData[ 6] = m_afEntry[ 6];
        afData[ 7] = m_afEntry[ 7];
        afData[ 8] = m_afEntry[ 8];
        afData[ 9] = m_afEntry[ 9];
        afData[10] = m_afEntry[10];
        afData[11] = m_afEntry[11];
        afData[12] = m_afEntry[12];
        afData[13] = m_afEntry[13];
        afData[14] = m_afEntry[14];
        afData[15] = m_afEntry[15];
    }

    /** Get member access. Copies matrix into input array in Column-Major order.
     * @param afData, copy matrix into array.
     */
    public void GetColumnMajor (float[] afCMajor)
    {
        afCMajor[ 0] = m_afEntry[ 0];
        afCMajor[ 1] = m_afEntry[ 4];
        afCMajor[ 2] = m_afEntry[ 8];
        afCMajor[ 3] = m_afEntry[12];
        afCMajor[ 4] = m_afEntry[ 1];
        afCMajor[ 5] = m_afEntry[ 5];
        afCMajor[ 6] = m_afEntry[ 9];
        afCMajor[ 7] = m_afEntry[13];
        afCMajor[ 8] = m_afEntry[ 2];
        afCMajor[ 9] = m_afEntry[ 6];
        afCMajor[10] = m_afEntry[10];
        afCMajor[11] = m_afEntry[14];
        afCMajor[12] = m_afEntry[ 3];
        afCMajor[13] = m_afEntry[ 7];
        afCMajor[14] = m_afEntry[11];
        afCMajor[15] = m_afEntry[15];
    }

    /** Multiply this matrix to the input matrix, return result, this is unchanged. 
     * @param rkM, input matrix
     * @return this*rkM
     */
    public Matrix4f mult( Matrix4f rkM )
    {
        return new Matrix4f(
                             m_afEntry[ 0]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 1]*rkM.m_afEntry[ 4] +
                             m_afEntry[ 2]*rkM.m_afEntry[ 8] +
                             m_afEntry[ 3]*rkM.m_afEntry[12],

                             m_afEntry[ 0]*rkM.m_afEntry[ 1] +
                             m_afEntry[ 1]*rkM.m_afEntry[ 5] +
                             m_afEntry[ 2]*rkM.m_afEntry[ 9] +
                             m_afEntry[ 3]*rkM.m_afEntry[13],

                             m_afEntry[ 0]*rkM.m_afEntry[ 2] +
                             m_afEntry[ 1]*rkM.m_afEntry[ 6] +
                             m_afEntry[ 2]*rkM.m_afEntry[10] +
                             m_afEntry[ 3]*rkM.m_afEntry[14],

                             m_afEntry[ 0]*rkM.m_afEntry[ 3] +
                             m_afEntry[ 1]*rkM.m_afEntry[ 7] +
                             m_afEntry[ 2]*rkM.m_afEntry[11] +
                             m_afEntry[ 3]*rkM.m_afEntry[15],

                             m_afEntry[ 4]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 4] +
                             m_afEntry[ 6]*rkM.m_afEntry[ 8] +
                             m_afEntry[ 7]*rkM.m_afEntry[12],

                             m_afEntry[ 4]*rkM.m_afEntry[ 1] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 5] +
                             m_afEntry[ 6]*rkM.m_afEntry[ 9] +
                             m_afEntry[ 7]*rkM.m_afEntry[13],

                             m_afEntry[ 4]*rkM.m_afEntry[ 2] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 6] +
                             m_afEntry[ 6]*rkM.m_afEntry[10] +
                             m_afEntry[ 7]*rkM.m_afEntry[14],

                             m_afEntry[ 4]*rkM.m_afEntry[ 3] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 7] +
                             m_afEntry[ 6]*rkM.m_afEntry[11] +
                             m_afEntry[ 7]*rkM.m_afEntry[15],

                             m_afEntry[ 8]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 9]*rkM.m_afEntry[ 4] +
                             m_afEntry[10]*rkM.m_afEntry[ 8] +
                             m_afEntry[11]*rkM.m_afEntry[12],

                             m_afEntry[ 8]*rkM.m_afEntry[ 1] +
                             m_afEntry[ 9]*rkM.m_afEntry[ 5] +
                             m_afEntry[10]*rkM.m_afEntry[ 9] +
                             m_afEntry[11]*rkM.m_afEntry[13],

                             m_afEntry[ 8]*rkM.m_afEntry[ 2] +
                             m_afEntry[ 9]*rkM.m_afEntry[ 6] +
                             m_afEntry[10]*rkM.m_afEntry[10] +
                             m_afEntry[11]*rkM.m_afEntry[14],

                             m_afEntry[ 8]*rkM.m_afEntry[ 3] +
                             m_afEntry[ 9]*rkM.m_afEntry[ 7] +
                             m_afEntry[10]*rkM.m_afEntry[11] +
                             m_afEntry[11]*rkM.m_afEntry[15],

                             m_afEntry[12]*rkM.m_afEntry[ 0] +
                             m_afEntry[13]*rkM.m_afEntry[ 4] +
                             m_afEntry[14]*rkM.m_afEntry[ 8] +
                             m_afEntry[15]*rkM.m_afEntry[12],

                             m_afEntry[12]*rkM.m_afEntry[ 1] +
                             m_afEntry[13]*rkM.m_afEntry[ 5] +
                             m_afEntry[14]*rkM.m_afEntry[ 9] +
                             m_afEntry[15]*rkM.m_afEntry[13],

                             m_afEntry[12]*rkM.m_afEntry[ 2] +
                             m_afEntry[13]*rkM.m_afEntry[ 6] +
                             m_afEntry[14]*rkM.m_afEntry[10] +
                             m_afEntry[15]*rkM.m_afEntry[14],

                             m_afEntry[12]*rkM.m_afEntry[ 3] +
                             m_afEntry[13]*rkM.m_afEntry[ 7] +
                             m_afEntry[14]*rkM.m_afEntry[11] +
                             m_afEntry[15]*rkM.m_afEntry[15]);
    }

    /** matrix times vector
     * M * v
     * @param rkV, vector
     * @return M * v
     */
    public Vector4f mult ( Vector4f rkV)   // M * v
    {
        return new Vector4f(
                            m_afEntry[ 0]*rkV.X() +
                            m_afEntry[ 1]*rkV.Y() +
                            m_afEntry[ 2]*rkV.Z() +
                            m_afEntry[ 3]*rkV.W(),
                            
                            m_afEntry[ 4]*rkV.X() +
                            m_afEntry[ 5]*rkV.Y() +
                            m_afEntry[ 6]*rkV.Z() +
                            m_afEntry[ 7]*rkV.W(),
                            
                            m_afEntry[ 8]*rkV.X() +
                            m_afEntry[ 9]*rkV.Y() +
                            m_afEntry[10]*rkV.Z() +
                            m_afEntry[11]*rkV.W(),
                            
                            m_afEntry[12]*rkV.X() +
                            m_afEntry[13]*rkV.Y() +
                            m_afEntry[14]*rkV.Z() +
                            m_afEntry[15]*rkV.W());
    }

    /** matrix times vector
     * v^T * M
     * @param rkV, vector
     * @param rkM, matrix
     * @return v^T * M 
     */
    public static Vector4f mult(Vector4f rkV, Matrix4f rkM)
    {
        return new
            Vector4f(
                     rkV.X()*rkM.m_afEntry[0] + rkV.Y()*rkM.m_afEntry[4] + rkV.Z()*rkM.m_afEntry[8] + rkV.W()*rkM.m_afEntry[12],
                     rkV.X()*rkM.m_afEntry[1] + rkV.Y()*rkM.m_afEntry[5] + rkV.Z()*rkM.m_afEntry[9] + rkV.W()*rkM.m_afEntry[13],
                     rkV.X()*rkM.m_afEntry[2] + rkV.Y()*rkM.m_afEntry[6] + rkV.Z()*rkM.m_afEntry[10] + rkV.W()*rkM.m_afEntry[14],
                     rkV.X()*rkM.m_afEntry[3] + rkV.Y()*rkM.m_afEntry[7] + rkV.Z()*rkM.m_afEntry[11] + rkV.W()*rkM.m_afEntry[15]);
    }


    /** Transpose this matrix and return the result, this matrix is unchanged.
     * @return  M^T
     */
    public Matrix4f Transpose ()  // M^T
    {
        return new Matrix4f(
                            m_afEntry[ 0],
                            m_afEntry[ 4],
                            m_afEntry[ 8],
                            m_afEntry[12],
                            m_afEntry[ 1],
                            m_afEntry[ 5],
                            m_afEntry[ 9],
                            m_afEntry[13],
                            m_afEntry[ 2],
                            m_afEntry[ 6],
                            m_afEntry[10],
                            m_afEntry[14],
                            m_afEntry[ 3],
                            m_afEntry[ 7],
                            m_afEntry[11],
                            m_afEntry[15]);
    }

    /** Transpose this matrix and multiply by input, return the result, this
     * matrix is unchanged.
     * @param rkM, matrix
     * @return  this^T * M
     */
    public Matrix4f TransposeTimes (Matrix4f rkM)  // this^T * M
    {
        // P = A^T*B
        return new Matrix4f(
                             m_afEntry[ 0]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 4]*rkM.m_afEntry[ 4] +
                             m_afEntry[ 8]*rkM.m_afEntry[ 8] +
                             m_afEntry[12]*rkM.m_afEntry[12],

                             m_afEntry[ 0]*rkM.m_afEntry[ 1] +
                             m_afEntry[ 4]*rkM.m_afEntry[ 5] +
                             m_afEntry[ 8]*rkM.m_afEntry[ 9] +
                             m_afEntry[12]*rkM.m_afEntry[13],

                             m_afEntry[ 0]*rkM.m_afEntry[ 2] +
                             m_afEntry[ 4]*rkM.m_afEntry[ 6] +
                             m_afEntry[ 8]*rkM.m_afEntry[10] +
                             m_afEntry[12]*rkM.m_afEntry[14],

                             m_afEntry[ 0]*rkM.m_afEntry[ 3] +
                             m_afEntry[ 4]*rkM.m_afEntry[ 7] +
                             m_afEntry[ 8]*rkM.m_afEntry[11] +
                             m_afEntry[12]*rkM.m_afEntry[15],

                             m_afEntry[ 1]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 4] +
                             m_afEntry[ 9]*rkM.m_afEntry[ 8] +
                             m_afEntry[13]*rkM.m_afEntry[12],

                             m_afEntry[ 1]*rkM.m_afEntry[ 1] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 5] +
                             m_afEntry[ 9]*rkM.m_afEntry[ 9] +
                             m_afEntry[13]*rkM.m_afEntry[13],

                             m_afEntry[ 1]*rkM.m_afEntry[ 2] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 6] +
                             m_afEntry[ 9]*rkM.m_afEntry[10] +
                             m_afEntry[13]*rkM.m_afEntry[14],

                             m_afEntry[ 1]*rkM.m_afEntry[ 3] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 7] +
                             m_afEntry[ 9]*rkM.m_afEntry[11] +
                             m_afEntry[13]*rkM.m_afEntry[15],

                             m_afEntry[ 2]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 6]*rkM.m_afEntry[ 4] +
                             m_afEntry[10]*rkM.m_afEntry[ 8] +
                             m_afEntry[14]*rkM.m_afEntry[12],

                             m_afEntry[ 2]*rkM.m_afEntry[ 1] +
                             m_afEntry[ 6]*rkM.m_afEntry[ 5] +
                             m_afEntry[10]*rkM.m_afEntry[ 9] +
                             m_afEntry[14]*rkM.m_afEntry[13],

                             m_afEntry[ 2]*rkM.m_afEntry[ 2] +
                             m_afEntry[ 6]*rkM.m_afEntry[ 6] +
                             m_afEntry[10]*rkM.m_afEntry[10] +
                             m_afEntry[14]*rkM.m_afEntry[14],

                             m_afEntry[ 2]*rkM.m_afEntry[ 3] +
                             m_afEntry[ 6]*rkM.m_afEntry[ 7] +
                             m_afEntry[10]*rkM.m_afEntry[11] +
                             m_afEntry[14]*rkM.m_afEntry[15],

                             m_afEntry[ 3]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 7]*rkM.m_afEntry[ 4] +
                             m_afEntry[11]*rkM.m_afEntry[ 8] +
                             m_afEntry[15]*rkM.m_afEntry[12],

                             m_afEntry[ 3]*rkM.m_afEntry[ 1] +
                             m_afEntry[ 7]*rkM.m_afEntry[ 5] +
                             m_afEntry[11]*rkM.m_afEntry[ 9] +
                             m_afEntry[15]*rkM.m_afEntry[13],

                             m_afEntry[ 3]*rkM.m_afEntry[ 2] +
                             m_afEntry[ 7]*rkM.m_afEntry[ 6] +
                             m_afEntry[11]*rkM.m_afEntry[10] +
                             m_afEntry[15]*rkM.m_afEntry[14],

                             m_afEntry[ 3]*rkM.m_afEntry[ 3] +
                             m_afEntry[ 7]*rkM.m_afEntry[ 7] +
                             m_afEntry[11]*rkM.m_afEntry[11] +
                             m_afEntry[15]*rkM.m_afEntry[15]);
    }

    /** Multiply this matrix by transpose of the input matrix, return the
     * result, this matrix is unchanged.
     * @param rkM, matrix
     * @return this * M^T
     */
    public Matrix4f TimesTranspose (Matrix4f rkM)  // this * M^T
    {
        // P = A*B^T
        return new Matrix4f(
                             m_afEntry[ 0]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 1]*rkM.m_afEntry[ 1] +
                             m_afEntry[ 2]*rkM.m_afEntry[ 2] +
                             m_afEntry[ 3]*rkM.m_afEntry[ 3],

                             m_afEntry[ 0]*rkM.m_afEntry[ 4] +
                             m_afEntry[ 1]*rkM.m_afEntry[ 5] +
                             m_afEntry[ 2]*rkM.m_afEntry[ 6] +
                             m_afEntry[ 3]*rkM.m_afEntry[ 7],

                             m_afEntry[ 0]*rkM.m_afEntry[ 8] +
                             m_afEntry[ 1]*rkM.m_afEntry[ 9] +
                             m_afEntry[ 2]*rkM.m_afEntry[10] +
                             m_afEntry[ 3]*rkM.m_afEntry[11],

                             m_afEntry[ 0]*rkM.m_afEntry[12] +
                             m_afEntry[ 1]*rkM.m_afEntry[13] +
                             m_afEntry[ 2]*rkM.m_afEntry[14] +
                             m_afEntry[ 3]*rkM.m_afEntry[15],

                             m_afEntry[ 4]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 1] +
                             m_afEntry[ 6]*rkM.m_afEntry[ 2] +
                             m_afEntry[ 7]*rkM.m_afEntry[ 3],

                             m_afEntry[ 4]*rkM.m_afEntry[ 4] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 5] +
                             m_afEntry[ 6]*rkM.m_afEntry[ 6] +
                             m_afEntry[ 7]*rkM.m_afEntry[ 7],

                             m_afEntry[ 4]*rkM.m_afEntry[ 8] +
                             m_afEntry[ 5]*rkM.m_afEntry[ 9] +
                             m_afEntry[ 6]*rkM.m_afEntry[10] +
                             m_afEntry[ 7]*rkM.m_afEntry[11],

                             m_afEntry[ 4]*rkM.m_afEntry[12] +
                             m_afEntry[ 5]*rkM.m_afEntry[13] +
                             m_afEntry[ 6]*rkM.m_afEntry[14] +
                             m_afEntry[ 7]*rkM.m_afEntry[15],

                             m_afEntry[ 8]*rkM.m_afEntry[ 0] +
                             m_afEntry[ 9]*rkM.m_afEntry[ 1] +
                             m_afEntry[10]*rkM.m_afEntry[ 2] +
                             m_afEntry[11]*rkM.m_afEntry[ 3],

                             m_afEntry[ 8]*rkM.m_afEntry[ 4] +
                             m_afEntry[ 9]*rkM.m_afEntry[ 5] +
                             m_afEntry[10]*rkM.m_afEntry[ 6] +
                             m_afEntry[11]*rkM.m_afEntry[ 7],

                             m_afEntry[ 8]*rkM.m_afEntry[ 8] +
                             m_afEntry[ 9]*rkM.m_afEntry[ 9] +
                             m_afEntry[10]*rkM.m_afEntry[10] +
                             m_afEntry[11]*rkM.m_afEntry[11],

                             m_afEntry[ 8]*rkM.m_afEntry[12] +
                             m_afEntry[ 9]*rkM.m_afEntry[13] +
                             m_afEntry[10]*rkM.m_afEntry[14] +
                             m_afEntry[11]*rkM.m_afEntry[15],

                             m_afEntry[12]*rkM.m_afEntry[ 0] +
                             m_afEntry[13]*rkM.m_afEntry[ 1] +
                             m_afEntry[14]*rkM.m_afEntry[ 2] +
                             m_afEntry[15]*rkM.m_afEntry[ 3],

                             m_afEntry[12]*rkM.m_afEntry[ 4] +
                             m_afEntry[13]*rkM.m_afEntry[ 5] +
                             m_afEntry[14]*rkM.m_afEntry[ 6] +
                             m_afEntry[15]*rkM.m_afEntry[ 7],

                             m_afEntry[12]*rkM.m_afEntry[ 8] +
                             m_afEntry[13]*rkM.m_afEntry[ 9] +
                             m_afEntry[14]*rkM.m_afEntry[10] +
                             m_afEntry[15]*rkM.m_afEntry[11],

                             m_afEntry[12]*rkM.m_afEntry[12] +
                             m_afEntry[13]*rkM.m_afEntry[13] +
                             m_afEntry[14]*rkM.m_afEntry[14] +
                             m_afEntry[15]*rkM.m_afEntry[15]);
    }

    /** Invert a 4x4 using cofactors.  This is faster than using a generic
     * Gaussian elimination because of the loop overhead of such a method.
     * @return resulting matrix
     */
    public Matrix4f Inverse ()
    {
        float fA0 = m_afEntry[ 0]*m_afEntry[ 5] - m_afEntry[ 1]*m_afEntry[ 4];
        float fA1 = m_afEntry[ 0]*m_afEntry[ 6] - m_afEntry[ 2]*m_afEntry[ 4];
        float fA2 = m_afEntry[ 0]*m_afEntry[ 7] - m_afEntry[ 3]*m_afEntry[ 4];
        float fA3 = m_afEntry[ 1]*m_afEntry[ 6] - m_afEntry[ 2]*m_afEntry[ 5];
        float fA4 = m_afEntry[ 1]*m_afEntry[ 7] - m_afEntry[ 3]*m_afEntry[ 5];
        float fA5 = m_afEntry[ 2]*m_afEntry[ 7] - m_afEntry[ 3]*m_afEntry[ 6];
        float fB0 = m_afEntry[ 8]*m_afEntry[13] - m_afEntry[ 9]*m_afEntry[12];
        float fB1 = m_afEntry[ 8]*m_afEntry[14] - m_afEntry[10]*m_afEntry[12];
        float fB2 = m_afEntry[ 8]*m_afEntry[15] - m_afEntry[11]*m_afEntry[12];
        float fB3 = m_afEntry[ 9]*m_afEntry[14] - m_afEntry[10]*m_afEntry[13];
        float fB4 = m_afEntry[ 9]*m_afEntry[15] - m_afEntry[11]*m_afEntry[13];
        float fB5 = m_afEntry[10]*m_afEntry[15] - m_afEntry[11]*m_afEntry[14];

        float fDet = fA0*fB5-fA1*fB4+fA2*fB3+fA3*fB2-fA4*fB1+fA5*fB0;
        if (Math.abs(fDet) <= Mathf.ZERO_TOLERANCE)
        {
            return new Matrix4f(Matrix4f.ZERO);
        }

        Matrix4f kInv = new Matrix4f();
        kInv.m_afEntry[ 0] =
            + m_afEntry[ 5]*fB5 - m_afEntry[ 6]*fB4 + m_afEntry[ 7]*fB3;
        kInv.m_afEntry[ 4] =
            - m_afEntry[ 4]*fB5 + m_afEntry[ 6]*fB2 - m_afEntry[ 7]*fB1;
        kInv.m_afEntry[ 8] =
            + m_afEntry[ 4]*fB4 - m_afEntry[ 5]*fB2 + m_afEntry[ 7]*fB0;
        kInv.m_afEntry[12] =
            - m_afEntry[ 4]*fB3 + m_afEntry[ 5]*fB1 - m_afEntry[ 6]*fB0;
        kInv.m_afEntry[ 1] =
            - m_afEntry[ 1]*fB5 + m_afEntry[ 2]*fB4 - m_afEntry[ 3]*fB3;
        kInv.m_afEntry[ 5] =
            + m_afEntry[ 0]*fB5 - m_afEntry[ 2]*fB2 + m_afEntry[ 3]*fB1;
        kInv.m_afEntry[ 9] =
            - m_afEntry[ 0]*fB4 + m_afEntry[ 1]*fB2 - m_afEntry[ 3]*fB0;
        kInv.m_afEntry[13] =
            + m_afEntry[ 0]*fB3 - m_afEntry[ 1]*fB1 + m_afEntry[ 2]*fB0;
        kInv.m_afEntry[ 2] =
            + m_afEntry[13]*fA5 - m_afEntry[14]*fA4 + m_afEntry[15]*fA3;
        kInv.m_afEntry[ 6] =
            - m_afEntry[12]*fA5 + m_afEntry[14]*fA2 - m_afEntry[15]*fA1;
        kInv.m_afEntry[10] =
            + m_afEntry[12]*fA4 - m_afEntry[13]*fA2 + m_afEntry[15]*fA0;
        kInv.m_afEntry[14] =
            - m_afEntry[12]*fA3 + m_afEntry[13]*fA1 - m_afEntry[14]*fA0;
        kInv.m_afEntry[ 3] =
            - m_afEntry[ 9]*fA5 + m_afEntry[10]*fA4 - m_afEntry[11]*fA3;
        kInv.m_afEntry[ 7] =
            + m_afEntry[ 8]*fA5 - m_afEntry[10]*fA2 + m_afEntry[11]*fA1;
        kInv.m_afEntry[11] =
            - m_afEntry[ 8]*fA4 + m_afEntry[ 9]*fA2 - m_afEntry[11]*fA0;
        kInv.m_afEntry[15] =
            + m_afEntry[ 8]*fA3 - m_afEntry[ 9]*fA1 + m_afEntry[10]*fA0;

        float fInvDet = ((float)1.0)/fDet;
        kInv.m_afEntry[ 0] *= fInvDet;
        kInv.m_afEntry[ 1] *= fInvDet;
        kInv.m_afEntry[ 2] *= fInvDet;
        kInv.m_afEntry[ 3] *= fInvDet;
        kInv.m_afEntry[ 4] *= fInvDet;
        kInv.m_afEntry[ 5] *= fInvDet;
        kInv.m_afEntry[ 6] *= fInvDet;
        kInv.m_afEntry[ 7] *= fInvDet;
        kInv.m_afEntry[ 8] *= fInvDet;
        kInv.m_afEntry[ 9] *= fInvDet;
        kInv.m_afEntry[10] *= fInvDet;
        kInv.m_afEntry[11] *= fInvDet;
        kInv.m_afEntry[12] *= fInvDet;
        kInv.m_afEntry[13] *= fInvDet;
        kInv.m_afEntry[14] *= fInvDet;
        kInv.m_afEntry[15] *= fInvDet;

        return kInv;
    }

    /** Return adjoint of this matrix, this is unchanged.
     * @return adjoint of this matrix, this is unchanged.
     */
    public Matrix4f Adjoint ()
    {
        float fA0 = m_afEntry[ 0]*m_afEntry[ 5] - m_afEntry[ 1]*m_afEntry[ 4];
        float fA1 = m_afEntry[ 0]*m_afEntry[ 6] - m_afEntry[ 2]*m_afEntry[ 4];
        float fA2 = m_afEntry[ 0]*m_afEntry[ 7] - m_afEntry[ 3]*m_afEntry[ 4];
        float fA3 = m_afEntry[ 1]*m_afEntry[ 6] - m_afEntry[ 2]*m_afEntry[ 5];
        float fA4 = m_afEntry[ 1]*m_afEntry[ 7] - m_afEntry[ 3]*m_afEntry[ 5];
        float fA5 = m_afEntry[ 2]*m_afEntry[ 7] - m_afEntry[ 3]*m_afEntry[ 6];
        float fB0 = m_afEntry[ 8]*m_afEntry[13] - m_afEntry[ 9]*m_afEntry[12];
        float fB1 = m_afEntry[ 8]*m_afEntry[14] - m_afEntry[10]*m_afEntry[12];
        float fB2 = m_afEntry[ 8]*m_afEntry[15] - m_afEntry[11]*m_afEntry[12];
        float fB3 = m_afEntry[ 9]*m_afEntry[14] - m_afEntry[10]*m_afEntry[13];
        float fB4 = m_afEntry[ 9]*m_afEntry[15] - m_afEntry[11]*m_afEntry[13];
        float fB5 = m_afEntry[10]*m_afEntry[15] - m_afEntry[11]*m_afEntry[14];

        return new Matrix4f(
                            + m_afEntry[ 5]*fB5 - m_afEntry[ 6]*fB4 + m_afEntry[ 7]*fB3,
                            - m_afEntry[ 1]*fB5 + m_afEntry[ 2]*fB4 - m_afEntry[ 3]*fB3,
                            + m_afEntry[13]*fA5 - m_afEntry[14]*fA4 + m_afEntry[15]*fA3,
                            - m_afEntry[ 9]*fA5 + m_afEntry[10]*fA4 - m_afEntry[11]*fA3,
                            - m_afEntry[ 4]*fB5 + m_afEntry[ 6]*fB2 - m_afEntry[ 7]*fB1,
                            + m_afEntry[ 0]*fB5 - m_afEntry[ 2]*fB2 + m_afEntry[ 3]*fB1,
                            - m_afEntry[12]*fA5 + m_afEntry[14]*fA2 - m_afEntry[15]*fA1,
                            + m_afEntry[ 8]*fA5 - m_afEntry[10]*fA2 + m_afEntry[11]*fA1,
                            + m_afEntry[ 4]*fB4 - m_afEntry[ 5]*fB2 + m_afEntry[ 7]*fB0,
                            - m_afEntry[ 0]*fB4 + m_afEntry[ 1]*fB2 - m_afEntry[ 3]*fB0,
                            + m_afEntry[12]*fA4 - m_afEntry[13]*fA2 + m_afEntry[15]*fA0,
                            - m_afEntry[ 8]*fA4 + m_afEntry[ 9]*fA2 - m_afEntry[11]*fA0,
                            - m_afEntry[ 4]*fB3 + m_afEntry[ 5]*fB1 - m_afEntry[ 6]*fB0,
                            + m_afEntry[ 0]*fB3 - m_afEntry[ 1]*fB1 + m_afEntry[ 2]*fB0,
                            - m_afEntry[12]*fA3 + m_afEntry[13]*fA1 - m_afEntry[14]*fA0,
                            + m_afEntry[ 8]*fA3 - m_afEntry[ 9]*fA1 + m_afEntry[10]*fA0);
    }

    /** 
     * Return the determinant of this matrix.
     * @return the determinant of this matrix.
     */
    public float Determinant ()
    {
        float fA0 = m_afEntry[ 0]*m_afEntry[ 5] - m_afEntry[ 1]*m_afEntry[ 4];
        float fA1 = m_afEntry[ 0]*m_afEntry[ 6] - m_afEntry[ 2]*m_afEntry[ 4];
        float fA2 = m_afEntry[ 0]*m_afEntry[ 7] - m_afEntry[ 3]*m_afEntry[ 4];
        float fA3 = m_afEntry[ 1]*m_afEntry[ 6] - m_afEntry[ 2]*m_afEntry[ 5];
        float fA4 = m_afEntry[ 1]*m_afEntry[ 7] - m_afEntry[ 3]*m_afEntry[ 5];
        float fA5 = m_afEntry[ 2]*m_afEntry[ 7] - m_afEntry[ 3]*m_afEntry[ 6];
        float fB0 = m_afEntry[ 8]*m_afEntry[13] - m_afEntry[ 9]*m_afEntry[12];
        float fB1 = m_afEntry[ 8]*m_afEntry[14] - m_afEntry[10]*m_afEntry[12];
        float fB2 = m_afEntry[ 8]*m_afEntry[15] - m_afEntry[11]*m_afEntry[12];
        float fB3 = m_afEntry[ 9]*m_afEntry[14] - m_afEntry[10]*m_afEntry[13];
        float fB4 = m_afEntry[ 9]*m_afEntry[15] - m_afEntry[11]*m_afEntry[13];
        float fB5 = m_afEntry[10]*m_afEntry[15] - m_afEntry[11]*m_afEntry[14];
        float fDet = fA0*fB5-fA1*fB4+fA2*fB3+fA3*fB2-fA4*fB1+fA5*fB0;
        return fDet;
    }


    /**
     * Calculate and return u^T*M*v
     * @param rkU, u
     * @param rkV, v
     * @return u^T*M*v
     */
    public float QForm ( Vector4f rkU,
                  Vector4f rkV)  // u^T*M*v
    {
        return rkU.Dot(this.mult(rkV));
    }


    /** projection matrices onto a specified plane
     * The projection plane is Dot(N,X-P) = 0 where N is a 3-by-1 unit-length
     * normal vector and P is a 3-by-1 point on the plane.  The projection
     * is oblique to the plane, in the direction of the 3-by-1 vector D.
     * Necessarily Dot(N,D) is not zero for this projection to make sense.
     * Given a 3-by-1 point U, compute the intersection of the line U+t*D
     * with the plane to obtain t = -Dot(N,U-P)/Dot(N,D).  Then
     *
     *   projection(U) = P + [I - D*N^T/Dot(N,D)]*(U-P)
     *
     * A 4-by-4 homogeneous transformation representing the projection is
     *
     *       +-                               -+
     *   M = | D*N^T - Dot(N,D)*I   -Dot(N,P)D |
     *       |          0^T          -Dot(N,D) |
     *       +-                               -+
     *
     * where M applies to [U^T 1]^T by M*[U^T 1]^T.  The matrix is chosen so
     * that M[3][3] > 0 whenever Dot(N,D) < 0 (projection is onto the
     * "positive side" of the plane).
     * @param rkNormal, normal vector
     * @param rkpoint, point
     * @param rkDirection, direction vector
     */
    public void MakeObliqueProjection ( Vector3f rkNormal,
                                        Vector3f rkPoint, Vector3f rkDirection)
    {

        float fNdD = rkNormal.Dot(rkDirection);
        float fNdP = rkNormal.Dot(rkPoint);
        m_afEntry[ 0] = rkDirection.X()*rkNormal.X() - fNdD;
        m_afEntry[ 1] = rkDirection.X()*rkNormal.Y();
        m_afEntry[ 2] = rkDirection.X()*rkNormal.Z();
        m_afEntry[ 3] = -fNdP*rkDirection.X();
        m_afEntry[ 4] = rkDirection.Y()*rkNormal.X();
        m_afEntry[ 5] = rkDirection.Y()*rkNormal.Y() - fNdD;
        m_afEntry[ 6] = rkDirection.Y()*rkNormal.Z();
        m_afEntry[ 7] = -fNdP*rkDirection.Y();
        m_afEntry[ 8] = rkDirection.Z()*rkNormal.X();
        m_afEntry[ 9] = rkDirection.Z()*rkNormal.Y();
        m_afEntry[10] = rkDirection.Z()*rkNormal.Z() - fNdD;
        m_afEntry[11] = -fNdP*rkDirection.Z();
        m_afEntry[12] = 0.0f;
        m_afEntry[13] = 0.0f;
        m_afEntry[14] = 0.0f;
        m_afEntry[15] = -fNdD;
    }

    /**
     *     +-                                                 -+
     * M = | Dot(N,E-P)*I - E*N^T    -(Dot(N,E-P)*I - E*N^T)*E |
     *     |        -N^t                      Dot(N,E)         |
     *     +-                                                 -+
     *
     * where E is the eye point, P is a point on the plane, and N is a
     * unit-length plane normal.
     * @param rkNormal, normal vector
     * @param rkpoint, point
     * @param rkEye, eye vector
     */
    public void MakePerspectiveProjection (Vector3f rkNormal,
                                           Vector3f rkPoint, Vector3f rkEye)
    {

        float fNdEmP = rkNormal.Dot(rkEye.sub(rkPoint));

        m_afEntry[ 0] = fNdEmP - rkEye.X()*rkNormal.X();
        m_afEntry[ 1] = -rkEye.X()*rkNormal.Y();
        m_afEntry[ 2] = -rkEye.X()*rkNormal.Z();
        m_afEntry[ 3] = -(m_afEntry[0]*rkEye.X() + m_afEntry[1]*rkEye.Y() +
                          m_afEntry[2]*rkEye.Z());
        m_afEntry[ 4] = -rkEye.Y()*rkNormal.X();
        m_afEntry[ 5] = fNdEmP - rkEye.Y()*rkNormal.Y();
        m_afEntry[ 6] = -rkEye.Y()*rkNormal.Z();
        m_afEntry[ 7] = -(m_afEntry[4]*rkEye.X() + m_afEntry[5]*rkEye.Y() +
                          m_afEntry[6]*rkEye.Z());
        m_afEntry[ 8] = -rkEye.Z()*rkNormal.X();
        m_afEntry[ 9] = -rkEye.Z()*rkNormal.Y();
        m_afEntry[10] = fNdEmP- rkEye.Z()*rkNormal.Z();
        m_afEntry[11] = -(m_afEntry[8]*rkEye.X() + m_afEntry[9]*rkEye.Y() +
                          m_afEntry[10]*rkEye.Z());
        m_afEntry[12] = -rkNormal.X();
        m_afEntry[13] = -rkNormal.Y();
        m_afEntry[14] = -rkNormal.Z();
        m_afEntry[15] = rkNormal.Dot(rkEye);
    }


    /** reflection matrix through a specified plane
     *     +-                         -+
     * M = | I-2*N*N^T    2*Dot(N,P)*N |
     *     |     0^T            1      |
     *     +-                         -+
     *
     * where P is a point on the plane and N is a unit-length plane normal.
     * @param rkNormal, normal vector
     * @param rkpoint, point
     */
    public void MakeReflection ( Vector3f rkNormal,
                                 Vector3f rkPoint)
    {

        float fTwoNdP = ((float)2.0)*(rkNormal.Dot(rkPoint));

        m_afEntry[ 0] = (float)1.0 - ((float)2.0)*rkNormal.X()*rkNormal.X();
        m_afEntry[ 1] = -((float)2.0)*rkNormal.X()*rkNormal.Y();
        m_afEntry[ 2] = -((float)2.0)*rkNormal.X()*rkNormal.Z();
        m_afEntry[ 3] = fTwoNdP*rkNormal.X();
        m_afEntry[ 4] = -((float)2.0)*rkNormal.Y()*rkNormal.X();
        m_afEntry[ 5] = (float)1.0 - ((float)2.0)*rkNormal.Y()*rkNormal.Y();
        m_afEntry[ 6] = -((float)2.0)*rkNormal.Y()*rkNormal.Z();
        m_afEntry[ 7] = fTwoNdP*rkNormal.Y();
        m_afEntry[ 8] = -((float)2.0)*rkNormal.Z()*rkNormal.X();
        m_afEntry[ 9] = -((float)2.0)*rkNormal.Z()*rkNormal.Y();
        m_afEntry[10] = (float)1.0 - ((float)2.0)*rkNormal.Z()*rkNormal.Z();
        m_afEntry[11] = fTwoNdP*rkNormal.Z();
        m_afEntry[12] = (float)0.0;
        m_afEntry[13] = (float)0.0;
        m_afEntry[14] = (float)0.0;
        m_afEntry[15] = (float)1.0;
    }
 
    /** Matrix data: */
    private float[] m_afEntry = new float[16];
}
