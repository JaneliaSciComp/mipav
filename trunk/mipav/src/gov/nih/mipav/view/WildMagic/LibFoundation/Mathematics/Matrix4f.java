// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Foundation Library source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4FoundationLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.1 (2006/08/19)

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
    /** special matrices */
    public static final Matrix4f ZERO =
        new Matrix4f(
                     0.0f,0.0f,0.0f,0.0f,
                     0.0f,0.0f,0.0f,0.0f,
                     0.0f,0.0f,0.0f,0.0f,
                     0.0f,0.0f,0.0f,0.0f);

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
    

    /** copy constructor */
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


    /** input Mrc is in row r, column c. */
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

    // member access
    public void SetData( int iRow, int iCol, float fValue )
    {
        m_afEntry[iRow*4 + iCol] = fValue;
    }

    public float GetData( int iRow, int iCol )
    {
        return m_afEntry[iRow*4 + iCol];
    }

    public void SetRow (int iRow, Vector4f rkV)
    {
        int i0 = 4*iRow, i1 = i0+1, i2 = i1+1, i3 = i2+1;
        m_afEntry[i0] = rkV.X();
        m_afEntry[i1] = rkV.Y();
        m_afEntry[i2] = rkV.Z();
        m_afEntry[i3] = rkV.W();
    }

    public Vector4f GetRow (int iRow)
    {
        int i0 = 4*iRow, i1 = i0+1, i2 = i1+1, i3 = i2+1;
        return new Vector4f(m_afEntry[i0],m_afEntry[i1],m_afEntry[i2],
                            m_afEntry[i3]);
    }

    public void SetColumn (int iCol, Vector4f rkV)
    {
        m_afEntry[iCol] = rkV.X();
        m_afEntry[iCol+4] = rkV.Y();
        m_afEntry[iCol+8] = rkV.Z();
        m_afEntry[iCol+12] = rkV.W();
    }

    public Vector4f GetColumn (int iCol)
    {
        return new Vector4f(m_afEntry[iCol],m_afEntry[iCol+4],m_afEntry[iCol+8],
                            m_afEntry[iCol+12]);
    }

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

    // arithmetic operations
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

    /** matrix times vector */
    Vector4f mult ( Vector4f rkV)   // M * v
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

    // other operations
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
 
    private float[] m_afEntry = new float[16];
}
