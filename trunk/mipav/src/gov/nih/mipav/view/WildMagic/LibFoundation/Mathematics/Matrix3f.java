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

 * The (x,y,z) coordinate system is assumed to be right-handed.  Coordinate
 * axis rotation matrices are of the form
 *   RX =    1       0       0
 *           0     cos(t) -sin(t)
 *           0     sin(t)  cos(t)
 * where t > 0 indicates a counterclockwise rotation in the yz-plane
 *   RY =  cos(t)    0     sin(t)
 *           0       1       0
 *        -sin(t)    0     cos(t)
 * where t > 0 indicates a counterclockwise rotation in the zx-plane
 *   RZ =  cos(t) -sin(t)    0
 *         sin(t)  cos(t)    0
 *           0       0       1
 * where t > 0 indicates a counterclockwise rotation in the xy-plane.
 */
public class Matrix3f
{
    /** Zero matrix: */
    public static final Matrix3f ZERO = new Matrix3f(0.0f,0.0f,0.0f,
                                                     0.0f,0.0f,0.0f,
                                                     0.0f,0.0f,0.0f);
    /** Identity matrix: */
    public static final Matrix3f IDENTITY = new Matrix3f(1.0f,0.0f,0.0f,
                                                         0.0f,1.0f,0.0f,
                                                         0.0f,0.0f,1.0f);;
    /** Constructor, defaults to zero-matrix: */
    public Matrix3f ()
    {
        MakeZero();
    }

    /** If bZero is true, create the zero matrix.  Otherwise, create the
     * identity matrix.
     * @param bZero, when true create zero matrix, when false create identity.
     */
    public Matrix3f (boolean bZero)
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
     * @param rkM, matrix to copy.
     */
    public Matrix3f ( Matrix3f rkM)
    {
        m_afEntry[0] = rkM.m_afEntry[0];
        m_afEntry[1] = rkM.m_afEntry[1];
        m_afEntry[2] = rkM.m_afEntry[2];
        m_afEntry[3] = rkM.m_afEntry[3];
        m_afEntry[4] = rkM.m_afEntry[4];
        m_afEntry[5] = rkM.m_afEntry[5];
        m_afEntry[6] = rkM.m_afEntry[6];
        m_afEntry[7] = rkM.m_afEntry[7];
        m_afEntry[8] = rkM.m_afEntry[8];
    }


    /** input Mrc is in row r, column c.
     * @param fM00, matrix[0] entry
     * @param fM01, matrix[1] entry
     * @param fM02, matrix[2] entry
     * @param fM10, matrix[3] entry
     * @param fM11, matrix[4] entry
     * @param fM12, matrix[5] entry
     * @param fM20, matrix[6] entry
     * @param fM21, matrix[7] entry
     * @param fM22, matrix[8] entry
     */
    public Matrix3f (float fM00, float fM01, float fM02,
                     float fM10, float fM11, float fM12,
                     float fM20, float fM21, float fM22)
    {
        m_afEntry[0] = fM00;
        m_afEntry[1] = fM01;
        m_afEntry[2] = fM02;
        m_afEntry[3] = fM10;
        m_afEntry[4] = fM11;
        m_afEntry[5] = fM12;
        m_afEntry[6] = fM20;
        m_afEntry[7] = fM21;
        m_afEntry[8] = fM22;
    }


    /** Create a matrix from an array of numbers.  The input array is
     * interpreted based on the Boolean input as
     *   true:  entry[0..8]={m00,m01,m02,m10,m11,m12,m20,m21,m22} [row major]
     *   false: entry[0..8]={m00,m10,m20,m01,m11,m21,m02,m12,m22} [col major]
     * @param afEntry, array of values to put in matrix
     * @param bRowMajor, when true copy in row major order.
     */
    public Matrix3f ( float[] afEntry, boolean bRowMajor)
    {
        if (bRowMajor)
        {
            m_afEntry[0] = afEntry[0];
            m_afEntry[1] = afEntry[1];
            m_afEntry[2] = afEntry[2];
            m_afEntry[3] = afEntry[3];
            m_afEntry[4] = afEntry[4];
            m_afEntry[5] = afEntry[5];
            m_afEntry[6] = afEntry[6];
            m_afEntry[7] = afEntry[7];
            m_afEntry[8] = afEntry[8];
        }
        else
        {
            m_afEntry[0] = afEntry[0];
            m_afEntry[1] = afEntry[3];
            m_afEntry[2] = afEntry[6];
            m_afEntry[3] = afEntry[1];
            m_afEntry[4] = afEntry[4];
            m_afEntry[5] = afEntry[7];
            m_afEntry[6] = afEntry[2];
            m_afEntry[7] = afEntry[5];
            m_afEntry[8] = afEntry[8];
        }
    }


    /** Create matrices based on vector input.  The Boolean is interpreted as
     *   true: vectors are columns of the matrix
     *   false: vectors are rows of the matrix
     * @param rkU, input vector1
     * @param rkV, input vector2
     * @param rkW, input vector3
     * @param bColumns, when true vectors are columns of matrix.
     */
    public Matrix3f (Vector3f rkU, Vector3f rkV,
                     Vector3f rkW, boolean bColumns)
    {
        if (bColumns)
        {
            m_afEntry[0] = rkU.X();
            m_afEntry[1] = rkV.X();
            m_afEntry[2] = rkW.X();
            m_afEntry[3] = rkU.Y();
            m_afEntry[4] = rkV.Y();
            m_afEntry[5] = rkW.Y();
            m_afEntry[6] = rkU.Z();
            m_afEntry[7] = rkV.Z();
            m_afEntry[8] = rkW.Z();
        }
        else
        {
            m_afEntry[0] = rkU.X();
            m_afEntry[1] = rkU.Y();
            m_afEntry[2] = rkU.Z();
            m_afEntry[3] = rkV.X();
            m_afEntry[4] = rkV.Y();
            m_afEntry[5] = rkV.Z();
            m_afEntry[6] = rkW.X();
            m_afEntry[7] = rkW.Y();
            m_afEntry[8] = rkW.Z();
        }
    }

    /** Create matrices based on vector input.  The Boolean is interpreted as
     *   true: vectors are columns of the matrix
     *   false: vectors are rows of the matrix
     * @param akV, input vector[3]
     * @param bColumns, when true vectors are columns of matrix.
     */
    public Matrix3f ( Vector3f[] akV, boolean bColumns)
    {
        if (bColumns)
        {
            m_afEntry[0] = akV[0].X();
            m_afEntry[1] = akV[1].X();
            m_afEntry[2] = akV[2].X();
            m_afEntry[3] = akV[0].Y();
            m_afEntry[4] = akV[1].Y();
            m_afEntry[5] = akV[2].Y();
            m_afEntry[6] = akV[0].Z();
            m_afEntry[7] = akV[1].Z();
            m_afEntry[8] = akV[2].Z();
        }
        else
        {
            m_afEntry[0] = akV[0].X();
            m_afEntry[1] = akV[0].Y();
            m_afEntry[2] = akV[0].Z();
            m_afEntry[3] = akV[1].X();
            m_afEntry[4] = akV[1].Y();
            m_afEntry[5] = akV[1].Z();
            m_afEntry[6] = akV[2].X();
            m_afEntry[7] = akV[2].Y();
            m_afEntry[8] = akV[2].Z();
        }
    }


    /** create a diagonal matrix
     * @param fM00, 0-diagonal value
     * @param fM11, 1-diagonal value
     * @param fM22, 2-diagonal value
     */
    public Matrix3f (float fM00, float fM11, float fM22)
    {
        MakeDiagonal(fM00,fM11,fM22);
    }


    /** Create rotation matrices (positive angle - counterclockwise).  The
     * angle must be in radians, not degrees.
     * @param rkAxis, rotation axis
     * @param fAngle, rotation angle in radians
     */
    public Matrix3f (Vector3f rkAxis, float fAngle)
    {
        FromAxisAngle(rkAxis,fAngle);
    }

    /** create a tensor product U*V^T
     * @param rkU, U-Vector
     * @param rkV, V-Vector
     */
    public Matrix3f (Vector3f rkU, Vector3f rkV)
    {
        MakeTensorProduct(rkU,rkV);
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        if ( (this == Matrix3f.ZERO) || (this == Matrix3f.IDENTITY) )
        {
            System.err.println( "Matrix3f" );
        }

        m_afEntry = null;
    }


    /** make this the zero matrix */
    public void MakeZero ()
    {
        m_afEntry[0] = (float)0.0;
        m_afEntry[1] = (float)0.0;
        m_afEntry[2] = (float)0.0;
        m_afEntry[3] = (float)0.0;
        m_afEntry[4] = (float)0.0;
        m_afEntry[5] = (float)0.0;
        m_afEntry[6] = (float)0.0;
        m_afEntry[7] = (float)0.0;
        m_afEntry[8] = (float)0.0;
    }

    /** make this the identity matrix */
    public void MakeIdentity ()
    {
        m_afEntry[0] = (float)1.0;
        m_afEntry[1] = (float)0.0;
        m_afEntry[2] = (float)0.0;
        m_afEntry[3] = (float)0.0;
        m_afEntry[4] = (float)1.0;
        m_afEntry[5] = (float)0.0;
        m_afEntry[6] = (float)0.0;
        m_afEntry[7] = (float)0.0;
        m_afEntry[8] = (float)1.0;
    }

    /** Create a diagonal matrix:
     * @param fM00, matrix[0] entry
     * @param fM11, matrix[4] entry
     * @param fM22, matrix[8] entry
     */
    public void MakeDiagonal (float fM00, float fM11, float fM22)
    {
        m_afEntry[0] = fM00;
        m_afEntry[1] = (float)0.0;
        m_afEntry[2] = (float)0.0;
        m_afEntry[3] = (float)0.0;
        m_afEntry[4] = fM11;
        m_afEntry[5] = (float)0.0;
        m_afEntry[6] = (float)0.0;
        m_afEntry[7] = (float)0.0;
        m_afEntry[8] = fM22;
    }

    /**
     * Create a rotation matrix from an axis and angle in radians.
     * @param rkAxis, rotation axis
     * @param fAngle, rotation angle in radians
     */
    public void FromAxisAngle (Vector3f rkAxis, float fAngle)
    {
        float fCos = (float)Math.cos(fAngle);
        float fSin = (float)Math.sin(fAngle);
        float fOneMinusCos = ((float)1.0)-fCos;
        float fX2 = rkAxis.X()*rkAxis.X();
        float fY2 = rkAxis.Y()*rkAxis.Y();
        float fZ2 = rkAxis.Z()*rkAxis.Z();
        float fXYM = rkAxis.X()*rkAxis.Y()*fOneMinusCos;
        float fXZM = rkAxis.X()*rkAxis.Z()*fOneMinusCos;
        float fYZM = rkAxis.Y()*rkAxis.Z()*fOneMinusCos;
        float fXSin = rkAxis.X()*fSin;
        float fYSin = rkAxis.Y()*fSin;
        float fZSin = rkAxis.Z()*fSin;
    
        m_afEntry[0] = fX2*fOneMinusCos+fCos;
        m_afEntry[1] = fXYM-fZSin;
        m_afEntry[2] = fXZM+fYSin;
        m_afEntry[3] = fXYM+fZSin;
        m_afEntry[4] = fY2*fOneMinusCos+fCos;
        m_afEntry[5] = fYZM-fXSin;
        m_afEntry[6] = fXZM-fYSin;
        m_afEntry[7] = fYZM+fXSin;
        m_afEntry[8] = fZ2*fOneMinusCos+fCos;
    }

    /** create a tensor product U*V^T
     * @param rkU, U-Vector
     * @param rkV, V-Vector
     */
    public void MakeTensorProduct (Vector3f rkU, Vector3f rkV)
    {
        m_afEntry[0] = rkU.X()*rkV.X();
        m_afEntry[1] = rkU.X()*rkV.Y();
        m_afEntry[2] = rkU.X()*rkV.Z();
        m_afEntry[3] = rkU.Y()*rkV.X();
        m_afEntry[4] = rkU.Y()*rkV.Y();
        m_afEntry[5] = rkU.Y()*rkV.Z();
        m_afEntry[6] = rkU.Z()*rkV.X();
        m_afEntry[7] = rkU.Z()*rkV.Y();
        m_afEntry[8] = rkU.Z()*rkV.Z();
    }

    /** Get member access
     * @param iRow, row-value
     * @param iCol, column-value
     * @return matrix[iRow*3 + iCol]
     */
    public final float GetData( int iRow, int iCol )
    {
        return m_afEntry[iRow*3 + iCol];
    }

    
    /** copy constructor
     * @param rkM, matrix to copy.
     */
    public void SetData ( Matrix3f rkM)
    {
        m_afEntry[0] = rkM.m_afEntry[0];
        m_afEntry[1] = rkM.m_afEntry[1];
        m_afEntry[2] = rkM.m_afEntry[2];
        m_afEntry[3] = rkM.m_afEntry[3];
        m_afEntry[4] = rkM.m_afEntry[4];
        m_afEntry[5] = rkM.m_afEntry[5];
        m_afEntry[6] = rkM.m_afEntry[6];
        m_afEntry[7] = rkM.m_afEntry[7];
        m_afEntry[8] = rkM.m_afEntry[8];
    }

    /** Set member access
     * @param iRow, row-value
     * @param iCol, column-value
     * @param fValue, matrix[iIndex] new value
     */
    public final void SetData( int iRow, int iCol, float fValue )
    {
        m_afEntry[iRow*3 + iCol] = fValue;
    }

    /** input Mrc is in row r, column c.
     * @param fM00, matrix[0] entry
     * @param fM01, matrix[1] entry
     * @param fM02, matrix[2] entry
     * @param fM10, matrix[3] entry
     * @param fM11, matrix[4] entry
     * @param fM12, matrix[5] entry
     * @param fM20, matrix[6] entry
     * @param fM21, matrix[7] entry
     * @param fM22, matrix[8] entry
     */
    public final void SetData (float fM00, float fM01, float fM02,
            float fM10, float fM11, float fM12,
            float fM20, float fM21, float fM22)
    {
        m_afEntry[0] = fM00;
        m_afEntry[1] = fM01;
        m_afEntry[2] = fM02;
        m_afEntry[3] = fM10;
        m_afEntry[4] = fM11;
        m_afEntry[5] = fM12;
        m_afEntry[6] = fM20;
        m_afEntry[7] = fM21;
        m_afEntry[8] = fM22;
    }

    
    /** Get member access
     * @param iIndex, matrix index
     * @return matrix[iIndex]
     */
    public final float GetData( int iIndex )
    {
        return m_afEntry[iIndex];
    }

    /** Set member access
     * @param iIndex, matrix index
     * @param fValue, matrix[iIndex] new value
     */
    public final void SetData( int iIndex, float fValue )
    {
        m_afEntry[iIndex] = fValue;
    }

    /** Set member access
     * @param iRow, row to set
     * @param rkV, new row vector values
     */
    public void SetRow (int iRow, Vector3f rkV)
    {
        int i0 = 3*iRow, i1 = i0+1, i2 = i1+1;
        m_afEntry[i0] = rkV.X();
        m_afEntry[i1] = rkV.Y();
        m_afEntry[i2] = rkV.Z();
    }

    /** Get member access
     * @param iRow, row to get
     * @return row vector values
     */
    public void GetRow (int iRow, Vector3f kResult)
    {
        int i0 = 3*iRow, i1 = i0+1, i2 = i1+1;
        kResult.SetData(m_afEntry[i0],m_afEntry[i1],m_afEntry[i2]);
    }

    /** Set member access
     * @param iCol, column to set
     * @param rkV, new column vector values
     */
    public void SetColumn (int iCol, Vector3f rkV)
    {
        m_afEntry[iCol] = rkV.X();
        m_afEntry[iCol+3] = rkV.Y();
        m_afEntry[iCol+6] = rkV.Z();
    }

    /** Get member access
     * @param iCol, column to get
     * @return column vector values
     */
    public void GetColumn (int iCol, Vector3f kResult)
    {
        kResult.SetData(m_afEntry[iCol],m_afEntry[iCol+3],m_afEntry[iCol+6]);
    }

    /** Get member access. Copies matrix into input array.
     * @param afData, copy matrix into array.
     */
    public void GetData( float[] afData )
    {
        afData[0] = m_afEntry[0];
        afData[1] = m_afEntry[1];
        afData[2] = m_afEntry[2];
        afData[3] = m_afEntry[3];
        afData[4] = m_afEntry[4];
        afData[5] = m_afEntry[5];
        afData[6] = m_afEntry[6];
        afData[7] = m_afEntry[7];
        afData[8] = m_afEntry[8];
    }

    /** Get member access. Copies matrix into input array in Column-Major order.
     * @param afData, copy matrix into array.
     */
    public void GetColumnMajor (float[] afCMajor)
    {
        afCMajor[0] = m_afEntry[0];
        afCMajor[1] = m_afEntry[3];
        afCMajor[2] = m_afEntry[6];
        afCMajor[3] = m_afEntry[1];
        afCMajor[4] = m_afEntry[4];
        afCMajor[5] = m_afEntry[7];
        afCMajor[6] = m_afEntry[2];
        afCMajor[7] = m_afEntry[5];
        afCMajor[8] = m_afEntry[8];
    }

    /** Multiply this matrix to the input matrix, return result, this is unchanged. 
     * @param rkM, input matrix
     * @return this*rkM
     */
    public Matrix3f mult ( Matrix3f rkM )
    {
        System.err.println("Matrix3f: mult 1");
        return new Matrix3f(
                            m_afEntry[0]*rkM.m_afEntry[0] +
                            m_afEntry[1]*rkM.m_afEntry[3] +
                            m_afEntry[2]*rkM.m_afEntry[6],
                            
                            m_afEntry[0]*rkM.m_afEntry[1] +
                            m_afEntry[1]*rkM.m_afEntry[4] +
                            m_afEntry[2]*rkM.m_afEntry[7],
                            
                            m_afEntry[0]*rkM.m_afEntry[2] +
                            m_afEntry[1]*rkM.m_afEntry[5] +
                            m_afEntry[2]*rkM.m_afEntry[8],
                        
                            m_afEntry[3]*rkM.m_afEntry[0] +
                            m_afEntry[4]*rkM.m_afEntry[3] +
                            m_afEntry[5]*rkM.m_afEntry[6],

                            m_afEntry[3]*rkM.m_afEntry[1] +
                            m_afEntry[4]*rkM.m_afEntry[4] +
                            m_afEntry[5]*rkM.m_afEntry[7],

                            m_afEntry[3]*rkM.m_afEntry[2] +
                            m_afEntry[4]*rkM.m_afEntry[5] +
                            m_afEntry[5]*rkM.m_afEntry[8],

                            m_afEntry[6]*rkM.m_afEntry[0] +
                            m_afEntry[7]*rkM.m_afEntry[3] +
                            m_afEntry[8]*rkM.m_afEntry[6],

                            m_afEntry[6]*rkM.m_afEntry[1] +
                            m_afEntry[7]*rkM.m_afEntry[4] +
                            m_afEntry[8]*rkM.m_afEntry[7],

                            m_afEntry[6]*rkM.m_afEntry[2] +
                            m_afEntry[7]*rkM.m_afEntry[5] +
                            m_afEntry[8]*rkM.m_afEntry[8]);
    }
    
    /** Multiply this matrix to the input matrix, return result, this is unchanged. 
     * @param rkM, input matrix
     * @return this*rkM
     */
    public void multEquals ( Matrix3f rkM )
    {
        SetData(
                            m_afEntry[0]*rkM.m_afEntry[0] +
                            m_afEntry[1]*rkM.m_afEntry[3] +
                            m_afEntry[2]*rkM.m_afEntry[6],
                            
                            m_afEntry[0]*rkM.m_afEntry[1] +
                            m_afEntry[1]*rkM.m_afEntry[4] +
                            m_afEntry[2]*rkM.m_afEntry[7],
                            
                            m_afEntry[0]*rkM.m_afEntry[2] +
                            m_afEntry[1]*rkM.m_afEntry[5] +
                            m_afEntry[2]*rkM.m_afEntry[8],
                        
                            m_afEntry[3]*rkM.m_afEntry[0] +
                            m_afEntry[4]*rkM.m_afEntry[3] +
                            m_afEntry[5]*rkM.m_afEntry[6],

                            m_afEntry[3]*rkM.m_afEntry[1] +
                            m_afEntry[4]*rkM.m_afEntry[4] +
                            m_afEntry[5]*rkM.m_afEntry[7],

                            m_afEntry[3]*rkM.m_afEntry[2] +
                            m_afEntry[4]*rkM.m_afEntry[5] +
                            m_afEntry[5]*rkM.m_afEntry[8],

                            m_afEntry[6]*rkM.m_afEntry[0] +
                            m_afEntry[7]*rkM.m_afEntry[3] +
                            m_afEntry[8]*rkM.m_afEntry[6],

                            m_afEntry[6]*rkM.m_afEntry[1] +
                            m_afEntry[7]*rkM.m_afEntry[4] +
                            m_afEntry[8]*rkM.m_afEntry[7],

                            m_afEntry[6]*rkM.m_afEntry[2] +
                            m_afEntry[7]*rkM.m_afEntry[5] +
                            m_afEntry[8]*rkM.m_afEntry[8]);
    }
    
    /** Multiply this matrix by the scalar input, this matrix is unchanged. 
     * @param fScalar, scalar value
     * @return this*fScalar
     */
    public Matrix3f scale (float fScalar)
    {
        System.err.println("Matrix3f: scale");
        return new Matrix3f(
                            fScalar*m_afEntry[0],
                            fScalar*m_afEntry[1],
                            fScalar*m_afEntry[2],
                            fScalar*m_afEntry[3],
                            fScalar*m_afEntry[4],
                            fScalar*m_afEntry[5],
                            fScalar*m_afEntry[6],
                            fScalar*m_afEntry[7],
                            fScalar*m_afEntry[8]);
    }

    /** matrix times vector
     * v^T * M
     * @param rkV, vector
     * @param rkM, matrix
     * @return v^T * M 
     */
    public static void mult(Vector3f rkV, Matrix3f rkM, Vector3f kResult)
    {
        kResult.SetData(
                        rkV.X()*rkM.m_afEntry[0] + rkV.Y()*rkM.m_afEntry[3] + rkV.Z()*rkM.m_afEntry[6],
                        rkV.X()*rkM.m_afEntry[1] + rkV.Y()*rkM.m_afEntry[4] + rkV.Z()*rkM.m_afEntry[7],
                        rkV.X()*rkM.m_afEntry[2] + rkV.Y()*rkM.m_afEntry[5] + rkV.Z()*rkM.m_afEntry[8]);
    }
    
    /** matrix times vector
     * v^T * M
     * @param rkV, vector
     * @param rkM, matrix
     * @return v^T * M 
     */
    public static void multEquals(Vector3f rkV, Matrix3f rkM)
    {
        float fX = rkV.X()*rkM.m_afEntry[0] + rkV.Y()*rkM.m_afEntry[3] + rkV.Z()*rkM.m_afEntry[6];
        float fY = rkV.X()*rkM.m_afEntry[1] + rkV.Y()*rkM.m_afEntry[4] + rkV.Z()*rkM.m_afEntry[7];
        float fZ = rkV.X()*rkM.m_afEntry[2] + rkV.Y()*rkM.m_afEntry[5] + rkV.Z()*rkM.m_afEntry[8];
        rkV.SetData(fX, fY, fZ);
    }

    /** matrix times vector
     * M * v
     * @param rkV, vector
     * @return M * v
     */
    public Vector3f mult (Vector3f rkV)  // M * v
    {
        System.err.println("Matrix3f: mult 2");
        return new Vector3f(
                            m_afEntry[0]*rkV.X() + m_afEntry[1]*rkV.Y() + m_afEntry[2]*rkV.Z(),
                            m_afEntry[3]*rkV.X() + m_afEntry[4]*rkV.Y() + m_afEntry[5]*rkV.Z(),
                            m_afEntry[6]*rkV.X() + m_afEntry[7]*rkV.Y() + m_afEntry[8]*rkV.Z());
    }

    /** matrix times vector
     * M * v
     * @param rkV, vector
     * @return M * v
     */
    public void mult (Vector3f rkV, Vector3f kResult)  // M * v
    {
        float fX = m_afEntry[0]*rkV.X() + m_afEntry[1]*rkV.Y() + m_afEntry[2]*rkV.Z();
        float fY = m_afEntry[3]*rkV.X() + m_afEntry[4]*rkV.Y() + m_afEntry[5]*rkV.Z();
        float fZ = m_afEntry[6]*rkV.X() + m_afEntry[7]*rkV.Y() + m_afEntry[8]*rkV.Z();
        kResult.SetData( fX, fY, fZ );
    }
    
    /** Transpose this matrix and return the result, this matrix is unchanged.
     * @return  M^T
     */
    public Matrix3f Transpose ()
    {
        System.err.println("Matrix3f: transpose");
        return new Matrix3f(
                            m_afEntry[0],
                            m_afEntry[3],
                            m_afEntry[6],
                            m_afEntry[1],
                            m_afEntry[4],
                            m_afEntry[7],
                            m_afEntry[2],
                            m_afEntry[5],
                            m_afEntry[8]);
    }

    /** Transpose this matrix and multiply by input, return the result, this
     * matrix is unchanged.
     * @param rkM, matrix
     * @return  this^T * M
     */
    public Matrix3f TransposeTimes (Matrix3f rkM)
    {
        // P = A^T*B
        System.err.println("Matrix3f: transpose times");
        return new Matrix3f(
                            m_afEntry[0]*rkM.m_afEntry[0] +
                            m_afEntry[3]*rkM.m_afEntry[3] +
                            m_afEntry[6]*rkM.m_afEntry[6],

                            m_afEntry[0]*rkM.m_afEntry[1] +
                            m_afEntry[3]*rkM.m_afEntry[4] +
                            m_afEntry[6]*rkM.m_afEntry[7],

                            m_afEntry[0]*rkM.m_afEntry[2] +
                            m_afEntry[3]*rkM.m_afEntry[5] +
                            m_afEntry[6]*rkM.m_afEntry[8],

                            m_afEntry[1]*rkM.m_afEntry[0] +
                            m_afEntry[4]*rkM.m_afEntry[3] +
                            m_afEntry[7]*rkM.m_afEntry[6],

                            m_afEntry[1]*rkM.m_afEntry[1] +
                            m_afEntry[4]*rkM.m_afEntry[4] +
                            m_afEntry[7]*rkM.m_afEntry[7],

                            m_afEntry[1]*rkM.m_afEntry[2] +
                            m_afEntry[4]*rkM.m_afEntry[5] +
                            m_afEntry[7]*rkM.m_afEntry[8],

                            m_afEntry[2]*rkM.m_afEntry[0] +
                            m_afEntry[5]*rkM.m_afEntry[3] +
                            m_afEntry[8]*rkM.m_afEntry[6],

                            m_afEntry[2]*rkM.m_afEntry[1] +
                            m_afEntry[5]*rkM.m_afEntry[4] +
                            m_afEntry[8]*rkM.m_afEntry[7],

                            m_afEntry[2]*rkM.m_afEntry[2] +
                            m_afEntry[5]*rkM.m_afEntry[5] +
                            m_afEntry[8]*rkM.m_afEntry[8]);
    }

    /** Multiply this matrix by transpose of the input matrix, return the
     * result, this matrix is unchanged.
     * @param rkM, matrix
     * @return this * M^T
     */
    public Matrix3f TimesTranspose (Matrix3f rkM)
    {
        // P = A*B^T
        System.err.println("Matrix3f: times transpose");
        return new Matrix3f(
                            m_afEntry[0]*rkM.m_afEntry[0] +
                            m_afEntry[1]*rkM.m_afEntry[1] +
                            m_afEntry[2]*rkM.m_afEntry[2],

                            m_afEntry[0]*rkM.m_afEntry[3] +
                            m_afEntry[1]*rkM.m_afEntry[4] +
                            m_afEntry[2]*rkM.m_afEntry[5],

                            m_afEntry[0]*rkM.m_afEntry[6] +
                            m_afEntry[1]*rkM.m_afEntry[7] +
                            m_afEntry[2]*rkM.m_afEntry[8],

                            m_afEntry[3]*rkM.m_afEntry[0] +
                            m_afEntry[4]*rkM.m_afEntry[1] +
                            m_afEntry[5]*rkM.m_afEntry[2],

                            m_afEntry[3]*rkM.m_afEntry[3] +
                            m_afEntry[4]*rkM.m_afEntry[4] +
                            m_afEntry[5]*rkM.m_afEntry[5],

                            m_afEntry[3]*rkM.m_afEntry[6] +
                            m_afEntry[4]*rkM.m_afEntry[7] +
                            m_afEntry[5]*rkM.m_afEntry[8],

                            m_afEntry[6]*rkM.m_afEntry[0] +
                            m_afEntry[7]*rkM.m_afEntry[1] +
                            m_afEntry[8]*rkM.m_afEntry[2],

                            m_afEntry[6]*rkM.m_afEntry[3] +
                            m_afEntry[7]*rkM.m_afEntry[4] +
                            m_afEntry[8]*rkM.m_afEntry[5],

                            m_afEntry[6]*rkM.m_afEntry[6] +
                            m_afEntry[7]*rkM.m_afEntry[7] +
                            m_afEntry[8]*rkM.m_afEntry[8]);
    }

    /** Invert a 3x3 using cofactors.  This is faster than using a generic
     * Gaussian elimination because of the loop overhead of such a method.
     * @return resulting matrix
     */
    public Matrix3f Inverse ()
    {

        System.err.println("Matrix3f: inverse");
        Matrix3f kInverse = new Matrix3f();

        kInverse.m_afEntry[0] =
            m_afEntry[4]*m_afEntry[8] - m_afEntry[5]*m_afEntry[7];
        kInverse.m_afEntry[1] =
            m_afEntry[2]*m_afEntry[7] - m_afEntry[1]*m_afEntry[8];
        kInverse.m_afEntry[2] =
            m_afEntry[1]*m_afEntry[5] - m_afEntry[2]*m_afEntry[4];
        kInverse.m_afEntry[3] =
            m_afEntry[5]*m_afEntry[6] - m_afEntry[3]*m_afEntry[8];
        kInverse.m_afEntry[4] =
            m_afEntry[0]*m_afEntry[8] - m_afEntry[2]*m_afEntry[6];
        kInverse.m_afEntry[5] =
            m_afEntry[2]*m_afEntry[3] - m_afEntry[0]*m_afEntry[5];
        kInverse.m_afEntry[6] =
            m_afEntry[3]*m_afEntry[7] - m_afEntry[4]*m_afEntry[6];
        kInverse.m_afEntry[7] =
            m_afEntry[1]*m_afEntry[6] - m_afEntry[0]*m_afEntry[7];
        kInverse.m_afEntry[8] =
            m_afEntry[0]*m_afEntry[4] - m_afEntry[1]*m_afEntry[3];

        float fDet =
            m_afEntry[0]*kInverse.m_afEntry[0] +
            m_afEntry[1]*kInverse.m_afEntry[3] +
            m_afEntry[2]*kInverse.m_afEntry[6];

        if (Math.abs(fDet) <= Mathf.ZERO_TOLERANCE)
        {
            System.err.println("Matrix3f: inverse 2");
            return new Matrix3f(Matrix3f.ZERO);
        }

        float fInvDet = ((float)1.0)/fDet;
        kInverse.m_afEntry[0] *= fInvDet;
        kInverse.m_afEntry[1] *= fInvDet;
        kInverse.m_afEntry[2] *= fInvDet;
        kInverse.m_afEntry[3] *= fInvDet;
        kInverse.m_afEntry[4] *= fInvDet;
        kInverse.m_afEntry[5] *= fInvDet;
        kInverse.m_afEntry[6] *= fInvDet;
        kInverse.m_afEntry[7] *= fInvDet;
        kInverse.m_afEntry[8] *= fInvDet;
        return kInverse;
    }

    /** Return adjoint of this matrix, this is unchanged.
     * @return adjoint of this matrix, this is unchanged.
     */
    public Matrix3f Adjoint ()
    {
        System.err.println("Matrix3f: adjoint");
        return new Matrix3f(
                            m_afEntry[4]*m_afEntry[8] - m_afEntry[5]*m_afEntry[7],
                            m_afEntry[2]*m_afEntry[7] - m_afEntry[1]*m_afEntry[8],
                            m_afEntry[1]*m_afEntry[5] - m_afEntry[2]*m_afEntry[4],
                            m_afEntry[5]*m_afEntry[6] - m_afEntry[3]*m_afEntry[8],
                            m_afEntry[0]*m_afEntry[8] - m_afEntry[2]*m_afEntry[6],
                            m_afEntry[2]*m_afEntry[3] - m_afEntry[0]*m_afEntry[5],
                            m_afEntry[3]*m_afEntry[7] - m_afEntry[4]*m_afEntry[6],
                            m_afEntry[1]*m_afEntry[6] - m_afEntry[0]*m_afEntry[7],
                            m_afEntry[0]*m_afEntry[4] - m_afEntry[1]*m_afEntry[3]);
    }

    /** 
     * Return the determinant of this matrix.
     * @return the determinant of this matrix.
     */
    public float Determinant ()
    {
        float fCo00 = m_afEntry[4]*m_afEntry[8] - m_afEntry[5]*m_afEntry[7];
        float fCo10 = m_afEntry[5]*m_afEntry[6] - m_afEntry[3]*m_afEntry[8];
        float fCo20 = m_afEntry[3]*m_afEntry[7] - m_afEntry[4]*m_afEntry[6];
        float fDet = m_afEntry[0]*fCo00 + m_afEntry[1]*fCo10 + m_afEntry[2]*fCo20;
        return fDet;
    }

    /**
     * Calculate and return u^T*M*v
     * @param rkU, u
     * @param rkV, v
     * @return u^T*M*v
     */
    public float QForm (Vector3f rkU,
                        Vector3f rkV)  // u^T*M*v
    {
        return rkU.Dot(this.mult(rkV));
    }

    /** Return the this matrix times the diagonal vector, this is unchanged:
     * @param rkDiag, diagonal vector
     * @return M*D
     */
    public Matrix3f TimesDiagonal (Vector3f rkDiag)  // M*D
    {
        System.err.println("Matrix3f: times diagonal");
        return new Matrix3f(
                            m_afEntry[0]*rkDiag.X(),m_afEntry[1]*rkDiag.Y(),m_afEntry[2]*rkDiag.Z(),
                            m_afEntry[3]*rkDiag.X(),m_afEntry[4]*rkDiag.Y(),m_afEntry[5]*rkDiag.Z(),
                            m_afEntry[6]*rkDiag.X(),m_afEntry[7]*rkDiag.Y(),m_afEntry[8]*rkDiag.Z());
    }
    
    /** Return the this matrix times the diagonal vector, this is unchanged:
     * @param rkDiag, diagonal vector
     * @return M*D
     */
    public void TimesDiagonalEquals (Vector3f rkDiag)  // M*D
    {
        SetData(
                            m_afEntry[0]*rkDiag.X(),m_afEntry[1]*rkDiag.Y(),m_afEntry[2]*rkDiag.Z(),
                            m_afEntry[3]*rkDiag.X(),m_afEntry[4]*rkDiag.Y(),m_afEntry[5]*rkDiag.Z(),
                            m_afEntry[6]*rkDiag.X(),m_afEntry[7]*rkDiag.Y(),m_afEntry[8]*rkDiag.Z());
    }

    /** Return the diagonal vector times this matrix, this is unchanged:
     * @param rkDiag, diagonal vector
     * @return D*M
     */
    public Matrix3f DiagonalTimes (Vector3f rkDiag)
    {
        System.err.println("Matrix3f: diagonal times");
        return new Matrix3f(
                            rkDiag.X()*m_afEntry[0],rkDiag.X()*m_afEntry[1],rkDiag.X()*m_afEntry[2],
                            rkDiag.Y()*m_afEntry[3],rkDiag.Y()*m_afEntry[4],rkDiag.Y()*m_afEntry[5],
                            rkDiag.Z()*m_afEntry[6],rkDiag.Z()*m_afEntry[7],rkDiag.Z()*m_afEntry[8]);
    }

    /** The matrix must be a rotation for these functions to be valid.  The
     * last function uses Gram-Schmidt orthonormalization applied to the
     * columns of the rotation matrix.  The angle must be in radians, not
     * degrees.
     */

    /** 
     * The matrix must be a rotation for these functions to be valid. 
     * Let (x,y,z) be the unit-length axis and let A be an angle of rotation.
     * The rotation matrix is R = I + sin(A)*P + (1-cos(A))*P^2 where
     * I is the identity and
     *
     *       +-        -+
     *   P = |  0 -z +y |
     *       | +z  0 -x |
     *       | -y +x  0 |
     *       +-        -+
     *
     * If A > 0, R represents a counterclockwise rotation about the axis in
     * the sense of looking from the tip of the axis vector towards the
     * origin.  Some algebra will show that
     *
     *   cos(A) = (trace(R)-1)/2  and  R - R^t = 2*sin(A)*P
     *
     * In the event that A = pi, R-R^t = 0 which prevents us from extracting
     * the axis through P.  Instead note that R = I+2*P^2 when A = pi, so
     * P^2 = (R-I)/2.  The diagonal entries of P^2 are x^2-1, y^2-1, and
     * z^2-1.  We can solve these for axis (x,y,z).  Because the angle is pi,
     * it does not matter which sign you choose on the square roots.
     * @param rkAxis, rotation axis
     * @return, rotation angle
     */
    public float ToAxisAngle (Vector3f rkAxis)
    {

        float fTrace = m_afEntry[0] + m_afEntry[4] + m_afEntry[8];
        float fCos = ((float)0.5)*(fTrace-(float)1.0);
        float rfAngle = (float)Math.acos(fCos);  // in [0,PI]

        if (rfAngle > (float)0.0)
        {
            if (rfAngle < Math.PI)
            {
                rkAxis.X( m_afEntry[7]-m_afEntry[5] );
                rkAxis.Y( m_afEntry[2]-m_afEntry[6] );
                rkAxis.Z( m_afEntry[3]-m_afEntry[1] );
                rkAxis.Normalize();
            }
            else
            {
                // angle is PI
                float fHalfInverse;
                if (m_afEntry[0] >= m_afEntry[4])
                {
                    // r00 >= r11
                    if (m_afEntry[0] >= m_afEntry[8])
                    {
                        // r00 is maximum diagonal term
                        rkAxis.X( (float)(((float)0.5)*Math.sqrt(m_afEntry[0] -
                                                                 m_afEntry[4] - m_afEntry[8] + (float)1.0)) );
                        fHalfInverse = ((float)0.5)/rkAxis.X();
                        rkAxis.Y( fHalfInverse*m_afEntry[1] );
                        rkAxis.Z( fHalfInverse*m_afEntry[2] );
                    }
                    else
                    {
                        // r22 is maximum diagonal term
                        rkAxis.Z( (float)(((float)0.5)*Math.sqrt(m_afEntry[8] -
                                                                 m_afEntry[0] - m_afEntry[4] + (float)1.0)) );
                        fHalfInverse = ((float)0.5)/rkAxis.Z();
                        rkAxis.X( fHalfInverse*m_afEntry[2] );
                        rkAxis.Y( fHalfInverse*m_afEntry[5] );
                    }
                }
                else
                {
                    // r11 > r00
                    if (m_afEntry[4] >= m_afEntry[8])
                    {
                        // r11 is maximum diagonal term
                        rkAxis.Y( (float)(((float)0.5)*Math.sqrt(m_afEntry[4] -
                                                                 m_afEntry[0] - m_afEntry[8] + (float)1.0)) );
                        fHalfInverse  = ((float)0.5)/rkAxis.Y();
                        rkAxis.X( fHalfInverse*m_afEntry[1] );
                        rkAxis.Z( fHalfInverse*m_afEntry[5] );
                    }
                    else
                    {
                        // r22 is maximum diagonal term
                        rkAxis.Z( (float)(((float)0.5)*Math.sqrt(m_afEntry[8] -
                                                                 m_afEntry[0] - m_afEntry[4] + (float)1.0)) );
                        fHalfInverse = ((float)0.5)/rkAxis.Z();
                        rkAxis.X( fHalfInverse*m_afEntry[2] );
                        rkAxis.Y( fHalfInverse*m_afEntry[5] );
                    }
                }
            }
        }
        else
        {
            // The angle is 0 and the matrix is the identity.  Any axis will
            // work, so just use the x-axis.
            rkAxis.X( (float)1.0 );
            rkAxis.Y( (float)0.0 );
            rkAxis.Z( (float)0.0 );
        }
        return rfAngle;
    }

    /** The matrix must be a rotation for these functions to be valid.  The
     * last function uses Gram-Schmidt orthonormalization applied to the
     * columns of the rotation matrix.  The angle must be in radians, not
     * degrees.
     *
     * Algorithm uses Gram-Schmidt orthogonalization.  If 'this' matrix is
     * M = [m0|m1|m2], then orthonormal output matrix is Q = [q0|q1|q2],
     *
     *   q0 = m0/|m0|
     *   q1 = (m1-(q0*m1)q0)/|m1-(q0*m1)q0|
     *   q2 = (m2-(q0*m2)q0-(q1*m2)q1)/|m2-(q0*m2)q0-(q1*m2)q1|
     *
     * where |V| indicates length of vector V and A*B indicates dot
     * product of vectors A and B.
     */
    public void Orthonormalize ()
    {
        // compute q0
        float fInvLength = Mathf.InvSqrt(m_afEntry[0]*m_afEntry[0] +
                                         m_afEntry[3]*m_afEntry[3] + m_afEntry[6]*m_afEntry[6]);

        m_afEntry[0] *= fInvLength;
        m_afEntry[3] *= fInvLength;
        m_afEntry[6] *= fInvLength;

        // compute q1
        float fDot0 = m_afEntry[0]*m_afEntry[1] + m_afEntry[3]*m_afEntry[4] +
            m_afEntry[6]*m_afEntry[7];

        m_afEntry[1] -= fDot0*m_afEntry[0];
        m_afEntry[4] -= fDot0*m_afEntry[3];
        m_afEntry[7] -= fDot0*m_afEntry[6];

        fInvLength = Mathf.InvSqrt(m_afEntry[1]*m_afEntry[1] +
                                   m_afEntry[4]*m_afEntry[4] + m_afEntry[7]*m_afEntry[7]);

        m_afEntry[1] *= fInvLength;
        m_afEntry[4] *= fInvLength;
        m_afEntry[7] *= fInvLength;

        // compute q2
        float fDot1 = m_afEntry[1]*m_afEntry[2] + m_afEntry[4]*m_afEntry[5] +
            m_afEntry[7]*m_afEntry[8];

        fDot0 = m_afEntry[0]*m_afEntry[2] + m_afEntry[3]*m_afEntry[5] +
            m_afEntry[6]*m_afEntry[8];

        m_afEntry[2] -= fDot0*m_afEntry[0] + fDot1*m_afEntry[1];
        m_afEntry[5] -= fDot0*m_afEntry[3] + fDot1*m_afEntry[4];
        m_afEntry[8] -= fDot0*m_afEntry[6] + fDot1*m_afEntry[7];

        fInvLength = Mathf.InvSqrt(m_afEntry[2]*m_afEntry[2] +
                                   m_afEntry[5]*m_afEntry[5] + m_afEntry[8]*m_afEntry[8]);

        m_afEntry[2] *= fInvLength;
        m_afEntry[5] *= fInvLength;
        m_afEntry[8] *= fInvLength;
    }


    public static boolean EigenDecomposition (Matrix3f rkRot, Matrix3f rkDiag)
    {
        // Factor M = R*D*R^T.  The columns of R are the eigenvectors.  The
        // diagonal entries of D are the corresponding eigenvalues.

        float[] afDiag = new float[3];
        float[] afSubd = new float[2];

        //rkRot = *this;
        boolean bReflection = Matrix3f.Tridiagonalize(rkRot,afDiag,afSubd);
        boolean bConverged = Matrix3f.QLAlgorithm(rkRot,afDiag,afSubd);
        if ( !bConverged )
        {
            return false;
        }

        // (insertion) sort eigenvalues in increasing order, d0 <= d1 <= d2
        int i;
        float fSave;
        
        if (afDiag[1] < afDiag[0])
        {
            // swap d0 and d1
            fSave = afDiag[0];
            afDiag[0] = afDiag[1];
            afDiag[1] = fSave;
            
            // swap V0 and V1
            for (i = 0; i < 3; i++)
            {
                fSave = rkRot.GetData(i,0);
                rkRot.SetData(i,0, rkRot.GetData(i,1));
                rkRot.SetData(i,1, fSave);
            }
            bReflection = !bReflection;
        }

        if (afDiag[2] < afDiag[1])
        {
            // swap d1 and d2
            fSave = afDiag[1];
            afDiag[1] = afDiag[2];
            afDiag[2] = fSave;
            
            // swap V1 and V2
            for (i = 0; i < 3; i++)
            {
                fSave = rkRot.GetData(i,1);
                rkRot.SetData(i,1, rkRot.GetData(i,2));
                rkRot.SetData(i,2, fSave);
            }
            bReflection = !bReflection;
        }
        
        if (afDiag[1] < afDiag[0])
        {
            // swap d0 and d1
            fSave = afDiag[0];
            afDiag[0] = afDiag[1];
            afDiag[1] = fSave;
            
            // swap V0 and V1
            for (i = 0; i < 3; i++)
            {
                fSave = rkRot.GetData(i,0);
                rkRot.SetData(i,0, rkRot.GetData(i,1));
                rkRot.SetData(i,1, fSave);
            }
            bReflection = !bReflection;
        }

        rkDiag.MakeDiagonal(afDiag[0],afDiag[1],afDiag[2]);

        if (bReflection)
        {
            // The orthogonal transformation that diagonalizes M is a reflection.
            // Make the eigenvectors a right--handed system by changing sign on
            // the last column.
            rkRot.SetData(0,2, -rkRot.GetData(0,2));
            rkRot.SetData(1,2, -rkRot.GetData(1,2));
            rkRot.SetData(2,2, -rkRot.GetData(2,2));
        }
        
        afDiag = null;
        afSubd = null;
        return true;
    }

    private static boolean Tridiagonalize (Matrix3f rkRot, float[] afDiag, float[] afSubd)
    {
        // Householder reduction T = Q^t M Q
        //   Input:   
        //     mat, symmetric 3x3 matrix M
        //   Output:  
        //     mat, orthogonal matrix Q (a reflection)
        //     diag, diagonal entries of T
        //     subd, subdiagonal entries of T (T is symmetric)
        
        float fM00 = rkRot.m_afEntry[0];
        float fM01 = rkRot.m_afEntry[1];
        float fM02 = rkRot.m_afEntry[2];
        float fM11 = rkRot.m_afEntry[4];
        float fM12 = rkRot.m_afEntry[5];
        float fM22 = rkRot.m_afEntry[8];
        
        afDiag[0] = fM00;
        if (Math.abs(fM02) >= Mathf.ZERO_TOLERANCE)
        {
            afSubd[0] = (float)Math.sqrt(fM01*fM01+fM02*fM02);
            float fInvLength = (1.0f)/afSubd[0];
            fM01 *= fInvLength;
            fM02 *= fInvLength;
            float fTmp = (2.0f)*fM01*fM12+fM02*(fM22-fM11);
            afDiag[1] = fM11+fM02*fTmp;
            afDiag[2] = fM22-fM02*fTmp;
            afSubd[1] = fM12-fM01*fTmp;

            rkRot.m_afEntry[0] = 1.0f;
            rkRot.m_afEntry[1] = 0.0f;
            rkRot.m_afEntry[2] = 0.0f;
            rkRot.m_afEntry[3] = 0.0f;
            rkRot.m_afEntry[4] = fM01;
            rkRot.m_afEntry[5] = fM02;
            rkRot.m_afEntry[6] = 0.0f;
            rkRot.m_afEntry[7] = fM02;
            rkRot.m_afEntry[8] = -fM01;
            return true;
        }
        else
        {
            afDiag[1] = fM11;
            afDiag[2] = fM22;
            afSubd[0] = fM01;
            afSubd[1] = fM12;
            
            rkRot.m_afEntry[0] = 1.0f;
            rkRot.m_afEntry[1] = 0.0f;
            rkRot.m_afEntry[2] = 0.0f;
            rkRot.m_afEntry[3] = 0.0f;
            rkRot.m_afEntry[4] = 1.0f;
            rkRot.m_afEntry[5] = 0.0f;
            rkRot.m_afEntry[6] = 0.0f;
            rkRot.m_afEntry[7] = 0.0f;
            rkRot.m_afEntry[8] = 1.0f;
            return false;
        }
    }

    private static boolean QLAlgorithm (Matrix3f rkRot, float[] afDiag, float[] afSubd)
    {
        // This is an implementation of the symmetric QR algorithm from the book
        // "Matrix Computations" by Gene H. Golub and Charles F. Van Loan, second
        // edition.  The algorithm is 8.2.3.  The implementation has a slight
        // variation to actually make it a QL algorithm, and it traps the case
        // when either of the subdiagonal terms s0 or s1 is zero and reduces the
        // 2-by-2 subblock directly.
        
        final int iMax = 32;
        for (int i = 0; i < iMax; i++)
        {
            float fSum, fDiff, fDiscr, fEValue0, fEValue1, fCos, fSin, fTmp;
            int iRow;

            fSum = Math.abs(afDiag[0]) + Math.abs(afDiag[1]);
            if (Math.abs(afSubd[0]) + fSum == fSum)
            {
                // The matrix is effectively
                //       +-        -+
                //   M = | d0  0  0 |
                //       | 0  d1 s1 |
                //       | 0  s1 d2 |
                //       +-        -+
                
                // Compute the eigenvalues as roots of a quadratic equation.
                fSum = afDiag[1] + afDiag[2];
                fDiff = afDiag[1] - afDiag[2];
                fDiscr = (float)Math.sqrt(fDiff*fDiff +
                                          (4.0f)*afSubd[1]*afSubd[1]);
                fEValue0 = (0.5f)*(fSum - fDiscr);
                fEValue1 = (0.5f)*(fSum + fDiscr);
                
                // Compute the Givens rotation.
                if (fDiff >= 0.0f)
                {
                    fCos = afSubd[1];
                    fSin = afDiag[1] - fEValue0;
                }
                else
                {
                    fCos = afDiag[2] - fEValue0;
                    fSin = afSubd[1];
                }
                fTmp = Mathf.InvSqrt(fCos*fCos + fSin*fSin);
                fCos *= fTmp;
                fSin *= fTmp;
                
                // Postmultiply the current orthogonal matrix with the Givens
                // rotation.
                for (iRow = 0; iRow < 3; iRow++)
                {
                    fTmp = rkRot.m_afEntry[2+3*iRow];
                    rkRot.m_afEntry[2+3*iRow] = fSin*rkRot.m_afEntry[1+3*iRow] + fCos*fTmp;
                    rkRot.m_afEntry[1+3*iRow] = fCos*rkRot.m_afEntry[1+3*iRow] - fSin*fTmp;
                }
                
                // Update the tridiagonal matrix.
                afDiag[1] = fEValue0;
                afDiag[2] = fEValue1;
                afSubd[0] = 0.0f;
                afSubd[1] = 0.0f;
                return true;
            }

            fSum = Math.abs(afDiag[1]) + Math.abs(afDiag[2]);
            if (Math.abs(afSubd[1]) + fSum == fSum)
            {
                // The matrix is effectively
                //       +-         -+
                //   M = | d0  s0  0 |
                //       | s0  d1  0 |
                //       | 0   0  d2 |
                //       +-         -+
                
                // Compute the eigenvalues as roots of a quadratic equation.
                fSum = afDiag[0] + afDiag[1];
                fDiff = afDiag[0] - afDiag[1];
                fDiscr = (float)Math.sqrt(fDiff*fDiff +
                                          (4.0f)*afSubd[0]*afSubd[0]);
                fEValue0 = (0.5f)*(fSum - fDiscr);
                fEValue1 = (0.5f)*(fSum + fDiscr);
                
                // Compute the Givens rotation.
                if (fDiff >= 0.0f)
                {
                    fCos = afSubd[0];
                    fSin = afDiag[0] - fEValue0;
                }
                else
                {
                    fCos = afDiag[1] - fEValue0;
                    fSin = afSubd[0];
                }
                fTmp = Mathf.InvSqrt(fCos*fCos + fSin*fSin);
                fCos *= fTmp;
                fSin *= fTmp;
                
                // Postmultiply the current orthogonal matrix with the Givens
                // rotation.
                for (iRow = 0; iRow < 3; iRow++)
                {
                    fTmp = rkRot.m_afEntry[1+3*iRow];
                    rkRot.m_afEntry[1+3*iRow] = fSin*rkRot.m_afEntry[0+3*iRow] + fCos*fTmp;
                    rkRot.m_afEntry[0+3*iRow] = fCos*rkRot.m_afEntry[0+3*iRow] - fSin*fTmp;
                }
                
                // Update the tridiagonal matrix.
                afDiag[0] = fEValue0;
                afDiag[1] = fEValue1;
                afSubd[0] = 0.0f;
                afSubd[1] = 0.0f;
                return true;
            }

            // The matrix is
            //       +-        -+
            //   M = | d0 s0  0 |
            //       | s0 d1 s1 |
            //       | 0  s1 d2 |
            //       +-        -+
            
            // Set up the parameters for the first pass of the QL step.  The
            // value for A is the difference between diagonal term D[2] and the
            // implicit shift suggested by Wilkinson.
            float fRatio = (afDiag[1]-afDiag[0])/((2.0f)*afSubd[0]);
            float fRoot = (float)Math.sqrt(1.0f + fRatio*fRatio);
            float fB = afSubd[1];
            float fA = afDiag[2] - afDiag[0];
            if (fRatio >= 0.0)
            {
                fA += afSubd[0]/(fRatio + fRoot);
            }
            else
            {
                fA += afSubd[0]/(fRatio - fRoot);
            }
            
            // Compute the Givens rotation for the first pass.
            if (Math.abs(fB) >= Math.abs(fA))
            {
                fRatio = fA/fB;
                fSin = Mathf.InvSqrt(1.0f + fRatio*fRatio);
                fCos = fRatio*fSin;
            }
            else
            {
                fRatio = fB/fA;
                fCos = Mathf.InvSqrt(1.0f + fRatio*fRatio);
                fSin = fRatio*fCos;
            }
            
            // Postmultiply the current orthogonal matrix with the Givens
            // rotation.
            for (iRow = 0; iRow < 3; iRow++)
            {
                fTmp = rkRot.m_afEntry[2+3*iRow];
                rkRot.m_afEntry[2+3*iRow] = fSin*rkRot.m_afEntry[1+3*iRow]+fCos*fTmp;
                rkRot.m_afEntry[1+3*iRow] = fCos*rkRot.m_afEntry[1+3*iRow]-fSin*fTmp;
            }
            
            // Set up the parameters for the second pass of the QL step.  The
            // values tmp0 and tmp1 are required to fully update the tridiagonal
            // matrix at the end of the second pass.
            float fTmp0 = (afDiag[1] - afDiag[2])*fSin +
                (2.0f)*afSubd[1]*fCos;
            float fTmp1 = fCos*afSubd[0];
            fB = fSin*afSubd[0];
            fA = fCos*fTmp0 - afSubd[1];
            fTmp0 *= fSin;
            
            // Compute the Givens rotation for the second pass.  The subdiagonal
            // term S[1] in the tridiagonal matrix is updated at this time.
            if (Math.abs(fB) >= Math.abs(fA))
            {
                fRatio = fA/fB;
                fRoot = (float)Math.sqrt(1.0f + fRatio*fRatio);
                afSubd[1] = fB*fRoot;
                fSin = (1.0f)/fRoot;
                fCos = fRatio*fSin;
            }
            else
            {
                fRatio = fB/fA;
                fRoot = (float)Math.sqrt(1.0f + fRatio*fRatio);
                afSubd[1] = fA*fRoot;
                fCos = (1.0f)/fRoot;
                fSin = fRatio*fCos;
            }
            
            // Postmultiply the current orthogonal matrix with the Givens
            // rotation.
            for (iRow = 0; iRow < 3; iRow++)
            {
                fTmp = rkRot.m_afEntry[1+3*iRow];
                rkRot.m_afEntry[1+3*iRow] = fSin*rkRot.m_afEntry[0+3*iRow]+fCos*fTmp;
                rkRot.m_afEntry[0+3*iRow] = fCos*rkRot.m_afEntry[0+3*iRow]-fSin*fTmp;
            }

            // Complete the update of the tridiagonal matrix.
            float fTmp2 = afDiag[1] - fTmp0;
            afDiag[2] += fTmp0;
            fTmp0 = (afDiag[0] - fTmp2)*fSin + (2.0f)*fTmp1*fCos;
            afSubd[0] = fCos*fTmp0 - fTmp1;
            fTmp0 *= fSin;
            afDiag[1] = fTmp2 + fTmp0;
            afDiag[0] -= fTmp0;
        }
        return false;
    }

    /** Matrix data: */
    private float[] m_afEntry = new float[9];
}
