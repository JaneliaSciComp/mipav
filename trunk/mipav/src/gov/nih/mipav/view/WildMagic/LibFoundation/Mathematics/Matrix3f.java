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
// Version: 4.0.2 (2006/09/05)

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
    /** special matrices*/
    public static final Matrix3f ZERO = new Matrix3f(0.0f,0.0f,0.0f,
                                                     0.0f,0.0f,0.0f,
                                                     0.0f,0.0f,0.0f);
    public static final Matrix3f IDENTITY = new Matrix3f(1.0f,0.0f,0.0f,
                                                         0.0f,1.0f,0.0f,
                                                         0.0f,0.0f,1.0f);;

    public Matrix3f ()
    {
        MakeZero();
    }

    /** If bZero is true, create the zero matrix.  Otherwise, create the
     * identity matrix.
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


    /** copy constructor */
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


    // input Mrc is in row r, column c.
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


    /** create a diagonal matrix */
    public Matrix3f (float fM00, float fM11, float fM22)
    {
        MakeDiagonal(fM00,fM11,fM22);
    }


    /** Create rotation matrices (positive angle - counterclockwise).  The
     * angle must be in radians, not degrees.
     */
    public Matrix3f (Vector3f rkAxis, float fAngle)
    {
        FromAxisAngle(rkAxis,fAngle);
    }

    /** create a tensor product U*V^T */
    public Matrix3f (Vector3f rkU, Vector3f rkV)
    {
        MakeTensorProduct(rkU,rkV);
    }


    // create various matrices
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

    // member access
    public float GetData( int iRow, int iCol )
    {
        return m_afEntry[iRow*3 + iCol];
    }

    public float GetData( int iIndex )
    {
        return m_afEntry[iIndex];
    }

    public void SetData( int iIndex, float fValue )
    {
        m_afEntry[iIndex] = fValue;
    }

    public void SetRow (int iRow, Vector3f rkV)
    {
        int i0 = 3*iRow, i1 = i0+1, i2 = i1+1;
        m_afEntry[i0] = rkV.X();
        m_afEntry[i1] = rkV.Y();
        m_afEntry[i2] = rkV.Z();
    }

    public Vector3f GetRow (int iRow)
    {
        int i0 = 3*iRow, i1 = i0+1, i2 = i1+1;
        return new Vector3f(m_afEntry[i0],m_afEntry[i1],m_afEntry[i2]);
    }

    public void SetColumn (int iCol, Vector3f rkV)
    {
        m_afEntry[iCol] = rkV.X();
        m_afEntry[iCol+3] = rkV.Y();
        m_afEntry[iCol+6] = rkV.Z();
    }

    public Vector3f GetColumn (int iCol)
    {
        return new Vector3f(m_afEntry[iCol],m_afEntry[iCol+3],m_afEntry[iCol+6]);
    }

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

    // arithmetic operations
    public Matrix3f mult ( Matrix3f rkM )
    {
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
    
    public Matrix3f scale (float fScalar)
    {
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
     */
    public static Vector3f mult(Vector3f rkV, Matrix3f rkM)
    {
        return new Vector3f(
                        rkV.X()*rkM.m_afEntry[0] + rkV.Y()*rkM.m_afEntry[3] + rkV.Z()*rkM.m_afEntry[6],
                        rkV.X()*rkM.m_afEntry[1] + rkV.Y()*rkM.m_afEntry[4] + rkV.Z()*rkM.m_afEntry[7],
                        rkV.X()*rkM.m_afEntry[2] + rkV.Y()*rkM.m_afEntry[5] + rkV.Z()*rkM.m_afEntry[8]);
    }

    /** matrix times vector M * v */
    public Vector3f mult (Vector3f rkV)  // M * v
    {
        return new Vector3f(
                            m_afEntry[0]*rkV.X() + m_afEntry[1]*rkV.Y() + m_afEntry[2]*rkV.Z(),
                            m_afEntry[3]*rkV.X() + m_afEntry[4]*rkV.Y() + m_afEntry[5]*rkV.Z(),
                            m_afEntry[6]*rkV.X() + m_afEntry[7]*rkV.Y() + m_afEntry[8]*rkV.Z());
    }

    // other operations
    public Matrix3f Transpose ()  // M^T
    {
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

    public Matrix3f TransposeTimes (Matrix3f rkM)  // this^T * M
    {
        // P = A^T*B
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

    public Matrix3f TimesTranspose (Matrix3f rkM)  // this * M^T
    {
        // P = A*B^T
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
     */
    public Matrix3f Inverse ()
    {

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

    public Matrix3f Adjoint ()
    {
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

    public float Determinant ()
    {
        float fCo00 = m_afEntry[4]*m_afEntry[8] - m_afEntry[5]*m_afEntry[7];
        float fCo10 = m_afEntry[5]*m_afEntry[6] - m_afEntry[3]*m_afEntry[8];
        float fCo20 = m_afEntry[3]*m_afEntry[7] - m_afEntry[4]*m_afEntry[6];
        float fDet = m_afEntry[0]*fCo00 + m_afEntry[1]*fCo10 + m_afEntry[2]*fCo20;
        return fDet;
    }

    public float QForm (Vector3f rkU,
                        Vector3f rkV)  // u^T*M*v
    {
        return rkU.Dot(this.mult(rkV));
    }

    public Matrix3f TimesDiagonal (Vector3f rkDiag)  // M*D
    {
        return new Matrix3f(
                            m_afEntry[0]*rkDiag.X(),m_afEntry[1]*rkDiag.Y(),m_afEntry[2]*rkDiag.Z(),
                            m_afEntry[3]*rkDiag.X(),m_afEntry[4]*rkDiag.Y(),m_afEntry[5]*rkDiag.Z(),
                            m_afEntry[6]*rkDiag.X(),m_afEntry[7]*rkDiag.Y(),m_afEntry[8]*rkDiag.Z());
    }

    public Matrix3f DiagonalTimes (Vector3f rkDiag)   // D*M
    {
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

    private float[] m_afEntry = new float[9];
}
