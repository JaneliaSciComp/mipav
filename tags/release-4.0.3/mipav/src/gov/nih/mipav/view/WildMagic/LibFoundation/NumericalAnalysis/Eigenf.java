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
public class Eigenf
{
    public Eigenf (int iSize)
    {
        m_kMat = new GMatrixf(iSize,iSize);
        assert(iSize >= 2);
        m_iSize = iSize;
        m_afDiag = new float[m_iSize];
        m_afSubd = new float[m_iSize];
        m_bIsRotation = false;
    }

    //public Eigenf (const Matrix2<Real>& rkM);
    //public Eigenf (const Matrix3<Real>& rkM);
    //public Eigenf (const GMatrix<Real>& rkM);
    public void dispose ()
    {
        m_kMat.dispose();
        m_afSubd = null;
        m_afDiag = null;
    }

    public void SetData( int iRow, int iCol, float fValue )
    {
        m_kMat.Set(iRow,iCol,fValue);
    }

    // set the matrix for eigensolving
//     Real& operator() (int iRow, int iCol);
//     Eigen& operator= (const Matrix2<Real>& rkM);
//     Eigen& operator= (const Matrix3<Real>& rkM);
//     Eigen& operator= (const GMatrix<Real>& rkM);

    // Get the eigenresults (eigenvectors are columns of eigenmatrix).  The
    // GetEigenvector calls involving Vector2 and Vector3 should only be
    // called if you know that the eigenmatrix is of the appropriate size.
    public float GetEigenvalue (int i)
    {
        return m_afDiag[i];
    }

    //const Real* GetEigenvalues () const;
    //void GetEigenvector (int i, Vector2<Real>& rkV) const;
    public void GetEigenvector (int i, Vector3f rkV)
    {
        assert(m_iSize == 3);
        if (m_iSize == 3)
        {
            for (int iRow = 0; iRow < m_iSize; iRow++)
            {
                rkV.SetData(iRow, m_kMat.Get(iRow,i) );
            }
        }
        else
        {
            rkV.SetData( Vector3f.ZERO );
        }
    }

    //GVector<Real> GetEigenvector (int i) const;
    //const GMatrix<Real>& GetEigenvectors () const;

    // solve eigensystem
    //void EigenStuff2 ();
    //void EigenStuff3 ();
    //void EigenStuffN ();
    //void EigenStuff  ();

    // solve eigensystem, use decreasing sort on eigenvalues
    //void DecrSortEigenStuff2 ();
    //void DecrSortEigenStuff3 ();
    //void DecrSortEigenStuffN ();
    //void DecrSortEigenStuff  ();

    // solve eigensystem, use increasing sort on eigenvalues
    //void IncrSortEigenStuff2 ();
    public void IncrSortEigenStuff3 ()
    {
        Tridiagonal3();
        QLAlgorithm();
        IncreasingSort();
        GuaranteeRotation();
    }

    //void IncrSortEigenStuffN ();
    //void IncrSortEigenStuff  ();

   
    private int m_iSize;
    private GMatrixf m_kMat;
    private float[] m_afDiag;
    private float[] m_afSubd;

    // For odd size matrices, the Householder reduction involves an odd
    // number of reflections.  The product of these is a reflection.  The
    // QL algorithm uses rotations for further reductions.  The final
    // orthogonal matrix whose columns are the eigenvectors is a reflection,
    // so its determinant is -1.  For even size matrices, the Householder
    // reduction involves an even number of reflections whose product is a
    // rotation.  The final orthogonal matrix has determinant +1.  Many
    // algorithms that need an eigendecomposition want a rotation matrix.
    // We want to guarantee this is the case, so m_bRotation keeps track of
    // this.  The DecrSort and IncrSort further complicate the issue since
    // they swap columns of the orthogonal matrix, causing the matrix to
    // toggle between rotation and reflection.  The value m_bRotation must
    // be toggled accordingly.
    private boolean m_bIsRotation;
    private void GuaranteeRotation ()
    {
        if (!m_bIsRotation)
        {
            // change sign on the first column
            for (int iRow = 0; iRow < m_iSize; iRow++)
            {
                m_kMat.Set(iRow,0, -m_kMat.Get(iRow,0) );
            }
        }
    }

    // Householder reduction to tridiagonal form
    //private void Tridiagonal2 ();
    private void Tridiagonal3 ()
    {
        float fM00 = m_kMat.Get(0,0);
        float fM01 = m_kMat.Get(0,1);
        float fM02 = m_kMat.Get(0,2);
        float fM11 = m_kMat.Get(1,1);
        float fM12 = m_kMat.Get(1,2);
        float fM22 = m_kMat.Get(2,2);

        m_afDiag[0] = fM00;
        m_afSubd[2] = 0.0f;
        if (Math.abs(fM02) > Mathf.ZERO_TOLERANCE)
        {
            float fLength = (float)Math.sqrt(fM01*fM01+fM02*fM02);
            float fInvLength = ((float)1.0)/fLength;
            fM01 *= fInvLength;
            fM02 *= fInvLength;
            float fQ = ((float)2.0)*fM01*fM12+fM02*(fM22-fM11);
            m_afDiag[1] = fM11+fM02*fQ;
            m_afDiag[2] = fM22-fM02*fQ;
            m_afSubd[0] = fLength;
            m_afSubd[1] = fM12-fM01*fQ;
            m_kMat.Set(0,0, (float)1.0);
            m_kMat.Set(0,1, (float)0.0);
            m_kMat.Set(0,2, (float)0.0);
            m_kMat.Set(1,0, (float)0.0);
            m_kMat.Set(1,1, fM01);
            m_kMat.Set(1,2, fM02);
            m_kMat.Set(2,0, (float)0.0);
            m_kMat.Set(2,1, fM02);
            m_kMat.Set(2,2, -fM01);
            m_bIsRotation = false;
        }
        else
        {
            m_afDiag[1] = fM11;
            m_afDiag[2] = fM22;
            m_afSubd[0] = fM01;
            m_afSubd[1] = fM12;
            m_kMat.Set(0,0, (float)1.0);
            m_kMat.Set(0,1, (float)0.0);
            m_kMat.Set(0,2, (float)0.0);
            m_kMat.Set(1,0, (float)0.0);
            m_kMat.Set(1,1, (float)1.0);
            m_kMat.Set(1,2, (float)0.0);
            m_kMat.Set(2,0, (float)0.0);
            m_kMat.Set(2,1, (float)0.0);
            m_kMat.Set(2,2, (float)1.0);
            m_bIsRotation = true;
        }
    }

    //private void TridiagonalN ();

    // QL algorithm with implicit shifting, applies to tridiagonal matrices
    private boolean QLAlgorithm ()
    {
        int iMaxIter = 32;

        for (int i0 = 0; i0 < m_iSize; i0++)
        {
            int i1;
            for (i1 = 0; i1 < iMaxIter; i1++)
            {
                int i2;
                for (i2 = i0; i2 <= m_iSize-2; i2++)
                {
                    float fTmp = Math.abs(m_afDiag[i2]) +
                        Math.abs(m_afDiag[i2+1]);

                    if (Math.abs(m_afSubd[i2]) + fTmp == fTmp)
                    {
                        break;
                    }
                }
                if (i2 == i0)
                {
                    break;
                }

                float fG = (m_afDiag[i0+1] - m_afDiag[i0])/(((float)2.0) *
                                                            m_afSubd[i0]);
                float fR = (float)Math.sqrt(fG*fG+1.0);
                if ( fG < (float)0.0 )
                {
                    fG = m_afDiag[i2]-m_afDiag[i0]+m_afSubd[i0]/(fG-fR);
                }
                else
                {
                    fG = m_afDiag[i2]-m_afDiag[i0]+m_afSubd[i0]/(fG+fR);
                }
                float fSin = (float)1.0, fCos = (float)1.0, fP = (float)0.0;
                for (int i3 = i2-1; i3 >= i0; i3--)
                {
                    float fF = fSin*m_afSubd[i3];
                    float fB = fCos*m_afSubd[i3];
                    if (Math.abs(fF) >= Math.abs(fG))
                    {
                        fCos = fG/fF;
                        fR = (float)Math.sqrt(fCos*fCos+1.0);
                        m_afSubd[i3+1] = fF*fR;
                        fSin = ((float)1.0)/fR;
                        fCos *= fSin;
                    }
                    else
                    {
                        fSin = fF/fG;
                        fR = (float)Math.sqrt(fSin*fSin+1.0);
                        m_afSubd[i3+1] = fG*fR;
                        fCos = ((float)1.0)/fR;
                        fSin *= fCos;
                    }
                    fG = m_afDiag[i3+1]-fP;
                    fR = (m_afDiag[i3]-fG)*fSin+((float)2.0)*fB*fCos;
                    fP = fSin*fR;
                    m_afDiag[i3+1] = fG+fP;
                    fG = fCos*fR-fB;

                    for (int i4 = 0; i4 < m_iSize; i4++)
                    {
                        fF = m_kMat.Get(i4,i3+1);
                        m_kMat.Set(i4,i3+1, fSin*m_kMat.Get(i4,i3)+fCos*fF);
                        m_kMat.Set(i4,i3, fCos*m_kMat.Get(i4,i3)-fSin*fF);
                    }
                }
                m_afDiag[i0] -= fP;
                m_afSubd[i0] = fG;
                m_afSubd[i2] = (float)0.0;
            }
            if (i1 == iMaxIter)
            {
                return false;
            }
        }

        return true;
    }


    // sort eigenvalues from largest to smallest
    //private void DecreasingSort ();

    // sort eigenvalues from smallest to largest
    private void IncreasingSort ()
    {
        // sort eigenvalues in increasing order, e[0] <= ... <= e[iSize-1]
        for (int i0 = 0, i1; i0 <= m_iSize-2; i0++)
        {
            // locate minimum eigenvalue
            i1 = i0;
            float fMin = m_afDiag[i1];
            int i2;
            for (i2 = i0+1; i2 < m_iSize; i2++)
            {
                if (m_afDiag[i2] < fMin)
                {
                    i1 = i2;
                    fMin = m_afDiag[i1];
                }
            }

            if (i1 != i0)
            {
                // swap eigenvalues
                m_afDiag[i1] = m_afDiag[i0];
                m_afDiag[i0] = fMin;

                // swap eigenvectors
                for (i2 = 0; i2 < m_iSize; i2++)
                {
                    float fTmp = m_kMat.Get(i2,i0);
                    m_kMat.Set(i2,i0, m_kMat.Get(i2,i1));
                    m_kMat.Set(i2,i1, fTmp);
                    m_bIsRotation = !m_bIsRotation;
                }
            }
        }
    }

}
