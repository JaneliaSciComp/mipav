// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Restricted Libraries source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4RestrictedLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.1 (2006/07/26)

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class Transformation
{
    // The transformation is Y = M*X+T, where M is a 3-by-3 matrix and T is
    // a translation vector.  In most cases, M = R, a rotation matrix, or
    // M = R*S, where R is a rotation matrix and S is a diagonal matrix
    // whose diagonal entries are positive scales.  To support modeling
    // packages that allow reflections and nonuniform scales, I now allow
    // the general transformation Y = M*X+T.  The vector X is transformed in
    // the "forward" direction to Y.  The "inverse" direction transforms Y
    // to X, namely X = M^{-1}*(Y-T) in the general case.  In the special
    // case of M = R*S, the inverse direction is X = S^{-1}*R^t*(Y-T).

    // Construction and destruction.  The default constructor produces the
    // identity transformation.  The default copy constructor is created by
    // the compiler, as is the default assignment operator.  The defaults are
    // consistent with the design of this class.
    public Transformation () {}

    // set the transformation to the identity
    public void MakeIdentity ()
    {
        m_kMatrix = new Matrix3f(Matrix3f.IDENTITY);
        m_kTranslate = new Vector3f(Vector3f.ZERO);
        m_kScale = new Vector3f(1.0f,1.0f,1.0f);
        m_bIsIdentity = true;
        m_bIsRSMatrix = true;
        m_bIsUniformScale = true;
    }


    // set the scales to 1
    public void MakeUnitScale ()
    {
        assert(m_bIsRSMatrix);

        m_kScale = new Vector3f(1.0f,1.0f,1.0f);
        m_bIsUniformScale = true;
    }


    // Hints about the structure of the transformation.  In the common case
    // of M = R*S, IsRSMatrix() returns true.
    public boolean IsIdentity ()
    {
        return m_bIsIdentity;
    }

    public boolean IsRSMatrix ()
    {
        return m_bIsRSMatrix;
    }

    public boolean IsUniformScale ()
    {
        return m_bIsRSMatrix && m_bIsUniformScale;
    }


    // Member access.
    // (1) The Set* functions set the is-identity hint to false.
    // (2) The SetRotate function sets the is-rsmatrix hint to true.  If this
    //     hint is false,  GetRotate fires an "assert" in debug mode.
    // (3) The SetMatrix function sets the is-rsmatrix and is-uniform-scale
    //     hints to false.
    // (4) The SetScale function sets the is-uniform-scale hint to false.
    //     The SetUniformScale function sets the is-uniform-scale hint to
    //     true.  If this hint is false, GetUniformScale fires an "assert" in
    //     debug mode.
    public void SetRotate (Matrix3f rkRotate)
    {
        m_kMatrix = rkRotate;
        m_bIsIdentity = false;
        m_bIsRSMatrix = true;
    }

    public Matrix3f GetRotate ()
    {
        assert(m_bIsRSMatrix);
        return m_kMatrix;
    }

    public void SetMatrix (Matrix3f rkMatrix)
    {
        m_kMatrix = rkMatrix;
        m_bIsIdentity = false;
        m_bIsRSMatrix = false;
        m_bIsUniformScale = false;
    }

    public Matrix3f GetMatrix ()
    {
        return m_kMatrix;
    }

    public void SetTranslate (Vector3f rkTranslate)
    {
        m_kTranslate = rkTranslate;
        m_bIsIdentity = false;
    }

    public Vector3f GetTranslate ()
    {
        return m_kTranslate;
    }

    public void SetScale (Vector3f rkScale)
    {
        assert(m_bIsRSMatrix && rkScale.X() != 0.0f && rkScale.Y() != 0.0f
               && rkScale.Z() != 0.0f);

        m_kScale = rkScale;
        m_bIsIdentity = false;
        m_bIsUniformScale = false;
    }

    public Vector3f GetScale ()
    {
        assert(m_bIsRSMatrix);
        return m_kScale;
    }

    public void SetUniformScale (float fScale)
    {
        assert(m_bIsRSMatrix && fScale != 0.0f);
        m_kScale = new Vector3f(fScale,fScale,fScale);
        m_bIsIdentity = false;
        m_bIsUniformScale = true;
    }

    public float GetUniformScale ()
    {
        assert(m_bIsRSMatrix && m_bIsUniformScale);
        return m_kScale.X();
    }


    // For M = R*S, the largest value of S in absolute value is returned.
    // For general M, the max-row-sum norm is returned (and is guaranteed to
    // be larger or equal to the largest eigenvalue of S in absolute value).
    public float GetNorm ()
    {
        if (m_bIsRSMatrix)
        {
            float fMax = Math.abs(m_kScale.X());
            if (Math.abs(m_kScale.Y()) > fMax)
            {
                fMax = Math.abs(m_kScale.Y());
            }
            if (Math.abs(m_kScale.Z()) > fMax)
            {
                fMax = Math.abs(m_kScale.Z());
            }
            return fMax;
        }

        // A general matrix.  Use the max-row-sum matrix norm.  The spectral
        // norm (the maximum absolute value of the eigenvalues) is smaller or
        // equal to this norm.  Therefore, this function returns an approximation
        // to the maximum scale.
        float fMaxRowSum =
            Math.abs(m_kMatrix.GetData(0,0)) +
            Math.abs(m_kMatrix.GetData(0,1)) +
            Math.abs(m_kMatrix.GetData(0,2));

        float fRowSum =
            Math.abs(m_kMatrix.GetData(1,0)) +
            Math.abs(m_kMatrix.GetData(1,1)) +
            Math.abs(m_kMatrix.GetData(1,2));

        if ( fRowSum > fMaxRowSum )
            fMaxRowSum = fRowSum;

        fRowSum =
            Math.abs(m_kMatrix.GetData(2,0)) +
            Math.abs(m_kMatrix.GetData(2,1)) +
            Math.abs(m_kMatrix.GetData(2,2));

        if (fRowSum > fMaxRowSum)
        {
            fMaxRowSum = fRowSum;
        }

        return fMaxRowSum;
    }


    // Compute Y = M*X+T where X is the input and Y is the output.
    public Vector3f ApplyForward (Vector3f rkInput) 
    {
        if (m_bIsIdentity)
        {
            // Y = X
            return rkInput;
        }

        if (m_bIsRSMatrix)
        {
            // Y = R*S*X + T
            Vector3f kOutput = new Vector3f(m_kScale.X()*rkInput.X(),m_kScale.Y()*rkInput.Y(),
                                            m_kScale.Z()*rkInput.Z());
            //kOutput = m_kMatrix*kOutput + m_kTranslate;
            kOutput = m_kMatrix.mult(kOutput).add( m_kTranslate );
            return kOutput;
        }

        // Y = M*X + T
        Vector3f kOutput = m_kMatrix.mult(rkInput).add( m_kTranslate );
        return kOutput;
    }

    // Compute X = M^{-1}*(Y-T) where Y is the input and X is the output.
    public Vector3f ApplyInverse (Vector3f rkInput)
    {
        if (m_bIsIdentity)
        {
            // X = Y
            return rkInput;
        }

        Vector3f kOutput = rkInput.sub( m_kTranslate );
        if (m_bIsRSMatrix)
        {
            // X = S^{-1}*R^t*(Y - T)
            kOutput = Matrix3f.mult(kOutput,m_kMatrix);
            if (m_bIsUniformScale)
            {
                kOutput.divEquals( GetUniformScale() );
            }
            else
            {
                // The direct inverse scaling is
                //   kOutput.X() /= m_kScale.X();
                //   kOutput.Y() /= m_kScale.Y();
                //   kOutput.Z() /= m_kScale.Z();
                // When division is much more expensive than multiplication,
                // three divisions are replaced by one division and ten
                // multiplications.
                float fSXY = m_kScale.X()*m_kScale.Y();
                float fSXZ = m_kScale.X()*m_kScale.Z();
                float fSYZ = m_kScale.Y()*m_kScale.Z();
                float fInvDet = 1.0f/(fSXY*m_kScale.Z());
                kOutput.X( kOutput.X()* fInvDet*fSYZ );
                kOutput.Y( kOutput.Y()* fInvDet*fSXZ );
                kOutput.Z( kOutput.Z()* fInvDet*fSXY );
            }
        }
        else
        {
            // X = M^{-1}*(Y - T)
            kOutput = m_kMatrix.Inverse().mult(kOutput);
        }

        return kOutput;
    }

    // Inverse-transform the input vector V0.  The output vector is
    // V1 = M^{-1}*V0.
    public Vector3f InvertVector (final Vector3f rkInput)
    {
        if (m_bIsIdentity)
        {
            // X = Y
            return rkInput;
        }

        Vector3f kOutput = new Vector3f();
        if (m_bIsRSMatrix)
        {
            // X = S^{-1}*R^t*Y
            kOutput = Matrix3f.mult(rkInput,m_kMatrix);
            if (m_bIsUniformScale)
            {
                //kOutput /= GetUniformScale();
                kOutput.divEquals( GetUniformScale() );
            }
            else
            {
                // The direct inverse scaling is
                //   kOutput.X() /= m_kScale.X();
                //   kOutput.Y() /= m_kScale.Y();
                //   kOutput.Z() /= m_kScale.Z();
                // When division is much more expensive than multiplication,
                // three divisions are replaced by one division and ten
                // multiplications.
                float fSXY = m_kScale.X()*m_kScale.Y();
                float fSXZ = m_kScale.X()*m_kScale.Z();
                float fSYZ = m_kScale.Y()*m_kScale.Z();
                float fInvDet = 1.0f/(fSXY*m_kScale.Z());
                kOutput.X( kOutput.X()* fInvDet*fSYZ);
                kOutput.Y( kOutput.Y()* fInvDet*fSXZ);
                kOutput.Z( kOutput.Z()* fInvDet*fSXY);
            }
        }
        else
        {
            // X = M^{-1}*Y
            kOutput = m_kMatrix.Inverse().mult(rkInput);
        }

        return kOutput;
    }


    // Transform the plane Dot(N0,X) = c0 to Dot(N1,Y) = c1 where both N0 and
    // N1 must be unit-length normals.
    public Plane3f ApplyForward ( Plane3f rkInput)
    {
        if (m_bIsIdentity)
        {
            return rkInput;
        }

        Plane3f kOutput = new Plane3f();
        if (m_bIsRSMatrix)
        {
            // Let X represent points in model space and Y = R*S*X+T represent
            // points in world space where S are the world scales, R is the world
            // rotation, and T is the world translation.  The inverse transform is
            // X = S^{-1}*R^t*(Y-T).  The model plane is Dot(N0,X) = C0.
            // Replacing the formula for X in it and applying some algebra leads
            // to the world plane Dot(N1,Y) = C1 where N1 = R*S^{-1}*N0 and
            // C1 = C0+Dot(N1,T).  If S is not the identity, then N1 is not unit
            // length.  We need to normalize N1 and adjust C1:  N1' = N1/|N1| and
            // C1' = C1/|N1|.
            if (m_bIsUniformScale)
            {
                kOutput.Normal = m_kMatrix.mult(rkInput.Normal);
                kOutput.Constant = GetUniformScale()*rkInput.Constant +
                    kOutput.Normal.Dot(m_kTranslate);
                return kOutput;
            }

            kOutput.Normal = rkInput.Normal;

            // The direct inverse scaling is
            //   kOutput.X() /= m_kScale.X();
            //   kOutput.Y() /= m_kScale.Y();
            //   kOutput.Z() /= m_kScale.Z();
            // When division is much more expensive than multiplication,
            // three divisions are replaced by one division and ten
            // multiplications.
            float fSXY = m_kScale.X()*m_kScale.Y();
            float fSXZ = m_kScale.X()*m_kScale.Z();
            float fSYZ = m_kScale.Y()*m_kScale.Z();
            float fInvDet = 1.0f/(fSXY*m_kScale.Z());
            kOutput.Normal.X( kOutput.Normal.X()* fInvDet*fSYZ );
            kOutput.Normal.Y( kOutput.Normal.Y()* fInvDet*fSXZ );
            kOutput.Normal.Z( kOutput.Normal.Z()* fInvDet*fSXY );
            kOutput.Normal = m_kMatrix.mult(kOutput.Normal);
        }
        else
        {
            // Let X represent points in model space and Y = M*X+T represent
            // points in world space.  The inverse transform is X = M^{-1}*(Y-T).
            // The model plane is Dot(N0,X) = C0.  Replacing the formula for X in
            // it and applying some algebra leads to the world plane
            // Dot(N1,Y) = C1 where N1 = M^{-T}*N0/|M^{-1}*N0| (superscript -T
            // denotes the transpose of the inverse) and
            // C1 = C0/|M^{-1}*N0|+Dot(N1,T).
            Matrix3f kInverse = m_kMatrix.Inverse();
            kOutput.Normal = Matrix3f.mult(rkInput.Normal,kInverse);
        }

        float fInvLength = 1.0f/kOutput.Normal.Length();
        kOutput.Normal.scaleEquals( fInvLength );
        kOutput.Constant = fInvLength*rkInput.Constant +
            kOutput.Normal.Dot(m_kTranslate);

        return kOutput;
    }

    // Compute C = A*B.
    public void Product ( Transformation rkA, Transformation rkB)
    {
        if (rkA.IsIdentity())
        {
            m_kMatrix = rkB.m_kMatrix;
            m_kTranslate = rkB.m_kTranslate;
            m_kScale = rkB.m_kScale;
            m_bIsIdentity = rkB.m_bIsIdentity;
            m_bIsRSMatrix = rkB.m_bIsRSMatrix;
            m_bIsUniformScale = rkB.m_bIsUniformScale;
            return;
        }

        if (rkB.IsIdentity())
        {
            m_kMatrix = rkA.m_kMatrix;
            m_kTranslate = rkA.m_kTranslate;
            m_kScale = rkA.m_kScale;
            m_bIsIdentity = rkA.m_bIsIdentity;
            m_bIsRSMatrix = rkA.m_bIsRSMatrix;
            m_bIsUniformScale = rkA.m_bIsUniformScale;
            return;
        }

        if (rkA.m_bIsRSMatrix && rkB.m_bIsRSMatrix)
        {
            if (rkA.m_bIsUniformScale)
            {
                SetRotate(rkA.m_kMatrix.mult(rkB.m_kMatrix));

                SetTranslate(((rkA.m_kMatrix.mult(rkB.m_kTranslate)).add(rkA.m_kTranslate)).scale(rkA.GetUniformScale()));

                if (rkB.IsUniformScale())
                {
                    SetUniformScale( rkB.GetUniformScale() * rkA.GetUniformScale() );
                }
                else
                {
                    SetScale((rkB.GetScale()).scale(rkA.GetUniformScale()));
                }

                return;
            }
        }

        // In all remaining cases, the matrix cannot be written as R*S*X+T.
        Matrix3f kMA = (rkA.m_bIsRSMatrix ?
                        rkA.m_kMatrix.TimesDiagonal(rkA.m_kScale) :
                        rkA.m_kMatrix);

        Matrix3f kMB = (rkB.m_bIsRSMatrix ?
                        rkB.m_kMatrix.TimesDiagonal(rkB.m_kScale) :
                        rkB.m_kMatrix);

        SetMatrix(kMA.mult(kMB));
        SetTranslate((kMA.mult(rkB.m_kTranslate).add(rkA.m_kTranslate)));
    }


    // Compute the inverse transformation.  If <M,T> is the matrix-translation
    // pair, the inverse is <M^{-1},-M^{-1}*T>.
    //void Inverse (Transformation& rkInverse) ;

    // Pack the transformation into a 4-by-4 matrix, stored so that it may be
    // applied to 1-by-4 vectors on the left.
    public void GetHomogeneous (Matrix4f rkHMatrix)
    {
        if (m_bIsRSMatrix)
        {
            rkHMatrix.SetData(0,0, m_kScale.X()*m_kMatrix.GetData(0,0));
            rkHMatrix.SetData(0,1, m_kScale.X()*m_kMatrix.GetData(1,0));
            rkHMatrix.SetData(0,2, m_kScale.X()*m_kMatrix.GetData(2,0));
            rkHMatrix.SetData(0,3, 0.0f);
            rkHMatrix.SetData(1,0, m_kScale.Y()*m_kMatrix.GetData(0,1));
            rkHMatrix.SetData(1,1, m_kScale.Y()*m_kMatrix.GetData(1,1));
            rkHMatrix.SetData(1,2, m_kScale.Y()*m_kMatrix.GetData(2,1));
            rkHMatrix.SetData(1,3, 0.0f);
            rkHMatrix.SetData(2,0, m_kScale.Z()*m_kMatrix.GetData(0,2));
            rkHMatrix.SetData(2,1, m_kScale.Z()*m_kMatrix.GetData(1,2));
            rkHMatrix.SetData(2,2, m_kScale.Z()*m_kMatrix.GetData(2,2));
            rkHMatrix.SetData(2,3, 0.0f);
        }
        else
        {
            rkHMatrix.SetData(0,0, m_kMatrix.GetData(0,0));
            rkHMatrix.SetData(0,1, m_kMatrix.GetData(1,0));
            rkHMatrix.SetData(0,2, m_kMatrix.GetData(2,0));
            rkHMatrix.SetData(0,3, 0.0f);
            rkHMatrix.SetData(1,0, m_kMatrix.GetData(0,1));
            rkHMatrix.SetData(1,1, m_kMatrix.GetData(1,1));
            rkHMatrix.SetData(1,2, m_kMatrix.GetData(2,1));
            rkHMatrix.SetData(1,3, 0.0f);
            rkHMatrix.SetData(2,0, m_kMatrix.GetData(0,2));
            rkHMatrix.SetData(2,1, m_kMatrix.GetData(1,2));
            rkHMatrix.SetData(2,2, m_kMatrix.GetData(2,2));
            rkHMatrix.SetData(2,3, 0.0f);
        }
        rkHMatrix.SetData(3,0, m_kTranslate.X());
        rkHMatrix.SetData(3,1, m_kTranslate.Y());
        rkHMatrix.SetData(3,2, m_kTranslate.Z());
        rkHMatrix.SetData(3,3, 1.0f);
    }


    // Allow Stream directly read/write the data members and Spatial to
    // access DISK_USED.
    public static int DISK_USED
        // bool written as char, so 3 is total bytes for the bools
        = 9*Stream.SIZEOF_FLOAT + //sizeof(Matrix3f) + 
        2*3*Stream.SIZEOF_FLOAT + 3*Stream.SIZEOF_BOOLEAN; //2*sizeof(Vector3f) + 3

    public void SetIsIdentity ( boolean bValue )
    {
        m_bIsIdentity = bValue;
    }

    public void SetIsRSMatrix ( boolean bValue )
    {
        m_bIsRSMatrix = bValue;
    }

    public void SetIsUniformScale ( boolean bValue )
    {
        m_bIsUniformScale = bValue;
    }

    private Matrix3f m_kMatrix = new Matrix3f(Matrix3f.IDENTITY);
    private Vector3f m_kTranslate = new Vector3f(Vector3f.ZERO);
    private Vector3f m_kScale = new Vector3f(Vector3f.ONE);
    private boolean m_bIsIdentity = true, m_bIsRSMatrix = true, m_bIsUniformScale = true;
}
