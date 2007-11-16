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
// Version: 4.0.1 (2007/03/07)
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

/** A quaternion is q = w + x*i + y*j + z*k where (w,x,y,z) is not
 * necessarily a unit length vector in 4D.
 */
public class Quaternion
{
    public static final Quaternion IDENTITY = new Quaternion(1.0f,0.0f,0.0f,0.0f);
    public static final Quaternion ZERO = new Quaternion(0.0f,0.0f,0.0f,0.0f);

    /** construction: uninitialized */
    public Quaternion () {}
    public Quaternion (float fW, float fX, float fY, float fZ)
    {
        m_afTuple[0] = fW;
        m_afTuple[1] = fX;
        m_afTuple[2] = fY;
        m_afTuple[3] = fZ;
    }

    public Quaternion (final Quaternion rkQ)
    {
        m_afTuple[0] = rkQ.m_afTuple[0];
        m_afTuple[1] = rkQ.m_afTuple[1];
        m_afTuple[2] = rkQ.m_afTuple[2];
        m_afTuple[3] = rkQ.m_afTuple[3];
    }

    /** quaternion for the input rotation matrix */
    public Quaternion (final Matrix3f rkRot)
    {
        FromRotationMatrix(rkRot);
    }

    /** quaternion for the rotation of the axis-angle pair */
    public Quaternion (final Vector3f rkAxis, float fAngle)
    {
        FromAxisAngle(rkAxis,fAngle);
    }

    /** quaternion for the rotation matrix with specified columns */
    public Quaternion (final Vector3f[] akRotColumn )
    {
        FromRotationMatrix(akRotColumn);
    }

    /*
    // member access:  0 = w, 1 = x, 2 = y, 3 = z
    inline operator const Real* () const;
    inline operator Real* ();
    inline Real operator[] (int i) const;
    inline Real& operator[] (int i);
    inline Real W () const;
    inline Real& W ();
    inline Real X () const;
    inline Real& X ();
    inline Real Y () const;
    inline Real& Y ();
    inline Real Z () const;
    inline Real& Z ();

    // assignment
    inline Quaternion& operator= (const Quaternion& rkQ);

    // comparison
    bool operator== (const Quaternion& rkQ) const;
    bool operator!= (const Quaternion& rkQ) const;
    bool operator<  (const Quaternion& rkQ) const;
    bool operator<= (const Quaternion& rkQ) const;
    bool operator>  (const Quaternion& rkQ) const;
    bool operator>= (const Quaternion& rkQ) const;

    // arithmetic operations
    inline Quaternion operator+ (const Quaternion& rkQ) const;
    inline Quaternion operator- (const Quaternion& rkQ) const;
    inline Quaternion operator* (const Quaternion& rkQ) const;
    inline Quaternion operator* (Real fScalar) const;
    inline Quaternion operator/ (Real fScalar) const;
    inline Quaternion operator- () const;

    // arithmetic updates
    inline Quaternion& operator+= (const Quaternion& rkQ);
    inline Quaternion& operator-= (const Quaternion& rkQ);
    inline Quaternion& operator*= (Real fScalar);
    inline Quaternion& operator/= (Real fScalar);
    */

    // conversion between quaternions, matrices, and axis-angle
    public void FromRotationMatrix (final Matrix3f rkRot)
    {
        // Algorithm in Ken Shoemake's article in 1987 SIGGRAPH course notes
        // article "Quaternion Calculus and Fast Animation".

        float fTrace = rkRot.GetData(0,0) + rkRot.GetData(1,1) + rkRot.GetData(2,2);
        float fRoot;

        if (fTrace > 0.0f)
        {
            // |w| > 1/2, may as well choose w > 1/2
            fRoot = (float)Math.sqrt(fTrace + 1.0f);  // 2w
            m_afTuple[0] = (0.5f)*fRoot;
            fRoot = (0.5f)/fRoot;  // 1/(4w)
            m_afTuple[1] = (rkRot.GetData(2,1)-rkRot.GetData(1,2))*fRoot;
            m_afTuple[2] = (rkRot.GetData(0,2)-rkRot.GetData(2,0))*fRoot;
            m_afTuple[3] = (rkRot.GetData(1,0)-rkRot.GetData(0,1))*fRoot;
        }
        else
        {
            // |w| <= 1/2
            int i = 0;
            if ( rkRot.GetData(1,1) > rkRot.GetData(0,0) )
            {
                i = 1;
            }
            if ( rkRot.GetData(2,2) > rkRot.GetData(i,i) )
            {
                i = 2;
            }
            int j = ms_iNext[i];
            int k = ms_iNext[j];

            fRoot = (float)Math.sqrt(rkRot.GetData(i,i)-rkRot.GetData(j,j)-rkRot.GetData(k,k)+1.0f);
            //             float[] apfQuat = new float[]{ &m_afTuple[1], &m_afTuple[2], &m_afTuple[3] };
            m_afTuple[i+1] = (0.5f)*fRoot;
            fRoot = (0.5f)/fRoot;
            m_afTuple[0] = (rkRot.GetData(k,j)-rkRot.GetData(j,k))*fRoot;
            m_afTuple[j+1] = (rkRot.GetData(j,i)+rkRot.GetData(i,j))*fRoot;
            m_afTuple[k+1] = (rkRot.GetData(k,i)+rkRot.GetData(i,k))*fRoot;
        }
    }

    public void ToRotationMatrix (Matrix3f rkRot)
    {
        float fTx  = ((float)2.0)*m_afTuple[1];
        float fTy  = ((float)2.0)*m_afTuple[2];
        float fTz  = ((float)2.0)*m_afTuple[3];
        float fTwx = fTx*m_afTuple[0];
        float fTwy = fTy*m_afTuple[0];
        float fTwz = fTz*m_afTuple[0];
        float fTxx = fTx*m_afTuple[1];
        float fTxy = fTy*m_afTuple[1];
        float fTxz = fTz*m_afTuple[1];
        float fTyy = fTy*m_afTuple[2];
        float fTyz = fTz*m_afTuple[2];
        float fTzz = fTz*m_afTuple[3];

        rkRot.SetData(0,0, (float)1.0-(fTyy+fTzz));
        rkRot.SetData(0,1, fTxy-fTwz);
        rkRot.SetData(0,2, fTxz+fTwy);
        rkRot.SetData(1,0, fTxy+fTwz);
        rkRot.SetData(1,1, (float)1.0-(fTxx+fTzz));
        rkRot.SetData(1,2, fTyz-fTwx);
        rkRot.SetData(2,0, fTxz-fTwy);
        rkRot.SetData(2,1, fTyz+fTwx);
        rkRot.SetData(2,2, (float)1.0-(fTxx+fTyy));
    }

    public void FromRotationMatrix (final Vector3f[] akRotColumn)
    {
        Matrix3f kRot = new Matrix3f();
        for (int iCol = 0; iCol < 3; iCol++)
        {
            kRot.SetData(0,iCol, akRotColumn[iCol].X());
            kRot.SetData(1,iCol, akRotColumn[iCol].Y());
            kRot.SetData(2,iCol, akRotColumn[iCol].Z());
        }
        FromRotationMatrix(kRot);
    }

    public void ToRotationMatrix (Vector3f[] akRotColumn)
    {
        Matrix3f kRot = new Matrix3f();
        ToRotationMatrix(kRot);
        for (int iCol = 0; iCol < 3; iCol++)
        {
            akRotColumn[iCol].X(kRot.GetData(0,iCol));
            akRotColumn[iCol].Y(kRot.GetData(1,iCol));
            akRotColumn[iCol].Z(kRot.GetData(2,iCol));
        }
    }

    public void FromAxisAngle (final Vector3f rkAxis, float fAngle)
    {
        // assert:  axis[] is unit length
        //
        // The quaternion representing the rotation is
        //   q = cos(A/2)+sin(A/2)*(x*i+y*j+z*k)

        float fHalfAngle = ((float)0.5)*fAngle;
        float fSin = (float)Math.sin(fHalfAngle);
        m_afTuple[0] = (float)Math.cos(fHalfAngle);
        m_afTuple[1] = fSin*rkAxis.X();
        m_afTuple[2] = fSin*rkAxis.Y();
        m_afTuple[3] = fSin*rkAxis.Z();
    }


    public float ToAxisAngle (Vector3f rkAxis)
    {
        // The quaternion representing the rotation is
        //   q = cos(A/2)+sin(A/2)*(x*i+y*j+z*k)

        float rfAngle = 0;

        float fSqrLength = m_afTuple[1]*m_afTuple[1] + m_afTuple[2]*m_afTuple[2]
            + m_afTuple[3]*m_afTuple[3];

        if (fSqrLength > Mathf.ZERO_TOLERANCE)
        {
            rfAngle = (float)(2.0f*Math.acos(m_afTuple[0]));
            float fInvLength = Mathf.InvSqrt(fSqrLength);
            rkAxis.X( m_afTuple[1]*fInvLength );
            rkAxis.Y( m_afTuple[2]*fInvLength );
            rkAxis.Z( m_afTuple[3]*fInvLength );
        }
        else
        {
            // angle is 0 (mod 2*pi), so any axis will do
            rfAngle = 0.0f;
            rkAxis.X( 1.0f );
            rkAxis.Y( 0.0f );
            rkAxis.Z( 0.0f );
        }
        return rfAngle;
    }
    /*
    // functions of a quaternion
    inline Real Length () const;  // length of 4-tuple
    inline Real SquaredLength () const;  // squared length of 4-tuple
    inline Real Dot (const Quaternion& rkQ) const;  // dot product of 4-tuples
    inline Real Normalize ();  // make the 4-tuple unit length
    Quaternion Inverse () const;  // apply to non-zero quaternion
    Quaternion Conjugate () const;
    Quaternion Exp () const;  // apply to quaternion with w = 0
    Quaternion Log () const;  // apply to unit-length quaternion

    // rotation of a vector by a quaternion
    Vector3<Real> Rotate (const Vector3<Real>& rkVector) const;

    // spherical linear interpolation
    Quaternion& Slerp (Real fT, const Quaternion& rkP, const Quaternion& rkQ);

    Quaternion& SlerpExtraSpins (Real fT, const Quaternion& rkP,
    const Quaternion& rkQ, int iExtraSpins);

    // intermediate terms for spherical quadratic interpolation
    Quaternion& Intermediate (const Quaternion& rkQ0,
    const Quaternion& rkQ1, const Quaternion& rkQ2);

    // spherical quadratic interpolation
    Quaternion& Squad (Real fT, const Quaternion& rkQ0,
    const Quaternion& rkA0, const Quaternion& rkA1,
    const Quaternion& rkQ1);
    */
    /** Compute a quaternion that rotates unit-length vector V1 to unit-length
     * vector V2.  The rotation is about the axis perpendicular to both V1 and
     * V2, with angle of that between V1 and V2.  If V1 and V2 are parallel,
     * any axis of rotation will do, such as the permutation (z2,x2,y2), where
     * V2 = (x2,y2,z2).
     */
    public void Align (final Vector3f rkV1, final Vector3f rkV2)
    {
        // If V1 and V2 are not parallel, the axis of rotation is the unit-length
        // vector U = Cross(V1,V2)/Length(Cross(V1,V2)).  The angle of rotation,
        // A, is the angle between V1 and V2.  The quaternion for the rotation is
        // q = cos(A/2) + sin(A/2)*(ux*i+uy*j+uz*k) where U = (ux,uy,uz).
        //
        // (1) Rather than extract A = acos(Dot(V1,V2)), multiply by 1/2, then
        //     compute sin(A/2) and cos(A/2), we reduce the computational costs by
        //     computing the bisector B = (V1+V2)/Length(V1+V2), so cos(A/2) =
        //     Dot(V1,B).
        //
        // (2) The rotation axis is U = Cross(V1,B)/Length(Cross(V1,B)), but
        //     Length(Cross(V1,B)) = Length(V1)*Length(B)*sin(A/2) = sin(A/2), in
        //     which case sin(A/2)*(ux*i+uy*j+uz*k) = (cx*i+cy*j+cz*k) where
        //     C = Cross(V1,B).
        //
        // If V1 = V2, then B = V1, cos(A/2) = 1, and U = (0,0,0).  If V1 = -V2,
        // then B = 0.  This can happen even if V1 is approximately -V2 using
        // floating point arithmetic, since Vector3::Normalize checks for
        // closeness to zero and returns the zero vector accordingly.  The test
        // for exactly zero is usually not recommend for floating point
        // arithmetic, but the implementation of Vector3::Normalize guarantees
        // the comparison is robust.  In this case, the A = pi and any axis
        // perpendicular to V1 may be used as the rotation axis.

        Vector3f kBisector = new Vector3f();
        rkV1.add( rkV2, kBisector );
        kBisector.Normalize();

        float fCosHalfAngle = rkV1.Dot(kBisector);
        m_afTuple[0] = fCosHalfAngle;

        if (fCosHalfAngle != 0.0)
        {
            Vector3f kCross = new Vector3f();
            rkV1.Cross(kBisector, kCross);
            m_afTuple[1] = kCross.X();
            m_afTuple[2] = kCross.Y();
            m_afTuple[3] = kCross.Z();
            kCross = null;
        }
        else
        {
            float fInvLength;
            if (Math.abs(rkV1.X()) >= Math.abs(rkV1.Y()))
            {
                // V1.x or V1.z is the largest magnitude component
                fInvLength = Mathf.InvSqrt(rkV1.X()*rkV1.X() +
                                           rkV1.Z()*rkV1.Z());

                m_afTuple[1] = -rkV1.Z()*fInvLength;
                m_afTuple[2] = 0.0f;
                m_afTuple[3] = +rkV1.X()*fInvLength;
            }
            else
            {
                // V1.y or V1.z is the largest magnitude component
                fInvLength = Mathf.InvSqrt(rkV1.Y()*rkV1.Y() +
                                           rkV1.Z()*rkV1.Z());

                m_afTuple[1] = 0.0f;
                m_afTuple[2] = +rkV1.Z()*fInvLength;
                m_afTuple[3] = -rkV1.Y()*fInvLength;
            }
        }
        kBisector = null;
    }

    /*
    // Decompose a quaternion into q = q_twist * q_swing, where q is 'this'
    // quaternion.  If V1 is the input axis and V2 is the rotation of V1 by
    // q, q_swing represents the rotation about the axis perpendicular to
    // V1 and V2 (see Quaternion::Align), and q_twist is a rotation about V1.
    void DecomposeTwistTimesSwing (const Vector3<Real>& rkV1,
    Quaternion& rkTwist, Quaternion& rkSwing);

    // Decompose a quaternion into q = q_swing * q_twist, where q is 'this'
    // quaternion.  If V1 is the input axis and V2 is the rotation of V1 by
    // q, q_swing represents the rotation about the axis perpendicular to
    // V1 and V2 (see Quaternion::Align), and q_twist is a rotation about V1.
    void DecomposeSwingTimesTwist (const Vector3<Real>& rkV1,
    Quaternion& rkSwing, Quaternion& rkTwist);
    */
    /** support for comparisons */
    /*
      private int CompareArrays (const Quaternion& rkQ) const;
    */
    /** support for FromRotationMatrix */
    private static int[] ms_iNext = new int[]{1,2,0};
    private float[] m_afTuple = new float[4];
};

// inline Quaternion<Real> operator* (Real fScalar, const Quaternion<Real>& rkQ);
