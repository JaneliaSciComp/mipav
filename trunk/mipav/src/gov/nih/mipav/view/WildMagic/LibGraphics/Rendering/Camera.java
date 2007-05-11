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
// Version: 4.0.1 (2006/08/07)

package gov.nih.mipav.view.WildMagic.LibGraphics.Rendering;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class Camera extends WmObject
    implements StreamInterface
{
    // Construction and destruction.
    public Camera ()
    {
        m_pkRenderer = null;

        SetFrustum(-0.5f,0.5f,-0.5f,0.5f,1.0f,2.0f);
        SetViewport(0.0f,1.0f,1.0f,0.0f);
        SetDepthRange(0.0f,1.0f);
        SetFrame(Vector3f.ZERO,Vector3f.UNIT_Z_NEG,Vector3f.UNIT_Y,
                 Vector3f.UNIT_X);

        Perspective = true;
    }

    // The camera frame is always in world coordinates.
    //   default location  E = (0,0,0)
    //   default direction D = (0,0,-1)
    //   default up        U = (0,1,0)
    //   default right     R = (1,0,0)
    public void SetFrame ( Vector3f rkLocation, Vector3f rkDVector,
                    Vector3f rkUVector, Vector3f rkRVector)
    {
        m_kLocation = rkLocation;
        m_kDVector = rkDVector;
        m_kUVector = rkUVector;
        m_kRVector = rkRVector;

        if (m_pkRenderer != null)
        {
            m_pkRenderer.OnFrameChange();
        }
    }

    public void SetLocation (Vector3f rkLocation)
    {
        m_kLocation = rkLocation;

        if (m_pkRenderer != null)
        {
            m_pkRenderer.OnFrameChange();
        }
    }

    public void SetAxes (Vector3f rkDVector, Vector3f rkUVector,
                  Vector3f rkRVector)
    {
        m_kDVector = rkDVector;
        m_kUVector = rkUVector;
        m_kRVector = rkRVector;

        if (m_pkRenderer != null)
        {
            m_pkRenderer.OnFrameChange();
        }
    }

    public Vector3f GetLocation ()
    {
        return m_kLocation;
    }

    public Vector3f GetDVector ()
    {
        return m_kDVector;
    }

    public Vector3f GetUVector ()
    {
        return m_kUVector;
    }

    public Vector3f GetRVector ()
    {
        return m_kRVector;
    }


    // The view frustum has parameters [rmin,rmax], [umin,umax], and
    // [dmin,dmax].  The interval [rmin,rmax] is measured in the right
    // direction R.  These are the "left" and "right" frustum values.  The
    // interval [umin,umax] is measured in the up direction U.  These are
    // the "bottom" and "top" values.  The interval [dmin,dmax] is measured
    // in the view direction D.  These are the "near" and "far" values.
    // The frustum values are stored in an array with the following mappings:
    public enum ViewFrustum
    {
        VF_DMIN (0),     //= 0,  // near
        VF_DMAX (1),     //= 1,  // far
        VF_UMIN (2),     //= 2,  // bottom
        VF_UMAX (3),     //= 3,  // top
        VF_RMIN (4),     //= 4,  // left
        VF_RMAX (5),     //= 5,  // right
        VF_QUANTITY (6);  //= 6

        private final int m_iValue;
        ViewFrustum( int iValue )
        {
            this.m_iValue = iValue;
        }
        public int Value() { return m_iValue; }
    };

    // Set the view frustum.  The interval [rmin,rmax] is measured in the
    // right direction R.  These are the "left" and "right" frustum values.
    // The interval [umin,umax] is measured in the up direction U.  These are
    // the "bottom" and "top" values.  The interval [dmin,dmax] is measured
    // in the view direction D.  These are the "near" and "far" values.
    public void SetFrustum (float fRMin, float fRMax, float fUMin, float fUMax,
                            float fDMin, float fDMax)
    {
        m_afFrustum[ViewFrustum.VF_DMIN.Value()] = fDMin;
        m_afFrustum[ViewFrustum.VF_DMAX.Value()] = fDMax;
        m_afFrustum[ViewFrustum.VF_UMIN.Value()] = fUMin;
        m_afFrustum[ViewFrustum.VF_UMAX.Value()] = fUMax;
        m_afFrustum[ViewFrustum.VF_RMIN.Value()] = fRMin;
        m_afFrustum[ViewFrustum.VF_RMAX.Value()] = fRMax;

        if (m_pkRenderer != null)
        {
            m_pkRenderer.OnFrustumChange();
        }
    }

    // Set a symmetric view frustum (umin = -umax, rmin = -rmax) using a field
    // of view in the "up" direction and an aspect ratio "width/height".  This
    // call is the equivalent of gluPerspective in OpenGL.  As such, the field
    // of view in this function must be specified in degrees and be in the
    // interval (0,180).
    public void SetFrustum (float fUpFovDegrees, float fAspectRatio, float fDMin,
        float fDMax)
    {
        float fHalfAngleRadians = 0.5f*fUpFovDegrees*Mathf.DEG_TO_RAD;
        m_afFrustum[ViewFrustum.VF_UMAX.Value()] = (float)(fDMin*Math.tan(fHalfAngleRadians));
        m_afFrustum[ViewFrustum.VF_RMAX.Value()] = fAspectRatio*m_afFrustum[ViewFrustum.VF_UMAX.Value()];
        m_afFrustum[ViewFrustum.VF_UMIN.Value()] = -m_afFrustum[ViewFrustum.VF_UMAX.Value()];
        m_afFrustum[ViewFrustum.VF_RMIN.Value()] = -m_afFrustum[ViewFrustum.VF_RMAX.Value()];
        m_afFrustum[ViewFrustum.VF_DMIN.Value()] = fDMin;
        m_afFrustum[ViewFrustum.VF_DMAX.Value()] = fDMax;

        if (m_pkRenderer != null)
        {
            m_pkRenderer.OnFrustumChange();
        }
    }


    // Get the view frustum.
    public float[] GetFrustum ()
    {
        return m_afFrustum;
    }


    // Get the parameters for a symmetric view frustum.  The return value is
    // 'true' iff the current frustum is symmetric, in which case the output
    // parameters are valid.

    // Get the individual frustum values.
    public float GetDMin ()
    {
        return m_afFrustum[ViewFrustum.VF_DMIN.Value()];
    }

    public float GetDMax ()
    {
        return m_afFrustum[ViewFrustum.VF_DMAX.Value()];
    }

    public float GetUMin ()
    {
        return m_afFrustum[ViewFrustum.VF_UMIN.Value()];
    }

    public float GetUMax ()
    {
        return m_afFrustum[ViewFrustum.VF_UMAX.Value()];
    }

    public float GetRMin ()
    {
        return m_afFrustum[ViewFrustum.VF_RMIN.Value()];
    }

    public float GetRMax ()
    {
        return m_afFrustum[ViewFrustum.VF_RMAX.Value()];
    }

    // Allow for orthogonal cameras as well as perspective cameras.  The
    // default is perspective (value is 'true').  Set to 'false' for an
    // orthogonal camera.  TO DO.  Stream this member.
    public boolean Perspective;

    // viewport (contained in [0,1]^2)
    public void SetViewport (float fLeft, float fRight, float fTop, float fBottom)
    {
        m_fPortL = fLeft;
        m_fPortR = fRight;
        m_fPortT = fTop;
        m_fPortB = fBottom;

        if (m_pkRenderer != null)
        {
            m_pkRenderer.OnViewportChange();
        }
    }

    public float GetPortL ()
    {
        return m_fPortL;
    }

    public float GetPortR ()
    {
        return m_fPortR;
    }

    public float GetPortT ()
    {
        return m_fPortT;
    }

    public float GetPortB ()
    {
        return m_fPortB;
    }

    // depth range (contained in [0,1])
    public void SetDepthRange (float fNear, float fFar)
    {
        m_fPortN = fNear;
        m_fPortF = fFar;

        if (m_pkRenderer != null)
        {
            m_pkRenderer.OnDepthRangeChange();
        }
    }

    public float GetPortN() 
    {
        return m_fPortN;
    }

    public float GetPortF() 
    {
        return m_fPortF;
    }

    // Mouse picking support.  The (x,y) input point is in left-handed screen
    // coordinates (what you usually read from the windowing system).  The
    // function returns 'true' if and only if the input point is located in
    // the current viewport.  When 'true', the origin and direction values
    // are valid and are in world coordinates.  The direction vector is unit
    // length.
    public boolean GetPickRay (int iX, int iY, int iWidth, int iHeight,
                               Vector3f rkOrigin, Vector3f rkDirection)
    {
        float fPortX = ((float)iX)/(float)(iWidth-1);
        if (fPortX < m_fPortL || fPortX > m_fPortR)
        {
            return false;
        }

        float fPortY = ((float)(iHeight-1-iY))/(float)(iHeight-1);
        if (fPortY < m_fPortB || fPortY > m_fPortT)
        {
            return false;
        }

        float fXWeight = (fPortX - m_fPortL)/(m_fPortR - m_fPortL);
        float fViewX = (1.0f-fXWeight)*m_afFrustum[ViewFrustum.VF_RMIN.Value()] +
            fXWeight*m_afFrustum[ViewFrustum.VF_RMAX.Value()];
        float fYWeight = (fPortY - m_fPortB)/(m_fPortT - m_fPortB);
        float fViewY = (1.0f-fYWeight)*m_afFrustum[ViewFrustum.VF_UMIN.Value()] +
            fYWeight*m_afFrustum[ViewFrustum.VF_UMAX.Value()];

        rkOrigin = m_kLocation;

        rkDirection = m_kDVector.
            scale(m_afFrustum[ViewFrustum.VF_DMIN.Value()]).
            add( m_kRVector.scale(fViewX).
                 add(m_kUVector.scale(fViewY)));
        rkDirection.Normalize();
        return true;
    }

    // world coordinate frame
    protected Vector3f m_kLocation, m_kDVector, m_kUVector, m_kRVector;

    // view frustum (near, far, bottom, top, left, right)
    protected float[] m_afFrustum = new float[ViewFrustum.VF_QUANTITY.Value()];

    // viewport
    protected float m_fPortL, m_fPortR, m_fPortT, m_fPortB;

    // depth range
    protected float m_fPortN, m_fPortF;

    // The renderer to which this camera has been attached.  The camera is
    // considered to be active if this pointer is not null.  By necessity, a
    // camera cannot be attached to multiple renderers, but a camera may be
    // shared by renderers as long as only one renderer at a time uses the
    // camera.  The renderer is responsible for setting this Camera member.
    protected Renderer m_pkRenderer;


    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        rkStream.Read(m_kLocation);
        rkStream.Read(m_kDVector);
        rkStream.Read(m_kUVector);
        rkStream.Read(m_kRVector);
        m_afFrustum[ViewFrustum.VF_DMIN.Value()] = rkStream.ReadFloat();
        m_afFrustum[ViewFrustum.VF_DMAX.Value()] = rkStream.ReadFloat();
        m_afFrustum[ViewFrustum.VF_UMIN.Value()] = rkStream.ReadFloat();
        m_afFrustum[ViewFrustum.VF_UMAX.Value()] = rkStream.ReadFloat();
        m_afFrustum[ViewFrustum.VF_RMIN.Value()] = rkStream.ReadFloat();
        m_afFrustum[ViewFrustum.VF_RMAX.Value()] = rkStream.ReadFloat();
        m_fPortL = rkStream.ReadFloat();
        m_fPortR = rkStream.ReadFloat();
        m_fPortT = rkStream.ReadFloat();
        m_fPortB = rkStream.ReadFloat();
        m_fPortN = rkStream.ReadFloat();
        m_fPortF = rkStream.ReadFloat();
    }

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_kLocation);
        rkStream.Write(m_kDVector);
        rkStream.Write(m_kUVector);
        rkStream.Write(m_kRVector);
        rkStream.Write(m_afFrustum[ViewFrustum.VF_DMIN.Value()]);
        rkStream.Write(m_afFrustum[ViewFrustum.VF_DMAX.Value()]);
        rkStream.Write(m_afFrustum[ViewFrustum.VF_UMIN.Value()]);
        rkStream.Write(m_afFrustum[ViewFrustum.VF_UMAX.Value()]);
        rkStream.Write(m_afFrustum[ViewFrustum.VF_RMIN.Value()]);
        rkStream.Write(m_afFrustum[ViewFrustum.VF_RMAX.Value()]);
        rkStream.Write(m_fPortL);
        rkStream.Write(m_fPortR);
        rkStream.Write(m_fPortT);
        rkStream.Write(m_fPortB);
        rkStream.Write(m_fPortN);
        rkStream.Write(m_fPortF);
    }

    public int GetDiskUsed (final StreamVersion rkVersion)
    {
        int iSize = super.GetDiskUsed(rkVersion) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(m_kLocation) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(m_kDVector) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(m_kUVector) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(m_kRVector) +
            Stream.SIZEOF_FLOAT + //sizeof(m_afFrustum[VF_DMIN]) +
            Stream.SIZEOF_FLOAT + //sizeof(m_afFrustum[VF_DMAX]) +
            Stream.SIZEOF_FLOAT + //sizeof(m_afFrustum[VF_UMIN]) +
            Stream.SIZEOF_FLOAT + //sizeof(m_afFrustum[VF_UMAX]) +
            Stream.SIZEOF_FLOAT + //sizeof(m_afFrustum[VF_RMIN]) +
            Stream.SIZEOF_FLOAT + //sizeof(m_afFrustum[VF_RMAX]) +
            Stream.SIZEOF_FLOAT + //sizeof(m_fPortL) +
            Stream.SIZEOF_FLOAT + //sizeof(m_fPortR) +
            Stream.SIZEOF_FLOAT + //sizeof(m_fPortT) +
            Stream.SIZEOF_FLOAT + //sizeof(m_fPortB) +
            Stream.SIZEOF_FLOAT + //sizeof(m_fPortN) +
            Stream.SIZEOF_FLOAT; //sizeof(m_fPortF);

        return iSize;
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("Camera",GetName()));
        // children
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("location =",m_kLocation));
        pkTree.Append(StringTree.Format("d vector =",m_kDVector));
        pkTree.Append(StringTree.Format("u vector =",m_kUVector));
        pkTree.Append(StringTree.Format("r vector =",m_kRVector));
        pkTree.Append(StringTree.Format("dmin =",m_afFrustum[ViewFrustum.VF_DMIN.Value()]));
        pkTree.Append(StringTree.Format("dmax =",m_afFrustum[ViewFrustum.VF_DMAX.Value()]));
        pkTree.Append(StringTree.Format("umin =",m_afFrustum[ViewFrustum.VF_UMIN.Value()]));
        pkTree.Append(StringTree.Format("umax =",m_afFrustum[ViewFrustum.VF_UMAX.Value()]));
        pkTree.Append(StringTree.Format("rmin =",m_afFrustum[ViewFrustum.VF_RMIN.Value()]));
        pkTree.Append(StringTree.Format("rmax =",m_afFrustum[ViewFrustum.VF_RMAX.Value()]));
        pkTree.Append(StringTree.Format("port L =",m_fPortL));
        pkTree.Append(StringTree.Format("port R =",m_fPortR));
        pkTree.Append(StringTree.Format("port T =",m_fPortT));
        pkTree.Append(StringTree.Format("port B =",m_fPortB));
        pkTree.Append(StringTree.Format("port N =",m_fPortN));
        pkTree.Append(StringTree.Format("port F =",m_fPortF));
        return pkTree;
    }

}
