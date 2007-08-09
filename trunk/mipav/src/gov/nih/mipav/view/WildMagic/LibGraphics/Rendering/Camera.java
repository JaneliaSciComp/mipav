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

package gov.nih.mipav.view.WildMagic.LibGraphics.Rendering;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class Camera extends GraphicsObject
    implements StreamInterface
{
    /** Default constructor. */
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

    /** Delete memory */
    public void finalize()
    {
        if ( m_kLocation != null )
        {
            m_kLocation.finalize();
            m_kLocation = null;
        }
        if ( m_kDVector != null )
        {
            m_kDVector.finalize();
            m_kDVector = null;
        }
        if ( m_kUVector != null )
        {
            m_kUVector.finalize();
            m_kUVector = null;
        }
        if ( m_kRVector != null )
        {
            m_kRVector.finalize();
            m_kRVector = null;
        }
        m_afFrustum = null;
        m_pkRenderer = null;

        super.finalize();
    }

    /** The camera frame is always in world coordinates.
     *   default location  E = (0,0,0)
     *   default direction D = (0,0,-1)
     *   default up        U = (0,1,0)
     *   default right     R = (1,0,0)
     * @param rkLocation, location vector.
     * @param rkDVector, direction vector.
     * @param rkUVector, up vector.
     * @param rkRVector, right vector.
     */
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

    /** The camera frame is always in world coordinates.
     *   default location  E = (0,0,0)
     *   default direction D = (0,0,-1)
     *   default up        U = (0,1,0)
     *   default right     R = (1,0,0)
     * @param rkLocation, location vector.
     */
    public void SetLocation (Vector3f rkLocation)
    {
        m_kLocation = rkLocation;

        if (m_pkRenderer != null)
        {
            m_pkRenderer.OnFrameChange();
        }
    }

    /** The camera frame is always in world coordinates.
     *   default location  E = (0,0,0)
     *   default direction D = (0,0,-1)
     *   default up        U = (0,1,0)
     *   default right     R = (1,0,0)
     * @param rkDVector, direction vector.
     * @param rkUVector, up vector.
     * @param rkRVector, right vector.
     */
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

    /** The camera frame is always in world coordinates.
     *   default location  E = (0,0,0)
     *   default direction D = (0,0,-1)
     *   default up        U = (0,1,0)
     *   default right     R = (1,0,0)
     * @return location vector.
     */
    public final Vector3f GetLocation ()
    {
        return m_kLocation;
    }

    /** The camera frame is always in world coordinates.
     *   default location  E = (0,0,0)
     *   default direction D = (0,0,-1)
     *   default up        U = (0,1,0)
     *   default right     R = (1,0,0)
     * @return direction vector.
     */
    public final Vector3f GetDVector ()
    {
        return m_kDVector;
    }

    /** The camera frame is always in world coordinates.
     *   default location  E = (0,0,0)
     *   default direction D = (0,0,-1)
     *   default up        U = (0,1,0)
     *   default right     R = (1,0,0)
     * @return up vector.
     */
    public final Vector3f GetUVector ()
    {
        return m_kUVector;
    }

    /** The camera frame is always in world coordinates.
     *   default location  E = (0,0,0)
     *   default direction D = (0,0,-1)
     *   default up        U = (0,1,0)
     *   default right     R = (1,0,0)
     * @return right vector.
     */
    public final Vector3f GetRVector ()
    {
        return m_kRVector;
    }


    /** The view frustum has parameters [rmin,rmax], [umin,umax], and
     * [dmin,dmax].  The interval [rmin,rmax] is measured in the right
     * direction R.  These are the "left" and "right" frustum values.  The
     * interval [umin,umax] is measured in the up direction U.  These are
     * the "bottom" and "top" values.  The interval [dmin,dmax] is measured
     * in the view direction D.  These are the "near" and "far" values.
     * The frustum values are stored in an array with the following mappings:
     */
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

    /** Set the view frustum.  The interval [rmin,rmax] is measured in the
     * right direction R.  These are the "left" and "right" frustum values.
     * The interval [umin,umax] is measured in the up direction U.  These are
     * the "bottom" and "top" values.  The interval [dmin,dmax] is measured
     * in the view direction D.  These are the "near" and "far" values.
     * @param fRMin, "left"
     * @param fRMax, "right"
     * @param fUMin, "bottom"
     * @param fUMax, "top"
     * @param fDMin, "near"
     * @param fDMax, "far"
     */
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

    /** Set a symmetric view frustum (umin = -umax, rmin = -rmax) using a field
     * of view in the "up" direction and an aspect ratio "width/height".  This
     * call is the equivalent of gluPerspective in OpenGL.  As such, the field
     * of view in this function must be specified in degrees and be in the
     * interval (0,180).
     * @param fUpFovDegrees, field of view in the "up" direction.
     * @param fAspectRatio, aspect ratio "width/height"
     * @param fDMin, "near"
     * @param fDMax, "far"
     */
    public void SetFrustum (float fUpFovDegrees, float fAspectRatio,
                            float fDMin, float fDMax)
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


    /** Get the view frustum.
     * @return the view frustum. */
    public final float[] GetFrustum ()
    {
        return m_afFrustum;
    }


    // Get the parameters for a symmetric view frustum.  The return value is
    // 'true' iff the current frustum is symmetric, in which case the output
    // parameters are valid.

    /** Get the near frustum value.
     * @return the near frustum value. */
    public final float GetDMin ()
    {
        return m_afFrustum[ViewFrustum.VF_DMIN.Value()];
    }

    /** Get the far frustum value.
     * @return the far frustum value. */
    public final float GetDMax ()
    {
        return m_afFrustum[ViewFrustum.VF_DMAX.Value()];
    }

    /** Get the bottom frustum value.
     * @return the bottom frustum value. */
    public final float GetUMin ()
    {
        return m_afFrustum[ViewFrustum.VF_UMIN.Value()];
    }

    /** Get the top frustum value.
     * @return the top frustum value. */
    public final float GetUMax ()
    {
        return m_afFrustum[ViewFrustum.VF_UMAX.Value()];
    }

    /** Get the left frustum value.
     * @return the left frustum value. */
    public final float GetRMin ()
    {
        return m_afFrustum[ViewFrustum.VF_RMIN.Value()];
    }

    /** Get the right frustum value.
     * @return the right frustum value. */
    public final float GetRMax ()
    {
        return m_afFrustum[ViewFrustum.VF_RMAX.Value()];
    }

    /** Allow for orthogonal cameras as well as perspective cameras.  The
     * default is perspective (value is 'true').  Set to 'false' for an
     * orthogonal camera.  TO DO.  Stream this member.
     */
    public boolean Perspective;

    /** Set the viewport (contained in [0,1]^2)
     * @param fLeft, left
     * @param fRight, right
     * @param fTop, top
     * @param fBottom, bottom
     */
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

    /** Get the viewport (contained in [0,1]^2)
     * @return left
     */
    public final float GetPortL ()
    {
        return m_fPortL;
    }

    /** Get the viewport (contained in [0,1]^2)
     * @return right
     */
    public final float GetPortR ()
    {
        return m_fPortR;
    }

    /** Get the viewport (contained in [0,1]^2)
     * @return top
     */
    public final float GetPortT ()
    {
        return m_fPortT;
    }

    /** Get the viewport (contained in [0,1]^2)
     * @return bottom
     */
    public final float GetPortB ()
    {
        return m_fPortB;
    }

    /** Set depth range (contained in [0,1])
     * @param fNear, near
     * @param fFar, near
     */
    public void SetDepthRange (float fNear, float fFar)
    {
        m_fPortN = fNear;
        m_fPortF = fFar;

        if (m_pkRenderer != null)
        {
            m_pkRenderer.OnDepthRangeChange();
        }
    }

    /** Get depth range (contained in [0,1])
     * @return near
     */
    public final float GetPortN() 
    {
        return m_fPortN;
    }

    /** Get depth range (contained in [0,1])
     * @return far
     */
    public final float GetPortF() 
    {
        return m_fPortF;
    }

    /** Mouse picking support.  The (x,y) input point is in left-handed screen
     * coordinates (what you usually read from the windowing system).  The
     * function returns 'true' if and only if the input point is located in
     * the current viewport.  When 'true', the origin and direction values
     * are valid and are in world coordinates.  The direction vector is unit
     * length.
     * @param iX, x screen coordinate (left-handed)
     * @param iY, y screen coordinate (left-handed)
     * @param iWidth, screen width
     * @param iHeight, screen height
     * @param rkOrigin, origin of PickRay, return value
     * @param rkDirection, direction of PickRay, return value
     * @return 'true' if and only if the input point is located in the current
     * viewport.
     */
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
    
    /** world coordinate frame */
    protected Vector3f m_kLocation, m_kDVector, m_kUVector, m_kRVector;

    /** view frustum (near, far, bottom, top, left, right) */
    protected float[] m_afFrustum = new float[ViewFrustum.VF_QUANTITY.Value()];

    /** viewport */
    protected float m_fPortL, m_fPortR, m_fPortT, m_fPortB;

    /** depth range */
    protected float m_fPortN, m_fPortF;

    /** The renderer to which this camera has been attached.  The camera is
     * considered to be active if this pointer is not null.  By necessity, a
     * camera cannot be attached to multiple renderers, but a camera may be
     * shared by renderers as long as only one renderer at a time uses the
     * camera.  The renderer is responsible for setting this Camera member.
     */
    protected Renderer m_pkRenderer;


    /**
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object
     * for linking after all objects are loaded from the Stream.
     * @param rkStream, the Stream from which this object is being read.
     * @param pkLink, the Link class for storing the IDs of this object's
     * children objcts.
     */
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

    /**
     * Copies this objects children objects from the input Stream's HashTable,
     * based on the LinkID of the child stored in the pkLink paramter.
     * @param rkStream, the Stream where the child objects are stored.
     * @param pkLink, the Link class from which the child object IDs are read.
     */
    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    /**
     * Registers this object with the input Stream parameter. All objects
     * streamed to disk are registered with the Stream so that a unique list
     * of objects is maintained.
     * @param rkStream, the Stream where the child objects are stored.
     * @return true if this object is registered, false if the object has
     * already been registered.
     */
    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
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

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
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

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
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
