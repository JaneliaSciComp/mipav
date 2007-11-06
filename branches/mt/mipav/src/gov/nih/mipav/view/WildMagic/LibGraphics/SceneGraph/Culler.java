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

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class Culler
{
    /** Construction and destruction.  The first two input parameters are used
     * to create the set of potentially visible objects.  If the camera is
     * not passed to the constructor, you should set it using SetCamera before
     * calling ComputeVisibleSet.
     * @param iMaxQuantity, maximum number of visible objects.
     * @param iGrowBy, number to grow by.
     * @param pkCamera, Camera for computing what's visible.
     */
    public Culler (int iMaxQuantity, int iGrowBy,
                   final Camera pkCamera)
    {
        m_kVisible = new VisibleSet(iMaxQuantity,iGrowBy);
        m_pkCamera = pkCamera;
        m_iPlaneQuantity = 6;

        // The data members m_afFrustum, m_akPlane, and m_uiPlaneState are
        // uninitialized.  They are initialized in the GetVisibleSet call.
        
        for ( int i = 0; i < VS_MAX_PLANE_QUANTITY; i++ )
        {
            m_akPlane[i] = new Plane3f();
        }
    }

    /** Delete memory. */
    public void finalize()
    {
        m_pkCamera = null;
        m_afFrustum = null;
        for ( int i = 0; i < VS_MAX_PLANE_QUANTITY; i++ )
        {
            m_akPlane[i].finalize();
            m_akPlane[i] = null;
        }
        m_akPlane = null;

        if ( m_kVisible != null )
        {
            m_kVisible.finalize();
            m_kVisible = null;
        }
    }

    /** Access to the camera.
     * @param pkCamera, Camera for computing what's visible.
     */
    public final void SetCamera (final Camera pkCamera)
    {
        m_pkCamera = pkCamera;
    }

    /** Access to the camera.
     * @return Camera for computing what's visible.
     */
    public final Camera GetCamera ()
    {
        return m_pkCamera;
    }

    /** Access to frustum copy.
     * @param afFrustum, new frustum. 
     */
    public void SetFrustum (final float[] afFrustum)
    {
        assert(m_pkCamera != null);
        if (m_pkCamera == null)
        {
            return;
        }

        // copy the frustum values
        for ( int i = 0; i < Camera.ViewFrustum.VF_QUANTITY.Value(); i++ )
        {
            m_afFrustum[i] = afFrustum[i];
        }
        float fDMin2 = m_afFrustum[Camera.ViewFrustum.VF_DMIN.Value()]*m_afFrustum[Camera.ViewFrustum.VF_DMIN.Value()];
        float fRMin2 = m_afFrustum[Camera.ViewFrustum.VF_RMIN.Value()]*m_afFrustum[Camera.ViewFrustum.VF_RMIN.Value()];
        float fRMax2 = m_afFrustum[Camera.ViewFrustum.VF_RMAX.Value()]*m_afFrustum[Camera.ViewFrustum.VF_RMAX.Value()];
        float fUMin2 = m_afFrustum[Camera.ViewFrustum.VF_UMIN.Value()]*m_afFrustum[Camera.ViewFrustum.VF_UMIN.Value()];
        float fUMax2 = m_afFrustum[Camera.ViewFrustum.VF_UMAX.Value()]*m_afFrustum[Camera.ViewFrustum.VF_UMAX.Value()];

        // get the camera coordinate frame
        Vector3f kLoc = m_pkCamera.GetLocation();
        Vector3f kDVec = m_pkCamera.GetDVector();
        Vector3f kUVec = m_pkCamera.GetUVector();
        Vector3f kRVec = m_pkCamera.GetRVector();
        float fDdE = kDVec.Dot(kLoc);

        // update the near plane
        m_akPlane[Camera.ViewFrustum.VF_DMIN.Value()].Normal.SetData(kDVec);
        m_akPlane[Camera.ViewFrustum.VF_DMIN.Value()].Constant =
            fDdE + m_afFrustum[Camera.ViewFrustum.VF_DMIN.Value()];

        // update the far plane
        m_akPlane[Camera.ViewFrustum.VF_DMAX.Value()].Normal.SetData(kDVec);
        m_akPlane[Camera.ViewFrustum.VF_DMAX.Value()].Normal.negEquals();
        m_akPlane[Camera.ViewFrustum.VF_DMAX.Value()].Constant =
            -(fDdE + m_afFrustum[Camera.ViewFrustum.VF_DMAX.Value()]);

        // update the bottom plane
        float fInvLength = (float)(1.0f/Math.sqrt(fDMin2 + fUMin2));
        float fC0 = -m_afFrustum[Camera.ViewFrustum.VF_UMIN.Value()]*fInvLength;  // D component
        float fC1 = +m_afFrustum[Camera.ViewFrustum.VF_DMIN.Value()]*fInvLength;  // U component
        //m_akPlane[Camera.ViewFrustum.VF_UMIN.Value()].Normal = kDVec.scale(fC0).add( kUVec.scale(fC1) );
        Vector3f kTemp1 = new Vector3f( kUVec );
        kTemp1.scaleEquals(fC1);
        Vector3f kTemp2 = new Vector3f( kDVec );
        kTemp2.scaleEquals(fC0);
        kTemp1.addEquals(kTemp2);
        m_akPlane[Camera.ViewFrustum.VF_UMIN.Value()].Normal.SetData( kTemp1 );
        m_akPlane[Camera.ViewFrustum.VF_UMIN.Value()].Constant =
            kLoc.Dot(m_akPlane[Camera.ViewFrustum.VF_UMIN.Value()].Normal);

        // update the top plane
        fInvLength = (float)(1.0f/Math.sqrt(fDMin2 + fUMax2));
        fC0 = +m_afFrustum[Camera.ViewFrustum.VF_UMAX.Value()]*fInvLength;  // D component
        fC1 = -m_afFrustum[Camera.ViewFrustum.VF_DMIN.Value()]*fInvLength;  // U component
        //m_akPlane[Camera.ViewFrustum.VF_UMAX.Value()].Normal = kDVec.scale(fC0).add( kUVec.scale(fC1) );
        kTemp1.SetData( kUVec );
        kTemp1.scaleEquals(fC1);
        kTemp2.SetData( kDVec );
        kTemp2.scaleEquals(fC0);
        kTemp1.addEquals(kTemp2);
        m_akPlane[Camera.ViewFrustum.VF_UMAX.Value()].Normal.SetData( kTemp1 );
        m_akPlane[Camera.ViewFrustum.VF_UMAX.Value()].Constant =
            kLoc.Dot(m_akPlane[Camera.ViewFrustum.VF_UMAX.Value()].Normal);

        // update the left plane
        fInvLength = (float)(1.0f/Math.sqrt(fDMin2 + fRMin2));
        fC0 = -m_afFrustum[Camera.ViewFrustum.VF_RMIN.Value()]*fInvLength;  // D component
        fC1 = +m_afFrustum[Camera.ViewFrustum.VF_DMIN.Value()]*fInvLength;  // R component
        //m_akPlane[Camera.ViewFrustum.VF_RMIN.Value()].Normal = kDVec.scale(fC0).add( kRVec.scale(fC1) );
        kTemp1.SetData(kRVec);
        kTemp1.scaleEquals(fC1);
        kTemp2.SetData(kDVec);
        kTemp2.scaleEquals(fC0);
        kTemp1.addEquals(kTemp2);
        m_akPlane[Camera.ViewFrustum.VF_RMIN.Value()].Normal.SetData(kTemp1);
        m_akPlane[Camera.ViewFrustum.VF_RMIN.Value()].Constant =
            kLoc.Dot(m_akPlane[Camera.ViewFrustum.VF_RMIN.Value()].Normal);

        // update the right plane
        fInvLength = (float)(1.0f/Math.sqrt(fDMin2 + fRMax2));
        fC0 = +m_afFrustum[Camera.ViewFrustum.VF_RMAX.Value()]*fInvLength;  // D component
        fC1 = -m_afFrustum[Camera.ViewFrustum.VF_DMIN.Value()]*fInvLength;  // R component
        //m_akPlane[Camera.ViewFrustum.VF_RMAX.Value()].Normal = kDVec.scale(fC0).add( kRVec.scale(fC1) );
        m_akPlane[Camera.ViewFrustum.VF_RMAX.Value()].Normal.SetData(kTemp1);
        m_akPlane[Camera.ViewFrustum.VF_RMAX.Value()].Constant =
            kLoc.Dot(m_akPlane[Camera.ViewFrustum.VF_RMAX.Value()].Normal);

        // all planes are active initially
        m_uiPlaneState = ~0;
    }

    /** Access to frustum copy.
     * @return current frustum. 
     */
    public final float[] GetFrustum ()
    {
        return m_afFrustum;
    }

    /** Access to the potentially visible set.
     * @return current visible set.
     */
    public final VisibleSet GetVisibleSet ()
    {
        return m_kVisible;
    }

    /** The base class behavior creates a VisibleObject from the input and
     * appends it to the end of the VisibleObject array.  Derived classes
     * may override this behavior; for example, the array might be maintained
     * as a sorted array for minimizing render state changes or it might be
     * maintained as a unique list of objects for a portal system.
     * @param pkObject, object to add to the visible object set.
     * @param pkGlobalEffect, global effect applied to object.
     */
    public void Insert (Spatial pkObject, Effect pkGlobalEffect)
    {
        m_kVisible.Insert(pkObject,pkGlobalEffect);
    }

    /** Access to the stack of world culling planes.  You may push and pop
     * planes to be used in addition to the view frustum planes.  PushPlane
     * requires the input plane to be in world coordinates.
     * public enum { VS_MAX_PLANE_QUANTITY = 32 };
     */
    public static final int VS_MAX_PLANE_QUANTITY = 32;

    /** Get the number of culling planes.
     * @return the number of culling planes.
     */
    public final int GetPlaneQuantity ()
    {
        return m_iPlaneQuantity;
    }

    /** Get the culling planes.
     * @return the culling planes.
     */
    public final Plane3f[] GetPlanes ()
    {
        return m_akPlane;
    }

    /** Set the culling plane state.
     * @param uiPlaneState the culling plane state.
     */
    public final void SetPlaneState (int uiPlaneState)
    {
        m_uiPlaneState = uiPlaneState;
    }

    /** Get the culling plane state.
     * @return the culling plane state.
     */
    public final int GetPlaneState ()
    {
        return m_uiPlaneState;
    }

    /**
     * Add a plane to the culling planes, if the current number of planes has not been
     * exceeded.
     * @param rkPlane, plane to add.
     */
    public void PushPlane (Plane3f rkPlane)
    {
        if (m_iPlaneQuantity < VS_MAX_PLANE_QUANTITY)
        {
            // The number of user-defined planes is limited.
            m_akPlane[m_iPlaneQuantity++] = rkPlane;
        }
    }

    /**
     * Remove the last plane from the culling planes, if the current number of
     * planes is not already 0.
     */
    public void PopPlane ()
    {
        if (m_iPlaneQuantity > Camera.ViewFrustum.VF_QUANTITY.Value())
        {
            // Frustum planes may not be removed from the stack.
            m_iPlaneQuantity--;
        }
    }

    /** Compare the object's world bounding volume against the culling planes.
     * Only Spatial calls this function.
     * @param pkBound, input bounding volume to compare.
     * @return false if object is on negative side of planes and is culled,
     * true otherwise.
     */
    public boolean IsVisible (final BoundingVolume pkBound)
    {
        // Start with the last pushed plane, which is potentially the most
        // restrictive plane.
        int iP = m_iPlaneQuantity - 1;
        int uiMask = 1 << iP;

        for (int i = 0; i < m_iPlaneQuantity; i++, iP--, uiMask >>= 1)
        {
            if ((m_uiPlaneState & uiMask) != 0)
            {
                int iSide = pkBound.WhichSide(m_akPlane[iP]);

                if (iSide < 0)
                {
                    // Object is on negative side.  Cull it.
                    return false;
                }

                if (iSide > 0)
                {
                    // Object is on positive side of plane.  There is no need to
                    // compare subobjects against this plane, so mark it as
                    // inactive.
                    m_uiPlaneState &= ~uiMask;
                }
            }
        }

        return true;
    }

    /** Support for Portal::GetVisibleSet.
     * @param iVertexQuantity, number of vertices
     * @param akVertex, vertices.
     * @param bIgnoreNearPlane.
     * @return false if the polygon is totally outside this plane, true otherwise.
     */
    public boolean IsVisible (int iVertexQuantity, final Vector3f[] akVertex,
                              boolean bIgnoreNearPlane)
    {
        // The Boolean variable bIgnoreNearPlane should be set to 'true' when
        // the test polygon is a portal.  This avoids the situation when the
        // portal is in the view pyramid (eye+left/right/top/bottom), but is
        // between the eye and near plane.  In such a situation you do not want
        // the portal system to cull the portal.  This situation typically occurs
        // when the camera moves through the portal from current region to
        // adjacent region.

        // Start with last pushed plane, which is potentially the most
        // restrictive plane.
        int iP = m_iPlaneQuantity - 1;
        for (int i = 0; i < m_iPlaneQuantity; i++, iP--)
        {
            Plane3f rkPlane = m_akPlane[iP];
            if (bIgnoreNearPlane && iP == Camera.ViewFrustum.VF_DMIN.Value())
            {
                continue;
            }

            int iV;
            for (iV = 0; iV < iVertexQuantity; iV++)
            {
                int iSide = rkPlane.WhichSide(akVertex[iV]);
                if (iSide >= 0)
                {
                    // polygon is not totally outside this plane
                    break;
                }
            }

            if (iV == iVertexQuantity)
            {
                // polygon is totally outside this plane
                return false;
            }
        }

        return true;
    }


    /** Support for BspNode::GetVisibleSet.  Determine if the view frustum is
     * fully on one side of a plane.  The "positive side" of the plane is the
     * half space to which the plane normal points.  The "negative side" is
     * the other half space.
     * @param rkPlane, The input plane is in world coordinates and the world
     * camera coordinate system is used for the test.
     * @return +1 if the view frustum is fully on the positive side of the
     * plane, -1 if the view frustum is fully on the negative side of the
     * plane, or 0 if the view frustum straddles the plane.
     */
    public int WhichSide (final Plane3f rkPlane)
    {
        // The plane is N*(X-C) = 0 where the * indicates dot product.  The signed
        // distance from the camera location E to the plane is N*(E-C).
        float fNdEmC = rkPlane.DistanceTo(m_pkCamera.GetLocation());

        float fNdD = rkPlane.Normal.Dot(m_pkCamera.GetDVector());
        float fNdU = rkPlane.Normal.Dot(m_pkCamera.GetUVector());
        float fNdR = rkPlane.Normal.Dot(m_pkCamera.GetRVector());
        float fFdN = m_afFrustum[Camera.ViewFrustum.VF_DMAX.Value()]/m_afFrustum[Camera.ViewFrustum.VF_DMIN.Value()];

        int iPositive = 0, iNegative = 0;
        float fSgnDist;

        // check near-plane vertices
        float fPDMin = m_afFrustum[Camera.ViewFrustum.VF_DMIN.Value()]*fNdD;
        float fNUMin = m_afFrustum[Camera.ViewFrustum.VF_UMIN.Value()]*fNdU;
        float fNUMax = m_afFrustum[Camera.ViewFrustum.VF_UMAX.Value()]*fNdU;
        float fNRMin = m_afFrustum[Camera.ViewFrustum.VF_RMIN.Value()]*fNdR;
        float fNRMax = m_afFrustum[Camera.ViewFrustum.VF_RMAX.Value()]*fNdR;

        // V = E + dmin*D + umin*U + rmin*R
        // N*(V-C) = N*(E-C) + dmin*(N*D) + umin*(N*U) + rmin*(N*R)
        fSgnDist = fNdEmC + fPDMin + fNUMin + fNRMin;
        if (fSgnDist > 0.0f)
        {
            iPositive++;
        }
        else if (fSgnDist < 0.0f)
        {
            iNegative++;
        }

        // V = E + dmin*D + umin*U + rmax*R
        // N*(V-C) = N*(E-C) + dmin*(N*D) + umin*(N*U) + rmax*(N*R)
        fSgnDist = fNdEmC + fPDMin + fNUMin + fNRMax;
        if (fSgnDist > 0.0f)
        {
            iPositive++;
        }
        else if (fSgnDist < 0.0f)
        {
            iNegative++;
        }

        // V = E + dmin*D + umax*U + rmin*R
        // N*(V-C) = N*(E-C) + dmin*(N*D) + umax*(N*U) + rmin*(N*R)
        fSgnDist = fNdEmC + fPDMin + fNUMax + fNRMin;
        if (fSgnDist > 0.0f)
        {
            iPositive++;
        }
        else if (fSgnDist < 0.0f)
        {
            iNegative++;
        }

        // V = E + dmin*D + umax*U + rmax*R
        // N*(V-C) = N*(E-C) + dmin*(N*D) + umax*(N*U) + rmax*(N*R)
        fSgnDist = fNdEmC + fPDMin + fNUMax + fNRMax;
        if (fSgnDist > 0.0f)
        {
            iPositive++;
        }
        else if (fSgnDist < 0.0f)
        {
            iNegative++;
        }

        // check far-plane vertices (s = dmax/dmin)
        float fPDMax = m_afFrustum[Camera.ViewFrustum.VF_DMAX.Value()]*fNdD;
        float fFUMin = fFdN*fNUMin;
        float fFUMax = fFdN*fNUMax;
        float fFRMin = fFdN*fNRMin;
        float fFRMax = fFdN*fNRMax;

        // V = E + dmax*D + umin*U + rmin*R
        // N*(V-C) = N*(E-C) + dmax*(N*D) + s*umin*(N*U) + s*rmin*(N*R)
        fSgnDist = fNdEmC + fPDMax + fFUMin + fFRMin;
        if (fSgnDist > 0.0f)
        {
            iPositive++;
        }
        else if (fSgnDist < 0.0f)
        {
            iNegative++;
        }

        // V = E + dmax*D + umin*U + rmax*R
        // N*(V-C) = N*(E-C) + dmax*(N*D) + s*umin*(N*U) + s*rmax*(N*R)
        fSgnDist = fNdEmC + fPDMax + fFUMin + fFRMax;
        if (fSgnDist > 0.0f)
        {
            iPositive++;
        }
        else if (fSgnDist < 0.0f)
        {
            iNegative++;
        }

        // V = E + dmax*D + umax*U + rmin*R
        // N*(V-C) = N*(E-C) + dmax*(N*D) + s*umax*(N*U) + s*rmin*(N*R)
        fSgnDist = fNdEmC + fPDMax + fFUMax + fFRMin;
        if (fSgnDist > 0.0f)
        {
            iPositive++;
        }
        else if (fSgnDist < 0.0f)
        {
            iNegative++;
        }

        // V = E + dmax*D + umax*U + rmax*R
        // N*(V-C) = N*(E-C) + dmax*(N*D) + s*umax*(N*U) + s*rmax*(N*R)
        fSgnDist = fNdEmC + fPDMax + fFUMax + fFRMax;
        if (fSgnDist > 0.0f)
        {
            iPositive++;
        }
        else if (fSgnDist < 0.0f)
        {
            iNegative++;
        }

        if (iPositive > 0)
        {
            if (iNegative > 0)
            {
                // frustum straddles the plane
                return 0;
            }

            // frustum is fully on the positive side
            return +1;
        }

        // frustum is fully on the negative side
        return -1;
    }


    /** This is the main function you should use for culling within a scene
     * graph.  Traverse the scene and construct the potentially visible set
     * relative to the world planes.
     * @param pkScene, root node of scene to cull.
     */
    public void ComputeVisibleSet (Spatial pkScene)
    {
        assert((m_pkCamera != null) && (pkScene != null));
        if ((m_pkCamera != null) && (pkScene != null))
        {
            SetFrustum(m_pkCamera.GetFrustum());
            m_kVisible.Clear();
            pkScene.OnGetVisibleSet(this,false);
        }
    }


    /** The input camera has information that might be needed during the
     * culling pass over the scene. */
    protected Camera m_pkCamera;

    /** A copy of the view frustum for the input camera.  This allows various
     * subsystems to change the frustum parameters during culling (for
     * example, the portal system) without affecting the camera, whose initial
     * state is needed by the renderer. */
    protected float[] m_afFrustum = new float[Camera.ViewFrustum.VF_QUANTITY.Value()];

    /** number of culling planes. */ 
    protected int m_iPlaneQuantity;
    /** The world culling planes corresponding to the view frustum plus any
     * additional user-defined culling planes. */ 
    protected Plane3f[] m_akPlane = new Plane3f[VS_MAX_PLANE_QUANTITY];
    /**  The member m_uiPlaneState represents bit flags to store whether or
     * not a plane is active in the culling system.  A bit of 1 means the
     * plane is active, otherwise the plane is inactive.  An active plane is
     * compared to bounding volumes, whereas an inactive plane is not.  This
     * supports an efficient culling of a hierarchy.  For example, if a node's
     * bounding volume is inside the left plane of the view frustum, then the
     * left plane is set to inactive because the children of the node are
     * automatically all inside the left plane.
     */
    protected int m_uiPlaneState;

    /** The potentially visible set for a call to GetVisibleSet. */
    protected VisibleSet m_kVisible;
}
