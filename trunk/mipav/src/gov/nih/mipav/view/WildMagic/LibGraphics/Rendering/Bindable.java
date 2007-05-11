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
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibGraphics.Rendering;
import java.util.Vector;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class Bindable extends WmObject
{
    public Bindable () {}

    // Use this function when the resource has a unique representation in
    // VRAM (all resources except for vertex buffers).
    public ResourceIdentifier GetIdentifier (Renderer pkUser)
    {
        for (int i = 0; i < m_kInfoArray.size(); i++)
        {
            Info rkInfo = m_kInfoArray.get(i);
            if (rkInfo.User == pkUser)
            {
                return rkInfo.ID;
            }
        }

        // The resource is not yet bound to the renderer.
        return null;
    }


    // Use these functions when the resource has multiple representations in
    // VRAM (vertex buffers).
    public int GetInfoQuantity ()
    {
        return m_kInfoArray.size();
    }

    public ResourceIdentifier GetIdentifier (int i, Renderer pkUser)
    {
        if (0 <= i && i < m_kInfoArray.size())
        {
            Info rkInfo = m_kInfoArray.get(i);
            if (rkInfo.User == pkUser)
            {
                return rkInfo.ID;
            }
        }

        // The resource is not yet bound to the renderer.
        return null;
    }

    public void Release ()
    {
        while (m_kInfoArray.size() > 0)
        {
            Info rkInfo = m_kInfoArray.get(0);
            //(rkInfo.User->*rkInfo.Release)(this);
            rkInfo.Release.Release(this);
        }
    }

    //friend class Renderer; these functions were private w/friend
    //accessibility, changed to default package scope:
    protected void OnLoad (Renderer pkUser, ReleaseFunction oRelease,
                 ResourceIdentifier pkID)
    {
        Info kInfo = new Info();
        kInfo.User = pkUser;
        kInfo.Release = oRelease;
        kInfo.ID = pkID;
        m_kInfoArray.add(kInfo);
    }

    protected void OnRelease (Renderer pkUser, ResourceIdentifier pkID)
    {
        int iQuantity = m_kInfoArray.size();
        for (int i = 0; i < iQuantity; i++)
        {
            Info rkInfo = m_kInfoArray.get(i);
            if (rkInfo.User == pkUser && rkInfo.ID == pkID)
            {
                // Move the last array element to the current slot, if necessary.
                if (i < --iQuantity)
                {
                    m_kInfoArray.set(i, m_kInfoArray.get(iQuantity) );
                }

                // Remove the last array element.
                m_kInfoArray.remove(iQuantity);
                return;
            }
        }
    }

    protected class Info
    {
        // The renderer to which the resource is bound.
        Renderer User;

        // The renderer function to call to release the resource.
        ReleaseFunction Release;

        // The identifier of the resource for the renderer's use.
        ResourceIdentifier ID;
    };

    protected Vector<Info> m_kInfoArray = new Vector<Info>();
}
