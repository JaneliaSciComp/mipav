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

package gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem;

import java.util.Vector;
import java.lang.Class;

public abstract class GraphicsObject
{
    // root class of object system
    public GraphicsObject ()
    {
        m_uiID = ms_uiNextID++;
    }
    public void finalize ()
    {
        //DetachAllControllers();
    }
    // copying system
    // public:
    //     Pointer<Object> Copy (bool bUniqueNames = true) const;
    //     static char NameAppend;

    // name-ID system
    public void SetName (String rkName)
    {
        m_kName = rkName;
    }

    public String GetName ()
    {
        return m_kName;
    }

    public int GetID ()
    {
        return m_uiID;
    }

    public static int GetNextID ()
    {
        return ms_uiNextID;
    }

    public GraphicsObject GetObjectByNameBase (final String rkName)
    {
        if (rkName.equals( m_kName ))
        {
            return this;
        }

//         for (int i = 0; i < (int)m_kControllers.size(); i++)
//         {
//             if (m_kControllers[i])
//             {
//                 Object* pkFound = m_kControllers[i]->GetObjectByName(rkName);
//                 if (pkFound)
//                 {
//                     return pkFound;
//                 }
//             }
//         }

        return null;
    }

    public GraphicsObject GetObjectByName (final String rkName)
    {
        if (rkName.equals( m_kName ))
        {
            return this;
        }

//         for (int i = 0; i < (int)m_kControllers.size(); i++)
//         {
//             if (m_kControllers[i])
//             {
//                 Object* pkFound = m_kControllers[i]->GetObjectByName(rkName);
//                 if (pkFound)
//                 {
//                     return pkFound;
//                 }
//             }
//         }

        return null;
    }

    public void GetAllObjectsByName ( final String rkName,
                                     Vector<GraphicsObject> rkObjects )
    {
        if (rkName.equals( m_kName ))
        {
            rkObjects.add(this);
        }

//         for (int i = 0; i < (int)m_kControllers.size(); i++)
//         {
//             if (m_kControllers[i])
//             {
//                 m_kControllers[i]->GetAllObjectsByName(rkName,rkObjects);
//             }
//         }
    }


    public GraphicsObject GetObjectByIDBase ( int uiID )
    {
        if (uiID == m_uiID)
        {
            return this;
        }

//         for (int i = 0; i < (int)m_kControllers.size(); i++)
//         {
//             if (m_kControllers[i])
//             {
//                 Object* pkFound = m_kControllers[i]->GetObjectByID(uiID);
//                 if (pkFound)
//                 {
//                     return pkFound;
//                 }
//             }
//         }

        return null;
    }

    public GraphicsObject GetObjectByID ( int uiID )
    {
        if (uiID == m_uiID)
        {
            return this;
        }

//         for (int i = 0; i < (int)m_kControllers.size(); i++)
//         {
//             if (m_kControllers[i])
//             {
//                 Object* pkFound = m_kControllers[i]->GetObjectByID(uiID);
//                 if (pkFound)
//                 {
//                     return pkFound;
//                 }
//             }
//         }

        return null;
    }

    private String m_kName = new String();
    private int m_uiID;
    private static int ms_uiNextID = 0;

    // streaming system
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        // get old address of object, save it for linking phase

        int iLinkID = rkStream.ReadInt();
        rkStream.InsertInMap(iLinkID,pkLink);
        
        // name of object
        m_kName = rkStream.ReadString();

        // link data
//         int[] iQuantity = new int[1];
//         rkStream.Read(iQuantity);
        //m_kControllers.resize(iQuantity);
//         for (int i = 0; i < iQuantity[0]; i++)
//         {
//             GraphicsObject pkObject;
//             rkStream.Read(pkObject);
//             pkLink.Add(pkObject);
//         }
    }

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
//         for (int i = 0; i < m_kControllers.size(); i++)
//         {
//             Object pkLinkID = pkLink.GetLinkID();
//             m_kControllers.set(i, (Controller)rkStream.GetFromMap(pkLinkID));
//         }
    }

    public boolean Register (Stream rkStream)
    {
        int iID = m_uiID;
        if (rkStream.InsertInMap(iID, rkStream.new Link()))
        {
            // Used to ensure the objects are saved in the order corresponding to
            // a depth-first traversal of the scene.
            rkStream.InsertInOrdered(this);

//             for (int i = 0; i < (int)m_kControllers.size(); i++)
//             {
//                 if (m_kControllers[i])
//                 {
//                     m_kControllers[i].Register(rkStream);
//                 }
//             }

            return true;
        }

        return false;
    }

    public void Save (Stream rkStream)
    {
        // RTTI name for factory lookup on Load
        Class kClass = this.getClass();
        rkStream.Write(kClass.getName());

        // address of object for unique ID on Load/Link
        rkStream.Write(m_uiID);

        // name of object
        rkStream.Write(m_kName);

        // link data
//         int iQuantity = m_kControllers.size();
//         rkStream.Write(iQuantity);
//         for (int i = 0; i < iQuantity; i++)
//         {
//             rkStream.Write(m_kControllers[i]);
//         }
    }

    public int GetDiskUsed (final StreamVersion rkVersion)
    {
        // RTTI name
        Class kClass = this.getClass();
        int iUsed = Stream.SIZEOF_INT + kClass.getName().length();

        // ID of object
        iUsed += Stream.SIZEOF_INT;

        // name of object
        iUsed += Stream.SIZEOF_INT + m_kName.length();

        // controllers
        //iUsed += sizeof(int) + ((int)m_kControllers.size())*sizeof(ControllerPtr);

        return iUsed;
    }

    public StringTree SaveStrings (final String acTitle/* = 0*/)
    {
        StringTree pkTree = new StringTree();
        
        // strings
        pkTree.Append(StringTree.Format("GraphicsObject",GetName()));
        //pkTree.Append(StringTree.Format("this =",this));
        pkTree.Append(StringTree.Format("ID   =",m_uiID));
        //pkTree.Append(StringTree.Format("refs =",0));

        // children
//         if (m_kControllers.size() > 0)
//         {
//             StringTree pkCTree = new StringTree();
//             pkCTree.Append(Format("controllers"));
//             for (int i = 0; i < (int)m_kControllers.size(); i++)
//             {
//                 if (m_kControllers.get(i) != null)
//                 {
//                     pkCTree.Append(m_kControllers.get(i).SaveStrings());
//                 }
//             }
//             pkTree.Append(pkCTree);
//         }

        return pkTree;
    }
}
