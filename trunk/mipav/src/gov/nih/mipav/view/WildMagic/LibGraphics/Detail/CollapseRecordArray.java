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
// Version: 4.0.1 (2006/08/07)


package gov.nih.mipav.view.WildMagic.LibGraphics.Detail;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import com.sun.opengl.util.BufferUtil;


public class CollapseRecordArray extends GraphicsObject
{
    public CollapseRecordArray ()
    {
        this( 0, null );
    }

    public CollapseRecordArray (int iQuantity, CollapseRecord[] akArray)
    {
        m_iQuantity = iQuantity;
        m_akArray = akArray;
    }

    //public CollapseRecordArray (const CollapseRecordArray& rkShared);
    public void dispose ()
    {
        m_akArray = null;
    }


    public int GetQuantity ()
    {
        return m_iQuantity;
    }

    public CollapseRecord[] GetData ()
    {
        return m_akArray;
    }
    
    protected int m_iQuantity;
    protected CollapseRecord[] m_akArray;

    //----------------------------------------------------------------------------
    // streaming
    //----------------------------------------------------------------------------
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);
        m_iQuantity = rkStream.ReadInt();
        m_akArray = new CollapseRecord[m_iQuantity];

        for (int i = 0; i < m_iQuantity; i++)
        {
            m_akArray[i].VKeep = rkStream.ReadInt();
            m_akArray[i].VThrow = rkStream.ReadInt();
            m_akArray[i].VQuantity = rkStream.ReadInt();
            m_akArray[i].TQuantity = rkStream.ReadInt();
            m_akArray[i].IQuantity = rkStream.ReadInt();
            if (m_akArray[i].IQuantity > 0)
            {
                m_akArray[i].Index = new int[m_akArray[i].IQuantity];
                rkStream.Read(m_akArray[i].IQuantity,m_akArray[i].Index);
            }
            else
            {
                m_akArray[i].Index = null;
            }
        }
    }

    //----------------------------------------------------------------------------
    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }
    //----------------------------------------------------------------------------
    public boolean Register (Stream rkStream) 
    {
        return super.Register(rkStream);
    }
    //----------------------------------------------------------------------------
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);
        rkStream.Write(m_iQuantity);

        for (int i = 0; i < m_iQuantity; i++)
        {
            rkStream.Write(m_akArray[i].VKeep);
            rkStream.Write(m_akArray[i].VThrow);
            rkStream.Write(m_akArray[i].VQuantity);
            rkStream.Write(m_akArray[i].TQuantity);
            rkStream.Write(m_akArray[i].IQuantity);
            if (m_akArray[i].IQuantity > 0)
            {
                rkStream.Write(m_akArray[i].IQuantity,m_akArray[i].Index);
            }
        }
    }
    //----------------------------------------------------------------------------
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        int iSize = super.GetDiskUsed(rkVersion) +
            BufferUtil.SIZEOF_INT;

        for (int i = 0; i < m_iQuantity; i++)
        {
            iSize += BufferUtil.SIZEOF_INT; // sizeof(m_akArray[i].VKeep);
            iSize += BufferUtil.SIZEOF_INT; // sizeof(m_akArray[i].VThrow);
            iSize += BufferUtil.SIZEOF_INT; // sizeof(m_akArray[i].VQuantity);
            iSize += BufferUtil.SIZEOF_INT; // sizeof(m_akArray[i].TQuantity);
            iSize += BufferUtil.SIZEOF_INT; // sizeof(m_akArray[i].IQuantity);
            iSize += m_akArray[i].IQuantity*BufferUtil.SIZEOF_INT; // sizeof(m_akArray[i].Index[0]);
        }

        return iSize;
    }
    //----------------------------------------------------------------------------
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();

        // strings
        pkTree.Append(StringTree.Format("CollapseRecordArray",GetName()));

        // children
        pkTree.Append(super.SaveStrings(acTitle));

        StringTree pkChild = new StringTree();
        pkChild.Append(StringTree.Format("records"));
        pkChild.Append(StringTree.Format("quantity =",m_iQuantity));
        for (int i = 0; i < m_iQuantity; i++)
        {
            CollapseRecord rkRecord = m_akArray[i];
            String acString = new String( i + 
                                          ": keep = " + rkRecord.VKeep +
                                          ", throw = " + rkRecord.VThrow + 
                                          ", vq = " + rkRecord.VQuantity +
                                          ", tq = " + rkRecord.TQuantity +
                                          ", iq = " + rkRecord.IQuantity    );
            
            pkChild.Append(acString);
        }
        pkTree.Append(pkChild);
        
        return pkTree;
    }
    //----------------------------------------------------------------------------


}
