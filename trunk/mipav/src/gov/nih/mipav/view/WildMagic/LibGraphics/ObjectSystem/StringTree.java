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

package gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem;

import java.util.Vector;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.swing.tree.*;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class StringTree
{
    // construction and destruction
    public StringTree () {}

    public void finalize ()
    {
        m_kStrings.clear();
        m_kChildren.clear();
    }

    // strings
    public int GetStringQuantity ()
    {
        return m_kStrings.size();
    }

    public void SetString (int i, String acString)
    {
        assert(0 <= i && i < m_kStrings.size());
        m_kStrings.set(i, acString);
    }

    public String GetString (int i)
    {
        assert(0 <= i && i < m_kStrings.size());
        return m_kStrings.get(i);
    }

    public void Append (String acString)
    {
        m_kStrings.add(acString);
    }

    // children
    public int GetChildQuantity ()
    {
        return m_kChildren.size();
    }

    public void SetChild (int i, StringTree pkChild)
    {
        assert(0 <= i && i < m_kStrings.size());
        m_kChildren.set(i, pkChild);
    }

    public StringTree GetChild (int i)
    {
        assert(0 <= i && i < m_kChildren.size());
        return m_kChildren.get(i);
    }

    public void Append (StringTree pkChild)
    {
        m_kChildren.add(pkChild);
    }

    // streaming
    public boolean Save (final String acFilename, int iTabSize/* = 4*/)
    {
        File kFile = new File(acFilename);
        if ( !kFile.exists() )
        {
            try {
                kFile.createNewFile();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        if ( !kFile.canWrite() )
        {
            return false;
        }
        FileOutputStream kFileWriter = null;
        try {
            kFileWriter = new FileOutputStream( kFile );
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        try {
            Save(kFileWriter,0,iTabSize);
        } catch (IOException e1) {
            e1.printStackTrace();
        }
        try {
            kFileWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return true;
    }

    public static String FormatFloat (float fValue)
    {
        if (fValue > -Float.MAX_VALUE)
        {
            if (fValue < Float.MAX_VALUE)
            {
                return ( new Float( fValue ).toString() ); 
            }
            else
            {
                return new String( "INFINITY" ); 
            }
        }
        else
        {
            return new String( "-INFINITY" ); 
        }
    }

    public static String FormatDouble (double dValue)
    {
        if (dValue > -Double.MAX_VALUE)
        {
            if (dValue < Double.MAX_VALUE)
            {
                return ( new Double( dValue ).toString() ); 
            }
            else
            {
                return new String( "INFINITY" ); 
            }
        }
        else
        {
            return new String( "-INFINITY" ); 
        }
    }

    // streaming (recursive)
    private void Save (FileOutputStream pkOFile, int iLevel, int iTabSize) throws IOException
    {
        // indent to proper location
        int i, iIndent = iLevel*iTabSize;
        String b = new String(" ");
        byte[] bSpace = b.getBytes();

        if (iIndent > 0)
        {
            for (i = 0; i < iIndent; i++)
            {
                pkOFile.write( bSpace );
            }
        }

        // label with level
        if (iLevel < 10)
        {
            String kS = new String( iLevel + ":  " );
            pkOFile.write( kS.getBytes() );
        }
        else
        {
            String kS = new String( iLevel + ": " );
            pkOFile.write( kS.getBytes() );
        }

        // header string
        if (m_kStrings.size() > 0)
        {
            String kS = new String( m_kStrings.get(0) + "\n" );
            pkOFile.write( kS.getBytes() );
        }
        else
        {
            String kS = new String( "<no header>\n" );
            pkOFile.write( kS.getBytes() );
        }

        // body strings
        iIndent += 4;

        int j;
        for (j = 1; j < m_kStrings.size(); j++)
        {
            for (i = 0; i < iIndent; i++)
            {
                pkOFile.write( bSpace );
            }
            String kS = new String( m_kStrings.get(j) + "\n" );
            pkOFile.write( kS.getBytes() );
        }

        iLevel++;
        for (j = 0; j < m_kChildren.size(); j++)
        {
            m_kChildren.get(j).Save(pkOFile,iLevel,iTabSize);
        }
    }

    public void CreateNodes (DefaultMutableTreeNode kTop)
    {
        // header string
        String kHeader = new String( "<no header>\n" );
        if (m_kStrings.size() > 0)
        {
            kHeader = new String( m_kStrings.get(0) + "\n" );
        }

        DefaultMutableTreeNode kNode = new DefaultMutableTreeNode( kHeader );
        kTop.add(kNode);

        if (m_kChildren.size() > 0)
        {
            m_kChildren.get(0).CreateNodes(kNode);
        }

        for (int i = 1; i < m_kStrings.size(); i++)
        {
            String kS = new String( m_kStrings.get(i) );
            kNode.add( new DefaultMutableTreeNode( kS ) );
        }

        for (int i = 1; i < m_kChildren.size(); i++)
        {
            m_kChildren.get(i).CreateNodes(kNode);
        }
    }



    private Vector<String> m_kStrings = new Vector<String>();
    private Vector<StringTree> m_kChildren = new Vector<StringTree>();

    // string creation helpers (native types)
    public static String Format (final Class pkRTTI, final String acName)
    {
        assert(pkRTTI != null);
        final String acRTTIName = pkRTTI.getName();

        if (acName != null)
        {
            return new String( acRTTIName + " <" + acName + ">" );
        }
        return new String( acRTTIName );
    }

    public static String Format (final String acString)
    {
        if (acString != null)
        {
            return new String(acString);
        }
        return new String("<no title>");
    }

    public static String Format (final String acPrefix, boolean bValue)
    {
        assert(acPrefix != null);
        if (bValue)
        {
            return new String( acPrefix + " true" );
        }
        return new String( acPrefix + " false" );
    }

    public static String Format (final String acPrefix, char cValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + cValue );
    }

    public static String Format (final String acPrefix, byte ucValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + ucValue );
    }

    public static String Format (final String acPrefix, short sValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + sValue );
    }

    public static String Format (final String acPrefix, int iValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + iValue );
    }

    public static String Format (final String acPrefix, long lValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + lValue );
    }

    public static String Format (final String acPrefix, float fValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + fValue );
    }

    public static String Format (final String acPrefix, double dValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + dValue );
    }

    public static String Format (final String acPrefix, final String sValue)
    {
        assert(acPrefix != null);
        if ( sValue != null )
        {
            return new String( acPrefix + " " + sValue );
        }
        return new String( acPrefix );
    }


    // string creation helpers (nonnative types)
    public static String Format (final String acPrefix, final BoundingVolume pkValue)
    {
        assert(acPrefix != null);
        String acX = StringTree.FormatFloat(pkValue.GetCenter().X());
        String acY = StringTree.FormatFloat(pkValue.GetCenter().Y());
        String acZ = StringTree.FormatFloat(pkValue.GetCenter().Z());
        String acR = StringTree.FormatFloat(pkValue.GetRadius());

        return new String( acPrefix + " " + 
                           "(x: " + acX + ", y: " + acY + ", z: " + acZ + ", r: " + acR + ")" );
    }

    public static String Format (final String acPrefix, final ColorRGBA rkValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + 
                           "(r: " + rkValue.R() + ", g: " + rkValue.G() + ", b: " + rkValue.B() + ", a: " + rkValue.A() + ")" );
    }

    public static String Format (final String acPrefix, final ColorRGB rkValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + 
                           "(r: " + rkValue.R() + ", g: " + rkValue.G() + ", b: " + rkValue.B() + ")" );
    }

    public static String Format (final String acPrefix, final Line3f rkValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + 
                           "(px: " + rkValue.Origin.X() +
                           ", py: " + rkValue.Origin.Y() +
                           ", pz: " + rkValue.Origin.Z() +
                           ", dx: " + rkValue.Direction.X() +
                           ", dy: " + rkValue.Direction.Y() +
                           ", dz: " + rkValue.Direction.Z() + ")" );
    }

    public static String Format (final String acPrefix, final Matrix3f rkValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " +
                           "[c0:(" + rkValue.GetData(0,0) +
                           "," + rkValue.GetData(1,0) +
                           "," + rkValue.GetData(2,0) +
                           "),c1:(" + rkValue.GetData(0,1) +
                           "," + rkValue.GetData(1,1) +
                           "," + rkValue.GetData(2,1) +
                           "),c2:(" + rkValue.GetData(0,2) +
                           "," + rkValue.GetData(1,2) +
                           "," + rkValue.GetData(2,2) + ")]" );
    }


    public static String Format (final String acPrefix, final Plane3f rkValue)
    {
        assert(acPrefix != null);
        return new String(acPrefix + " " + 
                          "(nx: " + rkValue.Normal.X() +
                          ", ny: " + rkValue.Normal.Y() +
                          ", nz: " + rkValue.Normal.Z() +
                          ", c: " + rkValue.Constant + ")" );
    }

    public static String Format (final String acPrefix, final Vector2f rkValue )
    {
        assert(acPrefix != null);
        String acX = StringTree.FormatFloat(rkValue.X());
        String acY = StringTree.FormatFloat(rkValue.Y());

        return new String( acPrefix + " " +
                           "(x: " + acX +
                           ", y: " + acY + ")" );
    }

    public static String Format (final String acPrefix, final Vector3f rkValue )
    {
        assert(acPrefix != null);
        String acX = StringTree.FormatFloat(rkValue.X());
        String acY = StringTree.FormatFloat(rkValue.Y());
        String acZ = StringTree.FormatFloat(rkValue.Z());

        return new String( acPrefix + " " +
                           "(x: " + acX +
                           ", y: " + acY +
                           ", z: " + acZ + ")" );
    }
    public static StringTree Format (final String acTitle, int iQuantity, final int[] aiValue)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format(acTitle));

        pkTree.Append(StringTree.Format("quantity =",iQuantity));
        for (int i = 0; i < iQuantity; i++)
        {
            String kPrefix = new String( i + ": ");
            pkTree.Append(StringTree.Format(kPrefix,aiValue[i]));
        }

        return pkTree;
    }

    public static StringTree Format (final String acTitle, int iQuantity, final float[] afValue)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format(acTitle));

        pkTree.Append(StringTree.Format("quantity =",iQuantity));
        for (int i = 0; i < iQuantity; i++)
        {
            String kPrefix = new String( i + ": ");
            pkTree.Append(StringTree.Format(kPrefix,afValue[i]));
        }

        return pkTree;
    }

}
