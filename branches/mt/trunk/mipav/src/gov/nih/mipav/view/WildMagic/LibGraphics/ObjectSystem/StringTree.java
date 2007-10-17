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
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.swing.tree.*;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class StringTree
{
    /** default construction */
    public StringTree () {}

    /** delete memory */
    public void finalize ()
    {
        m_kStrings.clear();
        m_kStrings = null;
        m_kChildren.clear();
        m_kChildren = null;
    }

    /** return number of strings */
    public int GetStringQuantity ()
    {
        return m_kStrings.size();
    }
    /**
     * Set string at position i
     * @param i, index into list
     * @param acString string to add to list
     */
    public void SetString (int i, String acString)
    {
        assert(0 <= i && i < m_kStrings.size());
        m_kStrings.set(i, acString);
    }
    /**
     * Get string at position i
     * @param i, index into list
     * @return string at position i
     */
    public String GetString (int i)
    {
        assert(0 <= i && i < m_kStrings.size());
        return m_kStrings.get(i);
    }
    /**
     * Append string to the end of the list
     * @param acString, string to append
     */
    public void Append (String acString)
    {
        m_kStrings.add(acString);
    }

    /**
     * Return the number of children StringTree nodes
     * @return the number of children StringTree nodes
     */
    public int GetChildQuantity ()
    {
        return m_kChildren.size();
    }
    /**
     * Set child i
     * @param i, index to set
     * @param pkChild, StringTree child
     */
    public void SetChild (int i, StringTree pkChild)
    {
        assert(0 <= i && i < m_kStrings.size());
        m_kChildren.set(i, pkChild);
    }
    /**
     * Get child at position i
     * @param i, index
     * @return child at index i
     */
    public StringTree GetChild (int i)
    {
        assert(0 <= i && i < m_kChildren.size());
        return m_kChildren.get(i);
    }
    /**
     * Append child to end of list.
     * @param pkChild, child to append.
     */
    public void Append (StringTree pkChild)
    {
        m_kChildren.add(pkChild);
    }
    /**
     * Save the StringTree and children to the file.
     * @param acFilename, filename
     * @param iTabSize, number of spaces in a 'tab'
     * @return true on success.
     */
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
    /**
     * Format float into String, prints INFINITY or -INFINITY if < or >
     * Float.MAX_VALUE
     * @param fValue value to print
     * @return formatted string
     */
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
    /**
     * Format double into String, prints INFINITY or -INFINITY if < or >
     * Double.MAX_VALUE
     * @param dValue value to print
     * @return formatted string
     */
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

    /**
     * Save this StringTree to the FileOutputStream
     * @param pkOFile, output stream
     * @param iLevel, current level to indent
     * @param iTabSize, number os spaces in a 'tab'
     * @throws IOException
     */
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
    /**
     * Creates nodes for the JTree GUI
     * @param kTop, top-level node.
     */
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


    /** String belonging to the current node: */
    private Vector<String> m_kStrings = new Vector<String>();
    /** StringTree children: */
    private Vector<StringTree> m_kChildren = new Vector<StringTree>();

    /** string creation helpers (native types)
     * @param pkRTTI, Class object
     * @param acName, Name
     * @return formatted string
     */
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

    /** string creation helpers (native types)
     * @param acString string to format or <no title>
     * @return formatted string
     */
    public static String Format (final String acString)
    {
        if (acString != null)
        {
            return new String(acString);
        }
        return new String("<no title>");
    }

    /** string creation helpers (native types)
     * @param acPrefix string prefix
     * @param bValue boolean value to print 
     * @return formatted string
     */
    public static String Format (final String acPrefix, boolean bValue)
    {
        assert(acPrefix != null);
        if (bValue)
        {
            return new String( acPrefix + " true" );
        }
        return new String( acPrefix + " false" );
    }

    /** string creation helpers (native types)
     * @param acPrefix string prefix
     * @param cValue char value to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, char cValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + cValue );
    }

    /** string creation helpers (native types)
     * @param acPrefix string prefix
     * @param ucValue byte value to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, byte ucValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + ucValue );
    }

    /** string creation helpers (native types)
     * @param acPrefix string prefix
     * @param sValue short value to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, short sValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + sValue );
    }

    /** string creation helpers (native types)
     * @param acPrefix string prefix
     * @param iValue int value to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, int iValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + iValue );
    }

    /** string creation helpers (native types)
     * @param acPrefix string prefix
     * @param lValue long value to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, long lValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + lValue );
    }

    /** string creation helpers (native types)
     * @param acPrefix string prefix
     * @param fValue float value to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, float fValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + fValue );
    }

    /** string creation helpers (native types)
     * @param acPrefix string prefix
     * @param dValue double value to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, double dValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + dValue );
    }

    /** string creation helpers (native types)
     * @param acPrefix string prefix
     * @param sValue string to append
     * @return formatted string
     */
    public static String Format (final String acPrefix, final String sValue)
    {
        assert(acPrefix != null);
        if ( sValue != null )
        {
            return new String( acPrefix + " " + sValue );
        }
        return new String( acPrefix );
    }


    /** string creation helpers (non-native types)
     * @param acPrefix string prefix
     * @param pkValue Boundvolume to print
     * @return formatted string
     */
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

    /** string creation helpers (non-native types)
     * @param acPrefix string prefix
     * @param rkValue ColorRGBA to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, final ColorRGBA rkValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + 
                           "(r: " + rkValue.R() + ", g: " + rkValue.G() + ", b: " + rkValue.B() + ", a: " + rkValue.A() + ")" );
    }

    /** string creation helpers (non-native types)
     * @param acPrefix string prefix
     * @param rkValue ColorRGB to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, final ColorRGB rkValue)
    {
        assert(acPrefix != null);
        return new String( acPrefix + " " + 
                           "(r: " + rkValue.R() + ", g: " + rkValue.G() + ", b: " + rkValue.B() + ")" );
    }

    /** string creation helpers (non-native types)
     * @param acPrefix string prefix
     * @param rkValue Line3f to print
     * @return formatted string
     */
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

    /** string creation helpers (non-native types)
     * @param acPrefix string prefix
     * @param rkValue Matrix3f to print
     * @return formatted string
     */
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


    /** string creation helpers (non-native types)
     * @param acPrefix string prefix
     * @param rkValue Plane3f to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, final Plane3f rkValue)
    {
        assert(acPrefix != null);
        return new String(acPrefix + " " + 
                          "(nx: " + rkValue.Normal.X() +
                          ", ny: " + rkValue.Normal.Y() +
                          ", nz: " + rkValue.Normal.Z() +
                          ", c: " + rkValue.Constant + ")" );
    }

    /** string creation helpers (non-native types)
     * @param acPrefix string prefix
     * @param rkValue Vector values to print
     * @return formatted string
     */
    public static String Format (final String acPrefix, final Vector2f rkValue )
    {
        assert(acPrefix != null);
        String acX = StringTree.FormatFloat(rkValue.X());
        String acY = StringTree.FormatFloat(rkValue.Y());

        return new String( acPrefix + " " +
                           "(x: " + acX +
                           ", y: " + acY + ")" );
    }

    /** string creation helpers (non-native types)
     * @param acPrefix string prefix
     * @param rkValue Vector values to print
     * @return formatted string
     */
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

    /** string creation helpers (non-native types)
     * @param acTitle title of sub-string
     * @param iQuantity number of ints
     * @param aiValue int array to print
     * @return formatted string
     */
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

    /** string creation helpers (non-native types)
     * @param acTitle title of sub-string
     * @param iQuantity number of floats
     * @param afValue float array to print
     * @return formatted string
     */
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
