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

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import com.sun.opengl.util.BufferUtil;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.DataInputStream;
import java.lang.Class;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class Stream
{
    public static int SIZEOF_INT = 4;
    public static int SIZEOF_FLOAT = 4;
    public static int SIZEOF_BOOLEAN = 1;
    public static int SIZEOF_BYTE = 1;

    /** construction */
    public Stream ()
    {
        m_kMap = new Hashtable<Integer,Link>(1024);
        m_apkTopLevel = new Vector<GraphicsObject>();
        m_kOrdered = new Vector<GraphicsObject>();
        m_iBufferSize = 0;
        m_iBufferNext = 0;
        m_acBuffer = null;
    }
    /** destruction */
    public void dispose()
    {
        m_kMap.clear();
        m_kOrdered.clear();
        RemoveAll();
    }


    /** Add an object to the list of objects to process, each object
     * representing an entry into a connected component of the abstract graph.
     * @param pkObject the objects to process
     * @return true if the object is inserted into the list, false if already
     * inserted
     */
    public boolean Insert (GraphicsObject pkObject)
    {
        if (pkObject != null)
        {
            // an object can only be inserted once
            for (int i = 0; i < m_apkTopLevel.size(); i++)
            {
                if (pkObject == m_apkTopLevel.get(i))
                {
                    return false;
                }
            }

            m_apkTopLevel.add(pkObject);
            return true;
        }

        return false;
    }

    /** Remove an object from the list of objects to process, each object
     * representing an entry into a connected component of the abstract graph.
     * @param pkObject the objects to remove
     * @return true if the object is removed, false otherwise
     */
    public boolean Remove (GraphicsObject pkObject)
    {
        if (pkObject != null)
        {
            for ( int i = 0; i < m_apkTopLevel.size(); i++)
            {
                if (pkObject == m_apkTopLevel.get(i))
                {
                    m_apkTopLevel.remove(i);
                    return true;
                }
            }
        }

        return false;
    }

    /** Remove all objects from the list of objects to process. */
    public void RemoveAll ()
    {
        m_apkTopLevel.clear();
    }

    /** Return the number of objects one the list of objects to process.
     * @return the number of objects one the list of objects to process.
     */
    public int GetObjectCount ()
    {
        return m_apkTopLevel.size();
    }

    /** Return the object at position i on the list.
     * @param i the ith object
     * @return the object at position i on the list.
     */
    public GraphicsObject GetObjectAt (int i)
    {
        if (0 <= i && i < m_apkTopLevel.size())
        {
            return m_apkTopLevel.get(i);
        }
        return null;
    }

    /** Return true if the object is in the TopLevel list
     * @param pkObject the object to check
     * @return true if the object is in the TopLevel list, false otherwise.
     */
    public boolean IsTopLevel (final GraphicsObject pkObject)
    {
        for (int i = 0; i < m_apkTopLevel.size(); i++)
        {
            if (pkObject == m_apkTopLevel.get(i))
            {
                return true;
            }
        }
        return false;
    }


    /** Memory loads and saves.  Stream does not assume responsibility for the
     * char arrays.  The application must manage the input acBuffer for the
     * call to Load and delete the output racBuffer for the call to Save.
     * @param acBuffer the ByteArrayInputStream storing the data to load
     * @param iSize the size of the ByteArrayInputStream storing the data to load
     * @return true if sucess, false on failure.
     */
    public boolean Load (ByteArrayInputStream acBuffer, int iSize)
    {
        m_kMap.clear();
        m_kOrdered.clear();

        // clear out all previous top level objects
        RemoveAll();

        // Initialize the internal Stream buffer.  The m_iBufferNext value is
        // already initialized to zero.
        m_iBufferSize = iSize;
        m_acBuffer = acBuffer;
        m_acDataIn = new DataInputStream( m_acBuffer );

        // load list of unique objects
        String kTopLevel = new String(ms_acTopLevel);
        GraphicsObject pkObject = null;
        while (m_iBufferNext < m_iBufferSize)
        {
            // read "Top Level" or RTTI name
            String kString = new String();
            kString = ReadString();
            boolean bTopLevel = (kString.equals( kTopLevel ));
            if (bTopLevel)
            {
                // read RTTI name
                kString = ReadString();
            }

            // Get the factory function for the type of object about to be read.
            Class kClass;
            try
            {
                kClass = Class.forName(kString);
            } catch ( ClassNotFoundException e )
            {
                assert(false);
                return false;
            }

            // load the object
            Constructor kConstructor = null;
            try {
                kConstructor = kClass.getConstructor((Class[])null);
            } catch (SecurityException e) {
                e.printStackTrace();
                assert(false);
                return false;
            } catch (NoSuchMethodException e) {
                e.printStackTrace();
                assert(false);
                return false;
            }
            try {
                pkObject = (GraphicsObject)kConstructor.newInstance((Object[])null);
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
                assert(false);
                return false;

            } catch (InstantiationException e) {
                e.printStackTrace();
                assert(false);
                return false;
            } catch (IllegalAccessException e) {
                e.printStackTrace();
                assert(false);
                return false;
            } catch (InvocationTargetException e) {
                e.printStackTrace();
                assert(false);
                return false;
            }
            Link pkLink = new Link(pkObject);
            pkObject.Load(this,pkLink);

            // keep track of all top level objects for application use
            if (bTopLevel)
            {
                Insert(pkObject);
            }
        }

        // Link the objects.  Maintain an array of objects for use by the
        // application (if so desired).
        for (Enumeration kE = m_kMap.elements() ; kE.hasMoreElements() ;) {
            Link ppkLink = (Link)kE.nextElement();
            pkObject = ppkLink.GetObject();
            pkObject.Link(this,ppkLink);
            m_kOrdered.add(pkObject);
        }

//         // delete the stream link records
//         ppkLink = m_kMap.GetFirst(&pkObject);
//         while (ppkLink)
//         {
//             WM4_DELETE *ppkLink;
//             ppkLink = m_kMap.GetNext(&pkObject);
//         }

        // reset internal management structures
        m_acBuffer = null;
        m_iBufferSize = 0;
        m_iBufferNext = 0;
        return true;
    }

    /** Memory loads and saves.  Stream does not assume responsibility for the
     * char arrays.  The application must manage the input acBuffer for the
     * call to Load and delete the output racBuffer for the call to Save.
     * @param iSize the size of the buffer storing the data to write
     * @return byte[] containing the scene-graph data
     */
    public byte[] Save (int[] riSize)
    {
        m_kMap.clear();
        m_kOrdered.clear();

        // build list of unique objects
        int i;
        for (i = 0; i < m_apkTopLevel.size(); i++)
        {
            m_apkTopLevel.get(i).Register(this);
        }

        // Accumulate the number of bytes used by object on disk.
        String kTopLevel = new String(ms_acTopLevel);
        int iTLGetDiskUsed = BufferUtil.SIZEOF_INT + (int)kTopLevel.length();
        m_iBufferSize = (m_apkTopLevel.size())*iTLGetDiskUsed;
        for (i = 0; i < m_kOrdered.size(); i++)
        {
            int iUsed = m_kOrdered.get(i).GetDiskUsed(StreamVersion.CURRENT);
            m_iBufferSize += iUsed;
        }

        m_acBufferOut = new ByteArrayOutputStream( m_iBufferSize );
        m_acDataOut = new DataOutputStream( m_acBufferOut );
        m_iBufferNext = 0;

        // save list of unique objects
        for (i = 0; i < m_kOrdered.size(); i++)
        {
            final GraphicsObject pkObject = m_kOrdered.get(i);
            if (IsTopLevel(pkObject))
            {
                Write(kTopLevel);
            }
            pkObject.Save(this);
        }

        // make sure the buffer is exactly filled
        assert(m_iBufferNext == m_iBufferSize);
        if (m_iBufferNext != m_iBufferSize)
        {
            // The buffer is not exactly filled.  This is an indication that some
            // class is either incorrectly reporting the number of disk bytes
            // used (via the GetDiskUsed function) or incorrectly writing the
            // member data to disk.
            assert(false);
            m_acBufferOut = null;
            riSize[0] = 0;
            return null;
        }

        // transfer ownership of buffer to caller
        byte[] racBuffer = m_acBufferOut.toByteArray();
        riSize[0] = m_iBufferSize;

        // reset internal management structures
        m_acBufferOut = null;
        m_iBufferSize = 0;
        m_iBufferNext = 0;
        return racBuffer;
    }


    /** File loads and saves.
     * Load the scene-graph from the file specified
     * @param acFilename the name of the scene-graph file
     * @return true on sucess, false on failure.
     */
    public boolean Load (final String acFilename)
    {
        // load the stream from disk to memory
        ByteArrayInputStream acBuffer = Stream.LoadFile(acFilename);
        if ( acBuffer == null )
        {
            return false;
        }
        int iSize = acBuffer.available();

        // get the file version
        if (iSize < StreamVersion.LENGTH)
        {
            // file does not exist or not large enough to store version string
            acBuffer = null;
            return false;
        }
        byte[] abHeader = new byte[StreamVersion.LENGTH];
        acBuffer.read( abHeader, 0, StreamVersion.LENGTH);
        String kHeader = new String( abHeader );

        m_kVersion = new StreamVersion(kHeader);
        if (!m_kVersion.IsValid())
        {
            // TO DO.  This should not abort if the input stream version is less
            // than current version.

            acBuffer = null;
            return false;
        }

        // reconstruct the scene graph from the buffer
        iSize -= StreamVersion.LENGTH;
        if (!Load(acBuffer,iSize))
        {
            acBuffer = null;
            return false;
        }

        acBuffer = null;
        return true;
    }

    /** File loads and saves.
     * Load the scene-graph from the file specified
     * @param acFilename the name of the scene-graph file
     * @return a ByteArrayInputStream containing the data.
     */
    public static ByteArrayInputStream LoadFile (final String acFilename)
    {
        File kFile = new File(acFilename);
        if ( !kFile.exists() || !kFile.canRead() )
        {
            return null;
        }
        int iLength = (int)kFile.length();
        if ( iLength <= 0 )
        {
            return null;
        }
        try {
            FileInputStream kFileReader = new FileInputStream(kFile);
            byte[] racBuffer = new byte[iLength];
            kFileReader.read(racBuffer,0,iLength);
            kFileReader.close();
            ByteArrayInputStream kByteStream = new ByteArrayInputStream(racBuffer);
            return kByteStream;
        } catch ( FileNotFoundException e1 ) {} catch ( IOException e2 ) {}

        return null;
    }

    /** File loads and saves.
     * Save the scene-graph to the file specified
     * @param acFilename the name of the scene-graph file
     * @return true on sucess, false on failure.
     */
    public boolean Save (final String acFilename)
    {
        // save the file version to disk
        File kFile = new File(acFilename);
        FileOutputStream kFileWriter = null;
        try {
            kFileWriter = new FileOutputStream(kFile);
        } catch ( FileNotFoundException e1 ) {
            return false;
        }

        try {
            kFileWriter.write(StreamVersion.LABEL.getBytes(),0,StreamVersion.LENGTH);
        } catch ( IOException e2 ) {
            return false;
        }

        // decompose the scene graph into the buffer
        int[] iSize = new int[1];
        byte[] acBuffer = Save(iSize);
        if ( acBuffer == null )
        {
            return false;
        }

        try {
            kFileWriter.write(acBuffer,0,iSize[0]);
            kFileWriter.close();
        } catch ( IOException e2 ) {
            acBuffer = null;
            return false;
        }
        acBuffer = null;
        return true;
    }

    /** Access the array of unique objects, ordered by time of visitation in
     * the Register traversal.  You may use this after a Load or a Save call.
     * If you use it after a Load, changing objects returned by the function
     * GetOrderedObject is valid.  If you use it after a Save, changing
     * objects returned by the function will not affect the saved copy, but
     * it will affect the original objects you streamed.
     * @return the number of unique objects.
     */
    public int GetOrderedQuantity ()
    {
        return (int)m_kOrdered.size();
    }

    /** Access the array of unique objects, ordered by time of visitation in
     * the Register traversal.  You may use this after a Load or a Save call.
     * If you use it after a Load, changing objects returned by the function
     * GetOrderedObject is valid.  If you use it after a Save, changing
     * objects returned by the function will not affect the saved copy, but
     * it will affect the original objects you streamed.
     * @param i, the ith object
     * @return the ith object
     */
    public GraphicsObject GetOrderedObject (int i)
    {
        assert(0 <= i && i < (int)m_kOrdered.size());
        if (i < 0 || i >= (int)m_kOrdered.size())
        {
            return null;
        }

        return m_kOrdered.get(i);
    }

    /** support for disk usage
     * @return the size of the scene-graph on disk.
     */
    public int GetDiskUsed ()
    {
        // build list of unique objects
        int i;
        for (i = 0; i < m_apkTopLevel.size(); i++)
        {
            m_apkTopLevel.get(i).Register((Stream)this);
        }

        // accumulate object bytes used
        int iSize = 0;
        for (i = 0; i < m_kOrdered.size(); i++)
        {
            iSize += m_kOrdered.get(i).GetDiskUsed(StreamVersion.CURRENT);
        }

        // clear out the structures (reduce memory, prepare for other calls)
        m_kMap.clear();
        m_kOrdered.clear();
        return iSize;
    }

    /** file save (ASCII text)
     * @param acFilename the name of the file to write
     * @param iTabSize the number of spaces to indent.
     * @return true on sucess, false otherwise
     */
    public boolean SaveText (final String acFilename, int iTabSize /*= 4*/)
    {
        StringTree kRoot = new StringTree();
        kRoot.Append(StringTree.Format(acFilename));

        final int iCQuantity = GetObjectCount();
        for (int i = 0; i < iCQuantity; i++)
        {
            GraphicsObject pkObject = m_apkTopLevel.get(i);
            assert(pkObject != null);
            kRoot.Append(pkObject.SaveStrings(null));
        }

        return kRoot.Save(acFilename,iTabSize);
    }


    /** The version of the last loaded file from disk.  If no file has been
     * loaded, the returned values are -1.
     * @return The version of the last loaded file from disk.  If no file has
     * been loaded, the returned values are -1.
     */
    public StreamVersion GetVersion ()
    {
        return m_kVersion;
    }

    // linking support
    public class Link
    {
        public Link () {}

        public Link (GraphicsObject pkObject)
        {
            m_pkObject = pkObject;
            m_iCurrent = 0;
        }

        public void SetObject (GraphicsObject pkObject)
        {
            m_pkObject = pkObject;
        }

        public GraphicsObject GetObject ()
        {
            return m_pkObject;
        }

        public int GetQuantity ()
        {
            return m_kLinkID.size();
        }

        public Integer GetLinkID ()
        {
            assert(m_iCurrent < m_kLinkID.size());
            return m_kLinkID.get(m_iCurrent++);
        }

        public void Add (Integer iLinkID)
        {
            m_kLinkID.add(iLinkID);
        }

        protected GraphicsObject m_pkObject = null;
        protected int m_iCurrent = 0;
        protected Vector<Integer> m_kLinkID = new Vector<Integer>();
    };

    // base level streaming access
    protected boolean InsertInMap (Integer iID, Link pkLink)
    {
        if ( m_kMap.containsKey(iID) )
        {
            return false;
        }
        m_kMap.put(iID,pkLink);
        return true;
    }

    protected void InsertInOrdered (GraphicsObject pkObject)
    {
        m_kOrdered.add(pkObject);
    }


    /** version of last loaded file */
    protected StreamVersion m_kVersion;

    /** top level object storage */
    protected Vector<GraphicsObject> m_apkTopLevel;

    /** registration of objects on Save */
    protected Hashtable<Integer,Link> m_kMap;

    /** For saving objects in depth-first order.  If instead the objects are
     * saved based on hash-table order (in m_kMap), then the order of objects
     * for a scene graph can change between different runs of a program since
     * the memory addresses for the objects can change between runs.
     */
    protected Vector<GraphicsObject> m_kOrdered;

    /** read/write always applied to buffer in memory */
    protected int m_iBufferSize, m_iBufferNext;
    protected ByteArrayInputStream m_acBuffer;
    protected ByteArrayOutputStream m_acBufferOut;
    protected DataOutputStream m_acDataOut;
    protected DataInputStream m_acDataIn;

    protected static final String ms_acTopLevel = new String("Top Level");

    // internal use
    // linking support
    public GraphicsObject GetFromMap (Integer iID)
    {
        Link ppkLink = m_kMap.get(iID);
        return ((ppkLink != null) ? ppkLink.GetObject() : null);
    }

    // debugging support
    public int GetBufferSize ()
    {
        return m_iBufferSize;
    }

    public int GetBufferNext ()
    {
        return m_iBufferNext;
    }

    // native read functions
    public void Read (boolean[] rbValue)
    {
        try {
            rbValue[0] = m_acDataIn.readBoolean();
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += SIZEOF_BOOLEAN;
        assert(m_iBufferNext <= m_iBufferSize);
    }
    public boolean ReadBoolean ()
    {
        boolean bReturn = false;
        try {
            bReturn = m_acDataIn.readBoolean();
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += SIZEOF_BOOLEAN;
        assert(m_iBufferNext <= m_iBufferSize);
        return bReturn;
    }

    public void Read (int iQuantity, byte[] aucValue)
    {
        try {
            m_acDataIn.read( aucValue, 0, iQuantity );
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void Read (int[] riValue)
    {
        try {
            riValue[0] = m_acDataIn.readInt();
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += SIZEOF_INT;
        assert(m_iBufferNext <= m_iBufferSize);
    }
    public int ReadInt ()
    {
        int iReturn = 0;
        try {
            iReturn = m_acDataIn.readInt();
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += SIZEOF_INT;
        assert(m_iBufferNext <= m_iBufferSize);
        return iReturn;
    }

    public void Read (float[] rfValue)
    {
        try {
            rfValue[0] = m_acDataIn.readFloat();
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }
    public float ReadFloat ()
    {
        float fReturn = 0;
        try {
            fReturn = m_acDataIn.readFloat();
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
        return fReturn;
    }

    public void Read (int iQuantity, float[] afValue)
    {
        try {
            for ( int i = 0; i < iQuantity; i++ )
            {
                afValue[i] = m_acDataIn.readFloat();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += iQuantity * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Read (int iQuantity, int[] aiValue)
    {
        try {
            for ( int i = 0; i < iQuantity; i++ )
            {
                aiValue[i] = m_acDataIn.readInt();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += iQuantity * SIZEOF_INT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    // aggregrate read functions
    public String ReadString ()
    {
        int iLength = ReadInt();
        String sReturn = null;
        if (iLength > 0)
        {
            byte[] abHeader = new byte[iLength];
            try {
                m_acDataIn.read( abHeader, 0, iLength );
            } catch (IOException e) {
                e.printStackTrace();
            }
            sReturn = new String( abHeader );
            m_iBufferNext += iLength;
            assert( m_iBufferNext <= m_iBufferSize );
        }
        return sReturn;
    }

    public void Read (ColorRGB rkValue)
    {
        try {
            rkValue.R( m_acDataIn.readFloat() );
            rkValue.G( m_acDataIn.readFloat() );
            rkValue.B( m_acDataIn.readFloat() );
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += 3 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Read (ColorRGBA rkValue)
    {
        try {
            rkValue.R( m_acDataIn.readFloat() );
            rkValue.G( m_acDataIn.readFloat() );
            rkValue.B( m_acDataIn.readFloat() );
            rkValue.A( m_acDataIn.readFloat() );
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += 4 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Read (int iQuantity, ColorRGB[] rkValue)
    {
        try {
            for ( int i = 0; i < iQuantity; i++ )
            {
                rkValue[i].R( m_acDataIn.readFloat() );
                rkValue[i].G( m_acDataIn.readFloat() );
                rkValue[i].B( m_acDataIn.readFloat() );
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += iQuantity * 3 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Read (int iQuantity, ColorRGBA[] rkValue)
    {
        try {
            for ( int i = 0; i < iQuantity; i++ )
            {
                rkValue[i].R( m_acDataIn.readFloat() );
                rkValue[i].G( m_acDataIn.readFloat() );
                rkValue[i].B( m_acDataIn.readFloat() );
                rkValue[i].A( m_acDataIn.readFloat() );
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += iQuantity * 4 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Read (Matrix3f rkValue)
    {
        try {
            for ( int i = 0; i < 9; i++ )
            {
                rkValue.SetData( i, m_acDataIn.readFloat() );
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += 9 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Read (Plane3f rkValue)
    {
        Read(rkValue.Normal);
        rkValue.Constant = ReadFloat();
    }

    public void Read (Vector3f rkValue)
    {
        try {
            rkValue.X( m_acDataIn.readFloat() );
            rkValue.Y( m_acDataIn.readFloat() );
            rkValue.Z( m_acDataIn.readFloat() );
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += 3 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Read (Transformation rkValue)
    {
        Matrix3f kMatrix = new Matrix3f();
        Read( kMatrix );
        rkValue.SetMatrix(kMatrix);
        Vector3f kVector = new Vector3f();
        Read(kVector);
        rkValue.SetTranslate(kVector);
        Read(kVector);
        rkValue.SetScale(kVector);
        boolean bValue = ReadBoolean();
        rkValue.SetIsIdentity(bValue);
        bValue = ReadBoolean();
        rkValue.SetIsRSMatrix(bValue);
        bValue = ReadBoolean();
        rkValue.SetIsUniformScale(bValue);
    }

    // native write functions
    public void Write (boolean bValue)
    {
        try {
            m_acDataOut.writeBoolean(bValue);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        m_iBufferNext += SIZEOF_BOOLEAN;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (int iQuantity, byte[] aucValue)
    {
        try {
            m_acDataOut.write( aucValue, 0, iQuantity );
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += iQuantity;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (int iValue)
    {
        try {
            m_acDataOut.writeInt(iValue);
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += SIZEOF_INT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (float fValue)
    {
        try {
            m_acDataOut.writeFloat(fValue);
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (int iQuantity, final float[] afValue)
    {
        try {
            for ( int i = 0; i < iQuantity; i++ )
            {
                m_acDataOut.writeFloat(afValue[i]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += iQuantity * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (int iQuantity, final int[] aiValue)
    {
        try {
            for ( int i = 0; i < iQuantity; i++ )
            {
                m_acDataOut.writeInt(aiValue[i]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += iQuantity * SIZEOF_INT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    // aggregrate write functions
    public void Write (final String rkValue)
    {
        Write(rkValue.length());
        try {
            m_acDataOut.writeBytes(rkValue);
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += rkValue.length();
    }

    public void Write (ColorRGB rkValue)
    {
        try {
            m_acDataOut.writeFloat( rkValue.R() );
            m_acDataOut.writeFloat( rkValue.G() );
            m_acDataOut.writeFloat( rkValue.B() );
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += 3 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (ColorRGBA rkValue)
    {
        try {
            m_acDataOut.writeFloat( rkValue.R() );
            m_acDataOut.writeFloat( rkValue.G() );
            m_acDataOut.writeFloat( rkValue.B() );
            m_acDataOut.writeFloat( rkValue.A() );
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += 4 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (int iQuantity, ColorRGB[] rkValue)
    {
        try {
            for ( int i = 0; i < iQuantity; i++ )
            {
                m_acDataOut.writeFloat( rkValue[i].R() );
                m_acDataOut.writeFloat( rkValue[i].G() );
                m_acDataOut.writeFloat( rkValue[i].B() );
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += iQuantity * 3 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (int iQuantity, ColorRGBA[] rkValue)
    {
        try {
            for ( int i = 0; i < iQuantity; i++ )
            {
                m_acDataOut.writeFloat( rkValue[i].R() );
                m_acDataOut.writeFloat( rkValue[i].G() );
                m_acDataOut.writeFloat( rkValue[i].B() );
                m_acDataOut.writeFloat( rkValue[i].A() );
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += iQuantity * 4 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (final Matrix3f rkValue)
    {
        try {
            for ( int i = 0; i < 9; i++ )
            {
                m_acDataOut.writeFloat(rkValue.GetData(i));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += 9 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }

    public void Write (final Plane3f rkValue)
    {
        Write(rkValue.Normal);
        Write(rkValue.Constant);
    }

    public void Write (final Vector3f rkValue)
    {
        try {
            m_acDataOut.writeFloat( rkValue.X() );
            m_acDataOut.writeFloat( rkValue.Y() );
            m_acDataOut.writeFloat( rkValue.Z() );
        } catch (IOException e) {
            e.printStackTrace();
        }
        m_iBufferNext += 3 * SIZEOF_FLOAT;
        assert(m_iBufferNext <= m_iBufferSize);
    }
    public void Write (final Transformation rkValue)
    {
        Write(rkValue.GetMatrix());
        Write(rkValue.GetTranslate());
        Write(rkValue.GetScale());
        Write(rkValue.IsIdentity());
        Write(rkValue.IsRSMatrix());
        Write(rkValue.IsUniformScale());
    }
}
