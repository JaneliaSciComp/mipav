package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.StringTokenizer;

import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

// JAXP packages
import org.xml.sax.*;
import org.xml.sax.helpers.*;


/**
 * Inherits from FileXML, reads Surface.XML files based on the "surface.xsd"
 * file. Defines specific variables for reading and writing surface.xml files:
 */
public class FileSurfaceXML extends FileXML {

    /** array of strings representing the tags under <Surface> in the xml
     * schema. */
    private static final String[] m_kSurfaceStr = {
        "Unique-ID", "Material", "Type", "Mesh" };

    /** array of strings representing the tags under <Material> in
     * the xml schema. */
    private static final String[] m_kMaterialStr = {
        "Ambient", "Diffuse", "Emissive", "Specular",
        "Shininess" };

    /** array of strings representing the tags under <Mesh> in the xml
     * schema. */
    private static final String[] m_kMeshStr = {
        "Vertices", "Normals", "Connectivity" };

    /**
     *   Constructs new file object.
     *   @param _UI    User interface.
     *   @param fName  File name.
     *   @param fDir   File directory.
     *   @param show   Flag for showing the progress bar.
     */
    public FileSurfaceXML( ViewUserInterface _UI,
                           String fName, String fDir,
                           boolean show )
    {
        super( _UI, fName, fDir, show );
        fileInfo = new FileInfoSurfaceXML( fName, fDir, FileBase.SURFACE_XML );
    }

    /**
     * Prepares class for cleanup
     */
    public void finalize() {
        fileInfo = null;
        super.finalize();
    }

    /**
     *   Returns the FileInfoSurfaceXML read from the file.
     *   @return         File info read from file, or null if it has not been read.
     */
    public FileInfoSurfaceXML getFileInfo() {
        return (FileInfoSurfaceXML)fileInfo;
    }

    /**
     *   Writes the XML header information, including the ModelTriangleMesh
     *   surface out to the given filename and path:
     *
     *   @param headerName    file name to write to
     *   @param headerDir     name of directory to write to
     *   @param kMesh         ModelTriangleMesh representing the surface
     *   @return              if header write was successful
     *   @throws IOException if a file I/O problem is encoutered while writing
     *   the header
     */
    public boolean writeHeader( String headerName, String headerDir,
                                ModelTriangleMesh kMesh,
                                Material kMaterial )
        throws IOException {
        /* Get the mesh information to write to the file: */
        int iVertexCount = kMesh.getVertexCount();
        Point3f[] akCoordinates = new Point3f[ iVertexCount ];
        Vector3f[] akNormals = new Vector3f[ iVertexCount ];

        for ( int i = 0; i < iVertexCount; i++ )
        {
            akCoordinates[ i ] = new Point3f();
            akNormals[ i ] = new Vector3f();
        }
        kMesh.getCoordinates( 0, akCoordinates );
        kMesh.getNormals( 0, akNormals );

        int iIndexCount = kMesh.getIndexCount();
        int[] aiIndex = new int[ iIndexCount ];
        kMesh.getCoordinateIndices( 0, aiIndex );

        /* Get the Material properties, colors to write to the file: */
        Color3f kAmbient = new Color3f();        kMaterial.getAmbientColor( kAmbient );
        Color3f kDiffuse = new Color3f();        kMaterial.getDiffuseColor( kDiffuse );
        Color3f kEmissive = new Color3f();       kMaterial.getEmissiveColor( kEmissive );
        Color3f kSpecular = new Color3f();       kMaterial.getSpecularColor( kSpecular );
        float kShininess = kMaterial.getShininess();

        /* Create the SurfaceXMLHandler which processes the vertex, normal,
         * connectivity arrays and colors for writing in the xml format: */
        SurfaceXMLHandler kSurfaceXMLHandler =
            new SurfaceXMLHandler( (FileInfoSurfaceXML)fileInfo );

        /* Output file: */
        BufferedWriter bw;
        FileWriter fw;
        File headerFile;

        headerFile = new File( headerDir + headerName );
        fw = new FileWriter( headerFile );
        bw = new BufferedWriter( fw );

        bw.write( XML_HEADER );
        bw.newLine();
        bw.write( MIPAV_HEADER );
        bw.newLine();

        /* Open the surface tag: */
        openTag( bw, "Surface xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\"", true );

        /* Write the Unique-ID: */
        closedTag( bw, m_kSurfaceStr[0], new String( "22" ) );
        
        /* Open the Material tag and write the material values (ambient,
         * diffuse, emissive, specular, shininess: */
        openTag( bw, m_kSurfaceStr[1], true );
        closedTag( bw, m_kMaterialStr[0],
                  kSurfaceXMLHandler.getColorString( kAmbient ) );
        closedTag( bw, m_kMaterialStr[1],
                  kSurfaceXMLHandler.getColorString( kDiffuse ) );
        closedTag( bw, m_kMaterialStr[2],
                  kSurfaceXMLHandler.getColorString( kEmissive ) );
        closedTag( bw, m_kMaterialStr[3],
                  kSurfaceXMLHandler.getColorString( kSpecular ) );
        closedTag( bw, m_kMaterialStr[4], new String( " " + kShininess + " " ) );
        openTag( bw, m_kSurfaceStr[1], false );

        /* Write the type of Mesh (TMesh) */
        closedTag( bw, m_kSurfaceStr[2], "TMesh" );

        /* Write the TriangleMesh, Vertices, (Normals), Connectivity */
        openTag( bw, m_kSurfaceStr[3], true );
        closedTag( bw, m_kMeshStr[0],
                  kSurfaceXMLHandler.getVertexString( akCoordinates ));
        if ( akNormals != null )
        {
            closedTag( bw, m_kMeshStr[1],
                      kSurfaceXMLHandler.getNormalString( akNormals ));
        }
        closedTag( bw, m_kMeshStr[2],
                  kSurfaceXMLHandler.getIndexString( aiIndex ));

        openTag( bw, m_kSurfaceStr[3], false );

        /* Close the surface tag: */
        openTag( bw, "Surface", false );

        bw.close();

        /* Delete local variables: */
        for ( int i = 0; i < iVertexCount; i++ )
        {
            akCoordinates[ i ] = null;
            akNormals[ i ] = null;
        }
        akCoordinates = null;
        akNormals = null;
        aiIndex = null;

        kAmbient = null;
        kDiffuse = null;
        kEmissive = null;
        kSpecular = null;
        kSurfaceXMLHandler = null;

        return true;
    }



    /**
     * Reads and parses a Surface.XML header
     * @param headerFileName file name of xml header
     * @param headerDir directory
     * @return FileInfoSurfaceXML
     * @throws IOException
     */
    public FileInfoSurfaceXML readSurfaceXML( String headerFileName, String headerDir )
        throws IOException {
        SurfaceXMLHandler kHandler = null;
        // Set the ContentHandler of the XMLReader
        kHandler = new SurfaceXMLHandler( (FileInfoSurfaceXML)fileInfo );
        m_kHandler = kHandler;
        /* Pass the .xsd file to the base class for parsing: */
        if ( super.readHeader( headerFileName, headerDir, "surface.xsd" ) == null )
        {
            return null;
        }
        return (FileInfoSurfaceXML)fileInfo;
    }

    /**
     * Used by the XML Parser to parse the Surface.XML header
     */
    private class SurfaceXMLHandler extends DefaultHandler {
        /* Current XML keyword: */
        String currentKey;
        /* Current buffer: */
        String elementBuffer = new String();
        FileInfoSurfaceXML fileInfo;

        /* Vertex, Normal, and Connectivity arrays to read into from the
         * file: */
        private Point3f[] m_akVertices = null;
        private Vector3f[] m_akNormals = null;
        private int[] m_aiIndex = null;

        public SurfaceXMLHandler( FileInfoSurfaceXML fInfo )
        {
            fileInfo = fInfo;
        }

        /**
         * Parser calls this for the beginning of each element in the document.
         * @param namespaceURI   the namespace uri
         * @param localName      the element name
         * @param qName          the qualified name
         * @param atts           the attached attributes
         * @throws SAXException  if a problem is encountered during parsing
         */
        public void startElement( String namespaceURI, String localName,
                                  String qName, Attributes atts )
            throws SAXException
        {
            elementBuffer = "";
        }

        /**
         * Called by parser when the end of an element is reached in the document.
         * @param namespaceURI   the namespace uri
         * @param localName      the element name
         * @param qName          the qualified name
         * @throws SAXException  if a problem is encountered during parsing
         */
        public void endElement( String namespaceURI, String localName, String qName )
            throws SAXException
        {
            currentKey = localName;
            if ( currentKey.equals( "Unique-ID" ) )
            {
                fileInfo.setID( Integer.valueOf( elementBuffer ).intValue() );
            }
            else if ( currentKey.equals( "Ambient" ) )
            {
                fileInfo.setAmbient( getColor( elementBuffer ) );
            }
            else if ( currentKey.equals( "Diffuse" ) )
            {
                fileInfo.setDiffuse( getColor( elementBuffer ) );
            }
            else if ( currentKey.equals( "Emissive" ) )
            {
                fileInfo.setEmissive( getColor( elementBuffer ) );
            }
            else if ( currentKey.equals( "Specular" ) )
            {
                fileInfo.setSpecular( getColor( elementBuffer ) );
            }
            else if ( currentKey.equals( "Shininess" ) )
            {
                fileInfo.setShininess( Float.valueOf( elementBuffer ).floatValue() );
            }
            else if ( currentKey.equals( "Type" ) )
            {
                fileInfo.setType( elementBuffer );
            }
            else if ( currentKey.equals( "Vertices" ) )
            {
                getVertices( elementBuffer );
            }
            else if ( currentKey.equals( "Normals" ) )
            {
                getNormals( elementBuffer );
            }
            else if ( currentKey.equals( "Connectivity" ) )
            {
                getConnectivity( elementBuffer );
            }
            else if ( currentKey.equals( "Mesh" ) )
            {
                if ( (m_akVertices == null) ||
                     (m_aiIndex == null) )
                {
                    System.err.println( "Error reading in SurfaceXML file" );
                }
                if ( m_akNormals != null )
                {
                    fileInfo.setMesh( m_akVertices,
                                      m_akNormals,
                                      m_aiIndex );
                }
                else
                {
                    fileInfo.setMesh( m_akVertices,
                                      null,
                                      m_aiIndex );
                }
            }
        }

        /**
         *   Text data callback from parser.  If the parser is not
         *   validating, this method can report whitespace.  We ignore
         *   strings that are entirely whitespace.
         *   @param ch       Character array
         *   @param start    Start of data in array.
         *   @param length   Length of data in array.
         */
        public void characters( char[] ch, int start, int length )
        {
            String s = new String( ch, start, length );
            if ( s.trim().length() != 0 ) {
                elementBuffer += s;
            }
        }

        /**
         * Do nothing.
         * @throws SAXException  never happens
         */
        public void endDocument() throws SAXException {}

        /**
         * Do nothing but show the entity we skipped.
         * @param name  the skipped entity name
         */
        public void skippedEntity( String name ) {
            System.out.println( name );
        }
        
        /* Called when reading the input surface.xml file: Parses the input
         * string into a Color3f variable: */
        private Color3f getColor( String kParseString )
        {
            Color3f kColor = new Color3f();
            StringTokenizer kSt = new StringTokenizer( kParseString, " " );
            try {
                kColor.x = Float.parseFloat( kSt.nextToken() );
                kColor.y = Float.parseFloat( kSt.nextToken() );
                kColor.z = Float.parseFloat( kSt.nextToken() );
            } catch ( Exception nfex ) {
                System.out.println( nfex.toString() );
            }            
            kSt = null;
            return kColor;
        }

        /* Called when writing the surface.xml file: Converts the input
         * Color3f to a String for writing: */
        public String getColorString( Color3f kColor )
        {
            String kColorString = new String( kColor.x + " " +
                                              kColor.y + " " + kColor.z );
            return kColorString;
        }

        /* Called when reading the input surface.xml file: Parses the input
         * string into a an array of Point3f[], representing the mesh
         * vertices: */
        private void getVertices( String kParseString )
        {
            LinkedList kVertList = new LinkedList();
            Point3f kPoint = new Point3f();
            StringTokenizer kSt = new StringTokenizer( kParseString, " " );
            while ( kSt.hasMoreTokens() )
            {
                try {
                    kPoint.x = Float.parseFloat( kSt.nextToken() );
                    kPoint.y = Float.parseFloat( kSt.nextToken() );
                    kPoint.z = Float.parseFloat( kSt.nextToken() );
                    kVertList.add( new Point3f( kPoint ) );
                } catch ( Exception nfex ) {
                    System.out.println( nfex.toString() );
                }            
            }
            int iSize = kVertList.size();
            if ( iSize > 0 )
            {
                m_akVertices = new Point3f[ iSize ];
                for ( int iIndex = 0; iIndex < iSize; iIndex++ )
                {
                    m_akVertices[ iIndex ] = (Point3f)kVertList.get( iIndex );
                }
            }
            kVertList = null;
            kPoint = null;
            kSt = null;
        }

        /* Called when writing the surface.xml file: Converts the input array
         * of Point3f[] to a String for writing: */
        public String getVertexString( Point3f[] akVertices )
        {
            String kVertexString = new String();
            for ( int iVert = 0; iVert < akVertices.length; iVert++ )
            {
                kVertexString = 
                    kVertexString.concat( new String( akVertices[iVert].x + " " + 
                                                      akVertices[iVert].y + " " +
                                                      akVertices[iVert].z + " "   ) );
            }
            return kVertexString;
        }


        /* Called when reading the input surface.xml file: Parses the input
         * string into a an array of Vector3f[], representing the mesh
         * normals: */
        private void getNormals( String kParseString )
        {
            LinkedList kNormalList = new LinkedList();
            Vector3f kVector = new Vector3f();
            StringTokenizer kSt = new StringTokenizer( kParseString, " " );
            while ( kSt.hasMoreTokens() )
            {
                try {
                    kVector.x = Float.parseFloat( kSt.nextToken() );
                    kVector.y = Float.parseFloat( kSt.nextToken() );
                    kVector.z = Float.parseFloat( kSt.nextToken() );
                    kNormalList.add( new Vector3f( kVector ) );
                } catch ( Exception nfex ) {
                    System.out.println( nfex.toString() );
                }            
            }
            int iSize = kNormalList.size();
            if ( iSize > 0 )
            {
                m_akNormals = new Vector3f[ iSize ];
                for ( int iIndex = 0; iIndex < iSize; iIndex++ )
                {
                    m_akNormals[ iIndex ] = (Vector3f)kNormalList.get( iIndex );
                }
            }
            kNormalList = null;
            kVector = null;
            kSt = null;
        }

        /* Called when writing the surface.xml file: Converts the input array
         * of Vector3f[] to a String for writing: */
        public String getNormalString( Vector3f[] akNormals )
        {
            String kNormalString = new String();
            for ( int iNormal = 0; iNormal < akNormals.length; iNormal++ )
            {
                kNormalString = 
                    kNormalString.concat( new String( akNormals[iNormal].x + " " + 
                                                      akNormals[iNormal].y + " " +
                                                      akNormals[iNormal].z + " "   ) );
            }
            return kNormalString;
        }

        /* Called when reading the input surface.xml file: Parses the input
         * string into a an array of int[] to represent the mesh
         * connectivity: */
        private void getConnectivity( String kParseString )
        {
            LinkedList kIndexList = new LinkedList();
            StringTokenizer kSt = new StringTokenizer( kParseString, " " );
            while ( kSt.hasMoreTokens() )
            {
                try {
                    kIndexList.add( new Integer( Integer.parseInt( kSt.nextToken() ) ) );
                } catch ( Exception nfex ) {
                    System.out.println( nfex.toString() );
                }            
            }
            int iSize = kIndexList.size();
            if ( iSize > 0 )
            {
                m_aiIndex = new int[ iSize ];
                for ( int iIndex = 0; iIndex < iSize; iIndex++ )
                {
                    m_aiIndex[ iIndex ] = ((Integer)kIndexList.get( iIndex )).intValue();
                }
            }
            kIndexList = null;
            kSt = null;
        }

        /* Called when writing the surface.xml file: Converts the input array
         * of int[] to a String for writing: */
        public String getIndexString( int[] aiConnectivity )
        {
            String kIndexString = new String();
            for ( int i = 0; i < aiConnectivity.length; i++ )
            {
                kIndexString = 
                    kIndexString.concat( new String( aiConnectivity[i] + " " ) );
            }
            return kIndexString;
        }

    }
}
