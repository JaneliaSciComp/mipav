package gov.nih.mipav.view.renderer.J3D.model.file;


import gov.nih.mipav.model.file.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Inherits from FileXML, reads SurfaceRef.XML files based on the "surfaceref.xsd" file. Defines specific variables for
 * reading and writing surfaceref.xml files:
 */
public class FileSurfaceRefXML_J3D extends FileSurfaceRefXML {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileSurfaceRefXML_J3D(String fName, String fDir) {
        super(fName, fDir);
        fileInfo = new FileInfoSurfaceRefXML_J3D(fName, fDir, FileUtility.SURFACEREF_XML);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares class for cleanup.
     */
    public void finalize() {
        fileInfo = null;
        super.finalize();
    }

    /**
     * Returns the FileInfoSurfaceRefXML read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoSurfaceRefXML_J3D getFileInfo() {
        return (FileInfoSurfaceRefXML_J3D) fileInfo;
    }


    /**
     * Reads and parses a SurfaceRef.XML header.
     *
     * @param   headerFileName  file name of xml header
     * @param   headerDir       directory
     *
     * @return  FileInfoSurfaceRefXML
     *
     * @throws  IOException  file exception
     */
    public FileInfoSurfaceRefXML_J3D readSurfaceXML(String headerFileName, String headerDir) throws IOException {
        SurfaceRefXMLHandler kHandler = null;
        
        // Set the ContentHandler of the XMLReader
        kHandler = new SurfaceRefXMLHandler((FileInfoSurfaceRefXML_J3D) fileInfo);
        m_kHandler = kHandler;

        /* Pass the .xsd file to the base class for parsing: */
        if (super.readHeader(headerFileName, headerDir, "surfaceref.xsd") == null) {
            return null;
        }
        
        
        return (FileInfoSurfaceRefXML_J3D) fileInfo;
    }

    /**
     * Writes the XML header information, including the ModelTriangleMesh surface out to the given filename and path:
     *
     * @param   headerName   file name to write to
     * @param   headerDir    name of directory to write to
     * @param   kMaterial    surface material
     * @param   opacity      surface opacity
     * @param   levelDetail  surface level of detail
     *
     * @return  if header write was successful
     *
     * @throws  IOException  if a file I/O problem is encoutered while writing the header
     */
    public boolean writeHeader(String headerName, String headerDir, Material kMaterial,
            float opacity, int levelDetail) throws IOException {

        /* Get the Material properties, colors to write to the file: */
        Color3f kAmbient = new Color3f();
        kMaterial.getAmbientColor(kAmbient);

        Color3f kDiffuse = new Color3f();
        kMaterial.getDiffuseColor(kDiffuse);

        Color3f kEmissive = new Color3f();
        kMaterial.getEmissiveColor(kEmissive);

        Color3f kSpecular = new Color3f();
        kMaterial.getSpecularColor(kSpecular);

        float kShininess = kMaterial.getShininess();
        
        /* Create the SurfaceRefXMLHandler which processes the vertex, normal,
         * connectivity arrays and colors for writing in the xml format: */
        SurfaceRefXMLHandler kSurfaceXMLHandler = new SurfaceRefXMLHandler((FileInfoSurfaceRefXML_J3D) fileInfo);

        /* Output file: */
        FileWriter fw;
        File headerFile;

        headerFile = new File(headerDir + headerName);
        fw = new FileWriter(headerFile);
        bw = new BufferedWriter(fw);

        bw.write(XML_HEADER);
        bw.newLine();
        bw.write(MIPAV_HEADER);
        bw.newLine();

        /* Open the surface tag: */
        openTag("Surface xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\"", true);

        /***************************************************************************************/ 
          
                /* Write the Unique-ID: */
                closedTag(m_kSurfaceStr[0], new String("22"));

                /* Open the Material tag and write the material values (ambient,
                 * diffuse, emissive, specular, shininess: */
                openTag(m_kSurfaceStr[1], true);
                closedTag(m_kMaterialStr[0], kSurfaceXMLHandler.getColorString(kAmbient));
                closedTag(m_kMaterialStr[1], kSurfaceXMLHandler.getColorString(kDiffuse));
                closedTag(m_kMaterialStr[2], kSurfaceXMLHandler.getColorString(kEmissive));
                closedTag(m_kMaterialStr[3], kSurfaceXMLHandler.getColorString(kSpecular));
                closedTag(m_kMaterialStr[4], new String(" " + kShininess + " "));
                openTag(m_kSurfaceStr[1], false);

                /* Write the type of Mesh (TMesh) */
                closedTag(m_kSurfaceStr[2], "TMesh");

                /* Write the surface opacity */
                closedTag(m_kSurfaceStr[3], new String(" " + opacity + " "));

                /* Write the surface level of detial */
                closedTag(m_kSurfaceStr[4], new String(" " + levelDetail + " "));

                /* Write the .sur surface file name: */
                closedTag(m_kSurfaceStr[5], headerName + ".sur");

        /********************************************************************************/
        /* Close the surface tag: */
        openTag("Surface", false);

        bw.close();

        kSurfaceXMLHandler = null;

        return true;
    }
    
    /**
     * Writes the XML file information, including the ModelTriangleMesh surface out to the given filename and path:
     *
     * @param   fileName   file name to write to
     * @param   kMaterial    surface material
     * @param   opacity      surface opacity
     * @param   levelDetail  surface level of detail
     * @return  if header write was successful
     *
     * @throws  IOException  if a file I/O problem is encoutered while writing the header
     */
    public boolean writeXMLsurface(String fileName, Material kMaterial,
            float opacity, int levelDetail) throws IOException {

    	int dotIndex = fileName.lastIndexOf('.');
    	int slashIndex = fileName.lastIndexOf('\\');
    	int nDims = 3;
    	String headerName, headerDir;
    	headerDir = fileName.substring(0, slashIndex);
    	headerName = fileName.substring(slashIndex+1, dotIndex);
    	
    	/* Get the Material properties, colors to write to the file: */
        Color3f kAmbient = new Color3f();
        kMaterial.getAmbientColor(kAmbient);

        Color3f kDiffuse = new Color3f();
        kMaterial.getDiffuseColor(kDiffuse);

        Color3f kEmissive = new Color3f();
        kMaterial.getEmissiveColor(kEmissive);

        Color3f kSpecular = new Color3f();
        kMaterial.getSpecularColor(kSpecular);

        float kShininess = kMaterial.getShininess();
    	
        /* Create the SurfaceXMLHandler which processes the vertex, normal,
         * connectivity arrays and colors for writing in the xml format: */
        SurfaceRefXMLHandler kSurfaceXMLHandler = new SurfaceRefXMLHandler((FileInfoSurfaceRefXML_J3D) fileInfo);

        /* Output file: */
        FileWriter fw;
        File headerFile;

        headerFile = new File(fileName);
        fw = new FileWriter(headerFile);
        bw = new BufferedWriter(fw);

        bw.write(XML_HEADER);
        bw.newLine();
        bw.write(MIPAV_HEADER);
        bw.newLine();

        /* Open the surface tag: */
        openTag("Surface xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\"", true);

        /***************************************************************************************/
        /* Write the Unique-ID: */
        closedTag(m_kSurfaceStr[0], new String("22"));

        /* Open the Material tag and write the material values (ambient,
         * diffuse, emissive, specular, shininess: */
        openTag(m_kSurfaceStr[1], true);
        closedTag(m_kMaterialStr[0], kSurfaceXMLHandler.getColorString(kAmbient));
        closedTag(m_kMaterialStr[1], kSurfaceXMLHandler.getColorString(kDiffuse));
        closedTag(m_kMaterialStr[2], kSurfaceXMLHandler.getColorString(kEmissive));
        closedTag(m_kMaterialStr[3], kSurfaceXMLHandler.getColorString(kSpecular));
        closedTag(m_kMaterialStr[4], new String(" " + kShininess + " "));
        openTag(m_kSurfaceStr[1], false);
        
        /* Write the type of Mesh (TMesh) */
        closedTag(m_kSurfaceStr[2], "TMesh");

        /* Write the surface opacity */
        closedTag(m_kSurfaceStr[3], new String(" " + opacity + " "));

        /* Write the surface level of detial */
        closedTag(m_kSurfaceStr[4], new String(" " + levelDetail + " "));
        
        /* Write the .sur surface file name */ 
        closedTag(m_kSurfaceStr[5], headerName + ".sur");
            
        /********************************************************************************/
        /* Close the surface tag: */
        openTag("Surface", false);

        bw.close();

        kSurfaceXMLHandler = null;

        return true;
    }    


    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Used by the XML Parser to parse the Surface.XML header.
     */
    private class SurfaceRefXMLHandler extends DefaultHandler {

        /** Current XML keyword:. */
        String currentKey;

        /** Current buffer:. */
        String elementBuffer = new String();

        /** fileInfo data structure for reading the surface information into:. */
        FileInfoSurfaceRefXML_J3D fileInfo;

        /**
         * Creates a new SurfaceXMLHandler object.
         *
         * @param  fInfo  FileInfo for storing the file data into.
         */
        public SurfaceRefXMLHandler(FileInfoSurfaceRefXML_J3D fInfo) {
            fileInfo = fInfo;
        }

        /**
         * Text data callback from parser. If the parser is not validating, this method can report whitespace. We ignore
         * strings that are entirely whitespace.
         *
         * @param  ch      Character array
         * @param  start   Start of data in array.
         * @param  length  Length of data in array.
         */
        public void characters(char[] ch, int start, int length) {
            String s = new String(ch, start, length);

            if (s.trim().length() != 0) {
                elementBuffer += s;
            }
        }

        /**
         * Do nothing.
         *
         * @throws  SAXException  never happens
         */
        public void endDocument() throws SAXException { }

        /**
         * Called by parser when the end of an element is reached in the document.
         *
         * @param   namespaceURI  the namespace uri
         * @param   localName     the element name
         * @param   qName         the qualified name
         *
         * @throws  SAXException  if a problem is encountered during parsing
         */
        public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
            currentKey = localName;
            if (currentKey.equals("Unique-ID")) {
                fileInfo.setID(Integer.valueOf(elementBuffer).intValue());
            } else if (currentKey.equals("Ambient")) {
                fileInfo.setAmbient(getColor(elementBuffer));
            } else if (currentKey.equals("Diffuse")) {
                fileInfo.setDiffuse(getColor(elementBuffer));
            } else if (currentKey.equals("Emissive")) {
                fileInfo.setEmissive(getColor(elementBuffer));
            } else if (currentKey.equals("Specular")) {
                fileInfo.setSpecular(getColor(elementBuffer));
            } else if (currentKey.equals("Shininess")) {
                fileInfo.setShininess(Float.valueOf(elementBuffer).floatValue());
            } else if (currentKey.equals("Type")) {
                fileInfo.setType(elementBuffer);
            } else if (currentKey.equals("Opacity")) {
                fileInfo.setOpacity(Float.valueOf(elementBuffer).floatValue());
            } else if (currentKey.equals("LevelDetail")) {
                fileInfo.setLevelDetail(Integer.valueOf(elementBuffer).intValue());
            } else if (currentKey.equals("Filename")) {
                fileInfo.setSurfaceFileName(elementBuffer);
            } 
        }

               /**
         * Do nothing but show the entity we skipped.
         *
         * @param  name  the skipped entity name
         */
        public void skippedEntity(String name) {
            System.out.println(name);
        }

        /**
         * Parser calls this for the beginning of each element in the document.
         *
         * @param   namespaceURI  the namespace uri
         * @param   localName     the element name
         * @param   qName         the qualified name
         * @param   atts          the attached attributes
         *
         * @throws  SAXException  if a problem is encountered during parsing
         */
        public void startElement(String namespaceURI, String localName, String qName, org.xml.sax.Attributes atts)
                throws SAXException {
            elementBuffer = "";
        }

        /**
         * Called when writing the surface.xml file: Converts the input Color3f to a String for writing:
         *
         * @param   kColor  color object
         *
         * @return  the color in string format
         */
        public String getColorString(Color3f kColor) {
            String kColorString = new String(kColor.x + " " + kColor.y + " " + kColor.z);

            return kColorString;
        }

        /**
         * Called when reading the input surface.xml file: Parses the input string into a Color3f variable:
         *
         * @param   kParseString  input string containing the material color
         *
         * @return  the material color in Color3f format
         */
        private Color3f getColor(String kParseString) {
            Color3f kColor = new Color3f();
            StringTokenizer kSt = new StringTokenizer(kParseString, " ");

            try {
                kColor.x = Float.parseFloat(kSt.nextToken());
                kColor.y = Float.parseFloat(kSt.nextToken());
                kColor.z = Float.parseFloat(kSt.nextToken());
            } catch (Exception nfex) {
                System.out.println(nfex.toString());
            }

            kSt = null;

            return kColor;
        }


    }
}
