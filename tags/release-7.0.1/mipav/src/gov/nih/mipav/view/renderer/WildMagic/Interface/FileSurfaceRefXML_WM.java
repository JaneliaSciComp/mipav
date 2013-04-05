package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.file.FileSurfaceRefXML;
import gov.nih.mipav.model.file.FileUtility;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.StringTokenizer;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibGraphics.Rendering.MaterialState;


/**
 * Inherits from FileXML, reads SurfaceRef.XML files based on the "surfaceref.xsd" file. Defines specific variables for
 * reading and writing surfaceref.xml files:
 */
public class FileSurfaceRefXML_WM extends FileSurfaceRefXML {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Used by the XML Parser to parse the Surface.XML header.
     */
    private class SurfaceRefXMLHandler extends DefaultHandler {

        /** Current XML keyword:. */
        String currentKey;

        /** Current buffer:. */
        String elementBuffer = new String();

        /** fileInfo data structure for reading the surface information into:. */
        FileInfoSurfaceRefXML_WM fileInfo;

        /**
         * Creates a new SurfaceXMLHandler object.
         *
         * @param  fInfo  FileInfo for storing the file data into.
         */
        public SurfaceRefXMLHandler(FileInfoSurfaceRefXML_WM fInfo) {
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
         * Called when writing the surface.xml file: Converts the input Color3f to a String for writing:
         *
         * @param   kColor  color object
         *
         * @return  the color in string format
         */
        public String getColorString(ColorRGB kColor) {
            String kColorString = new String(kColor.R + " " + kColor.G + " " + kColor.B);

            return kColorString;
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
         * Called when reading the input surface.xml file: Parses the input string into a Color3f variable:
         *
         * @param   kParseString  input string containing the material color
         *
         * @return  the material color in Color3f format
         */
        private ColorRGB getColor(String kParseString) {
        	ColorRGB kColor = new ColorRGB();
            StringTokenizer kSt = new StringTokenizer(kParseString, " ");

            try {
                kColor.R = Float.parseFloat(kSt.nextToken());
                kColor.G = Float.parseFloat(kSt.nextToken());
                kColor.B = Float.parseFloat(kSt.nextToken());
            } catch (Exception nfex) {
                System.out.println(nfex.toString());
            }

            kSt = null;

            return kColor;
        }


    }

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileSurfaceRefXML_WM(String fName, String fDir) {
        super(fName, fDir);
        fileInfo = new FileInfoSurfaceRefXML_WM(fName, fDir, FileUtility.SURFACEREF_XML);
    }

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
    public FileInfoSurfaceRefXML_WM getFileInfo() {
        return (FileInfoSurfaceRefXML_WM) fileInfo;
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
    public FileInfoSurfaceRefXML_WM readSurfaceXML(String headerFileName, String headerDir) 
    {
        SurfaceRefXMLHandler kHandler = null;
        
        // Set the ContentHandler of the XMLReader
        kHandler = new SurfaceRefXMLHandler((FileInfoSurfaceRefXML_WM) fileInfo);
        m_kHandler = kHandler;

        /* Pass the .xsd file to the base class for parsing: */
        if (super.readHeader(headerFileName, headerDir, "surfaceref.xsd") == null) {
            return null;
        }
        
        
        return (FileInfoSurfaceRefXML_WM) fileInfo;
    }    

    /**
     * Writes the XML file information, including the surface out to the given filename and path:
     *
     * @param   fileName   file name to write to
     * @param   kMaterial    surface material
     * @param   opacity      surface opacity
     * @param   levelDetail  surface level of detail
     * @return  if header write was successful
     *
     * @throws  IOException  if a file I/O problem is encountered while writing the header
     */
    public boolean writeXMLsurface_WM(String fileName, MaterialState kMaterial,
                                      float opacity, int levelDetail)
        throws IOException
    {

    	int dotIndex = fileName.lastIndexOf('.');
    	int slashIndex = fileName.lastIndexOf('\\');
    	String headerName = fileName.substring(slashIndex+1, dotIndex);
    	
        /* Create the SurfaceXMLHandler which processes the vertex, normal,
         * connectivity arrays and colors for writing in the xml format: */
        SurfaceRefXMLHandler kSurfaceXMLHandler = new SurfaceRefXMLHandler((FileInfoSurfaceRefXML_WM) fileInfo);

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
        closedTag(m_kMaterialStr[0], kSurfaceXMLHandler.getColorString(kMaterial.Ambient));
        closedTag(m_kMaterialStr[1], kSurfaceXMLHandler.getColorString(kMaterial.Diffuse));
        closedTag(m_kMaterialStr[2], kSurfaceXMLHandler.getColorString(kMaterial.Emissive));
        closedTag(m_kMaterialStr[3], kSurfaceXMLHandler.getColorString(kMaterial.Specular));
        closedTag(m_kMaterialStr[4], new String(" " + kMaterial.Shininess + " "));
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
}
