package gov.nih.mipav.model.file;

import WildMagic.LibFoundation.Mathematics.Vector3f;


import gov.nih.mipav.view.*;

import org.apache.xerces.jaxp.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;


import java.io.*;

import java.net.*;

import java.util.*;

import javax.xml.parsers.*;

public class FilePolylineVOIXML extends FileXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    /** The W3C XML schema. */
    private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** File directory where the VOI is to read or written. */
    private String fileDir;

    /** File name of the VOI. */
    private String fileName;
    
    float[] box = new float[3];
    int[] direction = new int[3];
    float[] origin = new float[3];
    double[][] inverseDicomArray = new double[4][4];
    boolean flip;
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    public FilePolylineVOIXML(String fileName, String fileDir) throws IOException {
    	super(fileName, fileDir);
        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * This method read a VOI file that has been saved in the MIPAV VOI format.
     *
     * @param      doLabel  boolean telling to read the files as VOITexts (labels)
     *
     * @return     the VOI read off the disk
     *
     * @exception  IOException  if there is an error reading the file
     */
    public Vector<Vector3f> readVOI(boolean doLabel) throws IOException {

    	Vector<Vector3f> coordVector = new Vector<Vector3f>();
    	
    	if (!readXML(coordVector)) {
    		throw (new IOException("Open VOI failed."));
    	}
    	
        return coordVector;

    }

    public float[] getBox() { 
    	return box;
    }
              
    public int[] getDirection() {
    	return direction;
    }
    
    public float[] getOrigin() {
       return origin;
    }
    
    public double[][] getInverseDicomArray() {
    	return inverseDicomArray;
    }
    
    public boolean getFlip() {
    	return flip;
    }
    
    private boolean readXML(Vector<Vector3f> coordVector) {

        SAXParserFactory spf = SAXParserFactory.newInstance();

        spf.setNamespaceAware(true);
        spf.setValidating(true);

        try {

            // Create a JAXP SAXParser
            SAXParser saxParser = spf.newSAXParser();

            // Validation part 2a: set the schema language if necessary
            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);

            URL xsdURL = getClass().getClassLoader().getResource("voi_polyline.xsd");

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find VOI XML schema.");

                return false;
            }

            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

            // Get the encapsulated SAX XMLReader
            XMLReader xmlReader = saxParser.getXMLReader();

            // Set the ContentHandler of the XMLReader
            xmlReader.setContentHandler(new MyXMLHandler(coordVector));

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new XMLErrorHandler());

            // Tell the XMLReader to parse the XML document
           
            xmlReader.parse(MipavUtil.convertToFileURL(fileDir + File.separatorChar + fileName));
        } catch (Exception error) {
        	error.printStackTrace();
            MipavUtil.displayError("Error: " + error.getMessage());

            return false;
        }

        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Handle events generated while parsing the XML file.
     */
    private class MyXMLHandler extends DefaultHandler {

        /** The contours of the VOI we are building. */
        private Vector<Vector3f> coordVector;

        /** The current XML tag we are parsing. */
        private String currentKey;

        /** The data for the current element being parsed. */
        private String elementBuffer = new String();
        
        /**
         * Construct our custom XML data handler.
         *
         * @param  voi  the VOI we should build from the XML file data
         */
        public MyXMLHandler(Vector<Vector3f> cVector) {
            this.coordVector = cVector;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  ch      char[]
         * @param  start   int
         * @param  length  int
         */
        public void characters(char[] ch, int start, int length) {
            String s = new String(ch, start, length);

            // don't need to de-entity-ize the string because the parser does
            // that automatically
            if (s.trim().length() != 0) {
                elementBuffer += s;
            }
        }

        /**
         * Parser calls this when the end tag of each element is reached. Data collected in the elementbuffer is
         * generally saved to the image info.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing
         * @param   qName         ? (not used)
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
          
        	currentKey = localName;
            if (currentKey.equals("Pt")) {
        	   
                float x = 0f, y = 0f, z = 0f;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");

                // try {
                    x = Float.parseFloat(st.nextToken());
                    y = Float.parseFloat(st.nextToken());
                    z = Float.parseFloat(st.nextToken());
                    coordVector.addElement(new Vector3f(x, y, z));
                /*
                } catch (NumberFormatException nfex) {
                    Preferences.debug("Error reading pt: " + nfex.toString() + "\n", Preferences.DEBUG_FILEIO);
                }
                */
            } else if (currentKey.equals("Direction")) {
            	int x = 0, y = 0, z = 0;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");
                    x = Integer.parseInt(st.nextToken());
                    y = Integer.parseInt(st.nextToken());
                    z = Integer.parseInt(st.nextToken());
                    direction[0] = x;
                    direction[1] = y;
                    direction[2] = z;
            } else if (currentKey.equals("Origin")) {
            	float x = 0, y = 0, z = 0;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");
                    x = Float.parseFloat(st.nextToken());
                    y = Float.parseFloat(st.nextToken());
                    z = Float.parseFloat(st.nextToken());
                    origin[0] = x;
                    origin[1] = y;
                    origin[2] = z;
            } else if (currentKey.equals("Box")) {
            	float x = 0, y = 0, z = 0;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");
                    x = Float.parseFloat(st.nextToken());
                    y = Float.parseFloat(st.nextToken());
                    z = Float.parseFloat(st.nextToken());
                    box[0] = x;
                    box[1] = y;
                    box[2] = z;
            } else if (currentKey.equals("Flip")) {
            	 StringTokenizer st = new StringTokenizer(elementBuffer, ",");
            	 flip = Boolean.parseBoolean(st.nextToken());
            } else if (currentKey.equals("InverseDicomArray")) {
            	StringTokenizer st = new StringTokenizer(elementBuffer, ",");
            	inverseDicomArray[0][0] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[0][1] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[0][2] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[0][3] = Double.parseDouble(st.nextToken());
            	
            	inverseDicomArray[1][0] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[1][1] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[1][2] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[1][3] = Double.parseDouble(st.nextToken());
            	
            	inverseDicomArray[2][0] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[2][1] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[2][2] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[2][3] = Double.parseDouble(st.nextToken());
            	
            	inverseDicomArray[3][0] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[3][1] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[3][2] = Double.parseDouble(st.nextToken());
            	inverseDicomArray[3][3] = Double.parseDouble(st.nextToken());
            }
        }

        /**
         * Parser calls this for each element in a document.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing
         * @param   qName         ? (not used)
         * @param   atts          attributes for the current tag
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void startElement(String namespaceURI, String localName, String qName, Attributes atts)
                throws SAXException {
            currentKey = localName;
            elementBuffer = "";

        }

    }

}
