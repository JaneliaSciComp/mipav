package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.provenance.*;

import gov.nih.mipav.view.*;

import org.apache.xerces.jaxp.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.io.*;

import java.net.*;
import java.util.Vector;

import javax.xml.parsers.*;


public class FileDataProvenance extends FileXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The tab character (makes writing out files cleaner-looking). */
    private static final String TAB = "\t";

    /** The W3C XML schema. */
    private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

    /** The charset the XML file is written in. */
    private static final String XML_ENCODING = "UTF-8";

    /** The XML version description header. */
    private static final String XML_HEADER = "<?xml version=\"1.0\" encoding=\"" + XML_ENCODING + "\"?>";

    /** The MIPAV XML header comment. */
    private static final String DATA_PROVENANCE = "<!-- MIPAV Data Provenance file -->";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** File reference. */
    private File file;

    /** File directory where the data provenance is to read or written. */
    private String fileDir;

    /** File name of the Data provenance. */
    private String fileName;

    private ModelImage image;

    /** The current level of tab nesting we are on. Used to auto-indent nested xml tags. */
    private int tabLevel = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * VOI reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     * @param      image     image model: needed during the read process to ensure the VOI "fits" in the image space.
     *
     */
    public FileDataProvenance(String fn, String fileDir, ModelImage image)  {
    	super(fn.substring(0, fn.lastIndexOf(".")) + ".xmp", fileDir); 
        this.image = image;
        
        file = new File(fileDir + fileName);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    

    /**
     * Writes VOI to an XML formatted file.
     *
     * @param   voi              VOI to be saved
     * @param   saveAllContours  if true save all contours, not just the active ones
     *
     * @throws  IOException  exception thrown if there is an error writing the file
     */
    public void writeXML() throws IOException {
       
        FileWriter fw;
        if (file.exists() == true) {
        	file.delete();
        	file = new File(fileDir + fileName);
        }

        try {

            fw = new FileWriter(file);
            bw = new BufferedWriter(fw);

            Vector<XMLAttributes> atVector = new Vector<XMLAttributes>();
            
            bw.write(XML_HEADER);
            bw.newLine();
            bw.write(DATA_PROVENANCE);
            bw.newLine();

            openTag("dataprovenance", true);
            
            int numEntries = image.getProvenanceHolder().size();
            
            ProvenanceEntry entry;
                        
            for (int i = 0; i < numEntries; i++) {
            	entry = image.getProvenanceHolder().elementAt(i);
            	
            	openTag("entry", true);
            	
            	closedTag("timestamp", Long.toString(entry.getTimeStamp()));
            	closedTag("javaversion", entry.getJavaVersion());
            	            	
            	atVector.add(new XMLAttributes("version", entry.getMipavVersion()));
            	atVector.add(new XMLAttributes("arguments", entry.getMipavArguments()));
            	
            	closedTag("mipav", "", atVector);
            	
            	atVector.add(new XMLAttributes("version", entry.getOSVersion()));
            	atVector.add(new XMLAttributes("name", entry.getOSName()));
            	closedTag("OS", "", atVector);
            	
            	closedTag("user", entry.getUser());
            	closedTag("action", entry.getAction());
            	
            	openTag("entry", false);
            }
            
            
            
            openTag("dataprovenance", false);
            bw.close();
        } catch (Exception e) {
            System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileDataProvenance");
            e.printStackTrace();
        }
    }
   
    /**
     * Reads the data provenance XML information from a file and places it in the supplied ProvenanceHolder
     * @param ph
     * @return
     */
    public boolean readXML() {

        SAXParserFactory spf = SAXParserFactory.newInstance();

        spf.setNamespaceAware(true);
        spf.setValidating(true);

        try {

            // Create a JAXP SAXParser
            SAXParser saxParser = spf.newSAXParser();

            // Validation part 2a: set the schema language if necessary
            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);

            URL xsdURL = getClass().getClassLoader().getResource("dataprovenance.xsd");

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find VOI XML schema.");

                return false;
            }

            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

            // Get the encapsulated SAX XMLReader
            XMLReader xmlReader = saxParser.getXMLReader();

            // Set the ContentHandler of the XMLReader
            xmlReader.setContentHandler(new MyXMLHandler(image.getProvenanceHolder()));

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new XMLErrorHandler());

            // Tell the XMLReader to parse the XML document
            xmlReader.parse(MipavUtil.convertToFileURL(fileDir + fileName));
        } catch (Exception error) {
        	//System.err.println(error);
            return false;
        }

        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Handle events generated while parsing the XML file.
     */
    private class MyXMLHandler extends DefaultHandler {
      
        /** The current XML tag we are parsing. */
        private String currentKey;

        /** The data for the current element being parsed. */
        private String elementBuffer = new String();

        
        private ProvenanceHolder holder;
        
        private ProvenanceEntry currentEntry;
        
        /**
         * Construct our custom XML data handler.
         *
         * @param  voi  the VOI we should build from the XML file data
         */
        public MyXMLHandler(ProvenanceHolder ph) {
           this.holder = ph;
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

            if (currentKey.equals("timestamp")) {
              currentEntry.setTimeStamp(Long.parseLong(elementBuffer));
            } else if (currentKey.equals("user")) {
            	currentEntry.setUser(elementBuffer);
            } else if (currentKey.equals("javaversion")) {
            	currentEntry.setJavaVersion(elementBuffer);
            } else if (currentKey.equals("action")) {
            	currentEntry.setAction(elementBuffer);
            	
            	//last to be read in, so add to ProvenanceHolder
            	holder.add(currentEntry);
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

            if (currentKey.equals("entry")) {
            	currentEntry = new ProvenanceEntry();
            } else if (currentKey.equals("OS")){
            	currentEntry.setOS(atts.getValue("name"), atts.getValue("version"));
            } else if (currentKey.equals("mipav")){
            	currentEntry.setMipavInfo(atts.getValue("version"), atts.getValue("arguments"));
            }
        }

    }

   

    
}
