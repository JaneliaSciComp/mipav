package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.provenance.*;

import gov.nih.mipav.view.*;

import org.apache.xerces.jaxp.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.io.*;

import java.net.*;

import javax.xml.parsers.*;


public class FileDataProvenance extends FileBase {

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
    	    	
        this.fileName = fn.substring(0, fn.lastIndexOf(".")) + ".xmp";        
        this.fileDir = fileDir;
        this.image = image;
        
        file = new File(fileDir + fileName);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * simple function to write an xml formatted closed tag including the tag value.
     *
     * @param  bw   write to use
     * @param  tag  tag name
     * @param  val  tag value
     */
    public final void closedTag(BufferedWriter bw, String tag, String val) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            // entity-ize some xml-unfriendly characters and convert to the XML
            // charset
            String writeVal = val.trim().replaceAll("&", "&amp;");

            writeVal = writeVal.trim().replaceAll("\"", "&quot;");
            writeVal = writeVal.trim().replaceAll("<", "&lt;");
            writeVal = writeVal.trim().replaceAll(">", "&gt;");
            writeVal = new String(writeVal.getBytes(XML_ENCODING));

            bw.write("<" + tag + ">" + writeVal + "</" + tag + ">");
            bw.newLine();
        } catch (IOException ex) { }
    }

    /**
     * simple function to write an xml formatted closed tag including the tag value.
     *
     * @param  bw   write to use
     * @param  tag  tag name
     * @param  attr attributes
     * @param  val  tag value
     */
    public final void closedTag(BufferedWriter bw, String tag, String attr, String val) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            // entity-ize some xml-unfriendly characters and convert to the XML
            // charset
            
            attr = new String(attr.getBytes(XML_ENCODING));
            
            String writeVal = val.trim().replaceAll("&", "&amp;");
            writeVal = writeVal.trim().replaceAll("\"", "&quot;");
            writeVal = writeVal.trim().replaceAll("<", "&lt;");
            writeVal = writeVal.trim().replaceAll(">", "&gt;");
            writeVal = new String(writeVal.getBytes(XML_ENCODING));

            bw.write("<" + tag + " " + attr + ">" + writeVal + "</" + tag + ">");
            bw.newLine();
        } catch (IOException ex) { }
    }
    
    /**
     * simple function to write an xml formatted open ended tag (value not included).
     *
     * @param  bw     writer to use
     * @param  tag    tag name
     * @param  start  whether this is the start of a container tag
     */
    public final void openTag(BufferedWriter bw, String tag, boolean start) {

        try {

            if (!start) {

                // done with this container
                tabLevel--;
            }

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            if (start) {
                bw.write("<" + tag + ">");

                // indent the contained tags
                tabLevel++;
            } else {
                bw.write("</" + tag + ">");
            }

            bw.newLine();
        } catch (IOException ex) { }
    }

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
        BufferedWriter bw;

        if (file.exists() == true) {
        	file.delete();
        	file = new File(fileDir + fileName);
        }

        try {

            fw = new FileWriter(file);
            bw = new BufferedWriter(fw);


            bw.write(XML_HEADER);
            bw.newLine();
            bw.write(DATA_PROVENANCE);
            bw.newLine();

            openTag(bw, "dataprovenance", true);
            
            int numEntries = image.getProvenanceHolder().size();
            
            ProvenanceEntry entry;
                        
            for (int i = 0; i < numEntries; i++) {
            	entry = image.getProvenanceHolder().elementAt(i);
            	
            	openTag(bw, "entry", true);
            	
            	closedTag(bw, "timestamp", Long.toString(entry.getTimeStamp()));
            	closedTag(bw, "javaversion", entry.getJavaVersion());
            	
            	closedTag(bw, "mipav", "version=\"" + entry.getMipavVersion() + "\" arguments=\"" + entry.getMipavArguments() + "\"", "");
            	closedTag(bw, "OS", "version=\"" + entry.getOSVersion() + "\" name=\"" + entry.getOSName() + "\"", "");
            	closedTag(bw, "user", entry.getUser());
            	closedTag(bw, "action", entry.getAction());
            	
            	openTag(bw, "entry", false);
            }
            
            
            
            openTag(bw, "dataprovenance", false);
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
