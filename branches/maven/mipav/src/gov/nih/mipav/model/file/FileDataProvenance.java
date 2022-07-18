package gov.nih.mipav.model.file;


import gov.nih.mipav.model.provenance.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.io.*;

import java.util.Vector;


public class FileDataProvenance extends FileXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The tab character (makes writing out files cleaner-looking). */
    //private static final String TAB = "\t";

    /** The W3C XML schema. */
    //private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

    /** The charset the XML file is written in. */
    private static final String XML_ENCODING = "UTF-8";

    /** The XML version description header. */
    private static final String XML_HEADER = "<?xml version=\"1.0\" encoding=\"" + XML_ENCODING + "\"?>";

    /** The MIPAV XML header comment. */
    private static final String DATA_PROVENANCE = "<!-- MIPAV Data Provenance file -->";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** File reference. */
    private File file;

    /** The provenance holder to write from and read into */
    private ProvenanceHolder pHolder;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * VOI reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     * @param      image     image model: needed during the read process to ensure the VOI "fits" in the image space.
     *
     */
    public FileDataProvenance(String fn, String fDir, ProvenanceHolder ph)  {
    	super(fn, fDir); 
        this.pHolder = ph;
        
        file = new File(fileDir + File.separator + fileName);
        this.m_kHandler = new ProvenanceXMLHandler(pHolder);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------


    

    /**
     * Writes the data provenance into xml formatted file
     */
    public void writeXML() throws IOException {
        FileWriter fw;
        if (file.exists() == true) {
        	file.delete();
        	file = new File(fileDir + File.separator + fileName);
        }
        
        try {

            fw = new FileWriter(file);
            bw = new BufferedWriter(fw);

            Vector<XMLAttributes> atVector = new Vector<XMLAttributes>();
            
            bw.write(XML_HEADER);
            bw.newLine();
            bw.write(DATA_PROVENANCE);
            bw.newLine();

            openTag("provenance", true);
            
            int numEntries = pHolder.size();
            
            ProvenanceEntry entry;
                        
            for (int i = 0; i < numEntries; i++) {
            	entry = pHolder.elementAt(i);
            	
            	openTag("processStep", true);
            	
            	atVector.add(new XMLAttributes("version", entry.getMipavVersion()));
            	this.closedTag("program", entry.getProgramName(), atVector);
            	
            	atVector.add(new XMLAttributes("inputs", entry.getProgramInputs()));
            	closedTag("programArguments", entry.getAction(), atVector);
            	
            	closedTag("timeStamp", entry.getTimeStamp());            	            	
            	
            	closedTag("user", entry.getUser());
            	
            	closedTag("hostName", entry.getHostName());
            	
            	closedTag("architecture", entry.getArchitecture());
            	
            	atVector.add(new XMLAttributes("version", entry.getPlatformVersion()));
            	closedTag("platform", entry.getPlatform(), atVector);
            	
            	
            	atVector.add(new XMLAttributes("version", entry.getJavaVersion()));
            	closedTag("compiler", "java", atVector);
            	
            	openTag("processStep", false);
            }
            
            openTag("provenance", false);
            bw.close();
        } catch (Exception e) {
            System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileDataProvenance");
            e.printStackTrace();
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Handle events generated while parsing the XML file.
     */
    private class ProvenanceXMLHandler extends DefaultHandler {
      
        /** The current XML tag we are parsing. */
        private String currentKey;

        /** The data for the current element being parsed. */
        private String elementBuffer = new String();

        /** the provenance holder to read information into*/
        private ProvenanceHolder holder;
        
        /** the current provenance entry (to be inserted into the holder)*/
        private ProvenanceEntry currentEntry;
        
        /**
         * Default constructor
         * @param ph the provenance holder to read the information into
         */
        public ProvenanceXMLHandler(ProvenanceHolder ph) {
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

            if (currentKey.equals("program")) {
            	currentEntry.setProgramName(elementBuffer);
            } else if (currentKey.equals("programArguments")) {
              currentEntry.setAction(elementBuffer);
            } else if (currentKey.equals("timeStamp")) {
              currentEntry.setTimeStamp(elementBuffer);
            } else if (currentKey.equals("user")) {
            	currentEntry.setUser(elementBuffer);
            } else if (currentKey.equals("hostName")) {
            	currentEntry.setHostName(elementBuffer);
            } else if (currentKey.equals("architecture")) {
            	currentEntry.setArchitecture(elementBuffer);
            } else if (currentKey.equals("platform")) {
            	currentEntry.setPlatform(elementBuffer);
            } else if (currentKey.equals("compiler")) {
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

            if (currentKey.equals("processStep")) {
            	currentEntry = new ProvenanceEntry();
            } else if (currentKey.equals("program")){
            	currentEntry.setProgram(atts.getValue("version"), atts.getValue("build"));
            } else if (currentKey.equals("programArguments")){
            	currentEntry.setProgramArguments(atts.getValue("inputs"), atts.getValue("outputs"));
            }  else if (currentKey.equals("platform")){
            	currentEntry.setPlatformVersion(atts.getValue("version"));
            } else if (currentKey.equals("compiler")) {
            	currentEntry.setJavaVersion(atts.getValue("version"));
            }
        }

    }

   

    
}
