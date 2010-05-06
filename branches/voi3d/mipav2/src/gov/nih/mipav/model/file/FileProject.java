package gov.nih.mipav.model.file;


import gov.nih.mipav.view.*;

import org.apache.xerces.jaxp.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.io.*;

import java.net.*;

import java.util.*;

// xml parser packages
import javax.xml.parsers.*;


/**
 * The class reads XML files with MIPAV's project schema.
 *
 * @version  0.1 June 12, 2003
 * @author   Evan McCreedy
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileIO
 * @see      FileInfoProject
 */
public class FileProject extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The tab character; used to make the xml output code prettier. */
    private static final String TAB = "\t";

    /** The W3C XML schema. */
    private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

    /** The encoding we're using in the xml file. */
    private static final String XML_ENCODING = "UTF-8";

    /** The xml declaration header. */
    private static final String XML_HEADER = "<?xml version=\"1.0\" encoding=\"" + XML_ENCODING + "\"?>";

    /** The mipav project header comment. */
    private static final String MIPAV_HEADER = "<!-- MIPAV project file -->";

    /** Array of strings representing the tags under &lt;project&gt; in the xml schema. */
    private static final String[] projectStr = { "Project-attributes", "Images", "Investigators", "Sets" };

    /** Array of strings representing the tags under &lt;Project-attributes&gt; in the xml schema. */
    private static final String[] projectAttributesStr = { "Description" };

    /** Array of strings representing the tags under &lt;Images&gt; in the xml schema. */
    private static final String[] imagesStr = { "File-name", "Image-information" };

    /** Array of strings representing the tags under &lt;Image-information&gt; in the xml schema. */
    private static final String[] imageInformationStr = {
        "Information-name", "Information-description", "Information-value-type", "Information-value"
    };

    /** Array of strings representing the tags under &lt;Investigators&gt; in the xml schema. */
    private static final String[] investigatorsStr = { "Investigator-name", "Title", "Affiliation", "Email", "Phone" };

    /** Array of strings representing the tags under &lt;Sets&gt; in the xml schema. */
    private static final String[] setStr = { "Set-description", "Parameters" };

    /** Array of strings representing the tags under &lt;Parameters&gt; in the xml schema. */
    private static final String[] parameterStr = {
        "Parameter-name", "Parameter-description", "Value-type", "Value", "Parameter-date-time"
    };

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The project xml file name. */
    private String projectFileName;

    /** The project info we are going to write out to or populate from a file. */
    private FileInfoProject projectInfo;

    /** The current level of nesting. Used to position tags within tags. */
    private int tabLevel = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileProject(String fName, String fDir) {
        projectInfo = new FileInfoProject(fName, fDir, FileUtility.PROJECT);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        projectInfo = null;
        super.finalize();
    }

    /**
     * Returns the FileInfoProject read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoProject getFileInfo() {
        return projectInfo;
    }

    /**
     * simple function to write an xml formatted open ended tag (value not included).
     *
     * @param  bw     writer to use
     * @param  tag    tag name
     * @param  start  is this a start or end tag
     */
    public void openTag(BufferedWriter bw, String tag, boolean start) {

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
     * Reads the project file and stores the information in projectInfo.
     *
     * @param      fileName  File name of image.
     * @param      fileDir   Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoProject
     */
    public boolean readProject(String fileName, String fileDir) throws IOException {

        // make sure the project exists
        if (!(new File(fileDir + fileName).exists())) {
            throw new IOException("File not found: " + fileDir + fileName);
        }

        // make sure it's readable
        if (!(new File(fileDir + fileName).canRead())) {
            throw new IOException("Cannot read file: " + fileDir + fileName);
        }

        // There are several ways to parse a document using SAX and JAXP.
        // We show one approach here.  The first step is to bootstrap a
        // parser.  There are two ways: one is to use only the SAX API, the
        // other is to use the JAXP utility classes in the
        // javax.xml.parsers package.  We use the second approach here
        // because at the time of this writing it probably is the most
        // portable solution for a JAXP compatible parser.  After
        // bootstrapping a parser/XMLReader, there are several ways to
        // begin a parse.  In this example, we use the SAX API.

        projectInfo.setProjectFileName(fileName);

        // System.err.println("SET HEADER NAME");

        // Create a JAXP SAXParserFactory and configure it
        SAXParserFactory spf = SAXParserFactory.newInstance();

        // Set namespaceAware to true to get a parser that corresponds to
        // the default SAX2 namespace feature setting.  This is necessary
        // because the default value from JAXP 1.0 was defined to be false.
        spf.setNamespaceAware(true);

        // Validation part 1: set whether validation is on
        spf.setValidating(true);

        try {

            // Create a JAXP SAXParser
            SAXParser saxParser = spf.newSAXParser();

            // Validation part 2a: set the schema language if necessary
            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);

            // Validation part 2b: Set the schema source, if any.  See the JAXP
            // 1.2 maintenance update specification for more complex usages of
            // this feature.
            URL xsdURL = getClass().getClassLoader().getResource("project.xsd");

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find project XML schema.");

                return false;
            }

            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

            // Get the encapsulated SAX XMLReader
            XMLReader xmlReader = saxParser.getXMLReader();

            // Set the ContentHandler of the XMLReader
            xmlReader.setContentHandler(new MyXMLHandler());

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new XMLErrorHandler());

            // Tell the XMLReader to parse the XML document
            xmlReader.parse(MipavUtil.convertToFileURL(fileDir + fileName));
        } catch (Exception error) {
            error.printStackTrace();
            MipavUtil.displayError("Error: " + error.getMessage());

            return false;
        }

        return true;
    }

    /**
     * Writes the XML project information out to the given filename and path.
     *
     * @param   fileInfo  the project info to write
     * @param   fileName  file name to write to
     * @param   fileDir   name of directory to write to
     *
     * @return  if project write was successful
     *
     * @throws  IOException  if there is a problem writing the file to disk
     */
    public boolean writeProject(FileInfoProject fileInfo, String fileName, String fileDir) throws IOException {
        int i;
        BufferedWriter bw;
        FileWriter fw;
        File projectFile;

        projectFile = new File(fileDir + fileName);
        fw = new FileWriter(projectFile);
        bw = new BufferedWriter(fw);

        bw.write(XML_HEADER);
        bw.newLine();
        bw.write(MIPAV_HEADER);
        bw.newLine();

        openTag(bw, "project xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\" filename=\"" + fileName + "\"", true);

        openTag(bw, projectStr[0], true);

        String temp = fileInfo.getProjectDescription();

        if ((temp != null) && !temp.equalsIgnoreCase("")) {
            closedTag(bw, projectAttributesStr[0], temp);
        }

        openTag(bw, projectStr[0], false);

        /** go through the hashtable of images for this project */
        Enumeration imageEnum = fileInfo.getImageKeys();

        while (imageEnum.hasMoreElements()) {
            openTag(bw, projectStr[1], true);
            temp = (String) imageEnum.nextElement();
            closedTag(bw, imagesStr[0], temp);

            Enumeration infoEnum = fileInfo.getImage(temp).getInfoKeys();

            while (infoEnum.hasMoreElements()) {
                String infoName = (String) infoEnum.nextElement();

                openTag(bw, imagesStr[1], true);
                closedTag(bw, imageInformationStr[0], infoName);

                String temp2 = fileInfo.getImage(temp).getInfo(infoName).getDescription();

                if ((temp2 != null) && !temp2.equalsIgnoreCase("")) {
                    closedTag(bw, imageInformationStr[1], temp2);
                }

                closedTag(bw, imageInformationStr[2], fileInfo.getImage(temp).getInfo(infoName).getValueType());

                closedTag(bw, imageInformationStr[3], fileInfo.getImage(temp).getInfo(infoName).getValue());
                openTag(bw, imagesStr[1], false);
            }

            openTag(bw, projectStr[1], false);
        }

        boolean[] invest = fileInfo.getInvestigatorsComplete();

        for (i = 0; i < 3; i++) {

            if (invest[i]) {

                openTag(bw, projectStr[2], true);

                closedTag(bw, investigatorsStr[0], fileInfo.getInvestigator(i).getName());

                temp = fileInfo.getInvestigator(i).getTitle();

                if ((temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(bw, investigatorsStr[1], temp);
                }

                temp = fileInfo.getInvestigator(i).getAffiliation();

                if ((temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(bw, investigatorsStr[2], temp);
                }

                temp = fileInfo.getInvestigator(i).getEmail();

                if ((temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(bw, investigatorsStr[3], temp);
                }

                temp = fileInfo.getInvestigator(i).getPhone();

                if ((temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(bw, investigatorsStr[4], temp);
                }

                openTag(bw, projectStr[2], false);
            }
        }

        /** go through the hashtable of parameter sets */
        Enumeration setEnum = fileInfo.getPSetKeys();

        while (setEnum.hasMoreElements()) {
            openTag(bw, projectStr[3], true);
            temp = (String) setEnum.nextElement();
            closedTag(bw, setStr[0], temp);

            Enumeration paramEnum = fileInfo.getPSet(temp).getParameterKeys();

            while (paramEnum.hasMoreElements()) {
                String paramName = (String) paramEnum.nextElement();

                openTag(bw, setStr[1], true);
                closedTag(bw, parameterStr[0], paramName);

                String temp2 = fileInfo.getPSet(temp).getParameter(paramName).getDescription();

                if ((temp2 != null) && !temp2.equalsIgnoreCase("")) {
                    closedTag(bw, parameterStr[1], temp2);
                }

                closedTag(bw, parameterStr[2], fileInfo.getPSet(temp).getParameter(paramName).getValueType());

                closedTag(bw, parameterStr[3], fileInfo.getPSet(temp).getParameter(paramName).getValue());

                if ((fileInfo.getPSet(temp).getParameter(paramName).getDate() != null) &&
                        (fileInfo.getPSet(temp).getParameter(paramName).getDate() != null)) {
                    closedTag(bw, parameterStr[4],
                              fileInfo.getPSet(temp).getParameter(paramName).getDate() + "T" +
                              fileInfo.getPSet(temp).getParameter(paramName).getTime());
                }

                openTag(bw, setStr[1], false);
            }

            openTag(bw, projectStr[3], false);
        }

        openTag(bw, "project", false);
        bw.close();

        return true;
    }

    /**
     * simple function to write an xml formatted closed tag including the tag value.
     *
     * @param  bw   write to use
     * @param  tag  tag name
     * @param  val  tag value
     */
    private void closedTag(BufferedWriter bw, String tag, String val) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            // entity-ize some xml-unfriendly characters and convert to the XML charset
            String writeVal = val.trim().replaceAll("&", "&amp;");
            writeVal = writeVal.trim().replaceAll("\"", "&quot;");
            writeVal = writeVal.trim().replaceAll("<", "&lt;");
            writeVal = writeVal.trim().replaceAll(">", "&gt;");
            writeVal = new String(writeVal.getBytes(XML_ENCODING));

            bw.write("<" + tag + ">" + writeVal + "</" + tag + ">");
            bw.newLine();
        } catch (IOException ex) { }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Private class used by the parser to parse the XML file.
     */
    private class MyXMLHandler extends DefaultHandler {

        /** The current XML tag we are parsing. */
        String currentKey;

        /** The data for the current element being parsed. */
        String elementBuffer = new String();

        /** The number of image information structures attached to this project. */
        int numInfos;

        /** The number of investigators for this project. */
        int numInvestigators;

        /** The number of parameters for this project. */
        int numParameters;

        /**
         * Create a handler to fill out the project info in the parent class from the xml header data.
         */
        public MyXMLHandler() { }

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

            // don't need to de-entity-ize the string because the parser does that automatically

            if (s.trim().length() != 0) {
                elementBuffer += s;
            }
        }

        /**
         * Handle the end of the XML document. Does nothing.
         *
         * @throws  SAXException  if there is a parser error
         */
        public void endDocument() throws SAXException { }

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

            if (currentKey.equals("Description")) {
                Preferences.debug("Description: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                projectInfo.setProjectDescription(elementBuffer);
                // System.out.println("Description = " + elementBuffer + "\n", 2);
            } else if (currentKey.equals("Investigator-name")) {
                numInvestigators++;
                projectInfo.setInvestigatorName(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Title")) {
                projectInfo.setTitle(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Affiliation")) {
                projectInfo.setAffiliation(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Email")) {
                projectInfo.setEmail(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Phone")) {
                projectInfo.setPhone(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Set-description")) {

                // System.out.println("Found SET: " + elementBuffer + "\n");
                projectInfo.createPSet(elementBuffer);
            } else if (currentKey.equals("Parameter-name")) {
                numParameters++;
                projectInfo.getCurrentPSet().addParameter(elementBuffer);
            } else if (currentKey.equals("Parameter-description")) {
                projectInfo.getCurrentPSet().getCurrentParameter().setDescription(elementBuffer);
            } else if (currentKey.equals("Value-type")) {
                projectInfo.getCurrentPSet().getCurrentParameter().setValueType(elementBuffer);
            } else if (currentKey.equals("Value")) {
                projectInfo.getCurrentPSet().getCurrentParameter().setValue(elementBuffer);
            } else if (currentKey.equals("Parameter-date-time")) {
                projectInfo.getCurrentPSet().getCurrentParameter().setDateTime(elementBuffer);
            } else if (currentKey.equals("File-name")) {
                projectInfo.addImage(elementBuffer);
            } else if (currentKey.equals("Information-name")) {
                numInfos++;
                projectInfo.getCurrentImage().addInfo(elementBuffer);
            } else if (currentKey.equals("Information-description")) {
                projectInfo.getCurrentImage().getCurrentInfo().setDescription(elementBuffer);
            } else if (currentKey.equals("Information-value-type")) {
                projectInfo.getCurrentImage().getCurrentInfo().setValueType(elementBuffer);
            } else if (currentKey.equals("Information-value")) {
                projectInfo.getCurrentImage().getCurrentInfo().setValue(elementBuffer);
            }
        }

        /**
         * Handle any skipped entities by writing them out to the debug window.
         *
         * @param  name  the skipped entity
         */
        public void skippedEntity(String name) {
            Preferences.debug("ProjectXML: skipped entity: " + name + "\n", Preferences.DEBUG_FILEIO);
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

            if (currentKey.equals("project")) {

                // Note: these don't have to be in this order, should use another method
                projectFileName = atts.getValue("filename");
                projectInfo.setFileName(projectFileName);
                Preferences.debug("FileProject: filename = " + TAB + projectFileName + "\n", Preferences.DEBUG_FILEIO);
                // System.out.println("Image file name: " + projectFileName);
            } else {
                elementBuffer = "";
            }
        }
    }
}
