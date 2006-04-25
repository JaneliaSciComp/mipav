package gov.nih.mipav.model.file;


import gov.nih.mipav.view.*;

import org.apache.xerces.jaxp.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.io.*;

import java.net.*;

// JAXP packages
import javax.xml.parsers.*;


/**
 * Abstract base class for reading/writing .XML file headers. The XML reader uses any schema (.XSD) file, pased into the
 * readHeader function, for parsing.
 */
public abstract class FileXML extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** TAB string. */
    protected static final String TAB = "\t";

    /** XML schema string. */
    protected static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";
    // protected static final String JAXP_SCHEMA_SOURCE = "http://java.sun.com/xml/jaxp/properties/schemaSource";

    /** XML encoding string. */
    protected static final String XML_ENCODING = "UTF-8";

    /** XML header string. */
    protected static final String XML_HEADER = "<?xml version=\"1.0\" encoding=\"" + XML_ENCODING + "\"?>";

    /** Mipav header string (for top of xml header). */
    protected static final String MIPAV_HEADER = "<!-- MIPAV header file -->";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Directory of XML file on disk. */
    protected String fileDir;

    /**
     * the file info storing xml specific information, abstract base class of FileInfoImageXML or FileInfoSurfaceXML:
     */
    protected FileInfoXML fileInfo = null;

    /** Name of the XML header file on disk. */
    protected String fileName;

    /** DOCUMENT ME! */
    protected DefaultHandler m_kHandler = null;

    /** progress bar implementation (either through a JProgressBar or JPanel) passed in for reading/saving file. */
    protected ProgressBarInterface pInterface = null;

    /** show progress of loading/saving file. */
    protected boolean showProgress = true;

    /** tab level counter for writing xml header. */
    protected int tabLevel = 0;

    /** The user interface. */
    protected ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  _UI    User interface.
     * @param  fName  File name.
     * @param  fDir   File directory.
     * @param  show   Flag for showing the progress bar.
     */
    public FileXML(ViewUserInterface _UI, String fName, String fDir, boolean show) {
        UI = _UI;
        fileName = fName;
        fileDir = fDir;
        showProgress = show;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares class for cleanup.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Simple function to write an xml formatted open ended tag (value not included).
     *
     * @param  bw     writer to use
     * @param  tag    tag name
     * @param  start  is this a start or end tag
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
     * readHeader parses the input XML file based on the input XSD file, kFileXSD:
     *
     * @param   headerFileName  DOCUMENT ME!
     * @param   headerDir       DOCUMENT ME!
     * @param   kFileXSD        DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public FileInfoXML readHeader(String headerFileName, String headerDir, String kFileXSD) {
        // There are several ways to parse a document using SAX and JAXP. We show one approach here.  The first step is
        // to bootstrap a parser.  There are two ways: one is to use only the SAX API, the other is to use the JAXP
        // utility classes in the javax.xml.parsers package.  We use the second approach here because at the time of
        // this writing it probably is the most portable solution for a JAXP compatible parser.  After bootstrapping a
        // parser/XMLReader, there are several ways to begin a parse.  In this example, we use the SAX API.

        fileInfo.setHeaderFileName(headerFileName);

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
            URL xsdURL = getClass().getClassLoader().getResource(kFileXSD);

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find XML schema: " + kFileXSD);

                return null;
            }

            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

            // Get the encapsulated SAX XMLReader
            XMLReader xmlReader = saxParser.getXMLReader();

            xmlReader.setContentHandler(m_kHandler);

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new XMLErrorHandler());

            // Tell the XMLReader to parse the XML document
            try {
                xmlReader.parse(MipavUtil.convertToFileURL(headerDir + File.separator + headerFileName));
            } catch (SAXException kSAXError) {
                MipavUtil.displayError(kFileXSD + ": \n" +
                                       kSAXError.getMessage().substring(kSAXError.getMessage().lastIndexOf(":") + 1));

                return null;
            }
        } catch (Exception error) {
            error.printStackTrace();
            System.err.println("Got error: " + error.getMessage());
            Preferences.debug("FileXML parse error: " + error.getMessage() + "\n", Preferences.DEBUG_FILEIO);

            return null;
        }

        return fileInfo;
    }

    /**
     * Sets the progress bar to be used for loading/saving image (JProgressBar or JPanel).
     *
     * @param  pBar  ProgressBarInterface
     */
    public void setPBar(ProgressBarInterface pBar) {
        this.pInterface = pBar;
    }

    /**
     * Simple function to write an xml formatted closed tag including the tag value.
     *
     * @param  bw   write to use
     * @param  tag  tag name
     * @param  val  tag value
     */
    protected final void closedTag(BufferedWriter bw, String tag, String val) {

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

}
