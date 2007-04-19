package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.xcede.*;

import gov.nih.mipav.view.*;

import org.apache.xerces.jaxp.*;

import org.w3c.dom.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.io.*;

import java.net.*;

import java.text.*;

import javax.xml.parsers.*;


/**
 * DOCUMENT ME!
 */
public class FileXCEDEXML extends FileXML {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Document document;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new FileXCEDEXML object.
     *
     * @param  fName  DOCUMENT ME!
     * @param  fDir   DOCUMENT ME!
     */
    public FileXCEDEXML(String fName, String fDir) {
        super(fName, fDir);
        m_kHandler = new XCEDEHandler();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public XCEDEElement parse() {
        return read(fileName, fileDir, "xcede.xsd");
    }

    /**
     * DOCUMENT ME!
     *
     * @param   fileName  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Document read(String fileName) {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        try {
            DocumentBuilder builder = factory.newDocumentBuilder();
            document = builder.parse(new File(fileName));

            return document;
        } catch (SAXParseException e) { }
        catch (SAXException e) { }
        catch (ParserConfigurationException e) { }
        catch (IOException e) { }

        return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   fileName     DOCUMENT ME!
     * @param   directory    DOCUMENT ME!
     * @param   xsdFileName  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public XCEDEElement read(String fileName, String directory, String xsdFileName) {

        // Create a JAXP SAXParserFactory and configure it
        SAXParserFactory spf = SAXParserFactory.newInstance();

        // Set namespaceAware to true to get a parser that corresponds to
        // the default SAX2 namespace feature setting.  This is necessary
        // because the default value from JAXP 1.0 was defined to be false.
        spf.setNamespaceAware(true);

        // Validation part 1: set whether validation is on
        spf.setValidating(false);


        try {

            // Create a JAXP SAXParser
            SAXParser saxParser = spf.newSAXParser();

            // Validation part 2a: set the schema language if necessary
            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);

            // Validation part 2b: Set the schema source, if any.  See the JAXP
            // 1.2 maintenance update specification for more complex usages of
            // this feature.
            URL xsdURL = getClass().getClassLoader().getResource(xsdFileName);

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find XML schema: " + xsdFileName);

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
                xmlReader.parse(MipavUtil.convertToFileURL(directory + File.separator + fileName));

                return ((XCEDEHandler) m_kHandler).getRootElement();
            } catch (SAXException kSAXError) {
                MipavUtil.displayError(xsdFileName + ": \n" +
                                       kSAXError.getMessage().substring(kSAXError.getMessage().lastIndexOf(":") + 1));

                return null;
            }
        } catch (Exception error) {
            error.printStackTrace();
            System.err.println("Got error: " + error.getMessage());
            Preferences.debug("FileXML parse error: " + error.getMessage() + "\n", Preferences.DEBUG_FILEIO);

            return null;
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Used to parse the xml XCEDE file.
     *
     * @author   Hailong Wang, Ph.D
     * @version  1.0, 04/25/06
     */
    public class XCEDEHandler extends DefaultHandler {

        /** Used to hold current element. */
        private XCEDEElement currentElement;

        /** Used to capture the current key. */
        private String currentKey;

        /** DOCUMENT ME! */
        private String elementBuffer = "";

        /** Used to hold parent element of the current element. */
        private XCEDEElement parentElement;

        /** This is the root element for the XCEDE file. */
        private XCEDEElement rootElement;

        /**
         * Creates a new XCEDEHandler object.
         */
        public XCEDEHandler() { }

        /**
         * Text data callback from parser. If the parser is not validating, this method can report whitespace. We ignore
         * strings that are entirely whitespace.
         *
         * @param   ch      Character array
         * @param   start   Start of data in array.
         * @param   length  Length of data in array.
         *
         * @throws  SAXException  DOCUMENT ME!
         */
        public void characters(char[] ch, int start, int length) throws SAXException {
            String s = new String(ch, start, length);

            if (s.trim().length() > 0) {
                elementBuffer += s;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param   uri        DOCUMENT ME!
         * @param   localName  DOCUMENT ME!
         * @param   qName      DOCUMENT ME!
         *
         * @throws  SAXException  DOCUMENT ME!
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            currentKey = localName;

            if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROJECT)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_PROJECT, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SUBJECT)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_SUBJECT, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VISIT)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_VISIT, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_STUDY)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_STUDY, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SERIES)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_SERIES, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ID)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_ID, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DESCRIPTION)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_DESCRIPTION, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FUNDING)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_FUNDING, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_BIRTHDATE)) {
                DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");

                try {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_BIRTHDATE, formatter.parse(elementBuffer));
                } catch (ParseException e) {
                    e.printStackTrace(System.err);
                    MipavUtil.displayError("Birth date parsing exception: " + elementBuffer);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DEATHDATE)) {
                DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");

                try {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_DEATHDATE, formatter.parse(elementBuffer));
                } catch (ParseException e) {
                    e.printStackTrace(System.err);
                    MipavUtil.displayError("Death date parsing exception: " + elementBuffer);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_NAME)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_NAME, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SEX)) {

                if (elementBuffer.equals(XCEDEElement.XCEDE_SEX_FEMALE) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_SEX_MALE) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_SEX_OTHER)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_SEX, elementBuffer);
                } else {
                    MipavUtil.displayError("The value of sex element must be " + XCEDEElement.XCEDE_SEX_FEMALE + ", " +
                                           XCEDEElement.XCEDE_SEX_MALE + " and " + XCEDEElement.XCEDE_SEX_OTHER);

                    return;
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SPECIES)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_SPECIES, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EXTENDEDDESCRIPTOR)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_EXTENDEDDESCRIPTOR, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_LOCATION)) {
                String level = currentElement.getLevel();

                if (level.equals(XCEDEElement.XCEDE_ELEMENT_VISIT)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_LOCATION, elementBuffer);
                } else {
                    parentElement.put(XCEDEElement.XCEDE_ELEMENT_LOCATION, currentElement);
                    currentElement = parentElement;
                    parentElement = (XCEDEElement) currentElement.getParentElement();
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SUBJECTVAR)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_SUBJECTVAR, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SCANNER)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_SCANNER, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EXPPROTOCOL)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_EXPPROTOCOL, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DATAREC)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_DATAREC, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_AGE)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_AGE_VALUE, Float.valueOf(elementBuffer));
                }

                parentElement.put(XCEDEElement.XCEDE_ELEMENT_AGE, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ASSESSMENT)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_ASSESSMENT, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ANNOTATION)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_ANNOTATION, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_MANUFACTURER)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_MANUFACTURER, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_MODEL)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_MODEL, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ADDITIONALEQUIPMENT)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_ADDITIONALEQUIPMENT, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FILENAME)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_FILENAME, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FILEOFFSET)) {

                if (elementBuffer != null) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_FILEOFFSET, Long.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FILERECORDSIZE)) {

                if (elementBuffer != null) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_FILERECORDSIZE, Long.valueOf(elementBuffer));
                }

                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_DATARECFRAG, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROVENANCEID)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_PROVENANCEID, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_RASORIGIN)) {
                DataPoints dataPoints = DataPoints.Float.parse(elementBuffer, "[\\s]+");

                if (dataPoints != null) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_RASORIGIN, dataPoints);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DIMENSION)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_DIMENSION, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_BYTEORDER)) {

                if (elementBuffer.equals(XCEDEElement.XCEDE_BYTEORDER_LSBFIRST) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_BYTEORDER_LSBFIRST)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_BYTEORDER, elementBuffer);
                } else {
                    MipavUtil.displayError("The value for the byte order must be msbfirst or lsbfirst: " +
                                           elementBuffer);

                    return;
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ELEMENTTYPE)) {

                if (elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_INT8) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_UINT8) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_INT16) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_UINT16) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_INT32) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_UINT32) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_INT64) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_UINT64) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_FLOAT32) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_FLOAT64) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_ELEMENTTYPE_ASCII)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_ELEMENTTYPE, elementBuffer);
                } else {
                    MipavUtil.displayError("The value for the byte order is not valid: " + elementBuffer);

                    return;
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SIZE)) {

                if (elementBuffer != null) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_SIZE, Integer.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ORIGIN)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_ORIGIN, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SPACING)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_SPACING, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_GAP)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_GAP, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DATAPOINTS)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_DATAPOINTS,
                                   DataPoints.Float.parse(elementBuffer, "[\\s]+"));
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DIRECTION)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_DIRECTION, Point3d.parse(elementBuffer, "[\\s]+"));
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_UNITS)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_UNITS, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROVENANCE)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_PROVENANCE, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_STATISTIC)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_STATISTIC, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROCESSSTEP)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_PROCESSSTEP, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROGRAMNAME)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_PROGRAMNAME, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROGRAMARGUMENT)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_PROGRAMARGUMENT, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VERSION)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_VERSION, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_TIMESTAMP)) {
                DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssz");

                try {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_TIMESTAMP, formatter.parse(elementBuffer));
                } catch (ParseException e) {
                    formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");

                    try {
                        currentElement.put(XCEDEElement.XCEDE_ELEMENT_TIMESTAMP, formatter.parse(elementBuffer));
                    } catch (ParseException e2) {
                        e2.printStackTrace(System.err);
                        MipavUtil.displayError("Death date parsing exception: " + elementBuffer);
                    }
                }

            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_CVS)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_CVS, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_USER)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_USER, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_MACHINE)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_MACHINE, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PLATFORM)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_PLATFORM, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PLATFORMVERSION)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_PLATFORMVERSION, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_COMPILERNAME)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_COMPILERNAME, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_COMPILERVERSION)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_COMPILERVERSION, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_LIBNAME)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_LIBNAME, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_LIBVERSION)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_LIBVERSION, elementBuffer);
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_LIBRARIES, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SOURCEDATA)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_SOURCEDATA, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROCESS)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_PROCESS, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VALUE)) {
                String level = currentElement.getLevel();

                if (level.equals(XCEDEElement.XCEDE_ELEMENT_LABEL)) {

                    if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                        currentElement.put(XCEDEElement.XCEDE_ELEMENT_VALUE, elementBuffer);
                    }
                } else if (level.equals(XCEDEElement.XCEDE_ELEMENT_THRESH)) {

                    if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                        currentElement.put(XCEDEElement.XCEDE_ELEMENT_VALUE, Float.valueOf(elementBuffer));
                    }
                } else {

                    parentElement.put(XCEDEElement.XCEDE_ELEMENT_VALUE, currentElement);
                    currentElement = parentElement;
                    parentElement = (XCEDEElement) currentElement.getParentElement();
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PUNCORRECTED)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_PUNCORRECTED, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PCORRECTED)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_PCORRECTED_VALUE, Float.valueOf(elementBuffer));
                }

                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_PCORRECTED, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ATTR_LOCATIONFOCI)) {
                currentElement.put(XCEDEElement.XCEDE_ATTR_LOCATIONFOCI, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_NUMCLUSTERS)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_NUMCLUSTERS, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_P)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_P, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EXTENTS)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_EXTENTS, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_LABEL)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_LABEL, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_X)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_X, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_Y)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_Y, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_Z)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_Z, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_THRESH)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_THRESH, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_BRODMAN)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_BRODMAN, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_LATERIZATION)) {

                if (elementBuffer.equals(XCEDEElement.XCEDE_LATERALIZATION_LEFT) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_LATERALIZATION_RIGHT) ||
                        elementBuffer.equals(XCEDEElement.XCEDE_LATERALIZATION_BILATERAL)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_LATERIZATION, elementBuffer);
                    MipavUtil.displayError("The value of the lateralization must to be " +
                                           XCEDEElement.XCEDE_LATERALIZATION_LEFT + ", " +
                                           XCEDEElement.XCEDE_LATERALIZATION_RIGHT + " and " +
                                           XCEDEElement.XCEDE_LATERALIZATION_BILATERAL + elementBuffer);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ATTR_ATLAS)) {
                currentElement.put(XCEDEElement.XCEDE_ATTR_ATLAS, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FWHM)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_FWHM, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ATTR_FWHMUNITS)) {
                currentElement.put(XCEDEElement.XCEDE_ATTR_FWHMUNITS, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_S)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_S, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SEARCHUNITS)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_SEARCHUNITS, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VOX)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.addChild(XCEDEElement.XCEDE_ELEMENT_VOX, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EXPVOXPERCLUSTER)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_EXPVOXPERCLUSTER, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EXPNUMCLUSTERS)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_EXPNUMCLUSTERS, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DF)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.addChild(XCEDEElement.XCEDE_ELEMENT_DF, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FWHMSMOOTHNESS)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_FWHMSMOOTHNESS, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VOXELSIZE)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_VOXELSIZE, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ACTIVATIONPARAMS)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_ACTIVATIONPARAMS, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_CLUSTERS)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_CLUSTERS, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VOXEL)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_VOXEL, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ITEM)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_ITEM, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_TAXONOMICCLASS)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_TAXONOMICCLASS, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ACQPARAM)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_ACQPARAM, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EVENTS)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_EVENTS, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EVENTPARAMS)) {
                parentElement.put(XCEDEElement.XCEDE_ELEMENT_EVENTPARAMS, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EVENT)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_EVENT, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FIRSTMRITIME)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_FIRSTMRITIME, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ONSET)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_ONSET, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_RT)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_RT, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DURATION)) {

                if ((elementBuffer != null) && (elementBuffer.length() > 0)) {
                    currentElement.put(XCEDEElement.XCEDE_ELEMENT_DURATION, Float.valueOf(elementBuffer));
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_TEXT)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_TEXT, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ANNOTATOR)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_ANNOTATOR, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ASSESSMENTVALUE)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_ASSESSMENTVALUE, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SUMMARYNAME)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_SUMMARYNAME, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SUMMARYVALUE)) {
                currentElement.addChild(XCEDEElement.XCEDE_ELEMENT_SUMMARYVALUE, elementBuffer);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_NORMALIZEDVALUE)) {
                parentElement.addChild(XCEDEElement.XCEDE_ELEMENT_NORMALIZEDVALUE, currentElement);
                currentElement = parentElement;
                parentElement = (XCEDEElement) currentElement.getParentElement();
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SCHEME)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_SCHEME, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FULLPATH)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_FULLPATH, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ACTUAL_VALUE)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_ACTUAL_VALUE, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DATACLASSIFICATION)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_DATACLASSIFICATION, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_COMMONNAME)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_COMMONNAME, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_LATINNAME)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_LATINNAME, elementBuffer);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_STRAIN)) {
                currentElement.put(XCEDEElement.XCEDE_ELEMENT_STRAIN, elementBuffer);
            }

            elementBuffer = "";
        }

        /**
         * Returns the root element of the XCEDE schema.
         *
         * @return  the root element of the XCEDE schema.
         */
        public XCEDEElement getRootElement() {
            return (XCEDEElement) rootElement;
        }

        /**
         * Parser calls this function for the beginning of each element in this document.
         *
         * @param   uri        DOCUMENT ME!
         * @param   localName  DOCUMENT ME!
         * @param   qName      DOCUMENT ME!
         * @param   atts       DOCUMENT ME!
         *
         * @throws  SAXException  DOCUMENT ME!
         */
        public void startElement(String uri, String localName, String qName, Attributes atts) throws SAXException {
            currentKey = localName;

            if (currentKey.equals(XCEDEElement.XCEDE_ROOT_ELEMENT_PROJECTLEVEL)) {
                rootElement = new XCEDEElement(XCEDEElement.XCEDE_ROOT_ELEMENT_PROJECTLEVEL);
                currentElement = rootElement;
                parentElement = null;
            } else if (currentKey.equals(XCEDEElement.XCEDE_ROOT_ELEMENT_SUBJECTLEVEL)) {
                rootElement = new XCEDEElement(XCEDEElement.XCEDE_ROOT_ELEMENT_SUBJECTLEVEL);
                currentElement = rootElement;
                parentElement = null;
            } else if (currentKey.equals(XCEDEElement.XCEDE_ROOT_ELEMENT_VISITLEVEL)) {
                rootElement = new XCEDEElement(XCEDEElement.XCEDE_ROOT_ELEMENT_VISITLEVEL);
                currentElement = rootElement;
                parentElement = null;
            } else if (currentKey.equals(XCEDEElement.XCEDE_ROOT_ELEMENT_STUDYLEVEL)) {
                rootElement = new XCEDEElement(XCEDEElement.XCEDE_ROOT_ELEMENT_STUDYLEVEL);
                currentElement = rootElement;
                parentElement = null;
            } else if (currentKey.equals(XCEDEElement.XCEDE_ROOT_ELEMENT_SERIESLEVEL)) {
                rootElement = new XCEDEElement(XCEDEElement.XCEDE_ROOT_ELEMENT_SERIESLEVEL);
                currentElement = rootElement;
                parentElement = null;
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROJECT)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_PROJECT, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SUBJECT)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_SUBJECT, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VISIT)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_VISIT, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_STUDY)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_STUDY, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SERIES)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_SERIES, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SPECIES)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_SPECIES, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EXTENDEDDESCRIPTOR)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_EXTENDEDDESCRIPTOR, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_LOCATION)) {
                String level = currentElement.getLevel();

                if (level.equals(XCEDEElement.XCEDE_ELEMENT_VOXEL)) {
                    parentElement = currentElement;
                    currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_LOCATION, parentElement);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SUBJECTVAR)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_SUBJECTVAR, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SCANNER)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_SCANNER, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EXPPROTOCOL)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_EXPPROTOCOL, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DATAREC)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_DATAREC, parentElement);

                String type = atts.getValue(XCEDEElement.XCEDE_ATTR_TYPE);

                if (type != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_TYPE, type);
                }

                String subtype = atts.getValue(XCEDEElement.XCEDE_ATTR_SUBTYPE);

                if (subtype != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_SUBTYPE, subtype);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_AGE)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_AGE, parentElement);

                String ageType = atts.getValue(XCEDEElement.XCEDE_ATTR_AGETYPE);

                if (ageType != null) {

                    if (ageType.equals(XCEDEElement.XCEDE_AGE_TYPE_GESTATIONAL) ||
                            ageType.equals(XCEDEElement.XCEDE_AGE_TYPE_PORTMORTEM) ||
                            ageType.equals(XCEDEElement.XCEDE_AGE_TYPE_POSTNATAL)) {
                        currentElement.put(XCEDEElement.XCEDE_ATTR_AGETYPE, ageType);
                    } else {
                        MipavUtil.displayError("The age type must be " + XCEDEElement.XCEDE_AGE_TYPE_GESTATIONAL +
                                               ", " + XCEDEElement.XCEDE_AGE_TYPE_PORTMORTEM + " or " +
                                               XCEDEElement.XCEDE_AGE_TYPE_POSTNATAL);

                        return;
                    }
                }

                String ageUnits = atts.getValue(XCEDEElement.XCEDE_ATTR_UNITS);

                if (ageUnits != null) {

                    if (ageUnits.equals(XCEDEElement.XCEDE_AGE_UNITS_DAYS) ||
                            ageUnits.equals(XCEDEElement.XCEDE_AGE_UNITS_HOURS) ||
                            ageUnits.equals(XCEDEElement.XCEDE_AGE_UNITS_WEEKS) ||
                            ageUnits.equals(XCEDEElement.XCEDE_AGE_UNITS_YEARS)) {
                        currentElement.put(XCEDEElement.XCEDE_ATTR_UNITS, ageUnits);
                    } else {
                        MipavUtil.displayError("The age units must be " + XCEDEElement.XCEDE_AGE_UNITS_DAYS + ", " +
                                               XCEDEElement.XCEDE_AGE_UNITS_HOURS + ", " +
                                               XCEDEElement.XCEDE_AGE_UNITS_WEEKS + ", " +
                                               XCEDEElement.XCEDE_AGE_UNITS_YEARS);

                        return;
                    }
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ASSESSMENT)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_ASSESSMENT, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ANNOTATION)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_ANNOTATION, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_DIMENSION)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_DIMENSION, parentElement);

                String type = atts.getValue(XCEDEElement.XCEDE_ATTR_TYPE);

                if (type != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_TYPE, type);
                }

                String outputSelect = atts.getValue(XCEDEElement.XCEDE_ATTR_OUTPUTSELECT);

                if (outputSelect != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_OUTPUTSELECT, outputSelect);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FILENAME)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_DATARECFRAG, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROVENANCE)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_PROVENANCE, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_STATISTIC)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_STATISTIC, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PROCESSSTEP)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_PROCESSSTEP, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_LIBNAME)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_LIBRARIES, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VALUE)) {
                String level = currentElement.getLevel();

                if (level.equals(XCEDEElement.XCEDE_ELEMENT_EVENT) ||
                        level.equals(XCEDEElement.XCEDE_ELEMENT_EVENTPARAMS) ||
                        level.equals(XCEDEElement.XCEDE_ELEMENT_EXTENDEDDESCRIPTOR)) {
                    parentElement = currentElement;
                    currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_VALUE, parentElement);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_PCORRECTED)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_PCORRECTED, parentElement);

                String correctionType = atts.getValue(XCEDEElement.XCEDE_ATTR_CORRECTIONTYPE);

                if (correctionType != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_CORRECTIONTYPE, correctionType);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EXTENTS)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_EXTENTS, parentElement);

                String locationFoci = atts.getValue(XCEDEElement.XCEDE_ATTR_LOCATIONFOCI);

                if (locationFoci != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_LOCATIONFOCI, locationFoci);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_LABEL)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_LABEL, parentElement);

                String atlas = atts.getValue(XCEDEElement.XCEDE_ATTR_ATLAS);

                if (atlas != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_ATLAS, atlas);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_THRESH)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_THRESH, parentElement);

                String threshType = atts.getValue(XCEDEElement.XCEDE_ATTR_THRESH_TYPE);

                if (threshType != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_THRESH_TYPE, threshType);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_FWHMSMOOTHNESS)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_FWHMSMOOTHNESS, parentElement);

                String fwhmUnits = atts.getValue(XCEDEElement.XCEDE_ATTR_FWHMUNITS);

                if (fwhmUnits != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_FWHMUNITS, fwhmUnits);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VOXELSIZE)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_VOXELSIZE, parentElement);

                String voxelUnits = atts.getValue(XCEDEElement.XCEDE_ATTR_VOXELUNITS);

                if (voxelUnits != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_VOXELUNITS, voxelUnits);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ACTIVATIONPARAMS)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_ACTIVATIONPARAMS, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_CLUSTERS)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_CLUSTERS, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_VOXEL)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_VOXEL, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ITEM)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_ITEM, parentElement);

                String name = atts.getValue(XCEDEElement.XCEDE_ATTR_NAME);

                if (name != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_NAME, name);
                }

                String type = atts.getValue(XCEDEElement.XCEDE_ATTR_TYPE);

                if (type != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_TYPE, name);
                }

                String units = atts.getValue(XCEDEElement.XCEDE_ATTR_UNITS);

                if (units != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_UNITS, units);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_TAXONOMICCLASS)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_TAXONOMICCLASS, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ACQPARAM)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_ACQPARAM, parentElement);

                String name = atts.getValue(XCEDEElement.XCEDE_ATTR_NAME);

                if (name != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_NAME, name);
                }

                String type = atts.getValue(XCEDEElement.XCEDE_ATTR_TYPE);

                if (type != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_TYPE, name);
                }

                String units = atts.getValue(XCEDEElement.XCEDE_ATTR_UNITS);

                if (units != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_UNITS, units);
                }

                String description = atts.getValue(XCEDEElement.XCEDE_ATTR_DESCRIPTION);

                if (description != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_DESCRIPTION, description);
                }

                String originalValue = atts.getValue(XCEDEElement.XCEDE_ATTR_ORIGINALVALUE);

                if (originalValue != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_ORIGINALVALUE, originalValue);
                }

                String originalUnits = atts.getValue(XCEDEElement.XCEDE_ATTR_ORIGINALUNITS);

                if (originalUnits != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_ORIGINALUNITS, originalUnits);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EVENTS)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_EVENTS, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EVENTPARAMS)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_EVENTS, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_EVENT)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_EVENT, parentElement);

                String name = atts.getValue(XCEDEElement.XCEDE_ATTR_NAME);

                if (name != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_NAME, name);
                }

                String type = atts.getValue(XCEDEElement.XCEDE_ATTR_TYPE);

                if (type != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_TYPE, name);
                }

                String units = atts.getValue(XCEDEElement.XCEDE_ATTR_UNITS);

                if (units != null) {
                    currentElement.put(XCEDEElement.XCEDE_ATTR_UNITS, units);
                }
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_ASSESSMENTVALUE)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_ASSESSMENTVALUE, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_SUMMARYVALUE)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_SUMMARYVALUE, parentElement);
            } else if (currentKey.equals(XCEDEElement.XCEDE_ELEMENT_NORMALIZEDVALUE)) {
                parentElement = currentElement;
                currentElement = new XCEDEElement(XCEDEElement.XCEDE_ELEMENT_NORMALIZEDVALUE, parentElement);
            }
        }
    }
}
