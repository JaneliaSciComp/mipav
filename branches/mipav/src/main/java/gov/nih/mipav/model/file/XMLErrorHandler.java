package gov.nih.mipav.model.file;


import gov.nih.mipav.view.Preferences;

import org.xml.sax.*;


/**
 * Error handler to report errors and warnings from the XML parser. Implements standard SAX ErrorHandler methods, see
 * SAX docs for more info.
 *
 * @see  FileOME
 * @see  FileVOI
 * @see  FileXML
 */
public class XMLErrorHandler implements ErrorHandler {

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Handles parse exception errors by passing the parse exception up as a SAXException.
     *
     * @param   spe  the parse exception
     *
     * @throws  SAXException  passed up with the parse exception info
     */
    public void error(final SAXParseException spe) throws SAXException {
        final String message = "Error: " + getParseExceptionInfo(spe);

        throw new SAXException(message);
    }

    /**
     * Handles parse exception fatal errors by passing the parse exception up as a SAXException.
     *
     * @param   spe  the parse exception
     *
     * @throws  SAXException  passed up with the parse exception info
     */
    public void fatalError(final SAXParseException spe) throws SAXException {
        final String message = "Fatal Error: " + getParseExceptionInfo(spe);

        throw new SAXException(message);
    }

    /**
     * Handles parse exception warnings by outputting them to the debug window.
     *
     * @param   spe  the parse exception
     *
     * @throws  SAXException  not reported for warnings
     */
    public void warning(final SAXParseException spe) throws SAXException {
        Preferences.debug("Warning: " + getParseExceptionInfo(spe), Preferences.DEBUG_FILEIO);
    }

    /**
     * Returns a string describing parse exception details.
     *
     * @param   spe  the parse exception
     *
     * @return  a string containing information about the exception
     */
    private String getParseExceptionInfo(final SAXParseException spe) {
        String systemId = spe.getSystemId();

        if (systemId == null) {
            systemId = "null";
        }

        final String info = "URI=" + systemId + " Line=" + spe.getLineNumber() + ": " + spe.getMessage();

        return info;
    }
}
