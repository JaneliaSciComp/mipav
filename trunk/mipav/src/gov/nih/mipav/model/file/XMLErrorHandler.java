package gov.nih.mipav.model.file;

import gov.nih.mipav.view.Preferences;

import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * Error handler to report errors and warnings from the XML parser.
 * @see FileOME
 * @see FileProject
 * @see FileVOI
 * @see FileXML
 */
class XMLErrorHandler implements ErrorHandler {
    // The following methods are standard SAX ErrorHandler methods.
    // See SAX documentation for more info.

    /**
     * Returns a string describing parse exception details
     * @param spe  the parse exception
     * @return     a string containing information about the exception
     */
    private String getParseExceptionInfo(SAXParseException spe) {
        String systemId = spe.getSystemId();

        if (systemId == null) {
            systemId = "null";
        }
        String info = "URI=" + systemId + " Line=" + spe.getLineNumber() + ": " + spe.getMessage();

        return info;
    }

    /**
     * Handles parse exception warnings by outputting them to the debug window.
     * @param spe  the parse exception
     * @throws SAXException not reported for warnings
     */
    public void warning(SAXParseException spe) throws SAXException {
        Preferences.debug("Warning: " + getParseExceptionInfo(spe), Preferences.DEBUG_FILEIO);
    }

    public void error(SAXParseException spe) throws SAXException {
        String message = "Error: " + getParseExceptionInfo(spe);

        throw new SAXException(message);
    }

    public void fatalError(SAXParseException spe) throws SAXException {
        String message = "Fatal Error: " + getParseExceptionInfo(spe);

        throw new SAXException(message);
    }
}