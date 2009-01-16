package gov.nih.mipav.model.file.xcede;

import org.w3c.dom.Document;

import java.io.File;

/**
 * The DocumentFactory interface defines a factory API that enables applications to 
 * create a document object from an xml file.
 * 
 * @author Hailong Wang, Ph.D
 * 
 * @version 1.0, 05/18/06
 */

public interface DocumentFactory {
    /**
     * Parses an xml file and create a document object which represents this xml file.
     * 
     * @param fileName   an xml file name.
     * 
     * @return           a document object which represents this xml file.
     */
    public Document createDocument(String fileName);
    /**
     * Parses an xml file and create a document object which represents this xml file.
     * 
     * @param fileName   an xml file.
     * 
     * @return           a document object which represents this xml file.
     */
    public Document createDocument(File file);
}
