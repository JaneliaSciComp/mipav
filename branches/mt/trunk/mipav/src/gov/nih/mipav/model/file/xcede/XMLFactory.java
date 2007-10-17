package gov.nih.mipav.model.file.xcede;

import org.w3c.dom.*;
import gov.nih.mipav.model.structures.*;

import java.io.File;

/**
 * The XMLFactory interface defines a factory API that enables applications to 
 * create an xml file from the ModelImage and return the document object which
 * represents this xml file.
 * 
 * @author Hailong Wang, Ph.D
 * 
 * @version 1.0, 05/17/06
 */

public interface XMLFactory {
    /**
     * Creates an document object from the ModelImage.
     * 
     * @param modelImage   an memory image
     * @return             a document object which represents an xml file.
     */
    public Document createDocument(ModelImage modelImage);
    
    /**
     * Returns the document which created by this factory.
     * @return the document which created by this factory.
     */
    public Document getDocument();
    /**
     * Saves an document object to the xml file.
     * 
     * @param document a document object.
     * @param file     an xml file.
     */
    public void saveXML(Document document, File file);
    /**
     * Saves an document object to the xml file.
     * 
     * @param document a document object.
     * @param fileName     an xml file name.
     */
    public void saveXML(Document document, String fileName);
}
