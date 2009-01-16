package gov.nih.mipav.model.file;

/**
 * This class facilitates reading and writing vtk xml
 * @author pandyan
 *
 */
public abstract class FileSurfaceVTKXML extends FileXML {
	
    /**
     * constructor
     * @param fName
     * @param fDir
     */
    public FileSurfaceVTKXML(String fName, String fDir) {
        super(fName, fDir);
    }
}
