package gov.nih.mipav.model.file;


/**
 * Inherits from FileXML, reads Surface.XML files based on the "surface.xsd" file. Defines specific variables for
 * reading and writing surface.xml files:
 */
public abstract class FileSurfaceXML extends FileXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** array of strings representing the tags under <Surface> in the xml schema. */
    protected static final String[] m_kSurfaceStr = { "Unique-ID", "Material", "Type", "Opacity", "LevelDetail", "Mesh" };

    /** array of strings representing the tags under <Material> in the xml schema. */
    protected static final String[] m_kMaterialStr = { "Ambient", "Diffuse", "Emissive", "Specular", "Shininess" };

    /** array of strings representing the tags under <Mesh> in the xml schema. */
    protected static final String[] m_kMeshStr = { "Vertices", "Normals", "Colors", "Connectivity" };

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileSurfaceXML(String fName, String fDir) {
        super(fName, fDir);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares class for cleanup.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Returns the FileInfoSurfaceXML read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoSurfaceXML getFileInfo() {
        return (FileInfoSurfaceXML) fileInfo;
    }
}
