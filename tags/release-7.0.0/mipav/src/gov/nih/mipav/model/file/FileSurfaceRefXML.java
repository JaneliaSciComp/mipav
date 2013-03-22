package gov.nih.mipav.model.file;


/**
 * Inherits from FileXML, reads SurfaceRef.XML files based on the "surfaceref.xsd" file. Defines specific variables for
 * reading and writing surfaceref.xml files:
 */
public abstract class FileSurfaceRefXML extends FileXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    /** array of strings representing the tags under <Surface> in the xml schema. */
    protected static final String[] m_kSurfaceStr = { "Unique-ID", "Material", "Type", "Opacity", "LevelDetail", "Filename" };

    /** array of strings representing the tags under <Material> in the xml schema. */
    protected static final String[] m_kMaterialStr = { "Ambient", "Diffuse", "Emissive", "Specular", "Shininess" };
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileSurfaceRefXML(String fName, String fDir) {
        super(fName, fDir);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares class for cleanup.
     */
    public void finalize() {
        super.finalize();
    }
}
