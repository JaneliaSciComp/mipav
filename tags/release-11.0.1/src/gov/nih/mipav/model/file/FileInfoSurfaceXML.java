package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;


/**
 * This structure contains the information that describes how an XML surface (see surface.xsd and FileSurfaceXML.java)
 * is stored on disk.
 *
 * @see  FileIO
 * @see  FileInfoXML
 * @see  FileSurfaceXML
 */
public class FileInfoSurfaceXML extends FileInfoXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6815854469776062772L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Unique ID, for the surface:. */
    protected int m_iUnique_ID;

    /** Type keyword for the surface:. */
    protected String m_kType = null;

    /** surface opacity:. */
    protected float m_kOpacity;

    /** surface level of detail:. */
    protected int m_kLevelDetail;

    /** mesh index. */
    protected int meshIndex = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Main constructor for FileInfoSurfaceXML.
     *
     * @param  name       String file name
     * @param  directory  String file directory
     * @param  format     int file format (data type)
     */
    public FileInfoSurfaceXML(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog  JDialogBase dialog box that is written to
     * @param  kMat  DOCUMENT ME!
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix kMat) { }

    /**
     * Prepares the class for cleanup.
     */
    public void finalize() {
        m_kType = null;
        super.finalize();
    }

    /**
     * Returns the unique id for the surface:
     *
     * @return  DOCUMENT ME!
     */
    public int getID() {
        return m_iUnique_ID;
    }

    /**
     * Returns the surface opacity value.
     *
     * @return  surface opacity.
     */
    public float getOpacity() {
      return m_kOpacity;
    }

    /**
     * Returns the surface level of detail.
     *
     * @return  surface level of detail.
     */
    public int getLevelDetail() {
      return m_kLevelDetail;
    }

    /**
     * Returns the surface type keyword:
     *
     * @return  DOCUMENT ME!
     */
    public String getType() {
        return m_kType;
    }

    /**
     * Set the unique id for the surface:
     *
     * @param  iID  DOCUMENT ME!
     */
    public void setID(int iID) {
        m_iUnique_ID = iID;
    }

    /**
     * set the surface opacity keyword:
     *
     * @param  opacity  surface opacity
     */
    public void setOpacity(float opacity) {
      m_kOpacity = opacity;
    }

    /**
     * set the surface level of detail keyword:
     *
     * @param  level of detail
     */
    public void setLevelDetail(int levelDetail) {
      m_kLevelDetail = levelDetail;
    }

    /**
     * set the surface type keyword:
     *
     * @param  kType  DOCUMENT ME!
     */
    public void setType(String kType) {
        m_kType = kType;
    }


    /**
     * Used to propogate all FileInfoSurfaceXML private variables to other FileInfosSurfaceXML.
     *
     * @param  fInfo  FileInfoSurfaceXML file info to be copied into
     */
    public void updateFileInfos(FileInfoXML fInfo) {

        if (this == fInfo) {
            return;
        }

        fInfo.setImageDescription(this.getImageDescription());
        ((FileInfoSurfaceXML) fInfo).setID(this.getID());
        ((FileInfoSurfaceXML) fInfo).setType(this.getType());
        ((FileInfoSurfaceXML) fInfo).setOpacity(this.getOpacity());
        ((FileInfoSurfaceXML) fInfo).setLevelDetail(this.getLevelDetail());
    }
}
