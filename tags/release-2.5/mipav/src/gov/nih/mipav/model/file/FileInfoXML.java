package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * Abstract base clase contains the information that contains the shared information for XML images and XML surfaces.
 *
 * <p>Derived classes: FileInfoImageXML.java and FileInforSurfaceXML.java</p>
 *
 * @version  0.1 Sept 19, 2002
 * @author   Alexandra Bokinsky, Ph.D.
 * @author   Neva Cherniavsky
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileIO
 * @see      FileInfoXML
 */
public abstract class FileInfoXML extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7709467226605763815L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Name (not path) of the header file. */
    protected String headerFileName;

    /** Brief description of the image - optional XML tag. */
    protected String imageDescription;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Main constructor for FileInfoXML.
     *
     * @param  name       String file name
     * @param  directory  String file directory
     * @param  format     int file format (data type)
     */
    public FileInfoXML(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    JDialogBase dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public abstract void displayAboutInfo(JDialogBase dlog, TransMatrix matrix);


    /**
     * Used to propogate all fileInfoXML private variables to other fileinfos.
     *
     * @param  fInfo  FileInfoXML file info to be copied into
     */
    public abstract void updateFileInfos(FileInfoXML fInfo);

    /**
     * Appends a string to the image description.
     *
     * @param  appendDescription  a brief description of the image to be appended.
     */
    public void appendImageDescription(String appendDescription) {
        imageDescription = imageDescription + appendDescription;
    }

    /**
     * Prepares the class for cleanup.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Returns the name of the header file.
     *
     * @return  name of the header file
     */
    public String getHeaderFileName() {
        return this.headerFileName;
    }

    /**
     * Returns description of the image.
     *
     * @return  a brief description of the image.
     */
    public String getImageDescription() {
        return imageDescription;
    }

    /**
     * Sets the name of the header file.
     *
     * @param  headerFileName  header file name (not path)
     */
    public void setHeaderFileName(String headerFileName) {
        this.headerFileName = headerFileName;
    }

    /**
     * Sets the image description.
     *
     * @param  newDescription  a brief description of the image.
     */
    public void setImageDescription(String newDescription) {
        imageDescription = newDescription;
    }
}
