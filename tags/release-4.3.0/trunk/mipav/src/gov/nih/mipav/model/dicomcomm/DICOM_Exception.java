package gov.nih.mipav.model.dicomcomm;


/**
 * Used to for DICOM communciation error handling.
 */

public class DICOM_Exception extends Exception {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4502658752483534014L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_Exception object.
     */
    public DICOM_Exception() {
        super();
    }

    /**
     * Creates a new DICOM_Exception object.
     *
     * @param  str  DOCUMENT ME!
     */
    public DICOM_Exception(String str) {
        super(str);
    }
}
