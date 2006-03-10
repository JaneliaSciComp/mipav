package gov.nih.mipav.model.dicomcomm;




/**
 * Used to for DICOM communciation error handling.
 *
 */

public class DICOM_Exception extends Exception {

	public DICOM_Exception()           { super();    }
	public DICOM_Exception(String str) { super(str); }
}
