package gov.nih.mipav.model.dicomcomm;


import java.util.*;


/**
 * Vector of DICOM_Objects.
 */
public class DICOM_ObjectList extends Vector {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5505943813889560828L;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Converts object to string
     *
     * @param   str  Prefix string to the output of the data.
     *
     * @return  Concatinated string of the DICOM object list.
     */
    public String toString(String str) {
        String sumStr = new String();

        for (int i = 0; i < size(); i++) {
            sumStr += ((DICOM_Object) elementAt(i)).toString(str + ": " + i);
        }

        return (sumStr);
    }
}
