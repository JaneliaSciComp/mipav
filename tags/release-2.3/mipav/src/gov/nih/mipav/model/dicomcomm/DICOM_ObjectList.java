package gov.nih.mipav.model.dicomcomm;




import java.util.*;

/**
* Vector of DICOM_Objects.
*
*/
public class DICOM_ObjectList extends Vector {

    /**
    * toString 
    * @param str
    */
    public String toString(String str) {
        String sumStr = new String();
        
        for (int i = 0; i < size(); i++){
            sumStr += ( (DICOM_Object)elementAt(i) ).toString( str + ": " + i);
        }

        return(sumStr);
    }
}
