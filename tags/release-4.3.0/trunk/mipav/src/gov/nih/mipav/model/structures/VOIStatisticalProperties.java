package gov.nih.mipav.model.structures;


import java.util.*;


/**
 * $Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIStatisticalProperties.java $ $Revision: 4 $ $Date: 7/27/04
 * 1:42p $
 */
public class VOIStatisticalProperties implements VOIStatisticList {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Hashtable data;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new VOIStatisticalProperties object.
     */
    public VOIStatisticalProperties() {
        data = new Hashtable();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   key  String
     *
     * @return  String
     */
    public String getProperty(String key) {
        return (String) data.get(key);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  key  String
     * @param  obj  Object
     */
    public void setProperty(String key, Object obj) {
        data.put(key, obj);
    }
}
