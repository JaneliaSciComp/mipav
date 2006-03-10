package gov.nih.mipav.model.structures;


import java.util.Hashtable;

/**
 *
 *       $Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIStatisticalProperties.java $
 *       $Revision: 4 $
 *       $Date: 7/27/04 1:42p $
 */
public class VOIStatisticalProperties implements VOIStatisticList
{
   /** */
    private Hashtable data;

    public VOIStatisticalProperties() {
        data = new Hashtable();
    }

    /**
     *
     * @param key String
     * @param obj Object
     */
    public void setProperty(String key, Object obj) {
        data.put(key, obj);
    }

    /**
     *
     * @param key String
     * @return String
     */
    public String getProperty(String key) {
        return (String)data.get(key);
    }
}
