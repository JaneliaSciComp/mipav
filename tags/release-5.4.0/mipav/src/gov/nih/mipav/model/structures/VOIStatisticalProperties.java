package gov.nih.mipav.model.structures;


import java.util.*;


/**
 * $Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIStatisticalProperties.java $ $Revision: 4 $ $Date: 7/27/04
 * 1:42p $
 */
public class VOIStatisticalProperties implements VOIStatisticList {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    public static final char DELIM = ';';
    
    public static final String TOTAL = "Total";
    
    /** The hashtable of statistics. */
    private Hashtable<String, Object> data;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new VOIStatisticalProperties object.
     */
    public VOIStatisticalProperties() {
        data = new Hashtable<String, Object>();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Gets the total statistics for all elements of the VOIs that are included in this properties object. 
     * For example, if statistics for every slice have been calculated, this method should be used to 
     * store the total of this statistic on all slices.
     * 
     * @param statistic The statistic that has been calculated
     * @return The calculated statistic
     */
    public Object getTotalStatistic(String statistic) {
        return data.get(statistic+TOTAL);
    }
    
    /**
     * Stores the total statistics for all elements of the VOIs that are included in this properties object. 
     * For example, if statistics for every slice have been calculated, this method should be used to 
     * store the total of this statistic on all slices.
     * 
     * @param statistic The statistic that has been calculated
     * @param statisticValue The value of the calculated statistic
     */
    public void setTotalStatistic(String statistic, Object statisticValue) {
        data.put(statistic+TOTAL, statisticValue);
    }
    
    /**
     * Gets the statistic for all the contours of a VOI that exist on a particular 
     * slice.
     * 
     * @param statistic The statistic that has been calculated
     * @param slice The slice that the contours exist on 
     * @return The value of the calculated statistic
     */
    public Object getSliceStatistic(String statistic, int slice) {
        return data.get(statistic+slice+DELIM);
    }
    
    /**
     * Stores the statistic for all the contours of a VOI that exist on a particular 
     * slice.
     * 
     * @param statistic The statistic that has been calculated
     * @param slice The slice that the contours exist on 
     * @param statisticValue The value of the calculated statistic
     */
    public void setSliceStatistic(String statistic, int slice, Object statisticValue) {
        data.put(statistic+slice+DELIM, statisticValue);
    }

    /**
     * Gets the statistic for a particular contour of a VOI without reference to the slice
     * a contour may exist on.
     * 
     * @param statistic The statistic that has been calculated
     * @param contourLabel The contour that this statistic relates to 
     * @return The value of the calculated statistic
     */
    public Object getContourStatistic(String statistic, String contourLabel) {
        return data.get(statistic+contourLabel);
    }
    
    /**
     * Stores the statistic for a particular contour of a VOI without reference to the slice
     * a contour may exist on.
     * 
     * @param statistic The statistic that has been calculated
     * @param contourLabel The contour that this statistic relates to 
     * @param statisticValue The value of the calculated statistic
     */
    public void setContourStatistic(String statistic, String contourLabel, Object statisticValue) {
        data.put(statistic+contourLabel, statisticValue);
    }
    
    /**
     * Gets the statistic for a particular contour on a particular slice of a VOI.  
     * 
     * @param statistic The statistic that has been calculated
     * @param slice The image slice that this statistic relate to
     * @param contourLabel The contour that this statistic relates to
     * @return The value of the calculated statistic
     */
    public Object getSliceContourStatistic(String statistic, int slice, String contourLabel) {
        return data.get(statistic+slice+DELIM+contourLabel);
    }
    
    /**
     * Stores the statistic for a particular contour on a particular slice of a VOI.  
     * 
     * @param statistic The statistic that has been calculated
     * @param slice The image slice that this statistic relate to
     * @param contourLabel The contour that this statistic relates to
     * @param statisticValue The value of the calculated statistic
     */
    public void setSliceContourStatistic(String statistic, int slice, String contourLabel, Object statisticValue) {
        data.put(statistic+slice+DELIM+contourLabel, statisticValue);
    }
    
    /**
     * Gets the statistic for the whole VOI.
     * 
     * @param statistic The statistic that has been calculated
     * @return The value of the calculated statistic
     */
    public Object getVOIStatistic(String statistic) {
        return data.get(statistic);
    }
    
    /**
     * Stores the statistic for the whole VOI.
     * 
     * @param statistic The statistic that has been calculated
     * @param statisticValue The value of the calculated statistic
     */
    public void setVOIStatistic(String statistic, Object statisticValue) {
        data.put(statistic, statisticValue);
    }

    public void setProperty(String string, Object value) {
        data.put(string, value);
    }
    
    public String getProperty(Object key) {
        return data.get(key).toString();
    }
}
