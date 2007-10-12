package gov.nih.mipav.model.structures;


/**
 * interface to carry statistic list information. all users of VOI statistics ought to implement this interface.
 *
 * <p>$Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIStatisticList.java $ $Revision: 6 $ $Date: 12/15/05 5:05p $
 * </p>
 */
public interface VOIStatisticList {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String quantityDescription = "# of Voxels";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String volumeDescription = "Volume";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String areaDescription = "Area";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String avgIntensity = "Avg Voxel Intensity";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String deviationDescription = "Std Dev of Intensity";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String centerDescription = "Center of Mass";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String axisDescription = "Principal Axis";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String eccentricityDescription = "Eccentricity";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String majorAxisDescription = "Major axis length";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String minorAxisDescription = "Minor axis length";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String minIntensity = "Min Intensity";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String maxIntensity = "Max Intensity";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String maxWidthDescription = "Max Width";

    /**
     * string to test for when checking on statics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String perimeterDescription = "Perimeter";
    
    /**
     * string to test for when checking on statics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String sumIntensities = "Sum Intensities";

    /**
     * strings to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String[] statisticDescription = {
        quantityDescription, volumeDescription, areaDescription, perimeterDescription, minIntensity, maxIntensity,
        avgIntensity, deviationDescription, sumIntensities, centerDescription, axisDescription, eccentricityDescription,
        majorAxisDescription, minorAxisDescription
    };

    /** number of statistics in the list. */
    int numberOfStatistics = statisticDescription.length;
}
