package gov.nih.mipav.model.structures;

/** interface to carry statistic list information.  all users of
 *  VOI statistics ought to implement this interface.
 *
 *       $Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIStatisticList.java $
 *       $Revision: 6 $
 *       $Date: 12/15/05 5:05p $
 */
public interface VOIStatisticList {
    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String quantityDescription     = "# of Voxels";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String volumeDescription       = "Volume";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String areaDescription         = "Area";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String avgIntensity            = "Avg Voxel Intensity";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String deviationDescription    = "Std Dev of Intensity";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String centerDescription       = "Center of Mass";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String axisDescription         = "Principal Axis";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String eccentricityDescription = "Eccentricity";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String majorAxisDescription = "Major axis length";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String minorAxisDescription = "Minor axis length";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String minIntensity = "Min Intensity";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String maxIntensity = "Max Intensity";

    /** string to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String maxWidthDescription = "Max Width";

    /** string to test for when checking on statics to calculate
     *   @see gov.nih.mipav.view.JPanelStatisticsList
     */
    static String perimeterDescription = "Perimeter";

    /** strings to test for when checking on statistics to calculate.
    *   @see gov.nih.mipav.view.JPanelStatisticsList
    */
    static String[] statisticDescription = {
        quantityDescription,
        volumeDescription,
        areaDescription,
        perimeterDescription,
        minIntensity,
        maxIntensity,
        avgIntensity,
        deviationDescription,
        centerDescription,
        axisDescription,
        eccentricityDescription,
        majorAxisDescription,
        minorAxisDescription
    };

    /** number of statistics in the list
    */
    static int numberOfStatistics = statisticDescription.length;
}
