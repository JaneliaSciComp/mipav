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
    String geometricCenterDescription = "Geometric center";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String massCenterDescription = "Center of Mass";

    /**
     * string to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String axisDescription = "Principal Axis with Minimum Moment of Inertia";

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
    
    String circularityDescription = "Circularity";
    
    String solidityDescription = "Solidity";
    
    /**
     * string to test for when checking on statics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String sumIntensities = "Sum Intensities";
    
    String skewnessDescription = "Coefficient of skewness";
    
    String kurtosisDescription = "Coefficient of kurtosis";
    
    /** Largest line segment contained entirely within a slice of a VOI */
    String largestSliceDistanceDescription = "Largest slice distance";
    
    /** Largest line segment contained entirely within a 3D VOI */
    String largestDistanceDescription = "Largest distance";
    
    /** Median */
    String median = "Median Intensity";
    
    /** Mode */
    String mode = "Mode Intensity";
    
    /** Mode Count */
    String modeCount = "Mode Count";
    
    String meanCurvatureDescription = "Mean Curvature";
    
    String stdDevCurvatureDescription = "Std Dev of Curvature";
    
    String meanNegativeCurvatureDescription = "Mean Negative Curvature";
    
    String numberOfIndentationsCurvatureDescription = "Number of Indentations Curvature";
    
    String numberOfIndentationsHullDescription = "Number of Indentations Hull";
    
    String asymmetryIndexDescription = "Asymmetry Index";
    
    String fractalDimensionBoxCountDescription = "Fractal Dimension Box Count";
    
    String fractalDimensionEuclideanDistanceDescription = "Fractal Dimension Euclidean Distance";
    
    String invariantMoment1Description = "Invariant Moment 1";
    
    String invariantMoment2Description = "Invariant Moment 2";
    
    String invariantMoment3Description = "Invariant Moment 3";
    
    String invariantMoment4Description = "Invariant Moment 4";
    
    String invariantMoment5Description = "Invariant Moment 5";
    
    String invariantMoment6Description = "Invariant Moment 6";
    
    String invariantMoment7Description = "Invariant Moment 7";

    /**
     * strings to test for when checking on statistics to calculate.
     *
     * @see  gov.nih.mipav.view.JPanelStatisticsList
     */
    String[] statisticDescription = {
        quantityDescription, volumeDescription, areaDescription, perimeterDescription, circularityDescription,
        solidityDescription, minIntensity, maxIntensity, avgIntensity, deviationDescription, sumIntensities, 
        geometricCenterDescription, massCenterDescription, axisDescription, eccentricityDescription, majorAxisDescription,
        minorAxisDescription, skewnessDescription, kurtosisDescription, largestSliceDistanceDescription, 
        largestDistanceDescription, median, mode, modeCount, meanCurvatureDescription, stdDevCurvatureDescription,
        meanNegativeCurvatureDescription, numberOfIndentationsCurvatureDescription, numberOfIndentationsHullDescription,
        asymmetryIndexDescription, fractalDimensionBoxCountDescription, fractalDimensionEuclideanDistanceDescription,
        invariantMoment1Description, invariantMoment2Description, invariantMoment3Description, invariantMoment4Description,
        invariantMoment5Description, invariantMoment6Description, invariantMoment7Description
    };

    /** number of statistics in the list. */
    int numberOfStatistics = statisticDescription.length;
}
