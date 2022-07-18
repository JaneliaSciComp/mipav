package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;

import java.util.*;


/**
 * Interface for dialogs which gather region grow information and then update a list of listeners who want to know about
 * the region grow.
 *
 * @author   Evan McCreedy
 * @version  1.0
 */
public interface RegionGrowDialog {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Accessor that returns displayFuzzy.
     *
     * @return  whether to show the fuzzy image.
     */
    boolean getDisplayFuzzy();

    /**
     * Accessor that returns the fuzzy threshold.
     *
     * @return  fuzzy threshold.
     */
    float getFuzzyThreshold();

    /**
     * Accessor to the lower bound slider.
     *
     * @return  The value to be subtracted to the seed intensity the defines the lower intensity value that the region
     *          can grow into.
     */
    float getLowerBound();

    /**
     * Accessor to the lower bound blue slider.
     *
     * @return  The value to be subtracted to the seed blue intensity that defines the lower intensity blue value that
     *          the region can grow into.
     */
    float getLowerBoundB();

    /**
     * Accessor to the lower bound green slider.
     *
     * @return  The value to be subtracted to the seed green intensity that defines the lower intensity green value that
     *          the region can grow into.
     */
    float getLowerBoundG();

    /**
     * Accessor to the lower bound red slider.
     *
     * @return  The value to be subtracted to the seed redintensity that defines the lower intensity red value that the
     *          region can grow into.
     */
    float getLowerBoundR();

    /**
     * Accessor that returns the maximum distance from the seed point to a point in the object.
     *
     * @return  The maximum distance from the seed point to the object in units of the image.
     */
    int getMaxDistance();

    /**
     * Accessor that returns the maximum size of the object.
     *
     * @return  The maximum size of the object in units of the image.
     */
    int getMaxSize();

    /**
     * Accessor to the upper bound slider.
     *
     * @return  The value to be added to the seed intensity that defines the upper intensity value that the region can
     *          grow into.
     */
    float getUpperBound();

    /**
     * Accessor to the upper bound blue slider.
     *
     * @return  The value to be added to the seed blue intensity that defines the upper intensity blue value that the
     *          region can grow into.
     */
    float getUpperBoundB();

    /**
     * Accessor to the upper bound green slider.
     *
     * @return  The value to be added to the seed green intensity that defines the upper intensity green value that the
     *          region can grow into.
     */
    float getUpperBoundG();

    /**
     * Accessor to the upper bound red slider.
     *
     * @return  The value to be added to the seed red intensity that defines the upper intensity red value that the
     *          region can grow into.
     */
    float getUpperBoundR();

    /**
     * Accessor that returns useVOI.
     *
     * @return  whether to region grow within a VOI.
     */
    boolean getUseVOI();


    /**
     * Accessor to checkbox that tells if thresholds vary with region growth.
     *
     * @return  boolean that tells if thresholds vary with region growth
     */
    boolean getVariableThresholds();

    /**
     * Tells images which are watching the paint region grow to update themselves.
     *
     * @param  isRegionGrow  whether the update was caused by a region grow or by something else (eg - eraseall)
     * @param  backup        whether to backup the paint region before updating it (to allow for undos).
     * @param  paintMask     the paint mask to tell the listeners about.
     */
    void notifyPaintListeners(boolean isRegionGrow, boolean backup, BitSet paintMask);

    /**
     * Sets the reference to this dialog for all of the images listening to it to null.
     */
    void resetDialogs();

    /**
     * Sets text in positionPanel.
     *
     * @param  posString  String to put in text field.
     */
    void setPositionText(String posString);

    /**
     * Sets the region grow algo associated with this dialog. Used for changing the fuzzy connectedness threshold.
     *
     * @param  regionGrowAlgo  the algorithm doing the actual region growing.
     */
    void setRegionGrowAlgo(AlgorithmRegionGrow regionGrowAlgo);
}
