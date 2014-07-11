package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.util.*;


/**
 * Interface to allow different types of ViewJComponents* to work with region grow dialogs.
 *
 * @author  Evan McCreedy
 */
public interface PaintGrowListener {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Get the active image we are growing in.
     *
     * @return  the active image.
     */
    ModelImage getActiveImage();

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between previously supplied bounds.
     *
     * @param  str  the string to prepend to message containing region growth statistics.
     */
    void regionGrow(String str);

    /**
     * Sets whether a fuzzy image is displayed.
     *
     * @param  val  whether to show the fuzzy image.
     */
    void setDisplayFuzzy(boolean val);

    /**
     * Set the fuzzy threshold value.
     *
     * @param  val  value to use when region growing with a fuzzy threshold.
     */
    void setFuzzyThreshold(float val);

    /**
     * Sets the JDialogPaintGrow for this class (usually used to set it to null).
     *
     * @param  dialog  the paint grow dialog.
     */
    void setGrowDialog(RegionGrowDialog dialog);

    /**
     * Determines the lower bound of what values we will grow into (lowThreshold = seedValue - val).
     *
     * @param  val  the lower bound delta.
     */
    void setLess(float val);

    /**
     * Set the maximum distance for the region grow.
     *
     * @param  val  the maximum distance to grow across.
     */
    void setMaxDistance(int val);

    /**
     * Determines the upper bound of what values we will grow into (highThreshold = seedValue + val).
     *
     * @param  val  the upper bound delta.
     */
    void setMore(float val);

    /**
     * Set a bound on the size of the region we are growing.
     *
     * @param  val  the maximum region size.
     */
    void setSizeLimit(int val);

    /**
     * Sets whether to only grow within the currently selected VOI.
     *
     * @param  val  whether to only grow within the currently selected VOI.
     */
    void setUseVOI(boolean val);

    /**
     * Display statistics about the grown region.
     *
     * @param  count  Number of pixels (voxels).
     * @param  str    the string to prepend to message containing region growth statistics.
     */
    void showRegionInfo(int count, String str);

    /**
     * Causes the image to update its paint bit mask and redisplay itself.
     *
     * @param  region    new paint region bit set.
     * @param  backup    whether to save the previous paint mask to allow the update to be un-done.
     * @param  isGrower  whether this paint listener is the one that did the region grow.
     */
    void updatePaint(BitSet region, boolean backup, boolean isGrower);
}
