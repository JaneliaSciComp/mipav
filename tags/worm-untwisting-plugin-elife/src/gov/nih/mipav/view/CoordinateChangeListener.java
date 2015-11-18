package gov.nih.mipav.view;


/**
 * This interface identifies a class which wants to know about changes to the current coordinates of another frame (such
 * as ViewJFrameTriImage).
 *
 * @author   Evan McCreedy
 * @version  1.0
 */
public interface CoordinateChangeListener {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * A change has occurred to the current coordinates of an image frame.
     *
     * @param  x  the new x coordinate
     * @param  y  the new y coordinate
     * @param  z  the new z coordinate
     */
    void coordinateChanged(int x, int y, int z);
}
