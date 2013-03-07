package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;


/**
 * Interface to update display of an image.
 *
 * @version  1.0 March 11, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */

public interface ViewImageUpdateInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * setSlice.
     *
     * @param  slice  Current slice
     */
    void setSlice(int slice);

    /**
     * setTimeSlice.
     *
     * @param  tSlice  Current time volume
     */
    void setTimeSlice(int tSlice);


    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. The extents on this image have
     * changed, so the extents need to be read in again and menus, panes and slide bars adjusted accordingly.
     *
     * @return  boolean confirming successful update
     */
    boolean updateImageExtents();

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. Without LUT changes or image changes
     *
     * @return  boolean confirming successful update
     */
    boolean updateImages();

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     *
     * @param   flag  forces show to re import image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    boolean updateImages(boolean flag);

    /**
     * This methods calls the componentImage's update method to redraw the screen.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   flag        forces show to re import image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming a successful update
     */
    boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode);

}
