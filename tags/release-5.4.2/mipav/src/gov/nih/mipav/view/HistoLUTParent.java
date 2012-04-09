package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.event.*;


/**
 * Common interface for swing containers which hold a HistoLUT component so that the image frames and thresholding text
 * areas can be updated correctly.
 *
 * @author   Evan McCreedy
 * @version  1.0
 */
public interface HistoLUTParent {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Handle the dragging of a transfer function point.
     *
     * @param  mouseEvent  the point dragging event
     */
    void dragPoint(MouseEvent mouseEvent);

    /**
     * Indicates if real-time update of the image should take place.
     *
     * @return  <code>true</code> if real time update should take place
     */
    boolean isImageUpdate();

    /**
     * Accessor to disable the all RGB color channels button if one of the lines has moved.
     */
    void setAllOff();

    /**
     * Replaces the LUT and builds a new LUT component.
     *
     * @param  newLUT  New LUT
     */
    void setLUT(ModelLUT newLUT);

    /**
     * Set the range value of the text field.
     *
     * @param  x       the x threshold range value
     * @param  y       the y threshold range value
     * @param  _index  the cursor index
     */
    void setRangeText(float x, float y, int _index);

    /**
     * Redraw the componentLUT.
     */
    void updateComponentLUT();

    /**
     * Update all of the image frames that display the image this histo lut is for.
     *
     * @see    ModelImage#notifyImageDisplayListeners
     *
     * @param  flag  this boolean indicates if the frame that displays the image should re-export the image and apply
     *               new LUT
     */
    void updateFrames(boolean flag);

    /**
     * Sets the position string in the LUT text field.
     *
     * @param  str  the position string
     */
    void updateLUTPositionString(String str);

    /**
     * Method to update the threshold text fields when the lower or upper threshold changes.
     *
     * @param  lower  the lower threshold value
     * @param  upper  the upper threshold value
     */
    void updateThresholdFields(float lower, float upper);
}
