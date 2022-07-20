package gov.nih.mipav.view;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.UpdateVOISelectionListener;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;

import java.awt.Component;
import java.awt.event.ActionListener;

import javax.swing.event.MouseInputListener;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * VOIHandlerInterface. Defines a list of functions expected of the VOI handling classes.
 * Some are deprecated and should be replaced with the actionPerformed(ActionEvent) function.
 * @see VOIManagerInterface which implements this VOIHandlerInterface.
 */
public interface VOIHandlerInterface extends ActionListener, MouseInputListener
{
    /**
     * Add a UpdateVOISelectionListener so it will receive VOI selection updates.
     * @param listener new UpdateVOISelectionListener.
     */
    public void addVOIUpdateListener(UpdateVOISelectionListener listener);
    /**
     * Delete the selected VOI. This function is deprecated, replace with actionPerformed() using CustomUIBuilder.PARAM_VOI_DELETE
     * @param contoursOnly when true only delete the selected contours in the VOI, do not delete the VOI.
     * @deprecated 
     */
    public void deleteSelectedVOI(boolean contoursOnly);
    /**
     * Delete all VOIs.
     */
    public void deleteVOIs();
    /**
     * Dispose of the implementing class.
     * @param flag not used.
     */
    public void disposeLocal(boolean flag);
    /**
     * Communicates a selection change to all UpdateVOISelectionListeners.
     * @param voi the modified VOI.
     */
    public void fireVOISelectionChange(VOI voi);
    /**
     * Communicates a selection change to all UpdateVOISelectionListeners.
     * @param voi modified VOI.
     * @param curve modified VOIBase.
     */
    public void fireVOISelectionChange(VOI voi, VOIBase curve);
    /**
     * Returns the current active image.
     * @return current active image.
     */
    public ModelImage getActiveImage();
    /**
     * Returns the Component displaying the current active image.
     * @return the Component displaying the current active image.
     */
    public Component getComponentImage();
    /**
     * @return 0.
     * @deprecated
     */
    public int getVOI_ID();
    /**
     * Generates and displays a 1D graph of the average or total intensity of
     * 2.5 VOI of 2.5D image (3D).
     * 
     * @param totalIntensity
     *            if true calculates total sum of the intensity else calculates
     *            the average pixel intensity
     * @param useThreshold
     *            whether or not to threshold this intensity plot
     * @param threshold
     *            the threshold value to use, if thresholding.
     */
    public void graph25VOI_CalcInten(boolean totalIntensity,
            boolean useThreshold, float threshold);
    /**
     * @param voiType
     * @return false
     * @deprecated
     */
    public boolean isNewVoiNeeded(int voiType);
    

    /**
     * Initiate a new VOI.
     * @param presetHue new VOI color
     */
    public void newVOI( float presetHue );
    
    /**
     * Propagates the selected VOI in a given direction. This function is deprecated,
     * replace with actionPerformed(ActionEvent) using CustomUIBuilder.PARAM_VOI_PROPAGATE_UP
     * or CustomUIBuilder.PARAM_VOI_PROPAGATE_DOWN
     * @param direction Up +1, or Down -1
     * @param active unused.    
     * @return true if success, false otherwise
     * @deprecated 
     */
    public boolean propVOI(int direction, boolean active);
    /**
     * Propagates the selected VOI to all slices. This function is deprecated, replace
     * with actionPerformed(ActionEvent) using CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL.
     * @return true if success, false otherwise
     * @deprecated 
     */
    public boolean propVOIAll();
    /**
     * Removes the UpdateVOISelectionListener from the list.
     * @param listener the UpdateVOIselectionListener to remove.
     */
    public void removeVOIUpdateListener(UpdateVOISelectionListener listener);
    /**
     * Resets the current livewire VOI.
     */
    public void resetLivewire();
    /**
     * Selects all VOIs. This function is deprecated, replace with actionPerformed(ActionEvent)
     * using CustomUIBuilder.PARAM_VOI_SELECT_ALL and CustomUIBuilder.PARAM_VOI_SELECT_NONE 
     * @param doSelect
     * @deprecated
     */
    public void selectAllVOIs(boolean doSelect);
    /**
     * Sets the current displayed slice for 3D images. The current center is the slice value of the three
     * orthogonal planes displayed in the ViewJFrameTriImage. When this function is called from ViewJFrameImage
     * the X,Y, parameters are ignored and current slice is the Z-value.
     * @param center ModelImage file coordinates representation of the slice values of the three orthogonal image planes.
     * @deprecated
     */
    public void setCenter( Vector3f center );
    /**
     * Sets the current displayed slice for 3D images. The current center is the slice value of the three
     * orthogonal planes displayed in the ViewJFrameTriImage. When this function is called from ViewJFrameImage
     * the X,Y, parameters are ignored and current slice is the Z-value.
     * @param center ModelImage file coordinates representation of the slice values of the three orthogonal image planes.
     * @param bParent when true call parent.setCenter()
     */
    public void setCenter( Vector3f center, boolean bParent );
    /**
     * Sets the hue that will be used to draw the new VOI.
     * @param presetHue the hue to be used
     */
    public void setPresetHue(float presetHue);
    /**
     * This function is deprecated.
     * @param ID not used.
     * @param UID not used.
     * @deprecated
     */
    public void setVOI_IDs(int ID, int UID);
    /**
     * Opens the color dialog for changing the selected VOI color.
     */
    public void showColorDialog();
}
