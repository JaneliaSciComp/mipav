package gov.nih.mipav.view.renderer.WildMagic.VOI;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * ScreenCoordinateListener
 * This interface defines functions that convert mouse coordinates to image file coordinates.
 * ViewJComponentEditImage is an example of a ScreenCoordinateListener. Given input mouse position from a MouseEvent:
 * MouseEvent.getX(), MouseEvent.getY(), and the current image slice (in local coordinates or 0 for 2D images) 
 * the ScreenCoordinateListener converts the mouse position to image file coordinates.
 * 
 * The ScreenCoordinateListener is responsible for canvas zoom, image file resolutions, and image orientation
 * information that is used to convert the input points. All that information is hidden from the
 * calling function.
 *
 */
public interface ScreenCoordinateListener
{
    /**
     * Converts image file X,Y,Z coordinates into local screen coordinates.
     * @param kFile image file coordinates.
     * @return local screen coordinates.
     */
    public Vector3f fileToScreenVOI( Vector3f kFile );
    /**
     * Returns the height of the display canvas.
     * @return height of the display canvas.
     */
    public int getHeight();
    
    /**
     * Returns the image resolution factor in the screen x-direction.
     * @return image resolution factor in the screen x-direction.
     */
    public float getResolutionX();
    /**
     * Returns the image resolution factor in the screen y-direction.
     * @return image resolution factor in the screen y-direction.
     */
    public float getResolutionY();

    /**
     * Returns the currently displayed image slice.
     * @return the currently displayed image slice.
     */
    public int getSlice();
    /**
     * Returns the width of the display canvas.
     * @return width of the display canvas.
     */
    public int getWidth();
    /**
     * Returns the image magnification factor in the screen x-direction.
     * @return the image magnification factor in the screen x-direction.
     */
    public float getZoomX();
    /**
     * Returns the image magnification factor in the screen y-direction.
     * @return the image magnification factor in the screen y-direction.
     */
    public float getZoomY();
    /**
     * Converts local patient coordinate view (Axial, Coronal, Sagittal) of the image into the original file coordinate
     * representation of the image. 
     * @param kPt local patient (Axial, Coronal, Sagittal) coordinates.
     * @return image file coordinate representation of the input point.
     */
    public Vector3f patientToScreenVOI( Vector3f kPt );
    /**
     * Converts local screen coordinates, usually from a MouseEvent, into image file coordinates.
     * @param iX input X position in local screen coordinates.
     * @param iY input Y position in local screen coordinates.
     * @param iZ input slice position in the local image orientation, or z=0 for 2D images.
     * @param kVolumePt image file coordinates return value.
     * @return returns true if the screen coordinate is clipped (is outside the displayed image space).
     */
    public boolean screenToFileVOI(int iX, int iY, int iZ, Vector3f kVolumePt );
    /**
     * Converts local screen coordinates, usually from a MouseEvent, into image file coordinates.
     * @param kScreen input X,Y,Z position in local screen coordinates, where z = local image slice or 0 for 2D images.
     * @return image file coordinates.
     */
    public Vector3f screenToFileVOI( Vector3f kScreen );
    /**
     * Converts local screen coordinates, usually from a MouseEvent, into image file coordinates.
     * @param kScreen input X,Y,Z position in local screen coordinates, where z = local image slice or 0 for 2D images.
     * @param kFile image file coordinates return value.
     * @return true if the screen coordinate is clipped (is outside the displayed image space).
     */
    public boolean screenToFileVOI( Vector3f kScreen, Vector3f kFile );
}
