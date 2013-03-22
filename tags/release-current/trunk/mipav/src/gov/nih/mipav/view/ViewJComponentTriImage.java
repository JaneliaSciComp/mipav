package gov.nih.mipav.view;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;



/**
 * One of the component images that make up the triplanar view (AXIAL, SAGITTAL, and CORONAL views).
 * 
 * <p>
 * Notes:<br>
 * The protractor does not show up in ViewJComponentEditImage because it is not registered. If newVOI is hit, then the
 * VOI must be registered for the points to show up in EditImage.
 * </p>
 * 
 * <p>
 * For the ViewJFrameTriImage addPoint command, that the ViewJComponentEditImage mode is set to DEFAULT while the
 * ViewJComponentTriImage mode is set to POINT_VOI. For the Protractor command, the ViewJComponentTriImage mode is set
 * to PROTRACTOR while the ViewJComponentEditImage mode is set to DEFAULT. For the Line command, the
 * ViewJComponentTriImage mode is set to LINE while the ViewJComponentEditImage mode is set to DEFAULT. For the
 * ViewJFrameTriImage center command, the ViewJComponentEditImage mode is set to DEFAULT while the
 * ViewJComponentTriImage mode is set to CENTER_VOI. For the boundingBox command, the ViewJComponentEdit image mode is
 * set to DEFAULT while the ViewJComponentTriImage mode is set to CUBE_BOUNDS. For the traverse command, both EditImage
 * and TriImage have their modes set to DEFAULT. NEW_VOI, PAINT_VOI, DROPPER_PAINT, ERASER_PAINT, and PAINT_CAN are set
 * in both EditImage and TriImage.
 * </p>
 * 
 * @author William Gandler
 * @see ViewJFrameTriImage
 * @see ViewJComponentDualTriImage
 */
public class ViewJComponentTriImage extends ViewJComponentEditImage implements MouseWheelListener, KeyListener,
        ScreenCoordinateListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -389214941507734031L;

    /** AC-PC: Superior Edge reference point. Used for conversions by the dialogs that create new AC-PC images. */
    public static final int SUPERIOR_EDGE = 0;

    /** AC-PC: Posterior Margin reference point. Used for conversions by the dialogs that create new AC-PC images. */
    public static final int POSTERIOR_MARGIN = 1;

    /** AC-PC: Inferior Edge reference point. Used for conversions by the dialogs that create new AC-PC images. */
    public static final int INFERIOR_EDGE = 2;

    /** AC-PC: First midsagittal reference point. Used for conversions by the dialogs that create new AC-PC images. */
    public static final int FIRST_PT = 3;

    /** AC-PC: Another midsagittal reference point. Used for conversions by the dialogs that create new AC-PC images. */
    public static final int ANOTHER_PT = 4;

    /** Talairach: Anterior reference point. Used for conversions by the dialogs that create new Talairach images. */
    public static final int ANTERIOR_PT = 5;

    /** Talairach: Posterior reference point. Used for conversions by the dialogs that create new Talairach images. */
    public static final int POSTERIOR_PT = 6;

    /** Talairach: Superior reference point. Used for conversions by the dialogs that create new Talairach images. */
    public static final int SUPERIOR_PT = 7;

    /** Talairach: Inferior reference point. Used for conversions by the dialogs that create new Talairach images. */
    public static final int INFERIOR_PT = 8;

    /** Talairach: Left reference point. Used for conversions by the dialogs that create new Talairach images. */
    public static final int LEFT_PT = 9;

    /** Talairach: Right reference point. Used for conversions by the dialogs that create new Talairach images. */
    public static final int RIGHT_PT = 10;

    /** DOCUMENT ME! */
    protected boolean dragCenterPt = false;

    /** Labels for the axes:. */
    private final String[][] axisLabels = new String[3][2];

    /** Crop Bounding Box Corners in Screen Coordinates:. */
    private final Vector3f[] cornerCropPt = {new Vector3f(), new Vector3f()};

    /** Crop Bounding Box Colors:. */
    private final Color[] cropColor = {Color.red, Color.green, Color.yellow};

    /** Crop Bounding Box in Screen Coordinates:. */
    private final Vector2f[] cropPoints = new Vector2f[4];

    /** The x and y coordinate of the cursor position in screen space of this component. */
    private final Vector3f crosshairPt = new Vector3f(0.0f, 0.0f, 0.0f);

    /**
     * Whether to show the center of rotation point in the volume and allow the user to change it. <code>doCenter</code>
     * is set true by center command and set false by DEFAULT and CUBE_BOUNDS modes.
     */
    private boolean doCenter = false;

    /** DOCUMENT ME! */
    private int dragBBpt = -1; // represents the index of the bounding box point that is currently being dragged

    /** true if the imageActive has an orientation, false if imageActive is of FileInfoBase.UNKNOWN_ORIENT. */
    private boolean hasOrientation = false;

    /** DOCUMENT ME! */
    private int[] horizontalTalGridPts;

    /** imageActive extents in the local (Patient) coordinate system:. */
    private int[] localImageExtents = new int[3];

    /** Lower Crop Bounding Box in Patient Coordinates:. */
    private final Vector3f m_kLocalCropLower = new Vector3f();

    /** Upper Crop Bounding Box in Patient Coordinates:. */
    private final Vector3f m_kLocalCropUpper = new Vector3f();

    /** Cursor 3D point in FileCoordinates. */
    private Vector3f m_kVolumePoint = new Vector3f();


    /**
     * Reordered resolutions according to orientation. See ModelStorageBase.getResolutions( int index, int orientation ).
     * The orientation parameter may be either: FileInfoBase.AXIAL, FileInfoBase.CORONAL, FileInfoBase.SAGITTAL for
     * oriented slices, or FileInfoBase.UNKNOWN_ORIENT for the default slice orientation.
     */
    private float[] res = new float[3];


    /** If true, show XY, XZ, or ZY orientation axes in a corner of the component. */
    protected boolean showAxes = true;

    /** Specifies whether the user wants to show the cropping / paint bounds rectangle. */
    protected boolean showBoundingRect = false;

    /** If true, show crosshairs indicating the currently position within the component. */
    protected boolean showCrosshairs = true;

    /** Whether to show the talairach grid on the component. */
    protected boolean showTalairachGrid = false;

    /** Whether to show the talairach gridmarkers on the component. */
    protected boolean showTalairachGridmarkers = false;

    /** DOCUMENT ME! */
    private final Color talairachMajorLineColor = new Color(253, 253, 253);

    /** DOCUMENT ME! */
    private final Color talairachMinorLineColor = new Color(213, 178, 183);

    /** The tri image frame of which this object is a component. */
    protected final ViewJFrameTriImage triImageFrame;

    /** DOCUMENT ME! */
    private int[] verticalTalGridPts;

    /** The protractor VOI. */
    private VOIProtractor voiProtractor = null;

    /** color of the crosshairs. */
    private final Color[] xColor = {Color.yellow, Color.yellow, Color.green};

    /** DOCUMENT ME! */
    private final Color[] yColor = {Color.green, Color.red, Color.red};

    /** DOCUMENT ME! */
    private final Color[] zColor = {Color.red, Color.green, Color.yellow};

    /** the gap (in one direction) between the center of the crosshair and the actual crosshair line */
    private int crosshairPixelGap = 0;
    
    private int axisOrientation[];
    
    private int xCursorAdjustment = 0;
    
    private int yCursorAdjustment = 0;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * The TriImage component constructor. Sets up the initial slice to show, the placement of the crosshairs, updates
     * the position labels, and the bounding box locations.
     * 
     * <p>
     * Note that reordering occurs in paintBuffer and pixBuffer, but reordering does not occur in imageBuffer. Thus, for
     * the ZY orientation in hasOrientation imageDim.width and imageDim.height are swapped in paintBuffer and pixBuffer,
     * but not in imageBuffer.
     * </p>
     * 
     * @param _frame frame where component will be displayed
     * @param _imageA Model of the image that will be displayed
     * @param _LUTa LUT used to display imageA
     * @param imgBufferA storage buffer used to display imageA
     * @param _imageB Model of the image that will be displayed
     * @param _LUTb LUT used to display imageB
     * @param imgBufferB storage buffer used to display imageB
     * @param pixelBuffer storage buffer used to build a displayable image
     * @param zoom initial magnification of the image
     * @param extents initial display dimensions of the image
     * @param logMagDisplay display log magnitude of the image
     * @param _orientation display orientation of the image
     */
    public ViewJComponentTriImage(final ViewJFrameBase _frame, final ModelImage _imageA, final ModelLUT _LUTa,
            final float[] imgBufferA, final ModelImage _imageB, final ModelLUT _LUTb, final float[] imgBufferB,
            final int[] pixelBuffer, final float zoom, final int[] extents, final boolean logMagDisplay,
            final int _orientation) {
        super(_frame, _imageA, _LUTa, imgBufferA, _imageB, _LUTb, imgBufferB, pixelBuffer, zoom, extents,
                logMagDisplay, _orientation);

        setZoom(zoom, zoom);

        if (imageA.getImageOrientation() == FileInfoBase.UNKNOWN_ORIENT) {
            hasOrientation = false;
            axisLabels[0][0] = new String("X");
            axisLabels[0][1] = new String("Y");
            axisLabels[1][0] = new String("X");
            axisLabels[1][1] = new String("Z");
            axisLabels[2][0] = new String("Z");
            axisLabels[2][1] = new String("Y");
        } else {
            hasOrientation = true;
            axisLabels[0][0] = new String("L");
            axisLabels[0][1] = new String("P");
            axisLabels[1][0] = new String("L");
            axisLabels[1][1] = new String("S");
            axisLabels[2][0] = new String("P");
            axisLabels[2][1] = new String("S");
        }

        triImageFrame = (ViewJFrameTriImage) frame;

        //removeMouseListener(voiHandler.getPopupVOI());
        //removeMouseListener(voiHandler.getPopupPt());

        /* res is ordered based on orientation: */
        res = imageActive.getResolutions(0, orientation);
        
        axisOrientation = imageActive.getFileInfo()[0].getAxisOrientation();
        
        // Adjust cursor when x, y, or z = 0 starts at the right hand boundary or bottom so that you position
        // the cursor from the right hand boundary or bottom rather than 1 away from the right hand boundary or 
        // bottom.
        if (orientation == FileInfoBase.AXIAL) {
        	if ((axisOrientation[0] == FileInfoBase.ORI_L2R_TYPE) || (axisOrientation[1] == FileInfoBase.ORI_L2R_TYPE) ||
        	    (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
        	    	xCursorAdjustment = 1;
        	    }
            if ((axisOrientation[0] == FileInfoBase.ORI_P2A_TYPE) || (axisOrientation[1] == FileInfoBase.ORI_P2A_TYPE) ||
               (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
            	   yCursorAdjustment = 1;
               }
        }
        
        if (orientation == FileInfoBase.SAGITTAL) {
        	if ((axisOrientation[0] == FileInfoBase.ORI_P2A_TYPE) || (axisOrientation[1] == FileInfoBase.ORI_P2A_TYPE) ||
                    (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
                 	   xCursorAdjustment = 1;
                    }
        	if ((axisOrientation[0] == FileInfoBase.ORI_I2S_TYPE) || (axisOrientation[1] == FileInfoBase.ORI_I2S_TYPE) ||
        	    (axisOrientation[2] == FileInfoBase.ORI_I2S_TYPE)) {
        	    	yCursorAdjustment = 1;
        	    }
        }
        
        if (orientation == FileInfoBase.CORONAL) {
        	if ((axisOrientation[0] == FileInfoBase.ORI_L2R_TYPE) || (axisOrientation[1] == FileInfoBase.ORI_L2R_TYPE) ||
            	    (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
            	    	xCursorAdjustment = 1;
            	    }
        	if ((axisOrientation[0] == FileInfoBase.ORI_I2S_TYPE) || (axisOrientation[1] == FileInfoBase.ORI_I2S_TYPE) ||
            	    (axisOrientation[2] == FileInfoBase.ORI_I2S_TYPE)) {
            	    	yCursorAdjustment = 1;
            	    }
        }

        if ( (res[0] == 0.0f) || (res[1] == 0.0f) || (res[2] == 0.0f)) {
            res[0] = 1.0f;
            res[1] = 1.0f;
            res[2] = 1.0f;
        }

        /* localImageExtents is ordered based on orientation: */
        localImageExtents = imageActive.getExtents(orientation);

        try {
            crosshairPixelGap = Integer.parseInt(Preferences.getProperty(Preferences.PREF_CROSSHAIR_PIXEL_GAP));
        } catch (final Exception e) {
            Preferences.setProperty(Preferences.PREF_CROSSHAIR_PIXEL_GAP, "0");
        }

        removeMouseWheelListener(this); // remove listener from superclass
        addMouseWheelListener(this);
        addKeyListener(this);
    }
    
    public void clearProtractor()
    {
        if ( voiManager != null && voiProtractor != null )
        {
            voiManager.deleteVOI(voiProtractor);
        }
        voiProtractor = null;
    }

    /**
     * Computes the talairach grid voxel from a screen coordinate. Also updates the talairach voxel label in the
     * tri-planar frame.
     * 
     * @param x screen x
     * @param y screen y
     */
    public void computeTalairachVoxelPosition(final int x, final int y) {

        if ( (horizontalTalGridPts == null) || (verticalTalGridPts == null)) {
            return;
        }

        String txt = triImageFrame.gettTalVoxLabel().getText();
        StringBuffer talVoxelLabelText = new StringBuffer(txt.subSequence(txt.indexOf(":")+2, txt.length()));

        if (talVoxelLabelText == null) {
            talVoxelLabelText = new StringBuffer(5);
        }

        if (orientation == FileInfoBase.AXIAL) {
            final String[] verticalIndexArray = new String[] {"d", "c", "b", "a", "a", "b", "c", "d"};
            final String[] horizontalIndexArray = new String[] {"A", "B", "C", "D", "E", "F", "G", "H", "I"};

            for (int i = 0; i < verticalTalGridPts.length - 1; i++) {

                if ( (x >= verticalTalGridPts[i]) && (x <= verticalTalGridPts[i + 1])) {
                    talVoxelLabelText.replace(1, 2, verticalIndexArray[i]);

                    break;
                }
            }

            for (int i = 0; i < horizontalTalGridPts.length - 1; i++) {

                if ( (y >= horizontalTalGridPts[i]) && (y <= horizontalTalGridPts[i + 1])) {
                    talVoxelLabelText.replace(0, 1, horizontalIndexArray[i]);

                    break;
                }
            }

            if (x > verticalTalGridPts[4]) {
                talVoxelLabelText.replace(2, 3, "R");
            } else {
                talVoxelLabelText.replace(2, 3, "L");
            }
        } else if (orientation == FileInfoBase.SAGITTAL) {
            final String[] verticalIndexArray = new String[] {"A", "B", "C", "D", "E", "F", "G", "H", "I"};
            final String[] horizontalIndexArray = new String[] {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                    "11", "12", "13"};

            for (int i = 0; i < verticalTalGridPts.length - 1; i++) {

                if ( (x >= verticalTalGridPts[i]) && (x <= verticalTalGridPts[i + 1])) {
                    talVoxelLabelText.replace(0, 1, verticalIndexArray[i]);

                    break;
                }
            }

            for (int i = 0; i < horizontalTalGridPts.length - 1; i++) {

                if ( (y >= horizontalTalGridPts[i]) && (y <= horizontalTalGridPts[i + 1])) {
                    talVoxelLabelText.replace(3, 5, horizontalIndexArray[i]);

                    break;
                }
            }
        } else // CORONAL
        {
            final String[] verticalIndexArray = new String[] {"d", "c", "b", "a", "a", "b", "c", "d"};
            final String[] horizontalIndexArray = new String[] {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                    "11", "12", "13"};

            for (int i = 0; i < verticalTalGridPts.length - 1; i++) {

                if ( (x >= verticalTalGridPts[i]) && (x <= verticalTalGridPts[i + 1])) {
                    talVoxelLabelText.replace(1, 2, verticalIndexArray[i]);

                    break;
                }
            }

            for (int i = 0; i < horizontalTalGridPts.length - 1; i++) {

                if ( (y >= horizontalTalGridPts[i]) && (y <= horizontalTalGridPts[i + 1])) {
                    talVoxelLabelText.replace(3, 5, horizontalIndexArray[i]);

                    break;
                }
            }

            if (x > verticalTalGridPts[4]) {
                talVoxelLabelText.replace(2, 3, "R");
            } else {
                talVoxelLabelText.replace(2, 3, "L");
            }
        }

        triImageFrame.settTalVoxLabelText("Talairach Voxel: " + new String(talVoxelLabelText));
    }

    /**
     * Delete all VOIs in the active image.
    public void deleteAllVOIs() {
        int i;
        int nVOI;

        final ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = nVOI - 1; i >= 0; i--) {
            VOIs.removeElementAt(i);
        }

        System.err.println( "    TRIImage.deleteAllVOIs" );
        voiHandler.setVOI_ID(-1);
        imageActive.notifyImageDisplayListeners(null, true);
    }
     */

    /**
     * Sets whether axes (either anatomical or x,y,z if no orientation info) are shown or not in one of the corners of
     * the component.
     * 
     * @param doShowAxes whether to show the axes
     */
    public void displayAxes(final boolean doShowAxes) {
        this.showAxes = doShowAxes;
    }

    /**
     * Sets whether or not crosshairs are shown in the images.
     * 
     * @param doShowCrosshairs whether to show the tri-image crosshairs
     */
    public void displayXHairs(final boolean doShowCrosshairs) {
        this.showCrosshairs = doShowCrosshairs;
    }

    /**
     * Sets all variables to null, disposes, and garbage collects (sometimes).
     * 
     * @param flag if true garbage collector should be called.
     */
    public void disposeLocal(final boolean flag) {
        voiProtractor = null;

        if (flag == true) {
            super.disposeLocal(true);
        }
    }

    /**
     * Returns the current center point in FileCoordinates:
     * 
     * @return DOCUMENT ME!
     */
    public Vector3f getCenter() {
        final Vector3f patientMousePoint = new Vector3f();
        super.ScreenToLocal(crosshairPt, patientMousePoint);
        MipavCoordinateSystems.patientToFile(patientMousePoint, m_kVolumePoint, imageActive, orientation);

        return new Vector3f(m_kVolumePoint.X, m_kVolumePoint.Y, m_kVolumePoint.Z);
    }

    /**
     * Returns the current cross hair position in screen coordinates.
     * 
     * @return crosshairPt, the current cross-hair position in screen coordinates
     */
    public Vector2f getCrosshairPoint() {
        return new Vector2f(crosshairPt.X, crosshairPt.Y);
    }

    /**
     * Returns the extents of the tri planar component (in the component's order, not the image volume's).
     * 
     * @return the extents of the tri image component
     */
    public int[] getExtents() {
        return localImageExtents;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public float[] getResolutions() {
        return res;
    }

    /**
     * Accepts a point in volume space and converts it to screen space.
     * 
     * @param point3d Point3D the point in volume space
     * 
     * @return Point
     */
    public Vector2f getScreenCoordinates(final Vector3f point3d) {
        final Vector3f patientPt = new Vector3f();
        MipavCoordinateSystems.fileToPatient(point3d, patientPt, imageActive, orientation);

        final Vector3f screenPt = new Vector3f();
        super.LocalToScreen(patientPt, screenPt);

        return new Vector2f(screenPt.X, screenPt.Y);
    }

    /**
     * Accessor that returns the protractor angle.
     * 
     * @return the protractor angle
     */
    public double getTheta() {
    	double theta;
        if ( voiProtractor != null )
        {
            theta = voiProtractor.getTheta(imageActive.getFileInfo()[0].getResolutions());
            Vector2f protractor0 = getScreenCoordinates(voiProtractor.get(0));
            Vector2f protractor2 = getScreenCoordinates(voiProtractor.get(2));
            if (protractor2.Y < protractor0.Y) {
            	theta = -theta;
            }
            return theta;
        }
        return 0;
    }
    
    public Vector3f getProtractorCommonVertex() {
    	if (voiProtractor != null) {
    	    return voiProtractor.get(1);
    	}
    	else {
    		return null;
    	}
    }

    /**
     * Translate a point on the x-y tri-image component into image volume space. Assumes input parameters have zoom and
     * voxel resolution already factored out.
     * 
     * @param x x value of the point within the component
     * @param y y value of the point within the component
     * @param z the z coordinate ( usually == slice ) (the out-of-component dimension)
     * 
     * @return the point translated into the image volume
     */
    public final Point3D getVolumePosition(final int x, final int y, final int z) {
        final Vector3f volumePt = getVolumePosition((float) x, (float) y, (float) z);
        final Point3D returnPt = new Point3D((int) volumePt.X, (int) volumePt.Y, (int) volumePt.Z);

        return returnPt;
    }

    /**
     * Translate a point on the x-y tri-image component into image volume space. Assumes input parameters have zoom and
     * voxel resolution already factored out.
     * 
     * @param x x value of the point within the component
     * @param y y value of the point within the component
     * @param z the z coordinate ( usually == slice ) (the out-of-component dimension)
     * 
     * @return the point translated into the image volume
     */
    public final Vector3f getVolumePosition(final float x, final float y, final float z) {
        final Vector3f volumePt = new Vector3f();
        MipavCoordinateSystems.patientToFile(new Vector3f(x, y, z), volumePt, imageActive, orientation);

        return volumePt;
    }

    /**
     * Get the color for the crosshairPt.X crosshair.
     * 
     * @return the x crosshair color
     */
    public Color getXSliceHairColor() {
        return xColor[orientation];
    }

    /**
     * Get the color for the ySlice crosshair.
     * 
     * @return the y crosshair color
     */
    public Color getYSliceHairColor() {
        return yColor[orientation];
    }

    /**
     * Get the color for the zSlice crosshair.
     * 
     * @return the z crosshair color
     */
    public Color getZSliceHairColor() {
        return zColor[orientation];
    }

    /**
     * Returns whether the bounding rectangle should be drawn.
     * 
     * @return whether the bounding rectangle should be drawn
     */
    public boolean isShowBoundingRect() {
        return showBoundingRect;
    }

    /**
     * keyReleased event method for KeyListener.
     * 
     * @param e KeyEvent
     */
    public void keyPressed(final KeyEvent e) {
        final int keyCode = e.getKeyCode();
        Vector3f currentCenterLocal = null;
        Vector3f newScreenPoint = null;
        Vector3f newLocalPoint  = null;
        String xString;
	    String yString;
	    String zString;
	    int x;
        int y;
        int z;
        int newY, newZ, newX;
        int[] exts = imageA.getExtents();
        switch (keyCode) {

            case KeyEvent.VK_PAGE_DOWN:
                updateSlice(slice - 1);
                break;

            case KeyEvent.VK_PAGE_UP:
                updateSlice(slice + 1);
                break;
                
                
                
            case KeyEvent.VK_UP:
            	if (orientation == FileInfoBase.CORONAL) {
            	     yString = triImageFrame.absoluteYTextField.getText().trim();
                     y = Integer.parseInt(yString);
                     if(y > 0) {
                    	newY = y - 1;
                 		triImageFrame.absoluteYTextField.setText(String.valueOf(newY));
                 		triImageFrame.absoluteGoTo();
                     }   
            	}else if(orientation == FileInfoBase.SAGITTAL) {
            	     yString = triImageFrame.absoluteYTextField.getText().trim();
                     y = Integer.parseInt(yString);
                     if(y > 0) {
                    	newY = y - 1;
                 		triImageFrame.absoluteYTextField.setText(String.valueOf(newY));
                 		triImageFrame.absoluteGoTo();
                     }   
            	}else if(orientation == FileInfoBase.AXIAL) {
            	     zString = triImageFrame.absoluteZTextField.getText().trim();
                     z = Integer.parseInt(zString);
                     if(z < exts[2]) {
                    	newZ = z + 1;
                 		triImageFrame.absoluteZTextField.setText(String.valueOf(newZ));
                 		triImageFrame.absoluteGoTo();
                     }   
            	}
                break;

                
            case KeyEvent.VK_DOWN:
            	if (orientation == FileInfoBase.CORONAL) {
            		yString = triImageFrame.absoluteYTextField.getText().trim();
                    y = Integer.parseInt(yString);
                    if(y < exts[1]) {
                    	newY = y + 1;
                		triImageFrame.absoluteYTextField.setText(String.valueOf(newY));
                		triImageFrame.absoluteGoTo();
                    }   
	           	}else if(orientation == FileInfoBase.SAGITTAL) {
	           	     	yString = triImageFrame.absoluteYTextField.getText().trim();
	                    y = Integer.parseInt(yString);
	                    if(y < exts[1]) {
	                    	newY = y + 1;
	                		triImageFrame.absoluteYTextField.setText(String.valueOf(newY));
	                		triImageFrame.absoluteGoTo();
	                    }   
	           	}else if(orientation == FileInfoBase.AXIAL) {
	           	     	zString = triImageFrame.absoluteZTextField.getText().trim();
	                    z = Integer.parseInt(zString);
	                    if(z > 0) {
	                    	newZ = z - 1;
	                		triImageFrame.absoluteZTextField.setText(String.valueOf(newZ));
	                		triImageFrame.absoluteGoTo();
	                    }   
	           	}
	            break;      
                
                
            case KeyEvent.VK_RIGHT:
            	if (orientation == FileInfoBase.CORONAL) {
           		 	xString = triImageFrame.absoluteXTextField.getText().trim();
           		 	x = Integer.parseInt(xString);
                    if(x > 0) {
                    	newX = x - 1;
                		triImageFrame.absoluteXTextField.setText(String.valueOf(newX));
                		triImageFrame.absoluteGoTo();
                    }   
	           	}else if(orientation == FileInfoBase.SAGITTAL) {
	           	     	zString = triImageFrame.absoluteZTextField.getText().trim();
	                    z = Integer.parseInt(zString);
	                    if(z > 0) {
	                    	newZ = z - 1;
	                		triImageFrame.absoluteZTextField.setText(String.valueOf(newZ));
	                		triImageFrame.absoluteGoTo();
	                    }   
	           	}else if(orientation == FileInfoBase.AXIAL) {
	           		 	xString = triImageFrame.absoluteXTextField.getText().trim();
	           		 	x = Integer.parseInt(xString);
	                    if(x > 0) {
	                    	newX = x - 1;
	                		triImageFrame.absoluteXTextField.setText(String.valueOf(newX));
	                		triImageFrame.absoluteGoTo();
	                    }   
	           	}
	            break;
                 
              
                
            case KeyEvent.VK_LEFT:
            	if (orientation == FileInfoBase.CORONAL) {
              		 	xString = triImageFrame.absoluteXTextField.getText().trim();
              		 	x = Integer.parseInt(xString);
              		 	if(x < exts[0]) {
              		 		newX = x + 1;
              		 		triImageFrame.absoluteXTextField.setText(String.valueOf(newX));
              		 		triImageFrame.absoluteGoTo();
                        }   
              	}else if(orientation == FileInfoBase.SAGITTAL) {
              	     	zString = triImageFrame.absoluteZTextField.getText().trim();
              	     	z = Integer.parseInt(zString);
              	     	if(z < exts[2]) {
              	     		newZ = z + 1;
              	     		triImageFrame.absoluteZTextField.setText(String.valueOf(newZ));
              	     		triImageFrame.absoluteGoTo();
                       }   
              	}else if(orientation == FileInfoBase.AXIAL) {
              		 xString = triImageFrame.absoluteXTextField.getText().trim();
              	     x = Integer.parseInt(xString);
              	     if(x < exts[0]) {
                	    newX = x + 1;
               			triImageFrame.absoluteXTextField.setText(String.valueOf(newX));
               			triImageFrame.absoluteGoTo();
                    }   
              	}
                break;
        }
    }


    @Override
    public void keyReleased(KeyEvent e) {}

    @Override
    public void keyTyped(KeyEvent e) {}
    
    
    /**
     * Constructs and initializes one of the 3 protractors, depending on which component this is.
     */
    public void makeProtractor() {

        try {
            if (voiProtractor != null) {
                imageActive.notifyImageDisplayListeners();

                return;
            }

            /* presetHue: 0.0f for first segment red hue, 1/3 for green, 1/6 for blue: */
            final float[] presetHue = {0.0f, 0.3333f, 0.1667f};
            voiProtractor = new VOIProtractor(); 
            // first end point
            Vector3f kVolumePt = new Vector3f();
            int halfWidth = (int)(getWidth() / 2f);
            int halfHeight = (int)(getHeight() / 2f);
            screenToFile( halfWidth + 20, halfHeight, slice, kVolumePt );
            voiProtractor.add( kVolumePt );
            // middle point
            kVolumePt = new Vector3f();
            screenToFile( halfWidth - 20, halfHeight, slice, kVolumePt );
            voiProtractor.add( kVolumePt );
            // second end point
            kVolumePt = new Vector3f();
            screenToFile( halfWidth + 30, halfHeight, slice, kVolumePt );
            voiProtractor.add( kVolumePt );
            
            voiProtractor.setActive(true);
            voiProtractor.setAllSlices(true);
            if ( voiManager != null )
            {
                voiProtractor.setPlane(voiManager.getPlane());
                voiManager.add( voiProtractor, presetHue[orientation] );
            }

            repaint();
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentTriImage.makeProtractor");
            setCursorMode(ViewJComponentBase.DEFAULT);

            return;
        }
    }

    /**
     * When an image is double-clicked with the right-mouse button, the slice of the parent image is updated to match
     * the current volume slice.
     * 
     * @param mouseEvent mouse event
     */
    public void mouseClicked(final MouseEvent mouseEvent) {

        if ( ( (mouseEvent.getModifiers() & InputEvent.BUTTON3_MASK) != 0) && (mouseEvent.getClickCount() == 2)) {
            (triImageFrame.getParentFrame()).setSlice((int) m_kVolumePoint.Z);
        }

    }

    /**
     * A mouse-dragged event.
     * 
     * @param mouseEvent event that triggered function
     */
    public void mouseDragged(final MouseEvent mouseEvent) {
        if ( (mouseEvent.getX() < 0) || (mouseEvent.getY() < 0) || (mouseEvent.getX() > getWidth())
                || (mouseEvent.getY() > getHeight())) {
            return;
        }
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        int xS = Math.max(getScaledX(mouseEvent.getX()), 0);
        int yS = Math.max(getScaledY(mouseEvent.getY()), 0);
        xS = Math.min(xS, imageDim.width - 1);
        yS = Math.min(yS, imageDim.height - 1);
        if (dragCenterPt) // if user is moving the center point
        {
            if (doCenter) {
                setCursor(MipavUtil.blankCursor);

                final Vector3f patientMousePoint = new Vector3f();
                super.ScreenToLocal(new Vector3f(mouseEvent.getX(), mouseEvent.getY(), slice), patientMousePoint);

                final Vector3f volumeMousePoint = new Vector3f();
                MipavCoordinateSystems.patientToFile(patientMousePoint, volumeMousePoint, imageActive, orientation);
                triImageFrame.setCenter((int) volumeMousePoint.X, (int) volumeMousePoint.Y, (int) volumeMousePoint.Z);

                frame.updateImages();

                return;
            }
        }
        if (cursorMode == DEFAULT) {
            if ( (mouseEvent.getModifiers() & InputEvent.BUTTON3_MASK) != 0) {

                // adjust window and level when in DEFAULT mode and dragging with right-button
                super.mouseDragged(mouseEvent);

                return;
            }

            // Hides the cursor during dragging so it doesn't get in the way.
            if (showCrosshairs == true) {
                setCursor(MipavUtil.blankCursor);
            }

            final Vector3f patientMousePoint = new Vector3f();
            super.ScreenToLocal(new Vector3f(mouseEvent.getX(), mouseEvent.getY(), slice), patientMousePoint);
            MipavCoordinateSystems.patientToFile(patientMousePoint, m_kVolumePoint, imageActive, orientation);
            triImageFrame.setCenter((int) m_kVolumePoint.X, (int) m_kVolumePoint.Y, (int) m_kVolumePoint.Z);

            return;
        } 
        else if (cursorMode == CUBE_BOUNDS) {
            final Vector2f mousePoint = new Vector2f(mouseEvent.getX(), mouseEvent.getY());
            // if we are not already dragging a point, see if the mouse event is near one of the corners
            if (dragBBpt == -1) {
                for (int i = 0; i < cropPoints.length; i++) {
                    if ((float) MipavMath.distance(cropPoints[i].X, mousePoint.X, cropPoints[i].Y, mousePoint.Y) < 5) {
                        // if we are dragging near a box corner, set 'dragBBpt' to indicate that point
                        dragBBpt = i;
                        break;
                    }
                }
            }
            if (dragBBpt != -1) {
                updateCrop(dragBBpt, mousePoint);
            }
        } 
        else if (cursorMode == ViewJComponentBase.PAINT_VOI) {
            final boolean isLeftMouseButtonDown = mouseEvent.getModifiers() == InputEvent.BUTTON1_MASK;
            updatePaintBitmap(isLeftMouseButtonDown, xS, yS);

            if (Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT)) {
                repaint();
                return;
            }
        } // end of else if (mode == PAINT_VOI)
        else if (cursorMode == ViewJComponentBase.ERASER_PAINT) {
            updatePaintBitmap(false, xS, yS);

            if (Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT)) {
                repaint();
                return;
            }
        } // end of else if (mode == ERASER_PAINT)
        else if (cursorMode == ViewJComponentBase.DROPPER_PAINT) {

            if ( (triImageFrame.getSelectedImage() == ViewJComponentBase.IMAGE_A) || (imageBufferB == null)) {
                intensityDropper = imageBufferA[ (yS * imageDim.width) + xS];
            } else {
                intensityDropper = imageBufferB[ (yS * imageDim.width) + xS];
            }

            triImageFrame.setIntensityDropper(intensityDropper);
            triImageFrame.setIntensityPaintName(intensityDropper);
            return;
        }
        repaint();
    }

    /**
     * Mouse entry handler: tells the parent tri-image frame about the current component orientation.
     * 
     * @param mouseEvent event that triggers this function
     */
    public void mouseEntered(final MouseEvent mouseEvent) {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        /* Get focus from the ViewJFrameTriImage for key events: */
        this.requestFocusInWindow();
        triImageFrame.setCurrentOrientation(orientation);
    }

    /**
     * Mouse exit handler: repaints the image to get rid of the cursor if we are in a mode that custom draws the mouse
     * cursor.
     * 
     * @param mouseEvent event that triggered function
     */
    public void mouseExited(final MouseEvent mouseEvent) {
        lastMouseX = OUT_OF_BOUNDS;
        lastMouseY = OUT_OF_BOUNDS;

        if ( (cursorMode == ViewJComponentBase.PAINT_VOI) || (cursorMode == ViewJComponentBase.ERASER_PAINT)) {
            paintComponent(getGraphics());
        }

        /* Return key-event focus to the ViewJFrameTriImage: */
        triImageFrame.requestFocusInWindow();
    }

    /**
     * A mouse event. If the mode is level set, draws level sets as user moves mouse. Otherwise, changes the cursor
     * depending on where the mouse is in relation to the VOI.
     * 
     * @param mouseEvent event that triggered the function
     */
    public void mouseMoved(final MouseEvent mouseEvent) {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        final int xS = getScaledX(mouseEvent.getX());
        final int yS = getScaledY(mouseEvent.getY());

        if ( (xS < 0) || (xS >= imageDim.width) || // Check to ensure point is within image bounds
                (yS < 0) || (yS >= imageDim.height)) {
            return;
        }
        if ((cursorMode == ViewJComponentBase.DROPPER_PAINT)
                || (cursorMode == ViewJComponentBase.CUBE_BOUNDS)) {
            return;
        }

        if ( (cursorMode == ViewJComponentBase.PAINT_VOI) || (cursorMode == ViewJComponentBase.ERASER_PAINT)) {
            repaint();

            return;
        }

        if (cursorMode == ViewJComponentBase.PAINT_CAN) {

            if (growDialog != null) {
                final Vector3f patientMousePoint = new Vector3f();
                super.ScreenToLocal(new Vector3f(mouseEvent.getX(), mouseEvent.getY(), slice), patientMousePoint);

                final Vector3f volumeMousePoint = new Vector3f();
                MipavCoordinateSystems.patientToFile(patientMousePoint, volumeMousePoint, imageActive, orientation);

                // the "++" here is to make the display 1-based, like the crosshairs, instead of 0-based
                volumeMousePoint.X++;
                volumeMousePoint.Y++;
                volumeMousePoint.Z++;

                growDialog.setPositionText("  X: " + String.valueOf(volumeMousePoint.X) + " Y: "
                        + String.valueOf(volumeMousePoint.Y) + " Z: " + String.valueOf(volumeMousePoint.Z)
                        + "  Intensity:  " + String.valueOf(imageBufferActive[ (yS * imageDim.width) + xS]));
            }

            return;
        }
        else if (cursorMode == ViewJComponentBase.ZOOMING_IN) {
            return;
        } else if (cursorMode == ViewJComponentBase.ZOOMING_OUT) {
            return;
        } else if (cursorMode == ViewJComponentBase.VOI_3D) {
            return;
        } else {
            setCursorMode(ViewJComponentBase.DEFAULT);

            return;
        } // else
    }

    /**
     * A mouse-pressed event. Sets the mode of the program depending on the cursor mode. If the mode is move, activates
     * the contour or line and enables the delete button.
     * 
     * @param mouseEvent event that triggered this function
     */
    public void mousePressed(final MouseEvent mouseEvent) {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        final int xS = getScaledX(mouseEvent.getX()); // zoomed x. Used as cursor
        final int yS = getScaledY(mouseEvent.getY()); // zoomed y. Used as cursor

        if ( (xS < 0) || (xS >= imageDim.width) || (yS < 0) || (yS >= imageDim.height)) {
            return;
        }

        if (mouseEvent.getModifiers() == InputEvent.BUTTON1_MASK) {
            if (doCenter) {

                if ( (Math.abs(getScaledX(mouseEvent.getX()) - crosshairPt.X) < 6)
                        && (Math.abs(getScaledY(mouseEvent.getY()) - crosshairPt.Y) < 6)) {
                    dragCenterPt = true;
                } else {
                    dragCenterPt = false;
                }
            }

            if (cursorMode == ViewJComponentBase.DEFAULT) {
                //handleVOIProcessing(mouseEvent);
            }
        }

        if (cursorMode == ViewJComponentBase.DEFAULT) {
            return;
        } 
        else if (cursorMode == ViewJComponentBase.PAINT_VOI) {
            final boolean isLeftMouseButtonDown = mouseEvent.getModifiers() == InputEvent.BUTTON1_MASK;
            updatePaintBitmap(isLeftMouseButtonDown, xS, yS);

            if (Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT)) {
                repaint();

                return;
            }
        } // end of else if (mode == PAINT_VOI)
        else if (cursorMode == ViewJComponentBase.ERASER_PAINT) {
            updatePaintBitmap(false, xS, yS);

            if (Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT)) {
                repaint();

                return;
            }
        } // end of else if (mode == ERASER_PAINT)
        else if (cursorMode == ViewJComponentBase.DROPPER_PAINT) {

            if ( (triImageFrame.getSelectedImage() == ViewJComponentBase.IMAGE_A) || (imageBufferB == null)) {
                intensityDropper = imageBufferA[ (yS * imageDim.width) + xS];
            } else {
                intensityDropper = imageBufferB[ (yS * imageDim.width) + xS];
            }

            triImageFrame.setIntensityDropper(intensityDropper);
            triImageFrame.setIntensityPaintName(intensityDropper);

            imageActive.notifyImageDisplayListeners(null, true);
        } // end of else if (mode == DROPPER_PAINT)

        //frame.updateImages();

        return;
    }

    /**
     * A mouse-released event. This function sets up and draws the VOI according to the mode.
     * 
     * @param mouseEvent event that triggered this function
     */
    public void mouseReleased(final MouseEvent mouseEvent) {
        dragBBpt = -1;

        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        final int xS = getScaledX(mouseEvent.getX()); // zoomed x. Used as cursor
        final int yS = getScaledY(mouseEvent.getY()); // zoomed y. Used as cursor

        if ( (xS < 0) || (xS >= imageDim.width) || (yS < 0) || (yS >= imageDim.height)) {
            return;
        }

        if ( (cursorMode == ViewJComponentBase.PAINT_VOI) || (cursorMode == ViewJComponentBase.ERASER_PAINT)) {
            imageActive.notifyImageDisplayListeners();
        }
        else if (cursorMode == ViewJComponentBase.PAINT_CAN) {
            final Vector3f patientMousePoint = new Vector3f();
            super.ScreenToLocal(new Vector3f(mouseEvent.getX(), mouseEvent.getY(), slice), patientMousePoint);

            final Vector3f volumeMousePoint = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, volumeMousePoint, imageActive, orientation);

            xPG = (short) volumeMousePoint.X;
            yPG = (short) volumeMousePoint.Y;
            zPG = (short) volumeMousePoint.Z;

            if (imageActive.isColorImage()) {
                final int index = 4 * (yS + imageActive.getExtents()[0] + xS);
                seedValR = imageBufferActive[index + 1];
                seedValG = imageBufferActive[index + 2];
                seedValB = imageBufferActive[index + 3];
                regionGrow(xPG, yPG, zPG, seedValR, seedValG, seedValB, null, true);
            } else {
                seedVal = imageBufferActive[ (yS * imageDim.width) + xS];
                regionGrow(xPG, yPG, zPG, seedVal, null, true);
            }

            triImageFrame.updatePaint(paintBitmap);
        } // end of else if (mode == PAINT_CAN)
        /*
        else if (cursorMode == POINT_VOI) {

            if ( (mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK) != 0) {
                final Vector3f patientMousePoint = new Vector3f();
                super.ScreenToLocal(new Vector3f(mouseEvent.getX(), mouseEvent.getY(), slice), patientMousePoint);

                final Vector3f volumeMousePoint = new Vector3f();
                MipavCoordinateSystems.patientToFile(patientMousePoint, volumeMousePoint, imageActive, orientation);

                xOrg = (int) volumeMousePoint.X;
                yOrg = (int) volumeMousePoint.Y;
                zOrg = (int) volumeMousePoint.Z;

                imageActive = imageA;

                if ( (this == triImageFrame.getTriImage(ViewJFrameTriImage.AXIAL_AB))
                        || (this == triImageFrame.getTriImage(ViewJFrameTriImage.SAGITTAL_AB))
                        || (this == triImageFrame.getTriImage(ViewJFrameTriImage.CORONAL_AB))) {

                    if (triImageFrame.getSelectedImage() == ViewJComponentBase.IMAGE_B) {
                        imageActive = imageB;
                    }
                }

                final float[] x = new float[1];
                final float[] y = new float[1];
                final float[] z = new float[1];

                // voiID is a protected variable set to -1 by NEW_VOI in ViewJComponentEditImage
                VOI newPointVOI;

                try {
                    System.err.println( "    TRIImage.mouseReleased" );
                    voiHandler.setVOI_ID(imageActive.getVOIs().size());
                    newPointVOI = new VOI((short) imageActive.getVOIs().size(), "point3D_"
                            + (voiHandler.getVOI_ID() + 1), imageActive.getExtents()[2], VOI.POINT, -1.0f);
                    x[0] = xOrg;
                    y[0] = yOrg;
                    z[0] = zOrg;
                    newPointVOI.importCurve(x, y, z, zOrg);
                } catch (final OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ViewJComponentTriImage.mouseReleased");
                    setCursorMode(ViewJComponentBase.DEFAULT);

                    return;
                }

                // lastPointVOI is now handled by VOIHandler
                // voiHandler.setLastPointVOI_ID(voiID);

                imageActive.registerVOI(newPointVOI);

                newPointVOI.setActive(true);
                ((VOIPoint) (newPointVOI.getCurves()[(int) z[0]].elementAt(0))).setActive(true);

                //
                // Used to be frame.updateImages() but it was changed to updateImageSubset() because we don't want all
                // the images to be updated. The reason is to facilitate the placement of VOIs during image
                // registration.
                //
                triImageFrame.updateImageSubset(this);
                System.err.println( "     TRIImage.mouseReleased" );
                triImageFrame.updatevoiID(voiHandler.getVOI_ID());

                if (mouseEvent.isShiftDown() != true) {
                    triImageFrame.setTraverseButton();
                    triImageFrame.setDefault();
                }

            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)
        } // end of else if (mode == POINT_VOI)
*/
        return;
    }

    /**
     * Mouse wheel movement handler, used to change the displayed slice in the component.
     * 
     * @param event the mouse wheel rotation event
     */
    public void mouseWheelMoved(final MouseWheelEvent event) {
        updateSlice(slice - event.getWheelRotation());
    }

    /**
     * Returns true if mouse point is close to bounds point.
     * 
     * @param mouseX the x component of the mouse click within the component
     * @param mouseY the y component of the mouse click within the component
     * @param boundsX the x component of the bounds point to check against
     * @param boundsY the y component of the bounds point to check against
     * 
     * @return true if mouse point is close to bounds point
     */
    public boolean nearBoundsPoint(int mouseX, int mouseY, final int boundsX, final int boundsY) {
        double dist;

        final Vector3f patientMousePoint = new Vector3f();
        super.ScreenToLocal(new Vector3f(mouseX, mouseY, slice), patientMousePoint);

        final Vector3f volumeMousePoint = new Vector3f();
        MipavCoordinateSystems.patientToFile(patientMousePoint, volumeMousePoint, imageActive, orientation);
        mouseX = (int) volumeMousePoint.X;
        mouseY = (int) volumeMousePoint.Y;

        dist = Math.sqrt( ( (mouseX - boundsX) * (mouseX - boundsX)) + ( (mouseY - boundsY) * (mouseY - boundsY)));

        if (dist < 3.0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Paints the image, optionally draws the axis, the center of rotation point, the talairach grid, etc., and calls
     * drawSelf for all VOIs.
     * 
     * @param graphics graphics to draw with
     */
    public void paintComponent(final Graphics graphics) {

        if (graphics == null) {
            Preferences.debug("Cannot paint image because graphics context is null\n", Preferences.DEBUG_MINOR);

            return;
        }

        if (img == null) {
            importImage(cleanImageBufferA);
            importImageB(cleanImageBufferB);
        }

        Image offscreenImage = null;
        Graphics2D offscreenGraphics2d = null;

        if (isDisplayable()) {
            final int zoomedWidth = Math.round(zoomX * img.getWidth(this) * resolutionX);
            final int zoomedHeight = Math.round(zoomY * img.getHeight(this) * resolutionY);

            offscreenImage = createImage(zoomedWidth, zoomedHeight);
            offscreenGraphics2d = (Graphics2D) offscreenImage.getGraphics();
        } else {
            return;
        }

        super.paintComponent(offscreenGraphics2d);

        if (showBoundingRect) {
            drawBoundingRect(offscreenGraphics2d);
        }

        if (showAxes) {
            drawAxes(offscreenGraphics2d);
        }

        if (orientation == FileInfoBase.AXIAL) {

            if (showTalairachGrid) {
                drawTalairachGrid_AXIAL(offscreenGraphics2d);

                computeTalairachVoxelPosition((int) crosshairPt.X, (int) crosshairPt.Y);
            }

        } // end of if (orientation == AXIAL)
        else if (orientation == FileInfoBase.CORONAL) {

            if (showTalairachGrid) {
                drawTalairachGrid_CORONAL(offscreenGraphics2d);

                computeTalairachVoxelPosition((int) crosshairPt.X, (int) crosshairPt.Y);
            }

        } // end of else if (orientation == CORONAL)
        else if (orientation == FileInfoBase.SAGITTAL) {

            if (showTalairachGrid) {
                drawTalairachGrid_SAGITTAL(offscreenGraphics2d);

                computeTalairachVoxelPosition((int) crosshairPt.X, (int) crosshairPt.Y);
            } // if (showTalairach)
        } // end of else if (orientation == SAGITTAL)

        drawCrosshairs(offscreenGraphics2d);

        if (doCenter) {
            drawCenterMark(offscreenGraphics2d);
        }

        //drawTriPlanarVOIs(offscreenGraphics2d);

        offscreenGraphics2d.dispose();

        graphics.drawImage(offscreenImage, 0, 0, null);
    }

    /**
     * Remove a talairach / ACPC reference point.
     * 
     * @param label the label of the reference point to remove
     * 
     * @return true if the requested reference point was found and removed
     */
    public boolean removeReference(final String label) {
        String label2;
        boolean found = false;
        int i, j;
        final ViewVOIVector VOIs = imageActive.getVOIs();
        final int nVOI = VOIs.size();

        if (nVOI == 0) {
            return false; // no point present
        }

        for (i = 0; (i < nVOI) && ( !found); i++) {
            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                Vector3f[] voiPoints = VOIs.VOIAt(i).exportAllPoints();

                for (j = 0; (j < voiPoints.length) && (!found); j++) {
                    label2 = ((VOIPoint) (VOIs.VOIAt(i).getCurves().elementAt(j))).getLabel();

                    if (label.equals(label2)) {
                        VOIs.VOIAt(i).getCurves().removeElementAt(j);
                        found = true;
                    }
                }
            }
        }

        frame.updateImages();

        return found;
    }

    /**
     * setCenter, sets the crosshairPt and the local copies of the volumePosition (in FileCoordinates and
     * PatientCoordinates).
     * 
     * @param i FileCoordinates
     * @param j FileCoordinates
     * @param k FileCoordinates
     */
    public void setCenter(final int i, final int j, final int k) {
        m_kVolumePoint.X = i;
        m_kVolumePoint.Y = j;
        m_kVolumePoint.Z = k;

        m_kPatientSlice.setCenter(i, j, k);

        final Vector3f kLocalPoint = new Vector3f();
        MipavCoordinateSystems.fileToPatient(m_kVolumePoint, kLocalPoint, imageActive, orientation);
        slice = (int) kLocalPoint.Z;

        super.LocalToScreen(kLocalPoint, crosshairPt);
    }

    /**
     * Sets the crop volume.
     * 
     * @param lower the lower corner of the crop volume in FileCoordinates
     * @param upper the upper corner of the crop volume in FileCoordinates
     */
    public void setCrop(final Vector3f lower, final Vector3f upper) {

        /* convert to Patient coordinates, store in m_kLocalCropLower */
        MipavCoordinateSystems.fileToPatient(lower, m_kLocalCropLower, imageActive, orientation);

        /* convert to Screen coordinates, store in cornerCropPt */
        super.LocalToScreen(m_kLocalCropLower, cornerCropPt[0]);

        /* convert to Patient coordinates, store in m_kLocalCropUpper */
        MipavCoordinateSystems.fileToPatient(upper, m_kLocalCropUpper, imageActive, orientation);

        /* convert to Screen coordinates, store in cornerCropPt */
        super.LocalToScreen(m_kLocalCropUpper, cornerCropPt[1]);

        /* Create the four coners of the crop bounding box: */
        cornerToCrop();
    }

    /**
     * Sets the pixel gap to be used by the crosshair (in one direction away from the center)
     * 
     * @param newGap the new gap value
     */
    public void setCrosshairPixelGap(final int newGap) {
        this.crosshairPixelGap = newGap;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param newMode DOCUMENT ME!
     */
    public void setCursorMode(final int newMode) {
        super.setCursorMode(newMode);
        if (newMode == ViewJComponentBase.PAINT_VOI) {
            setCursor(MipavUtil.blankCursor);
        } else {
            setCursor(crosshairCursor);
        }
    }

    /**
     * Sets whether the center of rotation point should be displayed and the user should be allowed to change it.
     * 
     * @param doCenter whether the center of rotation point should be displayed and the user should be allowed to change
     *            it
     */
    public void setDoCenter(final boolean doCenter) {
        this.doCenter = doCenter;
    }

    /**
     * Set a talairach / ACPC reference point.
     * 
     * @param pointType the reference point type
     * @param pt the point position (in image volume space)
     */
    public void setReferenceXY(final int pointType, final Vector3f pt) {
        final float[] x = new float[1];
        final float[] y = new float[1];
        final float[] z = new float[1];
        VOI newPointVOI;

        // Vector3f localPoint = new Vector3f();
        // MipavCoordinateSystems.FileToPatient( pt, localPoint, imageActive, orientation );
        // x[0] = localPoint.X;
        // y[0] = localPoint.Y;
        // z[0] = localPoint.Z;
        x[0] = pt.X;
        y[0] = pt.Y;
        z[0] = pt.Z;

        VOIBase newCurve = null;
        try {
            //voiHandler.setVOI_ID(imageActive.getVOIs().size());
            newPointVOI = new VOI((short) imageActive.getVOIs().size(), "point3D_" + (imageActive.getVOIs().size() + 1),
                    VOI.POINT, -1.0f);
            newCurve = newPointVOI.importCurve(x, y, z);
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentTriImage.setReferenceXY");
            setCursorMode(ViewJComponentBase.DEFAULT);
            return;
        }
        if ( newCurve == null )
        {
            return;
        }

        // lastPointVOI is now handled by VOIHandler
        // voiHandler.setLastPointVOI_ID(voiID);

        imageActive.registerVOI(newPointVOI);
        //triImageFrame.updatevoiID(voiHandler.getVOI_ID());
        newCurve.setFixed(true);

        switch (pointType) {

            case SUPERIOR_EDGE:
                newCurve.setLabel("ACS");
                break;

            case POSTERIOR_MARGIN:
                newCurve.setLabel("ACP");
                break;

            case INFERIOR_EDGE:
                newCurve.setLabel("PC");
                break;

            case FIRST_PT:
                newCurve.setLabel("MS1");
                break;

            case ANOTHER_PT:
                newCurve.setLabel("MS2");
                break;

            case ANTERIOR_PT:
                newCurve.setLabel("A");
                break;

            case POSTERIOR_PT:
                newCurve.setLabel("P");
                break;

            case SUPERIOR_PT:
                newCurve.setLabel("S");
                break;

            case INFERIOR_PT:
                newCurve.setLabel("I");
                break;

            case LEFT_PT:
                newCurve.setLabel("L");
                break;

            case RIGHT_PT:
                newCurve.setLabel("R");
                break;
        }

        frame.updateImages();
    }

    /**
     * Sets whether the bounding rectangle should be drawn.
     * 
     * @param flag whether the bounding rectangle should be drawn
     */
    public void setShowBoundingRect(final boolean flag) {
        showBoundingRect = flag;
    }

    /**
     * Change the color for the x crosshair.
     * 
     * @param c the new crosshair color
     */
    public void setXSliceHairColor(final Color c) {
        xColor[orientation] = c;
    }

    /**
     * Change the color for the y crosshair.
     * 
     * @param c the new crosshair color
     */
    public void setYSliceHairColor(final Color c) {
        yColor[orientation] = c;
    }

    /**
     * sets the screen scale variable when setZoom is called:
     * 
     * @param zX new zoom factor in X
     * @param zY new zoom factor in Y
     */
    public void setZoom(final float zX, final float zY) {
        final float xRatio = zX / zoomX;
        final float yRatio = zY / zoomY;
        super.setZoom(zX, zY);

        for (int i = 0; i < cropPoints.length; i++) {

            if (cropPoints[i] != null) {
                cropPoints[i].X *= xRatio;
                cropPoints[i].Y *= yRatio;
            }
        }

        for (int i = 0; i < cornerCropPt.length; i++) {

            if (cornerCropPt[i] != null) {
                cornerCropPt[i].X *= xRatio;
                cornerCropPt[i].Y *= yRatio;
            }
        }
    }

    /**
     * Change the color for the z crosshair.
     * 
     * @param c the new crosshair color
     */
    public void setZSliceHairColor(final Color c) {
        zColor[orientation] = c;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param tSlice DOCUMENT ME!
     * @param _LUTa DOCUMENT ME!
     * @param _LUTb DOCUMENT ME!
     * @param forceShow DOCUMENT ME!
     * @param _interpMode DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean show(final int tSlice, final ModelLUT _LUTa, final ModelLUT _LUTb, final boolean forceShow,
            final int _interpMode) {
        return showUsingOrientation(tSlice, _LUTa, _LUTb, forceShow, _interpMode);
    }

    /**
     * Sets whether or not lines dividing the Talairach regions are shown.
     * 
     * @param doShowTalairachGrid whether to show the talairach region grid
     */
    public void showTalairachGrid(final boolean doShowTalairachGrid) {
        this.showTalairachGrid = doShowTalairachGrid;
    }

    /**
     * Sets whether the talairach grid markers will be displayed.
     * 
     * @param doShowTalairachGridmarkers boolean show talairach grid markers
     */
    public void showTalairachGridmarkers(final boolean doShowTalairachGridmarkers) {
        this.showTalairachGridmarkers = doShowTalairachGridmarkers;
    }

    /**
     * Calls <code>paintComponent</code> - reduces flicker.
     * 
     * @param g graphics to draw with
     */
    public void update(final Graphics g) {
        this.paintComponent(g);
    }

    /**
     * Changes the crosshair coordinate (screen coordinate) that this component should display.
     * 
     * @param x which x screen coordinate to show
     * @param y which y screen coordinate to show
     * 
     * @see #xSlice
     * @see #ySlice
     * @see #zSlice
     */
    public void updateCrosshairPosition(final int x, final int y) {
        crosshairPt.X = x;
        crosshairPt.Y = y;
    }

    /**
     * Clean up memory used by the component.
     * 
     * @throws Throwable if there is a problem encountered during memory clean-up
     * 
     * @see #disposeLocal(boolean)
     */
    protected void finalize() throws Throwable {
        disposeLocal(true);
        super.finalize();
    }

    /**
     * This method creates a buffer that will be used to make an image of the painted area on-screen.
     * 
     * @param paintImageBuffer int[] the buffer to fill that will make the paint image
     * @param paintBitmap the bit map representing the painted pixels
     * @param slice the current slice to paint if this is a 3D image
     * @param frame the ViewJFrameBase containing the painted component.
     * @param b2D when true this is a 2D image component, when false it is greater than 2D
     */
    protected void makePaintImage(final int[] paintImageBuffer, final BitSet paintBitmap, final int slice,
            final ViewJFrameBase frame, final boolean b2D) {
        
        if ( paintBitmap.cardinality() <= 0 )
        {
            // Nothing to paint...
            return;
        }
        
        // get the color of the paint the user has selected
        int color = getSelectedPaintColor(frame);
        final float opacity = frame.getControls().getTools().getOpacity();
        int opacityInt = (int) (opacity * 255);
        opacityInt = opacityInt << 24;

        if (slice >= 0) {
            final Vector3d paintPoint = new Vector3d();
            int iIndex;
            final int[] iterFactors = imageActive.getVolumeIterationFactors();

            for (int iX = 0; iX < localImageExtents[0]; iX++) {

                for (int iY = 0; iY < localImageExtents[1]; iY++) {
                    MipavCoordinateSystems.patientToFile(new Vector3d(iX, iY, slice), paintPoint, imageActive,
                            orientation);

                    iIndex = (int) ( (iterFactors[0] * paintPoint.X) + (iterFactors[1] * paintPoint.Y) + (iterFactors[2] * paintPoint.Z));

                    if (paintBitmap.get(iIndex)) {
                        color = color & 0x00ffffff;
                        paintImageBuffer[iX + (iY * localImageExtents[0])] = color | opacityInt;
                    }
                }
            }
        }
    }

    /**
     * Updates the image volume's paint bitmap based on the current position of the mouse in the component, called after
     * a mouse action indicates the user wants part of the image either painted or erased.
     * 
     * @param paintPixels if true, the pixels under the paint brush should be painted. if false, they are cleared of any
     *            paint
     * @param x the x position of the mouse, adjusted for zoom and resolutions
     * @param y the y position of the mouse, adjusted for zoom and resolutions
     */
    protected void updatePaintBitmap(final boolean paintPixels, final int x, final int y) {
        final int brushSize = getBrushSize();
        final int hBrushSize = getHBrushSize();
        final int jMin = Math.max(y - hBrushSize, 0);
        final int jMax = Math.min(y - hBrushSize + brushSize - 1, imageDim.height - 1);
        final int iMin = Math.max(x - hBrushSize, 0);
        final int iMax = Math.min(x - hBrushSize + brushSize - 1, imageDim.width - 1);
        int i, j, index;
        int[] iterFactors = imageActive.getVolumeIterationFactors();

        Vector3d paintPoint = new Vector3d();
        Vector3d patientPaintPoint = new Vector3d();
        final int xDim = imageActive.getExtents()[0];
        final int yDim = imageActive.getExtents()[1];
        final int zDim = imageActive.getExtents()[2];

        if (paintBrush != null) {

            final int brushXDim = paintBrushDim.width;
            final int brushYDim = paintBrushDim.height;
            int heightCursor, widthCursor; // height and width of the paint cursor icon
            double height, width; // scaled height and width in the paintBitmap

            final double minResol = Math.min(resolutionX, resolutionY);

            for (height = 0, heightCursor = 0; (heightCursor < brushYDim) && ((y + height) <= (imageDim.height-1));
                    heightCursor++, height += minResol
                    / resolutionY) {

                for (width = 0, widthCursor = 0; (widthCursor < brushXDim) && ((x + width) <= (imageDim.width - 1));
                        widthCursor++, width += minResol
                        / resolutionX) {
                    final int idx = ( (heightCursor * brushXDim) + widthCursor);
                    if (paintBrush.get(idx)) {

                        patientPaintPoint.X =  (x + width);
                        patientPaintPoint.Y =  (y + height);
                        patientPaintPoint.Z = slice;
                        MipavCoordinateSystems.patientToFile(patientPaintPoint, paintPoint, imageActive, orientation);
                        if ( ( (paintPoint.X <= (xDim - 1)) && (paintPoint.X >= 0)) && (paintPoint.Y <= (yDim - 1))
                                && (paintPoint.Y >= 0) && (paintPoint.Z <= (zDim - 1)) && (paintPoint.Z >= 0)) {

                            index = (int) ( (iterFactors[0] * paintPoint.X) + (iterFactors[1] * paintPoint.Y) + (iterFactors[2] * paintPoint.Z));

                            if (paintPixels == true) {
                                paintBitmap.set(index);
                            } else {
                                paintBitmap.clear(index);
                            }
                        }
                    }

                }
            }

        } else {

            // Vector2f scalePoint = new Vector2f( 1, 1 );
            for (j = jMin; j <= jMax; j++) {

                for (i = iMin; i <= iMax; i++) {
                    patientPaintPoint.X = i;
                    patientPaintPoint.Y = j;
                    patientPaintPoint.Z = slice;
                    MipavCoordinateSystems.patientToFile(patientPaintPoint, paintPoint, imageActive, orientation);

                    index = (int) ( (iterFactors[0] * paintPoint.X) + (iterFactors[1] * paintPoint.Y) + (iterFactors[2] * paintPoint.Z));

                    if (paintPixels) {
                        paintBitmap.set(index);
                    } else {
                        paintBitmap.clear(index);
                    }
                }
            }
        }

        triImageFrame.updatePaint(paintBitmap);
        iterFactors = null;
        paintPoint = null;
        patientPaintPoint = null;
        // System.gc(); - make painting too slow
    }

    /**
     * Creates the four corners of the crop bounding box, based on the two corner crop points:
     */
    private void cornerToCrop() {
        cropPoints[0] = new Vector2f(cornerCropPt[0].X, cornerCropPt[0].Y);
        cropPoints[1] = new Vector2f(cornerCropPt[0].X, cornerCropPt[1].Y);
        cropPoints[2] = new Vector2f(cornerCropPt[1].X, cornerCropPt[1].Y);
        cropPoints[3] = new Vector2f(cornerCropPt[1].X, cornerCropPt[0].Y);
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * 
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawAxes(final Graphics2D offscreenGraphics2d) {
        String labelX = new String(axisLabels[orientation][0]);

        if ( !imageActive.getRadiologicalView() && (orientation != FileInfoBase.SAGITTAL)) {

            if ( !hasOrientation) {
                labelX = new String("-X");
            } else {
                labelX = new String("R");
            }
        }

        if ( !hasOrientation || (orientation == FileInfoBase.AXIAL)) {
            offscreenGraphics2d.setColor(xColor[orientation]);
            offscreenGraphics2d.drawString(labelX, 45, 15);
            offscreenGraphics2d.drawLine(10, 9, 39, 9);
            offscreenGraphics2d.drawLine(10, 10, 40, 10);
            offscreenGraphics2d.drawLine(10, 11, 39, 11);
            offscreenGraphics2d.drawLine(35, 5, 40, 10);
            offscreenGraphics2d.drawLine(35, 15, 40, 10);

            offscreenGraphics2d.setColor(yColor[orientation]);
            offscreenGraphics2d.drawString(axisLabels[orientation][1], 10, 55);
            offscreenGraphics2d.drawLine(9, 10, 9, 39);
            offscreenGraphics2d.drawLine(10, 10, 10, 40);
            offscreenGraphics2d.drawLine(11, 10, 11, 39);
            offscreenGraphics2d.drawLine(5, 35, 10, 40);
            offscreenGraphics2d.drawLine(15, 35, 10, 40);
        } else {
            offscreenGraphics2d.setColor(xColor[orientation]);

            final int componentHeight = getSize().height;
            offscreenGraphics2d.drawString(labelX, 45, componentHeight - 6);
            offscreenGraphics2d.drawLine(10, componentHeight - 12, 39, componentHeight - 12);
            offscreenGraphics2d.drawLine(10, componentHeight - 11, 40, componentHeight - 11);
            offscreenGraphics2d.drawLine(10, componentHeight - 10, 39, componentHeight - 10);
            offscreenGraphics2d.drawLine(35, componentHeight - 16, 40, componentHeight - 11);
            offscreenGraphics2d.drawLine(35, componentHeight - 6, 40, componentHeight - 11);

            offscreenGraphics2d.setColor(yColor[orientation]);
            offscreenGraphics2d.drawString(axisLabels[orientation][1], 10, componentHeight - 46);
            offscreenGraphics2d.drawLine(9, componentHeight - 11, 9, componentHeight - 40);
            offscreenGraphics2d.drawLine(10, componentHeight - 11, 10, componentHeight - 41);
            offscreenGraphics2d.drawLine(11, componentHeight - 11, 11, componentHeight - 40);
            offscreenGraphics2d.drawLine(5, componentHeight - 36, 10, componentHeight - 41);
            offscreenGraphics2d.drawLine(15, componentHeight - 36, 10, componentHeight - 41);
        }
    }

    /**
     * Draws the cropping rectangle.
     * 
     * @param graphics the graphics object to draw with
     */
    private void drawBoundingRect(final Graphics graphics) {
        graphics.setColor(cropColor[orientation]);

        for (int i = 0; i < 4; i++) {
            graphics.drawLine((int) (cropPoints[i].X), (int) (cropPoints[i].Y), (int) (cropPoints[ (i + 1) % 4].X),
                    (int) (cropPoints[ (i + 1) % 4].Y));
            graphics.fillRect((int) (cropPoints[i].X) - 2, (int) (cropPoints[i].Y) - 2, 4, 4);
        }
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * 
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawCenterMark(final Graphics2D offscreenGraphics2d) {
        offscreenGraphics2d.setColor(Color.yellow);

        final int centerXD = (int) (crosshairPt.X + xCursorAdjustment * zoomX * resolutionX);
        final int centerYD = (int) (crosshairPt.Y + yCursorAdjustment * zoomY * resolutionY);
        offscreenGraphics2d.drawLine(centerXD, centerYD - 5, centerXD, centerYD + 5);
        offscreenGraphics2d.drawLine(centerXD - 5, centerYD, centerXD + 5, centerYD);
        offscreenGraphics2d.setColor(Color.black);
        offscreenGraphics2d.drawLine(centerXD - 1, centerYD - 5, centerXD - 1, centerYD + 5);
        offscreenGraphics2d.drawLine(centerXD + 1, centerYD - 5, centerXD + 1, centerYD + 5);
        offscreenGraphics2d.drawLine(centerXD - 5, centerYD - 1, centerXD + 5, centerYD - 1);
        offscreenGraphics2d.drawLine(centerXD - 5, centerYD + 1, centerXD + 5, centerYD + 1);
    }

    /**
     * Convenience method called by drawCrosshairsXY(). Inserted here for simplicity's sake.
     * 
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawCrosshairLines(final Graphics2D offscreenGraphics2d) {
    	int adjustedX = (int) (crosshairPt.X + xCursorAdjustment * zoomX * resolutionX);
    	int adjustedY = (int) (crosshairPt.Y + yCursorAdjustment * zoomY * resolutionY);

        // This snaps the crosshair to the voxel boundary
        offscreenGraphics2d.setColor(xColor[orientation]);
        offscreenGraphics2d.drawLine(adjustedX, 0, adjustedX, adjustedY
                - crosshairPixelGap);
        offscreenGraphics2d.drawLine(adjustedX, getSize().height, adjustedX, adjustedY
                + crosshairPixelGap);

        offscreenGraphics2d.setColor(yColor[orientation]);
        offscreenGraphics2d.drawLine(0, adjustedY, adjustedX - crosshairPixelGap,
                adjustedY);
        offscreenGraphics2d.drawLine(getSize().width, adjustedY, adjustedX + crosshairPixelGap,
                adjustedY);
    }

    /**
     * Draws the crosshairs for this oriented tri-planar component.
     * 
     * @param offscreenGraphics2d the graphics context to draw with
     */
    private void drawCrosshairs(final Graphics2D offscreenGraphics2d) {
        drawCrosshairStubs(offscreenGraphics2d);

        if (showCrosshairs) {
            drawCrosshairLines(offscreenGraphics2d);
        }
    }

    /**
     * Convenience method called by drawCrosshairs. Inserted here for simplicity's sake.
     * 
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawCrosshairStubs(final Graphics2D offscreenGraphics2d) {
    	int adjustedX = (int) (crosshairPt.X + xCursorAdjustment * zoomX * resolutionX);
    	int adjustedY = (int) (crosshairPt.Y + yCursorAdjustment * zoomY * resolutionY);

        // border
        offscreenGraphics2d.setColor(zColor[orientation]);
        offscreenGraphics2d.drawRect(0, 0, getSize().width - 1, getSize().height - 1);

        // x stubs
        offscreenGraphics2d.setColor(xColor[orientation]);
        offscreenGraphics2d.drawLine(adjustedX, 0, adjustedX, 10);
        offscreenGraphics2d.drawLine(adjustedX, getSize().height, adjustedX, getSize().height - 10);

        // y stubs
        offscreenGraphics2d.setColor(yColor[orientation]);
        offscreenGraphics2d.drawLine(0, adjustedY, 10, adjustedY);
        offscreenGraphics2d.drawLine(getSize().width, adjustedY, getSize().width - 10, adjustedY);
    }


    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * 
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawTalairachGrid_AXIAL(final Graphics2D offscreenGraphics2d) {

        // we should check if we're inside the box first

        final Stroke defaultStroke = offscreenGraphics2d.getStroke();
        final Stroke dashedStroke = getDashedStroke();

        // slice separating right from left is always put ViewJFrameTriImage.ATLAS_BBOX_LAT. from
        // x = 0 at the right of the image
        final int xSliceT = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX);

        // new coordinates: middle -/+ Talairach dimensions
        final int x04 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX - ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 4 / 4);
        final int x14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX - ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 3 / 4);
        final int x24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX - ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 2 / 4);
        final int x34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX - ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 1 / 4);
        final int x54 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX + ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 1 / 4);
        final int x64 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX + ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 2 / 4);
        final int x74 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX + ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 3 / 4);
        final int x84 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX + ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 4 / 4);

        // slice separating anterior from median is always put ViewJFrameTriImage.ATLAS_BBOX_ANT. from y = 0 at
        // the anterior of image slice separating median from posterior is always put ViewJFrameTriImage.ATLAS_BBOX_ANT
        // + ViewJFrameTriImage.ATLAS_AC_TO_PC from the anterior of the image
        final int ySliceT = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY);
        final int ySliceT2 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY);

        // new coordinates: middle -/+ Talairach dimensions
        final int yA04 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_ANT
                * zoomY * 4 / 4);
        final int yA14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_ANT
                * zoomY * 3 / 4);
        final int yA24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_ANT
                * zoomY * 2 / 4);
        final int yA34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_ANT
                * zoomY * 1 / 4);

        final int yP14 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY + ViewJFrameTriImage.ATLAS_PC_TO_POS * zoomY * 1 / 4);
        final int yP24 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY + ViewJFrameTriImage.ATLAS_PC_TO_POS * zoomY * 2 / 4);
        final int yP34 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY + ViewJFrameTriImage.ATLAS_PC_TO_POS * zoomY * 3 / 4);
        final int yP44 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY + ViewJFrameTriImage.ATLAS_PC_TO_POS * zoomY * 4 / 4);

        final int yAP13 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_PC
                * zoomY * 1 / 3);
        final int yAP23 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_PC
                * zoomY * 2 / 3);

        // major axes
        offscreenGraphics2d.setColor(talairachMajorLineColor);
        offscreenGraphics2d.drawLine(xSliceT, yA04, xSliceT, yP44);
        offscreenGraphics2d.drawLine(x04, ySliceT, x84, ySliceT);
        offscreenGraphics2d.drawLine(x04, ySliceT2, x84, ySliceT2);

        // graduation lines
        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        offscreenGraphics2d.drawLine(x04, yAP13, x84, yAP13);
        offscreenGraphics2d.drawLine(x04, yAP23, x84, yAP23);

        offscreenGraphics2d.drawLine(x04, yA04, x04, yP44);
        offscreenGraphics2d.drawLine(x14, yA04, x14, yP44);
        offscreenGraphics2d.drawLine(x24, yA04, x24, yP44);
        offscreenGraphics2d.drawLine(x34, yA04, x34, yP44);
        offscreenGraphics2d.drawLine(x54, yA04, x54, yP44);
        offscreenGraphics2d.drawLine(x64, yA04, x64, yP44);
        offscreenGraphics2d.drawLine(x74, yA04, x74, yP44);
        offscreenGraphics2d.drawLine(x84, yA04, x84, yP44);

        verticalTalGridPts = new int[] {x04, x14, x24, x34, xSliceT, x54, x64, x74, x84};
        horizontalTalGridPts = new int[] {yA04, yA14, yA24, yA34, ySliceT, ySliceT2, yP14, yP24, yP34, yP44};

        if (showTalairachGridmarkers) {
            offscreenGraphics2d.setColor(talairachMajorLineColor);
            offscreenGraphics2d.setStroke(defaultStroke);
            drawStringBW("d", offscreenGraphics2d, x14 / 2, getSize().height - 2);
            drawStringBW("c", offscreenGraphics2d, ( (x24 - x14) / 2) + x14, getSize().height - 2);
            drawStringBW("b", offscreenGraphics2d, ( (x34 - x24) / 2) + x24, getSize().height - 2);
            drawStringBW("a", offscreenGraphics2d, ( (xSliceT - x34) / 2) + x34, getSize().height - 2);

            drawStringBW("a", offscreenGraphics2d, ( (x54 - xSliceT) / 2) + xSliceT, getSize().height - 2);
            drawStringBW("b", offscreenGraphics2d, ( (x64 - x54) / 2) + x54, getSize().height - 2);
            drawStringBW("c", offscreenGraphics2d, ( (x74 - x64) / 2) + x64, getSize().height - 2);
            drawStringBW("d", offscreenGraphics2d, ( (getSize().width - x74) / 2) + x74, getSize().height - 2);
        }

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        offscreenGraphics2d.drawLine(x04, yA04, x84, yA04);
        offscreenGraphics2d.drawLine(x04, yA14, x84, yA14);
        offscreenGraphics2d.drawLine(x04, yA24, x84, yA24);
        offscreenGraphics2d.drawLine(x04, yA34, x84, yA34);
        offscreenGraphics2d.drawLine(x04, yP14, x84, yP14);
        offscreenGraphics2d.drawLine(x04, yP24, x84, yP24);
        offscreenGraphics2d.drawLine(x04, yP34, x84, yP34);
        offscreenGraphics2d.drawLine(x04, yP44, x84, yP44);

        offscreenGraphics2d.setStroke(defaultStroke);

        if (showTalairachGridmarkers) {
            offscreenGraphics2d.setColor(talairachMajorLineColor);
            drawStringBW("A", offscreenGraphics2d, getSize().width - 9, ( (yA14 - yA04) / 2) + yA04);
            drawStringBW("B", offscreenGraphics2d, getSize().width - 9, ( (yA24 - yA14) / 2) + yA14);
            drawStringBW("C", offscreenGraphics2d, getSize().width - 9, ( (yA34 - yA24) / 2) + yA24);
            drawStringBW("D", offscreenGraphics2d, getSize().width - 9, ( (ySliceT - yA34) / 2) + yA34);
            drawStringBW("E", offscreenGraphics2d, getSize().width - 9, ( (ySliceT2 - ySliceT) / 2) + ySliceT);
            drawStringBW("F", offscreenGraphics2d, getSize().width - 9, ( (yP14 - ySliceT2) / 2) + ySliceT2);
            drawStringBW("G", offscreenGraphics2d, getSize().width - 9, ( (yP24 - yP14) / 2) + yP14);
            drawStringBW("H", offscreenGraphics2d, getSize().width - 9, ( (yP34 - yP24) / 2) + yP24);
            drawStringBW("I", offscreenGraphics2d, getSize().width - 9, ( (yP44 - yP34) / 2) + yP34);
        }
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * 
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawTalairachGrid_CORONAL(final Graphics2D offscreenGraphics2d) {

        // we should check if we're inside the box first

        final Stroke defaultStroke = offscreenGraphics2d.getStroke();
        final Stroke dashedStroke = getDashedStroke();

        // slice separating right from left is always put ViewJFrameTriImage.ATLAS_AC_TO_LAT or 68 mm. from
        // x = 0 at the right of the image
        final int xSliceT = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX);

        // new coordinates: middle -/+ Talairach dimensions
        final int x04 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX - ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 4 / 4);
        final int x14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX - ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 3 / 4);
        final int x24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX - ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 2 / 4);
        final int x34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX - ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 1 / 4);
        final int x54 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX + ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 1 / 4);
        final int x64 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX + ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 2 / 4);
        final int x74 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX + ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 3 / 4);
        final int x84 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_LAT * zoomX + ViewJFrameTriImage.ATLAS_AC_TO_LAT
                * zoomX * 4 / 4);

        // slice separating inferior from superior is usually put ViewJFrameTriImage.ATLAS_AC_TO_INF or 42 mm. from z
        // = 0 at the inferior of the image. In case the z dimension = (ViewJFrameTriImage.ATLAS_AC_TO_INF +
        // ViewJFrameTriImage.ATLAS_AC_TO_SUP + 1)/resZ Sometimes it is put ViewJFrameTriImage.ATLAS_AC_TO_INF or 42 mm
        // from z = 0 at the inferior of the image. In this case the z dimension = (ViewJFrameTriImage.ATLAS_AC_TO_INF +
        // ViewJFrameTriImage.ATLAS_AC_TO_SUP + 1)/resZ
        final int zSliceT = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY);

        final int zS08 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 8 / 8);
        final int zS18 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 7 / 8);
        final int zS28 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 6 / 8);
        final int zS38 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 5 / 8);
        final int zS48 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 4 / 8);
        final int zS58 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 3 / 8);
        final int zS68 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 2 / 8);
        final int zS78 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 1 / 8);

        final int zI14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 1 / 4);
        final int zI24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 2 / 4);
        final int zI34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 3 / 4);
        final int zI44 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 4 / 4);
        final int zI54 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 5 / 4);

        offscreenGraphics2d.setColor(talairachMajorLineColor);
        offscreenGraphics2d.drawLine(xSliceT, zS08, xSliceT, zI54);
        offscreenGraphics2d.drawLine(x04, zSliceT, x84, zSliceT);

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        offscreenGraphics2d.drawLine(x04, zS08, x04, zI54);
        offscreenGraphics2d.drawLine(x14, zS08, x14, zI54);
        offscreenGraphics2d.drawLine(x24, zS08, x24, zI54);
        offscreenGraphics2d.drawLine(x34, zS08, x34, zI54);
        offscreenGraphics2d.drawLine(x54, zS08, x54, zI54);
        offscreenGraphics2d.drawLine(x64, zS08, x64, zI54);
        offscreenGraphics2d.drawLine(x74, zS08, x74, zI54);
        offscreenGraphics2d.drawLine(x84, zS08, x84, zI54);

        verticalTalGridPts = new int[] {x04, x14, x24, x34, xSliceT, x54, x64, x74, x84};
        horizontalTalGridPts = new int[] {zS08, zS18, zS28, zS38, zS48, zS58, zS68, zS78, zSliceT, zI14, zI24, zI34,
                zI44, zI54};

        if (showTalairachGridmarkers) {
            offscreenGraphics2d.setColor(talairachMajorLineColor);
            offscreenGraphics2d.setStroke(defaultStroke);
            drawStringBW("d", offscreenGraphics2d, ( (x14 - x04) / 2) + x04, getSize().height - 2);
            drawStringBW("c", offscreenGraphics2d, ( (x24 - x14) / 2) + x14, getSize().height - 2);
            drawStringBW("b", offscreenGraphics2d, ( (x34 - x24) / 2) + x24, getSize().height - 2);
            drawStringBW("a", offscreenGraphics2d, ( (xSliceT - x34) / 2) + x34, getSize().height - 2);

            drawStringBW("a", offscreenGraphics2d, ( (x54 - xSliceT) / 2) + xSliceT, getSize().height - 2);
            drawStringBW("b", offscreenGraphics2d, ( (x64 - x54) / 2) + x54, getSize().height - 2);
            drawStringBW("c", offscreenGraphics2d, ( (x74 - x64) / 2) + x64, getSize().height - 2);
            drawStringBW("d", offscreenGraphics2d, ( (x84 - x74) / 2) + x74, getSize().height - 2);
        }

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);

        offscreenGraphics2d.drawLine(x04, zS08, x84, zS08);
        offscreenGraphics2d.drawLine(x04, zS18, x84, zS18);
        offscreenGraphics2d.drawLine(x04, zS28, x84, zS28);
        offscreenGraphics2d.drawLine(x04, zS38, x84, zS38);
        offscreenGraphics2d.drawLine(x04, zS48, x84, zS48);
        offscreenGraphics2d.drawLine(x04, zS58, x84, zS58);
        offscreenGraphics2d.drawLine(x04, zS68, x84, zS68);
        offscreenGraphics2d.drawLine(x04, zS78, x84, zS78);

        offscreenGraphics2d.drawLine(x04, zI14, x84, zI14);
        offscreenGraphics2d.drawLine(x04, zI24, x84, zI24);
        offscreenGraphics2d.drawLine(x04, zI34, x84, zI34);
        offscreenGraphics2d.drawLine(x04, zI44, x84, zI44);
        offscreenGraphics2d.drawLine(x04, zI54, x84, zI54);

        offscreenGraphics2d.setStroke(defaultStroke);

        if (showTalairachGridmarkers) {

            offscreenGraphics2d.setColor(talairachMajorLineColor);
            drawStringBW("13", offscreenGraphics2d, getSize().width - 13, ( (zI54 - zI44) / 2) + zI44 + 6);
            drawStringBW("12", offscreenGraphics2d, getSize().width - 13, ( (zI44 - zI34) / 2) + zI34 + 6);
            drawStringBW("11", offscreenGraphics2d, getSize().width - 13, ( (zI34 - zI24) / 2) + zI24 + 6);
            drawStringBW("10", offscreenGraphics2d, getSize().width - 13, ( (zI24 - zI14) / 2) + zI14 + 6);
            drawStringBW("9", offscreenGraphics2d, getSize().width - 9, ( (zI14 - zSliceT) / 2) + zSliceT + 6);
            drawStringBW("8", offscreenGraphics2d, getSize().width - 9, ( (zSliceT - zS78) / 2) + zS78 + 6);
            drawStringBW("7", offscreenGraphics2d, getSize().width - 9, ( (zS78 - zS68) / 2) + zS68 + 6);
            drawStringBW("6", offscreenGraphics2d, getSize().width - 9, ( (zS68 - zS58) / 2) + zS58 + 6);
            drawStringBW("5", offscreenGraphics2d, getSize().width - 9, ( (zS58 - zS48) / 2) + zS48 + 6);
            drawStringBW("4", offscreenGraphics2d, getSize().width - 9, ( (zS48 - zS38) / 2) + zS38 + 6);
            drawStringBW("3", offscreenGraphics2d, getSize().width - 9, ( (zS38 - zS28) / 2) + zS28 + 6);
            drawStringBW("2", offscreenGraphics2d, getSize().width - 9, ( (zS28 - zS18) / 2) + zS18 + 6);
            drawStringBW("1", offscreenGraphics2d, getSize().width - 9, ( (zS18 - zS08) / 2) + zS08 + 6);
        }
    }

    /**
     * Convenience method called by paintComponent(). Inserted here for simplicity's sake.
     * 
     * @param offscreenGraphics2d Graphics2D
     */
    private void drawTalairachGrid_SAGITTAL(final Graphics2D offscreenGraphics2d) {

        // we should check if we're inside the box first

        final Stroke defaultStroke = offscreenGraphics2d.getStroke();
        final Stroke dashedStroke = getDashedStroke();

        // slice separating inferior from superior is usually put ViewJFrameTriImage.ATLAS_AC_TO_INF or 42 mm. from z
        // = 0 at the inferior of the image. In case the z dimension = (ViewJFrameTriImage.ATLAS_AC_TO_INF +
        // ViewJFrameTriImage.ATLAS_AC_TO_SUP + 1)/resZ Sometimes it is put ViewJFrameTriImage.ATLAS_AC_TO_INF or 42 mm
        // from z = 0 at the inferior of the image. In this case the z dimension = (ViewJFrameTriImage.ATLAS_AC_TO_INF +
        // ViewJFrameTriImage.ATLAS_AC_TO_SUP + 1)/resZ
        final int zSliceT = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY);

        final int zS08 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 8 / 8);
        final int zS18 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 7 / 8);
        final int zS28 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 6 / 8);
        final int zS38 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 5 / 8);
        final int zS48 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 4 / 8);
        final int zS58 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 3 / 8);
        final int zS68 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 2 / 8);
        final int zS78 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_SUP
                * zoomY * 1 / 8);

        final int zI14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 1 / 4);
        final int zI24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 2 / 4);
        final int zI34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 3 / 4);
        final int zI44 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 4 / 4);
        final int zI54 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_SUP * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_INF
                * zoomY * 5 / 4);

        // slice separating anterior from median is always put ViewJFrameTriImage.ATLAS_AC_TO_ANT or 68 mm. from y = 0
        // at
        // the anterior of image slice separating median from posterior is always put ViewJFrameTriImage.ATLAS_AC_TO_ANT
        // + ViewJFrameTriImage.ATLAS_AC_TO_PC or 93 mm. from the anterior of the image
        final int ySliceT = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY);
        final int ySliceT2 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY);

        // new coordinates: middle -/+ Talairach dimensions
        final int yA04 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_ANT
                * zoomY * 4 / 4);
        final int yA14 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_ANT
                * zoomY * 3 / 4);
        final int yA24 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_ANT
                * zoomY * 2 / 4);
        final int yA34 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY - ViewJFrameTriImage.ATLAS_AC_TO_ANT
                * zoomY * 1 / 4);

        final int yP14 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY + ViewJFrameTriImage.ATLAS_PC_TO_POS * zoomY * 1 / 4);
        final int yP24 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY + ViewJFrameTriImage.ATLAS_PC_TO_POS * zoomY * 2 / 4);
        final int yP34 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY + ViewJFrameTriImage.ATLAS_PC_TO_POS * zoomY * 3 / 4);
        final int yP44 = MipavMath.round( (ViewJFrameTriImage.ATLAS_BBOX_ANT + ViewJFrameTriImage.ATLAS_AC_TO_PC)
                * zoomY + ViewJFrameTriImage.ATLAS_PC_TO_POS * zoomY * 4 / 4);

        final int yAP13 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_PC
                * zoomY * 1 / 3);
        final int yAP23 = MipavMath.round(ViewJFrameTriImage.ATLAS_BBOX_ANT * zoomY + ViewJFrameTriImage.ATLAS_AC_TO_PC
                * zoomY * 2 / 3);

        offscreenGraphics2d.setColor(talairachMajorLineColor);
        offscreenGraphics2d.setStroke(defaultStroke);
        offscreenGraphics2d.drawLine(yA04, zSliceT, yP44, zSliceT);
        offscreenGraphics2d.drawLine(ySliceT, zS08, ySliceT, zI54);
        offscreenGraphics2d.drawLine(ySliceT2, zS08, ySliceT2, zI54);

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);

        offscreenGraphics2d.drawLine(yA04, zS08, yP44, zS08);
        offscreenGraphics2d.drawLine(yA04, zS18, yP44, zS18);
        offscreenGraphics2d.drawLine(yA04, zS28, yP44, zS28);
        offscreenGraphics2d.drawLine(yA04, zS38, yP44, zS38);
        offscreenGraphics2d.drawLine(yA04, zS48, yP44, zS48);
        offscreenGraphics2d.drawLine(yA04, zS58, yP44, zS58);
        offscreenGraphics2d.drawLine(yA04, zS68, yP44, zS68);
        offscreenGraphics2d.drawLine(yA04, zS78, yP44, zS78);

        offscreenGraphics2d.drawLine(yA04, zI14, yP44, zI14);
        offscreenGraphics2d.drawLine(yA04, zI24, yP44, zI24);
        offscreenGraphics2d.drawLine(yA04, zI34, yP44, zI34);
        offscreenGraphics2d.drawLine(yA04, zI44, yP44, zI44);
        offscreenGraphics2d.drawLine(yA04, zI54, yP44, zI54);

        verticalTalGridPts = new int[] {yA04, yA14, yA24, yA34, ySliceT, ySliceT2, yP14, yP24, yP34, yP44};
        horizontalTalGridPts = new int[] {zS08, zS18, zS28, zS38, zS48, zS58, zS68, zS78, zSliceT, zI14, zI24, zI34,
                zI44, zI54};

        if (showTalairachGridmarkers) {

            offscreenGraphics2d.setColor(talairachMajorLineColor);
            offscreenGraphics2d.setStroke(defaultStroke);
            drawStringBW("13", offscreenGraphics2d, getSize().width - 13, ( (zI54 - zI44) / 2) + zI44 + 6);
            drawStringBW("12", offscreenGraphics2d, getSize().width - 13, ( (zI44 - zI34) / 2) + zI34 + 6);
            drawStringBW("11", offscreenGraphics2d, getSize().width - 13, ( (zI34 - zI24) / 2) + zI24 + 6);
            drawStringBW("10", offscreenGraphics2d, getSize().width - 13, ( (zI24 - zI14) / 2) + zI14 + 6);
            drawStringBW("9", offscreenGraphics2d, getSize().width - 9, ( (zI14 - zSliceT) / 2) + zSliceT + 6);
            drawStringBW("8", offscreenGraphics2d, getSize().width - 9, ( (zSliceT - zS78) / 2) + zS78 + 6);
            drawStringBW("7", offscreenGraphics2d, getSize().width - 9, ( (zS78 - zS68) / 2) + zS68 + 6);
            drawStringBW("6", offscreenGraphics2d, getSize().width - 9, ( (zS68 - zS58) / 2) + zS58 + 6);
            drawStringBW("5", offscreenGraphics2d, getSize().width - 9, ( (zS58 - zS48) / 2) + zS48 + 6);
            drawStringBW("4", offscreenGraphics2d, getSize().width - 9, ( (zS48 - zS38) / 2) + zS38 + 6);
            drawStringBW("3", offscreenGraphics2d, getSize().width - 9, ( (zS38 - zS28) / 2) + zS28 + 6);
            drawStringBW("2", offscreenGraphics2d, getSize().width - 9, ( (zS28 - zS18) / 2) + zS18 + 6);
            drawStringBW("1", offscreenGraphics2d, getSize().width - 9, ( (zS18 - zS08) / 2) + zS08 + 6);
        }

        offscreenGraphics2d.setColor(talairachMinorLineColor);
        offscreenGraphics2d.setStroke(dashedStroke);
        offscreenGraphics2d.drawLine(yA04, zS08, yA04, zI54);
        offscreenGraphics2d.drawLine(yA14, zS08, yA14, zI54);
        offscreenGraphics2d.drawLine(yA24, zS08, yA24, zI54);
        offscreenGraphics2d.drawLine(yA34, zS08, yA34, zI54);
        offscreenGraphics2d.drawLine(yP14, zS08, yP14, zI54);
        offscreenGraphics2d.drawLine(yP24, zS08, yP24, zI54);
        offscreenGraphics2d.drawLine(yP34, zS08, yP34, zI54);
        offscreenGraphics2d.drawLine(yP44, zS08, yP44, zI54);
        offscreenGraphics2d.drawLine(yAP13, zS08, yAP13, zI54);
        offscreenGraphics2d.drawLine(yAP23, zS08, yAP23, zI54);

        offscreenGraphics2d.setStroke(defaultStroke);

        if (showTalairachGridmarkers) {
            offscreenGraphics2d.setColor(talairachMajorLineColor);
            drawStringBW("A", offscreenGraphics2d, ( (yA14 - yA04) / 2) + yA04, getSize().height - 2);
            drawStringBW("B", offscreenGraphics2d, ( (yA24 - yA14) / 2) + yA14, getSize().height - 2);
            drawStringBW("C", offscreenGraphics2d, ( (yA34 - yA24) / 2) + yA24, getSize().height - 2);
            drawStringBW("D", offscreenGraphics2d, ( (ySliceT - yA34) / 2) + yA34, getSize().height - 2);
            drawStringBW("E", offscreenGraphics2d, ( (ySliceT2 - ySliceT) / 2) + ySliceT, getSize().height - 2);
            drawStringBW("F", offscreenGraphics2d, ( (yP14 - ySliceT2) / 2) + ySliceT2, getSize().height - 2);
            drawStringBW("G", offscreenGraphics2d, ( (yP24 - yP14) / 2) + yP14, getSize().height - 2);
            drawStringBW("H", offscreenGraphics2d, ( (yP34 - yP24) / 2) + yP24, getSize().height - 2);
            drawStringBW("I", offscreenGraphics2d, ( (yP44 - yP34) / 2) + yP34, getSize().height - 2);
        }
    }

    /**
     * Builds the dashed stroke used to render the minor talairach grid lines.
     * 
     * @return the BasicStroke object used to render the minor talairach grid lines
     */
    private BasicStroke getDashedStroke() {
        final BasicStroke stroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f,
                new float[] {1.0f}, 0.0f);

        return stroke;
    }

    /**
     * Convenience method called by mousePressed(). Inserted here for simplicity's sake because this method is quite
     * long. Handles the mouse pressed event when in the DEFAULT mode.
     * 
     * @param mouseEvent Graphics2D
    private void handleVOIProcessing(final MouseEvent mouseEvent) {
        int j;
        Vector3f pt;

        final ViewVOIVector VOIs = imageActive.getVOIs();

        if (VOIs == null) {
            return;
        }

        final int nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++) {
            VOIs.VOIAt(i).setAllActive(false); // deactivate all other VOIs
        }

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) // curve type is a VOI point
            {
                final Vector3f patientMousePoint = new Vector3f();
                super.ScreenToLocal(new Vector3f(mouseEvent.getX(), mouseEvent.getY(), slice), patientMousePoint);

                final Vector3f volumeMousePoint = new Vector3f();
                MipavCoordinateSystems.patientToFile(patientMousePoint, volumeMousePoint, imageActive, orientation);

                final int zOrg = (int) volumeMousePoint.Z;

                pt = null;

                Vector3f[] voiPoints;

                for (int p = zOrg - 1; p < (zOrg + 2); p++) {
                    voiPoints = VOIs.VOIAt(i).exportPoints(p);

                    for (j = 0; (j < voiPoints.length) && (pt == null); j++) {

                        if ( ((VOIPoint) (VOIs.VOIAt(i).getCurves()[p].elementAt(j)))
                                .nearPointInPlane(volumeMousePoint)) {
                            VOIs.VOIAt(i).setActive(true);
                            ((VOIPoint) (VOIs.VOIAt(i).getCurves()[p].elementAt(j))).setActive(true);
                            
                            System.err.println( "    TRIImage.handleVOIProcessing" );
                            voiHandler.setVOI_ID(VOIs.VOIAt(i).getID());
                            pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves()[p].elementAt(j))).exportPoint();
                        }
                    }
                }

                if (pt != null) {
                    lastZOrg = zOrg;
                    setCursorMode(ViewJComponentBase.MOVE_VOIPOINT);
                    imageActive.notifyImageDisplayListeners();
                }

            } // if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
        } // for (i = 0; i < nVOI; i++)
    }

     */
    
    
    /**
     * For generating the display of 1 or 2 RGB images.
     * 
     * @param tSlice t (time) slice to show
     * @param _LUTa DOCUMENT ME!
     * @param _LUTb DOCUMENT ME!
     * @param forceShow forces this method to import image and recalculate java image
     * @param _interpMode DOCUMENT ME!
     * 
     * @return boolean to indicate if the show was successful
     */
    private boolean showUsingOrientation(final int tSlice, final ModelLUT _LUTa, final ModelLUT _LUTb,
            final boolean forceShow, final int _interpMode) {

        m_kPatientSlice.setLUTa(_LUTa);
        m_kPatientSlice.setLUTb(_LUTb);
        m_kPatientSlice.showUsingOrientation(tSlice, cleanImageBufferA, cleanImageBufferB, forceShow, false);

        if (_interpMode > -1) {
            setInterpolationMode(_interpMode);
        }

        setSliceString(String.valueOf(slice + 1));
        repaint();

        return true;
    }

    /**
     * Updates the crop bounding volume when a point on the crop bounding box is dragged with the mouse. Converts the
     * mousePoint to the crop corners, converts from Screen space to PatientCoordinates (ScreenToLocal), then converts
     * from PatientCoordinates to FileCoordinates and passes the new crop volume information to the ViewJFrameTriImage
     * container class.
     * 
     * @param index the index of the bounding box that changed
     * @param mousePoint the screen-space coordinats of the changed bounding box point.
     */
    private void updateCrop(final int index, final Vector2f mousePoint) {

        /* set the crop corners based on which point moved: */
        if (index == 0) {
            cornerCropPt[0].X = mousePoint.X;
            cornerCropPt[0].Y = mousePoint.Y;
        } else if (index == 1) {
            cornerCropPt[0].X = mousePoint.X;
            cornerCropPt[1].Y = mousePoint.Y;
        } else if (index == 2) {
            cornerCropPt[1].X = mousePoint.X;
            cornerCropPt[1].Y = mousePoint.Y;
        } else if (index == 3) {
            cornerCropPt[1].X = mousePoint.X;
            cornerCropPt[0].Y = mousePoint.Y;
        }

        /* convert from Screen -> Patient -> File */
        super.ScreenToLocal(cornerCropPt[0], m_kLocalCropLower);

        final Vector3f kFileCropLower = new Vector3f();
        MipavCoordinateSystems.patientToFile(m_kLocalCropLower, kFileCropLower, imageActive, orientation);

        super.ScreenToLocal(cornerCropPt[1], m_kLocalCropUpper);

        final Vector3f kFileCropUpper = new Vector3f();
        MipavCoordinateSystems.patientToFile(m_kLocalCropUpper, kFileCropUpper, imageActive, orientation);

        /* update frame */
        triImageFrame.setCrop(kFileCropLower, kFileCropUpper);
    }

    /**
     * updates the slice value when the wheel is moved or the page_up, page_down keys are pressed. Does bounds checking
     * and comparison with the current slice value. Sets the new position and updates the triImageFrame.
     * 
     * @param newSlice the new slice value
     */
    private void updateSlice(final int newSlice) {

        if (m_kPatientSlice.updateSlice(newSlice)) {
            m_kVolumePoint = m_kPatientSlice.getCenter();
            triImageFrame.setCenter((int) m_kVolumePoint.X, (int) m_kVolumePoint.Y, (int) m_kVolumePoint.Z);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener#screenToFile(int, int, int, WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public boolean screenToFile(final int iX, final int iY, final int iZ, final Vector3f kVolumePt) {
        boolean bClipped = false;
        if ( (iX < 0) || (iX > getWidth()) || (iY < 0) || (iY > getHeight())) {
            bClipped = true;
        }
        final Vector3f screenPt = new Vector3f(iX, iY, iZ);
        final Vector3f patientPt = new Vector3f();
        super.ScreenToLocal(screenPt, patientPt);
        if ( (patientPt.X < 0) || (patientPt.X > localImageExtents[0] - 1) || (patientPt.Y < 0)
                || (patientPt.Y > localImageExtents[1] - 1)) {
            bClipped = true;
        }

        MipavCoordinateSystems.patientToFile(patientPt, kVolumePt, imageA, orientation);
        return bClipped;
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener#screenToFile(WildMagic.LibFoundation.Mathematics.Vector3f,
     *      WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public boolean screenToFile(final Vector3f kScreen, final Vector3f kFile) {
        boolean bClipped = false;
        if ( (kScreen.X < 0) || (kScreen.X > getWidth()) || (kScreen.Y < 0) || (kScreen.Y > getHeight())) {
            bClipped = true;
        }
        final Vector3f patientPt = new Vector3f();
        super.ScreenToLocal(kScreen, patientPt);
        if ( (patientPt.X < 0) || (patientPt.X > localImageExtents[0] - 1) || (patientPt.Y < 0)
                || (patientPt.Y > localImageExtents[1] - 1)) {
            bClipped = true;
        }

        MipavCoordinateSystems.patientToFile(patientPt, kFile, imageA, orientation);
        return bClipped;
    }

    /**
     * Increments the current slice in local coordinates and returns the new position in the view-independent file
     * coordinates.
     * 
     * @return
     */
    public Vector3f upSlice() {
        if (slice + 1 < localImageExtents[2]) {
            final Vector3f kLocalPoint = new Vector3f();
            MipavCoordinateSystems.fileToPatient(m_kVolumePoint, kLocalPoint, imageActive, orientation);
            kLocalPoint.Z++;
            MipavCoordinateSystems.patientToFile(kLocalPoint, m_kVolumePoint, imageActive, orientation);
            setCenter((int) m_kVolumePoint.X, (int) m_kVolumePoint.Y, (int) m_kVolumePoint.Z);
            return m_kVolumePoint;
        }
        return null;
    }

    /**
     * Decrements the current slice in local coordinates and returns the new position in the view-independent file
     * coordinates.
     * 
     * @return
     */
    public Vector3f downSlice() {
        if (slice - 1 > 0) {
            final Vector3f kLocalPoint = new Vector3f();
            MipavCoordinateSystems.fileToPatient(m_kVolumePoint, kLocalPoint, imageActive, orientation);
            kLocalPoint.Z--;
            MipavCoordinateSystems.patientToFile(kLocalPoint, m_kVolumePoint, imageActive, orientation);
            setCenter((int) m_kVolumePoint.X, (int) m_kVolumePoint.Y, (int) m_kVolumePoint.Z);
            return m_kVolumePoint;
        }
        return null;
    }
}
