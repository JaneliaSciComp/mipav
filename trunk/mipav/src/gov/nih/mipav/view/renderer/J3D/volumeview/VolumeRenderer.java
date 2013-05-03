package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.utils.universe.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.Date;

import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * Common interface for all volume renderers. Code common to the RayCastVolumeRender and ShearWarpVolumeRender classes
 * was factored into this, the parent class on 11/14/04.
 *
 * @author  Matthew J. McAuliffe, Ph.D.
 * @see     ViewJFrameSurface
 */

public abstract class VolumeRenderer extends RenderViewBase implements MouseMotionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6617098263675852750L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Component holding the texture of the XY plane image. */
    protected ViewJComponentRenderImage componentImageXY;

    /** Extents of image A, same as xDim, yDim, etc. */
    protected int[] extents;

    /** Current image A. */
    protected ModelImage imageA;

    /** Current image B. */
    protected ModelImage imageB;

    /** light control panel. */
    protected JPanelLights lightControl;

    /** max Extent of X, Y dimensions. */
    protected int maxRenExtent = 256;

    /** Dialog to turn bounding box of surface renderer on and off, and to change the color of the frame. */
    protected JPanelRendererBase optionsPanel;

    /** Actual bar which fills with color as the percentage of completion increases. */
    protected ViewJProgressBar pBar;

    /** Buffer that holds image A pixel data for the XY plane, used to build displayable image. */
    protected int[] pixBufferA_XY;

    /** Buffer that holds image B pixel data for the XY plane, used to build displayable image. */
    protected int[] pixBufferB_XY;

    /** Sliders for the image planes. */
    protected JSlider sliderT;

    /** Surface renderer reference. */
    protected SurfaceRender surfaceRender;

    /** Text fields that display the slice number next to the sliders. */
    protected JTextField textT;

    /** Which time slice is currently displayed. */
    protected int tSlice;

    /** Dimensions of image A. */
    protected int xDim, yDim, zDim, tDim;

    /** Which slice is currently displayed in the XY plane. */
    protected int zSlice;

    /** Mouse rotate behavior. */
    private MouseRotate mouseBehaviorRotate;

    /** Mouse translate behavior. */
    private MouseTranslate mouseBehaviorTranslate;

    /** Mouse zoom behavior. */
    private MouseZoom mouseBehaviorZoom;

    /** The frame around the x slice. */
    private ViewJComponentBoxSlice boxSliceX;

    /** Current transform matrix. */
    private Transform3D currentTransform;

    /** Current transform type. */
    private int currentTransType;

    /** Flag indicating mouse rotation, avoid image disappearing problem. */
    private boolean isRotate = false;

    /** Original Appearance obj of the ImageComponent2D texture. */
    private Appearance kDefaultAppearance;

    /** Shape object of the volume renderer. */
    private Shape3D kShape;

    /** Flag to turn mouseListener on or off. */
    private boolean m_bMouseListen = true;

    /** Buffer image for the volume renderer. */
    private BufferedImage m_kImage;

    /** Image component 2D to hold the volume renderer properties. */
    private ImageComponent2D m_kImageComponent;

    /** Set of lights to use for software-based lighting. */
    private SoftwareLightSet m_kSoftwareLightSet = new SoftwareLightSet();

    /** Material properties to use for software-based lighting. */
    private SoftwareMaterial m_kSoftwareMaterial = new SoftwareMaterial();

    /** 2D texture of the volume renderer. */
    private Texture2D m_kTextureXY;

    /** Flag to indicate whether the mouseReleased event triggered or not. */
    private boolean mouseReleased = false;

    /** Parent of the x slice. */
    private BranchGroup objBoxSliceX;

    /**
     * Member variables used to display the rendered image on the texture-mapped polygon, stored so that they can be
     * updated as the render target image size is changed by the user-interface:.
     */
    private TransformGroup objTransSliceX;

    /** Group dictating how the XY plane is translated. */
    private TransformGroup objTransXY;

    /** Parent of the XY plane image. */
    private BranchGroup objXYPlane;

    /** Parallel rotation flag. */
    private boolean parallelRotation = true;

    /** Resolutions of image A. */
    private float[] resols = new float[4];

    /** DOCUMENT ME! */
    private BranchGroup shapeBG;

    /** DOCUMENT ME! */
    private BranchGroup shapeSliceBG;

    /** Units of measure of imageA. */
    private int[] units = new int[4];

    /** Update the window level flag, call by the plane renderer. */
    private boolean updateWinlevel = true;

    /** Numbers dicatating the sizes of the planes based on the extents and resolutions of the image. */
    private float xBox, yBox, zBox, maxBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new VolumeRenderer object.
     *
     * @param  _imageA  First image to display, cannot be null.
     * @param  _imageB  Second loaded image, may be null.
     * @param  _config  Graphics configuration reference.
     */
    public VolumeRenderer(ModelImage _imageA, ModelImage _imageB, GraphicsConfiguration _config) {
        super(_imageA, _imageB, _config);
    }


    /**
     * Makes a frame and puts the three image planes into it. Creates the scene graph which dictates the behavior of the
     * image planes and surfaces. Initializes the surface dialog and the mouse recorder dialog, so that this original
     * view is saved. When the user opens these dialogs, they have already been created; they are just set to visible.
     *
     * @param  _imageA        First image to display, cannot be null.
     * @param  _LUTa          LUT of the imageA (if null grayscale LUT is constructed).
     * @param  _imageB        Second loaded image, may be null.
     * @param  _LUTb          LUT of the imageB, may be null.
     * @param  surfaceRender  Surface render reference.
     * @param  _config        Graphics configuration reference.
     * @param  _pBar          Build volume render progress bar reference.
     */
    public VolumeRenderer(ModelImage _imageA, ModelLUT _LUTa, ModelImage _imageB, ModelLUT _LUTb,
                          SurfaceRender surfaceRender, GraphicsConfiguration _config, ViewJProgressBar _pBar) {
        super(_imageA, _imageB, _config);
        this.imageA = _imageA;
        this.imageB = _imageB;
        this.LUTa = _LUTa;
        this.LUTb = _LUTb;
        this.surfaceRender = surfaceRender;
        pBar = _pBar;
        imageA.setImageOrder(ModelImage.IMAGE_A);

        if (imageB != null) {
            imageB.setImageOrder(ModelImage.IMAGE_B);
        }

        if (((imageA.getNDims() == 3) && (imageB == null)) ||
                ((imageA.getNDims() == 3) && (imageB != null) && (imageB.getNDims() == 3))) {
            extents = new int[3];
            extents[0] = imageA.getExtents()[0];
            extents[1] = imageA.getExtents()[1];
            extents[2] = imageA.getExtents()[2];
        } else { // imageA.getNDims() == 4 or imageB.getNDims() == 4
            extents = new int[4];
            extents[0] = imageA.getExtents()[0];
            extents[1] = imageA.getExtents()[1];
            extents[2] = imageA.getExtents()[2];

            if (imageA.getNDims() == 4) {
                extents[3] = imageA.getExtents()[3];
            } else {
                extents[3] = imageB.getExtents()[3];
            }
        }

        xDim = imageA.getExtents()[0];
        yDim = imageA.getExtents()[1];
        zDim = imageA.getExtents()[2];
        resols[0] = Math.abs(imageA.getFileInfo()[0].getResolutions()[0]);
        resols[1] = Math.abs(imageA.getFileInfo()[0].getResolutions()[1]);
        resols[2] = Math.abs(imageA.getFileInfo()[0].getResolutions()[2]);

        Preferences.debug("Using (raycast): " + resols[2]);

        if ((resols[0] == 0.0f) || (resols[1] == 0.0f) || (resols[2] == 0.0f)) {
            resols[0] = 1.0f;
            resols[1] = 1.0f;
            resols[2] = 1.0f;
        }

        units[0] = imageA.getFileInfo()[0].getUnitsOfMeasure()[0];
        units[1] = imageA.getFileInfo()[0].getUnitsOfMeasure()[1];
        units[2] = imageA.getFileInfo()[0].getUnitsOfMeasure()[2];

        zSlice = (zDim - 1) / 2;

        if (imageA.getNDims() == 4) {
            tDim = imageA.getExtents()[3];
            tSlice = (tDim - 1) / 2;
            resols[3] = imageA.getFileInfo()[0].getResolutions()[3];
        } else if (imageB != null) {

            if (imageB.getNDims() == 4) {
                tDim = imageB.getExtents()[3];
                tSlice = (tDim - 1) / 2;
                resols[3] = imageB.getFileInfo()[0].getResolutions()[3];
            }
        }

        configureFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets how the image plane should be displayed depending on value of slider.
     *
     * @param  e  Event that triggered this function.
     */
    public abstract void stateChanged(ChangeEvent e);

    /**
     * Calls various methods depending on the action.
     *
     * <ul>
     *   <li>Surface - opens the surface dialog</li>
     *   <li>View - opens the view control dialog</li>
     *   <li>Mouse - opens the mouse recorder dialog</li>
     *   <li>About - displays a message about this renderer</li>
     *   <li>MIPmode - MIP rendering mode</li>
     *   <li>DDRmode - DDR rendering mode</li>
     *   <li>CMPmode - CMP rendering mode</li>
     *   <li>SURmode - SUR rendering mode</li>
     *   <li>Exit - sets variables to null and disposes of this frame</li>
     *   <li>X, Y, Z checkboxes - toggles the appropriate image planes on or off</li>
     * </ul>
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("MIPmode")) {
            MIPMode();
        } else if (command.equals("DRRmode")) {
            DRRMode();
        } else if (command.equals("SURmode")) {
            SURMode();
        } else if (command.equals("CMPmode")) {
            CMPMode();
        }
    }

    /**
     * Override the parent autoCapture method to capture MIP image volume.
     */
    public void autoCapture() {
        super.autoCapture();
    }

    /**
     * Closes the frame.
     */
    public void close() {
        disposeLocal(true);
    }


    /**
     * Invoke the composite rendering mode.
     */
    public void CMPMode() {
        setRenderMode(ViewJComponentRenderImage.ModeCOMPOSITE);
        updateImages(true);
    }

    /**
     * Clean memory.
     *
     * @param  flag  is true call the super.disposeLocal
     */
    public void disposeLocal(boolean flag) {
        imageA = null;
        imageB = null;
        pixBufferA_XY = null;
        pixBufferB_XY = null;
        m_kSoftwareMaterial = null;
        m_kSoftwareLightSet = null;

        if (componentImageXY != null) {
            componentImageXY.disposeLocal(flag);
            componentImageXY = null;
        }

        if (lightControl != null) {
            lightControl.disposeLocal(flag);
            lightControl = null;
        }

        if (m_kImage != null) {
            m_kImage.flush();
            m_kImage = null;
        }

        if (universe != null) {
            universe.removeAllLocales();
            universe = null;
        }

        m_kImageComponent = null;
        m_kTextureXY = null;

        pBar = null;

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Invoke the DRR rendering mode.
     */
    public void DRRMode() {
        setRenderMode(ViewJComponentRenderImage.ModeXRAY);
        updateImages(true);
    }

    /**
     * Get the camera control panel.
     *
     * @return  JPanelCamera camera control panel
     */
    public JPanelCamera getCameraControl() {
        return rotationControlPanel;
    }

    /**
     * Accessor that returns the reference to componentImageXY.
     *
     * @return  DOCUMENT ME!
     */
    public ViewJComponentRenderImage getComponentImageXY() {
        return componentImageXY;
    }

    /**
     * Query information which describes the bounds of the volume.
     *
     * @return  int[] Array of three values containing the X, Y, and Z dimensions respectively.
     */
    public int[] getExtents() {
        return extents;
    }

    /**
     * Return access to the current eye point in world coordinates.
     *
     * @return  Point3f current eye point in world coordinates.
     */
    public Point3f getEyePoint() {
        return componentImageXY.getEyePoint();
    }


    /**
     * Accessor that returns the reference to image A.
     *
     * @return  Image A.
     */
    public ModelImage getImageA() {

        if (componentImageXY != null) {
            return componentImageXY.getImageA();
        } else {
            return null;
        }
    }

    /**
     * Accessor that returns the reference to image B.
     *
     * @return  Image B.
     */
    public ModelImage getImageB() {

        if (componentImageXY != null) {
            return componentImageXY.getImageB();
        } else {
            return null;
        }
    }

    /**
     * Accessor that returns the reference to m_kImageComponent.
     *
     * @return  DOCUMENT ME!
     */
    public ImageComponent2D getImageComponent() {
        return m_kImageComponent;
    }

    /**
     * Get the light control panel.
     *
     * @return  JPanel the light control panel
     */
    public JPanelLights getLightControlPanel() {
        return lightControl;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getMaxRenExtent() {
        return maxRenExtent;
    }

    /**
     * Accessor that returns the reference to objTransXY.
     *
     * @return  DOCUMENT ME!
     */
    public TransformGroup getObjTransXY() {
        return objTransXY;
    }

    /**
     * Get the view control panel.
     *
     * @return  JPanel view control panel
     */
    public JPanel getOptions() {
        return optionsPanel.getMainPanel();
    }

    /**
     * Get the option control panel.
     *
     * @return  optionsPanel JPanelShearWarpRenderOptions options panel
     */
    public JPanelRendererBase getOptionsPanel() {
        return optionsPanel;
    }

    /**
     * Change the camera model.
     *
     * @return  bParallel true for a parallel camera, false for a perspective camera
     */
    public boolean getParallel() {
        return (componentImageXY.getParallel());
    }

    /**
     * Change the camera model.
     *
     * @return  renderMode MIP, DDR, SUR
     */
    public int getRenderMode() {
        return componentImageXY.getRenderMode();
    }

    /**
     * Gets the current scene state, in terms of what numbers the slices are on and whether or not they are visible.
     *
     * @return  A SceneState object with the variables set appropriately.
     */
    public Object getSceneState() {
        return new Object(); // Here to fill space a the moment.
    }

    /**
     * Get the ray trace space size.
     *
     * @return  componentImageXY.getSpaceSize() space size
     */
    public int getSpaceSize() {
        return (componentImageXY.getSpaceSize());
    }

    /**
     * Get the ray trace step size.
     *
     * @return  componentImageXY.getStepSize() step size
     */
    public int getStepSize() {
        return (componentImageXY.getStepSize());
    }

    /**
     * Get the opacity histrogram control panel.
     *
     * @return  JPanelVolOpacity Opacity panel
     */
    public JPanelVolumeOpacity getVolOpacity() {
        return surfaceRender.getVolOpacityPanel();
    }

    /**
     * Invoke the mip rendering mode.
     */
    public void MIPMode() {
        setRenderMode(ViewJComponentRenderImage.ModeMIP);
        updateImages(true);
    }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  the mouse event generated by a mouse clicked
     */
    public void mouseClicked(MouseEvent kEvent) { /* stub */
    }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    public void mouseDragged(MouseEvent kEvent) {

        if (m_bMouseListen) {
            updateImage();
        }
    }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  the mouse event generated by a mouse entered
     */
    public void mouseEntered(MouseEvent kEvent) { /* stub */
    }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  the mouse event generated by a mouse exit
     */
    public void mouseExited(MouseEvent kEvent) { /* stub */
    }

    /**
     * One of the overrides necessary to be a MouseMotionListener. This member only exists to satisfy the conditions of
     * being a MouseMotionListener. It does nothing when invoked.
     *
     * @param  kEvent  the event generated by a mouse movement
     */
    public void mouseMoved(MouseEvent kEvent) { }

    /**
     * One of the overrides necessary to be a MouseListener. When a mouse button is pressed, the application is starting
     * a drag operation to rotate the virtual track ball. The initial mouse location is recorded for use by
     * 'moveTrackBall', stored as a point (x0,y0) in [-1,1]^2.
     *
     * @param  kEvent  the mouse event generated by a mouse press
     */
    public void mousePressed(MouseEvent kEvent) { }

    /**
     * One of the overrides necessary to be a MouseListener.
     *
     * @param  kEvent  the mouse event generated by a mouse release
     */
    public void mouseReleased(MouseEvent kEvent) {

        if (m_bMouseListen) {
            processMouseReleased(kEvent);
        }

        surfaceRender.transformUpdate(currentTransType, currentTransform);
    }

    /**
     * When a mouse button is released, the application is finishing a drag operation to rotate the virtual track ball.
     * The final mouse location is recorded for use by 'moveTrackBall', stored as a point (x1,y1) in [-1,1]^2. The
     * virtual track ball is moved into its final position and a ray trace is performed at highest resolution.
     *
     * @param  kEvent  the mouse event generated by a mouse release
     */
    public void processMouseReleased(MouseEvent kEvent) {

        /* If mouse activity is disabled, the do not process the mouseReleased
         * event: */
        if (m_bMouseListen == false) {
            return;
        }

        // isRotate ensure image updates only during the mouse rotation.
        // isRotate avoids the image disappearing problem, which caused by the first time
        // MouseZoom and MouseTranslate.  The solution is to avoid the RayTrace during
        // the MouseZoom and MouseTranslate.
        mouseReleased = true;

        if (isRotate) {
            componentImageXY.mouseReleased(kEvent);
            updateImage();
            isRotate = false;
            

            Date kDate = new Date();
            double dCurrentTime = kDate.getTime() / 1000.0;
            double dDelta = dCurrentTime - m_dLastTime;
            System.err.println( "FPS (full-resolution): " + 1.0f/dDelta );
            ResetTime();
        }

        mouseReleased = false;
    }

    /**
     * Override the parent resetImage method to update image volume in the MIP volume renderer.
     */
    public void resetAxisX() {
        super.resetAxisX();
        componentImageXY.updateTransform(transRotation);
        updateImages(true);

    }

    /**
     * Override the parent resetImage method to update image volume in the MIP volume renderer.
     */
    public void resetAxisY() {
        super.resetAxisY();
        componentImageXY.updateTransform(transRotation);
        updateImages(true);
    }


    /**
     * Override the parent resetImage method to update image volume in the MIP volume renderer.
     */
    public void resetImage() {
        super.resetImage();
        componentImageXY.updateTransform(transRotation);
        updateImages(true);
    }

    /**
     * Override the parent rotateImage method to rotate the MIP image volume. This method manully rotate the image
     * volume during the camera snapshooting.
     */
    public void rotateImage() {

        if (rotationAxis == ViewJFrameRenderCamera.NO_AXIS) {
            return;
        }

        /** Hack way to resolve one bug */
        rotationTotal += rotationAngle;

        if ((rotationTotal != 0) && ((rotationTotal % 180) == 0) && ((rotationTotal % 360) != 0)) {
            rotationAngle += 1;
        }

        super.rotateImage();

        // componentImageXY.mouseReleased( null );
        updateImages(true);

        if ((rotationTotal != 0) && ((rotationTotal % 180) == 0) && ((rotationTotal % 360) != 0)) {
            rotationAngle -= 1;
        }
    }

    /**
     * Does nothing but must instantiate for this to be a subclass of ViewJFrameBase.
     *
     * @param  image  DOCUMENT ME!
     */
    public void setActiveImage(int image) { }

    /**
     * Sets the alpha blending of parameter for two image displays.Do I need this ??
     *
     * @param  value  Amount [0,100] that is the percentage of Image A to be displayed.
     */
    public void setAlphaBlend(int value) {

        if (componentImageXY != null) {
            componentImageXY.setAlphaBlend(value);
        }
    }


    /**
     * Sets the background color for the frame and rendered image.
     *
     * @param  color  Color RGBA color to use as the background color.
     */
    public void setBackgroundColor(Color color) {
        super.setBackgroundColor(color);
        componentImageXY.setBackgroundColor(color);
        componentImageXY.show(tSlice, null, null, false, true);
        updateImage();
    }

    /**
     * Controls whether or not the images/VOIs of the frame can be modified.
     *
     * @param  flag  If <code>true</code> the image/VOIs can be modified; if <code>false</code> image/VOIs can NOT be
     *               modified.
     */
    public void setEnabled(boolean flag) { }

    /**
     * set the MouseBehavior enabled states. this is used to turn on and off the mouse behavior
     *
     * @param  bEnable  boolean bEnable to turn mouseListening on or off.
     */
    public void setEnableMouseBehaviors(boolean bEnable) {
        mouseBehaviorRotate.setEnable(bEnable);
        mouseBehaviorZoom.setEnable(bEnable);
        mouseBehaviorTranslate.setEnable(bEnable);
        m_bMouseListen = bEnable;
    }

    /**
     * Sets the GUI components to their proper state before the action is dispatched from the player.
     *
     * @param  scene  The state of the scene.
     */
    public void setGUI(Object scene) { }

    /**
     * Accessor that sets the LUT for image A.
     *
     * @param  LUT  The LUT.
     */
    public void setLUTa(ModelLUT LUT) {
        componentImageXY.setLUTa(LUT);
    }

    /**
     * Accessor that sets the LUT for image B.
     *
     * @param  LUT  The LUT
     */
    public void setLUTb(ModelLUT LUT) {
        componentImageXY.setLUTb(LUT);
    }

    /**
     * Set the texture material shininess value.
     *
     * @param  value  shininess value.
     */
    public void setMaterialShininess(float value) {
        m_kSoftwareMaterial.shininess = value;
    }

    /**
     * setMaxRenExtent changes the render target image by changing the pixBufferA_XY size and by changing the render
     * target image of the componentImageXY member variable. createTexturedImage (local) is called to reinitialize the
     * size of the texture displayed on the texture-mapped polygon.
     *
     * @param  iExtent  DOCUMENT ME!
     */
    public void setMaxRenExtent(int iExtent) {

        /* Only If the new target image size has changed: */
        if (maxRenExtent != iExtent) {
            maxRenExtent = iExtent;

            int bufferFactor = imageA.isColorImage() ? 4 : 1;
            /* re-initialize the size of the buffers: */
            pixBufferA_XY = null;
            pixBufferA_XY = new int[bufferFactor * maxRenExtent * maxRenExtent];

            if (imageB != null) {
            	bufferFactor = imageB.isColorImage() ? 4 : 1;
                pixBufferB_XY = null;
                pixBufferB_XY = new int[bufferFactor * maxRenExtent * maxRenExtent];
            }

            /* udate the render target image size for the
             * ViewJComponentRenderImage: */
            componentImageXY.setMaxExtent(iExtent, pixBufferA_XY, pixBufferB_XY);

            int iRenderingMode = componentImageXY.getRenderMode();
            componentImageXY.setRenderMode(iRenderingMode);

            /* Preserve the current rotation angle of view: */
            Transform3D rotation = new Transform3D();
            sceneRootTG.getTransform(rotation);
            componentImageXY.updateTransform(rotation);
            componentImageXY.show(tSlice, null, null, true, true);
            createTexturedImage();

            if (iRenderingMode == ViewJComponentRenderImage.ModeSURFACE) {
                lightControl.refreshLighting();
            } else {
                updateImage();
            }
        }
    }

    /**
     * Change the camera model.
     *
     * @param  bParallel  true for a parallel camera, false for a perspective camera
     */
    public void setParallel(boolean bParallel) {
        componentImageXY.setParallel(bParallel);
    }

    /**
     * Set the parallel rotation flag from the viewJFrameVolumeView.
     *
     * @param  flag  <code>true</code> set all the renderer to parallel rotation, <code>false</code> parallel rotation
     *               mode off.
     */
    public void setParallelRotation(boolean flag) {
        parallelRotation = flag;
    }

    /**
     * Change the camera model.
     *
     * @param  renderMode  MIP, DDR, SUR
     */
    public void setRenderMode(int renderMode) {
        componentImageXY.setRenderMode(renderMode);
    }

    /**
     * Sets the RGB table for image A.
     *
     * @param  RGBT  New RGB table for image A.
     */
    public void setRGBTA(ModelRGB RGBT) {

        if (componentImageXY != null) {
            componentImageXY.setRGBTA(RGBT);
        }
    }

    /**
     * Sets the RGB table for image B.
     *
     * @param  RGBT  New RGB table for image B.
     */
    public void setRGBTB(ModelRGB RGBT) {

        if (componentImageXY != null) {
            componentImageXY.setRGBTB(RGBT);
        }
    }

    /**
     * Set the rotation angle for the camera control.
     *
     * @param  value  rotation angle
     */
    public void setRotationAngle(int value) {
        rotationAngle = value;
    }

    /**
     * Set the rotation axis for the camera control.
     *
     * @param  axis  rotation axis
     */
    public void setRotationAxis(int axis) {
        rotationAxis = axis;
    }

    /**
     * Sets the scene state appropriately, so that the slices that are supposed to be visible are showing, the ones that
     * aren't are hidden, and the sliders are starting at the appropriate value.
     *
     * @param  scene  The state of the scene.
     */
    public void setSceneState(Object scene) { }

    /**
     * Set the light for the given light index.
     *
     * @param  iLight  int
     */
    public void setSelectedLight(int iLight) {

        if (null != lightControl) {
            lightControl.setSelectedIndex(iLight);
        }
    }

    /**
     * Does nothing but must instantiate for this to be a subclass of ViewJFrameBase.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setSlice(int slice) { }

    /**
     * Get the ray trace space size.
     *
     * @param  spaceSize  Raytracing region space size.
     */
    public void setSpaceSize(int spaceSize) {
        componentImageXY.setSpaceSize(spaceSize);
    }

    /**
     * Change the camera model.
     *
     * @param  stepSize  RayTracint step size.
     */
    public void setStepSize(int stepSize) {
        componentImageXY.setStepSize(stepSize);
    }

    /**
     * Sets the time slice to be displayed and changes the image planes displayed.
     *
     * @param  slice  Indicates image time-slice (4th dimension) to be displayed.
     */
    public void setTimeSlice(int slice) {

        if (imageA.getNDims() == 4) {

            if (tSlice < imageA.getExtents()[3]) {
                tSlice = slice;
                updateImages(true);
            }
        } else if ((imageB != null) && (imageB.getNDims() == 4)) {

            if (tSlice < imageB.getExtents()[3]) {
                tSlice = slice;
                updateImages(true);
            }
        } else {
            return;
        }
    }


    /**
     * Set the update win-level flag from the plane render mouse drag, release events.
     *
     * @param  flag  true update window level, false not update.
     */
    public void setWindlevelUpdate(boolean flag) {
        updateWinlevel = flag;
    }

    /**
     * Invoke the SURFACEFAST rendering mode.
     */
    public void SURFASTMode() {
        setRenderMode(ViewJComponentRenderImage.ModeSURFACEFAST);
        lightControl.refreshLighting();
        updateImages(true);
    }

    /**
     * Invoke the SURFACE rendering mode.
     */
    public void SURMode() {
        setRenderMode(ViewJComponentRenderImage.ModeSURFACE);
        lightControl.refreshLighting();
        updateImages(true);
    }

    /**
     * Tells the mouse dialog that the transform has changed. Does not use the type parameter.
     *
     * @param  type       Type of transform.
     * @param  transform  Transform that was changed to.
     */
    public void transformChanged(int type, Transform3D transform) {

        if (mouseReleased == true) {
            return;
        }

        currentTransType = type;
        currentTransform = transform;

        if (MouseBehaviorCallback.ROTATE == type) {
            isRotate = true;
            if ( componentImageXY.updateView(type, transform) )
            {
                MeasureTime();
                UpdateFrameCount();
            }
        } else {
            objTransXY.setTransform(transform);
        }
    }


    /**
     * Tells the mouse dialog that the transform has changed. Does not use the type parameter.
     *
     * @param  type       Type of transform.
     * @param  transform  Transform that was changed to.
     */
    public void transformUpdate(int type, Transform3D transform) {

        if (mouseReleased == true) {
            return;
        }

        if ((MouseBehaviorCallback.ROTATE == type) && parallelRotation && (transform != null)) {
            isRotate = true;
            if ( componentImageXY.updateView(type, transform) )
            {
                MeasureTime();
                UpdateFrameCount();
            }
            processMouseReleased(null);
        }
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     *
     * @return  Confirms successful update.
     */
    public boolean updateImage() {

        if (componentImageXY == null) {
            return false;
        }

        m_kImageComponent.set(componentImageXY.getImage());

        return true;
    }

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. The extents on this image have
     * changed, so the extents need to be read in again and menus, panes and slide bars adjusted accordingly.
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen - fastest of the three update methods.
     *
     * @return  Confirms successful update.
     */
    public final boolean updateImages() {
        return updateImages(false);
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     *
     * @param   forceShow  Forces show to reimport image and calculate java image.
     *
     * @return  Confirms successful update.
     */
    public final boolean updateImages(boolean forceShow) {
        return updateImages(null, null, forceShow, 0);
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen.
     *
     * @param   LUTa        LUT used to update imageA.
     * @param   LUTb        LUT used to update imageB.
     * @param   forceShow   Forces show to reimport image and calculate java image.
     * @param   interpMode  Image interpolation method (Nearest or Smooth).
     *
     * @return  Confirms successful update.
     */
    public final boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean forceShow, int interpMode) {

        if ((componentImageXY == null) || !updateWinlevel) {
            return false;
        }

        JPanelClip clipDialog = surfaceRender.getClipDialog();
        componentImageXY.setXBoundNeg(clipDialog.getBoundXInv());
        componentImageXY.setYBoundNeg(clipDialog.getBoundYInv());
        componentImageXY.setZBoundNeg(clipDialog.getBoundZInv());
        componentImageXY.setXBoundPos(clipDialog.getBoundXPos());
        componentImageXY.setYBoundPos(clipDialog.getBoundYPos());
        componentImageXY.setZBoundPos(clipDialog.getBoundZPos());

        if (componentImageXY.show(tSlice, LUTa, LUTb, forceShow, true) == true) {
            m_kImageComponent.set(componentImageXY.getImage());
        } else {
            return false;
        }

        return true;
    }

    /**
     * Setup the specified set of lights to use for rendering.
     */
    public void updateLighting() {
        m_kSoftwareLightSet.setModelLights(lightControl.getSoftwareLightsModel());
        m_kSoftwareLightSet.setWorldLights(lightControl.getSoftwareLightsWorld());
        componentImageXY.updateLighting(m_kSoftwareLightSet, m_kSoftwareMaterial);
    }

    /**
     * Called by the volume surface render to update the mouse zoom transform. Set the tranform matrix to scale matrix.
     *
     * @param  transform  Zoom transform.
     */
    public synchronized void updateScale(Transform3D transform) {
        Transform3D trans3d = new Transform3D(transform);
        Matrix3d matrix = new Matrix3d();

        trans3d.get(matrix);
        matrix.m01 = 0.0d;
        matrix.m02 = 0.0d;
        matrix.m10 = 0.0d;
        matrix.m12 = 0.0d;
        matrix.m20 = 0.0d;
        matrix.m21 = 0.0d;
        trans3d.setRotation(matrix);
        objTransXY.setTransform(trans3d);
        trans3d = null;
        matrix = null;
    }

    /**
     * Called by the surface volume render to update the tranlation behavior. Set the transform matrix to translate
     * matrix.
     *
     * @param  transform  Translate tranform
     */
    public synchronized void updateTranslate(Transform3D transform) {
        Transform3D trans3d = new Transform3D(transform);
        Matrix3d matrix = new Matrix3d();

        trans3d.get(matrix);
        matrix.m01 = 0.0d;
        matrix.m10 = 0.0d;
        matrix.m20 = 0.0d;
        matrix.m21 = 0.0d;
        trans3d.setRotation(matrix);
        objTransXY.setTransform(trans3d);
        trans3d = null;
        matrix = null;
    }

    /**
     * Cleans memory.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

    /**
     * Initialize the renderer.
     *
     * @param  _pBar   ViewJProgressBar
     * @param  canvas  VolumeCanvas3D
     */
    protected void init(ViewJProgressBar _pBar, VolumeCanvas3D canvas) {
        rotationControlPanel = new JPanelCamera(this);
        lightControl = surfaceRender.getSurfaceDialog().getLightDialog();

        _pBar.updateValueImmed(35);
        createImageSceneGraph();
        _pBar.updateValueImmed(45);

        universe = new SimpleUniverse(canvas);

        // This will move the ViewPlatform back a bit so the
        // objects in the scene can be viewed.
        universe.getViewingPlatform().setNominalViewingTransform();
        objRootBG.compile();
        universe.addBranchGraph(objRootBG);
        setLocation(500, 300);
        setVisible(true);
        canvas.update(canvas.getGraphics());

        // Rendering the surface volume.
        _pBar.updateValueImmed(50);

        rotationAxis = ViewJFrameRenderCamera.Y_AXIS;
    }

    /**
     * Constructs main frame structures for 3 image planes. Makes the LUT if necessary, then sets up the buffer arrays
     * appropriately and calls the constructors for the three image planes.
     */
    private void configureFrame() {

        if (imageA == null) {
            return;
        }

        // if not a color image and LUTa is null then make a LUT
        if (imageA.isColorImage() == false) {
            int[] dimExtentsLUT = new int[2];

            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            if (LUTa == null) {
                LUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

                float min, max;

                if (imageA.getType() == ModelStorageBase.UBYTE) {
                    min = 0;
                    max = 255;
                } else if (imageA.getType() == ModelStorageBase.BYTE) {
                    min = -128;
                    max = 127;
                } else {
                    min = (float) imageA.getMin();
                    max = (float) imageA.getMax();
                }

                float imgMin = (float) imageA.getMin();
                float imgMax = (float) imageA.getMax();

                LUTa.resetTransferLine(min, imgMin, max, imgMax);
            }

            if ((imageB != null) && (LUTb == null)) {
                LUTb = new ModelLUT(ModelLUT.HOTMETAL, 256, dimExtentsLUT);

                float min, max;

                if (imageB.getType() == ModelStorageBase.UBYTE) {
                    min = 0;
                    max = 255;
                } else if (imageB.getType() == ModelStorageBase.BYTE) {
                    min = -128;
                    max = 127;
                } else {
                    min = (float) imageB.getMin();
                    max = (float) imageB.getMax();
                }

                float imgMin = (float) imageB.getMin();
                float imgMax = (float) imageB.getMax();

                LUTb.resetTransferLine(min, imgMin, max, imgMax);
            }
        }

        maxRenExtent = xDim;

        if (yDim > maxRenExtent) {
            maxRenExtent = yDim;
        }

        int bufferFactor = imageA.isColorImage() ? 4 : 1;
        pixBufferA_XY = new int[bufferFactor * maxRenExtent * maxRenExtent];

        if (imageB != null) {
        	bufferFactor = imageB.isColorImage() ? 4 : 1;
            pixBufferB_XY = new int[bufferFactor * maxRenExtent * maxRenExtent];
        }

        if ((imageA.getNDims() == 4) || ((imageB != null) && (imageB.getNDims() == 4))) {
            tSlice = (tDim - 1) / 2;
        } else {
            tSlice = 0;
        }

        setBounds(200, 200, 450, 570);

        canvas = new VolumeCanvas3D(config);
        canvas.addMouseMotionListener(this);
        canvas.addMouseListener(this);
    }


    /**
     * Creates the scene graph, made up of a branch group parent, a transform group under that which applies mouse
     * behavior and lights to the scene, and three branch groups under the transform group for each of the three image
     * planes. The surfaces that can be added would * be children of the transform group. Each image plane has a
     * transform group associated with it * and beneath that, a box shape where the texture maps are applied. The shape
     * is what is actually * displayed.
     */
    private void createImageSceneGraph() {

        // Create a Transformgroup to scale all objects so they
        // appear in the scene.
        Transform3D t3d = new Transform3D();

        t3d.setScale(0.75f);

        sceneRootTG.setTransform(t3d);
        sceneRootTG.setCapability(Group.ALLOW_CHILDREN_READ);
        sceneRootTG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        sceneRootTG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        sceneRootTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        sceneRootTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        sceneRootTG.setCapability(TransformGroup.ENABLE_PICK_REPORTING);

        BranchGroup pointBG = new BranchGroup();

        pointBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        pointBG.setCapability(Group.ALLOW_CHILDREN_READ);
        pointBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        pointBG.setCapability(BranchGroup.ALLOW_DETACH);

        Appearance appearance = new Appearance();

        appearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);

        TransparencyAttributes tap = new TransparencyAttributes();

        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
        tap.setTransparencyMode(TransparencyAttributes.BLENDED);
        tap.setTransparency(1.0f);
        appearance.setTransparencyAttributes(tap);

        Shape3D pointShape = new Shape3D(new PointArray(1, 1), appearance);

        pointBG.addChild(pointShape);
        sceneRootTG.addChild(pointBG);

        objXYPlane = new BranchGroup();
        objXYPlane.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objXYPlane.setCapability(Group.ALLOW_CHILDREN_READ);
        objXYPlane.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objXYPlane.setCapability(BranchGroup.ALLOW_DETACH);
        objRootBG.addChild(objXYPlane);

        objTransXY = new TransformGroup();
        objTransXY.setTransform(t3d);
        objTransXY.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        objTransXY.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        objTransXY.setCapability(TransformGroup.ENABLE_PICK_REPORTING);
        objTransXY.setCapability(Group.ALLOW_CHILDREN_READ);
        objTransXY.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objTransXY.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objXYPlane.addChild(objTransXY);

        objBoxSliceX = new BranchGroup();
        objBoxSliceX.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objBoxSliceX.setCapability(Group.ALLOW_CHILDREN_READ);
        objBoxSliceX.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objBoxSliceX.setCapability(BranchGroup.ALLOW_DETACH);

        objTransSliceX = new TransformGroup();
        objTransSliceX.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        objTransSliceX.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        objTransSliceX.setCapability(TransformGroup.ENABLE_PICK_REPORTING);
        objTransSliceX.setCapability(Group.ALLOW_CHILDREN_READ);
        objTransSliceX.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objTransSliceX.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objBoxSliceX.addChild(objTransSliceX);

        createTexturedImage();

        objBehaviorBG = new BranchGroup();
        objBehaviorBG.setCapability(BranchGroup.ALLOW_DETACH);
        objBehaviorBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);

        // Create the rotate behavior node
        mouseBehaviorRotate = new MouseRotate(canvas, sceneRootTG);
        mouseBehaviorRotate.setupCallback(this);
        mouseBehaviorRotate.setSchedulingBounds(bounds);
        mouseBehaviorRotate.setFactor(0.005);
        mouseBehaviorRotate.setEnable(true);
        objBehaviorBG.addChild(mouseBehaviorRotate);

        sceneRootTG.addChild(objBehaviorBG);

        mouseBehaviorZoom = new MouseZoom(canvas, objTransXY);

        mouseBehaviorZoom.setupCallback(this);
        mouseBehaviorZoom.setSchedulingBounds(bounds);
        mouseBehaviorZoom.setFactor(0.005);
        objTransXY.addChild(mouseBehaviorZoom);

        mouseBehaviorTranslate = new MouseTranslate(canvas, objTransXY);

        mouseBehaviorTranslate.setupCallback(this);
        mouseBehaviorTranslate.setSchedulingBounds(bounds);
        mouseBehaviorTranslate.setFactor(0.005);
        objTransXY.addChild(mouseBehaviorTranslate);
    }

    /**
     * createTexturedImage is added so that the size of the texture used to display the rendered image on the
     * texture-mapped polygon can be changed dynamically. Instead of recreating the scene graph, this function
     * re-initializes the texture-size dependent components of the scene graph and can be accessed dynamically through
     * the user-interface.
     */
    private void createTexturedImage() {

        // Box for XY Plane
        xBox = maxRenExtent;
        yBox = maxRenExtent;
        maxBox = maxRenExtent;

        // Normalize the size. - might not need this
        xBox = xBox / maxBox;
        yBox = yBox / maxBox;

        double dX0 = -xBox;
        double dY0 = -yBox;
        double dX1 = xBox;
        double dY1 = yBox;

        // Setup appearance attributes common for all slices.
        kDefaultAppearance = new Appearance();

        // Default appearance: (RenderingAttributes)
        // Do not waste time rendering when alpha is zero.
        RenderingAttributes kRenderingAttributes = new RenderingAttributes();

        kRenderingAttributes.setAlphaTestValue(0.0f);
        kRenderingAttributes.setAlphaTestFunction(RenderingAttributes.GREATER);
        kDefaultAppearance.setRenderingAttributes(kRenderingAttributes);

        // Default appearance: (PolygonAttributes)
        // Do not perform any backface/frontbace polygon culling
        // since we will want to render either side of a slice face.
        PolygonAttributes kPolygonAttributes = new PolygonAttributes();

        kPolygonAttributes.setCullFace(PolygonAttributes.CULL_NONE);
        kDefaultAppearance.setPolygonAttributes(kPolygonAttributes);

        // Default appearance: (Material)
        // Disable lighting so that the color information comes from
        // the texture maps.
        Material kMaterial = new Material();

        kMaterial.setLightingEnable(false);
        kDefaultAppearance.setMaterial(kMaterial);

        // Default appearance: (TransparencyAttributes)
        // Use blended transparency mode which has the default blending
        // operation set to alpha_src*src + (1-alpha_src)*dst.  This works
        // only for back to front ordering.
        TransparencyAttributes kTransparencyAttr = new TransparencyAttributes();

        kDefaultAppearance.setTransparencyAttributes(kTransparencyAttr);
        kTransparencyAttr.setTransparency(1.0f);
        kTransparencyAttr.setTransparencyMode(TransparencyAttributes.BLENDED);

        // Default appearance: (TextureAttributes)
        // Use Replace mode because we don't want to have to worry about
        // what color the slice is rendered as before the texture is
        // applied to it.
        TextureAttributes kTextureAttr = new TextureAttributes();

        kDefaultAppearance.setTextureAttributes(kTextureAttr);
        kTextureAttr.setTextureMode(TextureAttributes.REPLACE);

        // Default appearance: (Texture)
        // Allow the Texture attribute to be modified.
        kDefaultAppearance.setCapability(Appearance.ALLOW_TEXTURE_WRITE);

        // Default appearance: (TexCoordGeneration)
        // Allow the TexCoordGeneration attribute to be modified.
        kDefaultAppearance.setCapability(Appearance.ALLOW_TEXGEN_WRITE);
        kDefaultAppearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ);
        kDefaultAppearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);

        Appearance kAppearance = (Appearance) kDefaultAppearance.cloneNodeComponent(true);

        QuadArray kGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_2);

        kGeometry.setCoordinate(0, new Point3d(dX0, dY0, 0));
        kGeometry.setCoordinate(1, new Point3d(dX1, dY0, 0));
        kGeometry.setCoordinate(2, new Point3d(dX1, dY1, 0));
        kGeometry.setCoordinate(3, new Point3d(dX0, dY1, 0));

        Vector4f m_kCoordMapX = new Vector4f((xBox) / 2.0f, 0.0f, 0.0f, 0.5f);
        Vector4f m_kCoordMapY = new Vector4f(0.0f, (yBox) / 2.0f, 0.0f, 0.5f);
        TexCoordGeneration kTexCoordGeneration = new TexCoordGeneration(TexCoordGeneration.OBJECT_LINEAR,
                                                                        TexCoordGeneration.TEXTURE_COORDINATE_2,
                                                                        m_kCoordMapX, m_kCoordMapY);

        m_kImage = componentImageXY.getImage();
        m_kImageComponent = new ImageComponent2D(ImageComponent.FORMAT_RGBA, maxRenExtent, maxRenExtent);
        m_kImageComponent.setCapability(ImageComponent2D.ALLOW_IMAGE_WRITE);
        m_kImageComponent.setCapability(ImageComponent2D.ALLOW_IMAGE_READ);
        m_kImageComponent.setCapability(ImageComponent2D.ALLOW_SIZE_READ);
        m_kImageComponent.setCapability(ImageComponent2D.ALLOW_FORMAT_READ);
        m_kImageComponent.set(m_kImage);

        m_kTextureXY = new Texture2D(Texture.BASE_LEVEL, Texture.RGBA, maxRenExtent, maxRenExtent);
        m_kTextureXY.setEnable(true);
        m_kTextureXY.setMinFilter(Texture.BASE_LEVEL_LINEAR);
        m_kTextureXY.setMagFilter(Texture.BASE_LEVEL_LINEAR);
        m_kTextureXY.setBoundaryModeS(Texture.CLAMP_TO_EDGE);
        m_kTextureXY.setBoundaryModeT(Texture.CLAMP_TO_EDGE);
        m_kTextureXY.setImage(0, m_kImageComponent);
        m_kTextureXY.setCapability(Texture2D.ALLOW_IMAGE_WRITE);
        m_kTextureXY.setCapability(Texture2D.ALLOW_IMAGE_READ);
        m_kTextureXY.setCapability(Texture2D.ALLOW_ENABLE_WRITE);

        kAppearance.setTexture(m_kTextureXY);
        kAppearance.setTexCoordGeneration(kTexCoordGeneration);

        // Create the shape (geometry+appearance) for this slice.
        // Allow the appearance to be read.
        kShape = new Shape3D(kGeometry, kAppearance);
        kShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        kShape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        kShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);

        /* If the shapeBG member variable is not null, then this is not the
         * first time the function has been called, so detach it from the
         * scene graph before adding the changed version: */
        if (shapeBG != null) {
            shapeBG.detach();
        }

        shapeBG = new BranchGroup();
        shapeBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        shapeBG.setCapability(Group.ALLOW_CHILDREN_READ);
        shapeBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        shapeBG.setCapability(BranchGroup.ALLOW_DETACH);
        shapeBG.addChild(kShape);
        objTransXY.addChild(shapeBG);

        createBoxFrame(xBox, yBox, zBox);

        boxSliceX = new ViewJComponentBoxSlice(0, yBox, zBox, ViewJComponentBoxSlice.X_SLICE);

        Shape3D shape = new Shape3D(boxSliceX, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        /* If the shapeSliceBG member variable is not null, then this is not
         * the first time the function has been called, so detach it from the
         * scene graph before adding the changed version: */
        if (shapeSliceBG != null) {
            shapeSliceBG.detach();
        }

        shapeSliceBG = new BranchGroup();
        shapeSliceBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        shapeSliceBG.setCapability(Group.ALLOW_CHILDREN_READ);
        shapeSliceBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        shapeSliceBG.setCapability(BranchGroup.ALLOW_DETACH);
        shapeSliceBG.addChild(shape);
        objTransSliceX.addChild(shapeSliceBG);

        kRenderingAttributes = null;
        kPolygonAttributes = null;
        kMaterial = null;
        kTransparencyAttr = null;
        kTextureAttr = null;
        kAppearance = null;
        kGeometry = null;
        m_kCoordMapX = null;
        m_kCoordMapY = null;
        kTexCoordGeneration = null;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Ray cast Scene State.
     *
     * <p>Title:</p>
     *
     * <p>Description:</p>
     *
     * <p>Copyright: Copyright (c) 2003</p>
     *
     * <p>Company:</p>
     *
     * @author   not attributable
     * @version  1.0
     */
    class SceneState {

        /** DOCUMENT ME! */
        public int x, y, z;

        /** DOCUMENT ME! */
        public boolean xVisible, yVisible, zVisible;

        /**
         * Creates a new SceneState object.
         *
         * @param  x         DOCUMENT ME!
         * @param  y         DOCUMENT ME!
         * @param  z         DOCUMENT ME!
         * @param  xVisible  DOCUMENT ME!
         * @param  yVisible  DOCUMENT ME!
         * @param  zVisible  DOCUMENT ME!
         */
        public SceneState(int x, int y, int z, boolean xVisible, boolean yVisible, boolean zVisible) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.xVisible = xVisible;
            this.yVisible = yVisible;
            this.zVisible = zVisible;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public String toString() {
            return ("SceneState[" + x + "," + y + "," + z + "," + xVisible + "," + yVisible + "," + zVisible + "]");
        }

    }

    private boolean m_bTestFrameRate = false;
    /** Framerate Performance parameters: */
    protected double m_dLastTime = -1.0f, m_dAccumulatedTime = 0.0f, m_dFrameRate = 0.0f;
    /** Framerate Performance parameters: */
    protected int m_iFrameCount = 0, m_iAccumulatedFrameCount = 0, m_iTimer = 30, m_iMaxTimer = 30;

    /** Resets time */
    public void ResetTime ()
    {
        m_dLastTime = -1.0f;
    }
    
    /** Measure time */
    protected void MeasureTime ()
    {
        Date kDate = new Date();
        // start performance measurements
        if (m_dLastTime == -1.0)
        {
            m_dLastTime = kDate.getTime() / 1000.0;
            m_dAccumulatedTime = 0.0;
            //m_dFrameRate = 0.0;
            m_iFrameCount = 0;
            m_iAccumulatedFrameCount = 0;
            m_iTimer = m_iMaxTimer;
        }

        // accumulate the time only when the miniature time allows it
        if (--m_iTimer == 0)
        {
            double dCurrentTime = kDate.getTime() / 1000.0;
            double dDelta = dCurrentTime - m_dLastTime;
            m_dLastTime = dCurrentTime;
            m_dAccumulatedTime += dDelta;
            m_iAccumulatedFrameCount += m_iFrameCount;
            m_iFrameCount = 0;
            m_iTimer = m_iMaxTimer;
            m_dFrameRate = m_iAccumulatedFrameCount/m_dAccumulatedTime;

            System.err.println( "FPS: " + m_dFrameRate );
            ResetTime();
        }
        kDate = null;
    }

    /** Update frame count */
    protected void UpdateFrameCount ()
    {
        m_iFrameCount++;
    }

    public void SetTestFrameRate( boolean bTest )
    {
        m_bTestFrameRate = bTest;
    }
    
    public boolean GetTestFrameRate( )
    {
        return m_bTestFrameRate;
    }

    
    
}
