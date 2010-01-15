package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Abstract class used for displaying images in the program MIPAV.
 *
 * @version  1.0 August 31, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public abstract class ViewJComponentRenderImage implements MouseMotionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** MIP rendering mode constant. */
    public static final int ModeMIP = 1;

    /** DRR rendering mode constant. */
    public static final int ModeXRAY = 2;

    /** SURFACE rendering mode constant. */
    public static final int ModeSURFACE = 3;

    /** COMPOSITE rendering mode constant. */
    public static final int ModeCOMPOSITE = 4;

    /** Surface faset rendering mode constant. */
    public static final int ModeSURFACEFAST = 5;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected int imageExtentMax;

    /** Lookup table for (intensity) image A. */
    protected ModelLUT LUTa;

    /** Lookup table for (intensity) image B. */
    protected ModelLUT LUTb;

    /** alphaBlending values for compositing two images. */
    protected float m_fAlphaBlend = 0.5f;

    /** DOCUMENT ME! */
    protected Color m_kBackgroundColor = Color.black;

    /** DOCUMENT ME! */
    protected BufferedImage m_kImage;

    /** Images to be renderer. */
    protected ModelImage m_kImageA;

    /** DOCUMENT ME! */
    protected ModelImage m_kImageB;

    /** DOCUMENT ME! */
    protected Dimension m_kImageDim;

    /** Manages the renderer data for each image. */
    protected RendererImageData m_kRendererImageDataA = null;

    /** DOCUMENT ME! */
    protected RendererImageData m_kRendererImageDataB = null;

    /** model of a RGB Table. */
    protected ModelRGB m_kRGBTA = null;

    /** DOCUMENT ME! */
    protected ModelRGB m_kRGBTB = null;

    /** Reference to the ShearWarpVolumeRenderer which hold this as an instance. */
    protected VolumeRenderer m_kVolumeRenderer;

    /** mode - used to describe the cursor mode. */
    protected int mode;

    /** Default ray tracing space size. */
    protected int raySpaceSize = 3;

    /** Default ray tracing step size. */
    protected int rayStepSize = 3;

    /** DOCUMENT ME! */
    protected Renderer rayTracerA;

    /** DOCUMENT ME! */
    protected Renderer rayTracerB;

    /** Buffer used to store ARGB images of the image presently being displayed. */
    protected int[] renBufferA = null;

    /** DOCUMENT ME! */
    protected int[] renBufferB = null;

    /** Default rending mode, MIP mode. */
    protected int renderingMode = ModeMIP;

    /** Time sliders value. */
    protected int timeSlice = 0;

    /** DOCUMENT ME! */
    protected int timeSliceA = 0;

    /** DOCUMENT ME! */
    protected int timeSliceB = 0;

    /** Current updated transform. */
    protected Transform3D transformBU = new Transform3D();

    /** Transform counter, used to reduce the rendering frequency. */
    protected int transformCounter = 0;

    /** buffer used for image to draw to display. */
    private int[] m_aiPaintBuffer = null;

    /** Axis angle vector. */
    private AxisAngle4f m_kAxisAngle;

    /** temporary variables to avoid 'new' calls. */
    private Vector3f m_kV0, m_kV1, m_kVCross;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  kVolumeRenderer  Access to renderer of which this is an instance.
     * @param  _imageA          Model of the image that will be displayed
     * @param  _LUTa            LUT used to display imageA
     * @param  _imageB          Model of the image that will be displayed
     * @param  _LUTb            LUT used to display imageB
     * @param  rvolBufferA      rendering buffer A
     * @param  rvolBufferB      rendering buffer B // Tagged for CHECKING DELETING
     * @param  extents          image dimension extents
     * @param  renderMode       rendering mode, MIP, DRR and SURFACE
     * @param  maxExtent        maximium extent value.
     */
    public ViewJComponentRenderImage(VolumeRenderer kVolumeRenderer, ModelImage _imageA, ModelLUT _LUTa,
                                     ModelImage _imageB, ModelLUT _LUTb, int[] rvolBufferA, int[] rvolBufferB,
                                     int[] extents, int renderMode, int maxExtent) {

        m_kVolumeRenderer = kVolumeRenderer;
        m_kImage = new BufferedImage(maxExtent, maxExtent, BufferedImage.TYPE_INT_ARGB);
        m_kImageDim = new Dimension(maxExtent, maxExtent);
        m_kImageA = _imageA;
        m_kImageB = _imageB;
        m_kRendererImageDataA = new RendererImageData(m_kImageA);

        if (null != m_kImageB) {
            m_kRendererImageDataB = new RendererImageData(m_kImageB);
        }

        LUTa = _LUTa;
        LUTb = _LUTb;

        renBufferA = rvolBufferA;
        renBufferB = rvolBufferB;

        // compute the maximum of the extents which can be used
        // for such things as computing the distance of the eye
        // from the center of the model for viewing
        imageExtentMax = extents[0];

        if (imageExtentMax < extents[1]) {
            imageExtentMax = extents[1];
        }

        if (imageExtentMax < extents[2]) {
            imageExtentMax = extents[2];
        }

        setRenderMode(renderMode);

        // For track ball motion.  The boolean variable is used to keep
        // track of mouse drags while a button is pressed.  The float
        // variable is used for the virtual track ball, a unit sphere that
        // is centered at the origin.  Integer mouse coordinates are
        // mapped to [-1,1]^2 using the float variable.
        m_kV0 = new Vector3f();
        m_kV1 = new Vector3f();
        m_kVCross = new Vector3f();
        m_kAxisAngle = new AxisAngle4f();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Change the camera model.
     *
     * @param  renderMode  rendering mode.
     */
    public abstract void setRenderMode(int renderMode);

    /**
     * Clean memory.
     *
     * @param  flag  is true call the super.disposeLocal
     */
    public void disposeLocal(boolean flag) {

        if (m_kImage != null) {
            m_kImage.flush();
            m_kImage = null;
        }

        m_kImageDim = null;

        m_kRGBTA = null;
        m_kRGBTB = null;
        m_aiPaintBuffer = null;

        m_kImageA = null;
        m_kImageB = null;
        m_kRendererImageDataA = null;
        m_kRendererImageDataB = null;

        renBufferA = null;
        renBufferB = null;

        LUTa = null;
        LUTb = null;

        m_kVolumeRenderer = null;
        transformBU = null;

        if (m_kImageA != null) {
            m_kImageA = null;
        }

        if (m_kImageB != null) {
            m_kImageB = null;
        }

        if (rayTracerA != null) {
            rayTracerA.disposeLocal();
            rayTracerA = null;
        }

        if (rayTracerB != null) {
            rayTracerB.disposeLocal();
            rayTracerB = null;
        }
    }

    /**
     * Note that alphaBlending is applied with 1 component taken as zero if both components are not present -for
     * example, if either imageA or imageB but not both has red, then the red component is alphaBlended with zero.
     *
     * @param  aiRenBufferA  DOCUMENT ME!
     * @param  aiRenBufferB  DOCUMENT ME!
     */
    public void drawImages(int[] aiRenBufferA, int[] aiRenBufferB) {

        // nothing to do if there is no A image to render
        if (null == aiRenBufferA) {
            return;
        }

        // reallocate the display buffer if the size changes
        if ((null == m_aiPaintBuffer) || (m_aiPaintBuffer.length != aiRenBufferA.length)) {
            m_aiPaintBuffer = new int[aiRenBufferA.length];
        }

        if (aiRenBufferB == null) {

            for (int index = 0; index < aiRenBufferA.length; index++) {
                m_aiPaintBuffer[index] = 0xff000000 | aiRenBufferA[index];
            }
        } else {
            float fAlphaPrime = 1.0f - m_fAlphaBlend;

            for (int index = 0; index < aiRenBufferA.length; index++) {

                int Ra = ((aiRenBufferA[index] & 0x00ff0000) >> 16);
                int Ga = ((aiRenBufferA[index] & 0x0000ff00) >> 8);
                int Ba = ((aiRenBufferA[index] & 0x000000ff));
                int Rb = ((aiRenBufferB[index] & 0x00ff0000) >> 16);
                int Gb = ((aiRenBufferB[index] & 0x0000ff00) >> 8);
                int Bb = ((aiRenBufferB[index] & 0x000000ff));

                Ra = (int) ((Ra * m_fAlphaBlend) + (Rb * fAlphaPrime));
                Ga = (int) ((Ga * m_fAlphaBlend) + (Gb * fAlphaPrime));
                Ba = (int) ((Ba * m_fAlphaBlend) + (Bb * fAlphaPrime));

                m_aiPaintBuffer[index] = 0xff000000 | ((int) Ra << 16) | ((int) Ga << 8) | (int) Ba;
            }
        }

        m_kImage.setRGB(0, 0, m_kImageDim.width, m_kImageDim.height, m_aiPaintBuffer, 0, m_kImageDim.width);
    }

    /**
     * Accessor that returns amount of alpha blending between two images.
     *
     * @return  DOCUMENT ME!
     */
    public float getalphaBlend() {
        return m_fAlphaBlend;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public AxisAngle4f getAxisAngle() {
        return m_kAxisAngle;
    }


    /**
     * Return access to the current eye point in world coordinates.
     *
     * @return  Point3f current eye point in world coordinates.
     */
    public Point3f getEyePoint() {
        return rayTracerA.getEyePoint();
    }

    /**
     * Gets the Java image.
     *
     * @return  Java image
     *
     * @see     Image
     */
    public BufferedImage getImage() {
        return m_kImage;
    }

    /**
     * Returns the imageA.
     *
     * @return  imageA Model image A.
     */
    public ModelImage getImageA() {
        return m_kImageA;
    }

    /**
     * Returns the imageB.
     *
     * @return  imageB Model image B.
     */
    public ModelImage getImageB() {
        return m_kImageB;
    }

    /**
     * Accessor that returns the model lut for the imageA.
     *
     * @return  LUTa the model LUT for imageA
     */
    public ModelLUT getLUTa() {
        return LUTa;
    }

    /**
     * Accessor that returns the model lut for the imageB.
     *
     * @return  LUTb the model LUT for imageB
     */
    public ModelLUT getLUTb() {
        return LUTb;
    }

    /**
     * Change the camera model.
     *
     * @return  DOCUMENT ME!
     */
    public boolean getParallel() {
        return (rayTracerA.getParallel());
    }

    /**
     * Accessor that returns the reference to rayTracerA.
     *
     * @return  DOCUMENT ME!
     */
    public Renderer getRayTracerA() {
        return rayTracerA;
    }

    /**
     * Get the rendering mode.
     *
     * @return  renderingMode MIP, DRR and SUR
     */
    public int getRenderMode() {
        return (renderingMode);
    }

    /**
     * Accessor that returns the ModelRGB RGBTA for imageA.
     *
     * @return  DOCUMENT ME!
     */
    public ModelRGB getRGBTA() {
        return m_kRGBTA;
    }

    /**
     * Accessor that returns the ModelRGB for imageB.
     *
     * @return  DOCUMENT ME!
     */
    public ModelRGB getRGBTB() {
        return m_kRGBTB;
    }

    /**
     * Get the raytrace region space size.
     *
     * @return  raySpaceSize space size.
     */
    public int getSpaceSize() {
        return raySpaceSize;
    }

    /**
     * Get raytrace step size.
     *
     * @return  rayStepSize raytrace step size.
     */
    public int getStepSize() {
        return rayStepSize;
    }


    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  the mouse event generated by a mouse press
     */
    public void mouseClicked(MouseEvent kEvent) { /* stub */
    }

    /**
     * One of the overrides necessary to be a MouseMotionListener. This member handles rotating the virtual track ball
     * during the dragging of the mouse after a mouse-press event but before a mouse-release event. Each drag event
     * causes the virtual track ball to be rotated. Each such rotation causes a ray trace to occur, but the trace is
     * generated at low resolution.
     *
     * @param  kEvent  the event generated by the mouse dragging
     */
    public void mouseDragged(MouseEvent kEvent) { }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  the mouse event generated by a mouse press
     */
    public void mouseEntered(MouseEvent kEvent) { /* stub */
    }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  the mouse event generated by a mouse press
     */
    public void mouseExited(MouseEvent kEvent) { /* stub */
    }

    /**
     * One of the overrides necessary to be a MouseMotionListener. This member only exists to satisfy the conditions of
     * being a MouseMotionListener. It does nothing when invoked.
     *
     * @param  kEvent  the event generated by a mouse movement
     */
    public void mouseMoved(MouseEvent kEvent) { /* stub */
    }


    /**
     * One of the overrides necessary to be a MouseListener. When a mouse button is pressed, the application is starting
     * a drag operation to rotate the virtual track ball. The initial mouse location is recorded for use by
     * 'moveTrackBall', stored as a point (x0,y0) in [-1,1]^2.
     *
     * @param  kEvent  the mouse event generated by a mouse press
     */
    public void mousePressed(MouseEvent kEvent) { }

    /**
     * One of the overrides necessary to be a MouseListener. When a mouse button is released, the application is
     * finishing a drag operation to rotate the virtual track ball. The final mouse location is recorded for use by
     * 'moveTrackBall', stored as a point (x1,y1) in [-1,1]^2. The virtual track ball is moved into its final position
     * and a ray trace is performed at highest resolution.
     *
     * @param  kEvent  the mouse event generated by a mouse press
     */
    public void mouseReleased(MouseEvent kEvent) {

        // trace( 1, 1 );
        rayTracerA.rotateFrameBy(transformBU);

        if (null != rayTracerB) {
            rayTracerB.rotateFrameBy(transformBU);
        }

        show(timeSliceA, null, null, true, true);
    }

    /**
     * A virtual track ball. This method generates a rotation matrix from (x0,y0) and (x1,y1), two points in [-1,1]^2.
     * If either point is outside the unit circle, it is projected onto the unit circle. The corresponding sphere points
     * (x0,y0,z0) and (x1,y1,z1) are computed. The cross product is used as the axis of rotation. The angle between the
     * vectors is used as the angle of rotation. The rotation matrix is used to rotate the oriented bounding box of the
     * 3D image.
     *
     * @param   fX0  DOCUMENT ME!
     * @param   fY0  DOCUMENT ME!
     * @param   fX1  DOCUMENT ME!
     * @param   fY1  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean moveTrackBall(float fX0, float fY0, float fX1, float fY1) {
        // (x0,y0) and (x1,y1) are in [-1,1]^2.  The track ball is the unit sphere centered at the origin.

        if ((fX0 == fX1) && (fY0 == fY1)) {

            // nothing to rotate
            return false;
        }

        // generate (x0,y0,z0) on track ball
        float fLength = (float) Math.sqrt((fX0 * fX0) + (fY0 * fY0));
        float fInvLength;

        if (fLength > 1.0f) {

            // outside unit disk, project onto it
            fInvLength = 1.0f / fLength;
            m_kV0.x = fInvLength * fX0;
            m_kV0.y = fInvLength * fY0;
            m_kV0.z = 0.0f;
        } else {

            // compute point (x0,y0,z0) on negative unit hemisphere
            m_kV0.x = fX0;
            m_kV0.y = fY0;
            m_kV0.z = 1.0f - (fX0 * fX0) - (fY0 * fY0);
            m_kV0.z = ((m_kV0.z <= 0.0f) ? 0.0f : (float) -Math.sqrt(m_kV0.z));
        }

        // generate (x1,y1,z1) on track ball
        fLength = (float) Math.sqrt((fX1 * fX1) + (fY1 * fY1));

        if (fLength > 1.0f) {

            // outside unit disk, project onto it
            fInvLength = 1.0f / fLength;
            m_kV1.x = fInvLength * fX1;
            m_kV1.y = fInvLength * fY1;
            m_kV1.z = 0.0f;
        } else {

            // compute point (x1,y1,z1) on negative unit hemisphere
            m_kV1.x = fX1;
            m_kV1.y = fY1;
            m_kV1.z = 1.0f - (fX1 * fX1) - (fY1 * fY1);
            m_kV1.z = ((m_kV1.z <= 0.0f) ? 0.0f : (float) -Math.sqrt(m_kV1.z));
        }

        // compute axis of rotation
        m_kVCross.cross(m_kV0, m_kV1);
        fLength = m_kVCross.length();

        if (fLength > 1e-06f) {
            fInvLength = 1.0f / fLength;
            m_kVCross.scale(fInvLength);
        } else {

            // rotated pi radians
            fLength = (float) Math.sqrt((m_kV0.x * m_kV0.x) + (m_kV0.y * m_kV0.y));
            fInvLength = 1.0f / fLength;
            m_kVCross.x = m_kV0.y * fInvLength;
            m_kVCross.y = -m_kV0.x * fInvLength;
            m_kVCross.z = 0.0f;
        }

        // compute angle of rotation
        float fAngle = (float) Math.acos(m_kV0.dot(m_kV1));

        // rotate the world box, normalize to avoid creeping numerical error
        m_kAxisAngle.set(m_kVCross, fAngle);
        rayTracerA.rotateBy(m_kAxisAngle);

        if (rayTracerB != null) {
            rayTracerB.rotateBy(m_kAxisAngle);
        }

        return true;
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  iValue  int amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setAlphaBlend(int iValue) {
        m_fAlphaBlend = iValue / 100.0f;
    }

    /**
     * Sets the background color for the frame and rendered image.
     *
     * @param  color  Color RGBA color to use as the background color.
     */
    public void setBackgroundColor(Color color) {
        m_kBackgroundColor = color;

        if (null != rayTracerA) {
            rayTracerA.setBackgroundColor(color);
        }

        if (null != rayTracerB) {
            rayTracerB.setBackgroundColor(color);
        }
    }

    /**
     * Sets component's ImageA.
     *
     * @param  image  DOCUMENT ME! assumes dimensionality same as image B's for now
     */
    public void setImageA(ModelImage image) {
        m_kImageA = image;
        // setZoom(1,1); // sets zoom
    }

    /**
     * Sets component's ImageB !!!!!! assumes dimensionality same as image A's for now will fix soon.
     *
     * @param  image  imageB
     */
    public void setImageB(ModelImage image) {
        m_kImageB = image;

        if (m_kImageB == null) { }
    }

    /**
     * Accessor that sets the model LUT for the imageA.
     *
     * @param  LUT  the model LUT
     */
    public void setLUTa(ModelLUT LUT) {
        LUTa = LUT;
    }

    /**
     * Accessor that sets the model LUTb for the imageB.
     *
     * @param  LUT  the model LUT
     */
    public void setLUTb(ModelLUT LUT) {
        LUTb = LUT;
    }

    /**
     * Updates the BufferedImage, Dimension, and render buffers when the render target image size changes so that the
     * image can be redisplayed at the new target resolution.
     *
     * @param  iMaxExtent   DOCUMENT ME!
     * @param  rvolBufferA  DOCUMENT ME!
     * @param  rvolBufferB  DOCUMENT ME!
     */
    public void setMaxExtent(int iMaxExtent, int[] rvolBufferA, int[] rvolBufferB) {

        if (m_kImage != null) {
            m_kImage.flush();
            m_kImage = null;
        }

        if (m_kImageDim != null) {
            m_kImageDim = null;
        }

        /* Allocate the new BufferedImage and Dimension with the new MaxExtent
         * size: */
        m_kImage = new BufferedImage(iMaxExtent, iMaxExtent, BufferedImage.TYPE_INT_ARGB);
        m_kImageDim = new Dimension(iMaxExtent, iMaxExtent);

        /* store the new image buffers: */
        renBufferA = rvolBufferA;
        renBufferB = rvolBufferB;
    }

    /**
     * Change the camera model.
     *
     * @param  bParallel  true for a parallel camera, false for a perspective camera
     */
    public void setParallel(boolean bParallel) {

        if (rayTracerA != null) {
            rayTracerA.setParallel(bParallel);
        }

        if (rayTracerB != null) {
            rayTracerB.setParallel(bParallel);
        }
    }

    /**
     * The following 2 functions set the RGB tables for ARGB images A and B. Sets the RGB table for ARGB image A
     *
     * @param  kRGBT  RGB table
     */
    public void setRGBTA(ModelRGB kRGBT) {
        m_kRGBTA = kRGBT;
    }

    /**
     * Sets the RGB table for ARGB image B.
     *
     * @param  kRGBT  RGB table
     */
    public void setRGBTB(ModelRGB kRGBT) {
        m_kRGBTB = kRGBT;
    }

    /**
     * Set the raytrace region space size.
     *
     * @param  spaceSize  region space size.
     */
    public void setSpaceSize(int spaceSize) {
        raySpaceSize = spaceSize;
    }

    /**
     * Set the raytrace step size.
     *
     * @param  stepSize  step size
     */
    public void setStepSize(int stepSize) {
        rayStepSize = stepSize;
    }

    /**
     * Setup the X Negative clipping plane position.
     *
     * @param  value  position of the X negative clip slider.
     */
    public void setXBoundNeg(float value) {

        if (null != rayTracerA) {
            rayTracerA.setXBoundNeg(value);
        }

        if (null != rayTracerB) {
            rayTracerB.setXBoundNeg(value);
        }
    }

    /**
     * Setup the X positive clipping plane position.
     *
     * @param  value  position of the X positive clip slider.
     */
    public void setXBoundPos(float value) {

        if (null != rayTracerA) {
            rayTracerA.setXBoundPos(value);
        }

        if (null != rayTracerB) {
            rayTracerB.setXBoundPos(value);
        }
    }

    /**
     * Setup the Y Negative clipping plane position.
     *
     * @param  value  position of the Y negative clip slider.
     */
    public void setYBoundNeg(float value) {

        if (null != rayTracerA) {
            rayTracerA.setYBoundNeg(value);
        }

        if (null != rayTracerB) {
            rayTracerB.setYBoundNeg(value);
        }
    }

    /**
     * Setup the Y positive clipping plane position.
     *
     * @param  value  positin of the Y positve clip slider.
     */
    public void setYBoundPos(float value) {

        if (null != rayTracerA) {
            rayTracerA.setYBoundPos(value);
        }

        if (null != rayTracerB) {
            rayTracerB.setYBoundPos(value);
        }
    }

    /**
     * Setup the Z negative clipping plane position.
     *
     * @param  value  position of the Z negative clip slider.
     */
    public void setZBoundNeg(float value) {

        if (null != rayTracerA) {
            rayTracerA.setZBoundNeg(value);
        }

        if (null != rayTracerB) {
            rayTracerB.setZBoundNeg(value);
        }
    }

    /**
     * Setup the Z positive clipping plane position.
     *
     * @param  value  position of the Z positive clip slider.
     */
    public void setZBoundPos(float value) {

        if (null != rayTracerA) {
            rayTracerA.setZBoundPos(value);
        }

        if (null != rayTracerB) {
            rayTracerB.setZBoundPos(value);
        }
    }

    /**
     * shows the image and the VOI(s).
     *
     * @param   tSlice        t (time) slice to show
     * @param   _LUTa         LUTa - to change to new LUT for imageA else null
     * @param   _LUTb         LUTb - to change to new LUT for imageB else null
     * @param   forceShow     forces this method to import image and recalculate java image
     * @param   bQualityHigh  forces a call to trace(1,1) instead of using lower res sampling
     *
     * @return  boolean to indicate if the show was successful
     */
    public synchronized boolean show(int tSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow,
                                     boolean bQualityHigh) {

        if (rayTracerA == null) {
            return false;
        }

        // update the time slice
        boolean bUpdatedTimeSlice = (timeSlice != tSlice);

        if (bUpdatedTimeSlice) {
            timeSlice = tSlice;

            if (m_kImageA.getNDims() < 4) {
                timeSliceA = 0;
            } else {
                timeSliceA = timeSlice;
            }

            if ((m_kImageB != null) && (m_kImageB.getNDims() < 4)) {
                timeSliceB = 0;
            } else {
                timeSliceB = timeSlice;
            }
        }

        // update the data for image A renderer
        if (null != rayTracerA) {

            // intensity-based color mapped image
            if (!m_kImageA.isColorImage()) {
                boolean bUpdatedLut = false;

                if (_LUTa != null) {
                    LUTa = _LUTa;
                    bUpdatedLut = true;
                }

                if (!m_kRendererImageDataA.updateRenderer(rayTracerA, LUTa,
                                                              ((ViewJComponentVolOpacity)
                                                                       ((JPanelVolOpacity)
                                                                                (m_kVolumeRenderer.getVolOpacity()))
                                                                       .getCompA()).getOpacityTransferFunction(),
                                                              ((ViewJComponentVolOpacity)
                                                                       ((JPanelVolOpacity)
                                                                                (m_kVolumeRenderer.getVolOpacity()))
                                                                       .getCompA_GM()), timeSliceA, bUpdatedTimeSlice,
                                                              bUpdatedLut, forceShow)) {
                    return false;
                }
            } else { // ARGB image

                if (!m_kRendererImageDataA.updateRenderer(rayTracerA, m_kRGBTA,
                						((JPanelVolOpacityRGB) (m_kVolumeRenderer.getVolOpacity())),
                                 		timeSliceA, bUpdatedTimeSlice, forceShow)) {
                    return false;
                }
            }
        }

        // update the data for image B renderer
        if ((null != m_kImageB) && (null != rayTracerB)) {

            // intensity-based color mapped image
            if (!m_kImageB.isColorImage()) {
                boolean bUpdatedLut = false;

                if (_LUTb != null) {
                    LUTb = _LUTb;
                    bUpdatedLut = true;
                }

                if (!m_kRendererImageDataB.updateRenderer(rayTracerB, LUTb,
                                                              ((ViewJComponentVolOpacity)
                                                                       ((JPanelVolOpacity)
                                                                                (m_kVolumeRenderer.getVolOpacity()))
                                                                       .getCompB()).getOpacityTransferFunction(),
                                                              ((ViewJComponentVolOpacity)
                                                                       ((JPanelVolOpacity)
                                                                                (m_kVolumeRenderer.getVolOpacity()))
                                                                       .getCompA_GM()), timeSliceB, bUpdatedTimeSlice,
                                                              bUpdatedLut, forceShow)) {
                    return false;
                }
            } // ARGB image
            else {

                if (!m_kRendererImageDataB.updateRenderer(rayTracerB, m_kRGBTB,
                                      ((JPanelVolOpacityRGB) (m_kVolumeRenderer.getVolOpacity())),                      
                                                   timeSliceB, bUpdatedTimeSlice, forceShow)) {
                    return false;
                }
            }
        }

        if (bQualityHigh) {
            trace(1, 1);
        } else {
            trace(rayStepSize, raySpaceSize);
        }

        drawImages(renBufferA, (null != rayTracerB) ? renBufferB : null);

        return true;
    }

    /**
     * Setup the specified set of lights to use for rendering.
     *
     * @param   kLightSet  SoftwareLightSet Set of world/model lights.
     * @param   kMaterial  SoftwareMaterial Set of default material properties to use when applying the lighting.
     *
     * @return  boolean True if the update of lighting triggers a need to redraw the images.
     */
    public boolean updateLighting(SoftwareLightSet kLightSet, SoftwareMaterial kMaterial) {

        boolean bLighting = rayTracerA.usesNormals();
        rayTracerA.setLighting(kLightSet, kMaterial);

        if (null != rayTracerB) {
            rayTracerB.setLighting(kLightSet, kMaterial);
            bLighting = bLighting || rayTracerB.usesNormals();
        }

        return bLighting;
    }

    /**
     * Update transform from the surface volume render.
     *
     * @param  transform  Transform3D
     */
    public void updateTransform(Transform3D transform) {
        transformBU = transform;
        rayTracerA.rotateFrameBy(transform);

        if (null != rayTracerB) {
            rayTracerB.rotateFrameBy(transform);
        }
    }

    /**
     * Call by the surface volume render to update transform changes.
     *
     * @param  type       DOCUMENT ME!
     * @param  transform  Transform3D
     */

    /**
     * Call by the surface volume render to update transform changes. Composite the slice trace by the specified space
     * size.
     *
     * @param  type       Transform type, current not used.
     * @param  transform  Transform3D
     */
    public synchronized boolean updateView(int type, Transform3D transform) {

        transformBU = transform;

        if (transformCounter < 2) {
            transformCounter++;

            return false;
        }

        transformCounter = 0;

        updateTransform(transform);
        show(timeSliceA, null, null, false, false);
        return true;
    }

    /**
     * Ray trace by step size.
     *
     * @param  rayStepSize   interp step size
     * @param  raySpaceSize  DOCUMENT ME!
     */
    protected abstract void trace(int rayStepSize, int raySpaceSize);

    /**
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

}
