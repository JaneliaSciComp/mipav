package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.utils.universe.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import java.io.*;

import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * This is an abstract class. This class defines the basic functionality of the renders, such as the LUT and bounding
 * box and image rotation, etc. It was extended by the surface render, surface plotter and the volume render.
 *
 * @version  0.1 Oct 1, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public abstract class RenderViewBase extends VolumeCanvas3D
        implements ViewImageUpdateInterface, ActionListener, MouseBehaviorCallback, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2913636735677876386L;

    /** Display mode image A. */
    public static final int IMAGE_A = 0;

    /** Display mode image B. */
    public static final int IMAGE_B = 1;

    /** Set the zero vector. */
    private static final Vector3f m_kZeroVector = new Vector3f(0.0f, 0.0f, 0.0f);

    /** Image volume size and x, y, z extents. */
    private static int m_iNumVoxels, m_iSizeX, m_iSizeY, m_iSizeZ;

    /** normal vector arrayes. */
    private static Vector3f[] akNormal;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * A BoundingSphere that contains the entire scene. This bound is used for all scheduling, including lighting and
     * mouse behaviors.
     */
    public BoundingSphere bounds;

    /** The background node for the canvas. */
    protected Background background;

    /** The outside box frame. */
    protected ViewJComponentBoxFrame boxFrame;

    /** The Canvas3D object on which the surfaces are drawn and where the picking happens. */
    protected VolumeCanvas3D canvas;

    /** Camera snapshot frame. */
    protected ViewJFrameRenderCamera captureFrame = null;

    /** The resulting ModelImage of the camera snapshot. */
    protected ModelImage captureImage = null;

    /** The config used to extends the Canvas3D class. */
    protected GraphicsConfiguration config;

    /** Indicates which image is to be acted upon when two images are displayed. */
    protected int displayMode = IMAGE_A;

    /** Reference to the image A of this frame. */
    protected ModelImage imageA = null;

    /** Reference to the image Bof this frame. */
    protected ModelImage imageB = null;

    /** Reference to LUT for image A. */
    protected ModelLUT LUTa;

    /** Reference to LUT for image B. */
    protected ModelLUT LUTb;

    /** The BranchGroup to which the standard behaviors are attached, zoom and rotate. */
    protected BranchGroup objBehaviorBG;

    /** Parent of the whole box frame. */
    protected BranchGroup objBoxFrameBG;

    /**
     * The BranchGroup root of the scene managed by the simple universe. The root has a single child, a TransformGroup,
     * that manages all of the actual scene objects.
     */
    protected BranchGroup objRootBG;

    /** Rotation angle when the camera takes snapshot. */
    protected int rotationAngle;

    /** Rotation axis when the camera takes snapshot. */
    protected int rotationAxis;

    /** Camera control dialog box. */
    protected JPanelCamera rotationControlPanel;

    /** Number of rotation during the camera auto snapshotting. */
    protected int rotationTimes;

    /** The total rotation degree, fixing one bug. */
    protected int rotationTotal = 0;

    /**
     * A TransformGroup object to which all the scene objects are attached, including 3D images, surface, behavior, and
     * lights.
     */
    protected TransformGroup sceneRootTG;

    /** transform rotation of the volume image during camera snapshot. */
    protected Transform3D transRotation;

    /** Rotation matrix. */
    protected Transform3D transRotationMatrix;

    /** Triplanar view branch group, which hold the three plane view and the slice box frame view. */
    protected BranchGroup triPlanarViewBG;

    /** The SimpleUniverse object which is the parent of everything else in the scene. */
    protected SimpleUniverse universe;

    /** View dialog associated with surface plotter or renderer. */
    protected JPanelView viewPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Calls the constructor for ViewJFrameBase with the same parameters.
     *
     * @param  _imageA  The current image A.
     * @param  _imageB  The current image B, or <code>null</code> if there isn't one.
     * @param  _config  The graphics config to use when rendering
     */
    public RenderViewBase(ModelImage _imageA, ModelImage _imageB, GraphicsConfiguration _config) {
        super(_config);
        config = _config;
        imageA = _imageA;
        imageB = _imageB;
        displayMode = IMAGE_A;

        if (imageA.getNDims() == 3) {
            m_iSizeX = imageA.getExtents()[0];
            m_iSizeY = imageA.getExtents()[1];
            m_iSizeZ = imageA.getExtents()[2];
            m_iNumVoxels = m_iSizeX * m_iSizeY * m_iSizeZ;
            calcImageNormals();
        }
        // calcImageNormals();

        objRootBG = new BranchGroup();
        objRootBG.setCapability(BranchGroup.ALLOW_DETACH);
        objRootBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objRootBG.setCapability(Group.ALLOW_CHILDREN_READ);
        objRootBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        sceneRootTG = new TransformGroup();

        objBoxFrameBG = new BranchGroup();
        objBoxFrameBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objBoxFrameBG.setCapability(Group.ALLOW_CHILDREN_READ);
        objBoxFrameBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objBoxFrameBG.setCapability(BranchGroup.ALLOW_DETACH);
        objBoxFrameBG.setPickable(false);

        background = new Background(new Color3f(Color.black));
        bounds = new BoundingSphere(new Point3d(0.0f, 0.0f, 0.0f), 100.0f);

        background.setApplicationBounds(bounds);
        background.setCapability(Background.ALLOW_COLOR_WRITE);
        objRootBG.addChild(background);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Gets the scene state in terms of slices visible, which slice number, etc.
     *
     * @return  A private object extension specific to the class that extends this one.
     */
    public abstract Object getSceneState();

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed and (1-percentage) of Image B to
     *                be displayed
     */
    public abstract void setAlphaBlend(int value);

    /**
     * Used by the JPanelMouse the redisplay the recorded actions.
     *
     * @param  scene  Object
     */
    public abstract void setGUI(Object scene);

    /**
     * Set the RGB table of imageA. Method will be extended in renders.
     *
     * @param  RGBT  the rgb table to use for image A
     */
    public abstract void setRGBTA(ModelRGB RGBT);

    /**
     * Get the RGB table of imageB. Method will be extended in renders.
     *
     * @param  RGBT  the rgb table to use for image B
     */
    public abstract void setRGBTB(ModelRGB RGBT);


    /**
     * Get the normal vector arrays.
     *
     * @return  Vector3f[] normal vector array.
     */
    public static final Vector3f[] getNormals() {
        return akNormal;
    }

    /**
     * Calculate the rotation matrix when the auto camera caputuring the images.
     */
    public void autoCapture() {

        rotationTimes = 360 / rotationAngle;
        transRotation = new Transform3D();
        transRotationMatrix = new Transform3D();

        Matrix4d matrix = new Matrix4d();

        matrix.setIdentity();
        sceneRootTG.getTransform(transRotation);
    }

    /**
     * Create array of normal vectors corresponding to the voxels in the volume. The normal vector is computed based on
     * the gradient of the volume intensity values.
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f[] calcImageNormals() {

        ModelSimpleImage kValueImageA;
        float[] afData;

        // Extract image slice.
        ModelSimpleImage kSimpleImageA = new ModelSimpleImage(imageA, 0);

        // Convert to intensity valued image.
        if (imageA.isColorImage()) {
            kValueImageA = kSimpleImageA.createIntensityImage();
            afData = kValueImageA.data;
        } else {
            afData = kSimpleImageA.data;

        }

        // Access intensity values as a linear array.
        // Initially allocate all normal vectors as the zero vector.
        akNormal = new Vector3f[afData.length];

        Vector3f[] akNormalTmp = new Vector3f[afData.length];

        for (int i = 0; i < akNormalTmp.length; i++) {
            akNormal[i] = new Vector3f(0, 0, 0);
            akNormalTmp[i] = new Vector3f(0, 0, 0);
        }

        int iXBound = m_iSizeX;
        int iYBound = m_iSizeY;
        int iZBound = m_iSizeZ;
        int iXYBound = iXBound * iYBound;

        // normals from gradient which are computed using central finite
        // differences everywhere except forward/backward finite differences
        // are used at the edges

        float fDX = 0;
        float fDY = 0;
        float fDZ = 0;

        int iOffX = 1;
        int iOffY = iXBound;
        int iOffZ = iXBound * iYBound;
        int iX, iY, iZ;

        for (iZ = 1; iZ < (iZBound - 1); iZ++) {
            boolean bMinZ = 0 == iZ;
            boolean bMaxZ = (iZBound - 1) == iZ;

            for (iY = 1; iY < (iYBound - 1); iY++) {
                boolean bMinY = 0 == iY;
                boolean bMaxY = (iYBound - 1) == iY;
                int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 0; iX < iXBound; iX++) {
                    boolean bMinX = 0 == iX;
                    boolean bMaxX = (iXBound - 1) == iX;

                    int i = iX + offset;

                    fDX = (((bMinX ? afData[i] : afData[i - iOffX - iXBound]) -
                            (bMaxX ? afData[i] : afData[i + iOffX - iXBound])) * 0.71f) +

                    (bMinX ? afData[i] : afData[i - iOffX]) - (bMaxX ? afData[i] : afData[i + iOffX]) +
                          (

                    ((bMinX ? afData[i] : afData[i - iOffX + iXBound]) -
                     (bMaxX ? afData[i] : afData[i + iOffX + iXBound])) * 0.71f);

                    fDY = (((bMinY ? afData[i] : afData[i - iOffY - 1]) - (bMaxY ? afData[i] : afData[i + iOffY - 1])) *
                               0.71f) +

                    (bMinY ? afData[i] : afData[i - iOffY]) - (bMaxY ? afData[i] : afData[i + iOffY]) +
                          (

                    ((bMinY ? afData[i] : afData[i - iOffY + 1]) - (bMaxY ? afData[i] : afData[i + iOffY + 1])) * 0.71f);

                    fDZ = (((bMinZ ? afData[i] : afData[i - iOffZ - 1]) - (bMaxZ ? afData[i] : afData[i + iOffZ - 1])) *
                               0.71f) +

                    (bMinZ ? afData[i] : afData[i - iOffZ]) - (bMaxZ ? afData[i] : afData[i + iOffZ]) +
                          (

                    ((bMinZ ? afData[i] : afData[i - iOffZ + 1]) - (bMaxZ ? afData[i] : afData[i + iOffZ + 1])) * 0.71f);


                    if ((fDX != 0.0f) || (fDY != 0.0f) || (fDZ != 0.0f)) {
                        akNormalTmp[i] = new Vector3f(fDX, fDY, fDZ);
                    }
                }
            }
        }

        // Catch any zero-vector normals and replace them by an average of
        // neighboring normals.
        for (iZ = 1; iZ < (iZBound - 1); iZ++) {

            for (iY = 1; iY < (iYBound - 1); iY++) {
                int offset = iXBound * (iY + (iYBound * iZ));

                for (iX = 1; iX < (iXBound - 1); iX++) {
                    int i = iX + offset;
                    akNormal[i].add(akNormalTmp[i]);
                    akNormal[i].add(akNormalTmp[i - 1]);
                    akNormal[i].add(akNormalTmp[i + 1]);
                    akNormal[i].add(akNormalTmp[i - iXBound]);
                    akNormal[i].add(akNormalTmp[i + iXBound]);
                    akNormal[i].add(akNormalTmp[i - iXYBound]);
                    akNormal[i].add(akNormalTmp[i + iXYBound]);
                    akNormal[i].normalize();
                }
            }
        }

        for (int i = 0; i < akNormalTmp.length; i++) {
            akNormalTmp[i] = null;
        }

        akNormalTmp = null;
        kSimpleImageA = null;
        System.gc();

        return akNormal;
    }

    /**
     * Set the captureFrame to null.
     */
    public void disableCamera() {

        if (captureFrame != null) {

            // captureFrame.close();
            captureFrame = null;
        }
    }

    /**
     * Clean memory.
     */
    public void disposeLocal() {

        if (imageA != null) {
            imageA.removeImageDisplayListener(this);
        }

        if (imageB != null) {
            imageB.removeImageDisplayListener(this);
        }

        if ((imageA != null) && (imageA.getImageFrameVector() != null)) {

            if (imageA.getImageFrameVector().isEmpty()) {
                imageA.disposeLocal(); // Should this class be disposing the
                                       // image or should it be
                                       // ViewJFrameVolumeView?
                imageA = null;
            }
        }

        if ((imageB != null) && (imageB.getImageFrameVector() != null)) {

            if (imageB.getImageFrameVector().isEmpty()) {
                imageB.disposeLocal();
                imageB = null;
            }
        }

        objBehaviorBG = null;
        sceneRootTG = null;

        if (universe != null) {
            universe.removeAllLocales();
            universe = null;
        }

        canvas = null;
        objRootBG = null;
        background = null;
        objBoxFrameBG = null;
        boxFrame = null;

        if (akNormal != null) {

            for (int i = 0; i < akNormal.length; i++) {
                akNormal[i] = null;
            }
        }

        akNormal = null;

        if (viewPanel != null) {
            viewPanel.disposeLocal(true);
            viewPanel = null;
        }

        if (captureImage != null) {
            captureImage.disposeLocal();
            captureImage = null;
        }

        transRotation = null;
        transRotationMatrix = null;

        if (captureFrame != null) {
            captureFrame.close();
            captureFrame = null;
        }

        if (rotationControlPanel != null) {
            rotationControlPanel.disposeLocal(true);
            rotationControlPanel = null;
        }
    }

    /**
     * Accessor for the branch group that holds the mouse behavior.
     *
     * @return  The mouse behavior's branch group.
     */
    public BranchGroup getBehaviorGroup() {
        return objBehaviorBG;
    }

    /**
     * Accessor for the bounds sphere.
     *
     * @return  bounding sphere
     */
    public BoundingSphere getBound() {
        return bounds;
    }

    /**
     * Accessor for the branch group of this 3D object, the parent of the transform group.
     *
     * @return  The branch group parent of the scene.
     */
    public BranchGroup getBranchGroup() {
        return objRootBG;
    }

    /**
     * Accessor for the canvas for this 3D object.
     *
     * @return  The canvas for the scene.
     */
    public Canvas3D getCanvas() {
        return canvas;
    }

    /**
     * Accessor that returns the reference to image A.
     *
     * @return  Image A.
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Accessor that returns the reference to image B.
     *
     * @return  Image B.
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Get the RGB table of imageA. Method will be extended in renders.
     *
     * @return  rgb table for image A
     */
    public ModelRGB getRGBTA() {
        return null;
    }

    /**
     * Get the RGB table of imageB. Method will be extended in renders.
     *
     * @return  rgb table for image B
     */
    public ModelRGB getRGBTB() {
        return null;
    }

    /**
     * Accessor for the transform group of the scene, which is the parent of everything displayed.
     *
     * @return  The transform group of the scene.
     */
    public TransformGroup getSceneRootTG() {
        return sceneRootTG;
    }

    /**
     * Return the triplanar view branch group.
     *
     * @return  triPlanarViewBG the triplanar view branch group
     */
    public BranchGroup getTriPlanarViewBG() {
        return triPlanarViewBG;
    }

    /**
     * Accessor for the universe for this 3D object.
     *
     * @return  The universe for the scene.
     */
    public SimpleUniverse getUniverse() {
        return universe;
    }

    /**
     * Reset the image volume to the original position.
     */
    public void resetAxisX() {
        double rotationRedian = (90d / 180d) * (double) Math.PI;

        transRotation = new Transform3D();

        Matrix4d matrix = new Matrix4d();

        matrix.setIdentity();
        matrix.m11 = Math.cos(rotationRedian);
        matrix.m12 = -Math.sin(rotationRedian);
        matrix.m21 = Math.sin(rotationRedian);
        matrix.m22 = Math.cos(rotationRedian);
        transRotation.set(matrix);
        transRotation.setScale(0.45f);
        sceneRootTG.setTransform(transRotation);
    }

    /**
     * Reset the image volume to the original position.
     */
    public void resetAxisY() {
        double rotationRedian = (90d / 180d) * (double) Math.PI;

        transRotation = new Transform3D();

        Matrix4d matrix = new Matrix4d();

        matrix.setIdentity();
        matrix.m00 = Math.cos(rotationRedian);
        matrix.m02 = Math.sin(rotationRedian);
        matrix.m20 = -Math.sin(rotationRedian);
        matrix.m22 = Math.cos(rotationRedian);
        transRotation.set(matrix);
        transRotation.setScale(0.45f);
        sceneRootTG.setTransform(transRotation);
    }

    /**
     * Reset the image volume to the original position.
     */
    public void resetImage() {
        transRotation = new Transform3D();

        Matrix4d matrix = new Matrix4d();

        matrix.setIdentity();
        transRotation.set(matrix);
        transRotation.setScale(0.45f);
        sceneRootTG.setTransform(transRotation);
    }

    /**
     * Rotates the image volume when the camera manually takes snapshot.
     */
    public void rotateImage() {
        transRotation = new Transform3D();
        transRotationMatrix = new Transform3D();

        Matrix4d matrix = new Matrix4d();

        matrix.setIdentity();

        // setup the rotation matrix.
        sceneRootTG.getTransform(transRotation);

        // This rotation is along the world x, y, z axis, not along the local x,
        // y, z axis
        Matrix4f m = new Matrix4f();
        Vector3f rotAxis = new Vector3f();
        Vector3f point = new Vector3f();

        transRotation.get(m);
        point.x = m.m03;
        point.y = m.m13;
        point.z = m.m23;

        if (rotationAxis == ViewJFrameRenderCamera.X_AXIS) {
            rotAxis.x = 1;
            rotAxis.y = 0;
            rotAxis.z = 0;
        } else if (rotationAxis == ViewJFrameRenderCamera.Y_AXIS) {
            rotAxis.x = 0;
            rotAxis.y = 1;
            rotAxis.z = 0;
        } else if (rotationAxis == ViewJFrameRenderCamera.Z_AXIS) {
            rotAxis.x = 0;
            rotAxis.y = 0;
            rotAxis.z = 1;
        } else if (rotationAxis == ViewJFrameRenderCamera.NO_AXIS) {
            return;
        }

        Matrix me = new Matrix();

        me.element[0][0] = m.m00;
        me.element[0][1] = m.m01;
        me.element[0][2] = m.m02;
        me.element[0][3] = m.m03;
        me.element[1][0] = m.m10;
        me.element[1][1] = m.m11;
        me.element[1][2] = m.m12;
        me.element[1][3] = m.m13;
        me.element[2][0] = m.m20;
        me.element[2][1] = m.m21;
        me.element[2][2] = m.m22;
        me.element[2][3] = m.m23;
        me.element[3][0] = m.m30;
        me.element[3][1] = m.m31;
        me.element[3][2] = m.m32;
        me.element[3][3] = m.m33;

        me.rotate(point, rotAxis, rotationAngle);

        m.m00 = me.element[0][0];
        m.m01 = me.element[0][1];
        m.m02 = me.element[0][2];
        m.m03 = me.element[0][3];
        m.m10 = me.element[1][0];
        m.m11 = me.element[1][1];
        m.m12 = me.element[1][2];
        m.m13 = me.element[1][3];
        m.m20 = me.element[2][0];
        m.m21 = me.element[2][1];
        m.m22 = me.element[2][2];
        m.m23 = me.element[2][3];
        m.m30 = me.element[3][0];
        m.m31 = me.element[3][1];
        m.m32 = me.element[3][2];
        m.m33 = me.element[3][3];

        transRotation.set(m);
        transformChanged(MouseBehaviorCallback.ROTATE, transRotation);
        sceneRootTG.setTransform(transRotation);
    }

    /**
     * Sets the color of the background.
     *
     * @param  color  Color to set to.
     */
    public void setBackgroundColor(Color color) {
        background.setColor(new Color3f(color));
    }

    /**
     * Sets the color of the box frame.
     *
     * @param  color  Color to set to.
     */
    public void setBoxColor(Color color) {
        boxFrame.setColor(color);
    }

    /**
     * Set the rotation angle with the camera takes snapshot.
     *
     * @param  value  angle value
     */
    public void setRotationAngle(int value) {
        rotationAngle = value;
    }

    /**
     * Set the roation axis ( X, Y, Z Axis ) when the camera takes snapshot.
     *
     * @param  axis  X_AXIS, Y_AXIS or Z_AXIS.
     */
    public void setRotationAxis(int axis) {
        rotationAxis = axis;
    }

    /**
     * Write the image from the rendering frame into the camera capture frame.
     *
     * @return  succeed <code>true</code> means succeed, <code>false</code> means unsucceed.
     */
    public boolean writeImage() {
        int[] pixels;
        int bufferSize, xDim, yDim;
        short[] buffer = null;
        Robot robot;

        Dimension d = new Dimension();
        Point p = new Point();

        p.x = 0;
        p.y = 0;
        SwingUtilities.convertPointToScreen(p, canvas);

        d.width = canvas.getWidth();
        d.height = canvas.getHeight();

        Rectangle currentRectangle = new Rectangle(p, d);

        // using Robot to capture image rectangle and transfering image into
        // the pixel buffer.
        try {
            robot = new Robot();

            Image imagePix = robot.createScreenCapture(currentRectangle);

            xDim = currentRectangle.width;
            yDim = currentRectangle.height;
            xDim = xDim - (xDim % 4);
            yDim = yDim - (yDim % 4);
            bufferSize = 4 * xDim * yDim;
            pixels = new int[xDim * yDim];

            PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, xDim, yDim, pixels, 0, xDim);

            pgTest.grabPixels();
            imagePix = null;
            pgTest = null;
        } catch (InterruptedException e) {
            Preferences.debug("Interrupted waiting for pixels!");

            return false;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("ViewFrameImage: unable to allocate enough memory for RGB image");

            return false;
        } catch (AWTException error) {
            MipavUtil.displayError("Platform doesn't support screen capture.");

            return false;
        }

        try {
            int[] extents = new int[3];

            extents[0] = xDim; // RGB
            extents[1] = yDim;
            extents[2] = 1;
            captureImage = new ModelImage(ModelStorageBase.ARGB, extents, "Screen capture");
            captureImage.unRegisterImage();
            buffer = new short[bufferSize];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("JDialogScreenCapture: unable to allocate enough memory for RGB image");

            return false;
        }

        int i, k;

        for (i = 0, k = 0; i < (xDim * yDim); i++, k += 4) {
            buffer[k] = (short) (255); // alpha
            buffer[k + 1] = (short) ((pixels[i] >> 16) & 0xFF); // Red
            buffer[k + 2] = (short) ((pixels[i] >> 8) & 0xFF); // Green
            buffer[k + 3] = (short) (pixels[i] & 0xFF); // Blue
        }

        // porting buffer into the resulting captureImage.
        try {
            captureImage.importData(0, buffer, true);
        } catch (IOException error) {
            MipavUtil.displayError("JDialogScreenCapture: Problems grabbing image!");
        }

        captureImage.getFileInfo()[0].setPhotometric((short) 2); // Indicates
                                                                 // RGB tiff
                                                                 // file
                                                                 // format
        pixels = null;
        buffer = null;
        robot = null;

        if (captureFrame == null) {
            captureFrame = new ViewJFrameRenderCamera(captureImage, this);
        } else {
            captureFrame.addImage(captureImage);
        }

        return true;
    }

    /**
     * Called when the camera button is clicked. This method brings up the camera control dialog.
     */
    public void writeImageAuto() {

        if (captureFrame != null) {
            disableCamera();
        }

        if (rotationControlPanel == null) {
            rotationControlPanel = new JPanelCamera(this);
        } else {
            rotationControlPanel.setVisible(true);
        }
    }

    /**
     * Creates a new box frame with the given dimensions.
     *
     * @param  xBox  X dimension of box frame.
     * @param  yBox  Y dimension of box frame.
     * @param  zBox  Z dimension of box frame.
     */
    protected void createBoxFrame(float xBox, float yBox, float zBox) {
        TransformGroup objTransBoxTG = new TransformGroup();

        objTransBoxTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        objTransBoxTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        objTransBoxTG.setCapability(Group.ALLOW_CHILDREN_READ);
        objTransBoxTG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objTransBoxTG.setCapability(TransformGroup.ENABLE_PICK_REPORTING);
        objBoxFrameBG.addChild(objTransBoxTG);

        Shape3D shape;

        boxFrame = new ViewJComponentBoxFrame(xBox, yBox, zBox);
        shape = new Shape3D(boxFrame, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        objTransBoxTG.addChild(shape);

        /*
         * Check added for when creatBoxFrame is called more than once (when the Render Target Image size changes).
         * sceneRootTG should only be added if it is not already in the scene graph.
         */
        if (objRootBG.indexOfChild(sceneRootTG) == -1) {
            objRootBG.addChild(sceneRootTG);
        }
    }

    /**
     * Calls finalize.
     *
     * @throws  Throwable  if a problem is encountered

    protected void finalize() throws Throwable {
        this.disposeLocal();
        super.finalize();
    }
     */
    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * This matrix class is used for 3D object rotate along a specifid axis. Also, the rotation is based on the 3D
     * object's location.
     */
    public class Matrix {

        /** Matrix element array. */
        public float[][] element;

        /**
         * Constructor to create identity matrix.
         */
        public Matrix() {
            element = new float[4][4];
            this.reset();
        }

        /**
         * Inverse the matrix this.m = inverse(m);
         *
         * @return  Matrix inverse matrix
         */
        public Matrix inverse() {
            Matrix m = new Matrix();

            // from row 1 to row 4
            for (int j = 0; j < 4; j++) {

                // normalize the diagonal element
                float e = element[j][j];

                for (int i = 0; i < 4; i++) {
                    element[i][j] = element[i][j] / e;
                    m.element[i][j] = m.element[i][j] / e;
                }

                // make other element to be 0

                for (int k = 0; k < 4; k++) {

                    if (k != j) {
                        e = -element[j][k];

                        for (int i = 0; i < 4; i++) {
                            element[i][k] = element[i][k] + (e * element[i][j]);
                            m.element[i][k] = m.element[i][k] + (e * m.element[i][j]);
                        }
                    }
                }
            }

            return m;
        }

        /**
         * Matrix multiplication, row based. this.m = this.m * m
         *
         * @param  m  Matrix
         */
        public void multiply(Matrix m) {
            float[][] e = new float[4][4];

            for (int i = 0; i < 4; ++i) {

                for (int j = 0; j < 4; ++j) {

                    for (int k = 0; k < 4; ++k) {
                        e[i][j] += element[k][j] * m.element[i][k];
                    }
                }
            }

            for (int i = 0; i < 4; ++i) {

                for (int j = 0; j < 4; ++j) {
                    element[i][j] = e[i][j];
                }
            }
        }

        /**
         * Print out the matrix.
         */
        public void print() {

            for (int j = 0; j < 4; j++) {
                System.out.println(element[0][j] + " " + element[1][j] + " " + element[2][j] + " " + element[3][j]);
            }
        }

        /**
         * Identity matrix setup.
         */
        public void reset() {

            for (int i = 0; i < 4; ++i) {

                for (int j = 0; j < 4; ++j) {

                    if (i == j) {
                        element[i][j] = 1;
                    } else {
                        element[i][j] = 0;
                    }
                }
            }
        }

        /**
         * rotate along a given axis, point and rotation angle.
         *
         * @param  point  Vector3f rotation axis
         * @param  axis   Vector3f rotation reference point
         * @param  angle  in degree.
         */
        public void rotate(Vector3f point, Vector3f axis, int angle) {
            float sinx, cosx; // angle rotate along x axis
            float siny, cosy; // angle rotate along y axis
            float yz_m; // the magnitude of the yz component of axis
            float m; // total magnitude of the axis vector

            Matrix matrix = new Matrix();

            yz_m = (float) Math.sqrt((axis.y * axis.y) + (axis.z * axis.z));

            if (yz_m == 0) {
                sinx = 0;
                cosx = 1;
            } else {
                sinx = axis.y / yz_m;
                cosx = axis.z / yz_m;
            }

            // m = axis.magnitude();
            m = axis.lengthSquared();

            if (m == 0) {
                siny = 0;
                cosy = 1;
            } else {
                siny = -axis.x / m;
                cosy = yz_m / m;
            }

            // first map the origin to the point, then rotate z axis around x
            // axis
            // (up/down), then rotate z axix around y axis (left/right), then
            // this map the z axix to be the axix vector given, then rotate
            // around
            // this z axix to the angle given.

            Vector3f origin = new Vector3f(point);
            Vector3f originNeg = new Vector3f();

            originNeg.x = -origin.x;
            originNeg.y = -origin.y;
            originNeg.z = -origin.z;
            matrix.translate(originNeg);
            matrix.rotatex(sinx, cosx);
            matrix.rotatey(siny, cosy);
            matrix.rotatez(angle);

            // reverse the above process

            matrix.rotatey(-siny, cosy);
            matrix.rotatex(-sinx, cosx);
            matrix.translate(point);

            this.multiply(matrix);
        }

        /**
         * rotate along x axis with given rotation angle in degree.
         *
         * @param  degree  int
         */
        public void rotatex(int degree) {
            float d;
            float cos, sin;

            d = (float) (degree * Math.PI / 180.);
            sin = (float) Math.sin(d);
            cos = (float) Math.cos(d);

            rotatex(sin, cos);
        }

        /**
         * rotate along x axis with given sin, cos value of the rotatino angle.
         *
         * @param  sin  float
         * @param  cos  float
         */
        public void rotatex(float sin, float cos) {
            Matrix m = new Matrix();

            m.element[1][1] = cos;
            m.element[2][2] = cos;
            m.element[1][2] = -sin;
            m.element[2][1] = sin;

            this.multiply(m);
        }

        /**
         * rotate along y axis with given rotation angle in degree.
         *
         * @param  degree  int
         */
        public void rotatey(int degree) {
            float d;
            float cos, sin;

            d = (float) (degree * Math.PI / 180.);
            sin = (float) Math.sin(d);
            cos = (float) Math.cos(d);

            rotatey(sin, cos);
        }

        /**
         * Rotate along the y axis with given sin, cos value of the rotation angle.
         *
         * @param  sin  float
         * @param  cos  float
         */
        public void rotatey(float sin, float cos) {
            Matrix m = new Matrix();

            m.element[0][0] = cos;
            m.element[2][2] = cos;
            m.element[0][2] = sin;
            m.element[2][0] = -sin;

            this.multiply(m);
        }

        /**
         * rotate along z axis with given rotation angle in degree.
         *
         * @param  degree  int
         */
        public void rotatez(int degree) {
            float d;
            float cos, sin;

            d = (float) (degree * Math.PI / 180.);
            sin = (float) Math.sin(d);
            cos = (float) Math.cos(d);

            rotatez(sin, cos);
        }

        /**
         * Rotate along the z axis with given sin, cos value of the rotation angle.
         *
         * @param  sin  float
         * @param  cos  float
         */
        public void rotatez(float sin, float cos) {
            Matrix m = new Matrix();

            m.element[0][0] = cos;
            m.element[1][1] = cos;
            m.element[0][1] = -sin;
            m.element[1][0] = sin;

            this.multiply(m);
        }

        /**
         * Scale the matrix along the diagonal.
         *
         * @param  f  scale factor.
         */
        public void scale(float f) {
            Matrix m = new Matrix();

            for (int i = 0; i < 4; ++i) {
                element[i][i] = f;
            }

            this.multiply(m);
        }

        /**
         * Translate the matrix to the given vector postion.
         *
         * @param  vec  Vector3f
         */
        public void translate(Vector3f vec) {
            translate(vec.x, vec.y, vec.z);
        }

        /**
         * Translate the matrix to the specified x, y, z postion.
         *
         * @param  x  float
         * @param  y  float
         * @param  z  float
         */
        public void translate(float x, float y, float z) {
            Matrix m = new Matrix();

            m.element[0][3] = x;
            m.element[1][3] = y;
            m.element[2][3] = z;

            this.multiply(m);
        }
    }

    /** Slider events used by the mouse recorder. */
    protected MouseEventVector sliderEvents;
    
    public MouseEventVector getSliderEvents()
    {
        return sliderEvents;
    }
    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void recordMouse(String name, JPanelMouse myMouseDialog, int mouseMode ) {
        Transform3D t3D = new Transform3D();
        // get the view
        getSceneRootTG().getTransform(t3D);
        double[] mat = new double[16];
        t3D.get(mat);
        // store name and view together
        sliderEvents = new MouseEventVector(name, mat, myMouseDialog.first,
                getSceneState(),
                mouseMode);
    }
}
