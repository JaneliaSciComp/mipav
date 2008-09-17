package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import com.sun.j3d.utils.behaviors.mouse.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.event.*;

import javax.vecmath.*;


/**
 * Frame that holds the surface renderer. This frame is only possible to activate if a three dimensional image is
 * loaded. The image shows up in the frame as the three planes, with an axial view, a coronal view, and a sagittal view.
 * The user can slide these planes up and down and can turn them on and off. The user can also load in surfaces created
 * from the original image. These 3D surfaces will appear in the proper place within the three image planes. There are
 * many options for viewing the surfaces. Additionally, the user can change the view mode, so that the mouse causes the
 * view to "fly". The user can then record the different mouse actions and play them back.
 *
 * @author  Matthew J. McAuliffe, Ph.D.
 * @see     ViewJComponentSurfaceImage
 * @see     JDialogSurface
 * @see     JDialogView
 * @see     JDialogMouseRecorder
 */
public class VolumeRendererRayCast extends VolumeRenderer implements MouseMotionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7864871507214707172L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Makes a frame and puts the three image planes into it. Creates the scene graph which dictates the behavior of the
     * image planes and surfaces. Initializes the surface dialog and the mouse recorder dialog, so that this original
     * view is saved. When the user opens these dialogs, they have already been created; they are just set to visible.
     *
     * @param  _imageA        First image to display, cannot be null.
     * @param  _LUTa          LUT of the imageA (if null grayscale LUT is constructed).
     * @param  _imageB        Second loaded image, may be null.
     * @param  _LUTb          LUT of the imageB, may be null.
     * @param  surfaceRender  Surface-based renderer
     * @param  _config        Graphics configuration
     * @param  _pBar          volume render progress bar reference.
     */
    public VolumeRendererRayCast(ModelImage _imageA, ModelLUT _LUTa, ModelImage _imageB, ModelLUT _LUTb,
                                 SurfaceRender surfaceRender, GraphicsConfiguration _config, ViewJProgressBar _pBar) {
        super(_imageA, _LUTa, _imageB, _LUTb, surfaceRender, _config, _pBar);


        componentImageXY = new ViewJComponentRenderImageRayCast(this, imageA, LUTa, imageB, LUTb, pixBufferA_XY,
                                                                pixBufferB_XY, extents,
                                                                ViewJComponentRenderImage.ModeCOMPOSITE, maxRenExtent);

        optionsPanel = new JPanelRenderOptionsRayCast(this);
        _pBar.updateValueImmed(30);
        init(_pBar, canvas);
        componentImageXY.show(tSlice, null, null, true, true);
        _pBar.updateValueImmed(60);
        updateImage();
        _pBar.updateValueImmed(70);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Override the parent autoCapture method to capture MIP image volume.
     */
    public void autoCapture() {
        super.autoCapture();

        while (rotationTimes > 0) {
            Matrix4f m = new Matrix4f();

            m.setIdentity();

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

            synchronized (this) {
                transformChanged(MouseBehaviorCallback.ROTATE, transRotation);
                updateImages(true);
                writeImage();
            }

            rotationTimes--;
        }

        // write to camera capture frame.
        writeImage();
    }

    /**
     * Dispose memory.
     */
    public void disposeLocal() {

        if (optionsPanel != null) {
            ((JPanelRenderOptionsRayCast) optionsPanel).disposeLocal(true);
            optionsPanel = null;
        }
    }

    /**
     * Blur the result ray traced image or not.
     *
     * @param  flag  true bluring to reduce contract, false not bluring
     */
    public void setBlurFlag(boolean flag) {

        if (componentImageXY != null) {
            ((ViewJComponentRenderImageRayCast) componentImageXY).setBlurFlag(flag);
        }
    }

    /**
     * Set the vertex diffuse color.
     *
     * @param  color  Color
     */
    public void setVertexDiffuse(Color color) {

        if (componentImageXY != null) {
            ((ViewJComponentRenderImageRayCast) componentImageXY).setVertexDiffuseColor(color);
        }

    }

    /**
     * Set the vertex specular color.
     *
     * @param  color  Color
     */
    public void setVertexSpecular(Color color) {

        if (componentImageXY != null) {
            ((ViewJComponentRenderImageRayCast) componentImageXY).setVertexSpecularColor(color);
        }

    }

    /**
     * Sets how the image plane should be displayed depending on value of slider.
     *
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) { }


    /**
     * Call from the parent frame to dispose memory.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }
}
