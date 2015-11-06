package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import com.sun.j3d.utils.behaviors.mouse.*;

import java.awt.*;
import java.awt.event.*;

import javax.media.j3d.*;

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
public class VolumeRendererShearWarp extends VolumeRenderer implements MouseMotionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3304622158293691309L;

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
    public VolumeRendererShearWarp(ModelImage _imageA, ModelLUT _LUTa, ModelImage _imageB, ModelLUT _LUTb,
                                   SurfaceRender surfaceRender, GraphicsConfiguration _config, ViewJProgressBar _pBar) {
        super(_imageA, _LUTa, _imageB, _LUTb, surfaceRender, _config, _pBar);


        componentImageXY = new ViewJComponentRenderImageShearWarp(this, imageA, LUTa, imageB, LUTb, pixBufferA_XY,
                                                                  pixBufferB_XY, extents,
                                                                  ViewJComponentRenderImage.ModeCOMPOSITE,
                                                                  maxRenExtent);

        optionsPanel = new JPanelRenderOptionsShearWarp(this);
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
            rotationTotal += rotationAngle;

            if ((rotationTotal != 0) && ((rotationTotal % 180) == 0) && ((rotationTotal % 360) != 0)) {

                // solving one bug issue, here
                rotationAngle += 1;

                double rotationRedian = (rotationAngle / 180d) * (double) Math.PI;
                Transform3D transRotMatrix = new Transform3D();
                Matrix4d matrix = new Matrix4d();

                matrix.setIdentity();

                if (rotationAxis == ViewJFrameRenderCamera.X_AXIS) {
                    matrix.m11 = Math.cos(rotationRedian);
                    matrix.m12 = -Math.sin(rotationRedian);
                    matrix.m21 = Math.sin(rotationRedian);
                    matrix.m22 = Math.cos(rotationRedian);
                    transRotMatrix.set(matrix);
                } else if (rotationAxis == ViewJFrameRenderCamera.Y_AXIS) {
                    matrix.m00 = Math.cos(rotationRedian);
                    matrix.m02 = Math.sin(rotationRedian);
                    matrix.m20 = -Math.sin(rotationRedian);
                    matrix.m22 = Math.cos(rotationRedian);
                    transRotMatrix.set(matrix);
                } else if (rotationAxis == ViewJFrameRenderCamera.Z_AXIS) {
                    matrix.m00 = Math.cos(rotationRedian);
                    matrix.m01 = -Math.sin(rotationRedian);
                    matrix.m10 = Math.sin(rotationRedian);
                    matrix.m11 = Math.cos(rotationRedian);
                    transRotMatrix.set(matrix);
                }

                transRotation.mul(transRotMatrix);
                rotationAngle -= 1;
                transRotMatrix = null;
                matrix = null;
            } else {
                transRotation.mul(transRotationMatrix);
            }

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
            ((JPanelRenderOptionsShearWarp) optionsPanel).disposeLocal(true);
            optionsPanel = null;
        }
    }

    /**
     * Sets how the image plane should be displayed depending on value of slider.
     *
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == sliderT) {

            // Change the currently displayed t slice
            tSlice = sliderT.getValue() - 1;
            textT.setText(String.valueOf(tSlice + 1));
            updateImages(true);
        }
    }

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
