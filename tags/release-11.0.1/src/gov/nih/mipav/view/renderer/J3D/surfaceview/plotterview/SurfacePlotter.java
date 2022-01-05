package gov.nih.mipav.view.renderer.J3D.surfaceview.plotterview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.utils.geometry.Box;
import com.sun.j3d.utils.image.*;
import com.sun.j3d.utils.universe.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * Frame that holds the surface plotter. The surface plotter takes a 2D image and renders it into a 3D object that is
 * similar to a relief map. Higher intensities are peaks, lower intensities are valleys. The colors are the same as
 * those used in the LUT, and when the LUT for the image is updated, so is the LUT for the plotted surface. The image
 * itself is also a part of the scene, and it can be slid along so that the peaks of the intensity levels match the
 * image.
 *
 * @see  ViewJComponentSurface
 */
public class SurfacePlotter extends RenderViewBase implements MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1650691820947415108L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Dialog for recording and playing back mouse events. */
    private JPanelSurfaceBox boxDialog;

    /** Main component found in this frame. */
    private ViewJComponentSurface componentImageXY;

    /** the control panel that hold the z slider. */
    private JPanel controlPanel;

    /** Current event vector index. */
    private int current;

    /** Extents of image. */
    private int[] extents;

    /**
     * Branch parent of the image in the scene, needed so image can be made invisible when another surface is loaded.
     */
    private BranchGroup imageBranch;

    /** Flag indicating if the image is currently visible. */
    private boolean isVisible = true;

    /** Check box menu item indicating if surface should be in line mode or full mode. */
    private JCheckBoxMenuItem itemLine;

    /** Label for slider, "1". */
    private JLabel label1;

    /** Label for slider, "100". */
    private JLabel label100;

    /** Label for slider, "50". */
    private JLabel label50;

    /** Label that says "Z (1-100)". */
    private JLabel labelZ;

    /** LUT that dictates how the vertices of the quad mesh are colored. */
    private ModelLUT LUTa;

    /** The main control panel. */
    private JPanel mainPanel;

    /** Mouse recorder dialog associated with surface plotter. */
    private JPanelMousePlotter mouseDialog;

    /** Mouse Rotate behavior. */
    private MouseRotate mouseRotateBehavior;

    /** Mouse Translate behavior. */
    private MouseTranslate mouseTranslateBehavior;

    /** Mouse Zoom behavior. */
    private MouseZoom mouseZoomBehavior;

    /** Initial center viewing point of the image. */
    private Point3d myEyePoint = new Point3d();

    /** Transform group guiding how image is displayed. */
    private TransformGroup objTransXY;

    /** Menu bar. */
    private JMenuBar openingMenuBar;

    /** Panel that holds the toolbars. */
    private JPanel panelToolbar;

    /** Transformation matrix. */
    private Transform3D parallelScaleT3D;

    /** Flag to indicate the first time slider name changes. */
    private boolean setSliderFlag;

    /** Current slice of 3D image, used to figure out XY plane to plot. */
    private int slice;

    /** Slider events used by the mouse recorder. */
    private MouseEventVectorPlotter sliderEvents;

    /** Slider for frame/slice of image. */
    private JSlider sliderFrame;

    /** 3, 1-100%, dictating where the image in the surface should be displayed. */
    private JSlider sliderZ;

    /** Actual transform of the image. */
    private Transform3D t3d_XY_t;

    /** Text field displaying the percentage of the slider. */
    private JTextField textZ;

    /**
     * Texture creating appearance for the "back" of the image. Needs to be array even though there will be only 1 item.
     */
    private TextureUnitState[] texUnitStateBXY = null;

    /**
     * Texture creating appearance for the "front" of the image. Needs to be array even though there will be only 1
     * item.
     */
    private TextureUnitState[] texUnitStateFXY = null;

    /** Check box for whether or not the image should be visible. */
    private JCheckBox visibleBox;

    /** X and Y resolution. */
    private float xRes, yRes;

    /** Z slice, which image of 1-100 are we showing. */
    private int zSlice = 49;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Makes a frame and puts the image and the plotted quad mesh into it.
     *
     * @param  _imageA  First image to display
     * @param  _LUTa    LUT of the imageA (if null grayscale LUT is constructed).
     * @param  slice    Slice of 3D image that we're going to plot; 0 if 2D.
     * @param  _config  Graphics configuration.
     */
    public SurfacePlotter(ModelImage _imageA, ModelLUT _LUTa, int slice, GraphicsConfiguration _config) {
        super(_imageA, null, _config);
        this.imageA = _imageA;
        this.LUTa = _LUTa;

        imageA.setImageOrder(ModelImage.IMAGE_A);
        extents = new int[2];
        extents[0] = imageA.getExtents()[0];
        extents[1] = imageA.getExtents()[1];

        parallelScaleT3D = new Transform3D();

        this.slice = slice;

        xRes = imageA.getFileInfo(0).getResolutions()[0];
        yRes = imageA.getFileInfo(0).getResolutions()[1];

        if ((xRes <= 0.0f) || (yRes <= 0.0f)) {
            xRes = 1.0f;
            yRes = 1.0f;
        }

        mouseDialog = new JPanelMousePlotter(this);
        mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

        configureFrame();
        createImageSceneGraph();

        universe = new SimpleUniverse(canvas);

        // This will move the ViewPlatform back a bit so the
        // objects in the scene can be viewed.
        universe.getViewingPlatform().setNominalViewingTransform();

        objRootBG.compile();
        universe.addBranchGraph(objRootBG);
        mouseDialog.setup();
        viewPanel = new JPanelView(this);
        boxDialog = new JPanelSurfaceBox(this);
        rotationControlPanel = new JPanelCamera(this);

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calls various methods depending on the action:
     *
     * <ul>
     *   <li>Load in menu - calls a file chooser and loads the quad surface</li>
     *   <li>Save in menu - calls a file chooser and saves the quad surface</li>
     *   <li>Exit in menu - exits this frame</li>
     *   <li>View in menu - opens view dialog</li>
     *   <li>Mouse in menu - opens mouse recorder dialog</li>
     *   <li>Line checkbox in menu - toggles polygon mode between fill and line</li>
     *   <li>Visible checkbox on panel - toggles image on and off</li>
     * </ul>
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("ViewControls")) {
            viewPanel.setVisible(true);
        } else if (command.equals("Mouse")) { }
        else if (command.equals("Load")) {
            load();
        } else if (command.equals("Save")) {
            save();
        } else if (command.equals("Line")) {
            componentImageXY.setPolygonMode(itemLine.getState());
        } else if (command.equals("Visible")) {

            if (!visibleBox.isSelected()) {
                imageBranch.detach();
                setSliderEnabled(false);
                isVisible = false;
            } else if (visibleBox.isSelected()) {
                sceneRootTG.addChild(imageBranch);
                setSliderEnabled(true);
                isVisible = true;
            }
        } else if (command.equals("Resample")) {
            String first = "" + componentImageXY.getSampleSize();
            String[] possibleValues = { "1", "2", "4", "8" };
            String selectedValue = (String) JOptionPane.showInputDialog(this, "Choose a sample size", "Resample",
                                                                        JOptionPane.PLAIN_MESSAGE, null, possibleValues,
                                                                        first);

            if (selectedValue != null) {
                int size = Integer.valueOf(selectedValue).intValue();

                componentImageXY.resample(size);
            }
        } else if (command.equals("Box")) {

            if (boxDialog == null) {
                boxDialog = new JPanelSurfaceBox(this);
            } else {
                boxDialog.setVisible(true);
            }
        } else if (command.equals("AutoCapture")) {
            writeImageAuto();
        } else if (command.equals("Exit")) {
            disposeLocal();
        }

    }

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
                sceneRootTG.setTransform(transRotation);
                writeImage();
            }

            rotationTimes--;
        }

        // write to camera capture frame.
        writeImage();
    }

    /**
     * Set the captureFrame to null.
     */
    public void disableCamera() {

        if (captureFrame != null) {
            captureFrame.close();
            captureFrame = null;
        }
    }

    /**
     * Dispatches event to appropriate object.
     *
     * @param  event  Event to dispatch.
     */
    public void dispatchSavedEvent(EventObject event) {

        if (event instanceof ActionEvent) {
            actionPerformed((ActionEvent) event);
        } else if (event instanceof MouseEvent) {
            canvas.dispatchEvent((MouseEvent) event);
        } else {

            // matt, do we need this else statement?  Up to now, I don't need it.
            stateChanged((ChangeEvent) event);
        }
    }

    /**
     * Dispose memory.
     */
    public void disposeLocal() {

        // System.err.println( "ViewJFrameSurfacePlotter dispose " );
        if (componentImageXY != null) {

            // System.err.println("disposing ViewJComponentSurface");
            componentImageXY.dispose(true);
            componentImageXY = null;
        }

        if (boxDialog != null) {
            boxDialog = null;
        }

        texUnitStateFXY = null;
        texUnitStateBXY = null;

        if (mouseDialog != null) {
            mouseDialog.dispose();
            mouseDialog = null;
        }

        if (sliderEvents != null) {
            sliderEvents = null;
        }

        if (captureFrame != null) {
            captureFrame.close();
            captureFrame = null;
        }

        imageBranch.detach();
        imageBranch = null;

        objTransXY = null;

        objBehaviorBG.detach();
        objBehaviorBG = null;

        sceneRootTG = null;

        universe.cleanup();
        universe.removeAllLocales();
        universe = null;

        canvas = null;

        objRootBG.detach();
        objRootBG = null;

        t3d_XY_t = null;

        if (viewPanel != null) {
            viewPanel.disposeLocal(true);
            viewPanel = null;
        }

        LUTa = null;

        super.disposeLocal();


    }

    /**
     * Finalize to free memory.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Get the camera control panel.
     *
     * @return  JPanelCamera camera control panel
     */
    public JPanelCamera getCameraPanel() {
        return rotationControlPanel;
    }

    /**
     * Get the main control panel.
     *
     * @return  JPanel the main control panel
     */
    public JPanel getControlPanel() {
        return controlPanel;
    }

    /**
     * Accessor that returns the reference to imageA.
     *
     * @return  image
     */
    public ModelImage getImageA() {

        if (componentImageXY != null) {
            return componentImageXY.getImageA();
        } else {
            return null;
        }
    }

    /**
     * Does nothing but must instantiate for this to be a subclass of RenderViewBase.
     *
     * @return  null
     */
    public ModelImage getImageB() {
        return null;
    }

    /**
     * Return mouseDialog from parent frame.
     *
     * @return  mouseDialog Mouse Dialog box.
     */
    public JPanelMousePlotter getMouseControl() {
        return mouseDialog;
    }

    /**
     * Gets the mouse pointer mode - standard or fly - from the view dialog.
     *
     * @return  The mouse pointer mode.
     */
    public int getMouseMode() {

        if (viewPanel == null) {
            return JPanelView.STD_MODE;
        }

        return viewPanel.getMouseMode();
    }

    /**
     * Get the mouse recorder control panel.
     *
     * @return  JPanelMousePlotter mouse control panel
     */
    public JPanelMousePlotter getMousePanel() {
        return mouseDialog;
    }

    /**
     * Get the sample factor.
     *
     * @return  int return the sample factor
     */
    public int getSampleSize() {
        return componentImageXY.getSampleSize();
    }

    /**
     * Gets the current scene state, in terms of what number the slice is on and if it is visible.
     *
     * @return  A SceneState object with the variables set appropriately.
     */
    public Object getSceneState() {
        return new SceneStatePlotter(zSlice, isVisible);
    }

    /**
     * Get the surface box control ???
     *
     * @return  JPanelSurfaceBox
     */
    public JPanelSurfaceBox getSurfaceBoxPanel() {
        return boxDialog;
    }

    /**
     * Get the view control panel.
     *
     * @return  JPanelView the view control
     */
    public JPanelView getViewPanel() {
        return viewPanel;
    }

    /**
     * Makes the box frame invisible.
     */
    public void hideBoxFrame() {
        objBoxFrameBG.detach();
    }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseClicked(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mousePressed(MouseEvent event) {

        if (mouseDialog.mode == JPanelMousePlotter.RECORD_MODE) {
            Transform3D t3D = new Transform3D();

            // get the view
            getSceneRootTG().getTransform(t3D);

            // store name and view together
            sliderEvents = new MouseEventVectorPlotter("Slider" + mouseDialog.sliderCount, t3D, mouseDialog.first,
                                                       getSceneState(), getMouseMode());
            setSliderFlag = true;
            mouseDialog.events.add(sliderEvents);
            current = mouseDialog.events.indexOf(sliderEvents);
        }

    }

    /**
     * Used in MouseRecorder to stop one series of slide moves.
     *
     * @param  event  Original mouse event.
     */
    public void mouseReleased(MouseEvent event) {

        if (mouseDialog.mode == JPanelMousePlotter.RECORD_MODE) {
            mouseDialog.sliderCount++;
        }
    }

    /**
     * Call by the plotter render frame to resample.
     *
     * @param  selectedValue  resample factor
     */
    public void resample(String selectedValue) {

        if (selectedValue != null) {
            int size = Integer.valueOf(selectedValue).intValue();

            componentImageXY.resample(size);
        }

    }

    /**
     * Reset mouseDialog box.
     */
    public void resetMouseDialog() {
        mouseDialog = new JPanelMousePlotter(this);
        mouseDialog.setup();
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
        updateImages(false);

        if ((rotationTotal != 0) && ((rotationTotal % 180) == 0) && ((rotationTotal % 360) != 0)) {
            rotationAngle -= 1;
        }
    }

    /**
     * Does nothing but must instantiate for this to be a subclass of RenderViewBase.
     *
     * @param  value  DOCUMENT ME!
     */
    public void setAlphaBlend(int value) { }

    /**
     * Does nothing but must instantiate for this to be a subclass of RenderViewBase.
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setEnabled(boolean flag) { }

    /**
     * Sets the GUI components to their proper state before the action is dispatched from the player.
     *
     * @param  scene  The state of the scene.
     */
    public void setGUI(Object scene) {
        visibleBox.setSelected(((SceneStatePlotter) scene).zVisible);
        setSliderEnabled(((SceneStatePlotter) scene).zVisible);
        sliderZ.setValue(((SceneStatePlotter) scene).z + 1);
    }

    /**
     * Accessor that sets the LUT.
     *
     * @param  LUT  the LUT
     */
    public void setLUTa(ModelLUT LUT) {
        componentImageXY.setLUTa(LUT);
    }

    /**
     * Sets the mouse pointer mode - standard or fly - in the view dialog.
     *
     * @param  mode  Mode to set to.
     */
    public void setMouseMode(int mode) {
        viewPanel.setMouseMode(mode);
    }

    /**
     * Set the polygonal render mode.
     *
     * @param  flag  mode is on or not
     */
    public void setPolygonMode(boolean flag) {
        componentImageXY.setPolygonMode(flag);
    }

    /**
     * Enable perspective projection rendering; otherwise use orthographic projection.
     *
     * @param  bEnable  true to enable perspective projection
     */
    public void setRenderPerspective(boolean bEnable) {
        // The projection policy is stored in the view.
        // Note that the window resize policy is PHYSICAL_WORLD.

        View kView = universe.getViewer().getView();

        if (bEnable) {
            kView.setProjectionPolicy(View.PERSPECTIVE_PROJECTION);
            kView.setScreenScalePolicy(View.SCALE_SCREEN_SIZE);
        } else {

            // The View's screen scale is always updated based
            // on the difference in z-coordinates of the eyepoint
            // and the center of the bounding sphere around the volume.
            setupEye();
            updateViewScreenScale(parallelScaleT3D);
            kView.setProjectionPolicy(View.PARALLEL_PROJECTION);
            kView.setScreenScalePolicy(View.SCALE_EXPLICIT);
        }
    }

    /**
     * Does nothing but must instantiate for this to be a subclass of RenderViewBase.
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTA(ModelRGB RGBT) { }

    /**
     * Does nothing but must instantiate for this to be a subclass of RenderViewBase.
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTB(ModelRGB RGBT) { }

    /**
     * Set the rotation angel for the camera control.
     *
     * @param  value  int
     */
    public void setRotationAngle(int value) {
        rotationAngle = value;
    }

    /**
     * Set the rotation axis for the camera control.
     *
     * @param  axis  rotation axis x, y, z
     */
    public void setRotationAxis(int axis) {
        rotationAxis = axis;
    }

    /**
     * Sets the scene state appropriately.
     *
     * @param  scene  The state of the scene.
     */
    public void setSceneState(Object scene) {

        if (isVisible && !((SceneStatePlotter) scene).zVisible) {
            imageBranch.detach();
            setSliderEnabled(false);
            isVisible = false;
            visibleBox.setSelected(false);
        } else if (!isVisible && ((SceneStatePlotter) scene).zVisible) {
            sceneRootTG.addChild(imageBranch);
            setSliderEnabled(true);
            isVisible = true;
            visibleBox.setSelected(true);
        }

        zSlice = ((SceneStatePlotter) scene).z;
    }

    /**
     * Does nothing but must instantiate for this to be a subclass of RenderViewBase.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setSlice(int slice) { }

    /**
     * Does nothing but must instantiate for this to be a subclass of RenderViewBase.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setTimeSlice(int slice) { }

    /**
     * Makes the box frame visible.
     */
    public void showBoxFrame() {
        sceneRootTG.addChild(objBoxFrameBG);
    }

    /**
     * Sets where image is in scene based on knob along slider.
     *
     * @param  e  Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == sliderZ) {

            // Change the currently displayed x slice
            zSlice = sliderZ.getValue() - 1;
            textZ.setText(String.valueOf(zSlice + 1));
            translateImage();

            if ((mouseDialog.mode == JPanelMousePlotter.RECORD_MODE) && setSliderFlag) {
                sliderEvents.setName("zSlider" + current);
                mouseDialog.listModel.addElement("zSlider" + current);
                setSliderFlag = false;
            }

        } else if ((sliderFrame != null) && (source == sliderFrame) && !sliderFrame.getValueIsAdjusting()) {

            int newSlice = sliderFrame.getValue();
            this.slice = newSlice;
            componentImageXY.setSlice(newSlice);
            updateImages(true);
        }


        if (mouseDialog.mode == JPanelMousePlotter.RECORD_MODE) {
            sliderEvents.add(e, getSceneState());
        }

    }

    /**
     * Does nothing but must instantiate for this to be a subclass of ViewJFrameSurface.
     *
     * @param  type       DOCUMENT ME!
     * @param  transform  DOCUMENT ME!
     */
    public void transformChanged(int type, Transform3D transform) {
        mouseDialog.transformChanged(type, transform);

        if (MouseBehaviorCallback.ZOOM == type) {
            updateViewScreenScale(transform);
        }
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
     * @return  boolean confirming successful update
     */
    public final boolean updateImages() {

        if (componentImageXY == null) {
            return false;
        }

        try {
            componentImageXY.paintComponent(componentImageXY.getGraphics());
        } catch (OutOfMemoryError error) {
            System.gc();

            return false;
        }

        return true;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     *
     * @param   forceShow  forces show to re import image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public final boolean updateImages(boolean forceShow) {

        if (componentImageXY == null) {
            return false;
        }

        if (componentImageXY.show(LUTa, forceShow, -1) == true) {

            if (forceShow) {
                TextureLoader tex = new TextureLoader(componentImageXY.getImage(), new String("RGB"), null);

                texUnitStateFXY[0].setTexture(tex.getTexture());
                texUnitStateBXY[0].setTexture(tex.getTexture());
                tex = null;
            }

            t3d_XY_t = new Transform3D();
            t3d_XY_t.setTranslation(new Vector3f(0, 0.0f, (zSlice / 100.0f) - 0.5f));
            objTransXY.setTransform(t3d_XY_t);
        } else {
            return false;
        }

        return true;
    }


    /**
     * This methods calls the componentImage's update method to redraw the screen.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB (not used in the plotter)
     * @param   forceShow   forces show to re import image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming successful update
     */
    public final boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean forceShow, int interpMode) {

        // call set slice here as it will force a reload on the quad mesh for any change in the LUT
        // the slice # does not actually change
        componentImageXY.setSlice(slice);

        if (componentImageXY.show(LUTa, forceShow, interpMode)) {
            TextureLoader tex = new TextureLoader(componentImageXY.getImage(), new String("RGB"), null);

            texUnitStateFXY[0].setTexture(tex.getTexture());
            texUnitStateBXY[0].setTexture(tex.getTexture());
            tex = null;
            t3d_XY_t = new Transform3D();
            t3d_XY_t.setTranslation(new Vector3f(0, 0.0f, (zSlice / 100.0f) - 0.5f));
            objTransXY.setTransform(t3d_XY_t);
        } else {
            return false;
        }

        return true;
    }

    /**
     * Panel that has a slider for the image.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildControlPanel() {
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.BOTH;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        controlPanel = new JPanel();
        controlPanel.setBorder(new EtchedBorder());
        controlPanel.setLayout(new GridBagLayout());

        visibleBox = new JCheckBox();
        visibleBox.setSelected(true);
        visibleBox.addActionListener(mouseDialog);
        visibleBox.addActionListener(this);
        visibleBox.setActionCommand("Visible");
        cpGBC.gridx = 0;
        cpGBC.gridy = 0;
        cpGBC.gridwidth = 1;
        cpGBC.gridheight = 1;
        controlPanel.add(visibleBox, cpGBC);

        labelZ = new JLabel(" Z (1 - 100)");
        labelZ.setForeground(Color.black);
        labelZ.setFont(MipavUtil.font12);
        labelZ.setEnabled(true);
        cpGBC.gridx = 1;
        cpGBC.gridy = 0;
        cpGBC.gridwidth = 2;
        cpGBC.gridheight = 1;
        controlPanel.add(labelZ, cpGBC);

        label1 = new JLabel("1");
        label50 = new JLabel(String.valueOf(50));
        label100 = new JLabel(String.valueOf(100));
        label1.setForeground(Color.black);
        label1.setFont(MipavUtil.font12);
        label1.setEnabled(true);
        label50.setForeground(Color.black);
        label50.setFont(MipavUtil.font12);
        label50.setEnabled(true);
        label100.setForeground(Color.black);
        label100.setFont(MipavUtil.font12);
        label100.setEnabled(true);

        sliderZ = new JSlider(1, 100, 50);
        sliderZ.setFont(MipavUtil.font12);
        sliderZ.setEnabled(true);
        sliderZ.setMinorTickSpacing(100 / 10);
        sliderZ.setPaintTicks(true);
        sliderZ.addChangeListener(this);
        sliderZ.addChangeListener(mouseDialog);
        sliderZ.addMouseListener(this);
        sliderZ.setVisible(true);

        if (imageA.getExtents().length > 2) {
            sliderFrame = new JSlider(0, imageA.getExtents()[2]-1, slice);
            sliderFrame.setFont(MipavUtil.font12);
            sliderFrame.setSnapToTicks(true);
            sliderFrame.setEnabled(true);
            sliderFrame.setMinorTickSpacing(1);
            sliderFrame.setPaintTicks(true);
            sliderFrame.addChangeListener(this);
            sliderFrame.addChangeListener(mouseDialog);
            sliderFrame.addMouseListener(this);
            sliderFrame.setVisible(true);
        }


        Hashtable<Integer,JLabel> labelTableZ = new Hashtable<Integer,JLabel> ();

        labelTableZ.put(new Integer(1), label1);
        labelTableZ.put(new Integer(50 + 1), label50);
        labelTableZ.put(new Integer(100), label100);
        sliderZ.setLabelTable(labelTableZ);
        sliderZ.setPaintLabels(true);
        cpGBC.gridx = 4;
        cpGBC.gridy = 0;
        cpGBC.gridwidth = 8;
        cpGBC.gridheight = 1;
        controlPanel.add(sliderZ, cpGBC);

        textZ = new JTextField(String.valueOf(50), 4);
        textZ.setFont(MipavUtil.font12);
        textZ.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.gridx = 14;
        cpGBC.gridy = 0;
        cpGBC.gridwidth = 1;
        cpGBC.gridheight = 1;
        controlPanel.add(textZ, cpGBC);

        if (sliderFrame != null) {

            JLabel sliderLabel = new JLabel("Image slice index");
            sliderLabel.setForeground(Color.black);
            sliderLabel.setFont(MipavUtil.font12);
            sliderLabel.setEnabled(true);

            int middle = ((int) imageA.getExtents()[2] / 2) - 1;
            int last = (imageA.getExtents()[2] - 1);

            JLabel label_1 = new JLabel("0");
            label_1.setForeground(Color.black);
            label_1.setFont(MipavUtil.font12);
            label_1.setEnabled(true);

            JLabel labelMiddle = new JLabel(String.valueOf(middle));
            labelMiddle.setForeground(Color.black);
            labelMiddle.setFont(MipavUtil.font12);
            labelMiddle.setEnabled(true);


            JLabel labelLast = new JLabel(String.valueOf(last));
            labelLast.setForeground(Color.black);
            labelLast.setFont(MipavUtil.font12);
            labelLast.setEnabled(true);

            Hashtable<Integer,JLabel>  labelTableFrame = new Hashtable<Integer,JLabel> ();
            labelTableFrame.put(new Integer(1), label_1);
            labelTableFrame.put(new Integer(middle), labelMiddle);
            labelTableFrame.put(new Integer(last), labelLast);
            sliderFrame.setLabelTable(labelTableFrame);
            sliderFrame.setPaintLabels(true);

            cpGBC.gridx = 1;
            cpGBC.gridy = 1;
            cpGBC.gridwidth = 2;
            controlPanel.add(sliderLabel, cpGBC);

            cpGBC.gridx = 4;
            cpGBC.gridy = 1;
            cpGBC.gridwidth = 8;
            cpGBC.gridheight = 1;
            controlPanel.add(sliderFrame, cpGBC);
        }

        return controlPanel;
    }

    /**
     * Builds menus for the frame.
     */
    private void buildMenu() {
        JPopupMenu.setDefaultLightWeightPopupEnabled(false);

        JMenu fileMenu = new JMenu("File");

        fileMenu.setFont(MipavUtil.font12B);

        JMenu optionsMenu = new JMenu("Options");

        optionsMenu.setFont(MipavUtil.font12B);

        JMenuItem itemExit = new JMenuItem("Exit");

        itemExit.addActionListener(this);
        itemExit.setActionCommand("Exit");
        itemExit.setFont(MipavUtil.font12B);
        fileMenu.add(itemExit);

        JMenuItem itemView = new JMenuItem("View mode");

        itemView.addActionListener(this);
        itemView.setActionCommand("ViewControls");
        itemView.setFont(MipavUtil.font12B);
        optionsMenu.add(itemView);

        JMenuItem itemMouse = new JMenuItem("Mouse recorder");

        itemMouse.addActionListener(this);
        itemMouse.setActionCommand("Mouse");
        itemMouse.setFont(MipavUtil.font12B);
        optionsMenu.add(itemMouse);

        itemLine = new JCheckBoxMenuItem("Line mode");
        itemLine.addActionListener(this);
        itemLine.setActionCommand("Line");
        itemLine.setFont(MipavUtil.font12B);
        optionsMenu.add(itemLine);

        JMenuItem itemSample = new JMenuItem("Resample");

        itemSample.addActionListener(this);
        itemSample.setActionCommand("Resample");
        itemSample.setFont(MipavUtil.font12B);
        optionsMenu.add(itemSample);

        JMenuItem itemBox = new JMenuItem("Display options");

        itemBox.addActionListener(this);
        itemBox.setActionCommand("Box");
        itemBox.setFont(MipavUtil.font12B);
        optionsMenu.add(itemBox);

        openingMenuBar = new JMenuBar();
        openingMenuBar.add(fileMenu);
        openingMenuBar.add(optionsMenu);

    }

    /**
     * Builds the toolbar for the volume render frame.
     */
    private void buildToolBar() {
        panelToolbar = new JPanel();
        panelToolbar.setLayout(new GridLayout(1, 6, 0, 0));

        Border etchedBorder = BorderFactory.createEtchedBorder();

        JToolBar toolBar = new JToolBar();

        toolBar.setBorder(etchedBorder);
        toolBar.setBorderPainted(true);
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

        JButton autosnapButton = new JButton(MipavUtil.getIcon("camera.gif"));

        autosnapButton.addActionListener(this);
        autosnapButton.setToolTipText("Auto snapshot screen");
        autosnapButton.setActionCommand("AutoCapture");
        autosnapButton.setBorderPainted(false);
        autosnapButton.setRolloverEnabled(true);
        autosnapButton.setRolloverIcon(MipavUtil.getIcon("cameraroll.gif"));
        autosnapButton.setFocusPainted(false);
        autosnapButton.setEnabled(true);
        toolBar.add(autosnapButton);

        JButton mouseRecorderButton = new JButton(MipavUtil.getIcon("movie.gif"));

        mouseRecorderButton.addActionListener(this);
        mouseRecorderButton.setToolTipText("Mouse Recorder");
        mouseRecorderButton.setActionCommand("Mouse");
        mouseRecorderButton.setBorderPainted(false);
        mouseRecorderButton.setRolloverEnabled(true);
        mouseRecorderButton.setRolloverIcon(MipavUtil.getIcon("movieroll.gif"));
        mouseRecorderButton.setFocusPainted(false);
        mouseRecorderButton.setEnabled(true);
        toolBar.add(mouseRecorderButton);

        toolBar.add(makeSeparator());

        panelToolbar.add(toolBar);
        mainPanel.add(panelToolbar);
    }

    /**
     * Constructs main frame structures for the surface.
     */
    private void configureFrame() {

        if (imageA == null) {
            return;
        }

        // if not a color image and LUTa is null then make a LUT
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

        componentImageXY = new ViewJComponentSurface(this, imageA, LUTa, extents, slice);
        componentImageXY.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));

        if ((imageA.getFileInfo(0).getResolutions()[0] <= 0.0f) ||
                (imageA.getFileInfo(0).getResolutions()[1] <= 0.0f)) {
            componentImageXY.setResolutions(1, 1);
        } else if (imageA.getFileInfo(0).getResolutions()[1] >= imageA.getFileInfo(0).getResolutions()[0]) {
            componentImageXY.setResolutions(1,
                                            imageA.getFileInfo(0).getResolutions()[1] /
                                                imageA.getFileInfo(0).getResolutions()[0]);
        } else {
            componentImageXY.setResolutions(imageA.getFileInfo(0).getResolutions()[0] /
                                                imageA.getFileInfo(0).getResolutions()[1], 1);
        }

        // ben added this to show the frame number if the sliderframe (3-d) is present
        componentImageXY.setShowSliceNumber((sliderFrame == null));

        setBounds(200, 200, 350, 500);

        JPanel panel3D = new JPanel();

        panel3D.setLayout(new BorderLayout());

        GraphicsConfiguration config = SimpleUniverse.getPreferredConfiguration();

        canvas = new VolumeCanvas3D(config);
        canvas.setSize(300, 300);
        canvas.setVisible(true);
        panel3D.setBounds(200, 200, 100, 100);
        panel3D.setSize(300, 300);
        panel3D.add(canvas, BorderLayout.CENTER);

        buildMenu();
        buildToolBar();

        mainPanel.add(buildControlPanel());
        mainPanel.add(panel3D);
        componentImageXY.setZoom(1, 1);
        componentImageXY.show(null, true, -1);
    }

    /**
     * Creates the scene graph.
     */
    private void createImageSceneGraph() {
        Transform3D t3d;
        float xBox, yBox;

        // Box for XY Plane
        float maxBox;

        xBox = extents[0] * xRes;
        yBox = extents[1] * yRes;
        maxBox = xBox;

        if (yBox > maxBox) {
            maxBox = yBox;
        }

        xBox = xBox / maxBox;
        yBox = yBox / maxBox;

        t3d = new Transform3D();
        t3d.setScale(1.0);
        t3d.setRotation(new AxisAngle4f(1, 1, 1, -1.52f));

        sceneRootTG.setTransform(t3d);
        sceneRootTG.setCapability(Group.ALLOW_CHILDREN_READ);
        sceneRootTG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        sceneRootTG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        sceneRootTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        sceneRootTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        sceneRootTG.setCapability(TransformGroup.ENABLE_PICK_REPORTING);

        imageBranch = new BranchGroup();
        imageBranch.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        imageBranch.setCapability(Group.ALLOW_CHILDREN_READ);
        imageBranch.setCapability(Group.ALLOW_CHILDREN_WRITE);
        imageBranch.setCapability(BranchGroup.ALLOW_DETACH);
        sceneRootTG.addChild(imageBranch);

        objTransXY = new TransformGroup();
        t3d_XY_t = new Transform3D();
        t3d_XY_t.setTranslation(new Vector3f(0, 0.0f, 0.0f)); // testing transform
        objTransXY.setTransform(t3d_XY_t);
        objTransXY.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        objTransXY.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        objTransXY.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objTransXY.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objTransXY.setCapability(TransformGroup.ENABLE_PICK_REPORTING);
        imageBranch.addChild(objTransXY);

        Transform3D t3DB = new Transform3D();

        t3DB.rotY(3.14159);

        TextureLoader texXY = new TextureLoader(componentImageXY.getImage(), new String("RGB"), null);
        Appearance appTmp = new Appearance();

        Box imPlane1 = new Box(xBox / 2, yBox / 2, 0.0f, Box.GENERATE_NORMALS | Box.GENERATE_TEXTURE_COORDS, appTmp, 1);

        objTransXY.addChild(imPlane1);

        Shape3D shape = imPlane1.getShape(Box.FRONT);
        TextureAttributes texAttrFXY = new TextureAttributes();

        texAttrFXY.setTextureMode(TextureAttributes.DECAL);
        texAttrFXY.setPerspectiveCorrectionMode(TextureAttributes.NICEST);
        texUnitStateFXY = new TextureUnitState[1];
        texUnitStateFXY[0] = new TextureUnitState(texXY.getTexture(), texAttrFXY, null);
        texUnitStateFXY[0].setCapability(TextureUnitState.ALLOW_STATE_WRITE);

        Appearance appFXY = new Appearance();

        appFXY.setTextureUnitState(texUnitStateFXY);
        shape.setAppearance(appFXY);

        shape = imPlane1.getShape(Box.BACK);

        TextureAttributes texAttrBXY = new TextureAttributes();

        texAttrBXY.setTextureTransform(t3DB);
        texAttrBXY.setTextureMode(TextureAttributes.DECAL);
        texAttrBXY.setPerspectiveCorrectionMode(TextureAttributes.NICEST);
        texUnitStateBXY = new TextureUnitState[1];
        texUnitStateBXY[0] = new TextureUnitState(texXY.getTexture(), texAttrBXY, null);
        texUnitStateBXY[0].setCapability(TextureUnitState.ALLOW_STATE_WRITE);

        Appearance appBXY = new Appearance();

        appBXY.setTextureUnitState(texUnitStateBXY);
        shape.setAppearance(appBXY);

        createBoxFrame(xBox / 1.5f, yBox / 1.5f, 1 / 1.5f);

        objBehaviorBG = new BranchGroup();
        objBehaviorBG.setCapability(BranchGroup.ALLOW_DETACH);
        objBehaviorBG.setCapability(Group.ALLOW_CHILDREN_READ);
        objBehaviorBG.setCapability(Group.ALLOW_CHILDREN_WRITE);

        // Create the rotate behavior node
        mouseRotateBehavior = new MouseRotate(canvas, sceneRootTG);
        objBehaviorBG.addChild(mouseRotateBehavior);
        mouseRotateBehavior.setSchedulingBounds(bounds);
        mouseRotateBehavior.setupCallback(this);
        mouseRotateBehavior.setFactor(0.005);

        // Create the zoom behavior node
        mouseZoomBehavior = new MouseZoom(canvas, sceneRootTG);
        objBehaviorBG.addChild(mouseZoomBehavior);
        mouseZoomBehavior.setupCallback(this);
        mouseZoomBehavior.setSchedulingBounds(bounds);
        mouseZoomBehavior.setFactor(0.005);

        // Create the translate behavior node
        mouseTranslateBehavior = new MouseTranslate(canvas, sceneRootTG);
        objBehaviorBG.addChild(mouseTranslateBehavior);
        mouseTranslateBehavior.setupCallback(this);
        mouseTranslateBehavior.setSchedulingBounds(bounds);
        sceneRootTG.addChild(objBehaviorBG);

        texXY = null;
        appTmp = null;
        shape = null;
        texAttrFXY = null;
    }

    /**
     * Calls a file chooser then loads the quad mesh from that file. Removes the image and disables the visible check
     * box, so the user may not see the image associated with the original quad mesh once a new one has been loaded.
     */
    private void load() {
        String fileName;
        String directory;
        RandomAccessFile file;
        ViewJProgressBar progress = null;

        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();

        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal = chooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = chooser.getCurrentDirectory() + "" + File.separatorChar;
        } else {
            return;
        }

        ViewUserInterface.getReference().setDefaultDirectory(directory);

        try {
            file = new RandomAccessFile(directory + fileName, "r");
            progress = new ViewJProgressBar("Loading quad mesh", "Loading quad mesh", 0, 100, false, null, null);

            int type = file.readInt();
            //int count = file.readInt();

            if (type != 2) {
                MipavUtil.displayError("File " + directory + fileName + " is not a quad mesh.");

                return;
            }

            ModelQuadMesh qMesh = ModelQuadMesh.loadQMesh(file, progress, 0, 1);

            componentImageXY.setQMesh(qMesh);
            imageBranch.detach();
            setSliderEnabled(false);
            visibleBox.setEnabled(false);
            componentImageXY.show(LUTa, true, -1);
            progress.dispose();
            qMesh.dispose();
            qMesh = null;
        } catch (IOException e) {

            if (progress != null) {
                progress.dispose();
            }

            MipavUtil.displayError("Load of " + directory + fileName + " failed.");
        }
    }

    /**
     * Makes a separator for the use in the toolbars.
     *
     * @return  Separator for the toolbar.
     */
    private JButton makeSeparator() {
        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));

        separator.setBorderPainted(false);
        separator.setFocusPainted(false);

        return (separator);
    }

    /**
     * Calls a file chooser then writes the quad mesh to that file.
     */
    private void save() {
        String fileName;
        String directory;
        ViewJProgressBar progress = null;

        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();

        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal = chooser.showSaveDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = chooser.getCurrentDirectory() + "" + File.separatorChar;
        } else {
            return;
        }

        ViewUserInterface.getReference().setDefaultDirectory(directory);

        try {
            progress = new ViewJProgressBar("Saving quad mesh", "Saving quad mesh", 0, 100, false, null, null);
            progress.setLocation(200, 200);
            progress.setVisible(true);
            componentImageXY.getQMesh().save(directory + fileName, progress, 0, 1);
            progress.dispose();
        } catch (IOException e) {
            MipavUtil.displayError("Save of " + directory + fileName + " failed.");

            if (progress != null) {
                progress.dispose();
            }
        }
    }

    /**
     * Sets the slider, the label "Z (1-100)" and the labels underneath the slider to enabled or disabled, depending on
     * flag.
     *
     * @param  flag  <code>true</code> means enable, <code>false</code> means disable.
     */
    private void setSliderEnabled(boolean flag) {
        sliderZ.setEnabled(flag);
        labelZ.setEnabled(flag);
        label1.setEnabled(flag);
        label50.setEnabled(flag);
        label100.setEnabled(flag);
    }

    /**
     * Set up the center point of the red line boxframe.
     */
    private void setupEye() {
        Shape3D shape;

        shape = new Shape3D(boxFrame, null);

        BoundingSphere kBounds = new BoundingSphere(shape.getBounds());

        Point3d kVolumeCenterPoint = new Point3d();

        kBounds.getCenter(kVolumeCenterPoint);

        // 1.43 is calcualted from 1.0/0.7, magic number from Matt.
        myEyePoint.x = kVolumeCenterPoint.x;
        myEyePoint.y = kVolumeCenterPoint.y;
        myEyePoint.z = kVolumeCenterPoint.z + (1.43 * kBounds.getRadius());
    }

    /**
     * DOCUMENT ME!
     */
    private void translateImage() {
        t3d_XY_t = new Transform3D();
        t3d_XY_t.setTranslation(new Vector3f(0, 0.0f, (zSlice / 100.0f) - 0.5f));
        objTransXY.setTransform(t3d_XY_t);
    }

    /**
     * This function calculates the scale factor for zooming in parallel projection. The scenario is to calculate the
     * distance between the origin boxframe center and tranformed boxframe center. This distance is used to compute the
     * screen scale factor.
     *
     * @param  kTransform  The tranformation matrix from tranformChanged().
     */
    private void updateViewScreenScale(Transform3D kTransform) {
        parallelScaleT3D = kTransform;

        // The boxframe center distance is acutally the bounding sphere
        // center distance.
        Shape3D shape;

        shape = new Shape3D(boxFrame, null);

        BoundingSphere kBounds = new BoundingSphere(shape.getBounds());

        kBounds.transform(kBounds, kTransform);

        Point3d kVolumeCenterPoint = new Point3d();

        kBounds.getCenter(kVolumeCenterPoint);

        Vector3d kViewVector = new Vector3d();

        kViewVector.sub(myEyePoint, kVolumeCenterPoint);

        double dDist = Math.abs(kViewVector.z);

        View kView = universe.getViewer().getView();
        double dFieldOfView = kView.getFieldOfView();
        double dViewWidth = 15.0f * dDist * Math.tan(dFieldOfView / 15.0f);

        Screen3D kScreen = canvas.getScreen3D();

        kView.setScreenScale(kScreen.getPhysicalScreenWidth() / dViewWidth);
    }
}
