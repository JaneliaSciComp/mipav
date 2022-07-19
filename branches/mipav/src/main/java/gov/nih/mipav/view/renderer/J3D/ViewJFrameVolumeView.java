package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.flythroughview.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.flythruview.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview.*;
import gov.nih.mipav.view.renderer.J3D.volumeview.*;

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
 * The volume view frame of the visualization. The frame includes the surface renderer, the raycast renderer, the
 * shearwarp renderer and the flythru renderer.
 *
 * @author  Ruida Cheng
 */
public class ViewJFrameVolumeView extends ViewJFrameBase implements MouseListener, ItemListener, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1898957906984534260L;

    /** Render mode values. */
    public static final int NONE = -1;

    /** DOCUMENT ME! */
    public static final int RAYCAST = 1;

    /** DOCUMENT ME! */
    public static final int SHEARWARP = 2;

    /** DOCUMENT ME! */
    public static final int SURFACE = 3;

    /** DOCUMENT ME! */
    public static final int DUALPANEL = 4;

    /** DOCUMENT ME! */
    public static final int ENDOSCOPY = 5;

    /** DOCUMENT ME! */
    public static final int BRAINSURFACE_FLATTENER = 6;
    
    /** DOCUMENT ME! */
    public static final int SURFACEVIEW = 7;

    /** The small bar on the top right corner the volume view frame. */
    protected static JProgressBar rendererProgressBar;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Configuration param, which will pass down to each render's constructor. */
    protected GraphicsConfiguration config;

    /** Isotropic version image A, B, used by the shear warp renderer. */
    protected ModelImage isoImageA, isoImageB;

    /** Menu items storage. */
    protected ViewMenuBuilder menuObj;

    /** Labels for the current position in 3D ModelView coordinates:. */
    protected JLabel modelViewLabel = null;

    /** DOCUMENT ME! */
    protected JLabel[] modelViewLabelVals = new JLabel[3];

    /** Orientations of the three axes. */
    protected int[] orient = new int[3];

    /** Panel that holds the toolbars. */
    protected JPanel panelToolbar = new JPanel();

    /** Labels for the current position in PatientSlice coordinates:. */
    protected JLabel patientSliceLabel = null;

    /** DOCUMENT ME! */
    protected JLabel[] patientSliceLabelVals = new JLabel[3];

    /** Lookup table of the color imageA, B. */
    protected ModelRGB RGBTA = null, RGBTB = null;

    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;

    /** Panel containing the position labels:. */
    JPanel panelLabels = new JPanel();

    /** Indicates that image orientation is unknown type or not. */
    private boolean axialOrientation = true;

    /** Rendering the brainsurfaceFlattener objects. */
    private MjCorticalAnalysis brainsurfaceFlattenerRender = null;

    /** DOCUMENT ME! */
    private JPanel cameraPanel;

    /** DOCUMENT ME! */
    private JPanel clipBox;

    /** Button to invoke all the six clipping planes. */
    private JButton clipButton;

    /** Button to disable all the six clipping planes. */
    private JButton clipDisableButton;

    /** Button to crop the clip volume. */
    private JButton clipMaskButton;

    /** Button to undo crop the clip volume. */
    private JButton clipMaskUndoButton;

    /** DOCUMENT ME! */
    private JPanel clipPanel;

    /** Button to invoke clipping planes. */
    private JButton clipPlaneButton;

    /** Button to save clipped region. */
    private JButton clipSaveButton;

    /** DOCUMENT ME! */
    private JPanel displayPanel;

    /** dual image pane that holds the left and right panel. */
    private JSplitPane dualImagePane;

    /** The image panel to hold two Canvas3D. */
    private JPanel dualImagePanel;

    /** Left panel of the dual panel view. */
    private JPanel dualLeftPanel;

    /** Right panel of the dual panel view. */
    private JPanel dualRightPanel;

    /** Keep track of whether we're switching to the volume renderer for the first time. */
    private boolean firstTimeVolView = true;

    /** Fly through setup control panel. */
    private JPanelVirtualEndoscopySetup flythruControl;

    /** Fly through movement control panel. */
    private JPanelFlythruMove flythruMoveControl;

    /** View panel contains the fly thru control panel. */
    private JPanel flythruMovePanel;

    /** Fly through options. */
    private FlythruRender.SetupOptions flythruOptions = new FlythruRender.SetupOptions();

    /** Fly through panel. */
    private JPanel flythruPanel;

    /** Reference to fly through renderer. */
    private FlythruRender flythruRender;

    /** Surface load button. */
    private JButton flythruSurfaceButton;

    /** DOCUMENT ME! */
    private Vector flythruTabVector = new Vector();

    /** Flythru toolbar. */
    private JToolBar flyThruToolbar;

    /** Control panel for the surface renderer. */
    private JPanel histoLUTPanel;

    /** Reference to the imageA original copy. */
    private ModelImage imageAOriginal;

    /** Image orientation: coronal, sagittal, axial, unknown. */
    private int imageOrientation;

    /** The image panel to hold one Canvas3D. */
    private JPanel imagePanel;

    /** DOCUMENT ME! */
    private JDialogIntensityPaint intensityDialog;

    /** Indicate if the brainsurface flattener render is enabled from the resample dialog or not. */
    private boolean isBrainsurfaceFlattenerEnable = false;

    /** Indicate if the fly through render is enabled from the resample dialog or not. */
    private boolean isEndoscopyEnable = false;

    /** Indicate if the raycast render is enabled from the resample dialog or not. */
    private boolean isRayCastEnable = false;

    /** Indicate if the shear warp render is enabled from the resample dialog or not. */
    private boolean isShearWarpEnable = false;

    /** Indicate if the surface render is enabled from the resample dialog or not. */
    private boolean isSurfaceRenderEnable = false;

    /** Indicate if the surface view is enabled from the resample dialog or not. */
    private boolean isSurfaceViewEnable = false;
    
    /** The left panel renderer mode. */
    private int leftPanelRenderMode = -1;

    /** DOCUMENT ME! */
    private JPanel lightPanel;

    /** The three slice views displayed as texture-mapped polygons:. */
    private PlaneRender[] m_akPlaneRender;

    /** Control panels for the Brainsurface Flattener:. */
    private JPanel m_kBrainsurfaceFlattenerPanel = null;

    /** Control panel for drawing geodesic curves. */
    private JPanel m_kGeodesicPanel;

    /** Control panel for volume sculpting. */
    private JPanel m_kSculptPanel;

    /** The max width of the control panels. */
    private int maxPanelWidth = -1;

    /** Menu bar. */
    private JMenuBar menuBar;

    /** DOCUMENT ME! */
    private JPanel mousePanel;

    /** DOCUMENT ME! */
    private JDialogOpacityControls opacityDialog;

    /** DOCUMENT ME! */
    private JPanel opacityPanel = null;

    /** Padding imageA with blank images feeding. */
    private ModelImage paddingImageA;

    /** Padding imageB with blank images feeding. */
    private ModelImage paddingImageB;

    /** Control panels of the triplanar view. */
    private JDialogPaintGrow paintGrowDialog;

    /** LUT control panel of the gray scale image. */
    private JFrameHistogram panelHistogram;

    /** Rendering parallel rotation button. */
    private JToggleButton parallelButton;

    /** DOCUMENT ME! */
    private JPanel probePanel;

    /** Radio button of the COMPOSITE mode option. */
    private JRadioButton radioCOMPOSITE;

    /** Radio button of the COMPOSITE mode option. */
    private JRadioButton radioCOMPOSITEShear;

    /** Radio button of the MIP mode option. */
    private JRadioButton radioMIP;

    /** Radio button of the MIP mode option. */
    private JRadioButton radioMIPShear;

    /** Radio button of the SURFACE mode option. */
    private JRadioButton radioSURFACE;

    /** Radio button of the SURFACE mode option. */
    private JRadioButton radioSURFACEFAST;

    /** Radio button of the SURFACE mode option. */
    private JRadioButton radioSURFACEShear;

    /** Radio button of the surface render composite mode. */
    private JRadioButton radioSurrenderCOMPOSITE;

    /** Radio button of the surface render lighting mode. */
    private JRadioButton radioSurrenderLIGHT;

    /** Radio button of the XRAY mode option. */
    private JRadioButton radioXRAY;

    /** Radio button of the XRAY mode option. */
    private JRadioButton radioXRAYShear;

    /** Panel Border view. */
    private Border raisedbevel, loweredbevel, compound, redBorder, etchedBorder, pressedBorder;

    /** DOCUMENT ME! */
    private JPanel raycastCameraPanel;

    /** Control panels for the raycast render. */
    private JPanel raycastOptionsPanel;

    /** DOCUMENT ME! */
    private VolumeRendererRayCast raycastRender;

    /** DOCUMENT ME! */
    private Vector raycastTabVector = new Vector();

    /** Raycast toolbar. */
    private JToolBar rayCastToolBar;

    /** Reference to resample dialog, use to null out the resample dialog in this frame. */
    private JDialogVolViewResample resampleDialog;

    /** Button for RFA. */
    private JButton rfaButton;

    /** RFA separator. */
    private JButton rfaSeparator;

    /** The view pane that contains the image view and tri-planar view panels. */
    private JSplitPane rightPane;

    /** The right panel renderer mode. */
    private int rightPanelRenderMode = -1;

    /** Screen width, screen height. */
    private int screenWidth, screenHeight;

    /** Sculpt region height. */
    private int sculptHeight;

    /** Sculpt region width. */
    private int sculptWidth;

    /** DOCUMENT ME! */
    private JPanel shearwarpCameraPanel;

    /** Control panels for the shearwarp render. */
    private JPanel shearwarpOptionsPanel;

    /** DOCUMENT ME! */
    private VolumeRendererShearWarp shearwarpRender;

    /** DOCUMENT ME! */
    private Vector shearwarpTabVector = new Vector();

    /** Shearwarp toolbar. */
    private JToolBar shearWarpToolBar;

    /** DOCUMENT ME! */
    private JPanel slicePanel;

    /** Previoius tab index recorder. */
    private int storeTabbedPaneIndex = 0;

    /** DOCUMENT ME! */
    private JPanel surfacePanel;

    /** Three types of renderer. */
    private SurfaceRender surRender;

    /** Surface View reference */
    private SurfaceView surView;    
    
    /** For each render, use the vector to store the currently active tabs. */
    private Vector surTabVector = new Vector();

    /** Toolbar builder reference. */
    private ViewToolBarBuilder toolbarBuilder;

    /** Tri image planar render panels. */
    private JPanel triImagePanel;

    /** Rendering unparallel rotaion button. */
    private JToggleButton unparallelButton;

    /** DOCUMENT ME! */
    private JPanel viewPanel;

    /** The top one render view switch toolbar. */
    private JToolBar viewToolBar;

    /** Surface Render toolbar. */
    private JToolBar volToolBar;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Make a volume rendering frame, which contains the toolbars on the top, control panel on the left, the volume
     * rendering panel on the right, and the three orthogonal view ( axial, sagittal, coronal, views) on the bottom
     * right.
     *
     * @param  _imageA                First image to display
     * @param  LUTa                   LUT of the imageA (if null grayscale LUT is constructed)
     * @param  _RGBTA                 RGB table of imageA
     * @param  _imageB                Second loaded image
     * @param  LUTb                   LUT of the imageB
     * @param  _RGBTB                 RGB table of imageB
     * @param  _leftPanelRenderMode   shear warp render mode enabled or not
     * @param  _rightPanelRenderMode  volume rendering panel render mode ( Raycast, shearwarp, etc).
     * @param  _resampleDialog        resample dialog reference.
     */
    public ViewJFrameVolumeView(ModelImage _imageA, ModelLUT LUTa, ModelRGB _RGBTA, ModelImage _imageB, ModelLUT LUTb,
                                ModelRGB _RGBTB, int _leftPanelRenderMode, int _rightPanelRenderMode,
                                JDialogVolViewResample _resampleDialog) {
        super(_imageA, _imageB);
        leftPanelRenderMode = _leftPanelRenderMode;
        rightPanelRenderMode = _rightPanelRenderMode;
        resampleDialog = _resampleDialog;
        RGBTA = _RGBTA;
        RGBTB = _RGBTB;
        this.LUTa = LUTa;
        this.LUTb = LUTb;

        try {
            setIconImage(MipavUtil.getIconImage("4plane_16x16.gif"));
        } catch (Exception e) {
            e.printStackTrace();
        }

        imageOrientation = imageA.getImageOrientation();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Retrieve the progress bar used in the volume renderer (the one in the upper right hand corner).
     *
     * @return  the volume renderer progress bar
     */
    public static final JProgressBar getRendererProgressBar() {

        if (rendererProgressBar == null) {
            rendererProgressBar = new JProgressBar();
        }

        return rendererProgressBar;
    }

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Extract")) {
            surRender.updateImageFromRotation();
        } else if (command.equals("parallelrotation")) {

            if (surRender != null) {
                surRender.setParallelRotation(true);
            }

            if (shearwarpRender != null) {
                shearwarpRender.setParallelRotation(true);
            }

            if (raycastRender != null) {
                raycastRender.setParallelRotation(true);
            }
        } else if (command.equals("rotatearbitrary")) {

            if (surRender != null) {
                surRender.setParallelRotation(false);
            }

            if (shearwarpRender != null) {
                shearwarpRender.setParallelRotation(false);
            }

            if (raycastRender != null) {
                raycastRender.setParallelRotation(false);
            }
        } else if (command.equals("HistoLUT")) {
            insertTab("LUT", histoLUTPanel);
            insertSurfaceTab("LUT", histoLUTPanel);
            insertRaycastTab("LUT", histoLUTPanel);
            insertShearwarpTab("LUT", histoLUTPanel);
        } else if (command.equals("Flythru")) {
            enableFlyThruRender();
        } else if (command.equals("FlythruControl")) {
            insertTab("FlythruControl", flythruMovePanel);
            insertFlythruTab("FlythruControl", flythruMovePanel);

        } else if (command.equals("ShearRender")) {
            enableShearWarpRender();
        } else if (command.equals("SurRender")) {
            enableSurfaceRender();
        } else if (command.equals("VolRender")) {
            enableVolumeRender();
        } else if (command.equals("DualPanelRender")) {
            enableDualPanelRender();
        } else if (command.equals("shearWarpOptions")) {
            insertTab("View", shearwarpOptionsPanel);
            insertShearwarpTab("View", shearwarpOptionsPanel);
        } else if (command.equals("MIPShear")) {
            shearwarpRender.MIPMode();
        } else if (command.equals("DRRShear")) {
            shearwarpRender.DRRMode();
        } else if (command.equals("SURShear")) {
            shearwarpRender.SURMode();
        } else if (command.equals("AutoCaptureShear")) {
            insertTab("Camera", shearwarpCameraPanel);
            insertShearwarpTab("Camera", shearwarpCameraPanel);
        } else if (command.equals("RayCastOptions")) {
            insertTab("View", raycastOptionsPanel);
            insertRaycastTab("View", raycastOptionsPanel);
        } else if (command.equals("MIPmode")) {
            raycastRender.MIPMode();
        } else if (command.equals("DRRmode")) {
            raycastRender.DRRMode();
        } else if (command.equals("SURmode")) {
            raycastRender.SURMode();
        } else if (command.equals("Geodesic")) {
            insertTab("Geodesic", m_kGeodesicPanel);
            insertSurfaceTab("Geodesic", m_kGeodesicPanel);
            insertFlythruTab("Geodesic", m_kGeodesicPanel);
        } else if (command.equals("Sculpt")) {
            insertTab("Sculpt", m_kSculptPanel);
            insertSurfaceTab("Sculpt", m_kSculptPanel);
            insertRaycastTab("Sculpt", m_kSculptPanel);
            insertShearwarpTab("Sculpt", m_kSculptPanel);
        } else if (command.equals("AutoCapture")) {
            insertTab("Camera", raycastCameraPanel);
            insertRaycastTab("Camera", raycastCameraPanel);
        } else if (command.equals("raycastRepaint")) {
            updateImages(LUTa, LUTb, true, -1);
        } else if (command.equals("shearwarpRepaint")) {
            updateImages(LUTa, LUTb, true, -1);
        } else if (command.equals("Repaint")) {
            volumeRepaint();
        } else if (command.equals("Clipping")) {

        	if (( surRender != null && surRender.getDisplayMode3D() ) ||
        		( surView != null )) {
                clipBox.setVisible(true);
            } else {
                clipBox.setVisible(false);
            }
            
            insertTab("Clip", clipPanel);
            insertSurfaceTab("Clip", clipPanel);
            insertRaycastTab("Clip", clipPanel);
            insertShearwarpTab("Clip", clipPanel);
            
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            if ( surRender != null ) {
               surRender.getClipDialog().resizePanel(maxPanelWidth, height);
            } else if ( surView != null ){
               surView.getClipDialog().resizePanel(maxPanelWidth, height);
            }
        } else if (command.equals("OpacityHistogram")) {
            insertTab("Opacity", opacityPanel);
            insertSurfaceTab("Opacity", opacityPanel);
            insertRaycastTab("Opacity", opacityPanel);
            insertShearwarpTab("Opacity", opacityPanel);
        } else if (command.equals("Opacity")) {
            clipBox.setVisible(true);
            clipButton.setEnabled(true);
            clipPlaneButton.setEnabled(true);
            clipDisableButton.setEnabled(true);
            clipMaskButton.setEnabled(true);
            clipMaskUndoButton.setEnabled(true);
            clipSaveButton.setEnabled(true);
            insertTab("Opacity", opacityPanel);
            insertSurfaceTab("Opacity", opacityPanel);
            insertRaycastTab("Opacity", opacityPanel);
            insertShearwarpTab("Opacity", opacityPanel);
        } else if (command.equals("Stereo")) {

            /* Launch the stereo viewer for the volumeTexture. Using the
             * current viewing transofrm from the SurfaceRender: */
            Transform3D kTransform = new Transform3D();
            surRender.getSceneRootTG().getTransform(kTransform);
            new JStereoWindow(surRender.getVolumeTextureCopy(0), surRender.getVolumeTextureCopy(1), kTransform,
                              surRender);
        } else if (command.equals("ChangeLight")) {
            insertTab("Light", lightPanel);
            insertSurfaceTab("Light", lightPanel);
            insertRaycastTab("Light", lightPanel);
            insertShearwarpTab("Light", lightPanel);
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            if ( surRender != null ) {
               surRender.getSurfaceDialog().getLightDialog().resizePanel(maxPanelWidth, height);
            } else if ( surView != null ){
               surView.getSurfaceDialog().getLightDialog().resizePanel(maxPanelWidth, height);
            }
        } else if (command.equals("Box")) {
            insertTab("Display", displayPanel);
            insertSurfaceTab("Display", displayPanel);
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            if ( surRender != null ) {
               surRender.getDisplayDialog().resizePanel(maxPanelWidth, height);
            } else if ( surView != null ){
               surView.getDisplayDialog().resizePanel(maxPanelWidth, height);
            }
        } else if (command.equals("ViewControls")) {
            insertTab("View", viewPanel);
            insertSurfaceTab("View", viewPanel);
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            if ( surRender != null ) {
               surRender.getViewDialog().resizePanel(maxPanelWidth, height);
            } else if ( surView != null ){
               surView.getViewDialog().resizePanel(maxPanelWidth, height);
            }
        } else if (command.equals("InvokeClipping")) {
        	 if (surRender != null && surRender.getDisplayMode3D()  || 
        	     (surView != null ) ) {
                 clipBox.setVisible(true);
                 if ( surRender != null ) {
                   surRender.invokeClipping();
                 }
                 if ( surView != null ) {
                   surView.invokeClipping();
                 }
             } else {
                 clipBox.setVisible(false);
             }
             
            insertTab("Clip", clipPanel);
            insertSurfaceTab("Clip", clipPanel);
            insertRaycastTab("Clip", clipPanel);
            insertShearwarpTab("Clip", clipPanel);
            
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            if ( surRender != null ) {
               surRender.getClipDialog().resizePanel(maxPanelWidth, height);
            } else if ( surView != null ){
               surView.getClipDialog().resizePanel(maxPanelWidth, height);
            }
            
        } else if (command.equals("DisableClipping")) {

        	if ( surRender != null && surRender.getDisplayMode3D() ||
        			( surView != null ) ) {
                clipBox.setVisible(true);
                if ( surRender != null ) {
                  surRender.disableClipping();
                }
                if ( surView != null ) {
                	surView.disableClipping();
                }
            } else {
                clipBox.setVisible(false);
            }

            
            insertTab("Clip", clipPanel);
            insertSurfaceTab("Clip", clipPanel);
            insertRaycastTab("Clip", clipPanel);
            insertShearwarpTab("Clip", clipPanel);
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            if ( surRender != null ) {
               surRender.getClipDialog().resizePanel(maxPanelWidth, height);
            } else if ( surView != null ){
               surView.getClipDialog().resizePanel(maxPanelWidth, height);
            }
        } else if (command.equals("CropClipVolume")) {
            surRender.cropClipVolume();
        } else if (command.equals("UndoCropVolume")) {
            surRender.undoCropVolume();
        } else if (command.equals("SaveCropVolume")) {
            surRender.saveCropVolume();
        } else if (command.equals("Slices")) {
            clipBox.setVisible(false);
            clipButton.setEnabled(false);
            clipPlaneButton.setEnabled(false);
            clipDisableButton.setEnabled(false);
            clipMaskButton.setEnabled(false);
            clipMaskUndoButton.setEnabled(false);
            clipSaveButton.setEnabled(false);
            insertTab("Slices", slicePanel);
            insertSurfaceTab("Slices", slicePanel);
        } else if (command.equals("SurfaceDialog")) {
            insertTab("Surface", surfacePanel);
            insertSurfaceTab("Surface", surfacePanel);
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            if ( surRender != null ) {
               surRender.getSurfaceDialog().resizePanel(maxPanelWidth, height);
            } else if ( surView != null ){
               surView.getSurfaceDialog().resizePanel(maxPanelWidth, height);
            }
        } else if (command.equals("SurfaceTexture")) {
        	if ( surRender != null ) {
                insertTab("SurfaceTexture", surRender.getSurfaceTexturePanel());
                insertSurfaceTab("SurfaceTexture", surRender.getSurfaceTexturePanel());
          	} else if ( surView != null ) {
          	  insertTab("SurfaceTexture", surView.getSurfaceTexturePanel());
                insertSurfaceTab("SurfaceTexture", surView.getSurfaceTexturePanel());
          	}
        	
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

			if ( surRender != null ) {
			    surRender.getSurfaceDialog().resizePanel(maxPanelWidth, height);
			  } else if ( surView != null ) {
			    surView.getSurfaceDialog().resizePanel(maxPanelWidth, height);
			  }
        } else if (command.equals("RFA")) {
            insertTab("RFA", probePanel);
            insertSurfaceTab("RFA", probePanel);

            // hack to get the panel's scroll pane to show up correctly
            // the MIPAV version of the RFAST needs this setSize() for some messed up reason...
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            if ( surRender != null ) {
                surRender.getProbeDialog().resizePanel(maxPanelWidth, height);
              } else if ( surView != null ) {
                surView.getProbeDialog().resizePanel(maxPanelWidth, height);
              }
        } else if (command.equals("Capture")) {
            insertTab("Camera", cameraPanel);
            insertSurfaceTab("Camera", cameraPanel);
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            if ( surRender != null ) {
                surRender.getCameraControl().resizePanel(maxPanelWidth, height);
              } else if ( surView != null ) {
                surView.getCameraControl().resizePanel(maxPanelWidth, height);
              }
        } else if (command.equals("Mouse")) {
            insertTab("Recorder", mousePanel);
            insertSurfaceTab("Recorder", mousePanel);

	        if( surRender != null ) {
	            surRender.cleanMouseRecorder();
	        }
	         
        } else if (command.equals("ResetX")) {
            resetAxisY();
        } else if (command.equals("ResetY")) {
            resetAxisX();
        } else if (command.equals("ResetZ")) {
            resetImage();
        } else if (command.equals("CloseFrame")) {
            windowClosing(null);
        } else if (command.equals("ShowAxes")) {
            boolean showAxes = menuObj.isMenuItemSelected("Show axes");
            if ( m_akPlaneRender != null ) {
	            for (int iPlane = 0; iPlane < 3; iPlane++) {
	                m_akPlaneRender[iPlane].showAxes(showAxes);
	                m_akPlaneRender[iPlane].update();
	            }
            }
        } else if (command.equals("ShowXHairs")) {
            boolean showXHairs = menuObj.isMenuItemSelected("Show crosshairs");
            if ( m_akPlaneRender != null ) {
		        for (int iPlane = 0; iPlane < 3; iPlane++) {
		            m_akPlaneRender[iPlane].showXHairs(showXHairs);
		            m_akPlaneRender[iPlane].update();
		        }
            }
        } else if (command.equals("RFAToolbar")) {
            boolean showRFA = menuObj.isMenuItemSelected("RFA toolbar");

            setRFAToolbarVisible(showRFA);
        } else if (command.equals("ProbeTargetPoint")) {
            enableTargetPointPicking();
        } else if (command.equals("traverse")) {
            disableTargetPointPicking();
        } else if (command.equals("RadiologicalView")) {
            imageA.setRadiologicalView(true);

            if (imageB != null) {
                imageB.setRadiologicalView(true);
            }

            surRender.setCenter(m_akPlaneRender[0].getCenter());
            setPositionLabels(surRender.getSlicePanel().getCenter());
            updateImages(true);
        } else if (command.equals("NeurologicalView")) {
            imageA.setRadiologicalView(false);

            if (imageB != null) {
                imageB.setRadiologicalView(false);
            }

            surRender.setCenter(m_akPlaneRender[0].getCenter());
            setPositionLabels(surRender.getSlicePanel().getCenter());
            updateImages(true);
        }

    }

    /**
     * Add any attached surfaces the current image has in its file info (if the file info is in the xml format).
     */
    public void addAttachedSurfaces() {

    	if ((surRender != null) && (surRender.getSurfaceDialog() != null)) {
            surRender.getSurfaceDialog().addAttachedSurfaces();
        }
        else if ((surView != null) && (surView.getSurfaceDialog() != null)) {
            surView.getSurfaceDialog().addAttachedSurfaces();
        }
    }

    /**
     * Updates the surRender -- adds a BranchGroup to the main Display.
     *
     * @param  kBranch  BranchGroup branch group
     * @param  kMesh    ModelTriangleMesh surface mesh
     * @param  kCenter  Point3f center of mass
     */
    public void addBranch(BranchGroup kBranch, ModelTriangleMesh kMesh, Point3f kCenter) {

        if ((surRender != null) && (surRender.getSurfaceDialog() != null)) {
            surRender.getSurfaceDialog().addBranch(kBranch, kMesh, kCenter);
        }
        else if ((surView != null) && (surView.getSurfaceDialog() != null)) {
        	surView.getSurfaceDialog().addBranch(kBranch, kMesh, kCenter);
        }
    }

    /**
     * Updates the surRender to display the flythru flight path from the flythruRender.
     */
    public void addFlightPath() {

        /* Gets the flythru path geometry and the current path position  (scaled) */
        surRender.getSurfaceDialog().addFlightPath(flythruRender.getBranchPathShape(),
                                                   flythruRender.getPositionScaled());
    }

    /**
     * Adding surface to the 3D texuture volume.
     *
     * @param  dir   surface file direcotry
     * @param  file  surface file name
     */
    public void addSurface(String dir, File file) {

        if ((surRender != null) && (surRender.getSurfaceDialog() != null)) {
            surRender.getSurfaceDialog().addSurfaces(dir, file);
        }
        else if ((surView != null) && (surView.getSurfaceDialog() != null)) {
        	surView.getSurfaceDialog().addSurfaces(dir, file);
        }
    }

    /**
     * Build the Brainsurface Flattener panel:
     */
    public void buildBrainsurfaceFlattener() {
        m_kBrainsurfaceFlattenerPanel = new JPanel();
        m_kBrainsurfaceFlattenerPanel.add(brainsurfaceFlattenerRender.getMainPanel());
        maxPanelWidth = Math.max(m_kBrainsurfaceFlattenerPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the camera control panel for the surface render.
     */
    public void buildCameraPanel() {
        cameraPanel = new JPanel();
        if ( surRender != null ) {
            cameraPanel.add(surRender.getCameraControl().getMainPanel());
          } else if ( surView != null ){
            cameraPanel.add(surView.getCameraControl().getMainPanel());
          }
        maxPanelWidth = Math.max(cameraPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the clipping control panel for the surface render.
     */
    public void buildClipPanel() {
        clipPanel = new JPanel();
        if ( surRender != null ) {
            clipBox = surRender.getClipDialog().getMainPanel();
           } else if ( surView != null ) {
            clipBox = surView.getClipDialog().getMainPanel();
        }
        clipPanel.add(clipBox);
        maxPanelWidth = Math.max(clipPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the display control panel for the surface render.
     */
    public void buildDisplayPanel() {
        displayPanel = new JPanel();
        if ( surRender != null ) {
            displayPanel.add(((SurfaceRender) surRender).getDisplayDialog().getMainPanel());
          } else if ( surView != null ) {
            displayPanel.add(((SurfaceRender) surView).getDisplayDialog().getMainPanel());	
          }
        maxPanelWidth = Math.max(displayPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the flythru move control panel.
     */
    public void buildFlythruMovePanel() {
        flythruMovePanel = new JPanel();
        flythruMoveControl = new JPanelFlythruMove(flythruRender);
        flythruMovePanel.add(flythruMoveControl.getMainPanel());
        maxPanelWidth = Math.max(flythruMovePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the flythru control panel.
     */
    public void buildFlythruPanel() {
        flythruPanel = new JPanel();
        flythruControl = new JPanelVirtualEndoscopySetup(flythruRender, this);
        flythruPanel.add(flythruControl.getMainPanel());
        maxPanelWidth = Math.max(flythruPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the Geodesic control panel.
     */
    public void buildGeodesic() {
        m_kGeodesicPanel = new JPanel();
        if ( surRender != null ) {
            m_kGeodesicPanel.add(surRender.getGeodesicPanel().getMainPanel());
          } else if ( surView != null ) {
            m_kGeodesicPanel.add(surView.getGeodesicPanel().getMainPanel());
          }
        maxPanelWidth = Math.max(m_kGeodesicPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * The histogram control panel of the lookup table.
     */
    public void buildHistoLUTPanel() {
        histoLUTPanel = new JPanel();
        histoLUTPanel.add(panelHistogram.getContainingPanel());
        maxPanelWidth = Math.max(histoLUTPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * The label panel of the x, y, z slider position.
     */
    public void buildLabelPanel() {
        super.buildLabelPanel();
        patientSliceLabel = new JLabel("Patient Slice Position");
        patientSliceLabel.setForeground(Color.black);
        patientSliceLabel.setFont(MipavUtil.font14B);
        patientSliceLabelVals[0] = new JLabel("sagittal slice: ");
        patientSliceLabelVals[1] = new JLabel("coronal slice: ");
        patientSliceLabelVals[2] = new JLabel("axial slice: ");

        modelViewLabel = new JLabel("3D Model Position");
        modelViewLabel.setForeground(Color.black);
        modelViewLabel.setFont(MipavUtil.font14B);
        modelViewLabelVals[0] = new JLabel("X: ");
        modelViewLabelVals[1] = new JLabel("Y: ");
        modelViewLabelVals[2] = new JLabel("Z: ");

        for (int i = 0; i < 3; i++) {
            patientSliceLabelVals[i].setForeground(Color.black);
            patientSliceLabelVals[i].setFont(MipavUtil.font12B);

            modelViewLabelVals[i].setForeground(Color.black);
            modelViewLabelVals[i].setFont(MipavUtil.font12B);
        }

        JPanel patientSlicePanel = new JPanel(new GridBagLayout());
        JPanel modelViewPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.WEST;

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;

        patientSlicePanel.add(patientSliceLabel, gbc2);
        modelViewPanel.add(modelViewLabel, gbc2);

        gbc2.gridy++;
        patientSlicePanel.add(new JLabel(), gbc2);
        modelViewPanel.add(new JLabel(), gbc2);

        for (int i = 0; i < 3; i++) {
            gbc2.gridy++;
            patientSlicePanel.add(patientSliceLabelVals[i], gbc2);
            modelViewPanel.add(modelViewLabelVals[i], gbc2);
        }

        JRadioButton radiologicalView = new JRadioButton("Radiological View");
        radiologicalView.setSelected(true);
        radiologicalView.addActionListener(this);
        radiologicalView.setActionCommand("RadiologicalView");

        JRadioButton neurologicalView = new JRadioButton("Neurological View");
        neurologicalView.setSelected(false);
        neurologicalView.addActionListener(this);
        neurologicalView.setActionCommand("NeurologicalView");

        ButtonGroup dataViewGroup = new ButtonGroup();
        dataViewGroup.add(radiologicalView);
        dataViewGroup.add(neurologicalView);


        JPanel viewPanel = new JPanel(new GridBagLayout());
        gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.CENTER;
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        viewPanel.setBorder(JPanelRendererBase.buildTitledBorder("Viewing Convention"));
        viewPanel.add(radiologicalView, gbc2);
        gbc2.gridy = 1;
        viewPanel.add(neurologicalView, gbc2);

        JPanel panelLabelsModel = new JPanel();
        panelLabelsModel.setLayout(new GridLayout(1, 2));
        panelLabelsModel.setBorder(JPanelRendererBase.buildTitledBorder("Rendering Coordinates"));
        panelLabelsModel.add(modelViewPanel);
        panelLabelsModel.add(patientSlicePanel);

        JPanel panelLabelsScanner = new JPanel();
        panelLabelsScanner.setLayout(new GridLayout(1, 2));
        panelLabelsScanner.setBorder(JPanelRendererBase.buildTitledBorder("Scanner Coordinates"));
        
        scannerTabbedPane = new JTabbedPane();
        absolutePanel.setBorder(JPanelRendererBase.buildTitledBorder(""));
        scannerTabbedPane.add("Voxel", absolutePanel);
        scannerTabbedPane.add("Scanner LPS", scannerLPSPanel);
        scannerTabbedPane.add("Scanner RAS", scannerRASPanel);
        panelLabelsScanner.add(scannerTabbedPane);

        panelLabels.setLayout(new GridLayout(3, 1));
        panelLabels.add(panelLabelsScanner);
        panelLabels.add(viewPanel);
        panelLabels.add(panelLabelsModel);

        tabbedPane.addTab("Positions", null, panelLabels);
    }

    /**
     * Build the light control panel for the surface render.
     */
    public void buildLightPanel() {
        lightPanel = new JPanel();
        if ( surRender != null ) {
            lightPanel.add(surRender.getSurfaceDialog().getLightDialog().getMainPanel());
          } else if ( surView != null ) {
            lightPanel.add(surView.getSurfaceDialog().getLightDialog().getMainPanel());
          }
        maxPanelWidth = Math.max(lightPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the mouse control panel for the raycast render.
     */
    public void buildMousePanel() {
        mousePanel = new JPanel();
        if ( surRender != null ) {
            mousePanel.add(surRender.getMouseDialog().getMainPanel());
          } else if ( surView != null ) {
          	mousePanel.add(surView.getMouseDialog().getMainPanel());
          }
        maxPanelWidth = Math.max(mousePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the volume opacity control panel for the surface render.
     */
    public void buildOpacityPanel() {
        opacityPanel = new JPanel();

        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();

        opacityPanel.setLayout(gbLayout);
        gbConstraints.weightx = 1;
        gbConstraints.weighty = 1;
        gbConstraints.fill = GridBagConstraints.BOTH;
        gbConstraints.anchor = GridBagConstraints.NORTH;
        gbLayout.setConstraints(surRender.getVolOpacityPanel().getMainPanel(), gbConstraints);
        opacityPanel.add(surRender.getVolOpacityPanel().getMainPanel());
        maxPanelWidth = Math.max(opacityPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the adding surface control panel for the surface render.
     */
    public void buildProbePanel() {
        probePanel = new JPanel();
        if ( surRender != null ) {
            probePanel.add(surRender.getProbeDialog().getMainPanel());
          } else if ( surView != null ) {
          	probePanel.add(surView.getProbeDialog().getMainPanel());
          }
        maxPanelWidth = Math.max(probePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the camera control panel for the raycast render.
     */
    public void buildRayCastCameraPanel() {
        raycastCameraPanel = new JPanel();
        raycastCameraPanel.add(raycastRender.getCameraControl().getMainPanel());
        maxPanelWidth = Math.max(raycastCameraPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the view control panel for the raycast render.
     */
    public void buildRayCastOptions() {
        raycastOptionsPanel = new JPanel();
        raycastOptionsPanel.add(raycastRender.getOptions());
        maxPanelWidth = Math.max(raycastOptionsPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the Sculpturing control panel.
     */
    public void buildSculpt() {
        m_kSculptPanel = new JPanel();
        m_kSculptPanel.add(surRender.getSculptorPanel().getMainPanel());
        maxPanelWidth = Math.max(m_kSculptPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the camera control panel for the shearwarp render.
     */
    public void buildShearWarpCameraPanel() {
        shearwarpCameraPanel = new JPanel();
        shearwarpCameraPanel.add(shearwarpRender.getCameraControl().getMainPanel());
        maxPanelWidth = Math.max(shearwarpCameraPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the view control panel for the shearwarp render.
     */
    public void buildShearWarpOptions() {
        shearwarpOptionsPanel = new JPanel();
        shearwarpOptionsPanel.add(shearwarpRender.getOptions());
        maxPanelWidth = Math.max(shearwarpOptionsPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the slices control panel for the surface render.
     */
    public void buildSlicePanel() {
        slicePanel = new JPanel();
        slicePanel.add(surRender.getSlicePanel().getMainPanel());
        maxPanelWidth = Math.max(slicePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the adding surface control panel for the surface render.
     */
    public void buildSurfacePanel() {
        surfacePanel = new JPanel();
        if ( surRender != null ) {
            surfacePanel.add(surRender.getSurfaceDialog().getMainPanel());
           } else if ( surView != null ) { 
           	surfacePanel.add(surView.getSurfaceDialog().getMainPanel());
           }
        maxPanelWidth = Math.max(surfacePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the view control panel for the surface render.
     */
    public void buildViewPanel() {
        viewPanel = new JPanel();
        if ( surRender != null ) {
            viewPanel.add(surRender.getViewDialog().getMainPanel());
          } else if ( surView != null ) {
          	viewPanel.add(surView.getViewDialog().getMainPanel());
          }
        maxPanelWidth = Math.max(viewPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Transform the image into shear warp image, which has the same resolution.
     *
     * @param  _imageA  the reference to image A.
     * @param  _imageB  the reference to image B.
     */
    public void calcShearWarpImage(ModelImage _imageA, ModelImage _imageB) {
        float resolsX = Math.abs(_imageA.getResolutions(0)[0]);
        float resolsY = Math.abs(_imageA.getResolutions(0)[1]);
        float resolsZ = Math.abs(_imageA.getResolutions(0)[2]);

        if ((resolsX != resolsY) || (resolsX != resolsZ)) {
            AlgorithmReslice resliceAlgo = new AlgorithmReslice(_imageA, AlgorithmReslice.LINEAR);

            resliceAlgo.setRunningInSeparateThread(false);
            resliceAlgo.run();
            isoImageA = resliceAlgo.getResultImage();

            if (_imageA.isColorImage()) {
                int[] RGBExtents = new int[2];

                RGBExtents[0] = 4;
                RGBExtents[1] = 256;
                RGBTA = new ModelRGB(RGBExtents);
            } else {
                LUTa = initLUT(isoImageA);
            }

            if (_imageB != null) {
                resliceAlgo = new AlgorithmReslice(_imageB, AlgorithmReslice.LINEAR);
                resliceAlgo.setRunningInSeparateThread(false);
                resliceAlgo.run();
                isoImageB = resliceAlgo.getResultImage();

                if (_imageB.isColorImage()) {
                    int[] RGBExtents = new int[2];

                    RGBExtents[0] = 4;
                    RGBExtents[1] = 256;
                    RGBTB = new ModelRGB(RGBExtents);
                } else {
                    LUTb = initLUT(isoImageB);
                }
            }

            resliceAlgo.finalize();
            resliceAlgo = null;
        }
    }

    /**
     * Method called when a component resize event is generated. This method snaps the size of the frame and pagePanel
     * to the nearest row, column sizing (so the gridRow and gridColumn and page layout may change).
     *
     * @param  event  frame resize event
     */
    public synchronized void componentResized(ComponentEvent event) {
        resizePanel();
    }

    /**
     * Construct the volume rendering methods based on the choices made from the resample dialog. This method is called
     * by the Resample dialog.
     */
    public void constructRenderers() {

        /** Progress bar show up during the volume view frame loading */
        progressBar = new ViewJProgressBar("Constructing renderers...", "Constructing renderers...", 0, 100, false,
                                           null, null);
        progressBar.updateValue(0, true);
        MipavUtil.centerOnScreen(progressBar);
        progressBar.setVisible(true);
        progressBar.updateValueImmed(1);

        try {

            if ((leftPanelRenderMode == SURFACE) || (rightPanelRenderMode == SURFACE)) {
                isSurfaceRenderEnable = true;
            }

            if ((leftPanelRenderMode == SURFACEVIEW) ) {
                isSurfaceViewEnable = true;
            }

            
            if ((leftPanelRenderMode == RAYCAST) || (rightPanelRenderMode == RAYCAST)) {
                isRayCastEnable = true;
            }

            if ((leftPanelRenderMode == SHEARWARP) || (rightPanelRenderMode == SHEARWARP)) {
                isShearWarpEnable = true;
            }

            if (rightPanelRenderMode == BRAINSURFACE_FLATTENER) {
                isBrainsurfaceFlattenerEnable = true;
            }

            if (leftPanelRenderMode == ENDOSCOPY) {
                leftPanelRenderMode = SURFACE;
                rightPanelRenderMode = ENDOSCOPY;
                isSurfaceRenderEnable = true;
                isEndoscopyEnable = true;
            }

            
            serif12 = MipavUtil.font12;
            serif12B = MipavUtil.font12B;
            config = SimpleUniverse.getPreferredConfiguration();
            progressBar.updateValueImmed(5);
            
            if ( isSurfaceViewEnable ) {
           	    progressBar.setMessage("Constructing surface view...");

                // TODO: Check 3D support from the card, and report to the user if the card support 3D or not.
                surView = new SurfaceView(imageA, LUTa, imageB, LUTb, this, config, progressBar);
               

                if (surView != null) {
               	 surView.configureVolumeFrame();
                }
            }
           
            if (isSurfaceRenderEnable ) {
                progressBar.setMessage("Constructing surface renderer...");

                // TODO: Check 3D support from the card, and report to the user if the card support 3D or not.
                surRender = new SurfaceRender(imageA, LUTa, imageB, LUTb, this, config, progressBar);
                surRender.setVolView(true);

                if (surRender != null) {
                    surRender.configureVolumeFrame();
                }

                m_akPlaneRender = new PlaneRender[3];
                m_akPlaneRender[0] = new PlaneRender(this, imageA, LUTa, imageB, LUTb, config, FileInfoBase.AXIAL,
                                                     false);
                m_akPlaneRender[1] = new PlaneRender(this, imageA, LUTa, imageB, LUTb, config, FileInfoBase.SAGITTAL,
                                                     false);
                m_akPlaneRender[2] = new PlaneRender(this, imageA, LUTa, imageB, LUTb, config, FileInfoBase.CORONAL,
                                                     false);
            }

            if (isShearWarpEnable) {
                progressBar.setMessage("Constructing shearwarp renderer...");

                if (isoImageA == null) {
                    shearwarpRender = new VolumeRendererShearWarp(imageA, LUTa, imageB, LUTb, surRender, config,
                                                                  progressBar);
                } else {
                    shearwarpRender = new VolumeRendererShearWarp(isoImageA, LUTa, isoImageB, LUTb, surRender, config,
                                                                  progressBar);
                }

                if (surRender != null) {
                    surRender.setRayBasedRender(shearwarpRender);
                }
            }

            if (isRayCastEnable) {
                progressBar.setMessage("Constructing raycast renderer...");
                raycastRender = new VolumeRendererRayCast(imageA, LUTa, imageB, LUTb, surRender, config, progressBar);

                if (surRender != null) {
                    surRender.setRayBasedRender(raycastRender);
                }
            }

            if (isEndoscopyEnable) {
                progressBar.setMessage("Constructing flythru renderer...");
                flythruRender = new FlythruRender(imageA, config, this);
                surRender.getGeodesicPanel().setFlythruRender(flythruRender);
            }

            if (isBrainsurfaceFlattenerEnable) {
                progressBar.setMessage("Constructing brainsurface flattener renderer...");
                brainsurfaceFlattenerRender = new MjCorticalAnalysis(imageA, imageB, config, this);

            }

            progressBar.updateValueImmed(80);
            progressBar.setMessage("Constructing Lookup Table...");
            

    		ModelStorageBase selectedLUTa = imageA.isColorImage() ? RGBTA : LUTa;
    		ModelStorageBase selectedLUTb = (imageB != null) ? imageB.isColorImage() ? RGBTB : LUTb : null;
    		panelHistogram = new JFrameHistogram( this, imageA, imageB, selectedLUTa, selectedLUTb );
    		panelHistogram.histogramLUT(true, false);

            progressBar.updateValueImmed(100);

            this.configureFrame();
        } finally {
            progressBar.dispose();
        }

        if (isEndoscopyEnable) {
            enableFlyThruRender();
        }

        if (isRayCastEnable || isShearWarpEnable || isBrainsurfaceFlattenerEnable) {
            enableDualPanelRender();
        }

        if (imageA.isColorImage()) {
            setRGBTA(RGBTA);

            if ((imageB != null) && imageB.isColorImage()) {
                setRGBTB(RGBTB);
            }

            updateImages(true);
        }

        // Toolkit.getDefaultToolkit().setDynamicLayout( false );
    }

    /**
     * Disable target point for the RFA probe from within the plane renderer.
     */
    public void disableTargetPointPicking() {
        if ( m_akPlaneRender != null ) {
	        for (int iPlane = 0; iPlane < 3; iPlane++) {
	            m_akPlaneRender[iPlane].enableTargetPointPicking(false);
	        }
        }
    }

    /**
     * Dispose memory.
     *
     * @param  flag  call super dispose or not
     */
    public void disposeLocal(boolean flag) {
        // System.out.println( "######ViewJFrameVolView disposeLocal" );

        /** Control panels for the raycast render */
        raycastOptionsPanel = null;
        raycastCameraPanel = null;

        /* Geodesic panel */
        m_kGeodesicPanel = null;

        /* Sculpturing panel */
        m_kSculptPanel = null;

        /** Control panels for the shearwarp render */
        shearwarpOptionsPanel = null;
        shearwarpCameraPanel = null;

        histoLUTPanel = null;
        displayPanel = null;
        viewPanel = null;
        lightPanel = null;
        clipPanel = null;
        panelLabels = null;
        slicePanel = null;
        opacityPanel = null;
        surfacePanel = null;
        cameraPanel = null;
        mousePanel = null;

        clipBox = null;
        volToolBar = null;

        if (paintGrowDialog != null) {
            paintGrowDialog.dispose();
            paintGrowDialog = null;
        }

        if (intensityDialog != null) {
            intensityDialog.dispose();
            intensityDialog = null;
        }

        if (opacityDialog != null) {
            opacityDialog.dispose();
            opacityDialog = null;
        }

        if (surRender != null) {
            surRender.close();
            surRender = null;
        }

        if (surView != null) {
        	surView.close();
        	surView = null;
        }
        
        if (raycastRender != null) {
            raycastRender.close();
            raycastRender = null;
        }

        if (shearwarpRender != null) {
            shearwarpRender.disposeLocal();
            shearwarpRender = null;
        }

        if (flythruRender != null) {
            flythruRender.dispose();
            flythruRender = null;
        }

        if (flythruControl != null) {
            flythruControl.dispose(true);
            flythruControl = null;
        }

        if (flythruMoveControl != null) {
            flythruMoveControl.dispose(true);
            flythruMoveControl = null;
        }

        if (brainsurfaceFlattenerRender != null) {
            brainsurfaceFlattenerRender = null;

            if (m_kBrainsurfaceFlattenerPanel != null) {
                m_kBrainsurfaceFlattenerPanel = null;
            }
        }
        panelHistogram.disposeLocal();
        panelHistogram = null;

        if (resampleDialog != null) {
            resampleDialog.disposeLocal();
            resampleDialog = null;
        }

        if ( m_akPlaneRender != null ) {
	        for (int i = 0; i < 3; i++) {
	
	            if (m_akPlaneRender[i] != null) {
	                m_akPlaneRender[i].disposeLocal();
	                m_akPlaneRender[i] = null;
	            }
	        }
        }

        if (imageA != null) {
            imageA.removeImageDisplayListener(this);
            imageA.disposeLocal();
            imageA = null;
        }

        if (imageB != null) {
            imageB.removeImageDisplayListener(this);
            imageB.disposeLocal();
            imageB = null;
        }

        // hack using the flag parameter to prevent a second resetting of the progress bar when
        // the finalizer comes around (window closing does the first one with flag = true)
        if (flag && (rendererProgressBar != null)) {
            viewToolBar.remove(getRendererProgressBar());
            rendererProgressBar = null;
        }
    }

    /**
     * Insert the blank images to the end of image. Padding the image to power of 2.
     *
     * @param  extents     int[] original extents
     * @param  volExtents  int[] padding to power of 2 extents.
     */
    public void doPadding(int[] extents, int[] volExtents) {
        ModelImage blankImage;
        AlgorithmConcat mathAlgo;

        int[] destExtents = null;

        destExtents = new int[3];
        destExtents[0] = imageA.getExtents()[0];
        destExtents[1] = imageA.getExtents()[1];
        destExtents[2] = volExtents[2] - extents[2];

        blankImage = new ModelImage(imageA.getType(), destExtents, imageA.getImageName());

        for (int i = 0; i < blankImage.getSize(); i++) {
            blankImage.set(i, imageA.getMin());
        }

        destExtents[2] = imageA.getExtents()[2] + blankImage.getExtents()[2];

        paddingImageA = new ModelImage(imageA.getType(), destExtents, imageA.getImageName());

        try {
            mathAlgo = new AlgorithmConcat(imageA, blankImage, paddingImageA);
            setVisible(false);
            mathAlgo.run();

            if (mathAlgo.isCompleted()) {
                mathAlgo.finalize();
                mathAlgo = null;
            }

        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Concatenation: unable to allocate enough memory");

            return;
        }

        JDialogBase.updateFileInfoStatic(imageA, paddingImageA);
        paddingImageA.calcMinMax();

        imageA.disposeLocal();

        imageA = paddingImageA;

        if (imageB != null) {
            paddingImageB = new ModelImage(imageB.getType(), destExtents, imageB.getImageName());

            try {
                mathAlgo = new AlgorithmConcat(imageB, blankImage, paddingImageB);
                setVisible(false);
                mathAlgo.run();

                if (mathAlgo.isCompleted()) {
                    mathAlgo.finalize();
                    mathAlgo = null;
                }

            } catch (OutOfMemoryError x) {
                System.gc();
                MipavUtil.displayError("Dialog Concatenation: unable to allocate enough memory");

                return;
            }

            JDialogBase.updateFileInfoStatic(imageB, paddingImageB);
            paddingImageB.calcMinMax();
            imageB.disposeLocal();

            imageB = paddingImageB;
        }

        blankImage.disposeLocal();
    }

    /**
     * Resample the images to power of 2.
     *
     * @param  volExtents     resampled volume extents
     * @param  newRes         new resampled resolution
     * @param  forceResample  resampled or not
     * @param  nDim           number of dimensions
     * @param  iFilterType    type of sample filter, may be one of 7 different filters: TriLinear Interpolation,
     *                        NearestNeighbor, CubicBSpline, QuadraticBSpline, CubicLagragian, QuinticLagragian,
     *                        HepticLagragian, or WindowedSinc (see AlgorithmTransform.java).
     */
    public void doResample(int[] volExtents, float[] newRes, boolean forceResample, int nDim, int iFilterType) {
        AlgorithmTransform transformFunct = null;

        if (forceResample) {

            // resample imageA
            if (nDim >= 3) {
                transformFunct = new AlgorithmTransform(imageA, new TransMatrix(4), iFilterType, newRes[0], newRes[1],
                                                        newRes[2], volExtents[0], volExtents[1], volExtents[2], false,
                                                        true, false);
            } else { // Should never even get here!

                // Maybe some error message and close dialog
            }

            transformFunct.setRunningInSeparateThread(false);
            transformFunct.run();

            if (transformFunct.isCompleted() == false) {

                // What to do
                transformFunct.finalize();
                transformFunct = null;
            }

            imageA = transformFunct.getTransformedImage();
            imageA.calcMinMax();

            if (!imageA.isColorImage()) {
                resetLUTMinMax(imageA, LUTa);
            }

            if (transformFunct != null) {
                transformFunct.disposeLocal();
            }

            transformFunct = null;
        }

        // resample imageB
        if ((imageB != null) && forceResample) {

            // Resample image into volume that is a power of two !
            Preferences.debug("ViewJFrameSurfaceRenderer.buildTexture: Volume resampled.");

            if (nDim >= 3) {
                transformFunct = new AlgorithmTransform(imageB, new TransMatrix(4), iFilterType, newRes[0], newRes[1],
                                                        newRes[2], volExtents[0], volExtents[1], volExtents[2], false,
                                                        true, false);
            } else { }

            transformFunct.setRunningInSeparateThread(false);
            transformFunct.run();

            if (transformFunct.isCompleted() == false) {

                // What to do
                transformFunct.finalize();
                transformFunct = null;
            }

            imageB = transformFunct.getTransformedImage();
            imageB.calcMinMax();

            if (!imageB.isColorImage()) {
                resetLUTMinMax(imageB, LUTb);
            }
        }
    }

    /**
     * Draws the point selected on the Mesh for the brainsurfaceFlattener.
     *
     * @param  kStart   Ruida please add comment
     * @param  aiIndex  Ruida please add comment
     * @param  iWhich   Ruida please add comment
     */
    public void drawBrainSurfaceFlattenerPoint(Point3f kStart, int[] aiIndex, int iWhich) {
        brainsurfaceFlattenerRender.drawPicked(kStart, aiIndex, iWhich);
    }

    /**
     * Called from the PlaneRender class when a new Probe Entry Point has been selected. The point is passed into each
     * PlaneRender class for display, and to the SurfaceRender class for display
     *
     * @param  kPoint  target point position
     */
    public void drawRFAPoint(Point3f kPoint) {
        if ( m_akPlaneRender != null ) {
		    for (int iPlane = 0; iPlane < 3; iPlane++) {
		        m_akPlaneRender[iPlane].drawRFAPoint(kPoint);
		        m_akPlaneRender[iPlane].update();
		    }
        }

        surRender.drawRFAPoint(kPoint);
    }

    /**
     * Enable target point for the RFA probe from within the plane renderer.
     */
    public void enableTargetPointPicking() {
        if ( m_akPlaneRender != null ) {
		    for (int iPlane = 0; iPlane < 3; iPlane++) {
		        m_akPlaneRender[iPlane].enableTargetPointPicking(true);
		    }
        }
    }

    /**
     * Get the imageA and imageB blending value from the PlaneRender.
     *
     * @return  blendValue blender slider value.
     */
    public int getBlendValue() {
        return surRender.getVolOpacityPanel().getAlphaBlendSliderValue();
    }

    /**
     * Required by the parent super class, do nothing.
     *
     * @return  null
     */
    public ViewControlsImage getControls() {
        return null;
    }

    /**
     * Returns which image is active in the HistoLUT -- either imageA or imageB. Called by the PlaneRenderer object to
     * determine which LUT to update based on dragging the right-mouse in the PlaneRender window:
     *
     * @return  ModelImage, either imageA or imageB, depending on which is selected in the HistoLUT
     */
    public ModelImage getHistoLUTActiveImage() {

    	if ( panelHistogram.isImageASelected() )
    	{
    		return imageA;
    	}
    	else if  ( panelHistogram.isImageBSelected() )
    	{
    		return imageB;
    	}

        return null;
    }

    /**
     * Returns which image is active in the HistoRGB -- either imageA or imageB. Called by the PlaneRenderer object to
     * determine which LUT to update based on dragging the right-mouse in the PlaneRender window:
     *
     * @return  ModelImage, either imageA or imageB, depending on which is selected in the HistoLUT
     */
    public ModelImage getHistoRGBActiveImage() {
    	return getHistoLUTActiveImage();
    }

    /**
     * Get the image A reference.
     *
     * @return  imageA model image A reference.
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Get the imageB reference.
     *
     * @return  imageB model image B reference.
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Get a reference to the original image we passed into the renderer from MIPAV (non-cloned).
     *
     * @return  the original image
     */
    public ModelImage getImageOriginal() {
        return imageAOriginal;
    }


    /**
     * Get the LUT panel (only should be used with grayscale images).
     *
     * @return  the histo LUT panel
     * @deprecated
     */
    public JPanelHistoLUT getLUTDialog() {
    	return null;
    }

    public JFrameHistogram getHistogramDialog() {
        return panelHistogram;
    }


    /**
     * Return the rfa probe panel.
     *
     * @return  the rfa probe panel
     */
    public JPanelProbe getProbeDialog() {
    	if ( surRender != null ) {
            return surRender.getProbeDialog();
      	} else if ( surView != null ){
      		return surView.getProbeDialog();
      	} else {
      		return null;
      	}
    }

    /**
     * Get the RGB panel (only should be used with color images).
     *
     * @return  the histo RGB panel
     * @deprecated
     */
    public JPanelHistoRGB getRGBDialog() {
    	return null;
    }

    /**
     * Return the segmentation region map image which contains info on where the vascualture, etc are located.
     *
     * @return  (vessel, etc) segmentation image
     */
    public ModelImage getSegmentationImage() {

        if (surRender != null) {
            return surRender.getSegmentationImage();
        } else {
            return null;
        }
    }


    /**
     * Return the image panel.
     *
     * @return  JSplitPane
     */
    public JSplitPane getViewPanel() {
        return rightPane;
    }

    /**
     * Insert tab into the flythru tab list ( flythru render ) for backup.
     *
     * @param  _name   flythru render control panel name
     * @param  _panel  flythru render control panel
     */
    public void insertFlythruTab(String _name, JPanel _panel) {
        int i;

        for (i = 0; i < flythruTabVector.size(); i++) {

            if ((flythruTabVector.elementAt(i) != null) &&
                    ((TabbedItem) (flythruTabVector.elementAt(i))).name.equals(_name)) {
                return;
            }
        }

        flythruTabVector.add(new TabbedItem(_name, _panel));
    }

    /**
     * Insert tab into the raycast tab list ( raycast render ) for backup.
     *
     * @param  _name   raycast render control panel name
     * @param  _panel  raycast render control panel
     */
    public void insertRaycastTab(String _name, JPanel _panel) {
        int i;

        for (i = 0; i < raycastTabVector.size(); i++) {

            if ((raycastTabVector.elementAt(i) != null) &&
                    ((TabbedItem) (raycastTabVector.elementAt(i))).name.equals(_name)) {
                return;
            }
        }

        raycastTabVector.add(new TabbedItem(_name, _panel));
    }

    /**
     * Insert tab into the shear warp tab list ( shear warp render ) for backup.
     *
     * @param  _name   shear warp render control panel name
     * @param  _panel  shear warp render control panel
     */
    public void insertShearwarpTab(String _name, JPanel _panel) {
        int i;

        for (i = 0; i < shearwarpTabVector.size(); i++) {

            if ((shearwarpTabVector.elementAt(i) != null) &&
                    ((TabbedItem) (shearwarpTabVector.elementAt(i))).name.equals(_name)) {
                return;
            }
        }

        shearwarpTabVector.add(new TabbedItem(_name, _panel));
    }

    /**
     * Insert tab into the surface tab list ( SurfaceRender ) for backup.
     *
     * @param  _name   surface render control panel name
     * @param  _panel  surface render control panel
     */
    public void insertSurfaceTab(String _name, JPanel _panel) {
        int i;

        for (i = 0; i < surTabVector.size(); i++) {

            if ((surTabVector.elementAt(i) != null) && ((TabbedItem) (surTabVector.elementAt(i))).name.equals(_name)) {
                return;
            }
        }

        surTabVector.add(new TabbedItem(_name, _panel));
    }

    /**
     * Insert the new tab into the current visible tab list.
     *
     * @param  _name   control panel name
     * @param  _panel  control panel
     */
    public void insertTab(String _name, JPanel _panel) {
        int i;

        for (i = 0; i < tabbedPane.getTabCount(); i++) {

            if ((tabbedPane.getComponentAt(i) != null) && tabbedPane.getTitleAt(i).equals(_name)) {
                tabbedPane.setSelectedIndex(i);

                return;
            }
        }

        tabbedPane.addTab(_name, null, _panel);
        tabbedPane.setSelectedIndex(tabbedPane.getTabCount() - 1);
    }

    /**
     * Returns the pick enabled state of the brainsurfaceFlattener.
     *
     * @return  flag brain surface pickable or not.
     */
    public boolean isBrainSurfaceFlattenerPickEnabled() {

        if (brainsurfaceFlattenerRender != null) {
            return brainsurfaceFlattenerRender.isPickEnabled();
        }

        return false;
    }

    /**
     * Check whether the Geodesic drawing is enabled or not.
     *
     * @return  boolean <code>true</code> Geodesic drawing enabled, <code>false</code> Geodesic disable.
     */
    public boolean isGeodesicEnable() {
    	if ( surRender != null ) {
            return surRender.getGeodesicPanel().isGeodesicEnable();
      	} else if ( surView != null ) {
      		return surView.getGeodesicPanel().isGeodesicEnable();
      	} else {
      		return false;
      	}
    }

    /**
     * Sets the flags for the getOptionses and resets labels.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (raycastRender != null) {

            if (radioMIP.isSelected() && (source == radioMIP)) {
                raycastRender.setRenderMode(ViewJComponentRenderImage.ModeMIP);
                raycastRender.MIPMode();
            } else if (radioXRAY.isSelected() && (source == radioXRAY)) {
                raycastRender.setRenderMode(ViewJComponentRenderImage.ModeXRAY);
                raycastRender.DRRMode();
            } else if (radioCOMPOSITE.isSelected() && (source == radioCOMPOSITE)) {
                raycastRender.setRenderMode(ViewJComponentRenderImage.ModeCOMPOSITE);
                raycastRender.CMPMode();
            } else if (radioSURFACE.isSelected() && (source == radioSURFACE)) {
                raycastRender.setRenderMode(ViewJComponentRenderImage.ModeSURFACE);
                raycastRender.SURMode();
            } else if (radioSURFACEFAST.isSelected() && (source == radioSURFACEFAST)) {
                raycastRender.setRenderMode(ViewJComponentRenderImage.ModeSURFACEFAST);
                raycastRender.SURFASTMode();
            }

        }

        if (shearwarpRender != null) {

            if (radioMIPShear.isSelected() && (source == radioMIPShear)) {
                shearwarpRender.setRenderMode(ViewJComponentRenderImage.ModeMIP);
                shearwarpRender.MIPMode();
            } else if (radioXRAYShear.isSelected() && (source == radioXRAYShear)) {
                shearwarpRender.setRenderMode(ViewJComponentRenderImage.ModeXRAY);
                shearwarpRender.DRRMode();
            } else if (radioCOMPOSITEShear.isSelected() && (source == radioCOMPOSITEShear)) {
                shearwarpRender.setRenderMode(ViewJComponentRenderImage.ModeCOMPOSITE);
                shearwarpRender.CMPMode();
            } else if (radioSURFACEShear.isSelected() && (source == radioSURFACEShear)) {
                shearwarpRender.setRenderMode(ViewJComponentRenderImage.ModeSURFACE);
                shearwarpRender.SURMode();
            }

        }

        if (surRender != null) {

            if (radioSurrenderCOMPOSITE.isSelected() && (source == radioSurrenderCOMPOSITE)) {
                surRender.setCompositeMode();
            } else if (radioSurrenderLIGHT.isSelected() && (source == radioSurrenderLIGHT)) {
                surRender.setLightingMode();
            }
        }

    }

    /**
     * Handle the double mouse click event when the use swith between the dual image panel view.
     *
     * @param  e  MouseEvent
     */
    public void mouseClicked(MouseEvent e) {
        Object source = e.getSource();

        if (e.getClickCount() == 2  && SwingUtilities.isLeftMouseButton(e)) {

            if ((surRender != null) && (source == surRender.getCanvas())) {
                switchTabList("SurRender");

                if (leftPanelRenderMode == SURFACE) {
                    dualLeftPanel.setBorder(redBorder);
                    dualRightPanel.setBorder(loweredbevel);
                    dualLeftPanel.repaint();
                    dualRightPanel.repaint();
                } else if (rightPanelRenderMode == SURFACE) {
                    dualLeftPanel.setBorder(loweredbevel);
                    dualRightPanel.setBorder(redBorder);
                    dualLeftPanel.repaint();
                    dualRightPanel.repaint();
                }

                if (isShearWarpEnable) {
                    panelToolbar.remove(shearWarpToolBar);
                }

                if (isRayCastEnable) {
                    panelToolbar.remove(rayCastToolBar);
                }

                if (isSurfaceRenderEnable) {
                    panelToolbar.remove(volToolBar);
                }

                if (isEndoscopyEnable) {
                    panelToolbar.remove(flyThruToolbar);
                }

                GridBagConstraints gbc = new GridBagConstraints();

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.BOTH;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.weightx = 1;
                gbc.weighty = 1;
                panelToolbar.add(volToolBar, gbc);
                panelToolbar.validate();
                panelToolbar.repaint();
            } else if ((flythruRender != null) && (source == flythruRender.getCanvas())) {
                switchTabList("FlythruRender");
                dualLeftPanel.setBorder(loweredbevel);
                dualRightPanel.setBorder(redBorder);
                dualLeftPanel.repaint();
                dualRightPanel.repaint();

                if (isSurfaceRenderEnable) {
                    panelToolbar.remove(volToolBar);
                }

                if (isEndoscopyEnable) {
                    panelToolbar.remove(flyThruToolbar);
                }

                GridBagConstraints gbc = new GridBagConstraints();

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.BOTH;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.weightx = 1;
                gbc.weighty = 1;
                panelToolbar.add(flyThruToolbar, gbc);
                panelToolbar.validate();
                panelToolbar.repaint();
            } else if ((raycastRender != null) && (source == raycastRender.getCanvas())) {
                switchTabList("VolRender");

                if (rightPanelRenderMode == RAYCAST) {
                    dualLeftPanel.setBorder(loweredbevel);
                    dualRightPanel.setBorder(redBorder);
                    dualLeftPanel.repaint();
                    dualRightPanel.repaint();
                } else if (leftPanelRenderMode == RAYCAST) {
                    dualLeftPanel.setBorder(redBorder);
                    dualRightPanel.setBorder(loweredbevel);
                    dualLeftPanel.repaint();
                    dualRightPanel.repaint();
                }

                if (isShearWarpEnable) {
                    panelToolbar.remove(shearWarpToolBar);
                }

                if (isRayCastEnable) {
                    panelToolbar.remove(rayCastToolBar);
                }

                if (isSurfaceRenderEnable) {
                    panelToolbar.remove(volToolBar);
                }

                GridBagConstraints gbc = new GridBagConstraints();

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.BOTH;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.weightx = 1;
                gbc.weighty = 1;
                panelToolbar.add(rayCastToolBar, gbc);
                panelToolbar.validate();
                panelToolbar.repaint();
            } else if ((shearwarpRender != null) && (source == shearwarpRender.getCanvas())) {
                switchTabList("ShearRender");

                if (rightPanelRenderMode == SHEARWARP) {
                    dualLeftPanel.setBorder(loweredbevel);
                    dualRightPanel.setBorder(redBorder);
                    dualLeftPanel.repaint();
                    dualRightPanel.repaint();
                } else if (leftPanelRenderMode == SHEARWARP) {
                    dualLeftPanel.setBorder(redBorder);
                    dualRightPanel.setBorder(loweredbevel);
                    dualLeftPanel.repaint();
                    dualRightPanel.repaint();
                }

                if (isShearWarpEnable) {
                    panelToolbar.remove(shearWarpToolBar);
                }

                if (isRayCastEnable) {
                    panelToolbar.remove(rayCastToolBar);
                }

                if (isSurfaceRenderEnable) {
                    panelToolbar.remove(volToolBar);
                }

                GridBagConstraints gbc = new GridBagConstraints();

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.BOTH;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.weightx = 1;
                gbc.weighty = 1;
                panelToolbar.add(shearWarpToolBar, gbc);
                panelToolbar.validate();
                panelToolbar.repaint();
            } else if ((brainsurfaceFlattenerRender != null) && (source == brainsurfaceFlattenerRender.getCanvas())) {

                if (rightPanelRenderMode == BRAINSURFACE_FLATTENER) {
                    dualLeftPanel.setBorder(loweredbevel);
                    dualRightPanel.setBorder(redBorder);
                    dualLeftPanel.repaint();
                    dualRightPanel.repaint();
                }

                if (isSurfaceRenderEnable) {
                    panelToolbar.remove(volToolBar);
                }
            }
        }

    }

    /**
     * Methods do nothing, implemented mouseListener.
     *
     * @param  e  MouseEvent
     */
    public void mouseEntered(MouseEvent e) { }

    /**
     * Methods do nothing, implemented mouseListener.
     *
     * @param  e  MouseEvent
     */
    public void mouseExited(MouseEvent e) { }

    /**
     * Methods do nothing, implemented mouseListener.
     *
     * @param  e  MouseEvent
     */
    public void mousePressed(MouseEvent e) { }

    /**
     * Methods do nothing, implemented mouseListener.
     *
     * @param  e  MouseEvent
     */
    public void mouseReleased(MouseEvent e) { }

    /**
     * Updates the surRender -- removes a BranchGroup to the main Display.
     *
     * @param  kBranch      BranchGroup surface branch group reference.
     * @param  bRemoveMesh  boolean flag to remove the surface mesh or not
     */
    public void removeBranch(BranchGroup kBranch, boolean bRemoveMesh) {

        if ((surRender != null) && (surRender.getSurfaceDialog() != null)) {
            surRender.getSurfaceDialog().removeBranch(kBranch, bRemoveMesh);
        }
    }

    /**
     * Required by the parent super class, do nothing.
     */
    public void removeControls() { }

    /**
     * Remove the red line showing where the probe will pass through. Used when changing the probe target point through
     * the tri-images.
     */
    public void removeProbeLine() {

    	if (surRender != null) {
            surRender.getProbeDialog().removeProbingPath();
        } else if ( surView != null ) {
        	surView.getProbeDialog().removeProbingPath();
        }
    }

    /**
     * Reset image volume orieint along X axis.
     */
    public void resetAxisX() {

        if (isSurfaceRenderEnable) {
            surRender.resetAxisX();
        }

        if (isShearWarpEnable) {
            shearwarpRender.resetAxisX();
        }

        if (isRayCastEnable) {
            raycastRender.resetAxisX();
        }
    }

    /**
     * Reset image volume orieint along Y axis.
     */
    public void resetAxisY() {

        if (isSurfaceRenderEnable) {
            surRender.resetAxisY();
        }

        if (isShearWarpEnable) {
            shearwarpRender.resetAxisY();
        }

        if (isRayCastEnable) {
            raycastRender.resetAxisY();
        }
    }

    /**
     * Reset image volume orieint along Z axis.
     */
    public void resetImage() {

        if (isSurfaceRenderEnable) {
            surRender.resetImage();
        }

        if (isShearWarpEnable) {
            shearwarpRender.resetImage();
        }

        if (isRayCastEnable) {
            raycastRender.resetImage();
        }
    }


    /**
     * Required by the parent super class, do nothing.
     *
     * @param  active  int
     */
    public void setActiveImage(int active) { }

    /**
     * Required by the parent super class, do nothing.
     *
     * @param  value  DOCUMENT ME!
     */
    public void setAlphaBlend(int value) { }


    /**
     * Required by the parent super class, do nothing.
     */
    public void setControls() { }

    /**
     * Do nothing methods, just extend the ViewJframeBase.
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setEnabled(boolean flag) { }

    /**
     * Set the flythru surface color.
     *
     * @param  _color  ModelLUT table
     */
    public void setFlythruColor(Color _color) {

        if (flythruControl != null) {
            flythruControl.setColor(_color);
        }
    }

    /**
     * Required by the parent super class, do nothing.
     *
     * @param  _imageB  image to set the frame to
     */
    public void setImageB(ModelImage _imageB) { }

    /**
     * Set the reference to the original image we passed into the renderer from MIPAV (non-cloned).
     *
     * @param  img  the original image
     */
    public void setImageOriginal(ModelImage img) {
        imageAOriginal = img;
    }

    /**
     * Accessor that sets the LUT.
     *
     * @param  LUT  the LUT
     */
    public void setLUTa(ModelLUT LUT) {

        if (surRender != null) {
            surRender.setLUTa(LUT);
        }

        if (raycastRender != null) {
            raycastRender.setLUTa(LUT);
        }

        if (shearwarpRender != null) {
            shearwarpRender.setLUTa(LUT);
        }

    }

    /**
     * Accessor that sets the LUT.
     *
     * @param  LUT  the LUT
     */
    public void setLUTb(ModelLUT LUT) {

        if (surRender != null) {
            surRender.setLUTb(LUT);
        }

        if (raycastRender != null) {
            raycastRender.setLUTb(LUT);
        }

        if (shearwarpRender != null) {
            shearwarpRender.setLUTb(LUT);
        }
    }

    /**
     * Set material ( texture or voxels ) shininess value.
     *
     * @param  value  float
     */
    public void setMaterialShininess(float value) {

        if (raycastRender != null) {
            raycastRender.setMaterialShininess(value);
        }

        if (surRender != null) {
            surRender.setMaterialShininess(value);
        }
        
        if (surView != null) {
        	surView.setMaterialShininess(value);
        }
    }

    /**
     * Required by the parent super class, do nothing.
     *
     * @param  paintBitmapSwitch  boolean
     */
    public void setPaintBitmapSwitch(boolean paintBitmapSwitch) { }

    /**
     * Called when the view position changes in the FlyThruRenderer, updates the position representation in the Slice
     * views:
     *
     * @param  kPosition        center position.
     * @param  kScaledPosition  position of the flythru view point along the flythru path.
     */
    public void setPathPosition(Point3f kPosition, Point3f kScaledPosition) {

    	Vector3f center = new Vector3f( kPosition.x * imageA.getExtents()[0], kPosition.y * imageA.getExtents()[1],
                kPosition.z * imageA.getExtents()[2] );
        WildMagic.LibFoundation.Mathematics.Vector3f kCenter = new WildMagic.LibFoundation.Mathematics.Vector3f(kPosition.x * imageA.getExtents()[0], kPosition.y * imageA.getExtents()[1],
                                        kPosition.z * imageA.getExtents()[2]);
        if ( m_akPlaneRender != null ) {
	        for (int iPlane = 0; iPlane < 3; iPlane++) {
	            m_akPlaneRender[iPlane].setCenter(kCenter);
	        }
        }

        surRender.setCenter(center);
        surRender.getSurfaceDialog().setPathPosition(kScaledPosition);

    }

    /**
     * Sets the position labels.
     *
     * @param  position  the slice positions in FileCoordinates.
     */
    public void setPositionLabels(WildMagic.LibFoundation.Mathematics.Vector3f position) {

        if (scannerLPSLabel == null) {
            return;
        }
        if (scannerRASLabel == null) {
            return;
        }

        setScannerPosition(position);
        setPatientSlicePosition(position);
        set3DModelPosition(position);
        setAbsPositionLabels(position);
    }


    /**
     * Sets the RGB table for ARGB image A.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTA(ModelRGB RGBT) {
        if ( m_akPlaneRender != null ) {
		    for (int iPlane = 0; iPlane < 3; iPlane++) {
		        m_akPlaneRender[iPlane].updateRGBTA(RGBT);
		    }
        }

        if (surRender != null) {
            surRender.setRGBTA(RGBT);
        }

        if (raycastRender != null) {
            raycastRender.setRGBTA(RGBT);
        }

        if (shearwarpRender != null) {
            shearwarpRender.setRGBTA(RGBT);
        }

    }

    /**
     * Sets the RGB table for image B.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTB(ModelRGB RGBT) {
        if ( m_akPlaneRender != null ) {
		    for (int iPlane = 0; iPlane < 3; iPlane++) {
		        m_akPlaneRender[iPlane].updateRGBTB(RGBT);
		    }
        }

        if (surRender != null) {
            surRender.setRGBTB(RGBT);
        }

        if (raycastRender != null) {
            raycastRender.setRGBTB(RGBT);
        }

        if (shearwarpRender != null) {
            shearwarpRender.setRGBTB(RGBT);
        }
    }

    /**
     * Update the right panel when finish loading Endoscopy images.
     */
    public void setRightPanelCanvas() {
        flythruRender.setupRenderControl(flythruControl);
        dualRightPanel.removeAll();
        dualLeftPanel.removeAll();
        dualRightPanel.add(flythruRender.getCanvas(), BorderLayout.CENTER);
        flythruRender.getCanvas().addMouseListener(this);
        dualLeftPanel.add(surRender.getCanvas(), BorderLayout.CENTER);
        dualRightPanel.validate();
        dualRightPanel.repaint();
        dualLeftPanel.validate();
        dualLeftPanel.repaint();
    }

    /**
     * Set the image which we can check to see if the probe is hitting anything important (such as vessels, etc).
     *
     * @param  img  segmentation image
     */
    public void setSegmentationImage(ModelImage img) {

        if (surRender != null) {
            surRender.setSegmentationImage(img);
        }
    }

    /**
     * Required by the parent super class, do nothing.
     *
     * @param  slice  int
     */
    public void setSlice(int slice) { }


    /**
     * Sets the position of the slices in the SurfaceRender and PlaneRender objects. Called from the PlaneRender class.
     *
     * @param  center  the new slice positions in FileCoordinates
     */
    public void setSliceFromPlane(WildMagic.LibFoundation.Mathematics.Vector3f kCenter) {
        setPositionLabels(kCenter);
        if ( m_akPlaneRender != null ) {
	        for (int i = 0; i < 3; i++) {
	            m_akPlaneRender[i].setCenter(kCenter);
	        }
        }
        
        if ( surRender != null ) {
        	Vector3f center = new Vector3f( kCenter.X, kCenter.Y, kCenter.Z );
        	surRender.setCenter(center);
        }
    }

    /**
     * Sets the position of the slices in the PlaneRender. Called from the SurfaceRender class.
     *
     * @param  center  the new slice positions in FileCoordinates
     */
    public void setSliceFromSurface(WildMagic.LibFoundation.Mathematics.Vector3f center) {
        setPositionLabels(center);

        if (m_akPlaneRender == null) {
            return;
        }
        for (int i = 0; i < 3; i++) {
            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].setCenter(center);
            }
        }
    }

    /**
     * Sets the color for the PlaneRender iView (AXIAL, SAGITTAL, CORONAL) slice.
     *
     * @param  iView  (AXIAL, SAGITTAL, CORONAL)
     * @param  color  the new axis color attribute.
     */
    public void setSliceHairColor(int iView, Color color) {
    	if (m_akPlaneRender == null) {
            return;
        }
    	
        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].setSliceHairColor(iView, new Color3f(color));
            m_akPlaneRender[iPlane].update();
        }
    }

    /**
     * Required by the parent super class, do nothing.
     *
     * @param  slice  int
     */
    public void setTimeSlice(int slice) { }

    /**
     * Set the title of the frame with the image name of slice location.
     */
    public void setTitle() {
        String str;

        if (displayMode == ViewJComponentBase.IMAGE_A) {
            str = imageA.getImageName();
            setTitle(str);
        } else {
            str = imageB.getImageName();
            setTitle(str);
        }
    }

    /**
     * Switch between slices control button and surface render button of the surface toolbar.
     *
     * @param  event  ChangeEvent
     */
    public void stateChanged(ChangeEvent event) {

        if ((surRender != null) && (event.getSource() == tabbedPane)) {

            if (tabbedPane.getSelectedComponent() == slicePanel) {
                surRender.switchToSliceView(true);
                clipBox.setVisible(false);
            } else if (tabbedPane.getSelectedComponent() == opacityPanel) {
                surRender.switchToVolView(true);
                clipBox.setVisible(true);
                // firstTimeVolView = false;
            }
        }
    }

    /**
     * Update the tabbed pane when switch view buttons in the View toolbar.
     *
     * @param  command  command of the renderer's view toolbar button click.
     */
    public void switchTabList(String command) {
        int i;
        int index = storeTabbedPaneIndex;
     
        storeTabbedPaneIndex = tabbedPane.getSelectedIndex();

        // remember what tabs were in use when switching to dual panel renderer
        Vector tempTabs = new Vector();

        for (i = 1; i < tabbedPane.getTabCount(); i++) {
            tempTabs.add(new TabbedItem(tabbedPane.getTitleAt(i), (JPanel) tabbedPane.getComponentAt(i)));
        }

        tabbedPane.removeAll();
        tabbedPane.addTab("Positions", null, panelLabels);

        if (command.equals("VolRender")) {

            for (i = 0; i < raycastTabVector.size(); i++) {
                String name = ((TabbedItem) (raycastTabVector.elementAt(i))).name;
                JPanel panel = ((TabbedItem) (raycastTabVector.elementAt(i))).panel;

                insertTab(name, panel);
            }

            if (index < tabbedPane.getTabCount()) {
                tabbedPane.setSelectedIndex(index);
            }
        } else if (command.equals("SurRender")) {

            for (i = 0; i < surTabVector.size(); i++) {
                String name = ((TabbedItem) (surTabVector.elementAt(i))).name;
                JPanel panel = ((TabbedItem) (surTabVector.elementAt(i))).panel;

                insertTab(name, panel);
            }

            if (index < tabbedPane.getTabCount()) {
                tabbedPane.setSelectedIndex(index);
            }
        } else if (command.equals("FlythruRender")) {

            for (i = 0; i < flythruTabVector.size(); i++) {
                String name = ((TabbedItem) (flythruTabVector.elementAt(i))).name;
                JPanel panel = ((TabbedItem) (flythruTabVector.elementAt(i))).panel;

                insertTab(name, panel);
            }

            if (index < tabbedPane.getTabCount()) {
                tabbedPane.setSelectedIndex(index);
            }

        } else if (command.equals("ShearRender")) {

            for (i = 0; i < shearwarpTabVector.size(); i++) {
                String name = ((TabbedItem) (shearwarpTabVector.elementAt(i))).name;
                JPanel panel = ((TabbedItem) (shearwarpTabVector.elementAt(i))).panel;

                insertTab(name, panel);
            }

            if (index < tabbedPane.getTabCount()) {
                tabbedPane.setSelectedIndex(index);
            }
        } else if (command.equals("DualPanelRender")) {

            for (i = 0; i < tempTabs.size(); i++) {
                String name = ((TabbedItem) (tempTabs.elementAt(i))).name;
                JPanel panel = ((TabbedItem) (tempTabs.elementAt(i))).panel;

                insertTab(name, panel);
            }

            if (index < tabbedPane.getTabCount()) {
                tabbedPane.setSelectedIndex(index);
            }
        }
    }

    /**
     * Update image extends from the ModelImage. Now, disabled.
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * Update images in surface render, raycast render and shearwarp render.
     *
     * @return  boolean boolean confirming successful update
     */
    public boolean updateImages() {

        if ( m_akPlaneRender != null) {
	        for (int i = 0; i < 3; i++) {
	            if (m_akPlaneRender[i] != null) {
	                m_akPlaneRender[i].updateData();
	            }
	        }
        }

        if (surRender != null) {
            surRender.updateImages();
        }

        if (raycastRender != null) {
            raycastRender.updateImages(true);
        }

        if (shearwarpRender != null) {
            shearwarpRender.updateImages(true);
        }

        return true;
    }

    /**
     * This methods calls corresponding render to update images without LUT changes.
     *
     * @param   forceShow  forces show to reimport image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(boolean forceShow) {
        if ( m_akPlaneRender != null ) {
	        for (int i = 0; i < 3; i++) {
	            if (m_akPlaneRender[i] != null) {
	                m_akPlaneRender[i].updateData();
	            }
	        }
        }

        if (surRender != null) {
            surRender.updateImages(forceShow);
        }

        if (raycastRender != null) {
            raycastRender.updateImages(forceShow);
        }

        if (shearwarpRender != null) {
            shearwarpRender.updateImages(forceShow);
        }

        return true;

    }

    /**
     * This methods calls corresponding render to update images with LUT changes.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   forceShow   forces show to reimport image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean forceShow, int interpMode) {
        if ( m_akPlaneRender != null ) {
		    for (int iPlane = 0; iPlane < 3; iPlane++) {
		        m_akPlaneRender[iPlane].updateLut(LUTa, LUTb);
		    }
        }

        if (surRender != null) {
            surRender.updateImages(LUTa, LUTb, forceShow, interpMode);
        }

        if (raycastRender != null) {
            raycastRender.updateImages(LUTa, LUTb, forceShow, interpMode);
        }

        if (shearwarpRender != null) {
            shearwarpRender.updateImages(LUTa, LUTb, forceShow, interpMode);
        }

        return true;
    }

    /**
     * The navigation mode update the probe position in 3D texture volume. Not used now. Might be used later on.
     */
    public void updateProbePos() {

    	 if (surRender != null) {
             surRender.updateProbePos();
         }
         else if (surView != null) {
             surView.updateProbePos();
         }
    }

    /**
     * Causes the PlaneRender objects to update the texture maps when the underlying ModelImage changes.
     */
    public void updateSliceData() {
        if ( m_akPlaneRender != null ) {
		    for (int iPlane = 0; iPlane < 3; iPlane++) {
		        m_akPlaneRender[iPlane].updateData();
		    }
        }
    }

    /**
     * Hack. Update the the surface render win-level from the plane renderer.
     *
     * @param  flag  true update win-level, false not update.
     */
    public void updateSurRenderWinlevel(boolean flag) {

        if (surRender != null) {
            surRender.setDisplayMode3D(flag);
        }

        if (raycastRender != null) {
            raycastRender.setWindlevelUpdate(flag);
        }

        if (shearwarpRender != null) {
            shearwarpRender.setWindlevelUpdate(flag);
        }

    }


    /**
     * Repaint the volume.
     */
    public void volumeRepaint() {
        surRender.updateVolume(LUTa, LUTb, false);
        updateImages(true);
    }

    /**
     * Closes window and disposes of frame and component.
     *
     * @param  event  Event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        close();
        disposeLocal(true);
        dispose();
    }

    /**
     * Builds menus for the tri-planar view.
     *
     * @return  DOCUMENT ME!
     */
    protected JMenuBar buildMenu() {
        JSeparator separator = new JSeparator();

        menuObj = new ViewMenuBuilder(this);

        JMenuBar menuBar = new JMenuBar();

        menuBar.add(menuObj.makeMenu("File", false,
                                     new JComponent[] {
                                         separator, menuObj.buildMenuItem("Close frame", "CloseFrame", 0, null, false)
                                     }));
        menuBar.add(menuObj.makeMenu("Options", false,
                                     new JComponent[] {
                                         menuObj.buildCheckBoxMenuItem("Show axes", "ShowAxes", true),
                                         menuObj.buildCheckBoxMenuItem("Show crosshairs", "ShowXHairs", true),
                                     }));
        menuBar.add(menuObj.makeMenu("Toolbars", false,
                                     new JMenuItem[] {
                                         menuObj.buildCheckBoxMenuItem("RFA toolbar", "RFAToolbar", true)
                                     }));

        return menuBar;
    }

    /**
     * Builds the toolbars for the tri-planar view.
     */
    protected void buildToolbars() {
        panelToolbar.setLayout(new GridBagLayout());
        panelToolbar.setVisible(true);
        getContentPane().add(panelToolbar, BorderLayout.NORTH);
    }


    /**
     * Constructs main frame structures for image canvas.
     */
    protected void configureFrame() {
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.setTabLayoutPolicy(JTabbedPane.WRAP_TAB_LAYOUT);
        tabbedPane.addChangeListener(this);
        getContentPane().add(tabbedPane, BorderLayout.WEST);

        screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
        screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;

        if (imageOrientation == FileInfoBase.UNKNOWN_ORIENT) {
            axialOrientation = false;
        } else {
            axialOrientation = true;
            orient = imageA.getAxisOrientation();
        }

        imageA.setImageOrder(ModelImage.IMAGE_A);

        if (imageB != null) {
            imageB.setImageOrder(ModelImage.IMAGE_B);
        }

        menuBar = buildMenu();

        setJMenuBar(menuBar);
        buildToolbars();
        addToolbar();

        if (imageA == null) {
            return;
        }

        setResizable(true);
        addComponentListener(this);

        raisedbevel = BorderFactory.createRaisedBevelBorder();
        loweredbevel = BorderFactory.createLoweredBevelBorder();
        compound = BorderFactory.createCompoundBorder(raisedbevel, loweredbevel);

        Border redline = BorderFactory.createLineBorder(Color.red);

        redBorder = BorderFactory.createCompoundBorder(redline, compound);

        if ( isSurfaceViewEnable ) {
            buildDisplayPanel();
            buildViewPanel();
            buildLightPanel();
            buildClipPanel();
            buildSurfacePanel();
            buildProbePanel();
            buildCameraPanel();
            buildMousePanel();
            buildGeodesic();
        } else {
            buildLabelPanel();
            buildHistoLUTPanel();
            buildOpacityPanel();
        }

        if (isEndoscopyEnable) {
            buildFlythruPanel();
            buildFlythruMovePanel();
        }

        if (isSurfaceRenderEnable) {
            buildDisplayPanel();
            buildViewPanel();
            buildLightPanel();
            buildClipPanel();
            buildSlicePanel();
            buildSurfacePanel();
            buildProbePanel();
            buildCameraPanel();
            buildMousePanel();
            buildGeodesic();
            buildSculpt();
        }

        if (isShearWarpEnable) {
            buildShearWarpOptions();
            buildShearWarpCameraPanel();
        }

        if (isRayCastEnable) {
            buildRayCastOptions();
            buildRayCastCameraPanel();
        }

        if (isBrainsurfaceFlattenerEnable) {
            buildBrainsurfaceFlattener();
        }
        if ( m_akPlaneRender != null ) {
	        JPanel panelAxial = new JPanel(new BorderLayout());
	
	        panelAxial.add(m_akPlaneRender[0].getCanvas(), BorderLayout.CENTER);
	
	        JPanel panelSagittal = new JPanel(new BorderLayout());
	
	        panelSagittal.add(m_akPlaneRender[1].getCanvas(), BorderLayout.CENTER);
	
	        JPanel panelCoronal = new JPanel(new BorderLayout());
	
	        panelCoronal.add(m_akPlaneRender[2].getCanvas(), BorderLayout.CENTER);
	
	        setTitle();
	
	        triImagePanel = new JPanel();
	        triImagePanel.setLayout(new GridLayout(1, 3, 10, 10));
	        triImagePanel.add(panelAxial);
	        triImagePanel.add(panelSagittal);
	        triImagePanel.add(panelCoronal);
	        triImagePanel.setBorder(raisedbevel);
	
	        int triImagePanelWidth = (int) (screenWidth * 0.51f);
	        int triImagePanelHeight = (int) (screenHeight * 0.25f);
	
	        triImagePanel.setPreferredSize(new Dimension(triImagePanelWidth, triImagePanelHeight));
	        triImagePanel.setMinimumSize(new Dimension(150, 50));
	
	        float widthAxial = m_akPlaneRender[0].getWidth();
	        float heightAxial = m_akPlaneRender[0].getHeight();
	        float widthCoronal = m_akPlaneRender[2].getWidth();
	        float heightCoronal = m_akPlaneRender[2].getHeight();
	        float widthSagittal = m_akPlaneRender[1].getWidth();
	        float heightSagittal = m_akPlaneRender[1].getHeight();
	
	        float leftWidth = Math.max(widthAxial, widthCoronal);
	
	        float upperHeight = Math.max(heightAxial, heightSagittal);
	        float rightWidth = widthSagittal;
	        float lowerHeight = heightCoronal;
	        float availableWidth = Toolkit.getDefaultToolkit().getScreenSize().width - 200 - (2 * getInsets().left) - 6;
	        float availableHeight = Toolkit.getDefaultToolkit().getScreenSize().height - 200 - getInsets().top -
	                                getInsets().bottom - panelToolbar.getSize().height - menuBar.getSize().height - 6;
	
	        float zoom = (availableWidth - 1) / (leftWidth + rightWidth - 1);
	
	        zoom = Math.min(zoom, (availableHeight - 1) / (upperHeight + lowerHeight - 1));
	
	        for (int i = -10; i <= 10; i++) {
	
	            if ((zoom >= Math.pow(2.0, (double) i)) && (zoom < Math.pow(2.0, (double) (i + 1)))) {
	                zoom = (float) Math.pow(2.0, (double) i);
	            }
	        }
	
	        if (!axialOrientation) {
	            zoom = 1.0f;
	        }
	
	        if (((zoom * leftWidth) > (triImagePanelWidth / 3.0f)) || ((zoom * upperHeight) > triImagePanelHeight)) {
	            zoom *= 0.5f;
	        }
        }
        GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;

        gbc2.weightx = 1;
        gbc2.weighty = 1;

        gbc2.ipadx = 5;
        gbc2.insets = new Insets(0, 5, 0, 5);

        imagePanel = new JPanel(new BorderLayout());

        setLocation(100, 100);

       
        
        if (leftPanelRenderMode == SURFACE) {
            imagePanel.add(surRender.getCanvas(), BorderLayout.CENTER);
        } else if (leftPanelRenderMode == RAYCAST) {
            imagePanel.add(raycastRender.getCanvas(), BorderLayout.CENTER);
        } else if (leftPanelRenderMode == SHEARWARP) {
            imagePanel.add(shearwarpRender.getCanvas(), BorderLayout.CENTER);
        } else if ( leftPanelRenderMode == SURFACEVIEW ) {
        	imagePanel.add(surView.getCanvas(), BorderLayout.CENTER);
        }


        int imagePanelWidth = (int) (screenWidth * 0.51f);
        int imagePanelHeight = (int) (screenHeight * 0.43f);
        
        if ( isSurfaceViewEnable ) imagePanelHeight = (int) (screenHeight * 0.7f);
        
        imagePanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        imagePanel.setMinimumSize(new Dimension(500, 500));
        imagePanel.setBorder(compound);

        rightPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, imagePanel, triImagePanel);

        rightPane.setOneTouchExpandable(true);
        rightPane.setDividerSize(6);
        rightPane.setContinuousLayout(true);
        rightPane.setResizeWeight(1);

        tabbedPane.setPreferredSize(new Dimension(maxPanelWidth, tabbedPane.getPreferredSize().height));

        JPanel tabPanel = new JPanel(new BorderLayout());

        tabPanel.add(tabbedPane);
        tabPanel.setMinimumSize(new Dimension(maxPanelWidth, 789));

        JSplitPane mainPane;
        if ( isSurfaceViewEnable ) {
           mainPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, tabPanel, imagePanel);
        } else {
           mainPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, tabPanel, rightPane);
        }

        mainPane.setOneTouchExpandable(true);
        mainPane.setDividerSize(6);
        mainPane.setContinuousLayout(true);
        
        getContentPane().add(mainPane, BorderLayout.CENTER);

        // MUST register frame to image models
        imageA.addImageDisplayListener(this);

        if (imageB != null) {
            imageB.addImageDisplayListener(this);
        }

        pack();
        setVisible(true);
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

        // initialize the sculptor region.
        sculptWidth = imagePanelWidth - (2 * getInsets().left);
        sculptHeight = imagePanelHeight - getInsets().top - getInsets().bottom;

        if ( surRender != null ) {
            surRender.getSculptorPanel().setFrameSize(sculptWidth, sculptHeight);
        }
    }

    /**
     * Cleans up memory from gc.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

    /**
     * Add surface volume renderer control buttons.
     */
    private void addToolbar() {
        etchedBorder = BorderFactory.createEtchedBorder();
        toolbarBuilder = new ViewToolBarBuilder(this);

        if ( isSurfaceViewEnable ) {
            buildSurfaceViewToolbar();	
         } else {
            buildViewToolbar();
         }
        
        if (isEndoscopyEnable) {
            buildFlyThruToolbar();
        }

        if (isSurfaceRenderEnable) {
            buildSurRenderToolbar();
        }

        if (isShearWarpEnable) {
            buildShearWarpToolbar();
        }

        if (isRayCastEnable) {
            buildRayCastToolbar();
        }

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;

        if (leftPanelRenderMode == SURFACE) {
            panelToolbar.add(volToolBar, gbc);
        } else if (leftPanelRenderMode == RAYCAST) {
            panelToolbar.add(rayCastToolBar, gbc);
        } else if (leftPanelRenderMode == SHEARWARP) {
            panelToolbar.add(shearWarpToolBar, gbc);
        }

    }

    /**
     * Build the fly through toolbar.
     */
    private void buildFlyThruToolbar() {
        flyThruToolbar = new JToolBar();
        flyThruToolbar.setBorder(etchedBorder);
        flyThruToolbar.setBorderPainted(true);
        flyThruToolbar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        flyThruToolbar.setFloatable(false);
        flyThruToolbar.add(toolbarBuilder.buildButton("OpenFile", "Open Endoscopy File", "open"));
        flyThruToolbar.add(toolbarBuilder.buildButton("OpenSurface", "Open Endoscopy Surface File", "smooth1"));
        flyThruToolbar.add(toolbarBuilder.buildButton("FlythruControl", "Move control panel", "control"));
        flyThruToolbar.add(ViewToolBarBuilder.makeSeparator());
    }

    /**
     * Build the raycast toolbar.
     */
    private void buildRayCastToolbar() {
        rayCastToolBar = new JToolBar();
        rayCastToolBar.setBorder(etchedBorder);
        rayCastToolBar.setBorderPainted(true);
        rayCastToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        rayCastToolBar.add(toolbarBuilder.buildButton("raycastRepaint", "Repaint image", "paint"));
        rayCastToolBar.add(ViewToolBarBuilder.makeSeparator());
        rayCastToolBar.add(toolbarBuilder.buildButton("RayCastOptions", "Option Dialog.", "options"));
        rayCastToolBar.add(ViewToolBarBuilder.makeSeparator());

        ButtonGroup group1 = new ButtonGroup();

        radioMIP = new JRadioButton("MIP", false);
        radioMIP.setFont(serif12);
        group1.add(radioMIP);
        radioXRAY = new JRadioButton("DRR", false);
        radioXRAY.setFont(serif12);
        group1.add(radioXRAY);
        radioCOMPOSITE = new JRadioButton("Composite", false);
        radioCOMPOSITE.setFont(serif12);
        group1.add(radioCOMPOSITE);
        radioSURFACEFAST = new JRadioButton("Surface", false);
        radioSURFACEFAST.setFont(serif12);
        group1.add(radioSURFACEFAST);
        radioSURFACE = new JRadioButton("Composite Surface", false);
        radioSURFACE.setFont(serif12);
        group1.add(radioSURFACE);

        int rayCastMode = raycastRender.getRenderMode();

        if (rayCastMode == ViewJComponentRenderImage.ModeMIP) {
            radioMIP.setSelected(true);
        } else if (rayCastMode == ViewJComponentRenderImage.ModeXRAY) {
            radioXRAY.setSelected(true);
        } else if (rayCastMode == ViewJComponentRenderImage.ModeCOMPOSITE) {
            radioCOMPOSITE.setSelected(true);
        } else if (rayCastMode == ViewJComponentRenderImage.ModeSURFACE) {
            radioSURFACE.setSelected(true);
        } else if (rayCastMode == ViewJComponentRenderImage.ModeSURFACEFAST) {
            radioSURFACEFAST.setSelected(true);
        }

        radioMIP.addItemListener(this);
        radioXRAY.addItemListener(this);
        radioCOMPOSITE.addItemListener(this);
        radioSURFACE.addItemListener(this);
        radioSURFACEFAST.addItemListener(this);
        rayCastToolBar.add(radioMIP);
        rayCastToolBar.add(radioXRAY);
        rayCastToolBar.add(radioCOMPOSITE);
        rayCastToolBar.add(radioSURFACEFAST);
        rayCastToolBar.add(radioSURFACE);
        rayCastToolBar.add(ViewToolBarBuilder.makeSeparator());
        rayCastToolBar.add(toolbarBuilder.buildButton("AutoCapture", "Auto snapshot screen", "camera"));

    }

    /**
     * Build the shearwarp toolbar.
     */
    private void buildShearWarpToolbar() {

        shearWarpToolBar = new JToolBar();
        shearWarpToolBar.setBorder(etchedBorder);
        shearWarpToolBar.setBorderPainted(true);
        shearWarpToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

        shearWarpToolBar.add(toolbarBuilder.buildButton("shearwarpRepaint", "Repaint image", "paintinside"));
        shearWarpToolBar.add(ViewToolBarBuilder.makeSeparator());
        shearWarpToolBar.add(toolbarBuilder.buildButton("shearWarpOptions", "Option Dialog.", "options"));

        ButtonGroup group2 = new ButtonGroup();

        radioMIPShear = new JRadioButton("MIPShear", false);
        radioMIPShear.setFont(serif12);
        group2.add(radioMIPShear);
        radioXRAYShear = new JRadioButton("DRRShear", false);
        radioXRAYShear.setFont(serif12);
        group2.add(radioXRAYShear);
        radioCOMPOSITEShear = new JRadioButton("CompositeShear", false);
        radioCOMPOSITEShear.setFont(serif12);
        group2.add(radioCOMPOSITEShear);
        radioSURFACEShear = new JRadioButton("SurfaceShear", false);
        radioSURFACEShear.setFont(serif12);
        group2.add(radioSURFACEShear);

        int shearwarpMode = shearwarpRender.getRenderMode();

        if (shearwarpMode == ViewJComponentRenderImage.ModeMIP) {
            radioMIPShear.setSelected(true);
        } else if (shearwarpMode == ViewJComponentRenderImage.ModeXRAY) {
            radioXRAYShear.setSelected(true);
        } else if (shearwarpMode == ViewJComponentRenderImage.ModeCOMPOSITE) {
            radioCOMPOSITEShear.setSelected(true);
        } else {
            radioSURFACEShear.setSelected(true);
        }

        radioMIPShear.addItemListener(this);
        radioXRAYShear.addItemListener(this);
        radioCOMPOSITEShear.addItemListener(this);
        radioSURFACEShear.addItemListener(this);
        shearWarpToolBar.add(radioMIPShear);
        shearWarpToolBar.add(radioXRAYShear);
        shearWarpToolBar.add(radioCOMPOSITEShear);
        shearWarpToolBar.add(radioSURFACEShear);
        shearWarpToolBar.add(ViewToolBarBuilder.makeSeparator());
        shearWarpToolBar.add(toolbarBuilder.buildButton("AutoCaptureShear", "Auto snapshot screen", "camera"));
    }

    /**
     * Build the surface render toolbar.
     */
    private void buildSurRenderToolbar() {
        volToolBar = new JToolBar();
        volToolBar.setBorder(etchedBorder);
        volToolBar.setBorderPainted(true);
        volToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        volToolBar.setFloatable(false);
        volToolBar.add(toolbarBuilder.buildButton("Repaint", "Repaints images", "repaint"));
        volToolBar.add(ViewToolBarBuilder.makeSeparator());
        volToolBar.add(toolbarBuilder.buildButton("SurfaceDialog", "Add surface to viewer", "isosurface"));
        volToolBar.add(toolbarBuilder.buildButton("Geodesic", "Draw geodesic curves on the surface", "geodesic"));
        volToolBar.add(ViewToolBarBuilder.makeSeparator());
        volToolBar.add(toolbarBuilder.buildButton("Mouse", "Record mouse changes", "camcorder"));
        volToolBar.add(toolbarBuilder.buildButton("Capture", "Capture screen shot", "camera"));
        volToolBar.add(ViewToolBarBuilder.makeSeparator());
        volToolBar.add(toolbarBuilder.buildButton("Box", "Display options", "perspective"));
        volToolBar.add(toolbarBuilder.buildButton("ViewControls", "View mode", "mousecontrol"));
        volToolBar.add(ViewToolBarBuilder.makeSeparator());

        ButtonGroup group1 = new ButtonGroup();

        radioSurrenderCOMPOSITE = new JRadioButton("Composite", false);
        radioSurrenderCOMPOSITE.setFont(serif12);
        group1.add(radioSurrenderCOMPOSITE);

        radioSurrenderLIGHT = new JRadioButton("Composite Surface", false);
        radioSurrenderLIGHT.setFont(serif12);
        group1.add(radioSurrenderLIGHT);

        radioSurrenderCOMPOSITE.setSelected(true);
        radioSurrenderLIGHT.setSelected(false);

        radioSurrenderCOMPOSITE.addItemListener(this);
        radioSurrenderLIGHT.addItemListener(this);

        volToolBar.add(radioSurrenderCOMPOSITE);
        volToolBar.add(radioSurrenderLIGHT);

        volToolBar.add(ViewToolBarBuilder.makeSeparator());
    }

    /**
     * The the top one volume view toolbar.
     */
    private void buildSurfaceViewToolbar() {
        viewToolBar = new JToolBar();
        viewToolBar.setBorder(etchedBorder);
        viewToolBar.setBorderPainted(true);
        viewToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
     
        viewToolBar.setLayout(new GridBagLayout());
        viewToolBar.setFloatable(false);
        
        viewToolBar.add(toolbarBuilder.buildButton("SurfaceDialog", "Add surface to viewer", "isosurface"));
        viewToolBar.add(toolbarBuilder.buildButton("Geodesic", "Draw geodesic curves on the surface", "geodesic"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
       //  viewToolBar.add(toolbarBuilder.buildButton("Mouse", "Record mouse changes", "camcorder"));
        viewToolBar.add(toolbarBuilder.buildButton("Capture", "Capture screen shot", "camera"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        viewToolBar.add(toolbarBuilder.buildButton("Box", "Display options", "perspective"));
        viewToolBar.add(toolbarBuilder.buildButton("ViewControls", "View mode", "mousecontrol"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        
       
        clipPlaneButton = toolbarBuilder.buildButton("Clipping", "Clipping Plane", "clip");
        clipPlaneButton.setEnabled(true);
        viewToolBar.add(clipPlaneButton);
        clipButton = toolbarBuilder.buildButton("InvokeClipping", "Enable all clipping planes", "clipall");
        clipButton.setEnabled(true);
        viewToolBar.add(clipButton);
        clipDisableButton = toolbarBuilder.buildButton("DisableClipping", "Disable all clipping planes", "disableclip");
        clipDisableButton.setEnabled(true);
        viewToolBar.add(clipDisableButton);
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        
        viewToolBar.add(toolbarBuilder.buildButton("ChangeLight", "Add light bulb to viewer", "lightsmall"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());

        rfaButton = toolbarBuilder.buildButton("RFA", "Add probe to viewer", "rfa");
        viewToolBar.add(rfaButton);

                
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 35;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 1;
        viewToolBar.add(getRendererProgressBar(), gbc);

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        panelToolbar.add(viewToolBar, gbc);

    }
    
    /**
     * The the top one volume view toolbar.
     */
    private void buildViewToolbar() {
        viewToolBar = new JToolBar();
        viewToolBar.setBorder(etchedBorder);
        viewToolBar.setBorderPainted(true);
        viewToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        viewToolBar.setLayout(new GridBagLayout());
        viewToolBar.setFloatable(false);

        if (isSurfaceRenderEnable && (isEndoscopyEnable || isShearWarpEnable || isRayCastEnable)) {
            viewToolBar.add(toolbarBuilder.buildButton("SurRender", "Surface Renderer", "surfacerender"));
        }

        if (isEndoscopyEnable) {
            viewToolBar.add(toolbarBuilder.buildButton("Flythru", "Fly Through Renderer", "flythrurender"));
        }

        if (isShearWarpEnable) {
            viewToolBar.add(toolbarBuilder.buildButton("ShearRender", "Shear warp Renderer", "shearwarprender"));
        }

        if (isRayCastEnable) {
            viewToolBar.add(toolbarBuilder.buildButton("VolRender", "Volume Renderer", "raycastrender"));
        }

        if (rightPanelRenderMode != NONE) {
            viewToolBar.add(toolbarBuilder.buildButton("DualPanelRender", "Dual Panel Renderer", "2frametri"));
        }

        if (isSurfaceRenderEnable && (isEndoscopyEnable || isShearWarpEnable || isRayCastEnable)) {
            viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        }

        viewToolBar.add(toolbarBuilder.buildButton("ResetX", "Reset X Axis", "xalign"));
        viewToolBar.add(toolbarBuilder.buildButton("ResetY", "Reset Y Axis", "yalign"));
        viewToolBar.add(toolbarBuilder.buildButton("ResetZ", "Reset Z Axis", "zalign"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        viewToolBar.add(toolbarBuilder.buildButton("HistoLUT", "Histogram Lookup Table", "histolut"));
        viewToolBar.add(toolbarBuilder.buildButton("OpacityHistogram", "Opacity histogram", "histogram"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        viewToolBar.add(toolbarBuilder.buildButton("Slices", "Slice render", "triplanar"));
        viewToolBar.add(toolbarBuilder.buildButton("Opacity", "Surface volume renderer", "renderer"));
        viewToolBar.add(toolbarBuilder.buildButton("Stereo", "Stereo volume renderer", "stereo"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        viewToolBar.add(toolbarBuilder.buildButton("Sculpt", "Sculpt and Remove Volume Region", "sculpt"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        clipPlaneButton = toolbarBuilder.buildButton("Clipping", "Clipping Plane", "clip");
        clipPlaneButton.setEnabled(false);
        viewToolBar.add(clipPlaneButton);
        clipButton = toolbarBuilder.buildButton("InvokeClipping", "Enable all clipping planes", "clipall");
        clipButton.setEnabled(false);
        viewToolBar.add(clipButton);
        clipDisableButton = toolbarBuilder.buildButton("DisableClipping", "Disable all clipping planes", "disableclip");
        clipDisableButton.setEnabled(false);
        viewToolBar.add(clipDisableButton);
        clipMaskButton = toolbarBuilder.buildButton("CropClipVolume", "Crop the clipping volume", "maskvolume");
        clipMaskButton.setEnabled(false);
        viewToolBar.add(clipMaskButton);
        clipMaskUndoButton = toolbarBuilder.buildButton("UndoCropVolume", "Undo crop", "undomask");
        clipMaskUndoButton.setEnabled(false);
        viewToolBar.add(clipMaskUndoButton);
        clipSaveButton = toolbarBuilder.buildButton("SaveCropVolume", "Save crop image", "savemask");
        clipSaveButton.setEnabled(false);
        viewToolBar.add(clipSaveButton);
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        viewToolBar.add(toolbarBuilder.buildButton("ChangeLight", "Add light bulb to viewer", "lightsmall"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());

        if (isRayCastEnable || isShearWarpEnable) {
            ButtonGroup cursorGroup = new ButtonGroup();
            viewToolBar.add(toolbarBuilder.buildToggleButton("parallelrotation", "Parallel rotation", "rotateparallel",
                                                             cursorGroup));
            viewToolBar.add(toolbarBuilder.buildToggleButton("rotatearbitrary", "Arbitrary rotation",
                                                             "rotateunparallel", cursorGroup));
            viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        }

        rfaButton = toolbarBuilder.buildButton("RFA", "Add probe to viewer", "rfa");
        viewToolBar.add(rfaButton);

        rfaSeparator = ViewToolBarBuilder.makeSeparator();
        viewToolBar.add(rfaSeparator);
        viewToolBar.add(toolbarBuilder.buildButton("Extract", "Extract rotated image", "imageextract"));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 35;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 1;
        viewToolBar.add(getRendererProgressBar(), gbc);

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        panelToolbar.add(viewToolBar, gbc);

    }

    /**
     * Enable dual panel renders.
     */
    private void enableDualPanelRender() {
        switchTabList("DualPanelRender");

        if (isShearWarpEnable) {
            panelToolbar.remove(shearWarpToolBar);
        }

        if (isRayCastEnable) {
            panelToolbar.remove(rayCastToolBar);
        }

        if (isSurfaceRenderEnable) {
            panelToolbar.remove(volToolBar);
        }

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;

        if (leftPanelRenderMode == SURFACE) {
            panelToolbar.add(volToolBar, gbc);
        } else if (leftPanelRenderMode == RAYCAST) {
            panelToolbar.add(rayCastToolBar, gbc);
        } else if (leftPanelRenderMode == SHEARWARP) {
            panelToolbar.add(shearWarpToolBar, gbc);
        }

        imagePanel.removeAll();

        int imagePanelWidth = (int) (screenWidth * 0.510f);
        int imagePanelHeight = (int) (screenHeight * 0.43f);

        dualImagePanel = null;
        dualLeftPanel = null;
        dualRightPanel = null;
        dualImagePanel = new JPanel(new BorderLayout());
        dualLeftPanel = new JPanel(new BorderLayout());
        dualLeftPanel.setMinimumSize(new Dimension(imagePanelWidth / 2, 1));
        dualLeftPanel.setPreferredSize(new Dimension(imagePanelWidth / 2, imagePanelHeight));

        if (leftPanelRenderMode == SURFACE) {
            dualLeftPanel.add(surRender.getCanvas(), BorderLayout.CENTER);
            surRender.getCanvas().addMouseListener(this);
        }

        dualLeftPanel.setBorder(redBorder);
        dualRightPanel = new JPanel(new BorderLayout());
        dualRightPanel.setPreferredSize(new Dimension(imagePanelWidth / 2, imagePanelHeight));
        dualRightPanel.setMinimumSize(new Dimension(imagePanelWidth / 2, 1));
        dualRightPanel.setBorder(loweredbevel);

        if (rightPanelRenderMode == SHEARWARP) {
            dualRightPanel.add(shearwarpRender.getCanvas(), BorderLayout.CENTER);
            shearwarpRender.getCanvas().addMouseListener(this);
        } else if (rightPanelRenderMode == RAYCAST) {
            dualRightPanel.add(raycastRender.getCanvas(), BorderLayout.CENTER);
            raycastRender.getCanvas().addMouseListener(this);
        } else if (rightPanelRenderMode == SURFACE) {
            dualRightPanel.add(surRender.getCanvas(), BorderLayout.CENTER);
            surRender.getCanvas().addMouseListener(this);
        } else if ((rightPanelRenderMode == ENDOSCOPY) && isEndoscopyEnable) {
            dualRightPanel.add(flythruRender.getCanvas(), BorderLayout.CENTER);
            flythruRender.getCanvas().addMouseListener(this);
            dualRightPanel.setBorder(redBorder);
            dualLeftPanel.setBorder(loweredbevel);
            dualRightPanel.validate();
            dualRightPanel.repaint();
        } else if (rightPanelRenderMode == BRAINSURFACE_FLATTENER) {
            dualRightPanel.add(brainsurfaceFlattenerRender.getCanvas(), BorderLayout.CENTER);
            brainsurfaceFlattenerRender.getCanvas().addMouseListener(this);
            insertSurfaceTab("Brain Surface Flattener", m_kBrainsurfaceFlattenerPanel);
            insertTab("Brain Surface Flattener", m_kBrainsurfaceFlattenerPanel);
        }

        dualImagePane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, dualLeftPanel, dualRightPanel);
        dualImagePane.setOneTouchExpandable(false);
        dualImagePane.setDividerSize(6);
        dualImagePane.setContinuousLayout(true);
        dualImagePanel.add(dualImagePane, BorderLayout.CENTER);
        imagePanel.add(dualImagePanel, BorderLayout.CENTER);

        getContentPane().validate();
        dualLeftPanel.setMinimumSize(new Dimension(12, 12));
        dualRightPanel.setMinimumSize(new Dimension(12, 12));

    }

    /**
     * Enable the fly through frame layout.
     */
    private void enableFlyThruRender() {
        switchTabList("FlythruRender");
        insertTab("Flythru", flythruPanel);
        insertFlythruTab("Flythru", flythruPanel);

        if (isSurfaceRenderEnable) {
            panelToolbar.remove(volToolBar);
        }

        if (isEndoscopyEnable) {
            panelToolbar.remove(flyThruToolbar);
        }

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        panelToolbar.add(flyThruToolbar, gbc);
        panelToolbar.validate();
        panelToolbar.repaint();

        if (flythruRender.getCanvas() != null) {
            imagePanel.removeAll();
            imagePanel.add(flythruRender.getCanvas(), BorderLayout.CENTER);
            getContentPane().validate();
        } else { // first time calling flythru render
            imagePanel.removeAll();

            int imagePanelWidth = (int) (screenWidth * 0.510f);
            int imagePanelHeight = (int) (screenHeight * 0.43f);

            dualImagePanel = null;
            dualLeftPanel = null;
            dualRightPanel = null;
            dualImagePanel = new JPanel(new BorderLayout());
            dualLeftPanel = new JPanel(new BorderLayout());
            dualLeftPanel.setMinimumSize(new Dimension(imagePanelWidth / 2, 1));
            dualLeftPanel.setPreferredSize(new Dimension(imagePanelWidth / 2, imagePanelHeight));

            if (leftPanelRenderMode == SURFACE) {
                dualLeftPanel.add(surRender.getCanvas(), BorderLayout.CENTER);
                surRender.getCanvas().addMouseListener(this);
            }

            dualRightPanel = new JPanel(new BorderLayout());
            dualRightPanel.setPreferredSize(new Dimension(imagePanelWidth / 2, imagePanelHeight));
            dualRightPanel.setMinimumSize(new Dimension(imagePanelWidth / 2, 1));
            dualRightPanel.setBorder(loweredbevel);

            if ((rightPanelRenderMode == ENDOSCOPY) && isEndoscopyEnable) {
                dualRightPanel.setBorder(redBorder);
                dualLeftPanel.setBorder(loweredbevel);
                dualRightPanel.validate();
                dualRightPanel.repaint();
            }

            dualImagePane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, dualLeftPanel, dualRightPanel);
            dualImagePane.setOneTouchExpandable(false);
            dualImagePane.setDividerSize(6);
            dualImagePane.setContinuousLayout(true);
            dualImagePanel.add(dualImagePane, BorderLayout.CENTER);
            imagePanel.add(dualImagePanel, BorderLayout.CENTER);

            getContentPane().validate();
            dualLeftPanel.setMinimumSize(new Dimension(12, 12));
            dualRightPanel.setMinimumSize(new Dimension(12, 12));

        }

    }

    /**
     * Enable the shear warp render.
     */
    private void enableShearWarpRender() {
        switchTabList("ShearRender");

        if (isShearWarpEnable) {
            panelToolbar.remove(shearWarpToolBar);
        }

        if (isRayCastEnable) {
            panelToolbar.remove(rayCastToolBar);
        }

        if (isSurfaceRenderEnable) {
            panelToolbar.remove(volToolBar);
        }

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;

        if (isShearWarpEnable) {
            panelToolbar.add(shearWarpToolBar, gbc);
        }

        imagePanel.removeAll();
        imagePanel.add(shearwarpRender.getCanvas(), BorderLayout.CENTER);
        getContentPane().validate();

    }

    /**
     * Enable surface render.
     */
    private void enableSurfaceRender() {

        if (isEndoscopyEnable) {
            panelToolbar.remove(flyThruToolbar);
        }

        switchTabList("SurRender");

        if (isShearWarpEnable) {
            panelToolbar.remove(shearWarpToolBar);
        }

        if (isRayCastEnable) {
            panelToolbar.remove(rayCastToolBar);
        }

        if (isSurfaceRenderEnable) {
            panelToolbar.remove(volToolBar);
        }

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        panelToolbar.add(volToolBar, gbc);
        imagePanel.removeAll();
        imagePanel.add(surRender.getCanvas(), BorderLayout.CENTER);
        getContentPane().validate();

    }

    /**
     * Enable volume render.
     */
    private void enableVolumeRender() {
        switchTabList("VolRender");

        if (isShearWarpEnable) {
            panelToolbar.remove(shearWarpToolBar);
        }

        if (isRayCastEnable) {
            panelToolbar.remove(rayCastToolBar);
        }

        if (isSurfaceRenderEnable) {
            panelToolbar.remove(volToolBar);
        }

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        panelToolbar.add(rayCastToolBar, gbc);
        imagePanel.removeAll();
        imagePanel.add(raycastRender.getCanvas(), BorderLayout.CENTER);
        getContentPane().validate();
    }

    /**
     * Calculate the LUT from the resampled image.
     *
     * @param  image  ModelImage reference
     * @param  lut    ModelLUT reference
     */
    private void resetLUTMinMax(ModelImage image, ModelLUT lut) {
        int nPts = lut.getTransferFunction().size();
        float[] x = new float[nPts];
        float[] y = new float[nPts];
        lut.getTransferFunction().exportArrays(x, y);

        for (int i = 0; i < nPts; i++) {

            if (x[i] < image.getMin()) {
                x[i] = (float) image.getMin();
            } else if (x[i] > image.getMax()) {
                x[i] = (float) image.getMax();
            }
        }

        lut.getTransferFunction().importArrays(x, y, nPts);

    }

    /**
     * Method that resizes the frame and adjusts the rows, columns as needed.
     */
    private void resizePanel() {
        int height;

        height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                 panelToolbar.getHeight();

        if (panelHistogram != null) {
        	panelHistogram.resizePanel(maxPanelWidth, height);
        }

        if (isEndoscopyEnable) {
            flythruControl.resizePanel(maxPanelWidth, height);
        }

        if (isSurfaceRenderEnable) {

            if (surRender.getSurfaceDialog() != null) {
                surRender.getSurfaceDialog().resizePanel(maxPanelWidth, height);
            }

            if (surRender.getViewDialog() != null) {
                surRender.getViewDialog().resizePanel(maxPanelWidth, height);
            }

            if (surRender.getSlicePanel() != null) {
                surRender.getSlicePanel().resizePanel(maxPanelWidth, height);
            }

            if (((SurfaceRender) surRender).getDisplayDialog() != null) {
                ((SurfaceRender) surRender).getDisplayDialog().resizePanel(maxPanelWidth, height);
            }

            if (surRender.getSurfaceDialog().getLightDialog() != null) {
                surRender.getSurfaceDialog().getLightDialog().resizePanel(maxPanelWidth, height);
            }

            if (surRender.getClipDialog() != null) {
                surRender.getClipDialog().resizePanel(maxPanelWidth, height);
            }

            if (surRender.getProbeDialog() != null) {
                surRender.getProbeDialog().resizePanel(maxPanelWidth, height);
            }

            if (surRender.getCameraControl() != null) {
                surRender.getCameraControl().resizePanel(maxPanelWidth, height);
            }

            if (surRender.getVolOpacityPanel() != null) {
                surRender.getVolOpacityPanel().resizePanel(maxPanelWidth, height);
            }

            if (surRender.getGeodesicPanel() != null) {
                surRender.getGeodesicPanel().resizePanel(maxPanelWidth, height);
            }

            if (surRender.getSculptorPanel() != null) {
                surRender.getSculptorPanel().resizePanel(maxPanelWidth, height);
            }
        }

        if (isRayCastEnable) {

            if (raycastRender.getVolOpacity() != null) {
                raycastRender.getVolOpacity().resizePanel(maxPanelWidth, height);
            }

            if (raycastRender.getCameraControl() != null) {
                raycastRender.getCameraControl().resizePanel(maxPanelWidth, height);
            }

            if (raycastRender.getOptionsPanel() != null) {
                raycastRender.getOptionsPanel().resizePanel(maxPanelWidth, height);
            }

            if (raycastRender.getLightControlPanel() != null) {
                raycastRender.getLightControlPanel().resizePanel(maxPanelWidth, height);
            }
        }

        if (isShearWarpEnable) {

            if (shearwarpRender.getVolOpacity() != null) {
                shearwarpRender.getVolOpacity().resizePanel(maxPanelWidth, height);
            }

            if (shearwarpRender.getOptionsPanel() != null) {
                shearwarpRender.getOptionsPanel().resizePanel(maxPanelWidth, height);
            }

            if (shearwarpRender.getLightControlPanel() != null) {
                shearwarpRender.getLightControlPanel().resizePanel(maxPanelWidth, height);
            }

            if (shearwarpRender.getCameraControl() != null) {
                shearwarpRender.getCameraControl().resizePanel(maxPanelWidth, height);
            }
        }

    }

    /**
     * Sets the 3DModel position label.
     *
     * @param  position  DOCUMENT ME!
     */
    private void set3DModelPosition(WildMagic.LibFoundation.Mathematics.Vector3f kPosition) {
        Vector3f screen = new Vector3f();
        Vector3f position = new Vector3f( kPosition.X, kPosition.Y, kPosition.Z );
        surRender.ModelToScreen(position, screen);

        modelViewLabelVals[0].setText("X: " + screen.x);
        modelViewLabelVals[1].setText("Y: " + screen.y);
        modelViewLabelVals[2].setText("Z: " + screen.z);
    }

    /**
     * Sets the PatientSlice position label.
     *
     * @param  position  DOCUMENT ME!
     */
    private void setPatientSlicePosition(WildMagic.LibFoundation.Mathematics.Vector3f position) {
    	WildMagic.LibFoundation.Mathematics.Vector3f axial = new WildMagic.LibFoundation.Mathematics.Vector3f();
        MipavCoordinateSystems.fileToPatient(position, axial, imageA, FileInfoBase.AXIAL);

        WildMagic.LibFoundation.Mathematics.Vector3f coronal = new WildMagic.LibFoundation.Mathematics.Vector3f();
        MipavCoordinateSystems.fileToPatient(position, coronal, imageA, FileInfoBase.CORONAL);

        WildMagic.LibFoundation.Mathematics.Vector3f sagittal = new WildMagic.LibFoundation.Mathematics.Vector3f();
        MipavCoordinateSystems.fileToPatient(position, sagittal, imageA, FileInfoBase.SAGITTAL);

        patientSliceLabelVals[0].setText("sagittal slice: " + (int) sagittal.Z);
        patientSliceLabelVals[1].setText("coronal slice: " + (int) coronal.Z);
        patientSliceLabelVals[2].setText("axial slice: " + (int) axial.Z);
    }

    /**
     * Set the RFA button visible or not.
     *
     * @param  flag  Set the RFA button visible or not
     */
    private void setRFAToolbarVisible(boolean flag) {
        rfaButton.setVisible(flag);
        rfaSeparator.setVisible(flag);
        viewToolBar.validate();
        viewToolBar.repaint();
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Item to hold tab name and corresponding panel.
     */
    class TabbedItem {

        /** DOCUMENT ME! */
        public String name;

        /** DOCUMENT ME! */
        public JPanel panel;

        /**
         * Creates a new TabbedItem object.
         *
         * @param  _name   DOCUMENT ME!
         * @param  _panel  DOCUMENT ME!
         */
        public TabbedItem(String _name, JPanel _panel) {
            name = _name;
            panel = _panel;
        }
    }

    public void setRenderPerspective(boolean bEnable) {}

}
