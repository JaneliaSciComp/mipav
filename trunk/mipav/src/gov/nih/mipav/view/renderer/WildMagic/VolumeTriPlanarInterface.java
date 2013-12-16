package gov.nih.mipav.view.renderer.WildMagic;


import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.JFrameHistogram;
import gov.nih.mipav.view.JPanelVolumeOpacity;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewControlsImage;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewMenuBar;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.Preferences.OperatingSystem;
import gov.nih.mipav.view.renderer.ViewJComponentVolOpacityBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanel3DMouse_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelClip_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelCustomBlend;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelDisplay_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelGeodesic_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelLights_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelMultiDimensionalTransfer;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelPositions;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelRenderMode_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSculptor_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSlices_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurfaceTexture_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurface_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelVolume4D;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceExtractorCubes;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceState;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeNode;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSlices;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterfaceListener;
import gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.CorticalAnalysisRender;
import gov.nih.mipav.view.renderer.WildMagic.flythroughview.FlyThroughRender;
import gov.nih.mipav.view.renderer.WildMagic.flythroughview.JPanelVirtualEndoscopySetup_WM;
import gov.nih.mipav.view.renderer.flythroughview.JPanelFlythruMove;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.BitSet;
import java.util.Vector;

import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLContext;
import javax.media.opengl.GLDrawableFactory;
import javax.media.opengl.GLOffscreenAutoDrawable;
import javax.media.opengl.GLProfile;
import javax.media.opengl.awt.GLCanvas;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.WindowConstants;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.TriMesh;


public class VolumeTriPlanarInterface extends JFrame implements ViewImageUpdateInterface, ActionListener, WindowListener, 
																ComponentListener, ChangeListener, VOIManagerInterfaceListener, 
																PropertyChangeListener
{
    public class IntVector extends Vector<Integer> {
        /**  */
        private static final long serialVersionUID = -7551972247476811252L;

        public IntVector() {
            super();
        }

        public IntVector(final int initialsize) {
            super(initialsize);
        }
    }

    /**
     * Item to hold tab name and corresponding panel.
     */
    class TabbedItem {

        /** Panel name */
        public String name;

        /** Panel */
        public JPanel panel;

        /**
         * Creates a new TabbedItem object.
         * 
         * @param _name panel name.
         * @param _panel JPanel to display in tab.
         */
        public TabbedItem(final String _name, final JPanel _panel) {
            name = _name;
            panel = _panel;
        }
    }

    /** Use serialVersionUID for interoperability. */
    protected static final long serialVersionUID = 1898957906984534260L;

    /** The small bar on the top right corner the volume view frame. */
    protected static JProgressBar rendererProgressBar;

    protected static GLProfile glp;

    protected static GLCapabilities caps;

    protected static int gl_width, gl_height;
    /**
     * Retrieve the progress bar used in the volume renderer (the one in the upper right hand corner).
     * 
     * @return the volume renderer progress bar
     */
    public static final JProgressBar getRendererProgressBar() {

        if (VolumeTriPlanarInterface.rendererProgressBar == null) {
            VolumeTriPlanarInterface.rendererProgressBar = new JProgressBar();
        }

        return VolumeTriPlanarInterface.rendererProgressBar;
    }
    
    private JSplitPane mainPane;

    /** Menu items storage. */
    protected ViewMenuBuilder menuObj;

    /** Panel that holds the toolbars. */
    protected JPanel panelToolbar = new JPanel();

    /** Constraints on panelToolbar layout: */
    protected GridBagConstraints panelToolBarGBC = new GridBagConstraints();

    /** Rendering the brainsurfaceFlattener objects. */
    protected CorticalAnalysisRender brainsurfaceFlattenerRender = null;

    /** Flythrough renderer: */
    protected FlyThroughRender m_kFlyThroughRender = null;

    /** Flythough setup panel: */
    protected JPanelVirtualEndoscopySetup_WM flythruControl;

    /** Flythough Move panel: */
    protected JPanelFlythruMove flythruMoveControl;

    /** Clipping user-interface panel: */
    protected JPanelClip_WM clipGUI;

    protected JPanelPositions positionsPanel;

    /** 3D Slice-view user-interface panel: */
    protected JPanelSlices_WM sliceGUI;

    /** 3D Surface user-interface panel: */
    protected JPanelSurface_WM surfaceGUI;

    /** Display options panel: */
    protected JPanelDisplay_WM displayGUI;

    /** Geodesic user-interface panel: */
    protected JPanelGeodesic_WM geodesicGUI;

    /** Sculpting user-interface panel: */
    protected JPanelSculptor_WM sculptGUI;

    /** Surface Texture user-interface panel: */
    protected JPanelSurfaceTexture_WM surfaceTextureGUI;

    /** Renderer mode user-interface panel */
    protected JPanelRenderMode_WM rendererGUI;

    /** Multihistogram panel: */
    protected JPanelMultiDimensionalTransfer multiHistogramGUI;

    /** Multihistogram panel: */
    protected JPanelCustomBlend customBlendGUI;

    /** Button to invoke all the six clipping planes. */
    protected JButton clipButton;

    /** Button to disable all the six clipping planes. */
    protected JButton clipDisableButton;

    /** Button to crop the clip volume. */
    protected JButton clipMaskButton;

    /** Button to undo crop the clip volume. */
    protected JButton clipMaskUndoButton;

    /** Button to invoke clipping planes. */
    protected JButton clipPlaneButton;

    /** Button to save clipped region. */
    protected JButton clipSaveButton;

    /** The image panel to hold one Canvas3D. */
    protected JPanel gpuPanel;

    /** Panel to hold the BrainSurfaceFlattener or Flythrough views. */
    protected JPanel bf_flyPanel;

    /** Light panel */
    protected JPanelLights_WM m_kLightsPanel;

    /** The three slice views displayed as texture-mapped polygons:. */
    protected PlaneRender_WM[] m_akPlaneRender;

    /** The max width of the control panels. */
    protected int maxPanelWidth = -1;

    /** Menu bar. */
    protected JMenuBar menuBar;
    protected JMenu voiMenu;
    
    protected JFrameHistogram frameHistogram;

    /** VolumeImage contains data and textures for ModelImage A. */
    protected VolumeImage m_kVolumeImageA;

    /** VolumeImage contains data and textures for ModelImage B. */
    protected VolumeImage m_kVolumeImageB;

    /** Volume/Slice/Surface renderer. */
    protected VolumeTriPlanarRender raycastRenderWM;

    /** The view pane that contains the image view and tri-planar view panels. */
    protected JSplitPane rightPane;

    /** For displaying the BrainSurfaceFlattener or Flythrough renderers. */
    protected JSplitPane dualPane;

    /** Toolbar builder reference. */
    protected ViewToolBarBuilder toolbarBuilder;

    /** Tri image planar render panels. */
    protected JPanel triImagePanel;

    /** The top one render view switch toolbar. */
    protected JToolBar viewToolBar;

    /** Opacity panel. */
    protected JPanelVolumeOpacity m_kVolOpacityPanel;
    
    /** Axial view panel. */
    protected JPanel panelAxial;
    /** Sagittal view panel. */
    protected JPanel panelSagittal;

    /** Coronal view panel. */
    protected JPanel panelCoronal;
    
    /** 3D mouse user-interface panel: */
    protected JPanel3DMouse_WM mouseGUI;

    /** Current frame width and height. */
    protected int screenWidth, screenHeight;

    protected VOIManagerInterface m_kVOIInterface = null;

    protected int m_iVOICount = 0;


    private JPanelVolume4D m_kVolume4DGUI;

    private boolean m_b4D = false;

    private JToggleButton m_kRecordToggle;

    private JButton m_kSaveButton;

    
    private JButton m_kLoadButton;

    /** The main tabbed pane in the volume view frame. */
    private JTabbedPane tabbedPane;
    
    /** Reference to the user interface. */
    protected ViewUserInterface userInterface;

    protected boolean m_bDependentInterfaceInit = false;

    protected GLOffscreenAutoDrawable sharedDrawable = null;

    protected VolumeTriPlanarRender sharedRenderer;

    protected static boolean init = initClass();

    public static boolean initClass() {
        GLProfile.initSingleton();
        glp = GLProfile.getMaxProgrammable(true);
        caps = new GLCapabilities(glp);
        caps.setHardwareAccelerated(true);
        gl_width  = 512;
        gl_height = 512;
        return true;
    }

    /**
     * Specific constructor call from the VolumeViewerDTI.
     */
    public VolumeTriPlanarInterface() {
        userInterface = ViewUserInterface.getReference();
        getContentPane().setLayout(new BorderLayout());
        addWindowListener(this);

        try {
            setIconImage(MipavUtil.getIconImage("wm.gif"));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }
        this.configureFrame();
        MipavInitGPU.InitGPU();
    }

    public VolumeTriPlanarInterface(final ModelImage _imageA, final ModelImage _imageB) {
        userInterface = ViewUserInterface.getReference();
        getContentPane().setLayout(new BorderLayout());
        addWindowListener(this);

        try {
            setIconImage(MipavUtil.getIconImage("wm.gif"));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }

        MipavInitGPU.InitGPU();
        /** Progress bar show up during the volume view frame loading */
        final ViewJProgressBar progressBar = new ViewJProgressBar("Creating Volume & Surface Renderer...",
                "Creating Volume & Surface Renderer...", 0, 100, false, null, null);
        MipavUtil.centerOnScreen(progressBar);
        progressBar.setVisible(true);
        progressBar.updateValueImmed(0);

        final int iProgress = (_imageB == null) ? 10 : 5;
        m_kVolumeImageA = new VolumeImage( true, _imageA, "A", progressBar, iProgress);
        progressBar.updateValueImmed(progressBar.getValue() + iProgress);
        if (_imageB != null) {
            m_kVolumeImageB = new VolumeImage( true, _imageB, "B", progressBar,
                    iProgress);
            progressBar.updateValueImmed(progressBar.getValue() + iProgress);
        } else {
            m_kVolumeImageB = new VolumeImage();
        }
        m_kVolumeImageA.GetImage().setImageOrder(ModelImage.IMAGE_A);

        if (m_kVolumeImageB.GetImage() != null) {
            m_kVolumeImageB.GetImage().setImageOrder(ModelImage.IMAGE_B);
        }
        progressBar.setMessage("Configuring frame...");
        this.configureFrame();
        constructRenderers(progressBar);

        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        gpuPanel.setVisible(true);

        progressBar.updateValueImmed(100);
        progressBar.dispose();
        

        raycastRenderWM.setVisible(true);
        raycastRenderWM.startAnimator(true);
    	
    	
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();
        
        if ( (voiMenu != null) && ViewMenuBar.isMenuCommand(voiMenu, command) && (m_kVOIInterface != null)) {
        	m_kVOIInterface.actionPerformed(event);
        } else if (command.equals("Extract")) {
            raycastRenderWM.updateImageFromRotation();
        } else if (command.equals("ExtractMeshFromVolume")) {
            raycastRenderWM.extractMeshFromVolume();
        } else if (command.equals("HistoLUT")) {
        	insertTab("LUT", frameHistogram.getContainingPanel());
        } else if (command.equals("VolRender")) {}
        else if (command.equals("Geodesic")) {
            insertTab("Geodesic", geodesicGUI.getMainPanel());
        } else if (command.equals("Sculpt")) {
            insertTab("Sculpt", sculptGUI.getMainPanel());
        } else if (command.equals("Clipping")) {
            insertTab("Clip", clipGUI.getMainPanel());
            clipMaskButton.setEnabled(true);
        } else if (command.equals("OpacityHistogram")) {
            insertTab("Opacity", m_kVolOpacityPanel.getMainPanel());
        } else if (command.equals("Opacity")) {
            insertTab("Opacity", m_kVolOpacityPanel.getMainPanel());
            raycastRenderWM.displayVolumeRaycast(true);
            rendererGUI.setDisplayVolumeCheck(true);
            rendererGUI.setDisplaySlicesCheck(false);
            raycastRenderWM.displayVolumeSlices(rendererGUI.getSlicesCheck().isSelected());
            raycastRenderWM.setVolumeBlend(rendererGUI.getBlendSliderValue() / 100.0f);
	        updateABBlend( );
        } else if (command.equals("VolumeRayCast")) {
            raycastRenderWM.displayVolumeRaycast(rendererGUI.getVolumeCheck().isSelected());
            raycastRenderWM.setVolumeBlend(rendererGUI.getBlendSliderValue() / 100.0f);
        } else if (command.equals("StereoOFF")) {
            raycastRenderWM.setStereo(0);
        } else if (command.equals("StereoRED")) {
            raycastRenderWM.setStereo(1);
        } else if (command.equals("StereoSHUTTER")) {
            raycastRenderWM.setStereo(2);
        } else if (command.equals("ChangeLight")) {
            insertTab("Light", m_kLightsPanel.getMainPanel());
        } else if (command.equals("Box")) {
            insertTab("Display", displayGUI.getMainPanel());
        } else if (command.equals("InvokeClipping")) {
            clipGUI.invokeClippingPlanes();
            clipMaskButton.setEnabled(true);
            insertTab("Clip", clipGUI.getMainPanel());
        } else if (command.equals("DisableClipping")) {
            clipMaskButton.setEnabled(false);
            clipGUI.disable6Planes();
        } else if (command.equals("CropClipVolume")) {
            raycastRenderWM.cropClipVolume();
            clipMaskUndoButton.setEnabled(true);
            clipSaveButton.setEnabled(true);
            setModified();
        } else if (command.equals("UndoCropVolume")) {
            updateData(false);
            clipMaskUndoButton.setEnabled(false);
            clipSaveButton.setEnabled(false);
        } else if (command.equals("SaveCropVolume")) {
            raycastRenderWM.saveCroppedImage();
        } else if (command.equals("Slices")) {
            insertTab("Slices", sliceGUI.getMainPanel());
            raycastRenderWM.displayVolumeRaycast(false);
            rendererGUI.setDisplayVolumeCheck(false);
            rendererGUI.setDisplaySlicesCheck(true);
            raycastRenderWM.displayVolumeSlices(rendererGUI.getSlicesCheck().isSelected());
        } else if (command.equals("VolumeSlices")) {
            raycastRenderWM.displayVolumeSlices(rendererGUI.getSlicesCheck().isSelected());
        } else if (command.equals("Surface")) {
            raycastRenderWM.displaySurface(rendererGUI.getSurfaceCheck().isSelected());

            if (m_akPlaneRender != null) {
                for (int i = 0; i < 3; i++) {

                    if (m_akPlaneRender[i] != null) {
                        m_akPlaneRender[i].displaySurface(rendererGUI.getSurfaceCheck().isSelected());
                    }
                }
            }

        } else if (command.equals("SurfaceDialog")) {
            insertTab("Surface", surfaceGUI.getMainPanel());
        } else if (command.equals("SurfaceTexture")) {
            insertTab("SurfaceTexture", surfaceTextureGUI.getMainPanel());
            surfaceTextureGUI.setSurfacePanel(surfaceGUI);
            if (surfaceGUI.surfaceAdded()) {
                surfaceTextureGUI.setEnabled(true);
            }
        } else if (command.equals("DTI")) {
        	MipavUtil.displayError("Update DTI Interface");
        } else if (command.equals("BrainSurface")) {
            if (brainsurfaceFlattenerRender == null) {
                brainsurfaceFlattenerRender = new gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.CorticalAnalysisRender(
                		new GLCanvas(caps, sharedDrawable.getContext()), this, m_kVolumeImageA, m_kVolumeImageB);
                final TriMesh kSurface = raycastRenderWM.getSurface(surfaceGUI.getSelectedSurface());
                final Node kMeshLines = brainsurfaceFlattenerRender.getPanel().displayCorticalAnalysis(kSurface,
                        raycastRenderWM.getSurfaceCenter(surfaceGUI.getSelectedSurface()));
                if (kMeshLines != null) {
                    maxPanelWidth = Math.max(brainsurfaceFlattenerRender.getMainPanel().getPreferredSize().width,
                            maxPanelWidth);
                    bf_flyPanel.add(brainsurfaceFlattenerRender.GetCanvas(), BorderLayout.CENTER);
                    dualPane.setDividerLocation(0.5f);
                    m_kLightsPanel.enableLight(0, true);
                    addNode(kMeshLines);
                    insertTab("BrainSurface", brainsurfaceFlattenerRender.getMainPanel());
                } else {
                    MipavUtil.displayError(surfaceGUI.getSelectedSurface()
                            + " is not a closed mesh. Unable to open brain flattener view.");
                    brainsurfaceFlattenerRender = null;
                }
            }
            resizePanel();
        } else if (command.equals("FlyThru")) {
            if (m_kFlyThroughRender == null) {
                m_kFlyThroughRender = FlyThroughRender.main(new GLCanvas(caps, sharedDrawable.getContext()), this,
						m_kVolumeImageA, m_kVolumeImageB, raycastRenderWM.getTranslate(), false);
                
//                new FlyThroughRender(this, m_kVolumeImageA, m_kVolumeImageB, raycastRenderWM.getTranslate());
                final TriMesh kSurface = raycastRenderWM.getSurface(surfaceGUI.getSelectedSurface());
                m_kFlyThroughRender.addSurface(kSurface, raycastRenderWM.getSurfaceCenter(surfaceGUI.getSelectedSurface()));
                bf_flyPanel.add(m_kFlyThroughRender.GetCanvas(), BorderLayout.CENTER);
                dualPane.setDividerLocation(0.5f);
                m_kLightsPanel.enableLight(0, true);
                buildFlythroughPanel();
                m_kFlyThroughRender.startAnimator(false);
            }
            insertTab("FlyThroughMove", flythruMoveControl.getMainPanel());
            insertTab("FlyThrough", flythruControl.getMainPanel());
            resizePanel();
        } else if (command.equals("Renderer")) {
            insertTab("Renderer", rendererGUI.getMainPanel());
            resizePanel();
        } else if ( command.equals("Home")) { 
            rollbackToImageCenter();
        } else if (command.equals("ResetX")) {
            resetAxisY();
        } else if (command.equals("ResetY")) {
            resetAxisX();
        } else if (command.equals("ResetZ")) {
            resetImage();
        } else if (command.equals("CloseFrame")) {
            windowClosing(null);
        } else if (command.equals("ShowAxes")) {
            final boolean showAxes = menuObj.isMenuItemSelected("Show axes");
            for (int iPlane = 0; iPlane < 3; iPlane++) {
                m_akPlaneRender[iPlane].showAxes(showAxes);
                m_akPlaneRender[iPlane].updateDisplay();
            }
        } else if (command.equals("ShowXHairs")) {
            final boolean showXHairs = menuObj.isMenuItemSelected("Show crosshairs");
            for (int iPlane = 0; iPlane < 3; iPlane++) {
                m_akPlaneRender[iPlane].showXHairs(showXHairs);
                m_akPlaneRender[iPlane].updateDisplay();
            }
        } else if (command.equals("RadiologicalView")) {
            setRadiological(true);
        } else if (command.equals("NeurologicalView")) {
            setRadiological(false);
        } else if (command.equals("VOIToolbar")) {
            final boolean showVOI = menuObj.isMenuItemSelected("VOI toolbar");
            if ( m_kVOIInterface == null )
            {
                initVOI();
            }
            m_kVOIInterface.getToolBar().setVisible(showVOI);
        } else if (command.equals("4DToolbar") && m_b4D) {
            insertTab("4D", m_kVolume4DGUI.getMainPanel());
            resizePanel();
        } else if (command.equals("Record")) {
            raycastRenderWM.record(m_kRecordToggle.isSelected());
        } else if (command.equals("SaveState")) {
            SaveState();
        } else if (command.equals("LoadState")) {
            LoadState();
        } else if (command.equals("Mouse3D")) {
        	if(mouseGUI.isInMenu()){
        		tabbedPane.remove(mouseGUI.getMainPanel());
        		mouseGUI.setInMenu(false);
        	}else{
        		insertTab("3D Mouse", mouseGUI.getMainPanel());
        		mouseGUI.setInMenu(true);
        	}
        }

    }

    /**
     * Add a geodesic element to the surface display.
     * 
     * @param kSurface the surface the geodesic element is attached to.
     * @param kNew the new geodesic element.
     * @param iGroup the Node index.
     */
    public void addGeodesic(final TriMesh kSurface, final Geometry kNew, final int iGroup) {
        raycastRenderWM.addGeodesic(kSurface, kNew, iGroup);
    }

    /**
     * Add a new display node to the volume/surface display list. This is done from the other renderers:
     * BrainSurfaceFlattener and Flythrough.
     * 
     * @param kNode
     */
    public VolumeObject addNode(final Node kNode) {
        return raycastRenderWM.AddNode(kNode);
    }

    /**
     * Add a polyline to the VolumeDTI display.
     * 
     * @param akPolyline new polyline.
     * @param groupIndex Node index.
     */
    public void addPolyline(VOIContour kContour, final Polyline akPolyline, final int groupIndex) {
        raycastRenderWM.addTract(kContour, akPolyline, groupIndex);
    }

    /**
     * Pass the VolumeSlices from the Volume Renderer to the PlaneRender objects.
     * 
     * @param kSlices
     */
    public void addSlices(final VolumeSlices kSlices) {
    	if ( m_akPlaneRender != null )
    	{
			if ( (m_akPlaneRender[0] == null) )
			{
				m_akPlaneRender[0] = PlaneRender_WM.main(new GLCanvas(caps, sharedDrawable.getContext()), this,
						m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.AXIAL, kSlices, false);
		        m_akPlaneRender[1] = PlaneRender_WM.main(new GLCanvas(caps, sharedDrawable.getContext()), this,
		        		m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.SAGITTAL, kSlices, false);
		        m_akPlaneRender[2] = PlaneRender_WM.main(new GLCanvas(caps, sharedDrawable.getContext()), this,
		        		m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.CORONAL, kSlices, false);
		        
		        panelAxial.add(m_akPlaneRender[0].GetCanvas(), BorderLayout.CENTER);
		        panelSagittal.add(m_akPlaneRender[1].GetCanvas(), BorderLayout.CENTER);
		        panelCoronal.add(m_akPlaneRender[2].GetCanvas(), BorderLayout.CENTER);
		        setModified();
		        mainPane.revalidate();
			}
    	}
    }

    /**
     * Add TriMesh surfaces to the Volume Renderer.
     * 
     * @param akSurfaces new surfaces.
     */
    public void addSurface(final SurfaceState kSurface) {
        raycastRenderWM.addSurface(kSurface);
        insertTab("Light", m_kLightsPanel.getMainPanel());
        m_kLightsPanel.enableLight(0, true);
        insertTab("Surface", surfaceGUI.getMainPanel());
        rendererGUI.setDisplaySurfaceCheck(true);
        raycastRenderWM.displaySurface(true);

        if (m_akPlaneRender != null) {
            for (int i = 0; i < 3; i++) {

                if (m_akPlaneRender[i] != null) {
                    m_akPlaneRender[i].displaySurface(true);
                }
            }
        }

        menuObj.setMenuItemEnabled("Open BrainSurface Flattener view", true);
        menuObj.setMenuItemEnabled("Open Fly Through view", true);

        if (geodesicGUI != null) {
            geodesicGUI.setEnabled(true);
            geodesicGUI.setSurfacePanel(surfaceGUI);
        }
    }

    /**
     * Build the clipping control panel for the surface render.
     */
    public void buildClipPanel() {
        clipGUI = new JPanelClip_WM(this);
        maxPanelWidth = Math.max(clipGUI.getPreferredSize().width, maxPanelWidth);
    }

    public void buildCustomBlendPanel() {
        customBlendGUI = new JPanelCustomBlend(this);
        maxPanelWidth = Math.max(customBlendGUI.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the display control panel for the surface render.
     */
    public void buildDisplayPanel() {
        displayGUI = new JPanelDisplay_WM(this);
        maxPanelWidth = Math.max(displayGUI.getMainPanel().getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the flythru move control panel.
     */
    public void buildFlythroughPanel() {
        flythruMoveControl = new JPanelFlythruMove(m_kFlyThroughRender);
        maxPanelWidth = Math.max(flythruMoveControl.getMainPanel().getPreferredSize().width, maxPanelWidth);

        flythruControl = new JPanelVirtualEndoscopySetup_WM(m_kFlyThroughRender);
        flythruControl.getMainPanel().setVisible(true);
        m_kFlyThroughRender.setupRenderControl(flythruControl);
        maxPanelWidth = Math.max(flythruControl.getMainPanel().getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the Geodesic control panel.
     */
    public void buildGeodesic() {
        geodesicGUI = new JPanelGeodesic_WM(this);
        maxPanelWidth = Math.max(geodesicGUI.getMainPanel().getPreferredSize().width, maxPanelWidth);
    }

    /**
     * The histogram control panel of the lookup table.
     */
    public void buildHistoLUTPanel() {

    	maxPanelWidth = Math.max(frameHistogram.getContainingPanel().getPreferredSize().width, maxPanelWidth);
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#buildLabelPanel()
     */
    public void buildLabelPanel() {
        positionsPanel = new JPanelPositions(this);
        tabbedPane.addTab("Positions", null, positionsPanel.getMainPanel());
    }

    /**
     * Build the light control panel for the surface render.
     */
    public void buildLightPanel() {
        m_kLightsPanel = new JPanelLights_WM(this);
        maxPanelWidth = Math.max(m_kLightsPanel.getMainPanel().getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the volume opacity control panel for the surface render.
     */
    public void buildOpacityPanel() {
        //m_kVolOpacityPanel = new JPanelVolumeOpacity(m_kVolumeImageA.GetImage(), m_kVolumeImageB.GetImage());

        maxPanelWidth = Math.max(m_kVolOpacityPanel.getMainPanel().getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Builds the render mode control panel.
     */
    public void buildRenderModePanel() {
        rendererGUI = new JPanelRenderMode_WM(this);
        maxPanelWidth = Math.max(rendererGUI.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the Sculpturing control panel.
     */
    public void buildSculpt() {
        sculptGUI = new JPanelSculptor_WM(this, m_kVolumeImageA.GetImage().is4DImage());
        maxPanelWidth = Math.max(sculptGUI.getMainPanel().getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the slices control panel for the surface render.
     */
    public void buildSlicePanel() {
        sliceGUI = new JPanelSlices_WM(this);
        maxPanelWidth = Math.max(sliceGUI.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the adding surface control panel for the surface render.
     */
    public void buildSurfacePanel() {
        surfaceGUI = new JPanelSurface_WM(this);
        maxPanelWidth = Math.max(surfaceGUI.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Builds the Surface texture panel.
     */
    public void buildSurfaceTexturePanel() {
        surfaceTextureGUI = new JPanelSurfaceTexture_WM(this);
        maxPanelWidth = Math.max(surfaceTextureGUI.getPreferredSize().width, maxPanelWidth);
    }
    
    /**
     * Build the clipping control panel for the surface render.
     */
    public void build3DMousePanel() {
        mouseGUI = new JPanel3DMouse_WM(this);
        maxPanelWidth = Math.max(mouseGUI.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Closes both image A and image B (if it exists). It ensures the images are un-registered from the main-frame then
     * removes any display listeners.
     */
    public void close() {
        setVisible(false);

        //userInterface.unregisterFrame(this);

        if (m_kVolumeImageA != null && m_kVolumeImageA.GetImage() != null) {
            m_kVolumeImageA.GetImage().removeImageDisplayListener(this);
        }

        if (m_kVolumeImageB != null && m_kVolumeImageB.GetImage() != null) {
            m_kVolumeImageB.GetImage().removeImageDisplayListener(this);
        }

        //dispose();
    }

    public void componentHidden(final ComponentEvent arg0) {}

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#componentMoved(java.awt.event.ComponentEvent)
     */
    public void componentMoved(final ComponentEvent event) {
        setModified();
    }

    /**
     * Method called when a component resize event is generated. This method snaps the size of the frame and pagePanel
     * to the nearest row, column sizing (so the gridRow and gridColumn and page layout may change).
     * 
     * @param event frame resize event
     */
    public synchronized void componentResized(final ComponentEvent event) {
        resizePanel();
    }

    public void componentShown(final ComponentEvent arg0) {}

    public void create3DVOI( boolean bIntersection )
    {
        ModelImage kImage = new ModelImage( ModelStorageBase.INTEGER, 
                m_kVolumeImageA.GetImage().getExtents(), "Temp" );
        kImage.copyFileTypeInfo(m_kVolumeImageA.GetImage());

        m_kVOIInterface.make3DVOI(bIntersection, kImage);
        kImage.calcMinMax();
        //new ViewJFrameImage(kImage, null, new Dimension(610, 200), false);

        int[] aiExtents = kImage.getExtents();
        int length = aiExtents[0] * aiExtents[1] * aiExtents[2];
        int[] buffer = new int[length];

        for (int i = 0; i < length; i++) {
            buffer[i] = kImage.getInt(i);
            if ( bIntersection && (buffer[i] < 250) )
            {
                buffer[i] = 0;
            }
        }

        float[] afResolutions = kImage.getResolutions(0);
        int[] direction = MipavCoordinateSystems.getModelDirections(m_kVolumeImageA.GetImage());
        float[] startLocation = m_kVolumeImageA.GetImage().getFileInfo(0).getOrigin();
        SurfaceExtractorCubes kExtractor = 
            new SurfaceExtractorCubes(aiExtents[0], 
                    aiExtents[1], 
                    aiExtents[2], buffer,
                    afResolutions[0], 
                    afResolutions[1], 
                    afResolutions[2], direction,
                    startLocation, null);
        TriMesh kMesh = kExtractor.getLevelSurface( 245, false );
        if ( kMesh != null )
        {
        	TriMesh[] kMeshes = new TriMesh[1];
        	kMeshes[0] = kMesh;
        	getVolumeGPU().displayVolumeRaycast(false);
        	kMeshes[0].SetName(new String("VOI_" + m_iVOICount++));
        	getSurfacePanel().addSurfaces(kMeshes);
        	getRendererGUI().setDisplaySurfaceCheck(true);
        	getRendererGUI().setDisplayVolumeCheck(false);
        }
        kExtractor = null;
        kImage.disposeLocal();
        kImage = null;
    }

    public void CustomBlendMode() {
        insertTab("CustomBlend", customBlendGUI.getMainPanel());
        customBlendGUI.getMainPanel().setVisible(true);
        customBlendGUI.actionPerformed(new ActionEvent(this, 0, ""));
    }

    /**
     * Causes redisplay of all components.
     */
    public void displayAll() {
        for (int i = 0; i < 3; i++) {
            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].GetCanvas().display();
            }
        }
        raycastRenderWM.GetCanvas().display();
        if (m_kFlyThroughRender != null) {
            m_kFlyThroughRender.GetCanvas().display();
        }
    }

    /**
     * Dispose memory.
     * 
     * @param flag call super dispose or not
     */
    public void disposeLocal(final boolean flag) {
        disposeImageIndependentComponents();
        disposeRenderers();
        disposeImageDependentComponents();

        // hack using the flag parameter to prevent a second resetting of the progress bar when
        // the finalizer comes around (window closing does the first one with flag = true)
        if (flag && (VolumeTriPlanarInterface.rendererProgressBar != null)) {
            viewToolBar.remove(VolumeTriPlanarInterface.getRendererProgressBar());
            VolumeTriPlanarInterface.rendererProgressBar = null;
        }
    }

    public void enableBoth( boolean bEnable ) {}

    /**
     * Enable geodesic calculations and display.
     * 
     * @param bEnable when true geodesic curves are enabled.
     */
    public void enableGeodesic(final boolean bEnable) {
        raycastRenderWM.enableGeodesic(bEnable);
    }

    /**
     * Enable painting on TriMesh surfaces.
     * 
     * @param kPaintColor paint color.
     * @param iBrushSize brush size.
     * @param bEnabled painting on/off.
     * @param bPaint when true apply paint.
     * @param bDropper when true do dropper mode.
     * @param bPaintCan when true do paint can mode.
     * @param bErase when true erase.
     */
    public void enablePaint(final ColorRGBA kPaintColor, final int iBrushSize, final boolean bEnabled,
            final boolean bPaint, final boolean bDropper, final boolean bPaintCan, final boolean bErase) {
        raycastRenderWM.enablePaint(kPaintColor, iBrushSize, bEnabled, bPaint, bDropper, bPaintCan, bErase);
    }

    /**
     * Erase all surface paint.
     */
    public void eraseAllPaint() {
        raycastRenderWM.eraseAllPaint();
    }

    public ModelImage getActiveImage()
    {
        return m_kVolumeImageA.GetImage();
    }

    /**
     * Returns the ModelLUT or ModelRGB based on which image is currently active, either imageA or imageB and they type
     * of image (color or grayscale).
     * 
     * @return the active LUT/RGB table.
     */
    public ModelStorageBase getActiveLookupTable(final ModelImage kImage) {

        if (kImage == m_kVolumeImageA.GetImage()) {

            if (m_kVolumeImageA.GetImage().isColorImage()) {
                return m_kVolumeImageA.GetRGB();
            }

            return m_kVolumeImageA.GetLUT();
        } else if ( (m_kVolumeImageB.GetImage() != null) && (m_kVolumeImageB.GetImage().isColorImage())) {
            return m_kVolumeImageB.GetRGB();
        }

        return m_kVolumeImageB.GetLUT();
    }

    @Override
    public ModelLUT getActiveLUT() {
        return m_kVolumeImageA.GetLUT();
    }

    @Override
    public ModelRGB getActiveRGB() {
        return m_kVolumeImageA.GetRGB();
    }

    /**
     * Get the imageA and imageB blending value from the PlaneRender.
     * 
     * @return blendValue blender slider value.
     */
    public int getBlendValue() {
        return m_kVolOpacityPanel.getAlphaBlendSliderValue();
    }

    /**
     * Get the camera current location
     * 
     * @return camera position vector
     */
    public Vector3f getCameraLocation() {
        return raycastRenderWM.getCameraLocation();
    }

    /**
     * Get the camera parameters.
     * 
     * @return camera parameters array.
     */
    public float[] getCameraParameters() {
        return raycastRenderWM.getCameraParameters();
    }

    public Vector3f getCenterPt()
    {
        return sliceGUI.getCenter();
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#getControls()
     */
    public ViewControlsImage getControls() {
        return null;
    }

    public JFrame getFrame()
    {
        return this;
    }

    /**
     * Returns which image is active in the HistoLUT -- either imageA or imageB. Called by the PlaneRenderer object to
     * determine which LUT to update based on dragging the right-mouse in the PlaneRender window:
     * 
     * @return ModelImage, either imageA or imageB, depending on which is selected in the HistoLUT
     */
    public ModelImage getHistoLUTActiveImage() {

        if (frameHistogram != null) {

            if (frameHistogram.isImageASelected()) {
                return m_kVolumeImageA.GetImage();
            }
            return m_kVolumeImageB.GetImage();
        }

        return null;
    }

    /**
     * Returns which image is active in the HistoRGB -- either imageA or imageB. Called by the PlaneRenderer object to
     * determine which LUT to update based on dragging the right-mouse in the PlaneRender window:
     * 
     * @return ModelImage, either imageA or imageB, depending on which is selected in the HistoLUT
     */
    public ModelImage getHistoRGBActiveImage() {
        return getHistoLUTActiveImage();
    }

    /**
     * Get the image A reference.
     * 
     * @return imageA model image A reference.
     */
    public ModelImage getImageA() {
        return m_kVolumeImageA.GetImage();
    }

    /**
     * Get the imageB reference.
     * 
     * @return imageB model image B reference.
     */
    public ModelImage getImageB() {
        return m_kVolumeImageB.GetImage();
    }

    /**
     * Return the Light[] used in the volume/surface display.
     * 
     * @return
     */
    public Light[] GetLights() {
        return raycastRenderWM.GetLights();
    }

    /**
     * Get the LUT panel (only should be used with grayscale images).
     * 
     * @return the histo LUT panel
     */
    public JFrameHistogram getLUTDialog() {
        return frameHistogram;
    }

    /**
     * Return the material properties of the given surface.
     * 
     * @param kSurfaceName the surface to query.
     * @return the material properties of the surface.
     */
    public MaterialState getMaterial(final String kSurfaceName) {
        return raycastRenderWM.getMaterial(kSurfaceName);
    }

    public VolumeNode getNode(final String kSurfaceName) {
        return raycastRenderWM.GetNode(kSurfaceName);
    }

    /**
     * Get the object parameters.
     * 
     * @return Get the object rotation parameters array.
     */
    public float[] getObjectParameters() {
        return raycastRenderWM.getObjectParameters();
    }

    /**
     * Get the object rotation matrix.
     * 
     * @return rotation matrix
     */
    public Matrix3f getObjectRotation() {
        return raycastRenderWM.getObjectRotation();
    }

    /**
     * Return the opacity properties of the given surface.
     * 
     * @param kSurfaceName the surface to query.
     * @return the opacity value of the surface.
     */
    public float getOpacity(final String kSurfaceName) {
        return raycastRenderWM.getOpacity(kSurfaceName);
    }

    /**
     * Get the render mode interface panel.
     * 
     * @return render mode interface panel.
     */
    public JPanelRenderMode_WM getRendererGUI() {
        return rendererGUI;
    }


    /**
     * Return the size of the surface-area of the given surface.
     * 
     * @param kSurfaceName the surface to calculate the surface-area for.
     * @return the surface-area of the surface.
     */
    public float getSurfaceArea(final String kSurfaceName) {
        return raycastRenderWM.getSurfaceArea(kSurfaceName);
    }

    /**
     * Return the size of the surface-area of the given surface.
     * 
     * @param kSurfaceName the surface to calculate the surface-area for.
     * @return the surface-area of the surface.
     */
    public String getSurfaceAreaString(final String kSurfaceName) {
        return raycastRenderWM.getSurfaceAreaString(kSurfaceName);
    }

    /**
     * Return the surface panel.
     * 
     * @returnthe surface panel.
     */
    public JPanelSurface_WM getSurfacePanel() {
        return surfaceGUI;
    }

    /**
     * Return the translation vector for the surface with the given name.
     * 
     * @param kSurfaceName the surface to move.
     * @return the translation vector
     */
    public Vector3f getTranslateSurface(final String kSurfaceName) {
        return raycastRenderWM.getTranslateSurface(kSurfaceName);
    }
    
    public VOIManagerInterface getVOIManager()
    {
        return m_kVOIInterface;
    }

    /**
     * Return the size of the volume of the given surface.
     * 
     * @param kSurfaceName the surface to calculate the volume for.
     * @return the volume of the surface.
     */
    public float getVolume(final String kSurfaceName) {
        return raycastRenderWM.getSurfaceVolume(kSurfaceName);
    }
    
    public String getVolumeString(final String kSurfaceName)
    {
        return raycastRenderWM.getSurfaceVolumeString(kSurfaceName);    	
    }

    /**
     * @return VolumeTriPlanarRender object.
     */
    public VolumeTriPlanarRender getVolumeGPU() {
        return raycastRenderWM;
    }

    /**
     * Insert the new tab into the current visible tab list.
     * 
     * @param _name control panel name
     * @param _panel control panel
     */
    public void insertTab(final String _name, final JPanel _panel) {
        int i;

        for (i = 0; i < tabbedPane.getTabCount(); i++) {

            if ( (tabbedPane.getComponentAt(i) != null) && tabbedPane.getTitleAt(i).equals(_name)) {
                tabbedPane.setSelectedIndex(i);

                return;
            }
        }

        tabbedPane.addTab(_name, null, _panel);
        tabbedPane.setSelectedIndex(tabbedPane.getTabCount() - 1);
    }

    @Override
    public void maskToPaint()
    {
        // TODO Auto-generated method stub
    }


    public GLCanvas newSharedCanvas()
    {
    	return new GLCanvas(caps, sharedDrawable.getContext());
    }


    public GLContext getSharedContext()
    {
    	return sharedDrawable.getContext();
    }

    @Override
    public void paintToShortMask()
    {
        // TODO Auto-generated method stub
    }

    @Override
    public void paintToUbyteMask()
    {
        // TODO Auto-generated method stub
    }

    public Vector3f patientToScreen( int iActive, Vector3f kPatient )
    {
        if ( m_akPlaneRender != null )
        {
            if ( m_akPlaneRender[iActive] != null )
            {
                return m_akPlaneRender[iActive].patientToScreen(kPatient);
            }
        }
        return null;
    }

    /**
     * Enables picking correspondence points between the surface renderer and the BrainSurfaceFlattener renderer.
     * 
     * @param bOn true enables, false disables.
     */
    public void PickCorrespondence(final boolean bOn) {
        raycastRenderWM.pickCorrespondence(bOn);
    }

    /**
     * Passes the triangle indices of the picked triangle to the BrainSurfaceFlattener renderer for display.
     * 
     * @param iV0 index 0 of the picked triangle.
     * @param iV1 index 1 of the picked triangle.
     * @param iV2 index 2 of the picked triangle.
     */
    public void PickCorrespondence(final int iV0, final int iV1, final int iV2) {
        if (brainsurfaceFlattenerRender != null) {
            brainsurfaceFlattenerRender.drawPicked(iV0, iV1, iV2);
        }
    }

    public void play4D(final boolean bOn) {
        raycastRenderWM.play4D(bOn);
    }

    public void PointerActive(boolean bActive) {
        if ( m_akPlaneRender != null )
        {
            for (int i = 0; i < 3; i++) {
                if ( m_akPlaneRender[i] != null )
                {
                    m_akPlaneRender[i].setMouseActive(!bActive);
                }
            }
        }
    }

    public Vector3f PropDown(int iActive) {
        if ( m_akPlaneRender != null )
        {
            if ( m_akPlaneRender[iActive] != null )
            {
                return m_akPlaneRender[iActive].upSlice();
            }
        }
        return null;
    }

    public Vector3f PropUp(int iActive) {
        if ( m_akPlaneRender != null )
        {
            if ( m_akPlaneRender[iActive] != null )
            {
                return m_akPlaneRender[iActive].downSlice();
            }
        }
        return null;
    }

    public void refreshLighting() {
        m_kLightsPanel.refreshLighting();
    }

    /**
     * Removes all geodesic curves for the given surface.
     * 
     * @param kSurface the surface to modify.
     */
    public void removeAllGeodesic(final TriMesh kSurface) {
        raycastRenderWM.removeAllGeodesic(kSurface);
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#removeControls()
     */
    public void removeControls() {}

    /**
     * Remove the specific geodesic curves from the given surface.
     * 
     * @param kSurface the surface to modify.
     * @param iNode the node to remove.
     * @param iGroup the group the node belongs to.
     */
    public void removeGeodesic(final TriMesh kSurface, final int iNode, final int iGroup) {
        raycastRenderWM.removeGeodesic(kSurface, iNode, iGroup);
    }

    public VolumeObject removeNode(final String kNodeName) {
        return raycastRenderWM.RemoveNode(kNodeName);
    }

    /**
     * Remove the polyline from the volume DTI display.
     * 
     * @param groupIndex the polyline to remove.
     */
    public void removePolyline(final int groupIndex) {
        raycastRenderWM.removePolyline(groupIndex);
    }

    /**
     * Remove the given surface from the render display list.
     * 
     * @param kSurfaceName the name of the surface to remove.
     */
    public void removeSurface(final String kSurfaceName) {
        raycastRenderWM.removeSurface(kSurfaceName);
        setModified();
    }

    /**
     * When the Geodesic object cuts the mesh along an open curve, the old mesh changes, but does not need to be deleted
     * and no new mesh needs to be added. This function allows the Geodesic object to replace the original mesh with the
     * sliced mesh in the surface renderer. ReplaceMesh is also used to undo cutting operations.
     * 
     * @param kOld TriMesh old surface mesh
     * @param kNew TriMesh new surface mesh
     */
    public void replaceGeodesic(final TriMesh kOld, final TriMesh kNew) {
        raycastRenderWM.replaceGeodesic(kOld, kNew);
    }

    /**
     * When the mouse tranlation moves the object out of the Viewing bounding box, click the
     * home button to bring the image back to center. 
     */
    public void rollbackToImageCenter() {
    	raycastRenderWM.rollbackToCenter();
    }
    
    
    /**
     * Reset image volume orientation along X axis.
     */
    public void resetAxisX() {
        raycastRenderWM.resetAxisX();
    }

    /*
    public void updateCurrentVOI(LocalVolumeVOI kOld, LocalVolumeVOI kNew)
    {
        raycastRenderWM.updateCurrentVOI(kOld, kNew);    
        setModified();
    }
*/
    
    /**
     * Reset image volume orientation along Y axis.
     */
    public void resetAxisY() {
        raycastRenderWM.resetAxisY();
    }

    
    /**
     * Reset image volume orientation along Z axis.
     */
    public void resetImage() {
        raycastRenderWM.resetAxis();
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#setActiveImage(int)
     */
    public void setActiveImage(final int active) {}

    @Override
    public void setActiveImage(ModelImage kImage) {
        // TODO Auto-generated method stub        
    }

    public void setAnimationSpeed(final float fValue) {
        raycastRenderWM.setAnimationSpeed(fValue);
    }

    /**
     * Enables backface culling for the given surface.
     * 
     * @param kSurfaceName the surface to modify.
     * @param bOn when true back-face culling is enabled, false disables backface culling.
     */
    public void setBackface(final String kSurfaceName, final boolean bOn) {
        raycastRenderWM.setBackface(kSurfaceName, bOn);
    }

    /**
     * Sets the background color.
     * 
     * @param color
     */
    public void setBackgroundColor(final Color color) {
        raycastRenderWM.setBackgroundColor(new ColorRGBA(color.getRed() / 255.0f, color.getGreen() / 255.0f, color
                .getBlue() / 255.0f, 1.0f));
    }

    /**
     * Sets the blend value between images A and B.
     * 
     * @param iValue
     */
    public void setBlendValue(final int iValue) {
        m_kVolOpacityPanel.setAlphaBlendSliderValue(iValue);
    }

    /**
     * Sets the volume bounding box color.
     * 
     * @param color
     */
    public void setBoundingBoxColor(final Color color) {
        raycastRenderWM.setBoundingBoxColor(new ColorRGB(color.getRed() / 255.0f, color.getGreen() / 255.0f, color
                .getBlue() / 255.0f));
    }

    /**
     * Set the camera location.
     * 
     * @param v camera position vector
     */
    public void setCameraLocation(final Vector3f v) {
        raycastRenderWM.setCameraLocation(v);
    }

    /**
     * Display the camera parameters in the user-interface.
     */
    public void setCameraParameters() {
        displayGUI.displayCameraParams(raycastRenderWM.getCameraParameters());
    }

    public void setCenter( Vector3f kCenter )
    {
        setSliceFromPlane(kCenter);
    }

    /**
     * Enable clipping for the given surface.
     * 
     * @param kSurfaceName the surface to modify.
     * @param bClip true enables clipping, false disables clipping.
     */
    public void setClipping(final String kSurfaceName, final boolean bClip) {
        raycastRenderWM.setClipping(kSurfaceName, bClip);
    }

    /**
     * Set the color for the given surface.
     * 
     * @param kSurfaceName the surface to modify.
     * @param kColor the new color.
     */
    public void setColor(final String kSurfaceName, final ColorRGB kColor, final boolean bUpdate) {
        raycastRenderWM.setColor(kSurfaceName, kColor, bUpdate);
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#setControls()
     */
    public void setControls() {}
    
    public void SetCustomBlend(final int iBlendEquation, final int iLogicOp, final int iSrcBlend, final int iDstBlend,
            final ColorRGBA kColor) {
        raycastRenderWM.SetCustomBlend(iBlendEquation, iLogicOp, iSrcBlend, iDstBlend, kColor);
    }

    public void setDefaultCursor( )
    {
        setCursor( new Cursor(Cursor.DEFAULT_CURSOR) );
        if ( m_kVOIInterface != null )
        {
            Component kComponent = m_kVOIInterface.getToolBar().getComponentAtIndex(0);
            if ( kComponent instanceof JToggleButton )
            {
                JToggleButton kButton = (JToggleButton)m_kVOIInterface.getToolBar().getComponentAtIndex(0);
                kButton.setSelected(true);
            }
        }
    }

    /**
     * Passes the picked dropper color to the surface interface.
     * 
     * @param kDropperColor the color of the surface at the picked point.
     * @param kPickPoint the picked point for use in the region-grow operation.
     */
    public void setDropperColor(final ColorRGBA kDropperColor, final Vector3f kPickPoint) {
        if (surfaceGUI != null) {
            surfaceGUI.setDropperColor(kDropperColor, kPickPoint);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#setEnabled(boolean)
     */
    public void setEnabled(final boolean flag) {}

    /**
     * Passes the picked point to the Geodesic object for calculating the geodesic curve on the TriMesh surface.
     * 
     * @param kMesh the surface that was picked.
     * @param kPickPoint the picked point.
     */
    public void setGeodesic(final TriMesh kMesh, final PickRecord kPickPoint) {
        if (geodesicGUI != null) {
            geodesicGUI.setPickedPoint(kPickPoint, kMesh);
        }
    }

    /**
     * Turn the gradient magnitude filter on/off for volume shaders.
     * 
     * @param bShow on/off.
     */
    public void setGradientMagnitude(final boolean bShow) {
        TransferFunction kTransfer = m_kVolOpacityPanel.getCompA_GM().getOpacityTransferFunction();
        m_kVolumeImageA.SetGradientMagnitude(m_kVolOpacityPanel.getGradMagA(), false, "A");
        m_kVolumeImageA.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagA());
        if (m_kVolumeImageB.GetImage() != null) {
            kTransfer = m_kVolOpacityPanel.getCompB_GM().getOpacityTransferFunction();
            m_kVolumeImageB.SetGradientMagnitude(m_kVolOpacityPanel.getGradMagB(), false, "B");
            m_kVolumeImageB.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagB());
        }
        raycastRenderWM.setGradientMagnitude(bShow);
    }

    /**
     * Sets the ModelImage to use as an alternative to the volume ModelImage for surface texturing.
     * 
     * @param kSurfaceName the surface to modify.
     * @param kImage the alternate ModelImage to use for the surface texture.
     */
    public void SetImageNew(final String kSurfaceName, final ModelImage kImage) {
        raycastRenderWM.setImageNew(kSurfaceName, kImage);
    }

    /**
     * Sets the inter-pupillary distance for stereo rendering.
     * 
     * @param fIPD the IPD value.
     */
    public void setIPD(final float fIPD) {
        raycastRenderWM.setIPD(fIPD);
    }

    /**
     * Sets the LUT to use as an alternative to the volume lut for surface texturing.
     * 
     * @param kSurfaceName the surface to modify.
     * @param kLUT the new LUT.
     * @param kRGBT the new ModelRGB (for color images).
     */
    public void SetLUTNew(final String kSurfaceName, final ModelStorageBase kLUT) {
        raycastRenderWM.setLUTNew(kSurfaceName, kLUT);
    }

    /**
     * Sets the material for the given surface.
     * 
     * @param kSurfaceName the surface to update.
     * @param kMaterial the new material.
     */
    public void setMaterial(final String kSurfaceName, final MaterialState kMaterial, final boolean bUpdate) {
        raycastRenderWM.setMaterial(kSurfaceName, kMaterial, bUpdate);
    }

    /**
     * Causes the bottom three panels to re-display.
     */
    public void setModified() {
        if (m_akPlaneRender != null) {
            for (int i = 0; i < 3; i++) {
                if (m_akPlaneRender[i] != null) {
                    m_akPlaneRender[i].updateDisplay();
                }
            }
        }
    }

    /**
     * Set the object rotation parameters for displaying.
     */
    public void setObjectParameters() {
        displayGUI.displayObjectParams(raycastRenderWM.getObjectParameters());
    }

    /**
     * Set the object rotation matrix
     * 
     * @param rot rotation matrix
     */
    public void setObjectRotation(final Matrix3f rot) {
        raycastRenderWM.setObjectRotation(rot);
    }
    
    @Override
    public void setPaintMask(BitSet mask) {
        // TODO Auto-generated method stub
        
    }

    /**
     * Turn picking on/off for the given surface.
     * 
     * @param kSurfaceName the surface to modify.
     * @param bOn when true enable picking, false disables picking.
     */
    public void setPickable(final String kSurfaceName, final boolean bOn) {
        raycastRenderWM.setPickable(kSurfaceName, bOn);
    }

    /**
     * Set the polygon mode (FILL, LINE, POINT) for the given surface.
     * 
     * @param kSurfaceName the surface to modify.
     * @param eMode FILL, LINE, or POINT.
     */
    public void setPolygonMode(final String kSurfaceName, final WireframeState.FillMode eMode) {
        raycastRenderWM.setPolygonMode(kSurfaceName, eMode);
    }

    /**
     * Sets the position labels.
     * 
     * @param position the slice positions in FileCoordinates.
     */
    public void setPositionLabels(final Vector3f position) {
        positionsPanel.setPositionLabels(position);
    }

    /**
     * Toggles between radiological and neurological views of the data.
     * 
     * @param bOn when true display using radiological coordinates, when false use neurological.
     */
    public void setRadiological(final boolean bOn) {
        m_kVolumeImageA.GetImage().setRadiologicalView(bOn);

        if (m_kVolumeImageB.GetImage() != null) {
            m_kVolumeImageB.GetImage().setRadiologicalView(bOn);
        }
        Vector3f center = sliceGUI.getCenter();

        raycastRenderWM.setCenter( new Vector3f( center.X, center.Y, center.Z ) );    
        for (int i = 0; i < 3; i++) {
            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].setRadiologicalView(bOn);
                m_akPlaneRender[i].setCenter(center);
            }
        }
        setPositionLabels(center);
    }

    /**
     * Switches between orthographic and perspective projection.
     * 
     * @param bEnable when true enable perspective projection, when false use orthographic projection.
     */
    public void setRenderPerspective(final boolean bEnable) {
        if (bEnable) {
            raycastRenderWM.setPerspectiveProjection();
        } else {
            raycastRenderWM.setOrthographicProjection();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#setRGBTA(gov.nih.mipav.model.structures.ModelRGB)
     */
    public void setRGBTA(final ModelRGB RGBT) {
        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.SetRGBT(RGBT);
        }
        raycastRenderWM.setRGBTA(RGBT);
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#setRGBTB(gov.nih.mipav.model.structures.ModelRGB)
     */
    public void setRGBTB(final ModelRGB RGBT) {
        if (m_kVolumeImageB != null) {
            m_kVolumeImageB.SetRGBT(RGBT);
        }
        raycastRenderWM.setRGBTB(RGBT);
    }

    /**
     * Turn the volume bounding box frame on/off.
     * 
     * @param bShow when true display the bounding box.
     */
    public void setShowBoxFrame(final boolean bShow) {
        raycastRenderWM.displayBoundingBox(bShow);
    }

    /**
     * Turn the orientation cube on/off.
     * 
     * @param bShow when true display the orientation cube, when false do not display the cube.
     */
    public void setShowOrientationCube(final boolean bShow) {
        raycastRenderWM.displayOrientationCube(bShow);
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#setSlice(int)
     */
    public void setSlice(final int slice) {}

    /**
     * Sets the position of the slices in the SurfaceRender and PlaneRender objects. Called from the PlaneRender class.
     * 
     * @param center the new slice positions in FileCoordinates
     */
    public void setSliceFromPlane(final Vector3f center) {
        setPositionLabels(center);

        for (int i = 0; i < 3; i++) {
            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].setCenter(center);
            }
        }

        raycastRenderWM.setCenter(center);
        sliceGUI.setCenter((int) center.X, (int) center.Y, (int) center.Z);
    }

    /**
     * When the mouse move in bottom three planar view, need to update the camera view location to the 
     * indicated image center.   
     * @param center  image center
     */
    public void setCamera(final Vector3f center) {
    	raycastRenderWM.setCameraCenter(center);
    }
    
    /**
     * Sets the position of the slices in the PlaneRender. Called from the SurfaceRender class.
     * 
     * @param center the new slice positions in FileCoordinates
     */
    public void setSliceFromSurface(final Vector3f center) {
        setPositionLabels(center);

        if (m_akPlaneRender != null) {
            for (int i = 0; i < 3; i++) {

                if (m_akPlaneRender[i] != null) {
                    m_akPlaneRender[i].setCenter(center);
                }
            }
        }
        raycastRenderWM.setCenter( new Vector3f( center.X, center.Y, center.Z ) );
    }

    /**
     * Sets the color for the PlaneRender iView (AXIAL, SAGITTAL, CORONAL) slice.
     * 
     * @param iView (AXIAL, SAGITTAL, CORONAL)
     * @param color the new axis color attribute.
     */
    public void setSliceHairColor(final int iView, final Color color) {

        final ColorRGB kColor = new ColorRGB(color.getRed() / 256.0f, color.getGreen() / 256.0f,
                color.getBlue() / 256.0f);

        for (int i = 0; i < 3; i++) {
            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].setSliceHairColor(iView, kColor);
            }
        }
        raycastRenderWM.setBoundingBoxColor(iView, kColor);
    }

    /**
     * Set the transparency value for the slice.
     * 
     * @param i the slice to modify.
     * @param fAlpha the new transparency value.
     */
    public void setSliceOpacity(final int i, final float fAlpha) {
        raycastRenderWM.setSliceOpacity(i, fAlpha);
    }

    /**
     * Turns on surface texture display for the given surface. The user can use a separate ModelImage and LUT than the
     * one displayed in the volume renderer.
     * 
     * @param kSurfaceName the name of the surface to texture.
     * @param bOn texture on/off.
     * @param bUseNewImage when false use the current ModelImage, when true the user specifies the model image.
     * @param bUseNewLUT when false use the current LUT, when true the user specifies the LUT.
     */
    public void setSurfaceTexture(final String kSurfaceName, final boolean bOn, final boolean bUseNewImage,
            final boolean bUseNewLUT) {
        raycastRenderWM.setSurfaceTexture(kSurfaceName, bOn, bUseNewImage, bUseNewLUT);
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#setTimeSlice(int)
     */
    public void setTimeSlice(final int slice) {
        m_kVolume4DGUI.setTimeSlice(slice);
        m_kVolumeImageA.SetTimeSlice(slice);
        setModified();
    }

    /**
     * Set the transparency for the given surface.
     * 
     * @param kSurfaceName the name of the surface to modify.
     * @param fValue transparency value.
     */
    public void setTransparency(final String kSurfaceName, final float fValue) {
        raycastRenderWM.blend(kSurfaceName, fValue);
    }

    /**
     * Turns showing the slice bounding box on/off.
     * 
     * @param i which slice bounding box to turn off.
     * @param bShow on/off.
     */
    public void showBoundingBox(final int i, final boolean bShow) {
        raycastRenderWM.showBoundingBox(i, bShow);
    }

    /**
     * Turns showing the slice on/off.
     * 
     * @param i which slice to turn off.
     * @param bShow on/off.
     */
    public void showSlice(final int i, final boolean bShow) {
        raycastRenderWM.showSlice(i, bShow);
        setModified();
    }

    /**
     * Smooth the given surface.
     * 
     * @param kSurfaceName the name of the surface to smooth.
     * @param iteration smooth iterations.
     * @param alpha smooth factor.
     * @param volumeLimit whether to use a volume % change limit.
     * @param volumePercent the % volume change limiting factor
     */
    public void smoothMesh(final String kSurfaceName, final int iteration, final float alpha,
            final boolean volumeLimit, final float volumePercent) {
        raycastRenderWM.smoothMesh(kSurfaceName, iteration, alpha, volumeLimit, volumePercent);
    }

    /**
     * Smooth the given surface.
     * 
     * @param kSurfaceName the name of the surface to smooth.
     * @param iteration smooth iterations.
     * @param lambda smooth factor.
     * @param mu smooth factor.
     */
    public void smoothThree(final String kSurfaceName, final int iteration, final float lambda, final float mu) {
        raycastRenderWM.smoothThree(kSurfaceName, iteration, lambda, mu);
    }

    /**
     * Smooth the given surface.
     * 
     * @param kSurfaceName the name of the surface to smooth.
     * @param iteration smooth iterations.
     * @param fStiffness stiffness factor.
     * @param volumeLimit whether to use a volume % change limit.
     * @param volumePercent the % volume change limiting factor.
     */
    public void smoothTwo(final String kSurfaceName, final int iteration, final float fStiffness,
            final boolean volumeLimit, final float volumePercent) {
        raycastRenderWM.smoothTwo(kSurfaceName, iteration, fStiffness, volumeLimit, volumePercent);
    }

    /**
     * Does nothing.
     * 
     * @param event the change event
     */
    public void stateChanged(final ChangeEvent event) 
    {
    	if ( (multiHistogramGUI == null) || (multiHistogramGUI.getMainPanel() == null) )
    	{
    		return;
    	}
    	if ( (event.getSource() == tabbedPane) &&
    			(tabbedPane.getSelectedComponent() == multiHistogramGUI.getMainPanel()) )
    	{
            multiHistogramGUI.getHistogram().display();
            multiHistogramGUI.update();

    	}
    }

    public void SURMode( boolean bSURFast )
    {

        setCursor(new Cursor(Cursor.WAIT_CURSOR));
        m_kVolumeImageA.GenerateNormalFiles( this );
        if ( m_kVolumeImageB.GetImage() != null )
        {
            m_kVolumeImageB.GenerateNormalFiles( this );
        }
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        
    	if ( bSURFast )
    	{
    		raycastRenderWM.SURFASTMode();
    	}
    	else
    	{
    		raycastRenderWM.SURMode();
    	}
		refreshLighting();
    }

    /**
     * Switches between different ways of displaying the geodesic path (Euclidean, Geodesic, or Mesh).
     * 
     * @param kSurfaceName the surface the path is on.
     * @param iWhich the type of display.
     */
    public void toggleGeodesicPathDisplay(final String kSurfaceName, final int iWhich) {
        raycastRenderWM.toggleGeodesicPathDisplay(kSurfaceName, iWhich);
    }

    /**
     * Toggle the display on/off for the given Node.
     * 
     * @param kNode node to toggle on/off.
     * @param bDisplay display toggle on/off.
     */
    public void toggleNode(final Node kNode, final boolean bDisplay) {
        raycastRenderWM.displayNode(kNode, bDisplay);
    }

    /**
     * Changes the translation vector for the surface with the given name.
     * 
     * @param kSurfaceName the surface to move.
     * @param kTranslate the new translation vector
     */
    public void translateSurface(final String kSurfaceName, final Vector3f kTranslate) {
        raycastRenderWM.translateSurface(kSurfaceName, kTranslate);
    }

    /**
     * update blending between images A/B.
     */
    public void updateABBlend() {
        raycastRenderWM.setABBlend(1 - getBlendValue() / 100.0f);
        setModified();
    }

    /**
     * Causes the PlaneRender objects to update the texture maps when the underlying ModelImage changes.
     */
    public void updateData(boolean bCopyToCopy) {
        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.UpdateData(m_kVolumeImageA.GetImage(), bCopyToCopy);
        }
        if ( m_kVolumeImageB.GetImage() != null )
        {
            m_kVolumeImageB.UpdateData(m_kVolumeImageB.GetImage(), bCopyToCopy);
        }
        raycastRenderWM.updateData();
        setModified();
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#updateImageExtents()
     */
    public boolean updateImageExtents() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages()
     */
    public boolean updateImages() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(boolean)
     */
    public boolean updateImages(final boolean forceShow) {

        if (m_kVolOpacityPanel == null) {
            return false;
        }
        final ViewJComponentVolOpacityBase kSelectedComp = m_kVolOpacityPanel.getSelectedComponent();
        if (kSelectedComp == null) {
            return false;
        }
        if (m_kVolumeImageA != null) {
            if ( (forceShow && m_kVolOpacityPanel.getCompA() != null)
                    || (kSelectedComp == m_kVolOpacityPanel.getCompA())) {
                final TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
                m_kVolumeImageA.UpdateImages(kTransfer, 0, null);
            } 
            if ( (forceShow && m_kVolOpacityPanel.getCompA_GM() != null)
                    || (kSelectedComp == m_kVolOpacityPanel.getCompA_GM())) {
                final TransferFunction kTransfer = m_kVolOpacityPanel.getCompA_GM().getOpacityTransferFunction();
                m_kVolumeImageA.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagA());
            }
        }
        if (m_kVolumeImageB != null) {
            if ( (forceShow && m_kVolOpacityPanel.getCompB() != null)
                    || (kSelectedComp == m_kVolOpacityPanel.getCompB())) {
                final TransferFunction kTransfer = m_kVolOpacityPanel.getCompB().getOpacityTransferFunction();
                m_kVolumeImageB.UpdateImages(kTransfer, 0, null);
            } 
            if ( (forceShow && m_kVolOpacityPanel.getCompB_GM() != null)
                    || (kSelectedComp == m_kVolOpacityPanel.getCompB_GM())) {
                final TransferFunction kTransfer = m_kVolOpacityPanel.getCompB_GM().getOpacityTransferFunction();
                m_kVolumeImageB.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagB());
            }
        }

        return true;

    }

    /**
     * This methods calls corresponding render to update images with LUT changes.
     * 
     * @param LUTa LUT used to update imageA
     * @param LUTb LUT used to update imageB
     * @param forceShow forces show to reimport image and calc. java image
     * @param interpMode image interpolation method (Nearest or Smooth)
     * 
     * @return boolean confirming successful update
     */
    public boolean updateImages(final ModelLUT LUTa, final ModelLUT LUTb, final boolean forceShow, final int interpMode) {
        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.UpdateImages(LUTa);
            setModified();
        }
        if (m_kVolumeImageB.GetImage() != null) {
            m_kVolumeImageB.UpdateImages(LUTb);
            setModified();
        }

        return true;
    }

    public void updateLevWidgetState(final Vector<ClassificationWidget> kLWS) {
        raycastRenderWM.updateLevWidgetState(kLWS);
    }

    public void updateLighting(final Light[] akGLights) {
        raycastRenderWM.updateLighting(akGLights);
        for (int i = 0; i < 3; i++) {
            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].updateLighting(akGLights);
            }
        }
        if (brainsurfaceFlattenerRender != null) {
            brainsurfaceFlattenerRender.updateLighting(akGLights);
        }
        if (m_kFlyThroughRender != null) {
            m_kFlyThroughRender.updateLighting(akGLights);
        }
    }

    /**
     * Update the multi-histogram tab when the MultiHistogram checkbox is checked in the renderMode panel.
     * 
     * @param flag MultiHistogram Check box checked or not.
     */
    public void updateMultihistoTab(final boolean flag) {
    	if ( !m_kVolumeImageA.isHistoInit() && flag )
    	{
    		m_kVolumeImageA.SetGradientMagnitude(null, true, "A");    
    		if (m_kVolumeImageB.GetImage() != null && !m_kVolumeImageB.isHistoInit() ) {
                m_kVolumeImageB.SetGradientMagnitude(m_kVolOpacityPanel.getGradMagB(), false, "B");
            }
    	}
        if (flag) {
        	if (multiHistogramGUI == null )
        	{
        		multiHistogramGUI = new JPanelMultiDimensionalTransfer(new GLCanvas(caps, sharedDrawable.getContext()), this, m_kVolumeImageA);
        		maxPanelWidth = Math.max(multiHistogramGUI.getPreferredSize().width, maxPanelWidth);
        	}
        	
            insertTab("MultiHistogram", multiHistogramGUI.getMainPanel());
            multiHistogramGUI.setMinMax( (float)m_kVolumeImageA.GetImage().getMin(), (float)m_kVolumeImageA.GetImage().getMax(), 
            		m_kVolumeImageA.GetGradientMagnitudeMin(), m_kVolumeImageA.GetGradientMagnitudeMax() );
            multiHistogramGUI.getHistogram().display();
            multiHistogramGUI.update();
            rendererGUI.setDisplayVolumeCheck(true);
            raycastRenderWM.displayVolumeRaycast(true);
            raycastRenderWM.setVolumeBlend(rendererGUI.getBlendSliderValue() / 100.0f);
     
        } 
    }

    /**
     * Causes the texture representation of all the surface meshes to be recalculated.
     */
    public void updatePlanes() {
        raycastRenderWM.redrawSurfaceTexture();
        raycastRenderWM.GetCanvas().display();
        setModified();
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#windowActivated(java.awt.event.WindowEvent)
     */
    public void windowActivated(final WindowEvent event) {
        setModified();
        resizePanel();
        if ( (multiHistogramGUI != null) && (multiHistogramGUI.getHistogram() != null) )
    	{
            multiHistogramGUI.getHistogram().display();
    	}
    }

    public void windowClosed(final WindowEvent arg0) {}

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.ViewJFrameBase#windowClosing(java.awt.event.WindowEvent)
     */
    public void windowClosing(final WindowEvent event) {
        close();
        disposeLocal(true);
        dispose();
    }

    /**
     * Does nothing.
     * 
     * @param event the window event
     */
    public void windowDeactivated(final WindowEvent event) {}
    
    /**
     * Does nothing.
     * 
     * @param event the window event
     */
    public void windowDeiconified(final WindowEvent event) {}
    

    
    /**
     * Does nothing.
     * 
     * @param event the window event
     */
    public void windowIconified(final WindowEvent event) {}
    
    public void windowOpened(final WindowEvent arg0) {}


    private void buildImageIndependentComponents() {
        buildDisplayPanel();
        buildGeodesic();
        buildCustomBlendPanel();
    }


    private void disposeImageDependentComponents() {
        if ( !m_bDependentInterfaceInit) {
            return;
        }
        m_bDependentInterfaceInit = false;

        if (surfaceGUI != null) {
            surfaceGUI.disposeLocal();
            surfaceGUI = null;
        }
        if (m_kVolume4DGUI != null) {
            m_kVolume4DGUI.disposeLocal();
            m_kVolume4DGUI = null;
        }
        if (m_kLightsPanel != null) {
            m_kLightsPanel.disposeLocal();
            m_kLightsPanel = null;
        }
        if (sculptGUI != null) {
            sculptGUI.disposeLocal();
            sculptGUI = null;
        }
        if (multiHistogramGUI != null) {
            multiHistogramGUI.disposeLocal();
            multiHistogramGUI = null;
        }
        if (surfaceTextureGUI != null) {
            surfaceTextureGUI.disposeLocal();
            surfaceTextureGUI = null;
        }
        if (positionsPanel != null) {
            positionsPanel.disposeLocal();
            positionsPanel = null;
        }
        if (clipGUI != null) {
            clipGUI.disposeLocal();
            clipGUI = null;
        }
        if (sliceGUI != null) {
            sliceGUI.disposeLocal();
            sliceGUI = null;
        }
        if (frameHistogram != null) {
        	frameHistogram.disposeLocal();
        	frameHistogram = null;
        }
        if (m_kVolOpacityPanel != null) {
            m_kVolOpacityPanel.disposeLocal();
            m_kVolOpacityPanel = null;
        }
        if (rendererGUI != null) {
            rendererGUI.disposeLocal();
            rendererGUI = null;
        }
        if ( m_kVOIInterface != null )
        {
            m_kVOIInterface.disposeLocal(true);
            m_kVOIInterface = null;
        }
        if (mouseGUI != null) {
            mouseGUI.disposeLocal();
            mouseGUI = null;
        }
    }

    private void disposeImageIndependentComponents() {
//    	if(SpaceNavigatorController.hasSpaceNavigator()) {
//        	SpaceNavigatorPoller.deRegisterListener(this);
//        }
    	
    	if (displayGUI != null) {
            displayGUI.disposeLocal();
            displayGUI = null;
        }
        if (surfaceGUI != null) {
            surfaceGUI.disposeLocal();
            surfaceGUI = null;
        }
        if (geodesicGUI != null) {
            geodesicGUI.disposeLocal();
            geodesicGUI = null;
        }
        if (customBlendGUI != null) {
            customBlendGUI.disposeLocal();
            customBlendGUI = null;
        }
    }

    private void disposeRenderers() {
        if (brainsurfaceFlattenerRender != null) {
        	brainsurfaceFlattenerRender.setVisible(false);
        	brainsurfaceFlattenerRender.dispose();
        	bf_flyPanel.remove(brainsurfaceFlattenerRender.GetCanvas());
        }
    	
        if (raycastRenderWM != null) {
            raycastRenderWM.setVisible(false);
            raycastRenderWM.dispose();
            gpuPanel.remove(raycastRenderWM.GetCanvas());
        }
        if (m_akPlaneRender != null) {
        	if ( m_akPlaneRender[0] != null )
        	{
        		panelAxial.remove(m_akPlaneRender[0].GetCanvas());
        	}
        	if ( m_akPlaneRender[1] != null )
        	{
        		panelSagittal.remove(m_akPlaneRender[1].GetCanvas());
        	}
        	if ( m_akPlaneRender[2] != null )
        	{
        		panelCoronal.remove(m_akPlaneRender[2].GetCanvas());
        	}
	        for (int i = 0; i < 3; i++) {
	
	            if (m_akPlaneRender[i] != null) {
	            	m_akPlaneRender[i].setVisible(false);
	                m_akPlaneRender[i].dispose();
	            }
	        }
        }
        
        if ( sharedRenderer != null )
        {
        	sharedRenderer.dispose();
        	sharedRenderer = null;
        }
        if ( sharedDrawable != null )
        {
        	releaseShared();
        	sharedDrawable = null;
        }

        if (m_kVolumeImageA != null) {
            if (m_kVolumeImageA.GetImage() != null) {
                m_kVolumeImageA.GetImage().removeImageDisplayListener(this);
            }
            m_kVolumeImageA.dispose();
            m_kVolumeImageA = null;
        }

        if (m_kVolumeImageB != null) {
            if (m_kVolumeImageB.GetImage() != null) {
                m_kVolumeImageB.GetImage().removeImageDisplayListener(this);
            }
            m_kVolumeImageB.dispose();
            m_kVolumeImageB = null;
        }
    }

    private String getVolumeRenderStateFile(boolean bSave) {
        final JFileChooser chooser = new JFileChooser();
        chooser.setMultiSelectionEnabled(false);

        // TODO: Use FileNameExtensionFilter introduced in 1.6
        FileNameExtensionFilter kFileExtFilter = new FileNameExtensionFilter( "VolumeRenderStateFiles", "vrs" );
        chooser.addChoosableFileFilter(kFileExtFilter);

        final FileFilter kFileFilter = new FileFilter() {
            public boolean accept(File f) {
                if (f.getName().toLowerCase().endsWith(".vrs")) {
                    return true;
                }
				return false;
            }

            public String getDescription() {
                return "VolumeRenderStateFiles";
            }
        };
        chooser.addChoosableFileFilter(kFileFilter);

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        if (bSave && JFileChooser.APPROVE_OPTION != chooser.showSaveDialog(null)) {
            return null;
        }
        else if (!bSave && JFileChooser.APPROVE_OPTION != chooser.showSaveDialog(null)) {
            return null;
        }
        String kFile = chooser.getSelectedFile().getName();
        final String kDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
        chooser.setVisible(false);
        if ( !kFile.endsWith(".vrs")) {
            kFile = kFile.concat(".vrs");
        }
        return new String(kDir + kFile);
    }

    private void initVOI()
    {        
        setSliceFromSurface( sliceGUI.getCenter() );
        m_kVOIInterface = new VOIManagerInterface( this, m_kVolumeImageA.GetImage(),
                m_kVolumeImageB.GetImage(), 3, true, null );
        panelToolbar.add( m_kVOIInterface.getToolBar(), panelToolBarGBC );
        for ( int i = 0; i < 3; i++ )
        {
            m_kVOIInterface.getVOIManager(i).init( this, m_kVolumeImageA.GetImage(), m_kVolumeImageB.GetImage(),                     
                    m_akPlaneRender[i].GetCanvas(), m_akPlaneRender[i], 
                    m_akPlaneRender[i].getOrientation() );
        }
        menuBar.add(voiMenu);
    }

    private void LoadState() {
        final String kFile = getVolumeRenderStateFile(false);
        if (kFile == null) {
            return;
        }
        disposeRenderers();
        disposeImageDependentComponents();
        try {
            ObjectInputStream objstream;
            objstream = new ObjectInputStream(new FileInputStream(kFile));
            final VolumeRenderState kState = (VolumeRenderState) objstream.readObject();
            objstream.close();
            if (kState != null) {
                RestoreState(kState);
            }
        } catch (final FileNotFoundException e) {
            e.printStackTrace();
        } catch (final ClassNotFoundException e) {
            e.printStackTrace();
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }
    

    private void RestoreState(final VolumeRenderState kState) {
        m_kVolumeImageA = kState.ImageA;
        m_kVolumeImageB = kState.ImageB;
        m_kVolumeImageA.UpdateImages(kState.LUTa);
        m_kVolumeImageA.SetRGBT(kState.RGBa);

        m_kVolumeImageA.GetImage().setImageOrder(ModelImage.IMAGE_A);
        if (m_kVolumeImageB.GetImage() != null) {
            m_kVolumeImageB.GetImage().setImageOrder(ModelImage.IMAGE_B);
            m_kVolumeImageB.UpdateImages(kState.LUTb);
            m_kVolumeImageB.SetRGBT(kState.RGBb);
        }
        /** Progress bar show up during the volume view frame loading */
        final ViewJProgressBar progressBar = new ViewJProgressBar("Creating Volume & Surface Renderer...",
                "Creating Volume & Surface Renderer...", 0, 100, false, null, null);
        MipavUtil.centerOnScreen(progressBar);
        progressBar.setVisible(true);
        progressBar.updateValueImmed(0);        

        constructRenderers(progressBar);

        setBlendValue(kState.Blend);

        // Menu:
        menuObj.getMenuItem("Show axes").setSelected(kState.ShowAxes);
        for (int i = 0; i < 3; i++) {
            m_akPlaneRender[i].showAxes(kState.ShowAxes);
            //m_akPlaneRender[i].updateDisplay();
        }
        menuObj.getMenuItem("Show crosshairs").setSelected(kState.ShowCrossHairs);
        //final boolean showXHairs = menuObj.isMenuItemSelected("Show crosshairs");
        for (int i = 0; i < 3; i++) {
            m_akPlaneRender[i].showXHairs(kState.ShowCrossHairs);
            //m_akPlaneRender[i].updateDisplay();
        }
        menuObj.getMenuItem("VOI toolbar").setSelected(kState.ShowVOI);
        if ( kState.ShowVOI && (m_kVOIInterface == null) )
        {
            initVOI();
            m_kVOIInterface.getToolBar().setVisible(kState.ShowVOI);
        }
        menuObj.getMenuItem("4D toolbar").setSelected(kState.Show4D);
        if (kState.Show4D) {
            insertTab("4D", m_kVolume4DGUI.getMainPanel());
            resizePanel();
        }
        // Position Panel:
        setRadiological(kState.Radiological);
        positionsPanel.setRadiological(kState.Radiological);

        // LUT:
        if (kState.TransferA != null) {
            m_kVolumeImageA.GetLUT().setTransferFunction(kState.TransferA);
            m_kVolumeImageA.UpdateImages(m_kVolumeImageA.GetLUT());
        }
        if (kState.RGBa != null) {
            m_kVolumeImageA.GetRGB().setRedFunction(kState.RedA);
            m_kVolumeImageA.GetRGB().setGreenFunction(kState.GreenA);
            m_kVolumeImageA.GetRGB().setBlueFunction(kState.BlueA);
            frameHistogram.setRedOn(kState.RedOnA, true);
            frameHistogram.setGreenOn(kState.GreenOnA, true);
            frameHistogram.setBlueOn(kState.BlueOnA, true);
            kState.RGBa.setROn(kState.RedOnA);
            kState.RGBa.setGOn(kState.GreenOnA);
            kState.RGBa.setBOn(kState.BlueOnA);
            m_kVolumeImageA.SetRGBT(m_kVolumeImageA.GetRGB());
            raycastRenderWM.setRGBTA(m_kVolumeImageA.GetRGB());
        }
        if (m_kVolumeImageB.GetImage() != null) {
            if (kState.TransferB != null) {
                m_kVolumeImageB.GetLUT().setTransferFunction(kState.TransferB);
                m_kVolumeImageB.UpdateImages(m_kVolumeImageB.GetLUT());
            }
            if (kState.RGBb != null) {
                m_kVolumeImageB.GetRGB().setRedFunction(kState.RedB);
                m_kVolumeImageB.GetRGB().setGreenFunction(kState.GreenB);
                m_kVolumeImageB.GetRGB().setBlueFunction(kState.BlueB);
                frameHistogram.setRedOn(kState.RedOnB, false);
                frameHistogram.setGreenOn(kState.GreenOnB, false);
                frameHistogram.setBlueOn(kState.BlueOnB, false);
                kState.RGBb.setROn(kState.RedOnB);
                kState.RGBb.setGOn(kState.GreenOnB);
                kState.RGBb.setBOn(kState.BlueOnB);
                m_kVolumeImageB.SetRGBT(m_kVolumeImageB.GetRGB());
                raycastRenderWM.setRGBTB(m_kVolumeImageB.GetRGB());
            }
        }
        // Opacity:
        m_kVolOpacityPanel.getCompA().updateTransFunc(kState.OpacityA);
        if (kState.OpacityGMA != null) {
            m_kVolOpacityPanel.addGM();
            m_kVolOpacityPanel.getCompA_GM().updateTransFunc(kState.OpacityGMA);
            m_kVolOpacityPanel.setGradientMagnitudeOpacityEnabled(kState.OpacityGMOnA);
        }
        if (kState.OpacityB != null) {
            m_kVolOpacityPanel.getCompB().updateTransFunc(kState.OpacityB);
            if (kState.OpacityGMB != null) {
                m_kVolOpacityPanel.addGM();
                m_kVolOpacityPanel.getCompB_GM().updateTransFunc(kState.OpacityGMB);
                m_kVolOpacityPanel.setGradientMagnitudeOpacityEnabled(kState.OpacityGMOnB);
            }
        }
        m_kVolOpacityPanel.setSelectedTabIndex(kState.SelectedTab);
        this.updateImages(true);

        // Slice Panel:
        for (int i = 0; i < 3; i++) {
            sliceGUI.setOpacity(i, kState.Opacity[i]);
            sliceGUI.setColor(i, kState.SliceColor[i]);
            sliceGUI.setShowSlice(i, kState.ShowSlice[i]);
            sliceGUI.setShowBound(i, kState.ShowSliceBox[i]);
        }
        setSliceFromPlane(kState.Center);

        // Render Mode Panel:
        rendererGUI.setRenderMode(kState.RenderMode);
        rendererGUI.setBlendSliderValue(kState.VolumeBlend);
        rendererGUI.setReleasedSliderValue(kState.ReleasedSamples);
        rendererGUI.setMovingSliderValue(kState.RotationSamples);
        rendererGUI.setIntensityLevel(kState.ExtractionIntensityLevel);
        // Custom Blend Panel:
        customBlendGUI.setUpdate(kState.RenderMode == 5);
        customBlendGUI.setEquation(kState.Equation);
        customBlendGUI.setSource(kState.SourceBlend);
        customBlendGUI.setDestination(kState.DestinationBlend);
        customBlendGUI.setColor(kState.BlendColor);
        customBlendGUI.setAlpha(kState.CustomAlpha);
        customBlendGUI.setUpdate(true);
        if (kState.RenderMode == 5) {
            customBlendGUI.updateVolumeRenderer();
        }
        // MultiHisto Panel:
        multiHistogramGUI.getHistogram().setWidgets(kState.MultiHistoWidgets);
        multiHistogramGUI.getHistogram().setPicked(kState.WidgetSelected);
        rendererGUI.setMultiHistoEnabled(kState.MultiHistogram);
        // Render Mode Panel:
        rendererGUI.setDisplayVolumeCheck(kState.DisplayRayCast);
        raycastRenderWM.displayVolumeRaycast(rendererGUI.getVolumeCheck().isSelected());
        rendererGUI.setDisplaySlicesCheck(kState.DisplaySlices);
        raycastRenderWM.displayVolumeSlices(rendererGUI.getSlicesCheck().isSelected());
        rendererGUI.setDisplaySurfaceCheck(kState.DisplaySurface);
        raycastRenderWM.displaySurface(rendererGUI.getSurfaceCheck().isSelected());
        rendererGUI.setStereo(kState.StereoType);
        if (kState.StereoType != 0) {
            raycastRenderWM.setStereo(kState.StereoType);
            raycastRenderWM.setIPD(kState.IPD);
        }

        // Display Panel:
        displayGUI.setBackgroundColor(kState.BackgroundColor);
        displayGUI.setBoundingBoxColor(kState.BoundingBoxColor);
        displayGUI.setBoundingBox(kState.ShowBoundingBox);
        displayGUI.setShowOrientationCube(kState.ShowOrientationCube);
        displayGUI.setPerspective(kState.Perspective);
        raycastRenderWM.setCameraParameters(kState.Camera);
        raycastRenderWM.setCameraLocation(kState.CameraLocation);
        raycastRenderWM.SetSceneRotation(kState.ObjectRotation);
        raycastRenderWM.setObjectParameters(kState.ObjectLocation);
        displayGUI.displayCameraParams(kState.Camera);
        displayGUI.displayObjectParams(kState.ObjectLocation);

        // Surface Panel:
        surfaceGUI.setSurfaceStates(kState.SurfaceList);
        surfaceGUI.setSelected(kState.SelectedSurface);

        // SurfaceTexture Info:
        surfaceTextureGUI.setSurfacePanel(surfaceGUI);
        surfaceTextureGUI.setEnabled(kState.TextureEnabled);
        surfaceTextureGUI.setTextureOn(kState.TextureOn);
        surfaceTextureGUI.setTextureImage(kState.OtherImageDirectory, kState.OtherImageName);
        surfaceTextureGUI.setTextureImageOn(kState.UseOtherImage);
        surfaceTextureGUI.setSeparateLUT(kState.OtherLUT);
        if (kState.OtherLUT instanceof ModelLUT ) {
            ((ModelLUT)kState.OtherLUT).setTransferFunction(kState.OtherTransfer);
        }
        else if (kState.OtherLUT instanceof ModelRGB ) {
        	((ModelRGB)kState.OtherLUT).setRedFunction(kState.OtherRed);
        	((ModelRGB)kState.OtherLUT).setGreenFunction(kState.OtherGreen);
        	((ModelRGB)kState.OtherLUT).setBlueFunction(kState.OtherBlue);
        }
        surfaceTextureGUI.setTextureLUTOn(kState.UseOtherLUT);

        // Clip Panel:
        clipGUI.setClipEnabled(kState.ClipEnabled);
        clipGUI.setClipDisplayed(kState.ClipDisplayed);
        clipGUI.setClipValues(kState.ClipValues);
        clipGUI.setClipColors(kState.ClipColors);
        raycastRenderWM.setArbitratyClip(kState.ArbitratyEquation);

        // Lights Panel Info:
        m_kLightsPanel.setAllLights(kState.Lights);
        m_kLightsPanel.setSelectedIndex(kState.LightSelected);
        refreshLighting();

        // Window Size Info:
        RestoreTabs(kState);
        resizePanel();

        mainPane.setDividerLocation(kState.MainDividerLocation);
        rightPane.setDividerLocation(kState.PlanesDividerLocation);
        dualPane.setDividerLocation(kState.DualDividerLocation);
        setSize(kState.WindowSize);
        setLocation(kState.WindowX, kState.WindowY);
        setExtendedState(kState.ExtendedState);

        // PlaneRender Info:
        for (int i = 0; i < 3; i++) {
            m_akPlaneRender[i].setZoom(kState.PlaneZoom[i]);
            //m_akPlaneRender[i].setVOICopy(kState.VOIList[i]);
            //m_akPlaneRender[i].setCurrentVOI(kState.CurrentVOI[i]);
        }
        setModified();

        // Sculpt Panel last:
        if (kState.SculptDrawn) {
            sculptGUI.setSculptShape(kState.SculptShape);
            sculptGUI.drawSculptRegion();
            raycastRenderWM.getSculpt().setSculptDrawn(kState.SculptDrawn);
            raycastRenderWM.getSculpt().setSculptImage(kState.SculptImage);
        }

        progressBar.updateValueImmed(100);
        progressBar.dispose();
        

        raycastRenderWM.setVisible(true);
        raycastRenderWM.startAnimator(true);
    }

    private void RestoreTabs(final VolumeRenderState kState) {
        tabbedPane.removeAll();
        tabbedPane.addTab("Positions", null, positionsPanel.getMainPanel());
        for (int i = 0; i < kState.TabbedList.size(); i++) {
            final String name = kState.TabbedList.get(i);
            if (name.equals("LUT")) {
            	insertTab("LUT", frameHistogram.getContainingPanel());
            } else if (name.equals("Renderer")) {
                insertTab("Renderer", rendererGUI.getMainPanel());
            } else if (name.equals("Light")) {
                insertTab("Light", m_kLightsPanel.getMainPanel());
            } else if (name.equals("Surface")) {
                insertTab("Surface", surfaceGUI.getMainPanel());
            } else if (name.equals("Geodesic")) {
                insertTab("Geodesic", geodesicGUI.getMainPanel());
            } else if (name.equals("Sculpt")) {
                insertTab("Sculpt", sculptGUI.getMainPanel());
            } else if (name.equals("Clip")) {
                insertTab("Clip", clipGUI.getMainPanel());
            } else if (name.equals("Opacity")) {
                insertTab("Opacity", m_kVolOpacityPanel.getMainPanel());
            } else if (name.equals("Display")) {
                insertTab("Display", displayGUI.getMainPanel());
            } else if (name.equals("Slices")) {
                insertTab("Slices", sliceGUI.getMainPanel());
            } else if (name.equals("SurfaceTexture")) {
                insertTab("SurfaceTexture", surfaceTextureGUI.getMainPanel());
            } else if (name.equals("BrainSurface")) {
                insertTab("BrainSurface", brainsurfaceFlattenerRender.getMainPanel());
            } else if (name.equals("FlyThroughMove")) {
                insertTab("FlyThroughMove", flythruMoveControl.getMainPanel());
            } else if (name.equals("FlyThrough")) {
                insertTab("FlyThrough", flythruControl.getMainPanel());
            } else if (name.equals("4D")) {
                insertTab("4D", m_kVolume4DGUI.getMainPanel());
            } else if (name.equals("CustomBlend")) {
                insertTab("CustomBlend", customBlendGUI.getMainPanel());
            } else if (name.equals("MultiHistogram")) {
                insertTab("MultiHistogram", multiHistogramGUI.getMainPanel());
            }
        }
    }

    private void SaveState() {
        final String kFile = getVolumeRenderStateFile(true);
        if (kFile == null) {
            return;
        }
        try {
            ObjectOutputStream objstream;
            objstream = new ObjectOutputStream(new FileOutputStream(kFile));
            final VolumeRenderState kState = StoreState();
            objstream.writeObject(kState);
            objstream.close();

        } catch (final FileNotFoundException e) {
            e.printStackTrace();
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }
    
    
    
    private void SaveTabs(final VolumeRenderState kState) {
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            kState.TabbedList.add(tabbedPane.getTitleAt(i));
        }
        final int iSelected = tabbedPane.getSelectedIndex();
        kState.TabbedList.add(tabbedPane.getTitleAt(iSelected));
    }
    private VolumeRenderState StoreState() {
        final VolumeRenderState kState = new VolumeRenderState();
        kState.ImageA = m_kVolumeImageA;
        kState.ImageB = m_kVolumeImageB;
        kState.Blend = getBlendValue();
        // LUT:
        kState.LUTa = m_kVolumeImageA.GetLUT();
        if (kState.LUTa != null) {
            kState.TransferA = new TransferFunction(kState.LUTa.getTransferFunction());
        }
        kState.RGBa = m_kVolumeImageA.GetRGB();
        if (kState.RGBa != null) {
            kState.RedA = new TransferFunction(kState.RGBa.getRedFunction());
            kState.GreenA = new TransferFunction(kState.RGBa.getGreenFunction());
            kState.BlueA = new TransferFunction(kState.RGBa.getBlueFunction());
            kState.RedOnA = kState.RGBa.getROn();
            kState.GreenOnA = kState.RGBa.getGOn();
            kState.BlueOnA = kState.RGBa.getBOn();
        }
        kState.LUTb = m_kVolumeImageB.GetLUT();
        if (kState.LUTb != null) {
            kState.TransferB = new TransferFunction(kState.LUTb.getTransferFunction());
        }
        kState.RGBb = m_kVolumeImageB.GetRGB();
        if (kState.RGBb != null) {
            kState.RedB = new TransferFunction(kState.RGBb.getRedFunction());
            kState.GreenB = new TransferFunction(kState.RGBb.getGreenFunction());
            kState.BlueB = new TransferFunction(kState.RGBb.getBlueFunction());
            kState.RedOnB = kState.RGBb.getROn();
            kState.GreenOnB = kState.RGBb.getGOn();
            kState.BlueOnB = kState.RGBb.getBOn();
        }
        // Opacity:
        kState.OpacityA = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
        kState.OpacityGMOnA = m_kVolOpacityPanel.isGradientMagnitudeOpacityEnabled();
        if (m_kVolOpacityPanel.getCompA_GM() != null) {
            kState.OpacityGMA = m_kVolOpacityPanel.getCompA_GM().getOpacityTransferFunction();
        }
        if (m_kVolOpacityPanel.getCompB() != null) {
            kState.OpacityB = m_kVolOpacityPanel.getCompB().getOpacityTransferFunction();
            kState.OpacityGMOnB = m_kVolOpacityPanel.isGradientMagnitudeOpacityEnabled();
            if (m_kVolOpacityPanel.getCompB_GM() != null) {
                kState.OpacityGMB = m_kVolOpacityPanel.getCompB_GM().getOpacityTransferFunction();
            }
        }
        kState.SelectedTab = m_kVolOpacityPanel.getSelectedTabIndex();

        SaveTabs(kState);
        // Menu state:
        kState.ShowAxes = menuObj.getMenuItem("Show axes").isSelected();
        kState.ShowCrossHairs = menuObj.getMenuItem("Show crosshairs").isSelected();
        kState.ShowVOI = menuObj.getMenuItem("VOI toolbar").isSelected();
        kState.Show4D = menuObj.getMenuItem("4D toolbar").isSelected();
        // Position Panel:
        kState.Radiological = m_kVolumeImageA.GetImage().getRadiologicalView();

        // Slice Panel:
        for (int i = 0; i < 3; i++) {
            kState.Opacity[i] = sliceGUI.getOpacity(i);
            kState.SliceColor[i] = sliceGUI.getColor(i);
            kState.ShowSlice[i] = sliceGUI.getShowSlice(i);
            kState.ShowSliceBox[i] = sliceGUI.getShowBound(i);
        }
        kState.Center = sliceGUI.getCenter();

        // Render Mode Panel:
        kState.DisplayRayCast = rendererGUI.getVolumeCheck().isSelected();
        kState.DisplaySlices = rendererGUI.getSlicesCheck().isSelected();
        kState.DisplaySurface = rendererGUI.getSurfaceCheck().isSelected();
        kState.StereoType = rendererGUI.getStereo();
        if (kState.StereoType != 0) {
        	kState.IPD = raycastRenderWM.getIPD();
        }
        kState.RenderMode = rendererGUI.getRenderMode();
        kState.MultiHistogram = rendererGUI.getMultiHistoEnabled();
        kState.VolumeBlend = rendererGUI.getBlendSliderValue();
        kState.ReleasedSamples = rendererGUI.getReleasedSliderValue();
        kState.RotationSamples = rendererGUI.getMovingSliderValue();
        kState.ExtractionIntensityLevel = rendererGUI.getIntensityLevel();
        // Custom Blend Panel:
        kState.Equation = customBlendGUI.getEquation();
        kState.SourceBlend = customBlendGUI.getSource();
        kState.DestinationBlend = customBlendGUI.getDestination();
        kState.BlendColor = customBlendGUI.getColor();
        kState.CustomAlpha = customBlendGUI.getAlpha();
        // MultiHisto Panel:
        kState.MultiHistoWidgets = multiHistogramGUI.getHistogram().getWidgets();
        kState.WidgetSelected = multiHistogramGUI.getHistogram().getPicked();

        // Display Panel:
        kState.BackgroundColor = displayGUI.getBackgroundColor();
        kState.BoundingBoxColor = displayGUI.getBoundingBoxColor();
        kState.ShowBoundingBox = displayGUI.getBoundingBox();
        kState.ShowOrientationCube = displayGUI.getShowOrientationCube();
        kState.Perspective = displayGUI.getPerspective();
        kState.Camera = getCameraParameters();
        kState.CameraLocation.copy(getCameraLocation());
        kState.ObjectLocation = getObjectParameters();
        kState.ObjectRotation = raycastRenderWM.GetSceneRotation();

        // Surface Panel:
        kState.SurfaceList = surfaceGUI.getSurfaceStates();
        kState.SelectedSurface = surfaceGUI.getSelected();

        // SurfaceTexture Info:
        kState.TextureEnabled = surfaceTextureGUI.getEnabled();
        kState.TextureOn = surfaceTextureGUI.getTextureOn();
        kState.OtherImageName = surfaceTextureGUI.getImageFileName();
        kState.OtherImageDirectory = surfaceTextureGUI.getImageDir();
        kState.UseOtherImage = surfaceTextureGUI.getTextureImageOn();
        kState.OtherLUT = surfaceTextureGUI.getSeparateLUT();
        if (kState.OtherLUT instanceof ModelLUT) {
            kState.OtherTransfer = ((ModelLUT)kState.OtherLUT).getTransferFunction();
        }
        else if (kState.OtherLUT instanceof ModelRGB) {
            kState.OtherRed = ((ModelRGB)kState.OtherLUT).getRedFunction();
            kState.OtherGreen = ((ModelRGB)kState.OtherLUT).getGreenFunction();
            kState.OtherBlue = ((ModelRGB)kState.OtherLUT).getBlueFunction();
        }
        kState.UseOtherLUT = surfaceTextureGUI.getTextureLUTOn();

        // Sculpt Panel:
        kState.SculptShape = sculptGUI.getSculptShape();
        kState.SculptDrawn = raycastRenderWM.getSculpt().IsSculptDrawn();
        kState.SculptImage = raycastRenderWM.getSculpt().getSculptImage();

        // Clip Panel:
        kState.ClipEnabled = clipGUI.getClipEnabled();
        kState.ClipDisplayed = clipGUI.getClipDisplayed();
        kState.ClipValues = clipGUI.getClipValues();
        kState.ClipColors = clipGUI.getClipColors();
        kState.ArbitratyEquation = raycastRenderWM.getArbitratyClip();

        // Lights Panel Info:
        kState.Lights = m_kLightsPanel.copyAllLights();
        kState.LightSelected = m_kLightsPanel.getSelected();

        // Window Options:
        kState.MainDividerLocation = mainPane.getDividerLocation();
        kState.PlanesDividerLocation = rightPane.getDividerLocation();
        kState.DualDividerLocation = dualPane.getDividerLocation();
        kState.WindowSize = getSize();
        kState.WindowX = getX();
        kState.WindowY = getY();
        kState.ExtendedState = getExtendedState();

        // PlaneRender Info:
        for (int i = 0; i < 3; i++) {
            kState.PlaneZoom[i] = m_akPlaneRender[i].getZoom();
            //kState.VOIList[i] = m_akPlaneRender[i].getVOICopy();
            //kState.CurrentVOI[i] = m_akPlaneRender[i].getCurrentVOI();
        }
        return kState;
    }
    protected void buildImageDependentComponents() {

        m_kVolOpacityPanel = new JPanelVolumeOpacity(m_kVolumeImageA.GetImage(), m_kVolumeImageB.GetImage(),
        		m_kVolumeImageA.GetGradientMagnitudeImage(), m_kVolumeImageB.GetGradientMagnitudeImage());
        m_kVolOpacityPanel.addPropertyChangeListener(this);
        
        TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
        m_kVolumeImageA.UpdateImages(kTransfer, 0, null);
        if (m_kVolumeImageB.GetImage() != null) {
            kTransfer = m_kVolOpacityPanel.getCompB().getOpacityTransferFunction();
            m_kVolumeImageB.UpdateImages(kTransfer, 0, null);
        }

        frameHistogram = new JFrameHistogram(this, m_kVolumeImageA.GetImage(), m_kVolumeImageB.GetImage(),
        		m_kVolumeImageA.getLUT(), m_kVolumeImageB.getLUT());
        boolean foundContour = false;
        for (int i = 0; i < getActiveImage().getVOIs().size(); i++)
        {
        	if (getActiveImage().getVOIs().VOIAt(i).getCurveType() == VOI.CONTOUR)
        	{
        		foundContour = true;
        	}
        }
        if (foundContour)
        {
        	frameHistogram.constructDialog(false);
        }
        else
        {
        	frameHistogram.histogramLUT(true, false);
        }

        if (m_kVolumeImageA.GetImage().is4DImage()) {
            if (m_kVolumeImageA.GetImage().getExtents()[3] != 0) {
                m_b4D = true;
                m_kVolume4DGUI = new JPanelVolume4D(this);
                maxPanelWidth = Math.max(m_kVolume4DGUI.getPreferredSize().width, maxPanelWidth);
            }
        }
        menuObj.setMenuItemEnabled("4D toolbar", m_kVolumeImageA.GetImage().is4DImage());
        setTitle(m_kVolumeImageA.GetImage().getImageName());

        gpuPanel.add(raycastRenderWM.GetCanvas(), BorderLayout.CENTER);
        gpuPanel.setVisible(true);

        buildSurfacePanel();
        buildLightPanel();
        buildSculpt();
        buildSurfaceTexturePanel();
        buildLabelPanel();
        buildClipPanel();
        buildSlicePanel();
        buildHistoLUTPanel();
        buildOpacityPanel();
        buildRenderModePanel();
        build3DMousePanel();

        m_kVolumeImageA.GetImage().addImageDisplayListener(this);
        if (m_kVolumeImageB.GetImage() != null) {
            m_kVolumeImageB.GetImage().addImageDisplayListener(this);
        }

        m_bDependentInterfaceInit = true;
        
        if (m_kVolumeImageA.GetImage().isColorImage()) {
            setRGBTA(m_kVolumeImageA.GetRGB());
        }

        if ( (m_kVolumeImageB.GetImage() != null) && m_kVolumeImageB.GetImage().isColorImage()) {
            setRGBTB(m_kVolumeImageB.GetRGB());
        }
        updateImages(true);
    }
    /**
     * Builds menus for the tri-planar view.
     * 
     * @return new menu bar containing menus.
     */
    protected JMenuBar buildMenu() {
        final JSeparator separator = new JSeparator();

        menuObj = new ViewMenuBuilder(this);
        ViewMenuBar menuBarMaker = new ViewMenuBar(menuObj);

        final JMenuBar menuBar = new JMenuBar();
        voiMenu = menuBarMaker.makeVOIMenu();

        menuBar.add(menuObj.makeMenu("File", false, new JComponent[] {separator,
        		// Use VoluemTriPlanarRendererDTI instead? Systems analysis -> DTI -> visualization...
                //menuObj.buildMenuItem("Open DTI Tract file", "DTI", 0, null, false),
                menuObj.buildMenuItem("Open BrainSurface Flattener view", "BrainSurface", 0, null, false),
                menuObj.buildMenuItem("Open Fly Through view", "FlyThru", 0, null, false),
                menuObj.buildMenuItem("Close frame", "CloseFrame", 0, null, false)}));
        menuBar.add(menuObj.makeMenu("Options", false, new JComponent[] {
                menuObj.buildCheckBoxMenuItem("Show axes", "ShowAxes", true),
                menuObj.buildCheckBoxMenuItem("Show crosshairs", "ShowXHairs", true)//,
//                menuObj.buildMenuItem("Open 3D Mouse Options", "Mouse3D", 0, null, false)
                }));
        menuBar.add(menuObj.makeMenu("Toolbars", false, new JMenuItem[] {
                menuObj.buildCheckBoxMenuItem("VOI toolbar", "VOIToolbar", false),
                menuObj.buildCheckBoxMenuItem("4D toolbar", "4DToolbar", false),
        // menuObj.buildCheckBoxMenuItem("RFA toolbar", "RFAToolbar", false)
                menuObj.buildCheckBoxMenuItem("Open 3D Mouse Options", "Mouse3D",false)
                }));

        menuObj.setMenuItemEnabled("RFA toolbar", false);
        menuObj.setMenuItemEnabled("Open BrainSurface Flattener view", false);
        menuObj.setMenuItemEnabled("Open Fly Through view", false);

        return menuBar;
    }
    
    /**
     * The the top one volume view toolbar.
     */
    protected void buildViewToolbar() {
        final Border etchedBorder = BorderFactory.createEtchedBorder();
        toolbarBuilder = new ViewToolBarBuilder(this);
        viewToolBar = new JToolBar();
        viewToolBar.setBorder(etchedBorder);
        viewToolBar.setBorderPainted(true);
        viewToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        viewToolBar.setLayout(new GridBagLayout());
        viewToolBar.setFloatable(false);

        
        viewToolBar.add(toolbarBuilder.buildButton("Home", "Rollback to center", "homeicon"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        
        viewToolBar.add(toolbarBuilder.buildButton("ResetX", "Reset X Axis", "xalign"));
        viewToolBar.add(toolbarBuilder.buildButton("ResetY", "Reset Y Axis", "yalign"));
        viewToolBar.add(toolbarBuilder.buildButton("ResetZ", "Reset Z Axis", "zalign"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        viewToolBar.add(toolbarBuilder.buildButton("HistoLUT", "Histogram Lookup Table", "histolut"));
        viewToolBar.add(toolbarBuilder.buildButton("OpacityHistogram", "Opacity histogram", "histogram"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        viewToolBar.add(toolbarBuilder.buildButton("Slices", "Slice render", "triplanar"));
        viewToolBar.add(toolbarBuilder.buildButton("Opacity", "Surface volume renderer", "renderer"));
        viewToolBar.add(toolbarBuilder.buildButton("Renderer", "Renderer mode control", "control"));

        viewToolBar.add(ViewToolBarBuilder.makeSeparator());

        viewToolBar.add(toolbarBuilder.buildButton("SurfaceDialog", "Add surface to viewer", "isosurface"));
        viewToolBar.add(toolbarBuilder.buildButton("Geodesic", "Draw geodesic curves on the surface", "geodesic"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        viewToolBar.add(toolbarBuilder.buildButton("Box", "Display options", "perspective"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());

        viewToolBar.add(toolbarBuilder.buildButton("Sculpt", "Sculpt and Remove Volume Region", "sculpt"));
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

        m_kRecordToggle = toolbarBuilder.buildToggleButton("Record", "Record to Avi", "movie");
        viewToolBar.add(m_kRecordToggle);

        m_kSaveButton = toolbarBuilder.buildButton("SaveState", "Save State", "save");
        m_kLoadButton = toolbarBuilder.buildButton("LoadState", "Load State", "open");
        viewToolBar.add(m_kSaveButton);
        viewToolBar.add(m_kLoadButton);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 35;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 1;
        viewToolBar.add(VolumeTriPlanarInterface.getRendererProgressBar(), gbc);

        panelToolBarGBC = new GridBagConstraints();
        panelToolBarGBC.gridx = 0;
        panelToolBarGBC.gridy = 0;
        panelToolBarGBC.gridwidth = 1;
        panelToolBarGBC.gridheight = 1;
        panelToolBarGBC.fill = GridBagConstraints.BOTH;
        panelToolBarGBC.anchor = GridBagConstraints.WEST;
        panelToolBarGBC.weightx = 1;
        panelToolBarGBC.weighty = 1;
        panelToolbar.add(viewToolBar, panelToolBarGBC);
        panelToolBarGBC.gridy++;
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

        menuBar = buildMenu();
        setJMenuBar(menuBar);

        panelToolbar.setLayout(new GridBagLayout());
        panelToolbar.setVisible(true);
        getContentPane().add(panelToolbar, BorderLayout.NORTH);
        buildViewToolbar();

        buildImageIndependentComponents();

        setResizable(true);
        addComponentListener(this);

        final Border raisedbevel = BorderFactory.createRaisedBevelBorder();
        final Border loweredbevel = BorderFactory.createLoweredBevelBorder();
        final Border compound = BorderFactory.createCompoundBorder(raisedbevel, loweredbevel);

        triImagePanel = new JPanel();
        triImagePanel.setLayout(new GridLayout(1, 3, 10, 10));
        triImagePanel.setBorder(raisedbevel);

        final int triImagePanelWidth = (int) (screenWidth * 0.51f);
        final int triImagePanelHeight = (int) (screenHeight * 0.25f);

        triImagePanel.setPreferredSize(new Dimension(triImagePanelWidth, triImagePanelHeight));
        triImagePanel.setMinimumSize(new Dimension(150, 50));

        panelAxial = new JPanel(new BorderLayout());
        panelSagittal = new JPanel(new BorderLayout());
        panelCoronal = new JPanel(new BorderLayout());
        triImagePanel.add(panelAxial);
        triImagePanel.add(panelSagittal);
        triImagePanel.add(panelCoronal);

        final GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.ipadx = 5;
        gbc2.insets = new Insets(0, 5, 0, 5);

        gpuPanel = new JPanel(new BorderLayout());
        setLocation(100, 100);

        final int imagePanelWidth = (int) (screenWidth * 0.51f);
        final int imagePanelHeight = (int) (screenHeight * 0.43f);

        gpuPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        gpuPanel.setMinimumSize(new Dimension(250, 250));

        bf_flyPanel = new JPanel(new BorderLayout());
        bf_flyPanel.setBorder(compound);
        bf_flyPanel.setMinimumSize(new Dimension(0, 250));
        
        dualPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, gpuPanel, bf_flyPanel);
        dualPane.setOneTouchExpandable(true);
        dualPane.setDividerSize(6);
        dualPane.setContinuousLayout(true);
        dualPane.setResizeWeight(1);

        rightPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, dualPane, triImagePanel);
        rightPane.setOneTouchExpandable(true);
        rightPane.setDividerSize(6);
        rightPane.setContinuousLayout(true);
        rightPane.setResizeWeight(1);

        tabbedPane.setPreferredSize(new Dimension(maxPanelWidth, tabbedPane.getPreferredSize().height));

        final JPanel tabPanel = new JPanel(new BorderLayout());

        tabPanel.add(tabbedPane);
        tabPanel.setMinimumSize(new Dimension(maxPanelWidth, 820));

        mainPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, tabPanel, rightPane);

        mainPane.setOneTouchExpandable(true);
        mainPane.setDividerSize(6);
        mainPane.setContinuousLayout(true);
   
        

        getContentPane().add(mainPane, BorderLayout.CENTER);
        

        prevHeight = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height
                - panelToolbar.getHeight();
        //System.err.println( prevHeight );
    }

    /**
     * Construct the volume rendering methods based on the choices made from the resample dialog. This method is called
     * by the Resample dialog.
     */
    protected void constructRenderers(final ViewJProgressBar progressBar) {

    	initShared();
    	
        final int iStep = (progressBar != null) ? (100 - progressBar.getValue()) / 5 : 0;
        if (progressBar != null) {
            progressBar.setMessage("Creating Slice Views...");
        }
        
        m_akPlaneRender = new PlaneRender_WM[3];

        if (progressBar != null) {
            progressBar.updateValueImmed(progressBar.getValue() + iStep);
        }
        if (progressBar != null) {
            progressBar.setMessage("Creating Volume & Surface Renderer...");
        }

        raycastRenderWM = new VolumeTriPlanarRender( sharedRenderer, new GLCanvas(caps, sharedDrawable.getContext()), this, m_kVolumeImageA, m_kVolumeImageB);
        
        if (progressBar != null) {
            progressBar.updateValueImmed(progressBar.getValue() + iStep);
        }
        if (progressBar != null) {
            progressBar.setMessage("Building interface...");
        }

        buildImageDependentComponents();

        if (progressBar != null) {
            progressBar.updateValueImmed(progressBar.getValue() + iStep);
        }
        if (progressBar != null) {
            progressBar.setMessage("Display...");
        }

        pack();
        setSize( 1331, 925 );
        setVisible(true);
    }

    protected void initShared() {
    	if ( sharedDrawable == null )
    	{
            sharedDrawable = GLDrawableFactory.getFactory(glp).createOffscreenAutoDrawable(null, caps, null, gl_width, gl_height, null);
            sharedRenderer = new VolumeTriPlanarRender(this, null, m_kVolumeImageA, m_kVolumeImageB);
    		sharedDrawable.addGLEventListener(sharedRenderer);
    		// init and render one frame, which will setup the shared textures
    		sharedDrawable.display();
    		
    		// need to check hardware capabilities...
            if ( Preferences.OperatingSystem.getOS() != OperatingSystem.OS_MAC )
            {
            	caps.setStereo(true);
            }
    	}
    	else
    	{
    		sharedRenderer.reInitialize(m_kVolumeImageA, m_kVolumeImageB);
        	sharedDrawable.display();  
    	}
    }

    protected void releaseShared() {
        sharedDrawable.destroy();
        sharedDrawable = null;
    }
        
    int prevHeight;
    /**
     * Method that resizes the frame and adjusts the rows, columns as needed.
     */
    protected void resizePanel() {

    	
        final int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height
                - panelToolbar.getHeight();
    	if ( prevHeight == height )
    	{
    		return;
    	}
    	prevHeight = height;
    	//System.err.println( getWidth() + " " + getHeight() + " " + prevHeight );

        if (m_bDependentInterfaceInit) {
            if (frameHistogram != null) {
                frameHistogram.getContainingPanel().setPreferredSize(new Dimension(maxPanelWidth, height));
            }
            surfaceGUI.resizePanel(maxPanelWidth, height);
            m_kLightsPanel.resizePanel(maxPanelWidth, height);
            positionsPanel.resizePanel(maxPanelWidth, height);
            sliceGUI.resizePanel(maxPanelWidth, height);
            clipGUI.resizePanel(maxPanelWidth, height);
            rendererGUI.resizePanel(maxPanelWidth, height);
            if (multiHistogramGUI != null) {
                multiHistogramGUI.resizePanel(maxPanelWidth, height);
            }
            if (brainsurfaceFlattenerRender != null) {
                brainsurfaceFlattenerRender.resizePanel(maxPanelWidth, height);
                // dualPane.setDividerLocation( 0.5f );
            }
            if (m_kFlyThroughRender != null) {
                flythruControl.resizePanel(maxPanelWidth, height);
                // dualPane.setDividerLocation( 0.5f );
            }
            if (m_kVolume4DGUI != null) {
                m_kVolume4DGUI.resizePanel(maxPanelWidth, height);
            }
        }

        displayGUI.resizePanel(maxPanelWidth, height);
        geodesicGUI.resizePanel(maxPanelWidth, height);
        // rightPane.setDividerLocation( 0.618f );
        // updatePlanes();
    }

	@Override
	public void propertyChange(PropertyChangeEvent event)
	{
		String propertyName = event.getPropertyName();
		if ( propertyName.equals("Opacity") )
		{
			boolean gmChanged = raycastRenderWM.getGradientMagnitude() != m_kVolOpacityPanel.isGradientMagnitudeOpacityEnabled();
	        raycastRenderWM.setGradientMagnitude(m_kVolOpacityPanel.isGradientMagnitudeOpacityEnabled());
	        final ViewJComponentVolOpacityBase kSelectedComp = m_kVolOpacityPanel.getSelectedComponent();
	        
	        if ( kSelectedComp == m_kVolOpacityPanel.getCompA() )
	        {
	        	final TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
	        	m_kVolumeImageA.UpdateImages(kTransfer, 0, null);
	        } 
	        if ( gmChanged || (kSelectedComp == m_kVolOpacityPanel.getCompA_GM()) )
	        {
	        	final TransferFunction kTransfer = m_kVolOpacityPanel.getCompA_GM().getOpacityTransferFunction();
	        	m_kVolumeImageA.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagA());
	        }
	        if ( kSelectedComp == m_kVolOpacityPanel.getCompB() )
	        {
	        	final TransferFunction kTransfer = m_kVolOpacityPanel.getCompB().getOpacityTransferFunction();
	        	m_kVolumeImageB.UpdateImages(kTransfer, 0, null);
	        } 
	        if ( (m_kVolumeImageB.GetImage() != null) && (gmChanged || (kSelectedComp == m_kVolOpacityPanel.getCompB_GM())) )
	        {
	        	if ( m_kVolOpacityPanel.getCompB_GM() != null )
	        	{
	        		final TransferFunction kTransfer = m_kVolOpacityPanel.getCompB_GM().getOpacityTransferFunction();
	        		m_kVolumeImageB.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagB());
	        	}
	        }
	        updateABBlend( );
		}

	}

	    

	public void setCameraNearPlane( float distance )
	{
		if ( raycastRenderWM != null )
		{
			raycastRenderWM.setCameraNearPlane(distance);
		}
	}
	
	public void setPlaneConstant( float distance )
	{
		if ( raycastRenderWM != null )
		{
			raycastRenderWM.setPlaneConstant(distance);
		}
	}
}
