package gov.nih.mipav.view.renderer.WildMagic;


import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmConcat;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewControlsImage;
import gov.nih.mipav.view.ViewJComponentBase;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.JPanelHistoLUT;
import gov.nih.mipav.view.renderer.JPanelHistoRGB;
import gov.nih.mipav.view.renderer.JPanelRendererBase;
import gov.nih.mipav.view.renderer.JPanelVolOpacityRGB;
import gov.nih.mipav.view.renderer.ViewJComponentVolOpacityBase;
import gov.nih.mipav.view.renderer.J3D.JPanelVolOpacity;
import gov.nih.mipav.view.renderer.J3D.JPanelVolOpacityBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JDialogDTIInput;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JDialogStereoControls;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelClip_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelCustumBlend;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelDisplay_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelGeodesic_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelLights_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelMultiDimensionalTransfer;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelRenderMode_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSculptor_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSlices_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurfaceTexture_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurface_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceExtractorCubes;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeNode;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSlices;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
import gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.CorticalAnalysisRender;
import gov.nih.mipav.view.renderer.WildMagic.flythroughview.FlyThroughRender;
import gov.nih.mipav.view.renderer.WildMagic.flythroughview.JPanelVirtualEndoscopySetup_WM;
import gov.nih.mipav.view.renderer.flythroughview.JPanelFlythruMove;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowEvent;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.border.Border;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Meshes.VETMesh;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.Shaders.CompiledProgramCatalog;
import WildMagic.LibGraphics.Shaders.ImageCatalog;
import WildMagic.LibGraphics.Shaders.PixelProgramCatalog;
import WildMagic.LibGraphics.Shaders.VertexProgramCatalog;

import com.sun.opengl.util.Animator;

public class VolumeTriPlanarInterface extends ViewJFrameBase {

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
         * @param  _name   panel name.
         * @param  _panel  JPanel to display in tab.
         */
        public TabbedItem(String _name, JPanel _panel) {
            name = _name;
            panel = _panel;
        }
    }

    public class IntVector extends Vector<Integer>
    {
        /**  */
        private static final long serialVersionUID = -7551972247476811252L;
        public IntVector() {
            super();
        }
        public IntVector(int initialsize) {
            super(initialsize);
        }
    }

    /** Use serialVersionUID for interoperability. */
    protected static final long serialVersionUID = 1898957906984534260L;

    /** The small bar on the top right corner the volume view frame. */
    protected static JProgressBar rendererProgressBar;

    /** Menu items storage. */
    protected ViewMenuBuilder menuObj;

    /** Labels for the current position in 3D ModelView coordinates. */
    protected JLabel modelViewLabel = null;

    /** Displayed values for the current position in 3D ModelView coordinates. */
    protected JLabel[] modelViewLabelVals = new JLabel[3];

    /** Panel that holds the toolbars. */
    protected JPanel panelToolbar = new JPanel();

    /** Labels for the current position in PatientSlice coordinates. */
    protected JLabel patientSliceLabel = null;

    /** Displayed values for the current position in PatientSlice coordinates. */
    protected JLabel[] patientSliceLabelVals = new JLabel[3];

    /** Lookup table of the color imageA, B. */
    protected ModelRGB RGBTA = null, RGBTB = null;

    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;
    /** Control panels for the Brain Surface Flattener:. */
    protected JPanel m_kBrainsurfaceFlattenerPanel = null;
    /** Rendering the brainsurfaceFlattener objects. */
    protected CorticalAnalysisRender brainsurfaceFlattenerRender = null;
    /** Flythrough renderer: */
    protected FlyThroughRender m_kFlyThroughRender =  null;
    /** Flythough setup user-interface container: */
    protected JPanel m_kFlyThroughPanel = null;
    /** Flythough setup panel: */
    protected JPanelVirtualEndoscopySetup_WM flythruControl;
    /** Flythough Move container: */
    protected JPanel flythruMovePanel; 
    /** Flythough Move panel: */
    protected JPanelFlythruMove flythruMoveControl;
    /** Clipping user-interface panel: */
    protected JPanelClip_WM clipBox;
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
    /** Clip user-interface container:  */
    protected JPanel clipPanel;
    /** 3D Slice panel container. */
    protected JPanel slicePanel;
    /** Multihistogram panel: */
    protected JPanelMultiDimensionalTransfer multiHistogramGUI;
    /** Display panel container */
    protected JPanel multiHistogramPanel;
    /** Multihistogram panel: */
    protected JPanelCustumBlend custumBlendGUI;

    /** Display panel container */
    protected JPanel custumBlendPanel;
    /** Surface texture panel container */
    protected JPanel surfaceTexturePanel;
    /** Render mode panel container */
    protected JPanel renderModePanel;
    /** Surface user-interface panel container */
    protected JPanel surfacePanel;
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

    /** Display panel container */
    protected JPanel displayPanel;

    /** Control panel for the surface renderer. */
    protected JPanel histoLUTPanel;

    /** Image orientation: coronal, sagittal, axial, unknown. */
    protected int imageOrientation;

    /** The image panel to hold one Canvas3D. */
    protected JPanel gpuPanel;

    /** Panel to hold the BrainSurfaceFlattener or Flythrough views.*/
    protected JPanel bf_flyPanel;

    /** Light panel container */
    protected JPanel lightPanel;

    /** Light panel */
    protected JPanelLights_WM m_kLightsPanel;
    /** The three slice views displayed as texture-mapped polygons:. */
    protected PlaneRender_WM[] m_akPlaneRender;

    /** Control panel for drawing geodesic curves. */
    protected JPanel m_kGeodesicPanel;

    /** Control panel for volume sculpting. */
    protected JPanel m_kSculptPanel;

    /** The max width of the control panels. */
    protected int maxPanelWidth = -1;

    /** Menu bar. */
    protected JMenuBar menuBar;

    /** Volume Opacity panel. */
    protected JPanel opacityPanel = null;

    /** Padding imageA with blank images feeding. */
    protected ModelImage paddingImageA;

    /** Padding imageB with blank images feeding. */
    protected ModelImage paddingImageB;

    /** LUT control panel of the gray scale image. */
    protected JPanelHistoLUT panelHistoLUT;

    /** RGB control panel of the color image. */
    protected JPanelHistoRGB panelHistoRGB;

    /** Radio button of the surface render lighting mode. */
    protected JRadioButton radioSurrenderLIGHT;

    /** Panel Border view. */
    protected Border raisedbevel, loweredbevel, compound, redBorder, etchedBorder, pressedBorder;

    /** VolumeImage contains data and textures for ModelImage A. */
    protected VolumeImage m_kVolumeImageA;
    /** VolumeImage contains data and textures for ModelImage B. */
    protected VolumeImage m_kVolumeImageB;
    /** Animator for GPU-based rendering with JOGL. */
    protected Animator m_kAnimator;
    /** Volume/Slice/Surface renderer. */
    protected VolumeTriPlanarRender raycastRenderWM;

    /** Button for RFA. */
    protected JButton rfaButton;

    /** RFA separator. */
    protected JButton rfaSeparator;

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
    protected JPanelVolOpacityBase m_kVolOpacityPanel;

    /** Stereo view IPD control. */
    protected JDialogStereoControls m_kStereoIPD = null;
    
    /** Axial view panel. */
    protected JPanel panelAxial;
    /** Sagittal view panel. */
    protected JPanel panelSagittal;
    /** Coronal view panel. */
    protected JPanel panelCoronal;
    /** Current frame width and height. */
    protected int screenWidth, screenHeight;
    protected int m_iVOICount = 0;

    protected int m_iVOITotal = 0;
    protected String m_kVOIName = "";
    protected Vector<String> m_kVOINameList = new Vector<String>();
    /** Panel containing the position labels:. */
    private JPanel panelLabels = new JPanel();
    private JToolBar m_kVOIToolbar;
    private IntVector[] m_kVOIImage = null;
    
    
    /**
     * Specific constructor call from the VolumeViewerDTI.   
     */
    public VolumeTriPlanarInterface()
    {
    	super(null, null);
    }

    /**
     * Make a volume rendering frame, which contains the toolbars on the top,
     * control panel on the left, the volume rendering panel on the right, and
     * the three orthogonal view ( axial, sagittal, coronal, views) on the
     * bottom right.
     *
     * @param  _imageA First image to display
     * @param LUTa  LUT of the imageA (if null grayscale LUT is constructed)
     * @param  _RGBTA  RGB table of imageA
     * @param  _imageB Second loaded image
     * @param  LUTb LUT of the imageB
     * @param  _RGBTB RGB table of imageB
     */
    public VolumeTriPlanarInterface(ModelImage _imageA, ModelLUT LUTa, ModelRGB _RGBTA,
                                    ModelImage _imageB, ModelLUT LUTb, ModelRGB _RGBTB)
    {
        super(_imageA, _imageB);
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


    /**
     * Get the default Shader directory.
     * @return string containing the default Shader directory.
     */
    static public String getExternalDirs()
    {
        String jar_filename = "";
        String class_path_key = "java.class.path";
        String class_path = System.getProperty(class_path_key);
        for (String fn : class_path.split(":")) {
            if (fn.contains("WildMagic.jar")) {
                jar_filename = fn;
                String externalDirs = jar_filename.substring(0, jar_filename.indexOf("lib"));
                externalDirs = externalDirs.concat("WildMagic");
                //System.err.println("Shader dir found: " + externalDirs);
                return externalDirs;
            }
        }
        System.err.println("Shader dir not found");
        return System.getProperties().getProperty("user.dir");
    }

    /**
     * Retrieve the progress bar used in the volume renderer (the one in the
     * upper right hand corner).
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
     * Creates and initializes the LUT for an image.
     * @param   img  the image to create a LUT for
     * @return  a LUT for the image <code>img</code> (null if a color image)
     * @throws  OutOfMemoryError  if enough memory cannot be allocated for this method
     */
    public static ModelLUT initLUT(ModelImage img) throws OutOfMemoryError {
        ModelLUT newLUT = null;

        // only make a lut for non color images
        if (img.isColorImage() == false) {
            int[] dimExtentsLUT = new int[2];

            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            newLUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

            float min, max;

            if (img.getType() == ModelStorageBase.UBYTE) {
                min = 0;
                max = 255;
            } else if (img.getType() == ModelStorageBase.BYTE) {
                min = -128;
                max = 127;
            } else {
                min = (float) img.getMin();
                max = (float) img.getMax();
            }

            float imgMin = (float) img.getMin();
            float imgMax = (float) img.getMax();

            newLUT.resetTransferLine(min, imgMin, max, imgMax);
        }

        return newLUT;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        
        if (command.equals("Extract")) {
            raycastRenderWM.updateImageFromRotation();
        } else if (command.equals("ExtractMeshFromVolume")) {
            raycastRenderWM.extractMeshFromVolume();
        } else if (command.equals("HistoLUT")) {
            insertTab("LUT", histoLUTPanel);
        } else if (command.equals("VolRender")) {
        } else if (command.equals("Geodesic")) {
            insertTab("Geodesic", m_kGeodesicPanel);
        } else if (command.equals("Sculpt")) {
            insertTab("Sculpt", m_kSculptPanel);
            sculptGUI.getMainPanel().setVisible(true);
        } else if (command.equals("Clipping")) {
            clipBox.getMainPanel().setVisible(true);
            insertTab("Clip", clipPanel);

            setSize(getSize().width, getSize().height - 1);
            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                panelToolbar.getHeight();
            clipBox.resizePanel(maxPanelWidth, height);
        } else if (command.equals("OpacityHistogram")) {
            //insertTab("MultiHistogram", multiHistogramPanel);
            //multiHistogramGUI.getMainPanel().setVisible(true);
            insertTab("Opacity", opacityPanel);
        } else if (command.equals("Opacity")) {
            clipBox.getMainPanel().setVisible(true);
            clipButton.setEnabled(true);
            clipPlaneButton.setEnabled(true);
            clipDisableButton.setEnabled(true);
            clipMaskButton.setEnabled(true);
            clipMaskUndoButton.setEnabled(true);
            clipSaveButton.setEnabled(true);
            insertTab("Opacity", opacityPanel);
            raycastRenderWM.displayVolumeRaycast( true );
            rendererGUI.setDisplayVolumeCheck(true);
            rendererGUI.setDisplaySlicesCheck(false);
            raycastRenderWM.displayVolumeSlices( rendererGUI.getSlicesCheck().isSelected() );
            raycastRenderWM.setVolumeBlend( rendererGUI.getBlendSliderValue()/100.0f );
        } else if ( command.equals( "VolumeRayCast") ) {
            clipBox.getMainPanel().setVisible(true);
            clipButton.setEnabled(true);
            clipPlaneButton.setEnabled(true);
            clipDisableButton.setEnabled(true);
            clipMaskButton.setEnabled(true);
            clipMaskUndoButton.setEnabled(true);
            clipSaveButton.setEnabled(true);
            raycastRenderWM.displayVolumeRaycast( rendererGUI.getVolumeCheck().isSelected() );
            raycastRenderWM.setVolumeBlend( rendererGUI.getBlendSliderValue()/100.0f );
        } else if (command.equals("Stereo")) {
            if ( (m_kStereoIPD == null) && rendererGUI.getStereoCheck().isSelected() )
            {
                m_kStereoIPD = new JDialogStereoControls( this, .02f );
            }
            else if ( (m_kStereoIPD != null) && ! rendererGUI.getStereoCheck().isSelected() )
            {
                m_kStereoIPD.close();
                m_kStereoIPD = null;
            }
            raycastRenderWM.setStereo( rendererGUI.getStereoCheck().isSelected() );
        } else if (command.equals("ChangeLight")) {
            insertTab("Light", lightPanel);
        } else if (command.equals("Box")) {
            insertTab("Display", displayPanel);
        } else if (command.equals("InvokeClipping")) {
            clipBox.getMainPanel().setVisible(true);
            clipBox.invokeClippingPlanes();
            insertTab("Clip", clipPanel);

            setSize(getSize().width, getSize().height - 1);
            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                panelToolbar.getHeight();
            clipBox.resizePanel(maxPanelWidth, height);

            insertTab("Clip", clipPanel);
        } else if (command.equals("DisableClipping")) {
            clipBox.getMainPanel().setVisible(true);
            clipBox.disable6Planes();
            insertTab("Clip", clipPanel);
        } else if (command.equals("CropClipVolume")) {
            raycastRenderWM.cropClipVolume();
            setModified();
        } else if (command.equals("UndoCropVolume")) {
            updateData();
        } else if (command.equals("SaveCropVolume")) {
            raycastRenderWM.saveImageFromTexture();
        } else if (command.equals("Slices")) {
            sliceGUI.getMainPanel().setVisible(true);
            insertTab("Slices", slicePanel);
            raycastRenderWM.displayVolumeRaycast( false );
            rendererGUI.setDisplayVolumeCheck(false);
            rendererGUI.setDisplaySlicesCheck(true);
            raycastRenderWM.displayVolumeSlices( rendererGUI.getSlicesCheck().isSelected() );
        } else if (command.equals("VolumeSlices")) {
            sliceGUI.getMainPanel().setVisible(true);
            raycastRenderWM.displayVolumeSlices( rendererGUI.getSlicesCheck().isSelected() );
        } else if (command.equals("Surface")) {
            raycastRenderWM.displaySurface( rendererGUI.getSurfaceCheck().isSelected() );
            
            
            if (m_akPlaneRender != null)
            {
                for (int i = 0; i < 3; i++) {
                    
                    if (m_akPlaneRender[i] != null) {
                        m_akPlaneRender[i].displaySurface(rendererGUI.getSurfaceCheck().isSelected());
                    }
                }
            }
            
        } else if (command.equals("SurfaceDialog")) {
            insertTab("Surface", surfacePanel);
            surfaceGUI.getMainPanel().setVisible(true);
            setSize(getSize().width, getSize().height - 1);
        } else if (command.equals("SurfaceTexture")) {
            insertTab("SurfaceTexture", surfaceTexturePanel);
            surfaceTextureGUI.getMainPanel().setVisible(true);
            surfaceTextureGUI.setSurfacePanel(surfaceGUI);
            if ( surfaceGUI.surfaceAdded() )
            {
                surfaceTextureGUI.setEnabled(true);
            }
        } else if (command.equals("DTI")) {
            JDialogDTIInput kDTIIn = new JDialogDTIInput( JDialogDTIInput.TRACTS_PANEL,
                                                          this, imageA);
            insertTab("DTI", kDTIIn.getMainPanel() );
            kDTIIn.getMainPanel().setVisible(true);
        } else if (command.equals("BrainSurface")) {
            if ( m_kBrainsurfaceFlattenerPanel == null )
            {
                brainsurfaceFlattenerRender = 
                    new gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.CorticalAnalysisRender(this, 
                                                                                                           m_kAnimator, m_kVolumeImageA, m_kVolumeImageB);
                TriMesh kSurface = raycastRenderWM.getSurface( surfaceGUI.getSelectedSurface() );
                Node kMeshLines = brainsurfaceFlattenerRender.getPanel().displayCorticalAnalysis(kSurface, raycastRenderWM.getSurfaceCenter( surfaceGUI.getSelectedSurface()));       
                if ( kMeshLines != null )
                {
                    m_kBrainsurfaceFlattenerPanel = new JPanel();
                    m_kBrainsurfaceFlattenerPanel.add(brainsurfaceFlattenerRender.getMainPanel());
                    maxPanelWidth = Math.max(m_kBrainsurfaceFlattenerPanel.getPreferredSize().width, maxPanelWidth);
                    bf_flyPanel.add( brainsurfaceFlattenerRender.GetCanvas(), BorderLayout.CENTER );
                    dualPane.setDividerLocation( 0.5f );
                    m_kLightsPanel.enableLight(0, true);
                    addNode( kMeshLines );
                }
                else
                {
                    MipavUtil.displayError(surfaceGUI.getSelectedSurface() + " is not a closed mesh. Unable to open brain flattener view.");
                }
            }
            insertTab("BrainSurface", m_kBrainsurfaceFlattenerPanel );
            resizePanel();
        } else if (command.equals("FlyThru")) {
            if ( m_kFlyThroughPanel == null )
            {
                m_kFlyThroughRender = new FlyThroughRender( this, 
                                                            m_kAnimator, m_kVolumeImageA,
                                                            m_kVolumeImageB, raycastRenderWM.getTranslate());
                TriMesh kSurface = raycastRenderWM.getSurface( surfaceGUI.getSelectedSurface() );
                m_kFlyThroughRender.addSurface(kSurface, raycastRenderWM.getSurfaceCenter( surfaceGUI.getSelectedSurface()) );
                bf_flyPanel.add( m_kFlyThroughRender.GetCanvas(), BorderLayout.CENTER );
                dualPane.setDividerLocation( 0.5f ); 
                m_kLightsPanel.enableLight(0, true);
                buildFlythroughPanel();
            }
            insertTab("FlyThroughMove", flythruMovePanel);
            insertTab("FlyThrough", m_kFlyThroughPanel );
            resizePanel();
        } else if (command.equals("Renderer")) {
        	insertTab("Renderer", renderModePanel );
        	resizePanel();
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
            for (int iPlane = 0; iPlane < 3; iPlane++) {
                m_akPlaneRender[iPlane].showAxes(showAxes);
                m_akPlaneRender[iPlane].SetModified(true);
            }
        } else if (command.equals("ShowXHairs")) {
            boolean showXHairs = menuObj.isMenuItemSelected("Show crosshairs");
            for (int iPlane = 0; iPlane < 3; iPlane++) {
                m_akPlaneRender[iPlane].showXHairs(showXHairs);
                m_akPlaneRender[iPlane].SetModified(true);
            }
        } else if (command.equals("RFAToolbar")) {
            boolean showRFA = menuObj.isMenuItemSelected("RFA toolbar");

            setRFAToolbarVisible(showRFA);
        } else if (command.equals("RadiologicalView")) {          
            setRadiological(true);
        } else if (command.equals("NeurologicalView")) {
            setRadiological(false);
        } else if (command.equals("ShaderParameters") ) {
            raycastRenderWM.displayShaderParameters();
        } else if (command.equals("VOIToolbar") ) {
            boolean showVOI = menuObj.isMenuItemSelected("VOI toolbar");
            m_kVOIToolbar.setVisible(showVOI);
        } else if (command.equals("RectVOI") ) {
            doVOI(command);
        } else if (command.equals("EllipseVOI") ) {
            doVOI(command);
        } else if (command.equals("Polyline") ) {
            doVOI(command);
        }  else if (command.equals("VOIColor") ) {
            doVOI(command);
        } else if (command.equals("LevelSetVOI") ) {
            doVOI(command);
        } else if (command.equals("deleteAllVOI") ) {
            doVOI(command);
        } else if (command.equals("deleteVOI") ) {
            doVOI(command);
        }  else if (command.equals("cutVOI") ) {
            doVOI(command);
        } else if (command.equals("copyVOI") ) {
            doVOI(command);
        } else if (command.equals("pasteVOI") ) {
            doVOI(command);
        } else if (command.equals("PropVOIUp") ) {
            doVOI(command);
        } else if (command.equals("PropVOIDown") ) {
            doVOI(command);
        } else if (command.equals("PropVOIAll") ) {
            doVOI(command);
        } else if (command.equals("Pointer") ) {
            doVOI(command);
        } else if (command.equals("Default") ) {
            doVOI(command);
        } else if (command.equals("3DVOIIntersect") ) {
            create3DVOI(true);
        } else if (command.equals("3DVOIUnion") ) {
            create3DVOI(false);
        }

    }
  
    /**
     * Add a geodesic element to the surface display.
     * @param kSurface the surface the geodesic element is attached to.
     * @param kNew the new geodesic element.
     * @param iGroup the Node index.
     */
    public void addGeodesic( TriMesh kSurface, Geometry kNew, int iGroup )
    {
        raycastRenderWM.addGeodesic(kSurface, kNew, iGroup);
    }
    
    /**
     * Add a new display node to the volume/surface display list. This is done
     * from the other renderers: BrainSurfaceFlattener and Flythrough.
     * @param kNode
     */
    public VolumeObject addNode( Node kNode )
    {
        return raycastRenderWM.AddNode( kNode );
    }

    /**
     * Add a polyline to the VolumeDTI display.
     * @param akPolyline new polyline.
     * @param groupIndex Node index.
     */
    public void addPolyline(Polyline akPolyline, int groupIndex)
    {
        raycastRenderWM.addPolyline(akPolyline, groupIndex);
    }

    /**
     * Pass the VolumeSlices from the Volume Renderer to the PlaneRender objects.
     * @param kSlices
     */
    public void addSlices(VolumeSlices kSlices)
    {
        for (int i = 0; i < 3; i++) 
        {
            if ( m_akPlaneRender[i] != null )
            {
                m_akPlaneRender[i].addSlices(kSlices);
            }
        }
    }

    /**
     * Add TriMesh surfaces to the Volume Renderer.
     * @param akSurfaces new surfaces.
     */
    public void addSurface(TriMesh[] akSurfaces)
    {
        raycastRenderWM.addSurface(akSurfaces);  
        insertTab("Light", lightPanel);
        m_kLightsPanel.enableLight(0, true);
        insertTab("Surface", surfacePanel);
        rendererGUI.setDisplaySurfaceCheck(true);
        raycastRenderWM.displaySurface(true);
        
        if (m_akPlaneRender != null)
        {
            for (int i = 0; i < 3; i++) {
                
                if (m_akPlaneRender[i] != null) {
                    m_akPlaneRender[i].displaySurface(true);
                }
            }
        }
        
        menuObj.setMenuItemEnabled("Open BrainSurface Flattener view", true);  
        menuObj.setMenuItemEnabled("Open Fly Through view", true);       

        if ( geodesicGUI != null )
        {
            geodesicGUI.setEnabled(true);
            geodesicGUI.setSurfacePanel(surfaceGUI);
        }
    }
    
    /**
     * Build the clipping control panel for the surface render.
     */
    public void buildClipPanel() {
        clipPanel = new JPanel();
        clipBox = new JPanelClip_WM(this);
        clipPanel.add(clipBox.getMainPanel());
        maxPanelWidth = Math.max(clipPanel.getPreferredSize().width, maxPanelWidth);
    }

    public void buildCustumBlendPanel() {
        custumBlendPanel = new JPanel();
        custumBlendGUI = new JPanelCustumBlend(this);
        custumBlendPanel.add(custumBlendGUI.getMainPanel());
        maxPanelWidth = Math.max(custumBlendPanel.getPreferredSize().width, maxPanelWidth);
    }
    
    /**
     * Build the display control panel for the surface render.
     */
    public void buildDisplayPanel() {
        displayPanel = new JPanel();
        displayGUI = new JPanelDisplay_WM(this);
        displayPanel.add(displayGUI.getMainPanel());
        maxPanelWidth = Math.max(displayPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the flythru move control panel.
     */
    public void buildFlythroughPanel() {
        flythruMovePanel = new JPanel();
        flythruMoveControl = new JPanelFlythruMove(m_kFlyThroughRender);
        flythruMovePanel.add(flythruMoveControl.getMainPanel());
        maxPanelWidth = Math.max(flythruMovePanel.getPreferredSize().width, maxPanelWidth);

        m_kFlyThroughPanel = new JPanel();
        flythruControl = new JPanelVirtualEndoscopySetup_WM(m_kFlyThroughRender);
        m_kFlyThroughPanel.add(flythruControl.getMainPanel());
        flythruControl.getMainPanel().setVisible(true);
        m_kFlyThroughRender.setupRenderControl(flythruControl);
        maxPanelWidth = Math.max(m_kFlyThroughPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the Geodesic control panel.
     */
    public void buildGeodesic() {
        m_kGeodesicPanel = new JPanel();
        geodesicGUI = new JPanelGeodesic_WM(this);
        //geodesicGUI.setVisible(true);
        m_kGeodesicPanel.add(geodesicGUI.getMainPanel());
        maxPanelWidth = Math.max(m_kGeodesicPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * The histogram control panel of the lookup table.
     */
    public void buildHistoLUTPanel() {
        histoLUTPanel = new JPanel();

        if (imageA.isColorImage()) {
            histoLUTPanel.add(panelHistoRGB.getMainPanel());
        } else {
            histoLUTPanel.add(panelHistoLUT.getMainPanel());
        }

        maxPanelWidth = Math.max(histoLUTPanel.getPreferredSize().width, maxPanelWidth);
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#buildLabelPanel()
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
        panelLabelsScanner.add(scannerPanel);
        panelLabelsScanner.add(absolutePanel);

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
        m_kLightsPanel = new JPanelLights_WM(this);
        lightPanel.add(m_kLightsPanel.getMainPanel());
        maxPanelWidth = Math.max(lightPanel.getPreferredSize().width, maxPanelWidth);
    }

    public void buildMultiHistogramPanel() {
        multiHistogramPanel = new JPanel();
        multiHistogramGUI = new JPanelMultiDimensionalTransfer(this, m_kAnimator, m_kVolumeImageA);
        multiHistogramPanel.add(multiHistogramGUI.getMainPanel());
        maxPanelWidth = Math.max(multiHistogramPanel.getPreferredSize().width, maxPanelWidth);
    }
    
    /**
     * Build the volume opacity control panel for the surface render.
     */
    public void buildOpacityPanel() {
        opacityPanel = new JPanel();


        if (imageA.isColorImage()) {
            m_kVolOpacityPanel = new JPanelVolOpacityRGB(this, imageA, imageB);
        } else {
            m_kVolOpacityPanel = new JPanelVolOpacity(this, imageA, imageB);
        }
        
       
        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();

        opacityPanel.setLayout(gbLayout);
        gbConstraints.weightx = 1;
        gbConstraints.weighty = 1;
        gbConstraints.fill = GridBagConstraints.BOTH;
        gbConstraints.anchor = GridBagConstraints.NORTH;
        
        gbLayout.setConstraints(m_kVolOpacityPanel.getMainPanel(), gbConstraints);
        opacityPanel.add(m_kVolOpacityPanel.getMainPanel());
        maxPanelWidth = Math.max(opacityPanel.getPreferredSize().width, maxPanelWidth);
    }
    
    /**
     * Builds the render mode control  panel.
     */
    public void buildRenderModePanel()
    {
    	renderModePanel = new JPanel();
        rendererGUI = new JPanelRenderMode_WM(this);
        renderModePanel.add(rendererGUI.getMainPanel());
        maxPanelWidth = Math.max(renderModePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the Sculpturing control panel.
     */
    public void buildSculpt() {
        m_kSculptPanel = new JPanel();
        sculptGUI = new JPanelSculptor_WM(this);
        m_kSculptPanel.add(sculptGUI.getMainPanel());
        maxPanelWidth = Math.max(m_kSculptPanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the slices control panel for the surface render.
     */
    public void buildSlicePanel() {
        slicePanel = new JPanel();
        sliceGUI = new JPanelSlices_WM(this);
        slicePanel.add(sliceGUI.getMainPanel());
        maxPanelWidth = Math.max(slicePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the adding surface control panel for the surface render.
     */
    public void buildSurfacePanel() {
        surfacePanel = new JPanel();
        surfaceGUI = new JPanelSurface_WM(this);
        surfacePanel.add(surfaceGUI.getMainPanel());
        maxPanelWidth = Math.max(surfacePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Builds the Surface texture panel.
     */
    public void buildSurfaceTexturePanel()
    {
        surfaceTexturePanel = new JPanel();
        surfaceTextureGUI = new JPanelSurfaceTexture_WM(this);
        surfaceTexturePanel.add(surfaceTextureGUI.getMainPanel());
        maxPanelWidth = Math.max(surfaceTexturePanel.getPreferredSize().width, maxPanelWidth);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#componentMoved(java.awt.event.ComponentEvent)
     */
    public void componentMoved(ComponentEvent event)
    {       
        setModified();
    }
    
    
    /**
     * Method called when a component resize event is generated. This method
     * snaps the size of the frame and pagePanel to the nearest row, column
     * sizing (so the gridRow and gridColumn and page layout may change).
     *
     * @param  event  frame resize event
     */
    public synchronized void componentResized(ComponentEvent event) {
        resizePanel();
    }
    
    /**
     * Construct the volume rendering methods based on the choices made from
     * the resample dialog. This method is called by the Resample dialog.
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

            serif12 = MipavUtil.font12;
            serif12B = MipavUtil.font12B;
            progressBar.updateValueImmed(5);

            if (imageA.isColorImage()) {
                m_kVolOpacityPanel = new JPanelVolOpacityRGB(this, imageA, imageB);
            } else {
                m_kVolOpacityPanel = new JPanelVolOpacity(this, imageA, imageB);
            }
            
            String kExternalDirs = getExternalDirs();        
            ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
            VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
            PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
            CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
            m_kVolumeImageA = new VolumeImage(  imageA, LUTa, RGBTA, "A" );
            //VolumeImageViewer.main(this, m_kVolumeImageA, true);

            if ( imageB != null )
            {
                m_kVolumeImageB = new VolumeImage( imageB, LUTb, RGBTB, "B" );
            }

            m_kAnimator = new Animator();
            m_akPlaneRender = new PlaneRender_WM[3];
            m_akPlaneRender[0] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.AXIAL);
            m_akPlaneRender[1] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.SAGITTAL);
            m_akPlaneRender[2] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.CORONAL);
            
            progressBar.setMessage("Constructing gpu renderer...");

            

            raycastRenderWM = new VolumeTriPlanarRender( this, m_kAnimator, m_kVolumeImageA,
                                                         m_kVolumeImageB);


            TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
            m_kVolumeImageA.UpdateImages(kTransfer, 0, null);
            if ( imageB != null )
            {
                kTransfer = m_kVolOpacityPanel.getCompB().getOpacityTransferFunction();
                m_kVolumeImageB.UpdateImages(kTransfer, 0, null);
            }

            progressBar.updateValueImmed(80);
            progressBar.setMessage("Constructing Lookup Table...");

            if (imageA.isColorImage()) {
                panelHistoRGB = new JPanelHistoRGB(imageA, imageB, RGBTA, RGBTB, true);
            } else {
                panelHistoLUT = new JPanelHistoLUT(imageA, imageB, LUTa, LUTb, true, true);
            }

            progressBar.updateValueImmed(100);

            this.configureFrame( true );
            
            // After the whole WM rendering framework built, force updating the color LUT table in order to 
            // update both the volume viewer and tri-planar viewer.  Otherwise, the render volume turns to be black.
            if ( panelHistoLUT != null ) 
            	panelHistoLUT.updateComponentLUT();
            
        } finally {
            progressBar.dispose();
        }

        if (imageA.isColorImage()) {
            setRGBTA(RGBTA);

            if ((imageB != null) && imageB.isColorImage()) {
                setRGBTB(RGBTB);
            }

            updateImages(true);
        }
        else
        {
            updateImages(true);
        }
        // Toolkit.getDefaultToolkit().setDynamicLayout( false );
    }

    public void CustumBlendMode()
    {
        insertTab("CustumBlend", custumBlendPanel);
        custumBlendGUI.getMainPanel().setVisible(true);
        custumBlendGUI.actionPerformed(new ActionEvent(this, 0, ""));
    }
    
    /**
     * Causes redisplay of all components.
     */
    public void displayAll()
    {
        for (int i = 0; i < 3; i++) 
        {
            if ( m_akPlaneRender[i] != null )
            {
                m_akPlaneRender[i].GetCanvas().display();
            }
        }
        raycastRenderWM.GetCanvas().display();
        if ( m_kFlyThroughRender != null )
        {
            m_kFlyThroughRender.GetCanvas().display();
        }
    }
    
    /**
     * Dispose memory.
     *
     * @param  flag  call super dispose or not
     */
    public void disposeLocal(boolean flag) {
        //System.err.println( "Dispose Local" ) ;
        /* Geodesic panel */
        m_kGeodesicPanel = null;

        /* Sculpturing panel */
        m_kSculptPanel = null;

        histoLUTPanel = null;
        displayPanel = null;
        lightPanel = null;
        clipPanel = null;
        panelLabels = null;
        slicePanel = null;
        opacityPanel = null;
        surfacePanel = null;

        clipBox = null;

        if ( surfaceTextureGUI != null ) {
            surfaceTextureGUI.dispose();
            surfaceTextureGUI = null;
        }
        
        if ( surfaceGUI != null ) {
            surfaceGUI.dispose();
            surfaceGUI = null;
        }
        
        
        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.dispose();
            m_kVolumeImageA = null;
        }

        if (m_kVolumeImageB != null) {
            m_kVolumeImageB.dispose();
            m_kVolumeImageB = null;
        }

        if (raycastRenderWM != null) {
            raycastRenderWM.dispose();
            raycastRenderWM = null;
        }

        if (panelHistoLUT != null) {
            panelHistoLUT.disposeLocal();
            panelHistoLUT = null;
        }

        if (panelHistoRGB != null) {
            panelHistoRGB.disposeLocal();
            panelHistoRGB = null;
        }
        
        if ( multiHistogramGUI != null )
        {
            multiHistogramGUI.dispose();
            multiHistogramGUI = null;
        }
        multiHistogramPanel = null;
        
        if ( custumBlendGUI != null )
        {
            custumBlendGUI.dispose();
            custumBlendGUI = null;
        }
        custumBlendPanel = null;

        for (int i = 0; i < 3; i++) {

            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].disposeLocal();
                m_akPlaneRender[i] = null;
            }
        }

        m_kAnimator.stop();
        java.util.Iterator kDrawables = m_kAnimator.drawableIterator();
        while ( kDrawables.hasNext() )
        {
            GLAutoDrawable kGL = (GLAutoDrawable)kDrawables.next();
            m_kAnimator.remove(kGL);
        }
        m_kAnimator = null;

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
        super.dispose();
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
     * @param iFilterType type of sample filter, may be one of 7 different
     * filters: TriLinear Interpolation, NearestNeighbor, CubicBSpline,
     * QuadraticBSpline, CubicLagragian, QuinticLagragian, HepticLagragian, or
     * WindowedSinc (see AlgorithmTransform.java).
     */
    public void doResample(int[] volExtents, float[] newRes
                           , boolean forceResample, int nDim, int iFilterType)
    {
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

            transformFunct.disposeLocal();
            transformFunct = null;
            
            //new ViewJFrameImage((ModelImage)(imageA), null, new Dimension(610, 200), false);
            
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
     * Enable geodesic calculations and display.
     * @param bEnable when true geodesic curves are enabled.
     */
    public void enableGeodesic( boolean bEnable )
    {
        raycastRenderWM.enableGeodesic(bEnable);
    }

    /**
     * Enable painting on TriMesh surfaces.
     * @param kPaintColor paint color.
     * @param iBrushSize brush size.
     * @param bEnabled painting on/off.
     * @param bPaint when true apply paint.
     * @param bDropper when true do dropper mode.
     * @param bPaintCan when true do paint can mode.
     * @param bErase when true erase.
     */
    public void enablePaint( ColorRGBA kPaintColor, int iBrushSize,
                             boolean bEnabled, boolean bPaint,
                             boolean bDropper, boolean bPaintCan, boolean bErase )
    {
        raycastRenderWM.enablePaint(kPaintColor, iBrushSize,
                                    bEnabled, bPaint, bDropper, bPaintCan, bErase);
    }


    /**
     * Erase all surface paint.
     */
    public void eraseAllPaint( )
    {
        raycastRenderWM.eraseAllPaint();
    }

    public int get3DVOIQuantity()
    {
        return m_iVOITotal;
    }


    /**
     * Returns the ModelLUT or ModelRGB based on which image is currently
     * active, either imageA or imageB and they type of image (color or
     * grayscale).
     *
     * @return  the active LUT/RGB table.
     */
    public ModelStorageBase getActiveLookupTable(ModelImage kImage) {

        if (kImage == imageA) {

            if (imageA.isColorImage()) {
                return RGBTA;
            }

            return LUTa;
        } else if ((imageB != null) && (imageB.isColorImage())) {
            return RGBTB;
        }

        return LUTb;
    }

    /**
     * Get the imageA and imageB blending value from the PlaneRender.
     *
     * @return  blendValue blender slider value.
     */
    public int getBlendValue() {
        JPanelVolOpacityBase opacityPanel = m_kVolOpacityPanel;
        return opacityPanel.getAlphaBlendSliderValue();
    }

    /**
     * Get the camera current location 
     * @return camera position vector
     */
    public Vector3f getCameraLocation() {
    	return raycastRenderWM.getCameraLocation();
    }

    /**
     * Get the camera parameters. 
     * @return  camera parameters array. 
     */
    public float[] getCameraParameters() {
    	return raycastRenderWM.getCameraParameters();
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#getControls()
     */
    public ViewControlsImage getControls() {
        return null;
    }

    /**
     * Returns which image is active in the HistoLUT -- either imageA or
     * imageB. Called by the PlaneRenderer object to determine which LUT to
     * update based on dragging the right-mouse in the PlaneRender window:
     *
     * @return ModelImage, either imageA or imageB, depending on which is
     * selected in the HistoLUT
     */
    public ModelImage getHistoLUTActiveImage() {

        if (panelHistoLUT != null) {

            if (panelHistoLUT.getDisplayMode() == JPanelHistoLUT.IMAGE_A) {
                return imageA;
            } 
            return imageB;
        }

        return null;
    }

    /**
     * Returns which image is active in the HistoRGB -- either imageA or
     * imageB. Called by the PlaneRenderer object to determine which LUT to
     * update based on dragging the right-mouse in the PlaneRender window:
     *
     * @return ModelImage, either imageA or imageB, depending on which is
     * selected in the HistoLUT
     */
    public ModelImage getHistoRGBActiveImage() {

        if (panelHistoRGB != null) {

            if (panelHistoRGB.getDisplayMode() == JPanelHistoRGB.IMAGE_A) {
                return imageA;
            } 
            return imageB;
        }

        return null;
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
     * Return the Light[] used in the volume/surface display. 
     * @return
     */
    public Light[] GetLights()
    {
        return raycastRenderWM.GetLights();
    }

    /**
     * Get the LUT panel (only should be used with grayscale images).
     *
     * @return  the histo LUT panel
     */
    public JPanelHistoLUT getLUTDialog() {
        return panelHistoLUT;
    }

    /**
     * Return the material properties of the given surface.
     * @param kSurfaceName the surface to query.
     * @return the material properties of the surface.
     */
    public MaterialState getMaterial(String kSurfaceName)
    {
        return raycastRenderWM.getMaterial(kSurfaceName);
    }


    public VolumeNode getNode(String kSurfaceName)
    {       
        return raycastRenderWM.GetNode(kSurfaceName);
    }

    /**
     * Get the object parameters.
     * @return Get the object rotation parameters array.
     */
    public float[] getObjectParameters() {
    	return raycastRenderWM.getObjectParameters();
    }


    /**
     * Get the object rotation matrix.
     * @return rotation matrix
     */
    public Matrix3f getObjectRotation() {
    	return raycastRenderWM.getObjectRotation();
    }

    /** 
     * Get the render mode interface panel.
     * @return render mode interface panel.
     */
    public JPanelRenderMode_WM getRendererGUI()
    {
        return rendererGUI;
    }
    
    /**
     * Get the RGB panel (only should be used with color images).
     *
     * @return  the histo RGB panel
     */
    public JPanelHistoRGB getRGBDialog() {
        return panelHistoRGB;
    }
    
    /**
     * Return the size of the surface-area of the given surface.
     * @param kSurfaceName the surface to calculate the surface-area for.
     * @return the surface-area of the surface.
     */
    public float getSurfaceArea(String kSurfaceName)
    {
        return raycastRenderWM.getSurfaceArea(kSurfaceName);
    }

    /**
     * Return the surface panel.
     * @returnthe surface panel.
     */
    public JPanelSurface_WM getSurfacePanel()
    {
        return surfaceGUI;
    }

    /**
     * Return the translation vector for the surface with the given name.
     * @param kSurfaceName the surface to move.
     * @return the translation vector
     */
    public Vector3f getTranslateSurface(String kSurfaceName)
    {
        return raycastRenderWM.getTranslateSurface(kSurfaceName);
    }

    public IntVector[] getVOIImage()
    {
        return m_kVOIImage;
    }

    /**
     * Return the size of the volume of the given surface.
     * @param kSurfaceName the surface to calculate the volume for.
     * @return the volume of the surface.
     */
    public float getVolume(String kSurfaceName)
    {
        return raycastRenderWM.getSurfaceVolume(kSurfaceName);
    }
    
    /**
     * @return VolumeTriPlanarRender object.
     */
    public VolumeTriPlanarRender getVolumeGPU()
    {
        return raycastRenderWM; 
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
     * Enables picking correspondence points between the surface renderer and
     * the BrainSurfaceFlattener renderer.
     * @param bOn true enables, false disables.
     */
    public void PickCorrespondence( boolean bOn )
    {
        raycastRenderWM.pickCorrespondence(bOn);
    }
    
    /**
     * Passes the triangle indices of the picked triangle to the
     * BrainSurfaceFlattener renderer for display.
     * @param iV0 index 0 of the picked triangle.
     * @param iV1 index 1 of the picked triangle.
     * @param iV2 index 2 of the picked triangle.
     */
    public void PickCorrespondence( int iV0, int iV1, int iV2 )
    {
        if ( m_kBrainsurfaceFlattenerPanel != null )
        {
            brainsurfaceFlattenerRender.drawPicked(iV0, iV1, iV2);
        }        
    }

    public void refreshLighting() {
    	m_kLightsPanel.refreshLighting();
    }

    /**
     * Removes all geodesic curves for the given surface.
     * @param kSurface the surface to modify.
     */
    public void removeAllGeodesic( TriMesh kSurface )
    {
        raycastRenderWM.removeAllGeodesic(kSurface);
    }
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#removeControls()
     */
    public void removeControls() { }

    /**
     * Remove the specific geodesic curves from the given surface.
     * @param kSurface the surface to modify.
     * @param iNode the node to remove.
     * @param iGroup the group the node belongs to.
     */
    public void removeGeodesic( TriMesh kSurface, int iNode, int iGroup )
    {
        raycastRenderWM.removeGeodesic(kSurface, iNode, iGroup);
    }

    public VolumeObject removeNode(String kNodeName)
    {       
        return raycastRenderWM.RemoveNode(kNodeName);
    }

    /**
     * Remove the polyline from the volume DTI display.
     * @param groupIndex the polyline to remove.
     */
    public void removePolyline(int groupIndex)
    {
        raycastRenderWM.removePolyline(groupIndex);
    }

    /**
     * Remove the given surface from the render display list.
     * @param kSurfaceName the name of the surface to remove.
     */
    public void removeSurface(String kSurfaceName)
    {       
        raycastRenderWM.removeSurface(kSurfaceName);
        deleteVOISurface(kSurfaceName);
    }
    
    /**
     * remove the multi-histo tab. 
     * @param _name
     */
    public void removeTab(String _name) {
    	int i;

        for (i = 0; i < tabbedPane.getTabCount(); i++) {

            if ((tabbedPane.getComponentAt(i) != null) && tabbedPane.getTitleAt(i).equals(_name)) {
                tabbedPane.remove(i);
                return;
            }
        }
    }

    /**
     * When the Geodesic object cuts the mesh along an open curve, the old
     * mesh changes, but does not need to be deleted and no new mesh needs to
     * be added. This function allows the Geodesic object to replace the
     * original mesh with the sliced mesh in the surface renderer. ReplaceMesh
     * is also used to undo cutting operations.
     *
     * @param  kOld  TriMesh old surface mesh
     * @param  kNew  TriMesh new surface mesh
     */
    public void replaceGeodesic(TriMesh kOld, TriMesh kNew) {
        raycastRenderWM.replaceGeodesic(kOld, kNew);
    }

    /**
     * Reset image volume orientation along X axis.
     */
    public void resetAxisX() {
        raycastRenderWM.resetAxisX();
    }

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
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setActiveImage(int)
     */
    public void setActiveImage(int active) { }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setAlphaBlend(int)
     */
    public void setAlphaBlend(int value) { }
    
    /**
     * Enables backface culling for the given surface.
     * @param kSurfaceName the surface to modify.
     * @param bOn when true back-face culling is enabled, false disables
     * backface culling.
     */
    public void setBackface(String kSurfaceName, boolean bOn)
    {
        raycastRenderWM.setBackface(kSurfaceName, bOn );
    }
    

    /**
     * Sets the background color.
     * @param  color  
     */
    public void setBackgroundColor(Color color)
    {
        raycastRenderWM.setBackgroundColor(new ColorRGBA( color.getRed()/255.0f,
                                                          color.getGreen()/255.0f,
                                                          color.getBlue()/255.0f,
                                                          1.0f ) );
    }
    
    /**
     * Sets the volume bounding box color.
     * @param  color
     */
    public void setBoundingBoxColor(Color color)
    {
        raycastRenderWM.setBoundingBoxColor(new ColorRGB( color.getRed()/255.0f,
                                                          color.getGreen()/255.0f,
                                                          color.getBlue()/255.0f ) );
    }

    /**
     * Set the camera location.
     * @param v  camera position vector
     */
    public void setCameraLocation(Vector3f v) {
    	raycastRenderWM.setCameraLocation(v);
    }


    /**
     * Display the camera parameters in the user-interface.
     */
    public void setCameraParameters() {
    	displayGUI.displayCameraParams(raycastRenderWM.getCameraParameters());
    }

    /**
     * Enable clipping for the given surface.
     * @param kSurfaceName the surface to modify.
     * @param bClip true enables clipping, false disables clipping.
     */
    public void setClipping(String kSurfaceName, boolean bClip)
    {
        raycastRenderWM.setClipping(kSurfaceName, bClip );
    }

    /**
     * Set the color for the given surface.
     * @param kSurfaceName the surface to modify.
     * @param kColor the new color.
     */
    public void setColor(String kSurfaceName, ColorRGB kColor)
    {
        raycastRenderWM.setColor(kSurfaceName, kColor);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setControls()
     */
    public void setControls() { }

    public void SetCustumBlend(int iBlendEquation, int iLogicOp, int iSrcBlend, int iDstBlend, ColorRGBA kColor )
    {
        raycastRenderWM.SetCustumBlend( iBlendEquation, iLogicOp, iSrcBlend, iDstBlend, kColor );
    }

    public void setDefaultCursor( )
    {
        setCursor( new Cursor(Cursor.DEFAULT_CURSOR) );
        Component kComponent = m_kVOIToolbar.getComponentAtIndex(0);
        if ( kComponent instanceof JToggleButton )
        {
            JToggleButton kButton = (JToggleButton)m_kVOIToolbar.getComponentAtIndex(0);
            kButton.setSelected(true);
            doVOI( "Default" );
        }
    }

    /**
     * Passes the picked dropper color to the surface interface.
     * @param kDropperColor the color of the surface at the picked point.
     * @param kPickPoint the picked point for use in the region-grow operation.
     */
    public void setDropperColor( ColorRGBA kDropperColor, Vector3f kPickPoint )
    {
        if ( surfaceGUI != null )
        {
            surfaceGUI.setDropperColor(kDropperColor, kPickPoint);
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setEnabled(boolean)
     */
    public void setEnabled(boolean flag) { }

    /**
     * Passes the picked point to the Geodesic object for calculating the
     * geodesic curve on the TriMesh surface.
     * @param kMesh the surface that was picked.
     * @param kPickPoint the picked point.
     */
    public void setGeodesic( TriMesh kMesh, PickRecord kPickPoint )
    {
        if ( geodesicGUI != null )
        {
            geodesicGUI.setPickedPoint( kPickPoint, kMesh );
        }
    }

    /**
     * Turn the gradient magnitude filter on/off for volume shaders.
     * @param bShow on/off.
     */
    public void setGradientMagnitude( boolean bShow )
    {
        raycastRenderWM.setGradientMagnitude(bShow);
        TransferFunction kTransfer =
            m_kVolOpacityPanel.getCompA_GM().getOpacityTransferFunction();
        m_kVolumeImageA.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagA());
        if ( imageB != null )
        {
            kTransfer = m_kVolOpacityPanel.getCompB_GM().getOpacityTransferFunction();
            m_kVolumeImageB.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagB());
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setImageB(gov.nih.mipav.model.structures.ModelImage)
     */
    public void setImageB(ModelImage _imageB) { }

    /**
     * Sets the ModelImage to use as an alternative to the volume ModelImage
     * for surface texturing.
     * @param kSurfaceName the surface to modify.
     * @param kImage the alternate ModelImage to use for the surface texture.
     */
    public void SetImageNew(  String kSurfaceName, ModelImage kImage )
    {
        raycastRenderWM.setImageNew(kSurfaceName, kImage);
    }

    /**
     * Sets the inter-pupillary distance for stereo rendering.
     * @param fIPD the IPD value.
     */
    public void setIPD( float fIPD )
    {
        raycastRenderWM.setIPD(fIPD);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setLUTa(gov.nih.mipav.model.structures.ModelLUT)
     */
    public void setLUTa(ModelLUT LUT) {} 
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setLUTb(gov.nih.mipav.model.structures.ModelLUT)
     */
    public void setLUTb(ModelLUT LUT) {}    
    
    /**
     * Sets the LUT to use as an alternative to the volume lut for surface texturing.
     * @param kSurfaceName the surface to modify.
     * @param kLUT the new LUT.
     * @param kRGBT the new ModelRGB (for color images).
     */
    public void SetLUTNew( String kSurfaceName, ModelLUT kLUT, ModelRGB kRGBT )
    {
        raycastRenderWM.setLUTNew(kSurfaceName, kLUT, kRGBT);
    }
    
    /**
     * Sets the material for the given surface.
     * @param kSurfaceName the surface to update.
     * @param kMaterial the new material.
     */
    public void setMaterial(String kSurfaceName, MaterialState kMaterial)
    {
        raycastRenderWM.setMaterial(kSurfaceName, kMaterial);
    }
      
    /**
     * Causes the bottom three panels to re-display.
     */
    public void setModified()
    {
        if ( m_akPlaneRender != null )
        {
            for (int i = 0; i < 3; i++) {
                if ( m_akPlaneRender[i] != null )
                {
                    m_akPlaneRender[i].SetModified(true);
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
     * @param rot  rotation matrix
     */
    public void setObjectRotation(Matrix3f rot) {
    	raycastRenderWM.setObjectRotation(rot);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setPaintBitmapSwitch(boolean)
     */
    public void setPaintBitmapSwitch(boolean paintBitmapSwitch) { }

    /**
     * Turn picking on/off for the given surface.
     * @param kSurfaceName the surface to modify.
     * @param bOn when true enable picking, false disables picking.
     */
    public void setPickable(String kSurfaceName, boolean bOn)
    {
        raycastRenderWM.setPickable(kSurfaceName, bOn );
    }

    /**
     * Set the polygon mode (FILL, LINE, POINT) for the given surface.
     * @param kSurfaceName the surface to modify.
     * @param eMode FILL, LINE, or POINT.
     */
    public void setPolygonMode(String kSurfaceName, WireframeState.FillMode eMode)
    {
        raycastRenderWM.setPolygonMode(kSurfaceName, eMode );
    } 

    /**
     * Sets the position labels.
     *
     * @param  position  the slice positions in FileCoordinates.
     */
    public void setPositionLabels(Vector3f position) {

        if (scannerLabel == null) {
            return;
        }

        setScannerPosition(position);
        setPatientSlicePosition(position);
        set3DModelPosition(position);
        setAbsPositionLabels(position);
    }    

    /**
     * Toggles between radiological and neurological views of the data.
     * @param bOn when true display using radiological coordinates, when
     * false use neurological.
     */
    public void setRadiological( boolean bOn )
    {
        imageA.setRadiologicalView(bOn);

        if (imageB != null) {
            imageB.setRadiologicalView(bOn);
        }
        Vector3f center = sliceGUI.getCenter();
        raycastRenderWM.setCenter( new Vector3f( center.X, center.Y, center.Z ) );    
        for (int i = 0; i < 3; i++) {
            if ( m_akPlaneRender[i] != null )
            {
                m_akPlaneRender[i].setRadiologicalView(bOn);
                m_akPlaneRender[i].setCenter(center);
            }
        }
        setPositionLabels(center);
    }

    /**
     * Switches between orthographic and perspective projection.
     * @param bEnable when true enable perspective projection, when false use
     * orthographic projection.
     */
    public void setRenderPerspective(boolean bEnable)
    {
        if ( bEnable )
        {
            raycastRenderWM.setPerspectiveProjection();
        }
        else
        {
            raycastRenderWM.setOrthographicProjection();
        }
    }    
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setRGBTA(gov.nih.mipav.model.structures.ModelRGB)
     */
    public void setRGBTA(ModelRGB RGBT) {
        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.SetRGBT(RGBT);
        }
        raycastRenderWM.setRGBTA(RGBT);
    }    
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setRGBTB(gov.nih.mipav.model.structures.ModelRGB)
     */
    public void setRGBTB(ModelRGB RGBT) {
        if (m_kVolumeImageB != null) {
            m_kVolumeImageB.SetRGBT(RGBT);
        }
        raycastRenderWM.setRGBTB(RGBT);
    }
    
    
    /**
     * Turn the volume bounding box frame on/off.
     * @param bShow when true display the bounding box.
     */
    public void setShowBoxFrame(boolean bShow)
    {
        raycastRenderWM.displayBoundingBox(bShow);
    }

    /**
     * Turn the orientation cube on/off.
     * @param bShow when true display the orientation cube, when false do not
     * display the cube.
     */
    public void setShowOrientationCube(boolean bShow)
    {
        raycastRenderWM.displayOrientationCube(bShow);
    }    
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#setSlice(int)
     */
    public void setSlice(int slice) {}

    /**
     * Sets the position of the slices in the SurfaceRender and PlaneRender
     * objects. Called from the PlaneRender class.
     * @param  center  the new slice positions in FileCoordinates
     */
    public void setSliceFromPlane(Vector3f center) {
        setPositionLabels(center);
       
        for (int i = 0; i < 3; i++) {
            if ( m_akPlaneRender[i] != null )
            {
                m_akPlaneRender[i].setCenter(center);
            }
        }

        raycastRenderWM.setCenter( center );
        sliceGUI.setCenter((int)center.X, (int)center.Y, (int)center.Z);
    }
    
    /**
     * Sets the position of the slices in the PlaneRender. Called from the
     * SurfaceRender class.
     * @param  center  the new slice positions in FileCoordinates
     */
    public void setSliceFromSurface(Vector3f center) {
        setPositionLabels(center);

        if (m_akPlaneRender != null)
        {
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
     * @param  iView  (AXIAL, SAGITTAL, CORONAL)
     * @param  color  the new axis color attribute.
     */
    public void setSliceHairColor(int iView, Color color) {

        ColorRGB kColor = new ColorRGB(color.getRed()/256.0f,
                                       color.getGreen()/256.0f,
                                       color.getBlue()/256.0f);

        for (int i = 0; i < 3; i++) {
            if ( m_akPlaneRender[i] != null )
            {
                m_akPlaneRender[i].setSliceHairColor(iView, kColor );
            }
        }
        raycastRenderWM.setBoundingBoxColor( iView, kColor);
    }
    
    
    /**
     * Set the transparency value for the slice.
     * @param i the slice to modify.
     * @param fAlpha the new transparency value.
     */
    public void setSliceOpacity( int i, float fAlpha )
    {
        raycastRenderWM.setSliceOpacity( i, fAlpha );
    }
    
    /**
     * Turns on surface texture display for the given surface. The user can
     * use a separate ModelImage and LUT than the one displayed in the volume
     * renderer.
     * @param kSurfaceName the name of the surface to texture.
     * @param bOn texture on/off.
     * @param bUseNewImage when false use the current ModelImage, when true
     * the user specifies the model image.
     * @param bUseNewLUT when false use the current LUT, when true the user
     * specifies the LUT.
     */
    public void setSurfaceTexture(String kSurfaceName, boolean bOn, boolean bUseNewImage, boolean bUseNewLUT)
    {
        raycastRenderWM.setSurfaceTexture(kSurfaceName, bOn, bUseNewImage, bUseNewLUT );
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#setTimeSlice(int)
     */
    public void setTimeSlice(int slice) { }
  
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#setTitle()
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
     * Set the transparency for the given surface.
     * @param kSurfaceName the name of the surface to modify.
     * @param fValue transparency value.
     */
    public void setTransparency(String kSurfaceName, float fValue)
    {
        raycastRenderWM.blend(kSurfaceName, fValue );
    }
    
    
    /**
     * Turns showing the slice bounding box on/off.
     * @param i which slice bounding box to turn off.
     * @param bShow on/off.
     */
    public void showBoundingBox( int i, boolean bShow )
    {
        raycastRenderWM.showBoundingBox( i, bShow );
    }    
    
    /**
     * Turns showing the slice on/off.
     * @param i which slice to turn off.
     * @param bShow on/off.
     */
    public void showSlice( int i, boolean bShow )
    {
        raycastRenderWM.showSlice( i, bShow );
        setModified();
    }
    
    /**
     * Smooth the given surface.
     * @param kSurfaceName the name of the surface to smooth.
     * @param iteration smooth iterations.
     * @param alpha smooth factor.
     * @param volumeLimit whether to use a volume % change limit.
     * @param volumePercent the % volume change limiting factor
     */
    public void smoothMesh( String kSurfaceName, int iteration,
                            float alpha, boolean volumeLimit, float volumePercent)
    {
        raycastRenderWM.smoothMesh(kSurfaceName, iteration, alpha,
                                   volumeLimit, volumePercent);
    }
    
    /**
     * Smooth the given surface.
     * @param kSurfaceName the name of the surface to smooth.
     * @param iteration smooth iterations.
     * @param lambda smooth factor.
     * @param mu smooth factor.
     */
    public void smoothThree( String kSurfaceName, int iteration, float lambda, float mu)
    {
        raycastRenderWM.smoothThree(kSurfaceName, iteration, lambda, mu);
    }   
    
    /**
     * Smooth the given surface.
     * @param kSurfaceName the name of the surface to smooth.
     * @param iteration smooth iterations.
     * @param fStiffness stiffness factor.
     * @param volumeLimit whether to use a volume % change limit.
     * @param volumePercent the % volume change limiting factor.
     */
    public void smoothTwo( String kSurfaceName, int iteration,
                           float fStiffness, boolean volumeLimit, float volumePercent)
    {
        raycastRenderWM.smoothTwo(kSurfaceName, iteration,
                                  fStiffness, volumeLimit, volumePercent);
    }    
    
    /**
     * Switches between different ways of displaying the geodesic path
     * (Euclidean, Geodesic, or Mesh).
     * @param kSurfaceName the surface the path is on.
     * @param iWhich the type of display.
     */
    public void toggleGeodesicPathDisplay(String kSurfaceName, int iWhich)
    {
        raycastRenderWM.toggleGeodesicPathDisplay(kSurfaceName, iWhich);
    }    
    
    /**
     * Toggle the display on/off for the given Node.
     * @param kNode node to toggle on/off.
     * @param bDisplay display toggle on/off.
     */
    public void toggleNode( Node kNode, boolean bDisplay )
    {
        raycastRenderWM.displayNode(kNode, bDisplay);
    }
    
    /**
     * Changes the translation vector for the surface with the given name.
     * @param kSurfaceName the surface to move.
     * @param kTranslate the new translation vector
     */
    public void translateSurface(String kSurfaceName, Vector3f kTranslate)
    {
        raycastRenderWM.translateSurface(kSurfaceName, kTranslate);
    }


    /** 
     * update blending between images A/B.
     */
    public void updateABBlend()
    {
        raycastRenderWM.setABBlend(1 - getBlendValue()/100.0f);
        setModified();
    }
    
    /**
     * Causes the PlaneRender objects to update the texture maps when the
     * underlying ModelImage changes.
     */
    public void updateData() {
        if ( m_kVolumeImageA != null )
        {
            m_kVolumeImageA.UpdateData(imageA, "A");
        }
        if ( m_kVolumeImageB != null )
        {
            m_kVolumeImageB.UpdateData(imageB, "B");
        }
        raycastRenderWM.updateData();
        setModified();
    }
    
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#updateImageExtents()
     */
    public boolean updateImageExtents() {
        return false;
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages()
     */
    public boolean updateImages() {
        return true;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(boolean)
     */
    public boolean updateImages(boolean forceShow) {

        if ( m_kVolOpacityPanel == null )
        {
            return false;
        }
        ViewJComponentVolOpacityBase kSelectedComp = m_kVolOpacityPanel.getSelectedComponent();
        if ( kSelectedComp == null )
        {
            return false;
        }
        if (m_kVolumeImageA != null) {
            if ( kSelectedComp == m_kVolOpacityPanel.getCompA() )
            {
                TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
                m_kVolumeImageA.UpdateImages(kTransfer, 0, null);
            }
            else if ( kSelectedComp == m_kVolOpacityPanel.getCompA_GM() )
            {
                TransferFunction kTransfer = m_kVolOpacityPanel.getCompA_GM().getOpacityTransferFunction();
                m_kVolumeImageA.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagA());
            }
        }
        if ( m_kVolumeImageB != null )
        {
            if ( kSelectedComp == m_kVolOpacityPanel.getCompB() )
            {
                TransferFunction kTransfer = m_kVolOpacityPanel.getCompB().getOpacityTransferFunction();
                m_kVolumeImageB.UpdateImages(kTransfer, 0, null);
            }
            else if ( kSelectedComp == m_kVolOpacityPanel.getCompB_GM() )
            {
                TransferFunction kTransfer = m_kVolOpacityPanel.getCompB_GM().getOpacityTransferFunction();
                m_kVolumeImageB.UpdateImages(kTransfer, 2, m_kVolOpacityPanel.getGradMagB());
            }
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
        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.UpdateImages(LUTa);    
            setModified();
        }
        if (m_kVolumeImageB != null) {
            m_kVolumeImageB.UpdateImages(LUTb);    
            setModified();
        }

        return true;
    }
    
    public void updateLevWidgetState( ClassificationWidgetState kLWS, int iState )
    {
        raycastRenderWM.updateLevWidgetState( kLWS, iState );
    }
    
    public void updateLighting( Light[] akGLights )
    {
        raycastRenderWM.updateLighting(akGLights);
        for (int i = 0; i < 3; i++) 
        {
            if ( m_akPlaneRender[i] != null )
            {
                m_akPlaneRender[i].updateLighting(akGLights);
            }
        }
        if ( brainsurfaceFlattenerRender != null )
        {
            brainsurfaceFlattenerRender.updateLighting(akGLights);
        }  
        if ( m_kFlyThroughRender != null )
        {
            m_kFlyThroughRender.updateLighting(akGLights);
        }
    }
    
    
    /**
     * Update the multi-histogram tab when the MultiHistogram checkbox is checked in the renderMode panel.
     * @param flag  MultiHistogram Check box checked or not. 
     */
    public void updateMultihistoTab(boolean flag) {
    	   if ( flag ) {
    		   insertTab("MultiHistogram", multiHistogramPanel);
               multiHistogramGUI.getMainPanel().setVisible(flag);
    	   } else {
    		   removeTab("MultiHistogram");
    	   }
    	   rendererGUI.setDisplayVolumeCheck(true);
           raycastRenderWM.displayVolumeRaycast( true );
           raycastRenderWM.setVolumeBlend( rendererGUI.getBlendSliderValue()/100.0f );
           
    }
    
   
    /**
     * Causes the texture representation of all the surface meshes to be recalculated.
     */
    public void updatePlanes()
    {
        raycastRenderWM.redrawSurfaceTexture();
        raycastRenderWM.GetCanvas().display();
        setModified();
    }

    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#windowActivated(java.awt.event.WindowEvent)
     */
    public void windowActivated(WindowEvent event) {
        setModified();
        super.windowActivated(event);
        resizePanel();
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#windowClosing(java.awt.event.WindowEvent)
     */
    public void windowClosing(WindowEvent event) {
        close();
        disposeLocal(true);
        dispose();
    }
    /**
     * Add surface volume renderer control buttons.
     */
    protected void addToolbar() {
        etchedBorder = BorderFactory.createEtchedBorder();
        toolbarBuilder = new ViewToolBarBuilder(this);
        buildViewToolbar();
    }
    
    
    /**
     * Builds menus for the tri-planar view.
     * @return  new menu bar containing menus.
     */
    protected JMenuBar buildMenu() {
        JSeparator separator = new JSeparator();

        menuObj = new ViewMenuBuilder(this);

        JMenuBar menuBar = new JMenuBar();

        menuBar.add(menuObj.makeMenu("File", false,
                                     new JComponent[] {
                                         separator,
                                         menuObj.buildMenuItem("Open DTI Tract file", "DTI", 0, null, false),
                                         menuObj.buildMenuItem("Open BrainSurface Flattener view", "BrainSurface", 0, null, false),
                                         menuObj.buildMenuItem("Open Fly Through view", "FlyThru", 0, null, false),
                                         menuObj.buildMenuItem("Close frame", "CloseFrame", 0, null, false)
                                     }));
        menuBar.add(menuObj.makeMenu("Options", false,
                                     new JComponent[] {
                                         menuObj.buildCheckBoxMenuItem("Show axes", "ShowAxes", true),
                                         menuObj.buildCheckBoxMenuItem("Show crosshairs", "ShowXHairs", true),
                                     }));
        menuBar.add(menuObj.makeMenu("Toolbars", false,
                                     new JMenuItem[] {
                menuObj.buildCheckBoxMenuItem("VOI toolbar", "VOIToolbar", false)
                //menuObj.buildCheckBoxMenuItem("RFA toolbar", "RFAToolbar", false)
                                     }));

        menuObj.setMenuItemEnabled("RFA toolbar", false);
        menuObj.setMenuItemEnabled("Open BrainSurface Flattener view", false);
        menuObj.setMenuItemEnabled("Open Fly Through view", false);
        
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
     * The the top one volume view toolbar.
     */
    protected void buildViewToolbar() {
        viewToolBar = new JToolBar();
        viewToolBar.setBorder(etchedBorder);
        viewToolBar.setBorderPainted(true);
        viewToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        viewToolBar.setLayout(new GridBagLayout());
        viewToolBar.setFloatable(false);

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

        rfaButton = toolbarBuilder.buildButton("RFA", "Add probe to viewer", "rfa");
        viewToolBar.add(rfaButton);
        rfaSeparator = ViewToolBarBuilder.makeSeparator();
        viewToolBar.add(rfaSeparator);
        viewToolBar.add(toolbarBuilder.buildButton("Extract", "Extract rotated image", "extract"));
        rfaButton.setVisible(false);
        rfaSeparator.setVisible(false);
        
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
        

        gbc.gridy++;
        ViewToolBarBuilder toolBarObj = new ViewToolBarBuilder(this);
        m_kVOIToolbar = toolBarObj.buildVolumeTriPlanarVOIToolBar();
        m_kVOIToolbar.setVisible(false);
        panelToolbar.add(m_kVOIToolbar, gbc);

    }
    
    /**
     * Constructs main frame structures for image canvas.
     */
    protected void configureFrame( boolean bVolumeViewer ) {
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.setTabLayoutPolicy(JTabbedPane.WRAP_TAB_LAYOUT);
        tabbedPane.addChangeListener(this);
        getContentPane().add(tabbedPane, BorderLayout.WEST);

        screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
        screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;

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

        
        if ( bVolumeViewer )
        {
        	buildLabelPanel();
            buildHistoLUTPanel();
            buildOpacityPanel();
        }

        buildDisplayPanel();
        buildLightPanel();
        buildClipPanel();
        buildSlicePanel();
        buildSurfacePanel();
        buildRenderModePanel();
        buildGeodesic();
        buildSculpt();
        buildCustumBlendPanel();

        setTitle();

        triImagePanel = new JPanel();
        triImagePanel.setLayout(new GridLayout(1, 3, 10, 10));

        if ( bVolumeViewer )
        {
            panelAxial = new JPanel(new BorderLayout());
            panelAxial.add(m_akPlaneRender[0].GetCanvas(), BorderLayout.CENTER);

            panelSagittal = new JPanel(new BorderLayout());
            panelSagittal.add(m_akPlaneRender[1].GetCanvas(), BorderLayout.CENTER);

            panelCoronal = new JPanel(new BorderLayout());
            panelCoronal.add(m_akPlaneRender[2].GetCanvas(), BorderLayout.CENTER);
            
            triImagePanel.add(panelAxial);
            triImagePanel.add(panelSagittal);
            triImagePanel.add(panelCoronal);
            buildMultiHistogramPanel();
            buildSurfaceTexturePanel();
        }
        triImagePanel.setBorder(raisedbevel);

        int triImagePanelWidth = (int) (screenWidth * 0.51f);
        int triImagePanelHeight = (int) (screenHeight * 0.25f);

        triImagePanel.setPreferredSize(new Dimension(triImagePanelWidth, triImagePanelHeight));
        triImagePanel.setMinimumSize(new Dimension(150, 50));

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

        gpuPanel = new JPanel(new BorderLayout());
        setLocation(100, 100);

        if ( bVolumeViewer )
        {
            gpuPanel.add(raycastRenderWM.GetCanvas(), BorderLayout.CENTER);
            gpuPanel.setVisible(true);
        }
        raycastRenderWM.setVisible(false);
        int imagePanelWidth = (int) (screenWidth * 0.51f);
        int imagePanelHeight = (int) (screenHeight * 0.43f);

        gpuPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        gpuPanel.setMinimumSize(new Dimension(250, 250));
       
        bf_flyPanel = new JPanel(new BorderLayout());
        bf_flyPanel.setBorder(compound);
        
        dualPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, gpuPanel, bf_flyPanel);
        dualPane.setOneTouchExpandable(true);
        dualPane.setDividerSize(6);
        dualPane.setContinuousLayout(true);
        dualPane.setResizeWeight(1);
        
        rightPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, dualPane, triImagePanel);
        rightPane.setOneTouchExpandable(true);
        rightPane.setDividerSize(6);
        rightPane.setContinuousLayout(true);

        tabbedPane.setPreferredSize(new Dimension(maxPanelWidth, tabbedPane.getPreferredSize().height));

        JPanel tabPanel = new JPanel(new BorderLayout());

        tabPanel.add(tabbedPane);
        tabPanel.setMinimumSize(new Dimension(maxPanelWidth, 820));

        JSplitPane mainPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, tabPanel, rightPane);

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

        gpuPanel.setVisible(true);
        raycastRenderWM.setVisible(true);
    }
    
    protected void create3DVOI( boolean bIntersection )
    {

        if ( m_akPlaneRender != null )
        {
            ModelImage kImage = new ModelImage( ModelStorageBase.INTEGER, 
                    m_kVolumeImageA.GetImage().getExtents(), "Temp" );
            kImage.copyFileTypeInfo(m_kVolumeImageA.GetImage());
            for (int i = 0; i < 3; i++)
            {
                if ( m_akPlaneRender[i] != null )
                {
                    m_akPlaneRender[i].make3DVOI(bIntersection, kImage, i);
                }
            }
            kImage.calcMinMax();
            //new ViewJFrameImage(kImage, null, new Dimension(610, 200), false);
            int[] aiExtents = kImage.getExtents();
            int length = aiExtents[0] * aiExtents[1] * aiExtents[2];
            int[] buffer = new int[length];
            
            if ( m_kVOIImage == null )
            {
                m_kVOIImage = new IntVector[length];
            }

            for (int i = 0; i < length; i++) {
                buffer[i] = kImage.getInt(i);
                if ( bIntersection && (buffer[i] < 250) )
                {
                    buffer[i] = 0;
                }
                if ( buffer[i] != 0 )
                {
                    if ( m_kVOIImage[i] == null )
                    {
                        m_kVOIImage[i] = new IntVector();
                    }
                    if ( !m_kVOIImage[i].contains( m_iVOICount ) )
                    {
                        m_kVOIImage[i].add( m_iVOICount );
                    }
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
//              Get the adjacent triangles:
                VETMesh kVETMesh = new VETMesh( 2* kMesh.VBuffer.GetVertexQuantity(), .9f,
                        2 * kMesh.IBuffer.GetIndexQuantity(), .9f,
                        2 * kMesh.GetTriangleQuantity(), .9f,
                        kMesh.IBuffer.GetData() );
                kMesh.IBuffer = new IndexBuffer( kVETMesh.GetTriangles() );
                TriMesh[] kMeshes = new TriMesh[1];
                kMeshes[0] = kMesh;
                if ( kMeshes[0] != null )
                {
                    getVolumeGPU().displayVolumeRaycast(false);
                    m_kVOIName = new String( "VOI_" + m_iVOICount++ );
                    m_kVOINameList.add(m_kVOIName);
                    kMeshes[0].SetName( m_kVOIName );
                    getSurfacePanel().addSurfaces(kMeshes);
                    getRendererGUI().setDisplaySurfaceCheck( true );
                    getRendererGUI().setDisplayVolumeCheck( false );
                    m_iVOITotal++;
                }
                else
                {
                    m_kVOIName = "";
                }
                kVETMesh = null;
            }
            else
            {
                m_kVOIName = "";
            }
            kExtractor = null;
            kImage.disposeLocal();
            kImage = null;
        }

    }


    /**
     * Calculate the LUT from the resampled image.
     *
     * @param  image  ModelImage reference
     * @param  lut    ModelLUT reference
     */
    protected void resetLUTMinMax(ModelImage image, ModelLUT lut) {
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
    protected void resizePanel() {
        int height;

        height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
            panelToolbar.getHeight();

        if (panelHistoLUT != null) {
            panelHistoLUT.resizePanel(maxPanelWidth, height);
        }

        if (panelHistoRGB != null) {
            panelHistoRGB.resizePanel(maxPanelWidth, height);
        }
        sliceGUI.resizePanel(maxPanelWidth, height);
        surfaceGUI.resizePanel(maxPanelWidth, height);
        displayGUI.resizePanel(maxPanelWidth, height);
        rendererGUI.resizePanel(maxPanelWidth, height);
        geodesicGUI.resizePanel(maxPanelWidth, height);
        m_kLightsPanel.resizePanel(maxPanelWidth, height);
        clipBox.resizePanel(maxPanelWidth, height);
        if ( multiHistogramGUI != null )
        {
            multiHistogramGUI.resizePanel(maxPanelWidth, height);
        }
        if ( brainsurfaceFlattenerRender != null )
        {
            brainsurfaceFlattenerRender.resizePanel(maxPanelWidth, height);
            dualPane.setDividerLocation( 0.5f );
        }
        if ( m_kFlyThroughRender != null )
        {
            flythruControl.resizePanel(maxPanelWidth, height);
            dualPane.setDividerLocation( 0.5f );
        }
        
        rightPane.setDividerLocation( 0.618f ); 
        
    }

    /**
     * Sets the 3DModel position label.
     * @param  position  3DModel position values.
     */
    protected void set3DModelPosition(Vector3f position) {

        float fMaxX = (m_kVolumeImageA.GetImage().getExtents()[0] - 1) * m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[0];
        float fMaxY = (m_kVolumeImageA.GetImage().getExtents()[1] - 1) * m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[1];
        float fMaxZ = (m_kVolumeImageA.GetImage().getExtents()[2] - 1) * m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[2];

        float fMax = fMaxX;
        if (fMaxY > fMax) {
            fMax = fMaxY;
        }
        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }
        float fX = fMaxX/fMax;
        float fY = fMaxY/fMax;
        float fZ = fMaxZ/fMax;

        fX *= position.X/(m_kVolumeImageA.GetImage().getExtents()[0] - 1);
        fY *= position.Y/(m_kVolumeImageA.GetImage().getExtents()[1] - 1);
        fZ *= position.Z/(m_kVolumeImageA.GetImage().getExtents()[2] - 1);

        modelViewLabelVals[0].setText("X: " + fX);
        modelViewLabelVals[1].setText("Y: " + fY);
        modelViewLabelVals[2].setText("Z: " + fZ);

    }
    
    /**
     * Sets the PatientSlice position label.
     * @param  position value.
     */
    protected void setPatientSlicePosition(Vector3f position) {
        Vector3f axial = new Vector3f();
        MipavCoordinateSystems.fileToPatient(position, axial, imageA, FileInfoBase.AXIAL);

        Vector3f coronal = new Vector3f();
        MipavCoordinateSystems.fileToPatient(position, coronal, imageA, FileInfoBase.CORONAL);

        Vector3f sagittal = new Vector3f();
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
    protected void setRFAToolbarVisible(boolean flag) {
        rfaButton.setVisible(flag);
        rfaSeparator.setVisible(flag);
        viewToolBar.validate();
        viewToolBar.repaint();
    }

    private void deleteVOISurface( String kVOIName )
    {
        if ( m_kVOIImage == null )
        {
            return;
        }
        if ( !m_kVOINameList.contains(kVOIName) )
        {
            return;
        }
        m_kVOINameList.remove(m_kVOIName);
        int iVOICount = Integer.valueOf(kVOIName.substring( 4 )).intValue();
        m_iVOITotal--;
        int[] aiExtents = imageA.getExtents();
        int length = aiExtents[0] * aiExtents[1] * aiExtents[2];

        for (int i = 0; i < length; i++) 
        {
            if ( m_kVOIImage[i] != null )
            {
                if ( m_kVOIImage[i].contains( iVOICount ) )
                {
                    int iIndex = m_kVOIImage[i].indexOf( iVOICount );
                    m_kVOIImage[i].remove(iIndex);
                }
            }
        }
    }
    
    /**
     * Passes VOI-commands to the bottom three panels.
     */
    private void doVOI( String kCommand )
    {
        if ( m_akPlaneRender != null )
        {
            for (int i = 0; i < 3; i++) {
                if ( m_akPlaneRender[i] != null )
                {
                    m_akPlaneRender[i].doVOI(kCommand);
                }
            }
        }
    }
}
