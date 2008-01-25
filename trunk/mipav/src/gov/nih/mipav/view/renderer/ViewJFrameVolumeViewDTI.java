package gov.nih.mipav.view.renderer;


import gov.nih.mipav.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.*;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.surfaceview.*;
import gov.nih.mipav.view.renderer.surfaceview.brainflattenerview.*;
import gov.nih.mipav.view.renderer.surfaceview.flythruview.*;
import gov.nih.mipav.view.renderer.surfaceview.rfaview.*;
import gov.nih.mipav.view.renderer.volumeview.*;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import javax.media.opengl.GLCanvas;

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
public class ViewJFrameVolumeViewDTI extends ViewJFrameVolumeView implements MouseListener, ItemListener, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1898957906984534260L;

    /** DOCUMENT ME! */
    private GPUVolumeRender raycastRenderWM;
    
    /** The small bar on the top right corner the volume view frame. */
    //private static JProgressBar rendererProgressBar;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Menu items storage. */
    protected ViewMenuBuilder menuObj;

    /** Orientations of the three axes. */
    protected int[] orient = new int[3];

    /** Panel that holds the toolbars. */
    protected JPanel panelToolbar = new JPanel();

    /** Lookup table of the color imageA, B. */
    protected ModelRGB RGBTA = null, RGBTB = null;

    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;

    /** Indicates that image orientation is unknown type or not. */
    private boolean axialOrientation = true;

    /** DOCUMENT ME! */
    private JPanel displayPanel;
      

    /** Control panel for the surface renderer. */
    private JPanel histoLUTPanel;

    /** Reference to the imageA original copy. */
    private ModelImage imageAOriginal;

    /** Image orientation: coronal, sagittal, axial, unknown. */
    private int imageOrientation;

    /** The image panel to hold one Canvas3D. */
    private JPanel imagePanel;
    private JPanel surfaceRenderPanel;
    private JPanel gpuPanel;

    /** DOCUMENT ME! */
    private JDialogIntensityPaint intensityDialog;

    /** DOCUMENT ME! */
    private JPanel lightPanel;

    /** The three slice views displayed as texture-mapped polygons:. */
    private PlaneRender[] m_akPlaneRender;

    /** Control panel for drawing geodesic curves. */
    private JPanel m_kGeodesicPanel;

    /** The max width of the control panels. */
    private int maxPanelWidth = -1;

    /** Menu bar. */
    private JMenuBar menuBar;

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
    private JPanelHistoLUT panelHistoLUT;

    /** RGB control panel of the color image. */
    private JPanelHistoRGB panelHistoRGB;

    /** Rendering parallel rotation button. */
    private JToggleButton parallelButton;

    /** Radio button of the COMPOSITE mode option. */
    private JRadioButton radioCOMPOSITE;

    /** Radio button of the MIP mode option. */
    private JRadioButton radioMIP;

    /** Radio button of the SURFACE mode option. */
    private JRadioButton radioSURFACE;

    /** Radio button of the SURFACE mode option. */
    private JRadioButton radioSURFACEFAST;

    /** Radio button of the surface render composite mode. */
    private JRadioButton radioSurrenderCOMPOSITE;

    //** Check box to enable/disable surface self-shadowing */
    JCheckBox kSelfShadow;

    /** Radio button of the surface render lighting mode. */
    private JRadioButton radioSurrenderLIGHT;

    /** Radio button of the XRAY mode option. */
    private JRadioButton radioXRAY;

    /** Panel Border view. */
    private Border raisedbevel, loweredbevel, compound, redBorder, etchedBorder, pressedBorder;

    /** Reference to resample dialog, use to null out the resample dialog in this frame. */
    private JDialogVolViewResample resampleDialog;

    /** The view pane that contains the image view and tri-planar view panels. */
    private JSplitPane rightPane;

    /** Screen width, screen height. */
    private int screenWidth, screenHeight;

    /** DOCUMENT ME! */
    private JPanel slicePanel;

    /** Previoius tab index recorder. */
    private int storeTabbedPaneIndex = 0;

    /** DOCUMENT ME! */
    private JPanel surfacePanel;

    /** Three types of renderer. */
    private SurfaceRender surRender;

    /** For each render, use the vector to store the currently active tabs. */
    private Vector surTabVector = new Vector();

    /** Toolbar builder reference. */
    private ViewToolBarBuilder toolbarBuilder;

    /** Tri image planar render panels. */
    private JPanel triImagePanel;

    /** DOCUMENT ME! */
    private JPanel viewPanel;

    /** The top one render view switch toolbar. */
    private JToolBar viewToolBar;

    /** Surface Render toolbar. */
    private JToolBar surfaceToolBar;

    private boolean m_bSurfaceVisible = true;

    private JPanelDTILoad DTIimageLoadPanel;
    
    private JPanelDTIFiberTrack DTIFiberTrackPanel;
    
    private JPanelDTIParametersPanel DTIparamsPanel;

    /** Eigenvector image **/
    private ModelImage m_kEigenVectorImage;
    
    /** Anisotropy image **/
    private ModelImage m_kAnisotropyImage;
    
    /** Diffusion Tensor image. */
	private ModelImage m_kDTIImage = null;
	
	/** Diffusion Tensor image. */
	private ModelImage m_kDTIColorImage = null;
	
	private String m_kParentDir;

	private JPanel DTIFiberPanel;
	
	private JPanel DTIParametersPanel;
	
	private JPanel panelAxial;
     
    private JPanel panelSagittal;

    private JPanel panelCoronal;
	
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
    public ViewJFrameVolumeViewDTI(ModelImage _imageA, ModelLUT LUTa, ModelRGB _RGBTA, ModelImage _imageB, ModelLUT LUTb,
                                ModelRGB _RGBTB, int _leftPanelRenderMode, int _rightPanelRenderMode,
                                JDialogVolViewResample _resampleDialog) {
        super(_imageA,LUTa,_RGBTA,_imageB,LUTb,_RGBTB,_leftPanelRenderMode,_rightPanelRenderMode,_resampleDialog);
        // super(_imageA, _imageB);
        
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

    public static final JProgressBar getRendererProgressBar() {

        if (rendererProgressBar == null) {
            rendererProgressBar = new JProgressBar();
        }

        return rendererProgressBar;
    }
     */
    /**
     * Calls various methods depending on the action.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if (command.equals("Slices")) {
            enableSurfaceRender();
        } else if (command.equals("Opacity")) {
        	insertTab("Opacity", opacityPanel);
            enableVolumeRender();
            updateRayTracingSteps();
        } 
        else if (command.equals("HistoLUT")) {
            insertTab("LUT", histoLUTPanel);
            insertSurfaceTab("LUT", histoLUTPanel);
            insertRaycastTab("LUT", histoLUTPanel);
        } else if (command.equals("SurRender")) {
            enableSurfaceRender();
            updateRayTracingSteps();
        } else if (command.equals("VolRender")) {
            enableVolumeRender();
            updateRayTracingSteps();
        } else if (command.equals("Geodesic")) {
            insertTab("Geodesic", m_kGeodesicPanel);
            insertSurfaceTab("Geodesic", m_kGeodesicPanel);
            insertFlythruTab("Geodesic", m_kGeodesicPanel);
        } else if (command.equals("Repaint")) {
            volumeRepaint();
        } else if (command.equals("ChangeLight")) {
            insertTab("Light", lightPanel);
            insertSurfaceTab("Light", lightPanel);
            insertRaycastTab("Light", lightPanel);
        } else if (command.equals("Box")) {
            insertTab("Display", displayPanel);
            insertSurfaceTab("Display", displayPanel);
        } else if (command.equals("ViewControls")) {
            insertTab("View", viewPanel);
            insertSurfaceTab("View", viewPanel);
        } else if (command.equals("SurfaceDialog")) {
            insertTab("Surface", surfacePanel);
            insertSurfaceTab("Surface", surfacePanel);
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            surRender.getSurfaceDialog().resizePanel(maxPanelWidth, height);
        } else if (command.equals("SurfaceTexture")) {
            insertTab("SurfaceTexture", surRender.getSurfaceTexturePanel());
            insertSurfaceTab("SurfaceTexture", surRender.getSurfaceTexturePanel());
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

            surRender.getSurfaceDialog().resizePanel(maxPanelWidth, height);
        } else if (command.equals("DTI")) {
            JDialogDTIInput kDTIIn = new JDialogDTIInput( JDialogDTIInput.TRACTS_PANEL,
                                                          raycastRenderWM,
                                                          surRender.getSurfaceDialog(), imageA);
            insertTab("DTI", kDTIIn.getMainPanel() );
            insertSurfaceTab("DTI", kDTIIn.getMainPanel() );
            insertRaycastTab("DTI", kDTIIn.getMainPanel() );
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
                m_akPlaneRender[iPlane].update();
            }
        } else if (command.equals("ShowXHairs")) {
            boolean showXHairs = menuObj.isMenuItemSelected("Show crosshairs");

            for (int iPlane = 0; iPlane < 3; iPlane++) {
                m_akPlaneRender[iPlane].showXHairs(showXHairs);
                m_akPlaneRender[iPlane].update();
            }
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
        } else if (command.equals("ShaderParameters") ) {
            if ( raycastRenderWM != null )
            {
                raycastRenderWM.displayShaderParameters();
            }
        }

    }

    
    public void setParentDir(String _path) {
       m_kParentDir = _path;
    }
    
    public void setEVimage(ModelImage _m_kEigenVectorImage) {
    	m_kEigenVectorImage = _m_kEigenVectorImage;
    }
    
    public void setFAimage(ModelImage _m_kAnisotropyImage) {
    	m_kAnisotropyImage = _m_kAnisotropyImage;
    }
    
    public void setDTIimage(ModelImage _m_kDTIImage) {
	    m_kDTIImage = _m_kDTIImage;
    }
	
    
    
    public void setDTIColorImage(ModelImage _m_kDTIColorImage) {
    	m_kDTIColorImage = _m_kDTIColorImage;
    	reConfiguration();
    }
    
    public String getParentDir() {
    	return m_kParentDir;
    }
    
    public ModelImage getEVimage() { 
       return m_kEigenVectorImage;
    }
    
    public ModelImage getFAimage() { 
    	return m_kAnisotropyImage;
    }
    
	public ModelImage getDTIimage() { 
		return m_kDTIImage;
	}
	

    /**
     * Add any attached surfaces the current image has in its file info (if the file info is in the xml format).
     */
    public void addAttachedSurfaces() {

        if ((surRender != null) && (surRender.getSurfaceDialog() != null)) {
            surRender.getSurfaceDialog().addAttachedSurfaces();
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
    }



    /**
     * Build the display control panel for the surface render.
     */
    public void buildDisplayPanel() {
        displayPanel = new JPanel();
        displayPanel.add(((SurfaceRender) surRender).getDisplayDialog().getMainPanel());
        maxPanelWidth = Math.max(displayPanel.getPreferredSize().width, maxPanelWidth);
    }



    /**
     * Build the Geodesic control panel.
     */
    public void buildGeodesic() {
        m_kGeodesicPanel = new JPanel();
        m_kGeodesicPanel.add(surRender.getGeodesicPanel().getMainPanel());
        maxPanelWidth = Math.max(m_kGeodesicPanel.getPreferredSize().width, maxPanelWidth);
    }

    public void buildDTIimageLoadPanel() {
    	JPanel DTIimagePanel = new JPanel();
    	
    	DTIimagePanel.add(DTIimageLoadPanel.getMainPanel());
    	
    	maxPanelWidth = Math.max(DTIimagePanel.getPreferredSize().width, maxPanelWidth);
    	
    	tabbedPane.addTab("DTI Images", null, DTIimagePanel);
    }
    
    public void buildDTIFiberTrackPanel() {
    	DTIFiberPanel = new JPanel();
    	
    	DTIFiberPanel.add(DTIFiberTrackPanel.getMainPanel());
    	
    	maxPanelWidth = Math.max(DTIFiberPanel.getPreferredSize().width, maxPanelWidth);
    	
    	tabbedPane.addTab("Fiber Tracks", null, DTIFiberPanel);
    }
    
    public void setFiberTrackActive() {
    	insertTab("Fiber Tracks", DTIFiberPanel);
    }
    
    public void setDTIParamsActive() {
    	insertTab("Parameters", DTIParametersPanel);
    }
    
    public void buildDTIParametersPanel() {
    	DTIParametersPanel = new JPanel();
    	
    	DTIParametersPanel.add(DTIparamsPanel.getMainPanel());
    	
    	maxPanelWidth = Math.max(DTIParametersPanel.getPreferredSize().width, maxPanelWidth);
    	
    	tabbedPane.addTab("Parameters", null, DTIParametersPanel);
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

    /**
     * The label panel of the x, y, z slider position.
     */
    public void buildLabelPanel() {
        super.buildLabelPanel();
    }

    /**
     * Build the light control panel for the surface render.
     */
    public void buildLightPanel() {
        lightPanel = new JPanel();
        lightPanel.add(surRender.getSurfaceDialog().getLightDialog().getMainPanel());
        maxPanelWidth = Math.max(lightPanel.getPreferredSize().width, maxPanelWidth);
        tabbedPane.addTab("Lights", null, lightPanel);
    }

    public JPanelLights getLightControl() {
    	return surRender.getSurfaceDialog().getLightDialog();
    }
    
    public JPanelSurface getSurfaceControl() {
    	return surRender.getSurfaceDialog();
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
        surfacePanel.add(surRender.getSurfaceDialog().getMainPanel());
        maxPanelWidth = Math.max(surfacePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the view control panel for the surface render.
     */
    public void buildViewPanel() {
        viewPanel = new JPanel();
        viewPanel.add(surRender.getViewDialog().getMainPanel());
        maxPanelWidth = Math.max(viewPanel.getPreferredSize().width, maxPanelWidth);
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

           

            serif12 = MipavUtil.font12;
            serif12B = MipavUtil.font12B;
            config = SimpleUniverse.getPreferredConfiguration();
            progressBar.updateValueImmed(5);

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

                progressBar.setMessage("Constructing gpu renderer...");
                raycastRenderWM = new GPUVolumeRender(imageA, LUTa, RGBTA, imageB, LUTb, RGBTB);
                TransferFunction kTransfer = surRender.getVolOpacityPanel().getCompA().getOpacityTransferFunction();
                raycastRenderWM.updateImages(0, kTransfer);
                if (surRender != null) {
                    surRender.setRayBasedRender(raycastRenderWM);
                }


            progressBar.updateValueImmed(80);
            progressBar.setMessage("Constructing Lookup Table...");

            if (imageA.isColorImage()) {
                panelHistoRGB = new JPanelHistoRGB(imageA, imageB, RGBTA, RGBTB, true);
            } else {
                panelHistoLUT = new JPanelHistoLUT(imageA, imageB, LUTa, LUTb, true);
            }

            DTIimageLoadPanel = new JPanelDTILoad(this);
            DTIFiberTrackPanel = new JPanelDTIFiberTrack(this);
            DTIparamsPanel = new JPanelDTIParametersPanel(this, raycastRenderWM);
            
            progressBar.updateValueImmed(100);

            this.configureFrame();
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

    /**
     * Construct the volume rendering methods based on the choices made from the resample dialog. This method is called
     * by the Resample dialog.
     */
    public void reConfiguration() {

        /** Progress bar show up during the volume view frame loading */
        progressBar = new ViewJProgressBar("Constructing renderers...", "Constructing renderers...", 0, 100, false,
                                           null, null);
        progressBar.updateValue(0, true);
        MipavUtil.centerOnScreen(progressBar);
        progressBar.setVisible(true);
        progressBar.updateValueImmed(1);
      
        /*
        if (m_kDTIColorImage.isColorImage()) {
            RGBTA = (ModelRGB) (m_kDTIColorImage.getParentFrame().getRGBTA().clone());

            if (RGBTA == null) {
                int[] RGBExtents = new int[2];
                RGBExtents[0] = 4;
                RGBExtents[1] = 256;
                RGBTA = new ModelRGB(RGBExtents);
            }
        } else {
            LUTa = (ModelLUT) (m_kDTIColorImage.getParentFrame().getLUTa().clone());

            if (LUTa == null) {
                LUTa = initLUT(imageA);
            }
        }
      
        if (_imageB != null) {
            imageB = (ModelImage) (_imageB.clone());

            if (imageB.isColorImage()) {
                ModelRGB modelRGB = _imageB.getParentFrame().getRGBTB();

                if (modelRGB == null) {
                    modelRGB = new ModelRGB(_imageB.getExtents());
                } else {
                    RGBTB = (ModelRGB) modelRGB.clone();
                }

                if (RGBTB == null) {
                    int[] RGBExtents = new int[2];
                    RGBExtents[0] = 4;
                    RGBExtents[1] = 256;
                    RGBTB = new ModelRGB(RGBExtents);
                }
            } else {
                ModelLUT modelLUT = _imageB.getParentFrame().getLUTb();

                if (modelLUT == null) {
                    LUTb = new ModelLUT(ModelLUT.GRAY, 256, _imageB.getExtents());
                } else {
                    LUTb = (ModelLUT) modelLUT.clone();

                    if (LUTa == null) {
                        LUTb = initLUT(imageB);
                    }
                }
            }
        }
         */

        try {

           

            serif12 = MipavUtil.font12;
            serif12B = MipavUtil.font12B;
            config = SimpleUniverse.getPreferredConfiguration();
            progressBar.updateValueImmed(5);

                progressBar.setMessage("Constructing surface renderer...");

                // TODO: Check 3D support from the card, and report to the user if the card support 3D or not.
                surRender = new SurfaceRender(m_kDTIColorImage, LUTa, imageB, LUTb, this, config, progressBar);
                surRender.setVolView(true);

                if (surRender != null) {
                    surRender.configureVolumeFrame();
                }
                
               
                m_akPlaneRender = new PlaneRender[3];
                m_akPlaneRender[0] = new PlaneRender(this, m_kDTIColorImage, LUTa, imageB, LUTb, config, FileInfoBase.AXIAL,
                                                     false);
                m_akPlaneRender[1] = new PlaneRender(this, m_kDTIColorImage, LUTa, imageB, LUTb, config, FileInfoBase.SAGITTAL,
                                                     false);
                m_akPlaneRender[2] = new PlaneRender(this, m_kDTIColorImage, LUTa, imageB, LUTb, config, FileInfoBase.CORONAL,
                                                     false);
                
                /*
                m_akPlaneRender[0].reLoadImage(m_kDTIColorImage);
                m_akPlaneRender[1].reLoadImage(m_kDTIColorImage);
                m_akPlaneRender[2].reLoadImage(m_kDTIColorImage);
                */
                
                progressBar.setMessage("Constructing gpu renderer...");
                raycastRenderWM.dispose();
                raycastRenderWM = new GPUVolumeRender(m_kDTIColorImage, LUTa, RGBTA, imageB, LUTb, RGBTB);
                // raycastRenderWM.reConfiguration(m_kDTIColorImage, LUTa, RGBTA);
                TransferFunction kTransfer = surRender.getVolOpacityPanel().getCompA().getOpacityTransferFunction();
                raycastRenderWM.updateImages(0, kTransfer);
                if (surRender != null) {
                    surRender.setRayBasedRender(raycastRenderWM);
                }


            progressBar.updateValueImmed(80);
            progressBar.setMessage("Constructing Lookup Table...");

            /*
            if (m_kDTIColorImage.isColorImage()) {
                panelHistoRGB = new JPanelHistoRGB(m_kDTIColorImage, imageB, RGBTA, RGBTB, true);
            } else {
                panelHistoLUT = new JPanelHistoLUT(m_kDTIColorImage, imageB, LUTa, LUTb, true);
            }
            */
            // panelHistoLUT = new JPanelHistoLUT(m_kDTIColorImage, imageB, LUTa, LUTb, true);
            
            
            DTIimageLoadPanel = new JPanelDTILoad(this);
            DTIFiberTrackPanel = new JPanelDTIFiberTrack(this);
            DTIparamsPanel = new JPanelDTIParametersPanel(this, raycastRenderWM);
            
            progressBar.updateValueImmed(100);

            this.reConfigureFrame();
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
    
    
    /**
     * Disable target point for the RFA probe from within the plane renderer.
     */
    public void disableTargetPointPicking() {

        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].enableTargetPointPicking(false);
        }
    }

    /**
     * Dispose memory.
     *
     * @param  flag  call super dispose or not
     */
    public void disposeLocal(boolean flag) {
        // System.out.println( "######ViewJFrameVolView disposeLocal" );


        /* Geodesic panel */
        m_kGeodesicPanel = null;


        histoLUTPanel = null;
        displayPanel = null;
        viewPanel = null;
        lightPanel = null;
        panelLabels = null;
        slicePanel = null;
        opacityPanel = null;
        surfacePanel = null;


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

        if (resampleDialog != null) {
            resampleDialog.disposeLocal();
            resampleDialog = null;
        }

        for (int i = 0; i < 3; i++) {

            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].disposeLocal();
                m_akPlaneRender[i] = null;
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
            
            new ViewJFrameImage((ModelImage)(imageA), null, new Dimension(610, 200), false);
            
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
     * Called from the PlaneRender class when a new Probe Entry Point has been selected. The point is passed into each
     * PlaneRender class for display, and to the SurfaceRender class for display
     *
     * @param  kPoint  target point position
     */
    public void drawRFAPoint(Point3f kPoint) {

        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].drawRFAPoint(kPoint);
            m_akPlaneRender[iPlane].update();
        }

        surRender.drawRFAPoint(kPoint);
    }

    /**
     * Enable target point for the RFA probe from within the plane renderer.
     */
    public void enableTargetPointPicking() {

        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].enableTargetPointPicking(true);
        }
    }

    /**
     * Get the imageA and imageB blending value from the PlaneRender.
     *
     * @return  blendValue blender slider value.
     */
    public int getBlendValue() {
        JPanelVolOpacityBase opacityPanel = surRender.getVolOpacityPanel();

        return opacityPanel.getAlphaBlendSliderValue();
    }
    
    /**
     * Get the raytracing steps value.
     *
     * @return  raytrcing steps slider value.
     */
    public int getStepsValue() {
        JPanelVolOpacityBase opacityPanel = surRender.getVolOpacityPanel();

        return opacityPanel.getStepsSliderValue();
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

        if (panelHistoLUT != null) {

            if (panelHistoLUT.getDisplayMode() == JPanelHistoLUT.IMAGE_A) {
                return imageA;
            } else {
                return imageB;
            }
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

        if (panelHistoRGB != null) {

            if (panelHistoRGB.getDisplayMode() == JPanelHistoRGB.IMAGE_A) {
                return imageA;
            } else {
                return imageB;
            }
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
     */
    public JPanelHistoLUT getLUTDialog() {
        return panelHistoLUT;
    }


    /**
     * Return the rfa probe panel.
     *
     * @return  the rfa probe panel
     */
    public JPanelProbe getProbeDialog() {
        return surRender.getProbeDialog();
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
     * Check whether the Geodesic drawing is enabled or not.
     *
     * @return  boolean <code>true</code> Geodesic drawing enabled, <code>false</code> Geodesic disable.
     */
    public boolean isGeodesicEnable() {
        return surRender.getGeodesicPanel().isGeodesicEnable();
    }

    /**
     * Sets the flags for the getOptionses and resets labels.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (raycastRenderWM != null) {
            if (radioMIP.isSelected() && (source == radioMIP)) {
                raycastRenderWM.MIPMode();
                updateRayTracingSteps();
            } else if (radioXRAY.isSelected() && (source == radioXRAY)) {
                raycastRenderWM.DDRMode();
                updateRayTracingSteps();
            } else if (radioCOMPOSITE.isSelected() && (source == radioCOMPOSITE)) {
                raycastRenderWM.CMPMode();
                updateRayTracingSteps();
            } else if (radioSURFACE.isSelected() && (source == radioSURFACE)) {
                raycastRenderWM.SURMode();
                updateRayTracingSteps();
                surRender.getSurfaceDialog().getLightDialog().refreshLighting();
            } else if (radioSURFACEFAST.isSelected() && (source == radioSURFACEFAST)) {
                raycastRenderWM.SURFASTMode();
                updateRayTracingSteps();
                surRender.getSurfaceDialog().getLightDialog().refreshLighting();
            } else if (radioSURFACEFAST.isSelected() && (source == kSelfShadow) )
                raycastRenderWM.SelfShadow( kSelfShadow.isSelected() );
            	updateRayTracingSteps();
        	}
        if ( (imageB == null) )
        {
            kSelfShadow.setEnabled(radioSURFACEFAST.isSelected());
        }
    }

    /**
     * Handle the double mouse click event when the use swith between the dual image panel view.
     *
     * @param  e  MouseEvent
     */
    public void mouseClicked(MouseEvent e) {
        Object source = e.getSource();

        if (e.getClickCount() == 2) {

            if ((surRender != null) && (source == surRender.getCanvas())) {
                switchTabList("SurRender");
                GridBagConstraints gbc = new GridBagConstraints();

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.BOTH;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.weightx = 1;
                gbc.weighty = 1;
//                 panelToolbar.add(volToolBar, gbc);
                panelToolbar.validate();
                panelToolbar.repaint();
            } else if ((raycastRenderWM != null) && (source == raycastRenderWM.GetCanvas())) {
                switchTabList("VolRender");


                GridBagConstraints gbc = new GridBagConstraints();

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.BOTH;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.weightx = 1;
                gbc.weighty = 1;
//                 panelToolbar.add(rayCastToolBar, gbc);
                panelToolbar.validate();
                panelToolbar.repaint();
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
        }
    }

    /**
     * Reset image volume orieint along X axis.
     */
    public void resetAxisX() {  
         surRender.resetAxisX();
    }

    /**
     * Reset image volume orieint along Y axis.
     */
    public void resetAxisY() {
         surRender.resetAxisY();
    }

    /**
     * Reset image volume orieint along Z axis.
     */
    public void resetImage() {
         surRender.resetImage();
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
     *
     * @param  color  
     */
    public void setBackgroundColor(Color color)
    {
        if (raycastRenderWM != null) {
            raycastRenderWM.SetBackgroundColor(new ColorRGBA( color.getRed()/255.0f, color.getGreen()/255.0f, color.getBlue()/255.0f, 1.0f ) );
        }
    }

    /**
     *
     * @param  color  
     */
    public void setBoundingBoxColor(Color color)
    {
        if (raycastRenderWM != null) {
            raycastRenderWM.SetBoundingBoxColor(new ColorRGB( color.getRed()/255.0f, color.getGreen()/255.0f, color.getBlue()/255.0f ) );
        }
    }

    /**
     *
     * @param  color  
     */
    public void setShowBoxFrame(boolean bShow)
    {
        if (raycastRenderWM != null) {
            raycastRenderWM.DisplayBoundingBox(bShow);
        }
    }

    /**
     *
     * @param  color  
     */
    public void setShowOrientationCube(boolean bShow)
    {
        if (raycastRenderWM != null) {
            raycastRenderWM.DisplayOrientationCube(bShow);
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

    }

    /**
     * Set material ( texture or voxels ) shininess value.
     *
     * @param  value  float
     */
    public void setMaterialShininess(float value) {


        if (surRender != null) {
            surRender.setMaterialShininess(value);
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
     * @param  kPosition        Ruida please add comment
     * @param  kScaledPosition  Ruida please add comment
     */
    public void setPathPosition(Point3f kPosition, Point3f kScaledPosition) {
        Point3Df kCenter = new Point3Df(kPosition.x * imageA.getExtents()[0], kPosition.y * imageA.getExtents()[1],
                                        kPosition.z * imageA.getExtents()[2]);

        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].setCenter(kCenter);
        }

        surRender.setCenter(kCenter);
        surRender.getSurfaceDialog().setPathPosition(kScaledPosition);

    }

    /**
     * Sets the position labels.
     *
     * @param  position  the slice positions in FileCoordinates.
     */
    public void setPositionLabels(Point3Df position) {

        if (scannerLabel == null) {
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

        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].updateRGBTA(RGBT);
        }

        if (surRender != null) {
            surRender.setRGBTA(RGBT);
        }

        if (raycastRenderWM != null) {
            raycastRenderWM.setRGBT(RGBT, 0);
        }
    }

    /**
     * Sets the RGB table for image B.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTB(ModelRGB RGBT) {

        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].updateRGBTB(RGBT);
        }

        if (surRender != null) {
            surRender.setRGBTB(RGBT);
        }

        if (raycastRenderWM != null) {
            raycastRenderWM.setRGBT(RGBT, 1);
        }
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
    public void setSliceFromPlane(Point3Df center) {
        setPositionLabels(center);

        for (int i = 0; i < 3; i++) {
            m_akPlaneRender[i].setCenter(center);
        }

        surRender.setCenter(center);
    }

    /**
     * Sets the position of the slices in the PlaneRender. Called from the SurfaceRender class.
     *
     * @param  center  the new slice positions in FileCoordinates
     */
    public void setSliceFromSurface(Point3Df center) {
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
                
                surRender.switchToSliceView(false);
            } else if (tabbedPane.getSelectedComponent() == opacityPanel) {
          
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
        //tabbedPane.addTab("Positions", null, panelLabels);
        
        if (command.equals("VolRender")) {
            
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

        for (int i = 0; i < 3; i++) {

            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].updateData();
            }
        }

        if (surRender != null) {
            surRender.updateImages();
        }

        return true;
    }

    /** 
     * update blenading value.
     */
    public void updateBlend()
    {
        if (raycastRenderWM != null) {
            raycastRenderWM.Blend(1 - getBlendValue()/100.0f);
        }
    }
    
    /**
     * Update the raytrcing step size. 
     */
    public void updateRayTracingSteps()
    {
        if (raycastRenderWM != null) {
            raycastRenderWM.StepsSize(Math.round(getStepsValue() * 4.5f));
        }
    }

    /**
     * This methods calls corresponding render to update images without LUT changes.
     *
     * @param   forceShow  forces show to reimport image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(boolean forceShow) {

        for (int i = 0; i < 3; i++) {

            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].updateData();
            }
        }

        if (surRender != null) {
            surRender.updateImages(forceShow);
        }

        if (raycastRenderWM != null) {
            ViewJComponentVolOpacityBase kSelectedComp = surRender.getVolOpacityPanel().getSelectedComponent();
            if ( imageB != null )
            {
                if ( kSelectedComp == surRender.getVolOpacityPanel().getCompA() )
                {
                    TransferFunction kTransfer = surRender.getVolOpacityPanel().getCompA().getOpacityTransferFunction();
                    raycastRenderWM.updateImages(0, kTransfer);
                }
                else
                {
                    TransferFunction kTransfer = surRender.getVolOpacityPanel().getCompB().getOpacityTransferFunction();
                    raycastRenderWM.updateImages(1, kTransfer);
                }
            }
            else
            {
                if ( kSelectedComp == surRender.getVolOpacityPanel().getCompA() )
                {
                    TransferFunction kTransfer = surRender.getVolOpacityPanel().getCompA().getOpacityTransferFunction();
                    raycastRenderWM.updateImages(0, kTransfer);
                }
                else
                {
                    TransferFunction kTransfer = surRender.getVolOpacityPanel().getCompA_GM().getOpacityTransferFunction();
                    raycastRenderWM.updateImages(2, kTransfer);
                }
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

        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].updateLut(LUTa, LUTb);
        }

        if (surRender != null) {
            surRender.updateImages(LUTa, LUTb, forceShow, interpMode);
        }

        if (raycastRenderWM != null) {
            raycastRenderWM.updateImages(LUTa, LUTb);
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
    }

    /**
     * Causes the PlaneRender objects to update the texture maps when the underlying ModelImage changes.
     */
    public void updateSliceData() {

        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].updateData();
        }
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.updateData(imageA);
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
                                         separator,
                                         menuObj.buildMenuItem("Open DTI Tract file", "DTI", 0, null, false),
                                         menuObj.buildMenuItem("Close frame", "CloseFrame", 0, null, false)
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

        buildDTIimageLoadPanel();
        buildDTIFiberTrackPanel();
        buildDTIParametersPanel();
        buildLightPanel();
        
        // buildLabelPanel();
        buildHistoLUTPanel();
        buildOpacityPanel();


            buildDisplayPanel();
            buildViewPanel();
            // buildLightPanel();
            buildSlicePanel();
            buildSurfacePanel();
            buildCameraPanel();
            buildGeodesic();
           
        panelAxial = new JPanel(new BorderLayout());

        panelAxial.add(m_akPlaneRender[0].getCanvas(), BorderLayout.CENTER);

        panelSagittal = new JPanel(new BorderLayout());

        panelSagittal.add(m_akPlaneRender[1].getCanvas(), BorderLayout.CENTER);

        panelCoronal = new JPanel(new BorderLayout());

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
        surfaceRenderPanel = new JPanel(new BorderLayout());
        gpuPanel = new JPanel(new BorderLayout());

        setLocation(100, 100);

       
            surfaceRenderPanel.add(surRender.getCanvas(), BorderLayout.CENTER);
            gpuPanel.add(raycastRenderWM.GetCanvas(), BorderLayout.CENTER);
            imagePanel.add(surfaceRenderPanel, BorderLayout.EAST);
            surfaceRenderPanel.setVisible(true);
            imagePanel.add(gpuPanel, BorderLayout.WEST);
            gpuPanel.setVisible(true);
            raycastRenderWM.setVisible(false);
        

        int imagePanelWidth = (int) (screenWidth * 0.51f);
        int imagePanelHeight = (int) (screenHeight * 0.43f);

        imagePanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        imagePanel.setMinimumSize(new Dimension(500, 500));
        imagePanel.setBorder(compound);

        surfaceRenderPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        surfaceRenderPanel.setMinimumSize(new Dimension(500, 500));

        gpuPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        gpuPanel.setMinimumSize(new Dimension(500, 500));


        rightPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, imagePanel, triImagePanel);

        rightPane.setOneTouchExpandable(true);
        rightPane.setDividerSize(6);
        rightPane.setContinuousLayout(true);
        rightPane.setResizeWeight(1);

        tabbedPane.setPreferredSize(new Dimension(maxPanelWidth, tabbedPane.getPreferredSize().height));

        JPanel tabPanel = new JPanel(new BorderLayout());

        tabPanel.add(tabbedPane);
        tabPanel.setMinimumSize(new Dimension(maxPanelWidth, 789));

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

       

        enableSurfaceRender();
    }
    
    /**
     * Constructs main frame structures for image canvas.
     */
    protected void reConfigureFrame() {

        m_kDTIColorImage.setImageOrder(ModelImage.IMAGE_A);

        if (imageB != null) {
            imageB.setImageOrder(ModelImage.IMAGE_B);
        }


        if (m_kDTIColorImage == null) {
            return;
        }

        setResizable(true);
        addComponentListener(this);
        panelToolbar.removeAll();
        menuBar = buildMenu();

        setJMenuBar(menuBar);
        buildToolbars();
        addToolbar();
       
        panelAxial.removeAll();
        panelAxial.add(m_akPlaneRender[0].getCanvas(), BorderLayout.CENTER);
       
        panelSagittal.removeAll();
        panelSagittal.add(m_akPlaneRender[1].getCanvas(), BorderLayout.CENTER);

        panelCoronal.removeAll();
        panelCoronal.add(m_akPlaneRender[2].getCanvas(), BorderLayout.CENTER);
        
        // setTitle();

        // triImagePanel = new JPanel();
        triImagePanel.setLayout(new GridLayout(1, 3, 10, 10));
        triImagePanel.removeAll();
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

        setLocation(100, 100);

        surfaceRenderPanel.removeAll();
        gpuPanel.removeAll();
        imagePanel.removeAll();
        surfaceRenderPanel.removeAll();
        gpuPanel.removeAll();
        
        surfaceRenderPanel.add(surRender.getCanvas(), BorderLayout.CENTER);
        gpuPanel.add(raycastRenderWM.GetCanvas(), BorderLayout.CENTER);
        imagePanel.add(surfaceRenderPanel, BorderLayout.EAST);
        surfaceRenderPanel.setVisible(true);
        imagePanel.add(gpuPanel, BorderLayout.WEST);
        gpuPanel.setVisible(true);
        raycastRenderWM.setVisible(false);
        

        int imagePanelWidth = (int) (screenWidth * 0.51f);
        int imagePanelHeight = (int) (screenHeight * 0.43f);

        imagePanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        imagePanel.setMinimumSize(new Dimension(500, 500));
        imagePanel.setBorder(compound);

        surfaceRenderPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        surfaceRenderPanel.setMinimumSize(new Dimension(500, 500));

        gpuPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        gpuPanel.setMinimumSize(new Dimension(500, 500));
         
        // buildHistoLUTPanel();
        // buildOpacityPanel();
        tabbedPane.removeAll();
        buildDTIimageLoadPanel();
        buildDTIFiberTrackPanel();
        buildDTIParametersPanel();
        buildLightPanel();
        buildOpacityPanel();
       
        /*
        // buildLabelPanel();
        buildHistoLUTPanel();
        buildOpacityPanel();


            buildDisplayPanel();
            buildViewPanel();
            // buildLightPanel();
            buildSlicePanel();
            buildSurfacePanel();
            buildCameraPanel();
            buildGeodesic();
       */
        // MUST register frame to image models
        m_kDTIColorImage.addImageDisplayListener(this);

        if (imageB != null) {
            imageB.addImageDisplayListener(this);
        }

        pack();
        setVisible(true);
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        repaint();

        enableSurfaceRender();
    }

    /**
     * Cleans up memory from gc.
     *
     * @throws  Throwable  DOCUMENT ME!

    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }
     */
    /**
     * Creates and initializes the LUT for an image.
     *
     * @param   img  the image to create a LUT for
     *
     * @return  a LUT for the image <code>img</code> (null if a color image)
     *
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

    /**
     * Add surface volume renderer control buttons.
     */
    private void addToolbar() {
        etchedBorder = BorderFactory.createEtchedBorder();
        toolbarBuilder = new ViewToolBarBuilder(this);
        buildViewToolbar();

       
            buildSurRenderToolbar();
       
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 1;
//         gbc.gridwidth = 1;
//         gbc.gridheight = 1;
//         gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
//         gbc.weightx = 1;
//         gbc.weighty = 1;

            panelToolbar.add(surfaceToolBar, gbc);
       
    }




    /**
     * Build the surface render toolbar.
     */
    private void buildSurRenderToolbar() {
        surfaceToolBar = new JToolBar();
        surfaceToolBar.setBorder(etchedBorder);
        surfaceToolBar.setBorderPainted(true);
        surfaceToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        surfaceToolBar.setFloatable(false);
        surfaceToolBar.add(toolbarBuilder.buildButton("Repaint", "Repaints images", "paint"));
        surfaceToolBar.add(ViewToolBarBuilder.makeSeparator());
        surfaceToolBar.add(toolbarBuilder.buildButton("SurfaceDialog", "Add surface to viewer", "isosurface"));
        surfaceToolBar.add(toolbarBuilder.buildButton("Geodesic", "Draw geodesic curves on the surface", "geodesic"));
        surfaceToolBar.add(ViewToolBarBuilder.makeSeparator());
        surfaceToolBar.add(toolbarBuilder.buildButton("Mouse", "Record mouse changes", "camcorder"));
        surfaceToolBar.add(toolbarBuilder.buildButton("Capture", "Capture screen shot", "camera"));
        surfaceToolBar.add(ViewToolBarBuilder.makeSeparator());
        surfaceToolBar.add(toolbarBuilder.buildButton("Box", "Display options", "perspective"));
        surfaceToolBar.add(toolbarBuilder.buildButton("ViewControls", "View mode", "mousecontrol"));
        surfaceToolBar.add(ViewToolBarBuilder.makeSeparator());

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

        int rayCastMode = ViewJComponentRenderImage.ModeMIP; 
        radioMIP.setSelected(true);

        radioMIP.addItemListener(this);
        radioXRAY.addItemListener(this);
        radioCOMPOSITE.addItemListener(this);
        radioSURFACE.addItemListener(this);
        radioSURFACEFAST.addItemListener(this);
        surfaceToolBar.add(radioMIP);
        surfaceToolBar.add(radioXRAY);
        surfaceToolBar.add(radioCOMPOSITE);
        surfaceToolBar.add(radioSURFACEFAST);
        surfaceToolBar.add(radioSURFACE);

        kSelfShadow = new JCheckBox("Self Shadow", false);
        kSelfShadow.setFont(MipavUtil.font12);
        kSelfShadow.addItemListener(this);
        kSelfShadow.setEnabled(false);
        if ( (imageB == null) )
        {
            surfaceToolBar.add(kSelfShadow);
        }
        surfaceToolBar.add(ViewToolBarBuilder.makeSeparator());
        JButton kShaderButton = new JButton( "Shader Parameters" );
        kShaderButton.addActionListener(this);
        kShaderButton.setActionCommand("ShaderParameters");
        kShaderButton.setToolTipText("Open Shader Dialig");
        kShaderButton.setBorderPainted(false);
        kShaderButton.setFocusPainted(true);
        kShaderButton.setMargin(new Insets(0, 0, 0, 0));
        surfaceToolBar.add(kShaderButton);
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
      
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());
        viewToolBar.add(toolbarBuilder.buildButton("ChangeLight", "Add light bulb to viewer", "lightsmall"));
        viewToolBar.add(ViewToolBarBuilder.makeSeparator());

       
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
     * Enable surface render.
     */
    private void enableSurfaceRender()
    {
        if ( !m_bSurfaceVisible )
        {
            //switchTabList("SurRender");
            gpuPanel.setVisible(false);
            raycastRenderWM.setVisible(false);
            surfaceRenderPanel.setVisible(true);
            m_bSurfaceVisible = true;
        }
    }

    /**
     * Enable volume render.
     */
    private void enableVolumeRender() {
        if ( m_bSurfaceVisible )
        {
            //switchTabList("VolRender");
            surfaceRenderPanel.setVisible(false);
            gpuPanel.setVisible(true);
            raycastRenderWM.setVisible(true);
            m_bSurfaceVisible = false;
        }
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

        if (panelHistoLUT != null) {
            panelHistoLUT.resizePanel(maxPanelWidth, height);
        }

        if (panelHistoRGB != null) {
            panelHistoRGB.resizePanel(maxPanelWidth, height);
        }

      
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

    /**
     * Sets the 3DModel position label.
     *
     * @param  position  DOCUMENT ME!
     */
    private void set3DModelPosition(Point3Df position) {
        Point3Df screen = new Point3Df();
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
    private void setPatientSlicePosition(Point3Df position) {
        Point3Df axial = new Point3Df();
        MipavCoordinateSystems.fileToPatient(position, axial, imageA, FileInfoBase.AXIAL);

        Point3Df coronal = new Point3Df();
        MipavCoordinateSystems.fileToPatient(position, coronal, imageA, FileInfoBase.CORONAL);

        Point3Df sagittal = new Point3Df();
        MipavCoordinateSystems.fileToPatient(position, sagittal, imageA, FileInfoBase.SAGITTAL);

        patientSliceLabelVals[0].setText("sagittal slice: " + (int) sagittal.z);
        patientSliceLabelVals[1].setText("coronal slice: " + (int) coronal.z);
        patientSliceLabelVals[2].setText("axial slice: " + (int) axial.z);
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

    public GPUVolumeRender getRaycastRenderWM()
    {
        return raycastRenderWM;
    }

    public void setGradientMagnitude( boolean bShow )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.SetGradientMagnitude(bShow);
            TransferFunction kTransfer = surRender.getVolOpacityPanel().getCompA_GM().getOpacityTransferFunction();
            raycastRenderWM.updateImages(2, kTransfer);
        }
    }

}
