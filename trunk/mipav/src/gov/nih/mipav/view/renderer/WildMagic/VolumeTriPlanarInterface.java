package gov.nih.mipav.view.renderer.WildMagic;


import gov.nih.mipav.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.*;

import gov.nih.mipav.view.renderer.WildMagic.Interface.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;
import gov.nih.mipav.view.renderer.WildMagic.flythroughview.*;

import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Collision.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.Shaders.CompiledProgramCatalog;
import WildMagic.LibGraphics.Shaders.ImageCatalog;
import WildMagic.LibGraphics.Shaders.PixelProgramCatalog;
import WildMagic.LibGraphics.Shaders.VertexProgramCatalog;

import com.sun.opengl.util.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

public class VolumeTriPlanarInterface extends ViewJFrameBase 
implements MouseListener, ItemListener, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    protected static final long serialVersionUID = 1898957906984534260L;

    /** The small bar on the top right corner the volume view frame. */
    protected static JProgressBar rendererProgressBar;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Menu items storage. */
    protected ViewMenuBuilder menuObj;

    /** Labels for the current position in 3D ModelView coordinates:. */
    protected JLabel modelViewLabel = null;

    /** DOCUMENT ME! */
    protected JLabel[] modelViewLabelVals = new JLabel[3];

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

    /** Control panels for the Brainsurface Flattener:. */
    protected JPanel m_kBrainsurfaceFlattenerPanel = null;
    /** Rendering the brainsurfaceFlattener objects. */
    protected gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.CorticalAnalysisRender brainsurfaceFlattenerRender = null;
    protected FlyThroughRender m_kFlyThroughRender =  null;
    /** DOCUMENT ME! */
    protected JPanelClip_WM clipBox;
    protected JPanelSlices_WM sliceGUI;
    protected JPanelSurface_WM surfaceGUI;
    protected JPanelDisplay_WM displayGUI;
    protected JPanelGeodesic_WM geodesicGUI;
    protected JPanelSculptor_WM sculptGUI;
    protected JPanelSurfaceTexture_WM surfaceTextureGUI;

    protected JCheckBox m_kDisplayVolumeCheck;
    protected JCheckBox m_kDisplaySlicesCheck;
    protected JCheckBox m_kDisplaySurfaceCheck;
    protected JCheckBox m_kStereoCheck;
    /** Button to invoke all the six clipping planes. */
    protected JButton clipButton;

    /** Button to disable all the six clipping planes. */
    protected JButton clipDisableButton;

    /** Button to crop the clip volume. */
    protected JButton clipMaskButton;

    /** Button to undo crop the clip volume. */
    protected JButton clipMaskUndoButton;

    /** DOCUMENT ME! */
    protected JPanel clipPanel;

    /** Button to invoke clipping planes. */
    protected JButton clipPlaneButton;

    /** Button to save clipped region. */
    protected JButton clipSaveButton;

    /** DOCUMENT ME! */
    protected JPanel displayPanel;

    /** Control panel for the surface renderer. */
    protected JPanel histoLUTPanel;

    /** Reference to the imageA original copy. */
    protected ModelImage imageAOriginal;

    /** Image orientation: coronal, sagittal, axial, unknown. */
    protected int imageOrientation;

    /** The image panel to hold one Canvas3D. */
    protected JPanel gpuPanel;
    protected JPanel bf_flyPanel;

    /** DOCUMENT ME! */
    protected JDialogIntensityPaint intensityDialog;

    /** DOCUMENT ME! */
    protected JPanel lightPanel;

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

    /** DOCUMENT ME! */
    protected JDialogOpacityControls opacityDialog;

    /** DOCUMENT ME! */
    protected JPanel opacityPanel = null;

    /** Padding imageA with blank images feeding. */
    protected ModelImage paddingImageA;

    /** Padding imageB with blank images feeding. */
    protected ModelImage paddingImageB;

    /** LUT control panel of the gray scale image. */
    protected JPanelHistoLUT panelHistoLUT;

    /** RGB control panel of the color image. */
    protected JPanelHistoRGB panelHistoRGB;

    /** DOCUMENT ME! */
    protected JPanel probePanel;

    /** Radio button of the COMPOSITE mode option. */
    protected JRadioButton radioCOMPOSITE;

    /** Radio button of the MIP mode option. */
    protected JRadioButton radioMIP;

    /** Radio button of the SURFACE mode option. */
    protected JRadioButton radioSURFACE;

    /** Radio button of the SURFACE mode option. */
    protected JRadioButton radioSURFACEFAST;

    /** Radio button of the surface render composite mode. */
    protected JRadioButton radioSurrenderCOMPOSITE;

    //** Check box to enable/disable surface self-shadowing */
    JCheckBox kSelfShadow;

    /** Radio button of the surface render lighting mode. */
    protected JRadioButton radioSurrenderLIGHT;

    /** Radio button of the XRAY mode option. */
    protected JRadioButton radioXRAY;

    /** Panel Border view. */
    protected Border raisedbevel, loweredbevel, compound, redBorder, etchedBorder, pressedBorder;

    protected VolumeImage m_kVolumeImageA;
    protected VolumeImage m_kVolumeImageB;

    protected Animator m_kAnimator;

    /** DOCUMENT ME! */
    protected VolumeTriPlanarRender raycastRenderWM;

    /** DOCUMENT ME! */
    protected Vector raycastTabVector = new Vector();

    /** Button for RFA. */
    protected JButton rfaButton;

    /** RFA separator. */
    protected JButton rfaSeparator;

    /** The view pane that contains the image view and tri-planar view panels. */
    protected JSplitPane rightPane;

    protected JSplitPane dualPane;

    /** Screen width, screen height. */
    protected int screenWidth, screenHeight;

    /** Sculpt region height. */
    protected int sculptHeight;

    /** Sculpt region width. */
    protected int sculptWidth;

    /** DOCUMENT ME! */
    protected JPanel slicePanel;
    
    /** DOCUMENT ME! */
    protected JPanel surfaceTexturePanel;

    /** DOCUMENT ME! */
    protected JPanel surfacePanel;

    /** Toolbar builder reference. */
    protected ViewToolBarBuilder toolbarBuilder;

    /** Tri image planar render panels. */
    protected JPanel triImagePanel;

    /** The top one render view switch toolbar. */
    protected JToolBar viewToolBar;

    /** Surface Render toolbar. */
    protected JToolBar surfaceToolBar;

    protected boolean m_bSurfaceVisible = true;


    protected JPanelVolOpacityBase m_kVolOpacityPanel;

    protected JSlider m_kVolumeBlendSlider;

    protected JDialogStereoControls m_kStereoIPD = null;
    
    protected JPanel panelAxial;
    protected JPanel panelSagittal;
    protected JPanel panelCoronal;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------
    /**
     * Specific constructor call from the VolumeViewerDTI.   
     */
    public VolumeTriPlanarInterface() {
    	super(null, null);
    }
    
    
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
    public VolumeTriPlanarInterface(ModelImage _imageA, ModelLUT LUTa, ModelRGB _RGBTA, ModelImage _imageB, ModelLUT LUTb,
                                ModelRGB _RGBTB,
                                VolumeTriPlanarDialog _resampleDialog) {
        //super(_imageA,LUTa,_RGBTA,_imageB,LUTb,_RGBTB,_leftPanelRenderMode,_rightPanelRenderMode,_resampleDialog);
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

    //~ Methods --------------------------------------------------------------------------------------------------------

    public VolumeTriPlanarRender getVolumeGPU()
    {
        return raycastRenderWM; 
    }


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
        	raycastRenderWM.updateImageFromRotation();
        } else if (command.equals("HistoLUT")) {
            insertTab("LUT", histoLUTPanel);
        } else if (command.equals("VolRender")) {
            enableVolumeRender();
            updateRayTracingSteps();
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
            ((JPanelClip_WM)clipBox).resizePanel(maxPanelWidth, height);
        } else if (command.equals("OpacityHistogram")) {
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
            enableVolumeRender();
            updateRayTracingSteps();
            raycastRenderWM.DisplayVolumeRaycast( m_kDisplayVolumeCheck.isSelected() );
        } else if ( command.equals( "VolumeRayCast") ) {
            clipBox.getMainPanel().setVisible(true);
            clipButton.setEnabled(true);
            clipPlaneButton.setEnabled(true);
            clipDisableButton.setEnabled(true);
            clipMaskButton.setEnabled(true);
            clipMaskUndoButton.setEnabled(true);
            clipSaveButton.setEnabled(true);

            insertTab("Opacity", opacityPanel);
            enableVolumeRender();
            updateRayTracingSteps();
            raycastRenderWM.DisplayVolumeRaycast( m_kDisplayVolumeCheck.isSelected() );
        } else if (command.equals("Stereo")) {
            if ( (m_kStereoIPD == null) && m_kStereoCheck.isSelected() )
            {
                m_kStereoIPD = new JDialogStereoControls( this, .02f );
            }
            else if ( (m_kStereoIPD != null) && !m_kStereoCheck.isSelected() )
            {
                m_kStereoIPD.close();
                m_kStereoIPD = null;
            }
            raycastRenderWM.SetStereo( m_kStereoCheck.isSelected() );
        } else if (command.equals("ChangeLight")) {
            insertTab("Light", lightPanel);
        } else if (command.equals("Box")) {
            insertTab("Display", displayPanel);
        } else if (command.equals("InvokeClipping")) {
            clipBox.getMainPanel().setVisible(true);
            ((JPanelClip_WM)clipBox).invokeClippingPlanes();
            insertTab("Clip", clipPanel);

            setSize(getSize().width, getSize().height - 1);
            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();
            ((JPanelClip_WM)clipBox).resizePanel(maxPanelWidth, height);

            insertTab("Clip", clipPanel);
        } else if (command.equals("DisableClipping")) {
            clipBox.getMainPanel().setVisible(true);
            clipBox.disable6Planes();
            insertTab("Clip", clipPanel);
        } else if (command.equals("CropClipVolume")) {
//             surRender.cropClipVolume();
        } else if (command.equals("UndoCropVolume")) {
//             surRender.undoCropVolume();
        } else if (command.equals("SaveCropVolume")) {
//             surRender.saveCropVolume();
        } else if (command.equals("Slices")) {
            sliceGUI.getMainPanel().setVisible(true);
            insertTab("Slices", slicePanel);
            raycastRenderWM.DisplayVolumeSlices( m_kDisplaySlicesCheck.isSelected() );
        } else if (command.equals("VolumeSlices")) {
            sliceGUI.getMainPanel().setVisible(true);
            raycastRenderWM.DisplayVolumeSlices( m_kDisplaySlicesCheck.isSelected() );
        } else if (command.equals("Surface")) {
            raycastRenderWM.DisplaySurface( m_kDisplaySurfaceCheck.isSelected() );
        } else if (command.equals("SurfaceDialog")) {
            insertTab("Surface", surfacePanel);
            surfaceGUI.getMainPanel().setVisible(true);
            setSize(getSize().width, getSize().height - 1);

            int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
                         panelToolbar.getHeight();

//             surRender.getSurfaceDialog().resizePanel(maxPanelWidth, height);
        } else if (command.equals("SurfaceTexture")) {
            insertTab("SurfaceTexture", surfaceTexturePanel);
            surfaceTextureGUI.getMainPanel().setVisible(true);
            surfaceTextureGUI.setSurfacePanel(surfaceGUI);
            if ( surfaceGUI.surfaceAdded() )
            {
                surfaceTextureGUI.setEnabled(true);
            }
        } else if (command.equals("RFA")) {
//             insertTab("RFA", probePanel);
//             insertSurfaceTab("RFA", probePanel);

//             // hack to get the panel's scroll pane to show up correctly
//             // the MIPAV version of the RFAST needs this setSize() for some messed up reason...
//             setSize(getSize().width, getSize().height - 1);

//             int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
//                          panelToolbar.getHeight();

//             surRender.getProbeDialog().resizePanel(maxPanelWidth, height);
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
                            m_kAnimator, m_kVolumeImageA, imageA, LUTa, RGBTA,
                        m_kVolumeImageB, imageB, LUTb, RGBTB);
                TriMesh kSurface = raycastRenderWM.getSurface( surfaceGUI.getSelectedSurface() );
                Vector3f kCenter = raycastRenderWM.GetCenter( surfaceGUI.getSelectedSurface() );
                Node kMeshLines = brainsurfaceFlattenerRender.getPanel().displayCorticalAnalysis(kSurface, kCenter);       
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
            m_kFlyThroughRender = new FlyThroughRender( this, 
                    m_kAnimator, m_kVolumeImageA, imageA, LUTa, RGBTA,
                    m_kVolumeImageB, imageB, LUTb, RGBTB);
            TriMesh kSurface = raycastRenderWM.getSurface( surfaceGUI.getSelectedSurface() );
            //m_kFlyThroughRender.AddSurfaces( raycastRenderWM.getSurfaces(surfaceGUI.getSelectedSurfaces()) );
            m_kFlyThroughRender.addSurface(kSurface);
            bf_flyPanel.add( m_kFlyThroughRender.GetCanvas(), BorderLayout.CENTER );
            dualPane.setDividerLocation( 0.5f );            
            m_kLightsPanel.enableLight(0, true);
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
        } else if (command.equals("ProbeTargetPoint")) {
            enableTargetPointPicking();
        } else if (command.equals("traverse")) {
            disableTargetPointPicking();
        } else if (command.equals("RadiologicalView")) {          
            setRadiological(true);
        } else if (command.equals("NeurologicalView")) {
            setRadiological(false);
        } else if (command.equals("ShaderParameters") ) {
            if ( raycastRenderWM != null )
            {
                raycastRenderWM.displayShaderParameters();
            }
        }

    }

    /**
     * Add any attached surfaces the current image has in its file info (if the file info is in the xml format).
     */
    public void addAttachedSurfaces() {

//         if ((surRender != null) && (surRender.getSurfaceDialog() != null)) {
//             surRender.getSurfaceDialog().addAttachedSurfaces();
//         }
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

    /**
     * Build the display control panel for the surface render.
     */
    public void buildDisplayPanel() {
        displayPanel = new JPanel();
        displayGUI = new JPanelDisplay_WM(this);
        //displayGUI.setVisible(true);
        displayPanel.add(displayGUI.getMainPanel());
        maxPanelWidth = Math.max(displayPanel.getPreferredSize().width, maxPanelWidth);
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
        //m_kLightsPanel.setVisible(true);
        lightPanel.add(m_kLightsPanel.getMainPanel());
        maxPanelWidth = Math.max(lightPanel.getPreferredSize().width, maxPanelWidth);
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
     * Build the volume opacity control panel for the surface render.

    public void buildOpacityPanel() {
        opacityPanel = new JPanel();

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
     */
    /**
     * Build the adding surface control panel for the surface render.
     */
    public void buildProbePanel() {
        probePanel = new JPanel();
//         probePanel.add(surRender.getProbeDialog().getMainPanel());
        maxPanelWidth = Math.max(probePanel.getPreferredSize().width, maxPanelWidth);
    }

    /**
     * Build the camera control panel for the raycast render.
     */
    public void buildRayCastCameraPanel() {
//         raycastCameraPanel = new JPanel();
//         raycastCameraPanel.add(raycastRender.getCameraControl().getMainPanel());
//         maxPanelWidth = Math.max(raycastCameraPanel.getPreferredSize().width, maxPanelWidth);
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

    public void buildSurfaceTexturePanel()
    {
        surfaceTexturePanel = new JPanel();
        surfaceTextureGUI = new JPanelSurfaceTexture_WM(this);
        surfaceTexturePanel.add(surfaceTextureGUI.getMainPanel());
        maxPanelWidth = Math.max(surfaceTexturePanel.getPreferredSize().width, maxPanelWidth);
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
            m_kVolumeImageA = new VolumeImage(  imageA, LUTa, RGBTA );
            if ( imageB != null )
            {
                m_kVolumeImageB = new VolumeImage( imageB, LUTb, RGBTB );
            }

            m_kAnimator = new Animator();
            m_akPlaneRender = new PlaneRender_WM[3];
            m_akPlaneRender[0] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa,
                                                    m_kVolumeImageB, imageB, LUTb, FileInfoBase.AXIAL,
                                                    false);
            m_akPlaneRender[1] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa,
                                                    m_kVolumeImageB, imageB, LUTb, FileInfoBase.SAGITTAL,
                                                    false);
            m_akPlaneRender[2] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa,
                                                    m_kVolumeImageB, imageB, LUTb, FileInfoBase.CORONAL,
                                                    false);
            
            progressBar.setMessage("Constructing gpu renderer...");

            

            raycastRenderWM = new VolumeTriPlanarRender( this, m_kAnimator, m_kVolumeImageA, imageA, LUTa, RGBTA,
                                                     m_kVolumeImageB, imageB, LUTb, RGBTB);


            TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
            m_kVolumeImageA.UpdateImages(kTransfer, 0);

            progressBar.updateValueImmed(80);
            progressBar.setMessage("Constructing Lookup Table...");

            if (imageA.isColorImage()) {
                panelHistoRGB = new JPanelHistoRGB(imageA, imageB, RGBTA, RGBTB, true);
            } else {
                panelHistoLUT = new JPanelHistoLUT(imageA, imageB, LUTa, LUTb, true, true);
            }

            progressBar.updateValueImmed(100);

            this.configureFrame( true );
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

//         for (int iPlane = 0; iPlane < 3; iPlane++) {
//             m_akPlaneRender[iPlane].enableTargetPointPicking(false);
//         }
    }

    /**
     * Dispose memory.
     *
     * @param  flag  call super dispose or not
     */
    public void disposeLocal(boolean flag) {
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
        
        
        if (intensityDialog != null) {
            intensityDialog.dispose();
            intensityDialog = null;
        }

        if (opacityDialog != null) {
            opacityDialog.dispose();
            opacityDialog = null;
        }

        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.dispose();
            m_kVolumeImageA = null;
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

        for (int i = 0; i < 3; i++) {

            if (m_akPlaneRender[i] != null) {
                m_akPlaneRender[i].disposeLocal();
                m_akPlaneRender[i] = null;
            }
        }

        m_kAnimator.stop();
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

        /*
        imageA = (ModelImage)imageA.clone( imageA.getImageFileName() );
        imageA.calcMinMax();
        */
    }

    /**
     * Called from the PlaneRender class when a new Probe Entry Point has been selected. The point is passed into each
     * PlaneRender class for display, and to the SurfaceRender class for display
     *
     * @param  kPoint  target point position
     */
//    public void drawRFAPoint(Point3f kPoint) {

//        for (int iPlane = 0; iPlane < 3; iPlane++) {
//            m_akPlaneRender[iPlane].drawRFAPoint(kPoint);
//            m_akPlaneRender[iPlane].update();
//        }

//         surRender.drawRFAPoint(kPoint);
 //   }

    /**
     * Enable target point for the RFA probe from within the plane renderer.
     */
    public void enableTargetPointPicking() {

//         for (int iPlane = 0; iPlane < 3; iPlane++) {
//             m_akPlaneRender[iPlane].enableTargetPointPicking(true);
//         }
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
     * Get the raytracing steps value.
     *
     * @return  raytrcing steps slider value.
     */
    public int getStepsValue() {
        JPanelVolOpacityBase opacityPanel = m_kVolOpacityPanel;
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
//    public JPanelProbe getProbeDialog() {
//         return surRender.getProbeDialog();
//        return null;
//    }

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

//         if (surRender != null) {
//             return surRender.getSegmentationImage();
//         } else {
            return null;
//         }
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
//         return surRender.getGeodesicPanel().isGeodesicEnable();
        return false;
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
                m_kLightsPanel.refreshLighting();
            } else if (radioSURFACEFAST.isSelected() && (source == radioSURFACEFAST)) {
                raycastRenderWM.SURFASTMode();
                updateRayTracingSteps();
                m_kLightsPanel.refreshLighting();
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
        //Object source = e.getSource();
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
     * Required by the parent super class, do nothing.
     */
    public void removeControls() { }

    /**
     * Remove the red line showing where the probe will pass through. Used when changing the probe target point through
     * the tri-images.
     */
    public void removeProbeLine() {

//         if (surRender != null) {
//             surRender.getProbeDialog().removeProbingPath();
//         }
    }

    /**
     * Reset image volume orieint along X axis.
     */
    public void resetAxisX() {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.resetAxisX();
        }
    }

    /**
     * Reset image volume orieint along Y axis.
     */
    public void resetAxisY() {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.resetAxisY();
        }
    }

    /**
     * Reset image volume orieint along Z axis.
     */
    public void resetImage() {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.resetAxis();
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

//         if (surRender != null) {
//             surRender.setLUTa(LUT);
//         }

//         if (raycastRenderWM != null) {
//             raycastRenderWM.setLUTa(LUT);
//         }
    }

    /**
     * Accessor that sets the LUT.
     *
     * @param  LUT  the LUT
     */
    public void setLUTb(ModelLUT LUT) {

//         if (surRender != null) {
//             surRender.setLUTb(LUT);
//         }

//         if (raycastRenderWM != null) {
//             raycastRenderWM.setLUTb(LUT);
//         }
    }

    /**
     * Set material ( texture or voxels ) shininess value.
     *
     * @param  value  float
     */
    public void setMaterialShininess(float value) {

//         if (raycastRenderWM != null) {
//             raycastRenderWM.setMaterialShininess(value);
//         }

//         if (surRender != null) {
//             surRender.setMaterialShininess(value);
//         }
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
//    public void setPathPosition(Point3f kPosition, Point3f kScaledPosition) {
//        Vector3f kCenter = new Vector3f(kPosition.x * imageA.getExtents()[0], kPosition.y * imageA.getExtents()[1],
//                                        kPosition.z * imageA.getExtents()[2]);

//         for (int iPlane = 0; iPlane < 3; iPlane++) {
//             m_akPlaneRender[iPlane].setCenter(kCenter);
//         }

//         surRender.setCenter(kCenter);
//         surRender.getSurfaceDialog().setPathPosition(kScaledPosition);

//    }

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
     * Sets the RGB table for ARGB image A.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTA(ModelRGB RGBT) {
//         if (surRender != null) {
//             surRender.setRGBTA(RGBT);
//         }

        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.SetRGBT(RGBT, 0);
        }
    }

    /**
     * Sets the RGB table for image B.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTB(ModelRGB RGBT) {
//         if (surRender != null) {
//             surRender.setRGBTB(RGBT);
//         }

        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.SetRGBT(RGBT, 1);
        }
    }

    /**
     * Set the image which we can check to see if the probe is hitting anything important (such as vessels, etc).
     *
     * @param  img  segmentation image
     */
    public void setSegmentationImage(ModelImage img) {

//         if (surRender != null) {
//             surRender.setSegmentationImage(img);
//         }
    }

    /**
     * Required by the parent super class, do nothing.
     *
     * @param  slice  int
     */
    public void setSlice(int slice) { }


    public void setSliceOpacity( int i, float fAlpha )
    {
        raycastRenderWM.SetSliceOpacity( i, fAlpha );
    }


    /**
     * Sets the position of the slices in the SurfaceRender and PlaneRender objects. Called from the PlaneRender class.
     *
     * @param  center  the new slice positions in FileCoordinates
     */
    public void setSliceFromPlane(Vector3f center) {
        setPositionLabels(center);

         for (int i = 0; i < 3; i++) {
             m_akPlaneRender[i].setCenter(center);
         }

         raycastRenderWM.SetCenter( center );
         sliceGUI.setCenter((int)center.X, (int)center.Y, (int)center.Z);
    }

    /**
     * Sets the position of the slices in the PlaneRender. Called from the SurfaceRender class.
     *
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
        raycastRenderWM.SetCenter( new Vector3f( center.X, center.Y, center.Z ) );
    }


    public void showSlice( int i, boolean bShow )
    {
        raycastRenderWM.ShowSlice( i, bShow );
    }

    public void showBoundingBox( int i, boolean bShow )
    {
        raycastRenderWM.ShowBoundingBox( i, bShow );
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

        for (int iPlane = 0; iPlane < 3; iPlane++) {
            m_akPlaneRender[iPlane].
                setSliceHairColor(iView, kColor );
        }
        raycastRenderWM.SetBoundingBoxColor( iView, kColor);
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
        Object source = event.getSource();
        if ( source == m_kVolumeBlendSlider )
        {
            if (raycastRenderWM != null)
            {
                raycastRenderWM.setVolumeBlend( m_kVolumeBlendSlider.getValue()/100.0f );
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

//         for (int i = 0; i < 3; i++) {

//             if (m_akPlaneRender[i] != null) {
//                 m_akPlaneRender[i].updateData();
//             }
//         }

//         if (surRender != null) {
//             surRender.updateImages();
//         }

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

//         for (int i = 0; i < 3; i++) {

//             if (m_akPlaneRender[i] != null) {
//                 m_akPlaneRender[i].updateData();
//             }
//         }

//         if (surRender != null) {
//             surRender.updateImages(forceShow);
//         }

        if (m_kVolumeImageA != null) {
            ViewJComponentVolOpacityBase kSelectedComp = m_kVolOpacityPanel.getSelectedComponent();
            if ( imageB != null )
            {
                if ( kSelectedComp == m_kVolOpacityPanel.getCompA() )
                {
                    TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
                    m_kVolumeImageA.UpdateImages(kTransfer, 0);
                }
                /*
                else
                {
                    TransferFunction kTransfer = m_kVolOpacityPanel.getCompB().getOpacityTransferFunction();
                    raycastRenderWM.updateImages(1, kTransfer);
                }
                */
            }
            else
            {
                if ( kSelectedComp == m_kVolOpacityPanel.getCompA() )
                {
                    TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
                    m_kVolumeImageA.UpdateImages(kTransfer, 0);
                    
                }
                else
                {
                    TransferFunction kTransfer = m_kVolOpacityPanel.getCompA_GM().getOpacityTransferFunction();
                    m_kVolumeImageA.UpdateImages(kTransfer, 2);
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
//         if (surRender != null) {
//             surRender.updateImages(LUTa, LUTb, forceShow, interpMode);
//         }

        if (m_kVolumeImageA != null) {
            m_kVolumeImageA.UpdateImages(LUTa, LUTb);    
            setModified();
        }


        return true;
    }

    /**
     * The navigation mode update the probe position in 3D texture volume. Not used now. Might be used later on.
     */
    public void updateProbePos() {

//         if (surRender != null) {
//             surRender.updateProbePos();
//         }
    }

    /**
     * Causes the PlaneRender objects to update the texture maps when the underlying ModelImage changes.
     */
    public void updateSliceData() {

//         for (int iPlane = 0; iPlane < 3; iPlane++) {
//             m_akPlaneRender[iPlane].updateData();
//         }
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

//         if (surRender != null) {
//             surRender.setDisplayMode3D(flag);
//         }
    }


    /**
     * Repaint the volume.
     */
    public void volumeRepaint() {
//         surRender.updateVolume(LUTa, LUTb, false);
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
     * Tells the UI that this frame is the currently active one.
     *
     * @param  event  the window event
     */
    public void windowActivated(WindowEvent event) {
        setModified();
        super.windowActivated(event);
    }

    public void setModified()
    {
        if ( m_akPlaneRender != null )
        {
            for (int i = 0; i < 3; i++) {
                m_akPlaneRender[i].SetModified(true);
            }
        }
    }
    
    /**
     * Does nothing.
     *
     * @param  event  the component event
     */
    public void componentMoved(ComponentEvent event)
    {       
        setModified();
    }
    

    public void setRadiological( boolean bOn )
    {
        imageA.setRadiologicalView(bOn);

        if (imageB != null) {
            imageB.setRadiologicalView(bOn);
        }
        Vector3f center = sliceGUI.getCenter();
        raycastRenderWM.SetCenter( new Vector3f( center.X, center.Y, center.Z ) );    
        for (int i = 0; i < 3; i++) {
            m_akPlaneRender[i].setRadiologicalView(bOn);
            m_akPlaneRender[i].setCenter(center);
        }
        setPositionLabels(center);
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
                                         menuObj.buildCheckBoxMenuItem("RFA toolbar", "RFAToolbar", false)
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

        /** Indicates that image orientation is unknown type or not. */
        boolean axialOrientation = true;
        if (imageOrientation == FileInfoBase.UNKNOWN_ORIENT) {
            axialOrientation = false;
        } else {
            axialOrientation = true;
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

        buildLabelPanel();
        if ( bVolumeViewer )
        {
            buildHistoLUTPanel();
            buildOpacityPanel();
        }

        buildDisplayPanel();
        buildLightPanel();
        buildClipPanel();
        buildSlicePanel();
        buildSurfacePanel();
        buildSurfaceTexturePanel();
        buildProbePanel();
        buildGeodesic();
        buildSculpt();

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

        //imagePanel = new JPanel(new BorderLayout());
        gpuPanel = new JPanel(new BorderLayout());

        setLocation(100, 100);

//         if (leftPanelRenderMode == SURFACE) {
//             surfaceRenderPanel.add(surRender.getCanvas(), BorderLayout.CENTER);
        gpuPanel.add(raycastRenderWM.GetCanvas(), BorderLayout.CENTER);
//             imagePanel.add(surfaceRenderPanel, BorderLayout.EAST);
//             surfaceRenderPanel.setVisible(true);
        //imagePanel.add(gpuPanel, BorderLayout.WEST);
        gpuPanel.setVisible(true);
        raycastRenderWM.setVisible(false);
//         }

        int imagePanelWidth = (int) (screenWidth * 0.51f);
        int imagePanelHeight = (int) (screenHeight * 0.43f);

        //imagePanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        //imagePanel.setMinimumSize(new Dimension(500, 500));
        //imagePanel.setBorder(compound);

        gpuPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        gpuPanel.setMinimumSize(new Dimension(250, 250));
        
        bf_flyPanel = new JPanel(new BorderLayout());
        bf_flyPanel.setBorder(compound);
        
        dualPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, gpuPanel, bf_flyPanel);
        dualPane.setOneTouchExpandable(false);
        dualPane.setDividerSize(6);
        dualPane.setContinuousLayout(true);
        dualPane.setResizeWeight(1);
        
        rightPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, dualPane, triImagePanel);
        rightPane.setOneTouchExpandable(true);
        rightPane.setDividerSize(6);
        rightPane.setContinuousLayout(true);
        //rightPane.setResizeWeight(0.75);

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

        // initialize the sculptor region.
        sculptWidth = imagePanelWidth - (2 * getInsets().left);
        sculptHeight = imagePanelHeight - getInsets().top - getInsets().bottom;

//         surRender.getSculptorPanel().setFrameSize(sculptWidth, sculptHeight);
        enableVolumeRender();
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
    protected void addToolbar() {
        etchedBorder = BorderFactory.createEtchedBorder();
        toolbarBuilder = new ViewToolBarBuilder(this);
        buildViewToolbar();

//        if (isSurfaceRenderEnable) {
            buildSurRenderToolbar();
//        }

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 1;
//         gbc.gridwidth = 1;
//         gbc.gridheight = 1;
//         gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
//         gbc.weightx = 1;
//         gbc.weighty = 1;

 //       if (leftPanelRenderMode == SURFACE) {
        panelToolbar.add(surfaceToolBar, gbc);
 //       }
    }

    /**
     * Build the surface render toolbar.
     */
    protected void buildSurRenderToolbar() {
        surfaceToolBar = new JToolBar();
        surfaceToolBar.setBorder(etchedBorder);
        surfaceToolBar.setBorderPainted(true);
        surfaceToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        surfaceToolBar.setFloatable(false);
        surfaceToolBar.add(toolbarBuilder.buildButton("SurfaceDialog", "Add surface to viewer", "isosurface"));
        surfaceToolBar.add(toolbarBuilder.buildButton("Geodesic", "Draw geodesic curves on the surface", "geodesic"));
        surfaceToolBar.add(ViewToolBarBuilder.makeSeparator());
        surfaceToolBar.add(toolbarBuilder.buildButton("Box", "Display options", "perspective"));
        surfaceToolBar.add(ViewToolBarBuilder.makeSeparator());

        ButtonGroup group1 = new ButtonGroup();

        radioMIP = new JRadioButton("MIP", false);
        radioMIP.setFont(serif12);
        group1.add(radioMIP);
        radioXRAY = new JRadioButton("DRR", false);
        radioXRAY.setFont(serif12);
        group1.add(radioXRAY);
        radioCOMPOSITE = new JRadioButton("Composite", true);
        radioCOMPOSITE.setFont(serif12);
        group1.add(radioCOMPOSITE);
        radioSURFACEFAST = new JRadioButton("Surface", false);
        radioSURFACEFAST.setFont(serif12);
        group1.add(radioSURFACEFAST);
        radioSURFACE = new JRadioButton("Composite Surface", false);
        radioSURFACE.setFont(serif12);
        group1.add(radioSURFACE);

        radioMIP.addItemListener(this);
        radioXRAY.addItemListener(this);
        radioCOMPOSITE.addItemListener(this);
        //radioCOMPOSITE.setSelected(true);
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

        surfaceToolBar.add(ViewToolBarBuilder.makeSeparator());
        JLabel kBlendLabel = new JLabel("Volume Blend" );
        surfaceToolBar.add(kBlendLabel);
        m_kVolumeBlendSlider = new JSlider( 0, 100, 75 );
        m_kVolumeBlendSlider.addChangeListener(this);
        surfaceToolBar.add(m_kVolumeBlendSlider);
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

        m_kDisplayVolumeCheck = new JCheckBox( "Display RayCast Volume" );
        m_kDisplayVolumeCheck.setSelected(false);
        m_kDisplayVolumeCheck.setActionCommand( "VolumeRayCast");
        m_kDisplayVolumeCheck.addActionListener(this);
        viewToolBar.add(m_kDisplayVolumeCheck);

        m_kDisplaySlicesCheck = new JCheckBox( "Display Slices" );
        m_kDisplaySlicesCheck.setSelected(true);
        m_kDisplaySlicesCheck.setActionCommand( "VolumeSlices");
        m_kDisplaySlicesCheck.addActionListener(this);
        viewToolBar.add(m_kDisplaySlicesCheck);
        
        m_kDisplaySurfaceCheck = new JCheckBox( "Display Surface" );
        m_kDisplaySurfaceCheck.setSelected(false);
        m_kDisplaySurfaceCheck.setEnabled(false);
        m_kDisplaySurfaceCheck.setActionCommand( "Surface");
        m_kDisplaySurfaceCheck.addActionListener(this);
        viewToolBar.add(m_kDisplaySurfaceCheck);
        
        
        m_kStereoCheck = new JCheckBox( "Stereo" );
        m_kStereoCheck.setSelected(false);
        m_kStereoCheck.setEnabled(true);
        m_kStereoCheck.setActionCommand( "Stereo");
        m_kStereoCheck.addActionListener(this);
        viewToolBar.add(m_kStereoCheck);
        
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
        viewToolBar.add(toolbarBuilder.buildButton("Extract", "Extract rotated image", "imageextract"));
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

    }

    /**
     * Enable volume render.
     */
    protected void enableVolumeRender() {
        if ( m_bSurfaceVisible )
        {
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
        //surfaceTextureGUI.resizePanel(maxPanelWidth, height);
        displayGUI.resizePanel(maxPanelWidth, height);
        geodesicGUI.resizePanel(maxPanelWidth, height);
        m_kLightsPanel.resizePanel(maxPanelWidth, height);
        ((JPanelClip_WM)clipBox).resizePanel(maxPanelWidth, height);
        
        if ( brainsurfaceFlattenerRender != null )
        {
            brainsurfaceFlattenerRender.resizePanel(maxPanelWidth, height);
        }
    }

    /**
     * Sets the 3DModel position label.
     *
     * @param  position  DOCUMENT ME!
     */
    protected void set3DModelPosition(Vector3f position) {

        float fMaxX = (m_kVolumeImageA.GetImage().getExtents()[0] - 1) * m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[0];
        float fMaxY = (float) (m_kVolumeImageA.GetImage().getExtents()[1] - 1) * m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[1];
        float fMaxZ = (float) (m_kVolumeImageA.GetImage().getExtents()[2] - 1) * m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[2];

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
     *
     * @param  position  DOCUMENT ME!
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

    public void setGradientMagnitude( boolean bShow )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.SetGradientMagnitude(bShow);
            TransferFunction kTransfer = m_kVolOpacityPanel.getCompA_GM().getOpacityTransferFunction();
            m_kVolumeImageA.UpdateImages(kTransfer, 2);
        }
    }
    
    public void addPolyline(Polyline akPolyline, int groupIndex)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.addPolyline(akPolyline, groupIndex);
        }
    }    
    
    public void removePolyline(int groupIndex)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.removePolyline(groupIndex);
        }
    }
    
    public void addSurface(TriMesh[] akSurfaces, boolean bReplace)
    {
        if ( raycastRenderWM != null )
        {
            VolumeSurface[] akVolumeSurfaces = raycastRenderWM.addSurface(akSurfaces, bReplace);  
            for (int i = 0; i < 3; i++) 
            {
                m_akPlaneRender[i].AddSurfaces(akVolumeSurfaces);
            }
            insertTab("Light", lightPanel);
            m_kLightsPanel.enableLight(0, true);
            insertTab("Surface", surfacePanel);
            m_kDisplaySurfaceCheck.setSelected(true);
            m_kDisplaySurfaceCheck.setEnabled(true);
            raycastRenderWM.DisplaySurface(true);
            
            menuObj.setMenuItemEnabled("Open BrainSurface Flattener view", true);  
            menuObj.setMenuItemEnabled("Open Fly Through view", true);       
        }
        if ( geodesicGUI != null )
        {
            geodesicGUI.setEnabled(true);
            geodesicGUI.setSurfacePanel(surfaceGUI);
        }
    }  
    
    public void addGeometry( Geometry kGeometry )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.addGeometry(kGeometry);
        }
    }

    public void AddSlices(VolumeSlices kSlices)
    {
        for (int i = 0; i < 3; i++) 
        {
            m_akPlaneRender[i].AddSlices(kSlices);
        }
    }
    
    public void updateLighting()
    {
        insertTab("Light", lightPanel);
        m_kLightsPanel.enableLight(0, true);
    }

    public void removeSurface(String kSurfaceName)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.removeSurface(kSurfaceName);
        }
    }

    public void translateSurface(String kSurfaceName, Vector3f kTranslate)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.translateSurface(kSurfaceName, kTranslate);
        }
    }

    public void setPolygonMode(String kSurfaceName, WireframeState.FillMode eMode)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.setPolygonMode(kSurfaceName, eMode );
        }
    }
    

    public void setTransparency(String kSurfaceName, float fValue)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.Blend(kSurfaceName, fValue );
        }
    }    

    public void setPerPixelLighting(String kSurfaceName, boolean bOn)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.setPerPixelLighting(kSurfaceName, bOn );
        }
    }
    
    public void setClipping(String kSurfaceName, boolean bClip)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.setClipping(kSurfaceName, bClip );
        }
    }    
    
    public void setBackface(String kSurfaceName, boolean bOn)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.setBackface(kSurfaceName, bOn );
        }
    }    
    
    public void setPickable(String kSurfaceName, boolean bOn)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.setPickable(kSurfaceName, bOn );
        }
    }
    
    
    public void setSurfaceTexture(String kSurfaceName, boolean bOn, boolean bUseNewImage, boolean bUseNewLUT)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.setSurfaceTexture(kSurfaceName, bOn, bUseNewImage, bUseNewLUT );
        }
    }

    public void setColor(String kSurfaceName, ColorRGB kColor)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.setColor(kSurfaceName, kColor);
        }
    }    

    public float getVolume(String kSurfaceName)
    {
        if ( raycastRenderWM != null )
        {
            return raycastRenderWM.GetVolume(kSurfaceName);
        }
        return 0;
    }

    public float getSurfaceArea(String kSurfaceName)
    {
        if ( raycastRenderWM != null )
        {
            return raycastRenderWM.GetSurfaceArea(kSurfaceName);
        }
        return 0;
    }
    
    public MaterialState getMaterial(String kSurfaceName)
    {
        if ( raycastRenderWM != null )
        {
            return raycastRenderWM.GetMaterial(kSurfaceName);
        }
        return null;
    }
    
    public void setMaterial(String kSurfaceName, MaterialState kMaterial)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.SetMaterial(kSurfaceName, kMaterial);
        }
    }
    
    
    public Light[] GetLights()
    {
        if ( raycastRenderWM != null )
        {
            return raycastRenderWM.GetLights();
        }
        return null;
    }
    
    public Animator GetAnimator()
    {
        return m_kAnimator;
    }
    
    public void enablePaint( ColorRGBA kPaintColor, int iBrushSize, boolean bEnabled, boolean bPaint, boolean bDropper, boolean bPaintCan, boolean bErase )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.enablePaint(kPaintColor, iBrushSize, bEnabled, bPaint, bDropper, bPaintCan, bErase);
        }
    }
  
    public void eraseAllPaint( )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.eraseAllPaint();
        }
    }
    
    public void setDropperColor( ColorRGBA kDropperColor, Vector3f kPickPoint )
    {
        if ( surfaceGUI != null )
        {
            surfaceGUI.setDropperColor(kDropperColor, kPickPoint);
        }
    }
    
    
    public void SetLUTNew( String kSurfaceName, ModelLUT kLUT, ModelRGB kRGBT )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.SetLUTNew(kSurfaceName, kLUT, kRGBT);
        }
    }    
    
    public void SetImageNew(  String kSurfaceName, ModelImage kImage )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.SetImageNew(kSurfaceName, kImage);
        }
    }
    
    
    public void enableGeodesic( boolean bEnable )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.enableGeodesic(bEnable);
        }
    }
    
    public void setGeodesic( TriMesh kMesh, PickRecord kPickPoint )
    {
        if ( geodesicGUI != null )
        {
            geodesicGUI.setPickedPoint( kPickPoint, kMesh );
        }
    }
    
    public void addGeodesic( TriMesh kSurface, Geometry kNew, int iGroup )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.addGeodesic(kSurface, kNew, iGroup);
        }
    }   
    
    public void removeGeodesic( TriMesh kSurface, int iNode, int iGroup )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.removeGeodesic(kSurface, iNode, iGroup);
        }
    }    
    
    public void removeAllGeodesic( TriMesh kSurface )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.removeAllGeodesic(kSurface);
        }
    }
    
    public void replaceGeodesic(TriMesh kOld, TriMesh kNew) {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.replaceGeodesic(kOld, kNew);
        }
    }

    public void toggleGeodesicPathDisplay(String kSurfaceName, int iWhich)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.toggleGeodesicPathDisplay(kSurfaceName, iWhich);
        }
    }
    
    public void smoothMesh( String kSurfaceName, int iteration, float alpha, boolean volumeLimit, float volumePercent)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.smoothMesh(kSurfaceName, iteration, alpha, volumeLimit, volumePercent);
        }
    }
    
    
    public void smoothTwo( String kSurfaceName, int iteration, float fStiffness, boolean volumeLimit, float volumePercent)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.smoothTwo(kSurfaceName, iteration, fStiffness, volumeLimit, volumePercent);
        }
    }
    
    
    
    public void smoothThree( String kSurfaceName, int iteration, float lambda, float mu)
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.smoothThree(kSurfaceName, iteration, lambda, mu);
        }
    }

    public void setIPD( float fIPD )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.setIPD(fIPD);
        }
    }

    public void updateLighting( Light[] akGLights )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.updateLighting(akGLights);
            for (int i = 0; i < 3; i++) 
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
    
    public void addNode( Node kNode )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.addNode( kNode );
        }
    }
    
    public void toggleNode( Node kNode, boolean bDisplay )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.DisplayNode(kNode, bDisplay);
        }
    }
    
    public void PickCorrespondence( boolean bOn )
    {
        if ( raycastRenderWM != null )
        {
            raycastRenderWM.PickCorrespondence(bOn);
        }        
    }
    
    
    public void PickCorrespondence( int iV0, int iV1, int iV2 )
    {
        if ( m_kBrainsurfaceFlattenerPanel != null )
        {
            brainsurfaceFlattenerRender.drawPicked(iV0, iV1, iV2);
        }        
    }
    
    protected String getExternalDirs()
	{
		String jar_filename = "";
		String class_path_key = "java.class.path";
		String class_path = System.getProperty(class_path_key);
		for (String fn : class_path.split(":")) {
			if (fn.endsWith("WildMagic.jar")) {
				jar_filename = fn;
				String externalDirs = jar_filename.substring(0, jar_filename.indexOf("lib"));
				externalDirs = externalDirs.concat("WildMagic");
				System.err.println("Shader dir found: " + externalDirs);
				return externalDirs;
			}
		}
		System.err.println("Shader dir not found");
		return System.getProperties().getProperty("user.dir");
	}
    
    /**
	 * Set the camera parameters for displaying.
	 */
    public void setCameraParameters() {
    	displayGUI.displayCameraParams(raycastRenderWM.getCameraParameters());
    }

    /**
     * Set the object rotation parameters for displaying. 
     */
    public void setObjectParameters() {
    	displayGUI.displayObjectParams(raycastRenderWM.getObjectParameters());
    }
    
    /**
     * Get the camera parameters. 
     * @return  camera parameters array. 
     */
    public float[] getCameraParameters() {
    	return raycastRenderWM.getCameraParameters();
    }
    
    /**
     * Get the object parameters.
     * @return Get the object rotation parameters array.
     */
    public float[] getObjectParameters() {
    	return raycastRenderWM.getObjectParameters();
    }
    
    /**
     * Get the camera current location 
     * @return camera position vector
     */
    public Vector3f getCameraLocation() {
    	return raycastRenderWM.getCameraLocation();
    }
    
    /**
     * Get the object rotation matrix.
     * @return rotation matrix
     */
    public Matrix3f getObjectRotation() {
    	return raycastRenderWM.getObjectRotation();
    }
    
    /**
     * Set the camera location.
     * @param v  camera position vector
     */
    public void setCameraLocation(Vector3f v) {
    	raycastRenderWM.setCameraLocation(v);
    }
    
    /**
     * Set the object rotation matrix
     * @param rot  rotation matrix
     */
    public void setObjectRotation(Matrix3f rot) {
    	raycastRenderWM.setObjectRotation(rot);
    }
    
}
