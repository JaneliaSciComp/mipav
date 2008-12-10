package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.dialogs.JDialogSmoothMesh;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Decimate.TriangleMesh;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Hashtable;
import java.util.Random;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Detail.ClodMesh;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;


public class JPanelRenderMode_WM extends JInterfaceBase
        implements ItemListener, ChangeListener {
	
	
	/** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;
    
    /** Turn display volume on/off */
    protected JCheckBox m_kDisplayVolumeCheck;
    
    /** Turn display 3D Slices on/off */
    protected JCheckBox m_kDisplaySlicesCheck;
    
    /** Turn display 3D TriMesh Surface on/off */
    protected JCheckBox m_kDisplaySurfaceCheck;
    
    /** Turn display 3D Stereo on/off */
    protected JCheckBox m_kStereoCheck;
    
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
    
    /** Radio button of the XRAY mode option. */
    protected JRadioButton radioXRAY;
    
    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;
    
    //** Check box to enable/disable surface self-shadowing */
    private JCheckBox kSelfShadow;
    
    /** Volume alpha-blending slider. */
    protected JSlider m_kVolumeBlendSlider;
    
    /** Button for extracting a TriMesh surface based on the ray-cast volume rendered in Surface mode */
    protected JButton m_kExtractTriMesh;
    
	/**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanelRenderMode_WM( VolumeTriPlanarInterface kVolumeViewer )
    {
        super(kVolumeViewer);
        init();
    }
	
    /**
     * Initializes the GUI components.
     */
    private void init() {
    	
    	 serif12 = MipavUtil.font12;
    	
    	 JPanel mainScrollPanel = new JPanel();
         mainScrollPanel.setLayout(new BorderLayout());

         scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                    JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

         mainPanel = new JPanel(new BorderLayout());

         JPanel componentsPanel = new JPanel(new GridBagLayout());
         componentsPanel.setBorder(buildTitledBorder("Display Components"));
         
         GridBagConstraints gbc = new GridBagConstraints();

         gbc.weightx = 1;
         gbc.anchor = GridBagConstraints.WEST;
         gbc.insets = new Insets(5, 5, 5, 5);
       
         m_kDisplayVolumeCheck = new JCheckBox( "Display RayCast Volume" );
         m_kDisplayVolumeCheck.setSelected(false);
         m_kDisplayVolumeCheck.setActionCommand( "VolumeRayCast");
         m_kDisplayVolumeCheck.addActionListener(this);
         componentsPanel.add(m_kDisplayVolumeCheck);
         componentsPanel.add(m_kDisplayVolumeCheck, gbc);
         

         m_kDisplaySlicesCheck = new JCheckBox( "Display Slices" );
         m_kDisplaySlicesCheck.setSelected(true);
         m_kDisplaySlicesCheck.setActionCommand( "VolumeSlices");
         m_kDisplaySlicesCheck.addActionListener(this);
         gbc.gridy = 1;
         componentsPanel.add(m_kDisplaySlicesCheck, gbc);
         
         m_kDisplaySurfaceCheck = new JCheckBox( "Display Surface" );
         m_kDisplaySurfaceCheck.setSelected(false);
         m_kDisplaySurfaceCheck.setEnabled(false);
         m_kDisplaySurfaceCheck.setActionCommand( "Surface");
         m_kDisplaySurfaceCheck.addActionListener(this);
         gbc.gridy = 2;
         componentsPanel.add(m_kDisplaySurfaceCheck, gbc);
         
         m_kStereoCheck = new JCheckBox( "Stereo" );
         m_kStereoCheck.setSelected(false);
         m_kStereoCheck.setEnabled(true);
         m_kStereoCheck.setActionCommand( "Stereo");
         m_kStereoCheck.addActionListener(this);
         gbc.gridy = 3;
         componentsPanel.add(m_kStereoCheck, gbc);
         
         kSelfShadow = new JCheckBox("Self Shadow", false);
         kSelfShadow.setFont(MipavUtil.font12);
         kSelfShadow.addItemListener(this);
         kSelfShadow.setEnabled(false);
         gbc.gridy = 4;
         componentsPanel.add(kSelfShadow, gbc);
    
         JPanel renderModePanel = new JPanel(new GridBagLayout());
         renderModePanel.setBorder(buildTitledBorder("Render Mode"));
         
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
         radioSURFACE.addItemListener(this);
         radioSURFACEFAST.addItemListener(this);
         gbc.gridy = 0;
         renderModePanel.add(radioMIP, gbc);
         gbc.gridy = 1;
         renderModePanel.add(radioXRAY, gbc);
         gbc.gridy = 2;
         renderModePanel.add(radioCOMPOSITE, gbc);
         gbc.gridy = 3;
         renderModePanel.add(radioSURFACEFAST, gbc);
         gbc.gridy = 4;
         renderModePanel.add(radioSURFACE, gbc);
         
         JPanel blendPanel = new JPanel();
         blendPanel.setLayout(new BoxLayout(blendPanel, BoxLayout.X_AXIS));
         blendPanel.setBorder(buildTitledBorder("Blend"));
         
         JLabel kBlendLabel = new JLabel("Volume Blend" );
         blendPanel.add(kBlendLabel);
         m_kVolumeBlendSlider = new JSlider( 0, 100, 100 );
         m_kVolumeBlendSlider.addChangeListener(this);
         blendPanel.add(m_kVolumeBlendSlider);
         
         JButton kShaderButton = new JButton( "Shader Parameters" );
         kShaderButton.addActionListener(this);
         kShaderButton.setActionCommand("ShaderParameters");
         kShaderButton.setToolTipText("Open Shader Dialig");
         kShaderButton.setBorderPainted(true);
         kShaderButton.setFocusPainted(true);
         kShaderButton.setMargin(new Insets(0, 0, 0, 0));
         
         JPanel buttonPanel = new JPanel(new GridBagLayout());
         buttonPanel.setBorder(buildTitledBorder("Advanced shader parameters"));
         gbc.gridy = 0;
         buttonPanel.add(kShaderButton, gbc);
         
         m_kExtractTriMesh = new JButton( "Extract Mesh from Volume" );
         m_kExtractTriMesh.addActionListener(this);
         m_kExtractTriMesh.setActionCommand("ExtractMeshFromVolume");
         m_kExtractTriMesh.setToolTipText("Extract and load a Triangle Mesh based on the Volume in Surface Mode");
         m_kExtractTriMesh.setBorderPainted(true);
         m_kExtractTriMesh.setFocusPainted(true);
         m_kExtractTriMesh.setMargin(new Insets(0, 0, 0, 0));
         m_kExtractTriMesh.setEnabled(false);
         
         JPanel extractPanel = new JPanel(new GridBagLayout());
         extractPanel.setBorder(buildTitledBorder("Surface Extraction"));
         gbc.gridy = 0;
         extractPanel.add(m_kExtractTriMesh, gbc);
         
         Box contentBox = new Box(BoxLayout.Y_AXIS);

         contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
         contentBox.add(componentsPanel);
         contentBox.add(renderModePanel);
         contentBox.add(blendPanel);
         contentBox.add(extractPanel);
         contentBox.add(buttonPanel);
  
         mainScrollPanel.add(contentBox, BorderLayout.NORTH);
         
         mainPanel.add(scroller, BorderLayout.CENTER);
    }
    
    /**
     * Set the surface check box to true of false
     * @param flag  true or false
     */
    public void setDisplaySurfaceCheck(boolean flag ) {
    	m_kDisplaySurfaceCheck.setSelected(flag);
    	m_kDisplaySurfaceCheck.setEnabled(flag);
    }
    
    
    /**
     * Set the volume check box to true of false
     * @param flag  true or false
     */
    public void setDisplayVolumeCheck(boolean flag ) {
        m_kDisplayVolumeCheck.setSelected(flag);
    }
    
    /**
     * Get the volume display checkbox.
     * @return  true or false
     */
    public JCheckBox getVolumeCheck()  {
    	return m_kDisplayVolumeCheck;
    }
    
    /**
     * Get the stereo checkbox. 
     * @return  true or false
     */
    public JCheckBox getStereoCheck() {
    	return m_kStereoCheck;
    }
    
    /**
     * Get the surface check box. 
     * @return true or false
     */
    public JCheckBox getSurfaceCheck() {
    	return m_kDisplaySurfaceCheck;
    }
    
    /**
     * Get the slice check box. 
     * @return true or false
     */
    public JCheckBox getSlicesCheck() {
    	return m_kDisplaySlicesCheck;
    }
    
    /**
     * Empty function call
     */
    public void actionPerformed(ActionEvent event) {
    	m_kVolumeViewer.actionPerformed(event);
    	
    }
    
    /**
     * Get the blender slider value
     * @return   slider value.
     */
    public int getBlendSliderValue() {
    	return m_kVolumeBlendSlider.getValue();
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#stateChanged(javax.swing.event.ChangeEvent)
     */
    public void stateChanged(ChangeEvent event) {
        Object source = event.getSource();
        if ( source == m_kVolumeBlendSlider )
        {
        	rayBasedRenderWM.setVolumeBlend( m_kVolumeBlendSlider.getValue()/100.0f );
        }
    }
    
    /* (non-Javadoc)
     * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (radioMIP.isSelected() && (source == radioMIP)) {
        	rayBasedRenderWM.MIPMode();
            m_kVolumeViewer.updateRayTracingSteps();
        } else if (radioXRAY.isSelected() && (source == radioXRAY)) {
        	rayBasedRenderWM.DDRMode();
            m_kVolumeViewer.updateRayTracingSteps();
        } else if (radioCOMPOSITE.isSelected() && (source == radioCOMPOSITE)) {
        	rayBasedRenderWM.CMPMode();
            m_kVolumeViewer.updateRayTracingSteps();
        } else if (radioSURFACE.isSelected() && (source == radioSURFACE)) {
        	rayBasedRenderWM.SURMode();
            m_kVolumeViewer.updateRayTracingSteps();
            m_kVolumeViewer.refreshLighting();
        } else if (radioSURFACEFAST.isSelected() && (source == radioSURFACEFAST)) {
        	rayBasedRenderWM.SURFASTMode();
            m_kVolumeViewer.updateRayTracingSteps();
            m_kVolumeViewer.refreshLighting();
        } else if (radioSURFACEFAST.isSelected() && (source == kSelfShadow) )
        	rayBasedRenderWM.selfShadow( kSelfShadow.isSelected() );
        	m_kVolumeViewer.updateRayTracingSteps();
        if ( (m_kVolumeViewer.getImageB() == null) )
        {
            kSelfShadow.setEnabled(radioSURFACEFAST.isSelected());
        }
        m_kExtractTriMesh.setEnabled(radioSURFACEFAST.isSelected());
    }
    
    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   int width
     * @param  frameHeight  int height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.setSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.revalidate();
    }
    
    /**
     * Builds a titled border with the given title, an etched border, and the
     * proper font and color.  Changed to public static member so that it can
     * be used for other JPanels not inherited from this base class.
     * @param   title  Title of the border
     *
     * @return  The titled border.
     */
    public static TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                                Color.black);
    }
    
}