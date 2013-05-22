package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


public class JPanelRenderMode_WM extends JInterfaceBase
        implements ItemListener, ChangeListener {
	
	
	/**  */
    private static final long serialVersionUID = 3015333092796701354L;

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
    /** Text box for setting the intensity level for extraction. */
    JTextField m_kIntensityTF;
    
    /** Turn display volume on/off */
    protected JCheckBox m_kDisplayVolumeCheck;
    
    /** Turn display 3D Slices on/off */
    protected JCheckBox m_kDisplaySlicesCheck;

    /** Turn display 3D TriMesh Surface on/off */
    protected JCheckBox m_kDisplaySurfaceCheck;
        
    /** The combo box for the polygon mode to display. */
    private JComboBox m_kStereoModeCB;

    /** Radio button of the COMPOSITE mode option. */
    protected JRadioButton radioCOMPOSITE;

    /** Radio button of the MIP mode option. */
    protected JRadioButton radioMIP;

    /** Radio button of the Custom blend mode option. */
    protected JRadioButton radioCustom;

    /** Radio button of the SURFACE mode option. */
    protected JRadioButton radioSURFACE;

    /** Radio button of the SURFACE mode option. */
    protected JRadioButton radioSURFACEFAST;
    
    /** Radio button of the surface render composite mode. */
    protected JRadioButton radioSurrenderCOMPOSITE;

    /** Radio button of the XRAY mode option. */
    protected JRadioButton radioXRAY;
    
    /** Checkbox for the Multi-histo mode option. */
    protected JCheckBox radioMULTIHISTO;

    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;
    
    /** Volume alpha-blending slider. */
    protected JSlider m_kVolumeBlendSlider;
    
    /** Volume number of samples slider when mouse released. */
    protected JSlider m_kVolumeSamplesSliderMouseReleased;
    
    /** Volume number of samples slider when mouse Dragged. */
    protected JSlider m_kVolumeSamplesSliderMouseDragged;
    
    /** Button for extracting a TriMesh surface based on the ray-cast volume rendered in Surface mode */
    protected JButton m_kExtractTriMesh;
    
    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;
    	
    /** Label that gives current value of slider. */
    private JLabel mkCurrent;
    
    /** IPD number. */
    private float m_fIPD;
    
    /** Opacity slider. */
    private JSlider m_kIPDSlider;

    /** Navigation (fly-thru) checkbox */
    private JCheckBox navigationCheckBox;

    /** Mouse translation speed slider. */
    private JSlider mouseTranslationSpeedSlider;
    
    /** Mouse rotation speed slider. */
    private JSlider mouseRotationSpeedSlider;
    
    /**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanelRenderMode_WM( VolumeTriPlanarInterface kVolumeViewer )
    {
        super(kVolumeViewer);
        init();
    }
    
	public void actionPerformed(ActionEvent event) {
        String levelStr = m_kIntensityTF.getText();
        rayBasedRenderWM.setIntenstityLevel(Integer.valueOf(levelStr).intValue());
        if ( event.getActionCommand().equals("ChangeStereoMode") )
        {
        	switch ( m_kStereoModeCB.getSelectedIndex() )
        	{
        	case 0: m_kVolumeViewer.actionPerformed( new ActionEvent(this, 0, "StereoOFF") );   break;
        	case 1: m_kVolumeViewer.actionPerformed( new ActionEvent(this, 0, "StereoRED") );   break;
        	case 2: m_kVolumeViewer.actionPerformed( new ActionEvent(this, 0, "StereoSHUTTER") );   break;
        	}        	
        }
        else
        {
        	m_kVolumeViewer.actionPerformed(event);
        }
    }
    
	public void disposeLocal(){}
	
    /**
     * Get the blender slider value
     * @return   slider value.
     */
    public int getBlendSliderValue() {
    	return m_kVolumeBlendSlider.getValue();
    }
    
    public int getIntensityLevel()
    {
        String levelStr = m_kIntensityTF.getText();
        return Integer.valueOf(levelStr).intValue();
    }
   
    public int getMovingSliderValue() {
        return m_kVolumeSamplesSliderMouseDragged.getValue();
    }
    
    public boolean getMultiHistoEnabled()
    {
        return radioMULTIHISTO.isSelected();
    }
    
    public int getReleasedSliderValue() {
        return m_kVolumeSamplesSliderMouseReleased.getValue();
    }
    
    
    public int getRenderMode()
    {
        if ( radioMIP.isSelected() ) { return 0; }
        if ( radioXRAY.isSelected() ) { return 1; } 
        if ( radioCOMPOSITE.isSelected() ) { return 2; } 
        if ( radioSURFACEFAST.isSelected() ) { return 3; }
        if ( radioSURFACE.isSelected() ) { return 4; }
        if ( radioCustom.isSelected() ) { return 5; }
        return 2;
    }
    
    /**
     * Get the slice check box. 
     * @return true or false
     */
    public JCheckBox getSlicesCheck() {
    	return m_kDisplaySlicesCheck;
    }
    
    public int getStereo()
    {
    	return m_kStereoModeCB.getSelectedIndex();
    }
    
    /**
     * Get the surface check box. 
     * @return true or false
     */
    public JCheckBox getSurfaceCheck() {
    	return m_kDisplaySurfaceCheck;
    }
    
    /**
     * Get the volume display checkbox.
     * @return  true or false
     */
    public JCheckBox getVolumeCheck()  {
    	return m_kDisplayVolumeCheck;
    }
    
    /**
     * Get the navigation checkbox, fly-thru checkbox
     * @return true or false
     */
    public JCheckBox getNaviCheckBox() {
    	return navigationCheckBox;
    }
    
    /* (non-Javadoc)
     * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
     */
	public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (radioMIP.isSelected() && (source == radioMIP)) {
        	rayBasedRenderWM.MIPMode();
        } else if (radioXRAY.isSelected() && (source == radioXRAY)) {
        	rayBasedRenderWM.DRRMode();
        } else if (radioCOMPOSITE.isSelected() && (source == radioCOMPOSITE)) {
        	rayBasedRenderWM.CMPMode();
        } else if (radioCustom.isSelected() && (source == radioCustom)) {
            m_kVolumeViewer.CustomBlendMode();
        } else if (radioSURFACE.isSelected() && (source == radioSURFACE)) {
        	m_kVolumeViewer.SURMode( false );
        } else if (radioSURFACEFAST.isSelected() && (source == radioSURFACEFAST)) {
        	m_kVolumeViewer.SURMode( true );
        }
        m_kExtractTriMesh.setEnabled(radioSURFACEFAST.isSelected());
        rayBasedRenderWM.MULTIHISTOMode(radioMULTIHISTO.isSelected());
        m_kVolumeViewer.updateMultihistoTab(radioMULTIHISTO.isSelected());
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
     * Get the blender slider value
     * @return   slider value.
     */
    public void setBlendSliderValue( int value ) {
        m_kVolumeBlendSlider.setValue(value);
        rayBasedRenderWM.setVolumeBlend( m_kVolumeBlendSlider.getValue()/100.0f );
    }
    
    /**
     * Set the tri-planar slices check box to true of false
     * @param flag  true or false
     */
    public void setDisplaySlicesCheck(boolean flag) {
    	m_kDisplaySlicesCheck.setSelected(flag);
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
    
    public void setIntensityLevel(int value)
    {
        String levelStr = String.valueOf(value);
        m_kIntensityTF.setText(levelStr);
        rayBasedRenderWM.setIntenstityLevel(Integer.valueOf(levelStr).intValue());
    }
    
    public void setMovingSliderValue( int value ) {
        m_kVolumeSamplesSliderMouseDragged.setValue(value);
        rayBasedRenderWM.setVolumeSamplesMouseDragged( m_kVolumeSamplesSliderMouseDragged.getValue()/1000.0f );
    }
    
    public void setMultiHistoEnabled( boolean value )
    {
        radioMULTIHISTO.setSelected(value);
        rayBasedRenderWM.MULTIHISTOMode(radioMULTIHISTO.isSelected());
        m_kVolumeViewer.updateMultihistoTab(radioMULTIHISTO.isSelected());
    }
    
    public void setReleasedSliderValue( int value ) {
        m_kVolumeSamplesSliderMouseReleased.setValue(value);
        rayBasedRenderWM.setVolumeSamplesMouseReleased( m_kVolumeSamplesSliderMouseReleased.getValue()/1000.0f );
    }
    
    public void setRenderMode( int which )
    {
        radioMIP.setSelected(false);
        radioXRAY.setSelected(false);
        radioCOMPOSITE.setSelected(false);
        radioSURFACEFAST.setSelected(false);
        radioSURFACE.setSelected(false);
        radioCustom.setSelected(false);

        if ( which == 0 ) 
        { 
            radioMIP.setSelected(true); 
            rayBasedRenderWM.MIPMode();
        }
        else if ( which == 1 )
        { 
            radioXRAY.setSelected(true); 
            rayBasedRenderWM.DRRMode();
        }
        else if ( which == 2 )
        { 
            radioCOMPOSITE.setSelected(true);
            rayBasedRenderWM.CMPMode();
        }
        else if ( which == 3 ) 
        {
            radioSURFACEFAST.setSelected(true); 
        	m_kVolumeViewer.SURMode( true );
        }
        else if ( which == 4 ) 
        { 
            radioSURFACE.setSelected(true); 
        	m_kVolumeViewer.SURMode( false );
        }
        else if ( which == 5 ) 
        { 
            radioCustom.setSelected(true); 
            m_kVolumeViewer.CustomBlendMode();
        }
        else
        { 
            radioCOMPOSITE.setSelected(true);
            rayBasedRenderWM.CMPMode();
        }
    }
    
    public void setStereo(int which )
    {
    	if ( which >= 0 && which < 3 )
    	{
    		m_kStereoModeCB.setSelectedIndex(which);
    	}
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
        if ( source == m_kVolumeSamplesSliderMouseReleased )
        {
            rayBasedRenderWM.setVolumeSamplesMouseReleased( m_kVolumeSamplesSliderMouseReleased.getValue()/1000.0f );
        }
        if ( source == m_kVolumeSamplesSliderMouseDragged )
        {
            rayBasedRenderWM.setVolumeSamplesMouseDragged( m_kVolumeSamplesSliderMouseDragged.getValue()/1000.0f );
        }
        if (source == m_kIPDSlider) {
            m_fIPD = m_kIPDSlider.getValue() / 100f;

            mkCurrent.setText("Stereo IPD " + String.valueOf(m_fIPD));
            rayBasedRenderWM.setIPD( m_fIPD );
        }
        if ( source == mouseTranslationSpeedSlider) {
        	float translationSpeed = mouseTranslationSpeedSlider.getValue()/1000.0f;
        	float rotationSpeed = mouseRotationSpeedSlider.getValue()/10.0f;
        	// System.err.println("translationSpeed = " + translationSpeed);
        	// System.err.println("rotationSpeed = " + rotationSpeed);
        	rayBasedRenderWM.setMouseTranslationSpeed(translationSpeed, rotationSpeed);
        }
        
        if ( source == mouseRotationSpeedSlider) {
        	float translationSpeed = mouseTranslationSpeedSlider.getValue()/1000.0f;
        	float rotationSpeed = mouseRotationSpeedSlider.getValue()/10.0f;
        	// System.err.println("translationSpeed = " + translationSpeed);
        	// System.err.println("rotationSpeed = " + rotationSpeed);
        	rayBasedRenderWM.setMouseRotationSpeed(translationSpeed, rotationSpeed);
        }
    }
    
    /**
     * Initializes the GUI components.
     */
    private void init() {
    	
    	 serif12 = MipavUtil.font12;
    	
    	 JPanel mainScrollPanel = new JPanel();
         mainScrollPanel.setLayout(new BorderLayout());

         scroller = new JScrollPane(mainScrollPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

         mainPanel = new JPanel(new BorderLayout());

         JPanel componentsPanel = new JPanel(new GridBagLayout());
         componentsPanel.setBorder(buildTitledBorder("Display Components"));
         
         GridBagConstraints gbc = new GridBagConstraints();

         //gbc.weightx = 1;
         gbc.anchor = GridBagConstraints.WEST;
         gbc.insets = new Insets(5, 5, 5, 5);
       
         m_kDisplayVolumeCheck = new JCheckBox( "Display RayCast Volume" );
         m_kDisplayVolumeCheck.setSelected(false);
         m_kDisplayVolumeCheck.setActionCommand( "VolumeRayCast");
         m_kDisplayVolumeCheck.addActionListener(this);
         componentsPanel.add(m_kDisplayVolumeCheck);
         gbc.gridx = 0;
         gbc.gridy = 0;
         componentsPanel.add(m_kDisplayVolumeCheck, gbc);
         
         m_kDisplaySlicesCheck = new JCheckBox( "Display Slices" );
         m_kDisplaySlicesCheck.setSelected(true);
         m_kDisplaySlicesCheck.setActionCommand( "VolumeSlices");
         m_kDisplaySlicesCheck.addActionListener(this);
         gbc.gridx = 1;
         gbc.gridy = 0;
         componentsPanel.add(m_kDisplaySlicesCheck, gbc);
         
         m_kDisplaySurfaceCheck = new JCheckBox( "Display Surface" );
         m_kDisplaySurfaceCheck.setSelected(false);
         m_kDisplaySurfaceCheck.setEnabled(false);
         m_kDisplaySurfaceCheck.setActionCommand( "Surface");
         m_kDisplaySurfaceCheck.addActionListener(this);
         gbc.gridx = 0;
         gbc.gridy = 1;
         componentsPanel.add(m_kDisplaySurfaceCheck, gbc);

         gbc.gridx = 0;
         gbc.gridy = 3;
         componentsPanel.add(new JLabel( "Stereo Mode" ), gbc);
         gbc.gridx = 1;
         m_kStereoModeCB = new JComboBox(new String[] { "Off", "Anaglyph", "Quad Buffer" });
         m_kStereoModeCB.addActionListener(this);
         m_kStereoModeCB.setActionCommand("ChangeStereoMode");
         m_kStereoModeCB.setAlignmentX(Component.LEFT_ALIGNMENT);
         m_kStereoModeCB.setFont(MipavUtil.font12);
         m_kStereoModeCB.setBackground(Color.white);
         gbc.gridy = 3;
         componentsPanel.add(m_kStereoModeCB, gbc);
         

         m_kIPDSlider = new JSlider(SwingConstants.HORIZONTAL, 0, 100, 10);

         //m_kIPDSlider.setMajorTickSpacing(20);
         //m_kIPDSlider.setPaintTicks(true);
         m_kIPDSlider.setEnabled(true);
         m_kIPDSlider.addChangeListener(this);

         JLabel maximum = new JLabel(String.valueOf(1));
         maximum.setForeground(Color.black);
         maximum.setFont(MipavUtil.font12);

         mkCurrent = new JLabel("Stereo IPD " + String.valueOf(m_kIPDSlider.getValue() / 100.0f));
         mkCurrent.setForeground(Color.black);
         mkCurrent.setFont(MipavUtil.font12B);

         JLabel minimum = new JLabel(String.valueOf(0));
         minimum.setForeground(Color.black);
         minimum.setFont(MipavUtil.font12);
         gbc.gridx = 0;
         gbc.gridy = 4;
         componentsPanel.add(mkCurrent, gbc);
         gbc.gridx++;
         componentsPanel.add(m_kIPDSlider, gbc);
    
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
         radioCustom = new JRadioButton("Custom Blend", false);
         radioCustom.setFont(serif12);
         group1.add(radioCustom);


         radioMULTIHISTO = new JCheckBox("MultiHistogram", false);
         radioMULTIHISTO.setFont(serif12);         
         
         radioMIP.addItemListener(this);
         radioXRAY.addItemListener(this);
         radioCOMPOSITE.addItemListener(this);
         radioSURFACE.addItemListener(this);
         radioSURFACEFAST.addItemListener(this);
         radioMULTIHISTO.addItemListener(this);
         radioCustom.addItemListener(this);
         gbc.gridx = 0;
         gbc.gridy = 0;
         renderModePanel.add(radioMIP, gbc);
         gbc.gridx = 1;
         gbc.gridy = 0;
         renderModePanel.add(radioXRAY, gbc);
         gbc.gridx = 2;
         gbc.gridy = 0;
         renderModePanel.add(radioCOMPOSITE, gbc);
         gbc.gridx = 0;
         gbc.gridy = 1;
         renderModePanel.add(radioSURFACEFAST, gbc);
         gbc.gridx = 1;
         gbc.gridy = 1;
         renderModePanel.add(radioSURFACE, gbc);
         gbc.gridx = 2;
         gbc.gridy = 1;
         renderModePanel.add(radioCustom, gbc);
         gbc.gridx = 0;
         gbc.gridy = 2;
         renderModePanel.add(radioMULTIHISTO, gbc);
         
         JPanel blendPanel = new JPanel(new GridBagLayout());
         blendPanel.setBorder(buildTitledBorder("Volume Sampling Controls"));
         
         JLabel kBlendLabel = new JLabel("Sample Opacity" );
         gbc.gridx = 0;
         gbc.gridy = 0;
         blendPanel.add(kBlendLabel, gbc);
         m_kVolumeBlendSlider = new JSlider( 0, 100, 100 );
         m_kVolumeBlendSlider.addChangeListener(this);
         gbc.gridx = 1;
         blendPanel.add(m_kVolumeBlendSlider, gbc);         
         
         JLabel kSamplesLabelMR = new JLabel("Samples Mouse Release" );
         gbc.gridx = 0;
         gbc.gridy = 1;
         blendPanel.add(kSamplesLabelMR, gbc);
         m_kVolumeSamplesSliderMouseReleased = new JSlider( 0, 1000, 
        		 (int)Math.min( 700, (m_kVolumeViewer.getImageA().getExtents()[2]*5.0f)) );
         m_kVolumeSamplesSliderMouseReleased.addChangeListener(this);
         rayBasedRenderWM.setVolumeSamplesMouseReleased( m_kVolumeSamplesSliderMouseReleased.getValue()/1000.0f );
         gbc.gridx = 1;
         blendPanel.add(m_kVolumeSamplesSliderMouseReleased, gbc);
         
         JLabel kSamplesLabelMD = new JLabel("Samples Mouse Rotation" );
         gbc.gridx = 0;
         gbc.gridy = 2;
         blendPanel.add(kSamplesLabelMD, gbc);
         m_kVolumeSamplesSliderMouseDragged = new JSlider( 0, 1000, 
        		 (int)Math.min( 250, m_kVolumeViewer.getImageA().getExtents()[2]*2.0f) );
         m_kVolumeSamplesSliderMouseDragged.addChangeListener(this);
         rayBasedRenderWM.setVolumeSamplesMouseDragged( m_kVolumeSamplesSliderMouseDragged.getValue()/1000.0f );
         gbc.gridx = 1;
         blendPanel.add(m_kVolumeSamplesSliderMouseDragged, gbc);
         
         
         m_kExtractTriMesh = new JButton( "Extract Mesh from Volume" );
         m_kExtractTriMesh.addActionListener(this);
         m_kExtractTriMesh.setActionCommand("ExtractMeshFromVolume");
         m_kExtractTriMesh.setToolTipText("Extract and load a Triangle Mesh based on the Volume in Surface Mode");
         m_kExtractTriMesh.setBorderPainted(true);
         m_kExtractTriMesh.setFocusPainted(true);
         m_kExtractTriMesh.setMargin(new Insets(0, 0, 0, 0));
         m_kExtractTriMesh.setEnabled(false);   
         
         m_kIntensityTF = new JTextField("50", 5);
         m_kIntensityTF.setEnabled(true);
         m_kIntensityTF.setFont(serif12);
         m_kIntensityTF.addActionListener(this);
         
         // add mouse rotateion and translation speed control slider
         JPanel mouseSpeedPanel = new JPanel(new GridBagLayout());
         mouseSpeedPanel.setBorder(buildTitledBorder("Mouse Sensitivity"));
         
         JLabel mouseTranslationSpeedLabel = new JLabel("Translation/Zoom Speed");
         gbc.gridx = 0;
         gbc.gridy = 0;
         mouseSpeedPanel.add(mouseTranslationSpeedLabel, gbc);
         mouseTranslationSpeedSlider = new JSlider( 0, 40, 20 );
         mouseTranslationSpeedSlider.addChangeListener(this);
         gbc.gridx = 1;
         mouseSpeedPanel.add(mouseTranslationSpeedSlider, gbc);         
         
         JLabel mouseRotationSpeedLabel = new JLabel("Rotation Speed");
         gbc.gridx = 0;
         gbc.gridy = 1;
         mouseSpeedPanel.add(mouseRotationSpeedLabel, gbc);
         mouseRotationSpeedSlider = new JSlider(0, 100, 1);
         mouseRotationSpeedSlider.addChangeListener(this);
         gbc.gridx = 1;
         mouseSpeedPanel.add(mouseRotationSpeedSlider, gbc);
         
         // surface extraction panel
         JPanel extractPanel = new JPanel(new GridBagLayout());
         extractPanel.setBorder(buildTitledBorder("Surface Extraction"));
         gbc.gridx = 0;
         gbc.gridy = 0;
         extractPanel.add(m_kExtractTriMesh, gbc);
         gbc.gridx = 1;
         extractPanel.add(new JLabel( "Intensity Level" ), gbc);
         gbc.gridx = 2;
         extractPanel.add(m_kIntensityTF, gbc);
         
         // add navigation panel
         navigationCheckBox = new JCheckBox( "Enable Navigation" );
         navigationCheckBox.setSelected(false);
         navigationCheckBox.setActionCommand( "Navigation");
         navigationCheckBox.addActionListener(this);
         
         gbc.gridx = 0;
         gbc.gridy = 0;
         
         JPanel naviPanel = new JPanel(new GridBagLayout());
         naviPanel.setBorder(buildTitledBorder("Navigation"));
         naviPanel.add(navigationCheckBox, gbc);
 
 
         // the main panel
         Box contentBox = new Box(BoxLayout.Y_AXIS);

         contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
         contentBox.add(componentsPanel);
         contentBox.add(renderModePanel);
         contentBox.add(blendPanel);
         contentBox.add(mouseSpeedPanel);
         contentBox.add(extractPanel);
         // contentBox.add(naviPanel);
  
         mainScrollPanel.add(contentBox, BorderLayout.NORTH);
         
         mainPanel.add(scroller, BorderLayout.CENTER);
    }
    
}