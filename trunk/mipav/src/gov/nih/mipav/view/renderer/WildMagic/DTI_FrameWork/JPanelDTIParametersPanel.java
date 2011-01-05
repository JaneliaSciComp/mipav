package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;


import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface.IntVector;

import java.awt.*;
import java.awt.event.*;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

public class JPanelDTIParametersPanel extends JInterfaceBase 
implements ListSelectionListener, ChangeListener {
    /**
     * 
     */
    private static final long serialVersionUID = 2778749302064237729L;

    /** Box layout for control panel. */
    private Box contentBox;

    private JCheckBox reconstructTracts;
    private JButton computeButton;

    /** The list box in the dialog for fiber bundle tracts. */
    private JList m_kTractList;
    /** The list box in the dialog for 3D VOIs. */
    private JList m_kVOIList;
    /** Color button for changing the color of the fiber bundles. */
    private JButton m_kColorButton;
    /** Color button detault color: */
    private Color m_kColorButtonDefault;
    /** Checkbox for turning on/off volume color for the polylines. */
    private JCheckBox m_kUseVolumeColor;
    /** Checkbox for switching between polylines and ellipsoids. */
    private JCheckBox m_kUseEllipsoids;
    /** Checkbox for displaying all tensors as ellipsoids. */
    private JCheckBox m_kAllEllipsoids;
    /** Checkbox for switching between polylines and ellipsoids and cylinders. */
    private JCheckBox m_kUseCylinders;
    /** Checkbox for switching between polylines and ellipsoids and cylinders, Tubes */
    private JCheckBox m_kTubes;
    /** Checkbox for displaying all tensors as ellipsoids. */
    private JCheckBox m_kAllCylinders;
    /** User-control over the number of glyphs displayed in GPUVolumeRender */
    private JSlider m_kDisplaySlider;
    private JLabel m_kSliderLabel;

    private VolumeTriPlanarInterfaceDTI parentFrame;

    private VolumeTriPlanarRender m_kVolumeDisplay;

    private ModelImage m_kDTIImage;

    /** Tract input file. */
    private File m_kTractFile = null;

    /** For TRACTS dialog: number of tracts to display. */
    private JTextField m_kTractsLimit;

    /** For TRACTS dialog: minimum tract length to display. */
    private JTextField m_kTractsMin;

    /** For TRACTS dialog: maximum tract length to display. */
    private JTextField m_kTractsMax;


    /** When selected, only tracts that intersect the VOI are displayed. */
    private JCheckBox m_kUseVOICheck = null;

    /** Keeps track of the groups of polylines loaded. */
    private Vector<Integer> m_kBundleList = new Vector<Integer>();

    /** Number of currently loaded fiber bundle groups. */
    private int m_iBundleCount = 0;

    private ModelImage m_kImage;

    /** Fiber bundle tract file input path name text box. */
    private JTextField m_kTractPath;

    /** Which tensor nodes are already on the fiber bundle tract */
    private boolean[] m_abVisited = null;

    private JCheckBox m_kNegX;
    private JCheckBox m_kNegY;
    private JCheckBox m_kNegZ;

    private JTextField m_kFAMinThreshold;
    private JTextField m_kFAMaxThreshold;
    private JTextField m_kMaxAngle;

    /*
    private JRadioButton radioLines;
    private JRadioButton radioEllipzoids;
    private JRadioButton radioCylinders;
    private JRadioButton radioTubes;
    private JRadioButton radioArrows;
    */
    
    private JCheckBox displayAllCheckBox;
    private int displayMode;
    private static int Polylines = 0;
    private static int Ellipzoids = 1;
    private static int Tubes = 2;
    private static int Cylinders = 3;
    private static int Arrows = 4;
    private boolean displayAll;
    private int centerIndex;
    private boolean loadingTrack = false;

    private ColorRGB m_kCInclude = new ColorRGB(0,1,0);
    private ColorRGB m_kCExclude = new ColorRGB(.5f,0,0);
    private ColorRGB m_kCIgnore = new ColorRGB(.2f,.2f,.2f);
    private JRadioButton m_kInclude;
    private JRadioButton m_kExclude;
    private JRadioButton m_kIgnore;
    private float m_fFAMin = 0.0f;
    private float m_fFAMax = 1.0f;
    private float m_fMaxAngle = (float)Math.PI/4.0f;
    
    private boolean m_bDTIImageSet = false;
    
    /** list to hold the glyphs type name */
    private JComboBox glyphsList;
    
    private class VOIParams {
        String Name;
        boolean Include;
        boolean Exclude;
        boolean Ignore;
        public VOIParams() {}
        public String ToString()
        {
            return new String( Name + " " + Include + " " + Exclude + " " + Ignore );
        }
    }
    private Vector<VOIParams> m_kVOIParamsList = null;
    
    public JPanelDTIParametersPanel(VolumeTriPlanarInterfaceDTI _parentFrame, VolumeTriPlanarRender _m_kVolumeDisplay) {
        parentFrame = _parentFrame;
        m_kVolumeDisplay = _m_kVolumeDisplay;
        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
       
        // mainPanel.add(createLoadTractDialog(), BorderLayout.NORTH);
        mainPanel.add(createTractDialog(), BorderLayout.NORTH);
        mainPanel.add(createTractPanel(), BorderLayout.CENTER );
    }

    /**
     * Dispose memory.
     *
     */
    public void disposeLocal() {
       
    	/*
       if ( m_kDTIImage != null ) {
    	   m_kDTIImage.disposeLocal();
    	   m_kDTIImage = null;
       }
    	*/
       
       
       if ( m_kVOIParamsList != null )
    	   m_kVOIParamsList = null;
    	
    	
       if ( m_kVolumeDisplay != null ) {
    	   m_kVolumeDisplay.dispose();
    	   m_kVolumeDisplay = null;
       }
       
    }
    
    /**
     * Color chooser for when the user wants to change the color of the fiber
     * bundle tracts.
     */
    private ViewJColorChooser m_kColorChooser;



    /**
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if ( source == m_kDisplaySlider )
        {
            if ( !m_kDisplaySlider.getValueIsAdjusting())
            {
                m_kVolumeDisplay.setEllipseMod( m_kDisplaySlider.getValue() + 1 );
            }
            if ( m_kDisplaySlider.getValue() == 0 )
            {
                m_kSliderLabel.setText( "Display Glphs every step: ");
            }
            else if ( m_kDisplaySlider.getValue() < 100 )
            {
                m_kSliderLabel.setText( "Display Glphs every " + (m_kDisplaySlider.getValue() + 1) + " steps: ");
            }
        }
    }

    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   DOCUMENT ME!
     * @param  frameHeight  DOCUMENT ME!
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        mainPanel.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        mainPanel.setSize(new Dimension(panelWidth, frameHeight - 40));
        mainPanel.revalidate();
    }

    public void valueChanged(ListSelectionEvent kEvent) {
        
    	if ( kEvent.getSource() == m_kTractList ) {
        	int index = m_kTractList.getSelectedIndex();
        	if ( index != -1 ) {
	        	DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
	            String bundleName = (String)kList.get(index);
	    		int endIndex = bundleName.length();
	    		int startIndex = new String("FiberBundle").length();
	    		String temp = bundleName.substring(startIndex, endIndex);
	    		int gid = Integer.valueOf(temp);
                ColorRGB color = m_kVolumeDisplay.getGroupColor(gid);
            	m_kColorButton.setBackground(new Color(color.R, color.G, color.B));
            }
            
        }
    	
    	if ( m_kVOIParamsList == null )
        {
            return;
        }
        if ( kEvent.getSource() == m_kVOIList )
        {
            int iVOI = m_kVOIList.getSelectedIndex();
            if ( iVOI >= 0 && iVOI < m_kVOIParamsList.size() )
            {
                m_kInclude.setSelected(m_kVOIParamsList.get(iVOI).Include);
                m_kExclude.setSelected(m_kVOIParamsList.get(iVOI).Exclude);
                m_kIgnore.setSelected(m_kVOIParamsList.get(iVOI).Ignore);
            }
        }
    }

    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command;       
        
        if (source instanceof JComboBox) {
			JComboBox cb = (JComboBox)source;
			command = (String) cb.getSelectedItem();
			if (command.equals("Lines")) {
				displayMode = Polylines;
				displayAllCheckBox.setEnabled(false);
			} else if (command.equals("Ellipsoids")) {
				displayMode = Ellipzoids;
				displayAllCheckBox.setEnabled(true);
			} else if (command.equals("Cylinders")) {
				displayMode = Cylinders;
				displayAllCheckBox.setEnabled(false);
			} else if (command.equals("Tubes")) {
				displayMode = Tubes;
				displayAllCheckBox.setEnabled(true);
			} else if (command.equals("Arrows")) {
				displayMode = Arrows;
				displayAllCheckBox.setEnabled(true);
			}
			invokeDisplayFunction();
		} else {
			command = event.getActionCommand();
			if (command.equals("ChangeColor")) {
				m_kColorChooser = new ViewJColorChooser(new Frame(),
						"Pick fiber bundle color", new OkColorListener(),
						new CancelListener());
			} else if (command.equals("VolumeColor")) {
				m_kVolumeDisplay.setVolumeColor(m_kUseVolumeColor.isSelected());
				if (m_kUseVolumeColor.isSelected()) {
					setColor(m_kColorButton.getBackground());
				} else {
					setColor(m_kColorButton.getBackground());
				}
			} else if (command.equals("DisplayAll")) {
				displayAll = displayAllCheckBox.isSelected();
				invokeDisplayFunction();
			} else if (command.equals("tractLoad")) {
				loadingTrack = true;
				loadTractFile();
				loadingTrack = false;
				((VolumeTriPlanerRenderDTI) m_kVolumeDisplay)
						.enableSlicePickable(true);
				processTractFile();
			} else if (command.equals("Add")) {
				processTractFile();
			} else if (command.equals("Remove")) {
				removePolyline();
			} else if (command.equals("Include")) {
				int iVOI = m_kVOIList.getSelectedIndex();
				if (m_kVOIParamsList != null) {
					m_kVOIParamsList.get(iVOI).Include = m_kInclude
							.isSelected();
					m_kVOIParamsList.get(iVOI).Exclude = m_kExclude
							.isSelected();
					m_kVOIParamsList.get(iVOI).Ignore = m_kIgnore.isSelected();
					parentFrame.setColor(m_kVOIParamsList.get(iVOI).Name,
							m_kCInclude, true);
				}
			} else if (command.equals("Exclude")) {
				int iVOI = m_kVOIList.getSelectedIndex();
				if (m_kVOIParamsList != null) {
					m_kVOIParamsList.get(iVOI).Include = m_kInclude
							.isSelected();
					m_kVOIParamsList.get(iVOI).Exclude = m_kExclude
							.isSelected();
					m_kVOIParamsList.get(iVOI).Ignore = m_kIgnore.isSelected();
					parentFrame.setColor(m_kVOIParamsList.get(iVOI).Name,
							m_kCExclude, true);
				}
			} else if (command.equals("Ignore")) {
				int iVOI = m_kVOIList.getSelectedIndex();
				if (m_kVOIParamsList != null) {
					m_kVOIParamsList.get(iVOI).Include = m_kInclude
							.isSelected();
					m_kVOIParamsList.get(iVOI).Exclude = m_kExclude
							.isSelected();
					m_kVOIParamsList.get(iVOI).Ignore = m_kIgnore.isSelected();
					parentFrame.setColor(m_kVOIParamsList.get(iVOI).Name,
							m_kCIgnore, true);
				}
			}
	        else if ( command.equals( "FAMINChanged" ) )
	        {
	            if (!JDialogBase.testParameter(m_kFAMinThreshold.getText(), 0.0f, 1.0f))
	            {
	                m_kFAMinThreshold.requestFocus();
	                m_kFAMinThreshold.selectAll();
	            }
	            else
	            {
	                m_fFAMin = Float.valueOf(m_kFAMinThreshold.getText()).floatValue();
	            }
	        }

	        else if ( command.equals( "FAMAXChanged" ) )
	        {
	            if (!JDialogBase.testParameter(m_kFAMaxThreshold.getText(), 0.0f, 1.0f))
	            {
	                m_kFAMaxThreshold.requestFocus();
	                m_kFAMaxThreshold.selectAll();
	            }
	            else
	            {
	                m_fFAMax = Float.valueOf(m_kFAMaxThreshold.getText()).floatValue();
	            }
	        }

	        else if ( command.equals( "MaxAngleChanged" ) )
	        {
	            if (!JDialogBase.testParameter(m_kMaxAngle.getText(), 0.0f, 180f))
	            {
	                m_kMaxAngle.requestFocus();
	                m_kMaxAngle.selectAll();
	            }
	            else
	            {
	                m_fMaxAngle = Float.valueOf(m_kMaxAngle.getText()).floatValue();
	                m_fMaxAngle = (float)(m_fMaxAngle*Math.PI/180.0f);
	            }
	        }

		}
        
      
    }

    public void processDTI() {
    	// loadingTrack = true;
		// loadTractFile();
		// loadingTrack = false;
		((VolumeTriPlanerRenderDTI) m_kVolumeDisplay).enableSlicePickable(true);
		processTractFile();
    }
    
    public void updateCounter() {
            updateTractCount();
    }
    
    public void add3DVOI( String kVOIName )
    {
        if ( m_kVOIList == null )
        {
            return;
        }
        DefaultListModel kList = (DefaultListModel) m_kVOIList.getModel();
        int iSize = kList.getSize();
        kList.add(iSize, kVOIName );
        m_kVOIList.setSelectedIndex(iSize);
        
        if ( m_kVOIParamsList == null )
        {
            m_kVOIParamsList = new Vector<VOIParams>();
        }
        VOIParams kParams = new VOIParams();
        kParams.Name = new String(kVOIName);
        kParams.Include = m_kInclude.isSelected();
        kParams.Exclude = m_kExclude.isSelected();
        kParams.Ignore = m_kIgnore.isSelected();
        m_kVOIParamsList.add( kParams );
        parentFrame.setColor( kVOIName, m_kCInclude, true );
    }
    
    public void remove3DVOI( String kVOIName )
    {
        if ( m_kVOIList == null )
        {
            return;
        }
        DefaultListModel kList = (DefaultListModel) m_kVOIList.getModel();
        if ( kList.contains( kVOIName ) )
        {
            int i = kList.indexOf( kVOIName );
            kList.remove(i);
            m_kVOIParamsList.remove( i );
        }
        if ( kList.size() != 1 )
        {
            m_kVOIList.setSelectedIndex( kList.size()-1);
        }
    }
    
    public int getVOIQuantity()
    {
        return m_kVOIParamsList.size();
    }

    public void addFiberTract() {
            addTract();
    }

    private JPanel createLoadTractDialog() {
    	JPanel tractLoadPanel = new JPanel(new BorderLayout());
    	
    	 GridBagLayout kGBL = new GridBagLayout();
         GridBagConstraints gbc = new GridBagConstraints();
         gbc.fill = GridBagConstraints.HORIZONTAL;
         gbc.weightx = 1;
         gbc.weighty = 0;
         gbc.anchor = GridBagConstraints.NORTHWEST;
    	
    	JPanel kParamsPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        JLabel kNumberTractsLimit = new JLabel(
        "Maximum number of tracts to display:");
        kParamsPanel.add(kNumberTractsLimit, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kTractsLimit = new JTextField("100", 5);
        m_kTractsLimit.setBackground(Color.white);
        kParamsPanel.add(m_kTractsLimit, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel m_kTractsMinLength = new JLabel("Minimum tract length:");
        kParamsPanel.add(m_kTractsMinLength, gbc);
        gbc.gridx++;
        m_kTractsMin = new JTextField("50", 5);
        m_kTractsMin.setBackground(Color.white);
        kParamsPanel.add(m_kTractsMin, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel m_kTractsMaxLength = new JLabel("Maximum tract length:");
        kParamsPanel.add(m_kTractsMaxLength, gbc);
        gbc.gridx++;
        m_kTractsMax = new JTextField("100", 5);
        m_kTractsMax.setBackground(Color.white);
        kParamsPanel.add(m_kTractsMax, gbc);
        
        JPanel filesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        JLabel kTractLabel = new JLabel(" DTI tract file: ");
        filesPanel.add(kTractLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        m_kTractPath = new JTextField(15);
        m_kTractPath.setEditable(true);
        m_kTractPath.setBackground(Color.white);
        filesPanel.add(m_kTractPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 0;
        JButton kTractLoadButton = new JButton("Load");
        kTractLoadButton.addActionListener(this);
        kTractLoadButton.setActionCommand("tractLoad");
        filesPanel.add(kTractLoadButton, gbc);
        
        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        contentBox.add(kParamsPanel);
        contentBox.add(filesPanel);
        
        tractLoadPanel.add(contentBox, BorderLayout.NORTH);

        tractLoadPanel.setBorder(buildTitledBorder("Load Fiber Tracts"));
        
        return tractLoadPanel;        	
    }
    
    /**
     * Creates the user-interface for the Fiber Bundle Tract dialog.
     * 
     * @return JPanel containing the user-interface for the Fiber Bundle Tract
     *         dialog.
     */
    private JPanel createTractDialog() {
        GridBagLayout kGBL = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;

        JPanel kTractPanel = new JPanel(kGBL);

        JPanel kParamsPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
               
        m_kUseVOICheck = new JCheckBox("Use VOI");
        kParamsPanel.add(m_kUseVOICheck, gbc);

        JLabel slicePicckableLabel = new JLabel("Ctrl and left mouse press to select the individual tract.");

        JPanel slicePanel = new JPanel();
        slicePanel.setLayout(new BorderLayout());
        slicePanel.add(slicePicckableLabel, BorderLayout.WEST);
        slicePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        slicePanel.setAlignmentY(Component.TOP_ALIGNMENT);

        m_kNegX = new JCheckBox("+/- x");
        m_kNegX.setSelected(false);
        m_kNegX.addActionListener(this);
        m_kNegX.setActionCommand("NegX");
        m_kNegX.setEnabled(true);

        m_kNegY = new JCheckBox("+/- y");
        m_kNegY.setSelected(false);
        m_kNegY.addActionListener(this);
        m_kNegY.setActionCommand("NegY");
        m_kNegY.setEnabled(true);

        m_kNegZ = new JCheckBox("+/- z");
        m_kNegZ.setSelected(false);
        m_kNegZ.addActionListener(this);
        m_kNegZ.setActionCommand("NegZ");
        m_kNegZ.setEnabled(true);


        JPanel kVectorPanel = new JPanel();
        kVectorPanel.setLayout(new BoxLayout(kVectorPanel, BoxLayout.X_AXIS));
        kVectorPanel.add(m_kNegX);
        kVectorPanel.add(m_kNegY);
        kVectorPanel.add(m_kNegZ);
        //kVectorPanel.setBorder(buildTitledBorder("Vector component-wise negation"));
        

        m_kFAMinThreshold = new JTextField("0.0", 4);
        m_kFAMinThreshold.setActionCommand("FAMINChanged");
        m_kFAMinThreshold.addActionListener(this);
        m_kFAMaxThreshold = new JTextField("1.0", 4);
        m_kFAMaxThreshold.setActionCommand("FAMAXChanged");
        m_kFAMaxThreshold.addActionListener(this);
        m_kMaxAngle = new JTextField("45", 4);
        m_kMaxAngle.setActionCommand("MaxAngleChanged");
        m_kMaxAngle.addActionListener(this);
        JPanel kTrackPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        kTrackPanel.add( kVectorPanel, gbc );
        gbc.gridy++;
        kTrackPanel.add(new JLabel( "FA Threshold Min (0.0-1.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add( m_kFAMinThreshold, gbc );
        gbc.gridx = 0;
        gbc.gridy++;
        kTrackPanel.add(new JLabel( "FA Threshold Max (0.0-1.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add( m_kFAMaxThreshold, gbc );
        gbc.gridx = 0;
        gbc.gridy++;
        kTrackPanel.add(new JLabel( "Maximum Angle (0.0-180.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add( m_kMaxAngle, gbc );
        kTrackPanel.setBorder(buildTitledBorder("Fiber Generation Options for Interactive Fiber Selection"));

       
        // list panel for surface filenames
        m_kVOIList = new JList( new DefaultListModel() );
        m_kVOIList.setVisibleRowCount(3);
        m_kVOIList.addListSelectionListener(this);
        m_kVOIList.setPrototypeCellValue("aaaaaaaaaaaaaaaa.aaa    ");


        JScrollPane kScrollPane = new JScrollPane(m_kVOIList);
        JPanel scrollPanel = new JPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        

        ButtonGroup kGroup = new ButtonGroup();
        m_kInclude = new JRadioButton("Include");
        m_kInclude.addActionListener(this);
        m_kInclude.setActionCommand("Include");
        m_kInclude.setFont(MipavUtil.font12B);
        m_kInclude.setPreferredSize(MipavUtil.defaultButtonSize);
        m_kInclude.setSelected(true);
        kGroup.add(m_kInclude);

        m_kExclude = new JRadioButton("Exclude");
        m_kExclude.addActionListener(this);
        m_kExclude.setActionCommand("Exclude");
        m_kExclude.setFont(MipavUtil.font12B);
        m_kExclude.setPreferredSize(MipavUtil.defaultButtonSize);
        kGroup.add(m_kExclude);

        m_kIgnore = new JRadioButton("Ignore");
        m_kIgnore.addActionListener(this);
        m_kIgnore.setActionCommand("Ignore");
        m_kIgnore.setFont(MipavUtil.font12B);
        m_kIgnore.setPreferredSize(MipavUtil.defaultButtonSize);
        kGroup.add(m_kIgnore);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(m_kInclude);
        buttonPanel.add(m_kExclude);
        buttonPanel.add(m_kIgnore);

        JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(buttonPanel, BorderLayout.SOUTH);
        listPanel.setBorder(buildTitledBorder("3D VOI list"));
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        kTractPanel.add(listPanel, gbc);
        gbc.gridy++;
        kTractPanel.add(kParamsPanel, gbc);
        gbc.gridy++;
        kTractPanel.add(slicePanel, gbc);
        //gbc.gridy++;
        //kTractPanel.add(kVectorPanel, gbc);

        gbc.gridy++;
        kTractPanel.add(kTrackPanel, gbc);
        
        
        kTractPanel.setBorder(buildTitledBorder("Inclusion & Exclusion Parameters"));
        return kTractPanel;
    }


    /**
     * Launches the JFileChooser for the user to select the tract file. Stores
     * the File for the tract file but does not read the file.
     */
    private void loadTractFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences
                .getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(
                ViewImageFileFilter.ALL));
        chooser.setDialogTitle("Choose Diffusion Tensor Tract file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            String kDTIName = new String(chooser.getSelectedFile().getName());
            String kTract = new String("_tract");
            kDTIName = kDTIName.substring(0, kDTIName.length()
                    - kTract.length());
            FileIO fileIO = new FileIO();
            /*
            m_kDTIImage = fileIO.readImage(kDTIName, chooser
                    .getCurrentDirectory()
                    + File.separator);
            if (m_kDTIImage.getNDims() != 4) {
                MipavUtil
                .displayError("Diffusion Tensor file does not have correct dimensions");
                if (m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                    m_kDTIImage = null;
                }
            }
            if (m_kDTIImage.getExtents()[3] != 6) {
                MipavUtil
                .displayError("Diffusion Tensor does not have correct dimensions");
                if (m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                    m_kDTIImage = null;
                }   
            }
            */
            m_kTractFile = new File(chooser.getSelectedFile().getAbsolutePath());
            if (!m_kTractFile.exists() || !m_kTractFile.canRead()) {
                m_kTractFile = null;
                return;
            }
            int iLength = (int) m_kTractFile.length();
            if (iLength <= 0) {
                m_kTractFile = null;
                return;
            }
            m_kTractPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser
                    .getCurrentDirectory().toString());
        }
    }

    /**
     * Pass the DTI image to the GPUVolumeRender.
     * 
     * @param kDTIImage
     *            new DTI image.
     */
    protected void setDTIImage(ModelImage kDTIImage, boolean bNegX, boolean bNegY, boolean bNegZ) {
        m_kDTIImage = kDTIImage;
        m_kVolumeDisplay.setDTIImage(m_kDTIImage, bNegX, bNegY, bNegZ );
        m_kVolumeDisplay.setEllipseMod(m_kDisplaySlider.getValue());
    }

    /** Updates the number of fiber bundle tract groups. */
    protected void updateTractCount() {
        m_iBundleCount = getMinUnused(m_kBundleList);
    }

    /**
     * Gets a new fiber bundle index.
     * 
     * @param kBundleList
     *            list of fiber bundles.
     */
    private int getMinUnused(Vector<Integer> kBundleList) {
        int iMin = 0;
        if (kBundleList.size() == 0) {
            return iMin;
        }
        boolean bFound = false;
		for (int i = 0; i < kBundleList.size(); i++) {
			iMin = i;
			bFound = false;
			for (int j = 0; j < kBundleList.size(); j++) {
				if (iMin == kBundleList.get(j).intValue()) {
					bFound = true;
				}
			}
			if (!bFound) {
				return iMin;
			}
		}
        return kBundleList.size();
    }


    public void setTractParams(File _m_kTractFile, JTextField _m_kTractsLimit, 
    		JTextField _m_kTractsMin, JTextField _m_kTractsMax, 
    		JTextField _m_kTractPath, ModelImage _m_kDTIImage) {
    	
    	m_kDTIImage = _m_kDTIImage;
    	m_kTractFile = _m_kTractFile;
    	m_kTractsLimit = _m_kTractsLimit;
    	m_kTractsMin = _m_kTractsMin;
    	m_kTractsMax = _m_kTractsMax;
    	m_kTractPath = _m_kTractPath;
    
    	
    }


    /**
     * process the tract file. Uses the File stored from the loadTractFile fn.
     * Loads fiber bundle tracts, filters them with the user-defined display
     * parameters, and passes them to the GPUVolumeRender for display.
     */
    public void processTractFile() {
        if (m_kTractFile == null) {
            // MipavUtil.displayError("Tract file must be set.");
            return;
        }

        try {
            updateTractCount();

            boolean bTractsAdded = false;

            int iNumTractsLimit = (new Integer(m_kTractsLimit.getText()))
            .intValue();
            int iTractMinLength = (new Integer(m_kTractsMin.getText()))
            .intValue();
            int iTractMaxLength = (new Integer(m_kTractsMax.getText()))
            .intValue();

            int iNumTracts = 0;

            VolumeTriPlanarInterface.IntVector[] kVOIImage = null;
            int iNum3DVOI = 0;
            if (m_kUseVOICheck.isSelected() && (parentFrame != null))
            {
                kVOIImage = parentFrame.getVOIImage();
                iNum3DVOI = parentFrame.get3DVOIQuantity();
                m_kUseVolumeColor.setSelected(false);
                setVolumeColor();
            }

            int iDimX = 0, iDimY = 0, iDimZ = 0;
            boolean bNegX = false, bNegY = false, bNegZ = false;
            FileInputStream kFileReader = new FileInputStream(m_kTractFile);
            int iBufferSize = 3 * 4 + 3;

            byte[] racBuffer = new byte[iBufferSize];
            kFileReader.read(racBuffer, 0, iBufferSize);
            ByteArrayInputStream acBufferIn = new ByteArrayInputStream(
                    racBuffer);
            DataInputStream acDataIn = new DataInputStream(acBufferIn);
            try {
                iDimX = acDataIn.readInt();
                iDimY = acDataIn.readInt();
                iDimZ = acDataIn.readInt();
                bNegX = acDataIn.readBoolean();
                bNegY = acDataIn.readBoolean();
                bNegZ = acDataIn.readBoolean();
            } catch (IOException e) {
                e.printStackTrace();
            }
            acBufferIn = null;
            acDataIn = null;
            racBuffer = null;

            m_kNegX.setSelected( bNegX );
            m_kNegY.setSelected( bNegY );
            m_kNegZ.setSelected( bNegZ );
            if ( !m_bDTIImageSet )
            {
                m_bDTIImageSet = true;
                setDTIImage(m_kDTIImage, bNegX, bNegY, bNegZ );
                // don't load any tracts the first time.
                iNumTractsLimit = 0;
            }
            
            int iLength = (int) m_kTractFile.length();
            int iBufferNext = iBufferSize;
            while (iBufferNext < iLength) {
                if (iNumTracts >= iNumTractsLimit) {
                    break;
                }

                Vector<Integer> kTract = inputTract(kFileReader);
                iBufferNext += kTract.size() * 4 + 4;
                int iVQuantity = kTract.size();
                if (contains(kVOIImage, iNum3DVOI, kTract)) {
                    if ((iVQuantity > iTractMinLength)
                            && (iVQuantity < iTractMaxLength)) {
                        if (iNumTracts < iNumTractsLimit) {
                            iNumTracts++;
                            bTractsAdded = true;
                            addTract(kTract, iVQuantity, iDimX, iDimY, iDimZ);
                        }
                    }
                }
            }
            if (bTractsAdded) {
                addTract();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        /*
        if ( m_kDTIImage != null ) {
        	m_kDTIImage.disposeLocal();
        	m_kDTIImage = null;
        }
        */
    }
    
    /** Updates the tract list user-interface. */
    public void addTract() {
    	// m_iBundleCount--;
        m_kVolumeDisplay.addGroupColor();
        DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        int iSize = kList.getSize();
        kList.add(iSize, new String("FiberBundle" + m_iBundleCount));
        m_kTractList.setSelectedIndex(iSize);
    }

    /** Adds a fiber bundle tract to the GPUVolumeRender and JPanelSurface.
     * @param kTract list of voxels in the fiber bundle.
     * @param iVQuantity number of voxels in the fiber bundle.
     * @param iDimX the x-dimensions of the DTI image used to create the tract.
     * @param iDimY the y-dimensions of the DTI image used to create the tract.
     * @param iDimZ the z-dimensions of the DTI image used to create the tract.
     */
    protected void addTract( Vector<Integer> kTract, int iVQuantity, int iDimX, int iDimY, int iDimZ )
    {
        m_kImage = parentFrame.getImageA();
        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];

        float fXDelta = m_kImage.getFileInfo(0).getResolutions()[0];
        float fYDelta = m_kImage.getFileInfo(0).getResolutions()[1];
        float fZDelta = m_kImage.getFileInfo(0).getResolutions()[2];
        
        float fMaxX = (float) (iXBound - 1) * m_kImage.getFileInfo(0).getResolutions()[0];
        float fMaxY = (float) (iYBound - 1) * m_kImage.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (float) (iZBound - 1) * m_kImage.getFileInfo(0).getResolutions()[2];

        float fMax = fMaxX;
        if (fMaxY > fMax) {
            fMax = fMaxY;
        }
        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetCChannels(1,3);
        VertexBuffer pkVBuffer = new VertexBuffer(kAttr,iVQuantity);                        

        int iTractCount = 0;
        try {
            float fR = 0, fG = 0, fB = 0;

            for (int i = 0; i < iVQuantity; i++)
            {
                int iIndex = kTract.get(i);

                int iX = iIndex % iDimX;
                iIndex -= iX;
                iIndex /= iDimX;

                int iY = iIndex % iDimY;
                iIndex -= iY;
                iIndex /= iDimY;

                int iZ = iIndex;

                iIndex = kTract.get(i);

                if ( loadingTrack == true && i == (int)(iVQuantity/2) ) {
                    centerIndex = iIndex;
                }
                ColorRGB kColor1;
                if ( m_kImage.isColorImage() )
                {
                    fR = m_kImage.getFloat( iIndex*4 + 1 )/255.0f;
                    fG = m_kImage.getFloat( iIndex*4 + 2 )/255.0f;
                    fB = m_kImage.getFloat( iIndex*4 + 3 )/255.0f;
                    kColor1 = new ColorRGB(fR, fG, fB);
                }
                else
                {
                    fR = m_kImage.getFloat( iIndex );
                    kColor1 = new ColorRGB(fR, fR, fR);
                }

                


                float xBox = (iXBound - 1) * fXDelta;
                float yBox = (iYBound - 1) * fYDelta;
                float zBox = (iZBound - 1) * fZDelta;
                float maxBox = Math.max(xBox, Math.max(yBox, zBox));
                
                float fX = ((2.0f * (iX * fXDelta)) - xBox)/(2.0f*maxBox);
                float fY = ((2.0f * (iY * fYDelta)) - yBox)/(2.0f*maxBox);
                float fZ = ((2.0f * (iZ * fZDelta)) - zBox)/(2.0f*maxBox);
                
                pkVBuffer.SetPosition3(i, iX, iY, iZ);
                pkVBuffer.SetColor3(0,i, new ColorRGB(fX, fY, fZ));
                pkVBuffer.SetColor3(1,i, kColor1 );
                iTractCount++;

            }
        } catch ( Exception e ) {
            return;
        }
        boolean bClosed = false;
        boolean bContiguous = true;
        // addPolyline( new Polyline(pkVBuffer,bClosed,bContiguous) );
        // apply B-spline filter to smooth the track
        if ( iVQuantity >= 7 ) {
            //addPolyline(new Polyline(pkVBuffer, bClosed, bContiguous ));
            addPolyline(new Polyline(smoothTrack(pkVBuffer, kTract,iVQuantity, iDimX, iDimY, iDimZ), bClosed, bContiguous ));
            m_kBundleList.add(new Integer(m_iBundleCount));
            m_iBundleCount++;
        }
    }

    /**
     * Smooth the fiber tracks with B-spline interpolation
     * 
     * @param pkVBuffer
     *            fiber track vertex coordinates as the control points.
     * @param kTract
     *            fiber track index list.
     * @param iVQuantity
     *            number of voxels in the fiber bundle.
     * @return  B-spline interpolated fiber track
     */
    private VertexBuffer smoothTrack(VertexBuffer pkVBuffer, Vector<Integer> kTract, int iVQuantity, int iDimX, int iDimY, int iDimZ ) {
        float fX_0, fY_0, fZ_0;
        float fX_1, fY_1, fZ_1;
        float fX_2, fY_2, fZ_2;
        float fX_3, fY_3, fZ_3;

        // curve sub-division number, default to 10.  
        int curveSubD = 1;
        float u, u_2, u_3;

        Attributes attr = new Attributes();
        attr.SetPChannels(3);
        attr.SetCChannels(0, 3);
        attr.SetCChannels(1, 3);
        VertexBuffer bsplineVBuffer = new VertexBuffer(attr, (iVQuantity -3) * curveSubD);		

        int index = 0;

        float fR = 0, fG = 0, fB = 0;

        float pos_x, pos_y, pos_z;

        for (int i = 0; i < iVQuantity-3; i++) {
            for(int j = 0; j < curveSubD; j++) {

                ColorRGB resultUnit0, resultUnit1;		

                u = (float)j / curveSubD;
                u_2 = u * u;
                u_3 = u_2 * u;

                fX_0 = pkVBuffer.GetPosition3fX(i);
                fY_0 = pkVBuffer.GetPosition3fY(i);
                fZ_0 = pkVBuffer.GetPosition3fZ(i);

                fX_1 = pkVBuffer.GetPosition3fX(i+1);
                fY_1 = pkVBuffer.GetPosition3fY(i+1);
                fZ_1 = pkVBuffer.GetPosition3fZ(i+1);

                fX_2 = pkVBuffer.GetPosition3fX(i+2);
                fY_2 = pkVBuffer.GetPosition3fY(i+2);
                fZ_2 = pkVBuffer.GetPosition3fZ(i+2);

                fX_3 = pkVBuffer.GetPosition3fX(i+3);
                fY_3 = pkVBuffer.GetPosition3fY(i+3);
                fZ_3 = pkVBuffer.GetPosition3fZ(i+3);

                pos_x = B_SPLINE(u, u_2, u_3, fX_0, fX_1, fX_2, fX_3);
                pos_y = B_SPLINE(u, u_2, u_3, fY_0, fY_1, fY_2, fY_3);
                pos_z = B_SPLINE(u, u_2, u_3, fZ_0, fZ_1, fZ_2, fZ_3);

                int iIndex = kTract.get(i);

                iIndex = kTract.get(i);

                int iX = iIndex % iDimX;
                iIndex -= iX;
                iIndex /= iDimX;

                int iY = iIndex % iDimY;
                iIndex -= iY;
                iIndex /= iDimY;

                int iZ = iIndex;

                float fX = (float)(iX)/(float)(iDimX);
                float fY = (float)(iY)/(float)(iDimY);
                float fZ = (float)(iZ)/(float)(iDimZ);

                resultUnit0 = new ColorRGB(fX, fY, fZ);

                iIndex = kTract.get(i);

                if (m_kImage.isColorImage()) {
                    fR = m_kImage.getFloat(iIndex * 4 + 1) / 255.0f;
                    fG = m_kImage.getFloat(iIndex * 4 + 2) / 255.0f;
                    fB = m_kImage.getFloat(iIndex * 4 + 3) / 255.0f;
                    resultUnit1 = new ColorRGB(fR, fG, fB);
                } else {
                    fR = m_kImage.getFloat(iIndex);
                    resultUnit1 = new ColorRGB(fR, fR, fR);
                }

                bsplineVBuffer.SetPosition3(index, pos_x, pos_y, pos_z);
                bsplineVBuffer.SetColor3(0, index, resultUnit0);
                bsplineVBuffer.SetColor3(1, index, resultUnit1);

                index++;

            }
        }

        return bsplineVBuffer;

    }

    /**
     * B-spline computation. 
     * @param u       u parameter 
     * @param u_2     u^2 parameter
     * @param u_3     u^3 parameter
     * @param cntrl0  1st control point coordinate
     * @param cntrl1  2nd control point coordinate
     * @param cntrl2  3rd control point coordinate
     * @param cntrl3  4th control point coordinate
     * @return  interpolated position
     */
    private float B_SPLINE(float u, float u_2, float u_3, float cntrl0, float cntrl1, float cntrl2, float cntrl3) {

        return (( 
                (-1*u_3 + 3*u_2 - 3*u + 1) * (cntrl0) + 
                ( 3*u_3 - 6*u_2 + 0*u + 4) * (cntrl1) + 
                (-3*u_3 + 3*u_2 + 3*u + 1) * (cntrl2) + 
                ( 1*u_3 + 0*u_2 + 0*u + 0) * (cntrl3)   
        ) / 6f);
    }


    /**
     * Add a polyline to the GPUVolumeRender.
     * 
     * @param kLine
     *            the Polyline to add.
     */
    protected void addPolyline(Polyline kLine) {
        m_kVolumeDisplay.addTract(kLine, m_iBundleCount, centerIndex);
    }

    /** Removes the fiber bundle from the GPUVolumeRender and JPanelSurface. */
    private void removePolyline() {
        int start = 0;
        int[] aiSelected = m_kTractList.getSelectedIndices();

        DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        int iHeaderLength = (new String("FiberBundle")).length();

        for (int i = aiSelected.length-1; i >= 0; i--) {
            if (m_kVolumeDisplay != null) {
            	String kName = ((String) (kList.elementAt(aiSelected[i])));
                int iLength = kName.length();
                int iGroup = (new Integer(kName.substring(iHeaderLength,
                        iLength))).intValue();
                start = 0;
                if ( (aiSelected[i] - 1) != -1 ) {
                    kName = ((String) (kList.elementAt((aiSelected[i] - 1))));
                    iLength = kName.length();
                    start = (new Integer(kName.substring(iHeaderLength,
                            iLength))).intValue();
                }
                System.err.println("start = " + start + " iGroup = " + iGroup);
                for ( int j = start; j < iGroup; j++ ) {
                    m_kVolumeDisplay.removePolyline(j);	
                    m_kVolumeDisplay.removeTractColor(j);
                }
               
                m_kBundleList.remove(new Integer(iGroup));
                System.err.println("iGroup = " + iGroup);
               
            }
            kList.remove(aiSelected[i]);
            m_kVolumeDisplay.removeGroupColor();
        }

        if (kList.size() == 0) {
        	m_kTractList.setSelectedIndex(0);
        
        } else {
        	int index = kList.size()-1;
        	m_kTractList.setSelectedIndex(index);
        }
        updateTractCount();
    }	

    /** Creates the user-interface for the Fiber Bundle Tract panel.
     * @return JPanel containing the user-interface for the Fiber Bundle Tract panel.
     */
    private JPanel createTractPanel()
    {
        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        JScrollPane scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        JPanel kTractPanel = new JPanel(new BorderLayout());

        JPanel buttonPanel = new JPanel();

        // buttons for add/remove of surfaces from list
        JButton addButton = new JButton("Add");

        addButton.addActionListener(this);
        addButton.setActionCommand("Add");
        addButton.setFont(MipavUtil.font12B);
        addButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JButton removeButton = new JButton("Remove");

        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");
        removeButton.setFont(MipavUtil.font12B);
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);

        buttonPanel.add(addButton);
        buttonPanel.add(removeButton);

        // list panel for fiber tract names
        m_kTractList = new JList( new DefaultListModel() );
        m_kTractList.setVisibleRowCount(3);
        m_kTractList.addListSelectionListener(this);
        m_kTractList.setPrototypeCellValue("aaaaaaaaaaaaaaaa.aaa    ");

        JScrollPane kScrollPane = new JScrollPane(m_kTractList);
        JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(buttonPanel, BorderLayout.SOUTH);
        listPanel.setBorder(buildTitledBorder("Fiber Bundle list"));

        JPanel paintTexturePanel = new JPanel();
        paintTexturePanel.setLayout(new FlowLayout(FlowLayout.LEFT));

        m_kColorButton = new JButton("   ");
        m_kColorButton.setToolTipText("Change fiber bundle color");
        m_kColorButtonDefault = m_kColorButton.getBackground( );
        m_kColorButton.addActionListener(this);
        m_kColorButton.setActionCommand("ChangeColor");

        JLabel kColorLabel = new JLabel("Fiber Bundle color");
        kColorLabel.setFont(MipavUtil.font12B);
        kColorLabel.setForeground(Color.black);

        m_kUseVolumeColor = new JCheckBox("Use volume color" );
        m_kUseVolumeColor.addActionListener(this);
        m_kUseVolumeColor.setActionCommand("VolumeColor");
        m_kUseVolumeColor.setSelected(true);

        m_kUseEllipsoids = new JCheckBox("Use Ellipsoids" );
        m_kUseEllipsoids.addActionListener(this);
        m_kUseEllipsoids.setActionCommand("UseEllipsoids");
        m_kUseEllipsoids.setSelected(false);

        m_kAllEllipsoids = new JCheckBox("Display All Ellipsoids" );
        m_kAllEllipsoids.addActionListener(this);
        m_kAllEllipsoids.setActionCommand("AllEllipsoids");
        m_kAllEllipsoids.setSelected(false);
        m_kAllEllipsoids.setEnabled(false);

        m_kDisplaySlider = new JSlider(0, 100, 1);
        m_kDisplaySlider.setEnabled(true);
        m_kDisplaySlider.setMajorTickSpacing(25);
        m_kDisplaySlider.setMinorTickSpacing(5);
        m_kDisplaySlider.setPaintTicks(true);
        m_kDisplaySlider.setPaintLabels(true);
        m_kDisplaySlider.addChangeListener(this);
        m_kDisplaySlider.setVisible(true);

        m_kSliderLabel = new JLabel("Display Glphs every step: ");
        m_kSliderLabel.setFont(MipavUtil.font12B);
        m_kSliderLabel.setForeground(Color.black);


        JPanel colorPanel = new JPanel();
        colorPanel.setLayout(new BorderLayout());
        colorPanel.add(m_kColorButton, BorderLayout.WEST);
        colorPanel.add(kColorLabel, BorderLayout.CENTER);
        colorPanel.add(m_kUseVolumeColor, BorderLayout.EAST);
        colorPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        colorPanel.setAlignmentY(Component.TOP_ALIGNMENT);

        JPanel sliderPanel = new JPanel();
        sliderPanel.setLayout(new BorderLayout());
        sliderPanel.add(m_kSliderLabel, BorderLayout.WEST);
        sliderPanel.add(m_kDisplaySlider, BorderLayout.SOUTH);
        sliderPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        sliderPanel.setAlignmentY(Component.TOP_ALIGNMENT);
        
        
        JLabel glyphsLabel = new JLabel("Glyphs : ");
        glyphsLabel.setFont(MipavUtil.font12B);
        glyphsLabel.setForeground(Color.black);
        
        String[] glyphStrings = { "Lines", "Ellipsoids", "Cylinders", "Tubes", "Arrows" };
        glyphsList = new JComboBox(glyphStrings);
        glyphsList.setSize(new Dimension(20, 22));
        glyphsList.setSelectedIndex(0);
        glyphsList.addActionListener(this);
        
        displayAllCheckBox = new JCheckBox("Display All");
        displayAllCheckBox.setSelected(false);
        displayAllCheckBox.addActionListener(this);
        displayAllCheckBox.setActionCommand("DisplayAll");
        displayAllCheckBox.setEnabled(false);
        
        JPanel glyphsPanel = new JPanel();
        glyphsPanel.setLayout(new BorderLayout());
        glyphsPanel.add(glyphsLabel, BorderLayout.WEST);
        glyphsPanel.add(glyphsList, BorderLayout.CENTER);
        glyphsPanel.add(displayAllCheckBox, BorderLayout.EAST);
        
        

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        JPanel optionsPanel = new JPanel(new GridBagLayout());
        optionsPanel.add(colorPanel, gbc); gbc.gridy++;
        optionsPanel.add(sliderPanel, gbc); gbc.gridy++;
        optionsPanel.add(glyphsPanel, gbc); gbc.gridy++;
        optionsPanel.setBorder(buildTitledBorder("Fiber bundle & Glyph options"));
        
        JPanel rightPanel = new JPanel();
        rightPanel.setLayout(new BorderLayout());
        rightPanel.add(optionsPanel, BorderLayout.NORTH);

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        contentBox.add(listPanel);
        contentBox.add(rightPanel);
        
        mainScrollPanel.add(contentBox, BorderLayout.NORTH);

        kTractPanel.add(scroller, BorderLayout.CENTER);

        return kTractPanel;
    }


    /**
     * Get the main control Panel.
     *
     * @return  mainPanel main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Cancel the color dialog, change nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Do nothing.
         * 
         * @param e
         *            action event
         */
        public void actionPerformed(ActionEvent e) {
        }
    }

    /**
     * Pick up the selected color and call method to change the fiber bundle
     * color.
     */
    class OkColorListener implements ActionListener {

        /**
         * Sets the button color to the chosen color and changes the color of
         * the fiber bundle.
         * 
         * @param e
         *            Event that triggered this method.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = m_kColorChooser.getColor();

            m_kColorButton.setBackground(color);
            setColor(color);
        }
    }    

    /**
     * This is called when the user chooses a new color for the fiber bundle. It
     * changes the color of the fiber bundle.
     * 
     * @param color
     *            Color to change fiber bundle to.
     */
    private void setColor(Color color) {
        int[] aiSelected = m_kTractList.getSelectedIndices();
        DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        int iHeaderLength = (new String("FiberBundle")).length();

        for (int i = 0; i < aiSelected.length; i++) {
            if (m_kVolumeDisplay != null) {
                String kName = ((String) (kList.elementAt(aiSelected[i])));
                int iLength = kName.length();
                int iGroup = (new Integer(kName.substring(iHeaderLength,
                        iLength))).intValue();
                if (color == null) {
                    m_kVolumeDisplay.setPolylineColor(iGroup, null);
                }

                else if (!m_kUseVolumeColor.isSelected()) {
                    m_kVolumeDisplay.setPolylineColor(iGroup, new ColorRGB(
                            color.getRed() / 255.0f, color.getGreen() / 255.0f,
                            color.getBlue() / 255.0f));
                    m_kVolumeDisplay.setTubesGroupColor(iGroup, new ColorRGB(
                            color.getRed() / 255.0f, color.getGreen() / 255.0f,
                            color.getBlue() / 255.0f));
                }
            }
        }
    }


    /**
     * Reads a single fiber bundle tract from disk.
     * 
     * @param kFileReader
     *            FileInputStream.
     * @return Vector<Integer> fiber bundle tract -- list of voxel indices in
     *         order in which they appear in the tract.
     */
    private Vector<Integer> inputTract(FileInputStream kFileReader) {
        int iVQuantity = 0;
        int iBufferSize = 4;

        byte[] racBuffer = new byte[iBufferSize];
        try {
            kFileReader.read(racBuffer, 0, iBufferSize);
        } catch (IOException e1) {
        }
        ByteArrayInputStream acBufferIn = new ByteArrayInputStream(racBuffer);
        DataInputStream acDataIn = new DataInputStream(acBufferIn);
        try {
            iVQuantity = acDataIn.readInt();
        } catch (IOException e) {
            e.printStackTrace();
        }
        acBufferIn = null;
        acDataIn = null;
        racBuffer = null;

        iBufferSize = 4 * iVQuantity;
        racBuffer = new byte[iBufferSize];
        try {
            kFileReader.read(racBuffer, 0, iBufferSize);
        } catch (IOException e1) {
        } catch (OutOfMemoryError e1) {
            System.err.println( iBufferSize );
        }
        acBufferIn = new ByteArrayInputStream(racBuffer);
        acDataIn = new DataInputStream(acBufferIn);

        Vector<Integer> kTract = new Vector<Integer>();
        for (int i = 0; i < iVQuantity; i++) {
            try {
                kTract.add(acDataIn.readInt());
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        acBufferIn = null;
        acDataIn = null;

        return kTract;
    }	

    /**
     * Determines if the input tract is contained within the ModelImage
     * representing user-selected VOIs.
     * 
     * @param kVOIImage
     *            user-selected VOI image.
     * @param kTract
     *            list of voxels in the current fiber bundle tract.
     * @return true if the tract passes through the VOI or if the VOIImage is
     *         null, false otherwise.
     */
    private boolean contains(VolumeTriPlanarInterface.IntVector[] kVOIImage, int iNumVOI, Vector<Integer> kTract) {
        if (kVOIImage == null) {
            return true;
        }
        boolean[] abVisited = new boolean[iNumVOI];

        for ( int j = 0; j < iNumVOI; j++ )
        {
            abVisited[j] = false;
            //System.err.println( m_kVOIParamsList.get(j).ToString() );
        }

        int iVOI;
        for (int i = 0; i < kTract.size(); i++)
        {
            int iIndex = kTract.get(i);
            if ( kVOIImage[iIndex] != null )
            {
                for ( int j = 0; j < kVOIImage[iIndex].size(); j++ )
                {
                    iVOI = kVOIImage[iIndex].get(j);
                    String kName = new String("VOI_" + iVOI);
                    for ( int k = 0; k < m_kVOIParamsList.size(); k++ )
                    {
                        if ( m_kVOIParamsList.get(k).Name.equals(kName) )
                        {
                            abVisited[k] = true;
                        }
                    }
                }
            }
        }
        for ( int j = 0; j < iNumVOI; j++ )
        {
            if ( abVisited[j] && m_kVOIParamsList.get(j).Exclude)
            {
                return false;
            }
            if ( m_kVOIParamsList.get(j).Include && !abVisited[j] )
            {
                return false;
            }
        }
        return true;
    }


    /** Constructs the Fiber Bundle Tracts from the dtiImage and the
     * eigenImage parameters. The fiber bundles are output to a file
     * sepecified by the user.
     * @param dtiImage Diffusion Tensor Image.
     * @param eigenImage EigenVector Image.
     */
    public void diplayTract(int iX, int iY, int iZ)
    {
        //System.err.println( "Picked " + iX + " " + iY + " " + iZ );
        m_kDTIImage = parentFrame.getDTIimage();
        int iDimX = m_kDTIImage.getExtents()[0];
        int iDimY = m_kDTIImage.getExtents()[1];
        int iDimZ = m_kDTIImage.getExtents()[2];
        int iLen = m_kDTIImage.getExtents()[0] *
        m_kDTIImage.getExtents()[1] * m_kDTIImage.getExtents()[2];
        float[] afVectorData = new float[3];
/*
        iX = iDimX/3;
        iY = iDimY/3;
        iZ = iDimZ/3;
  */      
        m_abVisited  = new boolean[iLen];
        for ( int i = 0; i < iLen; i++ )
        {
            m_abVisited[i] = false;
        }

        Vector<Integer> kTract = new Vector<Integer>();
        Vector3f kPos = new Vector3f();
        Vector3f kV1 = new Vector3f();
        Vector3f kV2 = new Vector3f();

        int i = iZ * (iDimY*iDimX) + iY * iDimX + iX;
        centerIndex = i;

        boolean bAllZero = true;
        for ( int j = 0; j < 3; j++ )
        {
            afVectorData[j] = parentFrame.getEVimage().getFloat(i + j*iLen);
            if ( afVectorData[j] != 0 )
            {
                bAllZero = false;
            }
        }
        
        if ( m_kNegX.isSelected() )
        {
            afVectorData[0] *= -1;
        }
        if ( m_kNegY.isSelected() )
        {
            afVectorData[1] *= -1;
        }
        if ( m_kNegZ.isSelected() )
        {
            afVectorData[2] *= -1;
        }
        
        if ( !bAllZero )
        {        
            kPos.Set( iX, iY, iZ );
            kTract.add(i);

            kV1.Set( afVectorData[0], afVectorData[1], afVectorData[2] );
            kV2.Copy(kV1);
            kV2.Neg();

            kV1.Normalize();
            kV2.Normalize();

            traceTract2( kTract, new Vector3f(kPos), new Vector3f(kV1), 
                    parentFrame.getEVimage(), parentFrame.getEValueimage(), parentFrame.getFAimage(), true );

            //traceTract( kTract, kPos, kV1, m_kDTIImage, true );
            m_abVisited[i] = true;
            //traceTract( kTract, kPos, kV2, m_kDTIImage, false );
            traceTract2( kTract, new Vector3f(kPos), new Vector3f(kV2), 
                    parentFrame.getEVimage(), parentFrame.getEValueimage(), parentFrame.getFAimage(), false );
            int iVQuantity = kTract.size();
            addTract(kTract, iVQuantity, iDimX, iDimY, iDimZ);
        }
    }

    /** Traces a single fiber bundle tract starting at the input
     * position and following the input direction.
     * @param kTract fiber bundle tract, new positions are stored in this tract as the fiber is traced.
     * @param kStart starting position of the tract.
     * @param kDir direction from the position.
     * @param dtiImage Diffusion Tensor image used to calculate next direction of tract.
     * @param bDir boolean when true the positions are added to the
     * end of the tract (positive direction). When false the positions
     * are added to the beginning of the tract (negative direction).
     */
    private void traceTract( Vector<Integer> kTract, Vector3f kStart, Vector3f kDir,
            ModelImage dtiImage, boolean bDir )
    {
        int iDimX = dtiImage.getExtents()[0];
        int iDimY = dtiImage.getExtents()[1];
        int iDimZ = dtiImage.getExtents()[2];
        int iLen = dtiImage.getExtents()[0] * dtiImage.getExtents()[1] * dtiImage.getExtents()[2];

        float[] afTensorData = new float[6];

        boolean bDone = false;
        Matrix3f kMatrix = new Matrix3f();
        Vector3f kOut = new Vector3f();
        Vector3f kNext = new Vector3f();
        int iX;
        int iY;
        int iZ;
        int i;
        boolean bAllZero = true;

        Matrix3f kEigenValues = new Matrix3f();
        Vector3f kV0 = new Vector3f();
        Vector3f kV1 = new Vector3f();
        Vector3f kV2 = new Vector3f();
        
        while ( !bDone )
        {
        	kNext.Add( kStart, kDir );
            iX = Math.round(kNext.X);
            iY = Math.round(kNext.Y);
            iZ = Math.round(kNext.Z);
            i = iZ * (iDimY*iDimX) + iY * iDimX + iX;

            if ( (iZ < 0) || (iZ >= iDimZ) ||
                    (iY < 0) || (iY >= iDimY) ||
                    (iX < 0) || (iX >= iDimX)  )
            {
                bDone = true;
                break;
            }

            bAllZero = true;
            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = dtiImage.getFloat(i + j*iLen);
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( !bAllZero )
            {
                kMatrix.Set( afTensorData[0], afTensorData[3], afTensorData[4],
                        afTensorData[3], afTensorData[1], afTensorData[5], 
                        afTensorData[4], afTensorData[5], afTensorData[2] );
                
                kMatrix.Mult(kDir, kOut);
                kOut.Normalize();

//                 if ( kDir.Angle(kOut) > (3.0f*Math.PI/4.0f) )
//                 {
//                 }
//                 else if ( kDir.Angle(kOut) > (Math.PI/2.0f) )
//                 {
//                     kOut.Neg();
//                 }
                        
                        
                if ( m_abVisited[i] )
                {
                    bDone = true;
                    break;
                }
                m_abVisited[i] = true;

                if ( bDir )
                {
                    kTract.add( i );
                }
                else
                {
                    kTract.add( 0, i );
                }
                //System.err.println( iX + " " + iY + " " + iZ );

                //kStart = kNext;
                //kDir = kOut;
                kStart.Copy(kNext);
                kDir.Copy(kOut);

                
                /*
                if ( Matrix3f.EigenDecomposition( kMatrix, kEigenValues ) )
                {
                    float fLambda1 = kEigenValues.M22;
                    float fLambda2 = kEigenValues.M11;
                    float fLambda3 = kEigenValues.M00;
                    kMatrix.GetColumn(2,kV0);
                    kMatrix.GetColumn(1,kV1);
                    kMatrix.GetColumn(0,kV2);

                    //kV0.Normalize();
                    //kV1.Normalize();
                    //kV2.Normalize();

                    //kMatrix.SetColumn(0,kV1);
                    //kMatrix.SetColumn(1,kV2);
                    //kMatrix.SetColumn(2,kV3);
                    //kMatrix = new Matrix3f(kV1,kV2,kV3,false);

                    if ( (fLambda1 == fLambda2) && (fLambda1 == fLambda3) )
                    {
                        kStart.Copy(kNext);
                    }
                    else if ( (fLambda1 > 0) && (fLambda2 > 0) && (fLambda3 > 0) )
                    {
                        kMatrix = new Matrix3f(kV0,kV1,kV2,false);
                        kMatrix.Mult(kDir, kOut);
                        kOut.Normalize();

                        if ( kDir.Angle(kOut) > (3.0f*Math.PI/4.0f) )
                        {
                        }
                        else if ( kDir.Angle(kOut) > (Math.PI/2.0f) )
                        {
                            kOut.Neg();
                        }
                        
                        
                        if ( m_abVisited[i] )
                        {
                            bDone = true;
                            break;
                        }
                        m_abVisited[i] = true;

                        if ( bDir )
                        {
                            kTract.add( i );
                        }
                        else
                        {
                            kTract.add( 0, i );
                        }
                        //System.err.println( iX + " " + iY + " " + iZ );

                        //kStart = kNext;
                        //kDir = kOut;
                        kStart.Copy(kNext);
                        kDir.Copy(kOut);
                    }
                    else
                    {
                        bDone = true;
                        break;
                    }
                }
                else
                {
                    bDone = true;
                    break;
                }
                */
            }
            else
            {
                bDone = true;
                break;
            }
        }
        kNext = null;
    }    
    
    
    private void traceTract2( Vector<Integer> kTract, Vector3f kStart, Vector3f kDir,
            ModelImage eigenImage, ModelImage eigenValueImage, ModelImage kFAImage, boolean bDir )
    {
        int iDimX = eigenImage.getExtents()[0];
        int iDimY = eigenImage.getExtents()[1];
        int iDimZ = eigenImage.getExtents()[2];
        int iLen = eigenImage.getExtents()[0] * eigenImage.getExtents()[1] * eigenImage.getExtents()[2];

        float[] afVectorData = new float[6];

        boolean bDone = false;
        Vector3f kOut = new Vector3f();
        Vector3f kNext = new Vector3f();
        int iX, iY, iZ, i;
        float fLambda1, fLambda2, fLambda3;
        float fDot, fAngle, fFA;
        boolean bAllZero = true;

        while ( !bDone )
        {
            kNext.Add( kStart, kDir );
            iX = Math.round(kNext.X);
            iY = Math.round(kNext.Y);
            iZ = Math.round(kNext.Z);
            i = iZ * (iDimY*iDimX) + iY * iDimX + iX;

            if ( (iZ < 0) || (iZ >= iDimZ) ||
                    (iY < 0) || (iY >= iDimY) ||
                    (iX < 0) || (iX >= iDimX)  )
            {
                bDone = true;
                break;
            }

            if ( kFAImage != null )
            {
                fFA = kFAImage.getFloat(i);
                if ( (fFA < m_fFAMin) || (fFA > m_fFAMax) )
                {
                    bDone = true;
                    break;
                }
            }
            
            bAllZero = true;
            for ( int j = 0; j < 3; j++ )
            {
                afVectorData[j] = eigenImage.getFloat(i + j*iLen);
                if ( afVectorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( m_kNegX.isSelected() )
            {
                afVectorData[0] *= -1;
            }
            if ( m_kNegY.isSelected() )
            {
                afVectorData[1] *= -1;
            }
            if ( m_kNegZ.isSelected() )
            {
                afVectorData[2] *= -1;
            }

            fLambda1 = eigenValueImage.getFloat(i*4+1);
            fLambda2 = eigenValueImage.getFloat(i*4+2);
            fLambda3 = eigenValueImage.getFloat(i*4+3);
            

            if ( (fLambda1 == fLambda2) && (fLambda1 == fLambda3) )
            {
                bDone = true;
                break;
            }
            else if ( !bAllZero && (fLambda1 > 0) && (fLambda2 > 0) && (fLambda3 > 0) )
            {
                kOut.Set( afVectorData[0], afVectorData[1], afVectorData[2] );
                fDot = kDir.Dot( kOut );
                if ( fDot < 0 )
                {
                    kOut.Neg();
                }
                fAngle = Vector3f.Angle(kDir,kOut);
                if ( fAngle > m_fMaxAngle )
                {
                    bDone = true;
                    break;
                }

                if ( m_abVisited[i] )
                {
                    bDone = true;
                    break;
                }
                m_abVisited[i] = true;

                if ( bDir )
                {
                    kTract.add( i );
                }
                else
                {
                    kTract.add( 0, i );
                }
                kStart.Copy(kNext);
                kDir.Copy(kOut);
            }
            else
            {
                bDone = true;
                break;
            }
        }
        kNext = null;
    }    
    
    
    /**
     * set the display volume color flags in the renderer. 
     */
    public void setVolumeColor() { 
    	m_kVolumeDisplay.setVolumeColor(m_kUseVolumeColor.isSelected());
    }

    
    public void invokeDisplayFunction() {
        if ( displayMode == Ellipzoids && displayAll == false) {
            parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayEllipsoids(true);
            m_kVolumeDisplay.setDisplayAllGlyphs(false);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayTubes(false);
            m_kVolumeDisplay.setDisplayArrows(false);
        } else if (displayMode == Ellipzoids && displayAll == true) {
            parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayAllGlyphs(true);
            m_kVolumeDisplay.setDisplayEllipsoids(true);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayTubes(false);
            m_kVolumeDisplay.setDisplayArrows(false);
        } else if (displayMode == Cylinders && displayAll == false) {
            parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayCylinders(true);
            m_kVolumeDisplay.setDisplayAllGlyphs(false);
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayTubes(false);
            m_kVolumeDisplay.setDisplayArrows(false);
        } else if (displayMode == Cylinders && displayAll == true) {
            parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayAllGlyphs(true);
            m_kVolumeDisplay.setDisplayCylinders(true);
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayTubes(false);
            m_kVolumeDisplay.setDisplayArrows(false);
        }  else if (displayMode == Tubes /* && displayAll == false */) {
            parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayTubes(true);
            m_kVolumeDisplay.setDisplayAllGlyphs(false);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayArrows(false);
        }  else if (displayMode == Arrows  && displayAll == false ) {
            parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayTubes(false);
            m_kVolumeDisplay.setDisplayAllGlyphs(false);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayArrows(true);
        }  else if (displayMode == Arrows  && displayAll == true ) {
            parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayTubes(false);
            m_kVolumeDisplay.setDisplayAllGlyphs(true);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayArrows(true);
        }  else if (displayMode == Polylines ) {
        	parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayAllGlyphs(false);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayTubes(false);
            m_kVolumeDisplay.setDisplayArrows(false);
        }
    }


}
