package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.BitSet;
import java.util.HashMap;
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
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.Picker;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.BoxBV;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public class JPanelDTIParametersPanel extends JInterfaceBase 
implements ListSelectionListener, ChangeListener {
    private class VOIParams {
        String Name;
        boolean Include;
        boolean Exclude;
        boolean Ignore;
        public VOIParams() {}
        public String toString()
        {
            return new String( Name + " " + Include + " " + Exclude + " " + Ignore );
        }
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
     * 
     */
    private static final long serialVersionUID = 2778749302064237729L;

    /** The list box in the dialog for fiber bundle tracts. */
    private JList m_kTractList;
    /** The list box in the dialog for 3D VOIs. */
    private JList m_kVOIList;
    /** Color button for changing the color of the fiber bundles. */
    private JButton m_kColorButton;

    /** Checkbox for turning on/off volume color for the polylines. */
    private JCheckBox m_kUseVolumeColor;
    /** Checkbox for switching between polylines and ellipsoids. */
    private JCheckBox m_kUseEllipsoids;

    /** Checkbox for displaying all tensors as ellipsoids. */
    private JCheckBox m_kAllEllipsoids;

    /** User-control over the number of glyphs displayed in GPUVolumeRender */
    private JSlider m_kDisplaySlider;

    private JLabel m_kSliderLabel;

    private VolumeTriPlanarInterfaceDTI parentFrame;

    private VolumeTriPlanarRender m_kVolumeDisplay;

    private ModelImage m_kDTIImage;

    /** When selected, only tracts that intersect the VOI are displayed. */
    private JCheckBox m_kUseVOICheck = null;

    /** Keeps track of the groups of polylines loaded. */
    private Vector<Integer> m_kBundleList = new Vector<Integer>();

    /** Number of currently loaded fiber bundle groups. */
    private int m_iBundleCount = 0;

    private ModelImage m_kImage;

    private JCheckBox m_kNegX;
    private JCheckBox m_kNegY;
    private JCheckBox m_kNegZ;
    private JTextField m_kFAMinThreshold;
    
    private JTextField m_kFAMaxThreshold;
    private JTextField m_kMaxAngle;
    private JTextField m_kMinLength;
    private JTextField m_kVoxelStepsize;
    //private JCheckBox displayAllCheckBox;
    private int displayMode;
    private static int Polylines = 0;
    private static int Ellipsoids = 1;
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
    
    private int m_iMinTractLength;
    
    /** list to hold the glyphs type name */
    private JComboBox glyphsList;

    private Vector<VOIParams> m_kVOIParamsList = null;
    
    /**
     * Color chooser for when the user wants to change the color of the fiber
     * bundle tracts.
     */
    private ViewJColorChooser m_kColorChooser;


    private float m_fFraction = .1f;

    private HashMap<String,BitSet> m_kSurfaceImages = new HashMap<String,BitSet>();

    public JPanelDTIParametersPanel(VolumeTriPlanarInterfaceDTI _parentFrame, VolumeTriPlanarRender _m_kVolumeDisplay,
    		String tractFileName ) {
        parentFrame = _parentFrame;
        m_kVolumeDisplay = _m_kVolumeDisplay;
        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
       
        mainPanel.add(createTractDialog(), BorderLayout.NORTH);
        mainPanel.add(createTractPanel(), BorderLayout.CENTER );
    }

    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command;       
        
        if (source instanceof JComboBox) {
			JComboBox cb = (JComboBox)source;
			command = (String) cb.getSelectedItem();
			if (command.equals("Lines")) {
				displayMode = Polylines;
				//displayAllCheckBox.setEnabled(false);
			} else if (command.equals("Ellipsoids")) {
				displayMode = Ellipsoids;
				//displayAllCheckBox.setEnabled(true);
			} else if (command.equals("Cylinders")) {
				displayMode = Cylinders;
				//displayAllCheckBox.setEnabled(false);
			} else if (command.equals("Tubes")) {
				displayMode = Tubes;
				//displayAllCheckBox.setEnabled(true);
			} else if (command.equals("Arrows")) {
				displayMode = Arrows;
				//displayAllCheckBox.setEnabled(true);
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
				//displayAll = displayAllCheckBox.isSelected();
				invokeDisplayFunction();
			} else if (command.equals("Add")) {		    
				createNewTracts();
			} else if (command.equals("Remove")) {
				removePolyline();
			}  else if (command.equals("Load")) {
				parentFrame.getSurfacePanel().addPolyline();
			}  else if (command.equals("Save")) {
				//parentFrame.getSurfacePanel().savePolyline();
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
	        else if ( command.equals( "MinLengthChanged" ) )
	        {
	        	if (!JDialogBase.testParameterMin(m_kMinLength.getText(), 1))
	            {
	        		m_kMinLength.requestFocus();
	        		m_kMinLength.selectAll();
	            }
	            else
	            {
	                m_iMinTractLength = Integer.valueOf(m_kMinLength.getText()).intValue();
	            }
	        }
	        else if ( command.equals( "VoxelStepsizeChanged" ) )
	        {
	        	if (!JDialogBase.testParameter(m_kVoxelStepsize.getText(), 0.10f, 1.0f))
	            {
	        		m_kVoxelStepsize.requestFocus();
	        		m_kVoxelStepsize.selectAll();
	            }
	            else
	            {
	            	m_fFraction = Float.valueOf(m_kVoxelStepsize.getText()).floatValue();
	            }
	        }

		}
        
      
    }
    
    public void add3DVOI( String kVOIName, VolumeSurface kSurface )
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
    	//BitSet mask = kSurface.createMask();
    }
    
    public void addFiberTract() {
            addTract();
    }
    
    /** Updates the tract list user-interface. */
    public void addTract() {
    	// m_iBundleCount--;
    	if ( m_iBundleCount != 0 )
    	{
    		m_kVolumeDisplay.addGroupColor();
    		DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
    		int iSize = kList.getSize();
    		kList.add(iSize, new String("FiberBundle" + m_iBundleCount));
    		m_kTractList.setSelectedIndex(iSize);
    	}
    }
    
    /** Constructs the Fiber Bundle Tracts from the dtiImage and the
     * eigenImage parameters. The fiber bundles are output to a file
     * sepecified by the user.
     * @param dtiImage Diffusion Tensor Image.
     * @param eigenImage EigenVector Image.
     */
    public void diplayTract(int iX, int iY, int iZ)
    {

    	m_fFraction = Float.valueOf(m_kVoxelStepsize.getText()).floatValue();
        m_iMinTractLength = Integer.valueOf(m_kMinLength.getText()).intValue();
        m_fFAMin = Float.valueOf(m_kFAMinThreshold.getText()).floatValue();
        m_fFAMax = Float.valueOf(m_kFAMaxThreshold.getText()).floatValue();

        m_fMaxAngle = Float.valueOf(m_kMaxAngle.getText()).floatValue();
        m_fMaxAngle = (float)(m_fMaxAngle*Math.PI/180.0f);
        
        
        m_kDTIImage = parentFrame.getDTIimage();
        int iDimX = m_kDTIImage.getExtents()[0];
        int iDimY = m_kDTIImage.getExtents()[1];
        int iDimZ = m_kDTIImage.getExtents()[2];
    	diplayTract( iX, iY, iZ, iDimX, iDimY, iDimZ, false );
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
    	if ( m_kSurfaceImages != null )
    	{
    		m_kSurfaceImages.clear();
    		m_kSurfaceImages = null;
    	}
       
       
       if ( m_kVOIParamsList != null )
    	   m_kVOIParamsList = null;

       
    }


    /**
     * Get the main control Panel.
     *
     * @return  mainPanel main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
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

    public void setSurfaceImage( String kName, ModelImage kImage )
    {
    	//m_kSurfaceImages.put( kName, kImage );
    }

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


    public void updateCounter() {
            updateTractCount();
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
                if ( color != null )
                {
                	m_kColorButton.setBackground(new Color(color.R, color.G, color.B));
                }
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

    /**
     * Add a polyline to the GPUVolumeRender.
     * 
     * @param kLine
     *            the Polyline to add.
     */
    private void addPolyline(Polyline kLine) {
        m_kVolumeDisplay.addTract(kLine, m_iBundleCount, centerIndex);
    }


    /** Adds a fiber bundle tract to the GPUVolumeRender and JPanelSurface.
     * @param kTract list of voxels in the fiber bundle.
     * @param iVQuantity number of voxels in the fiber bundle.
     * @param iDimX the x-dimensions of the DTI image used to create the tract.
     * @param iDimY the y-dimensions of the DTI image used to create the tract.
     * @param iDimZ the z-dimensions of the DTI image used to create the tract.
     */
    private void addTract( VOIContour kTract, int iVQuantity, int iDimX, int iDimY, int iDimZ )
    {
        m_kImage = parentFrame.getImageA();
        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];
        
        float fMaxX = (iXBound - 1) * m_kImage.getFileInfo(0).getResolutions()[0];
        float fMaxY = (iYBound - 1) * m_kImage.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (iZBound - 1) * m_kImage.getFileInfo(0).getResolutions()[2];

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
            	Vector3f kPos = kTract.elementAt(i);
                int iX = Math.round(kPos.X);
                int iY = Math.round(kPos.Y);
                int iZ = Math.round(kPos.Z);
                int iIndex = iZ * (iDimY*iDimX) + iY * iDimX + iX;

                if ( loadingTrack == true && i == (iVQuantity/2) ) {
                    centerIndex = iIndex;
                }
                ColorRGB kColor1;
                if ( m_kImage.isColorImage() )
                {
                    fR = m_kImage.getFloatTriLinearBounds( kPos.X, kPos.Y, kPos.Z, 1 )/255.0f;
                    fG = m_kImage.getFloatTriLinearBounds( kPos.X, kPos.Y, kPos.Z, 2 )/255.0f;
                    fB = m_kImage.getFloatTriLinearBounds( kPos.X, kPos.Y, kPos.Z,+ 3 )/255.0f;
                    kColor1 = new ColorRGB(fR, fG, fB);
                }
                else
                {
                    fR = m_kImage.getFloatTriLinearBounds( kPos.X, kPos.Y, kPos.Z );
                    kColor1 = new ColorRGB(fR, fR, fR);
                }

                                
                pkVBuffer.SetPosition3(i, kPos);
                pkVBuffer.SetColor3(0,i, new ColorRGB(kPos.X, kPos.Y, kPos.Z));
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
            addPolyline(new Polyline(pkVBuffer, bClosed, bContiguous ));
            //addPolyline(new Polyline(smoothTrack(pkVBuffer, kTract,iVQuantity, iDimX, iDimY, iDimZ), bClosed, bContiguous ));
            m_kBundleList.add(new Integer(m_iBundleCount));
            m_iBundleCount++;
        }
    }

    private void createNewTracts()
    {

    	m_fFraction = Float.valueOf(m_kVoxelStepsize.getText()).floatValue();
        m_iMinTractLength = Integer.valueOf(m_kMinLength.getText()).intValue();
        m_fFAMin = Float.valueOf(m_kFAMinThreshold.getText()).floatValue();
        m_fFAMax = Float.valueOf(m_kFAMaxThreshold.getText()).floatValue();

        m_fMaxAngle = Float.valueOf(m_kMaxAngle.getText()).floatValue();
        m_fMaxAngle = (float)(m_fMaxAngle*Math.PI/180.0f);
    	
        ViewJProgressBar kProgressBar =
            new ViewJProgressBar("Calculating Fiber Bundle Tracts ",
                                 "Calculating tracts...", 0, 100, true);
        
    	ModelImage fAImage = parentFrame.getFAimage();
    	int iDimX = fAImage.getExtents()[0];
    	int iDimY = fAImage.getExtents()[1];
    	int iDimZ = fAImage.getExtents()[2];
    	
    	float faVal, fR, fG, fB;
    	int count = 0;
    	
    	if ( m_kUseVOICheck.isSelected() )
    	{
    		// Get the list of VOIs:
            for ( int i = 0; i < m_kVOIParamsList.size(); i++ )
            {
            	// Add tracts from the include VOIs, check for exclusion while adding tracts...
            	if ( m_kVOIParamsList.get(i).Include )
            	{
            		BitSet kVOI = m_kSurfaceImages.get(m_kVOIParamsList.get(i).Name);
            		addTracts(kVOI);
            	}
            }
    	}
    	else
    	{
    		for ( int z = 0; z < iDimZ; z++ )
    		{
    			for ( int y = 0; y < iDimY; y++ )
    			{
    				for ( int x = 0; x < iDimX; x++ )
    				{
    					faVal = fAImage.getFloat(z*iDimX*iDimY + y*iDimX + x);
    					if ( (faVal >= m_fFAMin) && (faVal <= m_fFAMax) )
    					{
    						count += diplayTract(x,y,z, iDimX, iDimY, iDimZ, true);
    					}
    				}
    			}
    			int iValue = (int)(100 * (float)(z+1)/iDimZ);
    			kProgressBar.updateValueImmed( iValue );
    		}
    	}
        kProgressBar.dispose();
        if ( count > 0 )
        {
        	addTract();
        }
        MipavUtil.displayInfo( "Added " + count + " fiber tracts." );
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
        //kParamsPanel.add(m_kUseVOICheck, gbc);

        JLabel slicePicckableLabel = new JLabel("Ctrl and left mouse press to select the individual tract.");

        JPanel slicePanel = new JPanel();
        slicePanel.setLayout(new BorderLayout());
        slicePanel.add(slicePicckableLabel, BorderLayout.WEST);
        slicePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        slicePanel.setAlignmentY(Component.TOP_ALIGNMENT);

        /*
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
        
*/

        m_kFAMinThreshold = new JTextField( Double.toString(parentFrame.getFAimage().getMin()), 4);
        m_kFAMinThreshold.setActionCommand("FAMINChanged");
        m_kFAMinThreshold.addActionListener(this);
        m_kFAMaxThreshold = new JTextField( Double.toString(parentFrame.getFAimage().getMax()), 4);
        m_kFAMaxThreshold.setActionCommand("FAMAXChanged");
        m_kFAMaxThreshold.addActionListener(this);
        m_kMaxAngle = new JTextField("45", 4);
        m_kMaxAngle.setActionCommand("MaxAngleChanged");
        m_kMaxAngle.addActionListener(this);
        m_kMinLength = new JTextField("20", 4);
        m_kMinLength.setActionCommand("MinLengthChanged");
        m_kMinLength.addActionListener(this);
        m_kVoxelStepsize = new JTextField("0.5", 4);
        m_kVoxelStepsize.setActionCommand("VoxelStepsizeChanged");
        m_kVoxelStepsize.addActionListener(this);
        JPanel kTrackPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        //kTrackPanel.add( kVectorPanel, gbc );
        //gbc.gridy++;
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
        gbc.gridx = 0;
        gbc.gridy++;
        kTrackPanel.add(new JLabel( "Minimum tract length to display:"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add( m_kMinLength, gbc );
        gbc.gridx = 0;
        gbc.gridy++;
        kTrackPanel.add(new JLabel( "Voxel track step size (0.1-1.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add( m_kVoxelStepsize, gbc );
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
        listPanel.add(scrollPanel, BorderLayout.NORTH);
        listPanel.add(buttonPanel, BorderLayout.CENTER);
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
     * Reads a single fiber bundle tract from disk.
     * 
     * @param kFileReader
     *            FileInputStream.
     * @return Vector<Integer> fiber bundle tract -- list of voxel indices in
     *         order in which they appear in the tract.
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
     */

    /** Creates the user-interface for the Fiber Bundle Tract panel.
     * @return JPanel containing the user-interface for the Fiber Bundle Tract panel.
     */
    private JPanel createTractPanel()
    {
        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        JScrollPane scroller = new JScrollPane(mainScrollPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

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

        JButton loadButton = new JButton("Load");
        loadButton.addActionListener(this);
        loadButton.setActionCommand("Load");
        loadButton.setFont(MipavUtil.font12B);
        loadButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JButton saveButton = new JButton("Save");
        saveButton.addActionListener(this);
        saveButton.setActionCommand("Save");
        saveButton.setFont(MipavUtil.font12B);
        saveButton.setPreferredSize(MipavUtil.defaultButtonSize);

        buttonPanel.add(addButton);
        buttonPanel.add(removeButton);
        //buttonPanel.add(loadButton);
        //buttonPanel.add(saveButton);

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
        /*
        displayAllCheckBox = new JCheckBox("Display All");
        displayAllCheckBox.setSelected(false);
        displayAllCheckBox.addActionListener(this);
        displayAllCheckBox.setActionCommand("DisplayAll");
        displayAllCheckBox.setEnabled(false);
        */
        JPanel glyphsPanel = new JPanel();
        glyphsPanel.setLayout(new BorderLayout());
        glyphsPanel.add(glyphsLabel, BorderLayout.WEST);
        glyphsPanel.add(glyphsList, BorderLayout.CENTER);
        //glyphsPanel.add(displayAllCheckBox, BorderLayout.EAST);
        
        

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
    private int diplayTract(int iX, int iY, int iZ, int iDimX, int iDimY, int iDimZ, boolean bUseSizeLimit)
    {
        //System.err.println( "Picked " + iX + " " + iY + " " + iZ );
/*
        iX = iDimX/3;
        iY = iDimY/3;
        iZ = iDimZ/3;
  */      

    	int count = 0;
        int iLen = iDimX * iDimY * iDimZ;
        float[] afVectorData = new float[3];
        VOIContour kTract = new VOIContour(false);
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
        /*
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
        */
        
        if ( !bAllZero )
        {        
            kPos.Set( iX, iY, iZ );
            kTract.add( new Vector3f( kPos ) );

            kV1.Set( afVectorData[0], afVectorData[1], afVectorData[2] );
            kV2.Copy(kV1);
            kV2.Neg();

            kV1.Normalize();
            kV2.Normalize();

            traceTract2( kTract, new Vector3f( kPos ), new Vector3f( kV1 ), 
                    parentFrame.getEVimage(), parentFrame.getEValueimage(), parentFrame.getFAimage(), true );

            traceTract2( kTract, new Vector3f( kPos ), new Vector3f( kV2 ), 
                    parentFrame.getEVimage(), parentFrame.getEValueimage(), parentFrame.getFAimage(), false );
            int iVQuantity = kTract.size();
            if ( (iVQuantity*m_fFraction >= m_iMinTractLength) )
            {
            	//System.err.println( "Adding " + kTract.size() + " " + m_fFraction + " " + (iVQuantity*m_fFraction) );
            	addTract(kTract, iVQuantity, iDimX, iDimY, iDimZ);
            	count++;
            }
            else
            {
            	//System.err.println( "NOT Adding " + kTract.size() + " " + m_fFraction + " " + (iVQuantity*m_fFraction) );
            }
        }
        else
        {
        	//System.err.println( "Tensor at " + iX + " " + iY + " " + iZ + " is all 0" );
        }
        return count;
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

    private void invokeDisplayFunction() {    	
        parentFrame.getLightControl().refreshLighting();
        m_kVolumeDisplay.setDisplayEllipsoids( displayMode == Ellipsoids );
        m_kVolumeDisplay.setDisplayAllGlyphs( false );
        m_kVolumeDisplay.setDisplayCylinders( displayMode == Cylinders );
        m_kVolumeDisplay.setDisplayTubes( displayMode == Tubes );
        m_kVolumeDisplay.setDisplayArrows( displayMode == Arrows );
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
                //System.err.println("start = " + start + " iGroup = " + iGroup);
                for ( int j = start; j < iGroup; j++ ) {
                    m_kVolumeDisplay.removePolyline(j);	
                    m_kVolumeDisplay.removeTractColor(j);
                }
               
                m_kBundleList.remove(new Integer(iGroup));
                //System.err.println("iGroup = " + iGroup);
               
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

    private void traceTract2( VOIContour kTract, Vector3f kStart, Vector3f kDir,
            ModelImage eigenImage, ModelImage eigenValueImage, ModelImage kFAImage, boolean bDir )
    {
        int iDimX = eigenImage.getExtents()[0];
        int iDimY = eigenImage.getExtents()[1];
        int iDimZ = eigenImage.getExtents()[2];

        float[] afVectorData = new float[6];

        boolean bDone = false;
        Vector3f kOut = new Vector3f();
        Vector3f kNext = new Vector3f();
        int iX, iY, iZ;
        float fLambda1, fLambda2, fLambda3;
        float fDot, fAngle, fFA;
        boolean bAllZero = true;

        VOIContour currentPoints = new VOIContour(false);
        int maxInVoxel = 2*(int)Math.ceil(1f/m_fFraction);
        int count = 0;
        
        while ( !bDone )
        {
            kNext.ScaleAdd( m_fFraction, kDir, kStart ); // next = start + stepSize*dir
            iX = Math.round(kNext.X);
            iY = Math.round(kNext.Y);
            iZ = Math.round(kNext.Z);
            Vector3f voxel = new Vector3f(iX,iY,iZ);
            if ( currentPoints.contains(voxel) )
            {
            	count++;
            	//System.err.println(count);
            }
            else
            {
            	count = 0;
                currentPoints.add( voxel );
            }
            if ( count >= maxInVoxel )
            {
            	bDone = true;
                //System.err.println( "Same voxel " + count + " times > " + maxInVoxel );
                break;
            }
            

            if ( (iZ < 0) || (iZ >= iDimZ) ||
                    (iY < 0) || (iY >= iDimY) ||
                    (iX < 0) || (iX >= iDimX)  )
            {
                bDone = true;
                //System.err.println( "Track OUT of BOUNDS" );
                break;
            }

            if ( kFAImage != null )
            {
                fFA = kFAImage.getFloatTriLinearBounds(kNext.X, kNext.Y, kNext.Z);
                //fFA = kFAImage.getFloat(iX, iY, iZ);
                if ( (fFA < m_fFAMin) || (fFA > m_fFAMax) )
                {
                    bDone = true;
                    //System.err.println( "Track FA" );
                    break;
                }
            }
            
            bAllZero = true;
            for ( int j = 0; j < 3; j++ )
            {
                afVectorData[j] = eigenImage.getFloatTriLinearBoundsTime(kNext.X, kNext.Y, kNext.Z, j);
                //afVectorData[j] = parentFrame.getEVimage().getFloat(i + j*iLen);
                //afVectorData[j] = eigenImage.getFloat(iX, iY, iZ, j);
                if ( afVectorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            /*
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
*/
            fLambda1 = eigenValueImage.getFloatTriLinearBounds(kNext.X, kNext.Y, kNext.Z, 1);
            fLambda2 = eigenValueImage.getFloatTriLinearBounds(kNext.X, kNext.Y, kNext.Z, 2);
            fLambda3 = eigenValueImage.getFloatTriLinearBounds(kNext.X, kNext.Y, kNext.Z, 3);
            //fLambda1 = eigenValueImage.getFloat( i * 4 + 1 );
            //fLambda2 = eigenValueImage.getFloat( i * 4 + 2 );
            //fLambda3 = eigenValueImage.getFloat( i * 4 + 3 );
            

            if ( (fLambda1 == fLambda2) && (fLambda1 == fLambda3) )
            {
                bDone = true;
                //System.err.println( "Track ISOTROPIC" );
                break;
            }
            else if ( !bAllZero && (fLambda1 > 0) && (fLambda2 > 0) && (fLambda3 > 0)  )
            {
                kOut.Set( afVectorData[0], afVectorData[1], afVectorData[2] );
                kOut.Normalize();

                if ( !bDir )
                {
                    kOut.Neg();
                }
                fDot = kDir.Dot( kOut );
                if ( fDot < 0 )
                {
                    kOut.Neg();
                } 
                
                
                fAngle = Vector3f.Angle(kDir,kOut);
                if ( fAngle > m_fMaxAngle )
                {
                    bDone = true;
                    //System.err.println( "Track MAX ANGLE: " + (fAngle * 360f) / (2f * Math.PI) + " " + (m_fMaxAngle * 360f) / (2f * Math.PI) );
                    //System.err.println( kDir + "         " + kOut );
                    break;
                }

                //if ( m_abVisited[i] == null )
                //{
                //	m_abVisited[i] = new VOIContour(false);
                //}
                //if ( m_abVisited[i].contains( kStart ) )
                //{
                //    bDone = true;
                //    break;
                //}
                //m_abVisited[i].add( kStart );

                if ( kTract.contains( kNext ) )
                {
                	bDone = true;
                    //System.err.println( "Track REPEAT" );
                	break;
                }
            	//System.err.println( kNext );
                if ( bDir )
                {
                    kTract.add( new Vector3f( kNext ) );
                }
                else
                {
                    kTract.add( 0,  new Vector3f( kNext ) );
                }
                
                kStart.Copy(kNext);
                kDir.Copy(kOut);
            }
            else
            {
                bDone = true;
                //System.err.println( "Track LAMBDA NEG?" );
                //break;
            }
        }
        kNext = null;
    }
    /** Updates the number of fiber bundle tract groups. */
    private void updateTractCount() {
        m_iBundleCount = getMinUnused(m_kBundleList);
    }
    
    
    private void addTracts( BitSet voiMask )
    {
    	if ( voiMask.isEmpty() )
    	{
    		return;
    	}
    	for ( int i = 0; i < voiMask.length(); i++ )
    	{
    		if ( voiMask.get(i) )
    		{}
    	}
    }
}
