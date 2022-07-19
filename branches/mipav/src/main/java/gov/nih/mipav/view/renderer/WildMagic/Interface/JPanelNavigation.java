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

import javax.swing.*;
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

/**
 * The navigation (for virtual bronchoscopy) fly-thru control panel. 
 * 
 * @author Ruida Cheng
 *
 */
public class JPanelNavigation extends JInterfaceBase
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
  
    /** Navigation (fly-thru) checkbox */
    private JCheckBox navigationCheckBox;

    /** camera rotation degree slider */
    private JSlider cameraRotationDegreeSlider;

    /** camera rotation degree label */
    private JLabel cameraRotationDegreeLabel;
    
    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;
    
    /**
     * button for adding annotation points for the path planning
     */
    private JToggleButton addAnnotateButton;
    
    /** create path planning path button. */
    private JButton createPathButton;
    
    /** cleaning the planned path button. */
    private JButton clearPathButton;
    
    /**
     * Radio check button the path planning. 
     */
    private JRadioButton pathPlanningRadio;
    
    /**
     * Radio check button for mouse control. 
     */
    private JRadioButton mouseControlRadio;
    
    /** radio check button for fly-thru mode. */
    private ButtonGroup radioButtonGroupFlyMode;
    
    
    /**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanelNavigation( VolumeTriPlanarInterface kVolumeViewer )
    {
        super(kVolumeViewer);
        init();
    }
    
	public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if ( command.equals("Navigation")) {
        	boolean isSelected = navigationCheckBox.isSelected();
        	rayBasedRenderWM.toggleNavigation(isSelected);	
        	cameraRotationDegreeSlider.setEnabled(isSelected);
    		cameraRotationDegreeLabel.setEnabled(isSelected);
    		mouseControlRadio.setEnabled(isSelected);
            pathPlanningRadio.setEnabled(isSelected);
            addAnnotateButton.setEnabled(isSelected);
            createPathButton.setEnabled(isSelected);
            clearPathButton.setEnabled(isSelected);
    	
            if  ( isSelected ) {
	            if ( mouseControlRadio.isSelected() ) {
	            	rayBasedRenderWM.setMouseControlFlythru(true);
	    			m_kVolumeViewer.setMouseFlythruMode(true);
	    			rayBasedRenderWM.setPathPlanningFlythru(false);
	    			m_kVolumeViewer.setPathFlythruMode(false);
	            } else {
	            	rayBasedRenderWM.setPathPlanningFlythru(true);
	    			m_kVolumeViewer.setPathFlythruMode(true);	
	    			rayBasedRenderWM.setMouseControlFlythru(false);
	    			m_kVolumeViewer.setMouseFlythruMode(false);
	            }
            } else {
            	rayBasedRenderWM.setMouseControlFlythru(false);
    			m_kVolumeViewer.setMouseFlythruMode(false);
    			rayBasedRenderWM.setPathPlanningFlythru(false);
    			m_kVolumeViewer.setPathFlythruMode(false);
            }
			
		} else if ( command.equals("AddAnnotate")) {
			boolean toggleOn = addAnnotateButton.isSelected();
			if ( toggleOn ) {
				m_kVolumeViewer.setAnnotationMode(true);
				rayBasedRenderWM.setAnnotationMode(true);
				rayBasedRenderWM.setMouseControlFlythru(false);
				rayBasedRenderWM.setPathPlanningFlythru(false);
				m_kVolumeViewer.setMouseFlythruMode(false);
				m_kVolumeViewer.setPathFlythruMode(false);
			} else {
				m_kVolumeViewer.setAnnotationMode(false);
				rayBasedRenderWM.setAnnotationMode(false);
				m_kVolumeViewer.setPathFlythruMode(true);
				rayBasedRenderWM.setPathPlanningFlythru(true);
				pathPlanningRadio.setSelected(true);
			}
		} else if ( command.equals("CreatePath")) { 
			rayBasedRenderWM.generatePath();
			m_kVolumeViewer.setPathFlythruMode(true);
			rayBasedRenderWM.setPathPlanningFlythru(true);
			pathPlanningRadio.setSelected(true);
			rayBasedRenderWM.setMouseControlFlythru(false);
			m_kVolumeViewer.setMouseFlythruMode(false);
		} else if ( command.equals("ClearPath")) {
			rayBasedRenderWM.clearPath();
			m_kVolumeViewer.setPathFlythruMode(false);
			rayBasedRenderWM.setPathPlanningFlythru(true);
			rayBasedRenderWM.setMouseControlFlythru(false);
			m_kVolumeViewer.setMouseFlythruMode(false);
			
		} else if ( command.equals("MousePicking")) {
			rayBasedRenderWM.setMouseControlFlythru(true);
			m_kVolumeViewer.setMouseFlythruMode(true);
			rayBasedRenderWM.setPathPlanningFlythru(false);
			m_kVolumeViewer.setPathFlythruMode(false);
		} else if ( command.equals("PathPlanning")) {
			rayBasedRenderWM.setPathPlanningFlythru(true);
			m_kVolumeViewer.setPathFlythruMode(true);	
			rayBasedRenderWM.setMouseControlFlythru(false);
			m_kVolumeViewer.setMouseFlythruMode(false);
		}
        
    }
    
	public void disposeLocal(){}
	
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
        if ( source == cameraRotationDegreeSlider) {
        	int rotationDegree = cameraRotationDegreeSlider.getValue();
        	rayBasedRenderWM.setCameraViewRotationDegree(rotationDegree);
        }
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
    
  
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#stateChanged(javax.swing.event.ChangeEvent)
     */
	public void stateChanged(ChangeEvent event) {
        Object source = event.getSource();
        if ( source == cameraRotationDegreeSlider) {
        	int rotationDegree = cameraRotationDegreeSlider.getValue();
        	rayBasedRenderWM.setCameraViewRotationDegree(rotationDegree);
        }
    }
    
    /**
     * Initializes the GUI components.
     */
    private void init() {
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
  
         // add navigation panel for navigation checkbox
         JPanel naviPanel = new JPanel(new GridBagLayout());
         naviPanel.setBorder(buildTitledBorder("Navigation"));
         
         // navigation checkbox
         navigationCheckBox = new JCheckBox( "Enable Navigation Mode" );
         navigationCheckBox.setSelected(false);
         navigationCheckBox.setActionCommand( "Navigation");
         navigationCheckBox.addActionListener(this);        
         gbc.gridx = 0;
         gbc.gridy = 0;
         naviPanel.add(navigationCheckBox, gbc);
         
         // flythru mode panel
         JPanel flythruPanel = new JPanel(new GridBagLayout());
         flythruPanel.setBorder(buildTitledBorder("Flythru Mode"));
         
         // mouse control radio
         mouseControlRadio = new JRadioButton();
         mouseControlRadio.setText("Mouse Picking");
         mouseControlRadio.setSelected(true);
         mouseControlRadio.setActionCommand("MousePicking");
         mouseControlRadio.addActionListener(this);
         mouseControlRadio.setEnabled(false);
         gbc.gridx = 0;
         gbc.gridy = 0;
         flythruPanel.add(mouseControlRadio, gbc);
         
         // path planning radio button
         pathPlanningRadio = new JRadioButton(); 
         pathPlanningRadio.setText("Path Planning");
         pathPlanningRadio.setSelected(false);
         pathPlanningRadio.setActionCommand("PathPlanning");
         pathPlanningRadio.addActionListener(this);
         pathPlanningRadio.setEnabled(false);
         gbc.gridx = 1;
         gbc.gridy = 0;
         flythruPanel.add(pathPlanningRadio, gbc);
         
         radioButtonGroupFlyMode = new ButtonGroup();
         radioButtonGroupFlyMode.add(mouseControlRadio);
         radioButtonGroupFlyMode.add(pathPlanningRadio);
         
         // camera view panel
         JPanel cameraControlPanel = new JPanel(new GridBagLayout());
         cameraControlPanel.setBorder(buildTitledBorder("Camera Control"));
         
         // camera rotation sliders
         cameraRotationDegreeLabel = new JLabel("Camera Rotation");
         cameraRotationDegreeLabel.setEnabled(false);
         gbc.gridx = 0;
         gbc.gridy = 0;
         cameraControlPanel.add(cameraRotationDegreeLabel, gbc);
         cameraRotationDegreeSlider = new JSlider( 0, 60, 3 );
         cameraRotationDegreeSlider.setMajorTickSpacing(30);
         cameraRotationDegreeSlider.setMinorTickSpacing(3);
         cameraRotationDegreeSlider.addChangeListener(this);
         cameraRotationDegreeSlider.setPaintTicks(true);
         cameraRotationDegreeSlider.setPaintLabels(true);
         Font cFont = new Font("Serif", Font.ITALIC, 10);
         cameraRotationDegreeSlider.setFont(cFont);
         cameraRotationDegreeSlider.setEnabled(false);
         gbc.gridx = 1;
         cameraControlPanel.add(cameraRotationDegreeSlider, gbc);         
         
         // path planning panel
         // annotation panel
         JPanel pathPlanningPanel = new JPanel(new GridBagLayout());
         pathPlanningPanel.setBorder(buildTitledBorder("Path Planning"));
         
         addAnnotateButton = new JToggleButton("Add Annotation Point");
         addAnnotateButton.addActionListener(this);
         addAnnotateButton.setActionCommand("AddAnnotate");
         addAnnotateButton.setToolTipText("Adding annotation points");
         addAnnotateButton.setBorderPainted(true);
         addAnnotateButton.setFocusPainted(true);
         addAnnotateButton.setMargin(new Insets(0, 0, 0, 0));
         addAnnotateButton.setEnabled(false);
          
         createPathButton = new JButton("Generate Path");
         createPathButton.addActionListener(this);
         createPathButton.setActionCommand("CreatePath");
         createPathButton.setToolTipText("Generating tracking path");
         createPathButton.setBorderPainted(true);
         createPathButton.setFocusPainted(true);
         createPathButton.setMargin(new Insets(0, 0, 0, 0));
         createPathButton.setEnabled(false);
         
         clearPathButton = new JButton("Clear Path");
         clearPathButton.addActionListener(this);
         clearPathButton.setActionCommand("ClearPath");
         clearPathButton.setToolTipText("Erase tracking path");
         clearPathButton.setBorderPainted(true);
         clearPathButton.setFocusPainted(true);
         clearPathButton.setMargin(new Insets(0, 0, 0, 0));
         clearPathButton.setEnabled(false);
          
         gbc.gridx = 0;
         gbc.gridy = 0;
         pathPlanningPanel.add(addAnnotateButton, gbc);
         gbc.gridx = 0;
         gbc.gridy = 1;
         pathPlanningPanel.add(createPathButton, gbc);        
         gbc.gridx = 1;
         pathPlanningPanel.add(clearPathButton, gbc);
         
         // the main panel
         Box contentBox = new Box(BoxLayout.Y_AXIS);
         contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
         contentBox.add(naviPanel);
         contentBox.add(flythruPanel);
         contentBox.add(pathPlanningPanel);
         contentBox.add(cameraControlPanel);
  
         mainScrollPanel.add(contentBox, BorderLayout.NORTH);
         
         mainPanel.add(scroller, BorderLayout.CENTER);
    }
    
}