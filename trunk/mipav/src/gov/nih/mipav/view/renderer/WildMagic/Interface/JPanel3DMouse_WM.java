package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.WildMagic.GPURenderBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.Hashtable;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
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
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.GroupLayout;
import javax.swing.GroupLayout.Alignment;
import javax.swing.LayoutStyle.ComponentPlacement;

import java.awt.ComponentOrientation;
import java.io.FileNotFoundException;
import javax.swing.Icon;

public class JPanel3DMouse_WM extends JInterfaceBase implements ChangeListener {

	private static boolean DEBUG = false;
	
	/**
	 * eclipse generated this
	 */
	private static final long serialVersionUID = 2858702077770704429L;
	
    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;
    
    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;
    
    /** Mouse translation speed slider. */
    private JSlider mouseTranslationSpeedSlider;
    
    /** Mouse rotation speed slider. */
    private JSlider mouseRotationSpeedSlider;
    
    /** Mouse rotation sensitivity cutoff slider */
    private JSlider mouseRotationCutoffSlider;
    
    /** Invert movement checkbox */
    private JCheckBox zInvertCheckBox;
    
    /** Labels beneath sliders. */
    private JLabel labelX1, labelXMid, labelXEnd;
    
    /** Text fields that display the sensitivity number next to the sliders. */
    private JTextField rotationCutoffTextField;
    
    private boolean inMenu = false;
    
    private GridBagConstraints gbc_1;
    private GridBagConstraints gbc_2;
    private GridBagConstraints gbc_3;
    private GridBagConstraints gbc_4;
    private JCheckBox chckbxLeftright;
    private JCheckBox chckbxUpdown;
    private JCheckBox chckbxForwardsbackwards;
    private JCheckBox chckbxRotationRX;
    private JCheckBox chckbxRotationRy;
    private JCheckBox chckbxRotationRz;
    private JSlider mouseTranslationCutoffSlider;
    private JLabel lblTranslation;
    private JLabel lblRotation;
    private JTextField translationCutoffTextField;
    private JLabel leftRighticon;
    private JLabel leftRighticon_1;
    private JLabel tilticon;
    private JLabel spinIcon;
    private JLabel rollIcon;
    private JLabel lblTranslations;
    private JLabel lblRotations;
    
	
    /**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanel3DMouse_WM( VolumeTriPlanarInterface kVolumeViewer )
    {
//        super(kVolumeViewer);
//    	super(new VolumeTriPlanarInterface());
        init();
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
//         gbc.insets = new Insets(5, 5, 5, 5);
                
         // add mouse rotation and translation speed control slider and check box for inverting direction
         GridBagLayout gbl_mouseSpeedPanel = new GridBagLayout();
         gbl_mouseSpeedPanel.columnWeights = new double[]{0.0, 0.0, 0.0};
         JPanel mouseSpeedPanel = new JPanel(gbl_mouseSpeedPanel);
         mouseSpeedPanel.setBounds(10, 100, 500, 120);
         mouseSpeedPanel.setBorder(buildTitledBorder("Mouse Sensitivity"));
         
         //TODO rename the computer generated code variable names
         gbc_3 = new GridBagConstraints();
         gbc_3.insets = new Insets(0, 0, 5, 5);
         gbc_3.anchor = GridBagConstraints.WEST;
         
         JLabel mouseTranslationSpeedLabel = new JLabel("Translation/Zoom Speed");
         gbc_3.gridx = 1;
         gbc_3.gridy = 0;
         mouseSpeedPanel.add(mouseTranslationSpeedLabel, gbc_3);
         mouseTranslationSpeedSlider = new JSlider( 1, 10, GPURenderBase.getTranslationScaleFactor() );
         mouseTranslationSpeedSlider.addChangeListener(this);
         
         gbc_1 = new GridBagConstraints();
         gbc_1.insets = new Insets(0, 0, 5, 5);
         gbc_1.gridy = 0;
         gbc_1.anchor = GridBagConstraints.WEST;
         gbc_1.gridx = 2;
         mouseSpeedPanel.add(mouseTranslationSpeedSlider, gbc_1);         
         
         gbc_4 = new GridBagConstraints();
         gbc_4.insets = new Insets(0, 0, 0, 5);
         gbc_4.anchor = GridBagConstraints.WEST;
         
         JLabel mouseRotationSpeedLabel = new JLabel("Rotation Speed");
         gbc_4.gridx = 1;
         gbc_4.gridy = 1;
         mouseSpeedPanel.add(mouseRotationSpeedLabel, gbc_4);
         mouseRotationSpeedSlider = new JSlider(0, 100);
         mouseRotationSpeedSlider.setPaintTicks(true);
         mouseRotationSpeedSlider.setPaintLabels(true);
         mouseRotationSpeedSlider.setMinorTickSpacing(10);
         mouseRotationSpeedSlider.addChangeListener(this);
         
         gbc_2 = new GridBagConstraints();
         gbc_2.insets = new Insets(0, 0, 0, 5);
         gbc_2.gridy = 1;
         gbc_2.anchor = GridBagConstraints.WEST;
         
         gbc_2.gridx = 2;
         mouseSpeedPanel.add(mouseRotationSpeedSlider, gbc_2);
         
         //create the invert direction check boxes
         JPanel invertPanel = new JPanel();
         invertPanel.setMinimumSize(new Dimension(100, 100));
         invertPanel.setBorder(buildTitledBorder("Invert Directions"));
         invertPanel.setLayout(null);
         invertPanel.setPreferredSize(new Dimension(200, 250));
         
         JPanel cutoffPanel = new JPanel();
         cutoffPanel.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
         cutoffPanel.setBorder(buildTitledBorder("sensitivity cutoffs"));

         labelX1 = new JLabel("0");
         labelX1.setForeground(Color.black);
         labelX1.setFont(MipavUtil.font12);
         labelX1.setEnabled(true);
         labelXMid = new JLabel(String.valueOf(50));
         labelXMid.setForeground(Color.black);
         labelXMid.setFont(MipavUtil.font12);
         labelXMid.setEnabled(true);
         labelXEnd = new JLabel(String.valueOf(100));
         labelXEnd.setForeground(Color.black);
         labelXEnd.setFont(MipavUtil.font12);
         labelXEnd.setEnabled(true);

         Hashtable<Integer,JLabel> labelTableX = new Hashtable<Integer,JLabel>();

         labelTableX.put(new Integer(0), labelX1);
         labelTableX.put(new Integer(50), labelXMid);
         labelTableX.put(new Integer(100), labelXEnd);
         
         // the main panel
         Box contentBox = new Box(BoxLayout.Y_AXIS);

         contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
         contentBox.add(mouseSpeedPanel);
         contentBox.add(invertPanel);
         
         leftRighticon = new JLabel("icon");
         try {
               leftRighticon_1 = new JLabel(new ImageIcon(MipavUtil.getIconImage("panRightLeft_alphaBlended.gif").getScaledInstance(80, -80, Image.SCALE_DEFAULT)));
         //  leftRighticon_1 = new JLabel(new ImageIcon(MipavUtil.getIconImage("panLeftRight.gif").getScaledInstance(-1, 100, Image.SCALE_DEFAULT)));
//        	 leftRighticon_1 = new JLabel(new ImageIcon(MipavUtil.getIconImage("panLeftRight_alphaBlended.gif")));
        	 leftRighticon_1.setBounds(10, 46, 88, 48);
        	 invertPanel.add(leftRighticon_1);
//			 leftRighticon_1.setPreferredSize(new Dimension(80, 80));
		 } catch (FileNotFoundException e) {
			System.err.println("Exception ocurred while getting <" + e.getMessage()
                    + ">.  Check that this file is available.\n");
		 }
         
         chckbxUpdown = new JCheckBox("Up/Down");
         chckbxUpdown.setBounds(104, 110, 69, 23);
         chckbxUpdown.addActionListener(this);
         
         chckbxRotationRX = new JCheckBox("Tilt ");
         chckbxRotationRX.setBounds(304, 54, 83, 23);
         chckbxRotationRX.addActionListener(this);
         
         chckbxLeftright = new JCheckBox("Left/Right");
         chckbxLeftright.setBounds(104, 54, 73, 23);
         chckbxLeftright.addActionListener(this);

         invertPanel.add(chckbxLeftright);
         invertPanel.add(chckbxRotationRX);
         
         chckbxRotationRy = new JCheckBox("Roll");
         chckbxRotationRy.setBounds(304, 110, 83, 23);
         chckbxRotationRy.addActionListener(this);
         
         chckbxForwardsbackwards = new JCheckBox("Zoom In/Out");
         chckbxForwardsbackwards.setBounds(104, 170, 88, 23);
         chckbxForwardsbackwards.addActionListener(this);
         invertPanel.add(chckbxForwardsbackwards);
         invertPanel.add(chckbxRotationRy);
         invertPanel.add(chckbxUpdown);
         
         chckbxRotationRz = new JCheckBox("Spin");
         chckbxRotationRz.setBounds(304, 170, 83, 23);
         chckbxRotationRz.addActionListener(this);
         invertPanel.add(chckbxRotationRz);
         
         JLabel upDownIcon = new JLabel("Up/Down Icon");
         try {         			
        	 upDownIcon = new JLabel(new ImageIcon(MipavUtil.getIconImage("panUpDown_alphaBlended.gif").getScaledInstance(80, -1, Image.SCALE_DEFAULT)));
		 } catch (FileNotFoundException e) {
			System.err.println("Exception ocurred while getting <" + e.getMessage()
                    + ">.  Check that this file is available.\n");
		 }
         upDownIcon.setBounds(15, 93, 83, 63);
         invertPanel.add(upDownIcon);
         
         JLabel forwardsBackwardsIcon = new JLabel("Forwards/Backwards Icon");
         try {         			
        	 forwardsBackwardsIcon = new JLabel(new ImageIcon(MipavUtil.getIconImage("panZoom_alphaBlended.gif").getScaledInstance(85, -80, Image.SCALE_DEFAULT)));
		 } catch (FileNotFoundException e) {
			System.err.println("Exception ocurred while getting <" + e.getMessage()
                    + ">.  Check that this file is available.\n");
		 }
         forwardsBackwardsIcon.setBounds(10, 157, 88, 48);
         invertPanel.add(forwardsBackwardsIcon);
         
         tilticon = new JLabel("tilt Icon");
         try {         			
        	 tilticon = new JLabel(new ImageIcon(MipavUtil.getIconImage("tilt_alphaBlended.gif").getScaledInstance(80, -80, Image.SCALE_DEFAULT)));
		 } catch (FileNotFoundException e) {
			System.err.println("Exception ocurred while getting <" + e.getMessage()
                    + ">.  Check that this file is available.\n");
		 }
         tilticon.setBounds(203, 35, 88, 74);
         invertPanel.add(tilticon);
         
         spinIcon = new JLabel("Spin Icon");
         try {         			
        	 spinIcon = new JLabel(new ImageIcon(MipavUtil.getIconImage("spin_alphaBlended.gif").getScaledInstance(-80, 70, Image.SCALE_DEFAULT)));
		 } catch (FileNotFoundException e) {
			System.err.println("Exception ocurred while getting <" + e.getMessage()
                    + ">.  Check that this file is available.\n");
		 }
         spinIcon.setBounds(208, 78, 80, 80);
         invertPanel.add(spinIcon);
         
         rollIcon = new JLabel("Roll Icon");
         try {         			
        	 rollIcon = new JLabel(new ImageIcon(MipavUtil.getIconImage("roll_alphaBlended.gif").getScaledInstance(85, -80, Image.SCALE_DEFAULT)));
		 } catch (FileNotFoundException e) {
			System.err.println("Exception ocurred while getting <" + e.getMessage()
                    + ">.  Check that this file is available.\n");
		 }
         rollIcon.setBounds(210, 155, 83, 63);
         invertPanel.add(rollIcon);
         
         lblTranslations = new JLabel("Translations");
         lblTranslations.setBounds(68, 21, 71, 14);
         invertPanel.add(lblTranslations);
         
         lblRotations = new JLabel("Rotations");
         lblRotations.setBounds(279, 21, 46, 14);
         invertPanel.add(lblRotations);
         
         contentBox.add(cutoffPanel);
         
         //TODO fix indentations 
                  GridBagLayout gbl_cutoffPanel = new GridBagLayout();
                  gbl_cutoffPanel.columnWidths = new int[]{0, 0, 53, 223, 42, 0};
                  gbl_cutoffPanel.rowHeights = new int[]{32, 32, 31, 0};
                  gbl_cutoffPanel.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
                  gbl_cutoffPanel.rowWeights = new double[]{0.0, 0.0, 0.0, Double.MIN_VALUE};
                  cutoffPanel.setLayout(gbl_cutoffPanel);
                  
                  mouseRotationCutoffSlider = new JSlider(0, 100, GPURenderBase.getRotationCutoffValue());
                  mouseRotationCutoffSlider.setPaintLabels(true);
                  mouseRotationCutoffSlider.setFont(MipavUtil.font12);
                  mouseRotationCutoffSlider.setEnabled(true);
                  mouseRotationCutoffSlider.setMinorTickSpacing(10);
                  mouseRotationCutoffSlider.setPaintTicks(true);
                  mouseRotationCutoffSlider.addChangeListener(this);
                  mouseRotationCutoffSlider.setLabelTable(labelTableX);
                  
                  lblRotation = new JLabel("Rotation");
                  GridBagConstraints gbc_lblRotation = new GridBagConstraints();
                  gbc_lblRotation.insets = new Insets(0, 0, 5, 5);
                  gbc_lblRotation.gridx = 2;
                  gbc_lblRotation.gridy = 1;
                  cutoffPanel.add(lblRotation, gbc_lblRotation);
                  GridBagConstraints gbc_mouseRotationCutoffSlider = new GridBagConstraints();
                  gbc_mouseRotationCutoffSlider.anchor = GridBagConstraints.SOUTHWEST;
                  gbc_mouseRotationCutoffSlider.insets = new Insets(0, 0, 5, 5);
                  gbc_mouseRotationCutoffSlider.gridx = 3;
                  gbc_mouseRotationCutoffSlider.gridy = 1;
                  cutoffPanel.add(mouseRotationCutoffSlider, gbc_mouseRotationCutoffSlider);
                  
				  rotationCutoffTextField = new JTextField(String.valueOf(GPURenderBase.getRotationCutoffValue()), 4);
				  rotationCutoffTextField.setEditable(false);
				  rotationCutoffTextField.setFont(MipavUtil.font12);
				  GridBagConstraints gbc_textX = new GridBagConstraints();
				  gbc_textX.anchor = GridBagConstraints.NORTHWEST;
				  gbc_textX.insets = new Insets(0, 0, 5, 0);
				  gbc_textX.gridx = 4;
				  gbc_textX.gridy = 1;
				  cutoffPanel.add(rotationCutoffTextField, gbc_textX);
                  
                  lblTranslation = new JLabel("Translation");
                  GridBagConstraints gbc_lblTranslation = new GridBagConstraints();
                  gbc_lblTranslation.anchor = GridBagConstraints.WEST;
                  gbc_lblTranslation.insets = new Insets(0, 0, 0, 5);
                  gbc_lblTranslation.gridx = 2;
                  gbc_lblTranslation.gridy = 2;
                  cutoffPanel.add(lblTranslation, gbc_lblTranslation);
                  
                  mouseTranslationCutoffSlider = new JSlider(0, 100, GPURenderBase.getTranslationCutoffValue());
                  mouseTranslationCutoffSlider.setMinorTickSpacing(10);
                  mouseTranslationCutoffSlider.setPaintLabels(true);
                  mouseTranslationCutoffSlider.setPaintTicks(true);
                  mouseTranslationCutoffSlider.addChangeListener(this);
                  mouseTranslationCutoffSlider.setLabelTable(labelTableX);
                  GridBagConstraints gbc_slider = new GridBagConstraints();
                  gbc_slider.anchor = GridBagConstraints.NORTHWEST;
                  gbc_slider.insets = new Insets(0, 0, 0, 5);
                  gbc_slider.gridx = 3;
                  gbc_slider.gridy = 2;
                  cutoffPanel.add(mouseTranslationCutoffSlider, gbc_slider);
                  
                  translationCutoffTextField = new JTextField(String.valueOf(GPURenderBase.getTranslationCutoffValue()), 4);
                  translationCutoffTextField.setEditable(false);
                  translationCutoffTextField.setFont(MipavUtil.font12);
                  GridBagConstraints gbc_textField = new GridBagConstraints();
                  gbc_textField.anchor = GridBagConstraints.WEST;
                  gbc_textField.insets = new Insets(0, 0, 5, 0);
                  gbc_textField.gridx = 4;
                  gbc_textField.gridy = 2;
                  cutoffPanel.add(translationCutoffTextField, gbc_textField);
//                  translationCutoffTextField.setColumns(10);
  
         mainScrollPanel.add(contentBox, BorderLayout.NORTH);
         
         mainPanel.add(scroller, BorderLayout.CENTER);
    }

	@Override
	public void actionPerformed(ActionEvent e) {
		Object source = e.getSource();
		
		if(source == chckbxLeftright){
			GPURenderBase.invertTX();
		}else if(source == chckbxForwardsbackwards){
			GPURenderBase.invertTY();
		}else if(source == chckbxUpdown){
			GPURenderBase.invertTZ();
		}else if(source == chckbxRotationRX){
			GPURenderBase.invertRX();
		}else if(source == chckbxRotationRy){
			GPURenderBase.invertRY();
		}else if(source == chckbxRotationRz){
			GPURenderBase.invertRZ();
		}
		
	}

	@Override
	public void stateChanged(ChangeEvent e) {
		// TODO finish making each button/ckbx/slider work
		Object source = e.getSource();
		if(source == mouseRotationCutoffSlider){
			rotationCutoffTextField.setText(String.valueOf(mouseRotationCutoffSlider.getValue()));
//			if(!mouseRotationCutoffSlider.getValueIsAdjusting())
				GPURenderBase.setRotationCutoffValue(mouseRotationCutoffSlider.getValue());
		}else if(source == mouseTranslationCutoffSlider){
			translationCutoffTextField.setText(String.valueOf(mouseTranslationCutoffSlider.getValue()));
//			if(!mouseTranslationCutoffSlider.getValueIsAdjusting())
				GPURenderBase.setTranslationCutoffValue(mouseTranslationCutoffSlider.getValue());
		}else if(source == mouseTranslationSpeedSlider){
//			if(!mouseTranslationSpeedSlider.getValueIsAdjusting())
				GPURenderBase.setTranslationScaleFactor((mouseTranslationSpeedSlider.getValue()));
		}else if(source == mouseRotationSpeedSlider){
//			if(!mouseRotationSpeedSlider.getValueIsAdjusting())
				GPURenderBase.setRotationScaleFactor((mouseRotationSpeedSlider.getValue()));
		}
	}
	
	 /**
     * Dispose memory.
     */
    public void disposeLocal() {
    	
    }

	public boolean isInMenu() {
		return inMenu;
	}

	public void setInMenu(boolean inMenu) {
		this.inMenu = inMenu;
	}
    
}
