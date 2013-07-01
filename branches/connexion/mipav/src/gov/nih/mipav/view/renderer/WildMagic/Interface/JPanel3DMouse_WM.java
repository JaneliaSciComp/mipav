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
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

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
    
    /** Invert movement checkbox */
    private JCheckBox xInvertCheckBox, yInvertCheckBox, zInvertCheckBox;
	
    /**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanel3DMouse_WM( VolumeTriPlanarInterface kVolumeViewer )
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

         scroller = new JScrollPane(mainScrollPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

         mainPanel = new JPanel(new BorderLayout());

//         JPanel componentsPanel = new JPanel(new GridBagLayout());
//         componentsPanel.setBorder(buildTitledBorder("Display Components"));
         
         GridBagConstraints gbc = new GridBagConstraints();

         //gbc.weightx = 1;
         gbc.anchor = GridBagConstraints.WEST;
//         gbc.insets = new Insets(5, 5, 5, 5);
                
         // add mouse rotation and translation speed control slider
         JPanel mouseSpeedPanel = new JPanel(new GridBagLayout());
         mouseSpeedPanel.setBounds(10, 100, 500, 120);
         mouseSpeedPanel.setBorder(buildTitledBorder("Mouse Sensitivity"));
         
         xInvertCheckBox = new JCheckBox( "Invert X" );
         xInvertCheckBox.setSelected(false);
         xInvertCheckBox.setActionCommand("invertX");
         xInvertCheckBox.addActionListener(this);
         mouseSpeedPanel.add(xInvertCheckBox, gbc);
         
         JLabel mouseTranslationSpeedLabel = new JLabel("Translation/Zoom Speed");
         gbc.gridx = 1;
         gbc.gridy = 0;
         mouseSpeedPanel.add(mouseTranslationSpeedLabel, gbc);
         mouseTranslationSpeedSlider = new JSlider( 0, 40, 20 );
         mouseTranslationSpeedSlider.addChangeListener(this);
         gbc.gridx = 2;
         mouseSpeedPanel.add(mouseTranslationSpeedSlider, gbc);         
         
         if(DEBUG){
        	 mouseTranslationSpeedSlider.setBorder(BorderFactory.createLineBorder(Color.black));
        	 mouseTranslationSpeedLabel.setBorder(BorderFactory.createLineBorder(Color.black));
         }
         
         gbc.gridx = 0;
         gbc.gridy = 1;
         yInvertCheckBox = new JCheckBox( "Invert Y" );
         yInvertCheckBox.setSelected(false);
         yInvertCheckBox.setActionCommand("invertY");
         yInvertCheckBox.addActionListener(this);
         mouseSpeedPanel.add(yInvertCheckBox, gbc);
         
         JLabel mouseRotationSpeedLabel = new JLabel("Rotation Speed");
         gbc.gridx = 1;
         gbc.gridy = 1;
         mouseSpeedPanel.add(mouseRotationSpeedLabel, gbc);
         mouseRotationSpeedSlider = new JSlider(0, 100, 1);
         mouseRotationSpeedSlider.addChangeListener(this);
         gbc.gridx = 2;
         mouseSpeedPanel.add(mouseRotationSpeedSlider, gbc);
         
         
         // the main panel
         Box contentBox = new Box(BoxLayout.Y_AXIS);

         contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
         contentBox.add(mouseSpeedPanel);
//         contentBox.add(extractPanel);
  
         mainScrollPanel.add(contentBox, BorderLayout.NORTH);
         
         mainPanel.add(scroller, BorderLayout.CENTER);
    }

	@Override
	public void actionPerformed(ActionEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void stateChanged(ChangeEvent e) {
		// TODO Auto-generated method stub
		
	}
	
	 /**
     * Dispose memory.
     */
    public void disposeLocal() {
    	
    }
    
    /**
     * Helper method that adds components to the control panel for the grid
     * bag layout.
     *
     * @param  c    Component added to the control panel.
     * @param  gbc  GridBagConstraints of added component.
     * @param  x    Gridx location
     * @param  y    Gridy location
     * @param  w    Gridwidth
     * @param  h    Gridheight
     */
    private void addToMouseSpeedPanel(Component c, GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        mainPanel.add(c, gbc);
    }

}
