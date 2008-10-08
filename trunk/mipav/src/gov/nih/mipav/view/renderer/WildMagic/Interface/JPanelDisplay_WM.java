package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.*;
import WildMagic.LibFoundation.Mathematics.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import javax.swing.*;

/**
 * The display panel control the red bounding box frame ( on/off ), texture aligned rendering mode, cubic controk,
 * perspective and parrallel viewing mode, and back ground color.
 */
public class JPanelDisplay_WM extends JInterfaceBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 926266253314679850L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Check box for turning box on and off. */
    protected JCheckBox boundingCheck;

    /** Color button for changing color. */
    protected JButton colorButton;

    /** Color button for changing z color. */
    protected JButton colorButtonBackground;

    /** Color chooser dialog. */
    protected ViewJColorChooser colorChooser;

    /** Panel for the rotation cube. */
    protected JPanel cubePanel;

    /** Check box for cubic control. */
    protected JCheckBox cubicCheck;

    /** Button group for projections. */
    protected ButtonGroup radioButtonGroupProjections;

    /** Radio Button for Orthographic rendering. */
    protected JRadioButton radioButtonOrthographic;

    /** Radio Button for Perspective rendering. */
    protected JRadioButton radioButtonPerspective;

    /** Radio Button for Perspective rendering. */
    protected JRadioButton viewAlignedButton;

    /** Radio Button for Orthographic rendering. */
    protected JRadioButton viewButton;

    /** Button group for projections. */
    protected ButtonGroup viewTextureButtonGroup;

    /** Flag indicating if the red bounding box is on or off. */
    private boolean flag = false;

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;
   
    /** Save and Load camera and object parameters buttons. */
    private JButton saveButton, loadButton;
    
    /** Camera move parameter labels */
    private JLabel cameraXLabel, cameraYLabel, cameraZLabel;
    
    /** Camera move parameter values. */
    private float xCameraMove, yCameraMove, zCameraMove;
    
    /** Camera move parameter text-field. */
    private JTextField xCameraMoveText, yCameraMoveText, zCameraMoveText;
    
    /** Camera turn parameter labels */
    private JLabel cameraXTurnLabel, cameraYTurnLabel;
    
    /** Camera move parameter values */
    private float xCameraTurn, yCameraTurn;
    
    /** Camera move parameter text-field */
    private JTextField xCameraTurnText, yCameraTurnText;
    
    /** Object rotation parameter labels. */
    private JLabel objectXRotLabel, objectYRotLabel, objectZRotLabel;
    
    /** Object rotation parameter text-field. */
    private JTextField xObjectRotateText, yObjectRotateText, zObjectRotateText;
    
    /** Object rotation angle parameter values. */
    private float xObjectRotate, yObjectRotate, zObjectRotate;

    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for turning bounding box frame on and off.
     *
     * @param  parent  Should be of type ViewJFrameSurfaceRenderer
     */
    public JPanelDisplay_WM(VolumeTriPlanarInterface parent) {
        m_kVolumeViewer = parent;
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Changes color of box frame and button if color button was pressed; turns bounding box on and off if checkbox was
     * pressed; and closes dialog if "Close" button was pressed.
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if ( source == saveButton ) {
        	saveParameters();
        } else if ( source == loadButton ) {
        	loadParameters();
        } else if (source instanceof JButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener((JButton) source),
                                                 new CancelListener());
        } else if (source == boundingCheck) {

            if (boundingCheck.isSelected() != flag) {
                flag = boundingCheck.isSelected();

                m_kVolumeViewer.setShowBoxFrame(flag);
                if (flag == true) {
                    colorButton.setEnabled(true);
                } else {
                    colorButton.setEnabled(false);
                }
            }
        } else if (source == radioButtonOrthographic) {
            m_kVolumeViewer.setRenderPerspective(false);
        } else if (source == radioButtonPerspective) {
            m_kVolumeViewer.setRenderPerspective(true);
        } else if (source == cubicCheck) {
            m_kVolumeViewer.setShowOrientationCube(cubicCheck.isSelected());
        } 
    }

    /**
     * Dispose memory.
     */
    public void dispose() {
        boundingCheck = null;
        cubicCheck = null;
        colorButton = null;
        colorButtonBackground = null;
        colorChooser = null;
        flag = false;
        radioButtonOrthographic = null;
        radioButtonPerspective = null;
        radioButtonGroupProjections = null;
        cubePanel = null;
        viewButton = null;
        viewAlignedButton = null;
        viewTextureButtonGroup = null;
    }

    /**
     * Save camera and object viewing parameters.
     */
    public void saveParameters() {
    	float[] cameraParams;
    	float[] objectParams;
    	Matrix3f objectRotation;
    	Vector3f cameraLocation;
    	float[] data = new float[9];
    	String fileName = "viewParameters.txt";
        String fileDir = System.getProperties().getProperty("user.dir");
        System.err.println("filedir = " + fileDir);
        File file = new File(fileDir + File.separator + fileName);
        try {
        	RandomAccessFile raFile = new RandomAccessFile(file, "rw");
        	if (file.exists() == true) {
                 raFile.close();
                 file.delete();
                 file = new File(fileDir + File.separator + fileName);
                 raFile = new RandomAccessFile(file, "rw");
            }
        	cameraParams = m_kVolumeViewer.getCameraParameters();
        	cameraLocation = m_kVolumeViewer.getCameraLocation();
        	objectParams = m_kVolumeViewer.getObjectParameters();
        	objectRotation = m_kVolumeViewer.getObjectRotation();
        	
        	raFile.writeFloat(cameraParams[0]);
        	raFile.writeFloat(cameraParams[1]);
        	raFile.writeFloat(cameraParams[2]);
        	raFile.writeFloat(cameraParams[3]);
        	raFile.writeFloat(cameraParams[4]);
        	
        	raFile.writeFloat(objectParams[0]);
        	raFile.writeFloat(objectParams[1]);
        	raFile.writeFloat(objectParams[2]);
        	
        	raFile.writeFloat(cameraLocation.X);
        	raFile.writeFloat(cameraLocation.Y);
        	raFile.writeFloat(cameraLocation.Z);
        	
        	objectRotation.GetData(data);
        	for ( int i = 0; i < data.length; i++ ) {
        		raFile.writeFloat(data[i]);
        	}
        	
        	raFile.close();
        } catch ( IOException e ) {
        	e.printStackTrace();
        	System.err.println("Error saving viewing parameters file");
        }
        
    }
    
    /**
     * Load camera and object viewing parameters.
     */
    public void loadParameters() {
    	float[] cameraParams = new float[5];
    	float[] objectParams = new float[3];
    	Matrix3f objectRotation = new Matrix3f();
    	Vector3f cameraLocation = new Vector3f();
    	float[] data = new float[9];
    	String fileName = "viewParameters.txt";
        String fileDir = System.getProperties().getProperty("user.dir");
        File file = new File(fileDir + File.separator + fileName);
        try {
        	RandomAccessFile raFile = new RandomAccessFile(file, "r");
        	        	
        	cameraParams[0] = raFile.readFloat();
        	cameraParams[1] = raFile.readFloat();
        	cameraParams[2] = raFile.readFloat();
        	cameraParams[3] = raFile.readFloat();
        	cameraParams[4] = raFile.readFloat();
        	
        	objectParams[0] = raFile.readFloat();
        	objectParams[1] = raFile.readFloat();
        	objectParams[2] = raFile.readFloat();
        	
        	cameraLocation.X = raFile.readFloat();
        	cameraLocation.Y = raFile.readFloat();
        	cameraLocation.Z = raFile.readFloat();
        	
        	for ( int i = 0; i < data.length; i++ ) {
        		objectRotation.Set(i, raFile.readFloat());
        	}
        	
        	m_kVolumeViewer.setCameraLocation(cameraLocation);
        	m_kVolumeViewer.setObjectRotation(objectRotation);
        	
        	displayCameraParams(cameraParams);
        	displayObjectParams(objectParams);
        	
        	raFile.close();
        } catch ( IOException e ) {
        	e.printStackTrace();
        	System.err.println("Error openning viewing parameters file");
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
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
     * Set the color for the color button.
     *
     * @param  _color  Color
     */
    public void setColorButton(Color _color) {
        colorButtonBackground.setBackground(_color);
    }

    /**
     * Calls the appropriate method in the parent frame.
     *
     * @param  button  DOCUMENT ME!
     * @param  color   Color to set box frame to.
     */
    protected void setBoxColor(JButton button, Color color) {

        if (button == colorButton) {
            m_kVolumeViewer.setBoundingBoxColor(color);
        }
        if (button == colorButtonBackground) {
            m_kVolumeViewer.setBackgroundColor(color);
        }
    }

    /**
     * Initializes GUI components.
     */
    private void init() {
        boundingCheck = new JCheckBox("Show bounding frame");
        boundingCheck.setFont(MipavUtil.font12);
        boundingCheck.addActionListener(this);

        colorButton = new JButton();
        colorButton.setPreferredSize(new Dimension(25, 25));
        colorButton.setToolTipText("Change box frame color");
        colorButton.addActionListener(this);
        colorButton.setBackground(Color.red);
        colorButton.setEnabled(false);

        JPanel panel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        panel.add(colorButton, gbc);
        gbc.gridx = 1;
        panel.add(boundingCheck, gbc);

        panel.setBorder(buildTitledBorder("Bounding box options"));

        colorButtonBackground = new JButton();
        colorButtonBackground.setPreferredSize(new Dimension(25, 25));
        colorButtonBackground.setToolTipText("Change background color");
        colorButtonBackground.addActionListener(this);
        colorButtonBackground.setBackground(Color.darkGray);

        JLabel backgroundLabel = new JLabel("Background color");

        backgroundLabel.setFont(MipavUtil.font12);
        backgroundLabel.setForeground(Color.black);

        JPanel panel2 = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        panel2.add(colorButtonBackground, gbc);
        gbc.gridx = 1;
        panel2.add(backgroundLabel, gbc);
        panel2.setBorder(buildTitledBorder("Background"));

        JPanel projectionTypePanel = new JPanel();

        projectionTypePanel.setBorder(buildTitledBorder("Projection Type"));

        Box projectionTypeBox = new Box(BoxLayout.X_AXIS);

        radioButtonPerspective = new JRadioButton();
        radioButtonPerspective.addActionListener(this);
        radioButtonOrthographic = new JRadioButton();
        radioButtonOrthographic.addActionListener(this);
        radioButtonGroupProjections = new ButtonGroup();
        radioButtonPerspective.setSelected(true);
        radioButtonPerspective.setText("Perspective View ");
        radioButtonOrthographic.setText("Orthographic View");
        radioButtonGroupProjections.add(radioButtonPerspective);
        radioButtonGroupProjections.add(radioButtonOrthographic);
        projectionTypeBox.add(radioButtonPerspective);
        projectionTypeBox.add(radioButtonOrthographic);
        projectionTypePanel.add(projectionTypeBox);

        cubicCheck = new JCheckBox("Show orientation cube");
        cubicCheck.setFont(MipavUtil.font12);
        cubicCheck.addActionListener(this);

        cubePanel = new JPanel(new GridBagLayout());
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        cubePanel.add(cubicCheck, gbc);
        cubePanel.setBorder(buildTitledBorder("Orientation"));

        ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(this);

        JToolBar toolBar = new JToolBar();
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setFloatable(false);
        
        saveButton = toolbarBuilder.buildButton("SaveParameters", "Save camera and object viewing parameters", "save");
        loadButton = toolbarBuilder.buildButton("LoadParameters", "Load camera and object viewing parameters", "open");
        
        toolBar.add(loadButton);
        toolBar.add(saveButton);
        
        Box cameraParametersBox = new Box(BoxLayout.Y_AXIS);
        
        cameraParametersBox.setBorder(buildTitledBorder("Camera"));
        
        JPanel cameraMovePanel = new JPanel(new GridBagLayout());
        cameraMovePanel.setBorder(buildTitledBorder("Movements"));
        cameraParametersBox.add(cameraMovePanel);
        
        cameraXLabel = new JLabel("X ( Move left or Right ) ");
        cameraXLabel.setFont(serif12);
        cameraXLabel.setForeground(Color.black);
        cameraXLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 0;
        cameraMovePanel.add(cameraXLabel, gbc);
        xCameraMoveText = new JTextField(8);
       
        xCameraMoveText.setText(MipavUtil.makeFloatString(xCameraMove, 7));
        MipavUtil.makeNumericsOnly(xCameraMoveText, true, true);
        gbc.gridx = 1;
        cameraMovePanel.add(xCameraMoveText, gbc);

        cameraYLabel = new JLabel("Y ( Move Up or Down ) ");
        cameraYLabel.setFont(serif12);
        cameraYLabel.setForeground(Color.black);
        cameraYLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 1;
        cameraMovePanel.add(cameraYLabel, gbc);
        yCameraMoveText = new JTextField(8);
       
        yCameraMoveText.setText(MipavUtil.makeFloatString(yCameraMove, 7));
        MipavUtil.makeNumericsOnly(yCameraMoveText, true, true);
        gbc.gridx = 1;
        cameraMovePanel.add(yCameraMoveText, gbc);

        cameraZLabel = new JLabel("Z ( Zoom In or Out ) ");
        cameraZLabel.setFont(serif12);
        cameraZLabel.setForeground(Color.black);
        cameraZLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 2;
        cameraMovePanel.add(cameraZLabel, gbc);
        zCameraMoveText = new JTextField(8);
       
        zCameraMoveText.setText(MipavUtil.makeFloatString(zCameraMove, 7));
        MipavUtil.makeNumericsOnly(zCameraMoveText, true, true);
        gbc.gridx = 1;
        cameraMovePanel.add(zCameraMoveText, gbc);

        JPanel cameraTurnPanel = new JPanel(new GridBagLayout());
        cameraTurnPanel.setBorder(buildTitledBorder("Turns"));
        cameraParametersBox.add(cameraTurnPanel);

        cameraXTurnLabel = new JLabel("X ( Turn Left or Right ) ");
        cameraXTurnLabel.setFont(serif12);
        cameraXTurnLabel.setForeground(Color.black);
        cameraXTurnLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 0;
        cameraTurnPanel.add(cameraXTurnLabel, gbc);
        xCameraTurnText = new JTextField(8);
       
        xCameraTurnText.setText(MipavUtil.makeFloatString(xCameraTurn, 7));
        MipavUtil.makeNumericsOnly(xCameraTurnText, true, true);
        gbc.gridx = 1;
        cameraTurnPanel.add(xCameraTurnText, gbc);
        
        cameraYTurnLabel = new JLabel("Y ( Turn Up or Down ) ");
        cameraYTurnLabel.setFont(serif12);
        cameraYTurnLabel.setForeground(Color.black);
        cameraYTurnLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 1;
        cameraTurnPanel.add(cameraYTurnLabel, gbc);
        yCameraTurnText = new JTextField(8);
       
        yCameraTurnText.setText(MipavUtil.makeFloatString(yCameraTurn, 7));
        MipavUtil.makeNumericsOnly(yCameraTurnText, true, true);
        gbc.gridx = 1;
        cameraTurnPanel.add(yCameraTurnText, gbc);
            
        JPanel objectParametersPanel = new JPanel();
        objectParametersPanel.setBorder(buildTitledBorder("Object"));
        
        JPanel objectsPanel = new JPanel(new GridBagLayout());
        objectParametersPanel.add(objectsPanel);
        
        objectXRotLabel = new JLabel("X Rotation Angle");
        objectXRotLabel.setFont(serif12);
        objectXRotLabel.setForeground(Color.black);
        objectXRotLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 0;
        objectsPanel.add(objectXRotLabel, gbc);
        xObjectRotateText = new JTextField(8);
       
        xObjectRotateText.setText(MipavUtil.makeFloatString(xObjectRotate, 7));
        MipavUtil.makeNumericsOnly(xObjectRotateText, true, true);
        gbc.gridx = 1;
        objectsPanel.add(xObjectRotateText, gbc);
        
        objectYRotLabel = new JLabel("Y Rotation Angle");
        objectYRotLabel.setFont(serif12);
        objectYRotLabel.setForeground(Color.black);
        objectYRotLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 1;
        objectsPanel.add(objectYRotLabel, gbc);
        yObjectRotateText = new JTextField(8);
       
        yObjectRotateText.setText(MipavUtil.makeFloatString(yObjectRotate, 7));
        MipavUtil.makeNumericsOnly(yObjectRotateText, true, true);
        gbc.gridx = 1;
        objectsPanel.add(yObjectRotateText, gbc);
        
        objectZRotLabel = new JLabel("Z Rotation Angle");
        objectZRotLabel.setFont(serif12);
        objectZRotLabel.setForeground(Color.black);
        objectZRotLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 2;
        objectsPanel.add(objectZRotLabel, gbc);
        zObjectRotateText = new JTextField(8);
       
        zObjectRotateText.setText(MipavUtil.makeFloatString(zObjectRotate, 7));
        MipavUtil.makeNumericsOnly(zObjectRotateText, true, true);
        gbc.gridx = 1;
        objectsPanel.add(zObjectRotateText, gbc);
        
        
        JPanel viewPanel = new JPanel(new BorderLayout());
        viewPanel.setBorder(buildTitledBorder("View"));
        
        viewPanel.add(toolBar, BorderLayout.NORTH);
        viewPanel.add(cameraParametersBox, BorderLayout.CENTER);
        viewPanel.add(objectParametersPanel, BorderLayout.SOUTH);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        contentBox.add(panel);
        contentBox.add(panel2);
        contentBox.add(cubePanel);
        contentBox.add(projectionTypePanel);
        contentBox.add(viewPanel);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(contentBox, BorderLayout.NORTH);

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.add(scroller);
    }
    
    /**
     * Display camera related parameters
     * @param params
     */
    public void displayCameraParams(float[] params) {
    	xCameraMoveText.setText(MipavUtil.makeFloatString(params[0], 7));
    	yCameraMoveText.setText(MipavUtil.makeFloatString(params[1], 7));
    	zCameraMoveText.setText(MipavUtil.makeFloatString(params[2], 7));
        xCameraTurnText.setText(MipavUtil.makeFloatString(params[3], 7));
        yCameraTurnText.setText(MipavUtil.makeFloatString(params[4], 7));
    }
    
    /**
     * Display object related parameters
     * @param params
     */
    public void displayObjectParams(float[] params) {
    	xObjectRotateText.setText(MipavUtil.makeFloatString(params[0], 7));
    	yObjectRotateText.setText(MipavUtil.makeFloatString(params[1], 7));
    	zObjectRotateText.setText(MipavUtil.makeFloatString(params[2], 7));
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Does nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Does nothing.
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) { }
    }

    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -375187487188025368L;

        /**
         * DOCUMENT ME!
         *
         * @param  g  DOCUMENT ME!
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
        }
    }

    /**
     * Pick up the selected color and call method to change the color.
     */
    class OkColorListener implements ActionListener {

        /** DOCUMENT ME! */
        JButton button;

        /**
         * Creates a new OkColorListener object.
         *
         * @param  _button  DOCUMENT ME!
         */
        OkColorListener(JButton _button) {
            super();
            button = _button;
        }

        /**
         * Get color from chooser and set button and color.
         *
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();

            button.setBackground(color);
            setBoxColor(button, color);
        }
    }
}
