package gov.nih.mipav.view.renderer;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.WildMagic.ApplicationDemos.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.renderer.volumeview.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.media.j3d.Material;


/**
 * The display panel control the red bounding box frame ( on/off ), texture aligned rendering mode, cubic controk,
 * perspective and parrallel viewing mode, and back ground color.
 */
public class JPanelDisplayWM extends JPanelRendererBaseWM implements KeyListener {

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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for turning bounding box frame on and off.
     *
     * @param  parent  Should be of type ViewJFrameSurfaceRenderer
     */
    public JPanelDisplayWM(GPUVolumeRenderWM parent) {
        super(parent);
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
        String command = event.getActionCommand();

        if ( command.equals( "AdvancedMaterialOptions" ) ) {
            if ( renderBase instanceof GPUVolumeRenderWM )
            {
                Material kMaterial = new Material();
                if ( ((GPUVolumeRenderWM)renderBase) != null )
                {
                    MaterialState kMaterialSt = ((GPUVolumeRenderWM)renderBase).GetMaterialState();
                    kMaterial.setAmbientColor( new javax.vecmath.Color3f( kMaterialSt.Ambient.R(),
                                                                          kMaterialSt.Ambient.G(),
                                                                          kMaterialSt.Ambient.B() ) );

                    kMaterial.setEmissiveColor(new javax.vecmath.Color3f( kMaterialSt.Emissive.R(),
                                                                          kMaterialSt.Emissive.G(),
                                                                          kMaterialSt.Emissive.B() ) );
                    kMaterial.setDiffuseColor(new javax.vecmath.Color3f( kMaterialSt.Diffuse.R(),
                                                                         kMaterialSt.Diffuse.G(),
                                                                         kMaterialSt.Diffuse.B() ) );
                    kMaterial.setSpecularColor(new javax.vecmath.Color3f( kMaterialSt.Specular.R(),
                                                                          kMaterialSt.Specular.G(),
                                                                          kMaterialSt.Specular.B() ) );
                    kMaterial.setShininess(kMaterialSt.Shininess);
                }
                /*
                new JFrameSurfaceMaterialProperties(this, 0,
                                                    ((SurfaceRender)renderBase).getSurfaceDialog().getLightDialog().getGeneralLights(),
                                                    0, kMaterial );
                */
            }
        }
        else if (source instanceof JButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener((JButton) source),
                                                 new CancelListener());
        } else if (source == boundingCheck) {

            if (boundingCheck.isSelected() != flag) {
                flag = boundingCheck.isSelected();

                if (flag == true) {
                    ((GPUVolumeRenderWM) renderBase).getParentFrame().setShowBoxFrame(true);
                    colorButton.setEnabled(true);
                } else {
                    ((GPUVolumeRenderWM) renderBase).getParentFrame().setShowBoxFrame(false);
                    colorButton.setEnabled(false);
                }
            }
        } else if (source == radioButtonOrthographic) {

            if (renderBase instanceof GPUVolumeRenderWM) {
                ((GPUVolumeRenderWM) renderBase).getParentFrame().setRenderPerspective(false);
            }
        } else if (source == radioButtonPerspective) {

            if (renderBase instanceof GPUVolumeRenderWM) {
                ((GPUVolumeRenderWM) renderBase).getParentFrame().setRenderPerspective(true);
            }
        } else if (source == cubicCheck) {

            if (cubicCheck.isSelected()) {
                ((GPUVolumeRenderWM) renderBase).getParentFrame().setShowOrientationCube(true);
            } else {
                ((GPUVolumeRenderWM) renderBase).getParentFrame().setShowOrientationCube(false);
            }
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
     * Get the main control panel.
     *
     * @return  mainPanel main GUI.
     */
    public JPanel getMainPanel() {
        return mainPanel;

    }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * When the user enter the coarse and fine value, invoke this event to update fine or coarse sampling.
     *
     * @param  evt  key event
     */
    public void keyTyped(KeyEvent evt) {
        Object source = evt.getSource();
        char ch = evt.getKeyChar();

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
     * Set the radio button for view volume aligned enable or not.
     *
     * @param  flag  true enable and false disable.
     */
    public void setEnable(boolean flag) {
        viewButton.setEnabled(flag);
        viewAlignedButton.setEnabled(flag);
    }

 
    /**
     * Calls the appropriate method in the parent frame.
     *
     * @param  button  DOCUMENT ME!
     * @param  color   Color to set box frame to.
     */
    protected void setBoxColor(JButton button, Color color) {

        if (button == colorButton) {
        	// renderBase.setBoxColor(color);
            if (renderBase instanceof GPUVolumeRenderWM) {
                ViewJFrameVolumeView kParent = ((GPUVolumeRenderWM) renderBase).getParentFrame();
                if ( kParent instanceof ViewJFrameVolumeViewWildMagic )
                {
                    ((ViewJFrameVolumeViewWildMagic)kParent).setBoundingBoxColor(color);
                }                    
            }        } else if (button == colorButtonBackground) {
            // renderBase.setBackgroundColor(color);
            if (renderBase instanceof GPUVolumeRenderWM) {
                ViewJFrameVolumeView kParent = ((GPUVolumeRenderWM) renderBase).getParentFrame();
                if ( kParent instanceof ViewJFrameVolumeViewWildMagic )
                {
                    ((ViewJFrameVolumeViewWildMagic)kParent).setBackgroundColor(color);
                }                    
            }
        }
    }

    /**
     * Initializes GUI components.
     */
    private void init() {
        boundingCheck = new JCheckBox("Show bounding frame");
        boundingCheck.setFont(serif12);
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

        backgroundLabel.setFont(serif12);
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
        cubicCheck.setFont(serif12);
        cubicCheck.addActionListener(this);

        cubePanel = new JPanel(new GridBagLayout());
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        cubePanel.add(cubicCheck, gbc);
        cubePanel.setBorder(buildTitledBorder("Orientation"));

        JPanel viewTexturePanel = new JPanel();
        viewTexturePanel.setBorder(buildTitledBorder("Texture Type"));

        Box viewTextureBox = new Box(BoxLayout.Y_AXIS);
        viewButton = new JRadioButton();
        viewButton.addActionListener(this);
        viewAlignedButton = new JRadioButton();
        viewAlignedButton.addActionListener(this);
        viewTextureButtonGroup = new ButtonGroup();
        viewButton.setSelected(true);
        viewButton.setText("View volume texture   ");
        viewAlignedButton.setText("View aligned volume texture");
        viewTextureButtonGroup.add(viewButton);
        viewTextureButtonGroup.add(viewAlignedButton);

        
        viewTextureBox.add(viewButton);
        viewTextureBox.add(viewAlignedButton);
       
        viewTexturePanel.add(viewTextureBox);

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.add(panel);
        contentBox.add(panel2);
        contentBox.add(cubePanel);
        contentBox.add(projectionTypePanel);
        if ( (renderBase instanceof GPUVolumeRenderWM) &&
             !( ((GPUVolumeRenderWM)renderBase).getParentFrame() instanceof ViewJFrameVolumeViewWildMagic ))
        {
            contentBox.add(viewTexturePanel);
        }
        else
        {
            JPanel buttonPanel = new JPanel();
            /* Creates the advanced material options button, which launches the
             * material editor dialog: */
            JButton kAdvancedMaterialOptionsButton = new JButton("Material");
            kAdvancedMaterialOptionsButton.setToolTipText("Change material properties");
            kAdvancedMaterialOptionsButton.addActionListener(this);
            kAdvancedMaterialOptionsButton.setActionCommand("AdvancedMaterialOptions");
            kAdvancedMaterialOptionsButton.setEnabled(true);
            buttonPanel.add(kAdvancedMaterialOptionsButton);
            contentBox.add(buttonPanel);
        }

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(contentBox, BorderLayout.NORTH);

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.add(scroller);

        setEnable(false);
       
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

    public void setMaterial(Material kMaterial, int iIndex)
    {
        MaterialState kMaterialState = new MaterialState();

        javax.vecmath.Color3f kColor = new javax.vecmath.Color3f();
        kMaterial.getAmbientColor(kColor);
        kMaterialState.Ambient = new ColorRGB(kColor.x, kColor.y, kColor.z);

        kMaterial.getEmissiveColor(kColor);
        kMaterialState.Emissive = new ColorRGB(kColor.x, kColor.y, kColor.z);

        kMaterial.getDiffuseColor(kColor);
        kMaterialState.Diffuse = new ColorRGB(kColor.x, kColor.y, kColor.z);

        kMaterial.getSpecularColor(kColor);
        kMaterialState.Specular = new ColorRGB(kColor.x, kColor.y, kColor.z);

        kMaterialState.Shininess = kMaterial.getShininess();

        if ( (renderBase instanceof GPUVolumeRenderWM) &&
             ( ((GPUVolumeRenderWM)renderBase).getParentFrame() instanceof ViewJFrameVolumeViewWildMagic ))
        {
            ((ViewJFrameVolumeViewWildMagic)((GPUVolumeRenderWM)renderBase).getParentFrame()).getRaycastRenderWM().SetMaterialState( kMaterialState );
        }
    }


    public void restorePerVertexColor(Material kMaterial, int index)
    {
        setMaterial( kMaterial, index );
    }

}
