package gov.nih.mipav.view.renderer.volumeview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.surfaceview.*;
import gov.nih.mipav.view.WildMagic.ApplicationDemos.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import javax.media.j3d.Material;


/**
 * Dialog to turn bounding box of surface renderer on and off, and to change the color of the frame.
 */
public class JPanelRenderOptionsGPU extends JPanelRendererBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6813615755994835860L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Button Panel. */
    private JPanel buttonPanel;

    /** Volume rendering parent frame. */
    private GPUVolumeRender myParent;

    /** Button group for projections. */
    private ButtonGroup radioButtonGroupProjections;

    /** Radio Button for Orthographic rendering. */
    private JRadioButton radioButtonOrthographic;

    /** Radio Button for Perspective rendering. */
    private JRadioButton radioButtonPerspective;

    /** Scroll pane. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;

    /** The description of the lights so they can be duplicated in the "Advanced Material Properties" dialog:. */
    private GeneralLight[] m_akLights;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for turning bounding box frame on and off.
     *
     * @param  parent  parent reference
     */
    public JPanelRenderOptionsGPU(GPUVolumeRender parent) {
        myParent = parent;
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

        if (source == radioButtonOrthographic) {
            setRenderPerspective(false);
        } else if (source == radioButtonPerspective) {
            setRenderPerspective(true);
        }
        if ( command.equals( "AdvancedMaterialOptions" ) )
        {
            new JFrameSurfaceMaterialProperties(this, 0, m_akLights, 0, new Material() );
        }
    }

    /**
     * Dispose global variables.
     *
     * @param  flag  dispose super or not.
     */
    public void disposeLocal(boolean flag) {
        radioButtonOrthographic = null;
        radioButtonPerspective = null;
        radioButtonGroupProjections = null;
    }

    /**
     * Get the main control panel.
     *
     * @return  JPanel main panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Sets the flags for the checkboxes and resets labels.
     *
     * @param  event  Event that triggered this function.
     */
    public synchronized void itemStateChanged(ItemEvent event) { }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   DOCUMENT ME!
     * @param  frameHeight  DOCUMENT ME!
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - buttonPanel.getHeight()));
        scroller.setSize(new Dimension(panelWidth, frameHeight - buttonPanel.getHeight()));
        scroller.revalidate();

    }

    /**
     * Enable perspective projection rendering; otherwise use orthographic projection.
     *
     * @param  bEnable  true to enable perspective projection
     */
    public void setRenderPerspective(boolean bEnable) {
        if (bEnable)
        {
            myParent.setPerspectiveProjection();
        }
        else
        {
            myParent.setOrthographicProjection();
        }
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }


    /**
     * Initializes GUI components.
     */
    private void init() {

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());

        // Put the drawing area in a scroll pane.
        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);


        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 1;
        gbc.insets = new Insets(5, 5, 5, 5);

        JPanel projectionTypePanel = new JPanel();

        projectionTypePanel.setBorder( JPanelRendererBase.buildTitledBorder("Projection Type"));

        Box projectionTypeBox = new Box(BoxLayout.X_AXIS);

        radioButtonPerspective = new JRadioButton();
        radioButtonOrthographic = new JRadioButton();
        radioButtonGroupProjections = new ButtonGroup();

        radioButtonOrthographic.setText("Orthographic View");
        radioButtonPerspective.setText("Perspective View   ");
        radioButtonGroupProjections.add(radioButtonPerspective);
        radioButtonGroupProjections.add(radioButtonOrthographic);
        projectionTypeBox.add(radioButtonPerspective);
        projectionTypeBox.add(radioButtonOrthographic);
        projectionTypePanel.add(projectionTypeBox);

        boolean parallel = false;
        radioButtonOrthographic.setSelected(parallel);
        radioButtonPerspective.setSelected(!parallel);

        radioButtonOrthographic.addActionListener(this);
        radioButtonPerspective.addActionListener(this);

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        contentBox.add(projectionTypePanel);

        buttonPanel = new JPanel();
        /* Creates the advanced material options button, which launches the
         * material editor dialog: */
        JButton kAdvancedMaterialOptionsButton = new JButton("Material");
        kAdvancedMaterialOptionsButton.setToolTipText("Change material properties");
        kAdvancedMaterialOptionsButton.addActionListener(this);
        kAdvancedMaterialOptionsButton.setActionCommand("AdvancedMaterialOptions");
        kAdvancedMaterialOptionsButton.setEnabled(true);
        buttonPanel.add(kAdvancedMaterialOptionsButton);
        contentBox.add(buttonPanel);

        scrollPanel.add(contentBox, BorderLayout.NORTH);
        mainPanel.add(scroller, BorderLayout.NORTH);
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
        private static final long serialVersionUID = 9109313609990821556L;

        /**
         * DOCUMENT ME!
         *
         * @param  g  DOCUMENT ME!
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);

        }
    }

    public void updateLighting( GeneralLight[] akLights )
    {
        m_akLights = akLights;
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

        myParent.SetMaterialState( kMaterialState );
    }


    public void restorePerVertexColor(Material kMaterial, int index)
    {
        setMaterial( kMaterial, index );
    }

}
