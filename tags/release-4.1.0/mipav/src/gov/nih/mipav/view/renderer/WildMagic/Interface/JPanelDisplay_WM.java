package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.*;

import java.awt.*;
import java.awt.event.*;

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

    /** Coarse and fine value. */
    private float coarseValue, fineValue;

    /** Coarse and fine value text field. */
    private JTextField fine, coarse;

    /** Coarse and fine value label. */
    private JLabel fineLabel, coarseLabel;

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
    public JPanelDisplay_WM(VolumeTriPlanarInterface parent) {
        m_kVolumeViewer = parent;
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

        if (source instanceof JButton) {
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
        fineLabel = null;
        coarseLabel = null;
        fine = null;
        coarse = null;
    }

    /**
     * Get the coarse value.
     *
     * @return  float coarse value.
     */
    public float getCoarseVal() {
        return Float.parseFloat(coarse.getText());
    }

    /**
     * Get the fine value.
     *
     * @return  float fine value.
     */
    public float getFineVal() {
        return Float.parseFloat(fine.getText());
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

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        contentBox.add(panel);
        contentBox.add(panel2);
        contentBox.add(cubePanel);
        contentBox.add(projectionTypePanel);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(contentBox, BorderLayout.NORTH);

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.add(scroller);
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
