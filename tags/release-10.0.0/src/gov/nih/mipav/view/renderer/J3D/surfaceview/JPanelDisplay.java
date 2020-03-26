package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * The display panel control the red bounding box frame ( on/off ), texture aligned rendering mode, cubic controk,
 * perspective and parrallel viewing mode, and back ground color.
 */
public class JPanelDisplay extends JPanelRendererJ3D implements KeyListener {

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
    public JPanelDisplay(RenderViewBase parent) {
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

        if (source instanceof JButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener((JButton) source),
                                                 new CancelListener());
        } else if (source == boundingCheck) {

            if (boundingCheck.isSelected() != flag) {
                flag = boundingCheck.isSelected();

                if (flag == true) {
                    ((SurfaceRender) renderBase).showBoxFrame();
                    colorButton.setEnabled(true);
                } else {
                    ((SurfaceRender) renderBase).hideBoxFrame();
                    colorButton.setEnabled(false);
                }
            }
        } else if (source == radioButtonOrthographic) {

            if (renderBase instanceof SurfaceRender) {
                ((SurfaceRender) renderBase).setRenderPerspective(false);
            }
        } else if (source == radioButtonPerspective) {

            if (renderBase instanceof SurfaceRender) {
                ((SurfaceRender) renderBase).setRenderPerspective(true);
            }
        } else if (source == viewButton) {
            ((SurfaceRender) renderBase).setViewTextureAligned(false);
            setTextEnabled(false);
        } else if (source == viewAlignedButton) {
            ((SurfaceRender) renderBase).setViewTextureAligned(true);
            setTextEnabled(true);
        } else if (source == cubicCheck) {

            if (cubicCheck.isSelected()) {
                ((SurfaceRender) renderBase).addCubicControl();
            } else {
                ((SurfaceRender) renderBase).removeCubicControl();
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

        if (ch == KeyEvent.VK_ENTER) {
            fineValue = Float.parseFloat(fine.getText());
            coarseValue = Float.parseFloat(coarse.getText());
            ((SurfaceRender) renderBase).setSliceSpacingFine(fineValue);
            ((SurfaceRender) renderBase).setSliceSpacingCoarse(coarseValue);

            if (source.equals(fine)) {
                ((SurfaceRender) renderBase).useSliceSpacingFine();
            } else {
                ((SurfaceRender) renderBase).useSliceSpacingCoarse();
            }
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
     * Set the radio button for view volume aligned enable or not.
     *
     * @param  flag  true enable and false disable.
     */
    public void setEnable(boolean flag) {
        viewButton.setEnabled(flag);
        viewAlignedButton.setEnabled(flag);
    }

    /**
     * Enable the the fine or coarse labels and text fields with given flag value.
     *
     * @param  flag  true enable, false disable.
     */
    public void setTextEnabled(boolean flag) {
        fineLabel.setEnabled(flag);
        coarseLabel.setEnabled(flag);
        fine.setEnabled(flag);
        coarse.setEnabled(flag);
    }

    /**
     * Calls the appropriate method in the parent frame.
     *
     * @param  button  DOCUMENT ME!
     * @param  color   Color to set box frame to.
     */
    protected void setBoxColor(JButton button, Color color) {

        if (button == colorButton) {
            renderBase.setBoxColor(color);
        }
        if (button == colorButtonBackground) {
            renderBase.setBackgroundColor(color);
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

        JPanel inputPanel = new JPanel(new GridBagLayout());

        fineLabel = new JLabel("Fine");
        fineLabel.setFont(serif12);
        fineLabel.setForeground(Color.black);
        fineLabel.setRequestFocusEnabled(false);
        gbc.weightx = 0.5;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.insets = new Insets(1, 1, 1, 1);
        gbc.gridx = 0;
        gbc.gridy = 0;
        inputPanel.add(fineLabel, gbc);
        fine = new JTextField(8);
        fineValue = ((SurfaceRender) renderBase).getSliceSpacingFine();

        float resolX = renderBase.getImageA().getFileInfo(0).getResolutions()[0];

        if ((1.0 - resolX) >= 0.5) {
            fineValue = fineValue / (1.0f / resolX);
        }

        fine.setText(MipavUtil.makeFloatString(fineValue, 7));
        fine.addKeyListener(this);
        MipavUtil.makeNumericsOnly(fine, true, true);
        gbc.gridx = 1;
        inputPanel.add(fine, gbc);

        coarseLabel = new JLabel("Coarse");
        coarseLabel.setFont(serif12);
        coarseLabel.setForeground(Color.black);
        coarseLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 1;
        inputPanel.add(coarseLabel, gbc);
        coarse = new JTextField(8);
        coarseValue = ((SurfaceRender) renderBase).getSliceSpacingCoarse();

        if ((1.0 - resolX) >= 0.5) {
            coarseValue = coarseValue / (1.0f / resolX);
        }

        coarse.setText(MipavUtil.makeFloatString(coarseValue, 7));
        coarse.addKeyListener(this);
        MipavUtil.makeNumericsOnly(coarse, true, true);
        gbc.gridx = 1;
        inputPanel.add(coarse, gbc);

        viewTextureBox.add(viewButton);
        viewTextureBox.add(viewAlignedButton);
        viewTextureBox.add(inputPanel);
        viewTexturePanel.add(viewTextureBox);

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.add(panel);
        contentBox.add(panel2);
        contentBox.add(cubePanel);
        contentBox.add(projectionTypePanel);
        contentBox.add(viewTexturePanel);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(contentBox, BorderLayout.NORTH);

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.add(scroller);

        setEnable(false);
        setTextEnabled(false);
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
