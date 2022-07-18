package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to turn bounding box of surface renderer on and off, and to change the color of the frame.
 */
public class JPanelRenderOptionsRayCast extends JPanelRendererJ3D {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6813615755994835860L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Blur image check box. */
    private JCheckBox blurBox;

    /** Button Panel. */
    private JPanel buttonPanel;

    /** Color button for changing color. */
    private JButton colorButton;

    /** Color button for changing z color. */
    private JButton colorButtonBackground;

    /** Color chooser dialog. */
    private ViewJColorChooser colorChooser;

    /** Diffuse color button for vertex material. */
    private JButton diffuseButton;

    /** Flag indicating if box is on or off. */
    //private boolean flag = false;

    /** DOCUMENT ME! */
    private ButtonGroup m_kRadioGroupMaxRenExtent;

    /** DOCUMENT ME! */
    private JRadioButton m_kRadioMaxRenExtent_1024;

    /** DOCUMENT ME! */
    private JRadioButton m_kRadioMaxRenExtent_128;

    /** DOCUMENT ME! */
    private JRadioButton m_kRadioMaxRenExtent_256;

    /** DOCUMENT ME! */
    private JRadioButton m_kRadioMaxRenExtent_512;

    /**
     * Radio Button Group for the Render Image Target size of the raycast rendered image. Target sizes options are
     * preset to be typical texture sizes:
     */
    private JRadioButton m_kRadioMaxRenExtent_64;

    /** Volume rendering parent frame. */
    private VolumeRendererRayCast myParent;

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

    /** Text field of the ray trace space size. */
    private JTextField spaceText;

    /** Specular color button for vertex material. */
    private JButton specularButton;


    /** Text field of the ray trace step size. */
    private JTextField stepText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for turning bounding box frame on and off.
     *
     * @param  parent  parent reference
     */
    public JPanelRenderOptionsRayCast(VolumeRendererRayCast parent) {
        super(parent);
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

        if ((source == colorButton) || (source == colorButtonBackground)) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener((JButton) source),
                                                 new CancelListener());
        } else if (source == radioButtonOrthographic) {
            setRenderPerspective(true);
        } else if (source == radioButtonPerspective) {
            setRenderPerspective(false);
        }
        /* Set the MaxRenExtent value depending on which radio button is
         * selected: */
        else if (source == m_kRadioMaxRenExtent_64) {
            myParent.setMaxRenExtent(64);
        } else if (source == m_kRadioMaxRenExtent_128) {
            myParent.setMaxRenExtent(128);
        } else if (source == m_kRadioMaxRenExtent_256) {
            myParent.setMaxRenExtent(256);
        } else if (source == m_kRadioMaxRenExtent_512) {
            myParent.setMaxRenExtent(512);
        } else if (source == m_kRadioMaxRenExtent_1024) {
            myParent.setMaxRenExtent(1024);
        } else if (source == applyButton) {
            setStepSize();
            setSpaceSize();
        } else if (source == closeButton) {
            setVisible(false);
        } else if (source == blurBox) {

            if (!blurBox.isSelected()) {
                myParent.setBlurFlag(false);
            } else {
                myParent.setBlurFlag(true);
            }
        } else if (source == diffuseButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(diffuseButton),
                                                 new CancelListener());
        } else if (source == specularButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(specularButton),
                                                 new CancelListener());
        }

    }

    /**
     * Dispose global variables.
     *
     * @param  flag  dispose super or not.
     */
    public void disposeLocal(boolean flag) {
        colorButton = null;
        colorButtonBackground = null;

        if (colorChooser != null) {
            colorChooser = null;
        }

        radioButtonOrthographic = null;
        radioButtonPerspective = null;
        radioButtonGroupProjections = null;
        stepText = null;

        if (flag == true) {
            super.disposeLocal();
        }

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
        myParent.setParallel(bEnable);
        myParent.updateImages(true);
    }

    /**
     * Enable perspective projection rendering; otherwise use orthographic projection.
     */
    public void setSpaceSize() {
        String tmpStr;
        int spaceSize = 1;

        tmpStr = spaceText.getText();

        if (testParameter(tmpStr, 1, 8)) {
            spaceSize = Integer.valueOf(tmpStr).intValue();
        } else {
            spaceText.requestFocus();
            spaceText.selectAll();
        }

        myParent.setSpaceSize(spaceSize);
        // myParent.updateImages(true);
    }

    /**
     * Enable perspective projection rendering; otherwise use orthographic projection.
     */
    public void setStepSize() {
        String tmpStr;
        int stepSize = 1;

        tmpStr = stepText.getText();

        if (testParameter(tmpStr, 1, 8)) {
            stepSize = Integer.valueOf(tmpStr).intValue();
        } else {
            stepText.requestFocus();
            stepText.selectAll();
        }

        myParent.setStepSize(stepSize);
        // myParent.updateImages(true);
    }

    /**
     * Makes the dialog visible next to the parent frame. If this makes it go off the screen, puts the dialog in the
     * center of the screen.
     *
     * @param  status  Flag indicating if the dialog should be visible.
     */
    public void setVisible(boolean status) {
        Point location = new Point();

        location.x = renderBase.getLocation().x + renderBase.getWidth();
        location.y = renderBase.getLocation().y; // + parentFrame.getHeight() - this.getHeight();

        if (((location.x + getWidth()) < Toolkit.getDefaultToolkit().getScreenSize().width) &&
                ((location.y + getHeight()) < Toolkit.getDefaultToolkit().getScreenSize().height)) {
            setLocation(location);
        } else {
            Rectangle dialogBounds = getBounds();

            setLocation((Toolkit.getDefaultToolkit().getScreenSize().width / 2) - (dialogBounds.width / 2),
                        (Toolkit.getDefaultToolkit().getScreenSize().height / 2) - (dialogBounds.height / 2));
        }

        super.setVisibleStandard(status);
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
     * Calls the appropriate method in the parent frame.
     *
     * @param  button  color button reference.
     * @param  color   Color to set box frame to.
     */
    protected void setBoxColor(JButton button, Color color) {

        if (button == colorButton) {
            myParent.setBoxColor(color);
        } else if (button == colorButtonBackground) {
            myParent.setBackgroundColor(color);
        } else if (button == diffuseButton) {
            myParent.setVertexDiffuse(color);
        } else if (button == specularButton) {
            myParent.setVertexSpecular(color);
        }
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

        colorButtonBackground = new JButton();
        colorButtonBackground.setPreferredSize(new Dimension(25, 25));
        colorButtonBackground.setToolTipText("Change background color");
        colorButtonBackground.addActionListener(this);
        colorButtonBackground.setBackground(Color.black);

        JLabel backgroundLabel = new JLabel("Background color");
        backgroundLabel.setFont(serif12);
        backgroundLabel.setForeground(Color.black);

        JPanel panel2 = new JPanel();
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
        radioButtonOrthographic = new JRadioButton();
        radioButtonGroupProjections = new ButtonGroup();

        radioButtonOrthographic.setText("Orthographic View");
        radioButtonPerspective.setText("Perspective View   ");
        radioButtonGroupProjections.add(radioButtonPerspective);
        radioButtonGroupProjections.add(radioButtonOrthographic);
        projectionTypeBox.add(radioButtonPerspective);
        projectionTypeBox.add(radioButtonOrthographic);
        projectionTypePanel.add(projectionTypeBox);

        boolean parallel = true;

        parallel = myParent.getParallel();
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

        JPanel stepPanel = new JPanel(new GridBagLayout());

        stepPanel.setBorder(buildTitledBorder("Ray trace optimization during mouse rotation"));

        JLabel stepLabel = new JLabel("Step size ");

        int stepSize = 1;

        stepSize = myParent.getStepSize();
        stepText = new JTextField(Integer.toString(stepSize));
        stepPanel.add(stepLabel, gbc);
        gbc.gridx = 1;
        stepPanel.add(stepText, gbc);

        JLabel spaceLabel = new JLabel("Space size ");
        int spaceSize = 1;

        spaceSize = myParent.getSpaceSize();
        spaceText = new JTextField(Integer.toString(spaceSize));
        gbc.gridx = 0;
        gbc.gridy = 1;
        stepPanel.add(spaceLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        stepPanel.add(spaceText, gbc);

        /* Initialize the Render Target Image size radio buttons: */
        m_kRadioMaxRenExtent_64 = new JRadioButton("64x64");
        m_kRadioMaxRenExtent_128 = new JRadioButton("128x128");
        m_kRadioMaxRenExtent_256 = new JRadioButton("256x256");
        m_kRadioMaxRenExtent_512 = new JRadioButton("512x512");
        m_kRadioMaxRenExtent_1024 = new JRadioButton("1024x1024");
        m_kRadioGroupMaxRenExtent = new ButtonGroup();
        m_kRadioGroupMaxRenExtent.add(m_kRadioMaxRenExtent_64);
        m_kRadioGroupMaxRenExtent.add(m_kRadioMaxRenExtent_128);
        m_kRadioGroupMaxRenExtent.add(m_kRadioMaxRenExtent_256);
        m_kRadioGroupMaxRenExtent.add(m_kRadioMaxRenExtent_512);
        m_kRadioGroupMaxRenExtent.add(m_kRadioMaxRenExtent_1024);

        /* The current (default) texture size of the rendered image is used to
         * set which radio button is initially selected: */
        int iMaxRenExtent = myParent.getMaxRenExtent();

        if (iMaxRenExtent <= 64) {
            m_kRadioMaxRenExtent_64.setSelected(true);
        }

        if (iMaxRenExtent <= 128) {
            m_kRadioMaxRenExtent_128.setSelected(true);
        } else if (iMaxRenExtent <= 256) {
            m_kRadioMaxRenExtent_256.setSelected(true);
        } else if (iMaxRenExtent <= 512) {
            m_kRadioMaxRenExtent_512.setSelected(true);
        } else if (iMaxRenExtent <= 1024) {
            m_kRadioMaxRenExtent_1024.setSelected(true);
        }

        m_kRadioMaxRenExtent_64.addActionListener(this);
        m_kRadioMaxRenExtent_128.addActionListener(this);
        m_kRadioMaxRenExtent_256.addActionListener(this);
        m_kRadioMaxRenExtent_512.addActionListener(this);
        m_kRadioMaxRenExtent_1024.addActionListener(this);

        JPanel kMaxRenExtentPanel = new JPanel();
        kMaxRenExtentPanel.setBorder(buildTitledBorder("Render Target Image Size"));
        kMaxRenExtentPanel.add(m_kRadioMaxRenExtent_64);
        kMaxRenExtentPanel.add(m_kRadioMaxRenExtent_128);
        kMaxRenExtentPanel.add(m_kRadioMaxRenExtent_256);
        kMaxRenExtentPanel.add(m_kRadioMaxRenExtent_512);
        kMaxRenExtentPanel.add(m_kRadioMaxRenExtent_1024);

        JPanel blurPanel = new JPanel();
        blurPanel.setBorder(buildTitledBorder("Smooth Surface"));
        blurBox = new JCheckBox();
        blurBox.setSelected(false);
        blurBox.addActionListener(this);
        blurBox.setActionCommand("Blur");
        blurBox.setText("Smooth");
        blurBox.setFont(serif12);
        blurPanel.add(blurBox);


        diffuseButton = new JButton();
        diffuseButton.setPreferredSize(new Dimension(25, 25));
        diffuseButton.setToolTipText("Change diffuse color");
        diffuseButton.addActionListener(this);
        diffuseButton.setBackground(Color.white);

        JLabel fissueLabel = new JLabel("diffuse color");
        fissueLabel.setFont(serif12);
        fissueLabel.setForeground(Color.black);

        specularButton = new JButton();
        specularButton.setPreferredSize(new Dimension(25, 25));
        specularButton.setToolTipText("Change sepcular color");
        specularButton.addActionListener(this);
        specularButton.setBackground(Color.white);

        JLabel specularLabel = new JLabel("specular color");
        specularLabel.setFont(serif12);
        specularLabel.setForeground(Color.black);

        JPanel materialPanel = new JPanel();
        gbc.gridx = 0;
        gbc.gridy = 0;
        materialPanel.add(diffuseButton, gbc);
        gbc.gridx = 1;
        materialPanel.add(fissueLabel, gbc);
        gbc.gridx = 2;
        materialPanel.add(specularButton, gbc);
        gbc.gridx = 3;
        materialPanel.add(specularLabel, gbc);
        materialPanel.setBorder(buildTitledBorder("Material Color"));

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.add(panel2);
        contentBox.add(projectionTypePanel);
        contentBox.add(stepPanel);
        contentBox.add(kMaxRenExtentPanel);
        contentBox.add(blurPanel);
        contentBox.add(materialPanel);

        buttonPanel = new JPanel();

        buildApplyButton();
        buttonPanel.add(applyButton);

        contentBox.add(buttonPanel);

        // make Apply button
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

    /**
     * Pick up the selected color and call method to change the VOI color.
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
         * Get color from chooser and set button and VOI color.
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
