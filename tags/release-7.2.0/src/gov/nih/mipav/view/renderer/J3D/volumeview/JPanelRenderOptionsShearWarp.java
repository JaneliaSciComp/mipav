package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to turn bounding box of surface renderer on and off, and to change the color of the frame.
 */
public class JPanelRenderOptionsShearWarp extends JPanelRendererJ3D {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4059526095627109190L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Button Panel. */
    private JPanel buttonPanel;

    /** Color button for changing color. */
    private JButton colorButton;

    /** Color button for changing z color. */
    private JButton colorButtonBackground;

    /** Color chooser dialog. */
    private ViewJColorChooser colorChooser;

    /** Volume rendering parent frame. */
    private VolumeRendererShearWarp myParent;

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
    //private JTextField spaceText;

    /** Text field of the ray trace step size. */
    private JTextField stepText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for turning bounding box frame on and off.
     *
     * @param  parent  DOCUMENT ME!
     */
    public JPanelRenderOptionsShearWarp(VolumeRendererShearWarp parent) {
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
        } else if (source == applyButton) {
            setStepSize();
        } else if (source == closeButton) {
            setVisible(false);
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
     * @return  JPanel main panel.
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
     * Resizie the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   int width
     * @param  frameHeight  int height
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
    public void setStepSize() {
        String tmpStr;
        int stepSize = 1;

        tmpStr = stepText.getText();

        if (testParameter(tmpStr, 1, 20)) {
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
     * finalize - calls dispose.
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
     * @param  button  DOCUMENT ME!
     * @param  color   Color to set box frame to.
     */
    protected void setBoxColor(JButton button, Color color) {

        if (button == colorButton) {
            myParent.setBoxColor(color);
        } else if (button == colorButtonBackground) {
            myParent.setBackgroundColor(color);
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
        gbc.insets = new Insets(5, 5, 5, 5);
        gbc.gridx = 1;

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
        radioButtonPerspective.setText("Perspective  View");
        radioButtonGroupProjections.add(radioButtonOrthographic);
        radioButtonGroupProjections.add(radioButtonPerspective);
        projectionTypeBox.add(radioButtonOrthographic);
        projectionTypeBox.add(radioButtonPerspective);
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
        stepPanel.setBorder(buildTitledBorder("Shear warp optimization during mouse rotation"));

        JLabel stepLabel = new JLabel("Composite step size ");

        int stepSize = 1;

        stepSize = myParent.getStepSize();
        stepText = new JTextField(Integer.toString(stepSize));
        stepPanel.add(stepLabel, gbc);
        gbc.gridx = 1;
        stepPanel.add(stepText, gbc);

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        // contentBox.add(panel);
        contentBox.add(panel2);
        contentBox.add(projectionTypePanel);
        contentBox.add(stepPanel);

        buttonPanel = new JPanel();
        buildApplyButton();
        buttonPanel.add(applyButton);
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
        private static final long serialVersionUID = 6247533773322303653L;

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
