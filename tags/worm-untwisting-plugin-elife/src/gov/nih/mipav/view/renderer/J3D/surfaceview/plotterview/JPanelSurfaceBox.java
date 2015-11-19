package gov.nih.mipav.view.renderer.J3D.surfaceview.plotterview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to turn bounding box of surface renderer on and off, and to change the color of the frame.
 */
public class JPanelSurfaceBox extends JPanelRendererJ3D {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1435246141584042105L;

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

    /** DOCUMENT ME! */
    private JPanel buttonPanel;

    /** DOCUMENT ME! */
    private float coarseValue, fineValue;

    /** DOCUMENT ME! */
    private JTextField fine, coarse;

    /** DOCUMENT ME! */
    private JLabel fineLabel, coarseLabel;

    /** Flag indicating if box is on or off. */
    private boolean flag = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for turning bounding box frame on and off.
     *
     * @param  parent  Should be of type ViewJFrameSurfaceRenderer
     */
    public JPanelSurfaceBox(RenderViewBase parent) {
        super(parent);
        init();
        cubicCheck.setVisible(false);
        cubePanel.setVisible(false);
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

        if ((source instanceof JButton) && (source != cancelButton)) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener((JButton) source),
                                                 new CancelListener());
        } else if (source == boundingCheck) {

            if (boundingCheck.isSelected() != flag) {
                flag = boundingCheck.isSelected();

                if (flag == true) {
                    ((SurfacePlotter) renderBase).showBoxFrame();
                    colorButton.setEnabled(true);
                } else {
                    ((SurfacePlotter) renderBase).hideBoxFrame();
                    colorButton.setEnabled(false);
                }
            }
        } else if (source == radioButtonOrthographic) {

            if (renderBase instanceof SurfaceRender) {
                ((SurfaceRender) renderBase).setRenderPerspective(false);
            } else if (renderBase instanceof SurfacePlotter) {
                ((SurfacePlotter) renderBase).setRenderPerspective(false);
            }
        } else if (source == radioButtonPerspective) {

            if (renderBase instanceof SurfaceRender) {
                ((SurfaceRender) renderBase).setRenderPerspective(true);
            } else if (renderBase instanceof SurfacePlotter) {
                ((SurfacePlotter) renderBase).setRenderPerspective(true);
            }
        } else if (source == viewButton) {
            ((SurfaceRender) renderBase).setViewTextureAligned(false);
            setTextEnabled(false);
        } else if (source == viewAlignedButton) {
            coarse.setText(Float.toString(((SurfaceRender) renderBase).getSliceSpacingCoarse()));
            ((SurfaceRender) renderBase).setViewTextureAligned(true);
            setTextEnabled(true);
        } else if (source == cubicCheck) {

            if (cubicCheck.isSelected()) {
                ((SurfaceRender) renderBase).addCubicControl();
            } else {
                ((SurfaceRender) renderBase).removeCubicControl();
            }
        } else if (source == cancelButton) {
            this.setVisible(false);
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getCoarseVal() {
        return Float.parseFloat(coarse.getText());
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel() {
        buttonPanel.setVisible(false);

        return mainPanel;

    }

    /**
     * DOCUMENT ME!
     *
     * @param  evt  DOCUMENT ME!
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
     * DOCUMENT ME!
     *
     * @param  _color  DOCUMENT ME!
     */
    public void setColorButton(Color _color) {
        colorButtonBackground.setBackground(_color);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setEnable(boolean flag) {
        viewButton.setEnabled(flag);
        viewAlignedButton.setEnabled(flag);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setTextEnabled(boolean flag) {
        fineLabel.setEnabled(false);
        coarseLabel.setEnabled(flag);
        fine.setEnabled(false);
        coarse.setEnabled(flag);
    }


    /**
     * /** Takes a text field and forces the text field to accept numbers, backspace and delete-key entries.
     *
     * @param  status  Text field to modify.
     */
    /**
     * protected void makeNumericsOnly(JTextField txt, boolean allowFloatingPoint) { if (allowFloatingPoint) {
     * txt.addKeyListener(new KeyAdapter() { // make the field public void keyTyped(KeyEvent evt) { // not accept
     * letters JTextField t = (JTextField) evt.getComponent(); char ch = evt.getKeyChar(); if (ch == KeyEvent.VK_ENTER)
     * { // make sure the enter key acts as clicking OK t.getNextFocusableComponent(); } else if (ch == '.') { if
     * (t.getSelectedText() != null) { if (t.getText().length() == t.getSelectedText().length()) { t.setText("0"); }
     * else if ( (t.getText().indexOf('.') != -1) // there is a '.', but no && (t.getSelectedText().indexOf('.') == -1))
     * { // in the selected text evt.consume(); // ignore } } else if (t.getText().indexOf('.') != -1) { evt.consume();
     * } else if (t.getText().length() == 0) { t.setText("0"); } } //negative signs are not allowed else if ( ( (ch <
     * '0') || (ch > '9')) && ( (ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE))) { // if is the case that
     * ch is outside the bounds of a number AND it is the case that ch is neither a BS or a DE, then... // key is not a
     * digit or a deletion char evt.consume(); } } }); } else { txt.addKeyListener(new KeyAdapter() { // make the field
     * public void keyTyped(KeyEvent evt) { // not accept letters JTextField t = (JTextField) evt.getComponent(); char
     * ch = evt.getKeyChar(); if (ch == KeyEvent.VK_ENTER) { // make sure the enter key acts as clicking OK
     * t.getNextFocusableComponent(); } else if ( ( (ch < '0') || (ch > '9')) && ( (ch != KeyEvent.VK_DELETE) && (ch !=
     * KeyEvent.VK_BACK_SPACE))) { // if is the case that ch is outside the bounds of a number AND it is the case that
     * ch is neither a BS or a DE, then... // key is not a digit or a deletion char evt.consume(); } } }); } }
     *
     * @param  status  DOCUMENT ME!
     */

    /**
     * Makes the dialog visible next to the parent frame. If this makes it go off the screen, puts the dialog in the
     * center of the screen.
     *
     * @param  status  Flag indicating if the dialog should be visible.
     */
    public void setVisible(boolean status) {
        Point location = new Point();
        location.x = renderBase.getLocation().x - getWidth();
        location.y = renderBase.getLocation().y + renderBase.getHeight();

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
     * Calls the appropriate method in the parent frame.
     *
     * @param  button  DOCUMENT ME!
     * @param  color   Color to set box frame to.
     */
    protected void setBoxColor(JButton button, Color color) {

        if (button == colorButton) {
            renderBase.setBoxColor(color);
        } else if (button == colorButtonBackground) {
            renderBase.setBackgroundColor(color);
        }
    }

    /**
     * Initializes GUI components.
     */
    private void init() {

        // setTitle("Bounding box");
        mainPanel = new JPanel();
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
        fine = new JTextField(3);

        if (renderBase instanceof SurfaceRender) {
            fine.setText(Float.toString(((SurfaceRender) renderBase).getSliceSpacingFine()));
        }

        fine.addKeyListener(this);
        MipavUtil.makeNumericsOnly(fine, true);

        // makeNumericsOnly(fine, true);
        gbc.gridx = 1;
        inputPanel.add(fine, gbc);

        coarseLabel = new JLabel("Coarse");
        coarseLabel.setFont(serif12);
        coarseLabel.setForeground(Color.black);
        coarseLabel.setRequestFocusEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 1;
        inputPanel.add(coarseLabel, gbc);
        coarse = new JTextField(3);

        if (renderBase instanceof SurfaceRender) {
            coarse.setText(Float.toString(((SurfaceRender) renderBase).getSliceSpacingCoarse()));
        }

        coarse.addKeyListener(this);
        MipavUtil.makeNumericsOnly(coarse, true);

        // makeNumericsOnly(coarse, true);
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

        buttonPanel = new JPanel();
        buildCancelButton();
        cancelButton.setText("Close");
        buttonPanel.add(cancelButton);
        contentBox.add(buttonPanel);

        // getContentPane().add(contentBox);
        mainPanel.add(contentBox);
        setEnable(false);
        setTextEnabled(false);
        // pack();
        // setVisible(true);
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
