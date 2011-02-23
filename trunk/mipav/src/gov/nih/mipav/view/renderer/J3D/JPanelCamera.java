package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.view.*;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * JPanelCamera creates the camera control panel for the 3D visulization frame. The basic functionalities include auto
 * and manual snapshot 3D images, and create a film clip from the captuered 3D images.
 *
 * @version  0.1 May, 2003
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   Ruida Cheng
 */
public class JPanelCamera extends JPanelRendererJ3D implements ActionListener, MouseListener, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7970872222123739616L;

    /** Stardard mode of the camera auto snapshooting. */
    public static final int STD_MODE = 0;

    /** Fly mode of the camera manually snapshooting. */
    public static final int FLY_MODE = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Button panel. */
    private JPanel buttonPanel;

    /** Label of the rotation degree. */
    private JLabel degreeLabel;

    /** Textfield of the roation degree. */
    private JTextField degreeText;

    /** Raido Button group. */
    private JRadioButton flyButton;

    /** X, Y, Z labels inside the rotation panel. */
    private JLabel labelX, labelY, labelZ;

    /** Left panel mouse button down. */
    private JButton leftDownButton;

    /** Left panel empty button. */
    private JButton leftDownButtonEmpty;

    /** Left panel mouse button left. */
    private JButton leftLeftButton;

    /** Left panel mouse button right. */
    private JButton leftRightButton;

    /** Left panel mouse button up. */
    private JButton leftUpButton;

    /** Empty button group. */
    private JButton leftUpButtonEmpty;

    /** Middle panel down button. */
    private JButton middleDownButton;

    /** Middel panel left button. */
    private JButton middleLeftButton;

    /** Middle panel empty button. */
    private JButton middleLeftButtonEmpty;

    /** Middle panel right button. */
    private JButton middleRightButton;

    /** Middle panel right button empty. */
    private JButton middleRightButtonEmpty;

    /** Middel panel up button. */
    private JButton middleUpButton;

    /** Current mode std or fly. */
    private int mode;

    /** Panel holds the mouse move buttons. */
    private JPanel mousePanel;

    /** Parent frame. */
    private RenderViewBase parentScene;

    /** If any of the mouse move button pressed. */
    private boolean pressed;

    /** Radio button of the X_AXIS mode option. */
    private JRadioButton radioX;

    /** Radio button of the Y_AXIS mode option. */
    private JRadioButton radioY;

    /** Radio button of the Z_AXIS mode option. */
    private JRadioButton radioZ;

    /** Reset button. */
    private JButton resetButton;

    /** Right panel down button. */
    private JButton rightDownButton;

    /** Right panel left button. */
    private JButton rightLeftButton;

    /** Right panel right button. */
    private JButton rightRightButton;

    /** Right panel up button. */
    private JButton rightUpButton;

    /** Scroll pane. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;

    /** Camera button. */
    private JButton snapButton;

    /** Start button to auto snapshooting. */
    private JButton startButton;

    /** Auto snapshot button. */
    private JRadioButton stdButton;

    /** Time to wait for the next mouse event. */
    private long time = 100;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for converting type of image.
     *
     * @param  theParentFrame  Parent frame.
     */
    public JPanelCamera(RenderViewBase theParentFrame) {
        super(theParentFrame);
        parentScene = theParentFrame;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets the variables.
     *
     * @param  event  Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Start")) {
            setDegree(true);
            itemStateChanged(null);

            if (mode == STD_MODE) {
                parentScene.autoCapture();
            }
        } else if (command.equals("Reset")) {
            parentScene.resetImage();
        } else if (command.equals("ResetX")) {
            parentScene.resetAxisY();
        } else if (command.equals("ResetY")) {
            parentScene.resetAxisX();
        } else if (command.equals("ResetZ")) {
            parentScene.resetImage();
        } else if (command.equals("Stop")) { }
        else if (command.equals("Close")) {

            if (parentScene != null) {
                parentScene.disableCamera();
            }

            disposeLocal(true);
        } else if (command.equals("Capture")) {
            parentScene.writeImage();
        } else if (command.equals("Fly")) {
            startButton.setEnabled(false);
            mode = FLY_MODE;
            setFlyEnabled(true);
            setStdEnabled(false);
        } else if (command.equals("Standard")) {
            startButton.setEnabled(true);
            mode = STD_MODE;
            setFlyEnabled(false);
            setStdEnabled(true);
        } else if (command.equals("Help")) { // MipavUtil.showHelp("");
        }
    } // end actionPerformed()

    /**
     * Clear memory.
     *
     * @param  flag  if true then super.disposeLocal() is called
     */
    public void disposeLocal(boolean flag) {
        parentScene = null;

        radioX = null;
        radioY = null;
        radioZ = null;
        degreeLabel = null;
        degreeText = null;

        leftUpButton = null;
        leftDownButton = null;
        leftLeftButton = null;
        leftRightButton = null;
        rightUpButton = null;
        rightDownButton = null;
        rightLeftButton = null;
        rightRightButton = null;
        middleUpButton = null;
        middleDownButton = null;
        middleLeftButton = null;
        middleRightButton = null;

        leftUpButtonEmpty = null;
        leftDownButtonEmpty = null;
        middleRightButtonEmpty = null;
        middleLeftButtonEmpty = null;
        flyButton = null;
        stdButton = null;
        mousePanel = null;
        startButton = null;
        resetButton = null;
        labelX = null;
        labelY = null;
        labelZ = null;
        snapButton = null;

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     *
     * @return  JPanel the main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Sets the flags for the checkboxes and resets labels.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {

        if (radioX.isSelected()) {
            parentScene.setRotationAxis(ViewJFrameRenderCamera.X_AXIS);
        } else if (radioY.isSelected()) {
            parentScene.setRotationAxis(ViewJFrameRenderCamera.Y_AXIS);
        } else if (radioZ.isSelected()) {
            parentScene.setRotationAxis(ViewJFrameRenderCamera.Z_AXIS);
        }
    }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseClicked(MouseEvent event) { }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mousePressed(MouseEvent event) {
        setDegree(true);
        setIcon(event.getSource(), true);

        if (mode == FLY_MODE) {
            pressed = true;

            StandardMouse mouse = new StandardMouse(event);

            mouse.start();
        }
    }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseReleased(MouseEvent event) {
        setIcon(event.getSource(), false);

        if (mode == FLY_MODE) {
            pressed = false;
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   panel width.
     * @param  frameHeight  parent frame height.
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - buttonPanel.getHeight()));
        scroller.setSize(new Dimension(panelWidth, frameHeight - buttonPanel.getHeight()));
        scroller.revalidate();

    }


    /**
     * Set the image rotation angle.
     *
     * @param  positive  rotate counter clock wise or not.
     */
    public void setDegree(boolean positive) {
        String tmpStr;
        int degree = 1;

        tmpStr = degreeText.getText();

        if (testParameter(tmpStr, 0, 360)) {
            degree = Integer.valueOf(tmpStr).intValue();
        } else {
            degreeText.requestFocus();
            degreeText.selectAll();
        }

        if (positive) {
            parentScene.setRotationAngle(degree);
        } else {
            parentScene.setRotationAngle(-degree);
        }

        tmpStr = null;
    }

    /**
     * finalize - calls dispose.
     *
     * @throws  Throwable  Call disposeLocal to free memory.
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

    /**
     * Tests that the entered parameter is in range.
     *
     * @param   str       The value entered by the user.
     * @param   minValue  The minimum value this variable may be set to.
     * @param   maxValue  The maximum value this variable may be set to.
     *
     * @return  <code>true</code> if parameters passed range test, <code>false</code> if failed.
     */
    protected boolean testParameter(String str, double minValue, double maxValue) {
        double tmp;

        try {
            tmp = Double.valueOf(str).doubleValue();

            if ((tmp > maxValue) || (tmp < minValue)) {
                MipavUtil.displayError("Value is out of range: " + String.valueOf(minValue) + " , " +
                                       String.valueOf(maxValue));

                return false;
            } else {
                return true;
            }
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");

            return false;
        }
    }

    /**
     * Creates the mouse control panels. There are four arrows for each mouse button, left, right, and middle.
     *
     * @return  The panel containing the mouse controls.
     */
    private JPanel buildMouseControlPanel() {
        leftUpButton = new LeftButton("Left mouse button up");
        leftDownButton = new RightButton("Left mouse button down");
        leftRightButton = new RightButton("Left mouse button right");
        leftLeftButton = new LeftButton("Left mouse button left");
        leftUpButtonEmpty = new EmptyButton("");
        leftDownButtonEmpty = new EmptyButton("");

        middleUpButton = new UpButton("Middle mouse button up");
        middleDownButton = new DownButton("Middle mouse button down");
        middleRightButton = new RightButton("Middle mouse button right");
        middleLeftButton = new LeftButton("Middle mouse button left");
        middleRightButtonEmpty = new EmptyButton("");
        middleLeftButtonEmpty = new EmptyButton("");

        JPanel leftMousePanel = new JPanel();

        leftMousePanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        leftMousePanel.add(leftUpButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        leftMousePanel.add(leftUpButtonEmpty, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        labelY = new JLabel("X");
        leftMousePanel.add(labelY, gbc);
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        leftMousePanel.add(leftDownButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        leftMousePanel.add(leftLeftButton, gbc);
        leftUpButtonEmpty.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        labelX = new JLabel("Y");
        leftMousePanel.add(labelX, gbc);
        gbc.gridx = 3;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        leftMousePanel.add(leftRightButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        leftMousePanel.add(middleLeftButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        leftMousePanel.add(leftDownButtonEmpty, gbc);
        leftDownButtonEmpty.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        labelZ = new JLabel("Z");
        leftMousePanel.add(labelZ, gbc);
        gbc.gridx = 3;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        leftMousePanel.add(middleRightButton, gbc);
        leftMousePanel.setBorder(buildTitledBorder("Rotation"));

        JPanel middleMousePanel = new JPanel();

        middleMousePanel.setLayout(new GridBagLayout());
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        middleMousePanel.add(middleUpButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        middleMousePanel.add(middleLeftButtonEmpty, gbc);
        middleLeftButtonEmpty.setEnabled(false);
        gbc.gridx = 2;
        middleMousePanel.add(middleRightButtonEmpty, gbc);
        middleRightButtonEmpty.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 2;
        middleMousePanel.add(middleDownButton, gbc);
        middleMousePanel.setBorder(buildTitledBorder("Scale"));

        rightUpButton = new UpButton("Right mouse button up");
        rightDownButton = new DownButton("Right mouse button down");
        rightRightButton = new RightButton("Right mouse button right");
        rightLeftButton = new LeftButton("Right mouse button left");

        JPanel rightMousePanel = new JPanel();

        rightMousePanel.setLayout(new GridBagLayout());
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        rightMousePanel.add(rightUpButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        rightMousePanel.add(rightLeftButton, gbc);
        gbc.gridx = 2;
        rightMousePanel.add(rightRightButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        rightMousePanel.add(rightDownButton, gbc);
        rightMousePanel.setBorder(buildTitledBorder("Translation"));

        mousePanel = new JPanel();
        mousePanel.setLayout(new BoxLayout(mousePanel, BoxLayout.X_AXIS));
        mousePanel.add(leftMousePanel);
        mousePanel.add(middleMousePanel);
        mousePanel.add(rightMousePanel);
        mousePanel.setBorder(buildTitledBorder("Mouse button controls"));

        leftMousePanel = null;
        middleMousePanel = null;
        rightMousePanel = null;

        return mousePanel;
    }

    /**
     * Build the reset button.
     *
     * @return  resetButton reset button.
     */
    private JButton buildResetButton() {
        resetButton = new JButton("Reset");
        resetButton.addActionListener(this);
        resetButton.setToolTipText("Reset volume image");
        resetButton.setMinimumSize(MipavUtil.defaultButtonSize);
        resetButton.setPreferredSize(MipavUtil.defaultButtonSize);
        resetButton.setFont(serif12B);

        return resetButton;
    }

    /**
     * Builds the cancel button. Sets it internally as well return the just-built button.
     *
     * @return  Return the auto snapshot button. 
     */
    private JButton buildStartButton() {
        startButton = new JButton("Start");
        startButton.addActionListener(this);
        startButton.setToolTipText("Start Recording");
        startButton.setMinimumSize(MipavUtil.defaultButtonSize);
        startButton.setPreferredSize(MipavUtil.defaultButtonSize);
        startButton.setFont(serif12B);

        return startButton;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());

        // Put the drawing area in a scroll pane.
        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new GridBagLayout());

        JPanel panelRotationAxis = new JPanel(new GridBagLayout());

        panelRotationAxis.setForeground(Color.black);
        panelRotationAxis.setBorder(buildTitledBorder("Rotation Axis"));

        ButtonGroup group1 = new ButtonGroup();

        radioX = new JRadioButton("X", false);
        radioX.setFont(serif12);
        group1.add(radioX);

        radioY = new JRadioButton("Y", false);
        radioY.setFont(serif12);
        group1.add(radioY);

        radioZ = new JRadioButton("Z", false);
        radioZ.setFont(serif12);
        group1.add(radioZ);

        radioX.setSelected(false);
        radioY.setSelected(true);
        radioZ.setSelected(false);

        radioX.addItemListener(this);
        radioY.addItemListener(this);
        radioZ.addItemListener(this);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);

        panelRotationAxis.add(radioX, gbc);

        gbc.gridx = 1;
        panelRotationAxis.add(radioY, gbc);

        gbc.gridx = 2;
        panelRotationAxis.add(radioZ, gbc);

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);

        JPanel degreePanel = new JPanel(new GridBagLayout());

        degreePanel.setBorder(buildTitledBorder("Rotation Alpha"));
        degreeLabel = new JLabel("Degree ");

        int degreeValue = 15;

        degreeText = new JTextField(Integer.toString(degreeValue));
        degreePanel.add(degreeLabel, gbc);
        gbc.gridx = 1;
        degreePanel.add(degreeText, gbc);

        flyButton = new JRadioButton("Manual");
        flyButton.addActionListener(this);
        flyButton.setActionCommand("Fly");
        flyButton.setFont(serif12B);
        flyButton.setForeground(Color.black);

        stdButton = new JRadioButton("Auto");
        stdButton.addActionListener(this);
        stdButton.setActionCommand("Standard");
        stdButton.setSelected(true);
        stdButton.setFont(serif12B);
        stdButton.setForeground(Color.black);

        ButtonGroup modeGroup = new ButtonGroup();

        modeGroup.add(flyButton);
        modeGroup.add(stdButton);

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);

        JPanel modePanel = new JPanel();

        modePanel.setLayout(new GridBagLayout());
        modePanel.add(stdButton, gbc);
        gbc.gridx = 1;
        modePanel.add(flyButton, gbc);
        modePanel.setBorder(buildTitledBorder("Mode"));

        snapButton = new JButton(MipavUtil.getIcon("camera.gif"));
        snapButton.addActionListener((ActionListener) this);
        snapButton.setToolTipText("Capture screen");
        snapButton.setActionCommand("Capture");
        snapButton.setBorderPainted(false);
        snapButton.setRolloverEnabled(true);
        snapButton.setRolloverIcon(MipavUtil.getIcon("cameraroll.gif"));
        snapButton.setFocusPainted(false);
        snapButton.setEnabled(true);

        JPanel snapButtonPanel = new JPanel();

        snapButtonPanel.setLayout(new GridBagLayout());
        snapButtonPanel.setBorder(buildTitledBorder("Capture"));
        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        snapButtonPanel.add(snapButton, gbc);

        Box topBox = new Box(BoxLayout.Y_AXIS);

        topBox.add(modePanel);
        topBox.add(snapButtonPanel);

        buttonPanel = new JPanel();

        buildStartButton();
        buildResetButton();

        buildMouseControlPanel();
        buttonPanel.add(startButton);
        buttonPanel.add(resetButton);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        contentBox.add(topBox);
        contentBox.add(panelRotationAxis);
        contentBox.add(degreePanel);
        contentBox.add(mousePanel);
        contentBox.add(buttonPanel);

        scrollPanel.add(contentBox, BorderLayout.NORTH);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(scroller, gbc);

        setStdEnabled(true);
        setFlyEnabled(false);

        panelRotationAxis = null;
        group1 = null;
        gbc = null;
        degreePanel = null;
        modeGroup = null;
        modePanel = null;
        topBox = null;
        contentBox = null;

    }

    /**
     * Sets the button properties for all the arrow buttons.
     *
     * @param  button  Button whose properties need to be set.
     * @param  tip     Tool tip text to associate with this button.
     */
    private void setButtonProps(JButton button, String tip) {
        button.setRolloverEnabled(true);
        button.setMargin(new Insets(0, 0, 0, 0));
        button.setToolTipText(tip);
        button.setBorderPainted(false);
        button.setFocusPainted(false);
        button.setContentAreaFilled(false);
        button.addMouseListener(this);
    }

    /**
     * Enable manual snapshot buttons.
     *
     * @param  flag  <code>true</code> means turn on, <code>false</code> means turn off.
     */
    private void setFlyEnabled(boolean flag) {
        labelX.setEnabled(flag);
        labelY.setEnabled(flag);
        labelZ.setEnabled(flag);
        leftUpButton.setEnabled(flag);
        leftDownButton.setEnabled(flag);
        leftLeftButton.setEnabled(flag);
        leftRightButton.setEnabled(flag);
        rightUpButton.setEnabled(flag);
        rightDownButton.setEnabled(flag);
        rightLeftButton.setEnabled(flag);
        rightRightButton.setEnabled(flag);
        middleUpButton.setEnabled(flag);
        middleDownButton.setEnabled(flag);
        middleLeftButton.setEnabled(flag);
        middleRightButton.setEnabled(flag);
        snapButton.setEnabled(flag);
    }

    /**
     * Sets the pressed or unpressed icon for the button, depending on which type it is.
     *
     * @param  source  The button that was pressed or released.
     * @param  press   <code>true</code> indicates the button was pressed, <code>false</code> that it was released.
     */
    private void setIcon(Object source, boolean press) {

        if (press) {

            if (source instanceof UpButton) {
                ((UpButton) source).setIcon(MipavUtil.getIcon("uppress.gif"));
            } else if (source instanceof DownButton) {
                ((DownButton) source).setIcon(MipavUtil.getIcon("downpress.gif"));
            } else if (source instanceof RightButton) {
                ((RightButton) source).setIcon(MipavUtil.getIcon("rightarrowpress.gif"));
            } else if (source instanceof LeftButton) {
                ((LeftButton) source).setIcon(MipavUtil.getIcon("leftarrowpress.gif"));
            }

            ((JButton) source).setRolloverEnabled(false);
        } else {
            ((JButton) source).setRolloverEnabled(true);

            if (source instanceof UpButton) {
                ((UpButton) source).setIcon(MipavUtil.getIcon("up.gif"));
            } else if (source instanceof DownButton) {
                ((DownButton) source).setIcon(MipavUtil.getIcon("down.gif"));
            } else if (source instanceof RightButton) {
                ((RightButton) source).setIcon(MipavUtil.getIcon("rightarrow.gif"));
            } else if (source instanceof LeftButton) {
                ((LeftButton) source).setIcon(MipavUtil.getIcon("leftarrow.gif"));
            }
        }
    }

    /**
     * Enable auto snapshot buttons.
     *
     * @param  flag  <code>true</code> means turn on, <code>false</code> means turn off.
     */
    private void setStdEnabled(boolean flag) {
        radioX.setEnabled(flag);
        radioY.setEnabled(flag);
        radioZ.setEnabled(flag);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Helper class which sets the appropriate icons for the down button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class DownButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -6362044693437810740L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public DownButton(String tip) {
            super(MipavUtil.getIcon("down.gif"));
            setRolloverIcon(MipavUtil.getIcon("downroll.gif"));
            setButtonProps(this, tip);
        }
    }


    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 9172347561422650241L;

        /**
         * Wrapper to call the paintComponent.
         *
         * @param  g  Graphics reference.
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);

        }
    }


    /**
     * Helper class which create the empty button.
     */
    class EmptyButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -1884920826206278066L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public EmptyButton(String tip) {
            super(MipavUtil.getIcon("emptycursor.gif"));
            setRolloverIcon(MipavUtil.getIcon("emptycursor.gif"));
            setButtonProps(this, tip);
        }
    }


    /**
     * Helper class which sets the appropriate icons for the left button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class LeftButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 6199451328969923668L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public LeftButton(String tip) {
            super(MipavUtil.getIcon("leftarrow.gif"));
            setRolloverIcon(MipavUtil.getIcon("leftarrowroll.gif"));
            setButtonProps(this, tip);
        }
    }


    /**
     * Helper class which sets the appropriate icons for the right button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class RightButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -7976337846027116179L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public RightButton(String tip) {
            super(MipavUtil.getIcon("rightarrow.gif"));
            setRolloverIcon(MipavUtil.getIcon("rightarrowroll.gif"));
            setButtonProps(this, tip);
        }
    }

    /**
     * Class used to send Standard mouse events to the canvas. Must subclass Thread because a single <code>
     * mousePressed</code> event on one of the mouse buttons needs to generate <code>mouseDragged</code> events on the
     * canvas until the mouse is released.
     */
    class StandardMouse extends Thread {

        /** Canvas center coordinate. */
        int centerX, centerY;

        /** Mouse event */
        MouseEvent evt;

        /** Event source */
        Object source;

        /** Event time stamp. */
        long when;

        /** Mouseevent mask */
        int mod;
        
        /** Canvas center x and y. */
        int x, y;
        
        /** MouseEvent id. */
        int id;

        /**
         * Creates new thread and sets up mouse event variables appropriately.
         *
         * @param  event  Original mouse event, from button.
         */
        public StandardMouse(MouseEvent event) {
            when = event.getWhen();
            x = parentScene.getCanvas().getBounds().width / 2;
            centerX = parentScene.getCanvas().getBounds().width / 2;
            y = parentScene.getCanvas().getBounds().height / 2;
            centerY = parentScene.getCanvas().getBounds().height / 2;
            id = MouseEvent.MOUSE_DRAGGED;
            source = event.getSource();

            if ((source == leftUpButton) || (source == leftDownButton) || (source == leftLeftButton) ||
                    (source == leftRightButton)) {

                // left mouse button plus whatever extra modifiers were on original event (alt mask, shift mask, etc)
                mod = MouseEvent.BUTTON1_MASK + (event.getModifiers() - MouseEvent.BUTTON1_MASK);
            } else if ((source == middleUpButton) || (source == middleDownButton) || (source == middleLeftButton) ||
                           (source == middleRightButton)) {

                // middle mouse button plus whatever extra modifiers were on original event (alt mask, shift mask, etc)
                mod = MouseEvent.BUTTON2_MASK + (event.getModifiers() - MouseEvent.BUTTON1_MASK);
            } else {
                mod = MouseEvent.BUTTON3_MASK;
            }

            evt = new MouseEvent(parentScene.getCanvas(), MouseEvent.MOUSE_PRESSED, when, mod, Math.round(x),
                                 Math.round(y), 1, false);
        }

        /**
         * Runs the thread. While the button is pressed, dispatches mouse dragged events at a rate consistent with the
         * velocity slider. Once the mouse is released, <code>pressed</code> will be set to false and the loop will
         * stop.
         */
        public synchronized void run() {

            while (pressed) {
                parentScene.getCanvas().dispatchEvent(evt);

                if ((source == leftUpButtonEmpty) || (source == leftDownButtonEmpty) ||
                        (source == middleRightButtonEmpty) || (source == middleLeftButtonEmpty)) {
                    return;
                } else if (source == leftRightButton) {
                    setDegree(false);
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.Y_AXIS);
                } else if (source == leftLeftButton) {
                    setDegree(true);
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.Y_AXIS);
                } else if (source == leftUpButton) {
                    setDegree(true);
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.X_AXIS);
                } else if (source == leftDownButton) {
                    setDegree(false);
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.X_AXIS);
                } else if (source == middleLeftButton) {
                    setDegree(true);
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.Z_AXIS);
                } else if (source == middleRightButton) {
                    setDegree(false);
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.Z_AXIS);
                } else if (source == middleUpButton) {
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.NO_AXIS);
                    y++;
                } else if (source == middleDownButton) {
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.NO_AXIS);
                    y--;
                } else if (source == rightUpButton) {
                    y--;
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.NO_AXIS);
                } else if (source == rightDownButton) {
                    y++;
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.NO_AXIS);
                } else if (source == rightLeftButton) {
                    x--;
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.NO_AXIS);
                } else if (source == rightRightButton) {
                    x++;
                    parentScene.setRotationAxis(ViewJFrameRenderCamera.NO_AXIS);
                }

                when += time;

                try {
                    parentScene.rotateImage();
                    wait(time);
                } catch (InterruptedException error) { }

                evt = new MouseEvent(parentScene.getCanvas(), id, when, mod, Math.round(x), Math.round(y), 0, false);

            }

        }
    }


    /**
     * Helper class which sets the appropriate icons for the up button. Will use "instanceof" keyword to determine icons
     * later.
     */
    class UpButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -4142376788561359230L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public UpButton(String tip) {
            super(MipavUtil.getIcon("up.gif"));
            setRolloverIcon(MipavUtil.getIcon("uproll.gif"));
            setButtonProps(this, tip);
        }
    }

}
