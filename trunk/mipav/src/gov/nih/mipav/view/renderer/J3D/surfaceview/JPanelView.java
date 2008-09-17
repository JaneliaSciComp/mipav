package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;

import com.sun.j3d.utils.behaviors.vp.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog to control the view of the 3D surface viewer and renderer. User can switch between standard view and fly mode.
 * Standard view has rotating with standard mouse motions and zooming with the mouse and the alt key down. Fly mode has
 * zooming with the left mouse button, translation with the right mouse button, and pitch and roll with the middle mouse
 * button. There are also buttons on screen for Mac users. The user can warp to different views using the warp controls
 * and record the flight path using the mouse event recorder.
 *
 * @author  Neva Cherniavsky
 */
public class JPanelView extends JPanelRendererJ3D implements ChangeListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8408530937731996131L;

    /** Stardard viewing mode. */
    public static final int STD_MODE = 0;

    /** Fly through viewing mode. */
    public static final int FLY_MODE = 1;

    /** Pointer viewing mode. */
    public static final int PTR_MODE = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Fly behavior reference. */
    private FlyBehaviorRenderer flyBehavior;

    /** Fly behaviro radio button. */
    private JRadioButton flyButton;

    /** User message explain how the mouse moves in fly mode. */
    private String flyText = "Use the left mouse button for zooming and rotation.\nUse the right mouse button for translation.\nUse the middle mouse button for pitch and roll rotation.";

    /** speed control slider labels. */
    private JLabel[] labels;

    /** DOCUMENT ME! */
    private JButton leftDownButton;

    /** DOCUMENT ME! */
    private JButton leftLeftButton;

    /** DOCUMENT ME! */
    private JButton leftRightButton;

    /** Arrow button references. */
    private JButton leftUpButton;

    /** DOCUMENT ME! */
    private JButton middleDownButton;

    /** DOCUMENT ME! */
    private JButton middleLeftButton;

    /** DOCUMENT ME! */
    private JButton middleRightButton;

    /** DOCUMENT ME! */
    private JButton middleUpButton;

    /** Current behavior mode. */
    private int mode;

    /** Radio button group reference. */
    private ButtonGroup modeGroup;

    /** Text field reference that hold the user message. */
    private JTextArea mouseText;

    /** Pointer behavior reference, not used now. */
    private PointerBehaviorRenderer pointerBehavior;

    /** Flag to indicate mouse pressed or not. */
    private boolean pressed;

    /** Point behavior button. */
    private JRadioButton ptrButton;

    /** DOCUMENT ME! */
    private JButton rightDownButton;

    /** DOCUMENT ME! */
    private JButton rightLeftButton;

    /** DOCUMENT ME! */
    private JButton rightRightButton;

    /** DOCUMENT ME! */
    private JButton rightUpButton;

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;

    /** Standard behavior reference. */
    private ViewPlatformBehavior stdBehavior;

    /** Standard behavior button. */
    private JRadioButton stdButton;

    /** User message explain how the mouse moves in stardard mode. */
    private String stdText = "Use the left mouse button for rotation.\nUse the ALT key and the left mouse button for zooming.\nYou may also use the middle mouse button for zooming.";

    /** Standard mouse tread waiting time. */
    private long time = 50;

    /** fly Speed control slider. */
    private JSlider velocitySlider;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new dialog for controlling the view of the renderer.
     *
     * @param  parent  The parent frame that contains the image scene information.
     */
    public JPanelView(RenderViewBase parent) {
        super(parent);
        stdBehavior = renderBase.getUniverse().getViewingPlatform().getViewPlatformBehavior();
        flyBehavior = new FlyBehaviorRenderer(this);
        flyBehavior.setTarget(renderBase.getSceneRootTG());
        flyBehavior.setSchedulingBounds(renderBase.bounds);
        flyBehavior.setupCallback(parent);
        pointerBehavior = new PointerBehaviorRenderer(this);
        pointerBehavior.setTarget(renderBase.getSceneRootTG());
        pointerBehavior.setSchedulingBounds(renderBase.bounds);
        pointerBehavior.setupCallback(parent);
        mode = STD_MODE;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Executes the following actions depending on what called this method:<br>
     *
     * <ul>
     *   <li>Fly - changes the view platform to fly mode, so that the left mouse button is zoom, the right mouse button
     *     is translate, and the middle mouse button is pitch and roll.</li>
     *   <li>Standard - changes the view platform back to the standard, with the left mouse button grabbing the object
     *     and rotating it and the left mouse button with the alt mask doing a zoom.</li>
     *   <li>Reset - resets the view to what it was when the user called up this dialog.</li>
     * </ul>
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("Fly") && (mode != FLY_MODE)) {
            renderBase.getUniverse().getViewingPlatform().setViewPlatformBehavior(flyBehavior);

            for (int i = 0; i < renderBase.getSceneRootTG().numChildren(); i++) {
                Object elem = renderBase.getSceneRootTG().getChild(i);

                if (elem == renderBase.getBehaviorGroup()) {
                    renderBase.getSceneRootTG().removeChild(i);

                    break;
                }
            }

            mouseText.setText(flyText);
            setFlyEnabled(!flyBehavior.isIgnoreMouseMotion());
            mode = FLY_MODE;
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER) && (mode != PTR_MODE)) {
            renderBase.getUniverse().getViewingPlatform().setViewPlatformBehavior(pointerBehavior);

            for (int i = 0; i < renderBase.getSceneRootTG().numChildren(); i++) {
                Object elem = renderBase.getSceneRootTG().getChild(i);

                if (elem == renderBase.getBehaviorGroup()) {
                    renderBase.getSceneRootTG().removeChild(i);

                    break;
                }
            }

            mouseText.setText(flyText);
            setFlyEnabled(!pointerBehavior.isIgnoreMouseMotion());
            mode = PTR_MODE;
        } else if (command.equals("Standard") && (mode != STD_MODE)) {
            renderBase.getUniverse().getViewingPlatform().setViewPlatformBehavior(stdBehavior);
            mouseText.setText(stdText);
            setFlyEnabled(false);
            mode = STD_MODE;
        }
    }

    /**
     * Clear memory.
     *
     * @param  flag  if true then super.disposeLocal() is called
     */
    public void disposeLocal(boolean flag) {
        stdBehavior = null;
        flyBehavior = null;
        pointerBehavior = null;
        flyButton = null;
        stdButton = null;
        ptrButton = null;
        velocitySlider = null;
        labels = null;
        modeGroup = null;
        stdText = null;
        flyText = null;
        mouseText = null;
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

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Returns the contentPane of this dialog.
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Accessor that returns the current mouse mode of the dialog.
     *
     * @return  Standard mode or fly mode.
     */
    public int getMouseMode() {
        return mode;
    }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseClicked(MouseEvent event) {

        if (mode == FLY_MODE) {
            renderBase.getCanvas().dispatchEvent(translateEvent(event));
        }
    }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseEntered(MouseEvent event) {

        if (mode == FLY_MODE) {
            renderBase.getCanvas().dispatchEvent(translateEvent(event));
        }
    }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseExited(MouseEvent event) {

        if (mode == FLY_MODE) {
            renderBase.getCanvas().dispatchEvent(translateEvent(event));
        }
    }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mousePressed(MouseEvent event) {
        setIcon(event.getSource(), true);

        if (mode == FLY_MODE) {
            renderBase.getCanvas().dispatchEvent(translateEvent(event));
        } else {
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
            renderBase.getCanvas().dispatchEvent(translateEvent(event));
        } else {
            pressed = false;
        }
    }

    /**
     * Resize the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   int width
     * @param  frameHeight  int height
     */
    public void resizePanel(int panelWidth, int frameHeight) {

        ///
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.setSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.revalidate();
    }

    /**
     * Sets the elements associated with "fly" behavior to be enabled or disabled, depending on <code>flag</code>.
     *
     * @param  flag  <code>true</code> indicates that these elements should be enabled, <code>false</code> that they
     *               should be disabled.
     */
    public void setFlyEnabled(boolean flag) {
        rightUpButton.setEnabled(flag);
        rightDownButton.setEnabled(flag);
        rightRightButton.setEnabled(flag);
        rightLeftButton.setEnabled(flag);
        middleLeftButton.setEnabled(flag);
        middleRightButton.setEnabled(flag);
    }

    /**
     * Accessor that sets the current mouse mode of the dialog.
     *
     * @param  mod  Standard mode or fly mode.
     */
    public void setMouseMode(int mod) {

        if (mode != mod) {

            if (mod == STD_MODE) {
                actionPerformed(new ActionEvent(this, 1, "Standard"));
            } else if (mod == FLY_MODE) {
                actionPerformed(new ActionEvent(this, 1, "Fly"));
            }
        }
    }

    /**
     * Listens for changes in the velocity slider. If the user changes the velocity, it is changed within the parent
     * class of FlyBehavior.
     *
     * @param  event  Event that triggered this method.
     */
    public void stateChanged(ChangeEvent event) {

        if (event.getSource() == velocitySlider) {
            int value = velocitySlider.getValue();

            time = 100 - value;

            float velocity = 2 * value / 1000.0f;
            float angle = 2 * value / 100.0f;
            float velocity2 = 2 * value / 100.0f;
            float angle2 = 2 * value;

            angle = (float) Math.toRadians(angle);
            angle2 = (float) Math.toRadians(angle2);
            flyBehavior.setMaximumVelocity(velocity);
            flyBehavior.setMaximumAngle(angle);
            pointerBehavior.setMaximumVelocity(velocity2);
            pointerBehavior.setMaximumAngle(angle2);
        }
    }

    /**
     * Calls finalize.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        this.disposeLocal(false);
        super.finalize();
    }

    /**
     * Creates the mouse control panels. There are four arrows for each mouse button, left, right, and middle.
     *
     * @return  The panel containing the mouse controls.
     */
    private JPanel createMouseControlPanel() {
        leftUpButton = new UpButton("Left mouse button up");
        leftDownButton = new DownButton("Left mouse button down");
        leftRightButton = new RightButton("Left mouse button right");
        leftLeftButton = new LeftButton("Left mouse button left");

        JPanel leftMousePanel = new JPanel();

        leftMousePanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        leftMousePanel.add(leftUpButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        leftMousePanel.add(leftLeftButton, gbc);
        gbc.gridx = 2;
        leftMousePanel.add(leftRightButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        leftMousePanel.add(leftDownButton, gbc);
        leftMousePanel.setBorder(buildTitledBorder("Left"));

        middleUpButton = new UpButton("Middle mouse button up");
        middleDownButton = new DownButton("Middle mouse button down");
        middleRightButton = new RightButton("Middle mouse button right");
        middleLeftButton = new LeftButton("Middle mouse button left");

        JPanel middleMousePanel = new JPanel();

        middleMousePanel.setLayout(new GridBagLayout());
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        middleMousePanel.add(middleUpButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        middleMousePanel.add(middleLeftButton, gbc);
        gbc.gridx = 2;
        middleMousePanel.add(middleRightButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        middleMousePanel.add(middleDownButton, gbc);
        middleMousePanel.setBorder(buildTitledBorder("Middle"));

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
        rightMousePanel.setBorder(buildTitledBorder("Right"));

        JPanel panel = new JPanel();

        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        panel.add(leftMousePanel);
        panel.add(middleMousePanel);
        panel.add(rightMousePanel);
        panel.setBorder(buildTitledBorder("Mouse button controls"));
        setFlyEnabled(false);

        return panel;
    }

    /**
     * Initializes the GUI components.
     */
    private void init() {
        flyButton = new JRadioButton("Fly");
        flyButton.addActionListener(this);
        flyButton.setActionCommand("Fly");
        flyButton.setFont(serif12B);
        flyButton.setForeground(Color.black);

        stdButton = new JRadioButton("Standard");
        stdButton.addActionListener(this);
        stdButton.setActionCommand("Standard");
        stdButton.setSelected(true);
        stdButton.setFont(serif12B);
        stdButton.setForeground(Color.black);

        ptrButton = new JRadioButton("Pointer");
        ptrButton.addActionListener(this);
        ptrButton.setActionCommand("Pointer");
        ptrButton.setSelected(false);
        ptrButton.setFont(serif12B);
        ptrButton.setForeground(Color.black);

        modeGroup = new ButtonGroup();
        modeGroup.add(flyButton);
        modeGroup.add(stdButton);
        modeGroup.add(ptrButton);

        labels = new JLabel[2];
        labels[0] = new JLabel("Slow");
        labels[0].setFont(serif12);
        labels[0].setForeground(Color.black);
        labels[1] = new JLabel("Fast");
        labels[1].setFont(serif12);
        labels[1].setForeground(Color.black);

        Hashtable table = new Hashtable();

        table.put(new Integer(0), labels[0]);
        table.put(new Integer(100), labels[1]);

        velocitySlider = new JSlider(0, 100, 50);
        velocitySlider.setFont(serif12);
        velocitySlider.addChangeListener(this);
        velocitySlider.setLabelTable(table);
        velocitySlider.setPaintLabels(true);
        velocitySlider.setMinorTickSpacing(10);
        velocitySlider.setPaintTicks(true);

        JPanel mouseCS = new JPanel();

        mouseCS.setLayout(new BorderLayout());

        mouseText = new JTextArea(3, 5);
        mouseText.setText(stdText);
        mouseText.setEditable(false);
        mouseText.setFont(serif12);
        mouseText.setBackground(mouseCS.getBackground());
        mouseText.setForeground(Color.black);

        mouseCS.add(mouseText, BorderLayout.CENTER);
        mouseCS.setBorder(buildTitledBorder("Mouse movements"));

        JPanel modePanel = new JPanel();
        modePanel.setLayout(new BoxLayout(modePanel, BoxLayout.Y_AXIS));
        modePanel.add(stdButton);
        modePanel.add(flyButton);
        modePanel.add(ptrButton);
        modePanel.add(velocitySlider);
        modePanel.setBorder(buildTitledBorder("Mode"));

        JPanel leftPanel = new JPanel();
        leftPanel.setLayout(new BorderLayout());
        leftPanel.add(modePanel);
        leftPanel.add(mouseCS, BorderLayout.SOUTH);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        contentBox.add(leftPanel);
        contentBox.add(createMouseControlPanel());

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(contentBox, BorderLayout.NORTH);

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(scroller, BorderLayout.NORTH);
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
     * Translates the mouse event received from the mouse control buttons to its equivalent on the canvas.
     *
     * @param   event  Original mouse event.
     *
     * @return  Translated mouse event, with new source, x, y, and button mask.
     */
    private MouseEvent translateEvent(MouseEvent event) {
        int mod = MouseEvent.BUTTON1_MASK;
        int id = event.getID();
        float x = flyBehavior.getCanvasCenter().x;
        float y = flyBehavior.getCanvasCenter().y;

        if (event.getSource() == leftUpButton) {
            y = flyBehavior.getCanvasCenter().y - (flyBehavior.getCanvasCenter().y / 2);
            mod = MouseEvent.BUTTON1_MASK;
        } else if (event.getSource() == leftDownButton) {
            y = flyBehavior.getCanvasCenter().y + (flyBehavior.getCanvasCenter().y / 2);
            mod = MouseEvent.BUTTON1_MASK;
        } else if (event.getSource() == leftLeftButton) {
            x = flyBehavior.getCanvasCenter().x - (flyBehavior.getCanvasCenter().x / 2);
            mod = MouseEvent.BUTTON1_MASK;
        } else if (event.getSource() == leftRightButton) {
            x = flyBehavior.getCanvasCenter().x + (flyBehavior.getCanvasCenter().x / 2);
            mod = MouseEvent.BUTTON1_MASK;
        } else if (event.getSource() == middleUpButton) {
            y = flyBehavior.getCanvasCenter().y - (flyBehavior.getCanvasCenter().y / 2);
            mod = MouseEvent.BUTTON2_MASK;
        } else if (event.getSource() == middleDownButton) {
            y = flyBehavior.getCanvasCenter().y + (flyBehavior.getCanvasCenter().y / 2);
            mod = MouseEvent.BUTTON2_MASK;
        } else if (event.getSource() == middleLeftButton) {
            x = flyBehavior.getCanvasCenter().x - (flyBehavior.getCanvasCenter().x / 2);
            mod = MouseEvent.BUTTON2_MASK;
        } else if (event.getSource() == middleRightButton) {
            x = flyBehavior.getCanvasCenter().x + (flyBehavior.getCanvasCenter().x / 2);
            mod = MouseEvent.BUTTON2_MASK;
        } else if (event.getSource() == rightUpButton) {
            y = flyBehavior.getCanvasCenter().y - (flyBehavior.getCanvasCenter().y / 2);
            mod = MouseEvent.BUTTON3_MASK;
        } else if (event.getSource() == rightDownButton) {
            y = flyBehavior.getCanvasCenter().y + (flyBehavior.getCanvasCenter().y / 2);
            mod = MouseEvent.BUTTON3_MASK;
        } else if (event.getSource() == rightLeftButton) {
            x = flyBehavior.getCanvasCenter().x - (flyBehavior.getCanvasCenter().x / 2);
            mod = MouseEvent.BUTTON3_MASK;
        } else if (event.getSource() == rightRightButton) {
            x = flyBehavior.getCanvasCenter().x + (flyBehavior.getCanvasCenter().x / 2);
            mod = MouseEvent.BUTTON3_MASK;
        }

        MouseEvent e = new MouseEvent(renderBase.getCanvas(), id, event.getWhen(), mod, Math.round(x), Math.round(y),
                                      event.getClickCount(), event.isPopupTrigger());

        return e;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Helper class which sets the appropriate icons for the down button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class DownButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 1312076585635122029L;

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
        private static final long serialVersionUID = -6090606637553715141L;

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
     * Helper class which sets the appropriate icons for the left button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class LeftButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -433999106553563410L;

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
        private static final long serialVersionUID = -7557770624944478468L;

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

        /** DOCUMENT ME! */
        MouseEvent evt;

        /** DOCUMENT ME! */
        Object source;

        /** DOCUMENT ME! */
        long when;

        /** DOCUMENT ME! */
        int x, y, mod, id;

        /**
         * Creates new thread and sets up mouse event variables appropriately.
         *
         * @param  event  Original mouse event, from button.
         */
        public StandardMouse(MouseEvent event) {
            when = event.getWhen();
            x = renderBase.getCanvas().getBounds().width / 2;
            y = renderBase.getCanvas().getBounds().height / 2;
            id = MouseEvent.MOUSE_DRAGGED;
            source = event.getSource();

            if ((source == leftUpButton) || (source == leftDownButton) || (source == leftLeftButton) ||
                    (source == leftRightButton)) {

                // left mouse button plus whatever extra modifiers were on original event (alt mask, shift mask, etc)
                mod = MouseEvent.BUTTON1_MASK + (event.getModifiers() - MouseEvent.BUTTON1_MASK);
            } else {

                // middle mouse button plus whatever extra modifiers were on original event (alt mask, shift mask, etc)
                mod = MouseEvent.BUTTON2_MASK + (event.getModifiers() - MouseEvent.BUTTON1_MASK);
            }

            evt = new MouseEvent(renderBase.getCanvas(), MouseEvent.MOUSE_PRESSED, when, mod, Math.round(x),
                                 Math.round(y), 1, false);
        }

        /**
         * Runs the thread. While the button is pressed, dispatches mouse dragged events at a rate consistent with the
         * velocity slider. Once the mouse is released, <code>pressed</code> will be set to false and the loop will
         * stop.
         */
        public synchronized void run() {

            while (pressed) {
                renderBase.getCanvas().dispatchEvent(evt);

                if ((source == leftUpButton) || (source == middleUpButton)) {
                    y--;
                } else if ((source == leftDownButton) || (source == middleDownButton)) {
                    y++;
                } else if ((source == leftLeftButton) || (source == middleLeftButton)) {
                    x--;
                } else if ((source == leftRightButton) || (source == middleRightButton)) {
                    x++;
                }

                when += time;

                try {
                    wait(time);
                } catch (InterruptedException error) { }

                evt = new MouseEvent(renderBase.getCanvas(), id, when, mod, Math.round(x), Math.round(y), 0, false);
            }

        }
    }


    /**
     * Helper class which sets the appropriate icons for the up button. Will use "instanceof" keyword to determine icons
     * later.
     */
    class UpButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 8048065207592874906L;

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
