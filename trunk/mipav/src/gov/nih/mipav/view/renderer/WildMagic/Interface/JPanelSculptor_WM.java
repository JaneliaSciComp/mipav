package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;

/**
 * <p>Title: JPanelSculptor</p>
 *
 * <p>Description: Sculptor panel draws the sculptor and apply the sculptor to the whole volume region.</p>
 *
 * @author  Alexandra Bokinsky, Ph.D.
 */
public class JPanelSculptor_WM extends JInterfaceBase
{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4235930260988710821L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int m_iSculptHeight = 0;

    /** Window information for Sculpting:. */
    int m_iSculptWidth = 0;

    /** Line shape button. */
    private JToggleButton lineButton;

    /** Button to apply sculpt region to the volume. */
    private JButton m_kApplySculptButton;

    /** Button for clearing the sculpt region. */
    private JButton m_kClearDrawOutlineButton;

    /** Toggle button is down while the mouse is used to draw the sculpt outline. */
    private JToggleButton m_kDrawOutlineButton;

    /** Button for inverting the sculpt region. */
    private JButton m_kInvertOutlineButton;

    /** Button to save the sculpt image. */
    private JButton m_kSaveSculptButton;

    /* Sculpturing interface */

    /** Button to undo the sculpt and restor the original volume. */
    private JButton m_kUndoSculptButton;

    /** Rectangle shape button. */
    private JToggleButton rectButton;

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;

    /** Toolbar builder reference. */
    private ViewToolBarBuilder toolbarBuilder;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Contructor. Called from the surface render to create the sculptor control panel.
     *
     * @param  parent  surface render
     */
    public JPanelSculptor_WM(VolumeTriPlanarInterface kVolumeViewer) {
        super(kVolumeViewer);
        init();
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Command processor to handle the geodesic button events.
     *
     * @param  e  ActionEvent
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("lineShape")) {
            lineButton.setSelected(true);
            lineButton.setBorderPainted(true);
            rectButton.setSelected(false);
            rectButton.setBorderPainted(false);
            setSculptShape(Sculptor_WM.LINES);
        } else if (command.equals("rectShape")) {
            lineButton.setSelected(false);
            lineButton.setBorderPainted(false);
            rectButton.setSelected(true);
            rectButton.setBorderPainted(true);
            setSculptShape(Sculptor_WM.RECTANGLE);
        } else if (command.equals("DrawSculptRegion")) {
            drawSculptRegion();
        } else if (command.equals("UndoDrawSculptRegion")) {
            clearSculptRegion();
        } else if (command.equals("InvertSculptRegion")) {
            invertSculptRegion();
        } else if (command.equals("ApplySculptRegion")) {
            applySculptRegion();
        } else if (command.equals("UndoApplySculptRegion")) {
            undoSculptRegion();
        } else if (command.equals("SaveSculptImage")) {
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.save(new FileWriteOptions(true), -1);
            }
        }
    }


    /**
     * Cull the sculpt region through the 3D volume.
     */
    public void applySculptRegion() {

        /* the m_kDrawOutlineButton is a toggle button, once apply is pressed,
         * un-toggle the draw button. */
        m_kDrawOutlineButton.setSelected(false);
        m_kDrawOutlineButton.setBorderPainted(false);
        m_kClearDrawOutlineButton.setEnabled(false);
        m_kInvertOutlineButton.setEnabled(false);
        m_kApplySculptButton.setEnabled(false);
        m_kUndoSculptButton.setEnabled(true);
        m_kSaveSculptButton.setEnabled(true);

        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.applySculpt();
        }

    }

    /**
     * clearwSculptRegion: called when the "Clear Outline" Button is pressed.
     */
    public void clearSculptRegion() {

        /* the m_kDrawOutlineButton is a toggle button, once clear is pressed,
         * un-toggle the draw button. */
        m_kDrawOutlineButton.setSelected(false);
        m_kDrawOutlineButton.setBorderPainted(false);

        /* disable clear and apply sculpt buttons */
        m_kClearDrawOutlineButton.setEnabled(false);
        m_kInvertOutlineButton.setEnabled(false);
        m_kApplySculptButton.setEnabled(false);

        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.clearSculpt();
        }
    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  flag  dispose super or not, not used now.
     */
    public void disposeLocal() {
        rayBasedRenderWM = null;
    }

    /**
     * drawSculptRegion: called when the "Draw Sculpt Outline" Button is pressed.
     */
    public void drawSculptRegion() {

        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.enableSculpt(!rayBasedRenderWM.getSculptEnabled());
        }

        /* enable the clear and apply sculpt buttons */
        m_kDrawOutlineButton.setBorderPainted(true);
        m_kClearDrawOutlineButton.setEnabled(true);
        m_kInvertOutlineButton.setEnabled(true);
        m_kApplySculptButton.setEnabled(true);
    }

    /**
     * Initialize the buttons layout.
     */
    public void init() {
        JToolBar viewToolBar = new JToolBar();
        viewToolBar.setBorderPainted(true);
        viewToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        viewToolBar.setLayout(new GridBagLayout());
        viewToolBar.setFloatable(false);

        mainPanel = new JPanel(new BorderLayout());

        toolbarBuilder = new ViewToolBarBuilder(this);

        ButtonGroup cursorGroup = new ButtonGroup();
        Border pressedBorder = BorderFactory.createLoweredBevelBorder();

        // m_kDrawOutlineButton = new JToggleButton( MipavUtil.getIcon( "drawsculptor.gif" ), false );
        m_kDrawOutlineButton = toolbarBuilder.buildToggleButton("DrawSculptRegion", "Draw sculpt outline region",
                                                                "drawsculptor", cursorGroup);
        m_kDrawOutlineButton = new JToggleButton(MipavUtil.getIcon("sculptdraw.gif"), false);
        m_kDrawOutlineButton.addActionListener(this);
        m_kDrawOutlineButton.setMargin(new Insets(0, 0, 0, 0));
        m_kDrawOutlineButton.setToolTipText("Draw sculpt outline region");
        m_kDrawOutlineButton.setActionCommand("DrawSculptRegion");
        m_kDrawOutlineButton.setBorderPainted(false);
        m_kDrawOutlineButton.setRolloverEnabled(true);
        m_kDrawOutlineButton.setRolloverIcon(MipavUtil.getIcon("sculptdrawroll.gif"));
        m_kDrawOutlineButton.setBorder(pressedBorder);
        m_kDrawOutlineButton.setFocusPainted(false);
        cursorGroup.add(m_kDrawOutlineButton);

        m_kDrawOutlineButton.setEnabled(true);
        viewToolBar.add(m_kDrawOutlineButton);

        m_kClearDrawOutlineButton = toolbarBuilder.buildButton("UndoDrawSculptRegion",
                                                               "Clear draw sculpt outline region", "eraser");
        m_kClearDrawOutlineButton.setEnabled(false);
        viewToolBar.add(m_kClearDrawOutlineButton);

        m_kInvertOutlineButton = toolbarBuilder.buildButton("InvertSculptRegion", "Invert draw sculpt outline region",
                                                            "sculptinverse");
        m_kInvertOutlineButton.setEnabled(false);
        viewToolBar.add(m_kInvertOutlineButton);

        m_kApplySculptButton = toolbarBuilder.buildButton("ApplySculptRegion", "Apply sculpt region to volume",
                                                          "sculptapply");
        m_kApplySculptButton.setEnabled(false);
        viewToolBar.add(m_kApplySculptButton);

        m_kUndoSculptButton = toolbarBuilder.buildButton("UndoApplySculptRegion", "Undo apply sculpt region to volume",
                                                         "sculptorundo");
        m_kUndoSculptButton.setEnabled(false);
        viewToolBar.add(m_kUndoSculptButton);

        m_kSaveSculptButton = toolbarBuilder.buildButton("SaveSculptImage", "Save the sculpt region to image", "save");
        m_kSaveSculptButton.setEnabled(false);
        viewToolBar.add(m_kSaveSculptButton);

        viewToolBar.add(toolbarBuilder.makeSeparator());

        JLabel shapeLabel = new JLabel("Shape: ");
        shapeLabel.setFont(MipavUtil.font12B);
        shapeLabel.setForeground(Color.red);
        viewToolBar.add(shapeLabel);

        ButtonGroup shapeGroup = new ButtonGroup();
        lineButton = toolbarBuilder.buildToggleButton("lineShape", "Outline shape", "lineshape", shapeGroup);
        lineButton.addActionListener(this);
        lineButton.setMargin(new Insets(0, 0, 0, 0));
        lineButton.setBorderPainted(false);
        lineButton.setRolloverEnabled(true);
        lineButton.setBorder(pressedBorder);
        lineButton.setFocusPainted(false);
        lineButton.setSelected(true);
        shapeGroup.add(lineButton);
        viewToolBar.add(lineButton);

        rectButton = toolbarBuilder.buildToggleButton("rectShape", "Rectangle shape", "rectshape", shapeGroup);
        rectButton.addActionListener(this);
        rectButton.setMargin(new Insets(0, 0, 0, 0));
        rectButton.setBorderPainted(false);
        rectButton.setRolloverEnabled(true);
        rectButton.setBorder(pressedBorder);
        rectButton.setFocusPainted(false);
        rectButton.setSelected(false);
        shapeGroup.add(rectButton);
        viewToolBar.add(rectButton);


        JPanel panelToolbar = new JPanel();
        panelToolbar.setLayout(new GridBagLayout());
        panelToolbar.setVisible(true);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.WEST;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;

        panelToolbar.add(viewToolBar, gbc);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.setVisible(true);

        scrollPanel.add(panelToolbar, BorderLayout.PAGE_START);

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel.add(scroller, BorderLayout.NORTH);
    }

    /**
     * clearwSculptRegion: called when the "Clear Outline" Button is pressed.
     */
    public void invertSculptRegion() {

        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.invertSculpt();
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   panel width
     * @param  frameHeight  panel height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.setSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.revalidate();
    }

    /**
     * Initialize the sculpt region size. ViewJFrameVolumeView call this method to init the region.
     *
     * @param  width   region width
     * @param  height  region height
     */
    public void setFrameSize(int width, int height) {
        m_iSculptWidth = width;
        m_iSculptHeight = height;
    }

    /**
     * Set the sculpt shape, either lines or rectangle.
     *
     * @param  shape  shape number, 0 for lines, 1 for rectangle.
     */
    public void setSculptShape(int shape) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setDrawingShape(shape);
        }
    }

    /**
     * undoSculptRegion: called when the "Undo Sculpt" Button is pressed.
     */
    public void undoSculptRegion() {

        /* the m_kDrawOutlineButton is a toggle button, once undo is pressed,
         * un-toggle the draw button. */
        m_kDrawOutlineButton.setSelected(false);
        m_kDrawOutlineButton.setBorderPainted(false);
        m_kDrawOutlineButton.setFocusable(false);
        m_kClearDrawOutlineButton.setEnabled(false);
        m_kInvertOutlineButton.setEnabled(false);
        m_kApplySculptButton.setEnabled(false);
        m_kUndoSculptButton.setEnabled(false);
        m_kSaveSculptButton.setEnabled(false);

        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.undoSculpt();
       }

    }

    /**
     * Calls disposeLocal.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        this.disposeLocal();
        super.finalize();
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -2213835536118628636L;

        /**
         * DOCUMENT ME!
         *
         * @param  g  DOCUMENT ME!
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
        }
    }

}
