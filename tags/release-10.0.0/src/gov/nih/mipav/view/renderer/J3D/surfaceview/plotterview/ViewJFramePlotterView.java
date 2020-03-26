package gov.nih.mipav.view.renderer.J3D.surfaceview.plotterview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import com.sun.j3d.utils.universe.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;


/**
 * <p>Title: ViewJFramePlotterView</p>
 *
 * <p>Description: The plotter render view frame. The plotter render views images based on each slice intensity values.
 * Component surface created from a 2D image. The intensity values are mapped like a relief map, with higher intensities
 * making peaks and lower intensities forming valleys. A quad mesh is used to create the three-dimensional map. The same
 * LUT from the image is used to colors the vertices of the quad mesh.</p>
 *
 * @author  Matthew J. McAuliffe, Ph.D.
 */
public class ViewJFramePlotterView extends ViewJFrameBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8972056170229565046L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Configuration param, which will pass down to each render's constructor. */
    protected GraphicsConfiguration config;

    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;

    /** DOCUMENT ME! */
    private JPanel cameraPanel;

    /** The image panel to hold one Canvas3D. */
    private JPanel imagePanel;

    /** Image line mode check box. */
    private JCheckBoxMenuItem itemLine;

    /** DOCUMENT ME! */
    private JPanel mousePanel;

    /** Menu bar. */
    private JMenuBar openingMenuBar;

    /** Toolbar panel. */
    private JPanel panelToolbar;

    /** Surface plotter view. */
    private SurfacePlotter plotterRender;

    /** Panel Border view. */
    private Border raisedbevel, loweredbevel, compound;

    /** Screen width, screen height. */
    private int screenWidth, screenHeight;

    /** DOCUMENT ME! */
    private JPanel sliderControlPanel;

    /** DOCUMENT ME! */
    private JPanel surfaceBoxPanel;

    /** DOCUMENT ME! */
    private Vector surTabVector = new Vector();

    /** The main tabbed pane in the volume view frame. */
    private JTabbedPane tabbedPane;

    /** For each render, use the vector to store the currently active tabs. */
    private Vector tabVector = new Vector();

    /** Control panel for the surface renderer. */
    private JPanel viewPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The frame the rendering the plotter view of images. The plotter renderer view images based on the image intensity
     * values.
     *
     * @param  _imageA  ModelImage source model images
     * @param  _LUTa    ModelLUT image LUT table
     * @param  slice    int slice number
     */
    public ViewJFramePlotterView(ModelImage _imageA, ModelLUT _LUTa, int slice) {
        super(_imageA, null);

        /** Progress bar show up during the volume view frame loading */
        progressBar = new ViewJProgressBar("Constructing renderers...", "Constructing renderers...", 0, 100, false,
                                           null, null);
        progressBar.updateValue(0, true);
        progressBar.setLocation(300, 300);
        progressBar.setVisible(true);
        config = SimpleUniverse.getPreferredConfiguration();
        progressBar.updateValueImmed(1);

        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        progressBar.updateValueImmed(1);

        plotterRender = new SurfacePlotter(_imageA, _LUTa, slice, config);
        progressBar.updateValueImmed(80);

        progressBar.updateValueImmed(100);

        this.init();

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        progressBar.dispose();

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calls various methods depending on the action.
     *
     * <ul>
     *   <li>Surface - opens the surface dialog</li>
     *   <li>View - opens the view control dialog</li>
     *   <li>Mouse - opens the mouse recorder dialog</li>
     *   <li>About - displays a message about this renderer</li>
     *   <li>Exit - sets variables to null and disposes of this frame</li>
     *   <li>X, Y, Z checkboxes - toggles the appropriate image planes on or off</li>
     * </ul>
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (command.equals("ViewControls")) {
            insertTab("View", viewPanel);
        } else if (command.equals("Mouse")) {
            insertTab("Mouse", mousePanel);
        } else if (command.equals("Load")) { // load();
        } else if (command.equals("Save")) { // save();
        } else if (command.equals("Line")) {
            plotterRender.setPolygonMode(itemLine.getState());
        } else if (command.equals("Resample")) {
            String first = "" + plotterRender.getSampleSize();
            String[] possibleValues = { "1", "2", "4", "8" };
            String selectedValue = (String) JOptionPane.showInputDialog(this, "Choose a sample size", "Resample",
                                                                        JOptionPane.PLAIN_MESSAGE, null, possibleValues,
                                                                        first);

            plotterRender.resample(selectedValue);

        } else if (command.equals("Box")) {
            insertTab("Box", surfaceBoxPanel);
        } else if (command.equals("AutoCapture")) {
            insertTab("Camera", cameraPanel);
        }

    }

    /**
     * Dispose memory.
     *
     * @param  flag  DOCUMENT ME!
     */
    public void disposeLocal(boolean flag) {
        plotterRender.disposeLocal();

    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ViewControlsImage getControls() {
        return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getImageB() {
        return null;
    }

    /**
     * Insert the new tab into the current visible tab list.
     *
     * @param  _name   String Tab name.
     * @param  _panel  JPanel Control panel.
     */
    public void insertTab(String _name, JPanel _panel) {
        int i;
        int selected = 1;
        boolean foundCrop = false;

        for (i = 0; i < tabVector.size(); i++) {

            if ((tabVector.elementAt(i) != null) && ((TabbedItem) (tabVector.elementAt(i))).name.equals(_name)) {
                tabbedPane.setSelectedIndex(i);

                return;
            }

            if (((TabbedItem) (tabVector.elementAt(i))).name.equals("Crop")) {
                selected = i;
                foundCrop = true;
            }

        }

        if (tabVector.size() >= 4) {

            if (foundCrop) {
                selected -= 1;
            }

            tabVector.remove(1);
            tabbedPane.remove(1);
            tabVector.add(new TabbedItem(_name, _panel));
        } else {
            tabVector.add(new TabbedItem(_name, _panel));
        }

        for (i = 1; i < tabVector.size(); i++) {
            String name = ((TabbedItem) (tabVector.elementAt(i))).name;
            JPanel panel = ((TabbedItem) (tabVector.elementAt(i))).panel;

            tabbedPane.addTab(name, null, panel);
        }

        tabbedPane.setSelectedIndex(tabVector.size() - 1);
    }

    /**
     * DOCUMENT ME!
     */
    public void removeControls() { }

    /**
     * DOCUMENT ME!
     *
     * @param  active  DOCUMENT ME!
     */
    public void setActiveImage(int active) { }

    /**
     * DOCUMENT ME!
     *
     * @param  value  DOCUMENT ME!
     */
    public void setAlphaBlend(int value) { }

    /**
     * DOCUMENT ME!
     */
    public void setControls() { }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setEnabled(boolean flag) { }

    /**
     * DOCUMENT ME!
     *
     * @param  imageB  DOCUMENT ME!
     */
    public void setImageB(ModelImage imageB) { }

    /**
     * Accessor that sets the LUT.
     *
     * @param  LUT  the LUT
     */
    public void setLUTa(ModelLUT LUT) {

        if (plotterRender != null) {
            plotterRender.setLUTa(LUT);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setPaintBitmapSwitch(boolean flag) { }

    // The following 2 functions set the RGB tables for ARGB images A and B.
    /**
     * Sets the RGB table for ARGB image A.
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTA(ModelRGB RGBT) {

        if (plotterRender != null) {
            plotterRender.setRGBTA(RGBT);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTB(ModelRGB RGBT) { }

    /**
     * Funcions do nothing, but extends ViewJFrameBase.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setSlice(int slice) { }

    /**
     * DOCUMENT ME!
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setTimeSlice(int slice) { }

    /**
     * DOCUMENT ME!
     */
    public void setTitle() { }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * Update images in surface render, raycast render and shearwarp render.
     *
     * @return  boolean boolean confirming successful update
     */
    public boolean updateImages() {

        if (plotterRender != null) {
            plotterRender.updateImages();
        }

        return true;
    }

    /**
     * This methods calls corresponding render to update images without LUT changes.
     *
     * @param   forceShow  forces show to reimport image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(boolean forceShow) {

        if (plotterRender != null) {
            plotterRender.updateImages(forceShow);
        }

        return true;

    }

    /**
     * This methods calls corresponding render to update images with LUT changes.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   forceShow   forces show to reimport image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean forceShow, int interpMode) {

        if (plotterRender != null) {
            plotterRender.updateImages(LUTa, LUTb, forceShow, interpMode);
        }

        return true;
    }

    /**
     * Closes window and disposes of frame and component.
     *
     * @param  event  Event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        close();
        disposeLocal(true);
    }

    /**
     * Builds menu.
     *
     * @return  DOCUMENT ME!
     */
    protected JMenuBar buildMenu() {
        JPopupMenu.setDefaultLightWeightPopupEnabled(false);

        JMenu fileMenu = new JMenu("File");

        fileMenu.setFont(MipavUtil.font12B);

        JMenu optionsMenu = new JMenu("Options");

        optionsMenu.setFont(MipavUtil.font12B);

        JMenuItem itemExit = new JMenuItem("Exit");

        itemExit.addActionListener(this);
        itemExit.setActionCommand("Exit");
        itemExit.setFont(MipavUtil.font12B);
        fileMenu.add(itemExit);

        JMenuItem itemView = new JMenuItem("View mode");

        itemView.addActionListener(this);
        itemView.setActionCommand("ViewControls");
        itemView.setFont(MipavUtil.font12B);
        optionsMenu.add(itemView);

        JMenuItem itemMouse = new JMenuItem("Mouse recorder");

        itemMouse.addActionListener(this);
        itemMouse.setActionCommand("Mouse");
        itemMouse.setFont(MipavUtil.font12B);
        optionsMenu.add(itemMouse);

        itemLine = new JCheckBoxMenuItem("Line mode");
        itemLine.addActionListener(this);
        itemLine.setActionCommand("Line");
        itemLine.setFont(MipavUtil.font12B);
        optionsMenu.add(itemLine);

        JMenuItem itemSample = new JMenuItem("Resample");

        itemSample.addActionListener(this);
        itemSample.setActionCommand("Resample");
        itemSample.setFont(MipavUtil.font12B);
        optionsMenu.add(itemSample);

        openingMenuBar = new JMenuBar();
        openingMenuBar.add(fileMenu);
        openingMenuBar.add(optionsMenu);

        return openingMenuBar;
    }

    /**
     * Constructs main frame structures for 3 images.
     */
    protected void configureFrame() {
        JMenuBar menuBar = buildMenu();

        setJMenuBar(menuBar);
        buildToolBar();

        if (imageA == null) {
            return;
        }

        setResizable(true);

        raisedbevel = BorderFactory.createRaisedBevelBorder();
        loweredbevel = BorderFactory.createLoweredBevelBorder();
        compound = BorderFactory.createCompoundBorder(raisedbevel, loweredbevel);

        buildViewPanel();
        buildMousePanel();
        buildCameraPanel();
        buildSurfaceBoxPanel();

        setTitle("Surface Plotter");

        imagePanel = new JPanel(new BorderLayout());

        setSize(300, 500);

        setLocation(100, 100);

        imagePanel.add(plotterRender.getCanvas(), BorderLayout.CENTER);

        int imagePanelWidth = (int) (screenWidth * 0.4f);
        int imagePanelHeight = (int) (screenHeight * 0.4f);

        imagePanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
        imagePanel.setMinimumSize(new Dimension(imagePanelWidth, 1));
        imagePanel.setBorder(compound);

        JPanel tabPanel = new JPanel(new BorderLayout());

        tabPanel.add(tabbedPane);
        tabPanel.setMinimumSize(new Dimension(0, 789));

        JSplitPane mainPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, tabPanel, imagePanel);

        mainPane.setOneTouchExpandable(true);
        mainPane.setContinuousLayout(true);

        getContentPane().add(mainPane, BorderLayout.CENTER);

        // MUST register frame to image models
        imageA.addImageDisplayListener(this);

        pack();
        setVisible(true);

        updateImages(true);
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    }

    /**
     * Cleans up memory from gc.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {

        disposeLocal(false);
        super.finalize();
    }

    /**
     * Initialize the the frame layout.
     */
    protected void init() {
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        getContentPane().add(tabbedPane, BorderLayout.WEST);

        screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
        screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;

        int tabbedPaneWidth = (int) (screenWidth * 0.22f);
        int tabbedPaneHeight = (int) (screenHeight * 0.5f);

        tabbedPane.setPreferredSize(new Dimension(tabbedPaneWidth, tabbedPaneHeight));

        // tabbedPane.addChangeListener( this );
        sliderControlPanel = new JPanel();
        sliderControlPanel.add(plotterRender.getControlPanel());

        tabbedPane.addTab("Slider", null, sliderControlPanel);
        tabVector.add(new TabbedItem("Slider", sliderControlPanel));

        imageA.setImageOrder(ModelImage.IMAGE_A);

        this.configureFrame();

    }

    /**
     * Build the camera snap shot panel.
     */
    private void buildCameraPanel() {
        cameraPanel = new JPanel();
        cameraPanel.add(plotterRender.getCameraPanel().getMainPanel());
    }

    /**
     * Build the mouse recorder panel.
     */
    private void buildMousePanel() {
        mousePanel = new JPanel();
        mousePanel.add(plotterRender.getMousePanel().getMainPanel());

    }

    /**
     * Build the options panel.
     */
    private void buildSurfaceBoxPanel() {
        surfaceBoxPanel = new JPanel();
        surfaceBoxPanel.add(plotterRender.getSurfaceBoxPanel().getMainPanel());
    }

    /**
     * Builds the toolbar for the volume render frame.
     */
    private void buildToolBar() {

        panelToolbar = new JPanel();
        panelToolbar.setLayout(new GridLayout(1, 6, 0, 0));

        Border etchedBorder = BorderFactory.createEtchedBorder();

        JToolBar toolBar = new JToolBar();

        toolBar.setBorder(etchedBorder);
        toolBar.setBorderPainted(true);
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

        JButton autosnapButton = new JButton(MipavUtil.getIcon("camera.gif"));

        autosnapButton.addActionListener(this);
        autosnapButton.setToolTipText("Auto snapshot screen");
        autosnapButton.setActionCommand("AutoCapture");
        autosnapButton.setBorderPainted(false);
        autosnapButton.setRolloverEnabled(true);
        autosnapButton.setRolloverIcon(MipavUtil.getIcon("cameraroll.gif"));
        autosnapButton.setFocusPainted(false);
        autosnapButton.setEnabled(true);
        toolBar.add(autosnapButton);

        JButton mouseRecorderButton = new JButton(MipavUtil.getIcon("movie.gif"));

        mouseRecorderButton.addActionListener(this);
        mouseRecorderButton.setToolTipText("Mouse Recorder");
        mouseRecorderButton.setActionCommand("Mouse");
        mouseRecorderButton.setBorderPainted(false);
        mouseRecorderButton.setRolloverEnabled(true);
        mouseRecorderButton.setRolloverIcon(MipavUtil.getIcon("movieroll.gif"));
        mouseRecorderButton.setFocusPainted(false);
        mouseRecorderButton.setEnabled(true);
        toolBar.add(mouseRecorderButton);

        JButton optionButton = new JButton(MipavUtil.getIcon("options.gif"));

        optionButton.addActionListener(this);
        optionButton.setToolTipText("View Options");
        optionButton.setActionCommand("Box");
        optionButton.setBorderPainted(false);
        optionButton.setRolloverEnabled(true);
        optionButton.setRolloverIcon(MipavUtil.getIcon("optionsroll.gif"));
        optionButton.setFocusPainted(false);
        optionButton.setEnabled(true);
        toolBar.add(optionButton);

        panelToolbar.add(toolBar);
        getContentPane().add(panelToolbar, BorderLayout.NORTH);
    }

    /**
     * Build the view control panel.
     */
    private void buildViewPanel() {
        viewPanel = new JPanel();
        viewPanel.add(plotterRender.getViewPanel().getMainPanel());
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Item to hold tab name and corresponding panel.
     */
    class TabbedItem {

        /** DOCUMENT ME! */
        public String name;

        /** DOCUMENT ME! */
        public JPanel panel;

        /**
         * Creates a new TabbedItem object.
         *
         * @param  _name   DOCUMENT ME!
         * @param  _panel  DOCUMENT ME!
         */
        public TabbedItem(String _name, JPanel _panel) {
            name = _name;
            panel = _panel;
        }
    }

}
