package gov.nih.mipav.view;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.CustomUIBuilder.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.net.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * Builds the GUI toolbars for the user interface.
 */
public class ViewToolBarBuilder implements ItemListener, ActionListener, Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** A border to use for pressed buttons. */
    protected static final Border pressedBorder = BorderFactory.createLoweredBevelBorder();

    /** A border used for each toolbar. */
    protected static final Border etchedBorder = BorderFactory.createEtchedBorder();

    /** DOCUMENT ME! */
    protected static final int NUM_BRUSHES_INTERNAL = 9;

    /** DOCUMENT ME! */
    public static final String USER_BRUSHES = System.getProperty("user.home") + File.separator + "mipav" +
                                              File.separator + "brushes" + File.separator;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The button used to toggle borders around painted areas. */
    public JToggleButton borderPaintButton, bogusBorderPaintButton;

    /** The button used to enable checker board display of two images. */
    protected JButton checkerBoardButton;

    /** The button used to select the color of the paint used. */
    protected JButton colorPaintButton;

    /** The button for presets used only for CT images. */
    protected JButton ctButton;
    
    public JCheckBox scrollButton;
    

    public JButton syncImagesIcon;
    
    
    /**
     * The combo box containing the possible choices for the current script in the script toolbar. Filled with entries
     * from the currently selected &quot;Scripting directory&quot;.
     */
    protected JComboBox currentScriptComboBox = new JComboBox();

    /** The script currently selected in the scripting toolbar (null if no script is selected). */
    protected String currentSelectedScript = null;

    /** The spinner for indicating the intensity to fill the image with when commiting paint in an image. */
    protected JSpinner intensitySpinner;

    /** The opacity of the paint, between 0 (transparent) and 1 (opaque). Set from the opacity dialog. */
    protected float opacity = 0.3f;

    /** Combo box to hold all of the paint brushes. */
    protected JComboBox paintBox;


    /** DOCUMENT ME! */
    protected JToggleButton paintBrushButton;

    /** The paint color to be used when the user paints in the image. */
    protected Color paintColor = new Color(225, 0, 0);

    /**
     * The button for the &quot;default&quot; mode of the mouse, where clicking in the image shows the intensity of that
     * voxel.
     */
    protected JToggleButton pointerVOIButton;

    /** The button used to enable the showing of a small portion of image b near the mouse cursor. */
    protected JToggleButton regButton;

    /** DOCUMENT ME! */
    protected Hashtable<String,String> scriptTable = new Hashtable<String,String>();

    /**
     * The class which wants to listen to changes made to this components of the toolbars. May have to be a
     * ActionListener, MouseListener, ChangeListener, or ViewJFrameBase depending on which toolbars are being used in a
     * particular dialog or frame.
     */
    protected Object UI;

    /** A button group for all toggle buttons which change the effect of mouse usage in the image. */
    protected ButtonGroup VOIGroup = new ButtonGroup();

    protected VOIColorButton voiColorButton = null;
    
    /** Vector to hold all toggle groups for VOI toggle (and custom toggle configurations)*/
    protected Vector<ButtonGroup> bgVector = new Vector<ButtonGroup>();
    
    /** The amount to change the value in the intensity spinner by when the user clicks it. */
    private double intensityStep = 1;

    /** The current value chosen in the intensity spinner. */
    private double intensityValue = 1;

    /** The maximum value which can be chosen in the intensity spinner. */
    private double maxIntensity = 255;

    /** The minimum value which can be chosen in the intensity spinner. */
    private double minIntensity = 0;

    /** DOCUMENT ME! */
    private String[] paintBrushNames = null;

    /** DOCUMENT ME! */
    private JPopupMenu popup = null;

    /** DOCUMENT ME! */
    private PopupListener popupListener = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Sets the UI.
     *
     * @param  _UI  The user interface pointer.
     */
    public ViewToolBarBuilder(Object _UI) {
        UI = _UI;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Create a blank toolbar and set it up.
     *
     * @return  a new toolbar
     */
    public static final JToolBar initToolBar() {
        JToolBar bar = new JToolBar();
        bar.setBorder(etchedBorder);
        bar.setBorderPainted(true);
        bar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        bar.setFloatable(false);

        return bar;
    }

    /**
     * Makes a separator for the use in the toolbars - a button with the proper icon.
     *
     * @return  The separator.
     */
    public static final JButton makeSeparator() {

        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));
        separator.setMargin(new Insets(0, 0, 0, 0));
        separator.setBorderPainted(false);
        separator.setFocusPainted(false);

        return (separator);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {

        if (e.getActionCommand().equals("Refresh")) {
            refreshPaintBox(false, -1);

        } else if (e.getActionCommand().equals("Delete")) {
            int index = paintBox.getSelectedIndex();
            refreshPaintBox(true, index);
        }
    }

    /**
     * Builds a toolbar with just the basic lut buttons on it.
     *
     * @return  the basic lut toolbar
     */
    public JToolBar buildBasicLUTToolBar() {
        JToolBar bar = initToolBar();

        bar.add(buildButton(CustomUIBuilder.PARAM_LUT));
        bar.add(buildButton(CustomUIBuilder.PARAM_LUT_CT));
        bar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_QUICK, VOIGroup));
        bar.add(buildButton(CustomUIBuilder.PARAM_LUT_RESET));

        return bar;
    }

    /**
     * Create a new toolbar button.
     *
     * @param   cmd       the command generated by the button
     * @param   tooltip   tooltip for the button
     * @param   iconBase  the base of the icon file names (eg - &quot;icon.gif&quot; and &quot;iconroll.gif&quot; would
     *                    have an &quot;icon&quot; <code>iconBase</code>
     *
     * @return  a new button
     */
    public final JButton buildButton(String cmd, String tooltip, String iconBase) {
        JButton button = new JButton(MipavUtil.getIcon(iconBase + ".gif"));
        button.addActionListener((ActionListener) UI);
        button.addItemListener(this);
        button.setActionCommand(cmd);
        button.setToolTipText(tooltip);

        if ((cmd != null) & (cmd.equals("QuickMask") || cmd.equals("QuickMaskReverse") ||
        		cmd.equals("CommitPaint") || cmd.equals("CommitPaintExt"))) {
            if ( UI instanceof MouseListener )
            {
                button.addMouseListener((MouseListener) UI);
            }
        }
        
        if (cmd != null) {
            KeyStroke k = Preferences.getShortcut(cmd);

            if (k != null) {
                button.setToolTipText(tooltip + "        (" + k.toString().replaceAll("pressed", "").trim() + ")");
            }
        }

        button.setBorderPainted(false);
        button.setFocusPainted(true);
        button.setRolloverEnabled(true);
        button.setRolloverIcon(MipavUtil.getIcon(iconBase + "roll.gif"));
        button.setMargin(new Insets(0, 0, 0, 0));

        return button;
    }

    /**
     * Creates a JButton using a UIParams parameters
     * @param params UIParams (static final variable from CustomUIBuilder)
     * @return
     */
    public final JButton buildButton(UIParams params) {
    	return this.buildButton(params.getActionCommand(), params.getToolTip(), params.getIconBase());
    }
    
    /**
     * Builds the general image toolbar, with buttons for saving the image, the histogram, etc., and a slider for a 3D
     * image.
     *
     * @param   numberOfDimensions  Number of dimensions of the image.
     * @param   type                Data type of image (really only care about color vs. the rest).
     *
     * @return  The general image toolbar.
     */
    public JToolBar buildGeneralToolBar(int numberOfDimensions, int type) {
        boolean isColorImage = ModelImage.isColorImage(type);

        JToolBar tBar = initToolBar();

        tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGE_OPEN));
        tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGE_SAVE));
        //tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGE_PRINT));
        tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGE_CAPTURE));

        tBar.add(makeSeparator());

        tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGE_HEADER));
        tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGE_ATTRIBUTES));


        tBar.add(makeSeparator());

        JButton winLevelButton = buildButton(CustomUIBuilder.PARAM_WINDOW_LEVEL);
        JButton invertButton = buildButton(CustomUIBuilder.PARAM_LUT_INVERT);
        JButton grayButton = buildButton(CustomUIBuilder.PARAM_LUT_GRAY);
        JButton hotmetalButton = buildButton(CustomUIBuilder.PARAM_LUT_HOTMETAL);

        if (isColorImage) {
            winLevelButton.setEnabled(false);
            invertButton.setEnabled(false);
            grayButton.setEnabled(false);
            hotmetalButton.setEnabled(false);
        }

        tBar.add(buildButton(CustomUIBuilder.PARAM_LUT));
        ctButton = buildButton(CustomUIBuilder.PARAM_LUT_CT);
        setCTButtonEnabled(false);
        tBar.add(ctButton);
        tBar.add(winLevelButton);
        tBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_QUICK, VOIGroup));
        tBar.add(buildButton(CustomUIBuilder.PARAM_LUT_RESET));
        tBar.add(invertButton);
        tBar.add(grayButton);
        tBar.add(hotmetalButton);
        tBar.add(buildButton(CustomUIBuilder.PARAM_LUT_OPEN));
        tBar.add(buildButton(CustomUIBuilder.PARAM_LUT_SAVE));
        tBar.add(makeSeparator());

        JButton leftArrowButton = buildButton(CustomUIBuilder.PARAM_IMAGE_SLICE_PREVIOUS);
        JButton rightArrowButton = buildButton(CustomUIBuilder.PARAM_IMAGE_SLICE_NEXT);
        leftArrowButton.addMouseListener((MouseListener) UI);
        rightArrowButton.addMouseListener((MouseListener) UI);

        if (numberOfDimensions == 2) {
            leftArrowButton.setEnabled(false);
            rightArrowButton.setEnabled(false);
        }

        tBar.add(leftArrowButton);
        tBar.add(rightArrowButton);

        tBar.add(makeSeparator());
        
        scrollButton = new JCheckBox(MipavUtil.getIcon(CustomUIBuilder.PARAM_IMAGE_LINK.getIconBase()));
        scrollButton.setPreferredSize(new Dimension(35,24));
        scrollButton.setMinimumSize(new Dimension(24,24));
        scrollButton.setSelectedIcon(MipavUtil.getIcon("link.gif"));
        scrollButton.addActionListener((ActionListener) UI);
        scrollButton.setActionCommand(CustomUIBuilder.PARAM_IMAGE_LINK.getActionCommand());
        scrollButton.setToolTipText(CustomUIBuilder.PARAM_IMAGE_LINK.getToolTip());
        if (numberOfDimensions == 2) {
            scrollButton.setEnabled(false);
        }
        //scrollButton.se
        
        tBar.add(scrollButton);
        

        
        
        syncImagesIcon = buildButton(CustomUIBuilder.PARAM_IMAGE_SYNC);
        tBar.add(syncImagesIcon);
        
        
        tBar.add(makeSeparator());

        tBar.add(buildToggleButton(CustomUIBuilder.PARAM_IMAGE_MAG, VOIGroup));
        tBar.add(buildToggleButton(CustomUIBuilder.PARAM_IMAGE_UNMAG, VOIGroup));
        tBar.add(buildToggleButton(CustomUIBuilder.PARAM_IMAGE_MAG_CUSTOM, VOIGroup));
        tBar.add(buildToggleButton(CustomUIBuilder.PARAM_IMAGE_MAG_REGION, VOIGroup));
        regButton = buildToggleButton(CustomUIBuilder.PARAM_IMAGE_MAG_WINDOW, VOIGroup);
        regButton.setEnabled(false);
        tBar.add(regButton);
        checkerBoardButton = buildButton(CustomUIBuilder.PARAM_IMAGE_MAG_CHECKER);
        tBar.add(checkerBoardButton);
        tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGE_MAG_ONE_TO_ONE));

        tBar.add(makeSeparator());

        JButton triPlanarButton = buildButton(CustomUIBuilder.PARAM_IMAGE_TRIPLANAR);
        JButton quadPlanarButton = buildButton(CustomUIBuilder.PARAM_IMAGE_VOLUME_RENDERER);
        JButton wmPlanarButton = buildButton(CustomUIBuilder.PARAM_IMAGE_VOLUME_RENDERER_GPU);
        JButton lightBoxButton = buildButton(CustomUIBuilder.PARAM_IMAGE_LIGHTBOX);
        // JButton gpuButton = buildButton(CustomUIBuilder.PARAM_IMAGE_GPU);
        // JButton multiButton = buildButton(CustomUIBuilder.PARAM_IMAGE_MULTI);
        // JButton vtkButton = buildButton("VTK", "VTK rendering", "vtk");

        if (numberOfDimensions == 2) {
            triPlanarButton.setEnabled(false);
            quadPlanarButton.setEnabled(false);
            wmPlanarButton.setEnabled(false);
            lightBoxButton.setEnabled(false);
            // gpuButton.setEnabled(false);
            // multiButton.setEnabled(false);
            // vtkButton.setEnabled(false);
        }

        tBar.add(triPlanarButton);
        tBar.add(quadPlanarButton);
        tBar.add(wmPlanarButton);
        tBar.add(lightBoxButton);

        // TODO: removed until we decide on a visualization system..
        // tBar.add(gpuButton);
        // tBar.add(multiButton);
        // tBar.add(vtkButton);

        tBar.add(makeSeparator());

        //tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGE_FLIP_HORIZONTAL));
        //tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGE_FLIP_VERTICAL));
        
        tBar.add(buildButton(CustomUIBuilder.PARAM_LAUNCH_IMAGEJ));
        tBar.add(buildButton(CustomUIBuilder.PARAM_MIPAV_TO_IMAGEJ));
        tBar.add(buildButton(CustomUIBuilder.PARAM_IMAGEJ_TO_MIPAV));

        return tBar;
    }

    /**
     * Creates the LUT thresholding toolbar.
     *
     * @return  the new toolbar
     */
    public JToolBar buildLUTThresholdToolBar() {
        JToolBar LUTToolBar = initToolBar();

        JButton entropyButton = buildButton(CustomUIBuilder.PARAM_LUT_THRESHOLD_MAX_ENT);
        entropyButton.setEnabled(false);
        LUTToolBar.add(entropyButton);

        JButton otsuButton = buildButton(CustomUIBuilder.PARAM_LUT_THRESHOLD_OTSU);
        otsuButton.setEnabled(false);
        LUTToolBar.add(otsuButton);

        LUTToolBar.add(makeSeparator());

        JButton thresholdButton = buildButton(CustomUIBuilder.PARAM_LUT_THRESHOLD_RUN);
        thresholdButton.setEnabled(false);
        LUTToolBar.add(thresholdButton);

        JButton inverseThresholdButton = buildButton(CustomUIBuilder.PARAM_LUT_THRESHOLD_INVERSE_RUN);
        inverseThresholdButton.setEnabled(false);
        LUTToolBar.add(inverseThresholdButton);

        LUTToolBar.add(makeSeparator());

        return LUTToolBar;
    }

    /**
     * Builds the LUT toolbar, with buttons for quick-changing the LUT of the image.
     *
     * @return  The LUT toolbar.
     */
    public JToolBar buildLUTToolBarBottom() {
        JToolBar LUTToolBar = initToolBar();

        ButtonGroup groupLUT = new ButtonGroup();

        LUTToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_TRANSFER, groupLUT));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_TRANSFER_RESET));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_TRANSFER_EVEN_DIST));
        LUTToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_THRESHOLD, groupLUT));
        LUTToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_THRESHOLD_INVERSE, groupLUT));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_CT_PRESETS));

        LUTToolBar.add(makeSeparator());

        LUTToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_EDIT_ALPHA, groupLUT));
        LUTToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_EDIT_RED, groupLUT));
        LUTToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_EDIT_GREEN, groupLUT));
        LUTToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_EDIT_BLUE, groupLUT));

        LUTToolBar.add(makeSeparator());
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_OPEN));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_SAVE));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_GENERATE));

        return LUTToolBar;
    }

    /**
     * Build the top part of the LUT toolbar.
     *
     * @return  the top part of the LUT toolbar
     */
    public JToolBar buildLUTToolBarTop() {
        JToolBar LUTToolBar = initToolBar();

        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_GRAY));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_RED));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_GREEN));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_BLUE));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_GRAY_BLUE_RED));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_HOTMETAL));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_SPECTRUM));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_COOL_HOT));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_SKIN));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_BONE));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_STRIPED));
        LUTToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_INVERT));

        return LUTToolBar;
    }

    /**
     * Builds the paint toolbar, with buttons for widths of paint brushes, color chooser, etc.
     *
     * @param   type  Data type of image (really color vs. the rest).
     * @param   nDim  The number of dimensions in the image.
     *
     * @return  The paint toolbar.
     */
    public JToolBar buildPaintToolBar(int type, int nDim) {
        boolean isColorImage = ModelImage.isColorImage(type);

        JToolBar paintToolBar = initToolBar();
        paintToolBar.setSize(320, 30);
        paintToolBar.setBounds(0, 0, 340, 30);

        // ButtonGroup paintThicknessGroup = new ButtonGroup();

        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_ADD_MASK));
        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_OPEN_MASK));
        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_SAVE_MASK));
        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_AND_OP_MASK));

        paintToolBar.add(makeSeparator());

        paintBrushButton = buildToggleButton(CustomUIBuilder.PARAM_PAINT_BRUSH, VOIGroup);
        paintToolBar.add(paintBrushButton);
        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_ADVANCED));
        paintToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_PAINT_DROPPER, VOIGroup));
        paintToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_PAINT_FILL, VOIGroup));
        paintToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_PAINT_ERASER, VOIGroup));
        // using a diff icon until an icon is ready for me to use

        if (nDim > 2) {
            paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_ERASE_SLICE));
        }

        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_ERASE_ALL));

        paintToolBar.add(makeSeparator());

        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_PROPAGATE_DOWN));
        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_PROPAGATE_ALL));
        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_PROPAGATE_UP));
        

        paintToolBar.add(makeSeparator());


        // create the list of brushes

        Integer[] intArray = getPaintList();
        paintBox = new JComboBox(intArray);
        paintBox.setFont(MipavUtil.font12);

        String brushName = Preferences.getProperty(Preferences.PREF_LAST_PAINT_BRUSH);

        if (brushName == null) {
            paintBox.setSelectedIndex(2);
        } else {
            int selectedIndex = 2;

            for (int i = 0; i < paintBrushNames.length; i++) {

                if (brushName.endsWith(paintBrushNames[i])) {
                    selectedIndex = i;

                    break;
                }
            }

            paintBox.setSelectedIndex(selectedIndex);
        }

        paintBox.setRenderer(new PaintBoxRenderer());

        paintBox.addItemListener(this);

        popup = new JPopupMenu();

        JMenuItem menuItem = new JMenuItem("Refresh");
        menuItem.addActionListener(this);
        popup.add(menuItem);

        popup.addSeparator();

        menuItem = new JMenuItem("Delete");
        menuItem.addActionListener(this);
        popup.add(menuItem);

        popupListener = new PopupListener();
        paintBox.addMouseListener(popupListener);

        paintToolBar.add(paintBox);

        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_BRUSH_EDITOR));

        paintToolBar.add(makeSeparator());

        intensitySpinner = new JSpinner(new SpinnerNumberModel(intensityValue, minIntensity, maxIntensity,
                                                               intensityStep));
        intensitySpinner.setMaximumSize(new Dimension(56, 24)); // 56 pixels wide, 24 tall
        intensitySpinner.setPreferredSize(new Dimension(56, 24)); // 56 pixels wide, 24 tall
        intensitySpinner.addChangeListener((ChangeListener) UI);
        intensitySpinner.setToolTipText("Paint intensity"); // bug in API prevents this from working

        if (isColorImage) {
            intensitySpinner.setEnabled(false);
        } else {
            setSpinnerValues(type);
        }

        paintToolBar.add(intensitySpinner);

        paintToolBar.add(makeSeparator());

        colorPaintButton = buildButton(CustomUIBuilder.PARAM_PAINT_COLOR);
        colorPaintButton.setBackground(paintColor);
        paintToolBar.add(colorPaintButton);

        JButton rgbCompButton = buildButton(CustomUIBuilder.PARAM_PAINT_RGB_CHOOSER);
        paintToolBar.add(rgbCompButton);
        if (!isColorImage) {
        	rgbCompButton.setEnabled(false);
        }
        
        
        JButton opacityPaintButton = buildButton(CustomUIBuilder.PARAM_PAINT_OPACITY);
        paintToolBar.add(opacityPaintButton);
        ButtonGroup borderPaintGroup = new ButtonGroup();
        borderPaintButton = buildToggleButton(CustomUIBuilder.PARAM_PAINT_BORDER,borderPaintGroup);
        bogusBorderPaintButton = new JToggleButton();
        borderPaintGroup.add(bogusBorderPaintButton);
        if (Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)) {
        	borderPaintButton.setSelected(true);
        }else {
        	bogusBorderPaintButton.setSelected(true);
        }
        paintToolBar.add(borderPaintButton);

        paintToolBar.add(makeSeparator());

        JButton commitPaintButton = buildButton(CustomUIBuilder.PARAM_PAINT_MASK_INSIDE);
        paintToolBar.add(commitPaintButton);

        JButton commitPaintButtonExt = buildButton(CustomUIBuilder.PARAM_PAINT_MASK_OUTSIDE);
        paintToolBar.add(commitPaintButtonExt);

        paintToolBar.add(makeSeparator());

        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_UNDO));

        paintToolBar.add(makeSeparator());

        paintToolBar.add(buildButton(CustomUIBuilder.PARAM_PAINT_VOLUME_CALCULATOR));

        paintToolBar.add(makeSeparator());

        JButton ppButton = buildButton(CustomUIBuilder.PARAM_PAINT_POWERPAINT);
        ppButton.setEnabled(nDim > 2);
        paintToolBar.add(ppButton);

        return paintToolBar;
    }

    /**
     * Creates the RGB histogram toolbar.
     *
     * @return  the new toolbar
     */
    public JToolBar buildRGBToolBar() {
        ButtonGroup RGBGroup = new ButtonGroup();
        ButtonGroup lutGroup = new ButtonGroup();

        JToolBar RGBToolBar = initToolBar();

        
        RGBToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_EDIT_RED, RGBGroup));
        RGBToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_EDIT_GREEN, RGBGroup));
        RGBToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_EDIT_BLUE, RGBGroup));
        RGBToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_EDIT_RGB, RGBGroup));
       

        RGBToolBar.add(ViewToolBarBuilder.makeSeparator());

        RGBToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_TRANSFER, lutGroup));
        RGBToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_TRANSFER_RESET, lutGroup));
        RGBToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_TRANSFER_EVEN_DIST));
        RGBToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_THRESHOLD, lutGroup));
        RGBToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_LUT_THRESHOLD_INVERSE, lutGroup));
        RGBToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_THRESHOLD_RUN));
        RGBToolBar.add(buildButton(CustomUIBuilder.PARAM_LUT_THRESHOLD_INVERSE_RUN));
        
        return RGBToolBar;
    }

    /**
     * Builds the script toolbar, for quickly recording and playing back scripts.
     *
     * @param   isRecording  whether we are recording a script
     *
     * @return  The script toolbar.
     */
    public JToolBar buildScriptToolBar(boolean isRecording) {

        if (currentSelectedScript == null) {
            currentSelectedScript = ((ViewJFrameBase) UI).getUserInterface().getLastScript();
        }

        JScriptToolBar scriptToolBar = new JScriptToolBar();
        scriptToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        scriptToolBar.setBorder(etchedBorder);
        scriptToolBar.setBorderPainted(true);
        scriptToolBar.setAlignmentX(java.awt.Component.LEFT_ALIGNMENT);
        scriptToolBar.setFloatable(false);

        JButton dirScriptButton = new JButton("Scripts directory...");
        dirScriptButton.addActionListener((ActionListener) UI);
        dirScriptButton.setToolTipText("Set the Script Home Directory");
        dirScriptButton.setFont(MipavUtil.font12B);
        dirScriptButton.setMinimumSize(new Dimension(20, 20));
        dirScriptButton.setMargin(new Insets(2, 7, 2, 7));
        dirScriptButton.setActionCommand("ToolbarScriptDir");
        scriptToolBar.add(dirScriptButton);

        // create a combo box containing the names of script files
        // (i.e. files with a .sct extension) located in the default
        // script directory.  Then set the current selected script
        // to the one saved in the preferences.
        JPanel currentScriptPanel = new JPanel();
        JLabel currentScriptLabel = new JLabel("Current Script: ");
        currentScriptLabel.setFont(MipavUtil.font12B);

        // get the current script directory --
        String dir = ((ViewJFrameBase) UI).getUserInterface().getDefaultScriptDirectory();

        this.updateScripts(dir);
        currentScriptComboBox.setFont(MipavUtil.font12B);

        if (currentScriptComboBox.getItemCount() > 0) {
            currentScriptComboBox.addItemListener(this);
        }

        currentScriptComboBox.setToolTipText("The active script");

        currentScriptPanel.add(currentScriptLabel);
        currentScriptPanel.add(currentScriptComboBox);
        scriptToolBar.add(currentScriptPanel);

        scriptToolBar.add(buildButton(CustomUIBuilder.PARAM_SCRIPT_REFRESH));
        scriptToolBar.add(buildButton(CustomUIBuilder.PARAM_SCRIPT_RUN));

        Icon recordIcon = null;

        if (isRecording) {
            recordIcon = MipavUtil.getIcon("recordpress.gif");
        } else {
            recordIcon = MipavUtil.getIcon("record.gif");
        }

        JButton recordScriptButton = new JButton(recordIcon);

        recordScriptButton.addActionListener((ActionListener) UI);
        recordScriptButton.setToolTipText("Start recording script.");
        recordScriptButton.setActionCommand("ToolbarScriptRecord");
        recordScriptButton.setBorderPainted(false);
        recordScriptButton.setRolloverEnabled(false);
        scriptToolBar.add(recordScriptButton);
        scriptToolBar.setRecordButton(recordScriptButton);

        return scriptToolBar;
    }

    /**
     * Helper method to build a text button for the toolbar.
     *
     * @param   text     Text for button.
     * @param   toolTip  Tool tip to be associated with button.
     * @param   action   Action command for button.
     *
     * @return  a new text button
     */
    public final JButton buildTextButton(String text, String toolTip, String action) {
        JButton button = new JButton(text);
        button.addActionListener((ActionListener) UI);
        button.setToolTipText(toolTip);
        button.setFont(MipavUtil.font12B);
        button.setMinimumSize(new Dimension(20, 20));
        button.setPreferredSize(new Dimension(90, 20));
        button.setMargin(new Insets(2, 7, 2, 7));
        button.setActionCommand(action);

        return button;
    }

    /**
     * Create a new toolbar togglable button.
     *
     * @param   cmd       the command generated by the button
     * @param   tooltip   tooltip for the button
     * @param   iconBase  the base of the icon file names (eg - &quot;icon.gif&quot; and &quot;iconroll.gif&quot; would
     *                    have an &quot;icon&quot; <code>iconBase</code>
     *
     * @return  a new togglable button
     */
    public final JToggleButton buildToggleButton(String cmd, String tooltip, String iconBase)
    {
        JToggleButton button = new JToggleButton(MipavUtil.getIcon(iconBase + ".gif"));
        button.addActionListener((ActionListener) UI);

        if (UI instanceof MouseListener) {

            if ((cmd != null) & (cmd.equals("MagImage") || cmd.equals("UnMagImage"))) {
                button.addMouseListener((MouseListener) UI);
            }
        }

        button.addItemListener(this);
        button.setActionCommand(cmd);
        button.setToolTipText(tooltip);

        if (cmd != null) {
            KeyStroke k = Preferences.getShortcut(cmd);

            if (k != null) {
                button.setToolTipText(tooltip + "        (" + k.toString().replaceAll("pressed", "").trim() + ")");
            }
        }

        button.setBorder(pressedBorder);
        button.setBorderPainted(false);
        button.setFocusPainted(false);
        button.setRolloverEnabled(true);
        button.setRolloverIcon(MipavUtil.getIcon(iconBase + "roll.gif"));
        button.setMargin(new Insets(0, 0, 0, 0));
        return button;
    }

    /**
     * Create a new toolbar togglable button.
     *
     * @param   cmd       the command generated by the button
     * @param   tooltip   tooltip for the button
     * @param   iconBase  the base of the icon file names (eg - &quot;icon.gif&quot; and &quot;iconroll.gif&quot; would
     *                    have an &quot;icon&quot; <code>iconBase</code>
     * @param   group     the button group to add the togglable button to (use null for the VOIGroup)
     *
     * @return  a new togglable button
     */
    public final JToggleButton buildToggleButton(String cmd, String tooltip, String iconBase, ButtonGroup group) {
        JToggleButton button = new JToggleButton(MipavUtil.getIcon(iconBase + ".gif"));
        button.addActionListener((ActionListener) UI);

        if (UI instanceof MouseListener) {

            if ((cmd != null) & (cmd.equals("MagImage") || cmd.equals("UnMagImage"))) {
                button.addMouseListener((MouseListener) UI);
            }
        }

        button.addItemListener(this);
        button.setActionCommand(cmd);
        button.setToolTipText(tooltip);

        if (cmd != null) {
            KeyStroke k = Preferences.getShortcut(cmd);

            if (k != null) {
                button.setToolTipText(tooltip + "        (" + k.toString().replaceAll("pressed", "").trim() + ")");
            }
        }

        button.setBorder(pressedBorder);
        button.setBorderPainted(false);
        button.setFocusPainted(false);
        button.setRolloverEnabled(true);
        button.setRolloverIcon(MipavUtil.getIcon(iconBase + "roll.gif"));
        button.setMargin(new Insets(0, 0, 0, 0));

        if (group == null) {

            // if being called from outside the tool bar builder (custom buttons, etc)
            group = VOIGroup;
        }

        group.add(button);

        return button;
    }

    /**
     * Create a new toolbar togglable button.
     *      
     * @param   params    button parameters that include actioncommand, tooltiptext, and iconbase
     * @param   group     the button group to add the togglable button to (use null for the VOIGroup)
     *
     * @return  a new togglable button
     */
    public final JToggleButton buildToggleButton(UIParams params, ButtonGroup group) {
    	return buildToggleButton(params.getActionCommand(), params.getMnemonic(), params.getToolTip(), 
    			params.getIconBase(),  group);
    }
    
    /**
     * Create a new toolbar togglable button.
     *
     * @param   cmd       the command generated by the button
     * @param   mnemonic  short-cut mnemonic for this button
     * @param   tooltip   tooltip for the button
     * @param   iconBase  the base of the icon file names (eg - &quot;icon.gif&quot; and &quot;iconroll.gif&quot; would
     *                    have an &quot;icon&quot; <code>iconBase</code>
     * @param   group     the button group to add the togglable button to
     *
     * @return  a new togglable button
     */
    public final JToggleButton buildToggleButton(String cmd, int mnemonic, String tooltip, String iconBase,
                                                 ButtonGroup group) {
        JToggleButton button = buildToggleButton(cmd, tooltip, iconBase, group);
        if (button.getActionCommand().equals(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER.getActionCommand())) {
        	pointerVOIButton = button;
        }
        
        if (mnemonic != UIParams.INVALID_MNEMONIC) {
        	button.setMnemonic(mnemonic);
        }

        return button;
    }

    public JToolBar buildCustomToolBar(Vector<CustomUIBuilder.UIParams> paramVector) {
    	JToolBar tBar = initToolBar();
    	
    	ButtonGroup bg = null;
    	Component nextComponent = null;
    	for (int i = 0; i < paramVector.size(); i++) {
    		if (paramVector.elementAt(i).isToggle()) {
    			if (bg == null) {
    				bg = new ButtonGroup();
    			}
    			nextComponent = buildToggleButton(paramVector.elementAt(i), bg);
    		} else {
    			if (paramVector.elementAt(i).equals(CustomUIBuilder.PARAM_VOI_COLOR)) {
    				voiColorButton = new VOIColorButton(0);
    				nextComponent = voiColorButton;
    			} else {
    				nextComponent = buildButton(paramVector.elementAt(i));
    			}
    		}
    		tBar.add(nextComponent);
    	}
    	
    	if (bg != null) {
    		bgVector.add(bg);
    	}
    	tBar.add(makeSeparator());
    	return tBar;
    }    

    private JToolBar build3DVOIToolBar(JToolBar VOIToolBar, int numberOfDimensions, int voiIndex) {
        VOIToolBar.setBorder(etchedBorder);
        VOIToolBar.setBorderPainted(true);
        VOIToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        VOIToolBar.setFloatable(false);

        pointerVOIButton = buildToggleButton(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER, VOIGroup);
        VOIToolBar.add(pointerVOIButton);

        VOIToolBar.add(makeSeparator());
        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_TEXT, VOIGroup));
        
        VOIToolBar.add(makeSeparator());
        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_POINT, VOIGroup));

        JToggleButton polysliceButton = buildToggleButton(CustomUIBuilder.PARAM_VOI_POLY_SLICE, VOIGroup);

        // polysliceButton.setEnabled(false);
        polysliceButton.setEnabled(numberOfDimensions > 2);
        VOIToolBar.add(polysliceButton);


        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_LINE, VOIGroup));
        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_PROTRACTOR, VOIGroup));
        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_RECTANGLE, VOIGroup));
        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_ELLIPSE, VOIGroup));
        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_POLYGON, VOIGroup));


        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_LEVELSET, VOIGroup));
        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_LIVEWIRE, VOIGroup));

        JToggleButton cubeVOIButton = buildToggleButton(CustomUIBuilder.PARAM_VOI_3D_RECTANGLE, VOIGroup);
        VOIToolBar.add(cubeVOIButton);

        if (numberOfDimensions == 2) {
            cubeVOIButton.setEnabled(false);
        }

        

        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_SPLITTER, null));
        
        voiColorButton = new VOIColorButton(voiIndex);
        VOIToolBar.add(makeSeparator());
       
       
        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_NEW, VOIGroup));
        VOIToolBar.add(makeSeparator());
        
        VOIToolBar.add(buildToggleButton(CustomUIBuilder.PARAM_VOI_PROPERTIES.getActionCommand(), 
                CustomUIBuilder.PARAM_VOI_PROPERTIES.getMnemonic(), CustomUIBuilder.PARAM_VOI_PROPERTIES.getToolTip(), 
                "voiquestion", VOIGroup));
        VOIToolBar.add(voiColorButton);

        VOIToolBar.add(makeSeparator());

        VOIToolBar.add(buildButton(CustomUIBuilder.PARAM_VOI_UNDO));
        VOIToolBar.add(buildButton(CustomUIBuilder.PARAM_VOI_REDO));

        // VOIToolBar.add( buildButton( "deleteVOI", "Delete selected contour", "delete" ) );
        VOIToolBar.add(buildButton(CustomUIBuilder.PARAM_VOI_CUT));
        VOIToolBar.add(buildButton(CustomUIBuilder.PARAM_VOI_COPY));
        VOIToolBar.add(buildButton(CustomUIBuilder.PARAM_VOI_PASTE));

        VOIToolBar.add(makeSeparator());

        JButton propDownVOIButton = buildButton(CustomUIBuilder.PARAM_VOI_PROPAGATE_DOWN);
        JButton propAllVOIButton = buildButton(CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL);
        JButton propUpVOIButton = buildButton(CustomUIBuilder.PARAM_VOI_PROPAGATE_UP);

        if (numberOfDimensions == 2) {
            propUpVOIButton.setEnabled(false);
            propAllVOIButton.setEnabled(false);
            propDownVOIButton.setEnabled(false);
        }

        VOIToolBar.add(propDownVOIButton);
        VOIToolBar.add(propAllVOIButton);
        VOIToolBar.add(propUpVOIButton);

        VOIToolBar.add(makeSeparator());
        VOIToolBar.add(buildButton(CustomUIBuilder.PARAM_VOI_QUICK_AND_OP));
        VOIToolBar.add(buildButton(CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP));

        return VOIToolBar;
    }
    
    
    /**
     * Builds the VOI toolbar, with buttons for creating various types of VOIs (elliptical, square, etc.), and for cut
     * and paste operations.
     * @return  the VOI toolbar
     */
    public JToolBar buildVolumeTriPlanarVOIToolBar(int numberOfDimensions, int voiIndex, boolean bTraverseImage, boolean bOpacity, ButtonGroup kGroup) {
        if ( kGroup != null )
        {
            VOIGroup = kGroup;
        }
        JToolBar VOIToolBar = new JToolBar();
        VOIToolBar.setBorder(etchedBorder);
        VOIToolBar.setBorderPainted(true);
        VOIToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        VOIToolBar.setFloatable(false);

        if ( bTraverseImage )
        {
            JToggleButton pointerButton = buildToggleButton(new UIParams("Traverse image", "Default", UIParams.INVALID_MNEMONIC, 
                    "Default Mode", "translate", true), VOIGroup);
            VOIToolBar.add(pointerButton);
        }
        build3DVOIToolBar( VOIToolBar, numberOfDimensions, voiIndex );

        if ( bOpacity )
        {
            VOIToolBar.add(makeSeparator());
            VOIToolBar.add( buildButton(CustomUIBuilder.PARAM_PAINT_OPACITY) );
        }
        VOIToolBar.add(makeSeparator());        
        VOIToolBar.add(buildButton(CustomUIBuilder.PARAM_VOI_3D_INTERSECTION));
        VOIToolBar.add(buildButton(CustomUIBuilder.PARAM_VOI_3D_UNION));
        
        return VOIToolBar;
    }


    
    
    

    /**
     * Accessor that returns the current intensity paint name (that is, the text of the intensity button, 0, 1, etc.).
     *
     * @return  Current intensity paint name.
     */
    public String getIntensityPaintName() {
        return ((SpinnerNumberModel) intensitySpinner.getModel()).getNumber().toString();
    }

    /**
     * Accessor that returns the current opacity of the paint.
     *
     * @return  Current opacity of the paint.
     */
    public float getOpacity() {
        return opacity;
    }

    /**
     * Returns the selected paint brush's index.
     *
     * @return  DOCUMENT ME!
     */
    public int getPaintBrush() {

        if (paintBox != null) {
            return paintBox.getSelectedIndex();
        } else {
            return 0;
        }
    }

    /**
     * Returns the name of the paintbrush at the given index.
     *
     * @param   index  the index of the paint brush
     *
     * @return  the name
     */
    public String getPaintBrushName(int index) {
        String name = null;

        if ((paintBox != null) && (index < paintBrushNames.length)) {
            return paintBrushNames[index];
        }

        return name;
    }

    /**
     * Accessor that returns the current color of the paint.
     *
     * @return  Current color of the paint.
     */
    public Color getPaintColor() {
        return paintColor;
    }
    
    public JToggleButton getPointerButton( )
    {
        return pointerVOIButton;
    }

    /**
     * Returns the full path and file name of the currently selected script file in the scripting toolbar.
     *
     * @return  The full path and file name of the currently selected script.
     */
    public String getSelectedScriptFileName() {
        return (String) scriptTable.get((String) currentScriptComboBox.getSelectedItem());
    }

    /**
     * Returns the VOI Color/properties button
     * @return voi color button
     */
    public VOIColorButton getVOIColorButton() {
    	return this.voiColorButton;
    }
    
    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Sets border painted or not painted depending on if the button was selected or deselected. Changes the currently
     * selected script.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        int state = event.getStateChange();

        if ((source instanceof JComboBox) && ((JComboBox) source).equals(currentScriptComboBox)) {

            if (state == ItemEvent.SELECTED) {
                currentSelectedScript = (String) scriptTable.get((String) currentScriptComboBox.getSelectedItem());
                ((ViewJFrameBase) UI).getUserInterface().setLastScript(currentSelectedScript);


                Preferences.debug("toolbar:\tCurrent selected script is: " + currentSelectedScript + "\n",
                                  Preferences.DEBUG_SCRIPTING);
            }
        } else if (source.equals(paintBox)) {
            int index = paintBox.getSelectedIndex();

            if (UI instanceof ViewJFrameImage) {
                ((ViewJFrameImage) UI).getComponentImage().loadPaintBrush(paintBrushNames[index], false);
                Preferences.setProperty(Preferences.PREF_LAST_PAINT_BRUSH, paintBrushNames[index]);
            }

        } else if (source instanceof AbstractButton) {

            ((AbstractButton) source).setBorderPainted(state == ItemEvent.SELECTED);
        }
    }

    /**
     * Method to run the script currently selected in the scripting toolbar.
     */
    public void runCurrentScript() {

        try {
            String scriptFile = getSelectedScriptFileName();

            ((ViewJFrameBase) UI).getUserInterface().setLastScript(scriptFile);

            String[] imageVars = Parser.getImageVarsUsedInScript(scriptFile);

            if (imageVars.length == 0) {
                ScriptRunner.getReference().runScript(scriptFile, new Vector<String>(), new Vector<String>());
            } else if ((imageVars.length == 1) &&
                           (Parser.getNumberOfVOIsRequiredForImageVar(scriptFile, imageVars[0]) == 0)) {
                Vector<String> imageVector = new Vector<String>();
                String imageName = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage().getImageName();
                imageVector.addElement(imageName);
                ScriptRunner.getReference().runScript(scriptFile, imageVector, new Vector<String>());
            } else {
                new JDialogRunScriptController(scriptFile);
            }
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered running script:\n " + pe);
        }
    }

    /**
     * Accessor that enables or disables the checkerboard button.
     *
     * @param  flag  <code>true</code> to enable, <code>false</code> to disable.
     */
    public void setCheckboardButtonEnabled(boolean flag) {

        if (checkerBoardButton != null) {
            checkerBoardButton.setEnabled(flag);
        }
    }

    /**
     * Accessor that enables or disables the CT button.
     *
     * @param  flag  <code>true</code> to enable, <code>false</code> to disable.
     */
    public void setCTButtonEnabled(boolean flag) {

        if (ctButton != null) {
            ctButton.setEnabled(flag);
        }
    }

    /**
     * Accessor that sets the intensity paint name (that is, the text of the intensity spinner).
     *
     * @param  stringValue  Value to set it to.
     */
    public void setIntensityPaintName(String stringValue) {

        try {
            Double doubleValue = new Double(stringValue);
            ((SpinnerNumberModel) intensitySpinner.getModel()).setValue(doubleValue);
        } catch (Exception e) {
            // parameter was illegal
        }
    }

    /**
     * Accessor that sets the opacity of the paint.
     *
     * @param  op  Opacity to set to.
     */
    public void setOpacity(float op) {
        opacity = op;
    }

    /**
     * Sets the index of the paintBox (to select a different paint brush).
     *
     * @param  index  index of the paint box (brush) to select
     */
    public void setPaintBrush(int index) {

        if ((paintBox != null) && (index < paintBrushNames.length)) {
            paintBox.setSelectedIndex(index);
        }
    }


    /**
     * Accessor that sets the paint brush button to selected.
     */
    public void setPaintBrushButtonSelected() {

        if (paintBrushButton != null) {
            paintBrushButton.setSelected(true);
        }
    }

    /**
     * Accessor that sets the color of the paint.
     *
     * @param  color  Color to set paint to.
     */
    public void setPaintColor(Color color) {
        paintColor = color;

        if (colorPaintButton != null) {
            colorPaintButton.setBackground(color);
        }
    }
    
    public void setPointerButton( JToggleButton pointerButton )
    {
        if ( pointerButton != null )
        {
            pointerVOIButton = pointerButton;
        }
    }

    /**
     * Accessor that sets the pointer button to selected.
     */
    public void setPointerSelected() {

        if (pointerVOIButton != null) {
            pointerVOIButton.setSelected(true);
        }
    }

    /**
     * Accessor that enables or disables the window region button.
     *
     * @param  flag  <code>true</code> to enable, <code>false</code> to disable.
     */
    public void setRegButtonEnabled(boolean flag) {

        if (regButton != null) {
            regButton.setEnabled(flag);
        }
    }

    /**
     * Sets the spinner values based on image type.
     *
     * @param  type  Image type (BYTE, FLOAT, ...)
     */
    public void setSpinnerValues(int type) {

        intensityValue = 1.0;

        if (type == ModelStorageBase.BOOLEAN) {
            minIntensity = 0;
            maxIntensity = 1;
        } else if (type == ModelStorageBase.BYTE) {
            minIntensity = -128;
            maxIntensity = 127;
            intensityStep = 1;
        } else if (type == ModelStorageBase.UBYTE) {
            minIntensity = 0;
            maxIntensity = 255;
            intensityStep = 1;
        } else if (type == ModelStorageBase.SHORT) {
            minIntensity = -32768;
            maxIntensity = 32767;
            intensityStep = 1;
        } else if (type == ModelStorageBase.USHORT) {
            minIntensity = 0;
            maxIntensity = 65535;
            intensityStep = 1;
        } else if (type == ModelStorageBase.INTEGER) {
            minIntensity = Integer.MIN_VALUE;
            maxIntensity = Integer.MAX_VALUE;
            intensityStep = 1;
        } else if (type == ModelStorageBase.UINTEGER) {
            minIntensity = 0;
            maxIntensity = 4294967295L;
            intensityStep = 1;
        } else if (type == ModelStorageBase.LONG) {
            minIntensity = Long.MIN_VALUE;
            maxIntensity = Long.MAX_VALUE;
            intensityStep = 1;
        } else if (type == ModelStorageBase.FLOAT) {
            minIntensity = -Float.MAX_VALUE;
            maxIntensity = Float.MAX_VALUE;
            intensityStep = 0.1;
        } else if (type == ModelStorageBase.DOUBLE) {
            minIntensity = -Double.MAX_VALUE;
            maxIntensity = Double.MAX_VALUE;
            intensityStep = 1;
        }

        if (intensitySpinner != null) {
            ((SpinnerNumberModel) (intensitySpinner.getModel())).setMinimum(new Double(minIntensity));
            ((SpinnerNumberModel) (intensitySpinner.getModel())).setMaximum(new Double(maxIntensity));
            ((SpinnerNumberModel) (intensitySpinner.getModel())).setStepSize(new Double(intensityStep));
            ((SpinnerNumberModel) (intensitySpinner.getModel())).setValue(new Double(intensityValue));
        }
    }


    /**
     * Sets the correct Toggle button to be selected (based on action command).
     *
     * @param  command  String the action command of the VOI Button (easiest)
     */
    public void setToggleButtonSelected(String actionCommand) {
    	if (VOIGroup != null) {

            Enumeration<AbstractButton> e = VOIGroup.getElements();
            JToggleButton tButton;

            while (e.hasMoreElements()) {
                tButton = (JToggleButton) e.nextElement();

                if (tButton.getActionCommand().equalsIgnoreCase(actionCommand)) {
                    tButton.setSelected(true);

                    break;
                }
            }
        } 
    	
    	if (bgVector != null) {
    		ButtonGroup bg = null;
    		for (int i = 0; i < bgVector.size(); i++) {
    			bg = bgVector.elementAt(i);
    			Enumeration<AbstractButton> e = bg.getElements();
                JToggleButton tButton;

                while (e.hasMoreElements()) {
                    tButton = (JToggleButton) e.nextElement();

                    if (tButton.getActionCommand().equalsIgnoreCase(actionCommand)) {
                        tButton.setSelected(true);

                        break;
                    }
                }
    		}
    	}
    	
    }

    public void setVOIGroup( ButtonGroup newVOIGroup )
    {
        if ( newVOIGroup != null )
        {
            VOIGroup = newVOIGroup;
        }
    }
    
    /**
     * Method to update the list of scripts in the scripting toolbar based on the directory name provided. It is assumed
     * that all scripts end with an .sct extension.
     *
     * @param  dirName  The name of the directory containing the scripts
     */
    public void updateScripts(String dirName) {

        File dirFile = new File(dirName);

        // clear the current script combo box
        currentScriptComboBox.removeAllItems();

        scriptTable.clear();

        // if directory doesn't exist, or isn't a directory
        // then return
        if (!dirFile.exists() || !dirFile.isDirectory()) {
            currentScriptComboBox.setEnabled(false);

            return;
        }

        // create a filter for script files only
        ViewImageFileFilter filter = new ViewImageFileFilter(ViewImageFileFilter.SCRIPT);
        String[] filenames = null;

        try {
            filenames = filter.listFiles(dirFile);
        } catch (Exception e) {
            Preferences.debug("toolbar:\tUnable to access script files in " + dirName + "\n",
                              Preferences.DEBUG_SCRIPTING);
            currentScriptComboBox.setEnabled(false);

            return;
        }

        if ((filenames == null) || (filenames.length == 0)) {
            Preferences.debug("toolbar:\tFound no script files in " + dirName + "\n", Preferences.DEBUG_SCRIPTING);
            currentScriptComboBox.setEnabled(false);

            return;
        }

        String name;
        // add the filenames to the script combo box

        Arrays.sort(filenames);

        for (int i = 0; i < filenames.length; i++) {
            name = filenames[i].substring(filenames[i].lastIndexOf(File.separatorChar) + 1);
            scriptTable.put(name, filenames[i]);
            currentScriptComboBox.addItem(name);
        }

        if (currentSelectedScript != null) {
            name = currentSelectedScript.substring(currentSelectedScript.lastIndexOf(File.separatorChar) + 1);

            currentScriptComboBox.setSelectedItem(name);
        }

        currentScriptComboBox.setEnabled(true);

    } // end updateScripts

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Integer[] getPaintList() {
        Integer[] intArray = null;

        int numBrushes = NUM_BRUSHES_INTERNAL; // built in... 9 so far

        File brushesDir = new File(USER_BRUSHES);

        if (brushesDir.isDirectory()) {
            File[] brushes = brushesDir.listFiles();

            for (int i = 0; i < brushes.length; i++) {

                if (brushes[i].getName().endsWith(".png")) {
                    numBrushes++;
                }
            }
        }

        intArray = new Integer[numBrushes];

        for (int i = 0; i < intArray.length; i++) {
            intArray[i] = new Integer(i);
        }

        paintBrushNames = new String[numBrushes];

        paintBrushNames[0] = "square 1x1.gif";
        paintBrushNames[1] = "square 2x2.gif";
        paintBrushNames[2] = "square 4x4.gif";
        paintBrushNames[3] = "square 8x8.gif";
        paintBrushNames[4] = "square 16x16.gif";
        paintBrushNames[5] = "square 24x24.gif";
        paintBrushNames[6] = "circle 10x10.gif";
        paintBrushNames[7] = "circle 14x14.gif";
        paintBrushNames[8] = "circle 20x20.gif";

        if (brushesDir.isDirectory()) {
            File[] brushes = brushesDir.listFiles();
            int brushIndex = NUM_BRUSHES_INTERNAL;

            for (int i = 0; i < brushes.length; i++) {

                if (brushes[i].getName().endsWith(".png")) {
                    paintBrushNames[brushIndex] = brushes[i].getName();
                    brushIndex++;
                }
            }
        }

        return intArray;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doDelete        DOCUMENT ME!
     * @param  indexOfRemoval  DOCUMENT ME!
     */
    private void refreshPaintBox(boolean doDelete, int indexOfRemoval) {
        JComponent parent = (JComponent) paintBox.getParent();
        parent.setVisible(false);

        int index = 0;

        for (index = 0; index < parent.getComponentCount(); index++) {

            if (parent.getComponent(index).equals(paintBox)) {
                parent.remove(paintBox);

                break;
            }
        }

        paintBox.removeItemListener(this);
        paintBox.removeAllItems();
        paintBox = null;

        // remove the .png here
        if (doDelete) {

            if (indexOfRemoval >= NUM_BRUSHES_INTERNAL) {

                if (new File(USER_BRUSHES + File.separator + paintBrushNames[indexOfRemoval]).exists()) {
                    new File(USER_BRUSHES + File.separator + paintBrushNames[indexOfRemoval]).delete();
                }
            }
        }

        Integer[] intArray = getPaintList();
        paintBox = new JComboBox(intArray);
        paintBox.setFont(MipavUtil.font12);

        paintBox.setRenderer(new PaintBoxRenderer());
        paintBox.addItemListener(this);
        paintBox.addMouseListener(popupListener);

        paintBox.setSelectedIndex(2);

        parent.add(paintBox, index);
        parent.setVisible(true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Class used to indicate on the toolbar whether a script is being recorded.
     */
    public class JScriptToolBar extends JToolBar {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 6205577015710803423L;

        /** The recording button. */
        private JButton recordScriptButton = null;

        /**
         * Sets the button used to start/stop recording.
         *
         * @param  recordButton  the button to use
         */
        public void setRecordButton(JButton recordButton) {
            this.recordScriptButton = recordButton;
        }

        /**
         * Sets whether a script is being recorded and changes the appearance of the button accordingly.
         *
         * @param  isRecording  true if a script is being recorded, false otherwise
         */
        public void setRecording(boolean isRecording) {

            if (isRecording) {
                recordScriptButton.setToolTipText("Recording script.");
                recordScriptButton.setIcon(MipavUtil.getIcon("recordpress.gif"));
            } else {
                recordScriptButton.setToolTipText("Start recording script.");
                recordScriptButton.setIcon(MipavUtil.getIcon("record.gif"));
            }
        }
    }


    public class VOIColorButton extends JButton {
    	
    	public VOIColorButton(int voiIndex) {
    		super(MipavUtil.getIcon(CustomUIBuilder.PARAM_VOI_COLOR.getIconBase() + ".gif"));
            setVOIColor(voiIndex);
            setToolTipText(CustomUIBuilder.PARAM_VOI_COLOR.getToolTip());
            setActionCommand(CustomUIBuilder.PARAM_VOI_COLOR.getActionCommand());
            addActionListener((ActionListener) UI);
            setPreferredSize(new Dimension(24, 24));
            setMaximumSize(new Dimension(24, 24));
            setSize(new Dimension(24, 24));
            setEnabled(true);
            setRolloverEnabled(false);
            setBorder(BorderFactory.createEtchedBorder(Color.white, Color.black));
            
    	}
    	public void setVOIColor(int voiIndex) {

            if (voiIndex == -1) {
                voiIndex = 0;
            }

            float hue = (float) ((((voiIndex + Preferences.getVOIColorIncrement()) * 35) % 360) / 360.0);
            Color color = Color.getHSBColor(hue, (float) 1.0, (float) 1.0);

            //System.err.println("Color is: R=" + color.getRed() + ", G=" + color.getGreen() + ", B=" + color.getBlue());
            setBackground(color);
            setForeground(color);
        }
    	
    	/**
         * Sets the buttons background and foreground to a specified color.
         *
         * @param  newColor  Color
         */
        public void setVOIColor(Color newColor) {
            setBackground(newColor);
            setForeground(newColor);
        }
    }

    /**
     * DOCUMENT ME!
     */
    class PaintBoxRenderer extends JLabel implements ListCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -965184394601656795L;

        /**
         * Creates a new ComboBoxRenderer2 object.
         */
        public PaintBoxRenderer() {
            setOpaque(true);
            setHorizontalAlignment(LEFT);
            setVerticalAlignment(CENTER);
        }

        /**
         * This method finds the image and text corresponding to the selected value and returns the label, set up to
         * display the text and image.
         *
         * @param   list          DOCUMENT ME!
         * @param   value         DOCUMENT ME!
         * @param   index         DOCUMENT ME!
         * @param   isSelected    DOCUMENT ME!
         * @param   cellHasFocus  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                                                      boolean cellHasFocus) {

            // Get the selected index. (The index param isn't
            // always valid, so just use the value.)
            int selectedIndex = ((Integer) value).intValue();

            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }


            // Set the icon and text.  If icon was null, say so.
            ImageIcon icon = null;

            setFont(MipavUtil.font12);

            if (selectedIndex < NUM_BRUSHES_INTERNAL) {
                icon = MipavUtil.getIcon(paintBrushNames[selectedIndex]);

            } else {
                URL res = null;

                try {
                    res = new File(USER_BRUSHES + File.separator + paintBrushNames[selectedIndex]).toURI().toURL();
                    icon = new ImageIcon(res);

                    if ((icon.getIconHeight() >= 20) || (icon.getIconWidth() >= 20)) {
                        int newWidth, newHeight;

                        if (icon.getIconHeight() < icon.getIconWidth()) {
                            newWidth = 20;

                            float factor = 24f / icon.getIconWidth();
                            newHeight = (int) (icon.getIconHeight() * factor);
                        } else {
                            newHeight = 20;

                            float factor = 24f / icon.getIconHeight();
                            newWidth = (int) (icon.getIconWidth() * factor);
                        }

                        icon = new ImageIcon(icon.getImage().getScaledInstance(newWidth, newHeight, 0));
                    }
                } catch (Exception e) {
                    // e.printStackTrace();
                }
            }

            setText(paintBrushNames[selectedIndex].substring(0, paintBrushNames[selectedIndex].lastIndexOf(".")));
            setIcon(icon);

            setPreferredSize(new Dimension(90, 24));
            setIconTextGap(10);
            setHorizontalTextPosition(LEFT);

            return this;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private class PopupListener extends MouseAdapter {

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void mousePressed(MouseEvent e) {
            triggerPopup(e);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseReleased(MouseEvent e) {
            triggerPopup(e);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        private void triggerPopup(MouseEvent e) {

            if (e.isPopupTrigger()) {
                popup.show(e.getComponent(), e.getX(), e.getY());
            }
        }
    }

}
