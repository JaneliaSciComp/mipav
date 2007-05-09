package gov.nih.mipav.view;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.net.*;

/**
 * Builds the GUI toolbars for the user interface.
 */
public class ViewToolBarBuilder implements ItemListener, ActionListener{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** A border to use for pressed buttons. */
    protected static final Border pressedBorder = BorderFactory.createLoweredBevelBorder();

    /** A border used for each toolbar. */
    protected static final Border etchedBorder = BorderFactory.createEtchedBorder();

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The button used to enable checker board display of two images. */
    protected JButton checkerBoardButton;

    /** The button used to select the color of the paint used. */
    protected JButton colorPaintButton;
    
    /** The button used to toggle borders around painted areas. */
    protected JButton borderPaintButton;

    /** The button for presets used only for CT images. */
    protected JButton ctButton;

    /**
     * The combo box containing the possible choices for the current script in the script toolbar. Filled with entries
     * from the currently selected &quot;Scripting directory&quot;.
     */
    protected JComboBox currentScriptComboBox = new JComboBox();

    protected Hashtable scriptTable = new Hashtable();
    
    /** The script currently selected in the scripting toolbar (null if no script is selected). */
    protected String currentSelectedScript = null;

    /** The spinner for indicating the intensity to fill the image with when commiting paint in an image. */
    protected JSpinner intensitySpinner;

    /** The opacity of the paint, between 0 (transparent) and 1 (opaque). Set from the opacity dialog. */
    protected float opacity = 0.3f;

    /** The paint color to be used when the user paints in the image. */
    protected Color paintColor = new Color(225, 0, 0);

    /**
     * The button for the &quot;default&quot; mode of the mouse, where clicking in the image shows the intensity of that
     * voxel.
     */
    protected JToggleButton pointerVOIButton;
    
    
    protected JToggleButton paintBrushButton;

    /** The button used to enable the showing of a small portion of image b near the mouse cursor. */
    protected JToggleButton regButton;

    /** Combo box to hold all of the paint brushes */
    protected JComboBox paintBox;
    
    /**
     * The class which wants to listen to changes made to this components of the toolbars. May have to be a
     * ActionListener, MouseListener, ChangeListener, or ViewJFrameBase depending on which toolbars are being used in a
     * particular dialog or frame.
     */
    protected Object UI;

    /** A button group for all toggle buttons which change the effect of mouse usage in the image. */
    protected ButtonGroup VOIGroup = new ButtonGroup();

    /** The amount to change the value in the intensity spinner by when the user clicks it. */
    private double intensityStep = 1;

    /** The current value chosen in the intensity spinner. */
    private double intensityValue = 1;

    /** The maximum value which can be chosen in the intensity spinner. */
    private double maxIntensity = 255;

    /** The minimum value which can be chosen in the intensity spinner. */
    private double minIntensity = 0;

    private String [] paintBrushNames = null;
    
    protected static final int NUM_BRUSHES_INTERNAL = 5;
    
    private JPopupMenu popup = null;
    
    private PopupListener popupListener = null;
    
    public static final String USER_BRUSHES = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "brushes" + File.separator;
    
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
     * Builds a toolbar with just the basic lut buttons on it.
     *
     * @return  the basic lut toolbar
     */
    public JToolBar buildBasicLUTToolBar() {
        JToolBar bar = initToolBar();

        bar.add(buildButton("DisplayLUT", "Displays Lookup Table (LUT)", "histolut"));
        bar.add(buildButton("ctPresetsLUT", "CT preset function", "ctwindow"));
        bar.add(buildToggleButton("quickLUT", KeyEvent.VK_Q, "Quick LUT", "quicklut", VOIGroup));
        bar.add(buildButton("resetLUTs", "Reset LUT", "resetlut"));

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

        tBar.add(buildButton("OpenNewImage", "Open image", "open"));
        tBar.add(buildButton("SaveImage", "Save image", "save"));
        tBar.add(buildButton("PrintImage", "Print image", "printer"));
        tBar.add(buildButton("CaptureTiff", "Capture image to TIFF(RGB)", "camera"));

         tBar.add(makeSeparator());

        tBar.add(buildButton("AboutImage", "View Header", "header"));
        tBar.add(buildButton("EditImageInfo", "Edit attributes", "attributes"));



        tBar.add(makeSeparator());

        JButton winLevelButton = buildButton("winLevel", "Adjust window and level", "winlevel");
        JButton invertButton = buildButton("invertLUT", "Invert LUT", "invert");
        JButton grayButton = buildButton("GrayLUT", "Gray LUT", "gray");
        JButton hotmetalButton = buildButton("HotMetalLUT", "Hot Metal LUT", "hotmetal");

        if (isColorImage) {
            winLevelButton.setEnabled(false);
            invertButton.setEnabled(false);
            grayButton.setEnabled(false);
            hotmetalButton.setEnabled(false);
        }

        tBar.add(buildButton("DisplayLUT", "Displays Lookup Table (LUT)", "histolut"));
        ctButton = buildButton("ctPresetsLUT", "CT preset function", "ctwindow");
        setCTButtonEnabled(false);
        tBar.add(ctButton);
        tBar.add(winLevelButton);
        tBar.add(buildToggleButton("quickLUT", "Quick LUT", "quicklut", VOIGroup));
        tBar.add(buildButton("resetLUTs", "Reset LUT", "resetlut"));
        tBar.add(invertButton);
        tBar.add(grayButton);
        tBar.add(hotmetalButton);
        tBar.add(buildButton("OpenUDLUT", "Open user defined LUT", "userlutopen"));
        tBar.add(buildButton("SaveUDLUT", "Save user defined LUT", "userlutsave"));
        tBar.add(makeSeparator());

        JButton leftArrowButton = buildButton("PreviousImage", "<html>" + "Decrements image slice" + "<br>" + "Hold SHIFT to sync other images" + "</html>", "leftarrow");
        JButton rightArrowButton = buildButton("NextImage", "<html>" + "Increments image slice" + "<br>" + "Hold SHIFT to sync other images" + "</html>", "rightarrow");
        leftArrowButton.addMouseListener((MouseListener) UI);
        rightArrowButton.addMouseListener((MouseListener) UI);

        if (numberOfDimensions == 2) {
            leftArrowButton.setEnabled(false);
            rightArrowButton.setEnabled(false);
        }

        tBar.add(leftArrowButton);
        tBar.add(rightArrowButton);

        tBar.add(makeSeparator());

        tBar.add(buildToggleButton("MagImage","<html>" + "Magnify image 2.0x" + "<br>" + "Hold SHIFT for multiple zooming" + "</html>", "zoomin", VOIGroup));
        tBar.add(buildToggleButton("UnMagImage","<html>" + "Magnify image 0.5x" + "<br>" + "Hold SHIFT for multiple zooming" + "</html>", "zoomout", VOIGroup));
        tBar.add(buildToggleButton("MagRegion", "Magnify Region", "magregion", VOIGroup));
        regButton = buildToggleButton("WinRegion", "Window region of image B", "winregion", VOIGroup);
        regButton.setEnabled(false);
        tBar.add(regButton);
        checkerBoardButton = buildButton("CheckerBoard", "Checker Board A&B", "checker");
        tBar.add(checkerBoardButton);
        tBar.add(buildButton("ZoomOne", "Magnify image 1.0x", "zoom1"));

        tBar.add(makeSeparator());

        JButton triPlanarButton = buildButton("Tri-planar", "Tri-Planar View", "3plane");
        JButton quadPlanarButton = buildButton("VolTriplanar", "Volume Tri-Planar View", "4plane");
        JButton lightBoxButton = buildButton("Light box", "View Light Box", "lightbox");
        JButton gpuButton = buildButton("GPU", "GPU rendering", "gpu");
        JButton multiButton = buildButton("MultiHisto", "Multi-histo rendering", "multihisto");
        JButton vtkButton = buildButton("VTK", "VTK rendering", "vtk");
        
        if (numberOfDimensions == 2) {
            triPlanarButton.setEnabled(false);
            quadPlanarButton.setEnabled(false);
            lightBoxButton.setEnabled(false);
            gpuButton.setEnabled(false);
            multiButton.setEnabled(false);
            vtkButton.setEnabled(false);
        }

        tBar.add(triPlanarButton);
        tBar.add(quadPlanarButton);
        tBar.add(lightBoxButton);
        tBar.add(gpuButton);
        tBar.add(multiButton);
        tBar.add(vtkButton);

        tBar.add(makeSeparator());

        tBar.add(buildButton("ImageFlipY", "Flip horizontally", "fliphoriz"));
        tBar.add(buildButton("ImageFlipX", "Flip vertically", "flipvert"));

        return tBar;
    }

    /**
     * Creates the LUT thresholding toolbar.
     *
     * @return  the new toolbar
     */
    public JToolBar buildLUTThresholdToolBar() {
        JToolBar LUTToolBar = initToolBar();

        JButton entropyButton = buildButton("maxEntThreshold", "Maximum entropy threshold", "maxent");
        entropyButton.setEnabled(false);
        LUTToolBar.add(entropyButton);

        JButton otsuButton = buildButton("otsuThreshold", "Otsu threshold", "otsu");
        otsuButton.setEnabled(false);
        LUTToolBar.add(otsuButton);

        LUTToolBar.add(makeSeparator());

        JButton thresholdButton = buildButton("runThreshold", "Run threshold algorithm", "thresholdalgorithm");
        thresholdButton.setEnabled(false);
        LUTToolBar.add(thresholdButton);

        JButton inverseThresholdButton = buildButton("runInverseThreshold", "Run inverse threshold algorithm",
                                                     "thresholdalgorithminverse");
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

        LUTToolBar.add(buildToggleButton("linearLUT", "Transfer function", "transfer", groupLUT));
        LUTToolBar.add(buildButton("resetLinearLUT", "Reset transfer function", "linear"));
        LUTToolBar.add(buildButton("evendistriLUT", "Even distribution function", "evendistri"));
        LUTToolBar.add(buildToggleButton("thresholdLUT", "Dual threshold function", "threshold", groupLUT));
        LUTToolBar.add(buildToggleButton("inverseThresholdLUT", "Dual inverse threshold function", "thresholdinverse",
                                         groupLUT));
        LUTToolBar.add(buildButton("ctPresetsLUT", "CT preset function", "ctwindow"));

        LUTToolBar.add(makeSeparator());

        LUTToolBar.add(buildToggleButton("alpha", "Edit alpha function", "alpha", groupLUT));
        LUTToolBar.add(buildToggleButton("red", "Edit red LUT function", "red", groupLUT));
        LUTToolBar.add(buildToggleButton("green", "Edit green LUT function", "green", groupLUT));
        LUTToolBar.add(buildToggleButton("blue", "Edit blue LUT function", "blue", groupLUT));

        LUTToolBar.add(makeSeparator());
        LUTToolBar.add(buildButton("OpenUDLUT", "Open user defined LUT", "userlutopen"));
        LUTToolBar.add(buildButton("SaveUDLUT", "Save user defined LUT", "userlutsave"));
        LUTToolBar.add(buildButton("GenerateLUT", "Generate LUT table", "luttable"));

        return LUTToolBar;
    }

    /**
     * Build the top part of the LUT toolbar.
     *
     * @return  the top part of the LUT toolbar
     */
    public JToolBar buildLUTToolBarTop() {
        JToolBar LUTToolBar = initToolBar();

        LUTToolBar.add(buildButton("grayLUT", "Gray LUT", "gray"));
        LUTToolBar.add(buildButton("redLUT", "Red LUT", "redlut"));
        LUTToolBar.add(buildButton("greenLUT", "Green LUT", "greenlut"));
        LUTToolBar.add(buildButton("blueLUT", "Blue LUT", "bluelut"));
        LUTToolBar.add(buildButton("graybrLUT", "Gray Blue/Red LUT", "graybr"));
        LUTToolBar.add(buildButton("hotmetalLUT", "Hot metal LUT", "hotmetal"));
        LUTToolBar.add(buildButton("spectrumLUT", "Spectrum LUT", "spectrum"));
        LUTToolBar.add(buildButton("coolHotLUT", "Cool hot LUT", "coolhot"));
        LUTToolBar.add(buildButton("skinLUT", "Skin LUT", "skin"));
        LUTToolBar.add(buildButton("boneLUT", "Bone LUT", "bone"));
        LUTToolBar.add(buildButton("stripedLUT", "Striped LUT", "stripedLUT"));
        LUTToolBar.add(buildButton("invertLUT", "Invert LUT", "invert"));

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

        //ButtonGroup paintThicknessGroup = new ButtonGroup();

        paintToolBar.add(buildButton("NewMask", "Add a blank mask.", "newmask"));
        paintToolBar.add(buildButton("OpenMask", "Open mask from a file.", "openmask"));
        paintToolBar.add(buildButton("SaveMask", "Save current mask.", "savemask"));
        paintToolBar.add(buildButton("AndMask", "AND mask operation.", "andmask"));

        paintToolBar.add(makeSeparator());

        paintBrushButton = buildToggleButton("PaintBrush", "Draw using a brush.", "brush", VOIGroup);
        paintToolBar.add(paintBrushButton);
        paintToolBar.add(buildButton("AdvancedPaint", "Load advanced paint tools", "advancedpaint"));
        paintToolBar.add(buildToggleButton("Dropper", "Picks up a color from the image.", "dropper", VOIGroup));
        paintToolBar.add(buildToggleButton("PaintCan", "Fills an area with desired color.", "paintcan", VOIGroup));
        paintToolBar.add(buildToggleButton("Eraser", "Erases paint.", "eraser", VOIGroup));
        //using a diff icon until an icon is ready for me to use
        
        if (nDim > 2) {
            paintToolBar.add(buildButton("EraseCurrent", "Erase paint from current frame", "clearcurrent"));
        }

        paintToolBar.add(buildButton("EraseAll", "Erase all paint.", "clear"));

        paintToolBar.add(makeSeparator());

        paintToolBar.add(buildButton("PropagatePaintPrev", "Propagate the paint to the previous slice", "paintpropd"));
        paintToolBar.add(buildButton("PropagatePaintAll", "Propagate the paint to all slices", "paintpropall"));
        paintToolBar.add(buildButton("PropagatePaintNext", "Propagate the paint to the next slice", "paintpropu"));

        paintToolBar.add(makeSeparator());

       
        
        //create the list of brushes
        
        Integer [] intArray = getPaintList();
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
        
        paintToolBar.add(buildButton("PaintBrushEditor", "Paint brush editor.", "paint_brush_editor"));
        
        paintToolBar.add(makeSeparator());

        intensitySpinner = new JSpinner(new SpinnerNumberModel(intensityValue, minIntensity, maxIntensity, intensityStep));
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

        colorPaintButton = buildButton("colorPaint", "Change paint color.", "colorpaint");
        colorPaintButton.setBackground(paintColor);
        paintToolBar.add(colorPaintButton);

        JButton opacityPaintButton = new JButton("Opacity");
        opacityPaintButton.addActionListener((ActionListener) UI);
        opacityPaintButton.setToolTipText("Change opacity of paint.");
        opacityPaintButton.setFont(MipavUtil.font12B);
        opacityPaintButton.setMinimumSize(new Dimension(20, 20));
        opacityPaintButton.setMargin(new Insets(2, 7, 2, 7));
        opacityPaintButton.setActionCommand("OpacityPaint");
        paintToolBar.add(opacityPaintButton);
        
        borderPaintButton = buildButton(Preferences.PREF_SHOW_PAINT_BORDER, "Display border around painted areas.", "borderpaint");
        borderPaintButton.setName(Preferences.PREF_SHOW_PAINT_BORDER);
        paintToolBar.add(borderPaintButton);
        
        paintToolBar.add(makeSeparator());

        JButton commitPaintButton = buildButton("CommitPaint", "Masks the inside of the painted area.", "paintinside");
        paintToolBar.add(commitPaintButton);

        JButton commitPaintButtonExt = buildButton("CommitPaintExt", "Masks the outside of the painted area.",
                                                   "paintoutside");
        paintToolBar.add(commitPaintButtonExt);

        paintToolBar.add(makeSeparator());

        paintToolBar.add(buildButton("UndoPaint", "Undo last paint/region grow.", "undopaint"));

        paintToolBar.add(makeSeparator());

        paintToolBar.add(buildButton("CalcPaint", "Calculate volume of painted regions.", "calc"));

        paintToolBar.add(makeSeparator());

        paintToolBar.add(buildButton("PowerPaint", "Load power paint tools", "powerpaint"));

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

        JToggleButton redRGBButton = new JToggleButton(MipavUtil.getIcon("red.gif"));
        redRGBButton.addActionListener((ActionListener) UI);
        redRGBButton.setMargin(new Insets(0, 0, 0, 0));
        redRGBButton.setToolTipText("Edit red function");
        redRGBButton.setActionCommand("red");
        redRGBButton.setBorderPainted(false);
        redRGBButton.setRolloverEnabled(true);
        redRGBButton.setRolloverIcon(MipavUtil.getIcon("redroll.gif"));
        redRGBButton.setBorder(pressedBorder);
        redRGBButton.addItemListener(this);
        redRGBButton.setFocusPainted(false);
        RGBGroup.add(redRGBButton);
        RGBToolBar.add(redRGBButton);

        JToggleButton greenRGBButton = new JToggleButton(MipavUtil.getIcon("green.gif"));
        greenRGBButton.addActionListener((ActionListener) UI);
        greenRGBButton.setMargin(new Insets(0, 0, 0, 0));
        greenRGBButton.setToolTipText("Edit green function");
        greenRGBButton.setActionCommand("green");
        greenRGBButton.setBorderPainted(false);
        greenRGBButton.setRolloverEnabled(true);
        greenRGBButton.setRolloverIcon(MipavUtil.getIcon("greenroll.gif"));
        greenRGBButton.setBorder(pressedBorder);
        greenRGBButton.addItemListener(this);
        greenRGBButton.setFocusPainted(false);
        RGBGroup.add(greenRGBButton);
        RGBToolBar.add(greenRGBButton);

        JToggleButton blueRGBButton = new JToggleButton(MipavUtil.getIcon("blue.gif"));
        blueRGBButton.addActionListener((ActionListener) UI);
        blueRGBButton.setMargin(new Insets(0, 0, 0, 0));
        blueRGBButton.setToolTipText("Edit blue function");
        blueRGBButton.setActionCommand("blue");
        blueRGBButton.setBorderPainted(false);
        blueRGBButton.setRolloverEnabled(true);
        blueRGBButton.setRolloverIcon(MipavUtil.getIcon("blueroll.gif"));
        blueRGBButton.setBorder(pressedBorder);
        blueRGBButton.addItemListener(this);
        blueRGBButton.setFocusPainted(false);
        RGBGroup.add(blueRGBButton);
        RGBToolBar.add(blueRGBButton);

        JToggleButton allRGBButton = new JToggleButton(MipavUtil.getIcon("rgb.gif"));
        allRGBButton.addActionListener((ActionListener) UI);
        allRGBButton.setMargin(new Insets(0, 0, 0, 0));
        allRGBButton.setToolTipText("Lock RGB functions together");
        allRGBButton.setActionCommand("all");
        allRGBButton.setBorderPainted(true);
        allRGBButton.setRolloverEnabled(true);
        allRGBButton.setRolloverIcon(MipavUtil.getIcon("rgbroll.gif"));
        allRGBButton.setBorder(pressedBorder);
        allRGBButton.addItemListener(this);
        allRGBButton.setFocusPainted(false);
        allRGBButton.setSelected(true);
        RGBGroup.add(allRGBButton);
        RGBToolBar.add(allRGBButton);

        RGBToolBar.add(ViewToolBarBuilder.makeSeparator());

        /*
         * JButton resetButton = new JButton(MipavUtil.getIcon("resetlut.gif")); resetButton.addActionListener(al);
         * resetButton.setMargin(new Insets(0, 0, 0, 0)); resetButton.setToolTipText("Reset to linear transfer
         * function"); resetButton.setActionCommand("Linear"); resetButton.setBorderPainted(false);
         * resetButton.setRolloverEnabled(true); resetButton.setRolloverIcon(MipavUtil.getIcon("resetlutroll.gif"));
         * resetButton.setBorder(pressedBorder); resetButton.setFocusPainted(false); RGBToolBar.add(resetButton);
         * lutGroup.add(resetButton); RGBToolBar.add(ViewToolBarBuilder.makeSeparator());
         */

        JToggleButton transferButton = new JToggleButton(MipavUtil.getIcon("transfer.gif"));
        transferButton.addActionListener((ActionListener) UI);
        transferButton.setMargin(new Insets(0, 0, 0, 0)); //
        transferButton.setRolloverEnabled(true); //
        transferButton.setRolloverIcon(MipavUtil.getIcon("transferroll.gif"));
        transferButton.setBorder(pressedBorder); //
        transferButton.setBorderPainted(false);
        transferButton.setToolTipText("Transfer function");
        transferButton.setActionCommand("linearLUT");
        transferButton.addItemListener(this); //
        transferButton.setFocusPainted(false);

        // transferButton.setSelected( true );
        RGBToolBar.add(transferButton);
        lutGroup.add(transferButton);

        JButton linearButton = new JButton(MipavUtil.getIcon("linear.gif"));
        linearButton.addActionListener((ActionListener) UI);
        linearButton.setRolloverIcon(MipavUtil.getIcon("linearroll.gif"));
        linearButton.setBorderPainted(false);
        linearButton.setToolTipText("Reset transfer function");
        linearButton.setActionCommand("resetLinearLUT");
        RGBToolBar.add(linearButton);

        JButton evendistriButton = new JButton(MipavUtil.getIcon("evendistri.gif"));
        evendistriButton.addActionListener((ActionListener) UI);
        evendistriButton.setRolloverIcon(MipavUtil.getIcon("evendistriroll.gif"));
        evendistriButton.setBorderPainted(false);
        evendistriButton.setToolTipText("Event distribution function");
        evendistriButton.setActionCommand("evendistriLUT");
        RGBToolBar.add(evendistriButton);

        JToggleButton thresholdButton = new JToggleButton(MipavUtil.getIcon("threshold.gif"));
        thresholdButton.addActionListener((ActionListener) UI);
        thresholdButton.setMargin(new Insets(0, 0, 0, 0)); //
        thresholdButton.setRolloverEnabled(true); //
        thresholdButton.setRolloverIcon(MipavUtil.getIcon("thresholdroll.gif"));
        thresholdButton.setBorder(pressedBorder); //
        thresholdButton.setBorderPainted(false);
        thresholdButton.setToolTipText("Dual threshold function");
        thresholdButton.setActionCommand("thresholdLUT");
        thresholdButton.addItemListener(this); //
        thresholdButton.setFocusPainted(false);
        RGBToolBar.add(thresholdButton);
        lutGroup.add(thresholdButton);

        JToggleButton inverseThresholdButton = new JToggleButton(MipavUtil.getIcon("thresholdinverse.gif"));
        inverseThresholdButton.addActionListener((ActionListener) UI);
        inverseThresholdButton.setMargin(new Insets(0, 0, 0, 0)); //
        inverseThresholdButton.setRolloverEnabled(true); //
        inverseThresholdButton.setRolloverIcon(MipavUtil.getIcon("thresholdinverseroll.gif"));
        inverseThresholdButton.setBorder(pressedBorder); //
        inverseThresholdButton.setBorderPainted(false);
        inverseThresholdButton.setToolTipText("Dual inverse threshold function");
        inverseThresholdButton.setActionCommand("inverseThresholdLUT");
        inverseThresholdButton.addItemListener(this); //
        inverseThresholdButton.setFocusPainted(false);
        RGBToolBar.add(inverseThresholdButton);
        lutGroup.add(inverseThresholdButton);
        RGBToolBar.add(ViewToolBarBuilder.makeSeparator());

        JButton threshAlgoButton = new JButton(MipavUtil.getIcon("thresholdalgorithm.gif"));
        threshAlgoButton.addActionListener((ActionListener) UI);
        threshAlgoButton.setRolloverIcon(MipavUtil.getIcon("thresholdalgorithmroll.gif"));
        threshAlgoButton.setBorderPainted(false);
        threshAlgoButton.setToolTipText("Run threshold algorithm");
        threshAlgoButton.setActionCommand("runThreshold");
        threshAlgoButton.setEnabled(false);
        RGBToolBar.add(threshAlgoButton);

        JButton threshInverseAlgoButton = new JButton(MipavUtil.getIcon("thresholdalgorithminverse.gif"));
        threshInverseAlgoButton.addActionListener((ActionListener) UI);
        threshInverseAlgoButton.setRolloverIcon(MipavUtil.getIcon("thresholdalgorithminverseroll.gif"));
        threshInverseAlgoButton.setBorderPainted(false);
        threshInverseAlgoButton.setToolTipText("Run inverse threshold algorithm");
        threshInverseAlgoButton.setActionCommand("runInverseThreshold");
        threshInverseAlgoButton.setEnabled(false);
        RGBToolBar.add(threshInverseAlgoButton);

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

        // JButton dirScriptButton = new JButton( "Scripts Home..." );
        // dirScriptButton.addActionListener( (ActionListener) UI );
        // dirScriptButton.setToolTipText( "Set the Script Home Directory" );
        // dirScriptButton.setActionCommand( "ToolbarScriptDir" );
        // dirScriptButton.setBorder( beveledBorder );
        // scriptToolBar.add( dirScriptButton );

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

        scriptToolBar.add(buildButton("ToolbarScriptRefresh", "Refresh script listing.", "refresh"));
        scriptToolBar.add(buildButton("ToolbarScriptRun", "Run the selected script.", "play"));

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
        button.setMnemonic(mnemonic);

        return button;
    }

    /**
     * Builds the VOI toolbar, with buttons for creating various types of VOIs (elliptical, square, etc.), and for cut
     * and paste operations.
     *
     * @param   numberOfDimensions  the number of image dimensions
     * @param   voiIndex            index of the currently (or last) selected VOI
     *
     * @return  the VOI toolbar
     */
    public JVOIToolBar buildVOIToolBar(int numberOfDimensions, int voiIndex) {
        JVOIToolBar VOIToolBar = new JVOIToolBar(voiIndex);
        VOIToolBar.setBorder(etchedBorder);
        VOIToolBar.setBorderPainted(true);
        VOIToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        VOIToolBar.setFloatable(false);

        pointerVOIButton = buildToggleButton("Pointer", "Default Mode", "pointer", VOIGroup);
        VOIToolBar.add(pointerVOIButton);

        VOIToolBar.add(makeSeparator());
        VOIToolBar.add(buildToggleButton("TextVOI", "Annotation tool", "text", VOIGroup));
        VOIToolBar.add(makeSeparator());

        VOIToolBar.add(buildToggleButton("Point", "<html>" + "Draw point VOI" + "<br>" + "Hold SHIFT for multiple drawing" + "</html>", "pointROI", VOIGroup));

        JToggleButton polysliceButton = buildToggleButton( "Polyslice", "Draw inter-slice polyline", "polyframe", VOIGroup );
        //polysliceButton.setEnabled(false);
        polysliceButton.setEnabled( numberOfDimensions > 2);
        VOIToolBar.add( polysliceButton );



        VOIToolBar.add(buildToggleButton("Line", "Draw line VOI", "linear", VOIGroup));
        VOIToolBar.add(buildToggleButton("protractor", "Protractor tool", "protractor", VOIGroup));
        VOIToolBar.add(buildToggleButton("RectVOI", "<html>" + "Draw rectangle VOI" + "<br>" + "Hold SHIFT for multiple drawing" + "</html>", "rect", VOIGroup));
        VOIToolBar.add(buildToggleButton("EllipseVOI","<html>" + "Draw ellipse VOI" + "<br>" + "Hold SHIFT for multiple drawing" + "</html>", "circle", VOIGroup));
        VOIToolBar.add(buildToggleButton("Polyline", "Draw polygon/polyline VOI", "polygon", VOIGroup));


        VOIToolBar.add(buildToggleButton("LevelSetVOI","<html>" + "Draw levelset VOI" + "<br>" + "Hold SHIFT for multiple drawing" + "</html>", "contour", VOIGroup));
        VOIToolBar.add(buildToggleButton("LiveWireVOI", KeyEvent.VK_L, "Live wire VOI", "livewire", VOIGroup));

        JToggleButton cubeVOIButton = buildToggleButton("Rect3DVOI", "3D rectangular VOI", "cube", VOIGroup);
        VOIToolBar.add(cubeVOIButton);

        if (numberOfDimensions == 2) {
            cubeVOIButton.setEnabled(false);
        }

        VOIToolBar.add(buildToggleButton("NewVOI", "Initiate new VOI", "newvoi", VOIGroup));

        JButton temp = VOIToolBar.getVOIColorButton();
        temp.addActionListener((ActionListener) UI);
        temp.setActionCommand("VOIPropertiesColor");
        temp.setPreferredSize(new Dimension(24, 24));
        temp.setMaximumSize(new Dimension(24, 24));
        temp.setSize(new Dimension(24, 24));
        temp.setEnabled(true);
        temp.setRolloverEnabled(false);
        temp.setInputMap(0, new InputMap());
        temp.setBorder(BorderFactory.createEtchedBorder(Color.white, Color.black));
        VOIToolBar.add(makeSeparator());
        VOIToolBar.add(temp);

        VOIToolBar.add(makeSeparator());

        VOIToolBar.add(buildButton("undoVOI", "Undo last VOI change (Ctrl-Z)", "undopaint"));

        // VOIToolBar.add( buildButton( "deleteVOI", "Delete selected contour", "delete" ) );
        VOIToolBar.add(buildButton("cutVOI", "Cut selected contour (Ctrl-X)", "cutpaint"));
        VOIToolBar.add(buildButton("copyVOI", "Copy selected contour (Ctrl-C)", "copypaint"));
        VOIToolBar.add(buildButton("pasteVOI", "Paste contour (Ctrl-V)", "pastepaint"));

        VOIToolBar.add(makeSeparator());

        JButton propDownVOIButton = buildButton("PropVOIDown", "Propagate VOI down", "voipropd");
        JButton propAllVOIButton = buildButton("PropVOIAll", "Propagate VOI to all slices", "voipropall");
        JButton propUpVOIButton = buildButton("PropVOIUp", "Propagate VOI up", "voipropu");

        if (numberOfDimensions == 2) {
            propUpVOIButton.setEnabled(false);
            propAllVOIButton.setEnabled(false);
            propDownVOIButton.setEnabled(false);
        }

        VOIToolBar.add(propDownVOIButton);
        VOIToolBar.add(propAllVOIButton);
        VOIToolBar.add(propUpVOIButton);

        VOIToolBar.add(makeSeparator());
        VOIToolBar.add(buildButton("QuickMask", "Quick AND VOI mask operation.", "quickvoimask"));
        VOIToolBar.add(buildButton("QuickMaskReverse", "Quick NOT VOI mask operation", "quickvoimaskreverse"));

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
     * Returns the selected paint brush's index
     * @return
     */
    public int getPaintBrush() {
    	if (paintBox != null) {
    		return paintBox.getSelectedIndex();
    	} else {
    		return 0;
    	}
    }
    
    /** 
     * Returns the name of the paintbrush at the given index
     * @param index the index of the paint brush
     * @return the name
     */
    public String getPaintBrushName(int index) {
    	String name = null;
    	if (paintBox != null &&
    			index < paintBrushNames.length) {
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
                currentSelectedScript = (String)scriptTable.get((String) currentScriptComboBox.getSelectedItem());
                ((ViewJFrameBase) UI).getUserInterface().setLastScript(currentSelectedScript);
                
                
                
                Preferences.debug("toolbar:\tCurrent selected script is: " + currentSelectedScript + "\n", Preferences.DEBUG_SCRIPTING);
            }
        } else if (source.equals(paintBox)) {
        	int index = paintBox.getSelectedIndex();
        	if (UI instanceof ViewJFrameImage) {
        		((ViewJFrameImage)UI).getComponentImage().loadPaintBrush(paintBrushNames[index], false);
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
                ScriptRunner.getReference().runScript(scriptFile, new Vector(), new Vector());
            } else if (imageVars.length == 1 && Parser.getNumberOfVOIsRequiredForImageVar(scriptFile, imageVars[0]) == 0) {
                Vector imageVector = new Vector();
                String imageName = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage().getImageName();
                imageVector.addElement(imageName);
                ScriptRunner.getReference().runScript(scriptFile, imageVector, new Vector());
            } else {
                new JDialogRunScriptController(scriptFile);
            }
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered running script:\n " + pe);
        }
    }
    
    /**
     * Returns the full path and file name of the currently selected script file in the scripting toolbar.
     * @return The full path and file name of the currently selected script.
     */
    public String getSelectedScriptFileName() {
        return (String)scriptTable.get((String) currentScriptComboBox.getSelectedItem());
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
     * Sets the index of the paintBox (to select a different paint brush)
     * @param index index of the paint box (brush) to select
     */
    public void setPaintBrush(int index) {
    	if (paintBox != null && 
    			index < paintBrushNames.length) {
    		paintBox.setSelectedIndex(index);
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

    /**
     * Accessor that sets the pointer button to selected.
     */
    public void setPointerSelected() {
        if (pointerVOIButton != null) {
            pointerVOIButton.setSelected(true);
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
     * Sets the correct VOI button to be selected (based on action command)
     * @param command String the action command of the VOI Button (easiest)
     */
    public void setVOIButtonSelected(String command) {
        if ( VOIGroup != null) {

            Enumeration e = VOIGroup.getElements();
            JToggleButton tButton;
            while (e.hasMoreElements()) {
                tButton = (JToggleButton)e.nextElement();
                if (tButton.getActionCommand().equalsIgnoreCase(command)) {
                    tButton.setSelected(true);
                    break;
                }
            }
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
            Preferences.debug("toolbar:\tUnable to access script files in " + dirName + "\n", Preferences.DEBUG_SCRIPTING);
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

    public void actionPerformed(ActionEvent e) {
    	if (e.getActionCommand().equals("Refresh")) {
    		refreshPaintBox(false, -1);
    		
    	} else if (e.getActionCommand().equals("Delete")) {
    		int index = paintBox.getSelectedIndex();
    		refreshPaintBox(true, index);
    	}
    }
    
    private void refreshPaintBox(boolean doDelete, int indexOfRemoval) {
    	JComponent parent = (JComponent)paintBox.getParent();
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
		
		//remove the .png here
		if (doDelete) {
			if (indexOfRemoval >= NUM_BRUSHES_INTERNAL) {
				if (new File(USER_BRUSHES + File.separator + paintBrushNames[indexOfRemoval]).exists()) {
					new File(USER_BRUSHES + File.separator + paintBrushNames[indexOfRemoval]).delete();
				}
			}
		}
		
		Integer [] intArray = getPaintList();
		paintBox = new JComboBox(intArray);
		paintBox.setFont(MipavUtil.font12);
	
		paintBox.setRenderer(new PaintBoxRenderer()); 
		paintBox.addItemListener(this);
		paintBox.addMouseListener(popupListener);
		
		paintBox.setSelectedIndex(2);
		
		parent.add(paintBox, index);
		parent.setVisible(true);
    }
    
    private Integer [] getPaintList() {
    	Integer [] intArray = null;
		
        int numBrushes = NUM_BRUSHES_INTERNAL; //built in... 5 so far
        
		File brushesDir = new File(USER_BRUSHES);
		if (brushesDir.isDirectory()) {
			File [] brushes = brushesDir.listFiles();
			
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
        paintBrushNames[1] = "square 4x4.gif";
        paintBrushNames[2] = "square 8x8.gif";
        paintBrushNames[3] = "square 16x16.gif";
        paintBrushNames[4] = "square 24x24.gif";
        
        if (brushesDir.isDirectory()) {
			File [] brushes = brushesDir.listFiles();
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


    /**
     * Helper class used to build a button with user specified color.
     */
    public class JVOIToolBar extends JToolBar {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 454439928707152408L;

        /** The voiColor button. */
        private JButton voiColor;

        /**
         * Constructs a new button with a color based on the index.
         *
         * @param  voiIndex  controls the color of the button.
         */
        public JVOIToolBar(int voiIndex) {
            super();
            voiColor = new JButton(MipavUtil.getIcon("transparent.gif"));
            setVOIColor(voiIndex);
            voiColor.setToolTipText("Current/change VOI Color");
        }

        /**
         * Gets the button.
         *
         * @return  JButton the button of a specified color
         */
        public JButton getVOIColorButton() {
            return voiColor;
        }

        /**
         * Sets the color fo the button based on an index. A difference of 1 between indexes will produce a noticable
         * change in the color (hue) of the button.
         *
         * @param  voiIndex  The index.
         */
        public void setVOIColor(int voiIndex) {

            if (voiIndex == -1) {
                voiIndex = 0;
            }

            float hue = (float) ((((voiIndex + Preferences.getVOIColorIncrement()) * 35) % 360) / 360.0);
            Color color = Color.getHSBColor(hue, (float) 1.0, (float) 1.0);

            // System.err.println("Color is: R=" + color.getRed() + ", G=" + color.getGreen() + ", B=" +
            // color.getBlue());
            voiColor.setBackground(color);
            voiColor.setForeground(color);
        }

        /**
         * Sets the buttons background and foreground to a specified color.
         *
         * @param  newColor  Color
         */
        public void setVOIColor(Color newColor) {
            voiColor.setBackground(newColor);
            voiColor.setForeground(newColor);
        }
    }
    
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
        			res = new File(USER_BRUSHES + File.separator + paintBrushNames[selectedIndex]).toURL();
        			icon = new ImageIcon(res);
                	
        			if (icon.getIconHeight() >= 20 || icon.getIconWidth() >= 20) {
        				int newWidth, newHeight;
        				if (icon.getIconHeight() < icon.getIconWidth()) {
        					newWidth = 20;
        					float factor = 24f/icon.getIconWidth();
        					newHeight = (int)(icon.getIconHeight() * factor);
        				} else {
        					newHeight = 20;
        					float factor = 24f/icon.getIconHeight();
        					newWidth = (int)(icon.getIconWidth() * factor);
        				}
        				icon = new ImageIcon(icon.getImage().getScaledInstance(newWidth, newHeight, 0));
        			}
        		} catch (Exception e) {
        			//e.printStackTrace();
        		}
        	}
        	          
            
        	setText(paintBrushNames[selectedIndex].substring(0, paintBrushNames[selectedIndex].lastIndexOf(".")));
        	setIcon(icon);
        	
        	setPreferredSize(new Dimension(90,24));
        	setIconTextGap(10);
        	setHorizontalTextPosition(LEFT);
        	
        	return this;
        }
    }
        
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
