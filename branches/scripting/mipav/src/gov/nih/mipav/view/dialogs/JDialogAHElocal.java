package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image.
 *
 * <p>In should be noted, that the algorithms are executed in their own thread.</p>
 *
 * @version  1.0; 17 September 2001
 * @author   parsonsd
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogAHElocal extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6607139120847474768L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmAHElocal aheAlgo = null;

    /** DOCUMENT ME! */
    private boolean blue = true;

    /** DOCUMENT ME! */
    private JCheckBox blueChannel;

    /** DOCUMENT ME! */
    private boolean clamp = false;

    /** DOCUMENT ME! */
    private JCheckBox clampCheckBox;

    /** DOCUMENT ME! */
    private JLabel clampLabel;

    /** DOCUMENT ME! */
    private int clampValue = 75; // If Use clamping checked, defaults to 75%.

    /** DOCUMENT ME! */
    private JTextField clampValueText;

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelShape;

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelSize;

    /** DOCUMENT ME! */
    private JComboBox comboBoxScaleMax;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** black and white and "Image" for color. */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private boolean green = true;

    /** DOCUMENT ME! */
    private JCheckBox greenChannel;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** or if the source image is to be replaced. */
    private ButtonGroup imageVOIgroup;

    /** DOCUMENT ME! */
    private boolean isColorImage = false; // indicates the image is a color image

    /** DOCUMENT ME! */
    private int kernelShape;

    /** DOCUMENT ME! */
    private int kernelSize; // divide image into number of images (say, into threes)

    /** DOCUMENT ME! */
    private JLabel minThresholdLabel;

    /** DOCUMENT ME! */
    private JTextField minThresholdText;

    /** DOCUMENT ME! */
    private float minThresholdValue;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private boolean red = true;

    /** DOCUMENT ME! */
    private JCheckBox redChannel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int scaleMaxValue = 1; // defaults to "Slice" for

    /** DOCUMENT ME! */
    private boolean threshold = false;

    /** DOCUMENT ME! */
    private JCheckBox thresholdCheckBox;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    /** DOCUMENT ME! */
    private boolean wholeImageSelected = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAHElocal() { }

    /**
     * Creates a new JDialogAHElocal object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogAHElocal(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (im.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }

        if (im.isColorImage()) {
            isColorImage = true;
        }

        image = im;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogAHElocal(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();

        if (im.isColorImage()) {
            isColorImage = true;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source == clampCheckBox) {

            if (clampCheckBox.isSelected()) {
                clampValueText.setEnabled(true);
                clampLabel.setForeground(Color.black);
                clampValueText.setText(Integer.toString(clampValue));
            } else {
                clampValueText.setEnabled(false);
                clampLabel.setForeground(Color.gray);

                try {
                    clampValue = Integer.parseInt(clampValueText.getText());
                } catch (NumberFormatException npe) {

                    // user emptied text, then clicked checkbox.
                    clampValue = 75;
                }

                clampValueText.setText("100");
            }
        } else if (source == thresholdCheckBox) {

            if (thresholdCheckBox.isSelected()) {
                minThresholdText.setEnabled(true);
                minThresholdLabel.setForeground(Color.black);
            } else {
                minThresholdText.setEnabled(false);
                minThresholdLabel.setForeground(Color.gray);
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10051");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmAHElocal) {
            image.clearMask();

            if (aheAlgo.isCompleted() && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();
                // String shape;
                // if (kernelShape == 0)
                // shape = new String("Square");
                // else
                // shape = new String("Cross");

                try {

                    // if (clamp) {
                    // resultImage.setImageName("CLAHElocal ("+shape+", "+kernelSize+", "+clampValue+"%): "
                    // +image.getImageName());
                    // }
                    // else {      // not clipping the histogram
                    // resultImage.setImageName("AHElocal ("+shape+", "+kernelSize+"): " +image.getImageName());
                    // }
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));

                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {
                // String shape; if (kernelShape == 0)    shape = new String("Square"); else    shape = new
                // String("Cross");
                //
                // if (clamp) {    image.setImageName("CLAHElocal ("+shape+", "+kernelSize+", "+ clampValue+ "%): "
                //        +image.getImageName()); } else {      // not clipping the histogram
                // image.setImageName("AHElocal("+shape+", "+kernelSize+"): " +image.getImageName()); }

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }
        }

        insertScriptLine(aheAlgo);
        aheAlgo.finalize();
        aheAlgo = null;
        dispose();
        System.gc();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("AHElocal " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " " + clamp + " " + clampValue + " " + kernelSize + " " +
                                                           kernelShape + " " + threshold + " " + minThresholdValue +
                                                           " " + wholeImageSelected + " " + scaleMaxValue + " " + red +
                                                           " " + green + " " + blue + "\n");
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                           " " + clamp + " " + clampValue + " " + kernelSize + " " +
                                                           kernelShape + " " + threshold + " " + minThresholdValue +
                                                           " " + wholeImageSelected + " " + scaleMaxValue + " " + red +
                                                           " " + green + " " + blue + "\n");
                }
            }
        }
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        if (im.isColorImage()) {
            isColorImage = true;
        }

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        if (srcImageKey.equals(destImageKey)) {
            this.setDisplayLocReplace();
        } else {
            this.setDisplayLocNew();
        }

        try {
            setClampFlag(parser.getNextBoolean());
            setClampingValue(parser.getNextInteger());
            setKernelSize(parser.getNextInteger());
            setKernelShape(parser.getNextInteger());
            setThresholdFlag(parser.getNextBoolean());
            setThresholdValue(parser.getNextFloat());
            setWholeImageSelectedFlag(parser.getNextBoolean());
            setScaleMaxValue(parser.getNextInteger());
            setRGBChannelFilter(parser.getNextBoolean(), parser.getNextBoolean(), parser.getNextBoolean());
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }
        
        setSeparateThread(false);
        callAlgorithm();

        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }

    /**
     * Accessor that sets the clamp flag.
     *
     * @param  flag  <code>true</code> indicates clamp, <code>false</code> otherwise.
     */
    public void setClampFlag(boolean flag) {
        clamp = flag;
    }

    /**
     * Accessor that sets the clamping value.
     *
     * @param  value  Value to set clamping to.
     */
    public void setClampingValue(int value) {
        clampValue = value;
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * Accessor that sets the kernel shape.
     *
     * @param  value  Value to set kernel shape to.
     */
    public void setKernelShape(int value) {
        kernelShape = value;
    }

    /**
     * Accessor that sets the kernel size.
     *
     * @param  value  Value to set kernel size to.
     */
    public void setKernelSize(int value) {
        kernelSize = value;
    }

    /**
     * RGB images are histogram equalized by 'channel.' That is, each color, red, blue and green, is run independently
     * of the other two colors. This permits selectively filtering any combination of the three channels instead of
     * simply trying to handle all three at once. True filters that channel.
     *
     * @param  r  DOCUMENT ME!
     * @param  g  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     */
    public void setRGBChannelFilter(boolean r, boolean g, boolean b) {
        red = r;
        green = g;
        blue = b;
    }

    /**
     * Accessor that sets the max scale value (from the combo box).
     *
     * @param  value  Value to set max scale value to - local, slice, or image.
     */
    public void setScaleMaxValue(int value) {
        scaleMaxValue = value;
    }

    /**
     * Accessor that sets the threshold flag.
     *
     * @param  flag  <code>true</code> indicates threshold, <code>false</code> otherwise.
     */
    public void setThresholdFlag(boolean flag) {
        threshold = flag;
    }

    /**
     * Accessor that sets the minThreshold value.
     *
     * @param  value  Value to set minThresholdValue to.
     */
    public void setThresholdValue(float value) {
        minThresholdValue = value;
    }

    /**
     * Accessor that sets the wholeImageSelected flag.
     *
     * @param  flag  <code>true</code> indicates wholeImageSelected, <code>false</code> otherwise.
     */
    public void setWholeImageSelectedFlag(boolean flag) {
        wholeImageSelected = flag;
    }

    /**
     * controls the entire creation of the clamping panel and all controls related to the display of the clamping
     * parameter. places the clamping panel into the <i>holder</i> panel.
     *
     * @param  holder  DOCUMENT ME!
     * @param  gbc     DOCUMENT ME!
     * @param  gbl     DOCUMENT ME!
     */
    protected void buildClamping(JPanel holder, GridBagConstraints gbc, GridBagLayout gbl) {
        clampCheckBox = new JCheckBox("Use clamping - ");
        clampCheckBox.addActionListener(this);
        clampCheckBox.setFont(serif12);
        clampCheckBox.setSelected(false);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 0;
        gbl.setConstraints(clampCheckBox, gbc);
        holder.add(clampCheckBox);

        clampLabel = new JLabel("Fraction of most frequent pixel intensity (%)");
        clampLabel.setFont(serif12);
        clampLabel.setForeground(Color.gray);
        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.weightx = 1;
        gbl.setConstraints(clampLabel, gbc);
        holder.add(clampLabel);
        // clampingPanel.add(clampLabel);

        clampValueText = new JTextField("100"); // preset to no clamping
        clampValueText.setHorizontalAlignment(JTextField.RIGHT);
        makeNumericsOnly(clampValueText);
        clampValueText.setMaximumSize(clampValueText.getPreferredSize()); // don't let it get any bigger than what it
                                                                          // prefers
        clampValueText.setFont(serif12);
        clampValueText.setColumns(4);
        clampValueText.setEnabled(false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        gbl.setConstraints(clampValueText, gbc);
        holder.add(clampValueText);
        // clampingPanel.add(clampValueText);

        // return clampingPanel;
    }

    /**
     * makes the color panel.
     *
     * @param   gbc  DOCUMENT ME!
     * @param   gbl  DOCUMENT ME!
     *
     * @return  the color panel with adjustable attributes already added
     */
    protected JPanel buildColorPanel(GridBagConstraints gbc, GridBagLayout gbl) {
        JPanel colorPanel = new JPanel();
        colorPanel.setLayout(gbl);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        colorPanel.setForeground(Color.black);

        // set the border ... "Color channel Selection"
        colorPanel.setBorder(buildTitledBorder("Color channel selection"));

        redChannel = new JCheckBox("Red channel", true);
        redChannel.setFont(serif12);
        gbl.setConstraints(redChannel, gbc);
        colorPanel.add(redChannel);

        greenChannel = new JCheckBox("Green channel", true);
        greenChannel.setFont(serif12);
        gbl.setConstraints(greenChannel, gbc);
        colorPanel.add(greenChannel);

        blueChannel = new JCheckBox("Blue channel", true);
        blueChannel.setFont(serif12);
        gbl.setConstraints(blueChannel, gbc);
        colorPanel.add(blueChannel);

        // if not a color image, block access to the channel switches
        // since they don't mean anything
        if (!isColorImage) {
            redChannel.setEnabled(false);
            greenChannel.setEnabled(false);
            blueChannel.setEnabled(false);
        }

        colorPanel.setToolTipText("Color images can be filtered over any combination of color channels");

        return colorPanel;
    }

    /**
     * builds the panel which allows user to select which where the algorithm is to display the output, or resultant,
     * image:
     *
     * <ul>
     *   <li>create a new image</li>
     *   <li>replace the old image</li>
     * </ul>
     *
     * @return  a fully populated JPanel.
     */
    protected JPanel buildDestinationPanel() {

        // destination goes in the left of the lower box
        JPanel destinationPanel = new JPanel();
        Box destinationBox = new Box(BoxLayout.Y_AXIS);

        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage); // add the button to the grouping
        destinationBox.add(newImage); // add the button to the component

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage); // add the button to the grouping
        destinationBox.add(replaceImage); // add the button to the component
        destinationPanel.add(destinationBox);

        return destinationPanel;
    }

    /**
     * Creates the comboBox that allows user to define the way the neighboring pixels are chosen for the histogram. The
     * kernel-shape may be
     *
     * <ul>
     *   <li>square</li>
     *   <li>cross (shaped like a '+')</li>
     * </ul>
     *
     * <p>builds the label to be used, applies it to the left side of the JPanel <i>holder</i>, then builds the drop-box
     * so the user may make a selection, and applies it to the far-right of the JPanel <i>holder</i>.</p>
     *
     * @param  holder  DOCUMENT ME!
     * @param  gbc     DOCUMENT ME!
     * @param  gbl     DOCUMENT ME!
     */
    protected void buildKernelShape(JPanel holder, GridBagConstraints gbc, GridBagLayout gbl) {
        JLabel labelKernelShape = new JLabel("Kernel shape:");
        labelKernelShape.setForeground(Color.black);
        labelKernelShape.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbl.setConstraints(labelKernelShape, gbc);
        holder.add(labelKernelShape); // add kernel label

        comboBoxKernelShape = new JComboBox();
        comboBoxKernelShape.setFont(serif12);
        comboBoxKernelShape.setBackground(Color.white);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        gbl.setConstraints(comboBoxKernelShape, gbc);

        // ITEMS
        comboBoxKernelShape.addItem("Square");
        comboBoxKernelShape.addItem("Cross");
        holder.add(comboBoxKernelShape); // add kernel combo box
    }

    /**
     * Creates the editable text comboBox that allows user to define the number of neighboring pixels used for the
     * histogram. the number given may only be odd, as this is the total number of pixels chosen on both sides. For any
     * pixel, there are floor(<code>kernelSize</code>/2) pixels in either the horizontal or vertical direction.
     * Alternatively, it describes a box of the number here on a side, for which the pixel in question is at the center.
     * The drop-box has pre-selected:
     *
     * <ul>
     *   <li>21</li>
     *   <li>45</li>
     *   <li>65</li>
     *   <li>129</li>
     * </ul>
     *
     * but any odd figure could be entered.
     *
     * <p>builds the label to be used, applies it to the left side of the JPanel <i>holder</i>, then builds the drop-box
     * so the user may make a selection, and applies it to the far-right of the JPanel <i>holder</i>.</p>
     *
     * @param  holder  DOCUMENT ME!
     * @param  gbc     DOCUMENT ME!
     * @param  gbl     DOCUMENT ME!
     */
    protected void buildKernelSize(JPanel holder, GridBagConstraints gbc, GridBagLayout gbl) {
        JLabel labelKernelSize = new JLabel("Kernel size:");
        labelKernelSize.setForeground(Color.black);
        labelKernelSize.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbl.setConstraints(labelKernelSize, gbc);
        holder.add(labelKernelSize); // add kernel label

        comboBoxKernelSize = new JComboBox();
        comboBoxKernelSize.setEditable(true);

        JTextField fld = (JTextField) comboBoxKernelSize.getEditor().getEditorComponent();
        makeNumericsOnly(fld);
        fld.setHorizontalAlignment(JTextField.RIGHT);
        comboBoxKernelSize.setFont(serif12);
        comboBoxKernelSize.setBackground(Color.white);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        gbl.setConstraints(comboBoxKernelSize, gbc);

        // ITEMS
        comboBoxKernelSize.addItem("21");
        comboBoxKernelSize.addItem("45");
        comboBoxKernelSize.addItem("65");
        comboBoxKernelSize.addItem("129");
        holder.add(comboBoxKernelSize); // add the combobox to the panel
    }

    /**
     * builds the panel which allows user to select which portion of the image the algorithm is to operate on:
     *
     * <ul>
     *   <li>the entire image</li>
     *   <li>the currently highlighted volume of interest</li>
     * </ul>
     * Although specified elsewhere, it should be noted that if VOI is chosen, although no currently selected VOI
     * exists, an error notice will be posted to the user at the time of algorithm indicating that no VOI had been
     * selected.
     *
     * @return  a fully populated JPanel.
     */
    protected JPanel buildRegionSelectPanel() {

        // filter goes in the right of the lower box
        JPanel imageVOIPanel = new JPanel();
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Filter"));

        Box imageVOIBox = new Box(BoxLayout.Y_AXIS);
        imageVOIgroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIgroup.add(wholeImage); // add the button to the grouping
        imageVOIBox.add(wholeImage); // add the button to the component

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIgroup.add(VOIRegions); // add the button to the grouping
        imageVOIBox.add(VOIRegions); // add the button to the component

        imageVOIPanel.add(imageVOIBox); // place the box onto a border

        return imageVOIPanel;
    }

    /**
     * define the possibilities of where the scale max comes from.
     *
     * @param  holder  DOCUMENT ME!
     * @param  gbc     DOCUMENT ME!
     * @param  gbl     DOCUMENT ME!
     */
    protected void buildScaleMax(JPanel holder, GridBagConstraints gbc, GridBagLayout gbl) {
        JLabel labelScaleMax = new JLabel("Use as scale maximum:");
        labelScaleMax.setForeground(Color.black);
        labelScaleMax.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbl.setConstraints(labelScaleMax, gbc);
        holder.add(labelScaleMax); // add kernel label

        comboBoxScaleMax = new JComboBox();
        comboBoxScaleMax.setFont(serif12);
        comboBoxScaleMax.setBackground(Color.white);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        gbl.setConstraints(comboBoxScaleMax, gbc);

        // ITEMS
        comboBoxScaleMax.addItem("Local");
        comboBoxScaleMax.addItem("Slice");
        comboBoxScaleMax.addItem("Image");

        if (isColorImage) {
            comboBoxScaleMax.setSelectedIndex(2);
            scaleMaxValue = 2;
        } else {
            comboBoxScaleMax.setSelectedIndex(1);
            scaleMaxValue = 1;
        }

        holder.add(comboBoxScaleMax);
    }

    /**
     * Creates the txt-box that allows user to insert the bottom-end pixels to skip. Number entered into the textfield
     * is minimum value of the pixels considered. Defaults to the image minimum.
     *
     * @param  holder  DOCUMENT ME!
     * @param  gbc     DOCUMENT ME!
     * @param  gbl     DOCUMENT ME!
     */
    protected void buildThreshold(JPanel holder, GridBagConstraints gbc, GridBagLayout gbl) {

        if (isColorImage) {
            minThresholdValue = (float) Math.min(image.getMinR(), Math.min(image.getMinG(), image.getMinB()));
        } else {
            minThresholdValue = (float) image.getMin();
        }

        thresholdCheckBox = new JCheckBox("Use threshold - ");
        thresholdCheckBox.addActionListener(this);
        thresholdCheckBox.setFont(serif12);
        thresholdCheckBox.setSelected(false);
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(thresholdCheckBox, gbc);
        holder.add(thresholdCheckBox);
        // threshPanel.add(thresholdCheckBox);

        minThresholdLabel = new JLabel("Evaluate pixels when >= ");
        minThresholdLabel.setForeground(Color.gray);
        minThresholdLabel.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.RELATIVE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbl.setConstraints(minThresholdLabel, gbc);
        holder.add(minThresholdLabel); // add kernel label

        minThresholdText = new JTextField(String.valueOf(minThresholdValue));
        minThresholdText.setFont(serif12);
        minThresholdText.setHorizontalAlignment(JTextField.RIGHT);
        minThresholdText.setColumns(8);
        minThresholdText.setBackground(Color.white);
        minThresholdText.setEnabled(false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        gbl.setConstraints(minThresholdText, gbc);
        makeFloatingPointOnly(minThresholdText);
        holder.add(minThresholdText);
    }

    /**
     * Takes a text field, and forces the textfield to accept numbers, backspace and delete-key entries.
     *
     * @param  txt  Text field to modify.
     */
    protected void makeFloatingPointOnly(JTextField txt) {
        txt.addKeyListener(new KeyAdapter() { // make the field
                public void keyTyped(KeyEvent evt) { // not accept letters

                    JTextField t = (JTextField) evt.getComponent();
                    char ch = evt.getKeyChar();

                    if (ch == '.') {

                        if (t.getSelectedText() != null) {

                            if (t.getText().length() == t.getSelectedText().length()) {
                                t.setText("0");
                            } else if ((t.getText().indexOf('.') != -1) // there is a '.', but no
                                           && (t.getSelectedText().indexOf('.') == -1)) { // in the selected text
                                evt.consume(); // ignore
                            }
                        } else if (t.getText().indexOf('.') != -1) {
                            evt.consume();
                        } else if (t.getText().length() == 0) {
                            t.setText("0");
                        }
                    } else if (ch == '-') {
                        String text = t.getText().trim();
                        int minusPlace = text.indexOf('-');

                        if (minusPlace != -1) { // text does has a '-'
                            text = text.substring(minusPlace + 1); // only text after '-'...
                            t.setText(text);
                        } else {
                            t.setText("-" + text);
                        }

                        evt.consume();
                    } else if (((ch < '0') || (ch > '9')) &&
                                   ((ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE))) {

                        // if is the case that ch is outside the bounds of a number AND it is the case that ch is
                        // neither a BS or a DE, then... key is not a digit or a deletion char
                        evt.consume();
                    }
                }
            });
    }

    /**
     * Takes a text field, and forces the textfield to accept numbers, backspace and delete-key entries.
     *
     * @param  txt  Text field to modify.
     */
    protected void makeNumericsOnly(JTextField txt) {
        txt.addKeyListener(new KeyAdapter() { // make the field
                public void keyTyped(KeyEvent evt) { // not accept letters

                    JTextField t = (JTextField) evt.getComponent();
                    char ch = evt.getKeyChar();

                    if (((ch < '0') || (ch > '9')) && ((ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE))) {

                        // if is the case that ch is outside the bounds of a number
                        // AND it is the case that ch is neither a BS or a DE, then...
                        // key is not a digit or a deletion char
                        evt.consume();
                    }
                }
            });
    }

    /**
     * Creates the panel that allow user to select the kernel size and shape of the image when building the histogram.
     * Contains clamping attributes.
     *
     * @param   gbc  DOCUMENT ME!
     * @param   gbl  DOCUMENT ME!
     *
     * @return  the panel that allows setting kernel attributes
     */
    private JPanel buildKernelPanel(GridBagConstraints gbc, GridBagLayout gbl) {

        // paramPanel holds all the "Parameters"
        JPanel paramPanel = new JPanel();

        // panel gets a grid layout
        paramPanel.setLayout(gbl);

        // panel is titled & etched
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        buildKernelSize(paramPanel, gbc, gbl);
        buildKernelShape(paramPanel, gbc, gbl);
        buildScaleMax(paramPanel, gbc, gbl);

        buildThreshold(paramPanel, gbc, gbl);
        buildClamping(paramPanel, gbc, gbl);

        return paramPanel;
    }

    /**
     * Once all the necessary variables are set, call the local AHE algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     *
     * @see  AlgorithmAHElocal
     */
    private void callAlgorithm() {

        String name = makeImageName(image.getImageName(), "_LocalAHE");
        int[] destExtents = new int[image.getNDims()];

        for (int i = 0; i < image.getNDims(); i++) {
            destExtents[i] = image.getExtents()[i];
        } // E[i] dim

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                // resultImage = new ModelImage(image.getType(), destExtents,  image.getImageName(),userInterface);
                resultImage = (ModelImage) image.clone(name);

                if (resultImage.getNDims() == 2) {

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0002",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0008,0016",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26);
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
                    }
                } else {

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0002",
                                                                                    "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0008,0016",
                                                                                    "1.2.840.10008.5.1.4.1.1.7 ", 26);
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0012", "1.2.840.34379.17",
                                                                                    16); // bogus Implementation UID
                                                                                         // made up by Matt
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0013", "MIPAV--NIH", 10); //
                        }
                    }
                }

                // Make algorithm
                aheAlgo = new AlgorithmAHElocal(resultImage, image, kernelSize, kernelShape, wholeImageSelected);
                aheAlgo.setRGBChannelFilter(red, green, blue);

                if (clamp) {
                    aheAlgo.setContrastLimited(true);
                    aheAlgo.setClipLevel(clampValue);
                }

                aheAlgo.setHistogramScaleRule(scaleMaxValue);

                if (threshold) {
                    aheAlgo.setThresholdImage(true);
                    aheAlgo.setMinimumThreshold(minThresholdValue);
                }

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                aheAlgo.addListener(this);
                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (aheAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    if (!userInterface.isAppFrameVisible()) {
                        aheAlgo.setProgressBarVisible(false);
                    }

                    aheAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Adaptive Histogram Equalization local: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else { // displayLoc == REPLACE

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                aheAlgo = new AlgorithmAHElocal(image, kernelSize, kernelShape, wholeImageSelected);
                aheAlgo.setRGBChannelFilter(red, green, blue);

                if (clamp) {
                    aheAlgo.setContrastLimited(true);
                    aheAlgo.setClipLevel(clampValue);
                }

                aheAlgo.setHistogramScaleRule(scaleMaxValue);

                if (threshold) {
                    aheAlgo.setThresholdImage(true);
                    aheAlgo.setMinimumThreshold(minThresholdValue);
                }

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                aheAlgo.addListener(this);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (aheAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    if (!userInterface.isAppFrameVisible()) {
                        aheAlgo.setProgressBarVisible(false);
                    }

                    aheAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog AHE local: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Local Adaptive Histogram Equalization");

        // place everything setting up the equalisation into the setupBox
        Box setupBox = new Box(BoxLayout.Y_AXIS);
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)

        setupBox.add(buildKernelPanel(gbc, gbl));
        setupBox.add(buildColorPanel(gbc, gbl));

        Box lowerBox = new Box(BoxLayout.X_AXIS);
        lowerBox.add(buildDestinationPanel());
        lowerBox.add(buildRegionSelectPanel());
        setupBox.add(lowerBox); // place lowerBox into the setupBox
        mainDialogPanel.add(setupBox, BorderLayout.CENTER);

        // OK, Cancel, Help:
        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        JTextField fld;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        red = redChannel.isEnabled() && redChannel.isSelected();
        green = greenChannel.isEnabled() && greenChannel.isSelected();
        blue = blueChannel.isEnabled() && blueChannel.isSelected();

        if (clampCheckBox.isSelected()) {

            try {

                if ((Integer.parseInt(clampValueText.getText()) >= 100) ||
                        (Integer.parseInt(clampValueText.getText()) < 1)) {
                    MipavUtil.displayError("Value must be >= 1 && <= 99");
                    clampValueText.requestFocus();
                    clampValueText.selectAll();

                    return false;
                } else {
                    clamp = clampCheckBox.isSelected();
                    clampValue = Integer.parseInt(clampValueText.getText());
                }
            } catch (NumberFormatException nfe) { // when asked to clamp but text field is left blank
                MipavUtil.displayError("Clipping value must be >= 1 && <= 99");
                clampValueText.requestFocus();
                clampValueText.selectAll();

                return false;
            }
        }

        if (thresholdCheckBox.isSelected()) {

            try {
                threshold = true;
                minThresholdValue = Float.parseFloat(minThresholdText.getText());
            } catch (NumberFormatException nfe) { // when asked to clamp but text field is left blank
                MipavUtil.displayError("Invalid Number.");
                minThresholdText.requestFocus();
                minThresholdText.selectAll();

                return false;
            }
        } else {
            threshold = false;
        }

        try {
            kernelShape = comboBoxKernelShape.getSelectedIndex();
            kernelSize = Integer.parseInt((String) comboBoxKernelSize.getSelectedItem());

            if (kernelSize <= 1) {
                MipavUtil.displayError("Too small a kernel size; choose a size larger than 1.");
                fld = (JTextField) (comboBoxKernelSize.getEditor().getEditorComponent());
                fld.requestFocus();
                fld.selectAll();

                return false;
            } else if ((kernelSize % 2) == 0) { // even number
                MipavUtil.displayError("Even-length size; choose a size of odd length.");
                fld = (JTextField) (comboBoxKernelSize.getEditor().getEditorComponent());
                fld.requestFocus();
                fld.selectAll();

                return false;
            }
        } catch (NumberFormatException nfe) {
            MipavUtil.displayError("Kernel size must be a number");

            return false;
        }

        if (wholeImage.isSelected()) {
            wholeImageSelected = true;
        } else {
            wholeImageSelected = false;
        }

        // get the max scale value
        scaleMaxValue = comboBoxScaleMax.getSelectedIndex();

        return true;
    }

}
