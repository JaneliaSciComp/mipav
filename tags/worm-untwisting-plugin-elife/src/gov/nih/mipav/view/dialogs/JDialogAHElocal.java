package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

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
public class JDialogAHElocal extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6607139120847474768L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmAHElocal aheAlgo = null;

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
    private JPanelColorChannels colorPanel;

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelShape;

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelSize;

    /** DOCUMENT ME! */
    private JComboBox comboBoxScaleMax;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

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
    private JPanelAlgorithmOutputOptions outputPanel;

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

        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
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
            //MipavUtil.showHelp("10051");
            MipavUtil.showWebHelp("Histogram_Equalization:_Neighborhood_Adaptive");
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

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
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));

                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {
                // String shape; if (kernelShape == 0)    shape = new String("Square"); else    shape = new
                // String("Cross");
                //
                // if (clamp) {    image.setImageName("CLAHElocal ("+shape+", "+kernelSize+", "+ clampValue+ "%): "
                // +image.getImageName()); } else {      // not clipping the histogram
                // image.setImageName("AHElocal("+shape+", "+kernelSize+"): " +image.getImageName()); }

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

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

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

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
     * <p>but any odd figure could be entered.</p>
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

        if (image.isColorImage()) {
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

        if (image.isColorImage()) {
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
     * Once all the necessary variables are set, call the local AHE algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     *
     * @see  AlgorithmAHElocal
     */
    protected void callAlgorithm() {

        String name = makeImageName(image.getImageName(), "_LocalAHE");
        int[] destExtents = new int[image.getNDims()];

        for (int i = 0; i < image.getNDims(); i++) {
            destExtents[i] = image.getExtents()[i];
        } // E[i] dim

        if (outputPanel.isOutputNewImageSet()) {

            try {

                // Make result image of float type
                // resultImage = new ModelImage(image.getType(), destExtents,  image.getImageName(),userInterface);
                resultImage = (ModelImage) image.clone(name);

                if (resultImage.getNDims() == 2) {

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                    }
                } else {

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }
                }

                // Make algorithm
                aheAlgo = new AlgorithmAHElocal(resultImage, image, kernelSize, kernelShape,
                                                outputPanel.isProcessWholeImageSet());
                aheAlgo.setRGBChannelFilter(colorPanel.isRedProcessingRequested(),
                                            colorPanel.isGreenProcessingRequested(),
                                            colorPanel.isBlueProcessingRequested());

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

                createProgressBar(image.getImageName(), aheAlgo);

                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (aheAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {


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
                aheAlgo = new AlgorithmAHElocal(image, kernelSize, kernelShape, outputPanel.isProcessWholeImageSet());
                aheAlgo.setRGBChannelFilter(colorPanel.isRedProcessingRequested(),
                                            colorPanel.isGreenProcessingRequested(),
                                            colorPanel.isBlueProcessingRequested());

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

                createProgressBar(image.getImageName(), aheAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
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
                    aheAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog AHE local: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the
     * image table). Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {

        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
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
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        colorPanel = new JPanelColorChannels(image);
        scriptParameters.setColorOptionsGUI(colorPanel);

        clamp = scriptParameters.getParams().getBoolean("do_clamping");
        clampValue = scriptParameters.getParams().getInt("clamp_value");
        kernelShape = scriptParameters.getParams().getInt("kernel_type");
        kernelSize = scriptParameters.getParams().getInt("kernel_size");
        minThresholdValue = scriptParameters.getParams().getFloat("min_threshold");
        scaleMaxValue = scriptParameters.getParams().getInt("max_scale");
        threshold = scriptParameters.getParams().getBoolean("threshold");
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * Record the parameters just used to run this algorithm in a script.
     *
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), outputPanel.isOutputNewImageSet());

        scriptParameters.storeProcessWholeImage(outputPanel.isProcessWholeImageSet());

        scriptParameters.storeColorOptions(colorPanel);

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_clamping", clamp));
        scriptParameters.getParams().put(ParameterFactory.newParameter("clamp_value", clampValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_type", kernelShape));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size", kernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_threshold", minThresholdValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_scale", scaleMaxValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
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

        colorPanel = new JPanelColorChannels(image);
        setupBox.add(colorPanel);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        setupBox.add(outputPanel);

        mainDialogPanel.add(setupBox, BorderLayout.CENTER);

        // OK, Cancel, Help:
        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
        //setResizable(false);
        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        JTextField fld;

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

        // get the max scale value
        scaleMaxValue = comboBoxScaleMax.getSelectedIndex();

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Histogram tools");
            }

            public String getDescription() {
                return new String("Applies a Local Adaptive Histogram Equalization filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Local Adaptive Histogram Equalization filter.");
            }

            public String getShortLabel() {
                return new String("AHElocal");
            }

            public String getLabel() {
                return new String("Local Adaptive Histogram Equalization");
            }

            public String getName() {
                return new String("Local Adaptive Histogram Equalization");
            }
        };
    }


    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();




    
        
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterList(AlgorithmParameters.DO_PROCESS_RGB, Parameter.PARAM_BOOLEAN, "true,true,true"));
            table.put(new ParameterBoolean("do_clamping", false));
            table.put(new ParameterInt("clamp_value", 75));
            table.put(new ParameterInt("kernel_type", 0));
            table.put(new ParameterInt("kernel_size", 21));
            table.put(new ParameterFloat("min_threshold", -1024f));
            table.put(new ParameterInt("max_scale",1));
            table.put(new ParameterBoolean("threshold", false));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }


    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }


    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }


}
