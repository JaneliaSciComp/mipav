package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * JDialogLoadImage allows the MIPAV user to import an image (namely, ImageA) from another image frame. The titles of
 * other images are listed in a drop-down combo-box, as they are found by MIPAV when the dialog is opened. The dialog is
 * modal and the okay button will not be available if there is only one frame open (the only way to bring up the
 * dialog). The dialog will not discriminate between frames which have the same names, taking the first frame it finds
 * which the selected name.
 *
 * <p>This class has the option to include a "Browse Files..." button; however, this functionality has not been
 * completely implemented.</p>
 *
 * @author   David Parsons
 * @version  1.00
 */
public class JDialogLoadImage extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int LOAD_FROM_FRAME = 0;

    /** DOCUMENT ME! */
    public static final int LOAD_FROM_FILE = 1;

    /** DOCUMENT ME! */
    public static final int LOAD_BLANK = 2;

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8455082719816268161L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton browseButton;

    /** If blank image is to be loaded...need the type (for non-color images). */
    private int dataType;

    /** DOCUMENT ME! */
    private boolean doOrigins = true, doOrients = true;

    /** Source image. */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JComboBox imageChooser;

    /** image taken from the frame to be imported:. */
    private ModelImage importImage;

    /** are we loading from frame, file, or blank. */
    private int loadType;

    /** DOCUMENT ME! */
    private JCheckBox matchOrigins, matchOrients;

    /** DOCUMENT ME! */
    private JPanel picListingPanel;

    /** DOCUMENT ME! */
    private JRadioButton radioBool;

    /** DOCUMENT ME! */
    private JRadioButton radioByte;

    /** DOCUMENT ME! */
    private JRadioButton radioDouble;

    /** DOCUMENT ME! */
    private JRadioButton radioFloat;

    /** DOCUMENT ME! */
    private JRadioButton radioInt;

    /** DOCUMENT ME! */
    private JRadioButton radioLong;

    /** DOCUMENT ME! */
    private JRadioButton radioShort;

    /** DOCUMENT ME! */
    private JRadioButton radioUByte;

    /** DOCUMENT ME! */
    private JRadioButton radioUInt;

    /** DOCUMENT ME! */
    private JRadioButton radioUShort;

    /** a cloned version of importImage that will be inserted into the source images's B slot. */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty Constructor for script.
     */
    public JDialogLoadImage() { }

    /**
     * Creates a new JDialogLoadImage object.
     *
     * @param  parentFrame  DOCUMENT ME!
     * @param  srcImage     DOCUMENT ME!
     * @param  type         DOCUMENT ME!
     */
    public JDialogLoadImage(Frame parentFrame, ModelImage srcImage, int type) {
        super(parentFrame, true);
        image = srcImage;
        this.loadType = type;

        if (loadType == LOAD_FROM_FRAME) {
            init();
        } else if (loadType == LOAD_BLANK) {
            doOrigins = false;
            doOrients = false;

            if (image.isColorImage()) {
                dataType = image.getType();
                callAlgorithm();
            } else {
                init();
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * when a button is clicked.
     *
     * @param  ae  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent ae) {
        String command = ae.getActionCommand();

        if (command.equalsIgnoreCase("ok")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equalsIgnoreCase("browse")) {
            dispose();
            MipavUtil.displayError("Browse files...  Not Yet Supported.");
        } else if (command.equalsIgnoreCase("cancel")) {
            dispose();
        }
    }

    /**
     * adds the "Browse Files ...." button to the right of the panel.
     */
    public void addBrowseFilesButton() {
        browseButton = new JButton("Browse...");
        browseButton.setActionCommand("browse");
        browseButton.addActionListener(this);
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        picListingPanel.add(browseButton);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  algo  DOCUMENT ME!
     */
    public void algorithmPerformed(AlgorithmBase algo) {

        if (algo.isCompleted()) {
            insertScriptLine();
        } else {

            // remove the cloned image if there was a failure
            if (resultImage != null) {
                resultImage.disposeLocal();
                resultImage = null;
            }
        }

        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    public void callAlgorithm() {
        AlgorithmLoadB algoLoad = new AlgorithmLoadB();
        algoLoad.addListener(this);
        createProgressBar(image.getImageName(), algoLoad);

        algoLoad.run();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getMatchOrients() {
        return doOrients;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getMatchOrigins() {
        return doOrigins;
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        loadType = scriptParameters.getParams().getInt("load_type");

        if (loadType == LOAD_FROM_FRAME) {
            importImage = scriptParameters.retrieveImage("image_to_import");
        } else if (loadType == LOAD_BLANK) {
            dataType = scriptParameters.getParams().getInt("data_type");

            // check to see if there is a mismatch between color/non color datatypes
            if ((image.isColorImage() && !ModelImage.isColorImage(dataType)) ||
                    (!image.isColorImage() && ModelImage.isColorImage(dataType))) {
                dataType = image.getType();
            }

        }

        doOrigins = scriptParameters.getParams().getBoolean("do_match_origins");
        doOrients = scriptParameters.getParams().getBoolean("do_match_orients");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        if (loadType == LOAD_FROM_FRAME) {
            scriptParameters.storeImage(importImage, "image_to_import");
        } else if (loadType == LOAD_BLANK) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("data_type", dataType));
        }

        AlgorithmParameters.storeImageInRecorder(resultImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("load_type", loadType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_match_origins", doOrigins));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_match_orients", doOrients));
    }

    /**
     * DOCUMENT ME!
     */
    private void init() {

        if (loadType == LOAD_FROM_FRAME) {
            setTitle("Load ImageB onto " + image.getImageName());
            userInterface = ViewUserInterface.getReference();
            picListingPanel = new JPanel();
            picListingPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

            JLabel loading = new JLabel("Set as ImageB: ");
            loading.setFont(MipavUtil.font12);
            loading.setForeground(Color.black);
            picListingPanel.add(loading);
            imageChooser = buildImageComboBox(image);
            imageChooser.setToolTipText("On-screen images");
            picListingPanel.add(imageChooser);
            matchOrients = new JCheckBox("Match orientations of two images.");
            matchOrients.setAlignmentX(Component.LEFT_ALIGNMENT);
            matchOrients.setEnabled(true);
            matchOrients.setSelected(true);
            doOrients = matchOrients.isSelected();
            matchOrigins = new JCheckBox("Use image origin information to align images.");
            matchOrigins.setAlignmentX(Component.LEFT_ALIGNMENT);
            matchOrigins.setEnabled(true);
            matchOrigins.setSelected(true);
            doOrigins = matchOrigins.isSelected();

            Box mainBox = new Box(BoxLayout.Y_AXIS);
            mainBox.setAlignmentX(Component.LEFT_ALIGNMENT);
            mainBox.add(picListingPanel);
            mainBox.add(matchOrients);
            mainBox.add(matchOrigins);
            this.getContentPane().add(mainBox, BorderLayout.NORTH);

            JPanel okCancelPanel = new JPanel();
            buildOKButton();
            okCancelPanel.add(OKButton);

            if (imageChooser.getItemCount() == 0) {
                OKButton.setEnabled(false);
            }

            buildCancelButton();
            okCancelPanel.add(cancelButton);
            this.getContentPane().add(okCancelPanel, BorderLayout.SOUTH);
        } else if (loadType == LOAD_BLANK) {
            setTitle("Load blank image");

            ButtonGroup group1 = new ButtonGroup();
            radioBool = new JRadioButton("Boolean", false);
            radioBool.setFont(serif12);
            group1.add(radioBool);
            radioByte = new JRadioButton("Byte", false);
            radioByte.setFont(serif12);
            group1.add(radioByte);
            radioUByte = new JRadioButton("Unsigned Byte", false);
            radioUByte.setFont(serif12);
            group1.add(radioUByte);
            radioShort = new JRadioButton("Short", true);
            radioShort.setFont(serif12);
            group1.add(radioShort);
            radioUShort = new JRadioButton("Unsigned Short", false);
            radioUShort.setFont(serif12);
            group1.add(radioUShort);
            radioInt = new JRadioButton("Integer", false);
            radioInt.setFont(serif12);
            group1.add(radioInt);
            radioUInt = new JRadioButton("Unsigned Integer", false);
            radioUInt.setFont(serif12);
            group1.add(radioUInt);
            radioLong = new JRadioButton("Long", false);
            radioLong.setFont(serif12);
            group1.add(radioLong);
            radioFloat = new JRadioButton("Float", false);
            radioFloat.setFont(serif12);
            group1.add(radioFloat);
            radioDouble = new JRadioButton("Double", false);
            radioDouble.setFont(serif12);
            group1.add(radioDouble);

            JPanel panelImageType = new JPanel(new GridBagLayout());
            panelImageType.setBorder(buildTitledBorder("Image Type"));

            GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.gridx = 0;
            gbc.gridy = 0;
            panelImageType.add(radioBool, gbc);
            gbc.gridy = 1;
            panelImageType.add(radioByte, gbc);
            gbc.gridy = 2;
            panelImageType.add(radioUByte, gbc);
            gbc.gridy = 3;
            panelImageType.add(radioShort, gbc);
            gbc.gridy = 4;
            panelImageType.add(radioUShort, gbc);
            gbc.gridx = 1;
            gbc.gridy = 0;
            panelImageType.add(radioInt, gbc);
            gbc.gridy = 1;
            panelImageType.add(radioUInt, gbc);
            gbc.gridy = 2;
            panelImageType.add(radioLong, gbc);
            gbc.gridy = 3;
            panelImageType.add(radioFloat, gbc);
            gbc.gridy = 4;
            panelImageType.add(radioDouble, gbc);

            JPanel buttonPanel = new JPanel();
            buildOKButton();
            buttonPanel.add(OKButton);
            buildCancelButton();
            buttonPanel.add(cancelButton);
            mainDialogPanel.add(panelImageType);
            mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);
            getContentPane().add(mainDialogPanel);
        }

        pack();
        setLocation((Toolkit.getDefaultToolkit().getScreenSize().width / 2) - this.getSize().width,
                    (Toolkit.getDefaultToolkit().getScreenSize().height / 2) - this.getSize().height);
        setVisible(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {

        if (loadType == LOAD_FROM_FRAME) {
            Vector uiV = userInterface.getImageFrameVector();
            int elem = -1;

            for (int i = uiV.size() - 1; i >= 0; i--) { // get the first image-index which has this title

                try {

                    if (((ViewJFrameImage) uiV.elementAt(i)).getComponentImage().getActiveImage().getImageName().equals(imageChooser.getSelectedItem())) {
                        elem = i;

                        break;
                    }
                } catch (ClassCastException cce) {
                    // if cast fails, move on..
                }
            }

            if (elem != -1) {
                ViewJFrameImage imageFrame = (ViewJFrameImage) uiV.elementAt(elem);
                importImage = imageFrame.getImageA();

                if (matchOrigins.isSelected()) {
                    doOrigins = true;
                } else {
                    doOrigins = false;
                }

                if (matchOrients.isSelected()) {
                    doOrients = true;
                } else {
                    doOrients = false;
                }
            }

            return true;
        } else if (loadType == LOAD_BLANK) {

            if (radioBool.isSelected()) {
                dataType = ModelStorageBase.BOOLEAN;
            } else if (radioByte.isSelected()) {
                dataType = ModelStorageBase.BYTE;
            } else if (radioUByte.isSelected()) {
                dataType = ModelStorageBase.UBYTE;
            } else if (radioShort.isSelected()) {
                dataType = ModelStorageBase.SHORT;
            } else if (radioUShort.isSelected()) {
                dataType = ModelStorageBase.USHORT;
            } else if (radioInt.isSelected()) {
                dataType = ModelStorageBase.INTEGER;
            } else if (radioUInt.isSelected()) {
                dataType = ModelStorageBase.UINTEGER;
            } else if (radioLong.isSelected()) {
                dataType = ModelStorageBase.LONG;
            } else if (radioFloat.isSelected()) {
                dataType = ModelStorageBase.FLOAT;
            } else if (radioDouble.isSelected()) {
                dataType = ModelStorageBase.DOUBLE;
            } else {
                dataType = ModelStorageBase.BYTE;
            }
        }

        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    private class AlgorithmLoadB extends AlgorithmBase {

        /**
         * DOCUMENT ME!
         */
        public void runAlgorithm() {

            if (loadType == LOAD_BLANK) {
                resultImage = new ModelImage(dataType, image.getExtents(), " Blank");
                ((ViewJFrameImage) parentFrame).setImageB(resultImage);
                ((ViewJFrameImage) parentFrame).setControls();
                ((ViewJFrameImage) parentFrame).setTitle();
                setCompleted(true);
            } else {
                resultImage = (ModelImage) importImage.clone();
                setCompleted(((ViewJFrameImage) parentFrame).setAndLoad(resultImage, doOrigins, doOrients));
            }

        }
    }
}
