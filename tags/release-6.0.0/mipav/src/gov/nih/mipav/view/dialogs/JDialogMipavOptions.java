package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.util.ThreadUtil;

import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.algorithms.OpenCLInfo;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.Preferences.ComplexDisplay;
import gov.nih.mipav.view.Preferences.DefaultDisplay;
import gov.nih.mipav.view.Preferences.InterpolateDisplay;

import java.awt.*;
import java.awt.event.*;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

import javax.swing.*;


/**
 * This dialog contains access to MIPAV preferences.
 * 
 * @author parsonsd
 */
public class JDialogMipavOptions extends JDialogBase implements KeyListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6756915900242085699L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    JLabel[] labels = null;

    /** DOCUMENT ME! */
    private JButton activeColor;

    /** DOCUMENT ME! */
    private JCheckBox checkOnFrameClose;

    /** DOCUMENT ME! */
    private JColorChooser colorChooser;

    /** DOCUMENT ME! */
    private JComboBox crosshairChoices;

    /** DOCUMENT ME! */
    private String[] crosshairNames;

    /** DOCUMENT ME! */
    private JCheckBox debugAlgorithmBox;

    /** DOCUMENT ME! */
    private JCheckBox debugCommsBox;

    /** Multi-Threading Enabled Check Box */
    private JCheckBox multiThreadingEnabledCheckBox;

    /** GPU computing enabled check box */
    private JCheckBox gpuCompEnabledCheckBox;

    /** DOCUMENT ME! */
    private JButton gpuInfoButton;

    /** Dicom Receiver check box */
    private JCheckBox dicomReceiverOnStart;

    /** DOCUMENT ME! */
    private JCheckBox debugFileIOBox;

    /** DOCUMENT ME! */
    private JCheckBox debugMinorBox;

    private JCheckBox debugScriptingBox;

    /** DOCUMENT ME! */
    private JCheckBox dicomCatcher;

    /** DOCUMENT ME! */
    private final JPanel displayColorPanel;

    /** MIPAV global options private JPanel globalChangesPanel;. */
    private final JPanel displayPanel;

    /** DOCUMENT ME! */
    private JCheckBox displaySplash;

    /** DOCUMENT ME! */
    private final JPanel displayUserInterfacePanel;

    /** DOCUMENT ME! */
    private JCheckBox enableLoggingBox;

    /** DOCUMENT ME! */
    private int fileFilter;

    /** DOCUMENT ME! */
    private final JPanel fileMiscPanel;

    /** DOCUMENT ME! */
    private final JPanel filePanel;

    /** DOCUMENT ME! */
    private final JPanel fileSavePanel;

    /** DOCUMENT ME! */
    private JButton filterButton;

    /** Button to lasunch the Edit User Defined File Types Dialog */
    private JButton editUserDefButton;

    /** DOCUMENT ME! */
    private JComboBox fontChooser;

    /** DOCUMENT ME! */
    private String[] fontNames = null;

    /** DOCUMENT ME! */
    private JTextField fontSizeField;

    /** DOCUMENT ME! */
    private JTextField frameRateField;

    /** DOCUMENT ME! */
    private JComboBox imageChooser;

    /** DOCUMENT ME! */
    private JButton logFileButton;

    /** DOCUMENT ME! */
    private String logFilename;

    /** DOCUMENT ME! */
    private JCheckBox provenanceCheckBox;

    private String provenanceFilename;

    private JButton provenanceFileButton;

    private JCheckBox provenanceImageCheckBox;

    /** DOCUMENT ME! */
    private final JPanel otherPanel;

    /** DOCUMENT ME! */
    private JCheckBox performLaxCheck;

    /** DOCUMENT ME! */
    private JComboBox pointVOIChoices;

    /** DOCUMENT ME! */
    private Color preferredActiveColor;

    /** DOCUMENT ME! */
    private String preferredCrosshair;

    /** DOCUMENT ME! */
    private JComboBox quickListLevel;

    /** DOCUMENT ME! */
    private JCheckBox saveAllCheckBox;

    /** DOCUMENT ME! */
    private JCheckBox saveDefaultsCheckBox;

    /** Label before comboBoxSaveMethod */
    private JLabel saveLabel;

    /** Whether to save .img files as selected by dialog or always as analyze, interfile, or nifti. */
    private JComboBox comboBoxSaveImgMethod;

    /** Whether to save .mnc files as selected by dialog or always as minc1 or minc2. */
    private JComboBox comboBoxSaveMncMethod;

    /** DOCUMENT ME! */
    private JCheckBox savePromptOverwriteBox;

    /** DOCUMENT ME! */
    private JCheckBox saveThumbnailCheckBox;

    private JCheckBox flipNIFTIReadCheckBox;

    private JTextField fileTempDirField;

    private JButton fileTempDirBrowseButton;

    /** DOCUMENT ME! */
    private JCheckBox showLineVOIAngleBox;

    private JCheckBox continuousVOIBox;
    
    private ButtonGroup VOIGroup;

    private JRadioButton saveVOILPSButton;
    
    private JRadioButton saveVOIVoxelButton;

    /** DOCUMENT ME! */
    private JCheckBox showOutputWindow;

    /** DOCUMENT ME! */
    private final JTabbedPane tabbedPane;

    /** ui must be set to access the list of images to set image-specfic options (ie,. log mode) */
    private final ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JComboBox voiColorChoices;

    /** DOCUMENT ME! */
    private String[] voiColorNames;

    /** DOCUMENT ME! */
    private Color[] voiColors;

    /** DOCUMENT ME! */
    private JButton voiDrawButton, intensityLabelColorButton, intensityLabelBackgroundButton;

    /** DOCUMENT ME! */
    private Color voiDrawColor, intensityLabelColor, intensityLabelBackgroundColor;
    
    /** Check boxes for whether right and left mouse clicks produce default actions. */
	private JCheckBox doIntensityOnLeftBox, doWinLevOnRightBox;
	
	/** opens images in tiled format **/
	private JCheckBox openImagesInTiledFormatBox;

	/** Gives user choices for displaying complex images. */
    private JPanel displayImagePanel;

    /** The available choices for displaying the numerical values of complex data */
    private JComboBox complexDisplayChoices;
    
    /** Available choices for displaying brightness/color display correlations for pixel values. */
    private JComboBox defaultDisplayChoices;

    /** The check box to indicate whether images are displayed using the log of their magnitude */
    private JCheckBox displayLogMag;
    
    /** border size for active image color **/
    private JComboBox activeImageColorBorderSize;

    /** Whether images are updated in real-time based on histogram changes. */
    private JCheckBox displayHistogram;

    /** Available options for image interpolation */
    private JComboBox interpolateDisplayChoices;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * creates a dialog, builds in the options:
     * 
     * <ol>
     * <li>splash page option</lI>
     * <li>Swing file-dialog option</li>
     * <li>Active image color high-light option</li>
     * <li>Debugging options</li>
     * </ol>
     * 
     * <p>
     * It then builds in the Apply and Close (window) buttons and makes the dialog visible.
     * </p>
     */
    public JDialogMipavOptions() {
        super(ViewUserInterface.getReference().getMainFrame(), false, false);

        userInterface = ViewUserInterface.getReference();

        setTitle(userInterface.getAppTitle().replace(':', ' ') + "Options");

        displayPanel = new JPanel();
        displayUserInterfacePanel = new JPanel();
        displayImagePanel = new JPanel();
        displayColorPanel = new JPanel();
        filePanel = new JPanel();
        fileSavePanel = new JPanel();
        fileMiscPanel = new JPanel();
        otherPanel = new JPanel();

        // panel gets a grid layout
        final GridBagLayout gbl = new GridBagLayout();

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;

        // make the display options
        displayUserInterfacePanel.setLayout(gbl);
        displayUserInterfacePanel.setBorder(buildTitledBorder("User interface"));
        makeSplashOptions(gbc, gbl);
        makeMouseClickOptions(gbc, gbl);
        makeFontOptions(gbc, gbl);
        
        displayImagePanel.setLayout(gbl);
        displayImagePanel.setBorder(buildTitledBorder("Image"));
        makeDefaultLoadImageOptions(gbc, gbl);
        makeComplexImageOptions(gbc, gbl);
        makeLogMagImageOptions(gbc, gbl);
        makeInterpolateImageOptions(gbc, gbl);
        makeHistogramImageOptions(gbc, gbl);

        displayColorPanel.setLayout(gbl);
        displayColorPanel.setBorder(buildTitledBorder("Color\\VOI"));
        makeVOISaveLPSOptions(gbc, gbl);
        makeVOIContinuousOptions(gbc, gbl);
        makeVOILineAngleOptions(gbc, gbl);
        makeCrosshairOptions(gbc, gbl);
        makeActiveColorOptions(gbc, gbl);
        makeActiveColorBorderSizeOptions(gbc,gbl);
        makeIntensityLabelColorOptions(gbc, gbl);
        makeVOIDrawColorOptions(gbc, gbl);
        makeVOIColorOptions(gbc, gbl);
        makeVOIPointDrawTypeOptions(gbc, gbl);

        // make the saving options
        fileSavePanel.setLayout(gbl);
        fileSavePanel.setBorder(buildTitledBorder("Save"));
        makeSaveOverwriteOptions(gbc, gbl);
        makeSaveAllOptions(gbc, gbl);

        // makeSaveDefaultsOptions(gbc, gbl);
        makeSaveHdrImgOptions(gbc, gbl);
        makeSaveMncOptions(gbc, gbl);
        //makeSaveXMLOnHDRSaveOptions(gbc, gbl);
        makeSaveXMLThumbnailOptions(gbc, gbl);
        makeFrameRateOptions(gbc, gbl);
        makeFlipNIFTIReadOptions(gbc, gbl);

        fileMiscPanel.setLayout(gbl);
        fileMiscPanel.setBorder(buildTitledBorder("Misc"));
        makeQuickListOptions(gbc, gbl);
        makeFileFilterOptions(gbc, gbl);
        makeFileTemporaryDirectory(gbc, gbl);

        filePanel.setLayout(new BoxLayout(filePanel, BoxLayout.Y_AXIS));
        filePanel.add(fileSavePanel);
        filePanel.add(fileMiscPanel);

        displayPanel.setLayout(new BoxLayout(displayPanel, BoxLayout.Y_AXIS));
        displayPanel.add(displayUserInterfacePanel);
        displayPanel.add(displayImagePanel);
        displayPanel.add(displayColorPanel);

        // make the other options
        otherPanel.setLayout(gbl);
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        makeDicomReceiverOnStartOptions(gbc, gbl);
        makeMultiThreadingEnabledOptions(gbc, gbl);
        makeGpuCompEnabledOptions(gbc, gbl);
        makeSaveDefaultsOptions(gbc, gbl);
        makeProvenanceOptions(gbc, gbl);
        makeLaxCheckOptions(gbc, gbl);
        makeCheckOnCloseFrameOptions(gbc, gbl);
        makeLoggingOptions(gbc, gbl);
        makeOutputWindowOptions(gbc, gbl);
        makeDebugOptions(gbc, gbl);
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.addTab("Display", displayPanel);
        tabbedPane.addTab("File", filePanel);
        tabbedPane.addTab("Other", otherPanel);

        this.getContentPane().add(tabbedPane, BorderLayout.CENTER);

        // testing to see if Boolean.getBoolean("") works yet.
        // which it doesn't in the SUN java 1.3.01 for windows
        this.getContentPane().add(makeApplyClosePanel(), BorderLayout.SOUTH);

        pack();
        // this.setResizable(false);
        setVisible(true);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    // ***** ACTION LISTENER
    /**
     * Calls various methods based on the user's actions.
     * 
     * @param event Event that triggered this function.
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (event.getSource().equals(provenanceCheckBox)) {
            provenanceImageCheckBox.setEnabled(provenanceCheckBox.isSelected());
            provenanceFileButton.setEnabled(provenanceCheckBox.isSelected());
            if ( !provenanceCheckBox.isSelected()) {
                provenanceImageCheckBox.setSelected(false);
            }
        } else if (command.equalsIgnoreCase("ChooseProvenance")) {

            final JFileChooser chooser = new JFileChooser();

            try {
                chooser.setCurrentDirectory(new File(provenanceFilename).getParentFile());
            } catch (final Exception ex) {
                chooser.setCurrentDirectory(new File(System.getProperty("user.home")));
            }

            final int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                provenanceFilename = chooser.getSelectedFile().getPath();

                String shortName = provenanceFilename;

                if (provenanceFilename.length() > 24) {
                    shortName = ".."
                            + provenanceFilename.substring(provenanceFilename.length() - 22, provenanceFilename
                                    .length());
                }

                provenanceFileButton.setText(shortName);
                provenanceFileButton.setToolTipText(provenanceFilename);
            }

        } else if (command.equalsIgnoreCase("color")) {
            final int index = voiColorChoices.getSelectedIndex();
            voiColorChoices.setBackground(voiColors[index]);

        } else if (command.equalsIgnoreCase("intensityLabelColor")) {

            colorChooser = new ViewJColorChooser(null, "Pick label color", new ActionListener() { // OKAY listener
                        public void actionPerformed(final ActionEvent ae) {
                            intensityLabelColor = colorChooser.getColor();
                            intensityLabelColorButton.setBackground(intensityLabelColor);
                        }
                    }, new ActionListener() { // CANCEL listener
                        public void actionPerformed(final ActionEvent a) {}
                    });
        } else if (command.equalsIgnoreCase("intensityLabelBackground")) {
            colorChooser = new ViewJColorChooser(null, "Pick label background color", new ActionListener() { // OKAY
                        // listener
                        public void actionPerformed(final ActionEvent ae) {
                            intensityLabelBackgroundColor = colorChooser.getColor();
                            intensityLabelBackgroundButton.setBackground(intensityLabelBackgroundColor);
                        }
                    }, new ActionListener() { // CANCEL listener
                        public void actionPerformed(final ActionEvent a) {}
                    });
        } else if (command.equalsIgnoreCase(Preferences.PREF_VOI_DRAW_COLOR)) {
            colorChooser = new ViewJColorChooser(null, "Pick Active Color", new ActionListener() { // OKAY listener
                        public void actionPerformed(final ActionEvent ae) {
                            voiDrawColor = colorChooser.getColor();
                            voiDrawButton.setBackground(voiDrawColor);
                        }
                    }, new ActionListener() { // CANCEL listener
                        public void actionPerformed(final ActionEvent a) {}
                    });

        } else if (command.equalsIgnoreCase("choosefilter")) {
            final String filterString = (String) JOptionPane.showInputDialog(this, null, "Choose file filter",
                    JOptionPane.OK_CANCEL_OPTION, null, ViewImageFileFilter.getDescriptions(), ViewImageFileFilter
                            .getDescription(fileFilter));

            if (filterString != null) {
                fileFilter = ViewImageFileFilter.getFilterIndex(filterString);
                filterButton.setText(ViewImageFileFilter.getShortDescription(fileFilter));
                if (filterString.startsWith("User Defined")) {
                    editUserDefButton.setEnabled(true);
                } else {
                    editUserDefButton.setEnabled(false);
                }
            }
        } else if (command.equalsIgnoreCase("close")) { // close box
            dispose();
        } else if (command.equalsIgnoreCase("apply")) {
            String beforeComplexDisplayChoice = Preferences.getProperty(Preferences.PREF_COMPLEX_DISPLAY);
            boolean beforeDoLogMagChoice = Preferences.is(Preferences.PREF_LOGMAG_DISPLAY);
            
            // "apply" sets all the preferences in the dialog and then makes itself unset-able global preferences
            Preferences.setDebugLevels(new boolean[] {debugMinorBox.isSelected(), debugAlgorithmBox.isSelected(),
                    debugFileIOBox.isSelected(), debugCommsBox.isSelected(), debugScriptingBox.isSelected()});

            Preferences.setProperty(Preferences.PREF_SHOW_OUTPUT, String.valueOf(showOutputWindow.isSelected()));
            Preferences.setProperty(Preferences.PREF_MULTI_THREADING_ENABLED, String
                    .valueOf(multiThreadingEnabledCheckBox.isSelected()));
            Preferences.setProperty(Preferences.PREF_SHOW_INTENSITY_ON_LEFT_CLICK, String.valueOf(doIntensityOnLeftBox.isSelected()));
            Preferences.setProperty(Preferences.PREF_SHOW_WINLEV_ON_RIGHT_CLICK, String.valueOf(doWinLevOnRightBox.isSelected()));
            Preferences.setProperty(Preferences.PREF_GPU_COMP_ENABLED, String.valueOf(gpuCompEnabledCheckBox
                    .isSelected()));
            Preferences.setProperty(Preferences.PREF_AUTOSTART_DICOM_RECEIVER, String.valueOf(dicomReceiverOnStart
                    .isSelected()));

            Preferences.setProperty(Preferences.PREF_SHOW_SPLASH, String.valueOf(displaySplash.isSelected()));
            Preferences.setProperty(Preferences.PREF_OPEN_IMAGES_IN_TILED_FORMAT, String.valueOf(openImagesInTiledFormatBox.isSelected()));
            Preferences.setProperty(Preferences.PREF_SHOW_LINE_ANGLE, String.valueOf(showLineVOIAngleBox.isSelected()));

            Preferences.setProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR, MipavUtil
                    .makeColorString(preferredActiveColor));
            Preferences.setProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR_BORDERSIZE, (String)activeImageColorBorderSize.getSelectedItem());
            Preferences.setProperty(Preferences.PREF_CROSSHAIR_CURSOR, crosshairNames[crosshairChoices
                    .getSelectedIndex()]);
            Preferences.setProperty(Preferences.PREF_DEFAULT_DISPLAY, ((DefaultDisplay)defaultDisplayChoices.getSelectedItem()).name());
            Preferences.setProperty(Preferences.PREF_COMPLEX_DISPLAY, ((ComplexDisplay)complexDisplayChoices.getSelectedItem()).name());
            Preferences.setProperty(Preferences.PREF_LOGMAG_DISPLAY, String.valueOf(displayLogMag.isSelected()));
            Preferences.setProperty(Preferences.PREF_INTERPOLATE_MODE, ((InterpolateDisplay)interpolateDisplayChoices.getSelectedItem()).name());
            Preferences.setProperty(Preferences.PREF_HISTOGRAM_DISPLAY, String.valueOf(displayHistogram.isSelected()));
            
            // check to see if provenance should be turned on (if it was off)
            if (Preferences.is(Preferences.PREF_DATA_PROVENANCE) != provenanceCheckBox.isSelected()) {
                if (provenanceCheckBox.isSelected()) {
                    ProvenanceRecorder.getReference().startRecording();
                } else {
                    ProvenanceRecorder.getReference().stopRecording();
                }
            }

            Preferences.setProperty(Preferences.PREF_DATA_PROVENANCE, String.valueOf(provenanceCheckBox.isSelected()));
            Preferences.setProperty(Preferences.PREF_IMAGE_LEVEL_DATA_PROVENANCE, String
                    .valueOf(provenanceImageCheckBox.isSelected()));

            Preferences.setProperty(Preferences.PREF_ALWAYS_SAVE_IMG_AS_ANALYZE, String.valueOf(comboBoxSaveImgMethod
                    .getSelectedIndex() == 1));
            Preferences.setProperty(Preferences.PREF_ALWAYS_SAVE_IMG_AS_INTERFILE, String.valueOf(comboBoxSaveImgMethod
                    .getSelectedIndex() == 2));
            Preferences.setProperty(Preferences.PREF_ALWAYS_SAVE_IMG_AS_NIFTI, String.valueOf(comboBoxSaveImgMethod
                    .getSelectedIndex() == 3));
            Preferences.setProperty(Preferences.PREF_ALWAYS_SAVE_MNC_AS_MINC1, String.valueOf(comboBoxSaveMncMethod
                    .getSelectedIndex() == 1));
            Preferences.setProperty(Preferences.PREF_ALWAYS_SAVE_MNC_AS_MINC2, String.valueOf(comboBoxSaveMncMethod
                    .getSelectedIndex() == 2));
            Preferences.setProperty(Preferences.PREF_SAVE_ALL_ON_SAVE, String.valueOf(saveAllCheckBox.isSelected()));
            Preferences.setProperty(Preferences.PREF_SAVE_DEFAULTS, String.valueOf(saveDefaultsCheckBox.isSelected()));
            Preferences.setProperty(Preferences.PREF_SAVE_PROMPT_OVERWRITE, String.valueOf(savePromptOverwriteBox
                    .isSelected()));
            Preferences.setProperty(Preferences.PREF_SAVE_XML_THUMBNAIL, String.valueOf(saveThumbnailCheckBox
                    .isSelected()));
            Preferences.setProperty(Preferences.PREF_FLIP_NIFTI_READ, String
                    .valueOf(flipNIFTIReadCheckBox.isSelected()));
            Preferences.setProperty(Preferences.PREF_FILENAME_FILTER, String.valueOf(fileFilter));
            if (fileTempDirField.getText().length() > 0) {
                Preferences.setFileTempDir(fileTempDirField.getText());
            }

            Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, String.valueOf(checkOnFrameClose.isSelected()));
            Preferences.setProperty(Preferences.PREF_LAX_CHECK, String.valueOf(performLaxCheck.isSelected()));
            Preferences.setProperty(Preferences.PREF_VOI_START_COLOR, String
                    .valueOf(voiColorChoices.getSelectedIndex()));
            Preferences.setProperty(Preferences.PREF_INTENSITY_LABEL_COLOR, MipavUtil
                    .makeColorString(intensityLabelColor));
            Preferences.setProperty(Preferences.PREF_INTENSITY_LABEL_BACKGROUND_COLOR, MipavUtil
                    .makeColorString(intensityLabelBackgroundColor));
            Preferences.setProperty(Preferences.PREF_VOI_DRAW_COLOR, MipavUtil.makeColorString(voiDrawColor));
            Preferences.setProperty(Preferences.PREF_VOI_POINT_DRAW_TYPE, String.valueOf(pointVOIChoices
                    .getSelectedIndex()));
            Preferences.setProperty(Preferences.PREF_CONTINUOUS_VOI_CONTOUR, String.valueOf(continuousVOIBox
                    .isSelected()));
            Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, String.valueOf(saveVOILPSButton.isSelected()));
            Preferences.setProperty(Preferences.PREF_MENU_FONT, fontNames[fontChooser.getSelectedIndex()]);
            Preferences.setProperty(Preferences.PREF_MENU_FONT_SIZE, fontSizeField.getText());

            int fontSize = 12;

            try {
                fontSize = Integer.parseInt(fontSizeField.getText());

                if (fontSize < 4) {
                    fontSizeField.selectAll();
                    MipavUtil.displayError("Font size must be 4 or greater");
                    fontSizeField.requestFocus();

                    return;
                }

                Preferences.setProperty(Preferences.PREF_MENU_FONT_SIZE, fontSizeField.getText());
            } catch (final Exception ex) {
                fontSizeField.selectAll();
                MipavUtil.displayError("Font size must be 4 or greater");
                fontSizeField.requestFocus();

                return;
            }

            MipavUtil.buildDefaultFonts();

            float rate = 10.0f;

            try {
                rate = Float.parseFloat(frameRateField.getText());

                if ( (rate < 1.0f) || (rate > 100.0f)) {
                    frameRateField.selectAll();
                    MipavUtil.displayError("Frame rate must be between 1.0 and 100.0");
                    frameRateField.requestFocus();

                    return;
                }
            } catch (final Exception ex) {
                frameRateField.selectAll();
                MipavUtil.displayError("Frame rate must be between 1.0 and 100.0");
                frameRateField.requestFocus();

                return;
            }

            // System.err.println("rate is: " + rate);
            Preferences.setProperty(Preferences.PREF_DEFAULT_FRAME_RATE, Float.toString(rate));

            final String quickStr = Preferences.getProperty(Preferences.PREF_QUICKLIST_NUMBER);
            int quickNum = 4;
            final int newNum = quickListLevel.getSelectedIndex() + 1;

            if (quickStr != null) {
                quickNum = Integer.parseInt(quickStr);
            }

            if (newNum != quickNum) {
                Preferences.setProperty(Preferences.PREF_QUICKLIST_NUMBER, String.valueOf(newNum));
                userInterface.buildMenu();

                final Vector<Frame> imageFrames = userInterface.getImageFrameVector();

                if (imageFrames.size() < 1) {
                    userInterface.setControls();
                } else {

                    for (int i = 0; i < imageFrames.size(); i++) {

                        if (imageFrames.elementAt(i) instanceof ViewJFrameImage) {
                            ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
                        }
                    }
                }
            }

            if (!enableLoggingBox.isSelected()) {
                Preferences.setProperty(Preferences.PREF_LOGGING_ENABLED, "false");
                LogStdStreams.turnOffLogging();
                Preferences.debug("Turned off logging");
            } else if ( (Preferences.is(Preferences.PREF_DATA_PROVENANCE) && enableLoggingBox.isSelected())
                    && !Preferences.getProperty(Preferences.PREF_LOG_FILENAME).equalsIgnoreCase(logFilename)) {
                LogStdStreams.turnOffLogging();
                LogStdStreams.initializeErrorLogging(logFilename, "\n" + "Mipav Log: " + new Date(), true, true);
                Preferences.setProperty(Preferences.PREF_LOG_FILENAME, logFilename);
                Preferences.setProperty(Preferences.PREF_LOGGING_ENABLED, "true");
            } else if ( !Preferences.is(Preferences.PREF_DATA_PROVENANCE) && enableLoggingBox.isSelected()) {
                Preferences.debug("Turning on logging");
                LogStdStreams.initializeErrorLogging(logFilename, "\n" + "Mipav Log: " + new Date(), true, true);
                Preferences.setProperty(Preferences.PREF_LOGGING_ENABLED, "true");
                Preferences.setProperty(Preferences.PREF_LOG_FILENAME, logFilename);
            }

            if (dicomCatcher != null) {
                Preferences.setProperty(Preferences.PREF_AUTOSTART_DICOM_RECEIVER, String.valueOf(dicomCatcher
                        .isSelected()));
            }

            // OKButton.setEnabled(false); // doesn't act correctly when open and then new image frame is added.
            if (userInterface != null) {
                int i;
                final Enumeration<String> names = userInterface.getRegisteredImageNames();
                final Vector<Frame> imageframelist = userInterface.getImageFrameVector();

                for (i = 0; i < imageframelist.size(); i++) { // for all viewJFrames listed in the the user interface
                    // list

                    // have all images get the new color out of preferences
                    try {
                        ((ViewJFrameImage) userInterface.getImageFrameVector().elementAt(i)).getComponentImage()
                                .setHighlightColor(preferredActiveColor);

                        preferredCrosshair = Preferences.getProperty(Preferences.PREF_CROSSHAIR_CURSOR);

                        if (preferredCrosshair.equalsIgnoreCase("default")) {
                            ((ViewJFrameImage) userInterface.getImageFrameVector().elementAt(i)).getComponentImage()
                                    .setCrosshairCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
                        } else {

                            try {
                                final Toolkit toolkit = Toolkit.getDefaultToolkit();
                                ((ViewJFrameImage) userInterface.getImageFrameVector().elementAt(i))
                                        .getComponentImage().setCrosshairCursor(
                                                toolkit.createCustomCursor(MipavUtil.getIcon(preferredCrosshair)
                                                        .getImage(), new Point(12, 12), preferredCrosshair));
                            } catch (final NullPointerException noIcon) {

                                // specfied icon cannot be found. Instead, we set default:
                                ((ViewJFrameImage) userInterface.getImageFrameVector().elementAt(i))
                                        .getComponentImage().setCrosshairCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
                                Preferences.debug("JDialogMipavOptions: Crosshair icon \"" + preferredCrosshair
                                        + "\" cannot be found.  " + "Instead, using default crosshair pointer.\n", 2);
                            }
                        }

                    } catch (final ClassCastException cce) {}
                }

                // loop through all the registered images
                while (names.hasMoreElements()) {

                    // get the ModelImage
                    final ModelImage img = userInterface.getRegisteredImageByName((String) names.nextElement());

                    if (img == null) {
                        continue;
                    }

                    try {
                        img.getLightBoxFrame().setHighlightColor(preferredActiveColor);
                    } catch (final NullPointerException npe) {} finally {
                        img.notifyImageDisplayListeners();
                    }
                } // end while loop
            }
            
            // set changes which affect image display
            reloadComplexImages(beforeComplexDisplayChoice, beforeDoLogMagChoice);
            
            // set changes which affect GUI display
            if (userInterface != null) {
                userInterface.updateMultiCoreUsage();
                userInterface.updateGpuUsage();
            }

            // set the cancel button text to 'close' since the changes were accepted
            cancelButton.setText("Close");
        } else if (command.equalsIgnoreCase("active color")) {
            colorChooser = new ViewJColorChooser(null, "Pick Active Color", new ActionListener() { // OKAY listener
                        public void actionPerformed(final ActionEvent ae) {
                            preferredActiveColor = colorChooser.getColor();
                            activeColor.setBackground(preferredActiveColor);
                        }
                    }, new ActionListener() { // CANCEL listener
                        public void actionPerformed(final ActionEvent a) {}
                    });
        } else if (command.equals("ChooseLog")) {

            final JFileChooser chooser = new JFileChooser();

            try {
                chooser.setCurrentDirectory(new File(logFilename).getParentFile());
            } catch (final Exception ex) {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            final int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                logFilename = chooser.getSelectedFile().getPath();

                String shortName = logFilename;

                if (logFilename.length() > 24) {
                    shortName = ".." + logFilename.substring(logFilename.length() - 22, logFilename.length());
                }

                logFileButton.setText(shortName);
                logFileButton.setToolTipText(logFilename);
            }

        } else if (event.getSource() == enableLoggingBox) {

            if (enableLoggingBox.isSelected()) {
                logFileButton.setEnabled(true);
            } else {
                logFileButton.setEnabled(false);
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10247");
            MipavUtil.showWebHelp("Customizing_MIPAV");
        } else if (command.equals("fileTempDirBrowse")) {
            final JFileChooser chooser = new JFileChooser();
            if (Preferences.getFileTempDir() != null) {
                chooser.setCurrentDirectory(new File(Preferences.getFileTempDir()));
            }
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setMultiSelectionEnabled(false);
            final int retValue = chooser.showOpenDialog(fileMiscPanel);
            if (retValue == JFileChooser.APPROVE_OPTION) {
                final File file = chooser.getSelectedFile();
                fileTempDirField.setText(file.getAbsolutePath());
            }
        } else if (command.equals("editUserDef")) {
            final JDialogEditUserDefinedFileTypes editUserDefDialog = new JDialogEditUserDefinedFileTypes();
            MipavUtil.centerInComponent(this, editUserDefDialog);
        } else if (command.equals("GPU Info")) {
        	OpenCLInfo.main(null);
        } else {
            super.actionPerformed(event);
        }

    }

    /**
     * when the image is available, there is an option there is an option to have processes operating on the image send
     * logging information to the Log pane of the output window. This method would create the Log checkbox inside the
     * MIPAV options dialog. Note that this operation was not finished, and there is no way to ensure the image and the
     * log checkbox are truly connected.
     */
    /*
     * public void includeImageSettingsBoxGG() { // if the user interface isn't already avail, ignore if (userInterface ==
     * null || userInterface.getImageFrameVector().size() == 0) { return; } // image specific variables
     * localChangesPanel = new JPanel(); localChangesPanel.setBorder(buildTitledBorder("Image Settings")); // panel gets
     * a grid layout GridBagLayout gbl = new GridBagLayout(); GridBagConstraints gbc = new GridBagConstraints();
     * gbc.anchor = GridBagConstraints.WEST; localChangesPanel.setLayout(gbl); imageChooser = new JComboBox();
     * fillChooser(); imageChooser.setFont(MipavUtil.font12); gbc.gridheight = GridBagConstraints.REMAINDER;
     * gbc.gridwidth = 1; gbc.anchor = GridBagConstraints.NORTH; gbc.insets = new Insets(0, 0, 0, 10);
     * gbl.setConstraints(imageChooser, gbc); localChangesPanel.add(imageChooser); logMode = new JCheckBox("Use Log
     * Mode"); logMode.setFont(MipavUtil.font12); gbc.gridheight = 1; gbc.gridwidth = 1; gbc.anchor =
     * GridBagConstraints.NORTH; gbl.setConstraints(logMode, gbc); localChangesPanel.add(logMode);
     * this.getContentPane().add(localChangesPanel, BorderLayout.CENTER); pack(); setVisible(true); }
     */

    /**
     * Evaluates whether the user preferences which control complex image loading have changed in a way that requires reloading of all complex images
     */
    private void reloadComplexImages(String beforeComplexDisplayChoice, boolean beforeDoLogMagChoice) {
        boolean complexRefreshReq = false;
        if(beforeComplexDisplayChoice != null && Preferences.getProperty(Preferences.PREF_COMPLEX_DISPLAY) != null) {
            if(!beforeComplexDisplayChoice.equals(Preferences.getProperty(Preferences.PREF_COMPLEX_DISPLAY))) {
                Preferences.debug("\nSince complex display type has been changed, complex images must be reinitalized.\n", Preferences.DEBUG_MINOR);
                complexRefreshReq = true;
            }
        }
        if(beforeDoLogMagChoice != Preferences.is(Preferences.PREF_LOGMAG_DISPLAY)) { 
            Preferences.debug("\nSince logmag display preference has changed, complex images must be reinitalized.\n", Preferences.DEBUG_MINOR);
            complexRefreshReq = true;
        }
        
        if(complexRefreshReq && userInterface != null) {
            for(Enumeration<ModelImage> e = userInterface.getRegisteredImages(); e.hasMoreElements();) {
                ModelImage m = e.nextElement();
                if(m.isComplexImage()) {
                    Preferences.debug("Reloading "+m.getImageName()+"\n", Preferences.DEBUG_MINOR);
                    m.notifyImageExtentsListeners();
                }
            }
        }
    }

    /**
     * no information available.
     */
    public void fillChooser() {
        final Vector<Frame> uiV = userInterface.getImageFrameVector();

        if (uiV.size() > 0) {

            for (int i = uiV.size() - 1; i >= 0; i--) {
                imageChooser.addItem( ((ViewJFrameImage) uiV.elementAt(i)).getTitle());
            }

            imageChooser.setSelectedItem( ((ViewJFrameImage) uiV.elementAt(0)).getTitle());
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param e DOCUMENT ME!
     */
    public void itemStateChanged(final ItemEvent e) {

        if (e.getSource().equals(fontChooser)) {
            fontChooser.setFont(new Font(labels[fontChooser.getSelectedIndex()].getText(), 0, 12));
        } else if (e.getSource().equals(showOutputWindow)) {
            ViewUserInterface.getReference().enableOutputWindow(showOutputWindow.isSelected());
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param e DOCUMENT ME!
     */
    public void keyPressed(final KeyEvent e) {}

    /**
     * DOCUMENT ME!
     * 
     * @param e DOCUMENT ME!
     */
    public void keyReleased(final KeyEvent e) {

        if (e.getKeyCode() == KeyEvent.VK_ENTER) {

            try {
                final int newSize = Integer.parseInt(fontSizeField.getText());

                if (newSize >= 4) {

                    for (final JLabel element : labels) {
                        element.setFont(new Font(element.getText(), 0, newSize));
                    }

                    fontChooser.repaint();
                    fontChooser.validate();

                }
            } catch (final Exception ex) {}
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @param e DOCUMENT ME!
     */
    public void keyTyped(final KeyEvent e) {}

    /**
     * Displays the panel with the given name.
     */
    public void showPane(final String name) {
        int selected = 0;
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            if (tabbedPane.getTitleAt(i).equals(name)) {
                selected = i;
            }
        }

        tabbedPane.setSelectedIndex(selected);
    }

    /**
     * makes the active-colour option line in the globalChangesPanel, to allow user to select the colour used to denote
     * the active image. Sets the colour to either the colour in the preferences file or to the MIPAV default.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeActiveColorOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JLabel l1 = new JLabel("Active image border color:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(l1, gbc);
        displayColorPanel.add(l1,gbc);

        activeColor = new JButton();
        activeColor.setActionCommand("active color");
        activeColor.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(activeColor, gbc);
        displayColorPanel.add(activeColor,gbc);


        // preset the choices.
        if (Preferences.getProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR) == null) {
            Preferences.setProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR, MipavUtil
                    .makeColorString(ViewJComponentEditImage.ACTIVE_IMAGE_COLOR));
            activeColor.setBackground(ViewJComponentEditImage.ACTIVE_IMAGE_COLOR);
            preferredActiveColor = ViewJComponentEditImage.ACTIVE_IMAGE_COLOR;
        } else {
            preferredActiveColor = MipavUtil.extractColor(Preferences.getProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR));
            activeColor.setBackground(preferredActiveColor);
        }
    }
    
    
    
    protected void makeActiveColorBorderSizeOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JLabel l1 = new JLabel("Active image border color size:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(l1, gbc);
        displayColorPanel.add(l1,gbc);

        
        String[] borderSizes = {"1", "2", "3", "4", "5"};
        activeImageColorBorderSize = new JComboBox(borderSizes);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(activeImageColorBorderSize, gbc);
        displayColorPanel.add(activeImageColorBorderSize,gbc);


        // preset the choices.
        if (Preferences.getProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR_BORDERSIZE) == null) {
        	activeImageColorBorderSize.setSelectedIndex(0);
        } else {
        	String preferredSize = Preferences.getProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR_BORDERSIZE);
        	activeImageColorBorderSize.setSelectedItem(preferredSize);
        }
    }

    /**
     * makes the Apply/Close button panel, with the Apply button on the left and the Close button on the right.
     * 
     * <p>
     * The panel is created and organised, but not applied anywhere.
     * </p>
     * 
     * @return the Panel made.
     */
    protected JPanel makeApplyClosePanel() {
        final JPanel applyClosePanel = new JPanel();

        buildOKButton();
        OKButton.setText("Apply");

        // OKButton.setEnabled(false); // doesn't act correctly when open and then new image frame is added.
        applyClosePanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setText("Cancel");
        cancelButton.setActionCommand("close");
        applyClosePanel.add(cancelButton, BorderLayout.CENTER);
        applyClosePanel.add(buildHelpButton(), BorderLayout.EAST);

        return applyClosePanel;
    }

    /**
     * Makes the "Check on frame close" option line in the globalChangesPanel If checked the user is required to reply
     * to a dialog to close the frame. If unchecked the frame is closed and data my be lost.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeCheckOnCloseFrameOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        checkOnFrameClose = new JCheckBox("Check on closing frame");
        checkOnFrameClose.setFont(MipavUtil.font12);
        checkOnFrameClose.setForeground(Color.black);
        checkOnFrameClose.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbl.setConstraints(checkOnFrameClose, gbc);
        otherPanel.add(checkOnFrameClose);

        // preset the choices.
        checkOnFrameClose.setSelected(Preferences.is(Preferences.PREF_CLOSE_FRAME_CHECK));
    }
    
    /**
     * Makes the options for displaying how images should be displayed on default.
     */
    protected void makeDefaultLoadImageOptions(final GridBagConstraints gbc2, final GridBagLayout gbl) {
    	final JLabel l1 = new JLabel("Load image using default: ");
    	l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc2.insets = new Insets(0, 0, 0, 5);
        gbc2.gridwidth = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        displayImagePanel.add(l1, gbc2);
        
        defaultDisplayChoices = new JComboBox(DefaultDisplay.values());
        defaultDisplayChoices.setFont(MipavUtil.font12);
        
        gbc2.insets = new Insets(0, 0, 5, 0);
        gbc2.gridwidth = GridBagConstraints.REMAINDER;
        gbc2.anchor = GridBagConstraints.WEST;
        displayImagePanel.add(defaultDisplayChoices, gbc2);
        
        DefaultDisplay defaultChoice = DefaultDisplay.Default;
        //preset the choices.
        if(Preferences.getProperty(Preferences.PREF_DEFAULT_DISPLAY) == null) {
        	Preferences.setProperty(Preferences.PREF_DEFAULT_DISPLAY, DefaultDisplay.Default.name());
        } else {
        	defaultChoice = DefaultDisplay.valueOf(Preferences.getProperty(Preferences.PREF_DEFAULT_DISPLAY));
        }
        
        if(defaultChoice != null) {
        	defaultDisplayChoices.setSelectedItem(defaultChoice);
        }
    }
    
    /**
     * Makes the options for displaying complex image information
     * 
     * @param gbc2 GridBagConstraints
     * @param gbl GridBagLayout
     */
    protected void makeComplexImageOptions(final GridBagConstraints gbc2, final GridBagLayout gbl) {
        final JLabel l1 = new JLabel("Show complex image information using:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc2.insets = new Insets(0, 0, 0, 5);
        gbc2.gridwidth = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        displayImagePanel.add(l1, gbc2);

        complexDisplayChoices = new JComboBox(ComplexDisplay.values());
        complexDisplayChoices.setFont(MipavUtil.font12);

        gbc2.insets = new Insets(0, 0, 0, 0);
        gbc2.gridwidth = GridBagConstraints.REMAINDER;
        gbc2.anchor = GridBagConstraints.WEST;
        displayImagePanel.add(complexDisplayChoices, gbc2);
        
        ComplexDisplay defaultChoice = ComplexDisplay.MAGNITUDE;
        // preset the choices.
        if (Preferences.getProperty(Preferences.PREF_COMPLEX_DISPLAY) == null) {
            Preferences.setProperty(Preferences.PREF_COMPLEX_DISPLAY, ComplexDisplay.MAGNITUDE.name());
        } else {
            defaultChoice = ComplexDisplay.valueOf(Preferences.getProperty(Preferences.PREF_COMPLEX_DISPLAY));
        }

        if(defaultChoice != null) {
            complexDisplayChoices.setSelectedItem(defaultChoice);
        }
    }
    
    /**
     * Makes checkbox for whether to display the log of image magnitude on user's screen
     */
     protected void makeLogMagImageOptions(final GridBagConstraints gbc2, final GridBagLayout gbl) {
     
         displayLogMag = new JCheckBox("Display log magnitude of complex images");
         displayLogMag.setFont(MipavUtil.font12);
         displayLogMag.setForeground(Color.black);
         displayLogMag.addActionListener(this);
         gbc2.insets = new Insets(0, 0, 0, 0);
         gbc2.gridwidth = GridBagConstraints.REMAINDER;
         gbc2.anchor = GridBagConstraints.WEST;
         displayImagePanel.add(displayLogMag, gbc2);
         
         if(Preferences.getProperty(Preferences.PREF_LOGMAG_DISPLAY) == null) {
             Preferences.setProperty(Preferences.PREF_LOGMAG_DISPLAY, Boolean.valueOf(false).toString());
         } else {
             displayLogMag.setSelected(Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
         }
     }
     
     /**
      * Makes checkbox for whether to display the image with interpolation on user's screen
      */
      protected void makeInterpolateImageOptions(final GridBagConstraints gbc2, final GridBagLayout gbl) {
      
          final JLabel l1 = new JLabel("Interpolate images using:");
          l1.setFont(MipavUtil.font12);
          l1.setForeground(Color.black);
          gbc2.insets = new Insets(0, 0, 0, 5);
          gbc2.gridwidth = 1;
          gbc2.anchor = GridBagConstraints.WEST;
          displayImagePanel.add(l1, gbc2);
          
          interpolateDisplayChoices = new JComboBox(InterpolateDisplay.values());
          interpolateDisplayChoices.setFont(MipavUtil.font12);
          
          gbc2.insets = new Insets(0, 0, 0, 0);
          gbc2.gridwidth = GridBagConstraints.REMAINDER;
          gbc2.anchor = GridBagConstraints.WEST;
          displayImagePanel.add(interpolateDisplayChoices, gbc2);
          
          InterpolateDisplay defaultChoice = InterpolateDisplay.NEAREST;
          // preset the choices.
          if (Preferences.getProperty(Preferences.PREF_INTERPOLATE_MODE) == null) {
              Preferences.setProperty(Preferences.PREF_INTERPOLATE_MODE, InterpolateDisplay.NEAREST.name());
          } else {
              defaultChoice = InterpolateDisplay.valueOf(Preferences.getProperty(Preferences.PREF_INTERPOLATE_MODE));
          }

          if(defaultChoice != null) {
              interpolateDisplayChoices.setSelectedItem(defaultChoice);
          }
      }
      
      /**
       * Makes checkbox for whether to update image in real-time as histogram luts are changed
       */
       protected void makeHistogramImageOptions(final GridBagConstraints gbc2, final GridBagLayout gbl) {
       
           displayHistogram = new JCheckBox("Change image in real-time based on histogram changes");
           displayHistogram.setFont(MipavUtil.font12);
           displayHistogram.setForeground(Color.black);
           displayHistogram.addActionListener(this);
           gbc2.insets = new Insets(0, 0, 0, 0);
           gbc2.gridwidth = GridBagConstraints.REMAINDER;
           gbc2.anchor = GridBagConstraints.WEST;
           displayImagePanel.add(displayHistogram, gbc2);
           
           if(Preferences.getProperty(Preferences.PREF_HISTOGRAM_DISPLAY) == null) {
               Preferences.setProperty(Preferences.PREF_HISTOGRAM_DISPLAY, Boolean.valueOf(true).toString());
           } else {
               displayHistogram.setSelected(Preferences.is(Preferences.PREF_HISTOGRAM_DISPLAY));
           }
       }
    
    
    /**
     * Makes the options for crosshair display.
     * 
     * @param gbc2 GridBagConstraints
     * @param gbl GridBagLayout
     */
    protected void makeCrosshairOptions(final GridBagConstraints gbc2, final GridBagLayout gbl) {
        final JLabel l1 = new JLabel("Crosshair cursor color:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc2.insets = new Insets(0, 0, 0, 5);
        gbc2.gridwidth = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        displayColorPanel.add(l1, gbc2);

        crosshairNames = new String[8];
        crosshairNames[0] = "default";
        crosshairNames[1] = "crosshairWhite.gif";
        crosshairNames[2] = "crosshairRed.gif";
        crosshairNames[3] = "crosshairGreen.gif";
        crosshairNames[4] = "crosshairBlue.gif";
        crosshairNames[5] = "crosshairBlack.gif";
        crosshairNames[6] = "crosshairBlackWhite.gif";
        crosshairNames[7] = "crosshairHidden.gif";

        final Integer[] intArray = new Integer[crosshairNames.length];

        for (int i = 0; i < intArray.length; i++) {
            intArray[i] = new Integer(i);
        }

        crosshairChoices = new JComboBox(intArray);
        crosshairChoices.setFont(MipavUtil.font12);
        crosshairChoices.setRenderer(new ComboBoxRenderer());

        gbc2.insets = new Insets(0, 0, 0, 0);
        gbc2.gridwidth = GridBagConstraints.REMAINDER;
        gbc2.anchor = GridBagConstraints.WEST;
        displayColorPanel.add(crosshairChoices, gbc2);

        // preset the choices.
        if (Preferences.getProperty(Preferences.PREF_CROSSHAIR_CURSOR) == null) {
            Preferences.setProperty(Preferences.PREF_CROSSHAIR_CURSOR, "default");

            // crosshairColor.setBackground(ViewJComponentEditImage.CROSSHAIR_CURSOR_COLOR);
            preferredCrosshair = "default";
        } else {
            preferredCrosshair = Preferences.getProperty(Preferences.PREF_CROSSHAIR_CURSOR);
            // crosshairColor.setBackground(preferredCrosshairColor);
        }

        int index = -1;

        for (int i = 0; i < crosshairNames.length; i++) {

            if (preferredCrosshair.equals(crosshairNames[i])) {
                index = i;

                break;
            }
        }

        if (index != -1) {
            crosshairChoices.setSelectedIndex(index);
        } else {
            crosshairChoices.setSelectedIndex(0);
        }
    }

    /**
     * makes the debug option line in the globalChangesPanel.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeDebugOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JPanel debugPanel = new JPanel(new GridBagLayout());
        debugPanel.setBorder(this.buildTitledBorder("Debug levels"));

        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;

        final boolean[] levels = Preferences.getDebugLevels();

        debugMinorBox = new JCheckBox("Minor");
        debugMinorBox.setFont(MipavUtil.font12);
        debugMinorBox.setSelected(levels[0]);

        debugAlgorithmBox = new JCheckBox("Algorithm");
        debugAlgorithmBox.setFont(MipavUtil.font12);
        debugAlgorithmBox.setSelected(levels[1]);

        debugFileIOBox = new JCheckBox("FileIO");
        debugFileIOBox.setFont(MipavUtil.font12);
        debugFileIOBox.setSelected(levels[2]);

        debugCommsBox = new JCheckBox("Comms");
        debugCommsBox.setFont(MipavUtil.font12);
        debugCommsBox.setSelected(levels[3]);

        debugScriptingBox = new JCheckBox("Scripting");
        debugScriptingBox.setFont(MipavUtil.font12);
        debugScriptingBox.setSelected(levels[4]);

        debugPanel.add(debugMinorBox, gbc2);
        debugPanel.add(debugAlgorithmBox, gbc2);
        debugPanel.add(debugFileIOBox, gbc2);
        debugPanel.add(debugCommsBox, gbc2);
        debugPanel.add(debugScriptingBox, gbc2);

        gbc.fill = GridBagConstraints.BOTH;
        otherPanel.add(debugPanel, gbc);
    }

    /**
     * Makes the file filter option line in the globalChangesPanel.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeFileFilterOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JLabel fileFilterLabel = new JLabel("File filter default:");
        fileFilterLabel.setFont(MipavUtil.font12);
        fileFilterLabel.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(fileFilterLabel, gbc);
        fileMiscPanel.add(fileFilterLabel);

        filterButton = new JButton();
        filterButton.addActionListener(this);
        filterButton.setActionCommand("chooseFilter");
        filterButton.setFont(MipavUtil.font12);
        gbc.insets = new Insets(0, 0, 0, 0);
        // gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(filterButton, gbc);
        fileMiscPanel.add(filterButton);

        // preset the choices:
        try {
            fileFilter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (final NumberFormatException nfe) {

            // an invalid value was set in preferences -- so ignore it!
            fileFilter = 0;
        } catch (final NullPointerException npe) {

            // no property was set
            fileFilter = 0;
        }
        // fileFilterComboBox.setSelectedIndex(filter);

        filterButton.setText(ViewImageFileFilter.getShortDescription(fileFilter));

        editUserDefButton = new JButton("Edit User Defined");
        editUserDefButton.addActionListener(this);
        editUserDefButton.setActionCommand("editUserDef");
        editUserDefButton.setFont(MipavUtil.font12);
        if (filterButton.getText().equals("User Defined")) {
            editUserDefButton.setEnabled(true);
        } else {
            editUserDefButton.setEnabled(false);
        }
        gbc.insets = new Insets(0, 25, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(editUserDefButton, gbc);
        fileMiscPanel.add(editUserDefButton);

    } // end makeFileFilterOptions

    /**
     * Makes the temporary file directory fields in fileMiscPanel.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeFileTemporaryDirectory(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JLabel tempDirLabel = new JLabel("Temporary Directory : ");
        tempDirLabel.setFont(MipavUtil.font12);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(tempDirLabel, gbc);
        fileMiscPanel.add(tempDirLabel);

        fileTempDirField = new JTextField("");
        if (Preferences.getFileTempDir() != null) {
            fileTempDirField.setText(Preferences.getFileTempDir());
        }
        fileTempDirField.setColumns(16);
        fileTempDirField.setEditable(false);
        gbc.gridwidth = 1;
        gbl.setConstraints(fileTempDirField, gbc);
        fileMiscPanel.add(fileTempDirField);

        fileTempDirBrowseButton = new JButton("Browse");
        fileTempDirBrowseButton.setFont(MipavUtil.font12);
        fileTempDirBrowseButton.addActionListener(this);
        fileTempDirBrowseButton.setActionCommand("fileTempDirBrowse");
        gbc.insets = new Insets(0, 25, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(fileTempDirBrowseButton, gbc);
        fileMiscPanel.add(fileTempDirBrowseButton);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param gbc DOCUMENT ME!
     * @param gbl DOCUMENT ME!
     */
    protected void makeFontOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JPanel fontPanel = new JPanel(new GridBagLayout());
        fontPanel.setBorder(this.buildTitledBorder("Font options"));

        final GridBagConstraints gbc2 = new GridBagConstraints();

        String currentFont = "Serif";

        if (Preferences.getProperty(Preferences.PREF_MENU_FONT) != null) {
            currentFont = Preferences.getProperty(Preferences.PREF_MENU_FONT);
        }

        int size = 12;

        if (Preferences.getProperty(Preferences.PREF_MENU_FONT_SIZE) != null) {

            try {
                size = Integer.parseInt(Preferences.getProperty(Preferences.PREF_MENU_FONT_SIZE));
            } catch (final Exception e) {}
        }

        // Get all font family names
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        fontNames = ge.getAvailableFontFamilyNames();

        labels = new JLabel[fontNames.length];

        int index = 0;
        final Integer[] intArray = new Integer[labels.length];

        for (int i = 0; i < labels.length; i++) {

            intArray[i] = new Integer(i);

            labels[i] = new JLabel(fontNames[i]);
            labels[i].setFont(new Font(fontNames[i], 0, size));

            if (fontNames[i].equalsIgnoreCase(currentFont)) {
                index = i;
            }
        }

        fontChooser = new JComboBox(intArray);
        fontChooser.setSelectedIndex(index);
        fontChooser.setRenderer(new FontBoxRenderer());

        fontChooser.setFont(new Font(labels[index].getText(), 0, 12));
        fontChooser.addItemListener(this);

        fontSizeField = new JTextField(Integer.toString(size), 3);
        fontSizeField.addKeyListener(this);
        MipavUtil.makeNumericsOnly(fontSizeField, false);

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        fontPanel.add(fontChooser, gbc2);

        gbc2.gridx = 1;
        gbc2.insets = new Insets(0, 5, 0, 0);
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        fontPanel.add(fontSizeField, gbc2);

        // gbc.gridy++;
        // gbc.fill = GridBagConstraints;
        displayUserInterfacePanel.add(fontPanel, gbc);
    }
    
    /**
     * Makes options for how user interface will react when the left or right mouse buttons are clicked.
     * 
     * @param gbc DOCUMENT ME!
     * @param gbl DOCUMENT ME!
     */
    protected void makeMouseClickOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {

        boolean doLeft = true, doRight = true;
        
        

        if (Preferences.getProperty(Preferences.PREF_SHOW_INTENSITY_ON_LEFT_CLICK) != null) {
            doLeft = Preferences.is(Preferences.PREF_SHOW_INTENSITY_ON_LEFT_CLICK);
        }
        
        if (Preferences.getProperty(Preferences.PREF_SHOW_WINLEV_ON_RIGHT_CLICK) != null) {
            doRight = Preferences.is(Preferences.PREF_SHOW_WINLEV_ON_RIGHT_CLICK);
        }
        
        doIntensityOnLeftBox = new JCheckBox("Display position and intensity on left mouse click");
        doIntensityOnLeftBox.setFont(MipavUtil.font12);
        doIntensityOnLeftBox.setForeground(Color.black);
        doIntensityOnLeftBox.addActionListener(this);
        doIntensityOnLeftBox.setSelected(doLeft);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(doIntensityOnLeftBox, gbc);
        displayUserInterfacePanel.add(doIntensityOnLeftBox);
        
        doWinLevOnRightBox = new JCheckBox("Adjust window/level on right mouse click");
        doWinLevOnRightBox.setFont(MipavUtil.font12);
        doWinLevOnRightBox.setForeground(Color.black);
        doWinLevOnRightBox.addActionListener(this);
        doWinLevOnRightBox.setSelected(doRight);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(doWinLevOnRightBox, gbc);
        displayUserInterfacePanel.add(doWinLevOnRightBox);
        
        
        
        
    }

    /**
     * Makes options for default frame rate for saving AVIs.
     * 
     * @param gbc GridBagConstraints
     * @param gbl GridBagLayout
     */
    protected void makeFrameRateOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JLabel frameRateLabel = new JLabel("Default frame rate for Save-image-as AVI:");
        frameRateLabel.setFont(MipavUtil.font12);
        frameRateLabel.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(frameRateLabel, gbc);
        fileSavePanel.add(frameRateLabel);

        frameRateField = new JTextField(3);
        frameRateField.setFont(MipavUtil.font12);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(frameRateField, gbc);
        fileSavePanel.add(frameRateField);
        MipavUtil.makeNumericsOnly(frameRateField, true);
        frameRateField.setText(Float.toString(Preferences.getDefaultFrameRate()));
    }

    /**
     * Makes the "Perform LAX check" option line in the globalChangesPanel If checked, the initial heap size and maximum
     * heap size in the LAX startup file is checked against what is in the preferences for this option. If unchecked, it
     * signifies that no check should be made.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeLaxCheckOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        performLaxCheck = new JCheckBox("LAX/Preferences memory check");
        performLaxCheck.setFont(MipavUtil.font12);
        performLaxCheck.setForeground(Color.black);
        performLaxCheck.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(performLaxCheck, gbc);
        otherPanel.add(performLaxCheck);

        // preset the choices.
        if ( !Preferences.isPreferenceSet(Preferences.PREF_LAX_CHECK)) {
            Preferences.setProperty(Preferences.PREF_LAX_CHECK, "true");
        }

        final boolean flag = Preferences.is(Preferences.PREF_LAX_CHECK);
        performLaxCheck.setSelected(flag);
    }

    /**
     * Makes the Logging option line with checkbox/button in the globalChangesPanel.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeLoggingOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final boolean loggingOn = Preferences.is(Preferences.PREF_LOGGING_ENABLED);
        
        
        enableLoggingBox = new JCheckBox("Log errors to:", loggingOn);
        enableLoggingBox.setFont(MipavUtil.font12);
        enableLoggingBox.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        otherPanel.add(enableLoggingBox, gbc);
        enableLoggingBox.addActionListener(this);
        
        logFilename = Preferences.getProperty(Preferences.PREF_LOG_FILENAME);
        
        try {
			BufferedWriter out = new BufferedWriter(new FileWriter("logFilename"));
			out.write("");
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        
        String shortName = "exceptions.txt";

        if (logFilename.length() > 24) {
            shortName = ".." + logFilename.substring(logFilename.length() - 22, logFilename.length());
        }
        

        logFileButton = new JButton(shortName);
        logFileButton.setEnabled(true);
        logFileButton.setToolTipText(logFilename);
        logFileButton.setFont(MipavUtil.font12);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        logFileButton.setEnabled(loggingOn);
        logFileButton.addActionListener(this);
        logFileButton.setActionCommand("ChooseLog");      


        otherPanel.add(logFileButton, gbc);
        
        if(ViewUserInterface.getExceptions() != null) {
            ViewUserInterface.getExceptions().deleteOnExit();
        }
        
    }

    /**
     * Makes the Data provenance option line in the globalChangesPanel.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeProvenanceOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {

        provenanceCheckBox = new JCheckBox("Global data provenance");
        provenanceCheckBox.setFont(MipavUtil.font12);
        provenanceCheckBox.setForeground(Color.black);
        provenanceCheckBox.addActionListener(this);
        provenanceCheckBox.setToolTipText("");

        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbl.setConstraints(provenanceCheckBox, gbc);
        otherPanel.add(provenanceCheckBox);

        // preset the choices.
        provenanceCheckBox.setSelected(Preferences.is(Preferences.PREF_DATA_PROVENANCE));

        provenanceFilename = Preferences.getProperty(Preferences.PREF_DATA_PROVENANCE_FILENAME);
        if (provenanceFilename == null) {
            provenanceFilename = System.getProperty("user.home") + File.separator + "mipav" + File.separator
                    + "dataprovenance.xmp";
            Preferences.setProperty(Preferences.PREF_DATA_PROVENANCE_FILENAME, provenanceFilename);
        }

        String shortName = provenanceFilename;

        if (provenanceFilename.length() > 24) {
            shortName = ".."
                    + provenanceFilename.substring(provenanceFilename.length() - 22, provenanceFilename.length());
        }

        provenanceFileButton = new JButton(shortName);
        provenanceFileButton.setToolTipText(provenanceFilename);
        provenanceFileButton.setFont(MipavUtil.font12);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        provenanceFileButton.setEnabled(provenanceCheckBox.isSelected());
        provenanceFileButton.addActionListener(this);
        provenanceFileButton.setActionCommand("ChooseProvenance");
        otherPanel.add(provenanceFileButton, gbc);

        provenanceImageCheckBox = new JCheckBox("Image level data provenance");
        provenanceImageCheckBox.setFont(MipavUtil.font12);
        provenanceImageCheckBox.setForeground(Color.black);
        provenanceImageCheckBox.addActionListener(this);
        provenanceImageCheckBox
                .setToolTipText("Data provenance per image (eg saving file_test.jpg will create file_test.xmp");
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbl.setConstraints(provenanceImageCheckBox, gbc);
        otherPanel.add(provenanceImageCheckBox);

        provenanceImageCheckBox.setEnabled(provenanceCheckBox.isSelected());

        // preset the choices.
        provenanceImageCheckBox.setSelected(Preferences.is(Preferences.PREF_IMAGE_LEVEL_DATA_PROVENANCE));
    }

    /**
     * takes a txt field, and forces the textfield to accept numbers, backspace, period and delete-key entries.
     * 
     * @param txt DOCUMENT ME!
     */
    protected void makeNumericsOnly(final JTextField txt) {
        txt.addKeyListener(new KeyAdapter() { // make the field
                    public void keyTyped(final KeyEvent evt) { // not accept letters

                        final char ch = evt.getKeyChar();

                        if ( ( (ch < '0') || (ch > '9'))
                                && ( (ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE)
                                        && (ch != KeyEvent.VK_DECIMAL) && (ch != KeyEvent.VK_PERIOD))) {

                            // if is the case that ch is outside the bounds of a number
                            // AND it is the case that ch is neither a BS or a DE, then
                            // key is not a digit or a deletion char or a decimal point
                            evt.consume();
                        }
                    }
                });
    } // end makeNumericsOnly

    /**
     * Makes the checkbox where the user can decide whether the output window should appear on startup.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeOutputWindowOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        showOutputWindow = new JCheckBox("Show data/debugging output window");
        showOutputWindow.setFont(MipavUtil.font12);
        showOutputWindow.setForeground(Color.black);
        showOutputWindow.addActionListener(this);
        showOutputWindow.addItemListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbl.setConstraints(showOutputWindow, gbc);
        otherPanel.add(showOutputWindow);

        if ( !Preferences.isPreferenceSet(Preferences.PREF_SHOW_OUTPUT)) {
            Preferences.setProperty(Preferences.PREF_SHOW_OUTPUT, "true");
        }

        // preset the choices.
        showOutputWindow.setSelected(Preferences.is(Preferences.PREF_SHOW_OUTPUT));
    }

    /**
     * Makes the quicklist option line in the otherPanel.
     * 
     * @param gbc the constraints used in the otherPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeQuickListOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JLabel quickListLabel = new JLabel("Recently used image list:");
        quickListLabel.setFont(MipavUtil.font12);
        quickListLabel.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(quickListLabel, gbc);
        fileMiscPanel.add(quickListLabel);

        final String[] quickListNumber = {"1", "2", "3", "4", "5", "6", "7", "8", "9"};
        quickListLevel = new JComboBox(quickListNumber);
        quickListLevel.setFont(MipavUtil.font12);
        quickListLevel.setForeground(Color.black);
        quickListLevel.setBackground(Color.white);
        quickListLevel.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(quickListLevel, gbc);
        fileMiscPanel.add(quickListLevel);

        // preset the choices:
        int quickNum = 4;
        final String levelStr = Preferences.getProperty(Preferences.PREF_QUICKLIST_NUMBER);

        if (levelStr != null) {
            quickNum = Integer.parseInt(levelStr);
        }

        quickListLevel.setSelectedIndex(quickNum - 1);
    }

    /**
     * Makes the "Save All on Save" option line in the otherPanel.
     * 
     * @param gbc the constraints used in the otherPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeSaveAllOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        saveAllCheckBox = new JCheckBox("Save all on save");
        saveAllCheckBox.setFont(MipavUtil.font12);
        saveAllCheckBox.setForeground(Color.black);
        saveAllCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(saveAllCheckBox, gbc);
        fileSavePanel.add(saveAllCheckBox);

        // preset the choices.
        saveAllCheckBox.setSelected(Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE));
    }

    /**
     * makes the "Save dialog defaults" option line in the otherPanel.
     * 
     * @param gbc GridBagConstraints the contraints
     * @param gbl GridBagLayout the layout...
     */
    protected void makeSaveDefaultsOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {

        saveDefaultsCheckBox = new JCheckBox("Allow saving of dialog defaults");
        saveDefaultsCheckBox.setFont(MipavUtil.font12);
        saveDefaultsCheckBox.setForeground(Color.black);
        saveDefaultsCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(saveDefaultsCheckBox, gbc);
        otherPanel.add(saveDefaultsCheckBox);

        // preset the choices.
        saveDefaultsCheckBox.setSelected(Preferences.is(Preferences.PREF_SAVE_DEFAULTS));
    }

    protected void makeDicomReceiverOnStartOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        dicomReceiverOnStart = new JCheckBox("Start DICOM receiver on start-up");
        dicomReceiverOnStart.setFont(MipavUtil.font12);
        dicomReceiverOnStart.setForeground(Color.black);
        dicomReceiverOnStart.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(dicomReceiverOnStart, gbc);
        otherPanel.add(dicomReceiverOnStart);

        // preset the choices.
        dicomReceiverOnStart.setSelected(Preferences.is(Preferences.PREF_AUTOSTART_DICOM_RECEIVER));
    }

    protected void makeMultiThreadingEnabledOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        multiThreadingEnabledCheckBox = new JCheckBox("Multi-Threading Enabled(" + ThreadUtil.getAvailableCores()
                + " cores)");
        multiThreadingEnabledCheckBox.setFont(MipavUtil.font12);
        multiThreadingEnabledCheckBox.setForeground(Color.black);
        multiThreadingEnabledCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(multiThreadingEnabledCheckBox, gbc);
        otherPanel.add(multiThreadingEnabledCheckBox);

        // preset the choices.
        multiThreadingEnabledCheckBox.setSelected(Preferences.isMultiThreadingEnabled()  &&
        		(ThreadUtil.getAvailableCores() > 1));
        multiThreadingEnabledCheckBox.setEnabled(ThreadUtil.getAvailableCores() > 1);
    }

    protected void makeGpuCompEnabledOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        gpuCompEnabledCheckBox = new JCheckBox("GPU Computing Enabled"); 
        gpuCompEnabledCheckBox.setFont(MipavUtil.font12);
        gpuCompEnabledCheckBox.setForeground(Color.black);
        gpuCompEnabledCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbl.setConstraints(gpuCompEnabledCheckBox, gbc);
        otherPanel.add(gpuCompEnabledCheckBox);

        // preset the choices.
        gpuCompEnabledCheckBox.setSelected(Preferences.isGpuCompEnabled() && OpenCLAlgorithmBase.isOCLAvailable());
        gpuCompEnabledCheckBox.setEnabled(OpenCLAlgorithmBase.isOCLAvailable());
        

        gpuInfoButton = new JButton("GPU Info");
        gpuInfoButton.setToolTipText(logFilename);
        gpuInfoButton.setFont(MipavUtil.font12);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gpuInfoButton.setEnabled(OpenCLAlgorithmBase.isOCLAvailable());
        gpuInfoButton.addActionListener(this);
        gpuInfoButton.setActionCommand("GPU Info");

        otherPanel.add(gpuInfoButton, gbc);
    }

    /**
     * Makes the "Always save .hdr/.img files from dialog/ in Analyze format/ in Interfile format/ in Nifti format"
     * combo box in the otherPanel.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globbalChangesPanel
     */
    protected void makeSaveHdrImgOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        saveLabel = new JLabel("Always save .hdr/.img files ");
        saveLabel.setFont(MipavUtil.font12);
        saveLabel.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(saveLabel, gbc);
        fileSavePanel.add(saveLabel);

        comboBoxSaveImgMethod = new JComboBox();
        comboBoxSaveImgMethod.setFont(MipavUtil.font12);
        comboBoxSaveImgMethod.setBackground(Color.white);
        comboBoxSaveImgMethod.addItem("from dialog choice");
        comboBoxSaveImgMethod.addItem("in Analyze format");
        comboBoxSaveImgMethod.addItem("in Interfile format");
        comboBoxSaveImgMethod.addItem("in Nifti format");
        comboBoxSaveImgMethod.setSelectedIndex(0);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(comboBoxSaveImgMethod, gbc);
        fileSavePanel.add(comboBoxSaveImgMethod);

        // preset the choices.
        if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_ANALYZE)) {
            comboBoxSaveImgMethod.setSelectedIndex(1);
        } else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_INTERFILE)) {
            comboBoxSaveImgMethod.setSelectedIndex(2);
        } else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_NIFTI)) {
            comboBoxSaveImgMethod.setSelectedIndex(3);
        }
    }

    /**
     * Makes the "Always save .mnc files from dialog/ in Minc-1.0 CDF format/ in Minc-2.0 HDF5 format" combo box in the
     * otherPanel.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globbalChangesPanel
     */
    protected void makeSaveMncOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        saveLabel = new JLabel("Always save .mnc files ");
        saveLabel.setFont(MipavUtil.font12);
        saveLabel.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(saveLabel, gbc);
        fileSavePanel.add(saveLabel);

        comboBoxSaveMncMethod = new JComboBox();
        comboBoxSaveMncMethod.setFont(MipavUtil.font12);
        comboBoxSaveMncMethod.setBackground(Color.white);
        comboBoxSaveMncMethod.addItem("from dialog choice");
        comboBoxSaveMncMethod.addItem("in Minc-1.0 CDF format");
        comboBoxSaveMncMethod.addItem("in Minc-2.0 HDF5 format");
        comboBoxSaveMncMethod.setSelectedIndex(0);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(comboBoxSaveMncMethod, gbc);
        fileSavePanel.add(comboBoxSaveMncMethod);

        // preset the choices.
        if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_MNC_AS_MINC1)) {
            comboBoxSaveMncMethod.setSelectedIndex(1);
        } else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_MNC_AS_MINC2)) {
            comboBoxSaveMncMethod.setSelectedIndex(2);
        }
    }

    /**
     * Makes the option to prompt on overwrite for saving.
     * 
     * @param gbc GridBagConstraints
     * @param gbl GridBagLayout
     */
    protected void makeSaveOverwriteOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        savePromptOverwriteBox = new JCheckBox("Prompt overwrite on save");
        savePromptOverwriteBox.setFont(MipavUtil.font12);
        savePromptOverwriteBox.setForeground(Color.black);
        savePromptOverwriteBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(savePromptOverwriteBox, gbc);
        fileSavePanel.add(savePromptOverwriteBox);

        // preset the choices.
        savePromptOverwriteBox.setSelected(Preferences.is(Preferences.PREF_SAVE_PROMPT_OVERWRITE));
    }

  

    /**
     * Makes the "Save Thumbnail for XML Files" option line in the globalChangesPanel.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeSaveXMLThumbnailOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        saveThumbnailCheckBox = new JCheckBox("Save thumbnails for MIPAV XML files");
        saveThumbnailCheckBox.setFont(MipavUtil.font12);
        saveThumbnailCheckBox.setForeground(Color.black);
        saveThumbnailCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(saveThumbnailCheckBox, gbc);
        fileSavePanel.add(saveThumbnailCheckBox);

        // preset the choices.
        saveThumbnailCheckBox.setSelected(Preferences.is(Preferences.PREF_SAVE_XML_THUMBNAIL));
    }

    /**
     * Makes the "Flip Y axis on NIFTI read of IS and PA" option line in the globalChangesPanel.
     * 
     * @param gbc the constraints used in the globalChangesPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeFlipNIFTIReadOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        flipNIFTIReadCheckBox = new JCheckBox("Flip Y Axis on NIFTI read of IS and PA");
        flipNIFTIReadCheckBox.setFont(MipavUtil.font12);
        flipNIFTIReadCheckBox.setForeground(Color.black);
        flipNIFTIReadCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(flipNIFTIReadCheckBox, gbc);
        fileSavePanel.add(flipNIFTIReadCheckBox);

        // preset the choices.
        flipNIFTIReadCheckBox.setSelected(Preferences.is(Preferences.PREF_FLIP_NIFTI_READ));
    }

    /**
     * makes the splash-screen option line in the otherPanel.
     * 
     * @param gbc the constraints used in the otherPanel
     * @param gbl the layout used in the globablChangesPanel
     */
    protected void makeSplashOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        displaySplash = new JCheckBox("Display splash screen");
        displaySplash.setFont(MipavUtil.font12);
        displaySplash.setForeground(Color.black);
        displaySplash.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(displaySplash, gbc);
        displayUserInterfacePanel.add(displaySplash);

        // preset the choices:
        displaySplash.setSelected(Preferences.is(Preferences.PREF_SHOW_SPLASH));
        
        
        openImagesInTiledFormatBox = new JCheckBox("Open images in tiled format");
        openImagesInTiledFormatBox.setFont(MipavUtil.font12);
        openImagesInTiledFormatBox.setForeground(Color.black);
        openImagesInTiledFormatBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(openImagesInTiledFormatBox, gbc);
        displayUserInterfacePanel.add(openImagesInTiledFormatBox);
        
        openImagesInTiledFormatBox.setSelected(Preferences.is(Preferences.PREF_OPEN_IMAGES_IN_TILED_FORMAT));
        
    }

    /**
     * DOCUMENT ME!
     * 
     * @param gbc2 DOCUMENT ME!
     * @param gbl DOCUMENT ME!
     */
    protected void makeVOIColorOptions(final GridBagConstraints gbc2, final GridBagLayout gbl) {
        final JLabel l1 = new JLabel("Starting VOI color:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc2.insets = new Insets(0, 0, 0, 5);
        gbc2.gridwidth = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        displayColorPanel.add(l1, gbc2);

        voiColorNames = new String[10];
        voiColorNames[0] = new String("Red");
        voiColorNames[1] = new String("Orange");
        voiColorNames[2] = new String("Yellow");
        voiColorNames[3] = new String("Light Green");
        voiColorNames[4] = new String("Green");
        voiColorNames[5] = new String("Cyan");
        voiColorNames[6] = new String("Light Blue");
        voiColorNames[7] = new String("Blue");
        voiColorNames[8] = new String("Violet");
        voiColorNames[9] = new String("Pink");

        voiColors = new Color[voiColorNames.length];

        float hue = 0.0f;

        for (int i = 0; i < voiColors.length; i++) {
            hue = (float) ( ( ( (i) * 35) % 360) / 360.0);
            voiColors[i] = Color.getHSBColor(hue, (float) 1.0, (float) 1.0);
        }

        final Integer[] intArray = new Integer[voiColorNames.length];

        for (int i = 0; i < intArray.length; i++) {
            intArray[i] = new Integer(i);
        }

        voiColorChoices = new JComboBox(intArray);
        voiColorChoices.setRenderer(new ComboBoxRenderer2());
        voiColorChoices.addActionListener(this);
        voiColorChoices.setActionCommand("color");
        gbc2.insets = new Insets(0, 0, 0, 0);
        gbc2.gridwidth = GridBagConstraints.REMAINDER;
        gbc2.anchor = GridBagConstraints.WEST;
        displayColorPanel.add(voiColorChoices, gbc2);

        final String prefColor = Preferences.getProperty(Preferences.PREF_VOI_START_COLOR);

        if (prefColor == null) {
            Preferences.setProperty(Preferences.PREF_VOI_START_COLOR, "0");
            voiColorChoices.setBackground(Color.getHSBColor(0, (float) 1.0, (float) 1.0));
            voiColorChoices.setSelectedIndex(0);
        } else {

            try {
                final int index = Integer.parseInt(prefColor);

                if ( (index < 0) || (index > (voiColorNames.length - 1))) {
                    Preferences.setProperty(Preferences.PREF_VOI_START_COLOR, "0");
                    voiColorChoices.setBackground(Color.getHSBColor(0, (float) 1.0, (float) 1.0));
                    voiColorChoices.setSelectedIndex(0);
                } else {
                    final float selectedHue = (float) ( ( ( (index) * 35) % 360) / 360.0);
                    voiColorChoices.setSelectedIndex(index);
                    voiColorChoices.setBackground(Color.getHSBColor(selectedHue, (float) 1.0, (float) 1.0));
                }
            } catch (final Exception ex) {
                Preferences.setProperty(Preferences.PREF_VOI_START_COLOR, "0");
                voiColorChoices.setBackground(Color.getHSBColor(0, (float) 1.0, (float) 1.0));
                voiColorChoices.setSelectedIndex(0);
            }
        }
    }

    /**
     * Makes the drop down list for color options for the intensity label text color
     * 
     * @param gbc2
     * @param gbl
     */
    protected void makeIntensityLabelColorOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JLabel l1 = new JLabel("Intensity label color:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);

        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(l1, gbc);
        displayColorPanel.add(l1);

        intensityLabelColorButton = new JButton();
        intensityLabelColorButton.setActionCommand("intensityLabelColor");
        intensityLabelColorButton.addActionListener(this);

        intensityLabelBackgroundButton = new JButton();
        intensityLabelBackgroundButton.setActionCommand("intensityLabelBackground");
        intensityLabelBackgroundButton.addActionListener(this);

        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(intensityLabelColorButton, gbc);
        displayColorPanel.add(intensityLabelColorButton);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(intensityLabelBackgroundButton, gbc);
        displayColorPanel.add(intensityLabelBackgroundButton);

        String prefColor = Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_COLOR);

        if (prefColor != null) {
            intensityLabelColor = MipavUtil.extractColor(prefColor);
            intensityLabelColorButton.setBackground(intensityLabelColor);
        } else {
            Preferences.setProperty(Preferences.PREF_INTENSITY_LABEL_COLOR, MipavUtil.makeColorString(Color.yellow));
            intensityLabelColorButton.setBackground(Color.yellow);
            intensityLabelColor = Color.yellow;
        }

        prefColor = Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_BACKGROUND_COLOR);

        if (prefColor != null) {
            intensityLabelBackgroundColor = MipavUtil.extractColor(prefColor);
            intensityLabelBackgroundButton.setBackground(intensityLabelBackgroundColor);
        } else {
            Preferences.setProperty(Preferences.PREF_INTENSITY_LABEL_BACKGROUND_COLOR, MipavUtil
                    .makeColorString(Color.black));
            intensityLabelBackgroundButton.setBackground(Color.black);
            intensityLabelBackgroundColor = Color.black;
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @param gbc DOCUMENT ME!
     * @param gbl DOCUMENT ME!
     */
    protected void makeVOIDrawColorOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JLabel l1 = new JLabel("VOI draw color:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(l1, gbc);
        displayColorPanel.add(l1);

        voiDrawButton = new JButton();
        voiDrawButton.setActionCommand("VOIDrawColor");
        voiDrawButton.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(voiDrawButton, gbc);
        displayColorPanel.add(voiDrawButton);

        // preset the choices.

        final String prefColor = Preferences.getProperty(Preferences.PREF_VOI_DRAW_COLOR);

        if (prefColor != null) {
            voiDrawColor = MipavUtil.extractColor(prefColor);
            voiDrawButton.setBackground(voiDrawColor);
        } else {
            Preferences.setProperty(Preferences.PREF_VOI_DRAW_COLOR, MipavUtil.makeColorString(Color.yellow));
            voiDrawButton.setBackground(Color.yellow);
            voiDrawColor = Color.yellow;

        }
    }

    /**
     * Makes the options for displaying the angle for active line VOIs.
     * 
     * @param gbc GridBagConstraints
     * @param gbl GridBagLayout
     */
    protected void makeVOILineAngleOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        showLineVOIAngleBox = new JCheckBox("Display angle for line VOIs");
        showLineVOIAngleBox.setFont(MipavUtil.font12);
        showLineVOIAngleBox.setForeground(Color.black);
        showLineVOIAngleBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(showLineVOIAngleBox, gbc);
        displayColorPanel.add(showLineVOIAngleBox);

        // preset the choices:
        showLineVOIAngleBox.setSelected(Preferences.is(Preferences.PREF_SHOW_LINE_ANGLE));
    }

    /**
     * Makes the options for displaying the angle for active line VOIs.
     * 
     * @param gbc GridBagConstraints
     * @param gbl GridBagLayout
     */
    protected void makeVOIContinuousOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        continuousVOIBox = new JCheckBox("Draw new contours without holding [SHIFT]");
        continuousVOIBox.setFont(MipavUtil.font12);
        continuousVOIBox.setForeground(Color.black);
        continuousVOIBox.addActionListener(this);
        continuousVOIBox
                .setToolTipText("If selected, new contours can be drawn in the same VOI without holding down the [SHIFT] key");
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(continuousVOIBox, gbc);
        displayColorPanel.add(continuousVOIBox);

        // preset the choices:
        continuousVOIBox.setSelected(Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR));
    }

    protected void makeVOISaveLPSOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
    	VOIGroup = new ButtonGroup();
        saveVOILPSButton = new JRadioButton("Save VOIs in LPS mm. coordinates", 
        		Preferences.is(Preferences.PREF_VOI_LPS_SAVE));
        saveVOILPSButton.setFont(MipavUtil.font12);
        saveVOILPSButton.setForeground(Color.black);
        saveVOILPSButton.addActionListener(this);
        saveVOILPSButton.setToolTipText("If selected, VOIs will be saved in LPS mm. coordinates.");
        VOIGroup.add(saveVOILPSButton);
        
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(saveVOILPSButton, gbc);
        displayColorPanel.add(saveVOILPSButton);
        
        saveVOIVoxelButton = new JRadioButton("Save VOIs in voxel coordinates", 
        		!Preferences.is(Preferences.PREF_VOI_LPS_SAVE));
        saveVOIVoxelButton.setFont(MipavUtil.font12);
        saveVOIVoxelButton.setForeground(Color.black);
        saveVOIVoxelButton.addActionListener(this);
        saveVOIVoxelButton.setToolTipText("If selected, VOIs will be saved in voxel coordinates.");
        VOIGroup.add(saveVOIVoxelButton);
        gbl.setConstraints(saveVOIVoxelButton, gbc);
        displayColorPanel.add(saveVOIVoxelButton);
    }

    /**
     * Sets the graphic type for VOI points (4 types, each with a corresponding gif).
     * 
     * @param gbc GridBagConstraints the constraints
     * @param gbl GridBagLayout the layout
     */
    protected void makeVOIPointDrawTypeOptions(final GridBagConstraints gbc, final GridBagLayout gbl) {
        final JLabel l1 = new JLabel("VOI point draw type:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(l1, gbc);
        displayColorPanel.add(l1);

        final ImageIcon[] icons = new ImageIcon[4];

        for (int i = 0; i < icons.length; i++) {
            icons[i] = MipavUtil.getIcon("pointVOI" + i + ".gif");
        }

        pointVOIChoices = new JComboBox(icons);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        displayColorPanel.add(pointVOIChoices, gbc);

        int type = 0;
        final String str = Preferences.getProperty(Preferences.PREF_VOI_POINT_DRAW_TYPE);

        if (str != null) {

            try {
                type = Integer.parseInt(str);

                if ( (type < 0) || (type > 3)) {
                    type = 0;
                }
            } catch (final Exception ex) {
                // do nothing.. colorIncrement still is 0
            }
        }

        pointVOIChoices.setSelectedIndex(type);

    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    class ComboBoxRenderer extends JLabel implements ListCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 6321204105976614332L;

        /**
         * Creates a new ComboBoxRenderer object.
         */
        public ComboBoxRenderer() {
            setOpaque(true);
            setHorizontalAlignment(SwingConstants.LEFT);
            setVerticalAlignment(SwingConstants.CENTER);
            setFont(MipavUtil.font12);
        }

        /**
         * This method finds the image and text corresponding to the selected value and returns the label, set up to
         * display the text and image.
         * 
         * @param list DOCUMENT ME!
         * @param value DOCUMENT ME!
         * @param index DOCUMENT ME!
         * @param isSelected DOCUMENT ME!
         * @param cellHasFocus DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public Component getListCellRendererComponent(final JList list, final Object value, final int index,
                final boolean isSelected, final boolean cellHasFocus) {

            // Get the selected index. (The index param isn't
            // always valid, so just use the value.)
            final int selectedIndex = ((Integer) value).intValue();

            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            // Set the icon and text. If icon was null, say so.
            ImageIcon icon = null;

            if (selectedIndex != 0) {
                icon = MipavUtil.getIcon(crosshairNames[selectedIndex]);
            }

            // String name = crosshairNames[selectedIndex];
            if (selectedIndex != 0) {
                setIcon(icon);
                setText("");
            } else {
                setIcon(null);
                setText("Default");
            }

            /*
             * setIcon(icon); if (icon != null) { name = "\t" + name.substring(9, name.length() - 4) + "\t";
             * setText(name); setFont(list.getFont()); } else { setText(name + " (no image available)"); }
             */
            return this;
        }
    }

    /**
     * DOCUMENT ME!
     */
    class ComboBoxRenderer2 extends JLabel implements ListCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -965184394601656795L;

        /**
         * Creates a new ComboBoxRenderer2 object.
         */
        public ComboBoxRenderer2() {
            setOpaque(true);
            setHorizontalAlignment(SwingConstants.LEFT);
            setVerticalAlignment(SwingConstants.CENTER);
        }

        /**
         * This method finds the image and text corresponding to the selected value and returns the label, set up to
         * display the text and image.
         * 
         * @param list DOCUMENT ME!
         * @param value DOCUMENT ME!
         * @param index DOCUMENT ME!
         * @param isSelected DOCUMENT ME!
         * @param cellHasFocus DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public Component getListCellRendererComponent(final JList list, final Object value, final int index,
                final boolean isSelected, final boolean cellHasFocus) {

            // Get the selected index. (The index param isn't
            // always valid, so just use the value.)
            final int selectedIndex = ((Integer) value).intValue();

            if (isSelected) {
                setForeground(list.getForeground());
                setBackground(voiColors[selectedIndex]);
            } else {
                setBackground(Color.LIGHT_GRAY);
                setForeground(voiColors[selectedIndex]);
            }

            final String name = voiColorNames[selectedIndex];

            if (selectedIndex != -1) {
                setIcon(null);
                setText(name);
            }

            return this;
        }
    }

    /**
     * DOCUMENT ME!
     */
    class FontBoxRenderer extends JLabel implements ListCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 7996110542578871107L;

        /**
         * Creates a new FontBoxRenderer object.
         */
        public FontBoxRenderer() {
            setOpaque(true);
            setHorizontalAlignment(SwingConstants.LEFT);
            setVerticalAlignment(SwingConstants.CENTER);
        }

        /**
         * This method finds the image and text corresponding to the selected value and returns the label, set up to
         * display the text and image.
         * 
         * @param list DOCUMENT ME!
         * @param value DOCUMENT ME!
         * @param index DOCUMENT ME!
         * @param isSelected DOCUMENT ME!
         * @param cellHasFocus DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public Component getListCellRendererComponent(final JList list, final Object value, final int index,
                final boolean isSelected, final boolean cellHasFocus) {
            final int selectedIndex = ((Integer) value).intValue();

            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            setText(labels[selectedIndex].getText());
            setFont(labels[selectedIndex].getFont());

            return this;
        }
    }

}
