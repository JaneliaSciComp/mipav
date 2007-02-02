package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;

import edu.sdsc.grid.io.srb.SRBAccount;

import gov.nih.mipav.model.srb.SRBFileTransferer;

/**
 * This dialog contains access to MIPAV preferences.
 *
 * @author  parsonsd
 */
public class JDialogMipavOptions extends JDialogBase implements KeyListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6756915900242085699L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

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

    /** DOCUMENT ME! */
    private JCheckBox debugFileIOBox;

    /** DOCUMENT ME! */
    private JCheckBox debugMinorBox;
    
    private JCheckBox debugScriptingBox;

    /** DOCUMENT ME! */
    private JCheckBox dicomCatcher;

    /** DOCUMENT ME! */
    private JPanel displayColorPanel;

    /** MIPAV global options private JPanel globalChangesPanel;. */
    private JPanel displayPanel;

    /** DOCUMENT ME! */
    private JCheckBox displaySplash;

    /** DOCUMENT ME! */
    private JPanel displayUserInterfacePanel;

    /** DOCUMENT ME! */
    private JCheckBox enableLoggingBox;

    /** DOCUMENT ME! */
    private int fileFilter;

    /** DOCUMENT ME! */
    private JPanel fileMiscPanel;

    /** DOCUMENT ME! */
    private JPanel filePanel;

    /** DOCUMENT ME! */
    private JPanel fileSavePanel;

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
    private JCheckBox logModeCheckBox;

    /** DOCUMENT ME! */
    private JPanel otherPanel;

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

    /** DOCUMENT ME! */
    private JCheckBox saveImgAsAnalyzeCheckBox;

    /** DOCUMENT ME! */
    private JCheckBox savePromptOverwriteBox;

    /** DOCUMENT ME! */
    private JCheckBox saveThumbnailCheckBox;

    /** DOCUMENT ME! */
    private JCheckBox saveXMLOnHDRSaveCheckBox;

    /** DOCUMENT ME! */
    private JCheckBox saveZipCheckBox;

    /** DOCUMENT ME! */
    private JCheckBox showLineVOIAngleBox;

    private JCheckBox continuousVOIBox;
    
    /** DOCUMENT ME! */
    private JCheckBox showOutputWindow;

    /** DOCUMENT ME! */
    private JCheckBox showPaintBorderBox;

    /** DOCUMENT ME! */
    private JTabbedPane tabbedPane;

    /** DOCUMENT ME! */
    private JCheckBox useAWTBox;

    /** ui must be set to access the list of images to set image-specfic options (ie,. log mode) */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JComboBox voiColorChoices;

    /** DOCUMENT ME! */
    private String[] voiColorNames;

    /** DOCUMENT ME! */
    private Color[] voiColors;

    /** DOCUMENT ME! */
    private JButton voiDrawButton, intensityLabelColorButton;

    /** DOCUMENT ME! */
    private Color voiDrawColor, intensityLabelColor;

    // SRB panel contents
    private JPanel srbPanel;
    private JComboBox srbVersionComboBox;
    private JComboBox srbTransferModeComboBox;
    private JTextField srbBaseTempDirField;
    private JButton srbTempDirBrowseButton;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * creates a dialog, builds in the options:
     *
     * <ol>
     *   <li>splash page option</lI>
     *   <li>Swing file-dialog option</li>
     *   <li>Active image color high-light option</li>
     *   <li>Debugging options</li>
     * </ol>
     *
     * <p>It then builds in the Apply and Close (window) buttons and makes the dialog visible.</p>
     */
    public JDialogMipavOptions() {
        super(ViewUserInterface.getReference().getMainFrame(), false);

        userInterface = ViewUserInterface.getReference();

        setTitle(userInterface.getAppTitle().replace(':', ' ') + "Options");

        displayPanel = new JPanel();
        displayUserInterfacePanel = new JPanel();
        displayColorPanel = new JPanel();
        filePanel = new JPanel();
        fileSavePanel = new JPanel();
        fileMiscPanel = new JPanel();
        otherPanel = new JPanel();

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;

        // make the display options
        displayUserInterfacePanel.setLayout(gbl);
        displayUserInterfacePanel.setBorder(buildTitledBorder("User interface"));
        makeSplashOptions(gbc, gbl);
        makeAWTOptions(gbc, gbl);
        makePaintToolbarOptions(gbc, gbl);
        makeQuickListOptions(gbc, gbl);

        displayColorPanel.setLayout(gbl);
        displayColorPanel.setBorder(buildTitledBorder("Color\\VOI"));
        makeVOIContinuousOptions(gbc, gbl);
        makeVOILineAngleOptions(gbc, gbl);
        makeCrosshairOptions(gbc, gbl);
        makeActiveColorOptions(gbc, gbl);
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
        makeSaveImgAsAnalyzeOptions(gbc, gbl);
        makeSaveXMLOnHDRSaveOptions(gbc, gbl);
        makeSaveXMLThumbnailOptions(gbc, gbl);
        makeSaveXMLZipOptions(gbc, gbl);
        makeFrameRateOptions(gbc, gbl);

        fileMiscPanel.setLayout(gbl);
        fileMiscPanel.setBorder(buildTitledBorder("Misc"));
        makeFileFilterOptions(gbc, gbl);

        filePanel.setLayout(new BoxLayout(filePanel, BoxLayout.Y_AXIS));
        filePanel.add(fileSavePanel);
        filePanel.add(fileMiscPanel);

        displayPanel.setLayout(new BoxLayout(displayPanel, BoxLayout.Y_AXIS));
        displayPanel.add(displayUserInterfacePanel);
        displayPanel.add(displayColorPanel);
        
        // make the other options
        otherPanel.setLayout(gbl);
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        makeSaveDefaultsOptions(gbc, gbl);
        makeLogModeOptions(gbc, gbl);
        makeLaxCheckOptions(gbc, gbl);
        makeCheckOnCloseFrameOptions(gbc, gbl);
        makeLoggingOptions(gbc, gbl);
        makeOutputWindowOptions(gbc, gbl);
        makeDebugOptions(gbc, gbl);
        makeFontOptions(gbc, gbl);
        makeSRBOptions(gbc, gbl);
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.addTab("Display", displayPanel);
        tabbedPane.addTab("File", filePanel);
        tabbedPane.addTab("SRB", srbPanel);
        tabbedPane.addTab("Other", otherPanel);

        this.getContentPane().add(tabbedPane, BorderLayout.CENTER);

        // testing to see if Boolean.getBoolean("") works yet.
        // which it doesn't in the SUN java 1.3.01 for windows
        this.getContentPane().add(makeApplyClosePanel(), BorderLayout.SOUTH);

        pack();
        this.setResizable(false);
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // *****  ACTION LISTENER
    /**
     * Calls various methods based on the user's actions.
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equalsIgnoreCase("color")) {
            int index = voiColorChoices.getSelectedIndex();
            voiColorChoices.setBackground(voiColors[index]);

        }else if (command.equalsIgnoreCase("intensityLabelColor")) {
        	colorChooser = new ViewJColorChooser(null, "Pick Active Color", new ActionListener() { // OKAY listener
                public void actionPerformed(ActionEvent ae) {
                	intensityLabelColor = colorChooser.getColor();
                	intensityLabelColorButton.setBackground(intensityLabelColor);
                }
            }, new ActionListener() { // CANCEL listener
                public void actionPerformed(ActionEvent a) { }
            });
        }
        else if (command.equalsIgnoreCase("VOIDrawColor")) {
            colorChooser = new ViewJColorChooser(null, "Pick Active Color", new ActionListener() { // OKAY listener
                    public void actionPerformed(ActionEvent ae) {
                        voiDrawColor = colorChooser.getColor();
                        voiDrawButton.setBackground(voiDrawColor);
                    }
                }, new ActionListener() { // CANCEL listener
                    public void actionPerformed(ActionEvent a) { }
                });

        } else if (command.equalsIgnoreCase("choosefilter")) {
            String filterString = (String) JOptionPane.showInputDialog(this, null, "Choose file filter",
                                                                       JOptionPane.OK_CANCEL_OPTION, null,
                                                                       ViewImageFileFilter.getDescriptions(),
                                                                       ViewImageFileFilter.getDescription(fileFilter));

            if (filterString != null) {
                fileFilter = ViewImageFileFilter.getFilterIndex(filterString);
                filterButton.setText(ViewImageFileFilter.getShortDescription(fileFilter));
                if (filterString.startsWith("User Defined")) {
                	editUserDefButton.setEnabled(true);
                }
                else {
                	editUserDefButton.setEnabled(false);
                }
            }
        } else if (command.equalsIgnoreCase("close")) { // close box
            dispose();
        } else if (command.equalsIgnoreCase("apply")) {
            // "apply" sets all the preferences in the dialog and then makes itself unset-able global preferences

            Preferences.setDebugLevels(new boolean[] {
                                           debugMinorBox.isSelected(), debugAlgorithmBox.isSelected(),
                                           debugFileIOBox.isSelected(), debugCommsBox.isSelected(),
                                           debugScriptingBox.isSelected()
                                       });

            Preferences.setProperty(Preferences.PREF_SHOW_OUTPUT, String.valueOf(showOutputWindow.isSelected()));

            // Preferences.setProperty("DEBUG", new Integer(debugLevel.getSelectedIndex()));
            Preferences.setProperty("SplashGraphics", String.valueOf(displaySplash.isSelected()));
            Preferences.setProperty(Preferences.PREF_SHOW_LINE_ANGLE, String.valueOf(showLineVOIAngleBox.isSelected()));
            Preferences.setProperty("UseAWT", String.valueOf(useAWTBox.isSelected()));
            Preferences.setProperty("ActiveImageColor", MipavUtil.makeColorString(preferredActiveColor));
            Preferences.setProperty("CrosshairCursor", crosshairNames[crosshairChoices.getSelectedIndex()]);


            Preferences.setProperty("ShowPaintBorder", String.valueOf(showPaintBorderBox.isSelected()));
            Preferences.setProperty("Log", String.valueOf(logModeCheckBox.isSelected()));
            Preferences.setProperty(Preferences.PREF_ALWAYS_SAVE_IMG_AS_ANALYZE,
                                    String.valueOf(saveImgAsAnalyzeCheckBox.isSelected()));
            Preferences.setProperty("SaveXMLOnHDRSave", String.valueOf(saveXMLOnHDRSaveCheckBox.isSelected()));
            Preferences.setProperty("SaveAllOnSave", String.valueOf(saveAllCheckBox.isSelected()));
            Preferences.setProperty("SaveDefaults", String.valueOf(saveDefaultsCheckBox.isSelected()));
            Preferences.setProperty("SavePromptOverwrite", String.valueOf(savePromptOverwriteBox.isSelected()));
            Preferences.setProperty("SaveXMLThumbnail", String.valueOf(saveThumbnailCheckBox.isSelected()));
            Preferences.setProperty("SaveXMLZip", String.valueOf(saveZipCheckBox.isSelected()));
            Preferences.setProperty("FilenameFilter", String.valueOf(fileFilter));
            Preferences.setProperty("CloseFrameCheck", String.valueOf(checkOnFrameClose.isSelected()));
            Preferences.setProperty("PerformLaxCheck", String.valueOf(performLaxCheck.isSelected()));
            Preferences.setProperty("VOIColor", String.valueOf(voiColorChoices.getSelectedIndex()));
            Preferences.setProperty("IntensityLabelColor", MipavUtil.makeColorString(intensityLabelColor));
            Preferences.setProperty("VOIDrawColor", MipavUtil.makeColorString(voiDrawColor));
            Preferences.setProperty("VOIPointDrawType", String.valueOf(pointVOIChoices.getSelectedIndex()));
            Preferences.setProperty(Preferences.PREF_CONTINUOUS_VOI_CONTOUR, String.valueOf(continuousVOIBox.isSelected()));
            
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
            } catch (Exception ex) {
                fontSizeField.selectAll();
                MipavUtil.displayError("Font size must be 4 or greater");
                fontSizeField.requestFocus();

                return;
            }

            MipavUtil.buildDefaultFonts();

            float rate = 10.0f;

            try {
                rate = Float.parseFloat(frameRateField.getText());

                if ((rate < 1.0f) || (rate > 100.0f)) {
                    frameRateField.selectAll();
                    MipavUtil.displayError("Frame rate must be between 1.0 and 100.0");
                    frameRateField.requestFocus();

                    return;
                }
            } catch (Exception ex) {
                frameRateField.selectAll();
                MipavUtil.displayError("Frame rate must be between 1.0 and 100.0");
                frameRateField.requestFocus();

                return;
            }

            // System.err.println("rate is: " + rate);
            Preferences.setProperty("DefaultFrameRate", Float.toString(rate));

            String quickStr = Preferences.getProperty("QuickListNumber");
            int quickNum = 4;
            int newNum = quickListLevel.getSelectedIndex() + 1;

            if (quickStr != null) {
                quickNum = Integer.parseInt(quickStr);
            }

            if (newNum != quickNum) {
                Preferences.setProperty("QuickListNumber", String.valueOf(newNum));
                userInterface.buildMenu();

                Vector imageFrames = userInterface.getImageFrameVector();

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

            if (Preferences.is(Preferences.PREF_LOG) && !enableLoggingBox.isSelected()) {
                Preferences.setProperty("LoggingEnabled", "false");
                LogStdStreams.turnOffLogging();
                Preferences.debug("Turned off logging");
            } else if ((Preferences.is(Preferences.PREF_LOG) && enableLoggingBox.isSelected()) &&
                           !Preferences.getProperty(Preferences.PREF_LOG_FILENAME).equalsIgnoreCase(logFilename)) {
                LogStdStreams.turnOffLogging();
                LogStdStreams.initializeErrorLogging(logFilename, "\n" + "Mipav Log: " + new Date(), true, true);
                Preferences.setProperty("LogFilename", logFilename);
            } else if (!Preferences.is(Preferences.PREF_LOG) && enableLoggingBox.isSelected()) {
                Preferences.debug("Turning on logging");
                LogStdStreams.initializeErrorLogging(logFilename, "\n" + "Mipav Log: " + new Date(), true, true);
                Preferences.setProperty("LoggingEnabled", "true");
                Preferences.setProperty("LogFilename", logFilename);
            }

            if (dicomCatcher != null) {
                Preferences.setProperty("EnableDICOMReceiver", String.valueOf(dicomCatcher.isSelected()));
            }

            // OKButton.setEnabled(false); // doesn't act correctly when open and then new image frame is added.
            if (userInterface != null) {
                int i;
                Enumeration names = userInterface.getRegisteredImageNames();
                Vector imageframelist = userInterface.getImageFrameVector();

                for (i = 0; i < imageframelist.size(); i++) { // for all viewJFrames listed in the the user interface
                                                              // list

                    // have all images get the new color out of preferences
                    try {
                        ((ViewJFrameImage) userInterface.getImageFrameVector().elementAt(i)).getComponentImage().setHighlightColor(preferredActiveColor);

                        preferredCrosshair = Preferences.getProperty("CrosshairCursor");

                        if (preferredCrosshair.equalsIgnoreCase("default")) {
                            ((ViewJFrameImage) userInterface.getImageFrameVector().elementAt(i)).getComponentImage().setCrosshairCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
                        } else {

                            try {
                                Toolkit toolkit = Toolkit.getDefaultToolkit();
                                ((ViewJFrameImage) userInterface.getImageFrameVector().elementAt(i)).getComponentImage().setCrosshairCursor(toolkit.createCustomCursor(MipavUtil.getIcon(preferredCrosshair).getImage(),
                                                                                                                                                                       new Point(12,
                                                                                                                                                                                 12),
                                                                                                                                                                       preferredCrosshair));
                            } catch (NullPointerException noIcon) {

                                // specfied icon cannot be found.  Instead, we set default:
                                ((ViewJFrameImage) userInterface.getImageFrameVector().elementAt(i)).getComponentImage().setCrosshairCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
                                Preferences.debug("JDialogMipavOptions: Crosshair icon \"" + preferredCrosshair +
                                                  "\" cannot be found.  " +
                                                  "Instead, using default crosshair pointer.\n", 2);
                            }
                        }

                    } catch (ClassCastException cce) { }
                }

                // loop through all the registered images
                while (names.hasMoreElements()) {

                    // get the ModelImage
                    ModelImage img = userInterface.getRegisteredImageByName((String) names.nextElement());

                    if (img == null) {
                        continue;
                    }

                    try {
                        img.getLightBoxFrame().setHighlightColor(preferredActiveColor);
                    } catch (NullPointerException npe) { }
                    finally {
                        img.notifyImageDisplayListeners();
                    }
                } // end while loop
            }

            // set the cancel button text to 'close' since the changes were accepted
            cancelButton.setText("Close");
            
            // Store the srb related preference information.
            if(srbBaseTempDirField.getText().length() > 0){
                Preferences.setSRBTempDir(srbBaseTempDirField.getText());
            }
            Preferences.setSRBTransferMode((String)srbTransferModeComboBox.getSelectedItem());
            Preferences.setSRBVersion((String)srbVersionComboBox.getSelectedItem());

        } else if (command.equalsIgnoreCase("active color")) {
            colorChooser = new ViewJColorChooser(null, "Pick Active Color", new ActionListener() { // OKAY listener
                    public void actionPerformed(ActionEvent ae) {
                        preferredActiveColor = colorChooser.getColor();
                        activeColor.setBackground(preferredActiveColor);
                    }
                }, new ActionListener() { // CANCEL listener
                    public void actionPerformed(ActionEvent a) { }
                });
        } else if (command.equals("ChooseLog")) {

            if (Preferences.is(Preferences.PREF_USE_AWT)) {
                FileDialog fd = new FileDialog(userInterface.getMainFrame(), "Choose log file");

                try {
                    fd.setDirectory(new File(logFilename).getParentFile().getPath());
                } catch (Exception ex) {
                    fd.setDirectory(System.getProperty("user.dir"));
                }

                Dimension d = new Dimension(700, 400);
                fd.setSize(d);

                fd.setVisible(true);

                String fileName = fd.getFile();
                String directory = fd.getDirectory();

                if (fileName != null) {
                    logFilename = directory + fileName;

                    String shortName = logFilename;

                    if (logFilename.length() > 24) {
                        shortName = ".." + logFilename.substring(logFilename.length() - 22, logFilename.length());
                    }

                    logFileButton.setText(shortName);
                    logFileButton.setToolTipText(logFilename);
                }
            } else {
                JFileChooser chooser = new JFileChooser();

                try {
                    chooser.setCurrentDirectory(new File(logFilename).getParentFile());
                } catch (Exception ex) {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                int returnVal = chooser.showSaveDialog(this);

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    logFilename = chooser.getSelectedFile().getPath();

                    String shortName = logFilename;

                    if (logFilename.length() > 24) {
                        shortName = ".." + logFilename.substring(logFilename.length() - 22, logFilename.length());
                    }

                    logFileButton.setText(shortName);
                    logFileButton.setToolTipText(logFilename);
                }
            }
        } else if (event.getSource() == enableLoggingBox) {

            if (enableLoggingBox.isSelected()) {
                logFileButton.setEnabled(true);
            } else {
                logFileButton.setEnabled(false);
            }
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10247");
        } else if(command.equals("srbTempDirBrowse")){
            JFileChooser chooser = new JFileChooser();
            if(Preferences.getSRBTempDir() != null){
                chooser.setCurrentDirectory(new File(Preferences.getSRBTempDir()));
            }
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setMultiSelectionEnabled(false);
            int retValue = chooser.showOpenDialog(srbPanel);
            if(retValue == JFileChooser.APPROVE_OPTION){
                File file = chooser.getSelectedFile();
                srbBaseTempDirField.setText(file.getAbsolutePath());
            }
        } else if (command.equals("editUserDef")) {
        	JDialogEditUserDefinedFileTypes editUserDefDialog = new JDialogEditUserDefinedFileTypes();
        	MipavUtil.centerInComponent(this,editUserDefDialog);
        }else {
            // any other button on the dialog: allow user to select "apply"
            // OKButton.setEnabled(true);  // doesn't act correctly when open and then new image frame is added.
        }

    }

    /**
     * when the image is available, there is an option there is an option to have processes operating on the image send
     * logging information to the Log pane of the output window. This method would create the Log checkbox inside the
     * MIPAV options dialog. Note that this operation was not finished, and there is no way to ensure the image and the
     * log checkbox are truly connected.
     */
    /*public void includeImageSettingsBoxGG() {
     *  //  if the user interface isn't already avail, ignore if (userInterface == null ||
     * userInterface.getImageFrameVector().size() == 0) {     return; } // image specific variables localChangesPanel =
     * new JPanel(); localChangesPanel.setBorder(buildTitledBorder("Image Settings"));     // panel gets a grid layout
     * GridBagLayout gbl = new GridBagLayout(); GridBagConstraints gbc = new GridBagConstraints(); gbc.anchor =
     * GridBagConstraints.WEST; localChangesPanel.setLayout(gbl); imageChooser = new JComboBox(); fillChooser();
     * imageChooser.setFont(MipavUtil.font12); gbc.gridheight = GridBagConstraints.REMAINDER; gbc.gridwidth = 1;
     * gbc.anchor = GridBagConstraints.NORTH; gbc.insets = new Insets(0, 0, 0, 10); gbl.setConstraints(imageChooser,
     * gbc); localChangesPanel.add(imageChooser); logMode = new JCheckBox("Use Log Mode");
     * logMode.setFont(MipavUtil.font12); gbc.gridheight = 1; gbc.gridwidth = 1; gbc.anchor = GridBagConstraints.NORTH;
     * gbl.setConstraints(logMode, gbc); localChangesPanel.add(logMode); this.getContentPane().add(localChangesPanel,
     * BorderLayout.CENTER); pack(); setVisible(true);  }*/

    /**
     * no information available.
     */
    public void fillChooser() {
        Vector uiV = userInterface.getImageFrameVector();

        if (uiV.size() > 0) {

            for (int i = uiV.size() - 1; i >= 0; i--) {
                imageChooser.addItem(((ViewJFrameImage) uiV.elementAt(i)).getTitle());
            }

            imageChooser.setSelectedItem(((ViewJFrameImage) uiV.elementAt(0)).getTitle());
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent e) {

        if (e.getSource().equals(fontChooser)) {
            fontChooser.setFont(new Font(labels[fontChooser.getSelectedIndex()].getText(), 0, 12));
        } else if (e.getSource().equals(showOutputWindow)) {
            ViewUserInterface.getReference().enableOutputWindow(showOutputWindow.isSelected());
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) {

        if (e.getKeyCode() == KeyEvent.VK_ENTER) {

            try {
                int newSize = Integer.parseInt(fontSizeField.getText());

                if (newSize >= 4) {

                    for (int i = 0; i < labels.length; i++) {
                        labels[i].setFont(new Font(labels[i].getText(), 0, newSize));
                    }

                    fontChooser.repaint();
                    fontChooser.validate();

                }
            } catch (Exception ex) { }
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyTyped(KeyEvent e) { }

    /**
     * makes the active-colour option line in the globalChangesPanel, to allow user to select the colour used to denote
     * the active image. Sets the colour to either the colour in the preferences file or to the MIPAV default.
     *
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeActiveColorOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        JLabel l1 = new JLabel("Active image border color:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(l1, gbc);
        displayColorPanel.add(l1);

        activeColor = new JButton();
        activeColor.setActionCommand("active color");
        activeColor.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(activeColor, gbc);
        displayColorPanel.add(activeColor);

        // preset the choices.
        if (Preferences.getProperty("ActiveImageColor") == null) {
            Preferences.setProperty("ActiveImageColor",
                                    MipavUtil.makeColorString(ViewJComponentEditImage.ACTIVE_IMAGE_COLOR));
            activeColor.setBackground(ViewJComponentEditImage.ACTIVE_IMAGE_COLOR);
            preferredActiveColor = ViewJComponentEditImage.ACTIVE_IMAGE_COLOR;
        } else {
            preferredActiveColor = MipavUtil.extractColor(Preferences.getProperty("ActiveImageColor"));
            activeColor.setBackground(preferredActiveColor);
        }
    }

    /**
     * makes the Apply/Close button panel, with the Apply button on the left and the Close button on the right.
     *
     * <p>The panel is created and organised, but not applied anywhere.</p>
     *
     * @return  the Panel made.
     */
    protected JPanel makeApplyClosePanel() {
        JPanel applyClosePanel = new JPanel();

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
     * DOCUMENT ME!
     *
     * @param  gbc  DOCUMENT ME!
     * @param  gbl  DOCUMENT ME!
     */
    protected void makeAWTOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        useAWTBox = new JCheckBox("Use platform-style file dialog boxes");
        useAWTBox.setFont(MipavUtil.font12);
        useAWTBox.setForeground(Color.black);
        useAWTBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(useAWTBox, gbc);
        displayUserInterfacePanel.add(useAWTBox);
        useAWTBox.setSelected(Preferences.is(Preferences.PREF_USE_AWT));
    }

    /**
     * Makes the "Check on frame close" option line in the globalChangesPanel If checked the user is required to reply
     * to a dialog to close the frame. If unchecked the frame is closed and data my be lost.
     *
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeCheckOnCloseFrameOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        checkOnFrameClose = new JCheckBox("Check on closing frame?");
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
     * Makes the options for crosshair display.
     *
     * @param  gbc2  GridBagConstraints
     * @param  gbl   GridBagLayout
     */
    protected void makeCrosshairOptions(GridBagConstraints gbc2, GridBagLayout gbl) {
        JLabel l1 = new JLabel("Crosshair cursor color:");
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

        Integer[] intArray = new Integer[crosshairNames.length];

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
        if (Preferences.getProperty("CrosshairCursor") == null) {
            Preferences.setProperty("CrosshairCursor", "default");

            // crosshairColor.setBackground(ViewJComponentEditImage.CROSSHAIR_CURSOR_COLOR);
            preferredCrosshair = "default";
        } else {
            preferredCrosshair = Preferences.getProperty("CrosshairCursor");
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
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeDebugOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        JPanel debugPanel = new JPanel(new GridBagLayout());
        debugPanel.setBorder(this.buildTitledBorder("Debug levels"));

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;

        boolean[] levels = Preferences.getDebugLevels();

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
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeFileFilterOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        JLabel fileFilterLabel = new JLabel("File filter default:");
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
        //gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(filterButton, gbc);
        fileMiscPanel.add(filterButton);
        
        

        // preset the choices:
        try {
            fileFilter = Integer.parseInt(Preferences.getProperty("FilenameFilter"));
        } catch (NumberFormatException nfe) {

            // an invalid value was set in preferences -- so ignore it!
            fileFilter = 0;
        } catch (NullPointerException npe) {

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
        }
        else {
        	editUserDefButton.setEnabled(false);
        }
        gbc.insets = new Insets(0, 25, 0, 0);
        //gbc.gridwidth = GridBagConstraints.REMAINDER;
        //gbc.anchor = GridBagConstraints.LAST_LINE_END;
        gbl.setConstraints(editUserDefButton, gbc);
        fileMiscPanel.add(editUserDefButton);

    } // end makeFileFilterOptions

    /**
     * DOCUMENT ME!
     *
     * @param  gbc  DOCUMENT ME!
     * @param  gbl  DOCUMENT ME!
     */
    protected void makeFontOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        JPanel fontPanel = new JPanel(new GridBagLayout());
        fontPanel.setBorder(this.buildTitledBorder("Font options"));

        GridBagConstraints gbc2 = new GridBagConstraints();

        String currentFont = "Serif";

        if (Preferences.getProperty(Preferences.PREF_MENU_FONT) != null) {
            currentFont = Preferences.getProperty(Preferences.PREF_MENU_FONT);
        }

        int size = 12;

        if (Preferences.getProperty(Preferences.PREF_MENU_FONT_SIZE) != null) {

            try {
                size = Integer.parseInt(Preferences.getProperty(Preferences.PREF_MENU_FONT_SIZE));
            } catch (Exception e) { }
        }

        // Get all font family names
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        fontNames = ge.getAvailableFontFamilyNames();

        labels = new JLabel[fontNames.length];

        int index = 0;
        Integer[] intArray = new Integer[labels.length];

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
        otherPanel.add(fontPanel, gbc);
    }


    /**
     * Makes options for default frame rate for saving AVIs.
     *
     * @param  gbc  GridBagConstraints
     * @param  gbl  GridBagLayout
     */
    protected void makeFrameRateOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        JLabel frameRateLabel = new JLabel("Default frame rate for Save-image-as AVI:");
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
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeLaxCheckOptions(GridBagConstraints gbc, GridBagLayout gbl) {
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
        if (!Preferences.isPreferenceSet(Preferences.PREF_LAX_CHECK)) {
            Preferences.setProperty(Preferences.PREF_LAX_CHECK, "true");
        }

        boolean flag = Preferences.is(Preferences.PREF_LAX_CHECK);
        performLaxCheck.setSelected(flag);
    }

    /**
     * Makes the Logging option line with checkbox/button in the globalChangesPanel.
     *
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeLoggingOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        boolean loggingOn = Preferences.is(Preferences.PREF_LOG);

        enableLoggingBox = new JCheckBox("Log errors to:", loggingOn);
        enableLoggingBox.setFont(MipavUtil.font12);
        enableLoggingBox.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        otherPanel.add(enableLoggingBox, gbc);
        enableLoggingBox.addActionListener(this);

        logFilename = Preferences.getProperty(Preferences.PREF_LOG_FILENAME);

        String shortName = logFilename;

        if (logFilename.length() > 24) {
            shortName = ".." + logFilename.substring(logFilename.length() - 22, logFilename.length());
        }

        logFileButton = new JButton(shortName);
        logFileButton.setToolTipText(logFilename);
        logFileButton.setFont(MipavUtil.font12);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        logFileButton.setEnabled(loggingOn);
        logFileButton.addActionListener(this);
        logFileButton.setActionCommand("ChooseLog");

        otherPanel.add(logFileButton, gbc);
    }

    /**
     * Makes the "Log Mode" option line in the globalChangesPanel.
     *
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeLogModeOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        logModeCheckBox = new JCheckBox("Record history");
        logModeCheckBox.setFont(MipavUtil.font12);
        logModeCheckBox.setForeground(Color.black);
        logModeCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbl.setConstraints(logModeCheckBox, gbc);
        otherPanel.add(logModeCheckBox);

        // preset the choices.
        logModeCheckBox.setSelected(Preferences.is(Preferences.PREF_LOG));
    }

    /**
     * takes a txt field, and forces the textfield to accept numbers, backspace, period and delete-key entries.
     *
     * @param  txt  DOCUMENT ME!
     */
    protected void makeNumericsOnly(JTextField txt) {
        txt.addKeyListener(new KeyAdapter() { // make the field
                public void keyTyped(KeyEvent evt) { // not accept letters

                    JTextField t = (JTextField) evt.getComponent();
                    char ch = evt.getKeyChar();

                    if (((ch < '0') || (ch > '9')) &&
                            ((ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE) &&
                                 (ch != KeyEvent.VK_DECIMAL) && (ch != KeyEvent.VK_PERIOD))) {

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
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeOutputWindowOptions(GridBagConstraints gbc, GridBagLayout gbl) {
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

        if (!Preferences.isPreferenceSet(Preferences.PREF_SHOW_OUTPUT)) {
            Preferences.setProperty(Preferences.PREF_SHOW_OUTPUT, "true");
        }

        // preset the choices.
        showOutputWindow.setSelected(Preferences.is(Preferences.PREF_SHOW_OUTPUT));
    }

    /**
     * Makes the "Show Paint toolbar" and the "Show paint border" option lines in the globalChangesPanel.
     *
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makePaintToolbarOptions(GridBagConstraints gbc, GridBagLayout gbl) {

        showPaintBorderBox = new JCheckBox("Show paint border");
        showPaintBorderBox.setFont(MipavUtil.font12);
        showPaintBorderBox.setForeground(Color.black);
        showPaintBorderBox.addActionListener(this);

        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;

        displayUserInterfacePanel.add(showPaintBorderBox, gbc);

        // preset the choices.
        showPaintBorderBox.setSelected(Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER));
    }


    /**
     * Makes the quicklist option line in the otherPanel.
     *
     * @param  gbc  the constraints used in the otherPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeQuickListOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        JLabel quickListLabel = new JLabel("Recently used image list:");
        quickListLabel.setFont(MipavUtil.font12);
        quickListLabel.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(quickListLabel, gbc);
        displayUserInterfacePanel.add(quickListLabel);

        String[] quickListNumber = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };
        quickListLevel = new JComboBox(quickListNumber);
        quickListLevel.setFont(MipavUtil.font12);
        quickListLevel.setForeground(Color.black);
        quickListLevel.setBackground(Color.white);
        quickListLevel.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(quickListLevel, gbc);
        displayUserInterfacePanel.add(quickListLevel);

        // preset the choices:
        int quickNum = 4;
        String levelStr = Preferences.getProperty("QuickListNumber");

        if (levelStr != null) {
            quickNum = Integer.parseInt(levelStr);
        }

        quickListLevel.setSelectedIndex(quickNum - 1);
    }

    /**
     * Makes the "Save All on Save" option line in the otherPanel.
     *
     * @param  gbc  the constraints used in the otherPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeSaveAllOptions(GridBagConstraints gbc, GridBagLayout gbl) {
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
     * @param  gbc  GridBagConstraints the contraints
     * @param  gbl  GridBagLayout the layout...
     */
    protected void makeSaveDefaultsOptions(GridBagConstraints gbc, GridBagLayout gbl) {

        saveDefaultsCheckBox = new JCheckBox("Save dialog settings");
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

    /**
     * Makes the "Always save .img files in Analyze format" option line in the otherPanel.
     *
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeSaveImgAsAnalyzeOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        saveImgAsAnalyzeCheckBox = new JCheckBox("Always save .img files in Analyze format");
        saveImgAsAnalyzeCheckBox.setFont(MipavUtil.font12);
        saveImgAsAnalyzeCheckBox.setForeground(Color.black);
        saveImgAsAnalyzeCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(saveImgAsAnalyzeCheckBox, gbc);
        fileSavePanel.add(saveImgAsAnalyzeCheckBox);

        // preset the choices.
        saveImgAsAnalyzeCheckBox.setSelected(Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_ANALYZE));
    }

    /**
     * Makes the option to prompt on overwrite for saving.
     *
     * @param  gbc  GridBagConstraints
     * @param  gbl  GridBagLayout
     */
    protected void makeSaveOverwriteOptions(GridBagConstraints gbc, GridBagLayout gbl) {
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
     * Makes the "Save XML header with Analyze images" option line in the otherPanel.
     *
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeSaveXMLOnHDRSaveOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        saveXMLOnHDRSaveCheckBox = new JCheckBox("Save XML header with Analyze images");
        saveXMLOnHDRSaveCheckBox.setFont(MipavUtil.font12);
        saveXMLOnHDRSaveCheckBox.setForeground(Color.black);
        saveXMLOnHDRSaveCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(saveXMLOnHDRSaveCheckBox, gbc);
        fileSavePanel.add(saveXMLOnHDRSaveCheckBox);

        // preset the choices.
        saveXMLOnHDRSaveCheckBox.setSelected(Preferences.is(Preferences.PREF_SAVE_XML_ON_HDR_SAVE));
    }

    /**
     * Makes the "Save Thumbnail for XML Files" option line in the globalChangesPanel.
     *
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeSaveXMLThumbnailOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        saveThumbnailCheckBox = new JCheckBox("Save thumbnails for XML files");
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
     * Makes the "Save Thumbnail for XML Files" option line in the globalChangesPanel.
     *
     * @param  gbc  the constraints used in the globalChangesPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeSaveXMLZipOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        saveZipCheckBox = new JCheckBox("Compress image in zip format when saved as XML");
        saveZipCheckBox.setFont(MipavUtil.font12);
        saveZipCheckBox.setForeground(Color.black);
        saveZipCheckBox.addActionListener(this);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(saveZipCheckBox, gbc);
        fileSavePanel.add(saveZipCheckBox);

        // preset the choices.
        saveZipCheckBox.setSelected(Preferences.is(Preferences.PREF_SAVE_XML_ZIP));
    }

    /**
     * makes the splash-screen option line in the otherPanel.
     *
     * @param  gbc  the constraints used in the otherPanel
     * @param  gbl  the layout used in the globablChangesPanel
     */
    protected void makeSplashOptions(GridBagConstraints gbc, GridBagLayout gbl) {
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
    }

    /**
     * DOCUMENT ME!
     *
     * @param  gbc2  DOCUMENT ME!
     * @param  gbl   DOCUMENT ME!
     */
    protected void makeVOIColorOptions(GridBagConstraints gbc2, GridBagLayout gbl) {
        JLabel l1 = new JLabel("Starting VOI color:");
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
            hue = (float) ((((i) * 35) % 360) / 360.0);
            voiColors[i] = Color.getHSBColor(hue, (float) 1.0, (float) 1.0);
        }

        Integer[] intArray = new Integer[voiColorNames.length];

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

        String prefColor = Preferences.getProperty("VOIColor");

        if (prefColor == null) {
            Preferences.setProperty("VOIColor", "0");
            voiColorChoices.setBackground(Color.getHSBColor(0, (float) 1.0, (float) 1.0));
            voiColorChoices.setSelectedIndex(0);
        } else {

            try {
                int index = Integer.parseInt(prefColor);

                if ((index < 0) || (index > (voiColorNames.length - 1))) {
                    Preferences.setProperty("VOIColor", "0");
                    voiColorChoices.setBackground(Color.getHSBColor(0, (float) 1.0, (float) 1.0));
                    voiColorChoices.setSelectedIndex(0);
                } else {
                    float selectedHue = (float) ((((index) * 35) % 360) / 360.0);
                    voiColorChoices.setSelectedIndex(index);
                    voiColorChoices.setBackground(Color.getHSBColor(selectedHue, (float) 1.0, (float) 1.0));
                }
            } catch (Exception ex) {
                Preferences.setProperty("VOIColor", "0");
                voiColorChoices.setBackground(Color.getHSBColor(0, (float) 1.0, (float) 1.0));
                voiColorChoices.setSelectedIndex(0);
            }
        }
    }
    
    
    
    /**
     * Makes the drop down list for color options for the intensity label text color
     *
     * @param  gbc2  
     * @param  gbl   
     */
    protected void makeIntensityLabelColorOptions(GridBagConstraints gbc, GridBagLayout gbl) {
    	JLabel l1 = new JLabel("Intensity label color:");
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
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(intensityLabelColorButton, gbc);
        displayColorPanel.add(intensityLabelColorButton);

        String prefColor = Preferences.getProperty("IntensityLabelColor");

        if (prefColor != null) {
        	intensityLabelColor = MipavUtil.extractColor(prefColor);
        	intensityLabelColorButton.setBackground(intensityLabelColor);
        } else {
            Preferences.setProperty("IntensityLabelColor", MipavUtil.makeColorString(Color.yellow));
            intensityLabelColorButton.setBackground(Color.yellow);
            intensityLabelColor = Color.yellow;

        }
        
    }
    
    

    /**
     * DOCUMENT ME!
     *
     * @param  gbc  DOCUMENT ME!
     * @param  gbl  DOCUMENT ME!
     */
    protected void makeVOIDrawColorOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        JLabel l1 = new JLabel("VOI draw color:");
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

        String prefColor = Preferences.getProperty("VOIDrawColor");

        if (prefColor != null) {
            voiDrawColor = MipavUtil.extractColor(prefColor);
            voiDrawButton.setBackground(voiDrawColor);
        } else {
            Preferences.setProperty("VOIDrawColor", MipavUtil.makeColorString(Color.yellow));
            voiDrawButton.setBackground(Color.yellow);
            voiDrawColor = Color.yellow;

        }
    }

    /**
     * Makes the options for displaying the angle for active line VOIs.
     *
     * @param  gbc  GridBagConstraints
     * @param  gbl  GridBagLayout
     */
    protected void makeVOILineAngleOptions(GridBagConstraints gbc, GridBagLayout gbl) {
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
     * @param  gbc  GridBagConstraints
     * @param  gbl  GridBagLayout
     */
    protected void makeVOIContinuousOptions(GridBagConstraints gbc, GridBagLayout gbl) {
    	continuousVOIBox = new JCheckBox("Draw new contours without holding [SHIFT]");
        continuousVOIBox.setFont(MipavUtil.font12);
        continuousVOIBox.setForeground(Color.black);
        continuousVOIBox.addActionListener(this);
        continuousVOIBox.setToolTipText("If selected, new contours can be drawn in the same VOI without holding down the [SHIFT] key");
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(continuousVOIBox, gbc);
        displayColorPanel.add(continuousVOIBox);

        // preset the choices:
        continuousVOIBox.setSelected(Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR));
    }
    
    /**
     * Sets the graphic type for VOI points (4 types, each with a corresponding gif).
     *
     * @param  gbc  GridBagConstraints the constraints
     * @param  gbl  GridBagLayout the layout
     */
    protected void makeVOIPointDrawTypeOptions(GridBagConstraints gbc, GridBagLayout gbl) {
        JLabel l1 = new JLabel("VOI point draw type:");
        l1.setFont(MipavUtil.font12);
        l1.setForeground(Color.black);
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(l1, gbc);
        displayColorPanel.add(l1);

        ImageIcon[] icons = new ImageIcon[4];

        for (int i = 0; i < icons.length; i++) {
            icons[i] = MipavUtil.getIcon("pointVOI" + i + ".gif");
        }

        pointVOIChoices = new JComboBox(icons);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        displayColorPanel.add(pointVOIChoices, gbc);

        int type = 0;
        String str = Preferences.getProperty("VOIPointDrawType");

        if (str != null) {

            try {
                type = Integer.parseInt(str);

                if ((type < 0) || (type > 3)) {
                    type = 0;
                }
            } catch (Exception ex) {
                // do nothing.. colorIncrement still is 0
            }
        }

        pointVOIChoices.setSelectedIndex(type);

    }


    // make srb options
    protected void makeSRBOptions(GridBagConstraints gbc, GridBagLayout gbl){
        srbPanel = new JPanel();
        srbPanel.setLayout(gbl);
        
        JLabel jargonVersionLabel = new JLabel("Jargon Verion : ");
        String[] availableVersions = {SRBAccount.SRB_VERSION_1_1_8, 
                                      SRBAccount.SRB_VERSION_2, 
                                      SRBAccount.SRB_VERSION_3, 
                                      SRBAccount.SRB_VERSION_3_0_2, 
                                      SRBAccount.SRB_VERSION_3_3, 
                                      SRBAccount.SRB_VERSION_3_3_1, 
                                      SRBAccount.SRB_VERSION_3_4};
        gbc.insets = new Insets(10, 10, 10, 10);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.LINE_END;

        gbl.setConstraints(jargonVersionLabel, gbc);
        srbPanel.add(jargonVersionLabel);
        srbVersionComboBox = new JComboBox(availableVersions);
        if(Preferences.getSRBVersion() != null){
            srbVersionComboBox.setSelectedItem(Preferences.getSRBVersion());
        }else{
            srbVersionComboBox.setSelectedItem(availableVersions[6]);
        }
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(srbVersionComboBox, gbc);
        srbPanel.add(srbVersionComboBox);
        
        JLabel transferModeLabel = new JLabel("Transfer Mode : ");
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(transferModeLabel, gbc);
        srbPanel.add(transferModeLabel);
        String[] transferModes = {SRBFileTransferer.TRANSFER_MODE_PARELLEL, 
                                  SRBFileTransferer.TRANSFER_MODE_SEQUENTIAL};
        srbTransferModeComboBox = new JComboBox(transferModes);
        if(Preferences.getSRBTransferMode() != null){
            srbTransferModeComboBox.setSelectedItem(Preferences.getSRBTransferMode());
        }else{
            srbTransferModeComboBox.setSelectedItem(transferModes[1]);
        }
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(srbTransferModeComboBox, gbc);
        srbPanel.add(srbTransferModeComboBox);
        
        JLabel tempDirLabel = new JLabel("Temporary Directory : ");
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(tempDirLabel, gbc);
        srbPanel.add(tempDirLabel);

        srbBaseTempDirField = new JTextField("");
        if(Preferences.getSRBTempDir() != null){
            srbBaseTempDirField.setText(Preferences.getSRBTempDir());
        }
        srbBaseTempDirField.setColumns(16);
        srbBaseTempDirField.setEditable(false);
        gbc.gridwidth = 1;
        gbl.setConstraints(srbBaseTempDirField, gbc);
        srbPanel.add(srbBaseTempDirField);

        srbTempDirBrowseButton = new JButton("Browse");
        srbTempDirBrowseButton.addActionListener(this);
        srbTempDirBrowseButton.setActionCommand("srbTempDirBrowse");
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.LAST_LINE_END;
        gbl.setConstraints(srbTempDirBrowseButton, gbc);
        srbPanel.add(srbTempDirBrowseButton);
        
    }
    
    
    //~ Inner Classes --------------------------------------------------------------------------------------------------

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
            setHorizontalAlignment(LEFT);
            setVerticalAlignment(CENTER);
            setFont(MipavUtil.font12);
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
             *       setIcon(icon);      if (icon != null) { name = "\t" + name.substring(9, name.length() - 4) + "\t";
             * setText(name); setFont(list.getFont());      }      else { setText(name + " (no image available)"); }
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
                setForeground(list.getForeground());
                setBackground(voiColors[selectedIndex]);
            } else {
                setBackground(Color.LIGHT_GRAY);
                setForeground(voiColors[selectedIndex]);
            }

            String name = voiColorNames[selectedIndex];

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
            int selectedIndex = ((Integer) value).intValue();

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
