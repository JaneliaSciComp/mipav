import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewDirectoryChooser;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewOpenFileUI;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;


public class PlugInDialogNucleiStatistics extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface,
        ItemListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    private static final long serialVersionUID = 3516843154999038969L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private PlugInAlgorithmNucleiStatistics nucleiDeformAlgo = null;

    private String inputDir;

    private final Vector<File> inputFiles = new Vector<File>();

    private JCheckBox onlyProcessTiffCheckbox;

    private boolean onlyProcessTiff = true;

    private JRadioButton dirModeButton;

    private JLabel dirChooserLabel;

    private JTextField dirChooserText;

    private JButton dirChooserButton;

    private JRadioButton fileModeButton;

    private JLabel fileChooserLabel;

    private JTextField fileChooserText;

    private JButton fileChooserButton;

    private JCheckBox forceResolutionCheckbox;

    private JLabel forceResolutionLabel;

    private JTextField forceResolutionText;

    private boolean forceResolutionFlag = true;

    private double forceResolutionValue = 0.214; // old microscope = 0.279;

    private static final ViewImageFileFilter tiffFilter = new ViewImageFileFilter(new String[] {".tiff", ".tif"});

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogNucleiStatistics() {}

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     * 
     * @param modal Whether the dialog should be made modal.
     */
    public PlugInDialogNucleiStatistics(final boolean modal) {
        super(modal);

        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            // dispose();
            this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else if (command.equals("BrowseDir")) {
            final ViewDirectoryChooser chooser = new ViewDirectoryChooser();
            String initDir = dirChooserText.getText();
            final File initDirFile = new File(initDir);
            if ( !initDirFile.exists() || !initDirFile.isDirectory()) {
                initDir = Preferences.getImageDirectory();
            }
            final String dir = chooser.chooseDirectory(initDir);
            if (dir != null) {
                dirChooserText.setText(dir);
            } else {
                dirChooserText.setText("");
            }
        } else if (command.equals("BrowseFile")) {
            final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
            fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());

            final JFileChooser chooser = fileChooser.getFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

            // default to TECH filter
            final int filter = ViewImageFileFilter.MICROSCOPY;

            // don't respect preference, force to MICROSCOPY
            // try {
            // filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
            // } catch (final NumberFormatException nfe) {
            //
            // // an invalid value was set in preferences -- so don't use it!
            // filter = -1;
            // }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

            if (filter != -1) {
                // it seems that the set command adds the filter again...
                // chooser.addChoosableFileFilter(new ViewImageFileFilter(filter));

                // if filter is something we already added, then remove it before
                // setting it..... (kludgy, kludgy....)
                final javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

                if (found != null) {
                    chooser.removeChoosableFileFilter(found);
                }

                // initially set to the preferences
                chooser.setFileFilter(new ViewImageFileFilter(filter));
            }

            final int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                final File file = chooser.getSelectedFile();
                ViewUserInterface.getReference().setDefaultDirectory(file.getParent());

                fileChooserText.setText(file.getAbsolutePath());
            } else {
                fileChooserText.setText("");
            }
        } else if (command.equals("DirMode")) {
            if (dirModeButton.isSelected()) {
                enableDirChooserComponents(true);
                enableFileChooserComponents(false);
            }
        } else if (command.equals("FileMode")) {
            if (fileModeButton.isSelected()) {
                enableDirChooserComponents(false);
                enableFileChooserComponents(true);
            }
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof PlugInAlgorithmNucleiStatistics) {
            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            if (algorithm != null) {
                algorithm.finalize();
                algorithm = null;
            }

            if (dirModeButton.isSelected()) {
                Preferences.setImageDirectory(new File(inputDir));
            }

            String message = "<html>";
            message += "Finished calculating deformation statistics for " + nucleiDeformAlgo.getNumProcessedImages()
                    + " image(s).";
            message += "<br/>";
            message += "The statistics have been saved under:";
            message += "<br/>";
            message += inputFiles.elementAt(0).getParent() + File.separator + "statistics";
            message += "</html>";
            MipavUtil.displayInfo(message);

            // dispose();
            if (isExitRequired()) {
                System.exit(0);
                // ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
                return;
            }
        }
    }

    public void setOnlyProcessTiff(final boolean b) {
        onlyProcessTiff = b;
    }

    public void setForceResolutionFlag(final boolean b) {
        forceResolutionFlag = b;
    }

    public void setForceResolutionValue(final double d) {
        forceResolutionValue = d;
    }

    public void setInputDir(final String dir) {
        inputDir = dir;

        final File dirFile = new File(inputDir);
        if ( !dirFile.exists()) {
            MipavUtil.displayError("The directory selected does not exist.  Please choose another.");
        } else if ( !dirFile.isDirectory()) {
            MipavUtil.displayError("Please select a directory of images to process.");
        } else {
            File[] dirListing;
            if (onlyProcessTiff) {
                dirListing = dirFile.listFiles(tiffFilter);
            } else {
                dirListing = dirFile.listFiles();
            }
            for (final File file : dirListing) {
                if (file.isDirectory()) {
                    System.err.println("Skipping directory:\t" + file.getName());
                } else if (file.getName().startsWith(".")) {
                    System.err.println("Skipping file that starts with .:\t" + file.getName());
                } else {
                    inputFiles.add(file);
                }
            }
        }
    }

    public void setInputFile(final String s) {
        final File file = new File(s);
        if ( !file.exists()) {
            MipavUtil.displayError("The file selected does not exist.  Please choose another.");
        } else if ( !file.isFile()) {
            MipavUtil.displayError("Please select an image file to process.");
        } else {
            inputFiles.add(file);
        }
    }

    private void enableResolutionComponents(final boolean b) {
        forceResolutionLabel.setEnabled(b);
        forceResolutionText.setEnabled(b);
    }

    /**
     * Once all the necessary variables are set, call PlugInAlgorithmNucleiStatistics
     */
    @Override
    protected void callAlgorithm() {

        try {
            nucleiDeformAlgo = new PlugInAlgorithmNucleiStatistics(inputFiles, forceResolutionFlag,
                    forceResolutionValue);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            nucleiDeformAlgo.addListener(this);
            createProgressBar(inputFiles.elementAt(0).getParentFile().getName(), " ...", nucleiDeformAlgo);
            progressBar.setVisible(true);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (nucleiDeformAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                nucleiDeformAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            MipavUtil.displayError("Nuclei Statistics: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    @Override
    protected void doPostAlgorithmActions() {}

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setGUIFromParams() {
        /*
         * image = scriptParameters.retrieveInputImage(); parentFrame = image.getParentFrame();
         * 
         * if (image.isColorImage()) { throw new ParameterException(AlgorithmParameters.getInputImageLabel(1),
         * "Source Image must be black and white"); }
         */

        setOnlyProcessTiff(scriptParameters.getParams().getBoolean("only_tiff"));
        setForceResolutionFlag(scriptParameters.getParams().getBoolean("do_force_res"));
        setForceResolutionValue(scriptParameters.getParams().getDouble("force_res_value"));
        final boolean useDirMode = scriptParameters.getParams().getBoolean("use_dir_input_mode");
        if (useDirMode) {
            setInputDir(scriptParameters.getParams().getString("input_dir"));
        } else {
            setInputFile(scriptParameters.getParams().getString("input_file"));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
        // scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("use_dir_input_mode", dirModeButton.isSelected()));
        scriptParameters.getParams().put(ParameterFactory.newParameter("input_dir", inputDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("input_file", inputDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("only_tiff", onlyProcessTiff));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_force_res", forceResolutionFlag));
        scriptParameters.getParams().put(ParameterFactory.newParameter("force_res_value", forceResolutionValue));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Nuclei Statistics 06/06/2013");
        // int length = image.getExtents()[0] * image.getExtents()[1];

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        final JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Input parameters"));

        fileModeButton = new JRadioButton("Process single image");
        fileModeButton.setFont(serif12);
        fileModeButton.setActionCommand("FileMode");
        fileModeButton.setSelected(true);
        fileModeButton.addActionListener(this);

        dirModeButton = new JRadioButton("Process directory of images");
        dirModeButton.setFont(serif12);
        dirModeButton.setActionCommand("DirMode");
        dirModeButton.setSelected(false);
        dirModeButton.addActionListener(this);

        final ButtonGroup modeGroup = new ButtonGroup();
        modeGroup.add(fileModeButton);
        modeGroup.add(dirModeButton);

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(fileModeButton, gbc);

        fileChooserLabel = new JLabel("Choose an image file to process");
        fileChooserLabel.setForeground(Color.black);
        fileChooserLabel.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(fileChooserLabel, gbc);

        fileChooserText = new JTextField(20);
        fileChooserText.setFont(serif12);
        fileChooserText.setText("");
        gbc.gridx++;
        mainPanel.add(fileChooserText, gbc);

        fileChooserButton = new JButton("Browse");
        fileChooserButton.setActionCommand("BrowseFile");
        fileChooserButton.addActionListener(this);
        fileChooserButton.setFont(serif12B);
        gbc.gridx++;
        mainPanel.add(fileChooserButton, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(dirModeButton, gbc);

        dirChooserLabel = new JLabel("Choose the directory containing the images to process");
        dirChooserLabel.setForeground(Color.black);
        dirChooserLabel.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(dirChooserLabel, gbc);

        dirChooserText = new JTextField(20);
        dirChooserText.setFont(serif12);
        dirChooserText.setText(Preferences.getImageDirectory());
        gbc.gridx++;
        mainPanel.add(dirChooserText, gbc);

        dirChooserButton = new JButton("Browse");
        dirChooserButton.setActionCommand("BrowseDir");
        dirChooserButton.addActionListener(this);
        dirChooserButton.setFont(serif12B);
        gbc.gridx++;
        mainPanel.add(dirChooserButton, gbc);

        onlyProcessTiffCheckbox = new JCheckBox("Process only TIFF files", onlyProcessTiff);
        onlyProcessTiffCheckbox.setForeground(Color.black);
        onlyProcessTiffCheckbox.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 3;
        mainPanel.add(onlyProcessTiffCheckbox, gbc);

        forceResolutionCheckbox = new JCheckBox("Change image resolution", forceResolutionFlag);
        forceResolutionCheckbox.setForeground(Color.black);
        forceResolutionCheckbox.setFont(serif12);
        forceResolutionCheckbox.addItemListener(this);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 3;
        mainPanel.add(forceResolutionCheckbox, gbc);

        forceResolutionLabel = new JLabel("New resolution per pixel (micrometers)");
        forceResolutionLabel.setForeground(Color.black);
        forceResolutionLabel.setFont(serif12);
        gbc.gridy++;
        gbc.gridwidth = 2;
        mainPanel.add(forceResolutionLabel, gbc);

        forceResolutionText = new JTextField("" + forceResolutionValue);
        forceResolutionText.setFont(serif12);
        gbc.gridx++;
        gbc.gridwidth = 1;
        mainPanel.add(forceResolutionText, gbc);

        enableResolutionComponents(forceResolutionFlag);

        if (fileModeButton.isSelected()) {
            enableDirChooserComponents(false);
            enableFileChooserComponents(true);
        } else {
            enableDirChooserComponents(true);
            enableFileChooserComponents(false);
        }

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        onlyProcessTiff = onlyProcessTiffCheckbox.isSelected();

        forceResolutionFlag = forceResolutionCheckbox.isSelected();
        forceResolutionValue = Double.parseDouble(forceResolutionText.getText());

        if (dirModeButton.isSelected()) {
            inputDir = dirChooserText.getText();
            final File dirFile = new File(inputDir);
            if ( !dirFile.exists()) {
                MipavUtil.displayError("The directory selected does not exist.  Please choose another.");
                return false;
            } else if ( !dirFile.isDirectory()) {
                MipavUtil.displayError("Please select a directory of images to process.");
                return false;
            } else {
                File[] dirListing;
                if (onlyProcessTiff) {
                    dirListing = dirFile.listFiles(tiffFilter);
                } else {
                    dirListing = dirFile.listFiles();
                }
                for (final File file : dirListing) {
                    if (file.isDirectory()) {
                        System.err.println("Skipping directory:\t" + file.getName());
                    } else if (file.getName().startsWith(".")) {
                        System.err.println("Skipping file that starts with .:\t" + file.getName());
                    } else {
                        inputFiles.add(file);
                    }
                }
            }
        } else {
            final File file = new File(fileChooserText.getText());
            if ( !file.exists()) {
                MipavUtil.displayError("The file selected does not exist.  Please choose another.");
                return false;
            } else if ( !file.isFile()) {
                MipavUtil.displayError("Please select an image file to process.");
                return false;
            } else {
                inputFiles.add(file);
            }
        }

        return true;
    }

    private void enableDirChooserComponents(final boolean enable) {
        dirChooserLabel.setEnabled(enable);
        dirChooserText.setEnabled(enable);
        dirChooserButton.setEnabled(enable);
        onlyProcessTiffCheckbox.setEnabled(enable);
    }

    private void enableFileChooserComponents(final boolean enable) {
        fileChooserLabel.setEnabled(enable);
        fileChooserText.setEnabled(enable);
        fileChooserButton.setEnabled(enable);
    }

    @Override
    public void itemStateChanged(final ItemEvent event) {
        if (event.getSource() == forceResolutionCheckbox) {
            enableResolutionComponents(forceResolutionCheckbox.isSelected());
        }
    }
}
