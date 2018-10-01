import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewDirectoryChooser;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewOpenFileUI;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;


public class PlugInDialogNucleiSegmentation extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    private static final long serialVersionUID = 3516843154999038969L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private JTextField minSizeText;

    private int minSize = 550;

    private JTextField maxSizeText;

    private int maxSize = 40000;

    private PlugInAlgorithmNucleiSegmentation nucleiDeformAlgo = null;

    private String inputDir;

    private Vector<File> inputFiles = new Vector<File>();

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

    private static final ViewImageFileFilter tiffFilter = new ViewImageFileFilter(new String[] {".tiff", ".tif"});

    private JCheckBox interactiveVOIReviewCheckbox;

    private boolean doInteractiveVOIReview = true;
    
    private JCheckBox splitCheckbox;
    
    private boolean splitImgFlag = true;
    
    private JLabel splitNumLabel;
    
    private JTextField splitNumText;
    
    private JLabel splitSizeLabel;
    
    private JTextField splitSizeText;
    
    private int splitImgNum = 2;
    
    private int splitImgSizeThreshMB = 50;
    
    private static final String svnVersion = "$Rev$";

    private static final String svnLastUpdate = "$Date$";
    
    private static final String pluginVersion = MipavUtil.getSVNChangedDate(svnLastUpdate);

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogNucleiSegmentation() {}

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     * 
     * @param modal Whether the dialog should be made modal.
     */
    public PlugInDialogNucleiSegmentation(final boolean modal) {
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
    
    public void itemStateChanged(ItemEvent e) {
        if (e.getSource() == splitCheckbox) {
            splitNumLabel.setEnabled(splitCheckbox.isSelected());
            splitNumText.setEnabled(splitCheckbox.isSelected());
            splitSizeLabel.setEnabled(splitCheckbox.isSelected());
            splitSizeText.setEnabled(splitCheckbox.isSelected());
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

        if (algorithm instanceof PlugInAlgorithmNucleiSegmentation) {
            for (final ModelImage img : nucleiDeformAlgo.getResultImages()) {
                if (img.getFileInfo() != null) {
                    if (doInteractiveVOIReview) {
                        final boolean origVOINameSetting = Preferences.is(Preferences.PREF_SHOW_VOI_NAME);
                        ViewUserInterface.getReference().setUseVOIName(true);

                        // display the image and it's VOIs
                        final ViewJFrameImage frame = new ViewJFrameImage(img);

                        final InteractiveReviewDialog dialog = new InteractiveReviewDialog(frame, img);
                        
                        frame.setVisible(true);

                        // keep waiting until the user closes the dialog
                        while ( !dialog.isDoneWithReview()) {
                            // do nothing
                        }

                        frame.setVisible(false);

                        ViewUserInterface.getReference().setUseVOIName(origVOINameSetting);
                        ViewUserInterface.getReference().unregisterFrame(frame);
                    }

                    // save all VOIs to disk for this image
                    saveAllVOIs(img);
                    img.disposeLocal(false);
                } // if (img.getFileInfo() != null)
            }

            String message = "<html>";
            message += "Finished segmenting nuclei for " + nucleiDeformAlgo.getResultImages().size() + " image(s).";
            message += "<br/>";
            message += "Nuclei VOIs have been saved for each image under:";
            message += "<br/>";
            message += inputFiles.elementAt(0).getParent();
            message += "</html>";

            MipavUtil.displayInfo(message);

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

            // dispose();
            if (isExitRequired()) {
                System.exit(0);
                // ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
                return;
            }
        }
    }

    /**
     * 
     * @param minSize
     */
    public void setMinSize(final int minSize) {
        this.minSize = minSize;
    }

    /**
     * 
     * @param maxSize
     */
    public void setMaxSize(final int maxSize) {
        this.maxSize = maxSize;
    }

    public void setOnlyProcessTiff(final boolean b) {
        onlyProcessTiff = b;
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
    
    public void splitInputFiles(boolean splitFlag, int splitTiles, int splitThreshMB) {
        setSplitImgFlag(splitFlag);
        setSplitImgNum(splitTiles);
        setSplitImgSizeThresholdMB(splitThreshMB);
        
        if (splitImgFlag) {
            Vector<File> newFileList = new Vector<File>(inputFiles.size() * splitImgNum * 2);
            for (File origFile : inputFiles) {
                long sizeMB = origFile.length() / 1024 / 1024;
                
                if (sizeMB > this.splitImgSizeThreshMB) {
                    File[] splitFiles = splitFile(origFile, splitImgNum);
                    
                    for (File file : splitFiles) {
                        newFileList.add(file);
                    }
                } else {
                    newFileList.add(origFile);
                }
            }
            
            inputFiles = newFileList;
        }
    }

    public void setDoInteractiveVOIReview(final boolean b) {
        doInteractiveVOIReview = b;
    }
    
    public void setSplitImgFlag(boolean splitFlag) {
        splitImgFlag = splitFlag;
    }
    
    public void setSplitImgNum(int numTiles) {
        splitImgNum = numTiles;
    }
    
    public void setSplitImgSizeThresholdMB(int splitThreshMB) {
        splitImgSizeThreshMB = splitThreshMB;
    }

    /**
     * Once all the necessary variables are set, call PluginAlgorithmNucleiSegmentation
     */
    @Override
    protected void callAlgorithm() {

        try {
            nucleiDeformAlgo = new PlugInAlgorithmNucleiSegmentation(inputFiles, minSize, maxSize);

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
            MipavUtil.displayError("Nuclei Segmentation: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    @Override
    protected void doPostAlgorithmActions() {
        if (doInteractiveVOIReview) {
            for (final ModelImage img : nucleiDeformAlgo.getResultImages()) {
                AlgorithmParameters.storeImageInRunner(img);
            }
        }
    }

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
        final boolean useDirMode = scriptParameters.getParams().getBoolean("use_dir_input_mode");
        if (useDirMode) {
            setInputDir(scriptParameters.getParams().getString("input_dir"));
        } else {
            setInputFile(scriptParameters.getParams().getString("input_file"));
        }
        setMinSize(scriptParameters.getParams().getInt("min_size"));
        setMaxSize(scriptParameters.getParams().getInt("max_size"));
        
        splitInputFiles(scriptParameters.getParams().getBoolean("split_img"), scriptParameters.getParams().getInt("split_img_tiles"), scriptParameters.getParams().getInt("split_img_thresh_mb"));
        
        // no interactive review while in script
        setDoInteractiveVOIReview(false);
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
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_size", minSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_size", maxSize));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("split_img", splitImgFlag));
        scriptParameters.getParams().put(ParameterFactory.newParameter("split_img_tiles", splitImgNum));
        scriptParameters.getParams().put(ParameterFactory.newParameter("split_img_thresh_mb", splitImgSizeThreshMB));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        JLabel minSizeLabel;
        JLabel maxSizeLabel;
        setForeground(Color.black);
        setTitle("Nuclei Segmentation - " + pluginVersion);
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
        
        splitCheckbox = new JCheckBox("Split large images into smaller tiles", splitImgFlag);
        splitCheckbox.setForeground(Color.black);
        splitCheckbox.setFont(serif12);
        splitCheckbox.addItemListener(this);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 3;
        mainPanel.add(splitCheckbox, gbc);
        
        splitNumLabel = new JLabel("Number of tiles in each dimension (e.g., 2x2)");
        splitNumLabel.setForeground(Color.black);
        splitNumLabel.setFont(serif12);
        splitNumLabel.setEnabled(splitImgFlag);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 1;
        mainPanel.add(splitNumLabel, gbc);

        splitNumText = new JTextField(8);
        splitNumText.setText(String.valueOf(splitImgNum));
        splitNumText.setFont(serif12);
        splitNumText.setEnabled(splitImgFlag);
        gbc.gridx = 1;
        gbc.gridwidth = 2;
        mainPanel.add(splitNumText, gbc);
        
        splitSizeLabel = new JLabel("Image file size threshold for splitting (in MB)");
        splitSizeLabel.setForeground(Color.black);
        splitSizeLabel.setFont(serif12);
        splitSizeLabel.setEnabled(splitImgFlag);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 1;
        mainPanel.add(splitSizeLabel, gbc);

        splitSizeText = new JTextField(8);
        splitSizeText.setText(String.valueOf(splitImgSizeThreshMB));
        splitSizeText.setFont(serif12);
        splitSizeText.setEnabled(splitImgFlag);
        gbc.gridx = 1;
        gbc.gridwidth = 2;
        mainPanel.add(splitSizeText, gbc);

        interactiveVOIReviewCheckbox = new JCheckBox("Interactively review/delete nuclei segmentations",
                doInteractiveVOIReview);
        interactiveVOIReviewCheckbox.setForeground(Color.black);
        interactiveVOIReviewCheckbox.setFont(serif12);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 3;
        mainPanel.add(interactiveVOIReviewCheckbox, gbc);

        minSizeLabel = new JLabel("Minimum nucleus pixel count");
        minSizeLabel.setForeground(Color.black);
        minSizeLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 1;
        mainPanel.add(minSizeLabel, gbc);

        minSizeText = new JTextField(8);
        minSizeText.setText(String.valueOf(minSize));
        minSizeText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridwidth = 2;
        mainPanel.add(minSizeText, gbc);

        maxSizeLabel = new JLabel("Maximum nucleus pixel count");
        maxSizeLabel.setForeground(Color.black);
        maxSizeLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 1;
        mainPanel.add(maxSizeLabel, gbc);

        maxSizeText = new JTextField(8);
        maxSizeText.setText(String.valueOf(maxSize));
        maxSizeText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridwidth = 2;
        mainPanel.add(maxSizeText, gbc);

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
        String tmpStr;

        final int length = Integer.MAX_VALUE;

        onlyProcessTiff = onlyProcessTiffCheckbox.isSelected();

        doInteractiveVOIReview = interactiveVOIReviewCheckbox.isSelected();

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
        
        splitInputFiles(splitCheckbox.isSelected(), Integer.parseInt(splitNumText.getText()), Integer.parseInt(splitSizeText.getText()));

        tmpStr = minSizeText.getText();
        minSize = Integer.parseInt(tmpStr);

        if (minSize < 1) {
            MipavUtil.displayError("Nucleus minimum pixel size must be at least 1");
            minSizeText.requestFocus();
            minSizeText.selectAll();

            return false;
        } else if (minSize > length) {
            MipavUtil.displayError("Nucleus minimum pixel size must not exceed " + length);
            minSizeText.requestFocus();
            minSizeText.selectAll();

            return false;
        }

        tmpStr = maxSizeText.getText();
        maxSize = Integer.parseInt(tmpStr);

        if (maxSize < minSize) {
            MipavUtil.displayError("Nucleus maximum pixel size must be at least " + minSize);
            maxSizeText.requestFocus();
            maxSizeText.selectAll();

            return false;
        } else if (maxSize > length) {
            MipavUtil.displayError("Nucleus maximum pixel size must not exceed " + length);
            maxSizeText.requestFocus();
            maxSizeText.selectAll();

            return false;
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

    /**
     * This method saves all VOIs for the active image to the default VOI directory for that image.
     * 
     * @param img The image for which to save VOIs to disk.
     */
    private static final void saveAllVOIs(final ModelImage img) {
        String fileDir;
        String tmpImageName;
        String imageName;
        String voiDir;
        fileDir = img.getFileInfo(0).getFileDirectory();

        // if the image is a dicom image, then base the new directory name
        // on the actual filename, not the image name
        if (img.isDicomImage()) {
            tmpImageName = img.getFileInfo(0).getFileName();

            final int index = tmpImageName.lastIndexOf(".");

            if (index > 0) {
                tmpImageName = tmpImageName.substring(0, index);
            }

            // now, get rid of any numbers at the end of the name (these
            // are part of the dicom file name, but we only want the 'base'
            // part of the name
            int newIndex = tmpImageName.length();

            for (int i = tmpImageName.length() - 1; i >= 0; i--) {
                final char myChar = tmpImageName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex == 0) {

                // give the base name a generic name
                tmpImageName = new String("DICOM");
            } else {
                tmpImageName = tmpImageName.substring(0, newIndex);
            }
        } else {
            tmpImageName = img.getImageName();
        }

        // get rid of any '^' and ',' which may exist in dicom images
        imageName = tmpImageName.replace('^', '_');
        imageName = imageName.replace(',', '_');

        voiDir = new String(fileDir + File.separator + "defaultVOIs_" + imageName + File.separator);

        try {
            final ViewVOIVector VOIs = img.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            final int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                    final FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, img);
                    fileVOI.writeXML(VOIs.VOIAt(i), true, true);
                } else {
                    final FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, img);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), true);
                }
            }
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }
    }
    
    private File[] splitFile(File origFile, int numTiles) {
        File[] splitFiles = new File[numTiles * 2];
        
        ModelImage origImg = openFile(origFile);
        
        String tileFileBase = FileUtility.stripExtension(origFile.getName());
        
        int tileXSize = origImg.getExtents()[0] / numTiles;
        int tileYSize = origImg.getExtents()[1] / numTiles;
        
        int tileXLeftover = origImg.getExtents()[0] - (tileXSize * numTiles);
        int tileYLeftover = origImg.getExtents()[0] - (tileYSize * numTiles);
        
        int tileIndex = 0;
        for (int x = 0; x < numTiles; x++) {
            for (int y = 0; y < numTiles; y++) {
                int xLoc = x * tileXSize;
                int yLoc = y * tileYSize;
                
                ModelImage tileImg = cropImage(origImg, xLoc, yLoc, tileXSize, tileYSize);
                
                splitFiles[tileIndex] = saveImageFile(tileImg, origFile.getParent(), tileFileBase + "_" + (x+1) + "x" + (y+1), FileUtility.TIFF);
                
                if (tileImg != null) {
                    // don't garbage collect until we clean up the original img
                    tileImg.disposeLocal(false);
                }
                
                tileIndex++;
            }
        }
        
        if (origImg != null) {
            origImg.disposeLocal();
        }
        
        return splitFiles;
    }
    
    private ModelImage cropImage(ModelImage origImg, int x, int y, int xSize, int ySize) {
        int[] destExtents = null;
        
        int[] xBounds = new int[2];
        int[] yBounds = new int[2];
        
        xBounds[0] = x;
        xBounds[1] = x + xSize;
        yBounds[0] = y;
        yBounds[1] = y + ySize;

        destExtents = new int[2];
        destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1;
        destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1;

        // Make result image
        ModelImage resultImage = new ModelImage(origImg.getType(), destExtents, makeImageName(origImg.getImageName(), "_crop"));
        xBounds[0] *= -1;
        xBounds[1] = resultImage.getExtents()[0] - origImg.getExtents()[0];
        yBounds[0] *= -1;
        yBounds[1] = resultImage.getExtents()[1] - origImg.getExtents()[1];
        AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(origImg, resultImage, xBounds, yBounds, new int[] {0,0});

        cropAlgo.run();
        
        return resultImage;
    }
    
    /**
     * Try to open an image file.
     * @param file The file to try to open.
     * @return The ModelImage of the specified file, or null if there was an error.
     */
    private static final ModelImage openFile(File file) {
        ModelImage img = null;
        
        System.err.println("Trying to open file:\t" + file.getName());
        
        FileIO io = new FileIO();
        try {
            img = io.readImage(file.getAbsolutePath());
        } catch (Exception e) {
            System.err.println("Failed to open file:\t" + file.getName());
            e.printStackTrace();
            img = null;
        }
        
        return img;
    }
    
    private File saveImageFile(final ModelImage img, final String dir, final String fileBasename, int fileType) {
        FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);
        
        FileWriteOptions opts = new FileWriteOptions(true);
        opts.setFileDirectory(dir + File.separator);

        if (img.getNDims() == 3) {
            opts.setBeginSlice(0);
            opts.setEndSlice(img.getExtents()[2] - 1);
        } else if (img.getNDims() == 4) {
            opts.setBeginSlice(0);
            opts.setEndSlice(img.getExtents()[2] - 1);
            opts.setBeginTime(0);
            opts.setEndTime(img.getExtents()[3] - 1);
        }

        opts.setFileType(fileType);
        final String ext = FileTypeTable.getFileTypeInfo(fileType).getDefaultExtension();
        opts.setFileName(fileBasename + ext);

        opts.setOptionsSet(true);
        opts.setMultiFile(false);

        fileIO.writeImage(img, opts, false, false);
        
        return new File(dir + File.separator + fileBasename + ext);
    }

    protected class InteractiveReviewDialog extends JDialog implements ActionListener, ItemListener {
        private final ModelImage img;

        private boolean doneWithReviewFlag = false;

        private final VOIVector origVOIs;

        private final Vector<JCheckBox> voiCheckboxList = new Vector<JCheckBox>();

        private int origVoiThickness = 1;

        public InteractiveReviewDialog(final Frame parent, final ModelImage img) {
            super(parent, false);

            this.img = img;

            origVOIs = img.getVOIsCopy();

            init();

            if (Preferences.isPreferenceSet(Preferences.PREF_VOI_THICKNESS)) {
                origVoiThickness = Preferences.getVOIThickness();
            }
        }

        private void init() {
            setTitle("Review nuclei sementations for image: " + img.getImageName());
            setForeground(Color.black);

            final GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.weightx = 1;
            gbc.weighty = 1;
            // gbc.insets = new Insets(3, 3, 3, 3);
            gbc.fill = GridBagConstraints.HORIZONTAL;

            final JPanel mainPanel = new JPanel(new GridBagLayout());
            mainPanel.setForeground(Color.black);

            final JToggleButton showVoiButton = new JToggleButton("Show VOI borders", true);
            showVoiButton.setFont(serif12B);
            showVoiButton.addItemListener(this);
            gbc.gridx = 0;
            gbc.gridy = 0;
            mainPanel.add(showVoiButton, gbc);
            
            final JPanel voiPanel = new JPanel(new GridLayout(0, 1));
            voiPanel.setForeground(Color.black);
            voiPanel.setBackground(Color.darkGray);
            final JScrollPane scrollPane = new JScrollPane(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            for (final VOI voi : img.getVOIs()) {
                final JCheckBox checkbox = new JCheckBox(voi.getName(), true);
                checkbox.setForeground(voi.getColor());
                checkbox.setBackground(Color.darkGray);
                checkbox.addItemListener(this);
                voiCheckboxList.add(checkbox);
                voiPanel.add(checkbox);
            }
            
            scrollPane.setViewportView(voiPanel);
            Dimension panelSize = voiPanel.getPreferredSize();
            if (panelSize.height > 700) {
                scrollPane.setPreferredSize(new Dimension(panelSize.width, 700));
            }
            
            gbc.gridy++;
            mainPanel.add(new JLabel("Uncheck the box next to any nucleus segmentations that should be removed."), gbc);
            gbc.gridy++;
            gbc.fill = GridBagConstraints.BOTH;
            gbc.weighty = 2;
            mainPanel.add(scrollPane, gbc);

            final JPanel buttonPanel = new JPanel(new GridLayout());
            buttonPanel.setForeground(Color.black);

            final JButton saveButton = new JButton("Save only checked VOIs");
            saveButton.setFont(serif12B);
            saveButton.addActionListener(this);
            buttonPanel.add(saveButton);

            // final JButton discardButton = new JButton("Save all VOIs");
            // discardButton.setFont(serif12B);
            // discardButton.addActionListener(this);
            // buttonPanel.add(discardButton);

            getContentPane().setLayout(new GridBagLayout());
            final GridBagConstraints paneGBC = new GridBagConstraints();
            paneGBC.insets = new Insets(10, 10, 10, 10);
            paneGBC.gridx = 0;
            paneGBC.gridy = 0;
            paneGBC.weighty = 3;
            getContentPane().add(mainPanel, paneGBC);
            paneGBC.gridy++;
            paneGBC.weighty = 1;
            getContentPane().add(buttonPanel, paneGBC);

            this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
            this.addWindowListener(new WindowAdapter() {
                @Override
                public void windowClosing(final WindowEvent event) {
                    final int confirm = JOptionPane.showOptionDialog(getParent(),
                            "Remove unchecked VOIs before saving to disk?", "Exit Confirmation",
                            JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE, null, null, null);
                    if (confirm == JOptionPane.YES_OPTION) {
                        for (int i = 0; i < voiCheckboxList.size(); i++) {
                            if ( !voiCheckboxList.elementAt(i).isSelected()) {
                                for (final VOI voi : img.getVOIs()) {
                                    if (voi.getID() == origVOIs.elementAt(i).getID()) {
                                        img.unregisterVOI(voi);
                                        break;
                                    }
                                }
                            }
                        }
                        setDoneWithReview(true);
                        dispose();
                    } else if (confirm == JOptionPane.NO_OPTION) {
                        img.setVOIs(origVOIs);
                        setDoneWithReview(true);
                        dispose();
                    } else {
                        // do nothing
                    }
                }
            });

            pack();
            
            // MipavUtil.centerOnScreen(this);
            final Dimension size = getSize();
            //size.height += 20;
            if (size.height > 1000) {
                size.height = 1000;
            }
            size.width += 20;
//            setSize(size);
            this.setPreferredSize(size);
            
            setLocation(new Point(0,0));
            this.getParent().setLocation(new Point(getSize().width, 0));
            
//            final Point loc = this.getParent().getLocationOnScreen();
//            loc.x = loc.x - getSize().width;
//            setLocation(loc);
            
            setVisible(true);
            setResizable(true);
            System.gc();
        }

        @Override
        public void actionPerformed(final ActionEvent event) {
            final String command = event.getActionCommand();
            if (command.equals("Save only checked VOIs")) {
                for (int i = 0; i < voiCheckboxList.size(); i++) {
                    if ( !voiCheckboxList.elementAt(i).isSelected()) {
                        for (final VOI voi : img.getVOIs()) {
                            if (voi.getID() == origVOIs.elementAt(i).getID()) {
                                img.unregisterVOI(voi);
                                break;
                            }
                        }
                    }
                }
                setDoneWithReview(true);
                dispose();
            } else if (command.equals("Save all VOIs")) {
                img.setVOIs(origVOIs);
                setDoneWithReview(true);
                dispose();
            }
        }

        @Override
        public void itemStateChanged(final ItemEvent event) {
            if (event.getItem() instanceof JCheckBox) {
                final JCheckBox source = (JCheckBox) event.getItem();
            } else if (event.getItem() instanceof JToggleButton) {
                final boolean doShowVoiBorder = ((JToggleButton) event.getItem()).isSelected();

                final VOIVector vois = img.getVOIs();
                for (final VOI voi : vois) {
                    if (doShowVoiBorder) {
                        voi.setThickness(origVoiThickness);
                        // Preferences.setProperty(Preferences.PREF_VOI_THICKNESS, Integer.toString(thickChange));
                        voi.update();
                        // updateVOIPanel(voi, image);
                    } else {
                        voi.setThickness(0);
                        // Preferences.setProperty(Preferences.PREF_VOI_THICKNESS, Integer.toString(thickChange));
                        voi.update();
                        // updateVOIPanel(voi, image);
                    }
                }
                img.notifyImageDisplayListeners();
            }
        }

        public synchronized void setDoneWithReview(final boolean b) {
            doneWithReviewFlag = b;
        }

        public synchronized boolean isDoneWithReview() {
            return doneWithReviewFlag;
        }
    }
}
