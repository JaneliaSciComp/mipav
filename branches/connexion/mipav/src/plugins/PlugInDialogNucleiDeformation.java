import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;

import java.awt.*;
import java.awt.event.*;
import java.io.File;

import javax.swing.*;


public class PlugInDialogNucleiDeformation extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

	private static final long serialVersionUID = 3516843154999038969L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private JTextField minSizeText;
    
    private int minSize = 550;
    
    private JTextField maxSizeText;
    
    private int maxSize = 5500;

    private PlugInAlgorithmNucleiDeformation nucleiDeformAlgo = null;
    
    private String inputDir;
    
    private File[] inputFiles;
    
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
    
    private static final ViewImageFileFilter tiffFilter = new ViewImageFileFilter(new String[] { ".tiff", ".tif" });
    
    private JCheckBox showResultImagesCheckbox;
    
    private boolean showResultImages = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogNucleiDeformation() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  modal	Whether the dialog should be made modal.
     */
    public PlugInDialogNucleiDeformation(boolean modal) {
        super(modal);

        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            //dispose();
        	this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else if (command.equals("BrowseDir")) {
        	final ViewDirectoryChooser chooser = new ViewDirectoryChooser();
        	String initDir = dirChooserText.getText();
        	final File initDirFile = new File(initDir);
        	if (!initDirFile.exists() || !initDirFile.isDirectory()) {
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
            int filter = ViewImageFileFilter.MICROSCOPY;

            // don't respect preference, force to MICROSCOPY
//            try {
//                filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
//            } catch (final NumberFormatException nfe) {
//
//                // an invalid value was set in preferences -- so don't use it!
//                filter = -1;
//            }

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
        		showResultImagesCheckbox.setSelected(true);
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
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof PlugInAlgorithmNucleiDeformation) {
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

            //dispose();
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
    public void setMinSize(int minSize) {
        this.minSize = minSize;
    }
    
    /**
     * 
     * @param maxSize
     */
    public void setMaxSize(int maxSize) {
        this.maxSize = maxSize;
    }
    
    public void setOnlyProcessTiff(boolean b) {
    	onlyProcessTiff = b;
    }
    
    public void setInputDir(String dir) {
    	inputDir = dir;
    	
    	File dirFile = new File(inputDir);
        if (!dirFile.exists() || !dirFile.isDirectory()) {
        	MipavUtil.displayError("Please select a directory of images to process.");
        } else {
        	if (onlyProcessTiff) {
        		inputFiles = dirFile.listFiles(tiffFilter);
        	} else {
        		inputFiles = dirFile.listFiles();
        	}
        }
    }
    
    public void setInputFile(String s) {
    	File file = new File(s);
    	if (!file.exists() || !file.isFile()) {
    		MipavUtil.displayError("Please select an image file to process.");
    	} else {
    		inputFiles = new File[] {file};
    	}
    }
    
    public void setShowResults(boolean b) {
    	showResultImages = b;
    }
   
    /**
     * Once all the necessary variables are set, call PluginAlgorithmNucleiDeformation
     */
    protected void callAlgorithm() {

        try {
            nucleiDeformAlgo = new PlugInAlgorithmNucleiDeformation(inputFiles, minSize, maxSize, showResultImages);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            nucleiDeformAlgo.addListener(this);
            createProgressBar(inputFiles[0].getParentFile().getName(), " ...", nucleiDeformAlgo);

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
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Nuclei Deformation: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
    	if (showResultImages) {
    		for (ModelImage img : nucleiDeformAlgo.getResultImages()) {
    			AlgorithmParameters.storeImageInRunner(img);
    		}
    	}
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        /*image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (image.isColorImage()) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), "Source Image must be black and white");
        }*/
    	
    	setOnlyProcessTiff(scriptParameters.getParams().getBoolean("only_tiff"));
    	boolean useDirMode = scriptParameters.getParams().getBoolean("use_dir_input_mode");
    	if (useDirMode) {
    		setInputDir(scriptParameters.getParams().getString("input_dir"));
    	} else {
    		setInputFile(scriptParameters.getParams().getString("input_file"));
    	}
    	setShowResults(scriptParameters.getParams().getBoolean("show_results"));
        setMinSize(scriptParameters.getParams().getInt("min_size"));
        setMaxSize(scriptParameters.getParams().getInt("max_size"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        //scriptParameters.storeInputImage(image);
    	scriptParameters.getParams().put(ParameterFactory.newParameter("use_dir_input_mode", dirModeButton.isSelected()));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("input_dir", inputDir));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("input_file", inputDir));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("only_tiff", onlyProcessTiff));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("show_results", showResultImages));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_size", minSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_size", maxSize));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        JLabel minSizeLabel;
        JLabel maxSizeLabel;
        setForeground(Color.black);
        setTitle("Nuclei Deformation 05/20/2013");
        //int length = image.getExtents()[0] * image.getExtents()[1];

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel mainPanel = new JPanel(new GridBagLayout());
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
        
        ButtonGroup modeGroup = new ButtonGroup();
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
        
        showResultImagesCheckbox = new JCheckBox("Display images with VOI segmentations", showResultImages);
        showResultImagesCheckbox.setForeground(Color.black);
        showResultImagesCheckbox.setFont(serif12);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 3;
        mainPanel.add(showResultImagesCheckbox, gbc);
        
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
        	showResultImagesCheckbox.setSelected(true);
        } else {
        	enableDirChooserComponents(true);
        	enableFileChooserComponents(false);
        	showResultImagesCheckbox.setSelected(false);
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
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
        int length = Integer.MAX_VALUE;
        
        onlyProcessTiff = onlyProcessTiffCheckbox.isSelected();
        
        showResultImages = showResultImagesCheckbox.isSelected();
        
        if (dirModeButton.isSelected()) {
	        inputDir = dirChooserText.getText();
	        File dirFile = new File(inputDir);
	        if (!dirFile.exists() || !dirFile.isDirectory()) {
	        	MipavUtil.displayError("Please select a directory of images to process.");
	        	return false;
	        } else {
	        	if (onlyProcessTiff) {
	        		inputFiles = dirFile.listFiles(tiffFilter);
	        	} else {
	        		inputFiles = dirFile.listFiles();
	        	}
	        }
        } else {
        	File file = new File(fileChooserText.getText());
        	if (!file.exists() || !file.isFile()) {
        		MipavUtil.displayError("Please select an image file to process.");
        		return false;
        	} else {
        		inputFiles = new File[] {file};
        	}
        }
        
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
    
    private void enableDirChooserComponents(boolean enable) {
    	dirChooserLabel.setEnabled(enable);
    	dirChooserText.setEnabled(enable);
    	dirChooserButton.setEnabled(enable);
    	onlyProcessTiffCheckbox.setEnabled(enable);
    }
    
    private void enableFileChooserComponents(boolean enable) {
    	fileChooserLabel.setEnabled(enable);
    	fileChooserText.setEnabled(enable);
    	fileChooserButton.setEnabled(enable);
    }
}
