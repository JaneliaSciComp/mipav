import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;

import javax.swing.*;


public class PlugInDialogNucleiStatistics extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

	private static final long serialVersionUID = 3516843154999038969L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private PlugInAlgorithmNucleiStatistics nucleiDeformAlgo = null;
    
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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogNucleiStatistics() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  modal	Whether the dialog should be made modal.
     */
    public PlugInDialogNucleiStatistics(boolean modal) {
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
            
            String message = "Finished calculating deformation statistics for " + nucleiDeformAlgo.getNumProcessedImages() + " image(s).";
            MipavUtil.displayInfo(message);

            //dispose();
            if (isExitRequired()) {
                System.exit(0);
                // ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
                return;
            }
        }
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
   
    /**
     * Once all the necessary variables are set, call PlugInAlgorithmNucleiStatistics
     */
    protected void callAlgorithm() {

        try {
            nucleiDeformAlgo = new PlugInAlgorithmNucleiStatistics(inputFiles);

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
            MipavUtil.displayError("Nuclei Statistics: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {}

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
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Nuclei Statistics 05/28/2013");
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
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        onlyProcessTiff = onlyProcessTiffCheckbox.isSelected();
        
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
