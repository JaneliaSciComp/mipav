import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;
import java.util.Vector;

import javax.swing.*;


public class PlugInDialogNucleiSegmentation extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

	private static final long serialVersionUID = 3516843154999038969L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private JTextField minSizeText;
    
    private int minSize = 550;
    
    private JTextField maxSizeText;
    
    private int maxSize = 5500;

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
    
    private static final ViewImageFileFilter tiffFilter = new ViewImageFileFilter(new String[] { ".tiff", ".tif" });
    
    private JCheckBox interactiveVOIReviewCheckbox;
    
    private boolean doInteractiveVOIReview = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogNucleiSegmentation() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  modal	Whether the dialog should be made modal.
     */
    public PlugInDialogNucleiSegmentation(boolean modal) {
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

        if (algorithm instanceof PlugInAlgorithmNucleiSegmentation) {
	        for (ModelImage img : nucleiDeformAlgo.getResultImages()) {
	            if (img.getFileInfo() != null) {
    	        	if (doInteractiveVOIReview) {
    	        		boolean origVOINameSetting = Preferences.is(Preferences.PREF_SHOW_VOI_NAME);
    	        		ViewUserInterface.getReference().setUseVOIName(true);
    	        		
    	        		// display the image and it's VOIs
    	        		ViewJFrameImage frame = new ViewJFrameImage(img);
    	        		frame.setVisible(true);
    		        	
    	        		InteractiveReviewDialog dialog = new InteractiveReviewDialog(frame, img);
    	        		
    	        		// keep waiting until the user closes the dialog
    	        		while (!dialog.isDoneWithReview()) {
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
        if (!dirFile.exists()) {
        	MipavUtil.displayError("The directory selected does not exist.  Please choose another.");
        } else if (!dirFile.isDirectory()) {
        	MipavUtil.displayError("Please select a directory of images to process.");
        } else {
        	File[] dirListing;
        	if (onlyProcessTiff) {
        		dirListing = dirFile.listFiles(tiffFilter);
        	} else {
        		dirListing = dirFile.listFiles();
        	}
        	for (File file : dirListing) {
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
    
    public void setInputFile(String s) {
    	File file = new File(s);
    	if (!file.exists()) {
    		MipavUtil.displayError("The file selected does not exist.  Please choose another.");
    	} else if (!file.isFile()) {
    		MipavUtil.displayError("Please select an image file to process.");
    	} else {
    		inputFiles.add(file);
    	}
    }
    
    public void setDoInteractiveVOIReview(boolean b) {
    	doInteractiveVOIReview = b;
    }
   
    /**
     * Once all the necessary variables are set, call PluginAlgorithmNucleiSegmentation
     */
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
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Nuclei Segmentation: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
    	if (doInteractiveVOIReview) {
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
        setMinSize(scriptParameters.getParams().getInt("min_size"));
        setMaxSize(scriptParameters.getParams().getInt("max_size"));
        // no interactive review while in script
    	setDoInteractiveVOIReview(false);
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
        setTitle("Nuclei Segmentation 05/30/2013");
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
        
        interactiveVOIReviewCheckbox = new JCheckBox("Interactively review/delete nuclei segmentations", doInteractiveVOIReview);
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
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
        int length = Integer.MAX_VALUE;
        
        onlyProcessTiff = onlyProcessTiffCheckbox.isSelected();
        
        doInteractiveVOIReview = interactiveVOIReviewCheckbox.isSelected();
        
        if (dirModeButton.isSelected()) {
	        inputDir = dirChooserText.getText();
	        File dirFile = new File(inputDir);
	        if (!dirFile.exists()) {
	        	MipavUtil.displayError("The directory selected does not exist.  Please choose another.");
	        	return false;
	        } else if (!dirFile.isDirectory()) {
	        	MipavUtil.displayError("Please select a directory of images to process.");
	        	return false;
	        } else {
	        	File[] dirListing;
	        	if (onlyProcessTiff) {
	        		dirListing = dirFile.listFiles(tiffFilter);
	        	} else {
	        		dirListing = dirFile.listFiles();
	        	}
	        	for (File file : dirListing) {
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
        	File file = new File(fileChooserText.getText());
        	if (!file.exists()) {
        		MipavUtil.displayError("The file selected does not exist.  Please choose another.");
        		return false;
        	} else if (!file.isFile()) {
        		MipavUtil.displayError("Please select an image file to process.");
        		return false;
        	} else {
        		inputFiles.add(file);
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
    
    /**
     * This method saves all VOIs for the active image to the default VOI directory for that image.
     * @param img The image for which to save VOIs to disk.
     */
    private static final void saveAllVOIs(ModelImage img) {
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
        	ViewVOIVector VOIs = img.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, img);
                    fileVOI.writeXML(VOIs.VOIAt(i), true, true);
                } else {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, img);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), true);
                }
            }
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }
    }
    
    protected class InteractiveReviewDialog extends JDialog implements ActionListener, ItemListener {
    	private ModelImage img;
    	
    	private boolean doneWithReviewFlag = false;
    	
    	private VOIVector origVOIs;
    	
    	private Vector<JCheckBox> voiCheckboxList = new Vector<JCheckBox>();
    	
    	public InteractiveReviewDialog(Frame parent, ModelImage img) {
    		super(parent, false);
    		
    		this.img = img;
    		
    		origVOIs = img.getVOIsCopy();
    		
    		init();
    	}
    	
    	private void init() {
    		setTitle("Review nuclei sementations for image: " + img.getImageName());
    		setForeground(Color.black);
    		
    		GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.weightx = 1;
            gbc.insets = new Insets(3, 3, 3, 3);
            gbc.fill = GridBagConstraints.HORIZONTAL;

            JPanel mainPanel = new JPanel(new GridBagLayout());
            mainPanel.setForeground(Color.black);
            
            JPanel voiPanel = new JPanel(new GridLayout(0,1));
            voiPanel.setForeground(Color.black);
            voiPanel.setBackground(Color.darkGray);
            JScrollPane scrollPane = new JScrollPane(voiPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            for (VOI voi : img.getVOIs()) {
            	JCheckBox checkbox = new JCheckBox(voi.getName(), true);
            	checkbox.setForeground(voi.getColor());
            	checkbox.setBackground(Color.darkGray);
            	checkbox.addItemListener(this);
            	voiCheckboxList.add(checkbox);
            	voiPanel.add(checkbox);
            }
            gbc.gridx = 0;
            gbc.gridy = 0;
            mainPanel.add(new JLabel("Uncheck the box next to any nucleus segmentations that should be removed."), gbc);
            gbc.gridy++;
            mainPanel.add(scrollPane, gbc);
            
            JPanel buttonPanel = new JPanel(new GridLayout());
            buttonPanel.setForeground(Color.black);
            
            JButton saveButton = new JButton("Save only checked VOIs");
            saveButton.setFont(serif12B);
            saveButton.addActionListener(this);
            buttonPanel.add(saveButton);
            
            JButton discardButton = new JButton("Save all VOIs");
            discardButton.setFont(serif12B);
            discardButton.addActionListener(this);
            buttonPanel.add(discardButton);
            
            getContentPane().add(mainPanel, BorderLayout.CENTER);
            getContentPane().add(buttonPanel, BorderLayout.SOUTH);

            this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
            this.addWindowListener(new WindowAdapter() {
            	public void windowClosing(WindowEvent event) {
            		int confirm = JOptionPane.showOptionDialog(getParent(),
                            "Remove unchecked VOIs before saving to disk?",
                            "Exit Confirmation", JOptionPane.YES_NO_CANCEL_OPTION,
                            JOptionPane.QUESTION_MESSAGE, null, null, null);
                    if (confirm == JOptionPane.YES_OPTION) {
                    	for (int i = 0; i < voiCheckboxList.size(); i++) {
            				if (!voiCheckboxList.elementAt(i).isSelected()) {
            					for (VOI voi : img.getVOIs()) {
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
            //MipavUtil.centerOnScreen(this);
            Point loc = this.getParent().getLocationOnScreen();
            loc.x = loc.x - getSize().width;
            setLocation(loc);
            setVisible(true);
            setResizable(true);
            System.gc();
    	}
    	
    	public void actionPerformed(final ActionEvent event) {
    		final String command = event.getActionCommand();
    		if (command.equals("Save only checked VOIs")) {
    			for (int i = 0; i < voiCheckboxList.size(); i++) {
    				if (!voiCheckboxList.elementAt(i).isSelected()) {
    					for (VOI voi : img.getVOIs()) {
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
    	
    	public void itemStateChanged(final ItemEvent event) {
    		if (event.getItem() instanceof JCheckBox) {
    			JCheckBox source = (JCheckBox)event.getItem();
    			
    		}
    	}
    	
    	public synchronized void setDoneWithReview(boolean b) {
    		doneWithReviewFlag = b;
    	}
    	
    	public synchronized boolean isDoneWithReview() {
    		return doneWithReviewFlag;
    	}
    }
}
