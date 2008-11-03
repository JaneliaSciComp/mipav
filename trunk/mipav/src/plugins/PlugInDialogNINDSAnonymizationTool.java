import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.LineBorder;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ScrollCorrector;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

/**
 * @author pandyan
 * This is the main dialog for the NINDS Anonymization Plugin
 * This dialog takes in the input directory (where the images to be anonymized are) 
 * and the output directory (where the anonymized images are saved) as parameters
 *
 */
public class PlugInDialogNINDSAnonymizationTool extends JDialogScriptableBase implements AlgorithmInterface {
	
	/** textfields **/
	private JTextField inputDirectoryTextField, outputDirectoryTextField;
	
	/** buttons **/
	private JButton inputDirectoryBrowseButton, outputDirectoryBrowseButton;
	
	/** text area **/
	private JTextArea outputTextArea;
	
	/** scroll pane **/
	private JScrollPane scrollPane;
	
	/** current directory  **/
    private String currDir = null;
    
    /** handle to algorithm **/
    private PlugInAlgorithmNINDSAnonymizationTool alg;
    
    /** labels **/
    private JLabel inputDirectoryLabel,outputMessageLabel, outputDirectoryLabel, errorMessageLabel, enableTextAreaLabel, renameGrandParentDirLabel;
    
    private JCheckBox enableTextAreaCheckBox, renameGrandParentDirCheckBox;
    
    /** panels **/
    private JPanel mainPanel, OKCancelPanel;
    
    /** GridBagLayout **/
    private GridBagLayout mainPanelGridBagLayout;
    
    /** GridBagConstraints **/
    private GridBagConstraints mainPanelConstraints ;
    
    /** enable text area **/
    private boolean enableTextArea;
    
    /** rename grandparent dir name **/
    private boolean renameGrandParentDir;
  
	
	
	/**
	 *  default constructor
	 */
	public PlugInDialogNINDSAnonymizationTool() {
		
	}
	/**
	 * constructor
	 * @param modal
	 */
	
	public PlugInDialogNINDSAnonymizationTool(boolean modal) {
		super(modal);
		init();
	}
	
	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("NINDS Anonymization Tool " + " v1.3");
        
        mainPanelGridBagLayout = new GridBagLayout();
        mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.WEST;
        
        
        mainPanel = new JPanel(mainPanelGridBagLayout);
        
        //input directory
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        inputDirectoryLabel = new JLabel(" Input Directory : ");
        mainPanel.add(inputDirectoryLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		inputDirectoryTextField = new JTextField(55);
		mainPanel.add(inputDirectoryTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		inputDirectoryBrowseButton = new JButton("Browse");
		inputDirectoryBrowseButton.addActionListener(this);
		inputDirectoryBrowseButton.setActionCommand("inputDirectoryBrowse");
		mainPanel.add(inputDirectoryBrowseButton, mainPanelConstraints);
		
		//output directory
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        outputDirectoryLabel = new JLabel(" Output Directory : ");
        mainPanel.add(outputDirectoryLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		outputDirectoryTextField = new JTextField(55);
		mainPanel.add(outputDirectoryTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		outputDirectoryBrowseButton = new JButton("Browse");
		outputDirectoryBrowseButton.addActionListener(this);
		outputDirectoryBrowseButton.setActionCommand("outputDirectoryBrowse");
		mainPanel.add(outputDirectoryBrowseButton, mainPanelConstraints);
		
		
		//enable textArea Checkbox
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		enableTextAreaLabel = new JLabel(" Enable TextArea ");
        mainPanel.add(enableTextAreaLabel, mainPanelConstraints);
        
		mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		enableTextAreaCheckBox = new JCheckBox();
		enableTextAreaCheckBox.setSelected(true);
        mainPanel.add(enableTextAreaCheckBox, mainPanelConstraints);
		
        
        //renameGrandParent Checkbox
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		renameGrandParentDirLabel = new JLabel(" Rename GrandParent Dir name to new UID ");
        mainPanel.add(renameGrandParentDirLabel, mainPanelConstraints);
        
		mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		renameGrandParentDirCheckBox = new JCheckBox();
		renameGrandParentDirCheckBox.setSelected(true);
        mainPanel.add(renameGrandParentDirCheckBox, mainPanelConstraints);
        
        
		//error message
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 4;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		errorMessageLabel = new JLabel(" ");
		errorMessageLabel.setForeground(Color.RED);
		mainPanelConstraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(errorMessageLabel, mainPanelConstraints);


		//output message
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 5;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		outputMessageLabel = new JLabel(" ");
        mainPanel.add(outputMessageLabel, mainPanelConstraints);
        mainPanelConstraints.anchor = GridBagConstraints.WEST;
		
		//output text area
        mainPanelConstraints.fill = GridBagConstraints.BOTH;
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 6;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		outputTextArea = new JTextArea(15, 70);
		outputTextArea.setEditable(false);
		outputTextArea.setBackground(Color.lightGray);
		outputTextArea.setBorder(new LineBorder(Color.black));
		outputTextArea.setForeground(Color.black);
		scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
		mainPanel.add(scrollPane, mainPanelConstraints);
		
		//ok,cancel 
		OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
		
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        
        pack();
        setResizable(false);
        setVisible(true);
	}
	
	
	/**
	 * action performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("inputDirectoryBrowse")) {
			String defaultInputDirectory = Preferences.getProperty(Preferences.PREF_NINDS_ANON_PLUGIN_INPUTDIR);
			JFileChooser chooser = new JFileChooser();
			if (defaultInputDirectory != null) {
	            File file = new File(defaultInputDirectory);

	            if (file != null) {
	                chooser.setCurrentDirectory(file);
	            } else {
	                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	            }
	        } else {
	            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	        }
			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        chooser.setDialogTitle("Choose input directory");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	inputDirectoryTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getAbsolutePath();
	        	Preferences.setProperty(Preferences.PREF_NINDS_ANON_PLUGIN_INPUTDIR, currDir);
	        }
		}else if(command.equalsIgnoreCase("outputDirectoryBrowse")) {
			String defaultOutputDirectory = Preferences.getProperty(Preferences.PREF_NINDS_ANON_PLUGIN_OUTPUTDIR);
			JFileChooser chooser = new JFileChooser();
			if (defaultOutputDirectory != null) {
	            File file = new File(defaultOutputDirectory);

	            if (file != null) {
	                chooser.setCurrentDirectory(file);
	            } else {
	                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	            }
	        } else {
	            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	        }
			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        chooser.setDialogTitle("Choose output directory");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	outputDirectoryTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getAbsolutePath();
	        	Preferences.setProperty(Preferences.PREF_NINDS_ANON_PLUGIN_OUTPUTDIR, currDir);
	        }
		}else if(command.equalsIgnoreCase("cancel")) {
			if(alg != null) {
				alg.setAlgCanceled(true);
			}else {
				dispose();
			}
		}else if(command.equalsIgnoreCase("ok")) {
			if(enableTextArea) {
				outputTextArea.setText("");
			}
			outputMessageLabel.setText(" ");
			errorMessageLabel.setText(" ");
			if(inputDirectoryTextField.getText().trim().equals("") || outputDirectoryTextField.getText().trim().equals("")) {
				//MipavUtil.displayError("Input Directory and Output Directory are required");
				errorMessageLabel.setText("Input Directory and Output Directory are required");
				return;
			}
			boolean success = validateFilePaths();
			if(!success) {
				return;
			}
			if(enableTextAreaCheckBox.isSelected()) {
				enableTextArea = true;
			}else {
				enableTextArea = false;
			}
			if(renameGrandParentDirCheckBox.isSelected()) {
				renameGrandParentDir = true;
			}else {
				renameGrandParentDir = false;
			}
			OKButton.setEnabled(false);
			inputDirectoryBrowseButton.setEnabled(false);
			outputDirectoryBrowseButton.setEnabled(false);
			setCursor(new Cursor(Cursor.WAIT_CURSOR));
			callAlgorithm();
		}

	}
	
	
	

	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		String inputDirectoryPath = inputDirectoryTextField.getText().trim();
		String outputDirectoryPath = outputDirectoryTextField.getText().trim();
		alg = new PlugInAlgorithmNINDSAnonymizationTool(inputDirectoryPath, outputDirectoryPath, outputTextArea, errorMessageLabel, enableTextArea, renameGrandParentDir);
		
		alg.addListener(this);
		
		
		if (isRunInSeparateThread()) {

			// Start the thread as a low priority because we wish to still
			// have user interface work fast.
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				//MipavUtil.displayError("A thread is already running on this object");
				errorMessageLabel.setText("A thread is already running on this object");
			}
		} else {
			alg.start();
		}

	}
	
	
	
	/**
	 * valdiates the file paths for the study path and gradient file path
	 * @return boolean success
	 */
	public boolean validateFilePaths() {
		File test = new File(inputDirectoryTextField.getText().trim());
		if(!test.exists()) {
			//MipavUtil.displayError("Input Directory is not valid");
			errorMessageLabel.setText("Input Directory is not valid");
			return false;
		}
		File test2 = new File(outputDirectoryTextField.getText().trim());
		if(!test2.exists()) {
			//MipavUtil.displayError("Output Directory is not valid");
			errorMessageLabel.setText("Output Directory is not valid");
			return false;
		}
		if(test.getAbsolutePath().equalsIgnoreCase(test2.getAbsolutePath())) {
			//MipavUtil.displayError("Input and Ouput Directories need to be different directories");
			errorMessageLabel.setText("Input and Ouput Directories need to be different directories");
			return false;
		}
		
		return true;
		
	}

	/**
	 * set GUI
	 */
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub

	}

	/**
	 * store params
	 */
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub

	}

	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			outputMessageLabel.setForeground(Color.BLACK);
			outputMessageLabel.setText(alg.getOutputTextFileName() + " saved to  " + alg.getInputDirectoryPath());
			mainPanel.validate();
			pack();

			alg = null;

			OKButton.setEnabled(true);
			inputDirectoryBrowseButton.setEnabled(true);
			outputDirectoryBrowseButton.setEnabled(true);
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

		}

	}



}
