import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.util.HashMap;

import javax.swing.*;
import javax.swing.border.LineBorder;


/**
 * @author pandyan This is the main dialog for the NINDS Anonymization Plugin This dialog takes in the input directory
 *         (where the images to be anonymized are) and the output directory (where the anonymized images are saved) as
 *         parameters
 * 
 */
public class PlugInDialogNINDSInternalAnonymizationTool extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

	/**Action command for ok button**/
	protected static final String OK = "ok";
	
    /** textfields * */
    protected JTextField inputDirectoryTextField, outputDirectoryTextField, inputPatientListFileTextField, csvFilePathTextField, csvDirTextField, csvNameTextField;

    /** buttons * */
    private JButton inputDirectoryBrowseButton, outputDirectoryBrowseButton, inputPatientListFileBrowseButton, selectCSVFileBrowseButton, selectCSVDirBrowseButton;
    
    /** radio buttons **/
    protected JRadioButton selectCSVFileRadioButton;

	private JRadioButton createNewCSVFileRadioButton;
    
    /** button group for radio buttons **/
    private ButtonGroup optionsGroup = new ButtonGroup();;

    /** text area * */
    protected JTextArea outputTextArea;

    /** scroll pane * */
    private JScrollPane scrollPane;
       
    /** current directory * */
    private String currDir = null;

    /** handle to algorithm * */
    protected PlugInAlgorithmNINDSInternalAnonymizationTool alg;

    /** labels * */
    private JLabel inputDirectoryLabel, inputPatientListFileLabel;

	protected JLabel outputMessageLabel, outputDirectoryLabel, errorMessageLabel;

	private JLabel csvDirLabel, csvNameLabel;

    /** panels * */
    private JPanel mainPanel, OKCancelPanel;

    /** GridBagLayout * */
    private GridBagLayout mainPanelGridBagLayout;

    /** GridBagConstraints * */
    private GridBagConstraints mainPanelConstraints;

    /** path to csv/txt file **/
    protected String csvFilePath, patientListFilePath, parentPatientListFilePath;
    
    /** boolean indicating if csv file is new **/
    protected boolean newCSVFile;
    
    /** Map of patient id and blinded patient id */
    private HashMap<String, String> patientListMap = new HashMap<String, String>();
    
    /* List of patient ids' (interim ids') */
    //private String[] patientIdList;
    
    /**
     * default constructor
     */
    public PlugInDialogNINDSInternalAnonymizationTool() {

    }

    /**
     * constructor
     * 
     * @param modal
     */

    public PlugInDialogNINDSInternalAnonymizationTool(boolean modal) {
        super(modal);
        init();
    }

    /**
     * init
     */
    public void init() {
        setForeground(Color.black);
        setTitle("NINDS Anonymization Tool (Internal)" + " v2.3");

        mainPanelGridBagLayout = new GridBagLayout();
        mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.WEST;

        mainPanel = new JPanel(mainPanelGridBagLayout);

        // input directory
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 0;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        inputDirectoryLabel = new JLabel(" Input Directory : ");
        mainPanel.add(inputDirectoryLabel, mainPanelConstraints);

        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 0;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        inputDirectoryTextField = new JTextField(25);
        mainPanel.add(inputDirectoryTextField, mainPanelConstraints);

        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 0;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        inputDirectoryBrowseButton = new JButton("Browse");
        inputDirectoryBrowseButton.addActionListener(this);
        inputDirectoryBrowseButton.setActionCommand("inputDirectoryBrowse");
        mainPanel.add(inputDirectoryBrowseButton, mainPanelConstraints);

        // output directory
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 1;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        outputDirectoryLabel = new JLabel(" Output Directory : ");
        mainPanel.add(outputDirectoryLabel, mainPanelConstraints);

        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 1;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        outputDirectoryTextField = new JTextField(25);
        mainPanel.add(outputDirectoryTextField, mainPanelConstraints);

        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 1;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        outputDirectoryBrowseButton = new JButton("Browse");
        outputDirectoryBrowseButton.addActionListener(this);
        outputDirectoryBrowseButton.setActionCommand("outputDirectoryBrowse");
        mainPanel.add(outputDirectoryBrowseButton, mainPanelConstraints);
        
        // Input blinding file
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 2;
        //mainPanelConstraints.insets = new Insets(40, 5, 15, 0);
        inputPatientListFileLabel = new JLabel("Input Patient List File  (txt)");
        mainPanel.add(inputPatientListFileLabel, mainPanelConstraints);
        		
		mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 2;
        //mainPanelConstraints.insets = new Insets(40, 5, 15, 0);
        inputPatientListFileTextField = new JTextField(25);
        mainPanel.add(inputPatientListFileTextField, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 2;
        //mainPanelConstraints.insets = new Insets(40, 5, 15, 5);
        inputPatientListFileBrowseButton = new JButton("Browse");
        inputPatientListFileBrowseButton.addActionListener(this);
        inputPatientListFileBrowseButton.setActionCommand("inputPatientListFileBrowse");
        mainPanel.add(inputPatientListFileBrowseButton, mainPanelConstraints);
        
        //csv file
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 3;
        //mainPanelConstraints.insets = new Insets(40, 5, 15, 0);
        selectCSVFileRadioButton = new JRadioButton("Select CSV file: ");
        selectCSVFileRadioButton.setSelected(true);
        selectCSVFileRadioButton.addActionListener(this);
        selectCSVFileRadioButton.setActionCommand("selectCSV");
		optionsGroup.add(selectCSVFileRadioButton);
		mainPanel.add(selectCSVFileRadioButton, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 3;
        //mainPanelConstraints.insets = new Insets(40, 5, 15, 0);
        csvFilePathTextField = new JTextField(25);
        mainPanel.add(csvFilePathTextField, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 3;
        //mainPanelConstraints.insets = new Insets(40, 5, 15, 5);
        selectCSVFileBrowseButton = new JButton("Browse");
        selectCSVFileBrowseButton.addActionListener(this);
        selectCSVFileBrowseButton.setActionCommand("selectCSVFileBrowse");
        mainPanel.add(selectCSVFileBrowseButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 4;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        createNewCSVFileRadioButton = new JRadioButton("Create new CSV file: ");
        createNewCSVFileRadioButton.addActionListener(this);
        createNewCSVFileRadioButton.setActionCommand("createCSV");
		optionsGroup.add(createNewCSVFileRadioButton);
		mainPanel.add(createNewCSVFileRadioButton, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 0;
	    mainPanelConstraints.gridy = 5;
	    //mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
	    csvDirLabel = new JLabel("CSV file directory: ");
	    csvDirLabel.setEnabled(false);
	    mainPanel.add(csvDirLabel, mainPanelConstraints);
	    
	    mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 5;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        csvDirTextField = new JTextField(25);
        csvDirTextField.setEnabled(false);
        mainPanel.add(csvDirTextField, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 5;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        selectCSVDirBrowseButton = new JButton("Browse");
        selectCSVDirBrowseButton.addActionListener(this);
        selectCSVDirBrowseButton.setActionCommand("selectCSVDirBrowse");
        selectCSVDirBrowseButton.setEnabled(false);
        mainPanel.add(selectCSVDirBrowseButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 0;
	    mainPanelConstraints.gridy = 6;
	    //mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
	    csvNameLabel = new JLabel("             CSV file name: ");
	    csvNameLabel.setEnabled(false);
	    mainPanel.add(csvNameLabel, mainPanelConstraints);
        
	    mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 6;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        csvNameTextField = new JTextField(25);
        csvNameTextField.setEnabled(false);
        mainPanel.add(csvNameTextField, mainPanelConstraints);
        
        // error message
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 8;
        mainPanelConstraints.gridwidth = 3;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        errorMessageLabel = new JLabel(" ");
        errorMessageLabel.setForeground(Color.RED);
        mainPanelConstraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(errorMessageLabel, mainPanelConstraints);

        // output message
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 9;
        mainPanelConstraints.gridwidth = 3;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        outputMessageLabel = new JLabel(" ");
        mainPanel.add(outputMessageLabel, mainPanelConstraints);
        mainPanelConstraints.anchor = GridBagConstraints.WEST;
                
        // output text area
        mainPanelConstraints.fill = GridBagConstraints.BOTH;
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 10;
        mainPanelConstraints.gridwidth = 3;
        //mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        outputTextArea = new JTextArea(10, 30);
        outputTextArea.setEditable(false);
        outputTextArea.setBackground(Color.lightGray);
        outputTextArea.setBorder(new LineBorder(Color.black));
        outputTextArea.setForeground(Color.black);
        scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
        mainPanel.add(scrollPane, mainPanelConstraints);
        
               
        // ok,cancel
        OKCancelPanel = new JPanel(new FlowLayout());
        buildOKButton();
        OKButton.setActionCommand(OK);
        OKCancelPanel.add(OKButton);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton);

        setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        getContentPane().add(mainPanel, gbc);
        gbc.gridy++;
        getContentPane().add(OKCancelPanel, gbc);
        
        //getContentPane().add(mainPanel, BorderLayout.CENTER);
        //getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        
        validate();
        setMinimumSize(new Dimension(getPreferredSize().width + 40, getPreferredSize().height + 40));

        pack();
        setResizable(true);
        setVisible(true);
    }

    /**
     * action performed
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();
        if (command.equalsIgnoreCase("inputDirectoryBrowse")) {
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
        } else if (command.equalsIgnoreCase("outputDirectoryBrowse")) {
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
        } else if (command.equalsIgnoreCase("inputPatientListFileBrowse")) {
        	String defaultBlindingCSVDirectory = Preferences.getProperty(Preferences.PREF_NINDS_ANON_PLUGIN_OUTPUTDIR);
        	JFileChooser chooser = new JFileChooser();
            if (defaultBlindingCSVDirectory != null) {
                File file = new File(defaultBlindingCSVDirectory);

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        	
            chooser.setDialogTitle("Choose input patient list text file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	inputPatientListFileTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getAbsolutePath();
                Preferences.setProperty(Preferences.PREF_NINDS_ANON_PLUGIN_OUTPUTDIR, currDir);
	        }
        	
        } else if(command.equalsIgnoreCase("selectCSVFileBrowse")) {
        	String defaultCSVDirectory = Preferences.getProperty(Preferences.PREF_NINDS_ANON_PLUGIN_CSVDIR);
            JFileChooser chooser = new JFileChooser();
            if (defaultCSVDirectory != null) {
                File file = new File(defaultCSVDirectory);

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
	        chooser.setDialogTitle("Choose CSV file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	csvFilePathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getAbsolutePath();
                Preferences.setProperty(Preferences.PREF_NINDS_ANON_PLUGIN_CSVDIR, currDir);
	        }
		}else if(command.equalsIgnoreCase("selectCSVDirBrowse")) {
        	String defaultCSVDirectory = Preferences.getProperty(Preferences.PREF_NINDS_ANON_PLUGIN_CSVDIR);
            JFileChooser chooser = new JFileChooser();
            if (defaultCSVDirectory != null) {
                File file = new File(defaultCSVDirectory);

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        chooser.setDialogTitle("Choose CSV directory");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	csvDirTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getAbsolutePath();
                Preferences.setProperty(Preferences.PREF_NINDS_ANON_PLUGIN_CSVDIR, currDir);
	        }
		}else if(command.equalsIgnoreCase("selectCSV")) {
        	csvDirLabel.setEnabled(false);
        	csvDirTextField.setText("");
        	csvDirTextField.setEnabled(false);
        	selectCSVDirBrowseButton.setEnabled(false);
        	csvNameLabel.setEnabled(false);
        	csvNameTextField.setText("");
        	csvNameTextField.setEnabled(false);
			selectCSVFileBrowseButton.setEnabled(true);
			csvFilePathTextField.setEnabled(true);

		
		}else if(command.equalsIgnoreCase("createCSV")) {
        	csvDirLabel.setEnabled(true);
        	csvDirTextField.setEnabled(true);
        	selectCSVDirBrowseButton.setEnabled(true);
        	csvNameLabel.setEnabled(true);
        	csvNameTextField.setEnabled(true);
			selectCSVFileBrowseButton.setEnabled(false);
			csvFilePathTextField.setEnabled(false);
			csvFilePathTextField.setText("");
		
		}else if (command.equalsIgnoreCase("cancel")) {
            if (alg != null) {
                alg.setAlgCanceled(true);
            }
            windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else if (command.equals(OK)) {
            outputTextArea.setText("");
            outputMessageLabel.setText(" ");
            errorMessageLabel.setText(" ");
            if (inputDirectoryTextField.getText().trim().equals("")
                    || outputDirectoryTextField.getText().trim().equals("")) {
                errorMessageLabel.setText("Input Directory and Output Directory are required");
                return;
            }
            if(selectCSVFileRadioButton.isSelected()) {
            	if(csvFilePathTextField.getText().trim().equals("")) {
            		errorMessageLabel.setText("CSV File Path is required");
                    return;
            	}
            	newCSVFile = false;
            }else {
            	if(csvDirTextField.getText().trim().equals("") || csvNameTextField.getText().trim().equals("")){
            		errorMessageLabel.setText("CSV Dir and Name are required");
                    return;
            	}
            	newCSVFile = true;
            }
            boolean successValidateFp = validateFilePaths();
            boolean successParseTxt = parsePatientListTxtFile();
            
            if ( !successValidateFp || !successParseTxt) {
                return;
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
                      
        alg = new PlugInAlgorithmNINDSInternalAnonymizationTool(inputDirectoryPath, outputDirectoryPath, parentPatientListFilePath, patientListMap, outputTextArea,
                errorMessageLabel, csvFilePath,newCSVFile);

        alg.addListener(this);
        
        createProgressBar("Image De-identifcation Progress", "Analyzing...", alg);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still
            // have user interface work fast.
            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                // MipavUtil.displayError("A thread is already running on this object");
                errorMessageLabel.setText("A thread is already running on this object");
            }
        } else {
            alg.start();
        }

    }

    /**
     * valdiates the file paths for the study path and gradient file path
     * 
     * @return boolean success
     */
    public boolean validateFilePaths() {
        File test = new File(inputDirectoryTextField.getText().trim());
        if ( !test.exists()) {
            errorMessageLabel.setText("ERROR: Input Directory " + test.getAbsolutePath() + " does not exist!");
            return false;
        }
        
        File test2 = new File(outputDirectoryTextField.getText().trim());
        if ( !test2.exists()) {
            errorMessageLabel.setText("ERROR: Output Directory " + test2.getAbsolutePath() + " does not exist!");
            return false;
        }
        if (test.getAbsolutePath().equalsIgnoreCase(test2.getAbsolutePath())) {
            errorMessageLabel.setText("ERROR: Input and Ouput Directories cannot be same!");
            return false;
        }
        
        if (test2.getAbsolutePath().contains(test.getAbsolutePath())) {
        	errorMessageLabel.setText("ERROR: Output directory cannot be a sub directory of input directory!");
        	return false;
        }
        
        File test3 = new File(inputPatientListFileTextField.getText().trim());
        if (!test3.exists()) {
        	errorMessageLabel.setText("ERROR: Patient list text file " + test3.getAbsolutePath() + " does not exist!");
        	return false;
        } else {
        	patientListFilePath = inputPatientListFileTextField.getText().trim();
        	if (test3.getParent().equalsIgnoreCase(test.getAbsolutePath())) {
        		errorMessageLabel.setText("ERROR: The patient list file cannot be inside the input folder!");
        		return false;
        	}
        }
        
        if (!(patientListFilePath.endsWith(".txt") || patientListFilePath.endsWith(".TXT"))) {
        	errorMessageLabel.setText("ERROR: The patient list file should have an extension of .txt!");
            return false;
        }
        
        
        if(selectCSVFileRadioButton.isSelected()) {
	        File test4 = new File(csvFilePathTextField.getText().trim());
	        if ( !test4.exists()) {
	            errorMessageLabel.setText("ERROR: CSV file " + test4.getAbsolutePath() + " does not exist!");
	            return false;
	        }else {
	        	csvFilePath = csvFilePathTextField.getText().trim();
	        }
        } else {
	        String dir = csvDirTextField.getText().trim();
	        if(dir.endsWith(File.separator)) {
				dir = dir.substring(0,dir.length()-1);
			}
	        File test5 = new File(dir);
	        if ( !test5.exists()) {
	            errorMessageLabel.setText("ERROR: CSV directory " + test5.getAbsolutePath() + " does not exist!");
	            return false;
	        }
	        String fileName = csvNameTextField.getText().trim();
	        if(!(fileName.endsWith(".csv") || fileName.endsWith(".CSV"))) {
	        	errorMessageLabel.setText("ERROR: The csv file should have an extension of .csv!");
	            return false;
	        }
	        csvFilePath = dir + File.separator + fileName;
        }
		

        return true;

    }
    
    /**
     * Reads blinding csv file, sets map of original and blinding patient ids.  
     */
    
    public boolean parsePatientListTxtFile() {
    	
    	File patientListFile = new File(patientListFilePath);
    	
    	// Parent of blinding csv file to create a output log file. 
    	parentPatientListFilePath = patientListFile.getParent();
    	try {
    		BufferedReader reader = new BufferedReader(new FileReader(patientListFile));
    		String line = "";
        	try {
        		while((line = reader.readLine()) != null) {
        			try {
        				@SuppressWarnings("unused")
						int id = Integer.valueOf(line.trim());
        			} catch (NumberFormatException nfe) {
        				errorMessageLabel.setText("Patient ID " + line.trim() + " is not numeric.");
        				return false;
        			}
        			if (patientListMap.containsKey(line.trim())) {
        				errorMessageLabel.setText("ERROR: Duplicate patient ids not allowed in the patient list file!!");
        				return false;
        			}
            		patientListMap.put(line.trim(), "");    			
            	}
        	} catch (IOException ioe) {
        		errorMessageLabel.setText("ERROR: Unexpected I/O error occured!!");
        		return false;
        	}
        	
    	} catch (FileNotFoundException fnf) {
    		errorMessageLabel.setText(patientListFilePath + " not found on the requested path");
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
        if (alg.isCompleted()) {
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
