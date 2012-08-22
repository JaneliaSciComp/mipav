import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.ButtonGroup;
import javax.swing.JButton;

import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.LineBorder;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogDTICreateListFile extends JDialogBase implements AlgorithmInterface {
	
	/** path to dwi file/dir **/
    private JTextField dwiPathTextField;
    
    /** browse button for dwi file/folder **/
    private JButton dwiPathBrowseButton;

    
    /** button group for radio buttons **/
    private ButtonGroup optionsGroup;
    
	/** gradient file option **/
    private JRadioButton gradFileRadio;
    
    /** b-matrix file option **/
    private JRadioButton bmtxtFileRadio;
    
    /** path to gradient file **/
    private JTextField gradientFilePathTextField;
    
    /** browse button **/
    private JButton gradientFileBrowseButton;
    
    /** path to bmatrix file **/
    private JTextField bmtxtFilePathTextField;
    
    /** browse button **/
    private JButton bmtxtFileBrowseButton;
    
    /** output text area **/
    protected JTextArea outputTextArea;
    
    /** Scroll Pane for the Text Area **/
    private JScrollPane scrollPane;
    
    /** current directory  **/
    private String currDir = null;
    
    /** study name string **/
    private String studyName = "";
    
    /** par/rec file directory **/
    private String prFileDir;
    
    /** par/rec file name **/
    private String prFileName;
    
    /** handle to the algorithm **/
    private PlugInAlgorithmDTICreateListFile alg;
    
    /** gradient path **/
    private String gradientPath;
    
    /** bmatrix path**/
    private String bmatrixPath;
    
    /** study path **/
    private String studyPath;
    
    /** boolean if interleaved **/
    private boolean isInterleaved;
	
    /** boolean indicating if we are dealing with dicom or par/rec **/
    private boolean isDICOM;
	
	/**
	 * default constructor
	 *
	 */
	public PlugInDialogDTICreateListFile() {
		
	}
	
	
	
	/**
	 * constructor
	 * @param modal
	 */
	public PlugInDialogDTICreateListFile(boolean modal) {
		init();
	}
	
	
	
	
	/**
	 * init
	 *
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("DTI Create List File " + " v1.0");
        
        GridBagLayout mainPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.WEST;
        
        
        JPanel mainPanel = new JPanel(mainPanelGridBagLayout);
        
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        JLabel parRecLabel = new JLabel(" DWI data : ");
        mainPanel.add(parRecLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		dwiPathTextField = new JTextField(55);
		dwiPathTextField.setEditable(false);
		dwiPathTextField.setBackground(Color.white);
		dwiPathTextField.setBorder(new LineBorder(Color.black));
		mainPanel.add(dwiPathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		dwiPathBrowseButton = new JButton("Browse");
		dwiPathBrowseButton.addActionListener(this);
		dwiPathBrowseButton.setActionCommand("dwiPathBrowse");
		mainPanel.add(dwiPathBrowseButton, mainPanelConstraints);
		
		
		//mainPanelConstraints.anchor = GridBagConstraints.CENTER;
		//interleavedCheckbox = new JCheckBox("  interleaved dataset");
		//mainPanelConstraints.gridx = 1;
		//mainPanelConstraints.gridy = 1;
		//mainPanelConstraints.insets = new Insets(0,5,55,5);
		//mainPanel.add(interleavedCheckbox, mainPanelConstraints);
		
		mainPanelConstraints.anchor = GridBagConstraints.CENTER;
		JLabel dwiDataInfoLabel = new JLabel("For DICOM datasets, select the top level study directory.  For Par/Rec datasets, select the .rec file");
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(0,5,70,5);
		mainPanel.add(dwiDataInfoLabel, mainPanelConstraints);
		
		
		mainPanelConstraints.anchor = GridBagConstraints.WEST;
		optionsGroup = new ButtonGroup();
		
		gradFileRadio = new JRadioButton(" gradient file : ");
		gradFileRadio.setSelected(true);
		gradFileRadio.addActionListener(this);
		gradFileRadio.setActionCommand("gradFileRadio");
		optionsGroup.add(gradFileRadio);
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.gridwidth = 1;
		mainPanelConstraints.insets = new Insets(0,5,15,0);
		mainPanel.add(gradFileRadio, mainPanelConstraints);
		
		
		mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(0,5,15,0);
		gradientFilePathTextField = new JTextField(55);
		gradientFilePathTextField.setEditable(false);
		gradientFilePathTextField.setBorder(new LineBorder(Color.black));
		gradientFilePathTextField.setBackground(Color.white);
		mainPanel.add(gradientFilePathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(0,5,15,5);
		gradientFileBrowseButton = new JButton("Browse");
		gradientFileBrowseButton.addActionListener(this);
		gradientFileBrowseButton.setActionCommand("gradientFileBrowse");
		mainPanel.add(gradientFileBrowseButton, mainPanelConstraints);
  
		
		bmtxtFileRadio = new JRadioButton(" b-matrix file : ");
		bmtxtFileRadio.setSelected(false);
		bmtxtFileRadio.addActionListener(this);
		bmtxtFileRadio.setActionCommand("bmtxtFileRadio");
		optionsGroup.add(bmtxtFileRadio);
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.insets = new Insets(0,5,5,0);
		mainPanel.add(bmtxtFileRadio, mainPanelConstraints);
		
		
		mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.insets = new Insets(0,5,5,0);
		bmtxtFilePathTextField = new JTextField(55);
		bmtxtFilePathTextField.setText("");
		bmtxtFilePathTextField.setEnabled(false);
		bmtxtFilePathTextField.setEditable(false);
		bmtxtFilePathTextField.setBorder(new LineBorder(Color.gray));
		bmtxtFilePathTextField.setBackground(Color.lightGray);
		mainPanel.add(bmtxtFilePathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.insets = new Insets(0,5,5,5);
		bmtxtFileBrowseButton = new JButton("Browse");
		bmtxtFileBrowseButton.addActionListener(this);
		bmtxtFileBrowseButton.setActionCommand("bmtxtFileBrowse");
		bmtxtFileBrowseButton.setEnabled(false);
		mainPanel.add(bmtxtFileBrowseButton, mainPanelConstraints);

		
		mainPanelConstraints.anchor = GridBagConstraints.CENTER;
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 4;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(10,5,5,5);
		JLabel parRecInfoLabel = new JLabel("*Gradient file or b-matrix file must be provided for all DICOM study directories and for versions 4.0 and earlier of Par/Rec");
		mainPanel.add(parRecInfoLabel, mainPanelConstraints);
		
		
		//mainPanelConstraints.anchor = GridBagConstraints.CENTER;
		//mainPanelConstraints.gridx = 1;
		//mainPanelConstraints.gridy = 4;
		//mainPanelConstraints.insets = new Insets(10,5,5,5);
		//JLabel interleavedLabel = new JLabel("* for interleaved datasets, b-matrix file is required");
		//mainPanel.add(interleavedLabel, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 5;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		outputTextArea = new JTextArea(15, 70);
		outputTextArea.setEditable(false);
		outputTextArea.setBackground(Color.lightGray);
		outputTextArea.setBorder(new LineBorder(Color.black));
		outputTextArea.setForeground(Color.black);
		scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		mainPanel.add(scrollPane, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 6;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		JLabel refLabel = new JLabel("Developed in concert with Dr. Lin-Ching Chang D.Sc.,  Dr. Carlo Pierpaoli MD Ph.D.,  and Lindsay Walker MS from the NIH/NICHD/LIMB/STBB group");
		mainPanel.add(refLabel, mainPanelConstraints);
		
		
		
		JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton);
        buildHelpButton();
        helpButton.setActionCommand("help");
        OKCancelPanel.add(helpButton);
		
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        
        
        
        pack();
        setResizable(false);
        setVisible(true);
		
	}
	
	
	
	
	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			alg = null;
			//if OKButton is not null, then the rest are not null
			//we don't want to do these if this algoritm was done via a script
			if(OKButton != null) {
				OKButton.setEnabled(true);
				dwiPathBrowseButton.setEnabled(true);
				gradFileRadio.setEnabled(true);
				bmtxtFileRadio.setEnabled(true);
				//interleavedCheckbox.setEnabled(true);
				if(gradFileRadio.isSelected()) {
					gradientFileBrowseButton.setEnabled(true);
				}
				else {
					bmtxtFileBrowseButton.setEnabled(true);
				}
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				outputTextArea.append("*** End DTI Sorting Process *** \n");
			}
			
			//insertScriptLine();
			Preferences.debug("*** End DTI Sorting Process *** \n",Preferences.DEBUG_ALGORITHM);
			System.out.println("*** End DTI Sorting Process *** \n");
		}
	}

	/**
	 * action preformed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		
		if(command.equalsIgnoreCase("dwiPathBrowse")) {			
			JFileChooser chooser = new JFileChooser();
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }
	        chooser.setDialogTitle("Choose dicom study directory or rec file");
	        chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	if(chooser.getSelectedFile().isDirectory()) {
	        		prFileName = "";
	        		prFileDir = "";
	        		dwiPathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        		currDir = chooser.getSelectedFile().getAbsolutePath();
	        		studyName = chooser.getSelectedFile().getName();
	        		setStudyPath(dwiPathTextField.getText().trim());
	        		isDICOM = true;
	        	}else {
	        		String absolutePath = chooser.getSelectedFile().getAbsolutePath();
		        	String extension = FileUtility.getExtension(absolutePath);
		        	if(extension.equalsIgnoreCase(".rec") || extension.equalsIgnoreCase(".frec")) {
		        		dwiPathTextField.setText(absolutePath);
		        		prFileDir = chooser.getSelectedFile().getParent();
		        		prFileName = chooser.getSelectedFile().getName();
		        		currDir = chooser.getSelectedFile().getParentFile().getAbsolutePath();
		        		isDICOM = false;
		        	}else {
						MipavUtil.displayError("A proper Par/Rec data file was not chosen...file must be either .rec or .frec");
						return;
		        	}
	        	}
	        }		
		}else if(command.equalsIgnoreCase("gradientFileBrowse")) {
			JFileChooser chooser = new JFileChooser();
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }
	        chooser.setDialogTitle("Choose gradient file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	gradientFilePathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getParentFile().getAbsolutePath();
	        }
		}else if(command.equalsIgnoreCase("bmtxtFileBrowse")) {
			JFileChooser chooser = new JFileChooser();
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }
	        chooser.setDialogTitle("Choose b matrix file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	bmtxtFilePathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getParentFile().getAbsolutePath();
	        }
		}else if(command.equalsIgnoreCase("gradFileRadio")) {
			bmtxtFilePathTextField.setText("");
			bmtxtFilePathTextField.setEnabled(false);
			bmtxtFilePathTextField.setBackground(Color.lightGray);
			bmtxtFilePathTextField.setBorder(new LineBorder(Color.gray));
			bmtxtFileBrowseButton.setEnabled(false);
			gradientFilePathTextField.setEnabled(true);
			gradientFileBrowseButton.setEnabled(true);
			gradientFilePathTextField.setBackground(Color.white);
			gradientFilePathTextField.setBorder(new LineBorder(Color.black));
		
		}else if(command.equalsIgnoreCase("bmtxtFileRadio")) {
			gradientFilePathTextField.setText("");
			gradientFilePathTextField.setEnabled(false);
			gradientFilePathTextField.setBackground(Color.lightGray);
			gradientFilePathTextField.setBorder(new LineBorder(Color.gray));
			gradientFileBrowseButton.setEnabled(false);
			bmtxtFilePathTextField.setEnabled(true);
			bmtxtFileBrowseButton.setEnabled(true);
			bmtxtFilePathTextField.setBackground(Color.white);
			bmtxtFilePathTextField.setBorder(new LineBorder(Color.black));
		}else if(command.equalsIgnoreCase("ok")) {
			outputTextArea.setText("");
			if(prFileName.equals("")) {
				if(dwiPathTextField.getText().trim().equals("") || (gradientFilePathTextField.getText().trim().equals("") && bmtxtFilePathTextField.getText().trim().equals(""))) {
					MipavUtil.displayError("DWI data and either gradient file or b matrix file are required");
					return;
				}
			}
			/*if(interleavedCheckbox.isSelected()) {
				if(bmtxtFilePathTextField.getText().trim().equals("")) {
					MipavUtil.displayError("For interleaved datasets, b-matrix file is required");
					gradientFilePathTextField.setText("");
					bmtxtFilePathTextField.setText("");
					return;
				}	
			}*/
			boolean success = validateFilePaths();
			if(!success) {
				MipavUtil.displayError("One or both of the paths provided is not accurate");
				dwiPathTextField.setText("");
				gradientFilePathTextField.setText("");
				bmtxtFilePathTextField.setText("");
				return;
			}
			
			
			OKButton.setEnabled(false);
			dwiPathBrowseButton.setEnabled(false);
			gradientFileBrowseButton.setEnabled(false);
			bmtxtFileBrowseButton.setEnabled(false);
			gradFileRadio.setEnabled(false);
			bmtxtFileRadio.setEnabled(false);
			
			//setStudyPath(studyPathTextField.getText().trim());
			setGradientPath(gradientFilePathTextField.getText().trim());
			setBmtxtPath(bmtxtFilePathTextField.getText().trim());
			setCursor(new Cursor(Cursor.WAIT_CURSOR));
			//setInterleaved(interleavedCheckbox.isSelected());
			//interleavedCheckbox.setEnabled(false);
			callAlgorithm();
		}else if (command.equalsIgnoreCase("help")) {
			//MipavUtil.showHelp("DTI00010");
			MipavUtil.showWebHelp("DTI:_Create_List_File");
		}
	}
	
	
	
	
	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		
		Preferences.debug("*** Beginning DTI Sorting Process *** \n",Preferences.DEBUG_ALGORITHM);
		if(outputTextArea != null) {
			outputTextArea.append("*** Beginning DTI Sorting Process *** \n");
		}
		System.out.println("*** Beginning DTI Sorting Process *** \n");
		if(isDICOM) {
			alg = new PlugInAlgorithmDTICreateListFile(studyPath, studyName, gradientPath, bmatrixPath, outputTextArea, isInterleaved);
		}else {
			alg = new PlugInAlgorithmDTICreateListFile(prFileName, prFileDir, gradientPath, bmatrixPath, outputTextArea);
		}
		alg.addListener(this);
		
	
		if (isRunInSeparateThread()) {

			// Start the thread as a low priority because we wish to still
			// have user interface work fast.
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
		
		
	}
	
	
	/**
	 * valdiates the file paths for the study path and gradient file path
	 * @return boolean success
	 */
	public boolean validateFilePaths() {
		File dwiPathFile = new File(dwiPathTextField.getText().trim());
		if(!dwiPathFile.exists()) {
			return false;
		}
		if(!gradientFilePathTextField.getText().trim().equals("")) {
			File gradientPathFile = new File(gradientFilePathTextField.getText().trim());
			if(!gradientPathFile.exists()) {
				return false;
			}
		}
		if(!bmtxtFilePathTextField.getText().trim().equals("")) {
			File bmtxtPathFile = new File(bmtxtFilePathTextField.getText().trim());
			if(!bmtxtPathFile.exists()) {
				return false;
			}
		}
		
		return true;
		
	}
	
	
	
	
	/**
	 * 
	 * @param bmtxtPath
	 */
	public void setBmtxtPath(String bmatrixPath) {
		this.bmatrixPath = bmatrixPath;
	}

	/**
	 * 
	 * @param gradientPath
	 */
	public void setGradientPath(String gradientPath) {
		this.gradientPath = gradientPath;
	}
	
	/**
	 * 
	 * @param studyPath
	 */
	public void setStudyPath(String studyPath) {
		this.studyPath = studyPath;
	}
	
	
	/**
	 * 
	 * @param isInterleaved
	 */
	public void setInterleaved(boolean isInterleaved) {
		this.isInterleaved = isInterleaved;
	}


}
