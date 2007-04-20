import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
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
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

/**
 * @author pandyan
 * 
 * This is the main dialog for the DTI Sorting Process Plug-In
 *
 */
public class PlugInDialogDTISortingProcess extends JDialogBase implements
		AlgorithmInterface {
	
	
	/** handle to ViewUserInterface **/
    private ViewUserInterface UI;
    
    /** handle to the algorithm **/
    private PlugInAlgorithmDTISortingProcess alg;
    
    /** path to study dir **/
    private JTextField studyPathTextField;
    
    /** browse button **/
    private JButton studyPathBrowseButton;
    
    /** path to gradient file **/
    private JTextField gradientFilePathTextField;
    
    /** browse button **/
    private JButton gradientFileBrowseButton;
    
    /** path to bmatrix file **/
    private JTextField bmtxtFilePathTextField;
    
    /** browse button **/
    private JButton bmtxtFileBrowseButton;
    
    /** study name string **/
    private String studyName = "";
    
    /** output text area **/
    protected JTextArea outputTextArea;
    
    /** Scroll Pane for the Text Area **/
    private JScrollPane scrollPane;
    
    /** button group for radio buttons **/
    private ButtonGroup optionsGroup;
    
	/** gradient file option **/
    private JRadioButton gradFileRadio;
    
    /** b-matrix file option **/
    private JRadioButton bmtxtFileRadio;
    
    /** label  **/
    private JLabel gradientFileLabel;
    
    /** label **/
    private JLabel bmtxtFileLabel;
    
    /** current directory  **/
    private String currDir = null;
    
    /** comment for interleaved dataset needing to provide b-matrix file**/
    private JLabel interleavedLabel;


	
	/**
	 * Constructor
	 * @param modal
	 */
	public PlugInDialogDTISortingProcess(boolean modal) {
		super(modal);
		UI = ViewUserInterface.getReference();
		init();
	}
	
	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("DTI Sorting Process " + " v1.9");
        
        GridBagLayout mainPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints mainPanelConstraints = new GridBagConstraints();
        
        JPanel mainPanel = new JPanel(mainPanelGridBagLayout);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,30,0);
        JLabel studyPathLabel = new JLabel("study path directory : ");
        mainPanel.add(studyPathLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,30,0);
		studyPathTextField = new JTextField(55);
		mainPanel.add(studyPathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 3;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,30,5);
		studyPathBrowseButton = new JButton("Browse");
		studyPathBrowseButton.addActionListener(this);
		studyPathBrowseButton.setActionCommand("studyPathBrowse");
		mainPanel.add(studyPathBrowseButton, mainPanelConstraints);

		optionsGroup = new ButtonGroup();
		
		gradFileRadio = new JRadioButton();
		gradFileRadio.setSelected(true);
		gradFileRadio.addActionListener(this);
		gradFileRadio.setActionCommand("gradFileRadio");
		optionsGroup.add(gradFileRadio);
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(0,5,15,0);
		mainPanel.add(gradFileRadio, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(0,5,15,0);
        gradientFileLabel = new JLabel("gradient file : ");
        mainPanel.add(gradientFileLabel, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(0,5,15,0);
		gradientFilePathTextField = new JTextField(55);
		mainPanel.add(gradientFilePathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 3;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(0,5,15,5);
		gradientFileBrowseButton = new JButton("Browse");
		gradientFileBrowseButton.addActionListener(this);
		gradientFileBrowseButton.setActionCommand("gradientFileBrowse");
		mainPanel.add(gradientFileBrowseButton, mainPanelConstraints);
		
		
		
		bmtxtFileRadio = new JRadioButton();
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
        bmtxtFileLabel = new JLabel("b-matrix file : ");
        mainPanel.add(bmtxtFileLabel, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.insets = new Insets(0,5,5,0);
		bmtxtFilePathTextField = new JTextField(55);
		bmtxtFilePathTextField.setText("");
		bmtxtFilePathTextField.setEnabled(false);
		bmtxtFilePathTextField.setEditable(false);
		mainPanel.add(bmtxtFilePathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 3;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.insets = new Insets(0,5,5,5);
		bmtxtFileBrowseButton = new JButton("Browse");
		bmtxtFileBrowseButton.addActionListener(this);
		bmtxtFileBrowseButton.setActionCommand("bmtxtFileBrowse");
		bmtxtFileBrowseButton.setEnabled(false);
		mainPanel.add(bmtxtFileBrowseButton, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 4;
		mainPanelConstraints.gridwidth = 2;
		mainPanelConstraints.insets = new Insets(0,5,5,5);
		interleavedLabel = new JLabel("* for interleaved datasets, b-matrix file is required");
		mainPanel.add(interleavedLabel, mainPanelConstraints);
		
		
		
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 5;
		mainPanelConstraints.gridwidth = 4;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		outputTextArea = new JTextArea(15, 70);
		outputTextArea.setEditable(false);
		outputTextArea.setBackground(Color.lightGray);
		outputTextArea.setBorder(new LineBorder(Color.black));
		outputTextArea.setForeground(Color.black);
		
		scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		
		mainPanel.add(scrollPane, mainPanelConstraints);
		
		
		JPanel OKCancelPanel = new JPanel();
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
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		alg = null;
		OKButton.setEnabled(true);
		studyPathBrowseButton.setEnabled(true);
		gradFileRadio.setEnabled(true);
		bmtxtFileRadio.setEnabled(true);
		if(gradFileRadio.isSelected()) {
			gradientFileBrowseButton.setEnabled(true);
		}
		else {
			bmtxtFileBrowseButton.setEnabled(true);
		}
		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		Preferences.debug("*** End DTI Sorting Process *** \n",Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("*** End DTI Sorting Process *** \n");

	}

	
	/**
	 * action performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("studyPathBrowse")) {
			JFileChooser chooser = new JFileChooser();
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }
			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        chooser.setDialogTitle("Choose study path directory");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	studyPathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getAbsolutePath();
	        	studyName = chooser.getSelectedFile().getName();
	        }
		}
		else if(command.equalsIgnoreCase("gradientFileBrowse")) {
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
		}
		else if(command.equalsIgnoreCase("bmtxtFileBrowse")) {
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
		}
		else if(command.equalsIgnoreCase("gradFileRadio")) {
			bmtxtFilePathTextField.setText("");
			bmtxtFilePathTextField.setEnabled(false);
			bmtxtFilePathTextField.setEditable(false);
			bmtxtFileBrowseButton.setEnabled(false);
			gradientFilePathTextField.setEnabled(true);
			gradientFilePathTextField.setEditable(true);
			gradientFileBrowseButton.setEnabled(true);
		
		}
		else if(command.equalsIgnoreCase("bmtxtFileRadio")) {
			gradientFilePathTextField.setText("");
			gradientFilePathTextField.setEditable(false);
			gradientFilePathTextField.setEnabled(false);
			gradientFileBrowseButton.setEnabled(false);
			bmtxtFilePathTextField.setEnabled(true);
			bmtxtFilePathTextField.setEditable(true);
			bmtxtFileBrowseButton.setEnabled(true);
		}
		else if(command.equalsIgnoreCase("cancel")) {
			if(alg != null) {
				alg.setThreadStopped(true);
				Preferences.debug("! Algorithm Cancelled \n",Preferences.DEBUG_ALGORITHM);
			}
			dispose();
		}
		else if(command.equalsIgnoreCase("ok")) {
			outputTextArea.setText("");
			if(studyPathTextField.getText().trim().equals("") || (gradientFilePathTextField.getText().trim().equals("") && bmtxtFilePathTextField.getText().trim().equals(""))) {
				MipavUtil.displayError("Study path and either gradient file or b matrix file are required");
				return;
			}
			boolean success = validateFilePaths();
			if(!success) {
				MipavUtil.displayError("One or both of the paths provided is not accurate");
				studyPathTextField.setText("");
				gradientFilePathTextField.setText("");
				bmtxtFilePathTextField.setText("");
				return;
			}
			
			OKButton.setEnabled(false);
			studyPathBrowseButton.setEnabled(false);
			gradientFileBrowseButton.setEnabled(false);
			bmtxtFileBrowseButton.setEnabled(false);
			gradFileRadio.setEnabled(false);
			bmtxtFileRadio.setEnabled(false);
			callAlgorithm();
		}

	}
	
	
	/**
	 * valdiates the file paths for the study path and gradient file path
	 * @return boolean success
	 */
	public boolean validateFilePaths() {
		File studyPathFile = new File(studyPathTextField.getText().trim());
		if(!studyPathFile.exists()) {
			return false;
		}
		studyName = studyPathFile.getName();
		if(!gradientFilePathTextField.getText().trim().equals("")) {
			File gradientPathFile = new File(gradientFilePathTextField.getText().trim());
			if(!gradientPathFile.exists()) {
				return false;
			}
		}
		else {
			File bmtxtPathFile = new File(bmtxtFilePathTextField.getText().trim());
			if(!bmtxtPathFile.exists()) {
				return false;
			}
		}
		return true;
		
	}
	
	
	
	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		Preferences.debug("*** Beginning DTI Sorting Process *** \n",Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("*** Beginning DTI Sorting Process *** \n");
		
		alg = new PlugInAlgorithmDTISortingProcess(studyPathTextField.getText().trim(), studyName, gradientFilePathTextField.getText().trim(), bmtxtFilePathTextField.getText().trim(), outputTextArea);
		
		alg.addListener(this);
		
		if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }
		
		
	}
	
	/** 
	 * window closing
	 */
	public void windowClosing(WindowEvent event) {
		super.windowClosing(event);
		if(alg != null) {
			alg.setThreadStopped(true);
			Preferences.debug("! Algorithm Cancelled \n",Preferences.DEBUG_ALGORITHM);
		}

		dispose();
	}
	
	
	

}
