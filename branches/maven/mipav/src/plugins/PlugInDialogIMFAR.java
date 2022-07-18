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
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

/**
 * Plugin that parses and searches the IMFAR document
 * @author pandyan
 *
 */
public class PlugInDialogIMFAR extends JDialogScriptableBase implements AlgorithmInterface {
	
	/** labels **/
    private JLabel imfarDocLabel, searchDocLabel, outputFileNameLabel, includeConclusionLabel, numSubjectsLabel, messageLabel;
    
    /** textfields **/
	private JTextField imfarDocPathTextField, searchDocPathTextField, outputFileNameTextField, numSubjectsTextField;
	
	/** buttons **/
	private JButton imfarDocBrowseButton, searchDocBrowseButton;
	
	/** checkbox **/
	private JCheckBox includeConclusionCheckbox;
	
	/** panels **/
    private JPanel mainPanel, OKCancelPanel;
    
    /** GridBagLayout **/
    private GridBagLayout mainPanelGridBagLayout;
    
    /** GridBagConstraints **/
    private GridBagConstraints mainPanelConstraints ;
    
    /** doc paths **/
	private String imfarDocPath, searchDocPath;
	
	/** boolean to include conclusion in output **/
	private boolean includeConclusion;
	
	/** output filename **/
	private String outputDocPath;

	/** handle to algorithm **/
	private PlugInAlgorithmIMFAR alg;
	
	/** current directory  **/
    private String currDir = null;
    
    /** number of pre/post words to add on the n= search result **/
    private int numSubjectsPrePost;
	
	
	/**
	 * constructor
	 */
	public PlugInDialogIMFAR() {
		
	}
	
	
	/**
	 * constructor
	 * @param modal
	 */
	public PlugInDialogIMFAR(boolean modal) {
		super(modal);
		init();
	}
	
	
	
	
	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("IMFAR Parser Tool " + " v1.1");
        
        mainPanelGridBagLayout = new GridBagLayout();
        mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.WEST;
        
        mainPanel = new JPanel(mainPanelGridBagLayout);
        
        //imfar document path
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        imfarDocLabel = new JLabel(" IMFAR Document : ");
        mainPanel.add(imfarDocLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		imfarDocPathTextField = new JTextField(55);
		mainPanel.add(imfarDocPathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		imfarDocBrowseButton = new JButton("Browse");
		imfarDocBrowseButton.addActionListener(this);
		imfarDocBrowseButton.setActionCommand("imfarDocBrowse");
		mainPanel.add(imfarDocBrowseButton, mainPanelConstraints);
		
		
		//seach document path
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        searchDocLabel = new JLabel(" Search Document (optional) : ");
        mainPanel.add(searchDocLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		searchDocPathTextField = new JTextField(55);
		mainPanel.add(searchDocPathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		searchDocBrowseButton = new JButton("Browse");
		searchDocBrowseButton.addActionListener(this);
		searchDocBrowseButton.setActionCommand("searchDocBrowse");
		mainPanel.add(searchDocBrowseButton, mainPanelConstraints);
		
		//output filename
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        outputFileNameLabel = new JLabel(" Output Excel Filename : ");
        mainPanel.add(outputFileNameLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		outputFileNameTextField = new JTextField(25);
		outputFileNameTextField.setText("output.xls");
		mainPanel.add(outputFileNameTextField, mainPanelConstraints);
		
		
		//include conclusion checkbox
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        includeConclusionLabel = new JLabel(" Include Conclusion Summary in Output : ");
        mainPanel.add(includeConclusionLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		includeConclusionCheckbox = new JCheckBox();
		includeConclusionCheckbox.setSelected(true);
		mainPanel.add(includeConclusionCheckbox, mainPanelConstraints);
		
		
		//num subjects pre/post words
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 4;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		numSubjectsLabel = new JLabel(" Number of words pre/post n= ");
        mainPanel.add(numSubjectsLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 4;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		numSubjectsTextField = new JTextField(3);
		numSubjectsTextField.setText("3");
		mainPanel.add(numSubjectsTextField, mainPanelConstraints);
		
		
		//message
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 5;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		messageLabel = new JLabel(" ");
		messageLabel.setForeground(Color.RED);
		mainPanelConstraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(messageLabel, mainPanelConstraints);
		
		

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
		if(command.equalsIgnoreCase("imfarDocBrowse")) {
			JFileChooser chooser = new JFileChooser();
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }else {
            	chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            }
		    chooser.addChoosableFileFilter(new MyFilter());
	        chooser.setDialogTitle("Select IMFAR document");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	imfarDocPathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getAbsolutePath();

	        }
		}if(command.equalsIgnoreCase("searchDocBrowse")) {
			JFileChooser chooser = new JFileChooser();
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }else {
            	chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            }
		    chooser.addChoosableFileFilter(new MyFilter());
	        chooser.setDialogTitle("Select Search document");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	searchDocPathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getAbsolutePath();

	        }
		}else if(command.equalsIgnoreCase("ok")) {
			if(imfarDocPathTextField.getText().trim().equals("")) {
				MipavUtil.displayError("IMFAR Document is required");
				return;
			}
			
			if(outputFileNameTextField.getText().trim().equals("")) {
				MipavUtil.displayError("Output Filename is required");
				return;
			}
			
			boolean success = validateInputs();
			if(!success) {
				imfarDocPathTextField.setText("");
				searchDocPathTextField.setText("");
				outputFileNameTextField.setText("output.xls");
				return;
			}
			
			includeConclusion = includeConclusionCheckbox.isSelected();
			messageLabel.setText(" ");
			OKButton.setEnabled(false);
			imfarDocBrowseButton.setEnabled(false);
			searchDocBrowseButton.setEnabled(false);
			setCursor(new Cursor(Cursor.WAIT_CURSOR));
			callAlgorithm();
			
		}else if(command.equalsIgnoreCase("cancel")) {

				dispose();

		} else {
            super.actionPerformed(e);
        }

	}
	
	
	/**
	 * validate inputs
	 * @return
	 */
	public boolean validateInputs() {
		File test = new File(imfarDocPathTextField.getText().trim());
		if(!test.exists()) {
			MipavUtil.displayError("IMFAR doc path provided is not accurate");
			return false;
		}
		imfarDocPath = imfarDocPathTextField.getText().trim();
		
		String testName = outputFileNameTextField.getText().trim();
		if(!testName.matches("[a-zA-Z0-9]+\\.xls")) {
			MipavUtil.displayError("Output filename must be in the form of blah.xls");
			return false;
		}
		outputDocPath = test.getParent() + File.separator + testName;
		
		if(!searchDocPathTextField.getText().trim().equals("")) {
			test = new File(searchDocPathTextField.getText().trim());
			if(!test.exists()) {
				MipavUtil.displayError("Search doc path provided is not accurate");
				return false;
			}
		}
		searchDocPath = searchDocPathTextField.getText().trim();
		try {
			Integer integ = new Integer(numSubjectsTextField.getText().trim());
			numSubjectsPrePost = integ.intValue();
		}catch(NumberFormatException e) {
			MipavUtil.displayError("Number entered is not valid");
			return false;
		}
		if(numSubjectsPrePost < 0) {
			MipavUtil.displayError("Number entered must be >= 0");
			return false;
		}
		
		return true;
	}
	
	
	
	

	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		alg = new PlugInAlgorithmIMFAR(imfarDocPath, searchDocPath, outputDocPath, includeConclusion, numSubjectsPrePost);
		
		alg.addListener(this);
		
		if (isRunInSeparateThread()) {

			// Start the thread as a low priority because we wish to still
			// have user interface work fast.
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.start();
		}

	}
	
	/**
	 * 
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {

			alg = null;
			messageLabel.setText("Algorithm Completed");
			OKButton.setEnabled(true);
			imfarDocBrowseButton.setEnabled(true);
			searchDocBrowseButton.setEnabled(true);
			cancelButton.setText("Close");
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

		}

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

	
	
	
	
	//--------------------------------INNER CLASS--------------------------------------------------------
	class MyFilter extends javax.swing.filechooser.FileFilter {
        public boolean accept(File file) {
        	if(file.isDirectory()) {
        		return true;
        	}
            String filename = file.getName();
            return filename.endsWith(".txt");
        }
        public String getDescription() {
            return "*.txt";
        }
    }


}
