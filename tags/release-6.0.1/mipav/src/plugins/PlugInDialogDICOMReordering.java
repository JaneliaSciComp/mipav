import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.JButton;
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
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

/**
 * @author pandyan
 * 
 * The plugin is used for sorting DICOM image slices based on the
 * patient location <0020,0037>
 * 
 * New dirs are created using patientID info as well as studyID and series...as well as whether the initial images were
 * in a "pre" or "post" folder
 *
 */
public class PlugInDialogDICOMReordering extends JDialogScriptableBase implements AlgorithmInterface {
	
	/** text field **/
	private JTextField studyPathTextField;
	
	/** button **/
	private JButton studyPathBrowseButton;
	
	/** current directory  **/
    private String currDir;
    
    /** study path **/
    private String studyPath;
    
    /** algorithm **/
    private PlugInAlgorithmDICOMReordering alg;
    
    /** output text area **/
    protected JTextArea outputTextArea;
    
    /** Scroll Pane for the Text Area **/
    private JScrollPane scrollPane;
	
	
	/**
	 * constructor
	 *
	 */
	public PlugInDialogDICOMReordering(){
		
	}
	
	/**
	 * constructor
	 * @param modal
	 */
	public PlugInDialogDICOMReordering(boolean modal){
		super(modal);
		init();
	}
	
	/**
	 * init
	 *
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("DICOM Reordering");
        
        GridBagLayout mainPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.WEST;
        
        
        JPanel mainPanel = new JPanel(mainPanelGridBagLayout);
        
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        JLabel studyPathLabel = new JLabel(" study path directory : ");
        mainPanel.add(studyPathLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		studyPathTextField = new JTextField(55);
		mainPanel.add(studyPathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		studyPathBrowseButton = new JButton("Browse");
		studyPathBrowseButton.addActionListener(this);
		studyPathBrowseButton.setActionCommand("studyPathBrowse");
		mainPanel.add(studyPathBrowseButton, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.gridwidth = 3;
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
	        }
		}else if(command.equalsIgnoreCase("cancel")) {
			if(alg != null) {
				alg.setThreadStopped(true);
				Preferences.debug("! Algorithm Cancelled \n",Preferences.DEBUG_ALGORITHM);
			}
			dispose();
		}else if(command.equalsIgnoreCase("ok")) {
			outputTextArea.setText("");
			boolean success = validateStudyPath();
			if(!success) {
				MipavUtil.displayError("One or both of the paths provided is not accurate");
				studyPathTextField.setText("");
				return;
			}
			
			
			OKButton.setEnabled(false);
			cancelButton.setText("Cancel");
			studyPathBrowseButton.setEnabled(false);
			setStudyPath(studyPathTextField.getText().trim());
			setCursor(new Cursor(Cursor.WAIT_CURSOR));
			callAlgorithm();
		}
	}


	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		
		if(outputTextArea != null) {
			outputTextArea.append("*** Beginning DICOM Reordering *** \n");
		}
		
		alg = new PlugInAlgorithmDICOMReordering(studyPath,outputTextArea);
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
	 * setGUIFromPArams
	 */
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub

	}

	/**
	 * storeParamsFromGUI
	 */
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub

	}

	
	/**
	 * algorithmPerformed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			alg = null;
			//if OKButton is not null, then the rest are not null
			//we don't want to do these if this algoritm was done via a script
			if(OKButton != null) {
				cancelButton.setText("Close");
				OKButton.setEnabled(true);
				studyPathBrowseButton.setEnabled(true);
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				outputTextArea.append("*** End DICOM Reordering *** \n");
			}
		}

	}
	
	
	/**
	 * valdiates the file paths for the study path and gradient file path
	 * @return boolean success
	 */
	public boolean validateStudyPath() {
		File studyPathFile = new File(studyPathTextField.getText().trim());
		if(!studyPathFile.exists()) {
			return false;
		}
		return true;
	}
	
	/**
	 * 
	 * @param studyPath
	 */
	public void setStudyPath(String studyPath) {
		this.studyPath = studyPath;
	}

	

}
