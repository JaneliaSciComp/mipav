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
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

/**
 * 
 * @author pandyan
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
	
	
	/**
	 * 
	 *
	 */
	public PlugInDialogDICOMReordering(){
		
	}
	
	/**
	 * 
	 * @param modal
	 */
	public PlugInDialogDICOMReordering(boolean modal){
		super(modal);
		init();
	}
	
	/**
	 * 
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
	 * 
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
			
			boolean success = validateStudyPath();
			if(!success) {
				MipavUtil.displayError("One or both of the paths provided is not accurate");
				studyPathTextField.setText("");
				return;
			}
			
			OKButton.setEnabled(false);
			studyPathBrowseButton.setEnabled(false);
			setStudyPath(studyPathTextField.getText().trim());
			setCursor(new Cursor(Cursor.WAIT_CURSOR));
			callAlgorithm();
		}
	}


	/**
	 * 
	 */
	protected void callAlgorithm() {
		alg = new PlugInAlgorithmDICOMReordering(studyPath);
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
	 * 
	 */
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub

	}

	/**
	 * 
	 */
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub

	}

	
	/**
	 * 
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			alg = null;
			//if OKButton is not null, then the rest are not null
			//we don't want to do these if this algoritm was done via a script
			if(OKButton != null) {
				OKButton.setEnabled(true);
				studyPathBrowseButton.setEnabled(true);
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
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
