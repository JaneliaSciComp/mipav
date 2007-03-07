import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
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
    
    /** study name string **/
    private String studyName = "";
    
    /** output text area **/
    protected JTextArea outputTextArea;
    
    /** Scroll Pane for the Text Area **/
    private JScrollPane scrollPane;
    
	
	
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
        setTitle("DTI Sorting Process");
        
        GridBagLayout mainPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints mainPanelConstraints = new GridBagConstraints();
        
        JPanel mainPanel = new JPanel(mainPanelGridBagLayout);
        
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,0,0);
        JLabel studyPathLabel = new JLabel("study path directory : ");
        mainPanel.add(studyPathLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,0,0);
		studyPathTextField = new JTextField(25);
		mainPanel.add(studyPathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,0,5);
		studyPathBrowseButton = new JButton("Browse");
		studyPathBrowseButton.addActionListener(this);
		studyPathBrowseButton.setActionCommand("studyPathBrowse");
		mainPanel.add(studyPathBrowseButton, mainPanelConstraints);
		
		
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        JLabel gradientFileLabel = new JLabel("gradient file : ");
        mainPanel.add(gradientFileLabel, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		gradientFilePathTextField = new JTextField(25);
		mainPanel.add(gradientFilePathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		gradientFileBrowseButton = new JButton("Browse");
		gradientFileBrowseButton.addActionListener(this);
		gradientFileBrowseButton.setActionCommand("gradientFileBrowse");
		mainPanel.add(gradientFileBrowseButton, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		outputTextArea = new JTextArea(15, 40);
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
		gradientFileBrowseButton.setEnabled(true);
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
			if (UI.getDefaultDirectory() != null) {
				chooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            } else {
            	chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        chooser.setDialogTitle("Choose study path directory");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	studyPathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	studyName = chooser.getSelectedFile().getName();
	        }
		}
		else if(command.equalsIgnoreCase("gradientFileBrowse")) {
			JFileChooser chooser = new JFileChooser();
			if (UI.getDefaultDirectory() != null) {
				chooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            } else {
            	chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
	        chooser.setDialogTitle("Choose gradient file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	gradientFilePathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        }
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
			if(studyPathTextField.getText().trim().equals("") || gradientFilePathTextField.getText().trim().equals("")) {
				MipavUtil.displayError("Both study path and gradient file are required");
				return;
			}
			boolean success = validateFilePaths();
			if(!success) {
				MipavUtil.displayError("One or both of the paths provided is not accurate");
				studyPathTextField.setText("");
				gradientFilePathTextField.setText("");
				return;
			}
			
			OKButton.setEnabled(false);
			studyPathBrowseButton.setEnabled(false);
			gradientFileBrowseButton.setEnabled(false);
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
		File gradientPathFile = new File(gradientFilePathTextField.getText().trim());
		if(!gradientPathFile.exists()) {
			return false;
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
		
		alg = new PlugInAlgorithmDTISortingProcess(studyPathTextField.getText().trim(), studyName, gradientFilePathTextField.getText().trim(), outputTextArea);
		
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
