import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import gov.nih.mipav.view.ScrollCorrector;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogXMLtoCSVFile extends JDialogBase implements AlgorithmInterface, ActionListener {

	private JPanel mainPanel;
	
	private GridBagConstraints gbc;
	
	private JLabel XMLFileLabel;
	
	private JTextField XMLFilePathTextField;
	
	private JButton XMLFileBrowseButton;
	
	private JTextArea outputTextArea;
	
	private JScrollPane scrollPane;
	
	private File XMLDirectoryFile;
	
	private String currDir;
	
	private String CSVFileName;
	
	private JLabel CSVFileLabel;
	
	private JTextField CSVFileTextField;
	
	
	private PlugInAlgorithmXMLtoCSVFile alg;
	
	
	
	
	public PlugInDialogXMLtoCSVFile() {
		
	}
	
	
	public PlugInDialogXMLtoCSVFile(boolean modal) {
		super(modal);
		init();
	}
	
	
	
	
	private void init() {
		setForeground(Color.black);
        setTitle("XML Files to CSV File");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        
        XMLFileLabel = new JLabel("XML File Directory");
        XMLFilePathTextField = new JTextField(35);
        XMLFilePathTextField.setEditable(false);
        XMLFilePathTextField.setBackground(Color.white);
        XMLFileBrowseButton = new JButton("Browse");
        XMLFileBrowseButton.addActionListener(this);
        XMLFileBrowseButton.setActionCommand("XMLFileBrowse");   
        
        CSVFileLabel = new JLabel("CSV Output File");
        CSVFileTextField = new JTextField(20);
        CSVFileTextField.setEditable(true);
        CSVFileTextField.setText("result.csv");
        
        outputTextArea = new JTextArea();
        outputTextArea.setRows(15);
		outputTextArea.setEditable(false);
		outputTextArea.setBackground(Color.lightGray);
		outputTextArea.setBorder(new LineBorder(Color.black));
		outputTextArea.setForeground(Color.black);
		scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
		scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
		
		
		
		gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15,5,5,15);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(XMLFileLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(XMLFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(XMLFileBrowseButton,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(CSVFileLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(CSVFileTextField,gbc);
		
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        mainPanel.add(scrollPane,gbc);
        
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
        setMinimumSize(getSize());
        
        setVisible(true);
        setResizable(false);
	        
	        
		
		
	}
	
	private void callAlgorithm() {
		alg = new PlugInAlgorithmXMLtoCSVFile(XMLDirectoryFile, CSVFileName, outputTextArea);
	
		alg.addListener(this);
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		
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
	
	
	
	
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			 setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			 OKButton.setEnabled(false);
			 cancelButton.setText("Close");
			 
			 outputTextArea.append("Finished" + "\n");

		}

	}

	
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("XMLFileBrowse")) {
			JFileChooser chooser = new JFileChooser(Preferences.getImageDirectory());
	        /*if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
	        }*/
	        chooser.setDialogTitle("Choose XML File Directory");
	        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	XMLDirectoryFile = chooser.getSelectedFile();
	        	currDir = XMLDirectoryFile.getAbsolutePath();
	        	Preferences.setImageDirectory(new File(currDir));
	        	XMLFilePathTextField.setText(currDir);
	        }
		}else if(command.equalsIgnoreCase("ok")) {
			 if(setVariables()) {
				 callAlgorithm();
			 }
		 }else if(command.equalsIgnoreCase("cancel")){
			 
			 dispose();
		 }else {
		     super.actionPerformed(e);
		 }

	}
	
	
	private boolean setVariables() {
	
		if(XMLFilePathTextField.getText().trim().equals("")) {
			MipavUtil.displayError("XML directory file is required");
			return false;
			
		}
		
		CSVFileName = CSVFileTextField.getText();
		
		if ((CSVFileName.trim() == null) || (CSVFileName.trim().length() == 0)) {
			MipavUtil.displayError("CSV output file name is required");
			return false;
		}
		
		return true;
	}

}
