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
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogFlattenCSVFile extends JDialogBase implements AlgorithmInterface, ActionListener {

	private JPanel mainPanel;
	
	private GridBagConstraints gbc;
	
	private JLabel CSVFileLabel;
	
	private JTextField CSVFilePathTextField;
	
	private JButton CSVFileBrowseButton;
	
	private JTextArea outputTextArea;
	
	private JScrollPane scrollPane;
	
	private File inputFile;
	
	private File outputFile;
	
	private String currDir;
	
	
	private PlugInAlgorithmFlattenCSVFile alg;
	
	
	
	
	public PlugInDialogFlattenCSVFile() {
		
	}
	
	
	public PlugInDialogFlattenCSVFile(boolean modal) {
		super(modal);
		init();
	}
	
	
	
	
	private void init() {
		setForeground(Color.black);
        setTitle("Flatten CSV File v1.0");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        
        CSVFileLabel = new JLabel("CSV File ");
        CSVFilePathTextField = new JTextField(35);
        CSVFilePathTextField.setEditable(false);
        CSVFilePathTextField.setBackground(Color.white);
        CSVFileBrowseButton = new JButton("Browse");
        CSVFileBrowseButton.addActionListener(this);
        CSVFileBrowseButton.setActionCommand("CSVFileBrowse");        
        
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
        mainPanel.add(CSVFileLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(CSVFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(CSVFileBrowseButton,gbc);
		
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;
        
        gbc.gridx = 0;
        gbc.gridy = 1;
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
		alg = new PlugInAlgorithmFlattenCSVFile(inputFile, outputFile, outputTextArea);
	
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
		if(command.equalsIgnoreCase("CSVFileBrowse")) {
			JFileChooser chooser = new JFileChooser(Preferences.getImageDirectory());
	        /*if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
	        }*/
	        chooser.setDialogTitle("Choose CSV File");
	        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
	        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".csv",".CSV",".txt",".TXT"}));
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	currDir = chooser.getSelectedFile().getAbsolutePath();
	        	Preferences.setImageDirectory(new File(currDir));
	        	String fileName = chooser.getSelectedFile().getName();
	        	if((!fileName.toUpperCase().endsWith(".CSV"))&& (!fileName.toUpperCase().endsWith(".TXT"))) {
					MipavUtil.displayError("CSV files must end in .csv or .CSV or .txt or .TXT");
					return;
				}
	        	int index = fileName.lastIndexOf(".");
	        	String outputFileName = fileName.substring(0, index) + "_flat.csv";
	        	inputFile = new File(chooser.getCurrentDirectory() + File.separator + fileName);
	        	outputFile = new File(chooser.getCurrentDirectory() + File.separator + outputFileName);
	        	CSVFilePathTextField.setText(currDir);
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
	
		if(CSVFilePathTextField.getText().trim().equals("")) {
			MipavUtil.displayError("CSV file is required");
			return false;
			
		}
		
		
		return true;
	}

}
