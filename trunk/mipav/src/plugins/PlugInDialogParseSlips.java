import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileFilter;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.filechooser.FileNameExtensionFilter;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;


public class PlugInDialogParseSlips extends JDialogStandalonePlugin implements AlgorithmInterface{

	private PlugInAlgorithmParseSlips parseAlg;
	
	private File csvFile;
	
	private FileWriter csv;
	
	private JTextField reportField;
	
	private JTextField slipField;
	
	private JFileChooser fileChooser;
	
	private String whichFile;
	
	private String baseDir;
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -4866718288193356534L;

	public PlugInDialogParseSlips(){
		super();
		init();
	}
	
	public void actionPerformed(ActionEvent event){
		String command = event.getActionCommand();
		//System.out.println(command);
		if(command.equals("Report"))
			openDir(reportField, command);
		else if(command.equals("Slip"))
			openDir(slipField, command);
		else if(command.equals("ApproveSelection") && whichFile.equals("Report")){
			if(fileChooser.getSelectedFile().exists()){
				reportField.setText(fileChooser.getSelectedFile().toString());
				baseDir = reportField.getText();
			}
			else
				MipavUtil.displayError("This file does not exists");
		}
		else if(command.equals("ApproveSelection") && whichFile.equals("Slip")){
			if(fileChooser.getSelectedFile().exists()){
				slipField.setText(fileChooser.getSelectedFile().toString());
				baseDir = slipField.getText();
			}
			else
				MipavUtil.displayError("This file does not exists");
		}
		else if(command.equals("OK")){
			if(new File(reportField.getText()).exists() && new File(slipField.getText()).exists()){
				callAlgorithm();
			}
		}
		else if(command.equals("Cancel")){
			if (isExitRequired()) {
                System.exit(0);
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
            	dispose();
                return;
            }
		}
	}
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub
		MipavUtil.displayInfo("Comparison file is in" + new File(reportField.getText()).getParent());
		
	}
	
	protected void callAlgorithm(){
		
		File reportFile = new File(reportField.getText());
		File slipFile = new File(slipField.getText());
		String directory = reportFile.getParent();
		csvFile = new File(directory.concat("/comparison.csv"));
		try {
			if(csvFile.exists()){
				csv = new FileWriter(csvFile, true);
			}
			else{
				csv = new FileWriter(csvFile, true);
				initCSV();
			}
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		parseAlg = new PlugInAlgorithmParseSlips(reportFile, slipFile, csv);
		parseAlg.addListener(this);
		parseAlg.run();
		setVisible(false);
		
	}
	
	private void init(){
		setForeground(Color.black);
        setTitle("Choose CSV Locations");

        JPanel dirPanel = new JPanel();
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose CSV Directory"));
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please choose a folder that contains the two csv files to compare. <br>"
        		+ "The summary of reports should go to Report, and the other should <br>"
        		+ "into Slip.</html>";

        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel);
        getContentPane().add(dirPanel, BorderLayout.NORTH);
        
        JPanel choosePanel = new JPanel();
        
        reportField = new JTextField(30);
        reportField.setText("Report CSV");
        reportField.setFont(serif12);
        choosePanel.add(reportField);
        
        JButton repButton = new JButton("Choose Report");
        repButton.setFont(serif12);
        repButton.setActionCommand("Report");
        repButton.addActionListener(this);
        choosePanel.add(repButton);
        
        JPanel slipPanel = new JPanel();
        
        slipField = new JTextField(30);
        slipField.setText("Slip CSV");
        slipField.setFont(serif12);
        slipPanel.add(slipField);
        
        JButton slipButton = new JButton("Choose Slip");
        slipButton.setFont(serif12);
        slipButton.setActionCommand("Slip");
        slipButton.addActionListener(this);
        slipPanel.add(slipButton);
        
        PanelManager manager = new PanelManager();
        manager.add(choosePanel);
        manager.addOnNextLine(slipPanel);
        
        getContentPane().add(manager.getPanel(), BorderLayout.CENTER);

        JPanel OKCancelPanel = new JPanel();

        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
	}
	
	private void initCSV(){
		String header = "Site Tube ID,,";
		header += "Subject ID 1,Subject ID 2,";
		header += "Gender 1,Gender 2,";
		header += "Alias ID 1,Alias ID 2,";
		header += "Side ID 1,Site ID 2,";
		header += "Collect Date 1,Collect Date 2\n";
		try {
			csv.append(header);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private void openDir(JTextField dirText, String command){
		
		whichFile = command;
		FileNameExtensionFilter csvFilter = new FileNameExtensionFilter("CSV file", "csv");
		fileChooser = new JFileChooser(baseDir);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addActionListener(this);
		fileChooser.addChoosableFileFilter( csvFilter);
		fileChooser.setAcceptAllFileFilterUsed(false);
		fileChooser.setFileFilter(csvFilter);
		fileChooser.showOpenDialog(this);
		
	}

}
