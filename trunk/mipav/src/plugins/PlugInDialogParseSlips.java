import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.filechooser.FileNameExtensionFilter;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;


public class PlugInDialogParseSlips extends JDialogStandalonePlugin implements AlgorithmInterface{

	private PlugInAlgorithmParseSlips parseAlg;
	
	private File csvFile;
	
	private FileWriter csv;
	
	private File concatCSVFile;
	
	private FileWriter concatCSV;
	
	private JTextField reportField;
	
	private JTextField coriellField;
	
	private JFileChooser fileChooser;
	
	private String whichFile;
	
	private String baseDir;
	
	private JRadioButton delRB;
	
	private JRadioButton appRB;
	
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
		else if(command.equals("Coriell"))
			openDir(coriellField, command);
		else if(command.equals("ApproveSelection") && whichFile.equals("Report")){
			if(fileChooser.getSelectedFile().exists()){
				reportField.setText(fileChooser.getSelectedFile().toString());
				baseDir = reportField.getText();
			}
			else
				MipavUtil.displayError("This file does not exists");
		}
		else if(command.equals("ApproveSelection") && whichFile.equals("Coriell")){
			if(fileChooser.getSelectedFile().exists()){
				coriellField.setText(fileChooser.getSelectedFile().toString());
				baseDir = coriellField.getText();
			}
			else
				MipavUtil.displayError("This file does not exists");
		}
		else if(command.equals("OK")){
			if(new File(reportField.getText()).exists() && new File(coriellField.getText()).exists()){
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
		MipavUtil.displayInfo("Comparison file is in " + new File(reportField.getText()).getParent());
		
	}
	
	protected void callAlgorithm(){
		
		File reportFile = new File(reportField.getText());
		File coriellFile = new File(coriellField.getText());
		String reportText = reportField.getText();
		String reportName = reportText.substring(0, reportText.indexOf("."));
		String coriellText = coriellField.getText();
		String coriellName = coriellText.substring(0, coriellText.indexOf("."));
		csvFile = new File(coriellName + "_comparison.csv");
		concatCSVFile = new File(reportName + "_complete.csv");
		try {
			if(csvFile.exists()){
				if(delRB.isSelected()) csvFile.delete();
				csv = new FileWriter(csvFile, true);
			}
			else{
				csv = new FileWriter(csvFile, true);
				initCSV();
			}
			if(concatCSVFile.exists() && delRB.isSelected()){
				concatCSVFile.delete();
			}
			concatCSV = new FileWriter(concatCSVFile, true);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		parseAlg = new PlugInAlgorithmParseSlips(reportFile, coriellFile, csv, concatCSV);
		parseAlg.addListener(this);
		parseAlg.run();
		//setVisible(false);
		
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
        		+ "into Coriell.</html>";

        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel);
        getContentPane().add(dirPanel, BorderLayout.NORTH);
        
        JPanel choosePanel = new JPanel();
        
        reportField = new JTextField(30);
        reportField.setText("Sample Report CSV");
        reportField.setFont(serif12);
        choosePanel.add(reportField);
        
        JButton repButton = new JButton("Choose Sample");
        repButton.setFont(serif12);
        repButton.setActionCommand("Report");
        repButton.addActionListener(this);
        choosePanel.add(repButton);
        
        JPanel coriellPanel = new JPanel();
        
        coriellField = new JTextField(30);
        coriellField.setText("Coriell Report CSV");
        coriellField.setFont(serif12);
        coriellPanel.add(coriellField);
        
        JButton coriellButton = new JButton("Choose Coriell");
        coriellButton.setFont(serif12);
        coriellButton.setActionCommand("Coriell");
        coriellButton.addActionListener(this);
        coriellPanel.add(coriellButton);
        
        delRB = new JRadioButton("Delete to Old Files");
        delRB.setFont(serif12);
        delRB.setActionCommand("delete");
        
        appRB = new JRadioButton("Append Old Files");
        appRB.setFont(serif12);
        appRB.setActionCommand("append");
        appRB.setSelected(true);
        ButtonGroup group = new ButtonGroup();
        
        group.add(delRB);
        group.add(appRB);;
        
        JPanel radioPanel = new JPanel();
        radioPanel.add(appRB);
        radioPanel.add(delRB);
        
        PanelManager manager = new PanelManager();
        manager.add(choosePanel);
        manager.addOnNextLine(coriellPanel);
        manager.addOnNextLine(radioPanel);
        
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
		String header = "Site Tube ID, Coriell ID,";
		header += "Container 1,Container 2,";
		header += "Collect Date 1,Collect Date 2,";
		header += "GUID 1,GUID 2,";
		header += "Gender 1,Gender 2,";
		header += "Side ID 1,Site ID 2\n";
		
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
