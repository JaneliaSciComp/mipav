import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
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
	
	private JCheckBox headerCheck;
	
	private JTextField catalogField;
	
	private JTextField removeField;
	
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
		else if(command.equals("Catalog"))
			openDir(catalogField, command);
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
		else if(command.equals("ApproveSelection") && whichFile.equals("Catalog")){
			if(fileChooser.getSelectedFile().exists()){
				catalogField.setText(fileChooser.getSelectedFile().toString());
				baseDir = catalogField.getText();
			}
			else 
				MipavUtil.displayError("This file does not exists");
		}
		else if(command.equals("OK")){
			if(new File(reportField.getText()).exists() && new File(coriellField.getText()).exists()){
				callAlgorithm();
			}
		}
		else if(command.equals("Cancel") || command.equals("CatalogCan")){
			if (isExitRequired()) {
                System.exit(0);
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
            	dispose();
                return;
            }
		}
		else if(command.equals("CatalogOK")){
			if(new File(catalogField.getText()).exists()){
				callAlgorithm2();
			}
		}
	}
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub
		if(algorithm instanceof PlugInAlgorithmParseSlips && algorithm.isCompleted())
			MipavUtil.displayInfo("Comparison file is in " + new File(reportField.getText()).getParent());
		else if(algorithm instanceof PlugInAlgorithmCompleteCatalog && algorithm.isCompleted())
			MipavUtil.displayInfo("Output file is in " + new File(catalogField.getText()).getParent());
		
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
			if(concatCSVFile.exists() && delRB.isSelected()){
				concatCSVFile.delete();
			}
			concatCSV = new FileWriter(concatCSVFile, true);
			
			if(csvFile.exists()){
				if(delRB.isSelected()) {
					csvFile.delete();
					csv = new FileWriter(csvFile, true);
					initCSV();
				}
				else csv = new FileWriter(csvFile, true);
			}
			else{
				csv = new FileWriter(csvFile, true);
				initCSV();
			}
			
			parseAlg = new PlugInAlgorithmParseSlips(reportFile, coriellFile, csv, concatCSV);
			parseAlg.addListener(this);
			parseAlg.removeHeader(headerCheck.isSelected());
			parseAlg.run();
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			MipavUtil.displayError("CSV files are locked (something is open)");
		}
		
		//setVisible(false);
		
	}
	
	protected void callAlgorithm2(){
		File catalogFile = new File(catalogField.getText());
		String catalogFolder = catalogFile.getParent();
		File catalogCSV = new File(catalogFolder + "/Complete_count.csv");
		int remove = Integer.parseInt(removeField.getText());
		try{
			csv = new FileWriter(catalogCSV);
			PlugInAlgorithmCompleteCatalog catAlg = new PlugInAlgorithmCompleteCatalog(catalogFile, csv, remove);
			catAlg.addListener(this);
			catAlg.run();
		}
		catch (IOException e) {
			MipavUtil.displayError("CSV file is locked");
		}
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
        
        JPanel checkPanel = new JPanel();
        headerCheck = new JCheckBox("Remove header in Coriell file");
        headerCheck.setSelected(true);
        checkPanel.add(headerCheck);
        
        JPanel OKCancelPanel = new JPanel();

        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        
        PanelManager manager = new PanelManager();
        manager.add(dirPanel);
        manager.addOnNextLine(choosePanel);
        manager.addOnNextLine(coriellPanel);
        manager.addOnNextLine(radioPanel);
        manager.addOnNextLine(checkPanel);
        manager.addOnNextLine(OKCancelPanel);
        
        //getContentPane().add(manager.getPanel(), BorderLayout.CENTER);
        
        JPanel catalogPanel = new JPanel();
        
        catalogField = new JTextField(30);
        catalogField.setText("Catalog Report CSV");
        catalogField.setFont(serif12);
        catalogPanel.add(catalogField);
        
        JButton catalogButton = new JButton("Choose Catalog File");
        catalogButton.setFont(serif12);
        catalogButton.setActionCommand("Catalog");
        catalogButton.addActionListener(this);
        catalogPanel.add(catalogButton);
        
        JPanel removePanel = new JPanel();
        
        JLabel removeLabel = new JLabel("Remove how many header lines?");
        removeLabel.setBackground(Color.black);
        removeLabel.setFont(serif12);
        removePanel.add(removeLabel);
        
        removeField = new JTextField(5);
        removeField.setText("2");
        removeField.setFont(serif12);
        removeField.setHorizontalAlignment(JTextField.RIGHT);
        removePanel.add(removeField);
        
        JPanel OKCancelPanel2 = new JPanel();
        JButton catalogOK = new JButton("OK");
        catalogOK.setFont(serif12);
        catalogOK.setActionCommand("CatalogOK");
        catalogOK.addActionListener(this);
        OKCancelPanel2.add(catalogOK);
        JButton catalogCan = new JButton("Cancel");
        catalogCan.setFont(serif12);
        catalogCan.setActionCommand("CatalogCan");
        catalogCan.addActionListener(this);
        OKCancelPanel2.add(catalogCan);
        
        PanelManager manager2 = new PanelManager();
        manager2.add(catalogPanel);
        manager2.addOnNextLine(removePanel);
        manager2.addOnNextLine(OKCancelPanel2);
        
        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Coriel Compare", null, manager.getPanel(),
                "Coriel");
        tabbedPane.setMnemonicAt(0, KeyEvent.VK_1);
        tabbedPane.addTab("Catalog Counting", null, manager2.getPanel(),
                "Catalog");
        tabbedPane.setMnemonicAt(0, KeyEvent.VK_2);
        getContentPane().add(tabbedPane, BorderLayout.CENTER);
        getContentPane().add(new JPanel(), BorderLayout.NORTH);
        getContentPane().add(new JPanel(), BorderLayout.SOUTH);

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
		header += "Side ID 1,Site ID 2,";
		header += "Age 1,Age 2\n";
		
		String concatHeader = "Reported by Site,";
		concatHeader += ",,,,,,,,,,Reported by Coriell,\n";
		try {
			csv.append(header);
			csv.flush();
			concatCSV.append(concatHeader);
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
