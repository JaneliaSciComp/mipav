import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;

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


public class PlugInDialogDataElementTransfer extends JDialogStandalonePlugin
		implements AlgorithmInterface {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 3324816530672361998L;

	private JTextField dirText;
	
	private JFileChooser fileChooser;

	PlugInAlgorithmDataElementTransfer alg;
	
	public PlugInDialogDataElementTransfer(){
		super();
		init();
	}
	
	public void actionPerformed(ActionEvent e){
		String command = e.getActionCommand();
		
		if (command.equals("ApproveSelection")){
			dirText.setText(fileChooser.getSelectedFile().toString());
        	Preferences.setImageDirectory(fileChooser.getSelectedFile());
        }
		else if(command.equals("Choose")) chooseDir();
		else if(command.equals("Cancel")){
			
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		}
		else if(command.equals("OK")){
			String text = dirText.getText();
			int ind = text.lastIndexOf(".");
			String ext = text.substring(ind, text.length());
			File inFile = new File(text);
			
			if(inFile.exists() && ext.equalsIgnoreCase(".csv")){
				callAlgorithm();
			}
			else if (!inFile.exists())
				MipavUtil.displayError("File does not exist");
			else MipavUtil.displayError("File is not compatible, must be CSV");
		}
	}
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		
		if(algorithm.isCompleted()){
			File output = new File(dirText.getText());
			MipavUtil.displayInfo("Complete: Output is in " + output.getParent());
		}
		else MipavUtil.displayError("Transfer aborted due to errors");

	}
	
	protected void callAlgorithm(){
		
		PlugInAlgorithmDataElementTransfer alg =
				new PlugInAlgorithmDataElementTransfer(new File(dirText.getText()));
		alg.addListener(this);
		if (isRunInSeparateThread()) {
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
		
	}
	
	private void chooseDir(){

		FileNameExtensionFilter csvFilter = new FileNameExtensionFilter("CSV file", "csv");
		fileChooser = new JFileChooser();
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.addActionListener(this);
		fileChooser.addChoosableFileFilter( csvFilter);
		fileChooser.setAcceptAllFileFilterUsed(false);
		fileChooser.setFileFilter(csvFilter);
		fileChooser.showOpenDialog(this);
	}
	
	private void init(){
		
		setForeground(Color.black);
        setTitle("REDCap Conversion");

        JPanel dirPanel = new JPanel();
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose CSV File"));
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please choose the CDE file to convert to REDCap format.</html>";

        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel);
        getContentPane().add(dirPanel, BorderLayout.NORTH);
        
        JPanel choosePanel = new JPanel();
        
        dirText = new JTextField(30);
        dirText.setText("Choose file");
        dirText.setFont(serif12);
        choosePanel.add(dirText);
        
        JButton dirButton = new JButton("Choose");
        dirButton.setFont(serif12);
        dirButton.addActionListener(this);
        choosePanel.add(dirButton);
        
        getContentPane().add(choosePanel, BorderLayout.CENTER);

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

}
