import java.awt.BorderLayout;
import java.awt.Color;
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
import javax.swing.JTextField;
import javax.swing.filechooser.FileNameExtensionFilter;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;


public class PlugInDialogRedcapToCDE extends JDialogStandalonePlugin implements AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8643007311361159454L;

	private JTextField dirText;
	
	private JTextField stewardText;
	
	private JTextField submitText;
	
	private JTextField keyText;
	
	private JFileChooser fileChooser;
	
	public PlugInDialogRedcapToCDE(){
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
		if(algorithm instanceof PlugInAlgorithmRedcapToCDE){
				if(algorithm.isCompleted()){
				String fileStr = dirText.getText();
				File file = new File(fileStr);
				MipavUtil.displayInfo("Output file is in " + file.getParent());
			}
		}
	}

	protected void callAlgorithm(){
		
		String fileStr = dirText.getText();
		File file = new File(fileStr);
		
		if(!file.exists()){
			MipavUtil.displayError("File does not exists!");
			return;
		}
		
		String steward = stewardText.getText();
		String submitter = submitText.getText();
		String key = keyText.getText();
		
		if(steward.equals("") || submitter.equals("")
				|| key.equals("")){
			MipavUtil.displayError("All fields must be populated");
			return;
		}
		
		PlugInAlgorithmRedcapToCDE alg = new PlugInAlgorithmRedcapToCDE(file, submitter, steward, key);
		alg.addListener(this);
		alg.run();
	}
	
	private void chooseDir(){

		FileNameExtensionFilter csvFilter = new FileNameExtensionFilter("CSV file", "csv");
		fileChooser = new JFileChooser(Preferences.getImageDirectory());
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.addActionListener(this);
		fileChooser.addChoosableFileFilter( csvFilter);
		fileChooser.setAcceptAllFileFilterUsed(false);
		fileChooser.setFileFilter(csvFilter);
		fileChooser.showOpenDialog(this);
	}
	
	private void init(){
		
		setForeground(Color.black);
        setTitle("REDCap Conversion to CDE");

        JPanel dirPanel = new JPanel();
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose CSV File"));
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please choose the REDCap file to convert to CDE format.</html>";

        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel);
        getContentPane().add(dirPanel, BorderLayout.NORTH);
        
        JPanel centerPanel = new JPanel(new BorderLayout());
        
        JPanel choosePanel = new JPanel();
        
        dirText = new JTextField(30);
        dirText.setText("Choose file");
        dirText.setFont(serif12);
        choosePanel.add(dirText);
        
        JButton dirButton = new JButton("Choose");
        dirButton.setFont(serif12);
        dirButton.addActionListener(this);
        choosePanel.add(dirButton);
        
        centerPanel.add(choosePanel, BorderLayout.NORTH);
        
        JPanel textPanel = new JPanel(new GridBagLayout());
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);
        
        JLabel subLabel = new JLabel("Submitting organization");
        subLabel.setFont(serif12);
        textPanel.add(subLabel, gbc);
        
        gbc.gridx = 1;
        gbc.weightx = 1;
        
        submitText = new JTextField(20);
        submitText.setFont(serif12);
        textPanel.add(submitText);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        
        JLabel stewLabel = new JLabel("Steward organization");
        stewLabel.setFont(serif12);
        textPanel.add(stewLabel, gbc);
        
        gbc.gridx = 1;
        gbc.weighty = 1;
        
        stewardText = new JTextField(20);
        stewardText.setFont(serif12);
        textPanel.add(stewardText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        
        JLabel keyLabel = new JLabel("Keyword");
        keyLabel.setFont(serif12);
        textPanel.add(keyLabel, gbc);

        gbc.gridx = 1;
        gbc.weighty = 1;
        
        keyText = new JTextField(20);
        keyText.setFont(serif12);
        textPanel.add(keyText, gbc);
        
        centerPanel.add(textPanel);
        
        getContentPane().add(centerPanel, BorderLayout.CENTER);

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
