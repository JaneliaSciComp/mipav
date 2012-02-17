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
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ScrollCorrector;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogDrosophilaCreateSWC extends JDialogBase implements AlgorithmInterface {

	private JPanel mainPanel;
	
	private GridBagConstraints gbc;
	
	private JLabel imageLabel, filamentFileLabel, greenValueRadiusThresholdLabel, subsamplingDistanceLabel;
	
	private JTextField imageFilePathTextField, filamentFilePathTextField, greenValueRadiusThresholdTextField, subsamplingDistanceTextField;
	
	private JButton imageBrowseButton, filamentBrowseButton;
	
	private JTextArea outputTextArea;
	
	private JScrollPane scrollPane;
	
	private File filamentFile;
	
	private ModelImage finalImage;
	
	private String currDir;
	
	private float greenThreshold, subsamplingDistance;
	
	private PlugInAlgorithmDrosophilaCreateSWC alg;
	
	
	
	
	public PlugInDialogDrosophilaCreateSWC() {
		
	}
	
	
	public PlugInDialogDrosophilaCreateSWC(boolean modal) {
		super(modal);
		init();
	}
	
	
	
	
	private void init() {
		setForeground(Color.black);
        setTitle("Drosophila Create SWC v1.0");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        
        imageLabel = new JLabel("Image ");
        imageFilePathTextField = new JTextField(35);
        imageFilePathTextField.setEditable(false);
        imageFilePathTextField.setBackground(Color.white);
        imageBrowseButton = new JButton("Browse");
        imageBrowseButton.addActionListener(this);
        imageBrowseButton.setActionCommand("imageBrowse");
        
        
        filamentFileLabel = new JLabel("Filament file ");
        filamentFilePathTextField = new JTextField(35);
        filamentFilePathTextField.setEditable(false);
        filamentFilePathTextField.setBackground(Color.white);
        filamentBrowseButton = new JButton("Browse");
        filamentBrowseButton.addActionListener(this);
        filamentBrowseButton.setActionCommand("filamentBrowse");
        
        greenValueRadiusThresholdLabel = new JLabel("SWC-Green vaue radius threshold ");
        greenValueRadiusThresholdTextField = new JTextField(35);
        greenValueRadiusThresholdTextField.setText("0.0");
        
        subsamplingDistanceLabel = new JLabel("SWC-Subsampling distance (um) ");
        subsamplingDistanceTextField = new JTextField(35);

        
        
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
        mainPanel.add(imageLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(imageFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(imageBrowseButton,gbc);
		
		
		
	 	gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.insets = new Insets(15,5,5,15);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(filamentFileLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(filamentFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(filamentBrowseButton,gbc);
        
        
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridy = 2;
        gbc.gridx = 0;
        mainPanel.add(greenValueRadiusThresholdLabel,gbc);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(greenValueRadiusThresholdTextField,gbc);
        gbc.anchor = GridBagConstraints.EAST;
        
        
        
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridy = 3;
        gbc.gridx = 0;
        mainPanel.add(subsamplingDistanceLabel,gbc);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(subsamplingDistanceTextField,gbc);
        gbc.anchor = GridBagConstraints.EAST;
        
        
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;
        
        gbc.gridx = 0;
        gbc.gridy = 4;
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
		alg = new PlugInAlgorithmDrosophilaCreateSWC(finalImage, filamentFile, greenThreshold, subsamplingDistance, outputTextArea);
	
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
		if(command.equalsIgnoreCase("imageBrowse")) {
			JFileChooser chooser = new JFileChooser();
	        if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
	        }
	        chooser.setDialogTitle("Choose image");
	        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	currDir = chooser.getSelectedFile().getAbsolutePath();
	        	FileIO fileIO = new FileIO();
	        	finalImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, true, null);
	        	float[] resols = finalImage.getResolutions(0);
	        	subsamplingDistanceTextField.setText(String.valueOf(resols[0]));
	        	imageFilePathTextField.setText(currDir);
	        }
		 }else if(command.equalsIgnoreCase("filamentBrowse")) { 
			String absPath = "";
			JFileChooser chooser = new JFileChooser();
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
	        }
	        chooser.setDialogTitle("Choose filament file");
	        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	absPath = chooser.getSelectedFile().getAbsolutePath();
	        	filamentFile = new File(absPath);
	        	filamentFilePathTextField.setText(absPath);
	        	
	        }
		}else if(command.equalsIgnoreCase("ok")) {
			 if(setVariables()) {
				 callAlgorithm();
			 }
		 }

	}
	
	
	private boolean setVariables() {
	
		if(imageFilePathTextField.getText().trim().equals("")) {
			MipavUtil.displayError("Image is required");
			return false;
			
		}
		if(filamentFilePathTextField.getText().trim().equals("")) {
			MipavUtil.displayError("Filament file is required");
			return false;
			
		}
		
		if(!subsamplingDistanceTextField.getText().trim().equals("")) {
			try {
				subsamplingDistance = Float.parseFloat(subsamplingDistanceTextField.getText().trim());
			}catch(Exception e) {
				MipavUtil.displayError("Subsampling distance is not a valid number");
				return false;
			}
		}else {
			MipavUtil.displayError("Subsampling distance is required");
			return false;
		}
	
	
		if(!greenValueRadiusThresholdTextField.getText().trim().equals("")) {
			try {
				greenThreshold = Float.parseFloat(greenValueRadiusThresholdTextField.getText().trim());
			}catch(Exception e) {
				MipavUtil.displayError("Green value radiu threshold is not a valid number");
				return false;
			}
		}else {
			MipavUtil.displayError("Green value radiu threshold is required");
			return false;
		}
		
		return true;
	}

}
