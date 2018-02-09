package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;


import gov.nih.mipav.model.scripting.*;

import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;


import javax.swing.*;


public class JDialogProstateBoundaryFeatureTrain extends JDialogScriptableBase {
	
	private JPanel trainFilePanel;
	private JTextField textTrainFileName;
	private JButton buttonTrainFileName;
	
	private JPanel featuresTrainingPanel;
	private JPanel svmOptionsPanel;
	
	private JRadioButton radioButtonSVMBinary;
	private JRadioButton radioButtonSVMMulticlass;
	private ButtonGroup group;
	
	private JPanel modelFilePanel;
	private JTextField textModelFileName;
	private JButton buttonModelFileName;
	
	private String modelFileDirAbs;
	private String modelFileName;
	
	
	private String trainFileDirAbs;
	private String trainFileName;
	
	private boolean svmTrainingBinary = true;
	
	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	/**
	 * Empty constructor needed for dynamic instantiation.
	 */
	public JDialogProstateBoundaryFeatureTrain() {
	}

	/**
	 * Creates a new JDialogHaralickTexture object.
	 * 
	 * @param theParentFrame
	 *            Parent frame.
	 * @param im
	 *            Source image.
	 */
	public JDialogProstateBoundaryFeatureTrain(Frame theParentFrame) {
		super(theParentFrame, false);
		init();
		setVisible(true);
	}

	/**
	 * Closes dialog box when the OK button is pressed and calls the algorithm.
	 * 
	 * @param event
	 *            Event that triggers function.
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		Object source = event.getSource();
		
		if (command.equals("OK")) {
			try {
				if ( svmTrainingBinary ) {             // binary class training
					
					svm_train t = new svm_train();
					t.run(textTrainFileName.getText(), textModelFileName.getText());
					dispose();
					/*
					String[] args = new String[6];
					args[0] = "-e";
					args[1] = "0.01";
					args[2] = "-s";
					args[3] = "3";
					args[4] = textTrainFileName.getText();
					args[5] = textModelFileName.getText();
					Train trainer = new Train();
					trainer.run(args);
					*/
				} else {                   
				
					// multi class training
					/*
					svm_struct_main test = new svm_struct_main();
					String[] args = new String[6];
					args[0] = "-c";
					args[1] = "5000";
					args[2] = "-e";
					args[3] = "0.1";
					args[4] = textTrainFileName.getText();
					args[5] = textModelFileName.getText();
					int argc = args.length;
					test.run(argc, args);
				    
					// hssvm multi class training
					
					String[] args = new String[10];
					args[0] = "-m";
					args[1] = "0.1";
					args[2] = "-C";
					args[3] = "2.0";
					args[4] = "-g";
					args[5] = "0.5";
					args[6] = "-e";
					args[7] = "0.001";
					args[8] = convertFormatType(textTrainFileName.getText());
					args[9] = textModelFileName.getText();
					Trainer trainer = new Trainer();
					trainer.run(args);
				    */
				}
				dispose();
			} catch ( IOException e ) {
				e.printStackTrace();
			}
			
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equalsIgnoreCase("modelFileBrowse")) {
			JFileChooser chooser = new JFileChooser();

			chooser.setDialogTitle("Choose Model File Name");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				modelFileDirAbs = chooser.getSelectedFile().getAbsolutePath();
				modelFileName = chooser.getSelectedFile().getName();
				System.err.println("ModelFileDir = " + modelFileDirAbs);
				System.err.println("ModelFileName = " + modelFileName);
				textModelFileName.setText(modelFileDirAbs);

			}
		} else if (command.equalsIgnoreCase("trainFileBrowse")) {
			JFileChooser chooser = new JFileChooser();

			chooser.setDialogTitle("Choose Trained File Name");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				trainFileDirAbs = chooser.getSelectedFile().getAbsolutePath();
				trainFileName = chooser.getSelectedFile().getName();
				System.err.println("TrainFileDir = " + trainFileDirAbs);
				System.err.println("TrainFileName = " + trainFileName);
				textTrainFileName.setText(trainFileDirAbs);

			}
		} else if ( source == radioButtonSVMBinary ) {
			svmTrainingBinary = true;
		} else if ( source == radioButtonSVMMulticlass ) {
			svmTrainingBinary = false;
		
		} 
	}

	/*
	private String convertFormatType(String featureFileName) {
		try {
			DataTransfer dataTransfer = new DataTransfer();
			String[] args = new String[1];
			args[0] = featureFileName;
			dataTransfer.run(args);
			return featureFileName + ".n";
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}
	*/
	
	/**
	 * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
	 */
	private void init() {

		setForeground(Color.black);
		
		setTitle("Prostate Features Training");
		// getContentPane().setLayout(new BorderLayout());

		JPanel mainPanel;
		
		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		buildTrainFilePanel();
		
		mainPanel.add(featuresTrainingPanel, BorderLayout.CENTER);
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);

		// setVisible( true );

		System.gc();
		
	}
	
	private void buildTrainFilePanel() {
		
		featuresTrainingPanel = new JPanel();
		featuresTrainingPanel.setBorder(buildTitledBorder("Features Training"));
		featuresTrainingPanel.setLayout(new BoxLayout(featuresTrainingPanel, BoxLayout.Y_AXIS));
		
		svmOptionsPanel = new JPanel();
		svmOptionsPanel.setLayout(new BoxLayout(svmOptionsPanel, BoxLayout.X_AXIS));
		
		radioButtonSVMBinary = new JRadioButton("SVM Binary Class", true);
		radioButtonSVMBinary.addActionListener(this);
		radioButtonSVMMulticlass = new JRadioButton("SVM Multi Class", false);
		radioButtonSVMMulticlass.addActionListener(this);
		
		group = new ButtonGroup();
		group.add(radioButtonSVMBinary);
		group.add(radioButtonSVMMulticlass);
		
		svmOptionsPanel.add(radioButtonSVMBinary);
		svmOptionsPanel.add(radioButtonSVMMulticlass);
		
		trainFilePanel = new JPanel();
		trainFilePanel.setLayout(new GridBagLayout());
		
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;

		JLabel labelTrainFileName = WidgetFactory.buildLabel("Trained File Name");
		textTrainFileName = WidgetFactory.buildTextField("");
		textTrainFileName.setColumns(10);

		buttonTrainFileName = new JButton("Browse");
		buttonTrainFileName.addActionListener(this);
		buttonTrainFileName.setActionCommand("trainFileBrowse");
		buttonTrainFileName.setMinimumSize(MipavUtil.defaultButtonSize);
		buttonTrainFileName.setPreferredSize(MipavUtil.defaultButtonSize);
		buttonTrainFileName.setFont(serif12B);

		gbc.gridx = 0;
		gbc.gridy = 0;
		trainFilePanel.add(labelTrainFileName, gbc);
		gbc.gridx = 1;
		trainFilePanel.add(textTrainFileName, gbc);
		gbc.gridx = 2;
		trainFilePanel.add(buttonTrainFileName, gbc);

		JLabel labelModelFileName = WidgetFactory.buildLabel("Model File Name");
		textModelFileName = WidgetFactory.buildTextField("");
		textModelFileName.setColumns(10);

		buttonModelFileName = new JButton("Browse");
		buttonModelFileName.addActionListener(this);
		buttonModelFileName.setActionCommand("modelFileBrowse");
		buttonModelFileName.setMinimumSize(MipavUtil.defaultButtonSize);
		buttonModelFileName.setPreferredSize(MipavUtil.defaultButtonSize);
		buttonModelFileName.setFont(serif12B);

		gbc.gridx = 0;
		gbc.gridy = 1;
		trainFilePanel.add(labelModelFileName, gbc);
		gbc.gridx = 1;
		trainFilePanel.add(textModelFileName, gbc);
		gbc.gridx = 2;
		trainFilePanel.add(buttonModelFileName, gbc);
		
		featuresTrainingPanel.add(svmOptionsPanel);
		featuresTrainingPanel.add(trainFilePanel);
		
	}
	
	protected void setGUIFromParams() {
	}
	
	protected void storeParamsFromGUI() throws ParserException {
	
	}
	
	/**
	 * Once all the necessary variables are set, call the Gaussian Haralick
	 * feature algorithm.
	 */
	protected void callAlgorithm() {
	}
}