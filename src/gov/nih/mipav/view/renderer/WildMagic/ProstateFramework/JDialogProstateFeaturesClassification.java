package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class JDialogProstateFeaturesClassification extends JDialogScriptableBase {
	
	private JPanel trainFilePanel;
	private JTextField textTrainFileName;
	private JButton buttonTrainFileName;
	
	private JPanel modelFilePanel;
	private JTextField textModelFileName;
	private JButton buttonModelFileName;
	
	private JPanel outputFilePanel;
	private JTextField textOutputFileName;
	private JButton buttonOutputFileName;
	
	private String modelFileDirAbs;
	private String modelFileName;
	
	private String trainFileDirAbs;
	private String trainFileName;
	
	private String outputFileDirAbs;
	private String outputFileName;
	
	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	/**
	 * Empty constructor needed for dynamic instantiation.
	 */
	public JDialogProstateFeaturesClassification() {
	}

	/**
	 * Creates a new JDialogHaralickTexture object.
	 * 
	 * @param theParentFrame
	 *            Parent frame.
	 * @param im
	 *            Source image.
	 */
	public JDialogProstateFeaturesClassification(Frame theParentFrame) {
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

		if (command.equals("OK")) {
			try {
				// svm_train t = new svm_train();
				svm_predict.run(textTrainFileName.getText(), textModelFileName.getText(), textOutputFileName.getText());
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
		}	else if (command.equalsIgnoreCase("outputFileBrowse")) {
			JFileChooser chooser = new JFileChooser();

			chooser.setDialogTitle("Choose Output File Name");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				outputFileDirAbs = chooser.getSelectedFile().getAbsolutePath();
			    outputFileName = chooser.getSelectedFile().getName();
				System.err.println("OutputFileDir = " + outputFileDirAbs);
				System.err.println("OutputFileName = " + outputFileName);
				textOutputFileName.setText(outputFileDirAbs);

			}
		} else {
            super.actionPerformed(event);
        }
	}

	
	
	/**
	 * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
	 */
	private void init() {

		setForeground(Color.black);
		
		setTitle("Prostate Features Classification");
		// getContentPane().setLayout(new BorderLayout());

		JPanel mainPanel;
		
		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		buildTrainFilePanel();
		
		mainPanel.add(trainFilePanel, BorderLayout.CENTER);
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);

		// setVisible( true );

		System.gc();
		
	}
	
	private void buildTrainFilePanel() {
		trainFilePanel = new JPanel();
		trainFilePanel.setBorder(buildTitledBorder("Features Classification"));
		trainFilePanel.setLayout(new GridBagLayout());

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;

		JLabel labelTrainFileName = WidgetFactory.buildLabel("Classification File Name");
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

		// build Model file dialog
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
		
		
		// build classification output dialog
		JLabel labelOutputFileName = WidgetFactory.buildLabel("Output File Name");
		textOutputFileName = WidgetFactory.buildTextField("");
		textOutputFileName.setColumns(10);

		buttonOutputFileName = new JButton("Browse");
		buttonOutputFileName.addActionListener(this);
		buttonOutputFileName.setActionCommand("outputFileBrowse");
		buttonOutputFileName.setMinimumSize(MipavUtil.defaultButtonSize);
		buttonOutputFileName.setPreferredSize(MipavUtil.defaultButtonSize);
		buttonOutputFileName.setFont(serif12B);

		gbc.gridx = 0;
		gbc.gridy = 2;
		trainFilePanel.add(labelOutputFileName, gbc);
		gbc.gridx = 1;
		trainFilePanel.add(textOutputFileName, gbc);
		gbc.gridx = 2;
		trainFilePanel.add(buttonOutputFileName, gbc);
		
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