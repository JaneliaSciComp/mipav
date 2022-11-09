import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ScrollCorrector;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Enumeration;

import javax.swing.*;
import javax.swing.border.LineBorder;

public class PlugInDialogFullScreenDisplay extends JDialogBase implements
		AlgorithmInterface, ActionListener {

	private JPanel mainPanel;

	private GridBagConstraints gbc;

	private JLabel fileLabel;

	private JLabel imageLabel;

	private JComboBox comboBoxImage;

	private ModelImage image = null;

	private JTextField filePathTextField;

	private JButton fileBrowseButton;

	private JTextArea outputTextArea;

	private JScrollPane scrollPane;

	private JLabel csvLabel;
	
	private JTextField textFieldSavedCSVfile;
	
	private JButton buttonSavedCSVFile;
	
	private String fileName;

	String directory;

	private String currDir;

	private PlugInAlgorithmFullScreenDisplay alg;

	// csv file chooser
	private JFileChooser csvChooser = new JFileChooser();
	
	// user interface
	private ViewUserInterface UI;
	
	// csv file name
	private String csvFileName;
	
	// csv file directory
	private String csvFileDir;
		
	
	public PlugInDialogFullScreenDisplay() {

	}

	public PlugInDialogFullScreenDisplay(final boolean modal) {
		super(modal);
		init();
	}

	private void init() {
		setForeground(Color.black);
		setTitle("Full Screen Display v1.0");
		mainPanel = new JPanel(new GridBagLayout());
		gbc = new GridBagConstraints();

		fileLabel = new JLabel("File ");
		filePathTextField = new JTextField(35);
		filePathTextField.setEditable(false);
		filePathTextField.setBackground(Color.white);
		fileBrowseButton = new JButton("Browse");
		fileBrowseButton.addActionListener(this);
		fileBrowseButton.setActionCommand("FileBrowse");

		imageLabel = new JLabel("Image ");
		comboBoxImage = new JComboBox();
		comboBoxImage.setFont(serif12);
		comboBoxImage.setBackground(Color.white);

		buildComboBoxImage();
		Object selected = comboBoxImage.getSelectedItem();
		if (selected != null) {
			comboBoxImage.setSelectedItem(selected);
			String selectedName = (String) comboBoxImage.getSelectedItem();
			image = ViewUserInterface.getReference().getRegisteredImageByName(
					selectedName);
			image.getParentFrame().getComponentImage().setEyetrackerRecordingMode(ViewJComponentEditImage.PluginEyetrackerMode);
		}
		comboBoxImage.addActionListener(this);
		comboBoxImage.setActionCommand("ImageBrowse");

		outputTextArea = new JTextArea();
		outputTextArea.setRows(15);
		outputTextArea.setEditable(false);
		outputTextArea.setBackground(Color.lightGray);
		outputTextArea.setBorder(new LineBorder(Color.black));
		outputTextArea.setForeground(Color.black);
		scrollPane = new JScrollPane(outputTextArea,
				ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

		scrollPane.getVerticalScrollBar().addAdjustmentListener(
				new ScrollCorrector());
	
		String defaultDirectory = System.getProperties().getProperty("user.home") + File.separator + "mipav" + File.separator;
		String defaultFileName = defaultDirectory + "eyetracking-" + System.currentTimeMillis() + ".csv";
		
		
		csvLabel = new JLabel("CSV File ");
		textFieldSavedCSVfile = new JTextField(10);
		textFieldSavedCSVfile.setText(defaultFileName);
		textFieldSavedCSVfile.setFont(serif12);
		buttonSavedCSVFile = new JButton("Choose CSV file");
		buttonSavedCSVFile.addActionListener(this);
		buttonSavedCSVFile.setActionCommand("ChooseCSVfile");
		buttonSavedCSVFile.setFont(serif12B);

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.insets = new Insets(15, 5, 5, 15);
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.EAST;
		mainPanel.add(fileLabel, gbc);
		gbc.gridx = 1;
		mainPanel.add(filePathTextField, gbc);
		gbc.gridx = 2;
		mainPanel.add(fileBrowseButton, gbc);
		
		gbc.gridx = 0;
		gbc.weightx = 0;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.NONE;
		mainPanel.add(imageLabel, gbc);
		gbc.gridx = 1;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		mainPanel.add(comboBoxImage, gbc);

		gbc.anchor = GridBagConstraints.CENTER;
		gbc.fill = GridBagConstraints.BOTH;

		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.gridwidth = 3;
		mainPanel.add(scrollPane, gbc);

	
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.insets = new Insets(15, 5, 5, 15);
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.EAST;
		mainPanel.add(csvLabel, gbc);
		gbc.gridx = 1;
		mainPanel.add(textFieldSavedCSVfile, gbc);
		gbc.gridx = 2;
		mainPanel.add(buttonSavedCSVFile, gbc);
       
		
		
		final JPanel OKCancelPanel = new JPanel();
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

	/**
	 * Builds a list of images to operate on from the template image.
	 */
	private void buildComboBoxImage() {
		ViewUserInterface UI;

		comboBoxImage.removeAllItems();

		UI = ViewUserInterface.getReference();

		Enumeration<String> names = UI.getRegisteredImageNames();
		while (names.hasMoreElements()) {
			String name = names.nextElement();
			comboBoxImage.addItem(name);
		}
	}

	private void callAlgorithm() {
		int z;
		Image cornerImage;
		try {
			cornerImage = MipavUtil.getIconImage("WhiteCircle_550.png");
		} catch (final FileNotFoundException e) {
			MipavUtil.displayError("Eye tracker landmark image not found.");
			e.printStackTrace();
			return;
		}
		if (image == null) {
			final FileIO fileIO = new FileIO();
			final boolean multiFile = false;
			image = fileIO.readImage(fileName, directory, multiFile, null);
			image.getParentFrame().getComponentImage().setEyetrackerRecordingMode(ViewJComponentEditImage.PluginEyetrackerMode);
		}

		/*
		 * try { inputImage = ImageIO.read( new File(directory + fileName) ); }
		 * catch (IOException e) { MipavUtil.displayError("IOException " + e +
		 * " on ImageIO.read( new File(directory + fileName)"); return; }
		 */
		alg = new PlugInAlgorithmFullScreenDisplay(image, cornerImage,
				outputTextArea);

		alg.addListener(this);

		if (isRunInSeparateThread()) {

			// Start the thread as a low priority because we wish to still
			// have user interface work fast.
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil
						.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
	}

	@Override
	public void algorithmPerformed(final AlgorithmBase algorithm) {
		if (alg.isCompleted()) {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			OKButton.setEnabled(false);
			cancelButton.setText("Close");

			outputTextArea.append("Finished" + "\n");

		}

	}

	@Override
	public void actionPerformed(final ActionEvent e) {
		final String command = e.getActionCommand();
		if (command.equalsIgnoreCase("FileBrowse")) {
			final JFileChooser chooser = new JFileChooser(
					Preferences.getImageDirectory());
			/*
			 * if (currDir != null) { chooser.setCurrentDirectory(new
			 * File(currDir)); }
			 */
			chooser.setDialogTitle("Choose File");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			final int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				currDir = chooser.getSelectedFile().getAbsolutePath();
				Preferences.setImageDirectory(new File(currDir));
				fileName = chooser.getSelectedFile().getName();
				directory = chooser.getCurrentDirectory() + File.separator;
				filePathTextField.setText(currDir);
				if (image != null) {
					image.disposeLocal();
					image = null;
				}
			}
		} else if (command.equals("ImageBrowse")) {
			Object selected = comboBoxImage.getSelectedItem();
			if (selected != null) {
				comboBoxImage.setSelectedItem(selected);
				String selectedName = (String) comboBoxImage.getSelectedItem();
				if (image != null) {
					image.disposeLocal();
					image = null;
				}
				image = ViewUserInterface.getReference()
						.getRegisteredImageByName(selectedName);
			}
		} else if (command.equalsIgnoreCase("ok")) {
			MipavUtil.setEyeTrackingEnabled(true, textFieldSavedCSVfile.getText());
			if (setVariables()) {
				callAlgorithm();
			}
		} else if (command.equalsIgnoreCase("cancel")) {

			dispose();
		} else if (command.equals("ChooseCSVfile")) {	
			chooseCSV();
		} else {
			super.actionPerformed(e);
		} 

	}

	/**
	 * Let the user to choose the csv file. 
	 */
	private void chooseCSV() {

		csvChooser.setDialogTitle("Open CSV file directory");
		csvChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		final int returnValue = csvChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			csvFileName = csvChooser.getSelectedFile().getName();
			csvFileDir = String.valueOf(csvChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ csvFileName;
	
			textFieldSavedCSVfile.setText(csvFileDir);
			// System.err.println("check = " + csvFileDir);
		} else {
			return;
		}
	}
	
	private boolean setVariables() {

		if ((image == null) && (filePathTextField.getText().trim().equals(""))) {
			MipavUtil.displayError("File is required");
			return false;

		}

		return true;
	}

}
