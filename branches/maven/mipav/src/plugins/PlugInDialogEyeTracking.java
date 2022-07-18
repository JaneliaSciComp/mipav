import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Timer;

import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * Eye tracker plugin dialog to record simple image processing events, such as zoom-in, out, mouse adjusting win-level, etc. 
 * @author Ruida Cheng
 *
 */
public class PlugInDialogEyeTracking extends JDialogStandalonePlugin {

	// System time stamp label.   The label is used by Eye tracking system as a reference to conver the C++ time to Java defined
	// system time. 
	private JLabel systemTimeLabel;
	
	// csv file text field. 
	private JTextField textFieldSavedCSVfile;
	
	// csv file choose botton. 
	private JButton buttonSavedCSVFile;
	
	// csv file chooser
	private JFileChooser csvChooser = new JFileChooser();
	
	// user interface
	private ViewUserInterface UI;
	
	// csv file name
	private String csvFileName;
	
	// csv file directory
	private String csvFileDir;
	
	
	
	public PlugInDialogEyeTracking() {
		UI = ViewUserInterface.getReference();
		init();
	}

	// get the Java system time format.
	private String getTimeStamp() {
		SimpleDateFormat sdfDate = new SimpleDateFormat("HH:mm:ss:SSS");
		Date now = new Date();
		String strDate = sdfDate.format(now);
		return strDate;
	}

	// initial the eye tracking plugin dialog
	private void init() {

		setForeground(Color.black);
		setTitle("Mobile Eye Tracker");
		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		JPanel timePanel = new JPanel();
		timePanel.setLayout(new GridLayout(3, 2));
		timePanel.setBorder(buildTitledBorder("Recording"));

		systemTimeLabel = new JLabel(getTimeStamp());

		timePanel.add(systemTimeLabel, gbc);
		gbc.gridx = 1;
		JLabel emptyLabel1 = new JLabel("");
		timePanel.add(emptyLabel1, gbc);

		// Saved CSV file directory
		gbc.gridx = 0;
		gbc.gridy = 1;

		String defaultDirectory = System.getProperties().getProperty("user.home") + File.separator + "mipav" + File.separator;
		String defaultFileName = defaultDirectory + "eyetracking-" + System.currentTimeMillis() + ".csv";
		
		textFieldSavedCSVfile = new JTextField(10);
		textFieldSavedCSVfile.setText(defaultFileName);
		textFieldSavedCSVfile.setFont(serif12);

		timePanel.add(textFieldSavedCSVfile, gbc);

		buttonSavedCSVFile = new JButton("Choose CSV file");
		buttonSavedCSVFile.addActionListener(this);
		buttonSavedCSVFile.setActionCommand("ChooseCSVfile");
		buttonSavedCSVFile.setFont(serif12B);

		gbc.gridx = 1;
		timePanel.add(buttonSavedCSVFile, gbc);

		// Add Recording Start and Stop buttons. 
		gbc.gridx = 0;
		gbc.gridy = 2;
		buildRecordButton();
		buildStopButton();
		timePanel.add(OKButton, gbc);
		gbc.gridx = 1;
		timePanel.add(cancelButton, gbc);
		
		
		JPanel mainPanel = new JPanel(new BorderLayout());

		mainPanel.add(timePanel);
		mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

		getContentPane().add(mainPanel);

		new Timer(100, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				systemTimeLabel.setText(getTimeStamp());
			}
		}).start();

		pack();
		setVisible(true);

	}

	/**
	 * Action performed. 
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		if (command.equals("RecordCSV")) {
			MipavUtil.setEyeTrackingEnabled(true, textFieldSavedCSVfile.getText());
			OKButton.setEnabled(false);
			cancelButton.setEnabled(true);
		} else if (command.equals("StopCSV")) {
			MipavUtil.setEyeTrackingEnabled(false, null);
			OKButton.setEnabled(true);
			cancelButton.setEnabled(false);
		} else if (command.equals("ChooseCSVfile")) {	
			chooseCSV();
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

	/**
	 * Builds the OK button. Sets it internally as well return the just-built
	 * button.
	 *
	 * @return JButton ok button
	 */
	protected JButton buildRecordButton() {
		OKButton = new JButton("Start");
		OKButton.addActionListener(this);
		OKButton.setActionCommand("RecordCSV");
		OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
		OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
		OKButton.setFont(serif12B);

		return OKButton;
	}

	/**
	 * Builds the cancel button. Sets it internally as well return the
	 * just-built button.
	 *
	 * @return JButton cancel button
	 */
	protected JButton buildStopButton() {
		cancelButton = new JButton("Stop");
		cancelButton.addActionListener(this);
		cancelButton.setActionCommand("StopCSV");
		cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
		cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
		cancelButton.setFont(serif12B);
		cancelButton.setEnabled(false);

		return cancelButton;
	}
}
