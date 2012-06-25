package gov.nih.mipav.view.dialogs.reportbug;

import java.awt.*;
import java.awt.event.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

import javax.swing.*;

import java.util.*;
import javax.mail.*;
import javax.mail.internet.*;
import javax.activation.*;

public class ReportBugBuilder extends JDialogBase{

	//~ Static fields/initializers -------------------------------------------------------------------------------------
//
//    public enum UserType{
//    	/** User without developer capabilities */
//    	STANDARD,
//    	/** User with access to developer fields */
//    	DEVELOPER;
//    }
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -7015450168814651213L;

	private int userType;
    
    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** String to contain the user inputed summary (character limit?) */
    private String summary;
    
    /** String to contain the full user inputed bug description*/
    private String description;
    
    private String platform;
    
    private String os;
    
    /** String to house the bugzilla priority. Accessible only to developers */
    private String priority; //d
    
    /** String to indicate the urgency with which the user needs the bug fixed */
    private String urgency; //u
    
    private String component;
        
    /** Email of the person submitting the request. May be used to update on status of the bug. (bug update subscription checkbox?)*/
    private String email;
    
    /** Name of the submitter*/
    private String name;
    
    /** Date and time of submission */
    private Date dateHolder = new Date();;
    DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
    private String date = dateFormat.format(dateHolder);
    
    private String branch;
    
    private JCheckBox mipavList;
	private JCheckBox mipavDev;
	private JCheckBox bugzilla;
	
	private String[] bugTypeOptions = new String[]{"Unexpected output", "MIPAV crash", "MIPAV frozen", "Enhancement", "Other (specify in description)"};
	private String[] severityOptions = new String[]{"blocker", "critical", "major", "normal", "minor", "trivial", "enhancement"};
	private String[] componentOptions = new String[]{"Algorithms", "Build / Release", "DICOM Send/Recv", "DTI", "File I/O", "General", "Help / Documentation", "Histo / LUT", "Paint", "Plugins", "Scripting", "Support Request", "Utilities", "Visualization", "VOI", "Website"};
	private String[] versionOptions = new String[]{"3.0.x", "3.1.x", "4.0.x", "4.1.x", "4.2.x", "4.3.x", "4.4.x", "5.0.x", "unspecified"};
	private String[] platformOptions = new String[]{"All", "PC", "Macintosh", "Other"};
	private String[] osOptions = new String[]{"All", "Windows", "Mac OS", "Linux", "Other"};
	private String[] priorityOptions = new String[]{"P1", "P2", "P3", "P4", "P5"};
	private String[] initStateOptions = new String[]{"NEW", "ASSIGNED"};
    
    //~ Fields pulled from system --------------------------------------------------------------------------------------
    
    private String osArch = System.getProperties().getProperty("os.arch");
    
    private String osName = System.getProperties().getProperty("os.name");
    
    private String osVersion = System.getProperties().getProperty("os.version");
    
    private String javaVersion = System.getProperties().getProperty("java.version");
    
    private String javaVendor = System.getProperties().getProperty("java.vendor");
    
    private String javaVendorUrl = System.getProperties().getProperty("java.vender.url");

	private String severity;

	private String version;

	private String initState;
    
	private JFrame frame;
    
	private JTextField descriptionField = new JTextField();
	
	private JTextField summaryField = new JTextField();
	
	private JTextField nameField = new JTextField(20);
	
    private JTextField emailField = new JTextField(20);
    
    private JTextField branchField = new JTextField(20);
    
    private JTextField standardPlatform = new JTextField(20);
    
    private JTextField standardOS = new JTextField(20);
    
    private JTextField standardVersion = new JTextField(20);
	
    private JTextField standardUrgency = new JTextField(20);
    
    private File bugReport = new File("BugReport.txt");
    
    private File console = new File("console.txt");
    
    public ReportBugBuilder(){
    	init();
    	bugReport.delete();
    	console.delete();
    }
    
    private void init(){
    	userType = JOptionPane.showConfirmDialog(null,"Are you a MIPAV developer?", "Report a Bug", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
    	
    	displayReportForm(userType);
    }

	@Override
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		
		if (command.equals("OK")){
			description = descriptionField.getText();
			summary = summaryField.getText();
			if (userType == 1){
				name = nameField.getText();
				if (!emailField.getText().contains("@"))
					JOptionPane.showMessageDialog(null, "This is not a valid email address. Please check it and try again.", "Error", JOptionPane.ERROR_MESSAGE);
				email = emailField.getText();
				branch = branchField.getText();
				version = standardVersion.getText();
				platform = standardPlatform.getText();
				os = standardOS.getText();
				urgency = standardUrgency.getText();
				sendReport();
				frame.setVisible(false);
			}
			else{
				MipavUtil.displayError("You must select a method of report");
			}
		} else if (command.equals("Cancel")){
			dispose();
			System.gc();
			frame.setVisible(false);
		} else {
			comboBoxActions(command);
		}
		
	}
	
	private void comboBoxActions(String command) {
		String boxName = "bugTypeOptions";
		for(int x = 1; x<severityOptions.length; x++){
			if (command.equals(severityOptions[x])){
				boxName = "severityOptions";
				severity = command;
				break;
			}
		}
		for(int x = 1; x<componentOptions.length; x++){
			if (command.equals(componentOptions[x])){
				boxName = "componentOptions";
				component = command;
				break;
			}
		}
		for(int x = 1; x<versionOptions.length; x++){
			if (command.equals(versionOptions[x])){
				boxName = "versionOptions";
				version = command;
				break;
			}
		}
		for(int x = 1; x<platformOptions.length; x++){
			if (command.equals(platformOptions[x])){
				boxName = "platformOptions";
				platform = command;
				break;
			}
		}
		for(int x = 1; x<osOptions.length; x++){
			if (command.equals(osOptions[x])){
				boxName = "osOptions";
				os = command;
				break;
			}
		}
		for(int x = 1; x<priorityOptions.length; x++){
			if (command.equals(priorityOptions[x])){
				boxName = "priorityOptions";
				priority = command;
				break;
			}
		}
		for(int x = 1; x<initStateOptions.length; x++){
			if (command.equals(initStateOptions[x])){
				boxName = "initStateOptions";
				initState = command;
				break;
			}
		}
		
		
	}
	
	private void compileReport(){
		try {
			if (!bugReport.exists()) {
				bugReport.createNewFile();
			}
			BufferedWriter report = new BufferedWriter(new FileWriter(bugReport.getName(), true));
			report.write("Bug Report");
			report.newLine();
			report.newLine();
			report.write("Date: " + date.toString());
			report.newLine();
			report.write("Name: " + name);
			report.newLine();
			report.write("Email: " + email);
			report.newLine();
			report.write("Branch: " + branch);
			report.newLine();
			report.newLine();
			report.write("Product: MIPAV");
			report.newLine();
			report.write("Version: " + version);
			report.newLine();
			report.write("Platform: " + platform);
			report.newLine();
			report.write("OS: " + os);
			report.newLine();
			if(userType == 1){
				report.write("Urgency: " + urgency);
				report.newLine();
			}
			if (userType == 0){
				report.write("Priority: " + priority);
				report.newLine();
				report.write("Severity: " + severity);
				report.newLine();
				report.write("Component: " + component);
				report.newLine();
			}
			report.write("Summary: " + summary);
			report.newLine();
			report.newLine();
			report.write(description);
			report.newLine();
			report.newLine();
			report.write("System specifications:");
			report.newLine();
			report.write("osArch = " + osArch);
			report.newLine();
			report.write("osName = " + osName);
			report.newLine();
			report.write("osVersion = " + osVersion);
			report.newLine();
			report.write("javaVersion = " + javaVersion);
			report.newLine();
			report.write("javaVendor = " + javaVendor);
			report.newLine();
			report.write("javaVendorUrl = " + javaVendorUrl);
			report.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 	
	}

	private void sendReport(){
		compileReport();
		try {
			if (!console.exists())
				console.createNewFile();
			PrintStream consoleErrors = new PrintStream(new FileOutputStream(console.getName()));
			System.setErr(consoleErrors);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			Address to = new InternetAddress("shens2@mail.nih.gov");
			Address from = new InternetAddress(email, name);
			String host = "mailfwd.nih.gov";
			Properties properties = System.getProperties();
			properties.put("mail.host", host);
			Session session = Session.getDefaultInstance(properties);

			if (userType == 1){
				try{
					MimeMessage report = new MimeMessage(session);
					report.setFrom(from);
					
					report.addRecipient(Message.RecipientType.TO, to);
					
					report.setSubject("New Bug Report " + date);
					BodyPart reportSummary = new MimeBodyPart();
					reportSummary.setText(summary);
					Multipart parts = new MimeMultipart();
					parts.addBodyPart(reportSummary);
					
					reportSummary = new MimeBodyPart();
					String file = "BugReport.txt";
					DataSource source = new FileDataSource(file);
					reportSummary.setDataHandler(new DataHandler(source));
					reportSummary.setFileName(file);
					parts.addBodyPart(reportSummary);
					
					BodyPart console = new MimeBodyPart();
					file = "console.txt";
					source = new FileDataSource(file);
					console.setDataHandler(new DataHandler(source));
					console.setFileName(file);
					parts.addBodyPart(console);
					
					report.setContent(parts);
					
					Transport.send(report);
					
					JOptionPane.showMessageDialog(null, "Message sent successfully");
				} catch (MessagingException e) {
					e.printStackTrace();
				}
				
			}
			if (bugzilla.isSelected()){
				
			}
		} catch (AddressException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	private void displayReportForm(int user){
		GuiBuilder ref = new GuiBuilder(this);
		
		GridBagConstraints gbc = new GridBagConstraints();
    	gbc.gridwidth = 1;
    	gbc.gridheight = 1;
    	gbc.anchor = GridBagConstraints.WEST;
    	gbc.weightx = 1;
		
		
		/** temporary placeholder for the GUI */
		if(userType == 0)
			JOptionPane.showMessageDialog(null, "This feature is not ready yet", "Report a Bug", JOptionPane.ERROR_MESSAGE);
    	
    	setTitle("Report a Bug");
    	
    	JComboBox bugType = ref.buildComboBox("Bug type", bugTypeOptions);
    	bugType.addActionListener(this);
    	
    	bugzilla = new JCheckBox("Bugzilla");
        bugzilla.setFont(MipavUtil.font12);
        bugzilla.setForeground(Color.black);
        bugzilla.addActionListener(this);
        bugzilla.setActionCommand("bugzilla");
        
        mipavList = new JCheckBox("Mipav Listserv");
        mipavList.setFont(MipavUtil.font12);
        mipavList.setForeground(Color.black);
        mipavList.addActionListener(this);
        mipavList.setActionCommand("mipavList");
        
        mipavDev = new JCheckBox("Mipav Developers ListServ");
        mipavDev.setFont(MipavUtil.font12);
        mipavDev.setForeground(Color.black);
        mipavDev.addActionListener(this);
        mipavDev.setActionCommand("mipavList");
    	
        ButtonGroup sendMethod = new ButtonGroup();
        
        sendMethod.add(bugzilla);
        sendMethod.add(mipavList);
        sendMethod.add(mipavDev);
        
        
        JLabel summaryInstructions = new JLabel("Please give a brief (no more than 250 character) summary of the bug you encountered");
    	summaryField.setPreferredSize(new Dimension(400,20));
    	summaryInstructions.setFont(MipavUtil.font12);
        summaryInstructions.setForeground(Color.black);
        summaryField.addActionListener(this);
        
    	JLabel descInstructions = new JLabel("Please give a detailed description of the bug encountered");
        descriptionField.setPreferredSize(new Dimension(400,20));
    	descInstructions.setFont(MipavUtil.font12);
    	descInstructions.setForeground(Color.black);
    	descriptionField.addActionListener(this);
    	
    	JPanel descriptions = new JPanel();
    	descriptions.setLayout(new GridBagLayout());
    	gbc.gridx = 0;
    	gbc.gridy = 2;
    	gbc.weighty = 1;
    	descriptions.add(bugType);
    	gbc.gridy = 3;
    	gbc.fill = GridBagConstraints.VERTICAL;
    	descriptions.add(summaryInstructions, gbc);
    	gbc.gridy = 4;
    	descriptions.add(summaryField, gbc);
    	gbc.gridy = 5;
    	descriptions.add(descInstructions, gbc);
    	gbc.gridy = 6;
    	descriptions.add(descriptionField, gbc);
    	
    	JLabel emailLabel = new JLabel("Your email address");
    	emailLabel.setFont(MipavUtil.font12);
    	emailLabel.setForeground(Color.black);
    	//emailField.addActionListener(this);
    	
    	JLabel nameLabel = new JLabel("Your name");
    	nameLabel.setFont(MipavUtil.font12);
    	nameLabel.setForeground(Color.black);
    	//nameField.addActionListener(this);
    	
    	JLabel branchLabel = new JLabel("Your branch");
    	branchLabel.setFont(MipavUtil.font12);
    	branchLabel.setForeground(Color.black);
    	//branchField.addActionListener(this);
    	
    	JPanel personalInfo = new JPanel();
    	personalInfo.setLayout(new GridBagLayout());
    	gbc.gridx = 0;
    	gbc.gridy = 0;
    	gbc.weighty = 1;
    	gbc.fill = GridBagConstraints.VERTICAL;
    	personalInfo.add(nameLabel, gbc);
    	gbc.gridy = 1;
    	gbc.fill = GridBagConstraints.NONE;
    	personalInfo.add(nameField, gbc);
    	gbc.gridy = 2;
    	personalInfo.add(emailLabel, gbc);
    	gbc.gridy = 3;
    	personalInfo.add(emailField, gbc);
    	gbc.gridy = 4;
    	personalInfo.add(branchLabel, gbc);
    	gbc.gridy = 5;
    	personalInfo.add(branchField, gbc);
    	
    	
    	JLabel versionLabel = new JLabel("Version of MIPAV you are running");
    	versionLabel.setFont(MipavUtil.font12);
    	versionLabel.setForeground(Color.black);
    	//standardVersion.addActionListener(this);
    	
    	JLabel platformLabel = new JLabel("Platform you are operating (ex. PC)");
    	platformLabel.setFont(MipavUtil.font12);
    	platformLabel.setForeground(Color.black);
    	//standardPlatform.addActionListener(this);
    	
    	JLabel osLabel = new JLabel("Operating System you are using");
    	osLabel.setFont(MipavUtil.font12);
    	osLabel.setForeground(Color.black);
    	
    	
    	JLabel urgencyLabel = new JLabel("How urgent is this bug? When do you need it fixed by?");
    	urgencyLabel.setFont(MipavUtil.font12);
    	urgencyLabel.setForeground(Color.black);
    	
    	JPanel standardUserInput = new JPanel();
    	standardUserInput.setLayout(new GridBagLayout());
    	gbc.gridx = 0;
    	gbc.gridy = 0;
    	gbc.weighty = 1;
    	gbc.fill = GridBagConstraints.VERTICAL;
    	standardUserInput.add(versionLabel, gbc);
    	gbc.gridy = 1;
    	gbc.fill = GridBagConstraints.NONE;
    	standardUserInput.add(standardVersion, gbc);
    	gbc.gridy = 2;
    	standardUserInput.add(platformLabel, gbc);
    	gbc.gridy = 3;
    	standardUserInput.add(standardPlatform, gbc);
    	gbc.gridy = 4;
    	standardUserInput.add(osLabel, gbc);
    	gbc.gridy = 5;
    	standardUserInput.add(standardOS, gbc);
    	gbc.gridy = 6;
    	standardUserInput.add(urgencyLabel, gbc);
    	gbc.gridy = 7;
    	standardUserInput.add(standardUrgency, gbc);
    	
    	JPanel sidePanel = new JPanel();
    	sidePanel.setLayout(new BorderLayout());
    	sidePanel.add(personalInfo, BorderLayout.NORTH);
    	sidePanel.add(standardUserInput, BorderLayout.CENTER);

    	
    	JComboBox severity = ref.buildComboBox("Severity", severityOptions);
    	severity.addActionListener(this);
    	
    	JComboBox component = ref.buildComboBox("Component", componentOptions);
    	component.addActionListener(this);
    	
    	JComboBox version = ref.buildComboBox("Version", versionOptions);
    	version.addActionListener(this);
    	
    	JComboBox platform = ref.buildComboBox("Platform", platformOptions);
    	platform.addActionListener(this);
    	
    	JComboBox os = ref.buildComboBox("OS", osOptions);
    	os.addActionListener(this);
    	
    	JComboBox priority = ref.buildComboBox("Priority", priorityOptions);
    	priority.addActionListener(this);
    	
    	JComboBox initState = ref.buildComboBox("Initial State", initStateOptions);
    	initState.addActionListener(this);
    	
    	JPanel bugzillaFields = new JPanel();
    	bugzillaFields.add(severity);
    	bugzillaFields.add(component);
    	bugzillaFields.add(version);
    	bugzillaFields.add(platform);
    	bugzillaFields.add(os);
    	bugzillaFields.add(priority);
    	bugzillaFields.add(initState);
    	
    	buildOKButton();
    	buildCancelButton();
    	JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
    	
    	
    	JPanel mainPanel = new JPanel();
    	mainPanel.setLayout(new BorderLayout());
    	
    	if (userType == 1){
    		mainPanel.add(sidePanel, BorderLayout.WEST);
    		mainPanel.add(descriptions, BorderLayout.EAST);
    		mainPanel.add(buttonPanel, BorderLayout.SOUTH);
    	}
    	
    	frame = new JFrame("Report a Bug");
    	frame.getContentPane().add(mainPanel);
		frame.pack();
		frame.setVisible(true);
	}
}
