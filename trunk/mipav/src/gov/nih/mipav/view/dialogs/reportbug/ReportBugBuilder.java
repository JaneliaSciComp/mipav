package gov.nih.mipav.view.dialogs.reportbug;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.awt.datatransfer.*;
import java.awt.Dialog;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.*;
import java.util.Properties;

import javax.swing.*;
import javax.swing.event.*;

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
	
	private int userType;
    
    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** String to contain the user inputed summary (character limit?) */
    private String summary;
    
    /** String to contain the full user inputed bug description*/
    private String description;
    
    private String product; //*
    
    private String platform;
    
    private String os;
    
    private String status; //*
    
    /** String to house the bugzilla priority. Accessible only to developers */
    private String priority; //d
    
    /** String to indicate the urgency with which the user needs the bug fixed */
    private String urgency; //u
    
    private String component;
    
    /** String to house the email address of the selected listserv */
    private String listServEmail;
    
    
//    /** Boolean for whether it should be sent to Bugzilla*/
//    private boolean bugzilla;
    
    /** Email of the person submitting the request. May be used to update on status of the bug. (bug update subscription checkbox?)*/
    private String email;
    
    /** Name of the submitter*/
    private String name;
    
    /** Date and time of submission */
    private String date;
    
    private String branch;
    
    private JCheckBox mipavList;
	private JCheckBox mipavDev;
	private JCheckBox bugzilla;
	
	private String[] bugTypeOptions = new String[]{"Unexpected output", "MIPAV crash", "MIPAV frozen", "Enhancement", "Other (specify below)"};
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
    
    
    
    public ReportBugBuilder(){
    	init();
    }
    
    private void init(){
    	userType = JOptionPane.showConfirmDialog(null,"Are you a MIPAV developer?", "Report a Bug", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
    	
    	displayReportForm(userType);
    }

	@Override
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		
		if (command.equals("OK")){
			if (mipavList.isSelected() || mipavDev.isSelected() || bugzilla.isSelected())
				sendReport();
			else
				MipavUtil.displayError("You must select a method of report");
		} else if (command.equals("Cancel")){
			dispose();
			System.gc();
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
			BufferedWriter report = new BufferedWriter(new FileWriter("BugReport.txt"));
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
			report.write("Priority: " + priority);
			report.write("Severity: " + severity);
			report.newLine();
			report.write("Urgency: " + urgency);
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

	private void sendReport(){
		compileReport();
		Properties properties = System.getProperties();
		properties.setProperty("mail.smtp.host", "localHost");
		Session session = Session.getDefaultInstance(properties);
		
		if (mipavList.isSelected() || mipavDev.isSelected()){
			try{
				MimeMessage report = new MimeMessage(session);
				report.setFrom(new InternetAddress(email));
				
				if (mipavList.isSelected())
					report.addRecipient(Message.RecipientType.TO, new InternetAddress("MIPAV@LIST.NIH.GOV"));
				if (mipavDev.isSelected())
					report.addRecipient(Message.RecipientType.TO, new InternetAddress("MIPAV-DEV@LIST.NIH.GOV"));
				
				//String subject = "New MIPAV Bug Report: " + 
				report.setSubject("");
				BodyPart reportSummary = new MimeBodyPart();
				Multipart parts = new MimeMultipart();
				parts.addBodyPart(reportSummary);
				
				BodyPart fullReport = new MimeBodyPart();
				DataSource file = new FileDataSource("BugReport.txt");
				fullReport.setDataHandler(new DataHandler(file));
				fullReport.setFileName("FullReport.txt");
				parts.addBodyPart(fullReport);
				
				BodyPart console = new MimeBodyPart();
				DataSource consoleFile = new FileDataSource("console.txt");
				console.setDataHandler(new DataHandler(consoleFile));
				console.setFileName("console.txt");
				parts.addBodyPart(console);
				
				report.setContent(parts);
				
				Transport.send(report);
			} catch (MessagingException e) {
//				MipavUtil.displayError("");
			}
			
		}
		if (bugzilla.isSelected()){
			
		}
	}
	
	private void displayReportForm(int user){
		GuiBuilder ref = new GuiBuilder(this);
		
		/** temporary placeholder for the GUI */
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
        
        
    	JTextArea summaryField = new JTextArea("Please give a brief (no more than 250 character) summary of the bug you encountered");
    	summaryField.setFont(MipavUtil.font12);
        summaryField.setForeground(Color.black);
        
    	JTextArea descriptionField = new JTextArea("Please give a detailed description of the bug encountered");
    	descriptionField.setFont(MipavUtil.font12);
    	descriptionField.setForeground(Color.black);
    	
    	JPanel descriptions = new JPanel();
    	descriptions.add(summaryField);
    	descriptions.add(descriptionField);
    	
    	JTextArea emailField = new JTextArea("Your email address");
    	emailField.setFont(MipavUtil.font12);
    	emailField.setForeground(Color.black);
    	
    	JTextArea nameField = new JTextArea("Your name");
    	nameField.setFont(MipavUtil.font12);
    	nameField.setForeground(Color.black);
    	
    	JTextArea branchField = new JTextArea("Your branch");
    	branchField.setFont(MipavUtil.font12);
    	branchField.setForeground(Color.black);
    	
    	JPanel personalInfo = new JPanel();
    	personalInfo.add(nameField);
    	personalInfo.add(emailField);
    	personalInfo.add(branchField);
    	
    	
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
    	
    	
    	
	}
}
