package gov.nih.mipav.view.dialogs.reportbug;

import imaging.MetaImageHeader.DataType;

import java.awt.*;
import java.awt.event.*;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogCaptureScreen;

import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

import javax.swing.*;
import javax.swing.text.AbstractDocument;

import java.util.*;
import javax.mail.*;
import javax.mail.internet.*;
import javax.activation.*;

public class ReportBugBuilder extends JDialogBase{

	//~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = -7015450168814651213L;
	
	/** Options for the type of bug encountered */
	private static final String[] bugTypeOptions = new String[]{"Unexpected Output", "MIPAV Crash", "MIPAV Frozen", "Enhancement", "Other (specify in description)"};
	
	/** Options for the severity of the bug */
	private static final String[] severityOptions = new String[]{"blocker", "critical", "major", "normal", "minor", "trivial", "enhancement"};
	
	/** Options for the component in which the bug was found */
	private static final String[] componentOptions = new String[]{"Algorithms", "Build / Release", "DICOM Send/Recv", "DTI", "File I/O", "General", "Help / Documentation", "Histo / LUT", "Paint", "Plugins", "Scripting", "Support Request", "Utilities", "Visualization", "VOI", "Website"};
	
	/** Options for the version of MIPAV the bug was found in */
	private static final String[] versionOptions = new String[]{"3.0.x", "3.1.x", "4.0.x", "4.1.x", "4.2.x", "4.3.x", "4.4.x", "5.0.x", "unspecified"};
	
	/** Options for the operating platform */
	private static final String[] platformOptions = new String[]{"All", "PC", "Macintosh", "Other"};
	
	/** Options for the operating system */
	private static final String[] osOptions = new String[]{"All", "Windows", "Mac OS", "Linux", "Other"};
	
	/** Options for the priority of the bug */
	private static final String[] priorityOptions = new String[]{"P1", "P2", "P3", "P4", "P5"};
	
	/** Options for the initial state of the bugzilla report */
	private static final String[] initStateOptions = new String[]{"NEW", "ASSIGNED"};
    
    //~ Instance fields ------------------------------------------------------------------------------------------------
    
	/** int to store what type of user is running the program. Will be 0 for developer, 1 for standard */
	private int userType;
	
    /** String to contain the user inputed summary (character limit?) */
    private String summary;
    
    /** String to contain the full user inputed bug description*/
    private String description;
    
    /** String to contain the user inputed operating platform */
    private String platform;
    
    /** String to contain the user inputed operating system. */
    private String os;
    
    /** String to house the bugzilla priority. Accessible only to developers */
    private String priority; //d
    
    /** String to indicate the urgency with which the user needs the bug fixed */
    private String urgency; //u
    
    /** String to indicate which component the bug was found in */
    private String component;
        
    /** Email of the person submitting the request */
    private String email;
    
    /** Name of the submitter*/
    private String name;
    
    /** Type of bug encountered */
    private String bugTypeString = "Unexpected Output";
    
    /** Date and time of submission */
    private Date dateHolder = new Date();;
    DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
    private String date = dateFormat.format(dateHolder);
    
    /** Branch in which the submitter works */
    private String branch;
    
    /** Checkboxes for report sending methods */
    private JCheckBox mipavList;
	private JCheckBox mipavDev;
	private JCheckBox bugzilla;
    
	/** Serverity of the bug */
	private String severity;

	/** Version of MIPAV running */
	private String version;

	/** Initial state of the bug on a bugzilla report */
	private String initState;
    
	/** Frame for the GUI in which the form is presented */
	private JFrame frame;
	
	/** Text are for displaying the attached images*/
	private JTextArea attachedImages = new JTextArea();
	
	/** Text area for user inputed bug description */
	private JTextArea descriptionField = new JTextArea();
	
	/** Text area for user inputed bug summary */
	private JTextArea summaryField = new JTextArea();
	
	/** Text field for user inputed name */
	private JTextField nameField = new JTextField(25);
	
	/** Text field for user inputed email */
    private JTextField emailField = new JTextField(25);
    
    /** Text field for user inputed branch */
    private JTextField branchField = new JTextField(25);
    
    /** Text field for the user inputed operating platform */
    private JTextField standardPlatform = new JTextField(25);
    
    /** Text field for the user inputed operating system. Pre-populated using the system properties */
    private JTextField standardOS = new JTextField(System.getProperties().getProperty("os.name"),25);
    
    /** Text field for the user inputed mipav version. Pre-populated using mipav utilities */
    private JTextField standardVersion = new JTextField(MipavUtil.getVersion(),25);
	
    /** Text field for the user inputed urgency of the bug */
    private JTextField standardUrgency = new JTextField(25);
    
    /** File to hold the full bug report */
    private File bugReport = new File("BugReport.txt");
    
    /** File to hold the console error message if mipav encounters an unaccounted for exception while running */
    private File console = new File("console.txt");
    
    private String attachmentName;
    
    private ArrayList<String> fileNames = new ArrayList<String>();
    
    private String attachments;
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Initializes the GUI and then deletes the created files once the program has finished running
     * 
     */
    public ReportBugBuilder(){
    	init();
    	bugReport.deleteOnExit();
    	console.deleteOnExit();
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates a JFrame asking if the user is a developer and then runs the method displayReportForm, which
     * displays the appropriate form for the selected type.
     * 
     */
    private void init(){
    	userType = JOptionPane.showConfirmDialog(null,"Are you a MIPAV developer?", "Report a Bug", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
    	
    	displayReportForm(userType);
    }

    /**
     * Performs the following actions based on the command:<br>
     *
     * <ul>
     *   <li>OK - Initializes pertinent fields, checking that all fields are filled out correctly, before sending the information to be processed for an email</li>
     *   <li>Cancel - cleans up and disposes the dialog</li>
     *   <li>Other - sends event to secondary method that looks for combobox events</li>
     * </ul>
     *
     * @param  event  Event that triggered this function.
     */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		
		if (command.equals("Submit")){
			description = descriptionField.getText();
			summary = summaryField.getText();
			if (userType == 1){
				name = nameField.getText();
				email = emailField.getText();
				branch = branchField.getText();
				version = standardVersion.getText();
				platform = standardPlatform.getText();
				os = standardOS.getText();
				urgency = standardUrgency.getText();
				String tempEmail = emailField.getText();
				if (!tempEmail.contains("@") || !tempEmail.contains(".") || tempEmail.charAt(tempEmail.length() - 1) == '.' || tempEmail.charAt(0) == '@' || tempEmail.contains("@.")){
					JOptionPane.showMessageDialog(null, "This is not a valid email address.", "Error", JOptionPane.ERROR_MESSAGE);
				} else if (name.length() == 0 || email.length() == 0 || branch.length() == 0 || version.length() == 0 || platform.length() == 0 || os.length() == 0 || urgency.length() == 0 || description.length() == 0 || summary.length() == 0){
					JOptionPane.showMessageDialog(null, "You must fill out all sections on this form.", "Error", JOptionPane.ERROR_MESSAGE);
				} else {
					sendReport();
					frame.setVisible(false);
				}
			}
			else{
				MipavUtil.displayError("You must select a method of report");
			}
		} else if (command.equals("Cancel")){
			dispose();
			System.gc();
			frame.setVisible(false);
		} else if (command.equals("Attach an Image")){
			JOptionPane.showMessageDialog(null, "This feature is not ready yet", "Report a Bug", JOptionPane.ERROR_MESSAGE);
			
//			final JDialogCaptureScreen screenCapture = new JDialogCaptureScreen(null, true);
//			attachmentName = screenCapture.fileName + ".png";
//			fileNames.add(attachmentName);
//			for (int x = 0; x <fileNames.size(); x++)
//				attachments += fileNames.get(x) + "\n";
//			attachedImages = new JTextArea(attachments);
//			attachedImages.repaint();
		} else {
			comboBoxActions(event);
		}
		
	}
	
	/**
     * Checks the event against possible events for the various comboboxes in the GUI. When a match is found, it initializes the field associated with the combobox
     *
     * @param  event  Event that triggered this function.
     */
	private void comboBoxActions(ActionEvent event) {
		JComboBox curr = (JComboBox)event.getSource();
		String option = (String)curr.getSelectedItem();
		for(int x = 0; x<bugTypeOptions.length; x++){
			if (option.equals(bugTypeOptions[x])){
				bugTypeString = bugTypeOptions[x];
				if (bugTypeString.equals("Other (specify in description)"))
					bugTypeString = "Other";
				break;
			}
		}
		for(int x = 0; x<severityOptions.length; x++){
			if (option.equals(severityOptions[x])){
				severity = severityOptions[x];
				break;
			}
		}
		for(int x = 0; x<componentOptions.length; x++){
			if (option.equals(componentOptions[x])){
				component = componentOptions[x];
				break;
			}
		}
		for(int x = 0; x<versionOptions.length; x++){
			if (option.equals(versionOptions[x])){
				version = versionOptions[x];
				break;
			}
		}
		for(int x = 0; x<platformOptions.length; x++){
			if (option.equals(platformOptions[x])){
				platform = platformOptions[x];
				break;
			}
		}
		for(int x = 0; x<osOptions.length; x++){
			if (option.equals(osOptions[x])){
				os = osOptions[x];
				break;
			}
		}
		for(int x = 0; x<priorityOptions.length; x++){
			if (option.equals(priorityOptions[x])){
				priority = priorityOptions[x];
				break;
			}
		}
		for(int x = 0; x<initStateOptions.length; x++){
			if (option.equals(initStateOptions[x])){
				initState = initStateOptions[x];
				break;
			}
		}
		
		
	}
	
	/**
     * Pulls system specifications from the user's computer and then compiles all appropriate fields together into a text document
     *
     */
	private void compileReport(){
		String osArch = System.getProperties().getProperty("os.arch");	    
	    String osName = System.getProperties().getProperty("os.name");	    
	    String osVersion = System.getProperties().getProperty("os.version");	    
	    String javaVersion = System.getProperties().getProperty("java.version");	    
	    String javaVendor = System.getProperties().getProperty("java.vendor");	    
	    String javaVendorUrl = System.getProperties().getProperty("java.vendor.url");
	    String javaRuntimeName = System.getProperties().getProperty("java.runtime.name");
	    String javaRuntimeVersion = System.getProperties().getProperty("java.runtime.version");
	    String javaVmName = System.getProperties().getProperty("java.vm.name");
	    String javaVmVersion = System.getProperties().getProperty("java.vm.version");
	    String javaVmVendor = System.getProperties().getProperty("java.vm.vendor");
	    String javaInfo = System.getProperties().getProperty("java.vm.info");
	    String javaAwtGraphicsEnv = System.getProperties().getProperty("java.awt.graphicsenv");
	    String javaSpecName = System.getProperties().getProperty("java.specification.name");
	    String javaSpecVersion = System.getProperties().getProperty("java.specification.version");
	    String sunCpu = System.getProperties().getProperty("sun.cpu.endian");
	    String sunDesktop = System.getProperties().getProperty("sun.desktop");
	    String fileSeparator = System.getProperties().getProperty("file.separator");

		try {
			if (!bugReport.exists()) {
				File.createTempFile("bugReport", ".txt");
			}
			BufferedWriter report = new BufferedWriter(new FileWriter(bugReport.getName(), true));
			report.write("New Bug Report:");
			report.newLine();
			report.write(summary);
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
			report.newLine();
			report.write(description);
			report.newLine();
			report.newLine();
			report.write("System specifications:");
			report.newLine();
			report.write("os.arch = " + osArch);
			report.newLine();
			report.write("os.name = " + osName);
			report.newLine();
			report.write("os.version = " + osVersion);
			report.newLine();
			report.write("java.version = " + javaVersion);
			report.newLine();
			report.write("java.vendor = " + javaVendor);
			report.newLine();
			report.write("java.vendorUrl = " + javaVendorUrl);
			report.newLine();
			report.write("java.runtime.name = " + javaRuntimeName);
			report.newLine();
			report.write("java.runtime.version = " + javaRuntimeVersion);
			report.newLine();
			report.write("java.vm.name = " + javaVmName);
			report.newLine();
			report.write("java.vm.version = " + javaVmVersion);
			report.newLine();
			report.write("java.vm.vendor = " + javaVmVendor);
			report.newLine();
			report.write("java.vm.info = " + javaInfo);
			report.newLine();
			report.write("java.awt.graphicsenv = " + javaAwtGraphicsEnv);
			report.newLine();
			report.write("java.specifications.name = " + javaSpecName);
			report.newLine();
			report.write("java.specifications.version = " + javaSpecVersion);
			report.newLine();
			report.write("sun.cpu.endian = " + sunCpu);
			report.newLine();
			report.write("sun.desktop = " + sunDesktop);
			report.newLine();
			report.write("file.separator = " + fileSeparator);
			report.flush();
			report.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 	
	}

	/**
     * Grabs console error messages if applicable, sends them through the report lesson. the
     *
     */
	private void sendReport(){
		compileReport();
		try {
			if (!console.exists())
				File.createTempFile("console", ".txt");
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
			Address to = new InternetAddress("bug@mipav.cit.nih.gov");
			Address from = new InternetAddress("bug@mipav.cit.nih.gov", "MIPAV Bug Report");
			String host = "mailfwd.nih.gov";
			Properties properties = System.getProperties();
			properties.put("mail.host", host);
			Session session = Session.getDefaultInstance(properties);

			if (userType == 1){
				try{
					MimeMessage report = new MimeMessage(session);
					report.setFrom(from);
					
					report.addRecipient(Message.RecipientType.TO, to);
					
					report.setSubject("New " +bugTypeString + " Bug Report " + date);
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
	
	/**
     * Initializes GUI depending on which user type is selected. Contains two sections, one for information on the system and the user, 
     * the other containing fields for describing the bug itself.
     *
     * @param  user  Int number describing whether the user is a developer or a standard user.
     */
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
        
        
        JLabel summaryInstructions = new JLabel("Please give a brief (one or two sentence) summary of the bug you encountered");
    	summaryField.setLineWrap(true);
    	summaryField.setWrapStyleWord(true);
    	JScrollPane summaryScroll = new JScrollPane(summaryField);
    	summaryScroll.setPreferredSize(new Dimension(400, 20));
    	summaryInstructions.setFont(MipavUtil.font12);
        summaryInstructions.setForeground(Color.black);
        
//        AbstractDocument summaryLimit = (AbstractDocument)summaryField.getDocument();
//        summaryLimit.setDocumentFilter(new DocumentSizeFilter(250));
        
    	JLabel descInstructions = new JLabel("Please give a detailed description of the bug encountered");
    	descriptionField.setLineWrap(true);
    	descriptionField.setWrapStyleWord(true);
        JScrollPane descriptionScroll = new JScrollPane(descriptionField);
        descriptionScroll.setPreferredSize(new Dimension(400,100));
    	descInstructions.setFont(MipavUtil.font12);
    	descInstructions.setForeground(Color.black);
    	
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
    	descriptions.add(summaryScroll, gbc);
    	gbc.gridy = 5;
    	descriptions.add(descInstructions, gbc);
    	gbc.gridy = 6;
    	descriptions.add(descriptionScroll, gbc);
    	descriptions.setBorder(buildTitledBorder("Bug Description"));
    	
    	JLabel emailLabel = new JLabel("Your email address");
    	emailLabel.setFont(MipavUtil.font12);
    	emailLabel.setForeground(Color.black);
    	
    	JLabel nameLabel = new JLabel("Your name");
    	nameLabel.setFont(MipavUtil.font12);
    	nameLabel.setForeground(Color.black);
    	
    	JLabel branchLabel = new JLabel("Your branch");
    	branchLabel.setFont(MipavUtil.font12);
    	branchLabel.setForeground(Color.black);
    	
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
    	
    	JLabel platformLabel = new JLabel("Platform you are operating (ex. PC)");
    	platformLabel.setFont(MipavUtil.font12);
    	platformLabel.setForeground(Color.black);
    	
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
    	sidePanel.setBorder(buildTitledBorder("Information"));

    	
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
    	
    	JPanel imageCapture = new JPanel();
    	imageCapture.setLayout(new BorderLayout());
    	JButton imageAttachment = new JButton("Attach an Image");
    	imageAttachment.addActionListener(this);
    	imageCapture.add(imageAttachment, BorderLayout.WEST);
    	imageCapture.setBorder(buildTitledBorder("Images"));

    	attachedImages.setEditable(false);
    	attachedImages.setBackground(Color.LIGHT_GRAY);
    	attachedImages.setLineWrap(true);
    	attachedImages.setWrapStyleWord(true);
        JScrollPane attachedScroll = new JScrollPane(attachedImages);
        attachedScroll.setPreferredSize(new Dimension(400,100));
    	imageCapture.add(attachedScroll, BorderLayout.SOUTH);
    	
    	OKButton = new JButton("Submit");
        OKButton.addActionListener(this);
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);
    	buildCancelButton();
    	JPanel buttonPanel = new JPanel();

    	JPanel tempPanel = new JPanel();
    	tempPanel.add(OKButton);
        tempPanel.add(cancelButton);
        buttonPanel.setLayout(new BorderLayout());
    	buttonPanel.add(imageCapture, BorderLayout.NORTH);
    	buttonPanel.add(tempPanel, BorderLayout.SOUTH);
    	
    	JPanel mainPanel = new JPanel();
    	mainPanel.setLayout(new BorderLayout());
    	
    	if (userType == 1){
    		mainPanel.add(sidePanel, BorderLayout.WEST);
    		mainPanel.add(descriptions, BorderLayout.EAST);
    		mainPanel.add(buttonPanel, BorderLayout.SOUTH);
    	}
    	
    	frame = new JFrame("Report a Bug");
    	frame.getContentPane().add(mainPanel);
    	try {
			frame.setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		frame.pack();
		frame.setVisible(true);
	}
}
