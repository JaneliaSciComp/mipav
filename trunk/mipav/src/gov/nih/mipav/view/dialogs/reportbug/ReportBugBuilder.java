package gov.nih.mipav.view.dialogs.reportbug;

import java.awt.*;
import java.awt.event.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogCaptureScreen;

import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

import javax.swing.*;
import java.util.*;

import javax.imageio.ImageIO;
import javax.mail.*;
import javax.mail.internet.*;
import javax.activation.*;

public class ReportBugBuilder extends JDialogBase implements WindowListener{

	//~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = -7015450168814651213L;
	
	/** Options for the type of bug encountered */
	private static final String[] bugTypeOptions = new String[]{"Unexpected Output", "MIPAV Crash", "MIPAV Frozen", "Enhancement", "Other (specify in description)"};
    
    //~ Instance fields ------------------------------------------------------------------------------------------------
	
    /** String to contain the user inputed summary (character limit?) */
    private String summary;
    
    /** String to contain the full user inputed bug description*/
    private String description;
    
    /** String to contain the user inputed operating platform */
    private String platform;
    
    /** String to contain the user inputed operating system. */
    private String os;
    
    /** String to indicate the urgency with which the user needs the bug fixed */
    private String urgency; //u
        
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

	/** Version of MIPAV running */
	private String version;

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
     
    private String attachmentName;
    
    private ArrayList<String> fileNames = new ArrayList<String>();

	private JTextField directory;
	
	private JFileChooser browser;
	
	public static String capturedImageName;

	private ArrayList<String> filePaths = new ArrayList<String>();

	public JButton screenCap;

	private File image;
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Initializes the GUI and then deletes the created files once the program has finished running
     * 
     */
    public ReportBugBuilder(){
    	init();
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates a JFrame asking if the user is a developer and then runs the method displayReportForm, which
     * displays the appropriate form for the selected type.
     * 
     */
    private void init(){
    	displayReportForm();
    }
    
    public static void setCapturedImageName(String name){
    	capturedImageName = name;
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
		} else if (command.equals("Cancel")){
			dispose();
			System.gc();
			frame.setVisible(false);
		} else if (command.equals("Create New Image")){
			JDialogCaptureScreen screenCapture = new JDialogCaptureScreen(null, true);
		} else if (command.equals("Browse")) {
			browser = new JFileChooser();
			int returnVal = browser.showOpenDialog(ReportBugBuilder.this);
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				File attachedImage = browser.getSelectedFile();
				fileNames.add(attachedImage.getName());
				filePaths.add(attachedImage.getAbsolutePath());
				attachedImages.append(attachedImage.getName() + "\n");
			}
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
			File.createTempFile("bugReport", ".txt");
			BufferedWriter report = new BufferedWriter(new FileWriter("bugReport.txt", true));
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
			report.write("Urgency: " + urgency);
			report.newLine();
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
			Address to = new InternetAddress("shens2@mail.nih.gov");
			Address from = new InternetAddress("bug@mipav.cit.nih.gov", "MIPAV Bug Report");
			String host = "mailfwd.nih.gov";
			Properties properties = System.getProperties();
			properties.put("mail.host", host);
			Session session = Session.getDefaultInstance(properties);
			MimeMessage report = new MimeMessage(session);
			report.setFrom(from);
				
			report.addRecipient(Message.RecipientType.TO, to);
				
			report.setSubject("New " + bugTypeString + " Bug Report " + date);
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
					
			for (int x = 0; x < fileNames.size(); x++) {
				reportSummary = new MimeBodyPart();
				file = filePaths.get(x);
				source = new FileDataSource(file);
				reportSummary.setDataHandler(new DataHandler(source));
				reportSummary.setFileName(fileNames.get(x));
				parts.addBodyPart(reportSummary);
			}
					
			report.setContent(parts);
					
			Transport.send(report);
					
			JOptionPane.showMessageDialog(null, "Message sent successfully");
			frame.dispose();
		} catch (MessagingException e) {
				e.printStackTrace();
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
	private void displayReportForm(){
		GuiBuilder ref = new GuiBuilder(this);
		
		GridBagConstraints gbc = new GridBagConstraints();
    	gbc.gridwidth = 1;
    	gbc.gridheight = 1;
    	gbc.anchor = GridBagConstraints.WEST;
    	gbc.weightx = 1;

    	setTitle("Report a Bug");
    	
    	JComboBox bugType = ref.buildComboBox("Bug type", bugTypeOptions);
    	bugType.addActionListener(this);

        
        JLabel summaryInstructions = new JLabel("Please give a brief (one or two sentence) summary of the bug you encountered");
    	summaryField.setLineWrap(true);
    	summaryField.setWrapStyleWord(true);
    	JScrollPane summaryScroll = new JScrollPane(summaryField);
    	summaryScroll.setPreferredSize(new Dimension(400, 20));
    	summaryInstructions.setFont(MipavUtil.font12);
        summaryInstructions.setForeground(Color.black);
        
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

    	JPanel imageCapture = new JPanel();
    	imageCapture.setLayout(new BorderLayout());
    	JButton browse = new JButton("Browse");
    	browse.setMinimumSize(MipavUtil.defaultButtonSize);
        browse.setPreferredSize(MipavUtil.defaultButtonSize);
        browse.setFont(serif12B);
    	browse.addActionListener(this);
    	directory = new JTextField(20);
    	screenCap = new JButton("Create New Image");
        screenCap.setFont(serif12B);
    	screenCap.addActionListener(this);
    	
    	JPanel attachmentOptions = new JPanel();
    	
    	JPanel attachmentButtons = new JPanel();
    	attachmentButtons.setLayout(new BorderLayout());
    	attachmentButtons.add(directory, BorderLayout.WEST);
    	attachmentButtons.add(browse, BorderLayout.EAST);
    	
    	attachmentOptions.setLayout(new BorderLayout());
    	attachmentOptions.add(attachmentButtons, BorderLayout.WEST);
    	attachmentOptions.add(screenCap, BorderLayout.EAST);
    	imageCapture.add(attachmentOptions, BorderLayout.NORTH);
    	imageCapture.setBorder(buildTitledBorder("Attachments"));

    	attachedImages.setEditable(false);
    	attachedImages.setBackground(Color.LIGHT_GRAY);
    	attachedImages.setLineWrap(true);
    	attachedImages.setWrapStyleWord(true);
        JScrollPane attachedScroll = new JScrollPane(attachedImages);
        attachedScroll.setPreferredSize(new Dimension(400,100));
    	imageCapture.add(attachedScroll, BorderLayout.CENTER);
    	
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
    	
    	mainPanel.add(sidePanel, BorderLayout.WEST);
    	mainPanel.add(descriptions, BorderLayout.EAST);
    	mainPanel.add(buttonPanel, BorderLayout.SOUTH);
    	
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
	
	public void windowClosed(WindowEvent event){
		if(!JDialogCaptureScreen.cancel){
			attachmentName = JDialogCaptureScreen.fileName + ".png";
			try {
				if (JDialogCaptureScreen.fileName.length() < 3)
					MipavUtil.displayError("File name must be at least three characters long.");
				else if (JDialogCaptureScreen.currImage == null)
					MipavUtil.displayError("File name must be at least three characters long.");
				else {
					image = new File(Preferences.getPreferencesDir() + File.separatorChar + attachmentName);
					ImageIO.write(JDialogCaptureScreen.currImage, "png", image);
					fileNames.add(attachmentName);
					filePaths.add(Preferences.getPreferencesDir() + "\\" + attachmentName);
					attachedImages.append(fileNames.get(fileNames.size() - 1) + "\n");
					
					image.deleteOnExit();
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
	}

}
