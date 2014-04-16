package gov.nih.mipav.view.dialogs.reportbug;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogCaptureScreen;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.*;
import java.net.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;

import javax.imageio.ImageIO;
import javax.swing.*;

import org.apache.commons.codec.binary.Base64;


public class ReportBugBuilder extends JDialogBase implements WindowListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7015450168814651213L;

    public enum BugType {
        UNEXPECTED_OUTPUT("Unexpected Output"), MIPAV_CRASH("MIPAV Crash"), MIPAV_FROZEN("MIPAV Frozen"), ENHANCEMENT("Enhancement"), OTHER(
                "Other (specify in description)"), AUTOMATIC_ERROR_REPORTING("Automatic error/exception reporting");

        private final String description;

        BugType(final String bugDescription) {
            description = bugDescription;
        }

        static BugType getBugFromDescription(final String bugDescription) {
            for (final BugType bug : values()) {
                if (bug.description.equalsIgnoreCase(bugDescription)) {
                    return bug;
                }
            }

            return null;
        }
    }

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Frame for the GUI in which the form is presented */
    private JFrame frame;

    /** Text are for displaying the attached images */
    private final JTextArea attachedImages = new JTextArea();

    /** Text pane for user inputed bug description */
    private final JTextPane descriptionField = new JTextPane();

    /** Text area for user inputed bug title */
    private final JTextField summaryField = new JTextField(30);

    /** Text field for user inputed name */
    private final JTextField nameField = new JTextField(25);

    /** Text field for user inputed email */
    private final JTextField emailField = new JTextField(25);

    /** Text field for the user inputed operating system. */
    private final JTextField standardOS = new JTextField(25);

    /** Text field for the user inputed mipav version. */
    private final JTextField standardVersion = new JTextField(25);

    /** Text field for the user inputed urgency of the bug */
    private final JTextField standardUrgency = new JTextField(25);

    private JComboBox bugTypeComboBox;

    /** Name of the image being attached. */
    private String attachmentName;

    /** ArrayList of all file names of report attachments */
    private final ArrayList<String> fileNames = new ArrayList<String>();

    /** Text field for the directory of a file being attached from the user's computer */
    private JTextField directory;

    /** File chooser for the user to select an existing file for attachment */
    private JFileChooser browser;

    /** ArrayList of all file paths of report attachments */
    private final ArrayList<String> filePaths = new ArrayList<String>();

    /** Button to launch the create new image dialog */
    public JButton screenCap;

    /** Image file used to store images created through the screen capture function */
    private File image;

    /** Dialog used for taking screen captures */
    private JDialogCaptureScreen screenCapture;

    /** Hack-ish boolean to avoid double adding screen captures to the attachment list. */
    private boolean isScreenCapAttached;

    /** Icon used to insert images into the bug description */
    // private Icon icon;

    /** URL of the page we use to actually send the bug report email. */
    private static final String BUG_MAIL_URL = "http://mipav.cit.nih.gov/report_bug.php";

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Initializes the GUI and then deletes the created files once the program has finished running
     * 
     */
    public ReportBugBuilder() {
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Creates a JFrame asking if the user is a developer and then runs the method displayReportForm, which displays the
     * appropriate form for the selected type.
     * 
     */
    private void init() {
        loadFieldDefaults();
        displayReportForm();
    }

    /**
     * Performs the following actions based on the command:<br>
     * 
     * <ul>
     * <li>OK - Initializes pertinent fields, checking that all fields are filled out correctly, before sending the
     * information to be processed for an email</li>
     * <li>Cancel - cleans up and disposes the dialog</li>
     * <li>Other - sends event to secondary method that looks for combobox events</li>
     * </ul>
     * 
     * @param event Event that triggered this function.
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("Submit")) {
            final String description = descriptionField.getText();
            final String summary = summaryField.getText();
            final String name = nameField.getText();
            final String email = emailField.getText();
            final String version = standardVersion.getText();
            final String os = standardOS.getText();
            final String urgency = standardUrgency.getText();
            final BugType bugType = BugType.getBugFromDescription((String) bugTypeComboBox.getSelectedItem());
            final String tempEmail = emailField.getText();
            if ( !tempEmail.contains("@") || !tempEmail.contains(".") || tempEmail.charAt(tempEmail.length() - 1) == '.' || tempEmail.charAt(0) == '@'
                    || tempEmail.contains("@.")) {
                JOptionPane.showMessageDialog(null, "This is not a valid email address.", "Error", JOptionPane.ERROR_MESSAGE);
            } else if (name.length() == 0 || email.length() == 0 || version.length() == 0 || os.length() == 0 || urgency.length() == 0
                    || description.length() == 0 || summary.length() == 0) {
                JOptionPane.showMessageDialog(null, "You must fill out all sections on this form.", "Error", JOptionPane.ERROR_MESSAGE);
            } else {
                saveFieldDefaults();
                sendReportWeb(summary, name, email, version, os, urgency, description, bugType, filePaths, fileNames);
                frame.setVisible(false);
            }
        } else if (command.equals("Cancel")) {
            dispose();
            System.gc();
            frame.setVisible(false);
        } else if (command.equals("Create New Image")) {
            // we haven't added this new screen capture yet
            isScreenCapAttached = false;

            screenCapture = new JDialogCaptureScreen(null, true);
            screenCapture.addWindowListener(this);
        } else if (command.equals("Browse")) {
            browser = new JFileChooser();
            final int returnVal = browser.showOpenDialog(ReportBugBuilder.this);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                final File attachedImage = browser.getSelectedFile();
                fileNames.add(attachedImage.getName());
                filePaths.add(attachedImage.getAbsolutePath());
                attachedImages.append(attachedImage.getName() + "\n");
            }
        } else {
            super.actionPerformed(event);
        }

    }

    /**
     * Pulls system specifications from the user's computer and then compiles all appropriate fields together into a
     * text document
     * 
     */
    public static final String compileReport(final String summary, final String name, final String email, final String version, final String os,
            final String urgency, final String description) {
        String reportStr = new String();

        final String osArch = System.getProperties().getProperty("os.arch");
        final String osName = System.getProperties().getProperty("os.name");
        final String osVersion = System.getProperties().getProperty("os.version");
        final String javaVersion = System.getProperties().getProperty("java.version");
        final String javaVendor = System.getProperties().getProperty("java.vendor");
        final String javaVendorUrl = System.getProperties().getProperty("java.vendor.url");
        final String javaRuntimeName = System.getProperties().getProperty("java.runtime.name");
        final String javaRuntimeVersion = System.getProperties().getProperty("java.runtime.version");
        final String javaVmName = System.getProperties().getProperty("java.vm.name");
        final String javaVmVersion = System.getProperties().getProperty("java.vm.version");
        final String javaVmVendor = System.getProperties().getProperty("java.vm.vendor");
        final String javaInfo = System.getProperties().getProperty("java.vm.info");
        final String javaAwtGraphicsEnv = System.getProperties().getProperty("java.awt.graphicsenv");
        final String javaSpecName = System.getProperties().getProperty("java.specification.name");
        final String javaSpecVersion = System.getProperties().getProperty("java.specification.version");
        final String sunCpu = System.getProperties().getProperty("sun.cpu.endian");
        final String sunDesktop = System.getProperties().getProperty("sun.desktop");
        final String fileSeparator = System.getProperties().getProperty("file.separator");

        final String userName = System.getProperties().getProperty("user.name");

        final Date dateHolder = new Date();
        final DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
        final String date = dateFormat.format(dateHolder);

        reportStr += "New Bug Report:";
        reportStr += "\n";
        reportStr += summary;
        reportStr += "\n";
        reportStr += "\n";
        reportStr += "Date: " + date.toString();
        reportStr += "\n";
        reportStr += "Name: " + name;
        reportStr += "\n";
        reportStr += "Email: " + email;
        reportStr += "\n";
        reportStr += "Product: MIPAV";
        reportStr += "\n";
        reportStr += "Version: " + version;
        reportStr += "\n";
        reportStr += "OS: " + os;
        reportStr += "\n";
        reportStr += "Urgency: " + urgency;
        reportStr += "\n";
        reportStr += "\n";
        reportStr += "Detailed description:";
        reportStr += "\n";
        reportStr += description;
        reportStr += "\n";
        reportStr += "\n";
        reportStr += "System specifications:";
        reportStr += "\n";
        reportStr += "os.arch = " + osArch;
        reportStr += "\n";
        reportStr += "os.name = " + osName;
        reportStr += "\n";
        reportStr += "os.version = " + osVersion;
        reportStr += "\n";
        reportStr += "java.version = " + javaVersion;
        reportStr += "\n";
        reportStr += "java.vendor = " + javaVendor;
        reportStr += "\n";
        reportStr += "java.vendorUrl = " + javaVendorUrl;
        reportStr += "\n";
        reportStr += "java.runtime.name = " + javaRuntimeName;
        reportStr += "\n";
        reportStr += "java.runtime.version = " + javaRuntimeVersion;
        reportStr += "\n";
        reportStr += "java.vm.name = " + javaVmName;
        reportStr += "\n";
        reportStr += "java.vm.version = " + javaVmVersion;
        reportStr += "\n";
        reportStr += "java.vm.vendor = " + javaVmVendor;
        reportStr += "\n";
        reportStr += "java.vm.info = " + javaInfo;
        reportStr += "\n";
        reportStr += "java.awt.graphicsenv = " + javaAwtGraphicsEnv;
        reportStr += "\n";
        reportStr += "java.specifications.name = " + javaSpecName;
        reportStr += "\n";
        reportStr += "java.specifications.version = " + javaSpecVersion;
        reportStr += "\n";
        reportStr += "sun.cpu.endian = " + sunCpu;
        reportStr += "\n";
        reportStr += "sun.desktop = " + sunDesktop;
        reportStr += "\n";
        reportStr += "file.separator = " + fileSeparator;
        reportStr += "\n";
        reportStr += "user.name = " + userName;
        reportStr += "\n";
        reportStr += "\n";
        reportStr += "ip addresses =\t";
        try {
            final Enumeration e = NetworkInterface.getNetworkInterfaces();
            while (e.hasMoreElements()) {
                final NetworkInterface n = (NetworkInterface) e.nextElement();
                final Enumeration ee = n.getInetAddresses();
                while (ee.hasMoreElements()) {
                    final InetAddress i = (InetAddress) ee.nextElement();
                    if ( !i.isLoopbackAddress() && !i.isSiteLocalAddress() && !i.isAnyLocalAddress()) {
                        reportStr += i.getHostName() + " - " + i.getHostAddress() + "\n\t\t";
                    }
                }
            }
        } catch (final SocketException e) {
            // do nothing
        }
        reportStr += "\n";
        int i = 0;
        final Enumeration<String> images = ViewUserInterface.getReference().getRegisteredImageNames();
        while (images.hasMoreElements()) {
            final String imgName = images.nextElement();
            reportStr += "open image [" + i + "] = " + imgName;
            reportStr += "\n";
            i++;
        }

        return reportStr;
    }

    public static final void sendReportWeb(final String summary, final String name, final String email, final String version, final String os,
            final String urgency, final String description, final BugType bugType, final ArrayList<String> filePaths, final ArrayList<String> fileNames) {
        String urlVarStr = new String();
        try {
            urlVarStr += URLEncoder.encode("bug_email", "UTF-8") + "=" + URLEncoder.encode(email, "UTF-8");

            urlVarStr += "&";
            urlVarStr += URLEncoder.encode("bug_type", "UTF-8") + "=" + URLEncoder.encode(bugType.description, "UTF-8");

            urlVarStr += "&";
            urlVarStr += URLEncoder.encode("bug_summary", "UTF-8") + "=" + URLEncoder.encode(summary, "UTF-8");

            urlVarStr += "&";
            urlVarStr += URLEncoder.encode("bug_desc", "UTF-8") + "=" + URLEncoder.encode(description, "UTF-8");

            final String reportText = compileReport(summary, name, email, version, os, urgency, description);

            urlVarStr += "&";
            urlVarStr += URLEncoder.encode("bug_report_text", "UTF-8") + "=" + URLEncoder.encode(reportText, "UTF-8");

            File file = new File(Preferences.getProperty(Preferences.PREF_LOG_FILENAME));
            if (file.exists()) {
                try {
                    final byte[] fileContents = new byte[(int) file.length()];
                    final DataInputStream dis = new DataInputStream(new FileInputStream(file));
                    dis.readFully(fileContents);
                    dis.close();

                    urlVarStr += "&";
                    urlVarStr += URLEncoder.encode("bug_log_file", "UTF-8") + "=" + Base64.encodeBase64URLSafeString(fileContents);
                } catch (final IOException e) {
                    Preferences.debug("Unable to read and attach bug report file: " + file.getAbsolutePath(), Preferences.DEBUG_FILEIO);
                    e.printStackTrace();
                }
            }

            for (int x = 0; x < fileNames.size(); x++) {
                file = new File(filePaths.get(x));
                if (file.exists()) {
                    try {
                        final byte[] fileContents = new byte[(int) file.length()];
                        final DataInputStream dis = new DataInputStream(new FileInputStream(file));
                        dis.readFully(fileContents);
                        dis.close();

                        urlVarStr += "&";
                        urlVarStr += URLEncoder.encode("bug_other_file_name_" + (x + 1), "UTF-8") + "=" + URLEncoder.encode(fileNames.get(x), "UTF-8");
                        urlVarStr += "&";
                        // System.err.println("File name:\t" + fileNames.get(x));
                        // System.err.println("File base64:\t" + Base64.encodeBase64URLSafeString(fileContents));
                        urlVarStr += URLEncoder.encode("bug_other_file_contents_" + (x + 1), "UTF-8") + "=" + Base64.encodeBase64URLSafeString(fileContents);
                    } catch (final IOException e) {
                        Preferences.debug("Unable to read and attach bug report file: " + file.getAbsolutePath(), Preferences.DEBUG_FILEIO);
                        e.printStackTrace();
                    }
                }
            }
        } catch (final UnsupportedEncodingException e) {
            e.printStackTrace();
            MipavUtil.displayError("Unable to encode bug report data");
        }

        // use vars passed to a web page to send the bug email
        try {
            // URI mailURI = new URI(BUG_MAIL_URL + "?" + urlVarStr);
            // Desktop.getDesktop().browse(mailURI);

            final byte[] varBytes = urlVarStr.getBytes("UTF-8");
            final URL url = new URL(BUG_MAIL_URL);

            final HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setDoInput(true);
            connection.setDoOutput(true);
            connection.setInstanceFollowRedirects(true);
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
            connection.setRequestProperty("charset", "UTF-8");
            connection.setRequestProperty("Content-Length", Integer.toString(varBytes.length));
            connection.setRequestProperty("Content-Language", "en-US");
            connection.setUseCaches(false);

            connection.connect();
            final DataOutputStream out = new DataOutputStream(connection.getOutputStream());
            out.write(varBytes);
            out.flush();
            out.close();

            // debugging info back from the web server.
            final BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            // System.out.println("Resp code: " + connection.getResponseCode());
            // System.out.println("Resp msg: " + connection.getResponseMessage());
            String inputLine;
            final String successLine = "Bug report successfully sent to bug@mipav.cit.nih.gov.  Thank you!";
            final String failureLine = "Bug report not successfully sent! Please contact bug@mipav.cit.nih.gov from your usual email client.";
            while ( (inputLine = in.readLine()) != null) {
                // System.out.print(inputLine + "\n");
                if (inputLine.equalsIgnoreCase(successLine)) {
                    // right now, don't pop-up a message dialog when auto-sending an exception email
                    if (bugType != BugType.AUTOMATIC_ERROR_REPORTING) {
                        MipavUtil.displayInfo(inputLine);
                    }
                } else if (inputLine.equalsIgnoreCase(failureLine)) {
                    // right now, don't pop-up a message dialog when auto-sending an exception email
                    if (bugType != BugType.AUTOMATIC_ERROR_REPORTING) {
                        MipavUtil.displayError(inputLine);
                    }
                }
            }
            in.close();
            connection.disconnect();
        } catch (final MalformedURLException e) {
            e.printStackTrace();
            MipavUtil.displayError("Unable to open bug report mailer URL: " + BUG_MAIL_URL);
            return;
            /*
             * } catch (URISyntaxException e) { e.printStackTrace();
             * MipavUtil.displayError("Unable to open bug report mailer URL: " + BUG_MAIL_URL); return;
             */
        } catch (final IOException e) {
            e.printStackTrace();
            MipavUtil.displayError("Unable to open bug report mailer URL: " + BUG_MAIL_URL);
            return;
        }
    }

    /**
     * Initializes GUI. Contains two sections, one for information on the system and the user, the other containing
     * fields for describing the bug itself.
     * 
     */
    private void displayReportForm() {
        final GuiBuilder ref = new GuiBuilder(this);

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        setTitle("Report a Bug");

        final BugType[] bugTypes = BugType.values();
        final String[] bugTypeStrList = new String[bugTypes.length];
        for (int i = 0; i < bugTypes.length; i++) {
            bugTypeStrList[i] = bugTypes[i].description;
        }
        bugTypeComboBox = ref.buildComboBox("Bug type", bugTypeStrList);

        final JLabel summaryInstructions = new JLabel("Title");
        summaryInstructions.setFont(MipavUtil.font12);
        summaryInstructions.setForeground(Color.black);

        final JLabel descInstructions = new JLabel("Please give a detailed description of the bug encountered");
        final JScrollPane descriptionScroll = new JScrollPane(descriptionField);
        descriptionScroll.setPreferredSize(new Dimension(400, 100));
        descInstructions.setFont(MipavUtil.font12);
        descInstructions.setForeground(Color.black);

        final JPanel descriptions = new JPanel();
        descriptions.setLayout(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weighty = 1;
        descriptions.add(bugTypeComboBox);
        gbc.gridy = 3;
        gbc.fill = GridBagConstraints.VERTICAL;
        descriptions.add(summaryInstructions, gbc);
        gbc.gridy = 4;
        descriptions.add(summaryField, gbc);
        gbc.gridy = 5;
        descriptions.add(descInstructions, gbc);
        gbc.gridy = 6;
        descriptions.add(descriptionScroll, gbc);
        descriptions.setBorder(buildTitledBorder("Bug Description"));

        final JLabel emailLabel = new JLabel("Your email address");
        emailLabel.setFont(MipavUtil.font12);
        emailLabel.setForeground(Color.black);

        final JLabel nameLabel = new JLabel("Your name");
        nameLabel.setFont(MipavUtil.font12);
        nameLabel.setForeground(Color.black);

        final JPanel personalInfo = new JPanel();
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

        final JLabel versionLabel = new JLabel("Version of MIPAV you are running");
        versionLabel.setFont(MipavUtil.font12);
        versionLabel.setForeground(Color.black);

        final JLabel osLabel = new JLabel("Operating System you are using");
        osLabel.setFont(MipavUtil.font12);
        osLabel.setForeground(Color.black);

        final JLabel urgencyLabel = new JLabel("How urgent is this bug? When do you need it fixed by?");
        urgencyLabel.setFont(MipavUtil.font12);
        urgencyLabel.setForeground(Color.black);

        final JPanel standardUserInput = new JPanel();
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
        standardUserInput.add(osLabel, gbc);
        gbc.gridy = 3;
        standardUserInput.add(standardOS, gbc);
        gbc.gridy = 4;
        standardUserInput.add(urgencyLabel, gbc);
        gbc.gridy = 5;
        standardUserInput.add(standardUrgency, gbc);

        final JPanel sidePanel = new JPanel();
        sidePanel.setLayout(new BorderLayout());
        sidePanel.add(personalInfo, BorderLayout.NORTH);
        sidePanel.add(standardUserInput, BorderLayout.CENTER);
        sidePanel.setBorder(buildTitledBorder("Information"));

        final JPanel imageCapture = new JPanel();
        imageCapture.setLayout(new BorderLayout());
        final JButton browse = new JButton("Browse");
        browse.setMinimumSize(MipavUtil.defaultButtonSize);
        browse.setPreferredSize(MipavUtil.defaultButtonSize);
        browse.setFont(serif12B);
        browse.addActionListener(this);
        directory = new JTextField(20);
        screenCap = new JButton("Create New Image");
        screenCap.setFont(serif12B);
        screenCap.addActionListener(this);

        final JPanel attachmentOptions = new JPanel();

        final JPanel attachmentButtons = new JPanel();
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
        final JScrollPane attachedScroll = new JScrollPane(attachedImages);
        attachedScroll.setPreferredSize(new Dimension(400, 100));
        imageCapture.add(attachedScroll, BorderLayout.CENTER);

        OKButton = new JButton("Submit");
        OKButton.addActionListener(this);
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);
        buildCancelButton();
        final JPanel buttonPanel = new JPanel();

        final JPanel tempPanel = new JPanel();
        tempPanel.add(OKButton);
        tempPanel.add(cancelButton);
        buttonPanel.setLayout(new BorderLayout());
        buttonPanel.add(imageCapture, BorderLayout.NORTH);
        buttonPanel.add(tempPanel, BorderLayout.SOUTH);

        final JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());

        mainPanel.add(sidePanel, BorderLayout.WEST);
        mainPanel.add(descriptions, BorderLayout.EAST);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        frame = new JFrame("Report a Bug");
        frame.getContentPane().add(mainPanel);
        try {
            frame.setIconImage(MipavUtil.getIconImage("divinci.gif"));
        } catch (final FileNotFoundException e) {
            e.printStackTrace();
        }
        frame.pack();
        frame.setVisible(true);
    }

    /**
     * Performs the following actions based on the command:<br>
     * 
     * <ul>
     * <li>OK - creates a file using the currently selected image and the given file name and then attaches it to email</li>
     * <li>Insert - inserts the image into the description area and if a name is provided, attaches the image as a file
     * as well</li>
     * </ul>
     * 
     * @param event Event that triggered this function.
     */
    @Override
    public void windowClosed(final WindowEvent event) {
        // boolean hack to get around double-calls to this method on closing.
        if (event.getSource().equals(screenCapture) && !isScreenCapAttached) {
            if (screenCapture.insert) {
                try {
                    if (JDialogCaptureScreen.currImage == null) {
                        MipavUtil.displayError("You must select a region to insert.");
                    } else {
                        // stop inserting images into description (they don't save in a plain-text message anyway).
                        // icon = new ImageIcon(JDialogCaptureScreen.currImage);
                        // descriptionField.insertIcon(icon);

                        // avoid collisions by putting a timestamp into the image name
                        final long ts = System.currentTimeMillis();

                        attachmentName = screenCapture.fileName + "_" + ts + ".png";

                        // attach the screen capture with an auto-generated name if one wasn't specified
                        if (screenCapture.fileName.length() == 0) {
                            attachmentName = "screenCapture_" + ts + ".png";
                        }

                        final String screenCapFilePath = Preferences.getPreferencesDir() + File.separator + attachmentName;
                        image = new File(screenCapFilePath);
                        ImageIO.write(JDialogCaptureScreen.currImage, "png", image);
                        fileNames.add(attachmentName);
                        filePaths.add(screenCapFilePath);
                        attachedImages.append(fileNames.get(fileNames.size() - 1) + "\n");
                        image.deleteOnExit();

                        isScreenCapAttached = true;
                    }
                } catch (final IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Tries to set the bug report field defaults based on system properties or last-entered data.
     */
    private void loadFieldDefaults() {
        String str = getNameDefault();
        if (str != null) {
            nameField.setText(str);
        }
        str = getEmailDefault();
        if (str != null) {
            emailField.setText(str);
        }
        standardOS.setText(System.getProperties().getProperty("os.name") + " - " + System.getProperty("os.arch"));
        standardVersion.setText(MipavUtil.getVersion());
        str = getUrgencyDefault();
        if (str != null) {
            standardUrgency.setText(str);
        }
    }

    public static final String getNameDefault() {
        return Preferences.getProperty(Preferences.PREF_BUG_REPORT_NAME);
    }

    public static final String getEmailDefault() {
        return Preferences.getProperty(Preferences.PREF_BUG_REPORT_EMAIL);
    }

    public static final String getUrgencyDefault() {
        return Preferences.getProperty(Preferences.PREF_BUG_REPORT_URGENCY);
    }

    /**
     * Saves the last-entered bug report field text for loading the next time the dialog is used.
     */
    private void saveFieldDefaults() {
        Preferences.setProperty(Preferences.PREF_BUG_REPORT_NAME, nameField.getText());
        Preferences.setProperty(Preferences.PREF_BUG_REPORT_EMAIL, emailField.getText());
        Preferences.setProperty(Preferences.PREF_BUG_REPORT_URGENCY, standardUrgency.getText());
    }
}
