package gov.nih.mipav.view.srb;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import edu.sdsc.grid.io.srb.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogLoginSRB extends JDialog implements ActionListener, KeyListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2797550808781412556L;

    /** the available authentication schema. */
    private static final String[] auth_schemas = { "ENCRYPT1", "PASSWD_AUTH" };

    /** DOCUMENT ME! */
    public static SRBFileSystem srbFileSystem;

    /**
     * The maximum number of srb login failures that are allowed during a mipav session. The user cannot log in after
     * this number is reached.
     */
    public static final int MAX_SRB_LOGIN_FAILURES = 10;

    /** Text of the NDAR privacy notice displayed to the user after a successful login to the srb. */
    public static final String NDAR_PRIVACY_NOTICE = "MIPAV is a collaborative environment with privacy rules that pertain to the collection\n" + 
                                                     "and display of imaging data. Before accessing and using MIPAV, please ensure that you\n" + 
                                                     "familiarize yourself with our privacy rules, available through the NDAR Rules of Behavior\n" +
                                                     "document and supporting documentation.\n" +
                                                     "\n" +
                                                     "Collection of this information is authorized under 42 U.S.C. 241, 242, 248, 281(a)(b)(1)(P)\n" +
                                                     "and 44 U.S.C. 3101. The primary use of this information is to facilitate medical research\n" +
                                                     "around autism and autism treatment. This information may be disclosed to researchers for\n" +
                                                     "research purposes, and to system administrators for evaluation and data normalization.\n" + 
                                                     "\n" +
                                                     "Rules governing submission of this information are based on the data sharing rules defined\n" +
                                                     "in the Notice of Grant Award (NOGA). If you do not have a grant defining data sharing\n" +
                                                     "requirements, data submission is voluntary.  Data entered into NDAR will be used solely for\n" +
                                                     "scientific and research purposes and is designed to further the understanding of autism and\n" +
                                                     "autism treatments.  Modification of NDAR information may be addressed by contacting your NDAR\n" +
                                                     "system administrator at ndarhelp@nih.gov. Significant system update information may be posted\n" + 
                                                     "on the NDAR site as required.";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox authenticationComboBox;

    /** the authentication schema that the srb server uses. */
    private JLabel authenticationLabel;

    /** DOCUMENT ME! */
    private JButton cancelButton, helpButton;

    /** DOCUMENT ME! */
    private final int COLUMN_COUNT = 30;

    /** DOCUMENT ME! */
    private JButton connectButton;

    /** DOCUMENT ME! */
    private JTextField domainField;

    /** domain name of the server. */
    private JLabel domainLabel;

    /** DOCUMENT ME! */
    private JTextField hostField;

    /** host name of the srb server. */
    private JLabel hostLabel;

    /** DOCUMENT ME! */
    private JTextField nameField;

    /** user name. */
    private JLabel nameLabel;

    /** DOCUMENT ME! */
    private JPasswordField passwordField;

    /** the password that the user uses to login the server. */
    private JLabel passwordLabel;

    /** DOCUMENT ME! */
    private JTextField portField;

    /** the port number that the srb server listens. */
    private JLabel portLabel;

    /** Keep track of the number of login failures to limit password guessing. */
    private int srbLoginFailureCount = 0;

    /** DOCUMENT ME! */
    private JTextField storageResourceField;

    /** The default storage resource that the user uses. */
    private JLabel storageResourceLabel;

    /** Whether to show the NDAR Privacy notice after successful connection*/
    private boolean showPrivacyNotice = true;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogLoginSRB object.
     *
     * @param  dialogTitle  DOCUMENT ME!
     */
    public JDialogLoginSRB(String dialogTitle) {
    	this(dialogTitle, true);
    }

    /**
     * Constructor with Privacy notice control
     * @param dialogTitle
     * @param showNotice
     */
    public JDialogLoginSRB(String dialogTitle, boolean showNotice) {
        super(ViewUserInterface.getReference().getMainFrame(), dialogTitle, true);
        this.showPrivacyNotice = showNotice;
        init();
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns whether the srb file system is valid.
     *
     * @return  whether the srb file system is valid.
     */
    public static boolean hasValidSRBFileSystem() {

        if ((srbFileSystem == null) || !srbFileSystem.isConnected()) {
            return false;
        }

        String path = "/home";
        SRBFile root = new SRBFile(srbFileSystem, path);

        try {

            if (root.listFiles().length > 0) {
                return true;
            }
        } catch (Exception e) {
            return false;
        }

        return false;
    }

    /**
     * Action event listener.
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("Connect")) {

            if (srbLoginFailureCount >= MAX_SRB_LOGIN_FAILURES) {
                MipavUtil.displayError("Your account has been locked out of MIPAV because of too many login attempts.\n" +
                                       "Please contact the system administrator to regain access.");

                return;
            }

            /**
             * First check every text field, make sure it has right input.
             */
            if (nameField.getText().length() == 0) {
                nameField.requestFocus();

                return;
            }

            if (passwordField.getPassword().length == 0) {
                passwordField.requestFocus();

                return;
            }

            if (hostField.getText().length() == 0) {
                hostField.requestFocus();

                return;
            }

            if (domainField.getText().length() == 0) {
                domainField.requestFocus();

                return;
            }

            if (portField.getText().length() == 0) {
                portField.requestFocus();

                return;
            } else {

                try {
                    Integer.parseInt(portField.getText());
                } catch (NumberFormatException ex) {
                    portField.setText("");
                    portField.requestFocus();
                }
            }

            if (hostField.getText().length() == 0) {
                return;
            }

            if (storageResourceField.getText().length() == 0) {
                storageResourceField.requestFocus();

                return;
            }

            /**
             * Retrieves all the information which is needed to build a SRBAccount, then build the SRBAccount.
             */
            String name = nameField.getText();
            char[] password = passwordField.getPassword();
            String host = hostField.getText();
            String domain = domainField.getText();
            String auth = (String) authenticationComboBox.getSelectedItem();

            int port = -1;

            try {
                port = Integer.parseInt(portField.getText());
            } catch (NumberFormatException ex) {
                // this can't happen.
            }

            String storageResource = storageResourceField.getText();
            SRBAccount srbAccount = new SRBAccount(host, port, name, new String(password), "", domain, storageResource);
            // srbAccount.setMcatZone("birnzone");

            if (Preferences.getSRBVersion() != null) {
                System.out.println(Preferences.getSRBVersion());
                SRBAccount.setVersion(Preferences.getSRBVersion());
            }

            if (auth.equals(auth_schemas[0])) {
                srbAccount.setOptions(SRBAccount.ENCRYPT1);
            } else if (auth.equals(auth_schemas[1])) {
                srbAccount.setOptions(SRBAccount.PASSWD_AUTH);
            }

            try {
                srbFileSystem = new SRBFileSystem(srbAccount);
                System.out.println(srbFileSystem.getVersion());
                srbLoginFailureCount = 0;

            } catch (Exception ex) {
                srbFileSystem = null;
                MipavUtil.displayError("Can't log you in the SRB server: " + host + ", please try again.");
                srbLoginFailureCount++;

                return;
            }

            // sucessful login, display ndar privacy notice
            if (showPrivacyNotice) {
            	MipavUtil.displayInfo(NDAR_PRIVACY_NOTICE);
            }
            /**
             * Save the SRBAccount information into the MIPAV's preferences.
             */
            Preferences.setUserNameSRB(name);
            Preferences.setServerHostSRB(host);
            Preferences.setServerPortSRB(port);
            Preferences.setServerDomainSRB(domain);
            Preferences.setServerAuthSRB(auth);
            Preferences.setStorageResourceSRB(storageResource);
            this.dispose();
        } else if (command.equals("Cancel")) {
            this.dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("20000");
        }
    }

    /**
     * Returns the SRB file system.
     *
     * @return  DOCUMENT ME!
     */
    public SRBFileSystem getSRBFileSystem() {
        return srbFileSystem;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyTyped(KeyEvent e) {
        int keyChar = e.getKeyChar();

        if (keyChar == KeyEvent.VK_ENTER) {
            actionPerformed(new ActionEvent(this, 10, "Connect"));
        } else if (keyChar == KeyEvent.VK_ESCAPE) {
            actionPerformed(new ActionEvent(this, 11, "Cancel"));
        }
    }

    /**
     * Cleans memory.
     */

    /**
     * DOCUMENT ME!
     */
    private void init() {
        PanelManager manager = new PanelManager();
        manager.getConstraints().insets = new Insets(5, 5, 5, 5);

        // Sets up the name label.
        nameLabel = WidgetFactory.buildLabel("Name");
        nameLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.add(nameLabel);

        // Sets up the name field.
        nameField = WidgetFactory.buildTextField(Preferences.getUserNameSRB());
        nameField.setColumns(COLUMN_COUNT);
        nameField.addKeyListener(this);
        manager.add(nameField);

        // Sets up the password label.
        passwordLabel = WidgetFactory.buildLabel("Password");
        passwordLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.addOnNextLine(passwordLabel);

        // Sets up the password field.
        passwordField = WidgetFactory.buildPasswordField();
        passwordField.setColumns(COLUMN_COUNT);
        passwordField.addKeyListener(this);
        manager.add(passwordField);

        // Sets up the host label.
        hostLabel = WidgetFactory.buildLabel("Host");
        hostLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.addOnNextLine(hostLabel);

        // Sets up the host field.
        hostField = WidgetFactory.buildTextField(Preferences.getServerHostSRB());
        hostField.setColumns(COLUMN_COUNT);
        hostField.addKeyListener(this);
        manager.add(hostField);

        // Sets up the domain label.
        domainLabel = WidgetFactory.buildLabel("Domain");
        domainLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.addOnNextLine(domainLabel);

        // Sets up the domain field.
        domainField = WidgetFactory.buildTextField(Preferences.getServerDomainSRB());
        domainField.setColumns(COLUMN_COUNT);
        domainField.addKeyListener(this);
        manager.add(domainField);

        // Sets up the port label.
        portLabel = WidgetFactory.buildLabel("Port");
        portLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.addOnNextLine(portLabel);

        // Sets up the port field.
        portField = WidgetFactory.buildTextField(Integer.toString(Preferences.getServerPortSRB()));
        portField.setColumns(COLUMN_COUNT);
        portField.addKeyListener(this);
        manager.add(portField);

        // Sets up the default storage resource field.
        storageResourceLabel = WidgetFactory.buildLabel("Storage Resource");
        storageResourceLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.addOnNextLine(storageResourceLabel);

        // Sets up the port field.
        storageResourceField = WidgetFactory.buildTextField(Preferences.getStorageResourceSRB());
        storageResourceField.setColumns(COLUMN_COUNT);
        storageResourceField.addKeyListener(this);
        manager.add(storageResourceField);

        // Sets up the authentication label.
        authenticationLabel = WidgetFactory.buildLabel("Authentication");
        authenticationLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        manager.addOnNextLine(authenticationLabel);

        // Sets up the authentication field.
        authenticationComboBox = new JComboBox(auth_schemas);
        authenticationComboBox.setSelectedItem(Preferences.getServerAuthSRB());
        authenticationComboBox.addKeyListener(this);
        manager.add(authenticationComboBox);

        this.getContentPane().setLayout(new BorderLayout());
        this.getContentPane().add(manager.getPanel(), BorderLayout.CENTER);

        JPanel bottomPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        connectButton = WidgetFactory.buildTextButton("Connect", "Connect to the SRB server", "Connect", this);
        connectButton.setPreferredSize(new Dimension(90, 30));
        connectButton.addKeyListener(this);
        bottomPanel.add(connectButton);

        cancelButton = WidgetFactory.buildTextButton("Cancel", "Cancel connecting to the SRB server", "Cancel", this);
        cancelButton.setPreferredSize(new Dimension(90, 30));

        //helpButton = new JButton("Help");
        //helpButton.addActionListener(this);
        //helpButton.setPreferredSize(new Dimension(90, 30));
        bottomPanel.add(cancelButton);
        //bottomPanel.add(helpButton);
        
        this.getContentPane().add(bottomPanel, BorderLayout.SOUTH);
        this.pack();

        /**
         * You have to bring these two statement before the setVisible(true), otherwise doesn't work.
         */
        passwordField.requestFocus();
        MipavUtil.centerOnScreen(this);
        this.setVisible(true);
    }
}
