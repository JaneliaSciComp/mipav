package gov.nih.mipav.view.srb;

import java.awt.Insets;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JComboBox;
import javax.swing.JPasswordField;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.SwingConstants;
import javax.swing.JPanel;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;

import edu.sdsc.grid.io.srb.SRBFile;
import edu.sdsc.grid.io.srb.SRBFileSystem;
import edu.sdsc.grid.io.srb.SRBAccount;

import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class JDialogLoginSRB extends JDialog implements ActionListener, KeyListener{
    private static final String auth_schemas[] = {"ENCRYPT1", "PASSWD_AUTH"};
    private final int COLUMN_COUNT = 30;
    
    // user name
    private JLabel nameLabel;
    private JTextField nameField;
    
    // host name of the srb server
    private JLabel hostLabel;
    private JTextField hostField;
    
    // domain name of the server
    private JLabel domainLabel;
    private JTextField domainField;
    
    // the port number that the srb server listens
    private JLabel portLabel;
    private JTextField portField;
    
    // the authentication schema that the srb server uses
    private JLabel authenticationLabel;
    private JComboBox authenticationComboBox;
    
    // the password that the user uses to login the server
    private JLabel passwordLabel;
    private JPasswordField passwordField;
    
    // The default storage resource that the user uses
    private JLabel storageResourceLabel;
    private JTextField storageResourceField;
    
    private JButton connectButton;
    private JButton cancelButton;
    public static SRBFileSystem srbFileSystem;
    
    public JDialogLoginSRB(String dialogTitle){
        super(ViewUserInterface.getReference().getMainFrame(), dialogTitle, true);
        init();
    }
    
    private void init(){
        PanelManager manager = new PanelManager();
        manager.getConstraints().insets = new Insets(5,5,5,5);
        
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
        bottomPanel.add(cancelButton);
        this.getContentPane().add(bottomPanel, BorderLayout.SOUTH);
        this.pack();
        
        /**
         * You have to bring these two statement before the setVisible(true),
         * otherwise doesn't work.
         */
        passwordField.requestFocus();
        MipavUtil.centerOnScreen(this);
        this.setVisible(true);
    }
    
    /**
     * Returns the SRB file system.
     */
    public SRBFileSystem getSRBFileSystem(){
        return srbFileSystem;
    }
    
    /**
     * Returns whether the srb file system is valid. 
     * @return whether the srb file system is valid.
     */
    public static boolean hasValidSRBFileSystem(){
        if(srbFileSystem == null || !srbFileSystem.isConnected()){
            return false;
        }
        String path = "/home";
        SRBFile root = new SRBFile(srbFileSystem, path);
        try{
            if(root.listFiles().length > 0){
                return true;
            }
        }catch(Exception e){
            return false;
        }
        return false;
    }
    
    /**
     * Action event listener.
     */
    public void actionPerformed(ActionEvent e){
        String command = e.getActionCommand();
        if(command.equals("Connect")){
            /**
             * First check every text field, make sure it has right input.
             */ 
            if(nameField.getText().length() == 0){
                nameField.requestFocus();
                return;
            }
            if(passwordField.getPassword().length == 0){
                passwordField.requestFocus();
                return;
            }
            if(hostField.getText().length() == 0){
                hostField.requestFocus();
                return;
            }
            if(domainField.getText().length() == 0){
                domainField.requestFocus();
                return;
            }
            if(portField.getText().length() == 0){
                portField.requestFocus();
                return;
            }else{
                try{
                    Integer.parseInt(portField.getText());
                }catch(NumberFormatException ex){
                    portField.setText("");
                    portField.requestFocus();
                }
            }
            if(hostField.getText().length() == 0){
                return;
            }
            
            if(storageResourceField.getText().length() == 0){
                storageResourceField.requestFocus();
                return;
            }
            /**
             * Retrieves all the information which is needed to build a SRBAccount,
             * then build the SRBAccount.
             */
            String name = nameField.getText();
            char[] password = passwordField.getPassword();
            String host = hostField.getText();
            String domain = domainField.getText();
            String auth = (String)authenticationComboBox.getSelectedItem();
            
            int port = -1;
            try{
                port = Integer.parseInt(portField.getText());
            }catch(NumberFormatException ex){
                // this can't happen.
            }
            
            String storageResource = storageResourceField.getText();
            SRBAccount srbAccount = new SRBAccount(host, port, name, new String(password), "", domain, storageResource);
            // srbAccount.setMcatZone("birnzone");
            
            if(auth.equals(auth_schemas[0])){
                srbAccount.setOptions(SRBAccount.ENCRYPT1);
            }else if(auth.equals(auth_schemas[1])){
                srbAccount.setOptions(SRBAccount.PASSWD_AUTH);
            }
            try{
                srbFileSystem = new SRBFileSystem(srbAccount);
                
            }catch(Exception ex){
                srbFileSystem = null;
                return;
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
        }else if(command.equals("Cancel")){
            this.dispose();
        }
    }
    
    public void keyPressed(KeyEvent e){
        
    }
    
    public void keyReleased(KeyEvent e){
        
    }
    
    public void keyTyped(KeyEvent e){
        int keyChar = e.getKeyChar();
        if(keyChar == KeyEvent.VK_ENTER){
            actionPerformed(new ActionEvent(this, 10, "Connect"));
        }else if(keyChar == KeyEvent.VK_ESCAPE){
            actionPerformed(new ActionEvent(this, 11, "Cancel"));
        }
    }
}
