package gov.nih.mipav.view;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import javax.swing.border.*;
import javax.swing.table.*;
import javax.swing.tree.*;
import java.net.*;
import java.security.*;
import java.security.cert.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.MipavUtil;
import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.math.*;
import java.text.*;

//bouncy castle imports
import org.bouncycastle.jce.*;
import org.bouncycastle.asn1.x509.*;
import org.bouncycastle.asn1.*;
import org.bouncycastle.jce.provider.*;

public class JChatFrame
    extends JFrame
    implements ActionListener,
    ListSelectionListener,
    ComponentListener,
    ItemListener,
    TreeSelectionListener,
    TreeExpansionListener,
    ChangeListener,
    MouseListener,
    PreviewImageContainer {

  private JTextArea textArea; // text area where server output is displayed
  private JTextField inputText; // text field for chat input
  private JButton sendText; // send text in textfield to server
  private JMenu connect; // connect menu
  private JMenuBar menuBar; // for chat and server
  private JMenuBar menuBar2; // for file browser
  private JMenuItem disconnect; // disconnect from server
  private JMenuItem reconnect; // reconnect to last server
  private JMenuItem changeTrust; // change the keystore/truststore
  private JMenuItem changeReceiveDir; // change the default receive directory
  private JMenuItem listUsers; // list all users connected
  private JMenuItem changeUsername; // change username
  private JMenuItem hostServer; // start the chat server
  private JMenuItem changeServerPort; // change the chat server port
  private JMenuItem changeXferPort; // change the file transfer port
  private JMenuItem stopServer; // stop the chat server
  private JMenuItem close; // close the window
  private JTabbedPane tabbedPane; // tabbed pane to switch between main/hosts/file transfer
  private JButton create; // create a new host entry
  private JButton edit; // edit a host entry
  private JButton delete; // delete a host entry
  private JButton set; // set the host entry as default
  private String username; // user's unique ID
  private String userIP; // users IP
  private String defaultAddress; // default address for server
  private int defaultPort; //default port for client
  private String currentAddress; // current address for server
  private int currentPort; // current port for client
  private int currentServerPort; // current server port
  private int xferPort; //port for file sending/receiving
  private Hashtable hostsTable;  // hashtable that holds the jmenuitem for each host
  private ViewTableModel serverModel; // model associated with serverTable
  private JTable serverTable; // table to hold list of host entries
  private ChatClient client; // chat client to connect to server: one per window
  private JPanel fileBrowser; // File chooser/sending panel

  /** For the File Browser/sender */
  protected ViewUserInterface userInterface;
  protected String directory; // initial directory for tree listing
  protected JTree directoryTree; // directory tree for file listings
  protected ViewJComponentPreviewImage img; // image preview
  protected FileImageXML.Thumbnail thumbnail;
  protected ViewImageFileFilter imageFilter;  // filter for image files
  protected File file; //currently selected file
  protected ViewFileTreeNode node; // current node in directory tree
  protected JPanel imagePanel; // panel to hold image preview
  protected JPanel treePanel; // panel to hold directory tree
  protected JPanel toolPanel; // panel to hold toolbar

  private JSlider brightSlider, contSlider;
  protected Font serif12, serif12B;
  private int origBrightness = 0;
  private float origContrast = 1;
  private int brightness = 0;
  private float contrast = 1;
  private JLabel current, current2;
  private NumberFormat nfc;
  private JPanel brightPanel;
  private JSplitPane imageSliderPane;

  private JTable primaryTable;  // primary table for image info
  private JTable secondaryTable; // secondary table for additional image info
  private ViewTableModel primaryModel; // model for primary table
  private ViewTableModel secondaryModel; // model for secondary table
  private JLabel otherLabel;  // "other information" label

  private Dimension defaultImageSize;  // default image size
  private boolean smaller;
  private boolean shift;

  private boolean showPreview = false; // show or hide image preview
  private JCheckBoxMenuItem itemPreview; // checkbox menu for showing/hiding image preview
  private JCheckBoxMenuItem thumbnailOption; // checkbox menu for showing XML Thumbnails
  private boolean showXMLThumbnail = true;

  private String keyStore;  // location of keystore
  private String keyPasswd; // password for keystore
  private String defaultReceiveDirectory; // default receive directory
  private Vector userList;  // list of users currently connected

  private JTextField receiveField; // textfield to display the default receive directory
  private ViewJProgressBar progressBar; // progressbar to show sending/receiving progress
  private KeyStore ks; // keystore & truststore to hold private keys/certificates
  private ChatServer server = null; // the chat server (if user chooses to host)

  public JChatFrame(ViewUserInterface UI) throws HeadlessException {
      //cleanup if window is closed
      this.addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent we) {
            if (img != null) {
                img.dispose(true);
                img = null;
            }
            else if (thumbnail != null) {
                thumbnail.finalize();
                thumbnail = null;
            }
            setOnline(false);
        }
    });

    userInterface = UI;
    currentServerPort = 9030;  // default the port to 9030..but can change
    xferPort = 9130;

    serif12 = MipavUtil.font12;
    serif12B = MipavUtil.font12B;

    this.showXMLThumbnail = Preferences.is(Preferences.PREF_SAVE_XML_THUMBNAIL);
    directory = userInterface.getDefaultDirectory();
    userList = new Vector();

    initUsername();
    initStores();
    initReceiveDir();

    hostsTable = new Hashtable();
    setTitle("MIPAV - Secure File Transfer & Chat");
    setSize(780, 630);

    textArea = new JTextArea(22, 65);
    textArea.setWrapStyleWord(true);

    textArea.setEditable(false);

    JScrollPane scrollPane = new JScrollPane(textArea);
    scrollPane.setVerticalScrollBarPolicy(JScrollPane.
                                          VERTICAL_SCROLLBAR_AS_NEEDED);
    scrollPane.setBorder(buildTitledBorder("Chat Window"));
    scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());

    inputText = new JTextField(65);
    inputText.setEditable(true);
    inputText.setEnabled(false);
    inputText.setFont(MipavUtil.font12B);

    // hotkeys for server commands CTRL-T to tell specific user a message,
    // CTRL-L to list users, pressing enter clears the send text field
    inputText.addKeyListener(new KeyAdapter() { // make the field
      public void keyTyped(KeyEvent evt) { // not accept letters
        JTextField t = (JTextField) evt.getComponent();
        char ch = evt.getKeyChar();
        if (ch == KeyEvent.VK_ENTER) { // make sure the enter key acts as clicking Send
          client.send(t.getText());
          t.setText("");
        }
        else if (ch == 20) {
          t.setText("/TELL ");
        }
        else if (ch == 12) {
          t.setText("/LIST");
        }
        else if (ch == 6) {
          t.setText("/SEND ");
        }
        else {
          //System.out.println(Integer.toString(ch));
        }
      }
    });

    sendText = new JButton("Send");
    sendText.addActionListener(this);
    sendText.setActionCommand("Send");
    sendText.setEnabled(false);

    JButton receiveButton = new JButton("Change receive directory");
    receiveButton.addActionListener(this);
    receiveButton.setActionCommand("ChangeReceiveDir");

    receiveField = new JTextField(65);
    receiveField.setEditable(false);
    receiveField.setText(defaultReceiveDirectory);

    GridBagLayout gbl = new GridBagLayout();
    GridBagConstraints gbc = new GridBagConstraints();
    JPanel sendPanel = new JPanel();
    sendPanel.setLayout(gbl);

    gbc.fill = gbc.HORIZONTAL;
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 100;
    gbc.insets = new Insets(0, 0, 0, 20);
    sendPanel.add(inputText, gbc);

    gbc.weightx = 0;
    gbc.insets = new Insets(0, 0, 0, 0);
    gbc.fill = gbc.NONE;
    gbc.gridx = 1;
    sendPanel.add(sendText, gbc);

    sendPanel.setBorder(buildTitledBorder("Send Text"));

    JPanel receivePanel = new JPanel();
    GridBagLayout gbl3 = new GridBagLayout();
    GridBagConstraints gbc3 = new GridBagConstraints();
    receivePanel.setLayout(gbl3);

    gbc3.fill = gbc.HORIZONTAL;
    gbc3.gridx = 0;
    gbc.gridy = 0;
    gbc3.weightx = 100;
    gbc3.insets = new Insets(0, 0, 0, 20);
    receivePanel.add(receiveField, gbc3);

    gbc3.weightx = 0;
    gbc3.gridx = 1;
    gbc3.fill = gbc.NONE;
    gbc3.insets = new Insets(0, 0, 0, 0);
    receivePanel.add(receiveButton, gbc3);
    receivePanel.setBorder(buildTitledBorder("Directory to save file(s)"));

    JPanel mainPanel = new JPanel();
    GridBagConstraints gbc2 = new GridBagConstraints();
    GridBagLayout gbl2 = new GridBagLayout();
    mainPanel.setLayout(gbl2);

    gbc2.gridx = 0;
    gbc2.gridy = 0;
    gbc2.weightx = 100;
    gbc2.weighty = 100;
    gbc2.gridwidth = gbc2.REMAINDER;
    gbc2.fill = gbc2.BOTH;
    gbc2.insets = new Insets(0, 10, 10, 10);
    mainPanel.add(scrollPane, gbc2);

    gbc2.gridy = 1;
    gbc2.weightx = 100;
    gbc2.weighty = 0;
    gbc2.fill = gbc2.HORIZONTAL;
    gbc2.insets = new Insets(0, 10, 10, 10);
    mainPanel.add(sendPanel, gbc2);

    gbc2.gridy = 2;
    gbc2.weightx = 100;
    gbc2.weighty = 0;
    gbc2.fill = gbc2.HORIZONTAL;
    gbc2.insets = new Insets(0, 10, 10, 10);
    mainPanel.add(receivePanel, gbc2);

    JPanel serverPanel = buildServerPanel();
    buildMenuBar();

    tabbedPane = new JTabbedPane();
    tabbedPane.setFont(MipavUtil.font12B);
    tabbedPane.addTab("Main", null, mainPanel);
    tabbedPane.addTab("Hosts", null, serverPanel);
    tabbedPane.addTab("File Transfer", null, buildFileBrowserPanel());

    tabbedPane.addChangeListener(this);

    try {
      userIP = InetAddress.getLocalHost().getHostAddress();
    }
    catch (UnknownHostException error) {
      userIP = new String("IP Unknown");
    }

    getContentPane().add(tabbedPane, BorderLayout.CENTER);
    setResizable(true);
    setVisible(true);
  }

  /**
   * Initializes the username (reads from preferences)
   */
  private void initUsername() {
    String key = "Username";
    StringTokenizer tok;
    Preferences.read();

    username = "Default_User";

    if (Preferences.getProperty(key) != null) {
      username = Preferences.getProperty(key);
    }
  }

  /**
   * Initializes the keystore and truststore from the prefs file.
   * if they do not exist, they are defaulted
   * to user.home/.keystore with password "changeit"
   */
  private void initStores() {
    StringTokenizer tok;
    String key = "KeyStore";
    Preferences.read();
    keyPasswd = "changeit"; // for all keystores created, the password is set to "changeit"
                            // if the keystore is loaded, the user must specify the password (or it will be in the prefs file

    if (Preferences.getProperty(key) != null) {
      try {
        tok = new StringTokenizer(Preferences.getProperty(key), ";");
      }
      catch (OutOfMemoryError error) {
        MipavUtil.displayError("Out of memory: JChatFrame");
        keyStore = System.getProperty("user.home") + File.separator +
            ".truststore";
        System.setProperty("javax.net.ssl.trustStore", keyStore);
        return;
      }
      try {
        keyStore = tok.nextToken();
        keyPasswd = tok.nextToken();
      }
      catch (Exception error) {
        MipavUtil.displayError("JChatFrame.initTrustStore:" +
                               error.getMessage());
        keyStore = System.getProperty("user.home") + File.separator +
            ".truststore";

      }
    }
    else {
      //Keystore will be set to .keystore in user.home
      keyStore = System.getProperty("user.home") + File.separator + ".keystore";
      keyPasswd = "changeit";
      Preferences.setProperty("KeyStore", keyStore + ";" + keyPasswd);
    }

    System.setProperty("javax.net.ssl.keyStore", keyStore);
    System.setProperty("javax.net.ssl.keyStorePassword", keyPasswd);

    System.setProperty("javax.net.ssl.trustStore", keyStore);
    System.setProperty("javax.net.ssl.trustStorePassword", keyPasswd);
  }

  /**
   * Initialize the receive directory from the prefs file.  if it does not exist
   * it will be defaulted to user.home
   */
  private void initReceiveDir() {
    String key = "ReceiveDirectory";
    if (Preferences.getProperty(key) == null) {
      defaultReceiveDirectory = System.getProperty("user.home");
      Preferences.setProperty(key, defaultReceiveDirectory);
    }
    else {
      defaultReceiveDirectory = Preferences.getProperty(key);
    }

  }

  /**
   * Builds the server panel with the host table
   * @return the server panel
   */
  private JPanel buildServerPanel() {

    Font font12B = MipavUtil.font12B;
    JPanel serverPanel;
    StringTokenizer tok;
    String key = "Host1";
    Object rowData[];
    GridBagConstraints gbc;
    JScrollPane scrollPane;
    String columnNames[] = {
        "Default",
        "Name",
        "Alias",
        "IP Address",
        "Port"};
    try {
      serverPanel = new JPanel();
      serverPanel.setLayout(new GridBagLayout());
      gbc = new GridBagConstraints();
      rowData = new Object[5];
      serverModel = new ViewTableModel();
      serverTable = new JTable(serverModel);
      create = new JButton("Create");
      edit = new JButton("Edit");
      delete = new JButton("Delete");
      set = new JButton("Set As Default");
    }
    catch (OutOfMemoryError error) {
      MipavUtil.displayError(
          "Out of memory: ViewJFrameDICOMQuery.buildServerPanel");
      return null;
    }

    for (int i = 0; i < 5; i++) {
      serverModel.addColumn(columnNames[i]);
    }
    serverTable.setAutoResizeMode(serverTable.AUTO_RESIZE_ALL_COLUMNS);
    serverTable.getColumn("Default").setMinWidth(50);
    serverTable.getColumn("Default").setMaxWidth(50);
    serverTable.getColumn("Name").setMinWidth(75);
    serverTable.getColumn("Name").setMaxWidth(200);
    serverTable.getColumn("Alias").setMinWidth(75);
    serverTable.getColumn("Alias").setMaxWidth(200);
    serverTable.getColumn("IP Address").setMinWidth(200);
    serverTable.getColumn("IP Address").setMaxWidth(400);
    serverTable.getColumn("Port").setMinWidth(50);
    serverTable.getColumn("Port").setMaxWidth(100);

    serverTable.getTableHeader().setReorderingAllowed(false);

    Preferences.read();
    if (Preferences.getProperty(key) == null) {
      //System.err.println("Could NOT find Host1 key in prefs file");
      rowData[0] = Boolean.TRUE;
      rowData[1] = "Localhost";
      rowData[2] = "localhost";
      rowData[3] = "127.0.0.1";
      rowData[4] = "9030";
      String values[] = {
          (String) rowData[1], (String) rowData[2],
          (String) rowData[3], (String) rowData[4]};
      Preferences.setProperty(key, makeString(values) + "DEFAULT");

      defaultAddress = (String) rowData[3];
      defaultPort = Integer.parseInt( (String) rowData[4]);
    }

    while (Preferences.getProperty(key) != null) {
      //System.err.println("Found key: " + key);
      try {
        tok = new StringTokenizer(Preferences.getProperty(key), ";");
      }
      catch (OutOfMemoryError error) {
        MipavUtil.displayError("Out of memory: JChatFrame");
        return null;
      }

      try {
        rowData[1] = tok.nextToken();
        rowData[2] = tok.nextToken();
        rowData[3] = tok.nextToken();
        rowData[4] = tok.nextToken();
        if (key.equals(Preferences.getDefaultHostKey())) {
          rowData[0] = Boolean.TRUE;
        }
        else
          rowData[0] = Boolean.FALSE;
        serverModel.addRow(rowData);
        key = key.substring(0, 4) +
            (Integer.valueOf(key.substring(4)).intValue() + 1);
      }
      catch (Exception error) {
        MipavUtil.displayError("JChatFrame.buildServerPanel:" +
                               error.getMessage());
        //return null;
      }
    }

    try {
      serverTable.setPreferredScrollableViewportSize(new Dimension(450, 200));
      serverTable.setMinimumSize(new Dimension(450, 100));
      serverTable.setToolTipText("Double click on a server to set as default. ");
      scrollPane = new JScrollPane(serverTable,
                                   JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      scrollPane.setPreferredSize(new Dimension(450, 200));
      scrollPane.setMinimumSize(new Dimension(150, 100));
    }
    catch (OutOfMemoryError error) {
      MipavUtil.displayError("Out of memory: JChatFrame.buildServerPanel");
      return null;
    }

    serverTable.getSelectionModel().addListSelectionListener(this);
    serverTable.addMouseListener(this);
    serverTable.getTableHeader().addMouseListener(this);
    scrollPane.setBackground(Color.black);

    gbc = setGBC(0, 0, 4, 3);
    gbc.weightx = 100;
    gbc.weighty = 100;
    gbc.fill = GridBagConstraints.BOTH;

    serverPanel.add(scrollPane, gbc);

    create.setFont(font12B);
    create.setActionCommand("Create");
    create.setBackground(Color.gray.brighter());
    create.addActionListener(this);
    gbc = setGBC(0, 3, 1, 1);
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.weightx = 1.0;
    gbc.weighty = 1.0;

    serverPanel.add(create, gbc);

    edit.setFont(font12B);
    edit.setActionCommand("Edit");
    edit.setBackground(Color.gray.brighter());
    edit.addActionListener(this);
    edit.setEnabled(false);
    gbc = setGBC(1, 3, 1, 1);
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.weightx = 1.0;
    gbc.weighty = 1.0;

    serverPanel.add(edit, gbc);

    delete.setFont(font12B);
    delete.setActionCommand("Delete");
    delete.setBackground(Color.gray.brighter());
    delete.addActionListener(this);
    delete.setEnabled(false);
    gbc = setGBC(2, 3, 1, 1);
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.weightx = 1.0;
    gbc.weighty = 1.0;

    serverPanel.add(delete, gbc);

    set.setFont(font12B);
    set.setActionCommand("SetAs");
    set.setBackground(Color.gray.brighter());
    set.addActionListener(this);
    set.setEnabled(false);
    gbc = setGBC(3, 3, 1, 1);
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.weightx = 1.0;
    gbc.weighty = 1.0;

    serverPanel.add(set, gbc);

    return serverPanel;
  }

  /**
   * Builds the menu bar to be used for the "Main" and "Hosts" tabs
   */
  private void buildMenuBar() {
    menuBar = new JMenuBar();
    JMenu fileMenu = new JMenu("File");
    fileMenu.setFont(MipavUtil.font12B);

    JMenu userMenu = new JMenu("User");
    userMenu.setFont(MipavUtil.font12B);

    JMenu securityMenu = new JMenu("Security");
    securityMenu.setFont(MipavUtil.font12B);

    connect = new JMenu("Connect to...");
    connect.setFont(MipavUtil.font12B);
    JMenuItem addNewHost = new JMenuItem("Add new host");
    addNewHost.setFont(MipavUtil.font12B);
    addNewHost.addActionListener(this);
    addNewHost.setActionCommand("Create");

    JMenuItem defaultChoice = new JMenuItem("Default");
    defaultChoice.addActionListener(this);
    defaultChoice.setActionCommand("DefaultHost");
    defaultChoice.setFont(MipavUtil.font12B);

    connect.add(addNewHost);
    connect.add(new JSeparator());
    connect.add(defaultChoice);

    // find the default host
    JMenuItem serverTemp;
    int index;
    // builds the connect-to JMenuItems with actionCommands for each server name
    for (index = 0; index < serverTable.getRowCount(); index++) {
      String name = new String( (String) serverTable.getValueAt(index, 1));
      serverTemp = new JMenuItem(name);
      serverTemp.addActionListener(this);
      serverTemp.setActionCommand(name);
      serverTemp.setFont(MipavUtil.font12B);
      connect.add(serverTemp);
      hostsTable.put(name, serverTemp);

      if ( (Boolean) serverTable.getValueAt(index, 0) == Boolean.TRUE) {
        defaultAddress = (String) serverTable.getValueAt(index, 3);
        defaultPort = Integer.parseInt( (String) serverTable.getValueAt(index,
            4));
      }
    }

    reconnect = new JMenuItem("Reconnect");
    reconnect.addActionListener(this);
    reconnect.setActionCommand("Reconnect");
    reconnect.setEnabled(false);
    reconnect.setFont(MipavUtil.font12B);

    disconnect = new JMenuItem("Disconnect");
    disconnect.addActionListener(this);
    disconnect.setActionCommand("Disconnect");
    disconnect.setEnabled(false);
    disconnect.setFont(MipavUtil.font12B);

    changeTrust = new JMenuItem("Change KeyStore & TrustStore");
    changeTrust.addActionListener(this);
    changeTrust.setActionCommand("ChangeKeyStore");
    changeTrust.setFont(MipavUtil.font12B);

    changeReceiveDir = new JMenuItem("Change Receive Directory");
    changeReceiveDir.addActionListener(this);
    changeReceiveDir.setActionCommand("ChangeReceiveDir");
    changeReceiveDir.setEnabled(true);
    changeReceiveDir.setFont(MipavUtil.font12B);

    listUsers = new JMenuItem("List users (CTRL-L)");
    listUsers.addActionListener(this);
    listUsers.setActionCommand("ListUsers");
    listUsers.setEnabled(false);
    listUsers.setFont(MipavUtil.font12B);

    changeUsername = new JMenuItem("Change username");
    changeUsername.addActionListener(this);
    changeUsername.setActionCommand("ChangeUsername");
    changeUsername.setFont(MipavUtil.font12B);

    hostServer = new JMenuItem("Host chat server");
    hostServer.addActionListener(this);
    hostServer.setActionCommand("HostChatServer");
    hostServer.setFont(MipavUtil.font12B);

    stopServer = new JMenuItem("Stop chat server");
    stopServer.addActionListener(this);
    stopServer.setActionCommand("StopChatServer");
    stopServer.setFont(MipavUtil.font12B);
    stopServer.setEnabled(false);

    close = new JMenuItem("Close");
    close.addActionListener(this);
    close.setActionCommand("Close");
    close.setFont(MipavUtil.font12B);

    JMenuItem genKey = new JMenuItem("Generate key pair");
    genKey.addActionListener(this);
    genKey.setActionCommand("GenerateKeys");
    genKey.setFont(MipavUtil.font12B);

    JMenuItem importCert = new JMenuItem("Import certificate");
    importCert.addActionListener(this);
    importCert.setActionCommand("ImportCert");
    importCert.setFont(MipavUtil.font12B);

    changeServerPort = new JMenuItem("Change server port");
    changeServerPort.addActionListener(this);
    changeServerPort.setActionCommand("ChangeServerPort");
    changeServerPort.setFont(MipavUtil.font12B);

    changeXferPort = new JMenuItem("Change file transfer port");
    changeXferPort.addActionListener(this);
    changeXferPort.setActionCommand("ChangeXferPort");
    changeXferPort.setFont(MipavUtil.font12B);

    fileMenu.add(connect);
    fileMenu.add(reconnect);
    fileMenu.add(disconnect);
    fileMenu.add(new JSeparator());
    fileMenu.add(hostServer);
    fileMenu.add(changeServerPort);
    fileMenu.add(stopServer);
    fileMenu.add(new JSeparator());
    fileMenu.add(changeReceiveDir);
    fileMenu.add(changeXferPort);
    fileMenu.add(new JSeparator());
    fileMenu.add(close);

    userMenu.add(changeUsername);
    userMenu.add(listUsers);

    securityMenu.add(changeTrust);
    securityMenu.add(genKey);
    securityMenu.add(importCert);

    menuBar.add(fileMenu);
    menuBar.add(userMenu);
    menuBar.add(securityMenu);
    setJMenuBar(menuBar);

  }

  private JPanel buildFileBrowserPanel() {

    treePanel = new JPanel(new BorderLayout());
    imagePanel = new JPanel();
    addComponentListener(this);
    //toolPanel = buildToolbar();

    try {
        setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
    } catch ( FileNotFoundException error ) {
        Preferences.debug("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n");
        System.err.println("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n");
    }

    return initImagePanel();
  }

  /**
   *   Builds a toolbar with the same functionality as the menu.
   */
  private JPanel buildToolbar() {
    Border pressedBorder = BorderFactory.createLoweredBevelBorder();
    Border etchedBorder = BorderFactory.createEtchedBorder();

    JPanel panel = new JPanel(new BorderLayout());
    JToolBar tBar = new JToolBar();
    tBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
    tBar.setBorder(etchedBorder);
    tBar.setBorderPainted(true);

    JButton openButton = new JButton(MipavUtil.getIcon("filexfer.gif"));
    openButton.addActionListener(this);
    openButton.setToolTipText("Send selected files");
    openButton.setActionCommand("SendFiles");
    openButton.setBorderPainted(false);
    openButton.setRolloverEnabled(true);
    openButton.setRolloverIcon(MipavUtil.getIcon("filexferroll.gif"));
    openButton.setBorder(pressedBorder);
    openButton.addItemListener(this);
    openButton.setFocusPainted(false);
    tBar.add(openButton);

    JButton refreshButton = new JButton(MipavUtil.getIcon("refresh.gif"));
    refreshButton.addActionListener(this);
    refreshButton.setToolTipText("Refresh file list");
    refreshButton.setActionCommand("Refresh");
    refreshButton.setBorderPainted(false);
    refreshButton.setRolloverEnabled(true);
    refreshButton.setRolloverIcon(MipavUtil.getIcon("refreshroll.gif"));
    refreshButton.setBorder(pressedBorder);
    refreshButton.addItemListener(this);
    refreshButton.setFocusPainted(false);
    tBar.add(refreshButton);

    JButton newButton = new JButton(MipavUtil.getIcon("new.gif"));
    newButton.addActionListener(this);
    newButton.setToolTipText("New top directory");
    newButton.setActionCommand("New");
    newButton.setBorderPainted(false);
    newButton.setRolloverEnabled(true);
    newButton.setRolloverIcon(MipavUtil.getIcon("newroll.gif"));
    newButton.setBorder(pressedBorder);
    newButton.addItemListener(this);
    newButton.setFocusPainted(false);
    tBar.add(newButton);

    tBar.setFloatable(false);
    panel.add(tBar, BorderLayout.NORTH);
    return panel;
  }

  /**
   *   Initializes the Image Panel
   */
  protected JPanel initImagePanel() {

    buildMenu();
    buildSourceTreeListing(false);
    buildBrightContPanel();
    JSplitPane splitPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                                           true, treePanel, buildImagePanel());
    splitPanel.setBorder(new TitledBorder("Tree of Files in Directory"));

    toolPanel = buildToolbar();

    JPanel mainPanel = new JPanel();
    mainPanel.setLayout(new GridBagLayout());
    GridBagConstraints gbc = new GridBagConstraints();

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = gbc.REMAINDER;
    gbc.weightx = 100;
    gbc.fill = gbc.HORIZONTAL;
    mainPanel.add(toolPanel, gbc);

    gbc.gridy = 1;
    gbc.gridwidth = gbc.REMAINDER;
    gbc.gridheight = gbc.REMAINDER;
    gbc.weightx = 100;
    gbc.weighty = 100;
    gbc.fill = gbc.BOTH;
    mainPanel.add(splitPanel, gbc);

    return mainPanel;
  }

  /**
   *   Builds a small menu with "New directory", "Refresh directory",
   *   "Show Image Preview", and "send selected files" options.
   */
  private void buildMenu() {
    JMenu menu;
    JMenuItem itemRefresh;
    JMenuItem itemNew;
    JMenuItem itemFilter;
    JMenuItem itemSend;

    menuBar2 = new JMenuBar();
    menu = new JMenu("File");
    itemNew = new JMenuItem("New directory");
    itemRefresh = new JMenuItem("Refresh list");
    itemSend = new JMenuItem("Send selected files");
    itemPreview = new JCheckBoxMenuItem("Show image previews");


    thumbnailOption = new JCheckBoxMenuItem("Show XML thumbnail if available");
    thumbnailOption.setFont(MipavUtil.font12B);
    thumbnailOption.setSelected(showXMLThumbnail);
    thumbnailOption.addActionListener(this);
    thumbnailOption.setActionCommand("ToggleThumbnail");


    menu.setFont(MipavUtil.font12B);
    itemNew.setFont(MipavUtil.font12B);
    itemNew.setActionCommand("New");
    itemNew.addActionListener(this);
    itemRefresh.setFont(MipavUtil.font12B);
    itemRefresh.setActionCommand("Refresh");
    itemRefresh.addActionListener(this);
    itemSend.setFont(MipavUtil.font12B);
    itemSend.setActionCommand("SendFiles");
    itemSend.addActionListener(this);

    itemPreview.setFont(MipavUtil.font12B);
    itemPreview.setSelected(false);
    itemPreview.addActionListener(this);
    itemPreview.setActionCommand("TogglePreview");

    //menu.add(changeXferPort);
    menu.add(itemNew);
    menu.add(itemRefresh);
    menu.add(new JSeparator());
    menu.add(itemSend);
    menu.add(new JSeparator());
    menu.add(itemPreview);
    menu.add(thumbnailOption);
    //menu.add(close);
    menuBar2.add(menu);
  }

  /**
   *   Creates the tree that holds the image files and returns
   *   the panel containing the tree.
   *   @return The panel containing the file tree.
   */
  protected void buildSourceTreeListing(boolean directoriesOnly) {
    // build a directory tree by first finding the roots of the filesystem.
    DefaultMutableTreeNode fs = new DefaultMutableTreeNode("Computer");
    ViewFileTreeNode rootNode = null;
    if (fs != null) { // fs is null when the set of roots could not be determined
      rootNode = new ViewFileTreeNode(new File(directory), true);
      rootNode.exploreDirectoriesOnly(directoriesOnly);
      fs.add(rootNode);
      rootNode.getPath();
      directoryTree = new JTree(fs);
      directoryTree.setRootVisible(false);
    }
    else { // we can build an empty tree, but it won't mean anything.  throw error??  FIXME
      directoryTree = new JTree();
    }
    JScrollPane treeScroll = new JScrollPane(directoryTree);
    treePanel.add(treeScroll, BorderLayout.CENTER);
    treePanel.setPreferredSize(new Dimension(290, 420));
    directoryTree.getSelectionModel().setSelectionMode(TreeSelectionModel.
        DISCONTIGUOUS_TREE_SELECTION);
    directoryTree.addTreeSelectionListener(this);
    directoryTree.addTreeExpansionListener(this);

    directoryTree.expandRow(0);
  }

  /**
   *   Sets up the image panel and the table that will
   *   store basic header info, and returns the panel
   *   containing these.
   *   @return The panel containing the image and the header
   *           info table.
   */
  private JSplitPane buildImagePanel() {

    //JPanel panel        = new JPanel(new GridBagLayout());
    JPanel tablePanel = new JPanel(new BorderLayout());

    defaultImageSize = new Dimension(213, 160);
    smaller = false;
    imagePanel.setPreferredSize(defaultImageSize);

    Box scrollingBox;
    JScrollPane scrollPane;
    try {
      scrollingBox = new Box(BoxLayout.Y_AXIS);

      primaryModel = new ViewTableModel();
      primaryTable = new JTable(primaryModel);
      secondaryModel = new ViewTableModel();
      secondaryTable = new JTable(secondaryModel);
    }
    catch (OutOfMemoryError error) {
      MipavUtil.displayError("ViewFileInfo reports: Out of memory!");
      return null;
    }
    primaryModel.addColumn("Name");
    primaryModel.addColumn("Value");

    primaryTable.setAutoResizeMode(primaryTable.AUTO_RESIZE_ALL_COLUMNS);
    primaryTable.setSelectionMode(ListSelectionModel.
                                  MULTIPLE_INTERVAL_SELECTION);
    primaryTable.getColumn("Name").setMinWidth(160);
    primaryTable.getColumn("Name").setMaxWidth(500);
    primaryTable.getColumn("Value").setMinWidth(50);
    primaryTable.getColumn("Value").setMaxWidth(1000);

    JLabel headerLabel = new JLabel("Image Information");
    headerLabel.setForeground(Color.black);
    headerLabel.setFont(MipavUtil.font12);
    headerLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
    scrollingBox.add(headerLabel);
    scrollingBox.add(primaryTable);

    secondaryModel.addColumn("Tag");
    secondaryModel.addColumn("Name");
    secondaryModel.addColumn("Value");

    secondaryTable.setAutoResizeMode(secondaryTable.AUTO_RESIZE_ALL_COLUMNS);
    secondaryTable.setSelectionMode(ListSelectionModel.
                                    MULTIPLE_INTERVAL_SELECTION);
    secondaryTable.getColumn("Tag").setMinWidth(90);
    secondaryTable.getColumn("Tag").setMaxWidth(90);
    secondaryTable.getColumn("Name").setMinWidth(160);
    secondaryTable.getColumn("Name").setMaxWidth(500);
    secondaryTable.getColumn("Value").setMinWidth(50);
    secondaryTable.getColumn("Value").setMaxWidth(1000);

    otherLabel = new JLabel("Other Information");
    otherLabel.setForeground(Color.black);
    otherLabel.setFont(MipavUtil.font12);
    otherLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
    otherLabel.setVisible(false);
    scrollingBox.add(otherLabel);
    scrollingBox.add(secondaryTable);

    try {
      scrollPane = new JScrollPane(scrollingBox,
                                   JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      scrollPane.setPreferredSize(new Dimension(420, 170));
      scrollPane.setMinimumSize(new Dimension(60, 170));
    }
    catch (OutOfMemoryError error) {
      MipavUtil.displayError("ViewFileInfo reports: Out of memory!");
      return null;
    }

    scrollPane.setBackground(Color.black);

    tablePanel.add(scrollPane);
    tablePanel.setPreferredSize(new Dimension(400, 150));

    imageSliderPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, imagePanel, brightPanel);
    JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, imageSliderPane, tablePanel);

    return splitPane;
  }

  /*   Initializes GUI components and displays dialog.<p>
  *   For the brightSlider the slider values and the brightness values are identical.
  *   brightness is an offset going from -255 to 255.  This is enough to change all 0 values
  *   to 255 and all 255 values to 0.  brightness is added to all contrast scaled red,
  *   green, and blue.<p>
  *   However, for the constrastSlider the slider values are different from the contrast values.
  *   The contrast values go from 0.1 to 10.0 while the slider values go from -200 to 200.
  *   contrast = (float)Math.pow(10.0,contSlider.getValue()/200.0)
  *   The original red, green, and blue are mutliplied by contrast.
  *   @param brightness   Initial brightness.
  *   @param contrast     Initial contrast.
  */
 private void buildBrightContPanel() {
   serif12 = MipavUtil.font12;
   serif12B = MipavUtil.font12B;
   brightSlider = new JSlider(JSlider.HORIZONTAL, -255, 255, origBrightness);

   brightSlider.setMajorTickSpacing(102);
   brightSlider.setPaintTicks(true);
   brightSlider.setEnabled(true);
   brightSlider.addChangeListener(this);

   JLabel maximum = new JLabel(String.valueOf(255));
   maximum.setForeground(Color.black);
   maximum.setFont(serif12);

   current = new JLabel(String.valueOf(origBrightness));
   current.setForeground(Color.black);
   current.setFont(serif12B);

   JLabel minimum = new JLabel(String.valueOf( -255));
   minimum.setForeground(Color.black);
   minimum.setFont(serif12);

   JPanel sliderPanel = new JPanel(new GridBagLayout());
   GridBagConstraints gbc = new GridBagConstraints();

   gbc.gridx = 0;
   gbc.gridy = 0;
   gbc.gridwidth = 3;
   gbc.weightx = 1;
   gbc.gridheight = 1;
   gbc.fill = gbc.HORIZONTAL;

   sliderPanel.add(brightSlider, gbc);

   gbc.gridx = 0;
   gbc.gridy = 1;
   gbc.gridwidth = 1;
   gbc.weightx = 0;
   gbc.anchor = gbc.WEST;
   gbc.fill = gbc.NONE;

   sliderPanel.add(minimum, gbc);

   gbc.gridx = 1;
   gbc.anchor = gbc.CENTER;
   gbc.weightx = .5;

   sliderPanel.add(current, gbc);

   gbc.gridx = 2;
   gbc.anchor = gbc.EAST;
   gbc.weightx = 0;

   sliderPanel.add(maximum, gbc);
   sliderPanel.setBorder(buildTitledBorder("Level"));

   contSlider = new JSlider(JSlider.HORIZONTAL, -200, 200, (int) (Math.round(86.85889638 * Math.log(origContrast))));

   contSlider.setMajorTickSpacing(80);
   contSlider.setPaintTicks(true);
   contSlider.setEnabled(true);
   contSlider.addChangeListener(this);

   JLabel maximum2 = new JLabel(String.valueOf(10));
   maximum2.setForeground(Color.black);
   maximum2.setFont(serif12);

   nfc = NumberFormat.getNumberInstance();
   nfc.setMaximumFractionDigits(3);

   current2 = new JLabel(String.valueOf(nfc.format(origContrast)));
   current2.setForeground(Color.black);
   current2.setFont(serif12B);

   JLabel minimum2 = new JLabel(String.valueOf(0.100));
   minimum2.setForeground(Color.black);
   minimum2.setFont(serif12);

   JPanel sliderPanel2 = new JPanel(new GridBagLayout());

   gbc.gridx = 0;
   gbc.gridy = 0;
   gbc.gridwidth = 3;
   gbc.weightx = 1;
   gbc.gridheight = 1;
   gbc.fill = gbc.HORIZONTAL;

   sliderPanel2.add(contSlider, gbc);

   gbc.gridx = 0;
   gbc.gridy = 1;
   gbc.gridwidth = 1;
   gbc.weightx = 0;
   gbc.anchor = gbc.WEST;
   gbc.fill = gbc.NONE;

   sliderPanel2.add(minimum2, gbc);

   gbc.gridx = 1;
   gbc.anchor = gbc.CENTER;
   gbc.weightx = .5;

   sliderPanel2.add(current2, gbc);

   gbc.gridx = 2;
   gbc.anchor = gbc.EAST;
   gbc.weightx = 0;

   sliderPanel2.add(maximum2, gbc);
   sliderPanel2.setBorder(buildTitledBorder("Window"));

   JPanel centerPanel = new JPanel(new GridLayout(2, 1));
   centerPanel.add(sliderPanel2);
   centerPanel.add(sliderPanel);

   brightPanel = new JPanel(new BorderLayout());
   brightPanel.add(centerPanel);
   brightPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
   brightPanel.setPreferredSize(new Dimension(80, 40));

 }


  /**
   * Notifies the Chatframe the status of your connect and takes
   * the appropriate actions for successful and failed attempts
   * @param success (whether you were able to connect)
   */

  public void notifyLogin(boolean success) {
    setOnline(success);
  }

  /**
   *  finalizes (cleanup)
   */
  public void finalize() {
    if (client != null) {
      client.finalize();
    }
    client = null;
    textArea = null;
    inputText = null;

    try {
      super.finalize();
    }
    catch (Throwable t) {
      System.out.println(t.toString());
    }
  }

  /**
   * for every action.. there is a reaction
   * @param event - the action that occurred
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();

    // displays a new directory in the file transfer tab
    if (command.equals("New")) {
      ViewDirectoryChooser chooser = new ViewDirectoryChooser(userInterface, this);
      String dir = chooser.getImageDirectory();
      if (dir != null) {
          userInterface.setDefaultDirectory(dir);
        directory = dir;
        treePanel.removeAll();
        buildSourceTreeListing(false);
        validate();
      }
    }
    else if (command.equals("ToggleThumbnail")) {
        if (thumbnailOption.isSelected()) {
            showXMLThumbnail = true;
        }
        else {
            showXMLThumbnail = false;
        }
    }
    // generates a private/public key for export
    else if (command.equals("GenerateKeys")) {
      generateKeyPair();
    }
    // imports a certificate w\ public key into the keystore
    else if (command.equals("ImportCert")) {
      JFileChooser chooser = new JFileChooser(System.getProperty("user.home"));
      chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      chooser.setDialogTitle("Import Certificate");

      Point p = this.getLocation();
      p.x += 200;
      p.y += 200;
      chooser.setLocation(p);

      int returnVal = chooser.showDialog(this, "Import");
      if (returnVal == JFileChooser.APPROVE_OPTION) {
        try {
          FileInputStream fr = new FileInputStream(chooser.getSelectedFile());
          CertificateFactory cf = CertificateFactory.getInstance("X509");
          X509Certificate cert = (X509Certificate) cf.generateCertificate(fr);
          fr.close();


          Preferences.debug("Certificate Details: ", Preferences.DEBUG_COMMS);
          Preferences.debug("\tfor: " + cert.getSubjectDN(), Preferences.DEBUG_COMMS);
          Preferences.debug("\tissued by: " + cert.getIssuerDN(), Preferences.DEBUG_COMMS);
          Preferences.debug("\tvalid from " + cert.getNotBefore() + " to " +
                             cert.getNotAfter(), Preferences.DEBUG_COMMS);
          Preferences.debug("\tserial number: " + cert.getSerialNumber(), Preferences.DEBUG_COMMS);
          Preferences.debug("\tgenerated with: " + cert.getSigAlgName(), Preferences.DEBUG_COMMS);

          if (!loadKeyStore()) {
            return;
          }
          ks.setCertificateEntry(cert.getIssuerDN().toString(), cert);
          if (!saveKeyStore()) {
            System.err.println("Unable to save keystore");
            return;
          }
          //System.err.println("Keystore saved with new certificate entry");
        }
        catch (FileNotFoundException e) {
          e.printStackTrace();
        }
        catch (CertificateException e) {
          e.printStackTrace();
        }
        catch (KeyStoreException e) {
          e.printStackTrace();
        }
        catch (IOException e) {
          System.err.println("Got an Exception in Import Certificate: " + e.toString());
          e.printStackTrace();
        }
      }
    }
    //changes username
    else if (command.equals("ChangeUsername")) {
      UsernameDialog usernameDialog = new UsernameDialog(this, username);
      Point p = this.getLocation();
      p.x += 300;
      p.y += 300;
      usernameDialog.setLocation(p);
      usernameDialog.setVisible(true);
      if (usernameDialog.isOkay()) {
        this.username = usernameDialog.getNewName();
        Preferences.setProperty("Username", this.username);
      }
      usernameDialog = null;
    }
    //starts the chatserver on LOCALHOST
    else if (command.equals("HostChatServer")) {
      if (server == null) {
        try {
          //System.err.println(System.getProperty("javax.net.ssl.keyStore"));
          //System.err.println(System.getProperty("javax.net.ssl.trustStore"));
          server = new ChatServer(currentServerPort);
          Thread serverThread = new Thread(server);
          serverThread.start();
        }
        catch (IOException e) {
          System.out.println("Caught exception: " + e.toString());
        }
      }
      else {
        MipavUtil.displayInfo("Server is currently running");
      }
      hostServer.setEnabled(false);
      changeServerPort.setEnabled(false);
      stopServer.setEnabled(true);
    }
    // stops the chat server
    else if (command.equals("StopChatServer")) {
      if (server != null) {
        server.stopServer();
        server = null;
      }
      stopServer.setEnabled(false);
      changeServerPort.setEnabled(true);
      hostServer.setEnabled(true);

    }
    // sends the list-user command to the server
    else if (command.equals("ListUsers")) {
      client.send("/LIST");
    }
    // changes the receive-to directory for files
    else if (command.equals("ChangeReceiveDir")) {
      JFileChooser chooser;

      chooser = new JFileChooser(new File(defaultReceiveDirectory));
      chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
      chooser.setDialogTitle("Choose Receive Directory");

      Point p = this.getLocation();
      p.x += 300;
      p.y += 300;
      chooser.setLocation(p);

      int returnVal = chooser.showDialog(this, "Select");
      if (returnVal == JFileChooser.APPROVE_OPTION) {
        defaultReceiveDirectory = chooser.getSelectedFile().getAbsolutePath();
        Preferences.setProperty("ReceiveDirectory", defaultReceiveDirectory);
        receiveField.setText(defaultReceiveDirectory);
      }
    }
    //changes the server's port (for hosting)
    else if (command.equals("ChangeServerPort")) {
      PortDialog portD = new PortDialog(this, currentServerPort, true);
      Point p = this.getLocation();
        p.x += 300;
        p.y += 300;
        portD.setLocation(p);
        portD.setVisible(true);
        if (portD.isOkay()) {
          currentServerPort = portD.getPort();
        }
    }

    else if (command.equals("ChangeXferPort")) {
      PortDialog portD = new PortDialog(this, xferPort, false);
      Point p = this.getLocation();
        p.x += 300;
        p.y += 300;
        portD.setLocation(p);
        portD.setVisible(true);
        if (portD.isOkay()) {
          xferPort = portD.getPort();
        }

    }

    //changes the keystore/truststore.. requires a password as well
    else if (command.equals("ChangeKeyStore")) {
      JFileChooser chooser;

      chooser = new JFileChooser(new File(keyStore));
      chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      chooser.setDialogTitle("Select KeyStore/TrustStore File");

      Point p = this.getLocation();
      p.x += 300;
      p.y += 300;
      chooser.setLocation(p);

      int returnVal = chooser.showDialog(this, "Select");
      if (returnVal == JFileChooser.APPROVE_OPTION) {
        keyStore = chooser.getSelectedFile().getAbsolutePath();
        System.setProperty("javax.net.ssl.keyStore", keyStore);
        System.setProperty("javax.net.ssl.trustStore", keyStore);

        PasswordDialog passwd = new PasswordDialog(this);
        p = this.getLocation();
        p.x += 300;
        p.y += 300;
        passwd.setLocation(p);
        passwd.setVisible(true);
        if (passwd.isOkay()) {
          keyPasswd = passwd.getPassword();
          System.setProperty("javax.net.ssl.keyStorePassword",
                             passwd.getPassword());
          System.setProperty("javax.net.ssl.trustStorePassword",
                             passwd.getPassword());
          Preferences.setProperty("KeyStore",
                                  keyStore + ";" + passwd.getPassword());
        }
        else {
          MipavUtil.displayWarning("KeyStore/TrustStore password is not set!");
        }
        passwd = null;
      }
    }
    // toggles between show image preview (slower) and no preview
    else if (command.equals("TogglePreview")) {
      if (itemPreview.isSelected()) {
        showPreview = true;
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) directoryTree.getLastSelectedPathComponent();
        if (node == null)
            return;
        if (! ( (ViewFileTreeNode) node).isDirectory() && showPreview) {
            FileInfoBase fileInfo = buildImage( ( (ViewFileTreeNode) node).getName(),
                                                ( (ViewFileTreeNode) node).
                                                getDirectory() + File.separatorChar);
            if (fileInfo == null)
                return;
            primaryModel.setRowCount(0);
            secondaryModel.setRowCount(0);
            otherLabel.setVisible(false);
            String[] info = new String[] {"", ""};
            for (int i = 0; i < fileInfo.getExtents().length; i++) {
                info[0] = "Dimension " + i;
                info[1] = Integer.toString(fileInfo.getExtents()[i]);
                primaryModel.addRow(info);
            }

            info[0] = "Type";
            info[1] = ModelStorageBase.getBufferTypeStr(fileInfo.getDataType());
            primaryModel.addRow(info);

            info[0] = "Max";
            info[1] = Double.toString(fileInfo.getMax());
            primaryModel.addRow(info);

            info[0] = "Min";
            info[1] = Double.toString(fileInfo.getMin());
            primaryModel.addRow(info);

            info[0] = "Modality";
            info[1] = FileInfoBase.getModalityStr(fileInfo.getModality());
            primaryModel.addRow(info);

            info[0] = "Orientation";
            info[1] = FileInfoBase.getImageOrientationStr(fileInfo.
                getImageOrientation());
            primaryModel.addRow(info);

            float[] resolutions;
            resolutions = fileInfo.getResolutions();
            int[] measure;
            measure = fileInfo.getUnitsOfMeasure();
            for (int i = 0; i < fileInfo.getExtents().length; i++) {
                if (resolutions[i] > 0.0) {
                    info[0] = "Pixel resolution " + i;
                    info[1] = Float.toString(resolutions[i]) + " " +
                        FileInfoBase.getUnitsOfMeasureStr(measure[i]);
                    primaryModel.addRow(info);
                } // end of if (resolutions[i] > 0.0)
            } // for (i=0; i < 5; i++)

            info[0] = "Endianess";
            if (fileInfo.getEndianess() == FileBase.LITTLE_ENDIAN) {
                info[1] = "Little Endian";
            }
            else {
                info[1] = "Big Endian";
            }

            if (fileInfo.getFileFormat() == FileBase.DICOM) {
                otherLabel.setVisible(true);
                JDialogFileInfoDICOM.showTags(secondaryModel, (FileInfoDicom) fileInfo, false);
            }
        }

      }
      else {
        showPreview = false;
        if (img != null) {
          imagePanel.remove(img);
          img.dispose(true);
        }
        else if (thumbnail != null) {
            imagePanel.remove(thumbnail);
            thumbnail.finalize();
            thumbnail = null;
        }

        int numrows = primaryModel.getRowCount();
        for (int i = numrows - 1; i >= 0; i--) {
          primaryModel.removeRow(i);
        }
        primaryTable.removeAll();
        primaryModel.fireTableDataChanged();
        primaryTable.updateUI();

        numrows = secondaryTable.getRowCount();
        for (int i = numrows - 1; i >= 0; i--) {
          secondaryModel.removeRow(i);
        }
        secondaryTable.removeAll();
        secondaryModel.fireTableDataChanged();
        secondaryTable.updateUI();

        imagePanel.updateUI();
      }
    }
    //brings up the file send dialog
    else if (command.equals("SendFiles")) {
      String IP2SEND = null;

      String [] ips = null;
      String [] names = null;

      if (userList.size() < 1) {
        MipavUtil.displayError("There are no other users connected");
        return;
      }

      // allow the user to choose which user will receive the files
      SendToUserDialog sendDialog = new SendToUserDialog(this);
      Point p = this.getLocation();
      p.x += 300;
      p.y += 300;
      sendDialog.setLocation(p);
      sendDialog.setVisible(true);
      if (sendDialog.isOkay()) {
        ips = sendDialog.getIPs();
        names = sendDialog.getNames();
      }
      else {
        // operation cancelled
        return;
      }

      Vector files2send = new Vector();
      String fileName;
      String fileDir;
      String fileInfo;

      TreePath selected[] = directoryTree.getSelectionPaths();
      if (selected == null) {
        MipavUtil.displayError("You must select a file to send.");
        return;
      }

      // add all file/directory names and paths to a vector
      int index;
      for (index = 0; index < selected.length; index++) {
        //check to see if it is a file or a directory
        addFilesToVector( ( (ViewFileTreeNode) selected[index].
                           getLastPathComponent()).
                         getDirectory() + File.separator +
                         ( (ViewFileTreeNode) selected[index].
                          getLastPathComponent()).
                         getName(), files2send);
      }

      // Create a buffer for reading the files
      byte[] buf = new byte[1024];

      progressBar = new ViewJProgressBar("File Send", "Zipping files: ", 0, 100, false, null, null);

      p = this.getLocation();
      p.x += 300;
      p.y += 300;

      progressBar.setLocation(p);
      progressBar.setVisible(true);
      progressBar.updateValue(0, false);

      try {
        // Create the ZIP file
        String outFilename = directory + File.separator + "temp.zip";
        ZipOutputStream out = new ZipOutputStream(new FileOutputStream(
            outFilename));

        // Compress the files
        int size = files2send.size();
        for (int i = 0; i < size; i++) {
          String fullName = (String) files2send.elementAt(i);
          File tempF = new File(fullName);
          String zippedName = fullName.substring(directory.length());

          if (tempF.isDirectory()) {
            zippedName += "/";
          }
          else {
            progressBar.setMessage("Zipping file: " + zippedName);
          }

          // Add ZIP entry to output stream.
          out.putNextEntry(new ZipEntry(zippedName));

          if (tempF.isFile()) {
            FileInputStream in = new FileInputStream(fullName);

            // Transfer bytes from the file to the ZIP file
            int len;
            while ( (len = in.read(buf)) > 0) {
              out.write(buf, 0, len);
            }
            in.close();
          }
          // Complete the entry
          out.closeEntry();

          int value = (int) ( ( (float) i + 1.0f) / (float) size * 100.0f);
          progressBar.updateValue(value, false);
        }

        // Complete the ZIP file
        out.close();

        progressBar.setVisible(false);
        progressBar = null;

        FileSender fs = null;

        for (int i = 0; i < ips.length; i++) {
          client.send("/SEND " + names[i]);
          fs = new FileSender(outFilename, ips[i], xferPort);
          if (!fs.login()) {
            System.err.println("Unable to login");
            client.send("/CANCELSEND " + names[i]);
            fs = null;
          }
          else {
            while (!fs.isDone()) {
              // wait to send to next user
            }
            System.err.println("finished sending to user");

          }
        }

        // Delete the zip file created

      }
      catch (IOException e) {
        System.err.println("IOException while zipping files: " + e.toString());
      }

    }
    // refreshes the file view for file-transfer tab
    else if (command.equals("Refresh")) {
      treePanel.removeAll();
      buildSourceTreeListing(false);
      validate();
    }
    //sends text from client to server
    else if (command.equals("Send")) {
      String newText = inputText.getText();
      //don't allow user to send through this command (starts the file receiver)
      if (!newText.startsWith("/SEND"))
        client.send(inputText.getText());
      inputText.setText("");
    }
    //reconnects to last connected-to server
    else if (command.equals("Reconnect")) {

      client = new ChatClient(this.username, this.userIP, this, currentAddress,
                              currentPort);
    }
    //connects to the default server
    else if (command.equals("DefaultHost")) {
      if (client != null) {
        client.finalize();
        client = null;
      }
      currentAddress = defaultAddress;
      currentPort = defaultPort;
      client = new ChatClient(this.username, this.userIP, this, currentAddress,
                              currentPort);
    }
    //disconnects from server
    else if (command.equals("Disconnect")) {
      if (client != null) {
        textArea.append("Logged out of server.\n");

        setOnline(false);
      }
    }
    //closes the chat window
    else if (command.equals("Close")) {
      if (client != null) {
        client.send("/LOGOUT");
        client.finalize();
        client = null;
      }
      this.finalize();
      this.setVisible(false);
    }
    //creates a new server entry
    else if (command.equals("Create")) {
      String values[];
      String key;
      JDialogServer createDialog;
      ViewTableModel model;

      try {
        createDialog = new JDialogServer(this, "Create Host", true);
      }
      catch (OutOfMemoryError error) {
        MipavUtil.displayError(
            "Out of memory: JChatFrame.actionPerformed");
        return;
      }

      key = Preferences.getNextHostKey();
      model = serverModel;

      if (!createDialog.isCancelled()) {
        values = createDialog.getValues();
        // add code here
        if (Preferences.getHostIP(values[0]) == null) {
          String value = makeString(values);

          // if this the only entry, set it to default
          Boolean onlyEntry = new Boolean(false);
          if (model.getRowCount() == 0) {
            onlyEntry = Boolean.TRUE;
            value += "DEFAULT";
            defaultAddress = values[2];
            defaultPort = Integer.parseInt(values[3]);
          }

          Object[] rowData = {
              onlyEntry, values[0], values[1], values[2], values[3]};
          model.addRow(rowData);
          Preferences.setProperty(key, value);
          Preferences.save();

          JMenuItem newServer = new JMenuItem(values[0]);
          newServer.setFont(MipavUtil.font12B);
          newServer.addActionListener(this);
          newServer.setActionCommand(values[0]);
          connect.add(newServer);
          hostsTable.put(values[0], newServer);


        }
        else {
          MipavUtil.displayError("Duplicate Names not allowed.");
        }
      }
    }
    //edits a server entry
    else if (command.equals("Edit")) {
      int row;
      String key;
      ViewTableModel model;
      JTable table;
      JDialogServer editDialog;

      model = serverModel;
      table = serverTable;
      key = "Server";

      row = table.getSelectedRow();
      String values[] = {
          (String) model.getValueAt(row, 1),
          (String) model.getValueAt(row, 2),
          (String) model.getValueAt(row, 3),
          (String) model.getValueAt(row, 4)};
      String temp = values[0];
      try {
        if (event.getSource().equals(edit))
          editDialog = new JDialogServer(this, "Edit Server", values, true);
        else
          editDialog = new JDialogServer(this, "Edit Storage Destination",
                                         values, false);
      }
      catch (OutOfMemoryError error) {
        MipavUtil.displayError(
            "Out of memory: ViewJFrameDICOMQuery.actionPerformed");
        return;
      }

      if (!editDialog.isCancelled()) {
        JMenuItem server2remove = (JMenuItem) hostsTable.get(temp);
        hostsTable.remove(temp);
        connect.remove(server2remove);

        values = editDialog.getValues();
        String value = makeString(values);
        key = key + (row + 1);
        if ( ( (Boolean) model.getValueAt(row, 0)).booleanValue()) {
          value = value + "DEFAULT;";
        }
        Preferences.setProperty(key, value);
        Object rowData[] = {

            model.getValueAt(row, 0),
            values[0],
            values[1],
            values[2],
            values[3]};
        model.removeRow(row);
        model.insertRow(row, rowData);
        setEnabled(set, false);
        setEnabled(delete, false);
        setEnabled(edit, false);
        Preferences.save();
        table.repaint();
        server2remove = new JMenuItem(values[0]);
        server2remove.addActionListener(this);
        server2remove.setActionCommand(values[0]);
        server2remove.setFont(MipavUtil.font12B);
        connect.add(server2remove);
        hostsTable.put(values[0], server2remove);
      }
    }

    //deletes the server from the table and preferences
    else if (command.equals("Delete")) {
      int row, option;

      row = serverTable.getSelectedRow();
      option = JOptionPane.showConfirmDialog(this,
          "Are you sure you wish to delete\nthis server?",
          "Confirm delete",
          JOptionPane.YES_NO_OPTION);

      if (option == JOptionPane.YES_OPTION) {
        String key, nextKey, value, stem;
        int i;

        int index;

        String name = (String) serverTable.getValueAt(row, 1);
        JMenuItem server2remove = (JMenuItem) hostsTable.get(name);
        hostsTable.remove(name);
        connect.remove(server2remove);

        i = row + 1;
        stem = "Host";
        key = stem + (i++);
        nextKey = stem + (i++);
        value = Preferences.getProperty(nextKey);
        while (value != null) {
          Preferences.setProperty(key, value);
          key = nextKey;
          nextKey = stem + (i++);
          value = Preferences.getProperty(nextKey);
        }
        Preferences.removeProperty(key);
        Preferences.save();

        serverModel.removeRow(row);
      }
    }

    //sets the selected server as default
    else if (command.equals("SetAs")) {
      String key, value, newValue, stem;
      StringTokenizer tok;
      int row;
      ViewTableModel model;

      row = serverTable.getSelectedRow();
      model = serverModel;
      key = Preferences.getDefaultHostKey();
      stem = "Host";

      defaultAddress = (String) serverTable.getValueAt(row, 3);
      defaultPort = Integer.parseInt( (String) serverTable.getValueAt(row, 4));

      // Reset the look of the table
      for (int i = 0; i < model.getRowCount(); i++) {
        if (i == row)
          model.setValueAt(Boolean.TRUE, i, 0);
        else
          model.setValueAt(Boolean.FALSE, i, 0);
      }

      // Erase the old DEFAULT from the preference file
      value = Preferences.getProperty(key);
      newValue = "";
      try {
        tok = new StringTokenizer(value, ";");
      }
      catch (OutOfMemoryError error) {
        MipavUtil.displayError(
            "Out of memory: JChatFrame.actionPerformed");
        return;
      }
      String port = "9030";
      for (int i = 0; i < 4; i++) {
        if (i == 3) {
          port = tok.nextToken();
          newValue = newValue + port + ";";
        }
        else
          newValue = newValue + tok.nextToken() + ";";
      }
      Preferences.setProperty(key, newValue);

      // Make the selected row the new DEFAULT
      key = stem + (row + 1);
      value = Preferences.getProperty(key);
      newValue = value + "DEFAULT;";
      Preferences.setProperty(key, newValue);

      Preferences.save();
      serverTable.repaint();
    }

    else {
      // check for specific server connects
      int index;
      for (index = 0; index < serverTable.getRowCount(); index++) {
        if (command.equalsIgnoreCase( (String) serverTable.getValueAt(index, 1))) {
          // command matched servername
          currentAddress = (String) serverTable.getValueAt(index, 3);
          currentPort = Integer.parseInt( (String) serverTable.getValueAt(index,
              4));
          client = new ChatClient(this.username, this.userIP, this,
                                  currentAddress, currentPort);
        }
      }
    }
  }


  /**
   * Loads the keystore, generates a private/public key,
   * saves the private in the keystore, prompts user to save public key as a
   * certificate file (auto-generated)
   */
  private void generateKeyPair() {

    char[] passchar = keyPasswd.toCharArray();

    if (!loadKeyStore()) {
      return;
    }

    //Generate a public and private key... the public will go in the certificate
    try {
      KeyPairGenerator keyGen = KeyPairGenerator.getInstance("RSA");
      keyGen.initialize(1024);
      KeyPair keypair = keyGen.genKeyPair();
      PrivateKey privateKey = keypair.getPrivate();
      PublicKey publicKey = keypair.getPublic();

      Security.addProvider(new BouncyCastleProvider());

      //create an x509 cert
      X509V1CertificateGenerator certGen = new X509V1CertificateGenerator();

      // Holds certificate attributes
      Hashtable attrs = new Hashtable();
      Vector vOrder = new Vector();

      //Common Name
      attrs.put(X509Principal.CN, username);
      vOrder.add(0, X509Principal.CN);
 /*     //Organizational Unit
      attrs.put(X509Principal.OU, "NIH");
      vOrder.add(0, X509Principal.OU);
      //Organization
      attrs.put(X509Principal.O, "NIH");
      vOrder.add(0, X509Principal.O);
      //Locality
      attrs.put(X509Principal.L, "NIH");
      vOrder.add(0, X509Principal.L);
      //State
      attrs.put(X509Principal.ST, "MD");
      vOrder.add(0, X509Principal.ST);
      //CountryCode
      attrs.put(X509Principal.C, "USA");
      vOrder.add(0, X509Principal.C);
*/

      // Set the issuer distinguished name
      certGen.setIssuerDN(new X509Principal(vOrder, attrs));
      // Valid before and after dates now to iValidity days in the future
      int iValidity = 365; // num of days
      certGen.setNotBefore(new Date(System.currentTimeMillis()));
      certGen.setNotAfter(new Date(System.currentTimeMillis() +
                                   ( (long) iValidity * 24 * 60 * 60 * 1000)));

      // Set the subject distinguished name (same as issuer for our purposes)
      certGen.setSubjectDN(new X509Principal(vOrder, attrs));

      // Set the public key
      certGen.setPublicKey(publicKey);

      // Set the algorithm
      certGen.setSignatureAlgorithm("MD5withRSA");

      // Set the serial number
      certGen.setSerialNumber(new BigInteger(Long.toString(System.
          currentTimeMillis() / 1000)));

      try {
        // Generate an X.509 certificate, based on the current issuer and
        // subject
        X509Certificate cert = certGen.generateX509Certificate(privateKey);

        Preferences.debug("Certificate Details: ", Preferences.DEBUG_COMMS);
        Preferences.debug("\tfor: " + cert.getSubjectDN(), Preferences.DEBUG_COMMS);
        Preferences.debug("\tissued by: " + cert.getIssuerDN(), Preferences.DEBUG_COMMS);
        Preferences.debug("\tvalid from " + cert.getNotBefore() + " to " + cert.getNotAfter(), Preferences.DEBUG_COMMS);
        Preferences.debug("\tserial number: " + cert.getSerialNumber(), Preferences.DEBUG_COMMS);
        Preferences.debug("\tgenerated with: " + cert.getSigAlgName(), Preferences.DEBUG_COMMS);

        JFileChooser chooser = new JFileChooser(System.getProperty("user.home"));
        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        chooser.setDialogTitle("Save Certificate");

        Point p = this.getLocation();
        p.x += 300;
        p.y += 300;
        chooser.setLocation(p);

        int returnVal = chooser.showDialog(this, "Save");
        if (returnVal == JFileChooser.APPROVE_OPTION) {
          FileOutputStream fout = new FileOutputStream(chooser.getSelectedFile());
          fout.write(cert.getEncoded());
          fout.close();
          //System.err.println("Certificate saved to: " +
          //                   chooser.getSelectedFile().getAbsolutePath());
        }
        else {
          return;
        }

        X509Certificate[] certChain = new X509Certificate[1];
        certChain[0] = cert;

        ks.setKeyEntry(username, privateKey, passchar, certChain);
        if (!saveKeyStore()) {
          System.err.println("KeyStore not saved!");
        }
      }
      // Something went wrong
      catch (Exception e) {
        e.printStackTrace();
      }
    }
    catch (NoSuchAlgorithmException ex) {

    }
  }

  /**
   * loads the keystore into memory
   * @return success on loading
   */
  private boolean loadKeyStore() {
    char[] passchar = keyPasswd.toCharArray();
    try {
      ks = KeyStore.getInstance("JKS");
    }
    catch (KeyStoreException kex) {
      System.err.println(kex.toString());
      return false;
    }

    try {
      FileInputStream fis = new FileInputStream(keyStore);

      ks.load(fis, passchar);
      return true;

    }
    catch (FileNotFoundException e) {
      try {
        ks.load(null, passchar);
        return true;
      }
      catch (Exception e2) {
        return false;
      }
    }
    catch (IOException e) {
      System.err.println(e.toString());
      return false;
    }
    catch (CertificateException e) {
      System.err.println(e.toString());
      return false;
    }
    catch (NoSuchAlgorithmException e) {
      System.err.println(e.toString());
      return false;
    }
  }

  /**
   * saves the keystore to a file
   * @return success on saving
   */
  private boolean saveKeyStore() {

    try {
      FileOutputStream fout = new FileOutputStream(keyStore);
      ks.store(fout, keyPasswd.toCharArray());
      return true;
    }
    catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }

  public void valueChanged(ListSelectionEvent e) {
    Object source = e.getSource();

    if (source.equals(serverTable.getSelectionModel())) {
      serverTable.repaint();

      if (serverTable.getSelectionModel().getMinSelectionIndex() !=
          serverTable.getSelectionModel().getMaxSelectionIndex()) {
        setEnabled(edit, false);
        setEnabled(delete, false);
        setEnabled(set, false);
      }
      else {
        setEnabled(edit, true);
        setEnabled(delete, true);
        setEnabled(set, true);
      }
    }
  }

  public void mouseClicked(MouseEvent e) {
    Object source = e.getSource();
    if (source.equals(serverTable) && e.getClickCount() == 2) {
      ActionEvent ev;
      try {
        ev = new ActionEvent(set, ActionEvent.ACTION_PERFORMED, "SetAs");
      }
      catch (OutOfMemoryError error) {
        MipavUtil.displayError("Out of memory: JChatFrame.actionPerformed");
        return;
      }
      serverModel.updateBulbs(serverTable.getSelectedRow());
      actionPerformed(ev);
      serverTable.repaint();
    }
  }

  public void mouseEntered(MouseEvent e) {}

  public void mouseExited(MouseEvent e) {}

  public void mousePressed(MouseEvent e) {}

  public void mouseReleased(MouseEvent e) {}


  //recursively adds file and directory paths to a Vector... to be zipped later
  private void addFilesToVector(String name, Vector vec) {
    File tempFile = new File(name);

    if (tempFile.isDirectory()) {
      String[] fileList = tempFile.list();
      for (int i = 0; i < fileList.length; i++) {
        addFilesToVector(name + File.separator + fileList[i], vec);
      }
      vec.add(0, name);
    }
    else {
      vec.add(name);
    }

  }

  /**
   *   Sets the specified button to enabled or disabled,  depending on the boolean parameter.
   *   @param button  button to set
   *   @param enabled true is enabled, false if disabled
   */
  private void setEnabled(JButton button, boolean enabled) {
    button.setEnabled(enabled);
  }

  /**
       *   A helper method for adding a component using GridBagLayout, so we don't have
   *    to set up the x, y, width, and height over and over again.
   *
   *   @param x    GridBagConstraints.gridx
   *   @param y    GridBagConstraints.gridy
   *   @param w    GridBagContraints.gridwidth
   *   @param h    GridBagConstraints.gridheight
   *   @return     the grid bag constraints
   *   @see        GridBagConstraints
   */
  private GridBagConstraints setGBC(int x, int y, int w, int h) {
    GridBagConstraints gbc;

    try {
      gbc = new GridBagConstraints();
    }
    catch (OutOfMemoryError error) {
      MipavUtil.displayError("Out of memory: JChatFrame.setGBC");
      return null;
    }

    gbc.gridx = x;
    gbc.gridy = y;
    gbc.gridwidth = w;
    gbc.gridheight = h;
    return gbc;
  }

  /**
   *   Makes the .preferences string out of the array
   *   @param stuff  array to make the string from
   *   @return       the string with the array items separated by semicolons
   */
  private String makeString(String[] stuff) {
    String newString = "";

    for (int i = 0; i < stuff.length; i++) {
      newString = newString + stuff[i].trim() + ";";
    }
    return newString;
  }

  /**
   * received text from server is processed and or printed
   * @param message (from server)
   */
  public void println(String message) {
    // message from server saying there is someone wanting to send files...
    // so open up a filereceiver and start getting em
    if (message.startsWith("FILESEND")) {
      try {
        FileReceiver fr = new FileReceiver(this, defaultReceiveDirectory, xferPort);
        fr.start();
      }
      catch (IOException ioex) {
        System.err.println(ioex.toString());
      }
    }
    // message from server that has all connected users and their respective IPs
    else if (message.startsWith("CLIENTLIST")) {
      StringTokenizer tokens = new StringTokenizer(message);
      tokens.nextToken();
      userList.clear();
      while (tokens.hasMoreTokens()) {
        String usern = tokens.nextToken();
        String ip = tokens.nextToken();
        if (! (ip.equals(userIP) && usern.equals(username))) {
          userList.add(usern);
          userList.add(ip);
        }
      }
    }
    // message from server notifying all clients that server is stopping
    else if (message.startsWith("SERVERDISCONNECT")) {
      MipavUtil.displayWarning("Server has stopped");
      textArea.append("Server has stopped.  You are logged out." + "\n");
      client.finalize();
      client = null;
      setOnline(false);
    }
    // message from server saying it won't accept the connection because there is
    // already a user with the same name connected
    else if (message.equals("CLIENTBOOT")) {
      setOnline(false);
    }
    else {
      textArea.append(message + "\n");
    }
  }

  /**
   * sets the chatframe to appear online or offline (with correct options)
   * @param connected (is online... or offline)
   */
  private void setOnline(boolean connected) {
    if (connected) {
      sendText.setEnabled(true);
      inputText.setEnabled(true);
      reconnect.setEnabled(false);
      listUsers.setEnabled(true);
      disconnect.setEnabled(true);
      changeUsername.setEnabled(false);
    }
    else {
      //System.err.println("client is null? " + (client == null));
      if (client != null) {
        client.finalize();
        client = null;
      }
      sendText.setEnabled(false);
      inputText.setEnabled(false);
      reconnect.setEnabled(true);
      listUsers.setEnabled(false);
      disconnect.setEnabled(false);
      userList.clear();
      changeUsername.setEnabled(true);
    }
  }

  /**
   *   Sets border painted or not painted depending on if the
   *   button was selected or deselected.
   *   @param event    Event that triggered this function.
   */
  public void itemStateChanged(ItemEvent event) {
    ( (AbstractButton) event.getSource()).setBorderPainted(event.getStateChange() ==
        ItemEvent.SELECTED);
  }

  /**
   *   Re-create the image and header info by reacting to
   *   each selection on the tree.  On selection, the image
   *   is read in again and constructed, and the table is
   *   cleared and the new header info is posted to the table.
   *   Note: no check has been made to remove child-nodes
   *   from a selection when the parent has been selected.
   *   @param e    Event that triggered this function.
   */
  public void valueChanged(TreeSelectionEvent e) {
    DefaultMutableTreeNode node = (DefaultMutableTreeNode) directoryTree.
        getLastSelectedPathComponent();
    if (node == null)
      return;

    if (! ( (ViewFileTreeNode) node).isDirectory() && showPreview) {
      FileInfoBase fileInfo = buildImage( ( (ViewFileTreeNode) node).getName(),
                                         ( (ViewFileTreeNode) node).
                                         getDirectory() + File.separatorChar);
      if (fileInfo == null)
        return;
      primaryModel.setRowCount(0);
      secondaryModel.setRowCount(0);
      otherLabel.setVisible(false);
      String[] info = new String[] {
          "", ""};
      for (int i = 0; i < fileInfo.getExtents().length; i++) {
        info[0] = "Dimension " + i;
        info[1] = Integer.toString(fileInfo.getExtents()[i]);
        primaryModel.addRow(info);
      }

      info[0] = "Type";
      info[1] = ModelStorageBase.getBufferTypeStr(fileInfo.getDataType());
      primaryModel.addRow(info);

      info[0] = "Max";
      info[1] = Double.toString(fileInfo.getMax());
      primaryModel.addRow(info);

      info[0] = "Min";
      info[1] = Double.toString(fileInfo.getMin());
      primaryModel.addRow(info);

      info[0] = "Modality";
      info[1] = FileInfoBase.getModalityStr(fileInfo.getModality());
      primaryModel.addRow(info);

      info[0] = "Orientation";
      info[1] = FileInfoBase.getImageOrientationStr(fileInfo.
          getImageOrientation());
      primaryModel.addRow(info);

      float[] resolutions;
      resolutions = fileInfo.getResolutions();
      int[] measure;
      measure = fileInfo.getUnitsOfMeasure();
      for (int i = 0; i < fileInfo.getExtents().length; i++) {
        if (resolutions[i] > 0.0) {
          info[0] = "Pixel resolution " + i;
          info[1] = Float.toString(resolutions[i]) + " " +
              FileInfoBase.getUnitsOfMeasureStr(measure[i]);
          primaryModel.addRow(info);
        } // end of if (resolutions[i] > 0.0)
      } // for (i=0; i < 5; i++)

      info[0] = "Endianess";
      if (fileInfo.getEndianess() == FileBase.LITTLE_ENDIAN) {
        info[1] = "Little Endian";
      }
      else {
        info[1] = "Big Endian";
      }

      if (fileInfo.getFileFormat() == FileBase.DICOM) {
        otherLabel.setVisible(true);
        JDialogFileInfoDICOM.showTags(secondaryModel, (FileInfoDicom) fileInfo, false);
      }
    }
  }

  protected FileInfoBase buildImage(String fileName, String directory) {
      FileIO io = new FileIO();
      FileInfoBase fileInfo = null;

      if (img != null) {
          imagePanel.remove(img);
          img.dispose(true);
          img = null;
      }
      else if (thumbnail != null) {
          imagePanel.remove(thumbnail);
          thumbnail.finalize();
      }

      if (showXMLThumbnail &&
          (fileName.endsWith(".xml") || fileName.endsWith(".XML"))) {
          FileImageXML xmlTemp = io.readXMLThumbnail(fileName, directory);

          if (xmlTemp != null) {
              fileInfo = xmlTemp.getFileInfo();
              thumbnail = xmlTemp.getThumbnail();
              imagePanel.add(thumbnail, BorderLayout.CENTER);
              imagePanel.validate();
              imagePanel.repaint();
              //thumbnail.setImgSize(imagePanel.getBounds().width, imagePanel.getBounds().height);
              thumbnail.setBrightnessContrast(brightness, contrast);
              return fileInfo;
          }
      }

    ModelImage image = io.readOneImage(fileName, directory);
    if (image == null)
      return null;
    int[] extents = new int[] {
        image.getExtents()[0], image.getExtents()[1]};

    if (img != null) {
      imagePanel.remove(img);
    }
    img = new ViewJComponentPreviewImage(image, extents, this);
    imagePanel.add(img);
    if (image.getNDims() > 2) {
      img.createImg(image.getExtents()[2] / 2);
    }
    else {
      img.createImg(0);
    }
    img.setImgSize(imagePanel.getBounds().width, imagePanel.getBounds().height);
    imagePanel.validate();
    imagePanel.repaint();
    img.paintComponent(img.getGraphics());

    fileInfo = (FileInfoBase) image.getFileInfo(0).clone();
    image.disposeLocal();
    image = null;
    return fileInfo;

  }

  /**
   *   Tells the component image that the size of the image panel
   *   has changed, then repaints the component image.
   *   @param event    Event that triggered this function.
   */
  public void componentResized(ComponentEvent event) {
    if (img != null) {
      if (imagePanel != null) {
        img.setImgSize(imagePanel.getBounds().width,
                       imagePanel.getBounds().height);
      }
      else {
        img.setImgSize(400, 200);
      }
      img.paintComponent(img.getGraphics());
    }
  }

  /**
   *   Unchanged.
   */
  public void componentMoved(ComponentEvent event) {}

  /**
   *   Unchanged.
   */
  public void componentShown(ComponentEvent event) {}

  /**
   *   Unchanged.
   */
  public void componentHidden(ComponentEvent event) {}

  /**
   *   Unchanged.
   */
  public void treeCollapsed(TreeExpansionEvent tee) {}

  /**
   *  Expands tree node in file tree.
   *  On expansion, the tree queries the selected node;
   *  if a selected node has had its children previously
   *  added, then no nodes will be added, but the tree
   *  will display the previously added children.
   *  Otherwize, the node will add nodes which will be
   *  displayed; each node will be marked as adding
   *  only directories as child-nodes.
   *  @param tee  Event that triggered this function.
   */
  public void treeExpanded(TreeExpansionEvent tee) {
    TreePath path = tee.getPath();
    node = (ViewFileTreeNode) path.getLastPathComponent();
    file = (File) node.getUserObject();
    if (!node.isExplored()) {
      DefaultTreeModel model = (DefaultTreeModel) directoryTree.getModel();
      node.explore(imageFilter);
      model.nodeStructureChanged(node);
    }
  }

  /**
   *    sets values based on knob along slider
   *    @param e  event that triggered this function
   */
  public void stateChanged(ChangeEvent e) {
    Object source = e.getSource();

    if (source == brightSlider) {
      brightness = brightSlider.getValue();
      current.setText(String.valueOf(brightness));
      //if the image is not a thumbnail and it is larger than 1024 x 768, do not adjust while sliding
      if (img != null && img.getImageSize() > 786432 && brightSlider.getValueIsAdjusting()) {
          return;
      }
      // Change only the brightness and contrast of the current slice
      if (img != null) {
        img.setSliceBrightness(brightness, contrast);
      }
      else if (thumbnail != null) {
          thumbnail.setBrightnessContrast(brightness, contrast);
      }
    }
    else if (source == contSlider) {
      contrast = (float) Math.pow(10.0, contSlider.getValue() / 200.0);
      current2.setText(String.valueOf(nfc.format(contrast)));
      //if the image is not a thumbnail and it is larger than 1024 x 768, do not adjust while sliding
        if (img != null && img.getImageSize() > 786432 && contSlider.getValueIsAdjusting()) {
            return;
        }

      // Change only the brightness and contrast of the current slice
      if (img != null) {
        img.setSliceBrightness(brightness, contrast);
      }
      else if (thumbnail != null) {
          thumbnail.setBrightnessContrast(brightness, contrast);
      }
    }
    else if (source == tabbedPane) {
      if (tabbedPane.getSelectedIndex() == 0 ||
          tabbedPane.getSelectedIndex() == 1) {
        setJMenuBar(menuBar);
      }
      else {
        setJMenuBar(menuBar2);
      }
    }
  } // end stateChanged()



  /**
   *   Called by the component image to get the real-time
   *   size of the panel before centering.
   *   @return The size of the panel.
   */
  public Dimension getPanelSize() {
    return new Dimension(imagePanel.getBounds().width,
                         imagePanel.getBounds().height);
  }

  protected TitledBorder buildTitledBorder(String title) {
    return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT,
                            TitledBorder.CENTER, MipavUtil.font12B, Color.black);
  }

  /**
   * <p>Title: UsernameDialog </p>
   * <p>Description: Dialog to allow changing of username </p>
   * <p>Copyright: Copyright (c) 2003</p>
   * <p>Company: </p>
   * @author not attributable
   * @version 1.0
   */
  private class UsernameDialog
      extends JDialog
      implements ActionListener {
    private JTextField usernameField;
    private JOptionPane optionPane;

    private JButton okayButton;
    private JButton cancelButton;

    private String newName = null;
    private boolean okay = false;

    public UsernameDialog(Frame aFrame, String oldName) {
      super(aFrame, true);
      setTitle("Enter username");
      setSize(270, 100);
      setResizable(false);
      //Handle window closing correctly.
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

      okayButton = new JButton("OK");
      okayButton.addActionListener(this);
      okayButton.setActionCommand("OK");
      okayButton.setFont(MipavUtil.font12B);
      cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(this);
      cancelButton.setActionCommand("Cancel");
      cancelButton.setFont(MipavUtil.font12B);

      JPanel buttonPanel = new JPanel();
      buttonPanel.add(okayButton);
      buttonPanel.add(cancelButton);

      JLabel usernameLabel = new JLabel("Enter Username");
      usernameLabel.setFont(MipavUtil.font12B);
      usernameField = new JTextField(10);
      usernameField.setText(oldName);
      JPanel usernamePanel = new JPanel();
      usernamePanel.add(usernameLabel);
      usernamePanel.add(usernameField);

      getContentPane().add(usernamePanel, BorderLayout.CENTER);
      getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }

    public void actionPerformed(ActionEvent event) {
      String command = event.getActionCommand();
      if (command.equals("OK")) {
        newName = new String(usernameField.getText());
        okay = true;
      }
      this.setVisible(false);
    }

    public boolean isOkay() {
      return okay;
    }

    public String getNewName() {
      int index = newName.indexOf(" ");
      if (index != -1) {
        StringTokenizer tokens = new StringTokenizer(newName);
        String temp = tokens.nextToken();
        while (tokens.hasMoreTokens()) {
          temp += "_" + tokens.nextToken();
        }
        newName = temp;
      }
      return newName;
    }

  }

  /**
   * <p>Title: UsernameDialog </p>
   * <p>Description: Dialog to allow changing of username </p>
   * <p>Copyright: Copyright (c) 2003</p>
   * <p>Company: </p>
   * @author not attributable
   * @version 1.0
   */
  private class PortDialog
      extends JDialog
      implements ActionListener {
    private JTextField portField;
    private JOptionPane optionPane;

    private JButton okayButton;
    private JButton cancelButton;

    private int port = 0;
    private boolean okay = false;

    public PortDialog(Frame aFrame, int oldPort, boolean server) {
      super(aFrame, true);

      if (server)
        setTitle("Enter new server port");
      else
        setTitle("Enter new file xfer port");
      setSize(270, 100);
      setResizable(false);
      //Handle window closing correctly.
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

      okayButton = new JButton("OK");
      okayButton.addActionListener(this);
      okayButton.setActionCommand("OK");
      okayButton.setFont(MipavUtil.font12B);
      cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(this);
      cancelButton.setActionCommand("Cancel");
      cancelButton.setFont(MipavUtil.font12B);

      JPanel buttonPanel = new JPanel();
      buttonPanel.add(okayButton);
      buttonPanel.add(cancelButton);

      JLabel usernameLabel = new JLabel("Enter port");
      usernameLabel.setFont(MipavUtil.font12B);
      portField = new JTextField(4);
      portField.setText(Integer.toString(oldPort));
      MipavUtil.makeNumericsOnly(portField, false, false);
      JPanel portPanel = new JPanel();
      portPanel.add(usernameLabel);
      portPanel.add(portField);

      getContentPane().add(portPanel, BorderLayout.CENTER);
      getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }

    public void actionPerformed(ActionEvent event) {
      String command = event.getActionCommand();
      if (command.equals("OK")) {
        try {
          port = Integer.parseInt(portField.getText());
          if (port < 1024 || port > 9999) {
            MipavUtil.displayError("Port must be between 1024 and 9999");
            return;
          }
        }
        catch (NumberFormatException e) {
          okay = false;
          return;
        }
        okay = true;
      }
      this.setVisible(false);
    }

    public boolean isOkay() {
      return okay;
    }

    public int getPort() {
      return port;
    }
  }


  /**
   *
   * <p>Title: Password Dialog</p>
   * <p>Description: Dialog to allow entering of Keystore password </p>
   * <p>Copyright: Copyright (c) 2003</p>
   * <p>Company: </p>
   * @author not attributable
   * @version 1.0
   */
  private class PasswordDialog
      extends JDialog
      implements ActionListener {

    private JPasswordField passwordField;
    private JOptionPane optionPane;

    private JButton okayButton;
    private JButton cancelButton;

    private String password = null;
    private boolean okay = false;

    public PasswordDialog(Frame aFrame) {
      super(aFrame, true);
      setTitle("Enter KeyStore/TrustStore Password");
      setSize(270, 100);
      setResizable(false);

      //Handle window closing correctly.
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

      okayButton = new JButton("OK");
      okayButton.addActionListener(this);
      okayButton.setActionCommand("OK");
      okayButton.setFont(MipavUtil.font12B);
      cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(this);
      cancelButton.setActionCommand("Cancel");
      cancelButton.setFont(MipavUtil.font12B);

      JPanel buttonPanel = new JPanel();
      buttonPanel.add(okayButton);
      buttonPanel.add(cancelButton);

      JLabel passwdLabel = new JLabel("Enter Password");
      passwdLabel.setFont(MipavUtil.font12B);
      passwordField = new JPasswordField(10);
      JPanel passwdPanel = new JPanel();
      passwdPanel.add(passwdLabel);
      passwdPanel.add(passwordField);

      getContentPane().add(passwdPanel, BorderLayout.CENTER);
      getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }

    public void actionPerformed(ActionEvent event) {
      String command = event.getActionCommand();
      if (command.equals("OK")) {
        password = new String(passwordField.getPassword());
        okay = true;
      }
      this.setVisible(false);
    }

    public boolean isOkay() {
      return okay;
    }

    public String getPassword() {
      return password;
    }
  }

  /**
   *
   * <p>Title: SendToUserDialog </p>
   * <p>Description: dialog to allow selection of user for file-transfer</p>
   * <p>Copyright: Copyright (c) 2003</p>
   * <p>Company: </p>
   * @author not attributable
   * @version 1.0
   */
  private class SendToUserDialog
      extends JDialog
      implements ActionListener {
    private JOptionPane optionPane;
    private ViewTableModel model;
    private JTable table;

    private JButton okayButton;
    private JButton cancelButton;

    private String ip = null;
    private String [] ips = null;
    private String[] names = null;

    private String name = null;
    private boolean okay = false;

    public SendToUserDialog(Frame aFrame) {
      super(aFrame, true);
      setTitle("Select User");
      setSize(300, 150);
      setResizable(false);
      //Handle window closing correctly.
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

      okayButton = new JButton("OK");
      okayButton.addActionListener(this);
      okayButton.setActionCommand("OK");
      okayButton.setFont(MipavUtil.font12B);
      cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(this);
      cancelButton.setActionCommand("Cancel");
      cancelButton.setFont(MipavUtil.font12B);

      JPanel buttonPanel = new JPanel();
      buttonPanel.add(okayButton);
      buttonPanel.add(cancelButton);

      model = new ViewTableModel();
      table = new JTable(model);

      model.addColumn("Username");
      model.addColumn("IP Address");
      table.setAutoResizeMode(table.AUTO_RESIZE_ALL_COLUMNS);
      table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION );
      table.getTableHeader().setReorderingAllowed(false);

      Object rowData[] = new Object[2];
      for (int i = 0; i < userList.size(); i += 2) {
        rowData[0] = (String) userList.elementAt(i);
        rowData[1] = (String) userList.elementAt(i + 1);
        model.addRow(rowData);
      }

      JScrollPane scrollP = new JScrollPane(table,
                                            JScrollPane.
                                            VERTICAL_SCROLLBAR_AS_NEEDED,
                                            JScrollPane.
                                            HORIZONTAL_SCROLLBAR_AS_NEEDED);

      getContentPane().add(scrollP, BorderLayout.CENTER);
      getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }

    public void actionPerformed(ActionEvent event) {
      String command = event.getActionCommand();
      if (command.equals("OK")) {
        int [] rows = table.getSelectedRows();

        if (rows.length < 1) {
          okay = false;
          MipavUtil.displayError("Please select user{s)");
        }
        else {
          ips = new String[rows.length];
          names = new String[rows.length];

          for (int i = 0; i < rows.length; i++) {
            ips[i] = (String) model.getValueAt(rows[i], 1);
            names[i] = (String) model.getValueAt(rows[i], 0);
            okay = true;
            this.setVisible(false);
          }
        }
      }
      else if (command.equals("Cancel")) {
        okay = false;
        this.setVisible(false);
      }
    }

    public boolean isOkay() {
      return okay;
    }

    public String [] getIPs() {
      return ips;
    }

    public String [] getNames() {
      return names;
    }
  }
}
