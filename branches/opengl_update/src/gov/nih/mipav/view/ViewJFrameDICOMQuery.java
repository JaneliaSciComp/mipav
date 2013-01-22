package gov.nih.mipav.view;


import gov.nih.mipav.model.dicomcomm.*;
import gov.nih.mipav.model.file.FileUtility;

import gov.nih.mipav.view.dialogs.JDialogServer;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.TableColumn;


/**
 * GUI for DICOM Query and retreival from DICOM compliant servers. There are three tabbed panels in the GUI. The first
 * one is DICOM Query Panel. It has a panel for entering the query information, a panel for setting the dates of the
 * query, a table that displays the results of the query, and a table that displays the status of any move requests. The
 * user may either double click on the query table, hit the "Send Query" button, or hit the "Down" button to send a
 * query to the next level. To go up a level, the user can click the "Up" button. To move an image from any level, the
 * user can select the item to move on and click the "Move Image" button. At the images level, double clicking on an
 * image will move that image. When a move request is sent, it is put in a separate thread. That thread updates the
 * message table as to the progress of the move request.
 * 
 * @version 1.0
 * @author Neva Cherniavsky
 * @author Matthew McAuliffe, Ph.D.
 * @see DICOM_Query
 * @see DICOM_Move
 */
public class ViewJFrameDICOMQuery extends JFrame implements ActionListener, ListSelectionListener, MouseListener,
        ChangeListener, ComponentListener, WindowListener, FocusListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3899062842723888728L;

    /** DOCUMENT ME! */
    private static final int DEFAULT_MIN_YEAR = 1970;

    /** DOCUMENT ME! */
    private static final int PATIENT_LEVEL = 0;

    /** DOCUMENT ME! */
    private static final int STUDY_LEVEL = 1;

    /** DOCUMENT ME! */
    private static final int SERIES_LEVEL = 2;

    /** DOCUMENT ME! */
    private static final int IMAGE_LEVEL = 3;

    /** DOCUMENT ME! */
    private static final Dimension MINIMUM_SIZE = new Dimension(550, 500);

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton browseButton;

    /** DOCUMENT ME! */
    private JLabel currentDateL;

    /** DOCUMENT ME! */
    private DICOM_Query dicomQuery;

    /** DOCUMENT ME! */
    private JComboBox endYearBox, endMonthBox, endDayBox;

    /** DOCUMENT ME! */
    private static final Font font12 = MipavUtil.font12;

    /** DOCUMENT ME! */
    private static final Font font12B = MipavUtil.font12B;

    /** DOCUMENT ME! */
    private JPanel listPanel;

    /** DOCUMENT ME! */
    private DICOMDisplayer messageTable;

    /** DOCUMENT ME! */
    private static final String[] monthString = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
            "Nov", "Dec"};

    /** DOCUMENT ME! */
    private Hashtable<String, MoveRequestInfo> moveRequestHash;

    /** DOCUMENT ME! */
    private JTextField ptNameText, ptIDText, studyNOText, physText;

    /** DOCUMENT ME! */
    private JLabel queryLev;

    /** DOCUMENT ME! */
    private int queryLevel = ViewJFrameDICOMQuery.PATIENT_LEVEL;

    /** DOCUMENT ME! */
    private int queryMsgID;

    private JPanel aboutPanel;

    /** DOCUMENT ME! */
    private DICOM_PDUService queryPDU;

    /** DOCUMENT ME! */
    private JTable queryResultTable;

    /** DOCUMENT ME! */
    private ViewTableModel queryTableModel;

    /** DOCUMENT ME! */
    private JButton refreshButton;

    /** DOCUMENT ME! */
    private JPanel rootQueryPanel;

    /** DOCUMENT ME! */
    private JButton sendButton;

    /** DOCUMENT ME! */
    private JComboBox sendDestCBox;

    /** DOCUMENT ME! */
    private ViewTableModel sendModel;

    /** DOCUMENT ME! */
    private Thread sendQueryThread;

    /** DOCUMENT ME! */
    private JButton sendStatusClearButton;

    /** DOCUMENT ME! */
    private JTextArea sendStatusTArea;

    /** DOCUMENT ME! */
    private JTable sendTable;

    /** DOCUMENT ME! */
    private String seriesInstanceUID = "";

    /** DOCUMENT ME! */
    private ViewTableModel serverModel;

    /** DOCUMENT ME! */
    private JTable serverTable;

    /** Buttons used for the host tab */
    private JButton set, activateStore, createStore, editStore, deleteStore, setStore, cancel, help1, help2, help3,
            help4;

    /** DOCUMENT ME! */
    private String SOPInstanceUID = "";

    /** DOCUMENT ME! */
    private JTextField sourceTextF = null;

    /** DOCUMENT ME! */
    private Calendar startCalendar, endCalendar, todayCalendar;

    /** DOCUMENT ME! */
    private JComboBox startYearBox, startMonthBox, startDayBox;

    /** DOCUMENT ME! */
    private ViewTableModel storageModel;

    /** DOCUMENT ME! */
    private JTable storageTable;

    /** DOCUMENT ME! */
    private String studyInstanceUID = "";

    /** DOCUMENT ME! */
    private JPanel subQueryPanel;

    /** DOCUMENT ME! */
    private JTabbedPane tabbedPane;

    /** DOCUMENT ME! */
    private JButton testButton;

    /** DOCUMENT ME! */
    private JTextArea textArea;

    /** DOCUMENT ME! */
    private DICOM_UID[] uids;

    /** DOCUMENT ME! */
    private JButton up, down, cancelQ, send, move, create, edit, delete;

    /** DOCUMENT ME! */
    private JButton upDirButton;

    private final JPanel sendPanel;

    private static final ViewUserInterface userInterface = ViewUserInterface.getReference();

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructs three tabbed panels - DICOM Query, Host and Help panels.
     */
    public ViewJFrameDICOMQuery() {
        Color back;
        BorderLayout border;
        GridLayout grid;
        Insets insets;
        JScrollPane scrollPane;
        String IPAddress = null;

        try {
            back = new Color(160, 160, 160);
            tabbedPane = new JTabbedPane();
            border = new BorderLayout();
            grid = new GridLayout(1, 1);
            aboutPanel = new JPanel();
            textArea = new JTextArea();
            insets = new Insets(5, 5, 5, 5);
            moveRequestHash = new Hashtable<String, MoveRequestInfo>();
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery constructor");
            sendPanel = null;
            return;
        }

        try {
            IPAddress = InetAddress.getLocalHost().getHostAddress();
        } catch (final UnknownHostException error) {}

        setTitle("DICOM Communication Panel - IP address = " + IPAddress);
        setSize(600, 650);
        setResizable(true);
        getContentPane().setLayout(border);
        setLocation(200, 50);
        setBackground(back);

        try {
            setIconImage(MipavUtil.getIconImage("connect.gif")); // add this
            // line to
            // change
            // icon for
            // frame!!!!!
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }

        tabbedPane.setFont(ViewJFrameDICOMQuery.font12B);
        tabbedPane.addChangeListener(this);
        final JPanel queryPanel = buildQueryPanel();
        tabbedPane.addTab("QR Client", null, queryPanel);

        final JPanel hostsPanel = buildHostPanel();
        sendPanel = buildSendPanel();

        tabbedPane.addTab("Send", null, sendPanel);
        setupSendTab();

        tabbedPane.addTab("Hosts", null, hostsPanel);

        aboutPanel.setLayout(grid);
        textArea.setBackground(Color.lightGray);
        textArea.setEditable(false);
        textArea.setFont(ViewJFrameDICOMQuery.font12);
        textArea.setMargin(insets);
        buildHelpText();

        try {
            scrollPane = new JScrollPane(textArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery constructor");

            return;
        }

        aboutPanel.add(scrollPane, "Center");
        tabbedPane.addTab("Help", null, aboutPanel);

        tabbedPane.setSelectedIndex(0);
        getContentPane().add(tabbedPane, "Center");
        tabbedPane.validate();
        validate();
        addComponentListener(this);

        addWindowListener(this);
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        setVisible(true);

    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     * 
     * @param event event that triggered function
     */
    public void actionPerformed(final ActionEvent event) {

        // Object source = event.getSource();
        String s;
        final String command = event.getActionCommand();
        final Object source = event.getSource();

        s = "";

        if (command.equals("ClearPtInfo")) {
            ptNameText.setText(s);
            ptIDText.setText(s);
            studyNOText.setText(s);
            physText.setText(s);
        } else if (command.equals("CancelQuery")) {
            dicomQuery.sendFindCancelRQ(queryMsgID, queryPDU);
            send.setEnabled(true);
            cancelQ.setEnabled(false);
        } else if (command.equals("SendQuery")) {

            // this.setCursor(Cursor.WAIT_CURSOR);
            send.setEnabled(false);
            cancelQ.setEnabled(true);

            getStartCalendar();
            getEndCalendar();

            if ( ( (getPtName())).equals("") && ( (getPtID())).equals("") && ( (getStudyNo())).equals("")
                    && ( (getPhysName())).equals("")) {
                final int option = JOptionPane.showConfirmDialog(this, "Query on all studies on all patients from "
                        + startMonthBox.getSelectedItem() + "-" + startDayBox.getSelectedItem() + "-"
                        + startYearBox.getSelectedItem() + "\nto " + endMonthBox.getSelectedItem() + "-"
                        + endDayBox.getSelectedItem() + "-" + endYearBox.getSelectedItem() + "?", "Confirm query",
                        JOptionPane.YES_NO_OPTION);

                if (option == JOptionPane.NO_OPTION) {
                    send.setEnabled(true);
                    cancelQ.setEnabled(false);

                    return;
                }

            }

            if ( ( (getPtID())).equals("")) {
                sendQuery(ViewJFrameDICOMQuery.PATIENT_LEVEL);
            } else if (studyInstanceUID.equals("")) {
                sendQuery(ViewJFrameDICOMQuery.STUDY_LEVEL);
            } else if (seriesInstanceUID.equals("")) {
                sendQuery(ViewJFrameDICOMQuery.SERIES_LEVEL);
            } else if (SOPInstanceUID.equals("")) {
                sendQuery(ViewJFrameDICOMQuery.IMAGE_LEVEL);
            }

            // this.setCursor(Cursor.DEFAULT_CURSOR);
            // cancelQ.setEnabled(false);
            // tabbedPane.paintAll(tabbedPane.getGraphics());
        } else if (command.equals("Move")) {
            final int begin = queryResultTable.getSelectionModel().getMinSelectionIndex();
            final int end = queryResultTable.getSelectionModel().getMaxSelectionIndex() + 1;
            final int option = JOptionPane
                    .showConfirmDialog(
                            this,
                            "Moving images may take some time, especially\nif you are moving at the patient, study,\nor series level.  Proceed anyway?",
                            "Confirm move", JOptionPane.YES_NO_OPTION);

            if (option == JOptionPane.YES_OPTION) {

                for (int i = begin; i < end; i++) {

                    // SOPInstanceUID = uids[i].getSOPInstanceUID();
                    // sendMoveRequest(IMAGE_LEVEL);
                    sendMoveRequest(queryLevel);
                }

                if (DICOMDisplayer.getSucceeded()) {
                    JOptionPane.showMessageDialog(this, "Move request successful", "Success",
                            JOptionPane.INFORMATION_MESSAGE);
                }
            }
        } else if (command.equals("ResultUp")) {

            if (queryLevel == ViewJFrameDICOMQuery.STUDY_LEVEL) {
                ptNameText.setText(lastName(ptNameText.getText()));
                studyInstanceUID = "";
                seriesInstanceUID = "";
                sendQuery(ViewJFrameDICOMQuery.PATIENT_LEVEL);
            } else if (queryLevel == ViewJFrameDICOMQuery.SERIES_LEVEL) {
                studyInstanceUID = "";
                seriesInstanceUID = "";
                sendQuery(ViewJFrameDICOMQuery.STUDY_LEVEL);
            } else if (queryLevel == ViewJFrameDICOMQuery.IMAGE_LEVEL) {
                seriesInstanceUID = "";
                sendQuery(ViewJFrameDICOMQuery.SERIES_LEVEL);
            }
        } else if (command.equals("TodayRButton")) {
            startCalendar = assignCalendar(todayCalendar, startCalendar);
            setStartCalendar(startCalendar);
        } else if (command.equals("One WeekRButton")) {
            startCalendar = assignCalendar(todayCalendar, startCalendar);
            startCalendar = getDaysBefore(startCalendar, 7);
            setStartCalendar(startCalendar);
        } else if (command.equals("One MonthRButton")) {
            startCalendar = assignCalendar(todayCalendar, startCalendar);
            startCalendar = getMonthsBefore(startCalendar, 1);
            setStartCalendar(startCalendar);
        } else if (command.equals("Three MonthRButton")) {
            startCalendar = assignCalendar(todayCalendar, startCalendar);
            startCalendar = getMonthsBefore(startCalendar, 3);
            setStartCalendar(startCalendar);
        } else if (command.equals("Six MonthRButton")) {
            startCalendar = assignCalendar(todayCalendar, startCalendar);
            startCalendar = getMonthsBefore(startCalendar, 6);
            setStartCalendar(startCalendar);
        } else if (command.equals("One YearRButton")) {
            startCalendar = assignCalendar(todayCalendar, startCalendar);
            startCalendar = getYearsBefore(startCalendar, 1);
            setStartCalendar(startCalendar);
        } else if (command.equals("StartYear")) {
            setStartYear(getStartYear());
        } else if (command.equals("StartMonth")) {
            setStartMonth(getStartMonth());
        } else if (command.equals("StartDay")) {
            setStartDay(getStartDay());
        } else if (command.equals("EndYear")) {
            setEndYear(getEndYear());
        } else if (command.equals("EndMonth")) {
            setEndMonth(getEndMonth());
        } else if (command.equals("EndDay")) {
            setEndDay(getEndDay());
        } else if (command.equals("Create")) {
            String[] values;
            String key;
            JDialogServer createDialog;
            ViewTableModel model;

            if (event.getSource().equals(create)) {

                try {
                    createDialog = new JDialogServer(this, "Create Server", true);
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.actionPerformed");

                    return;
                }

                key = Preferences.getNextServerKey();
                model = serverModel;
            } else {

                try {
                    createDialog = new JDialogServer(this, "Create Storage Destination", false);
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.actionPerformed");

                    return;
                }

                key = Preferences.getNextStorageKey();
                model = storageModel;
            }

            if ( !createDialog.isCancelled()) {
                values = createDialog.getValues();

                // add code here
                if (Preferences.getIP(values[0]) == null) {
                    final String value = makeString(values);
                    final Object[] rowData = {Boolean.FALSE, values[0], values[1], values[2], values[3]};
                    model.addRow(rowData);
                    Preferences.setProperty(key, value);
                    Preferences.save();
                } else {
                    MipavUtil.displayError("Duplicate AE Titles not allowed.");
                }
            }
        } else if (command.equals("Edit")) {
            int row;
            String key;
            ViewTableModel model;
            JTable table;
            JDialogServer editDialog;

            if (event.getSource().equals(edit)) {
                model = serverModel;
                table = serverTable;
                key = "Server";
            } else {
                model = storageModel;
                table = storageTable;
                key = "Storage";
            }

            row = table.getSelectedRow();

            String[] values = {(String) model.getValueAt(row, 1), (String) model.getValueAt(row, 2),
                    (String) model.getValueAt(row, 3), (String) model.getValueAt(row, 4)};

            try {

                if (event.getSource().equals(edit)) {
                    editDialog = new JDialogServer(this, "Edit Server", values, true);
                } else {
                    editDialog = new JDialogServer(this, "Edit Storage Destination", values, false);
                }
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.actionPerformed");

                return;
            }

            if ( !editDialog.isCancelled()) {

                // editDialog.setVisible(false);
                values = editDialog.getValues();

                String value = makeString(values);
                key = key + (row + 1);

                if ( ((Boolean) model.getValueAt(row, 0)).booleanValue()) {
                    value = value + "DEFAULT;";
                }

                Preferences.setProperty(key, value);

                final Object[] rowData = {model.getValueAt(row, 0), values[0], values[1], values[2], values[3]};
                model.removeRow(row);
                model.insertRow(row, rowData);

                if (event.getSource().equals(edit)) {
                    setEnabled(set, false);
                    setEnabled(delete, false);
                    setEnabled(edit, false);
                } else {
                    setEnabled(setStore, false);
                    setEnabled(deleteStore, false);
                    setEnabled(editStore, false);
                }

                Preferences.save();
                table.repaint();
            }
        } else if (command.equals("Delete")) {
            int row, option;

            if (event.getSource().equals(delete)) {
                row = serverTable.getSelectedRow();
                option = JOptionPane.showConfirmDialog(this, "Are you sure you wish to delete\nthis server?",
                        "Confirm delete", JOptionPane.YES_NO_OPTION);
            } else {
                row = storageTable.getSelectedRow();
                option = JOptionPane.showConfirmDialog(this,
                        "Are you sure you wish to delete\nthis storage destination?", "Confirm delete",
                        JOptionPane.YES_NO_OPTION);
            }

            if (option == JOptionPane.YES_OPTION) {
                String key, nextKey, value, stem;
                int i;

                i = row + 1;

                if (event.getSource().equals(delete)) {
                    stem = "Server";
                } else {
                    stem = "Storage";
                }

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

                if (event.getSource().equals(delete)) {
                    serverModel.removeRow(row);
                } else {
                    storageModel.removeRow(row);
                }
            }
        } else if (command.equals("SetAs")) {
            String key, value, newValue, stem;
            StringTokenizer tok;
            int row;
            ViewTableModel model;

            if (event.getSource().equals(set)) {
                row = serverTable.getSelectedRow();
                model = serverModel;
                key = Preferences.getDefaultServerKey();
                stem = "Server";
            } else {
                row = storageTable.getSelectedRow();
                model = storageModel;
                key = Preferences.getDefaultStorageKey();
                stem = "Storage";
            }

            // Reset the look of the table
            for (int i = 0; i < model.getRowCount(); i++) {

                if (i == row) {
                    model.setValueAt(Boolean.TRUE, i, 0);
                } else {
                    model.setValueAt(Boolean.FALSE, i, 0);
                }
            }

            // Erase the old DEFAULT from the preference file
            value = Preferences.getProperty(key);
            newValue = "";

            try {
                tok = new StringTokenizer(value, ";");
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.actionPerformed");

                return;
            }

            String port = "3000";

            for (int i = 0; i < 4; i++) {

                if (i == 3) {
                    port = tok.nextToken();
                    newValue = newValue + port + ";";
                } else {
                    newValue = newValue + tok.nextToken() + ";";
                }
            }

            Preferences.setProperty(key, newValue);

            // Make the selected row the new DEFAULT
            key = stem + (row + 1);
            value = Preferences.getProperty(key);
            newValue = value + "DEFAULT;";
            Preferences.setProperty(key, newValue);

            Preferences.save();

            serverTable.repaint();
        } else if (command.equals("ClearTable")) {

            for (final Enumeration<MoveRequestInfo> e = moveRequestHash.elements(); e.hasMoreElements();) {

                if (e.nextElement().getThread().isAlive()) {
                    MipavUtil.displayError("Can't clear table if a request is not yet completed.");

                    return;
                }
            }

            while (messageTable.getModel().getRowCount() > 0) {
                ((ViewTableModel) messageTable.getModel()).removeRow(0);
            }
        } else if (command.equals("Browse")) {
            String directory;
            JFileChooser chooser;

            try {
                chooser = new JFileChooser();

                if (sourceTextF.getText() != null) {
                    final File file = new File(sourceTextF.getText());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

                final int returnValue = chooser.showOpenDialog(this);

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    directory = String.valueOf(chooser.getSelectedFile()); // +
                    // File.separatorChar;
                } else {
                    return;
                }

                sourceTextF.setText(directory);

            } catch (final OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return;
            }

            setupSendFileListing(sourceTextF.getText());
            ((TitledBorder) (listPanel.getBorder())).setTitle(sourceTextF.getText());
            sendPanel.validate();
            sendPanel.repaint();

        } else if (command.equals("Cancel")) {
            DICOM_Move dm;
            final int row = messageTable.getSelectedRow();
            final Object messageID = messageTable.getValueAt(row, 5);
            final MoveRequestInfo info = moveRequestHash.get(messageID);

            if (info.getThread().isAlive()) {

                try {
                    dm = new DICOM_Move();
                } catch (final OutOfMemoryError e) {
                    MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.actionPerformed");

                    return;
                }

                dm.sendCancelRQ(Integer.valueOf((String) messageID).intValue(), info.getPDU());
            }

            ViewJFrameDICOMQuery.userInterface.getDICOMCatcher().setCancelled(true);
            DICOMDisplayer.setMessageType(row, DICOMDisplayer.STATUS);
            messageTable.showMessage("Cancelled");
            info.getThread().stop();
            messageTable.repaint();
            setEnabled(cancel, false);
        } else if (command.equals("UpDir")) {
            String str = null;
            str = sourceTextF.getText();

            final int index = str.lastIndexOf(File.separatorChar);

            if (index <= 0) {
                return;
            }

            str = str.substring(0, index);

            // dir = sourceTextF.getText();
            // sourceTextF.setText(sourceTextF.getText() + File.separatorChar +
            // str);
            if (setupSendFileListing(str) == false) {
                return;
            }

            sourceTextF.setText(str);
            ((TitledBorder) (listPanel.getBorder())).setTitle(sourceTextF.getText());
            sendPanel.validate();
            sendPanel.repaint();
        } else if (command.equals("RefreshDir")) {
            setupSendFileListing(sourceTextF.getText());
            sendPanel.validate();
            sendPanel.repaint();
        } else if (command.equals("SendImages")) {

            final int[] rows = sendTable.getSelectedRows();

            if (rows.length == 0) { // no row selected
                MipavUtil.displayError("Select a directory or image.");

                return;
            }
            final String fileList[] = getFiles(rows);

            for (final String element : fileList) {

                final DICOM_Store dcmStore = new DICOM_Store(element, (String) sendDestCBox.getSelectedItem(), this);

                final Thread runner = new Thread(dcmStore);
                runner.start();

                sendStatusTArea.append("Sending -- " + element + " to " + (String) sendDestCBox.getSelectedItem()
                        + "\n");
            }
        } else if (command.equals("SendClear")) {
            sendStatusTArea.setText(" ");
        } else if (command.equals("TestConnection")) {
            final DICOM_Verification verify = new DICOM_Verification((String) sendDestCBox.getSelectedItem(), this);
            verify.verify();
        } else if (command.equals("Help1")) {
            //MipavUtil.showHelp("10308");
            MipavUtil.showWebHelp("Posing_queries_and_retrieving_images"); 
        } else if (command.equals("Help2")) {
            //MipavUtil.showHelp("10309");
            MipavUtil.showWebHelp("Receiving_and_sending_image_files");
        } else if (command.equals("Help3")) {
            //MipavUtil.showHelp("10305");
            MipavUtil.showWebHelp("Sending_and_retrieving_DICOM_images#Creating.2C_editing.2C_and_deleting_servers");
        } else if (command.equals("Help4")) {
            //MipavUtil.showHelp("10304");
            MipavUtil.showWebHelp("Sending_and_retrieving_DICOM_images#Setting_up_the_hosts_table");
        } else if (command.equals("Activate")) {
            final int selected = storageTable.getSelectedRow();
            DICOM_Receiver rec;
            if (ViewJFrameDICOMQuery.userInterface.getDICOMCatcher() != null) {
                ViewJFrameDICOMQuery.userInterface.getDICOMCatcher().setStop();
            }
            if (selected == -1) {
                ViewJFrameDICOMQuery.userInterface.setDICOMCatcher(rec = new DICOM_Receiver());
            } else {
                String storageKey = new String();
                // should not store default property
                for (int i = 1; i < storageModel.getColumnCount(); i++) {
                    storageKey += storageModel.getValueAt(selected, i) + ";";
                }
                ViewJFrameDICOMQuery.userInterface.setDICOMCatcher(rec = new DICOM_Receiver(storageKey));
            }
            ViewJFrameDICOMQuery.userInterface.getMenuBuilder().setMenuItemSelected("Activate DICOM receiver",
                    rec.isAlive());
            if (rec.isAlive()) {
                ((JButton) source).setText("Deactivate");
                ((JButton) source).setActionCommand("Deactivate");

            }
        } else if (command.equals("Deactivate")) {
            if (ViewJFrameDICOMQuery.userInterface.getDICOMCatcher() != null) {
                ViewJFrameDICOMQuery.userInterface.getDICOMCatcher().setStop();
            }

            ViewJFrameDICOMQuery.userInterface.getMenuBuilder().setMenuItemSelected("Activate DICOM receiver", false);
            ((JButton) source).setText("Activate");
            ((JButton) source).setActionCommand("Activate");
        }

    }

    private String[] getFiles(final int[] rows) {
        final Vector<String> list = new Vector<String>();

        for (final int element : rows) {
            final String fileDir = sourceTextF.getText() + File.separatorChar;
            final String fileName = (String) sendTable.getValueAt(element, 0);

            final File current = new File(fileDir + fileName);

            if ( !current.isDirectory()) {

                try {

                    if (FileUtility.isDicom(fileName, fileDir, true) != FileUtility.DICOM) {
                        sendStatusTArea.append("Skipping -- " + fileDir + fileName + " -- not a DICOM file.\n");

                        continue;
                    } else {
                        list.add(current.getAbsolutePath());
                    }
                } catch (final IOException ioe) {
                    sendStatusTArea.append("Skipping -- " + fileDir + fileName
                            + " -- I/O error encountered reading file.\n");
                    ioe.printStackTrace();

                    continue;
                }

            }

            else {
                addFiles(current, list);
            }

        }
        final String output[] = new String[list.size()];
        for (int i = 0; i < list.size(); i++) {
            output[i] = list.get(i);
        }
        return output;
    }

    private void addFiles(final File current, final Vector<String> list) {

        final File files[] = current.listFiles();

        for (final File element : files) {
            if (element.isFile()) {
                try {
                    if (FileUtility.isDicom(element.getName(), element.getParent() + File.separator, true) == FileUtility.DICOM) {
                        list.add(element.getAbsolutePath());
                    } else {
                        sendStatusTArea.append("Skipping -- " + element.getParent() + File.separator
                                + element.getName() + " -- not a DICOM file.\n");
                    }
                } catch (final IOException e) {}
            } else {
                addFiles(element, list);
            }
        }

    }

    /**
     * Appends the text area with the message.
     * 
     * @param appMessage the message
     */
    public void appendSendMessage(final String appMessage) {
        sendStatusTArea.append(appMessage);
    }

    /**
     * Cancels pending moves.
     * 
     * @return DOCUMENT ME!
     */
    public boolean cancelPendingMoves() {
        int i, nRows;
        DICOM_Move dm;
        Object messageID = null;
        MoveRequestInfo info;
        String pending;
        int cancel = -99;
        int reply;

        try {
            dm = new DICOM_Move();
        } catch (final OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.actionPerformed");

            return true;
        }

        nRows = messageTable.getRowCount();

        for (i = 0; i < nRows; i++) {
            pending = (String) messageTable.getValueAt(i, 0);

            if ( (pending != null)
                    && (pending.trim().equals("Saving images") || pending.trim().equals("Sending request"))) {
                messageID = messageTable.getValueAt(i, 5);

                if (cancel == -99) { // first time through
                    reply = JOptionPane.showConfirmDialog(this, "Cancel move request(s)?", "DICOM - Exit",
                            JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

                    if (reply == JOptionPane.YES_OPTION) {
                        cancel = 1;
                    } else {
                        return false;
                    }
                }

                if ( (messageID != null) && (cancel == 1)) {
                    info = (moveRequestHash.get(messageID));

                    if ( (info != null) && info.getThread().isAlive()) {
                        dm.sendCancelRQ(Integer.valueOf((String) messageID).intValue(), info.getPDU());
                    }

                    info.getThread().stop();
                }
            }
        }

        return true;
    }

    /**
     * Cancels pending query.
     * 
     * @return DOCUMENT ME!
     */
    public boolean cancelPendingQuery() {
        int reply;

        if (cancelQ.isEnabled() && (dicomQuery != null)) {
            reply = JOptionPane.showConfirmDialog(this, "Cancel query request?", "DICOM - Exit",
                    JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

            if (reply == JOptionPane.YES_OPTION) {
                dicomQuery.sendFindCancelRQ(queryMsgID, queryPDU);

                return true;
            } else {
                return false;
            }
        }

        return true;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void componentHidden(final ComponentEvent event) {}

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void componentMoved(final ComponentEvent event) {}

    // ************************************************************************
    // **************************** Component Events **************************
    // ************************************************************************

    /**
     * Does not allow component to be smaller than MINIMUM_SIZE.
     * 
     * @param event event that triggered this method
     */
    public void componentResized(final ComponentEvent event) {

        if ( (getBounds().width < ViewJFrameDICOMQuery.MINIMUM_SIZE.width)
                && (getBounds().height < ViewJFrameDICOMQuery.MINIMUM_SIZE.height)) {
            setSize(ViewJFrameDICOMQuery.MINIMUM_SIZE);
        } else if (getBounds().height < ViewJFrameDICOMQuery.MINIMUM_SIZE.height) {
            setSize(getBounds().width, ViewJFrameDICOMQuery.MINIMUM_SIZE.height);
        } else if (getBounds().width < ViewJFrameDICOMQuery.MINIMUM_SIZE.width) {
            setSize(ViewJFrameDICOMQuery.MINIMUM_SIZE.width, getBounds().height);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void componentShown(final ComponentEvent event) {}

    /**
     * Displays the query results.
     * 
     * @param type indicates the query level
     */
    public void displayQueryResults(final int type) {

        DICOM_Object results;
        Object[] rawData;

        queryResultTable.getSelectionModel().removeListSelectionListener(this);
        queryResultTable.getSelectionModel().clearSelection();

        final int end = queryTableModel.getRowCount();

        for (int i = 0; i < end; i++) {

            // System.out.println("Number of rows = " +
            // queryTableModel.getRowCount() );
            queryTableModel.removeRow(0);
        }

        queryResultTable.getSelectionModel().addListSelectionListener(this);

        queryLevel = type;

        switch (queryLevel) {

            case PATIENT_LEVEL:
                queryLev.setText("Query level: Patient");
                try {
                    rawData = new Object[3];
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.displayQueryResults");

                    return;
                }

                while (queryTableModel.getColumnCount() < 3) {
                    queryTableModel.addColumn("");
                }

                final String[] columnName = {"Pat. Name", "Pat. ID", "Referring Physician"};
                queryTableModel.setColumnIdentifiers(columnName);

                for (int i = 0; i < queryPDU.findResults.size(); i++) {
                    results = ((DICOM_Object) (queryPDU.findResults.elementAt(i)));
                    rawData[0] = parseName(results.getStr(DICOM_RTC.DD_PatientName));
                    rawData[1] = results.getStr(DICOM_RTC.DD_PatientID);

                    // Might want to add code to parse physician name
                    rawData[2] = results.getStr(DICOM_RTC.DD_ReferringPhysicianName);
                    queryTableModel.addRow(rawData);
                }

                // Needs some work -- large list are set in the table above and then
                // some removed here
                for (int i = 0; i < queryTableModel.getRowCount(); i++) {

                    for (int j = 1; j < queryTableModel.getRowCount(); j++) {

                        if ( (j != i) && (queryTableModel.getValueAt(i, 1)).equals(queryTableModel.getValueAt(j, 1))) {
                            queryTableModel.removeRow(j--);
                        }
                    }
                }

                break;

            case STUDY_LEVEL:
                queryLev.setText("Query level: Study");
                try {
                    rawData = new Object[4];
                    uids = new DICOM_UID[queryPDU.findResults.size()];
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendQuery");

                    return;
                }

                while (queryTableModel.getColumnCount() < 4) {
                    queryTableModel.addColumn("");
                }

                final String[] columnName2 = {"Study ID", "Date", "Time", "Description"};

                queryTableModel.setColumnIdentifiers(columnName2);
                for (int i = 0; i < queryPDU.findResults.size(); i++) {
                    results = (DICOM_Object) (queryPDU.findResults.elementAt(i));
                    rawData[0] = results.getStr(DICOM_RTC.DD_StudyID);
                    rawData[1] = results.getStr(DICOM_RTC.DD_StudyDate);
                    rawData[2] = results.getStr(DICOM_RTC.DD_StudyTime);
                    rawData[3] = results.getStr(DICOM_RTC.DD_StudyDescription);

                    try {
                        uids[i] = new DICOM_UID(results.getStr(DICOM_RTC.DD_StudyInstanceUID));
                    } catch (final OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendQuery");

                        return;
                    }

                    queryTableModel.addRow(rawData);
                }

                if (queryTableModel.getRowCount() == 0) {
                    MipavUtil.displayError("No studies returned.  Check your start and end dates.");
                }

                break;

            case SERIES_LEVEL:
                queryLev.setText("Query level: Series");
                try {
                    rawData = new Object[5];
                    uids = new DICOM_UID[queryPDU.findResults.size()];
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendQuery");

                    return;
                }

                while (queryTableModel.getColumnCount() < 5) {
                    queryTableModel.addColumn("");
                }

                final String[] columnName3 = {"#", "Date", "Time", "Mod", "Description"};

                queryTableModel.setColumnIdentifiers(columnName3);
                for (int i = 0; i < queryPDU.findResults.size(); i++) {
                    results = (DICOM_Object) (queryPDU.findResults.elementAt(i));
                    rawData[0] = results.getStr(DICOM_RTC.DD_SeriesNumber);

                    if (results.getStr(DICOM_RTC.DD_SeriesNumber).length() == 0) {
                        rawData[0] = "0";
                    }

                    rawData[1] = results.getStr(DICOM_RTC.DD_SeriesDate);
                    rawData[2] = results.getStr(DICOM_RTC.DD_SeriesTime);
                    rawData[3] = results.getStr(DICOM_RTC.DD_Modality);
                    rawData[4] = results.getStr(DICOM_RTC.DD_SeriesDescription);

                    try {
                        uids[i] = new DICOM_UID(results.getStr(DICOM_RTC.DD_SeriesInstanceUID), results
                                .getStr(DICOM_RTC.DD_StudyInstanceUID));
                    } catch (final OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendQuery");

                        return;
                    }

                    queryTableModel.addRow(rawData);
                }

                break;

            case IMAGE_LEVEL:
                queryLev.setText("Query level: Image");
                try {
                    rawData = new Object[5];
                    uids = new DICOM_UID[queryPDU.findResults.size()];
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendQuery");

                    return;
                }

                while (queryTableModel.getColumnCount() < 5) {
                    queryTableModel.addColumn("");
                }

                final String[] columnName4 = {"#", "Date", "Time", "Acq. Date", "Acc. #"};

                queryTableModel.setColumnIdentifiers(columnName4);
                for (int i = 0; i < queryPDU.findResults.size(); i++) {
                    results = (DICOM_Object) (queryPDU.findResults.elementAt(i));
                    rawData[0] = results.getStr(DICOM_RTC.DD_InstanceNumber);
                    rawData[1] = results.getStr(DICOM_RTC.DD_ContentDate);
                    rawData[2] = results.getStr(DICOM_RTC.DD_ContentTime);
                    rawData[3] = results.getStr(DICOM_RTC.DD_AcquisitionDate);
                    rawData[4] = results.getStr(DICOM_RTC.DD_AccessionNumber);

                    try {
                        uids[i] = new DICOM_UID(results.getStr(DICOM_RTC.DD_SeriesInstanceUID), results
                                .getStr(DICOM_RTC.DD_SOPInstanceUID), results.getStr(DICOM_RTC.DD_StudyInstanceUID));
                    } catch (final OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendQuery");

                        return;
                    }

                    queryTableModel.addRow(rawData);
                }

                break;
        }

        sort(queryTableModel, 0, false);
        setColumns();

        queryResultTable.sizeColumnsToFit(JTable.AUTO_RESIZE_ALL_COLUMNS);
        queryResultTable.getTableHeader().resizeAndRepaint();
        queryResultTable.repaint();
        // validate();

        send.setEnabled(true);
        cancelQ.setEnabled(false);

        if (queryTableModel.getRowCount() == 0) {
            MipavUtil.displayError("No information was returned.  Be sure\nto check your start and end dates.");
        }
    }

    // *******************************************************************
    // ************************* Focus Events ****************************
    // *******************************************************************

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void focusGained(final FocusEvent event) {}

    /**
     * When the user clicks the mouse out of a text field, resets the neccessary variables.
     * 
     * @param event event that triggers this function
     */
    public void focusLost(final FocusEvent event) {
        final Object source = event.getSource();
        JTextField field;
        String text;

        if (source == sourceTextF) {
            field = (JTextField) source;
            text = field.getText();
            setupSendFileListing(text);

        }
    }

    // ************************************************************************
    // **************************** Mouse Events ******************************
    // ************************************************************************

    /**
     * When the user double clicks on a selection, sends a query at the next query level; when the user clicks on a
     * header, sorts the column.
     * 
     * @param e event that triggered this method
     */

    public void mouseClicked(final MouseEvent e) {
        final Object source = e.getSource();
        final Point p = e.getPoint();
        int col;

        if (source.equals(queryResultTable.getTableHeader())) {
            col = queryResultTable.columnAtPoint(p);

            if (e.isShiftDown()) {
                sort(queryTableModel, col, true);
            } else {
                sort(queryTableModel, col, false);
            }
        } else if (source.equals(serverTable.getTableHeader())) {
            col = serverTable.columnAtPoint(p);

            if (col == 0) {
                return;
            }

            if (e.isShiftDown()) {
                sort(serverModel, col, true);
            } else {
                sort(serverModel, col, false);
            }
        } else if (source.equals(storageTable.getTableHeader())) {
            col = storageTable.columnAtPoint(p);

            if (col == 0) {
                return;
            }

            if (e.isShiftDown()) {
                sort(storageModel, col, true);
            } else {
                sort(storageModel, col, false);
            }
        } else if (source.equals(serverTable) && (e.getClickCount() == 2)) {
            ActionEvent ev;

            try {
                ev = new ActionEvent(set, ActionEvent.ACTION_PERFORMED, "SetAs");
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.actionPerformed");

                return;
            }

            serverModel.updateBulbs(serverTable.getSelectedRow());
            actionPerformed(ev);
            serverTable.repaint();
        } else if (source.equals(storageTable) && (e.getClickCount() == 2)) {
            ActionEvent ev;

            try {
                ev = new ActionEvent(setStore, ActionEvent.ACTION_PERFORMED, "SetAs");
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.actionPerformed");

                return;
            }

            storageModel.updateBulbs(storageTable.getSelectedRow());
            actionPerformed(ev);
            storageTable.repaint();
        } else if (source.equals(queryResultTable) && (e.getClickCount() == 2)) {

            if (queryResultTable.getSelectionModel().isSelectionEmpty()) {
                return;
            } else if (queryLevel == ViewJFrameDICOMQuery.PATIENT_LEVEL) {
                sendQuery(ViewJFrameDICOMQuery.STUDY_LEVEL);
            } else if (queryLevel == ViewJFrameDICOMQuery.STUDY_LEVEL) {
                sendQuery(ViewJFrameDICOMQuery.SERIES_LEVEL);
            } else if (queryLevel == ViewJFrameDICOMQuery.SERIES_LEVEL) {
                sendQuery(ViewJFrameDICOMQuery.IMAGE_LEVEL);
            } else if (queryLevel == ViewJFrameDICOMQuery.IMAGE_LEVEL) {
                sendMoveRequest(ViewJFrameDICOMQuery.IMAGE_LEVEL);

                if (DICOMDisplayer.getSucceeded()) {
                    JOptionPane.showMessageDialog(this, "Move request successful", "Success",
                            JOptionPane.INFORMATION_MESSAGE);
                }
            }
        } else if (source.equals(sendTable) && (e.getClickCount() == 2)) {
            String str = null;
            String dir = null;
            str = (String) sendModel.getValueAt( ((JTable) source).getSelectedRow(), 0);

            // try string to determine if it is a directory - if not return.
            dir = sourceTextF.getText();

            // sourceTextF.setText(sourceTextF.getText() + File.separatorChar +
            // str);
            if (setupSendFileListing(dir + File.separatorChar + str) == false) {
                return;
            }

            sourceTextF.setText(dir + File.separatorChar + str);

            ((TitledBorder) (listPanel.getBorder())).setTitle(sourceTextF.getText());
            sendPanel.validate();
            sendPanel.repaint();
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param e DOCUMENT ME!
     */
    public void mouseEntered(final MouseEvent e) {}

    /**
     * DOCUMENT ME!
     * 
     * @param e DOCUMENT ME!
     */
    public void mouseExited(final MouseEvent e) {}

    /**
     * DOCUMENT ME!
     * 
     * @param e DOCUMENT ME!
     */
    public void mousePressed(final MouseEvent e) {}

    /**
     * DOCUMENT ME!
     * 
     * @param e DOCUMENT ME!
     */
    public void mouseReleased(final MouseEvent e) {}

    /**
     * ChangeListener************************************************************************* /** Sets values based on
     * knob along slider.
     * 
     * @param e DOCUMENT ME! event that triggered this function
     */
    public void stateChanged(final ChangeEvent e) {
        final Object source = e.getSource();

        if (source == tabbedPane) {

            if (tabbedPane.getSelectedIndex() == 2) { // Send tab
                setupSendTab();
            }
        }
    }

    // ************************************************************************
    // ************************* List Selection Events ************************
    // ************************************************************************
    /**
     * A List Selection Listener event for changes in the tables. If this is the query table, it will enable the down
     * and move images buttons and set up the UIDs for querying. If this is the server or storage table, it will enable
     * the edit, delete, and set as default buttons.
     * 
     * @param e event that triggered this method.
     */
    public void valueChanged(final ListSelectionEvent e) {
        final Object source = e.getSource();

        if (e.getValueIsAdjusting()) {
            return;
        }

        if (source.equals(queryResultTable.getSelectionModel()) && queryTableModel.getRowCount() != 0) {
            setEnabled(down, true);
            setEnabled(move, true);

            final int queryRow = queryResultTable.getSelectedRow();

            if (queryLevel == ViewJFrameDICOMQuery.PATIENT_LEVEL) {
                ptIDText.setText((String) queryTableModel.getValueAt(queryRow, 1));
                ptNameText.setText((String) queryTableModel.getValueAt(queryRow, 0));
            } else if (queryLevel == ViewJFrameDICOMQuery.STUDY_LEVEL) {
                studyNOText.setText((String) queryTableModel.getValueAt(queryRow, 0));
                studyInstanceUID = uids[queryRow].getStudyInstanceUID();
            } else if (queryLevel == ViewJFrameDICOMQuery.SERIES_LEVEL) {
                seriesInstanceUID = uids[queryRow].getSeriesInstanceUID();
            } else if (queryLevel == ViewJFrameDICOMQuery.IMAGE_LEVEL) {
                SOPInstanceUID = uids[queryRow].getSOPInstanceUID();
            }
        } else if (source.equals(serverTable.getSelectionModel())) {
            serverTable.repaint();

            if (serverTable.getSelectionModel().getMinSelectionIndex() != serverTable.getSelectionModel()
                    .getMaxSelectionIndex()) {
                setEnabled(edit, false);
                setEnabled(delete, false);
                setEnabled(set, false);
            } else {
                setEnabled(edit, true);
                setEnabled(delete, true);
                setEnabled(set, true);
            }
        } else if (source.equals(storageTable.getSelectionModel())) {
            storageTable.repaint();

            if (storageTable.getSelectionModel().getMinSelectionIndex() != storageTable.getSelectionModel()
                    .getMaxSelectionIndex()) {
                setEnabled(editStore, false);
                setEnabled(deleteStore, false);
                setEnabled(setStore, false);
            } else {
                setEnabled(editStore, true);
                setEnabled(deleteStore, true);
                setEnabled(setStore, true);
            }
        } else if (source.equals(messageTable.getSelectionModel())) {
            messageTable.repaint();
            setEnabled(cancel, true);
        }

    }

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowActivated(final WindowEvent event) {}

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowClosed(final WindowEvent event) {}

    /**
     * Closes frame is requested. The user is notified if there are any pending image move requests. The user is able to
     * 
     * @param event event that triggered this function
     */
    public void windowClosing(final WindowEvent event) {

        if (messageTable.getRowCount() != 0) {

            // setVisible(false);

            if (cancelPendingMoves() == false) {
                return;
            }

            if (cancelPendingQuery() == false) {
                return;
            }

            // if (cancelQ.isEnabled() && dicomQuery != null) {
            // dicomQuery.sendFindCancelRQ(queryMsgID, queryPDU);
            // }
            setVisible(false);
            ViewJFrameDICOMQuery.userInterface.setDICOMQueryFrame(null);
            dispose();
        } else {
            setVisible(false);
            ViewJFrameDICOMQuery.userInterface.setDICOMQueryFrame(null);
            dispose();
        }
    }

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowDeactivated(final WindowEvent event) {}

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowDeiconified(final WindowEvent event) {}

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowIconified(final WindowEvent event) {}

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void windowOpened(final WindowEvent event) {}

    /**
     * Assign source Calendar to target calendar.
     * 
     * @param sourceC The source calendar
     * @param targetC The target calendar that will copy the values of the source Calendar.
     * 
     * @return The target calendar.
     */
    private Calendar assignCalendar(final Calendar sourceC, final Calendar targetC) {
        final int year = sourceC.get(Calendar.YEAR);
        final int month = sourceC.get(Calendar.MONTH);
        final int day = sourceC.get(Calendar.DATE);

        targetC.set(year, month, day);

        return targetC;
    }

    /**
     * Builds the panel that designed for inputing study duration information. It contains a panel of radiobuttons, and
     * two rows of pull down boxes to facilitate the input of information.
     * 
     * @return The constructed panel.
     */
    private JPanel buildDatePanel() {
        JPanel rootPanel, datePanel, blockPanel;
        String label;
        GridBagConstraints gbc;
        Insets insets;
        ButtonGroup g1;

        try {
            gbc = new GridBagConstraints();
            rootPanel = new JPanel();
            rootPanel.setLayout(new GridBagLayout());
            blockPanel = new JPanel();
            blockPanel.setLayout(new GridLayout(2, 3));
            g1 = new ButtonGroup();
            datePanel = new JPanel();
            datePanel.setLayout(new GridBagLayout());
            startMonthBox = new JComboBox();
            startDayBox = new JComboBox();
            startYearBox = new JComboBox();
            endMonthBox = new JComboBox();
            endDayBox = new JComboBox();
            endYearBox = new JComboBox();
            insets = new Insets(0, 0, 0, 0);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildDatePanel");

            return null;
        }

        todayCalendar = Calendar.getInstance();
        rootPanel.setForeground(Color.black);

        final TitledBorder border = buildTitledBorder("Query Duration");
        border.setTitleColor(Color.blue);
        rootPanel.setBorder(border);

        blockPanel.setForeground(Color.black);

        final JRadioButton b1 = buildRadioButton("Today", true);
        g1.add(b1);
        b1.setToolTipText("Same date study ");
        blockPanel.add(b1);

        final JRadioButton b2 = buildRadioButton("One Week", false);
        g1.add(b2);
        b2.setToolTipText("Studies up to one week ago from today ");
        blockPanel.add(b2);

        final JRadioButton b3 = buildRadioButton("One Month", false);
        g1.add(b3);
        b3.setToolTipText("Studies up to one month ago from today ");
        blockPanel.add(b3);

        final JRadioButton b4 = buildRadioButton("Three Month", false);
        g1.add(b4);
        b4.setToolTipText("Studies up to three months ago ");
        blockPanel.add(b4);

        final JRadioButton b5 = buildRadioButton("Six Month", false);
        g1.add(b5);
        b5.setToolTipText("Studies up to half year ago ");
        blockPanel.add(b5);

        final JRadioButton b6 = buildRadioButton("One Year", false);
        g1.add(b6);
        b6.setToolTipText("Studies up to one year ago ");
        blockPanel.add(b6);

        datePanel.setForeground(Color.black);
        datePanel.setBorder(buildTitledBorder(""));

        datePanel.add(buildLabel("Start Date:   "), setupLabelLayout());

        for (int i = 0; i < 12; i++) {
            startMonthBox.addItem(ViewJFrameDICOMQuery.monthString[i]);
        }

        startMonthBox.setFont(ViewJFrameDICOMQuery.font12);
        startMonthBox.addActionListener(this);
        startMonthBox.setActionCommand("StartMonth");
        datePanel.add(startMonthBox, setupComboBoxLayout());

        for (int i = 1; i < 32; i++) {
            startDayBox.addItem(Integer.toString(i));
        }

        startDayBox.setFont(ViewJFrameDICOMQuery.font12);
        startDayBox.addActionListener(this);
        startDayBox.setActionCommand("StartDay");
        datePanel.add(startDayBox, setupComboBoxLayout());

        for (int i = ViewJFrameDICOMQuery.DEFAULT_MIN_YEAR; i < (todayCalendar.get(Calendar.YEAR) + 1); i++) {
            startYearBox.addItem(Integer.toString(i));
        }

        startYearBox.setFont(ViewJFrameDICOMQuery.font12);
        startYearBox.addActionListener(this);
        startYearBox.setActionCommand("StartYear");
        gbc = setupComboBoxLayout();
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        datePanel.add(startYearBox, gbc);

        startCalendar = copyOfCalendar(todayCalendar);
        startCalendar = getMonthsBefore(startCalendar, 3);

        // End
        endCalendar = copyOfCalendar(todayCalendar);
        datePanel.add(buildLabel("End Date:   "), setupLabelLayout());

        for (int i = 0; i < 12; i++) {
            endMonthBox.addItem(ViewJFrameDICOMQuery.monthString[i]);
        }

        endMonthBox.setFont(ViewJFrameDICOMQuery.font12);
        endMonthBox.setSelectedIndex(todayCalendar.get(Calendar.MONTH));
        endMonthBox.addActionListener(this);
        endMonthBox.setActionCommand("EndMonth");
        datePanel.add(endMonthBox, setupComboBoxLayout());

        for (int i = 1; i < 32; i++) {
            endDayBox.addItem(Integer.toString(i));
        }

        endDayBox.setFont(ViewJFrameDICOMQuery.font12);
        endDayBox.setSelectedIndex(todayCalendar.get(Calendar.DATE) - 1);
        endDayBox.addActionListener(this);
        endDayBox.setActionCommand("EndDay");
        datePanel.add(endDayBox, setupComboBoxLayout());

        for (int i = (todayCalendar.get(Calendar.YEAR)); i >= ViewJFrameDICOMQuery.DEFAULT_MIN_YEAR; i--) {
            endYearBox.addItem(Integer.toString(i));
        }

        endYearBox.setFont(ViewJFrameDICOMQuery.font12);
        gbc = setupComboBoxLayout();
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        endYearBox.addActionListener(this);
        endYearBox.setActionCommand("EndYear");
        datePanel.add(endYearBox, gbc);
        setEndCalendar(endCalendar);
        setStartCalendar(todayCalendar);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        ;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.insets = insets;
        rootPanel.add(blockPanel, gbc);
        rootPanel.add(datePanel, gbc);

        label = "Today's Date: " + (ViewJFrameDICOMQuery.monthString[todayCalendar.get(Calendar.MONTH)]) + "-"
                + todayCalendar.get(Calendar.DATE) + "-" + todayCalendar.get(Calendar.YEAR);

        try {
            currentDateL = new JLabel(label);
            currentDateL.setPreferredSize(new Dimension(170, 40));
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildDatePanel");

            return null;
        }

        currentDateL.setFont(ViewJFrameDICOMQuery.font12);
        currentDateL.setForeground(Color.blue);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.CENTER;
        rootPanel.add(currentDateL, gbc);

        return rootPanel;
    }

    /**
     * Constructs the helping manual in the help panel - one of the three panels in the tabbed panel. Reads the help
     * information from a file "DICOMhelp.txt" that should be in the classpath. If it is not, throws up an error message
     * and displays nothing in the tabbed pane.
     */
    private void buildHelpText() {
        BufferedReader buffReader = null;
        textArea.setFont(ViewJFrameDICOMQuery.font12B);
        textArea.setText(" Instructions for DICOM Server Access for MIPAV \n\n");

        try {
            final URL fileURL = getClass().getClassLoader().getResource("DICOMhelp.txt");

            if (fileURL == null) {
                MipavUtil.displayError("The help file DICOMhelp.txt was not found, so\nthe help tab is empty.");
                textArea.setText("");

                return;
            }

            final Reader reader = new InputStreamReader(fileURL.openStream());
            buffReader = new BufferedReader(reader);
        } catch (final IOException ioe) {
            MipavUtil.displayError("The help file DICOMhelp.txt was not found, so\nthe help tab is empty.");
            textArea.setText("");

            try {

                if (buffReader != null) {
                    buffReader.close();
                }
            } catch (final IOException closee) {}

            return;
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildHelpText");

            try {

                if (buffReader != null) {
                    buffReader.close();
                }
            } catch (final IOException closee) {}

            return;
        }

        try {
            String s;
            s = buffReader.readLine();

            while (s != null) {
                textArea.append(s + "\n");
                s = buffReader.readLine();
            }
        } catch (final IOException e) {
            MipavUtil.displayError("Error " + e);
        } finally {

            try {

                if (buffReader != null) {
                    buffReader.close();
                }
            } catch (final IOException closee) {}
        }
    }

    /**
     * Builds the host panel by calling methods to build the server and storage panels.
     * 
     * @return the panel containing the hosts
     */
    private JPanel buildHostPanel() {
        JPanel serverPanel, storagePanel, rootPanel;
        GridBagConstraints gbc;

        try {
            gbc = new GridBagConstraints();
            rootPanel = new JPanel();
            rootPanel.setLayout(new GridBagLayout());
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildHostPanel");

            return null;
        }

        serverPanel = buildServerPanel();
        serverPanel.setBorder(buildTitledBorder("Servers"));

        storagePanel = buildStoragePanel();
        storagePanel.setBorder(buildTitledBorder("Storage Destination"));

        gbc = setGBC(0, 0, 1, 1);
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.BOTH;

        rootPanel.add(serverPanel, gbc);

        gbc = setGBC(0, 1, 1, 1);
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.BOTH;
        rootPanel.add(storagePanel, gbc);

        return rootPanel;
    }

    /**
     * Builds the label for the textField, try to standardize the label appearance in the GUI.
     * 
     * @param s a string which encode the name of the label
     * 
     * @return the constructed label.
     */
    private JLabel buildLabel(final String s) {
        JLabel label;

        try {
            label = new JLabel(s);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildLabel");

            return null;
        }

        label.setFont(ViewJFrameDICOMQuery.font12);
        label.setForeground(Color.black);
        label.setHorizontalTextPosition(SwingConstants.LEFT);

        return label;
    }

    /**
     * Builds the listing panel.
     * 
     * @return the server panel
     */
    private JPanel buildListingPanel() {

        GridBagConstraints gbc;
        JScrollPane scrollPane;
        final String[] columnNames = {"Listing"};
        JLabel destLabel = null;
        JLabel sourceLabel = null;
        JPanel basePanel;
        JPanel sendStatusPanel;
        JScrollPane sendStatusSPane;

        try {
            listPanel = new JPanel();
            listPanel.setLayout(new GridBagLayout());
            basePanel = new JPanel();
            basePanel.setLayout(new GridBagLayout());
            gbc = new GridBagConstraints();
            sendModel = new ViewTableModel();
            sendTable = new JTable(sendModel);
            sendDestCBox = new JComboBox();
            sendButton = new JButton("Send Images");
            testButton = new JButton("Test connection");
            destLabel = new JLabel("Destination");

            sourceLabel = new JLabel("Source directory");
            sourceTextF = new JTextField();
            browseButton = new JButton("Browse");
            upDirButton = new JButton("Up directory");
            refreshButton = new JButton("Refresh dir.");
            sendStatusPanel = new JPanel();
            sendStatusTArea = new JTextArea();
            sendStatusClearButton = new JButton("Clear");
            help2 = new JButton("Help");
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildSendPanel");

            return null;
        }

        listPanel.setBorder(buildTitledBorder("Directory Listing:"));

        sendModel.addColumn(columnNames[0]);
        sendTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);

        try {
            sendTable.setPreferredScrollableViewportSize(new Dimension(450, 200));
            sendTable.setMinimumSize(new Dimension(450, 100));

            // sendTable.setToolTipText("Double click on a item to send
            // image(s). ");
            scrollPane = new JScrollPane(sendTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setPreferredSize(new Dimension(450, 200));
            scrollPane.setMinimumSize(new Dimension(150, 100));
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildSendPanel");

            return null;
        }

        sendTable.getSelectionModel().addListSelectionListener(this);
        sendTable.addMouseListener(this);
        sendTable.getTableHeader().addMouseListener(this);
        scrollPane.setBackground(Color.black);
        gbc = setGBC(0, 1, 5, 3);
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.BOTH;
        listPanel.add(scrollPane, gbc);

        sourceLabel.setFont(ViewJFrameDICOMQuery.font12B);
        sourceLabel.setForeground(Color.black);
        gbc = setGBC(0, 0, 1, 1);

        // gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.anchor = GridBagConstraints.CENTER;
        listPanel.add(sourceLabel, gbc);

        sourceTextF.setFont(ViewJFrameDICOMQuery.font12B);

        // sourceTextF.setBackground(Color.gray.brighter());
        sourceTextF.setEnabled(true);
        sourceTextF.addActionListener(this);
        sourceTextF.addFocusListener(this);
        gbc = setGBC(1, 0, 1, 1);
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        listPanel.add(sourceTextF, gbc);

        Boolean test;
        String str = new String();

        for (int r = 0; r < storageTable.getRowCount(); r++) {
            test = (Boolean) storageTable.getValueAt(r, 0);

            if (test.booleanValue() == true) {
                str = (String) storageTable.getValueAt(r, 3);
            }
        }

        sourceTextF.setText(str);

        browseButton.setFont(ViewJFrameDICOMQuery.font12B);
        browseButton.setActionCommand("Browse");
        // browseButton.setBackground(Color.gray.brighter());
        browseButton.addActionListener(this);
        gbc = setGBC(2, 0, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        listPanel.add(browseButton, gbc);

        upDirButton.setFont(ViewJFrameDICOMQuery.font12B);
        upDirButton.setActionCommand("UpDir");
        // upDirButton.setBackground(Color.gray.brighter());
        upDirButton.addActionListener(this);
        gbc = setGBC(3, 0, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        listPanel.add(upDirButton, gbc);

        refreshButton.setFont(ViewJFrameDICOMQuery.font12B);
        refreshButton.setActionCommand("RefreshDir");
        // refreshButton.setBackground(Color.gray.brighter());
        refreshButton.addActionListener(this);
        gbc = setGBC(4, 0, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        listPanel.add(refreshButton, gbc);

        destLabel.setFont(ViewJFrameDICOMQuery.font12B);
        destLabel.setForeground(Color.black);
        gbc = setGBC(0, 4, 1, 1);

        // gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.anchor = GridBagConstraints.CENTER;
        listPanel.add(destLabel, gbc);

        sendDestCBox.setFont(ViewJFrameDICOMQuery.font12B);
        sendDestCBox.addActionListener(this);
        gbc = setGBC(1, 4, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        /*
         * String strs[] = new String[serverTable.getRowCount()]; for (int r = 0; r < serverTable.getRowCount(); r++) {
         * test = (Boolean)serverTable.getValueAt(r, 0); str = (String)serverTable.getValueAt(r,1); if
         * (test.booleanValue() == true) { strs[0] = str;} else if (r == 0) { strs[1] = str; } else { strs[r] = str;} }
         */

        int defaultChoice = -1;

        for (int s = 0; s < serverTable.getRowCount(); s++) {

            if ((Boolean) serverTable.getValueAt(s, 0) == Boolean.TRUE) {
                defaultChoice = s;

                break;
            }
        }

        if (defaultChoice != -1) {
            sendDestCBox.addItem(serverTable.getValueAt(defaultChoice, 1));
        }

        for (int r = 0; r < serverTable.getRowCount(); r++) {

            if (r != defaultChoice) {
                sendDestCBox.addItem(serverTable.getValueAt(r, 1));
                // sendDestCBox.addItem(strs[r]);
            }
        }

        listPanel.add(sendDestCBox, gbc);

        sendButton.setFont(ViewJFrameDICOMQuery.font12B);
        sendButton.setActionCommand("SendImages");
        // sendButton.setBackground(Color.gray.brighter());
        sendButton.addActionListener(this);
        gbc = setGBC(2, 4, 2, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        listPanel.add(sendButton, gbc);

        testButton.setFont(ViewJFrameDICOMQuery.font12B);
        testButton.setActionCommand("TestConnection");
        // testButton.setBackground(Color.gray.brighter());
        testButton.addActionListener(this);
        gbc = setGBC(4, 4, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        listPanel.add(testButton, gbc);

        try {
            sendStatusTArea.setBackground(Color.lightGray);
            sendStatusTArea.setEditable(false);
            sendStatusTArea.setFont(ViewJFrameDICOMQuery.font12);
            sendStatusTArea.setMargin(new Insets(3, 3, 3, 3));
            sendStatusSPane = new JScrollPane(sendStatusTArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            sendStatusSPane.setPreferredSize(new Dimension(450, 200));
            sendStatusSPane.setMinimumSize(new Dimension(150, 100));
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildSendPanel");

            return null;
        }

        sendStatusPanel.setBorder(buildTitledBorder("Send status:"));
        sendStatusPanel.setLayout(new GridBagLayout());
        gbc = setGBC(0, 0, 2, 5);
        gbc.fill = GridBagConstraints.BOTH;

        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.anchor = GridBagConstraints.CENTER;
        sendStatusPanel.add(sendStatusSPane, gbc);

        sendStatusClearButton.setActionCommand("SendClear");
        sendStatusClearButton.addActionListener(this);
        gbc = setGBC(0, 5, 1, 1);
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        sendStatusPanel.add(sendStatusClearButton, gbc);

        help2.setActionCommand("Help2");
        help2.addActionListener(this);
        gbc = setGBC(1, 5, 1, 1);
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        sendStatusPanel.add(help2, gbc);

        gbc = setGBC(0, 0, 1, 4);
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.anchor = GridBagConstraints.CENTER;
        basePanel.add(listPanel, gbc);

        gbc = setGBC(0, 5, 1, 2);
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.anchor = GridBagConstraints.CENTER;
        basePanel.add(sendStatusPanel, gbc);

        return basePanel;
    }

    /**
     * Construct the Query retrieval message panel. It contains a table for displaying the information.
     * 
     * @return The constructed panel.
     */
    private JPanel buildMessagePanel() {
        JPanel messagePanel;
        GridBagConstraints gbc;
        JScrollPane scrollPane;
        JButton clearText;

        try {
            messagePanel = new JPanel();
            messagePanel.setBorder(buildTitledBorder("Query Retrieval Information"));
            messagePanel.setLayout(new GridBagLayout());
            gbc = new GridBagConstraints();
            messageTable = new DICOMDisplayer();

            messageTable.setPreferredScrollableViewportSize(new Dimension(450, 200));
            messageTable.setMinimumSize(new Dimension(450, 200));
            messageTable.getSelectionModel().addListSelectionListener(this);
            scrollPane = new JScrollPane(messageTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setPreferredSize(new Dimension(450, 100));
            scrollPane.setMinimumSize(new Dimension(150, 50));
            clearText = new JButton("Clear Table");
            cancel = new JButton("Cancel");
            help1 = new JButton("Help");
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildMessagePanel");

            return null;
        }

        scrollPane.setBackground(Color.black);

        gbc = setGBC(0, 0, 3, 2);
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        messagePanel.add(scrollPane, gbc);

        clearText.setFont(ViewJFrameDICOMQuery.font12B);
        clearText.setActionCommand("ClearTable");
        clearText.addActionListener(this);
        gbc = setGBC(0, 2, 1, 1);
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        messagePanel.add(clearText, gbc);

        cancel.setFont(ViewJFrameDICOMQuery.font12B);
        cancel.setActionCommand("Cancel");
        cancel.setEnabled(false);
        cancel.addActionListener(this);
        gbc = setGBC(1, 2, 1, 1);
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        messagePanel.add(cancel, gbc);

        help1.setFont(ViewJFrameDICOMQuery.font12B);
        help1.setActionCommand("Help1");
        help1.setEnabled(true);
        help1.addActionListener(this);
        gbc = setGBC(2, 2, 1, 1);
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        messagePanel.add(help1, gbc);

        return messagePanel;

    }

    /**
     * Constructs the patient information input panel.
     * 
     * @return the constructed panel.
     */
    private JPanel buildPtInfoPanel() {
        JPanel rootPanel;
        JButton button;
        GridBagConstraints gbc;

        try {
            gbc = new GridBagConstraints();
            rootPanel = new JPanel();
            rootPanel.setLayout(new GridBagLayout());
            ptNameText = new JTextField("", 10);
            ptIDText = new JTextField("", 10);
            studyNOText = new JTextField("", 10);
            physText = new JTextField("", 10);
            button = new JButton("Clear");
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildPtInfoPanel");

            return null;
        }

        rootPanel.setForeground(Color.black);
        rootPanel.setBorder(buildTitledBorder("Patient Query Information"));

        rootPanel.add(buildLabel("Patient Name:   "), setupLabelLayout());
        ptNameText.setFont(ViewJFrameDICOMQuery.font12);
        ptNameText.setToolTipText("Enter the patient's last name ");
        ptNameText.addActionListener(this);
        ptNameText.setActionCommand("SendQuery");
        rootPanel.add(ptNameText, setupTextFieldLayout());

        rootPanel.add(buildLabel("Patient ID:   "), setupLabelLayout());
        ptIDText.setFont(ViewJFrameDICOMQuery.font12);
        ptIDText.addActionListener(this);
        ptIDText.setActionCommand("SendQuery");
        ptIDText.setToolTipText("Enter the patient's medical recorder number ");
        rootPanel.add(ptIDText, setupTextFieldLayout());

        rootPanel.add(buildLabel("Study Number:   "), setupLabelLayout());
        studyNOText.setFont(ViewJFrameDICOMQuery.font12);
        studyNOText.addActionListener(this);
        studyNOText.setActionCommand("SendQuery");
        studyNOText.setToolTipText("Enter the study number ");
        rootPanel.add(studyNOText, setupTextFieldLayout());

        rootPanel.add(buildLabel("Physician:   "), setupLabelLayout());
        physText.setFont(ViewJFrameDICOMQuery.font12);
        physText.addActionListener(this);
        physText.setActionCommand("SendQuery");
        physText.setToolTipText("Enter the physician's name ");
        rootPanel.add(physText, setupTextFieldLayout());

        button.setFont(ViewJFrameDICOMQuery.font12B);
        button.setActionCommand("ClearPtInfo");
        button.addActionListener(this);
        button.setToolTipText("Clear the information in the fields ");
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        rootPanel.add(button, gbc);

        return rootPanel;
    }

    /**
     * Builds the DICOM Query panel, one of the three tabbed Panels in the DICOMQuery GUI. Inside, there are three
     * different elements: The patient information panel - patient name, ID, study dates ... The Query result panel -
     * display the result of the query The message area - display the status of the query
     * 
     * @return the DICOM Query Panel which contains the patient info and query result
     */
    private JPanel buildQueryPanel() {
        JPanel containPanel; // , rootPanel;
        GridBagConstraints gbc;
        Insets insets;

        try {
            rootQueryPanel = new JPanel();
            rootQueryPanel.setLayout(new GridBagLayout());
            containPanel = new JPanel();
            containPanel.setLayout(new GridBagLayout());
            insets = new Insets(0, 0, 0, 0);
            send = new JButton("Send Query");
            move = new JButton("Retrieve Image");
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildQueryPanel");

            return null;
        }

        gbc = setGBC(0, 0, 2, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = insets;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        containPanel.add(buildPtInfoPanel(), gbc);

        gbc = setGBC(2, 0, GridBagConstraints.REMAINDER, GridBagConstraints.REMAINDER);
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0.01;
        gbc.weighty = 1.0;
        containPanel.add(buildDatePanel(), gbc);

        send.setFont(ViewJFrameDICOMQuery.font12B);
        send.setActionCommand("SendQuery");
        // send.setBackground(Color.gray.brighter());
        send.addActionListener(this);

        gbc = setGBC(0, 1, 1, GridBagConstraints.REMAINDER);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = insets;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        containPanel.add(send, gbc);

        move.setFont(ViewJFrameDICOMQuery.font12B);
        move.setActionCommand("Move");
        // move.setBackground(Color.gray.brighter());
        move.addActionListener(this);

        gbc = setGBC(1, 1, 1, GridBagConstraints.REMAINDER);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = insets;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        setEnabled(move, false);
        containPanel.add(move, gbc);

        gbc = setGBC(0, 0, GridBagConstraints.REMAINDER, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = insets;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        rootQueryPanel.add(containPanel, gbc);

        gbc = setGBC(0, 1, GridBagConstraints.REMAINDER, 2);
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = insets;
        gbc.weightx = 100;
        gbc.weighty = 100;
        subQueryPanel = buildTextPanel();
        rootQueryPanel.add(subQueryPanel, gbc);

        gbc = setGBC(0, 3, GridBagConstraints.REMAINDER, 1);
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = insets;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        rootQueryPanel.add(buildMessagePanel(), gbc);
        rootQueryPanel.setToolTipText("Query information Page ");

        return rootQueryPanel;
    }

    /**
     * Constructs the radio button for fast input of study date range.
     * 
     * @param s The name of the radio button: today, one week, one month ...
     * @param flag Whether the radio button is active by default or not.
     * 
     * @return The constructed radio button with appropriated attributes attached.
     */
    private JRadioButton buildRadioButton(String s, final boolean flag) {
        JRadioButton r1;

        try {
            r1 = new JRadioButton(s, flag);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildRadioButton");

            return null;
        }

        r1.setFont(ViewJFrameDICOMQuery.font12);
        s = s + "RButton";
        r1.setActionCommand(s);
        r1.addActionListener(this);

        return r1;
    }

    /**
     * Builds the host panel by calling methods to build the server and storage panels.
     * 
     * @return the panel containing
     */
    private final JPanel buildSendPanel() {
        JPanel sendListingPanel;

        sendListingPanel = buildListingPanel();

        return sendListingPanel;
    }

    /**
     * Builds the panel that contains the server table and four buttons. The server table consists of five columns with
     * information the user needs to query the server. The buttons are "Create", which creates a new server; "Edit",
     * which edits the currently selected server; "Delete", which deletes the currently selected server; and "Set As
     * Default", which sets the current server as default. To get the data in the table, the table model reads the
     * .preferences file and parses the data. When changes are made, they are saved to the .preferences file.
     * 
     * @return the server panel
     */
    private JPanel buildServerPanel() {

        JPanel serverPanel;
        StringTokenizer tok;
        String key = "Server1";
        Object[] rowData;
        GridBagConstraints gbc;
        JScrollPane scrollPane;
        final String[] columnNames = {"Default", "AE Title", "Alias", "IP Address", "Port"};

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
            help3 = new JButton("Help");
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildServerPanel");

            return null;
        }

        for (int i = 0; i < 5; i++) {
            serverModel.addColumn(columnNames[i]);
        }

        serverTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        serverTable.getColumn("Default").setMinWidth(50);
        serverTable.getColumn("Default").setMaxWidth(50);
        serverTable.getColumn("AE Title").setMinWidth(75);
        serverTable.getColumn("AE Title").setMaxWidth(200);
        serverTable.getColumn("Alias").setMinWidth(75);
        serverTable.getColumn("Alias").setMaxWidth(200);
        serverTable.getColumn("IP Address").setMinWidth(200);
        serverTable.getColumn("IP Address").setMaxWidth(400);
        serverTable.getColumn("Port").setMinWidth(50);
        serverTable.getColumn("Port").setMaxWidth(100);

        Preferences.read();
        // if (Preferences.getProperty(key) == null) {
        // rowData[0] = Boolean.TRUE;
        // rowData[1] = "Kodak";
        // rowData[2] = "PACS";
        // rowData[3] = "137.222.26.22";
        // rowData[4] = "104";
        // String values[] = {
        // (String) rowData[1], (String) rowData[2],
        // (String) rowData[3], (String) rowData[4]};
        // Preferences.setProperty(key, makeString(values));
        // }

        while (Preferences.getProperty(key) != null) {

            try {
                tok = new StringTokenizer(Preferences.getProperty(key), ";");
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildServerPanel");

                return null;
            }

            try {
                rowData[1] = tok.nextToken();
                rowData[2] = tok.nextToken();
                rowData[3] = tok.nextToken();
                rowData[4] = tok.nextToken();

                if (key.equals(Preferences.getDefaultServerKey())) {
                    rowData[0] = Boolean.TRUE;
                } else {
                    rowData[0] = Boolean.FALSE;
                }

                serverModel.addRow(rowData);
                key = key.substring(0, 6) + (Integer.valueOf(key.substring(6)).intValue() + 1);
            } catch (final Exception error) {
                MipavUtil.displayError("ViewJFrameDICOMQuery.buildServerPanel:" + error.getMessage());
                // return null;
            }
        }

        try {
            serverTable.setPreferredScrollableViewportSize(new Dimension(450, 200));
            serverTable.setMinimumSize(new Dimension(450, 100));
            serverTable.setToolTipText("Double click on a server to set as default. ");
            scrollPane = new JScrollPane(serverTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setPreferredSize(new Dimension(450, 200));
            scrollPane.setMinimumSize(new Dimension(150, 100));
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildServerPanel");

            return null;
        }

        serverTable.getSelectionModel().addListSelectionListener(this);
        serverTable.addMouseListener(this);
        serverTable.getTableHeader().addMouseListener(this);
        scrollPane.setBackground(Color.black);

        gbc = setGBC(0, 0, 5, 3);
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.BOTH;

        serverPanel.add(scrollPane, gbc);

        create.setFont(ViewJFrameDICOMQuery.font12B);
        create.setActionCommand("Create");
        // create.setBackground(Color.gray.brighter());
        create.addActionListener(this);
        gbc = setGBC(0, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        serverPanel.add(create, gbc);

        edit.setFont(ViewJFrameDICOMQuery.font12B);
        edit.setActionCommand("Edit");
        // edit.setBackground(Color.gray.brighter());
        edit.addActionListener(this);
        edit.setEnabled(false);
        gbc = setGBC(1, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        serverPanel.add(edit, gbc);

        delete.setFont(ViewJFrameDICOMQuery.font12B);
        delete.setActionCommand("Delete");
        // delete.setBackground(Color.gray.brighter());
        delete.addActionListener(this);
        delete.setEnabled(false);
        gbc = setGBC(2, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        serverPanel.add(delete, gbc);

        set.setFont(ViewJFrameDICOMQuery.font12B);
        set.setActionCommand("SetAs");
        // set.setBackground(Color.gray.brighter());
        set.addActionListener(this);
        set.setEnabled(false);
        gbc = setGBC(3, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        serverPanel.add(set, gbc);

        help3.setFont(ViewJFrameDICOMQuery.font12B);
        help3.setActionCommand("Help3");
        // set.setBackground(Color.gray.brighter());
        help3.addActionListener(this);
        gbc = setGBC(4, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        serverPanel.add(help3, gbc);

        return serverPanel;
    }

    /**
     * Builds the panel that contains the storage table and four buttons. The storage table consists of five columns
     * with information the user needs to move images. The buttons are "Create", which creates a new destination;
     * "Edit", which edits the currently selected destination; "Delete", which deletes the currently selected
     * destination; and "Set As Default", which sets the current destination as default. To get the data in the table,
     * the table model reads the .preferences file and parses the data. When changes are made, they are saved to the
     * .preferences file.
     * 
     * @return the storage panel
     */
    private JPanel buildStoragePanel() {

        JPanel storagePanel;
        StringTokenizer tok;
        String key = Preferences.PREF_DICOM_STORAGE_DIR;
        Object[] rowData;
        GridBagConstraints gbc;
        JScrollPane scrollPane;
        final String[] columnNames = {"Default", "AE Title", "Alias", "Directory", "Port"};

        try {
            storagePanel = new JPanel();
            storagePanel.setLayout(new GridBagLayout());
            gbc = new GridBagConstraints();
            rowData = new Object[5];
            storageModel = new ViewTableModel();
            storageTable = new JTable(storageModel);
            if (ViewJFrameDICOMQuery.userInterface.getDICOMCatcher() != null) {
                if (ViewJFrameDICOMQuery.userInterface.getDICOMCatcher().isAlive()) {
                    activateStore = new JButton("Deactivate");
                    activateStore.setActionCommand("Deactivate");
                } else {
                    activateStore = new JButton("Activate");
                    activateStore.setActionCommand("Activate");
                }
            } else {
                activateStore = new JButton("Activate");
                activateStore.setActionCommand("Activate");
            }
            createStore = new JButton("Create");
            editStore = new JButton("Edit");
            deleteStore = new JButton("Delete");
            setStore = new JButton("Set As Default");
            help4 = new JButton("Help");
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildStoragePanel");

            return null;
        }

        for (int i = 0; i < 5; i++) {
            storageModel.addColumn(columnNames[i]);
        }

        storageTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        storageTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        storageTable.getColumn("Default").setMinWidth(50);
        storageTable.getColumn("Default").setMaxWidth(50);
        storageTable.getColumn("AE Title").setMinWidth(150);
        storageTable.getColumn("AE Title").setMaxWidth(200);
        storageTable.getColumn("Alias").setMinWidth(75);
        storageTable.getColumn("Alias").setMaxWidth(200);
        storageTable.getColumn("Directory").setMinWidth(200);
        storageTable.getColumn("Directory").setMaxWidth(400);
        storageTable.getColumn("Port").setMinWidth(50);
        storageTable.getColumn("Port").setMaxWidth(100);

        Preferences.read();

        while (Preferences.getProperty(key) != null) {

            try {
                tok = new StringTokenizer(Preferences.getProperty(key), ";");
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildStoragePanel");

                return null;
            }

            rowData[1] = tok.nextToken();
            rowData[2] = tok.nextToken();
            rowData[3] = tok.nextToken();
            rowData[4] = tok.nextToken();

            if (key.equals(Preferences.getDefaultStorageKey())) {
                rowData[0] = Boolean.TRUE;
            } else {
                rowData[0] = Boolean.FALSE;
            }

            storageModel.addRow(rowData);
            key = key.substring(0, 7) + (Integer.valueOf(key.substring(7)).intValue() + 1);
        }

        try {
            storageTable.setPreferredScrollableViewportSize(new Dimension(450, 200));
            storageTable.setMinimumSize(new Dimension(450, 100));
            storageTable.setToolTipText("Double click on a destination to set as default. ");
            scrollPane = new JScrollPane(storageTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setPreferredSize(new Dimension(450, 200));
            scrollPane.setMinimumSize(new Dimension(150, 100));
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildStoragePanel");

            return null;
        }

        storageTable.getSelectionModel().addListSelectionListener(this);
        storageTable.addMouseListener(this);
        storageTable.getTableHeader().addMouseListener(this);
        scrollPane.setBackground(Color.black);

        gbc = setGBC(0, 0, 6, 3);
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.BOTH;

        storagePanel.add(scrollPane, gbc);

        activateStore.setFont(ViewJFrameDICOMQuery.font12B);
        activateStore.addActionListener(this);
        gbc = setGBC(0, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        storagePanel.add(activateStore, gbc);

        createStore.setFont(ViewJFrameDICOMQuery.font12B);
        createStore.setActionCommand("Create");
        // createStore.setBackground(Color.gray.brighter());
        createStore.addActionListener(this);
        gbc = setGBC(1, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        storagePanel.add(createStore, gbc);

        editStore.setFont(ViewJFrameDICOMQuery.font12B);
        editStore.setActionCommand("Edit");
        // editStore.setBackground(Color.gray.brighter());
        editStore.addActionListener(this);
        editStore.setEnabled(false);
        gbc = setGBC(2, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        storagePanel.add(editStore, gbc);

        deleteStore.setFont(ViewJFrameDICOMQuery.font12B);
        deleteStore.setActionCommand("Delete");
        // deleteStore.setBackground(Color.gray.brighter());
        deleteStore.addActionListener(this);
        deleteStore.setEnabled(false);
        gbc = setGBC(3, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        storagePanel.add(deleteStore, gbc);

        setStore.setFont(ViewJFrameDICOMQuery.font12B);
        setStore.setActionCommand("SetAs");
        // setStore.setBackground(Color.gray.brighter());
        setStore.addActionListener(this);
        setStore.setEnabled(false);
        gbc = setGBC(4, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        storagePanel.add(setStore, gbc);

        help4.setFont(ViewJFrameDICOMQuery.font12B);
        help4.setActionCommand("Help4");
        // set.setBackground(Color.gray.brighter());
        help4.addActionListener(this);
        gbc = setGBC(5, 3, 1, 1);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        storagePanel.add(help4, gbc);

        return storagePanel;
    }

    /**
     * Constructs the panel that contains the query result table. There are four action-attached buttons to facilitate
     * different levels of query: up, down, send query, and move images. Up moves the query up a level; down moves it
     * down a level; send query sends a query; and move images moves an image or a set of images at that level.
     * 
     * @return The constructed panel.
     */
    private JPanel buildTextPanel() {
        JPanel buttonPanel, rootPanel;
        TitledBorder rootBorder;
        String label;
        GridBagConstraints gbc;
        GridBagLayout grid;
        EtchedBorder border;
        JScrollPane scrollPane;

        try {
            gbc = new GridBagConstraints();
            grid = new GridBagLayout();
            border = new EtchedBorder();
            rootPanel = new JPanel();
            grid = new GridBagLayout();
            label = "Query Result";
            rootBorder = new TitledBorder(label);
            buttonPanel = new JPanel();
            buttonPanel.setLayout(new GridLayout(1, 4));
            queryLev = new JLabel("Query Level: Patient");
            up = new JButton("Up");
            down = new JButton("Down");
            cancelQ = new JButton("Cancel");
            gbc.insets = new Insets(0, 0, 0, 0);
            queryTableModel = new ViewTableModel();
            queryResultTable = new JTable(queryTableModel);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildTextPanel");

            return null;
        }

        rootPanel.setLayout(grid);
        rootPanel.setForeground(Color.black);

        rootBorder.setTitleColor(Color.black);
        rootBorder.setTitleFont(ViewJFrameDICOMQuery.font12B);
        rootBorder.setBorder(border);
        rootPanel.setBorder(rootBorder);

        // buttonPanel.setForeground(Color.black);

        queryLev.setFont(ViewJFrameDICOMQuery.font12B);
        buttonPanel.add(queryLev);

        up.setFont(ViewJFrameDICOMQuery.font12B);
        up.setActionCommand("ResultUp");
        up.addActionListener(this);
        up.setEnabled(false);
        up.setToolTipText("Move the query up one level ");
        buttonPanel.add(up);

        down.setFont(ViewJFrameDICOMQuery.font12B);
        down.setActionCommand("SendQuery");
        down.addActionListener(this);
        down.setEnabled(false);
        down.setToolTipText("Move the query down one level ");
        buttonPanel.add(down);

        cancelQ.setFont(ViewJFrameDICOMQuery.font12B);
        cancelQ.setActionCommand("CancelQuery");
        cancelQ.addActionListener(this);
        cancelQ.setEnabled(false);
        cancelQ.setToolTipText("Cancel query");
        buttonPanel.add(cancelQ);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridheight = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        rootPanel.add(buttonPanel, gbc);

        // if (queryLevel == PATIENT_LEVEL) {
        final String[] columnName = {"Pat. Name", "Pat. ID", "Referring Physician"};

        for (int i = 0; i < 3; i++) {
            queryTableModel.addColumn(columnName[i]);
            // }
        }

        queryResultTable.getTableHeader().setToolTipText("Click on the header to sort ");
        queryResultTable.setToolTipText("Double click to query ");
        setColumns();

        try {
            queryResultTable.setPreferredScrollableViewportSize(new Dimension(450, 200));
            queryResultTable.setMinimumSize(new Dimension(450, 200));
            scrollPane = new JScrollPane(queryResultTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setPreferredSize(new Dimension(450, 200));
            scrollPane.setMinimumSize(new Dimension(150, 100));
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildTextPanel");

            return null;
        }

        queryResultTable.getSelectionModel().addListSelectionListener(this);
        queryResultTable.addMouseListener(this);

        queryResultTable.getTableHeader().addMouseListener(this);

        scrollPane.setBackground(Color.black);
        gbc.gridheight = GridBagConstraints.REMAINDER;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 10.0;
        gbc.weighty = 10.0;
        rootPanel.add(scrollPane, gbc);
        rootPanel.validate();

        return rootPanel;
    }

    /**
     * Constructs the titled border for the panel.
     * 
     * @param s a string carried the name of the border
     * 
     * @return the constructed titled border
     */
    private TitledBorder buildTitledBorder(final String s) {
        TitledBorder titledBorder;

        try {
            titledBorder = new TitledBorder(s);
            titledBorder.setBorder(new EtchedBorder());
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildTitledBorder");

            return null;
        }

        titledBorder.setTitleColor(Color.black);
        titledBorder.setTitleFont(ViewJFrameDICOMQuery.font12B);

        return titledBorder;
    }

    /**
     * To check if the date is a valid date in the calendar.
     * 
     * @param date The date to be checked.
     * 
     * @return The new calendar class with correctd date.
     */
    private Calendar checkCalendar(final Calendar date) {
        final int day = date.get(Calendar.DATE);
        final int endOfMonth = endOfMonth(date);

        if (day > endOfMonth) {
            final int year = date.get(Calendar.YEAR);
            final int month = date.get(Calendar.MONTH);
            date.set(year, month, endOfMonth);
        }

        return date;
    }

    /**
     * Checks the date to be sure the user didn't enter the start after the end.
     * 
     * @param year year of the date to check
     * @param month month of the date to check
     * @param day day of the date to check
     * @param start flag that says if this is the start date or end date
     * 
     * @return boolean that tells if the date checked out
     */
    private boolean checkDate(final int year, final int month, final int day, final boolean start) {

        if (start) {

            if ( (year > getEndYear()) || ( (year == getEndYear()) && (month > getEndMonth()))
                    || ( (year == getEndYear()) && (month == getEndMonth()) && (day > getEndDay()))) {
                MipavUtil.displayError("Start date must be before end date");

                return false;
            }
        } else {

            if ( (year < getStartYear()) || ( (year == getStartYear()) && (month < getStartMonth()))
                    || ( (year == getStartYear()) && (month == getStartMonth()) && (day < getStartDay()))) {
                MipavUtil.displayError("End date must be after start date");

                return false;

            }
        }

        return true;
    }

    /**
     * Constructs and new class and copy the year, month, day paramenters.
     * 
     * @param date The source Calendar class which will be copied from.
     * 
     * @return The newly constructed calendar class with the same info as source.
     */
    private Calendar copyOfCalendar(final Calendar date) {
        final Calendar tmp = Calendar.getInstance();
        final int year = date.get(Calendar.YEAR);
        final int month = date.get(Calendar.MONTH);
        final int day = date.get(Calendar.DATE);

        tmp.set(year, month, day);

        return tmp;
    }

    /**
     * Calculates the last date of the month.
     * 
     * @param tmp The calendar to be calculated.
     * 
     * @return the last date of the month.
     */
    private int endOfMonth(final Calendar tmp) {
        final Calendar date = copyOfCalendar(tmp);
        final int year = date.get(Calendar.YEAR);
        final int month = date.get(Calendar.MONTH);

        date.set(year, month, 27);

        for (int i = 27; i < 33; i++) {
            date.roll(Calendar.DATE, true);

            if (date.get(Calendar.DATE) < i) {
                return i;
            }
        }

        return 31;
    }

    /**
     * Returns the new date of num days before date.
     * 
     * @param date the date to get the days before of
     * @param num the number of days before
     * 
     * @return the calendar with the right number of years before
     */
    private Calendar getDaysBefore(Calendar date, final int num) {
        int day;

        for (int i = 0; i < num; i++) {
            day = date.get(Calendar.DATE);
            date.roll(Calendar.DATE, false);

            if (day < date.get(Calendar.DATE)) {
                date = getMonthsBefore(date, 1);
            }
        }

        return checkCalendar(date);
    }

    /**
     * Gets all the ending date of the patient's study from the all three comboboxes.
     * 
     * @return a Calendar type stores the ending date information
     */
    private Calendar getEndCalendar() {
        endCalendar.set(getEndYear(), getEndMonth(), getEndDay());
        endCalendar = checkCalendar(endCalendar);

        if (endCalendar.before(startCalendar)) {
            endCalendar = assignCalendar(startCalendar, endCalendar);
        }

        setEndCalendar(endCalendar);

        return endCalendar;
    }

    /**
     * Gets the ending day of the study in the endDayBox combobox.
     * 
     * @return integer: the ending day of the study
     */
    private int getEndDay() {
        return itemToInteger(endDayBox.getSelectedItem());
    }

    /**
     * Gets the ending month of the study in the endMonthBox combobox.
     * 
     * @return integer: the ending month of the study
     */
    private int getEndMonth() {
        return itemToInteger(endMonthBox.getSelectedItem());
    }

    /**
     * Gets the ending year of the study in the endYearBox combobox.
     * 
     * @return integer: the ending year of the study
     */
    private int getEndYear() {
        return itemToInteger(endYearBox.getSelectedItem());
    }

    /**
     * Returns the new date of num months before date.
     * 
     * @param tmp the date to get the months before of
     * @param num the number of months before
     * 
     * @return the calendar with the right number of months before
     */
    private Calendar getMonthsBefore(final Calendar tmp, final int num) {
        int month;
        Calendar date = tmp;

        for (int i = 0; i < num; i++) {
            month = date.get(Calendar.MONTH);
            date.roll(Calendar.MONTH, false);

            if (month < date.get(Calendar.MONTH)) {
                date = getYearsBefore(date, 1);
            }
        }

        return checkCalendar(date);
    }

    /**
     * Gets the physician's name from the physText field.
     * 
     * @return The string that stores the physician's name.
     */
    private String getPhysName() {
        return physText.getText();
    }

    /**
     * Gets the Patient's ID from the ptIDText field.
     * 
     * @return The string that stores the patient ID.
     */
    private String getPtID() {
        return ptIDText.getText();
    }

    /**
     * Gets the Patient name from the ptNameText field.
     * 
     * @return The string that contained the patient's name.
     */
    private String getPtName() {
        return ptNameText.getText().trim();
    }

    /**
     * Gets all the starting date of the patient's study from the all three comboboxes.
     * 
     * @return a Calendar type stores the starting date information
     */
    private Calendar getStartCalendar() {
        startCalendar.set(getStartYear(), getStartMonth(), getStartDay());
        startCalendar = checkCalendar(startCalendar);

        if (startCalendar.after(todayCalendar)) {
            startCalendar = assignCalendar(todayCalendar, startCalendar);
        }

        setStartCalendar(startCalendar);

        return startCalendar;
    }

    /**
     * Gets the starting day of the study in the startDayBox combobox.
     * 
     * @return integer: the starting day of the study
     */
    private int getStartDay() {
        return itemToInteger(startDayBox.getSelectedItem());
    }

    /**
     * Gets the starting Month of the study in the startMonthBox combobox.
     * 
     * @return integer: the starting month of the study
     */
    private int getStartMonth() {
        return itemToInteger(startMonthBox.getSelectedItem());
    }

    /**
     * Gets the starting year of the study in the startYearBox combobox.
     * 
     * @return integer: the starting year of the study
     */
    private int getStartYear() {
        return itemToInteger(startYearBox.getSelectedItem());
    }

    /**
     * Gets the study id from the studyNOText field.
     * 
     * @return The string that stores the study number.
     */
    private String getStudyNo() {
        return studyNOText.getText();
    }

    /**
     * Returns the new date of num years before date.
     * 
     * @param tmp the date to get the years before of
     * @param num the number of years before
     * 
     * @return the calendar with the right number of years before
     */
    private Calendar getYearsBefore(final Calendar tmp, final int num) {
        final Calendar date = tmp;

        for (int i = 0; i < num; i++) {

            if (date.get(Calendar.YEAR) <= ViewJFrameDICOMQuery.DEFAULT_MIN_YEAR) {
                date.set(ViewJFrameDICOMQuery.DEFAULT_MIN_YEAR, 0, 1);

                break;
            }

            date.roll(Calendar.YEAR, false);
        }

        return checkCalendar(date);
    }

    /**
     * Converts the combo box item to its corresponding integer. Year and date - are numbers as they represented. Month -
     * will be converted to integer, starting with 0 = January,
     * 
     * @param item DOCUMENT ME!
     * 
     * @return The appropriated integer number.
     */
    private int itemToInteger(final Object item) {
        final String s = item.toString();
        int tmp = 0;

        // try - if the item is Year or Day
        // catch if the item is Month
        try {
            tmp = (new Integer(s).intValue());
        } catch (final NumberFormatException error) { // month item

            for (tmp = 0; ( (tmp < 12) && (s.compareTo(ViewJFrameDICOMQuery.monthString[tmp]) != 0)); tmp++) {
                ;
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.itemToInteger");

            return -1;
        }

        return tmp;
    }

    /**
     * Finds the last name of the current patient.
     * 
     * @param name patient's name
     * 
     * @return the last name
     */
    private String lastName(final String name) {
        int count = 0;

        for (int i = 0; i < (name.length() - 1); i++) {

            if (name.charAt(i) == ',') {
                count = i;
            }
        }

        return name.substring(0, count);
    }

    /**
     * Makes the .preferences string out of the array.
     * 
     * @param stuff array to make the string from
     * 
     * @return the string with the array items separated by semicolons
     */
    private String makeString(final String[] stuff) {
        String newString = "";

        for (final String element : stuff) {
            newString = newString + element.trim() + ";";
        }

        return newString;
    }

    /**
     * Transforms the name from the usual form to the DICOM form.
     * 
     * @param name the usual name
     * 
     * @return the DICOM version of the name
     */
    private String originalName(final String name) {
        String lastname = "", firstname = "", middlename = "";
        int count1 = -1, count2 = -1;

        for (int i = 0; i < (name.length() - 1); i++) {

            if (name.charAt(i) == ' ') {

                if (count1 == -1) {
                    count1 = i;
                } else if (count2 == -1) {
                    count2 = i;
                }
            }
        }

        if ( (count1 != -1) && (count2 != -1)) {
            lastname = name.substring(0, count1 - 1);
            firstname = name.substring(count1 + 1, count2);
            middlename = name.substring(count2 + 1);

            return (lastname.trim() + "^" + firstname.trim() + " " + middlename.trim() + "^^^");
        } else {
            return name;
        }
    }

    /**
     * Transforms the name from the DICOM form to a more usual form.
     * 
     * @param name the original name
     * 
     * @return the more usual version of the name
     */
    private String parseName(final String name) {
        String lastname = "", firstname = "", middlename = "";
        int count1 = -1, count2 = -1, count3 = -1;

        for (int i = 0; i < (name.length() - 1); i++) {

            if (name.charAt(i) == '^') {

                if (count1 == -1) {
                    count1 = i;
                } else if (count2 == -1) {
                    count2 = i;
                } else if (count3 == -1) {
                    count3 = i;
                }
            }
        }

        if ( (count1 != -1) && (count2 != -1) && (count3 != -1)) {
            lastname = name.substring(0, count1);
            firstname = name.substring(count1 + 1, count2);
            middlename = name.substring(count2 + 1, count3);

            return (lastname + ", " + firstname + " " + middlename);
        } else {
            return name;
        }
    }

    /**
     * Sends a move request at the current level, setting up the data objects appropriately. It runs the move request in
     * its own thread.
     * 
     * @param type type of move request: PATIENT, STUDY, SERIES, or IMAGE.
     */
    private void sendMoveRequest(final int type) {
        DICOM_PDUService pdu;
        final DICOM_Move move = new DICOM_Move();
        DICOM_Object dataObject;
        Thread moveRequestThread;

        switch (type) {

            case PATIENT_LEVEL: // ptIDText.setText("");

                // studyNOText.setText("");
                // physText.setText("");
                dataObject = move.setMovePatientData(getPtID().trim());
                messageTable.updateRow();
                break;

            case STUDY_LEVEL: // studyNOText.setText("");
                dataObject = move.setMoveStudyData(getPtID().trim(), studyInstanceUID);
                messageTable.updateRow();
                break;

            case SERIES_LEVEL:
                dataObject = move.setMoveSeriesData(getPtID().trim(), studyInstanceUID, seriesInstanceUID);
                messageTable.updateRow();
                break;

            case IMAGE_LEVEL:
                dataObject = move.setMoveImageData(getPtID().trim(), studyInstanceUID, seriesInstanceUID,
                        SOPInstanceUID);
                messageTable.updateRow();
                break;

            default: // ptIDText.setText("");

                // studyNOText.setText("");
                // physText.setText("");
                // dataObject = move.setMovePatientData(getPtID().trim());
                return;
        }

        pdu = move.connectToServer(DICOM_PDUService.parseServerInfo(Preferences.getProperty(Preferences
                .getDefaultServerKey()))[0]); //

        if (pdu == null) {
            MipavUtil
                    .displayError("Connection to DICOM database is closed.\nCheck that you properly obtained a ticket\nand specified a valid host.");

            return;
        }

        final String[] serverInfo = DICOM_PDUService.parseServerInfo(Preferences.getProperty(Preferences
                .getDefaultServerKey()));
        final String[] storageInfo = DICOM_PDUService.parseServerInfo(Preferences.getProperty(Preferences
                .getDefaultStorageKey()));

        try {

            if (ViewJFrameDICOMQuery.userInterface.getDICOMCatcher() == null) {
                ViewJFrameDICOMQuery.userInterface.setDICOMCatcher(new DICOM_Receiver()); // //////
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendMoveRequest");

            return;
        }

        if (ViewJFrameDICOMQuery.userInterface.getDICOMCatcher().isAlive() == false) {
            Preferences.debug(" ***** Receiver not alive. !!!!!!!!!!!! \n");
            // Should we display an error message.
            // Should we return; ?????? 12/15/99
        }

        pdu.setDICOMMessageDisplayer(messageTable); // ////////////////////

        final int row = messageTable.updateRow();
        ViewJFrameDICOMQuery.userInterface.getDICOMCatcher().setDICOMMessageDisplayer(messageTable); // /////////
        DICOMDisplayer.setMessageType(row, DICOMDisplayer.SOURCE);
        messageTable.showMessage(serverInfo[0]);

        final String title = storageInfo[0];
        byte[] localAppTitle;

        try {
            localAppTitle = new byte[16];
            moveRequestThread = new Thread(move);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendMoveRequest");

            return;
        }

        DICOM_Util.fillByteArray(localAppTitle, ' ');

        for (int i = 0; i < title.length(); i++) {
            localAppTitle[i] = (byte) title.charAt(i);
        }

        final int id = move.setMsgID();
        DICOMDisplayer.setMessageType(row, DICOMDisplayer.STATUS);
        messageTable.showMessage("Sending request");
        DICOMDisplayer.setMessageType(row, DICOMDisplayer.ID);
        messageTable.showMessage(String.valueOf(id));
        messageTable.repaint();
        messageTable.setSucceeded(false);

        move.setMoveParameters(pdu, dataObject, localAppTitle);
        moveRequestThread.setPriority(Thread.NORM_PRIORITY);
        moveRequestThread.start();

        final MoveRequestInfo mr = new MoveRequestInfo(pdu, moveRequestThread);
        moveRequestHash.put(String.valueOf(id), mr);
    }

    /**
     * This method sets up the information for the DICOM query depending on the type, sends the query, and parses the
     * information that the server sends back. On a study query, it filters out studies before the start date or after
     * the end date.
     * 
     * @param type type of query: PATIENT, STUDY, SERIES, or IMAGE.
     */
    private void sendQuery(final int type) {
        DICOM_Object dataObject;
        String month;
        String day;

        if ( (getStartMonth() + 1) < 10) {
            month = "0" + (getStartMonth() + 1);
        } else {
            month = String.valueOf(getStartMonth() + 1);
        }

        if ( (getStartDay()) < 10) {
            day = "0" + (getStartDay());
        } else {
            day = String.valueOf(getStartDay());
        }

        final String start = "" + getStartYear() + month + day;

        if ( (getEndMonth() + 1) < 10) {
            month = "0" + (getEndMonth() + 1);
        } else {
            month = String.valueOf(getEndMonth() + 1);
        }

        if (getEndDay() < 10) {
            day = "0" + getEndDay();
        } else {
            day = String.valueOf(getEndDay());
        }

        final String end = "" + getEndYear() + month + day;

        setEnabled(down, false);
        setEnabled(move, false);

        try {

            if (dicomQuery != null) {
                dicomQuery.setStop();
            }

            dicomQuery = new DICOM_Query(this, type);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendQuery");

            return;
        }

        switch (type) {

            case PATIENT_LEVEL:
                setEnabled(up, false);
                dataObject = dicomQuery.setQueryStudyData(originalName(getPtName().trim()).concat("*"), getPtID()
                        .trim(), getStudyNo().trim(), start + "-" + end, getPhysName().trim());

                // dataObject =
                // dicomQuery.setQueryPatientData(getPtName().trim().concat("*"),
                // "");
                break;

            case STUDY_LEVEL:
                studyNOText.setText("");
                setEnabled(up, true);

                // dataObject =
                // dicomQuery.setQueryStudyData(originalName(getPtName().trim()).concat("*"),
                // getPtID().trim(),
                // getStudyNo().trim(), start+"-"+end, getPhysName().trim());
                // If have the patient ID no need to send patient name. Full name
                // seems to cause agfa server to choke.
                // Works with just last name. We may want to add just the last name.
                dataObject = dicomQuery.setQueryStudyData("", getPtID().trim(), getStudyNo().trim(), start + "-" + end,
                        getPhysName().trim());
                break;

            case SERIES_LEVEL:
                dataObject = dicomQuery.setQuerySeriesData(getPtID().trim(), studyInstanceUID);
                setEnabled(up, true);
                break;

            case IMAGE_LEVEL:
                dataObject = dicomQuery.setQueryImagesData(getPtID().trim(), studyInstanceUID, seriesInstanceUID);
                setEnabled(up, true);
                break;

            default:
                ptIDText.setText("");
                studyNOText.setText("");
                physText.setText("");
                setEnabled(up, false);
                dataObject = dicomQuery.setQueryPatientData(getPtName().trim().concat("*"), getPtID().trim());
                break;
        }

        Preferences.debug("ViewJFrameDICOMQuery.sendQuery. \n");
        queryPDU = dicomQuery.connectToServer(DICOM_PDUService.parseServerInfo(Preferences.getProperty(Preferences
                .getDefaultServerKey()))[0]);
        dicomQuery.setParameters(queryPDU, dataObject);

        if (queryPDU == null) {
            MipavUtil
                    .displayError("Connection to DICOM database is closed.\nCheck that you properly connected\nand specified a valid host.");
            send.setEnabled(true);
            cancelQ.setEnabled(false);

            return;
        }

        // query.sendQuery(pdu, dataObject); // will go away
        // pdu.close(); // closes service connection -- will go away
        // and put in DICOMQuery.sendQuery
        if (sendQueryThread != null) {

            // sendQueryThread.stop();
        }

        try {
            sendQueryThread = new Thread(dicomQuery);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sendQuery");
        }

        sendQueryThread.setPriority(Thread.NORM_PRIORITY);
        queryMsgID = dicomQuery.setMsgID();
        sendQueryThread.start();
    }

    /**
     * Sets the columns of the query table to certain values.
     */
    private void setColumns() {

        TableColumn tableColumn;
        queryResultTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);

        if (queryLevel == ViewJFrameDICOMQuery.PATIENT_LEVEL) {

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(0));
            tableColumn.setMaxWidth(500);
            tableColumn.setMinWidth(250);
            tableColumn.setPreferredWidth(250);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(1));
            tableColumn.setMaxWidth(500);
            tableColumn.setMinWidth(100);
            tableColumn.setPreferredWidth(150);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(2));
            tableColumn.setMaxWidth(1000);
            tableColumn.setMinWidth(300);
            tableColumn.setPreferredWidth(300);
        } else if (queryLevel == ViewJFrameDICOMQuery.STUDY_LEVEL) {

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(0));
            tableColumn.setMaxWidth(300);
            tableColumn.setMinWidth(100);
            tableColumn.setPreferredWidth(100);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(1));
            tableColumn.setMaxWidth(300);
            tableColumn.setMinWidth(100);
            tableColumn.setPreferredWidth(100);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(2));
            tableColumn.setMaxWidth(300);
            tableColumn.setMinWidth(100);
            tableColumn.setPreferredWidth(100);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(3));
            tableColumn.setMaxWidth(1000);
            tableColumn.setMinWidth(350);
            tableColumn.setPreferredWidth(400);
        } else if (queryLevel == ViewJFrameDICOMQuery.SERIES_LEVEL) {

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(0));
            tableColumn.setMaxWidth(30);
            tableColumn.setMinWidth(30);
            tableColumn.setPreferredWidth(30);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(1));
            tableColumn.setMaxWidth(300);
            tableColumn.setMinWidth(100);
            tableColumn.setPreferredWidth(100);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(2));
            tableColumn.setMaxWidth(300);
            tableColumn.setMinWidth(100);
            tableColumn.setPreferredWidth(100);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(3));
            tableColumn.setMaxWidth(25);
            tableColumn.setMinWidth(25);
            tableColumn.setPreferredWidth(25);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(4));
            tableColumn.setMaxWidth(2000);
            tableColumn.setMinWidth(300);
            tableColumn.setPreferredWidth(300);
            // tableColumn.sizeWidthToFit();
            // tableColumn.sizeWidthToFit()
        } else if (queryLevel == ViewJFrameDICOMQuery.IMAGE_LEVEL) {

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(0));
            tableColumn.setMaxWidth(30);
            tableColumn.setMinWidth(30);
            tableColumn.setPreferredWidth(30);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(1));
            tableColumn.setMaxWidth(300);
            tableColumn.setMinWidth(100);
            tableColumn.setPreferredWidth(100);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(2));
            tableColumn.setMaxWidth(300);
            tableColumn.setMinWidth(100);
            tableColumn.setPreferredWidth(100);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(3));
            tableColumn.setMaxWidth(300);
            tableColumn.setMinWidth(150);
            tableColumn.setPreferredWidth(150);

            tableColumn = queryResultTable.getColumn(queryTableModel.getColumnName(4));
            tableColumn.setMaxWidth(2000);
            tableColumn.setMinWidth(150);
            tableColumn.setPreferredWidth(150);

        }
    }

    /**
     * Sets the specified button to enabled or disabled, depending on the boolean parameter.
     * 
     * @param button button to set
     * @param enabled true is enabled, false if disabled
     */
    private void setEnabled(final JButton button, final boolean enabled) {
        button.setEnabled(enabled);
    }

    /**
     * Set the end calendar.
     * 
     * @param date date to set it to
     */
    private void setEndCalendar(final Calendar date) {
        setEndYear(date.get(Calendar.YEAR));
        setEndMonth(date.get(Calendar.MONTH));
        setEndDay(date.get(Calendar.DATE));
    }

    /**
     * Sets the end day.
     * 
     * @param day day to set it to
     */
    private void setEndDay(final int day) {

        if ( !checkDate(getEndYear(), getEndMonth(), day, false)) {
            return;
        }

        endDayBox.setSelectedIndex(day - 1);
    }

    /**
     * Sets the end month.
     * 
     * @param month month to set it to
     */
    private void setEndMonth(final int month) {

        if ( !checkDate(getEndYear(), month, getEndDay(), false)) {
            return;
        }

        endMonthBox.setSelectedIndex(month);
    }

    /**
     * Sets the end year.
     * 
     * @param year year to set it to
     */
    private void setEndYear(final int year) {
        final int thisYear = todayCalendar.get(Calendar.YEAR);

        if ( !checkDate(year, getEndMonth(), getEndDay(), false)) {
            return;
        }

        if (thisYear < year) {
            return;
        }

        endYearBox.setSelectedIndex(thisYear - year);
    }

    /**
     * A helper method for adding a component using GridBagLayout, so we don't have to set up the x, y, width, and
     * height over and over again.
     * 
     * @param x GridBagConstraints.gridx
     * @param y GridBagConstraints.gridy
     * @param w GridBagContraints.gridwidth
     * @param h GridBagConstraints.gridheight
     * 
     * @return the grid bag constraints
     * 
     * @see GridBagConstraints
     */
    private GridBagConstraints setGBC(final int x, final int y, final int w, final int h) {
        GridBagConstraints gbc;

        try {
            gbc = new GridBagConstraints();
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.setGBC");

            return null;
        }

        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;

        return gbc;
    }

    /**
     * Sets the start calender to the calendar given.
     * 
     * @param date calendar to set it to
     */
    private void setStartCalendar(final Calendar date) {
        setStartYear(date.get(Calendar.YEAR));
        setStartMonth(date.get(Calendar.MONTH));
        setStartDay(date.get(Calendar.DATE));
    }

    /**
     * Displays the starting day in the startDayBox combobox.
     * 
     * @param day an integer of the starting date: day
     */
    private void setStartDay(final int day) {

        if ( !checkDate(getStartYear(), getStartMonth(), day, true)) {
            return;
        }

        startDayBox.setSelectedIndex(day - 1);
    }

    /**
     * Displays the starting month in the startMonthBox combobox.
     * 
     * @param month an integer of the starting date: month
     */
    private void setStartMonth(final int month) {

        if ( !checkDate(getStartYear(), month, getStartDay(), true)) {
            return;
        }

        startMonthBox.setSelectedIndex(month);
    }

    /**
     * Displays the starting year in the startYearBox combobox.
     * 
     * @param year an integer of the starting date: year
     */
    private void setStartYear(final int year) {
        final int smallestYear = ViewJFrameDICOMQuery.DEFAULT_MIN_YEAR;

        if (smallestYear > year) {
            return;
        }

        if ( !checkDate(year, getStartMonth(), getStartDay(), true)) {
            return;
        }

        startYearBox.setSelectedIndex(year - smallestYear);
    }

    /**
     * Sets up the gridbag layout of the combo boxes.
     * 
     * @return The gbc for the combo boxes
     */
    private GridBagConstraints setupComboBoxLayout() {
        GridBagConstraints gbc;

        try {
            gbc = new GridBagConstraints();
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.setupComboBoxLayout");

            return null;
        }

        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = GridBagConstraints.RELATIVE;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        return gbc;
    }

    /**
     * Sets up the gridBagLayout of the textField label.
     * 
     * @return the gbc layout for label
     */
    private GridBagConstraints setupLabelLayout() {
        GridBagConstraints gbc;
        Insets insets;

        try {
            gbc = new GridBagConstraints();
            insets = new Insets(0, 0, 0, 0);
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.setupLabelLayout");

            return null;
        }

        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.NONE;
        gbc.insets = insets;
        gbc.weightx = 0.01;
        gbc.weighty = 1.0;

        return gbc;
    }

    /**
     * Sets ups the send file listing.
     * 
     * @param dir the directory from which to build the listing
     * 
     * @return DOCUMENT ME!
     */
    private boolean setupSendFileListing(final String dir) {
        File fileDir = null;
        String[] fileList;
        final Object[] fileObj = new Object[1];

        try {
            fileDir = new File(dir); // + File.separatorChar);
        } catch (final OutOfMemoryError error) {

            // System.gc();
            // MipavUtil.displayError("FileIO: " + error);
            return false;
        }

        if (fileDir.isFile() == true) {
            return false;
        }

        if (fileDir.isDirectory() == false) {
            fileDir = new File(dir + File.separatorChar);
            // return false;
        }

        // Read directory and find no. of images
        fileList = fileDir.list();

        if (fileList != null) {

            for (final int i = 0; i < sendModel.getRowCount();) {
                sendModel.removeRow(0);
            }

            for (final String element : fileList) {
                fileObj[0] = element;
                sendModel.addRow(fileObj);
            }
        } else {
            return false;
        }

        return true;
    }

    /**
     * Update Send tab to reflect any changes in hosts.
     */
    private void setupSendTab() {
        Boolean test;
        String str = new String();

        for (int r = 0; r < storageTable.getRowCount(); r++) {
            test = (Boolean) storageTable.getValueAt(r, 0);

            if (test.booleanValue() == true) {
                str = (String) storageTable.getValueAt(r, 3);
            }
        }

        sourceTextF.setText(str);

        /*
         * String strs[] = new String[serverTable.getRowCount()]; for (int r = 0; r < serverTable.getRowCount(); r++) {
         * test = (Boolean)serverTable.getValueAt(r, 0); str = (String)serverTable.getValueAt(r,1); if
         * (test.booleanValue() == true) { strs[0] = str; } else if (r == 0) { strs[1] = str; } else { strs[r] = str; } }
         */
        sendDestCBox.removeAllItems();

        int defaultChoice = -1;

        for (int s = 0; s < serverTable.getRowCount(); s++) {

            if ((Boolean) serverTable.getValueAt(s, 0) == Boolean.TRUE) {
                defaultChoice = s;

                break;
            }
        }

        if (defaultChoice != -1) {
            sendDestCBox.addItem(serverTable.getValueAt(defaultChoice, 1));
        }

        for (int r = 0; r < serverTable.getRowCount(); r++) {

            if (r != defaultChoice) {
                sendDestCBox.addItem(serverTable.getValueAt(r, 1));
            }
        }

        sendDestCBox.validate();
        setupSendFileListing(sourceTextF.getText());
    }

    /**
     * Sets up the gridbaglayout of the textField.
     * 
     * @return The gbc layout for the text field.
     */
    private GridBagConstraints setupTextFieldLayout() {
        GridBagConstraints gbc;

        try {
            gbc = new GridBagConstraints();
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.buildTitledBorder");

            return null;
        }

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = GridBagConstraints.RELATIVE;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        return gbc;
    }

    /**
     * Sorts the column of the table model. If reverse is true, sorts in reverse order.
     * 
     * @param model table model to sort on
     * @param col column to sort on
     * @param reverse whether or not to sort in reverse order.
     */
    private void sort(final ViewTableModel model, final int col, final boolean reverse) {

        Vector<DICOM_UID> tmpUIDs;
        DICOM_UID tmpUID;

        if (model.getColumnName(col).equals("Study ID") || model.getColumnName(col).equals("#")) {

            try {
                tmpUIDs = new Vector<DICOM_UID>();
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameDICOMQuery.sort");

                return;
            }

            for (final DICOM_UID element : uids) {
                tmpUIDs.addElement(element);
            }

            for (int p = 1; p < model.getRowCount(); p++) {

                for (int j = 0; j < p; j++) {

                    if (model.getValueAt(p, col) != null) {

                        if (reverse) {

                            if (Integer.valueOf((String) model.getValueAt(p, col)).intValue() > Integer.valueOf(
                                    (String) model.getValueAt(j, col)).intValue()) {
                                tmpUID = (tmpUIDs.elementAt(p));
                                tmpUIDs.removeElementAt(p);
                                tmpUIDs.insertElementAt(tmpUID, j);
                                model.moveRow(p, p, j);

                                break;
                            }
                        } else {

                            if (Integer.valueOf((String) model.getValueAt(p, col)).intValue() < Integer.valueOf(
                                    (String) model.getValueAt(j, col)).intValue()) {
                                tmpUID = (tmpUIDs.elementAt(p));
                                tmpUIDs.removeElementAt(p);
                                tmpUIDs.insertElementAt(tmpUID, j);
                                model.moveRow(p, p, j);

                                break;
                            }
                        }
                    }
                }
            }

            for (int i = 0; i < uids.length; i++) {
                uids[i] = (tmpUIDs.elementAt(i));
            }
        } else {

            for (int p = 1; p < model.getRowCount(); p++) {

                for (int j = 0; j < p; j++) {

                    if (model.getValueAt(p, col) != null) {

                        if (reverse) {

                            if ( ((String) model.getValueAt(p, col)).compareTo((String) model.getValueAt(j, col)) > 0) {
                                model.moveRow(p, p, j);

                                break;
                            }
                        } else {

                            if ( ((String) model.getValueAt(p, col)).compareTo((String) model.getValueAt(j, col)) < 0) {
                                model.moveRow(p, p, j);

                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Simple class to support move requests.
     */
    private class MoveRequestInfo {

        /** DOCUMENT ME! */
        Thread moveThread;

        /** DOCUMENT ME! */
        DICOM_PDUService pdu;

        /**
         * Creates a new MoveRequestInfo object.
         * 
         * @param pdu DOCUMENT ME!
         * @param moveThread DOCUMENT ME!
         */
        public MoveRequestInfo(final DICOM_PDUService pdu, final Thread moveThread) {
            this.pdu = pdu;
            this.moveThread = moveThread;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public DICOM_PDUService getPDU() {
            return pdu;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public Thread getThread() {
            return moveThread;
        }
    }

}
