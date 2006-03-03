package gov.nih.mipav.view;

import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import java.awt.event.*;
import java.io.*;
import java.awt.*;

/**
 *   This class produces a message frame where user data, logging and debug
 *   information can be displayed. The frame can be resize and a scroll pane is
 *   used where scroll bars are displayed as needed. This frame also
 *   gives the user the ability to edit and save the data as needed to
 *   a text file. Each image (ModelImage) keeps a data and a logging (JTextAreas)
 *   objects to record information specific to itself. Only one global data object and
 *   and one debug text object exists for the whole MIPAV application.
 *
 *		@version    1.0 Oct 24, 1998
 *		@author     Matthew J. McAuliffe, Ph.D.
 *
 */
public class ViewJFrameMessage
    extends JFrame implements ActionListener, ChangeListener {

    /** Are used to indicate which of the 2 JTextAreas the data (message) is to be displayed
     * @see #setMessage(String, int)
     */
    public static final int DATA = 0;
    public static final int DEBUG = 1;

    private Insets frameInsets;
    private JToolBar tBar;
    private JMenuBar menu;
    private JTabbedPane tabbedPane;

    private JButton delTabButton = null;

    private JMenuItem removeCurrentTab = null;

    /** Reference to the ViewUserInterface Object */
    private ViewUserInterface UI;

    /** Indicates last state of frame - NORMAL or ICONIFIED */
    private int lastState = Frame.NORMAL;

    /**
     *  Creates new frame.
     *  @param title     Title of dialog frame
     *  @param _UI       GUI object where working directory is located
     */
    public ViewJFrameMessage(String title, ViewUserInterface _UI) {
        super(title);

        UI = _UI;
        setResizable(true);
        init(title);
    }

    /**
     *   Sets the display state of the Frame to be either Frame.NORMAL or Frame.ICONIFIED
     *   @param state Should be either Frame.NORMAL or Frame.ICONIFIED
     */
    public void setLastState(int state) {
        lastState = state;
    }

    /**
     *   Gets the display state of the Frame ( either Frame.NORMAL or Frame.ICONIFIED )
     *   @return state Should be either Frame.NORMAL or Frame.ICONIFIED
     */
    public int getLastState() {
        return lastState;
    }

    /**
     * Used by the script parser
     * @return JTextArea
     */
    public JTextArea getData() {
        return ((ScrollTextArea)tabbedPane.getComponentAt(DATA)).getTextArea();
    }

    /**
     *  Initializes the dialog box to a certain size and adds
     *  the components.
     *  @param title Title of the dialog box
     */
    private void init(String title) {

        int width = 450;
        int height = 350;

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.defaultMenuFont);

        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent we) {
                UI.actionPerformed(new ActionEvent(this, 0, "ShowOutput"));
            }
        });

        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setTitle(title);
        frameInsets = getInsets();

        setSize(frameInsets.left + frameInsets.right + width,
                frameInsets.top + frameInsets.bottom + height);



        tabbedPane.addTab("Data", null, new ScrollTextArea());

        tabbedPane.addTab("Debug", null, new ScrollTextArea());

        tabbedPane.addChangeListener(this);

        getContentPane().add(tabbedPane);

        buildMenu();
        buildToolBar();

        getContentPane().add(tBar, BorderLayout.NORTH);
    }

    /**
     *  Sets the text area to the message, erasing what was
     *  there earlier.
     *  @param message   message
     *  @param textAreaID      DATA, DEBUG, DATA
     */
    public void setMessage(String message, int textAreaID) {

        if (textAreaID < tabbedPane.getTabCount() && message != null) {
            ((ScrollTextArea)tabbedPane.getComponentAt(textAreaID)).getTextArea().setText(message);
        }
    }

    /**
     *  Appends the text area with the message
     *  @param appMessage  the message
     *  @param textAreaID        DATA, DEBUG, DATA
     */
    public void append(String appMessage, int textAreaID) {
        if (textAreaID < tabbedPane.getTabCount() && appMessage != null) {
            ((ScrollTextArea)tabbedPane.getComponentAt(textAreaID)).getTextArea().append(appMessage);
        }

    }

    /**
     * Method to append text to an attached JTextArea (not DEBUG or DATA areas)
     * @param tabTitle String The title of the attached tab
     * @param appMessage String the message to be appended
     */
    public void append(String tabTitle, String appMessage) {

        int index = tabbedPane.indexOfTab(tabTitle);

        if (index > -1) {
            try {
                ((ScrollTextArea)tabbedPane.getComponentAt(index)).getTextArea().append(appMessage);
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }

    }

    /**
     *  Clears the text area
     *  @param textAreaID
     */
    public void clear(int textAreaID) {

        if (textAreaID < tabbedPane.getTabCount()) {
            ( (ScrollTextArea) tabbedPane.getComponentAt(textAreaID)).getTextArea().removeAll();
            ( (ScrollTextArea) tabbedPane.getComponentAt(textAreaID)).getTextArea().setText("");
        }
    }

    /**
     * Saves the tab's text to a file "Tabname_currenttimems.txt"
     * @param tabName String tabName (can be data/debug/ or any custom tab added)
     */
    public void save(String tabName) {
        int index = tabbedPane.indexOfTab(tabName);

        if (index >= 0) {

            try {
                BufferedWriter br = new BufferedWriter(new FileWriter(UI.getDefaultDirectory() + File.separator +
                    tabName + "_" + System.currentTimeMillis() + ".txt"));
                ((ScrollTextArea)tabbedPane.getComponentAt(index)).getTextArea().write(br);
                br.flush();
                br.close();
            }
            catch (Exception e) {
                e.printStackTrace();
            }

        }
    }

    /**
     * Adds a tab to the MessageFrame tabbed pane with the given Title
     * @param tabTitle String the title of the new tab to add
     */
    public void addTab(String tabTitle) {
        if (tabTitle == null) {
            return;
        }

        int i = tabbedPane.indexOfTab(tabTitle);

        if (i > -1) {

            ( (ScrollTextArea) tabbedPane.getComponentAt(i)).getTextArea().append("\n\n");
            return;
        }

        ScrollTextArea st = new ScrollTextArea();

        tabbedPane.addTab(tabTitle, null, st);
        tabbedPane.setSelectedComponent(st);

        if (UI.isScriptRecording()) {

        }
    }

    /**
     * Removes the Tab associated with the given title
     * (will not allow the removal of DEBUG or DATA tabs
     * @param tabTitle String the title of the tab to be removed
     */
    public void removeTab(String tabTitle) {
        int index =  tabbedPane.indexOfTab(tabTitle);

        if (index > 1) {
            tabbedPane.removeTabAt(index);
        }
    }

    /**
     * Watches for tab index changes
     * @param event ChangeEvent the change
     */
    public void stateChanged(ChangeEvent event) {

        if (event.getSource().equals(tabbedPane)) {
            if (tabbedPane.getSelectedIndex() > 1) {
                removeCurrentTab.setEnabled(true);
                delTabButton.setEnabled(true);
                MipavUtil.setComponentsEnabled(removeCurrentTab, true);
            }
            else {
                removeCurrentTab.setEnabled(false);
                delTabButton.setEnabled(false);
                MipavUtil.setComponentsEnabled(removeCurrentTab, false);
            }
        }

    }

    /**
     *   If "Save", saves text to file; if "Clear", clears appropriate text area; if
     *   "Copy", copies text to clipboard; if "Cut", removes the text and copies it
     *   to the clipboard; and if "Select", selects all text in text area.
     *   @param event    Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {

        if (event.getActionCommand().equals("Save")) {
            String fileName = "", directory = "";

            JFileChooser chooser = new JFileChooser();
            if (UI.getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            }
            else
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty(
                    "user.dir")));
            int returnValue = chooser.showSaveDialog(this);

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = chooser.getCurrentDirectory().toString() +
                    File.separatorChar;
                UI.setDefaultDirectory(chooser.getCurrentDirectory().toString());
            }
            else
                return;

            try {
                BufferedWriter br = new BufferedWriter(new FileWriter(directory +
                    fileName));


                ((ScrollTextArea)tabbedPane.getSelectedComponent()).getTextArea().write(br);
                br.flush();
                br.close();
            }
            catch (Exception error) {
                error.printStackTrace();
                MipavUtil.displayError("Error writing file");
            }
            // if the user is recording a script, write a "SaveData"
            //     or "SaveData" (not debug)
            if (UI.isScriptRecording()) {
                UI.getScriptDialog().append("SaveTab " + tabbedPane.getTitleAt(tabbedPane.getSelectedIndex()) + "\n");
            }
        }
        else if (event.getActionCommand().equals("Clear")) {
            try {
                ((ScrollTextArea)tabbedPane.getSelectedComponent()).getTextArea().setText("");
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        else if (event.getActionCommand().equals("Copy")) {
            try {
                ((ScrollTextArea)tabbedPane.getSelectedComponent()).getTextArea().copy();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        else if (event.getActionCommand().equals("Cut")) {
            try {
                ((ScrollTextArea)tabbedPane.getSelectedComponent()).getTextArea().cut();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        else if (event.getActionCommand().equals("Select")) {
            try {
               ((ScrollTextArea)tabbedPane.getSelectedComponent()).getTextArea().selectAll();
            } catch (Exception e) {
                e.printStackTrace();
            }

        }
        else if (event.getActionCommand().equals("Remove")) {
            int index = tabbedPane.getSelectedIndex();
            if (index > 1) {
                tabbedPane.removeTabAt(index);
            }
        } else if (event.getActionCommand().equals("Add")) {
            addTab(JOptionPane.showInputDialog(this, "New tab name:", "Add Tab", JOptionPane.OK_OPTION));
        }

    }

    /**
     *   Creates the needed menus
     */
    private void buildMenu() {

        JMenu fileMenu = ViewMenuBuilder.buildMenu("File", 0, false);

        JMenu editMenu = ViewMenuBuilder.buildMenu("Edit", 0, false);

        fileMenu.add(ViewMenuBuilder.buildMenuItem("Save messages", "Save", 0, this, "save.gif", true));
        editMenu.add(ViewMenuBuilder.buildMenuItem("Clear messages", "Clear", 0, this, "clear.gif", true));
        editMenu.add(ViewMenuBuilder.buildMenuItem("Copy", "Copy", 0, this, "copypaint.gif", true));
        editMenu.add(ViewMenuBuilder.buildMenuItem("Cut", "Cut", 0, this, "cutpaint.gif", true));
        editMenu.add(ViewMenuBuilder.buildMenuItem("Select All", "Select", 0, this, "copypaint.gif", true));

        fileMenu.add(ViewMenuBuilder.buildMenuItem("Add tab", "Add", 0, this, null, true));

        removeCurrentTab = ViewMenuBuilder.buildMenuItem("Remove tab", "Remove", 0, this, null, true);
        removeCurrentTab.setEnabled(false);
        MipavUtil.setComponentsEnabled(removeCurrentTab, false);
        fileMenu.add(removeCurrentTab);

        menu = new JMenuBar();
        menu.add(fileMenu);
        menu.add(editMenu);
        setJMenuBar(menu);
    }

    /**
     *   Builds the toolbar
     */
    private void buildToolBar() {

        tBar = new JToolBar();
        tBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        tBar.setBorder(BorderFactory.createEtchedBorder());
        tBar.setBorderPainted(true);

        JButton saveButton = new JButton(MipavUtil.getIcon("save.gif"));
        saveButton.addActionListener(this);
        saveButton.setToolTipText("Save results");
        saveButton.setActionCommand("Save");
        saveButton.setBorderPainted(false);
        saveButton.setRolloverEnabled(true);
        saveButton.setRolloverIcon(MipavUtil.getIcon("saverollover.gif"));
        saveButton.setBorder(BorderFactory.createLoweredBevelBorder());
        saveButton.setFocusPainted(false);
        tBar.add(saveButton);
        //tBar.add(makeSeparator());

        JButton newButton = new JButton(MipavUtil.getIcon("clear.gif"));
        newButton.addActionListener(this);
        newButton.setToolTipText("Clears the message area");
        newButton.setActionCommand("Clear");
        newButton.setBorderPainted(false);
        newButton.setRolloverEnabled(true);
        newButton.setRolloverIcon(MipavUtil.getIcon("clearroll.gif"));
        newButton.setMargin(new Insets(0, 0, 0, 0));
        tBar.add(newButton);

        JButton copyButton = new JButton(MipavUtil.getIcon("copypaint.gif"));
        copyButton.addActionListener(this);
        copyButton.setToolTipText("Copies selected text");
        copyButton.setActionCommand("Copy");
        copyButton.setBorderPainted(false);
        copyButton.setRolloverEnabled(true);
        copyButton.setRolloverIcon(MipavUtil.getIcon("copypaintroll.gif"));
        copyButton.setMargin(new Insets(0, 0, 0, 0));
        tBar.add(copyButton);

        JButton cutButton = new JButton(MipavUtil.getIcon("cutpaint.gif"));
        cutButton.addActionListener(this);
        cutButton.setToolTipText("Cuts selected text");
        cutButton.setActionCommand("Cut");
        cutButton.setBorderPainted(false);
        cutButton.setRolloverEnabled(true);
        cutButton.setRolloverIcon(MipavUtil.getIcon("cutpaintroll.gif"));
        cutButton.setMargin(new Insets(0, 0, 0, 0));
        tBar.add(cutButton);


        tBar.add(ViewToolBarBuilder.makeSeparator());
        delTabButton = new JButton(MipavUtil.getIcon("deletetab.gif"));
        delTabButton.addActionListener(this);
        delTabButton.setToolTipText("Remove selected tab");
        delTabButton.setActionCommand("Remove");
        delTabButton.setBorderPainted(false);
        delTabButton.setRolloverEnabled(true);
        delTabButton.setRolloverIcon(MipavUtil.getIcon("deletetabroll.gif"));
        delTabButton.setMargin(new Insets(0, 0, 0, 0));
        delTabButton.setEnabled(false);
        tBar.add(delTabButton);


        tBar.setFloatable(false);
    }

    private class ScrollTextArea
        extends JScrollPane {

        private JTextArea tArea = null;

        public ScrollTextArea() {
            super(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
            tArea = new JTextArea();
            tArea.setBackground(Color.lightGray);
            tArea.setEditable(true);
            tArea.setFont(MipavUtil.font12);
            tArea.setMargin(new Insets(3, 3, 3, 3));
            setViewportView(tArea);
        }

        public JTextArea getTextArea() {
            return tArea;
        }
    }

}
