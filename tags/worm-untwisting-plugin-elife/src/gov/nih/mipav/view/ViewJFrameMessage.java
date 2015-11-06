package gov.nih.mipav.view;


import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.actions.ActionSaveTab;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


/**
 * This class produces a message frame where user data, logging and debug information can be displayed. The frame can be
 * resize and a scroll pane is used where scroll bars are displayed as needed. This frame also gives the user the
 * ability to edit and save the data as needed to a text file. Each image (ModelImage) keeps a data and a logging
 * (JTextAreas) objects to record information specific to itself. Only one global data object and and one debug text
 * object exists for the whole MIPAV application.
 * 
 * @version 1.0 Oct 24, 1998
 * @author Matthew J. McAuliffe, Ph.D.
 */
public class ViewJFrameMessage extends JFrame implements ActionListener, ChangeListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1198653323631787447L;

    /**
     * Used to indicate which of the 2 JTextAreas the data (message) is to be displayed.
     * 
     * @see #setMessage(String, int)
     */
    public static final int DATA = 0;

    /**
     * Used to indicate which of the 2 JTextAreas the data (message) is to be displayed.
     * 
     * @see #setMessage(String, int)
     */
    public static final int DEBUG = 1;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton delTabButton = null;

    /** DOCUMENT ME! */
    private Insets frameInsets;

    /** Indicates last state of frame - NORMAL or ICONIFIED. */
    private int lastState = Frame.NORMAL;

    /** DOCUMENT ME! */
    private JMenuBar menu;

    /** DOCUMENT ME! */
    private JMenuItem removeCurrentTab = null;

    /** DOCUMENT ME! */
    private JTabbedPane tabbedPane;

    /** DOCUMENT ME! */
    private JToolBar tBar;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates new frame.
     * 
     * @param title Title of dialog frame
     */
    public ViewJFrameMessage(final String title) {
        super(title);

        setResizable(true);
        init(title);
        append("MIPAV Version: " + MipavUtil.getVersion() + "\n", DEBUG);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * If "Save", saves text to file; if "Clear", clears appropriate text area; if "Copy", copies text to clipboard; if
     * "Cut", removes the text and copies it to the clipboard; and if "Select", selects all text in text area.
     * 
     * @param event Event that triggers this function
     */
    @Override
    public void actionPerformed(final ActionEvent event) {

        if (event.getActionCommand().equals("Print")) {
            try {
                final String jobtitle = "";
                PrintJob pjob = getToolkit().getPrintJob(this, jobtitle, null);
                if (pjob != null) {

                    final String textString = ((ScrollTextArea) tabbedPane.getSelectedComponent()).getTextArea().getText();
                    int tabCount = 0;
                    int lineCount = 0;
                    int currentTabOnLine = 0;
                    int maxTabOnLine = 0;
                    int i;
                    int j;
                    boolean lastCharTab = false;
                    for (i = 0; i < textString.length(); i++) {
                        if ( (textString.charAt(i) == '\t') && ( !lastCharTab)) {
                            lastCharTab = true;
                            tabCount++;
                            currentTabOnLine++;
                            if (currentTabOnLine > maxTabOnLine) {
                                maxTabOnLine = currentTabOnLine;
                            }
                        } else if ( (textString.charAt(i) == '\t') && (lastCharTab)) {

                        } else if (textString.charAt(i) == '\n') {
                            lastCharTab = false;
                            lineCount++;
                            currentTabOnLine = 0;
                        } else {
                            lastCharTab = false;
                        }
                    }
                    final int maxCharsBeforeTab[] = new int[maxTabOnLine];
                    int charNum = 0;
                    int tabNum = 0;
                    lastCharTab = false;
                    for (i = 0; i < textString.length(); i++) {
                        if ( (textString.charAt(i) == '\t') && ( !lastCharTab)) {
                            lastCharTab = true;
                            if (charNum > maxCharsBeforeTab[tabNum]) {
                                maxCharsBeforeTab[tabNum++] = charNum;
                            }
                            charNum = 0;
                        } else if ( (textString.charAt(i) == '\t') && (lastCharTab)) {

                        } else if (textString.charAt(i) == '\n') {
                            lastCharTab = false;
                            charNum = 0;
                            tabNum = 0;
                        } else {
                            lastCharTab = false;
                            charNum++;
                        }
                    }
                    final String paddedString[] = new String[lineCount];
                    int startPos = 0;
                    int currentPos = 0;
                    charNum = 0;
                    tabNum = 0;
                    int spacesNeeded;
                    int lineNum = 0;
                    lastCharTab = false;
                    for (i = 0; i < textString.length(); i++) {
                        if ( (textString.charAt(i) == '\t') && ( !lastCharTab)) {
                            lastCharTab = true;
                            spacesNeeded = maxCharsBeforeTab[tabNum++] - charNum + 2;
                            charNum = 0;
                            if (paddedString[lineNum] == null) {
                                paddedString[lineNum] = textString.substring(startPos, currentPos);
                            } else {
                                paddedString[lineNum] = paddedString[lineNum].concat(textString.substring(startPos, currentPos));
                            }
                            startPos = currentPos + 1;
                            for (j = 0; j < spacesNeeded; j++) {
                                paddedString[lineNum] = paddedString[lineNum].concat(" ");
                            }
                        } else if ( (textString.charAt(i) == '\t') && (lastCharTab)) {

                        } else if (textString.charAt(i) == '\n') {
                            lastCharTab = false;
                            charNum = 0;
                            if (paddedString[lineNum] == null) {
                                paddedString[lineNum] = textString.substring(startPos, currentPos + 1);
                            } else {
                                paddedString[lineNum] = paddedString[lineNum].concat(textString.substring(startPos, currentPos + 1));
                            }
                            tabNum = 0;
                            lineNum++;
                            startPos = currentPos + 1;
                        } else {
                            lastCharTab = false;
                            charNum++;
                        }
                        currentPos++;
                    }

                    Graphics g = null;
                    // Dimension pDim = pjob.getPageDimension();
                    // int pRes = pjob.getPageResolution();
                    // System.out.println("Page size " + pDim + "; Res " + pRes);
                    g = pjob.getGraphics();
                    g.setColor(Color.black);
                    g.setFont(new Font("Courier", Font.PLAIN, 12));
                    final int x = 20;
                    int y = 100;
                    for (i = 0; i < lineCount; i++) {
                        g.drawString(paddedString[i], x, y);
                        y += 18;
                    }

                    g.dispose(); // flush page
                    pjob.end(); // total end of print job.
                    pjob = null; // avoid redundant calls to pjob.end()
                }
            } catch (final Exception error) {
                error.printStackTrace();
                MipavUtil.displayError("Error writing file");
            }
        } else if (event.getActionCommand().equals("Save")) {
            String fileName = "", directory = "";

            final JFileChooser chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            final int returnValue = chooser.showSaveDialog(this);

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = chooser.getCurrentDirectory().toString() + File.separatorChar;
                ViewUserInterface.getReference().setDefaultDirectory(chooser.getCurrentDirectory().toString());
            } else {
                return;
            }

            try {
                final BufferedWriter br = new BufferedWriter(new FileWriter(directory + fileName));

                ((ScrollTextArea) tabbedPane.getSelectedComponent()).getTextArea().write(br);
                br.flush();
                br.close();
            } catch (final Exception error) {
                error.printStackTrace();
                MipavUtil.displayError("Error writing file");
            }

            ScriptRecorder.getReference().addLine(new ActionSaveTab(tabbedPane.getTitleAt(tabbedPane.getSelectedIndex())));
        } else if (event.getActionCommand().equals("Clear")) {

            try {
                ((ScrollTextArea) tabbedPane.getSelectedComponent()).getTextArea().setText("");
            } catch (final Exception e) {
                e.printStackTrace();
            }

            if (getTabbedPane().getSelectedIndex() == 1) {
                append("MIPAV Version: " + MipavUtil.getVersion() + "\n", DEBUG);
            }

        } else if (event.getActionCommand().equals("Copy")) {

            try {
                ((ScrollTextArea) tabbedPane.getSelectedComponent()).getTextArea().copy();
            } catch (final Exception e) {
                e.printStackTrace();
            }
        } else if (event.getActionCommand().equals("Cut")) {

            try {
                ((ScrollTextArea) tabbedPane.getSelectedComponent()).getTextArea().cut();
            } catch (final Exception e) {
                e.printStackTrace();
            }
        } else if (event.getActionCommand().equals("Select")) {

            try {
                ((ScrollTextArea) tabbedPane.getSelectedComponent()).getTextArea().selectAll();
            } catch (final Exception e) {
                e.printStackTrace();
            }

        } else if (event.getActionCommand().equals("Remove")) {
            final int index = tabbedPane.getSelectedIndex();

            if (index > 1) {
                tabbedPane.removeTabAt(index);
            }
        } else if (event.getActionCommand().equals("Add")) {
            addTab(JOptionPane.showInputDialog(this, "New tab name:", "Add Tab", JOptionPane.OK_OPTION));
        }

    }

    /**
     * Adds a tab to the MessageFrame tabbed pane with the given Title.
     * 
     * @param tabTitle String the title of the new tab to add
     */
    public void addTab(final String tabTitle) {

        if (tabTitle == null) {
            return;
        }

        final int i = tabbedPane.indexOfTab(tabTitle);

        if (i > -1) {

            ((ScrollTextArea) tabbedPane.getComponentAt(i)).getTextArea().append("\n\n");

            return;
        }

        final ScrollTextArea st = new ScrollTextArea();

        tabbedPane.addTab(tabTitle, null, st);
        tabbedPane.setSelectedComponent(st);
    }

    /**
     * Appends the text area with the message.
     * 
     * @param appMessage the message
     * @param textAreaID DATA, DEBUG, DATA
     */
    public void append(final String appMessage, final int textAreaID) {

        if ( (textAreaID < tabbedPane.getTabCount()) && (appMessage != null)) {
            ((ScrollTextArea) tabbedPane.getComponentAt(textAreaID)).getTextArea().append(appMessage);
        }

    }

    /**
     * Method to append text to an attached JTextArea (not DEBUG or DATA areas).
     * 
     * @param tabTitle String The title of the attached tab
     * @param appMessage String the message to be appended
     */
    public void append(final String tabTitle, final String appMessage) {

        final int index = tabbedPane.indexOfTab(tabTitle);

        if (index > -1) {

            try {
                ((ScrollTextArea) tabbedPane.getComponentAt(index)).getTextArea().append(appMessage);
            } catch (final Exception e) {
                e.printStackTrace();
            }
        }

    }

    /**
     * Method to append text to an attached JTextArea (not DEBUG or DATA areas).
     * 
     * @param tabTitle String The title of the attached tab
     * @param font font of the appended message
     */
    public void setFont(final String tabTitle, final Font font) {

        final int index = tabbedPane.indexOfTab(tabTitle);

        if (index > -1) {

            try {
                ((ScrollTextArea) tabbedPane.getComponentAt(index)).getTextArea().setFont(font);
            } catch (final Exception e) {
                e.printStackTrace();
            }
        }

    }

    /**
     * Clears the text area.
     * 
     * @param textAreaID DOCUMENT ME!
     */
    public void clear(final int textAreaID) {

        if (textAreaID < tabbedPane.getTabCount()) {
            ((ScrollTextArea) tabbedPane.getComponentAt(textAreaID)).getTextArea().removeAll();
            ((ScrollTextArea) tabbedPane.getComponentAt(textAreaID)).getTextArea().setText("");
        }
    }

    /**
     * Gets text area which data tab prints to.
     * 
     * @return JTextArea
     */
    public JTextArea getData() {
        return ((ScrollTextArea) tabbedPane.getComponentAt(DATA)).getTextArea();
    }

    /**
     * Gets text area which debug tab prints to.
     * 
     * @return JTextArea
     */
    public JTextArea getDebug() {
        return ((ScrollTextArea) tabbedPane.getComponentAt(DEBUG)).getTextArea();
    }

    /**
     * Gets the display state of the Frame ( either Frame.NORMAL or Frame.ICONIFIED ).
     * 
     * @return state Should be either Frame.NORMAL or Frame.ICONIFIED
     */
    public int getLastState() {
        return lastState;
    }

    public JTabbedPane getTabbedPane() {
        return this.tabbedPane;
    }

    /**
     * Removes the Tab associated with the given title (will not allow the removal of DEBUG or DATA tabs.
     * 
     * @param tabTitle String the title of the tab to be removed
     */
    public void removeTab(final String tabTitle) {
        final int index = tabbedPane.indexOfTab(tabTitle);

        if (index > 1) {
            tabbedPane.removeTabAt(index);
        }
    }

    /**
     * Saves the tab's text to a file "Tabname_currenttimems.txt"
     * 
     * @param tabName String tabName (can be data/debug/ or any custom tab added)
     */
    public void save(final String tabName) {
        save(ViewUserInterface.getReference().getDefaultDirectory(), tabName);
    }

    /**
     * Saves the tab's text to a file "Tabname_currenttimems.txt"
     * 
     * @param tabName String tabName (can be data/debug/ or any custom tab added)
     */
    public void save(final String directory, final String tabName) {
        final int index = tabbedPane.indexOfTab(tabName);

        if (index >= 0) {

            try {
                final BufferedWriter br = new BufferedWriter(new FileWriter(directory + File.separator + tabName + "_" + System.currentTimeMillis() + ".txt"));
                ((ScrollTextArea) tabbedPane.getComponentAt(index)).getTextArea().write(br);
                br.flush();
                br.close();
            } catch (final Exception e) {
                e.printStackTrace();
            }

        }
    }

    /**
     * Sets the display state of the Frame to be either Frame.NORMAL or Frame.ICONIFIED.
     * 
     * @param state Should be either Frame.NORMAL or Frame.ICONIFIED
     */
    public void setLastState(final int state) {
        lastState = state;
    }

    /**
     * Sets the text area to the message, erasing what was there earlier.
     * 
     * @param message message
     * @param textAreaID DATA, DEBUG, DATA
     */
    public void setMessage(final String message, final int textAreaID) {

        if ( (textAreaID < tabbedPane.getTabCount()) && (message != null)) {
            ((ScrollTextArea) tabbedPane.getComponentAt(textAreaID)).getTextArea().setText(message);
        }
    }

    /**
     * Watches for tab index changes.
     * 
     * @param event ChangeEvent the change
     */
    @Override
    public void stateChanged(final ChangeEvent event) {

        if (event.getSource().equals(tabbedPane)) {

            if (tabbedPane.getSelectedIndex() > 1) {
                removeCurrentTab.setEnabled(true);
                delTabButton.setEnabled(true);
                MipavUtil.setComponentsEnabled(removeCurrentTab, true);
            } else {
                removeCurrentTab.setEnabled(false);
                delTabButton.setEnabled(false);
                MipavUtil.setComponentsEnabled(removeCurrentTab, false);
            }
        }

    }

    /**
     * Creates the needed menus.
     */
    private void buildMenu() {

        final JMenu fileMenu = ViewMenuBuilder.buildMenu("File", 0, false);

        final JMenu editMenu = ViewMenuBuilder.buildMenu("Edit", 0, false);

        fileMenu.add(ViewMenuBuilder.buildMenuItem("Print", "Print", 0, this, "printer.gif", true));
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
     * Builds the toolbar.
     */
    private void buildToolBar() {

        tBar = new JToolBar();
        tBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        tBar.setBorder(BorderFactory.createEtchedBorder());
        tBar.setBorderPainted(true);

        final JButton saveButton = new JButton(MipavUtil.getIcon("save.gif"));
        saveButton.addActionListener(this);
        saveButton.setToolTipText("Save results");
        saveButton.setActionCommand("Save");
        saveButton.setBorderPainted(false);
        saveButton.setRolloverEnabled(true);
        saveButton.setRolloverIcon(MipavUtil.getIcon("saverollover.gif"));
        saveButton.setBorder(BorderFactory.createLoweredBevelBorder());
        saveButton.setFocusPainted(false);
        tBar.add(saveButton);
        // tBar.add(makeSeparator());

        final JButton newButton = new JButton(MipavUtil.getIcon("clear.gif"));
        newButton.addActionListener(this);
        newButton.setToolTipText("Clears the message area");
        newButton.setActionCommand("Clear");
        newButton.setBorderPainted(false);
        newButton.setRolloverEnabled(true);
        newButton.setRolloverIcon(MipavUtil.getIcon("clearroll.gif"));
        newButton.setMargin(new Insets(0, 0, 0, 0));
        tBar.add(newButton);

        final JButton copyButton = new JButton(MipavUtil.getIcon("copypaint.gif"));
        copyButton.addActionListener(this);
        copyButton.setToolTipText("Copies selected text");
        copyButton.setActionCommand("Copy");
        copyButton.setBorderPainted(false);
        copyButton.setRolloverEnabled(true);
        copyButton.setRolloverIcon(MipavUtil.getIcon("copypaintroll.gif"));
        copyButton.setMargin(new Insets(0, 0, 0, 0));
        tBar.add(copyButton);

        final JButton cutButton = new JButton(MipavUtil.getIcon("cutpaint.gif"));
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

    /**
     * Initializes the dialog box to a certain size and adds the components.
     * 
     * @param title Title of the dialog box
     */
    private void init(final String title) {

        final int width = 450;
        final int height = 350;

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.defaultMenuFont);

        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(final WindowEvent we) {
                ViewUserInterface.getReference().actionPerformed(new ActionEvent(this, 0, "ShowOutput"));
            }
        });

        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setTitle(title);
        frameInsets = getInsets();

        setSize(frameInsets.left + frameInsets.right + width, frameInsets.top + frameInsets.bottom + height);

        tabbedPane.addTab("Data", null, new ScrollTextArea());

        tabbedPane.addTab("Debug", null, new ScrollTextArea());

        tabbedPane.addChangeListener(this);

        getContentPane().add(tabbedPane);

        buildMenu();
        buildToolBar();
        getContentPane().add(tBar, BorderLayout.NORTH);

    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public static class ScrollTextArea extends JScrollPane {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 3869765356771292936L;

        /** DOCUMENT ME! */
        private JTextArea tArea = null;

        /**
         * Creates a new ScrollTextArea object.
         */
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

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public JTextArea getTextArea() {
            return tArea;
        }
    }

}
