package gov.nih.mipav.view;


import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;


/**
 * This class produces a message frame where user data and program can be displayed. The frame can be resize and a
 * scroll pane is used where scroll bars are displayed as needed. This frame also gives the user the ability to edit and
 * save the data as needed to a text file.
 *
 * @version  0.1 Oct 24, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class ViewJFrameMessageGraph extends JFrame implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3229727417859189823L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JTextArea debugArea;

    /** DOCUMENT ME! */
    private JScrollPane debugSPane;

    /** DOCUMENT ME! */
    private Font font12, font12B;

    /** DOCUMENT ME! */
    private Insets frameInsets;

    /** DOCUMENT ME! */
    private JMenuBar menu;

    /** DOCUMENT ME! */
    private JToolBar tBar;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  title  title of dialog frame
     */
    public ViewJFrameMessageGraph(String title) {
        super(title);

        font12 = MipavUtil.font12;
        font12B = MipavUtil.font12B;

        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        init(title);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed.
     *
     * @param  event  Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {

        if (event.getActionCommand().equals("Save")) {
            String fileName = "", directory = "";

            JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));

            int returnValue = chooser.showSaveDialog(this);

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = chooser.getCurrentDirectory().toString() + File.separatorChar;
                Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            } else {
                return;
            }

            try {
                BufferedWriter br = new BufferedWriter(new FileWriter(directory + fileName));

                debugArea.write(br);
                br.flush();
                br.close();
            } catch (IOException error) {
                MipavUtil.displayError("Error writing file");
            }
        } else if (event.getActionCommand().equals("Clear")) {
            debugArea.setText("");
        } else if (event.getActionCommand().equals("Copy")) {
            debugArea.copy();
        } else if (event.getActionCommand().equals("Cut")) {
            debugArea.cut();
        } else if (event.getActionCommand().equals("Select")) {
            debugArea.selectAll();
        }
    }

    /**
     * Appends the text area with the message.
     *
     * @param  appMessage  The message to append.
     */
    public void append(String appMessage) {
        debugArea.append(appMessage);
    }

    /**
     * Clears the text area.
     *
     * @param  mode  DATA, LOG, DEBUG
     */
    public void clear(int mode) {
        debugArea.setText("");
    }

    /**
     * Sets the text area to the message, erasing what was there earlier.
     *
     * @param  message  message
     */
    public void setMessage(String message) {
        debugArea.setText(message);
    }

    /**
     * Creates the needed menus.
     */
    private void buildMenu() {

        JMenu fileMenu = new JMenu("File");
        fileMenu.setFont(font12B);

        JMenu editMenu = new JMenu("Edit");
        editMenu.setFont(font12B);
        // JMenu helpMenu        = new JMenu("Help");

        JMenuItem itemSave = new JMenuItem("Save messages)", MipavUtil.getIcon("save.gif"));
        itemSave.addActionListener(this);
        itemSave.setActionCommand("Save");
        itemSave.setFont(font12B);
        fileMenu.add(itemSave);

        JMenuItem itemClear = new JMenuItem("Clear messages", MipavUtil.getIcon("clear.gif"));
        itemClear.addActionListener(this);
        itemClear.setActionCommand("Clear");
        itemClear.setFont(font12B);
        editMenu.add(itemClear);

        JMenuItem itemCopy = new JMenuItem("Copy", MipavUtil.getIcon("copypaint.gif"));
        itemCopy.addActionListener(this);
        itemCopy.setActionCommand("Copy");
        itemCopy.setFont(font12B);
        editMenu.add(itemCopy);

        JMenuItem itemCut = new JMenuItem("Cut", MipavUtil.getIcon("cutpaint.gif"));
        itemCut.addActionListener(this);
        itemCut.setActionCommand("Cut");
        itemCut.setFont(font12B);
        editMenu.add(itemCut);

        JMenuItem itemSelectAll = new JMenuItem("Select All");
        itemSelectAll.addActionListener(this);
        itemSelectAll.setActionCommand("Select");
        itemSelectAll.setFont(font12B);
        editMenu.add(itemSelectAll);

        menu = new JMenuBar();
        menu.add(fileMenu);
        menu.add(editMenu);
        setJMenuBar(menu);
        // menu.add(helpMenu);
        // menuBar.setHelpMenu(helpMenu);
    }

    /**
     * Builds the needed toolbar.
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

        // saveButton.addItemListener(this);
        saveButton.setFocusPainted(false);
        tBar.add(saveButton);
        // tBar.add(makeSeparator());

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

        tBar.setFloatable(false);
    }

    /**
     * Initializes the dialog box to a certain size and adds the components.
     *
     * @param  title  title of the dialog box
     */
    private void init(String title) {
        int width = 450;
        int height = 350;

        setTitle(title);
        frameInsets = getInsets();
        setSize(frameInsets.left + frameInsets.right + width, frameInsets.top + frameInsets.bottom + height);

        debugArea = new JTextArea();
        debugArea.setBackground(Color.lightGray);
        debugArea.setEditable(true);
        debugArea.setFont(font12);
        debugArea.setMargin(new Insets(3, 3, 3, 3));
        debugSPane = new JScrollPane(debugArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        getContentPane().add(debugSPane);

        buildMenu();
        buildToolBar();

        getContentPane().add(tBar, BorderLayout.NORTH);
    }

}
