package gov.nih.mipav.view;


import gov.nih.mipav.view.CustomUIBuilder.UIParams;
import gov.nih.mipav.view.Preferences.OperatingSystem;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

import javax.swing.*;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;


/**
 * This class provides a number of helper methods for building and manipulating menus.
 */
public class ViewMenuBuilder {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Link to the file menu so that quicklist can be rebuilt and added. */
    private JMenu fileMenu;

    /** The class which wants to listen to the menu items generated. */
    private ActionListener listener;

    /** Holder of all the menu items. */
    private Vector<MipavMenuItem> menuItemVector;

    /** List that holds the last X number of recently opened images. */
    private QuickList quickList;

    /** Index for rebuilding quicklist. */
    private int quicklistIndex = 0;
    
    /** Menu dragging listener */
    private final MenuDragOp op;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Sets frame to add menus to, and initializes menu item vector.
     * 
     * @param al class which wants notification of interations with the generated menu items
     */
    public ViewMenuBuilder(ActionListener al) {
        listener = al;
        menuItemVector = new Vector<MipavMenuItem>();
        op = new MenuDragOp();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Static method for building JCheckBoxes with the given parameters.
     * 
     * @param text String text to display on the box
     * @param cmd String the action command
     * @param listener ActionListener the action listener for the box
     * @param selected boolean whether the box should be selected by default
     * 
     * @return JCheckBoxMenuItem the jcheckbox that is built
     */
    public static JCheckBoxMenuItem buildCheckBoxMenuItem(String text, String cmd, ActionListener listener,
            boolean selected) {

        JCheckBoxMenuItem menuEntry = new JCheckBoxMenuItem();

        menuEntry.addActionListener(listener);

        menuEntry.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        JLabel menuName = new JLabel();
        JLabel menuAccel = new JLabel();

        menuName.setText(text);
        menuName.setFont(MipavUtil.defaultMenuFont);

        int paddingX = 5;

        paddingX += MipavUtil.DEFAULT_ICON_WIDTH;

        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;

        gbc.insets = new Insets(0, paddingX, 5, 0);

        menuEntry.add(menuName, gbc);
        menuEntry.setName(text);

        if ( (cmd != null) && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);

            KeyStroke ks = Preferences.getShortcut(cmd);

            if (ks != null) {
                String sc = ks.toString();
                sc = sc.replaceAll("pressed", "").replaceAll("ctrl", "Ctrl").replaceAll("shift", "Shift").replaceAll(
                        "alt", "Alt").trim();

                menuAccel.setText(sc);
                menuAccel.setFont(MipavUtil.defaultAcceleratorFont);
                menuAccel.setForeground(Color.GRAY);
                menuAccel.setHorizontalAlignment(SwingConstants.TRAILING);
                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                gbc.insets = new Insets(0, 0, 0, 5);

                menuEntry.add(menuAccel, gbc);
            }
        }

        Dimension d = menuName.getPreferredSize();
        Dimension d2 = menuAccel.getPreferredSize();

        d.width += d2.width + paddingX;
        d.height = (d.height > d2.height) ? d.height : d2.height;

        d.height += 10;

        d.width += 20;

        menuEntry.setPreferredSize(d);

        menuEntry.setSelected(selected);

        return menuEntry;

    }

    /**
     * DOCUMENT ME!
     * 
     * @param text DOCUMENT ME!
     * @param cmd DOCUMENT ME!
     * @param mnemonic DOCUMENT ME!
     * @param listener DOCUMENT ME!
     * @param iconName DOCUMENT ME!
     * @param iconPadding DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static JMenuItem buildHTMLMenuItem(String text, String cmd, int mnemonic, ActionListener listener,
            String iconName, boolean iconPadding) {
        JMenuItem menuEntry = new JMenuItem();
        menuEntry.addActionListener(listener);

        String mnemonicStr = null;

        // set Icon
        ImageIcon icon = null;

        if (iconName != null) {
            icon = MipavUtil.getIcon(iconName);
        }

        try {

            if (icon != null) {

                menuEntry.setIcon(icon);
            }
        } catch (Exception e) {}

        // get the size of the font (HTML SIZES)
        int size = 3;

        try {
            size = Integer.parseInt(Preferences.getProperty(Preferences.PREF_MENU_FONT_SIZE));
        } catch (Exception e) {}

        // get the name of the font to be used
        String fontName = "Serif";

        if (Preferences.getProperty(Preferences.PREF_MENU_FONT) != null) {
            fontName = Preferences.getProperty(Preferences.PREF_MENU_FONT);
        }

        if (mnemonic != 0) {
            menuEntry.setMnemonic(mnemonic);

            int index = text.indexOf(mnemonic);

            // if the mnemonic is present as a letter, make it underlined
            if (index != -1) {

                if (index == 0) {
                    mnemonicStr = MipavUtil.makeHTMLFontString(Color.black, fontName, size, Font.PLAIN, true, text
                            .substring(0, 1));
                    mnemonicStr += MipavUtil.makeHTMLFontString(Color.black, fontName, size, Font.PLAIN, false, text
                            .substring(1, text.length()));

                } else {
                    mnemonicStr = MipavUtil.makeHTMLFontString(Color.black, fontName, size, Font.PLAIN, false, text
                            .substring(0, index));
                    mnemonicStr += MipavUtil.makeHTMLFontString(Color.black, fontName, size, Font.PLAIN, true, text
                            .substring(index, index + 1));

                    if (index != (text.length() - 1)) {
                        mnemonicStr += MipavUtil.makeHTMLFontString(Color.black, fontName, size, Font.PLAIN, false,
                                text.substring(index + 1));
                    }

                }
            } else {
                mnemonicStr = MipavUtil.makeHTMLFontString(Color.black, fontName, size, Font.PLAIN, false, text);
            }
        }

        menuEntry.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        JLabel menuName = new JLabel();
        JLabel menuAccel = new JLabel();

        menuName.setText("<html>" + MipavUtil.makeHTMLFontString(Color.black, fontName, size, Font.PLAIN, false, text)
                + "</html>");

        int paddingX = 5;

        if (iconPadding) {
            paddingX += MipavUtil.DEFAULT_ICON_WIDTH;
        }

        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;

        gbc.insets = new Insets(0, paddingX, 5, 0);

        menuEntry.add(menuName, gbc);

        if ( (cmd != null) && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);

            KeyStroke ks = null;

            ks = Preferences.getShortcut(cmd);

            if (ks != null) {
                String sc = ks.toString();
                sc = sc.replaceAll("pressed", "").replaceAll("ctrl", "Ctrl").replaceAll("shift", "Shift").replaceAll(
                        "alt", "Alt").trim();

                menuAccel.setText("<html>"
                        + MipavUtil.makeHTMLFontString(Color.LIGHT_GRAY, fontName, size, Font.PLAIN, false, sc)
                        + "</html>");

                menuAccel.setHorizontalAlignment(SwingConstants.TRAILING);
                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                gbc.insets = new Insets(0, 0, 0, 5);

                menuEntry.add(menuAccel, gbc);
            }
        }

        Dimension d = menuName.getPreferredSize();
        Dimension d2 = menuAccel.getPreferredSize();

        d.width += d2.width + paddingX;
        d.height = (d.height > d2.height) ? d.height : d2.height;

        d.height += 10;

        d.width += 20;

        if (icon != null) {
            d.height = (d.height > icon.getIconHeight()) ? d.height : icon.getIconHeight();
        }

        menuEntry.setPreferredSize(d);

        return menuEntry;
    }

    /**
     * Builds a menu item that can be added to a menu.
     * 
     * @param text Title of the menu item, must be non null.
     * @param mnemonic Action command of the menu item, can be null.
     * @param iconPadding Font of the menu item, must be non null.
     * 
     * @return The menu item or null if an error has occurred. public static JMenuItem buildMenuItem(String menuTitle,
     *         String actionCommand, Font font, ActionListener listener, KeyStroke keyStroke, String iconName) { return
     *         buildMenuItem(menuTitle, actionCommand, 0, listener, iconName, true); }
     */

    /**
     * Static method for building a JMenu.
     * 
     * @param text String the name of the menu to be displayed
     * @param mnemonic int mnemonic for the menu
     * @param iconPadding boolean an empty icon (transparent) is used to properly pad the text
     * 
     * @return JMenu the menu that is built
     */
    public static JMenu buildMenu(String text, int mnemonic, boolean iconPadding) {
        JMenu menu = new JMenu();

        if (mnemonic != 0) {
            menu.setMnemonic(mnemonic);
        }

        menu.setText(text);
        menu.setName(text);

        if (iconPadding) {

            try {
                menu.setIcon(MipavUtil.getIcon("empty.gif"));

                try {
                    float ver = Float.parseFloat(System.getProperty("java.version").substring(0, 3));

                    if (ver < 1.6 && !OperatingSystem.getOS().equals(OperatingSystem.OS_MAC)) {
                        menu.setIconTextGap(50);
                    }
                } catch (Exception e) {}

            } catch (Exception e) {}
        }

        menu.setFont(MipavUtil.defaultMenuFont);

        Dimension d = menu.getPreferredSize();
        d.setSize(d.width, MipavUtil.MENU_Y_PADDING);

        menu.setPreferredSize(d);

        return menu;
    }

    /**
     * Creates a Menu Item using pre-packaged UIParams
     * 
     * @param params UI Params
     * @param listener Action listener
     * @param iconPadding whether to use icon padding
     * @return the jmenuitem
     */
    public static JMenuItem buildMenuItem(UIParams params, ActionListener listener, boolean iconPadding) {
        if ( iconPadding )
        {
            return buildMenuItem(params.getText(), params.getActionCommand(), params.getMnemonic(), listener, params
                    .getIconBase() + ".gif", iconPadding);
        }
        return buildMenuItem(params.getText(), params.getActionCommand(), params.getMnemonic(), listener, params
                .getIconBase(), iconPadding);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param text DOCUMENT ME!
     * @param cmd DOCUMENT ME!
     * @param mnemonic DOCUMENT ME!
     * @param listener DOCUMENT ME!
     * @param iconName DOCUMENT ME!
     * @param iconPadding DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static JMenuItem buildMenuItem(String text, String cmd, int mnemonic, ActionListener listener,
            String iconName, boolean iconPadding) {
        JMenuItem menuEntry = new JMenuItem();
        menuEntry.addActionListener(listener);

        // set Icon
        ImageIcon icon = null;

        if (iconName != null) {
            icon = MipavUtil.getIcon(iconName);
        }

        try {

            if (icon != null) {

                menuEntry.setIcon(icon);
            }
        } catch (Exception e) {}

        if (mnemonic != 0) {
            menuEntry.setMnemonic(mnemonic);
        }

        menuEntry.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        JLabel menuName = new JLabel();
        JLabel menuAccel = new JLabel();

        menuName.setText(text);
        menuName.setFont(MipavUtil.defaultMenuFont);

        /*int paddingX = 5;

        if (iconPadding) {
            paddingX += MipavUtil.DEFAULT_ICON_WIDTH;
        }*/
        
        int paddingX = 8;

        if (iconPadding) {
            paddingX = 5 + MipavUtil.DEFAULT_ICON_WIDTH;
        }

        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;

        //gbc.insets = new Insets(0, paddingX, 5, 0);
        gbc.insets = new Insets(0, paddingX, 0, 0);

        menuEntry.add(menuName, gbc);

        if ( (cmd != null) && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);

            try {
                KeyStroke ks = Preferences.getShortcut(cmd);

                if (ks != null) {
                    String sc = ks.toString();
                    sc = sc.replaceAll("pressed", "").replaceAll("ctrl", "Ctrl").replaceAll("shift", "Shift")
                            .replaceAll("alt", "Alt").trim();

                    menuAccel.setText(sc);
                    menuAccel.setFont(MipavUtil.defaultAcceleratorFont);
                    menuAccel.setForeground(Color.GRAY);

                    menuAccel.setHorizontalAlignment(SwingConstants.TRAILING);
                    gbc.gridx = 1;
                    gbc.anchor = GridBagConstraints.EAST;
                    gbc.gridwidth = GridBagConstraints.REMAINDER;
                    gbc.insets = new Insets(0, 0, 0, 5);

                    menuEntry.add(menuAccel, gbc);
                }
            } catch (Exception e) {
                System.err.println("text is: " + text);
                System.err.println("Command is: " + cmd);
                e.printStackTrace();
            }
        }

        Dimension d = menuName.getPreferredSize();
        Dimension d2 = menuAccel.getPreferredSize();

        d.width += d2.width + paddingX;
        d.height = (d.height > d2.height) ? d.height : d2.height;

        d.height += 10;

        d.width += 20;

        if (icon != null) {
            d.height = (d.height > icon.getIconHeight()) ? d.height : icon.getIconHeight();
        }

        menuEntry.setPreferredSize(d);
        // System.err.println(d);

        return menuEntry;
    }

    /**
     * Builds a JCheckBox with the given parameters and adds it to the Vector of menu items.
     * 
     * @param text String Checkbox text
     * @param cmd String actioncommand
     * @param selected boolean whether the box is selected by default
     * 
     * @return JCheckBoxMenuItem
     */
    public JCheckBoxMenuItem buildCheckBoxMenuItem(String text, String cmd, boolean selected) {

        JCheckBoxMenuItem menuEntry = new JCheckBoxMenuItem();

        if (listener != null) {
            menuEntry.addActionListener(listener);
        }

        menuEntry.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        JLabel menuName = new JLabel();
        JLabel menuAccel = new JLabel();

        menuName.setText(text);
        menuName.setFont(MipavUtil.defaultMenuFont);

        int paddingX = 5;

        paddingX += MipavUtil.DEFAULT_ICON_WIDTH;

        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;

        gbc.insets = new Insets(0, paddingX, 5, 0);

        menuEntry.add(menuName, gbc);

        if (cmd == null) {
            cmd = new String(text);
        }

        if ( (cmd != null) && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);

            KeyStroke ks = Preferences.getShortcut(cmd);

            if (ks != null) {
                String sc = ks.toString();
                sc = sc.replaceAll("pressed", "").replaceAll("ctrl", "Ctrl").replaceAll("shift", "Shift").replaceAll(
                        "alt", "Alt").trim();

                menuAccel.setText(sc);
                menuAccel.setFont(MipavUtil.defaultAcceleratorFont);
                menuAccel.setForeground(Color.GRAY);
                menuAccel.setHorizontalAlignment(SwingConstants.TRAILING);
                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                gbc.insets = new Insets(0, 0, 0, 5);

                menuEntry.add(menuAccel, gbc);
            }
        }

        Dimension d = menuName.getPreferredSize();
        Dimension d2 = menuAccel.getPreferredSize();

        d.width += d2.width + paddingX;
        d.height = (d.height > d2.height) ? d.height : d2.height;

        d.height += 10;

        d.width += 20;

        menuEntry.setPreferredSize(d);

        menuEntry.setSelected(selected);

        menuItemVector.add(new MipavMenuItem(text, menuEntry));

        return menuEntry;

    }

    /**
     * Build a menu item using a pre-made UIParams
     * 
     * @param params the UIParams (from CustomUIBuilder)
     * @param useIconPadding use icon padding
     * @return
     */
    public JMenuItem buildMenuItem(UIParams params, boolean useIconPadding) {
        if ( useIconPadding )
        {
            return buildMenuItem(params.getText(), params.getActionCommand(), params.getMnemonic(), params.getIconBase()
                    + ".gif", useIconPadding);
        }
        return buildMenuItem(params.getText(), params.getActionCommand(), params.getMnemonic(), params.getIconBase(), useIconPadding);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param text String
     * @param cmd String
     * @param mnemonic int
     * @param iconName String
     * @param useIconPadding boolean
     * 
     * @return JMenuItem
     */
    public JMenuItem buildMenuItem(String text, String cmd, int mnemonic, String iconName, boolean useIconPadding) {
        JMenuItem menuEntry = new JMenuItem();

        if (listener != null) {
            menuEntry.addActionListener(listener);
        }

        // set Icon
        ImageIcon icon = null;

        if (iconName != null) {
            icon = MipavUtil.getIcon(iconName);
        }

        try {

            if (icon != null) {

                menuEntry.setIcon(icon);

                // in case someone mistakenly sends in an icon without telling it to pad....
                useIconPadding = true;
            }
        } catch (Exception e) {}

        if (mnemonic != 0) {
            menuEntry.setMnemonic(mnemonic);
        }

        menuEntry.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        JLabel menuName = new JLabel();
        JLabel menuAccel = new JLabel();

        menuName.setText(text);
        menuName.setFont(MipavUtil.defaultMenuFont);
        
        menuEntry.setName(text);
        
        int paddingX = 8;

        if (useIconPadding) {
            paddingX = 5 + MipavUtil.DEFAULT_ICON_WIDTH;
        }

        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;

        gbc.insets = new Insets(0, paddingX, 0, 0);

        menuEntry.add(menuName, gbc);

        if (cmd == null) {
            cmd = new String(text);
        }

        if ( (cmd != null) && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);

            KeyStroke ks = null;

            ks = Preferences.getShortcut(cmd);

            if (ks != null) {
                String sc = ks.toString();
                sc = sc.replaceAll("pressed", "").replaceAll("ctrl", "Ctrl").replaceAll("shift", "Shift").replaceAll(
                        "alt", "Alt").trim();

                menuAccel.setText(sc);
                menuAccel.setFont(MipavUtil.defaultAcceleratorFont);
                menuAccel.setForeground(Color.GRAY);

                menuAccel.setHorizontalAlignment(SwingConstants.TRAILING);
                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                gbc.insets = new Insets(0, 0, 0, 5);

                menuEntry.add(menuAccel, gbc);
            }
        }

        Dimension d = menuName.getPreferredSize();
        Dimension d2 = menuAccel.getPreferredSize();

        d.width += d2.width + paddingX;
        d.height = (d.height > d2.height) ? d.height : d2.height;

        d.height += 10;

        d.width += 20;

        if (icon != null) {
            d.height = (d.height > icon.getIconHeight()) ? d.height : icon.getIconHeight();
        }

        menuEntry.setPreferredSize(d);

        menuItemVector.addElement(new MipavMenuItem(text, menuEntry));

        return menuEntry;
    }

    /**
     * Builds the quicklist for the first time.
     * 
     * @return QuickList
     */
    public QuickList buildQuickList() {
        return new QuickList(listener);
    }

    /**
     * Clean up memory used by the menu builder.
     */
    public void finalize() {

        if (menuItemVector != null) {
            menuItemVector.removeAllElements();
        }

        menuItemVector = null;
        listener = null;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param menuItemName DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public JMenuItem getMenuItem(String menuItemName) {
        for (int i = 0; i < menuItemVector.size(); i++) {
            if (menuItemVector.elementAt(i).getName().equals(menuItemName)) {
                return menuItemVector.elementAt(i).getItem();
            }
        }
        return null;
    }

    /**
     * Provides a method of checking the state of specified menu.
     * 
     * @param name The name of the item whose state needs to be checked.
     * 
     * @return Whether or not the item is enabled.
     */
    public boolean isMenuItemEnabled(String name) {

        JMenuItem menuItem = null;
        int i;

        for (i = 0; i < menuItemVector.size(); i++) {

            if (menuItemVector.elementAt(i).getName().equals(name)) {
                menuItem = menuItemVector.elementAt(i).getItem();

                break;
            }
        }

        if (menuItem == null) {
            Preferences.debug("Called isEnabled on " + name + " which does not exist.\n", Preferences.DEBUG_MINOR);

            return false;
        } else {
            return menuItem.isEnabled();
        }
    }

    /**
     * Provides a method of checking the state of specified menu.
     * 
     * @param name The name of the item whose state needs to be checked.
     * 
     * @return Whether or not the item is selected.
     */
    public boolean isMenuItemSelected(String name) {

        JMenuItem menuItem = getMenuItem(name);
        JCheckBoxMenuItem checkItem;

        if (menuItem == null) {
            System.err.println("called isSelected on " + name + " which does not exist.");

            return false;
        }

        try {
            checkItem = (JCheckBoxMenuItem) menuItem;

            return ((JCheckBoxMenuItem) checkItem).isSelected();
        } catch (ClassCastException e) {
            System.err.println("called isSelected on " + name + " which is not a checkbox.");

            return false;
        }
    }

    /**
     * Makes a menu out of the menu components, using <code>parentMenu</code> as the base for this menu.
     * 
     * @param parentMenu The parent menu, must be of type <code>String</code> or a <code>JMenu</code>, cannot be
     *            null.
     * @param iconPadding Array of menu items, must be of type<code>JMenu</code>, <code>JMenuItem</code>, <code>
     *                         JCheckBoxMenuItem</code>,
     *            or <code>JSeparator</code>.
     * @param menuComponent Parent frame, used to add as an item listener to the checkbox menu items.
     * 
     * @return The newly created menu.
     */
    public JMenu makeMenu(Object parentMenu, boolean iconPadding, JComponent[] menuComponent) {
        JMenu menu;
        int i;

        try {

            if (parentMenu instanceof String) {
                menu = buildMenu((String) parentMenu, 0, iconPadding);
            } else if (parentMenu instanceof JMenu) {
                menu = (JMenu) parentMenu;
            } else {
                return null;
            }

            for (i = 0; i < menuComponent.length; i++) {

                if (menuComponent[i] instanceof JMenu) {
                    menu.add((JMenu) menuComponent[i]);
                } else if (menuComponent[i] instanceof JMenuItem) {
                    menu.add((JMenuItem) menuComponent[i]);
                } else if ( (menuComponent[i] instanceof JCheckBoxMenuItem) && (listener instanceof ItemListener)) {
                    JCheckBoxMenuItem checkboxItem = (JCheckBoxMenuItem) menuComponent[i];
                    checkboxItem.addItemListener((ItemListener) listener);
                    menu.add(checkboxItem);
                } else if (menuComponent[i] instanceof JSeparator) {
                    menu.addSeparator();
                }
            }

            menuItemVector.addElement(new MipavMenuItem(menu.getText(), menu));

            return menu;
        } catch (OutOfMemoryError error) {
            System.gc();

            return null;
        }
    }

    /**
     * Makes a menu out of the menu components, using <code>parentMenu</code> as the base for this menu and adding a
     * mnemonic.
     * 
     * @param parentMenu The parent menu, must be of type <code>String</code> or a <code>JMenu</code>, cannot be
     *            null.
     * @param mnemonic Mnemonic key for the parent menu name.
     * @param iconPadding Array of menu items, must be of type<code>JMenu</code>, <code>JMenuItem</code>, <code>
     *                         JCheckBoxMenuItem</code>,
     *            or <code>JSeparator</code>.
     * @param menuComponent Parent frame, used to add as an item listener to the checkbox menu items.
     * 
     * @return The newly created menu.
     */
    public JMenu makeMenu(Object parentMenu, char mnemonic, boolean iconPadding, JComponent[] menuComponent) {

        JMenu menu;
        int i;

        try {

            if (parentMenu instanceof String) {
                menu = buildMenu((String) parentMenu, mnemonic, iconPadding);
            } else if (parentMenu instanceof JMenu) {
                menu = (JMenu) parentMenu;
            } else {
                return null;
            }

            if (Character.isDefined(mnemonic)) { // the check may not be needed...
                menu.setMnemonic(mnemonic);
            }

            for (i = 0; i < menuComponent.length; i++) {

                if (menuComponent[i] instanceof JMenu) {
                    menu.add((JMenu) menuComponent[i]);
                } else if (menuComponent[i] instanceof JMenuItem) {
                    menu.add((JMenuItem) menuComponent[i]);
                } else if ( (menuComponent[i] instanceof JCheckBoxMenuItem) && (listener instanceof ItemListener)) {
                    JCheckBoxMenuItem checkboxItem = (JCheckBoxMenuItem) menuComponent[i];
                    checkboxItem.addItemListener((ItemListener) listener);
                    menu.add(checkboxItem);
                } else if (menuComponent[i] instanceof JSeparator) {
                    menu.addSeparator();
                } else if (menuComponent[i] instanceof QuickList) {
                    Vector<JMenuItem> list = ((QuickList) menuComponent[i]).getList();

                    // save the index of the quicklist here for rebuilding
                    quicklistIndex = menu.getItemCount();

                    if (list.size() > 0) {
                        for (int j = 0; j < list.size(); j++) {
                            menu.add(list.elementAt(j));
                        }
                    }

                    this.quickList = (QuickList) menuComponent[i];
                }
            }

            menuItemVector.addElement(new MipavMenuItem(menu.getText(), menu));

            // if we just built the file menu, save the reference here
            if ( (parentMenu instanceof String) && ((String) parentMenu).equals("File")) {

                // System.err.println("Saved file menu");
                this.fileMenu = menu;
            }
            
            addMenuDragListener(menu, op);
            
            return menu;
        } catch (OutOfMemoryError error) {
            System.gc();

            return null;
        }
    }
    
    

    public JMenu makeMenu(Object parentMenu, char mnemonic, boolean iconPadding, Vector<JComponent> menuComponent) {

        JMenu menu;
        int i;

        try {

            if (parentMenu instanceof String) {
                menu = buildMenu((String) parentMenu, mnemonic, iconPadding);
            } else if (parentMenu instanceof JMenu) {
                menu = (JMenu) parentMenu;
            } else {
                return null;
            }

            if (Character.isDefined(mnemonic)) { // the check may not be needed...
                menu.setMnemonic(mnemonic);
            }

            for (i = 0; i < menuComponent.size(); i++) {

                if (menuComponent.elementAt(i) instanceof JMenu) {
                    menu.add((JMenu) menuComponent.elementAt(i));
                } else if (menuComponent.elementAt(i) instanceof JMenuItem) {
                    menu.add((JMenuItem) menuComponent.elementAt(i));
                } else if ( (menuComponent.elementAt(i) instanceof JCheckBoxMenuItem) && (listener instanceof ItemListener)) {
                    JCheckBoxMenuItem checkboxItem = (JCheckBoxMenuItem) menuComponent.elementAt(i);
                    checkboxItem.addItemListener((ItemListener) listener);
                    menu.add(checkboxItem);
                } else if (menuComponent.elementAt(i) instanceof JSeparator) {
                    menu.addSeparator();
                } else if (menuComponent.elementAt(i) instanceof QuickList) {
                    Vector<JMenuItem> list = ((QuickList) menuComponent.elementAt(i)).getList();

                    // save the index of the quicklist here for rebuilding
                    quicklistIndex = menu.getItemCount();

                    if (list.size() > 0) {
                        for (int j = 0; j < list.size(); j++) {
                            menu.add(list.elementAt(j));
                        }
                    }

                    this.quickList = (QuickList) menuComponent.elementAt(i);
                }
            }

            menuItemVector.addElement(new MipavMenuItem(menu.getText(), menu));

            // if we just built the file menu, save the reference here
            if ( (parentMenu instanceof String) && ((String) parentMenu).equals("File")) {

                // System.err.println("Saved file menu");
                this.fileMenu = menu;
            }
            
            addMenuDragListener(menu, op);
            
            return menu;
        } catch (OutOfMemoryError error) {
            System.gc();

            return null;
        }
    }
    
    /**
     * Provides a method of setting the state of specified menu.
     * 
     * @param name The name of the item whose state needs to be set.
     * @param state <code>true</code> sets the menu item active; <code>false</code> sets the menu item inactive.
     */
    public void setMenuItemEnabled(String name, boolean state) {
        JMenuItem menuItem;
        boolean found = false;

        for (int i = 0; i < menuItemVector.size(); i++) {

            if (menuItemVector.elementAt(i).getName().equals(name)) {
                menuItem = menuItemVector.elementAt(i).getItem();
                menuItem.setEnabled(state);
                MipavUtil.setComponentsEnabled(menuItem, state);
                found = true;
            }
        }

        if ( !found) {
            Preferences.debug("Unable to find menu item named " + name + "\n", Preferences.DEBUG_MINOR);
        }
    }

    /**
     * Provides a method for changing the status of a checkbox menu item.
     * 
     * @param actionCommand The name of the action command whose state needs to be checked.
     * @param val whether the checkbox should be selected.
     */
    public void setMenuItemSelected(String name, boolean val) {
        JMenuItem menuItem = getMenuItem(name);
        JCheckBoxMenuItem checkItem;

        if (menuItem == null) {
            Preferences.debug("called setSelected on " + name + " which does not exist.\n", Preferences.DEBUG_MINOR);

            return;
        }

        try {
            checkItem = (JCheckBoxMenuItem) menuItem;
        } catch (ClassCastException e) {
            Preferences.debug("called setSelected on " + name + " which is not a checkbox.\n", Preferences.DEBUG_MINOR);

            return;
        }

        checkItem.setSelected(val);
    }

    /**
     * Updates the most recently opened images list (called when images are opened etc).
     */
    public void updateQuickList() {

        // and this MUST be changed if new items are added

        // System.err.println("Updating quickList");
        Vector<JMenuItem> list = quickList.getList();

        for (int i = 0; i < list.size(); i++) {
            fileMenu.remove(list.elementAt(i));
        }

        quickList.rebuild();
        list = quickList.getList();

        for (int i = 0; i < list.size(); i++) {
            fileMenu.add(list.elementAt(i), (quicklistIndex + i));
        }
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Attaches a MenuDragMouseListener to <code>comp</code> and all of its
     * sub-components (if comp is a JMenu/JMenuItem.
     */
    private void addMenuDragListener(Component comp, MenuDragMouseListener op) {
         if(comp instanceof JMenuItem) {
            ((JMenuItem)comp).addMenuDragMouseListener(op);
            if(comp instanceof JMenu) {
                JMenu compSub = (JMenu)comp;
                for(int i=0; i<compSub.getMenuComponentCount(); i++) {
                    addMenuDragListener(compSub.getMenuComponent(i), op);
                }
            }
         } else if(comp instanceof JMenuBar) {
             JMenuBar bar = (JMenuBar)comp;
             for(int i=0; i<bar.getComponentCount(); i++) {
                 addMenuDragListener(bar.getComponent(i), op);
             }
         }
    }

    /**
     * QuickList class uses Preferences to build a list of the most recently opened images. the frame is passed in for
     * purposes of ActionListener The QuickList is stored as JMenuItems in a Vector that can be retrieved.
     * 
     * @author not attributable
     * @version 1.0
     */
    public class QuickList extends JComponent {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -4787325627153954566L;

        /** DOCUMENT ME! */
        private ActionListener listener = null;

        /** DOCUMENT ME! */
        private Vector<JMenuItem> quickListItems = null;

        /**
         * Creates a new QuickList object.
         * 
         * @param listener DOCUMENT ME!
         */
        public QuickList(ActionListener listener) {
            this.listener = listener;
            this.quickListItems = new Vector<JMenuItem>();
            rebuild();
        }

        /**
         * Returns the vector of quicklist items (JMenuItems w\ actionlistener attached).
         * 
         * @return Vector
         */
        public Vector<JMenuItem> getList() {
            return this.quickListItems;
        }

        /**
         * Rebuilds the QuickList by calling Preferences.getLastXImages().
         */
        private void rebuild() {
            Enumeration<JMenuItem> e = quickListItems.elements();
            JMenuItem temp = null;
            ActionListener[] listeners = null;
            int index = 0;

            while (e.hasMoreElements()) {
                temp = e.nextElement();
                listeners = temp.getActionListeners();

                for (index = 0; index < listeners.length; index++) {
                    temp.removeActionListener(listeners[index]);
                }

                temp.removeAll();
            }

            quickListItems.removeAllElements();

            String[] lastImages = Preferences.getLastXImages();

            String tempStr;

            int numDims;
            int length;

            if (lastImages != null) {

                for (int i = 0; i < lastImages.length; i++) {
                    // System.err.println("lastimage " + i + " " + lastImages[i]); make menu item and add actionListener

                    tempStr = new String(lastImages[i]);


                    length = tempStr.length();

                    if (tempStr.endsWith("M")) {
                    	try {
                            numDims = Integer.parseInt(tempStr.substring(length - 2, length - 1));
                    	}
                    	catch (NumberFormatException ex) {
                    		continue;
                    	}
                        tempStr = tempStr.substring(0, tempStr.lastIndexOf(","));
                        temp = ViewMenuBuilder.buildMenuItem( (i + 1) + " " + tempStr, "LastImage " + i, 0, listener,
                                "multifile_" + numDims + "d.gif", true);
                    } else {
                    	try {
                            numDims = Integer.parseInt(tempStr.substring(length - 1, length));
                    	}
                    	catch (NumberFormatException ex) {
                    		continue;
                    	}
                        tempStr = tempStr.substring(0, tempStr.lastIndexOf(","));
                        temp = ViewMenuBuilder.buildMenuItem( (i + 1) + " " + tempStr, "LastImage " + i, 0, listener,
                                "singlefile_" + numDims + "d.gif", true);
                    }

                    temp.setToolTipText(Preferences.getLastImageAt(i));

                    // temp.setAccelerator(KeyStroke.getKeyStroke('0' + (i + 1), Event.CTRL_MASK));
                    quickListItems.add(temp);
                }
            }
        }
    }

    /**
     * Class for listening to dragging events within a menu.
     * 
     * @author senseneyj
     *
     */
    private class MenuDragOp implements MenuDragMouseListener {
    
        private int insideCount = 0;
        
        private boolean draggingGlobal = false;
        
        private MenuMouse menu = null;
        
        @Override
        public void menuDragMouseDragged(MenuDragMouseEvent e) {
            insideCount++;
            if(insideCount > 10 && !draggingGlobal) {
                if(e.getComponent().getParent() instanceof JMenu || e.getComponent().getParent() instanceof JPopupMenu) {
                    int index = 0;
                    JComponent parent = (JComponent)e.getComponent().getParent();
                    if(e.getComponent().getParent() instanceof JMenu) {
                        JMenu parentSub = (JMenu) e.getComponent().getParent();
                        for(int i=0; i<parentSub.getMenuComponentCount(); i++) {
                            if(parentSub.getMenuComponent(i) == e.getComponent()) {
                                index = i;
                                break;
                            }
                        }
                    } else { //instanceof JPopupMenu
                        JPopupMenu parentSub = (JPopupMenu) e.getComponent().getParent();
                        int count = parentSub.getComponentCount();
                        for(int i=0; i<count; i++) {
                            if(parentSub.getComponent(i) == e.getComponent()) {
                                index = i;
                                break;
                            }
                        }
                    }
                    menu = new MenuMouse(e.getButton(), (JComponent) parent, index);
                    draggingGlobal = true;
                    java.awt.Toolkit.getDefaultToolkit().addAWTEventListener(menu, AWTEvent.MOUSE_EVENT_MASK);
                } else {
                    insideCount = 0;
                    //menu = null;
                }
            }
        }
    
        @Override
        public void menuDragMouseEntered(MenuDragMouseEvent e) {
            insideCount = 0;
        }
    
        @Override
        public void menuDragMouseExited(MenuDragMouseEvent e) {
            insideCount = 0;
        }
    
        @Override
        public void menuDragMouseReleased(MenuDragMouseEvent e) {
            insideCount = 0;
        }
        
        /**
         * Class for listening to mouse events after a candidate menu is being dragged.
         * 
         * @author senseneyj
         *
         */
        private class MenuMouse implements MouseListener, AWTEventListener {
    
            private int button;
            private JComponent parent; //either a JPopupMenu or a JMenu
            private int index;
            private ViewMenuBuilder build;
    
            public MenuMouse(int button, JComponent parent, int index) {
                this.button = button;
                this.parent = parent;
                this.index = index;
                build = new ViewMenuBuilder(ViewUserInterface.getReference());
            }
            
            public void mouseClicked(MouseEvent e) {}
    
            public void mouseEntered(MouseEvent e) {}
    
            public void mouseExited(MouseEvent e) {}
    
            public void mousePressed(MouseEvent e) {}
    
            public void mouseReleased(MouseEvent e) {
                if((button == e.getButton() || button == MouseEvent.NOBUTTON) && menu != null) {
                    int downIndex = index;
                    while(downIndex > 0) {
                        if(testComponent(parent, downIndex) instanceof JMenuItem) {
                            downIndex--;
                        } else {
                            break;
                        }
                    }
                    int upIndex = index;
                    int count = getCount(parent);
                    while(upIndex < count) {
                        if(testComponent(parent, upIndex) instanceof JMenuItem) {
                            upIndex++;
                        } else {
                            break;
                        }
                    }
                    JFrame frame = new JFrame();
                    //JDialog dialog = new JDialog(frame);
                    
                    
                    
                    JPanel compPanel = new JPanel();
                    BoxLayout y = new BoxLayout(compPanel, BoxLayout.Y_AXIS);
                    compPanel.setLayout(y);
                    final JMenuBar bar = new JMenuBar();
                    y = new BoxLayout(bar, BoxLayout.Y_AXIS);
                    bar.setLayout(y);
                    bar.setFocusable(true);
                    JMenu menu = new JMenu();
                    
                    int height = 4;
                    int width = parent.getWidth()+5;
                    
                    try {
                        for(int i=downIndex; i<upIndex; i++) {
                            JMenuItem tempItem = getComponent(parent, i);
                            height += tempItem.getHeight();
                            JMenuItem newItem = buildItem(tempItem, menu);
                            if(newItem instanceof JMenu) {
                                newItem.setText(newItem.getText()+"  \u21b4"); //down arrow
                            }
                            bar.add(newItem);
                        }
                    } catch(Exception ex) {
                        ex.printStackTrace();
                        System.out.println("Invalid menu item count");
                    }
                    
                    addMenuDragListener(bar, op);
                    
                    compPanel.setPreferredSize(new Dimension(width, height));
                    compPanel.add(bar);
                    String title = "MIPAV: ";
                    if(parent instanceof JMenuItem) {
                        title += ((JMenuItem) parent).getText();
                    } else if(parent instanceof JPopupMenu) {
                        title += ((JPopupMenu) parent).getInvoker().getName();
                    } 
                    compPanel.setBorder(MipavUtil.buildTitledBorder(title));
                    compPanel.setFocusable(true);
                    
                    JPanel outerPanel = new JPanel();
                    outerPanel.setPreferredSize(new Dimension(width, height));
                    outerPanel.add(compPanel);
                    
                    frame.setTitle(title);
                    frame.getContentPane().add(outerPanel);
                    frame.pack();
                    frame.setLocation(e.getLocationOnScreen());
                    frame.setVisible(true);
                    frame.setResizable(false);
                    try {
                        frame.setIconImage(MipavUtil.getIconImage("divinci.gif"));
                    } catch (FileNotFoundException e1) {}
                    frame.addWindowListener(new WindowListener() {
                        
                        public void windowActivated(WindowEvent arg0) {}

                        @Override
                        public void windowClosed(WindowEvent arg0) {
                            ViewUserInterface.getReference().removeAloneMenu(bar);
                        }
                        public void windowClosing(WindowEvent arg0) {}
                        public void windowDeactivated(WindowEvent arg0) {}
                        public void windowDeiconified(WindowEvent arg0) {}
                        public void windowIconified(WindowEvent arg0) {}
                        public void windowOpened(WindowEvent arg0) {}
                        
                    });
                    
                    ViewUserInterface.getReference().addAloneMenu(bar);
                }
                dispose();
            }
            
            private void dispose() {
                java.awt.Toolkit.getDefaultToolkit().removeAWTEventListener(this);
                draggingGlobal = false;
                insideCount = 0;
            }

            private JMenuItem buildItem(JMenuItem tempItem, Object parentMenu) {
                if(tempItem instanceof JMenu) {
                    JMenu newMenu = new JMenu();
                    
                    JMenu menu = (JMenu)tempItem;
                    newMenu.setName(menu.getName());
                    newMenu.setText(menu.getText());
                    newMenu.setFont(menu.getFont());
                    //newMenu.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
                    ArrayList<JComponent> newMenuAr = new ArrayList<JComponent>();
                    for(int i=0; i<menu.getMenuComponentCount(); i++) {
                        if(menu.getMenuComponent(i) instanceof JMenuItem) {
                            newMenuAr.add(buildItem((JMenuItem) menu.getMenuComponent(i), newMenu));
                        }
                    }
                    newMenu = build.makeMenu(newMenu, false, newMenuAr.toArray(new JComponent[newMenuAr.size()]));
                    newMenu.setAlignmentX(Component.RIGHT_ALIGNMENT);
                    for(ActionListener l : menu.getActionListeners()) {
                        newMenu.addActionListener(l);
                    }
                    return newMenu;
                } else {
                    String name = tempItem.getName();
                    if(name == null || (tempItem.getText() != null && name.length() < tempItem.getText().length())) {
                        name = tempItem.getText();
                    }
                    Icon icon = tempItem.getIcon();
                    boolean usePadding = false;
                    String iconName = null;
                    if(icon != null) {
                        usePadding = true;
                        try {
                            iconName = icon.toString().substring(icon.toString().lastIndexOf('/')+1);
                        } catch(Exception e) {
                            iconName = icon.toString();
                        }
                    }
                    JMenuItem newItem = build.buildMenuItem(name, tempItem.getActionCommand(), tempItem.getMnemonic(), iconName, usePadding);
                    newItem.setEnabled(tempItem.isEnabled());
                    newItem.setFont(tempItem.getFont());
                    newItem.setMinimumSize(tempItem.getMinimumSize());
                    newItem.setPreferredSize(tempItem.getPreferredSize());
                    newItem.setMaximumSize(tempItem.getMaximumSize());
                    newItem.setAlignmentX(Component.RIGHT_ALIGNMENT);
                    for(ActionListener l : tempItem.getActionListeners()) {
                        newItem.addActionListener(l);
                    }
                    
                    return newItem;
                }
            }
    
            @Override
            public void eventDispatched(AWTEvent event) {
                if(event instanceof MouseEvent) {
                    if(event.getID() == MouseEvent.MOUSE_RELEASED) {
                        mouseReleased((MouseEvent)event);
                    }
                }
            }
            
        }
        
        private int getCount(JComponent comp) {
            if(comp instanceof JMenu) {
                return ((JMenu)comp).getMenuComponentCount();
            } else { //instanceof JPopupMenu
                return ((JPopupMenu)comp).getComponentCount();
            }
        }
        
        private JMenuItem getComponent(JComponent comp, int index) {
            if(comp instanceof JMenu) {
                return (JMenuItem) ((JMenu) comp).getMenuComponent(index);
            } else { //instanceof JPopupMenu
                return (JMenuItem) ((JPopupMenu)comp).getComponent(index);
            }
        }
        
        private Component testComponent(JComponent comp, int index) {
            if(comp instanceof JMenu) {
                return ((JMenu) comp).getMenuComponent(index);
            } else if(comp instanceof JPopupMenu) {
                return ((JPopupMenu)comp).getComponent(index);
            } else {
                return null;
            }
        }
        
    }

    /**
     * DOCUMENT ME!
     */
    private class MipavMenuItem {

        /** DOCUMENT ME! */
        private JMenuItem item = null;

        /** DOCUMENT ME! */
        private String name = null;

        /**
         * Creates a new MipavMenuItem object.
         * 
         * @param name DOCUMENT ME!
         * @param item DOCUMENT ME!
         */
        public MipavMenuItem(String name, JMenuItem item) {
            this.name = name;
            this.item = item;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public JMenuItem getItem() {
            return this.item;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public String getName() {
            return this.name;
        }
    }
}
