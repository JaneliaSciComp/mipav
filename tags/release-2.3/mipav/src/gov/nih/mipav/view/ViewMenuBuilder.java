package gov.nih.mipav.view;

import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

/**
 *   This class provides a number of helper methods for building and manipulating menus.
 */
public class ViewMenuBuilder {
    /** Holder of all the menu items. */
    private Vector menuItemVector;

    /** Frame to add menus to. */
    private JFrame frame;

    /** Link to the file menu so that quicklist can be rebuilt and added */
    private JMenu fileMenu;

    /** List that holds the last X number of recently opened images */
    private QuickList quickList;

    /**
     *  Sets frame to add menus to, and initializes menu item vector.
     *  @param _UI   Frame to add menus to.
     */
    public ViewMenuBuilder(JFrame _UI) {
        frame = _UI;
        menuItemVector = new Vector();
    }

    /**
     * Clean up memory used by the menu builder.
     */
    public void finalize() {
        if (menuItemVector != null) {
            menuItemVector.removeAllElements();
        }
        menuItemVector = null;
        frame = null;
    }

    /**
     *   Builds a menu item that can be added to a menu.
     *   @param  menuTitle       Title of the menu item, must be non null.
     *   @param  actionCommand   Action command of the menu item, can be null.
     *   @param  font            Font of the menu item, must be non null.
     *   @param  listener        Action listener for the menu item, can be null.
     *   @param  keyStroke       Keystroke to set menu item accelerator, can be null.
     *   @param  iconName        Name of icon to be associated to the menu item, can be null.
     *   @return                 The menu item or null if an error has occurred.

    public static JMenuItem buildMenuItem(String menuTitle, String actionCommand, Font font,
                                          ActionListener listener, KeyStroke keyStroke, String iconName) {

        return buildMenuItem(menuTitle, actionCommand, 0, listener, iconName, true);
    }
*/


    /**
     * Static method for building a JMenu
     * @param text String the name of the menu to be displayed
     * @param mnemonic int mnemonic for the menu
     * @param iconPadding boolean an empty icon (transparent) is used to properly pad the text
     * @return JMenu the menu that is built
     */
    public static JMenu buildMenu(String text, int mnemonic, boolean iconPadding) {
        JMenu menu = new JMenu();

        if (mnemonic != 0) {
            menu.setMnemonic(mnemonic);
        }

        menu.setText(text);

        if (iconPadding) {

            try {
                menu.setIcon(MipavUtil.getIcon("empty.gif"));
                menu.setIconTextGap(50);
            } catch (Exception e) {

            }
        }


        menu.setFont(MipavUtil.defaultMenuFont);

        Dimension d = menu.getPreferredSize();
        d.setSize(d.width, MipavUtil.MENU_Y_PADDING);

        menu.setPreferredSize(d);

        return menu;
    }


    public static JMenuItem buildHTMLMenuItem(String text, String cmd, int mnemonic,
                                          ActionListener listener, String iconName, boolean iconPadding) {
        JMenuItem menuEntry = new JMenuItem();
        menuEntry.addActionListener(listener);
        String mnemonicStr = null;

        //set Icon
        ImageIcon icon = null;
        if (iconName != null) icon = MipavUtil.getIcon(iconName);
        try {
            if (icon != null) {

                menuEntry.setIcon(icon);
            }
        }
        catch (Exception e) {

        }

        //get the size of the font (HTML SIZES)
        int size = 3;

        try {
            size = Integer.parseInt(Preferences.getProperty(Preferences.PREF_MENU_FONT_SIZE));
        }
        catch (Exception e) {
        }

        //get the name of the font to be used
        String fontName = "Serif";
        if (Preferences.getProperty(Preferences.PREF_MENU_FONT) != null) {
            fontName = Preferences.getProperty(Preferences.PREF_MENU_FONT);
        }

        if (mnemonic != 0) {
            menuEntry.setMnemonic(mnemonic);

            int index = text.indexOf(mnemonic);

            //if the mnemonic is present as a letter, make it underlined
            if (index != -1) {
                if (index == 0) {
                    mnemonicStr = MipavUtil.makeHTMLFontString(Color.black, fontName, size,
                        Font.PLAIN, true, text.substring(0, 1));
                    mnemonicStr += MipavUtil.makeHTMLFontString(Color.black, fontName, size,
                        Font.PLAIN, false, text.substring(1, text.length()));

                }
                else {
                    mnemonicStr = MipavUtil.makeHTMLFontString(Color.black, fontName, size,
                        Font.PLAIN, false, text.substring(0, index));
                    mnemonicStr += MipavUtil.makeHTMLFontString(Color.black, fontName, size,
                        Font.PLAIN, true, text.substring(index, index + 1));

                    if (index != text.length() - 1) {
                        mnemonicStr += MipavUtil.makeHTMLFontString(Color.black, fontName, size,
                            Font.PLAIN, false, text.substring(index + 1));
                    }

                }
            }
            else {
                mnemonicStr = MipavUtil.makeHTMLFontString(Color.black, fontName, size,
                    Font.PLAIN, false, text);
            }
        }

        menuEntry.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        JLabel menuName = new JLabel();
        JLabel menuAccel = new JLabel();

        menuName.setText("<html>" + MipavUtil.makeHTMLFontString(Color.black, fontName, size,
            Font.PLAIN, false, text) + "</html>");

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

        if (cmd != null && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);
            KeyStroke ks = null;

            ks = Preferences.getShortcut(cmd);

            if (ks != null) {
                String sc = ks.toString();
                sc = sc.replaceAll("pressed", "").replaceAll("ctrl","Ctrl").replaceAll("shift","Shift").replaceAll("alt","Alt").trim();

                menuAccel.setText("<html>" +
                                  MipavUtil.makeHTMLFontString(Color.LIGHT_GRAY, fontName, size, Font.PLAIN, false, sc) +
                                  "</html>");

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
        d.height = d.height > d2.height ? d.height : d2.height;

        d.height += 10;

        d.width += 20;
        if (icon != null) {
            d.height = d.height > icon.getIconHeight() ? d.height : icon.getIconHeight();
        }

        menuEntry.setPreferredSize(d);

        return menuEntry;
    }

    public static JMenuItem buildMenuItem(String text, String cmd, int mnemonic,
                                          ActionListener listener, String iconName, boolean iconPadding) {
        JMenuItem menuEntry = new JMenuItem();
        menuEntry.addActionListener(listener);

        //set Icon
        ImageIcon icon = null;
        if (iconName != null) icon = MipavUtil.getIcon(iconName);
        try {
            if (icon != null) {

                menuEntry.setIcon(icon);
            }
        }
        catch (Exception e) {

        }

        if (mnemonic != 0) {
            menuEntry.setMnemonic(mnemonic);
        }

        menuEntry.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        JLabel menuName = new JLabel();
        JLabel menuAccel = new JLabel();

        menuName.setText(text);
        menuName.setFont(MipavUtil.defaultMenuFont);

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

        if (cmd != null && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);

            try {
                KeyStroke ks = Preferences.getShortcut(cmd);

                if (ks != null) {
                    String sc = ks.toString();
                    sc = sc.replaceAll("pressed", "").replaceAll("ctrl", "Ctrl").replaceAll("shift", "Shift").replaceAll("alt",
                        "Alt").trim();

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
                System.err.println("Command is: "  + cmd);
                e.printStackTrace();
            }
        }
        Dimension d = menuName.getPreferredSize();
        Dimension d2 = menuAccel.getPreferredSize();

        d.width += d2.width + paddingX;
        d.height = d.height > d2.height ? d.height : d2.height;

        d.height += 10;

        d.width += 20;
        if (icon != null) {
            d.height = d.height > icon.getIconHeight() ? d.height : icon.getIconHeight();
        }

        menuEntry.setPreferredSize(d);
        //System.err.println(d);

        return menuEntry;
    }



    /**
     *
     * @param text String
     * @param cmd String
     * @param mnemonic int
     * @param iconName String
     * @param useIconPadding boolean
     * @return JMenuItem
     */
    public JMenuItem buildMenuItem(String text, String cmd, int mnemonic,
                                   String iconName, boolean useIconPadding) {
        JMenuItem menuEntry = new JMenuItem();

       if ( (ActionListener) frame != null)
           menuEntry.addActionListener( (ActionListener) frame);

        //set Icon
        ImageIcon icon = null;
        if (iconName != null) icon = MipavUtil.getIcon(iconName);
        try {
            if (icon != null) {

                menuEntry.setIcon(icon);
                //in case someone mistakenly sends in an icon without telling it to pad....
                useIconPadding = true;
            }
        }
        catch (Exception e) {

        }

        if (mnemonic != 0) {
            menuEntry.setMnemonic(mnemonic);
        }

        menuEntry.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        JLabel menuName = new JLabel();
        JLabel menuAccel = new JLabel();

        menuName.setText(text);
        menuName.setFont(MipavUtil.defaultMenuFont);

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

        if (cmd != null && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);
            KeyStroke ks = null;

            ks = Preferences.getShortcut(cmd);

            if (ks != null) {
                String sc = ks.toString();
                sc = sc.replaceAll("pressed", "").replaceAll("ctrl","Ctrl").replaceAll("shift","Shift").replaceAll("alt","Alt").trim();

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
        d.height = d.height > d2.height ? d.height : d2.height;

        d.height += 10;

        d.width += 20;
        if (icon != null) {
            d.height = d.height > icon.getIconHeight() ? d.height : icon.getIconHeight();
        }

        menuEntry.setPreferredSize(d);

        menuItemVector.addElement(new MipavMenuItem(text, menuEntry));

        return menuEntry;
    }


    /**
     * Static method for building JCheckBoxes with the given parameters
     * @param text String text to display on the box
     * @param cmd String the action command
     * @param listener ActionListener the action listener for the box
     * @param selected boolean whether the box should be selected by default
     * @return JCheckBoxMenuItem the jcheckbox that is built
     */
    public static JCheckBoxMenuItem buildCheckBoxMenuItem(String text, String cmd,
        ActionListener listener, boolean selected) {

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

        if (cmd != null && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);
            KeyStroke ks = Preferences.getShortcut(cmd);

            if (ks != null) {
                String sc = ks.toString();
                sc = sc.replaceAll("pressed", "").replaceAll("ctrl","Ctrl").replaceAll("shift","Shift").replaceAll("alt","Alt").trim();

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
        d.height = d.height > d2.height ? d.height : d2.height;

        d.height += 10;

        d.width += 20;

        menuEntry.setPreferredSize(d);

        menuEntry.setSelected(selected);

        return menuEntry;

    }

    /**
     * Builds a JCheckBox with the given parameters and adds it to the Vector of menu items
     * @param text String Checkbox text
     * @param cmd String actioncommand
     * @param selected boolean whether the box is selected by default
     * @return JCheckBoxMenuItem
     */
    public JCheckBoxMenuItem buildCheckBoxMenuItem(String text, String cmd, boolean selected) {

        JCheckBoxMenuItem menuEntry = new JCheckBoxMenuItem();

        if ( (ActionListener) frame != null)
            menuEntry.addActionListener( (ActionListener) frame);

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

        if (cmd != null && !cmd.equals("")) {
            menuEntry.setActionCommand(cmd);
            KeyStroke ks = Preferences.getShortcut(cmd);

            if (ks != null) {
                String sc = ks.toString();
                sc = sc.replaceAll("pressed", "").replaceAll("ctrl","Ctrl").replaceAll("shift","Shift").replaceAll("alt","Alt").trim();

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
        d.height = d.height > d2.height ? d.height : d2.height;

        d.height += 10;

        d.width += 20;

        menuEntry.setPreferredSize(d);

        menuEntry.setSelected(selected);

        menuItemVector.add(new MipavMenuItem(text, menuEntry));

        return menuEntry;

    }

    /**
     *	Makes a menu out of the menu components, using <code>parentMenu</code> as the
     *	base for this menu.
     *	@param parentMenu  The parent menu, must be of type <code>String</code> or a
     *					   <code>JMenu</code>, cannot be null.
     *	@param objs        Array of menu items, must be of type <code>JMenu</code>,
     *					   <code>JMenuItem</code>, <code>JCheckBoxMenuItem</code>, or
     *					   <code>JSeparator</code>.
     *	@param source      Parent frame, used to add as an item listener to the checkbox
     *					   menu items.
     *	@return            The newly created menu.
     */
    public JMenu makeMenu(Object parentMenu, boolean iconPadding, JComponent[] menuComponent) {
        JMenu menu;
        int i;

        try {
            if (parentMenu instanceof String) {
                menu = buildMenu((String)parentMenu, 0, iconPadding);
            }
            else if (parentMenu instanceof JMenu) {
                menu = (JMenu) parentMenu;
            }
            else {
                return null;
            }

            for (i = 0; i < menuComponent.length; i++) {
                if (menuComponent[i] instanceof JMenu) {
                    menu.add( (JMenu) menuComponent[i]);
                }
                else if (menuComponent[i] instanceof JMenuItem) {
                    menu.add( (JMenuItem) menuComponent[i]);
                }
                else if (menuComponent[i] instanceof JCheckBoxMenuItem
                         && frame instanceof ItemListener) {
                    JCheckBoxMenuItem checkboxItem = (JCheckBoxMenuItem) menuComponent[i];
                    checkboxItem.addItemListener( (ItemListener) frame);
                    menu.add(checkboxItem);
                }
                else if (menuComponent[i] instanceof JSeparator) {
                    menu.addSeparator();
                }
            }
            menuItemVector.addElement(new MipavMenuItem(menu.getText(), menu));
            return menu;
        }
        catch (OutOfMemoryError error) {
            System.gc();
            return null;
        }
    }

    /**
     *	Makes a menu out of the menu components, using <code>parentMenu</code> as the
     *	base for this menu and adding a mnemonic.
     *	@param parentMenu  The parent menu, must be of type <code>String</code> or a
     *					   <code>JMenu</code>, cannot be null.
     *	@param objs        Array of menu items, must be of type <code>JMenu</code>,
     *					   <code>JMenuItem</code>, <code>JCheckBoxMenuItem</code>, or
     *					   <code>JSeparator</code>.
     *	@param source      Parent frame, used to add as an item listener to the checkbox
     *					   menu items.
     *  @param mnemonic     Mnemonic key for the parent menu name.
     *  @return             The newly created menu.
     */
    public JMenu makeMenu(Object parentMenu, char mnemonic,
                          boolean iconPadding, JComponent[] menuComponent) {

        JMenu menu;
        int i;

        try {
            if (parentMenu instanceof String) {
                menu = buildMenu((String)parentMenu, mnemonic, iconPadding);
            }
            else if (parentMenu instanceof JMenu) {
                menu = (JMenu) parentMenu;
            }
            else {
                return null;
            }
            if (Character.isDefined(mnemonic)) { // the check may not be needed...
                menu.setMnemonic(mnemonic);
            }

            for (i = 0; i < menuComponent.length; i++) {
                if (menuComponent[i] instanceof JMenu) {
                    menu.add( (JMenu) menuComponent[i]);
                }
                else if (menuComponent[i] instanceof JMenuItem) {
                    menu.add( (JMenuItem) menuComponent[i]);
                }
                else if (menuComponent[i] instanceof JCheckBoxMenuItem
                         && frame instanceof ItemListener) {
                    JCheckBoxMenuItem checkboxItem = (JCheckBoxMenuItem) menuComponent[i];
                    checkboxItem.addItemListener( (ItemListener) frame);
                    menu.add(checkboxItem);
                }
                else if (menuComponent[i] instanceof JSeparator) {
                    menu.addSeparator();
                }
                else if (menuComponent[i] instanceof QuickList) {
                    Vector list = ( (QuickList) menuComponent[i]).getList();
                    for (int j = 0; j < list.size(); j++) {
                        menu.add( (JMenuItem) list.elementAt(j));
                    }
                    this.quickList = (QuickList) menuComponent[i];

                }
            }
            menuItemVector.addElement(new MipavMenuItem(menu.getText(), menu));

            //if we just built the file menu, save the reference here
            if (parentMenu instanceof String &&
                ( (String) parentMenu).equals("File")) {
                // System.err.println("Saved file menu");
                this.fileMenu = menu;
            }

            return menu;
        }
        catch (OutOfMemoryError error) {
            System.gc();
            return null;
        }
    }

    /**
     *   Provides a method of setting the state of specified menu.
     *   @param name		The name of the item whose state needs to be set.
     *   @param state    <code>true</code> sets the menu item active;
     *					<code>false</code> sets the menu item inactive.
     */
    public void setMenuItemEnabled(String name, boolean state) {
        JMenuItem menuItem;
        boolean found = false;

        for (int i = 0; i < menuItemVector.size(); i++) {
            if ( ( (MipavMenuItem) menuItemVector.elementAt(i)).getName().equals(name)) {
                menuItem = ((MipavMenuItem) (menuItemVector.elementAt(i))).getItem();
                menuItem.setEnabled(state);
                MipavUtil.setComponentsEnabled(menuItem, state);
                found = true;
            }
        }

        if (!found) {
            Preferences.debug("Unable to find menu item named " + name + "\n", Preferences.DEBUG_MINOR);
        }
    }

    /**
     *   Provides a method of checking the state of specified menu.
     *   @param name		The name of the item whose state needs to be checked.
     *   @return         Whether or not the item is enabled.
     */
    public boolean isMenuItemEnabled(String name) {

        JMenuItem menuItem = null;
        int i;

        for (i = 0; i < menuItemVector.size(); i++) {
            if ( ( (MipavMenuItem) menuItemVector.elementAt(i)).getName().equals(name)) {
                menuItem = ((MipavMenuItem) (menuItemVector.elementAt(i))).getItem();
                break;
            }
        }
        if (menuItem == null) {
            Preferences.debug("Called isEnabled on " + name + " which does not exist.");
            return false;
        }
        else return menuItem.isEnabled();
    }

    /**
     *	Provides a method of checking the state of specified menu.
     *   @param name		The name of the item whose state needs to be checked.
     *   @return         Whether or not the item is selected.
     */
    public boolean isMenuItemSelected(String name) {

        JMenuItem menuItem = null;
        JCheckBoxMenuItem checkItem;

        for (int i = 0; i < menuItemVector.size(); i++) {
            if ( ( (MipavMenuItem) menuItemVector.elementAt(i)).getName().equals(name)) {
                menuItem = ((MipavMenuItem) (menuItemVector.elementAt(i))).getItem();
                break;
            }
        }
        if (menuItem == null) {
            System.err.println("called isSelected on " + name + " which does not exist.");
            return false;
        }
        try {
            checkItem = (JCheckBoxMenuItem) menuItem;
            return ( (JCheckBoxMenuItem) checkItem).isSelected();
        }
        catch (ClassCastException e) {
            System.err.println("called isSelected on " + name + " which is not a checkbox.");
            return false;
        }
    }

    /**
     *	Provides a method for changing the status of a checkbox menu item.
     *	@param actionCommand The name of the action command whose state needs to be checked.
     *	@param val	whether the checkbox should be selected.
     */
    public void setMenuItemSelected(String actionCommand, boolean val) {
        JMenuItem menuItem = null;
        JCheckBoxMenuItem checkItem;
        MipavMenuItem mipavItem = null;

        for (int i = 0; i < menuItemVector.size(); i++) {
            mipavItem = (MipavMenuItem) menuItemVector.elementAt(i);
            if (mipavItem.getItem().getActionCommand() != null &&
                mipavItem.getItem().getActionCommand().equals(actionCommand)) {
                menuItem = mipavItem.getItem();
                break;
            }
        }
        if (menuItem == null) {
            Preferences.debug("called setSelected on " + actionCommand + " which does not exist.");
            return;
        }
        try {
            checkItem = (JCheckBoxMenuItem) menuItem;
        }
        catch (ClassCastException e) {
            Preferences.debug("called setSelected on " + actionCommand + " which is not a checkbox.");
            return;
        }

        checkItem.setSelected(val);
    }

    private class MipavMenuItem {
        private String name = null;
        private JMenuItem item = null;

        public MipavMenuItem(String name, JMenuItem item) {
            this.name = name;
            this.item = item;
        }

        public String getName() {
            return this.name;
        }

        public JMenuItem getItem() {
            return this.item;
        }


    }

    /**
     * Updates the most recently opened images list (called when images are opened etc)
     */
    public void updateQuickList() {
        int index = 25; // this is the number of items in the file menu BEFORE the quicklist
        // and this MUST be changed if new items are added

        //System.err.println("Updating quickList");
        Vector list = quickList.getList();
        for (int i = 0; i < list.size(); i++) {
            fileMenu.remove( (JMenuItem) list.elementAt(i));
        }
        quickList.rebuild();
        list = quickList.getList();
        for (int i = 0; i < list.size(); i++) {
            fileMenu.add( (JMenuItem) list.elementAt(i), (index + i));
        }
    }

    /**
     * Builds the quicklist for the first time
     * @return QuickList
     */
    public QuickList buildQuickList() {
        return new QuickList(frame);
    }

    /**
     * QuickList class uses Preferences to build a list of the most
     * recently opened images.  the frame is passed in for purposes of ActionListener
     * The QuickList is stored as JMenuItems in a Vector that can be retrieved.
     * @author not attributable
     * @version 1.0
     */
    public class QuickList
        extends JComponent {
        private Vector quickListItems = null;
        private JFrame listener = null;

        public QuickList(JFrame listener) {
            this.listener = listener;
            this.quickListItems = new Vector();
            rebuild();
        }

        /**
         * Returns the vector of quicklist items (JMenuItems w\ actionlistener attached)
         * @return Vector
         */
        public Vector getList() {
            return this.quickListItems;
        }

        /**
         * Rebuilds the QuickList by calling Preferences.getLastXImages()
         */
        private void rebuild() {

            Enumeration e = quickListItems.elements();
            JMenuItem temp = null;
            ActionListener [] listeners = null;
            int index = 0;

            while (e.hasMoreElements()) {
                temp = (JMenuItem)e.nextElement();
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
                    //System.err.println("lastimage " + i + " " + lastImages[i]);
                    //make menu item and add actionListener

                    tempStr = new String(lastImages[i]);

                    length = tempStr.length();

                    if (tempStr.endsWith("M"))
                    {
                        numDims = Integer.parseInt(tempStr.substring(length - 2, length - 1));
                        tempStr = tempStr.substring(0, tempStr.indexOf(","));
                        temp = ViewMenuBuilder.buildMenuItem((i+1) + " " + tempStr, "LastImage " + i, 0,
                            (ActionListener) frame, "multifile_" + numDims + "d.gif", true);
                    }
                    else
                    {
                        numDims = Integer.parseInt(tempStr.substring(length - 1, length));
                        tempStr = tempStr.substring(0, tempStr.indexOf(","));
                        temp = ViewMenuBuilder.buildMenuItem((i+1) + " " + tempStr, "LastImage " + i, 0,
                            (ActionListener) frame, "singlefile_" + numDims + "d.gif", true);
                    }

                    temp.setToolTipText(Preferences.getLastImageAt(i));
                    //temp.setAccelerator(KeyStroke.getKeyStroke('0' + (i + 1), Event.CTRL_MASK));
                    quickListItems.add(temp);
                }
            }
        }
    }
}
