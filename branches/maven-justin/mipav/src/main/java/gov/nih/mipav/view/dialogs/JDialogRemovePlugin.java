package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * Simple dialog to remove a plugin. The user selects which plugin to remove using a scroll pane. The user has the
 * option to also delete the file from MIPAV's class directory. The mipav.preferences file is updated accordingly. The
 * menubars are also updated.
 *
 * @version  1.0 July 19, 2000
 * @author   Harman Singh
 */

public class JDialogRemovePlugin extends JDialogBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6309199966911968353L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Toggles whether or not the plugin file should be deleted from MIPAV's class directory. */
    private JCheckBox destroyFileCheckbox;

    /** List of installed plugins. */
    private JList list;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new dialog to remove plugins.
     *
     * @param  theParentFrame  Parent frame.
     */
    public JDialogRemovePlugin(JFrame theParentFrame) {
        super(theParentFrame, false);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  Event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {
            String selectedPluginName = (String) (list.getSelectedValue());
            String plugInName;

            if (selectedPluginName == null) {
                return;
            }

            boolean plugInFound = false; // boolean for whether or not the selected
                                         // plugin was found in the mipav.preferences file
            int i, j;
            String classPath = System.getProperty("java.class.path"); // set to MIPAV's classpath
            i = classPath.indexOf(File.pathSeparatorChar);
            classPath = classPath.substring(0, i);

            // make sure that this first entry in the classpath is not a jar file
            if (classPath.endsWith(".jar")) {

                // need to trim to the directory containing the jar file
                int ind = classPath.lastIndexOf(File.separator) + 1;
                classPath = classPath.substring(0, ind);
            }


            for (i = 1; i < 100; i++) {
                plugInName = Preferences.getProperty(Preferences.PREF_PLUGIN_ALGORITHM + i);

                if (plugInName == null) {
                    break;
                } else if (selectedPluginName.equals(plugInName)) {
                    plugInFound = true;
                    Preferences.removeProperty(Preferences.PREF_PLUGIN_ALGORITHM + i); // removes the property from the Preference file

                    for (j = i + 1; j < 100; j++) {

                        // check whether plugin with a higher number needs to be changed and makes proper adjustments
                        plugInName = Preferences.getProperty(Preferences.PREF_PLUGIN_ALGORITHM + j);

                        if (plugInName == null) {
                            break;
                        }

                        Preferences.setProperty(Preferences.PREF_PLUGIN_ALGORITHM + (j - 1), plugInName);
                        Preferences.removeProperty(Preferences.PREF_PLUGIN_ALGORITHM + j);
                    }

                    Preferences.save();

                    break;
                }
            }

            if (!plugInFound) {

                for (i = 1; i < 100; i++) {
                    plugInName = Preferences.getProperty(Preferences.PREF_PLUGIN_FILE + i);

                    if (plugInName == null) {
                        break;
                    } else if (selectedPluginName.equals(plugInName)) {
                        plugInFound = true;
                        Preferences.removeProperty(Preferences.PREF_PLUGIN_FILE + i);

                        for (j = i + 1; j < 100; j++) {

                            // check whether plugin with a higher number needs to be changed and makes proper
                            // adjustments
                            plugInName = Preferences.getProperty(Preferences.PREF_PLUGIN_FILE + j);

                            if (plugInName == null) {
                                break;
                            }

                            Preferences.setProperty(Preferences.PREF_PLUGIN_FILE + (j - 1), plugInName);
                            Preferences.removeProperty(Preferences.PREF_PLUGIN_FILE + j);
                        }

                        Preferences.save();

                        break;
                    }
                }
            }

            if (!plugInFound) {

                for (i = 1; i < 100; i++) {
                    plugInName = Preferences.getProperty(Preferences.PREF_PLUGIN_VIEW + i);

                    if (plugInName == null) {
                        break;
                    } else if (selectedPluginName.equals(plugInName)) {

                        // check whether plugin with a higher number needs to be changed and makes proper adjustments
                        plugInFound = true;
                        Preferences.removeProperty(Preferences.PREF_PLUGIN_VIEW + i);

                        for (j = i + 1; j < 100; j++) {
                            plugInName = Preferences.getProperty(Preferences.PREF_PLUGIN_VIEW + j);

                            if (plugInName == null) {
                                break;
                            }

                            Preferences.setProperty(Preferences.PREF_PLUGIN_VIEW + (j - 1), plugInName);
                            Preferences.removeProperty(Preferences.PREF_PLUGIN_VIEW + j);
                        }

                        Preferences.save();

                        break;
                    }
                }
            }

            if (!plugInFound) {
                return; // does nothing if file was not found
            }

            // deletes file from MIPAV's class directory if user selected the appropriate checkbox
            if (destroyFileCheckbox.isSelected()) {
                FileDeleter fd = new FileDeleter(classPath + selectedPluginName + ".class");
                fd.start();
            }

            // updates menubar for each image
            Vector<Frame> imageFrames = ViewUserInterface.getReference().getImageFrameVector();

            if (imageFrames.size() < 1) {
                ViewUserInterface.getReference().buildMenu();
                ViewUserInterface.getReference().setControls();
            } else {

                for (i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
                }
            }

            dispose();
        } else if (source == cancelButton) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Sets up GUI dialog.
     */
    private void init() {
        setForeground(Color.black);
        addNotify();
        setTitle("Remove Plugin");

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Remove Plugin Paramaters"));

        int i;
        String plugInName;
        Vector<String> plugins = new Vector<String>();

        // obtains all 3 types of plugins from the Preferences File
        // and adds them to a vector
        for (i = 1; i < 100; i++) {
            plugInName = Preferences.getProperty(Preferences.PREF_PLUGIN_ALGORITHM + i);

            if (plugInName == null) {
                break;
            } else {
                plugins.addElement(plugInName);
            }
        }

        for (i = 1; i < 100; i++) {
            plugInName = Preferences.getProperty(Preferences.PREF_PLUGIN_FILE + i);

            if (plugInName == null) {
                break;
            } else {
                plugins.addElement(plugInName);
            }
        }

        for (i = 1; i < 100; i++) {
            plugInName = Preferences.getProperty(Preferences.PREF_PLUGIN_VIEW + i);

            if (plugInName == null) {
                break;
            } else {
                plugins.addElement(plugInName);
            }
        }

        list = new JList(plugins);

        JScrollPane sp = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                         JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        sp.setFont(serif12);
        sp.setPreferredSize(new Dimension(165, 95));
        mainPanel.add(sp);

        destroyFileCheckbox = new JCheckBox("Delete Plugin File Permanently", true);
        destroyFileCheckbox.setFont(serif12);
        destroyFileCheckbox.setForeground(Color.black);
        destroyFileCheckbox.addActionListener(this);
        mainPanel.add(destroyFileCheckbox, BorderLayout.SOUTH);

        buildOKButton();
        buildCancelButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel panel = new JPanel();
        panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        panel.add(mainPanel);

        getContentPane().add(panel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
    }

}
