package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import com.ice.tar.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.lang.reflect.Field;

import java.util.*;
import java.util.zip.*;

import javax.swing.*;


/**
 * Simple dialog to uninstall a plugin. The user selects which .class file to install using a file chooser. The file is
 * copied into MIPAV's class directory and the mipav.preferences file is updated accordingly. The menubars are also
 * updated.
 *
 * @author   senseneyj
 */

public class JDialogUninstallPlugin extends JDialogBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8736744495208652866L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton browseButton;

    /** DOCUMENT ME! */
    private Vector files = new Vector();

    /** DOCUMENT ME! */
    private String pluginDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "plugins" +
                               File.separator;

    /** DOCUMENT ME! */
    private JTextField textName;

    /** DOCUMENT ME! */
    private ViewUserInterface ui;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Does not create the uninstall dialog, instead performs the install algorithm as if 
     * only the <code>name</code> plugin had been selected, but confirmation has not occurred.
     */
    public JDialogUninstallPlugin(String name) {
    	System.out.println("Reached "+name);
    }
    
    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     */
    public JDialogUninstallPlugin(JFrame theParentFrame) {
        super(theParentFrame, false);
        ui = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------



    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
       
    }

    /**
     * Sets up GUI dialog.
     */
    private void init() {
        setForeground(Color.black);
        addNotify();
        setTitle("Install Plugin");

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Install Plugin Parameters"));

        JLabel labelType = new JLabel("Plugin Type");
        labelType.setForeground(Color.black);
        labelType.setFont(serif12);

        textName = new JTextField(15);
        textName.setText(".class, .jar, .zip, .tar, .tar.gz");
        textName.setFont(serif12);
        textName.setEnabled(false);

        browseButton = new JButton("Browse");
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        browseButton.addActionListener(this);

        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        mainPanel.add(browseButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(textName, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        JPanel panel = new JPanel();
        panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        panel.add(mainPanel);

        mainDialogPanel.add(panel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
    }
    
    /**
     * Called by either userInterface (this) or by another actionlistener (ViewJFrameImage) to build the plugins menu
     * bar.
     * 
     * @param al the listener that wants to know about actions on the plugins menu
     * 
     * @return the new plugin menu
     */
    /*public JTree buildPlugInsTree(ActionListener al) {
    	String userPlugins = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "plugins"
                + File.separator;

        JMenu menu = ViewMenuBuilder.buildMenu("Plugins", 'P', false);
        
        File pluginsDir = new File(userPlugins);
        if (pluginsDir.isDirectory()) {

            File[] allFiles = pluginsDir.listFiles(new FileFilter() {
                public boolean accept(File f) {

                    if (f.getPath().endsWith(".class")) {
                        return true;
                    }   
                    return false;
                }
            });

            String name, pluginName;
            Field catField;
            Class plugin;
            String fieldName = "CATEGORY";
            
            for (int i = 0; i < allFiles.length; i++) {
            	JMenu currentMenu = menu;
            	name = allFiles[i].getName();

                try {
                	name = name.substring(0, name.indexOf(".class"));
                	pluginName = name.substring(name.indexOf("PlugIn") + 6, name.length());
                } catch(Exception e) {
                	pluginName = name;
                }
                try {
                	plugin = Class.forName(name);
                	plugin.newInstance();   //instantiated to allow loading into SCRIPT_ACTION_LOCATIONS
                	catField = plugin.getField(fieldName);
                	String[] hier = (String[])catField.get(plugin);
                	Class[] interList = plugin.getInterfaces();
                	String interName = new String();
                	for(int j=0; j<interList.length; j++) {
                		if(interList[j].getName().contains("PlugIn")) {
                			interName = interList[j].getName().substring(interList[j].getName().indexOf("PlugIn"));
                		}
                	}
                	
                	for(int j=0; j<hier.length; j++) {
                		Component[] subComp = currentMenu.getMenuComponents();
                		boolean subExists = false;
                		for(int k=0; k<subComp.length; k++) {
                			if(subComp[k] instanceof JMenu && ((JMenu)subComp[k]).getText().equals(hier[j])) {
                				currentMenu = (JMenu) subComp[k];
                				subExists = true;
                				break;
                			}
                		}
                		if(!subExists) {
                			JMenu newMenu = ViewMenuBuilder.buildMenu(hier[j], 0, false);
                			currentMenu.add(newMenu);
                			currentMenu = newMenu;
                		}
                	}
                	if(!(al instanceof ViewUserInterface && interName.equals("PlugInAlgorithm"))) {
	                	currentMenu.add(ViewMenuBuilder.buildMenuItem(pluginName, 
	                			interName+pluginName, 0, al, null, false));	
                	}
                
                } catch(Exception e) {
                	//usually this means other files/folders exist in the installed plugins directory besides plugin files
                }
            }
        }
        
        if(menu.getItemCount() > 0) {
        	menu.addSeparator();
        }
        
        deleteMenu(menu);
        
        menu.add(ViewMenuBuilder.buildMenuItem("Install plugin", "InstallPlugin", 0, al, null, false));
        
        menu.add(ViewMenuBuilder.buildMenuItem("Uninstall plugin", "UninstallPlugin", 0, al, null, false));
        return menu;
    }*/
}
