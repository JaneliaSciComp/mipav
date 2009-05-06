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
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;


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

	private static final String UNINSTALL = "Uninstall plugin(s)";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Vector files = new Vector();

    /** The default location of MIPAV's plugin directory */
    private String pluginDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "plugins" +
                               File.separator;

    /** DOCUMENT ME! */
    private JTextField textName;
    
    /** The JTree that describes the plugin structure **/
    private JTree pluginTree;

    /** The main user interface */
    private ViewUserInterface ui;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Does not create the uninstall dialog, instead performs the install algorithm as if 
     * only the <code>name</code> plugin had been selected, but confirmation has not occurred.
     */
    public JDialogUninstallPlugin(String name) {
    	System.out.println("Reached "+name);
    	ui = ViewUserInterface.getReference();
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
       if(event.getSource().equals(OKButton)) {
    	   if(pluginTree.getSelectionModel().getSelectionPaths() != null && isPluginSelected()) {
    		   if(JOptionPane.showConfirmDialog(mainDialogPanel, "Do you want to uninstall the selected plugin(s)?", "Uninstall plugin", JOptionPane.YES_NO_OPTION) == 
       		   	JOptionPane.YES_OPTION) {
    			   uninstallPlugins();
    			   ui.buildMenu();
    			   ui.setControls();
    			   pluginTree.setModel(new JTree(buildPluginsTree()).getModel());
    	    	   pluginTree.setRootVisible(false);
    	    	   pluginTree.setMinimumSize(new Dimension(450, 300));
    	    	   pluginTree.setPreferredSize(new Dimension(450, 300));
    	           for(int i=0; i<pluginTree.getRowCount(); i++) {
    	           		pluginTree.expandRow(i);
    	           }
    			   dispose();
    		   } else {
    			   dispose();
    		   }
    	   } else {
    		   dispose();
    	   }
       } else if(event.getSource().equals(cancelButton)) {
    	   dispose();
       } else if(event.getActionCommand().equals(UNINSTALL)) {
    	   if(!isPluginSelected()) {
    		   MipavUtil.displayInfo("No plugins are selected.");
    		   return;
    	   }
    	   uninstallPlugins();
    	   ui.buildMenu();
    	   ui.setControls();
    	   pluginTree.setModel(new JTree(buildPluginsTree()).getModel());
    	   pluginTree.setRootVisible(false);
    	   pluginTree.setMinimumSize(new Dimension(450, 300));
    	   pluginTree.setPreferredSize(new Dimension(450, 300));
           for(int i=0; i<pluginTree.getRowCount(); i++) {
           		pluginTree.expandRow(i);
           }
       }
    }

    /**
     * Sets up GUI dialog.
     */
    private void init() {
        setForeground(Color.black);
        addNotify();
        setTitle("Uninstall Plugin(s)");

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Select plugins to uninstall"));

        JLabel labelType = new JLabel("Plugin Type");
        labelType.setForeground(Color.black);
        labelType.setFont(serif12);

        TreeNode root = buildPluginsTree();
        
        pluginTree = new JTree(root);
        pluginTree.setRootVisible(false);
        for(int i=0; i<pluginTree.getRowCount(); i++) {
        	pluginTree.expandRow(i);
        }
        pluginTree.setMinimumSize(new Dimension(450, 300));
        pluginTree.setPreferredSize(new Dimension(450, 300));
        
        JScrollPane scroll = new JScrollPane(pluginTree);
        scroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        scroll.setMinimumSize(new Dimension(450, 300));
        //scroll.setPreferredSize(new Dimension(450, 300));
        mainPanel.add(scroll);

        JPanel buttonPanel = new JPanel();
        JButton uninstall = new JButton(UNINSTALL);
        uninstall.setActionCommand(UNINSTALL);
        uninstall.addActionListener(this);
        uninstall.setMinimumSize(MipavUtil.defaultButtonSize);
        uninstall.setPreferredSize(new Dimension(180, 30));
        uninstall.setFont(serif12B);
        buttonPanel.add(uninstall);
        buildOKButton();
        OKButton.setText("Done");
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        JPanel panel = new JPanel();
        panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        panel.add(mainPanel);

        mainDialogPanel.add(panel, BorderLayout.CENTER);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
    }
    
    /**
     * Starts process to build exact copy of installed plugin structure as tree nodes.
     */
    private MutableTreeNode buildPluginsTree() {
    	JMenu plugin = (JMenu)ui.getMenuBuilder().getMenuItem("Plugins");
    	DefaultMutableTreeNode root = new DefaultMutableTreeNode("Plugins");
    	for(int i=0; i<plugin.getItemCount(); i++) {
    		if(plugin.getItem(i) instanceof JMenu) {
    			root.add(createBranch((JMenu) plugin.getItem(i)));
    		} else if(plugin.getItem(i) != null && !(plugin.getItem(i).equals(ui.getMenuBuilder().getMenuItem("Install plugin")) 
    							|| plugin.getItem(i).equals(ui.getMenuBuilder().getMenuItem("Uninstall plugin")))) {
    			root.add(new DefaultMutableTreeNode(plugin.getItem(i).getName()));
    		}
    	}
    	return root;
    }
    
    /**
     * Helper method for creating sub-menus of the plugin.
     */
    private MutableTreeNode createBranch(JMenu menu) {
    	DefaultMutableTreeNode root = new DefaultMutableTreeNode(menu.getText());
    	for(int i=0; i<menu.getItemCount(); i++) {
    		if(menu.getItem(i) instanceof JMenu) {
    			root.add(createBranch((JMenu) menu.getItem(i)));
    		} else if(menu.getItem(i) != null) {
    			root.add(new DefaultMutableTreeNode(menu.getItem(i).getName()));
    		}
    	}
    	return root;
    }
    
    /**
     * Whether plugins are selected in the JTree, equivalent to whether a leaf is selected in the JTree
     */
    
    private boolean isPluginSelected() {
    	return getSelectedPlugins().length > 0;
    }
    
    private void uninstallPlugins() {
    	TreeNode[] selectedPlugins = getSelectedPlugins();
    	String[] deleteStatus = new String[selectedPlugins.length];
    	int numYes = 0, numNo = 0;
    	for(int i=0; i<selectedPlugins.length; i++) {
    		if(deletePluginDependents(selectedPlugins[i].toString()) && 
    				deletePluginFile(selectedPlugins[i].toString())) {
    			deleteStatus[i] = selectedPlugins[i].toString()+" was successfully deleted.";
    			numYes++;
    		} else {
    			deleteStatus[i] = selectedPlugins[i].toString()+" could not be deleted.";
    			numNo++;
    		}
    	}
    	String message = "Plugin uninstall results:\t  ("+numYes+" deleted, "+numNo+" failed)\n";
    	for(int i=0; i<deleteStatus.length; i++) {
    		message += deleteStatus[i]+"\n";
    	}
    	MipavUtil.displayInfo(message);
    }
    
    private boolean deletePluginDependents(String name) {
    	String pluginName = "PlugIn"+name;
    	try {
			Class plugin = Class.forName(pluginName);
			//work on dependents, possibly using/generating manifest
			return true;
		} catch (ClassNotFoundException e) {
			return false;
		}
    }
    
    private boolean deletePluginFile(String name) {
    	String pluginName = "PlugIn"+name;
		File f = new File(pluginDir + pluginName+".class");
		if(f.exists()) {
			return f.delete();
		} 	
		return false;
    }
    
    private TreeNode[] getSelectedPlugins() {
    	ArrayList<TreeNode> selectedList = new ArrayList<TreeNode>();
    	TreePath[] selectedPaths = pluginTree.getSelectionModel().getSelectionPaths();
    	if(selectedPaths == null)
    		return new TreeNode[0];
    	for(int i=0; i<selectedPaths.length; i++) {
    		if(selectedPaths[i].getLastPathComponent() instanceof TreeNode) {
    			if(((TreeNode)selectedPaths[i].getLastPathComponent()).isLeaf()) {
    				selectedList.add((TreeNode)selectedPaths[i].getLastPathComponent());
    			}
    		}
    	}  
    	TreeNode[] selectedArr = new TreeNode[selectedList.size()];
    	for(int i=0; i<selectedArr.length; i++) {
    		selectedArr[i] = selectedList.get(i);
    	}
    	return selectedArr;
    }
}
