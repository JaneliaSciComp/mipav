package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.plugins.ManifestFile;
import gov.nih.mipav.view.*;

import com.ice.tar.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;

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
	
	private static final String INSTALL = "Install plugin(s)";
	
	private static final String MANIFEST = "Manifest" + File.separator + "mf_plugin.mf";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Vector<File> files = new Vector<File>();

    /** The default location of MIPAV's plugin directory */
    private String pluginDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "plugins" +
                               File.separator;

    /** File list for installing plugins */
    private JTextField textName;
    
    /** The JTree that describes the plugin structure **/
    private JTree pluginTree;

    /** The main user interface */
    private ViewUserInterface ui;

    /**Browse button for installing plugins**/
	private JButton browseButton;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Does not create the uninstall dialog, instead performs the install algorithm as if 
     * only the <code>name</code> plugin had been selected, but confirmation has not occurred.
     */
    public JDialogUninstallPlugin(String name) {
    	System.out.println("Reached "+name);
    	ui = ViewUserInterface.getReference();
    	
    	if(!manifestExists()) {
    		createManifest();
    	}
    }

	/**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     */
    public JDialogUninstallPlugin(JFrame theParentFrame) {
        super(theParentFrame, false);
        ui = ViewUserInterface.getReference();
        if(!manifestExists()) {
    		createManifest();
    	}
        
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
    	if(event.getSource().equals(cancelButton)) {
    	   dispose();
       } else if(event.getActionCommand().equals(UNINSTALL)) {
    	   if(!isPluginSelected()) {
    		   MipavUtil.displayInfo("No plugins are selected.");
    		   return;
    	   }
    	   uninstallPlugins();
    	   
    	   updateMenuBar();
    	   
    	   Vector<Frame> imageFrames = ui.getImageFrameVector();

    	   pluginTree.setModel(new JTree(buildPluginsTree()).getModel());
    	   pluginTree.setRootVisible(false);
    	   pluginTree.setMinimumSize(new Dimension(800, 300));
           for(int i=0; i<pluginTree.getRowCount(); i++) {
           		pluginTree.expandRow(i);
           }
       } 
    }

    private void updateMenuBar() {
    	// updates menubar for each image
        Vector imageFrames = ui.getImageFrameVector();

        if (imageFrames.size() < 1) {
            ui.buildMenu();
            ui.setControls();
        } else {

            for (int i = 0; i < imageFrames.size(); i++) {
                ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
            }
        }

        if(ui.getRegisteredImagesNum() > 0) {
 		   
 	   }
    }
    
    /**
     * Sets up GUI dialog.
     */
    private void init() {
        setForeground(Color.black);
        addNotify();
        setTitle("Uninstall Plugin(s)");

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.setForeground(Color.black);

        JLabel labelType = new JLabel("Plugin Type");
        labelType.setForeground(Color.black);
        labelType.setFont(serif12);

        TreeNode root = buildPluginsTree();
        
        pluginTree = new JTree(root);
        pluginTree.setRootVisible(false);
        for(int i=0; i<pluginTree.getRowCount(); i++) {
        	pluginTree.expandRow(i);
        }
        pluginTree.setMinimumSize(new Dimension(800, 300));
        
        JScrollPane scroll = new JScrollPane(pluginTree);
        scroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        scroll.setMinimumSize(new Dimension(450, 300));
        scroll.setPreferredSize(new Dimension(450, 300));
        
        JPanel subPanel = new JPanel();
        subPanel.add(scroll);
        subPanel.setBorder(buildTitledBorder("Select plugins to uninstall"));
        mainPanel.add(subPanel);

        JPanel buttonPanel = new JPanel();
        
        JButton uninstall = new JButton(UNINSTALL);
        uninstall.setActionCommand(UNINSTALL);
        uninstall.addActionListener(this);
        uninstall.setMinimumSize(MipavUtil.defaultButtonSize);
        uninstall.setPreferredSize(new Dimension(180, 30));
        uninstall.setFont(serif12B);
        
        buttonPanel.add(uninstall);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        cancelButton.setText("Exit");

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
    	JMenu plugin = buildPlugInsMenu(this);
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
    			DefaultMutableTreeNode d = null;
    			root.add(d = new DefaultMutableTreeNode(menu.getItem(i).getName()));
    			
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
			ManifestFile mf = ManifestFile.getReference();
			ArrayList<Class> allDep = mf.removeEntry(plugin);
			for(int i=0; i<allDep.size(); i++) {
				if(isInPluginFolder(allDep.get(i))) {
					File f = new File(pluginDir+allDep.get(i).getName()+".class");
					boolean delete = f.delete();
					System.out.println(allDep.get(i)+" deleted "+delete);
				}
			}
		} catch (ClassNotFoundException e) {
			return false;
		}
		return true;
    }
    
    private boolean helpPluginSearch(File f, String className) {
    	boolean found = false;
    	File plugin = new File(pluginDir);
    	File[] fList = plugin.listFiles();
    	String fileName;
    	for(int i=0; i<fList.length; i++) {
    		if(fList[i].isDirectory()) {
    			found = helpPluginSearch(fList[i], className);
    		} else if(fList[i].getName().contains(".class")) {
    			fileName = fList[i].getName().substring(0, fList[i].getName().indexOf(".class"));
    			found = fileName.equals(className);
    		}
    		
    		if(found) {
    			return found; //true
    		}
    	}
    	return found; //false
    }
    
    /**
     * Determines whether the <code>className</code> is in the plugin folder.
     * 
     * @param className
     * @return
     */
    private boolean isInPluginFolder(String className) {
    	boolean found = false;
    	File plugin = new File(pluginDir);
    	File[] fList = plugin.listFiles();
    	String fileName;
    	for(int i=0; i<fList.length; i++) {
    		if(fList[i].isDirectory()) {
    			found = helpPluginSearch(fList[i], className);
    		} else if(fList[i].getName().contains(".class")) {
    			fileName = fList[i].getName().substring(0, fList[i].getName().indexOf(".class"));
    			found = fileName.equals(className);
    		}
    		
    		if(found) {
    			return found; //true
    		}
    	}
    	return found; //false
    }
    
    /**
     * Determines whether <code>c</code> is in the current plugin folder.
     * 
     * @param c
     * @return
     */
    private boolean isInPluginFolder(Class c) {
    	boolean found = isInPluginFolder(c.getSimpleName());
    	
    	if(found) {
			return found; //true
		}
    	
    	File plugin = new File(pluginDir);
    	URL fileLoc = null;
    	try {
    		fileLoc = c.getProtectionDomain().getCodeSource().getLocation();
    	} catch (NullPointerException e) {
    		return false;
    	}
    		
    	try {
			if(fileLoc.toString().contains(plugin.toURI().toURL().toString())) {
				return true;
			} else {
				return false;
			}
		} catch (MalformedURLException e) {
			//pluginDir needs to specify a valid location
			e.printStackTrace();
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
    
    /**
     * A custom build PluginsMenu to allow for PluginAlgorithms to be shown
     * even when no image is displayed
     * 
     * @param al the listener that wants to know about actions on the plugins menu
     * 
     * @return the new plugin menu
     */
    private JMenu buildPlugInsMenu(ActionListener al) {
    	
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

                	JMenuItem pluginMenuItem = ViewMenuBuilder.buildMenuItem(pluginName, 
                			interName+pluginName, 0, al, null, false);
                	pluginMenuItem.setName(pluginName);
                	pluginMenuItem.addMouseListener(ViewJPopupPlugin.getReference());
            		currentMenu.add(pluginMenuItem);	
            		addManifestEntry(plugin);
            			
                
                } catch(Exception e) {
                	//usually this means other files/folders exist in the installed plugins directory besides plugin files
                }
            }
        }
        
        if(menu.getItemCount() > 0) {
        	menu.addSeparator();
        }
        
        deleteMenu(menu);
        
        return menu;
    }
    
    private void addManifestEntry(Class plugin) {
		// TODO Auto-generated method stub
		
	}

	/**
     * Recursive deletion algorithm to delete JMenus which contain no JMenuItems exclusive of JMenus in any children.
     * 
     * @param menu The menu to run through deletion
     */
    private void deleteMenu(JMenu menu) {
    	for(int i=0; i<menu.getItemCount(); i++) {
        	if(menu.getItem(i) instanceof JMenu) {
        		deleteMenu(((JMenu)menu.getItem(i))); 	
        		if(((JMenu)menu.getItem(i)).getItemCount() == 0) {
        			menu.remove(i);
        		}		
        	}
        }
    }
    
    /**
     * Creates the manifest file upon dialog startup.
     */
    private void createManifest() {
		File f = new File(pluginDir + MANIFEST);
		try {
			f.createNewFile();
		} catch (IOException e) {
			MipavUtil.displayInfo("Could not create manifest file.  Plugin dependents will not be stored.");
			e.printStackTrace();
		}
	}

    /**
     * Checks for manifest file existance.
     * @return
     */
	private boolean manifestExists() {
		return new File(pluginDir + MANIFEST).exists();
	}
    
    /**
     * DOCUMENT ME!
     *
     * @param   tarFileName  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public static InputStream getInputStream(String tarFileName) throws IOException {

        if (tarFileName.substring(tarFileName.lastIndexOf(".") + 1, tarFileName.lastIndexOf(".") + 3).equalsIgnoreCase("gz")) {
            Preferences.debug("Creating an GZIPInputStream for the file\n", Preferences.DEBUG_MINOR);

            return new GZIPInputStream(new FileInputStream(new File(tarFileName)));
        } else {
            Preferences.debug("Creating an InputStream for the file\n", Preferences.DEBUG_MINOR);

            return new FileInputStream(new File(tarFileName));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   in        DOCUMENT ME!
     * @param   untarDir  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public static void readTar(InputStream in, String untarDir) throws IOException {
        Preferences.debug("Reading TarInputStream... (using classes from http://www.trustice.com/java/tar/)\n",
                          Preferences.DEBUG_MINOR);

        TarInputStream tin = new TarInputStream(in);
        TarEntry tarEntry = tin.getNextEntry();

        if (new File(untarDir).exists()) {

            while (tarEntry != null) {
                File destPath = new File(untarDir + File.separatorChar + tarEntry.getName());
                Preferences.debug("Processing " + destPath.getAbsoluteFile() + "\n", Preferences.DEBUG_MINOR);

                if (!tarEntry.isDirectory()) {
                    FileOutputStream fout = new FileOutputStream(destPath);
                    tin.copyEntryContents(fout);
                    fout.close();
                } else {
                    destPath.mkdir();
                }

                tarEntry = tin.getNextEntry();
            }

            tin.close();
        } else {
            Preferences.debug("That destination directory doesn't exist! " + untarDir + "\n", Preferences.DEBUG_MINOR);
        }
    }
}
