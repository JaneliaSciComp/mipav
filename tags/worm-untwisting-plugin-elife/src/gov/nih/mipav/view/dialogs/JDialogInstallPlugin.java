package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.plugins.ManifestFile;
import gov.nih.mipav.plugins.PlugIn;
import gov.nih.mipav.plugins.PlugInAlgorithm;
import gov.nih.mipav.plugins.PlugInFile;
import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.plugins.PlugInView;
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
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.filechooser.FileSystemView;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;


/**
 * Simple dialog to install a plugin. The user selects which .class file to install using a file chooser. The file is
 * copied into MIPAV's class directory and the mipav.preferences file is updated accordingly. The menubars are also
 * updated.
 * 
 * @version 1.0 July 19, 2000
 * @author Harman Singh
 * @author senseneyj
 */
public class JDialogInstallPlugin extends JDialogBase implements ActionListener {
    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8736744495208652866L;

    /** File system view. */
    private static FileSystemView fileSystem = FileSystemView.getFileSystemView();

    private static String initTreeLoc = System.getProperty("user.home");

    /** The class, zip, jar etc files that were selected before they were unzipped */
    private Vector<File> files = new Vector<File>();

    /** The results of working with the <code>files</code> in a temporary class environment */
    private Vector<Color> filesColor = new Vector<Color>();

    /** The default storage location of plugins */
    private String pluginDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "plugins"
            + File.separator;

    /** The default user interface */
    private ViewUserInterface ui;

    /** The sub-gui * */
    private ClassSelectorPanel selectorPanel;

    /** Check box for whether to display files that are not likely sources of MIPAV plugins.* */
    private JCheckBox checkShow;
    
    /** Check box for unpacking container files */
    private JCheckBox checkUnpack;

    /**
     * Creates new dialog.
     * 
     * @param theParentFrame Parent frame
     */
    public JDialogInstallPlugin(JFrame theParentFrame) {
        super(theParentFrame, false);
        File f = new File(pluginDir);
        if ( !f.exists()) {
            f.mkdirs();
            MipavUtil
                    .displayError("The MIPAV plugin folder was just created, please restart MIPAV before installing plugins.");
        } else {
            ui = ViewUserInterface.getReference();
            init();
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     * 
     * @param tarFileName DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    public static InputStream getInputStream(String tarFileName) throws IOException {

        if (tarFileName.substring(tarFileName.lastIndexOf(".") + 1, tarFileName.lastIndexOf(".") + 3).equalsIgnoreCase(
                "gz")) {
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
     * @param in DOCUMENT ME!
     * @param untarDir DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    public static ArrayList<File> readTar(InputStream in, String untarDir) throws IOException {
        ArrayList<File> allRead = new ArrayList<File>();

        Preferences.debug("Reading TarInputStream... (using classes from http://www.trustice.com/java/tar/)\n",
                Preferences.DEBUG_MINOR);

        TarInputStream tin = new TarInputStream(in);
        TarEntry tarEntry = tin.getNextEntry();

        if (new File(untarDir).exists()) {

            while (tarEntry != null) {
                File destPath = new File(untarDir + File.separatorChar + tarEntry.getName());
                Preferences.debug("Processing " + destPath.getAbsoluteFile() + "\n", Preferences.DEBUG_MINOR);

                if ( !tarEntry.isDirectory()) {
                    FileOutputStream fout = new FileOutputStream(destPath);
                    tin.copyEntryContents(fout);
                    fout.close();
                    allRead.add(destPath);
                } else {
                    destPath.mkdir();
                }

                tarEntry = tin.getNextEntry();
            }

            tin.close();
        } else {
            Preferences.debug("That destination directory doesn't exist! " + untarDir + "\n", Preferences.DEBUG_MINOR);
        }

        return allRead;
    }

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     * 
     * @param event event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {
            JList selected = selectorPanel.getSelectedFiles();
            installPlugins();
        } else if (source == cancelButton) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    } 

    private void installPlugins() {
        ArrayList<File> files = moveFiles();
        ArrayList<String> installSimpleName = new ArrayList<String>();
        for (int i = 0; i < files.size(); i++) {
            String name = files.get(i).getName();
            if(name.contains(".class")) {
	            name = name.substring(0, name.indexOf(".class"));
	            try {
	                Class c = Class.forName(name);
	                boolean isPlugin = isPluginClass(c);
	
	                if (isPlugin) {
	                    Class[] dep = null;
	                    try {
	                        dep = gatherDependents(c);
	                    } catch (NoClassDefFoundError e) {
	                        removeFiles(files);
	                        MipavUtil.displayInfo("Unable to install plugin " + name + ". " + e.getLocalizedMessage()
	                                + " could not be found.");
	                        e.printStackTrace();
	                    } catch (Throwable e) {
	                        removeFiles(files);
	                        MipavUtil.displayInfo("Unable to install plugin " + name
	                                + ".  Make sure you have included all needed class files.");
	                        e.printStackTrace();
	                    }
	                    if (dep != null) {
	                        installSimpleName.add(name);
	                        ManifestFile mf = ManifestFile.getReference();
	                        mf.addEntry(c, dep); // if exists will only modi
	                    }
	                }
	            } catch (ClassNotFoundException e) {
	                System.out.println(name + " plugin file was not found.");
	                // name could not likely be resolved given the current classpath
	            } catch (NoClassDefFoundError e) {
	            	System.err.println(name + "plugin could not be loaded.");
	            }
            } else if(name.contains(".jar") && !checkUnpack.isSelected()) {
            	installSimpleName.add(name);
            }
        }

        // updates menubar for each image
        Vector<Frame> imageFrames = ui.getImageFrameVector();

        if (imageFrames.size() < 1) {
            ui.buildMenu();
            ui.setControls();
        } else {

            for (int i = 0; i < imageFrames.size(); i++) {
                ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
            }
        }

        String install = new String();
        if (installSimpleName.size() == 0) {
            install = "No plugins were installed, please select valid MIPAV plugin files.";
        } else {
            install = "<html>The following plugins were successfully installed <br>to " + pluginDir + ":<br>";
            for (int i = 0; i < installSimpleName.size(); i++) {
                install += installSimpleName.get(i) + "<br>";
            }
            install += "</html>";
            selectorPanel.clearList();
        }

        MipavUtil.displayInfo(install);

        return;
    }

    /**
     * Gets the dependents of a given class, defined as those classes which exist solely in the pluginDir path and
     * should be uninstalled.
     * 
     * @param c
     * @return
     */
    private Class[] gatherDependents(Class c) {
        ArrayList<Class> dep = new ArrayList<Class>();
        String simpleName = c.getName().substring("PlugIn".length());

        File[] allPluginFiles = new File(pluginDir).listFiles();
        for (int i = 0; i < allPluginFiles.length; i++) {
            if (allPluginFiles[i].getName().indexOf(".class") != -1) {
                String simpleFileName = allPluginFiles[i].getName().substring(0,
                        allPluginFiles[i].getName().indexOf(".class"));
                if ( !simpleFileName.equals("PlugIn" + simpleName) && simpleFileName.contains(simpleName)) {
                    try {
                        Class initDep = Class.forName(simpleFileName);
                        if ( !dep.contains(initDep)) {
                            dep.add(initDep);
                        }

                        ArrayList<Class> subDep = gatherSubClassDependents(initDep, 0);
                        for (int k = 0; k < subDep.size(); k++) {
                            if ( !dep.contains(subDep.get(k))) {
                                dep.add(subDep.get(k));
                            }
                        }
                    } catch (ClassNotFoundException e) {
                        // name could not be resolved given current classpath
                    }
                }
            }
        }

        Class[] possibleDep = c.getDeclaredClasses();
        for (int j = 0; j < possibleDep.length; j++) {
            if (isInPluginFolder(possibleDep[j])) {
                if ( !dep.contains(possibleDep[j])) {
                    dep.add(possibleDep[j]);
                }

                ArrayList<Class> subDep = gatherSubClassDependents(possibleDep[j], 0);
                for (int k = 0; k < subDep.size(); k++) {
                    if ( !dep.contains(subDep.get(k))) {
                        dep.add(subDep.get(k));
                    }
                }
            }
        }

        Field[] f = c.getDeclaredFields();
        for (int i = 0; i < f.length; i++) {
            if (isInPluginFolder(f[i].getType())) {
                if ( !dep.contains(f[i].getType())) {
                    dep.add(f[i].getType());
                }

                ArrayList<Class> subDep = gatherSubClassDependents(f[i].getType(), 0);
                for (int k = 0; k < subDep.size(); k++) {
                    if ( !dep.contains(subDep.get(k))) {
                        dep.add(subDep.get(k));
                    }
                }
            }
        }

        Class[] depAr = new Class[dep.size()];
        for (int i = 0; i < dep.size(); i++) {
            depAr[i] = dep.get(i);
        }
        return depAr;
    }

    /**
     * Attempts to go no further than third level.
     * 
     * @param c
     * @param level
     * @return
     */
    private ArrayList<Class> gatherSubClassDependents(Class c, int level) {
        ArrayList<Class> dep = new ArrayList<Class>();

        if (level > 3) {
            return dep;
        }

        Class[] possibleDep = c.getDeclaredClasses();
        for (int j = 0; j < possibleDep.length; j++) {
            if (isInPluginFolder(possibleDep[j])) {
                if ( !dep.contains(possibleDep[j])) {
                    dep.add(possibleDep[j]);
                }

                ArrayList<Class> subDep = gatherSubClassDependents(possibleDep[j], ++level);
                for (int k = 0; k < subDep.size(); k++) {
                    if ( !dep.contains(subDep.get(k))) {
                        dep.add(subDep.get(k));
                    }
                }
            }
        }

        Field[] f = c.getDeclaredFields();
        for (int i = 0; i < f.length; i++) {
            if (isInPluginFolder(f[i].getType())) {
                if ( !dep.contains(f[i].getType())) {
                    dep.add(f[i].getType());
                }

                ArrayList<Class> subDep = gatherSubClassDependents(f[i].getType(), ++level);
                for (int k = 0; k < subDep.size(); k++) {
                    if ( !dep.contains(subDep.get(k))) {
                        dep.add(subDep.get(k));
                    }
                }
            }
        }

        return dep;
    }

    /**
     * Sets up GUI dialog.
     */
    private void init() {
        GridBagConstraints gbc = new GridBagConstraints();

        setForeground(Color.black);
        addNotify();
        setTitle("Install Plugin");

        JPanel mainPanel = new JPanel(new GridBagLayout());

        JLabel intro = new JLabel(
                "<html><center>This interface allows for batch installation of plugins into MIPAV.  <br>"
                        + "Either MIPAV or ImageJ plugins may be installed. <br>"
                        + "You may select Java class files or container files.  "
                        + "These include *.class, *.tar.gz, *.zip, *.jar files.<br></center></html>");
        /*
         * "Detected plugins that will likely install correctly are displayed in <font color=\"blue\">blue</font>.<br>"+
         * "Plugins and any problem components listed are displayed in <font color=\"red\">red</font>.</center></html>
         * ");
         */
        intro.setBorder(new EmptyBorder(10, 75, 0, 75));

        gbc.fill = GridBagConstraints.BOTH;
        gbc.weighty = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        mainPanel.add(intro, gbc);

        selectorPanel = new ClassSelectorPanel();
        selectorPanel.setVisible(true);

        gbc.gridy = 1;
        gbc.weighty = 1;

        mainPanel.add(selectorPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        OKButton.setText("Install Plugin(s)");
        OKButton.setPreferredSize(new Dimension(160, 30));
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        cancelButton.setText("Close");

        gbc.gridy = 2;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weighty = 0;
        mainPanel.add(buttonPanel, gbc);

        getContentPane().add(mainPanel);

        pack();

        setMinimumSize(this.getSize());
    }

    private class ClassSelectorPanel extends JPanel implements ActionListener {

        private static final String BROWSE = "Browse";

        private static final String MOVE_RIGHT = "Move Right";

        private static final String DELETE = "Delete";

        private static final String CHECK = "Check";

        private JTree fileTree;

        private JFileTreePanel subFilePanel;

        private JTextField initDir;

        private JList selected;

        /** Currently used file */
        private File selectedFile = new File(System.getProperty("user.home"));

        public ClassSelectorPanel() {
            if (Preferences.getPluginInstallDirectory() != null) {
                initTreeLoc = Preferences.getPluginInstallDirectory();
                selectedFile = new File(initTreeLoc);
                if ( !selectedFile.exists()) {
                    initTreeLoc = System.getProperty("user.home");
                    selectedFile = new File(initTreeLoc);
                }
            }
            GridBagConstraints gbc = new GridBagConstraints();
            setLayout(new GridBagLayout());

            JPanel dirSelectPanel = new JPanel();

            dirSelectPanel.setBorder(MipavUtil.buildTitledBorder("Select a plugin directory"));

            JButton browseButton = new JButton(BROWSE);
            browseButton.addActionListener(this);
            browseButton.setActionCommand(BROWSE);
            dirSelectPanel.add(browseButton);

            JLabel dirLabel = new JLabel("Current directory: ");
            dirLabel.setBorder(new EmptyBorder(0, 20, 0, 5));
            dirSelectPanel.add(dirLabel);

            initDir = new JTextField(initTreeLoc);
            initDir.setColumns(45);
            initDir.setFont(MipavUtil.font12);
            dirSelectPanel.add(initDir);

            gbc.fill = GridBagConstraints.BOTH;
            gbc.weighty = 0;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.weightx = 1;

            add(dirSelectPanel, gbc);

            GridBagConstraints gbc2 = new GridBagConstraints();
            JPanel mainSelectorPanel = new JPanel(new GridBagLayout());

            subFilePanel = new JFileTreePanel();
            subFilePanel.setBorder(MipavUtil.buildTitledBorder("Select class files"));
            subFilePanel.setFont(MipavUtil.font12);
            fileTree = subFilePanel.getFileTree();

            gbc2.fill = GridBagConstraints.BOTH;
            gbc2.gridheight = 1;
            gbc2.gridwidth = 1;
            gbc2.weighty = 1;
            gbc2.anchor = GridBagConstraints.CENTER;
            gbc2.gridx = 0;
            gbc2.gridy = 0;
            gbc2.weightx = .5;

            mainSelectorPanel.add(subFilePanel, gbc2);

            JPanel selectOptionsPanel = buildSelectOptionsPanel();

            gbc2.weighty = 1;
            gbc2.anchor = GridBagConstraints.SOUTH;
            gbc2.gridx = 1;
            gbc2.gridy = 0;
            gbc2.weightx = 0;

            mainSelectorPanel.add(selectOptionsPanel, gbc2);

            JPanel fileListPanel = new JPanel(new BorderLayout());
            fileListPanel.setBorder(MipavUtil.buildTitledBorder("Selected class files"));
            selected = new JList();
            selected.setModel(new DefaultListModel());
            selected.setCellRenderer(new FileCellRenderer());
            selected.setFont(MipavUtil.font12);
            JScrollPane scrollPane = new JScrollPane(selected);
            scrollPane.setFont(MipavUtil.font12);
            scrollPane.setPreferredSize(new Dimension(288, 317));
            scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
            fileListPanel.setPreferredSize(new Dimension(300, 350));
            fileListPanel.add(scrollPane, BorderLayout.CENTER);
            fileListPanel.setFont(MipavUtil.font12);

            gbc2.weighty = 1;
            gbc2.anchor = GridBagConstraints.CENTER;
            gbc2.gridx = 2;
            gbc2.gridy = 0;
            gbc2.weightx = .5;

            mainSelectorPanel.add(fileListPanel, gbc2);

            gbc.fill = GridBagConstraints.BOTH;
            gbc.weighty = .9;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.weightx = 1;

            add(mainSelectorPanel, gbc);

            JPanel checkPanel = new JPanel();
            checkPanel.setLayout(new BorderLayout());
            checkShow = new JCheckBox("Show only *.class, *.tar.gz, *.zip, and *.jar files.");
            checkShow.setSelected(true);
            checkShow.addActionListener(this);
            checkShow.setActionCommand(CHECK);
            checkShow.setBorder(new EmptyBorder(3, 10, 0, 10));
            checkPanel.add(checkShow, BorderLayout.WEST);

            gbc.fill = GridBagConstraints.BOTH;
            gbc.weighty = 0;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.gridx = 0;
            gbc.gridy = 2;
            gbc.weightx = 1;
            
            add(checkPanel, gbc);
            
            JPanel checkUnpackPanel = new JPanel();
            checkUnpackPanel.setLayout(new BorderLayout());
            checkUnpack = new JCheckBox("Unpack *.jar files.");
            checkUnpack.setSelected(true);
            checkUnpack.setBorder(new EmptyBorder(3, 10, 0, 10));
            checkUnpackPanel.add(checkUnpack, BorderLayout.WEST);

            gbc.fill = GridBagConstraints.BOTH;
            gbc.weighty = 0;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.gridx = 0;
            gbc.gridy++;
            gbc.weightx = 1;

            add(checkUnpackPanel, gbc);

            validate();
            pack();
        }

        public JList getSelectedFiles() {
            return selected;
        }

        public void clearList() {
            selected.setModel(new DefaultListModel());
            files = new Vector<File>();
            filesColor = new Vector<Color>();
        }

        private JPanel buildSelectOptionsPanel() {
            JPanel selectOptionsPanel = new JPanel(new GridBagLayout());
            GridBagConstraints gbc = new GridBagConstraints();

            JButton moveRight = new JButton();
            moveRight.addActionListener(this);
            moveRight.setActionCommand(MOVE_RIGHT);
            moveRight.setIcon(MipavUtil.getIcon("rightarrow.gif"));

            gbc.anchor = GridBagConstraints.CENTER;
            gbc.gridx = 0;
            gbc.gridy = 0;

            selectOptionsPanel.add(moveRight, gbc);

            JButton delete = new JButton();
            delete.addActionListener(this);
            delete.setActionCommand(DELETE);
            delete.setIcon(MipavUtil.getIcon("delete.gif"));

            gbc.gridy = 1;
            selectOptionsPanel.add(delete, gbc);

            return selectOptionsPanel;
        }

        public void actionPerformed(ActionEvent e) {
            if (e.getActionCommand().equals(BROWSE)) {
                JFileChooser chooser = new JFileChooser();

                if (Preferences.getPluginInstallDirectory() != null) {
                    chooser.setCurrentDirectory(new File(Preferences.getPluginInstallDirectory()));
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
                chooser.setDialogTitle("Select a plugin directory");
                chooser.setMultiSelectionEnabled(false);
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

                if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
                    selectedFile = chooser.getSelectedFile();

                    fileTree = subFilePanel.setRootDir(selectedFile);
                    Preferences.setPluginInstallDirectory(selectedFile);

                    initDir.setText(selectedFile.toString());
                }
            } else if (e.getActionCommand().equals(MOVE_RIGHT)) {
                TreePath[] paths = fileTree.getSelectionModel().getSelectionPaths();
                if (paths == null) {
                    return;
                }

                ArrayList<String> conflict = new ArrayList<String>();
                for (int i = 0; i < paths.length; i++) {
                    String name = "";

                    if ( ((JFileTreeNode) paths[i].getLastPathComponent()).getFile().toString().equals(
                            initDir.getText())) {
                        continue; // shouldn't be able to select root
                    }

                    int inc = 1;
                    if (initDir.getText().charAt(initDir.getText().length() - 1) == File.separatorChar) {
                        inc = 0;
                    }
                    name = ((JFileTreeNode) paths[i].getLastPathComponent()).getFile().toString().substring(
                            initDir.getText().length() + inc);

                    if ( ! ((DefaultListModel) selected.getModel()).contains(name)) {
                        if (isInPluginFolder(name)) {
                            conflict.add(name);
                        }
                        // even though conflicting, still added to potential list
                        ((DefaultListModel) selected.getModel()).addElement(name);
                        files.add( ((JFileTreeNode) paths[i].getLastPathComponent()).getFile());
                    }
                }

                if (conflict.size() > 0) {
                    String conflictList = new String();
                    for (int i = 0; i < conflict.size(); i++) {
                        conflictList += conflict.get(i) + "<br>";
                    }

                    MipavUtil.displayInfo("<html>The following files are already in the plugin directory: <br>"
                            + conflictList + "</html>");
                }

                // TODO: color tables were built here to find missing dependencies
                // ArrayList<File> allFiles = moveFiles();
                // filesColor = buildColorTable(files);
                // removeFiles(allFiles);
            } else if (e.getActionCommand().equals(DELETE)) {
                Object[] numSelected = selected.getSelectedValues();
                int[] selectedIndex = selected.getSelectedIndices();
                for (int i = 0; i < numSelected.length; i++) {
                    ((DefaultListModel) selected.getModel()).removeElement(numSelected[i]);
                    files.remove(selectedIndex[0]);
                    selectedIndex = selected.getSelectedIndices();
                }

                for (int i = 0; i < files.size(); i++) {
                    System.out.println(i + ": " + files.get(i));
                }
                JList temp = new JList();
                DefaultListModel m;
                temp.setModel(m = new DefaultListModel());
                // TODO: Future DefaultListModels are expected to be collections so addAll should be implemented.
                for (int i = 0; i < files.size(); i++) {
                    m.add(i, files.get(i));
                }

                // TODO: color tables were built here to find missing dependencies
                // filesColor = buildColorTable(files);
            } else if (e.getActionCommand().equals(CHECK)) {
                // rebuilds index, renderer is already aware of the check button's status
                fileTree = subFilePanel.setRootDir(selectedFile);
                // rootNameCache = new HashMap<File, String>();
            }
        }
    }

    private ArrayList<File> moveFiles() {
        ArrayList<File> allFiles = new ArrayList<File>();

        int i;

        if (files.size() == 0) {
            MipavUtil.displayError("Please select PlugIn file(s)");

            return null;
        }

        // make the plugins directory if it does not exist
        if ( !new File(pluginDir).isDirectory()) {
            new File(pluginDir).mkdirs();
            MipavUtil
                    .displayError("The MIPAV plugin directory was just created, please restart MIPAV before installing plugins.");
        }

        FileOutputStream fw = null; // for outputting copied file
        FileInputStream fr = null; // for inputting the source plugin file
        BufferedInputStream br = null; // buffers are used to speed up the process
        BufferedOutputStream bw = null;
        ZipEntry entry = null;
        ZipInputStream zIn = null;

        byte[] buf = null;
        int len;

        for (i = 0; i < files.size(); i++) {
            File currentFile = (File) files.elementAt(i);

            if (currentFile.getName().endsWith(".class")  || (currentFile.getName().endsWith(".jar") && !checkUnpack.isSelected())) {
                int readStatus = -1;
                byte[] byteBuff = null;
                int fileLength = 0;

                try {
                    fr = new FileInputStream(currentFile); // sets the fileinput to the directory chosen from the
                    // browse option
                    br = new BufferedInputStream(fr);

                    fileLength = (int) currentFile.length();

                    byteBuff = new byte[fileLength];

                    if (fileLength != 0) {
                        readStatus = br.read(byteBuff, 0, fileLength);
                    }

                } catch (FileNotFoundException fnfe) {
                    MipavUtil.displayError("InstallPlugin: " + fnfe);
                    dispose();

                    return null;
                } catch (IOException ioe) {
                    MipavUtil.displayError("Error reading plugin files.  Try manually copying .class files to "
                            + pluginDir);
                    dispose();

                    return null;
                } finally {

                    try {

                        if (br != null) {
                            br.close();
                        }
                    } catch (IOException ioe) {
                        ioe.printStackTrace();
                    }
                }

                if (readStatus != -1) {
                    try {
                        // TODO: Populate files here based on derived class name (found by searching clas file).
                        allFiles.add(new File(pluginDir + File.separatorChar + currentFile.getName()));
                        fw = new FileOutputStream(pluginDir + File.separatorChar + currentFile.getName()); // the
                                                                                                            // location
                                                                                                            // to be
                                                                                                            // copied is
                                                                                                            // MIPAV's
                                                                                                            // class
                                                                                                            // path
                        bw = new BufferedOutputStream(fw);
                        bw.write(byteBuff, 0, fileLength);
                    } catch (IOException ioe) {
                        MipavUtil.displayError("Error writing plugin files.  Try manually copying .class files to "
                                + pluginDir);
                        dispose();

                        return null;
                    }

                    if (bw != null) {
                        try {
                            bw.close();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
            // must be a .jar or .zip so extract files
            else if (currentFile.getName().endsWith(".zip") || (currentFile.getName().endsWith(".jar") && checkUnpack.isSelected())) {

                try {
                    zIn = new ZipInputStream(new FileInputStream(currentFile));
                    entry = null;

                    // if the entry is a directory of is a class file, extract it
                    while ( (entry = zIn.getNextEntry()) != null) {

                        if (entry.isDirectory()) {

                            String dirname = pluginDir + File.separator
                                    + entry.getName().substring(0, entry.getName().length() - 1);
                            new File(dirname).mkdir();
                        } else {

                            File f = null;
                            try {
                                (f = new File(pluginDir + File.separator + entry.getName())).getParentFile().mkdirs();
                                if (f.getName().contains(".class")) {
                                    allFiles.add(f);
                                }
                            } catch (Exception ex) {
                                // do nothing...no parent dir here
                            }

                            try {
                                fw = new FileOutputStream(pluginDir + File.separator + entry.getName());
                            } catch (FileNotFoundException fe) {
                                System.err.println("Warning: could not create the file " + pluginDir + File.separator
                                        + entry.getName());
                            }

                            try {
                                // Transfer bytes from the ZIP file to the output file
                                buf = new byte[4096];

                                while ( (len = zIn.read(buf)) > 0) {
                                    fw.write(buf, 0, len);
                                }

                                fw.close();
                            } catch (IOException io) {
                                // likely that a file not found exception was thrown previously
                            }
                        }
                    }

                } catch (Exception e) {
                    e.printStackTrace();
                } finally {

                    try {

                        if (zIn != null) {
                            zIn.close();
                        }

                        if (fw != null) {
                            fw.close();
                        }
                    } catch (IOException ioe) {
                        ioe.printStackTrace();
                    }
                }

            } else if (currentFile.getName().endsWith(".tar") || currentFile.getName().endsWith(".tar.gz")) {

                try {
                    ArrayList<File> tarFiles = readTar(getInputStream(currentFile.getPath()), pluginDir);
                    for (int j = 0; j < tarFiles.size(); j++) {
                        if (tarFiles.get(j).getName().contains(".class")) {
                            allFiles.add(tarFiles.get(j));
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

        try {

            if (zIn != null) {
                zIn.close();
            }

            if (fw != null) {
                fw.close();
            }

            if (bw != null) {
                bw.close();
            }

            if (br != null) {
                br.close();
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }

        return allFiles;
    }

    private void removeFiles(ArrayList<File> allFiles) {
        for (int i = 0; i < allFiles.size(); i++) {
            allFiles.get(i).delete();
        }

        removeEmptyDirs();
    }

    private void removeEmptyDirs() {
        return;
    }

    private boolean helpPluginSearch(File f, String className) {
        boolean found = false;
        File[] fList = f.listFiles();
        String fileName;
        for (int i = 0; i < fList.length; i++) {
            if (fList[i].isDirectory()) {
                found = helpPluginSearch(fList[i], className);
            } else if (fList[i].getName().contains(".class")) {
                fileName = fList[i].getName().substring(0, fList[i].getName().indexOf(".class"));
                found = fileName.equals(className);
            }

            if (found) {
                return found; // true
            }
        }
        return found; // false
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
        for (int i = 0; i < fList.length; i++) {
            if (fList[i].isDirectory()) {
                found = helpPluginSearch(fList[i], className);
            } else if (fList[i].getName().contains(".class")) {
                fileName = fList[i].getName().substring(0, fList[i].getName().indexOf(".class"));
                found = fileName.equals(className);
            }

            if (found) {
                return found; // true
            }
        }
        return found; // false
    }

    /**
     * Determines whether <code>c</code> is in the current plugin folder.
     * 
     * @param c
     * @return
     */
    private boolean isInPluginFolder(Class c) {
        boolean found = isInPluginFolder(c.getSimpleName());

        if (found) {
            return found; // true
        }

        File plugin = new File(pluginDir);
        URL fileLoc = null;
        try {
            fileLoc = c.getProtectionDomain().getCodeSource().getLocation();
        } catch (NullPointerException e) {
            return false;
        }

        try {
            if (fileLoc.toString().contains(plugin.toURI().toURL().toString())) {
                return true;
            } else {
                return false;
            }
        } catch (MalformedURLException e) {
            // pluginDir needs to specify a valid location
            e.printStackTrace();
            return false;
        }
    }

    /**
     * Returns whether the given class is a MIPAV/ImageJ plugin.
     * @param c A class.
     * @return True, if the given class is a MIPAV/ImageJ plugin.
     */
    public static final boolean isPluginClass(Class c) {
        return isMipavPluginClass(c) || isImageJPluginClass(c);
    }
    
    /**
     * Returns whether the given class is a MIPAV plugin.
     * @param c A class.
     * @return True, if the given class is a MIPAV plugin.
     */
    public static final boolean isMipavPluginClass(Class c) {
        for (Class inter : c.getInterfaces()) {
            if (inter.equals(PlugInAlgorithm.class) || inter.equals(PlugInGeneric.class) || inter.equals(PlugInFile.class)
                    || inter.equals(PlugIn.class) || inter.equals(PlugInView.class)) {
                return true;
            }
        }
        
        return false;
    }
    
    /**
     * Returns whether the given class is an ImageJ plugin.
     * @param c A class.
     * @return True, if the given class is an ImageJ plugin.
     */
    public static final boolean isImageJPluginClass(Class c) {
        for (Class inter : c.getInterfaces()) {
            if (inter.equals(ij.plugin.PlugIn.class) || inter.equals(ij.plugin.filter.PlugInFilter.class) || c.getSuperclass().equals(ij.plugin.frame.PlugInFrame.class)) {
                return true;
            }
        }
        
        return false;
    }

    private boolean examineClass(Class c) {
        try {
            c.newInstance();
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }

        Class[] possibleDep = c.getDeclaredClasses();
        for (int j = 0; j < possibleDep.length; j++) {
            if (isInPluginFolder(possibleDep[j])) {
                boolean compiles = examineClass(possibleDep[j]);
                if ( !compiles) {
                    return false;
                }
            }
        }

        try {
            @SuppressWarnings("unused")
            Field[] f = c.getDeclaredFields();
        } catch (NoClassDefFoundError e) {
            e.printStackTrace();
            return false;
        }

        return true;
    }

    /**
     * Attempted to find dependencies of used files after they were temporarily added to the class path (included
     * unpacking jars)
     * 
     * @param allFiles
     * @see moveFiles()
     * @see removeFiles(ArrayList<File>)
     * @return
     */
    @SuppressWarnings("unused")
    private Vector<Color> buildColorTable(Vector<File> allFiles) {
        Vector<Color> vectorColors = new Vector<Color>();
        for (int i = 0; i < allFiles.size(); i++) {
            vectorColors.add(Color.black);
        }
        for (int i = 0; i < allFiles.size(); i++) {
            String name = files.get(i).getName();
            if (name.indexOf(".class") != -1) {
                Class c = null;
                try {
                    c = Class.forName(name.substring(0, name.indexOf(".class")));
                } catch (ClassNotFoundException e) { // likely class issue
                    e.printStackTrace();
                    vectorColors.set(i, Color.red);
                } catch (NoClassDefFoundError e) { // likely class issue
                    e.printStackTrace();
                    vectorColors.set(i, Color.red);
                }
                if (c != null) {
                    boolean isPlugin = isPluginClass(c);
                    
                    if (isPlugin) {
                        Class[] dep = null;
                        try {
                            dep = gatherDependents(c);
                            if (dep != null) {
                                vectorColors.set(i, Color.blue);
                            }
                        } catch (NoClassDefFoundError e) {
                            Preferences.debug("Missing required class " + e.getMessage());
                            vectorColors.set(i, Color.red);
                        } catch (Throwable e) {
                            vectorColors.set(i, Color.red);
                        }
                    }
                }
            }
        }

        return vectorColors;
    }

    private class FileCellRenderer extends JLabel implements ListCellRenderer {

        public FileCellRenderer() {
            setOpaque(true);
        }

        public Component getListCellRendererComponent(JList list, Object value, int index, boolean selected, boolean chf) {
            setComponentOrientation(list.getComponentOrientation());

            Color bg = null;

            if (selected) {
                setBackground(bg == null ? list.getSelectionBackground() : bg);
            } else {
                setBackground(list.getBackground());
            }

            try {
                setForeground(filesColor.get(index));
            } catch (ArrayIndexOutOfBoundsException e) {
                setForeground(Color.black);
            }

            if (value instanceof Icon) {
                setIcon((Icon) value);
                setText("");
            } else {
                setIcon(null);
                setText( (value == null) ? "" : value.toString());
            }

            setEnabled(list.isEnabled());

            Border border = null;
            if (chf) {
                if (selected) {
                    border = new LineBorder(new Color(99, 130, 191), 1);
                } else {
                    border = new EmptyBorder(1, 1, 1, 1);
                }
            } else {
                border = new EmptyBorder(1, 1, 1, 1);
            }
            setBorder(border);

            return this;
        }
    }

    /**
     * A generic file tree.
     * 
     * @author senseneyj
     */
    public class JFileTreePanel extends JPanel {

        /** The file tree. */
        private JTree tree;

        public JFileTreePanel() {
            this.setLayout(new BorderLayout());

            File root = new File(initTreeLoc);
            JFileTreeNode rootTreeNode = new JFileTreeNode(root);
            tree = new JTree(rootTreeNode);
            tree.setCellRenderer(new JFileTreeCellRenderer());
            tree.setRootVisible(false);
            tree.expandRow(0);

            JScrollPane scrollPane = new JScrollPane(tree);
            scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
            setPreferredSize(new Dimension(300, 350));
            add(scrollPane, BorderLayout.CENTER);
        }

        public JTree getFileTree() {
            return tree;
        }

        public JTree setRootDir(File rootDir) {
            JFileTreeNode rootTreeNode = new JFileTreeNode(rootDir);
            JTree tempTree = new JTree(rootTreeNode);

            tree.setModel(tempTree.getModel());
            tree.setRootVisible(false);
            tree.expandRow(0);

            return tree;
        }

        public File getRootDir() {
            return ((JFileTreeNode) tree.getModel().getRoot()).getFile();
        }

        /**
         * Renderer for the file tree.
         * 
         * @author senseneyj
         */
        private class JFileTreeCellRenderer extends DefaultTreeCellRenderer {

            private HashMap<String, Icon> iconCache = new HashMap<String, Icon>();

            /** cache for speeding up loading of files, cleared when select occurs* */
            private HashMap<File, String> rootNameCache = new HashMap<File, String>();

            public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded,
                    boolean leaf, int row, boolean hasFocus) {
                JFileTreeNode node = (JFileTreeNode) value;
                File file = node.file;
                String filename = "";
                if (file != null) {
                    if (node.isRoot) {
                        filename = rootNameCache.get(file);
                        if (filename == null) {
                            filename = fileSystem.getSystemDisplayName(file);
                            rootNameCache.put(file, filename);
                        }
                    } else {
                        filename = file.getName();
                    }
                }
                JLabel result = (JLabel) super.getTreeCellRendererComponent(tree, filename, sel, expanded, leaf, row,
                        hasFocus);
                if (file != null) {
                    Icon icon = iconCache.get(filename);
                    if (icon == null) {
                        icon = fileSystem.getSystemIcon(file);
                        this.iconCache.put(filename, icon);
                    }
                    result.setIcon(icon);
                }
                return result;
            }
        }

    }

    /**
     * A node in the file tree.
     * 
     * @author senseneyj
     */
    class JFileTreeNode implements TreeNode {

        File file;

        File[] children;

        TreeNode parent;

        /** Whether root of file system */
        boolean isRoot;

        private boolean displayAll = ! (checkShow == null || checkShow.isSelected());

        /**
         * Creates a new file tree node.
         * 
         * @param file Node file
         * @param isFileSystemRoot whether the file is a file system root
         * @param parent parent node
         */
        public JFileTreeNode(File file, boolean isFileSystemRoot, TreeNode parent) {
            this.file = file;
            this.isRoot = isFileSystemRoot;
            this.parent = parent;
            File[] potentialFileList = file.listFiles();
            ArrayList<File> fileList = new ArrayList<File>();
            if (potentialFileList != null) {
                for (int i = 0; i < potentialFileList.length; i++) {
                    if (displayAll || isAJavaContainer(potentialFileList[i])) {
                        fileList.add(potentialFileList[i]);
                    }
                }
            }
            File[] finalFileList = new File[fileList.size()];
            fileList.toArray(finalFileList);
            this.children = finalFileList;
            if (this.children == null) {
                this.children = new File[0];
            }
        }

        /**
         * Creates a new file tree node.
         */
        public JFileTreeNode(File child) {
            this.file = null;
            this.parent = null;
            this.children = new File[1];
            this.children[0] = child;
        }

        public Enumeration<?> children() {
            final int elementCount = children.length;
            return new Enumeration<File>() {
                int count = 0;

                public boolean hasMoreElements() {
                    return count < elementCount;
                }

                public File nextElement() {
                    if (this.count < elementCount) {
                        return JFileTreeNode.this.children[count++];
                    }
                    throw new NoSuchElementException("Vector Enumeration");
                }
            };
        }

        public File getFile() {
            return file;
        }

        public boolean getAllowsChildren() {
            return true;
        }

        public TreeNode getChildAt(int childIndex) {
            return new JFileTreeNode(children[childIndex], parent == null, this);
        }

        public int getChildCount() {
            return children.length;
        }

        public int getIndex(TreeNode node) {
            JFileTreeNode subNode = (JFileTreeNode) node;
            for (int i = 0; i < children.length; i++) {
                if (subNode.file.equals(children[i])) {
                    return i;
                }
            }
            return -1;
        }

        public TreeNode getParent() {
            return this.parent;
        }

        public boolean isLeaf() {
            return (getChildCount() == 0);
        }

        /**
         * Whether file qualifies as being a Java container (this is anything that may include or is a class file).
         */
        private boolean isAJavaContainer(File file) {
            if (file.isDirectory()) {
                return true;
            } else {
                String name = file.getName();
                if (name.contains(".class") || name.contains(".jar") || name.contains(".zip")
                        || name.contains(".tar.gz")) {
                    return true;
                }
            }

            return false;
        }
    }
}
