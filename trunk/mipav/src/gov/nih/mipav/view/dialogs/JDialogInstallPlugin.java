package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import com.ice.tar.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;
import java.util.zip.*;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.filechooser.FileSystemView;
import javax.swing.table.DefaultTableModel;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;


/**
 * Simple dialog to install a plugin. The user selects which .class file to install using a file chooser. The file is
 * copied into MIPAV's class directory and the mipav.preferences file is updated accordingly. The menubars are also
 * updated.
 *
 * @version  1.0 July 19, 2000
 * @author   Harman Singh
 */

public class JDialogInstallPlugin extends JDialogBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8736744495208652866L;
    
	/** File system view. */
	private static FileSystemView fileSystem = FileSystemView.getFileSystemView();

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

	private ClassSelectorPanel selectorPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     */
    public JDialogInstallPlugin(JFrame theParentFrame) {
        super(theParentFrame, false);
        ui = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        /*if (source == OKButton) {
            int i;

            if (files.size() == 0) {
                MipavUtil.displayError("Please select PlugIn file(s)");

                return;
            }

            // make the plugins directory if it does not exist
            if (!new File(pluginDir).isDirectory()) {
                new File(pluginDir).mkdirs();
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

                if (currentFile.getName().endsWith(".class")) {

                    try {
                        fr = new FileInputStream(currentFile); // sets the fileinput to the directory chosen from the
                                                               // browse option
                        fw = new FileOutputStream(pluginDir + File.separatorChar + currentFile.getName()); // the location to be copied is MIPAV's class path

                        br = new BufferedInputStream(fr);
                        bw = new BufferedOutputStream(fw);

                        int fileLength = (int) currentFile.length();

                        byte[] byteBuff = new byte[fileLength];

                        if (fileLength != 0) {

                            while (br.read(byteBuff, 0, fileLength) != -1) {
                                bw.write(byteBuff, 0, fileLength);
                            }
                        }

                    } catch (FileNotFoundException fnfe) {
                        MipavUtil.displayError("InstallPlugin: " + fnfe);
                        dispose();

                        return;
                    } catch (IOException ioe) {
                        MipavUtil.displayError("Error reading/writing plugin files.  Try manually copying .class files to " +
                                               pluginDir);
                        dispose();

                        return;
                    } finally {

                        try {

                            if (br != null) {
                                br.close();
                            }

                            if (bw != null) {
                                bw.close();
                            }
                        } catch (IOException ioe) {
                            ioe.printStackTrace();
                        }
                    }
                }
                // must be a .jar or .zip so extract files
                else if (currentFile.getName().endsWith(".zip") || currentFile.getName().endsWith(".jar")) {

                    try {
                        zIn = new ZipInputStream(new FileInputStream(currentFile));
                        entry = null;

                        // if the entry is a directory of is a class file, extract it
                        while ((entry = zIn.getNextEntry()) != null) {

                            if (entry.isDirectory()) {
                                String dirname = pluginDir + File.separator +
                                                 entry.getName().substring(0, entry.getName().length() - 1);
                                new File(dirname).mkdir();
                            } else {

                                try {
                                    new File(pluginDir + File.separator + entry.getName()).getParentFile().mkdirs();
                                } catch (Exception ex) {
                                    // do nothing...no parent dir here
                                }

                                fw = new FileOutputStream(pluginDir + File.separator + entry.getName());

                                // Transfer bytes from the ZIP file to the output file
                                buf = new byte[1024];

                                while ((len = zIn.read(buf)) > 0) {
                                    fw.write(buf, 0, len);
                                }

                                fw.close();

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
                        readTar(getInputStream(currentFile.getPath()), pluginDir);
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

            // updates menubar for each image
            Vector imageFrames = ui.getImageFrameVector();

            if (imageFrames.size() < 1) {
                ui.buildMenu();
                ui.setControls();
            } else {

                for (i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
                }
            }

            dispose();

            return;
        }*/
        if(source == OKButton) {
        	JList selected = selectorPanel.getSelectedFiles();
        	for(int i=0; i<selected.getModel().getSize(); i++) {
        		System.out.println(selected.getModel().getElementAt(i));
        	}
        } else if (source == cancelButton) {
            dispose();
        }
    }

    /**
     * Sets up GUI dialog.
     */
    private void init() {
        setForeground(Color.black);
        addNotify();
        setTitle("Install Plugin");

        JPanel mainPanel = new JPanel();
        mainPanel.setForeground(Color.black);
        
        selectorPanel = new ClassSelectorPanel();
        selectorPanel.setVisible(true);
        mainPanel.add(selectorPanel);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        OKButton.setText("Install Plugin(s)");
        OKButton.setPreferredSize(new Dimension(160, 30));
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
    
    private class ClassSelectorPanel extends JPanel implements ActionListener {

    	private static final String BROWSE = "Browse";
    	
    	private static final String MOVE_RIGHT = "Move Right";
    	
    	private static final String DELETE = "Delete";
    	
    	private final Dimension DEFAULT_DIM = new Dimension(400, 200);
    	
    	private JTree fileTree;
    	
    	private JFileTreePanel subFilePanel;
    	
    	private JTextField initDir;
    	
    	private JList selected;
    	
		public ClassSelectorPanel() {
			setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
			
			JPanel dirSelectPanel = new JPanel();
			dirSelectPanel.setLayout(new BoxLayout(dirSelectPanel, BoxLayout.X_AXIS));
			dirSelectPanel.setBorder(MipavUtil.buildTitledBorder("Select a directory"));
			
			JButton browseButton = new JButton(BROWSE);
			browseButton.addActionListener(this);
			browseButton.setActionCommand(BROWSE);
			dirSelectPanel.add(browseButton);
			
			JLabel dirLabel = new JLabel("Current directory: ");
			dirSelectPanel.add(dirLabel);
			
			initDir = new JTextField("Select a directory to search for plugins");
			initDir.setFont(MipavUtil.font12);
			dirSelectPanel.add(initDir);
			add(dirSelectPanel);
			
			JPanel mainSelectorPanel = new JPanel();
			mainSelectorPanel.setLayout(new BoxLayout(mainSelectorPanel, BoxLayout.X_AXIS));
			
			subFilePanel = new JFileTreePanel();
			subFilePanel.setBorder(MipavUtil.buildTitledBorder("Select class files"));
			fileTree = subFilePanel.getFileTree();
			mainSelectorPanel.add(subFilePanel);
			
			JPanel selectOptionsPanel = buildSelectOptionsPanel();
			mainSelectorPanel.add(selectOptionsPanel);
			
			JPanel fileListPanel = new JPanel();
			fileListPanel.setBorder(MipavUtil.buildTitledBorder("Selected class files"));
			selected = new JList();
			selected.setModel(new DefaultListModel());
			selected.setMinimumSize(new Dimension(300, 800));
			selected.setPreferredSize(new Dimension(300, 800));
			JScrollPane scrollPane = new JScrollPane(selected);
			scrollPane.setMinimumSize(new Dimension(300, 400)); //TODO: Fix dim
			scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
			scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
			fileListPanel.add(scrollPane);
			mainSelectorPanel.add(fileListPanel);
			add(mainSelectorPanel);
			
			pack();
		}
		
		public JList getSelectedFiles() {
			return selected;
		}
		
		private JPanel buildSelectOptionsPanel() {
			JPanel selectOptionsPanel = new JPanel();
			selectOptionsPanel.setLayout(new BoxLayout(selectOptionsPanel, BoxLayout.Y_AXIS));
			
			JButton moveRight = new JButton();
			moveRight.addActionListener(this);
			moveRight.setActionCommand(MOVE_RIGHT);
			moveRight.setIcon(MipavUtil.getIcon("rightarrow.gif"));
			selectOptionsPanel.add(moveRight);
			
			JButton delete = new JButton();
			delete.addActionListener(this);
			delete.setActionCommand(DELETE);
			delete.setIcon(MipavUtil.getIcon("delete.gif"));
			selectOptionsPanel.add(delete);
			
			return selectOptionsPanel;
		}
    	
    	public void actionPerformed(ActionEvent e) {
			if(e.getActionCommand().equals(BROWSE)) {
				JFileChooser chooser = new JFileChooser();

                if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                    chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                }
	            chooser.setMultiSelectionEnabled(false);
	            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

	            if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
	                File selectedFile = chooser.getSelectedFile();
	                
	                fileTree = subFilePanel.setRootDir(selectedFile);
	                
	                initDir.setText(selectedFile.toString());
	            }
			} else if(e.getActionCommand().equals(MOVE_RIGHT)) {
				TreePath[] paths = fileTree.getSelectionModel().getSelectionPaths();
				String[] selectedString = new String[paths.length];
				for(int i=0; i<paths.length; i++) {
					if(!((DefaultListModel)selected.getModel()).contains(((JFileTreeNode)paths[i].getLastPathComponent()).getFile().getName())) {
						((DefaultListModel)selected.getModel()).addElement(((JFileTreeNode)paths[i].getLastPathComponent()).getFile().getName());
					}
				}				
			} else if(e.getActionCommand().equals(DELETE)) {
				Object[] numSelected = selected.getSelectedValues();
				for(int i=0; i<numSelected.length; i++) {
					((DefaultListModel)selected.getModel()).removeElement(numSelected[i]);
				}
			}
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

    		File[] roots = File.listRoots();
    		JFileTreeNode rootTreeNode = new JFileTreeNode(roots[1]);
    		tree = new JTree(rootTreeNode);
    		tree.setCellRenderer(new JFileTreeCellRenderer());
    		tree.setRootVisible(false);
    		tree.setMinimumSize(new Dimension(300, 800)); //TODO: Fix dim
    		tree.setPreferredSize(new Dimension(300, 800));
    		JScrollPane scrollPane = new JScrollPane(tree);
    		scrollPane.setMinimumSize(new Dimension(300, 400));
    		scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
			scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
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
    		
    		return tree;
    	}
    	
    	/**
    	 * Renderer for the file tree.
    	 * 
    	 * @author senseneyj
    	 */
    	private class JFileTreeCellRenderer extends DefaultTreeCellRenderer {

    		private HashMap<String, Icon> iconCache = new HashMap<String, Icon>();

    		private HashMap<File, String> rootNameCache = new HashMap<File, String>();

    		public Component getTreeCellRendererComponent(JTree tree, Object value,
    				boolean sel, boolean expanded, boolean leaf, int row,
    				boolean hasFocus) {
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
    			JLabel result = (JLabel) super.getTreeCellRendererComponent(tree,
    					filename, sel, expanded, leaf, row, hasFocus);
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
		this.children = file.listFiles();
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
}


