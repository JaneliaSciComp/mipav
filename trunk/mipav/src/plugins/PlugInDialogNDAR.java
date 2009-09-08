import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.LightboxGenerator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.srb.JDialogLoginSRB;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.zip.*;

import javax.swing.*;
import javax.swing.border.LineBorder;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.jdom.Document;
import org.jdom.input.SAXBuilder;
import org.jdom.Attribute;
import org.jdom.Element;




public class PlugInDialogNDAR extends JDialogStandalonePlugin implements ActionListener, ChangeListener, ItemListener, TreeSelectionListener {

    /** Scrolling text area for log output */
    private WidgetFactory.ScrollTextArea logOutputArea;

    private JScrollPane listPane, psetPane;

    private JButton loadGUIDsButton;

    private JTable guidTable;

    private JList sourceList, psetList;

    private JButton nextButton, previousButton, addSourceButton, removeSourceButton, addSetButton, addParamButton, editButton, deleteButton;

    private JTabbedPane tabbedPane;

    private JPanel guidPanel;
    
    private JPanel psetPanel;

    private DefaultListModel sourceModel;

    private JCheckBox anonConfirmBox;

    private JTextArea privacyTextArea;

    private boolean doneAddingFiles = false;

    private static final String outputDirBase = System.getProperty("user.home") + File.separator + "mipav"
            + File.separator + "NDAR_Imaging_Submission" + File.separator;

    /** Length of the NDAR GUID */
    private static final int GUID_LENGTH = 12;

    private Hashtable<File, Boolean> multiFileTable = null;

    /** NDAR data object passed into FileWriteOptions and onto the writeXML with specific NDAR info */
    private NDARWriteData ndarData;

    /** Static tab indices */
    private static final int TAB_MAIN = 0;

    private static final int TAB_SOURCE = 1;
    
    private static final int TAB_PSETS = 2;

    private static final int TAB_GUID = 3;

    private static final int TAB_LOG = 4;

    /** GUID table column indices */
    private static final int GUID_TABLE_IMAGE_COLUMN = 0;

    private static final int GUID_TABLE_GUID_COLUMN = 1;
    
    private DefaultMutableTreeNode top, currentNode;
    



    private JTree tree;
    
    private HashMap imagePsets = new HashMap();

    


    public PlugInDialogNDAR() {
        super(false);

        init();
        setVisible(true);

        validate();
    }

    public void actionPerformed(ActionEvent e) {

        /*
         * @todo Implement this java.awt.event.ActionListener abstract method
         */

        String command = e.getActionCommand();

        // System.err.println("size : " + this.getSize());

        if (command.equals("Next")) {
            int index = tabbedPane.getSelectedIndex();

            if (index == TAB_SOURCE) {
                if ( !doneAddingFiles) {
                    int response = JOptionPane.showConfirmDialog(this, "Done adding image datasets?",
                            "Done adding image datasets?", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

                    if (response == JOptionPane.YES_OPTION) {
                        doneAddingFiles = true;
                        
                        tabbedPane.setEnabledAt(TAB_PSETS, true);
                        tabbedPane.setComponentAt(TAB_PSETS, buildPSetPanel());
                        tabbedPane.setSelectedIndex(TAB_PSETS);
                    }
                } else {
                    tabbedPane.setSelectedIndex(TAB_SOURCE);
                }
            }else if(index == TAB_PSETS) {
            	generateGUIFields();
            	tabbedPane.setEnabledAt(TAB_GUID, true);
                tabbedPane.setSelectedIndex(TAB_GUID);
                addSetButton.setEnabled(false);
        		addParamButton.setEnabled(false);
        		editButton.setEnabled(false);
        		deleteButton.setEnabled(false);
            } else if (index == TAB_GUID) {
                //tabbedPane.setEnabledAt(TAB_MAIN, true);
                //tabbedPane.setEnabledAt(TAB_SOURCE, true);
                //tabbedPane.setEnabledAt(TAB_LOG, true);

                // move to TAB_LOG
                tabbedPane.setSelectedIndex(TAB_LOG);

                nextButton.setText("Close");
                nextButton.setEnabled(false);
                previousButton.setEnabled(false);
                tabbedPane.setEnabledAt(TAB_MAIN, false);
                tabbedPane.setEnabledAt(TAB_SOURCE, false);
                tabbedPane.setEnabledAt(TAB_PSETS, false);
                tabbedPane.setEnabledAt(TAB_GUID, false);
                tabbedPane.setEnabledAt(TAB_LOG, true);

                final gov.nih.mipav.SwingWorker worker = new gov.nih.mipav.SwingWorker() {
                    public Object construct() {
                        createSubmissionFiles();

                        return null;
                    }
                };

                worker.start();
            } else if (index == TAB_LOG) {
                dispose();
            } else if (index == TAB_PSETS) {
            	tabbedPane.setSelectedIndex(TAB_GUID);
            	tabbedPane.setEnabledAt(TAB_GUID, true);
            } else if (tabbedPane.getTabCount() > index + 1) {
                tabbedPane.setSelectedIndex(index + 1);
            }

        } else if (command.equals("Previous")) {
            int index = tabbedPane.getSelectedIndex();

            if (index == TAB_GUID) {
                // if (checkGUIDs()) {
                // tabbedPane.setEnabledAt(TAB_LOG, true);
                // }

                tabbedPane.setEnabledAt(TAB_MAIN, true);
                tabbedPane.setEnabledAt(TAB_SOURCE, true);
                tabbedPane.setSelectedIndex(index - 1);
            } else if (index > 0) {
            	if(index == TAB_PSETS) {
            		addSetButton.setEnabled(false);
            		addParamButton.setEnabled(false);
            		editButton.setEnabled(false);
            		deleteButton.setEnabled(false);
            	}
                tabbedPane.setSelectedIndex(index - 1);
            }
        } else if (command.equals("AddSource")) {
            ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
            fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());

            JFileChooser chooser = fileChooser.getFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

            // default to TECH filter
            int filter = ViewImageFileFilter.TECH;

            try {
                filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
            } catch (NumberFormatException nfe) {

                // an invalid value was set in preferences -- so don't use it!
                filter = -1;
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

            if (filter != -1) {
                // it seems that the set command adds the filter again...
                // chooser.addChoosableFileFilter(new ViewImageFileFilter(filter));

                // if filter is something we already added, then remove it before
                // setting it..... (kludgy, kludgy....)
                javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

                if (found != null) {
                    chooser.removeChoosableFileFilter(found);
                }

                // initially set to the preferences
                chooser.setFileFilter(new ViewImageFileFilter(filter));
            }

            int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                boolean isMultiFile = fileChooser.isMulti();

                File[] files = chooser.getSelectedFiles();
                ViewUserInterface.getReference().setDefaultDirectory(files[0].getParent());
                for (int i = 0; i < files.length; i++) {
                    if ( !sourceModel.contains(files[i])) {
                        sourceModel.addElement(files[i]);
                        multiFileTable.put(files[i], new Boolean(isMultiFile));
                    }
                }
            }
            removeSourceButton.setEnabled(sourceModel.size() > 0);
            nextButton.setEnabled(sourceModel.size() > 0);
            
            listPane.setBorder(buildTitledBorder(sourceModel.size() + " image(s) selected"));

        } else if (command.equals("RemoveSource")) {
            int[] selected = sourceList.getSelectedIndices();
            for (int i = selected.length - 1; i >= 0; i--) {
                sourceModel.removeElementAt(selected[i]);
                multiFileTable.remove(selected[i]);
            }
            removeSourceButton.setEnabled(sourceModel.size() > 0);
            nextButton.setEnabled(sourceModel.size() > 0);
            listPane.setBorder(buildTitledBorder(sourceModel.size() + " image(s) selected"));
        } else if (command.equals("Source")) {
            tabbedPane.setSelectedIndex(TAB_SOURCE);
        } else if (command.equals("LoadGUIDs")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(false);

            chooser.setFont(MipavUtil.defaultMenuFont);

            int returnVal = chooser.showOpenDialog(null);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                loadGUIDsFromFile(chooser.getSelectedFile());

            }
        } else if (command.equals("Help")) {
            switch (tabbedPane.getSelectedIndex()) {
                case TAB_MAIN:
                    MipavUtil.showHelp("ISPMain01");
                    break;
                case TAB_SOURCE:
                    MipavUtil.showHelp("ISPImages01");
                    break;
                case TAB_GUID:
                    MipavUtil.showHelp("ISPGUID01");
                    break;
                case TAB_LOG:
                    MipavUtil.showHelp("ISPLog01");
                    break;
            }
        } else if (command.equals("AddSet")) {
        	new PSetDialog(this,false);
        } else if (command.equals("AddParam")) {
        	new ParamDialog(this,false);
        }else if(command.equals("edit")) {
        	if(currentNode.getPath().length == 3) {
        		new PSetDialog(this,true);
        	}else {
        		new ParamDialog(this,true);
        	}
        	
        }else if(command.equals("delete")) {
        	 int response = JOptionPane.showConfirmDialog(this, "Are you sure you want to delete?", "Delete", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
             if (response == JOptionPane.YES_OPTION) {
            	 if(currentNode.getPath().length == 3) {
	            	 String imageName = "";
	            	 DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode)currentNode.getParent();
	            	 imageName = parentNode.toString();
	            	 HashMap pSetsHashMap = (HashMap)(imagePsets.get(imageName));
	            	 String description = currentNode.toString();
	            	 pSetsHashMap.remove(description);
	            	 DefaultTreeModel model = (DefaultTreeModel)tree.getModel();
	            	 model.removeNodeFromParent(currentNode);
            	 }else {
            		 String imageName = "";
            		 DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode)currentNode.getParent();
            		 DefaultMutableTreeNode grandParentNode = (DefaultMutableTreeNode)parentNode.getParent(); 
            		 imageName = grandParentNode.toString();
            		 HashMap pSetsHashMap = (HashMap)(imagePsets.get(imageName));
            		 String description = parentNode.toString();
            		 XMLPSet p = (XMLPSet)pSetsHashMap.get(description);
 					 String currentParam = currentNode.toString();
 					 int index1 = currentParam.indexOf("::");
					 currentParam = currentParam.substring(0, index1);
 					 p.removeParameter(currentParam);
 					 DefaultTreeModel model = (DefaultTreeModel)tree.getModel();
	            	 model.removeNodeFromParent(currentNode); 
            	 } 
             }
        }
    }

    public void stateChanged(ChangeEvent e) {
        int index = tabbedPane.getSelectedIndex();
        if (index == TAB_MAIN) {
            previousButton.setEnabled(false);
        } else {
            previousButton.setEnabled(true);
        }

        if (index == TAB_SOURCE) {
            nextButton.setEnabled(sourceModel.size() > 0);
        } else if (index == TAB_GUID) {
            previousButton.setEnabled(true);
            nextButton.setEnabled(true);
            //tabbedPane.setEnabledAt(TAB_MAIN, false);
            //tabbedPane.setEnabledAt(TAB_SOURCE, false);
            //tabbedPane.setEnabledAt(TAB_LOG, false);
        } else {
            nextButton.setEnabled(true);
        }

        addSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && !doneAddingFiles);
        removeSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && sourceModel.size() > 0
                && !doneAddingFiles);
        //addSetButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_PSETS);
        //addParamButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_PSETS);
        //editButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_GUID);
        //deleteButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_GUID);
        
        loadGUIDsButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_GUID);
    }

    public void itemStateChanged(ItemEvent e) {
        if (e.getSource().equals(anonConfirmBox)) {
            if (anonConfirmBox.isSelected()) {
                anonConfirmBox.setEnabled(false);
                nextButton.setEnabled(true);

                tabbedPane.setEnabledAt(TAB_SOURCE, true);
                privacyTextArea.setBackground(helpButton.getBackground());
            }
        }
    }

    private void loadGUIDsFromFile(File guidFile) {
        RandomAccessFile raFile;
        try {
            raFile = new RandomAccessFile(guidFile, "r");
            String tempStr = null;
            String validGUID = null;
            int counter = 0;
            do {

                tempStr = raFile.readLine();
                if (tempStr != null) {
                    validGUID = getValidGUID(tempStr);
                    if (validGUID != null) {
                        guidTable.getModel().setValueAt(validGUID, counter, GUID_TABLE_GUID_COLUMN);
                    }
                }
                counter++;
            } while (tempStr != null);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void init() {
        setTitle("NDAR Image Submission Package Creation Tool");

        multiFileTable = new Hashtable<File, Boolean>();

        tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Main", buildMainTab());
        tabbedPane.addTab("Images", buildSourcePanel());
        tabbedPane.addTab("Parameter Sets", buildPSetPanel());
        tabbedPane.addTab("GUIDs", buildGUIDPane());
        tabbedPane.addTab("Log", buildLogTab());

        tabbedPane.setEnabledAt(TAB_SOURCE, false);
        tabbedPane.setEnabledAt(TAB_PSETS, false);
        tabbedPane.setEnabledAt(TAB_GUID, false);
        tabbedPane.setEnabledAt(TAB_LOG, false);

        tabbedPane.addChangeListener(this);

        getContentPane().add(tabbedPane);
        getContentPane().add(buildButtonPanel(), BorderLayout.SOUTH);
        pack();
        validate();
        this.setMinimumSize(new Dimension(610, 437));
        this.setSize(new Dimension(610, 437));
    }

    private JScrollPane buildMainTab() {
        JPanel mainPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;

        privacyTextArea = new JTextArea();
        privacyTextArea.setFont(MipavUtil.font12);
        privacyTextArea.setText(JDialogLoginSRB.NDAR_PRIVACY_NOTICE);
        privacyTextArea.setEditable(false);

        mainPanel.add(privacyTextArea, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        anonConfirmBox = WidgetFactory.buildCheckBox("I agree to the above statement", false);
        anonConfirmBox.addItemListener(this);

        mainPanel.add(anonConfirmBox, gbc);

        JScrollPane privacyPane = WidgetFactory.buildScrollPane(mainPanel);

        return privacyPane;
    }

    /**
     * Build a panel for the zip and metadata file creation log.
     */
    private JPanel buildLogTab() {
        JPanel destPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.gridy = 0;
        gbc2.gridx = 0;

        logOutputArea = WidgetFactory.buildScrollTextArea(Color.white);
        logOutputArea.setBorder(buildTitledBorder("Output log"));
        logOutputArea.getTextArea().setEditable(false);

        destPanel.add(logOutputArea, gbc2);

        return destPanel;
    }
    
    private JScrollPane buildPSetPanel(){
    	
    	if(sourceModel.size() > 0) {
    		DefaultMutableTreeNode imageNode = null;
    		top = new DefaultMutableTreeNode("Images");
    		tree = new JTree(top);
    		tree.setCellRenderer(new MyTreeRenderer());
    		tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    		tree.addTreeSelectionListener(this);
    		DefaultTreeModel model = (DefaultTreeModel)tree.getModel();

    		for(int i=0;i<sourceModel.size();i++) {
    			imageNode = new DefaultMutableTreeNode(((File)sourceModel.elementAt(i)).getName());
    		    //top.add(imageNode);
    		    model.insertNodeInto(imageNode, top, top.getChildCount());
    		}
    
    		for (int i = 0; i < tree.getRowCount(); i++) {
    	         tree.expandRow(i);
    	    }

	        psetPane = new JScrollPane(tree);

	        return psetPane;
    	}else {
    		psetPane = new JScrollPane();
            return psetPane;
    		
    	}
    }
    
    
    

    public void valueChanged(TreeSelectionEvent e) {

    	currentNode = (DefaultMutableTreeNode)tree.getLastSelectedPathComponent();
    	
    	if(currentNode == null) {
    		addSetButton.setEnabled(false);
    		addParamButton.setEnabled(false);
    		editButton.setEnabled(false);
    		deleteButton.setEnabled(false);
    		return;
    	}
    	
    	
    	if(currentNode.getPath().length == 2) {
    		addSetButton.setEnabled(true);
    		addParamButton.setEnabled(false);
    		editButton.setEnabled(false);
    		deleteButton.setEnabled(false);
    	}else if(currentNode.getPath().length == 3) {
    		addParamButton.setEnabled(true);
    		addSetButton.setEnabled(false);
    		editButton.setEnabled(true);
    		deleteButton.setEnabled(true);	
    	}else if(currentNode.getPath().length == 4) {
    		addParamButton.setEnabled(false);
    		addSetButton.setEnabled(false);
    		editButton.setEnabled(true);
    		deleteButton.setEnabled(true);	
    	}else {
    		addSetButton.setEnabled(false);
    		addParamButton.setEnabled(false);
    		editButton.setEnabled(false);
    		deleteButton.setEnabled(false);
    	}
    	
    
	}

	private JScrollPane buildSourcePanel() {
        //JPanel sourcePanel = new JPanel();
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;

        sourceModel = new DefaultListModel();
        sourceList = new JList(sourceModel);

        listPane = WidgetFactory.buildScrollPane(sourceList);
        listPane.setBorder(buildTitledBorder(0 + " image(s) selected"));
        //sourcePanel.add(listPane, gbc);

        return listPane;
    }

    private JScrollPane buildGUIDPane() {
        guidPanel = new JPanel(new GridLayout());
        JScrollPane guidPane = WidgetFactory.buildScrollPane(guidPanel);

        return guidPane;
    }
    
    private void generatePSETFields() {
    	
    }

    private void generateGUIFields() {
        int numImages = sourceModel.size();

        GUIDTableModel guidTableModel = new GUIDTableModel(numImages);
        guidTable = new JTable(guidTableModel);

        JScrollPane guidScrollPane = new JScrollPane(guidTable);

        guidTable.getTableHeader().setReorderingAllowed(false);
        guidTable.getTableHeader().setResizingAllowed(true);

        String longestValue = new String();

        for (int i = 0; i < numImages; i++) {
            guidTable.getModel().setValueAt(sourceModel.elementAt(i).toString(), i, GUID_TABLE_IMAGE_COLUMN);

            if (sourceModel.elementAt(i).toString().length() > longestValue.length()) {
                longestValue = sourceModel.elementAt(i).toString();
            }

            String guidString = getValidGUID(sourceModel.elementAt(i).toString());
            if (guidString != null) {
                guidTable.getModel().setValueAt(guidString, i, GUID_TABLE_GUID_COLUMN);
            }
        }

        // set the file name column width based on the longest file path
        TableCellRenderer headerRenderer = guidTable.getTableHeader().getDefaultRenderer();
        TableColumn column = guidTable.getColumnModel().getColumn(GUID_TABLE_IMAGE_COLUMN);
        Component comp = headerRenderer
                .getTableCellRendererComponent(null, column.getHeaderValue(), false, false, 0, 0);
        int headerWidth = comp.getPreferredSize().width;
        comp = guidTable.getDefaultRenderer(guidTableModel.getColumnClass(GUID_TABLE_IMAGE_COLUMN))
                .getTableCellRendererComponent(guidTable, longestValue, false, false, 0, GUID_TABLE_IMAGE_COLUMN);
        int cellWidth = comp.getPreferredSize().width;
        column.setPreferredWidth(Math.max(headerWidth, cellWidth));

        // the guids are of a fixed length
        guidTable.getColumnModel().getColumn(GUID_TABLE_GUID_COLUMN).setPreferredWidth(50);

        guidPanel.setBorder(buildTitledBorder("Assign a GUID to each image dataset"));
        guidPanel.add(guidScrollPane);
    }

    private String getValidGUID(String testString) {
        String validGUID = null;
        int ndarIndex = testString.indexOf("NDAR");
        if (ndarIndex != -1 && (ndarIndex + GUID_LENGTH < testString.length())) {

            validGUID = testString.substring(ndarIndex, ndarIndex + GUID_LENGTH);
            if (isValidGUID(validGUID)) {
                return validGUID;
            }
        }
        validGUID = null;
        return validGUID;
    }

    /**
     * Checks to see if the given string is a valid NDAR GUID
     * 
     * @param checkString the string to check
     * @return whether this is a valid guid
     */
    private boolean isValidGUID(String checkString) {
        if (checkString.length() != GUID_LENGTH) {
            return false;
        }

        if (isValidChar(checkString.charAt(4)) && isValidChar(checkString.charAt(5))
                && isNumChar(checkString.charAt(6)) && isNumChar(checkString.charAt(7))
                && isNumChar(checkString.charAt(8)) && isValidChar(checkString.charAt(9))
                && isValidChar(checkString.charAt(10))
                && (isNumChar(checkString.charAt(11)) || isValidChar(checkString.charAt(11)))) {
            return true;
        }
        return false;
    }

    /**
     * Is the char a valid number character
     * 
     * @param checkChar char to check
     * @return whether is a number
     */
    private boolean isNumChar(char checkChar) {
        return (checkChar >= '0' && checkChar <= '9');
    }

    /**
     * Check if this is a valid NDAR character ( no I, O, Q, or S)
     * 
     * @param checkChar char to check
     * @return is the char valid
     */
    private boolean isValidChar(char checkChar) {
        if ( (checkChar >= 'a' && checkChar <= 'z') || (checkChar >= 'A' && checkChar <= 'Z')) {
            if (checkChar != 'i' && checkChar != 'I' && checkChar != 'o' && checkChar != 'O' && checkChar != 'q'
                    && checkChar != 'Q' && checkChar != 's' && checkChar != 'S') {
                return true;
            }
        }

        return false;
    }

    /**
     * Create the ZIP(s) containing the original image files and the XML meta-data for each image dataset.
     */
    private void createSubmissionFiles() {
        if ( !new File(outputDirBase).exists()) {
            new File(outputDirBase).mkdirs();
        }

        ndarData = new NDARWriteData();

        int numImages = sourceModel.size();
        System.out.println(numImages);
        for (int i = 0; i < numImages; i++) {
        	System.out.println("i is " + i);
            File imageFile = (File) sourceModel.elementAt(i);

            printlnToLog("Opening: " + imageFile + ", multifile: " + multiFileTable.get(imageFile));

            // ViewJFrameImage invisFrame = new ViewJFrameImage(tempImage);

            FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);
            ModelImage origImage = fileIO.readImage(imageFile.getName(), imageFile.getParent() + File.separator,
                    multiFileTable.get(imageFile), null);

            List<String> origFiles = FileUtility.getFileNameList(origImage);
            
            //create a thumbnail image...4 colums, 2 rows
            //grab the middle 8 slices from the image for the thumbnail
            //need to determine by what percentage...so...need to figure out by what percebtahe the xdim will go down to 128
            //startSLice will be 3 less than middle slice 
            //endSlice will be 4 more than middle slixe
            int xDim = origImage.getExtents()[0];
            int percentage = 100;
            if(xDim > 128) {
            	float perc = 128f/xDim * 100;
            	percentage = (int)Math.floor(perc);
            }
            int columns = 4;
            int rows = 2;
            int rBorderVal = 255;
            int gBorderVal = 0;
            int bBorderVal = 0;
            int borderThick = 1;
            int startSlice = 0;
            int endSlice = 0;
            int numSlices = 0;
            int middleSlice = 0;
            LightboxGenerator lightGen;
            ModelImage thumbnailImage = null;
            String dir = origImage.getImageDirectory();
            System.out.println(dir);
            ViewJFrameImage vf = null;
            if(origImage.is2DImage()) {
            	
            	//Creating a blank TransMatrix for resampling
        		TransMatrix percentSizer = new TransMatrix(4);
        		percentSizer.Set((float)1, (float)0, (float)0, (float)0, (float)0,
        				(float)1, (float)0, (float)0, (float)0, (float)0, (float)1, (float)0, 
        				(float)0, (float)0, (float)0, (float)1);
            	
        		//Resample image size based on percent inputted
            	AlgorithmTransform transformer = new AlgorithmTransform(origImage, percentSizer, 1, (float)(origImage.getResolutions(0)[0]/(percentage*.01)),
            			(float)(origImage.getResolutions(0)[1]/(percentage*.01)), (int)(origImage.getExtents()[0] * percentage*.01),
            			(int)(origImage.getExtents()[1]*percentage*.01), origImage.getUnitsOfMeasure(), false, true, false, true, origImage.getImageCentermm(false) );
            	transformer.runAlgorithm();
            	thumbnailImage = transformer.getTransformedImage();
            	thumbnailImage.calcMinMax();
            	vf = new ViewJFrameImage(thumbnailImage, (ModelImage)null);

            	
            }else if(origImage.is3DImage()) {
            	numSlices = origImage.getExtents()[2];
            	numSlices = numSlices - 1;  //its 0 based
            	middleSlice = numSlices/2;
            	startSlice = middleSlice - 3;
            	if(startSlice < 0) {
            		startSlice = 0;
            	}
            	endSlice = middleSlice + 4;
            	if(endSlice > numSlices - 1) {
            		endSlice = numSlices - 1;
            	}
            	
            	try {
                    // Make algorithm
            		lightGen = new LightboxGenerator(origImage, startSlice, endSlice, percentage, rows, columns, rBorderVal, gBorderVal, bBorderVal, false, borderThick);
            		lightGen.run();
            		thumbnailImage = lightGen.getImage();
            		thumbnailImage.calcMinMax();
            		System.out.println("here1");
            		
            		vf = new ViewJFrameImage(thumbnailImage, (ModelImage)null);

            		
            	}catch(Exception e) {
            		e.printStackTrace();
            	}	
            }else if(origImage.is4DImage()) {
            	//get middle time volume
            	int[] destExtents = new int[3]; 
                int xSlices = origImage.getExtents()[0];
                int ySlices = origImage.getExtents()[1];
                int zSlices = origImage.getExtents()[2];
                destExtents[0] = xSlices;
                destExtents[1] = ySlices;
                destExtents[2] = zSlices;
                
                ModelImage timeImage = new ModelImage(origImage.getType(), destExtents, "");
                
                int tSlices =  origImage.getExtents()[3];
                System.out.println("tSlices is " + tSlices);
                
                int middleVol = (int)Math.floor(tSlices/2);
                if(middleVol > 0) {
                	middleVol = middleVol - 1;  // 0 based
                }
                AlgorithmSubset subsetAlgo = new AlgorithmSubset(origImage, timeImage, AlgorithmSubset.REMOVE_T, middleVol);
                subsetAlgo.run();
                
                numSlices = timeImage.getExtents()[2];
            	numSlices = numSlices - 1;  //its 0 based
            	middleSlice = numSlices/2;
            	startSlice = middleSlice - 3;
            	if(startSlice < 0) {
            		startSlice = 0;
            	}
            	endSlice = middleSlice + 4;
            	if(endSlice > numSlices - 1) {
            		endSlice = numSlices - 1;
            	}
            	
            	try {
                    // Make algorithm
            		lightGen = new LightboxGenerator(timeImage, startSlice, endSlice, percentage, rows, columns, rBorderVal, gBorderVal, bBorderVal, false, borderThick);
            		lightGen.run();
            		thumbnailImage = lightGen.getImage();
            		thumbnailImage.calcMinMax();

            		vf = new ViewJFrameImage(thumbnailImage, (ModelImage)null);

            	}catch(Exception e) {
            		
            	}	
                
                
            }
            

            

            String currentGuid = (String) guidTable.getModel().getValueAt(i, GUID_TABLE_GUID_COLUMN);
            int modality = origImage.getFileInfo(0).getModality();
            String modalityString = FileInfoBase.getModalityStr(modality).replaceAll("\\s+", "");

            String outputFileNameBase;
            if (modality == FileInfoBase.UNKNOWN_MODALITY) {
                outputFileNameBase = currentGuid + "_" + System.currentTimeMillis();
            } else {
                outputFileNameBase = currentGuid + "_" + modalityString + "_" + System.currentTimeMillis();
            }

            String zipFilePath = outputDirBase + outputFileNameBase + ".zip";
            
            //write out thumbnail image
            FileWriteOptions opts = new FileWriteOptions(outputFileNameBase + ".jpg", outputDirBase, true);
	        fileIO.writeImage(thumbnailImage, opts);
	        vf.close();
	        
            try {
                printlnToLog("Creating ZIP file:\t" + zipFilePath);
                for (String file : origFiles) {
                    printlnToLog("Adding file to ZIP:\t" + file);
                }

                makeZipFile(zipFilePath, origFiles);
            } catch (IOException ioe) {
                ioe.printStackTrace();
                MipavUtil.displayError("Unable to write original image dataset files to ZIP package:\n"
                        + ioe.getMessage());
                continue;
            }

            // add the name of the zip file, so that it can be included in the XML header History tag
            ndarData.zipFileName = FileUtility.getFileName(zipFilePath);

            // set the valid GUID into the NDAR data object
            ndarData.validGUID = currentGuid;

            String imageName = origImage.getImageFileName();
            HashMap pSetsHashMap = (HashMap)(imagePsets.get(imageName));
            

            writeMetaDataFiles(outputDirBase, outputFileNameBase, imageFile, origImage, pSetsHashMap);

            origImage.disposeLocal();

            printlnToLog("");
        }

        printlnToLog("*** Submission package processing complete. ***");

        nextButton.setEnabled(true);
        previousButton.setEnabled(false);
    }

    /**
     * Writes out the XML meta-information for a given image dataset.
     * 
     * @param outputDir Where to write the XML header.
     * @param outputFileNameBase The prefix to put on the XML header file name.
     * @param imageFile The main image file to use to read in the dataset.
     */
    private void writeMetaDataFiles(String outputDir, String outputFileNameBase, File imageFile, ModelImage image, HashMap pSetsHashMap) {
        // Create the FileIO
        FileIO fileIO = new FileIO();

        // if the dicomsave.dictionary doesn't exist, all the
        if (image.getFileInfo(0) instanceof FileInfoDicom && !DicomDictionary.doesSubsetDicomTagTableExist()) {
            fileIO.setQuiet(false);
        } else {
            fileIO.setQuiet(true);
        }

        FileWriteOptions options = new FileWriteOptions(true);
        String fName;

        options.setMultiFile(multiFileTable.get(imageFile));
        options.setWriteHeaderOnly(true);
        options.setNDARData(ndarData);

        // get the image file name and add .xml to it (maintain the previous extension in the name)
        fName = outputFileNameBase;
        if ( !fName.endsWith(".xml")) {
            fName += ".xml";
        }

        if (image.getNDims() > 2) {
            options.setBeginSlice(0);
            options.setEndSlice(image.getExtents()[2] - 1);
        }
        if (image.getNDims() > 3) {
            options.setBeginSlice(0);
            options.setEndTime(image.getExtents()[3] - 1);
        }
        options.setFileDirectory(outputDir);
        options.setFileName(fName);
        options.setFileType(FileUtility.XML);
        options.doPutInQuicklist(false);
        options.setMultiFile(false);
        options.setPSetsHashMap(pSetsHashMap);
        options.setOptionsSet(true);
        
        
        

        printlnToLog("Saving XML header: " + fName + " to: " + outputDir);

        // write out only the header to userdir/mipav/temp
        fileIO.writeImage(image, options);
    }

    /**
     * Adds a set of files to a ZIP archive.
     * 
     * @param destZipFile The full path to the ZIP archive to create.
     * @param srcFiles A list of files (full paths) to include in the ZIP archive.
     * @throws IOException If there is a problem reading the srcFiles or writing to the ZIP file.
     */
    private void makeZipFile(String destZipFile, List<String> srcFiles) throws IOException {
        // Create a buffer for reading the files
        byte[] buf = new byte[1024];

        // Create the ZIP file
        ZipOutputStream out = new ZipOutputStream(new FileOutputStream(destZipFile));

        // Compress the files
        for (String file : srcFiles) {
            FileInputStream in = new FileInputStream(file);

            // Add ZIP entry to output stream.
            out.putNextEntry(new ZipEntry(FileUtility.getFileName(file)));

            // Transfer bytes from the file to the ZIP file
            int len;
            while ( (len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }

            // Complete the entry
            out.closeEntry();
            in.close();
        }

        // Complete the ZIP file
        out.close();
    }

    /**
     * Append a line to the log output area in the Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    private void printlnToLog(String line) {
        logOutputArea.getTextArea().append(line + "\n");
    }

    private JPanel buildButtonPanel() {
    	
        JPanel buttonPanel1 = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        
        previousButton = WidgetFactory.buildTextButton("Previous", "Go to previous tab", "Previous", this);
        nextButton = WidgetFactory.buildTextButton("Next", "Go to next tab", "Next", this);
        addSourceButton = WidgetFactory.buildTextButton("Add images", "Add image datasets", "AddSource", this);
        removeSourceButton = WidgetFactory.buildTextButton("Remove images", "Remove the selected image datasets", "RemoveSource", this);
        addSetButton= WidgetFactory.buildTextButton("Add Set", "Add Parameter Set", "AddSet", this);
        addParamButton= WidgetFactory.buildTextButton("Add Parameter", "Add Parameter", "AddParam", this);
        editButton= WidgetFactory.buildTextButton("Edit", "Edit", "edit", this);
        deleteButton= WidgetFactory.buildTextButton("Delete", "Delete", "delete", this);
        loadGUIDsButton = WidgetFactory.buildTextButton("Load GUIDs", "Parse GUIDs from text file", "LoadGUIDs", this);
        helpButton = WidgetFactory.buildTextButton("Help", "Show MIPAV help", "Help", this);
        

        previousButton.setPreferredSize(MipavUtil.defaultButtonSize);
        nextButton.setPreferredSize(MipavUtil.defaultButtonSize);
        addSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        addSetButton.setPreferredSize(MipavUtil.defaultButtonSize);
        addParamButton.setPreferredSize(MipavUtil.defaultButtonSize);
        editButton.setPreferredSize(MipavUtil.defaultButtonSize);
        deleteButton.setPreferredSize(MipavUtil.defaultButtonSize);
        loadGUIDsButton.setPreferredSize(MipavUtil.defaultButtonSize);
        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);

        previousButton.setEnabled(false);
        nextButton.setEnabled(false);
        addSourceButton.setEnabled(false);
        removeSourceButton.setEnabled(false);
        addSetButton.setEnabled(false);
        addParamButton.setEnabled(false);
        editButton.setEnabled(false);
        deleteButton.setEnabled(false);
        loadGUIDsButton.setEnabled(false);

        buttonPanel1.add(previousButton,gbc);
        gbc.gridx = 1;
        buttonPanel1.add(nextButton,gbc);
        gbc.gridx = 2;
        buttonPanel1.add(addSourceButton,gbc);
        gbc.gridx = 3;
        buttonPanel1.add(removeSourceButton,gbc);
        gbc.gridx = 4;
        buttonPanel1.add(loadGUIDsButton,gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        buttonPanel1.add(addSetButton,gbc);
        gbc.gridx = 1;
        buttonPanel1.add(addParamButton,gbc);
        gbc.gridx = 2;
        buttonPanel1.add(editButton,gbc);
        gbc.gridx = 3;
        buttonPanel1.add(deleteButton,gbc);
        gbc.gridx = 4;
        buttonPanel1.add(helpButton,gbc);

        return buttonPanel1;
    }

    private class GUIDTableModel extends AbstractTableModel {
        private String[] columnNames = {"Image", "GUID"};

        private Object[][] data;

        public GUIDTableModel(int numGUIDs) {
            super();

            data = new Object[numGUIDs][columnNames.length];

            for (int i = 0; i < numGUIDs; i++) {
                data[i][GUID_TABLE_IMAGE_COLUMN] = new String();
                data[i][GUID_TABLE_GUID_COLUMN] = new String();
            }
        }

        public int getColumnCount() {
            return columnNames.length;
        }

        public int getRowCount() {
            return data.length;
        }

        public String getColumnName(int col) {
            return columnNames[col];
        }

        public Object getValueAt(int row, int col) {
            return data[row][col];
        }

        public boolean isCellEditable(int row, int col) {
            // Note that the data/cell address is constant,
            // no matter where the cell appears onscreen.
            if (col == 0) {
                return false;
            } else {
                return true;
            }
        }

        public void setValueAt(Object value, int row, int col) {
            data[row][col] = value;
            fireTableCellUpdated(row, col);
        }
    }
    
    

    /**
     * launches dialog for adding a new XMLParameter
     * @author pandyan
     *
     */
    private class ParamDialog extends JDialog implements ActionListener, ItemListener {

    	
    	private JPanel mainPanel;
    	
    	private JComboBox paramComboBox, valueTypeComboBox;
    	
    	private JTextArea udTextArea, valueTextArea, descTextArea;
    	
    	private JLabel udLabel, paramComboBoxLabel, descLabel, valueLabel, valueTypeComboBoxLabel;
    	
    	private String setName;
    	
    	private DefaultTreeModel model;
    	
    	private boolean editing;
    	
    	private String psetXMLFilePath = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "pset.xml";
    	
    	private ArrayList<String> predefinedParams = new ArrayList<String>();
    	
    	private HashMap<String,ArrayList> setsParmsMap = new HashMap<String,ArrayList>();

    	private HashMap<String,String> paramsMap = new HashMap<String,String>();
    	
    	private Dialog owner;
    	
    	
    	public ParamDialog(Dialog owner, boolean editing) {
    		super(owner,true);
    		this.owner = owner;
    		if(!editing) {
    			this.setName = currentNode.toString();
    		}else {
    			this.setName = currentNode.getParent().toString();
    		}
    		
    		this.editing = editing;
    		init();
    	}
    	
    	
    	private void readPsetXML() {
    		try {
    			Element setElement;
    			String sName;
    			Element paramElement;
    			SAXBuilder builder = new SAXBuilder();
                Document doc= builder.build(new File(psetXMLFilePath));

    			String paramName;
    			String type;
    			String description;
    			
    			Element root = doc.getRootElement();
    			List setTags = root.getChildren("set");
    			
    			
    			int setTagsCount = setTags.size();
    			for (int i = 0; i < setTagsCount; i++) {
    				setElement = (Element)setTags.get(i);
    				sName = setElement.getAttributeValue("name");
    				List paramTags = setElement.getChildren("parameter");
    				int paramTagsCount = paramTags.size();
    				predefinedParams = new ArrayList<String>();
    				for(int k = 0;k < paramTagsCount; k++) {
    					paramElement = (Element)paramTags.get(k);
    					paramName = paramElement.getAttributeValue("name");
    					predefinedParams.add(paramName);
    					type = paramElement.getAttributeValue("type");
    					description = paramElement.getChild("description").getValue();
    					paramsMap.put(paramName, type+"::"+description);
    				}
    				setsParmsMap.put(sName, predefinedParams);
    				
    			}
    			
    	
    			
    			
    		}catch(Exception e) {

    		}
    	}
    	
    	private ArrayList getPredefinedParams(String sName) {
    		ArrayList params;
    		params = setsParmsMap.get(sName);
    		return params;
    	}
    	
    	
    	private String getType(String paramName) {
    		String type = "";
    		String value = paramsMap.get(paramName);
    		if(value != null) {
    			type = value.substring(0,value.indexOf("::"));
    		}
    		return type;
    	}
    	
    	
    	private String getDescription(String paramName) {
    		String desc = "";
    		String value = paramsMap.get(paramName);
    		if(value != null) {
    			desc = value.substring(value.indexOf("::")+2,value.length());
    		}
    		return desc;
    	}
    	
    	
    	
    	private void init() {
    		 readPsetXML();
    		 setTitle("Add Parameter for " + setName);
             
             model = (DefaultTreeModel)tree.getModel();

             GridBagConstraints gbc = new GridBagConstraints();
             mainPanel = new JPanel(new GridBagLayout());
             
             paramComboBoxLabel = new JLabel("Name");
             
             paramComboBox = new JComboBox();
        	 ArrayList<String> params = getPredefinedParams(setName);
        	 if(params != null) {
                 for(int i=0;i<params.size();i++) {
                	 String item = params.get(i);
                	 paramComboBox.addItem(item);
                 }
        	 }
             paramComboBox.addItem("User Defined");

             paramComboBox.addItemListener(this);
             
            
             
  
             udLabel = new JLabel("User Defined Name");
             udLabel.setEnabled(false);
             valueLabel = new JLabel("Value");
             descLabel = new JLabel("Description");
             descLabel.setEnabled(false);
             valueTypeComboBoxLabel = new JLabel("Value Type");

             
             udTextArea = new JTextArea(3,30);
             udTextArea.setEditable(false);
             udTextArea.setBackground(Color.lightGray);
             udTextArea.setBorder(new LineBorder(Color.gray));
             
          
             
             
             String selected = (String)paramComboBox.getSelectedItem();
             if(selected.equals("User Defined")) {
             	udTextArea.setEditable(true);
             	udTextArea.setBorder(new LineBorder(Color.black));
             	udLabel.setEnabled(true);
             	udTextArea.setBackground(Color.white);
             	
             	
             }else {
            	 udTextArea.setEditable(false);
            	 udLabel.setEnabled(false);
            	 udTextArea.setBorder(new LineBorder(Color.gray));
            	 udTextArea.setText("");
            	 udTextArea.setBackground(Color.lightGray);
            	 
             }
             
             
             
             
             
             
             
             valueTextArea = new JTextArea(3,30);
             valueTextArea.setBorder(new LineBorder(Color.black));
             
             valueTypeComboBox = new JComboBox();
             valueTypeComboBox.addItem("string");
             valueTypeComboBox.addItem("ubyte");
             valueTypeComboBox.addItem("byte");
             valueTypeComboBox.addItem("ushort");
             valueTypeComboBox.addItem("short");
             valueTypeComboBox.addItem("int");
             valueTypeComboBox.addItem("long");
             valueTypeComboBox.addItem("float");
             valueTypeComboBox.addItem("double");
             valueTypeComboBox.addItem("boolean");
             if(selected.equals("User Defined")) {
            	 valueTypeComboBox.setEnabled(true);
             }else {
            	 valueTypeComboBox.setEnabled(false);
            	 String type = getType(selected);
            	 for(int i=0;i<valueTypeComboBox.getItemCount();i++) {
              		if(((String)valueTypeComboBox.getItemAt(i)).equals(type)) {
              			valueTypeComboBox.setSelectedIndex(i);
              		}
            	 }
             }
             

             descTextArea = new JTextArea(3,30);
             descTextArea.setBorder(new LineBorder(Color.gray));
             descTextArea.setEditable(false);
             descTextArea.setBackground(Color.lightGray);
             descTextArea.setForeground(Color.gray);
             if(selected.equals("User Defined")) {
            	 descTextArea.setEditable(true);
            	 descTextArea.setBorder(new LineBorder(Color.black));
            	 descTextArea.setForeground(Color.black);
            	 descTextArea.setText("");
            	 descTextArea.setBackground(Color.white);
            	 descLabel.setEnabled(true);
             }else {
            	 String desc = getDescription(selected);
            	 descTextArea.setText(desc);
             }
             
             
             gbc.gridx = 0;
             gbc.gridy = 0;
             gbc.insets = new Insets(15,15,15,15);
             gbc.anchor = GridBagConstraints.EAST;
             mainPanel.add(paramComboBoxLabel, gbc);
             
             gbc.gridx = 1;
             gbc.gridy = 0;
             gbc.anchor = GridBagConstraints.WEST;
             mainPanel.add(paramComboBox, gbc);
             
             gbc.gridx = 0;
             gbc.gridy = 1;
             gbc.anchor = GridBagConstraints.EAST;
             mainPanel.add(udLabel, gbc);
             
             gbc.gridx = 1;
             gbc.gridy = 1;
             gbc.anchor = GridBagConstraints.WEST;
             mainPanel.add(udTextArea, gbc);
             
             gbc.gridx = 0;
             gbc.gridy = 2;
             gbc.anchor = GridBagConstraints.EAST;
             mainPanel.add(valueLabel, gbc);
             
             gbc.gridx = 1;
             gbc.gridy = 2;
             gbc.anchor = GridBagConstraints.WEST;
             mainPanel.add(valueTextArea, gbc);  
             
             gbc.gridx = 0;
             gbc.gridy = 3;
             gbc.anchor = GridBagConstraints.EAST;
             mainPanel.add(valueTypeComboBoxLabel, gbc);
             
             gbc.gridx = 1;
             gbc.gridy = 3;
             gbc.anchor = GridBagConstraints.WEST;
             mainPanel.add(valueTypeComboBox, gbc);
             
             gbc.gridx = 0;
             gbc.gridy = 4;
             gbc.anchor = GridBagConstraints.EAST;
             mainPanel.add(descLabel, gbc);
             
             gbc.gridx = 1;
             gbc.gridy = 4;
             gbc.anchor = GridBagConstraints.WEST;
             mainPanel.add(descTextArea, gbc);
             
             
             
             JPanel OKCancelPanel = new JPanel();
             buildOKButton();
             OKButton.setActionCommand("ok2");
             OKButton.addActionListener(this);
             OKCancelPanel.add(OKButton, BorderLayout.WEST);
             buildCancelButton();
             cancelButton.setActionCommand("cancel2");
             cancelButton.addActionListener(this);
             OKCancelPanel.add(cancelButton, BorderLayout.EAST);
             
             if(editing) {
             	String currentParamSet = currentNode.toString();
             	int index1 = currentParamSet.indexOf("::");
             	String name = currentParamSet.substring(0, index1);
             	boolean foundMatch = false;
             	for(int i=0;i<paramComboBox.getItemCount();i++) {
             		if(((String)paramComboBox.getItemAt(i)).equals(name)) {
             			paramComboBox.setSelectedIndex(i);
             			foundMatch = true;
             			break;
             		}
             	}
             	if(!foundMatch){
             		//set the comboBox to "user defined"
             		for(int i=0;i<paramComboBox.getItemCount();i++) {
                 		if(((String)paramComboBox.getItemAt(i)).equals("User Defined")) {
                 			paramComboBox.setSelectedIndex(i);
                 			udTextArea.setText(name);
                 			break;
                 		}
                 	}
             	}
             	
             	
             	
             	
             	currentParamSet = currentParamSet.substring(currentParamSet.indexOf("::")+2,currentParamSet.length());
             	index1 = currentParamSet.indexOf("::");
             	String val = currentParamSet.substring(0, index1);
             	//since val starts off as value="blah"...get the substring
             	val = val.substring(6,val.length());
             	valueTextArea.setText(val);
             	
             	
             	currentParamSet = currentParamSet.substring(currentParamSet.indexOf("::")+2,currentParamSet.length());
             	index1 = currentParamSet.indexOf("::");
             	val = currentParamSet.substring(0, index1);
             	//since type starts off as type="blah"...get the substring
             	val = val.substring(5,val.length());
             	for(int i=0;i<valueTypeComboBox.getItemCount();i++) {
             		if(((String)valueTypeComboBox.getItemAt(i)).equals(val)) {
             			valueTypeComboBox.setSelectedIndex(i);
             			break;
             		}
             	}
             	if(((String)paramComboBox.getSelectedItem()).equals("User Defined")) {
             		valueTypeComboBox.setEnabled(true);
             	}else {
             		valueTypeComboBox.setEnabled(false);
             	}
             	
             	
             	
             	
             	val = currentParamSet.substring(currentParamSet.indexOf("::")+2,currentParamSet.length());

                //since desc starts off as description="blah"...get the substring
             	val = val.substring(12,val.length());
             	descTextArea.setText(val);
             	
             	
             }
             
             getContentPane().add(mainPanel,BorderLayout.CENTER);
             getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
             
             
             
             pack();
             MipavUtil.centerInWindow(owner, this);
             setResizable(false);
             setVisible(true);
    	}
    	
    	
    	
    	
		public void actionPerformed(ActionEvent e) {
			String command = e.getActionCommand();
			if(command.equals("ok2")) {
				if(!validateValue()) {
					MipavUtil.displayError("The value entered does not match its value type");
					return;
				}
				if(!editing) {
					String paramName;
					if(((String)paramComboBox.getSelectedItem()).equals("User Defined")) {
						paramName = udTextArea.getText().trim();
						if(paramName.equals("")) {
							//display error and reurn
							MipavUtil.displayError("You must enter a parameter name when choosing user defined");
							return;
						}
					}else {
						paramName = (String)paramComboBox.getSelectedItem();
					}
					if(valueTextArea.getText().trim().equals("")) {
						MipavUtil.displayError("You must enter a value");
						return;
					}
					DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode)currentNode.getParent(); 
           		    String imageName = parentNode.toString();
					HashMap pSetsHashMap = (HashMap)(imagePsets.get(imageName));
					String currentDescription = (String)currentNode.toString();
					XMLPSet p = (XMLPSet)pSetsHashMap.get(currentDescription);
					String value = valueTextArea.getText().trim();
					String desc = descTextArea.getText().trim();
					String type = (String)valueTypeComboBox.getSelectedItem();
					if(p.getParameter(paramName) == null) {
						p.addParameter(paramName);
						p.getParameter(paramName).setValue(value);
						p.getParameter(paramName).setDescription(desc);
						p.getParameter(paramName).setValueType(type);

						paramName = paramName + "::value=" + value + "::type=" + type + "::description=" + desc;
						DefaultMutableTreeNode paramNode = new DefaultMutableTreeNode(paramName);
						model.insertNodeInto(paramNode, currentNode, currentNode.getChildCount());
		    		    for (int i = 0; i < tree.getRowCount(); i++) {
		       	         tree.expandRow(i);
		       	       }
						
					}else {
						//inform that param already exists with this name
						MipavUtil.displayError("There is already a parameter of that name");
						return;
					}
					
					
				}else {
					//we are editing!
					//we are in editing mode
					String newParamName;
					if(((String)paramComboBox.getSelectedItem()).equals("User Defined")) {
						newParamName = udTextArea.getText().trim();
						if(newParamName.equals("")) {
							//display error and reurn
							MipavUtil.displayError("You must enter a parameter name when choosing user defined");
							return;
						}
					}else {
						newParamName = (String)paramComboBox.getSelectedItem();
					}
					if(valueTextArea.getText().trim().equals("")) {
						MipavUtil.displayError("You must enter a value");
						return;
					}
					if(descTextArea.getText().trim().equals("")) {
						MipavUtil.displayError("You must enter a description");
						return;
					}
					
					
					
					
					DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode)currentNode.getParent();
	            	DefaultMutableTreeNode grandParentNode = (DefaultMutableTreeNode)parentNode.getParent(); 
	                String imageName = grandParentNode.toString();
					HashMap pSetsHashMap = (HashMap)(imagePsets.get(imageName));
					String psetDescription = (String)parentNode.toString();
					XMLPSet p = (XMLPSet)pSetsHashMap.get(psetDescription);
					String currentParam = currentNode.toString();
					int index1 = currentParam.indexOf("::");
					currentParam = currentParam.substring(0, index1);
					p.removeParameter(currentParam);
					String value = valueTextArea.getText().trim();
					String desc = descTextArea.getText().trim();
					String type = (String)valueTypeComboBox.getSelectedItem();
					p.addParameter(newParamName);
					p.getParameter(newParamName).setValue(value);
					p.getParameter(newParamName).setDescription(desc);
					p.getParameter(newParamName).setValueType(type);

					newParamName = newParamName + "::value=" + value + "::type=" + type + "::description=" + desc;
					currentNode.setUserObject(newParamName);
					tree.repaint();
					repaint();
					
					
					
				}

				dispose();
			}else if(command.equals("cancel2")) {
				dispose();
			}
			
		}
		
		
		private boolean validateValue() {
			boolean success = true;
			String type = (String)valueTypeComboBox.getSelectedItem();
			String value = valueTextArea.getText().trim();
			if(type.equals("ubyte")) {
				try{
					short s = Short.valueOf(value);
					if(s<0 || s>255) {
						return false;
					}
				}catch(NumberFormatException e) {
					success = false;
				}
			}else if(type.equals("byte")) {
				try{
					Byte.valueOf(value);
				}catch(NumberFormatException e) {
					success = false;
				}
			}else if(type.equals("ushort")) {
				try{
					double d = Double.valueOf(value);
					if((d<0)|| (d>65535)) {
						return false;
					}
				}catch(NumberFormatException e) {
					success = false;
				}
			}else if(type.equals("short")) {
				try{
					Short.valueOf(value);
				}catch(NumberFormatException e) {
					success = false;
				}
			}else if(type.equals("int")) {
				try{
					Integer.valueOf(value);
				}catch(NumberFormatException e) {
					success = false;
				}
			}else if(type.equals("long")) {
				try{
					Long.valueOf(value);
				}catch(NumberFormatException e) {
					success = false;
				}
			}else if(type.equals("float")) {
				try{
					Float.valueOf(value);
				}catch(NumberFormatException e) {
					success = false;
				}
			}else if(type.equals("double")) {
				try{
					Double.valueOf(value);
				}catch(NumberFormatException e) {
					success = false;
				}
			}else if(type.equals("boolean")) {
				if(!(value.equalsIgnoreCase("true") || value.equalsIgnoreCase("false"))) {
					return false;
				}
			}
			
			return success;
		}

		public void itemStateChanged(ItemEvent e) {
			Object source = e.getSource();
			if (e.getStateChange() == ItemEvent.SELECTED) {
				if (source == paramComboBox) {
		            String selected = (String)paramComboBox.getSelectedItem();
		            if(selected.equals("User Defined")) {
		            	udTextArea.setEditable(true);
		            	udTextArea.setBorder(new LineBorder(Color.black));
		            	udLabel.setEnabled(true);
		            	udTextArea.setBackground(Color.white);
		            	valueTypeComboBox.setEnabled(true);
		            	descTextArea.setEditable(true);
		            	descTextArea.setForeground(Color.black);
		            	descTextArea.setBorder(new LineBorder(Color.black));
		                descTextArea.setBackground(Color.white);
		                descTextArea.setText("");
		                descLabel.setEnabled(true);
		                valueTextArea.setText("");

		            }else {
		            	udTextArea.setEditable(false);
		            	udTextArea.setBorder(new LineBorder(Color.gray));
		            	udLabel.setEnabled(false);
		            	udTextArea.setText("");
		            	udTextArea.setBackground(Color.lightGray);
		            	String type = getType(selected);
		            	 for(int i=0;i<valueTypeComboBox.getItemCount();i++) {
		              		if(((String)valueTypeComboBox.getItemAt(i)).equals(type)) {
		              			valueTypeComboBox.setSelectedIndex(i);
		              		}
		            	 }
		            	valueTypeComboBox.setEnabled(false);
		            	descTextArea.setEditable(false);
		            	descTextArea.setForeground(Color.gray);
		            	descTextArea.setBorder(new LineBorder(Color.gray));
		                descTextArea.setBackground(Color.lightGray);
		                String desc = getDescription(selected);
		            	descTextArea.setText(desc);
		            	descLabel.setEnabled(false);
		            	valueTextArea.setText("");

		            }
		            
				}
				
			}
			
		}
    	
    }
    
    
    

    
    
    
    /**
     * launches the dialog to add a new XMLPSet
     * @author pandyan
     *
     */
    private class PSetDialog extends JDialog implements ActionListener, ItemListener {
    	
    	private JPanel mainPanel;
    	
    	private JComboBox psetComboBox;
    	
    	private JTextArea udTextArea;
    	
    	private JLabel comboBoxLabel, udLabel;
    	
    	private String imageName;
    	
    	private DefaultTreeModel model;
    	
    	private boolean editing;
    	
    	private String psetXMLFilePath = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "pset.xml";
    	
    	private ArrayList<String> predefinedPsets = new ArrayList<String>();
    	
    	private Dialog owner;
    	
    	public PSetDialog(Dialog owner, boolean editing) {
    		super(owner,true);
    		this.owner = owner;
    		if(!editing) {
    			this.imageName = currentNode.toString();
    		}else {
    			this.imageName = currentNode.getParent().toString();
    		}
    		
    		this.editing = editing;
    		init();
    	}
    	
    	private void readPsetXML() {
    		try {
    			Element setElement;
    			String setName;
    			SAXBuilder builder = new SAXBuilder();
                Document doc= builder.build(new File(psetXMLFilePath));

                Element root = doc.getRootElement();
    			List setTags = root.getChildren("set");

    			int setNodeCount = setTags.size();
    			for (int i = 0; i < setNodeCount; i++) {
    				setElement = (Element)setTags.get(i);
    				setName = setElement.getAttributeValue("name");
    				predefinedPsets.add(setName);
    			}
    			
    			
    			
    		}catch(Exception e) {
    			
    		}
    	}

    	
    	private void init() {
    		readPsetXML();
            setTitle("Add Set for " + imageName);
            
            model = (DefaultTreeModel)tree.getModel();

            GridBagConstraints gbc = new GridBagConstraints();
            mainPanel = new JPanel(new GridBagLayout());
            
            comboBoxLabel = new JLabel("Name");
            
            psetComboBox = new JComboBox();
            for(int i=0;i<predefinedPsets.size();i++) {
            	String item = predefinedPsets.get(i);
            	psetComboBox.addItem(item);
            }
            psetComboBox.addItem("User Defined");
            psetComboBox.addItemListener(this);
            
 
            udLabel = new JLabel("User Defined Name");
            udLabel.setEnabled(false);

            
            udTextArea = new JTextArea(3,30);
            udTextArea.setEditable(false);
            udTextArea.setBackground(Color.lightGray);
            udTextArea.setBorder(new LineBorder(Color.gray));
            
            String selected = (String)psetComboBox.getSelectedItem();
            if(selected.equals("User Defined")) {
            	udTextArea.setEditable(true);
            	udTextArea.setBorder(new LineBorder(Color.black));
            	udLabel.setEnabled(true);
            	udTextArea.setBackground(Color.white);
            	
            }else {
            	udTextArea.setEditable(false);
            	udLabel.setEnabled(false);
            	udTextArea.setBackground(Color.lightGray);
            	udTextArea.setBorder(new LineBorder(Color.gray));
            	
            }
            
            
            
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.insets = new Insets(15,15,15,15);
            gbc.anchor = GridBagConstraints.EAST;
            mainPanel.add(comboBoxLabel, gbc);
            
            gbc.gridx = 1;
            gbc.gridy = 0;
            gbc.anchor = GridBagConstraints.WEST;
            mainPanel.add(psetComboBox, gbc);
            
            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.anchor = GridBagConstraints.EAST;
            mainPanel.add(udLabel, gbc);
            
            gbc.gridx = 1;
            gbc.gridy = 1;
            gbc.anchor = GridBagConstraints.WEST;
            mainPanel.add(udTextArea, gbc);
            
            
            JPanel OKCancelPanel = new JPanel();
            buildOKButton();
            OKButton.setActionCommand("ok2");
            OKButton.addActionListener(this);
            OKCancelPanel.add(OKButton, BorderLayout.WEST);
            buildCancelButton();
            cancelButton.setActionCommand("cancel2");
            cancelButton.addActionListener(this);
            OKCancelPanel.add(cancelButton, BorderLayout.EAST);
            
            if(editing) {
            	String currentPSet = currentNode.toString();
            	boolean foundMatch = false;
            	for(int i=0;i<psetComboBox.getItemCount();i++) {
            		if(((String)psetComboBox.getItemAt(i)).equals(currentPSet)) {
            			psetComboBox.setSelectedIndex(i);
            			foundMatch = true;
            			break;
            		}
            	}
            	if(!foundMatch){
            		//set the comboBox to "user defined"
            		for(int i=0;i<psetComboBox.getItemCount();i++) {
                		if(((String)psetComboBox.getItemAt(i)).equals("User Defined")) {
                			psetComboBox.setSelectedIndex(i);
                			udTextArea.setText(currentPSet);
                			break;
                		}
                	}
            		
            	}
            }
            
            getContentPane().add(mainPanel,BorderLayout.CENTER);
            getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
            
            
            
            pack();
            MipavUtil.centerInWindow(owner, this);
            setResizable(false);
            setVisible(true);
            
            
    		
    	}
    	
    	
    	
		public void actionPerformed(ActionEvent e) {
			String command = e.getActionCommand();
			if(command.equals("ok2")) {
				if(!editing) {
					String name;
					if(((String)psetComboBox.getSelectedItem()).equals("User Defined")) {
						name = udTextArea.getText().trim();
						if(name.equals("")) {
							//display error and reurn
							MipavUtil.displayError("You must enter a set name when choosing user defined");
							return;
						}
					}else {
						name = (String)psetComboBox.getSelectedItem();
					}
					
					HashMap pSetsHashMap = (HashMap)(imagePsets.get(imageName));
					if(pSetsHashMap == null) {
						pSetsHashMap = new HashMap();
						XMLPSet p = new XMLPSet(name);
						pSetsHashMap.put(name, p);
						imagePsets.put(imageName, pSetsHashMap);
						DefaultMutableTreeNode psetNode = new DefaultMutableTreeNode(name);
						model.insertNodeInto(psetNode, currentNode, currentNode.getChildCount());
		    		    for (int i = 0; i < tree.getRowCount(); i++) {
		       	         tree.expandRow(i);
		       	       }
						
					}else {
	
						XMLPSet p = (XMLPSet)pSetsHashMap.get(name);
						if(p == null) {
							p = new XMLPSet(name);
							pSetsHashMap.put(name, p);
							//imagePsets.put(imageName, pSetsHashMap);
							DefaultMutableTreeNode psetNode = new DefaultMutableTreeNode(name);
							//currentNode.add(psetNode);
							model.insertNodeInto(psetNode, currentNode, currentNode.getChildCount());
			    		    for (int i = 0; i < tree.getRowCount(); i++) {
				       	         tree.expandRow(i);
				       	       }
						}else {
							//inform user that there is already a PSet with that name
							MipavUtil.displayError("There is already a set of that name");
							return;
						}
					}
				}else {
					//we are in editing mode
					String newName;
					if(((String)psetComboBox.getSelectedItem()).equals("User Defined")) {
						newName = udTextArea.getText().trim();
						if(newName.equals("")) {
							//display error and reurn
							MipavUtil.displayError("You must enter a set name when choosing user defined");
							return;
						}
					}else {
						newName = (String)psetComboBox.getSelectedItem();
					}
					HashMap pSetsHashMap = (HashMap)(imagePsets.get(imageName));
					String currentDescription = (String)currentNode.toString();
					XMLPSet p = (XMLPSet)pSetsHashMap.get(currentDescription);
					p.setDescription(newName);
					pSetsHashMap.remove(currentDescription);
					pSetsHashMap.put(newName, p);
					currentNode.setUserObject(newName);
					tree.repaint();
					repaint();
					
					
				}
				dispose();
			}else if(command.equals("cancel2")) {
				dispose();
			}
			
		}


		public void itemStateChanged(ItemEvent e) {
			Object source = e.getSource();
			if (e.getStateChange() == ItemEvent.SELECTED) {
				if (source == psetComboBox) {
		            String selected = (String)psetComboBox.getSelectedItem();
		            if(selected.equals("User Defined")) {
		            	udTextArea.setEditable(true);
		            	udLabel.setEnabled(true);
		            	udTextArea.setBackground(Color.white);
		            	udTextArea.setBorder(new LineBorder(Color.black));
		            }else {
		            	udTextArea.setEditable(false);
		            	udLabel.setEnabled(false);
		            	udTextArea.setText("");
		            	udTextArea.setBackground(Color.lightGray);
		            	udTextArea.setBorder(new LineBorder(Color.gray));
		            }
				}
			}
		}	
    }
    
    
    private class MyTreeRenderer extends DefaultTreeCellRenderer {
    	
    	public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
    		Component comp = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);

    		if(((DefaultMutableTreeNode)value).getPath().length == 4) {
    			setIcon(this.leafIcon);
    		}else {
    			setIcon(this.openIcon);
    		}
    		
    		
    		return comp;
    	}
    	
    	
    }
    
    
}
