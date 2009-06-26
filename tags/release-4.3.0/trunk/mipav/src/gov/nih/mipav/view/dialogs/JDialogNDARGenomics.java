package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.srb.SRBFileTransferer;
import gov.nih.mipav.model.srb.SRBUtility;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.srb.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;
import java.util.zip.*;
import java.io.*;

import javax.swing.*;
import javax.swing.event.*;

import edu.sdsc.grid.io.*;
import edu.sdsc.grid.io.local.*;
import edu.sdsc.grid.io.srb.*;

/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */

public class JDialogNDARGenomics extends JDialogBase implements ActionListener, ChangeListener, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------


    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private JTextField privateField;
	
	private JTextField irbField, abstractTitleField;
	
	private JTextField piNameField, piEmailField, piPhoneField, piTitleField;
	
	/** Scrolling text area for abstract */
	private WidgetFactory.ScrollTextArea abstractArea;
	
	/** Scrolling text area for log output */
	private WidgetFactory.ScrollTextArea logOutputArea;
	
	JScrollPane metaListPane, rawListPane;
	private JList metaSourceList, rawSourceList;
	private DefaultListModel metaSourceModel, rawSourceModel;
	
	private JButton nextButton, previousButton, addMetaSourceButton, addRawSourceButton,
		removeSourceButton;
	
	private JButton privateBrowseButton, openAbstractButton;
	
	private JRadioButton publicButton, privateButton;
	
	private JTabbedPane tabbedPane;
	private JCheckBox anonConfirmBox;
	
	private JTextArea privacyTextArea;
		
	/** Static tab indices */
	private static final int TAB_MAIN = 0;
	private static final int TAB_PI = 1;
	private static final int TAB_ABSTRACT = 2;
	private static final int TAB_SOURCE = 3;
	private static final int TAB_DESTINATION = 4;
	
	private static final String SPACE = "";

	private static final String USER_MIPAV_TEMP_DIR = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "temp" +
    	File.separator;
	
	private static final String REPOSITORY_SUBMISSION_FILENAME = "submission_info.xml";
	
	private static final String [] ORGANIZATIONS = { "ACE Location 1",
													 "ACE Location 2",
													 "ACE Location 3",
													 "ACE Location 4",
													 "ACE Location 5",
													 "ACE Location 6",
													 "ACE Location 7",
													 "ACE Location 8",
													 "ACE Location 9",
													 "ACE Location 10" };
	
	
    //~ Constructors ---------------------------------------------------------------------------------------------------

  
    public JDialogNDARGenomics(Frame theParentFrame) {
        super(theParentFrame, false);
        
        init();
        setVisible(true);
        
        validate();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {

        String command = e.getActionCommand();
        
        if (command.equals("Next")) {
        	int index = tabbedPane.getSelectedIndex();
        	
        	if (index == TAB_DESTINATION) {
        		if (createXML()) {
        			transfer();
        		}
        		
        	}  else if (tabbedPane.getTabCount() > index + 1) {
        		tabbedPane.setSelectedIndex(index + 1);
        	}
        	        	
        } else if (command.equals("Previous")) {
        	int index = tabbedPane.getSelectedIndex();
        	
        	if (index > 0) {
        		tabbedPane.setSelectedIndex(index - 1);
        	}
        } else if (command.equals("AddMetaSource")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            chooser.setMultiSelectionEnabled(true);
            
            int returnVal = chooser.showOpenDialog(null);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File[] files = chooser.getSelectedFiles();
                ViewUserInterface.getReference().setDefaultDirectory(files[0].getParent());
                for (int i = 0; i < files.length; i++) {
                	if (!metaSourceModel.contains(files[i])) {
                		metaSourceModel.addElement(files[i]);  
                	}
                }
            }
            removeSourceButton.setEnabled(metaSourceModel.size() > 0);
            metaListPane.setBorder(buildTitledBorder(metaSourceModel.size() + " meta-file(s) selected for transfer"));            
        } else if (command.equals("AddRawSource")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            chooser.setMultiSelectionEnabled(true);
            
            int returnVal = chooser.showOpenDialog(null);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File[] files = chooser.getSelectedFiles();
                ViewUserInterface.getReference().setDefaultDirectory(files[0].getParent());
                for (int i = 0; i < files.length; i++) {
                	if (!rawSourceModel.contains(files[i])) {
                		rawSourceModel.addElement(files[i]);  
                	}
                }
            }
            removeSourceButton.setEnabled(rawSourceModel.size() > 0);            
            rawListPane.setBorder(buildTitledBorder(rawSourceModel.size() + " raw-file(s) selected for transfer"));            
        }  else if (command.equals("RemoveSource")) {
        	int [] selected = metaSourceList.getSelectedIndices();
        	for (int i = selected.length - 1; i >= 0; i--) {
        		metaSourceModel.removeElementAt(selected[i]);
        	}
        	
        	selected = rawSourceList.getSelectedIndices();
        	for (int i = selected.length - 1; i >= 0; i--) {
        		rawSourceModel.removeElementAt(selected[i]);
        	}
        	
        	removeSourceButton.setEnabled(metaSourceModel.size() > 0 && rawSourceModel.size() > 0);
        	metaListPane.setBorder(buildTitledBorder(metaSourceModel.size() + " meta-file(s) selected for transfer"));
        	rawListPane.setBorder(buildTitledBorder(rawSourceModel.size() + " raw-file(s) selected for transfer"));
        } else if (command.equals("Source")) {
        	tabbedPane.setSelectedIndex(TAB_SOURCE);
        } else if (command.equals("LoadAbstract")) {
        	JFileChooser chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(false);
            
            chooser.setFont(MipavUtil.defaultMenuFont);
            
            int returnVal = chooser.showOpenDialog(null);
            
            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	loadAbstract(chooser.getSelectedFile());
            	
            }
        } else if (command.equals("Browse")) {
        	browseSRB();
        } else if (command.equals("LoadAbstract")) {
        	JFileChooser chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(false);
            
            chooser.setFont(MipavUtil.defaultMenuFont);
            
            int returnVal = chooser.showOpenDialog(null);
            
            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	loadAbstract(chooser.getSelectedFile());
            	
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
    		addMetaSourceButton.setEnabled(true);
    		addRawSourceButton.setEnabled(true);
    		removeSourceButton.setEnabled(metaSourceModel.size() > 0 || rawSourceModel.size() > 0);
    	} else {
    		addMetaSourceButton.setEnabled(false);
    		addRawSourceButton.setEnabled(false);
    		removeSourceButton.setEnabled(false);
    	}
    	
    	
    }
   
    public void itemStateChanged(ItemEvent e) {
    	if (e.getSource().equals(anonConfirmBox)) {
    		if (anonConfirmBox.isSelected()) {
    			anonConfirmBox.setEnabled(false);
    			nextButton.setEnabled(true);
    			
    			tabbedPane.setEnabledAt(TAB_SOURCE, true);
    			tabbedPane.setEnabledAt(TAB_DESTINATION, true);
    			tabbedPane.setEnabledAt(TAB_PI, true);
    			tabbedPane.setEnabledAt(TAB_ABSTRACT, true);
    			privacyTextArea.setBackground(helpButton.getBackground());
    		}
    	} else if (e.getSource().equals(privateButton)) {
    		privateBrowseButton.setEnabled(privateButton.isSelected());
			privateField.setEnabled(privateButton.isSelected());
    	}
    }
    
    private boolean createXML() {
    	
    	if (!new File(USER_MIPAV_TEMP_DIR).exists()) {
    		new File(USER_MIPAV_TEMP_DIR).mkdirs();
    	}
    	
    	
    	
    	try {
    		RepositoryXML repoXML = new RepositoryXML(REPOSITORY_SUBMISSION_FILENAME, USER_MIPAV_TEMP_DIR );
    		repoXML.writeXML();
    	} catch (Exception e) {
    		e.printStackTrace();
    		return false;
    	}
    	
    	return true;
    }
    
    private void browseSRB() {
    	if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
            new JDialogLoginSRB("Connect to", false);

            if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
                return;
            }
        }


        /**
         * Uses the JargonFileChooser to retrieve the file that the user wants to open.
         */
        JargonFileChooser chooser = null;

        try {
            chooser = new JargonFileChooser(JDialogLoginSRB.srbFileSystem);
        } catch (OutOfMemoryError e) {
            e.printStackTrace(System.err);
            MipavUtil.displayError("Out of memory!");

            return;
        } catch (IOException e) {
            e.printStackTrace(System.err);
            MipavUtil.displayError(e.getMessage());

            return;
        }

        chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
        chooser.setMultiSelectionEnabled(false);

        int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

        if (returnValue == JargonFileChooser.APPROVE_OPTION) {

            /**
             * According to the files selected by user, tries to create the srb file list.
             */
            SRBFile[] files = chooser.getSelectedFiles();
            privateField.setText(SRBUtility.convertToString(files));
        }
    }

    private void loadAbstract(File abstractFile) {
    	RandomAccessFile raFile;
    	try {
    		raFile = new RandomAccessFile(abstractFile, "r");
    		String tempStr = null;
    		do {
    			
    			tempStr = raFile.readLine();
    			if (tempStr != null) {
    				abstractArea.getTextArea().append(tempStr + "\n");
    			}
    		} while (tempStr != null);
    		
    	} catch (IOException e) {
    		
    	}
    }
    
    /**
     * DOCUMENT ME!
     */
    private void init() {
        setTitle("NDAR Genomics file import tool");
        
        tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Main", buildMainTab());
        tabbedPane.addTab("P.I.", buildPITab()); 
        tabbedPane.addTab("Abstract", buildAbstractPanel());
        tabbedPane.addTab("Source", buildSourcePanel());
        tabbedPane.addTab("Destination", buildDestinationPanel());
        
        tabbedPane.setEnabledAt(TAB_SOURCE, false);
        tabbedPane.setEnabledAt(TAB_DESTINATION, false);
        tabbedPane.setEnabledAt(TAB_PI, false);
        tabbedPane.setEnabledAt(TAB_ABSTRACT, false);
        
        tabbedPane.addChangeListener(this);
        
        getContentPane().add(tabbedPane);
        getContentPane().add(buildButtonPanel(), BorderLayout.SOUTH);
        pack();
        validate();
        this.setMinimumSize(new Dimension(610,437));
        this.setSize(new Dimension(610,437));
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
     * build Principal Investor
     * @return JPanel
     */
    private JPanel buildPITab() {
    	JPanel piPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        Insets insets1 = new Insets(2, 10, 2,0);
        Insets insets2 = new Insets(2, 10, 2, 10);
        
        JLabel piNameLabel = WidgetFactory.buildLabel("Name");
        gbc.insets = insets1;
        piPanel.add(piNameLabel, gbc);
        
        gbc.weightx = 1;
        piNameField = WidgetFactory.buildTextField(SPACE);
        gbc.gridx++;
        gbc.insets = insets2;
        piPanel.add(piNameField, gbc);
    	
        gbc.gridx = 0;
        gbc.gridy++;
        JLabel piTitleLabel = WidgetFactory.buildLabel("Title");
        gbc.insets = insets1;
        piPanel.add(piTitleLabel, gbc);
        
        gbc.weightx = 1;
        piTitleField = WidgetFactory.buildTextField(SPACE);
        gbc.gridx++;
        gbc.insets = insets2;
        piPanel.add(piTitleField, gbc);
        
        
        JLabel piEmailLabel = WidgetFactory.buildLabel("Email");
        gbc.weightx = 0;
        gbc.gridx =0;
        gbc.gridy++;
        gbc.insets = insets1;
        piPanel.add(piEmailLabel, gbc);
        
        piEmailField = WidgetFactory.buildTextField(SPACE);
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.insets = insets2;
        piPanel.add(piEmailField, gbc);
        
        JLabel piPhoneLabel = WidgetFactory.buildLabel("Phone");
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        gbc.insets = insets1;
        piPanel.add(piPhoneLabel, gbc);
        
        piPhoneField = WidgetFactory.buildTextField(SPACE);
    	gbc.gridx++;
    	gbc.weightx = 1;
    	gbc.insets = insets2;
    	piPanel.add(piPhoneField, gbc);
        
    	
    	gbc.gridx= 0;
    	gbc.gridy++;
    	gbc.weightx = 0;
    	 gbc.insets = insets1;
    	JLabel irbLabel = WidgetFactory.buildLabel("IRB #");
    	piPanel.add(irbLabel, gbc);
        
        gbc.gridx++;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.insets = insets2;
        irbField = WidgetFactory.buildTextField(SPACE);
        piPanel.add(irbField, gbc);              
               	
    	return piPanel;
    }
    
    private JPanel buildAbstractPanel() {
    	JPanel abstractTitlePanel = new JPanel(new GridBagLayout());
    	GridBagConstraints gbc = new GridBagConstraints();
    	
    	gbc.anchor = GridBagConstraints.NORTHWEST;
    	gbc.gridx = 0;
    	gbc.gridy = 0;
    	gbc.weightx = 0;
    	gbc.weighty = 0;
    	gbc.gridwidth = 1;
    	gbc.gridheight = 1;
    	gbc.fill = GridBagConstraints.HORIZONTAL;
    	
    	JLabel abstractLabel = WidgetFactory.buildLabel("Abstract title");
    	gbc.insets = new Insets(0, 10, 0, 0);
    	abstractTitlePanel.add(abstractLabel, gbc);
    	
    	gbc.insets = new Insets(0, 0, 0, 0);
    	gbc.gridx++;
    	gbc.gridwidth = 2;
    	gbc.weightx = 1;
    	abstractTitleField = WidgetFactory.buildTextField(SPACE);
    	abstractTitlePanel.add(abstractTitleField, gbc);
    	
    	openAbstractButton = WidgetFactory.buildTextButton("Load from file", "Load abstract from text file", "LoadAbstract", this);
    	gbc.gridwidth = 1;
    	gbc.weightx = .5;
    	gbc.gridx+=2;
    	abstractTitlePanel.add(openAbstractButton, gbc);
    	
    	
    	JPanel abstractPanel = new JPanel(new BorderLayout());
    	
    	abstractArea = WidgetFactory.buildScrollTextArea(Color.white);
    	abstractArea.getTextArea().setBorder(buildTitledBorder("Summary"));    	
    	abstractPanel.add(abstractArea);
    	abstractPanel.add(abstractTitlePanel, BorderLayout.NORTH);
    	
    	return abstractPanel;
    }
    
    private JPanel buildSourcePanel() {
    	JPanel sourcePanel = new JPanel(new GridBagLayout());
    	GridBagConstraints gbc = new GridBagConstraints();
    	
    	gbc.anchor = GridBagConstraints.NORTHWEST;	
    	gbc.gridx = 0; gbc.gridy = 0;
    	gbc.weightx = 1;
    	gbc.weighty = 1;
    	gbc.fill = GridBagConstraints.BOTH;
    	
    	metaSourceModel = new DefaultListModel();
    	metaSourceList = new JList(metaSourceModel);
    	
    	metaListPane = WidgetFactory.buildScrollPane(metaSourceList);
    	metaListPane.setBorder(buildTitledBorder(0 + " meta-file(s) selected for transfer"));
    	
    	rawSourceModel = new DefaultListModel();
    	rawSourceList = new JList(rawSourceModel);
    	
    	rawListPane = WidgetFactory.buildScrollPane(rawSourceList);
    	rawListPane.setBorder(buildTitledBorder(0 + " raw-file(s) selected for transfer"));
    	
    	sourcePanel.add(metaListPane, gbc);
    	gbc.gridx++;
    	sourcePanel.add(rawListPane, gbc);
    	
    	return sourcePanel;
    }
    
    private JPanel buildDestinationPanel() {
    	JPanel destPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.weightx = 1;
        gbc2.gridy = 0;
        gbc2.gridx = 0;
        
        JPanel visPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();        
        
        ButtonGroup group = new ButtonGroup();
        publicButton = WidgetFactory.buildRadioButton("Public", true, group);
        privateButton = WidgetFactory.buildRadioButton("Private", true, group);
        privateButton.addItemListener(this);
        
        privateField = WidgetFactory.buildTextField("");
        privateBrowseButton = WidgetFactory.buildTextButton("Browse", "Browse for directory on SRB", "Browse", this);
        
        privateField.setEnabled(false);
        privateBrowseButton.setEnabled(false);
        
        gbc.weightx = 0;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        
        gbc.fill = GridBagConstraints.NONE;
        visPanel.add(publicButton, gbc);
        
        gbc.gridy = 1;
        visPanel.add(privateButton, gbc);
        
        gbc.gridx++;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.insets = new Insets(0,5,0,5);
        visPanel.add(privateField, gbc);
        
        gbc.insets = new Insets(0,0,0,0);
        gbc.weightx = 0;
        gbc.gridx+=3;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        visPanel.add(privateBrowseButton, gbc);
        
        
        visPanel.setBorder(buildTitledBorder("Destination visibility"));

        gbc2.gridy++;
        destPanel.add(visPanel, gbc2);
        
        logOutputArea = WidgetFactory.buildScrollTextArea(Color.white);
        logOutputArea.setBorder(buildTitledBorder("Output log")); 
        logOutputArea.getTextArea().setEditable(false);
    	
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.gridy++;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
      //  gbc2.gridheight = 4;
        destPanel.add(logOutputArea, gbc2);
        
        
    	return destPanel;
    	
    }
    
   
    /**
     * Open the file(s), save header off to local temp folder
     * transfer the file(s) to the given destination
     *
     */
    private void transfer() {
    	int numFiles = metaSourceModel.size();
    	
    	
        // Create a buffer for reading the files
        byte[] buf = new byte[1024];
        
        try {
            // Create the ZIP file
        	long currentTime  = System.currentTimeMillis();
            String outFilename = USER_MIPAV_TEMP_DIR + piNameField.getText() + "_" + currentTime + ".zip";
            ZipOutputStream out = new ZipOutputStream(new FileOutputStream(outFilename));
        
            //compress the repository submission xml
            FileInputStream in = new FileInputStream(USER_MIPAV_TEMP_DIR + REPOSITORY_SUBMISSION_FILENAME);
            
            // Add ZIP entry to output stream.
            out.putNextEntry(new ZipEntry(REPOSITORY_SUBMISSION_FILENAME));
    
            // Transfer bytes from the file to the ZIP file
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
    
            // Complete the entry
            out.closeEntry();
            in.close();
            
            // Compress meta files
            for (int i=0; i < numFiles; i++) {
                in = new FileInputStream((File)metaSourceModel.elementAt(i));
        
                // Add ZIP entry to output stream.
                out.putNextEntry(new ZipEntry(((File)metaSourceModel.elementAt(i)).getName()));
        
                // Transfer bytes from the file to the ZIP file
                while ((len = in.read(buf)) > 0) {
                    out.write(buf, 0, len);
                }
        
                // Complete the entry
                out.closeEntry();
                in.close();
            }
            
            numFiles = rawSourceModel.size();
            
//          Compress raw files
            for (int i=0; i < numFiles; i++) {
                in = new FileInputStream((File)rawSourceModel.elementAt(i));
        
                // Add ZIP entry to output stream.
                out.putNextEntry(new ZipEntry(((File)rawSourceModel.elementAt(i)).getName()));
        
                // Transfer bytes from the file to the ZIP file
                while ((len = in.read(buf)) > 0) {
                    out.write(buf, 0, len);
                }
        
                // Complete the entry
                out.closeEntry();
                in.close();
            }
            
            
            // Complete the ZIP file
            out.close();
            
            
            
            //create the SRB File Transferer
            SRBFileTransferer transferer = new SRBFileTransferer();
            transferer.initFileSystemNDARSend();
            LocalFile localFile = new LocalFile(outFilename);
            
            String targetDir = null;
            boolean isPublic = publicButton.isSelected();
            if (isPublic) {
            //	targetDir = Preferences.getProperty(Preferences.PREF_NDAR_GENOMICS_DIR_PUBLIC);
            	if (targetDir == null) {
            		targetDir = "/home/Public/Link/Public/";
            //		Preferences.setProperty(Preferences.PREF_NDAR_GENOMICS_DIR_PUBLIC, targetDir);
            	}
            	
            } else {
            //	targetDir = Preferences.getProperty(Preferences.PREF_NDAR_GENOMICS_DIR_PRIVATE);
            	if (targetDir == null) {
            		targetDir = "/home/Public/Link/Private/";
            //		Preferences.setProperty(Preferences.PREF_NDAR_GENOMICS_DIR_PRIVATE, targetDir);
            	}
            }
            
            //transfer the zipped meta and raw data to the destination dump folder (private or public)
            System.err.println("targetDir: " + targetDir + ", tempdir: " + USER_MIPAV_TEMP_DIR + ", outfilename: " + outFilename);
            GeneralFile targetFile = transferer.createTargetFile(targetDir + File.separator, USER_MIPAV_TEMP_DIR, outFilename);
                        
            System.err.println("created target file: " + targetFile);
            logOutputArea.getTextArea().append(new Date() + " Transferring " + outFilename + " to " + targetDir + "\n");
            transferer.transfer(localFile, targetFile);
            
            logOutputArea.getTextArea().append(new Date() + " Transfer successful\n");
            
            //if the destination is private, send all raw data to user specified directory
            
            //temp
            if (logOutputArea != null) {
            	return;
            }
            
            if (!isPublic) {
            	String privateUserDir = privateField.getText();
            	
            	GeneralFile [] sourceFiles = new GeneralFile[rawSourceModel.getSize()];
            	GeneralFile [] targetFiles = new GeneralFile[sourceFiles.length];
            	String sourceFileName;
            	for (int i = 0; i < sourceFiles.length; i++) {
            		sourceFileName = ((File)rawSourceModel.elementAt(i)).getPath();
            		sourceFiles[i] = new LocalFile((File)rawSourceModel.elementAt(i));
            		targetFiles[i] = transferer.createTargetFile(privateUserDir, new File(sourceFileName).getParent(), sourceFileName);
            		logOutputArea.getTextArea().append(new Date() + "Adding " + sourceFileName + " to transfer list\n");
            	}
            	logOutputArea.getTextArea().append(new Date() + " Transferring files to " + privateUserDir +"\n");
            	transferer.transfer(sourceFiles, targetFiles);
            	logOutputArea.getTextArea().append(new Date() + " Successfully transferred files to " + privateUserDir +"\n");
            }
            
            
        } catch (IOException e) {
        	
        	logOutputArea.getTextArea().append(new Date() + " " + e.getMessage() +"\n");
        }
        logOutputArea.getTextArea().append("\n\n");
    }
       
    private JPanel buildButtonPanel() {
    	JPanel buttonPanel = new JPanel();
    	previousButton = WidgetFactory.buildTextButton("Previous", "Go to previous tab", "Previous", this);
    	nextButton = WidgetFactory.buildTextButton("Next", "Go to next tab", "Next", this);
    	addMetaSourceButton = WidgetFactory.buildTextButton("Add meta", "Add meta-data files", "AddMetaSource", this);
    	addRawSourceButton = WidgetFactory.buildTextButton("Add raw", "Add raw files", "AddRawSource", this);
    	removeSourceButton = WidgetFactory.buildTextButton("Remove files", "Remove source files", "RemoveSource", this);
    	helpButton = WidgetFactory.buildTextButton("Help", "Show MIPAV help", "", this);
    	
    	previousButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	nextButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	addMetaSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	addRawSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	removeSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	
    	previousButton.setEnabled(false);
    	nextButton.setEnabled(false);
    	addMetaSourceButton.setEnabled(false);
    	addRawSourceButton.setEnabled(false);
    	removeSourceButton.setEnabled(false);
    	
    	
    	buttonPanel.add(previousButton);
    	buttonPanel.add(nextButton);
    	buttonPanel.add(addMetaSourceButton);
    	buttonPanel.add(addRawSourceButton);
    	buttonPanel.add(removeSourceButton);
    	buttonPanel.add(helpButton);
    	
    	return buttonPanel;
    }
    
    
    private class RepositoryXML extends FileXML {
    	
    	private File file;
    	
    	 public RepositoryXML(String fileName, String fileDir) throws IOException {
    	    	super(fileName, fileDir);
    	    	file = new File(fileDir + fileName);
    	 }
    	
    	 public void writeXML() throws IOException {
    	       
    	        FileWriter fw;   	      

    	        try {

    	            fw = new FileWriter(file);
    	            bw = new BufferedWriter(fw);

    	            bw.write(XML_HEADER);
    	            bw.newLine();

    	            openTag("SubmissionInfo", true);
    	                	            
    	            closedTag( "Path", privateField.getText());
    	            closedTag( "Name", piNameField.getText());
    	            closedTag( "Title", piTitleField.getText());
    	            closedTag( "Email", piTitleField.getText());
    	            closedTag( "Phone", piTitleField.getText());
    	            closedTag( "IRBNumber", piTitleField.getText());
    	            
    	            closedTag("AbstractTitle", abstractTitleField.getText());
    	            
    	            closedTag( "AbstractBody", abstractArea.getTextArea().getText());


    	            openTag("SubmissionInfo", false);
    	            bw.close();
    	        } catch (Exception e) {
    	            System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
    	            e.printStackTrace();
    	        }
    	    }
    	 
    }
    
}
