package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.srb.SRBFileTransferer;
import gov.nih.mipav.model.structures.*;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.srb.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;
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

public class JDialogNDAR extends JDialogBase implements ActionListener, ChangeListener, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------


    //~ Instance fields ------------------------------------------------------------------------------------------------

	private JTextField irbField, abstractTitleField;
	
	private JTextField piNameField, piEmailField, piPhoneField, piTitleField;
	
	private JTextField [] guidFields;
	
	/** Scrolling text area for abstract */
	private WidgetFactory.ScrollTextArea abstractArea;
	
	/** Scrolling text area for log output */
	private WidgetFactory.ScrollTextArea logOutputArea;
	
	JScrollPane listPane;
	
	private JButton loadGUIDsButton;
   
	private JList sourceList;
	
	private JButton nextButton, previousButton, addSourceButton, removeSourceButton, openAbstractButton;
	
	private JRadioButton publicButton, organizationButton, privateButton;
	
	private JTabbedPane tabbedPane;
	
	private JPanel guidPanel;
	
	private DefaultListModel sourceModel;

	private JComboBox organizationBox;
	
	private JCheckBox anonConfirmBox;
	
	private JTextArea privacyTextArea;
	
	private boolean doneAddingFiles = false;
	
	/** NDAR data object passed into FileWriteOptions and onto the writeXML with specific NDAR info */
	private NDARData ndarData;
	
	/** Length of the NDAR GUID  */
	private static final int GUID_LENGTH = 12;
	
	private static final String SPACE = " ";

	private Hashtable<File, Boolean> multiFileTable = null;
	
	/** Static tab indices */
	private static final int TAB_MAIN = 0;
	private static final int TAB_PI = 1;
	private static final int TAB_ABSTRACT = 2;
	private static final int TAB_SOURCE = 3;
	private static final int TAB_GUID = 4;
	private static final int TAB_DESTINATION = 5;
	
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

  
    public JDialogNDAR(Frame theParentFrame) {
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

        /**
         * @todo  Implement this java.awt.event.ActionListener abstract method
         */

        String command = e.getActionCommand();

       // System.err.println("size : " + this.getSize());
        
        if (command.equals("Next")) {
        	int index = tabbedPane.getSelectedIndex();
        	
        	if (index == TAB_DESTINATION) {
        		if (setVariables()) {
        			//MipavUtil.displayInfo("Transfer not yet supported.");
        			transfer();
        		}
        	} else if (index == TAB_SOURCE) {
        		if (!doneAddingFiles) {
        			int response = JOptionPane.showConfirmDialog(this, "Done adding source files?", "Done adding source files?",
                                                             JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
                
        			if (response == JOptionPane.YES_OPTION) {
        				doneAddingFiles = true;
        				generateGUIFields();
        				tabbedPane.setEnabledAt(TAB_GUID, true);
        				tabbedPane.setSelectedIndex(TAB_GUID);
        			}
        		} else {
        			tabbedPane.setSelectedIndex(TAB_GUID);
        		}
        	} else if (index == TAB_GUID) {
        		if (checkGUIDs()) {
        			tabbedPane.setEnabledAt(TAB_MAIN, true);
            		tabbedPane.setEnabledAt(TAB_PI, true);
            		tabbedPane.setEnabledAt(TAB_ABSTRACT, true);
            		tabbedPane.setEnabledAt(TAB_SOURCE, true);
            		tabbedPane.setEnabledAt(TAB_DESTINATION, true);
        			tabbedPane.setSelectedIndex(index + 1);
        		}
        	} else if (tabbedPane.getTabCount() > index + 1) {
        		tabbedPane.setSelectedIndex(index + 1);
        	}
        	        	
        } else if (command.equals("Previous")) {
        	int index = tabbedPane.getSelectedIndex();
        	
        	if (index == TAB_GUID) {
        		if (checkGUIDs()) {
        			tabbedPane.setEnabledAt(TAB_MAIN, true);
            		tabbedPane.setEnabledAt(TAB_PI, true);
            		tabbedPane.setEnabledAt(TAB_ABSTRACT, true);
            		tabbedPane.setEnabledAt(TAB_SOURCE, true);
            		tabbedPane.setEnabledAt(TAB_DESTINATION, true);
        			tabbedPane.setSelectedIndex(index -1);
        		}
        	} else if (index > 0) {
        		tabbedPane.setSelectedIndex(index - 1);
        	}
        } else if (command.equals("AddSource")) {
        	ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
            JFileChooser chooser = fileChooser.getFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

            int returnVal = chooser.showOpenDialog(null);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                boolean isMultiFile = fileChooser.isMulti();
                
                File[] files = chooser.getSelectedFiles();
                ViewUserInterface.getReference().setDefaultDirectory(files[0].getParent());
                for (int i = 0; i < files.length; i++) {
                	if (!sourceModel.contains(files[i])) {
                		sourceModel.addElement(files[i]);  
                		multiFileTable.put(files[i], new Boolean(isMultiFile));
                	}
                }
            }
            removeSourceButton.setEnabled(sourceModel.size() > 0);
            nextButton.setEnabled(sourceModel.size() > 0);
            
            listPane.setBorder(buildTitledBorder(sourceModel.size() + " image(s) selected for transfer"));
            
        } else if (command.equals("RemoveSource")) {
        	int [] selected = sourceList.getSelectedIndices();
        	for (int i = selected.length - 1; i >= 0; i--) {
        		sourceModel.removeElementAt(selected[i]);
        		multiFileTable.remove(selected[i]);
        	}
        	removeSourceButton.setEnabled(sourceModel.size() > 0);
        	nextButton.setEnabled(sourceModel.size() > 0);
        	listPane.setBorder(buildTitledBorder(sourceModel.size() + " image(s) selected for transfer"));
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
        } else if (command.equals("LoadGUIDs")) {
        	JFileChooser chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(false);
            
            chooser.setFont(MipavUtil.defaultMenuFont);
            
            int returnVal = chooser.showOpenDialog(null);
            
            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	loadGUIDsFromFile(chooser.getSelectedFile());
            	
            }
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("");
        }
        
        
    }

    public void stateChanged(ChangeEvent e) {
    	int index = tabbedPane.getSelectedIndex();
    	if (index == TAB_MAIN) {
    		previousButton.setEnabled(false);
    	} else if (index == TAB_DESTINATION) {
    		previousButton.setEnabled(tabbedPane.isEnabledAt(TAB_GUID));
    	} else {
    		previousButton.setEnabled(true);
    	}
    	
    	
    	
    	if (index == TAB_SOURCE) {
    		nextButton.setEnabled(sourceModel.size() > 0);
    	} else if (index == TAB_DESTINATION) {
    		nextButton.setEnabled(tabbedPane.isEnabledAt(TAB_GUID));
    	} else if (index == TAB_GUID) {
    		previousButton.setEnabled(true);
    		nextButton.setEnabled(true);
    		tabbedPane.setEnabledAt(TAB_MAIN, false);
    		tabbedPane.setEnabledAt(TAB_PI, false);
    		tabbedPane.setEnabledAt(TAB_ABSTRACT, false);
    		tabbedPane.setEnabledAt(TAB_SOURCE, false);
    		tabbedPane.setEnabledAt(TAB_DESTINATION, false);
    	}
    	else {
    		nextButton.setEnabled(true);
    	}
    	
    	addSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && !doneAddingFiles);
    	removeSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && sourceModel.size() > 0 && !doneAddingFiles);
    	loadGUIDsButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_GUID);
    }
   
    public void itemStateChanged(ItemEvent e) {
    	if (e.getSource().equals(anonConfirmBox)) {
    		if (anonConfirmBox.isSelected()) {
    			anonConfirmBox.setEnabled(false);
    			nextButton.setEnabled(true);
    			
    			tabbedPane.setEnabledAt(TAB_PI, true);
    			tabbedPane.setEnabledAt(TAB_ABSTRACT, true);
    			tabbedPane.setEnabledAt(TAB_SOURCE, true);
    			tabbedPane.setEnabledAt(TAB_DESTINATION, true);
    			privacyTextArea.setBackground(helpButton.getBackground());
    		}
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
    					guidFields[counter].setText(validGUID);
    				}
    			}
    			counter++;
    		} while (tempStr != null);
    		
    	} catch (Exception e) {
    		
    	}
    }
    
    /**
     * DOCUMENT ME!
     */
    private void init() {
        setTitle("NDAR Imaging Import Tool");

        multiFileTable = new Hashtable<File, Boolean>();
        
        tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Main", buildMainTab());
        tabbedPane.addTab("P.I.", buildPITab());
        tabbedPane.addTab("Abstract", buildAbstractPanel());
        tabbedPane.addTab("Source", buildSourcePanel());
        tabbedPane.addTab("GUIDs", buildGUIDPane());
        tabbedPane.addTab("Destination", buildDestinationPanel());
        
        
        
        tabbedPane.setEnabledAt(TAB_PI, false);
        tabbedPane.setEnabledAt(TAB_ABSTRACT, false);
        tabbedPane.setEnabledAt(TAB_SOURCE, false);
        tabbedPane.setEnabledAt(TAB_GUID, false);
        tabbedPane.setEnabledAt(TAB_DESTINATION, false);
        
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
    
    private JPanel buildDestinationPanel() {
    	JPanel destPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.weightx = 1;
        
        
        
        
        gbc2.gridy = 0;
        gbc2.gridx = 0;
        gbc2.insets = new Insets(5, 10, 5, 0);
        destPanel.add(organizationBox, gbc2);
        
        JPanel visPanel = new JPanel();
        
        ButtonGroup group = new ButtonGroup();
        publicButton = WidgetFactory.buildRadioButton("Public", true, group);
        organizationButton = WidgetFactory.buildRadioButton("Organization", true, group);
        privateButton = WidgetFactory.buildRadioButton("Private", true, group);
        
        visPanel.add(publicButton);
        visPanel.add(organizationButton);
        visPanel.add(privateButton);

        visPanel.setBorder(buildTitledBorder("Destination visibility"));

        gbc2.gridy++;
        destPanel.add(visPanel, gbc2);
        
        logOutputArea = WidgetFactory.buildScrollTextArea(Color.white);
        logOutputArea.getTextArea().setBorder(buildTitledBorder("Output log")); 
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
    	        
    	organizationBox = new JComboBox(ORGANIZATIONS);
        organizationBox.setFont(MipavUtil.font12);        
              
               	
    	return piPanel;
    }
    
    private JScrollPane buildSourcePanel() {
    	JPanel sourcePanel = new JPanel();
    	GridBagConstraints gbc = new GridBagConstraints();
    	
    	gbc.anchor = GridBagConstraints.NORTHWEST;	
    	gbc.weightx = 1;
    	gbc.weighty = 1;
    	gbc.fill = GridBagConstraints.BOTH;
    	
    	sourceModel = new DefaultListModel();
    	sourceList = new JList(sourceModel);
    	
    	listPane = WidgetFactory.buildScrollPane(sourceList);
    	listPane.setBorder(buildTitledBorder(0 + " image(s) selected for transfer"));
    	sourcePanel.add(listPane, gbc);
    	
    	return listPane;
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
    
    private JScrollPane buildGUIDPane() {
    	guidPanel = new JPanel(new GridBagLayout());
    	JScrollPane guidPane = WidgetFactory.buildScrollPane(guidPanel);
    	
    	return guidPane;
    }
    
    private void generateGUIFields() {
    	GridBagConstraints gbc = new GridBagConstraints();
    	
    	gbc.anchor = GridBagConstraints.NORTHWEST;
    	gbc.gridx = 0;
    	gbc.gridy = 0;
    	gbc.weightx = 0;
    	gbc.weighty = 0;
    	gbc.gridwidth = 1;
    	gbc.gridheight = 1;

		gbc.fill = GridBagConstraints.REMAINDER;    		
    	
    	int numImages = sourceModel.size();
    	
    	JTextField [] guidLabels = new JTextField[numImages];
    	
    	guidFields = new JTextField[numImages];
    	
    	
    	for (int i = 0; i < numImages; i++) {
    		guidLabels[i] = new JTextField(45);
    		guidLabels[i].setFont(WidgetFactory.font12);
    		guidLabels[i].setForeground(Color.black);
    		
    		guidFields[i] = new JTextField(15);
    		guidFields[i].setFont(WidgetFactory.font12);
    		guidFields[i].setForeground(Color.black);
    		
    		gbc.gridx = 0;
    		gbc.weightx = 1;
    		guidPanel.add(guidLabels[i], gbc);
    		
    		gbc.gridx++;
    		gbc.weightx = 1;
    		guidPanel.add(guidFields[i], gbc);
    		gbc.gridy++;
    	}
    	for (int i = 0; i < numImages; i++) {  	
    		guidLabels[i].setText(sourceModel.elementAt(i).toString());
    		guidLabels[i].setCaretPosition(guidLabels[i].getText().length());
    		
    	    guidLabels[i].setEditable(false);
    	}
    	
    	//parse the potential GUIDs into the fields NDARCJ743PV3	
    	
    	String guidString = null;
    	for (int i = 0; i < numImages; i++) {  	
    		guidString = getValidGUID(sourceModel.elementAt(i).toString());
    		if (guidString != null){
    			guidFields[i].setText(guidString);
    		}
    		
    	}
    	
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
     * @param checkString the string to check
     * @return whether this is a valid guid
     */
    private boolean isValidGUID(String checkString) {
    	if (checkString.length() != GUID_LENGTH) {
    		return false;
    	}
    	
    	if (	isValidChar(checkString.charAt(4)) &&
				isValidChar(checkString.charAt(5)) &&
				isNumChar(checkString.charAt(6)) &&
				isNumChar(checkString.charAt(7)) &&
				isNumChar(checkString.charAt(8)) &&
				isValidChar(checkString.charAt(1)) &&
				isValidChar(checkString.charAt(10)) &&
				(isNumChar(checkString.charAt(11)) || isValidChar(checkString.charAt(11)))) {
			return true;
		} 
    	return false;
    }
    
    /**
     * Is the char a valid number character
     * @param checkChar char to check
     * @return whether is a number
     */
    private boolean isNumChar(char checkChar) {
    	
    	return (checkChar >= '0' && checkChar <= '9');
    }
    
    /**
     * Check if this is a valid NDAR character ( no I, O, Q, or S)
     * @param checkChar char to check
     * @return is the char valid
     */
    private boolean isValidChar(char checkChar) {
    	if ((checkChar >= 'a' && checkChar <= 'z') ||
    			(checkChar >= 'A' && checkChar <= 'Z')) {
    		if (	checkChar != 'i' &&
    				checkChar != 'I' &&
    				checkChar != 'o' && 
    				checkChar != 'O' &&
    				checkChar != 'q' &&
    				checkChar != 'Q' &&
    				checkChar != 's' &&
    				checkChar != 'S') {
    			return true;
    		}
    	}
    	
    	return false;
    }
    
    /**
     * Open the file(s), save header off to local temp folder
     * transfer the file(s) to the given destination
     *
     */
    private void transfer() {
    	//Create the FileIO
    	FileIO fileIO = new FileIO();
    	fileIO.setQuiet(true);
    	SRBFileTransferer transferer = new SRBFileTransferer();
    	
    	
    	transferer.setNDAR(true);
    	
    	int numImages = sourceModel.size();
    	ModelImage tempImage = null;
    	
    	FileWriteOptions options = new FileWriteOptions(true);
    	String tempDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "temp" +
    	    File.separator;
    	if (!new File(tempDir).exists()) {
    		new File(tempDir).mkdirs();
    	}
    	
    	String fName;
    	File currentImageFile;
    	LocalFile localFile;
    	
    	for (int i = 0; i < numImages; i++) {
    		currentImageFile = (File)sourceModel.elementAt(i);
    		
    		logOutputArea.getTextArea().append("Opening: " + currentImageFile + ", multifile: " + 
    				multiFileTable.get(currentImageFile).booleanValue() + "\n");
    		tempImage = fileIO.readImage(currentImageFile.getAbsolutePath());
    		//Save the image's XML to disk
    		
    		    		
    		//set the valid GUID into the NDAR data object, and set up the filewriteoptions for this image
    		ndarData.validGUID = guidFields[i].getText();
    		options.setMultiFile(multiFileTable.get(currentImageFile).booleanValue());
    		options.setWriteHeaderOnly(true);
    		options.setNDARData(ndarData);
    		
    		// get the image file name and add .xml to it (maintain the previous extension in the name)
    		fName = tempImage.getImageFileName();
    		if (!fName.endsWith(".xml")) {
    			fName += ".xml";
    		}
    		
    		if (tempImage.getNDims() > 2) {
    			options.setBeginSlice(0);
    			options.setEndSlice(tempImage.getExtents()[2] - 1);
    		} 
    		if (tempImage.getNDims() > 3) {
    			options.setBeginSlice(0);
    			options.setEndTime(tempImage.getExtents()[3] - 1);
    		}
    		options.setFileDirectory(tempDir);
    		options.doPutInQuicklist(false);
    		options.setFileName(fName);
    		options.setFileType(FileUtility.XML);
    		options.setMultiFile(false);
    		options.setOptionsSet(true);
    		
    		logOutputArea.getTextArea().append("Saving header: " + fName + " to: " + tempDir + "\n");
    		
    		//write out only the header to userdir/mipav/temp
    		fileIO.writeImage(tempImage, options);
    		
    		
    		//localFile = new LocalFile(options.getFileDirectory() + File.separator + options.getFileName());
    		
    		//use the directory and filename to send the .xml file
    		
    		
    		//transferer.transfer(sourceFile, targetFile)
    		
    		//now send the model image
    		
    		//transferer.saveToSRB(tempImage);
    	
    		
    	}
    	
    }
    
    private boolean checkGUIDs() {
//    	check to see that valid GUIDs are present for all listed files
    	int numImages = sourceModel.size();
    	for (int i = 0; i < numImages; i++) {
    		if (!isValidGUID(guidFields[i].getText())) {
    			MipavUtil.displayWarning("Invalid GUID");
    			tabbedPane.setSelectedIndex(TAB_GUID);
    			guidFields[i].requestFocus();
    			guidFields[i].setSelectionStart(0);
    			guidFields[i].setSelectionEnd(guidFields[i].getText().length());
    			return false;
    		}
    	}
    	return true;
    }
    
    private boolean setVariables() {
    	 	    	
    	//parse out the information from the text fields/abstract info etc
    	
    	ndarData = new NDARData(piNameField.getText(),
    			piTitleField.getText(),
    			piEmailField.getText(),	
    			piPhoneField.getText(),
    			abstractTitleField.getText(),
    			abstractArea.getTextArea().getText());
    	
    	return true;
    }
    
    private JPanel buildButtonPanel() {
    	JPanel buttonPanel = new JPanel();
    	previousButton = WidgetFactory.buildTextButton("Previous", "Go to previous tab", "Previous", this);
    	nextButton = WidgetFactory.buildTextButton("Next", "Go to next tab", "Next", this);
    	addSourceButton = WidgetFactory.buildTextButton("Add files", "Add source files", "AddSource", this);
    	removeSourceButton = WidgetFactory.buildTextButton("Remove files", "Remove source files", "RemoveSource", this);
    	loadGUIDsButton = WidgetFactory.buildTextButton("Load GUIDs", "Parse GUIDs from text file", "LoadGUIDs", this);
    	helpButton = WidgetFactory.buildTextButton("Help", "Show MIPAV help", "", this);
    	
    	previousButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	nextButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	addSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	removeSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	loadGUIDsButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	
    	previousButton.setEnabled(false);
    	nextButton.setEnabled(false);
    	addSourceButton.setEnabled(false);
    	removeSourceButton.setEnabled(false);
    	loadGUIDsButton.setEnabled(false);
    	
    	
    	buttonPanel.add(previousButton);
    	buttonPanel.add(nextButton);
    	buttonPanel.add(addSourceButton);
    	buttonPanel.add(removeSourceButton);
    	buttonPanel.add(loadGUIDsButton);
    	buttonPanel.add(helpButton);
    	
    	return buttonPanel;
    }
    
    public class NDARData {
    	
    	public String irbNumber;
    	
    	public String piName;
    	public String piEmail;
    	public String piPhone;
    	public String piTitle;
    	public String abstractTitle;
    	public String abstractBody;
    	public String validGUID;
    	
    	public NDARData(String name, String title, String email, String ph, String abT, String abB) {
    		piName = name;
    		piTitle = title;
    		piEmail = email;
    		piPhone = ph;
    		abstractTitle = abT;
    		abstractBody = abB;
    	}
    	
    }
    
}
