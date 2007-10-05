package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.srb.SRBFileTransferer;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.MipavMath;

import java.awt.*;
import java.awt.event.*;

import java.util.*;
import java.io.*;

import javax.swing.*;
import javax.swing.event.*;


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

public class JDialogNDAR extends JDialogBase implements ActionListener, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------


    //~ Instance fields ------------------------------------------------------------------------------------------------

	private JTextField irbField, abstractTitleField;
	
	private JTextField piNameField, piEmailField, piPhoneField, piTitleField;
	
	private JTextField [] guidFields;
	
	private WidgetFactory.ScrollTextArea abstractArea;
	
	JScrollPane listPane;
	
	private JButton loadGUIDsButton;
   
	private JList sourceList;
	
	private JButton nextButton, previousButton, addSourceButton, removeSourceButton, openAbstractButton;
	
	private JRadioButton publicButton, organizationButton, privateButton;
	
	private JTabbedPane tabbedPane;
	
	private JPanel guidPanel;
	
	private DefaultListModel sourceModel;

	private JComboBox organizationBox;
	
	private boolean doneAddingFiles = false;
	
	private NDARData ndarData;
	
	private static final int GUID_LENGTH = 12;
	
	private static final String SPACE = " ";

	private static final int TAB_PI = 0;
	private static final int TAB_ABSTRACT = 1;
	private static final int TAB_SOURCE = 2;
	private static final int TAB_GUID = 3;
	
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
        super(theParentFrame, true);
        
        init();
        setVisible(true);
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

        if (command.equals("Next")) {
        	int index = tabbedPane.getSelectedIndex();
        	
        	if (index == TAB_GUID) {
        		if (setVariables()) {
        			transfer();
        		}
        	} else if (index == TAB_SOURCE) {
        		int response = JOptionPane.showConfirmDialog(this, "Done adding source files?", "Done adding source files?",
                                                             JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
                
        		
        		if (response == JOptionPane.YES_OPTION) {
        			doneAddingFiles = true;
        			generateGUIFields();
        			tabbedPane.setEnabledAt(TAB_GUID, true);
        			tabbedPane.setSelectedIndex(TAB_GUID);
        		}
        	} else if (tabbedPane.getTabCount() > index + 1) {
        		tabbedPane.setSelectedIndex(index + 1);
        	}
        	        	
        } else if (command.equals("Previous")) {
        	int index = tabbedPane.getSelectedIndex();
        	if (index > 0) {
        		tabbedPane.setSelectedIndex(index - 1);
        	}
        } else if (command.equals("AddSource")) {
        	System.err.println("addsrc");
        	JFileChooser chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(true);
            
            chooser.setFont(MipavUtil.defaultMenuFont);
            
            int returnVal = chooser.showOpenDialog(null);
            
            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	File [] files = chooser.getSelectedFiles();
            	for (int i = 0; i < files.length; i++) {
            		sourceModel.addElement(files[i]);            	
            	}
            }
            removeSourceButton.setEnabled(sourceModel.size() > 0);
            nextButton.setEnabled(sourceModel.size() > 0);
            
            listPane.setBorder(buildTitledBorder(sourceModel.size() + " image(s) selected for transfer"));
            
        } else if (command.equals("RemoveSource")) {
        	int [] selected = sourceList.getSelectedIndices();
        	for (int i = selected.length - 1; i >= 0; i--) {
        		sourceModel.removeElementAt(selected[i]);
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
        }
        
        
    }

    public void stateChanged(ChangeEvent e) {
    	int index = tabbedPane.getSelectedIndex();
    	if (index == TAB_PI) {
    		previousButton.setEnabled(false);
    	} else {
    		previousButton.setEnabled(true);
    	}
    	
    	if (index == TAB_SOURCE) {
    		nextButton.setEnabled(sourceModel.size() > 0);
    	} else {
    		nextButton.setEnabled(true);
    	}
    	
    	addSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && !doneAddingFiles);
    	removeSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && sourceModel.size() > 0 && !doneAddingFiles);
    	loadGUIDsButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_GUID);
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
        setTitle("NDAR");

        tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Main", buildPITab());
        tabbedPane.addTab("Abstract", buildAbstractPanel());
        tabbedPane.addTab("Source", buildSourcePanel());
        tabbedPane.addTab("GUIDs", buildGUIDPane());
        tabbedPane.setEnabledAt(TAB_GUID, false);
        
        tabbedPane.addChangeListener(this);
        
        getContentPane().add(tabbedPane);
        getContentPane().add(buildButtonPanel(), BorderLayout.SOUTH);
        pack();
        validate();
        this.setMinimumSize(new Dimension(600,320));
        this.setSize(new Dimension(600,320));
    }    
    
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
        
        JPanel destPanel = new JPanel(new GridBagLayout());
        destPanel.setBorder(buildTitledBorder("Destination"));
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.weightx = 1;
               
        
        ButtonGroup group = new ButtonGroup();
        publicButton = WidgetFactory.buildRadioButton("Public", true, group);
        organizationButton = WidgetFactory.buildRadioButton("Organization", true, group);
        privateButton = WidgetFactory.buildRadioButton("Private", true, group);
        
        gbc2.gridx = 0;
        destPanel.add(organizationBox, gbc2);
        
        gbc2.anchor = GridBagConstraints.EAST;
        gbc2.gridx++;
        destPanel.add(publicButton, gbc2);
        gbc2.gridx++;
        destPanel.add(organizationButton, gbc2);
        gbc2.gridx++;
        destPanel.add(privateButton, gbc2);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        piPanel.add(destPanel, gbc);
               
        
        
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
    		guidLabels[i] = new JTextField(40);
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
    		guidLabels[i].setHorizontalAlignment(JTextField.RIGHT);
    	
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
				isNumChar(checkString.charAt(11))) {
			return true;
		} 
    	return false;
    }
    
    private boolean isNumChar(char checkChar) {
    	
    	return (checkChar >= '0' && checkChar <= '9');
    }
    
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
    
    private void transfer() {
    	System.err.println("transfer...fake");
    	
    	FileIO fileIO = new FileIO();
    	SRBFileTransferer transferer = new SRBFileTransferer();
    	
    	int numImages = sourceModel.size();
    	ModelImage tempImage = null;
    	
    	FileWriteOptions options = new FileWriteOptions(true);
    	String tempDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "temp" +
    	    File.separator;
    	if (!new File(tempDir).exists()) {
    		new File(tempDir).mkdirs();
    	}
    	
    	String fName;
    	
    	for (int i = 0; i < numImages; i++) {
    		tempImage = fileIO.readImage(sourceModel.elementAt(i).toString());
    		//Save the image's XML to disk
    		
    		//set the valid GUID into the NDAR data object, and set up the filewriteoptions for this image
    		ndarData.validGUID = guidFields[i].getText();
    		options.setWriteHeaderOnly(true);
    		options.setNDARData(ndarData);
    		
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
    		
    		
    		fileIO.writeImage(tempImage, options);
    		
    		
    		
    	//	transferer.saveToSRB(tempImage);
    	
    		
    	}
    	
    }
    
    private boolean setVariables() {
    	
    	//check to see that valid GUIDs are present for all listed files
    	int numImages = sourceModel.size();
    	
    	for (int i = 0; i < numImages; i++) {
    		if (!isValidGUID(guidFields[i].getText())) {
    			MipavUtil.displayWarning("Invalid GUID");
    			guidFields[i].requestFocus();
    			guidFields[i].setSelectionStart(0);
    			guidFields[i].setSelectionEnd(guidFields[i].getText().length());
    			return false;
    		}
    	}
    	
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
    	
    	previousButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	nextButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	addSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	removeSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	loadGUIDsButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	
    	addSourceButton.setEnabled(false);
    	removeSourceButton.setEnabled(false);
    	loadGUIDsButton.setEnabled(false);
    	buttonPanel.add(previousButton);
    	buttonPanel.add(nextButton);
    	buttonPanel.add(addSourceButton);
    	buttonPanel.add(removeSourceButton);
    	buttonPanel.add(loadGUIDsButton);
    	
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
