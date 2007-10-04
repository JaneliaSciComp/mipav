package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
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

	private JTextField piField, irbField, abstractTitleField, sourceField, destinationField;
	
	private JTextField piNameField, piEmailField, piPhoneField;
	
	private JTextField [] guidFields;
	
	private WidgetFactory.ScrollTextArea abstractArea;
	
	private JButton sourceButton, loadGUIDsButton;
   
	private JList sourceList;
	
	private JButton nextButton, previousButton, addSourceButton, removeSourceButton, openAbstractButton;
	
	private JTabbedPane tabbedPane;
	
	private JPanel guidPanel;
	
	private DefaultListModel sourceModel;
	
	private boolean doneAddingFiles = false;
	
	private static final int GUID_LENGTH = 12;
	
	private static final String SPACE = " ";

	private static final int TAB_MAIN = 0;
	private static final int TAB_PI = 1;
	private static final int TAB_ABSTRACT = 2;
	private static final int TAB_SOURCE = 3;
	private static final int TAB_GUID = 4;
	
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
        	
        	if (index == TAB_SOURCE) {
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
            
            sourceField.setText(sourceModel.size() + " image(s) selected for transfer");
            
        } else if (command.equals("RemoveSource")) {
        	int [] selected = sourceList.getSelectedIndices();
        	for (int i = selected.length - 1; i >= 0; i--) {
        		sourceModel.removeElementAt(selected[i]);
        	}
        	removeSourceButton.setEnabled(sourceModel.size() > 0);
        	nextButton.setEnabled(sourceModel.size() > 0);
        	sourceField.setText(sourceModel.size() + " image(s) selected for transfer");
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
    	if (index == TAB_MAIN) {
    		previousButton.setEnabled(false);
    	} else {
    		previousButton.setEnabled(true);
    	}
    	
    	if (index == TAB_SOURCE) {
    		nextButton.setEnabled(sourceModel.size() > 0);
    	} else if (index < tabbedPane.getTabCount() - 1) {
    		nextButton.setEnabled(true);
    	} else {
    		nextButton.setEnabled(false);
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
        tabbedPane.addTab("Main", buildMainTab());
        tabbedPane.addTab("Principle Investigator", buildPITab());
        tabbedPane.addTab("Abstract", buildAbstractPanel());
        tabbedPane.addTab("Source", buildSourcePanel());
        tabbedPane.addTab("GUIDs", buildGUIDPane());
        tabbedPane.setEnabledAt(TAB_GUID, false);
        
        tabbedPane.addChangeListener(this);
        
        getContentPane().add(tabbedPane);
        getContentPane().add(buildButtonPanel(), BorderLayout.SOUTH);
        pack();
        validate();
        sourceField.setText(sourceModel.size() + " image(s) selected for transfer");
        this.setMinimumSize(new Dimension(600,320));
        this.setSize(new Dimension(600,320));
    }    
    
    private JPanel buildMainTab() {
    	JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
       
       
        JLabel irbLabel = WidgetFactory.buildLabel("IRB #");
        mainPanel.add(irbLabel, gbc);
        
        gbc.gridx++;
        gbc.gridwidth = 2;
        irbField = WidgetFactory.buildTextField(SPACE);
        mainPanel.add(irbField, gbc);
              
        
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        JLabel sourceLabel = WidgetFactory.buildLabel("Source images");
        mainPanel.add(sourceLabel, gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.gridwidth = 2;
        sourceField = WidgetFactory.buildTextField(SPACE);
        sourceField.setEditable(false);
        mainPanel.add(sourceField, gbc);
        
        gbc.gridx+=2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        sourceButton = WidgetFactory.buildTextButton("Details", "Go to source image tab", "Source", this);
        mainPanel.add(sourceButton, gbc);
        
        return mainPanel;
    }
    
    private JPanel buildPITab() {
    	JPanel piPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = .5;
        gbc.weighty = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        JLabel piNameLabel = WidgetFactory.buildLabel("Name");
        piPanel.add(piNameLabel, gbc);
        
        gbc.weightx = 1;
        piNameField = WidgetFactory.buildTextField(SPACE);
        gbc.gridx++;
        piPanel.add(piNameField, gbc);
    	
        
        JLabel piEmailLabel = WidgetFactory.buildLabel("Email");
        gbc.weightx = .5;
        gbc.gridx =0;
        gbc.gridy++;
        piPanel.add(piEmailLabel, gbc);
        
        piEmailField = WidgetFactory.buildTextField(SPACE);
        gbc.gridx++;
        gbc.weightx = 1;
        piPanel.add(piEmailField, gbc);
        
        JLabel piPhoneLabel = WidgetFactory.buildLabel("Phone");
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = .5;
        piPanel.add(piPhoneLabel, gbc);
        
        piPhoneField = WidgetFactory.buildTextField(SPACE);
    	gbc.gridx++;
    	gbc.weightx = 1;
    	piPanel.add(piPhoneField, gbc);
        
    	return piPanel;
    }
    
    private JPanel buildSourcePanel() {
    	JPanel sourcePanel = new JPanel();
        	
    	
    	sourceModel = new DefaultListModel();
    	sourceList = new JList(sourceModel);
    	
    	JScrollPane listPane = WidgetFactory.buildScrollPane(sourceList);
    	listPane.setBorder(buildTitledBorder("Source images"));
    	listPane.setPreferredSize(new Dimension(500,200));
    	sourcePanel.add(listPane, BorderLayout.CENTER);
    	
    	return sourcePanel;
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
    	abstractTitlePanel.add(abstractLabel, gbc);
    	
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
    		guidLabels[i].setCaretPosition(guidLabels[i].getText().length());
    		guidLabels[i].setSelectionStart(guidLabels[i].getText().lastIndexOf(File.separator));
    		guidLabels[i].setSelectionEnd(guidLabels[i].getText().length());
    	
    		guidLabels[i].setEditable(false);
    	}
    	
    	//parse the potential GUIDs into the fields NDARCJ743PV3	
    	
    	String guidString = null;
    	for (int i = 0; i < numImages; i++) {  	
    		guidString = getValidGUID(sourceModel.elementAt(i).toString());
    		if (guidString != null) {
    			guidFields[i].setText(guidString);
    		}
    		
    	}
    	
    }
    
    private String getValidGUID(String testString) {
    	String validGUID = null;
    	int ndarIndex = testString.indexOf("NDAR");
		if (ndarIndex != -1 && (ndarIndex + GUID_LENGTH < testString.length())) {
		
			testString = testString.substring(ndarIndex, ndarIndex + GUID_LENGTH);
		//	System.err.println(testString);
			if (	isValidChar(testString.charAt(4)) &&
					isValidChar(testString.charAt(5)) &&
					isNumChar(testString.charAt(6)) &&
					isNumChar(testString.charAt(7)) &&
					isNumChar(testString.charAt(8)) &&
					isValidChar(testString.charAt(1)) &&
					isValidChar(testString.charAt(10)) &&
					isNumChar(testString.charAt(11))) {
				validGUID = testString;
			}
			
		}
    	
    	return validGUID;
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
    
}
