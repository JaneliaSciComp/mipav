package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.srb.SRBFileTransferer;
import gov.nih.mipav.model.srb.SRBUtility;
import gov.nih.mipav.model.structures.*;


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
	
	/** Scrolling text area for abstract */
	private WidgetFactory.ScrollTextArea abstractArea;
	
	/** Scrolling text area for log output */
	private WidgetFactory.ScrollTextArea logOutputArea;
	
	JScrollPane listPane;
	private JList sourceList;
	
	private JButton nextButton, previousButton, addSourceButton, removeSourceButton;
	
	private JButton privateBrowseButton;
	
	private JRadioButton publicButton, privateButton;
	
	private JTabbedPane tabbedPane;
		
	private DefaultListModel sourceModel;

	private JComboBox organizationBox;
	
	private JCheckBox anonConfirmBox;
	
	private JTextArea privacyTextArea;
	
	private boolean doneAddingFiles = false;
	
	/** Static tab indices */
	private static final int TAB_MAIN = 0;
	private static final int TAB_SOURCE = 1;
	private static final int TAB_DESTINATION = 2;
	
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
        				tabbedPane.setSelectedIndex(TAB_DESTINATION);
        			}
        		} 
        	}  else if (tabbedPane.getTabCount() > index + 1) {
        		tabbedPane.setSelectedIndex(index + 1);
        	}
        	        	
        } else if (command.equals("Previous")) {
        	int index = tabbedPane.getSelectedIndex();
        	
        	if (index > 0) {
        		tabbedPane.setSelectedIndex(index - 1);
        	}
        } else if (command.equals("AddSource")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            chooser.setMultiSelectionEnabled(true);
            
            int returnVal = chooser.showOpenDialog(null);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File[] files = chooser.getSelectedFiles();
                ViewUserInterface.getReference().setDefaultDirectory(files[0].getParent());
                for (int i = 0; i < files.length; i++) {
                	if (!sourceModel.contains(files[i])) {
                		sourceModel.addElement(files[i]);  
                	}
                }
            }
            removeSourceButton.setEnabled(sourceModel.size() > 0);
            nextButton.setEnabled(sourceModel.size() > 0);
            
            listPane.setBorder(buildTitledBorder(sourceModel.size() + " file(s) selected for transfer"));
            
        } else if (command.equals("RemoveSource")) {
        	int [] selected = sourceList.getSelectedIndices();
        	for (int i = selected.length - 1; i >= 0; i--) {
        		sourceModel.removeElementAt(selected[i]);
        	}
        	removeSourceButton.setEnabled(sourceModel.size() > 0);
        	nextButton.setEnabled(sourceModel.size() > 0);
        	listPane.setBorder(buildTitledBorder(sourceModel.size() + " file(s) selected for transfer"));
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
    	} else {
    		nextButton.setEnabled(true);
    	}
    	
    	addSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && !doneAddingFiles);
    	removeSourceButton.setEnabled(tabbedPane.getSelectedIndex() == TAB_SOURCE && sourceModel.size() > 0 && !doneAddingFiles);
    }
   
    public void itemStateChanged(ItemEvent e) {
    	if (e.getSource().equals(anonConfirmBox)) {
    		if (anonConfirmBox.isSelected()) {
    			anonConfirmBox.setEnabled(false);
    			nextButton.setEnabled(true);
    			
    			tabbedPane.setEnabledAt(TAB_SOURCE, true);
    			tabbedPane.setEnabledAt(TAB_DESTINATION, true);
    			privacyTextArea.setBackground(helpButton.getBackground());
    		}
    	} else if (e.getSource().equals(privateButton)) {
    		privateBrowseButton.setEnabled(privateButton.isSelected());
			privateField.setEnabled(privateButton.isSelected());
    	}
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
        tabbedPane.addTab("Source", buildSourcePanel());
        tabbedPane.addTab("Destination", buildDestinationPanel());
        
        tabbedPane.setEnabledAt(TAB_SOURCE, false);
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
    	listPane.setBorder(buildTitledBorder(0 + " file(s) selected for transfer"));
    	sourcePanel.add(listPane, gbc);
    	
    	return listPane;
    }
    
    private JPanel buildDestinationPanel() {
    	JPanel destPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.weightx = 1;
        
    	organizationBox = new JComboBox(ORGANIZATIONS);
        organizationBox.setFont(MipavUtil.font12);        
        
        
        gbc2.gridy = 0;
        gbc2.gridx = 0;
        gbc2.insets = new Insets(5, 10, 5, 0);
        destPanel.add(organizationBox, gbc2);
        
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
        gbc.fill = gbc.BOTH;
        gbc.insets = new Insets(0,5,0,5);
        visPanel.add(privateField, gbc);
        
        gbc.insets = new Insets(0,0,0,0);
        gbc.weightx = 0;
        gbc.gridx+=3;
        gbc.gridwidth = 1;
        gbc.fill = gbc.NONE;
        visPanel.add(privateBrowseButton, gbc);
        
        
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
     * Open the file(s), save header off to local temp folder
     * transfer the file(s) to the given destination
     *
     */
    private void transfer() {
    	int numFiles = sourceModel.size();
    	String tempDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "temp" +
    	    File.separator;
    	if (!new File(tempDir).exists()) {
    		new File(tempDir).mkdirs();
    	}
    	
        // Create a buffer for reading the files
        byte[] buf = new byte[1024];
        
        try {
            // Create the ZIP file
            String outFilename = tempDir + "ndar_genomics.zip";
            ZipOutputStream out = new ZipOutputStream(new FileOutputStream(outFilename));
        
            // Compress the files
            for (int i=0; i < numFiles; i++) {
                FileInputStream in = new FileInputStream((File)sourceModel.elementAt(i));
        
                // Add ZIP entry to output stream.
                out.putNextEntry(new ZipEntry(((File)sourceModel.elementAt(i)).getName()));
        
                // Transfer bytes from the file to the ZIP file
                int len;
                while ((len = in.read(buf)) > 0) {
                    out.write(buf, 0, len);
                }
        
                // Complete the entry
                out.closeEntry();
                in.close();
            }
        
            // Complete the ZIP file
            out.close();
        } catch (IOException e) {
        }
    	
    }
    
    /**
     * Puts all available information (JTextField/JTextArea) into an NDARData object
     * @return true
     */
    private boolean setVariables() {
    		    	
    	
    	
    	return true;
    }
    
    private JPanel buildButtonPanel() {
    	JPanel buttonPanel = new JPanel();
    	previousButton = WidgetFactory.buildTextButton("Previous", "Go to previous tab", "Previous", this);
    	nextButton = WidgetFactory.buildTextButton("Next", "Go to next tab", "Next", this);
    	addSourceButton = WidgetFactory.buildTextButton("Add files", "Add source files", "AddSource", this);
    	removeSourceButton = WidgetFactory.buildTextButton("Remove files", "Remove source files", "RemoveSource", this);
    	helpButton = WidgetFactory.buildTextButton("Help", "Show MIPAV help", "", this);
    	
    	previousButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	nextButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	addSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	removeSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
    	
    	previousButton.setEnabled(false);
    	nextButton.setEnabled(false);
    	addSourceButton.setEnabled(false);
    	removeSourceButton.setEnabled(false);
    	
    	
    	buttonPanel.add(previousButton);
    	buttonPanel.add(nextButton);
    	buttonPanel.add(addSourceButton);
    	buttonPanel.add(removeSourceButton);
    	buttonPanel.add(helpButton);
    	
    	return buttonPanel;
    }
}
