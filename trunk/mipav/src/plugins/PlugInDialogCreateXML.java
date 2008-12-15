import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
import javax.swing.*;

import java.io.File;
import java.util.Vector;

/**
 * @author joshim2
 *
 */
public class PlugInDialogCreateXML extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

	// ~ Static fields -------------------------------------------------------------------------
	
	public static final String BROWSE_FILE = "Browse file";

	public static final String REMOVE_FILE = "Remove file";
	
	public static final String REMOVE_ALL_FILE = "Remove all file";
	
	public static final String BROWSE_FOLDER = "Browse folder";
	
	public static final String REMOVE_FOLDER = "Remove folder";
	
	public static final String REMOVE_ALL_FOLDER = "Remove all folder";
	
	// ~ Instance fields ------------------------------------------------------------------------
	
	/** GridBagLayout * */
    private GridBagLayout mainPanelGridBagLayout;
    
    /** GridBagConstraints * */
    private GridBagConstraints mainPanelConstraints;
    
    /** panels * */
    private JPanel mainPanel, OKCancelPanel;
    
    /** Labels **/
    private JLabel inputFileLabel, inputFolderLabel;
    
    /** Buttons **/
    private JButton inputFileBrowseButton;
    
    private JButton removeFileButton, removeAllFileButton;
    
    private JButton inputFolderBrowseButton;
    
    private JButton removeFolderButton, removeAllFolderButton;
    
    /**All files to generate image information for*/
    private JList inputFileList;
    
    /**All file sets to generate image information for*/
    private JList inputFolderList;

	/** Textfields **/
    private JTextField tagListTextField; 
   	
	/** File chooser object */
	private JFileChooser fileChooser;
	
	/** Files selected by the user */
	private File[] selectedFiles;
	
	/** Folders selected by the user. */
	private File[] selectedFolders;
	
	/** Additional tags to anonymize */
	private String[] tagArray;
	
	/** List of current files and folders to work with */
	private Vector<String> fileList, folderList;
	
	/** Algorithm instance */
    private PlugInAlgorithmCreateXML algoCreateXML;
	
	
	//	~ Constructors --------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public PlugInDialogCreateXML() { }
    
    /**
     * Sets up variables but does not show dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogCreateXML(boolean modal) {
        super(modal); 
        fileList = new Vector<String>();
        folderList = new Vector<String>();
        
    	init();
    }
    
    // ~ Methods ----------------------------------------------------------------------------------
    
    public void init() {
    	setForeground(Color.black);
        setTitle("DICOM Anonymization Tool");
        
        mainPanelGridBagLayout = new GridBagLayout();
        mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.NORTH;

        mainPanel = new JPanel(mainPanelGridBagLayout);
        
        // Input file
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 0;
        mainPanelConstraints.gridheight = 3;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        inputFileLabel = new JLabel(" Input files : ");
        mainPanel.add(inputFileLabel, mainPanelConstraints);

        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 0;
        mainPanelConstraints.gridheight = 3;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        mainPanelConstraints.fill = GridBagConstraints.BOTH;
        inputFileList = new JList(fileList);
        inputFileList.setVisibleRowCount(4);
        inputFileList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        inputFileList.setMinimumSize(new Dimension(300, 94));
        inputFileList.setMaximumSize(new Dimension(300, 500));
        JScrollPane scrollPaneFile = new JScrollPane(inputFileList);
        scrollPaneFile.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPaneFile.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);    
        scrollPaneFile.setMinimumSize(new Dimension(300, 94));
        scrollPaneFile.setPreferredSize(new Dimension(300, 94));
        mainPanel.add(scrollPaneFile, mainPanelConstraints);

        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 0;
        mainPanelConstraints.gridheight = 1;
        mainPanelConstraints.insets = new Insets(15, 5, 5, 5);
        mainPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        inputFileBrowseButton = new JButton("Browse");
        inputFileBrowseButton.addActionListener(this);
        inputFileBrowseButton.setActionCommand(BROWSE_FILE);
        mainPanel.add(inputFileBrowseButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 1;
        mainPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        mainPanelConstraints.insets = new Insets(5, 5, 5, 5);
        removeFileButton = new JButton("Remove");
        removeFileButton.addActionListener(this);
        removeFileButton.setActionCommand(REMOVE_FILE);
        mainPanel.add(removeFileButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 2;
        mainPanelConstraints.fill = GridBagConstraints.NONE;
        mainPanelConstraints.insets = new Insets(5, 5, 15, 5);
        removeAllFileButton = new JButton("Remove All");
        removeAllFileButton.addActionListener(this);
        removeAllFileButton.setActionCommand(REMOVE_ALL_FILE);
        mainPanel.add(removeAllFileButton, mainPanelConstraints);
        
        // Input folder
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 3;
        mainPanelConstraints.gridheight = 3;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        inputFolderLabel = new JLabel(" Input folders : ");
        mainPanel.add(inputFolderLabel, mainPanelConstraints);

        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 3;
        mainPanelConstraints.gridheight = 3;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        mainPanelConstraints.fill = GridBagConstraints.BOTH;
        inputFolderList = new JList(folderList);
        inputFolderList.setVisibleRowCount(4);
        inputFolderList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        inputFolderList.setMinimumSize(new Dimension(300, 94));
        inputFolderList.setMaximumSize(new Dimension(300, 500));
        JScrollPane scrollPaneFolder = new JScrollPane(inputFolderList);
        scrollPaneFolder.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPaneFolder.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);    
        scrollPaneFolder.setMinimumSize(new Dimension(300, 94));
        scrollPaneFolder.setPreferredSize(new Dimension(300, 94));
        mainPanel.add(scrollPaneFolder, mainPanelConstraints);

        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 3;
        mainPanelConstraints.gridheight = 1;
        mainPanelConstraints.insets = new Insets(15, 5, 5, 5);
        mainPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        inputFolderBrowseButton = new JButton("Browse");
        inputFolderBrowseButton.addActionListener(this);
        inputFolderBrowseButton.setActionCommand(BROWSE_FOLDER);
        mainPanel.add(inputFolderBrowseButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 4;
        mainPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        mainPanelConstraints.insets = new Insets(5, 5, 5, 5);
        removeFolderButton = new JButton("Remove");
        removeFolderButton.addActionListener(this);
        removeFolderButton.setActionCommand(REMOVE_FOLDER);
        mainPanel.add(removeFolderButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 5;
        mainPanelConstraints.fill = GridBagConstraints.NONE;
        mainPanelConstraints.insets = new Insets(5, 5, 15, 5);
        removeAllFolderButton = new JButton("Remove All");
        removeAllFolderButton.addActionListener(this);
        removeAllFolderButton.setActionCommand(REMOVE_ALL_FOLDER);
        mainPanel.add(removeAllFolderButton, mainPanelConstraints);
        
        // OK,Cancel 
        OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setResizable(false);
        setVisible(true);
        requestFocus();

    }
    
    private boolean createTagArray () {
    	String tagList;
    	try {
    		tagList = tagListTextField.getText();
    	} catch (NullPointerException npe) {
    		tagArray = null;
    		return true;
    	}
    	
    	tagArray = tagList.split(";");
    	return true;
       	
    }
    
    
    
    /**
     * Once all the necessary variables are set, call the DICOM anonymizer algorithm.
     */
    
    protected void callAlgorithm() {
    
    	try{
    		System.gc();
    		selectedFiles = new File[fileList.size() + folderList.size()];
    		for(int i=0; i<fileList.size(); i++) {
    			System.out.println("Working with file "+fileList.get(i));
    			selectedFiles[i] = new File(fileList.get(i));
    		}
    		for(int i=fileList.size()-1; i<fileList.size() + folderList.size(); i++) {
    			System.out.println("Working with folder "+folderList.get(i-fileList.size()));
    			selectedFiles[i] = new File(folderList.get(i-fileList.size()));
    		}
    		
    		//Make algorithm.
    		algoCreateXML = new PlugInAlgorithmCreateXML(selectedFiles);
    		
    		// This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algoCreateXML.addListener(this);
            
            createProgressBar("DICOM Anonymization Tool", algoCreateXML);
            
            setVisible(false);
            
            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoCreateXML.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoCreateXML.run();
            }
    		
    	} catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Plugin Anonymize DICOM: unable to allocate enough memory");

            return;
    	}
    
    }
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
    	
    	String command = event.getActionCommand();
    	if (command.equalsIgnoreCase(BROWSE_FILE)) {
    		
    		fileChooser = new JFileChooser(Preferences.getImageDirectory());
        	fileChooser.setFont(MipavUtil.defaultMenuFont);
        	fileChooser.setMultiSelectionEnabled(true);
        	fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        	
        	Dimension d = new Dimension(700, 400);
            fileChooser.setMinimumSize(d);
            fileChooser.setPreferredSize(d);
            
            int returnVal = fileChooser.showOpenDialog(null);
                        
            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	selectedFiles = fileChooser.getSelectedFiles();
            	Preferences.setImageDirectory(fileChooser.getCurrentDirectory());
            	for(int i=0; i<selectedFiles.length; i++) {
            		boolean fileExists = false;
            		for(int j=0; j<fileList.size(); j++) {
            			if(selectedFiles[i].getAbsolutePath().equals(fileList.get(j))) {
            				fileExists = true;
            			}
            		}
            		if(!fileExists) {
            			fileList.add(selectedFiles[i].getAbsolutePath());
            		}
            	}
            	inputFileList.updateUI();
            	
            	
            }
        } if (command.equalsIgnoreCase(BROWSE_FOLDER)) {
    		
    		fileChooser = new JFileChooser(Preferences.getImageDirectory());
        	fileChooser.setFont(MipavUtil.defaultMenuFont);
        	fileChooser.setMultiSelectionEnabled(true);
        	fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        	
        	Dimension d = new Dimension(700, 400);
            fileChooser.setMinimumSize(d);
            fileChooser.setPreferredSize(d);
            
            int returnVal = fileChooser.showOpenDialog(null);
                        
            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	selectedFolders = fileChooser.getSelectedFiles();
            	Preferences.setImageDirectory(fileChooser.getCurrentDirectory());
            	for(int i=0; i<selectedFolders.length; i++) {
            		boolean fileExists = false;
            		for(int j=0; j<folderList.size(); j++) {
            			if(selectedFolders[i].getAbsolutePath().equals(folderList.get(j))) {
            				fileExists = true;
            			}
            		}
            		if(!fileExists) {
            			folderList.add(selectedFolders[i].getAbsolutePath());
            		}
            	}
            	inputFolderList.updateUI();
            	
            	
            }
        } else if (command.equalsIgnoreCase("Cancel")) {
        	dispose();
        } else if (command.equalsIgnoreCase("OK")) {
        	createTagArray();
        	callAlgorithm();
        } else if(command.equalsIgnoreCase(REMOVE_ALL_FILE)) {
        	fileList.removeAllElements();
        	inputFileList.updateUI();
        } else if(command.equalsIgnoreCase(REMOVE_ALL_FOLDER)) {
        	folderList.removeAllElements();
        	inputFolderList.updateUI();
        } else if(command.equals(REMOVE_FILE)) {
        	Object[] objAr = inputFileList.getSelectedValues();
        	for(Object obj : objAr) {
        		fileList.remove(obj);
        	}
        	inputFileList.updateUI();
        } else if(command.equals(REMOVE_FILE)) {
        	Object[] objAr = inputFolderList.getSelectedValues();
        	for(Object obj : objAr) {
        		folderList.remove(obj);
        	}
        	inputFolderList.updateUI();
        }
    }

	/**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	progressBar.dispose();
    	algoCreateXML.finalize();
    	algoCreateXML = null;
    	
    }
    
    protected void setGUIFromParams() {
    	
    }
    protected void storeParamsFromGUI() {
    	
    }
    
}
