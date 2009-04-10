import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeListener;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileDicomItem;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQ;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileXML;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.JTextComponent;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Vector;

/**
 * @author joshim2
 *
 */
public class PlugInDialogImageSubmit extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

	// ~ Static fields -------------------------------------------------------------------------
	
	public static final String BROWSE_FILE_INPUT = "Browse file input";
	
	public static final String BROWSE_FILE_SUBMIT = "Browse file submit";

	public static final String BROWSE_AVAILABLE_TAGS = "Browse available tags";
	
	public static final String REMOVE = "Remove file";
	
	public static final String REMOVE_ALL = "Remove all file";
	
	public static final String IMPORT_XML = "Import XML";
	
	// ~ Instance fields ------------------------------------------------------------------------
	
	/** GridBagLayout * */
    private GridBagLayout mainPanelGridBagLayout;
    
    /** GridBagConstraints * */
    private GridBagConstraints mainPanelConstraints;
    
    /** Panels * */
    private JPanel mainPanel, imagePanel, OKCancelPanel;
    
    /** Labels **/
    private JLabel inputFileLabel, anatLabel, tagListSampleLabel, 
    				sourceNameLabel, sourceOrgLabel, sourceProjLabel,
    				testingLabel, detailLabel, tagListLabel, submitFileLabel;
    
    /** Buttons **/
    private JButton inputFileBrowseButton, submitFileBrowseButton, tagEditorBrowseButton;
    
    private JButton removeFileButton, removeAllFileButton;
    
    private JButton importButton;
    
    /**All files to generate image information for*/
    private JList inputFileList;

	/** Textfields **/
    private JTextField anatField, sourceNameField, sourceOrgField,
    					sourceProjField, tagListTextField, submitField; 
   	
	/** File chooser object */
	private JFileChooser fileChooser;
	
	/** Files selected by the user */
	private File[] selectedFiles = new File[0];
	
	/** List of current files to work with (adding a folder through GUI will add all of the folder's files */
	private Vector<String> fileList;
	
	/**Submission location*/
	private String submitLocation;
	
	/** Algorithm instance */
    private PlugInAlgorithmImageSubmit algoImageSubmit;

    /**Areas for adding additional file information*/
	private JTextArea testingArea, detailArea;

	/**File (directory) that will contain anonymized image copies*/
	private File submitFile;

	/** Array of tags from FileDicom*/
	private String[] tagArray;
	
	/**InfoGathering threads to find Dicom data*/
	private ArrayList<InformationUpdate> infoGather;
	
	private TagEditorDialog currentTagEditor;
	
	//	~ Constructors --------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public PlugInDialogImageSubmit() { }
    
    /**
     * Sets up variables but does not show dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogImageSubmit(boolean modal) {
        super(modal); 
        fileList = new Vector<String>();
        infoGather = new ArrayList<InformationUpdate>();
        
    	init();
    }
    
    // ~ Methods ----------------------------------------------------------------------------------
    
    public void init() {
    	setForeground(Color.black);
        setTitle("Image Submission Tool");
        
        mainPanelGridBagLayout = new GridBagLayout();
        mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.NORTH;

        mainPanel = new JPanel(mainPanelGridBagLayout);
        
        // Image Panel
        imagePanel = buildImagePanel();
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 0;
        mainPanelConstraints.gridwidth = 3;
        mainPanel.add(imagePanel, mainPanelConstraints);
        
        //Provenance panel
        JPanel subProv = buildProvPanel();        
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 1;
        mainPanelConstraints.gridwidth = 3;
        mainPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(subProv, mainPanelConstraints);
        
        //Testing panel
        JPanel subPanelDetail = buildTestPanel();
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 2;
        mainPanelConstraints.gridwidth = 3;
        mainPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(subPanelDetail, mainPanelConstraints);
        
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
    
    private JPanel buildImagePanel() {
    	
    	//image browser
    	JPanel imagePanel = new JPanel(new GridBagLayout());
        imagePanel.setBorder(MipavUtil.buildTitledBorder("Image information"));
        GridBagConstraints imageConstraints = new GridBagConstraints();
        imageConstraints.gridx = 0;
        imageConstraints.gridy = 0;
        imageConstraints.gridheight = 3;
        imageConstraints.anchor = GridBagConstraints.WEST;
        imageConstraints.insets = new Insets(15, 5, 15, 0);
        inputFileLabel = new JLabel(" Input files : ");
        imagePanel.add(inputFileLabel, imageConstraints);
        imageConstraints.gridx = 1;
        imageConstraints.gridy = 0;
        imageConstraints.gridheight = 3;
        imageConstraints.insets = new Insets(15, 70, 15, 0);
        imageConstraints.fill = GridBagConstraints.BOTH;
        
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
        imagePanel.add(scrollPaneFile, imageConstraints);

        imageConstraints.gridx = 2;
        imageConstraints.gridy = 0;
        imageConstraints.gridheight = 1;
        imageConstraints.insets = new Insets(15, 5, 5, 5);
        imageConstraints.fill = GridBagConstraints.HORIZONTAL;
        inputFileBrowseButton = new JButton("Browse");
        inputFileBrowseButton.addActionListener(this);
        inputFileBrowseButton.setActionCommand(BROWSE_FILE_INPUT);
        imagePanel.add(inputFileBrowseButton, imageConstraints);
        
        imageConstraints.gridx = 2;
        imageConstraints.gridy = 1;
        imageConstraints.fill = GridBagConstraints.HORIZONTAL;
        imageConstraints.insets = new Insets(5, 5, 5, 5);
        removeFileButton = new JButton("Remove");
        removeFileButton.addActionListener(this);
        removeFileButton.setActionCommand(REMOVE);
        imagePanel.add(removeFileButton, imageConstraints);
        
        imageConstraints.gridx = 2;
        imageConstraints.gridy = 2;
        imageConstraints.fill = GridBagConstraints.NONE;
        imageConstraints.insets = new Insets(5, 5, 15, 5);
        removeAllFileButton = new JButton("Remove All");
        removeAllFileButton.addActionListener(this);
        removeAllFileButton.setActionCommand(REMOVE_ALL);
        imagePanel.add(removeAllFileButton, imageConstraints);
        
        //location copy selector
        imageConstraints.gridx = 0;
        imageConstraints.gridy = 3;
        imageConstraints.anchor = GridBagConstraints.CENTER;
        imageConstraints.insets = new Insets(15, 5, 15, 0);
        submitFileLabel = new JLabel(" Submission location : ");
        imagePanel.add(submitFileLabel, imageConstraints);

        imageConstraints.gridx = 1;
        imageConstraints.gridy = 3;
        
        imageConstraints.insets = new Insets(15, 70, 15, 5);
        imageConstraints.fill = GridBagConstraints.BOTH;
        submitField = new JTextField(45);
        imagePanel.add(submitField, imageConstraints);
        imageConstraints.gridx = 2;
        imageConstraints.gridy = 3;
        imageConstraints.insets = new Insets(5, 5, 5, 5);
        imageConstraints.fill = GridBagConstraints.HORIZONTAL;
        submitFileBrowseButton = new JButton("Browse");
        submitFileBrowseButton.addActionListener(this);
        submitFileBrowseButton.setActionCommand(BROWSE_FILE_SUBMIT);
        imagePanel.add(submitFileBrowseButton, imageConstraints);
        
        // Anatomical area
        imageConstraints.gridx = 0;
        imageConstraints.gridy = 4;
        imageConstraints.insets = new Insets(15, 5, 15, 5);
        anatLabel = new JLabel(" Anatomical Area : ");
        imagePanel.add(anatLabel, imageConstraints);
        
        imageConstraints.gridx = 1;
        imageConstraints.gridy = 4;
        imageConstraints.anchor = GridBagConstraints.WEST;
        imageConstraints.insets = new Insets(15, 70, 5, 5);
        anatField = new JTextField(45);
        imagePanel.add(anatField, imageConstraints);
        
        // Tag list
        imageConstraints.gridx = 0;
        imageConstraints.gridy = 5;
        imageConstraints.fill = GridBagConstraints.NONE;
        imageConstraints.insets = new Insets(15, 5, 15, 5);
        tagListLabel = new JLabel(" Anonymize tags : ");
        imagePanel.add(tagListLabel, imageConstraints);
        
        imageConstraints.gridx = 1;
        imageConstraints.gridy = 5;
        imageConstraints.anchor = GridBagConstraints.WEST;
        imageConstraints.insets = new Insets(15, 70, 5, 5);
        tagListTextField = new JTextField(45);
        imagePanel.add(tagListTextField, imageConstraints);
        
        imageConstraints.gridx = 2;
        imageConstraints.gridy = 5;
        imageConstraints.fill = GridBagConstraints.HORIZONTAL;
        imageConstraints.anchor = GridBagConstraints.CENTER;
        imageConstraints.insets = new Insets(15, 5, 5, 5);
        tagEditorBrowseButton = new JButton("Browse");
        tagEditorBrowseButton.setEnabled(false);
        tagEditorBrowseButton.addActionListener(this);
        tagEditorBrowseButton.setActionCommand(BROWSE_AVAILABLE_TAGS);
        imagePanel.add(tagEditorBrowseButton, imageConstraints);

        imageConstraints.gridx = 1;
        imageConstraints.gridy = 6;
        imageConstraints.anchor = GridBagConstraints.WEST;
        imageConstraints.insets = new Insets(2, 70, 5, 5);
        tagListSampleLabel = new JLabel(" Additional tags format: group,element;group,element e.g. 0002,0000;0002,0001  ");
        imagePanel.add(tagListSampleLabel, imageConstraints);
        
        return imagePanel;
    }
    
    private JPanel buildProvPanel() {
    	//source name
        JPanel subProv = new JPanel(new GridBagLayout());
        subProv.setBorder(MipavUtil.buildTitledBorder("Data provenance"));
        GridBagConstraints provConstraints = new GridBagConstraints();
        provConstraints.gridx = 0;
        provConstraints.gridy = 0;
        provConstraints.weightx = 0;
        provConstraints.fill = GridBagConstraints.NONE;
        provConstraints.anchor = GridBagConstraints.WEST;
        provConstraints.insets = new Insets(15, 5, 15, 5);
        sourceNameLabel = new JLabel(" Source name:  ");
        subProv.add(sourceNameLabel, provConstraints);
        
        provConstraints.gridx = 1;
        provConstraints.gridy = 0;
        provConstraints.weightx = 1;
        provConstraints.insets = new Insets(10, 60, 5, 0);
        sourceNameField = new JTextField(45);
        subProv.add(sourceNameField, provConstraints);
        
        provConstraints.gridx = 2;
        provConstraints.gridy = 0;
        provConstraints.weightx = 0;
        provConstraints.anchor = GridBagConstraints.EAST;
        provConstraints.fill = GridBagConstraints.HORIZONTAL;
        provConstraints.insets = new Insets(5, 0, 5, 5);
        importButton = new JButton("Import XML");
        importButton.setActionCommand(IMPORT_XML);
        importButton.addActionListener(this);
        subProv.add(importButton, provConstraints);
        
        //source organization
        provConstraints.gridx = 0;
        provConstraints.gridy = 1;
        provConstraints.weightx = 0;
        provConstraints.anchor = GridBagConstraints.WEST;
        provConstraints.fill = GridBagConstraints.NONE;
        provConstraints.insets = new Insets(15, 5, 15, 5);
        sourceOrgLabel = new JLabel(" Source organization:  ");
        subProv.add(sourceOrgLabel, provConstraints);
        
        provConstraints.gridx = 1;
        provConstraints.gridy = 1;
        provConstraints.weightx = 1;
        provConstraints.insets = new Insets(10, 60, 5, 5);
        sourceOrgField = new JTextField(45);
        subProv.add(sourceOrgField, provConstraints);
        
        //source project
        provConstraints.gridx = 0;
        provConstraints.gridy = 2;
        provConstraints.weightx = 0;
        provConstraints.anchor = GridBagConstraints.WEST;
        provConstraints.insets = new Insets(15, 5, 15, 5);
        sourceProjLabel = new JLabel(" Source project:  ");
        subProv.add(sourceProjLabel, provConstraints);
        
        provConstraints.gridx = 1;
        provConstraints.gridy = 2;
        provConstraints.weightx = 1;
        provConstraints.insets = new Insets(10, 60, 5, 5);
        sourceProjField = new JTextField(45);
        subProv.add(sourceProjField, provConstraints);

        return subProv;
    }
    
    private JPanel buildTestPanel() {
    	//Testing information
        JPanel subPanelDetail = new JPanel(new GridBagLayout());
        subPanelDetail.setBorder(MipavUtil.buildTitledBorder("Testing and details"));
        GridBagConstraints subBagConstraints = new GridBagConstraints();
        subBagConstraints.anchor = GridBagConstraints.NORTH;
        subBagConstraints.gridx = 0;
        subBagConstraints.gridy = 0;
        subBagConstraints.weightx = 0;
        subBagConstraints.anchor = GridBagConstraints.WEST;
        subBagConstraints.insets = new Insets(15, 5, 15, 5);
        testingLabel = new JLabel(" Testing information:  ");
        subPanelDetail.add(testingLabel, subBagConstraints);
        
        subBagConstraints.gridx = 1;
        subBagConstraints.gridy = 0;
        subBagConstraints.weightx = 1;
        subBagConstraints.insets = new Insets(10, 60, 5, 5);
        testingArea = new JTextArea();
        testingArea.setMinimumSize(new Dimension(500, 94));
        testingArea.setMaximumSize(new Dimension(500, 500));
        JScrollPane scrollPaneTesting = new JScrollPane(testingArea);
        scrollPaneTesting.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPaneTesting.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);    
        scrollPaneTesting.setMinimumSize(new Dimension(500, 94));
        scrollPaneTesting.setPreferredSize(new Dimension(500, 94));
        subPanelDetail.add(scrollPaneTesting, subBagConstraints);
        
        //Details
        subBagConstraints.gridx = 0;
        subBagConstraints.gridy = 2;
        subBagConstraints.weightx = 0;
        subBagConstraints.insets = new Insets(15, 5, 15, 5);
        detailLabel = new JLabel(" Details:  ");
        subPanelDetail.add(detailLabel, subBagConstraints);
        
        subBagConstraints.gridx = 1;
        subBagConstraints.gridy = 2;
        subBagConstraints.weightx = 1;
        subBagConstraints.insets = new Insets(10, 60, 5, 5);
        detailArea = new JTextArea();
        detailArea.setMinimumSize(new Dimension(500, 94));
        detailArea.setMaximumSize(new Dimension(500, 500));
        JScrollPane scrollPaneDetail = new JScrollPane(detailArea);
        scrollPaneDetail.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPaneDetail.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);    
        scrollPaneDetail.setMinimumSize(new Dimension(500, 94));
        scrollPaneDetail.setPreferredSize(new Dimension(500, 94));
        subPanelDetail.add(scrollPaneDetail, subBagConstraints);
        
        return subPanelDetail;
    }
    
    private boolean createTagArray () {
    	String tagList;
    	try {
    		tagList = tagListTextField.getText();
    	} catch (NullPointerException npe) {
    		tagArray = null;
    		return true;
    	}
    	
    	tagArray = tagList.toUpperCase().split(";");
    	return true;
       	
    }
    
    /**
     * Tests whether f already has been added to the GUI
     * @param f
     * @return
     */
    private boolean fileAlreadyAdded(File f) {
    	boolean fileExists = false;
    	for(int i=0; i<fileList.size(); i++) {
    		if(f.getAbsolutePath().equals(fileList.get(i))) {
    			fileExists = true;
    			break;
    		}
    	}
    	
    	return fileExists;
    }
    
    /**
     * Once all the necessary variables are set, call the image submission algorithm.
     */
    
    protected void callAlgorithm() {
    
    	try{
    		System.gc();
    		selectedFiles = new File[fileList.size()];
    		for(int i=0; i<fileList.size(); i++) {
    			selectedFiles[i] = new File(fileList.get(i));
    		}
    		
    		//map keys exact xml tag to particular text
    		HashMap<String, String> map = new HashMap();
    		map.put("anatomical-area", anatField.getText());
    		map.put("source-name", sourceNameField.getText());
    		map.put("source-org", sourceOrgField.getText());
    		map.put("source-project", sourceProjField.getText());
    		map.put("testing-information", testingArea.getText());
    		map.put("details", detailArea.getText());
    		
    		submitLocation = submitField.getText();
    		
    		//Make algorithm.
    		algoImageSubmit = new PlugInAlgorithmImageSubmit(selectedFiles, map, submitLocation, tagArray, true);
    		
    		// This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algoImageSubmit.addListener(this);
            
            createProgressBar("Image Submission Tool", algoImageSubmit);
            
            setVisible(false);
            
            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoImageSubmit.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoImageSubmit.run();
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
    	if (command.equals(BROWSE_FILE_INPUT)) {

    		fileChooser = new JFileChooser(Preferences.getImageDirectory());
        	fileChooser.setFont(MipavUtil.defaultMenuFont);
        	fileChooser.setMultiSelectionEnabled(true);
        	fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
        	
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
            			if(selectedFiles[i].isDirectory()) {
            				for(File f : selectedFiles[i].listFiles()) {
            					if(!f.isDirectory() && !fileAlreadyAdded(f)) {
            						fileList.add(f.getAbsolutePath());
            					}
            				}
            			} else {
            				fileList.add(selectedFiles[i].getAbsolutePath());
            			}
            		}
            	}
            	inputFileList.updateUI();         	
            	InformationUpdate update = new InformationUpdate(fileList, this);
            	if(update.isActionable()) {
            		infoGather.add(update);
            		Thread t = new Thread(update);
            		t.start();
            	}
            	if(selectedFiles[0].isDirectory()) {
            		submitField.setText(selectedFiles[0] + File.separator + "submit");
            	} else {
            		submitField.setText(selectedFiles[0].getParent() + File.separator + "submit");
            	}
            	tagEditorBrowseButton.setEnabled(true);
            }
            
            
        } else if(command.equals(BROWSE_FILE_SUBMIT)) { 
        	String startingFile;
        	if(submitField.getText() != null && submitField.getText().length() > 0) {
        		startingFile = submitField.getText();
        	} else {
        		startingFile = Preferences.getImageDirectory();
        	}
        	
        	fileChooser = new JFileChooser(startingFile);
        	fileChooser.setFont(MipavUtil.defaultMenuFont);
        	fileChooser.setMultiSelectionEnabled(false);
        	fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        	
        	Dimension d = new Dimension(700, 400);
        	fileChooser.setMinimumSize(d);
        	fileChooser.setPreferredSize(d);
        	
        	int returnVal = fileChooser.showOpenDialog(null);
        	
        	if(returnVal == JFileChooser.APPROVE_OPTION) {
        		submitFile = fileChooser.getSelectedFile();
        		submitField.setText(submitFile.getAbsolutePath());
        	}
        } else if (command.equalsIgnoreCase("Cancel")) {
        	dispose();
        } else if (command.equalsIgnoreCase("OK")) {
        	createTagArray();
        	if(!(selectedFiles.length > 0)) {
        		MipavUtil.displayError("No files were selected for submission");
        	} else if(!(submitField.getText().length() > 0)) {
        		MipavUtil.displayError("A submission location is required.");
        	} else {
        		callAlgorithm();
        	}
    	
        } else if(command.equals(REMOVE_ALL)) {
        	fileList.removeAllElements();
        	clearEntries();
        	imagePanel.updateUI();
        } else if(command.equals(REMOVE)) {
        	Object[] objAr = inputFileList.getSelectedValues();
        	for(Object obj : objAr) {
        		fileList.remove(obj);
        	}
        	if(fileList.size() == 0) {
        		clearEntries();
        	}
        	inputFileList.updateUI();
        } else if(command.equals(BROWSE_AVAILABLE_TAGS)) {
        	currentTagEditor.setVisible();	
        } else if(command.equals(IMPORT_XML)) {
        	fileChooser = new JFileChooser(Preferences.getImageDirectory());
        	fileChooser.setFont(MipavUtil.defaultMenuFont);
        	fileChooser.setMultiSelectionEnabled(false);
        	fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        	
        	Dimension d = new Dimension(700, 400);
            fileChooser.setMinimumSize(d);
            fileChooser.setPreferredSize(d);
            
            int returnVal = fileChooser.showOpenDialog(null);
                        
            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	File xmlFile = fileChooser.getSelectedFile();
            	Preferences.setImageDirectory(fileChooser.getCurrentDirectory());
            	insertXmlData(xmlFile);
            }
        }
    }
    
    private void clearEntries() {
    	anatField.setText("");
    	submitField.setText("");
    	tagEditorBrowseButton.setEnabled(false);
    }
    
    private void insertXmlData(File xmlFile) {
    	XmlReader xml = new XmlReader(xmlFile.getName(), xmlFile.getParent());
    	xml.readHeader(xmlFile.getName(), xmlFile.getParent(), "dataset.xsd");
    }
    
    private class XmlReader extends FileXML {
    	
    	public XmlReader(String fName, String fDir) {
    		super(fName, fDir);
    		
    		m_kHandler = new DataSetHandler();
    	}
    	
    }
    
    private class DataSetHandler extends DefaultHandler {
    	
    	protected JTextComponent receiver;
    	
    	private String text;
    	
    	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
    		super.startElement(uri, localName, qName, attributes);
    		if(localName.equals("source-name")) {
    			receiver = sourceNameField;
    		} else if(localName.equals("source-org")) {
    			receiver = sourceOrgField;
    		} else if(localName.equals("source-project")) {
    			receiver = sourceProjField;
    		} else if(localName.equals("testing-information")) {
    			receiver = testingArea;
    		} else if(localName.equals("details")) {
    			receiver = detailArea;
    		} else if(localName.equals("anatomical-area")) {
    			receiver = anatField;
    		}
    		text = new String();
    	}
    	
    	public void endElement(String uri, String localName, String qName) throws SAXException {
    		super.endElement(uri, localName, qName);
    		if(receiver != null && text.length() > 0) {
    			receiver.setText(text);
    		}
    		receiver = null;
    		text = null;
    	}
    	
    	public void characters(char[] ch, int start, int length) {
    		if(receiver == null) {
    			return;
    		}
    		for(int i=start; i<start+length; i++) {
    			text += ch[i];
    		}
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
    	algoImageSubmit.finalize();
    	algoImageSubmit = null;
    	
    }
    
    protected void setGUIFromParams() {
    	
    }
    protected void storeParamsFromGUI() {
    	
    }
    
    private class InformationUpdate implements Runnable {

    	private Vector<File> fileListUpdate;
    	private boolean likelyActionable;
    	private FileDicom imageFile;
    	private PlugInDialogImageSubmit parent;
    	
		public InformationUpdate(Vector<String> fileListString, PlugInDialogImageSubmit parent) {
			this.parent = parent;
			this.fileListUpdate = new Vector<File>();
			for(int i=0; i<fileListString.size(); i++) {
				fileListUpdate.add(new File(fileListString.get(i)));
			}
			this.likelyActionable = examineFileList(fileListUpdate);
		}
    	
		public void run() {
			if(!likelyActionable) {
				return;
			}
			FileInfoBase info = null;
			try {
	    		imageFile = new FileDicom(fileListUpdate.get(0).getName(), fileListUpdate.get(0).getParent()+File.separator);
	            imageFile.setQuiet(true); // if we want quiet, we tell the reader, too.
	            imageFile.readHeader(true); // can we read the header?	
	            info = imageFile.getFileInfo();
			} catch(Exception e) {
				e.printStackTrace();
			}
            if(info instanceof FileInfoDicom) {
            	FileDicomTagTable tagTable = ((FileInfoDicom) info).getTagTable();
            	Object obj = tagTable.get(new FileDicomKey("0018,0015"));
            	if(obj instanceof FileDicomTag) {
            		if(((FileDicomTag) obj).getValue(true).toString().length() > 0) {
            			anatField.setText(((FileDicomTag) obj).getValue(true).toString());
            		}
            	}
            	
            	currentTagEditor = new TagEditorDialog(tagTable, parent);
            }
		}
    	
    	private boolean examineFileList(Vector<File> f) {
    		if(f.size() == 0) {
    			return false;
    		} else if(f.size() == 1) {
    			return true;
    		} else {
    			return true;
    		}	
    	}	
    	
    	public FileDicom getImageFile() {
    		return imageFile;
    	}
    	
    	public boolean isActionable() {
    		return likelyActionable;
    	}
    }
    
    private class TagEditorDialog extends JDialogBase implements ListSelectionListener {

    	/**For adding a top level dicom tag*/
    	private static final String ADD_TAG = "Add";
    	
    	/**For adding a dicom tag contained within a sequence*/
    	private static final String ADD_TAG_SEQ = "Add sequence tag";

    	/**Closes the dialog*/
		private static final String CLOSE = "Close";

		/**For removing all tags from the main dialog*/
		private static final String CLEAR_TAGS = "Clear tags";
		
		/**Indicates tag value may exist but cannot be understood by the dialog. */
		private static final String UNKNOWN = "Unknown";
		
		/**Indicates tag value is a sequence tag*/
		private static final String SEQUENCE = "Sequence";

		/**Indicates tag is a private tag*/
		private static final String PRIVATE = "Private tag";
		
		/**Min/max size of all text boxes contained in scroll panes.*/
		private final Dimension SCROLL_PANE_SIZE = new Dimension(200, 20);

		/**List of all file's elements for each group of a FileDicomKey set*/
		private TreeMap<String, ArrayList<String>> groupToElement, groupToElementSeq;
    	
		/**FileDicomTag to its name in the Dicom dictionary*/
    	private TreeMap<String, String> keyToName, keyToNameSeq;

    	/**FileDicomTag to its value in the file*/
    	private TreeMap<String, String> keyToValue, keyToValueSeq;

		/**Panels used by this dialog box*/
		private JPanel tagSelectorPanel, tagInformationPanel, sequenceInformationPanel;

		/**Lists used to display available DICOM tags*/
		private JList groupList, elementList;
		
		/**Combo boxes used for sequence tags*/
		private JComboBox groupCombo, elementCombo;

		/**Buttons used by this dialog*/
		private JButton addButton, addButtonSeq, clearButton, closeButton;

		/**Name and property labels that describe a DICOM tag for a particular file*/
		private JLabel nameValue, nameValueSeq, propertyValue, propertyValueSeq;
    	
		/**Original tag table for this file*/
		private FileDicomTagTable tagTable;
		
		public TagEditorDialog(FileDicomTagTable tagTable, PlugInDialogImageSubmit parent) {
			super(parent, false);
			
			this.tagTable = tagTable;
			
			//these data structures relate to the top level dicom tags
			this.groupToElement = new TreeMap<String, ArrayList<String>>();
			this.keyToName = new TreeMap<String, String>();
			this.keyToValue = new TreeMap<String, String>();
			this.setResizable(false);
			
			buildGroupElementMap(tagTable);
			
			init();
		}
		
		public void init() {
			setForeground(Color.black);
	        setTitle("Tag Selector Tool");
	        
	        tagSelectorPanel = buildTagSelectorPanel();
	        
	        tagInformationPanel = buildTagInfoPanel();
	        
	        sequenceInformationPanel = buildSequenceInfoPanel();
	        
	        add(tagSelectorPanel, BorderLayout.NORTH);
	        add(tagInformationPanel, BorderLayout.CENTER);
	        
	        pack();
		}
		
		private JPanel buildTagSelectorPanel() {
			GridBagLayout tagSelectorGridBagLayout = new GridBagLayout();
	        GridBagConstraints selectorPanelConstraints = new GridBagConstraints();
	        selectorPanelConstraints.anchor = GridBagConstraints.NORTH;

	        JPanel tagSelectorPanel = new JPanel(tagSelectorGridBagLayout);
	        tagSelectorPanel.setBorder(MipavUtil.buildTitledBorder("Select DICOM tags"));
	        
	        // Dialog column
	        selectorPanelConstraints.gridx = 0;
	        selectorPanelConstraints.gridy = 0;
	        selectorPanelConstraints.gridheight = 4;
	        selectorPanelConstraints.insets = new Insets(5, 5, 5, 5);
	        selectorPanelConstraints.anchor = GridBagConstraints.CENTER;
	        JLabel firstLabel = new JLabel("<html>Select<br>tags:</html>");
	        tagSelectorPanel.add(firstLabel, selectorPanelConstraints);
	        
	        // Group Column
	        selectorPanelConstraints.gridx = 1;
	        selectorPanelConstraints.gridy = 0;
	        selectorPanelConstraints.gridheight = 1;
	        selectorPanelConstraints.anchor = GridBagConstraints.NORTHWEST;
	        selectorPanelConstraints.insets = new Insets(5, 15, 5, 15);
	        JLabel groupLabel = new JLabel("Group:");
	        tagSelectorPanel.add(groupLabel, selectorPanelConstraints);
	        
	        selectorPanelConstraints.gridx = 1;
	        selectorPanelConstraints.gridy = 1;
	        selectorPanelConstraints.gridheight = 3;
	        
	        selectorPanelConstraints.anchor = GridBagConstraints.CENTER;
	        Vector<String> vGroup;
	        Collections.sort(vGroup = new Vector<String>(groupToElement.keySet()), new NumberComparator());
	        groupList = new JList(vGroup);
	        groupList.setSelectedIndex(0);
	        groupList.setVisibleRowCount(4);
	        groupList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	        groupList.setMinimumSize(new Dimension(150, 94));
	        groupList.setMaximumSize(new Dimension(150, 500));
	        groupList.getSelectionModel().addListSelectionListener(this);
	        JScrollPane scrollPaneGroup = new JScrollPane(groupList);
	        scrollPaneGroup.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	        scrollPaneGroup.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);    
	        scrollPaneGroup.setMinimumSize(new Dimension(150, 94));
	        scrollPaneGroup.setPreferredSize(new Dimension(150, 94));
	        tagSelectorPanel.add(scrollPaneGroup, selectorPanelConstraints);
	        
	        // Element Column
	        selectorPanelConstraints.gridx = 2;
	        selectorPanelConstraints.gridy = 0;
	        selectorPanelConstraints.gridheight = 1;
	        selectorPanelConstraints.anchor = GridBagConstraints.NORTHWEST;
	        JLabel elementLabel = new JLabel("Element:");
	        tagSelectorPanel.add(elementLabel, selectorPanelConstraints);
	        
	        selectorPanelConstraints.gridx = 2;
	        selectorPanelConstraints.gridy = 1;
	        selectorPanelConstraints.gridheight = 3;
	        selectorPanelConstraints.anchor = GridBagConstraints.CENTER;
	        Vector<String> vElement;
	        Collections.sort(vElement = new Vector<String>(groupToElement.get(vGroup.get(0))), new NumberComparator());
	        elementList = new JList(vElement);
	        elementList.setSelectedIndex(0);
	        elementList.setVisibleRowCount(4);
	        elementList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
	        elementList.setMinimumSize(new Dimension(150, 94));
	        elementList.setMaximumSize(new Dimension(150, 500));
	        elementList.getSelectionModel().addListSelectionListener(this);
	        JScrollPane scrollPaneElement = new JScrollPane(elementList);
	        scrollPaneElement.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	        scrollPaneElement.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);    
	        scrollPaneElement.setMinimumSize(new Dimension(150, 94));
	        scrollPaneElement.setPreferredSize(new Dimension(150, 94));
	        tagSelectorPanel.add(scrollPaneElement, selectorPanelConstraints);
	        
	        //Buttons Column
	        selectorPanelConstraints.gridx = 3;
	        selectorPanelConstraints.gridy = 1;
	        selectorPanelConstraints.gridheight = 1;
	        selectorPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
	        selectorPanelConstraints.insets = new Insets(5, 5, 5, 5);
	        addButton = new JButton("Add");
	        addButton.setActionCommand(ADD_TAG);
	        addButton.addActionListener(this);
	        tagSelectorPanel.add(addButton, selectorPanelConstraints);
	        
	        selectorPanelConstraints.gridx = 3;
	        selectorPanelConstraints.gridy = 2;
	        clearButton = new JButton("Clear");
	        clearButton.setActionCommand(CLEAR_TAGS);
	        clearButton.addActionListener(this);
	        tagSelectorPanel.add(clearButton, selectorPanelConstraints);
	        
	        selectorPanelConstraints.gridx = 3;
	        selectorPanelConstraints.gridy = 3;
	        closeButton = new JButton("Close");
	        closeButton.setActionCommand(CLOSE);
	        closeButton.addActionListener(this);
	        tagSelectorPanel.add(closeButton, selectorPanelConstraints);
	        
	        return tagSelectorPanel;
		}
		
		private JPanel buildTagInfoPanel() {
			GridBagLayout tagInfoGridBagLayout = new GridBagLayout();
	        GridBagConstraints infoPanelConstraints = new GridBagConstraints();
	        infoPanelConstraints.anchor = GridBagConstraints.NORTH;

	        JPanel tagInformationPanel = new JPanel(tagInfoGridBagLayout);
	        tagInformationPanel.setBorder(MipavUtil.buildTitledBorder("DICOM tag information"));
	        
	        //tag name row
	        infoPanelConstraints.gridx = 0;
	        infoPanelConstraints.gridy = 0;
	        infoPanelConstraints.insets = new Insets(5, 10, 5, 10);
	        infoPanelConstraints.anchor = GridBagConstraints.NORTHWEST;
	        infoPanelConstraints.fill = GridBagConstraints.BOTH;
	        infoPanelConstraints.weightx = 0;
	        JLabel nameLabel = new JLabel("Tag name:");
	        tagInformationPanel.add(nameLabel, infoPanelConstraints);
	        
	        infoPanelConstraints.gridx = 1;
	        infoPanelConstraints.gridy = 0;
	        infoPanelConstraints.weightx = 1;
	        String tagName = groupList.getSelectedValue().toString()+","+elementList.getSelectedValue().toString();
	        nameValue = new JLabel(keyToName.get(tagName));
	        JScrollPane namePane = new JScrollPane(nameValue);
	        namePane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	        namePane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
	        namePane.setBorder(null);
	        namePane.setMinimumSize(SCROLL_PANE_SIZE);
	        namePane.setPreferredSize(SCROLL_PANE_SIZE);
	        namePane.setPreferredSize(SCROLL_PANE_SIZE);
	        tagInformationPanel.add(namePane, infoPanelConstraints);
	        
	        //tag value row
	        infoPanelConstraints.gridx = 0;
	        infoPanelConstraints.gridy = 1;
	        infoPanelConstraints.weightx = 0;
	        JLabel propertyLabel = new JLabel("Tag value:");
	        tagInformationPanel.add(propertyLabel, infoPanelConstraints);
	        
	        infoPanelConstraints.gridx = 1;
	        infoPanelConstraints.gridy = 1;
	        infoPanelConstraints.weightx = 1;
	        propertyValue = new JLabel(keyToValue.get(tagName));
	        JScrollPane propPane = new JScrollPane(propertyValue);
	        propPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	        propPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
	        propPane.setMinimumSize(SCROLL_PANE_SIZE);
	        propPane.setMaximumSize(SCROLL_PANE_SIZE);
	        propPane.setPreferredSize(SCROLL_PANE_SIZE);
	        propPane.setBorder(null);
	        tagInformationPanel.add(propPane, infoPanelConstraints);
	        
	        return tagInformationPanel;
		}
		
		private JPanel buildSequenceInfoPanel() {
			GridBagLayout seqInfoGridBagLayout = new GridBagLayout();
			GridBagConstraints seqPanelConstraints = new GridBagConstraints();
			seqPanelConstraints.anchor = GridBagConstraints.NORTH;
			
			JPanel sequenceInformationPanel = new JPanel(seqInfoGridBagLayout);
			sequenceInformationPanel.setBorder(MipavUtil.buildTitledBorder("Sequence tag information"));
			
			// Dialog column
	        seqPanelConstraints.gridx = 0;
	        seqPanelConstraints.gridy = 0;
	        seqPanelConstraints.weightx = 0;
	        seqPanelConstraints.gridheight = 2;
	        seqPanelConstraints.insets = new Insets(5, 5, 5, 5);
	        seqPanelConstraints.anchor = GridBagConstraints.CENTER;
	        JLabel firstLabel = new JLabel("Select tags:");
	        sequenceInformationPanel.add(firstLabel, seqPanelConstraints);
	        
	        // Group Column
	        seqPanelConstraints.gridx = 1;
	        seqPanelConstraints.gridy = 0;
	        seqPanelConstraints.weightx = 1;
	        seqPanelConstraints.gridheight = 1;
	        seqPanelConstraints.anchor = GridBagConstraints.WEST;
	        seqPanelConstraints.insets = new Insets(5, 15, 5, 15);
	        JLabel groupLabel = new JLabel("Group:");
	        sequenceInformationPanel.add(groupLabel, seqPanelConstraints);
	        
	        seqPanelConstraints.gridx = 1;
	        seqPanelConstraints.gridy = 1;
	        groupCombo = new JComboBox();
	        groupCombo.addActionListener(this);
	        groupCombo.setMinimumSize(new Dimension(100, 26));
	        groupCombo.setPreferredSize(new Dimension(100, 26));
	        sequenceInformationPanel.add(groupCombo, seqPanelConstraints);
	        
	        // Element Column
	        seqPanelConstraints.gridx = 2;
	        seqPanelConstraints.gridy = 0;
	        JLabel elementLabel = new JLabel("Element:");
	        sequenceInformationPanel.add(elementLabel, seqPanelConstraints);
	        
	        seqPanelConstraints.gridx = 2;
	        seqPanelConstraints.gridy = 1;
	        elementCombo = new JComboBox();
	        elementCombo.addActionListener(this);
	        elementCombo.setMinimumSize(new Dimension(100, 26));
	        elementCombo.setPreferredSize(new Dimension(100, 26));
	        sequenceInformationPanel.add(elementCombo, seqPanelConstraints);
	        
	        //tag name row
	        seqPanelConstraints.gridx = 0;
	        seqPanelConstraints.gridy = 2;
	        seqPanelConstraints.insets = new Insets(5, 10, 5, 10);
	        seqPanelConstraints.anchor = GridBagConstraints.WEST;
	        seqPanelConstraints.fill = GridBagConstraints.BOTH;
	        seqPanelConstraints.weightx = 0;
	        JLabel nameLabel = new JLabel("Tag name:");
	        sequenceInformationPanel.add(nameLabel, seqPanelConstraints);
	        
	        seqPanelConstraints.gridx = 1;
	        seqPanelConstraints.gridy = 2;
	        seqPanelConstraints.weightx = 1;
	        seqPanelConstraints.gridwidth = 3;
	        nameValueSeq = new JLabel("Name");
	        JScrollPane namePane = new JScrollPane(nameValueSeq);
	        namePane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	        namePane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
	        namePane.setBorder(null);
	        namePane.setMinimumSize(SCROLL_PANE_SIZE);
	        namePane.setPreferredSize(SCROLL_PANE_SIZE);
	        namePane.setPreferredSize(SCROLL_PANE_SIZE);
	        sequenceInformationPanel.add(namePane, seqPanelConstraints);
	        
	        //tag value row
	        seqPanelConstraints.gridx = 0;
	        seqPanelConstraints.gridy = 3;
	        seqPanelConstraints.weightx = 0;
	        JLabel propertyLabel = new JLabel("Tag value:");
	        sequenceInformationPanel.add(propertyLabel, seqPanelConstraints);
	        
	        seqPanelConstraints.gridx = 1;
	        seqPanelConstraints.gridy = 3;
	        seqPanelConstraints.weightx = 1;
	        seqPanelConstraints.gridwidth = 3;
	        propertyValueSeq = new JLabel("Value");
	        JScrollPane propPane = new JScrollPane(propertyValueSeq);
	        propPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	        propPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
	        propPane.setBorder(null);
	        propPane.setMinimumSize(SCROLL_PANE_SIZE);
	        propPane.setPreferredSize(SCROLL_PANE_SIZE);
	        propPane.setPreferredSize(SCROLL_PANE_SIZE);
	        sequenceInformationPanel.add(propPane, seqPanelConstraints);
	        
	        //button column
	        seqPanelConstraints.gridx = 3;
	        seqPanelConstraints.gridy = 1;
	        seqPanelConstraints.weightx = 0;
	        seqPanelConstraints.anchor = GridBagConstraints.CENTER;
	        addButtonSeq = new JButton("Add");
	        addButtonSeq.setMinimumSize(new Dimension(64, 26));
	        addButtonSeq.setPreferredSize(new Dimension(64, 26));
	        addButtonSeq.setActionCommand(ADD_TAG_SEQ);
	        addButtonSeq.addActionListener(this);
	        sequenceInformationPanel.add(addButtonSeq, seqPanelConstraints);
	        
	        return sequenceInformationPanel;
			
		}
		
		private void buildGroupElementMap(FileDicomTagTable tagTable) {
			Hashtable<FileDicomKey, FileDicomTag> tagHash = tagTable.getTagList();
			Enumeration<FileDicomKey> e = tagHash.keys();
			while(e.hasMoreElements()) {
				FileDicomKey key = e.nextElement();
				ArrayList<String> allElements = groupToElement.get(key.getGroup());
				if(allElements == null) {
					allElements = new ArrayList<String>(); 
					groupToElement.put(key.getGroup(), allElements);
				}
				if(Integer.valueOf(key.getElement(), 16) != 0) {
					allElements.add(key.getElement());
				}
				String tagName = DicomDictionary.getName(key);
				if(tagName == null || tagName.length() == 0) {
					keyToName.put(key.toString(), PRIVATE);
				} else {
					keyToName.put(key.toString(), tagName);
				}
				Object obj = tagTable.getValue(key);
				if(obj instanceof FileDicomSQ && ((FileDicomSQ)obj).getSequenceLength() > 0) {
					keyToValue.put(key.toString(), SEQUENCE);
				} else if(obj == null || (obj instanceof FileDicomSQ && ((FileDicomSQ)obj).getSequenceLength() == 0)) {
					keyToValue.put(key.toString(), UNKNOWN);
				} else {
					keyToValue.put(key.toString(), obj.toString());
				}
			}
		}
		
		private void buildSeqGroupElementMap(TreeMap<String, FileDicomTag> tagHash) {
			
			//these data structures are filled in only by an open sequence
			this.groupToElementSeq = new TreeMap<String, ArrayList<String>>();
			this.keyToNameSeq = new TreeMap<String, String>();
			this.keyToValueSeq = new TreeMap<String, String>();
			
			Iterator<String> e = tagHash.keySet().iterator();
			
			while(e.hasNext()) {
				FileDicomKey key = new FileDicomKey(e.next());
				ArrayList<String> allElements = groupToElementSeq.get(key.getGroup());
				if(allElements == null) {
					allElements = new ArrayList<String>(); 
					groupToElementSeq.put(key.getGroup(), allElements);
				}
				if(Integer.valueOf(key.getElement(), 16) != 0) {
					allElements.add(key.getElement());
				}
				String tagName = DicomDictionary.getName(key);
				if(tagName == null || tagName.length() == 0) {
					keyToNameSeq.put(key.toString(), PRIVATE);
				} else {
					keyToNameSeq.put(key.toString(), tagName);
				}
				Object obj = tagHash.get(key.toString()).getValue(true);
				if(obj instanceof FileDicomSQ) {
					keyToValueSeq.put(key.toString(), SEQUENCE);
				} else if(obj == null) {
					keyToValueSeq.put(key.toString(), UNKNOWN);
				} else {
					keyToValueSeq.put(key.toString(), obj.toString());
				}
			}
		}
    	
		public void actionPerformed(ActionEvent e) {
			if(e.getActionCommand().equals(CLEAR_TAGS)) {
				tagListTextField.setText("");
			} else if(e.getActionCommand().equals(ADD_TAG)) {
				if(!tagExistsInField(groupList.getSelectedValue()+","+elementList.getSelectedValue())) {
					String existingText = tagListTextField.getText();
					String prefix = existingText.length() == 0 || existingText.charAt(existingText.length()-1) == ';' ? "" : ";";
					tagListTextField.setText(existingText+prefix+groupList.getSelectedValue()+","+elementList.getSelectedValue()+";");
				}
			} else if(e.getActionCommand().equals(CLOSE)) {
				this.dispose();
			} else if(e.getActionCommand().equals(ADD_TAG_SEQ)) {
				if(!tagExistsInField(groupCombo.getSelectedItem()+","+elementCombo.getSelectedItem())) {
					String existingText = tagListTextField.getText();
					String prefix = existingText.length() == 0 || existingText.charAt(existingText.length()-1) == ';' ? "" : ";";
					tagListTextField.setText(existingText+prefix+groupCombo.getSelectedItem()+","+elementCombo.getSelectedItem()+";");
				}
			} else if(e.getSource() instanceof JComboBox) {
				if(e.getSource().equals(groupCombo)) {
					Vector<String> v;
					Collections.sort(v = new Vector<String>(groupToElementSeq.get(groupCombo.getSelectedItem())), new NumberComparator());
					elementCombo.setModel(new DefaultComboBoxModel(v));
					elementCombo.setSelectedIndex(0);
				}
				String tagName = groupCombo.getSelectedItem()+","+elementCombo.getSelectedItem();
				nameValueSeq.setText(keyToNameSeq.get(tagName));
				propertyValueSeq.setText(keyToValueSeq.get(tagName));
				sequenceInformationPanel.updateUI();
			}
		}
    	
    	private boolean tagExistsInField(String text) {
    		String[] tagList = tagListTextField.getText().split(";");
    		for(int i=0; i<tagList.length; i++) {
    			if(tagList[i].equals(text)) {
    				return true;
    			}
    		}
    		return false;
    	}
    	
		public void valueChanged(ListSelectionEvent e) {
			if(e.getSource().equals(groupList.getSelectionModel())) {
				Vector<String> vElement;
		        Collections.sort(vElement = new Vector<String>(groupToElement.get(groupList.getSelectedValue())), new NumberComparator());
		        elementList.setListData(vElement);
		        elementList.setSelectedIndex(0);
		        elementList.updateUI();
			}
			if(groupList.getSelectedIndex() != -1 && elementList.getSelectedIndex() != -1) {
				String tagName = groupList.getSelectedValue().toString()+","+elementList.getSelectedValue().toString();
				nameValue.setText(keyToName.get(tagName));
				propertyValue.setText(keyToValue.get(tagName));
				if(keyToValue.get(tagName).equals(SEQUENCE)) {
					add(sequenceInformationPanel, BorderLayout.SOUTH);
					FileDicomSQ sq = (FileDicomSQ)tagTable.getValue(tagName);
					FileDicomItem item = sq.getItem(0);
					buildSeqGroupElementMap(item.getDataSet());
					Vector<String> vGroup;
			        Collections.sort(vGroup = new Vector<String>(groupToElementSeq.keySet()), new NumberComparator());
					groupCombo.setModel(new DefaultComboBoxModel(vGroup));
					groupCombo.setSelectedIndex(0);
					Vector<String> vElement;
			        Collections.sort(vElement = new Vector<String>(groupToElementSeq.get(vGroup.get(0))), new NumberComparator());
					elementCombo.setModel(new DefaultComboBoxModel(vElement));
					elementCombo.setSelectedIndex(0);
					pack();
				} else {
					remove(sequenceInformationPanel);
					pack();
				}
				nameValue.updateUI();
				tagInformationPanel.updateUI();
			}
		}
    	
    	private class NumberComparator implements Comparator<String>, Serializable {

			public int compare(String o1, String o2) {
				return Integer.valueOf(o1, 16) - Integer.valueOf(o2, 16);
			}
    		
    	}
    }
    
    public static void main(String[] args) {
    	new PlugInDialogImageSubmit(true);
    }
    
}
