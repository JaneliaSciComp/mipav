import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileXML;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import javax.swing.*;
import javax.swing.text.JTextComponent;

import org.xml.sax.Attributes;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Vector;

/**
 * @author joshim2
 *
 */
public class PlugInDialogCreateXML extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

	// ~ Static fields -------------------------------------------------------------------------
	
	public static final String BROWSE_FILE = "Browse file";

	public static final String REMOVE = "Remove file";
	
	public static final String REMOVE_ALL = "Remove all file";
	
	public static final String IMPORT_XML = "Import XML";
	
	// ~ Instance fields ------------------------------------------------------------------------
	
	/** GridBagLayout * */
    private GridBagLayout mainPanelGridBagLayout;
    
    /** GridBagConstraints * */
    private GridBagConstraints mainPanelConstraints;
    
    /** panels * */
    private JPanel mainPanel, OKCancelPanel;
    
    /** Labels **/
    private JLabel inputFileLabel, anatLabel, sourceNameLabel, sourceOrgLabel, 
    				sourceProjLabel, testingLabel, detailLabel;
    
    /** Buttons **/
    private JButton inputFileBrowseButton;
    
    private JButton removeFileButton, removeAllFileButton;
    
    private JButton importButton;
    
    /**All files to generate image information for*/
    private JList inputFileList;

	/** Textfields **/
    private JTextField anatField, sourceNameField, sourceOrgField,
    					sourceProjField; 
   	
    /** Text areas. */
    private JTextArea testingArea, detailArea;
    
	/** File chooser object */
	private JFileChooser fileChooser;
	
	/** Files selected by the user */
	private File[] selectedFiles;
	
	/** List of current files and folders to work with */
	private Vector<String> fileList;
	
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
        
    	init();
    }
    
    // ~ Methods ----------------------------------------------------------------------------------
    
    public void init() {
    	setForeground(Color.black);
        setTitle("Image XML Creation Tool");
        
        mainPanelGridBagLayout = new GridBagLayout();
        mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.NORTH;

        mainPanel = new JPanel(mainPanelGridBagLayout);
        
        // Image Panel
        JPanel imagePanel = buildImagePanel();
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
    	JPanel imagePanel = new JPanel(new GridBagLayout());
        imagePanel.setBorder(MipavUtil.buildTitledBorder("Image information"));
        GridBagConstraints imageConstraints = new GridBagConstraints();
        imageConstraints.gridx = 0;
        imageConstraints.gridy = 0;
        imageConstraints.gridheight = 3;
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
        inputFileBrowseButton.setActionCommand(BROWSE_FILE);
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
        
        // Anatomical area
        imageConstraints.gridx = 0;
        imageConstraints.gridy = 3;
        imageConstraints.insets = new Insets(15, 5, 15, 5);
        anatLabel = new JLabel(" Anatomical Area : ");
        imagePanel.add(anatLabel, imageConstraints);
        
        imageConstraints.gridx = 1;
        imageConstraints.gridy = 3;
        imageConstraints.anchor = GridBagConstraints.WEST;
        imageConstraints.insets = new Insets(15, 70, 5, 5);
        anatField = new JTextField(45);
        imagePanel.add(anatField, imageConstraints);
        
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
        provConstraints.insets = new Insets(15, 5, 15, 5);
        sourceNameLabel = new JLabel(" Source name:  ");
        subProv.add(sourceNameLabel, provConstraints);
        
        provConstraints.gridx = 1;
        provConstraints.gridy = 0;
        provConstraints.weightx = 1;
        provConstraints.anchor = GridBagConstraints.WEST;
        provConstraints.insets = new Insets(10, 50, 5, 0);
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
        provConstraints.anchor = GridBagConstraints.CENTER;
        provConstraints.fill = GridBagConstraints.NONE;
        provConstraints.insets = new Insets(15, 5, 15, 5);
        sourceOrgLabel = new JLabel(" Source organization:  ");
        subProv.add(sourceOrgLabel, provConstraints);
        
        provConstraints.gridx = 1;
        provConstraints.gridy = 1;
        provConstraints.weightx = 1;
        provConstraints.anchor = GridBagConstraints.WEST;
        provConstraints.insets = new Insets(10, 50, 5, 5);
        sourceOrgField = new JTextField(45);
        subProv.add(sourceOrgField, provConstraints);
        
        //source project
        provConstraints.gridx = 0;
        provConstraints.gridy = 2;
        provConstraints.weightx = 0;
        provConstraints.anchor = GridBagConstraints.CENTER;
        provConstraints.insets = new Insets(15, 5, 15, 5);
        sourceProjLabel = new JLabel(" Source project:  ");
        subProv.add(sourceProjLabel, provConstraints);
        
        provConstraints.gridx = 1;
        provConstraints.gridy = 2;
        provConstraints.weightx = 1;
        provConstraints.anchor = GridBagConstraints.WEST;
        provConstraints.insets = new Insets(10, 50, 5, 5);
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
        subBagConstraints.insets = new Insets(15, 5, 15, 5);
        testingLabel = new JLabel(" Testing information:  ");
        subPanelDetail.add(testingLabel, subBagConstraints);
        
        subBagConstraints.gridx = 1;
        subBagConstraints.gridy = 0;
        subBagConstraints.weightx = 1;
        subBagConstraints.anchor = GridBagConstraints.WEST;
        subBagConstraints.insets = new Insets(10, 50, 5, 5);
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
        subBagConstraints.anchor = GridBagConstraints.NORTH;
        subBagConstraints.insets = new Insets(15, 5, 15, 5);
        detailLabel = new JLabel(" Details:  ");
        subPanelDetail.add(detailLabel, subBagConstraints);
        
        subBagConstraints.gridx = 1;
        subBagConstraints.gridy = 2;
        subBagConstraints.weightx = 1;
        subBagConstraints.anchor = GridBagConstraints.WEST;
        subBagConstraints.insets = new Insets(10, 50, 5, 5);
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
    
    /**
     * Once all the necessary variables are set, call the DICOM anonymizer algorithm.
     */
    
    protected void callAlgorithm() {
    
    	try{
    		System.gc();
    		selectedFiles = new File[fileList.size()];
    		for(int i=0; i<fileList.size(); i++) {
    			System.out.println("Working with file "+fileList.get(i));
    			selectedFiles[i] = new File(fileList.get(i));
    		}
    		
    		//map keys exact xml tag to particular text
    		HashMap<String, String> map = new HashMap<String, String>();
    		map.put("anatomical-area", anatField.getText());
    		map.put("source-name", sourceNameField.getText());
    		map.put("source-org", sourceOrgField.getText());
    		map.put("source-project", sourceProjField.getText());
    		map.put("testing-information", testingArea.getText());
    		map.put("details", detailArea.getText());
    		
    		//Make algorithm.
    		algoCreateXML = new PlugInAlgorithmCreateXML(selectedFiles, map, selectedFiles[0].getParent());
    		
    		// This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algoCreateXML.addListener(this);
            
            createProgressBar("Create Image XML Tool", algoCreateXML);
            
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
            					fileList.add(f.getAbsolutePath());
            				}
            			} else {
            				fileList.add(selectedFiles[i].getAbsolutePath());
            			}
            		}
            	}
            	inputFileList.updateUI();
            }
        } else if (command.equalsIgnoreCase("Cancel")) {
        	dispose();
        } else if (command.equalsIgnoreCase("OK")) {
        	callAlgorithm();
        } else if(command.equals(REMOVE_ALL)) {
        	fileList.removeAllElements();
        	inputFileList.updateUI();
        } else if(command.equals(REMOVE)) {
        	Object[] objAr = inputFileList.getSelectedValues();
        	for(Object obj : objAr) {
        		fileList.remove(obj);
        	}
        	inputFileList.updateUI();
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
    	ArrayList<String> xmlList = algoCreateXML.getXmlList();
    	String totalStr = new String();
    	for(int i=0; i<xmlList.size(); i++) {
    		totalStr += xmlList.get(i) + "\n";
    	}
    	if(totalStr.length() > 0) {
    		MipavUtil.displayInfo("The following XML files have been written:\n"+totalStr);
    	} else {
    		MipavUtil.displayError("No XML files have been written.");
    	}
    	
    	progressBar.dispose();
    	algoCreateXML.finalize();
    	algoCreateXML = null;
    	
    }
    
    protected void setGUIFromParams() {
    	
    }
    protected void storeParamsFromGUI() {
    	
    }
    
    public static void main(String[] args) {
    	new PlugInDialogCreateXML(true);
    }
    
}
