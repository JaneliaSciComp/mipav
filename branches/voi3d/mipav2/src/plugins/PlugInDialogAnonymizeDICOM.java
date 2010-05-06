import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
import javax.swing.*;

import java.io.File;
import java.util.ArrayList;
import java.util.Vector;

/**
 * @author joshim2
 *
 */
public class PlugInDialogAnonymizeDICOM extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

	// ~ Instance fields ------------------------------------------------------------------------
	
	/** GridBagLayout * */
    private GridBagLayout mainPanelGridBagLayout;
    
    /** GridBagConstraints * */
    private GridBagConstraints mainPanelConstraints;
    
    /** panels * */
    private JPanel mainPanel, OKCancelPanel;
    
    /** Labels **/
    private JLabel inputFileLabel, tagListLabel, tagListSampleLabel;
    
    /** Buttons **/
    private JButton inputFileBrowseButton;
    
    private JButton removeFileButton;
    
    private JButton removeAllButton;
    
    private JList inputFileList;

	/** Textfields **/
    private JTextField tagListTextField; 
   	
	/** File chooser object */
	private JFileChooser fileChooser;
	
	/** File selected by the user */
	private File[] selectedFiles;
	
	/** Additional tags to anonymize */
	private String[] tagArray;
	
	/** List of current files to work with */
	private Vector<String> fileList;
	
	/** Algorithm instance */
    private PlugInAlgorithmAnonymizeDicom algoAnonymizeDicom;
	
	
	//	~ Constructors --------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public PlugInDialogAnonymizeDICOM() { }
    
    /**
     * Sets up variables but does not show dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogAnonymizeDICOM(boolean modal) {
        super(modal); 
        fileList = new Vector<String>();
       
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
        JScrollPane scrollPane = new JScrollPane(inputFileList);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);    
        scrollPane.setMinimumSize(new Dimension(300, 94));
        scrollPane.setPreferredSize(new Dimension(300, 94));
        mainPanel.add(scrollPane, mainPanelConstraints);

        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 0;
        mainPanelConstraints.gridheight = 1;
        mainPanelConstraints.insets = new Insets(15, 5, 5, 5);
        mainPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        inputFileBrowseButton = new JButton("Browse");
        inputFileBrowseButton.addActionListener(this);
        inputFileBrowseButton.setActionCommand("inputFileBrowse");
        mainPanel.add(inputFileBrowseButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 1;
        mainPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        mainPanelConstraints.insets = new Insets(5, 5, 5, 5);
        removeFileButton = new JButton("Remove");
        removeFileButton.addActionListener(this);
        removeFileButton.setActionCommand("Remove");
        mainPanel.add(removeFileButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 2;
        mainPanelConstraints.fill = GridBagConstraints.NONE;
        mainPanelConstraints.insets = new Insets(5, 5, 15, 5);
        removeAllButton = new JButton("Remove All");
        removeAllButton.addActionListener(this);
        removeAllButton.setActionCommand("Remove All");
        mainPanel.add(removeAllButton, mainPanelConstraints);
        
        // Tag list
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 3;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        tagListLabel = new JLabel(" Anonymize additional tags : ");
        mainPanel.add(tagListLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 3;
        mainPanelConstraints.insets = new Insets(15, 5, 0, 0);
        tagListTextField = new JTextField(45);
        mainPanel.add(tagListTextField, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 4;
        mainPanelConstraints.insets = new Insets(1, 5, 15, 5);
        tagListSampleLabel = new JLabel(" Format: group,element;group,element e.g. 0002,0000;0002,0001  ");
        mainPanel.add(tagListSampleLabel, mainPanelConstraints);
        
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
    		selectedFiles = new File[fileList.size()];
    		for(int i=0; i<fileList.size(); i++) {
    			System.out.println("Working with file "+fileList.get(i));
    			selectedFiles[i] = new File(fileList.get(i));
    		}
    		
    		//Make algorithm.
    		algoAnonymizeDicom = new PlugInAlgorithmAnonymizeDicom(selectedFiles, tagArray, selectedFiles[0].getParent(), selectedFiles[0].getParent()+File.separator+"Anon"+File.separator);
    		
    		// This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algoAnonymizeDicom.addListener(this);
            
            createProgressBar("DICOM Anonymization Tool", algoAnonymizeDicom);
            
            setVisible(false);
            
            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoAnonymizeDicom.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoAnonymizeDicom.run();
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
    	if (command.equalsIgnoreCase("inputFileBrowse")) {
    		
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
        	createTagArray();
        	if(selectedFiles.length > 0)
        		callAlgorithm();
        	else
        		MipavUtil.displayError("No files were selected to process");
        } else if(command.equalsIgnoreCase("Remove All")) {
        	fileList.removeAllElements();
        	inputFileList.updateUI();
        } else if(command.equals("Remove")) {
        	Object[] objAr = inputFileList.getSelectedValues();
        	for(Object obj : objAr) {
        		fileList.remove(obj);
        	}
        	inputFileList.updateUI();
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
    	algoAnonymizeDicom.finalize();
    	algoAnonymizeDicom = null;
    	
    }
    
    protected void setGUIFromParams() {
    	
    }
    protected void storeParamsFromGUI() {
    	
    }
    
    
    
}
