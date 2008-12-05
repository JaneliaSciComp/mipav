import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
import javax.swing.*;

import java.io.File;

/**
 * @author joshim2
 *
 */
public class PlugInDialogAnonymizeDICOM extends JDialogScriptableBase implements AlgorithmInterface {

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
    
    private JTextArea inputFileTextArea;

	/** Textfields **/
    private JTextField tagListTextField; 
   	
	/** File chooser object */
	private JFileChooser fileChooser;
	
	/** File selected by the user */
	private File[] selectedFiles;
	
	/** Additional tags to anonymize */
	private String[] tagArray;
	
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
    	init();
    	//setSeparateThread(false);
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
        mainPanelConstraints.gridheight = 2;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        inputFileLabel = new JLabel(" Input files : ");
        mainPanel.add(inputFileLabel, mainPanelConstraints);

        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 0;
        mainPanelConstraints.gridheight = 2;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        mainPanelConstraints.fill = GridBagConstraints.BOTH;
        inputFileTextArea = new JTextArea();
        inputFileTextArea.setEditable(false);
        inputFileTextArea.setRows(4);
        inputFileTextArea.setMinimumSize(new Dimension(300, 75));
        //inputFileTextArea.setPreferredSize(new Dimension(300, 75));
        inputFileTextArea.setMaximumSize(new Dimension(300, 500));
        JScrollPane scrollPane = new JScrollPane(inputFileTextArea);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);    
        scrollPane.setMinimumSize(new Dimension(300, 75));
        scrollPane.setPreferredSize(new Dimension(300, 75));
        mainPanel.add(scrollPane, mainPanelConstraints);

        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 0;
        mainPanelConstraints.gridheight = 1;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        mainPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        inputFileBrowseButton = new JButton("Browse");
        inputFileBrowseButton.addActionListener(this);
        inputFileBrowseButton.setActionCommand("inputFileBrowse");
        mainPanel.add(inputFileBrowseButton, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 2;
        mainPanelConstraints.gridy = 1;
        mainPanelConstraints.fill = GridBagConstraints.NONE;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        removeFileButton = new JButton("Remove All");
        removeFileButton.addActionListener(this);
        removeFileButton.setActionCommand("Remove All");
        mainPanel.add(removeFileButton, mainPanelConstraints);
        
        // Tag list
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 2;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 5);
        tagListLabel = new JLabel(" Anonymize additional tags : ");
        mainPanel.add(tagListLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 2;
        mainPanelConstraints.insets = new Insets(15, 5, 0, 0);
        tagListTextField = new JTextField(45);
        mainPanel.add(tagListTextField, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 3;
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
    		
    		String selectedFilesText = inputFileTextArea.getText();
    		String[] allFiles = selectedFilesText.split("\n");
    		selectedFiles = new File[allFiles.length];
    		for(int i=0; i<allFiles.length; i++) {
    			System.out.println("Working with file "+allFiles[i]);
    			selectedFiles[i] = new File(allFiles[i]);
    		}
    		
    		//Make algorithm.
    		algoAnonymizeDicom = new PlugInAlgorithmAnonymizeDicom(selectedFiles, tagArray);
    		
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
        	
        	Dimension d = new Dimension(700, 400);
            fileChooser.setMinimumSize(d);
            fileChooser.setPreferredSize(d);
            
            int returnVal = fileChooser.showOpenDialog(null);
                        
            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	selectedFiles = fileChooser.getSelectedFiles();
            	String totalText = inputFileTextArea.getText();
            	String[] fileLine = totalText.split("\n");
            	for(int i=0; i<selectedFiles.length; i++) {
            		boolean fileExists = false;
            		for(int j=0; j<fileLine.length; j++) {
            			if(selectedFiles[i].getAbsolutePath().equals(fileLine[j])) {
            				fileExists = true;
            			}
            		}
            		if(!fileExists) {
            			totalText = totalText + selectedFiles[i].getAbsolutePath()+"\n";
            		}
            	}
            	inputFileTextArea.setText(totalText);
            	System.out.println(inputFileTextArea.getText());
            	inputFileTextArea.validate();
            }
        } else if (command.equalsIgnoreCase("Cancel")) {
        	dispose();
        } else if (command.equalsIgnoreCase("OK")) {
        	createTagArray();
        	callAlgorithm();
        } else if(command.equalsIgnoreCase("Remove All")) {
        	inputFileTextArea.setText("");
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
