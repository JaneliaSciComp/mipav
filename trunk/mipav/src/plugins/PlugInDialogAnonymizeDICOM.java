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
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
import javax.swing.*;

import java.io.File;

/**
 * @author joshim2
 *
 */
public class PlugInDialogAnonymizeDICOM extends JDialogScriptableBase implements AlgorithmInterface, MouseListener, KeyListener {

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
    
    private SelectMode currentMode = SelectMode.NONE;
    
    private int selectedRow = -1;
	
    public enum SelectMode {
    	CTRL_MODE,
    	
    	SHIFT_MODE,
    	
    	NONE
    }
	
	
	//	~ Constructors --------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public PlugInDialogAnonymizeDICOM() { 
    	
    	currentMode = SelectMode.NONE;
    }
    
    /**
     * Sets up variables but does not show dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogAnonymizeDICOM(boolean modal) {
        super(modal); 
    	init();
    	
    	currentMode = SelectMode.NONE;
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
        mainPanelConstraints.gridheight = 3;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        inputFileLabel = new JLabel(" Input files : ");
        mainPanel.add(inputFileLabel, mainPanelConstraints);

        mainPanelConstraints.gridx = 1;
        mainPanelConstraints.gridy = 0;
        mainPanelConstraints.gridheight = 3;
        mainPanelConstraints.insets = new Insets(15, 5, 15, 0);
        mainPanelConstraints.fill = GridBagConstraints.BOTH;
        inputFileTextArea = new JTextArea();
        inputFileTextArea.setEditable(false);
        inputFileTextArea.setRows(4);
        inputFileTextArea.setMinimumSize(new Dimension(300, 94));
        inputFileTextArea.addMouseListener(this);
        inputFileTextArea.setMaximumSize(new Dimension(300, 500));
        JScrollPane scrollPane = new JScrollPane(inputFileTextArea);
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
        addKeyListener(this);
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
            	Preferences.setImageDirectory(fileChooser.getCurrentDirectory());
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
            	requestFocus();
            	requestFocusInWindow();
            }
        } else if (command.equalsIgnoreCase("Cancel")) {
        	dispose();
        } else if (command.equalsIgnoreCase("OK")) {
        	createTagArray();
        	callAlgorithm();
        } else if(command.equalsIgnoreCase("Remove All")) {
        	inputFileTextArea.setText("");
        } else if(command.equals("Remove")) {
        	if(selectedRow != -1) {
        		String replaceText = "";
        		String[] totalText = inputFileTextArea.getText().split("\n");
        		for(int i=0; i<totalText.length; i++) {
        			if(i != selectedRow) {
        				replaceText = replaceText + totalText[i]+"\n";
        			}
        		}
        		inputFileTextArea.setText(replaceText);
        	}
        }
    }
    
    public void keyPressed(KeyEvent e) {
		System.out.println("Looking at key codes");
    	switch(e.getKeyCode()) {
		
		case KeyEvent.VK_CONTROL:
			currentMode = SelectMode.CTRL_MODE;
			break;
			
		case KeyEvent.VK_SHIFT:
			currentMode = SelectMode.SHIFT_MODE;
			break;
			
			default:
				currentMode = SelectMode.NONE;
		}
		
	}

	public void keyReleased(KeyEvent e) {
		currentMode = SelectMode.NONE;
	}

	public void keyTyped(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseClicked(MouseEvent e) {
		switch(currentMode) {
		
		case CTRL_MODE:
			System.out.println("In control mode");
			break;
			
		case SHIFT_MODE:
			System.out.println("In shift mode");
			break;
			
		case NONE:
			System.out.println("Not in a mode");
			break;
		
		}
		
		selectedRow = e.getY()/16;
		int selectStart;
		System.out.println("Selected Row "+selectedRow);
		String[] ar = inputFileTextArea.getText().split("\n");
		if(selectedRow < ar.length) {
			inputFileTextArea.setSelectionStart(selectStart = inputFileTextArea.getText().indexOf(ar[selectedRow]));
			inputFileTextArea.setSelectionEnd(selectStart+ar[selectedRow].length());
		} 
	}

	public void mouseEntered(MouseEvent e) {
		
	}

	public void mouseExited(MouseEvent e) {
		
	}

	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
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
