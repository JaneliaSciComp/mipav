import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.LineBorder;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

/**
 * @author pandyan
 * 
 * This is the main dialog for the DTI Par Rec Sorting Process Plug-In
 * 
 * References: This algorithm was developed in concert with Okan Irfanoglu, Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 *
 */
public class PlugInDialogDTIParRecSortingProcess extends JDialogScriptableBase
		implements AlgorithmInterface {
    
    /** path to Par file **/
    private JTextField parRecFilePathTextField;
    
    /** path to gradient file (necessary for versions < 4.1) **/
    private JTextField gradientFilePathTextField;
    
    /** browse button for rec file**/
    private JButton parRecFilePathBrowseButton;
    
    /** browse button for gradient file **/
    private JButton gradientFilePathBrowseButton;
    
    /** current directory  **/
    private String currDir = null;
    
    /** handle to algorithm **/
    private PlugInAlgorithmDTIParRecSortingProcess alg;
    
    /** file directory **/
    private String fileDir;
    
    /** file name **/
    private String fileName;
    
    /** path to gradient file (only necessary for version 4 of Par/Rec) **/
	private String gradientFilePath = null;
    
    /** output text area **/
    protected JTextArea outputTextArea;
    
    /** Scroll Pane for the Text Area **/
    private JScrollPane scrollPane;
    
	 
	
	 /**
     * Default Constructor
     */
    public PlugInDialogDTIParRecSortingProcess() {
    	
    }
    
	/**
	 * Constructor
	 * @param modal
	 */
	public PlugInDialogDTIParRecSortingProcess(boolean modal) {
		super(modal);
		init();
	}
	
	
	/**
	 * init
	 *
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("DTI Par/Rec Sorting Process " + " v1.0");
        
        GridBagLayout mainPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.WEST;
        
        
        JPanel mainPanel = new JPanel(mainPanelGridBagLayout);
        
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        JLabel parRecLabel = new JLabel(" Par/Rec File : ");
        mainPanel.add(parRecLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		parRecFilePathTextField = new JTextField(45);
		mainPanel.add(parRecFilePathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 0;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		parRecFilePathBrowseButton = new JButton("Browse");
		parRecFilePathBrowseButton.addActionListener(this);
		parRecFilePathBrowseButton.setActionCommand("parFilePathBrowse");
		mainPanel.add(parRecFilePathBrowseButton, mainPanelConstraints);
		

		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        JLabel gradientLabel = new JLabel(" Gradient File : ");
        mainPanel.add(gradientLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		gradientFilePathTextField = new JTextField(45);
		mainPanel.add(gradientFilePathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		gradientFilePathBrowseButton = new JButton("Browse");
		gradientFilePathBrowseButton.addActionListener(this);
		gradientFilePathBrowseButton.setActionCommand("gradientFilePathBrowse");
		mainPanel.add(gradientFilePathBrowseButton, mainPanelConstraints);

		
		mainPanelConstraints.anchor = GridBagConstraints.CENTER;
		mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 2;
		mainPanelConstraints.insets = new Insets(10,5,5,5);
		JLabel gradientTipLabel = new JLabel("* gradient file is only necessary for version 4 of Par/Rec");
		mainPanel.add(gradientTipLabel, mainPanelConstraints);
		

		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 3;
		mainPanelConstraints.gridwidth = 3;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		outputTextArea = new JTextArea(15, 60);
		outputTextArea.setEditable(false);
		outputTextArea.setBackground(Color.lightGray);
		outputTextArea.setBorder(new LineBorder(Color.black));
		outputTextArea.setForeground(Color.black);
		scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		mainPanel.add(scrollPane, mainPanelConstraints);
		
		JPanel OKCancelPanel = new JPanel();
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
	
	
	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		Preferences.debug("*** Beginning DTI Par/Rec Sorting Process *** \n",Preferences.DEBUG_ALGORITHM);
		if(outputTextArea != null) {
			outputTextArea.append("*** Beginning DTI Par/Rec Sorting Process *** \n");
		}
		alg = new PlugInAlgorithmDTIParRecSortingProcess(fileName, fileDir, gradientFilePath, outputTextArea);
		alg.addListener(this);
		
		
		if (isRunInSeparateThread()) {

			// Start the thread as a low priority because we wish to still
			// have user interface work fast.
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}

	}

	@Override
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub

	}

	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			alg = null;
			//if OKButton is not null, then the rest are not null
			//we don't want to do these if this algoritm was done via a script
			if(OKButton != null) {
				OKButton.setEnabled(true);
				parRecFilePathBrowseButton.setEnabled(true);
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				outputTextArea.append("*** End DTI Par/Rec Sorting Process *** \n");
					
			}
			Preferences.debug("*** End DTI Par/Rec Sorting Process *** \n",Preferences.DEBUG_ALGORITHM);
		}

	}

	/**
	 * algorithm performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("parFilePathBrowse")) {
			JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }
	        chooser.setDialogTitle("Choose Par/Rec data file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	String absolutePath = chooser.getSelectedFile().getAbsolutePath();
	        	String extension = FileUtility.getExtension(absolutePath);
	        	if(extension.equalsIgnoreCase(".rec") || extension.equalsIgnoreCase(".frec")) {
	        		parRecFilePathTextField.setText(absolutePath);
	        		fileDir = chooser.getSelectedFile().getParent();
	        		fileName = chooser.getSelectedFile().getName();
	        		currDir = chooser.getSelectedFile().getParentFile().getAbsolutePath();
	        	}else {
					MipavUtil.displayError("A proper Par/Rec data file was not chosen...file must be either .rec or .frec");
					return;
	        	}	
	        }
		}if(command.equalsIgnoreCase("gradientFilePathBrowse")) {
			JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }
	        chooser.setDialogTitle("Choose gradient file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	gradientFilePath = chooser.getSelectedFile().getAbsolutePath();
	        	gradientFilePathTextField.setText(gradientFilePath);
	        		
	        }
		}else if(command.equalsIgnoreCase("cancel")) {
			if(alg != null) {
				alg.setThreadStopped(true);
				Preferences.debug("! Algorithm Cancelled \n",Preferences.DEBUG_ALGORITHM);
			}
			dispose();
		}else if(command.equalsIgnoreCase("ok")) {
			OKButton.setEnabled(false);
			parRecFilePathBrowseButton.setEnabled(false);
			callAlgorithm();
		} else {
            super.actionPerformed(e);
        }

	}

}
