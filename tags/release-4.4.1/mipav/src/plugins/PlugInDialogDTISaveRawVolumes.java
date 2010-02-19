import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

/**
 * 
 * @author pandyan
 * 
 * This is the main dialog for the DTI Save raw Volumes Plug-In
 * 
 * References: This algorithm was developed in concert with Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 *
 */
public class PlugInDialogDTISaveRawVolumes extends JDialogScriptableBase implements AlgorithmInterface {
	
	
    /** panels **/
    private JPanel mainPanel, listPanel, optionsPanel, createVolsPanel, saveVolsPanel;
    
    /** path to study dir **/
    private JTextField listFileTextField;
    
    /** browse button **/
    private JButton listFileBrowseButton;
    
    /** abs path to list file **/
    private String absPath;
    
    /** version **/
    private String version = "1.0";
    
    /** titled border **/
    private TitledBorder titledBorder;
    
    /** radio groups **/
    private ButtonGroup createVolsRadioGroup, saveVolsRadioGroup;
    
    /** radio buttons **/
    private JRadioButton xyzRadio, xyrRadio, nrrdRadio, niftiRadio;
    
    /** handle to algorithm **/
    private PlugInAlgorithmDTISaveRawVolumes alg;
    
    /** boolean for if is xyz **/
    private boolean isXYZ = true;
    
    /** boolean for if is nifti **/
    private boolean isNIFTI = true;
    
    
    
     

	/**
	 * empty constructor needed for scripting
	 */
	public PlugInDialogDTISaveRawVolumes() {
		
	}
	
	/**
	 * Constructor
	 * @param modal
	 */
	public PlugInDialogDTISaveRawVolumes(boolean modal) {
		super(modal);
		init();
	}
	
	
	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
		setTitle("DTI Save Raw Volumes " + version);
		
		GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        //gbc.anchor = GridBagConstraints.NONE;
		
		listPanel = new JPanel(gbl);
        
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,15,0);
        JLabel listFileLabel = new JLabel(" list file : ");
        listPanel.add(listFileLabel, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.insets = new Insets(15,5,15,0);
		listFileTextField = new JTextField(35);
		listPanel.add(listFileTextField, gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,15,5);
		listFileBrowseButton = new JButton("Browse");
		listFileBrowseButton.addActionListener(this);
		listFileBrowseButton.setActionCommand("listFileBrowse");
		listPanel.add(listFileBrowseButton, gbc);
		
		createVolsPanel = new JPanel(gbl);
		createVolsPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), "Create volumes as ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		createVolsPanel.setBorder(titledBorder);
		createVolsRadioGroup = new ButtonGroup();
		xyzRadio = new JRadioButton("x,y,z");
		xyzRadio.setSelected(true);
		createVolsRadioGroup.add(xyzRadio);
		//gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(0,30,0,30);
		gbc.gridx = 0;
		gbc.gridy = 0;
		createVolsPanel.add(xyzRadio, gbc);
		xyrRadio = new JRadioButton("x,y,r");
		xyrRadio.setSelected(false);
		createVolsRadioGroup.add(xyrRadio);
		gbc.gridx = 0;
		gbc.gridy = 1;
		createVolsPanel.add(xyrRadio, gbc);
		
		saveVolsPanel = new JPanel(gbl);
		saveVolsPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), "Save volumes as ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		saveVolsPanel.setBorder(titledBorder);
		saveVolsRadioGroup = new ButtonGroup();
		niftiRadio = new JRadioButton("NIFTI");
		niftiRadio.setSelected(true);
		saveVolsRadioGroup.add(niftiRadio);
		//gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(0,30,0,30);
		gbc.gridx = 0;
		gbc.gridy = 0;
		saveVolsPanel.add(niftiRadio, gbc);
		nrrdRadio = new JRadioButton("NRRD");
		nrrdRadio.setSelected(false);
		nrrdRadio.setEnabled(false);
		saveVolsRadioGroup.add(nrrdRadio);
		gbc.gridx = 0;
		gbc.gridy = 1;
		saveVolsPanel.add(nrrdRadio, gbc);
		
		optionsPanel = new JPanel(gbl);
		gbc.insets = new Insets(0,0,0,0);
		gbc.gridx = 0;
		gbc.gridy = 0;
		optionsPanel.add(createVolsPanel,gbc);
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		optionsPanel.add(saveVolsPanel,gbc);
		
		JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        
        mainPanel = new JPanel(gbl);
        gbc.insets = new Insets(0,0,0,0);
        
        gbc.gridx = 0;
		gbc.gridy = 0;
		mainPanel.add(listPanel,gbc);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 1;
		mainPanel.add(optionsPanel,gbc);
		
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        
        
        
        pack();
        setResizable(false);
        setVisible(true);
		
	}
	
	
	/**
	 * action performed
	 * @param event
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		if(command.equalsIgnoreCase("listFileBrowse")) {
			JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
	        chooser.setDialogTitle("Choose list file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	if(!(chooser.getSelectedFile().getName().endsWith(".list"))) {
	        		MipavUtil.displayError("File selected must have a .list extension");
	        		listFileTextField.setText("");
	        	}else {
	        		absPath = chooser.getSelectedFile().getAbsolutePath();
	        		listFileTextField.setText(absPath);
	        	}
	        }
		}else if(command.equalsIgnoreCase("cancel")) {
			dispose();
		}else if(command.equalsIgnoreCase("ok")) {
			if(listFileTextField.getText().trim().equals("")) {
				MipavUtil.displayError("You must select a .list file");
				return;
			}
			if(xyzRadio.isSelected()) {
				isXYZ = true;
			}else {
				isXYZ = false;
			}
			if(niftiRadio.isSelected()) {
				isNIFTI = true;
			}else {
				isNIFTI = false;
			}
			callAlgorithm();
		}

	}
	
	
	
	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		alg = new PlugInAlgorithmDTISaveRawVolumes(absPath, isXYZ, isNIFTI);
		
		alg.addListener(this);
		
		createProgressBar("Save Raw Volumes", alg);
		
		if (isRunInSeparateThread()) {

			// Start the thread as a low priority because we wish to still have user interface work fast.
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
	}

	/**
	 * algorithm performed
	 * @param algorithm
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			alg = null;
			dispose();
		}

	}


	/**
	 * 
	 */
	protected void setGUIFromParams() {
		setAbsPath(scriptParameters.getParams().getString("abs_path"));
		setXYZ(scriptParameters.getParams().getBoolean("is_XYZ"));
		setNIFTI(scriptParameters.getParams().getBoolean("is_NIFTI"));

	}

	/**
	 * 
	 */
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.getParams().put(ParameterFactory.newParameter("abs_path", absPath));
		scriptParameters.getParams().put(ParameterFactory.newParameter("is_XYZ", isXYZ));
		scriptParameters.getParams().put(ParameterFactory.newParameter("is_NIFTI", isNIFTI));
	}
	
	
	/**
	 * set list file abs path
	 * @param absPath
	 */
	public void setAbsPath(String absPath) {
		this.absPath = absPath;
	}
	
	/**
	 * set isXYZ
	 * @param isXYZ
	 */
	public void setXYZ(boolean isXYZ) {
		this.isXYZ = isXYZ;
	}

	/**
	 * set isNIFTI
	 * @param isNIFTI
	 */
	public void setNIFTI(boolean isNIFTI) {
		this.isNIFTI = isNIFTI;
	}

	

}
