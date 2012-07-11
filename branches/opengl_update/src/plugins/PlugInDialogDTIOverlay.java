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
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

/**
 * @author pandyan
 * 
 * 
 * This is the main dialog for the DTI Overlay Plugin
 * 
 * References: Developed in concert with Bennett Landman from Johns Hopkins University
 *
 */
public class PlugInDialogDTIOverlay extends JDialogScriptableBase implements AlgorithmInterface {
	
	/** src image **/
	private ModelImage frecImage;
    
	/** GridBagLayout **/
	private GridBagLayout gbl;
    
	/** GridBagConstraints **/
	private GridBagConstraints gbc;
	
	/** panels **/
	private JPanel mainPanel, OKCancelPanel;
	
	/** label **/
	private JLabel fiberPathLabel;
	
	/** text field **/
	private JTextField fiberPathTextField;
	
	/** browse button **/
	private JButton fiberPathBrowseButton;
    
    /** handle to algorithm **/
    private PlugInAlgorithmDTIOverlay alg;
    
    /** filepath **/
    private String filepath;
    
    /** result image b **/
    private ModelImage imageB;
    
    /** lut b **/
    private ModelLUT lutb;
    
    /** Frame of src image **/
    private ViewJFrameImage srcFrame;
    
    
    
    

	
	/**
	 * constructor
	 * @param frame
	 * @param image
	 */
	public PlugInDialogDTIOverlay(ViewJFrameImage frame, ModelImage image) {
		this.srcFrame = frame;
		this.frecImage = image;
		init();
	}
	
	
	
	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("DTI Overlay");
        
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        
        mainPanel = new JPanel(gbl);
        
        fiberPathLabel = new JLabel(" Fiber file : ");
        fiberPathTextField = new JTextField(35);
        fiberPathTextField.setEditable(false);
        fiberPathTextField.setBackground(Color.white);
        fiberPathBrowseButton = new JButton("Browse");
        fiberPathBrowseButton.addActionListener(this);
        fiberPathBrowseButton.setActionCommand("fiberPathBrowse");
        gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,5,0);
		mainPanel.add(fiberPathLabel, gbc);
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,5,0);
		mainPanel.add(fiberPathTextField, gbc);
		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,5,0);
		mainPanel.add(fiberPathBrowseButton, gbc);

    
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
	
	
	
	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			
			imageB = alg.getImageB();
			lutb = alg.getLutb();
			
			if(imageB != null) {
				
				srcFrame.setImageB(imageB);
				srcFrame.setLUTb(lutb);
				srcFrame.setControls();
				
				srcFrame.updateImages(true);
				
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				
			}
			
			
			dispose();
		}else {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}

	}


	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		alg = new PlugInAlgorithmDTIOverlay(frecImage,filepath);
		
		alg.addListener(this);
		
		alg.run();
		
	}


	/**
	 * set GUI from params
	 */
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub
		
	}


	/**
	 * store paramss from GUI
	 */
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub
		
	}


	/**
	 * action performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("fiberPathBrowse")) {
			JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
	        chooser.setDialogTitle("Select Fiber file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	fiberPathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	filepath = chooser.getSelectedFile().getAbsolutePath();
	        	Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
	        }
		}
		else if(command.equalsIgnoreCase("ok")) {
			if(srcFrame.getImageB() == null) {
				if(filepath == null || filepath.equals("")) {
					MipavUtil.displayError("Fiber file must be specified");
					return;
				}
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				callAlgorithm();
			}
			else {
				MipavUtil.displayError("Source image already has an imageB...please close imageB in order to run plugin");
				return;
			}
		}
		else if(command.equalsIgnoreCase("cancel")) {
			if(alg != null) {
				alg.setThreadStopped(true);
			}
			dispose();
		}
	}

	
	
	
}
