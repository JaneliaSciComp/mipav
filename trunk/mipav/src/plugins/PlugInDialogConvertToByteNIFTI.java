import java.awt.BorderLayout;
import java.awt.Color;
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
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.dialogs.JDialogScriptableBase;


public class PlugInDialogConvertToByteNIFTI extends JDialogScriptableBase implements AlgorithmInterface {
	
	 /** path to study dir **/
    private JTextField studyPathTextField;
    
    /** browse button **/
    private JButton studyPathBrowseButton;
    
    /** current directory  **/
    private String currDir = null;
    
    private AlgorithmChangeType changeTypeAlgo;
    
    private File studyFile;
	
	 /**
     * Default Constructor
     */
    public PlugInDialogConvertToByteNIFTI() {
    	
    }
    
	/**
	 * Constructor
	 * @param modal
	 */
	public PlugInDialogConvertToByteNIFTI(boolean modal) {
		super(modal);
		init();
	}
	
	private void init() {
		setForeground(Color.black);
        setTitle("Convert to Byte NIFTI ");
        
        GridBagLayout mainPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints mainPanelConstraints = new GridBagConstraints();
        mainPanelConstraints.anchor = GridBagConstraints.WEST;
        
        
        JPanel mainPanel = new JPanel(mainPanelGridBagLayout);
        
        mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
        JLabel studyPathLabel = new JLabel(" study path directory : ");
        mainPanel.add(studyPathLabel, mainPanelConstraints);
        
        mainPanelConstraints.gridx = 1;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,0);
		studyPathTextField = new JTextField(55);
		mainPanel.add(studyPathTextField, mainPanelConstraints);
		
		mainPanelConstraints.gridx = 2;
		mainPanelConstraints.gridy = 1;
		mainPanelConstraints.insets = new Insets(15,5,15,5);
		studyPathBrowseButton = new JButton("Browse");
		studyPathBrowseButton.addActionListener(this);
		studyPathBrowseButton.setActionCommand("studyPathBrowse");
		mainPanel.add(studyPathBrowseButton, mainPanelConstraints);
		
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
	
	

	@Override
	protected void callAlgorithm() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub

	}

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}

	@Override
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("studyPathBrowse")) {
			JFileChooser chooser = new JFileChooser();
			if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }
			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        chooser.setDialogTitle("Choose study path directory");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	studyPathTextField.setText(chooser.getSelectedFile().getAbsolutePath());
	        	currDir = chooser.getSelectedFile().getAbsolutePath();
	        	studyFile = new File(chooser.getSelectedFile().getAbsolutePath());
	        }
		}else if(command.equals("ok")){
			parse(studyFile);
			dispose();
		}

	}
	
	
	 public void parse(File file) {
		 File[] children = file.listFiles();
		 FileIO fileIO = new FileIO();
		 
		 
		 for (int i = 0; i < children.length; i++) {
			 
			 if (children[i].isDirectory()) {
                 parse(children[i]);
             } else if (!children[i].isDirectory() && children[i].getName().endsWith(".nii") && (!children[i].getName().contains("byte"))) {
            	 ModelImage img = fileIO.readImage(children[i].getName(), children[i].getParent() + File.separator, false, null);
            	 double inTempMin = (float) img.getMin();
                 double inTempMax = (float) img.getMax();
                 double outTempMin = 0;
                 double outTempMax = 255;
                 
                 //ModelImage resultImage = new ModelImage(ModelStorageBase.UBYTE, img.getExtents(), makeImageName(img.getImageName(), "_byte"));
                 
            	 changeTypeAlgo = new AlgorithmChangeType(img, ModelStorageBase.UBYTE, inTempMin, inTempMax, outTempMin, outTempMax, false);
            	 
            	 changeTypeAlgo.run();
            	 
            	/* resultImage.setFileInfo(img.getFileInfo());
                 for(int k=0;k<resultImage.getExtents()[2];k++) {
                	 resultImage.getFileInfo(k).setDataType(ModelStorageBase.UBYTE);
                	 resultImage.getFileInfo(k).setImageOrientation(img.getImageOrientation());	
                	 resultImage.getFileInfo(k).setAxisOrientation(img.getAxisOrientation());
                
                 }*/
            	 
            	 FileWriteOptions opts = new FileWriteOptions(true);
                 opts.setFileType(FileUtility.NIFTI);
                 opts.setFileDirectory(children[i].getParent() + File.separator);
                 opts.setFileName(img.getImageName() + "_byte.nii");
                 opts.setBeginSlice(0);
                 opts.setEndSlice(img.getExtents()[2]-1);
                 opts.setOptionsSet(true);
                 
                 fileIO.writeImage(img, opts, false);
            	 
                 
                 img.disposeLocal();
                 img = null;
                 
                 /*resultImage.disposeLocal();
                 resultImage = null;*/
            	
            	 
             }
		 }
		 
		 
	 }

}
