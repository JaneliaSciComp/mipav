import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.RandomAccessFile;
import java.util.TreeMap;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogDrosophilaStandardColumnRegistration extends JDialogBase implements AlgorithmInterface {

	/** grid bag constraints **/
	private GridBagConstraints gbc;
	
	/** main panel **/
	private JPanel mainPanel;
	
	/** image to register to standard column **/
	private ModelImage neuronImage;
	
	/** points file **/
	private File pointsFile;
	
	 /** textfields **/
    private JTextField imageFilePathTextField, pointsFilePathTextField, imageXFilePathTextField, imageYFilePathTextField;
    
    /** browse button **/
    private JButton imageBrowseButton, pointsBrowseButton;
    
    /** **/
    private JLabel pointsLabel, imageLabel;
    
    /** **/
    //private JCheckBox minimizeInterpCheckBox;
    
    /** current directory  **/
    private String currDir = null;
    
    /** handle to algorithm **/
    private PlugInAlgorithmDrosophilaStandardColumnRegistration alg;
    
    /** points collection **/
    public TreeMap<Integer, float[]> pointsMap;
    
    
	//private File retinalRegistrationInfoFile;
	
	//private boolean minimizeInterp = false;
	
	/**
	 * constructor
	 */
	public PlugInDialogDrosophilaStandardColumnRegistration() {
		
	}

	/**
	 * constructor
	 * @param modal
	 */
	public PlugInDialogDrosophilaStandardColumnRegistration(boolean modal) {
		super(modal);
		init();
	}
	
	
	public void init() {
		setForeground(Color.black);
        setTitle("Drosophila Standard Column Registration v1.1");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        
        imageLabel = new JLabel("Result Image ");
        imageFilePathTextField = new JTextField(35);
        imageFilePathTextField.setEditable(false);
        imageFilePathTextField.setBackground(Color.white);
        imageBrowseButton = new JButton("Browse");
        imageBrowseButton.addActionListener(this);
        imageBrowseButton.setActionCommand("imageBrowse");
        
        pointsLabel = new JLabel("Points file ");
        pointsFilePathTextField = new JTextField(35);
        pointsFilePathTextField.setEditable(false);
        pointsFilePathTextField.setBackground(Color.white);
        pointsBrowseButton = new JButton("Browse");
        pointsBrowseButton.addActionListener(this);
        pointsBrowseButton.setActionCommand("pointsBrowse");
        
        //minimizeInterpCheckBox = new JCheckBox("Minimize Interpolation");
        //minimizeInterpCheckBox.setSelected(false);
        //minimizeInterpCheckBox.setActionCommand("minimizeInterp");
        //minimizeInterpCheckBox.addActionListener(this);
        
        //retinalRegistrationInfoLabel = new JLabel("Retinal Registration Info File");
        //retinalRegistrationInfoPathTextField = new JTextField(35);
        //retinalRegistrationInfoPathTextField.setEditable(false);
        //retinalRegistrationInfoPathTextField.setBackground(Color.white);
        //retinalRegistrationInfoBrowseButton = new JButton("Browse");
        //retinalRegistrationInfoBrowseButton.addActionListener(this);
        //retinalRegistrationInfoBrowseButton.setActionCommand("retinalRegistrationInfoBrowse");
        

        //retinalRegistrationInfoLabel.setEnabled(false);
        //retinalRegistrationInfoPathTextField.setEnabled(false);
        //retinalRegistrationInfoBrowseButton.setEnabled(false);
        
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15,5,5,15);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(imageLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(imageFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(imageBrowseButton,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(pointsLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(pointsFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(pointsBrowseButton,gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        //gbc.anchor = GridBagConstraints.CENTER;
        //mainPanel.add(minimizeInterpCheckBox,gbc);
        //gbc.anchor = GridBagConstraints.EAST;
        //gbc.gridx = 0;
        //gbc.gridy = 3;
        //mainPanel.add(retinalRegistrationInfoLabel,gbc);
        //gbc.gridx = 1;
        //mainPanel.add(retinalRegistrationInfoPathTextField,gbc);
        //gbc.gridx = 2;
        //mainPanel.add(retinalRegistrationInfoBrowseButton,gbc);
        
        
        
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
        setMinimumSize(getSize());
        setVisible(true);
        
        //hard coding for testing
        //currDir = "C:\\images\\nichd\\points\\neuron1\\resultImage_nlt_1.ics";
        //FileIO fileIO = new FileIO();
        //neuronImage = fileIO.readImage("resultImage_nlt_1.ics", "C:\\images\\nichd\\points\\neuron1" + File.separator, true, null);
        //imageFilePathTextField.setText(currDir);
        
        
	}
	

	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("imageBrowse")) {
			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose image");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	FileIO fileIO = new FileIO();
		        	neuronImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, true, null);
		            imageFilePathTextField.setText(currDir);
		        }
		 }else if(command.equalsIgnoreCase("pointsBrowse")) {
			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose transformation 1");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	pointsFile = new File(currDir);
		        	if(!readPointsFile(pointsFile)) {
		        		MipavUtil.displayError("Error parsing points file");
		        	}
		        	pointsFilePathTextField.setText(currDir);
		        }
		 }else if(command.equalsIgnoreCase("ok")) {
			 if(setVariables()) {
				 callAlgorithm();
			 }else {
				 MipavUtil.displayError("Input image and points file are required");
			 }
		 }/*else if(command.equals("minimizeInterp")) {
			 if(minimizeInterpCheckBox.isSelected()) {
				 minimizeInterp = true;
				 retinalRegistrationInfoLabel.setEnabled(true);
			     retinalRegistrationInfoPathTextField.setEnabled(true);
			     retinalRegistrationInfoBrowseButton.setEnabled(true);
			 }else {
				 minimizeInterp = false;
				 retinalRegistrationInfoLabel.setEnabled(false);
			     retinalRegistrationInfoPathTextField.setEnabled(false);
			     retinalRegistrationInfoPathTextField.setText("");
			     retinalRegistrationInfoBrowseButton.setEnabled(false);
			 }
		 }*/else if(command.equals("retinalRegistrationInfoBrowse")) {
			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose file");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	//retinalRegistrationInfoFile = new File(currDir);
		        	//retinalRegistrationInfoPathTextField.setText(currDir);
		        }
		 }

	}
	
	
	/**
	 * set variables
	 * @return
	 */
	private boolean setVariables() {
		if(pointsFilePathTextField.getText().trim().equals("")) {
			return false;
			
		}
		if(imageFilePathTextField.getText().trim().equals("")) {
			return false;
			
		}
		
		return true;
	}
	
	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		alg = new PlugInAlgorithmDrosophilaStandardColumnRegistration(neuronImage,pointsMap);
		alg.addListener(this);
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		
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
	
	
	/**
	 * read points file
	 * @param pointsFile
	 * @return
	 */
	private boolean readPointsFile(File pointsFile) {
		try {
			pointsMap = new TreeMap<Integer, float[]>();
            RandomAccessFile raFile = new RandomAccessFile(pointsFile, "r");
            String[] arr;
            String[] arr2;
            String line;
            float x,y,z;
            int counter = 1;
            while((line = raFile.readLine()) != null) {
            	if(line.trim().equals("top") || line.trim().equals("r8") || line.trim().equals("r7")) {
            	}else {
            		arr = line.trim().split(":");
            		if(arr.length == 2 && arr[0].trim().equals(String.valueOf(counter))) {
                    	if(arr[1].trim().equals("")) {
                    		pointsMap.put(new Integer(counter), null);
                    	}else {
                    		arr2 = arr[1].trim().split(",");
                        	if(arr2.length != 3) {
                        		return false;
                        	}
                        	x = new Float(arr2[0]).floatValue();
                        	y = new Float(arr2[1]).floatValue();
                        	z = new Float(arr2[2]).floatValue();
                        	float[] f = {x,y,z};
                        	pointsMap.put(new Integer(counter), f);
                    	}
                    }else {
                    	if(arr.length == 1) {
                    		int length = line.trim().length();
                    		String numString;
                    		if(line.trim().charAt(length-1) == ':') {
                    			numString = line.trim().substring(0, length-1);
                    			if(numString.equals(String.valueOf(counter))) {
                    				pointsMap.put(new Integer(counter), null);
                    			}else {
                    				return false;
                    			}
                    		}else {
                    			return false;
                    		}	
                    	}else {
                    		return false;
                    	}
                    }
            		counter++;
            	}
            }
            if(pointsMap.size() != 27) {
            	return false;
            }
            return true;
		}catch(Exception e) {
			e.printStackTrace();
			return false;
		}
	}
	
	
	
	
	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		System.out.println("Finished");
		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		dispose();
	}
	
	
	


}
