import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.TreeMap;


import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.LineBorder;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ScrollCorrector;

import gov.nih.mipav.view.dialogs.JDialogBase;


/**
 * This plugin was done for Dr Chi-hon Lee of NICHD
 * 
 * This plugin basically standardizes the output from the DrosophilaRetinalRegistration to a standard model
 * 
 * It also standardizes an input surface file
 * 
 * It also allows for inverting the output surface file
 * 
 * @author pandyan
 */
public class PlugInDialogDrosophilaStandardColumnRegistration extends JDialogBase implements AlgorithmInterface {

	/** grid bag constraints **/
	private GridBagConstraints gbc;
	
	/** main panel **/
	private JPanel mainPanel;
	
	/** image to register to standard column **/
	public ModelImage neuronImage,cityBlockImage;
	
	/** points file **/
	public File pointsFile, surfaceFile;
	
	 /** textfields **/
    public JTextField imageFilePathTextField, pointsFilePathTextField, surfaceFilePathTextField;
    
    /** browse button **/
    public JButton imageBrowseButton, pointsBrowseButton, surfaceBrowseButton;
    
    /** labels **/
    private JLabel pointsLabel, imageLabel, surfaceLabel, surfaceSamplingLabel, invertIVFileCBLabel;

    /** current directory  **/
    private String currDir = null;
    
    /** handle to algorithm **/
    public PlugInAlgorithmDrosophilaStandardColumnRegistration alg;
    
    /** points collection **/
    public TreeMap<Integer, float[]> pointsMap;
    
    /** coords of filament **/
    public ArrayList <ArrayList<float[]>> allFilamentCoords = new ArrayList <ArrayList<float[]>>();

    /** resolutions **/
    public float[] resols;
    
    /** surface file sampling checkbox **/
    public JComboBox surfaceFileSamplingCB;
    
    /** flip checkboxes **/
    public JCheckBox flipXCB, flipYCB, flipZCB;
    
    /** rigidOnly registration **/
    //private JCheckBox rigidOnlyCB;
    
    /** checkbox for swc output **/
    public JCheckBox swcCB;
    
    /** boolean for doing swc **/
    public boolean doSWC = false;
    
    /** flip panel **/
    private JPanel flipPanel;

    /** output text area **/
    private JTextArea outputTextArea;
    
    /** scroll pane **/
    private JScrollPane scrollPane;
    
    /** button group * */
    private ButtonGroup eyeGroup, regGroup, pointsGroup;
    
    /** radio buttons * */
    public JRadioButton lvrdRadio, rvldRadio, rigTPSRadio, rigRadio;
    
    public JRadioButton _27PointsRadio, _75PointsRadio, _147PointsRadio;
    
    
    

    
    //SWC params

	 /** textfields **/
   public JTextField  greenValueRadiusThresholdTextField, subsamplingDistanceTextField, outputFilenameTextField, outputFilenameTextField_auto, outputFilenameTextField_regionGrow;
   
   /** labels **/
   private JLabel  greenValueRadiusThresholdLabel, subsamplingDistanceLabel, outputFilenameLabel, outputFilenameLabel_auto, outputFilenameLabel_regionGrow;
   
   /** green value radius threshold **/
   public float greenThreshold;
   
	/** subsampling distance **/
   public float subsamplingDistance;
   
   /** output filename **/
   private String outputFilename, outputFilename_auto, outputFilename_regionGrow;
   
   private boolean plugInCompleted = false;
   
   public static final int _27POINTS = 27;
   public static final int _75POINTS = 75;
   public static final int _147POINTS = 147;
   
   private int numPoints;
   

	

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
	
	public  void setNumPoints(int numPoints) {
		this.numPoints = numPoints;
	}
	
	
	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("Drosophila Standard Column Registration v5.4");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        
        imageLabel = new JLabel("Image ");
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
        pointsBrowseButton.setEnabled(false);
        pointsBrowseButton.addActionListener(this);
        pointsBrowseButton.setActionCommand("pointsBrowse");
        
        surfaceLabel = new JLabel("Filament file ");
        surfaceFilePathTextField = new JTextField(35);
        surfaceFilePathTextField.setEditable(false);
        surfaceFilePathTextField.setBackground(Color.white);
        surfaceBrowseButton = new JButton("Browse");
        surfaceBrowseButton.setEnabled(false);
        surfaceBrowseButton.addActionListener(this);
        surfaceBrowseButton.setActionCommand("surfaceBrowse");
        
        surfaceSamplingLabel = new JLabel("Filament sampling ");
        surfaceFileSamplingCB = new JComboBox();
        surfaceFileSamplingCB.addItem("1.0");
        surfaceFileSamplingCB.addItem("0.5");
        surfaceFileSamplingCB.addItem("0.25");
        surfaceFileSamplingCB.addItem("0.125");
        surfaceFileSamplingCB.setSelectedIndex(1);
        
        invertIVFileCBLabel = new JLabel("Invert IV File");
        flipXCB = new JCheckBox("Flip X");
        flipYCB = new JCheckBox("Flip Y");
        flipZCB = new JCheckBox("Flip Z");
        
        flipPanel = new JPanel();
        flipPanel.add(flipXCB);
        flipPanel.add(flipYCB);
        flipPanel.add(flipZCB);
        
        
        
        pointsGroup = new ButtonGroup();
        _27PointsRadio = new JRadioButton("27 Points (3X3X3)");
        _75PointsRadio = new JRadioButton("75 Points (5X5X3)");
        _75PointsRadio.setSelected(true);
        _147PointsRadio = new JRadioButton("147 Points (7X7X3)");

        pointsGroup.add(_27PointsRadio);
        pointsGroup.add(_75PointsRadio);
        pointsGroup.add(_147PointsRadio);
        JPanel pointsPanel = new JPanel();
        pointsPanel.add(_27PointsRadio);
        pointsPanel.add(_75PointsRadio);
        pointsPanel.add(_147PointsRadio);
        
        
        eyeGroup = new ButtonGroup();
        lvrdRadio = new JRadioButton("LV/RD");
        lvrdRadio.setSelected(true);
        rvldRadio = new JRadioButton("RV/LD");
        eyeGroup.add(lvrdRadio);
        eyeGroup.add(rvldRadio);
        JPanel eyePanel = new JPanel();
        eyePanel.add(lvrdRadio);
        eyePanel.add(rvldRadio);
        
        //rigidOnlyCB = new JCheckBox("Rigid Body Registration Only");
        
        
        

        regGroup = new ButtonGroup();
        rigTPSRadio = new JRadioButton("Rigid and TPS Registration");
        rigTPSRadio.setSelected(true);
        rigRadio = new JRadioButton("Rigid Registration Only");
        rigRadio.setSelected(false);
        regGroup.add(rigTPSRadio);
        regGroup.add(rigRadio);
        
        JPanel regPanel = new JPanel();
        regPanel.add(rigTPSRadio);
        regPanel.add(rigRadio);
        
        
        
        swcCB = new JCheckBox("Create SWC file");
        swcCB.addActionListener(this);
        swcCB.setActionCommand("swcCB");
        
        greenValueRadiusThresholdLabel = new JLabel("SWC-Green vaue radius threshold ");
        greenValueRadiusThresholdTextField = new JTextField(35);
        greenValueRadiusThresholdTextField.setEnabled(false);
        greenValueRadiusThresholdTextField.setText("0.0");
        
        subsamplingDistanceLabel = new JLabel("SWC-Subsampling distance (um) ");
        subsamplingDistanceTextField = new JTextField(35);
        subsamplingDistanceTextField.setEnabled(false);

        
        outputFilenameLabel = new JLabel("SWC-threshold output filename ");
        outputFilenameTextField = new JTextField(35);
        outputFilenameTextField.setEnabled(false);
        outputFilenameTextField.setText("test.swc");
        
        outputFilenameLabel_auto = new JLabel("SWC-automatic output filename ");
        outputFilenameTextField_auto = new JTextField(35);
        outputFilenameTextField_auto.setEnabled(false);
        outputFilenameTextField_auto.setText("test_auto.swc");
        
        outputFilenameLabel_regionGrow = new JLabel("SWC-region grow output filename ");
        outputFilenameTextField_regionGrow = new JTextField(35);
        outputFilenameTextField_regionGrow.setEnabled(false);
        outputFilenameTextField_regionGrow.setText("test_regionGrow.swc");
        
        
        
        
        outputTextArea = new JTextArea();
        outputTextArea.setRows(15);
		outputTextArea.setEditable(false);
		outputTextArea.setBackground(Color.lightGray);
		outputTextArea.setBorder(new LineBorder(Color.black));
		outputTextArea.setForeground(Color.black);
		scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
		scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());

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
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(surfaceLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(surfaceFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(surfaceBrowseButton,gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(surfaceSamplingLabel,gbc);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(surfaceFileSamplingCB,gbc);
        
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(invertIVFileCBLabel,gbc);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(flipPanel,gbc);
        
        
        gbc.gridy = 5;
        gbc.gridx = 1;
        mainPanel.add(pointsPanel,gbc);
        
        
        
        gbc.gridy = 6;
        gbc.gridx = 1;
        mainPanel.add(eyePanel,gbc);
        
        gbc.gridy = 7;
        gbc.gridx = 1;
        mainPanel.add(regPanel,gbc);
        
        gbc.gridy = 8;
        gbc.gridx = 1;
        mainPanel.add(swcCB,gbc);
        
        
        
        gbc.gridy = 9;
        gbc.gridx = 0;
        mainPanel.add(greenValueRadiusThresholdLabel,gbc);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(greenValueRadiusThresholdTextField,gbc);
        gbc.anchor = GridBagConstraints.EAST;
        
        
        
        gbc.gridy = 10;
        gbc.gridx = 0;
        mainPanel.add(subsamplingDistanceLabel,gbc);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(subsamplingDistanceTextField,gbc);
        gbc.anchor = GridBagConstraints.EAST;
        
        
        /*gbc.gridy = 10;
        gbc.gridx = 0;
        mainPanel.add(outputFilenameLabel,gbc);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(outputFilenameTextField,gbc);
        gbc.anchor = GridBagConstraints.EAST;*/
        
       /* gbc.gridy = 11;
        gbc.gridx = 0;
        mainPanel.add(outputFilenameLabel_auto,gbc);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(outputFilenameTextField_auto,gbc);
        gbc.anchor = GridBagConstraints.EAST;
        
        gbc.gridy = 12;
        gbc.gridx = 0;
        mainPanel.add(outputFilenameLabel_regionGrow,gbc);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(outputFilenameTextField_regionGrow,gbc);
        gbc.anchor = GridBagConstraints.EAST;*/
        
        
        
        
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;
        
        
        
        
        
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 3;
        mainPanel.add(scrollPane,gbc);

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
        setResizable(false);

	}
	
	/**
	 * action performed
	 */
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
		        	resols = neuronImage.getResolutions(0);
		        	subsamplingDistanceTextField.setText(String.valueOf(resols[0]));
		        	imageFilePathTextField.setText(currDir);
		        	surfaceBrowseButton.setEnabled(true);
		        	pointsBrowseButton.setEnabled(true);
		        }
		 }else if(command.equalsIgnoreCase("pointsBrowse")) {
			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose points file");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	pointsFile = new File(currDir);
		        	if(!readPointsFile(pointsFile)) {
		        		MipavUtil.displayError("Error parsing points file");
		        	}else {
		        		pointsFilePathTextField.setText(currDir);
		        	}
		        	
		        }
		 }else if(command.equalsIgnoreCase("ok")) {
			 if(setVariables()) {
				 callAlgorithm();
			 }
		 }else if(command.equalsIgnoreCase("surfaceBrowse")) {

			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose filament file");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	surfaceFile = new File(currDir);
		        	if(!readSurfaceFile(surfaceFile)) {
		        		
		        		MipavUtil.displayError("Error parsing surface file");
		        		allFilamentCoords = new ArrayList <ArrayList<float[]>>();
		        		
		        	}else {

		        		boolean success = determineIfProperlyConnected();
		        		if(success == false) {
		        			allFilamentCoords = new ArrayList <ArrayList<float[]>>();
		        			
		        		}else {
		        			createCityBlockImage();
		        			surfaceFilePathTextField.setText(currDir);
		        		}
		        	}
		        	
		        }
		 }else if(command.equalsIgnoreCase("swcCB")) {
			 if(swcCB.isSelected()) {
				 greenValueRadiusThresholdTextField.setEnabled(true);
				 subsamplingDistanceTextField.setEnabled(true);
				 outputFilenameTextField.setEnabled(true);
				 outputFilenameTextField_auto.setEnabled(true);
				 outputFilenameTextField_regionGrow.setEnabled(true);
				 doSWC = true;
			 }else {
				 greenValueRadiusThresholdTextField.setEnabled(false);
				 subsamplingDistanceTextField.setEnabled(false);
				 outputFilenameTextField.setEnabled(false);
				 outputFilenameTextField_auto.setEnabled(false);
				 outputFilenameTextField_regionGrow.setEnabled(false);
				 doSWC = false;
			 }
		 }
		 else if(command.equalsIgnoreCase("cancel")) {
			 if(neuronImage != null) {
				 neuronImage.disposeLocal();
				 neuronImage = null;
			 }
			 dispose();
		 }
	}
	
	
	
	/**
	 * reads surface file
	 * @param surfaceFile
	 * @return
	 */
	public boolean readSurfaceFile(File surfaceFile) {
		boolean success = true;
		RandomAccessFile raFile = null;
		try {

			raFile = new RandomAccessFile(surfaceFile, "r");
			
			String line;
			
			
			while((line=raFile.readLine())!= null) {
				line = line.trim();
				if(line.startsWith("Translate1Dragger")) {
					break;
				}
				if(line.contains("Coordinate3")) {
					ArrayList<float[]> filamentCoords = new ArrayList<float[]>();
					while(!((line=raFile.readLine()).endsWith("}"))) {
						line = line.trim();
						if(!line.equals("")) {
							if(line.startsWith("point [")) {
								line = line.substring(line.indexOf("point [") + 7, line.length()).trim();
								if(line.equals("")) {
									continue;
								}
							}
							if(line.endsWith("]")) {
								line = line.substring(0, line.indexOf("]")).trim();
								if(line.equals("")) {
									continue;
								}
							}
							if(line.endsWith(",")) {
								line = line.substring(0, line.indexOf(",")).trim();
								if(line.equals("")) {
									continue;
								}
							}
							String[] splits = line.split("\\s+");
							splits[0] = splits[0].trim();
							splits[1] = splits[1].trim();
							splits[2] = splits[2].trim();
							float coord_x = new Float(splits[0]).floatValue();
							float coord_y = new Float(splits[1]).floatValue();
							float coord_z = new Float(splits[2]).floatValue();
							
							//we will get the coords in proper mipav space in the method, determineIfProperlyConnected
							/*float x = coord_x/resols[0];
							float y = coord_y/resols[1];
							float z = coord_z/resols[2];*/
							  
							float[] coords = {coord_x,coord_y,coord_z,0};
							
							filamentCoords.add(coords);
						}
					}
					allFilamentCoords.add(filamentCoords);
				}
				
				
				
				
				
			}
			raFile.close();
			
		}catch(Exception e) {
			try {
				if(raFile != null) {
					raFile.close();
				}
			}catch(Exception ex) {
				
			}
			e.printStackTrace();
			return false;
		}
		
		return success;
	}
	
	
	
	public boolean determineIfProperlyConnected() {
		boolean success = true;
		int[] connectednessList = new int[allFilamentCoords.size()];
		//initialize all to 0
		for(int i=0;i<connectednessList.length;i++) {
			connectednessList[i] = 0;
		}
		
		
		
		
		//set first one to -1
		connectednessList[0] = -1;
		int alMatchSize;
		ArrayList<float[]> al;
		ArrayList<float[]> alMatch;
		float[] coordsMatch = new float[3];
		float[] coords;
		
		for(int i=1;i<allFilamentCoords.size();i++) {
			
			
			 al = allFilamentCoords.get(i);
			 coords = al.get(0);
			
			int k;
			 
			 for(k=0;k<i;k++) {
				 alMatch = allFilamentCoords.get(k);
				 alMatchSize = alMatch.size();
				 
				 coordsMatch[0] = alMatch.get(alMatchSize-1)[0];
				 coordsMatch[1] = alMatch.get(alMatchSize-1)[1];
				 coordsMatch[2] = alMatch.get(alMatchSize-1)[2];
				 
				 if(coords[0]==coordsMatch[0] && coords[1]==coordsMatch[1] && coords[2]==coordsMatch[2]) {

					 connectednessList[i] = k+1;


					 break;
				 }
				 
				 
				 
			 }
			
			
		}
		
		
		//if any in connectednessList is 0, then this is not a properly connected iv file
		int index = 0;
		for(int i=0;i<connectednessList.length;i++) {
			if(connectednessList[i] == 0) {
				success = false;
				index = i;
				break;
			}
		}
		
		if(success == false) {
			al = allFilamentCoords.get(index);
			coords = al.get(0);
			String coordsString = coords[0] + "," + coords[1] + "," + coords[2];
			MipavUtil.displayError("IV file is not properly connected: the block of points starting with " + coordsString + " is not connected to anything");
			System.out.println("IV file is not properly connected: the block of points starting with " + coordsString + " is not connected to anything");
		}else {
			//get the coords in proper mipav space!!!
			for(int i=0;i<allFilamentCoords.size();i++) {
				 al = allFilamentCoords.get(i);
				 for(int k=0;k<al.size();k++) {
					 float[] coords2 = al.get(k);
					 
					 
					 float x = coords2[0]/resols[0];
					 float y = coords2[1]/resols[1];
					 float z = coords2[2]/resols[2];
					 
					 coords2[0] = x;
					 coords2[1] = y;
					 coords2[2] = z;
					 
					 al.set(k, coords2);
					 
					 
					 
					 
				 }
				 
				 
				 
			}
			
		}
		
		
		
		return success;
	}
	
	
	/**
	 * set variables
	 * @return
	 */
	private boolean setVariables() {
		if(pointsFilePathTextField.getText().trim().equals("")) {
			MipavUtil.displayError("Points file is required");
			return false;
			
		}
		if(imageFilePathTextField.getText().trim().equals("")) {
			MipavUtil.displayError("Image is required");
			return false;
			
		}
		if(surfaceFilePathTextField.getText().trim().equals("")) {
			MipavUtil.displayError("Filament file is required");
			return false;
			
		}
		
		if(_27PointsRadio.isSelected()) {
			numPoints = _27POINTS;
		}else if (_75PointsRadio.isSelected()) {
			numPoints = _75POINTS;
		}else if (_147PointsRadio.isSelected()) {
			numPoints = _147POINTS;
		}
		
		
		if(doSWC) {
		
		
			if(!subsamplingDistanceTextField.getText().trim().equals("")) {
				try {
					subsamplingDistance = Float.parseFloat(subsamplingDistanceTextField.getText().trim());
				}catch(Exception e) {
					MipavUtil.displayError("Subsampling distance is not a valid number");
					return false;
				}
			}else {
				MipavUtil.displayError("Subsampling distance is required");
				return false;
			}
		
		
			if(!greenValueRadiusThresholdTextField.getText().trim().equals("")) {
				try {
					greenThreshold = Float.parseFloat(greenValueRadiusThresholdTextField.getText().trim());
				}catch(Exception e) {
					MipavUtil.displayError("Green value radiu threshold is not a valid number");
					return false;
				}
			}else {
				MipavUtil.displayError("Green value radiu threshold is required");
				return false;
			}
			
			if(!outputFilenameTextField.getText().trim().equals("")) {
				if(outputFilenameTextField.getText().trim().endsWith(".swc")){
					outputFilename = outputFilenameTextField.getText().trim();
				}else {
					MipavUtil.displayError("Invalid SWC filename");
					return false;
				}
				if(outputFilenameTextField_auto.getText().trim().endsWith(".swc")){
					outputFilename_auto = outputFilenameTextField_auto.getText().trim();
				}else {
					MipavUtil.displayError("Invalid SWC filename");
					return false;
				}
				if(outputFilenameTextField_regionGrow.getText().trim().endsWith(".swc")){
					outputFilename_regionGrow = outputFilenameTextField_regionGrow.getText().trim();
				}else {
					MipavUtil.displayError("Invalid SWC filename");
					return false;
				}
			}else {
				MipavUtil.displayError("SWC output filename is required");
				return false;
			}
		}
		
		
		return true;
	}
	
	/**
	 * call algorithm
	 */
	public void callAlgorithm() {
		float samplingRate = Float.valueOf((String)surfaceFileSamplingCB.getSelectedItem()).floatValue();

		alg = new PlugInAlgorithmDrosophilaStandardColumnRegistration(neuronImage,pointsMap,allFilamentCoords,surfaceFile,samplingRate,cityBlockImage,pointsFile,outputTextArea,flipXCB.isSelected(), flipYCB.isSelected(), flipZCB.isSelected(),greenThreshold,subsamplingDistance,rigRadio.isSelected(),doSWC,rvldRadio.isSelected(),numPoints);
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
	public boolean readPointsFile(File pointsFile) {
		RandomAccessFile raFile = null;
		try {
			pointsMap = new TreeMap<Integer, float[]>();
            raFile = new RandomAccessFile(pointsFile, "r");
            String[] arr;
            //String[] arr2;
            String line;
            float x,y,z;
            int counter = 1;
            while((line = raFile.readLine()) != null) {
            		arr = line.trim().split(",");
            		if(arr.length == 4) {
            			if(arr[0].trim().equals("") && arr[1].trim().equals("") && arr[2].trim().equals("")) {
            				pointsMap.put(new Integer(counter), null);
	            		}else {
	                    	x = new Float(arr[0]).floatValue();
	                    	y = new Float(arr[1]).floatValue();
	                    	z = new Float(arr[2]).floatValue();
	                    	float[] f = {x,y,z};
	                    	pointsMap.put(new Integer(counter), f);
	            		}
                    }else {
                    	return false;
                    	/*if(arr.length == 1) {
                    		pointsMap.put(new Integer(counter), null);
                    	}else {
                    		return false;
                    	}*/
                    }
            		counter++;
            	
            }
            raFile.close();
            if(pointsMap.size() == 27 || pointsMap.size() == 75 || pointsMap.size() == 147) {
            	return true;
            }else {
            	return false;
            }
		}catch(Exception e) {
			try {
				if(raFile != null) {
					raFile.close();
				}
			}catch(Exception ex) {
				
			}
			e.printStackTrace();
			return false;
		}
	}
	
	
	/**
	 * creates city block image
	 */
	public void createCityBlockImage() {
		
		int[] extents = {512,512,512};
        cityBlockImage = new ModelImage(ModelImage.UBYTE, extents,"cityBlockImage");
        float[] cityBlockImageResols = new float[3];
        cityBlockImageResols[0] = neuronImage.getResolutions(0)[0];
        cityBlockImageResols[1] = neuronImage.getResolutions(0)[1];
        cityBlockImageResols[2] = neuronImage.getResolutions(0)[2];
		for(int i=0;i<cityBlockImage.getExtents()[2];i++) {
			cityBlockImage.setResolutions(i, cityBlockImageResols);
		}
		
		FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[cityBlockImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(neuronImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(neuronImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(neuronImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(neuronImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(neuronImage.getExtents());
            fileInfoBases[i].setOrigin(neuronImage.getFileInfo()[0].getOrigin());

        }

        cityBlockImage.setFileInfo(fileInfoBases);
        byte[] cityBlockImageBuffer = new byte[512*512*512];
        for(int i=0;i<cityBlockImageBuffer.length;i++) {
        	cityBlockImageBuffer[i] = 100;	
        }
        try {
        	cityBlockImage.importData(0, cityBlockImageBuffer, true);
        } catch (IOException error) {
            System.out.println("IO exception");
            error.printStackTrace();
            return;
        }
        
        
         int x1,y1,z1;
         
         ArrayList<int[]> zeroCoords = new ArrayList<int[]>();

		 
		 int allFilamentsSize = allFilamentCoords.size();
		 int alSize;
		 ArrayList<float[]> al;
		 float[] coords;
		 for(int i=0;i<allFilamentsSize;i++) {
			 al = allFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 x1 = Math.round(coords[0]);
				 y1 = Math.round(coords[1]);
				 z1 = Math.round(coords[2]);
				 int[] arr = {x1,y1,z1};
		         zeroCoords.add(arr);
		         cityBlockImage.set(x1,y1,z1,0);
		         
			 }
		 }

		 
		 ArrayList<int[]> oneCoords = new ArrayList<int[]>();
		 for(int i=0;i<zeroCoords.size();i++) {
			int[] zeroCoord = zeroCoords.get(i);
			int zeroX = zeroCoord[0];
			int zeroY = zeroCoord[1];
			int zeroZ = zeroCoord[2];
			
			int zeroXPlus1 = zeroX + 1;
			int zeroXMinus1 = zeroX - 1;
			int zeroYPlus1 = zeroY + 1;
			int zeroYMinus1 = zeroY - 1;
			int zeroZPlus1 = zeroZ + 1;
			int zeroZMinus1 = zeroZ - 1;
			
			
			
			if ((cityBlockImage.get(zeroXPlus1, zeroY, zeroZ)).byteValue() == 100) {
				cityBlockImage.set(zeroXPlus1,zeroY,zeroZ,1);
				int[] arr = {zeroXPlus1,zeroY,zeroZ};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroXMinus1, zeroY, zeroZ)).byteValue() == 100) {
				cityBlockImage.set(zeroXMinus1,zeroY,zeroZ,1);
				int[] arr = {zeroXMinus1,zeroY,zeroZ};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroX, zeroYPlus1, zeroZ)).byteValue() == 100) {
				cityBlockImage.set(zeroX,zeroYPlus1,zeroZ,1);
				int[] arr = {zeroX,zeroYPlus1,zeroZ};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroX, zeroYMinus1, zeroZ)).byteValue() == 100) {
				cityBlockImage.set(zeroX,zeroYMinus1,zeroZ,1);
				int[] arr = {zeroX,zeroYMinus1,zeroZ};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroX, zeroY, zeroZPlus1)).byteValue() == 100) {
				cityBlockImage.set(zeroX,zeroY,zeroZPlus1,1);
				int[] arr = {zeroX,zeroY,zeroZPlus1};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroX, zeroY, zeroZMinus1)).byteValue() == 100) {
				cityBlockImage.set(zeroX,zeroY,zeroZMinus1,1);
				int[] arr = {zeroX,zeroY,zeroZMinus1};
				oneCoords.add(arr);
			}
		 }
		 
		 
		 
		 
		 

		 for(int i=0;i<oneCoords.size();i++) {
				int[] oneCoord = oneCoords.get(i);
				int oneX = oneCoord[0];
				int oneY = oneCoord[1];
				int oneZ = oneCoord[2];
				
				int oneXPlus1 = oneX + 1;
				int oneXMinus1 = oneX - 1;
				int oneYPlus1 = oneY + 1;
				int oneYMinus1 = oneY - 1;
				int oneZPlus1 = oneZ + 1;
				int oneZMinus1 = oneZ - 1;
				
				
				
				if ((cityBlockImage.get(oneXPlus1, oneY, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneXPlus1,oneY,oneZ,2);
				}
				if ((cityBlockImage.get(oneXMinus1, oneY, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneXMinus1,oneY,oneZ,2);
				}
				if ((cityBlockImage.get(oneX, oneYPlus1, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneYPlus1,oneZ,2);
				}
				if ((cityBlockImage.get(oneX, oneYMinus1, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneYMinus1,oneZ,2);
				}
				if ((cityBlockImage.get(oneX, oneY, oneZPlus1)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneY,oneZPlus1,2);
				}
				if ((cityBlockImage.get(oneX, oneY, oneZMinus1)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneY,oneZMinus1,2);
				}
		 }

		 cityBlockImage.calcMinMax();
                                  
	}
        
		
		

	
	
	
	
	
	public synchronized boolean isPlugInCompleted() {
		return plugInCompleted;
	}

	public synchronized void setPlugInCompleted(boolean plugInCompleted) {
		this.plugInCompleted = plugInCompleted;
	}

	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			 OKButton.setEnabled(false);
			 cancelButton.setText("Close");
			 
			 outputTextArea.append("Finished" + "\n");
			 setPlugInCompleted(true);
		}
		
	}
	
	
	


}
