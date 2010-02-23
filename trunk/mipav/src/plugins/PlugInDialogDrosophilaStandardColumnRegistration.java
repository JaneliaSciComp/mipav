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
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
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
	private ModelImage neuronImage,cityBlockImage;
	
	/** points file **/
	private File pointsFile, surfaceFile;
	
	 /** textfields **/
    private JTextField imageFilePathTextField, pointsFilePathTextField, surfaceFilePathTextField;
    
    /** browse button **/
    private JButton imageBrowseButton, pointsBrowseButton, surfaceBrowseButton;
    
    /** **/
    private JLabel pointsLabel, imageLabel, surfaceLabel, surfaceSamplingLabel;
    
    /** **/
    //private JCheckBox minimizeInterpCheckBox;
    
    /** current directory  **/
    private String currDir = null;
    
    /** handle to algorithm **/
    private PlugInAlgorithmDrosophilaStandardColumnRegistration alg;
    
    /** points collection **/
    public TreeMap<Integer, float[]> pointsMap;
    
    ArrayList <ArrayList<float[]>> allFilamentCoords = new ArrayList <ArrayList<float[]>>();
    ArrayList <ArrayList<float[]>> allFilamentNorms = new ArrayList <ArrayList<float[]>>();
    
    private float[] resols;
    
    private JComboBox surfaceFileSamplingCB;
    
    
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
        setTitle("Drosophila Standard Column Registration v1.7");
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
        surfaceFileSamplingCB.setSelectedIndex(1);
        
        
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
		        	resols = neuronImage.getResolutions(0);
		        	imageFilePathTextField.setText(currDir);
		        	surfaceBrowseButton.setEnabled(true);
		        	pointsBrowseButton.setEnabled(true);
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
		        	}else {
		        		pointsFilePathTextField.setText(currDir);
		        	}
		        	
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
		 }else if(command.equals("retinalRegistrationInfoBrowse")) {
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
		 }*/
		 else if(command.equalsIgnoreCase("surfaceBrowse")) {
			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose surface file");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	surfaceFile = new File(currDir);
		        	if(!readSurfaceFile(surfaceFile)) {
		        		MipavUtil.displayError("Error parsing surface file");
		        	}else {
		        		createCityBlockImage();
		        		surfaceFilePathTextField.setText(currDir);
		        	}
		        	
		        }
		 }else if(command.equalsIgnoreCase("cancel")) {
			 if(neuronImage != null) {
				 neuronImage.disposeLocal();
				 neuronImage = null;
			 }
			 dispose();
		 }
	}
	
	
	
	
	private boolean readSurfaceFile(File surfaceFile) {
		boolean success = true;
		
		try {

			RandomAccessFile raFile = new RandomAccessFile(surfaceFile, "r");
			
			String line;
			
			
			while((line=raFile.readLine())!= null) {
				line = line.trim();
				if(line.startsWith("Translate1Dragger")) {
					break;
				}
				if(line.contains("Coordinate3")) {
					//System.out.println();
					//System.out.println();
					ArrayList<float[]> filamentCoords = new ArrayList<float[]>();
					while(!((line=raFile.readLine()).endsWith("}"))) {
						line = line.trim();
						if(!line.equals("")) {
							if(line.startsWith("point [")) {
								line = line.substring(line.indexOf("point [") + 7, line.length()).trim();
							}
							if(line.endsWith("]")) {
								line = line.substring(0, line.indexOf("]")).trim();
							}
							if(line.endsWith(",")) {
								line = line.substring(0, line.indexOf(",")).trim();
							}
							//System.out.println(line);
							String[] splits = line.split("\\s+");
							float coord_x = new Float(splits[0]).floatValue();
							float coord_y = new Float(splits[1]).floatValue();
							float coord_z = new Float(splits[2]).floatValue();
							
							/*int x = Math.round(coord_x/resols[0]);
							int y = Math.round(coord_y/resols[1]);
							int z = Math.round(coord_z/resols[2]);
							
							int[] coords = {x,y,z};*/
							
							float x = coord_x/resols[0];
							float y = coord_y/resols[1];
							float z = coord_z/resols[2];
							  
							float[] coords = {x,y,z,0};
							
							filamentCoords.add(coords);
						}
					}
					allFilamentCoords.add(filamentCoords);
				}
				
				/*if(line.startsWith("Normal")) {
					System.out.println();
					System.out.println();
					Vector<float[]> filamentNorms = new Vector<float[]>();
					while(!((line=raFile.readLine()).endsWith("}"))) {
						line = line.trim();
						if(!line.equals("")) {
							if(line.startsWith("vector [")) {
								line = line.substring(line.indexOf("vector [") + 8, line.length()).trim();
							}
							if(line.endsWith("]")) {
								line = line.substring(0, line.indexOf("]")).trim();
							}
							if(line.endsWith(",")) {
								line = line.substring(0, line.indexOf(",")).trim();
							}
							System.out.println(line);
							String[] splits = line.split("\\s+");
							float norm_x = new Float(splits[0]).floatValue();
							float norm_y = new Float(splits[1]).floatValue();
							float norm_z = new Float(splits[2]).floatValue();
							float[] norms = {norm_z,norm_y,norm_z};
							filamentNorms.add(norms);
						}
					}
					allFilamentNorms.add(filamentNorms);
				}*/
				
				
				
				
			}
			/*for(int i=0;i<allFilamentCoords.size();i++) {
	         	//Vector<float[]> filCoords = allFilamentCoords.get(i);
	         	ArrayList<float[]> filCoords = allFilamentCoords.get(i);
	         	System.out.println("XXXX " + filCoords.size());
	         	
	         	//Vector<float[]> filNorms = allFilamentNorms.get(i);
	         	System.out.println(i);
	         	for(int k=0;k<filCoords.size();k++) {
	         		float[] filCoord = filCoords.get(k);
	         		System.out.println("***** " + filCoord[0] + " " + filCoord[1] + " " + filCoord[2] + ",");
	         	}
			}*/
			raFile.close();
			
		}catch(Exception e) {
			e.printStackTrace();
			return false;
		}
		
		return success;
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
		float samplingRate = Float.valueOf((String)surfaceFileSamplingCB.getSelectedItem()).floatValue();
		alg = new PlugInAlgorithmDrosophilaStandardColumnRegistration(neuronImage,pointsMap,allFilamentCoords,surfaceFile,samplingRate,cityBlockImage);
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
	
	
	
	private void createCityBlockImage() {
		
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
        
        
        
         int key;
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
		 
		 
		 
		 
		 
		 //ArrayList<int[]> twoCoords = new ArrayList<int[]>();
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
					int[] arr = {oneXPlus1,oneY,oneZ};
					//twoCoords.add(arr);
				}
				if ((cityBlockImage.get(oneXMinus1, oneY, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneXMinus1,oneY,oneZ,2);
					int[] arr = {oneXMinus1,oneY,oneZ};
					//twoCoords.add(arr);
				}
				if ((cityBlockImage.get(oneX, oneYPlus1, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneYPlus1,oneZ,2);
					int[] arr = {oneX,oneYPlus1,oneZ};
					//twoCoords.add(arr);
				}
				if ((cityBlockImage.get(oneX, oneYMinus1, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneYMinus1,oneZ,2);
					int[] arr = {oneX,oneYMinus1,oneZ};
					//twoCoords.add(arr);
				}
				if ((cityBlockImage.get(oneX, oneY, oneZPlus1)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneY,oneZPlus1,2);
					int[] arr = {oneX,oneY,oneZPlus1};
					//twoCoords.add(arr);
				}
				if ((cityBlockImage.get(oneX, oneY, oneZMinus1)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneY,oneZMinus1,2);
					int[] arr = {oneX,oneY,oneZMinus1};
					//twoCoords.add(arr);
				}
			 }
		 
		 
		 
		 cityBlockImage.calcMinMax();
		 
         //new ViewJFrameImage(cityBlockImage);                                     
        
        

				 
	
	
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
