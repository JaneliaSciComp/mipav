package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmMeanShiftClustering;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Enumeration;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

@SuppressWarnings("serial")
public class JDialogMeanShiftClustering extends JDialogScriptableBase implements AlgorithmInterface {
	public static final int EVERY_POINT = 1;
	public static final int SELECT_ON_JUMP = 2;
	public static final int SELECT_PERCENT = 3;
	
	/** handle to algorithm **/
    private AlgorithmMeanShiftClustering alg;
    
    /** source image. **/
    private ModelImage image;
    
    /** result image **/
    private ModelImage resultImage;
    
    // K, L are LSH parameters.  If either K or L is not greater than zero, the LSH data structure is not built
 	// and the linear algorithm is run.
    // The readme file uses K = 30 and K = 24.
 	private int K;
    // The readme file uses L = 46 and L = 35.
 	private int L;
 	// k_neigh is the number of neighbors used in the construction of pilot density
    // In the article : The data is processed correctly from k_neigh = 100 to k_neigh = 500
 	// except for a few points, and even for k_neigh = 700 only some of the points in the
 	// cluster with the largest spread converge to the adjacent node.  The readme file uses k_neigh = 200.
 	private int k_neigh;
 	// input_directory and data_file_name provide the location of the input data file.
 	private String data_file_name;
 	private String input_directory;
 	// jump and percent are two ways to choose from which points to start the mean shift procedure.
 	// The default is to perform the procedure on every point.
 	// jump means once every jump a point is selected.  If jump is 5 then the points 1,6,11,16,...
 	// are chosen.  percent means that (percent*n)/100 (n the number of points) are chosen at
 	// random with replacement.  Obviously, each run of the program will yield a different set.
 	// Only 1 of EVERY_POINT, SELECT_ON_JUMP, or SELECT_percent can be chosen.
 	private int choosePoints;
 	// jump must be at least 1.  The readme file uses jump = 5.
 	private int jump = 5;
 	// Need 0.0 <= percent <= 100.0
 	private double percent = 0.0;
 	// if fixedWidth is true, the user runs the fixed bandwidth mean shift procedure.
 	// The width * d (d is the dimension of the data) is the distance under the L1 norm
 	// used as the fixed bandwidth.
 	// This is the bandwidth value associated with the data points
 	// The article uses values of 100, 1400, 2700, and 4000.
 	private boolean fixedWidth;
 	private float width = -1;
 	// If findOptimalKL is true, the program automatically computes the optimal K and L.
 	// The optimal K is searched between Kmin and K (the first parameter) with step Kjump.
 	// The optimal L is searched between 1 and L (the second parameter)
    // epsilon, Kmin, and Kjump are only used when findOptimalKL is true.
 	// epsilon represents the allowed error (in experiments epsilon = 0.05)
 	private boolean findOptimalKL;
 	private float epsilon;
    // The readme file uses Kmin = 10.
 	private int Kmin;
    // The readme file uses Kjump = 2.
 	private int Kjump;
 	private boolean FAMS_DO_SPEEDUP;
 	private int nPoints;
 	private int nDims;
 	private float pttemp[];
 	private ModelImage modesImage = null;
 	private ModelImage prunedModesImage = null;
 	
    private JTextField textImage;
    
    private JButton buttonImage;
    
    private JComboBox imageList;
 	
 	private JTextField textPointsFile;
     
    private JButton buttonPointsFile;
    
    private File filePoints;
    
    private boolean havePoints = false;
    
    private JTextField KText;
    
    private JTextField LText;
    
    private JCheckBox optimalCheckBox;
    
    private JLabel optimal1Label;
    
    private JLabel optimal2Label;
    
    private JLabel epsilonLabel;
    
    private JTextField epsilonText;
    
    private JLabel KminLabel;
    
    private JTextField KminText;
    
    private JLabel KjumpLabel;
    
    private JTextField KjumpText;
    
    private JTextField neighborText;
    
    private ButtonGroup chooseGroup;
    
    private JRadioButton everyButton;
    
    private JRadioButton jumpButton;
    
    private JLabel jumpLabel;
    
    private JTextField jumpText;
    
    private JRadioButton percentButton;
    
    private JLabel percentLabel;
    
    private JTextField percentText;
    
    private JCheckBox fixedCheckBox;
    
    private JLabel widthLabel;
    
    private JTextField widthText;
    
    private JCheckBox speedupCheckBox;
 	
 	public JDialogMeanShiftClustering() {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		init();
	}
 	
 	/**
	 *  algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if (algorithm instanceof AlgorithmMeanShiftClustering) {

            if ((alg.isCompleted() == true) && (image != null)) {

                
               
            }
		}
		
		if (algorithm.isCompleted()) {
            insertScriptLine();
        }
		alg.finalize();
        alg = null;
		dispose();
	}
 	
 	/**
	 * init
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    @SuppressWarnings("unchecked")
	private void init() {

    	GuiBuilder gui = new GuiBuilder(this);
    	
        JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridx = 0;
    	setForeground(Color.black);
        setTitle("Mean Shift Clustering");
        
        JLabel choiceLabel = new JLabel("Choose an image or a points file");
        choiceLabel.setForeground(Color.black);
        choiceLabel.setFont(serif12);
        mainPanel.add(choiceLabel, gbc);
        
        buttonImage = new JButton("Choose an image");
        buttonImage.setForeground(Color.black);
        buttonImage.setFont(serif12B);
        buttonImage.addActionListener(this);
        buttonImage.setPreferredSize(new Dimension(235, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        
        Object[] imgList = new Object[ViewUserInterface.getReference().getRegisteredImagesNum()+1];
        imgList[0] = "Load image...";
        Enumeration<String> strEnum = ViewUserInterface.getReference().getRegisteredImageNames();
        for(int i=1; i<imgList.length; i++) {
        	imgList[i] = strEnum.nextElement();
        }
        imageList = gui.buildComboBox("Choose an image: ", imgList, 0);
        imageList.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
	            if(imageList.getSelectedIndex() == 0) {
	            	loadImage();
	            } else {
	            	textImage.setText(imageList.getSelectedItem().toString());
	            }
            }
        });
        mainPanel.add(imageList.getParent(), gbc);

        textImage = new JTextField();
        textImage.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textImage, gbc);
        
        buttonPointsFile = new JButton("Open a file of point locations");
        buttonPointsFile.setForeground(Color.black);
        buttonPointsFile.setFont(serif12B);
        buttonPointsFile.addActionListener(this);
        buttonPointsFile.setActionCommand("PointFile");
        buttonPointsFile.setPreferredSize(new Dimension(225, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(buttonPointsFile, gbc);
        
        textPointsFile = new JTextField();
        textPointsFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textPointsFile, gbc);
        
        JLabel KLabel = new JLabel("K");
        KLabel.setForeground(Color.black);
        KLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(KLabel, gbc);
        
        KText = new JTextField("30");
        KText.setForeground(Color.black);
        KText.setFont(serif12);;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(KText, gbc);
        
        JLabel LLabel = new JLabel("L");
        LLabel.setForeground(Color.black);
        LLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(LLabel, gbc);
        
        LText = new JTextField("46");
        LText.setForeground(Color.black);
        LText.setFont(serif12);;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(LText, gbc);
        
        optimalCheckBox = new JCheckBox("Find optimal K and L");
        optimalCheckBox.setSelected(true);
        optimalCheckBox.setFont(serif12);
        optimalCheckBox.setForeground(Color.black);
        optimalCheckBox.addActionListener(this);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(optimalCheckBox, gbc);
        
        optimal1Label = new JLabel("Optimal K is searched between Kmin and K");
        optimal1Label.setForeground(Color.black);
        optimal1Label.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(optimal1Label, gbc);
        
        optimal2Label = new JLabel("Optimal L is searched between 1 and L");
        optimal2Label.setForeground(Color.black);
        optimal2Label.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(optimal2Label, gbc);
        
        epsilonLabel = new JLabel("Allowed error epsilon");
        epsilonLabel.setForeground(Color.black);
        epsilonLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(epsilonLabel, gbc);
        
        epsilonText = new JTextField("0.05");
        epsilonText.setForeground(Color.black);
        epsilonText.setFont(serif12);;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(epsilonText, gbc);
        
        KminLabel = new JLabel("Kmin");
        KminLabel.setForeground(Color.black);
        KminLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(KminLabel, gbc);
        
        KminText = new JTextField("10");
        KminText.setForeground(Color.black);
        KminText.setFont(serif12);;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(KminText, gbc);
        
        KjumpLabel = new JLabel("Kjump");
        KjumpLabel.setForeground(Color.black);
        KjumpLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(KjumpLabel, gbc);
        
        KjumpText = new JTextField("2");
        KjumpText.setForeground(Color.black);
        KjumpText.setFont(serif12);;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(KjumpText, gbc);
        
        JLabel neighborLabel = new JLabel("Number of neighbors");
        neighborLabel.setForeground(Color.black);
        neighborLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(neighborLabel, gbc);
        
        neighborText = new JTextField("200");
        neighborText.setForeground(Color.black);
        neighborText.setFont(serif12);;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(neighborText, gbc);
        
        chooseGroup = new ButtonGroup();
        everyButton = new JRadioButton("Every point", true);
        everyButton.setFont(serif12);
        everyButton.setForeground(Color.black);
        everyButton.addActionListener(this);
        chooseGroup.add(everyButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(everyButton, gbc);
        
        jumpButton = new JRadioButton("Select points on jump", false);
        jumpButton.setFont(serif12);
        jumpButton.setForeground(Color.black);
        jumpButton.addActionListener(this);
        chooseGroup.add(jumpButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(jumpButton, gbc);
        
        jumpLabel = new JLabel("jump");
        jumpLabel.setForeground(Color.black);
        jumpLabel.setFont(serif12);
        jumpLabel.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(jumpLabel, gbc);
        
        jumpText = new JTextField("5");
        jumpText.setForeground(Color.black);
        jumpText.setFont(serif12);
        jumpText.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(jumpText, gbc);
        
        percentButton = new JRadioButton("Select percent points", false);
        percentButton.setFont(serif12);
        percentButton.setForeground(Color.black);
        percentButton.addActionListener(this);
        chooseGroup.add(percentButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(percentButton, gbc);
        
        percentLabel = new JLabel("percent");
        percentLabel.setForeground(Color.black);
        percentLabel.setFont(serif12);
        percentLabel.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(percentLabel, gbc);
        
        percentText = new JTextField("20.0");
        percentText.setForeground(Color.black);
        percentText.setFont(serif12);
        percentText.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(percentText, gbc);
        
        fixedCheckBox = new JCheckBox("Fixed data points bandwidth");
        fixedCheckBox.setSelected(false);
        fixedCheckBox.setFont(serif12);
        fixedCheckBox.setForeground(Color.black);
        fixedCheckBox.addActionListener(this);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(fixedCheckBox, gbc);
        
        widthLabel = new JLabel("bandwidth");
        widthLabel.setForeground(Color.black);
        widthLabel.setFont(serif12);
        widthLabel.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(widthLabel, gbc);
        
        widthText = new JTextField("1400.0");
        widthText.setForeground(Color.black);
        widthText.setFont(serif12);
        widthText.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(widthText, gbc);
        
        speedupCheckBox = new JCheckBox("Speedup");
        speedupCheckBox.setSelected(true);
        speedupCheckBox.setFont(serif12);
        speedupCheckBox.setForeground(Color.black);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(speedupCheckBox, gbc);
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setSize(600,700);
        setVisible(true);
    }
    
    private void loadImage() {
    	String fileNameBase;
    	int i;
    	boolean isMultifile;
		ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
         JFileChooser chooser = fileChooser.getFileChooser();
         if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }
         chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
         chooser.setDialogTitle("Choose image");
         int returnValue = chooser.showOpenDialog(this);
         if (returnValue == JFileChooser.APPROVE_OPTION) { 	
         	FileIO fileIO = new FileIO();
         	isMultifile = fileChooser.isMulti();
         	image = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator,
         			                 isMultifile, null);
         	i = chooser.getSelectedFile().getName().indexOf(".");
			if (i > 0) {
				fileNameBase = chooser.getSelectedFile().getName().substring(0,i);
			}
			else {
				fileNameBase = new String(chooser.getSelectedFile().getName());
			}
			input_directory = chooser.getCurrentDirectory() + File.separator;
			data_file_name = fileNameBase + ".txt";
    		if (image.isComplexImage()) {
    			MipavUtil.displayError("Image cannot be a complex image");
    		    image.disposeLocal();
    		    image = null;
    		    return;	
    		}
    		
    		imageList.addItem(image.getImageName());
    		imageList.setSelectedItem(image.getImageName());
    		textImage.setText(image.getImageName());
         } 
     } 
    
    /**
	 *  action performed
	 */
	public void actionPerformed(ActionEvent event) {
		RandomAccessFile raFile;
		String firstLine;
		String values[];
		int numValues;
		int presentValues;
		String dataLine;
		int i;
		boolean selected;
		String extension = null;
		boolean haveCSV = false;
		Object source = event.getSource();
		String command = event.getActionCommand();
		 if (command.equals("OK")) {
			 if (setVariables()) {
			     callAlgorithm();
			 }
	     } 
		 else if (command.equals("Cancel")) {
	    	 if (image != null) {
	    		 image.disposeLocal();
	    		 image = null;
	    	 }
	         dispose();
	     } else if (command.equals("Help")) {
	            //MipavUtil.showHelp("");
	     } else if (command.equals("PointFile")) {

	            try {
	                JFileChooser chooser = new JFileChooser();

	                if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
	                    File file = new File(ViewUserInterface.getReference().getDefaultDirectory());

	                    if (file != null) {
	                        chooser.setCurrentDirectory(file);
	                    } else {
	                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	                    }
	                } else {
	                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	                }

	                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
	                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
	                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
	                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

	                chooser.setDialogTitle("Open file of point locations");
	                input_directory = String.valueOf(chooser.getCurrentDirectory()) + File.separator;

	                int returnValue = chooser.showOpenDialog(ViewUserInterface.getReference().getMainFrame());

	                if (returnValue == JFileChooser.APPROVE_OPTION) {
	                    data_file_name = chooser.getSelectedFile().getName();
	                    input_directory = String.valueOf(chooser.getCurrentDirectory()) + File.separator;
	                    ViewUserInterface.getReference().setDefaultDirectory(input_directory);
	                } else {
	                    data_file_name = null;

	                    return;
	                }

	                if (data_file_name != null) {
	                	filePoints = new File(input_directory + data_file_name);
	                	i = data_file_name.indexOf(".");
	    				if (i > 0) {
	    					extension = chooser.getSelectedFile().getName().substring(i+1);
	    					if (extension.equalsIgnoreCase("CSV")) {
	    						haveCSV = true;
	    					}
	    				}
	    				if (haveCSV) {
	    					try {
		    					FileReader fr;
	            				fr = new FileReader(filePoints);
	            				BufferedReader br = new BufferedReader(fr);
	            				String line = br.readLine();
	            				line = br.readLine();
	            				Vector<Float>xVector = new Vector<Float>();
	            				Vector<Float>yVector = new Vector<Float>();
	            				Vector<Float>zVector = new Vector<Float>();
	            				nPoints = 0;
	            				nDims = 3;
	            				while ( line != null )
	            				{
	            					String[] parsed = line.split( "," );
	            					if ( parsed.length != 0 )
	            					{
	            						float z    = (parsed.length > 0) ? (parsed[0].length() > 0) ? Float.valueOf( parsed[0] ) : 0 : 0; 
	            						float x    = (parsed.length > 1) ? (parsed[1].length() > 0) ? Float.valueOf( parsed[1] ) : 0 : 0; 
	            						float y    = (parsed.length > 2) ? (parsed[2].length() > 0) ? Float.valueOf( parsed[2] ) : 0 : 0; 
	            						xVector.add(x);
	            						yVector.add(y);
	            						zVector.add(z);
	            		           	   	nPoints++;
	            					}
	            					line = br.readLine();
		            				}
		            				fr.close();	
		    					
		    					numValues = nPoints * nDims;
		    					pttemp = new float[numValues];
		    					for (i = 0; i < nPoints; i++) {
		    						pttemp[3*i] = xVector.get(i);
		    						pttemp[3*i+1] = yVector.get(i);
		    						pttemp[3*i+2] = zVector.get(i);
		    					}
	    					}
	    					catch (IOException e) {
	    						MipavUtil.displayError("IOException on BufferedReader");
	    		            	return;
	    					}
	    				} // if (haveCSV)
	    				else { // not CSV
		                	try {
		            	    	raFile = new RandomAccessFile(filePoints, "r");
		            	    }
		                	catch (FileNotFoundException e) {
		                		MipavUtil.displayError((input_directory + data_file_name) + " was not found");
		                        return;	
		                	}
		            	    
		            	    do {
		            		    try {
		            		    	firstLine = raFile.readLine();
		            		    }
		            		    catch (IOException e) {
		            		    	MipavUtil.displayError("IOException " + e + " firstLine = raFile.readLine)"); 
		            		    	return;
		            		    }
		            	    } while ((firstLine == null) || (firstLine.isEmpty()));
		            	    values = retrieveValues(firstLine);
		            	    if (values == null) {
		            	    	MipavUtil.displayError("No values found in first line");
		            	    	return;
		            	    }
		            	    if (values.length != 2) {
		            	    	MipavUtil.displayError("First line has " + values.length + " values instead of the expected 2");
		            	    	return;
		            	    }
		            	    try {
		            	        nPoints = Integer.parseInt(values[0]);
		            	    }
		            	    catch (NumberFormatException e) {
		            	    	MipavUtil.displayError("NumberFormatException " + e + " on nPoints = Integer.parseInt(values[0])");
		            	    	return;
		            	    }
		            	    if (nPoints < 1) {
		            	    	MipavUtil.displayError("nPoints = " + nPoints);
		            	    	return;
		            	    }
		            	    try {
		            	    	nDims= Integer.parseInt(values[1]);
		            	    }
		            	    catch (NumberFormatException e) {
		            	    	MipavUtil.displayError("NumberFormatException " + e + " on nDims = Integer.parseInt(values[1])");
		            	    	return;
		            	    }
		            	    if (nDims < 1) {
		            	    	MipavUtil.displayError("nDims = " + nDims);
		            	    	return;
		            	    }
		            	    numValues = nPoints * nDims;
		            	    presentValues = 0;
		            	    // Allocate data
		            	    pttemp = new float[numValues];
		            	    while (presentValues < numValues) {
		            	    	try {
		            		    	dataLine = raFile.readLine();
		            		    }
		            		    catch (IOException e) {
		            		    	MipavUtil.displayError("IOException " + e + " dataLine = raFile.readLine)"); 
		            		    	return;
		            		    }
		            	    	 values = retrieveValues(dataLine);
		            	    	 if (values != null) {
		            	    		 for (i = 0; i < values.length && presentValues < numValues; i++) {
		            	    			 try {
		            	    				 pttemp[presentValues++] = Float.parseFloat(values[i]);
		            	    			 }
		            	    			 catch (NumberFormatException e) {
		            	    				 MipavUtil.displayError("NumberFormatException " + e + " pttemp["+presentValues+"++] = "+
		            	    			     "Float.parseFloat(values["+i+"])");
		            	    			    return;	 
		            	    			 }
		            	    		 } // for (i = 0; i < values.length; i++)
		            	    	 } // if (values != null)
		            	    } // while (presentValues < numValues)
		            	    try {
		            	    	raFile.close();
		            	    }
		            	    catch (IOException e) {
		            	    	MipavUtil.displayError("IOException " + e + " on raFile.close()");
		            	    	return;
		            	    }
	    				} // else not CSV
	                    
	                    
                     havePoints = true;
	                 textPointsFile.setText(data_file_name);
	                }
	            } catch (OutOfMemoryError e) {
	                MipavUtil.displayError("Out of memory in JDialogMeanShiftClustering");

	                return;
	            }
	     } else if (source == optimalCheckBox) {
	         selected = optimalCheckBox.isSelected();
	         optimal1Label.setEnabled(selected);
	         optimal2Label.setEnabled(selected);
	         epsilonLabel.setEnabled(selected);
	         epsilonText.setEnabled(selected);
	         KminLabel.setEnabled(selected);
	         KminText.setEnabled(selected);
	         KjumpLabel.setEnabled(selected);
	         KjumpText.setEnabled(selected);
	     } else if ((source == everyButton) || (source == jumpButton) || (source == percentButton)) {
	    	 if (everyButton.isSelected()) {
	    		 jumpLabel.setEnabled(false);
	    		 jumpText.setEnabled(false);
	    		 percentLabel.setEnabled(false);
	    		 percentText.setEnabled(false);
	    	 }
	    	 else if (jumpButton.isSelected()) {
	    		 jumpLabel.setEnabled(true);
	    		 jumpText.setEnabled(true);
	    		 percentLabel.setEnabled(false);
	    		 percentText.setEnabled(false);	 
	    	 }
	    	 else {
	    		 jumpLabel.setEnabled(false);
	    		 jumpText.setEnabled(false);
	    		 percentLabel.setEnabled(true);
	    		 percentText.setEnabled(true);
	    	 }
	     } else if (source == fixedCheckBox) {
	    	 selected = fixedCheckBox.isSelected();
	    	 widthLabel.setEnabled(selected);
	    	 widthText.setEnabled(selected);
	    } else {
         super.actionPerformed(event);
     }
	}
	
	private String[] retrieveValues(String inString) {
        String outString[] = null;
        int i;
        int numValues = 0;
        Vector<Integer> firstValue = new Vector<Integer>();
        Vector<Integer> lastValue = new Vector<Integer>();

        if ((inString != null) && (!inString.isEmpty())) {
        	for (i = 0; i < inString.length(); i++) {
        	    if ((inString.charAt(i) > 0x20) &&	((i == 0) || (inString.charAt(i-1) <= 0x20))) {
        	    	numValues++;
        	    	firstValue.add(i);
        	    	if (i == inString.length() - 1) {
        	    		lastValue.add(i);
        	    	}
        	    }
        	    else if ((inString.charAt(i) <= 0x20) && (inString.charAt(i-1) > 0x20)) {
        	    	lastValue.add(i-1);
        	    }
        	    else if ((i == inString.length() - 1) && (inString.charAt(i) > 0x20)) {
        	    	lastValue.add(i);
        	    }
        	}
        	outString = new String[numValues];
        	char[] val = new char[inString.length()];
            for (i = 0; i < inString.length(); i++) {
            	val[i] = inString.charAt(i);
            }
        	for (i = 0; i < numValues; i++) {
        	    outString[i] = new String(val, firstValue.get(i), lastValue.get(i) - firstValue.get(i) + 1);    
        	} // for (i = 0; i < numValues; i++)
            
            return outString;
            
        } // if ((inString != null) && (!inString.isEmpty()))
        else {
            return null;
        }

    }
	
	private boolean setVariables() {
		
		int length;
		int i;
		double buffer[];
		int numValues;
		int xDim;
		int yDim;
		int zDim;
		int tDim;
		int extents[];
		int sliceSize;
		int volume;
		int nval;
		int x;
		int y;
		int z;
		int t;
		int index;
		String tmpStr;
		String imageName;

		if (!havePoints) {
    		image = ViewUserInterface.getReference().getRegisteredImageByName(imageList.getSelectedItem().toString());
        	
        	if(image == null) {
        		MipavUtil.displayError("No image with name "+imageList.getSelectedItem()+" was found.");
        		return false;
        	}
        	new ViewJFrameImage(image);
        	input_directory = image.getImageDirectory();
        	data_file_name = image.getImageName() + ".txt";
        	extents = image.getExtents();
        	// modesImage has too many modes to be useful
        	// imageName  = image.getImageName() + "_modes";
            // modesImage = new ModelImage(ModelStorageBase.INTEGER, extents, imageName);
        	imageName = image.getImageName() + "_prunedModes";
        	prunedModesImage = new ModelImage(ModelStorageBase.INTEGER, extents, imageName);
        	
        	nDims = image.getNDims();
            length = extents[0];
            for (i = 1; i < nDims; i++) {
           	 length = length * extents[i];
            }
            buffer = new double[length];
            try {
            	image.exportData(0, length, buffer);
            }
            catch(IOException e) {
            	MipavUtil.displayError("IOException on image.exportData(0, length, buffer)");
            	image.disposeLocal();
            	image = null;
            	return false;
            }
            nPoints = 0;
	        for (i = 0; i < length; i++) {
	        	if (buffer[i] > 0) {
	        		nPoints++;
	        	}
	        }
	        if (nPoints == 0) {
	        	MipavUtil.displayError("No set of point values found in " + image.getImageFileName());
	        	image.disposeLocal();
	        	image = null;
                return false;	 
	        }
	        numValues = nPoints * nDims;
	        pttemp = new float[numValues];
	        if (nDims >= 4) {
              	 tDim = extents[3];
               }
               else {
              	 tDim = 1;
               }
   	         if (nDims >= 3) {
   	        	 zDim = extents[2];
   	         }
   	         else {
   	        	 zDim = 1;
   	         }
   	         if (nDims >= 2) {
   	        	 yDim = extents[1];
   	         }
   	         else {
   	        	 yDim = 1;
   	         }
   	         xDim = extents[0];
   	         sliceSize = xDim * yDim;
   	         volume = sliceSize * zDim;
   	         nval = 0;
   	         for (t = 0; t < tDim; t++) {
   	        	 for (z = 0; z < zDim; z++) {
   	        		 for (y = 0; y < yDim; y++) {
   	        			 for (x = 0; x < xDim; x++) {
   	        			     index = x + y*xDim + z*sliceSize + t*volume;
   	        			     if (buffer[index] > 0) {
   	        			    	 pttemp[nval++] = x;
   	        			         if (nDims >= 2) {
   	        			        	 pttemp[nval++] = y;
   	        			        	 if (nDims >= 3) {
   	        			        		 pttemp[nval++] = z;
   	        			        		 if (nDims >= 4) {
   	        			        			 pttemp[nval++] = t;
   	        			        		 }
   	        			        	 }
   	        			         }
   	        			     } // if (buffer[index] > 0)
   	        			 }
   	        		 }
   	        	 }
   	         } // for (t = 0; t < tDim; t++)
   	         buffer = null;
            textImage.setText(image.getImageFileName());
            havePoints = true;
		}
		
		if (!havePoints) {
    	    MipavUtil.displayError("Must obtain points from a text file or an image");
    	    return false;
    	}
		
		tmpStr = KText.getText();
		try {
		    K = Integer.parseInt(tmpStr);
		}
		catch (NumberFormatException e) {
			MipavUtil.displayError("The K entry is not a valid integer");
			KText.requestFocus();
			KText.selectAll();
			return false;
		}
		if (K <= 0) {
			MipavUtil.displayError("K must be greater than zero");
			KText.requestFocus();
			KText.selectAll();
			return false;
		}
		
		tmpStr = LText.getText();
		try {
		    L = Integer.parseInt(tmpStr);
		}
		catch (NumberFormatException e) {
			MipavUtil.displayError("The L entry is not a valid integer");
			LText.requestFocus();
			LText.selectAll();
			return false;
		}
		if (L <= 0) {
			MipavUtil.displayError("L must be greater than zero");
			LText.requestFocus();
			LText.selectAll();
			return false;
		}
		
		findOptimalKL = optimalCheckBox.isSelected();
		
		if (findOptimalKL) {
			tmpStr = epsilonText.getText();
			try {
				epsilon = Float.parseFloat(tmpStr);
			}
			catch (NumberFormatException e) {
				MipavUtil.displayError("The epsilon entry is not a valid float");
				epsilonText.requestFocus();
				epsilonText.selectAll();
				return false;
			}
			if (epsilon <= 0) {
				MipavUtil.displayError("epsilon must be greater than zero");
				epsilonText.requestFocus();
				epsilonText.selectAll();
				return false;
			}
			if (epsilon >= 1) {
				MipavUtil.displayError("epsilon must be less than 1");
				epsilonText.requestFocus();
				epsilonText.selectAll();
				return false;
			}
			
			tmpStr = KminText.getText();
			try {
			    Kmin = Integer.parseInt(tmpStr);
			}
			catch (NumberFormatException e) {
				MipavUtil.displayError("The Kmin entry is not a valid integer");
				KminText.requestFocus();
				KminText.selectAll();
				return false;
			}
			if (Kmin <= 0) {
				MipavUtil.displayError("Kmin must be greater than zero");
				KminText.requestFocus();
				KminText.selectAll();
				return false;
			}
			if (Kmin > K) {
				MipavUtil.displayError("Kmin must not be greater than K");
				KminText.requestFocus();
				KminText.selectAll();
				return false;
			}
			
			tmpStr = KjumpText.getText();
			try {
			    Kjump = Integer.parseInt(tmpStr);
			}
			catch (NumberFormatException e) {
				MipavUtil.displayError("The Kjump entry is not a valid integer");
				KjumpText.requestFocus();
				KjumpText.selectAll();
				return false;
			}
			if (Kjump <= 0) {
				MipavUtil.displayError("Kjump must be greater than zero");
				KjumpText.requestFocus();
				KjumpText.selectAll();
				return false;
			}
		} // if (findOptimalKL)
		
		tmpStr = neighborText.getText();
		try {
		    k_neigh = Integer.parseInt(tmpStr);
		}
		catch (NumberFormatException e) {
			MipavUtil.displayError("The neighbor entry is not a valid integer");
			neighborText.requestFocus();
			neighborText.selectAll();
			return false;
		}
		if (k_neigh <= 0) {
			MipavUtil.displayError("neighbor must be greater than zero");
			neighborText.requestFocus();
			neighborText.selectAll();
			return false;
		}
		
		if (everyButton.isSelected()) {
			choosePoints = EVERY_POINT;
			percent = 100.0;
		}
		else if (jumpButton.isSelected()) {
			tmpStr = jumpText.getText();
			try {
			    jump = Integer.parseInt(tmpStr);
			}
			catch (NumberFormatException e) {
				MipavUtil.displayError("The jump entry is not a valid integer");
				jumpText.requestFocus();
				jumpText.selectAll();
				return false;
			}
			if (jump <= 0) {
				MipavUtil.displayError("jump must be greater than zero");
				jumpText.requestFocus();
				jumpText.selectAll();
				return false;
			}
			percent = 0.0;
		}
		else {
			tmpStr = percentText.getText();
			try {
			    percent = Double.parseDouble(tmpStr);
			}
			catch (NumberFormatException e) {
				MipavUtil.displayError("The percent entry is not a valid double");
				percentText.requestFocus();
				percentText.selectAll();
				return false;
			}
			if (percent <= 0) {
				MipavUtil.displayError("percent must be greater than zero");
				percentText.requestFocus();
				percentText.selectAll();
				return false;
			}
			if (percent >= 100.0) {
				MipavUtil.displayError("percent must not be greater than 100");
				percentText.requestFocus();
				percentText.selectAll();
				return false;
			}
		}
		
		fixedWidth = fixedCheckBox.isSelected();
		
		if (fixedWidth) {
			tmpStr = widthText.getText();
			try {
				width = Float.parseFloat(tmpStr);
			}
			catch (NumberFormatException e) {
				MipavUtil.displayError("The width entry is not a valid float");
				widthText.requestFocus();
				widthText.selectAll();
				return false;
			}
			if (width <= 0) {
				MipavUtil.displayError("width must be greater than zero");
				widthText.requestFocus();
				widthText.selectAll();
				return false;
			}	
		} // if (fixedWidth)
		else {
			width = 0.0f;
		}
		
		FAMS_DO_SPEEDUP = speedupCheckBox.isSelected();
		
		return true;
	}
    
    /**
	 *  call algorithm
	 */
	protected void callAlgorithm() {
		try {
			 alg = new AlgorithmMeanShiftClustering(image, K, L, k_neigh, data_file_name, input_directory, choosePoints,
					 jump, percent, fixedWidth, width, findOptimalKL, epsilon, Kmin, Kjump, FAMS_DO_SPEEDUP,
					 nPoints, nDims, pttemp, modesImage, prunedModesImage);
			 
			 
			 //This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            alg.addListener(this);
            
            if (image != null) {
                createProgressBar(image.getImageName(), alg);
            }
            else {
           	 createProgressBar(data_file_name + "_meanShiftClustering", alg);
            }


            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                alg.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Mean Shift Clustering: unable to allocate enough memory");

            return;
        }
		
		
			
	}
	
	 /**
     * get result image
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }
	
    /**
     *  window closing
     */
    public void windowClosing(WindowEvent event) {
        if (image != null) {
        	image.disposeLocal();
        	image = null;
        }
        dispose();
    }
	/**
	 * set GUI from params
	 */
	protected void setGUIFromParams(){
		
	}
	
	/**
	 * store params from gui
	 */
	protected void storeParamsFromGUI() throws ParserException {
		
	}
}