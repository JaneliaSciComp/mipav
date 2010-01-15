import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.util.StringTokenizer;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import WildMagic.LibFoundation.Curves.BSplineBasisDiscretef;
import WildMagic.LibFoundation.Curves.BSplineBasisf;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmVOIProps;
import gov.nih.mipav.model.algorithms.BSplineProcessing;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.BSplineLattice3Df;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics;

/**
 * Plugin that applies a series of transformations to one image so that it registers to second image
 * Then, either an average or closest z calculation is done to build a result image of 512x512x512
 * @author pandyan
 *
 */
public class PlugInDialogDrosophilaRetinalRegistration extends JDialogBase implements AlgorithmInterface {
	
	/** grid bag constraints **/
	private GridBagConstraints gbc;
	
	/** main panel **/
	private JPanel mainPanel, optionsPanel, processPanel, interpPanel, rescalePanel, averagingOptionsPanel;
	
	/** images **/
	private ModelImage imageX, imageY, resultImage;
	
	/** transform files **/
	private File transform1File, transform2File, transform3File;
	
	 /** current directory  **/
    private String currDir = null;
    
    /** parent dir **/
    private String parentDir;
    
    /** transform matrices **/
    private TransMatrix matrixGreen, matrixAffine;
	
    /** textfields **/
    private JTextField imageXFilePathTextField, imageYFilePathTextField, transform1FilePathTextField, transform2FilePathTextField, transform3FilePathTextField;
    
    /** browse button **/
    private JButton imageXBrowseButton, imageYBrowseButton, transform1BrowseButton, transform2BrowseButton, transform3BrowseButton;
    
    /** boolean indicating average or closest z **/
	//private boolean doAverage = true;

	/** button group **/
    private ButtonGroup processGroup, interpGroup, rescaleGroup, ignoreBGGroup;

    /** radio buttons **/
    private JRadioButton doAverageRadio, doClosestZRadio, doSqRtRadio, doTrilinearRadio, doBsplineRadio, doRescaleRadio, noRescaleRadio, ignoreBGRadio, includeBGRadio;
    
    /** checkbox **/
    private JCheckBox concatMatricesOnly;
    
    /** 2.5 d */
    private boolean have25D = false;
    
    /** num dims */
    private int nDims;
    
    /** resolutions */
    private float[] resolutions;
    
    /** extents */
    private int[] destExtents;

    /** extents */
    private int destMinExtent;
    
    /** num slices */
    private int numberSlices;
    
    /** spline degree */
    private int splineDegree;
    
    /** number control points */
    private int numControlPoints;
    
    /** control matrix */
    private float[][] controlMat;
    
    /** control matrix */
    private float[][][] controlMat25D;
    
    /** 2D and 3D B-Spline basis definitions. */
    private BSplineBasisDiscretef m_kBSplineBasisX;

    /** b spline */
    private BSplineBasisDiscretef m_kBSplineBasisY;

    /** b spline */
    private BSplineBasisDiscretef m_kBSplineBasisZ;
    
    /** b slpine */
    private BSplineLattice3Df m_kBSpline3D;
    
    /** coefficients need for b-spline **/
    private float[][][] imageX_R_coeff;
    
    /** coefficients need for b-spline **/
    private float[][][] imageX_G_coeff;
    
    /** coefficients need for b-spline **/
    private float[][][] imageX_B_coeff;
    
    /** coefficients need for b-spline **/
    private float[][][] imageY_R_coeff;
    
    /** coefficients need for b-spline **/
    private float[][][] imageY_G_coeff;
    
    /** coefficients need for b-spline **/
    private float[][][] imageY_B_coeff;
    
    /** handle to alg **/
    private AlgorithmVOIProps algoVOIProps;
    
    /** min and maxes of vois **/
    private float minR_X,minG_X,minB_X,minR_Y,minG_Y,minB_Y,maxR_X,maxG_X,maxB_X,maxR_Y,maxG_Y,maxB_Y;
    
    /** slope of transfer function **/
    private float slopeR, slopeG, slopeB;
    
    /** b-intercept of transfer function **/
    private float bR, bG, bB;

    /** vjf to draw vois on **/
    private ViewJFrameImage vjfX, vjfY;

    
	/**
	 * constructor
	 */
	public PlugInDialogDrosophilaRetinalRegistration() {
		
	}

	/**
	 * constructor
	 * @param modal
	 */
	public PlugInDialogDrosophilaRetinalRegistration(boolean modal) {
		super(modal);
		init();
	}
	
	
	/**
	 * init
	 *
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("Drosophila Retinal Registration v1.3");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();

        JLabel imageXLabel = new JLabel("Image H");
        imageXFilePathTextField = new JTextField(35);
        imageXFilePathTextField.setEditable(false);
        imageXFilePathTextField.setBackground(Color.white);
        imageXBrowseButton = new JButton("Browse");
        imageXBrowseButton.addActionListener(this);
        imageXBrowseButton.setActionCommand("imageXBrowse");

        JLabel imageYLabel = new JLabel("Image F");
        imageYFilePathTextField = new JTextField(35);
        imageYFilePathTextField.setEditable(false);
        imageYFilePathTextField.setBackground(Color.white);
        imageYBrowseButton = new JButton("Browse");
        imageYBrowseButton.addActionListener(this);
        imageYBrowseButton.setActionCommand("imageYBrowse");

        JLabel transform1Label = new JLabel("Transformation 1 - Green");
        transform1FilePathTextField = new JTextField(35);
        transform1FilePathTextField.setEditable(false);
        transform1FilePathTextField.setBackground(Color.white);
        transform1BrowseButton = new JButton("Browse");
        transform1BrowseButton.addActionListener(this);
        transform1BrowseButton.setActionCommand("transform1Browse");

        JLabel transform2Label = new JLabel("Transformation 2 - Affine");
        transform2FilePathTextField = new JTextField(35);
        transform2FilePathTextField.setEditable(false);
        transform2FilePathTextField.setBackground(Color.white);
        transform2BrowseButton = new JButton("Browse");
        transform2BrowseButton.addActionListener(this);
        transform2BrowseButton.setActionCommand("transform2Browse");
        
        JLabel transform3Label = new JLabel("Transformation 3 - Nonlinear");
        transform3FilePathTextField = new JTextField(35);
        transform3FilePathTextField.setEditable(false);
        transform3FilePathTextField.setBackground(Color.white);
        transform3BrowseButton = new JButton("Browse");
        transform3BrowseButton.addActionListener(this);
        transform3BrowseButton.setActionCommand("transform3Browse");
        
        processGroup = new ButtonGroup();
        doAverageRadio = new JRadioButton("Average");
        doAverageRadio.addActionListener(this);
        doAverageRadio.setActionCommand("average");
        doSqRtRadio = new JRadioButton("SqrRt(Intensity-H x Intensity-F)");
        doSqRtRadio.addActionListener(this);
        doSqRtRadio.setActionCommand("sqrRt");
        doClosestZRadio = new JRadioButton("Closest Z");
        doClosestZRadio.addActionListener(this);
        doClosestZRadio.setActionCommand("closestZ");
        doAverageRadio.setSelected(true);
        processGroup.add(doAverageRadio);
        processGroup.add(doSqRtRadio);
        //processGroup.add(doClosestZRadio);
        
        interpGroup = new ButtonGroup();
        doTrilinearRadio = new JRadioButton("Trilinear");
        doBsplineRadio = new JRadioButton("B Spline");
        doTrilinearRadio.setSelected(true);
        interpGroup.add(doTrilinearRadio);
        interpGroup.add(doBsplineRadio);
        
        rescaleGroup = new ButtonGroup();
        doRescaleRadio = new JRadioButton("Rescale");
        noRescaleRadio = new JRadioButton("No Rescale");
        doRescaleRadio.setSelected(true);
        rescaleGroup.add(doRescaleRadio);
        rescaleGroup.add(noRescaleRadio);
        
        ignoreBGGroup = new ButtonGroup();
        ignoreBGRadio = new JRadioButton("Ignore background pixels");
        includeBGRadio = new JRadioButton("Include background pixels");
        ignoreBGRadio.setSelected(true);
        ignoreBGGroup.add(ignoreBGRadio);
        ignoreBGGroup.add(includeBGRadio);
        
        
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15,5,5,15);
        
        optionsPanel = new JPanel();
        
        processPanel = new JPanel(new GridBagLayout());
        interpPanel = new JPanel(new GridBagLayout());
        rescalePanel = new JPanel(new GridBagLayout());
        averagingOptionsPanel = new JPanel(new GridBagLayout());
        processPanel.setBorder(new TitledBorder("Process using"));
        interpPanel.setBorder(new TitledBorder("Interpolation"));
        rescalePanel.setBorder(new TitledBorder("Rescale H to F"));
        averagingOptionsPanel.setBorder(new TitledBorder("Averaging Options"));
        
        
        //gbc.anchor = GridBagConstraints.WEST;
        processPanel.add(doAverageRadio,gbc);
        gbc.gridy = 1;
        processPanel.add(doSqRtRadio,gbc);
        //gbc.gridy = 2;
        //processPanel.add(doClosestZRadio,gbc);
        gbc.gridy = 0;
        interpPanel.add(doTrilinearRadio,gbc);
        gbc.gridy = 1;
        interpPanel.add(doBsplineRadio,gbc);
        gbc.gridy = 0;
        rescalePanel.add(doRescaleRadio,gbc);
        gbc.gridy = 1;
        rescalePanel.add(noRescaleRadio,gbc);
        gbc.gridy = 0;
        averagingOptionsPanel.add(ignoreBGRadio,gbc);
        gbc.gridy = 1;
        averagingOptionsPanel.add(includeBGRadio,gbc);
        
        
        optionsPanel.add(processPanel);
        optionsPanel.add(interpPanel);
        optionsPanel.add(rescalePanel);
        optionsPanel.add(averagingOptionsPanel);

        concatMatricesOnly = new JCheckBox("concat affine and green matrices only"); //used for debugging purposes only

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15,5,5,15);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(imageXLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(imageXFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(imageXBrowseButton,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(imageYLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(imageYFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(imageYBrowseButton,gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(transform1Label,gbc);
        gbc.gridx = 1;
        mainPanel.add(transform1FilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(transform1BrowseButton,gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(transform2Label,gbc);
        gbc.gridx = 1;
        mainPanel.add(transform2FilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(transform2BrowseButton,gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(transform3Label,gbc);
        gbc.gridx = 1;
        mainPanel.add(transform3FilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(transform3BrowseButton,gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;
        mainPanel.add(optionsPanel,gbc);
        //gbc.gridx = 0;
        //gbc.gridy = 6;
        //mainPanel.add(concatMatricesOnly,gbc);  //this is to just generate a concatenated matrix
        
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

        //hard coding for testing...1st dataset
        //imageX      
        /*currDir = "C:\\images\\nichd\\1\\N4A07-TM2-40XO-NA-13-12bit-080608-1x0.ics";
        FileIO fileIO = new FileIO();
        imageX = fileIO.readImage("N4A07-TM2-40XO-NA-13-12bit-080608-1x0.ics", "C:\\images\\nichd\\1" + File.separator, true, null);
        vjfX = new ViewJFrameImage(imageX);
        imageXFilePathTextField.setText(currDir);
        //imageY
        currDir = "C:\\images\\nichd\\1\\N4A07-TM2-40XO-NA-13-12bit-080608-1y0.ics";
        fileIO = new FileIO();
        imageY = fileIO.readImage("N4A07-TM2-40XO-NA-13-12bit-080608-1y0.ics", "C:\\images\\nichd\\1" + File.separator, true, null);
        vjfY = new ViewJFrameImage(imageY);
        imageYFilePathTextField.setText(currDir);
        //transform1
        currDir = "C:\\images\\nichd\\1\\N4A07-ix0-greenChannel_To_N4A07-iy0-greenChannel-9degrees-105-10-3.mtx";
    	transform1File = new File(currDir);
    	readTransform1(transform1File);
    	//transform2
    	currDir = "C:\\images\\nichd\\1\\N4A07-TM2-40XO-NA-13-12bit-080608-1x0Gray-afterTransformUsingGreenChannelTransformMatrix_To_N4A07-TM2-40XO-NA-13-12bit-080608-1y0Gray-12degrees-5-3-1.mtx";
    	transform2File = new File(currDir);
    	readTransform2(transform2File);*/
    	//end hard coding for testing

        //hard coding for testing...2nd dataset
        //imageX
        /*currDir = "C:\\images\\nichd\\2_withBlurring2\\N4A07-TM2-40XO-NA-13-12bit-080608-3x0.ics";
        FileIO fileIO = new FileIO();
        imageX = fileIO.readImage("N4A07-TM2-40XO-NA-13-12bit-080608-3x0.ics", "C:\\images\\nichd\\2_withBlurring2" + File.separator, true, null);
        new ViewJFrameImage(imageX);
        imageXFilePathTextField.setText(currDir);
        //imageY
        currDir = "C:\\images\\nichd\\2_withBlurring2\\N4A07-TM2-40XO-NA-13-12bit-080608-3y0.ics";
        fileIO = new FileIO();
        imageY = fileIO.readImage("N4A07-TM2-40XO-NA-13-12bit-080608-3y0.ics", "C:\\images\\nichd\\2_withBlurring2" + File.separator, true, null);
        new ViewJFrameImage(imageY);
        imageYFilePathTextField.setText(currDir);
        //transform1
        currDir = "C:\\images\\nichd\\2_withBlurring2\\GrayG_To_GrayG1-9degrees-105-10-3..mtx";
    	transform1File = new File(currDir);
    	readTransform1(transform1File);
    	//transform2
    	currDir = "C:\\images\\nichd\\2_withBlurring2\\N4A07-TM2-40XO-NA-13-12bit-080608-3x0Gray_transform_gblur_To_N4A07-TM2-40XO-NA-13-12bit-080608-3y0Gray_gblur-12-5-3-1.mtx";
    	transform2File = new File(currDir);
    	readTransform2(transform2File);
    	//transform3
    	currDir = "C:\\images\\nichd\\2_withBlurring2\\N4A07-TM2-40XO-NA-13-12bit-080608-3x0Gray_transform_transform_gblur.nlt";
    	transform3File = new File(currDir);
    	if(!transform3File.getName().endsWith(".nlt")) {
    		MipavUtil.displayError("A valid .nlt file is required");
    		return;
    	}
    	boolean success = readNLTFile(transform3File);
    	if(!success) {
    		MipavUtil.displayError("Error reading nlt file");
    		return;
    	}
    	transform3FilePathTextField.setText(currDir);*/
    	//end hard coding for testing
	}
	
	/**
	 * action performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if (command.equalsIgnoreCase("average")) { 
			ignoreBGRadio.setEnabled(true);
			includeBGRadio.setEnabled(true);
		}else if (command.equalsIgnoreCase("sqrRt")) { 
			ignoreBGRadio.setEnabled(true);
			includeBGRadio.setEnabled(true);
		}else if (command.equalsIgnoreCase("closestZ")) { 
			ignoreBGRadio.setEnabled(false);
			includeBGRadio.setEnabled(false);
		}else if (command.equalsIgnoreCase("imageXBrowse")) { 
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
	            imageX = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, true, null);
	            if(doRescaleRadio.isSelected()) {
	            	vjfX = new ViewJFrameImage(imageX);
	            }
	            imageXFilePathTextField.setText(currDir);
	        }
		 }else if(command.equalsIgnoreCase("imageYBrowse")) {
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
		            imageY = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, true, null);
		            destExtents =imageY.getExtents();
		            if(doRescaleRadio.isSelected()) {
		            	vjfY = new ViewJFrameImage(imageY);
		            }
		            imageYFilePathTextField.setText(currDir);
		        }
		 }else if(command.equalsIgnoreCase("transform1Browse")) {
			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose transformation 1");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	parentDir = chooser.getSelectedFile().getParent();
		        	transform1File = new File(currDir);
		        	readTransform1(transform1File);
		        }
		 }else if(command.equalsIgnoreCase("transform2Browse")) {
			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose transformation 2");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	transform2File = new File(currDir);
		        	readTransform2(transform2File);
		        }
		 }else if(command.equalsIgnoreCase("transform3Browse")) {
			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose transformation 3");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	transform3File = new File(currDir);
		        	if(!transform3File.getName().endsWith(".nlt")) {
		        		MipavUtil.displayError("A valid .nlt file is required");
		        		return;
		        	}
		        	boolean success = readNLTFile(transform3File);
		        	if(!success) {
		        		MipavUtil.displayError("Error reading nlt file");
		        		return;
		        	}
		        	transform3FilePathTextField.setText(currDir);
		        }
		 }
		 else if(command.equalsIgnoreCase("cancel")) {
			 if(imageX != null) {
		    	 imageX.disposeLocal();
		    	 imageX = null;
		     }
		     if(imageY != null) {
		    	 imageY.disposeLocal();
		    	 imageY = null;
		     }
		     if(vjfX != null) {
		    	 vjfX.close();
		     }
		     if(vjfY != null) {
		    	 vjfY.close();
		     }
			 dispose();
		 }else if(command.equalsIgnoreCase("ok")) {
			 setCursor(new Cursor(Cursor.WAIT_CURSOR));
			 //rescale imageX intensity to imageY based on VOI
			 if(doRescaleRadio.isSelected()) {
				 VOIVector VOIsX = imageX.getVOIs();
			     int nVOIX = VOIsX.size();
				 if(nVOIX != 1) {
					 MipavUtil.displayError("Both images must contain one VOI");
					 setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		        	 return;
				 }
				 VOIVector VOIsY = imageY.getVOIs();
			     int nVOIY = VOIsY.size();
				 if(nVOIY != 1) {
					 MipavUtil.displayError("Both images must contain one VOI");
					 setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		        	 return;
				 }
				 VOI VOIX = VOIsX.VOIAt(0);
				 VOIX.setAllActive(true);
		         algoVOIProps = new AlgorithmVOIProps(imageX, AlgorithmVOIProps.PROCESS_PER_VOI, JDialogVOIStatistics.NO_RANGE, getActiveVOIs(imageX));
		         algoVOIProps.run();
		         minR_X = algoVOIProps.getMinIntensityRed();
		         maxR_X = algoVOIProps.getMaxIntensityRed();
		            
		         minG_X = algoVOIProps.getMinIntensityGreen();
		         maxG_X = algoVOIProps.getMaxIntensityGreen();
		         
		         minB_X = algoVOIProps.getMinIntensityBlue();
		         maxB_X = algoVOIProps.getMaxIntensityBlue();
		         
		         System.out.println("imageX VOI values: minR maxR minG maxG minB maxB :" + minR_X + " " + maxR_X + " " + minG_X + " " + maxG_X + " " + minB_X + " " + maxB_X);
		         
		         algoVOIProps.finalize();
		         algoVOIProps = null;
				 VOI VOIY = VOIsY.VOIAt(0);
				 VOIY.setAllActive(true);
		         algoVOIProps = new AlgorithmVOIProps(imageY, AlgorithmVOIProps.PROCESS_PER_VOI, JDialogVOIStatistics.NO_RANGE, getActiveVOIs(imageY));
		         algoVOIProps.run();
		         minR_Y = algoVOIProps.getMinIntensityRed();
		         maxR_Y = algoVOIProps.getMaxIntensityRed();
		            
		         minG_Y = algoVOIProps.getMinIntensityGreen();
		         maxG_Y = algoVOIProps.getMaxIntensityGreen();
		         
		         minB_Y = algoVOIProps.getMinIntensityBlue();
		         maxB_Y = algoVOIProps.getMaxIntensityBlue();
		         
		         System.out.println("imageY VOI values: minR maxR minG maxG minB maxB :" + minR_Y + " " + maxR_Y + " " + minG_Y + " " + maxG_Y + " " + minB_Y + " " + maxB_Y);
		         
		         algoVOIProps.finalize();
		         algoVOIProps = null;
		         
		         vjfX.setVisible(false);
		         vjfY.setVisible(false);
		         
		         VOIsX.clear();
		         VOIsY.clear();
		         
		         //calculate slope and b-intercept for voi-window
		         slopeR = calculateSlope(minR_Y,minR_X,maxR_Y,maxR_X);
		         bR = calculateB(minR_Y,minR_X,slopeR);
		         System.out.println("slopeR = " + slopeR + " , bR = " + bR);
		         
		         slopeG = calculateSlope(minG_Y,minG_X,maxG_Y,maxG_X);
		         bG = calculateB(minG_Y,minG_X,slopeG);
		         System.out.println("slopeG = " + slopeG + " , bG = " + bG);
		         
		         slopeB = calculateSlope(minB_Y,minB_X,maxB_Y,maxB_X);
		         bB = calculateB(minB_Y,minB_X,slopeB);
		         System.out.println("slopeB = " + slopeB + " , bB = " + bB);
		         
		         //now we go through imageX and rescale
		         int length = imageX.getExtents()[0] * imageX.getExtents()[1] * imageX.getExtents()[2] * 4;
		         float[] buffer = new float[length];
		        
		         try {
		        	 imageX.exportData(0, length, buffer);
		         } catch (IOException error) {
		             System.out.println("IO exception");
		             setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		             return;
		         }
		         float red,green,blue;
		         float newRed, newGreen, newBlue;
		         
		         for(int i=0;i<buffer.length;i=i+4) {
		        	 red = buffer[i+1];
		        	 if(slopeR == 0 && bR == 0) {
		        		 newRed = red;
		        	 }else {
			        	 newRed = getNewValue(red,slopeR,bR);
			        	 if(newRed < 0) {
			        		 newRed = 0;
			        	 }else if(newRed > 255) {
			        		 newRed = 255;
			        	 }
		        	 }
		        	 buffer[i+1] = newRed;
		        	 
		        	 green = buffer[i+2];
		        	 if(slopeG == 0 && bG == 0) {
		        		 newGreen = green;
		        	 }else {
			        	 newGreen = getNewValue(green,slopeG,bG);
			        	 if(newGreen < 0) {
			        		 newGreen = 0;
			        	 }else if(newGreen > 255) {
			        		 newGreen = 255;
			        	 }
		        	 }
		        	 buffer[i+2] = newGreen;
		        	 
		        	 blue = buffer[i+3];
		        	 if(slopeB == 0 && bB == 0) {
		        		 newBlue = blue;
		        	 }else {
			        	 newBlue = getNewValue(blue,slopeB,bB);
			        	 if(newBlue < 0) {
			        		 newBlue = 0;
			        	 }else if(newBlue > 255) {
			        		 newBlue = 255;
			        	 }
		        	 }
		        	 buffer[i+3] = newBlue;           
		         }
		         
		         try {
		             imageX.importData(0, buffer, true);
		         } catch (IOException error) {
		             System.out.println("IO exception");
		             setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		             return;
		         }
		         imageX.calcMinMax();
			 }//done rescaling

			 //if(doAverageRadio.isSelected()) {
				 //doAverage = true;
			 //}else {
				 //doAverage = false;
			 //}

			 TransMatrix intermMatrix1 = new TransMatrix(4);
			 intermMatrix1.Mult(matrixAffine, matrixGreen); //pretty sure this is correct
			 
			 /*if(concatMatricesOnly.isSelected()) {
				 try {
			            File concatFile = new File(parentDir + File.separator + "concatOfGreenAndAff.mtx");
			            FileOutputStream outputStream = new FileOutputStream(concatFile);
			            PrintStream printStream = new PrintStream(outputStream);
			            printStream.println("4");
			            printStream.println("4");
			            printStream.println(intermMatrix1.matrixToString(10, 4));
			            outputStream.close();
			            
				 }catch(Exception ex) {
					 ex.printStackTrace();
				 }
				 return;
			 }*/
			 
			 intermMatrix1.Inverse();

			 int[] extents = {512,512,512};
			 resultImage = new ModelImage(ModelImage.ARGB, extents,"resultImage");
			 float[] resultImageResols = new float[3];
			 resultImageResols[0] = imageY.getResolutions(0)[0];
			 resultImageResols[1] = imageY.getResolutions(0)[1];
			 resultImageResols[2] = imageY.getResolutions(0)[2]*imageY.getExtents()[2]/512;
			 for(int i=0;i<resultImage.getExtents()[2];i++) {
				 resultImage.setResolutions(i, resultImageResols);
			 }
			 byte[] resultBuffer = new byte[512*512*512*4];
			 int index = 0; //index into resultBuffer
			 double[] tPt1 = new double[3];
			 double[] tPt2 = new double[3];
			 
			 byte[] imageXBuffer;
	         int length1 = imageX.getExtents()[0] * imageX.getExtents()[1] * imageX.getExtents()[2] * 4;
	         imageXBuffer = new byte[length1];
	         try {
	        	 imageX.exportData(0, length1, imageXBuffer);
	         } catch (IOException error) {
	             System.out.println("IO exception");
	             setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	             return;
	         }
	         
	         //b-spline stuff in case b-spline interp was selected
	         imageX_R_coeff = new float[imageX.getExtents()[0]][imageX.getExtents()[1]][imageX.getExtents()[2]];
	         imageX_G_coeff = new float[imageX.getExtents()[0]][imageX.getExtents()[1]][imageX.getExtents()[2]];
	         imageX_B_coeff = new float[imageX.getExtents()[0]][imageX.getExtents()[1]][imageX.getExtents()[2]];
	         for (int c = 0; c < 4; c++) {
	             for (int z = 0; z < imageX.getExtents()[2]; z++) {
	                 for (int y = 0; y < imageX.getExtents()[1]; y++) {
	                     for (int x = 0; x < imageX.getExtents()[0]; x++) {
	                    	 if(c==1) {
	                    		 imageX_R_coeff[x][y][z] = (float)(imageXBuffer[ (4 * (x + (imageX.getExtents()[0] * y) + (imageX.getExtents()[0] * imageX.getExtents()[1] * z))) + c] & 0xff);
	                    	 }else if(c==2) {
	                    		 imageX_G_coeff[x][y][z] = (float)(imageXBuffer[ (4 * (x + (imageX.getExtents()[0] * y) + (imageX.getExtents()[0] * imageX.getExtents()[1] * z))) + c] & 0xff);
	                    	 }else if(c==3) {
	                    		 imageX_B_coeff[x][y][z] = (float)(imageXBuffer[ (4 * (x + (imageX.getExtents()[0] * y) + (imageX.getExtents()[0] * imageX.getExtents()[1] * z))) + c] & 0xff);
	                    	 }
	                         
	                     }
	                 }
	             }
	         }
	         BSplineProcessing splineAlgX_R;
	         splineAlgX_R = new BSplineProcessing();
	         splineAlgX_R.samplesToCoefficients(imageX_R_coeff, imageX.getExtents()[0], imageX.getExtents()[1], imageX.getExtents()[2], 3);
	         
	         BSplineProcessing splineAlgX_G;
	         splineAlgX_G = new BSplineProcessing();
	         splineAlgX_G.samplesToCoefficients(imageX_G_coeff, imageX.getExtents()[0], imageX.getExtents()[1], imageX.getExtents()[2], 3);
	         
	         BSplineProcessing splineAlgX_B;
	         splineAlgX_B = new BSplineProcessing();
	         splineAlgX_B.samplesToCoefficients(imageX_B_coeff, imageX.getExtents()[0], imageX.getExtents()[1], imageX.getExtents()[2], 3);
	         
	         
	         byte[] imageYBuffer;
	         int length2 = imageY.getExtents()[0] * imageY.getExtents()[1] * imageY.getExtents()[2] * 4;
	         imageYBuffer = new byte[length2];
	         try {
	        	 imageY.exportData(0, length2, imageYBuffer);
	         } catch (IOException error) {
	        	 setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	             System.out.println("IO exception");
	             return;
	         }
	         
	         imageY_R_coeff = new float[imageY.getExtents()[0]][imageY.getExtents()[1]][imageY.getExtents()[2]];
	         imageY_G_coeff = new float[imageY.getExtents()[0]][imageY.getExtents()[1]][imageY.getExtents()[2]];
	         imageY_B_coeff = new float[imageY.getExtents()[0]][imageY.getExtents()[1]][imageY.getExtents()[2]];
	         for (int c = 0; c < 4; c++) {
	             for (int z = 0; z < imageY.getExtents()[2]; z++) {
	                 for (int y = 0; y < imageY.getExtents()[1]; y++) {
	                     for (int x = 0; x < imageY.getExtents()[0]; x++) {
	                    	 if(c==1) {
	                    		 imageY_R_coeff[x][y][z] = (float)(imageYBuffer[ (4 * (x + (imageY.getExtents()[0] * y) + (imageY.getExtents()[0] * imageY.getExtents()[1] * z))) + c] & 0xff);
	                    		 
	                    	 }else if(c==2) {
	                    		 imageY_G_coeff[x][y][z] = (float)(imageYBuffer[ (4 * (x + (imageY.getExtents()[0] * y) + (imageY.getExtents()[0] * imageY.getExtents()[1] * z))) + c] & 0xff);
	                    	 }else if(c==3) {
	                    		 imageY_B_coeff[x][y][z] = (float)(imageYBuffer[ (4 * (x + (imageY.getExtents()[0] * y) + (imageY.getExtents()[0] * imageY.getExtents()[1] * z))) + c] & 0xff);
	                    	 }
	                         
	                     }
	                 }
	             }
	         }
	         BSplineProcessing splineAlgY_R;
	         splineAlgY_R = new BSplineProcessing();
	         splineAlgY_R.samplesToCoefficients(imageY_R_coeff, imageY.getExtents()[0], imageY.getExtents()[1], imageY.getExtents()[2], 3);
	         
	         BSplineProcessing splineAlgY_G;
	         splineAlgY_G = new BSplineProcessing();
	         splineAlgY_G.samplesToCoefficients(imageY_G_coeff, imageY.getExtents()[0], imageY.getExtents()[1], imageY.getExtents()[2], 3);
	         
	         BSplineProcessing splineAlgY_B;
	         splineAlgY_B = new BSplineProcessing();
	         splineAlgY_B.samplesToCoefficients(imageY_B_coeff, imageY.getExtents()[0], imageY.getExtents()[1], imageY.getExtents()[2], 3);
	         
	         
	         //following is if nlt file is inputted also
	         ModelSimpleImage[] akSimpleImageSourceMap = null;
	         if(!transform3FilePathTextField.getText().trim().equals("")){
	        	 	//create the non-linear image-maps
			        m_kBSplineBasisX = new BSplineBasisDiscretef(numControlPoints, splineDegree, destExtents[0]);
			        m_kBSplineBasisY = new BSplineBasisDiscretef(numControlPoints, splineDegree, destExtents[1]);
			        m_kBSplineBasisZ = new BSplineBasisDiscretef(numControlPoints, splineDegree, destExtents[2]);
			        m_kBSpline3D = new BSplineLattice3Df(m_kBSplineBasisX, m_kBSplineBasisY, m_kBSplineBasisZ);

			        Vector3f kPoint = new Vector3f();
			
			        int ind = 0;
			
			        for (int iControlX = 0; iControlX < numControlPoints; iControlX++) {
			        		System.out.println("iControlX = " + iControlX);
			            for (int iControlY = 0; iControlY < numControlPoints; iControlY++) {
			                for (int iControlZ = 0; iControlZ < numControlPoints; iControlZ++) {
			                    kPoint.X = controlMat[ind][0];
			                    kPoint.Y = controlMat[ind][1];
			                    kPoint.Z = controlMat[ind++][2];
			                    m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kPoint);
			                }
			            }
			        }
			
			        akSimpleImageSourceMap = m_kBSpline3D.createImageMap(destExtents[0], destExtents[1], destExtents[2]);
	         }

	         
	         
	         //okay....now....
			 float xmm,ymm,zmm;
			 byte[] rgb1 = new byte[3];
			 byte[] rgb2 = new byte[3];
			 float r1_float, g1_float, b1_float, r2_float, g2_float, b2_float;
			 short[] rgb1_short = new short[3];
			 short[] rgb2_short = new short[3];
			 //loop through each point in result image
			 for(int z=0;z<512;z++) {
				 	System.out.println("z is " +  z);
				 for(int y=0;y<512;y++) {
					 for(int x=0;x<512;x++) {
						 //first transform the point back to both spaces...results in tPt1 and tPt2
						 if(!transform3FilePathTextField.getText().trim().equals("")){ //if nlt file is inputted
							 xmm = x * resultImage.getResolutions(0)[0];
							 ymm = y * resultImage.getResolutions(0)[1];
							 zmm = z * resultImage.getResolutions(0)[2];
							 
							 tPt1[0] = MipavMath.round(xmm /imageX.getResolutions(0)[0]);
							 tPt1[1] = MipavMath.round(ymm /imageX.getResolutions(0)[1]);
							 tPt1[2] = MipavMath.round(zmm /imageX.getResolutions(0)[2]);
							 
							 tPt2[0] = xmm /imageY.getResolutions(0)[0];
							 tPt2[1] = ymm /imageY.getResolutions(0)[1];
							 tPt2[2] = zmm /imageY.getResolutions(0)[2];

					        int iIndexTrg = (int)tPt1[0] + ((int)tPt1[1] * destExtents[0]) + ((int)tPt1[2] * destExtents[0]*destExtents[1]);
					        
					        float xMapPt =  0.0f;
							float yMapPt =  0.0f;
							float zMapPt =  0.0f;
							
					        if (iIndexTrg < akSimpleImageSourceMap[0].data.length){					
						        xMapPt = (destExtents[0] - 1) * akSimpleImageSourceMap[0].data[iIndexTrg];
								yMapPt = (destExtents[1] - 1) * akSimpleImageSourceMap[1].data[iIndexTrg];
								zMapPt = (destExtents[2] - 1) * akSimpleImageSourceMap[2].data[iIndexTrg];
					        }

							xmm = xMapPt * imageY.getResolutions(0)[0];
							ymm = yMapPt * imageY.getResolutions(0)[1];
							zmm = zMapPt * imageY.getResolutions(0)[2];
							
							intermMatrix1.transform(xmm, ymm, zmm, tPt1);
							
							tPt1[0] = tPt1[0]/imageX.getResolutions(0)[0];
							tPt1[1] = tPt1[1]/imageX.getResolutions(0)[1];
							tPt1[2] = tPt1[2]/imageX.getResolutions(0)[2];
						 }else{ //if nlt file is NOT inputted
							 xmm = x * resultImage.getResolutions(0)[0];
							 ymm = y * resultImage.getResolutions(0)[1];
							 zmm = z * resultImage.getResolutions(0)[2];
					 
							 intermMatrix1.transform(xmm, ymm, zmm, tPt1);
							 
							 tPt1[0] = tPt1[0]/imageX.getResolutions(0)[0];
							 tPt1[1] = tPt1[1]/imageX.getResolutions(0)[1];
							 tPt1[2] = tPt1[2]/imageX.getResolutions(0)[2];

							 xmm = x * resultImage.getResolutions(0)[0];
							 ymm = y * resultImage.getResolutions(0)[1];
							 zmm = z * resultImage.getResolutions(0)[2];

							 tPt2[0] = xmm /imageY.getResolutions(0)[0];
							 tPt2[1] = ymm /imageY.getResolutions(0)[1];
							 tPt2[2] = zmm /imageY.getResolutions(0)[2];
							 
							 
							 //TESTING....9/21/2009
							 /*int xInt1 = (int)Math.round(tPt1[0]);
							 int yInt1 = (int)Math.round(tPt1[1]);
							 int zInt1 = (int)Math.round(tPt1[2]);
							 try {
								 imageX.setC(xInt1, yInt1, zInt1, 3, 200);
							 }catch(Exception ex) {
								 
							 }
							 
							 int xInt2 = (int)Math.round(tPt2[0]);
							 int yInt2 = (int)Math.round(tPt2[1]);
							 int zInt2 = (int)Math.round(tPt2[2]);
							 try {
								 imageY.setC(xInt2, yInt2, zInt2, 3, 200);
							 }catch(Exception ex) {
								 
							 }*/
							 
							 
						 }
						 //Now either do averaging or closest-Z
						 int floorPointIndex1=0, floorPointIndex2=0;
						 if(doAverageRadio.isSelected() || doSqRtRadio.isSelected()) {
							//get linear interpolated values from both transformed points
							 if(tPt1[0] < 0 || tPt1[1] < 0 || tPt1[2] < 0 || tPt1[0] > imageX.getExtents()[0]-1 || tPt1[1] > imageX.getExtents()[1]-1 || tPt1[2] > imageX.getExtents()[2]-1) {
								 rgb1_short[0] = 0;
								 rgb1_short[1] = 0;
								 rgb1_short[2] = 0;
								 
							 }else {
								 double tX1_floor = Math.floor(tPt1[0]);
								 double tY1_floor = Math.floor(tPt1[1]);
								 double tZ1_floor = Math.floor(tPt1[2]);
								 float dx1 = (float)(tPt1[0] - tX1_floor);
								 float dy1 = (float)(tPt1[1] - tY1_floor);
								 float dz1 = (float)(tPt1[2] - tZ1_floor);
								 int[] extents1 = imageX.getExtents();
								 floorPointIndex1 = (int)(((tZ1_floor * (extents1[0] * extents1[1])) + (tY1_floor * extents1[0]) + tX1_floor) * 4);
								 if(floorPointIndex1 < imageXBuffer.length) {
									 if(doTrilinearRadio.isSelected()) {
										 rgb1 = AlgorithmConvolver.getTrilinearC(floorPointIndex1, dx1, dy1, dz1, extents1, imageXBuffer);
										 rgb1_short[0] = (short)(rgb1[0] & 0xff);
										 rgb1_short[1] = (short)(rgb1[1] & 0xff);
										 rgb1_short[2] = (short)(rgb1[2] & 0xff);
									 }else {
										 r1_float = splineAlgX_R.interpolatedValue(imageX_R_coeff, tX1_floor, tY1_floor, tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
									        if (r1_float > 255) {
									        	r1_float = 255;
									        } else if (r1_float < 0) {
									        	r1_float = 0;
									        }
									        
									        g1_float = splineAlgX_G.interpolatedValue(imageX_G_coeff, tX1_floor, tY1_floor, tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
									        if (g1_float > 255) {
									        	g1_float = 255;
									        } else if (g1_float < 0) {
									        	g1_float = 0;
									        }
									        
									        b1_float = splineAlgX_B.interpolatedValue(imageX_B_coeff, tX1_floor, tY1_floor, tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
									        if (b1_float > 255) {
									        	b1_float = 255;
									        } else if (b1_float < 0) {
									        	b1_float = 0;
									        }
									        
									        rgb1_short[0] = (short)(r1_float);
											rgb1_short[1] = (short)(g1_float);
											rgb1_short[2] = (short)(b1_float);
									 }
								 }else{
									 rgb1_short[0] = 0;
									 rgb1_short[1] = 0;
									 rgb1_short[2] = 0;
								 }
							 }
							
							 if(tPt2[0] < 0 || tPt2[1] < 0 || tPt2[2] < 0 || tPt2[0] > imageY.getExtents()[0]-1 || tPt2[1] > imageY.getExtents()[1]-1 || tPt2[2] > imageY.getExtents()[2]-1) {
								 rgb2_short[0] = 0;
								 rgb2_short[1] = 0;
								 rgb2_short[2] = 0;
							 }else {
								 double tX2_floor = Math.floor(tPt2[0]);
								 double tY2_floor = Math.floor(tPt2[1]);
								 double tZ2_floor = Math.floor(tPt2[2]);
								 float dx2 = (float)(tPt2[0] - tX2_floor);
								 float dy2 = (float)(tPt2[1] - tY2_floor);
								 float dz2 = (float)(tPt2[2] - tZ2_floor);
								 int[] extents2 = imageY.getExtents();
								 floorPointIndex2 = (int)(((tZ2_floor * (extents2[0] * extents2[1])) + (tY2_floor * extents2[0]) + tX2_floor) * 4);
								 if(floorPointIndex2 < imageYBuffer.length) {
									 if(doTrilinearRadio.isSelected()) {
										 rgb2 = AlgorithmConvolver.getTrilinearC(floorPointIndex2, dx2, dy2, dz2, extents2, imageYBuffer); 
										 rgb2_short[0] = (short)(rgb2[0] & 0xff);
										 rgb2_short[1] = (short)(rgb2[1] & 0xff);
										 rgb2_short[2] = (short)(rgb2[2] & 0xff);
									 }else {
										r2_float = splineAlgY_R.interpolatedValue(imageY_R_coeff, tX2_floor, tY2_floor, tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
								        if (r2_float > 255) {
								        	r2_float = 255;
								        } else if (r2_float < 0) {
								        	r2_float = 0;
								        }
								        
								        g2_float = splineAlgY_G.interpolatedValue(imageY_G_coeff, tX2_floor, tY2_floor, tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
								        if (g2_float > 255) {
								        	g2_float = 255;
								        } else if (g2_float < 0) {
								        	g2_float = 0;
								        }
								        
								        b2_float = splineAlgY_B.interpolatedValue(imageY_B_coeff, tX2_floor, tY2_floor, tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
								        if (b2_float > 255) {
								        	b2_float = 255;
								        } else if (b2_float < 0) {
								        	b2_float = 0;
								        }
								        
								        rgb2_short[0] = (short)(r2_float);
										rgb2_short[1] = (short)(g2_float);
										rgb2_short[2] = (short)(b2_float);
									 }
								 }else {
									 rgb2_short[0] = 0;
									 rgb2_short[1] = 0;
									 rgb2_short[2] = 0;
								 }
							 }

							 byte avgR, avgG, avgB;
							 
							 if(doAverageRadio.isSelected()) {
								 //dont do combining if other point is all background
								 if(ignoreBGRadio.isSelected()) {
									 if(rgb1_short[0] == 0 && rgb1_short[1] == 0 && rgb1_short[2] == 0) {
										 avgR = (byte)rgb2_short[0];
										 avgG = (byte)rgb2_short[1];
										 avgB = (byte)rgb2_short[2];
									 }else if (rgb2_short[0] == 0 && rgb2_short[1] == 0 && rgb2_short[2] == 0) {
										 avgR = (byte)rgb1_short[0];
										 avgG = (byte)rgb1_short[1];
										 avgB = (byte)rgb1_short[2];
									 }else {
										 //averaging
										 avgR = (byte)Math.round(((rgb1_short[0] + rgb2_short[0])/2.0f));
										 avgG = (byte)Math.round(((rgb1_short[1] + rgb2_short[1])/2.0f));
										 avgB = (byte)Math.round(((rgb1_short[2] + rgb2_short[2])/2.0f));
									 }
								 }else {
									//averaging
									 avgR = (byte)Math.round(((rgb1_short[0] + rgb2_short[0])/2.0f));
									 avgG = (byte)Math.round(((rgb1_short[1] + rgb2_short[1])/2.0f));
									 avgB = (byte)Math.round(((rgb1_short[2] + rgb2_short[2])/2.0f));
								 }
							 }else {
								//dont do combining if other point is all background
								 if(ignoreBGRadio.isSelected()) {
									 if(rgb1_short[0] == 0 && rgb1_short[1] == 0 && rgb1_short[2] == 0) {
										 avgR = (byte)rgb2_short[0];
										 avgG = (byte)rgb2_short[1];
										 avgB = (byte)rgb2_short[2];
									 }else if (rgb2_short[0] == 0 && rgb2_short[1] == 0 && rgb2_short[2] == 0) {
										 avgR = (byte)rgb1_short[0];
										 avgG = (byte)rgb1_short[1];
										 avgB = (byte)rgb1_short[2];
									 }else {
										//doing Sqrt (Intensity X * Intensity Y)
										 avgR = (byte)Math.sqrt(rgb1_short[0] * rgb2_short[0]);
										 avgG = (byte)Math.sqrt(rgb1_short[1] * rgb2_short[1]);
										 avgB = (byte)Math.sqrt(rgb1_short[2] * rgb2_short[2]); 
									 }
								 }else {
									//doing Sqrt (Intensity X * Intensity Y)
									 avgR = (byte)Math.sqrt(rgb1_short[0] * rgb2_short[0]);
									 avgG = (byte)Math.sqrt(rgb1_short[1] * rgb2_short[1]);
									 avgB = (byte)Math.sqrt(rgb1_short[2] * rgb2_short[2]); 
								 }
							 }
							 


							 //alpha
							 resultBuffer[index] = (byte)1;
							 //r
							 index = index + 1;
							 resultBuffer[index] = avgR;
							 //g
							 index = index + 1;
							 resultBuffer[index] = avgG;
							 //b
							 index = index + 1;
							 resultBuffer[index] = avgB;
							 index = index + 1;
						 }else { //CLOSEST Z
							 //look at z transformed points
							 double diff1,diff2;
							 
							 if(tPt1[2] - Math.floor(tPt1[2]) <= .5) {
								 diff1 = tPt1[2] - Math.floor(tPt1[2]);
							 }else {
								 diff1 = Math.ceil(tPt1[2]) - tPt1[2];
							 }
							 
							 if(tPt2[2] - Math.floor(tPt2[2]) <= .5) {
								 diff2 = tPt2[2] - Math.floor(tPt2[2]);
							 }else {
								 diff2 = Math.ceil(tPt2[2]) - tPt2[2];
							 }

							 diff1 = diff1 * imageX.getResolutions(0)[2];
							 diff2 = diff2 * imageY.getResolutions(0)[2];

							//get linear interpolated values from both transformed points
							 if(tPt1[0] < 0 || tPt1[1] < 0 || tPt1[2] < 0 || tPt1[0] > imageX.getExtents()[0]-1 || tPt1[1] > imageX.getExtents()[1]-1 || tPt1[2] > imageX.getExtents()[2]-1) {
								 rgb1_short[0] = 0;
								 rgb1_short[1] = 0;
								 rgb1_short[2] = 0;
								 
							 }else {
								 double tX1_floor = Math.floor(tPt1[0]);
								 double tY1_floor = Math.floor(tPt1[1]);
								 double tZ1_floor = Math.floor(tPt1[2]);
								 float dx1 = (float)(tPt1[0] - tX1_floor);
								 float dy1 = (float)(tPt1[1] - tY1_floor);
								 float dz1 = (float)(tPt1[2] - tZ1_floor);
								 int[] extents1 = imageX.getExtents();
								 floorPointIndex1 = (int)(((tZ1_floor * (extents1[0] * extents1[1])) + (tY1_floor * extents1[0]) + tX1_floor) * 4);
								 if(doTrilinearRadio.isSelected()) {
									 rgb1 = AlgorithmConvolver.getTrilinearC(floorPointIndex1, dx1, dy1, dz1, extents1, imageXBuffer);
									 rgb1_short[0] = (short)(rgb1[0] & 0xff);
									 rgb1_short[1] = (short)(rgb1[1] & 0xff);
									 rgb1_short[2] = (short)(rgb1[2] & 0xff);
								 }else {
									r1_float = splineAlgX_R.interpolatedValue(imageX_R_coeff, tX1_floor, tY1_floor, tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
							        if (r1_float > 255) {
							        	r1_float = 255;
							        } else if (r1_float < 0) {
							        	r1_float = 0;
							        }
							        
							        g1_float = splineAlgX_G.interpolatedValue(imageX_G_coeff, tX1_floor, tY1_floor, tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
							        if (g1_float > 255) {
							        	g1_float = 255;
							        } else if (g1_float < 0) {
							        	g1_float = 0;
							        }
							        
							        b1_float = splineAlgX_B.interpolatedValue(imageX_B_coeff, tX1_floor, tY1_floor, tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
							        if (b1_float > 255) {
							        	b1_float = 255;
							        } else if (b1_float < 0) {
							        	b1_float = 0;
							        }
							        
							        rgb1_short[0] = (short)(r1_float);
									rgb1_short[1] = (short)(g1_float);
									rgb1_short[2] = (short)(b1_float);
								 } 
							 }
							
							 
							 if(tPt2[0] < 0 || tPt2[1] < 0 || tPt2[2] < 0 || tPt2[0] > imageY.getExtents()[0]-1 || tPt2[1] > imageY.getExtents()[1]-1 || tPt2[2] > imageY.getExtents()[2]-1) {
								 rgb2_short[0] = 0;
								 rgb2_short[1] = 0;
								 rgb2_short[2] = 0;
							 }else {
								 double tX2_floor = Math.floor(tPt2[0]);
								 double tY2_floor = Math.floor(tPt2[1]);
								 double tZ2_floor = Math.floor(tPt2[2]);
								 float dx2 = (float)(tPt2[0] - tX2_floor);
								 float dy2 = (float)(tPt2[1] - tY2_floor);
								 float dz2 = (float)(tPt2[2] - tZ2_floor);
								 int[] extents2 = imageY.getExtents();
								 floorPointIndex2 = (int)(((tZ2_floor * (extents2[0] * extents2[1])) + (tY2_floor * extents2[0]) + tX2_floor) * 4);
								 if(doTrilinearRadio.isSelected()) {
									 rgb2 = AlgorithmConvolver.getTrilinearC(floorPointIndex2, dx2, dy2, dz2, extents2, imageYBuffer); 
									 rgb2_short[0] = (short)(rgb2[0] & 0xff);
									 rgb2_short[1] = (short)(rgb2[1] & 0xff);
									 rgb2_short[2] = (short)(rgb2[2] & 0xff);
								 }else {
									r2_float = splineAlgY_R.interpolatedValue(imageY_R_coeff, tX2_floor, tY2_floor, tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
							        if (r2_float > 255) {
							        	r2_float = 255;
							        } else if (r2_float < 0) {
							        	r2_float = 0;
							        }
							        
							        g2_float = splineAlgY_G.interpolatedValue(imageY_G_coeff, tX2_floor, tY2_floor, tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
							        if (g2_float > 255) {
							        	g2_float = 255;
							        } else if (g2_float < 0) {
							        	g2_float = 0;
							        }
							        
							        b2_float = splineAlgY_B.interpolatedValue(imageY_B_coeff, tX2_floor, tY2_floor, tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
							        if (b2_float > 255) {
							        	b2_float = 255;
							        } else if (b2_float < 0) {
							        	b2_float = 0;
							        }
							        
							        rgb2_short[0] = (short)(r2_float);
									rgb2_short[1] = (short)(g2_float);
									rgb2_short[2] = (short)(b2_float);
								 }	 
							 }

							 
							 byte r,g,b;
							 //float r1,g1,b1;
							 //float r2,g2,b2;

							if(diff1 < diff2) {
								//r1 = ((1-(float)diff1) * rgb1_short[0]) + ((float)diff1 * rgb2_short[0]);
								//r2 = ((float)diff2 * rgb1_short[0]) + ((1 - (float)diff2) * rgb2_short[0]);
								
								//g1 = ((1-(float)diff1) * rgb1_short[1]) + ((float)diff1 * rgb2_short[1]);
								//g2 = ((float)diff2 * rgb1_short[1]) + ((1 - (float)diff2) * rgb2_short[1]);
								
								//b1 = ((1-(float)diff1) * rgb1_short[2]) + ((float)diff1 * rgb2_short[2]);
								//b2 = ((float)diff2 * rgb1_short[2]) + ((1 - (float)diff2) * rgb2_short[2]);
								
								//r = (byte)(r1);
								//g = (byte)(g1);
								//b = (byte)(b1);
								
								//r = (byte)((r1+r2)/2);
								//g = (byte)((g1+g2)/2);
								//b = (byte)((b1+b2)/2);
								
								r = (byte)rgb1_short[0];
								g = (byte)rgb1_short[1];
								b = (byte)rgb1_short[2]; 
							}else {
								//r1 = ((1-(float)diff2) * rgb2_short[0]) + ((float)diff2 * rgb1_short[0]);
								//r2 = ((float)diff1 * rgb2_short[0]) + ((1 - (float)diff1) * rgb1_short[0]);
								
								//g1 = ((1-(float)diff2) * rgb2_short[1]) + ((float)diff2 * rgb1_short[1]);
								//g2 = ((float)diff1 * rgb2_short[1]) + ((1 - (float)diff1) * rgb1_short[1]);
								
								//b1 = ((1-(float)diff2) * rgb2_short[2]) + ((float)diff2 * rgb1_short[2]);
								//b2 = ((float)diff1 * rgb2_short[2]) + ((1 - (float)diff1) * rgb1_short[2]);
								
								//r = (byte)(r1);
								//g = (byte)(g1);
								//b = (byte)(b1);
								
								//r = (byte)((r1+r2)/2);
								//g = (byte)((g1+g2)/2);
								//b = (byte)((b1+b2)/2);

								r = (byte)rgb2_short[0];
								g = (byte)rgb2_short[1];
								b = (byte)rgb2_short[2];
							}

							 //alpha
							 resultBuffer[index] = (byte)255;
							 //r
							 index = index + 1;
							 resultBuffer[index] = r;
							 //g
							 index = index + 1;
							 resultBuffer[index] = g;
							 //b
							 index = index + 1;
							 resultBuffer[index] = b;
							 index = index + 1;
						 } 
					 }
				 }
			 }

		    try {
		    	resultImage.importData(0, resultBuffer, true);
	        } catch (IOException error) {
	            System.out.println("IO exception");
	            error.printStackTrace();
	            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	            return;
	        }

	        
	         //resultImageResols[2] = imageX.getResolutions(0)[0];
			 //for(int i=0;i<resultImage.getExtents()[2];i++) {
			 //	 resultImage.setResolutions(i, resultImageResols);
			 //}
			 FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[resultImage.getExtents()[2]];
			 for (int i = 0; i < fileInfoBases.length; i++) {
		            fileInfoBases[i] = new FileInfoImageXML(resultImage.getImageName(), null, FileUtility.XML);
		            fileInfoBases[i].setEndianess(imageX.getFileInfo()[0].getEndianess());
		            fileInfoBases[i].setUnitsOfMeasure(imageX.getFileInfo()[0].getUnitsOfMeasure());
		            fileInfoBases[i].setResolutions(resultImageResols);
		            fileInfoBases[i].setExtents(resultImage.getExtents());
		            fileInfoBases[i].setImageOrientation(imageX.getFileInfo()[0].getImageOrientation());
		            fileInfoBases[i].setAxisOrientation(imageX.getFileInfo()[0].getAxisOrientation());
		            fileInfoBases[i].setOrigin(imageX.getFileInfo()[0].getOrigin());
		            fileInfoBases[i].setPixelPadValue(imageX.getFileInfo()[0].getPixelPadValue());
		            fileInfoBases[i].setPhotometric(imageX.getFileInfo()[0].getPhotometric());
		            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);
		            fileInfoBases[i].setFileDirectory(imageX.getFileInfo()[0].getFileDirectory());
		     }
			 
			 resultImage.setFileInfo(fileInfoBases);
		     resultImage.calcMinMax();
		     
		     //write out input params to file
		     writeInputParams();
		     
		     new ViewJFrameImage(resultImage);
		     if(imageX != null) {
		    	 imageX.disposeLocal();
		    	 imageX = null;
		     }
		     if(imageY != null) {
		    	 imageY.disposeLocal();
		    	 imageY = null;
		     }
		     if(vjfX != null) {
		    	 vjfX.close();
		     }
		     if(vjfY != null) {
		    	 vjfY.close();
		     }
		     //new ViewJFrameImage(imageX); //testing
		     //new ViewJFrameImage(imageY); //testing
			 dispose();
		 }
	}
	
	
	
	private void writeInputParams() {
		String outputFilename = imageX.getImageName() + "to" + imageY.getImageName() + "_params.txt";
		BufferedWriter bw;
		FileWriter fw;
		try {
	    	File outputFile = new File(parentDir + File.separator + outputFilename);
	        fw = new FileWriter(outputFile);
	        bw = new BufferedWriter(fw);
	        bw.write("imageH:" + imageXFilePathTextField.getText());
	        bw.newLine();
	        bw.write("imageF:" + imageYFilePathTextField.getText());
	        bw.newLine();
	        bw.write("trans1:" + transform1FilePathTextField.getText());
	        bw.newLine();
	        bw.write("trans2:" + transform2FilePathTextField.getText());
	        bw.newLine();
	        bw.write("trans3:" + transform3FilePathTextField.getText());
	        bw.newLine();
	        if(doAverageRadio.isSelected()) {
	        	if(ignoreBGRadio.isSelected()) {
	        		bw.write("average:ignore");
	        	}else {
	        		bw.write("average:include");
	        	}
	        }else if(doSqRtRadio.isSelected()) {
	        	bw.write("sqrRt");
	        }else {
	        	bw.write("closestZ");
	        }
	        bw.newLine();
	        if(doTrilinearRadio.isSelected()) {
	        	bw.write("trilinear");
	        }else {
	        	bw.write("bSpline");
	        }
	        bw.newLine();
	        if(doRescaleRadio.isSelected()) {
	        	bw.write("rescale:" + slopeR + ":" + bR + ":" + slopeG + ":" + bG + ":" + slopeB + ":" + bB);
	        }else {
	        	bw.write("noRescale");
	        }

	        bw.close();
		}catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	
	
	
	/**
	 * reads the non-linear transform file
	 * @param nltFile
	 * @return
	 */
	private boolean readNLTFile(File nltFile) {
		String directory;
        RandomAccessFile in;
        String str = null;
        StringTokenizer stoken = null;
        int i, j, k;
        int srcMinExtent;
        int iNumControlPointsMax;
		try {
            in = new RandomAccessFile(nltFile, "r");

            // read number of dimensions
            do {
                str = in.readLine().trim();
            } while (str.substring(0, 1).equals("#"));

            float fDims = Float.valueOf(str).floatValue();

            if (2.5f == fDims) {
                nDims = 3;
                have25D = true;
            } else {
                nDims = (int) fDims;
                have25D = false;
            }

            if (imageY.getNDims() != nDims) {
                MipavUtil.displayError("");
                in.close();

                return false;
            }

            // read resolutions for output image
            do {
                str = in.readLine().trim();
            } while (str.substring(0, 1).equals("#"));

            stoken = new StringTokenizer(str);
            resolutions = new float[nDims];
            srcMinExtent = Integer.MAX_VALUE;

            for (i = 0; i < nDims; i++) {
                resolutions[i] = Float.valueOf(stoken.nextToken()).floatValue();

                if ((imageY.getExtents()[i] < srcMinExtent) && ((!have25D) || (i < 2))) {
                    srcMinExtent = imageY.getExtents()[i];
                }
            }

            // If 2D/3D, read dimensions for target image
            if (!have25D) {

                do {
                    str = in.readLine().trim();
                } while (str.substring(0, 1).equals("#"));

                stoken = new StringTokenizer(str);
                destExtents = new int[nDims];
                destMinExtent = Integer.MAX_VALUE;

                for (i = 0; i < nDims; i++) {
                    destExtents[i] = Integer.valueOf(stoken.nextToken()).intValue();

                    if (destExtents[i] < destMinExtent) {
                        destMinExtent = destExtents[i];
                    }
                }
            } else {
                numberSlices = imageY.getExtents()[2];
            }

            // read B-spline degree
            do {
                str = in.readLine().trim();
            } while (str.substring(0, 1).equals("#"));

            stoken = new StringTokenizer(str);
            splineDegree = Integer.valueOf(stoken.nextToken()).intValue();

            if ((splineDegree < 1) || (splineDegree > 4)) {
                MipavUtil.displayError("Error! Spline degree has an illegal value = " + splineDegree);
                in.close();

                return false;
            }

            // read number of control points
            do {
                str = in.readLine().trim();
            } while (str.substring(0, 1).equals("#"));

            stoken = new StringTokenizer(str);
            numControlPoints = Integer.valueOf(stoken.nextToken()).intValue();

            int iNumControlPointsMin = BSplineBasisf.GetMinNumControlPoints(splineDegree);

            if (have25D) {
                iNumControlPointsMax = srcMinExtent / 2;
            } else {
                iNumControlPointsMax = destMinExtent / 2;
            }

            if (numControlPoints < iNumControlPointsMin) {
                MipavUtil.displayError("Error! The parameter file specifies " + numControlPoints +
                                       " control points, but " + iNumControlPointsMin + " are required");
                in.close();

                return false;
            }

            if (numControlPoints > iNumControlPointsMax) {
                MipavUtil.displayError("Error! The parameter file specifies " + numControlPoints +
                                       " control points, but no more than " + iNumControlPointsMax +
                                       " are allowed");
                in.close();

                return false;
            }

            if (!have25D) {
                int allDimControlPoints = (nDims == 2) ? (numControlPoints * numControlPoints)
                                                       : (numControlPoints * numControlPoints * numControlPoints);

                controlMat = new float[allDimControlPoints][nDims];

                for (i = 0; i < allDimControlPoints; i++) {

                    do {
                        str = in.readLine().trim();
                    } while (str.substring(0, 1).equals("#"));

                    stoken = new StringTokenizer(str);

                    for (j = 0; j < nDims; j++) {
                        controlMat[i][j] = Float.valueOf(stoken.nextToken()).floatValue();
                    }
                } // for (i = 0; i < allDimControlPoints; i++)
            } // if (!have25D)
            else { // have25D

                int allDimControlPoints = numControlPoints * numControlPoints;
                controlMat25D = new float[numberSlices][allDimControlPoints][2];

                for (k = 0; k < numberSlices; k++) {

                    for (i = 0; i < allDimControlPoints; i++) {

                        do {
                            str = in.readLine().trim();
                        } while (str.substring(0, 1).equals("#"));

                        stoken = new StringTokenizer(str);
                        controlMat25D[k][i][0] = Float.valueOf(stoken.nextToken()).floatValue();
                        controlMat25D[k][i][1] = Float.valueOf(stoken.nextToken()).floatValue();
                    } // for (i = 0; i < allDimControlPoints; i++)
                } // for (k = 0; k < numberSlices; k++)
            } // else have25D

            in.close();

            return true;
        } catch (IOException e) {
            MipavUtil.displayError("Read Error reading nlt file : "  +   e.getMessage());

            return false;
        }
	}
	
	/**
	 * reads the transform file
	 * @param transformFile
	 */
	private void readTransform1(File transform1File) {
		try {
            RandomAccessFile raFile = new RandomAccessFile(transform1File, "r");
            String[] arr;
            raFile.readLine(); //skip over num columns since we know it is 4
            raFile.readLine(); //skip over num rows since we know it is 4
            double[][] doubleArr = new double[4][4];
            String line1 = raFile.readLine().trim();
            arr = line1.split("\\s+");
            if(arr.length == 4) {
           	 doubleArr[0][0] = Double.valueOf(arr[0]).doubleValue();
           	 doubleArr[0][1] = Double.valueOf(arr[1]).doubleValue();
           	 doubleArr[0][2] = Double.valueOf(arr[2]).doubleValue();
           	 doubleArr[0][3] = Double.valueOf(arr[3]).doubleValue();
            }
            String line2 = raFile.readLine().trim();
            arr = line2.split("\\s+");
            if(arr.length == 4) {
           	 doubleArr[1][0] = Double.valueOf(arr[0]).doubleValue();
           	 doubleArr[1][1] = Double.valueOf(arr[1]).doubleValue();
           	 doubleArr[1][2] = Double.valueOf(arr[2]).doubleValue();
           	 doubleArr[1][3] = Double.valueOf(arr[3]).doubleValue();
            }
            String line3 = raFile.readLine().trim();
            arr = line3.split("\\s+");
            if(arr.length == 4) {
           	 doubleArr[2][0] = Double.valueOf(arr[0]).doubleValue();
           	 doubleArr[2][1] = Double.valueOf(arr[1]).doubleValue();
           	 doubleArr[2][2] = Double.valueOf(arr[2]).doubleValue();
           	 doubleArr[2][3] = Double.valueOf(arr[3]).doubleValue();
            }
            String line4 = raFile.readLine().trim();
            arr = line4.split("\\s+");
            if(arr.length == 4) {
           	 doubleArr[3][0] = Double.valueOf(arr[0]).doubleValue();
           	 doubleArr[3][1] = Double.valueOf(arr[1]).doubleValue();
           	 doubleArr[3][2] = Double.valueOf(arr[2]).doubleValue();
           	 doubleArr[3][3] = Double.valueOf(arr[3]).doubleValue();
            }
            raFile.close(); 
            
            matrixGreen = new TransMatrix(4);
            matrixGreen.setMatrix(doubleArr);
            System.out.println("matrixGreen:");
            System.out.println(matrixGreen.toString());
            
            transform1FilePathTextField.setText(currDir);
            
            //matrixGreen = new Matrix(doubleArr,4,4);
   	 }catch(Exception ex) {
   		 ex.printStackTrace();
   	 }
	}
	
	/**
	 * reads the transform file
	 * @param transformFile
	 */
	private void readTransform2(File transform2File){
		try {
            RandomAccessFile raFile = new RandomAccessFile(transform2File, "r");
            String[] arr;
            raFile.readLine(); //skip over num columns since we know it is 4
            raFile.readLine(); //skip over num rows since we know it is 4
            double[][] doubleArr = new double[4][4];
            String line1 = raFile.readLine().trim();
            arr = line1.split("\\s+");
            if(arr.length == 4) {
           	 doubleArr[0][0] = Double.valueOf(arr[0]).doubleValue();
           	 doubleArr[0][1] = Double.valueOf(arr[1]).doubleValue();
           	 doubleArr[0][2] = Double.valueOf(arr[2]).doubleValue();
           	 doubleArr[0][3] = Double.valueOf(arr[3]).doubleValue();
            }
            String line2 = raFile.readLine().trim();
            arr = line2.split("\\s+");
            if(arr.length == 4) {
           	 doubleArr[1][0] = Double.valueOf(arr[0]).doubleValue();
           	 doubleArr[1][1] = Double.valueOf(arr[1]).doubleValue();
           	 doubleArr[1][2] = Double.valueOf(arr[2]).doubleValue();
           	 doubleArr[1][3] = Double.valueOf(arr[3]).doubleValue();
            }
            String line3 = raFile.readLine().trim();
            arr = line3.split("\\s+");
            if(arr.length == 4) {
           	 doubleArr[2][0] = Double.valueOf(arr[0]).doubleValue();
           	 doubleArr[2][1] = Double.valueOf(arr[1]).doubleValue();
           	 doubleArr[2][2] = Double.valueOf(arr[2]).doubleValue();
           	 doubleArr[2][3] = Double.valueOf(arr[3]).doubleValue();
            }
            String line4 = raFile.readLine().trim();
            arr = line4.split("\\s+");
            if(arr.length == 4) {
           	 doubleArr[3][0] = Double.valueOf(arr[0]).doubleValue();
           	 doubleArr[3][1] = Double.valueOf(arr[1]).doubleValue();
           	 doubleArr[3][2] = Double.valueOf(arr[2]).doubleValue();
           	 doubleArr[3][3] = Double.valueOf(arr[3]).doubleValue();
            }
            raFile.close(); 
            matrixAffine = new TransMatrix(4);
            matrixAffine.setMatrix(doubleArr);
            System.out.println("matrixAffine:");
            System.out.println(matrixAffine.toString());
            
            transform2FilePathTextField.setText(currDir);
            //matrixAffine = new Matrix(doubleArr,4,4);
   	 }catch(Exception ex) {
   		 ex.printStackTrace();
   	 }
	}
	
	
	/**
	 * calculates slop based on 4 data points
	 * @param Y1
	 * @param X1
	 * @param Y2
	 * @param X2
	 * @return
	 */
	private float calculateSlope(float Y1, float X1, float Y2, float X2) {
		float slope = 0;
		float Y = Y2 - Y1;
		float X = X2 - X1;
		if(X == 0) {
			slope = 0;
		}else {
			slope = Y/X;
		}
		return slope;
	}
	
	/**
	 * calculates b-intercept
	 * @param Y1
	 * @param X1
	 * @param slope
	 * @return
	 */
	private float calculateB(float Y1, float X1, float slope) {
		float b = 0;
		float mx = X1 * slope;
		b = Y1 - mx;
		return b;
	}
	
	/**
	 * gets new value based on slope and b-intercept
	 * @param X
	 * @param slope
	 * @param b
	 * @return
	 */
	private float getNewValue(float X, float slope, float b) {
		float Y = 0;
		float mx = slope * X;
		Y = mx + b;
		return Y;
	}
	
	
	
	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}

	/**
     * This legacy code returns all active vois for a given source image.  PlugIns should explicitly identify VOIs they would
     * like to process using AlgorithmVOIProps, because the user may have already added other VOIs to srcImage, or VOIs
     * may be created by the algorithm in an unexpected way.  This plugin relied on <code>AlgorithmVOIProp</code>'s 
     * getActiveVOIs() code, so that code has been moved into this plugin.
     * 
     * Use of this method is discouraged, as shown by the old documentation for this method:
     * not for use. should be moved to a better location. does NOT clone the VOIs that it find to be active, and inserts
     * into a new ViewVOIVector. if no VOIs are active, the ViewVOIVector returned is <code>null</code>.
     *
     * @return  All the active VOIs for a given srcImage.
     */
    private ViewVOIVector getActiveVOIs(ModelImage srcImage) {
        ViewVOIVector voiList;

        voiList = new ViewVOIVector();

        int i;

        try {

            for (i = 0; i < srcImage.getVOIs().size(); i++) {

                if (srcImage.getVOIs().VOIAt(i).isActive()) {

                    // voi at i is the active voi
                    voiList.addElement(srcImage.getVOIs().VOIAt(i));
                }
            }
        } catch (ArrayIndexOutOfBoundsException indexException) {

            // got to the end of list and never found an active VOI.
            // return an  empty VOI list.
            return new ViewVOIVector();
        }

        return voiList;
    }

}
