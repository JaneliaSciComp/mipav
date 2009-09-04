import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileOutputStream;
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

import WildMagic.LibFoundation.Curves.BSplineBasisDiscretef;
import WildMagic.LibFoundation.Curves.BSplineBasisf;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.BSplineLattice3Df;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

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
	private JPanel mainPanel;
	
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
	private boolean doAverage = true;

	/** button group **/
    private ButtonGroup doAverageGroup;

    /** radio buttons **/
    private JRadioButton doAverageRadio, doClosestZRadio;
    
    /** checkbox **/
    private JCheckBox concatMatricesOnly;
    
    /** 2.5 d */
    private boolean have25D = false;
    
    /** num dims */
    private int nDims;
    
    /** resolutions */
    private float[] resolutions;
    
    /** DOCUMENT ME! */
    private int[] destExtents;

    /** DOCUMENT ME! */
    private int destMinExtent;
    
    /** DOCUMENT ME! */
    private int numberSlices;
    
    /** DOCUMENT ME! */
    private int splineDegree;
    
    /** DOCUMENT ME! */
    private int numControlPoints;
    
    /** DOCUMENT ME! */
    private float[][] controlMat;
    
    /** DOCUMENT ME! */
    private float[][][] controlMat25D;
    
    /** DOCUMENT ME! */
    private ModelSimpleImage m_kSimpleImageResult;
    
    /** 2D and 3D B-Spline basis definitions. */
    private BSplineBasisDiscretef m_kBSplineBasisX;

    /** DOCUMENT ME! */
    private BSplineBasisDiscretef m_kBSplineBasisY;

    /** DOCUMENT ME! */
    private BSplineBasisDiscretef m_kBSplineBasisZ;
    
    /** DOCUMENT ME! */
    private BSplineLattice3Df m_kBSpline3D;
    
    
    
    
    
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
        setTitle("Drosophila Retinal Registration");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();

        JLabel imageXLabel = new JLabel("ImageX");
        imageXFilePathTextField = new JTextField(20);
        imageXFilePathTextField.setEditable(false);
        imageXFilePathTextField.setBackground(Color.white);
        imageXBrowseButton = new JButton("Browse");
        imageXBrowseButton.addActionListener(this);
        imageXBrowseButton.setActionCommand("imageXBrowse");

        JLabel imageYLabel = new JLabel("ImageY");
        imageYFilePathTextField = new JTextField(20);
        imageYFilePathTextField.setEditable(false);
        imageYFilePathTextField.setBackground(Color.white);
        imageYBrowseButton = new JButton("Browse");
        imageYBrowseButton.addActionListener(this);
        imageYBrowseButton.setActionCommand("imageYBrowse");

        JLabel transform1Label = new JLabel("Transformation 1 - Green");
        transform1FilePathTextField = new JTextField(20);
        transform1FilePathTextField.setEditable(false);
        transform1FilePathTextField.setBackground(Color.white);
        transform1BrowseButton = new JButton("Browse");
        transform1BrowseButton.addActionListener(this);
        transform1BrowseButton.setActionCommand("transform1Browse");

        JLabel transform2Label = new JLabel("Transformation 2 - Affine");
        transform2FilePathTextField = new JTextField(20);
        transform2FilePathTextField.setEditable(false);
        transform2FilePathTextField.setBackground(Color.white);
        transform2BrowseButton = new JButton("Browse");
        transform2BrowseButton.addActionListener(this);
        transform2BrowseButton.setActionCommand("transform2Browse");
        
        JLabel transform3Label = new JLabel("Transformation 3 - Nonlinear");
        transform3FilePathTextField = new JTextField(20);
        transform3FilePathTextField.setEditable(false);
        transform3FilePathTextField.setBackground(Color.white);
        transform3BrowseButton = new JButton("Browse");
        transform3BrowseButton.addActionListener(this);
        transform3BrowseButton.setActionCommand("transform3Browse");
        
        doAverageGroup = new ButtonGroup();
        doAverageRadio = new JRadioButton("Average");
        doClosestZRadio = new JRadioButton("Closest Z");
        doAverageRadio.setSelected(true);
        doAverageGroup.add(doAverageRadio);
        doAverageGroup.add(doClosestZRadio);

        concatMatricesOnly = new JCheckBox("concat affine and green matrices only");

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
        mainPanel.add(doAverageRadio,gbc);
        gbc.gridx = 1;
        mainPanel.add(doClosestZRadio,gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
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
        imageXFilePathTextField.setText(currDir);
        //imageY
        currDir = "C:\\images\\nichd\\1\\N4A07-TM2-40XO-NA-13-12bit-080608-1y0.ics";
        fileIO = new FileIO();
        imageY = fileIO.readImage("N4A07-TM2-40XO-NA-13-12bit-080608-1y0.ics", "C:\\images\\nichd\\1" + File.separator, true, null);
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
        imageXFilePathTextField.setText(currDir);
        //imageY
        currDir = "C:\\images\\nichd\\2_withBlurring2\\N4A07-TM2-40XO-NA-13-12bit-080608-3y0.ics";
        fileIO = new FileIO();
        imageY = fileIO.readImage("N4A07-TM2-40XO-NA-13-12bit-080608-3y0.ics", "C:\\images\\nichd\\2_withBlurring2" + File.separator, true, null);
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
		
		 if (command.equalsIgnoreCase("imageXBrowse")) { 
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
			 dispose();
		 }else if(command.equalsIgnoreCase("ok")) {
			 if(doAverageRadio.isSelected()) {
				 doAverage = true;
			 }else {
				 doAverage = false;
			 }

			 TransMatrix intermMatrix1 = new TransMatrix(4);
			 intermMatrix1.Mult(matrixAffine, matrixGreen); //pretty sure this is correct
			 System.out.println("intermMatrix1");
			 System.out.println(intermMatrix1.toString());
			 
			 if(concatMatricesOnly.isSelected()) {
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
			 }
			 
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
	             return;
	         }
	         
	         byte[] imageYBuffer;
	         int length2 = imageY.getExtents()[0] * imageY.getExtents()[1] * imageY.getExtents()[2] * 4;
	         imageYBuffer = new byte[length2];
	         try {
	        	 imageY.exportData(0, length2, imageYBuffer);
	         } catch (IOException error) {
	             System.out.println("IO exception");
	             return;
	         }
	         
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
			        
			        //FOR TESTING
			        /*new  ViewJFrameImage(akSimpleImageSourceMap[0], "imageMapX");
			        new  ViewJFrameImage(akSimpleImageSourceMap[1], "imageMapY");
			        new  ViewJFrameImage(akSimpleImageSourceMap[2], "imageMapZ");
			        return;*/
			        //END FOR TESTING
			        
	         }

			 float xmm,ymm,zmm;
			 byte[] rgb1 = new byte[3];
			 byte[] rgb2 = new byte[3];
			 short[] rgb1_short = new short[3];
			 short[] rgb2_short = new short[3];
			 //loop through each point in result image
			 for(int z=0;z<512;z++) {
				 	System.out.println("z is " +  z);
				 for(int y=0;y<512;y++) {
					 for(int x=0;x<512;x++) {
						 if(!transform3FilePathTextField.getText().trim().equals("")){
							 xmm = x * resultImage.getResolutions(0)[0];
							 ymm = y * resultImage.getResolutions(0)[1];
							 zmm = z * resultImage.getResolutions(0)[2];
							 
							 tPt1[0] = MipavMath.round(xmm /imageY.getResolutions(0)[0]);
							 tPt1[1] = MipavMath.round(ymm /imageY.getResolutions(0)[1]);
							 tPt1[2] = MipavMath.round(zmm /imageY.getResolutions(0)[2]);
							 
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
						 }else{
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
						 }
						 int floorPointIndex1=0, floorPointIndex2=0;
						 if(doAverage) {
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
									 rgb1 = AlgorithmConvolver.getTrilinearC(floorPointIndex1, dx1, dy1, dz1, extents1, imageXBuffer);
									 rgb1_short[0] = (short)(rgb1[0] & 0xff);
									 rgb1_short[1] = (short)(rgb1[1] & 0xff);
									 rgb1_short[2] = (short)(rgb1[2] & 0xff);
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
									 rgb2 = AlgorithmConvolver.getTrilinearC(floorPointIndex2, dx2, dy2, dz2, extents2, imageYBuffer); 
									 rgb2_short[0] = (short)(rgb2[0] & 0xff);
									 rgb2_short[1] = (short)(rgb2[1] & 0xff);
									 rgb2_short[2] = (short)(rgb2[2] & 0xff);
								 }else {
									 rgb2_short[0] = 0;
									 rgb2_short[1] = 0;
									 rgb2_short[2] = 0;
								 }
							 }

							 byte avgR, avgG, avgB;

							 //now do the averaging
							 avgR = (byte)Math.round(((rgb1_short[0] + rgb2_short[0])/2.0f));
							 avgG = (byte)Math.round(((rgb1_short[1] + rgb2_short[1])/2.0f));
							 avgB = (byte)Math.round(((rgb1_short[2] + rgb2_short[2])/2.0f));

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
						 }else {
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
								 rgb1 = AlgorithmConvolver.getTrilinearC(floorPointIndex1, dx1, dy1, dz1, extents1, imageXBuffer);
								 rgb1_short[0] = (short)(rgb1[0] & 0xff);
								 rgb1_short[1] = (short)(rgb1[1] & 0xff);
								 rgb1_short[2] = (short)(rgb1[2] & 0xff);
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
								 rgb2 = AlgorithmConvolver.getTrilinearC(floorPointIndex2, dx2, dy2, dz2, extents2, imageYBuffer); 
								 rgb2_short[0] = (short)(rgb2[0] & 0xff);
								 rgb2_short[1] = (short)(rgb2[1] & 0xff);
								 rgb2_short[2] = (short)(rgb2[2] & 0xff);
							 }

							 
							 byte r,g,b;
							 float r1,g1,b1;
							 float r2,g2,b2;

							if(diff1 < diff2) {
								r1 = ((1-(float)diff1) * rgb1_short[0]) + ((float)diff1 * rgb2_short[0]);
								r2 = ((float)diff2 * rgb1_short[0]) + ((1 - (float)diff2) * rgb2_short[0]);
								
								g1 = ((1-(float)diff1) * rgb1_short[1]) + ((float)diff1 * rgb2_short[1]);
								g2 = ((float)diff2 * rgb1_short[1]) + ((1 - (float)diff2) * rgb2_short[1]);
								
								b1 = ((1-(float)diff1) * rgb1_short[2]) + ((float)diff1 * rgb2_short[2]);
								b2 = ((float)diff2 * rgb1_short[2]) + ((1 - (float)diff2) * rgb2_short[2]);
								
								//r = (byte)(r1);
								//g = (byte)(g1);
								//b = (byte)(b1);
								
								r = (byte)((r1+r2)/2);
								g = (byte)((g1+g2)/2);
								b = (byte)((b1+b2)/2);
								
								//r = (byte)rgb1_short[0];
								//g = (byte)rgb1_short[1];
								//b = (byte)rgb1_short[2]; 
							}else {
								r1 = ((1-(float)diff2) * rgb2_short[0]) + ((float)diff2 * rgb1_short[0]);
								r2 = ((float)diff1 * rgb2_short[0]) + ((1 - (float)diff1) * rgb1_short[0]);
								
								g1 = ((1-(float)diff2) * rgb2_short[1]) + ((float)diff2 * rgb1_short[1]);
								g2 = ((float)diff1 * rgb2_short[1]) + ((1 - (float)diff1) * rgb1_short[1]);
								
								b1 = ((1-(float)diff2) * rgb2_short[2]) + ((float)diff2 * rgb1_short[2]);
								b2 = ((float)diff1 * rgb2_short[2]) + ((1 - (float)diff1) * rgb1_short[2]);
								
								//r = (byte)(r1);
								//g = (byte)(g1);
								//b = (byte)(b1);
								
								r = (byte)((r1+r2)/2);
								g = (byte)((g1+g2)/2);
								b = (byte)((b1+b2)/2);

								//r = (byte)rgb2_short[0];
								//g = (byte)rgb2_short[1];
								//b = (byte)rgb2_short[2];
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
	            return;
	        }

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
		     new ViewJFrameImage(resultImage);
		     if(imageX != null) {
		    	 imageX.disposeLocal();
		    	 imageX = null;
		     }
		     if(imageY != null) {
		    	 imageY.disposeLocal();
		    	 imageY = null;
		     }
			 dispose();
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
	private void readTransform1(File transformFile) {
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
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}

	

}
