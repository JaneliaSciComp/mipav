package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewTableModel;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.table.DefaultTableModel;

import WildMagic.LibFoundation.Mathematics.GMatrixf;


public class JDialogDTIEstimateTensor extends JDialogBase implements AlgorithmInterface, WindowListener  {
	
	private GridBagConstraints gbc,gbc2,gbc3;
	
	private JPanel mainPanel;
	
	/** table to display the src image names. */
    private JTable srcImagesTable;

    /** table model for the srcimages. */
    private DefaultTableModel srcTableModel;
    
    /** current directory  **/
    private String currDir = null;
    
    private ModelImage srcImage;
    
    private Vector<ArrayList> slicesVector = new Vector<ArrayList>();;
    
    private JTextField bValueTextField,xdimTextField, ydimTextField, numSlicesTextField, numVolumesTextField, hFOVTextField, vFOVTextField, outputDirTextField, formatTextField, gapTextField, sliceThicknessTextField, imagePlaneTextField, phaseEncodingTextField,maskImageTextField;
    
    private ModelImage maskImage;
    
    private File listFile;
    
    private String m_kParentDir;
    
    /** X-dimensions for Diffusion Weighted Images. */
    private int m_iDimX = 0;

    /** Y-dimensions for Diffusion Weighted Images. */
    private int m_iDimY = 0;
    
    /** Mean noise vale read from the .list file */
    private float m_fMeanNoise = 0f;

    /** raw image format read from the .list file: */
    private String m_kRawFormat;

    /** Number of slices in the Diffusion Weighted Images series. */
    private int m_iSlices = 0;

    /** Number of weights in the Diffusion Weighted Images series. */
    private int m_iWeights = 0;
    
    /** Slice thickness read from .list file */
    private float m_fResX = 1f, m_fResY = 1f, m_fResZ = 1f;
    /** Set to true if the slice resolution is read from the .list file: (xRes) */
    private boolean m_bUseXRes = false;
    /** Set to true if the slice resolution is read from the .list file: (yRes) */
    private boolean m_bUseYRes = false;
    /** Set to true if the slice resolution is read from the .list file: (zRes) */
    private boolean m_bUseZRes = false;
    
    /** General matrix storing BMatrix values. */
    private GMatrixf m_kBMatrix = null;
    
    /** keeps track of unique entries in the BMatrix */
    private int[] m_aiMatrixEntries;
    
    /** Number of different BMatrix rows: */
    private int m_iBOrig = 0;
    
    private AlgorithmDWI2DTI kAlgorithm;
    
    /** Diffusion Tensor image. */
    private ModelImage DTIImage = null;
    
    /**
     * List of file names for the Diffusion Weighted Images, from the .path
     * file.
     */
    private String[][] m_aakDWIList = null;
	
	public JDialogDTIEstimateTensor() {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		init();
	}
	
	
	public void init() {
		setForeground(Color.black);
        setTitle("Estimate Tensor");

        gbc = new GridBagConstraints();
        gbc2 = new GridBagConstraints();
        gbc3 = new GridBagConstraints();
        mainPanel = new JPanel(new GridBagLayout());
        
        
        
        
        JPanel srcPanel = new JPanel(new GridBagLayout());
        srcTableModel = new DefaultTableModel() {
			public boolean isCellEditable(int row, int column)
			 {
					return false;
			 }
		};
        srcTableModel.addColumn("Image");
        srcTableModel.addColumn("B-Value");
        srcTableModel.addColumn("X Gradient");
        srcTableModel.addColumn("Y Gradient");
        srcTableModel.addColumn("Z Gradient");
        srcImagesTable = new JTable(srcTableModel) {
        	public String getToolTipText(MouseEvent e) {
                String tip = null;
                java.awt.Point p = e.getPoint();
                int rowIndex = rowAtPoint(p);
                int columnIndex = columnAtPoint(p);
                if(columnIndex == 0){
                	String inputField = (String)srcTableModel.getValueAt(rowIndex, 0);
                	tip = inputField;
                	return tip;
                	
                }else {
                	return null;
                }
                
        	}
        };
        srcImagesTable.setPreferredScrollableViewportSize(new Dimension(1200, 200));
        srcImagesTable.getColumn("Image").setMinWidth(800);
        
        JLabel bValueLabel = new JLabel("     Apply B-Value to selected rows:");
        bValueTextField = new JTextField(6);
        JButton bValueButton = new JButton("Apply");
        bValueButton.addActionListener(this);
        bValueButton.setActionCommand("bValue");
        JPanel bValuePanel = new JPanel();
        bValuePanel.add(bValueLabel);
        bValuePanel.add(bValueTextField);
        bValuePanel.add(bValueButton);
        
        
        JButton srcBrowseButton = new JButton("Load DWI Volume");
        srcBrowseButton.addActionListener(this);
        srcBrowseButton.setActionCommand("srcBrowse");
        JButton loadBValGradFileButton = new JButton("Load B-Value/Grad File");
        loadBValGradFileButton.addActionListener(this);
        loadBValGradFileButton.setActionCommand("bvalGradBrowse");
        JPanel DWIButtonPanel = new JPanel();
        DWIButtonPanel.add(srcBrowseButton);
        DWIButtonPanel.add(loadBValGradFileButton);
        DWIButtonPanel.add(bValuePanel);

        
        
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.insets = new Insets(15,5,5,15);
        gbc2.gridwidth = 1;
        gbc2.anchor = GridBagConstraints.CENTER;
        JScrollPane srcImagesScrollPane = new JScrollPane(srcImagesTable);
        //srcImagesTable.addMouseListener(new MouseHandler());
        srcPanel.add(srcImagesScrollPane, gbc2);
        gbc2.gridy = 1;
        srcPanel.add(DWIButtonPanel, gbc2);

        
        
        JPanel studyParamsPanel = new JPanel(new GridBagLayout());
        studyParamsPanel.setBorder(buildTitledBorder("Study Parameters"));
        
        
        JPanel maskImagePanel = new JPanel();
        JLabel maskImageLabel = new JLabel("Mask Image");
        maskImageTextField = new JTextField(20);
        maskImageTextField.setEditable(false);
        maskImageTextField.setBackground(Color.white);
        JButton loadMaskButton = new JButton("Browse");
        loadMaskButton.addActionListener(this);
        loadMaskButton.setActionCommand("maskBrowse");
        maskImagePanel.add(maskImageTextField);
        maskImagePanel.add(loadMaskButton);
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        gbc3.insets = new Insets(15,5,5,15);
        gbc3.anchor = GridBagConstraints.WEST;
        studyParamsPanel.add(maskImageLabel,gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(maskImagePanel,gbc3);
        
        
        
        

        JLabel xdimLabel = new JLabel("Image X Dimension");
        xdimTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 1;
        studyParamsPanel.add(xdimLabel,gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(xdimTextField,gbc3);
        
        
        JLabel ydimLabel = new JLabel("Image Y Dimension");
        ydimTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 2;
        studyParamsPanel.add(ydimLabel,gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(ydimTextField,gbc3);
        
        
        
        JLabel numSlicesLabel = new JLabel("Num Slices per 3D Volume");
        numSlicesTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 3;
        studyParamsPanel.add(numSlicesLabel,gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(numSlicesTextField,gbc3);
        
        
        
        JLabel numVolumesLabel = new JLabel("Num 3D Volumes");
        numVolumesTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 4;
        studyParamsPanel.add(numVolumesLabel,gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(numVolumesTextField,gbc3);
        
        
        
       
        
        JLabel hFOVLabel = new JLabel("Horizontal Field of View (in mm)");
        hFOVTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 5;
        studyParamsPanel.add(hFOVLabel,gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(hFOVTextField,gbc3);
        
        JLabel vFOVLabel = new JLabel("Vertical Field of View (in mm)");
        vFOVTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 6;
        studyParamsPanel.add(vFOVLabel,gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(vFOVTextField,gbc3);
        
        
        
        
        JPanel outputDirPanel = new JPanel();
        JLabel outputDirLabel = new JLabel("Output Dir");
        outputDirTextField = new JTextField(20);
        outputDirTextField.setEditable(false);
        outputDirTextField.setBackground(Color.white);
        JButton outputDirButton = new JButton("Browse");
        outputDirButton.addActionListener(this);
        outputDirButton.setActionCommand("outputDirBrowse");
        outputDirPanel.add(outputDirTextField);
        outputDirPanel.add(outputDirButton);
        gbc3.gridx = 2;
        gbc3.gridy = 0;
        gbc3.insets = new Insets(15,5,5,15);
        gbc3.anchor = GridBagConstraints.WEST;
        studyParamsPanel.add(outputDirLabel,gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(outputDirPanel,gbc3);
        
        
        
        JLabel formatLabel = new JLabel("Format of raw images (integer, float, dicom)");
        formatTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 1;
        studyParamsPanel.add(formatLabel,gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(formatTextField,gbc3);
        
        
        JLabel gapLabel = new JLabel("Gap between slices (in mm)");
        gapTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 2;
        studyParamsPanel.add(gapLabel,gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(gapTextField,gbc3);
        
        JLabel sliceThicknessLabel = new JLabel("Slice thickness (in mm)");
        sliceThicknessTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 3;
        studyParamsPanel.add(sliceThicknessLabel,gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(sliceThicknessTextField,gbc3);
        
        
        JLabel imagePlaneLabel = new JLabel("Image plane (axial, coronal, sagittal)");
        imagePlaneTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 4;
        studyParamsPanel.add(imagePlaneLabel,gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(imagePlaneTextField,gbc3);
        
        JLabel phaseEncodingLabel = new JLabel("Orienation of Phase Encoding (vertical, horizontal)");
        phaseEncodingTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 5;
        studyParamsPanel.add(phaseEncodingLabel,gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(phaseEncodingTextField,gbc3);
        
        
       
        gbc2.gridy = 2;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.BOTH;
        srcPanel.add(studyParamsPanel, gbc2);
        
        
   
        
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15,5,5,0);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;

        mainPanel.add(srcPanel, gbc);
        
        
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

	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		
		 if (command.equalsIgnoreCase("srcBrowse")) {
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
		        	fileIO.setQuiet(true);

		            srcImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, true, null);
		            if(srcImage.isDicomImage()) {
		            	if(formatTextField.getText().trim().equals("")) {
		            		formatTextField.setText("dicom");
		            		formatTextField.setEditable(false);
		            	}else {
		            		if(!formatTextField.getText().trim().equals("dicom")) {
		            			MipavUtil.displayError("Image format for this volume does not match previous image format");
		                        return;
		            		}
		            	}
		            }else {
		            	int type = srcImage.getType();
		            	if (type == ModelStorageBase.INTEGER) {
		            		if(formatTextField.getText().trim().equals("")) {
			            		formatTextField.setText("integer");
			            		formatTextField.setEditable(false);
		            		}else {
		            			if(!formatTextField.getText().trim().equals("integer")) {
			            			MipavUtil.displayError("Image format for this volume does not match previous image format");
			                        return;
			            		}
		            		}
		            	}else if(type == ModelStorageBase.FLOAT) {
		            		if(formatTextField.getText().trim().equals("")) {
			            		formatTextField.setText("float");
			            		formatTextField.setEditable(false);
		            		}else {
		            			if(!formatTextField.getText().trim().equals("float")) {
			            			MipavUtil.displayError("Image format for this volume does not match previous image format");
			                        return;
			            		}
		            		}
		            	}
		            }
		            int numDims = srcImage.getNDims();
		            
		            int xDim = srcImage.getExtents()[0];
		            if(xdimTextField.getText().trim().equals("")) {
			            xdimTextField.setText(String.valueOf(xDim));
			            xdimTextField.setEditable(false);
		            }else {
		            	if(!xdimTextField.getText().trim().equals(String.valueOf(xDim))) {
	            			MipavUtil.displayError("Image X Dimension for this volume does not match previous image X dimension");
	                        return;
	            		}
		            }
		            int yDim = srcImage.getExtents()[1];
		            if(ydimTextField.getText().trim().equals("")) {
		            	ydimTextField.setText(String.valueOf(yDim));
		            	ydimTextField.setEditable(false);
		            }else {
		            	if(!ydimTextField.getText().trim().equals(String.valueOf(yDim))) {
	            			MipavUtil.displayError("Image Y Dimension for this volume does not match previous image Y dimension");
	                        return;
	            		}
		            }
		            
		            
		            int orientation = srcImage.getImageOrientation();
		            if(orientation == FileInfoBase.AXIAL) {
		            	if(imagePlaneTextField.getText().trim().equals("")) {
			            	imagePlaneTextField.setText("axial");
			            	imagePlaneTextField.setEditable(false);
		            	}else {
		            		if(!imagePlaneTextField.getText().trim().equals("axial")) {
		            			MipavUtil.displayError("Image plane for this volume does not match previous image plane");
		                        return;
		            		}
		            	}
		            }else if(orientation == FileInfoBase.SAGITTAL) {
		            	if(imagePlaneTextField.getText().trim().equals("")) {
			            	imagePlaneTextField.setText("sagittal");
			            	imagePlaneTextField.setEditable(false);
		            	}else {
		            		if(!imagePlaneTextField.getText().trim().equals("sagittal")) {
		            			MipavUtil.displayError("Image plane for this volume does not match previous image plane");
		                        return;
		            		}
		            	}
		            }else if(orientation == FileInfoBase.CORONAL) {
		            	if(imagePlaneTextField.getText().trim().equals("")) {
			            	imagePlaneTextField.setText("coronal");
			            	imagePlaneTextField.setEditable(false);
		            	}else {
		            		if(!imagePlaneTextField.getText().trim().equals("coronal")) {
		            			MipavUtil.displayError("Image plane for this volume does not match previous image plane");
		                        return;
		            		}
		            	}
		            }
		            
		            
		            
		            if(srcImage.isDicomImage()) {
		            	FileInfoDicom fileInfoDicom = (FileInfoDicom)srcImage.getFileInfo(0);
		            	
		            	String sliceThickness = ((String) (fileInfoDicom.getTagTable().getValue("0018,0050"))).trim();
		                float sliceTh = new Float(sliceThickness.trim()).floatValue();

		                String sliceGap = ((String) (fileInfoDicom.getTagTable().getValue("0018,0088"))).trim();
		                float sliceGp = new Float(sliceGap.trim()).floatValue();
		                
		                sliceGp = sliceTh - sliceGp;
		                sliceGap = String.valueOf(sliceGp);
		                
		                if(sliceThicknessTextField.getText().trim().equals("")) {
			                sliceThicknessTextField.setText(sliceThickness);
			                sliceThicknessTextField.setEditable(false);
		                }else {
		                	if(!sliceThicknessTextField.getText().trim().equals(sliceThickness)) {
		            			MipavUtil.displayError("Slice thickness for this volume does not match previous slice thickness");
		                        return;
		            		}
		                }
		                if(gapTextField.getText().trim().equals("")) {
			                gapTextField.setText(String.valueOf(sliceGap));
			                gapTextField.setEditable(false);
		                }else {
		                	if(!gapTextField.getText().trim().equals(String.valueOf(sliceGap))) {
		            			MipavUtil.displayError("Gap thickness for this volume does not match previous gap thickness");
		                        return;
		            		}
		                }
		                
		                
		                String fieldOfView = (String) (fileInfoDicom.getTagTable().getValue("0018,1100"));

		                if ((fieldOfView == null) || fieldOfView.trim().equals("")) {

		                    // get pixel space in x direction
		                    String xyPixelSpacingString = ((String) (fileInfoDicom.getTagTable().getValue("0028,0030"))).trim();
		                    int index = xyPixelSpacingString.indexOf("\\");
		                    String xPixelSpacingString = xyPixelSpacingString.substring(0, index);
		                    float xPixelSpacing = new Float(xPixelSpacingString).floatValue();
		                    float xFieldOfViewFloat = xPixelSpacing * xDim;
		                    
		                    String yPixelSpacingString = xyPixelSpacingString.substring(index+1, xyPixelSpacingString.length());
		                    float yPixelSpacing = new Float(yPixelSpacingString).floatValue();
		                    float yFieldOfViewFloat = yPixelSpacing * xDim;
		                    
		                    String xFieldOfView = String.valueOf(xFieldOfViewFloat);
		                    String yFieldOfView = String.valueOf(yFieldOfViewFloat);
		                    if(hFOVTextField.getText().trim().equals("")) {
		                    	hFOVTextField.setText(xFieldOfView);
		                    	hFOVTextField.setEditable(false);
		                    }else {
		                    	if(!hFOVTextField.getText().trim().equals(xFieldOfView.trim())) {
			            			MipavUtil.displayError("Horizontal FOV for this volume does not match previous horizontal FOV");
			                        return;
			            		}
		                    }
		                    
		                    if(vFOVTextField.getText().trim().equals("")) {
				                vFOVTextField.setText(yFieldOfView);
				                vFOVTextField.setEditable(false);
		                    }else {
		                    	if(!vFOVTextField.getText().trim().equals(yFieldOfView.trim())) {
			            			MipavUtil.displayError("Vertical FOV for this volume does not match previous vertical FOV");
			                        return;
			            		}
		                    }
		                    
		                }else {
		                	if(hFOVTextField.getText().trim().equals("")) {
		                    	hFOVTextField.setText(fieldOfView);
		                    	hFOVTextField.setEditable(false);
		                    }else {
		                    	if(!hFOVTextField.getText().trim().equals(fieldOfView.trim())) {
			            			MipavUtil.displayError("Horizontal FOV for this volume does not match previous horizontal FOV");
			                        return;
			            		}
		                    }
		                    
		                    if(vFOVTextField.getText().trim().equals("")) {
				                vFOVTextField.setText(fieldOfView);
				                vFOVTextField.setEditable(false);
		                    }else {
		                    	if(!vFOVTextField.getText().trim().equals(fieldOfView.trim())) {
			            			MipavUtil.displayError("Vertical FOV for this volume does not match previous vertical FOV");
			                        return;
			            		}
		                    }

		                }
		                
		                
		                String dir = ((String) (fileInfoDicom.getTagTable().getValue("0018,1312"))).trim();
		                String phaseEncodeDirection = "";

		                if (dir.equalsIgnoreCase("col")) {
		                    phaseEncodeDirection = "vertical";
		                } else if (dir.equalsIgnoreCase("row")) {
		                    phaseEncodeDirection = "horizontal";
		                }
		                
		                if(phaseEncodingTextField.getText().trim().equals("")) {
			                phaseEncodingTextField.setText(phaseEncodeDirection);
			                phaseEncodingTextField.setEditable(false);
		                }else {
		                	if(!phaseEncodingTextField.getText().trim().equals(phaseEncodeDirection)) {
		            			MipavUtil.displayError("Phase encoding orientaion for this volume does not match previous phase encoding orientation");
		                        return;
		            		}
		                }
		            	
		            	
		            }else {
		            	float sliceTh = srcImage.getFileInfo()[0].getSliceThickness();
		            	
		            	if(sliceThicknessTextField.getText().trim().equals("")) {
			                sliceThicknessTextField.setText(String.valueOf(sliceTh));
			                sliceThicknessTextField.setEditable(false);
		                }else {
		                	if(!sliceThicknessTextField.getText().trim().equals(String.valueOf(sliceTh))) {
		            			MipavUtil.displayError("Slice thickness for this volume does not match previous slice thickness");
		                        return;
		            		}
		                }
		            }
		            
		            
		            
		            
		            if(numDims == 3) {
		            	int numSlices = srcImage.getExtents()[2];
		            	if(numSlicesTextField.getText().trim().equals("")) {
			            	numSlicesTextField.setText(String.valueOf(numSlices));
			            	numSlicesTextField.setEditable(false);
		            	}else {
		            		if(!numSlicesTextField.getText().trim().equals(String.valueOf(numSlices))) {
		            			MipavUtil.displayError("Num slices for this volume does not match previous num slices");
		                        return;
		            		}
		            	}
		            	String firstImageSliceAbsPath = srcImage.getFileInfo(0).getFileDirectory() + srcImage.getFileInfo(0).getFileName();
		            	ArrayList<String> slicesArrayList = new ArrayList<String>();
		            	for(int i=0;i<numSlices;i++) {
		            		String absPath = srcImage.getFileInfo(i).getFileDirectory() + srcImage.getFileInfo(i).getFileName();
		            		//System.out.println(absPath);
		            		slicesArrayList.add(absPath);
		            	}
		            	slicesVector.add(slicesArrayList);
		            	
		            	
		            	
		            	// we will display the full path
                        Vector rowData = new Vector();
                        rowData.add(firstImageSliceAbsPath);
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        
                        srcTableModel.addRow(rowData);
		            
		            	numVolumesTextField.setText(String.valueOf(srcTableModel.getRowCount()));
		            	numVolumesTextField.setEditable(false);
		            }else if(numDims == 4) {
		            	//to do
		            	
		            }
		            
		            
		            srcImage.disposeLocal();
		            srcImage = null;
		        }
		        
		        
		        		
		 }else if(command.equalsIgnoreCase("bvalGradBrowse")) {
			 JFileChooser chooser = new JFileChooser();

	            if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
	            }
		        chooser.setDialogTitle("Choose image");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	boolean success = readBValGradientFile(chooser.getSelectedFile().getAbsolutePath());
		        	
		        	
		        }
			 
		 }else if(command.equals("bValue")) {
			 int[] selectedRows = srcImagesTable.getSelectedRows();
			 String bValue = bValueTextField.getText();
			 for(int i=0;i<selectedRows.length;i++) {
				 srcTableModel.setValueAt(bValue, selectedRows[i], 1);
			 }
			 bValueTextField.setText("");
			 
		 }else if(command.equals("maskBrowse")){
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
		        	fileIO.setQuiet(true);

		            maskImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, true, null);
		            maskImageTextField.setText(currDir);
		        }
		 }else if(command.equals("outputDirBrowse")) {
			 	JFileChooser chooser = new JFileChooser();

	            if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
	            }
		        chooser.setDialogTitle("Choose dir");
		        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	outputDirTextField.setText(currDir);
		        }
		 }else if(command.equals("ok")) {
			 boolean success = validateData();
			 if(!success) {
				 return;
			 }
			 createListFile();
			 createBMatrixFile();
			 createPathFile();
			 readListFile();
			 if ( m_kBMatrix == null || m_aakDWIList == null )
             {
                 MipavUtil.displayError("Both BMatrix and .path files are needed.");
                 return;
             }
             processDWI();
			 
		 }else if(command.equals("cancel")) {
			 if(maskImage != null) {
					maskImage.disposeLocal();
					maskImage = null;
				}
			 dispose();
		 }

	}
	
	
	
	
	
	
	public boolean readBValGradientFile(String gradientFilePath) {

        try {
            String str;
            File file = new File(gradientFilePath);
            RandomAccessFile raFile = new RandomAccessFile(file, "r");

            String firstLine = raFile.readLine();
            if(firstLine.contains(":")) {
            	raFile.seek(0);
            	//this is DTI Studio
            	int numRows = srcTableModel.getRowCount();
            	
            	
            	for(int i=0;i<numRows;i++) {
            		if(((String)srcTableModel.getValueAt(i, 2)).trim().equals("")) {
            			str = raFile.readLine();
            			if(str != null) {
            				String[] arr = str.split(":");
            				if(arr.length == 2) {
            					String grads = arr[1].trim();
        	                	String[] arr2 = grads.split("\\s+");
        	                	srcTableModel.setValueAt(arr2[0], i, 2);
        	                	srcTableModel.setValueAt(arr2[1], i, 3);
        	                	srcTableModel.setValueAt(arr2[2], i, 4);
            				}
            			}
            			
            			
            			
            		}
            		
            	}
            
            	
            }else {
            	int numRows = srcTableModel.getRowCount();
            	int start = 0;
            	for(int i=0;i<numRows;i++) {
            		if(((String)srcTableModel.getValueAt(i, 2)).trim().equals("")) {
            			start = i;
            			break;
            		}
            	}
            	//this is FSL
            	firstLine = firstLine.trim();
            	String[] arr = firstLine.split("\\s+");
            	int k = start;

            	for(int i=0;i<arr.length;i++) {
            		if(k < numRows) {
            			srcTableModel.setValueAt(arr[i], k, 2);
            			k = k+1;
            		}else {
            			break;
            		}
            	}
       

            	k = start;
            	String secondLine = raFile.readLine();
            	secondLine = secondLine.trim();
            	arr = secondLine.split("\\s+");
            	for(int i=0;i<arr.length;i++) {
            		if(k < numRows) {
            			srcTableModel.setValueAt(arr[i], k, 3);
            			k = k+1;
            		}else {
            			break;
            		}
            	}
            	
            	k = start;
            	String thirdLine = raFile.readLine();
            	thirdLine = thirdLine.trim();
            	arr = thirdLine.split("\\s+");
            	for(int i=0;i<arr.length;i++) {
            		if(k < numRows) {
            			srcTableModel.setValueAt(arr[i], k, 4);
            			k = k+1;
            		}else {
            			break;
            		}
            	}
            	
            	k = start;
            	String fourthLine = raFile.readLine();
            	fourthLine = fourthLine.trim();
            	arr = fourthLine.split("\\s+");
            	for(int i=0;i<arr.length;i++) {
            		if(k < numRows) {
            			srcTableModel.setValueAt(arr[i], k, 1);
            			k = k+1;
            		}else {
            			break;
            		}
            	}
            	
            	
            	
            }
            
            raFile.close();
        } catch (Exception e) {
            
        	MipavUtil.displayError("Error reading B-Value/Grad File");
            return false;
        }

        
        return true;
    }
	
	
	
	private void createListFile() {
		
		
		 try {
	            listFile = new File(outputDirTextField.getText()  + File.separator + "dti.list");
	            FileOutputStream outputStream = new FileOutputStream(listFile);
	            PrintStream printStream = new PrintStream(outputStream);

	            printStream.println("<!-- DTI initialization file -->");
	            printStream.println("<!-- do not remove the above comment line -->");
	            printStream.println();
	            printStream.println("<!-- NUMBER OF COLUMNS -->");
	            printStream.println("<original_columns>" + xdimTextField.getText().trim() + "</original_columns>");
	            printStream.println();
	            printStream.println("<!-- NUMBER OF ROWS -->");
	            printStream.println("<original_rows>" + ydimTextField.getText().trim() + "</original_rows>");
	            printStream.println();
	            printStream.println("<!-- NUMBER OF SLICES -->");
	            printStream.println("<slice>" + numSlicesTextField.getText().trim() + "</slice>");
	            printStream.println();
	            printStream.println("<!-- NUMBER OF BMATRICES -->");
	            printStream.println("<nim>" + numVolumesTextField.getText().trim() + "</nim>");
	            printStream.println();
	            printStream.println("<!-- ORIENTATION OF PHASE ENCODING (vertical, horizontal) -->");
	            printStream.println("<phase_encode_direction>" + phaseEncodingTextField.getText() + "</phase_encode_direction>");
	            printStream.println();
	            printStream.println("<!-- HORIZONTAL FIELD OF VIEW (in mm) -->");
	            printStream.println("<x_field_of_view>" + hFOVTextField.getText().trim() + "</x_field_of_view>");
	            printStream.println();
	            printStream.println("<!-- VERTICAL FIELD OF VIEW (in mm) -->");
	            printStream.println("<y_field_of_view>" + vFOVTextField.getText().trim() + "</y_field_of_view>");
	            printStream.println();
	            printStream.println("<!-- FORMAT OF RAW IMAGES (integer, float, dicom, dummy) -->");
	            printStream.println("<rawimageformat>" + formatTextField.getText().trim() + "</rawimageformat>");
	            printStream.println();
	            printStream.println("<!-- NAME OF BMATRIX FILE -->");
	            printStream.println("<bmatrixfile>dti.BMTXT</bmatrixfile>");
	            printStream.println();
	            printStream.println("<!-- GAP BETWEEN SLICES (in mm. Write 0 for contiguous slices) -->");
	            printStream.println("<slice_gap>" + gapTextField.getText().trim() + "</slice_gap>");
	            printStream.println();
	            printStream.println("<!-- SLICE THICKNESS (in mm) -->");
	            printStream.println("<slice_thickness>" + sliceThicknessTextField.getText().trim() + "</slice_thickness>");
	            printStream.println();
	            printStream.println("<!-- IMAGE PLANE (axial,coronal,sagittal) -->");
	            printStream.println("<image_plane>" + imagePlaneTextField.getText().trim() + "</image_plane>");
	            printStream.println();
	            printStream.println("<!-- NAME OF FILE CONTAINING PATH OF RAW IMAGES -->");
	            printStream.println("<raw_image_path_filename>dti.path</raw_image_path_filename>");

	            outputStream.close();
	        } catch (Exception e) {
	           
	        }
		
	}
	
	
	private void createBMatrixFile() {
		 try {
	            StringBuffer sb;
	            int padLength;
	            File bMatrixFile = new File(outputDirTextField.getText()  + File.separator + "dti.BMTXT");
	            FileOutputStream outputStream = new FileOutputStream(bMatrixFile);
	            PrintStream printStream = new PrintStream(outputStream);
	            DecimalFormat decFormat = new DecimalFormat("%16f");
		
		
		
		
		int numRows = srcTableModel.getRowCount();
		//formula for bmtxt values is :
        // bxx 2bxy 2bxz byy 2byz bzz
		for(int i=0;i<numRows;i++) {
			String bVal = ((String)srcTableModel.getValueAt(i, 1)).trim();
			String xGrad = ((String)srcTableModel.getValueAt(i, 2)).trim();
			String yGrad = ((String)srcTableModel.getValueAt(i, 3)).trim();
			String zGrad = ((String)srcTableModel.getValueAt(i, 4)).trim();
			float b = Float.valueOf(bVal).floatValue();
			float x = Float.valueOf(xGrad).floatValue();
			float y = Float.valueOf(yGrad).floatValue();
			float z = Float.valueOf(zGrad).floatValue();
			
			float _bxx = b * x * x;

            if (Math.abs(_bxx) == 0) {
                _bxx = Math.abs(_bxx);
            }

            float _2bxy = 2 * b * x * y;

            if (Math.abs(_2bxy) == 0) {
                _2bxy = Math.abs(_2bxy);
            }

            float _2bxz = 2 * b * x * z;

            if (Math.abs(_2bxz) == 0) {
                _2bxz = Math.abs(_2bxz);
            }

            float _byy = b * y * y;

            if (Math.abs(_byy) == 0) {
                _byy = Math.abs(_byy);
            }

            float _2byz = 2 * b * y * z;

            if (Math.abs(_2byz) == 0) {
                _2byz = Math.abs(_2byz);
            }

            float _bzz = b * z * z;

            if (Math.abs(_bzz) == 0) {
                _bzz = Math.abs(_bzz);
            }
            
            // following is for 1.4 compliant
            // otherwise, it would be : printStream.printf("%16f", b*x*x);
            String _bxx_string = String.valueOf(_bxx);
            int _bxx_stringLength = _bxx_string.length();
            sb = new StringBuffer(16);
            padLength = 16 - _bxx_stringLength;

            for (int j = 0; j < padLength; j++) {
                sb.insert(j, " ");
            }

            sb.insert(padLength, _bxx_string);
            printStream.print(sb.toString());


            String _2bxy_string = String.valueOf(_2bxy);
            int _2bxy_stringLength = _2bxy_string.length();
            sb = new StringBuffer(16);
            padLength = 16 - _2bxy_stringLength;

            for (int j = 0; j < padLength; j++) {
                sb.insert(j, " ");
            }

            sb.insert(padLength, _2bxy_string);
            printStream.print(sb.toString());


            String _2bxz_string = String.valueOf(_2bxz);
            int _2bxz_stringLength = _2bxz_string.length();
            sb = new StringBuffer(16);
            padLength = 16 - _2bxz_stringLength;

            for (int j = 0; j < padLength; j++) {
                sb.insert(j, " ");
            }

            sb.insert(padLength, _2bxz_string);
            printStream.print(sb.toString());


            String _byy_string = String.valueOf(_byy);
            int _byy_stringLength = _byy_string.length();
            sb = new StringBuffer(16);
            padLength = 16 - _byy_stringLength;

            for (int j = 0; j < padLength; j++) {
                sb.insert(j, " ");
            }

            sb.insert(padLength, _byy_string);
            printStream.print(sb.toString());


            String _2byz_string = String.valueOf(_2byz);
            int _2byz_stringLength = _2byz_string.length();
            sb = new StringBuffer(16);
            padLength = 16 - _2byz_stringLength;

            for (int j = 0; j < padLength; j++) {
                sb.insert(j, " ");
            }

            sb.insert(padLength, _2byz_string);
            printStream.print(sb.toString());


            String _bzz_string = String.valueOf(_bzz);
            int _bzz_stringLength = _bzz_string.length();
            sb = new StringBuffer(16);
            padLength = 16 - _bzz_stringLength;

            for (int j = 0; j < padLength; j++) {
                sb.insert(j, " ");
            }

            sb.insert(padLength, _bzz_string);
            printStream.print(sb.toString());

            printStream.println();
			
			
			
			
			
			
		}

		outputStream.close();
		
		}catch(Exception e) {
			 
		}
		
	}
	
	
	
	private void createPathFile() {
		
		
		
		int numSlicesPer3DVolume = Integer.valueOf(numSlicesTextField.getText().trim()).intValue();
		 try {
	            File pathFile = new File(outputDirTextField.getText()  + File.separator + "dti.path");
	            FileOutputStream outputStream = new FileOutputStream(pathFile);
	            PrintStream printStream = new PrintStream(outputStream);
	            
	           
	            for(int i=0;i<numSlicesPer3DVolume;i++) {
	            	Iterator iter = slicesVector.iterator();
	            	while (iter.hasNext()) {
	            		ArrayList slicesList = (ArrayList)iter.next();
	            		String path = (String)slicesList.get(i);
	            		printStream.println(path);
	            	}
	            	
	            	
	            	
	            	
	            	
	            	
	            	
	            }
	            outputStream.close();
	            
		 }catch(Exception e) {
			 
		 }
		
	}
	
	
	
	
	private void readListFile() {
		m_kParentDir =listFile.getParent();


        String pathFilename = null;
        String pathFileAbsPath = null;

        String bMatrixFilename = null;
        String bMatrixFileAbsPath = null;
        try {
            BufferedReader kReader = new BufferedReader(new FileReader(listFile));
            String lineString = null;
            while ((lineString = kReader.readLine()) != null) {
                if (lineString.startsWith("<original_columns>")) {
                    String columnsStr = lineString.substring(
                            lineString.indexOf("<original_columns>") + 18,
                            lineString.indexOf("</original_columns>"))
                            .trim();
                    m_iDimX = Integer.parseInt(columnsStr);
                } else if (lineString.startsWith("<original_rows>")) {
                    String rowsStr = lineString.substring(
                            lineString.indexOf("<original_rows>") + 15,
                            lineString.indexOf("</original_rows>")).trim();
                    m_iDimY = Integer.parseInt(rowsStr);
                } else if (lineString.startsWith("<slice>")) {
                    String sliceStr = lineString.substring(
                            lineString.indexOf("<slice>") + 7,
                            lineString.indexOf("</slice>")).trim();
                    m_iSlices = Integer.parseInt(sliceStr);
                } else if (lineString.startsWith("<nim>")) {
                    String nimStr = lineString.substring(
                            lineString.indexOf("<nim>") + 5,
                            lineString.indexOf("</nim>")).trim();
                    m_iWeights = Integer.parseInt(nimStr);
                } else if (lineString.startsWith("<rawimageformat>")) {
                    m_kRawFormat = lineString.substring(
                            lineString.indexOf("<rawimageformat>") + 16,
                            lineString.indexOf("</rawimageformat>")).trim();
                } else if (lineString
                        .startsWith("<raw_image_path_filename>")) {
                    pathFilename = lineString
                    .substring(
                            lineString
                            .indexOf("<raw_image_path_filename>") + 25,
                            lineString
                            .indexOf("</raw_image_path_filename>"))
                            .trim();
                    pathFileAbsPath = m_kParentDir + File.separator
                    + pathFilename;
                    // studyName = pathFilename.substring(0,
                    // pathFilename.indexOf(".path"));
                } else if (lineString.startsWith("<bmatrixfile>")) {
                    bMatrixFilename = lineString.substring(
                            lineString.indexOf("<bmatrixfile>") + 13,
                            lineString.indexOf("</bmatrixfile>")).trim();
                    bMatrixFileAbsPath = m_kParentDir + File.separator
                    + bMatrixFilename;
                    // studyName = pathFilename.substring(0,
                    // pathFilename.indexOf(".path"));
                } else if (lineString.startsWith("<x_field_of_view>")) {
                    String xFOVStr = lineString.substring(
                            lineString.indexOf("<x_field_of_view>") + 17,
                            lineString.indexOf("</x_field_of_view>"))
                            .trim();
                    float xFOV = Float.parseFloat(xFOVStr);
                    m_fResX = xFOV;
                    m_bUseXRes = true;
                } else if (lineString.startsWith("<y_field_of_view>")) {
                    String yFOVStr = lineString.substring(
                            lineString.indexOf("<y_field_of_view>") + 17,
                            lineString.indexOf("</y_field_of_view>"))
                            .trim();
                    float yFOV = Float.parseFloat(yFOVStr);
                    m_fResY = yFOV;
                    m_bUseYRes = true;
                } else if (lineString.startsWith("<slice_thickness>")) {
                    String zResStr = lineString.substring(
                            lineString.indexOf("<slice_thickness>") + 17,
                            lineString.indexOf("</slice_thickness>"))
                            .trim();
                    m_fResZ = Float.parseFloat(zResStr);
                    m_bUseZRes = true;
                } else if (lineString.startsWith("<noise_mean_ori>")) {
                    String noiseStr = lineString.substring(
                            lineString.indexOf("<noise_mean_ori>") + 16,
                            lineString.indexOf("</noise_mean_ori>")).trim();
                    m_fMeanNoise = Float.parseFloat(noiseStr);
                }
            }
            kReader.close();
            kReader = null;
        } catch (Exception e) {
            e.printStackTrace();
        }

        if (pathFilename != null) {
            readPathFile(pathFileAbsPath);
        }
        if (bMatrixFileAbsPath != null) {
            readBMatrixFile(bMatrixFileAbsPath);
        }
        m_fResX /= (float) m_iDimX;
        m_fResY /= (float) m_iDimY;
    }
	
	

    /**
     * Loads the .path file.
     * 
     * @param kFileName
     *            path file name.
     * @param kPathName
     *            parent directory.
     */
    public void readPathFile(String kFileName) {
        File kFile = new File(kFileName);
        if (!kFile.exists() || !kFile.canRead()) {
            return;
        }
        int iLength = (int) kFile.length();
        if (iLength <= 0) {
            return;
        }
        m_aakDWIList = new String[m_iSlices][m_iWeights];
        try {
            BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;
            for (int i = 0; i < m_iSlices; i++) {
                for (int j = 0; j < m_iWeights; j++) {
                    str = in.readLine();
                    //m_aakDWIList[i][j] = new String("blah" + File.separator
                           //+ str);
                    m_aakDWIList[i][j] = str;
                }
            }
            in.close();
        } catch (IOException e) {
        }
    }
    
    
    
    /**
     * Loads the BMatrix file.
     * 
     * @param kFileName
     *            name of BMatrix file.
     */
    private void readBMatrixFile(String kFileName) {
        File kFile = new File(kFileName);
        if (!kFile.exists() || !kFile.canRead()) {
            return;
        }
        int iLength = (int) kFile.length();
        if (iLength <= 0) {
            return;
        }

        try {
            BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;

            m_kBMatrix = new GMatrixf(m_iWeights, 6 + 1);

            String[] kBMatrixString = new String[m_iWeights];
            int nb = 0;

            m_aiMatrixEntries = new int[m_iWeights];
            for (int iRow = 0; iRow < m_iWeights; iRow++) {
                str = in.readLine();

                boolean gotit = false;
                for (int j = 0; j < nb; j++) {
                    if (str.equals(kBMatrixString[j])) {
                        gotit = true;
                        m_aiMatrixEntries[iRow] = j;
                        break;
                    }
                }
                if (!gotit) {
                    kBMatrixString[nb] = str;
                    m_aiMatrixEntries[iRow] = nb;
                    nb = nb + 1;
                }

                java.util.StringTokenizer st = new java.util.StringTokenizer(
                        str);
                for (int iCol = 0; iCol < 6; iCol++) {
                    float fValue = Float.valueOf(st.nextToken()).floatValue();
                    m_kBMatrix.Set(iRow, iCol, fValue);
                }
                m_kBMatrix.Set(iRow, 6, 1f);
            }
            in.close();

            m_iBOrig = nb;

        } catch (Exception e) {
        	MipavUtil.displayError("Error reading B-Matrix File");
            return;
        }
    }
	
    
    /** Calls AlgorithmDWI2DTI to create the diffusion tensor image. */
    private void processDWI()
    {
        if ( m_kBMatrix == null )
        {
            MipavUtil.displayError("BMatrix file must be set to create tensor data.");
            return;
        }
        if ( m_aakDWIList == null )
        {
            MipavUtil.displayError("Path file must be set to create tensor data.");
            return;
        }

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        kAlgorithm = new AlgorithmDWI2DTI( maskImage, false,
                m_iSlices, m_iDimX, m_iDimY,
                m_iBOrig, m_iWeights,
                m_fMeanNoise, m_aakDWIList,
                m_aiMatrixEntries, m_kBMatrix, m_kRawFormat);
        kAlgorithm.addListener(this);
        kAlgorithm.run();

    }
    
    public void algorithmPerformed(AlgorithmBase algorithm) {

    	if(kAlgorithm.isCompleted()) {
           DTIImage = ((AlgorithmDWI2DTI)kAlgorithm).getDTIImage();

           DTIImage.saveImage(listFile.getParent() + File.separator, "DTI.xml", FileUtility.XML, false);
           setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
           if(maskImage != null) {
        	   maskImage.disposeLocal();
        	   maskImage = null;
           }

    	}
    }
    
    
    
    private boolean validateData() {
    	boolean isValid = true;
    	int numRows = srcTableModel.getRowCount();
    	if(numRows == 0) {
    		MipavUtil.displayError("DWI data is required");
			return false;
    	}
    	for(int i=0;i<numRows;i++) {
    		String bValString = ((String)srcTableModel.getValueAt(i, 1)).trim();
    		String xgradString = ((String)srcTableModel.getValueAt(i, 2)).trim();
    		String ygradString = ((String)srcTableModel.getValueAt(i, 3)).trim();
    		String zgradString = ((String)srcTableModel.getValueAt(i, 4)).trim();
    		if(bValString.equals("") || xgradString.equals("") || ygradString.equals("") || zgradString.equals("")) {
    			MipavUtil.displayError("B-Values and Gradient Direction Inputs are required");
    			return false;
    		}
    		try {
    			Float.valueOf(bValString);
    		}catch(NumberFormatException e) {
    			MipavUtil.displayError("B-Value and Gradient Direction Inputs must be valid numbers");
    			return false;
    		}
    		try {
    			Float.valueOf(xgradString);
    		}catch(NumberFormatException e) {
    			MipavUtil.displayError("B-Value and Gradient Direction Inputs must be valid numbers");
    			return false;
    		}
    		try {
    			Float.valueOf(ygradString);
    		}catch(NumberFormatException e) {
    			MipavUtil.displayError("B-Value and Gradient Direction Inputs must be valid numbers");
    			return false;
    		}
    		try {
    			Float.valueOf(zgradString);
    		}catch(NumberFormatException e) {
    			MipavUtil.displayError("B-Value and Gradient Direction Inputs must be valid numbers");
    			return false;
    		}
    	}
    	
    	String maskImageString = maskImageTextField.getText().trim();
    	String xdimString = xdimTextField.getText().trim();
    	String ydimString = ydimTextField.getText().trim();
    	String numSlicesString = numSlicesTextField.getText().trim();
    	String numVolsString = numVolumesTextField.getText().trim();
    	String hFOVString = hFOVTextField.getText().trim();
    	String vFOVString = vFOVTextField.getText().trim();
    	String outputDirString = outputDirTextField.getText().trim();
    	String formatString = formatTextField.getText().trim();
    	String gapString = gapTextField.getText().trim();
    	String sliceThicknessString = sliceThicknessTextField.getText().trim();
    	String imagePlaneString = imagePlaneTextField.getText().trim();
    	String phaseEncodingString = phaseEncodingTextField.getText().trim();
    	
    	if(maskImageString.equals("") || xdimString.equals("") || ydimString.equals("") || numSlicesString.equals("") || numVolsString.equals("") || hFOVString.equals("") || vFOVString.equals("") || outputDirString.equals("") || formatString.equals("") || gapString.equals("") || sliceThicknessString.equals("") || imagePlaneString.equals("") || phaseEncodingString.equals("") ) {
    		MipavUtil.displayError("One or more required study parameters is missing");
			return false;
    	}
    	
    	try {
			Integer.valueOf(xdimString);
		}catch(NumberFormatException e) {
			MipavUtil.displayError("Image X Dimension must be a valid number");
			return false;
		}
    	
		
		try {
			Integer.valueOf(ydimString);
		}catch(NumberFormatException e) {
			MipavUtil.displayError("Image Y Dimension must be a valid number");
			return false;
		}
		
		
		try {
			Integer.valueOf(numSlicesString);
		}catch(NumberFormatException e) {
			MipavUtil.displayError("Num Slices per 3D Volume must be a valid number");
			return false;
		}
		
		try {
			Integer.valueOf(numVolsString);
		}catch(NumberFormatException e) {
			MipavUtil.displayError("Num 3D Volumes must be a valid number");
			return false;
		}
		
		if(!(phaseEncodingString.equalsIgnoreCase("vertical") || phaseEncodingString.equalsIgnoreCase("horizontal"))) {
			MipavUtil.displayError("Origin of Phase Encoding is not valid");
			return false;
		}
		
		if(!(imagePlaneString.equalsIgnoreCase("axial") || imagePlaneString.equalsIgnoreCase("coronal") || imagePlaneString.equalsIgnoreCase("saggittal")))  {
			MipavUtil.displayError("Image plane is not valid");
			return false;
		}
		
		if(!(formatString.equalsIgnoreCase("integer") || formatString.equalsIgnoreCase("float") || formatString.equalsIgnoreCase("dicom"))) {
			MipavUtil.displayError("Format of raw images is not valid");
			return false;
		}
		
		
		
		
    
    	
    	
    	return isValid;
    }


	@Override
	public void windowClosing(WindowEvent event) {
		super.windowClosing(event);
		if(maskImage != null) {
			maskImage.disposeLocal();
			maskImage = null;
		}
	}
	
	
	

}
