package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;

import WildMagic.LibFoundation.Mathematics.GMatrixf;


public class JDialogDTIEstimateTensor extends JDialogBase implements AlgorithmInterface, WindowListener {

    /** grid bag constraints * */
    private GridBagConstraints gbc, gbc2, gbc3;

    /** main panel * */
    private JPanel mainPanel;

    /** table to display the src image names. */
    private JTable srcImagesTable;

    /** table model for the srcimages. */
    private DefaultTableModel srcTableModel;

    /** current directory * */
    private String currDir = null;

    /** src image * */
    private ModelImage srcImage;

    /** slices * */
    private final Vector<ArrayList<String>> slicesVector = new Vector<ArrayList<String>>();;

    /** textfields * */
    private JTextField bValueTextField, xdimTextField, ydimTextField, numSlicesTextField, numVolumesTextField,
            hFOVTextField, vFOVTextField, outputDirTextField, formatTextField, gapTextField, sliceThicknessTextField,
            imagePlaneTextField, phaseEncodingTextField, maskImageTextField;

    /** mask Image * */
    private ModelImage maskImage;

    /** list file * */
    private File listFile;

    /** paren tdir * */
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
    @SuppressWarnings("unused")
    private float m_fResX = 1f, m_fResY = 1f, m_fResZ = 1f;



    /** General matrix storing BMatrix values. */
    private GMatrixf m_kBMatrix = null;

    /** keeps track of unique entries in the BMatrix */
    private int[] m_aiMatrixEntries;

    /** Number of different BMatrix rows: */
    private int m_iBOrig = 0;

    /** handle to algorithm * */
    private AlgorithmDWI2DTI kAlgorithm;

    /** Diffusion Tensor image. */
    private ModelImage DTI = null;

    /** button * */
    private JButton loadMaskButton;

    /** List of file names for the Diffusion Weighted Images, from the .path * */
    private String[][] m_aakDWIList = null;

    /**
     * constructor
     */
    public JDialogDTIEstimateTensor() {
        super(ViewUserInterface.getReference().getMainFrame(), false);
        init();
    }

    /**
     * init
     */
    public void init() {
        setForeground(Color.black);
        setTitle("Estimate Tensor");

        gbc = new GridBagConstraints();
        gbc2 = new GridBagConstraints();
        gbc3 = new GridBagConstraints();
        mainPanel = new JPanel(new GridBagLayout());

        final JPanel srcPanel = new JPanel(new GridBagLayout());
        srcTableModel = new DefaultTableModel() {
            public boolean isCellEditable(final int row, final int column) {
                return false;
            }
        };
        srcTableModel.addColumn("Image");
        srcTableModel.addColumn("B-Value");
        srcTableModel.addColumn("X Gradient");
        srcTableModel.addColumn("Y Gradient");
        srcTableModel.addColumn("Z Gradient");
        srcImagesTable = new JTable(srcTableModel) {
            public String getToolTipText(final MouseEvent e) {
                System.out.println("working101");
                String tip = null;
                final java.awt.Point p = e.getPoint();
                final int rowIndex = rowAtPoint(p);
                final int columnIndex = columnAtPoint(p);
                if (columnIndex == 0) {
                    final String inputField = (String) srcTableModel.getValueAt(rowIndex, 0);
                    tip = inputField;
                    return tip;

                } else {
                    return null;
                }

            }
        };
        srcImagesTable.setPreferredScrollableViewportSize(new Dimension(1200, 200));
        srcImagesTable.getColumn("Image").setMinWidth(800);

        final JLabel bValueLabel = new JLabel("     Apply B-Value to selected rows:");
        bValueTextField = new JTextField(6);
        final JButton bValueButton = new JButton("Apply");
        bValueButton.addActionListener(this);
        bValueButton.setActionCommand("bValue");
        final JPanel bValuePanel = new JPanel();
        bValuePanel.add(bValueLabel);
        bValuePanel.add(bValueTextField);
        bValuePanel.add(bValueButton);

        
        final JButton dirBrowseButton = new JButton("Load DWI Directory");
        dirBrowseButton.addActionListener(this);
        dirBrowseButton.setActionCommand("dirBrowse");
        final JButton srcBrowseButton = new JButton("Load DWI Volume");
        srcBrowseButton.addActionListener(this);
        srcBrowseButton.setActionCommand("srcBrowse");
        final JButton loadBValGradFileButton = new JButton("Load B-Value/Grad File");
        loadBValGradFileButton.addActionListener(this);
        loadBValGradFileButton.setActionCommand("bvalGradBrowse");
        final JPanel DWIButtonPanel = new JPanel();
        DWIButtonPanel.add(dirBrowseButton);
        DWIButtonPanel.add(srcBrowseButton);
        DWIButtonPanel.add(loadBValGradFileButton);
        DWIButtonPanel.add(bValuePanel);

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.insets = new Insets(15, 5, 5, 15);
        gbc2.gridwidth = 1;
        gbc2.anchor = GridBagConstraints.CENTER;
        final JScrollPane srcImagesScrollPane = new JScrollPane(srcImagesTable);
        // srcImagesTable.addMouseListener(new MouseHandler());
        srcPanel.add(srcImagesScrollPane, gbc2);
        gbc2.gridy = 1;
        srcPanel.add(DWIButtonPanel, gbc2);

        final JPanel studyParamsPanel = new JPanel(new GridBagLayout());
        studyParamsPanel.setBorder(buildTitledBorder("Study Parameters"));
        final JPanel maskImagePanel = new JPanel();
        final JLabel maskImageLabel = new JLabel("Mask Image");
        maskImageTextField = new JTextField(20);
        maskImageTextField.setEditable(false);
        maskImageTextField.setBackground(Color.white);
        loadMaskButton = new JButton("Browse");
        loadMaskButton.addActionListener(this);
        loadMaskButton.setActionCommand("maskBrowse");
        loadMaskButton.setEnabled(false);
        maskImagePanel.add(maskImageTextField);
        maskImagePanel.add(loadMaskButton);
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        gbc3.insets = new Insets(15, 5, 5, 15);
        gbc3.anchor = GridBagConstraints.WEST;
        studyParamsPanel.add(maskImageLabel, gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(maskImagePanel, gbc3);

        final JLabel xdimLabel = new JLabel("Image X Dimension");
        xdimTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 1;
        studyParamsPanel.add(xdimLabel, gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(xdimTextField, gbc3);

        final JLabel ydimLabel = new JLabel("Image Y Dimension");
        ydimTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 2;
        studyParamsPanel.add(ydimLabel, gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(ydimTextField, gbc3);

        final JLabel numSlicesLabel = new JLabel("Num Slices per 3D Volume");
        numSlicesTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 3;
        studyParamsPanel.add(numSlicesLabel, gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(numSlicesTextField, gbc3);

        final JLabel numVolumesLabel = new JLabel("Num 3D Volumes");
        numVolumesTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 4;
        studyParamsPanel.add(numVolumesLabel, gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(numVolumesTextField, gbc3);

        final JLabel hFOVLabel = new JLabel("Horizontal Field of View (in mm)");
        hFOVTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 5;
        studyParamsPanel.add(hFOVLabel, gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(hFOVTextField, gbc3);

        final JLabel vFOVLabel = new JLabel("Vertical Field of View (in mm)");
        vFOVTextField = new JTextField(20);
        gbc3.gridx = 0;
        gbc3.gridy = 6;
        studyParamsPanel.add(vFOVLabel, gbc3);
        gbc3.gridx = 1;
        studyParamsPanel.add(vFOVTextField, gbc3);

        final JPanel outputDirPanel = new JPanel();
        final JLabel outputDirLabel = new JLabel("Output Dir");
        outputDirTextField = new JTextField(20);
        outputDirTextField.setEditable(false);
        outputDirTextField.setBackground(Color.white);
        final JButton outputDirButton = new JButton("Browse");
        outputDirButton.addActionListener(this);
        outputDirButton.setActionCommand("outputDirBrowse");
        outputDirPanel.add(outputDirTextField);
        outputDirPanel.add(outputDirButton);
        gbc3.gridx = 2;
        gbc3.gridy = 0;
        gbc3.insets = new Insets(15, 5, 5, 15);
        gbc3.anchor = GridBagConstraints.WEST;
        studyParamsPanel.add(outputDirLabel, gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(outputDirPanel, gbc3);

        final JLabel formatLabel = new JLabel("Format of raw images (integer, float, dicom)");
        formatTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 1;
        studyParamsPanel.add(formatLabel, gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(formatTextField, gbc3);

        final JLabel gapLabel = new JLabel("Gap between slices (in mm)");
        gapTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 2;
        studyParamsPanel.add(gapLabel, gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(gapTextField, gbc3);

        final JLabel sliceThicknessLabel = new JLabel("Slice thickness (in mm)");
        sliceThicknessTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 3;
        studyParamsPanel.add(sliceThicknessLabel, gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(sliceThicknessTextField, gbc3);

        final JLabel imagePlaneLabel = new JLabel("Image plane (axial, coronal, sagittal)");
        imagePlaneTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 4;
        studyParamsPanel.add(imagePlaneLabel, gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(imagePlaneTextField, gbc3);

        final JLabel phaseEncodingLabel = new JLabel("Orienation of Phase Encoding (vertical, horizontal)");
        phaseEncodingTextField = new JTextField(20);
        gbc3.gridx = 2;
        gbc3.gridy = 5;
        studyParamsPanel.add(phaseEncodingLabel, gbc3);
        gbc3.gridx = 3;
        studyParamsPanel.add(phaseEncodingTextField, gbc3);

        gbc2.gridy = 2;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.BOTH;
        srcPanel.add(studyParamsPanel, gbc2);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15, 5, 5, 0);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;

        mainPanel.add(srcPanel, gbc);

        final JPanel OKCancelPanel = new JPanel();
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
    }

    /**
     * action performed
     */
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();

        
        if(command.equalsIgnoreCase("dirBrowse")) {
        	JFileChooser chooser = new JFileChooser();

            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose directory");
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
            	currDir = chooser.getSelectedFile().getAbsolutePath();
                File file = new File(currDir);
                parse(file);
            	
            }

            loadMaskButton.setEnabled(true);
        }else if (command.equalsIgnoreCase("srcBrowse")) {
            JFileChooser chooser = new JFileChooser();

            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose image");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                FileIO fileIO = new FileIO();
                fileIO.setQuiet(true);

                srcImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                        + File.separator, true, null);

                

                boolean succ = populateFields();
                
                if(!succ) {
                	srcImage.disposeLocal();
                    srcImage = null;
                    return;
                }

                srcImage.disposeLocal();
                srcImage = null;
            }

            loadMaskButton.setEnabled(true);

        } else if (command.equalsIgnoreCase("bvalGradBrowse")) {
            final JFileChooser chooser = new JFileChooser();

            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose image");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                readBValGradientFile(currDir);
            }

        } else if (command.equals("bValue")) {
            final int[] selectedRows = srcImagesTable.getSelectedRows();
            final String bValue = bValueTextField.getText();
            for (final int element : selectedRows) {
                srcTableModel.setValueAt(bValue, element, 1);
            }
            bValueTextField.setText("");

        } else if (command.equals("maskBrowse")) {
            final JFileChooser chooser = new JFileChooser();

            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose image");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                final FileIO fileIO = new FileIO();
                fileIO.setQuiet(true);

                maskImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                        + File.separator, true, null);

                if (maskImage.getNDims() != 3) {
                    MipavUtil.displayError("Mask Image must be a 3D image");
                    maskImage.disposeLocal();
                    maskImage = null;
                    return;
                }

                final int xdim = maskImage.getExtents()[0];
                final int ydim = maskImage.getExtents()[1];

                if ( !xdimTextField.getText().trim().equals(String.valueOf(xdim))) {
                    MipavUtil.displayError("Mask image xdim does not match with dwi xdim");
                    maskImage.disposeLocal();
                    maskImage = null;
                    return;

                }

                if ( !ydimTextField.getText().trim().equals(String.valueOf(ydim))) {
                    MipavUtil.displayError("Mask image ydim does not match with dwi ydim");
                    maskImage.disposeLocal();
                    maskImage = null;
                    return;

                }

                maskImageTextField.setText(currDir);
            }
        } else if (command.equals("outputDirBrowse")) {
            final JFileChooser chooser = new JFileChooser();

            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose dir");
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                outputDirTextField.setText(currDir);
            }
        } else if (command.equals("ok")) {
            boolean success = validateData();
            if ( !success) {
                return;
            }
            createListFile();
            createBMatrixFile();
            createPathFile();
            readListFile();
            if (m_kBMatrix == null || m_aakDWIList == null) {
                MipavUtil.displayError("Both BMatrix and .path files are needed.");
                return;
            }
            processDWI();

        } else if (command.equals("cancel")) {
            if (maskImage != null) {
                maskImage.disposeLocal();
                maskImage = null;
            }
            dispose(); 
        } else {
            super.actionPerformed(e);
        }

    }
    
    
    /**
     * Parses study directory
     * @param file
     * @return
     */
    public boolean parse(File file){

    	
    	
    	File[] children = file.listFiles();
    	FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);
    	 try {

             
                 if (children[0].isDirectory()) {
                	 for(int i=0;i<children.length;i++) {
                		 parse(children[i]);
                	 }
                 } else {
                	 currDir = children[0].getParent();
                	 //if(fileIO == null) {
                		 //System.out.println("null");
                	 //}
                	 srcImage = fileIO.readImage(children[0].getName(), currDir
                             + File.separator, true, null);

                     

                     boolean succ = populateFields();
                     
                     if(!succ) {
                     	srcImage.disposeLocal();
                         srcImage = null;
                         return false;
                     }

                     srcImage.disposeLocal();
                     srcImage = null;
                	 
                 }
             
    	
    	 }catch(Exception e) {
    		 e.printStackTrace();
    		 return false;
    	 }
    	return true;
    	
    	
    }
    
    
    
    private boolean populateFields() {
    	
    	boolean isMultifile = srcImage.getFileInfo()[0].getMultiFile();
        int numDims = srcImage.getNDims();

                if (isMultifile && numDims == 4) {
                    MipavUtil.displayError("Multifile 4D images are not currently supported");
                    srcImage.disposeLocal();
                    srcImage = null;
            return false;

                }
                if (srcImage.isDicomImage()) {
                    if (formatTextField.getText().trim().equals("")) {
                        formatTextField.setText("dicom");
                        formatTextField.setEditable(false);
                    } else {
                        if ( !formatTextField.getText().trim().equals("dicom")) {
                            MipavUtil.displayError("Image format for this volume does not match previous image format");
                    return false;
                        }
                    }
                } else {
                    final int type = srcImage.getType();
                    if (type == ModelStorageBase.INTEGER) {
                        if (formatTextField.getText().trim().equals("")) {
                            formatTextField.setText("integer");
                            formatTextField.setEditable(false);
                        } else {
                            if ( !formatTextField.getText().trim().equals("integer")) {
                                MipavUtil
                                        .displayError("Image format for this volume does not match previous image format");
                        return false;
                            }
                        }
                    } else if (type == ModelStorageBase.FLOAT) {
                        if (formatTextField.getText().trim().equals("")) {
                            formatTextField.setText("float");
                            formatTextField.setEditable(false);
                        } else {
                            if ( !formatTextField.getText().trim().equals("float")) {
                                MipavUtil
                                        .displayError("Image format for this volume does not match previous image format");
                        return false;
                            }
                        }
                    }
                }

                final int xDim = srcImage.getExtents()[0];
                if (xdimTextField.getText().trim().equals("")) {
                    xdimTextField.setText(String.valueOf(xDim));
                    xdimTextField.setEditable(false);
                } else {
                    if ( !xdimTextField.getText().trim().equals(String.valueOf(xDim))) {
                        MipavUtil
                                .displayError("Image X Dimension for this volume does not match previous image X dimension");
                return false;
                    }
                }
                final int yDim = srcImage.getExtents()[1];
                if (ydimTextField.getText().trim().equals("")) {
                    ydimTextField.setText(String.valueOf(yDim));
                    ydimTextField.setEditable(false);
                } else {
                    if ( !ydimTextField.getText().trim().equals(String.valueOf(yDim))) {
                        MipavUtil
                                .displayError("Image Y Dimension for this volume does not match previous image Y dimension");
                return false;
                    }
                }

                final int orientation = srcImage.getImageOrientation();
                if (orientation == FileInfoBase.AXIAL) {
                    if (imagePlaneTextField.getText().trim().equals("")) {
                        imagePlaneTextField.setText("axial");
                        imagePlaneTextField.setEditable(false);
                    } else {
                        if ( !imagePlaneTextField.getText().trim().equals("axial")) {
                            MipavUtil.displayError("Image plane for this volume does not match previous image plane");
                    return false;
                        }
                    }
                } else if (orientation == FileInfoBase.SAGITTAL) {
                    if (imagePlaneTextField.getText().trim().equals("")) {
                        imagePlaneTextField.setText("sagittal");
                        imagePlaneTextField.setEditable(false);
                    } else {
                        if ( !imagePlaneTextField.getText().trim().equals("sagittal")) {
                            MipavUtil.displayError("Image plane for this volume does not match previous image plane");
                    return false;
                        }
                    }
                } else if (orientation == FileInfoBase.CORONAL) {
                    if (imagePlaneTextField.getText().trim().equals("")) {
                        imagePlaneTextField.setText("coronal");
                        imagePlaneTextField.setEditable(false);
                    } else {
                        if ( !imagePlaneTextField.getText().trim().equals("coronal")) {
                            MipavUtil.displayError("Image plane for this volume does not match previous image plane");
                    return false;
                        }
                    }
                }

                if (srcImage.isDicomImage()) {
                    final FileInfoDicom fileInfoDicom = (FileInfoDicom) srcImage.getFileInfo(0);

                    final String sliceThickness = ((String) (fileInfoDicom.getTagTable().getValue("0018,0050"))).trim();
                    final float sliceTh = new Float(sliceThickness.trim()).floatValue();

                    String sliceGap = ((String) (fileInfoDicom.getTagTable().getValue("0018,0088"))).trim();
                    float sliceGp = new Float(sliceGap.trim()).floatValue();

                    sliceGp = sliceTh - sliceGp;
                    sliceGap = String.valueOf(sliceGp);

                    if (sliceThicknessTextField.getText().trim().equals("")) {
                        sliceThicknessTextField.setText(sliceThickness);
                        sliceThicknessTextField.setEditable(false);
                    } else {
                        if ( !sliceThicknessTextField.getText().trim().equals(sliceThickness)) {
                            MipavUtil
                                    .displayError("Slice thickness for this volume does not match previous slice thickness");
                    return false;
                        }
                    }
                    if (gapTextField.getText().trim().equals("")) {
                        gapTextField.setText(String.valueOf(sliceGap));
                        gapTextField.setEditable(false);
                    } else {
                        if ( !gapTextField.getText().trim().equals(String.valueOf(sliceGap))) {
                            MipavUtil
                                    .displayError("Gap thickness for this volume does not match previous gap thickness");
                    return false;
                        }
                    }

                    final String fieldOfView = (String) (fileInfoDicom.getTagTable().getValue("0018,1100"));

                    if ( (fieldOfView == null) || fieldOfView.trim().equals("")) {

                        // get pixel space in x direction
                        final String xyPixelSpacingString = ((String) (fileInfoDicom.getTagTable()
                                .getValue("0028,0030"))).trim();
                        final int index = xyPixelSpacingString.indexOf("\\");
                        final String yPixelSpacingString = xyPixelSpacingString.substring(0, index);
                        final float yPixelSpacing = new Float(yPixelSpacingString).floatValue();
                        final float yFieldOfViewFloat = yPixelSpacing * xDim;

                        final String xPixelSpacingString = xyPixelSpacingString.substring(index + 1,
                                xyPixelSpacingString.length());
                        final float xPixelSpacing = new Float(xPixelSpacingString).floatValue();
                        final float xFieldOfViewFloat = xPixelSpacing * xDim;

                        final String xFieldOfView = String.valueOf(xFieldOfViewFloat);
                        final String yFieldOfView = String.valueOf(yFieldOfViewFloat);
                        if (hFOVTextField.getText().trim().equals("")) {
                            hFOVTextField.setText(xFieldOfView);
                            hFOVTextField.setEditable(false);
                        } else {
                            if ( !hFOVTextField.getText().trim().equals(xFieldOfView.trim())) {
                                MipavUtil
                                        .displayError("Horizontal FOV for this volume does not match previous horizontal FOV");
                        return false;
                            }
                        }

                        if (vFOVTextField.getText().trim().equals("")) {
                            vFOVTextField.setText(yFieldOfView);
                            vFOVTextField.setEditable(false);
                        } else {
                            if ( !vFOVTextField.getText().trim().equals(yFieldOfView.trim())) {
                                MipavUtil
                                        .displayError("Vertical FOV for this volume does not match previous vertical FOV");
                        return false;
                            }
                        }

                    } else {
                        if (hFOVTextField.getText().trim().equals("")) {
                            hFOVTextField.setText(fieldOfView);
                            hFOVTextField.setEditable(false);
                        } else {
                            if ( !hFOVTextField.getText().trim().equals(fieldOfView.trim())) {
                                MipavUtil
                                        .displayError("Horizontal FOV for this volume does not match previous horizontal FOV");
                        return false;
                            }
                        }

                        if (vFOVTextField.getText().trim().equals("")) {
                            vFOVTextField.setText(fieldOfView);
                            vFOVTextField.setEditable(false);
                        } else {
                            if ( !vFOVTextField.getText().trim().equals(fieldOfView.trim())) {
                                MipavUtil
                                        .displayError("Vertical FOV for this volume does not match previous vertical FOV");
                        return false;
                            }
                        }

                    }

                    final String dir = ((String) (fileInfoDicom.getTagTable().getValue("0018,1312"))).trim();
                    String phaseEncodeDirection = "";

                    if (dir.equalsIgnoreCase("col")) {
                        phaseEncodeDirection = "vertical";
                    } else if (dir.equalsIgnoreCase("row")) {
                        phaseEncodeDirection = "horizontal";
                    }

                    if (phaseEncodingTextField.getText().trim().equals("")) {
                        phaseEncodingTextField.setText(phaseEncodeDirection);
                        phaseEncodingTextField.setEditable(false);
                    } else {
                        if ( !phaseEncodingTextField.getText().trim().equals(phaseEncodeDirection)) {
                            MipavUtil
                                    .displayError("Phase encoding orientaion for this volume does not match previous phase encoding orientation");
                    return false;
                        }
                    }

                } else {
                    final float sliceTh = srcImage.getFileInfo()[0].getSliceThickness();

                    if (sliceThicknessTextField.getText().trim().equals("")) {
                        sliceThicknessTextField.setText(String.valueOf(sliceTh));
                        sliceThicknessTextField.setEditable(false);
                    } else {
                        if ( !sliceThicknessTextField.getText().trim().equals(String.valueOf(sliceTh))) {
                            MipavUtil
                                    .displayError("Slice thickness for this volume does not match previous slice thickness");
                    return false;
                        }
                    }
                }

                if (numDims == 3) {
                    final int numSlices = srcImage.getExtents()[2];
                    if (numSlicesTextField.getText().trim().equals("")) {
                        numSlicesTextField.setText(String.valueOf(numSlices));
                        numSlicesTextField.setEditable(false);
                    } else {
                        if ( !numSlicesTextField.getText().trim().equals(String.valueOf(numSlices))) {
                            MipavUtil.displayError("Num slices for this volume does not match previous num slices");
                    return false;
                        }
                    }

                    if (isMultifile) {
                        final String firstImageSliceName = srcImage.getFileInfo(0).getFileName();
                        final ArrayList<String> slicesArrayList = new ArrayList<String>();
                        for (int i = 0; i < numSlices; i++) {
                            final String absPath = srcImage.getFileInfo(i).getFileDirectory()
                                    + srcImage.getFileInfo(i).getFileName();
                            // System.out.println(absPath);
                            slicesArrayList.add(absPath);
                        }
                        slicesVector.add(slicesArrayList);

                        // we will display the full path
                        final Vector<String> rowData = new Vector<String>();
                        rowData.add(firstImageSliceName + " - multifile");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");

                        srcTableModel.addRow(rowData);
                    } else {
                        // to do
                        final String imageAbsPath = srcImage.getFileInfo(0).getFileDirectory()
                                + srcImage.getFileInfo(0).getFileName();
                        final String imageName = srcImage.getFileInfo(0).getFileName();

                        final ArrayList<String> slicesArrayList = new ArrayList<String>();

                        for (int i = 0; i < numSlices; i++) {
                            final String slicePath = imageAbsPath + "_3D_numSlices_" + numSlices + "_slice_" + i;
                            // System.out.println(absPath);
                            slicesArrayList.add(slicePath);
                        }
                        slicesVector.add(slicesArrayList);

                        final Vector<String> rowData = new Vector<String>();
                        rowData.add(imageName);
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");

                        srcTableModel.addRow(rowData);

                    }

                    numVolumesTextField.setText(String.valueOf(srcTableModel.getRowCount()));
                    numVolumesTextField.setEditable(false);
                } else if (numDims == 4) {
                    final int numVolumes = srcImage.getExtents()[3];
                    final int numSlices = srcImage.getExtents()[2];

                    if (numSlicesTextField.getText().trim().equals("")) {
                        numSlicesTextField.setText(String.valueOf(numSlices));
                        numSlicesTextField.setEditable(false);
                    } else {
                        if ( !numSlicesTextField.getText().trim().equals(String.valueOf(numSlices))) {
                            MipavUtil.displayError("Num slices for this volume does not match previous num slices");
                    return false;
                        }
                    }

                    final String imageAbsPath = srcImage.getFileInfo(0).getFileDirectory()
                            + srcImage.getFileInfo(0).getFileName();
                    final String imageName = srcImage.getFileInfo(0).getFileName();

                    int i = 0;
                    for (i = 0; i < numVolumes; i++) {
                        final ArrayList<String> slicesArrayList = new ArrayList<String>();
                        for (int k = 0; k < numSlices; k++) {
                            final String slicePath = imageAbsPath + "_4D_numVols_" + numVolumes + "_numSlices_"
                                    + numSlices + "_vol_" + i + "_slice_" + k;
                            slicesArrayList.add(slicePath);
                        }

                        slicesVector.add(slicesArrayList);

                        final Vector<String> rowData = new Vector<String>();
                        rowData.add(imageName + "_vol_" + i);
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");
                        rowData.add("");

                        srcTableModel.addRow(rowData);

                    }

                    numVolumesTextField.setText(String.valueOf(srcTableModel.getRowCount()));
                    numVolumesTextField.setEditable(false);

                }

        return true;
            }




    /**
     * reads the bval/gradient file...both dti studio format and fsl format are accepted
     * 
     * @param gradientFilePath
     * @return
     */
    public boolean readBValGradientFile(final String gradientFilePath) {

        try {
            String str;
            final File file = new File(gradientFilePath);
            final RandomAccessFile raFile = new RandomAccessFile(file, "r");

            String firstLine = raFile.readLine();
            if (firstLine.contains(":")) {
                raFile.seek(0);
                // this is DTI Studio
                final int numRows = srcTableModel.getRowCount();
                System.out.println("numRows:"+numRows);

                for (int i = 0; i < numRows; i++) {
                    if ( ((String) srcTableModel.getValueAt(i, 2)).trim().equals("")) {
                        str = raFile.readLine();
                        System.out.println("str:"+str);
                        if (str != null) {
                            final String[] arr = str.split(":");
                            System.out.println("arr:"+arr);
                            if (arr.length == 2) {
                                final String grads = arr[1].trim();
                                final String[] arr2 = grads.split("\\s+");
                                System.out.println("arr2:"+arr2);
                                if ( arr2.length == 3 )
                                {
                                	srcTableModel.setValueAt(arr2[0], i, 2);
                                	srcTableModel.setValueAt(arr2[1], i, 3);
                                	srcTableModel.setValueAt(arr2[2], i, 4);
                                }
                                else if ( arr2.length == 4 )
                                {
                                	srcTableModel.setValueAt(arr2[0], i, 1);
                                	srcTableModel.setValueAt(arr2[1], i, 2);
                                	srcTableModel.setValueAt(arr2[2], i, 3);
                                	srcTableModel.setValueAt(arr2[3], i, 4);
                                }
                            }
                        }

                    }

                }

            } else {
                System.out.println("working6");
                final int numRows = srcTableModel.getRowCount();
                System.out.println("numRows:"+numRows);
                int start = 0;
                for (int i = 0; i < numRows; i++) {
                    if ( ((String) srcTableModel.getValueAt(i, 2)).trim().equals("")) {
                        start = i;
                        System.out.println("start:"+start);
                        break;
                    }
                }
                // this is FSL
                System.out.println("working7");
                firstLine = firstLine.trim();
                System.out.println("firstLine:"+firstLine);
                String[] arr = firstLine.split("\\s+");
                int k = start;

                for (final String element : arr) {
                    if (k < numRows) {
                        srcTableModel.setValueAt(element, k, 2);
                        k = k + 1;
                        System.out.println("k:"+k);
                    } else {
                        break;
                    }
                }

                k = start;
                String secondLine = raFile.readLine();
                secondLine = secondLine.trim();
                System.out.println("secondLine:"+secondLine);
                arr = secondLine.split("\\s+");
                for (final String element : arr) {
                    if (k < numRows) {
                        srcTableModel.setValueAt(element, k, 3);
                        k = k + 1;
                    } else {
                        break;
                    }
                }

                k = start;
                String thirdLine = raFile.readLine();
                thirdLine = thirdLine.trim();
                System.out.println("thirdLine:"+thirdLine);
                arr = thirdLine.split("\\s+");
                for (final String element : arr) {
                    if (k < numRows) {
                        srcTableModel.setValueAt(element, k, 4);
                        k = k + 1;
                    } else {
                        break;
                    }
                }

                k = start;
                String fourthLine = raFile.readLine();
                fourthLine = fourthLine.trim();
                System.out.println("fourthLine:"+fourthLine);
                arr = fourthLine.split("\\s+");
                for (final String element : arr) {
                    if (k < numRows) {
                        srcTableModel.setValueAt(element, k, 1);
                        k = k + 1;
                    } else {
                        break;
                    }
                }

            }

            raFile.close();
        } catch (final Exception e) {

            MipavUtil.displayError("Error reading B-Value/Grad File...DTI Studio and FSL formats are accepted");
            return false;
        }

        return true;
    }

    /**
     * create list file
     */
    private void createListFile() {

        try {
            listFile = new File(outputDirTextField.getText() + File.separator + "dti.list");
            final FileOutputStream outputStream = new FileOutputStream(listFile);
            final PrintStream printStream = new PrintStream(outputStream);

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
            printStream.println("<phase_encode_direction>" + phaseEncodingTextField.getText()
                    + "</phase_encode_direction>");
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
        } catch (final Exception e) {

        }

    }

    /**
     * create b-matrix file
     */
    private void createBMatrixFile() {
        try {
            StringBuffer sb;
            int padLength;
            final File bMatrixFile = new File(outputDirTextField.getText() + File.separator + "dti.BMTXT");
            final FileOutputStream outputStream = new FileOutputStream(bMatrixFile);
            final PrintStream printStream = new PrintStream(outputStream);

            final int numRows = srcTableModel.getRowCount();
            // formula for bmtxt values is :
            // bxx 2bxy 2bxz byy 2byz bzz
            for (int i = 0; i < numRows; i++) {
                final String bVal = ((String) srcTableModel.getValueAt(i, 1)).trim();
                final String xGrad = ((String) srcTableModel.getValueAt(i, 2)).trim();
                final String yGrad = ((String) srcTableModel.getValueAt(i, 3)).trim();
                final String zGrad = ((String) srcTableModel.getValueAt(i, 4)).trim();
                final float b = Float.valueOf(bVal).floatValue();
                final float x = Float.valueOf(xGrad).floatValue();
                final float y = Float.valueOf(yGrad).floatValue();
                final float z = Float.valueOf(zGrad).floatValue();

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
                final String _bxx_string = String.valueOf(_bxx);
                final int _bxx_stringLength = _bxx_string.length();
                sb = new StringBuffer(16);
                padLength = 16 - _bxx_stringLength;

                for (int j = 0; j < padLength; j++) {
                    sb.insert(j, " ");
                }

                sb.insert(padLength, _bxx_string);
                printStream.print(sb.toString());

                final String _2bxy_string = String.valueOf(_2bxy);
                final int _2bxy_stringLength = _2bxy_string.length();
                sb = new StringBuffer(16);
                padLength = 16 - _2bxy_stringLength;

                for (int j = 0; j < padLength; j++) {
                    sb.insert(j, " ");
                }

                sb.insert(padLength, _2bxy_string);
                printStream.print(sb.toString());

                final String _2bxz_string = String.valueOf(_2bxz);
                final int _2bxz_stringLength = _2bxz_string.length();
                sb = new StringBuffer(16);
                padLength = 16 - _2bxz_stringLength;

                for (int j = 0; j < padLength; j++) {
                    sb.insert(j, " ");
                }

                sb.insert(padLength, _2bxz_string);
                printStream.print(sb.toString());

                final String _byy_string = String.valueOf(_byy);
                final int _byy_stringLength = _byy_string.length();
                sb = new StringBuffer(16);
                padLength = 16 - _byy_stringLength;

                for (int j = 0; j < padLength; j++) {
                    sb.insert(j, " ");
                }

                sb.insert(padLength, _byy_string);
                printStream.print(sb.toString());

                final String _2byz_string = String.valueOf(_2byz);
                final int _2byz_stringLength = _2byz_string.length();
                sb = new StringBuffer(16);
                padLength = 16 - _2byz_stringLength;

                for (int j = 0; j < padLength; j++) {
                    sb.insert(j, " ");
                }

                sb.insert(padLength, _2byz_string);
                printStream.print(sb.toString());

                final String _bzz_string = String.valueOf(_bzz);
                final int _bzz_stringLength = _bzz_string.length();
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

        } catch (final Exception e) {

        }

    }

    /**
     * create path file
     */
    private void createPathFile() {

        final int numSlicesPer3DVolume = Integer.valueOf(numSlicesTextField.getText().trim()).intValue();
        try {
            final File pathFile = new File(outputDirTextField.getText() + File.separator + "dti.path");
            final FileOutputStream outputStream = new FileOutputStream(pathFile);
            final PrintStream printStream = new PrintStream(outputStream);

            for (int i = 0; i < numSlicesPer3DVolume; i++) {
                final Iterator<ArrayList<String>> iter = slicesVector.iterator();
                while (iter.hasNext()) {
                    final ArrayList<String> slicesList = (ArrayList<String>) iter.next();
                    final String path = (String) slicesList.get(i);
                    printStream.println(path);
                }

            }
            outputStream.close();

        } catch (final Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * read list file
     */
    private void readListFile() {
        m_kParentDir = listFile.getParent();

        String pathFilename = null;
        String pathFileAbsPath = null;

        String bMatrixFilename = null;
        String bMatrixFileAbsPath = null;
        try {
            BufferedReader kReader = new BufferedReader(new FileReader(listFile));
            String lineString = null;
            while ( (lineString = kReader.readLine()) != null) {
                if (lineString.startsWith("<original_columns>")) {
                    final String columnsStr = lineString.substring(lineString.indexOf("<original_columns>") + 18,
                            lineString.indexOf("</original_columns>")).trim();
                    m_iDimX = Integer.parseInt(columnsStr);
                } else if (lineString.startsWith("<original_rows>")) {
                    final String rowsStr = lineString.substring(lineString.indexOf("<original_rows>") + 15,
                            lineString.indexOf("</original_rows>")).trim();
                    m_iDimY = Integer.parseInt(rowsStr);
                } else if (lineString.startsWith("<slice>")) {
                    final String sliceStr = lineString.substring(lineString.indexOf("<slice>") + 7,
                            lineString.indexOf("</slice>")).trim();
                    m_iSlices = Integer.parseInt(sliceStr);
                } else if (lineString.startsWith("<nim>")) {
                    final String nimStr = lineString.substring(lineString.indexOf("<nim>") + 5,
                            lineString.indexOf("</nim>")).trim();
                    m_iWeights = Integer.parseInt(nimStr);
                } else if (lineString.startsWith("<rawimageformat>")) {
                    m_kRawFormat = lineString.substring(lineString.indexOf("<rawimageformat>") + 16,
                            lineString.indexOf("</rawimageformat>")).trim();
                } else if (lineString.startsWith("<raw_image_path_filename>")) {
                    pathFilename = lineString.substring(lineString.indexOf("<raw_image_path_filename>") + 25,
                            lineString.indexOf("</raw_image_path_filename>")).trim();
                    pathFileAbsPath = m_kParentDir + File.separator + pathFilename;
                    // studyName = pathFilename.substring(0,
                    // pathFilename.indexOf(".path"));
                } else if (lineString.startsWith("<bmatrixfile>")) {
                    bMatrixFilename = lineString.substring(lineString.indexOf("<bmatrixfile>") + 13,
                            lineString.indexOf("</bmatrixfile>")).trim();
                    bMatrixFileAbsPath = m_kParentDir + File.separator + bMatrixFilename;
                    // studyName = pathFilename.substring(0,
                    // pathFilename.indexOf(".path"));
                } else if (lineString.startsWith("<x_field_of_view>")) {
                    final String xFOVStr = lineString.substring(lineString.indexOf("<x_field_of_view>") + 17,
                            lineString.indexOf("</x_field_of_view>")).trim();
                    final float xFOV = Float.parseFloat(xFOVStr);
                    m_fResX = xFOV;
                  
                } else if (lineString.startsWith("<y_field_of_view>")) {
                    final String yFOVStr = lineString.substring(lineString.indexOf("<y_field_of_view>") + 17,
                            lineString.indexOf("</y_field_of_view>")).trim();
                    final float yFOV = Float.parseFloat(yFOVStr);
                    m_fResY = yFOV;
                   
                } else if (lineString.startsWith("<slice_thickness>")) {
                    final String zResStr = lineString.substring(lineString.indexOf("<slice_thickness>") + 17,
                            lineString.indexOf("</slice_thickness>")).trim();
                    m_fResZ = Float.parseFloat(zResStr);
                    
                } else if (lineString.startsWith("<noise_mean_ori>")) {
                    final String noiseStr = lineString.substring(lineString.indexOf("<noise_mean_ori>") + 16,
                            lineString.indexOf("</noise_mean_ori>")).trim();
                    m_fMeanNoise = Float.parseFloat(noiseStr);
                }
            }
            kReader.close();
            kReader = null;
        } catch (final Exception e) {
            e.printStackTrace();
        }

        if (pathFilename != null) {
            readPathFile(pathFileAbsPath);
        }
        if (bMatrixFileAbsPath != null) {
            readBMatrixFile(bMatrixFileAbsPath);
        }
        m_fResX /= m_iDimX;
        m_fResY /= m_iDimY;
    }

    /**
     * Loads the .path file.
     * 
     * @param kFileName path file name.
     * @param kPathName parent directory.
     */
    public void readPathFile(final String kFileName) {
        final File kFile = new File(kFileName);
        if ( !kFile.exists() || !kFile.canRead()) {
            return;
        }
        final int iLength = (int) kFile.length();
        if (iLength <= 0) {
            return;
        }
        m_aakDWIList = new String[m_iSlices][m_iWeights];
        try {
            final BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;
            for (int i = 0; i < m_iSlices; i++) {
                for (int j = 0; j < m_iWeights; j++) {
                    str = in.readLine();
                    // m_aakDWIList[i][j] = new String("blah" + File.separator
                    // + str);
                    m_aakDWIList[i][j] = str;
                }
            }
            in.close();
        } catch (final IOException e) {}
    }

    /**
     * Loads the BMatrix file.
     * 
     * @param kFileName name of BMatrix file.
     */
    private void readBMatrixFile(final String kFileName) {
        final File kFile = new File(kFileName);
        if ( !kFile.exists() || !kFile.canRead()) {
            return;
        }
        final int iLength = (int) kFile.length();
        if (iLength <= 0) {
            return;
        }

        try {
            final BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;

            m_kBMatrix = new GMatrixf(m_iWeights, 6 + 1);

            final String[] kBMatrixString = new String[m_iWeights];
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
                if ( !gotit) {
                    kBMatrixString[nb] = str;
                    m_aiMatrixEntries[iRow] = nb;
                    nb = nb + 1;
                }

                final java.util.StringTokenizer st = new java.util.StringTokenizer(str);
                for (int iCol = 0; iCol < 6; iCol++) {
                    final float fValue = Float.valueOf(st.nextToken()).floatValue();
                    m_kBMatrix.Set(iRow, iCol, fValue);
                }
                m_kBMatrix.Set(iRow, 6, 1f);
            }
            in.close();

            m_iBOrig = nb;

        } catch (final Exception e) {
            MipavUtil.displayError("Error reading B-Matrix File");
            return;
        }
    }

    /** Calls AlgorithmDWI2DTI to create the diffusion tensor image. */
    private void processDWI() {
        if (m_kBMatrix == null) {
            MipavUtil.displayError("BMatrix file must be set to create tensor data.");
            return;
        }
        if (m_aakDWIList == null) {
            MipavUtil.displayError("Path file must be set to create tensor data.");
            return;
        }

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        kAlgorithm = new AlgorithmDWI2DTI(maskImage, false, m_iSlices, m_iDimX, m_iDimY, m_iBOrig, m_iWeights,
                m_fMeanNoise, m_aakDWIList, m_aiMatrixEntries, m_kBMatrix, m_kRawFormat);
        kAlgorithm.addListener(this);
        kAlgorithm.run();

    }

    /**
     * algorithm performed
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (kAlgorithm.isCompleted()) {
            DTI = (kAlgorithm).getDTI();

            float[] buffer;

            // determine length of dec image
            final int length = DTI.getExtents()[0] * DTI.getExtents()[1] * DTI.getExtents()[2]
                    * DTI.getExtents()[3];

            buffer = new float[length];

            try {
                DTI.exportData(0, length, buffer);
            } catch (final IOException error) {
                System.out.println("IO exception");
                return;
            }

            // hack to trim low and high values
            for (int i = 0; i < buffer.length; i++) {
                final float val = buffer[i];
                if (val < 0) {
                    buffer[i] = 0;
                } else if (val > 5000) {
                    buffer[i] = 5000;
                }
            }

            try {
                DTI.importData(0, buffer, true);
            } catch (final IOException error) {
                System.out.println("IO exception");

                return;
            }
            // end hack

            // change to power of two
            final int[] extents = DTI.getExtents();
            final float[] res = DTI.getFileInfo(0).getResolutions();
            final float[] saveRes = new float[] {res[0], res[1], res[2], res[3]};

            final float[] newRes = new float[extents.length];
            final int[] volExtents = new int[extents.length];
            boolean originalVolPowerOfTwo = true;
            int volSize = 1;
            for (int i = 0; i < extents.length; i++) {
                volExtents[i] = MipavMath.dimPowerOfTwo(extents[i]);
                volSize *= volExtents[i];

                if ( (i < 3) && volExtents[i] != extents[i]) {
                    originalVolPowerOfTwo = false;
                }
                newRes[i] = (res[i] * (extents[i])) / (volExtents[i]);
                saveRes[i] = (saveRes[i] * (extents[i])) / (volExtents[i]);
            }

            if ( !originalVolPowerOfTwo) {
                AlgorithmTransform transformFunct = new AlgorithmTransform(DTI, new TransMatrix(4),
                        AlgorithmTransform.TRILINEAR, newRes[0], newRes[1], newRes[2], volExtents[0], volExtents[1],
                        volExtents[2], false, true, false);
                transformFunct.setRunningInSeparateThread(false);
                transformFunct.run();

                if (transformFunct.isCompleted() == false) {
                    transformFunct.finalize();
                    transformFunct = null;
                }

                final ModelImage kDTIScaled = transformFunct.getTransformedImage();
                kDTIScaled.calcMinMax();

                /*
                 * transformFunct.disposeLocal(); transformFunct = null;
                 */

                DTI.disposeLocal();
                DTI = null;
                DTI = transformFunct.getTransformedImage();
                DTI.calcMinMax();

                /*
                 * for ( int i = 0; i < DTI.getFileInfo().length; i++ ) {
                 * DTI.getFileInfo(i).setResolutions(saveRes);
                 * DTI.getFileInfo(i).setSliceThickness(saveRes[2]); }
                 */

            }

            DTI.saveImage(outputDirTextField.getText() + File.separator, "DTI.xml", FileUtility.XML, false);
            MipavUtil.displayInfo("Tensor image saved as " + outputDirTextField.getText() + File.separator
                    + "DTI.xml");
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            if (maskImage != null) {
                maskImage.disposeLocal();
                maskImage = null;
            }

        }
    }

    /**
     * validate data
     * 
     * @return
     */
    private boolean validateData() {
        final boolean isValid = true;
        final int numRows = srcTableModel.getRowCount();
        if (numRows == 0) {
            MipavUtil.displayError("DWI data is required");
            return false;
        }
        for (int i = 0; i < numRows; i++) {
            final String bValString = ((String) srcTableModel.getValueAt(i, 1)).trim();
            final String xgradString = ((String) srcTableModel.getValueAt(i, 2)).trim();
            final String ygradString = ((String) srcTableModel.getValueAt(i, 3)).trim();
            final String zgradString = ((String) srcTableModel.getValueAt(i, 4)).trim();
            if (bValString.equals("") || xgradString.equals("") || ygradString.equals("") || zgradString.equals("")) {
                MipavUtil.displayError("B-Values and Gradient Direction Inputs are required");
                return false;
            }
            try {
                Float.valueOf(bValString);
            } catch (final NumberFormatException e) {
                MipavUtil.displayError("B-Value and Gradient Direction Inputs must be valid numbers");
                return false;
            }
            try {
                Float.valueOf(xgradString);
            } catch (final NumberFormatException e) {
                MipavUtil.displayError("B-Value and Gradient Direction Inputs must be valid numbers");
                return false;
            }
            try {
                Float.valueOf(ygradString);
            } catch (final NumberFormatException e) {
                MipavUtil.displayError("B-Value and Gradient Direction Inputs must be valid numbers");
                return false;
            }
            try {
                Float.valueOf(zgradString);
            } catch (final NumberFormatException e) {
                MipavUtil.displayError("B-Value and Gradient Direction Inputs must be valid numbers");
                return false;
            }
        }

        final String maskImageString = maskImageTextField.getText().trim();
        final String xdimString = xdimTextField.getText().trim();
        final String ydimString = ydimTextField.getText().trim();
        final String numSlicesString = numSlicesTextField.getText().trim();
        final String numVolsString = numVolumesTextField.getText().trim();
        final String hFOVString = hFOVTextField.getText().trim();
        final String vFOVString = vFOVTextField.getText().trim();
        final String outputDirString = outputDirTextField.getText().trim();
        final String formatString = formatTextField.getText().trim();
        final String gapString = gapTextField.getText().trim();
        final String sliceThicknessString = sliceThicknessTextField.getText().trim();
        final String imagePlaneString = imagePlaneTextField.getText().trim();
        final String phaseEncodingString = phaseEncodingTextField.getText().trim();

        if (maskImageString.equals("") || xdimString.equals("") || ydimString.equals("") || numSlicesString.equals("")
                || numVolsString.equals("") || hFOVString.equals("") || vFOVString.equals("")
                || outputDirString.equals("") || formatString.equals("") || gapString.equals("")
                || sliceThicknessString.equals("") || imagePlaneString.equals("") || phaseEncodingString.equals("")) {
            MipavUtil.displayError("One or more required study parameters is missing");
            return false;
        }

        try {
            Integer.valueOf(xdimString);
        } catch (final NumberFormatException e) {
            MipavUtil.displayError("Image X Dimension must be a valid number");
            return false;
        }

        try {
            Integer.valueOf(ydimString);
        } catch (final NumberFormatException e) {
            MipavUtil.displayError("Image Y Dimension must be a valid number");
            return false;
        }

        try {
            Integer.valueOf(numSlicesString);
        } catch (final NumberFormatException e) {
            MipavUtil.displayError("Num Slices per 3D Volume must be a valid number");
            return false;
        }

        try {
            Integer.valueOf(numVolsString);
        } catch (final NumberFormatException e) {
            MipavUtil.displayError("Num 3D Volumes must be a valid number");
            return false;
        }

        if ( ! (phaseEncodingString.equalsIgnoreCase("vertical") || phaseEncodingString.equalsIgnoreCase("horizontal"))) {
            MipavUtil.displayError("Origin of Phase Encoding is not valid");
            return false;
        }

        if ( ! (imagePlaneString.equalsIgnoreCase("axial") || imagePlaneString.equalsIgnoreCase("coronal") || imagePlaneString
                .equalsIgnoreCase("saggittal"))) {
            MipavUtil.displayError("Image plane is not valid");
            return false;
        }

        if ( ! (formatString.equalsIgnoreCase("integer") || formatString.equalsIgnoreCase("float") || formatString
                .equalsIgnoreCase("dicom"))) {
            MipavUtil.displayError("Format of raw images is not valid");
            return false;
        }

        return isValid;
    }

    /**
     * window closing
     */
    public void windowClosing(final WindowEvent event) {
        super.windowClosing(event);
        if (maskImage != null) {
            maskImage.disposeLocal();
            maskImage = null;
        }
    }

}
