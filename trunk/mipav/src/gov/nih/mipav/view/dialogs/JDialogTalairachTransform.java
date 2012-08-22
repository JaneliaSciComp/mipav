package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.algorithms.*;


import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.io.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;
 /** 
*   
*   Interface for the TalairachTransform plugin.
*
*
*	@version    July 2002
*	@author     Pierre-Louis Bazin
*	@see		TalairachTransformInfo
*	@see		AlgorithmTalairachTransform
*	@see		JDialogACPC
*	@see		JDialogTLRC
*
*/  
public class JDialogTalairachTransform extends JDialogBase implements AlgorithmInterface {
    
	// algorithm 
    private AlgorithmTalairachTransform algo = null;
    // acpc dialog
	private JDialogACPC	acpcDialog = null;
	// source image : contains the transform
    private ModelImage image;    
    // second source image : contain the data to be changed
    private ModelImage otherImage = null;      
    //result image
    private ModelImage resultImage = null;	
    // acpc image
    private ModelImage acpcImage = null;		
    // Talairach image
    private ModelImage  tlrcImage = null;	
    private ViewUserInterface userInterface;
	private String title;
	// algorithm parameters
	private String transformType;
	private	TalairachTransformInfo transform;
	private	String interpolation;
    // dialog elements
    private JPanel  		talairachPanel;
	private JPanel  		newImagePanel;
	private	JButton			computeACPC;
	private	JButton			computeTLRC;
	private	JButton			computeImage;
	private	JLabel			labelImage;
    private JComboBox       comboBoxImage;
	private JButton			updateImage;
	private JLabel			labelTransform;
    private JComboBox       comboBoxTransform;
	private JLabel			labelInterpolation;
    private JComboBox       comboBoxInterpolation;
	private	JLabel			loadsaveLabel;
	private	JButton			loadButton;
	private	JButton			saveButton;
	private	JPanel			botPanel;
	private	JPanel			imgPanel;
	private	JPanel			transPanel;
	private	JPanel			interpPanel;
	private	JPanel			loadsavePanel;
	private	JFileChooser	loadDialog;
	private	JFileChooser	saveDialog;
	private JLabel          acpcResLabel;
	private JTextField      acpcResText;
	private JPanel          acpcResPanel;
	private float           acpcRes = 1.0f;
	private float           tlrcRes[];
	private float           acpcResOrg;
	private Vector3f        acpcPC;
	
	/**
    *  Creates dialog for plugin.
    *  @param theParentFrame          Parent frame.
    *  @param im              Source image.
    */
    public JDialogTalairachTransform(Frame theParentFrame, ModelImage im) {
		super(theParentFrame, false);
        image = im;
		userInterface = ViewUserInterface.getReference();	    
		transform = image.getTalairachTransformInfo();
		if (transform==null)
			transform = new TalairachTransformInfo();
		init();
	}
	
    /**
    *	Used primarily for the script to store variables and run the algorithm.  No
    *	actual dialog will appear but the set up info and result image will be stored here.
    *	@param UI   The user interface, needed to create the image frame.
    *	@param im	Source image.
    */
    public JDialogTalairachTransform(ViewUserInterface UI, ModelImage im) {
        super();
    	userInterface = UI;
    	image = im;
		transform = image.getTalairachTransformInfo();
		if (transform==null)
			transform = new TalairachTransformInfo();
    }
    
    /**
    *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
    */
	private void init(){
        setForeground(Color.black);
        setTitle("Talairach Transform");
        GridBagConstraints gbc = new GridBagConstraints();
 				      
        talairachPanel = new JPanel(new GridBagLayout());
        talairachPanel.setForeground(Color.black);
        talairachPanel.setBorder(buildTitledBorder("Define Talairach alignment"));
		
		newImagePanel = new JPanel(new GridBagLayout());
        newImagePanel.setForeground(Color.black);
        newImagePanel.setBorder(buildTitledBorder("Transform new images"));
		
		computeACPC = new JButton("ACPC");
		computeACPC.setForeground(Color.black);
        computeACPC.setFont(serif12);
        computeACPC.addActionListener(this);
        computeACPC.setActionCommand("ACPC");
		computeACPC.setToolTipText("Gather information to transform the image into AC-PC coordinates");       

		computeTLRC = new JButton("Talairach");
		computeTLRC.setForeground(Color.black);
        computeTLRC.setFont(serif12);
        computeTLRC.addActionListener(this);
        computeTLRC.setActionCommand("TLRC");
		computeTLRC.setToolTipText("Gather information to transform the image into Talairach coordinates");       
                
		loadsaveLabel = new JLabel("Transform information");
        loadsaveLabel.setForeground(Color.black);
        loadsaveLabel.setFont(serif12);
		
		loadButton = new JButton("Load");
		loadButton.setForeground(Color.black);
        loadButton.setFont(serif12);
        loadButton.addActionListener(this);
        loadButton.setActionCommand("Load");
		loadButton.setToolTipText("Load the transform information from a text file");
        
		saveButton = new JButton("Save");
		saveButton.setForeground(Color.black);
        saveButton.setFont(serif12);
        saveButton.addActionListener(this);
        saveButton.setActionCommand("Save");
		saveButton.setToolTipText("Save the transform information to a text file");
		
		//buildLoadDialog();
		//buildSaveDialog();
		
		loadsavePanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        loadsavePanel.add(loadButton, gbc);
        gbc.gridx = 1;
        loadsavePanel.add(saveButton, gbc);
		
		labelImage = new JLabel("New image to transform: ");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);

        buildComboBoxImage();
        
		//updateImage = new JButton(MedicUtil.getIcon("refresh.png"));
		updateImage = new JButton(MipavUtil.getIcon("refresh.gif"));
		updateImage.setForeground(Color.black);
        updateImage.setFont(serif12);
        updateImage.addActionListener(this);
        updateImage.setActionCommand("Select");
        updateImage.setBorderPainted( false );
        updateImage.setRolloverEnabled( true );
        updateImage.setRolloverIcon( MipavUtil.getIcon( "refreshroll.gif" ) );
        updateImage.setFocusPainted( false );
        updateImage.setToolTipText("Refresh image list");

		imgPanel = new JPanel(new GridBagLayout());
        gbc.insets = new Insets(0, 0, 0, 0);
		gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        imgPanel.add(comboBoxImage, gbc);
        gbc.gridx = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
		imgPanel.add(updateImage, gbc);

		labelTransform = new JLabel("transformation: ");
        labelTransform.setForeground(Color.black);
        labelTransform.setFont(serif12);
                
		String[] val = {"orig to acpc", "orig to Tlrc", "acpc to Tlrc", "Tlrc to acpc", "Tlrc to orig", "acpc to orig"};
		comboBoxTransform = new JComboBox(val);
		comboBoxTransform.setFont(serif12);
        comboBoxTransform.setBackground(Color.white);
        comboBoxTransform.addActionListener(this);
        comboBoxTransform.setActionCommand("ChangeTransformType");

		transPanel = new JPanel();
		transPanel.add(labelTransform);
        transPanel.add(comboBoxTransform);
        
        acpcResLabel = new JLabel("acpc resolution (mm): ");
        acpcResLabel.setForeground(Color.black);
        acpcResLabel.setFont(serif12);
        
        acpcResText = new JTextField(10);
        acpcResText.setText("1.0");
        acpcResText.setForeground(Color.black);
        acpcResText.setFont(serif12);
        
        acpcResPanel = new JPanel();
        acpcResPanel.add(acpcResLabel);
        acpcResPanel.add(acpcResText);

		labelInterpolation = new JLabel("interpolation: ");
        labelInterpolation.setForeground(Color.black);
        labelInterpolation.setFont(serif12);
                
		String[] interp = {"Nearest Neighbor", "Trilinear", "Bspline 3rd order", "Bspline 4th order", "Cubic Lagrangian", "Quintic Lagrangian", "Heptic Lagrangian", "Windowed sinc"};
		comboBoxInterpolation = new JComboBox(interp);
		comboBoxInterpolation.setFont(serif12);
        comboBoxInterpolation.setBackground(Color.white);
		comboBoxInterpolation.setSelectedItem("Trilinear");

		interpPanel = new JPanel();
		interpPanel.add(labelInterpolation);
        interpPanel.add(comboBoxInterpolation);
        
        
		
		computeImage = new JButton("Compute");
		computeImage.setForeground(Color.black);
        computeImage.setFont(serif12);
        computeImage.addActionListener(this);
        computeImage.setActionCommand("Image");
		computeImage.setToolTipText("Transform the new image according to the chosen transformation");       

        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.CENTER;
		gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        talairachPanel.add(computeACPC, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        talairachPanel.add(computeTLRC, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        talairachPanel.add(loadsaveLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        talairachPanel.add(loadsavePanel, gbc);
		
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        newImagePanel.add(labelImage, gbc);
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        newImagePanel.add(imgPanel, gbc);
        gbc.gridy = 6;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        newImagePanel.add(transPanel, gbc);
        gbc.gridy = 7;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        newImagePanel.add(acpcResPanel, gbc);
        gbc.gridy = 8;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        newImagePanel.add(interpPanel, gbc);
        gbc.gridy = 9;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        newImagePanel.add(computeImage, gbc);
		         
		botPanel = new JPanel();
		botPanel.add(buildCloseButton());
        botPanel.add(buildHelpButton());
		
        getContentPane().add(talairachPanel, BorderLayout.NORTH);
        getContentPane().add(newImagePanel, BorderLayout.CENTER);
        getContentPane().add(botPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true); 
    	System.gc();
	
	} // end init()
	
    private void buildComboBoxImage() {
        
        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);

        Enumeration<String> names = userInterface.getRegisteredImageNames();

        // Add images from user interface that have the same exact dimensionality
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            ModelImage img = userInterface.getRegisteredImageByName(name);
			if (userInterface.getFrameContainingImage(img) != null) {
				if ( (image.getNDims() == img.getNDims()) && (!img.isColorImage()) ) {
					comboBoxImage.addItem(name);
				}
            }
        }
    }//buildComboBoxImage
    
    private void updateComboBoxImage() {
       
        comboBoxImage.removeAllItems();
        
        Enumeration<String> names = userInterface.getRegisteredImageNames();

        // Add images from user interface that have the same exact dimensionality
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            ModelImage img = userInterface.getRegisteredImageByName(name);
			if (userInterface.getFrameContainingImage(img) != null) {
				if ( (image.getNDims() == img.getNDims()) && (!img.isColorImage()) ) {
					comboBoxImage.addItem(name);
				}
            }
        }
    }//buildComboBoxImage
    
	private void buildLoadDialog() {
		loadDialog = new javax.swing.JFileChooser();
        loadDialog.setDialogTitle("Load Talairach Transform file");
        loadDialog.setDialogType(JFileChooser.OPEN_DIALOG);
        loadDialog.setMaximumSize(new java.awt.Dimension(500, 326));
        loadDialog.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadFileActionPerformed(evt);
            }
        });
        
		if (userInterface.getDefaultDirectory() != null) {
			loadDialog.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
        } else {
			loadDialog.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

		loadDialog.setFileSelectionMode(JFileChooser.FILES_ONLY);
        loadDialog.showOpenDialog(this);
	}
	private void buildSaveDialog() {
        saveDialog = new javax.swing.JFileChooser();
        saveDialog.setDialogTitle("Save Talairach Transform file");
        saveDialog.setDialogType(JFileChooser.SAVE_DIALOG);
        saveDialog.setMaximumSize(new java.awt.Dimension(500, 326));
        saveDialog.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveFileActionPerformed(evt);
            }
        });
        
		if (userInterface.getDefaultDirectory() != null) {
			saveDialog.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
        } else {
			saveDialog.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

		saveDialog.setFileSelectionMode(JFileChooser.FILES_ONLY);
		saveDialog.setSelectedFile(new File(saveDialog.getCurrentDirectory(),image.getImageName()+"_talairach_info.txt"));
        saveDialog.showSaveDialog(this);
	}
	
	public void dispose() {
		algo = null;
		acpcDialog = null;
    	image = null;        
    	otherImage = null;  
    	resultImage = null;	
    	acpcImage = null;	
    	tlrcImage = null;		
		transform = null;
		super.dispose();	
	}
	
	private void transformToACPC() {
        ViewJFrameTriImage view;
        
		// get the triplanar view and the ACPC dialog
		ViewJComponentEditImage edit = image.getParentFrame().getComponentImage();
        view = new ViewJFrameTriImage(edit.getImageA(), edit.getLUTa(),
                                            edit.getImageB(), edit.getLUTb(), 
                                            image.getParentFrame().getControls(), image.getParentFrame() );

		view.setVisible(true);
		
		interpolation = (String)comboBoxInterpolation.getSelectedItem();
		int interpolationID = -1;
		
		if (interpolation.equals("Nearest Neighbor")) {
			interpolationID = AlgorithmTalairachTransform.NEAREST_NEIGHBOR;
		} else if (interpolation.equals("Bilinear")) {
			interpolationID = AlgorithmTalairachTransform.BILINEAR;
		} else if (interpolation.equals("Trilinear")) {
			interpolationID = AlgorithmTalairachTransform.TRILINEAR;
		} else if (interpolation.equals("Bspline 3rd order")) {
			interpolationID = AlgorithmTalairachTransform.BSPLINE3;
		} else if (interpolation.equals("Bspline 4th order")) {
			interpolationID = AlgorithmTalairachTransform.BSPLINE4;
		} else if (interpolation.equals("Cubic Lagrangian")) {
			interpolationID = AlgorithmTalairachTransform.CUBIC_LAGRANGIAN;
		} else if (interpolation.equals("Quintic Lagrangian")) {
			interpolationID = AlgorithmTalairachTransform.QUINTIC_LAGRANGIAN;
		} else if (interpolation.equals("Heptic Lagrangian")) {
			interpolationID = AlgorithmTalairachTransform.HEPTIC_LAGRANGIAN;
		} else if (interpolation.equals("Windowed sinc")) {
			interpolationID = AlgorithmTalairachTransform.WSINC;
		}        
		if (acpcDialog!=null) {
			acpcImage = acpcDialog.getACPCImage();
			acpcDialog.dispose();
		}
		acpcDialog = new JDialogACPC(view, image, acpcImage, transform, interpolationID);
	}
	
	
	private void transformToTLRC() {
        ViewJFrameTriImage view;
       
		// check if a transform is associated with the image
		if (image.getTalairachTransformInfo()!=null) {
			transform = image.getTalairachTransformInfo();
		}
		
		if (!transform.isAcpc()) {
			MipavUtil.displayError( "The ACPC transformation has not been properly set");
			System.gc();
			return;
		}
		if (acpcDialog!=null) {
			acpcImage = acpcDialog.getACPCImage();
		} else {
			transformToACPC();
			if (acpcDialog.convertToACPC()) {
				acpcDialog.setVisible(false);
				acpcDialog.dispose();
				acpcImage = acpcDialog.getACPCImage();
			}
		}
		if (acpcImage == null) {
            MipavUtil.displayError( "ACPC image not found. Please create it first.");
			System.gc();
			return;
		}
        // if the image was closed: it is re-generated
		if (acpcImage.getParentFrame()==null) {
			transformToACPC();
			if (acpcDialog.convertToACPC()) {
				acpcDialog.setVisible(false);
				acpcDialog.dispose();
				acpcImage = acpcDialog.getACPCImage();
			}
		}		
		
		interpolation = (String)comboBoxInterpolation.getSelectedItem();
		int interpolationID = -1;
		if (interpolation.equals("Nearest Neighbor")) {
			interpolationID = AlgorithmTalairachTransform.NEAREST_NEIGHBOR;
		} else if (interpolation.equals("Bilinear")) {
			interpolationID = AlgorithmTalairachTransform.BILINEAR;
		} else if (interpolation.equals("Trilinear")) {
			interpolationID = AlgorithmTalairachTransform.TRILINEAR;
		} else if (interpolation.equals("Bspline 3rd order")) {
			interpolationID = AlgorithmTalairachTransform.BSPLINE3;
		} else if (interpolation.equals("Bspline 4th order")) {
			interpolationID = AlgorithmTalairachTransform.BSPLINE4;
		} else if (interpolation.equals("Cubic Lagrangian")) {
			interpolationID = AlgorithmTalairachTransform.CUBIC_LAGRANGIAN;
		} else if (interpolation.equals("Quintic Lagrangian")) {
			interpolationID = AlgorithmTalairachTransform.QUINTIC_LAGRANGIAN;
		} else if (interpolation.equals("Heptic Lagrangian")) {
			interpolationID = AlgorithmTalairachTransform.HEPTIC_LAGRANGIAN;
		} else if (interpolation.equals("Windowed sinc")) {
			interpolationID = AlgorithmTalairachTransform.WSINC;
		}        
		
		// get the triplanar view and the TLRC dialog
		ViewJComponentEditImage edit = acpcImage.getParentFrame().getComponentImage();
        view = new ViewJFrameTriImage(edit.getImageA(), edit.getLUTa(), 
                                            edit.getImageB(), edit.getLUTb(), 
                                            acpcImage.getParentFrame().getControls(), image.getParentFrame() );
		view.setVisible(true);
		new JDialogTLRC(view, image, acpcImage, tlrcImage, transform, interpolationID);
	}
	
	
	
	private void transformImage() {

		// check if a transform is associated with the image
		if (image.getTalairachTransformInfo()!=null) {
			transform = image.getTalairachTransformInfo();
		}
		transformType = (String)comboBoxTransform.getSelectedItem();
        
        if (!transform.isAcpc()) {
			System.gc();
			MipavUtil.displayError( "The ACPC transformation is not set");
			return;
		} else if (!transform.isTlrc()) {
			if ( (!transformType.equals("acpc to orig")) && (!transformType.equals("orig to acpc"))) {
				System.gc();
				MipavUtil.displayError( "The Talairach transformation is not set");
				return;
			}
		}
		
        // retrieve parameters
        String selectedName = (String) comboBoxImage.getSelectedItem();
        otherImage = userInterface.getRegisteredImageByName(selectedName);
        interpolation = (String)comboBoxInterpolation.getSelectedItem();
        		
		if (otherImage==null) {	
			System.gc();
			MipavUtil.displayError( "image not found");
			return;
		}
		
		if ((transformType.equals("acpc to orig")) || (transformType.equals("acpc to Tlrc")) || 
		    (transformType.equals("Tlrc to orig")) || (transformType.equals("Tlrc to acpc"))) {
			acpcRes = otherImage.getFileInfo()[0].getResolution(0);
			transform.setAcpcRes(acpcRes);
		}
		else {
			String tmpStr = acpcResText.getText();
			if (testParameter(tmpStr, 0.1, 2.0)) {
				acpcResOrg = transform.getAcpcRes();
	            acpcRes = Float.valueOf(tmpStr).floatValue();
	            transform.setAcpcRes(acpcRes);
	            if (transform.isTlrc()) {
	            	tlrcRes = transform.getTlrcRes();
	            	for (int i = 0; i < tlrcRes.length; i++) {
	            		tlrcRes[i] = tlrcRes[i] * acpcRes / acpcResOrg;
	            	}
	            	transform.setTlrcRes(tlrcRes);
	            	acpcPC = transform.getAcpcPC();
	            	acpcPC.X = acpcPC.X * acpcResOrg / acpcRes;
	            	acpcPC.Y = acpcPC.Y * acpcResOrg / acpcRes;
	            	acpcPC.Z = acpcPC.Z * acpcResOrg / acpcRes;
	            	transform.setAcpcPC(acpcPC);
	            }
 			}
			else {
				 MipavUtil.displayError("acpc resolution must be between 0.1 and 2.0");
	             acpcResText.requestFocus();
	             acpcResText.selectAll();
	             return;
			}
		}
        int[] dims = null;
		String suffix = "";
		int transformID=-1;
		Vector3f currentOrigin = new Vector3f(image.getOrigin()); 
		Vector3f newOrigin = new Vector3f();
		int[] newOrientations = new int[3];

		if (transformType.equals("acpc to orig")) 	   { 
			dims = transform.getOrigDim(); 
			suffix = "_orig"; 
			transformID = AlgorithmTalairachTransform.ACPC_TO_ORIG;
			transform.acpcToOrig(currentOrigin, newOrigin);
			newOrientations = transform.getOrigOrientLabelsInverse();
		} else if (transformType.equals("Tlrc to orig")) { 
			dims = transform.getOrigDim(); 
			suffix = "_orig"; 
			transformID = AlgorithmTalairachTransform.TLRC_TO_ORIG;
			transform.tlrcToOrig(currentOrigin, newOrigin);
			newOrientations = transform.getOrigOrientLabelsInverse();
		} else if (transformType.equals("orig to acpc")) { 
			dims = transform.getAcpcDim(); 
			suffix = "_acpc"; 
			transformID = AlgorithmTalairachTransform.ORIG_TO_ACPC;
			transform.origToAcpc(currentOrigin, newOrigin);
		} else if (transformType.equals("Tlrc to acpc")) { 
			dims = transform.getAcpcDim(); 
			suffix = "_acpc"; 
			transformID = AlgorithmTalairachTransform.TLRC_TO_ACPC;
			transform.tlrcToAcpc(currentOrigin, newOrigin);
		} else if (transformType.equals("orig to Tlrc")) { 
			dims = transform.getTlrcDim();
			suffix = "_Tlrc"; 
			transformID = AlgorithmTalairachTransform.ORIG_TO_TLRC;
			transform.origToTlrc(currentOrigin, newOrigin);
		} else if (transformType.equals("acpc to Tlrc")) { 
			dims = transform.getTlrcDim(); 
			suffix = "_Tlrc"; 
			transformID = AlgorithmTalairachTransform.ACPC_TO_TLRC;
			transform.acpcToTlrc(currentOrigin, newOrigin);
		}
		int interpolationID = -1;
		if (interpolation.equals("Nearest Neighbor")) {
			interpolationID = AlgorithmTalairachTransform.NEAREST_NEIGHBOR;
		} else if (interpolation.equals("Bilinear")) {
			interpolationID = AlgorithmTalairachTransform.BILINEAR;
		} else if (interpolation.equals("Trilinear")) {
			interpolationID = AlgorithmTalairachTransform.TRILINEAR;
		} else if (interpolation.equals("Bspline 3rd order")) {
			interpolationID = AlgorithmTalairachTransform.BSPLINE3;
		} else if (interpolation.equals("Bspline 4th order")) {
			interpolationID = AlgorithmTalairachTransform.BSPLINE4;
		} else if (interpolation.equals("Cubic Lagrangian")) {
			interpolationID = AlgorithmTalairachTransform.CUBIC_LAGRANGIAN;
		} else if (interpolation.equals("Quintic Lagrangian")) {
			interpolationID = AlgorithmTalairachTransform.QUINTIC_LAGRANGIAN;
		} else if (interpolation.equals("Heptic Lagrangian")) {
			interpolationID = AlgorithmTalairachTransform.HEPTIC_LAGRANGIAN;
		} else if (interpolation.equals("Windowed sinc")) {
			interpolationID = AlgorithmTalairachTransform.WSINC;
		}
		float[] newOriginFloatArray = {newOrigin.X,newOrigin.Y,newOrigin.Z};
		try {
			resultImage = new ModelImage(ModelImage.FLOAT, dims, makeImageName(otherImage.getImageName(), suffix));
				
			FileInfoBase[] fileInfo = new FileInfoBase[dims[2]];
			for (int i=0;i<fileInfo.length;i++) 
				fileInfo[i] = (FileInfoBase)otherImage.getFileInfo()[0].clone();
				
			// set the proper file info
			if ( (transformType.equals("acpc to orig")) || (transformType.equals("Tlrc to orig")) ) {
				resultImage.setImageOrientation(transform.getOrigImageOrientLabel());
				
				for (int i=0;i<fileInfo.length;i++) {
					fileInfo[i].setUnitsOfMeasure(image.getFileInfo(0).getUnitsOfMeasure());
					fileInfo[i].setResolutions(transform.getOrigRes());
					fileInfo[i].setExtents(transform.getOrigDim());
                    fileInfo[i].setOrigin(newOriginFloatArray);
					fileInfo[i].setAxisOrientation(newOrientations);
					fileInfo[i].setImageOrientation(transform.getOrigImageOrientLabel());
				}
			} else {
				resultImage.setImageOrientation(FileInfoBase.AXIAL);
				
				int[] units = new int[3];
				units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();
				float[] resol = new float[3];
				
				resol[0] = resol[1] = resol[2] = acpcRes;
				int[] orient = new int[3];
				orient[0] = FileInfoBase.ORI_R2L_TYPE;
				orient[1] = FileInfoBase.ORI_A2P_TYPE;
				orient[2] = FileInfoBase.ORI_I2S_TYPE;

				for (int i=0;i<fileInfo.length;i++) {
					fileInfo[i].setUnitsOfMeasure(units);
					fileInfo[i].setResolutions(resol);
					fileInfo[i].setExtents(dims);
					fileInfo[i].setAxisOrientation(orient);
					fileInfo[i].setOrigin(newOriginFloatArray);
					fileInfo[i].setImageOrientation(FileInfoBase.AXIAL);
				}
			}
             
            algo = new AlgorithmTalairachTransform(resultImage, otherImage, transform, transformID, interpolationID, true, true);                
            
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algo.addListener(this);
                           
            createProgressBar(otherImage.getImageName(), algo);
            
			if (runInSeparateThread) {
                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                //algo.setActiveImage(isActiveImage);
                algo.run();
            }
			resultImage.setFileInfo(fileInfo);
			setTalairachHeader(resultImage);
			setTalairachHeader(otherImage);
		} catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }
            System.gc();
            MipavUtil.displayError( "Dialog: unable to allocate enough memory");
            return;
        }
	}
	
    /**
    *  Accessor that returns the image.
    *  @return          The result image.
    */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
    *	Accessor that sets the parameters
    */
    public void setParameters() {
    }
	
    //************************************************************************
    //************************** Event Processing ****************************
    //************************************************************************

	/**
	*  Closes dialog box when the OK button is pressed and calls the algorithm.
	*  @param event       Event that triggers function.
	*/
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
	
		if (command.equals("Close")) {
 			dispose();
		}  else if (command.equals("Help")) {
            //MipavUtil.showHelp("13010");
            MipavUtil.showWebHelp("Mapping_Brains_in_Talairach_Space");
        } else if (command.equals("ACPC")) {
			transformToACPC();
		} else if (command.equals("TLRC")) {
			transformToTLRC();
		} else if (command.equals("Image")) {
			transformImage();
		} else if (command.equals("Select")) {
			updateComboBoxImage();
		} else if (command.equals("Save")) {
			buildSaveDialog();
            saveDialog.setSize(500,326);
			
		} else if (command.equals("Load")) {
			buildLoadDialog();
            loadDialog.setSize(500,326);
			
		} else if (command.equals("ChangeTransformType")) {
			transformType = (String)comboBoxTransform.getSelectedItem();
			if ((transformType.equals("acpc to orig")) || (transformType.equals("acpc to Tlrc")) ||
			    (transformType.equals("Tlrc to orig")) || (transformType.equals("Tlrc to acpc"))) {
				acpcResLabel.setEnabled(false);
				acpcResText.setEnabled(false);
			}
			else {
				acpcResLabel.setEnabled(true);
				acpcResText.setEnabled(true);
			}
		}
    }
	
	public void loadFileActionPerformed(ActionEvent evt) {
		if ( JFileChooser.APPROVE_SELECTION.equals(evt.getActionCommand()) ) {
			String filename = loadDialog.getSelectedFile().getAbsolutePath();
			//MipavUtil.displayInfo("data file: "+filename+"\n");
			transform.readFromFile(filename);
			setTalairachHeader(image);
			//MipavUtil.displayInfo("transform data loaded from "+filename+"\n");
			userInterface.setDefaultDirectory(loadDialog.getCurrentDirectory().getAbsolutePath());
		}
	}

 	public void saveFileActionPerformed(ActionEvent evt) {
		if ( JFileChooser.APPROVE_SELECTION.equals(evt.getActionCommand()) ) {
			String filename = saveDialog.getSelectedFile().getAbsolutePath();
			transform.writeToFile(filename);
			//MipavUtil.displayInfo("transform data saved to "+filename+"\n");
			userInterface.setDefaultDirectory(saveDialog.getCurrentDirectory().getAbsolutePath());
		}
	}
	
	/* add the Talairach Transform to the image header */
	public void setTalairachHeader(ModelImage img) {
		img.setTalairachTransformInfo(transform);
	}
	//************************************************************************
    //************************** Algorithm Events ****************************
    //************************************************************************
    
    /** 
    *	This method is required if the AlgorithmPerformed interface is implemented. 
    *   It is called by the algorithm when it has completed or failed to to complete, 
    *   so that the dialog can be display the result image and/or clean up.
    *   @param algorithm   Algorithm that caused the event.
    */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        
        if ( algorithm instanceof AlgorithmTalairachTransform) {
            image.clearMask();
			if(algorithm.isCompleted() == true && resultImage != null) {
                //The algorithm has completed and produced a new image to be displayed.
                //updateFileInfo(image, resultImage);
                resultImage.clearMask();

				try {
					new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
				} catch (OutOfMemoryError error) {
					System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
               }
            }  else if (resultImage == null) {
				// These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                for (int i = 0; i < imageFrames.size(); i++) {
                    ( (Frame) (imageFrames.elementAt(i))).setTitle(title);
                    ( (Frame) (imageFrames.elementAt(i))).setEnabled(true);
                    if ( ( (Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame( (Frame) (imageFrames.elementAt(i)));
                    }
                }
                if (parentFrame != null)
                    userInterface.registerFrame(parentFrame);
                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {
                //algorithm failed but result image still has garbage
                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }
                System.gc();
            }
       }
       algorithm.finalize();
       algorithm = null;
       updateComboBoxImage();
    }  // end AlgorithmPerformed()
    
}
