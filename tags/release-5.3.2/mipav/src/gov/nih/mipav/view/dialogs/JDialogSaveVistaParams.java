package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoVista;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;


/**
 * @author pandyan
 *
 */
public class JDialogSaveVistaParams extends JDialogBase {

	/** image **/
	private ModelImage image;
	
	/** tabbed pane **/
	private JTabbedPane tabbedPane;
	
	/** panels **/
	private JPanel mainPanel, requiredPanel, optionalPanel, talairachPanel;
	
	/** textfields **/
	private JTextField nameField, modalityField, patientField, birthField, sexField, deviceField, protocolField, sequenceField, 
	CoilIDField, dateField, timeField, boundingBoxField, ori_nrowsField, ori_ncolumnsField, acquisitionOrientationField, 
	caField, cpField, extentField, fixpointField, talairachField, nbandsField, nframesField, nrowsField, ncolumnsField, 
	bandtypeField, repnField, voxelField, conventionField, orientationField, MPIL_vista_0Field, ntimestepsField, repetition_timeField, 
	slice_timeField;
	
	/** array list of text fields **/
	private ArrayList<JTextField> vistaParamTextfields = new ArrayList<JTextField>();
	
	
	
	/**
	 * constructor
	 * @param theParentFrame
	 * @param image
	 */
	public JDialogSaveVistaParams(Frame theParentFrame,ModelImage image) {
		 super(theParentFrame, true);
		 this.image = image;
		 
		 init();
		 
	}
	
	
	/**
	 * init
	 */
	private void init() {
		
		setForeground(Color.black);
        setTitle("Vista parameters");
        
        mainPanel = new JPanel(new GridBagLayout());
        
        requiredPanel = new JPanel(new GridBagLayout());
        talairachPanel = new JPanel(new GridBagLayout());
        optionalPanel = new JPanel(new GridBagLayout());
        
        populateRequiredPanel();
        populateTalairachPanel();
        populateOptionalPanel();
        
        
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        
        JScrollPane requiredScrollPane = new JScrollPane(requiredPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        requiredScrollPane.setPreferredSize(new Dimension(600, 200));
        tabbedPane.addTab("Required", requiredScrollPane);
        
        JScrollPane taliarachScrollPane = new JScrollPane(talairachPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        taliarachScrollPane.setPreferredSize(new Dimension(600, 200));
        tabbedPane.addTab("Talairach", taliarachScrollPane);
        
        JScrollPane optionalScrollPane = new JScrollPane(optionalPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        optionalScrollPane.setPreferredSize(new Dimension(600, 200));
        tabbedPane.addTab("Optional", optionalScrollPane);
        


        GridBagConstraints gbc = new GridBagConstraints();
        
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(10, 5, 10, 25);
        gbc.gridwidth = 1;

        final JPanel OKPanel = new JPanel();
        buildOKButton();
        buildCancelButton();
        OKButton.setText("OK");
        OKButton.addActionListener(this);
        cancelButton.addActionListener(this);
        OKPanel.add(OKButton);
        OKPanel.add(cancelButton);


        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.weighty = 0;
        mainPanel.add(tabbedPane, gbc);
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.gridy = 1;
        mainPanel.add(OKPanel, gbc);

        getContentPane().add(mainPanel);

        pack();

        this.setMinimumSize(this.getSize());
        setVisible(true);
		
	}

	
	
	/**
	 * prepopulates fields
	 * @param textfield
	 */
	private void prepopulateField(JTextField textfield) {
		vistaParamTextfields.add(textfield);
		String textfieldKey = textfield.getName();
		if(image.getFileInfo(0)instanceof  FileInfoVista) {
			ArrayList<HashMap<String,String>> imagesInfo = ((FileInfoVista)(image.getFileInfo(0))).getImagesInfo();
			 HashMap<String,String> info = imagesInfo.get(0);
			 Set<String> keys = info.keySet();
			 Iterator<String> iter = keys.iterator();
			 while(iter.hasNext()) {
				 String key = iter.next();
				 if(textfieldKey.equals(key)) {
					 String value = info.get(key);
					 textfield.setText(value);
					 break;
				 }
			 }
			 
			 //even though bandttype is required....sometimes it is not there
			 //so set it
			 if(textfield == bandtypeField) {
				 if(bandtypeField.getText().trim().equals("")) {
					 if(image.is3DImage()) {
							bandtypeField.setText("spatial");
						}else {
							bandtypeField.setText("temporal");
						}
				 }
			 }
		}else {
			String value = "";
			if(image.is3DImage()) {
				 if(textfield == nbandsField) {
					 nbandsField.setText(String.valueOf(image.getExtents()[2]));
				 }
				 if(textfield == nframesField) {
					 nframesField.setText(String.valueOf(image.getExtents()[2]));
				 }
				 if(textfield == bandtypeField) {
					 bandtypeField.setText("spatial");
				 }
			}else {
				 if(textfield == nbandsField) {
					 nbandsField.setText(String.valueOf(image.getExtents()[3]));
				 }
				 if(textfield == nframesField) {
					 nframesField.setText(String.valueOf(image.getExtents()[3]));
				 }
				 if(textfield == ntimestepsField) {
					 ntimestepsField.setText(String.valueOf(image.getExtents()[3]));
				 }
				 if(textfield == bandtypeField) {
					 bandtypeField.setText("temporal");
				 }
			}
			
			 	if(textfield == nrowsField) {
			 		nrowsField.setText(String.valueOf(image.getExtents()[1]));
			 	}
			 	 if(textfield == ncolumnsField) {
			 		 ncolumnsField.setText(String.valueOf(image.getExtents()[0]));
			 	 }
				
    			 if(image.getType() == ModelStorageBase.UBYTE) {
    				 value = "ubyte";
    			 }else if(image.getType() == ModelStorageBase.SHORT) {
    				 value = "short";
    			 }else if(image.getType() == ModelStorageBase.LONG) {
    				 value = "long";
    			 }else if(image.getType() == ModelStorageBase.FLOAT) {
    				 value = "float";
    			 }else if(image.getType() == ModelStorageBase.DOUBLE) {
    				 value = "double";
    			 }
    			 if(textfield == repnField) {
    				 repnField.setText(value);
    			 }
				value = "\"" + image.getResolutions(0)[0] + " " + image.getResolutions(0)[1] + " " + image.getResolutions(0)[2] + "\"";
				 if(textfield == voxelField) {
					 voxelField.setText(value);
				 }
				value = "";
				if(image.getImageOrientation() == FileInfoBase.AXIAL) {
	   				 if(image.getAxisOrientation()[0] == FileInfoBase.ORI_R2L_TYPE) {
	   					 value = "natural";
	   				 }else {
	   					 value = "radiological";
	   				 }
	   			}else if(image.getImageOrientation() == FileInfoBase.CORONAL) {
	   				 if(image.getAxisOrientation()[2] == FileInfoBase.ORI_R2L_TYPE) {
	   					 value = "natural";
	   				 }else {
	   					 value = "radiological";
	   				 }
	   			}else if(image.getImageOrientation() == FileInfoBase.SAGITTAL) {
	   				 if(image.getAxisOrientation()[2] == FileInfoBase.ORI_R2L_TYPE) {
	   					 value = "natural";
	   				 }else {
	   					 value = "radiological";
	   				 }
	   			 }
				if(textfield == conventionField) {
					conventionField.setText(value);
				}
				value = "";
	   			 if(image.getImageOrientation() == FileInfoBase.AXIAL) {
	   				 value = "axial";
	   			 }else if(image.getImageOrientation() == FileInfoBase.CORONAL) {
	   				 value = "coronal";
	   			 }else if(image.getImageOrientation() == FileInfoBase.SAGITTAL) {
	   				 value = "sagittal";
	   			 }
	   			 if(textfield == orientationField) {
	   				 orientationField.setText(value);
	   			 }
	   			 
				
				
				
			
			
			
			
		}
	}
	
	/**
	 * populates the options panel
	 */
	private void populateOptionalPanel() {
		GridBagConstraints gbc = new GridBagConstraints();
		
		gbc.insets = new Insets(5,5,5,5);
		gbc.gridx = 0;
		gbc.gridy = 0;
		JLabel nameLabel = new JLabel("name");
		optionalPanel.add(nameLabel,gbc);
		nameField = new JTextField();
		nameField.setName("name");
		nameField.setPreferredSize(new Dimension(100, 25));
		nameField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(nameField);
		optionalPanel.add(nameField,gbc);
		
		gbc.gridx = 2;
		JLabel modalityLabel = new JLabel("modality");
		optionalPanel.add(modalityLabel,gbc);
		modalityField = new JTextField();
		modalityField.setName("modality");
		modalityField.setPreferredSize(new Dimension(100, 25));
		modalityField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(modalityField);
		optionalPanel.add(modalityField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		JLabel patientLabel = new JLabel("patient");
		optionalPanel.add(patientLabel,gbc);
		patientField = new JTextField();
		patientField.setName("patient");
		patientField.setPreferredSize(new Dimension(100, 25));
		patientField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(patientField);
		optionalPanel.add(patientField,gbc);
		
		gbc.gridx = 2;
		JLabel birthLabel = new JLabel("birth");
		optionalPanel.add(birthLabel,gbc);
		birthField = new JTextField();
		birthField.setName("birth");
		birthField.setPreferredSize(new Dimension(100, 25));
		birthField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(birthField);
		optionalPanel.add(birthField,gbc);
		
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		JLabel sexLabel = new JLabel("sex");
		optionalPanel.add(sexLabel,gbc);
		sexField = new JTextField();
		sexField.setName("sex");
		sexField.setPreferredSize(new Dimension(100, 25));
		sexField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(sexField);
		optionalPanel.add(sexField,gbc);
		
		gbc.gridx = 2;
		JLabel deviceLabel = new JLabel("device");
		optionalPanel.add(deviceLabel,gbc);
		deviceField = new JTextField();
		deviceField.setName("device");
		deviceField.setPreferredSize(new Dimension(100, 25));
		deviceField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(deviceField);
		optionalPanel.add(deviceField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		JLabel protocolLabel = new JLabel("protocol");
		optionalPanel.add(protocolLabel,gbc);
		protocolField = new JTextField();
		protocolField.setName("protocol");
		protocolField.setPreferredSize(new Dimension(100, 25));
		protocolField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(protocolField);
		optionalPanel.add(protocolField,gbc);
		
		gbc.gridx = 2;
		JLabel sequenceLabel = new JLabel("sequence");
		optionalPanel.add(sequenceLabel,gbc);
		sequenceField = new JTextField();
		sequenceField.setName("sequence");
		sequenceField.setPreferredSize(new Dimension(100, 25));
		sequenceField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(sequenceField);
		optionalPanel.add(sequenceField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 4;
		JLabel CoilIDLabel = new JLabel("CoilID");
		optionalPanel.add(CoilIDLabel,gbc);
		CoilIDField = new JTextField();
		CoilIDField.setName("CoilID");
		CoilIDField.setPreferredSize(new Dimension(100, 25));
		CoilIDField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(CoilIDField);
		optionalPanel.add(CoilIDField,gbc);
		
		gbc.gridx = 2;
		JLabel dateLabel = new JLabel("date");
		optionalPanel.add(dateLabel,gbc);
		dateField = new JTextField();
		dateField.setName("date");
		dateField.setPreferredSize(new Dimension(100, 25));
		dateField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(dateField);
		optionalPanel.add(dateField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 5;
		JLabel timeLabel = new JLabel("time");
		optionalPanel.add(timeLabel,gbc);
		timeField = new JTextField();
		timeField.setName("time");
		timeField.setPreferredSize(new Dimension(100, 25));
		timeField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(timeField);
		optionalPanel.add(timeField,gbc);
		
		gbc.gridx = 2;
		JLabel boundingBoxLabel = new JLabel("boundingBox");
		optionalPanel.add(boundingBoxLabel,gbc);
		boundingBoxField = new JTextField();
		boundingBoxField.setName("boundingBox");
		boundingBoxField.setPreferredSize(new Dimension(100, 25));
		boundingBoxField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(boundingBoxField);
		optionalPanel.add(boundingBoxField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 6;
		JLabel ori_nrowsLabel = new JLabel("ori_nrows");
		optionalPanel.add(ori_nrowsLabel,gbc);
	    ori_nrowsField = new JTextField();
	    ori_nrowsField.setName("ori_nrows");
		ori_nrowsField.setPreferredSize(new Dimension(100, 25));
		ori_nrowsField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(ori_nrowsField);
		optionalPanel.add(ori_nrowsField,gbc);
		
		gbc.gridx = 2;
		JLabel ori_ncolumnsLabel = new JLabel("ori_ncolumns");
		optionalPanel.add(ori_ncolumnsLabel,gbc);
		ori_ncolumnsField = new JTextField();
		ori_ncolumnsField.setName("ori_ncolumns");
		ori_ncolumnsField.setPreferredSize(new Dimension(100, 25));
		ori_ncolumnsField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(ori_ncolumnsField);
		optionalPanel.add(ori_ncolumnsField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 7;
		JLabel acquisitionOrientationLabel = new JLabel("acquisitionOrientation");
		optionalPanel.add(acquisitionOrientationLabel,gbc);
		acquisitionOrientationField = new JTextField();
		acquisitionOrientationField.setName("acquisitionOrientation");
		acquisitionOrientationField.setPreferredSize(new Dimension(100, 25));
		acquisitionOrientationField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(acquisitionOrientationField);
		optionalPanel.add(acquisitionOrientationField,gbc);
		
		
	}
	
	
	
	
	
	/**
	 * popul
	 */
	private void populateTalairachPanel() {
		GridBagConstraints gbc = new GridBagConstraints();
		
		gbc.insets = new Insets(5,5,5,5);
		gbc.gridx = 0;
		gbc.gridy = 0;
		JLabel caLabel = new JLabel("ca");
		talairachPanel.add(caLabel,gbc);
		caField = new JTextField();
		caField.setName("ca");
		caField.setPreferredSize(new Dimension(100, 25));
		caField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(caField);
		talairachPanel.add(caField,gbc);
		
		gbc.gridx = 2;
		JLabel cpLabel = new JLabel("cp");
		talairachPanel.add(cpLabel,gbc);
		cpField = new JTextField();
		cpField.setName("cp");
		cpField.setPreferredSize(new Dimension(100, 25));
		cpField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(cpField);
		talairachPanel.add(cpField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		JLabel extentLabel = new JLabel("extent");
		talairachPanel.add(extentLabel,gbc);
		extentField = new JTextField();
		extentField.setName("extent");
		extentField.setPreferredSize(new Dimension(100, 25));
		extentField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(extentField);
		talairachPanel.add(extentField,gbc);
		
		gbc.gridx = 2;
		JLabel fixpointLabel = new JLabel("fixpoint");
		talairachPanel.add(fixpointLabel,gbc);
		fixpointField = new JTextField();
		fixpointField.setName("fixpoint");
		fixpointField.setPreferredSize(new Dimension(100, 25));
		fixpointField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(fixpointField);
		talairachPanel.add(fixpointField,gbc);
		
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		JLabel talairachLabel = new JLabel("talairach");
		talairachPanel.add(talairachLabel,gbc);
		talairachField = new JTextField();
		talairachField.setName("talairach");
		talairachField.setPreferredSize(new Dimension(100, 25));
		talairachField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(talairachField);
		talairachPanel.add(talairachField,gbc);
		
		
	}
	
	
	
	private void populateRequiredPanel() {
		GridBagConstraints gbc = new GridBagConstraints();
		
		gbc.insets = new Insets(5,5,5,5);
		gbc.gridx = 0;
		gbc.gridy = 0;
		JLabel nbandsLabel = new JLabel("nbands");
		requiredPanel.add(nbandsLabel,gbc);
		nbandsField = new JTextField();
		nbandsField.setName("nbands");
		nbandsField.setPreferredSize(new Dimension(100, 25));
		nbandsField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(nbandsField);
		requiredPanel.add(nbandsField,gbc);
		
		gbc.gridx = 2;
		JLabel nframesLabel = new JLabel("nframes");
		requiredPanel.add(nframesLabel,gbc);
		nframesField = new JTextField();
		nframesField.setName("nframes");
		nframesField.setPreferredSize(new Dimension(100, 25));
		nframesField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(nframesField);
		requiredPanel.add(nframesField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		JLabel nrowsLabel = new JLabel("nrows");
		requiredPanel.add(nrowsLabel,gbc);
		nrowsField = new JTextField();
		nrowsField.setName("nrows");
		nrowsField.setPreferredSize(new Dimension(100, 25));
		nrowsField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(nrowsField);
		requiredPanel.add(nrowsField,gbc);
		
		gbc.gridx = 2;
		JLabel ncolumnsLabel = new JLabel("ncolumns");
		requiredPanel.add(ncolumnsLabel,gbc);
		ncolumnsField = new JTextField();
		ncolumnsField.setName("ncolumns");
		ncolumnsField.setPreferredSize(new Dimension(100, 25));
		ncolumnsField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(ncolumnsField);
		requiredPanel.add(ncolumnsField,gbc);
		
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		JLabel bandtypeLabel = new JLabel("bandtype");
		requiredPanel.add(bandtypeLabel,gbc);
	    bandtypeField = new JTextField();
	    bandtypeField.setName("bandtype");
		bandtypeField.setPreferredSize(new Dimension(100, 25));
		bandtypeField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(bandtypeField);
		requiredPanel.add(bandtypeField,gbc);
		
		gbc.gridx = 2;
		JLabel repnLabel = new JLabel("repn");
		requiredPanel.add(repnLabel,gbc);
		repnField = new JTextField();
		repnField.setName("repn");
		repnField.setPreferredSize(new Dimension(100, 25));
		repnField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(repnField);
		requiredPanel.add(repnField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		JLabel voxelLabel = new JLabel("voxel");
		requiredPanel.add(voxelLabel,gbc);
		voxelField  = new JTextField();
		voxelField.setName("voxel");
		voxelField.setPreferredSize(new Dimension(100, 25));
		voxelField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(voxelField);
		requiredPanel.add(voxelField,gbc);
		
		gbc.gridx = 2;
		JLabel conventionLabel = new JLabel("convention");
		requiredPanel.add(conventionLabel,gbc);
		conventionField = new JTextField();
		conventionField.setName("convention");
		conventionField.setPreferredSize(new Dimension(100, 25));
		conventionField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 3;
		prepopulateField(conventionField);
		requiredPanel.add(conventionField,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 4;
		JLabel orientationLabel = new JLabel("orientation");
		requiredPanel.add(orientationLabel,gbc);
		orientationField = new JTextField();
		orientationField.setName("orientation");
		orientationField.setPreferredSize(new Dimension(100, 25));
		orientationField.setMinimumSize(new Dimension(100, 25));
		gbc.gridx = 1;
		prepopulateField(orientationField);
		requiredPanel.add(orientationField,gbc);
		
		if(image.is4DImage()) {
			
			
			gbc.gridx = 2;
			JLabel MPIL_vista_0Label = new JLabel("MPIL_vista_0");
			requiredPanel.add(MPIL_vista_0Label,gbc);
			MPIL_vista_0Field = new JTextField();
			MPIL_vista_0Field.setName("MPIL_vista_0");
			MPIL_vista_0Field.setPreferredSize(new Dimension(100, 25));
			MPIL_vista_0Field.setMinimumSize(new Dimension(100, 25));
			gbc.gridx = 3;
			prepopulateField(MPIL_vista_0Field);
			requiredPanel.add(MPIL_vista_0Field,gbc);
			
			
			
			gbc.gridx = 0;
			gbc.gridy = 5;
			JLabel ntimestepsLabel = new JLabel("ntimesteps");
			requiredPanel.add(ntimestepsLabel,gbc);
			ntimestepsField = new JTextField();
			ntimestepsField.setName("ntimesteps");
			ntimestepsField.setPreferredSize(new Dimension(100, 25));
			ntimestepsField.setMinimumSize(new Dimension(100, 25));
			gbc.gridx = 1;
			prepopulateField(ntimestepsField);
			requiredPanel.add(ntimestepsField,gbc);
			
			gbc.gridx = 2;
			JLabel repetition_timeLabel = new JLabel("repetition_time");
			requiredPanel.add(repetition_timeLabel,gbc);
			repetition_timeField = new JTextField();
			repetition_timeField.setName("repetition_time");
			repetition_timeField.setPreferredSize(new Dimension(100, 25));
			repetition_timeField.setMinimumSize(new Dimension(100, 25));
			gbc.gridx = 3;
			prepopulateField(repetition_timeField);
			requiredPanel.add(repetition_timeField,gbc);
			
			gbc.gridx = 0;
			gbc.gridy = 6;
			JLabel slice_timeLabel = new JLabel("slice_time");
			requiredPanel.add(slice_timeLabel,gbc);
			slice_timeField = new JTextField();
			slice_timeField.setName("slice_time");
			slice_timeField.setPreferredSize(new Dimension(100, 25));
			slice_timeField.setMinimumSize(new Dimension(100, 25));
			gbc.gridx = 1;
			prepopulateField(slice_timeField);
			requiredPanel.add(slice_timeField,gbc);
			
			
			
			
			
		}
		
		
		
	}
	
	
	
	
	private boolean validateRequiredParams() {
		
		if(nbandsField.getText().trim().equals("")) {
			return false;
		}
		
		if(nframesField.getText().trim().equals("")) {
			return false;
		}
		
		if(nrowsField.getText().trim().equals("")) {
			return false;
		}
		
		if(ncolumnsField.getText().trim().equals("")) {
			return false;
		}
		
		if(bandtypeField.getText().trim().equals("")) {
			return false;
		}
		
		if(repnField.getText().trim().equals("")) {
			return false;
		}
		
		if(voxelField.getText().trim().equals("")) {
			return false;
		}
		
		if(conventionField.getText().trim().equals("")) {
			return false;
		}
		
		if(orientationField.getText().trim().equals("")) {
			return false;
		}
		
		
		if(image.is4DImage()) {
			if(MPIL_vista_0Field.getText().trim().equals("")) {
				return false;
			}
			
			if(ntimestepsField.getText().trim().equals("")) {
				return false;
			}
			
			if(repetition_timeField.getText().trim().equals("")) {
				return false;
			}
			
			if(slice_timeField.getText().trim().equals("")) {
				return false;
			}
		}
		
		
		
		return true;
		
	}
	
	
	
	public ArrayList<JTextField> getVistaParamTextfields() {
		return vistaParamTextfields;
	}



	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equals("OK")) {
			if(!validateRequiredParams()) {
				MipavUtil.displayError("All required params must be entered");
			}else {
				setVisible(false);
			}
			
		}else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        }

	}
	
	
	

}
