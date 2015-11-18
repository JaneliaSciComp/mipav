package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoNIFTI;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterString;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.MatrixHolder;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.LegacyDialogDefaultsInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.StringTokenizer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/** 
*   
*   Dialog box for basic image processing tools
*
*
*	@version    April 2006
*	@author     Pilou Bazin
*
*
*/  
public class JDialogReorient extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery, LegacyDialogDefaultsInterface  {
    
    private     AlgorithmTransform 		algoTrans = null;
    private     ModelImage              image;                // source image
    private     ModelImage              template = null;      // template image
    private     ModelImage              resultImage = null;   // result image
	
    public static final int AXIAL_INDEX = 0;
    
    public static final int CORONAL_INDEX = 1;
    
    public static final int SAGITTAL_INDEX = 2;
    
    public static final int USER_INDEX = 3;
    
	// parameters
	private     String[]    orientTypes 	= 		{"Axial",
													 "Coronal",
													 "Sagittal",
                                                     "Unknown"};
    private     String[]    newOrientTypes =        {"Axial",
                                                     "Coronal",
                                                     "Sagittal",
                                                     "User defined"};
    
		
	private     String[]    resolutionTypes 	= 	{"Unchanged",
													 "Finest Cubic",
													 "Coarsest Cubic",
													 "Same as template"};	
    private	    int         resolutionIndex     =  0;
	
	private		String[]	interpTypes	=	{"Nearest Neighbor",
												 "Trilinear",
                                                 "Bspline 3rd order",
                                                 "Bspline 4th order",
                                                 "Cubic Lagrangian",
                                                 "Quintic Lagrangian",
                                                 "Heptic Lagrangian",
												 "Windowed Sinc"};
	private		String		interpType	=	"Trilinear";
    private String[] orients = {
            "Unknown", "Patient Right to Left", "Patient Left to Right", "Patient Posterior to Anterior",
            "Patient Anterior to Posterior", "Patient Inferior to Superior", "Patient Superior to Inferior"
        };
    
	// storage for header information
	FileInfoBase 	fileInfo;
	FileInfoNIFTI   fileInfoNIFTI;
	
    // dialog elements
	private 	JPanel  	mainPanel;
	private 	JLabel  	labelResType;
    private		JComboBox	comboResType;
	private		JLabel		labelTemplate;	
	private		JComboBox	comboTemplate;
	private		JLabel		labelInterpType;	
	private		JComboBox	comboInterpType;
    private     JLabel      presentOrientLabel2;
    private     JComboBox   presentOrientBoxX;
    private     JComboBox   presentOrientBoxY;
    private     JComboBox   presentOrientBoxZ;
    private     int[]       or = new int[3];
    private     JComboBox   newOrientBox;
    private     JComboBox   newOrientBoxX;
    private     JComboBox   newOrientBoxY;
    private     JComboBox   newOrientBoxZ;
    private     int[]       newOr = new int[3];
    private     int[]       axisOrder = new int[3];
    private     boolean[]   axisFlip = new boolean[3];
	
    /**
    *  Creates dialog for plugin.
    *  @param parent          Parent frame.
    *  @param im              Source image.
    */
    public JDialogReorient(Frame theParentFrame, ModelImage im) {
		super(theParentFrame, false);
		image = im;
        init();
	}
	
    /**
     * Empty constructor needed for dynamic instantiation.
     */
	public JDialogReorient() {}
    

    /**
    *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
    */
	private void init(){
        int i;
        boolean isDicom = false;
        JPanel presentOrientPanel;
        JPanel newOrientPanel;
        JLabel presentOrientLabel;
        JLabel presentOrientLabelX;
        JLabel presentOrientLabelY;
        JLabel presentOrientLabelZ;
        JLabel newOrientLabel;
        JLabel newOrientLabelX;
        JLabel newOrientLabelY;
        JLabel newOrientLabelZ;
        float rx = image.getFileInfo()[0].getResolutions()[0];
        float ry = image.getFileInfo()[0].getResolutions()[1];
        float rz = image.getFileInfo()[0].getResolutions()[2];
        float minResolution = Math.min(rx, Math.min(ry, rz));
        float maxResolution = Math.max(rx, Math.max(ry, rz));
        setForeground(Color.black);
        setTitle("Reorientation / Resampling");
        
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.insets = new Insets(0, 5, 0, 5);
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.NONE;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        
        presentOrientPanel = new JPanel(new GridBagLayout());
        presentOrientPanel.setBorder(buildTitledBorder("Present Orientation"));
        
        presentOrientLabel = new JLabel("Image orientation:");
        presentOrientLabel.setFont(serif12);
        presentOrientLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        presentOrientPanel.add(presentOrientLabel, gbc2);
        
        presentOrientLabel2 = new JLabel(orientTypes[image.getImageOrientation()]);
        presentOrientLabel2.setBackground(Color.black);
        gbc2.gridx = 1;
        presentOrientPanel.add(presentOrientLabel2, gbc2);
        
        presentOrientLabelX = new JLabel("X-axis orientation (image left to right):");
        presentOrientLabelX.setFont(serif12);
        presentOrientLabelX.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        presentOrientPanel.add(presentOrientLabelX, gbc2);
        
        presentOrientBoxX = new JComboBox(orients);
        presentOrientBoxX.setBackground(Color.white);
        presentOrientBoxX.setFont(MipavUtil.font12);
        for (i = 0; i <=2; i++) {
            or[i] = image.getFileInfo()[0].getAxisOrientation()[i];   
        }
        presentOrientBoxX.setSelectedIndex(or[0]);
        gbc2.gridx = 1;
        presentOrientPanel.add(presentOrientBoxX, gbc2);
        
        presentOrientLabelY = new JLabel("Y-axis orientation (image top to bottom):");
        presentOrientLabelY.setFont(serif12);
        presentOrientLabelY.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        presentOrientPanel.add(presentOrientLabelY, gbc2);
        
        presentOrientBoxY = new JComboBox(orients);
        presentOrientBoxY.setBackground(Color.white);
        presentOrientBoxY.setFont(MipavUtil.font12);
        presentOrientBoxY.setSelectedIndex(or[1]);
        gbc2.gridx = 1;
        presentOrientPanel.add(presentOrientBoxY, gbc2);
        
        presentOrientLabelZ = new JLabel("Z-axis orientation (into the screen):");
        presentOrientLabelZ.setFont(serif12);
        presentOrientLabelZ.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 3;
        presentOrientPanel.add(presentOrientLabelZ, gbc2);
        
        presentOrientBoxZ = new JComboBox(orients);
        presentOrientBoxZ.setBackground(Color.white);
        presentOrientBoxZ.setFont(MipavUtil.font12);
        presentOrientBoxZ.setSelectedIndex(or[2]);
        presentOrientBoxZ.addItemListener(this);
        gbc2.gridx = 1;
        presentOrientPanel.add(presentOrientBoxZ, gbc2);
        
        newOrientPanel = new JPanel(new GridBagLayout());
        newOrientPanel.setBorder(buildTitledBorder("New Orientation"));
        
        newOrientLabel = new JLabel("Image orientation:");
        newOrientLabel.setFont(serif12);
        newOrientLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        newOrientPanel.add(newOrientLabel, gbc2);
        
        newOrientBox = new JComboBox(newOrientTypes);
        for (i = 0; i <=2; i++) {
            newOr[i] = image.getFileInfo()[0].getAxisOrientation()[i];   
        }
        if ((newOr[0] == FileInfoBase.ORI_R2L_TYPE) && (newOr[1] == FileInfoBase.ORI_A2P_TYPE) &&
                (newOr[2] == FileInfoBase.ORI_I2S_TYPE)) {
            isDicom = true;
            newOrientBox.setSelectedIndex(AXIAL_INDEX);
        }
        else if ((newOr[0] == FileInfoBase.ORI_R2L_TYPE) && (newOr[1] == FileInfoBase.ORI_S2I_TYPE) &&
                     (newOr[2] == FileInfoBase.ORI_A2P_TYPE)) {
            isDicom = true;
            newOrientBox.setSelectedIndex(CORONAL_INDEX);
        }
        else if ((newOr[0] == FileInfoBase.ORI_A2P_TYPE) && (newOr[1] == FileInfoBase.ORI_S2I_TYPE) &&
                 (newOr[2] == FileInfoBase.ORI_R2L_TYPE)) {
            isDicom = true;
            newOrientBox.setSelectedIndex(SAGITTAL_INDEX);
        }
        else {
            newOrientBox.setSelectedIndex(USER_INDEX);
        }
        
        newOrientBox.setBackground(Color.white);
        newOrientBox.setFont(MipavUtil.font12);
        newOrientBox.addItemListener(this);
        gbc2.gridx = 1;
        newOrientPanel.add(newOrientBox, gbc2);
        
        newOrientLabelX = new JLabel("X-axis orientation (image left to right):");
        newOrientLabelX.setFont(serif12);
        newOrientLabelX.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        newOrientPanel.add(newOrientLabelX, gbc2);
        
        newOrientBoxX = new JComboBox(orients);
        newOrientBoxX.setBackground(Color.white);
        newOrientBoxX.setFont(MipavUtil.font12);
        newOrientBoxX.setSelectedIndex(newOr[0]);
        if (isDicom) {
            newOrientBoxX.setEnabled(false);
        }
        gbc2.gridx = 1;
        newOrientPanel.add(newOrientBoxX, gbc2);
        
        newOrientLabelY = new JLabel("Y-axis orientation (image top to bottom):");
        newOrientLabelY.setFont(serif12);
        newOrientLabelY.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        newOrientPanel.add(newOrientLabelY, gbc2);
        
        newOrientBoxY = new JComboBox(orients);
        newOrientBoxY.setBackground(Color.white);
        newOrientBoxY.setFont(MipavUtil.font12);
        newOrientBoxY.setSelectedIndex(newOr[1]);
        if (isDicom) {
            newOrientBoxY.setEnabled(false);
        }
        gbc2.gridx = 1;
        newOrientPanel.add(newOrientBoxY, gbc2);
        
        newOrientLabelZ = new JLabel("Z-axis orientation (into the screen):");
        newOrientLabelZ.setFont(serif12);
        newOrientLabelZ.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 3;
        newOrientPanel.add(newOrientLabelZ, gbc2);
        
        newOrientBoxZ = new JComboBox(orients);
        newOrientBoxZ.setBackground(Color.white);
        newOrientBoxZ.setFont(MipavUtil.font12);
        newOrientBoxZ.setSelectedIndex(newOr[2]);
        if (isDicom) {
            newOrientBoxZ.setEnabled(false);
        }
        gbc2.gridx = 1;
        newOrientPanel.add(newOrientBoxZ, gbc2);
		
		labelResType = new JLabel("Resolution ");
        labelResType.setFont(serif12);
        labelResType.setForeground(Color.black);
        		
		resolutionTypes[1] = "Finest Cubic " + Float.toString(minResolution);
        resolutionTypes[2] = "Coarsest Cubic " + Float.toString(maxResolution);
        comboResType = new JComboBox(resolutionTypes);
		comboResType.setFont(serif12);
        comboResType.setBackground(Color.white);
		comboResType.setSelectedIndex(resolutionIndex);
        comboResType.addItemListener(this);
		
		labelInterpType = new JLabel("Interpolation ");
        labelInterpType.setFont(serif12);
        labelInterpType.setForeground(Color.black);
        		
		comboInterpType = new JComboBox(interpTypes);
		comboInterpType.setFont(serif12);
        comboInterpType.setBackground(Color.white);
		comboInterpType.setSelectedItem(interpType);
		
		labelTemplate = new JLabel("Template: ");
        labelTemplate.setForeground(Color.black);
        labelTemplate.setFont(serif12);
		
		buildTemplateList();

		GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

		mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(presentOrientPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
		gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(newOrientPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(labelResType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
		gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(comboResType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(labelInterpType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
		gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(comboInterpType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(labelTemplate, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
		gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(comboTemplate, gbc);
        
		getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true); 
		//setResizable(false);
    	System.gc();
		
	} // end init()
	
    private void buildTemplateList() {
       
        comboTemplate = new JComboBox();
        comboTemplate.setFont(serif12);
        comboTemplate.setBackground(Color.white);
        comboTemplate.setEnabled(false);

        Enumeration<String> names = ViewUserInterface.getReference().getRegisteredImageNames();

        // Add images from user interface that have the same exact dimensionality
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            ModelImage img = ViewUserInterface.getReference().getRegisteredImageByName(name);
			if (ViewUserInterface.getReference().getFrameContainingImage(img) != null) {
				comboTemplate.addItem(name);
           }
        }
    }//buildTemplateList
    
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
	
    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     * @param delim  the parameter delimiter (defaults to " " if empty)
     * @return       the parameter string
     */
	public String getParameterString( String delim ) {
        if ( delim.equals( "" ) ) {
            delim = " ";
        }

        String str = new String();
        str += Integer.toString(newOr[0]) + delim;
        str += Integer.toString(newOr[1]) + delim;
        str += Integer.toString(newOr[2]) + delim;
        str += Integer.toString(resolutionIndex) + delim;
        str += interpType;

        return str;
    }
	
 	/**
     *  Loads the default settings from Preferences to set up the dialog
     */
	public void legacyLoadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                //System.out.println(defaultsString);
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
				newOr[0] = new Integer(st.nextToken()).intValue();
                newOr[1] = new Integer(st.nextToken()).intValue();
                newOr[2] = new Integer(st.nextToken()).intValue();
				resolutionIndex = new Integer(st.nextToken()).intValue();
				interpType = st.nextToken();
			}
            catch (Exception ex) {
                // since there was a problem parsing the defaults string, start over with the original defaults
                //System.out.println( "Resetting defaults for dialog: " + getDialogName() );
                Preferences.removeProperty( getDialogName() );
            }
        } else {
			System.out.println( "no saved dialogs for "+getDialogName() );
		}
    }
		
    /**
     * Saves the default settings into the Preferences file
     */
	
    public void legacySaveDefaults() {
        String defaultsString = new String( getParameterString(",") );
        //System.out.println(defaultsString);
        Preferences.saveDialogDefaults(getDialogName(),defaultsString);
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
	
		if (command.equals("OK")) {
			if (setVariables()) { 
				callAlgorithm();
			}    
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
        } else {
            super.actionPerformed(event);
        }
		
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
                
		
        if ( algorithm instanceof AlgorithmTransform) {
        	TransMatrix newMatrix = null;
        	TransMatrix newMatrix2 = null;
            resultImage = algoTrans.getTransformedImage();
			if (algorithm.isCompleted() == true && resultImage != null) {
                // The algorithm has completed and produced a new image to be displayed.
				if (image.getFileInfo()[0].getFileFormat() == FileUtility.NIFTI) {
					fileInfoNIFTI.setMin(resultImage.getMin());
					fileInfoNIFTI.setMax(resultImage.getMax());
                    if (resultImage.getNDims() == 3) {
                    	for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                    		resultImage.setFileInfo((FileInfoNIFTI)fileInfoNIFTI.clone(),i);
                    	}
                    }
                    else if (resultImage.getNDims() == 4) {
                    	for (int i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
                    		resultImage.setFileInfo((FileInfoNIFTI)fileInfoNIFTI.clone(),i);
                    	}	
                    }
				}
				else {
				    JDialogBase.updateFileInfoStatic(fileInfo, resultImage);
				}
				if (resultImage.getNDims() >= 3) {
		        	// Update any destImage NIFTI matrices
		            MatrixHolder matHolder = null;
		            int i;
		            int j;
		            int t;
		            int index;
		            int tDim;
		            matHolder = resultImage.getMatrixHolder();
		            
		            float loc;
		            int orient;

		            if (matHolder != null) {
		            	
		                LinkedHashMap<String, TransMatrix> matrixMap = matHolder.getMatrixMap();
		                Iterator<String> iter = matrixMap.keySet().iterator();
		                String nextKey = null;
		                
		                TransMatrix tempMatrix = null;
		                

			            
		                while (iter.hasNext()) {
		                    nextKey = iter.next();
		                    tempMatrix = matrixMap.get(nextKey);
		                    if (tempMatrix.isNIFTI()) {
		                    	if (newMatrix == null) {
		                    	    newMatrix = new TransMatrix(4);
			                    	for (i = 0; i < 3; i++) {
			                            for (j = 0; j < 3; j++) {
			                            	if (axisFlip[i]) {
			                            		newMatrix.set(j, i, -tempMatrix.get(j, axisOrder[i]));
			                            	}
			                            	else {
			                                    newMatrix.set(j, i, tempMatrix.get(j, axisOrder[i]));
			                            	}
			                            }
			                            loc = tempMatrix.get(axisOrder[i], 3);
			                            if (axisFlip[i]) {
			                            	orient = image.getFileInfo(0).getAxisOrientation(axisOrder[i]);
			                            	if ((orient == FileInfoBase.ORI_R2L_TYPE) || 
			                                        (orient == FileInfoBase.ORI_A2P_TYPE) || 
			                                        (orient == FileInfoBase.ORI_I2S_TYPE)) {
			                                	loc = loc + ((image.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * image.getFileInfo(0).getResolutions()[axisOrder[i]]);
			                                }
			                                else {
			                                	loc = loc - ((image.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * image.getFileInfo(0).getResolutions()[axisOrder[i]]);	
			                                }
			                            }
			                            newMatrix.set(i, 3, loc);
			                    	} // for (i = 0; i < 3; i++)
			                    	tempMatrix.Copy(newMatrix);
			                    	if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
				                        if (tempMatrix.isQform()) {
				                            if (resultImage.getNDims() == 3) {
				                                for (i = 0; i < resultImage.getExtents()[2]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixQ(newMatrix);
				                                }
				                            }
				                            else if (resultImage.getNDims() == 4) {
				                                for (i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixQ(newMatrix);    
				                                }
				                            }
				                        } // if (tempMatrix.isQform())
				                        else { // tempMatrix is sform
				                            if (resultImage.getNDims() == 3) {
				                                for (i = 0; i < resultImage.getExtents()[2]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixS(newMatrix);
				                                }
				                            }
				                            else if (resultImage.getNDims() == 4) {
				                                for (i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixS(newMatrix);    
				                                }
				                            }    
				                        } // else tempMatrix is sform
			                    	} // if (destImage.getFileInfo(0) instanceof FileInfoNIFTI)
		                    	} // if (newMatrix == null)
		                    	else {
		                    		newMatrix2 = new TransMatrix(4);
		                    		for (i = 0; i < 3; i++) {
			                            for (j = 0; j < 3; j++) {
			                            	if (axisFlip[i]) {
			                            		newMatrix2.set(j, i, -tempMatrix.get(j, axisOrder[i]));
			                            	}
			                            	else {
			                                    newMatrix2.set(j, i, tempMatrix.get(j, axisOrder[i]));
			                            	}
			                            }
			                            loc = tempMatrix.get(axisOrder[i], 3);
			                            if (axisFlip[i]) {
			                            	orient = image.getFileInfo(0).getAxisOrientation(axisOrder[i]);
			                            	if ((orient == FileInfoBase.ORI_R2L_TYPE) || 
			                                        (orient == FileInfoBase.ORI_A2P_TYPE) || 
			                                        (orient == FileInfoBase.ORI_I2S_TYPE)) {
			                                	loc = loc + ((image.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * image.getFileInfo(0).getResolutions()[axisOrder[i]]);
			                                }
			                                else {
			                                	loc = loc - ((image.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * image.getFileInfo(0).getResolutions()[axisOrder[i]]);	
			                                }
			                            }
			                            newMatrix2.set(i, 3, loc);
			                    	} // for (i = 0; i < 3; i++)
			                    	tempMatrix.Copy(newMatrix2);
			                    	if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
				                        if (tempMatrix.isQform()) {
				                            if (resultImage.getNDims() == 3) {
				                                for (i = 0; i < resultImage.getExtents()[2]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixQ(newMatrix2);
				                                }
				                            }
				                            else if (resultImage.getNDims() == 4) {
				                                for (i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixQ(newMatrix2);    
				                                }
				                            }
				                        } // if (tempMatrix.isQform())
				                        else { // tempMatrix is sform
				                            if (resultImage.getNDims() == 3) {
				                                for (i = 0; i < resultImage.getExtents()[2]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixS(newMatrix2);
				                                }
				                            }
				                            else if (resultImage.getNDims() == 4) {
				                                for (i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixS(newMatrix2);    
				                                }
				                            }    
				                        } // else tempMatrix is sform
			                    	} // if (destImage.getFileInfo(0) instanceof FileInfoNIFTI)
		                    	}
		                    } // if (tempMatrix.isNIFTI())
		                }
		                if (newMatrix != null) {
		                	matHolder.clearMatrices();
		                	matHolder.addMatrix(newMatrix);
		                	if (newMatrix2 != null) {
		                		matHolder.addMatrix(newMatrix2);
		                	}
		                }
		            } // if (matHolder != null)  
		            
		            if ( (image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
		                    || (image.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
		            	TransMatrix dicomMatrix = null;
		            	dicomMatrix = image.getMatrix();
		            	newMatrix = new TransMatrix(4);
		            	for (i = 0; i < 3; i++) {
		                    for (j = 0; j < 3; j++) {
		                    	if (axisFlip[i]) {
		                    		newMatrix.set(j, i, -dicomMatrix.get(j, axisOrder[i]));
		                    	}
		                    	else {
		                            newMatrix.set(j, i, dicomMatrix.get(j, axisOrder[i]));
		                    	}
		                    }
		            	} // for (i = 0; i < 3; i++)
		            	newMatrix.setTransformID(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL);
		            	resultImage.getMatrixHolder().clearMatrices();
		            	resultImage.getMatrixHolder().addMatrix(newMatrix);
		            	if (resultImage.getNDims() >= 4) {
		            		tDim = resultImage.getExtents()[3];
		            	}
		            	else {
		            		tDim = 1;
		            	}
		            	
		            	for (t = 0; t < tDim; t++) {
			            	for (i = 0; i < resultImage.getExtents()[2]; i++) {
			            		index = i + t * resultImage.getExtents()[2];
			    	            Vector3f pos = new Vector3f(0, 0, i);
			    	        	Vector3f out = new Vector3f(pos);
			    	            MipavCoordinateSystems.fileToScanner(pos, out, resultImage);
			    	            float origin[] = new float[3];
			    	            origin[0] = out.X;
			    	            origin[1] = out.Y;
			    	            origin[2] = out.Z;
			    	            for (j = 0; j < 3; j++) {
			                		if ((resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_R2L_TYPE) ||
			                			(resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_L2R_TYPE)) {
			                	        resultImage.getFileInfo()[index].setOrigin(origin[0],j);
			                		}
			                		else if ((resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_A2P_TYPE) ||
			                		        (resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_P2A_TYPE)) {
			                		    resultImage.getFileInfo()[index].setOrigin(origin[1], j);
			                		}
			                		else {
			                		    resultImage.getFileInfo()[index].setOrigin(origin[2], j);        	
			                	    }
			                	}
			                }
		            	}
		            } // if ( (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
		        } // if (destImage.getNDims() >= 3)
                resultImage.clearMask();
				resultImage.calcMinMax();

                try {
					new ViewJFrameImage(resultImage, null, new Dimension(610, 200) );
                } catch (OutOfMemoryError error) {
                    System.gc();
                    JOptionPane.showMessageDialog(null, 
                                                "Out of memory: unable to open new frame",
                                                "Error", JOptionPane.ERROR_MESSAGE);
                }
				if (algorithm.isCompleted()) {
					insertScriptLine();
				}
            } else if (resultImage != null) {
                //algorithm failed but result image still has garbage
                resultImage = null;
                System.gc();
            }
       }
       algorithm.finalize();
       algorithm = null;
       dispose();
    }  // end AlgorithmPerformed()
    
  
    /**
    *	Use the GUI results to set up the variables needed to run the algorithm.
    *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
    */
    private boolean setVariables() {
        int rl, is, ap;
        int i;
         
        or[0] = presentOrientBoxX.getSelectedIndex();
        or[1] = presentOrientBoxY.getSelectedIndex();
        or[2] = presentOrientBoxZ.getSelectedIndex();
        
        rl = 0;
        ap = 0;
        is = 0;
        for (i = 0; i <= 2; i++) {
            if ((or[i] == FileInfoBase.ORI_L2R_TYPE) || (or[i] == FileInfoBase.ORI_R2L_TYPE)) {
                rl++;
            }
            else if ((or[i] == FileInfoBase.ORI_A2P_TYPE) || (or[i] == FileInfoBase.ORI_P2A_TYPE)) {
                ap++;
            }
            else if ((or[i] == FileInfoBase.ORI_I2S_TYPE) || (or[i] == FileInfoBase.ORI_S2I_TYPE)) {
                is++;
            }
        }
        if ((rl != 1) || (ap != 1) || (is != 1)) {
            MipavUtil.displayError("Error! Present orientation must have one RL, one AP, and one IS axis");
            return false;
        }
        
        newOr[0] = newOrientBoxX.getSelectedIndex();
        newOr[1] = newOrientBoxY.getSelectedIndex();
        newOr[2] = newOrientBoxZ.getSelectedIndex();
        
        rl = 0;
        ap = 0;
        is = 0;
        for (i = 0; i <= 2; i++) {
            if ((newOr[i] == FileInfoBase.ORI_L2R_TYPE) || (newOr[i] == FileInfoBase.ORI_R2L_TYPE)) {
                rl++;
            }
            else if ((newOr[i] == FileInfoBase.ORI_A2P_TYPE) || (newOr[i] == FileInfoBase.ORI_P2A_TYPE)) {
                ap++;
            }
            else if ((newOr[i] == FileInfoBase.ORI_I2S_TYPE) || (newOr[i] == FileInfoBase.ORI_S2I_TYPE)) {
                is++;
            }
        }
        if ((rl != 1) || (ap != 1) || (is != 1)) {
            MipavUtil.displayError("Error! New orientation must have one RL, one AP, and one IS axis");
            return false;
        }
		resolutionIndex = comboResType.getSelectedIndex();
		interpType = (String)comboInterpType.getSelectedItem();
		
		String name = (String)comboTemplate.getSelectedItem();
		template = ViewUserInterface.getReference().getRegisteredImageByName(name);
		
		//System.out.println(getParameterString("|"));
        
    	return true;  	
    }   // end setVariables()
    
    /**
    *	Once all the necessary variables are set, call the Gaussian Blur
    *	algorithm based on what type of image this is and whether or not there
    *	is a separate destination image.
    */
    protected void callAlgorithm() {
        int i, j;
        boolean found;
        int newOrient;
        float ri[] = new float[3];
        int   ni[] = new int[3];
        float r0[] = new float[3];
        int   n0[] = new int[3];
        float org[] = new float[3];
		setVisible(false);
		float origin[];
		float newOrigin[];
		float originalOr[] = new float[3];
		float flippedOr[] = new float[3];
		float xOr = 0.0f;
	    float yOr = 0.0f;;
	    float zOr = 0.0f;
	    Vector3f position;
	    Vector3f out;
        
	    if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
	    	fileInfoNIFTI = (FileInfoNIFTI)(image.getFileInfo()[0].clone());
	    }
	    else {
	        fileInfo = (FileInfoBase)(image.getFileInfo()[0].clone());
	    }
		
		// set resampled resolutions, dimensions
		ri[0] = image.getFileInfo()[0].getResolutions()[0];
		ri[1] = image.getFileInfo()[0].getResolutions()[1];
		ri[2] = image.getFileInfo()[0].getResolutions()[2];
		
		ni[0] = image.getExtents()[0];
		ni[1] = image.getExtents()[1];
		ni[2] = image.getExtents()[2];
		
		origin = image.getFileInfo()[0].getOrigin();
		newOrigin = origin.clone();
        
        float r[] = new float[3];
        int   n[] = new int[3];
        for (i = 0; i <= 2; i++) {
            r[i] = ri[i];
            n[i] = ni[i];
        }
		
		if (resolutionIndex == 1) {
            // Finest cubic
			float rn = Math.min(r[0],Math.min(r[1],r[2]));
			n[0] = (int)Math.ceil(n[0]*r[0]/rn);
			r[0] = rn;
			n[1] = (int)Math.ceil(n[1]*r[1]/rn);
			r[1] = rn;
			n[2] = (int)Math.ceil(n[2]*r[2]/rn);
			r[2] = rn;
		} else if (resolutionIndex == 2) {
            // Coarsest cubic
			float rn = Math.max(r[0],Math.max(r[1],r[2]));
			n[0] = (int)Math.ceil(n[0]*r[0]/rn);
			r[0] = rn;
			n[1] = (int)Math.ceil(n[1]*r[1]/rn);
			r[1] = rn;
			n[2] = (int)Math.ceil(n[2]*r[2]/rn);
			r[2] = rn;
		} else if (resolutionIndex == 3) {
            // Same as template
			r[0] = template.getFileInfo()[0].getResolutions()[0];
			r[1] = template.getFileInfo()[0].getResolutions()[1];
			r[2] = template.getFileInfo()[0].getResolutions()[2];
			n[0] = template.getExtents()[0];
			n[1] = template.getExtents()[1];
			n[2] = template.getExtents()[2];
		}
        
        double X[][] = new double[4][4];
        for (j = 0; j <= 2; j++) {
            switch (or[j]) {
                case FileInfoBase.ORI_R2L_TYPE:
                    found = false;
                    for (i = 0; (i <= 2) && (!found); i++) {
                        if (newOr[i] == FileInfoBase.ORI_R2L_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = false;
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_L2R_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = true;
                            found = true;
                            X[i][j] = -1.0;
                            X[i][3] = ri[j]*(ni[j] - 1);
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                    }
                    break;
                case FileInfoBase.ORI_L2R_TYPE:
                    found = false;
                    for (i = 0; (i <= 2) && (!found); i++) {
                        if (newOr[i] == FileInfoBase.ORI_L2R_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = false;
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_R2L_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = true;
                            found = true;
                            X[i][j] = -1.0;
                            X[i][3] = ri[j]*(ni[j] - 1);
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                    }
                    break;
                case FileInfoBase.ORI_A2P_TYPE:
                    found = false;
                    for (i = 0; (i <= 2) && (!found); i++) {
                        if (newOr[i] == FileInfoBase.ORI_A2P_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = false;
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_P2A_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = true;
                            found = true;
                            X[i][j] = -1.0;
                            X[i][3] = ri[j]*(ni[j] - 1);
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                    }
                    break;
                case FileInfoBase.ORI_P2A_TYPE:
                    found = false;
                    for (i = 0; (i <= 2) && (!found); i++) {
                        if (newOr[i] == FileInfoBase.ORI_P2A_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = false;
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_A2P_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = true;
                            found = true;
                            X[i][j] = -1.0;
                            X[i][3] = ri[j]*(ni[j] - 1);
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                    }
                    break;
                case FileInfoBase.ORI_I2S_TYPE:
                    found = false;
                    for (i = 0; (i <= 2) && (!found); i++) {
                        if (newOr[i] == FileInfoBase.ORI_I2S_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = false;
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_S2I_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = true;
                            found = true;
                            X[i][j] = -1.0;
                            X[i][3] = ri[j]*(ni[j] - 1);
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                    }
                    break;
                case FileInfoBase.ORI_S2I_TYPE:
                    found = false;
                    for (i = 0; (i <= 2) && (!found); i++) {
                        if (newOr[i] == FileInfoBase.ORI_S2I_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = false;
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_I2S_TYPE) {
                        	axisOrder[i] = j;
                        	axisFlip[i] = true;
                            found = true;
                            X[i][j] = -1.0;
                            X[i][3] = ri[j]*(ni[j] - 1);
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                    }
                    break;
            }
        } // for (j = 0; j <= 2; j++)
        
        for (i = 0; i <= 2; i++) {
        	if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
        		fileInfoNIFTI.setResolutions(r0[i], i);   
                fileInfoNIFTI.setExtents(n0[i], i);
                fileInfoNIFTI.setAxisOrientation(newOr[i], i);	
        	}
        	else {
                fileInfo.setResolutions(r0[i], i);   
                fileInfo.setExtents(n0[i], i);
                fileInfo.setAxisOrientation(newOr[i], i);
        	}
        }
        
        for (i = 0; i < 3; i++) {
            if (i == 0) {
            	originalOr[0] = 0.0f;
            	flippedOr[0] = ni[0] - 1;
            }
            else if (i == 1) {
            	originalOr[1] = 0.0f;
            	flippedOr[1] = ni[1] - 1;
            }
            else {
            	originalOr[2] = 0.0f;
            	flippedOr[2] = ni[2] - 1;
            }
        }
        
        for (i = 0; i < 3; i++) {
        	if (axisFlip[i]) {
        		if (axisOrder[i] == 0) {
        		    xOr = flippedOr[0];	
        		}
        		else if (axisOrder[i] == 1) {
        			yOr = flippedOr[1];
        		}
        		else {
        			zOr = flippedOr[2];
        		}
        	}
        	else {
        		if (axisOrder[i] == 0) {
        			xOr = originalOr[0];
        		}
        		else if (axisOrder[i] == 1) {
        			yOr = originalOr[1];
        		}
        		else {
        			zOr = originalOr[2];
        		}
        	}
        }
        
        position = new Vector3f(xOr, yOr, zOr);
        out = new Vector3f(position);
        MipavCoordinateSystems.fileToScanner(position, out, image);
        for (i = 0; i < 3; i++) {
	        if ((or[i] == FileInfoBase.ORI_R2L_TYPE) || (or[i] == FileInfoBase.ORI_L2R_TYPE)) {
	            org[i] = out.X;
	        }
	        else if ((or[i] == FileInfoBase.ORI_A2P_TYPE) || (or[i] == FileInfoBase.ORI_P2A_TYPE)) {
	            org[i] = out.Y;
	        }
	        else {
	            org[i] = out.Z;
	        }
        }

        for (i = 0; i < 3; i++) {
            newOrigin[i] = org[axisOrder[i]];
            if (Math.abs(newOrigin[i]) < .000001f) {
                newOrigin[i] = 0f;
            }
        }
        
        if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
        	fileInfoNIFTI.setOrigin(newOrigin);
        }
        else {
            fileInfo.setOrigin(newOrigin);
        }
        
        if ((newOr[2] == FileInfoBase.ORI_I2S_TYPE) || (newOr[2] == FileInfoBase.ORI_S2I_TYPE)) {
            newOrient = FileInfoBase.AXIAL;
        }
        else if ((newOr[2] == FileInfoBase.ORI_A2P_TYPE) || (newOr[2] == FileInfoBase.ORI_P2A_TYPE)) {
            newOrient = FileInfoBase.CORONAL;
        }
        else if ((newOr[2] == FileInfoBase.ORI_L2R_TYPE) || (newOr[2] == FileInfoBase.ORI_R2L_TYPE)) {
            newOrient = FileInfoBase.SAGITTAL;
        }
        else {
            newOrient = FileInfoBase.UNKNOWN_ORIENT;
        } 
        if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
        	fileInfoNIFTI.setImageOrientation(newOrient);
        }
        else {
            fileInfo.setImageOrientation(newOrient);
        }
		
		TransMatrix transform = new TransMatrix(4);
        transform.setMatrix(0, 2, 0, 3, X);
		
		//System.out.println(transform.toString());
		
		int interp = AlgorithmTransform.TRILINEAR;
		if (interpType.equals("Nearest Neighbor")) {
            interp = AlgorithmTransform.NEAREST_NEIGHBOR;
        } else if (interpType.equals("Trilinear")) {
			interp = AlgorithmTransform.TRILINEAR;
        } else if (interpType.equals("Bspline 3rd order")) {
            interp = AlgorithmTransform.BSPLINE3;
        } else if (interpType.equals("Bspline 4th order")) {
            interp = AlgorithmTransform.BSPLINE4;
        } else if (interpType.equals("Cubic Lagrangian")) {
            interp = AlgorithmTransform.CUBIC_LAGRANGIAN;
        } else if (interpType.equals("Quintic Lagrangian")) {
            interp = AlgorithmTransform.QUINTIC_LAGRANGIAN;
        } else if (interpType.equals("Heptic Lagrangian")) {
            interp = AlgorithmTransform.HEPTIC_LAGRANGIAN;
        } else if  (interpType.equals("Windowed Sinc")) {
            interp = AlgorithmTransform.WSINC;
        }
			
		algoTrans = new AlgorithmTransform(image, transform, interp, r0[0], r0[1], r0[2], n0[0], n0[1], n0[2], 
											true, false, false);
		algoTrans.setUpdateOriginFlag(true);
		
		// This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        algoTrans.addListener(this);

        createProgressBar(image.getImageName(), algoTrans);

        // Start the thread as a low priority because we wish to still have
        // user interface work fast

        if (isRunInSeparateThread()) {

            if (algoTrans.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            algoTrans.run();
        }

    } // end callAlgorithm()
        
	/**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage(1);
        template = scriptParameters.retrieveInputImage(2);
		parentFrame = image.getParentFrame();

        or[0] = image.getFileInfo()[0].getAxisOrientation()[0];
        or[1] = image.getFileInfo()[0].getAxisOrientation()[1];
        or[2] = image.getFileInfo()[0].getAxisOrientation()[2];
        //scripting reorient does not store original orientation in params (should be in image)
        newOr[0] = scriptParameters.getParams().getInt("orientationx");
        newOr[1] = scriptParameters.getParams().getInt("orientationy");
        newOr[2] = scriptParameters.getParams().getInt("orientationz");
		resolutionIndex = scriptParameters.getParams().getInt("resolution");
		interpType = scriptParameters.getParams().getString("interpolation");
		
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     * 
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeInputImage(template);
		scriptParameters.storeOutputImageParams(resultImage, true);  // false means computation in place

        scriptParameters.getParams().put(ParameterFactory.newParameter("orientationx", newOr[0]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("orientationy", newOr[1]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("orientationz", newOr[2]));
		scriptParameters.getParams().put(ParameterFactory.newParameter("resolution", resolutionIndex));
		scriptParameters.getParams().put(ParameterFactory.newParameter("interpolation", interpType));
		
    }
    
    public void itemStateChanged(ItemEvent e) {
        int presentOrient;
        int newOrientIndex;
        int resolutionIndex;
        if (e.getSource().equals(presentOrientBoxZ)) {
            or[2] = presentOrientBoxZ.getSelectedIndex();
            if ((or[2] == FileInfoBase.ORI_I2S_TYPE) || (or[2] == FileInfoBase.ORI_S2I_TYPE)) {
                presentOrient = FileInfoBase.AXIAL;
            }
            else if ((or[2] == FileInfoBase.ORI_A2P_TYPE) || (or[2] == FileInfoBase.ORI_P2A_TYPE)) {
                presentOrient = FileInfoBase.CORONAL;
            }
            else if ((or[2] == FileInfoBase.ORI_L2R_TYPE) || (or[2] == FileInfoBase.ORI_R2L_TYPE)) {
                presentOrient = FileInfoBase.SAGITTAL;
            }
            else {
                presentOrient = FileInfoBase.UNKNOWN_ORIENT;
            }
            presentOrientLabel2.setText(orientTypes[presentOrient]);
        }
        else if (e.getSource().equals(newOrientBox)) {
            newOrientIndex = newOrientBox.getSelectedIndex();
            if (newOrientIndex == AXIAL_INDEX) {
                newOrientBoxX.setSelectedIndex(FileInfoBase.ORI_R2L_TYPE);
                newOrientBoxX.setEnabled(false);
                newOrientBoxY.setSelectedIndex(FileInfoBase.ORI_A2P_TYPE);
                newOrientBoxY.setEnabled(false);
                newOrientBoxZ.setSelectedIndex(FileInfoBase.ORI_I2S_TYPE);
                newOrientBoxZ.setEnabled(false);
            }
            else if (newOrientIndex == CORONAL_INDEX) {
                newOrientBoxX.setSelectedIndex(FileInfoBase.ORI_R2L_TYPE);
                newOrientBoxX.setEnabled(false);
                newOrientBoxY.setSelectedIndex(FileInfoBase.ORI_S2I_TYPE);
                newOrientBoxY.setEnabled(false);
                newOrientBoxZ.setSelectedIndex(FileInfoBase.ORI_A2P_TYPE);
                newOrientBoxZ.setEnabled(false);    
            }
            else if (newOrientIndex == SAGITTAL_INDEX) {
                newOrientBoxX.setSelectedIndex(FileInfoBase.ORI_A2P_TYPE);
                newOrientBoxX.setEnabled(false);
                newOrientBoxY.setSelectedIndex(FileInfoBase.ORI_S2I_TYPE);
                newOrientBoxY.setEnabled(false);
                newOrientBoxZ.setSelectedIndex(FileInfoBase.ORI_R2L_TYPE);
                newOrientBoxZ.setEnabled(false);
            }
            else { //USER_INDEX selected
                newOrientBoxX.setEnabled(true);
                newOrientBoxY.setEnabled(true);
                newOrientBoxZ.setEnabled(true);
            }
        }
        else if (e.getSource().equals(comboResType)) {
            resolutionIndex = comboResType.getSelectedIndex();
            if (resolutionIndex == 3) {
                comboTemplate.setEnabled(true);
            }
            else {
                comboTemplate.setEnabled(false);
            }
        }
    }

/**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Utilities.Reorientation");
            }

            public String getDescription() {
                return new String("Transforms Volume by resampling using transformation matrix and the choice of " +
                		"nearest-neighbor, trilinear interpolation, 3rd order Bspline, 4th order Bspline, " +
                		"cubic Lagrangian, quintic Lagrangian, heptic Lagrangian, or windowed sinc. " +
                		"Must indicate output volume's desired resolutions and dimensions." +
                		"For resolution, 0 == Unchanged, 1 == Finest Cubic, 2 == Coarsest Cubic, 3 == Same as template. " +
                		"For interpolation, chose 'Nearest Neighbor','Trilinear','Bspline 3rd order', " +
                		"'Bspline 4th order', 'Cubic Lagrangian', 'Quintic Lagrangian', 'Heptic Lagrangian', " +
                		"'Windowed Sinc'");
            }

            public String getDescriptionLong() {
                return new String("Transforms Volume by resampling using transformation matrix and the choice of " +
                		"nearest-neighbor, trilinear interpolation, 3rd order Bspline, 4th order Bspline, " +
                		"cubic Lagrangian, quintic Lagrangian, heptic Lagrangian, or windowed sinc. " +
                		"Must indicate output volume's desired resolutions and dimensions." +
                		"For resolution, 0 == Unchanged, 1 == Finest Cubic, 2 == Coarsest Cubic, 3 == Same as template. " +
                		"For interpolation, chose 'Nearest Neighbor','Trilinear','Bspline 3rd order', " +
                		"'Bspline 4th order', 'Cubic Lagrangian', 'Quintic Lagrangian', 'Heptic Lagrangian', " +
                		"'Windowed Sinc'");
            }

            public String getShortLabel() {
                return new String("Reorient");
            }

            public String getLabel() {
                return new String("Reorient");
            }

            public String getName() {
                return new String("Reorient");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(2)));

            //0 == Unknown, 1 == Right to left, 2 == Left to right,
            //3 == Posterior to Anterior, 4 == Anterior to Posterior,
            //5 == Inferior to Posterior, 6 == Posterior to Interior
			table.put(new ParameterInt("orientationx", 1));
		    table.put(new ParameterInt("orientationy", 4));
			table.put(new ParameterInt("orientationz", 5));
			
			//0 == Unchanged, 1 == Finest Cubic, 2 == Coarsest Cubic, 3 == Same as template
			table.put(new ParameterInt("resolution", 1));

			table.put(new ParameterString("interpolation", "Trilinear"));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    public JComboBox getNewOrientBox() {
		return newOrientBox;
	}

	/**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }
}
