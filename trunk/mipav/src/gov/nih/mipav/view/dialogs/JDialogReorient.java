package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.DialogDefaultsInterface;
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
import java.util.StringTokenizer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

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
public class JDialogReorient extends JDialogScriptableBase implements AlgorithmInterface, DialogDefaultsInterface  {
    
    private     AlgorithmTransform 		algoTrans = null;
    private     ModelImage              image;                // source image
    private     ModelImage              template = null;      // template image
    private     ModelImage              resultImage = null;   // result image
		
	// parameters
	private     String[]    orientTypes 	= 		{"Axial",
													 "Coronal",
													 "Sagittal",
                                                     "Unknown"};	
    
		
	private     String[]    resolutionTypes 	= 	{"Unchanged",
													 "Finest-Cubic",
													 "Coarsest-Cubic",
													 "Same_as_template"};	
    private		String		resolutionType		=		"Unchanged";
	
	private		String[]	interpTypes	=	{"Nearest_Neighbor",
												 "Linear",
												 "Windowed_Sinc"};
	private		String		interpType	=	"Linear";
    private String[] orients = {
            "Unknown", "Patient Right to Left", "Patient Left to Right", "Patient Posterior to Anterior",
            "Patient Anterior to Posterior", "Patient Inferior to Superior", "Patient Superior to Inferior"
        };
    
	// storage for header information
	FileInfoBase 	fileInfo;
	
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
    private     int[]       presentAxisOrient;
    private     JLabel      newOrientLabel2;
    private     int         newOrient;
    private     JComboBox   newOrientBoxX;
    private     JComboBox   newOrientBoxY;
    private     JComboBox   newOrientBoxZ;
    private     int[]       newAxisOrient;
    private     int[]       newOr = new int[3];
	
    /**
    *  Creates dialog for plugin.
    *  @param parent          Parent frame.
    *  @param im              Source image.
    */
    public JDialogReorient(Frame theParentFrame, ModelImage im) {
		super(theParentFrame, false);
		image = im;
		loadDefaults();
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
        presentAxisOrient = image.getFileInfo()[0].getAxisOrientation();
        presentOrientBoxX.setSelectedIndex(presentAxisOrient[0]);
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
        presentOrientBoxY.setSelectedIndex(presentAxisOrient[1]);
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
        presentOrientBoxZ.setSelectedIndex(presentAxisOrient[2]);
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
        
        newOrientLabel2 = new JLabel(orientTypes[image.getImageOrientation()]);
        newOrientLabel2.setBackground(Color.black);
        gbc2.gridx = 1;
        newOrientPanel.add(newOrientLabel2, gbc2);
        
        newOrientLabelX = new JLabel("X-axis orientation (image left to right):");
        newOrientLabelX.setFont(serif12);
        newOrientLabelX.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        newOrientPanel.add(newOrientLabelX, gbc2);
        
        newOrientBoxX = new JComboBox(orients);
        newOrientBoxX.setBackground(Color.white);
        newOrientBoxX.setFont(MipavUtil.font12);
        newAxisOrient = image.getFileInfo()[0].getAxisOrientation();
        newOrientBoxX.setSelectedIndex(newAxisOrient[0]);
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
        newOrientBoxY.setSelectedIndex(newAxisOrient[1]);
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
        newOrientBoxZ.setSelectedIndex(presentAxisOrient[2]);
        newOrientBoxZ.addItemListener(this);
        gbc2.gridx = 1;
        newOrientPanel.add(newOrientBoxZ, gbc2);
		
		labelResType = new JLabel("resolution ");
        labelResType.setFont(serif12);
        labelResType.setForeground(Color.black);
        		
		comboResType = new JComboBox(resolutionTypes);
		comboResType.setFont(serif12);
        comboResType.setBackground(Color.white);
		comboResType.setSelectedItem(resolutionType);
		
		labelInterpType = new JLabel("interpolation ");
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
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

		mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.gridwidth = 2;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(presentOrientPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
		gbc.gridwidth = 2;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(newOrientPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = gbc.NONE;
        mainPanel.add(labelResType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
		gbc.gridwidth = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(comboResType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = gbc.NONE;
        mainPanel.add(labelInterpType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
		gbc.gridwidth = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(comboInterpType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = gbc.NONE;
        mainPanel.add(labelTemplate, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
		gbc.gridwidth = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(comboTemplate, gbc);
        
		getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true); 
		setResizable(false);
    	System.gc();
		
	} // end init()
	
    private void buildTemplateList() {
       
        comboTemplate = new JComboBox();
        comboTemplate.setFont(serif12);
        comboTemplate.setBackground(Color.white);

        Enumeration names = ViewUserInterface.getReference().getRegisteredImageNames();

        // Add images from user interface that have the same exact dimensionality
        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();
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
        str += resolutionType + delim;
        str += interpType;

        return str;
    }
	
 	/**
     *  Loads the default settings from Preferences to set up the dialog
     */
	public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                System.out.println(defaultsString);
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
				newOr[0] = new Integer(st.nextToken()).intValue();
                newOr[1] = new Integer(st.nextToken()).intValue();
                newOr[2] = new Integer(st.nextToken()).intValue();
				resolutionType = st.nextToken();
				interpType = st.nextToken();
			}
            catch (Exception ex) {
                // since there was a problem parsing the defaults string, start over with the original defaults
                System.out.println( "Resetting defaults for dialog: " + getDialogName() );
                Preferences.removeProperty( getDialogName() );
            }
        } else {
			System.out.println( "no saved dialogs for "+getDialogName() );
		}
    }
		
    /**
     * Saves the default settings into the Preferences file
     */
	
    public void saveDefaults() {
        String defaultsString = new String( getParameterString(",") );
        System.out.println(defaultsString);
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
                
		if (Preferences.isPreference(Preferences.PREF_SAVE_DEFAULTS) && this.getOwner() != null && !isScriptRunning()) {
			saveDefaults();
		}
		
        if ( algorithm instanceof AlgorithmTransform) {
            resultImage = algoTrans.getTransformedImage();
			if (algorithm.isCompleted() == true && resultImage != null) {
                // The algorithm has completed and produced a new image to be displayed.
				JDialogBase.updateFileInfoStatic(fileInfo, resultImage);
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
		resolutionType = (String)comboResType.getSelectedItem();
		interpType = (String)comboInterpType.getSelectedItem();
		
		String name = (String)comboTemplate.getSelectedItem();
		template = ViewUserInterface.getReference().getRegisteredImageByName(name);
		
		System.out.println(getParameterString("|"));

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
        float ri[] = new float[3];
        int   ni[] = new int[3];
        float r0[] = new float[3];
        int   n0[] = new int[3];
		setVisible(false);
        
		fileInfo = (FileInfoBase)(image.getFileInfo()[0].clone());
		
		// set resampled resolutions, dimensions
		ri[0] = image.getFileInfo()[0].getResolutions()[0];
		ri[1] = image.getFileInfo()[0].getResolutions()[1];
		ri[2] = image.getFileInfo()[0].getResolutions()[2];
		
		ni[0] = image.getExtents()[0];
		ni[1] = image.getExtents()[1];
		ni[2] = image.getExtents()[2];
        
        float r[] = new float[3];
        int   n[] = new int[3];
        for (i = 0; i <= 2; i++) {
            r[i] = ri[i];
            n[i] = ni[i];
        }
		
		if (resolutionType.equals("Finest-Cubic")) {
			float rn = Math.min(r[0],Math.min(r[1],r[2]));
			n[0] = (int)Math.ceil(n[0]*r[0]/rn);
			r[0] = rn;
			n[1] = (int)Math.ceil(n[1]*r[1]/rn);
			r[1] = rn;
			n[2] = (int)Math.ceil(n[2]*r[2]/rn);
			r[2] = rn;
		} else if (resolutionType.equals("Coarsest-Cubic")) {
			float rn = Math.max(r[0],Math.max(r[1],r[2]));
			n[0] = (int)Math.ceil(n[0]*r[0]/rn);
			r[0] = rn;
			n[1] = (int)Math.ceil(n[1]*r[1]/rn);
			r[1] = rn;
			n[2] = (int)Math.ceil(n[2]*r[2]/rn);
			r[2] = rn;
		} else if (resolutionType.equals("Same_as_template")) {
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
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_L2R_TYPE) {
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
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_R2L_TYPE) {
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
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_P2A_TYPE) {
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
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_A2P_TYPE) {
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
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_S2I_TYPE) {
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
                            found = true;
                            X[i][j] = 1.0;
                            r0[i] = r[j];
                            n0[i] = n[j];
                        }
                        else if (newOr[i] == FileInfoBase.ORI_I2S_TYPE) {
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
            fileInfo.setResolutions(r0[i], i);   
            fileInfo.setExtents(n0[i], i);
            fileInfo.setAxisOrientation(newOr[i], i);
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
        fileInfo.setImageOrientation(newOrient);
		
		TransMatrix transform = new TransMatrix(4);
        transform.setMatrix(0, 2, 0, 3, X);
		
		System.out.println(transform.toString());
		
		int interp = AlgorithmTransform.TRILINEAR;
		if (interpType.equals("Nearest_Neighbor")) {
            interp = AlgorithmTransform.NEAREST_NEIGHBOR;
        } else if (interpType.equals("Linear")) {
			interp = AlgorithmTransform.TRILINEAR;
        } else if  (interpType.equals("Windowed_Sinc")) {
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
        newOr[0] = scriptParameters.getParams().getInt("neworientationx");
        newOr[1] = scriptParameters.getParams().getInt("neworientationy");
        newOr[2] = scriptParameters.getParams().getInt("neworientationz");
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
		resolutionType = scriptParameters.getParams().getString("resolution");
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
		scriptParameters.getParams().put(ParameterFactory.newParameter("resolution", resolutionType));
		scriptParameters.getParams().put(ParameterFactory.newParameter("interpolation", interpType));
		
    }
    
    public void itemStateChanged(ItemEvent e) {
        int presentOrient;
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
        else if (e.getSource().equals(newOrientBoxZ)) {
            newOr[2] = newOrientBoxZ.getSelectedIndex();
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
            newOrientLabel2.setText(orientTypes[newOrient]);
        }
    }
}
