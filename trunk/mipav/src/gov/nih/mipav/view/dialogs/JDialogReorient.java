package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.io.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;

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
    
	private static final	float   ISQRT2 = (float)(1.0/Math.sqrt(2.0f));
		
	// parameters
	private     String[]    orientTypes 	= 		{"Axial",
													 "Coronal",
													 "Sagittal"};	
    private		String		orientType		=		"Axial";
		
	private     String[]    resolutionTypes 	= 	{"Unchanged",
													 "Finest-Cubic",
													 "Coarsest-Cubic",
													 "Same_as_template"};	
    private		String		resolutionType		=		"Unchanged";
	
	private		String[]	interpTypes	=	{"Nearest_Neighbor",
												 "Linear",
												 "Windowed_Sinc"};
	private		String		interpType	=	"Linear";
    
	// storage for header information
	FileInfoBase 	fileInfo;
	
    // dialog elements
	private 	JPanel  	mainPanel;
	private 	JLabel  	labelOrientType;
    private		JComboBox	comboOrientType;
	private 	JLabel  	labelResType;
    private		JComboBox	comboResType;
	private		JLabel		labelTemplate;	
	private		JComboBox	comboTemplate;
	private		JLabel		labelInterpType;	
	private		JComboBox	comboInterpType;
	
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
        setForeground(Color.black);
        setTitle("Reorientation / Resampling");
				
 		labelOrientType = new JLabel("orientation ");
        labelOrientType.setFont(serif12);
        labelOrientType.setForeground(Color.black);
        		
		comboOrientType = new JComboBox(orientTypes);
		comboOrientType.setFont(serif12);
        comboOrientType.setBackground(Color.white);
		comboOrientType.setSelectedItem(orientType);
		
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
        gbc.gridwidth = 1;
        gbc.fill = gbc.NONE;
        mainPanel.add(labelOrientType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
		gbc.gridwidth = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(comboOrientType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = gbc.NONE;
        mainPanel.add(labelResType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
		gbc.gridwidth = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(comboResType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = gbc.NONE;
        mainPanel.add(labelInterpType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
		gbc.gridwidth = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(comboInterpType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = gbc.NONE;
        mainPanel.add(labelTemplate, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
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
        int j;
       
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
        str += orientType + delim;
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
				orientType = st.nextToken();
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
	           
		int i;
        ViewJFrameImage imageFrame;
		
        if ( algorithm instanceof AlgorithmTransform) {
            resultImage = algoTrans.getTransformedImage();
			if (algorithm.isCompleted() == true && resultImage != null) {
                // The algorithm has completed and produced a new image to be displayed.
				MedicUtil.updateFileInfo(fileInfo, resultImage);
                resultImage.clearMask();
				resultImage.calcMinMax();

                try {
					imageFrame = new ViewJFrameImage(resultImage, null,
                                           new Dimension(610, 200) );
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
    	String tmpStr;
         
		orientType = (String)comboOrientType.getSelectedItem();
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

		setVisible(false);
        
		fileInfo = (FileInfoBase)(image.getFileInfo()[0].clone());
		
		// set resampled resolutions, dimensions
		float r0x = image.getFileInfo()[0].getResolutions()[0];
		float r0y = image.getFileInfo()[0].getResolutions()[1];
		float r0z = image.getFileInfo()[0].getResolutions()[2];
		
		int n0x = image.getExtents()[0];
		int	n0y = image.getExtents()[1];
		int	n0z = image.getExtents()[2];
			
		float rx = r0x, ry = r0y, rz= r0z;
		int nx = n0x, ny = n0y, nz = n0z;
		
		if (resolutionType.equals("Finest-Cubic")) {
			float rn = Numerics.min(rx,ry,rz);
			nx = Numerics.ceil(nx*rx/rn);
			rx = rn;
			ny = Numerics.ceil(ny*ry/rn);
			ry = rn;
			nz = Numerics.ceil(nz*rz/rn);
			rz = rn;
		} else if (resolutionType.equals("Coarsest-Cubic")) {
			float rn = Numerics.max(rx,ry,rz);
			nx = Numerics.ceil(nx*rx/rn);
			rx = rn;
			ny = Numerics.ceil(ny*ry/rn);
			ry = rn;
			nz = Numerics.ceil(nz*rz/rn);
			rz = rn;
		} else if (resolutionType.equals("Same_as_template")) {
			rx = template.getFileInfo()[0].getResolutions()[0];
			ry = template.getFileInfo()[0].getResolutions()[1];
			rz = template.getFileInfo()[0].getResolutions()[2];
			nx = template.getExtents()[0];
			ny = template.getExtents()[1];
			nz = template.getExtents()[2];
		}
		
		// compute the matrix from re-orientation information
		RotationMatrix rot = new RotationMatrix();
		
		// retrieve image info
		int orient = image.getImageOrientation();
		int orx = image.getFileInfo()[0].getAxisOrientation(0);
		int ory = image.getFileInfo()[0].getAxisOrientation(1);
		int orz = image.getFileInfo()[0].getAxisOrientation(2);
		
		float[] ao = new float[3];
		for (int i=0;i<3;i++) ao[i] = 1.0f;
		float[] at = new float[3];
		for (int i=0;i<3;i++) at[i] = 0.0f;
			
		if (orientType.equals("Axial")) {
			// note : assumes a rotation around the image center
			if (orient==FileInfoBase.AXIAL) {
				rot.setParameters(0.0f,0.0f,0.0f);
			
				if (orx==FileInfoBase.ORI_L2R_TYPE) { ao[0] = -1.0f; at[0] = r0x*(n0x-1); }
				if (ory==FileInfoBase.ORI_P2A_TYPE) { ao[1] = -1.0f; at[1] = r0y*(n0y-1); }
				if (orz==FileInfoBase.ORI_S2I_TYPE) { ao[2] = -1.0f; at[2] = r0z*(n0z-1); }
			
			} else if (orient==FileInfoBase.CORONAL) {
				rot.setParameters(-ISQRT2,0.0f,0.0f);
				at[2] = r0y*(n0y-1); 
				
				if (orx==FileInfoBase.ORI_L2R_TYPE) { ao[0] = -1.0f; at[0] = r0x*(n0x-1); }
				if (ory==FileInfoBase.ORI_I2S_TYPE) { ao[1] = -1.0f; at[1] = r0z*(n0z-1); }
				if (orz==FileInfoBase.ORI_P2A_TYPE) { ao[2] = -1.0f; at[2] = 0.0f; }
			
				float rt = ry; ry = rz; rz = rt;
				int   nt = ny; ny = nz; nz = nt;
			} else if (orient==FileInfoBase.SAGITTAL) {
				rot.setParameters(-0.5f,-0.5f,0.5f);
				at[0] = r0z*(n0z-1);
				at[2] = r0y*(n0y-1);
				
				if (orx==FileInfoBase.ORI_P2A_TYPE) { ao[0] = -1.0f; at[0] = 0.0f; }
				if (ory==FileInfoBase.ORI_I2S_TYPE) { ao[1] = -1.0f; at[1] = r0x*(n0x-1); }
				if (orz==FileInfoBase.ORI_R2L_TYPE) { ao[2] = -1.0f; at[2] = 0.0f; }
				
				float rt = rz; rz = ry; ry = rx; rx = rt;
				int   nt = nz; nz = ny; ny = nx; nx = nt;
			}
			fileInfo.setImageOrientation(FileInfoBase.AXIAL);
			fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE,0);
			fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE,1);
			fileInfo.setAxisOrientation(FileInfoBase.ORI_I2S_TYPE,2);
		} else if (orientType.equals("Coronal")) {
			// note : assumes a rotation around the image center
			if (orient==FileInfoBase.CORONAL) {
				rot.setParameters(0.0f,0.0f,0.0f);
			
				if (orx==FileInfoBase.ORI_L2R_TYPE) { ao[0] = -1.0f; at[0] = r0x*(n0x-1); }
				if (ory==FileInfoBase.ORI_I2S_TYPE) { ao[1] = -1.0f; at[1] = r0y*(n0y-1); }
				if (orz==FileInfoBase.ORI_P2A_TYPE) { ao[2] = -1.0f; at[2] = r0z*(n0z-1); }
			
			} else if (orient==FileInfoBase.AXIAL) {
				rot.setParameters(ISQRT2,0.0f,0.0f);
				at[1] = r0z*(n0z-1); 
				
				if (orx==FileInfoBase.ORI_L2R_TYPE) { ao[0] = -1.0f; at[0] = r0x*(n0x-1); }
				if (ory==FileInfoBase.ORI_P2A_TYPE) { ao[1] = -1.0f; at[1] = 0.0f; }
				if (orz==FileInfoBase.ORI_S2I_TYPE) { ao[2] = -1.0f; at[2] = r0y*(n0y-1); }
			
				float rt = ry; ry = rz; rz = rt;
				int   nt = ny; ny = nz; nz = nt;
			} else if (orient==FileInfoBase.SAGITTAL) {
				rot.setParameters(0.0f,-ISQRT2,0.0f);
				at[0] = r0z*(n0z-1);
				
				if (orx==FileInfoBase.ORI_P2A_TYPE) { ao[0] = -1.0f; at[0] = 0.0f; }
				if (ory==FileInfoBase.ORI_I2S_TYPE) { ao[1] = -1.0f; at[1] = r0y*(n0y-1); }
				if (orz==FileInfoBase.ORI_R2L_TYPE) { ao[2] = -1.0f; at[2] = r0x*(n0x-1); }
				
				float rt = rz; rz = rx; rx = rt;
				int   nt = nz; nz = nx; nx = nt;
			}
			fileInfo.setImageOrientation(FileInfoBase.CORONAL);
			fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE,0);
			fileInfo.setAxisOrientation(FileInfoBase.ORI_S2I_TYPE,1);
			fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE,2);
		}  else if (orientType.equals("Sagittal")) {
			// note : assumes a rotation around the image center
			if (orient==FileInfoBase.SAGITTAL) {
				rot.setParameters(0.0f,0.0f,0.0f);
			
				if (orx==FileInfoBase.ORI_P2A_TYPE) { ao[0] = -1.0f; at[0] = r0x*(n0x-1); }
				if (ory==FileInfoBase.ORI_I2S_TYPE) { ao[1] = -1.0f; at[1] = r0y*(n0y-1); }
				if (orz==FileInfoBase.ORI_R2L_TYPE) { ao[2] = -1.0f; at[2] = r0z*(n0z-1); }
			
			} else if (orient==FileInfoBase.CORONAL) {
				rot.setParameters(0.0f,ISQRT2,0.0f);
				at[2] = r0x*(n0x-1);
				
				if (orx==FileInfoBase.ORI_L2R_TYPE) { ao[0] = -1.0f; at[0] = r0z*(n0z-1); }
				if (ory==FileInfoBase.ORI_I2S_TYPE) { ao[1] = -1.0f; at[1] = r0y*(n0y-1); }
				if (orz==FileInfoBase.ORI_P2A_TYPE) { ao[2] = -1.0f; at[2] = 0.0f; }
				
				float rt = rz; rz = rx; rx = rt;
				int   nt = nz; nz = nx; nx = nt;
			} else if (orient==FileInfoBase.AXIAL) {
				rot.setParameters(0.5f,0.5f,-0.5f);
				at[1] = r0z*(n0z-1);
				at[2] = r0x*(n0x-1);
				
				if (orx==FileInfoBase.ORI_L2R_TYPE) { ao[0] = -1.0f; at[0] = r0y*(n0y-1); }
				if (ory==FileInfoBase.ORI_P2A_TYPE) { ao[1] = -1.0f; at[1] = 0.0f; }
				if (orz==FileInfoBase.ORI_S2I_TYPE) { ao[2] = -1.0f; at[2] = 0.0f; }
				
				float rt = rx; rx = ry; ry = rz; rz = rt;
				int   nt = nx; nx = ny; ny = nz; nz = nt;
			} 
			fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
			fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE,0);
			fileInfo.setAxisOrientation(FileInfoBase.ORI_S2I_TYPE,1);
			fileInfo.setAxisOrientation(FileInfoBase.ORI_L2R_TYPE,2);
		}

		fileInfo.setResolutions(rx,0);
		fileInfo.setResolutions(ry,1);
		fileInfo.setResolutions(rz,2);
		fileInfo.setExtents(nx,0);
		fileInfo.setExtents(ny,1);
		fileInfo.setExtents(nz,2);
		
		TransMatrix transform = new TransMatrix(4);
        double X[][] = new double[4][4];
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                X[i][j] = rot.getMatrix(i, j)*ao[i];
            }
            X[i][3] = at[i];
        }
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
			
		algoTrans = new AlgorithmTransform(image, transform, interp, rx, ry, rz, nx, ny, nz, 
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

        orientType = scriptParameters.getParams().getString("orientation");
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

        scriptParameters.getParams().put(ParameterFactory.newParameter("orientation", orientType));
		scriptParameters.getParams().put(ParameterFactory.newParameter("resolution", resolutionType));
		scriptParameters.getParams().put(ParameterFactory.newParameter("interpolation", interpType));
		
    }
}
