package gov.nih.mipav.view.dialogs;

//import edu.jhmi.rad.medic.algorithms.*;
//import edu.jhmi.rad.medic.methods.*;
//import edu.jhmi.rad.medic.utilities.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;

/** 
*   
*   Dialog box for Demons 
*
*	@version    July 2005
*	@author     Pilou Bazin
*   @see        AlgorithmDemons
*	@see		DemonsRegistration
*
*
*/  
public class JDialogDemonsLite extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery, DialogDefaultsInterface  {
    
    private     AlgorithmDemonsLite 	algo = null;
    private     ModelImage              image;                // source image
    private     ModelImage              targetImage;          // image to deform to
    
	
	private     ModelImage[]            resultImage; // result image
    private		String[]				outputTypes	=		{"transformed_image", 
																 "transformation_field",
																 "all_images"};
	private		String					outputType	=		"transformed_image";
	
	private 	ViewUserInterface       userInterface;
    
	// parameters
	private 	float 		smoothing       =       2.0f;
	private		float		scale			=		1.0f;
	private		int			levels			=		4;
	private		int			iter			= 		20;
	private     String[]    regTypes 	= {"diffusion-like","fluid-like"};	
    private		String		regType		=		"diffusion-like";
	
	// dialog elements
	private 	JPanel  	mainPanel;
	
	private 	JLabel  	labelImage;
    private 	JLabel  	labelTarget;
    private		JComboBox	comboTarget;
	
	private 	JLabel  	labelOutput;
    private		JComboBox	comboOutput;
	
	private 	JTextField  textSmoothing;
	private 	JLabel  	labelSmoothing;
	private 	JTextField  textScale;
	private 	JLabel  	labelScale;
	private 	JTextField  textRegIter;
	private 	JLabel  	labelRegIter;
	private 	JTextField  textRegLevels;
	private 	JLabel  	labelRegLevels;
	private 	JLabel  	labelRegType;
    private		JComboBox	comboRegType;
	
    /**
    *  Creates dialog for plugin.
    *  @param theParentFrame          Parent frame.
    *  @param im              Source image.
    */
    public JDialogDemonsLite(Frame theParentFrame, ModelImage im) {
		super(theParentFrame, false);
		if (im.getType() == ModelImage.BOOLEAN || im.isColorImage()) {
            MipavUtil.displayError("Source Image must NOT be Boolean or Color"); 
            dispose();
            return;
        }
        image = im;
		userInterface = ((ViewJFrameBase)(parentFrame)).getUserInterface();	    
		loadDefaults();
        init();
	}
	
    /**
    *	Used primarily for the script to store variables and run the algorithm.  No
    *	actual dialog will appear but the set up info and result image will be stored here.
    *	@param UI   The user interface, needed to create the image frame.
    *	@param im	Source image.
    */
    public JDialogDemonsLite(ViewUserInterface UI, ModelImage im) {
        super();
    	userInterface = UI;
		if (im.getType() == ModelImage.BOOLEAN || im.isColorImage()) {
            MipavUtil.displayError("Source Image must NOT be Boolean or Color"); 
            dispose();
            return;
        }
    	image = im;
    }
    
    /**
     * Empty constructor needed for dynamic instantiation.
     */
	 public JDialogDemonsLite() {}
    

    /**
    *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
    */
	private void init(){
        setForeground(Color.black);
        setTitle("Demons 2");
				
 		labelSmoothing = new JLabel("Smoothing kernel ");
		labelSmoothing.setForeground(Color.black);
		labelSmoothing.setFont(serif12);
	
		textSmoothing = new JTextField(5);
		textSmoothing.setText(String.valueOf(smoothing));
		textSmoothing.setFont(serif12);
	
		labelScale = new JLabel("Step scale ");
		labelScale.setForeground(Color.black);
		labelScale.setFont(serif12);
	
		textScale = new JTextField(5);
		textScale.setText(String.valueOf(scale));
		textScale.setFont(serif12);
	
		labelRegIter = new JLabel("Iterations ");
		labelRegIter.setForeground(Color.black);
		labelRegIter.setFont(serif12);
	
		textRegIter = new JTextField(3);
		textRegIter.setText(String.valueOf(iter));
		textRegIter.setFont(serif12);
	
		labelRegLevels = new JLabel("Pyramid levels ");
		labelRegLevels.setForeground(Color.black);
		labelRegLevels.setFont(serif12);
	
		textRegLevels = new JTextField(5);
		textRegLevels.setText(String.valueOf(levels));
		textRegLevels.setFont(serif12);
	
        labelImage = new JLabel("image to register: "+image.getImageName());
        labelImage.setFont(serif12);
        labelImage.setForeground(Color.black);
        		
		labelTarget = new JLabel("target image: ");
        labelTarget.setFont(serif12);
        labelTarget.setForeground(Color.black);
        		
		comboTarget = buildImageList();
		
		
		labelOutput = new JLabel("output: ");
        labelOutput.setFont(serif12);
        labelOutput.setForeground(Color.black);
        		
		comboOutput = new JComboBox(outputTypes);
		comboOutput.setFont(serif12);
        comboOutput.setBackground(Color.white);
		comboOutput.setSelectedItem(outputType);
		
		labelRegType = new JLabel("regularization: ");
        labelRegType.setFont(serif12);
        labelRegType.setForeground(Color.black);
        		
		comboRegType = new JComboBox(regTypes);
		comboRegType.setFont(serif12);
        comboRegType.setBackground(Color.white);
		comboRegType.setSelectedItem(regType);
		
		GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Parameters"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.gridwidth = 4;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(labelImage, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(labelTarget, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
		gbc.gridwidth = 3;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(comboTarget, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(labelSmoothing, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.gridwidth = 3;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(textSmoothing, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(labelScale, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.gridwidth = 3;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(textScale, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(labelRegLevels, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.gridwidth = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(textRegLevels, gbc);
		gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(labelRegIter, gbc);
        gbc.gridx = 1;
        gbc.gridy = 5;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(textRegIter, gbc);
		gbc.gridx = 0;
        gbc.gridy = 8;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(labelRegType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 8;
        gbc.weightx = 1;
        gbc.gridwidth = 3;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(comboRegType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(labelOutput, gbc);
        gbc.gridx = 1;
        gbc.gridy = 11;
        gbc.weightx = 1;
        gbc.gridwidth = 3;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(comboOutput, gbc);
        
		getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true); 
		setResizable(false);
    	System.gc();
		
	} // end init()
	
	
   private JComboBox buildImageList() {
		JComboBox combo;
       
        combo = new JComboBox();
        combo.setFont(serif12);
        combo.setBackground(Color.white);

        Enumeration<String> names = userInterface.getRegisteredImageNames();

        // Add images from user interface that have the same exact dimensionality
        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();
			if (!name.equals(image.getImageName())) {
				ModelImage img = userInterface.getRegisteredImageByName(name);
				if (userInterface.getFrameContainingImage(img) != null) {
					if ( (image.getNDims() == img.getNDims()) && (!img.isColorImage()) ) {
						combo.addItem(name);
					}
				}
			}
		}
		return combo;
    }//buildTargetList
    
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
        str += levels + delim;
        str += iter + delim;
        str += smoothing + delim;
		str += scale + delim;
		str += regType + delim;
		str += outputType;

        return str;
    }
	
 	/**
     *  Loads the default settings from Preferences to set up the dialog
     */
	public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                //System.out.println(defaultsString);
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
				levels = MipavUtil.getInt(st);
				iter = MipavUtil.getInt(st);
				smoothing = MipavUtil.getFloat(st);
				scale = MipavUtil.getFloat(st);
				regType = st.nextToken();
				outputType = st.nextToken();
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
                
		if (Preferences.isPreference(Preferences.PREF_SAVE_DEFAULTS) && this.getOwner() != null && !isScriptRunning()) {
			saveDefaults();
		}
	           
		if ( algorithm instanceof AlgorithmDemonsLite) {
            image.clearMask();
			resultImage = ((AlgorithmDemonsLite)algorithm).getResultImages();
			
			//System.out.println("got output");
			
			if (algorithm.isCompleted() == true && resultImage != null) {
                //System.out.println("send it out");
				//The algorithm has completed and produced a new image to be displayed.
				
				ViewJFrameImage imageFrame[] = new ViewJFrameImage[resultImage.length];
				for (int i = 0; i < resultImage.length; i++) {
                    updateFileInfo(image, resultImage[i]);
                    
                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null,
                                           new Dimension(610, 200 + i * 20));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, 
                                                "Out of memory: unable to open new frame",
                                                "Error", JOptionPane.ERROR_MESSAGE);
                    }
                }
            } else if (resultImage != null) {
                //algorithm failed but result image still has garbage
                for (int i = 0; i < resultImage.length; i++) {
                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }
                resultImage = null;
                System.gc();
            }
			if (algorithm.isCompleted()) {
				insertScriptLine();
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
         
		String name = (String)comboTarget.getSelectedItem();
		targetImage = userInterface.getRegisteredImageByName(name);
		if (targetImage==null) {
			comboTarget.requestFocus();
			return false;
		}
		    	
       	tmpStr = textSmoothing.getText();
        if ( testParameter(tmpStr, 0.0, 10000.0) ){
            smoothing = Float.valueOf(tmpStr).floatValue();
        } else {
            textSmoothing.requestFocus();
            textSmoothing.selectAll();
            return false;
        }
        tmpStr = textScale.getText();
        if ( testParameter(tmpStr, 0.0, 10000.0) ){
            scale = Float.valueOf(tmpStr).floatValue();
        } else {
            textScale.requestFocus();
            textScale.selectAll();
            return false;
        }
        tmpStr = textRegLevels.getText();
        if (testParameter(tmpStr, 0, 10)) {
          levels = Integer.valueOf(tmpStr).intValue();
        } else {
          textRegLevels.requestFocus();
          textRegLevels.selectAll();
          return false;
        }
        tmpStr = textRegIter.getText();
        if (testParameter(tmpStr, 0, 500)) {
          iter = Integer.valueOf(tmpStr).intValue();
        } else {
          textRegIter.requestFocus();
          textRegIter.selectAll();
          return false;
        }
		
		regType = (String)comboRegType.getSelectedItem();
        
		outputType = (String)comboOutput.getSelectedItem();
        
		userInterface.setDataText(getParameterString("|"));
		
    	return true;  	
    }   // end setVariables()
    
    /**
    *	Once all the necessary variables are set, call the Gaussian Blur
    *	algorithm based on what type of image this is and whether or not there
    *	is a separate destination image.
    */
    protected void callAlgorithm() {

        try {
			// Create algorithm
			algo = new AlgorithmDemonsLite(image, targetImage, 
										levels, iter,
										smoothing, scale,
										regType,
										outputType);
			
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algo.addListener(this);
                
            setVisible(false);  // Hide dialog
			
			createProgressBar(image.getImageName(), algo);
                
			// for linux debugs: no threading
			// setSeparateThread(false);

            if (runInSeparateThread) {
                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                //algo.setActiveImage(isActiveImage);
                algo.run();
            }
        } catch (OutOfMemoryError x) {

            resultImage = null;
			System.gc();
            MipavUtil.displayError( "Dialog Itk Demons: unable to allocate enough memory");
            return;
        }
    } // end callAlgorithm()
    
		/**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {
		for (int n=0;n<resultImage.length;n++)
			AlgorithmParameters.storeImageInRunner(resultImage[n]);
	}

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage(1);
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
		targetImage = scriptParameters.retrieveInputImage(2);
        
        levels = scriptParameters.getParams().getInt("levels");
		iter = scriptParameters.getParams().getInt("iterations");
		smoothing = scriptParameters.getParams().getFloat("smoothing");
		scale = scriptParameters.getParams().getFloat("scale");
		regType = scriptParameters.getParams().getString("reg_type");
		outputType = scriptParameters.getParams().getString("output_type");
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     * 
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeInputImage(targetImage);
        for (int n=0;n<resultImage.length;n++) {
			scriptParameters.storeImageInRecorder(resultImage[n]);
		}
		
        scriptParameters.getParams().put(ParameterFactory.newParameter("levels", levels));
		scriptParameters.getParams().put(ParameterFactory.newParameter("iterations", iter));
		scriptParameters.getParams().put(ParameterFactory.newParameter("smoothing", smoothing));
		scriptParameters.getParams().put(ParameterFactory.newParameter("scale", scale));
		scriptParameters.getParams().put(ParameterFactory.newParameter("reg_type", regType));
		scriptParameters.getParams().put(ParameterFactory.newParameter("output_type", outputType));
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Registration");
            }

            public String getDescription() {
                return new String("Performs non-linear registration with the DEMONS algorithm." +
				"Note: For reg_type, type 'diffusion-like', or 'fluid-like'." +
				"For output_type, type 'transformed_image', 'transformation_field' or 'all_images'");
            }

            public String getDescriptionLong() {
                return new String("Performs non-linear registration with the DEMONS algorithm." +
				"Note: For reg_type, type 'diffusion-like', or 'fluid-like'." +
				"For output_type, type 'transformed_image', 'transformation_field' or 'all_images'");
            }

            public String getShortLabel() {
                return new String("DemonsLite");
            }

            public String getLabel() {
                return new String("Demons Lite");
            }

            public String getName() {
                return new String("Demons Lite");
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

			table.put(new ParameterFloat("smoothing", 2.0f));
			table.put(new ParameterFloat("scale", 1.0f));
		    table.put(new ParameterInt("levels", 4));
			table.put(new ParameterInt("iterations", 20));

			//Choose "diffusion-like", or "fluid-like"
            table.put(new ParameterString("reg_type", "diffusion-like"));
            
            //Choose "transformed_image", "transformation_field" or "all_images"
            table.put(new ParameterString("output_type", "transformed_image"));
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
            if (resultImage != null) {
                // algo produced a new result image
                return resultImage[0].getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
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
