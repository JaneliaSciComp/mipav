package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.Vector;

import javax.swing.JPanel;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.JPanelAlgorithmOutputOptions;

public class JDialogBGAndFGDistanceMap extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {
	/** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JPanelAlgorithmOutputOptions outputPanel;
    
    private AlgorithmMorphology2D distanceMapAlgo2D;
    
    private AlgorithmMorphology3D distanceMapAlgo3D = null;
    
    /** DOCUMENT ME! */
    private String[] titles;
	
	

	public JDialogBGAndFGDistanceMap() {
		
	}
	
	public JDialogBGAndFGDistanceMap(Frame theParentFrame, ModelImage im) {
		
		if ((im.getType() != ModelImage.BOOLEAN) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be BOOLEAN, UNSIGNED BYTE or UNSIGNED SHORT");
            dispose();

            return;
        }

        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
		
	}
	
	public void init() {
		 setForeground(Color.black);

	        setTitle("BG and FG Distance map");
	        
	        outputPanel = new JPanelAlgorithmOutputOptions(image);
	        
	        JPanel mainPanel = new JPanel(new GridBagLayout());

	        GridBagConstraints gbc = new GridBagConstraints();
	        gbc.anchor = GridBagConstraints.WEST;
	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.gridwidth = 1;
	        gbc.weightx = 1;
	        gbc.fill = GridBagConstraints.BOTH;
	        gbc.insets = new Insets(5, 5, 5, 5);
	        mainPanel.add(outputPanel, gbc);

	        JPanel buttonPanel = new JPanel();
	        buildOKButton();
	        buttonPanel.add(OKButton);
	        buildCancelButton();
	        buttonPanel.add(cancelButton);
	        buildHelpButton();
	        buttonPanel.add(helpButton);
	        
	        

	        mainDialogPanel.add(mainPanel);
	        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

	        getContentPane().add(mainDialogPanel);

	        pack();
	        setVisible(true);
	}
	

	protected void callAlgorithm() {
		String name = makeImageName(image.getImageName(), "_BGAndFGDistance");
		if (image.getNDims() == 2) { // source image is 2D
			if (outputPanel.isOutputNewImageSet()) {
	
	            try {
	
	                // Make result image of float type
	                resultImage = (ModelImage) image.clone();
	                resultImage.setImageName(name);
	                
	                distanceMapAlgo2D = new AlgorithmMorphology2D(resultImage, 0, 0, AlgorithmMorphology2D.DISTANCE_MAP_FOR_SHAPE_INTERPOLATION, 0, 0, 0, 0, outputPanel.isProcessWholeImageSet());
	                
	                
	                // This is very important. Adding this object as a listener allows the algorithm to
	                // notify this object when it has completed of failed. See algorithm performed event.
	                // This is made possible by implementing AlgorithmedPerformed interface
	                distanceMapAlgo2D.addListener(this);
	
	                createProgressBar(resultImage.getImageName(), distanceMapAlgo2D);
	                
	                if (outputPanel.isProcessWholeImageSet() == false) {
	                	distanceMapAlgo2D.setMask(resultImage.generateVOIMask());
	                }
	
	                // Hide dialog
	                setVisible(false);
	
	                if (isRunInSeparateThread()) {
	                    // Start the thread as a low priority because we wish to still have user interface work fast.
	                    if (distanceMapAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
	                } else {
	                	//to maintain proper script execution, directly calling run
	                	distanceMapAlgo2D.run();
	                }
	            }catch (OutOfMemoryError x) {
	                MipavUtil.displayError("Dialog bg and fg distance map: unable to allocate enough memory");
	
	                return;
	            }
			}else {
				try {
	
	                // No need to make new image space because the user has choosen to replace the source image
	                // Make the algorithm class
	                distanceMapAlgo2D = new AlgorithmMorphology2D(image, 0, 0, AlgorithmMorphology2D.DISTANCE_MAP_FOR_SHAPE_INTERPOLATION, 0, 0, 0, 0, outputPanel.isProcessWholeImageSet());
	
	                // This is very important. Adding this object as a listener allows the algorithm to
	                // notify this object when it has completed of failed. See algorithm performed event.
	                // This is made possible by implementing AlgorithmedPerformed interface
	                distanceMapAlgo2D.addListener(this);
	                
	                createProgressBar(image.getImageName(), distanceMapAlgo2D);
	                
	
	                if (outputPanel.isProcessWholeImageSet() == false) {
	                	distanceMapAlgo2D.setMask(image.generateVOIMask());
	                }
	
	                // Hide the dialog since the algorithm is about to run.
	                setVisible(false);
	
	                // These next lines set the titles in all frames where the source image is displayed to
	                // "locked - " image name so as to indicate that the image is now read/write locked!
	                // The image frames are disabled and then unregisted from the userinterface until the
	                // algorithm has completed.
	                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
	                titles = new String[imageFrames.size()];
	
	                for (int i = 0; i < imageFrames.size(); i++) {
	                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
	                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
	                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
	                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
	                }
	
	                if (isRunInSeparateThread()) {
	                    // Start the thread as a low priority because we wish to still have user interface work fast.
	                    if (distanceMapAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
	                } else {
	                	//to maintain proper script execution, directly calling run
	                	distanceMapAlgo2D.run();
	                }
	            } catch (OutOfMemoryError x) {
	                MipavUtil.displayError("Dialog distance map: unable to allocate enough memory");
	
	                return;
	            }
				
			}
		}else if (image.getNDims() == 3) {
			if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    distanceMapAlgo3D = new AlgorithmMorphology3D(resultImage, 0, 0,
                                                                  AlgorithmMorphology3D.DISTANCE_MAP_FOR_SHAPE_INTERPOLATION, 0, 0, 0, 0,
                                                                  outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        distanceMapAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    distanceMapAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), distanceMapAlgo3D);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {
	                    // Start the thread as a low priority because we wish to still have user interface work fast.
	                    if (distanceMapAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
                    } else {
                    	//to maintain proper script execution, directly calling run
                    	distanceMapAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog distance map: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    distanceMapAlgo3D = new AlgorithmMorphology3D(image, 0, 0,
                                                                  AlgorithmMorphology3D.DISTANCE_MAP_FOR_SHAPE_INTERPOLATION, 0, 0, 0, 0,
                                                                  outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        distanceMapAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    distanceMapAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), distanceMapAlgo3D);
                    
                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {
	                    // Start the thread as a low priority because we wish to still have user interface work fast.
	                    if (distanceMapAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
                    } else {
                    	//to maintain proper script execution, directly calling run
                    	distanceMapAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog distance map: unable to allocate enough memory");

                    return;
                }
            }
		}

	}

	 /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams(){        
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);
    }
    
    /**
     * Record the parameters just used to run this algorithm in a script.
     * 
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException{
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputPanel.isOutputNewImageSet());
       
        scriptParameters.storeProcessWholeImage(outputPanel.isProcessWholeImageSet());
     }
    
    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the image table).
     * Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {
        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

	public void algorithmPerformed(AlgorithmBase algorithm) {
		 if (algorithm instanceof AlgorithmMorphology2D) {
			 image.clearMask();
			 if ((distanceMapAlgo2D.isCompleted() == true) && (resultImage != null)) {
	                updateFileInfo(image, resultImage);
	                resultImage.clearMask();

	                // The algorithm has completed and produced a new image to be displayed.
	                try {
	                    resultImage.setImageName("Bg. distance map image");
	                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
	                } catch (OutOfMemoryError error) {
	                    MipavUtil.displayError("Out of memory: unable to open new frame");
	                }
	            } else if (resultImage == null) {

	                // These next lines set the titles in all frames where the source image is displayed to
	                // image name so as to indicate that the image is now unlocked!
	                // The image frames are enabled and then registed to the userinterface.
	                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

	                for (int i = 0; i < imageFrames.size(); i++) {
	                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
	                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

	                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
	                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
	                    }
	                }

	                if (parentFrame != null) {
	                    userInterface.registerFrame(parentFrame);
	                }

	                image.notifyImageDisplayListeners(null, true);
	            } else if (resultImage != null) {

	                // algorithm failed but result image still has garbage
	                resultImage.disposeLocal(); // clean up memory
	                resultImage = null;
	            }
	            
		 }else if(algorithm instanceof AlgorithmMorphology3D) {
			 image.clearMask();
			 if ((distanceMapAlgo3D.isCompleted() == true) && (resultImage != null)) {
	                updateFileInfo(image, resultImage);
	                resultImage.clearMask();

	                // The algorithm has completed and produced a new image to be displayed.
	                try {
	                    resultImage.setImageName("Bg. distance map image");
	                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
	                } catch (OutOfMemoryError error) {
	                    MipavUtil.displayError("Out of memory: unable to open new frame");
	                }
	            } else if (resultImage == null) {

	                // These next lines set the titles in all frames where the source image is displayed to
	                // image name so as to indicate that the image is now unlocked!
	                // The image frames are enabled and then registed to the userinterface.
	                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

	                for (int i = 0; i < imageFrames.size(); i++) {
	                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
	                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

	                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
	                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
	                    }
	                }

	                if (parentFrame != null) {
	                    userInterface.registerFrame(parentFrame);
	                }

	                image.notifyImageDisplayListeners(null, true);
	            } else if (resultImage != null) {

	                // algorithm failed but result image still has garbage
	                resultImage.disposeLocal(); // clean up memory
	                resultImage = null;
	            }
		 }

	}

	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();

        if (command.equals("OK")) {
                callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("");
        } else {
            super.actionPerformed(event);
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
                return new String("Algorithms.Morphological");
            }

            public String getDescription() {
                return new String("Creates a distance map for shape interpolation of the image.");
            }

            public String getDescriptionLong() {
                return new String("Creates a distance map for shape interpolation of the image.");
            }

            public String getShortLabel() {
                return new String("BGAndFGDistanceMap");
            }

            public String getLabel() {
                return new String("BG + FG Distance Map");
            }

            public String getName() {
                return new String("BG + FG Distance Map");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
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
                return resultImage.getImageName();
            } 
            else {
                // algo was done in place
                return image.getImageName();
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
