package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmGenerateIsolines;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class JDialogGenerateIsolines extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {
	//~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textThreshold;
    
    private double threshold;
    
    private Color contourColor;
    
    private JButton colorButton;
    
    private ViewJColorChooser colorChooser;
    
    private float intensityR;
    private float intensityG;
    private float intensityB;
    
    private double imageMin;
    private double imageMax;
    private double defaultThreshold;
    
    private AlgorithmGenerateIsolines isoAlgo;
	
	 /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
	public JDialogGenerateIsolines() {}
	
	public JDialogGenerateIsolines(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        imageMin = image.getMin();
        imageMax = image.getMax();
        defaultThreshold = (imageMin + imageMax)/2.0;
        init();
	}
	
	/**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Generate isolines");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelThreshold = new JLabel(String.valueOf(imageMin) + " <= Threshold <= " + String.valueOf(imageMax)+")");
        labelThreshold.setForeground(Color.black);
        labelThreshold.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelThreshold, gbc);

        textThreshold = new JTextField(10);
        textThreshold.setText(String.valueOf(defaultThreshold));
        textThreshold.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textThreshold, gbc);
        
        JLabel colorButtonLabel = new JLabel("Isolines color: ");
        colorButtonLabel.setForeground(Color.black);
    	colorButtonLabel.setFont(serif12);
    	gbc.gridx = 0;
    	gbc.gridy++;
    	paramPanel.add(colorButtonLabel, gbc);
    	
    	contourColor = new Color(255,0,0);
    	intensityR = (float)(imageMax);
    	intensityG = 0.0f;
    	intensityB = 0.0f;
    	colorButton = new JButton();
    	colorButton.setBackground(contourColor);
    	colorButton.addActionListener(this);
    	colorButton.setActionCommand("contourColor");
    	colorButton.setToolTipText("Click to change grid color");
    	gbc.gridx = 1;
    	paramPanel.add(colorButton, gbc);
    	
    	 JPanel buttonPanel = new JPanel();
         buildOKButton();
         buttonPanel.add(OKButton);
         buildCancelButton();
         buttonPanel.add(cancelButton);
         
         getContentPane().add(paramPanel);
         getContentPane().add(buttonPanel, BorderLayout.SOUTH);

         pack();
         //setResizable(false);
         setVisible(true);
    }
    
    public void actionPerformed(ActionEvent e) {
		
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("OK")) {
			if(setVariables()) {
				callAlgorithm();
				
			}
		}else if(command.equalsIgnoreCase("Cancel")) {
			dispose();
		}else if (command.equalsIgnoreCase("contourColor")) {
                        	
			colorChooser = new ViewJColorChooser(null, "Pick contour color", new ActionListener() { // OKAY listener
                public void actionPerformed(ActionEvent ae) {
                	contourColor = colorChooser.getColor();
                	colorButton.setBackground(contourColor);
                	intensityR = (float)colorChooser.getColor().getRed();
                	intensityG = (float)colorChooser.getColor().getGreen();
                	intensityB = (float)colorChooser.getColor().getBlue();
                	float intensityMax = Math.max(intensityR,Math.max(intensityG,intensityB));
                	double scale = imageMax/intensityMax;
                	intensityR = (float)(scale * intensityR);
                	intensityG = (float)(scale * intensityG);
                	intensityB = (float)(scale * intensityB);
                }
            }, new ActionListener() { // CANCEL listener
                public void actionPerformed(ActionEvent a) { }
            });
        } else {
            super.actionPerformed(e);
        }

	}
    
    private boolean setVariables() {

        System.gc();

        String tmpStr;
        
        tmpStr = textThreshold.getText();

        if (testParameter(tmpStr, imageMin, imageMax)) {
            threshold = Double.valueOf(tmpStr).doubleValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();

            return false;
        }
        return true;
    }
	
	protected void callAlgorithm() {
		String name = makeImageName(image.getImageName(), "_Isolines");

        try {
        	
            resultImage     = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), name);
            if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags(resultImage.getNDims() > 2,
                		resultImage.getFileInfo()[0].getDataType());
            }

            // Make algorithm
            isoAlgo = new AlgorithmGenerateIsolines(resultImage, image, threshold, intensityR, intensityG, intensityB);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            isoAlgo.addListener(this);

            createProgressBar(image.getImageName(), isoAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (isoAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                isoAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Generate Isolines: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }	
	}
	
	/**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmGenerateIsolines) {
            System.err.println("Generate isolines Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((isoAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        isoAlgo.finalize();
        isoAlgo = null;
        dispose();
    }
    
    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }
	
	protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        threshold = scriptParameters.getParams().getDouble("thresh");
        intensityR = scriptParameters.getParams().getFloat("intenR");
        intensityG = scriptParameters.getParams().getFloat("intenG");
        intensityB = scriptParameters.getParams().getFloat("intenB");
	}
	
	 protected void storeParamsFromGUI() throws ParserException {
	        scriptParameters.storeInputImage(image);
	        scriptParameters.storeOutputImageParams(resultImage, true);
	        scriptParameters.getParams().put(ParameterFactory.newParameter("thresh", threshold));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("intenR", intensityR));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("intenG", intensityG));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("intenB", intensityB));
	 }
	 
	 /**
	     * Return meta-information about this discoverable action for categorization and labeling purposes.
	     * 
	     * @return Metadata for this action.
	     */
	    public ActionMetadata getActionMetadata() {
	        return new MipavActionMetadata() {
	            public String getCategory() {
	                return new String("Utilities (Generates isolines)");
	            }

	            public String getDescription() {
	                return new String("Generates isolines");
	            }

	            public String getDescriptionLong() {
	                return new String("Generates isolines");
	            }

	            public String getShortLabel() {
	                return new String("GeneratesIsolines");
	            }

	            public String getLabel() {
	                return new String("Generates isolines");
	            }

	            public String getName() {
	                return new String("Generates isolines");
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
	            table.put(new ParameterDouble("thresh", defaultThreshold));
	            table.put(new ParameterFloat("intenR", (float)imageMax));
	            table.put(new ParameterFloat("intenG", 0.0f));
	            table.put(new ParameterFloat("intenB", 0.0f));
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
	            } else {
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