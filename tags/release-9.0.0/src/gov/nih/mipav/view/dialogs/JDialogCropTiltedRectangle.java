package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmCropTilted;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Creates the dialog to crop pixels around the 4 selected tilted rectangle points.
 *
 * <p>User selects:</p>
 *
 * <ol>
 *   <li>Upper left x,y</li>
 *   <li>Upper right x,y</li>
 *   <li>Bottom right x,y</li>
 *   <li>Bottom left x,y</li>
 * </ol>
 *
 * <p>A new image will be created.</p>
 */
public class JDialogCropTiltedRectangle extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ----------------------------------------------------------------------------------------------

    // or if the source image is to be replaced

   

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    final int VERTICES_METHOD = 1;
    final int VOI_METHOD = 2;
    final int MASK_METHOD = 3;
    int method = VERTICES_METHOD;
    
    private double x1;
    private double x2;
    private double x3;
    private double x4;
    
    private double y1;
    private double y2;
    private double y3;
    private double y4;
    
    private ButtonGroup methodGroup;
    private JRadioButton VOIButton;
    private JRadioButton maskButton;
    private JRadioButton verticesButton;
    
    private AlgorithmCropTilted cropAlgo;
    
   
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCropTiltedRectangle() { }

    /**
     * Constructor that makes dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCropTiltedRectangle(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
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
        		//MipavUtil.showHelp("");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        
        
        if (algorithm instanceof AlgorithmCropTilted) {
        	
        	if (cropAlgo.isCompleted()) {
        		resultImage = cropAlgo.getResultImage();
        		if (resultImage != null) {
        			 try {
                         new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                     } catch (OutOfMemoryError error) {
                         MipavUtil.displayError("Out of memory: unable to open new frame");
                     }

                     insertScriptLine();	
        		}
        	}

            
        // save the completion status for later
        
        setComplete(algorithm.isCompleted());

        cropAlgo.finalize();
        cropAlgo = null;
        dispose();
        }
    }
    
    

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
    	try {
                cropAlgo = new AlgorithmCropTilted(image, x1, y1, x2, y2,
                		x3, y3, x4, y4, method);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                cropAlgo.addListener(this);

                createProgressBar(image.getImageName(), cropAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (cropAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    cropAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog CropTitled: unable to allocate enough memory");

                return;
            }
       
    }

   
    /**
     * Accessor that returns the image after adding image margins.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

   

    /**
     * Initializes the GUI components and places them in the dialog.
     */
    private void init() {
        setTitle("Crop Pixels Around Tilted Rectangle");
        setSize(350, 230);
        setForeground(Color.black);

        JPanel cropPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        methodGroup = new ButtonGroup();
        VOIButton = new JRadioButton("VOI contour or polyline at rectangle boundary", true);
        VOIButton.setFont(serif12);
        VOIButton.setForeground(Color.black);
        VOIButton.addActionListener(this);
        methodGroup.add(VOIButton);
        cropPanel.add(VOIButton,gbc);
        
        maskButton = new JRadioButton("Enter 1 point on the rectangle mask", false);
        maskButton.setFont(serif12);
        maskButton.setForeground(Color.black);
        maskButton.addActionListener(this);
        methodGroup.add(maskButton);
        gbc.gridy = 1;
        cropPanel.add(maskButton, gbc);
        
        verticesButton = new JRadioButton("Enter 4 points at vertices", false);
        verticesButton.setFont(serif12);
        verticesButton.setForeground(Color.black);
        verticesButton.addActionListener(this);
        methodGroup.add(verticesButton);
        gbc.gridy = 2;
        cropPanel.add(verticesButton, gbc);
        
        JLabel labelVertices = new JLabel("in order upper left, upper right");
        labelVertices.setForeground(Color.black);
        labelVertices.setFont(serif12);
        gbc.gridy = 3;
        cropPanel.add(labelVertices, gbc);
        
        JLabel labelVertices2 = new JLabel("lower right, lower left");
        labelVertices2.setForeground(Color.black);
        labelVertices2.setFont(serif12);
        gbc.gridy = 4;
        cropPanel.add(labelVertices2, gbc);
        
        gbc.gridy = 5;
        cropPanel.add(buildButtons(), gbc);

        mainDialogPanel.add(cropPanel);
        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
    	if (verticesButton.isSelected()) {
    		method = VERTICES_METHOD;
    		
    		if ((image.getVOIs() == null) || (image.getVOIs().size() == 0)) {
                MipavUtil.displayError("4 points must be entered");
                return false;
            }
            VOIBaseVector curves = image.getVOIs().VOIAt(0).getCurves();
            int nPts = curves.size();

            if (nPts < 4) {
                MipavUtil.displayError("Number of points = " + nPts + " less than required 4");

                return false;
            }

            Vector3f[] pts = image.getVOIs().VOIAt(0).exportAllPoints();
            x1 = pts[0].X;
            y1 = pts[0].Y;
            x2 = pts[1].X;
            y2 = pts[1].Y;
            x3 = pts[2].X;
            y3 = pts[2].Y;
            x4 = pts[3].X;
            y4 = pts[3].Y;

	        
    	} // if (verticesButton.isSelected())
    	else if (VOIButton.isSelected()) {
    		method = VOI_METHOD;
        
	        int i;
	        int nVOIs;
	        ViewVOIVector VOIs = image.getVOIs();
	        nVOIs = VOIs.size();
	        int nBoundingVOIs = 0;
	        int nActiveBoundingVOIs = 0;
	        
	        for (i = 0; i < nVOIs; i++) {
	
	            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
	                nBoundingVOIs++;
	                if (VOIs.VOIAt(i).isActive()) {
	                    nActiveBoundingVOIs++;	
	                }
	            }
	        } // for (i = 0; i < nVOIs; i++)
	        if (nBoundingVOIs == 0) {
	        	MipavUtil.displayError("Must have one contour or polyline VOI");
	        	return false;
	        }
	        
	        if (nActiveBoundingVOIs > 1) {
	            MipavUtil.displayError("Cannot have more than one active bounding VOI");
	            return false;
	        }
    	} // else if (VOIButton.isSelected())
    	else if (maskButton.isSelected()) {
    		method = MASK_METHOD;
    		if ((image.getVOIs() == null) || (image.getVOIs().size() == 0)) {
                MipavUtil.displayError("1 point must be entered");
                return false;
            }
            
            Vector3f[] pts = image.getVOIs().VOIAt(0).exportAllPoints();
            x1 = pts[0].X;
            y1 = pts[0].Y;
            
    	}

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Utilities.Crop");
            }

            public String getDescription() {
                return new String("Crops image around a tilted rectangle.");
            }

            public String getDescriptionLong() {
                return new String("Crops image around a tilted rectangle.");
            }

            public String getShortLabel() {
                return new String("CropAroundTiltedRectangle");
            }

            public String getLabel() {
                return new String("Crop around Tilted Rectangle");
            }

            public String getName() {
                return new String("Crop around Tilted Rectangle");
            }
        };
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

	@Override
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub
		
	}


}
