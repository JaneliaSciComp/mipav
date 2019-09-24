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
 * Creates the dialog to crop pixels around the 8 selected tilted cuboid points.
 *
 * <p>User selects:</p>
 *
 * <ol>
 *   <li>Front upper left x,y</li>
 *   <li>Front upper right x,y</li>
 *   <li>Front bottom right x,y</li>
 *   <li>Front bottom left x,y</li>
 *   <li>Back upper left x,y</li>
 *   <li>Back upper right x,y</li>
 *   <li>Back bottom right x,y</li>
 *   <li>Back bottom left x,y</li>
 * </ol>
 *
 * <p>A new image will be created.</p>
 */
public class JDialogCropTiltedCuboid extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ----------------------------------------------------------------------------------------------

    // or if the source image is to be replaced

   

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private double x1;
    private double x2;
    private double x3;
    private double x4;
    private double x5;
    private double x6;
    private double x7;
    private double x8;
    
    private double y1;
    private double y2;
    private double y3;
    private double y4;
    private double y5;
    private double y6;
    private double y7;
    private double y8;
    
    private double z1;
    private double z2;
    private double z3;
    private double z4;
    private double z5;
    private double z6;
    private double z7;
    private double z8;
    
    final int VERTICES_METHOD = 1;
    final int VOI_METHOD = 2;
    final int MASK_METHOD = 3;
    int method = VERTICES_METHOD;
    
    private ButtonGroup methodGroup;
    //private JRadioButton VOIButton;
    private JRadioButton maskButton;
    private JRadioButton verticesButton;
    
    private AlgorithmCropTilted cropAlgo;
    
   
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCropTiltedCuboid() { }

    /**
     * Constructor that makes dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCropTiltedCuboid(Frame theParentFrame, ModelImage im) {
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
                cropAlgo = new AlgorithmCropTilted(image, x1, y1, z1, x2, y2, z2,
                		x3, y3, z3, x4, y4, z4, x5, y5, z5, x6, y6, z6,
                		x7, y7, z7, x8, y8, z8, method);

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
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        
    }

    /**
     * Initializes the GUI components and places them in the dialog.
     */
    private void init() {
        setTitle("Crop Pixels Around Tilted Cuboid");
        setSize(350, 650);
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
        maskButton = new JRadioButton("Use cuboid volume from mask generated from VOI", true);
        maskButton.setFont(serif12);
        maskButton.setForeground(Color.black);
        maskButton.addActionListener(this);
        methodGroup.add(maskButton);
        cropPanel.add(maskButton, gbc);
        
        verticesButton = new JRadioButton("Enter 8 points at vertices", false);
        verticesButton.setFont(serif12);
        verticesButton.setForeground(Color.black);
        verticesButton.addActionListener(this);
        methodGroup.add(verticesButton);
        gbc.gridy = 1;
        cropPanel.add(verticesButton, gbc);

        JLabel labelVertices = new JLabel("in order front upper left, front upper right");
        labelVertices.setForeground(Color.black);
        labelVertices.setFont(serif12);
        gbc.gridy = 2;
        cropPanel.add(labelVertices, gbc);
        
        JLabel labelVertices2 = new JLabel("front lower right, front lower left");
        labelVertices2.setForeground(Color.black);
        labelVertices2.setFont(serif12);
        gbc.gridy = 3;
        cropPanel.add(labelVertices2, gbc);
        
        JLabel labelVertices3= new JLabel("back upper left, back upper right");
        labelVertices3.setForeground(Color.black);
        labelVertices3.setFont(serif12);
        gbc.gridy = 4;
        cropPanel.add(labelVertices3, gbc);
        
        JLabel labelVertices4 = new JLabel("front lower right, front lower left");
        labelVertices4.setForeground(Color.black);
        labelVertices4.setFont(serif12);
        gbc.gridy = 5;
        cropPanel.add(labelVertices4, gbc);

        
        gbc.gridy = 6;
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
                MipavUtil.displayError("8 points must be entered");
                return false;
            }
            VOIBaseVector curves = image.getVOIs().VOIAt(0).getCurves();
            int nPts = curves.size();

            if (nPts < 8) {
                MipavUtil.displayError("Number of points = " + nPts + " less than required 8");

                return false;
            }

            Vector3f[] pts = image.getVOIs().VOIAt(0).exportAllPoints();
            x1 = pts[0].X;
            y1 = pts[0].Y;
            z1 = pts[0].Z;
            x2 = pts[1].X;
            y2 = pts[1].Y;
            z2 = pts[1].Z;
            x3 = pts[2].X;
            y3 = pts[2].Y;
            z3 = pts[2].Z;
            x4 = pts[3].X;
            y4 = pts[3].Y;
            z4 = pts[3].Z;
            x5 = pts[4].X;
            y5 = pts[4].Y;
            z5 = pts[4].Z;
            x6 = pts[5].X;
            y6 = pts[5].Y;
            z6 = pts[5].Z;
            x7 = pts[6].X;
            y7 = pts[6].Y;
            z7 = pts[6].Z;
            x8 = pts[7].X;
            y8 = pts[7].Y;
            z8 = pts[7].Z;
	        
    	} // if (verticesButton.isSelected())
    	else if (maskButton.isSelected()) {
    		
    	    method = MASK_METHOD;
        
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
	        
    	} // else if (maskButton.isSelected())
    	

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
                return new String("Crops image around a tilted cuboid.");
            }

            public String getDescriptionLong() {
                return new String("Crops image around a tilted cuboid.");
            }

            public String getShortLabel() {
                return new String("CropAroundTiltedCuboid");
            }

            public String getLabel() {
                return new String("Crop around Tilted Cuboid");
            }

            public String getName() {
                return new String("Crop around Tilted Cuboid");
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


}
