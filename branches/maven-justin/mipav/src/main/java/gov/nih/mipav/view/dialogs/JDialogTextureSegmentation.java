package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTextureSegmentation;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


// import javax.swing.*;

/**
 * DOCUMENT ME!
 */
@SuppressWarnings("serial")
public class JDialogTextureSegmentation extends JDialogScriptableBase implements AlgorithmInterface,
                             ActionDiscovery, ScriptableActionInterface, ItemListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage destImage = null; // match image array

    /** DOCUMENT ME! */
    private AlgorithmTextureSegmentation textureSegAlgo;
    
    private int windowSize = 25;
    
    private int segmentNumber = 0;
    
    private boolean nonNegativity = true;
    
    private boolean removeSmallRegions = false;

    private JCheckBox segmentCheckBox;
    
    private JRadioButton noVOIsButton;
    
    private JRadioButton pointButton;
    
    private JRadioButton contourButton;
    
    private ButtonGroup VOIGroup;
    
    private boolean usePoints = false;
    
    private boolean useContours = false;

    private JLabel labelWindowSize;

    private JTextField textWindowSize;

    private JLabel labelSegmentNumber;

    private JTextField textSegmentNumber;

    private JCheckBox nonNegativityCheckBox;

    private JCheckBox removeSmallRegionsCheckBox;
    
    private Vector3f pt[] = null;
    
    private BitSet contourMask = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogTextureSegmentation() {}

    /**
     * Creates new dialog for entering parameters for texture segmentation.
     * 
     * @param theParentFrame Parent frame
     * @param im Source image
     */
    public JDialogTextureSegmentation(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event event that triggers function
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmTextureSegmentation) {
            System.err.println("Texture segmentation Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if ((textureSegAlgo.isCompleted() == true) && (destImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, destImage);
                destImage.clearMask();

                try {
                    new ViewJFrameImage(destImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                insertScriptLine();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        textureSegAlgo.finalize();
        textureSegAlgo = null;
        // dispose();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     * 
     * @param event event that cause the method to fire
     */
    @Override
    public void itemStateChanged(final ItemEvent event) {
        Object source = event.getSource();
        
        if (source != null && source == segmentCheckBox && labelSegmentNumber != null && textSegmentNumber != null &&
        		noVOIsButton != null && pointButton != null && contourButton != null) {
        	labelSegmentNumber.setEnabled(!segmentCheckBox.isSelected());
        	textSegmentNumber.setEnabled(!segmentCheckBox.isSelected());
        	noVOIsButton.setEnabled(!segmentCheckBox.isSelected());
        	pointButton.setEnabled(!segmentCheckBox.isSelected());
        	contourButton.setEnabled(!segmentCheckBox.isSelected());
        	if (segmentCheckBox.isSelected()) {
        		noVOIsButton.setSelected(true);
        		pointButton.setSelected(false);
        		contourButton.setSelected(false);
        	}
        }

    }

    /**
     * Once all the necessary variables are set, call the texture segmentation algorithm.
     */
    @Override
    protected void callAlgorithm() {

        try {

            // Make algorithm
        	destImage = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), image.getImageName()+"_segmented");
            textureSegAlgo = new AlgorithmTextureSegmentation(destImage, image, windowSize, segmentNumber, pt, contourMask,
            		nonNegativity, removeSmallRegions);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            textureSegAlgo.addListener(this);

            createProgressBar(image.getImageName(), textureSegAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (textureSegAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                textureSegAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Texture Segmentation: unable to allocate enough memory");

            return;
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void doPostAlgorithmActions() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        windowSize = scriptParameters.getParams().getInt("window_size");
        segmentNumber = scriptParameters.getParams().getInt("segment_number");
        nonNegativity = scriptParameters.getParams().getBoolean("non_negativity");
        removeSmallRegions = scriptParameters.getParams().getBoolean("remove_small_regions");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("window_size", windowSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("segment_number", segmentNumber));
        scriptParameters.getParams().put(ParameterFactory.newParameter("non_negativity", nonNegativity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("remove_small_regions", removeSmallRegions));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Texture Segmentation");

        labelWindowSize = new JLabel("Window size");
        labelWindowSize.setForeground(Color.black);
        labelWindowSize.setFont(serif12);

        textWindowSize = new JTextField(5);
        textWindowSize.setText("25");
        textWindowSize.setFont(serif12);
        
        segmentCheckBox = new JCheckBox("Automatically determine number of segments");
        segmentCheckBox.setFont(serif12);
        segmentCheckBox.addItemListener(this);
        segmentCheckBox.setSelected(true);

        labelSegmentNumber = new JLabel("Number of segments");
        labelSegmentNumber.setForeground(Color.black);
        labelSegmentNumber.setFont(serif12);
        labelSegmentNumber.setEnabled(false);

        textSegmentNumber = new JTextField(5);
        textSegmentNumber.setText("3");
        textSegmentNumber.setFont(serif12);
        textSegmentNumber.setEnabled(false);
        
        VOIGroup = new ButtonGroup();
        noVOIsButton = new JRadioButton("No User points or contours");
        noVOIsButton.setFont(serif12);
        noVOIsButton.setSelected(true);
        noVOIsButton.setEnabled(false);
        VOIGroup.add(noVOIsButton);;
        
        pointButton = new JRadioButton("User points specify centers of texture windows");
        pointButton.setFont(serif12);
        pointButton.setSelected(false);
        pointButton.setEnabled(false);
        VOIGroup.add(pointButton);
        
        contourButton = new JRadioButton("User contours limit regions of texture windows");
        contourButton.setFont(serif12);
        contourButton.setSelected(false);
        contourButton.setEnabled(false);
        VOIGroup.add(contourButton);

        nonNegativityCheckBox = new JCheckBox("Nonnegativity");
        nonNegativityCheckBox.setFont(serif12);
        nonNegativityCheckBox.setSelected(true);

        removeSmallRegionsCheckBox = new JCheckBox("Remove small regions");
        removeSmallRegionsCheckBox.setFont(serif12);
        removeSmallRegionsCheckBox.setSelected(false);

        final JPanel paramPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(labelWindowSize, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(textWindowSize, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        paramPanel.add(segmentCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(labelSegmentNumber, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(textSegmentNumber, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 2;
        paramPanel.add(noVOIsButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 2;
        paramPanel.add(pointButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 2;
        paramPanel.add(contourButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridwidth = 2;
        paramPanel.add(nonNegativityCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.gridwidth = 2;
        paramPanel.add(removeSmallRegionsCheckBox, gbc);

        getContentPane().add(paramPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        int nPts = 0;
        int nContours = 0;
        Vector<VOIBase> curves;
        VOIVector voiVector;
        VOI presentVOI;
        int i;
        Vector3f[] tmppt = null;
        int num;
        int j;
        int halfWindowSize;
        int xDim;
        int yDim;

        tmpStr = textWindowSize.getText();

        if (testParameter(tmpStr, 1, 255)) {
            windowSize = Integer.valueOf(tmpStr).intValue();
        } else {
            textWindowSize.requestFocus();
            textWindowSize.selectAll();

            return false;
        }
        
        if (segmentCheckBox.isSelected()) {
        	segmentNumber = 0;
        }
        else {
	        tmpStr = textSegmentNumber.getText();
	
	        if (testParameter(tmpStr, 2, 2000)) {
	            segmentNumber = Integer.valueOf(tmpStr).intValue();
	        } else {
	            textSegmentNumber.requestFocus();
	            textSegmentNumber.selectAll();
	
	            return false;
	        }
        }
        
        usePoints = pointButton.isSelected();
        
        if (usePoints) {
        	if (image.getVOIs().size() == 0) {
                MipavUtil.displayError("Select points before clicking OK");

                return false;
            }
        	
        	voiVector = image.getVOIs();
            for (i = 0; i < voiVector.size(); i++) {
                presentVOI = image.getVOIs().VOIAt(i);
                if (presentVOI.getCurveType() == VOI.POINT) {
                	curves = presentVOI.getCurves();
                	nPts += curves.size();
                }
            }
            
            if (nPts != segmentNumber) {
            	MipavUtil.displayError("Segment number = " + segmentNumber + ", but number of points = " + nPts);
            	return false;
            }
            
            pt = new Vector3f[nPts];
            for (i = 0, num = 0; i < voiVector.size(); i++) {
                presentVOI = image.getVOIs().VOIAt(i);
                if (presentVOI.getCurveType() == VOI.POINT) {
                	tmppt = presentVOI.exportAllPoints();
                	for (j = 0; j < tmppt.length; j++) {
                		pt[num++] = tmppt[j];
                	}
                }
            }
            
            halfWindowSize = windowSize/2;
            xDim = image.getExtents()[0];
            yDim = image.getExtents()[1];
            for (i = 0; i < nPts; i++) {
                if ((pt[i].X - halfWindowSize < 0.0) || (pt[i].X + halfWindowSize > xDim - 1) ||
                    (pt[i].Y - halfWindowSize < 0.0) || (pt[i].Y + halfWindowSize > yDim - 1)) {
                	MipavUtil.displayError("Point at X = " + pt[i].X + " Y = " + pt[i].Y + 
                			" cannot be center for a window inside the image");
                	return false;
                    }
            }
        } // if (usePoints)
        
        useContours = contourButton.isSelected();
        
        if (useContours) {
        	if (image.getVOIs().size() == 0) {
                MipavUtil.displayError("Draw contours before clicking OK");

                return false;
            }
        	
        	voiVector = image.getVOIs();
            for (i = 0; i < voiVector.size(); i++) {
                presentVOI = image.getVOIs().VOIAt(i);
                if (presentVOI.getCurveType() == VOI.CONTOUR) {
                	curves = presentVOI.getCurves();
                	nContours += curves.size();
                }
            }
            
            contourMask = image.generateVOIMask();
        } // if (useContours)

        nonNegativity = nonNegativityCheckBox.isSelected();

        removeSmallRegions = removeSmallRegionsCheckBox.isSelected();

        return true;

    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    @Override
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            @Override
            public String getCategory() {
                return new String("Algorithms Texture Segmentation");
            }

            @Override
            public String getDescription() {
                return new String("Applies texture segmentation.");
            }

            @Override
            public String getDescriptionLong() {
                return new String("Applies texture segmentation.");
            }

            @Override
            public String getShortLabel() {
                return new String("Texture segmentation");
            }

            @Override
            public String getLabel() {
                return new String("Texture segmentation");
            }

            @Override
            public String getName() {
                return new String("Texture segmentation");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    @Override
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
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
    @Override
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
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    @Override
    public boolean isActionComplete() {
        return isComplete();
    }

    /**
     * Accessor that returns the image.
     * 
     * @return the result image
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    @Override
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

}
