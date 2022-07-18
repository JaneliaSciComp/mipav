package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

public class JDialogTextureAnalysis extends JDialogScriptableBase implements AlgorithmInterface {
	private ModelImage image; // source image
	
	private ModelImage[] resultImage = null; // result image
	
	private JCheckBox scaleCheckBox;
	
	private boolean scaleImage = false;
	
	private AlgorithmTextureAnalysis textureAlgo;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogTextureAnalysis() { }


    /**
     * Creates a new JDialogTextureAnalysis object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogTextureAnalysis(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        setVisible(true);
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
           // MipavUtil.showWebHelp("Filters_(Spatial):Texture_Analysis");
        } else {
            super.actionPerformed(event);
        }
    }
    
    private void init() {
        setForeground(Color.black);

        setTitle("Texture Analysis");
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel;

        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.BOTH;
        
        scaleCheckBox = new JCheckBox("Scale images to 219 by 146");
        scaleCheckBox.setFont(serif12);
        scaleCheckBox.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(scaleCheckBox, gbc);
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        // setVisible( true );

        System.gc();
    }
    
    private boolean setVariables() {
    	scaleImage = scaleCheckBox.isSelected();
    	return true;
    }
    
    protected void callAlgorithm() {
    	int i;
    	int extents[] = new int[2];
    	try {
            resultImage = new ModelImage[9];
            if (scaleImage) {
            	extents[0] = 219;
            	extents[1] = 146;
            }
            else {
            	extents[0] = image.getExtents()[0];
            	extents[1] = image.getExtents()[1];
            }
            resultImage[0] = new ModelImage(ModelStorageBase.DOUBLE, extents, "Texture Synthesis");
            resultImage[1] = new ModelImage(ModelStorageBase.DOUBLE, extents, "Texture Amplitude");
            resultImage[2] = new ModelImage(ModelStorageBase.DOUBLE, extents, "Texture Frequency Magnitude");
            resultImage[3] = new ModelImage(ModelStorageBase.DOUBLE, extents, "Edge Synthesis");
            resultImage[4] = new ModelImage(ModelStorageBase.DOUBLE, extents, "Edge Amplitude");
            resultImage[5] = new ModelImage(ModelStorageBase.DOUBLE, extents, "Edge Frequency Magnitude");
            resultImage[6] = new ModelImage(ModelStorageBase.DOUBLE, extents, "Percent Texture");
            resultImage[7] = new ModelImage(ModelStorageBase.DOUBLE, extents, "Percent Edge");
            resultImage[8] = new ModelImage(ModelStorageBase.DOUBLE, extents, "Percent Smooth");
            
            textureAlgo = new AlgorithmTextureAnalysis(resultImage, image, scaleImage);
            
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            textureAlgo.addListener(this);
            createProgressBar(image.getImageName(), textureAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (textureAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                textureAlgo.run();
            }
           
    	}
    	catch (OutOfMemoryError x) {

            if (resultImage != null) {

                for (i = 0; i < 9; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
            }
            
         // save the completion status for later
            setComplete(textureAlgo.isCompleted());

            System.gc();
            MipavUtil.displayError("Dialog Texture Analysis: unable to allocate enough memory");

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
        int i;
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[9];

        if (algorithm instanceof AlgorithmTextureAnalysis) {
            image.clearMask();

            if ((textureAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                // Take resultImage out of array form or null pointer errors can
                // result in one of the resultImages after another of the resultImages
                // has been deleted.
            	
            	
            	// save the completion status for later
            	setComplete(textureAlgo.isCompleted());

                for (i = 0; i < 9; i++) {
                    updateFileInfo(image, resultImage[i]);
                    resultImage[i].clearMask();
                    
                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new resultImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }

                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < 9; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();

    }
    
    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage[] getResultImage() {
        return resultImage;
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        for (int i = 0; i < 9; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
    }
    
    public void setScaleImage(boolean scaleImage) {
    	this.scaleImage = scaleImage;
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        setScaleImage(scriptParameters.getParams().getBoolean("scale_image"));
    }
    
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        for (int i = 0; i < 9; i++) {
            scriptParameters.storeImageInRecorder(getResultImage()[i]);
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("scale_image", scaleImage));
    }

}