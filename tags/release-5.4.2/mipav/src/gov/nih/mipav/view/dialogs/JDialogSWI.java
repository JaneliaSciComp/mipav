package gov.nih.mipav.view.dialogs;
//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmSWI;



import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterDouble;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.*;


/**
 * This class displays a basic dialog for a MIPAV plug-in.  The dialog has been made scriptable, 
 * meaning it can be executed and recorded as part of a script.  It implements AlgorithmInterface,
 * meaning it has methods for listening to and recording the progress of an algorithm.
 * 
 * @version  June 3, 2011
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class JDialogSWI extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** This is the SWIs algorithm */
    private AlgorithmSWI swiAlgo = null;

	/** The variable representing whether the blur should be performed. */
	private boolean doErnst;

    private String[] titles;
    
    private GuiBuilder guiBuilder;

    private JTextField maskThresholdField;

    private JTextField xFilterSizeField;

    private JTextField yFilterSizeField;

    private double maskThreshold = 5000;

    private int xFilterSize = 64;

    private int yFilterSize = 64;

    private JComboBox magnitudeCombo;

    private JComboBox phaseCombo;
    
    private JCheckBox showInterImagesBox;

    private JTextField multFactorField;

    private int multFactor = 4;

    private ModelImage magImage;

    private ModelImage phaseImage;

    private boolean showInterImages = false;
    
    private String resultImageName = "result_image";
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public JDialogSWI() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogSWI(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
        init();
    }
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

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
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        }
    } // end actionPerformed()

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
       if (algorithm instanceof AlgorithmSWI) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            resultImage = algorithm.getDestImage();
            resultImage.setImageName(resultImageName);
            if ((swiAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);

                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage);
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

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
            
            setComplete(algorithm.isCompleted());

            if (algorithm != null) {
                algorithm.finalize();
                algorithm = null;
            }

            dispose();
        }

    } // end algorithmPerformed()

    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {
        System.out.println("Algorithm is called");
        try {
            String name = makeImageName(image.getImageName(), "_T1Map");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            
        
            
            swiAlgo = new AlgorithmSWI(isScriptRunning(), resultImage, magImage, phaseImage, 
                                                    maskThreshold, xFilterSize, yFilterSize, multFactor, showInterImages);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            swiAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", swiAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                System.out.println("Algorithm is started");
                if (swiAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                System.out.println("Algorithm is running");
                swiAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Generic algorithm: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
    	AlgorithmParameters.storeImageInRunner(resultImage);
    }
    
    /**
     * Used in turning the plugin into a script
     */
    protected void setGUIFromParams() {
    	xFilterSize = scriptParameters.getParams().getInt("xFilterSize");
    	yFilterSize = scriptParameters.getParams().getInt("yFilterSize");
    	multFactor = scriptParameters.getParams().getInt("multFactor");
    	showInterImages = scriptParameters.getParams().getBoolean("showInterImages");
    	maskThreshold = scriptParameters.getParams().getDouble("maskThreshold");
    	
    	magImage = scriptParameters.retrieveImage("MagnitudeImage");
    	image = magImage;
    	phaseImage = scriptParameters.retrieveImage("PhaseImage");
    } //end setGUIFromParams()

    /**
     * Used in turning the plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeImage(magImage, "MagnitudeImage");
        scriptParameters.storeImage(phaseImage, "PhaseImage");
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("xFilterSize", xFilterSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("yFilterSize", yFilterSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("multFactor", multFactor));
        scriptParameters.getParams().put(ParameterFactory.newParameter("showInterImages", showInterImages));
        scriptParameters.getParams().put(ParameterFactory.newParameter("maskThreshold", maskThreshold));
        
        if(swiAlgo != null && showInterImages) {
            scriptParameters.storeOutputImageParams(swiAlgo.getkImage(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getiImage(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getBrainMask(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getkCenterImage(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getiFinal(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getPhaseMask(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getiCenter(), true);
        }
        scriptParameters.storeOutputImageParams(swiAlgo.getDestImage(), true);  //magEnhancedFINAL image
        
        //algorithm shows inter images, but referencing them here allows JIST to see them for possible post-processing
    } //end storeParamsFromGUI()
   
    private void init() {
        setForeground(Color.black);
        setTitle("SWI Plugin 5.3.4a");
        guiBuilder = new GuiBuilder(this);
        
        try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}
        
        JPanel imagePanel = new JPanel(new GridBagLayout());
        imagePanel.setBorder(MipavUtil.buildTitledBorder("Select images"));
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        titles = new String[ViewUserInterface.getReference().getRegisteredImagesNum()];
        Enumeration<String> imageStr = ViewUserInterface.getReference().getRegisteredImageNames();
        int index = 0;
        int phaseDefault = 0, magDefault = 0;
        String imageEl = null;
        while(imageStr.hasMoreElements()) {
            imageEl = imageStr.nextElement();
            if(imageEl.matches(".*(p|P)ha.*")) {
                phaseDefault = index;
            } else if(imageEl.matches(".*(m|M)ag.*")) {
                magDefault = index;
            }
            titles[index] = imageEl;
            index++;
        }
        
        
        magnitudeCombo = guiBuilder.buildComboBox("Magnitude:", titles, magDefault);
        imagePanel.add(magnitudeCombo.getParent(), gbc);
        gbc.gridy++;
        
        phaseCombo = guiBuilder.buildComboBox("Phase:", titles, phaseDefault);
        imagePanel.add(phaseCombo.getParent(), gbc);
        gbc.gridy++;
        
        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(MipavUtil.buildTitledBorder("SWI Parameters"));
        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        maskThresholdField = guiBuilder.buildDecimalField("Brain Mask Threshold", .5e4);
        xFilterSizeField = guiBuilder.buildIntegerField("Ro filter size", 64);
        yFilterSizeField = guiBuilder.buildIntegerField("Pe filter size", 64);
        multFactorField = guiBuilder.buildIntegerField("Multiplication factor", 4);
        showInterImagesBox = guiBuilder.buildCheckBox("Show intermediate images", true);
        
        paramPanel.add(maskThresholdField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(xFilterSizeField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(yFilterSizeField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(multFactorField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(showInterImagesBox.getParent(), gbc);
        gbc.gridy++;
        
        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        mainPanel.add(imagePanel, gbc);
        gbc.gridy++;
        mainPanel.add(paramPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = guiBuilder.buildOKCancelPanel();

        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()

    /**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
	private boolean setVariables() {
	    
	    maskThreshold = Double.valueOf(maskThresholdField.getText()).doubleValue();
	    xFilterSize = Integer.valueOf(xFilterSizeField.getText()).intValue();
	    yFilterSize = Integer.valueOf(yFilterSizeField.getText()).intValue();
	    multFactor = Integer.valueOf(multFactorField.getText()).intValue();
	    showInterImages = showInterImagesBox.isSelected();
	    
	    try {
	        magImage = ViewUserInterface.getReference().getRegisteredImageByName(magnitudeCombo.getSelectedItem().toString());
            
            phaseImage = ViewUserInterface.getReference().getRegisteredImageByName(phaseCombo.getSelectedItem().toString());
	    } catch(Exception e) {
	        MipavUtil.displayError("Please select magnitude and phase images.");
	        return false;
	    }
        
		return true;
	} //end setVariables()

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.MRI");
            }

            public String getDescription() {
                return new String("Creates susceptibility weighted image.");
            }

            public String getDescriptionLong() {
                return new String("Models magnetic susceptibility of anatomy in MRI.");
            }

            public String getShortLabel() {
                return new String("SWI534a");
            }

            public String getLabel() {
                return new String("SWI534a");
            }

            public String getName() {
                return new String("SWI534a");
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
        	table.put(new ParameterExternalImage("MagnitudeImage"));
            table.put(new ParameterExternalImage("PhaseImage"));
        	
        	table.put(new ParameterInt("xFilterSize", xFilterSize));
            table.put(new ParameterInt("yFilterSize", yFilterSize));
            table.put(new ParameterInt("multFactor", multFactor));
            table.put(new ParameterBoolean("showInterImages", showInterImages));
            table.put(new ParameterDouble("maskThreshold", maskThreshold));
            
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
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
    public String getOutputImageName(String imageParamName) {
    	if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
    		return resultImageName;
        }

    	System.out.println("Unrecognized output image parameter: " + imageParamName);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        System.out.println("Returning result of SWI action: "+isComplete());
    	return isComplete();
    }
}
