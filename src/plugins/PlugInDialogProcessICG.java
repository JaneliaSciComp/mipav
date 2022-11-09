import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.util.Vector;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class PlugInDialogProcessICG extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 9196816548519771470L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton replaceDistortButton = null;

    private JRadioButton removeDistortButton = null;
    
    private JCheckBox checkBlinksBox;
    
    private JRadioButton replaceBlinksButton = null;
    private JRadioButton removeBlinksButton = null;
    
    
    private JCheckBox checkDistortionBox;
    
    /** DOCUMENT ME! */
    private double distortionThreshold = .8;

    /** DOCUMENT ME! */
    private boolean doDistortion = true;

    private boolean doBlinks = true;
    
    private boolean doReplaceBlinks = true;
    
    private boolean doReplaceDistortion = true;
    
    /** DOCUMENT ME! */
    private JTextField frameField = null;

    /** */
    private JTextField desiredFrameField = null;
    
    private JCheckBox desiredFrameBox = null;
    
    /** DOCUMENT ME! */
    private PlugInAlgorithmProcessICG icgAlgo;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private int registrationFrame = 1;

    private int desiredFrames = 0;
    
    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private JTextField sdField = null;

    /** DOCUMENT ME! */
    private String[] titles = null;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    private String versionNum = ".5";
    
            //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new PlugInDialogProcessICG object.
     */
    public PlugInDialogProcessICG() { }


    /**
     * Sets variables needed to call algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image
     */
    public PlugInDialogProcessICG(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);

        image = imA;

        if (image.getNDims() != 3) {
            MipavUtil.displayError("Image must be 2.5D");

            return;
        }

        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
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
        } else if (command.equals("Script")) {
            callAlgorithm();
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
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof PlugInAlgorithmProcessICG) {
            System.err.println("ICG Process completed");

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

            resultImage = ((PlugInAlgorithmProcessICG)algorithm).getResultImage();
            
            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
            
            if (parentFrame != null) {
                userInterface.registerFrame(parentFrame);
                ((ViewJFrameImage) parentFrame).initExtentsVariables(((ViewJFrameImage) parentFrame).getActiveImage());
                ((ViewJFrameImage) parentFrame).setTitle();
                ((ViewJFrameImage) parentFrame).updateImageExtents();
            }

        }
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent e) {

        if (e.getSource() == checkDistortionBox) {
        	
            if (checkDistortionBox.isSelected()) {
            	removeDistortButton.setEnabled(true);
            	replaceDistortButton.setEnabled(true);
                sdField.setEnabled(true);
            } else {
            	removeDistortButton.setEnabled(false);
            	replaceDistortButton.setEnabled(false);
                sdField.setEnabled(false);
            }
        } else if (e.getSource() == checkBlinksBox) {
        	if (checkBlinksBox.isSelected()) {
            	removeBlinksButton.setEnabled(true);
            	replaceBlinksButton.setEnabled(true);
            } else {
            	removeBlinksButton.setEnabled(false);
            	replaceBlinksButton.setEnabled(false);
            }
        } else if (e.getSource() == desiredFrameBox) {
        	desiredFrameField.setEnabled(desiredFrameBox.isSelected());
        	if (desiredFrameBox.isSelected()) {
        		checkDistortionBox.setSelected(true);
        		removeDistortButton.setSelected(true);
        		checkDistortionBox.setEnabled(false);
        		removeDistortButton.setEnabled(false);
            	replaceDistortButton.setEnabled(false);
        	} else {
        		checkDistortionBox.setEnabled(true);
        		removeDistortButton.setEnabled(checkDistortionBox.isSelected());
            	replaceDistortButton.setEnabled(checkDistortionBox.isSelected());
        	}
        	
        }
    }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {

        try {

            // unregister the frame holding the source image

            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
            titles = new String[imageFrames.size()];

            for (int i = 0; i < imageFrames.size(); i++) {
                titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
            }

            // Make algorithm
            icgAlgo = new PlugInAlgorithmProcessICG(image, registrationFrame, distortionThreshold, doBlinks,
            		doReplaceBlinks, doDistortion, doReplaceDistortion, desiredFrames);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            icgAlgo.addListener(this);

            createProgressBar(image.getImageName(), " ...", icgAlgo);

            // Hide dialog
            setVisible(false);

            if(isRunInSeparateThread()) {
    	        // Start the thread as a low priority because we wish to still have user interface work fast
    	        if (icgAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
    	            MipavUtil.displayError("A thread is already running on this object");
    	        }
            } else {
            	icgAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog RGB to Gray: unable to allocate enough memory");

            return;
        }
    }
  
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();


        doDistortion = scriptParameters.getParams().getBoolean("doDistortion");

        if (doDistortion) {
            distortionThreshold = scriptParameters.getParams().getDouble("distortionThreshold");
            doReplaceDistortion = scriptParameters.getParams().getBoolean("doReplaceDistortion");
        }
        
        doBlinks = scriptParameters.getParams().getBoolean("doBlinks");
        if (doBlinks) {
        	doReplaceBlinks = scriptParameters.getParams().getBoolean("doReplaceBlinks");
        }
        
        desiredFrames = scriptParameters.getParams().getInt("desiredFrames");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeOutputImageParams(getResultImage(), true);
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("doDistortion", doDistortion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("doReplaceDistortion", doReplaceDistortion)); 
        scriptParameters.getParams().put(ParameterFactory.newParameter("doBlinks", doBlinks)); 
        scriptParameters.getParams().put(ParameterFactory.newParameter("doReplaceBlinks", doReplaceBlinks)); 
        scriptParameters.getParams().put(ParameterFactory.newParameter("distortionThreshold", distortionThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("desiredFrames", desiredFrames));
    } 

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
            AlgorithmParameters.storeImageInRunner(getResultImage());
       
    }
    
    /**
     * DOCUMENT ME!
     */
    private void init() {

        setTitle("NEI Plugin - version " + versionNum);

        PanelManager optionsPanel = new PanelManager("Options");
        sdField = WidgetFactory.buildTextField(Double.toString(distortionThreshold));
        MipavUtil.makeNumericsOnly(sdField, true);

        int currentFrame = ((ViewJFrameImage) parentFrame).getViewableSlice();
        frameField = WidgetFactory.buildTextField(Integer.toString(currentFrame + 1));
        MipavUtil.makeNumericsOnly(frameField, false);

        checkDistortionBox = WidgetFactory.buildCheckBox("Check for distortion post registration", true, this);
        checkDistortionBox.setEnabled(false);
        
        checkBlinksBox = WidgetFactory.buildCheckBox("Check for blinks pre registration", true, this);
        
        ButtonGroup group2 = new ButtonGroup();
        replaceBlinksButton = WidgetFactory.buildRadioButton("Replace blinks", false, group2);
        removeBlinksButton = WidgetFactory.buildRadioButton("Remove blinks", true, group2);
        
        
        ButtonGroup group = new ButtonGroup();
        replaceDistortButton = WidgetFactory.buildRadioButton("Replace poorly aligned frames", false, group);
        removeDistortButton = WidgetFactory.buildRadioButton("Remove poorly aligned frames", true, group);
        replaceDistortButton.setEnabled(false);
        removeDistortButton.setEnabled(false);
        
        desiredFrameBox = WidgetFactory.buildCheckBox("Desired number of frames", true, this);
                
        desiredFrameField = WidgetFactory.buildTextField("15");
        MipavUtil.makeNumericsOnly(desiredFrameField, false);

        optionsPanel.add(checkBlinksBox);
        optionsPanel.addOnNextLine(removeBlinksButton);
        optionsPanel.addOnNextLine(replaceBlinksButton);
        
        optionsPanel.addOnNextLine(checkDistortionBox);
        optionsPanel.addOnNextLine(removeDistortButton);
        optionsPanel.addOnNextLine(replaceDistortButton);
        optionsPanel.addOnNextLine(WidgetFactory.buildLabel("Standard deviation: "));
        optionsPanel.add(sdField);

        optionsPanel.addOnNextLine(WidgetFactory.buildLabel("Reference frame: "));
        optionsPanel.add(frameField);
        optionsPanel.addOnNextLine(desiredFrameBox);
        optionsPanel.add(desiredFrameField);
        getContentPane().add(optionsPanel.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(false);

    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {

        if (replaceDistortButton.isSelected()) {

            try {
                distortionThreshold = Double.parseDouble(sdField.getText());
            } catch (Exception e) {
                sdField.requestFocus();

                return false;
            }
        }

        try {
            registrationFrame = Integer.parseInt(frameField.getText());

            if ((registrationFrame < 1) || (registrationFrame > image.getExtents()[2])) {
                MipavUtil.displayWarning("Registration frame must be between 1 and " + image.getExtents()[2]);
            }
        } catch (Exception e) {
            frameField.requestFocus();

            return false;
        }

        doBlinks = checkBlinksBox.isSelected();
        doReplaceBlinks = replaceBlinksButton.isSelected();
        
        doDistortion = checkDistortionBox.isSelected();
        doReplaceDistortion = replaceDistortButton.isSelected();

        if (desiredFrameBox.isSelected()) {
        	desiredFrames = Integer.parseInt(desiredFrameField.getText());
        }
        
        return true;
    }
}
