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
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileInfoDicom;

import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.*;

/**
 * This class displays a basic dialog for a MIPAV plug-in.  The dialog has been made scriptable, 
 * meaning it can be executed and recorded as part of a script.  It implements AlgorithmInterface,
 * meaning it has methods for listening to and recording the progress of an algorithm.
 * 
 * @version  September 28, 2011
 * @see      JDialogScriptableBase
 * @see      AlgorithmInterface
 *
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInDialogDicomStitch extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The stitched image */
    private ModelImage resultImage = null;

    /** Images that will be stitched together */
    private ModelImage origImage, stitchImage; // 
    
    /** Algorithm which performs dicom image stitching between two images. */
    private PlugInAlgorithmDicomStitch dicomAlgo = null;
	
	/** Internal builder for gui components */
	private GuiBuilder guiBuilder;

	/** Gui comboboxes for selecting images */
    private JComboBox origCombo, toStitchCombo;

    /** Gui component for seperate images */
    private JCheckBox doImageBCheckBox;

    /** Whether to display the images as a single frame with separate images. */
    private boolean doImageB;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogDicomStitch() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogDicomStitch(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        if(!isEligibleImage(im)) {
            MipavUtil.displayError("The selected image does not contain populated tag values for (0020,0032), (0020,0037), or (0028,0030)");
            return;
        }
        
        origImage = im;
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
        } else {
            super.actionPerformed(event);
        }
    } // end actionPerformed()

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
       if (algorithm instanceof PlugInAlgorithmDicomStitch) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            origImage.clearMask();
            
            if ((dicomAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(origImage, resultImage);

                resultImage.clearMask();

                try {
                    //new ViewJFrameImage(resultImage);
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

        try {
            resultImage = (ModelImage) origImage.clone();
            resultImage.setImageName(origImage.getImageName()+"_Stitched");
            
            dicomAlgo = new PlugInAlgorithmDicomStitch(resultImage, origImage, stitchImage, doImageB);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            dicomAlgo.addListener(this);
            createProgressBar(origImage.getImageName(), " ...", dicomAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (dicomAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                dicomAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Dicom stitch algorithm: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

    /**
     * Used in turning your plugin into a script
     */
    protected void setGUIFromParams() {
        origImage = scriptParameters.retrieveImage("origImage");
        stitchImage = scriptParameters.retrieveImage("stitchImage");
        
        doImageB = scriptParameters.getParams().getBoolean("doImageB");
    } //end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeImage(origImage, "origImage");
        scriptParameters.storeImage(stitchImage, "stitchImage");
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("doImageB", doImageB));
    } //end storeParamsFromGUI()
   
    private void init() {
        guiBuilder = new GuiBuilder(this);
        
        setForeground(Color.black);
        setTitle("Dicom Stitch Plugin");
        try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Dicom stitching images"));

        int selectedIndex = 0, totalImages = 0;
        ArrayList<String> validImageName = new ArrayList<String>();
        Enumeration<ModelImage> enumStr = ViewUserInterface.getReference().getRegisteredImages();
        while(enumStr.hasMoreElements()) {
            ModelImage img = enumStr.nextElement();
            if(origImage.getImageName().equals(img.getImageName())) {
                selectedIndex = totalImages;
            }
            if(isEligibleImage(img)) {
                totalImages++;
                validImageName.add(img.getImageName());
            }
        }
        
        String[] validImageNameArr = validImageName.toArray(new String[validImageName.size()]);
        
        origCombo = guiBuilder.buildComboBox("Original image", validImageNameArr, selectedIndex);
        toStitchCombo = guiBuilder.buildComboBox("Stitching image", validImageNameArr, selectedIndex == 0 && totalImages > 1 ? 1 : 0);
        doImageBCheckBox = guiBuilder.buildCheckBox("Overlay stitch as seperate images", doImageB);
        
        JLabel labelVOI = new JLabel("<html>Select the images that will be stitched together." +
        		                        "<br>Any images shown below are valid for stitching.</html>");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        mainPanel.add(labelVOI, gbc);

        gbc.gridy++;
        mainPanel.add(origCombo.getParent(), gbc);
        gbc.gridy++;
        mainPanel.add(toStitchCombo.getParent(), gbc);
        gbc.gridy++;
        mainPanel.add(doImageBCheckBox.getParent(), gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = guiBuilder.buildOKCancelPanel();

        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()

    private boolean isEligibleImage(ModelImage img) {
        if(img.isDicomImage() && ((FileInfoDicom)img.getFileInfo()[0]).getTagTable().get(new FileDicomKey("0020,0037")) != null
                && ((FileInfoDicom)img.getFileInfo()[0]).getTagTable().get(new FileDicomKey("0020,0032")) != null
                && ((FileInfoDicom)img.getFileInfo()[0]).getTagTable().get(new FileDicomKey("0028,0030")) != null) {
                    return true;
                }
        return false;
    }

    /**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
	private boolean setVariables() {
		if(origCombo.getSelectedIndex() == toStitchCombo.getSelectedIndex()) {
		    MipavUtil.displayInfo("Select two different images for stitching.");
		    return false;
		}
		
		origImage = ViewUserInterface.getReference().getRegisteredImageByName(origCombo.getSelectedItem().toString());
		stitchImage = ViewUserInterface.getReference().getRegisteredImageByName(toStitchCombo.getSelectedItem().toString());
		doImageB = doImageBCheckBox.isSelected();
		
		if(origImage == null || stitchImage == null) {
		    MipavUtil.displayInfo("Could not find selected images");
		    return false;
		}
		
		if(!isEligibleImage(origImage) || !isEligibleImage(stitchImage)) { //checking again useful for scripting
		    MipavUtil.displayInfo("Selected images are not valid dicom images");
		    return false;
		}
		
		return true;
	} //end setVariables()
}
