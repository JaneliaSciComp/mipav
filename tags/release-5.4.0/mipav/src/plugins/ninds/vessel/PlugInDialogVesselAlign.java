package ninds.vessel;
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

import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.FileNotFoundException;

import javax.swing.*;

/**
 * This class provides the user interface for the vessel alignment plugin.  The dialog has been made scriptable, 
 * meaning it can be executed and recorded as part of a script.  It implements AlgorithmInterface,
 * meaning it has methods for listening to and recording the progress of an algorithm.
 * 
 * @version  July 29, 2011
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInDialogVesselAlign extends JDialogScriptableBase implements AlgorithmInterface {
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

	public enum Interp {
		LINEAR("Linear"),
        CUBIC_CONVOLUTION("Cubic convolution"),
        CUBIC_BSPLINE("Cubic Bspline");

		/** Display name for interpolation */
		private String name;

		Interp(String name) {
			this.name = name;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Enum#toString()
		 */
		public String toString() {
			return name;
		}
	}
	
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** This is your algorithm */
    private PlugInAlgorithmVesselAlign vesselAlgo = null;

    /** Combobox to allow user to select interpolation */
	private JComboBox comboBoxInterp;
	
	/** Type of interpolation to perform between regions of interest. */
	private Interp interpType = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogVesselAlign() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogVesselAlign(Frame theParentFrame, ModelImage im) {
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
       if (algorithm instanceof PlugInAlgorithmVesselAlign) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if ((vesselAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);

                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage); //result image is enlarged to align all necessary vessels
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
            String name = makeImageName(image.getImageName(), "_vesselAligned");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            
            vesselAlgo = new PlugInAlgorithmVesselAlign(resultImage, image);
            vesselAlgo.setInterpType(interpType);
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            vesselAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", vesselAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (vesselAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                vesselAlgo.run();
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
     * Used in turning your plugin into a script
     */
    protected void setGUIFromParams() {
    	image = scriptParameters.retrieveInputImage();

    	interpType = Interp.valueOf(scriptParameters.getParams().getString("interp_type"));
    } //end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.storeInputImage(image);
   
        scriptParameters.getParams().put(ParameterFactory.newParameter("interp_type", interpType.name()));
    } //end storeParamsFromGUI()
   
    private void init() {
        setForeground(Color.black);
        setTitle("Generic Plugin");
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
        mainPanel.setBorder(buildTitledBorder("Choose interpolation"));

        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);
        for(Interp i : Interp.values()) {
        	comboBoxInterp.addItem(i.toString());
        }

        mainPanel.add(labelInterp, gbc);
        gbc.gridx++;
        mainPanel.add(comboBoxInterp, gbc);
        gbc.gridy++;

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
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
		for(Interp i : Interp.values()) {
        	if(i.toString().equals(comboBoxInterp.getSelectedItem().toString())) {
        		interpType = i;
        	}
        }
		if(interpType == null) {
			MipavUtil.displayError("Please select a valid interpolation type.");
			return false;
		}
		return true;
	} //end setVariables()
}
