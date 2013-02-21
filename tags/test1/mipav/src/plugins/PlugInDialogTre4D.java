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

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.*;

/**
 * This class displays a basic dialog for a MIPAV plug-in.  The dialog has been made scriptable, 
 * meaning it can be executed and recorded as part of a script.  It implements AlgorithmInterface,
 * meaning it has methods for listening to and recording the progress of an algorithm.
 * 
 * @version  June 4, 2010
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInDialogTre4D extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** The tre4D algorithm */
    private PlugInAlgorithmTre4D treAlgo = null;

    private String[] titles;

    private ModelImage dceHigh;

    private ModelImage r1;

    private ModelImage newM0;

    private ModelImage b1;

    private JComboBox dceCombo;

    private JComboBox newM0Combo;

    private JComboBox r1Combo;

    private JComboBox b1Combo;

    private JTextField flipAngleField;

    private JTextField repTimeField;

    private JTextField dceCenterField;

    private int fa;

    private double trTime;

    private String dceCenterFile;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogTre4D() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogTre4D(Frame theParentFrame, ModelImage im) {
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
       if (algorithm instanceof PlugInAlgorithmTre4D) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if ((treAlgo.isCompleted() == true) && (resultImage != null)) {

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
            String name = makeImageName(image.getImageName(), "_kidneys");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            
            treAlgo = new PlugInAlgorithmTre4D(resultImage, image, dceHigh, r1, newM0, b1, fa, trTime, dceCenterFile);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            treAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", treAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (treAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                treAlgo.run();
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
    } //end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.storeInputImage(image);
    } //end storeParamsFromGUI()
   
    private void init() {
        setForeground(Color.black);
        setTitle("TRE4D Plugin");
        
        try {
            setIconImage(MipavUtil.getIconImage("divinci.gif"));
        } catch (FileNotFoundException e) {
            Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
        }
		
		GuiBuilder guiBuilder = new GuiBuilder(this);
        
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
        int dceGuess = 0, r1Guess = 0, newM0Guess = 0, b1Guess = 0;
        while(imageStr.hasMoreElements()) {
            titles[index] = imageStr.nextElement();
            if(titles[index].indexOf("DCE") != -1) {
                dceGuess = index;
            } else if(titles[index].indexOf("r1") != -1) {
                r1Guess = index;
            } else if(titles[index].indexOf("m0") != -1) {
                newM0Guess = index;
            } else if(titles[index].indexOf("b1") != -1) {
                b1Guess = index;
            }
            index++;
            
        }
        
        dceCombo = guiBuilder.buildComboBox("DCE Full:", titles, dceGuess);
        imagePanel.add(dceCombo.getParent(), gbc);
        gbc.gridy++;
        
        r1Combo = guiBuilder.buildComboBox("R1 results:", titles, r1Guess);
        imagePanel.add(r1Combo.getParent(), gbc);
        gbc.gridy++;
        
        newM0Combo = guiBuilder.buildComboBox("M0 results:", titles, newM0Guess);
        imagePanel.add(newM0Combo.getParent(), gbc);
        gbc.gridy++;
        
        b1Combo = guiBuilder.buildComboBox("B1 results:", titles, b1Guess);
        imagePanel.add(b1Combo.getParent(), gbc);
        gbc.gridy++;
        
        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(MipavUtil.buildTitledBorder("TRE 4D Parameters"));
        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        flipAngleField = guiBuilder.buildIntegerField("Flip angle:", 15);
        repTimeField = guiBuilder.buildDecimalField("Repetition Time:", 5.6);
        dceCenterField = guiBuilder.buildFileField("DCE Center Times:", " ", false, JFileChooser.FILES_ONLY);
        
        paramPanel.add(flipAngleField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(repTimeField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(dceCenterField.getParent(), gbc);
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
		
		dceHigh = ViewUserInterface.getReference().getRegisteredImageByName(dceCombo.getSelectedItem().toString());
        r1 = ViewUserInterface.getReference().getRegisteredImageByName(r1Combo.getSelectedItem().toString());
        newM0 = ViewUserInterface.getReference().getRegisteredImageByName(newM0Combo.getSelectedItem().toString());
        b1 = ViewUserInterface.getReference().getRegisteredImageByName(b1Combo.getSelectedItem().toString());
        
        try {
            fa = Integer.valueOf(flipAngleField.getText()).intValue();
            trTime = Double.valueOf(repTimeField.getText()).doubleValue();
        } catch(NumberFormatException nfe) {
            return false;
        }
        
        dceCenterFile = dceCenterField.getText();
        
        File f = new File(dceCenterFile);
        if(!f.exists() || !f.canWrite()) {
            return false;
        }
        
		return true;
	} //end setVariables()
}
