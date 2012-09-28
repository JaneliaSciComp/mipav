package niaid.tumorSim.simParams;
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
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.FileNotFoundException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Random;

import javax.swing.*;

import niaid.tumorSim.createMap.PlugInDialogCreateTumorMap544a;
import niaid.tumorSim.postTreatment.PlugInDialogGeneratePostTreatment544a;

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
public class PlugInDialogSimulateParams544a extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    //private PlugInDialogGeneratePostTreatment543a generatePostTreatmentDialog;

    private JTextField tumorSizeDevText;

    private JTextField iterNumText;

    private JPanel okCancelPanel;

    private double tumorSizeDev;

    private int iterNum;

    private PlugInDialogCreateTumorMap544a createTumorDialogTemplate;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogSimulateParams544a() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogSimulateParams544a(boolean modal) {
        super(modal); 
        
        createTumorDialogTemplate = new PlugInDialogCreateTumorMap544a(false);
        createTumorDialogTemplate.setVisible(false);
        //generatePostTreatmentDialog = new PlugInDialogGeneratePostTreatment543a();
        
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
        dispose();

    } // end algorithmPerformed()

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
        setTitle("Run simulation 544a");
        try {
            setIconImage(MipavUtil.getIconImage("divinci.gif"));
        } catch (FileNotFoundException e) {
            Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
        }
        
        GuiBuilder gui = new GuiBuilder(this);

        JPanel mainPanel = new JPanel();
        mainPanel.setForeground(Color.black);
        mainPanel.setLayout(new GridBagLayout());
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel createTumorPanel = createTumorDialogTemplate.getMainPanel();
        mainPanel.add(createTumorPanel, gbc);
        
        gbc.gridx++;
        tumorSizeDevText = gui.buildDecimalField("Tumor radius standard deviation: ", 1);
        mainPanel.add(tumorSizeDevText.getParent(), gbc);
        
//        gbc.gridx = 0;
//        gbc.gridy++;
//        JPanel generatePostPanel = generatePostTreatmentDialog.buildMainPanel(false, gui);
//        mainPanel.add(generatePostPanel, gbc);
        
        gbc.gridx++;
        iterNumText = gui.buildIntegerField("Number of iterations: ", 10);              
        mainPanel.add(iterNumText.getParent(), gbc);
        
        gbc.gridx = 0;
        gbc.gridwidth = 2;
        gbc.gridy++;
        okCancelPanel = gui.buildOKCancelPanel();
        mainPanel.add(okCancelPanel, gbc);

        JScrollPane scroll = new JScrollPane(mainPanel);
        
        getContentPane().add(scroll, BorderLayout.CENTER);

        pack();
        setVisible(true);
        setResizable(true);
        System.gc();
        
    } // end init()

    /**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
    private boolean setVariables() {
        try {
            tumorSizeDev = Double.valueOf(tumorSizeDevText.getText());
            iterNum = Integer.valueOf(iterNumText.getText());
            
        } catch(NumberFormatException nfe) {
            MipavUtil.displayError("Input error, enter numerical values only.");
            return false;
        }
        
        return true;
    } //end setVariables()

    protected void callAlgorithm() {
        Thread t = new Thread(new RunTumorMap());
        t.start();
        
        this.dispose();
    }
    
    private class RunTumorMap implements Runnable {

        @Override
        public void run() {
            performAlgComp();
        }
        
        private void performAlgComp() {
            Random r = new Random();

            for(int i=0; i<iterNum; i++) {
                Preferences.data("************Begin Iteration "+(i+1)+"*****************\n");
                
                PlugInDialogCreateTumorMap544a createTumorDialog = new PlugInDialogCreateTumorMap544a(createTumorDialogTemplate, false);
                
                double radius = 0;
                while(radius <= 0) {
                    radius = createTumorDialog.getRadiusField() + r.nextGaussian()*tumorSizeDev;
                }
                
                Preferences.data("Unique simulation fields:\nRadius: "+radius+"\n");
                
                createTumorDialog.setRadiusField(radius);
                createTumorDialog.setIter((i+1));
                createTumorDialog.setSeparateThread(false);
                createTumorDialog.actionPerformed(new ActionEvent(this, 0, "OK"));
                createTumorDialog.destroy();
                createTumorDialog.dispose();
                
                ViewUserInterface ui = ViewUserInterface.getReference();
                
                tryToCleanUp(ui);
                
//                generatePostTreatmentDialog.setSeparateThread(false);
//                generatePostTreatmentDialog.setImage1ComboItem(createTumorDialog.getTumorSimAlgo().getImage1a().getImageName());
//                generatePostTreatmentDialog.setImage2ComboItem(createTumorDialog.getTumorSimAlgo().getImage2a().getImageName());
//                generatePostTreatmentDialog.setImage1TumorComboItem(createTumorDialog.getTumorSimAlgo().getImage1aTumor().getImageName());
//                generatePostTreatmentDialog.setImage2TumorComboItem(createTumorDialog.getTumorSimAlgo().getImage2aTumor().getImageName());
//                generatePostTreatmentDialog.actionPerformed(new ActionEvent(this, 0, "OK"));
                
                Preferences.data("************End Iteration "+(i+1)+"*****************\n");
            }
        }

        private void tryToCleanUp(ViewUserInterface ui) {
            System.out.println("memory before: "+Runtime.getRuntime().freeMemory());
            
            ui.getImageFrameVector().removeAllElements();
            Runtime.getRuntime().gc();
            
            System.out.println("memory after: "+Runtime.getRuntime().freeMemory());
            System.out.println("Done.");
            
        }
        
    }

    
}
