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
public class PlugInDialogGeneratePostTreatment542a extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** This is your algorithm */
    private PlugInAlgorithmGeneratePostTreatment542a generatePostAlgo = null;

    /** The check box for whether a blur should be performed. */
	private JCheckBox check;

	/** The variable representing whether the blur should be performed. */
	private boolean doGaussian;

    private JComboBox image1Combo;

    private JTextField image1IntensityText, image2IntensityText;

    private JComboBox image2Combo;

    private JTextField image1ScaleText, image2ScaleText;
    
    private JTextField image1NoiseText, image2NoiseText;

    private JPanel okCancelPanel;

    private double image1Intensity, image2Intensity;

    private ModelImage image1, image2;

    private double image1Scale, image2Scale;

    private double image1Noise, image2Noise;

    private JCheckBox doInterImagesCheckBox;

    private boolean doInterImages;

    private JTextField stdDevNumText;

    private double stdDevNum;

    private GuiBuilder gui = new GuiBuilder(this);

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogGeneratePostTreatment542a() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogGeneratePostTreatment542a(Frame theParentFrame, ModelImage im) {
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
        if (algorithm instanceof PlugInAlgorithmGeneratePostTreatment542a) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            
            if ((generatePostAlgo.isCompleted() == true)) {
                if(doInterImages) {
                    new ViewJFrameImage(generatePostAlgo.getImage1b());
                    new ViewJFrameImage(generatePostAlgo.getImage2b());
                    new ViewJFrameImage(generatePostAlgo.getImage1c());
                    new ViewJFrameImage(generatePostAlgo.getImage2c());
                }
                new ViewJFrameImage(generatePostAlgo.getPostTreatment());
            } 

            if (generatePostAlgo.isCompleted()) {
                
                insertScriptLine();
            }

            if (generatePostAlgo != null) {
                generatePostAlgo.finalize();
                generatePostAlgo = null;
            }

            dispose();
        }

    } // end algorithmPerformed()

    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            
            generatePostAlgo = new PlugInAlgorithmGeneratePostTreatment542a(image1, image1Intensity, image1Scale, image1Noise,
                                                                            image2, image2Intensity, image2Scale, image2Noise, stdDevNum);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            generatePostAlgo.addListener(this);
            createProgressBar(image1.getImageName(), " ...", generatePostAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (generatePostAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                generatePostAlgo.run();
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
        setTitle("Generate post treatment map 542a");
        try {
            setIconImage(MipavUtil.getIconImage("divinci.gif"));
        } catch (FileNotFoundException e) {
            Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
        }

        JPanel mainPanel = buildMainPanel(true, gui);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
        
    } // end init()

    public JPanel buildMainPanel(boolean doOKCancel, GuiBuilder gui) {
        JPanel mainPanel = new JPanel(new GridBagLayout());
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        mainPanel.setForeground(Color.black);

        JPanel image1Panel = new JPanel(new GridBagLayout());
        image1Panel.setForeground(Color.black);
        image1Panel.setBorder(buildTitledBorder("Image 1 parameters"));
        
        int numDefault1=0, numDefault2=0, i=0;
        Enumeration<String> imageList = ViewUserInterface.getReference().getRegisteredImageNames();
        while(imageList.hasMoreElements()) {
            String name = imageList.nextElement();
            if(name.contains("1a")) {
                numDefault1 = i;
            } else if(name.contains("2a")) {
                numDefault2 = i;
            }
            i++;
        }
        Object[] imageAr = Collections.list(ViewUserInterface.getReference().getRegisteredImageNames()).toArray();
        
        image1Combo = gui.buildComboBox("Image 1: ", imageAr, numDefault1);
        image1Panel.add(image1Combo.getParent(), gbc);
        
        double intensity1 = 109, intensity2 = 201;
        String intenSearch = Preferences.getData();
        try {
            int loc = intenSearch.lastIndexOf(PlugInAlgorithmCreateTumorMap542a.INTENSITY1);
            intensity1 = Double.valueOf(intenSearch.substring(loc+PlugInAlgorithmCreateTumorMap542a.INTENSITY1.length(), intenSearch.indexOf(';', loc)).trim());
            loc = intenSearch.lastIndexOf(PlugInAlgorithmCreateTumorMap542a.INTENSITY2);
            intensity2 = Double.valueOf(intenSearch.substring(loc+PlugInAlgorithmCreateTumorMap542a.INTENSITY2.length(), intenSearch.indexOf(';', loc)).trim());
        } catch(Exception e) {
            intensity1 = 109;
            intensity2 = 201;
        }
        
        gbc.gridx++;
        image1IntensityText = gui.buildDecimalField("Tumor intensity value: ", intensity1);            
        image1Panel.add(image1IntensityText.getParent(), gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        image1ScaleText = gui.buildDecimalField("Partial volume scaling: ", .67);
        image1Panel.add(image1ScaleText.getParent(), gbc);
        
        double noise1 = .05, noise2 = .05;
        String noiseSearch = Preferences.getData();
        try {
            int loc = noiseSearch.lastIndexOf(PlugInAlgorithmCreateTumorMap542a.NOISE_LEVEL);
            double noise = Double.valueOf(noiseSearch.substring(loc+PlugInAlgorithmCreateTumorMap542a.NOISE_LEVEL.length(), noiseSearch.indexOf(';', loc)).trim());
            noise1 = noise2 = noise;
        } catch(Exception e) {
            noise1 = noise2 = .05;
        }
        
        gbc.gridx++;
        image1NoiseText = gui.buildDecimalField("Image 1 noise std dev percentage: ", noise1);
        image1Panel.add(image1NoiseText.getParent(), gbc);
         
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(image1Panel, gbc);
        
        JPanel image2Panel = new JPanel(new GridBagLayout());
        image2Panel.setForeground(Color.black);
        image2Panel.setBorder(buildTitledBorder("Image 2 parameters"));
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        image2Combo = gui.buildComboBox("Image 2: ", imageAr, numDefault2);
        image2Panel.add(image2Combo.getParent(), gbc);
        
        gbc.gridx++;
        image2IntensityText = gui.buildDecimalField("Tumor intensity value: ", intensity2);
        image2Panel.add(image2IntensityText.getParent(), gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        image2ScaleText = gui.buildDecimalField("Partial volume scaling: ", .67);
        image2Panel.add(image2ScaleText.getParent(), gbc);
        
        gbc.gridx++;
        image2NoiseText = gui.buildDecimalField("Image 2 noise std dev percentage: ", noise2);
        image2Panel.add(image2NoiseText.getParent(), gbc);  
        
        gbc.gridy = 1;
        gbc.gridx = 0;
        mainPanel.add(image2Panel, gbc);
        
        gbc.gridy++;
        stdDevNumText = gui.buildDecimalField("True intensity values are within this number of standard deviations: ", 2.0);
        mainPanel.add(stdDevNumText.getParent(), gbc);
        
        gbc.gridy++;
        doInterImagesCheckBox = gui.buildCheckBox("Output intermediate images", true);
        mainPanel.add(doInterImagesCheckBox.getParent(), gbc);
        
        if(doOKCancel) {
            gbc.gridy++;
            okCancelPanel = gui.buildOKCancelPanel();
            mainPanel.add(okCancelPanel, gbc);
        }
        
        return mainPanel;
    }

    public void setImage1ComboItem(String imageName) {
        Object[] imageAr = Collections.list(ViewUserInterface.getReference().getRegisteredImageNames()).toArray();
        
        image1Combo = gui.buildComboBox("Image 1: ", imageAr, 0);
        
        this.image1Combo.setSelectedItem(imageName);
    }

    public void setImage2ComboItem(String imageName) { 
        Object[] imageAr = Collections.list(ViewUserInterface.getReference().getRegisteredImageNames()).toArray();

        image2Combo = gui.buildComboBox("Image 2: ", imageAr, 0);
        
        this.image2Combo.setSelectedItem(imageName);
    }

    /**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
	private boolean setVariables() {
		try {
		    image1Intensity = Double.valueOf(image1IntensityText.getText());
		    image1Scale = Double.valueOf(image1ScaleText.getText());
		    image1Noise = Double.valueOf(image1NoiseText.getText());
		    
		    image2Intensity = Double.valueOf(image2IntensityText.getText());
		    image2Scale = Double.valueOf(image2ScaleText.getText());
            image2Noise = Double.valueOf(image2NoiseText.getText());
            
            stdDevNum = Double.valueOf(stdDevNumText.getText());
            
            Preferences.data("====Generate Post-treatment algorithm information====\n");
            Preferences.data("Image 1 information:\n\tIntensity: "+image1Intensity+"\n\tScale: "+image1Scale+"\n\tNoise: "+image1Noise+"\n");
            Preferences.data("Image 2 information:\n\tIntensity: "+image2Intensity+"\n\tScale: "+image2Scale+"\n\tNoise: "+image2Noise+"\n");
            Preferences.data("Standard deviation number: "+stdDevNum+"\n");
            Preferences.data("====End generate Post-treatment algorithm information====\n");
		    
		} catch(NumberFormatException nfe) {
            MipavUtil.displayError("Input error, enter numerical values only.");
            return false;
        }
		System.out.println(image1Combo.getSelectedItem().toString());
		image1 = ViewUserInterface.getReference().getRegisteredImageByName(image1Combo.getSelectedItem().toString());
		if(image1 == null) {
		    MipavUtil.displayError(image1Combo.getSelectedItem().toString()+" is not a valid image name.");
		    return false;
		}
		System.out.println(image2Combo.getSelectedItem().toString());
		image2 = ViewUserInterface.getReference().getRegisteredImageByName(image2Combo.getSelectedItem().toString());
		if(image2 == null) {
            MipavUtil.displayError(image2Combo.getSelectedItem().toString()+" is not a valid image name.");
            return false;
        }
		
		doInterImages = doInterImagesCheckBox.isSelected();
		
		return true;
	} //end setVariables()
}
