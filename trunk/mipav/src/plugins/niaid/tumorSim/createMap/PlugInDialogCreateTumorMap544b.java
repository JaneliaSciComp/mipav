package niaid.tumorSim.createMap;
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
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;

import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.text.DecimalFormat;
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
public class PlugInDialogCreateTumorMap544b extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */
    
    /** Type of tumor simulation */
    public enum TumorSimMode {
        //intensify,
        //deintensify,
        shrink,
        grow,
        none;
    }
    
    /** Noise profile mode */
    public enum NoiseMode {
        gaussian,
        rician;
    }
    
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;
    
    /** This is your algorithm */
    private PlugInAlgorithmCreateTumorMap544b tumorSimAlgo = null;

    private JTextField initRadiusText;

    private JComboBox growthShrinkCombo;

    private JTextField percentChangeText;

    private JPanel okCancelPanel;

    private JTextField xyDimText;

    private JTextField zDimText;

    private JTextField xyResText;

    private JTextField zResText;
    
    private JTextField intensity1Text;

    private int xyDim, zDim;

    private double xyRes, zRes;

    private double initRadius;

    private double tumorChange;

    private TumorSimMode simMode;

    private double intensity1;

    private JComboBox subSampleCombo;

    private int subsample;

    /** Units of image */
	private JComboBox unitsCombo;

    private boolean doCenter;

    private JCheckBox doCenterCheck;

    private JTextField intensity2Text;

    private Double intensity2;

    private JTextField noiseMaxText;

    private Double noiseMax;

    /** Whether partial-voluming minimization using perturbation should occur. */
    //private boolean doPerturbRadius = true; no longer necessary since PV of acquisition taken into account

    private JTextField normalTissueText;

    private double normalTissue;

    private JTextField stdDevNormalText;

    private JTextField stdDevIntensity1Text;

    private JTextField stdDevIntensity2Text;

    private JRadioButton ricianRadio;

    private JRadioButton gaussianRadio;

    private JTextField gaussianText;

    /** Standard deviations for normal tissue, tumor1, tumor2, and noise profile*/
    private double stdDevNormal, stdDevIntensity1, stdDevIntensity2, stdDevGaussian;

    private NoiseMode noise;

	private JRadioButton adcButton;

	private JRadioButton kTransButton;

	private JRadioButton rStarButton;

	/** Iteration number */
    private int iter;

    private JPanel mainPanel;

    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogCreateTumorMap544b() { }

    /**
     * Sets up variables but does not show dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogCreateTumorMap544b(boolean modal, boolean doOkCancel) {
        super(modal); 

        init(doOkCancel);
    }
    
    public PlugInDialogCreateTumorMap544b(PlugInDialogCreateTumorMap544b template, boolean modal, boolean doOkCancel) {
        this(modal, doOkCancel);
        System.out.println("Test2");
        template.setVisible(true);
        template.saveDefaults();
        template.setVisible(false);
        
        this.loadDefaults();
    }
    
    //  ~ Methods --------------------------------------------------------------------------------------------------------
    //
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
            super.dispose();
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
       if (algorithm instanceof PlugInAlgorithmCreateTumorMap544b) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());

            if (tumorSimAlgo.isCompleted()) {
                insertScriptLine();
            }

            /*if (tumorSimAlgo != null) {
                tumorSimAlgo.finalize();
                tumorSimAlgo = null;
            }

            dispose();*/
        }

    } // end algorithmPerformed()

    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        if(tumorSimAlgo != null) {
            tumorSimAlgo.finalize();
        }
        
        try {
            double noiseParam;
            switch(noise) {
            case gaussian:
                noiseParam = stdDevGaussian;
                break;
            default:
                noiseParam = noiseMax;
                break;
            }
            
            
            tumorSimAlgo = new PlugInAlgorithmCreateTumorMap544b(xyDim, zDim, xyRes, zRes, initRadius, tumorChange, simMode, 
                    intensity1, stdDevIntensity1, intensity2, stdDevIntensity2, subsample, doCenter, noise, noiseParam, normalTissue, stdDevNormal);

            tumorSimAlgo.setIter(iter);
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            tumorSimAlgo.addListener(this);
            createProgressBar("Creating images", " ...", tumorSimAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
         
                if (tumorSimAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                tumorSimAlgo.run();
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

    public void destroy() {
        System.out.println("Before: "+Runtime.getRuntime().freeMemory());
        
        if(tumorSimAlgo != null) {
            
            tumorSimAlgo = null;
            
            ViewUserInterface.getReference().closeAllImages();
            
            Runtime.getRuntime().gc();
            
            System.out.println("After: "+Runtime.getRuntime().freeMemory());
        }
    }
    
    /**
     * Used in turning your plugin into a script
     */
    protected void setGUIFromParams() {
    	//image = scriptParameters.retrieveInputImage();

    	//doGaussian = scriptParameters.getParams().getBoolean("do_gaussian");
    } //end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
    	//scriptParameters.storeInputImage(image);
   
        //scriptParameters.getParams().put(ParameterFactory.newParameter("do_gaussian", doGaussian));
    } //end storeParamsFromGUI()
   
    private void init(boolean doOKCancel) {
        setForeground(Color.black);
        setTitle("Create tumor maps 544b");
        try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}
        GuiBuilder gui = new GuiBuilder(this);
        
        mainPanel = buildMainPanel(doOKCancel, gui);

        JScrollPane scroll = new JScrollPane(mainPanel);
        
        getContentPane().add(scroll, BorderLayout.CENTER);

        pack();
        setVisible(true);
        setResizable(true);
        System.gc();
        
    } // end init()
    
    public void setRadiusField(double radius) {
        initRadiusText.setText(String.valueOf(radius));
    }
    
    public double getRadiusField() {
        return Double.valueOf(initRadiusText.getText()).doubleValue();
    }

    public PlugInAlgorithmCreateTumorMap544b getTumorSimAlgo() {
        return tumorSimAlgo;
    }
    
    public JPanel getMainPanel() {
        return mainPanel;
    }

    private JPanel buildMainPanel(boolean doOKCancel, GuiBuilder gui) {

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
        
        JPanel simTypePanel = new JPanel(new GridBagLayout());
        simTypePanel.setForeground(Color.black);
        simTypePanel.setBorder(MipavUtil.buildTitledBorder("Sample simulation params (based on image 1 tumor)"));
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        adcButton = gui.buildRadioButton("ADC", true);
        simTypePanel.add(adcButton.getParent(), gbc);
        
        gbc.gridx++;
        kTransButton = gui.buildRadioButton("Ktrans", false);
        simTypePanel.add(kTransButton.getParent(), gbc);
        
        gbc.gridx++;
        rStarButton = gui.buildRadioButton("R*", false);
        simTypePanel.add(rStarButton.getParent(), gbc);
        
        ButtonGroup simMode = new ButtonGroup();
        simMode.add(adcButton);
        simMode.add(kTransButton);
        simMode.add(rStarButton);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(simTypePanel, gbc);
        
        JPanel imageSizePanel = new JPanel(new GridBagLayout());
        imageSizePanel.setForeground(Color.black);
        imageSizePanel.setBorder(buildTitledBorder("Image size parameters"));
        
        xyDimText = gui.buildIntegerField("X/Y Dimension: ", 256);
        imageSizePanel.add(xyDimText.getParent(), gbc);
        
        gbc.gridx++;
        zDimText = gui.buildIntegerField("Z Dimension: ", 256);
        imageSizePanel.add(zDimText.getParent(), gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        xyResText = gui.buildDecimalField("X/Y Resolution: ", .117);
        imageSizePanel.add(xyResText.getParent(), gbc);
        
        gbc.gridx++;
        zResText = gui.buildDecimalField("Z Resolution: ", .117);
        imageSizePanel.add(zResText.getParent(), gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        Unit[] units = UnitType.getUnitsOfType(UnitType.LENGTH);
        int selected = 0;
        for(int i=0; i<units.length; i++) {
            if(units[i] == Unit.MILLIMETERS) {
                selected = i;
            }
        }
        unitsCombo = gui.buildComboBox("Units of image: ", UnitType.getUnitsOfType(UnitType.LENGTH), selected);
        imageSizePanel.add(unitsCombo.getParent(), gbc);
        
        gbc.gridy = 1;
        gbc.gridx = 0;
        mainPanel.add(imageSizePanel, gbc);
        
        JPanel tumorSimPanel = new JPanel(new GridBagLayout());
        tumorSimPanel.setForeground(Color.black);
        tumorSimPanel.setBorder(buildTitledBorder("Tumor simulation parameters"));
        
        doCenterCheck = gui.buildCheckBox("Enclose entire tumor within field of view", true);
        tumorSimPanel.add(doCenterCheck.getParent(), gbc);
        
        gbc.gridy++;
        normalTissueText = gui.buildDecimalField("Normal tissue intensity: ", 70);
        tumorSimPanel.add(normalTissueText.getParent(), gbc);
        
        gbc.gridx++;
        stdDevNormalText = gui.buildDecimalField("Std dev: ", (int)(70*.1));
        tumorSimPanel.add(stdDevNormalText.getParent(), gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        intensity1Text = gui.buildDecimalField("Image 1 tumor intensity: ", 109);
        tumorSimPanel.add(intensity1Text.getParent(), gbc);
        
        gbc.gridx++;
        stdDevIntensity1Text = gui.buildDecimalField("Std dev: ", (int)(109*.1));
        tumorSimPanel.add(stdDevIntensity1Text.getParent(), gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        intensity2Text = gui.buildDecimalField("Image 2 tumor intensity: ", 201);
        tumorSimPanel.add(intensity2Text.getParent(), gbc);
        
        gbc.gridx++;
        stdDevIntensity2Text = gui.buildDecimalField("Std dev: ", (int)(201*.1));
        tumorSimPanel.add(stdDevIntensity2Text.getParent(), gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        JPanel panel = new JPanel();
        FlowLayout flow = new FlowLayout(FlowLayout.LEFT);
        panel.setLayout(flow);
        
        final JLabel measureLabel = new JLabel("Radius of initial tumor (in "+Unit.MILLIMETERS.getAbbrev()+"): ");
        panel.add(measureLabel, flow);
        
        initRadiusText = gui.buildDecimalField("", 2.808);
        panel.add(initRadiusText, flow);
        tumorSimPanel.add(panel, gbc);
        
        unitsCombo.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent e) {
                measureLabel.setText("Radius of initial tumor (in "+((Unit)unitsCombo.getSelectedItem()).getAbbrev()+"): ");
            }
        });
        
        gbc.gridy++;
        subSampleCombo = gui.buildComboBox("Subsampling amount: ", new Integer[] {8, 4, 2, 0}, 0);
        tumorSimPanel.add(subSampleCombo.getParent(), gbc);
        
        gbc.gridy++;
        growthShrinkCombo = gui.buildComboBox("Simulate tumor: ", TumorSimMode.values());
        tumorSimPanel.add(growthShrinkCombo.getParent(), gbc);
        
        gbc.gridy++;      
        percentChangeText = gui.buildDecimalField("Percentage change: ", .33);
        tumorSimPanel.add(percentChangeText.getParent(), gbc);
              
        gbc.gridy = 2;
        mainPanel.add(tumorSimPanel, gbc);
       
        JPanel noisePanel = new JPanel(new GridBagLayout());
        noisePanel.setForeground(Color.black);
        noisePanel.setBorder(MipavUtil.buildTitledBorder("Noise profile"));
        
        ButtonGroup noise = new ButtonGroup();
        
        gbc.gridy = 0;
        gbc.gridx = 0;
        ricianRadio = gui.buildRadioButton("Rician", true);
        noise.add(ricianRadio);
        noisePanel.add(ricianRadio.getParent(), gbc);
       
        gbc.gridx++;
        gaussianRadio = gui.buildRadioButton("Gaussian", false);
        noise.add(gaussianRadio);
        noisePanel.add(gaussianRadio.getParent(), gbc);
        
        ricianRadio.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                noiseMaxText.getParent().setVisible(ricianRadio.isSelected());
                gaussianText.getParent().setVisible(gaussianRadio.isSelected());
            }
        });
        
        gaussianRadio.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                noiseMaxText.getParent().setVisible(ricianRadio.isSelected());
                gaussianText.getParent().setVisible(gaussianRadio.isSelected());
            }
        });
        
        gbc.gridy++;
        gbc.gridx = 0;
        noiseMaxText = gui.buildDecimalField("Maximum Rician noise value: ", 1);
        noisePanel.add(noiseMaxText.getParent(), gbc);
        
        gbc.gridy++;
        gaussianText = gui.buildDecimalField("Standard deviation", 1);
        noisePanel.add(gaussianText.getParent(), gbc);
        gaussianText.getParent().setVisible(false);
        
        gbc.gridy = 3;
        gbc.gridx = 0;
        mainPanel.add(noisePanel, gbc);
        
        ActionListener listen = new TumorSimListener();
        
        adcButton.addActionListener(listen);
        kTransButton.addActionListener(listen);
        rStarButton.addActionListener(listen);
        
        if(doOKCancel) {
            gbc.gridy++;
            okCancelPanel = gui.buildOKCancelPanel();
            mainPanel.add(okCancelPanel, gbc);
        }
        
        return mainPanel;
    }
    
    private class TumorSimListener implements ActionListener {

        public void actionPerformed(ActionEvent e) {
	        System.out.println("Simulating "+e.getSource().toString());
        }
    }

    /**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
	private boolean setVariables() {
		
	    try {
    	    xyDim = Integer.valueOf(xyDimText.getText());
    	    zDim = Integer.valueOf(zDimText.getText());
    	    
    	    xyRes = Double.valueOf(xyResText.getText());
    	    zRes = Double.valueOf(zResText.getText());
    	    
    	    initRadius = Double.valueOf(initRadiusText.getText());
    	    
    	    subsample = Integer.valueOf(subSampleCombo.getSelectedItem().toString());
    	    
    	    tumorChange = Double.valueOf(percentChangeText.getText());
    	    
    	    intensity1 = Double.valueOf(intensity1Text.getText());
    	    
    	    stdDevIntensity1 = Double.valueOf(stdDevIntensity1Text.getText());
    	    
    	    intensity2 = Double.valueOf(intensity2Text.getText());
    	    
    	    stdDevIntensity2 = Double.valueOf(stdDevIntensity2Text.getText());
    	    
    	    noiseMax = Double.valueOf(noiseMaxText.getText());
    	    
    	    stdDevGaussian = Double.valueOf(gaussianText.getText());
    	    
    	    normalTissue = Double.valueOf(normalTissueText.getText());
    	    
    	    stdDevNormal = Double.valueOf(stdDevNormalText.getText());
    	    
    	    noise = NoiseMode.rician; //default
    	    if(gaussianRadio.isSelected()) {
    	        noise = NoiseMode.gaussian;
    	    }
    	    
    	    Preferences.data("====Create tumor map algorithm information====\n");
            Preferences.data("Dimensions:\tXY: "+xyDim+"\tZ: "+zDim+"\n");
            Preferences.data("Resolution:\tXY: "+xyRes+"\nZ: "+zRes+"\n");
            Preferences.data("Initial Radius: "+initRadius+"\n");
            Preferences.data("Subsample: "+subsample+"\n");
            Preferences.data("Percent change: "+tumorChange+"\n");
            Preferences.data("Intensity1: "+intensity1+"\tIntensity2: "+intensity2+"\n");
            Preferences.data("Noise maximum: "+noiseMax+"\n");
            Preferences.data("Noise maximum: "+normalTissue+"\n");
            Preferences.data("====End create tumor map algorithm information====\n");
	    } catch(NumberFormatException nfe) {
	        MipavUtil.displayError("Input error, enter numerical values only.");
	        return false;
	    }
	    
	    doCenter = doCenterCheck.isSelected();

	    simMode = (TumorSimMode)growthShrinkCombo.getSelectedItem();
	    
		return true;
	} //end setVariables()

    private double perturbRadius(double initRadius, double xyRes, double zRes) {
        double minDiff = Double.MAX_VALUE, percentMin = 0.0;
        
        for(double j=-.05; j<.05; j+=.0005) {
            double newRadius = initRadius*(1+j);
            double xyDiff = getDiff(newRadius, xyRes);
            double zDiff = getDiff(newRadius, zRes);           
            double newDiff =  xyDiff+zDiff;
            if(newDiff < minDiff) {
                minDiff = newDiff;
                percentMin = j;
            }
        }
        
        DecimalFormat decForm = new DecimalFormat("0.#######");
        double newRadius = Double.valueOf(decForm.format(((1+percentMin)*initRadius))).doubleValue();
        
        if(newRadius != initRadius) {
            int result = JOptionPane.showConfirmDialog(parentFrame, "Using initial radius of "+newRadius+" instead of "+initRadius+" reduces rounding errors of radius conversion to pixels.  Use new radius of "+newRadius+"?\n"+
                                                        "\tFor example, in x/y dimensions radius will use "+decForm.format(newRadius/xyRes)+" pixels instead of "+decForm.format(initRadius/xyRes)+" pixels,\n"+
                                                        "\tin z dimension radius will use "+decForm.format(newRadius/zRes)+" pixels instead of "+decForm.format(initRadius/zRes)+" pixels.", "Do radius change",  
                                                        JOptionPane.YES_NO_OPTION, JOptionPane.NO_OPTION);
            if(result == JOptionPane.YES_OPTION) {
                return newRadius; 
            }
        }
        
        return initRadius;
    }

    private double getDiff(double newRadius, double res) {
        double diff = (newRadius/res) % 1;
        if(diff > .5) {
            diff = Math.abs(1-diff);
        }
        return diff;
    }

    public void setIter(int i) {
        this.iter = i;
    }
}
