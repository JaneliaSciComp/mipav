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
public class PlugInDialogCreateTumorMap541a extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */
    
    /** Type of tumor simulation */
    public enum TumorSimMode {
        intensify,
        deintensify,
        shrink,
        grow,
        none;
    }
    
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;
    
    /** This is your algorithm */
    private PlugInAlgorithmCreateTumorMap541a tumorSimAlgo = null;

    private JTextField initRadiusText;

    private JComboBox growthShrinkCombo;

    private JTextField percentChangeText;

    private JPanel okCancelPanel;

    private JTextField xyDimText;

    private JTextField zDimText;

    private JTextField xyResText;

    private JTextField zResText;
    
    private JTextField intensityText;

    private int xyDim, zDim;

    private double xyRes, zRes;

    private double initRadius;

    private double tumorChange;

    private TumorSimMode simMode;

    private double intensity;

    private JComboBox subSampleCombo;

    private int subsample;

    /** Units of image */
	private JComboBox unitsCombo;

    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogCreateTumorMap541a() { }

    /**
     * Sets up variables but does not show dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogCreateTumorMap541a(boolean modal) {
        super(modal); 
        
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
       if (algorithm instanceof PlugInAlgorithmCreateTumorMap541a) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            
            if ((tumorSimAlgo.isCompleted() == true)) {
                tumorSimAlgo.getImage1a().getParentFrame().setVisible(true);
                tumorSimAlgo.getImage2a().getParentFrame().setVisible(true);
                
            } 

            if (tumorSimAlgo.isCompleted()) {
                insertScriptLine();
            }

            if (tumorSimAlgo != null) {
                tumorSimAlgo.finalize();
                tumorSimAlgo = null;
            }

            dispose();
        }

    } // end algorithmPerformed()

    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            
            tumorSimAlgo = new PlugInAlgorithmCreateTumorMap541a(xyDim, zDim, xyRes, zRes, initRadius, tumorChange, simMode, intensity, subsample);

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
   
    private void init() {
        setForeground(Color.black);
        setTitle("Create tumor maps 541a");
        try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}
        
        GuiBuilder gui = new GuiBuilder(this);

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
        
        gbc.gridy = 0;
        gbc.gridx = 0;
        mainPanel.add(imageSizePanel, gbc);
        
        JPanel tumorSimPanel = new JPanel(new GridBagLayout());
        tumorSimPanel.setForeground(Color.black);
        tumorSimPanel.setBorder(buildTitledBorder("Tumor simulation parameters"));
        
        intensityText = gui.buildDecimalField("Intensity value: ", 1300);
        tumorSimPanel.add(intensityText.getParent(), gbc);
        
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
        
        gbc.gridy = 1;
        mainPanel.add(tumorSimPanel, gbc);
        
        gbc.gridy++;
        okCancelPanel = gui.buildOKCancelPanel();
        mainPanel.add(okCancelPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

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
		
	    try {
    	    xyDim = Integer.valueOf(xyDimText.getText());
    	    zDim = Integer.valueOf(zDimText.getText());
    	    
    	    xyRes = Double.valueOf(xyResText.getText());
    	    zRes = Double.valueOf(zResText.getText());
    	    
    	    initRadius = Double.valueOf(initRadiusText.getText());
    	    
    	    subsample = Integer.valueOf(subSampleCombo.getSelectedItem().toString());
    	    
    	    tumorChange = Double.valueOf(percentChangeText.getText());
    	    
    	    intensity = Double.valueOf(intensityText.getText());
	    } catch(NumberFormatException nfe) {
	        MipavUtil.displayError("Input error, enter numerical values only.");
	        return false;
	    }

	    simMode = (TumorSimMode)growthShrinkCombo.getSelectedItem();
	    
	    initRadius = perturbRadius(initRadius, xyRes, zRes);
	    
		return true;
	} //end setVariables()

    private double perturbRadius(double initRadius, double xyRes, double zRes) {
        double minDiff = Double.MAX_VALUE, percentMin = 0.0;
        
        for(double j=-.1; j<.1; j+=.001) {
            double newRadius = initRadius*(1+j);
            double xyDiff = Math.abs(1-((newRadius/xyRes) % 1));
            double zDiff = Math.abs(1-((newRadius/zRes) % 1));
            double newDiff =  xyDiff+zDiff;
            if(newDiff < minDiff) {
                minDiff = newDiff;
                percentMin = j;
            }
            if(j == 0.0) {
                System.out.println("NewDiff: "+newDiff);
            } else {
                System.out.println(j+" "+newRadius+" "+newDiff+" "+minDiff);
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
}
