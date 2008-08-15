package gov.nih.mipav.view.components;


import gov.nih.mipav.view.*;

import java.awt.event.*;

import javax.swing.*;


/**
 * A panel containing fields which allow the user to input WaterShed parameters.
 *
 * @author  Ruida Cheng
 */
public class JPanelWaterShedITK extends JPanel implements FocusListener, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID = 4032897701053956108L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private JTextField textConductance;
    private JTextField textIterations;
    private JTextField textThreshold;
    private JTextField textLevel;
    private JTextField textTimeStep;
    
    private float conductance;
    private int iterations;
    private float threshold;
    private float level;
    private float timeStep;
 
    private JCheckBox principleCheckBox;
    private boolean principleComponents = false;

    /** Parameter panel contains the WaterShed parameters. */
    private JPanel parametersPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct the waterShed parameters panel.
     */
    public JPanelWaterShedITK() {
        initGUI();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

 
    /**
     * Do nothing.
     *
     * @param  event  focus gained event
     */
    public void focusGained(FocusEvent event) { }

    /**
     * When the user clicks the mouse out of a text field, resets the neccessary variables.
     *
     * @param  event  focus lost event
     */
    public void focusLost(FocusEvent event) {
     }

    /**
     * Returns the parameters to use in the algorithm.
     *
     * @return  array that contains the 5 parameters.
     */
    public float[] getParameters() {

    	return new float[] {
    			 Float.valueOf(textConductance.getText()).floatValue(),
    		     Float.valueOf(textIterations.getText()).floatValue(),
    		     Float.valueOf(textThreshold.getText()).floatValue(),
    		     Float.valueOf(textLevel.getText()).floatValue(),
    		     Float.valueOf(textTimeStep.getText()).floatValue()		
    	};        
    }

 
    /**
     * Changes resolution normalization label based on whether or not the resolution normalization check box is checked.
     *
     * @param  event  checkbox item change event
     */
    public void itemStateChanged(ItemEvent event) {

    }

    
    /**
     * Changes the conductance value.
     *
     * @param  conductance 
     */
    public void setConductance(float conductance) {
    	textConductance.setText("" + conductance);
    }
    
    /**
     * Changes the iterations value.
     *
     * @param  iterations
     */
    public void setIterations(float iterations) {
    	textIterations.setText("" + iterations);
    }

    
    /**
     * Changes the Theshold value.
     *
     * @param  Theshold
     */
    public void setThreshold(float theshold) {
    	textThreshold.setText("" + theshold);
    }
    
    /**
     * Changes the level value.
     *
     * @param  Level
     */
    public void setLevel(float level) {
    	textLevel.setText("" + level);
    }
    
    /**
     * Changes the timeStep value.
     *
     * @param  Level
     */
    public void setTimeStep(float timeStep) {
    	textTimeStep.setText("" + timeStep);
    }
    
    
    /**
     * Tests the parameter values against default values.
     *
     * @return  true if all the sigmas are in a good range, false otherwise
     */
    public boolean testParametersValues() {

        if (!MipavUtil.testParameter(textTimeStep.getText(), 0.0, 0.24)) {
        	textTimeStep.requestFocus();
        	textTimeStep.selectAll();

            return false;
        }

        return true;
    }



    /**
     * Initialize the panel's GUI.
     */
    private void initGUI() {
    	PanelManager scalePanelManager = new PanelManager(this);
        setBorder(WidgetFactory.buildTitledBorder("WaterShed Parameters"));
        
        textConductance = WidgetFactory.buildTextField("2.0");
        textConductance.setColumns(5);
        textIterations = WidgetFactory.buildTextField("10");
        textIterations.setColumns(5);
        textThreshold = WidgetFactory.buildTextField("0.001");
        textThreshold.setColumns(5);
        textLevel = WidgetFactory.buildTextField("0.15");
        textLevel.setColumns(5);
        textTimeStep = WidgetFactory.buildTextField("0.0625");
        textTimeStep.setColumns(5);
        
        
        scalePanelManager.add(WidgetFactory.buildLabel(" Conductance "));
        scalePanelManager.add(textConductance);
        scalePanelManager.addOnNextLine(WidgetFactory.buildLabel(" Iterations "));
        scalePanelManager.add(textIterations);
        scalePanelManager.addOnNextLine(WidgetFactory.buildLabel(" Threshold "));
        scalePanelManager.add(textThreshold);
        scalePanelManager.addOnNextLine(WidgetFactory.buildLabel(" Level "));
        scalePanelManager.add(textLevel);
        scalePanelManager.addOnNextLine(WidgetFactory.buildLabel(" TimeStep "));
        scalePanelManager.add(textTimeStep);
        
        /*
        principleCheckBox = new JCheckBox("Principal components");
        principleCheckBox.setFont(serif12);
        parametersPanel.add(principleCheckBox);
        principleCheckBox.setSelected(true);
        */
    }

}
