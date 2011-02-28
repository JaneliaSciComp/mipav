import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;


import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.JDialogBase;


/**
 * @author pandyan
 * 
 * This class is the registration options dialog
 * that is used in the Image Average Registration plugin
 *
 */
public class PlugInImageAverageRegistration_RegOptionsDialog extends JDialogBase {



    /** Panels for x,y.,x*/
    private JPanel coarsePanelX, coarsePanelY, coarsePanelZ;

    /** Textfields for x,y,z */
    private JTextField coarseRateTextX, coarseRateTextY, coarseRateTextZ;

    /** combo box for cost function */
    private JComboBox comboBoxCostFunct;

    /** combo box for DOF */
    private JComboBox comboBoxDOF;

    /** combo box for Interp */
    private JComboBox comboBoxInterp;

    /** combo box for Interp2 */
    private JComboBox comboBoxInterp2;

    /** ints for cost,interp,interp2,DOF */
    private int cost, interp, interp2, DOF;

    /** cost function name */
    //private String costName = null;

    /** boolean if target image is color or not */
    private boolean doColor;

    /** Panels needed */
    private JPanel finePanelX, finePanelY, finePanelZ;

    /** Textfields needed */
    private JTextField fineRateTextX, fineRateTextY, fineRateTextZ;

    /** GridBag Constraints */
    private GridBagConstraints gbc;

    /** Label for Interp2*/
    private JLabel labelInterp2;

    /** boolean for maxOfMin resol */
    private boolean maxOfMinResol;

    /** checkbox for minMax */
    private JCheckBox minMaxCheckbox;

    /** textfields needed */
    private JTextField rotateBeginTextX, rotateBeginTextY, rotateBeginTextZ;

    /** floats for x*/
    private float rotateBeginX, rotateEndX, coarseRateX, fineRateX;

    /** floats for y */
    private float rotateBeginY, rotateEndY, coarseRateY, fineRateY;

    /** floats for z*/
    private float rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;

    /** textfields needed */
    private JTextField rotateEndTextX, rotateEndTextY, rotateEndTextZ;

    /** panel for rotate panel */
    private JPanel rotatePanel;

    /** panels for rotate panel */
    private JPanel rotateRangePanelX, rotateRangePanelY, rotateRangePanelZ;

    /** universal checkbox */
    private JCheckBox universalCheckbox;

    /** radio button for x*/
    private JRadioButton xRadio;

    /** boolean for xSelected*/
    private boolean xSelected = true;

    /** radio button for y */
    private JRadioButton yRadio;

    /** boolean for ySelected  */
    private boolean ySelected = false;

    /** radio button for z */
    private JRadioButton zRadio;

	
	
	/**
	 * constructor
	 * @param doColor
	 */
	public PlugInImageAverageRegistration_RegOptionsDialog (boolean doColor) {
		this.doColor = doColor;
		init();
	}
	
	
	/**
	 * init
	 *
	 */
	private void init() {
		
		setForeground(Color.black);
        setTitle("Optimized Automatic Image Registration 3D");

        JPanel optPanel = new JPanel();
        optPanel.setLayout(new GridBagLayout());
        optPanel.setBorder(buildTitledBorder("Input Options"));
        
        JLabel labelDOF = new JLabel("Degrees of freedom:");
        labelDOF.setForeground(Color.black);
        labelDOF.setFont(serif12);
        labelDOF.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxDOF = new JComboBox();
        comboBoxDOF.setFont(MipavUtil.font12);
        comboBoxDOF.setBackground(Color.white);
        comboBoxDOF.setToolTipText("Degrees of freedom");
        comboBoxDOF.addItem("Rigid - 6");
        comboBoxDOF.addItem("Global rescale - 7");
        comboBoxDOF.addItem("Specific rescale - 9");
        comboBoxDOF.addItem("Affine - 12");
        comboBoxDOF.setSelectedIndex(2);
        comboBoxDOF.addItemListener(this);

        JLabel labelCost = new JLabel("Cost function:");
        labelCost.setForeground(Color.black);
        labelCost.setFont(serif12);
        labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxCostFunct = new JComboBox();
        comboBoxCostFunct.setFont(MipavUtil.font12);
        comboBoxCostFunct.setBackground(Color.white);
        comboBoxCostFunct.setToolTipText("Cost function");
        
        if (!doColor) {
            comboBoxCostFunct.addItem("Correlation ratio");
        }

        comboBoxCostFunct.addItem("Least squares");
        if (!doColor) {
            comboBoxCostFunct.addItem("Normalized cross correlation");
        }
        if (!doColor) {
            comboBoxCostFunct.addItem("Normalized mutual information");
        }
        comboBoxCostFunct.setSelectedIndex(0);
        
        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Trilinear");
        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");
        comboBoxInterp.addItem("Cubic Lagrangian");
        comboBoxInterp.addItem("Quintic Lagrangian");
        comboBoxInterp.addItem("Heptic Lagrangian");
        comboBoxInterp.addItem("Windowed sinc");

        minMaxCheckbox = new JCheckBox("Use the max of the min resolutions of the two datasets when resampling.");
        minMaxCheckbox.setFont(serif12);
        minMaxCheckbox.setForeground(Color.black);
        minMaxCheckbox.setSelected(true);
        minMaxCheckbox.addItemListener(this);

     
        Insets insets = new Insets(0, 2, 0, 2);
        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;


        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelDOF, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxDOF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxInterp, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelCost, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxCostFunct, gbc);

        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 7;
        optPanel.add(minMaxCheckbox, gbc);


        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.REMAINDER;
        //optPanel.add(calcLSBox, gbc);

        universalCheckbox = new JCheckBox("Apply same rotations to all dimensions.");
        universalCheckbox.setFont(serif12);
        universalCheckbox.setForeground(Color.black);
        universalCheckbox.setSelected(true);
        universalCheckbox.addItemListener(this);

        ButtonGroup dimensionGroup = new ButtonGroup();

        xRadio = new JRadioButton("X");
        xRadio.setFont(serif12);
        xRadio.setForeground(Color.black);
        xRadio.setAlignmentX(Component.LEFT_ALIGNMENT);
        xRadio.setSelected(true);
        xRadio.setEnabled(false);
        xRadio.addItemListener(this);
        dimensionGroup.add(xRadio);

        yRadio = new JRadioButton("Y");
        yRadio.setFont(serif12);
        yRadio.setForeground(Color.black);
        yRadio.setSelected(false);
        yRadio.setEnabled(false);
        yRadio.addItemListener(this);
        dimensionGroup.add(yRadio);

        zRadio = new JRadioButton("Z");
        zRadio.setFont(serif12);
        zRadio.setForeground(Color.black);
        zRadio.setSelected(false);
        zRadio.setEnabled(false);
        zRadio.addItemListener(this);
        dimensionGroup.add(zRadio);

        JPanel xyzPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        xyzPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        xyzPanel.add(xRadio);
        xyzPanel.add(yRadio);
        xyzPanel.add(zRadio);
        
        //Rotation Range Panel
        rotateRangePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelRotateRangeX = new JLabel("Rotation angle sampling range:");
        labelRotateRangeX.setForeground(Color.black);
        labelRotateRangeX.setFont(serif12);

        JLabel labelRotateRangeToX = new JLabel("to");
        labelRotateRangeToX.setForeground(Color.black);
        labelRotateRangeToX.setFont(serif12);

        JLabel labelRotateDegreesX = new JLabel("degrees");
        labelRotateDegreesX.setFont(serif12);

        rotateBeginTextX = new JTextField("-30", 3);
        rotateEndTextX = new JTextField("30", 3);

        rotateRangePanelX.add(labelRotateRangeX);
        rotateRangePanelX.add(rotateBeginTextX);
        rotateRangePanelX.add(labelRotateRangeToX);
        rotateRangePanelX.add(rotateEndTextX);
        rotateRangePanelX.add(labelRotateDegreesX);

        // Coarse sampling rate panel
        coarsePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));
        coarsePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseX = new JLabel("Coarse angle increment: ");
        labelCoarseX.setForeground(Color.black);
        labelCoarseX.setFont(serif12);
        labelCoarseX.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseDegreesX = new JLabel("degrees");
        labelCoarseDegreesX.setFont(serif12);
        coarseRateTextX = new JTextField("15", 3);

        coarsePanelX.add(labelCoarseX);
        coarsePanelX.add(coarseRateTextX);
        coarsePanelX.add(labelCoarseDegreesX);
        coarsePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        finePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));

        JLabel labelFineX = new JLabel("Fine angle increment:");
        labelFineX.setForeground(Color.black);
        labelFineX.setFont(serif12);
        labelFineX.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelFineDegreesX = new JLabel("degrees");
        labelFineDegreesX.setFont(serif12);
        fineRateTextX = new JTextField("6", 3);

        finePanelX.add(labelFineX);
        finePanelX.add(fineRateTextX);
        finePanelX.add(labelFineDegreesX);
        finePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        rotatePanel = new JPanel();
        rotatePanel.setLayout(new GridBagLayout());
        rotatePanel.setBorder(buildTitledBorder("Rotations"));

        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        rotatePanel.add(universalCheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(xyzPanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(rotateRangePanelX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(coarsePanelX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(finePanelX, gbc);

        rotateRangePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelRotateRangeY = new JLabel("Rotation angle sampling range:");
        labelRotateRangeY.setForeground(Color.black);
        labelRotateRangeY.setFont(serif12);

        JLabel labelRotateRangeToY = new JLabel("to");
        labelRotateRangeToY.setForeground(Color.black);
        labelRotateRangeToY.setFont(serif12);

        JLabel labelRotateDegreesY = new JLabel("degrees");
        labelRotateDegreesY.setFont(serif12);

        rotateBeginTextY = new JTextField("-30", 3);
        rotateEndTextY = new JTextField("30", 3);

        rotateRangePanelY.add(labelRotateRangeY);
        rotateRangePanelY.add(rotateBeginTextY);
        rotateRangePanelY.add(labelRotateRangeToY);
        rotateRangePanelY.add(rotateEndTextY);
        rotateRangePanelY.add(labelRotateDegreesY);

        // Coarse sampling rate panel
        coarsePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));
        coarsePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseY = new JLabel("Coarse angle increment: ");
        labelCoarseY.setForeground(Color.black);
        labelCoarseY.setFont(serif12);
        labelCoarseY.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseDegreesY = new JLabel("degrees");
        labelCoarseDegreesY.setFont(serif12);

        coarseRateTextY = new JTextField("15", 3);

        coarsePanelY.add(labelCoarseY);
        coarsePanelY.add(coarseRateTextY);
        coarsePanelY.add(labelCoarseDegreesY);
        coarsePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        finePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));

        JLabel labelFineY = new JLabel("Fine angle increment:");
        labelFineY.setForeground(Color.black);
        labelFineY.setFont(serif12);
        labelFineY.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelFineDegreesY = new JLabel("degrees");
        labelFineDegreesY.setFont(serif12);

        fineRateTextY = new JTextField("6", 3);

        finePanelY.add(labelFineY);
        finePanelY.add(fineRateTextY);
        finePanelY.add(labelFineDegreesY);
        finePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        rotateRangePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelRotateRangeZ = new JLabel("Rotation angle sampling range:");
        labelRotateRangeZ.setForeground(Color.black);
        labelRotateRangeZ.setFont(serif12);

        JLabel labelRotateRangeToZ = new JLabel("to");
        labelRotateRangeToZ.setForeground(Color.black);
        labelRotateRangeToZ.setFont(serif12);

        JLabel labelRotateDegreesZ = new JLabel("degrees");
        labelRotateDegreesZ.setFont(serif12);

        rotateBeginTextZ = new JTextField("-30", 3);
        rotateEndTextZ = new JTextField("30", 3);

        rotateRangePanelZ.add(labelRotateRangeZ);
        rotateRangePanelZ.add(rotateBeginTextZ);
        rotateRangePanelZ.add(labelRotateRangeToZ);
        rotateRangePanelZ.add(rotateEndTextZ);
        rotateRangePanelZ.add(labelRotateDegreesZ);

        // Coarse sampling rate panel
        coarsePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));
        coarsePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseZ = new JLabel("Coarse angle increment: ");
        labelCoarseZ.setForeground(Color.black);
        labelCoarseZ.setFont(serif12);
        labelCoarseZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseDegreesZ = new JLabel("degrees");
        labelCoarseDegreesZ.setFont(serif12);

        coarseRateTextZ = new JTextField("15", 3);

        coarsePanelZ.add(labelCoarseZ);
        coarsePanelZ.add(coarseRateTextZ);
        coarsePanelZ.add(labelCoarseDegreesZ);
        coarsePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        finePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));

        JLabel labelFineZ = new JLabel("Fine angle increment:");
        labelFineZ.setForeground(Color.black);
        labelFineZ.setFont(serif12);
        labelFineZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelFineDegreesZ = new JLabel("degrees");
        labelFineDegreesZ.setFont(serif12);

        fineRateTextZ = new JTextField("6", 3);

        finePanelZ.add(labelFineZ);
        finePanelZ.add(fineRateTextZ);
        finePanelZ.add(labelFineDegreesZ);
        finePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        JPanel outPanel = new JPanel();
        outPanel.setLayout(new GridBagLayout());
        outPanel.setBorder(buildTitledBorder("Output Options"));

        labelInterp2 = new JLabel("Interpolation:");
        labelInterp2.setForeground(Color.black);
        labelInterp2.setFont(serif12);
        labelInterp2.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp2 = new JComboBox();
        comboBoxInterp2.setFont(serif12);
        comboBoxInterp2.setBackground(Color.white);
        comboBoxInterp2.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp2.addItem("Trilinear");
        comboBoxInterp2.addItem("Bspline 3rd order");
        comboBoxInterp2.addItem("Bspline 4th order");
        comboBoxInterp2.addItem("Cubic Lagrangian");
        comboBoxInterp2.addItem("Quintic Lagrangian");
        comboBoxInterp2.addItem("Heptic Lagrangian");
        comboBoxInterp2.addItem("Windowed sinc");
        comboBoxInterp2.addItem("Nearest Neighbor");

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(labelInterp2, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(comboBoxInterp2, gbc);
        
        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        optPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        rotatePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        mainPanel.add(optPanel);
        mainPanel.add(rotatePanel);
        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        
        
      
		
		
	}
	
	
	
	
	
	
	/**
     * Sets the variables needed to call the registration algorithm based on the values entered in the dialog.
     *
     * @return  <code>true</code> if the variables are properly set, <code>false</code> otherwise.
     */
    public boolean setVariables() {

        maxOfMinResol = minMaxCheckbox.isSelected();
        
        if (doColor) {
        	switch (comboBoxCostFunct.getSelectedIndex()) {
            	case 0:
                  cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_COLOR;
                  //costName = "LEAST_SQUARES_SMOOTHED_COLOR";
                  break;
            }
        } 
        else { // black and white
        	switch (comboBoxCostFunct.getSelectedIndex()) {
                    case 0:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                        //costName = "CORRELATION_RATIO_SMOOTHED";
                        break;
                        // case 0:  cost = AlgorithmCostFunctions.CORRELATION_RATIO;                     break;

                    case 1:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
                        //costName = "LEAST_SQUARES_SMOOTHED";

                        // cost = AlgorithmCostFunctions.LEAST_SQUARES;
                        // costName = "LEAST_SQUARES_SMOOTHED";
                        break;
                        // case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED;           break;

                    case 2:
                        cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED;
                        //costName = "NORMALIZED_XCORRELATION_SMOOTHED";
                        break;
                        // case 3:  cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION;         break;

                    case 3:
                        cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
                        //costName = "NORMALIZED_MUTUAL_INFORMATION_SMOOTHED";
                        break;

                    default:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                        //costName = "CORRELATION_RATIO_SMOOTHED";
                        break;
           }
        } // else black and white

        switch (comboBoxDOF.getSelectedIndex()) {

            case 0:
                DOF = 6;
                break;

            case 1:
                DOF = 7;
                break;

            case 2:
                DOF = 9;
                break;

            case 3:
                DOF = 12;
                break;

            default:
                DOF = 12;
                break;
        }

        switch (comboBoxInterp.getSelectedIndex()) {

            case 0:
                interp = AlgorithmTransform.TRILINEAR;
                break;

            case 1:
                interp = AlgorithmTransform.BSPLINE3;
                break;

            case 2:
                interp = AlgorithmTransform.BSPLINE4;
                break;

            case 3:
                interp = AlgorithmTransform.CUBIC_LAGRANGIAN;
                break;

            case 4:
                interp = AlgorithmTransform.QUINTIC_LAGRANGIAN;
                break;

            case 5:
                interp = AlgorithmTransform.HEPTIC_LAGRANGIAN;
                break;

            case 6:
                interp = AlgorithmTransform.WSINC;
                break;
                // case 7:  interp = AlgorithmTransform.NEAREST_NEIGHBOR;  break;

            default:
                interp = AlgorithmTransform.TRILINEAR;
                break;
        }

        if (!testParameter(rotateBeginTextX.getText(), -360, 360)) {
            showX();
            rotateBeginTextX.requestFocus();
            rotateBeginTextX.selectAll();

            return false;
        } else {
            rotateBeginX = Float.valueOf(rotateBeginTextX.getText()).floatValue();
        }

        if (!testParameter(rotateEndTextX.getText(), -360, 360)) {
            showX();
            rotateEndTextX.requestFocus();
            rotateEndTextX.selectAll();

            return false;
        } else {
            rotateEndX = Float.valueOf(rotateEndTextX.getText()).floatValue();
        }

        if (!testParameter(coarseRateTextX.getText(), 0.01, 360)) {
            showX();
            coarseRateTextX.requestFocus();
            coarseRateTextX.selectAll();

            return false;
        } else {
            coarseRateX = Float.valueOf(coarseRateTextX.getText()).floatValue();
        }

        if (rotateBeginX > rotateEndX) {
            MipavUtil.displayError("Beginning of rangeX must be less than end of range.");
            showX();
            rotateBeginTextX.requestFocus();
            rotateBeginTextX.selectAll();

            return false;
        }

        if (((rotateEndX - rotateBeginX) / coarseRateX) < 1) {
            int response = JOptionPane.showConfirmDialog(this,
                                                         "Warning: with such a large rateX, there will only be 1 sampling.  Continue?",
                                                         "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                         JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                showX();
                coarseRateTextX.requestFocus();
                coarseRateTextX.selectAll();

                return false;
            }
        }

        if (!testParameter(fineRateTextX.getText(), 0.01, 360)) {
            showX();
            fineRateTextX.requestFocus();
            fineRateTextX.selectAll();

            return false;
        } else {
            fineRateX = Float.valueOf(fineRateTextX.getText()).floatValue();
        }

        if (((rotateEndX - rotateBeginX) / fineRateX) < 1) {
            int response = JOptionPane.showConfirmDialog(this,
                                                         "Warning: with such a large rateX, there will only be 1 sampling.  Continue?",
                                                         "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                         JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                showX();
                coarseRateTextX.requestFocus();
                coarseRateTextX.selectAll();

                return false;
            }
        }

        if (universalCheckbox.isSelected()) {
            rotateBeginY = rotateBeginX;
            rotateBeginZ = rotateBeginX;
            rotateEndY = rotateEndX;
            rotateEndZ = rotateEndX;
            coarseRateY = coarseRateX;
            coarseRateZ = coarseRateX;
            fineRateY = fineRateX;
            fineRateZ = fineRateX;
        } else { // universalCheckbox not selected
            if (!testParameter(rotateBeginTextY.getText(), -360, 360)) {
                showY();
                rotateBeginTextY.requestFocus();
                rotateBeginTextY.selectAll();

                return false;
            } else {
                rotateBeginY = Float.valueOf(rotateBeginTextY.getText()).floatValue();
            }

            if (!testParameter(rotateEndTextY.getText(), -360, 360)) {
                showY();
                rotateEndTextY.requestFocus();
                rotateEndTextY.selectAll();

                return false;
            } else {
                rotateEndY = Float.valueOf(rotateEndTextY.getText()).floatValue();
            }

            if (!testParameter(coarseRateTextY.getText(), 0.01, 360)) {
                showY();
                coarseRateTextY.requestFocus();
                coarseRateTextY.selectAll();

                return false;
            } else {
                coarseRateY = Float.valueOf(coarseRateTextY.getText()).floatValue();
            }

            if (rotateBeginY > rotateEndY) {
                MipavUtil.displayError("Beginning of rangeY must be less than end of range.");
                showY();
                rotateBeginTextY.requestFocus();
                rotateBeginTextY.selectAll();

                return false;
            }

            if (((rotateEndY - rotateBeginY) / coarseRateY) < 1) {
                int response = JOptionPane.showConfirmDialog(this,
                                                             "Warning: with such a large rateY, there will only be 1 sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showY();
                    coarseRateTextY.requestFocus();
                    coarseRateTextY.selectAll();

                    return false;
                }
            }

            if (!testParameter(fineRateTextY.getText(), 0.01, 360)) {
                showY();
                fineRateTextY.requestFocus();
                fineRateTextY.selectAll();

                return false;
            } else {
                fineRateY = Float.valueOf(fineRateTextY.getText()).floatValue();
            }

            if (((rotateEndY - rotateBeginY) / fineRateY) < 1) {
                int response = JOptionPane.showConfirmDialog(this,
                                                             "Warning: with such a large rateY, there will only be 1 sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showY();
                    coarseRateTextY.requestFocus();
                    coarseRateTextY.selectAll();

                    return false;
                }
            }

            if (!testParameter(rotateBeginTextZ.getText(), -360, 360)) {
                showZ();
                rotateBeginTextZ.requestFocus();
                rotateBeginTextZ.selectAll();

                return false;
            } else {
                rotateBeginZ = Float.valueOf(rotateBeginTextZ.getText()).floatValue();
            }

            if (!testParameter(rotateEndTextZ.getText(), -360, 360)) {
                showZ();
                rotateEndTextZ.requestFocus();
                rotateEndTextZ.selectAll();

                return false;
            } else {
                rotateEndZ = Float.valueOf(rotateEndTextZ.getText()).floatValue();
            }

            if (!testParameter(coarseRateTextZ.getText(), 0.01, 360)) {
                showZ();
                coarseRateTextZ.requestFocus();
                coarseRateTextZ.selectAll();

                return false;
            } else {
                coarseRateZ = Float.valueOf(coarseRateTextZ.getText()).floatValue();
            }

            if (rotateBeginZ > rotateEndZ) {
                MipavUtil.displayError("Beginning of rangeZ must be less than end of range.");
                showZ();
                rotateBeginTextZ.requestFocus();
                rotateBeginTextZ.selectAll();

                return false;
            }

            if (((rotateEndZ - rotateBeginZ) / coarseRateZ) < 1) {
                int response = JOptionPane.showConfirmDialog(this,
                                                             "Warning: with such a large rateZ, there will only be 1 sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showZ();
                    coarseRateTextZ.requestFocus();
                    coarseRateTextZ.selectAll();

                    return false;
                }
            }

            if (!testParameter(fineRateTextZ.getText(), 0.01, 360)) {
                showZ();
                fineRateTextZ.requestFocus();
                fineRateTextZ.selectAll();

                return false;
            } else {
                fineRateZ = Float.valueOf(fineRateTextZ.getText()).floatValue();
            }

            if (((rotateEndZ - rotateBeginZ) / fineRateZ) < 1) {
                int response = JOptionPane.showConfirmDialog(this,
                                                             "Warning: with such a large rateZ, there will only be 1 sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showZ();
                    coarseRateTextZ.requestFocus();
                    coarseRateTextZ.selectAll();

                    return false;
                }
            }
        } // else universalCheckbox not selected

        return true;
    }
    
    
    
    /**
     * showX
     */
    private void showX() {

        if (xSelected) {
            return;
        } else if (ySelected) {
            rotatePanel.remove(rotateRangePanelY);
            rotatePanel.remove(coarsePanelY);
            rotatePanel.remove(finePanelY);
            ySelected = false;
        } else { // if (zSelected)
            rotatePanel.remove(rotateRangePanelZ);
            rotatePanel.remove(coarsePanelZ);
            rotatePanel.remove(finePanelZ);

        } // else if zSelected

        xSelected = true;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(rotateRangePanelX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(coarsePanelX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(finePanelX, gbc);
        xRadio.setEnabled(false);
        yRadio.setEnabled(false);
        zRadio.setEnabled(false);
        xRadio.setSelected(true);
        yRadio.setSelected(false);
        zRadio.setSelected(false);
        xRadio.setEnabled(true);
        yRadio.setEnabled(true);
        zRadio.setEnabled(true);
    }

    /**
     * showY
     */
    private void showY() {

        if (xSelected) {
            rotatePanel.remove(rotateRangePanelX);
            rotatePanel.remove(coarsePanelX);
            rotatePanel.remove(finePanelX);
            xSelected = false;
        } // if (xSelected)
        else if (ySelected) {
            return;
        } else { // zSelected
            rotatePanel.remove(rotateRangePanelZ);
            rotatePanel.remove(coarsePanelZ);
            rotatePanel.remove(finePanelZ);

        } // else zSelected

        ySelected = true;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(rotateRangePanelY, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(coarsePanelY, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(finePanelY, gbc);
        xRadio.setEnabled(false);
        yRadio.setEnabled(false);
        zRadio.setEnabled(false);
        xRadio.setSelected(false);
        yRadio.setSelected(true);
        zRadio.setSelected(false);
        xRadio.setEnabled(true);
        yRadio.setEnabled(true);
        zRadio.setEnabled(true);
    }

    /**
     * showZ
     */
    private void showZ() {

        if (xSelected) {
            rotatePanel.remove(rotateRangePanelX);
            rotatePanel.remove(coarsePanelX);
            rotatePanel.remove(finePanelX);
            xSelected = false;
        } // if (xSelcted)
        else if (ySelected) {
            rotatePanel.remove(rotateRangePanelY);
            rotatePanel.remove(coarsePanelY);
            rotatePanel.remove(finePanelY);
            ySelected = false;
        } // else if (ySelected)
        else { // zSelected
            return;
        } // else zSelected


        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(rotateRangePanelZ, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(coarsePanelZ, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(finePanelZ, gbc);
        xRadio.setEnabled(false);
        yRadio.setEnabled(false);
        zRadio.setEnabled(false);
        xRadio.setSelected(false);
        yRadio.setSelected(false);
        zRadio.setSelected(true);
        xRadio.setEnabled(true);
        yRadio.setEnabled(true);
        zRadio.setEnabled(true);
    }
	
	
	
    public void itemStateChanged(ItemEvent event) {
    	if (event.getSource() == universalCheckbox) {

            if (universalCheckbox.isSelected()) {
                xRadio.setEnabled(false);
                yRadio.setEnabled(false);
                zRadio.setEnabled(false);
                xRadio.setSelected(true);
                yRadio.setSelected(false);
                zRadio.setSelected(false);

                if (xSelected) {
                    return;
                } else if (ySelected) {
                    rotatePanel.remove(rotateRangePanelY);
                    rotatePanel.remove(coarsePanelY);
                    rotatePanel.remove(finePanelY);
                    ySelected = false;
                } else { // if (zSelected)
                    rotatePanel.remove(rotateRangePanelZ);
                    rotatePanel.remove(coarsePanelZ);
                    rotatePanel.remove(finePanelZ);

                } // else if zSelected

                xSelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 4;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(finePanelX, gbc);
            } else {
                xRadio.setEnabled(true);
                yRadio.setEnabled(true);
                zRadio.setEnabled(true);
            }
        } // else if (event.getSource() == universalCheckbox)
        else if ((event.getSource() == xRadio) || (event.getSource() == yRadio) || (event.getSource() == zRadio)) {

            if (xRadio.isSelected()) {

                if (xSelected) {
                    return;
                } else if (ySelected) {
                    rotatePanel.remove(rotateRangePanelY);
                    rotatePanel.remove(coarsePanelY);
                    rotatePanel.remove(finePanelY);
                    ySelected = false;
                } else { // if (zSelected)
                    rotatePanel.remove(rotateRangePanelZ);
                    rotatePanel.remove(coarsePanelZ);
                    rotatePanel.remove(finePanelZ);

                } // else if zSelected

                xSelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 4;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(finePanelX, gbc);
            } // if (xRadio.isSelected)
            else if (yRadio.isSelected()) {

                if (xSelected) {
                    rotatePanel.remove(rotateRangePanelX);
                    rotatePanel.remove(coarsePanelX);
                    rotatePanel.remove(finePanelX);
                    xSelected = false;
                } // if (xSelected)
                else if (ySelected) {
                    return;
                } else { // zSelected
                    rotatePanel.remove(rotateRangePanelZ);
                    rotatePanel.remove(coarsePanelZ);
                    rotatePanel.remove(finePanelZ);

                } // else zSelected

                ySelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelY, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelY, gbc);

                gbc.gridx = 0;
                gbc.gridy = 4;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(finePanelY, gbc);
            } // else if (yRadio.isSelected())
            else if (zRadio.isSelected()) {

                if (xSelected) {
                    rotatePanel.remove(rotateRangePanelX);
                    rotatePanel.remove(coarsePanelX);
                    rotatePanel.remove(finePanelX);
                    xSelected = false;
                } // if (xSelcted)
                else if (ySelected) {
                    rotatePanel.remove(rotateRangePanelY);
                    rotatePanel.remove(coarsePanelY);
                    rotatePanel.remove(finePanelY);
                    ySelected = false;
                } // else if (ySelected)
                else { // zSelected
                    return;
                } // else zSelected


                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelZ, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelZ, gbc);

                gbc.gridx = 0;
                gbc.gridy = 4;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(finePanelZ, gbc);
            } // else if (zRadio.isSelected())

            rotatePanel.validate();
            repaint();
        } // else if xRadio, yRadio, or zRadio
    	
    }
	
	
	
	
	/**
	 * actionPerformed
	 * @param e
	 */
	public void actionPerformed(ActionEvent e) {
		
		String command = e.getActionCommand();
		if (command.equals("OK")) {
            if (setVariables()) {
                setVisible(false);
            }
        } 
		else if (command.equals("Cancel")) {
			setVisible(false);
        }
	}



	public int getCost() {
		return cost;
	}



	public int getDOF() {
		return DOF;
	}



	public int getInterp() {
		return interp;
	}



	public float getCoarseRateX() {
		return coarseRateX;
	}



	public float getCoarseRateY() {
		return coarseRateY;
	}



	public float getCoarseRateZ() {
		return coarseRateZ;
	}



	public float getRotateBeginX() {
		return rotateBeginX;
	}



	public float getRotateBeginY() {
		return rotateBeginY;
	}



	public float getRotateBeginZ() {
		return rotateBeginZ;
	}



	public float getRotateEndX() {
		return rotateEndX;
	}



	public float getRotateEndY() {
		return rotateEndY;
	}



	public float getRotateEndZ() {
		return rotateEndZ;
	}



	public float getFineRateX() {
		return fineRateX;
	}



	public float getFineRateY() {
		return fineRateY;
	}



	public float getFineRateZ() {
		return fineRateZ;
	}



	public boolean isMaxOfMinResol() {
		return maxOfMinResol;
	}



	public int getInterp2() {
		return interp2;
	}
	
	
	
	
	


}
