package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.view.MipavUtil;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public class JDialogDTICreateListFileRegOAR35DOptions extends JDialogBase {

//	~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4309868934393418962L;

    //~ Instance fields ------------------------------------------------------------------------------------------------



    /** Variables for Advanced Settings dialog. */
    private JDialog advancedDialog;



    /** DOCUMENT ME! */
    private JTextField bracketBoundText, maxIterationsText, numMinText;


    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private JComboBox comboBoxDOF;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp2;

    /** DOCUMENT ME! */
    private int cost, interp, interp2, DOF;

    /** DOCUMENT ME! */
    private boolean doSubsample;
    
    private boolean doMultiThread;

    /** DOCUMENT ME! */
    private boolean fastMode;

    /** DOCUMENT ME! */
    private JCheckBox fastModeCheckbox;
    
    /** DOCUMENT ME! */
    private boolean maxOfMinResol;


    /** DOCUMENT ME! */
    private JLabel labelInterp2;
    
    /** DOCUMENT ME! */
    private GridBagConstraints gbc;
    
    /** DOCUMENT ME! */
    private boolean xSelected = true;
    
    /** DOCUMENT ME! */
    private boolean ySelected = false;
    
    private JPanel rotatePanel;


    /** DOCUMENT ME! */
    private int maxIterations_def = 2, bracketBound_def = 10, numMinima_def = 3;

    /** DOCUMENT ME! */
    private int maxIterations = maxIterations_def, bracketBound = bracketBound_def;

    /** DOCUMENT ME! */
    private int numMinima = numMinima_def;

    // 3 for reference
    private int registerTo = 3;
    
    private boolean isDICOM;

    /** DOCUMENT ME! */
    private JCheckBox universalCheckbox;
    
    /** DOCUMENT ME! */
    private JRadioButton xRadio, yRadio, zRadio;
    
    /** DOCUMENT ME! */
    private JPanel rotateRangePanelX, rotateRangePanelY, rotateRangePanelZ;
    
    /** DOCUMENT ME! */
    private JPanel finePanelX, finePanelY, finePanelZ;
    
    /** DOCUMENT ME! */
    private JPanel coarsePanelX, coarsePanelY, coarsePanelZ;
   
    /** DOCUMENT ME! */
    private float rotateBegin, rotateEnd, coarseRate, fineRate, rotateBeginX, rotateEndX, coarseRateX, fineRateX,rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;

    /** DOCUMENT ME! */
    private JTextField rotateBeginText, rotateEndText, coarseRateText, fineRateText,rotateBeginTextX, rotateEndTextX, coarseRateTextX, fineRateTextX,rotateBeginTextY, rotateEndTextY, coarseRateTextY, fineRateTextY,rotateBeginTextZ, rotateEndTextZ, coarseRateTextZ, fineRateTextZ;

    /** DOCUMENT ME! */
    private JCheckBox sampleCheckBox;
    
    private JCheckBox multiThreadCheckBox;
    
    /** DOCUMENT ME! */
    private JCheckBox minMaxCheckbox;




    //~ Constructors ---------------------------------------------------------------------------------------------------



    /**
     * Creates new dialog for user to choose variables for internal registration.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogDTICreateListFileRegOAR35DOptions(boolean isDICOM) {
    	super(true);
        this.isDICOM = isDICOM;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        String tmpStr;

        if (command.equals("OK")) {
            if (setVariables()) {
            	setVisible(false);
            }
        } else if (command.equals("Cancel")) {
        	setVisible(false);
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("OAR19076");
            MipavUtil.showWebHelp("Optimized_automatic_registration_3D#Optimized_Automatic_Registration_dialog_box_options");
        } else if (command.equals("AdvancedSettings")) {
            bracketBound_def = bracketBound;
            maxIterations_def = maxIterations;
            numMinima_def = numMinima;
            advancedDialog = buildAdvancedDialog(bracketBound, maxIterations, numMinima);
        }else if (command.equals("AdvancedOkay")) {
            tmpStr = bracketBoundText.getText();

            if (testParameter(tmpStr, 1, 60)) {
                bracketBound = Integer.valueOf(tmpStr).intValue();
            } else {
                bracketBound = bracketBound_def;
            }

            tmpStr = maxIterationsText.getText();

            if (testParameter(tmpStr, 1, 100)) {
                maxIterations = Integer.valueOf(tmpStr).intValue();
            } else {
                maxIterations = maxIterations_def;
            }

            tmpStr = numMinText.getText();

            if (testParameter(tmpStr, 1, 25)) {
                numMinima = Integer.valueOf(tmpStr).intValue();
            } else {
                numMinima = numMinima_def;
            }

            advancedDialog.setVisible(false);
            advancedDialog.dispose();
        } else if (command.equals("AdvancedCancel")) {
            maxIterations = maxIterations_def;
            bracketBound = bracketBound_def;
            numMinima = numMinima_def;
            advancedDialog.setVisible(false);
            advancedDialog.dispose();
        } else if (command.equals("AdvancedHelp")) {
        	//MipavUtil.showHelp("OAR19078");
            MipavUtil.showWebHelp("Optimized_automatic_registration_3D#Advanced_OAR_settings_for_Constrained_Optimized_Automatic_Registration_3D");
        } else {
            super.actionPerformed(event);
        }
    }



    /**
     * Changes the interpolation box to enabled or disabled depending on if the transform box is checked or not.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {

    	if (event.getSource() == fastModeCheckbox) {

            // enable or disable search variables
            fastMode = fastModeCheckbox.isSelected();
            if(!isDICOM) {
	            rotateBeginText.setEnabled(!fastModeCheckbox.isSelected());
	            rotateEndText.setEnabled(!fastModeCheckbox.isSelected());
	            coarseRateText.setEnabled(!fastModeCheckbox.isSelected());
	            fineRateText.setEnabled(!fastModeCheckbox.isSelected());
            }else {
            	rotateBeginTextX.setEnabled(!fastModeCheckbox.isSelected());
                rotateEndTextX.setEnabled(!fastModeCheckbox.isSelected());
                coarseRateTextX.setEnabled(!fastModeCheckbox.isSelected());
                fineRateTextX.setEnabled(!fastModeCheckbox.isSelected());
                rotateBeginTextY.setEnabled(!fastModeCheckbox.isSelected());
                rotateEndTextY.setEnabled(!fastModeCheckbox.isSelected());
                coarseRateTextY.setEnabled(!fastModeCheckbox.isSelected());
                fineRateTextY.setEnabled(!fastModeCheckbox.isSelected());
                rotateBeginTextZ.setEnabled(!fastModeCheckbox.isSelected());
                rotateEndTextZ.setEnabled(!fastModeCheckbox.isSelected());
                coarseRateTextZ.setEnabled(!fastModeCheckbox.isSelected());
                fineRateTextZ.setEnabled(!fastModeCheckbox.isSelected());
            }

        }else if (event.getSource() == universalCheckbox) {

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
        }
    }

    /**
     * Accessor to set bracketBound.
     *
     * @param  bracketBound  DOCUMENT ME!
     */
    public void setBracketBound(int bracketBound) {
        this.bracketBound = bracketBound;
    }

    /**
     * Accessor to set the coarse sample begin.
     *
     * @param  x  Coarse begin
     */
    public void setCoarseBegin(float x) {
        rotateBegin = x;
    }

    /**
     * Accessor to set the coarse sample end.
     *
     * @param  x  Coarse end
     */
    public void setCoarseEnd(float x) {
        rotateEnd = x;
    }

    /**
     * Accessor to set the coarse sample rate.
     *
     * @param  x  Coarse rate
     */
    public void setCoarseRate(float x) {
        coarseRate = x;
    }

    /**
     * Accessor to set the choice of cost function.
     *
     * @param  x  Cost function.
     */
    public void setCostChoice(int x) {
        cost = x;
    }

    /**
     * Accessor to set the degrees of freedom.
     *
     * @param  x  Degrees of freedom
     */
    public void setDOF(int x) {
        DOF = x;
    }

    /**
     * Accessor to set whether or not to execute the fast mode (skip sub sample and goto last final optimization).
     *
     * @param  flag  <code>true</code> then skip to level one (last ) optimization.
     */
    public void setFastMode(boolean flag) {
        fastMode = flag;
    }

    /**
     * Accessor to set the fine sample rate.
     *
     * @param  x  Fine rate
     */
    public void setFineRate(float x) {
        fineRate = x;
    }




    /**
     * Accessor to set the initial interpolation.
     *
     * @param  x  Interpolation
     */
    public void setInterp(int x) {
        interp = x;
    }

    /**
     * Accessor to set the final interpolation.
     *
     * @param  x  Interpolation
     */
    public void setInterp2(int x) {
        interp2 = x;
    }

    /**
     * Accessor to set maxIterations.
     *
     * @param  maxIterations  DOCUMENT ME!
     */
    public void setMaxIterations(int maxIterations) {
        this.maxIterations = maxIterations;
    }

    /**
     * Accessor to set numMinima.
     *
     * @param  numMinima  DOCUMENT ME!
     */
    public void setNumMinima(int numMinima) {
        this.numMinima = numMinima;
    }



    /**
     * Accessor to set whether or not subsampling occurs.
     *
     * @param  doSubsample  DOCUMENT ME!
     */
    public void setSubsample(boolean doSubsample) {
        this.doSubsample = doSubsample;
    }

    /**
     * Accessor to set if multithreading is used
     * @param doMultiThread
     */
    public void setMultiThread(boolean doMultiThread) {
    	this.doMultiThread = doMultiThread;
    }

    

    



    /**
     * Build advanced settings dialog. Returns JDialog.
     *
     * @param   bracketBound  DOCUMENT ME!
     * @param   maxIter       DOCUMENT ME!
     * @param   numMinima     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JDialog buildAdvancedDialog(int bracketBound, int maxIter, int numMinima) {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        advancedDialog = new JDialog(this, "Advanced OAR settings", false);
        // Parent is the JDialogRegistrationOAR3D, title, modal
        // Changed dialog to non-modal after adding Help button 12/17/07

        // Setting panel
        JPanel settingsPanel = new JPanel();
        settingsPanel.setBorder(BorderFactory.createTitledBorder("Optimization settings"));
        settingsPanel.setLayout(new BoxLayout(settingsPanel, BoxLayout.Y_AXIS));

        JPanel bracketPanel = new JPanel();
        bracketPanel.setLayout(new BorderLayout(1, 3)); // BorderLayout(int hgap, int vgap)
        bracketPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel bracketBoundLabel = new JLabel("Multiple of tolerance to bracket the minimum: ", JLabel.LEFT);
        bracketPanel.add(bracketBoundLabel, BorderLayout.WEST);
        bracketPanel.setToolTipText("Used for translation, scale and skew.");
        bracketBoundText = new JTextField(String.valueOf(bracketBound), 5);
        bracketBoundText.addFocusListener(this);
        bracketPanel.add(bracketBoundText, BorderLayout.CENTER);

        JLabel bracketInstruct = new JLabel("Recommended values 10-60.", JLabel.RIGHT);
        bracketPanel.add(bracketInstruct, BorderLayout.SOUTH);

        JPanel maxIterPanel = new JPanel();
        maxIterPanel.setLayout(new BorderLayout(1, 3));
        maxIterPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel maxIterationsLabel = new JLabel("Number of iterations: ", JLabel.LEFT);
        maxIterPanel.add(maxIterationsLabel, BorderLayout.WEST);
        maxIterPanel.setToolTipText("Used for levelOne. Other levels are multiples of this #.");
        maxIterationsText = new JTextField(String.valueOf(maxIter), 5);
        maxIterationsText.addFocusListener(this);
        maxIterPanel.add(maxIterationsText, BorderLayout.CENTER);

        JLabel maxIterInstruct = new JLabel("Recommended value 1-5.", JLabel.RIGHT);
        maxIterPanel.add(maxIterInstruct, BorderLayout.SOUTH);

        JPanel numMinPanel = new JPanel();
        numMinPanel.setLayout(new BorderLayout(1, 3));
        numMinPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel numMinLabel = new JLabel("Number of minima from Level 8 to test at Level 4: ", JLabel.LEFT);
        numMinPanel.add(numMinLabel, BorderLayout.WEST);
        numMinPanel.setToolTipText("Increasing will significantly increase processing time.");
        numMinText = new JTextField(String.valueOf(numMinima), 5);
        numMinText.addFocusListener(this);
        numMinPanel.add(numMinText, BorderLayout.CENTER);

        fastModeCheckbox = new JCheckBox("Skip multilevel search.  Assume images are close to alignment.");
        fastModeCheckbox.setFont(serif12);
        fastModeCheckbox.setForeground(Color.black);
        fastModeCheckbox.setSelected(false);
        fastModeCheckbox.setEnabled(true);
        fastModeCheckbox.addItemListener(this);
        fastModeCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);


        settingsPanel.add(bracketPanel);
        settingsPanel.add(Box.createVerticalStrut(20));
        settingsPanel.add(maxIterPanel);
        settingsPanel.add(Box.createVerticalStrut(20));
        settingsPanel.add(numMinPanel);
        settingsPanel.add(Box.createVerticalStrut(15));
        settingsPanel.add(fastModeCheckbox);

        advancedDialog.getContentPane().add(settingsPanel, BorderLayout.NORTH);

        // Okay-Cancel Panel
        JPanel okayCancelPanel = new JPanel(new FlowLayout());
        JButton advCancelButton = new JButton("Cancel");
        advCancelButton.setActionCommand("AdvancedCancel");
        advCancelButton.addActionListener(this);
        advCancelButton.setPreferredSize(new Dimension(120, 30));
        advCancelButton.setFont(serif12B);

        // okayCancelPanel.add(cancelButton);
        JButton okayButton = new JButton("OK");
        okayButton.setActionCommand("AdvancedOkay");
        okayButton.addActionListener(this);
        okayButton.setPreferredSize(new Dimension(120, 30));
        okayButton.setFont(serif12B);
        
        // Help Button
        JButton helpButton = new JButton("Help");
        helpButton.setActionCommand("AdvancedHelp");
        helpButton.addActionListener(this);
        helpButton.setPreferredSize(new Dimension(120,30));
        helpButton.setFont(serif12B);
        
        okayCancelPanel.add(okayButton);
        okayCancelPanel.add(advCancelButton);
        okayCancelPanel.add(helpButton);
        

        advancedDialog.getContentPane().add(okayCancelPanel, BorderLayout.SOUTH);

        Rectangle dialogBounds = this.getBounds();
        advancedDialog.setLocation((int) ((Toolkit.getDefaultToolkit().getScreenSize().width * 0.75) -
                                          (dialogBounds.width / 2)),
                                   (Toolkit.getDefaultToolkit().getScreenSize().height / 2) -
                                   (dialogBounds.height / 2));

        advancedDialog.pack();
        advancedDialog.setVisible(true);

        return advancedDialog;
    }

    /**
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Optimized Automatic Image Registration Options");

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
        comboBoxCostFunct.addItem("Correlation ratio");
        comboBoxCostFunct.addItem("Least squares");
        comboBoxCostFunct.addItem("Normalized cross correlation");
        comboBoxCostFunct.addItem("Normalized mutual information");
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

        sampleCheckBox = new JCheckBox("Subsample image for speed");
        sampleCheckBox.setFont(serif12);
        sampleCheckBox.setForeground(Color.black);
        sampleCheckBox.setSelected(true);
        sampleCheckBox.setEnabled(true);
        
        multiThreadCheckBox = new JCheckBox("Multi-threading enabled (not deterministic)");
        multiThreadCheckBox.setFont(serif12);
        multiThreadCheckBox.setForeground(Color.black);
        multiThreadCheckBox.setSelected(true);
        multiThreadCheckBox.setEnabled(true);

        
        
        Insets insets = new Insets(0, 2, 0, 2);
        gbc = new GridBagConstraints();

        gbc.insets = insets;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;


        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelDOF, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxDOF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxInterp, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelCost, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxCostFunct, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        optPanel.add(sampleCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        optPanel.add(multiThreadCheckBox, gbc);
        
        if(isDICOM) {
        	minMaxCheckbox = new JCheckBox("Use the max of the min resolutions of the two datasets when resampling.");
            minMaxCheckbox.setFont(serif12);
            minMaxCheckbox.setForeground(Color.black);
            minMaxCheckbox.setSelected(true);
            minMaxCheckbox.addItemListener(this);

        }
        
        
        
        
        
        rotatePanel = new JPanel();
        rotatePanel.setLayout(new GridBagLayout());
        rotatePanel.setBorder(buildTitledBorder("Rotate Options"));

        
        
        if(!isDICOM) {
        	//Rotation Range Panel
	        JPanel rotateRangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
	        JLabel labelRotateRange = new JLabel("Rotation angle sampling range:");
	        labelRotateRange.setForeground(Color.black);
	        labelRotateRange.setFont(serif12);
	
	        JLabel labelRotateRangeTo = new JLabel("to");
	        labelRotateRangeTo.setForeground(Color.black);
	        labelRotateRangeTo.setFont(serif12);
	
	        JLabel labelRotateDegrees = new JLabel("degrees");
	        labelRotateDegrees.setFont(serif12);
	
	        rotateBeginText = new JTextField("-3", 3);
	        rotateEndText = new JTextField("3", 3);
	        
	        rotateRangePanel.add(labelRotateRange);
	        rotateRangePanel.add(rotateBeginText);
	        rotateRangePanel.add(labelRotateRangeTo);
	        rotateRangePanel.add(rotateEndText);
	        rotateRangePanel.add(labelRotateDegrees);
	
	        // Coarse sampling rate panel
	        JPanel coarsePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
	
	        JLabel labelCoarse = new JLabel("Coarse angle increment: ");
	        labelCoarse.setForeground(Color.black);
	        labelCoarse.setFont(serif12);
	        labelCoarse.setAlignmentX(Component.LEFT_ALIGNMENT);
	
	        JLabel labelCoarseDegrees = new JLabel("degrees");
	        labelCoarseDegrees.setFont(serif12);
	        coarseRateText = new JTextField("1", 3);
	
	        coarsePanel.add(labelCoarse);
	        coarsePanel.add(coarseRateText);
	        coarsePanel.add(labelCoarseDegrees);
	        coarsePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
	
	        // Fine sampling rate panel
	        JPanel finePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
	
	        JLabel labelFine = new JLabel("Fine angle increment:");
	        labelFine.setForeground(Color.black);
	        labelFine.setFont(serif12);
	        labelFine.setAlignmentX(Component.LEFT_ALIGNMENT);
	
	        JLabel labelFineDegrees = new JLabel("degrees");
	        labelFineDegrees.setFont(serif12);
	        fineRateText = new JTextField("1", 3);
	
	        finePanel.add(labelFine);
	        finePanel.add(fineRateText);
	        finePanel.add(labelFineDegrees);
	        finePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
	        
	        
	        gbc.weightx = 0;
	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        rotatePanel.add(rotateRangePanel, gbc);
	
	        gbc.gridx = 0;
	        gbc.gridy = 1;
	        rotatePanel.add(coarsePanel, gbc);
	
	        gbc.gridx = 0;
	        gbc.gridy = 2;
	        rotatePanel.add(finePanel, gbc);
        }else {
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

            rotateBeginTextX = new JTextField("-3", 3);
            rotateEndTextX = new JTextField("3", 3);

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
            coarseRateTextX = new JTextField("1", 3);

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
            fineRateTextX = new JTextField("1", 3);

            finePanelX.add(labelFineX);
            finePanelX.add(fineRateTextX);
            finePanelX.add(labelFineDegreesX);
            finePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

   

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

            rotateBeginTextY = new JTextField("-3", 3);
            rotateEndTextY = new JTextField("3", 3);

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

            coarseRateTextY = new JTextField("1", 3);

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

            fineRateTextY = new JTextField("1", 3);

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

            rotateBeginTextZ = new JTextField("-3", 3);
            rotateEndTextZ = new JTextField("3", 3);

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

            coarseRateTextZ = new JTextField("1", 3);

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

            fineRateTextZ = new JTextField("1", 3);

            finePanelZ.add(labelFineZ);
            finePanelZ.add(fineRateTextZ);
            finePanelZ.add(labelFineDegreesZ);
            finePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);
        	
        	
        	
        	
        }
        

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
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(labelInterp2, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(comboBoxInterp2, gbc);


        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        JButton advancedButton = new JButton("Advanced settings");
        advancedButton.setActionCommand("AdvancedSettings");
        advancedButton.addActionListener(this);
        advancedButton.setMinimumSize(new Dimension(90, 30));
        advancedButton.setFont(serif12B);
        buttonPanel.add(advancedButton);

        
        
        
        
        JPanel mainPanel = new JPanel(new BorderLayout());
        //mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        //mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        //optPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        mainPanel.add(optPanel,BorderLayout.NORTH);
        mainPanel.add(rotatePanel,BorderLayout.CENTER);
        mainPanel.add(outPanel,BorderLayout.SOUTH);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(false);
    }


    /**
     * Sets the variables needed to call the registration algorithm based on the values entered in the dialog.
     *
     * @return  <code>true</code> if the variables are properly set, <code>false</code> otherwise.
     */
    public boolean setVariables() {
        
        switch (comboBoxCostFunct.getSelectedIndex()) {

            case 0:
                cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                break;

            case 1:
                cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
                break;
                // case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED;             break;

            case 2:
                cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED;
                break;

            case 3:
                cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
                break;

            default:
                cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                break;
        }


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

            default:
                interp = AlgorithmTransform.TRILINEAR;
                break;
        }


        switch (comboBoxInterp2.getSelectedIndex()) {

            case 0:
                interp2 = AlgorithmTransform.TRILINEAR;
                break;

            case 1:
                interp2 = AlgorithmTransform.BSPLINE3;
                break;

            case 2:
                interp2 = AlgorithmTransform.BSPLINE4;
                break;

            case 3:
                interp2 = AlgorithmTransform.CUBIC_LAGRANGIAN;
                break;

            case 4:
                interp2 = AlgorithmTransform.QUINTIC_LAGRANGIAN;
                break;

            case 5:
                interp2 = AlgorithmTransform.HEPTIC_LAGRANGIAN;
                break;

            case 6:
                interp2 = AlgorithmTransform.WSINC;
                break;

            case 7:
                interp2 = AlgorithmTransform.NEAREST_NEIGHBOR;
                break;

            default:
                interp2 = AlgorithmTransform.TRILINEAR;
                break;
        }

        if(!isDICOM) {
	        
	        if (!testParameter(rotateBeginText.getText(), -360, 360)) {
	            rotateBeginText.requestFocus();
	            rotateBeginText.selectAll();
	
	            return false;
	        } else {
	            rotateBegin = Float.valueOf(rotateBeginText.getText()).floatValue();
	        }
	
	        if (!testParameter(rotateEndText.getText(), -360, 360)) {
	            rotateEndText.requestFocus();
	            rotateEndText.selectAll();
	
	            return false;
	        } else {
	            rotateEnd = Float.valueOf(rotateEndText.getText()).floatValue();
	        }
	
	        if (!testParameter(coarseRateText.getText(), 0.01, 360)) {
	            coarseRateText.requestFocus();
	            coarseRateText.selectAll();
	
	            return false;
	        } else {
	            coarseRate = Float.valueOf(coarseRateText.getText()).floatValue();
	        }
	
	        if (rotateBegin > rotateEnd) {
	            MipavUtil.displayError("Beginning of range must be less than end of range.");
	            rotateBeginText.requestFocus();
	            rotateBeginText.selectAll();
	
	            return false;
	        }
	
	        if (((rotateEnd - rotateBegin) / coarseRate) < 1) {
	            int response = JOptionPane.showConfirmDialog(this,
	                                                         "Warning: with such a large rate, there will only be 1 sampling.  Continue?",
	                                                         "Sampling warning", JOptionPane.YES_NO_OPTION,
	                                                         JOptionPane.WARNING_MESSAGE);
	
	            if (response == JOptionPane.NO_OPTION) {
	                coarseRateText.requestFocus();
	                coarseRateText.selectAll();
	
	                return false;
	            }
	        }
	
	        if (!testParameter(rotateBeginText.getText(), -360, 360)) {
	            rotateBeginText.requestFocus();
	            rotateBeginText.selectAll();
	
	            return false;
	        } else {
	            rotateBegin = Float.valueOf(rotateBeginText.getText()).floatValue();
	        }
	
	        if (!testParameter(rotateEndText.getText(), -360, 360)) {
	            rotateEndText.requestFocus();
	            rotateEndText.selectAll();
	
	            return false;
	        } else {
	            rotateEnd = Float.valueOf(rotateEndText.getText()).floatValue();
	        }
	
	        if (!testParameter(fineRateText.getText(), 0.01, 360)) {
	            fineRateText.requestFocus();
	            fineRateText.selectAll();
	
	            return false;
	        } else {
	            fineRate = Float.valueOf(fineRateText.getText()).floatValue();
	        }
	
	        if (rotateBegin > rotateEnd) {
	            MipavUtil.displayError("Beginning of range must be less than end of range.");
	            rotateBeginText.requestFocus();
	            rotateBeginText.selectAll();
	
	            return false;
	        }
	
	        if (((rotateEnd - rotateBegin) / fineRate) < 1) {
	            int response = JOptionPane.showConfirmDialog(this,
	                                                         "Warning: with such a large rate, there will only be 1 sampling.  Continue?",
	                                                         "Sampling warning", JOptionPane.YES_NO_OPTION,
	                                                         JOptionPane.WARNING_MESSAGE);
	
	            if (response == JOptionPane.NO_OPTION) {
	                coarseRateText.requestFocus();
	                coarseRateText.selectAll();
	
	                return false;
	            }
	        }
        }else {
        	maxOfMinResol = minMaxCheckbox.isSelected();
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
        }

 

        // reference
        registerTo = 3;


       

        doSubsample = sampleCheckBox.isSelected();
        doMultiThread = multiThreadCheckBox.isSelected();

        return true;
    }

	@Override
	public void windowClosing(WindowEvent event) {
		setVisible(false);
	}
	
	/**
     * DOCUMENT ME!
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
     * DOCUMENT ME!
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
     * DOCUMENT ME!
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

	
	public int getCost() {
		return cost;
	}

	public int getDOF() {
		return DOF;
	}

	public int getInterp() {
		return interp;
	}

	public int getInterp2() {
		return interp2;
	}

	public int getRegisterTo() {
		return registerTo;
	}

	public float getRotateBegin() {
		return rotateBegin;
	}

	public float getRotateEnd() {
		return rotateEnd;
	}

	public float getCoarseRate() {
		return coarseRate;
	}

	public float getFineRate() {
		return fineRate;
	}

	public boolean isDoSubsample() {
		return doSubsample;
	}
	
	public boolean isDoMultiThread() {
		return doMultiThread;
	}

	public boolean isFastMode() {
		return fastMode;
	}

	public int getBracketBound() {
		return bracketBound;
	}

	public int getMaxIterations() {
		return maxIterations;
	}

	public int getNumMinima() {
		return numMinima;
	}

	public float getCoarseRateX() {
		return coarseRateX;
	}

	public float getCoarseRateZ() {
		return coarseRateZ;
	}

	public float getCoarseRateY() {
		return coarseRateY;
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

	public boolean isMaxOfMinResol() {
		return maxOfMinResol;
	}
	
	
	
	
	
	
	
	
	
	
	
    
    
    

}
