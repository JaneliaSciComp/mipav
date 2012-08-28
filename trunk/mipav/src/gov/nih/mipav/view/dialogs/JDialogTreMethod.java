package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.AlgorithmTreParams.Threshold;

import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public abstract class JDialogTreMethod extends JDialogScriptableBase {

    protected JRadioButton smartCheckBox, hardCheckBox, noCheckBox;
    
    private ButtonGroup thresholdGroup;
    protected JPanel totalThreshold;
    private JPanel generalThresholdPanel;
    
    protected Threshold thresholdMethod;
    
    protected GuiBuilder guiBuilder;
    
    /**Whether, during "smart-thresholding", noise should be calculated from a given corner */
    protected boolean upperLeftCorner = true;
    protected boolean upperRightCorner = false;
    protected boolean lowerLeftCorner = false;
    protected boolean lowerRightCorner = false;
    
    private JTextField hardNoiseField;
    private JTextField smartNoiseField;
    private JCheckBox topLeftBox;
    private JCheckBox topRightBox;
    private JCheckBox bottomLeftBox;
    private JCheckBox bottomRightBox;
    
    protected float noiseScale = (float) 4.00;
    protected float hardNoiseThreshold = (float) 0.00;
    
    protected static final String SUCCESS = "Successful";
    
    public JDialogTreMethod() {}
    
    public JDialogTreMethod(Frame theParentFrame, boolean b) {
        super(theParentFrame, b);
    }

    protected JPanel buildThresholdPanel() {
        //System.out.println("The selected state: "+thresholdMethod);
        
        JPanel panel = new JPanel();
        panel.setName("Threshold total");
        LayoutManager panelLayout = new GridBagLayout();
        panel.setLayout(panelLayout);
        GridBagConstraints gbc = new GridBagConstraints();
        panel.setBorder(MipavUtil.buildTitledBorder("Thresholding"));
        
        JPanel methodPanel = new JPanel();
        LayoutManager methodLayout = new BoxLayout(methodPanel, BoxLayout.Y_AXIS);
        methodPanel.setLayout(methodLayout);
        methodPanel.setBorder(MipavUtil.buildTitledBorder("Thresholding method"));
        
        smartCheckBox = guiBuilder.buildRadioButton("Use Smart Thresholding", thresholdMethod.equals(Threshold.SMART));
        hardCheckBox = guiBuilder.buildRadioButton("Use Hard Thresholding", thresholdMethod.equals(Threshold.HARD));
        noCheckBox = guiBuilder.buildRadioButton("No Thresholding", thresholdMethod.equals(Threshold.NONE));
        
        thresholdGroup = new ButtonGroup();
        thresholdGroup.add(smartCheckBox);
        thresholdGroup.add(hardCheckBox);
        thresholdGroup.add(noCheckBox);
        
        if(thresholdMethod == null) {
            smartCheckBox.setSelected(true);
            thresholdMethod = Threshold.SMART;
        }
        
        ActionListener c = new ThresholdChoiceListener();
        smartCheckBox.addActionListener(c);
        hardCheckBox.addActionListener(c);
        noCheckBox.addActionListener(c);
    
        methodPanel.add(smartCheckBox, methodLayout);
        methodPanel.add(hardCheckBox, methodLayout);
        methodPanel.add(noCheckBox, methodLayout);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        panel.add(methodPanel, gbc);
        
        generalThresholdPanel = new JPanel();
        
        JPanel innerPanel = null;
        
        if(thresholdMethod.equals(Threshold.SMART)) {
            innerPanel = buildSmartThresholdPanel();
        } else if(thresholdMethod.equals(Threshold.HARD)) {
            innerPanel = buildHardThresholdPanel();
        } else {
            innerPanel = buildNoThresholdPanel();
            thresholdMethod = Threshold.NONE;
        }
        generalThresholdPanel.add(innerPanel);
        
        gbc.gridy = 1;
        gbc.weighty = 0;
        gbc.insets = new Insets(20, 0, 0, 0);
        panel.add(generalThresholdPanel, gbc);
        
        gbc.gridy++;
        gbc.weighty = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        panel.add(new JLabel(""), gbc);
        
        return panel;
    }
    
    /**
     * No border
     * @return
     */
    protected JPanel buildHardThresholdPanel() {
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        hardNoiseField = guiBuilder.buildDecimalField("Hard Noise Level", hardNoiseThreshold);
        
        panel.add(hardNoiseField.getParent(), panelLayout);
        
        return panel;
    }

    protected JPanel buildSmartThresholdPanel() {
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        smartNoiseField = guiBuilder.buildDecimalField("Noise Level Scale", noiseScale);
        topLeftBox = guiBuilder.buildCheckBox("Calculate Noise from TOP LEFT Corner?", upperLeftCorner);
        topRightBox = guiBuilder.buildCheckBox("Calculate Noise from TOP RIGHT Corner?", upperRightCorner);
        bottomLeftBox = guiBuilder.buildCheckBox("Calculate Noise from BOTTOM LEFT Corner?", lowerLeftCorner);
        bottomRightBox = guiBuilder.buildCheckBox("Calculate Noise from BOTTOM RIGHT Corner?", lowerRightCorner);
        
        panel.add(smartNoiseField.getParent(), panelLayout);
        panel.add(topLeftBox.getParent(), panelLayout);
        panel.add(topRightBox.getParent(), panelLayout);
        panel.add(bottomLeftBox.getParent(), panelLayout);
        panel.add(bottomRightBox.getParent(), panelLayout);
    
        return panel;
    }
    
    protected JPanel buildNoThresholdPanel() {
        JPanel panel = new JPanel();
        return panel;
    }
    
    /**
     * This listener focuses on the user's choice of hard or smart thresholding.
     * A change will prompt a different panel to be displayed.
     *
     * @author senseneyj
     *
     */
    private class ThresholdChoiceListener implements ActionListener {
        private void varSet() {
            if(thresholdMethod.equals(Threshold.HARD)) {
                setHardThresholdUI(false);
            } else if(thresholdMethod.equals(Threshold.SMART)) {
                setSmartThresholdUI(false);
            } else {
                hardNoiseThreshold = 0.0f;
            }
            
            generalThresholdPanel.removeAll();
            JPanel genericPanel = null;
            
            if (hardCheckBox.isSelected()) {
                thresholdMethod = Threshold.HARD;
                genericPanel = buildHardThresholdPanel();
            } else if (smartCheckBox.isSelected()) {
                thresholdMethod = Threshold.SMART;
                genericPanel = buildSmartThresholdPanel();
            } else if(noCheckBox.isSelected()) {
                thresholdMethod = Threshold.NONE;
                genericPanel = buildNoThresholdPanel();
            }
            generalThresholdPanel.add(genericPanel);
            generalThresholdPanel.updateUI();
        }

        public void actionPerformed(ActionEvent e) {
            varSet();
            //System.out.println("Action: "+e.getActionCommand()+"\t "+e.getSource());
        }
    }
    
    protected String setHardThresholdUI(boolean process) {
        try {
            //hard threshold
            hardNoiseThreshold = Float.valueOf(hardNoiseField.getText()).floatValue();
            ViewUserInterface.getReference().getMessageFrame().append("hardNoiseThreshold: "+hardNoiseThreshold+"\n", ViewJFrameMessage.DEBUG);
        } catch(Exception e) {
            if(process) {
                return "Invalid hard threshold noise entered";
            }
        }
        return SUCCESS;
    }

    protected String setSmartThresholdUI(boolean process) {
        try {
            //smart noise threshold
            noiseScale = Float.valueOf(smartNoiseField.getText()).floatValue();
            upperLeftCorner = topLeftBox.isSelected();
            upperRightCorner = topRightBox.isSelected();
            lowerLeftCorner = bottomLeftBox.isSelected();
            lowerRightCorner = bottomRightBox.isSelected();
            
            ViewUserInterface.getReference().getMessageFrame().append("noiseScale: "+noiseScale+"\n", ViewJFrameMessage.DEBUG);
            ViewUserInterface.getReference().getMessageFrame().append("upperLeftCorner: "+upperLeftCorner+"\n", ViewJFrameMessage.DEBUG);
            ViewUserInterface.getReference().getMessageFrame().append("upperRightCorner: "+upperRightCorner+"\n", ViewJFrameMessage.DEBUG);
            ViewUserInterface.getReference().getMessageFrame().append("lowerLeftCorner: "+lowerLeftCorner+"\n", ViewJFrameMessage.DEBUG);
            ViewUserInterface.getReference().getMessageFrame().append("lowerRightCorner: "+lowerRightCorner+"\n", ViewJFrameMessage.DEBUG);
        } catch(Exception e) {
            if(process) {
                return "Invalid smart thresholding noise entered";
            }
        }
        return SUCCESS;
    }
    
}
