package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Simple dialog to change trim parameters.
 *
 * @version  2.0 Jul 3, 2012
 * @author   Matthew J. McAuliffe
 * @author   Justin Senseney
 */

public class JDialogTrim extends JDialogBase implements ActionListener, ChangeListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2967441574461453380L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Labels for values of the voi trim points property */
    private JLabel voiCurrent, voiMaximum, voiMinimum;

    /** Checkbox for whether adjacent points are trimmed */
    private JCheckBox trimCheckbox;

    /** Sliders for setting the mask and voi trim parameters */
    private JSlider voiTrimSlider, maskTrimSlider;
    
    /** The currently selected image */
    private ModelImage activeImage;

    /** Labels for values of the mask trim points property */
    private JLabel maskMaximum, maskCurrent, maskMinimum;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new trim parameter dialog with slider.
     *
     * @param  theParentFrame  Parent frame
     */
    public JDialogTrim(Frame theParentFrame, ModelImage active) {
        super(theParentFrame, false);
        activeImage = active;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  Event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == trimCheckbox) {
            //only set trim when OK is pressed
            //Preferences.setProperty(Preferences.PREF_TRIM_FLAG, trimCheckbox.isSelected());  
        } else if (source == OKButton) {

            Preferences.setProperty(Preferences.PREF_TRIM_FLAG, trimCheckbox.isSelected()); 
            Preferences.setProperty(Preferences.PREF_TRIM_MASK, String.valueOf(maskTrimSlider.getValue() / (float) 100));
            Preferences.setProperty(Preferences.PREF_TRIM_VOI, String.valueOf(voiTrimSlider.getValue() / (float) 100));
            
            if ( activeImage != null ) {
                activeImage.trimVOIs();
            }
            dispose();
        } else if (source == cancelButton) {
            //Preferences.setProperty(Preferences.PREF_TRIM_VOI, String.valueOf(voiTrimSlider.getValue() / (float) 100));
            //only set trim when OK is pressed
            dispose();
        }
    }

    /**
     * Sets values based on knob along slider.
     *
     * @param  e  Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == voiTrimSlider) {
            voiCurrent.setText(String.valueOf(voiTrimSlider.getValue() / (float) 100));
        } else if(source == maskTrimSlider) {
            maskCurrent.setText(String.valueOf(maskTrimSlider.getValue() / (float) 100));
        }
    }

    /**
     * Makes slider to set opacity of VOI. Opaque = 1, Transparency = 0.
     */
    private void init() {

        setTitle("VOI and mask trim parameters");

        int initialVoiVal = 50, initialMaskVal = 0;

        if (Preferences.getProperty(Preferences.PREF_TRIM_VOI) != null) {
            initialVoiVal = (int) (Float.valueOf(Preferences.getProperty(Preferences.PREF_TRIM_VOI)).floatValue() * 100);
        }
        
        if (Preferences.getProperty(Preferences.PREF_TRIM_MASK) != null) {
            initialMaskVal = (int) (Float.valueOf(Preferences.getProperty(Preferences.PREF_TRIM_MASK)).floatValue() * 100);
        }

        //initialize voi gui elements
        voiTrimSlider = new JSlider(JSlider.HORIZONTAL, 0, 200, initialVoiVal);

        voiTrimSlider.setMajorTickSpacing(20);
        voiTrimSlider.setPaintTicks(true);
        voiTrimSlider.setEnabled(true);
        voiTrimSlider.addChangeListener(this);

        voiMaximum = new JLabel(String.valueOf((voiTrimSlider.getMaximum()) / 100.0f));
        voiMaximum.setForeground(Color.black);
        voiMaximum.setFont(serif12);

        voiCurrent = new JLabel(String.valueOf(voiTrimSlider.getValue() / 100.0f));
        voiCurrent.setForeground(Color.black);
        voiCurrent.setFont(serif12B);

        voiMinimum = new JLabel(String.valueOf(voiTrimSlider.getMinimum() / 100.0f));
        voiMinimum.setForeground(Color.black);
        voiMinimum.setFont(serif12);
        
        //initialize mask gui elements
        maskTrimSlider = new JSlider(JSlider.HORIZONTAL, 0, 200, initialMaskVal);

        maskTrimSlider.setMajorTickSpacing(20);
        maskTrimSlider.setPaintTicks(true);
        maskTrimSlider.setEnabled(true);
        maskTrimSlider.addChangeListener(this);

        maskMaximum = new JLabel(String.valueOf((maskTrimSlider.getMaximum()) / 100.0f));
        maskMaximum.setForeground(Color.black);
        maskMaximum.setFont(serif12);

        maskCurrent = new JLabel(String.valueOf(maskTrimSlider.getValue() / 100.0f));
        maskCurrent.setForeground(Color.black);
        maskCurrent.setFont(serif12B);

        maskMinimum = new JLabel(String.valueOf(maskTrimSlider.getMinimum() / 100.0f));
        maskMinimum.setForeground(Color.black);
        maskMinimum.setFont(serif12);

        boolean flag = true;

        if (Preferences.getProperty(Preferences.PREF_TRIM_FLAG) != null) {

            if (Preferences.getProperty(Preferences.PREF_TRIM_FLAG).equals("false")) {
                flag = false;
            }
        }

        trimCheckbox = new JCheckBox("Trim adjacent points.", flag);
        trimCheckbox.setEnabled(true);
        trimCheckbox.setFont(MipavUtil.font12);
        trimCheckbox.setForeground(Color.black);
        trimCheckbox.addActionListener(this);

        JPanel sliderPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        //set voi trim gui elements
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.insets = new Insets(5, 0, 3, 0);
        gbc.anchor = GridBagConstraints.WEST;
        
        sliderPanel.add(new JLabel("VOI trim parameter:"), gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(voiTrimSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(voiMinimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(voiCurrent, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(voiMaximum, gbc);
        
        //set mask trim gui elements
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.insets = new Insets(10, 0, 3, 0);
        gbc.anchor = GridBagConstraints.WEST;
        
        sliderPanel.add(new JLabel("Mask trim parameter:"), gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(maskTrimSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(maskMinimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(maskCurrent, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maskMaximum, gbc);

        //set trim adjacent points gui elements
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(trimCheckbox, gbc);

        sliderPanel.setBorder(buildTitledBorder("Trim parameters"));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        OKButton.setText("Apply");
        buildCancelButton();
        cancelButton.setText("Close");
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(sliderPanel);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainPanel);

        pack();
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setVisible(true);
    }

}
