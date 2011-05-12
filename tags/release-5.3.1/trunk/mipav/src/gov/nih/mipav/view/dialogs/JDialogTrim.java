package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Simple dialog to change Magnification Box Settings.
 *
 * @version  1.0 Sep 15, 1999
 * @author   Matthew J. McAuliffe
 */

public class JDialogTrim extends JDialogBase implements ActionListener, ChangeListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2967441574461453380L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JLabel current;

    /** DOCUMENT ME! */
    private JLabel maximum;

    /** DOCUMENT ME! */
    private JLabel minimum;

    /** DOCUMENT ME! */
    private JCheckBox trimCheckbox;


    /** DOCUMENT ME! */
    private JSlider trimSlider;
    
    private ModelImage activeImage;

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

            if (trimCheckbox.isSelected()) {

                // Enable trim slider and set preferences to slider value
                Preferences.setProperty(Preferences.PREF_TRIM_FLAG, "true");
            } else {

                // Disable trim slider
                Preferences.setProperty(Preferences.PREF_TRIM_FLAG, "false");
            }
        } else if (source == OKButton) {

            if (trimCheckbox.isSelected()) {
                Preferences.setProperty(Preferences.PREF_TRIM_FLAG, "true");
            } else {
                Preferences.setProperty(Preferences.PREF_TRIM_FLAG, "false");
            }

            Preferences.setProperty(Preferences.PREF_TRIM, String.valueOf(trimSlider.getValue() / (float) 100));
            
            if ( activeImage != null )
            {
                activeImage.trimVOIs();
            }
            dispose();
        } else if (source == cancelButton) {
            Preferences.setProperty(Preferences.PREF_TRIM, String.valueOf(trimSlider.getValue() / (float) 100));
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

        if (source == trimSlider) {
            current.setText(String.valueOf(trimSlider.getValue() / (float) 100));
        }
    }

    /**
     * Makes slider to set opacity of VOI. Opaque = 1, Transparency = 0.
     */
    private void init() {

        setTitle("VOI trim parameter");

        int initialVal = 50;

        if (Preferences.getProperty(Preferences.PREF_TRIM) != null) {
            initialVal = (int) (Float.valueOf(Preferences.getProperty(Preferences.PREF_TRIM)).floatValue() * 100);
        }

        trimSlider = new JSlider(JSlider.HORIZONTAL, 0, 200, initialVal);

        trimSlider.setMajorTickSpacing(20);
        trimSlider.setPaintTicks(true);
        trimSlider.setEnabled(true);
        trimSlider.addChangeListener(this);

        maximum = new JLabel(String.valueOf((trimSlider.getMaximum()) / 100.0f));
        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(trimSlider.getValue() / 100.0f));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        minimum = new JLabel(String.valueOf(trimSlider.getMinimum() / 100.0f));
        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

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

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(trimSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(minimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(current, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(trimCheckbox, gbc);

        sliderPanel.setBorder(buildTitledBorder("Trim parameter"));

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
