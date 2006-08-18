package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Simple dialog to change Magnification Box Settings.
 *
 * @version  1.0 Sep 15, 1999
 * @author   Harman Singh
 */

public class JDialogMagnificationControls extends JDialogBase
        implements ActionListener, ChangeListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2838490243455666340L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int box_width;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage;

    /** DOCUMENT ME! */
    private JLabel current;

    /** DOCUMENT ME! */
    private JCheckBox intensityCheckbox;

    /** DOCUMENT ME! */
    private JSlider magSlider;

    /** DOCUMENT ME! */
    private int max;

    /** DOCUMENT ME! */
    private int min;

    /** DOCUMENT ME! */
    private JTextField widthText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new magnification controls dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Image
     * @param  initZoom        Initial zoom
     * @param  title           Title
     */
    public JDialogMagnificationControls(Frame theParentFrame, ViewJComponentEditImage im, float initZoom,
                                        String title) {
        super(theParentFrame, false);

        parentFrame = theParentFrame;
        componentImage = im;

        max = 3200;
        min = 100;

        if ((componentImage.getZoomX() * 100) > max) {
            max = (int) (componentImage.getZoomX() * 100);
        }

        magSlider = new JSlider(JSlider.HORIZONTAL, min, max, (int) (componentImage.MAGR_MAG * 100));

        magSlider.setMajorTickSpacing((max - min) / 5);
        magSlider.setPaintTicks(true);
        magSlider.setEnabled(true);
        magSlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf((magSlider.getMaximum()) / 100.0f));
        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(magSlider.getValue() / 100.0f));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf(magSlider.getMinimum() / 100.0f));
        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        intensityCheckbox = new JCheckBox("Display intensity values", false);
        intensityCheckbox.setEnabled(componentImage.getShowMagIntensityEnabled());
        intensityCheckbox.setFont(serif12);
        intensityCheckbox.setForeground(Color.black);
        intensityCheckbox.addActionListener(this);

        JPanel sliderPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(magSlider, gbc);

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

        sliderPanel.add(intensityCheckbox, gbc);

        sliderPanel.setBorder(buildTitledBorder("Magnification"));

        JPanel dimPanel = new JPanel();
        dimPanel.setLayout(new GridBagLayout());
        dimPanel.setBorder(buildTitledBorder("Size"));

        // heightLabel  =  new JLabel("Height:");
        // heightLabel.setFont(serif);
        // heightLabel.setForeground(Color.black);

        JLabel widthLabel = new JLabel("Width:");
        widthLabel.setFont(serif12);
        widthLabel.setForeground(Color.black);

        // heightText   =  new JTextField();
        // heightText.setFont(serif12);
        // String str = Integer.toString((int)((parentFrame.getSize().height-
        // parentFrame.getInsets().top + 1)* 0.25));
        // heightText.setText(str);
        // heightText.setForeground(Color.black);

        widthText = new JTextField(5);
        widthText.setFont(serif12);

        String str = Integer.toString((int) (parentFrame.getSize().width * 0.25));
        widthText.setText(str);
        widthText.setForeground(Color.black);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridwidth = 1;
        dimPanel.add(widthLabel, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        dimPanel.add(widthText, gbc);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(sliderPanel);
        mainPanel.add(dimPanel, BorderLayout.SOUTH);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        OKButton = buildOKButton();
        OKButton.setText("Apply");
        buttonPanel.add(OKButton);
        cancelButton = buildCancelButton();
        cancelButton.setText("Close");
        buttonPanel.add(cancelButton);

        if (title != null) {
            setTitle(title);
        } else {
            setTitle(((ViewJFrameImage) parentFrame).getTitle());
        }

        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == intensityCheckbox) {

            if (intensityCheckbox.isSelected()) {
                componentImage.setShowMagIntensity(true);

            } else {
                componentImage.setShowMagIntensity(false);
            }
        } else if (source == OKButton) {

            // int xDim = (int)(parentFrame.getSize().width);
            int yDim = (int) (parentFrame.getSize().height - parentFrame.getInsets().top + 1);

            if (!testParameter(widthText.getText(), 64, (int) (yDim * 0.75))) {
                widthText.requestFocus();
                widthText.selectAll();

                return;
            }

            box_width = Integer.valueOf(widthText.getText()).intValue();
            componentImage.MAGR_HEIGHT = box_width;
            componentImage.MAGR_WIDTH = box_width;

            paint(getGraphics());
        } else if (source == cancelButton) {
            dispose();
        }
    }

    /**
     * Set the width & height of the Magnification window.
     *
     * @param  width  width of the magnification window
     */
    public void setWidthText(int width) {
        widthText.setText(Integer.toString(width));
    }

    /**
     * Sets values based on knob along slider.
     *
     * @param  e  event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == magSlider) {
            current.setText(String.valueOf(magSlider.getValue() / (float) 100));
            componentImage.MAGR_MAG = (int) ((magSlider.getValue() / (float) 100) + 0.5);
            magSlider.setValue((int) componentImage.MAGR_MAG * 100);

            if (componentImage.getShowMagIntensityEnabled()) {
                intensityCheckbox.setEnabled(true);
            } else {
                intensityCheckbox.setSelected(false);
                intensityCheckbox.setEnabled(false);
            }
        }
    }
}
