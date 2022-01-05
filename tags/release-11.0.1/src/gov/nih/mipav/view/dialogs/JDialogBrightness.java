package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.text.NumberFormat;

import javax.swing.*;
import javax.swing.event.*;


/**
 * This class creates a simple dialog in which the user can specify a brightness integer and a contrast float using
 * sliders. Called from ViewJFrameAnimate.
 * 
 * @version April 24, 2002
 * @author Neva Cherniavsky
 */
public class JDialogBrightness extends JDialogBase implements ChangeListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1222614304368736757L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int brightness;

    /** DOCUMENT ME! */
    private JSlider brightnessSlider, contrastSlider;

    /** DOCUMENT ME! */
    private ViewJComponentAnimate componentImage = null;

    /** DOCUMENT ME! */
    private ViewJComponentColocalizationEM componentImageColEM = null;

    /** DOCUMENT ME! */
    private ViewJComponentColocalizationRegression componentImageColReg = null;

    /** DOCUMENT ME! */
    private float contrast;

    /** DOCUMENT ME! */
    private JLabel current, current2;

    /** DOCUMENT ME! */
    private NumberFormat nfc;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for entering brightness and contrast.
     * 
     * @param parent Parent frame.
     * @param compImage Component image to change.
     * @param bright Initial brightness.
     * @param con Initial contrast.
     */
    public JDialogBrightness(JFrame parent, ViewJComponentAnimate compImage, int bright, float con) {
        super(parent, false);
        componentImage = compImage;
        brightness = bright;
        contrast = con;
        init(brightness, contrast);
    }

    /**
     * Creates new dialog for entering brightness and contrast.
     * 
     * @param parent Parent frame.
     * @param compImage Component image to change.
     * @param bright Initial brightness.
     * @param con Initial contrast.
     */
    public JDialogBrightness(JFrame parent, ViewJComponentColocalizationRegression compImage, int bright, float con) {
        super(parent, false);
        componentImageColReg = compImage;
        brightness = bright;
        contrast = con;
        init(brightness, contrast);
    }

    /**
     * Creates new dialog for entering brightness and contrast.
     * 
     * @param parent Parent frame.
     * @param compImage Component image to change.
     * @param bright Initial brightness.
     * @param con Initial contrast.
     */
    public JDialogBrightness(JFrame parent, ViewJComponentColocalizationEM compImage, int bright, float con) {
        super(parent, false);
        componentImageColEM = compImage;
        brightness = bright;
        contrast = con;
        init(brightness, contrast);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     * 
     * @param evt DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent evt) {
        String command = evt.getActionCommand();

        if (command.equals("Cancel")) {
            dispose();

            if (componentImage != null) {
                ((ViewJFrameAnimate) parentFrame).cancelBrightness();
            }

            if (componentImageColReg != null) {
                ((ViewJFrameColocalizationRegression) parentFrame).cancelBrightness();
            }

            if (componentImageColEM != null) {
                ((ViewJFrameColocalizationEM) parentFrame).cancelBrightness();
            }
        } else if (command.equals("Apply")) {
            dispose();

            if (componentImage != null) {
                ((ViewJFrameAnimate) parentFrame).setBrightness(brightness, contrast);
            }

            if (componentImageColReg != null) {
                ((ViewJFrameColocalizationRegression) parentFrame).setBrightness(brightness, contrast);
            }

            if (componentImageColEM != null) {
                ((ViewJFrameColocalizationEM) parentFrame).setBrightness(brightness, contrast);
            }
        } else {
            super.actionPerformed(evt);
        }
    }

    /**
     * Sets values based on knob along slider.
     * 
     * @param e Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == brightnessSlider) {
            brightness = brightnessSlider.getValue();
            current.setText(String.valueOf(brightness));

            // Change only the brightness and contrast of the current slice
            if (componentImage != null) {
                componentImage.setSliceBrightness(brightness, contrast);
            }

            if (componentImageColReg != null) {
                componentImageColReg.setBrightness(brightness, contrast);
            }

            if (componentImageColEM != null) {
                componentImageColEM.setBrightness(brightness, contrast);
            }
        } else if (source == contrastSlider) {
            contrast = (float) Math.pow(10.0, contrastSlider.getValue() / 200.0);
            current2.setText(String.valueOf(nfc.format(contrast)));

            // Change only the brightness and contrast of the current slice
            if (componentImage != null) {
                componentImage.setSliceBrightness(brightness, contrast);
            }

            if (componentImageColReg != null) {
                componentImageColReg.setBrightness(brightness, contrast);
            }

            if (componentImageColEM != null) {
                componentImageColEM.setBrightness(brightness, contrast);
            }
        }
    }

    /**
     * Initializes GUI components and displays dialog.
     * 
     * <p>
     * For the brightnessSlider the slider values and the brightness values are identical. brightness is an offset going
     * from -255 to 255. This is enough to change all 0 values to 255 and all 255 values to 0. brightness is added to
     * all contrast scaled red, green, and blue.
     * </p>
     * 
     * <p>
     * However, for the contrastSlider the slider values are different from the contrast values. The contrast values go
     * from 0.1 to 10.0 while the slider values go from -200 to 200. contrast =
     * (float)Math.pow(10.0,contrastSlider.getValue()/200.0) The original red, green, and blue are mutliplied by
     * contrast.
     * </p>
     * 
     * @param brightness Initial brightness.
     * @param contrast Initial contrast.
     */
    private void init(int brightness, float contrast) {
        setTitle("Brightness/Contrast");
        brightnessSlider = new JSlider(JSlider.HORIZONTAL, -255, 255, brightness);

        brightnessSlider.setMajorTickSpacing(102);
        brightnessSlider.setPaintTicks(true);
        brightnessSlider.setEnabled(true);
        brightnessSlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf(255));
        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(brightness));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf( -255));
        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        JPanel sliderPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(brightnessSlider, gbc);

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
        sliderPanel.setBorder(buildTitledBorder("Brightness"));

        contrastSlider = new JSlider(JSlider.HORIZONTAL, -200, 200,
                (int) (Math.round(86.85889638 * Math.log(contrast))));

        contrastSlider.setMajorTickSpacing(80);
        contrastSlider.setPaintTicks(true);
        contrastSlider.setEnabled(true);
        contrastSlider.addChangeListener(this);

        JLabel maximum2 = new JLabel(String.valueOf(10));
        maximum2.setForeground(Color.black);
        maximum2.setFont(serif12);

        nfc = NumberFormat.getNumberInstance();
        nfc.setMaximumFractionDigits(3);

        current2 = new JLabel(String.valueOf(nfc.format(contrast)));
        current2.setForeground(Color.black);
        current2.setFont(serif12B);

        JLabel minimum2 = new JLabel(String.valueOf(0.100));
        minimum2.setForeground(Color.black);
        minimum2.setFont(serif12);

        JPanel sliderPanel2 = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel2.add(contrastSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel2.add(minimum2, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel2.add(current2, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel2.add(maximum2, gbc);
        sliderPanel2.setBorder(buildTitledBorder("Contrast"));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buildCancelButton();
        OKButton.setText("Apply");
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel centerPanel = new JPanel(new GridLayout(2, 1));
        centerPanel.add(sliderPanel);
        centerPanel.add(sliderPanel2);

        mainDialogPanel.add(centerPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainDialogPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

}
