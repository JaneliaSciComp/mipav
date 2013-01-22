package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Simple dialog for CT window presets.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileIO
 */
public class JDialogCTPreset extends JDialogBase implements ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3709628941183114805L;

    /** DOCUMENT ME! */
    public static final int IMAGE_A = 0;

    /** DOCUMENT ME! */
    public static final int IMAGE_B = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected Dimension dim = new Dimension(256, 256);

    /** Preset labels. */
    private JLabel abdomenLabel;

    /** Preset min, max values. Min presents the min window value. Max presents the max window value. */
    private int abdomenMin, abdomenMax;

    /**
     * Reference to the image data of the slice presently displayed. Needed to calculate the max/min of the slice used
     * to adjust the transfer function.
     */
    private float[] dataSlice;

    /** DOCUMENT ME! */
    private JLabel headLabel;

    /** DOCUMENT ME! */
    private int headMin, headMax;

    /** Reference to the image that will be affected by the adjust of the window and level. */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JLabel lungLabel;

    /** DOCUMENT ME! */
    private int lungMin, lungMax;

    /** Reference to the LUT used to display the image. */
    private ModelLUT LUT;

    /** Image's maximum intensity. */
    private float maxImage;

    /** DOCUMENT ME! */
    private JLabel mediastinumLabel;

    /** DOCUMENT ME! */
    private int mediastinumMin, mediastinumMax;

    /** Image's minimum intensity. */
    private float minImage;

    /** DOCUMENT ME! */
    private JLabel minLabel, maxLabel;

    /** Min, max text fields. */
    private JTextField minText, maxText;

    /** Preset radio buttons. */
    private JRadioButton radioAbdomen;

    /** DOCUMENT ME! */
    private JRadioButton radioHead;

    /** DOCUMENT ME! */
    private JRadioButton radioLung;

    /** DOCUMENT ME! */
    private JRadioButton radioMediastinum;

    /** DOCUMENT ME! */
    private JRadioButton radioSpine;

    /** DOCUMENT ME! */
    private JRadioButton radioVertebrae;

    /** Reset button to reset the default preset values. */
    private JButton resetButton;

    /** When user change the window, level values, setButton sets the min, max of the window values. */
    private JButton setButton;

    /** DOCUMENT ME! */
    private JLabel spineLabel;

    /** DOCUMENT ME! */
    private int spineMin, spineMax;

    /** DOCUMENT ME! */
    private JLabel vertebraeLabel;

    /** DOCUMENT ME! */
    private int vertebraeMin, vertebraeMax;

    /** Three arrays to save the coordinates of the LUT's transfer fucntion. z[] not used. */
    private float[] x = new float[4];

    /** DOCUMENT ME! */
    private float[] y = new float[4];

    /** DOCUMENT ME! */
    private float[] z = new float[4];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  image           DOCUMENT ME!
     * @param  LUT             DOCUMENT ME!
     */
    public JDialogCTPreset(Frame theParentFrame, ModelImage image, ModelLUT LUT) {
        super(theParentFrame, false);

        float min, max;
        int i;
        int levelTemp, windowTemp;

        this.image = image;
        this.LUT = LUT;

        setForeground(Color.black);
        cancelFlag = false;

        setTitle("CT Presets");

        calcMinMax();

        dataSlice = ((ViewJFrameImage) parentFrame).getComponentImage().getActiveImageBuffer();
        min = Float.MAX_VALUE;
        max = -Float.MAX_VALUE;

        for (i = 0; i < dataSlice.length; i++) {

            if (dataSlice[i] > max) {
                max = dataSlice[i];
            }

            if (dataSlice[i] < min) {
                min = dataSlice[i];
            }
        }

        // Set LUT min max values of the image slice !!
        x[0] = minImage;
        y[0] = 255;
        z[0] = 0;
        x[1] = min;
        y[1] = 255;
        z[1] = 0;
        x[2] = max;
        y[2] = 0;
        z[2] = 0;
        x[3] = maxImage;
        y[3] = 0;
        z[3] = 0;
        LUT.getTransferFunction().importArrays(x, y, 4);

        JPanel panelImageType = new JPanel();

        panelImageType.setLayout(new GridBagLayout());
        panelImageType.setForeground(Color.black);
        panelImageType.setBorder(buildTitledBorder("CT presets ( level, window)"));

        ButtonGroup group1 = new ButtonGroup();

        abdomenMin = -75;
        abdomenMax = 175;
        headMin = -25;
        headMax = 125;
        lungMin = -1550;
        lungMax = 450;
        mediastinumMin = -175;
        mediastinumMax = 275;
        spineMin = -110;
        spineMax = 190;
        vertebraeMin = -620;
        vertebraeMax = 1680;

        radioAbdomen = new JRadioButton("Abdomen ", false);
        radioAbdomen.setFont(serif12);
        radioAbdomen.addItemListener(this);
        group1.add(radioAbdomen);
        levelTemp = (abdomenMax + abdomenMin) / 2;
        windowTemp = abdomenMax - abdomenMin;
        abdomenLabel = new JLabel("(" + levelTemp + ", " + windowTemp + ")");
        abdomenLabel.setFont(serif12);

        radioHead = new JRadioButton("Head ", false);
        radioHead.setFont(serif12);
        radioHead.addItemListener(this);
        group1.add(radioHead);
        levelTemp = (headMax + headMin) / 2;
        windowTemp = headMax - headMin;
        headLabel = new JLabel("(" + levelTemp + ", " + windowTemp + ")");
        headLabel.setFont(serif12);

        radioLung = new JRadioButton("Lung ", false);
        radioLung.setFont(serif12);
        radioLung.addItemListener(this);
        group1.add(radioLung);
        levelTemp = (lungMax + lungMin) / 2;
        windowTemp = lungMax - lungMin;
        lungLabel = new JLabel("(" + levelTemp + ", " + windowTemp + ")");
        lungLabel.setFont(serif12);

        radioMediastinum = new JRadioButton("Mediastinum ", true);
        radioMediastinum.setFont(serif12);
        radioMediastinum.addItemListener(this);
        group1.add(radioMediastinum);
        levelTemp = (mediastinumMax + mediastinumMin) / 2;
        windowTemp = mediastinumMax - mediastinumMin;
        mediastinumLabel = new JLabel("(" + levelTemp + ", " + windowTemp + ")");
        mediastinumLabel.setFont(serif12);

        radioSpine = new JRadioButton("Spine ", false);
        radioSpine.setFont(serif12);
        radioSpine.addItemListener(this);
        group1.add(radioSpine);
        levelTemp = (spineMax + spineMin) / 2;
        windowTemp = spineMax - spineMin;
        spineLabel = new JLabel("(" + levelTemp + ", " + windowTemp + ")");
        spineLabel.setFont(serif12);

        radioVertebrae = new JRadioButton("Vertebrae ", false);
        radioVertebrae.setFont(serif12);
        radioVertebrae.addItemListener(this);
        group1.add(radioVertebrae);
        levelTemp = (vertebraeMax + vertebraeMin) / 2;
        windowTemp = vertebraeMax - vertebraeMin;
        vertebraeLabel = new JLabel("(" + levelTemp + ", " + windowTemp + ")");
        vertebraeLabel.setFont(serif12);

        GridBagConstraints gbc = new GridBagConstraints();

        // gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        panelImageType.add(radioAbdomen, gbc);
        gbc.gridx = 1;
        panelImageType.add(abdomenLabel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        panelImageType.add(radioHead, gbc);
        gbc.gridx = 1;
        panelImageType.add(headLabel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        panelImageType.add(radioLung, gbc);
        gbc.gridx = 1;
        panelImageType.add(lungLabel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        panelImageType.add(radioMediastinum, gbc);
        gbc.gridx = 1;
        panelImageType.add(mediastinumLabel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        panelImageType.add(radioSpine, gbc);
        gbc.gridx = 1;
        panelImageType.add(spineLabel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 5;
        panelImageType.add(radioVertebrae, gbc);
        gbc.gridx = 1;
        panelImageType.add(vertebraeLabel, gbc);

        JPanel editPanel = new JPanel();

        minLabel = new JLabel("Level ");
        minLabel.setFont(serif12);
        maxLabel = new JLabel("  Window ");
        maxLabel.setFont(serif12);

        levelTemp = (mediastinumMax + mediastinumMin) / 2;
        windowTemp = mediastinumMax - mediastinumMin;
        minText = new JTextField("" + levelTemp + "", 5);
        maxText = new JTextField("" + windowTemp + "", 5);
        editPanel.add(minLabel);
        editPanel.add(minText);
        editPanel.add(maxLabel);
        editPanel.add(maxText);

        JPanel buttonPanel = new JPanel();

        buildCloseButton();
        buildCancelButton();
        buttonPanel.add(closeButton);
        buttonPanel.add(cancelButton);

        JPanel resetPanel = new JPanel();

        buildSetButton();
        buildResetButton();
        resetPanel.add(setButton);
        resetPanel.add(resetButton);

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.add(panelImageType);
        contentBox.add(editPanel);
        contentBox.add(resetPanel);
        contentBox.add(buttonPanel);
        getContentPane().add(contentBox);

        pack();
        ctMode(mediastinumMin, mediastinumMax);

        image.notifyImageDisplayListeners(LUT, false);

        /*
         * if (image.getHistoLUTFrame() != null) { updateHistoLUTFrame(); } */
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {

        String command = event.getActionCommand();
        int levelTemp;
        int windowTemp;

        if (command.equals("Close")) {
            dispose();
        } else if (command.equals("Cancel")) {
            LUT.getTransferFunction().importArrays(x, y, 4);
            linearMode();
            dispose();

        } else if (command.equals("Set")) {

            if (radioAbdomen.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                abdomenLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                abdomenMin = levelTemp - (windowTemp / 2);
                abdomenMax = levelTemp + (windowTemp / 2);
                ctMode(abdomenMin, abdomenMax);
            } else if (radioHead.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                headLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                headMin = levelTemp - (windowTemp / 2);
                headMax = levelTemp + (windowTemp / 2);
                ctMode(headMin, headMax);
            } else if (radioLung.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                lungLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                lungMin = levelTemp - (windowTemp / 2);
                lungMax = levelTemp + (windowTemp / 2);
                ctMode(lungMin, lungMax);
            } else if (radioMediastinum.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                mediastinumLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                mediastinumMin = levelTemp - (windowTemp / 2);
                mediastinumMax = levelTemp + (windowTemp / 2);
                ctMode(mediastinumMin, mediastinumMax);
            } else if (radioSpine.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                spineLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                spineMin = levelTemp - (windowTemp / 2);
                spineMax = levelTemp + (windowTemp / 2);
                ctMode(spineMin, spineMax);
            } else if (radioVertebrae.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                vertebraeLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                vertebraeMin = levelTemp - (windowTemp / 2);
                vertebraeMax = levelTemp + (windowTemp / 2);
                ctMode(vertebraeMin, vertebraeMax);
            }
        } else if (command.equals("Reset")) {
            abdomenMin = -75;
            abdomenMax = 175;
            levelTemp = (abdomenMax + abdomenMin) / 2;
            windowTemp = abdomenMax - abdomenMin;
            abdomenLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioAbdomen.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ctMode(abdomenMin, abdomenMax);
            }

            headMin = -25;
            headMax = 125;
            levelTemp = (headMax + headMin) / 2;
            windowTemp = headMax - headMin;
            headLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioHead.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ctMode(headMin, headMax);
            }

            lungMin = -1550;
            lungMax = 450;
            levelTemp = (lungMax + lungMin) / 2;
            windowTemp = lungMax - lungMin;
            lungLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioLung.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ctMode(lungMin, lungMax);
            }

            mediastinumMin = -175;
            mediastinumMax = 275;
            levelTemp = (mediastinumMax + mediastinumMin) / 2;
            windowTemp = mediastinumMax - mediastinumMin;
            mediastinumLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioMediastinum.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ctMode(mediastinumMin, mediastinumMax);
            }

            spineMin = -110;
            spineMax = 190;
            levelTemp = (spineMax + spineMin) / 2;
            windowTemp = spineMax - spineMin;
            spineLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioSpine.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ctMode(spineMin, spineMax);
            }

            vertebraeMin = -620;
            vertebraeMax = 1680;
            levelTemp = (vertebraeMax + vertebraeMin) / 2;
            windowTemp = vertebraeMax - vertebraeMin;
            vertebraeLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioVertebrae.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ctMode(vertebraeMin, vertebraeMax);
            }
        } else {
            super.actionPerformed(event);
        }

    }

    /**
     * Sets mode to CT and sets range to CT presets.
     *
     * @param  preset1  first CT preset
     * @param  preset2  second CT preset
     */
    public void ctMode(int preset1, int preset2) {
        float min, max;
        float yVal, m, b;
        min = (float) image.getMin();
        max = (float) image.getMax();

        x[0] = min; // -1024;
        y[0] = dim.height - 1;
        z[0] = 0;

        if (preset2 < max) {
            x[2] = preset2;
        } else {
            x[2] = max;
        }

        y[2] = 0;
        z[2] = 0;

        if (preset1 < min) {

            // y = m * x + b, line equation
            // Assume given points: pt1 ( preset1, 255 ),  pt2 ( x[2], y[2])
            // find point: pt3 ( -1024, yVal);
            m = (255 - y[2]) / (preset1 - x[2]);
            b = 255 - (m * preset1);
            yVal = (m * (-1024)) + b;
            x[1] = -1024;
            y[1] = yVal;
            z[1] = 0;
            Preferences.debug("yVal = " + yVal);
        } else {
            x[1] = preset1;
            y[1] = dim.height - 1;
            z[1] = 0;
        }

        if (y[1] > 255) {
            y[1] = 255;
        }

        x[3] = max; // 3071;
        y[3] = 0;
        z[3] = 0;
        ((ViewJFrameImage) parentFrame).updateWinLevel((int) (x[1] + 0.5), (int) (x[2] + 0.5));
        LUT.getTransferFunction().importArrays(x, y, 4);
        image.notifyImageDisplayListeners(LUT, false);

        if (image.getHistoLUTFrame() != null) {
            updateHistoLUTFrame();
        }
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();

        str += abdomenMin + delim;
        str += abdomenMax + delim;
        str += headMin + delim;
        str += headMax + delim;
        str += lungMin + delim;
        str += lungMax + delim;
        str += mediastinumMin + delim;
        str += mediastinumMax + delim;
        str += spineMin + delim;
        str += spineMax + delim;
        str += vertebraeMin + delim;
        str += vertebraeMax;

        return str;
    }

    /**
     * Sets the flags for the checkboxes.
     *
     * @param  event  Event that triggered this function.
     */
    public synchronized void itemStateChanged(ItemEvent event) {
        int levelTemp, windowTemp;

        if (radioAbdomen.isSelected()) {
            ctMode(abdomenMin, abdomenMax);
            levelTemp = (abdomenMax + abdomenMin) / 2;
            windowTemp = abdomenMax - abdomenMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");
        } else if (radioHead.isSelected()) {
            ctMode(headMin, headMax);
            levelTemp = (headMax + headMin) / 2;
            windowTemp = headMax - headMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");
        } else if (radioLung.isSelected()) {
            ctMode(lungMin, lungMax);
            levelTemp = (lungMax + lungMin) / 2;
            windowTemp = lungMax - lungMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");

        } else if (radioMediastinum.isSelected()) {
            ctMode(mediastinumMin, mediastinumMax);
            levelTemp = (mediastinumMax + mediastinumMin) / 2;
            windowTemp = mediastinumMax - mediastinumMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");
        } else if (radioSpine.isSelected()) {
            ctMode(spineMin, spineMax);
            levelTemp = (spineMax + spineMin) / 2;
            windowTemp = spineMax - spineMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");

        } else if (radioVertebrae.isSelected()) {
            ctMode(vertebraeMin, vertebraeMax);
            levelTemp = (vertebraeMax + vertebraeMin) / 2;
            windowTemp = vertebraeMax - vertebraeMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");
        }

    }

    /**
     * Sets mode to linear and shows component.
     */
    public void linearMode() {
        float min, max;

        if (image.getType() == ModelStorageBase.UBYTE) {
            min = 0;
            max = 255;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            min = -128;
            max = 127;
        } else {
            min = (float) image.getMin();
            max = (float) image.getMax();
        }

        x[0] = min;
        y[0] = dim.height - 1;
        z[0] = 0;

        x[1] = (min + ((max - min) / 3.0f));
        y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
        z[1] = 0;

        x[2] = (min + ((max - min) * 2.0f / 3.0f));
        y[2] = (dim.height - 1) - (((dim.height - 1) * 2.0f) / 3.0f);
        z[2] = 0;

        x[3] = max; // - (max-min)/255.0f;
        y[3] = 0;
        z[3] = 0;

        ((ViewJFrameImage) parentFrame).updateWinLevel((int) (x[1] + 0.5), (int) (x[2] + 0.5));
        LUT.getTransferFunction().importArrays(x, y, 4);
        image.notifyImageDisplayListeners(LUT, false);

        if (image.getHistoLUTFrame() != null) {
            updateHistoLUTFrame();
        }

    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void legacyLoadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                abdomenMin = MipavUtil.getInt(st);
                abdomenMax = MipavUtil.getInt(st);
                headMin = MipavUtil.getInt(st);
                headMax = MipavUtil.getInt(st);
                lungMin = MipavUtil.getInt(st);
                lungMax = MipavUtil.getInt(st);
                mediastinumMin = MipavUtil.getInt(st);
                mediastinumMax = MipavUtil.getInt(st);
                spineMin = MipavUtil.getInt(st);
                spineMax = MipavUtil.getInt(st);
                vertebraeMin = MipavUtil.getInt(st);
                vertebraeMax = MipavUtil.getInt(st);

            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void legacySaveDefaults() {
        String defaultsString = new String(getParameterString(","));

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Builds the reset button.
     */
    protected void buildResetButton() {
        resetButton = new JButton("Reset");
        resetButton.addActionListener(this);
        resetButton.setMinimumSize(MipavUtil.defaultButtonSize);
        resetButton.setPreferredSize(MipavUtil.defaultButtonSize);
        resetButton.setFont(serif12B);
    }

    /**
     * Builds the set button.
     */
    protected void buildSetButton() {
        setButton = new JButton("Set");
        setButton.addActionListener(this);
        setButton.setMinimumSize(MipavUtil.defaultButtonSize);
        setButton.setPreferredSize(MipavUtil.defaultButtonSize);
        setButton.setFont(serif12B);
    }

    /**
     * Calculate the maximum and minimum valuse to setup the window and level sliders.
     */
    private void calcMinMax() {

        if (image.getType() == ModelStorageBase.UBYTE) {
            minImage = 0;
            maxImage = 255;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            minImage = -128;
            maxImage = 127;
        } else {
            minImage = (float) image.getMin();
            maxImage = (float) image.getMax();
        }
    }

    /**
     * Displays histoLUT frame for a gray scale image.
     */
    private void updateHistoLUTFrame() {

        image.notifyImageDisplayListeners(LUT, false);
        image.getHistoLUTFrame().update();

    }

}
