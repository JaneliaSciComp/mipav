package gov.nih.mipav.view.dialogs;


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
public class JDialogCT extends JDialogBase implements ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8061222665925432857L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Preset labels. */
    private JLabel abdomenLabel;

    /** Preset min, max values. Min presents the min window value. Max presents the max window value. */
    private int abdomenMin, abdomenMax;

    /** DOCUMENT ME! */
    private JLabel headLabel;

    /** DOCUMENT ME! */
    private int headMin, headMax;

    /** DOCUMENT ME! */
    private JLabel lungLabel;

    /** DOCUMENT ME! */
    private int lungMin, lungMax;

    /** DOCUMENT ME! */
    private JLabel mediastinumLabel;

    /** DOCUMENT ME! */
    private int mediastinumMin, mediastinumMax;

    /** DOCUMENT ME! */
    private JLabel minLabel, maxLabel;

    /** Min, max text fields. */
    private JTextField minText, maxText;

    /** Single Transfer Line size. */
    private int origNPts;

    /** x, y, z arrays of the LUT transfer function. */
    private float[] origX, origY;

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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     */
    public JDialogCT(Frame theParentFrame) {
        super(theParentFrame, false);

        int levelTemp, windowTemp;

        origNPts = ((ViewJFrameHistoLUT) (parentFrame)).getTransferLine().size();
        origX = new float[origNPts];
        origY = new float[origNPts];
        ((ViewJFrameHistoLUT) (parentFrame)).getTransferLine().exportArrays(origX, origY);

        setForeground(Color.black);
        cancelFlag = false;

        setTitle("CT Presets");

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

        loadDefaults();

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
        ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(mediastinumMin, mediastinumMax);
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
        int levelTemp, windowTemp;
        String command = event.getActionCommand();

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }


        if (command.equals("Close")) {
            dispose();
        } else if (command.equals("Cancel")) {
            ((ViewJFrameHistoLUT) (parentFrame)).getTransferLine().importArrays(origX, origY, origNPts);
            ((ViewJFrameHistoLUT) (parentFrame)).setLinearLUT();
            ((ViewJFrameHistoLUT) (parentFrame)).setLUT(((ViewJFrameHistoLUT) (parentFrame)).getLUT());
            ((ViewJFrameHistoLUT) (parentFrame)).updateFrames(false);
            dispose();

        } else if (command.equals("Set")) {

            if (radioAbdomen.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                abdomenLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                abdomenMin = levelTemp - (windowTemp / 2);
                abdomenMax = levelTemp + (windowTemp / 2);
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(abdomenMin, abdomenMax);
            } else if (radioHead.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                headLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                headMin = levelTemp - (windowTemp / 2);
                headMax = levelTemp + (windowTemp / 2);
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(headMin, headMax);
            } else if (radioLung.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                lungLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                lungMin = levelTemp - (windowTemp / 2);
                lungMax = levelTemp + (windowTemp / 2);
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(lungMin, lungMax);
            } else if (radioMediastinum.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                mediastinumLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                mediastinumMin = levelTemp - (windowTemp / 2);
                mediastinumMax = levelTemp + (windowTemp / 2);
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(mediastinumMin, mediastinumMax);
            } else if (radioSpine.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                spineLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                spineMin = levelTemp - (windowTemp / 2);
                spineMax = levelTemp + (windowTemp / 2);
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(spineMin, spineMax);
            } else if (radioVertebrae.isSelected()) {
                levelTemp = Integer.valueOf(minText.getText()).intValue();
                windowTemp = Integer.valueOf(maxText.getText()).intValue();
                vertebraeLabel.setText("(" + levelTemp + "," + windowTemp + ")");
                vertebraeMin = levelTemp - (windowTemp / 2);
                vertebraeMax = levelTemp + (windowTemp / 2);
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(vertebraeMin, vertebraeMax);
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
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(abdomenMin, abdomenMax);
            }

            headMin = -25;
            headMax = 125;
            levelTemp = (headMax + headMin) / 2;
            windowTemp = headMax - headMin;
            headLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioHead.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(headMin, headMax);
            }

            lungMin = -1550;
            lungMax = 450;
            levelTemp = (lungMax + lungMin) / 2;
            windowTemp = lungMax - lungMin;
            lungLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioLung.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(lungMin, lungMax);
            }

            mediastinumMin = -175;
            mediastinumMax = 275;
            levelTemp = (mediastinumMax + mediastinumMin) / 2;
            windowTemp = mediastinumMax - mediastinumMin;
            mediastinumLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioMediastinum.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(mediastinumMin, mediastinumMax);
            }

            spineMin = -110;
            spineMax = 190;
            levelTemp = (spineMax + spineMin) / 2;
            windowTemp = spineMax - spineMin;
            spineLabel.setText("(" + levelTemp + "," + windowTemp + ")");

            if (radioSpine.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(spineMin, spineMax);
            }

            vertebraeMin = -620;
            vertebraeMax = 1680;
            levelTemp = (vertebraeMax + vertebraeMin) / 2;
            windowTemp = vertebraeMax - vertebraeMin;
            vertebraeLabel.setText("(" + vertebraeMin + "," + vertebraeMax + ")");

            if (radioVertebrae.isSelected()) {
                minText.setText("" + levelTemp + "");
                maxText.setText("" + windowTemp + "");
                ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(levelTemp, windowTemp);
            }

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

            // ((ViewJFrameHistoLUT)(parentFrame)).setCTMode(-75, 175);
            ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(abdomenMin, abdomenMax);
            levelTemp = (abdomenMax + abdomenMin) / 2;
            windowTemp = abdomenMax - abdomenMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");
        } else if (radioHead.isSelected()) {

            // ((ViewJFrameHistoLUT)(parentFrame)).setCTMode(-25, 125);
            ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(headMin, headMax);
            levelTemp = (headMax + headMin) / 2;
            windowTemp = headMax - headMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");
        } else if (radioLung.isSelected()) {

            // ((ViewJFrameHistoLUT)(parentFrame)).setCTMode(-1550, 450);
            ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(lungMin, lungMax);
            levelTemp = (lungMax + lungMin) / 2;
            windowTemp = lungMax - lungMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");

        } else if (radioMediastinum.isSelected()) {

            // ((ViewJFrameHistoLUT)(parentFrame)).setCTMode(-175, 275);
            ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(mediastinumMin, mediastinumMax);
            levelTemp = (mediastinumMax + mediastinumMin) / 2;
            windowTemp = mediastinumMax - mediastinumMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");

        } else if (radioSpine.isSelected()) {

            // ((ViewJFrameHistoLUT)(parentFrame)).setCTMode(-110, 190);
            ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(spineMin, spineMax);
            levelTemp = (spineMax + spineMin) / 2;
            windowTemp = spineMax - spineMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");


        } else if (radioVertebrae.isSelected()) {

            // ((ViewJFrameHistoLUT)(parentFrame)).setCTMode(-620, 1680);
            ((ViewJFrameHistoLUT) (parentFrame)).setCTMode(vertebraeMin, vertebraeMax);
            levelTemp = (vertebraeMax + vertebraeMin) / 2;
            windowTemp = vertebraeMax - vertebraeMin;
            minText.setText("" + levelTemp + "");
            maxText.setText("" + windowTemp + "");

        }

    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {

                // System.err.println(defaultsString);
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
                System.out.println("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(","));

        // System.err.println(defaultsString);
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Builds the Edit button. Sets it internally as well return the just-built button.
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


}
