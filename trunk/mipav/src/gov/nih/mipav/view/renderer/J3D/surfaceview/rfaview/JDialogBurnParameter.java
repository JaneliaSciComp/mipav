package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogBurnParameter extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1568251410627012713L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public float diameter;

    /** DOCUMENT ME! */
    public boolean ok = false;

    /** DOCUMENT ME! */
    public float opacity;

    /** DOCUMENT ME! */
    public float radius;

    /** DOCUMENT ME! */
    public float time;

    /** DOCUMENT ME! */
    public float voltage;

    /** DOCUMENT ME! */
    public float volume;

    /** DOCUMENT ME! */
    private JTextField diameterField;

    /** DOCUMENT ME! */
    private JPanelProbe probePanel;

    /** DOCUMENT ME! */
    private JTextField timeField;

    /** DOCUMENT ME! */
    private JTextField voltageField;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogBurnParameter object.
     *
     * @param  _probePanel   DOCUMENT ME!
     * @param  _parentFrame  DOCUMENT ME!
     */
    public JDialogBurnParameter(JPanelProbe _probePanel, ViewJFrameBase _parentFrame) {
        super(_parentFrame, false);
        init();
        probePanel = _probePanel;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            setVariables();
            setVisible(false);
            probePanel.startBurn();
        } else if (command.equals("Cancel")) {
            setVisible(false);
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void setVariables() {
        String timeString = timeField.getText();
        String diameterSting = diameterField.getText();
        diameter = Float.valueOf(diameterSting).floatValue();
        time = Float.valueOf(timeString).floatValue();
        ok = true;
    }


    /**
     * DOCUMENT ME!
     */
    private void init() {

        setTitle("Parameter");
        setLocation(300, 400);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        JPanel optionPanel = new JPanel();

        // make border
        optionPanel.setBorder(buildTitledBorder("Burn Parameter"));
        contentBox.add(optionPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        optionPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;

        // diameter
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel diameterLabel = new JLabel("Burn diameter: ");
        diameterLabel.setFont(serif12);
        diameterLabel.setForeground(Color.black);

        gbc.gridwidth = 2;
        gbl.setConstraints(diameterLabel, gbc);
        optionPanel.add(diameterLabel);
        optionPanel.add(Box.createHorizontalStrut(10));

        diameterField = new JTextField(10);
        diameterField.setText("0.1");
        MipavUtil.makeNumericsOnly(diameterField, true, true);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(diameterField, gbc);
        optionPanel.add(diameterField);


        // time

        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel timeLabel = new JLabel(" Burn Time (s): ");
        timeLabel.setFont(serif12);
        timeLabel.setForeground(Color.black);
        timeLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(timeLabel, gbc);
        optionPanel.add(timeLabel);
        optionPanel.add(Box.createHorizontalStrut(10));

        timeField = new JTextField(10);
        timeField.setText("1");

        MipavUtil.makeNumericsOnly(timeField, true, true);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(timeField, gbc);
        optionPanel.add(timeField);

        // voltage
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel voltageLabel = new JLabel(" Burn Voltage (v): ");
        voltageLabel.setFont(serif12);
        voltageLabel.setForeground(Color.black);

        gbc.gridwidth = 2;
        gbl.setConstraints(voltageLabel, gbc);
        optionPanel.add(voltageLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        voltageField = new JTextField(10);
        MipavUtil.makeNumericsOnly(voltageField, true, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(voltageField, gbc);
        optionPanel.add(voltageField);
        voltageLabel.setEnabled(false);
        voltageField.setEnabled(false);


        JPanel buttonPanel = new JPanel();
        OKButton = buildOKButton();
        cancelButton = buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        contentBox.add(buttonPanel);
        getContentPane().add(contentBox);
        pack();
        setVisible(true);
    }

}
