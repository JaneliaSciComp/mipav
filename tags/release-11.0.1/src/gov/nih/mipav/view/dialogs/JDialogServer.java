package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Simple dialog to set certain values for the new or edited server or destination.
 *
 * @version  1.0 July 1, 1999
 * @author   Neva Cherniavsky
 * @see      ViewJFrameDICOMQuery
 */
public class JDialogServer extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1348641833737787663L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String alias;

    /** DOCUMENT ME! */
    private JTextField aliasField;

    /** DOCUMENT ME! */
    private JLabel aliasLabel;

    /** DOCUMENT ME! */
    private String ip;

    /** DOCUMENT ME! */
    private JTextField ipField;

    /** DOCUMENT ME! */
    private JLabel ipLabel;

    /** DOCUMENT ME! */
    private String port;

    /** DOCUMENT ME! */
    private JTextField portField;

    /** DOCUMENT ME! */
    private JLabel portLabel;

    /** DOCUMENT ME! */
    private boolean server;

    /** DOCUMENT ME! */
    private String title;

    /** DOCUMENT ME! */
    private JTextField titleField;

    /** DOCUMENT ME! */
    private JLabel titleLabel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for setting server.
     *
     * @param  theParentFrame  parent frame
     * @param  title           title of dialog frame
     * @param  server          flag indicating if this is server or storage
     */
    public JDialogServer(JFrame theParentFrame, String title, boolean server) {

        super(theParentFrame, true);

        setTitle(title);

        buildContentPane(server);
        this.server = server;

        setBounds(theParentFrame.getBounds().width / 2, theParentFrame.getBounds().height / 2, 300, 200);
        setVisible(true);
    }

    /**
     * Creates new dialog for setting server.
     *
     * @param  theParentFrame  parent frame
     * @param  title           title of dialog frame
     * @param  values          values of the text fields
     * @param  server          flag indicating if this is server or storage
     */
    public JDialogServer(JFrame theParentFrame, String title, String[] values, boolean server) {

        super(theParentFrame, true);
        setTitle(title);
        buildContentPane(server);
        this.server = server;
        setValues(values);
        setBounds(theParentFrame.getBounds().width / 2, theParentFrame.getBounds().height / 2, 300, 200);
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * If the user hits the "OK" button, checks to make sure he or she entered valid data. Then sets the values to the
     * ones that the user entered. If the user hits the "Cancel" button, disposes of dialog.
     *
     * @param  e  Event that triggered this method.
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("OK")) {

            if (titleField.getText().equals("") || aliasField.getText().equals("") || ipField.getText().equals("") ||
                    portField.getText().equals("")) {

                MipavUtil.displayError("You must enter values for every field.");
            } else {

                try {

                    if ((Integer.valueOf(portField.getText()).intValue() < 1) ||
                            (Integer.valueOf(portField.getText()).intValue() > 65535)) {

                        MipavUtil.displayError("The port field must be a value between 1 and 65535.");

                        return;
                    }
                } catch (NumberFormatException error) {
                    MipavUtil.displayError("The port field must be a value between 1 and 65535.");

                    return;
                }

                if (server) {
                    char[] ipChars = ipField.getText().toCharArray();
                    int index = 0;
                    int count = 0;
                    String s;

                    for (int i = 0; i < ipChars.length; i++) {

                        if (!Character.isDigit(ipChars[i]) && (ipChars[i] != '.')) {
                            MipavUtil.displayError("The IP address field must contain a valid IP address.");

                            return;
                        }

                        if (ipChars[i] == '.') {
                            count++;
                            s = ipField.getText().substring(index, i);
                            index = i + 1;

                            if ((Integer.valueOf(s).intValue() < 0) || (Integer.valueOf(s).intValue() > 999)) {
                                MipavUtil.displayError("The IP address field must contain a valid IP address.");

                                return;
                            }
                        }

                    }

                    s = ipField.getText().substring(index);

                    if ((Integer.valueOf(s).intValue() < 0) || (Integer.valueOf(s).intValue() > 999) || (count != 3)) {
                        MipavUtil.displayError("The IP address field must contain a valid IP address.");

                        return;
                    }
                }

                title = titleField.getText();
                alias = aliasField.getText();
                ip = ipField.getText();
                port = portField.getText();
                dispose();
            }
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        } else {
            super.actionPerformed(e);
        }
    }

    /**
     * Returns the values entered into the text fields.
     *
     * @return  An array of the values.
     */
    public String[] getValues() {
        String[] values = { title, alias, ip, port };

        return values;

    }

    /**
     * Builds the content pane for the dialog, making the text fields and labels.
     *
     * @param  server  Flag indicating if this is server or storage
     */
    private void buildContentPane(boolean server) {
        GridBagConstraints gbc;
        Insets rightInsets, leftInsets;

        try {
            getContentPane().setLayout(new GridBagLayout());
            gbc = new GridBagConstraints();
            rightInsets = new Insets(0, 0, 10, 10);
            leftInsets = new Insets(0, 10, 10, 0);
            titleField = new JTextField();
            aliasField = new JTextField();
            ipField = new JTextField();
            portField = new JTextField();
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: JDialogServer.buildContentPane");

            return;
        }

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridheight = 1;
        gbc.gridwidth = 2;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.insets = leftInsets;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        titleLabel = buildLabel("AE Title");
        getContentPane().add(titleLabel, gbc);

        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.gridwidth = 4;
        gbc.insets = rightInsets;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        getContentPane().add(titleField, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.insets = leftInsets;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        aliasLabel = buildLabel("Alias");
        getContentPane().add(aliasLabel, gbc);

        gbc.gridx = 2;
        gbc.gridy = 1;
        gbc.gridwidth = 4;
        gbc.insets = rightInsets;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        getContentPane().add(aliasField, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 2;
        gbc.insets = leftInsets;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        if (server) {
            ipLabel = buildLabel("IP Address");
        } else {
            ipLabel = buildLabel("Directory");
        }

        getContentPane().add(ipLabel, gbc);

        gbc.gridx = 2;
        gbc.gridy = 2;
        gbc.gridwidth = 4;
        gbc.insets = rightInsets;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        getContentPane().add(ipField, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 2;
        gbc.insets = leftInsets;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        portLabel = buildLabel("Port");
        getContentPane().add(portLabel, gbc);

        gbc.gridx = 2;
        gbc.gridy = 3;
        gbc.gridwidth = 4;
        gbc.insets = rightInsets;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        getContentPane().add(portField, gbc);

        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.NONE;

        buildOKButton();
        getContentPane().add(OKButton, gbc);

        gbc.gridx = 3;
        gbc.gridy = 4;
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.NONE;
        buildCancelButton();
        getContentPane().add(cancelButton, gbc);

        cancelFlag = false;


    }

    /**
     * Build the label for the textField, try to standardize the label appearance in the GUI.
     *
     * @param   s  The name of the label
     *
     * @return  The constructed label.
     */
    private JLabel buildLabel(String s) {
        JLabel label;

        try {
            label = new JLabel(s);
            label.setFont(MipavUtil.font12);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: JDialogServer.buildContentPane");

            return null;
        }

        label.setForeground(Color.black);
        label.setHorizontalTextPosition(SwingConstants.LEFT);

        return label;
    }

    /**
     * Sets the current values in the text fields to the values in the array.
     *
     * @param  values  The array of current values
     */
    private void setValues(String[] values) {
        titleField.setText(values[0]);
        aliasField.setText(values[1]);
        ipField.setText(values[2]);
        portField.setText(values[3]);
    }
}
