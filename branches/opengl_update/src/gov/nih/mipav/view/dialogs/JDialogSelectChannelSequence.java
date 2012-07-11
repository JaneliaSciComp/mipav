package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * The purpose of this dialog is to allow the user to rearrange the channel order of an image before it is loaded. It is
 * meant to be used exclusively with the ViewOpenImageSequence class.
 */
public class JDialogSelectChannelSequence extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7969429539433435403L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] channelOrder;

    /** DOCUMENT ME! */
    private JComboBox[] comboBoxes;

    /** DOCUMENT ME! */
    private ViewOpenImageSequence parent;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogSelectChannelSequence object.
     *
     * @param  parent        ViewOpenImageSequence - the parent object for this dialog
     * @param  channelOrder  int[] - an array of ints indicating the channel order. Valid values range from 0 to 3, and
     *                       correspond to the ARGB interleave.
     */
    public JDialogSelectChannelSequence(ViewOpenImageSequence parent, int[] channelOrder) {
        super(true);

        this.parent = parent;
        this.channelOrder = channelOrder;

        setSize(200, 185);

        buildGUI();

        MipavUtil.centerOnScreen(this);

        setTitle("Channel order");

        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Invoked when an action occurs. If the user clicks "OK", ViewOpenImageSequence.newChannelMap is called on the
     * parent object. This effectively rearranges the channel ordering of the TIFF sequence in the parent object.
     *
     * @param  event  ActionEvent
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Cancel")) {
            dispose();
        }

        if (command.equals("OK")) {

            for (int i = 0; i < channelOrder.length; i++) {
                channelOrder[i] = comboBoxes[i].getSelectedIndex();
            }

            parent.newChannelMap(channelOrder);

            comboBoxes = null;
            dispose();
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void buildGUI() {
        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();

        JPanel mainPanel = new JPanel(gbLayout);

        comboBoxes = new JComboBox[4];

        for (int i = 0; i < 4; i++) {
            comboBoxes[i] = new JComboBox(new String[] { "alpha", "red", "green", "blue" });

            JLabel label = new JLabel("Channel " + (i + 1));

            gbConstraints.gridx = 0;
            gbConstraints.gridy = i;
            gbConstraints.ipadx = 2;
            gbConstraints.ipady = 0;
            gbLayout.setConstraints(label, gbConstraints);

            mainPanel.add(label);

            gbConstraints.gridx = 1;
            gbConstraints.ipady = 2;

            gbLayout.setConstraints(comboBoxes[i], gbConstraints);
            mainPanel.add(comboBoxes[i]);
            comboBoxes[i].setSelectedIndex(channelOrder[i]);
        }

        mainPanel.setBorder(BorderFactory.createEtchedBorder());

        JButton btnCancel = new JButton("Cancel");
        btnCancel.addActionListener(this);
        btnCancel.setActionCommand("Cancel");

        JButton btnOK = new JButton("OK");
        btnOK.addActionListener(this);
        btnOK.setActionCommand("OK");

        JPanel subPanel = new JPanel();
        subPanel.add(btnOK);
        subPanel.add(btnCancel);

        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(subPanel, BorderLayout.SOUTH);
    }
}
