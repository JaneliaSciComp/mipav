package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Simple dialog to tell the program important information about the MINC file.
 *
 * @version  1.0 Aug 1, 2000
 * @author   Neva Cherniavsky
 * @see      FileMinc
 */
public class JDialogSaveMinc extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5951116204923790986L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboX;

    /** DOCUMENT ME! */
    private JComboBox comboY;

    /** DOCUMENT ME! */
    private JComboBox comboZ;

    /** DOCUMENT ME! */
    private int[] defaultAxisOrient;

    /** DOCUMENT ME! */
    private boolean defaultSet = false;

    /** DOCUMENT ME! */
    private float defaultXSpace;

    /**
     * Variables for holding default values. Then if the default values are used (as in, the user hit "OK" without
     * changing any values), that option would be set in "options". This affects the way the script records.
     */
    private float defaultXStart;

    /** DOCUMENT ME! */
    private float defaultYSpace;

    /** DOCUMENT ME! */
    private float defaultYStart;

    /** DOCUMENT ME! */
    private float defaultZSpace;

    /** DOCUMENT ME! */
    private float defaultZStart;

    /** Variables for holding and storing file information. */
    private FileInfoBase fileInfo;

    /** DOCUMENT ME! */
    private FileWriteOptions options;

    /** Axes orientations. */
    private int[] ori;

    /** DOCUMENT ME! */
    private int orient;

    /**
     * Variables for the actual GUI; not all need to be global, but just as easy. The text fields and radio buttons do
     * need to be global.
     */
    private JLabel orientLabel;

    /** DOCUMENT ME! */
    private JTextField xSpace;

    /** DOCUMENT ME! */
    private JTextField xStart;

    /** DOCUMENT ME! */
    private JTextField ySpace;

    /** DOCUMENT ME! */
    private JTextField yStart;

    /** DOCUMENT ME! */
    private JTextField zSpace;

    /** DOCUMENT ME! */
    private JTextField zStart;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new dialog to set necessary information for saving a MINC file.
     *
     * @param  theParentFrame  Parent frame.
     * @param  _fileInfo       File info object to get initialization info from.
     * @param  options         Storage place for the info recorded in this dialog.
     */
    public JDialogSaveMinc(Frame theParentFrame, FileInfoBase _fileInfo, FileWriteOptions options) {
        super(theParentFrame, true);

        fileInfo = _fileInfo;
        this.options = options;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets the information.
     *
     * @param  event  event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {

        if (event.getActionCommand().equals("OK")) {
            cancelFlag = false;

            try {
                float test = Float.valueOf(xStart.getText()).floatValue();
                test = Float.valueOf(yStart.getText()).floatValue();

                if (zStart.isEnabled()) {
                    test = Float.valueOf(zStart.getText()).floatValue();
                }

                test = Float.valueOf(xSpace.getText()).floatValue();
                test = Float.valueOf(ySpace.getText()).floatValue();

                if (zSpace.isEnabled()) {
                    test = Float.valueOf(zSpace.getText()).floatValue();
                }

                setOptions();
                dispose();
            } catch (NumberFormatException error) {
                MipavUtil.displayError("The values in X Space, Y Space, and Z Space must be numbers.");
            }
        } else if (event.getActionCommand().equals("AxisChanged")) {
            ori[0] = comboX.getSelectedIndex();
            ori[1] = comboY.getSelectedIndex();
            ori[2] = comboZ.getSelectedIndex();
            setSpace();
        } else if (event.getActionCommand().equals("Cancel")) {
            cancelFlag = true;
            dispose();
        }
    }

    /**
     * Accessor that returns the write options set up in this dialog.
     *
     * @return  The write options.
     */
    public FileWriteOptions getOptions() {
        return options;
    }

    /**
     * Sets up the options to the default values as determined by the file info in setSpace and returns the new options.
     *
     * @return  The default options.
     */
    public FileWriteOptions setOptionsDefault() {
        options.setXStart(defaultXStart);
        options.setYStart(defaultYStart);

        if (zStart.isEnabled()) {
            options.setZStart(defaultZStart);
        }

        options.setXSpace(defaultXSpace);
        options.setYSpace(defaultYSpace);

        if (zSpace.isEnabled()) {
            options.setZSpace(defaultZSpace);
        }

        options.setAxisOrientation(defaultAxisOrient);

        return options;
    }

    /**
     * Creates a combo box and adds it to the panel.
     *
     * @param  layout  The layout to add the constraints to.
     * @param  gbc     The constraints for this combo box.
     * @param  panel   The panel to add the combo box to.
     */
    private void createComboBoxX(GridBagLayout layout, GridBagConstraints gbc, JPanel panel) {
        Object[] vector = {
            "Unknown", "Right to left", "Left to right", "Posterior to anterior", "Anterior to posterior",
            "Inferior to superior", "Superior to inferior"
        };
        comboX = new JComboBox(vector);
        comboX.setFont(serif12);
        comboX.setForeground(Color.black);
        comboX.setBackground(Color.white);
        layout.setConstraints(comboX, gbc);

        switch (ori[0]) {

            case FileInfoBase.ORI_UNKNOWN_TYPE:
                comboX.setSelectedItem("Unknown");
                break;

            case FileInfoBase.ORI_R2L_TYPE:
                comboX.setSelectedItem("Right to left");
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                comboX.setSelectedItem("Left to right");
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                comboX.setSelectedItem("Posterior to anterior");
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                comboX.setSelectedItem("Anterior to posterior");
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                comboX.setSelectedItem("Inferior to superior");
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                comboX.setSelectedItem("Superior to inferior");
                break;

            default:
                comboX.setSelectedItem("Unknown");
        }

        comboX.setActionCommand("AxisChanged");
        comboX.addActionListener(this);
        panel.add(comboX);
    }

    /**
     * Creates a combo box and adds it to the panel.
     *
     * @param  layout  The layout to add the constraints to.
     * @param  gbc     The constraints for this combo box.
     * @param  panel   The panel to add the combo box to.
     */
    private void createComboBoxY(GridBagLayout layout, GridBagConstraints gbc, JPanel panel) {
        Object[] vector = {
            "Unknown", "Right to left", "Left to right", "Posterior to anterior", "Anterior to posterior",
            "Inferior to superior", "Superior to inferior"
        };
        comboY = new JComboBox(vector);
        comboY.setFont(serif12);
        comboY.setForeground(Color.black);
        comboY.setBackground(Color.white);
        layout.setConstraints(comboY, gbc);

        switch (ori[1]) {

            case FileInfoBase.ORI_UNKNOWN_TYPE:
                comboY.setSelectedItem("Unknown");
                break;

            case FileInfoBase.ORI_R2L_TYPE:
                comboY.setSelectedItem("Right to left");
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                comboY.setSelectedItem("Left to right");
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                comboY.setSelectedItem("Posterior to anterior");
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                comboY.setSelectedItem("Anterior to posterior");
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                comboY.setSelectedItem("Inferior to superior");
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                comboY.setSelectedItem("Superior to inferior");
                break;

            default:
                comboY.setSelectedItem("Unknown");
        }

        comboY.setActionCommand("AxisChanged");
        comboY.addActionListener(this);
        panel.add(comboY);
    }

    /**
     * Creates a combo box and adds it to the panel.
     *
     * @param  layout  The layout to add the constraints to.
     * @param  gbc     The constraints for this combo box.
     * @param  panel   The panel to add the combo box to.
     */
    private void createComboBoxZ(GridBagLayout layout, GridBagConstraints gbc, JPanel panel) {
        Object[] vector = {
            "Unknown", "Right to left", "Left to right", "Posterior to anterior", "Anterior to posterior",
            "Inferior to superior", "Superior to inferior"
        };
        comboZ = new JComboBox(vector);
        comboZ.setFont(serif12);
        comboZ.setForeground(Color.black);
        comboZ.setBackground(Color.white);
        layout.setConstraints(comboZ, gbc);

        switch (ori[2]) {

            case FileInfoBase.ORI_UNKNOWN_TYPE:
                comboZ.setSelectedItem("Unknown");
                break;

            case FileInfoBase.ORI_R2L_TYPE:
                comboZ.setSelectedItem("Right to left");
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                comboZ.setSelectedItem("Left to right");
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                comboZ.setSelectedItem("Posterior to anterior");
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                comboZ.setSelectedItem("Anterior to posterior");
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                comboZ.setSelectedItem("Inferior to superior");
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                comboZ.setSelectedItem("Superior to inferior");
                break;

            default:
                comboZ.setSelectedItem("Unknown");
        }

        comboZ.setActionCommand("AxisChanged");
        comboZ.addActionListener(this);
        panel.add(comboZ);
    }

    /**
     * Makes a label and adds it to the panel.
     *
     * @param   title   The value of the label.
     * @param   layout  The layout to add the constraints to.
     * @param   gbc     The constraints for this label.
     * @param   panel   The panel to add the label to.
     *
     * @return  DOCUMENT ME!
     */
    private JLabel createLabel(String title, GridBagLayout layout, GridBagConstraints gbc, JPanel panel) {
        JLabel label = new JLabel(title);
        label.setFont(serif12);
        label.setForeground(Color.black);
        layout.setConstraints(label, gbc);
        panel.add(label);

        return label;
    }

    /**
     * Initializes the GUI components and puts them in the dialog, attaching necessary actions.
     */
    private void init() {
        ori = fileInfo.getAxisOrientation();
        //ori[1] = FileInfoMinc.oppositeOrient(ori[1]);
        setTitle("Attributes to save");
        setResizable(false);
        cancelFlag = false;

        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.insets = new Insets(1, 1, 1, 1);

        JPanel panel = new JPanel();
        panel.setLayout(layout);
        panel.setForeground(Color.black);
        panel.setBorder(buildTitledBorder("MINC Attributes to Save"));

        switch (ori[2]) {

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                orient = FileInfoBase.AXIAL;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                orient = FileInfoBase.CORONAL;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                orient = FileInfoBase.SAGITTAL;
                break;

            default:
                orient = FileInfoBase.AXIAL;
        }

        createLabel("Image orientation:", layout, setGBC(gbc, 0, 0, 2, 1), panel);

        if (orient == FileInfoBase.AXIAL) {
            orientLabel = createLabel("AXIAL", layout, setGBC(gbc, 2, 0, 1, 1), panel);
        } else if (orient == FileInfoBase.CORONAL) {
            orientLabel = createLabel("CORONAL", layout, setGBC(gbc, 2, 0, 1, 1), panel);
        } else {
            orientLabel = createLabel("SAGITTAL", layout, setGBC(gbc, 2, 0, 1, 1), panel);
        }

        createLabel("X axis orientation:", layout, setGBC(gbc, 0, 1, 2, 1), panel);
        createComboBoxX(layout, setGBC(gbc, 2, 1, gbc.REMAINDER, 1), panel);

        createLabel("Y axis orientation:", layout, setGBC(gbc, 0, 2, 2, 1), panel);
        createComboBoxY(layout, setGBC(gbc, 2, 2, gbc.REMAINDER, 1), panel);

        createLabel("Z axis orientation:", layout, setGBC(gbc, 0, 3, 2, 1), panel);
        createComboBoxZ(layout, setGBC(gbc, 2, 3, gbc.REMAINDER, 1), panel);

        createLabel(" ", layout, setGBC(gbc, 0, 4, gbc.REMAINDER, 1), panel);

        createLabel("X Start:", layout, setGBC(gbc, 0, 5, 1, 1), panel);
        xStart = setTextField("", layout, setGBC(gbc, 1, 5, 1, 1), panel);
        createLabel("  X Space:", layout, setGBC(gbc, 2, 5, 1, 1), panel);
        xSpace = setTextField("", layout, setGBC(gbc, 3, 5, 1, 1), panel);

        createLabel("Y Start:", layout, setGBC(gbc, 0, 6, 1, 1), panel);
        yStart = setTextField("", layout, setGBC(gbc, 1, 6, 1, 1), panel);
        createLabel("  Y Space:", layout, setGBC(gbc, 2, 6, 1, 1), panel);
        ySpace = setTextField("", layout, setGBC(gbc, 3, 6, 1, 1), panel);

        createLabel("Z Start:", layout, setGBC(gbc, 0, 7, 1, 1), panel);
        zStart = setTextField("", layout, setGBC(gbc, 1, 7, 1, 1), panel);
        createLabel("  Z Space:", layout, setGBC(gbc, 2, 7, 1, 1), panel);
        zSpace = setTextField("", layout, setGBC(gbc, 3, 7, 1, 1), panel);

        createLabel(" ", layout, setGBC(gbc, 0, 8, gbc.REMAINDER, 1), panel);

        JLabel l = createLabel("In the current image:", layout, setGBC(gbc, 0, 9, gbc.REMAINDER, 1), panel);
        l.setFont(serif12B);

        JLabel l2 = createLabel(" The (0,0) pixel is in the lower left hand corner of the image for MINC files. ",
                                layout, setGBC(gbc, 0, 10, gbc.REMAINDER, 1), panel);
        l2.setFont(MipavUtil.font12I);

        JLabel l3 = createLabel(" Therefore, the positive directions are left to right, bottom to top,", layout,
                                setGBC(gbc, 0, 11, gbc.REMAINDER, 1), panel);
        l3.setFont(MipavUtil.font12I);

        JLabel l4 = createLabel(" and lower slice number to higher slice number.", layout,
                                setGBC(gbc, 0, 12, gbc.REMAINDER, 1), panel);
        l4.setFont(MipavUtil.font12I);

        createLabel(" ", layout, setGBC(gbc, 0, 13, gbc.REMAINDER, 1), panel);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        setSpace();
        getContentPane().add(panel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
    }

    /**
     * A helper method for adding a component using GridBagLayout, so we don't have to set up the x, y, width, and
     * height over and over again.
     *
     * @param   gbc  The constraints to set.
     * @param   x    gridx
     * @param   y    gridy
     * @param   w    gridwidth
     * @param   h    gridheight
     *
     * @return  The new grid bag constraints.
     *
     * @see     GridBagConstraints
     */
    private GridBagConstraints setGBC(GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;

        return gbc;
    }

    /**
     * Sets up the options depending on what the user entered in the dialog.
     */
    private void setOptions() {
        options.setXStart(Float.valueOf(xStart.getText()).floatValue());
        options.setYStart(Float.valueOf(yStart.getText()).floatValue());

        if (zStart.isEnabled()) {
            options.setZStart(Float.valueOf(zStart.getText()).floatValue());
        }

        options.setXSpace(Float.valueOf(xSpace.getText()).floatValue());
        options.setYSpace(Float.valueOf(ySpace.getText()).floatValue());

        if (zSpace.isEnabled()) {
            options.setZSpace(Float.valueOf(zSpace.getText()).floatValue());
        }

        options.setAxisOrientation(ori);

        if (zStart.isEnabled()) {

            if (!((options.getXStart() == defaultXStart) && (options.getYStart() == defaultYStart) &&
                      (options.getZStart() == defaultZStart) && (options.getXSpace() == defaultXSpace) &&
                      (options.getYSpace() == defaultYSpace) && (options.getZSpace() == defaultZSpace) &&
                      (options.getAxisOrientation()[0] == defaultAxisOrient[0]) &&
                      (options.getAxisOrientation()[1] == defaultAxisOrient[1]) &&
                      (options.getAxisOrientation()[1] == defaultAxisOrient[2]))) {
                options.setDefault(false);
            }
        } else {

            if (!((options.getXStart() == defaultXStart) && (options.getYStart() == defaultYStart) &&
                      (options.getXSpace() == defaultXSpace) && (options.getYSpace() == defaultYSpace) &&
                      (options.getAxisOrientation()[0] == defaultAxisOrient[0]) &&
                      (options.getAxisOrientation()[1] == defaultAxisOrient[1]) &&
                      (options.getAxisOrientation()[1] == defaultAxisOrient[2]))) {
                options.setDefault(false);
            }

        }
    }


    /**
     * Initializes the text fields for the dialog. MORE!
     */
    private void setSpace() {
        int i;
        float x = 0, y = 0, z = 0, xRes = 1, yRes = 1, zRes = 1;
        float[] start = new float[3];

        switch (ori[2]) {

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                orient = FileInfoBase.AXIAL;
                orientLabel.setText("AXIAL");
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                orient = FileInfoBase.CORONAL;
                orientLabel.setText("CORONAL");
                break;

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                orient = FileInfoBase.SAGITTAL;
                orientLabel.setText("SAGITTAL");
                break;

            default:
                orient = FileInfoBase.AXIAL;
                orientLabel.setText("AXIAL");
        }

        for (i = 0; i < 3; i++) {

            if ((ori[i] != FileInfoBase.ORI_I2S_TYPE) && (ori[i] != FileInfoBase.ORI_S2I_TYPE)) {
                start[i] = -fileInfo.getOrigin(i);
            } else {
                start[i] = fileInfo.getOrigin(i);
            }
        }

        if ((ori[1] == FileInfoBase.ORI_R2L_TYPE) || (ori[1] == FileInfoBase.ORI_A2P_TYPE) ||
                (ori[1] == FileInfoBase.ORI_S2I_TYPE)) {
            start[1] = start[1] + (fileInfo.getResolutions()[1] * (fileInfo.getExtents()[1] - 1));
        } else {
            start[1] = start[1] - (fileInfo.getResolutions()[1] * (fileInfo.getExtents()[1] - 1));
        }

        for (i = 0; i < ori.length; i++) {

            switch (ori[i]) {

                case FileInfoBase.ORI_R2L_TYPE:
                    x = start[i];
                    xRes = -fileInfo.getResolutions()[i];
                    break;

                case FileInfoBase.ORI_L2R_TYPE:
                    x = start[i];
                    xRes = fileInfo.getResolutions()[i];
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    y = start[i];
                    yRes = fileInfo.getResolutions()[i];
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                    y = start[i];
                    yRes = -fileInfo.getResolutions()[i];
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                    z = start[i];
                    zRes = fileInfo.getResolutions()[i];
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    z = start[i];
                    zRes = -fileInfo.getResolutions()[i];
                    break;
            }

        }

        if (fileInfo.getExtents().length == 2) {
            zStart.setEnabled(false);
            zSpace.setEnabled(false);
        }

        if (fileInfo.getFileFormat() == FileBase.MINC) {
            Preferences.debug("Is MINC format\n");
        }

        if ((fileInfo.getFileFormat() == FileBase.MINC) &&
                !((FileInfoMinc) fileInfo).resetStartLocationsOrientations()) {
            float[] steps = ((FileInfoMinc) fileInfo).getStepMinc();
            xRes = steps[0];
            yRes = steps[1];

            if (steps.length > 2) {
                zRes = steps[2];
            }

            Preferences.debug("xRes = " + xRes + " yRes = " + yRes + " zRes = " + zRes + "\n");

            float[] values = ((FileInfoMinc) fileInfo).getStartMinc();
            x = values[0];
            y = values[1];

            if (values.length > 2) {
                z = values[2] + (zRes * options.getBeginSlice());
            }

            Preferences.debug("x = " + x + " y = " + y + "\n");
            Preferences.debug("values[2] = " + values[2] + "\n");
            Preferences.debug("options.getBeginSlice() = " + options.getBeginSlice() + "\n");
        } else {

            switch (ori[2]) {

                case FileInfoBase.ORI_R2L_TYPE:
                case FileInfoBase.ORI_L2R_TYPE:
                    x += xRes * options.getBeginSlice();
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                case FileInfoBase.ORI_A2P_TYPE:
                    y += yRes * options.getBeginSlice();
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                case FileInfoBase.ORI_S2I_TYPE:
                    z += zRes * options.getBeginSlice();
                    break;
            }
        }

        xStart.setText("" + x);
        yStart.setText("" + y);

        if (zStart.isEnabled()) {
            zStart.setText("" + z);
        }

        xSpace.setText("" + xRes);
        ySpace.setText("" + yRes);

        if (zSpace.isEnabled()) {
            zSpace.setText("" + zRes);
        }

        if (!defaultSet) {
            defaultXStart = x;
            defaultYStart = y;

            if (zStart.isEnabled()) {
                defaultZStart = z;
            }

            defaultXSpace = xRes;
            defaultYSpace = yRes;

            if (zSpace.isEnabled()) {
                defaultZSpace = zRes;
            }

            defaultAxisOrient = ori;
            defaultSet = true;
        }
    }

    /**
     * Makes a text field and adds it to the panel.
     *
     * @param   initial  The initial string in the text field.
     * @param   layout   The layout to add the constraints to.
     * @param   gbc      The constraints for this text field.
     * @param   panel    The panel to add the text field to.
     *
     * @return  The text field created.
     */
    private JTextField setTextField(String initial, GridBagLayout layout, GridBagConstraints gbc, JPanel panel) {
        JTextField field = new JTextField(initial, 5);
        layout.setConstraints(field, gbc);
        panel.add(field);

        return field;
    }

}
