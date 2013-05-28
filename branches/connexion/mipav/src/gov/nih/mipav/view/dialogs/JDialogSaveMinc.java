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

    /** DOCUMENT ME! */
    private float defaultTSpace;

    
    /** DOCUMENT ME! */
    private float defaultTStart;

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

    private JTextField tStart;
    
    private JTextField tSpace;

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
            	@SuppressWarnings("unused")
                float test = Float.valueOf(xStart.getText()).floatValue();
                test = Float.valueOf(yStart.getText()).floatValue();

                if (zStart.isEnabled()) {
                    test = Float.valueOf(zStart.getText()).floatValue();
                }

                if (tStart.isEnabled()) {
                    test = Float.valueOf(tStart.getText()).floatValue();
                }

                test = Float.valueOf(xSpace.getText()).floatValue();
                test = Float.valueOf(ySpace.getText()).floatValue();

                if (zSpace.isEnabled()) {
                    test = Float.valueOf(zSpace.getText()).floatValue();
                }

                if (tSpace.isEnabled()) {
                    test = Float.valueOf(tSpace.getText()).floatValue();
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
        } else {
            super.actionPerformed(event);
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

        if (tStart.isEnabled()) {
            options.setTStart(defaultTStart);
        }

        options.setXSpace(defaultXSpace);
        options.setYSpace(defaultYSpace);

        if (zSpace.isEnabled()) {
            options.setZSpace(defaultZSpace);
        }

        if (tSpace.isEnabled()) {
            options.setTSpace(defaultTSpace);
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
        createComboBoxX(layout, setGBC(gbc, 2, 1, GridBagConstraints.REMAINDER, 1), panel);

        createLabel("Y axis orientation:", layout, setGBC(gbc, 0, 2, 2, 1), panel);
        createComboBoxY(layout, setGBC(gbc, 2, 2, GridBagConstraints.REMAINDER, 1), panel);

        createLabel("Z axis orientation:", layout, setGBC(gbc, 0, 3, 2, 1), panel);
        createComboBoxZ(layout, setGBC(gbc, 2, 3, GridBagConstraints.REMAINDER, 1), panel);

        createLabel(" ", layout, setGBC(gbc, 0, 4, GridBagConstraints.REMAINDER, 1), panel);

        createLabel("L/R Start:", layout, setGBC(gbc, 0, 5, 1, 1), panel);
        xStart = setTextField("", layout, setGBC(gbc, 1, 5, 1, 1), panel);
        createLabel("  L/R Space:", layout, setGBC(gbc, 2, 5, 1, 1), panel);
        xSpace = setTextField("", layout, setGBC(gbc, 3, 5, 1, 1), panel);

        createLabel("P/A Start:", layout, setGBC(gbc, 0, 6, 1, 1), panel);
        yStart = setTextField("", layout, setGBC(gbc, 1, 6, 1, 1), panel);
        createLabel("  P/A Space:", layout, setGBC(gbc, 2, 6, 1, 1), panel);
        ySpace = setTextField("", layout, setGBC(gbc, 3, 6, 1, 1), panel);

        createLabel("I/S Start:", layout, setGBC(gbc, 0, 7, 1, 1), panel);
        zStart = setTextField("", layout, setGBC(gbc, 1, 7, 1, 1), panel);
        createLabel("  I/S Space:", layout, setGBC(gbc, 2, 7, 1, 1), panel);
        zSpace = setTextField("", layout, setGBC(gbc, 3, 7, 1, 1), panel);

        createLabel("Time Start:", layout, setGBC(gbc, 0, 8, 1, 1), panel);
        tStart = setTextField("", layout, setGBC(gbc, 1, 8, 1, 1), panel);
        createLabel("  Time Space:", layout, setGBC(gbc, 2, 8, 1, 1), panel);
        tSpace = setTextField("", layout, setGBC(gbc, 3, 8, 1, 1), panel);

        createLabel(" ", layout, setGBC(gbc, 0, 8, GridBagConstraints.REMAINDER, 1), panel);

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

        if (tStart.isEnabled()) {
            options.setTStart(Float.valueOf(tStart.getText()).floatValue());
        }

        options.setXSpace(Float.valueOf(xSpace.getText()).floatValue());
        options.setYSpace(Float.valueOf(ySpace.getText()).floatValue());

        if (zSpace.isEnabled()) {
            options.setZSpace(Float.valueOf(zSpace.getText()).floatValue());
        }

        if (tSpace.isEnabled()) {
            options.setTSpace(Float.valueOf(tSpace.getText()).floatValue());
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

        float lr = 0, pa = 0, is = 0, xRes = 1, yRes = 1, zRes = 1, tRes = 1;;

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
        
        if (orient == FileInfoBase.SAGITTAL){

            lr = -fileInfo.getOrigin(2);
            xRes = fileInfo.getResolutions()[2];
            
            pa = -fileInfo.getOrigin(0);
            yRes = fileInfo.getResolutions()[0];
            
            is =  fileInfo.getOrigin(1);
            zRes = fileInfo.getResolutions()[1];
        }
        else if (orient == FileInfoBase.AXIAL){

            lr = -fileInfo.getOrigin(0);
            xRes = fileInfo.getResolutions()[0];
            
            pa = -fileInfo.getOrigin(1);
            yRes = fileInfo.getResolutions()[1];
            
            is =  fileInfo.getOrigin(2);
            if (fileInfo.getResolutions().length >= 3) {
                zRes = fileInfo.getResolutions()[2];
            }
        }
        else if (orient == FileInfoBase.CORONAL){

            lr = -fileInfo.getOrigin(0);
            xRes = fileInfo.getResolutions()[0];
            
            pa = -fileInfo.getOrigin(2);
            yRes = fileInfo.getResolutions()[2];
            
            is = fileInfo.getOrigin(1);
            zRes = fileInfo.getResolutions()[1];
        }
        else {

            lr = fileInfo.getOrigin(0);
            xRes = fileInfo.getResolutions()[0];
            
            pa = fileInfo.getOrigin(1);
            yRes = fileInfo.getResolutions()[1];
            
            is =  fileInfo.getOrigin(2);
            zRes = fileInfo.getResolutions()[2];
            
        }
        
        if (lr < 0 && xRes < 0){
            xRes = -xRes;
        }
        else if (lr > 0 && xRes > 0){
            xRes = -xRes;
        }
        
        if (pa < 0 && yRes < 0){
            yRes = -yRes;
        }
        else if (pa > 0 && yRes > 0){
            yRes = -yRes;
        }
        
 
        if (is < 0 && zRes < 0){
            zRes = -zRes;
        }
        else if (is > 0 && zRes > 0){
            zRes = -zRes;
        }
        
        // since we write the spaces with the alignement = centre attribute, adjust the x and y space values by half the x and y steps
        lr += xRes / 2;
        pa += yRes / 2;

        if (fileInfo.getExtents().length == 2) {
            zStart.setEnabled(false);
            zSpace.setEnabled(false);
            tStart.setEnabled(false);
            tSpace.setEnabled(false);
        }

        if (fileInfo.getExtents().length == 3) {
            tStart.setEnabled(false);
            tSpace.setEnabled(false);
        }
        
        if (fileInfo.getExtents().length == 4) {
        	tRes = fileInfo.getResolutions()[3];
        }

        if (fileInfo.getFileFormat() == FileUtility.MINC) {
            Preferences.debug("Is MINC format\n");
        }

        xStart.setText("" + lr);
        yStart.setText("" + pa);

        if (zStart.isEnabled()) {
            zStart.setText("" + is);
        }

        if (tStart.isEnabled()) {
            tStart.setText("" + 0.0);
        }

        xSpace.setText("" + xRes);
        ySpace.setText("" + yRes);

        if (zSpace.isEnabled()) {
            zSpace.setText("" + zRes);
        }

        if (tSpace.isEnabled()) {
            tSpace.setText("" + tRes);
        }
        
        

        if (!defaultSet) {
            defaultXStart = lr;
            defaultYStart = pa;

            if (zStart.isEnabled()) {
                defaultZStart = is;
            }

            defaultXSpace = xRes;
            defaultYSpace = yRes;

            if (zSpace.isEnabled()) {
                defaultZSpace = zRes;
            }

            if (tSpace.isEnabled()) {
                defaultTSpace = tRes;
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
