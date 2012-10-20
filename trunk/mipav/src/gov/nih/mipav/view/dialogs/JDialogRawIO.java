package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Simple dialog to indicate type, dimensionality, of a raw image or image with a fixed length header in front of the
 * image data. Checks the Preferences file, and will default to the most recently used JDialogRawIO values. The dialog
 * will save the values chosen when [OK] is struck.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileIO
 * @version  0.2 Jun 20, 2001
 * @author   David Parsons
 */
public class JDialogRawIO extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1588091095156376702L;

    /** DOCUMENT ME! */
    private static final int DEFAULT_IMAGE_TYPE = ModelStorageBase.SHORT;

    /** DOCUMENT ME! */
    private static final boolean DEFAULT_BIG_ENDIAN_BYTE_ORDER = true;

    /** DOCUMENT ME! */
    private static final int DEFAULT_DATA_OFFSET = 0;

    /** DOCUMENT ME! */
    private static final String DEFAULT_EXTENTS = "256,256,2,0,0";

    /** DOCUMENT ME! */
    private static final String DEFAULT_RES = "1.0,1.0,0,0,0";

    /** DOCUMENT ME! */
    private static final String DEFAULT_UNIT_INDEX = "7,7,7,7";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox checkboxEnd;

    /** DOCUMENT ME! */
    private JComboBox comboBoxUnitOfMeasure1;

    /** DOCUMENT ME! */
    private JComboBox comboBoxUnitOfMeasure2;

    /** DOCUMENT ME! */
    private JComboBox comboBoxUnitOfMeasure3;

    /** DOCUMENT ME! */
    private JComboBox comboBoxUnitOfMeasure4;

    /** DOCUMENT ME! */
    private JComboBox comboBoxUnitOfMeasure5;

    /** DOCUMENT ME! */
    private int dataType;

    /** DOCUMENT ME! */
    private int[] dimExtents;

    /** DOCUMENT ME! */
    private boolean endianess = FileBase.BIG_ENDIAN;

    /** DOCUMENT ME! */
    private int[] extents = new int[5];

    /** DOCUMENT ME! */
    private JLabel labelOffset;

    /** DOCUMENT ME! */
    private int offset;

    /** DOCUMENT ME! */
    private JRadioButton radioARGB;

    /** DOCUMENT ME! */
    private JRadioButton radioARGB_USHORT;

    /** DOCUMENT ME! */
    private JRadioButton radioBool; // image data type

    /** DOCUMENT ME! */
    private JRadioButton radioByte;

    /** DOCUMENT ME! */
    private JRadioButton radioDouble;

    /** DOCUMENT ME! */
    private JRadioButton radioFloat;

    /** DOCUMENT ME! */
    private JRadioButton radioInt;

    /** DOCUMENT ME! */
    private JRadioButton radioLong;

    /** DOCUMENT ME! */
    private JRadioButton radioShort;

    /** DOCUMENT ME! */
    private JRadioButton radioUByte;

    /** DOCUMENT ME! */
    private JRadioButton radioUInt;

    /** DOCUMENT ME! */
    private JRadioButton radioUShort;


    /** DOCUMENT ME! */
    private float[] resolutions = new float[5];

    /** DOCUMENT ME! */
    private JTextField textDim1; // dimensions

    /** DOCUMENT ME! */
    private JTextField textDim2;

    /** DOCUMENT ME! */
    private JTextField textDim3;

    /** DOCUMENT ME! */
    private JTextField textDim4;

    /** DOCUMENT ME! */
    private JTextField textDim5;

    /** DOCUMENT ME! */
    private JTextField textOffset;

    /** DOCUMENT ME! */
    private JTextField textRes1; // resolutions

    /** DOCUMENT ME! */
    private JTextField textRes2;

    /** DOCUMENT ME! */
    private JTextField textRes3;

    /** DOCUMENT ME! */
    private JTextField textRes4;

    /** DOCUMENT ME! */
    private JTextField textRes5;


    /** DOCUMENT ME! */
    private int[] unitsOfMeasure;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for entering necessary info to read in RAW image.
     *
     * @param  theParentFrame  Parent frame
     * @param  title           Title of dialog frame
     */
    public JDialogRawIO(Frame theParentFrame, String title) {
        super(theParentFrame, true);
        setTitle(title);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets the variables.
     *
     * @param  event  Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String tmpStr;
        int i, j;

        if (source == OKButton) {

            if (radioBool.isSelected()) {
                dataType = ModelStorageBase.BOOLEAN;
            } else if (radioByte.isSelected()) {
                dataType = ModelStorageBase.BYTE;
            } else if (radioUByte.isSelected()) {
                dataType = ModelStorageBase.UBYTE;
            } else if (radioShort.isSelected()) {
                dataType = ModelStorageBase.SHORT;
            } else if (radioUShort.isSelected()) {
                dataType = ModelStorageBase.USHORT;
            } else if (radioInt.isSelected()) {
                dataType = ModelStorageBase.INTEGER;
            } else if (radioUInt.isSelected()) {
                dataType = ModelStorageBase.UINTEGER;
            } else if (radioLong.isSelected()) {
                dataType = ModelStorageBase.LONG;
            } else if (radioFloat.isSelected()) {
                dataType = ModelStorageBase.FLOAT;
            } else if (radioDouble.isSelected()) {
                dataType = ModelStorageBase.DOUBLE;
            } else if (radioARGB.isSelected()) {
                dataType = ModelStorageBase.ARGB;
            } else if (radioARGB_USHORT.isSelected()) {
                dataType = ModelStorageBase.ARGB_USHORT;
            } else {
                dataType = ModelStorageBase.BYTE;
            }

            // button = (JRadioButton)group2.getSelection();
            // if (radioMM.isSelected())   unitOfMeasure = Unit.MILLIMETERS.getLegacyNum();
            // else                        unitOfMeasure = Unit.INCHES.getLegacyNum();

            for (int k = 0; k < unitsOfMeasure.length; k++) {
                unitsOfMeasure[k] = -1;
            }


            if (checkboxEnd.isSelected() == true) {
                endianess = FileBase.BIG_ENDIAN;
            } else {
                endianess = FileBase.LITTLE_ENDIAN;
            }

            tmpStr = textOffset.getText();

            if (testParameter(tmpStr, 0, 2000000)) {
                offset = Double.valueOf(tmpStr).intValue();
            } else {
                textOffset.requestFocus();
                textOffset.selectAll();

                return;
            }

            tmpStr = textDim1.getText();

            if (testParameter(tmpStr, 0, 5000)) {
                extents[0] = Double.valueOf(tmpStr).intValue();
            } else {
                textDim1.requestFocus();
                textDim1.selectAll();

                return;
            }

            tmpStr = textDim2.getText();

            if (testParameter(tmpStr, 0, 5000)) {
                extents[1] = Double.valueOf(tmpStr).intValue();
            } else {
                textDim2.requestFocus();
                textDim2.selectAll();

                return;
            }

            tmpStr = textDim3.getText();

            if (testParameter(tmpStr, 0, 5000)) {
                extents[2] = Double.valueOf(tmpStr).intValue();
            } else {
                textDim3.requestFocus();
                textDim3.selectAll();

                return;
            }

            tmpStr = textDim4.getText();

            if (testParameter(tmpStr, 0, 5000)) {
                extents[3] = Double.valueOf(tmpStr).intValue();
            } else {
                textDim4.requestFocus();
                textDim4.selectAll();

                return;
            }

            tmpStr = textDim5.getText();

            if (testParameter(tmpStr, 0, 5000)) {
                extents[4] = Double.valueOf(tmpStr).intValue();
            } else {
                textDim5.requestFocus();
                textDim5.selectAll();

                return;
            }

            for (i = 0, j = 0; i < 5; i++) {

                if (extents[i] != 0) {
                    j++;
                }
            }

            dimExtents = new int[j];

            for (i = 0; i < j; i++) {
                dimExtents[i] = extents[i];
            }

            
            unitsOfMeasure[0] = comboBoxUnitOfMeasure1.getSelectedIndex() + 1; // See FileInfoBase
            unitsOfMeasure[1] = comboBoxUnitOfMeasure1.getSelectedIndex() + 1;

            if (dimExtents.length >= 3) {
                unitsOfMeasure[2] = comboBoxUnitOfMeasure3.getSelectedIndex() + 1;
            }

            if (dimExtents.length >= 4) {
                unitsOfMeasure[3] = comboBoxUnitOfMeasure4.getSelectedIndex() + 1;
            }

            if (dimExtents.length >= 5) {
                unitsOfMeasure[4] = comboBoxUnitOfMeasure5.getSelectedIndex() + 1;
            }

            tmpStr = textRes1.getText();

            if (testParameter(tmpStr, 0, 100)) {
                resolutions[0] = Double.valueOf(tmpStr).floatValue();
            } else {
                textRes1.requestFocus();
                textRes1.selectAll();

                return;
            }

            tmpStr = textRes2.getText();

            if (testParameter(tmpStr, 0, 100)) {
                resolutions[1] = Double.valueOf(tmpStr).floatValue();
            } else {
                textRes2.requestFocus();
                textRes2.selectAll();

                return;
            }

            tmpStr = textRes3.getText();

            if (testParameter(tmpStr, 0, 100)) {
                resolutions[2] = Double.valueOf(tmpStr).floatValue();
            } else {
                textRes3.requestFocus();
                textRes3.selectAll();

                return;
            }

            tmpStr = textRes4.getText();

            if (testParameter(tmpStr, 0, 100)) {
                resolutions[3] = Double.valueOf(tmpStr).floatValue();
            } else {
                textRes4.requestFocus();
                textRes4.selectAll();

                return;
            }

            tmpStr = textRes5.getText();

            if (testParameter(tmpStr, 0, 100)) {
                resolutions[4] = Double.valueOf(tmpStr).floatValue();
            } else {
                textRes5.requestFocus();
                textRes5.selectAll();

                return;
            }

            // set the properties, cos we are sure we are done with the dialog
            Preferences.setProperty(Preferences.PREF_RAW_BIG_ENDIAN, String.valueOf(endianess));
            Preferences.setProperty(Preferences.PREF_RAW_DATA_OFFSET, textOffset.getText());
            Preferences.setProperty(Preferences.PREF_RAW_TYPE, Integer.toString(dataType));
            Preferences.setProperty(Preferences.PREF_RAW_EXTENTS, this.makeExtentsString());
            Preferences.setProperty(Preferences.PREF_RAW_RESOLUTIONS, this.makeResolutionString());
            Preferences.setProperty(Preferences.PREF_RAW_UNITS, this.makeUnitString());
            dispose();
        } else if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Inserts the values given by the preference file into the extents fields of the dialog.
     *
     * @param  commas  Delimited string representing the extents.
     */
    public void extractExtents(String commas) {
        StringTokenizer st;

        try {
            st = new StringTokenizer(commas, ",");
        } catch (NullPointerException npe) {
            return;
        }
        
       
        if(st.countTokens() == 5) {
	        textDim1.setText(st.nextToken());
	        textDim2.setText(st.nextToken());
	        textDim3.setText(st.nextToken());
	        textDim4.setText(st.nextToken());
	        textDim5.setText(st.nextToken());
        }
	    }

    /**
     * Inserts the values given by the preference file into the resolutions fields of the dialog.
     *
     * @param  commas  Delimited string representing the resolutions.
     */
    public void extractResolutions(String commas) {
        StringTokenizer st;

        try {
            st = new StringTokenizer(commas, ",");
        } catch (NullPointerException npe) {
            return;
        }

        textRes1.setText(st.nextToken());
        textRes2.setText(st.nextToken());
        textRes3.setText(st.nextToken());
        textRes4.setText(st.nextToken());
        textRes5.setText(st.nextToken());
    }

    /**
     * Inserts the values given by the preference file into the units of measure fields of the dialog.
     *
     * @param  commas  Delimited string representing the units of measure.
     */
    public void extractUnitIndeces(String commas) {
        StringTokenizer st;
        try {
            st = new StringTokenizer(commas, ",");
        } catch (NullPointerException npe) {
            return;
        }

        comboBoxUnitOfMeasure1.setSelectedIndex(Integer.parseInt(st.nextToken()));
        comboBoxUnitOfMeasure2.setSelectedIndex(comboBoxUnitOfMeasure1.getSelectedIndex());
        comboBoxUnitOfMeasure3.setSelectedIndex(Integer.parseInt(st.nextToken()));
        comboBoxUnitOfMeasure4.setSelectedIndex(Integer.parseInt(st.nextToken()));
        comboBoxUnitOfMeasure5.setSelectedIndex(Integer.parseInt(st.nextToken()));
    }

    /**
     * Accessor that returns the data type.
     *
     * @return  The data type
     */
    public int getDataType() {
        return dataType;
    }

    /**
     * Accessor that returns the endianess.
     *
     * @return  boolean indicating the endianess
     */
    public boolean getEndianess() {
        return endianess;
    }

    /**
     * Accessor that returns the extents.
     *
     * @return  The extents in an array
     */
    public int[] getExtents() {
        return dimExtents;
    }

    /**
     * Accessor that returns the offset.
     *
     * @return  The offset
     */
    public int getOffset() {
        return offset;
    }

    /**
     * Accessor that returns the resolutions.
     *
     * @return  The resolutions in an array
     */
    public float[] getResolutions() {
        return resolutions;
    }

    /**
     * Accessor that returns the units of measure for each dimension.
     *
     * @return  Int array indicating the unit of measure
     */
    public int[] getUnitsOfMeasure() {
        return unitsOfMeasure;
    }

    /**
     * Makes a single string out of the extents.
     *
     * @return  String with extents separated by commas.
     */
    public String makeExtentsString() {
        return textDim1.getText() + "," + textDim2.getText() + "," + textDim3.getText() + "," + textDim4.getText() +
               "," + textDim5.getText();
    }

    /**
     * Makes a single string out of the resolutions.
     *
     * @return  String with resolutions separated by commas.
     */
    public String makeResolutionString() {
        return (textRes1.getText() + "," + textRes2.getText() + "," + textRes3.getText() + "," + textRes4.getText() +
                "," + textRes5.getText());
    }

    /**
     * Makes a single string out of the units of measure.
     *
     * @return  String with units of measure separated by commas.
     */
    public String makeUnitString() {
        return comboBoxUnitOfMeasure1.getSelectedIndex() + "," + comboBoxUnitOfMeasure3.getSelectedIndex() + "," +
               comboBoxUnitOfMeasure4.getSelectedIndex() + "," + comboBoxUnitOfMeasure5.getSelectedIndex();
    }

    /**
     * Marks the checkmark when true. Equivalent to checkboxEnd.setSelected(b)
     *
     * @param  b  Value for the checkbox "Big Endian"
     */
    public void setBigEndian(boolean b) {
        checkboxEnd.setSelected(b);
    }

    /**
     * Method to preset the image data offset.
     *
     * @param  o  The offset
     */
    public void setDataOffset(String o) {
        textOffset.setText(o);
    }

    /**
     * Accessor that sets the appropriate radio button as given by the image-data type.
     *
     * @param  t  The data type
     */
    public void setDataType(int t) {

        if (t == ModelStorageBase.BOOLEAN) {
            radioByte.setSelected(true);
        } else if (t == ModelStorageBase.BYTE) {
            radioUByte.setSelected(true);
        } else if (t == ModelStorageBase.UBYTE) {
            radioUByte.setSelected(true);
        } else if (t == ModelStorageBase.SHORT) {
            radioShort.setSelected(true);
        } else if (t == ModelStorageBase.USHORT) {
            radioUShort.setSelected(true);
        } else if (t == ModelStorageBase.INTEGER) {
            radioInt.setSelected(true);
        } else if (t == ModelStorageBase.UINTEGER) {
            radioUInt.setSelected(true);
        } else if (t == ModelStorageBase.LONG) {
            radioLong.setSelected(true);
        } else if (t == ModelStorageBase.FLOAT) {
            radioFloat.setSelected(true);
        } else if (t == ModelStorageBase.DOUBLE) {
            radioDouble.setSelected(true);
        } else if (t == ModelStorageBase.ARGB) {
            radioARGB.setSelected(true);
        } else if (t == ModelStorageBase.ARGB_USHORT) {
            radioARGB_USHORT.setSelected(true);
        } else {
            radioShort.setSelected(true);
        }
    }

    /**
     * Initializes the GUI components and makes the dialog visible.
     */
    private void init() {
        JLabel dim1;
        JLabel dim2;
        JLabel dim3;
        JLabel dim4;
        JLabel dim5;

        Font serif12;
        serif12 = MipavUtil.font12;

        JPanel offsetPanel = new JPanel();

        labelOffset = new JLabel("Header offset");
        labelOffset.setFont(serif12);
        labelOffset.setForeground(Color.black);
        offsetPanel.add(labelOffset);

        textOffset = new JTextField(5);
        textOffset.setText("0");
        textOffset.setFont(serif12);
        textOffset.addFocusListener(this);
        offsetPanel.add(textOffset);

        checkboxEnd = new JCheckBox("Big endian", false);
        checkboxEnd.setFont(serif12);

        JPanel panelDims = new JPanel(new GridBagLayout());
        panelDims.setBorder(buildTitledBorder("Dimensions & resolutions"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(5, 5, 5, 5);
        gbc.fill = GridBagConstraints.NONE;

        dim1 = new JLabel("1st");
        dim1.setFont(serif12);
        dim1.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 0;
        panelDims.add(dim1, gbc);

        textDim1 = new JTextField(5);
        textDim1.setText("256");
        textDim1.setFont(serif12);
        textDim1.addFocusListener(this);
        gbc.gridx = 1;
        gbc.gridy = 0;
        panelDims.add(textDim1, gbc);

        textRes1 = new JTextField(5);
        textRes1.setText("1.0");
        textRes1.setFont(serif12);
        textRes1.addFocusListener(this);
        gbc.gridx = 2;
        gbc.gridy = 0;
        panelDims.add(textRes1, gbc);

        dim2 = new JLabel("2nd");
        dim2.setFont(serif12);
        dim2.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 1;
        panelDims.add(dim2, gbc);

        textDim2 = new JTextField(5);
        textDim2.setText("256");
        textDim2.setFont(serif12);
        textDim2.addFocusListener(this);
        gbc.gridx = 1;
        gbc.gridy = 1;
        panelDims.add(textDim2, gbc);

        textRes2 = new JTextField(5);
        textRes2.setText("1.0");
        textRes2.setFont(serif12);
        textRes2.addFocusListener(this);
        gbc.gridx = 2;
        gbc.gridy = 1;
        panelDims.add(textRes2, gbc);

        dim3 = new JLabel("3rd");
        dim3.setFont(serif12);
        dim3.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 2;
        panelDims.add(dim3, gbc);

        textDim3 = new JTextField(5);
        textDim3.setText("0");
        textDim3.setFont(serif12);
        textDim3.addFocusListener(this);
        gbc.gridx = 1;
        gbc.gridy = 2;
        panelDims.add(textDim3, gbc);

        textRes3 = new JTextField(5);
        textRes3.setText("0");
        textRes3.setFont(serif12);
        textRes3.addFocusListener(this);
        gbc.gridx = 2;
        gbc.gridy = 2;
        panelDims.add(textRes3, gbc);

        dim4 = new JLabel("4th");
        dim4.setFont(serif12);
        dim4.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 3;
        panelDims.add(dim4, gbc);

        textDim4 = new JTextField(5);
        textDim4.setText("0");
        textDim4.setFont(serif12);
        textDim4.addFocusListener(this);
        gbc.gridx = 1;
        gbc.gridy = 3;
        panelDims.add(textDim4, gbc);

        textRes4 = new JTextField(5);
        textRes4.setText("0");
        textRes4.setFont(serif12);
        textRes4.addFocusListener(this);
        gbc.gridx = 2;
        gbc.gridy = 3;
        panelDims.add(textRes4, gbc);

        dim5 = new JLabel("5th");
        dim5.setFont(serif12);
        dim5.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 4;
        panelDims.add(dim5, gbc);

        textDim5 = new JTextField(5);
        textDim5.setText("0");
        textDim5.setFont(serif12);
        textDim5.addFocusListener(this);
        gbc.gridx = 1;
        gbc.gridy = 4;
        panelDims.add(textDim5, gbc);

        textRes5 = new JTextField(5);
        textRes5.setText("0");
        textRes5.setFont(serif12);
        textRes5.addFocusListener(this);
        gbc.gridx = 2;
        gbc.gridy = 4;
        panelDims.add(textRes5, gbc);

        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 3;
        gbc.fill = GridBagConstraints.VERTICAL;
        gbc.weighty = 1;
        panelDims.add(new JPanel(), gbc);

        JPanel panelImageType = new JPanel(new GridLayout(11, 1));
        panelImageType.setBorder(buildTitledBorder("Image type"));

        ButtonGroup group1 = new ButtonGroup();
        radioBool = new JRadioButton("Boolean", false);
        radioBool.setFont(serif12);
        group1.add(radioBool);
        panelImageType.add(radioBool);

        radioByte = new JRadioButton("Byte", false);
        radioByte.setFont(serif12);
        group1.add(radioByte);
        panelImageType.add(radioByte);

        radioUByte = new JRadioButton("Unsigned byte", false);
        radioUByte.setFont(serif12);
        group1.add(radioUByte);
        panelImageType.add(radioUByte);

        radioShort = new JRadioButton("Short", true);
        radioShort.setFont(serif12);
        group1.add(radioShort);
        panelImageType.add(radioShort);

        radioUShort = new JRadioButton("Unsigned short", false);
        radioUShort.setFont(serif12);
        group1.add(radioUShort);
        panelImageType.add(radioUShort);

        radioInt = new JRadioButton("Integer", false);
        radioInt.setFont(serif12);
        group1.add(radioInt);
        panelImageType.add(radioInt);

        radioUInt = new JRadioButton("Unsigned integer", false);
        radioUInt.setFont(serif12);
        group1.add(radioUInt);
        panelImageType.add(radioUInt);

        radioLong = new JRadioButton("Long", false);
        radioLong.setFont(serif12);
        group1.add(radioLong);
        panelImageType.add(radioLong);

        radioFloat = new JRadioButton("Float", false);
        radioFloat.setFont(serif12);
        group1.add(radioFloat);
        panelImageType.add(radioFloat);

        radioDouble = new JRadioButton("Double", false);
        radioDouble.setFont(serif12);
        group1.add(radioDouble);
        panelImageType.add(radioDouble);

        radioARGB = new JRadioButton("ARGB", false);
        radioARGB.setFont(serif12);
        group1.add(radioARGB);
        panelImageType.add(radioARGB);

        radioARGB_USHORT = new JRadioButton("ARGB U short", false);
        radioARGB_USHORT.setFont(serif12);
        group1.add(radioARGB_USHORT);
        panelImageType.add(radioARGB_USHORT);

        unitsOfMeasure = new int[5];

        JPanel panelUnits = new JPanel(new GridBagLayout());
        panelUnits.setBorder(buildTitledBorder("Units of measure"));
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.weighty = 0;
        gbc.insets = new Insets(3, 5, 3, 5);

        comboBoxUnitOfMeasure1 = new JComboBox();
        setComboBox(comboBoxUnitOfMeasure1);
        comboBoxUnitOfMeasure1.setSelectedIndex(7);
        comboBoxUnitOfMeasure1.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    comboBoxUnitOfMeasure2.setSelectedIndex(comboBoxUnitOfMeasure1.getSelectedIndex());
                }
            });
        panelUnits.add(comboBoxUnitOfMeasure1, gbc);

        comboBoxUnitOfMeasure2 = new JComboBox();
        setComboBox(comboBoxUnitOfMeasure2);
        comboBoxUnitOfMeasure2.setSelectedIndex(7);
        comboBoxUnitOfMeasure2.setEnabled(false);
        gbc.gridy = 1;
        panelUnits.add(comboBoxUnitOfMeasure2, gbc);

        comboBoxUnitOfMeasure3 = new JComboBox();
        setComboBox(comboBoxUnitOfMeasure3);
        comboBoxUnitOfMeasure3.setSelectedIndex(7);
        gbc.gridy = 2;
        panelUnits.add(comboBoxUnitOfMeasure3, gbc);

        comboBoxUnitOfMeasure4 = new JComboBox();
        setComboBox(comboBoxUnitOfMeasure4);
        comboBoxUnitOfMeasure4.setSelectedIndex(7);
        gbc.gridy = 3;
        panelUnits.add(comboBoxUnitOfMeasure4, gbc);

        comboBoxUnitOfMeasure5 = new JComboBox();
        setComboBox(comboBoxUnitOfMeasure5);
        comboBoxUnitOfMeasure5.setSelectedIndex(7);
        gbc.gridy = 4;
        panelUnits.add(comboBoxUnitOfMeasure5, gbc);

        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.NORTH;
        gbc.gridy = 5;
        gbc.fill = GridBagConstraints.VERTICAL;
        panelUnits.add(new JPanel(), gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.NORTH;
        gbc.weighty = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.VERTICAL;
        mainPanel.add(panelImageType, gbc);
        gbc.gridx = 1;
        mainPanel.add(panelDims, gbc);
        gbc.gridx = 2;
        mainPanel.add(panelUnits, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(offsetPanel, gbc);
        gbc.gridx = 1;
        mainPanel.add(checkboxEnd, gbc);

        // check the properties for one of the necessary entries, otherwise we'll use & set defaults
        if (Preferences.getProperty(Preferences.PREF_RAW_BIG_ENDIAN) != null) {
            boolean b = (Preferences.getProperty(Preferences.PREF_RAW_BIG_ENDIAN).equalsIgnoreCase("true")) ? true : false;
            //            setBigEndian(Boolean.getBoolean(Preferences.getProperty(Preferences.PREF_RAW_BIG_ENDIAN)));
            setBigEndian(b);
            setDataOffset(Preferences.getProperty(Preferences.PREF_RAW_DATA_OFFSET));
            setDataType(Integer.parseInt(Preferences.getProperty(Preferences.PREF_RAW_TYPE)));
            extractExtents(Preferences.getProperty(Preferences.PREF_RAW_EXTENTS));
            extractResolutions(Preferences.getProperty(Preferences.PREF_RAW_RESOLUTIONS));
            extractUnitIndeces(Preferences.getProperty(Preferences.PREF_RAW_UNITS));
        } else { // we'll tell the properties that we want to remember these preferences. (use defaults or flag values
                 // here!)
            Preferences.setProperty(Preferences.PREF_RAW_BIG_ENDIAN, new Boolean(DEFAULT_BIG_ENDIAN_BYTE_ORDER).toString());
            Preferences.setProperty(Preferences.PREF_RAW_DATA_OFFSET, new Integer(DEFAULT_DATA_OFFSET).toString());
            Preferences.setProperty(Preferences.PREF_RAW_TYPE, new Integer(DEFAULT_IMAGE_TYPE).toString());
            Preferences.setProperty(Preferences.PREF_RAW_EXTENTS, DEFAULT_EXTENTS);
            Preferences.setProperty(Preferences.PREF_RAW_RESOLUTIONS, DEFAULT_RES);
            Preferences.setProperty(Preferences.PREF_RAW_UNITS, DEFAULT_UNIT_INDEX);
        }

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
    }

    /**
     * Sets combo box choices that match resolution units listed in FileInfoBase in the same order.
     *
     * @param  cBox  Combo box to setup to display the units
     */
    private void setComboBox(JComboBox cBox) {

        cBox.setFont(serif12);
        cBox.setBackground(Color.white);
        cBox.addItem(" UNKNOWN");
        cBox.addItem(" INCHES ");
        cBox.addItem(" MILS (.001 INCH) ");
        cBox.addItem(" CENTIMETERS ");
        cBox.addItem(" ANGSTROMS ");
        cBox.addItem(" NANOMETERS ");
        cBox.addItem(" MICROMETERS ");
        cBox.addItem(" MILLIMETERS ");
        cBox.addItem(" METERS ");
        cBox.addItem(" KILOMETERS ");
        cBox.addItem(" MILES ");
        cBox.addItem(" NANOSECONDS ");
        cBox.addItem(" MICROSECONDS ");
        cBox.addItem(" MILLISECONDS ");
        cBox.addItem(" SECONDS ");
        cBox.addItem(" MINUTES ");
        cBox.addItem(" HOURS ");
        cBox.addItem(" HZ ");

        cBox.addItemListener(this);
    }
}
