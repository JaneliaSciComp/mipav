package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;

import javax.swing.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.awt.event.*;
import java.io.*;
import java.awt.*;

/**
 *   The image attribute input dialog, which consists of six tabbled panes allowing the
 *   user to edit image name, resolutions, orientations, dataset origin,
 *   history, and transformation matrix.
 *
 *		@version    0.1 Nov 23, 1999
 *		@author     Matthew J. McAuliffe, Ph.D.
 *
 */
public class JDialogImageInfo
    extends JDialogBase implements ActionListener {

    private Font font12B;
    private JTabbedPane tabbedPane;

    private ModelImage image;
    private JTextField textRes1, textRes2, textRes3, textRes4, textRes5;
    private JTextField textSt1, textSt2, textSt3, textSt4, textSliceSpacing;
    private JCheckBox resolutionBox; // checkbox for "apply to all slices" for resolution changes
    private JTextField nameText;
    private JButton applyButton;
    private JToolBar tBar;
    private JComboBox comboBoxUnitOfMeasure1;
    private JComboBox comboBoxUnitOfMeasure3;
    private JComboBox comboBoxUnitOfMeasure4;
    private JComboBox comboBoxUnitOfMeasure5;
    private JComboBox orientBox;
    private JComboBox orientationBox1;
    private JComboBox orientationBox2;
    private JComboBox orientationBox3;
    private JComboBox transformIDBox;
    private JTextField[][] textMatrix;
    private String newImageName;
    private int orient;
    private int orientAxis[] = new int[3];
    private float resolutions[];
    private float origin[];
    private int measure1, measure3, measure4, measure5;
    private double[][] matrix;
    private JComboBox modalityBox;
    private String[] modalityStr;
    private int modality;
    private float sliceSpacing = 0;

    private JTextField linkedImageField;
    private JButton linkedImageButton;

    private String matrixFile;
    private TransMatrix fileTransMatrix;
    private int DIM;
    private ModelImage resampleImage;

    private int resIndex = 0; //index for saving resolution

    private JPanel buttonPanel;

    //ACPC Specific info
    private JTextField[] origACFields;
    private JTextField[] origPCFields;
    private JTextField[] origDimFields;
    private JTextField[] origResFields;
    private JTextField[] acpcACFields;
    private JTextField[] acpcPCFields;
    private JTextField acpcResField;
    private JTextField[] acpcDimFields;
    private JTextField[] orientFields;

    //TLRC Specific info
    private JTextField[] acpcMinFields;
    private JTextField[] acpcMaxFields;
    private JTextField[] tlrcACFields;
    private JTextField[] tlrcPCFields;
    private JTextField[] tlrcResFields;
    private JTextField[] tlrcDimFields;

    private JCheckBox isTLRCBox;

    private JButton loadButton;
    private JButton saveButton;

    /**
     *   Builds the image attribute input dialog, with three tabbled panes allowing the
     *   user to edit image name, orientation, resolutions, and transformation matrix.
     *   @param theParentFrame   Parent frame of dialog.
     *   @param im               Image whose attributes the user is editing.
     */
    public JDialogImageInfo(Frame theParentFrame, ModelImage im, int zSlice, int tSlice) {
        super(theParentFrame, false);

        image = im;
        resampleImage = im;
        String addTitle = "";

        if (image.getNDims() == 3) {
            resIndex = zSlice;
            addTitle = Integer.toString(zSlice + 1);
        }
        else if (image.getNDims() > 3) {
            resIndex = zSlice * image.getExtents()[3] + tSlice;
            addTitle = Integer.toString(zSlice + 1) + " : " + Integer.toString(tSlice + 1);
        }
        font12B = MipavUtil.font12B;
        if (image.getNDims() >= 3)
            matrix = new double[4][4];
        else
            matrix = new double[3][3];
        if (image.getNDims() == 4)
            DIM = 4;
        else if (image.getNDims() == 3)
            DIM = 3;
        else if (image.getNDims() == 2)
            DIM = 2;

        init(addTitle);
    }

    /**
     *   Initializes the dialog box and adds the components.
     */
    private void init(String addTitle) {
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(font12B);

        setTitle("Image Attributes: " + image.getImageName() + " " + addTitle);
        tabbedPane.addTab("Name", null, buildNamePanel());
        tabbedPane.addTab("Resolutions", null, buildResolutionPanel());
        tabbedPane.addTab("Orientations", null, buildOrientPanel());
        tabbedPane.addTab("Dataset Origin", null, buildStartLocationsPanel());
        tabbedPane.addTab("Transform matrix", null, buildMatrixPanel());
        tabbedPane.addTab("History", null, buildHistoryPanel());
        tabbedPane.addTab("Talairach", null, buildTalairachPanel());

        /**
                if (((String)transformIDBox.getSelectedItem()).equals("Talairach Tournoux")) {
                    showTalairachTab(true);
                }
         */
        mainDialogPanel.add(tabbedPane);

        buttonPanel = new JPanel();

        applyButton = new JButton("Apply");
        applyButton.setFont(serif12B);
        applyButton.setPreferredSize(MipavUtil.defaultButtonSize);
        applyButton.setMinimumSize(MipavUtil.defaultButtonSize);
        applyButton.addActionListener(this);
        buttonPanel.add(applyButton);

        buildOKButton();
        buildCancelButton();
        cancelButton.setText("Close");
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        mainDialogPanel.add(buttonPanel, "South");

        getContentPane().add(mainDialogPanel);

        pack();
    }

    /**
     *   Builds the "orientation edit" panel.
     *	@return	The panel on which the user can edit the name of the image.
     */
    private JPanel buildOrientPanel() {
        JLabel orientIconLabel = new JLabel(
            "Image origin is in the upper left hand corner. Righthand coordinate system.",
            MipavUtil.getIcon("orient.gif"), JLabel.LEFT);
        orientIconLabel.setFont(serif12);
        orientIconLabel.setForeground(Color.black);

        JPanel orientPanel = new JPanel(new GridBagLayout());
        orientPanel.setBorder(buildTitledBorder(""));

        JLabel orientLabel = new JLabel("Image orientation:");
        orientLabel.setFont(serif12);
        orientLabel.setForeground(Color.black);

        orientBox = new JComboBox();
        orientBox.setBackground(Color.white);
        orientBox.addItem("Axial");
        orientBox.addItem("Coronal");
        orientBox.addItem("Sagittal");
        orientBox.addItem("Unknown");
        int orient;
        if (image.getImageOrientation() == FileInfoBase.AXIAL ||
            image.getImageOrientation() == FileInfoBase.CORONAL ||
            image.getImageOrientation() == FileInfoBase.SAGITTAL) {
            orient = image.getImageOrientation();
        }
        else {
            orient = FileInfoBase.UNKNOWN_ORIENT; // FileInfoBase.UNKNOWN_ORIENT = 3
        }

        orientBox.setSelectedIndex(orient);
        orientBox.setFont(serif12);
        orientBox.addFocusListener(this);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(0, 5, 0, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = gbc.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.anchor = gbc.WEST;
        orientPanel.add(orientIconLabel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.fill = gbc.NONE;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        orientPanel.add(orientLabel, gbc);
        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.weightx = 1;
        orientPanel.add(orientBox, gbc);

        String orients[] = {
            "Unknown",
            "Patient Right to Left",
            "Patient Left to Right",
            "Patient Posterior to Anterior",
            "Patient Anterior to Posterior",
            "Patient Inferior to Superior",
            "Patient Superior to Inferior"};

        JLabel orientLabelX = new JLabel(
            "X-axis orientation (image left to right) corresponds to:");
        orientLabelX.setFont(serif12);
        orientLabelX.setForeground(Color.black);

        orientationBox1 = new JComboBox(orients);
        orientationBox1.setBackground(Color.white);
        orientationBox1.setFont(MipavUtil.font12);

        int axisOrient[] = image.getFileInfo()[0].getAxisOrientation();
        orientationBox1.setSelectedIndex(axisOrient[0]);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.fill = gbc.NONE;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        orientPanel.add(orientLabelX, gbc);
        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.weightx = 1;
        orientPanel.add(orientationBox1, gbc);

        JLabel orientLabelY = new JLabel(
            "Y-axis orientation (image top to bottom) corresponds to:");
        orientLabelY.setFont(serif12);
        orientLabelY.setForeground(Color.black);

        orientationBox2 = new JComboBox(orients);
        orientationBox2.setBackground(Color.white);
        orientationBox2.setFont(MipavUtil.font12);
        orientationBox2.setSelectedIndex(axisOrient[1]);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.fill = gbc.NONE;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        orientPanel.add(orientLabelY, gbc);
        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.weightx = 1;
        orientPanel.add(orientationBox2, gbc);

        JLabel orientLabelZ = new JLabel(
            "Z-axis orientation (into the screen) corresponds to:");
        orientLabelZ.setFont(serif12);
        orientLabelZ.setForeground(Color.black);

        orientationBox3 = new JComboBox(orients);
        orientationBox3.setBackground(Color.white);
        orientationBox3.setFont(MipavUtil.font12);
        orientationBox3.setSelectedIndex(axisOrient[2]);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.fill = gbc.NONE;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        orientPanel.add(orientLabelZ, gbc);
        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.weightx = 1;
        orientPanel.add(orientationBox3, gbc);

        return orientPanel;
    }

    /**
     *   Builds the "edit image name" panel.
     *	@return	The panel on which the user can edit the name of the image.
     */
    private JPanel buildNamePanel() {
        int i;

        JPanel namePanel = new JPanel(new GridBagLayout());
        namePanel.setBorder(buildTitledBorder(""));

        JLabel nameLabel = new JLabel("Image name (without suffix):");
        nameLabel.setFont(serif12);
        nameLabel.setForeground(Color.black);

        nameText = new JTextField();
        nameText.setText(image.getImageName());
        nameText.setFont(serif12);
        nameText.addFocusListener(this);

        JLabel modalityLabel = new JLabel("Image modality :");
        modalityLabel.setFont(serif12);
        modalityLabel.setForeground(Color.black);

        modalityBox = new JComboBox();
        modalityBox.setBackground(Color.white);
        modalityStr = FileInfoBase.getModalityStr();
        for (i = 0; i < modalityStr.length; i++) {
            modalityBox.addItem(modalityStr[i]);
        }
        modality = image.getFileInfo(0).getModality();
        modalityBox.setSelectedIndex(modality);
        modalityBox.setFont(serif12);
        modalityBox.addFocusListener(this);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = gbc.WEST;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(5, 5, 5, 5);

        // if the image is an XML file.. we can now edit the XML linked image path
        if (image.getFileInfo(0) instanceof FileInfoXML) {
            JLabel linkLabel = new JLabel("XML linked image:");
            linkLabel.setFont(serif12);
            linkLabel.setForeground(Color.black);

            namePanel.add(linkLabel, gbc);
            gbc.gridx = 1;
            gbc.gridwidth = gbc.REMAINDER;

            JPanel linkedImagePanel = new JPanel();
            linkedImageField = new JTextField(40);
            linkedImageField.setFont(MipavUtil.font12);

            linkedImageButton = new JButton("Browse");
            linkedImageButton.setFont(MipavUtil.font12B);
            linkedImageButton.addActionListener(this);
            linkedImageButton.setActionCommand("BrowseLinked");

            linkedImagePanel.add(linkedImageField);
            linkedImagePanel.add(linkedImageButton);
            linkedImagePanel.setBorder(buildTitledBorder(""));

            String path = ( (FileInfoImageXML) image.getFileInfo(0)).getLinkedImagePath();

            if (path != null) {
                if (new File(path).exists()) {
                    linkedImageField.setText(path);
                } else {
                    int response = JOptionPane.showConfirmDialog(this, "Linked file does not exist: maintain link?", "Linked file", JOptionPane.YES_NO_OPTION);
                    if (response == JOptionPane.YES_OPTION) {
                        linkedImageField.setText(path);
                    }
                }
            }
            namePanel.add(linkedImagePanel, gbc);

            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.gridwidth = 1;
        }

        namePanel.add(nameLabel, gbc);
        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.weightx = 1;
        namePanel.add(nameText, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.fill = gbc.NONE;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        namePanel.add(modalityLabel, gbc);
        gbc.gridx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.weightx = 1;
        namePanel.add(modalityBox, gbc);

        return namePanel;
    }

    /**
     *	Builds the panels which is edited in the tabbed pane "resolutions".
     *	@return The resolutions panel.
     */
    private JPanel buildResolutionPanel() {

        int nDims = image.getNDims();

        JPanel resolPanel = new JPanel(new GridBagLayout());
        resolPanel.setBorder(buildTitledBorder(""));

        JLabel dim1 = new JLabel("1st dimension:");
        dim1.setFont(serif12);
        dim1.setForeground(Color.black);

        JLabel dim2 = new JLabel("2nd dimension:");
        dim2.setFont(serif12);
        dim2.setForeground(Color.black);

        JLabel dim3 = new JLabel("3rd dimension:");
        dim3.setFont(serif12);
        dim3.setForeground(Color.black);
        if (nDims < 3)
            dim3.setEnabled(false);

        JLabel dim4 = new JLabel("4th dimension:");
        dim4.setFont(serif12);
        dim4.setForeground(Color.black);
        if (nDims < 4)
            dim4.setEnabled(false);

        JLabel dim5 = new JLabel("5th dimension:");
        dim5.setFont(serif12);
        dim5.setForeground(Color.black);
        if (nDims < 5)
            dim5.setEnabled(false);
        
        JLabel sliceSpacingLabel = new JLabel("Slice spacing:");
        sliceSpacingLabel.setFont(serif12);
        sliceSpacingLabel.setForeground(Color.black);
        if (nDims < 3)
        	sliceSpacingLabel.setEnabled(false);

        textRes1 = new JTextField(5);
        textRes1.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[0]));
        textRes1.setFont(serif12);
        textRes1.addFocusListener(this);

        textRes2 = new JTextField(5);
        textRes2.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[1]));
        textRes2.setFont(serif12);
        textRes2.addFocusListener(this);

        textRes3 = new JTextField(5);
        textRes3.setText("1");
        textRes3.setFont(serif12);
        textRes3.addFocusListener(this);
        if (nDims < 3) {
            textRes3.setEnabled(false);
        }
        else {
            textRes3.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[2]));
        }

        textRes4 = new JTextField(5);
        textRes4.setText("1");
        textRes4.setFont(serif12);
        textRes4.addFocusListener(this);
        if (nDims < 4) {
            textRes4.setEnabled(false);
        }
        else {
            textRes4.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[3]));
        }

        textRes5 = new JTextField(5);
        textRes5.setText("1");
        textRes5.setFont(serif12);
        textRes5.addFocusListener(this);
        if (nDims < 5) {
            textRes5.setEnabled(false);
        }
        else {
            textRes5.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[4]));
        }
        
        textSliceSpacing = new JTextField(5);
        textSliceSpacing.setText("0");
        textSliceSpacing.setFont(serif12);
        if (nDims < 3)
        {
        	textSliceSpacing.setEnabled(false);
        }
        else
        {
        	sliceSpacing = image.getFileInfo()[resIndex].getSliceSpacing();
        	textSliceSpacing.setText(String.valueOf(sliceSpacing));
        }

        resolutionBox = new JCheckBox("Apply resolution changes to all slices and/or times");
        resolutionBox.setFont(serif12);
        resolutionBox.setSelected(true);
        resolutionBox.setEnabled(image.getNDims() > 2);

        JPanel labelPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        labelPanel.add(dim1, gbc);
        gbc.gridy = 1;
        labelPanel.add(dim2, gbc);
        gbc.gridy = 2;
        labelPanel.add(dim3, gbc);
        gbc.gridy = 3;
        labelPanel.add(dim4, gbc);
        gbc.gridy = 4;
        labelPanel.add(dim5, gbc);
        gbc.gridy = 5;
        labelPanel.add(sliceSpacingLabel, gbc);
        gbc.gridy = 6;
        gbc.gridwidth = 2;
        gbc.insets = new Insets(5, 0, 5, 5);
        labelPanel.add(resolutionBox, gbc);

        gbc.insets = new Insets(5, 5, 5, 5);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        labelPanel.add(textRes1, gbc);
        gbc.gridy = 1;
        labelPanel.add(textRes2, gbc);
        gbc.gridy = 2;
        labelPanel.add(textRes3, gbc);
        gbc.gridy = 3;
        labelPanel.add(textRes4, gbc);
        gbc.gridy = 4;
        labelPanel.add(textRes5, gbc);
        gbc.gridy = 5;
        labelPanel.add(textSliceSpacing, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.NORTHWEST;
        gbc.fill = gbc.HORIZONTAL;
        gbc.weighty = 1;
        resolPanel.add(labelPanel, gbc);
        gbc.gridx = 1;
        resolPanel.add(buildComboBox(), gbc);
        return resolPanel;
    }

    /**
     *	Builds the panels which is edited in the tabbed pane "Dataset Origin".
     *	@return The Dataset Origin panel.
     */
    private JPanel buildStartLocationsPanel() {

        int nDims = image.getNDims();

        JPanel stPanel = new JPanel(new GridBagLayout());
        stPanel.setBorder(buildTitledBorder(
            " Origin for the first image slice (upper left corner) "));

        JLabel dim1 = new JLabel("1st dimension:");
        dim1.setFont(serif12);
        dim1.setForeground(Color.black);

        JLabel dim2 = new JLabel("2nd dimension:");
        dim2.setFont(serif12);
        dim2.setForeground(Color.black); ;

        JLabel dim3 = new JLabel("3rd dimension:");
        dim3.setFont(serif12);
        dim3.setForeground(Color.black);
        if (nDims < 3)
            dim3.setEnabled(false);

        JLabel dim4 = new JLabel("4th dim.");
        dim4.setFont(serif12);
        dim4.setForeground(Color.black);
        if (nDims < 4)
            dim4.setEnabled(false);

        //JLabel dim5 = new JLabel("5th dim.");
        //dim5.setFont(serif12);
        //dim5.setForeground(Color.black);
        //if (nDims < 5) dim5.setEnabled(false);

        textSt1 = new JTextField(5);
        textSt1.setText(String.valueOf(image.getFileInfo()[0].getOrigin(0)));
        textSt1.setFont(serif12);
        textSt1.addFocusListener(this);

        textSt2 = new JTextField(5);
        textSt2.setText(String.valueOf(image.getFileInfo()[0].getOrigin(1)));
        textSt2.setFont(serif12);
        textSt2.addFocusListener(this);

        textSt3 = new JTextField(5);
        textSt3.setText("1");
        textSt3.setFont(serif12);
        textSt3.addFocusListener(this);
        if (nDims < 3) {
            textSt3.setEnabled(false);
        }
        else {
            textSt3.setText(String.valueOf(image.getFileInfo()[0].getOrigin(2)));
        }

        textSt4 = new JTextField(5);
        textSt4.setText("1");
        textSt4.setFont(serif12);
        textSt4.addFocusListener(this);
        if (nDims < 4) {
            textSt4.setEnabled(false);
        }
        else {
            textSt4.setText(String.valueOf(image.getFileInfo()[0].getOrigin(3)));
        }
        /*
             textSt5 = new JTextField(5);
             textSt5.setText("1");
             textSt5.setFont(serif12);
             textSt5.addFocusListener(this);
             if (nDims < 5) {
                 textSt5.setEnabled(false);
             }
             else {
               textSt5.setText(String.valueOf(image.getFileInfo()[0].getStartLocation(4)));
             }
         */

        JPanel labelPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        labelPanel.add(dim1, gbc);
        gbc.gridy = 1;
        labelPanel.add(dim2, gbc);
        gbc.gridy = 2;
        labelPanel.add(dim3, gbc);
        gbc.gridy = 3;
        labelPanel.add(dim4, gbc);
        //gbc.gridy = 4;
        //labelPanel.add(dim5, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = gbc.REMAINDER;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        labelPanel.add(textSt1, gbc);
        gbc.gridy = 1;
        labelPanel.add(textSt2, gbc);
        gbc.gridy = 2;
        labelPanel.add(textSt3, gbc);
        gbc.gridy = 3;
        labelPanel.add(textSt4, gbc);
        //gbc.gridy = 4;
        //labelPanel.add(textSt5, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.NORTHWEST;
        gbc.fill = gbc.HORIZONTAL;
        gbc.weighty = 1;
        stPanel.add(labelPanel, gbc);

        //gbc.gridx = 1;
        //resolPanel.add(buildComboBox(), gbc);
        return stPanel;
    }

    /**
     * Builds the Talairach Transform scrollpane
     * with all talairach related data
     * @return JScrollPane talairach scrollpane
     */
    private JScrollPane buildTalairachPanel() {

        //build the ACPC Panel
        JPanel acpcPanel = new JPanel(new GridBagLayout());
        acpcPanel.setBorder(buildTitledBorder("ACPC"));

        JLabel origACLabel = new JLabel("Orig AC:");
        origACLabel.setFont(serif12);
        JLabel origPCLabel = new JLabel("Orig PC:");
        origPCLabel.setFont(serif12);
        JLabel origDimLabel = new JLabel("Orig Dim:");
        origDimLabel.setFont(serif12);
        JLabel origResLabel = new JLabel("Orig Res:");
        origResLabel.setFont(serif12);
        JLabel acpcACLabel = new JLabel("ACPC AC:");
        acpcACLabel.setFont(serif12);
        JLabel acpcPCLabel = new JLabel("ACPC PC:");
        acpcPCLabel.setFont(serif12);
        JLabel acpcResLabel = new JLabel("ACPC Res:");
        acpcResLabel.setFont(serif12);
        JLabel acpcDimLabel = new JLabel("ACPC Dim:");
        acpcDimLabel.setFont(serif12);
        JLabel orientLabel = new JLabel("Orig Orient:");
        orientLabel.setFont(serif12);

        origACFields = new JTextField[3];
        origPCFields = new JTextField[3];
        origDimFields = new JTextField[3];
        origResFields = new JTextField[3];
        acpcACFields = new JTextField[3];
        acpcPCFields = new JTextField[3];
        acpcDimFields = new JTextField[3];
        orientFields = new JTextField[9];

        for (int i = 0; i < 3; i++) {
            origACFields[i] = new JTextField("1.0", 3);
            origPCFields[i] = new JTextField("1.0", 3);
            origDimFields[i] = new JTextField("1", 3);
            origResFields[i] = new JTextField("1.0", 3);
            acpcACFields[i] = new JTextField("1.0", 3);
            acpcACFields[i].setEditable(false);
            acpcPCFields[i] = new JTextField("1.0", 3);
            acpcDimFields[i] = new JTextField("1.0", 3);
            acpcDimFields[i].setEditable(false);
        }

        for (int i = 0; i < 9; i++) {
            orientFields[i] = new JTextField("1.0", 3);
        }

        acpcResField = new JTextField("1.0", 3);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = gbc.HORIZONTAL;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(3, 5, 3, 5);

        //orig AC
        gbc.gridx = 0;
        gbc.gridy = 0;
        acpcPanel.add(origACLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            acpcPanel.add(origACFields[i], gbc);
        }

        //orig PC
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        acpcPanel.add(origPCLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            acpcPanel.add(origPCFields[i], gbc);
        }

        //orig Dim
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        acpcPanel.add(origDimLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            acpcPanel.add(origDimFields[i], gbc);
        }

        //orig Res
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        acpcPanel.add(origResLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            acpcPanel.add(origResFields[i], gbc);
        }

        //Original Orientation
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        acpcPanel.add(orientLabel, gbc);

        gbc.weightx = 1;
        for (int j = 0; j < 3; j++) {
            gbc.gridx = 0;
            for (int i = 0; i < 3; i++) {
                gbc.gridx++;
                acpcPanel.add(orientFields[ (j * 3) + i], gbc);
            }
            gbc.gridy++;
        }

        //ACPC AC
        //gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        acpcPanel.add(acpcACLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            acpcPanel.add(acpcACFields[i], gbc);
        }

        //ACPC PC
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        acpcPanel.add(acpcPCLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            acpcPanel.add(acpcPCFields[i], gbc);
        }

        //ACPC Dim
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        acpcPanel.add(acpcDimLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            acpcPanel.add(acpcDimFields[i], gbc);
        }

        //ACPC Res
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        acpcPanel.add(acpcResLabel, gbc);

        gbc.gridx++;
        gbc.weightx = 1;
        acpcPanel.add(acpcResField, gbc);

        // Build the Talairach Specific Panel
        JPanel tlrcPanel = new JPanel(new GridBagLayout());
        tlrcPanel.setBorder(buildTitledBorder("Talairach"));

        JLabel acpcMinLabel = new JLabel("ACPC Min:");
        acpcMinLabel.setFont(serif12);
        JLabel acpcMaxLabel = new JLabel("ACPC Max:");
        acpcMaxLabel.setFont(serif12);
        JLabel tlrcACLabel = new JLabel("Talairach AC:");
        tlrcACLabel.setFont(serif12);
        JLabel tlrcPCLabel = new JLabel("Talairach PC:");
        tlrcPCLabel.setFont(serif12);
        JLabel tlrcResLabel = new JLabel("Talairach Res:");
        tlrcResLabel.setFont(serif12);
        JLabel tlrcDimLabel = new JLabel("Talairach Dim:");
        tlrcDimLabel.setFont(serif12);

        acpcMinFields = new JTextField[3];
        acpcMaxFields = new JTextField[3];
        tlrcACFields = new JTextField[3];
        tlrcPCFields = new JTextField[3];
        tlrcDimFields = new JTextField[3];
        tlrcResFields = new JTextField[9];

        for (int i = 0; i < 3; i++) {
            acpcMinFields[i] = new JTextField("1.0", 3);
            acpcMinFields[i].setEnabled(false);
            acpcMaxFields[i] = new JTextField("1.0", 3);
            acpcMaxFields[i].setEnabled(false);
            tlrcACFields[i] = new JTextField("1.0", 3);
            tlrcACFields[i].setEnabled(false);
            tlrcACFields[i].setEditable(false);
            tlrcPCFields[i] = new JTextField("1.0", 3);
            tlrcPCFields[i].setEnabled(false);
            tlrcPCFields[i].setEditable(false);
            tlrcDimFields[i] = new JTextField("1.0", 3);
            tlrcDimFields[i].setEnabled(false);
            tlrcDimFields[i].setEditable(false);
        }

        for (int i = 0; i < 7; i++) {
            tlrcResFields[i] = new JTextField("1.0", 3);
            tlrcResFields[i].setEnabled(false);
        }

        //ACPC Min
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridx = 0;
        gbc.weightx = 0;
        gbc.gridy++;
        tlrcPanel.add(acpcMinLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            tlrcPanel.add(acpcMinFields[i], gbc);
        }

        //ACPC Max
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        tlrcPanel.add(acpcMaxLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            tlrcPanel.add(acpcMaxFields[i], gbc);
        }

        //TLRC AC
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        tlrcPanel.add(tlrcACLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            tlrcPanel.add(tlrcACFields[i], gbc);
        }

        //TLRC PC
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        tlrcPanel.add(tlrcPCLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            tlrcPanel.add(tlrcPCFields[i], gbc);
        }

        //TLRC Res
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        tlrcPanel.add(tlrcResLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            tlrcPanel.add(tlrcResFields[i], gbc);
        }
        gbc.gridx = 0;
        gbc.gridy++;
        for (int i = 3; i < 6; i++) {
            gbc.gridx++;
            tlrcPanel.add(tlrcResFields[i], gbc);
        }
        gbc.gridx = 1;
        gbc.gridy++;
        tlrcPanel.add(tlrcResFields[6], gbc);

        //TLRC Dim
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        tlrcPanel.add(tlrcDimLabel, gbc);

        gbc.weightx = 1;
        for (int i = 0; i < 3; i++) {
            gbc.gridx++;
            tlrcPanel.add(tlrcDimFields[i], gbc);
        }

        JPanel buttonPanel = new JPanel();

        loadButton = new JButton("Load");
        loadButton.addActionListener(this);
        loadButton.setActionCommand("loadTal");
        loadButton.setFont(serif12B);
        saveButton = new JButton("Save");
        saveButton.addActionListener(this);
        saveButton.setActionCommand("saveTal");
        saveButton.setFont(serif12B);

        gbc.gridy = 0;
        gbc.gridx = 1;
        buttonPanel.add(loadButton, gbc);

        gbc.gridx = 2;
        buttonPanel.add(saveButton, gbc);

        isTLRCBox = new JCheckBox("Include Talairach", false);
        isTLRCBox.addActionListener(this);
        isTLRCBox.setActionCommand("tlrcSwitch");

        JPanel wholePanel = new JPanel();
        wholePanel.setLayout(new BoxLayout(wholePanel, BoxLayout.Y_AXIS));

        wholePanel.add(acpcPanel);
        wholePanel.add(isTLRCBox);
        wholePanel.add(tlrcPanel);
        wholePanel.add(buttonPanel);

        //populate the fields if information is present
        populateTalairachTab();

        JScrollPane scrollPane = new JScrollPane(wholePanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension(595, 200));

        return scrollPane;
    }

    /**
     * Looks at TalairachTransformInfo saved in ModelImage
     * and populates the talairach scrollpane with the
     * appropriate data
     */
    public void populateTalairachTab() {
        TalairachTransformInfo tInfo = image.getTalairachTransformInfo();

        if (tInfo != null) {
            origACFields[0].setText(Float.toString(tInfo.getOrigAC().x));
            origACFields[1].setText(Float.toString(tInfo.getOrigAC().y));
            origACFields[2].setText(Float.toString(tInfo.getOrigAC().z));

            origPCFields[0].setText(Float.toString(tInfo.getOrigPC().x));
            origPCFields[1].setText(Float.toString(tInfo.getOrigPC().y));
            origPCFields[2].setText(Float.toString(tInfo.getOrigPC().z));

            for (int i = 0; i < 3; i++) {
                origDimFields[i].setText(Integer.toString(tInfo.getOrigDim()[i]));
                origResFields[i].setText(Float.toString(tInfo.getOrigRes()[i]));
            }

            acpcACFields[0].setText(Float.toString(tInfo.getAcpcAC().x));
            acpcACFields[1].setText(Float.toString(tInfo.getAcpcAC().y));
            acpcACFields[2].setText(Float.toString(tInfo.getAcpcAC().z));

            acpcPCFields[0].setText(Float.toString(tInfo.getAcpcPC().x));
            acpcPCFields[1].setText(Float.toString(tInfo.getAcpcPC().y));
            acpcPCFields[2].setText(Float.toString(tInfo.getAcpcPC().z));

            acpcResField.setText(Float.toString(tInfo.getAcpcRes()));

            for (int i = 0; i < 3; i++) {
                acpcDimFields[i].setText(Integer.toString(tInfo.getAcpcDim()[i]));
            }

            for (int j = 0; j < 3; j++) {
                for (int i = 0; i < 3; i++) {
                    orientFields[ (j * 3) + i].setText(Float.toString(tInfo.getOrigOrient()[j][i]));
                }
            }

            if (tInfo.isTlrc()) {
                acpcMinFields[0].setText(Float.toString(tInfo.getAcpcMin().x));
                acpcMinFields[1].setText(Float.toString(tInfo.getAcpcMin().y));
                acpcMinFields[2].setText(Float.toString(tInfo.getAcpcMin().z));

                acpcMaxFields[0].setText(Float.toString(tInfo.getAcpcMax().x));
                acpcMaxFields[1].setText(Float.toString(tInfo.getAcpcMax().y));
                acpcMaxFields[2].setText(Float.toString(tInfo.getAcpcMax().z));

                for (int i = 0; i < 7; i++) {
                    tlrcResFields[i].setText(Float.toString(tInfo.getTlrcRes()[i]));
                }
            }

            //do these regardless b\c if AC is set, then these tlrc ones will be set as well
            tlrcACFields[0].setText(Float.toString(tInfo.getTlrcAC().x));
            tlrcACFields[1].setText(Float.toString(tInfo.getTlrcAC().y));
            tlrcACFields[2].setText(Float.toString(tInfo.getTlrcAC().z));

            tlrcPCFields[0].setText(Float.toString(tInfo.getTlrcPC().x));
            tlrcPCFields[1].setText(Float.toString(tInfo.getTlrcPC().y));
            tlrcPCFields[2].setText(Float.toString(tInfo.getTlrcPC().z));

            for (int i = 0; i < 3; i++) {
                tlrcDimFields[i].setText(Float.toString(tInfo.getTlrcDim()[i]));
            }

            isTLRCBox.setSelected(tInfo.isTlrc());
            actionPerformed(new ActionEvent(isTLRCBox, 0, "tlrcSwitch"));
        }

    }

    /**
     *   Builds the toolbar
     */
    private void buildToolBar() {

        tBar = new JToolBar();
        tBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        tBar.setBorder(BorderFactory.createEtchedBorder());
        tBar.setBorderPainted(true);

        JButton saveButton = new JButton(MipavUtil.getIcon("save.gif"));
        saveButton.addActionListener(this);
        saveButton.setToolTipText("Save history");
        saveButton.setActionCommand("SaveHistory");
        saveButton.setBorderPainted(false);
        saveButton.setRolloverEnabled(true);
        saveButton.setRolloverIcon(MipavUtil.getIcon("saverollover.gif"));
        saveButton.setBorder(BorderFactory.createLoweredBevelBorder());
        //saveButton.addItemListener(this);
        saveButton.setFocusPainted(false);
        tBar.add(saveButton);
        //tBar.add(makeSeparator());

        JButton newButton = new JButton(MipavUtil.getIcon("clear.gif"));
        newButton.addActionListener(this);
        newButton.setToolTipText("Clears the message area");
        newButton.setActionCommand("Clear");
        newButton.setBorderPainted(false);
        newButton.setRolloverEnabled(true);
        newButton.setRolloverIcon(MipavUtil.getIcon("clearroll.gif"));
        newButton.setMargin(new Insets(0, 0, 0, 0));
        tBar.add(newButton);

        JButton cutButton = new JButton(MipavUtil.getIcon("cutpaint.gif"));
        cutButton.addActionListener(this);
        cutButton.setToolTipText("Cuts selected text");
        cutButton.setActionCommand("Cut");
        cutButton.setBorderPainted(false);
        cutButton.setRolloverEnabled(true);
        cutButton.setRolloverIcon(MipavUtil.getIcon("cutpaintroll.gif"));
        cutButton.setMargin(new Insets(0, 0, 0, 0));
        tBar.add(cutButton);

        JButton copyButton = new JButton(MipavUtil.getIcon("copypaint.gif"));
        copyButton.addActionListener(this);
        copyButton.setToolTipText("Copies selected text");
        copyButton.setActionCommand("Copy");
        copyButton.setBorderPainted(false);
        copyButton.setRolloverEnabled(true);
        copyButton.setRolloverIcon(MipavUtil.getIcon("copypaintroll.gif"));
        copyButton.setMargin(new Insets(0, 0, 0, 0));
        tBar.add(copyButton);

        JButton pasteButton = new JButton(MipavUtil.getIcon("pastepaint.gif"));
        pasteButton.addActionListener(this);
        pasteButton.setToolTipText("Pastes text");
        pasteButton.setActionCommand("Paste");
        pasteButton.setBorderPainted(false);
        pasteButton.setRolloverEnabled(true);
        pasteButton.setRolloverIcon(MipavUtil.getIcon("pastepaintroll.gif"));
        pasteButton.setMargin(new Insets(0, 0, 0, 0));
        tBar.add(pasteButton);

        tBar.setFloatable(false);
    }

    /**
     *   Sets the variables appropriately from the GUI.
     *   @return Flag indicating successful set.
     */
    private boolean setVariables() {
        String tmpStr;
        float tmpResolutions[] = null;
        float tmpOrigin[] = null;
        int nDims = image.getNDims();

        newImageName = nameText.getText();

        modality = modalityBox.getSelectedIndex();

        switch (orientBox.getSelectedIndex()) {
            case 0:
                orient = FileInfoBase.AXIAL;
                break;
            case 1:
                orient = FileInfoBase.CORONAL;
                break;
            case 2:
                orient = FileInfoBase.SAGITTAL;
                break;
            case 3:
                orient = FileInfoBase.UNKNOWN_ORIENT;
                break;
            default:
                orient = FileInfoBase.UNKNOWN_ORIENT;
        }

        switch (orientationBox1.getSelectedIndex()) {
            case 0:
                orientAxis[0] = FileInfoBase.ORI_UNKNOWN_TYPE;
                break;
            case 1:
                orientAxis[0] = FileInfoBase.ORI_R2L_TYPE;
                break;
            case 2:
                orientAxis[0] = FileInfoBase.ORI_L2R_TYPE;
                break;
            case 3:
                orientAxis[0] = FileInfoBase.ORI_P2A_TYPE;
                break;
            case 4:
                orientAxis[0] = FileInfoBase.ORI_A2P_TYPE;
                break;
            case 5:
                orientAxis[0] = FileInfoBase.ORI_I2S_TYPE;
                break;
            case 6:
                orientAxis[0] = FileInfoBase.ORI_S2I_TYPE;
                break;
            default:
                orientAxis[0] = FileInfoBase.ORI_UNKNOWN_TYPE;
                break;
        }

        switch (orientationBox2.getSelectedIndex()) {
            case 0:
                orientAxis[1] = FileInfoBase.ORI_UNKNOWN_TYPE;
                break;
            case 1:
                orientAxis[1] = FileInfoBase.ORI_R2L_TYPE;
                break;
            case 2:
                orientAxis[1] = FileInfoBase.ORI_L2R_TYPE;
                break;
            case 3:
                orientAxis[1] = FileInfoBase.ORI_P2A_TYPE;
                break;
            case 4:
                orientAxis[1] = FileInfoBase.ORI_A2P_TYPE;
                break;
            case 5:
                orientAxis[1] = FileInfoBase.ORI_I2S_TYPE;
                break;
            case 6:
                orientAxis[1] = FileInfoBase.ORI_S2I_TYPE;
                break;
            default:
                orientAxis[1] = FileInfoBase.ORI_UNKNOWN_TYPE;
                break;
        }

        switch (orientationBox3.getSelectedIndex()) {
            case 0:
                orientAxis[2] = FileInfoBase.ORI_UNKNOWN_TYPE;
                break;
            case 1:
                orientAxis[2] = FileInfoBase.ORI_R2L_TYPE;
                break;
            case 2:
                orientAxis[2] = FileInfoBase.ORI_L2R_TYPE;
                break;
            case 3:
                orientAxis[2] = FileInfoBase.ORI_P2A_TYPE;
                break;
            case 4:
                orientAxis[2] = FileInfoBase.ORI_A2P_TYPE;
                break;
            case 5:
                orientAxis[2] = FileInfoBase.ORI_I2S_TYPE;
                break;
            case 6:
                orientAxis[2] = FileInfoBase.ORI_S2I_TYPE;
                break;
            default:
                orientAxis[2] = FileInfoBase.ORI_UNKNOWN_TYPE;
                break;
        }

        switch (comboBoxUnitOfMeasure1.getSelectedIndex()) {
            case 0:
                measure1 = FileInfoBase.UNKNOWN_MEASURE;
                break;
            case 1:
                measure1 = FileInfoBase.INCHES;
                break;
            case 2:
                measure1 = FileInfoBase.CENTIMETERS;
                break;
            case 3:
                measure1 = FileInfoBase.ANGSTROMS;
                break;
            case 4:
                measure1 = FileInfoBase.NANOMETERS;
                break;
            case 5:
                measure1 = FileInfoBase.MICROMETERS;
                break;
            case 6:
                measure1 = FileInfoBase.MILLIMETERS;
                break;
            case 7:
                measure1 = FileInfoBase.METERS;
                break;
            case 8:
                measure1 = FileInfoBase.KILOMETERS;
                break;
            case 9:
                measure1 = FileInfoBase.MILES;
                break;
            case 10:
                measure1 = FileInfoBase.NANOSEC;
                break;
            case 11:
                measure1 = FileInfoBase.MICROSEC;
                break;
            case 12:
                measure1 = FileInfoBase.MILLISEC;
                break;
            case 13:
                measure1 = FileInfoBase.SECONDS;
                break;
            case 14:
                measure1 = FileInfoBase.MINUTES;
                break;
            case 15:
                measure1 = FileInfoBase.HOURS;
                break;
            case 16:
                measure1 = FileInfoBase.HZ;
                break;
            default:
                measure1 = FileInfoBase.UNKNOWN_MEASURE;
        }

        if (nDims > 2) {
            switch (comboBoxUnitOfMeasure3.getSelectedIndex()) {
                case 0:
                    measure3 = FileInfoBase.UNKNOWN_MEASURE;
                    break;
                case 1:
                    measure3 = FileInfoBase.INCHES;
                    break;
                case 2:
                    measure3 = FileInfoBase.CENTIMETERS;
                    break;
                case 3:
                    measure3 = FileInfoBase.ANGSTROMS;
                    break;
                case 4:
                    measure3 = FileInfoBase.NANOMETERS;
                    break;
                case 5:
                    measure3 = FileInfoBase.MICROMETERS;
                    break;
                case 6:
                    measure3 = FileInfoBase.MILLIMETERS;
                    break;
                case 7:
                    measure3 = FileInfoBase.METERS;
                    break;
                case 8:
                    measure3 = FileInfoBase.KILOMETERS;
                    break;
                case 9:
                    measure3 = FileInfoBase.MILES;
                    break;
                case 10:
                    measure3 = FileInfoBase.NANOSEC;
                    break;
                case 11:
                    measure3 = FileInfoBase.MICROSEC;
                    break;
                case 12:
                    measure3 = FileInfoBase.MILLISEC;
                    break;
                case 13:
                    measure3 = FileInfoBase.SECONDS;
                    break;
                case 14:
                    measure3 = FileInfoBase.MINUTES;
                    break;
                case 15:
                    measure3 = FileInfoBase.HOURS;
                    break;
                case 16:
                    measure3 = FileInfoBase.HZ;
                    break;
                default:
                    measure3 = FileInfoBase.UNKNOWN_MEASURE;
            }
            if (nDims > 3) {
                switch (comboBoxUnitOfMeasure4.getSelectedIndex()) {
                    case 0:
                        measure4 = FileInfoBase.UNKNOWN_MEASURE;
                        break;
                    case 1:
                        measure4 = FileInfoBase.INCHES;
                        break;
                    case 2:
                        measure4 = FileInfoBase.CENTIMETERS;
                        break;
                    case 3:
                        measure4 = FileInfoBase.ANGSTROMS;
                        break;
                    case 4:
                        measure4 = FileInfoBase.NANOMETERS;
                        break;
                    case 5:
                        measure4 = FileInfoBase.MICROMETERS;
                        break;
                    case 6:
                        measure4 = FileInfoBase.MILLIMETERS;
                        break;
                    case 7:
                        measure4 = FileInfoBase.METERS;
                        break;
                    case 8:
                        measure4 = FileInfoBase.KILOMETERS;
                        break;
                    case 9:
                        measure4 = FileInfoBase.MILES;
                        break;
                    case 10:
                        measure4 = FileInfoBase.NANOSEC;
                        break;
                    case 11:
                        measure4 = FileInfoBase.MICROSEC;
                        break;
                    case 12:
                        measure4 = FileInfoBase.MILLISEC;
                        break;
                    case 13:
                        measure4 = FileInfoBase.SECONDS;
                        break;
                    case 14:
                        measure4 = FileInfoBase.MINUTES;
                        break;
                    case 15:
                        measure4 = FileInfoBase.HOURS;
                        break;
                    case 16:
                        measure4 = FileInfoBase.HZ;
                        break;
                    default:
                        measure4 = FileInfoBase.UNKNOWN_MEASURE;
                }

                if (nDims > 4) {
                    switch (comboBoxUnitOfMeasure5.getSelectedIndex()) {
                        case 0:
                            measure5 = FileInfoBase.UNKNOWN_MEASURE;
                            break;
                        case 1:
                            measure5 = FileInfoBase.INCHES;
                            break;
                        case 2:
                            measure5 = FileInfoBase.CENTIMETERS;
                            break;
                        case 3:
                            measure5 = FileInfoBase.ANGSTROMS;
                            break;
                        case 4:
                            measure5 = FileInfoBase.NANOMETERS;
                            break;
                        case 5:
                            measure5 = FileInfoBase.MICROMETERS;
                            break;
                        case 6:
                            measure5 = FileInfoBase.MILLIMETERS;
                            break;
                        case 7:
                            measure5 = FileInfoBase.METERS;
                            break;
                        case 8:
                            measure5 = FileInfoBase.KILOMETERS;
                            break;
                        case 9:
                            measure5 = FileInfoBase.MILES;
                            break;
                        case 10:
                            measure5 = FileInfoBase.NANOSEC;
                            break;
                        case 11:
                            measure5 = FileInfoBase.MICROSEC;
                            break;
                        case 12:
                            measure5 = FileInfoBase.MILLISEC;
                            break;
                        case 13:
                            measure5 = FileInfoBase.SECONDS;
                            break;
                        case 14:
                            measure5 = FileInfoBase.MINUTES;
                            break;
                        case 15:
                            measure5 = FileInfoBase.HOURS;
                            break;
                        case 16:
                            measure5 = FileInfoBase.HZ;
                            break;
                        default:
                            measure5 = FileInfoBase.UNKNOWN_MEASURE;
                    }
                }
            }
        }

        try {
            tmpResolutions = new float[5];
            resolutions = new float[nDims];

            tmpStr = textRes1.getText();
            if (testParameter(tmpStr, 0, 10000)) {
                tmpResolutions[0] = Double.valueOf(tmpStr).floatValue();
            }
            else {
                textRes1.requestFocus();
                textRes1.selectAll();
                return false;
            }

            tmpStr = textRes2.getText();
            if (testParameter(tmpStr, 0, 10000)) {
                tmpResolutions[1] = Double.valueOf(tmpStr).floatValue();
            }
            else {
                textRes2.requestFocus();
                textRes2.selectAll();
                return false;
            }

            tmpStr = textRes3.getText();
            if (testParameter(tmpStr, 0, 10000000)) {
                tmpResolutions[2] = Double.valueOf(tmpStr).floatValue();
            }
            else {
                textRes3.requestFocus();
                textRes3.selectAll();
                return false;
            }

            tmpStr = textRes4.getText();
            if (testParameter(tmpStr, 0, 10000)) {
                tmpResolutions[3] = Double.valueOf(tmpStr).floatValue();
            }
            else {
                textRes4.requestFocus();
                textRes4.selectAll();
                return false;
            }

            tmpStr = textRes5.getText();
            if (testParameter(tmpStr, 0, 10000)) {
                tmpResolutions[4] = Double.valueOf(tmpStr).floatValue();
            }
            else {
                textRes5.requestFocus();
                textRes5.selectAll();
                return false;
            }
            
            if (nDims > 2)
            {
            	try
            	{
            		sliceSpacing = Float.parseFloat(textSliceSpacing.getText());
            	}
            	catch (Exception e)
            	{
            		Preferences.debug("Failed to save slice spacing information.");
            	}
            }
        }
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("JDialogImageInfo: Out of memory");
        }

        for (int i = 0; i < nDims; i++) {
            resolutions[i] = tmpResolutions[i];
        }

        // Get start location information
        try {
            tmpOrigin = new float[4];

            if (nDims <= 4) {
                origin = new float[nDims];
            }
            else {
                origin = new float[4];
            }

            tmpStr = textSt1.getText();
            if (testParameter(tmpStr, -10000, 10000)) {
                tmpOrigin[0] = Double.valueOf(tmpStr).floatValue();
            }
            else {
                textSt1.requestFocus();
                textSt1.selectAll();
                return false;
            }

            tmpStr = textSt2.getText();
            if (testParameter(tmpStr, -10000, 10000)) {
                tmpOrigin[1] = Double.valueOf(tmpStr).floatValue();
            }
            else {
                textSt2.requestFocus();
                textSt2.selectAll();
                return false;
            }

            tmpStr = textSt3.getText();
            if (testParameter(tmpStr, -10000, 10000)) {
                tmpOrigin[2] = Double.valueOf(tmpStr).floatValue();
            }
            else {
                textSt3.requestFocus();
                textSt3.selectAll();
                return false;
            }

            tmpStr = textSt4.getText();
            if (testParameter(tmpStr, -10000, 10000)) {
                tmpOrigin[3] = Double.valueOf(tmpStr).floatValue();
            }
            else {
                textSt3.requestFocus();
                textSt3.selectAll();
                return false;
            }
        }
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("JDialogImageInfo: Out of memory");
        }

        int end = nDims;
        if (nDims == 5)
            end = 4;
        for (int i = 0; i < end; i++) {
            origin[i] = tmpOrigin[i];
        }

        // Get Transformation matrix information
        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[0].length; j++) {
                try {
                    matrix[i][j] = Double.parseDouble(textMatrix[i][j].getText());
                }
                catch (NumberFormatException e) {
                    MipavUtil.displayError("Transform matrix must contain numbers.");
                    return false;
                }
            }
        }
        return true;
    }

    /**
     *	Builds the ComboBox panel editing units of measure.
     *	@return	The combo box panel.
     */
    private JPanel buildComboBox() {
        JPanel comboPanel = new JPanel();
        comboPanel.setBorder(buildTitledBorder("Unit of measure"));

        comboPanel.setLayout(new BoxLayout(comboPanel, BoxLayout.Y_AXIS));
        int index;

        comboBoxUnitOfMeasure1 = new JComboBox();
        setComboBox(comboBoxUnitOfMeasure1);
        comboBoxUnitOfMeasure1.setAlignmentX(Component.LEFT_ALIGNMENT);
        index = image.getFileInfo()[0].getUnitsOfMeasure(0);
        setIndex(comboBoxUnitOfMeasure1, index);
        comboBoxUnitOfMeasure1.setEnabled(true);
        comboPanel.add(comboBoxUnitOfMeasure1);
        comboPanel.add(Box.createVerticalStrut(17));

        comboBoxUnitOfMeasure3 = new JComboBox();
        comboBoxUnitOfMeasure3.setAlignmentX(Component.LEFT_ALIGNMENT);
        setComboBox(comboBoxUnitOfMeasure3);
        index = image.getFileInfo()[0].getUnitsOfMeasure(2);

        if (image.getNDims() >= 3) {
            setIndex(comboBoxUnitOfMeasure3, index);
            comboBoxUnitOfMeasure3.setEnabled(true);
        }
        else {
            comboBoxUnitOfMeasure3.setEnabled(false);
        }
        comboPanel.add(comboBoxUnitOfMeasure3);
        comboPanel.add(Box.createVerticalStrut(5));

        comboBoxUnitOfMeasure4 = new JComboBox();
        setComboBox(comboBoxUnitOfMeasure4);
        comboBoxUnitOfMeasure4.setAlignmentX(Component.LEFT_ALIGNMENT);
        index = image.getFileInfo()[0].getUnitsOfMeasure(3);

        if (image.getNDims() >= 4) {
            setIndex(comboBoxUnitOfMeasure4, index);
            comboBoxUnitOfMeasure4.setEnabled(true);
        }
        else {
            comboBoxUnitOfMeasure4.setEnabled(false);
        }
        comboPanel.add(comboBoxUnitOfMeasure4);
        comboPanel.add(Box.createVerticalStrut(5));

        comboBoxUnitOfMeasure5 = new JComboBox();
        setComboBox(comboBoxUnitOfMeasure5);
        comboBoxUnitOfMeasure5.setAlignmentX(Component.LEFT_ALIGNMENT);
        index = image.getFileInfo()[0].getUnitsOfMeasure(4);
        if (image.getNDims() == 5) {
            setIndex(comboBoxUnitOfMeasure5, index);
            comboBoxUnitOfMeasure5.setEnabled(true);
        }
        else {
            comboBoxUnitOfMeasure5.setEnabled(false);
        }
        comboPanel.add(comboBoxUnitOfMeasure5);

        return comboPanel;
    }

    /**
     * Builds the panel for displaying/editing history
     * of image
     * @return history panel
     */
    private JPanel buildHistoryPanel() {
        JPanel historyPanel = new JPanel(new BorderLayout());

        buildToolBar();
        historyPanel.add(tBar, BorderLayout.NORTH);
        historyPanel.add(image.getHistoryPane(), BorderLayout.CENTER);

        return historyPanel;
    }

    /**
     *	Builds the panel usd in the tabbed pane "transform"
     *   as appropriate for the number of dimensions of the image.
     *   @return The newly created matrix panel.
     */
    private JPanel buildMatrixPanel() {
        double mat[][] = image.getMatrix().getMatrix();

        textMatrix = new JTextField[mat.length][mat[0].length];
        JPanel transformPanel = new JPanel(new BorderLayout());

        JPanel tPanel = new JPanel();
        tPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.insets = new Insets(5, 5, 5, 5);

        JLabel transformIDLabel = new JLabel("Transform ID:");
        transformIDLabel.setFont(serif12);
        transformIDBox = new JComboBox(FileInfoBase.getTransformIDStr());
        transformIDBox.setBackground(Color.white);
        transformIDBox.setFont(MipavUtil.font12);
        transformIDBox.setSelectedIndex(image.getFileInfo()[0].getTransformID());

        //add the transform ID here
        gbc.gridx = 0;
        gbc.gridy = 0;
        tPanel.add(transformIDLabel, gbc);

        gbc.gridx = 1;
        gbc.gridwidth = 2;
        tPanel.add(transformIDBox, gbc);
        gbc.gridwidth = 1;

        for (int i = 0; i < mat.length; i++) {
            for (int j = 0; j < mat[0].length; j++) {
                gbc.gridx = j;
                gbc.gridy = i + 1;
                textMatrix[i][j] = new JTextField(Double.toString(mat[i][j]), 5);
                textMatrix[i][j].setHorizontalAlignment(JTextField.LEFT);
                textMatrix[i][j].setCaretPosition(0);
                MipavUtil.makeNumericsOnly(textMatrix[i][j], true, true);
                tPanel.add(textMatrix[i][j], gbc);
            }
        }

        transformPanel.add(tPanel, BorderLayout.CENTER);

        JPanel buttonPanel = new JPanel();
        JButton loadFromFile = new JButton("Load");
        loadFromFile.addActionListener(this);
        loadFromFile.setPreferredSize(MipavUtil.defaultButtonSize);
        loadFromFile.setFont(serif12B);
        buttonPanel.add(loadFromFile);

        JButton saveToFile = new JButton("Save");
        saveToFile.addActionListener(this);
        saveToFile.setPreferredSize(MipavUtil.defaultButtonSize);
        saveToFile.setFont(serif12B);
        buttonPanel.add(saveToFile);

        JButton identityMatrix = new JButton("Identity");
        identityMatrix.addActionListener(this);
        identityMatrix.setPreferredSize(MipavUtil.defaultButtonSize);
        identityMatrix.setFont(serif12B);
        buttonPanel.add(identityMatrix);

        JButton invertMatrix = new JButton("Invert");
        invertMatrix.addActionListener(this);
        invertMatrix.setPreferredSize(MipavUtil.defaultButtonSize);
        invertMatrix.setFont(serif12B);
        buttonPanel.add(invertMatrix);

        JButton compositeMatrix = new JButton("Composite");
        compositeMatrix.addActionListener(this);
        compositeMatrix.setPreferredSize(new Dimension(100, 30));
        compositeMatrix.setFont(serif12B);
        buttonPanel.add(compositeMatrix);

        JButton decomposeMatrix = new JButton("Decompose");
        decomposeMatrix.addActionListener(this);
        decomposeMatrix.setPreferredSize(new Dimension(100, 30));
        decomposeMatrix.setFont(serif12B);
        buttonPanel.add(decomposeMatrix);

        transformPanel.add(buttonPanel, BorderLayout.SOUTH);
        return transformPanel;
    }

    /**
     *   Sets combo box choices that match resolution units listed in FileInfoBase
     *   and in the same order.
     *   @param  cBox  Combo box to setup to display the units.
     */
    private void setComboBox(JComboBox cBox) {

        cBox.setFont(serif12);
        cBox.setBackground(Color.white);
        cBox.addItem(" UNKNOWN");
        cBox.addItem(" INCHES ");
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
    }

    /**
     *   When Apply button is pressed, applies changes to all three areas: image name,
     *   resolutions, and transformation matrix.  When OK button is pressed, applies changes
     *   and closes dialog box.  When Cancel button is pressed, closes dialog without making
     *   any additional changes.
     *   @param event      Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK") || command.equals("Apply")) {
            // System.err.println("image hist pane size: " + image.getHistoryPane().getSize());
            if (setVariables()) {
                if (!newImageName.equals(image.getImageName())) {
                    image.updateFileName(newImageName);
                }
                updateImageModality();
                updateImageOrientation();

                //only update the resolutions if the tab is selected
                // otherwise might do an apply to all for specific slice/time resolutions
                if (tabbedPane.getSelectedIndex() == 1) {
                    updateResolInfo();
                }

                //only update (save) talairach info if ON THE TALAIRACH TAB
                if (tabbedPane.getSelectedIndex() == 6) {
                    updateTalairachInfo();
                }
                updateOriginInfo();
                updateTransformInfo();
                updateTransformIDInfo();

                if (linkedImageField != null) {
                    updateXMLLinkedFile();
                }

                if (command.equals("OK")) {
                    dispose();
                }
            }
        } else if (command.equals("BrowseLinked")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Select linked image");
            if (new File(linkedImageField.getText()).exists()) {
                chooser.setCurrentDirectory(new File(linkedImageField.getText()));
            } else {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            }

            int returnValue = chooser.showOpenDialog(this);

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                linkedImageField.setText(chooser.getSelectedFile().getPath());
            }

        }
        else if (command.equals("Load")) {
            TransMatrix result;
            /*
                   image.readTransformMatrix(false);
                   double mat[][] = image.getMatrix().getMatrix();
                   for (int i = 0; i < mat.length; i++) {
              for (int j = 0; j < mat[0].length; j++) {
                textMatrix[i][j].setText(Double.toString(mat[i][j]));
                textMatrix[i][j].setCaretPosition(0);
              }
                   }
                   validate();
             */
            matrixFile = matrixFileMenu();
            if (matrixFile != null) {
                result = reorientCoordSystem(fileTransMatrix);
                ( (ViewJFrameImage) parentFrame).getUserInterface().setGlobalDataText("Matrix loaded from Image info dialog:\n");
                ( (ViewJFrameImage) parentFrame).getUserInterface().setGlobalDataText(result.toString());
                double mat[][] = image.getMatrix().getMatrix();
                for (int i = 0; i < mat.length; i++) {
                    for (int j = 0; j < mat[0].length; j++) {
                        textMatrix[i][j].setText(Double.toString(result.get(i, j)));
                        textMatrix[i][j].setCaretPosition(0);
                    }
                }
                validate();
            }
        }
        else if (command.equals("loadTal")) {
            JFileChooser chooser = new JFileChooser(image.getUserInterface().getDefaultDirectory());
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setDialogTitle("Select talairach transform file");

            int returnVal = chooser.showDialog(this, "Open");
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                TalairachTransformInfo tInfo = image.getTalairachTransformInfo();
                if (tInfo == null) {
                    tInfo = new TalairachTransformInfo();
                }
                tInfo.readFromFile(chooser.getSelectedFile().getPath());
                image.setTalairachTransformInfo(tInfo);
                populateTalairachTab();
            }
            else {
                return;
            }

        }
        else if (command.equals("saveTal")) {
            JFileChooser chooser = new JFileChooser(image.getUserInterface().getDefaultDirectory());
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.setDialogTitle("Select talairach transform file");

            int returnVal = chooser.showDialog(this, "Save");
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                updateTalairachInfo();
                image.getTalairachTransformInfo().writeToFile(chooser.getSelectedFile().getPath());
            }
            else {
                return;
            }

        }
        else if (command.equals("tlrcSwitch")) {
            boolean en = isTLRCBox.isSelected();

            for (int i = 0; i < 3; i++) {
                acpcMinFields[i].setEnabled(en);
                acpcMaxFields[i].setEnabled(en);
                tlrcACFields[i].setEnabled(en);
                tlrcPCFields[i].setEnabled(en);
                tlrcDimFields[i].setEnabled(en);
            }
            for (int i = 0; i < 7; i++) {
                tlrcResFields[i].setEnabled(en);
            }

        }
        else if (command.equals("Save")) {
            image.saveTransformMatrix(); // opens dialog
        }
        else if (command.equals("SaveHistory")) {
            String fileName = "", directory = "";

            JFileChooser chooser = new JFileChooser();
            if (image.getUserInterface().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(image.getUserInterface().getDefaultDirectory()));
            }
            else
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty(
                    "user.dir")));
            int returnValue = chooser.showSaveDialog(this);

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = chooser.getCurrentDirectory().toString() +
                    File.separatorChar;
                image.getUserInterface().setDefaultDirectory(chooser.getCurrentDirectory().toString());
            }
            else
                return;

            try {
                BufferedWriter br = new BufferedWriter(new FileWriter(directory +
                                                                      fileName));
                image.getHistoryArea().write(br);
                br.flush();
                br.close();
            }
            catch (IOException error) {
                MipavUtil.displayError("Error writing history file");
            }

        }
        else if (command.equals("Clear")) {
            image.getHistoryArea().setText("");
        }
        else if (command.equals("Copy")) {
            image.getHistoryArea().copy();
        }
        else if (command.equals("Cut")) {
            image.getHistoryArea().cut();
        }
        else if (command.equals("Paste")) {
            image.getHistoryArea().paste();
        }
        else if (command.equals("Invert")) {
            image.getMatrix().invert();
            double mat[][] = image.getMatrix().getMatrix();
            for (int i = 0; i < mat.length; i++) {
                for (int j = 0; j < mat[0].length; j++) {
                    textMatrix[i][j].setText(Double.toString(mat[i][j]));
                    textMatrix[i][j].setCaretPosition(0);
                }
            }
            validate();
        }
        else if (command.equals("Identity")) {
            image.getMatrix().identity();
            double mat[][] = image.getMatrix().getMatrix();
            for (int i = 0; i < mat.length; i++) {
                for (int j = 0; j < mat[0].length; j++) {
                    textMatrix[i][j].setText(Double.toString(mat[i][j]));
                }
            }
            validate();
        }
        else if (command.equals("Composite")) {
            image.readTransformMatrix(true);
            double mat[][] = image.getMatrix().getMatrix();
            for (int i = 0; i < mat.length; i++) {
                for (int j = 0; j < mat[0].length; j++) {
                    textMatrix[i][j].setText(Double.toString(mat[i][j]));
                    textMatrix[i][j].setCaretPosition(0);
                }
            }
            validate();
        }
        else if (command.equals("Decompose")) {

            TransMatrix m = image.getMatrix();
            m.decomposeMatrix(m);
            ( (ViewJFrameImage) parentFrame).getUserInterface().setGlobalDataText("\n\nRotation X: " + m.getRotateX() +
                                                                                  "   Rotation Y: " + m.getRotateY() +
                                                                                  "   Rotation Z: " + m.getRotateZ());
            ( (ViewJFrameImage) parentFrame).getUserInterface().setGlobalDataText("\nX scale: " + m.getScaleX() + "   Y scale: " +
                                                                                  m.getScaleY() + "   Z scale: " + m.getScaleZ());
            ( (ViewJFrameImage) parentFrame).getUserInterface().setGlobalDataText("\nTranslate X: " + m.getTranslateX() +
                                                                                  "   Translate Y: " + m.getTranslateY() +
                                                                                  "   Translate Z: " + m.getTranslateZ());
        }
        else if (command.equals("Close")) {
            dispose();
        }
    }

    /**
     * method to update the xml file infos with a new linked image path
     */
    private void updateXMLLinkedFile() {
        if (image.getFileInfo(0) instanceof FileInfoImageXML) {
            for (int x = 0; x < image.getFileInfo().length; x++) {
                ( (FileInfoImageXML) image.getFileInfo(x)).setLinkedImagePath(linkedImageField.getText());
            }
        }
        else {
            System.err.println("THIS IS NOT AN XML FILE!!!");
        }
    }

    /**
     *    Updates the image orientation.
     *    @param orient   The new orientation (ModelImage.AXIAL, ModelImage.CORONAL, etc.)
     */
    private void updateImageOrientation() {
        FileInfoBase fileInfo[];

        if (image.getNDims() == 2) {
            fileInfo = image.getFileInfo();
            fileInfo[0].setImageOrientation(orient);
        }
        else if (image.getNDims() == 3) {
            fileInfo = image.getFileInfo();
            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setImageOrientation(orient);
                fileInfo[i].setAxisOrientation(orientAxis);
            }
        }
        else if (image.getNDims() == 4) {
            fileInfo = image.getFileInfo();
            for (int i = 0; i < image.getExtents()[2] * image.getExtents()[3]; i++) {
                fileInfo[i].setImageOrientation(orient);
                fileInfo[i].setAxisOrientation(orientAxis);
            }
        }
        else if (image.getNDims() == 5) {
            fileInfo = image.getFileInfo();
            for (int i = 0;
                 i <
                 image.getExtents()[2] * image.getExtents()[3] * image.getExtents()[4];
                 i++) {
                fileInfo[i].setImageOrientation(orient);
                fileInfo[i].setAxisOrientation(orientAxis);
            }
        }
    }

    /**
     *    Updates the image modality.
     */
    private void updateImageModality() {
        FileInfoBase fileInfo[];

        if (image.getNDims() == 2) {
            fileInfo = image.getFileInfo();
            fileInfo[0].setModality(modality);
        }
        else if (image.getNDims() == 3) {
            fileInfo = image.getFileInfo();
            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setModality(modality);
            }
        }
        else if (image.getNDims() == 4) {
            fileInfo = image.getFileInfo();
            for (int i = 0; i < image.getExtents()[2] * image.getExtents()[3]; i++) {
                fileInfo[i].setModality(modality);
            }
        }
        else if (image.getNDims() == 5) {
            fileInfo = image.getFileInfo();
            for (int i = 0;
                 i <
                 image.getExtents()[2] * image.getExtents()[3] * image.getExtents()[4];
                 i++) {
                fileInfo[i].setModality(modality);
            }
        }
    }

    /**
     *    Gives the image new resolutions.
     */
    private void updateResolInfo() {
        FileInfoBase fileInfo[];

        if (image.getNDims() == 2) {
            fileInfo = image.getFileInfo();
            fileInfo[0].setResolutions(resolutions);
            fileInfo[0].setUnitsOfMeasure(measure1, 0);
            fileInfo[0].setUnitsOfMeasure(measure1, 1);
            if (fileInfo[0].getFileFormat() == FileBase.DICOM) {
                String s = String.valueOf(resolutions[0]) + "\\" +
                    String.valueOf(resolutions[1]);
                ( (FileInfoDicom) (fileInfo[0])).setValue("0028,0030", s, s.length());
            }
        }
        else if (image.getNDims() == 3) {
            fileInfo = image.getFileInfo();
            

            for (int i = 0; i < image.getExtents()[2]; i++) {
                if (resolutionBox.isSelected()) {
                    fileInfo[i].setResolutions(resolutions);
                }
                fileInfo[i].setUnitsOfMeasure(measure1, 0);
                fileInfo[i].setUnitsOfMeasure(measure1, 1);
                fileInfo[i].setUnitsOfMeasure(measure3, 2);
                fileInfo[i].setSliceSpacing(sliceSpacing);
                if (fileInfo[i].getFileFormat() == FileBase.DICOM) {
                    if ( ( (FileInfoDicom) (fileInfo[i])).getValue("0018,0088") != null)
                        ( (FileInfoDicom) (fileInfo[i])).setValue("0018,0088",
                                                                  String.valueOf(resolutions[2]),
                                                                  String.valueOf(resolutions[2]).length());
                    if ( ( (FileInfoDicom) (fileInfo[i])).getValue("0018,0050") != null)
                        ( (FileInfoDicom) (fileInfo[i])).setValue("0018,0050",
                                                                  String.valueOf(resolutions[2]),
                                                                  String.valueOf(resolutions[2]).length());
                    String s = String.valueOf(resolutions[0]) + "\\" +
                        String.valueOf(resolutions[1]);
                    ( (FileInfoDicom) (fileInfo[i])).setValue("0028,0030", s, s.length());
                }
            }
            if (!resolutionBox.isSelected()) {
                fileInfo[resIndex].setResolutions(resolutions);
            }
        }
        else if (image.getNDims() == 4) {
            fileInfo = image.getFileInfo();

            for (int i = 0; i < image.getExtents()[2] * image.getExtents()[3]; i++) {
                fileInfo[i].setUnitsOfMeasure(measure1, 0);
                fileInfo[i].setUnitsOfMeasure(measure1, 1);
                fileInfo[i].setUnitsOfMeasure(measure3, 2);
                fileInfo[i].setUnitsOfMeasure(measure4, 3);
                fileInfo[i].setSliceSpacing(sliceSpacing);
                if (resolutionBox.isSelected()) {
                    fileInfo[i].setResolutions(resolutions);
                }
            }
            if (!resolutionBox.isSelected()) {
                fileInfo[resIndex].setResolutions(resolutions);
            }
        }
        else if (image.getNDims() == 5) {
            fileInfo = image.getFileInfo();
            for (int i = 0; i < image.getExtents()[2] * image.getExtents()[3] * image.getExtents()[4]; i++) {
                fileInfo[i].setUnitsOfMeasure(measure1, 0);
                fileInfo[i].setUnitsOfMeasure(measure1, 1);
                fileInfo[i].setUnitsOfMeasure(measure3, 2);
                fileInfo[i].setUnitsOfMeasure(measure4, 3);
                fileInfo[i].setUnitsOfMeasure(measure5, 4);
                fileInfo[i].setSliceSpacing(sliceSpacing);
                if (resolutionBox.isSelected()) {
                    fileInfo[i].setResolutions(resolutions);
                }
            }
            if (!resolutionBox.isSelected()) {
                fileInfo[resIndex].setResolutions(resolutions);
            }
        }
    }

    /**
     *    Updates the origin. Each image has a fileinfo where the origin
     *    are stored. Note that the start location for the Z (3rd) dimension change with
     *    the change is the slice. The origin is in the upper left corner and we are using
     *    the right hand rule. + x -> left to right;  + y -> top to bottom and + z -> into
     *    screen.
     */
    private void updateOriginInfo() {
        FileInfoBase fileInfo[];
        int axisOrient;

        if (image.getNDims() == 2) {
            fileInfo = image.getFileInfo();
            fileInfo[0].setOrigin(origin);
        }
        else if (image.getNDims() == 3) {
            fileInfo = image.getFileInfo();
            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setOrigin(origin);
                axisOrient = fileInfo[i].getAxisOrientation(2);
                if (axisOrient == FileInfoBase.ORI_R2L_TYPE ||
                    axisOrient == FileInfoBase.ORI_P2A_TYPE ||
                    axisOrient == FileInfoBase.ORI_I2S_TYPE)
                    origin[2] += resolutions[2];
                else // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                    origin[2] -= resolutions[2];
            }
        }
        else if (image.getNDims() == 4) {
            fileInfo = image.getFileInfo();
            float tmp = origin[2];
            for (int i = 0; i < image.getExtents()[3]; i++) {
                for (int j = 0; j < image.getExtents()[2]; j++) {
                    fileInfo[i * image.getExtents()[2] + j].setOrigin(origin);
                    axisOrient = fileInfo[i].getAxisOrientation(2);
                    if (axisOrient == FileInfoBase.ORI_R2L_TYPE ||
                        axisOrient == FileInfoBase.ORI_P2A_TYPE ||
                        axisOrient == FileInfoBase.ORI_I2S_TYPE)
                        origin[2] += resolutions[2];
                    else // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                        origin[2] -= resolutions[2];
                }
                origin[3] += resolutions[3];
                origin[2] = tmp;
            }
        }
        /*else if (image.getNDims() == 5) {
          fileInfo = image.getFileInfo();
          for (int i = 0;
               i <
               image.getExtents()[2] * image.getExtents()[3] * image.getExtents()[4];
               i++) {
            fileInfo[i].setStartLocations(startLocs);
            startLocs[4] += resolutions[4];
          }
             }*/
    }

    /**
     *   Applies the values in the JTabbedPane "Transform" to the
     *   transform matrix in the image.  Note that there are no
     *   visual changes made to the image itself.
     */
    private void updateTransformInfo() {
        double mat[][] = image.getMatrix().getMatrix();
        for (int i = 0; i < mat.length; i++) {
            for (int j = 0; j < mat[0].length; j++) {
                mat[i][j] = matrix[i][j];
            }
        }
    }

    /**
     * updates the talairach transform info
     */
    private void updateTalairachInfo() {
        TalairachTransformInfo tInfo = image.getTalairachTransformInfo();

        if (tInfo == null) {
            tInfo = new TalairachTransformInfo();
        }

        try {
            tInfo.isAcpc(true);
            tInfo.setOrigAC(new Point3Df(Float.parseFloat(origACFields[0].getText()),
                                         Float.parseFloat(origACFields[1].getText()),
                                         Float.parseFloat(origACFields[2].getText())));

            tInfo.setOrigPC(new Point3Df(Float.parseFloat(origPCFields[0].getText()),
                                         Float.parseFloat(origPCFields[1].getText()),
                                         Float.parseFloat(origPCFields[2].getText())));

            int[] origDim = new int[3];
            float[] origRes = new float[3];
            for (int i = 0; i < 3; i++) {
                origDim[i] = Integer.parseInt(origDimFields[i].getText());
                origRes[i] = Float.parseFloat(origResFields[i].getText());
            }

            tInfo.setOrigDim(origDim);
            tInfo.setOrigRes(origRes);

            tInfo.setAcpcPC(new Point3Df(Float.parseFloat(acpcPCFields[0].getText()),
                                         Float.parseFloat(acpcPCFields[1].getText()),
                                         Float.parseFloat(acpcPCFields[2].getText())));

            tInfo.setAcpcRes(Float.parseFloat(acpcResField.getText()));

            float[][] origOrient = new float[3][3];

            for (int j = 0; j < 3; j++) {
                for (int i = 0; i < 3; i++) {
                    origOrient[j][i] = Float.parseFloat(orientFields[ (j * 3) + i].getText());
                }
            }
            tInfo.setOrigOrient(origOrient);

            tInfo.isTlrc(isTLRCBox.isEnabled());
            if (tInfo.isTlrc()) {

                tInfo.setAcpcMin(new Point3Df(Float.parseFloat(acpcMinFields[0].getText()),
                                              Float.parseFloat(acpcMinFields[1].getText()),
                                              Float.parseFloat(acpcMinFields[2].getText())));

                tInfo.setAcpcMax(new Point3Df(Float.parseFloat(acpcMaxFields[0].getText()),
                                              Float.parseFloat(acpcMaxFields[1].getText()),
                                              Float.parseFloat(acpcMaxFields[2].getText())));

                float[] tRes = new float[7];
                for (int i = 0; i < 7; i++) {
                    tRes[i] = Float.parseFloat(tlrcResFields[i].getText());
                }
                tInfo.setTlrcRes(tRes);

            }

            image.setTalairachTransformInfo(tInfo);

        }
        catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    /**
     * Updates the transform ID for all file infos
     */
    private void updateTransformIDInfo() {
        FileInfoBase fileInfo[];
        int transformID = transformIDBox.getSelectedIndex();
        if (image.getNDims() == 2) {
            fileInfo = image.getFileInfo();
            fileInfo[0].setTransformID(transformID);
        }
        else if (image.getNDims() == 3) {
            fileInfo = image.getFileInfo();
            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setTransformID(transformID);
            }
        }
        else if (image.getNDims() == 4) {
            fileInfo = image.getFileInfo();
            for (int i = 0; i < image.getExtents()[2] * image.getExtents()[3]; i++) {
                fileInfo[i].setTransformID(transformID);
            }
        }
        else if (image.getNDims() == 5) {
            fileInfo = image.getFileInfo();
            for (int i = 0;
                 i <
                 image.getExtents()[2] * image.getExtents()[3] * image.getExtents()[4];
                 i++) {
                fileInfo[i].setTransformID(transformID);
            }
        }
    }

    /**
     *   Sets the selected index of the combo box based on
     *   what was set in the file info.
     *   @param comboBox Combo box to set.
     *   @param index    Value read in the file info.
     */
    private void setIndex(JComboBox comboBox, int index) {

        switch (index) {
            case FileInfoBase.UNKNOWN_MEASURE:
                comboBox.setSelectedIndex(0);
                break;
            case FileInfoBase.INCHES:
                comboBox.setSelectedIndex(1);
                break;
            case FileInfoBase.CENTIMETERS:
                comboBox.setSelectedIndex(2);
                break;
            case FileInfoBase.ANGSTROMS:
                comboBox.setSelectedIndex(3);
                break;
            case FileInfoBase.NANOMETERS:
                comboBox.setSelectedIndex(4);
                break;
            case FileInfoBase.MICROMETERS:
                comboBox.setSelectedIndex(5);
                break;
            case FileInfoBase.MILLIMETERS:
                comboBox.setSelectedIndex(6);
                break;
            case FileInfoBase.METERS:
                comboBox.setSelectedIndex(7);
                break;
            case FileInfoBase.KILOMETERS:
                comboBox.setSelectedIndex(8);
                break;
            case FileInfoBase.MILES:
                comboBox.setSelectedIndex(9);
                break;
            case FileInfoBase.NANOSEC:
                comboBox.setSelectedIndex(10);
                break;
            case FileInfoBase.MICROSEC:
                comboBox.setSelectedIndex(11);
                break;
            case FileInfoBase.MILLISEC:
                comboBox.setSelectedIndex(12);
                break;
            case FileInfoBase.SECONDS:
                comboBox.setSelectedIndex(13);
                break;
            case FileInfoBase.MINUTES:
                comboBox.setSelectedIndex(14);
                break;
            case FileInfoBase.HOURS:
                comboBox.setSelectedIndex(15);
                break;
            case FileInfoBase.HZ:
                comboBox.setSelectedIndex(16);
                break;
            default:
                comboBox.setSelectedIndex(0);
        }

    }

    /**
     *   Allows the user to select matrix file.
     *   @return fileName
     */
    public String matrixFileMenu() {
        String fileName, directory;
        JFileChooser chooser;
        ViewUserInterface UI = image.getUserInterface();
        fileName = null;
        //bring up file dialog
        try {
            chooser = new JFileChooser();
            if (UI.getDefaultDirectory() != null)
                chooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            else
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty(
                    "user.dir")));

            chooser.addChoosableFileFilter(new ViewImageFileFilter(
                ViewImageFileFilter.MATRIX));
            int returnVal = chooser.showOpenDialog(UI.getMainFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) +
                    File.separatorChar;
                UI.setDefaultDirectory(directory);
            }
            else {
                return null;
            }
        }
        catch (OutOfMemoryError error) {
            MipavUtil.displayError(
                "Out of memory: JDialogTransform.displayMatrixFileMenu");
            return null;
        }
        readTransformMatrixFile(fileName);
        return fileName;
    }

    /**
     *   Reads a matrix from a file.
     *   @param fileName name of the matrix file.
     *   @return TransMatrix
     */
    public void readTransformMatrixFile(String fileName) {
        ViewUserInterface UI = image.getUserInterface();
        JDialogOrientMatrix diaglogMatrix = null;
        TransMatrix matrix = new TransMatrix(DIM + 1);
        matrix.identity();
        if (fileName == null) {
            MipavUtil.displayError("filename = null");
        }
        try {
            File file = new File(UI.getDefaultDirectory() + fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "r");
            matrix.readMatrix(raFile, false);
            raFile.close();
            fileTransMatrix = matrix;
            // We don't know the coordinate system that the transformation represents. Therefore
            // bring up a dialog where the user can ID the coordinate system changes (i.e.
            // world coordinate and/or the "left-hand" coordinate system!
            diaglogMatrix = new JDialogOrientMatrix(parentFrame, (JDialogBase)this);
        }
        catch (IOException error) {
            MipavUtil.displayError("Matrix read error");
            fileTransMatrix.identity();
        }
    }

    /**
     *   Reorient the matix to world and left-hand coordinate systems if required.
     *   Note at the moment the voxel resolutions are handled in the transformation
     *   algorithm. At some future point we should adjust for voxel resolutions in
     *   the transformation matrix - its faster.
     *
     *   @param matrix the matrix to be converted to the
     */
    private TransMatrix reorientCoordSystem(TransMatrix matrix) {
        try {
            TransMatrix mat = new TransMatrix(4);
            TransMatrix wcMatrix = new TransMatrix(4);
            TransMatrix rh_lhMatrix = new TransMatrix(4);
            double[][] dMat;

            dMat = rh_lhMatrix.getArray(); // Identity
            dMat[2][2] = -1; // R-> L or L -> R  p.223 Foley, Van Dam ...
            // 1  0  0  0
            // 0  1  0  0
            // 0  0 -1  0
            // 0  0  0  1

            dMat = wcMatrix.getArray(); // Identity
            dMat[1][1] = -1;
            dMat[2][2] = -1; // world coordinate
            // 1  0  0  0
            // 0 -1  0  0
            // 0  0 -1  0
            // 0  0  0  1

            Point3Df cPt = image.getImageCentermm();
            Point3Df cPtRS = resampleImage.getImageCentermm();

            if (wcSystem == true && leftHandSystem == true) {
                // change to both the "left-hand" and world coordinate system.
                mat.setTranslate(cPtRS.x, cPtRS.y, cPtRS.z);
                mat.timesEquals(rh_lhMatrix);
                mat.timesEquals(wcMatrix);
                mat.setTranslate( -cPtRS.x, -cPtRS.y, -cPtRS.z);

                mat.timesEquals(matrix);

                mat.setTranslate(cPt.x, cPt.y, cPt.z);
                mat.timesEquals(wcMatrix);
                mat.timesEquals(rh_lhMatrix);
                mat.setTranslate( -cPt.x, -cPt.y, -cPt.z);

                //mat.print();
                return mat;
            }
            else if (wcSystem == true) { // Change just to the world coordinate system

                mat.setTranslate(cPtRS.x, cPtRS.y, cPtRS.z);
                mat.timesEquals(wcMatrix);
                mat.setTranslate( -cPtRS.x, -cPtRS.y, -cPtRS.z);

                mat.timesEquals(matrix);

                mat.setTranslate(cPt.x, cPt.y, cPt.z);
                mat.timesEquals(wcMatrix);
                mat.setTranslate( -cPt.x, -cPt.y, -cPt.z);

                return mat;
            }
            else if (leftHandSystem == true) { // Change just to the "left-hand" system

                mat.setTranslate(cPtRS.x, cPtRS.y, cPtRS.z);
                mat.timesEquals(rh_lhMatrix);
                mat.setTranslate( -cPtRS.x, -cPtRS.y, -cPtRS.z);

                mat.timesEquals(matrix);

                mat.setTranslate(cPt.x, cPt.y, cPt.z);
                mat.timesEquals(rh_lhMatrix);
                mat.setTranslate( -cPt.x, -cPt.y, -cPt.z);

                return mat;
            }
            else {
                return matrix;
            }
        }
        catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: Orient matrix.");
            return matrix;
        }
    }

    /**
     *   If true change matrix to the world coordinate system.
     */
    private boolean wcSystem = false;

    /**
     *   If true change matrix to the left-hand coordinate system.
     */
    private boolean leftHandSystem = false;

    /**
     * Update the title bar and resolution information
     * @param z int z-dim
     * @param t int t-dim
     */
    public void setSlice(int z, int t) {
        String addTitle = "";
        if (image.getNDims() == 3) {
            resIndex = z;
            addTitle = Integer.toString(z + 1);
        }
        else if (image.getNDims() > 3) {
            resIndex = z * image.getExtents()[3] + t;
            addTitle = Integer.toString(z + 1) + " : " + Integer.toString(t + 1);
        }

        setTitle("Image Attributes: " + image.getImageName() + " " + addTitle);

        textRes1.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[0]));
        textRes2.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[1]));
        if (image.getNDims() > 2) {
            textRes3.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[2]));
        }

        if (image.getNDims() > 3) {
            textRes4.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[3]));
        }

        textRes5 = new JTextField(5);
        textRes5.setText("1");
        textRes5.setFont(serif12);
        textRes5.addFocusListener(this);
        if (image.getNDims() > 4) {
            textRes5.setText(String.valueOf(image.getFileInfo()[resIndex].getResolutions()[4]));
        }

    }

    /**
     *   Sets the world coordinate flag. If true, change matrix to the world coordinate system.
     */
    public void setWCSystem(boolean wcSys) {
        wcSystem = wcSys;
    }

    /**
     *   Sets the left-hand coordinate flag. If true, change matrix to the left-hand coordinate system.
     */
    public void setLeftHandSystem(boolean leftHandSys) {
        leftHandSystem = leftHandSys;
    }

    /**
     * Set the resolution tag in front view.
     */
    public void setResolutionTag() {
    	tabbedPane.setSelectedIndex(1);
    }
    
    public void setMatrix(double [][] newMatrix)
    {
    	try
    	{
	    	if (matrix != null && newMatrix != null && textMatrix != null)
	    	{
		    	for (int i = 0; i < matrix.length; i++)
		    	{
		    		for (int j = 0; j < matrix[i].length; j++)
		    		{
		    			matrix[i][j] = newMatrix[i][j];
		    			textMatrix[i][j].setText(String.valueOf(newMatrix[i][j]));
		    		}
		    	}
	    	}
	    	else
	    	{
	    		Preferences.debug("Failed to set new matrix in JDialogImageInfo.setMatrix()");
	    	}
    	}
    	catch (Exception e)
    	{
    		Preferences.debug("Failed to set new matrix in JDialogImageInfo.setMatrix()");
    		e.printStackTrace();
    	}
    }
}
