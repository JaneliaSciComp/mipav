package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;

import java.awt.event.*;
import java.awt.*;
import java.awt.image.*;
import java.util.*;

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
import javax.swing.border.*;

/**
 *
 *		@version    1.1 June 15, 1999
 *		@author     Matthew J. McAuliffe, Ph.D.
 *
 *       $Logfile: /mipav/src/gov/nih/mipav/view/dialogs/JDialogVOIStats.java $
 *       $Revision: 56 $
 *       $Date: 2/17/06 6:20p $
 */
public class JDialogVOIStats extends JDialogBase implements ItemListener,
        ChangeListener,
        FocusListener,
        UpdateVOISelectionListener,
        TreeSelectionListener {

    private JButton calcButton;
    private JButton applyButton;
    private JButton colorButton;
    private JTextField VOIName;
    private JTextField VOIThicknessField;
    
    private JCheckBox checkboxBoundingBox;
    private JCheckBox checkboxAdditiveOrSubtractive;
    private JCheckBox checkboxIncludeForProcessing;
    private JCheckBox checkboxBoundary;
    private JCheckBox checkboxVOIName;
    
    private JPanel statsPanel;

    private JCheckBox checkboxExclude;
    private JTextField textMin;
    private JTextField textMax;
    private JLabel labelMin;
    private JLabel labelMax;
    private JSlider opacitySlider;
    private JLabel current;

    private VOI voi;
    private ModelImage image;
    private JPanelStatisticsList listPanel;
    private AlgorithmVOIProps algoVOI;

    private JTextField seedValueTF;
    private short seedValue;

    private ViewJColorChooser colorChooser;
    private Color colorVOI;

    private JTree voiTree;
    private DefaultTreeModel voiModel;

    private JScrollPane voiTreePane;
    private JScrollPane voiContourPane;
    private JTextArea contourTextArea;

    private DefaultMutableTreeNode root;

    private boolean treeSelectionChange = false;

    private static Icon ICON_POLYGON = MipavUtil.getIcon("polygon.gif");
    private static Icon ICON_POLYLINE = MipavUtil.getIcon("polyline.gif");
    private static Icon ICON_POINT = MipavUtil.getIcon("pointROI.gif");
    private static Icon ICON_LINE = MipavUtil.getIcon("linear.gif");
    private static Icon ICON_MEDICAL_FRAME = MipavUtil.getIcon("med_frame.gif");
    private static Icon ICON_PROTRACTOR = MipavUtil.getIcon("protractor.gif");

    private boolean frameFollowsSelection = true;
    private JCheckBox followVOISelectionBox = null;
    private Border frameBorder = null;

    private VOITreePopup popup = null;

    private static Color background = new Color(100,100,100);

    /** Constructor for the JDialogVOIStats.
     *   <p>
     *   this class ought to listen for
     *   VOI updates, but we are having it
     *   implemented elsewhere.
     */
    public JDialogVOIStats(Frame theParentFrame, ModelImage img, VOI _voi) {
        super(theParentFrame, false);
        voi = _voi;
        image = img;

        init();
    }

    /**
     *   Sets up GUI components - buttons, checkboxes, sliders, etc.
     */
    private void init() {
        //setTitle("VOI Statistics");
        frameBorder = BorderFactory.createCompoundBorder(BorderFactory.
                createRaisedBevelBorder(),
                BorderFactory.createLoweredBevelBorder());

        JLabel labelName = new JLabel("Name of VOI:");
        labelName.setFont(serif12);
        labelName.setForeground(Color.black);

        JLabel labelColor = new JLabel("Color of VOI:");
        labelColor.setFont(serif12);
        labelColor.setForeground(Color.black);

        JLabel labelThickness = new JLabel("Thickness of VOI:");
        labelThickness.setFont(serif12);
        labelThickness.setForeground(Color.black);
        
        colorButton = new JButton();
        colorButton.setPreferredSize(new Dimension(25, 25));
        colorButton.setToolTipText("Change VOI color");
        colorButton.addActionListener(this);

        VOIName = new JTextField(15);
        VOIName.setFont(serif12);

        VOIThicknessField = new JTextField(3);
        VOIThicknessField.setFont(serif12);
        MipavUtil.makeNumericsOnly(VOIThicknessField, false);
        
        JPanel namePanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(5,5,5,5);
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        namePanel.add(labelName, gbc);

        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        namePanel.add(VOIName, gbc);

        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.NONE;
        namePanel.add(labelThickness, gbc);
        
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        namePanel.add(VOIThicknessField, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        namePanel.add(labelColor, gbc);

        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        namePanel.add(colorButton, gbc);

        gbc.insets = new Insets(0,0,0,0);

        checkboxBoundingBox = new JCheckBox("Show contour bounding box");
        checkboxBoundingBox.setFont(serif12);

        checkboxAdditiveOrSubtractive = new JCheckBox(
                "Use additive polarity for VOI");
        checkboxAdditiveOrSubtractive.setFont(serif12);

        checkboxIncludeForProcessing = new JCheckBox("Include for processing");
        checkboxIncludeForProcessing.setFont(serif12);

        checkboxBoundary = new JCheckBox("Display VOI shading");
        checkboxBoundary.setFont(serif12);
        checkboxBoundary.addItemListener(this);

        checkboxVOIName = new JCheckBox("Show VOI name");
        checkboxVOIName.setFont(serif12);
        checkboxVOIName.setSelected(Preferences.is(Preferences.PREF_SHOW_VOI_NAME));
        
        JPanel checkboxPanel = new JPanel();
        checkboxPanel.setLayout(new BoxLayout(checkboxPanel, BoxLayout.Y_AXIS));
        checkboxPanel.add(checkboxBoundingBox);
        checkboxPanel.add(checkboxAdditiveOrSubtractive);
        checkboxPanel.add(checkboxIncludeForProcessing);
        checkboxPanel.add(checkboxVOIName);
        checkboxPanel.add(checkboxBoundary);

        opacitySlider = new JSlider(JSlider.HORIZONTAL, 0, 100, 30);

        opacitySlider.setMajorTickSpacing(20);
        opacitySlider.setValue(30);
        opacitySlider.setPaintTicks(true);
        opacitySlider.setEnabled(false);
        opacitySlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf(1));
        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(opacitySlider.getValue() / 100.0f));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf(0));
        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        JPanel sliderPanel = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(opacitySlider, gbc);

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
        sliderPanel.setBorder(buildTitledBorder("Opacity"));

        JPanel panelVOIProps = new JPanel(new GridBagLayout());
        panelVOIProps.setBorder(buildTitledBorder("VOI properties"));
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        panelVOIProps.add(namePanel, gbc);
        gbc.gridy = 1;
        panelVOIProps.add(checkboxPanel, gbc);
        gbc.gridy = 2;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.NORTH; //gbc.fill = GridBagConstraints.BOTH;
        panelVOIProps.add(sliderPanel, gbc);

        listPanel = new JPanelStatisticsList();
        try {
            listPanel.setSliceCount(image.getExtents()[2]);
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            // otherwise, this must be a 2d image.
            listPanel.setSliceCount(1);
        } finally {
            listPanel.setCheckBoxesEnabled();
        }

        checkboxExclude = new JCheckBox("Exclude intensity range");
        checkboxExclude.setFont(serif12);
        checkboxExclude.addActionListener(this);

        labelMin = new JLabel("Range: ");
        labelMin.setFont(serif12);
        labelMin.setForeground(Color.black);
        labelMin.setEnabled(false);

        textMin = new JTextField(5);
        textMin.setEnabled(false);
        textMin.setFont(serif12);

        labelMax = new JLabel(" to ");
        labelMax.setFont(serif12);
        labelMax.setForeground(Color.black);
        labelMax.setEnabled(false);

        textMax = new JTextField(5);
        textMax.setEnabled(false);
        textMax.setFont(serif12);

        JPanel checkPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        checkPanel.add(checkboxExclude);

        JPanel rangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        rangePanel.add(labelMin);
        rangePanel.add(textMin);
        rangePanel.add(labelMax);
        rangePanel.add(textMax);

        JPanel intensityPanel = new JPanel();
        intensityPanel.setLayout(new BoxLayout(intensityPanel, BoxLayout.Y_AXIS));
        intensityPanel.add(checkPanel);
        intensityPanel.add(rangePanel);

        statsPanel = new JPanel(new BorderLayout());
        statsPanel.add(listPanel);
        statsPanel.add(intensityPanel, BorderLayout.SOUTH);

        JLabel labelSeed = new JLabel("Seed value (0-32K)");
        labelSeed.setFont(serif12);
        labelSeed.setForeground(Color.black);

        seedValueTF = new JTextField(5);
        seedValueTF.setFont(serif12);
        seedValueTF.addFocusListener(this);

        JPanel seedValuePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        seedValuePanel.setBorder(buildTitledBorder("Watershed seed value"));
        seedValuePanel.add(labelSeed);
        seedValuePanel.add(seedValueTF);

        JPanel calcPanel = new JPanel(new BorderLayout());
        calcPanel.add(statsPanel);
        calcPanel.add(seedValuePanel, BorderLayout.SOUTH);

        applyButton = new JButton("Apply");
        applyButton.setPreferredSize(MipavUtil.defaultButtonSize);
        applyButton.setFont(serif12B);
        applyButton.addActionListener(this);

        cancelButton = buildCancelButton();
        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);

        //build the VOI tree
        buildVOITree();
        buildVOIContourPane();

        GridBagConstraints gb = new GridBagConstraints();

        JPanel mainTreePanel = new JPanel(new GridBagLayout());
        mainTreePanel.setBorder(buildTitledBorder("VOI Tree"));

        gb.anchor = GridBagConstraints.CENTER;
        gb.gridx = 0; gbc.gridy = 0;
        gb.weightx = 1.0;
        gb.weighty = 1.0;
        gb.fill = GridBagConstraints.BOTH;

        mainTreePanel.add(voiTreePane, gb);

        JPanel treeOptionPanel = new JPanel(new BorderLayout());
        treeOptionPanel.setBorder(buildTitledBorder("Tree Options"));
        followVOISelectionBox = new JCheckBox("Frame follows VOI selection", true);
        followVOISelectionBox.setFont(MipavUtil.font12);
        followVOISelectionBox.addActionListener(this);
        followVOISelectionBox.setEnabled(image.getNDims() > 2);
        treeOptionPanel.add(followVOISelectionBox, BorderLayout.CENTER);


        gb.gridy = 1;
        gb.weightx = 1;
        gb.weighty = 0;
        gb.fill = GridBagConstraints.HORIZONTAL;
        mainTreePanel.add(treeOptionPanel, gb);

        gb.gridy = 2;
        gb.weightx = .5;
        gb.weighty = .5;
        gb.fill = GridBagConstraints.BOTH;
        mainTreePanel.add(voiContourPane, gb);

        JPanel leftButton = new JPanel();
        leftButton.add(applyButton);
        leftButton.add(cancelButton);

        JPanel leftWholePanel = new JPanel(new BorderLayout());
        leftWholePanel.add(panelVOIProps, BorderLayout.NORTH);
        leftWholePanel.add(mainTreePanel, BorderLayout.CENTER);

        JPanel leftPanel = new JPanel(new BorderLayout());
        leftPanel.add(leftWholePanel);
        leftPanel.add(leftButton, BorderLayout.SOUTH);

        calcButton = new JButton("Calculate");
        calcButton.setPreferredSize(new Dimension(100, 30));
        calcButton.setFont(serif12B);
        calcButton.addActionListener(this);

        JPanel rightButton = new JPanel();
        rightButton.add(calcButton);

        JPanel rightPanel = new JPanel(new BorderLayout());
        rightPanel.add(calcPanel);
        rightPanel.add(rightButton, BorderLayout.SOUTH);

        mainDialogPanel.setLayout(new GridBagLayout());
        gb.gridx = 0;
        gb.gridy = 0;
        gb.weightx = 1;
        gb.weighty = 1;
        gb.fill = GridBagConstraints.BOTH;
        mainDialogPanel.add(leftPanel, gb);

        gb.gridx = 1;
        mainDialogPanel.add(rightPanel, gb);


     //   mainDialogPanel.setLayout(new BorderLayout());
     //   mainDialogPanel.add(leftPanel, BorderLayout.WEST);
     //   mainDialogPanel.add(rightPanel);
        mainDialogPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(mainDialogPanel);
        pack();

    }

    private void buildVOIContourPane() {
        contourTextArea = new JTextArea();
        contourTextArea.setFont(MipavUtil.font10);
        contourTextArea.setEditable(false);

        voiContourPane = new JScrollPane(contourTextArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                         JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

    }

    private void buildVOITree() {

        ViewVOIVector VOIs = image.getVOIs();

        root = new DefaultMutableTreeNode(image.getImageName());
        voiModel = new DefaultTreeModel(root);

        Enumeration e = VOIs.elements();

        VOI currentVOI = null;

        int index = 0;
        while (e.hasMoreElements()) {
            currentVOI = (VOI) e.nextElement();
            voiModel.insertNodeInto(new VOIGroupNode(currentVOI), root, index);
            index++;
        }

        voiTree = new JTree(voiModel);
        voiTree.setCellRenderer(new VOITreeRenderer());

        voiTree.setFont(MipavUtil.font12);
        voiTree.addTreeSelectionListener(this);


        voiTreePane = new JScrollPane(voiTree, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                         JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

    }

    private void updateContourPane(VOIBase leadBase) {

        int i = 0;
        int size = leadBase.size();
        contourTextArea.setText("VOI name: " + leadBase.getName() + "\n");
        contourTextArea.append("contour name: " + leadBase.getLabel() + "\n");
        contourTextArea.append("number of points: " + leadBase.size() + "\n");

        String [] positions = null;
        Point3Df currentPt = null;
        int currentX, currentY, currentZ;
        if (image.getFileInfo(0).getOrigin()[0] != 0 ||
            image.getFileInfo(0).getOrigin()[1] != 0 ||
            image.getFileInfo(0).getOrigin()[2] != 0) {

            for (i = 0; i < size; i++){
                currentPt = (Point3Df)leadBase.elementAt(i);
                currentX = (int)currentPt.x;
                currentY = (int)currentPt.y;
                currentZ = (int)currentPt.z;

                if (((ViewJFrameImage) parentFrame).getComponentImage().getImageA().getNDims() > 2) {
                	positions = ((ViewJFrameImage) parentFrame).getComponentImage().
                	setScannerPosition((int)currentPt.x, (int)currentPt.z, (int)currentPt.z);
                }

                if (positions != null) {
                    for (i = 0; i < size; i++) {
                        currentPt = (Point3Df) leadBase.elementAt(i);
                        currentX = (int) currentPt.x;
                        currentY = (int) currentPt.y;
                        currentZ = (int) currentPt.z;

                        contourTextArea.append(" X: " + String.valueOf(currentX + 1) +
                                               " Y: " + String.valueOf(currentY + 1) +
                                               " Z: " + String.valueOf(currentZ + 1) +
                                               "  Position: " + positions[0] +
                                               " " + positions[1] + " " + positions[2] + "\n");



                    }
                } else {
                    for (i = 0; i < size; i++) {
                        currentPt = (Point3Df)leadBase.elementAt(i);
                        currentX = (int)currentPt.x;
                        currentY = (int)currentPt.y;
                        currentZ = (int)currentPt.z;

                        contourTextArea.append(" X: " + String.valueOf(currentX + 1) +
                                               " Y: " + String.valueOf(currentY + 1) +
                                               " Z: " + String.valueOf(currentZ + 1) + "\n");
                    }
                }

            }
        } else {

            for (i = 0; i < size; i++) {
                currentPt = (Point3Df)leadBase.elementAt(i);
                currentX = (int)currentPt.x;
                currentY = (int)currentPt.y;
                currentZ = (int)currentPt.z;

                contourTextArea.append(" X: " + String.valueOf(currentX + 1) +
                                       " Y: " + String.valueOf(currentY + 1) +
                                       " Z: " + String.valueOf(currentZ + 1) + "\n");
            }

        }


    //    for (i = 0; i < leadBase.size(); i++) {
      //      contourTextArea.append("\t");
    //    }

        contourTextArea.setCaretPosition(0);

        //System.err.println(contourTextArea.getText());
    }

    private void updateTree() {

        if (treeSelectionChange) {
            treeSelectionChange = false;
            return;
        }

        if (this.isVisible()) {

            root.removeAllChildren();
            ViewVOIVector VOIs = image.getVOIs();
            Enumeration e = VOIs.elements();

            VOI tempVOI = null;

            VOIGroupNode currentNode = null;
            Vector[] curves = null;

            int index = 0;

            Enumeration voiEnum = null;
            VOIBase voiBase = null;

            Enumeration voiNodeEnum = null;
            VOINode currentVOINode = null;
            VOIFrameNode currentVOIFrameNode = null;
            Enumeration voiFrameEnum = null;

            Vector treePaths = new Vector();

            TreeNode tempNode = null;

            //iterate through all VOIs
            while (e.hasMoreElements()) {
                tempVOI = (VOI) e.nextElement();

                //create VOI group node (for VOI)
                currentNode = new VOIGroupNode(tempVOI);

                //check to see if the current VOI is the VOI shown in this dialog
                // or if the VOI isActive (can have multiple selections on tree)
                if (tempVOI.isActive()) {

                    //add a new tree path so the VOI node is selected
                    treePaths.addElement(new TreePath(new Object[] {root,
                            currentNode}));

                    curves = tempVOI.getCurves();

                    //look through curves to find which of the VOI's VOIBases (contours etc)
                    // are active
                    for (index = 0; index < curves.length; index++) {

                        //iterate through Vector for VOIBases
                        voiEnum = curves[index].elements();
                        while (voiEnum.hasMoreElements()) {
                            voiBase = (VOIBase) voiEnum.nextElement();

                            //check to see if the VOIBase is active
                            if (voiBase.isActive()) {

                                voiFrameEnum = currentNode.children();
                                while (voiFrameEnum.hasMoreElements()) {

                                    tempNode = (TreeNode) voiFrameEnum.
                                               nextElement();
                                    if (tempNode instanceof VOIFrameNode) {

                                        voiNodeEnum = tempNode.children();

                                        //find the child that matches this selected contour
                                        while (voiNodeEnum.hasMoreElements()) {
                                            currentVOINode = (VOINode)
                                                    voiNodeEnum.nextElement();
                                            if (currentVOINode.getVOI().equals(
                                                    voiBase)) {
                                                treePaths.addElement(new
                                                        TreePath(new Object[] {
                                                        root, currentNode,
                                                        tempNode,
                                                        currentVOINode}));
                                            }
                                        }

                                    }
                                }

                            }
                        }
                    }
                }

                root.add(currentNode);
            }

            voiModel.reload();

            if (treePaths.size() > 0) {
                TreePath[] tPaths = new TreePath[treePaths.size()];

                for (int i = 0; i < tPaths.length; i++) {
                    tPaths[i] = (TreePath) treePaths.elementAt(i);
                }

                voiTree.setSelectionPaths(tPaths);
            } else {
                voiTree.setSelectionPath(new TreePath(root));
            }
            voiTreePane.validate();
        }

    }

    public void setVisible(boolean visible) {

        if (popup == null && visible) {
            popup = new VOITreePopup();
            voiTree.addMouseListener(popup);
        }

        super.setVisible(visible);
        if (visible) {
            updateTree();
        }
    }

    /**
     *   Updates the dialog based on the VOI passed in.
     *   @param _voi VOI whose properties we want to calculate.
     *   @param img  Image where voi is to be updated
     */
    public void updateVOI(VOI _voi, ModelImage img) {

        voi = _voi;
        image = img;

        if (voi != null) {

            if (voi.getBoundingBoxFlag() == true) {
                checkboxBoundingBox.setSelected(true);
            } else {
                checkboxBoundingBox.setSelected(false);
            }
            ViewUserInterface.getReference().setUseVOIName(checkboxVOIName.isSelected());
            
            if (voi.getPolarity() == voi.ADDITIVE) {
                checkboxAdditiveOrSubtractive.setSelected(true);
            } else {
                checkboxAdditiveOrSubtractive.setSelected(false);
            }
            checkboxIncludeForProcessing.setSelected(voi.getProcess());

            if (voi.getDisplayMode() == VOI.BOUNDARY) {
                checkboxBoundary.setSelected(false);
                opacitySlider.setEnabled(false);
            } else {
                checkboxBoundary.setSelected(true);
                opacitySlider.setEnabled(true);
            }

            //VOIName.setBackground(voi.getColor());
            colorButton.setBackground(voi.getColor());
            colorVOI = voi.getColor();

            seedValueTF.setText(String.valueOf(voi.getWatershedID()));

            VOIName.setText(voi.getName());
            setTitle("VOI Statistics - " + voi.getUID());

            VOIThicknessField.setText(new Integer(voi.getThickness()).toString());
            
            //turn things on/off depending on if a PolyLine is selected
            if (voi.getCurveType() == VOI.POLYLINE ||
                voi.getCurveType() == VOI.POLYLINE_SLICE ||
                voi.getCurveType() == VOI.POINT ||
                voi.getCurveType() == VOI.LINE ||
                voi.getCurveType() == VOI.PROTRACTOR) {

                listPanel.setCheckBoxesDisabled();
                calcButton.setEnabled(false);
                if (checkboxExclude.isSelected()) {
                    textMin.setEnabled(false);
                    textMax.setEnabled(false);
                }
                checkboxExclude.setEnabled(false);
                seedValueTF.setEnabled(false);
            } else {
                checkboxExclude.setEnabled(true);
                if (checkboxExclude.isSelected()) {
                    textMin.setEnabled(true);
                    textMax.setEnabled(true);
                }
                seedValueTF.setEnabled(true);
                calcButton.setEnabled(true);
                // on the asssumption that the image is a 3d image, this will work
                try {
                    listPanel.setSliceCount(img.getExtents()[2]);
                } catch (ArrayIndexOutOfBoundsException aioobe) {
                    // otherwise, this must be a 2d image.
                    listPanel.setSliceCount(1);
                } finally {

                    listPanel.setCheckBoxesEnabled();
                }
            }
        }
        validate();
    }

    /**
     *   Applies changes to VOI when "Apply" is pressed; closes when
     *   "Cancel" is pressed; and calculates statistics and outputs them
     *   to the message frame when "Calculate" is pressed.  Also brings
     *   up a color chooser when the color button is pressed.
     *   @param event    Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String tmpStr;
        int xUnits = image.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = image.getFileInfo(0).getUnitsOfMeasure()[1];
        int zUnits = FileInfoBase.UNKNOWN_MEASURE;
        if (image.getNDims() > 2) {
            zUnits = image.getFileInfo(0).getUnitsOfMeasure()[2];
        }
        String unitsString = null;

        if (source == colorButton) {
            colorChooser = new ViewJColorChooser(new Frame(),
                                                 "Pick VOI color",
                                                 new OkColorListener(),
                                                 new CancelListener());
        } else if (source == followVOISelectionBox) {
            frameFollowsSelection = followVOISelectionBox.isSelected();
        } else if (source == applyButton) {
            ViewVOIVector vectorVOI = image.getVOIs();
            ViewVOIVector newVOIVector;
            int temp[];
            int j = 0, location = -1;
            String name = "";
            boolean changedName = false;

            tmpStr = seedValueTF.getText();
            if (testParameter(tmpStr, 0, 32000)) {
                seedValue = Short.valueOf(tmpStr).shortValue();
                voi.setWatershedID(seedValue);
            }

            try {
                newVOIVector = new ViewVOIVector();
                temp = new int[100];
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError(
                        "Out of memory: JDialogVOIStats.actionPerformed");
                return;
            }

            for (int i = 0; i < vectorVOI.size(); i++) {
                if (((VOI) vectorVOI.elementAt(i)).getName().equals(VOIName.
                        getText()) &&
                    !((VOI) vectorVOI.elementAt(i)).equals(voi)) {
                    newVOIVector.addElement(vectorVOI.elementAt(i));
                    temp[j++] = i;
                    name = VOIName.getText();
                    changedName = true;
                }
                if (((VOI) vectorVOI.elementAt(i)).equals(voi)) {
                    location = i;
                }
            }
            voi.setName(VOIName.getText());

            boolean changedThickness = false;
            int thickChange = 1;
            try {
            	int thickness = voi.getThickness();
            	thickChange = Integer.parseInt(VOIThicknessField.getText());
            	if ((thickChange < 0 || thickChange > 20)) {
            		MipavUtil.displayWarning("VOI thickness must be greater than 0 and less than 20");
            	}else if (thickness != thickChange ) {
            		changedThickness = true;
            	} 
            } catch (Exception e) {
            	VOIThicknessField.setText("1");
            }
            
            if (changedThickness) {
            	voi.setThickness(thickChange);
            	Preferences.setProperty("VOIThickness", Integer.toString(thickChange));
            }
            
            
            if (checkboxBoundingBox.isSelected() == true) {
                voi.setBoundingBoxFlag(true);
            } else {
                voi.setBoundingBoxFlag(false);
            }

            if (checkboxAdditiveOrSubtractive.isSelected() == true) {
                voi.setPolarity(VOI.ADDITIVE);
            } else {
                voi.setPolarity(VOI.SUBTRACTIVE);
            }

            if (checkboxIncludeForProcessing.isSelected() == true) {
                voi.setProcess(true);
            } else {
                voi.setProcess(false);
            }

            if (checkboxBoundary.isSelected() == true) {
                voi.setDisplayMode(VOI.SOLID);
            } else {
                voi.setDisplayMode(VOI.BOUNDARY);
            }

            voi.setColor(colorVOI);
            if (parentFrame != null && parentFrame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) parentFrame).getControls().setVOIColor(colorVOI);
            }
            if (j != 0 && location != -1) {

                newVOIVector.addElement(voi);
                temp[j++] = location;

                int where[] = new int[j];
                for (int i = 0; i < j; i++)
                    where[i] = temp[i];
                image.groupVOIs(newVOIVector, where, name);
                Vector VOIs = image.getVOIs();
                updateVOI((VOI) (VOIs.elementAt(VOIs.size() - 1)), image);
            } else {
                updateVOI(voi, image);
            }
            updateTree();
            image.notifyImageDisplayListeners(null, true);
        } else if (source == calcButton) {
            if (textMin.isEnabled()) {
                String tempStr = textMin.getText();
                if (testParameter(tempStr, -10000000, 10000000)) {
                    voi.setMinimumIgnore(Float.valueOf(tempStr).floatValue());
                } else {
                    textMin.requestFocus();
                    textMin.selectAll();
                    return;
                }
                tempStr = textMax.getText();
                if (testParameter(tempStr, -10000000, 10000000)) {
                    voi.setMaximumIgnore(Float.valueOf(tempStr).floatValue());
                } else {
                    textMax.requestFocus();
                    textMax.selectAll();
                    return;
                }
            } else {
                voi.setMinimumIgnore(Float.MAX_VALUE);
                voi.setMaximumIgnore(Float.MIN_VALUE);
            }

            voi.setStatisticList(listPanel.getViewList());
            // only loading the image works because we have been changing
            // the thing held BY the image.
            algoVOI = new AlgorithmVOIProps(image);
            algoVOI.setRunningInSeparateThread(false);
            algoVOI.run();
            if (!algoVOI.isCompleted()) {
                MipavUtil.displayError("Please make sure VOI is selected.");
                return;
            }
            voi.setActive(true);
            ViewUserInterface UI = ((ViewJFrameImage) (parentFrame)).
                                   getUserInterface();
            UI.setDataText("\n -----------------------------------------------------------------------------\n");
            //UI.setDataText("Image:     " + image.getFileInfo(0).getFileName() + "\n");
            UI.setDataText("Image:     " + image.getImageName() + "\n");
            UI.setDataText("VOI  :     " + voi.getName() + "\n");
            int measure;
            String str = new String();

            /*
                         if ( image.isColorImage() ) {
             if (algoVOI.getMaxIntensityRed() > algoVOI.getMinIntensityRed()) {
             UI.setDataText("  Red min: " + algoVOI.getMinIntensityRed() + " \t\tRed max: " +
                 algoVOI.getMaxIntensityRed() + "\n");
                }
             if (algoVOI.getMaxIntensityGreen() > algoVOI.getMinIntensityGreen()) {
                    UI.setDataText("  Green min: " + algoVOI.getMinIntensityGreen() + " \tGreen max: " +
                 algoVOI.getMaxIntensityGreen() + "\n");
                }
             if (algoVOI.getMaxIntensityBlue() > algoVOI.getMinIntensityBlue()) {
                    UI.setDataText("  Blue min: " + algoVOI.getMinIntensityBlue() + " \t\tBlue max: " + algoVOI.getMaxIntensityBlue() + "\n");
                }
                         }
                         else {
                if (algoVOI.getMaxIntensity() > algoVOI.getMinIntensity()) {
                    UI.setDataText("  Min: " + algoVOI.getMinIntensity() + " \t\tMax: " + algoVOI.getMaxIntensity() + "\n");
                }
                         }
             */

            //Only if selected
            ViewList statsList[] = voi.getStatisticList();
            for (int i = 0; i < statsList.length; i++) {
                if (statsList[i].getState() == true) {
                    if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[0])) {
                        UI.setDataText("  No. of Voxels                \t= " +
                                       algoVOI.getNVoxels() + "\n");
                    } else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[1])) {
                        if ((xUnits == yUnits) && (xUnits == zUnits) &&
                            (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                            UI.setDataText("  Volume                       \t= " +
                                       algoVOI.getVolume() + "   " +
                                       str + "\n");
                        }
                        else {
                            UI.setDataText("  Volume                       \t= " +
                                    algoVOI.getVolume() + "\n");
                        }
                    } else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[2])) {
                        if ((xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
                            UI.setDataText("  Area                         \t= " +
                                       algoVOI.getArea() + "   " +
                                       str + "\n");
                        }
                        else {
                            UI.setDataText("  Area                         \t= " +
                                    algoVOI.getArea() + "\n");
                        }
                    } else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[3])) {
                        if ((xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(image.
                                getFileInfo(0).getUnitsOfMeasure(0));
                            UI.setDataText(
                                "  Perimeter                         \t= " +
                                algoVOI.getPerimeter() + "   " + str + "\n");
                        }
                        else {
                            UI.setDataText(
                                    "  Perimeter                         \t= " +
                                    algoVOI.getPerimeter() + "\n");
                        }
                    } else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[4])) {
                        if (image.isColorImage()) {
                            UI.setDataText("  Red min:   \t\t= " +
                                           algoVOI.getMinIntensityRed() +
                                           "\n");

                            UI.setDataText("  Green min: \t\t= " +
                                           algoVOI.getMinIntensityGreen() +
                                           "\n");

                            UI.setDataText("  Blue min: \t\t= " +
                                           algoVOI.getMinIntensityBlue() +
                                           "\n");
                        } else {
                            UI.setDataText("  Min: \t\t= " +
                                           algoVOI.getMinIntensity() + "\n");
                        }
                    } else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[5])) {
                        if (image.isColorImage()) {
                            UI.setDataText("  Red max:   \t\t= " +
                                           algoVOI.getMaxIntensityRed() +
                                           "\n");

                            UI.setDataText("  Green max: \t\t= " +
                                           algoVOI.getMaxIntensityGreen() +
                                           "\n");

                            UI.setDataText("  Blue max:  \t\t= " +
                                           algoVOI.getMaxIntensityBlue() + "\n");
                        } else {
                            UI.setDataText("  Max: \t\t= " +
                                           algoVOI.getMaxIntensity() + "\n");
                        }
                    }

                    else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[6])) {
                        if (image.isColorImage()) {
                            UI.setDataText(
                                    "  Average voxel intensity      \t= " +
                                    algoVOI.getAvgIntenR() + " R, " +
                                    algoVOI.getAvgIntenG() + " G, " +
                                    algoVOI.getAvgIntenB() + " B, " + "\n");
                        } else {
                            UI.setDataText(
                                    "  Average voxel intensity      \t= " +
                                    algoVOI.getAvgInten() + "\n");
                        }
                    } else if (statsList[i].getString().equalsIgnoreCase(
                            algoVOI.
                            makeStatisticListDescriptions()[7])) {
                        if (image.isColorImage()) {
                            UI.setDataText(
                                    "  Std. dev. of voxel intensity \t= " +
                                    algoVOI.getStdDevR() + " R, " +
                                    algoVOI.getStdDevG() + " G, " +
                                    algoVOI.getStdDevB() + " B, " + "\n");
                        } else {
                            UI.setDataText(
                                    "  Std. dev. of voxel intensity \t= " +
                                    algoVOI.getStdDev() + "\n");
                        }
                    } else if (statsList[i].getString().equalsIgnoreCase(
                            algoVOI.
                            makeStatisticListDescriptions()[8])) {
                        UI.setDataText("  Center of Mass               \t= " +
                                       algoVOI.getCenterOfMass() + "\n");
                    } else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[9])) {
                        UI.setDataText("  Principal axis (only 2D)     \t= " +
                                       algoVOI.getPrincipalAxis() +
                                       "  degrees\n");
                    } else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[10])) {
                        UI.setDataText("  Eccentricity (only 2D)       \t= " +
                                       algoVOI.getEccentricity() + "\n");
                    } else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[11])) {
                        if ((xUnits == yUnits) &&
                            (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            unitsString = FileInfoBase.getUnitsOfMeasureStr(
                                    xUnits);
                        }
                        if (unitsString != null) {
                            UI.setDataText(
                                    "  Major axis length (only 2D)   \t= " +
                                    algoVOI.getMajorAxis() +
                                    " " + unitsString + "\n");
                        } else {
                            UI.setDataText(
                                    "  Major axis length (only 2D)     \t= " +
                                    algoVOI.getMajorAxis() + "\n");
                        }
                    } else if (statsList[i].getString().equals(algoVOI.
                            makeStatisticListDescriptions()[12])) {
                        if ((xUnits == yUnits) &&
                            (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
                            unitsString = FileInfoBase.getUnitsOfMeasureStr(
                                    xUnits);
                        }
                        if (unitsString != null) {
                            UI.setDataText(
                                    "  Minor axis length (only 2D)   \t= " +
                                    algoVOI.getMinorAxis() +
                                    " " + unitsString + "\n");
                        } else {
                            UI.setDataText(
                                    "  Minor axis length (only 2D)     \t= " +
                                    algoVOI.getMinorAxis() + "\n");
                        }
                    }
                }
            }
        } else if (source == checkboxExclude) {
            boolean flag = checkboxExclude.isSelected();
            labelMax.setEnabled(flag);
            labelMin.setEnabled(flag);
            textMin.setEnabled(flag);
            textMax.setEnabled(flag);
        } else if (source == cancelButton) {
            cancelFlag = true;
            setVisible(false);
        }
    }

    //*******************************************************************
     //************************* Item Events ****************************
      //*******************************************************************

       /**
        *   Sets opacity slider to enabled or disabled depending on
        *   boundary checkbox.
        *   @param event    Event that cause the method to fire
        */
       public void itemStateChanged(ItemEvent event) {
           Object source = event.getSource();

           if (source == checkboxBoundary) {
               if (checkboxBoundary.isSelected()) {
                   opacitySlider.setEnabled(true);
               } else {
                   opacitySlider.setEnabled(false);
               }
           }
       }

    public void showColorChooser() {
        colorChooser = new ViewJColorChooser(new Frame(),
                                             "Pick VOI color",
                                             new OkColorListener(),
                                             new CancelListener());
    }

    //*******************************************************************
     //************************* Change Events ****************************
      //*******************************************************************

       /**
        *    Sets values based on knob along slider
        *    @param e    Event that triggered this function
        */
       public void stateChanged(ChangeEvent e) {
           Object source = e.getSource();

           if (source == opacitySlider) {
               current.setText(String.valueOf(opacitySlider.getValue() /
                                              (float) 100));
               if (voi != null) {
                   voi.setOpacity(opacitySlider.getValue() / (float) 100);
               }
           }
       }


    /**
     * Updates the ViewJFrameImage when a VOI/contour is selected
     * @param e TreeSelectionEvent
     */
    public void valueChanged(TreeSelectionEvent e) {
        TreePath leadPath = e.getNewLeadSelectionPath();
        if (leadPath != null) {
            Object[] leadObjects = leadPath.getPath();
            int curveIndex = 0;
            if (leadObjects[leadObjects.length - 1] instanceof VOINode) {
                VOIBase leadBase = ((VOINode) leadObjects[leadObjects.
                                    length - 1]).getVOI();
                VOI leadVOI = ((VOIGroupNode) ((VOIFrameNode) ((VOINode)
                        leadObjects[
                        leadObjects.length - 1]).getParent()).getParent()).
                              getVOIgroup();

                //find the frame where this is located
                Vector[] curves = leadVOI.getCurves();

                for (curveIndex = 0; curveIndex < curves.length; curveIndex++) {
                    if (curves[curveIndex].contains(leadBase)) {
                        break;
                    }
                }

                if (frameFollowsSelection && image.getNDims() > 2) {
                    ((ViewJFrameImage) parentFrame).setSlice(curveIndex);
                }
                updateContourPane(leadBase);

            } else if (leadObjects[leadObjects.length -
                       1] instanceof VOIFrameNode) {
                curveIndex = ((VOIFrameNode) leadObjects[leadObjects.length -
                              1]).getFrameNumber();
                if (frameFollowsSelection && image.getNDims() > 2) {
                    ((ViewJFrameImage) parentFrame).setSlice(curveIndex);
                }
            }

        }
        //look for all selection paths
        TreePath[] tPaths = voiTree.getSelectionPaths();

        if (tPaths != null &&
            tPaths.length > 0) {

            // System.err.println("Length of paths: " + tPaths.length);

            TreePath currentPath = null;

            Object[] currentObjects = null;

            VOI currentVOI = null;

            Vector selectedVector = new Vector();

            for (int i = 0; i < tPaths.length; i++) {
                currentPath = tPaths[i];

                currentObjects = currentPath.getPath();

                for (int y = 0; y < currentObjects.length; y++) {

                    //do nothing for root...

                    if (currentObjects[y] instanceof VOIGroupNode) {
                        currentVOI = ((VOIGroupNode) currentObjects[y]).
                                     getVOIgroup();

                        if (!selectedVector.contains(currentObjects[y])) {
                            selectedVector.addElement(currentObjects[y]);
                        }

                    } else if (currentObjects[y] instanceof VOINode) {

                        if (!selectedVector.contains(((VOINode) currentObjects[
                                y]).getParent())) {

                            selectedVector.addElement(((VOINode) currentObjects[
                                    y]).getParent());
                        }

                        selectedVector.addElement(currentObjects[y]);
                    }

                }
            }

            //make un-selected VOI nodes inactive
            Enumeration en = root.children();
            Enumeration frameEnum = null;
            Enumeration nodeChildren = null;

            VOIGroupNode groupNode = null;
            TreeNode tempNode = null;
            VOINode voiNode = null;

            while (en.hasMoreElements()) {
                groupNode = (VOIGroupNode) en.nextElement();

                groupNode.getVOIgroup().setActive(selectedVector.contains(
                        groupNode));

                frameEnum = groupNode.children();
                while (frameEnum.hasMoreElements()) {
                    tempNode = (TreeNode) frameEnum.nextElement();
                    if (tempNode instanceof VOIFrameNode) {
                        nodeChildren = tempNode.children();

                        while (nodeChildren.hasMoreElements()) {
                            voiNode = (VOINode) nodeChildren.nextElement();

                            voiNode.getVOI().setActive(selectedVector.contains(
                                    voiNode));
                        }
                    }
                }
            }

            treeSelectionChange = true;
            ((ViewJFrameImage) parentFrame).getComponentImage().
                    getVOIHandler().fireVOISelectionChange(currentVOI, null);
            image.notifyImageDisplayListeners();
            //System.err.println("notifying of change");
        }

    }

    /** responds to the volume of interest
     *   (<code>VOI</code>) change events.
     *   <p>
     *   This method calls <code>updateVOI</code>
     *   using the <code>UpdateVOIEvent</code>
     *   changed <code>VOI</code>, and retrieves
     *   the runningInSeparateThread out of the current image's
     *   frame.
     *
     *   @see UpdateVOIEvent
     *   @see #updateVOI
     *   @see ViewJFrameBase#getActiveImage
     */
    public void selectionChanged(UpdateVOIEvent newVOIselection) {

        if (newVOIselection == null) {
            System.err.println("JDialogVOIStats.selectionChanged: new selection null");
            return;
        } else if (image == null) {
            System.err.println("JDialogVOIStats.selectionChanged: image is null");
            return;
        } else if (image.getParentFrame() == null) {
            System.err.println("JDialogVOIStats.selectionChanged: image parentFrame() is null");
            return;
        } else if (image.getParentFrame().getComponentImage() == null) {
            System.err.println("JDialogVOIStats.selectionChanged: image parentframe componentImage is null");
            return;
        } else if (image.getParentFrame().getComponentImage().getActiveImage() == null) {
            System.err.println("JDialogVOIStats.selectionChanged: image parentframe componentImage().getActiveImage() is null");
            return;
        }

        updateVOI(newVOIselection.getChangedVolumeOfInterest(),
                  image.getParentFrame().getComponentImage().getActiveImage());
        updateTree();
    }

    /**
     *    Pick up the selected color and call method to change the VOI color
     *
     */
    class OkColorListener implements ActionListener {

        /**
         *   Get color from chooser and set button
         *   and VOI color.
         *   @param e    Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();
            colorButton.setBackground(color);
            colorVOI = color;
        }
    }


    /**
     *    Does nothing.
     */
    class CancelListener implements ActionListener {

        /**
         *   Does nothing.
         */
        public void actionPerformed(ActionEvent e) {
        }
    }


    //*******************************************************************
     //************************* Focus Events ****************************
      //*******************************************************************

       /**
        *   Test the seed value and if appropriate, sets it.
        *   @param event    Event that triggered function.
        */
       public void focusLost(FocusEvent event) {

           String tmpStr = seedValueTF.getText();
           if (testParameter(tmpStr, 0, 32000)) {
               seedValue = Short.valueOf(tmpStr).shortValue();
               voi.setWatershedID(seedValue);
           }

       }

    private class VOITreePopup extends JPanel implements ActionListener,
            PopupMenuListener, MouseListener {

        private JPopupMenu voiPopup;
        private JMenuItem itemShowGraph;
        private JMenuItem itemShowPAAIDialog;
        private JCheckBoxMenuItem itemShowVOIName;
        private JMenu propSubMenu;

        private JMenu orderSubMenu;
        private JMenu contourOrderSubMenu;
        private JMenu editSubMenu;
        private JMenu graphSubMenu;
        private JMenuItem itemClose;
        private JMenu groupSubMenu;

        public VOITreePopup() {

            try {
                voiPopup = new JPopupMenu();

                orderSubMenu = ViewMenuBuilder.buildMenu("VOI Order", 0, false);
                contourOrderSubMenu = ViewMenuBuilder.buildMenu("Contour Order",
                        0, false);
                editSubMenu = ViewMenuBuilder.buildMenu("Edit", 0, false);
                propSubMenu = ViewMenuBuilder.buildMenu("Propagate", 0, false);
                itemShowGraph = ViewMenuBuilder.buildMenuItem("Show VOI Graph",
                        "ShowGraph", 0, (ViewJFrameImage) parentFrame, null, false);
                itemShowPAAIDialog = ViewMenuBuilder.buildMenuItem(
                        "Point area average intensities", "ShowPAIIDialog", 0, (ViewJFrameImage) parentFrame, null, false);
                itemShowVOIName = ViewMenuBuilder.buildCheckBoxMenuItem(
                        "Show VOI name", "ShowName", (ViewJFrameImage) parentFrame,
                        Preferences.is(Preferences.PREF_SHOW_VOI_NAME));

                graphSubMenu = ViewMenuBuilder.buildMenu("Graph", 0, false);
                itemClose = ViewMenuBuilder.buildMenuItem("Close VOI (polyline->polygon)","closeVOI",0,
                                                          (ViewJFrameImage) parentFrame, null, false);

                groupSubMenu = ViewMenuBuilder.buildMenu("Group", 0, false);

            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError(
                        "Out of memory: VOITreePopup Constructor");
                return;
            }

            //Graph Submenu
            graphSubMenu.add(ViewMenuBuilder.buildMenuItem("Boundary intensity","boundaryIntensity", 0,
                    (ViewJFrameImage) parentFrame, null, false));

            if( ((ViewJFrameImage) parentFrame).getActiveImage().getNDims() == 3 ||
                ((ViewJFrameImage) parentFrame).getActiveImage().getNDims() == 4) {
                graphSubMenu.add(ViewMenuBuilder.buildMenuItem("2.5D Total Intensity","totalIntensity", 0, this, null, false));
                graphSubMenu.add(ViewMenuBuilder.buildMenuItem("2.5D Average Intensity","avgIntensity", 0, this, null, false));
                graphSubMenu.add(ViewMenuBuilder.buildMenuItem("2.5D Total Intensity with Threshold","totalIntensityThreshold", 0, this, null, false));
                graphSubMenu.add(ViewMenuBuilder.buildMenuItem("2.5D Average Intensity with Threshold","avgIntensityThreshold", 0, this, null, false));
            }

            //Grouping menu
            groupSubMenu.add(ViewMenuBuilder.buildMenuItem("Group VOIs", "GroupVOIs", 0,
                    (ViewJFrameImage)parentFrame, null, false));
            groupSubMenu.add(ViewMenuBuilder.buildMenuItem("Ungroup VOIs", "UngroupVOIs", 0,
                    (ViewJFrameImage)parentFrame, null, false));


            //Order submenu
            orderSubMenu.add(ViewMenuBuilder.buildMenuItem("Bring VOI to Front",
                    "BringToFront", 0, (ViewJFrameImage) parentFrame,
                    "front.gif", true));
            orderSubMenu.add(ViewMenuBuilder.buildMenuItem("Send VOI to Back",
                    "SendToBack", 0, (ViewJFrameImage) parentFrame, "back.gif", true));
            orderSubMenu.add(ViewMenuBuilder.buildMenuItem("Bring VOI Forward",
                    "BringForward", 0, (ViewJFrameImage) parentFrame,
                    "forward.gif", true));
            orderSubMenu.add(ViewMenuBuilder.buildMenuItem("Send VOI Backward",
                    "SendBackward", 0, (ViewJFrameImage) parentFrame,
                    "backward.gif", true));

            //Contour order submenu
            contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem(
                    "Bring Contour to Front", "BringContourToFront", 0,
                    (ViewJFrameImage) parentFrame,
                    "front.gif", true));
            contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem(
                    "Send Contour to Back", "SendContourToBack", 0,
                    (ViewJFrameImage) parentFrame,
                    "back.gif", true));
            contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem(
                    "Bring Contour Forward", "BringContourForward", 0,
                    (ViewJFrameImage) parentFrame,
                    "forward.gif", true));
            contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem(
                    "Send Contour Backward", "SendContourBackward", 0,
                    (ViewJFrameImage) parentFrame,
                    "backward.gif", true));

            //Edit submenu
            editSubMenu.add(ViewMenuBuilder.buildMenuItem("Delete", "deleteVOI",
                    0, (ViewJFrameImage) parentFrame, "delete.gif", true));
            editSubMenu.add(ViewMenuBuilder.buildMenuItem("Cut", "cutVOI", 0,
                    (ViewJFrameImage) parentFrame,
                    "cutpaint.gif", true));
            editSubMenu.add(ViewMenuBuilder.buildMenuItem("Copy", "copyVOI", 0,
                    (ViewJFrameImage) parentFrame,
                    "copypaint.gif", true));
            editSubMenu.add(ViewMenuBuilder.buildMenuItem("Paste", "pasteVOI",
                    0, (ViewJFrameImage) parentFrame, "pastepaint.gif", true));

            //propagate submenu
            propSubMenu.add(ViewMenuBuilder.buildMenuItem("To Next Slice",
                    "PropVOIUp", 0, (ViewJFrameImage) parentFrame,
                    "voipropu.gif", true));
            propSubMenu.add(ViewMenuBuilder.buildMenuItem("To Previous Slice",
                    "PropVOIDown", 0, (ViewJFrameImage) parentFrame,
                    "voipropd.gif", true));
            propSubMenu.add(ViewMenuBuilder.buildMenuItem("To All Slices",
                    "PropVOIAll", 0, (ViewJFrameImage) parentFrame, null, true));
        }

        public void actionPerformed(ActionEvent e) {

        }

        public void mousePressed(MouseEvent event) {
            checkPopup(event);
        }

        public void mouseClicked(MouseEvent event) {
            checkPopup(event);
        }

        public void mouseEntered(MouseEvent event) {}

        public void mouseExited(MouseEvent event) {}

        public void mouseReleased(MouseEvent event) {
            checkPopup(event);
        }

        private void checkPopup(MouseEvent event) {
            if (event.isPopupTrigger()) {

                int voiGrouping = getSelectedGrouping();
                if (voiGrouping != -1) {
                    // itemShowVOIName.setSelected(Preferences.is(Preferences.
                    //         PREF_SHOW_VOI_NAME));

                    initToVOIType(voiGrouping);

                    voiPopup.show(voiTree, event.getX(), event.getY());
                } else {
                    System.err.println(
                            "VOIs are of different types, select only like-contours...");
                }
            }
        }

        public void popupMenuWillBecomeVisible(PopupMenuEvent event) {
            //  Preferences.debug("Popup menu will be visible!");
        }

        public void popupMenuWillBecomeInvisible(PopupMenuEvent event) {
            //    Preferences.debug("Popup menu will be invisible!");
        }

        public void popupMenuCanceled(PopupMenuEvent event) {
            //  Preferences.debug("Popup menu will be visible!");
        }

        private void initToVOIType(int type) {
            switch (type) {

            case VOI.CONTOUR:
                voiPopup.removeAll();
                voiPopup.add(groupSubMenu);
                voiPopup.addSeparator();
                voiPopup.add(orderSubMenu);
                voiPopup.add(contourOrderSubMenu);
                voiPopup.add(editSubMenu);
                voiPopup.add(propSubMenu);
                voiPopup.add(itemShowVOIName);
                break;
            case VOI.POLYLINE:
                voiPopup.removeAll();
                voiPopup.add(groupSubMenu);
                voiPopup.addSeparator();
                voiPopup.add(orderSubMenu);
                voiPopup.add(contourOrderSubMenu);
                voiPopup.add(editSubMenu);
                voiPopup.add(propSubMenu);
                voiPopup.add(itemShowVOIName);
                voiPopup.add(itemClose);
                break;
            case VOI.POINT:
                voiPopup.removeAll();
                voiPopup.add(groupSubMenu);
                voiPopup.addSeparator();
                voiPopup.add(itemShowVOIName);


            }
        }

        private int getSelectedGrouping() {

            int numOpenContours = 0;
            int numClosedContours = 0;
            int numLines = 0;
            int numPoints = 0;

            TreePath[] tPaths = voiTree.getSelectionPaths();
            TreePath currentPath = null;
            Object[] currentObjects = null;
            VOIBase currentVOIBase = null;

            int type = 0;

            if (tPaths != null && tPaths.length > 0) {

                for (int idx = 0; idx < tPaths.length; idx++) {

                    currentPath = tPaths[idx];
                    currentObjects = currentPath.getPath();

                    if (currentObjects != null) {

                        //look at the final object in path of current TreePath
                        //  to see if it is a VOINode
                        if (currentObjects[currentObjects.length -
                            1] instanceof VOINode) {

                            currentVOIBase = ((VOINode) currentObjects[
                                              currentObjects.length - 1]).
                                             getVOI();

                            if (currentVOIBase instanceof VOIContour) {

                                if (((VOIContour) currentVOIBase).isClosed()) {
                                    numClosedContours++;
                                    type = VOI.CONTOUR;
                                } else {
                                    numOpenContours++;
                                    type = VOI.POLYLINE;
                                }

                            } else if (currentVOIBase instanceof VOIPoint) {
                                numPoints++;
                                type = VOI.POINT;
                            } else if (currentVOIBase instanceof VOILine) {
                                numLines++;
                                type = VOI.LINE;
                            }

                        } //otherwise check to see if a VOIGroupNode is selected

                        else if (currentObjects[currentObjects.length -
                                 1] instanceof VOIGroupNode) {
                            //do something
                        }
                    }
                }
            }
            //      System.err.println("Num open contours: " + numOpenContours);
            //      System.err.println("Num closed contours: " + numClosedContours);
            //      System.err.println("Num pts: " + numPoints);
            //      System.err.println("Num lines: " + numLines);


            int numDiff = 0;

            if (numOpenContours > 0) {
                numDiff++;
            }
            if (numClosedContours > 0) {
                numDiff++;
            }
            if (numPoints > 0) {
                numDiff++;
            }
            if (numLines > 0) {
                numDiff++;
            }

            if (numDiff != 1) {
                return -1;
            }

            return type;
        }
    }


    private class VOITreeRenderer extends DefaultTreeCellRenderer {

        public Component getTreeCellRendererComponent(
                JTree tree,
                Object value,
                boolean sel,
                boolean expanded,
                boolean leaf,
                int row,
                boolean hasFocus) {

            super.getTreeCellRendererComponent(tree, value, sel,
                                               expanded, leaf, row,
                                               hasFocus);

            if (value instanceof VOIGroupNode) {

                setIcon(null);
                int type = ((VOIGroupNode) value).getVOIgroup().getCurveType();

                Icon typeIcon = null;

                switch (type) {
                case VOI.POLYLINE:
                    typeIcon = ICON_POLYLINE;
                    break;
                case VOI.CONTOUR:
                    typeIcon = ICON_POLYGON;
                    break;
                case VOI.LINE:
                    typeIcon = ICON_LINE;
                    break;
                case VOI.POINT:
                    typeIcon = ICON_POINT;
                    break;
                case VOI.PROTRACTOR:
                    typeIcon = ICON_PROTRACTOR;
                    break;
                default:
                    setIcon(null);
                }

                int rgb = ((VOIGroupNode) value).getVOIgroup().getColor().getRGB();
                int black = Color.black.getRGB();

                if (typeIcon != null) {
                    ImageIcon ico = (ImageIcon)typeIcon;
                    int imageWidth = ico.getIconWidth();
                    int imageHeight = ico.getIconHeight();

                    int[] pixels = new int[imageWidth * imageHeight];
                    PixelGrabber pg = new PixelGrabber(ico.getImage(), 0, 0, imageWidth, imageHeight, pixels, 0, imageWidth);
                    try {
                        pg.grabPixels();
                    } catch (InterruptedException e) {
                        Preferences.debug("JIMI: Interrupted waiting for pixels!" + "\n");

                        return null;
                    }

                    BufferedImage image2 = new BufferedImage( ico.getIconWidth() + 15,
                                                              ico.getIconHeight(),
                                                              BufferedImage.TYPE_INT_ARGB);

                    for (int y = 0; y < imageHeight; y++) {
                        for (int x = 0; x < imageWidth; x++) {
                           image2.setRGB(x, y, pixels[(y * imageWidth) + x]);
                        }
                    }



                    //draw black border around color box
                    for (int i = ico.getIconWidth() + 3; i < image2.getWidth() - 3; i++) {
                        for (int j = 0; j < 2; j++) {
                            image2.setRGB(i, j, black);
                        }
                        for (int j = image2.getHeight() - 2; j < image2.getHeight(); j++) {
                            image2.setRGB(i, j , black);
                        }
                    }
                    for (int j = 0; j < image2.getHeight(); j++) {

                        for (int i = ico.getIconWidth() + 3; i < ico.getIconWidth() + 5; i++) {
                            image2.setRGB(i, j, black);
                        }

                        for (int i = image2.getWidth() - 5; i < image2.getWidth() - 3; i++) {
                            image2.setRGB(i, j, black);
                        }

                    }

                    //draw color
                    for (int i = ico.getIconWidth() + 5; i < image2.getWidth() - 5; i++) {
                        for (int j = 2; j < image2.getHeight() - 2; j++) {
                            image2.setRGB(i, j, rgb);
                        }
                    }



                    setIcon(new ImageIcon(image2));

                } else {
                    BufferedImage image = new BufferedImage(9, 26,  BufferedImage.TYPE_INT_ARGB);

                    for (int i = 2; i < 7; i++) {
                        for (int j = 4; j < 24; j++) {
                            image.setRGB(i, j, rgb);
                        }
                    }

                    //draw black border
                    for (int i = 0; i < 9; i++) {
                        for (int j = 2; j < 4; j++) {
                            image.setRGB(i, j, black);
                        }
                        for (int j = 24; j < 26; j++) {
                            image.setRGB(i, j, black);
                        }
                    }
                    for (int j = 2; j < 26; j++) {
                        for (int i = 0; i < 2; i++) {
                            image.setRGB(i, j, black);
                        }
                        for (int i = 7; i < 9; i++) {
                            image.setRGB(i, j, black);
                        }
                    }

                    setIcon (new ImageIcon(image));
                }

               // ImageIcon ico = new ImageIcon(image);
               // setIcon(ico);


                setBorder(null);
                setFont(MipavUtil.font12);
            } else if (value instanceof VOIFrameNode) {
                setBorder(frameBorder);
                setFont(MipavUtil.font10);
                setIcon(null);

            } else if (value instanceof VOINode) {
                setIcon(null);
                setBorder(null);
                setFont(MipavUtil.font12);
            } else {
                //setForeground(Color.white);
                setIcon(ICON_MEDICAL_FRAME);
                setFont(MipavUtil.font12);
                setBorder(null);
                //setIcon(null);
            }
            return this;
        }
    }


}
