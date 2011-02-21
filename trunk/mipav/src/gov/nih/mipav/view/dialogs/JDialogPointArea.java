package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.text.*;


/**
 * Dialog that will call AlgorithmPointArea in order to calculate the average intensity through a volume around an area
 * with a given size (x by y) at a given point. The results are then graphed.
 *
 * @author   ben link
 * @version  1.0
 */
public class JDialogPointArea extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2157089114762708039L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    float[] averageIntensities = null;

    /** DOCUMENT ME! */
    float[][] rgbAverageIntensities = null;

    /** DOCUMENT ME! */
    private JCheckBox constrainBox;

    /** DOCUMENT ME! */
    private boolean leftPad;

    /** DOCUMENT ME! */
    private JCheckBox leftPadBox;

    /** DOCUMENT ME! */
    private JTextField locationField;

    /** DOCUMENT ME! */
    private AlgorithmPointArea pointAlgo;

    /** DOCUMENT ME! */
    private boolean showGraph = false;

    /** DOCUMENT ME! */
    private ModelImage srcImage = null; // source image

    /** DOCUMENT ME! */
    private float threshold = 0.0f;

    /** DOCUMENT ME! */
    private JCheckBox thresholdBox;

    /** DOCUMENT ME! */
    private JTextField thresholdField;

    /** DOCUMENT ME! */
    private boolean topPad;

    /** DOCUMENT ME! */
    private JCheckBox topPadBox;

    /** DOCUMENT ME! */
    private boolean useThreshold = false;

    /** DOCUMENT ME! */
    private int xLoc = -1;

    /** DOCUMENT ME! */
    private JTextField xSpaceField;

    /** DOCUMENT ME! */
    private int xSpacing;

    /** DOCUMENT ME! */
    private int yLoc = -1;

    /** DOCUMENT ME! */
    private JTextField ySpaceField;

    /** DOCUMENT ME! */
    private int ySpacing;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogPointArea() { }

    /**
     * Constructor called from a ViewJFrameImage.
     *
     * @param  theParentFrame  Frame
     * @param  image           ModelImage
     * @param  showGraph       boolean
     */
    public JDialogPointArea(Frame theParentFrame, ModelImage image, boolean showGraph) {
        super(theParentFrame, false);
        srcImage = image;
        this.showGraph = showGraph;
        init();
    }

    /**
     * Constructor called from ViewJComponentEditImage (by clicking on a point and selecting from menu).
     *
     * @param  theParentFrame  Frame
     * @param  image           ModelImage
     * @param  xLoc            int X location for point
     * @param  yLoc            int Y location for point
     * @param  showGraph       boolean whether or not to show the graph
     */
    public JDialogPointArea(Frame theParentFrame, ModelImage image, int xLoc, int yLoc, boolean showGraph) {
        super(theParentFrame, false);
        setSeparateThread(false);

        // setLocation(xLoc, yLoc);
        setXLoc(xLoc);
        setYLoc(yLoc);
        this.showGraph = showGraph;
        srcImage = image;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Constrain")) {

            if (constrainBox.isSelected()) {
                ySpaceField.setEnabled(false);

                String tmpString = xSpaceField.getText();

                try {
                    int xspace = Integer.parseInt(tmpString);
                    ySpaceField.setText(Integer.toString(xspace));
                } catch (Exception ex) {
                    xSpaceField.setText("");
                    ySpaceField.setText("");
                }
            } else {
                ySpaceField.setEnabled(true);
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == thresholdBox) {

            if (thresholdBox.isSelected()) {
                thresholdField.setEnabled(true);
            } else {
                thresholdField.setEnabled(false);
            }
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {


        if (pointAlgo.isCompleted() == true) {

            if (srcImage.isColorImage()) {
                rgbAverageIntensities = pointAlgo.getRGBAverageIntensities();
            } else {
                averageIntensities = pointAlgo.getAverageIntensities();
            }

            insertScriptLine();

            pointAlgo.disposeLocal();
            pointAlgo = null;

            if (showGraph) {
                showIntensityGraph();
            }

            dispose();
        }

    }

    /**
     * Accessor to set if padded with extra column on left.
     *
     * @param  leftPad  DOCUMENT ME!
     */
    public void setLeftPad(boolean leftPad) {
        this.leftPad = leftPad;
    }

    /**
     * Accessor to set showGraph.
     *
     * @param  showGraph  DOCUMENT ME!
     */
    public void setShowGraph(boolean showGraph) {
        this.showGraph = showGraph;
    }

    /**
     * Accessor to set threshold.
     *
     * @param  threshold  DOCUMENT ME!
     */
    public void setThreshold(float threshold) {
        this.threshold = threshold;
    }

    /**
     * Accessor to set if padded with extra row on top.
     *
     * @param  topPad  DOCUMENT ME!
     */
    public void setTopPad(boolean topPad) {
        this.topPad = topPad;
    }

    /**
     * Accessor to set if threshold is used.
     *
     * @param  useThreshold  DOCUMENT ME!
     */
    public void setUseThreshold(boolean useThreshold) {
        this.useThreshold = useThreshold;
    }

    /**
     * Accessor to set x point location.
     *
     * @param  xLoc  DOCUMENT ME!
     */
    public void setXLoc(int xLoc) {
        this.xLoc = xLoc;
    }


    /**
     * Accessor to set x spacing around point.
     *
     * @param  xSpacing  DOCUMENT ME!
     */
    public void setXSpacing(int xSpacing) {
        this.xSpacing = xSpacing;
    }

    /**
     * Accessor to set y point location.
     *
     * @param  yLoc  DOCUMENT ME!
     */
    public void setYLoc(int yLoc) {
        this.yLoc = yLoc;
    }

    /**
     * Accessor to set y spacing around point.
     *
     * @param  ySpacing  DOCUMENT ME!
     */
    public void setYSpacing(int ySpacing) {
        this.ySpacing = ySpacing;
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {

        System.err.println("Use threshold: " + useThreshold + " threshold: " + threshold);
        pointAlgo = new AlgorithmPointArea(srcImage, xLoc, yLoc, xSpacing, ySpacing, leftPad, topPad, useThreshold,
                                           threshold);

        pointAlgo.addListener(this);

        createProgressBar(srcImage.getImageName(), pointAlgo);

        setVisible(false); // Hide dialog

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (pointAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            pointAlgo.run();
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        srcImage = scriptParameters.retrieveInputImage();
        parentFrame = srcImage.getParentFrame();

        setXLoc(scriptParameters.getParams().getInt("x_location"));
        setYLoc(scriptParameters.getParams().getInt("y_location"));
        setXSpacing(scriptParameters.getParams().getInt("x_spacing"));
        setYSpacing(scriptParameters.getParams().getInt("y_spacing"));
        setLeftPad(scriptParameters.getParams().getBoolean("do_add_left_padding"));
        setTopPad(scriptParameters.getParams().getBoolean("do_add_top_padding"));
        setUseThreshold(scriptParameters.getParams().getBoolean("do_use_threshold"));
        setThreshold(scriptParameters.getParams().getFloat("threshold"));
        setShowGraph(scriptParameters.getParams().getBoolean("do_show_graph"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("x_location", xLoc));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y_location", yLoc));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x_spacing", xSpacing));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y_spacing", ySpacing));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_add_left_padding", leftPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_add_top_padding", topPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_threshold", useThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_show_graph", showGraph));
    }

    /**
     * Initializes GUI components and adds them to the dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Point area average intensities");

        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Options"));

        JLabel locationLabel = new JLabel("Point location (x,y)");
        locationLabel.setFont(MipavUtil.font12B);
        gbc.insets = new Insets(0, 5, 0, 0);
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 0;
        optionsPanel.add(locationLabel, gbc);

        locationField = new JTextField(5);

        JTextFieldFilter filter3 = new JTextFieldFilter(JTextFieldFilter.NUMERIC + ",");
        locationField.setDocument(filter3);

        if ((xLoc != -1) && (yLoc != -1)) {
            locationField.setText(Integer.toString(xLoc) + "," + Integer.toString(yLoc));
            locationField.setEnabled(false);
        }

        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = 2;
        gbc.gridx = 2;
        optionsPanel.add(locationField, gbc);

        JLabel spaceLabel = new JLabel("Area around point");
        spaceLabel.setFont(MipavUtil.font12B);
        gbc.insets = new Insets(0, 5, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        optionsPanel.add(spaceLabel, gbc);

        gbc.gridwidth = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        xSpaceField = new JTextField(3);

        JTextFieldFilter filter = new JTextFieldFilter(JTextFieldFilter.NUMERIC);

        // for whatever reason, the keylistener wasn't getting the enter key event after the
        // input/actionmap sets were added to jdialogbase, but this seems to work
        xSpaceField.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke("ENTER"), "updateY");
        xSpaceField.getActionMap().put("updateY", new UpdateYSpaceAction());

        xSpaceField.setDocument(filter);
        gbc.gridx = 2;
        optionsPanel.add(xSpaceField, gbc);

        JLabel crossLabel = new JLabel("x");
        crossLabel.setFont(MipavUtil.font12B);
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 3;
        optionsPanel.add(crossLabel, gbc);

        ySpaceField = new JTextField(3);

        JTextFieldFilter filter2 = new JTextFieldFilter(JTextFieldFilter.NUMERIC);
        ySpaceField.setDocument(filter2);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 4;
        optionsPanel.add(ySpaceField, gbc);


        constrainBox = new JCheckBox("Constrain length-width ratio", false);
        constrainBox.setFont(MipavUtil.font12B);
        constrainBox.addActionListener(this);
        constrainBox.setActionCommand("Constrain");
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridwidth = 2;
        optionsPanel.add(constrainBox, gbc);

        leftPadBox = new JCheckBox("Pad extra column on left", false);
        leftPadBox.setFont(MipavUtil.font12B);
        leftPadBox.setEnabled(false);
        filter.setCheckBox(leftPadBox);
        gbc.gridx = 0;
        gbc.gridy = 3;
        optionsPanel.add(leftPadBox, gbc);

        topPadBox = new JCheckBox("Pad extra row on top", false);
        topPadBox.setFont(MipavUtil.font12B);
        topPadBox.setEnabled(false);
        filter2.setCheckBox(topPadBox);
        gbc.gridx = 0;
        gbc.gridy = 4;
        optionsPanel.add(topPadBox, gbc);

        thresholdBox = new JCheckBox("Use threshold", false);
        thresholdBox.setFont(MipavUtil.font12B);
        thresholdBox.setEnabled(true);
        thresholdBox.addActionListener(this);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 2;
        optionsPanel.add(thresholdBox, gbc);

        thresholdField = new JTextField(4);
        thresholdField.setEnabled(false);

        JTextFieldFilter filter4 = new JTextFieldFilter(JTextFieldFilter.FLOAT);
        thresholdField.setDocument(filter4);
        gbc.gridx = 2;
        gbc.gridwidth = 1;
        optionsPanel.add(thresholdField, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        mainDialogPanel.add(optionsPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        // check location field
        String tmpString = locationField.getText();

        try {
            StringTokenizer tokens = new StringTokenizer(tmpString, ",");
            xLoc = Integer.parseInt(tokens.nextToken().trim());
            yLoc = Integer.parseInt(tokens.nextToken().trim());
        } catch (Exception ex) {
            MipavUtil.displayError("Location must be in the format: x,y");

            return false;
        }

        try {
            xSpacing = Integer.parseInt(xSpaceField.getText().trim());

            if (constrainBox.isSelected()) {
                ySpacing = xSpacing;
            } else {
                ySpacing = Integer.parseInt(ySpaceField.getText().trim());
            }
        } catch (Exception ex) {
            MipavUtil.displayError("Length and width around point must be integers");

            return false;
        }

        this.leftPad = leftPadBox.isSelected();
        this.topPad = topPadBox.isSelected();

        if (thresholdBox.isSelected()) {
            useThreshold = true;

            try {
                threshold = Float.parseFloat(thresholdField.getText().trim());
            } catch (Exception ex) {
                MipavUtil.displayError("Please enter threshold float value");

                return false;
            }
        }

        return true;
    }

    /**
     * Shows a graph of the average intensities through a volume (calculated from AlgorithmPointArea).
     */
    private void showIntensityGraph() {

        ViewJFrameGraph graph = null;

        if (srcImage.isColorImage()) {
            float[][] pos = new float[3][rgbAverageIntensities[0].length];

            for (int i = 0; i < 3; i++) {

                for (int j = 0; j < rgbAverageIntensities[0].length; j++) {
                    pos[i][j] = j;
                }
            }

            graph = new ViewJFrameGraph(pos, rgbAverageIntensities, "Point Area Average Intensities");
            graph.setUnitsInLabel((Unit.getUnitFromLegacyNum(srcImage.getFileInfo(0).getUnitsOfMeasure(0))).getAbbrev());
        } else {
            float[] pos = null;
            pos = new float[averageIntensities.length];

            for (int i = 0; i < pos.length; i++) {
                pos[i] = i;
            }

            graph = new ViewJFrameGraph(pos, averageIntensities, "Point Area Average Intensities",(int[][])null);
            graph.setUnitsInLabel((Unit.getUnitFromLegacyNum(srcImage.getFileInfo(0).getUnitsOfMeasure(0))).getAbbrev());
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Filter that allows only integers or floating point numbers into a textfield while monitoring the numbers
     * themselves in order to activate/deactivate related JCheckboxes.
     *
     * @author  ben link
     */
    public class JTextFieldFilter extends PlainDocument {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -4552252563901427164L;

        /** DOCUMENT ME! */
        public static final String NUMERIC = "0123456789";

        /** DOCUMENT ME! */
        public static final String FLOAT = "012345679.";

        /** DOCUMENT ME! */
        protected String acceptedChars = null;

        /** DOCUMENT ME! */
        protected boolean negativeAccepted = false;

        /** DOCUMENT ME! */
        private JCheckBox box = null;

        /**
         * Creates a new JTextFieldFilter object.
         *
         * @param  acceptedchars  DOCUMENT ME!
         */
        public JTextFieldFilter(String acceptedchars) {
            acceptedChars = acceptedchars;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   offset  DOCUMENT ME!
         * @param   str     DOCUMENT ME!
         * @param   attr    DOCUMENT ME!
         *
         * @throws  BadLocationException  DOCUMENT ME!
         */
        public void insertString(int offset, String str, AttributeSet attr) throws BadLocationException {

            if (str == null) {
                return;
            }

            for (int i = 0; i < str.length(); i++) {

                if (acceptedChars.indexOf(String.valueOf(str.charAt(i))) == -1) {
                    return;
                }
            }

            super.insertString(offset, str, attr);

            if (box != null) {
                String fullString = super.getText(0, super.getLength());

                try {
                    int num = Integer.parseInt(fullString);

                    if ((num % 2) == 1) {
                        box.setEnabled(false);
                    } else {
                        box.setEnabled(true);
                    }
                } catch (Exception ex) {
                    // nada
                }
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param   offs  DOCUMENT ME!
         * @param   len   DOCUMENT ME!
         *
         * @throws  BadLocationException  DOCUMENT ME!
         */
        public void remove(int offs, int len) throws BadLocationException {
            super.remove(offs, len);

            if (box != null) {
                String fullString = super.getText(0, super.getLength());

                try {
                    int num = Integer.parseInt(fullString);

                    if ((num % 2) == 1) {
                        box.setEnabled(false);
                    } else {
                        box.setEnabled(true);
                    }
                } catch (Exception ex) {
                    box.setEnabled(false);
                }
            }


        }

        /**
         * DOCUMENT ME!
         *
         * @param  box  DOCUMENT ME!
         */
        public void setCheckBox(JCheckBox box) {
            this.box = box;
        }
    }

    /**
     * Make the x and y space bounds match if the constrain checkbox is selected.
     */
    protected class UpdateYSpaceAction extends AbstractAction {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 578406985180315919L;

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent event) {
            String tmpString = xSpaceField.getText();

            try {
                int xspace = Integer.parseInt(tmpString);

                if (constrainBox.isSelected()) {
                    ySpaceField.setText(Integer.toString(xspace));
                }
            } catch (Exception ex) {
                xSpaceField.setText("");
                ySpaceField.setText("");
            }
        }
    }
} // end class JDialogPointArea
