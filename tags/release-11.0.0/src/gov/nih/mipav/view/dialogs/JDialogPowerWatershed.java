package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmPowerWatershed;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.io.IOException;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * DOCUMENT ME!
 */
public class JDialogPowerWatershed extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------
    private static int Kruskal = 1;

    private static int PW_qis2 = 2;

    private static int Prim = 3;

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private int algo;

    // The index in the image array of the seed
    private final Vector<Integer> index_seeds = new Vector<Integer>();

    // For 2-labels 1 for white foreground and 2 for black background
    // For multi-labels values from 1 to n with n <= 255 for segmentation in n labels
    private final Vector<Short> index_labels = new Vector<Short>();

    // Geodesic reconstruction
    private boolean geod;

    private ButtonGroup algoGroup;

    private JRadioButton KruskalButton;

    private JRadioButton PowerButton;

    private JRadioButton PrimButton;

    private ButtonGroup labelsGroup;

    private JRadioButton twoButton;

    private JRadioButton multiButton;

    private JCheckBox geodesicCheckBox;

    private JButton pointsButton;

    private JButton maskButton;

    private short pointNum = 1;

    private boolean haveOne = false;

    private boolean haveTwo = false;

    private int presentvoiIndex = 0;

    /** DOCUMENT ME! */
    private AlgorithmPowerWatershed pwAlgo;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    private ViewJComponentEditImage componentImage;

    private JComboBox imageComboBox;

    private String maskName;

    private ModelImage maskImage;

    private final ViewUserInterface UI = ViewUserInterface.getReference();

    private ArrayList<labelIndexItem> pointList = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogPowerWatershed() {}

    // or if the source image is to be replaced

    /**
     * Creates new dialog for entering parameters for Power watershed.
     * 
     * @param theParentFrame Parent frame
     * @param im Source image
     */
    public JDialogPowerWatershed(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
        componentImage.getVOIHandler().newVOI(0.0f);
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);

        scriptParameters.getParams().put(ParameterFactory.newParameter("algorithm", algo));
        scriptParameters.getParams().put(ParameterFactory.newParameter("geodesic", geod));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        algo = scriptParameters.getParams().getInt("algorithm");
        geod = scriptParameters.getParams().getBoolean("goedesic");
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    @Override
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event event that triggers function
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        }
        if (command.equals("Add")) {
            int i, j;
            int x, y, z;
            VOIVector voiVector;
            VOI presentVOI;
            Vector3f[] tmppt = null;
            int index;
            final int xDim = image.getExtents()[0];
            final int yDim = image.getExtents()[1];
            final int sliceSize = xDim * yDim;
            voiVector = image.getVOIs();
            if (pointList == null) {
                pointList = new ArrayList<labelIndexItem>();
            }
            for (i = presentvoiIndex; i < voiVector.size(); i++) {
                presentVOI = image.getVOIs().VOIAt(i);
                if (presentVOI.getCurveType() == VOI.POINT) {
                    tmppt = presentVOI.exportAllPoints();
                    for (j = 0; j < tmppt.length; j++) {
                        x = Math.round(tmppt[j].X);
                        y = Math.round(tmppt[j].Y);
                        z = Math.round(tmppt[j].Z);
                        index = x + y * xDim + z * sliceSize;
                        pointList.add(new labelIndexItem(pointNum, index));
                        if (pointNum == 1) {
                            haveOne = true;
                        } else if (pointNum == 2) {
                            haveTwo = true;
                        }
                    }
                }
            }
            presentvoiIndex = voiVector.size();
            if ( (twoButton.isSelected() && pointNum < 2) || (multiButton.isSelected() && pointNum < 255)) {
                pointNum++;
                pointsButton.setText("Finished adding " + String.valueOf(pointNum) + " points");
                if (pointNum == 2) {
                    componentImage.getVOIHandler().newVOI(1.0f / 3.0f);
                } else if (pointNum == 3) {
                    componentImage.getVOIHandler().newVOI(2.0f / 3.0f);
                } else if (pointNum == 4) {
                    componentImage.getVOIHandler().newVOI(1.0f / 6.0f);
                } else if (pointNum == 5) {
                    componentImage.getVOIHandler().newVOI(1.0f / 2.0f);
                } else if (pointNum >= 6) {
                    componentImage.getVOIHandler().newVOI(5.0f / 6.0f);
                }
            } else {
                pointsButton.setText("Cannot add any more points");
                pointsButton.setEnabled(false);
            }
        } else if (command.equals("Mask")) {
            if (maskImage == null) {
                MipavUtil.displayError("No mask image has been found");
                return;
            }
            int i;
            int firstLabel = -1;
            // maskImage is same size as image
            final int xDim = maskImage.getExtents()[0];
            final int yDim = maskImage.getExtents()[1];
            int length = xDim * yDim;
            int zDim = 1;
            if (maskImage.getNDims() > 2) {
                zDim = maskImage.getExtents()[2];
                length = length * zDim;
            }
            final short buffer[] = new short[length];
            try {
                maskImage.exportData(0, length, buffer);
            } catch (final IOException e) {
                MipavUtil.displayError("IOException " + e + " on maskImage.exportData(0, length, buffer)");
                return;
            }
            final short minValue = (short) maskImage.getMin();
            final short maxValue = (short) maskImage.getMax();
            if (twoButton.isSelected()) {
                for (i = 0; i < length; i++) {
                    if (buffer[i] == minValue) {
                        index_seeds.add(i);
                        index_labels.add((short) 1);
                        haveOne = true;
                    } else if (buffer[i] == maxValue) {
                        index_seeds.add(i);
                        index_labels.add((short) 2);
                        haveTwo = true;
                    }
                }
            } else { // multiButton.isSelected()
                for (i = 0; i < length; i++) {
                    if (buffer[i] != 0) {
                        index_seeds.add(i);
                        index_labels.add(buffer[i]);
                        if ( !haveOne) {
                            haveOne = true;
                            firstLabel = buffer[i];
                        } else if (haveOne && ( !haveTwo) && (buffer[i] != firstLabel)) {
                            haveTwo = true;
                        }
                    }
                }
            }
            maskButton.setEnabled(false);
        } else {
            super.actionPerformed(event);
        }

    }

    /**
     * itemStateChanged.
     * 
     * @param event DOCUMENT ME!
     */
    @Override
    public void itemStateChanged(final ItemEvent event) {
        final Object source = event.getSource();

        if (source == imageComboBox) {
            maskName = (String) imageComboBox.getSelectedItem();

            if (maskName != null) {
                maskImage = UI.getRegisteredImageByName(maskName);
            }
        }

    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmPowerWatershed) {
            System.err.println("finished algorithm");
            image.clearMask();

            if ( (pwAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                insertScriptLine();
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if ( ((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     * 
     * @return the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * 
     * @param algo
     */
    public void setAlgo(final int algo) {
        this.algo = algo;
    }

    /**
     * 
     * @param geod
     */
    public void setGeod(final boolean geod) {
        this.geod = geod;
    }

    /**
     * DOCUMENT ME!
     */
    @Override
    public void callAlgorithm() {

        String name;
        if (algo == Kruskal) {
            name = makeImageName(image.getImageName(), "_Kruskal");
        } else if (algo == PW_qis2) {
            name = makeImageName(image.getImageName(), "_PowerWatershed");
        } else {
            name = makeImageName(image.getImageName(), "_Prim");
        }

        try {

            // Make result image of unsigned type
            resultImage = new ModelImage(ModelImage.UBYTE, image.getExtents(), name);

            // Make algorithm
            pwAlgo = new AlgorithmPowerWatershed(resultImage, image, algo, index_seeds, index_labels, geod);

        } catch (final OutOfMemoryError x) {
            MipavUtil.displayError("Dialog power watershed: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }

        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed of failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        pwAlgo.addListener(this);

        // Hide dialog
        setVisible(false);
        createProgressBar(image.getImageName(), pwAlgo);

        // Start the thread as a low priority because we wish to still have user interface work fast
        if (isRunInSeparateThread()) {

            if (pwAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            pwAlgo.run();
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Power Watershed");

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;

        int yPos = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        final JPanel algoPanel = new JPanel(new GridBagLayout());
        algoPanel.setBorder(buildTitledBorder("Algorithms"));

        algoGroup = new ButtonGroup();
        KruskalButton = new JRadioButton("Kruskal", true);
        KruskalButton.setFont(serif12);
        algoGroup.add(KruskalButton);
        gbc.gridy = yPos++;
        algoPanel.add(KruskalButton, gbc);

        PowerButton = new JRadioButton("Power Watershed, q = 2", false);
        PowerButton.setFont(serif12);
        algoGroup.add(PowerButton);
        gbc.gridy = yPos++;
        algoPanel.add(PowerButton, gbc);

        PrimButton = new JRadioButton("Prim", false);
        PrimButton.setFont(serif12);
        algoGroup.add(PrimButton);
        gbc.gridy = yPos++;
        algoPanel.add(PrimButton, gbc);

        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.weightx = 1;

        int yPos2 = 0;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.HORIZONTAL;

        final JPanel labelsPanel = new JPanel(new GridBagLayout());
        labelsPanel.setForeground(Color.black);
        labelsPanel.setBorder(buildTitledBorder("Labels"));

        final JLabel headLabel = new JLabel("Add points or load mask image");
        headLabel.setForeground(Color.black);
        headLabel.setFont(serif12);
        gbc2.gridy = yPos2++;
        labelsPanel.add(headLabel, gbc2);

        labelsGroup = new ButtonGroup();
        twoButton = new JRadioButton("Two labels", false);
        twoButton.setFont(serif12);
        labelsGroup.add(twoButton);
        gbc2.gridy = yPos2++;
        labelsPanel.add(twoButton, gbc2);

        final JLabel oneLabel = new JLabel("VOI 1 or maskImage minValue for black background seeds");
        oneLabel.setForeground(Color.black);
        oneLabel.setFont(serif12);
        gbc2.gridy = yPos2++;
        labelsPanel.add(oneLabel, gbc2);

        final JLabel twoLabel = new JLabel("VOI 2 or maskImage maxValue for white foreground seeds");
        twoLabel.setForeground(Color.black);
        twoLabel.setFont(serif12);
        gbc2.gridy = yPos2++;
        labelsPanel.add(twoLabel, gbc2);

        final JLabel grayLabel = new JLabel("Intermediate mask values do not contribute to two level seeds");
        grayLabel.setForeground(Color.black);
        grayLabel.setFont(serif12);
        gbc2.gridy = yPos2++;
        labelsPanel.add(grayLabel, gbc2);

        multiButton = new JRadioButton("Multiple labels", true);
        multiButton.setFont(serif12);
        labelsGroup.add(multiButton);
        gbc2.gridy = yPos2++;
        labelsPanel.add(multiButton, gbc2);

        final JLabel valuesLabel = new JLabel("Values from 1 to n");
        valuesLabel.setForeground(Color.black);
        valuesLabel.setFont(serif12);
        gbc2.gridy = yPos2++;
        labelsPanel.add(valuesLabel, gbc2);

        pointsButton = new JButton("Finished adding 1 points");
        pointsButton.setPreferredSize(MipavUtil.defaultButtonSize);
        pointsButton.setFont(serif12B);
        gbc2.gridy = yPos2++;
        labelsPanel.add(pointsButton, gbc2);
        pointsButton.addActionListener(this);
        pointsButton.setActionCommand("Add");

        final JLabel labelImage2 = new JLabel("Mask image: ");
        labelImage2.setForeground(Color.black);
        labelImage2.setFont(serif12);
        gbc2.gridy = yPos2++;
        labelsPanel.add(labelImage2, gbc2);
        imageComboBox = buildComboBox(image);
        imageComboBox.addItemListener(this);
        gbc2.gridx = 1;
        labelsPanel.add(imageComboBox, gbc2);

        maskName = (String) imageComboBox.getSelectedItem();
        if (maskName != null) {
            maskImage = UI.getRegisteredImageByName(maskName);
        }

        maskButton = new JButton("Use mask image to generate seeds");
        maskButton.setPreferredSize(MipavUtil.defaultButtonSize);
        maskButton.setFont(serif12B);
        gbc2.gridx = 0;
        gbc2.gridy = yPos2++;
        labelsPanel.add(maskButton, gbc2);
        maskButton.addActionListener(this);
        maskButton.setActionCommand("Mask");

        geodesicCheckBox = new JCheckBox("Geodesic reconstruction", true);
        geodesicCheckBox.setFont(serif12);
        geodesicCheckBox.setForeground(Color.black);

        final JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        mainPanel.add(algoPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(labelsPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(geodesicCheckBox, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        final JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i;

        if (KruskalButton.isSelected()) {
            algo = Kruskal;
        } else if (PowerButton.isSelected()) {
            algo = PW_qis2;
        } else if (PrimButton.isSelected()) {
            algo = Prim;
        }

        geod = geodesicCheckBox.isSelected();

        if ( !haveOne) {
            MipavUtil.displayError("No 1 point is present");
            return false;
        } else if ( !haveTwo) {
            MipavUtil.displayError("No 2 point is present");
            return false;
        }

        if (pointList != null) {
            Collections.sort(pointList, new labelIndexComparator());
            for (i = 0; i < pointList.size(); i++) {
                index_labels.add(pointList.get(i).getLabel());
                index_seeds.add(pointList.get(i).getIndex());
            }
            pointList.clear();
        } // if (pointList != null)

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    @Override
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            @Override
            public String getCategory() {
                return new String("Algorithms.Segmentation");
            }

            @Override
            public String getDescription() {
                return new String("Performs retrospective segmentation." + "Note that in the algo field, "
                        + "1 == Maximum Spanning Forest computed by Kruskal algorithm,"
                        + "2 == Powerwatersheds(p=infinite, q=2): Maximum Spanning Forest Computed by "
                        + "Kruskal algorithm and Random walker on plateaus, and"
                        + "3 == Maximum Spanning Forest computed by Prim algorithm using Red and black trees.");
            }

            @Override
            public String getDescriptionLong() {
                return new String("Performs retrospective segmentation." + "Note that in the algo field, "
                        + "1 == Maximum Spanning Forest computed by Kruskal algorithm,"
                        + "2 == Powerwatersheds(p=infinite, q=2): Maximum Spanning Forest Computed by "
                        + "Kruskal algorithm and Random walker on plateaus, and"
                        + "3 == Maximum Spanning Forest computed by Prim algorithm using Red and black trees.");
            }

            @Override
            public String getShortLabel() {
                return new String("PowerWatershed");
            }

            @Override
            public String getLabel() {
                return new String("Power Watershed");
            }

            @Override
            public String getName() {
                return new String("Power Watershed");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    @Override
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterInt("algorithm", Kruskal));
            table.put(new ParameterBoolean("geodesic", true));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    @Override
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    @Override
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    @Override
    public boolean isActionComplete() {
        return isComplete();
    }

    /**
     * Builds a list of images. Returns combobox. List must be all color or all black and white.
     * 
     * @param image DOCUMENT ME!
     * 
     * @return Newly created combo box.
     */
    private JComboBox buildComboBox(final ModelImage image) {
        ViewUserInterface UI;
        ModelImage nextImage;
        boolean doAdd;
        int i;

        final JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        final Enumeration<String> names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            final String name = names.nextElement();

            if ( !name.equals(image.getImageName())) {
                nextImage = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(nextImage) != null) {

                    if ( ( !nextImage.isColorImage()) && (nextImage.getNDims() == image.getNDims())) {
                        doAdd = true;

                        for (i = 0; i < image.getNDims(); i++) {

                            if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                                doAdd = false;
                            }
                        }

                        if (doAdd) {
                            comboBox.addItem(name);
                        }
                    }
                }
            }
        }

        return comboBox;
    }

    private class labelIndexItem {

        /** DOCUMENT ME! */
        private final short label;

        /** DOCUMENT ME! */
        private final int index;

        /**
         * Creates a new labelIndexItem object.
         * 
         * @param label
         * @param index
         */
        public labelIndexItem(final short label, final int index) {
            this.label = label;
            this.index = index;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public short getLabel() {
            return label;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getIndex() {
            return index;
        }
    }

    private class labelIndexComparator implements Comparator<labelIndexItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        @Override
        public int compare(final labelIndexItem o1, final labelIndexItem o2) {
            final int a = o1.getIndex();
            final int b = o2.getIndex();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }

}
