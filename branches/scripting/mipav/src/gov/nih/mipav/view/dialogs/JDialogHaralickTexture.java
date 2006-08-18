package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 *
 * @version  0.1 Nov 3, 2005
 * @author   William Gandler
 * @see      AlgorithmHaralickTexture
 */
public class JDialogHaralickTexture extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2408406330754526954L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean asm = false;

    /** DOCUMENT ME! */
    private JCheckBox asmCheckBox;

    /** DOCUMENT ME! */
    private boolean contrast = true;

    /** DOCUMENT ME! */
    private JCheckBox contrastCheckBox;

    /** DOCUMENT ME! */
    private boolean correlation = false;

    /** DOCUMENT ME! */
    private JCheckBox correlationCheckBox;

    /** DOCUMENT ME! */
    private boolean dissimilarity = false;

    /** DOCUMENT ME! */
    private JCheckBox dissimilarityCheckBox;

    /** DOCUMENT ME! */
    private boolean energy = false;

    /** DOCUMENT ME! */
    private JCheckBox energyCheckBox;

    /** DOCUMENT ME! */
    private boolean entropy = false;

    /** DOCUMENT ME! */
    private JCheckBox entropyCheckBox;

    /** DOCUMENT ME! */
    private boolean ew = false;

    /** DOCUMENT ME! */
    private JCheckBox ewCheckBox;

    /** DOCUMENT ME! */
    private boolean homogeneity = false;

    /** DOCUMENT ME! */
    private JCheckBox homogeneityCheckBox;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JCheckBox invariantCheckBox;

    /** DOCUMENT ME! */
    private boolean invariantDir = false;

    /** DOCUMENT ME! */
    private boolean inverseOrder1 = false;

    /** DOCUMENT ME! */
    private JCheckBox inverseOrder1CheckBox;

    /** DOCUMENT ME! */
    private JLabel labelOffsetDistance;

    /** DOCUMENT ME! */
    private JLabel labelWindowSize;

    /** DOCUMENT ME! */
    private boolean maxProbability = false;

    /** DOCUMENT ME! */
    private JCheckBox maxProbabilityCheckBox;

    /** DOCUMENT ME! */
    private boolean mean = false;

    /** DOCUMENT ME! */
    private JCheckBox meanCheckBox;

    /** DOCUMENT ME! */
    private boolean nesw = false;

    /** DOCUMENT ME! */
    private JCheckBox neswCheckBox;

    /** DOCUMENT ME! */
    private boolean ns = true;

    /** DOCUMENT ME! */
    private JCheckBox nsCheckBox;

    /** DOCUMENT ME! */
    private int numDirections = 0;

    /** DOCUMENT ME! */
    private int numOperators = 0;

    /** DOCUMENT ME! */
    private int offsetDistance;

    /** DOCUMENT ME! */
    private ModelImage[] resultImage = null; // result image

    /** DOCUMENT ME! */
    private int resultNumber;

    /** DOCUMENT ME! */
    private JPanel scalePanel;

    /** DOCUMENT ME! */
    private boolean senw = false;

    /** DOCUMENT ME! */
    private JCheckBox senwCheckBox;

    /** DOCUMENT ME! */
    private boolean standardDeviation = false;

    /** DOCUMENT ME! */
    private JCheckBox standardDeviationCheckBox;

    /** DOCUMENT ME! */
    private JTextField textOffsetDistance;

    /** DOCUMENT ME! */
    private AlgorithmHaralickTexture textureAlgo;

    /** DOCUMENT ME! */
    private JTextField textWindowSize;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private boolean variance = false;

    /** DOCUMENT ME! */
    private JCheckBox varianceCheckBox;

    /** DOCUMENT ME! */
    private int windowSize;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogHaralickTexture() { }


    /**
     * Creates a new JDialogHaralickTexture object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogHaralickTexture(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        int i;
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[resultNumber];

        if (algorithm instanceof AlgorithmHaralickTexture) {
            image.clearMask();

            if ((textureAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                // Take resultImage out of array form or null pointer errors can
                // result in one of the resultImages after another of the resultImages
                // has been deleted.
                for (i = 0; i < resultNumber; i++) {
                    updateFileInfo(image, resultImage[i]);
                    resultImage[i].clearMask();

                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new resultImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }

                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();

    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += windowSize + delim;
        str += offsetDistance + delim;
        str += ns + delim;
        str += nesw + delim;
        str += ew + delim;
        str += senw + delim;
        str += invariantDir + delim;
        str += contrast + delim;
        str += dissimilarity + delim;
        str += homogeneity + delim;
        str += inverseOrder1 + delim;
        str += asm + delim;
        str += energy + delim;
        str += maxProbability + delim;
        str += entropy + delim;
        str += mean + delim;
        str += variance + delim;
        str += standardDeviation + delim;
        str += correlation + delim;

        return str;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage[] getResultImage() {
        return resultImage;
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_result_images", resultNumber));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("window_size", windowSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("offset_distance", offsetDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_north_south_dir", ns));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_NE_SW_dir", nesw));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_east_west_dir", ew));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_SE_NW_dir", senw));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_invariant_dir", invariantDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_contrast", contrast));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_dissimilarity", dissimilarity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_homogeneity", homogeneity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_inverse_order_1", inverseOrder1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_ASM", asm));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_energy", energy));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_max_probability", maxProbability));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_entropy", entropy));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_mean", mean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_variance", variance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_standard_deviation", standardDeviation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_correlation", correlation));
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        resultNumber = scriptParameters.getParams().getInt("number_of_result_images");
        
        setWindowSize(scriptParameters.getParams().getInt("window_size"));
        setOffsetDistance(scriptParameters.getParams().getInt("offset_distance"));
        setNS(scriptParameters.getParams().getBoolean("do_calc_north_south_dir"));
        setNESW(scriptParameters.getParams().getBoolean("do_calc_NE_SW_dir"));
        setEW(scriptParameters.getParams().getBoolean("do_calc_east_west_dir"));
        setSENW(scriptParameters.getParams().getBoolean("do_calc_SE_NW_dir"));
        setInvariant(scriptParameters.getParams().getBoolean("do_calc_invariant_dir"));
        setContrast(scriptParameters.getParams().getBoolean("do_calc_contrast"));
        setDissimilarity(scriptParameters.getParams().getBoolean("do_calc_dissimilarity"));
        setHomogeneity(scriptParameters.getParams().getBoolean("do_calc_homogeneity"));
        setInverseOrder1(scriptParameters.getParams().getBoolean("do_inverse_order_1"));
        setASM(scriptParameters.getParams().getBoolean("do_calc_ASM"));
        setEnergy(scriptParameters.getParams().getBoolean("do_calc_energy"));
        setMaxProbability(scriptParameters.getParams().getBoolean("do_calc_max_probability"));
        setEntropy(scriptParameters.getParams().getBoolean("do_calc_entropy"));
        setMean(scriptParameters.getParams().getBoolean("do_calc_mean"));
        setVariance(scriptParameters.getParams().getBoolean("do_calc_variance"));
        setStandardDeviation(scriptParameters.getParams().getBoolean("do_calc_standard_deviation"));
        setCorrelation(scriptParameters.getParams().getBoolean("do_calc_correlation"));
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        for (int i = 0; i < resultNumber; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                textWindowSize.setText("" + MipavUtil.getInt(st));
                textOffsetDistance.setText("" + MipavUtil.getInt(st));
                nsCheckBox.setSelected(MipavUtil.getBoolean(st));
                neswCheckBox.setSelected(MipavUtil.getBoolean(st));
                ewCheckBox.setSelected(MipavUtil.getBoolean(st));
                senwCheckBox.setSelected(MipavUtil.getBoolean(st));
                invariantCheckBox.setSelected(MipavUtil.getBoolean(st));
                contrastCheckBox.setSelected(MipavUtil.getBoolean(st));
                dissimilarityCheckBox.setSelected(MipavUtil.getBoolean(st));
                homogeneityCheckBox.setSelected(MipavUtil.getBoolean(st));
                inverseOrder1CheckBox.setSelected(MipavUtil.getBoolean(st));
                asmCheckBox.setSelected(MipavUtil.getBoolean(st));
                energyCheckBox.setSelected(MipavUtil.getBoolean(st));
                maxProbabilityCheckBox.setSelected(MipavUtil.getBoolean(st));
                entropyCheckBox.setSelected(MipavUtil.getBoolean(st));
                meanCheckBox.setSelected(MipavUtil.getBoolean(st));
                varianceCheckBox.setSelected(MipavUtil.getBoolean(st));
                standardDeviationCheckBox.setSelected(MipavUtil.getBoolean(st));
                correlationCheckBox.setSelected(MipavUtil.getBoolean(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(","));
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Accessor that sets if asm operator is calculated.
     *
     * @param  asm  boolean
     */
    public void setASM(boolean asm) {
        this.asm = asm;
    }

    /**
     * Accessor that sets if the contrast operator is calculated.
     *
     * @param  contrast  boolean
     */
    public void setContrast(boolean contrast) {
        this.contrast = contrast;
    }

    /**
     * Accessor that sets if gray level coordinate matrix correlation is calculated.
     *
     * @param  correlation  boolean
     */
    public void setCorrelation(boolean correlation) {
        this.correlation = correlation;
    }

    /**
     * Accessor that sets if dissimilarity operator is calculated.
     *
     * @param  dissimilarity  boolean
     */
    public void setDissimilarity(boolean dissimilarity) {
        this.dissimilarity = dissimilarity;
    }

    /**
     * Accessor that sets if energy operator is calculated.
     *
     * @param  energy  boolean
     */
    public void setEnergy(boolean energy) {
        this.energy = energy;
    }

    /**
     * Accessor that sets if entropy operator is calculated.
     *
     * @param  entropy  boolean
     */
    public void setEntropy(boolean entropy) {
        this.entropy = entropy;
    }

    /**
     * Accessor that sets if east west offset direction is calculated.
     *
     * @param  ew  boolean
     */
    public void setEW(boolean ew) {
        this.ew = ew;
    }

    /**
     * Accessor that sets if homogeneity operator is calculated.
     *
     * @param  homogeneity  boolean
     */
    public void setHomogeneity(boolean homogeneity) {
        this.homogeneity = homogeneity;
    }

    /**
     * Accessor that sets if spatially invariant offset direction is performed.
     *
     * @param  invariantDir  boolean
     */
    public void setInvariant(boolean invariantDir) {
        this.invariantDir = invariantDir;
    }

    /**
     * Accessor that sets if the inverse difference moment of order 1 operator is called.
     *
     * @param  inverseOrder1  boolean
     */
    public void setInverseOrder1(boolean inverseOrder1) {
        this.inverseOrder1 = inverseOrder1;
    }

    /**
     * Accessor that sets if maximum probability operator is calculated.
     *
     * @param  maxProbability  boolean
     */
    public void setMaxProbability(boolean maxProbability) {
        this.maxProbability = maxProbability;
    }

    /**
     * Accessor that set if the gray level coordinate matrix mean is calculated.
     *
     * @param  mean  boolean
     */
    public void setMean(boolean mean) {
        this.mean = mean;
    }

    /**
     * Accessor that sets if northeast-southest offset direction is calculated.
     *
     * @param  nesw  boolean
     */
    public void setNESW(boolean nesw) {
        this.nesw = nesw;
    }

    /**
     * Accessor that sets if north south offset direction is calculated.
     *
     * @param  ns  boolean
     */
    public void setNS(boolean ns) {
        this.ns = ns;
    }

    /**
     * Accessor that sets the offset distance.
     *
     * @param  offsetDistance  int
     */
    public void setOffsetDistance(int offsetDistance) {
        this.offsetDistance = offsetDistance;
    }

    /**
     * Accessor that sets if southeast-northwest offset direction is calculated.
     *
     * @param  senw  boolean
     */
    public void setSENW(boolean senw) {
        this.senw = senw;
    }

    /**
     * Accessor that sets if gray level coordinate matrix standard deviation is calculated.
     *
     * @param  standardDeviation  boolean
     */
    public void setStandardDeviation(boolean standardDeviation) {
        this.standardDeviation = standardDeviation;
    }

    /**
     * Accessor that sets if the gray level coordinate matrix variance is calculated.
     *
     * @param  variance  boolean
     */
    public void setVariance(boolean variance) {
        this.variance = variance;
    }

    /**
     * Accessor that sets the window size.
     *
     * @param  windowSize  int
     */
    public void setWindowSize(int windowSize) {
        this.windowSize = windowSize;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int i, j, index;
        resultNumber = numDirections * numOperators;

        String[] name;
        String dirString = null;
        String opString = null;
        boolean doneNS = false;
        boolean doneNESW = false;
        boolean doneEW = false;
        boolean doneSENW = false;
        boolean doneInvariant = false;
        boolean doneContrast;
        boolean doneDissimilarity;
        boolean doneHomogeneity;
        boolean doneInverseOrder1;
        boolean doneASM;
        boolean doneEnergy;
        boolean doneMaxProbability;
        boolean doneEntropy;
        boolean doneMean;
        boolean doneVariance;
        boolean doneStandardDeviation;
        boolean doneCorrelation;

        try {
            resultImage = new ModelImage[resultNumber];
            name = new String[resultNumber];

            for (i = 0; i < numDirections; i++) {

                if (ns && (!doneNS)) {
                    dirString = "_ns";
                    doneNS = true;
                } else if (nesw && (!doneNESW)) {
                    dirString = "_nesw";
                    doneNESW = true;
                } else if (ew && (!doneEW)) {
                    dirString = "_ew";
                    doneEW = true;
                } else if (senw && (!doneSENW)) {
                    dirString = "_senw";
                    doneSENW = true;
                } else if (invariantDir && (!doneInvariant)) {
                    dirString = "_invariantDir";
                    doneInvariant = true;
                }

                doneContrast = false;
                doneDissimilarity = false;
                doneHomogeneity = false;
                doneInverseOrder1 = false;
                doneASM = false;
                doneEnergy = false;
                doneMaxProbability = false;
                doneEntropy = false;
                doneMean = false;
                doneVariance = false;
                doneStandardDeviation = false;
                doneCorrelation = false;

                for (j = 0; j < numOperators; j++) {
                    index = j + (i * numOperators);

                    if (contrast && (!doneContrast)) {
                        opString = "_contrast";
                        doneContrast = true;
                    } else if (dissimilarity && (!doneDissimilarity)) {
                        opString = "_dissimilarity";
                        doneDissimilarity = true;
                    } else if (homogeneity && (!doneHomogeneity)) {
                        opString = "_homogeneity";
                        doneHomogeneity = true;
                    } else if (inverseOrder1 && (!doneInverseOrder1)) {
                        opString = "_inverseOrder1";
                        doneInverseOrder1 = true;
                    } else if (asm && (!doneASM)) {
                        opString = "_asm";
                        doneASM = true;
                    } else if (energy && (!doneEnergy)) {
                        opString = "_energy";
                        doneEnergy = true;
                    } else if (maxProbability && (!doneMaxProbability)) {
                        opString = "_maxProbability";
                        doneMaxProbability = true;
                    } else if (entropy && (!doneEntropy)) {
                        opString = "_entropy";
                        doneEntropy = true;
                    } else if (mean && (!doneMean)) {
                        opString = "_mean";
                        doneMean = true;
                    } else if (variance && (!doneVariance)) {
                        opString = "_variance";
                        doneVariance = true;
                    } else if (standardDeviation && (!doneStandardDeviation)) {
                        opString = "_standardDeviation";
                        doneStandardDeviation = true;
                    } else if (correlation && (!doneCorrelation)) {
                        opString = "_correlation";
                        doneCorrelation = true;
                    }

                    name[index] = makeImageName(image.getImageName(), dirString + opString);
                    resultImage[index] = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), name[index],
                                                        userInterface);

                } // for (j = 0; j < numOperators; j++)
            } // for (i = 0; i < numDirections; i++)

            textureAlgo = new AlgorithmHaralickTexture(resultImage, image, windowSize, offsetDistance, ns, nesw, ew,
                                                       senw, invariantDir, contrast, dissimilarity, homogeneity,
                                                       inverseOrder1, asm, energy, maxProbability, entropy, mean,
                                                       variance, standardDeviation, correlation);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            textureAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (textureAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    textureAlgo.setProgressBarVisible(false);
                }

                textureAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {

                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Haralick Texture: unable to allocate enough memory");

            return;
        }

    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Haralick Texture");
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel;

        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        scalePanel = new JPanel(new GridLayout(2, 2));
        scalePanel.setForeground(Color.black);
        scalePanel.setBorder(buildTitledBorder("Sizes"));
        mainPanel.add(scalePanel, gbc);

        labelWindowSize = new JLabel("Window size - odd");
        labelWindowSize.setForeground(Color.black);
        labelWindowSize.setFont(serif12);
        scalePanel.add(labelWindowSize);
        textWindowSize = new JTextField(10);
        textWindowSize.setText("7");
        textWindowSize.setFont(serif12);
        textWindowSize.setForeground(Color.black);
        scalePanel.add(textWindowSize);

        labelOffsetDistance = new JLabel("Offset distance");
        labelOffsetDistance.setForeground(Color.black);
        labelOffsetDistance.setFont(serif12);
        scalePanel.add(labelOffsetDistance);
        textOffsetDistance = new JTextField(10);
        textOffsetDistance.setText("1");
        textOffsetDistance.setFont(serif12);
        textOffsetDistance.setForeground(Color.black);
        scalePanel.add(textOffsetDistance);

        JPanel directionPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        directionPanel.setBorder(buildTitledBorder("Offset directions"));

        nsCheckBox = new JCheckBox("North-South");
        nsCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        directionPanel.add(nsCheckBox, gbc2);
        nsCheckBox.setSelected(true);

        neswCheckBox = new JCheckBox("NE-SW");
        neswCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        directionPanel.add(neswCheckBox, gbc2);
        neswCheckBox.setSelected(false);

        ewCheckBox = new JCheckBox("East-West");
        ewCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        directionPanel.add(ewCheckBox, gbc2);
        ewCheckBox.setSelected(false);

        senwCheckBox = new JCheckBox("SE-NW");
        senwCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 3;
        directionPanel.add(senwCheckBox, gbc2);
        senwCheckBox.setSelected(false);

        invariantCheckBox = new JCheckBox("Spatially invariant");
        invariantCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 4;
        directionPanel.add(invariantCheckBox, gbc2);
        invariantCheckBox.setSelected(false);

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(directionPanel, gbc);

        JPanel operatorPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.weightx = 1;
        gbc3.insets = new Insets(3, 3, 3, 3);
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        operatorPanel.setBorder(buildTitledBorder("Texture operators"));

        contrastCheckBox = new JCheckBox("Contrast (Inertia) (Sum of Squares Variance)");
        contrastCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        operatorPanel.add(contrastCheckBox, gbc3);
        contrastCheckBox.setSelected(true);

        dissimilarityCheckBox = new JCheckBox("Dissimilarity");
        dissimilarityCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 1;
        operatorPanel.add(dissimilarityCheckBox, gbc3);
        dissimilarityCheckBox.setSelected(false);

        homogeneityCheckBox = new JCheckBox("Homogeneity (Inverse Difference Moment of Order 2)");
        homogeneityCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 2;
        operatorPanel.add(homogeneityCheckBox, gbc3);
        homogeneityCheckBox.setSelected(false);

        inverseOrder1CheckBox = new JCheckBox("Inverse Difference Moment of Order 1");
        inverseOrder1CheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 3;
        operatorPanel.add(inverseOrder1CheckBox, gbc3);
        inverseOrder1CheckBox.setSelected(false);

        asmCheckBox = new JCheckBox("Angular Second Moment (ASM)");
        asmCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 4;
        operatorPanel.add(asmCheckBox, gbc3);
        asmCheckBox.setSelected(false);

        energyCheckBox = new JCheckBox("Energy (Uniformity)");
        energyCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 5;
        operatorPanel.add(energyCheckBox, gbc3);
        energyCheckBox.setSelected(false);

        maxProbabilityCheckBox = new JCheckBox("Maximum Probability (MAX)");
        maxProbabilityCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 6;
        operatorPanel.add(maxProbabilityCheckBox, gbc3);
        maxProbabilityCheckBox.setSelected(false);

        entropyCheckBox = new JCheckBox("Entropy (ENT)");
        entropyCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 7;
        operatorPanel.add(entropyCheckBox, gbc3);
        entropyCheckBox.setSelected(false);

        meanCheckBox = new JCheckBox("Gray Level Co-occurrence Matrix Mean");
        meanCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 8;
        operatorPanel.add(meanCheckBox, gbc3);
        meanCheckBox.setSelected(false);

        varianceCheckBox = new JCheckBox("Gray Level Co-occurrence Matrix Variance");
        varianceCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 9;
        operatorPanel.add(varianceCheckBox, gbc3);
        varianceCheckBox.setSelected(false);

        standardDeviationCheckBox = new JCheckBox("Gray Level Co-occurrence Matrix Standard Deviation");
        standardDeviationCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 10;
        operatorPanel.add(standardDeviationCheckBox, gbc3);
        standardDeviationCheckBox.setSelected(false);

        correlationCheckBox = new JCheckBox("Gray Level Co-occurrence Matrix Correlation");
        correlationCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 11;
        operatorPanel.add(correlationCheckBox, gbc3);
        correlationCheckBox.setSelected(false);

        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(operatorPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        // setVisible( true );

        System.gc();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************


    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        numDirections = 0;
        numOperators = 0;

        tmpStr = textWindowSize.getText();

        if (testParameter(tmpStr, 3, Integer.MAX_VALUE)) {
            windowSize = Integer.valueOf(tmpStr).intValue();
        } else {
            textWindowSize.requestFocus();
            textWindowSize.selectAll();

            return false;
        }

        if ((windowSize % 2) == 0) {
            MipavUtil.displayError("Window size must not be even");
            textWindowSize.requestFocus();
            textWindowSize.selectAll();

            return false;
        }

        if (windowSize > image.getExtents()[0]) {
            MipavUtil.displayError("Window size must not excced image width of " + image.getExtents()[0]);
            textWindowSize.requestFocus();
            textWindowSize.selectAll();

            return false;
        }

        if (windowSize > image.getExtents()[1]) {
            MipavUtil.displayError("Window size must not excced image height of " + image.getExtents()[1]);
            textWindowSize.requestFocus();
            textWindowSize.selectAll();

            return false;
        }

        tmpStr = textOffsetDistance.getText();

        if (testParameter(tmpStr, 1, windowSize - 1)) {
            offsetDistance = Integer.valueOf(tmpStr).intValue();
        } else {
            textOffsetDistance.requestFocus();
            textOffsetDistance.selectAll();

            return false;
        }

        ns = nsCheckBox.isSelected();

        if (ns) {
            numDirections++;
        }

        nesw = neswCheckBox.isSelected();

        if (nesw) {
            numDirections++;
        }

        ew = ewCheckBox.isSelected();

        if (ew) {
            numDirections++;
        }

        senw = senwCheckBox.isSelected();

        if (senw) {
            numDirections++;
        }

        invariantDir = invariantCheckBox.isSelected();

        if (invariantDir) {
            numDirections++;
        }

        if (numDirections == 0) {
            MipavUtil.displayError("At least one direction must be checked");

            return false;
        }

        contrast = contrastCheckBox.isSelected();

        if (contrast) {
            numOperators++;
        }

        dissimilarity = dissimilarityCheckBox.isSelected();

        if (dissimilarity) {
            numOperators++;
        }

        homogeneity = homogeneityCheckBox.isSelected();

        if (homogeneity) {
            numOperators++;
        }

        inverseOrder1 = inverseOrder1CheckBox.isSelected();

        if (inverseOrder1) {
            numOperators++;
        }

        asm = asmCheckBox.isSelected();

        if (asm) {
            numOperators++;
        }

        energy = energyCheckBox.isSelected();

        if (energy) {
            numOperators++;
        }

        maxProbability = maxProbabilityCheckBox.isSelected();

        if (maxProbability) {
            numOperators++;
        }

        entropy = entropyCheckBox.isSelected();

        if (entropy) {
            numOperators++;
        }

        mean = meanCheckBox.isSelected();

        if (mean) {
            numOperators++;
        }

        variance = varianceCheckBox.isSelected();

        if (variance) {
            numOperators++;
        }

        standardDeviation = standardDeviationCheckBox.isSelected();

        if (standardDeviation) {
            numOperators++;
        }

        correlation = correlationCheckBox.isSelected();

        if (correlation) {
            numOperators++;
        }

        if (numOperators == 0) {
            MipavUtil.displayError("At least 1 texture operator must be selected");

            return false;
        }

        return true;
    }

}
