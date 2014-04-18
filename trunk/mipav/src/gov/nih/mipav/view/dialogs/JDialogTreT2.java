package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTreT2;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.Parameter;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


/**
 * The dialog for the calculation of Tre methods. This dialog is scriptable, but not yet Jistable.
 * 
 * @author senseneyj
 * 
 */
public class JDialogTreT2 extends JDialogScriptableBase implements AlgorithmInterface, ChangeListener {

    private static String title = "TRE T2 Mapper";

    private double treTR = 5.00;

    private double maxT2 = 1000;

    private double maxM0 = 10000;

    private double[] treFA_phase0;

    private double[] treFA_phase180;

    private int[] ssfpImageIndex_phase0;

    private int[] ssfpImageIndex_phase180;

    private int t1ImageIndex;

    private int b1ImageIndex;

    private double[] simplexLineValues, simplexResiduals, simplexCentre, reflection, expansion, contraction, shrink;

    private double[][] simplex;

    private double[] twoPSimplexLineValues, twoPSimplexResiduals, twoPSimplexCentre, twoPReflection, twoPExpansion, twoPContraction, twoPShrink;

    private double[][] twoPSimplex;

    private int[] bestToWorst;

    private int Nfa_phase0 = 0;

    private int Nfa_phase180 = 2;

    private boolean calculateT2 = true;

    private boolean includeB1Map = false;

    private boolean performConventionalModelling = true;

    private boolean performApproxModelling = false;

    private boolean performFullModelling = false;

    private boolean calculateM0 = true;

    private boolean invertT2toR2 = false;

    private boolean calculateB0 = false;

    private boolean performConventionalWith180Phase = true;

    private boolean performConventionalWith0Phase = false;

    private boolean geScanner = true;

    private boolean siemensScanner = false;

    private boolean upperLeftCorner = true;

    private boolean upperRightCorner = false;

    private boolean lowerLeftCorner = false;

    private boolean lowerRightCorner = false;

    private boolean useSmartThresholding = true;

    private boolean useHardThresholding = false;

    private final float noiseScale = (float) 1.00;

    private final float hardNoiseThreshold = (float) 0.00;

    private String[] wList;

    private String[] titles;

    private AlgorithmTreT2 cAlgo;

    private JTextField nfaPhase0Text;

    private JTextField nfaPhase180Text;

    private JRadioButton doConventionalT2Button;

    private JRadioButton doApproximateT2Button;

    private JRadioButton doFullT2Button;

    private JCheckBox doB1MapBox;

    private ButtonGroup processType;

    private GuiBuilder guiHelp;

    private JTextField maxT2Text;

    private JTextField maxM0Text;

    private JCheckBox doT2Box;

    private JCheckBox doMoBox;

    private JCheckBox showB0Box;

    private JCheckBox showR2Box;

    private JTextField ssfpRepTime;

    private JComboBox preCalcT1Box;

    private JComboBox[] phaseImageList;

    private JTextField[] phaseFlipAngleList;

    private JComboBox preCalcB1MapBox;

    private JComboBox[] phase180ImageList;

    private JTextField[] phase180FlipAngleList;

    private JRadioButton isGEScannerButton;

    private JRadioButton isSiemensButton;

    private JTabbedPane algoPane;

    private JPanel paramPanel;

    private JPanel conventionalPanel;

    private JPanel advancedPanel;

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogTreT2() {}

    /**
     * Construct the barrel/pin cushion correction dialog.
     * 
     * @param theParentFrame Parent frame.
     * @param im Source image.
     */
    public JDialogTreT2(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        init();
    }

    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (algorithm instanceof AlgorithmTreT2) {
            Preferences.debug("TreT2: " + algorithm.getElapsedTime());
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if ( !runningScriptFlag) {
            if (cAlgo != null) {
                cAlgo.finalize();
                cAlgo = null;
            }

            dispose();
        }
    }

    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equalsIgnoreCase("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            cAlgo.interrupt();
        } else {
            super.actionPerformed(event);
        }
    }

    private boolean setVariables() {

        // general variables
        Nfa_phase0 = (int) Double.valueOf(nfaPhase0Text.getText()).doubleValue();
        Nfa_phase180 = (int) Double.valueOf(nfaPhase180Text.getText()).doubleValue();
        performConventionalModelling = doConventionalT2Button.isSelected();
        performApproxModelling = doApproximateT2Button.isSelected();
        performFullModelling = doFullT2Button.isSelected();
        includeB1Map = doB1MapBox.isSelected();

        if (processType.getSelection() == null) {
            MipavUtil.displayInfo("Please select a processing method");
            return false;
        }

        if (performConventionalModelling == true) {
            performApproxModelling = false;
            performFullModelling = false;
        }
        if (performApproxModelling == true) {
            performConventionalModelling = false;
            performFullModelling = false;
        }
        if (performFullModelling == true) {
            performConventionalModelling = false;
            performApproxModelling = false;
        }

        // do some checking
        if (Nfa_phase0 + Nfa_phase180 + 1 > wList.length) {
            MipavUtil.displayWarning("Please import all nessesary images first: At least 2 SSFP Images + 1 T1 Map");
            return false;
        }

        if (performApproxModelling == true || performFullModelling == true) {
            if (Nfa_phase0 + Nfa_phase180 < 3) {
                MipavUtil.displayWarning("T2 calculations require at least three SSFP images (2 at each RF phase increment).");
                return false;
            }
        }

        if (performConventionalModelling) {
            if (Nfa_phase0 >= 1) {
                performConventionalWith180Phase = false;
                performConventionalWith0Phase = true;
            } else {
                performConventionalWith0Phase = false;
                performConventionalWith180Phase = true;
            }
        } else {
            performConventionalWith180Phase = false;
            performConventionalWith0Phase = false;
        }

        // specifics variables
        maxT2 = Double.valueOf(maxT2Text.getText()).doubleValue();
        maxM0 = Double.valueOf(maxM0Text.getText()).doubleValue();
        calculateT2 = doT2Box.isSelected();
        calculateM0 = doMoBox.isSelected();
        if (performFullModelling == true) {
            calculateB0 = showB0Box.isSelected();
        }
        invertT2toR2 = showR2Box.isSelected();

        if ( !performConventionalModelling || performConventionalWith0Phase) {
            for (int i = 0; i < Nfa_phase0; i++) {
                ssfpImageIndex_phase0[i] = phaseImageList[i].getSelectedIndex();
                treFA_phase0[i] = Float.valueOf(phaseFlipAngleList[i].getText()).floatValue();
            }
        }

        if ( !performConventionalModelling || performConventionalWith0Phase) {
            for (int i = 0; i < Nfa_phase180; i++) {
                ssfpImageIndex_phase180[i] = phaseImageList[i].getSelectedIndex();
                treFA_phase180[i] = Float.valueOf(phaseFlipAngleList[i].getText()).floatValue();
            }
        }

        treTR = Double.valueOf(ssfpRepTime.getText()).doubleValue();

        t1ImageIndex = preCalcT1Box.getSelectedIndex();
        if (includeB1Map) {
            b1ImageIndex = preCalcB1MapBox.getSelectedIndex();
        }

        geScanner = isGEScannerButton.isSelected();
        siemensScanner = isSiemensButton.isSelected();

        if (geScanner == true) {
            siemensScanner = false;
        }
        if (siemensScanner == true) {
            geScanner = false;
        }

        return true;
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    @Override
    protected void doPostAlgorithmActions() {

        if (cAlgo != null) {
            if (includeB1Map && cAlgo.getB0ResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getB0ResultStack());
            }

            if (calculateM0 && cAlgo.getM0ResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getM0ResultStack());
            }

            if (invertT2toR2 && cAlgo.getR2ResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getR2ResultStack());
            }

            if (calculateT2 && cAlgo.getT2ResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getT2ResultStack());
            }
        }

        // algorithm was not disposed of during algorithm performed since script is running
        if (cAlgo != null) {
            cAlgo.finalize();
            cAlgo = null;
        }

        dispose();
    }

    @Override
    protected void setGUIFromParams() {
        treTR = scriptParameters.getParams().getDouble("tre_TR");
        maxT2 = scriptParameters.getParams().getDouble("max_T2");
        maxM0 = scriptParameters.getParams().getDouble("max_M0");

        Nfa_phase0 = scriptParameters.getParams().getInt("Nfa_phase_0");
        Nfa_phase180 = scriptParameters.getParams().getInt("Nfa_phase_180");

        calculateT2 = scriptParameters.getParams().getBoolean("calculate_T2");
        includeB1Map = scriptParameters.getParams().getBoolean("include_B1Map");
        performConventionalModelling = scriptParameters.getParams().getBoolean("perform_Conventional_Modelling");
        performApproxModelling = scriptParameters.getParams().getBoolean("perform_Approx_Modelling");
        performFullModelling = scriptParameters.getParams().getBoolean("perform_Full_Modelling");
        calculateM0 = scriptParameters.getParams().getBoolean("calculate_M0");
        invertT2toR2 = scriptParameters.getParams().getBoolean("invert_T2toR2");
        calculateB0 = scriptParameters.getParams().getBoolean("calculate_B0");
        performConventionalWith180Phase = scriptParameters.getParams().getBoolean("perform_ConventionalWith180Phase");
        performConventionalWith0Phase = scriptParameters.getParams().getBoolean("perform_ConventionalWith0Phase");
        geScanner = scriptParameters.getParams().getBoolean("ge_Scanner");
        siemensScanner = scriptParameters.getParams().getBoolean("siemens_Scanner");
        upperLeftCorner = scriptParameters.getParams().getBoolean("upper_LeftCorner");
        upperRightCorner = scriptParameters.getParams().getBoolean("upper_RightCorner");
        lowerLeftCorner = scriptParameters.getParams().getBoolean("lower_LeftCorner");
        lowerRightCorner = scriptParameters.getParams().getBoolean("lower_RightCorner");
        useSmartThresholding = scriptParameters.getParams().getBoolean("use_SmartThresholding");
        useHardThresholding = scriptParameters.getParams().getBoolean("use_HardThresholding");

        // souble[]
        treFA_phase0 = scriptParameters.getParams().getList("tre_FA_phase0").getAsDoubleArray();
        treFA_phase180 = scriptParameters.getParams().getList("tre_FA_phase180").getAsDoubleArray();
        // int[]
        ssfpImageIndex_phase0 = scriptParameters.getParams().getList("tre_FA_phase0").getAsIntArray();
        ssfpImageIndex_phase180 = scriptParameters.getParams().getList("sspf_Image_Index_phase180").getAsIntArray();

        t1ImageIndex = scriptParameters.getParams().getInt("t1_Image_Index");
        b1ImageIndex = scriptParameters.getParams().getInt("b1_Image_Index");
        // double[]
        simplexLineValues = scriptParameters.getParams().getList("simplex_Line_Values").getAsDoubleArray();
        simplexResiduals = scriptParameters.getParams().getList("simplex_Residuals").getAsDoubleArray();
        simplexCentre = scriptParameters.getParams().getList("simplex_Centre").getAsDoubleArray();
        reflection = scriptParameters.getParams().getList("reflection").getAsDoubleArray();
        expansion = scriptParameters.getParams().getList("expansion").getAsDoubleArray();
        contraction = scriptParameters.getParams().getList("contraction").getAsDoubleArray();
        shrink = scriptParameters.getParams().getList("shrink").getAsDoubleArray();
        // double[][]
        final ArrayList<double[]> simplexArr = new ArrayList<double[]>();

        int count = 0;
        double[] lastDouble = new double[0];

        while (scriptParameters.getParams().containsParameter("simplex_" + count)) {
            lastDouble = scriptParameters.getParams().getList("simplex_" + count++).getAsDoubleArray();
            simplexArr.add(lastDouble);
        }

        simplex = simplexArr.toArray(new double[0][]);

        // double[]
        twoPSimplexLineValues = scriptParameters.getParams().getList("two_PSimplex_Line_Values").getAsDoubleArray();
        twoPSimplexResiduals = scriptParameters.getParams().getList("tre_FA").getAsDoubleArray();
        twoPSimplexCentre = scriptParameters.getParams().getList("two_PSimplex_Centre").getAsDoubleArray();
        twoPReflection = scriptParameters.getParams().getList("two_PReflection").getAsDoubleArray();
        twoPExpansion = scriptParameters.getParams().getList("two_PExpansion").getAsDoubleArray();
        twoPContraction = scriptParameters.getParams().getList("two_PContraction").getAsDoubleArray();
        twoPShrink = scriptParameters.getParams().getList("two_PShrink").getAsDoubleArray();
        // double[][]
        final ArrayList<double[]> twoPSimplexArr = new ArrayList<double[]>();

        count = 0;
        lastDouble = new double[0];

        while (scriptParameters.getParams().containsParameter("two_PSimplex_" + count)) {
            lastDouble = scriptParameters.getParams().getList("two_PSimplex_" + count++).getAsDoubleArray();
            twoPSimplexArr.add(lastDouble);
        }

        twoPSimplex = twoPSimplexArr.toArray(new double[0][]);

        // int[]
        bestToWorst = scriptParameters.getParams().getList("best_To_Worst").getAsIntArray();

        // These are ModelImage names, one finds they are in a useful order
        final ArrayList<String> wListArr = new ArrayList<String>();

        // get the list of possible images
        final ArrayList<Parameter> parImageArr = new ArrayList<Parameter>();
        final Parameter[] parTotal = scriptParameters.getParams().getParameters();
        for (int i = 0; i < parTotal.length; i++) {
            if (parTotal[i].getType() == Parameter.PARAM_EXTERNAL_IMAGE) {
                parImageArr.add(parTotal[i]);
            }
        }

        ModelImage result = null;
        // only way of getting number of images without throwing uncatchable NullPointer
        final int numInputImages = scriptParameters.getParams().getInt("number_of_input_images");
        for (int imageNum = 0; imageNum < numInputImages; imageNum++) {
            result = scriptParameters.retrieveImage(parImageArr.get(imageNum).getLabel());
            wListArr.add(result.getImageName());
        }

        wList = wListArr.toArray(new String[0]);
        titles = wListArr.toArray(new String[0]);
    }

    @Override
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.getParams().put(ParameterFactory.newParameter("tre_TR", treTR));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_T2", maxT2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_M0", maxM0));

        scriptParameters.getParams().put(ParameterFactory.newParameter("Nfa_phase_0", Nfa_phase0));
        scriptParameters.getParams().put(ParameterFactory.newParameter("Nfa_phase_180", Nfa_phase180));

        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_T2", calculateT2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("include_B1Map", includeB1Map));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_Conventional_Modelling", performConventionalModelling));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_Approx_Modelling", performApproxModelling));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_Full_Modelling", performFullModelling));
        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_M0", calculateM0));
        scriptParameters.getParams().put(ParameterFactory.newParameter("invert_T2toR2", invertT2toR2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_B0", calculateB0));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_ConventionalWith180Phase", performConventionalWith180Phase));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_ConventionalWith0Phase", performConventionalWith0Phase));
        scriptParameters.getParams().put(ParameterFactory.newParameter("ge_Scanner", geScanner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("siemens_Scanner", siemensScanner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("upper_LeftCorner", upperLeftCorner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("upper_RightCorner", upperRightCorner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("lower_LeftCorner", lowerLeftCorner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("lower_RightCorner", lowerRightCorner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_SmartThresholding", useSmartThresholding));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_HardThresholding", useHardThresholding));

        // double[]
        if (treFA_phase0 != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("tre_FA_phase0", treFA_phase0));
        }
        if (treFA_phase180 != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("tre_FA_phase180", treFA_phase180));
        }
        // int[]
        if (ssfpImageIndex_phase0 != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("sspf_Image_Index_phase0", ssfpImageIndex_phase0));
        }
        if (ssfpImageIndex_phase180 != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("sspf_Image_Index_phase180", ssfpImageIndex_phase180));
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("t1_Image_Index", t1ImageIndex));
        scriptParameters.getParams().put(ParameterFactory.newParameter("b1_Image_Index", b1ImageIndex));
        // double[]
        if (simplexLineValues != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("simplex_Line_Values", simplexLineValues));
        }
        if (simplexResiduals != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("simplex_Residuals", simplexResiduals));
        }
        if (simplexCentre != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("simplex_Centre", simplexCentre));
        }
        if (reflection != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("reflection", reflection));
        }
        if (expansion != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("expansion", expansion));
        }
        if (contraction != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("contraction", contraction));
        }
        if (shrink != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("shrink", shrink));
        }
        // double[][], each double[] will need to be stored individually
        if (simplex != null) {
            for (int i = 0; i < simplex.length; i++) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("simplex_" + i, simplex[i]));
            }
        }
        // double[]
        if (twoPSimplexLineValues != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PSimplex_Line_Values", twoPSimplexLineValues));
        }
        if (twoPSimplexResiduals != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PSimplex_Residuals", twoPSimplexResiduals));
        }
        if (twoPSimplexCentre != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PSimplex_Centre", twoPSimplexCentre));
        }
        if (twoPReflection != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PReflection", twoPReflection));
        }
        if (twoPExpansion != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PExpansion", twoPExpansion));
        }
        if (twoPContraction != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PContraction", twoPContraction));
        }
        if (twoPShrink != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PShrink", twoPShrink));
        }
        // double[][], each double[] will need to be stored individually
        if (twoPSimplex != null) {
            for (int i = 0; i < twoPSimplex.length; i++) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("two_PSimplex_" + i, twoPSimplex[i]));
            }
        }
        // int[]
        if (bestToWorst != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("best_To_Worst", bestToWorst));
        }
        // need a count of wList
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_input_images", wList.length));
        // String[], are in fact ModelImage identifiers need to be stored in order, titles is presumed to be identical
        // for scripting
        if (wList != null) {
            for (int i = 0; i < wList.length; i++) {
                scriptParameters.storeImage(ViewUserInterface.getReference().getRegisteredImageByName(wList[i]), wList[i]);
            }
        }

        if (cAlgo != null) {
            if (includeB1Map && cAlgo.getB0ResultStack() != null) {
                scriptParameters.storeOutputImageParams(cAlgo.getB0ResultStack(), true);
            }

            if (calculateM0 && cAlgo.getM0ResultStack() != null) {
                scriptParameters.storeOutputImageParams(cAlgo.getM0ResultStack(), true);
            }

            if (invertT2toR2 && cAlgo.getR2ResultStack() != null) {
                scriptParameters.storeOutputImageParams(cAlgo.getR2ResultStack(), true);
            }

            if (calculateT2 && cAlgo.getT2ResultStack() != null) {
                scriptParameters.storeOutputImageParams(cAlgo.getT2ResultStack(), true);
            }
        }
    }

    private void init() {
        final JPanel mainPanel = new JPanel();
        final BoxLayout b = new BoxLayout(mainPanel, BoxLayout.Y_AXIS);
        mainPanel.setLayout(b);
        final Enumeration<String> imageEnum = ViewUserInterface.getReference().getRegisteredImageNames();
        final ArrayList<String> imageList = new ArrayList<String>();
        while (imageEnum.hasMoreElements()) {
            imageList.add(imageEnum.nextElement());
        }
        wList = new String[imageList.size()];
        wList = imageList.toArray(wList);
        if (wList == null || wList.length < 3) {
            MipavUtil.displayWarning("Need at least 2 SSFP images and a pre-computed T1 map to use this algorithm.");
            return;
        }
        titles = new String[wList.length];
        for (int i = 0; i < wList.length; i++) {
            final ModelImage imp = ViewUserInterface.getReference().getRegisteredImageByName(wList[i]);
            if (imp != null) {
                titles[i] = imp.getImageName();
            } else {
                titles[i] = "";
            }
        }

        algoPane = new JTabbedPane();

        guiHelp = new GuiBuilder(this);

        final JPanel methodPanel = buildMethodPanel();
        algoPane.add("Modeling method", methodPanel);

        paramPanel = null;
        algoPane.add("Parameters", paramPanel);

        conventionalPanel = null;
        algoPane.add("Conventional modeling", conventionalPanel);

        advancedPanel = null;
        algoPane.add("Advanced modeling", advancedPanel);

        mainPanel.add(algoPane, b);

        mainPanel.add(guiHelp.buildOKCancelPanel(), b);

        getContentPane().add(mainPanel);

        algoPane.addChangeListener(this);

        setTitle(title);

        pack();
        validate();

        setSize(new Dimension(700, 700));

        setVisible(true);
    }

    @Override
    protected void callAlgorithm() {
        // Make algorithm
        cAlgo = new AlgorithmTreT2(this, treFA_phase0, treFA_phase180, ssfpImageIndex_phase0, ssfpImageIndex_phase180, t1ImageIndex, b1ImageIndex,
                simplexLineValues, simplexResiduals, simplexCentre, reflection, expansion, contraction, shrink, simplex, twoPSimplexLineValues,
                twoPSimplexResiduals, twoPSimplexCentre, twoPReflection, twoPExpansion, twoPContraction, twoPShrink, twoPSimplex, bestToWorst, wList);

        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed of failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        cAlgo.addListener(this);

        createProgressBar(title, cAlgo);
        progressBar.addActionListener(this);

        // Hide dialog
        setVisible(false);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (cAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {

            cAlgo.run();
        }
    }

    public double getTreTR() {
        return treTR;
    }

    public double getMaxT2() {
        return maxT2;
    }

    public double getMaxM0() {
        return maxM0;
    }

    public int getNfa_phase0() {
        return Nfa_phase0;
    }

    public int getNfa_phase180() {
        return Nfa_phase180;
    }

    public boolean isCalculateT2() {
        return calculateT2;
    }

    public boolean isIncludeB1Map() {
        return includeB1Map;
    }

    public boolean isPerformConventionalModelling() {
        return performConventionalModelling;
    }

    public boolean isPerformApproxModelling() {
        return performApproxModelling;
    }

    public boolean isPerformFullModelling() {
        return performFullModelling;
    }

    public boolean isCalculateM0() {
        return calculateM0;
    }

    public boolean isInvertT2toR2() {
        return invertT2toR2;
    }

    public boolean isCalculateB0() {
        return calculateB0;
    }

    public boolean isPerformConventionalWith180Phase() {
        return performConventionalWith180Phase;
    }

    public boolean isPerformConventionalWith0Phase() {
        return performConventionalWith0Phase;
    }

    public boolean isGeScanner() {
        return geScanner;
    }

    public boolean isSiemensScanner() {
        return siemensScanner;
    }

    public boolean isUpperLeftCorner() {
        return upperLeftCorner;
    }

    public boolean isUpperRightCorner() {
        return upperRightCorner;
    }

    public boolean isLowerLeftCorner() {
        return lowerLeftCorner;
    }

    public boolean isLowerRightCorner() {
        return lowerRightCorner;
    }

    public boolean isUseSmartThresholding() {
        return useSmartThresholding;
    }

    public boolean isUseHardThresholding() {
        return useHardThresholding;
    }

    public float getNoiseScale() {
        return noiseScale;
    }

    public float getHardNoiseThreshold() {
        return hardNoiseThreshold;
    }

    public JPanel buildMethodPanel() {
        final JPanel panel = new JPanel();
        final LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);

        nfaPhase0Text = guiHelp.buildDecimalField("Number of SSFP Flip Angles (0 Phase Increment):", Nfa_phase0);
        nfaPhase180Text = guiHelp.buildDecimalField("Number of SSFP Flip Angles (180 Phase Increment):", Nfa_phase180);
        doConventionalT2Button = guiHelp.buildRadioButton("Perform Conventional TRE-T2 Modeling", performConventionalModelling);
        doApproximateT2Button = guiHelp.buildRadioButton("Perform Approximate Modeling", performApproxModelling);
        doFullT2Button = guiHelp.buildRadioButton("Perform Full Modelling of the Signal (slow but accurate)", performFullModelling);
        doFullT2Button.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                showB0Box.getParent().setVisible(doFullT2Button.isSelected());
            }
        });

        doB1MapBox = guiHelp.buildCheckBox("<html>Use Calculated B<sub>1</sub> Map</html>", includeB1Map);
        doB1MapBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                preCalcB1MapBox.getParent().setVisible(doB1MapBox.isSelected());
            }
        });

        processType = new ButtonGroup();
        processType.add(doConventionalT2Button);
        processType.add(doApproximateT2Button);
        processType.add(doFullT2Button);

        panel.add(nfaPhase0Text.getParent(), panelLayout);
        panel.add(nfaPhase180Text.getParent(), panelLayout);
        panel.add(doConventionalT2Button.getParent(), panelLayout);
        panel.add(doApproximateT2Button.getParent(), panelLayout);
        panel.add(doFullT2Button.getParent(), panelLayout);
        panel.add(doB1MapBox.getParent(), panelLayout);

        return panel;
    }

    public JPanel buildAdvancedPanel() {
        final JPanel panel = new JPanel();
        final LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);

        if (treFA_phase0 == null || treFA_phase0.length != Nfa_phase0) {
            treFA_phase0 = new double[Nfa_phase0];
        }
        if (ssfpImageIndex_phase0 == null || ssfpImageIndex_phase0.length != Nfa_phase0) {
            ssfpImageIndex_phase0 = new int[Nfa_phase0];
        }
        if (treFA_phase180 == null || treFA_phase180.length != Nfa_phase180) {
            treFA_phase180 = new double[Nfa_phase180];
        }
        if (ssfpImageIndex_phase180 == null || ssfpImageIndex_phase180.length != Nfa_phase180) {
            ssfpImageIndex_phase180 = new int[Nfa_phase180];
        }

        phaseImageList = new JComboBox[Nfa_phase0];
        phaseFlipAngleList = new JTextField[Nfa_phase0];

        for (int i = 0; i < Nfa_phase0; i++) {
            phaseImageList[i] = guiHelp.buildComboBox("Phase 0, SSFP Image #" + i, titles, i);
            panel.add(phaseImageList[i].getParent(), panelLayout);

            phaseFlipAngleList[i] = guiHelp.buildDecimalField("Phase 0, Flip Angle #" + i, treFA_phase0[i]);
            panel.add(phaseImageList[i].getParent(), panelLayout);
        }

        phase180ImageList = new JComboBox[Nfa_phase180];
        phase180FlipAngleList = new JTextField[Nfa_phase180];
        for (int i = 0; i < Nfa_phase180; i++) {
            phase180ImageList[i] = guiHelp.buildComboBox("Phase 180, SSFP Image #" + i, titles, i);
            panel.add(phase180ImageList[i].getParent(), panelLayout);

            phase180FlipAngleList[i] = guiHelp.buildDecimalField("Phase 180, Flip Angle #" + i, treFA_phase180[i]);
            panel.add(phase180FlipAngleList[i].getParent(), panelLayout);
        }

        ssfpRepTime = guiHelp.buildDecimalField("SSFP Repetition Time (ms):", treTR);
        panel.add(ssfpRepTime.getParent(), panelLayout);

        // preCalcT1Box = guiHelp.buildComboBox("<html>Pre-Calculated T<sub>1</sub> Map</html>", titles, 0);
        panel.add(preCalcT1Box.getParent(), panelLayout);

        panel.add(preCalcB1MapBox.getParent(), panelLayout);

        isGEScannerButton = guiHelp.buildRadioButton("Scan Performed on a GE Scanner", geScanner);
        isSiemensButton = guiHelp.buildRadioButton("Scan Performed on a Siemens Scanner", siemensScanner);

        final ButtonGroup scannerType = new ButtonGroup();
        scannerType.add(isGEScannerButton);
        scannerType.add(isSiemensButton);

        panel.add(isGEScannerButton.getParent(), panelLayout);
        panel.add(isSiemensButton.getParent(), panelLayout);

        return panel;
    }

    public JPanel buildConventionalPanel() {
        final JPanel panel = new JPanel();
        final LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);

        if (treFA_phase0 == null || treFA_phase0.length != Nfa_phase0) {
            treFA_phase0 = new double[Nfa_phase0];
        }
        if (ssfpImageIndex_phase0 == null || ssfpImageIndex_phase0.length != Nfa_phase0) {
            ssfpImageIndex_phase0 = new int[Nfa_phase0];
        }
        if (treFA_phase180 == null || treFA_phase180.length != Nfa_phase180) {
            treFA_phase180 = new double[Nfa_phase180];
        }
        if (ssfpImageIndex_phase180 == null || ssfpImageIndex_phase180.length != Nfa_phase180) {
            ssfpImageIndex_phase180 = new int[Nfa_phase180];
        }

        final int maxPhase = Nfa_phase0 > Nfa_phase180 ? Nfa_phase0 : Nfa_phase180;
        phaseImageList = new JComboBox[maxPhase];
        phaseFlipAngleList = new JTextField[maxPhase];
        if (performConventionalWith0Phase == true) {
            for (int i = 0; i < Nfa_phase0; i++) {
                phaseImageList[i] = guiHelp.buildComboBox("Phase 0, SSFP Image #" + i, titles, i);
                panel.add(phaseImageList[i].getParent(), panelLayout);

                phaseFlipAngleList[i] = guiHelp.buildDecimalField("Phase 0, Flip Angle #" + i, treFA_phase0[i]);
                panel.add(phaseFlipAngleList[i].getParent(), panelLayout);
            }
        } else {
            for (int i = 0; i < Nfa_phase180; i++) {
                phaseImageList[i] = guiHelp.buildComboBox("Phase 180, SSFP Image #" + i, titles, i);
                panel.add(phaseImageList[i].getParent(), panelLayout);

                phaseFlipAngleList[i] = guiHelp.buildDecimalField("Phase 180, Flip Angle #" + i, treFA_phase180[i]);
                panel.add(phaseFlipAngleList[i].getParent(), panelLayout);
            }
        }
        ssfpRepTime = guiHelp.buildDecimalField("SSFP Repetition Time (ms):", treTR);
        panel.add(ssfpRepTime.getParent(), panelLayout);

        preCalcT1Box = guiHelp.buildComboBox("<html>Pre-Calculated T<sub>1</sub> Map</html>", titles, 0);
        panel.add(preCalcT1Box.getParent(), panelLayout);

        preCalcB1MapBox = guiHelp.buildComboBox("<html>Pre-Calculated B<sub>1</sub> Map</html>", titles, 0);
        panel.add(preCalcB1MapBox.getParent(), panelLayout);

        return panel;
    }

    public JPanel buildParamPanel() {
        final JPanel panel = new JPanel();
        final LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);

        maxT2Text = guiHelp.buildDecimalField("Maximum Allowable T2:", maxT2);
        maxM0Text = guiHelp.buildDecimalField("Maximum Allowable M0:", maxM0);
        doT2Box = guiHelp.buildCheckBox("Show T2 Map", calculateT2);
        doMoBox = guiHelp.buildCheckBox("Show M0 Map", calculateM0);
        panel.add(maxT2Text.getParent(), panelLayout);
        panel.add(maxM0Text.getParent(), panelLayout);
        panel.add(doT2Box.getParent(), panelLayout);
        panel.add(doMoBox.getParent(), panelLayout);

        showB0Box = guiHelp.buildCheckBox("<html>Show B<sub>0</sub> Map</html>", calculateB0);
        panel.add(showB0Box.getParent(), panelLayout);

        showR2Box = guiHelp.buildCheckBox("<html>Show R<sub>2</sub> Map</html>", invertT2toR2);
        panel.add(showR2Box.getParent(), panelLayout);

        return panel;
    }

    @Override
    public void stateChanged(final ChangeEvent e) {
        final Object source = e.getSource();
        if (source == algoPane) {
            if (algoPane.getSelectedComponent() == paramPanel) {
                paramPanel = buildParamPanel();
                algoPane.setComponentAt(algoPane.getSelectedIndex(), paramPanel);
            } else if (algoPane.getSelectedComponent() == conventionalPanel) {
                conventionalPanel = buildConventionalPanel();
                algoPane.setComponentAt(algoPane.getSelectedIndex(), conventionalPanel);
            } else if (algoPane.getSelectedComponent() == advancedPanel) {
                advancedPanel = buildAdvancedPanel();
                algoPane.setComponentAt(algoPane.getSelectedIndex(), advancedPanel);
            }
        }
    }
}
