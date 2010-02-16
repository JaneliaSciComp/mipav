package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmDespotT1;
import gov.nih.mipav.model.algorithms.AlgorithmDespotT2;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.Parameter;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;

public class JDialogDespotT2 extends JDialogScriptableBase implements AlgorithmInterface {

    private static String title = "DESPOT2 T2 Mapper";
    private double despotTR = 5.00;
    private double maxT2 = 1000;
    private double maxMo = 10000;
    private double[] despotFA_phase0;
    private double[] despotFA_phase180;
    
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
    private boolean calculateMo = true;
    private boolean invertT2toR2 = false;
    private boolean calculateBo = false;
    
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
    private float noiseScale = (float) 1.00;
    private float hardNoiseThreshold = (float) 0.00;
    
    private String[] wList;
    private String[] titles;
    
    private AlgorithmDespotT2 cAlgo;

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogDespotT2() { }

    /**
     * Construct the barrel/pin cushion correction dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogDespotT2(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        run();
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof AlgorithmDespotT2) {
            Preferences.debug("DespotT2: " + algorithm.getElapsedTime());
        } 
        
        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (cAlgo != null) {
            cAlgo.finalize();
            cAlgo = null;
        }

        dispose();
    }

    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("Cancel")) {
            cAlgo.interrupt();
        } 
    }
    
    protected void setGUIFromParams() {
        despotTR = scriptParameters.getParams().getDouble("despot_TR");
        maxT2 = scriptParameters.getParams().getDouble("max_T2");
        maxMo = scriptParameters.getParams().getDouble("max_Mo");
        
        Nfa_phase0 = scriptParameters.getParams().getInt("Nfa_phase_0");
        Nfa_phase180 = scriptParameters.getParams().getInt("Nfa_phase_180");
        
        calculateT2 = scriptParameters.getParams().getBoolean("calculate_T2");
        includeB1Map = scriptParameters.getParams().getBoolean("include_B1Map");
        performConventionalModelling = scriptParameters.getParams().getBoolean("perform_Conventional_Modelling");
        performApproxModelling = scriptParameters.getParams().getBoolean("perform_Approx_Modelling");
        performFullModelling = scriptParameters.getParams().getBoolean("perform_Full_Modelling");
        calculateMo = scriptParameters.getParams().getBoolean("calculate_Mo");
        invertT2toR2 = scriptParameters.getParams().getBoolean("invert_T2toR2");
        calculateBo = scriptParameters.getParams().getBoolean("calculate_Bo");
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
        
        //souble[]
        despotFA_phase0 = scriptParameters.getParams().getList("despot_FA_phase0").getAsDoubleArray();
        despotFA_phase180 = scriptParameters.getParams().getList("despot_FA_phase180").getAsDoubleArray();
        //int[]
        ssfpImageIndex_phase0 = scriptParameters.getParams().getList("despot_FA_phase0").getAsIntArray();
        ssfpImageIndex_phase180 = scriptParameters.getParams().getList("sspf_Image_Index_phase180").getAsIntArray();
        
        t1ImageIndex = scriptParameters.getParams().getInt("t1_Image_Index");
        b1ImageIndex = scriptParameters.getParams().getInt("b1_Image_Index");
        //double[]
        simplexLineValues = scriptParameters.getParams().getList("simplex_Line_Values").getAsDoubleArray();
        simplexResiduals = scriptParameters.getParams().getList("simplex_Residuals").getAsDoubleArray();
        simplexCentre = scriptParameters.getParams().getList("simplex_Centre").getAsDoubleArray();
        reflection = scriptParameters.getParams().getList("reflection").getAsDoubleArray();
        expansion = scriptParameters.getParams().getList("expansion").getAsDoubleArray();
        contraction = scriptParameters.getParams().getList("contraction").getAsDoubleArray();
        shrink = scriptParameters.getParams().getList("shrink").getAsDoubleArray();
        //double[][]
        ArrayList<double[]> simplexArr = new ArrayList<double[]>();
        
        int count = 0;
        double[] lastDouble = new double[0];
        
        while(scriptParameters.getParams().containsParameter("simplex_"+count)) {
            lastDouble = scriptParameters.getParams().getList("simplex_"+count++).getAsDoubleArray();
            simplexArr.add(lastDouble);
        }
        
        simplex = simplexArr.toArray(new double[0][]);
        
        //double[]
        twoPSimplexLineValues = scriptParameters.getParams().getList("two_PSimplex_Line_Values").getAsDoubleArray();
        twoPSimplexResiduals = scriptParameters.getParams().getList("despot_FA").getAsDoubleArray();
        twoPSimplexCentre = scriptParameters.getParams().getList("two_PSimplex_Centre").getAsDoubleArray();
        twoPReflection = scriptParameters.getParams().getList("two_PReflection").getAsDoubleArray();
        twoPExpansion = scriptParameters.getParams().getList("two_PExpansion").getAsDoubleArray();
        twoPContraction = scriptParameters.getParams().getList("two_PContraction").getAsDoubleArray();
        twoPShrink = scriptParameters.getParams().getList("two_PShrink").getAsDoubleArray();
        //double[][]
        ArrayList<double[]> twoPSimplexArr = new ArrayList<double[]>();
        
        count = 0;
        lastDouble = new double[0];
        
        while(scriptParameters.getParams().containsParameter("two_PSimplex_"+count)) {
            lastDouble = scriptParameters.getParams().getList("two_PSimplex_"+count++).getAsDoubleArray();
            twoPSimplexArr.add(lastDouble);
        }
        
        twoPSimplex = twoPSimplexArr.toArray(new double[0][]);
        
        //int[]
        bestToWorst = scriptParameters.getParams().getList("best_To_Worst").getAsIntArray();
      
        //These are ModelImage names, one finds they are in a useful order
        ArrayList<String> wListArr = new ArrayList<String>();
        
        //get the list of possible images
        ArrayList<Parameter> parImageArr = new ArrayList<Parameter>();
        Parameter[] parTotal = scriptParameters.getParams().getParameters();
        for(int i=0; i<parTotal.length; i++) {
        	if(parTotal[i].getType() == Parameter.PARAM_EXTERNAL_IMAGE) {
        		parImageArr.add(parTotal[i]);
        	}
        }
        
        ModelImage result = null;
        //only way of getting number of images without  throwing uncatchable NullPointer
        int numInputImages = scriptParameters.getParams().getInt("number_of_input_images");
        for(int imageNum=0; imageNum < numInputImages; imageNum++) {
        	result = scriptParameters.retrieveImage(parImageArr.get(imageNum).getLabel());
        	wListArr.add(result.getImageName());
        }

        wList = wListArr.toArray(new String[0]);
        titles = wListArr.toArray(new String[0]);   
    }

    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.getParams().put(ParameterFactory.newParameter("despot_TR", despotTR));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_T2", maxT2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_Mo", maxMo));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("Nfa_phase_0", Nfa_phase0));
        scriptParameters.getParams().put(ParameterFactory.newParameter("Nfa_phase_180", Nfa_phase180));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_T2", calculateT2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("include_B1Map", includeB1Map));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_Conventional_Modelling", performConventionalModelling));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_Approx_Modelling", performApproxModelling));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_Full_Modelling", performFullModelling));
        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_Mo", calculateMo));
        scriptParameters.getParams().put(ParameterFactory.newParameter("invert_T2toR2", invertT2toR2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_Bo", calculateBo));
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
        
        //double[]
        if (despotFA_phase0 != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("despot_FA_phase0", despotFA_phase0));
        }
        if(despotFA_phase180 != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("despot_FA_phase180", despotFA_phase180));
        }
        //int[]
        if(ssfpImageIndex_phase0 != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("sspf_Image_Index_phase0", ssfpImageIndex_phase0));
        }
        if(ssfpImageIndex_phase180 != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("sspf_Image_Index_phase180", ssfpImageIndex_phase180));
        }
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("t1_Image_Index", t1ImageIndex));
        scriptParameters.getParams().put(ParameterFactory.newParameter("b1_Image_Index", b1ImageIndex));
        //double[]
        if(simplexLineValues != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("simplex_Line_Values", simplexLineValues));
        }
        if(simplexResiduals != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("simplex_Residuals", simplexResiduals));
        }
        if(simplexCentre != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("simplex_Centre", simplexCentre));
        }
        if(reflection != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("reflection", reflection));
        }
        if(expansion != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("expansion", expansion));
        }
        if(contraction != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("contraction", contraction));
        }
        if(shrink != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("shrink", shrink));
        }
        //double[][], each double[] will need to be stored individually
        if(simplex != null) {
            for(int i=0; i<simplex.length; i++) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("simplex_"+i, simplex[i]));
            }
        }  
        //double[]
        if(twoPSimplexLineValues != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PSimplex_Line_Values", twoPSimplexLineValues));
        }
        if(twoPSimplexResiduals != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PSimplex_Residuals", twoPSimplexResiduals));
        }
        if(twoPSimplexCentre != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PSimplex_Centre", twoPSimplexCentre));
        }
        if(twoPReflection != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PReflection", twoPReflection));
        }
        if(twoPExpansion != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PExpansion", twoPExpansion));
        }
        if(twoPContraction != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PContraction", twoPContraction));
        }
        if(twoPShrink != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("two_PShrink", twoPShrink));
        }
        //double[][], each double[] will need to be stored individually
        if(twoPSimplex != null) {
            for(int i=0; i<twoPSimplex.length; i++) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("two_PSimplex_"+i, twoPSimplex[i]));
            }
        }
        //int[]
        if(bestToWorst != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("best_To_Worst", bestToWorst));
        }
        //need a count of wList
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_input_images", wList.length));
        //String[], are in fact ModelImage identifiers need to be stored in order, titles is presumed to be identical for scripting
        if(wList != null) {
            for(int i=0; i<wList.length; i++) {
            	scriptParameters.storeImage(ViewUserInterface.getReference().getRegisteredImageByName(wList[i]), wList[i]);
            }
        }
        
        if(cAlgo != null) {
        	if(includeB1Map && cAlgo.getBoResultStack() != null) {
        		scriptParameters.storeImageInRecorder(cAlgo.getBoResultStack());
        	}
        	
        	if(calculateMo && cAlgo.getMoResultStack() != null) {
        		scriptParameters.storeImageInRecorder(cAlgo.getMoResultStack());
        	}
        	
        	if(invertT2toR2 && cAlgo.getR2ResultStack() != null) {
        		scriptParameters.storeImageInRecorder(cAlgo.getR2ResultStack());
        	}
        	
        	if(calculateT2 && cAlgo.getT2ResultStack() != null) {
        		scriptParameters.storeImageInRecorder(cAlgo.getT2ResultStack());
        	}
        }
    }

    private void run() {
        
        Enumeration<String> imageEnum = ViewUserInterface.getReference().getRegisteredImageNames();
        ArrayList<String> imageList = new ArrayList<String>();
        while(imageEnum.hasMoreElements()) {
            imageList.add(imageEnum.nextElement());
        }
        wList = new String[imageList.size()];
        wList = imageList.toArray(wList);
        if (wList==null || wList.length<3) {
            MipavUtil.displayWarning("Need at least 2 SSFP images and a pre-computed T1 map to use this algorithm.");
            return;
        }
        titles = new String[wList.length];
        for (int i=0; i<wList.length; i++) {
            ModelImage imp = ViewUserInterface.getReference().getRegisteredImageByName(wList[i]);
            if (imp!=null)
                titles[i] = imp.getImageName();
            else
                titles[i] = "";
        }
        
        if (!showDialog()) 
            return;
        
        
        despotFA_phase0 = new double[Nfa_phase0];
        despotFA_phase180 = new double[Nfa_phase180];
        ssfpImageIndex_phase0 = new int[Nfa_phase0];
        ssfpImageIndex_phase180 = new int[Nfa_phase180];
        
        if (performConventionalModelling == false) {
            if (!showLongDialog()) return; 
        }
        else {
            if (!showConventionalLongDialog()) return;
        }
        
        if (!showSpecificsDialog()) return;     
        
        try {  
            callAlgorithm();
        } catch (OutOfMemoryError x) {
    
            System.gc();
            MipavUtil.displayError("Dialog Circle Generation: unable to allocate enough memory");
    
            return;
        }
        
    }
    
    protected void callAlgorithm() {
        // Make algorithm
        cAlgo = new AlgorithmDespotT2(this, despotFA_phase0,
                despotFA_phase180, ssfpImageIndex_phase0,
                ssfpImageIndex_phase180, t1ImageIndex, b1ImageIndex,
                simplexLineValues, simplexResiduals,
                simplexCentre, reflection, expansion,
                contraction, shrink, simplex,
                twoPSimplexLineValues, twoPSimplexResiduals,
                twoPSimplexCentre, twoPReflection,
                twoPExpansion, twoPContraction,
                twoPShrink, twoPSimplex, bestToWorst,
                wList);

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
    
    public double getDespotTR() {
        return despotTR;
    }

    public double getMaxT2() {
        return maxT2;
    }

    public double getMaxMo() {
        return maxMo;
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

    public boolean isCalculateMo() {
        return calculateMo;
    }

    public boolean isInvertT2toR2() {
        return invertT2toR2;
    }

    public boolean isCalculateBo() {
        return calculateBo;
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

    public boolean showDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT2: General Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JTextField field1 = guiHelp.buildDecimalField("Number of SSFP Flip Angles (0 Phase Increment):", Nfa_phase0);
        JTextField field2 = guiHelp.buildDecimalField("Number of SSFP Flip Angles (180 Phase Increment):", Nfa_phase180);
        JRadioButton button1 = guiHelp.buildRadioButton("Perform Conventional DESPOT2 Modeling", performConventionalModelling);
        JRadioButton button2 = guiHelp.buildRadioButton("Perform Approximate Modeling", performApproxModelling);
        JRadioButton button3 = guiHelp.buildRadioButton("Perform Full Modelling of the Signal (slow but accurate)", performFullModelling);
        JCheckBox box1 = guiHelp.buildCheckBox("Use Calculated B1 Map", includeB1Map);
        
        ButtonGroup processType = new ButtonGroup();
        processType.add(button1);
        processType.add(button2);
        processType.add(button3);
        
        panel.add(field1.getParent(), panelLayout);
        panel.add(field2.getParent(), panelLayout);
        panel.add(button1.getParent(), panelLayout);
        panel.add(button2.getParent(), panelLayout);
        panel.add(button3.getParent(), panelLayout);
        panel.add(box1.getParent(), panelLayout);
        
        dialog.add(panel, BorderLayout.CENTER);
        dialog.add(guiHelp.buildOKCancelPanel(), BorderLayout.SOUTH);
        dialog.setLocationRelativeTo(null);
        dialog.pack();
        dialog.setModal(true);
        dialog.setVisible(true);
        
        if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL) || 
                guiHelp.getExitStatus().equals(ExitStatus.INCOMPLETE)) {
            return false;
        }
        
        Nfa_phase0 = (int) Double.valueOf(field1.getText()).doubleValue();
        Nfa_phase180 = (int) Double.valueOf(field2.getText()).doubleValue();
        performConventionalModelling = button1.isSelected();
        performApproxModelling = button2.isSelected();
        performFullModelling = button3.isSelected();
        includeB1Map = box1.isSelected();
    
        if(processType.getSelection() == null) {
            MipavUtil.displayInfo("Please select a processing method");
            dialog.dispose();
            return showDialog();
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
            }
            else {
                performConventionalWith0Phase = false;
                performConventionalWith180Phase = true;
            }
        }
        return true;
    }
    
    public boolean showLongDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT2-AMFM: Image Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JComboBox[] comboArr1 = new JComboBox[Nfa_phase0];
        JTextField[] fieldArr1 = new JTextField[Nfa_phase0];

        for (int i=0; i<Nfa_phase0; i++) {
            comboArr1[i] = guiHelp.buildComboBox("Phase 0, SSFP Image #"+i, titles, i);
            panel.add(comboArr1[i].getParent(), panelLayout);
            
            fieldArr1[i] = guiHelp.buildDecimalField("Phase 0, Flip Angle #"+i, despotFA_phase0[i]);
            panel.add(comboArr1[i].getParent(), panelLayout);
        }
        
        JComboBox[] comboArr2 = new JComboBox[Nfa_phase180];
        JTextField[] fieldArr2 = new JTextField[Nfa_phase180];
        for (int i=0; i<Nfa_phase180; i++) {
            comboArr2[i] = guiHelp.buildComboBox("Phase 180, SSFP Image #"+i, titles, i);
            panel.add(comboArr2[i].getParent(), panelLayout);
            
            fieldArr2[i] = guiHelp.buildDecimalField("Phase 180, Flip Angle #"+i, despotFA_phase180[i]);
            panel.add(fieldArr2[i].getParent(), panelLayout);
        }
        
        JTextField field1 = guiHelp.buildDecimalField("SSFP Repetition Time (ms):", despotTR);
        panel.add(field1.getParent(), panelLayout);
        
        JComboBox combo1 = guiHelp.buildComboBox("Pre-Calculated T1 Map", titles, 0);
        panel.add(combo1.getParent(), panelLayout);
        
        JComboBox comboOpt = null;
        if (includeB1Map) {
            comboOpt = guiHelp.buildComboBox("Pre-Calculated B1 Map", titles, 0);
            panel.add(comboOpt.getParent(), panelLayout);
        }
        
        JRadioButton button1 = guiHelp.buildRadioButton("Scan Performed on a GE Scanner", geScanner);
        JRadioButton button2 = guiHelp.buildRadioButton("Scan Performed on a Siemens Scanner", siemensScanner);
        
        ButtonGroup scannerType = new ButtonGroup();
        scannerType.add(button1);
        scannerType.add(button2);
        
        panel.add(button1.getParent(), panelLayout);
        panel.add(button2.getParent(), panelLayout);
        
        dialog.add(panel, BorderLayout.CENTER);
        dialog.add(guiHelp.buildOKCancelPanel(), BorderLayout.SOUTH);
        dialog.setLocationRelativeTo(null);
        dialog.pack();
        dialog.setModal(true);
        dialog.setVisible(true);
        
        if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL) || 
                guiHelp.getExitStatus().equals(ExitStatus.INCOMPLETE)) {
            return false;
        }
        
        for (int i=0; i<Nfa_phase0; i++) {
            ssfpImageIndex_phase0[i] = comboArr1[i].getSelectedIndex();
            despotFA_phase0[i] = Float.valueOf(fieldArr1[i].getText()).floatValue();
        }
        for (int i=0; i<Nfa_phase180; i++) {
            ssfpImageIndex_phase180[i] = comboArr2[i].getSelectedIndex();
            despotFA_phase180[i] = Float.valueOf(fieldArr2[i].getText()).floatValue();
        }
        despotTR = Double.valueOf(field1.getText()).doubleValue();
        
        t1ImageIndex = combo1.getSelectedIndex();
        if (includeB1Map) {
            b1ImageIndex = comboOpt.getSelectedIndex();
        }
        
        geScanner = button1.isSelected();
        siemensScanner = button2.isSelected();
        
        if(scannerType.getSelection() == null) {
            MipavUtil.displayInfo("Please select a scanner type");
            dialog.dispose();
            return showLongDialog();
        }
        
        if (geScanner == true) {
            siemensScanner = false;
        }
        if (siemensScanner == true) {
            geScanner = false;
        }
        
        return true;
   }
    
    public boolean showConventionalLongDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT2: Image Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        int maxPhase = Nfa_phase0 > Nfa_phase180 ? Nfa_phase0 : Nfa_phase180;
        JComboBox[] comboArr = new JComboBox[maxPhase];
        JTextField[] fieldArr = new JTextField[maxPhase];
        if (performConventionalWith0Phase == true) {
            for (int i=0; i<Nfa_phase0; i++) {
                comboArr[i] = guiHelp.buildComboBox("Phase 0, SSFP Image #"+i, titles, i);
                panel.add(comboArr[i].getParent(), panelLayout);
                
                fieldArr[i] = guiHelp.buildDecimalField("Phase 0, Flip Angle #"+i, despotFA_phase0[i]);
                panel.add(fieldArr[i].getParent(), panelLayout);
            }
        }
        else {
            for (int i=0; i<Nfa_phase180; i++) {
                comboArr[i] = guiHelp.buildComboBox("Phase 180, SSFP Image #"+i, titles, i);
                panel.add(comboArr[i].getParent(), panelLayout);
                
                fieldArr[i] = guiHelp.buildDecimalField("Phase 180, Flip Angle #"+i, despotFA_phase180[i]);
                panel.add(fieldArr[i].getParent(), panelLayout);
            }
        }
        JTextField field1 = guiHelp.buildDecimalField("SSFP Repetition Time (ms):", despotTR);
        panel.add(field1.getParent(), panelLayout);
        
        JComboBox combo1 = guiHelp.buildComboBox("Pre-Calculated T1 Map", titles, 0);
        panel.add(combo1.getParent(), panelLayout);
        
        JComboBox comboOpt = null;
        if (includeB1Map) {
            comboOpt = guiHelp.buildComboBox("Pre-Calculated B1 Map", titles, 0);
        }
        
        dialog.add(panel, BorderLayout.CENTER);
        dialog.add(guiHelp.buildOKCancelPanel(), BorderLayout.SOUTH);
        dialog.setLocationRelativeTo(null);
        dialog.setModal(true);
        dialog.pack();
        dialog.setVisible(true);
        
        if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL) || 
                guiHelp.getExitStatus().equals(ExitStatus.INCOMPLETE)) {
            return false;
        }
        
        if (performConventionalWith0Phase == true) {
            for (int i=0; i<Nfa_phase0; i++) {
                ssfpImageIndex_phase0[i] = comboArr[i].getSelectedIndex();
                despotFA_phase0[i] = Float.valueOf(fieldArr[i].getText()).floatValue();
            }
        }
        else {
            for (int i=0; i<Nfa_phase180; i++) {
                ssfpImageIndex_phase180[i] = comboArr[i].getSelectedIndex();
                despotFA_phase180[i] = Float.valueOf(fieldArr[i].getText()).floatValue();
            }
        }
        despotTR = Double.valueOf(field1.getText()).doubleValue();
        
        t1ImageIndex = combo1.getSelectedIndex();
        if (includeB1Map) {
            b1ImageIndex = comboOpt.getSelectedIndex();
        }
        
        return true;
    }
   
    public boolean showSpecificsDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT2: Other Specifics");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JTextField field1 = guiHelp.buildDecimalField("Maximum Allowable T2:", maxT2);
        JTextField field2 = guiHelp.buildDecimalField("Maximum Allowable Mo:", maxMo);
        JCheckBox box1 = guiHelp.buildCheckBox("Show T2 Map", calculateT2);
        JCheckBox box2 = guiHelp.buildCheckBox("Show Mo Map", calculateMo);
        panel.add(field1.getParent(), panelLayout);
        panel.add(field2.getParent(), panelLayout);
        panel.add(box1.getParent(), panelLayout);
        panel.add(box2.getParent(), panelLayout);
        
        JCheckBox boxOpt = null;
        if (performFullModelling == true) {
            boxOpt = guiHelp.buildCheckBox("Show Bo Map", calculateBo);
            panel.add(boxOpt.getParent(), panelLayout);
        }
        JCheckBox box4 = guiHelp.buildCheckBox("Show R2 Map", invertT2toR2);
        panel.add(box4.getParent(), panelLayout);
        
        dialog.add(panel, BorderLayout.CENTER);
        dialog.add(guiHelp.buildOKCancelPanel(), BorderLayout.SOUTH);
        dialog.setLocationRelativeTo(null);
        dialog.setModal(true);
        dialog.pack();
        dialog.setVisible(true);
        
        if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL) || 
                guiHelp.getExitStatus().equals(ExitStatus.INCOMPLETE)) {
            return false;
        }
        
        maxT2 = Double.valueOf(field1.getText()).doubleValue();
        maxMo = Double.valueOf(field2.getText()).doubleValue();
        calculateT2 = box1.isSelected();
        calculateMo = box2.isSelected();
        if (performFullModelling == true) {
            calculateBo = boxOpt.isSelected();
        }
        invertT2toR2 = box4.isSelected();
        
        return true;
    }
    
public enum ExitStatus {
        
        /**Ok button pressed and listener conditions passed*/
        OK_SUCCESS,
        
        /**Ok button pressed and listener conditions failed*/
        OK_FAIL,
        
        /**Ok button pressed*/
        OK,
        
        /**Cancel button pressed*/
        CANCEL,
        
        /**Yes button pressed*/
        YES,
        
        /**No button pressed*/
        NO,
        
        /**Gui has yet to exit*/
        INCOMPLETE
    }
    
    /**
     * Provides methods for quickly building panel components. I can think of many other (better)
     * ways to do this, but for the ImageJ port this works well for now.
     * 
     * @author senseneyj
     *
     */
    private class GuiBuilder implements ActionListener {
        
        public static final int GUI_BUILDER_OK_ID = ActionEvent.RESERVED_ID_MAX + 20;

        private ArrayList<ActionListener> listenerList;
        
        private boolean passedListeners;

        private ExitStatus exit;
        
        private JButton ok, cancel, yes, no;
        
        private JDialog parent;
        
        public GuiBuilder(JDialog parent) {
            this.parent = parent;
            this.listenerList = new ArrayList<ActionListener>();
            this.exit = ExitStatus.INCOMPLETE;
        }
        
        public ExitStatus getExitStatus() {
            return exit;
        }
        
        public ActionListener[] getListenerList() {
            ActionListener[] list = new ActionListener[listenerList.size()];
            for(int i=0; i<listenerList.size(); i++) {
                list[i] = listenerList.get(i);
            }
            return list;
        }
        
        public JRadioButton buildRadioButton(String label, boolean selected) {
            FlowLayout f = new FlowLayout();
            f.setAlignment(FlowLayout.LEFT);
            JPanel radioPanel = new JPanel(f);
            JRadioButton radioButton = new JRadioButton(label);
            radioButton.setSelected(selected);
            radioPanel.add(radioButton);
            return radioButton;
        }
        
        public JCheckBox buildCheckBox(String label, boolean selected) {
            FlowLayout f = new FlowLayout();
            f.setAlignment(FlowLayout.LEFT);
            JPanel checkPanel = new JPanel(f);
            JCheckBox checkBox = new JCheckBox(label);
            checkBox.setSelected(selected);
            checkPanel.add(checkBox);
            return checkBox;
        }
        
        public JTextField buildField(String labelText, String initText) {
            FlowLayout f = new FlowLayout();
            f.setAlignment(FlowLayout.LEFT);
            JPanel panel = new JPanel(f);
            JLabel label = new JLabel(labelText);
            JTextField text = new JTextField(initText);
            text.setColumns(8);
            panel.add(label);
            panel.add(text);
            return text;
        }
        
        public JTextField buildIntegerField(final String labelText, int initNum) {
            final JTextField genericField = buildField(labelText, String.valueOf(initNum));
            ActionListener listener = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    if(e.getSource().equals(ok)) {
                        try {
                            Integer.valueOf(genericField.getText());
                        } catch(NumberFormatException e1) {
                            MipavUtil.displayInfo(labelText+" must be an integer.");
                            passedListeners = false;
                        }
                    }
                }
            };
            genericField.addActionListener(listener);
            listenerList.add(listener);
            return genericField;
        }
        
        public JTextField buildDecimalField(final String labelText, double initNum) {
            final JTextField genericField = buildField(labelText, String.valueOf(initNum));
            ActionListener listener = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    if(e.getSource().equals(ok)) {
                        try {
                            Double.valueOf(genericField.getText());
                        } catch(NumberFormatException e1) {
                            MipavUtil.displayInfo(labelText+" must be a number.");
                            passedListeners = false;
                        }
                    }
                }
            };
            genericField.addActionListener(listener);
            listenerList.add(listener);
            return genericField;
        }
        
        public JComboBox buildComboBox(String labelText, Object[] options) {
            FlowLayout f = new FlowLayout();
            f.setAlignment(FlowLayout.LEFT);
            JPanel panel = new JPanel(f);
            JLabel label = new JLabel(labelText);
            JComboBox comboBox = new JComboBox(options);
            panel.add(label);
            panel.add(comboBox);
            return comboBox;
        }
        
        public JComboBox buildComboBox(String labelText, Object[] options, int numDefault) {
            JComboBox comboBox = buildComboBox(labelText, options); //call default
            comboBox.setSelectedIndex(numDefault);
            return comboBox;
        }
        
        public JPanel buildOKCancelPanel() {
            JPanel panel = new JPanel();
            ok = new JButton("OK");
            cancel = new JButton("Cancel");
            cancel.addActionListener(this);
            panel.add(ok);
            ok.addActionListener(this);
            panel.add(cancel);
            return panel;
        }

        public void actionPerformed(ActionEvent e) {
            passedListeners = true;
            if(e.getSource().equals(ok)) {
                for(int i=0; i<listenerList.size(); i++) {
                    if(passedListeners) {
                        listenerList.get(i).actionPerformed(e);
                    } else {
                        exit = ExitStatus.OK_FAIL;
                        return;
                    }
                }
                if(passedListeners) {
                    exit = ExitStatus.OK_SUCCESS;
                    parent.dispose();
                } else {    
                    exit = ExitStatus.OK_FAIL;
                    return;
                }
            } else if(e.getSource().equals(cancel)) {
                exit = ExitStatus.CANCEL;
                parent.dispose();
            }
        }
        
    }

}
