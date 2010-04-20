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
import gov.nih.mipav.model.algorithms.AlgorithmCircleGeneration;
import gov.nih.mipav.model.algorithms.AlgorithmDespotT1;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.Parameter;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterDouble;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterFloat;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterList;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;

public class JDialogDespotT1 extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    private static String title = "DESPOT1 T1 Mapper";
    private double despotTR = 5.00;
    private double irspgrTR = 5.00;
    private double irspgrKy = 96.00;
    private double irspgrFA = 5.00;
    private double maxT1 = 5000;
    private double maxMo = 10000;
    private double[] despotFA;
    private double[] irspgrTr;
    private double[] irspgrTI;
    
    private double[] spgrData;
    private double[] irspgrData;
    private double scale, pointScale, scaleIncrement;
    private double[] estimates, residuals;
    private int[] direction;
    
    private int[] spgrImageIndex;
    private int[] irspgrImageIndex;
    private int b1ImageIndex;
    private double angleIncrement;
    private int Nsa = 2;
    private int Nti = 1;
    private double maxAngle = 20;
    
    private boolean smoothB1Field = true;
    /**The following GUI choices change algorithm operation in binary ways*/
    private boolean performStraightDESPOT1 = true;
    private boolean performDESPOT1withPreCalculatedB1Map = false;
    private boolean performDESPOT1HIFI = false;
    private boolean doubleInversion = true;
    private boolean singleInversion = false;
    private boolean geScanner = true;
    private boolean siemensScanner = false;
    private boolean threeTField = true;
    private boolean onefiveTField = false;
    
    /**The list of possible maps that can be calculated*/
    private boolean calculateT1 = true;
    private boolean showB1Map = false;
    private boolean calculateMo = false;
    private boolean invertT1toR1 = false;
    
    private boolean useWeights = true;
    
    private boolean uniformAngleSpacing = true;
    
    /**Whether, during "smart-thresholding", noise should be calculated from a given corner */
    private boolean upperLeftCorner = true;
    private boolean upperRightCorner = false;
    private boolean lowerLeftCorner = false;
    private boolean lowerRightCorner = false;
    
    private boolean useSmartThresholding = true;
    private boolean useHardThresholding = false;
    private float noiseScale = (float) 4.00;
    private float hardNoiseThreshold = (float) 0.00;
    
    /**The ordered list of images that the algorithm is dependent on. */
    private String[] wList;
    
    /**The GUI list of images */
    private String[] titles;

    private AlgorithmDespotT1 cAlgo;
    
    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogDespotT1() { }

    /**
     * Construct the barrel/pin cushion correction dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogDespotT1(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        run();
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof AlgorithmDespotT1) {
            Preferences.debug("DespotT1: " + algorithm.getElapsedTime());
        } 

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        
        if(!runningScriptFlag) {
            if (cAlgo != null) {
                cAlgo.finalize();
                cAlgo = null;
            }
    
            dispose();
        }
    }

    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("Cancel")) {
            cAlgo.interrupt();
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
        if (wList==null || wList.length<2) {
            MipavUtil.displayWarning("Need at least 2 SPGR images to use this algorithm.");
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
        
        if (!showOpeningDialog()) return;
        
        if (performDESPOT1HIFI == true) {
            if (!showHIFIDialog()) return;
        }
        else {
            if (!showConventionalDESPOT1Dialog()) return;
        }
    
        System.out.println("number of flip angles: "+Nsa);
        
        despotFA = new double[Nsa];
        if (Nsa == 2) {
            despotFA[0] = 4.00;
            despotFA[1] = 18.0;
        }
        spgrImageIndex = new int[Nsa];
        
        spgrData = new double[Nsa];
        
        if (performDESPOT1HIFI == true) {
            irspgrTI = new double[Nti];
            if (Nti == 1) irspgrTI[0] = 450.00;
            irspgrTr = new double[Nti];
            irspgrImageIndex = new int[Nti];
            irspgrData = new double[Nti];
            
            if (!showSPGRDialog()) return;
            
            irspgrTR = despotTR;
            
            if (geScanner == true) {
                if (!showIRSPGRDialogGE()) return;
            }
            else {
                if (!showIRSPGRDialogSiemens()) return;
            }
            
            if (!showDESPOT1HIFISpecificsDialog()) return;
            
        }
        else {
            if (!showDESPOT1LongDialog()) return; 
            if (!showDESPOT1SpecificsDialog()) return;
        }
        
        //note that smart/hard thresholding are not required methods
        if (useSmartThresholding) {
            if (!showSmartThresholdDialog()) return;
        }
        else {
            if (!showHardThresholdDialog()) return;
        }
          
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
        cAlgo = new AlgorithmDespotT1(despotTR, irspgrTR,
                irspgrKy, irspgrFA, maxT1, maxMo,
                despotFA,  irspgrTr,  irspgrTI,
                spgrData,  irspgrData, scale,
                pointScale, scaleIncrement,  estimates,
                residuals,  direction,  spgrImageIndex,
                irspgrImageIndex,  b1ImageIndex, angleIncrement,
                Nsa,  Nti, maxAngle,  smoothB1Field,
                performStraightDESPOT1,
                performDESPOT1withPreCalculatedB1Map,
                performDESPOT1HIFI,  doubleInversion,
                singleInversion,  geScanner,  siemensScanner,
                threeTField,  onefiveTField,  calculateT1,
                showB1Map,  calculateMo,  invertT1toR1,
                useWeights,  uniformAngleSpacing,
                upperLeftCorner,  upperRightCorner,
                lowerLeftCorner,  lowerRightCorner,
                useSmartThresholding,  useHardThresholding,
                noiseScale, hardNoiseThreshold, wList,
                titles);

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
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        
        if(cAlgo != null) {
            if(showB1Map && cAlgo.getB1ResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getB1ResultStack());
            }
            
            if(calculateMo && cAlgo.getMoResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getMoResultStack());
            }
            
            if(invertT1toR1 && cAlgo.getR1ResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getR1ResultStack());
            }
            
            if(calculateT1 && cAlgo.getT1ResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getT1ResultStack());
            }
        }
        
        //algorithm was not disposed of during algorithm performed since script is running
        if (cAlgo != null) {
            cAlgo.finalize();
            cAlgo = null;
        }

        dispose();
    }
    
    protected void setGUIFromParams() {
        despotTR = scriptParameters.getParams().getDouble("despot_TR");
        irspgrTR = scriptParameters.getParams().getDouble("irspgr_TR");
        irspgrKy = scriptParameters.getParams().getDouble("irspgr_Ky");
        irspgrFA = scriptParameters.getParams().getDouble("irspgr_FA");
        maxT1 = scriptParameters.getParams().getDouble("max_T1");
        maxMo = scriptParameters.getParams().getDouble("max_Mo");
        //double[], note all non-string arrays are stored as parameter lists
        if(scriptParameters.getParams().containsParameter("despot_FA")) {
            despotFA = scriptParameters.getParams().getList("despot_FA").getAsDoubleArray();
        }
        if(scriptParameters.getParams().containsParameter("irspgr_Tr")) {
            irspgrTr = scriptParameters.getParams().getList("irspgr_Tr").getAsDoubleArray();
        }
        if(scriptParameters.getParams().containsParameter("irspgr_TI")) {
            irspgrTI = scriptParameters.getParams().getList("irspgr_TI").getAsDoubleArray();
        }
        if(scriptParameters.getParams().containsParameter("spgr_Data")) {
            spgrData = scriptParameters.getParams().getList("spgr_Data").getAsDoubleArray();
        }
        if(scriptParameters.getParams().containsParameter("irspgr_Data")) {
            irspgrData = scriptParameters.getParams().getList("irspgr_Data").getAsDoubleArray();
        }
            
        scale = scriptParameters.getParams().getDouble("scale");
        pointScale = scriptParameters.getParams().getDouble("point_Scale");
        scaleIncrement = scriptParameters.getParams().getDouble("scale_Increment");
        //double[]
        if(scriptParameters.getParams().containsParameter("estimates")) {
            estimates = scriptParameters.getParams().getList("estimates").getAsDoubleArray();
        }
        if(scriptParameters.getParams().containsParameter("residuals")) {
            residuals = scriptParameters.getParams().getList("residuals").getAsDoubleArray();
        }
        //int[]
        if(scriptParameters.getParams().containsParameter("direction")) {
            direction = scriptParameters.getParams().getList("direction").getAsIntArray();
        }
        if(scriptParameters.getParams().containsParameter("spgr_Image_Index")) {
            spgrImageIndex = scriptParameters.getParams().getList("spgr_Image_Index").getAsIntArray();
        }
        if(scriptParameters.getParams().containsParameter("irspgr_Image_Index")) {
            irspgrImageIndex = scriptParameters.getParams().getList("irspgr_Image_Index").getAsIntArray();
        }
        
        b1ImageIndex = scriptParameters.getParams().getInt("b1_Image_Index");
        angleIncrement = scriptParameters.getParams().getDouble("angle_Increment");
        Nsa = scriptParameters.getParams().getInt("Nsa");
        Nti = scriptParameters.getParams().getInt("Nti");
        maxAngle = scriptParameters.getParams().getDouble("max_Angle");
        smoothB1Field = scriptParameters.getParams().getBoolean("smooth_B1_Field");
        performStraightDESPOT1 = scriptParameters.getParams().getBoolean("perform_Straight_DESPOT1");
        performDESPOT1withPreCalculatedB1Map = scriptParameters.getParams().getBoolean("perform_DESPOT1_with_PreCalculatedB1Map");
        performDESPOT1HIFI = scriptParameters.getParams().getBoolean("perform_DESPOT1_HIFI");
        doubleInversion = scriptParameters.getParams().getBoolean("double_Inversion");
        singleInversion = scriptParameters.getParams().getBoolean("single_Inversion");
        geScanner = scriptParameters.getParams().getBoolean("ge_Scanner");
        siemensScanner = scriptParameters.getParams().getBoolean("siemens_Scanner");
        threeTField = scriptParameters.getParams().getBoolean("three_T_Field");
        onefiveTField = scriptParameters.getParams().getBoolean("one_five_TField");
        calculateT1 = scriptParameters.getParams().getBoolean("calculate_T1");
        showB1Map = scriptParameters.getParams().getBoolean("show_B1_Map");
        calculateMo = scriptParameters.getParams().getBoolean("calculate_Mo");
        invertT1toR1 = scriptParameters.getParams().getBoolean("invert_T1_to_R1");
        useWeights = scriptParameters.getParams().getBoolean("use_Weights");
        uniformAngleSpacing = scriptParameters.getParams().getBoolean("uniform_Angle_Spacing");
        upperLeftCorner = scriptParameters.getParams().getBoolean("upper_Left_Corner");
        upperRightCorner = scriptParameters.getParams().getBoolean("upper_Right_Corner");
        lowerLeftCorner = scriptParameters.getParams().getBoolean("lower_Left_Corner");
        lowerRightCorner = scriptParameters.getParams().getBoolean("lower_Right_Corner");
        useSmartThresholding = scriptParameters.getParams().getBoolean("use_Smart_Thresholding");
        useHardThresholding = scriptParameters.getParams().getBoolean("use_Hard_Thresholding");
        noiseScale = scriptParameters.getParams().getFloat("noise_Scale");
        hardNoiseThreshold = scriptParameters.getParams().getFloat("hard_Noise_Threshold");
        
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
        scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_TR", irspgrTR));
        scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_Ky", irspgrKy)); 
        scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_FA", irspgrFA));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_T1", maxT1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_Mo", maxMo));
        //double[], note all non-string arrays are stored as parameter lists
        if(despotFA != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("despot_FA", despotFA));
        }
        if(irspgrTr != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_Tr", irspgrTr));
        }
        if(irspgrTI != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_TI", irspgrTI));
        }
        if(spgrData != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("spgr_Data", spgrData));
        }
        if(irspgrData != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_Data", irspgrData));
        }
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("scale", scale));
        scriptParameters.getParams().put(ParameterFactory.newParameter("point_Scale", pointScale));
        scriptParameters.getParams().put(ParameterFactory.newParameter("scale_Increment", scaleIncrement));
        //double[]
        if(estimates != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("estimates", estimates));
        }
        if(residuals != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("residuals", residuals));
        }
        //int[]
        if(direction != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("direction", direction));
        }
        if(spgrImageIndex != null) {   
            scriptParameters.getParams().put(ParameterFactory.newParameter("spgr_Image_Index", spgrImageIndex));
        }
        if(irspgrImageIndex != null) {   
            scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_Image_Index", irspgrImageIndex));
        }
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("b1_Image_Index", b1ImageIndex));
        scriptParameters.getParams().put(ParameterFactory.newParameter("angle_Increment", angleIncrement));
        scriptParameters.getParams().put(ParameterFactory.newParameter("Nsa", Nsa));
        scriptParameters.getParams().put(ParameterFactory.newParameter("Nti", Nti));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_Angle", maxAngle));
        scriptParameters.getParams().put(ParameterFactory.newParameter("smooth_B1_Field", smoothB1Field));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_Straight_DESPOT1", performStraightDESPOT1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_DESPOT1_with_PreCalculatedB1Map", performDESPOT1withPreCalculatedB1Map));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_DESPOT1_HIFI", performDESPOT1HIFI));
        scriptParameters.getParams().put(ParameterFactory.newParameter("double_Inversion", doubleInversion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("single_Inversion", singleInversion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("ge_Scanner", geScanner));  
        scriptParameters.getParams().put(ParameterFactory.newParameter("siemens_Scanner", siemensScanner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("three_T_Field", threeTField));
        scriptParameters.getParams().put(ParameterFactory.newParameter("one_five_TField", onefiveTField));
        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_T1", calculateT1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("show_B1_Map", showB1Map));
        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_Mo", calculateMo));
        scriptParameters.getParams().put(ParameterFactory.newParameter("invert_T1_to_R1", invertT1toR1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_Weights", useWeights));
        scriptParameters.getParams().put(ParameterFactory.newParameter("uniform_Angle_Spacing", uniformAngleSpacing));
        scriptParameters.getParams().put(ParameterFactory.newParameter("upper_Left_Corner", upperLeftCorner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("upper_Right_Corner", upperRightCorner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("lower_Left_Corner", lowerLeftCorner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("lower_Right_Corner", lowerRightCorner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_Smart_Thresholding", useSmartThresholding));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_Hard_Thresholding", useHardThresholding));
        scriptParameters.getParams().put(ParameterFactory.newParameter("noise_Scale", noiseScale));
        scriptParameters.getParams().put(ParameterFactory.newParameter("hard_Noise_Threshold", hardNoiseThreshold));
        //need a count of wList
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_input_images", wList.length));
        //String[], are in fact ModelImage identifiers need to be stored in order, titles is presumed to be identical for scripting
        if(wList != null) {
            for(int i=0; i<wList.length; i++) {
            	scriptParameters.storeImage(ViewUserInterface.getReference().getRegisteredImageByName(wList[i]), wList[i]);
            }
        }
        
        if(cAlgo != null) {
        	if(showB1Map && cAlgo.getB1ResultStack() != null) {
        		//scriptParameters.storeImageInRecorder(cAlgo.getB1ResultStack());
        		scriptParameters.storeOutputImageParams(cAlgo.getB1ResultStack(), true);
        	}
        	
        	if(calculateMo && cAlgo.getMoResultStack() != null) {
        		//scriptParameters.storeImageInRecorder(cAlgo.getMoResultStack());
        		scriptParameters.storeOutputImageParams(cAlgo.getMoResultStack(), true);
        	}
        	
        	if(invertT1toR1 && cAlgo.getR1ResultStack() != null) {
        		//scriptParameters.storeImageInRecorder(cAlgo.getR1ResultStack());
        		scriptParameters.storeOutputImageParams(cAlgo.getR1ResultStack(), true);
        	}
        	
        	if(calculateT1 && cAlgo.getT1ResultStack() != null) {
        		//scriptParameters.storeImageInRecorder(cAlgo.getT1ResultStack());
        		scriptParameters.storeOutputImageParams(cAlgo.getT1ResultStack(), true);
        	}
        }
    }

    public boolean showOpeningDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1: Opening Dialog");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JRadioButton button1 = guiHelp.buildRadioButton("Perform Conventional DESPOT1 Processing", performStraightDESPOT1);
        JRadioButton button2 = guiHelp.buildRadioButton("Perform DESPOT1 with Pre-calculated B1 Map", performDESPOT1withPreCalculatedB1Map);
        JRadioButton button3 = guiHelp.buildRadioButton("Perform DESPOT1-HIFI Processing", performDESPOT1HIFI);
        
        ButtonGroup processType = new ButtonGroup();
        processType.add(button1);
        processType.add(button2);
        processType.add(button3);
        
        panel.add(button1.getParent(), panelLayout);
        panel.add(button2.getParent(), panelLayout);
        panel.add(button3.getParent(), panelLayout);
        
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
        
        performStraightDESPOT1 = button1.isSelected();
        performDESPOT1withPreCalculatedB1Map = button2.isSelected();
        performDESPOT1HIFI = button3.isSelected();

        if(processType.getSelection() == null) {
            MipavUtil.displayInfo("Please select a processing method");
            dialog.dispose();
            return showOpeningDialog();
        }   
        
        if (performStraightDESPOT1 == true) {
            performDESPOT1withPreCalculatedB1Map = false;
            performDESPOT1HIFI = false;
        }
        if (performDESPOT1withPreCalculatedB1Map == true) {
            performStraightDESPOT1 = false;
            performDESPOT1HIFI = false;
        }
        if (performDESPOT1HIFI == true) {
            performStraightDESPOT1 = false;
            performDESPOT1withPreCalculatedB1Map = false;
        }
        
        return true;
    }
    
    public boolean showHIFIDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1-HIFI: General Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JTextField field1 = guiHelp.buildDecimalField("Number of SPGR Flip Angles:", Nsa);
        JTextField field2 = guiHelp.buildDecimalField("Number of IR-SPGR TI Times:", Nti);
        JRadioButton button1 = guiHelp.buildRadioButton("Scan Performed on a GE Scanner", geScanner);
        JRadioButton button2 = guiHelp.buildRadioButton("Scan Performed on a Siemens Scanner", siemensScanner);
        
        ButtonGroup scannerType = new ButtonGroup();
        scannerType.add(button1);
        scannerType.add(button2);
        
        panel.add(field1.getParent(), panelLayout);
        panel.add(field2.getParent(), panelLayout);
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
        
        Nsa = (int) Double.valueOf(field1.getText()).doubleValue();
        Nti = (int) Double.valueOf(field2.getText()).doubleValue();
    
        geScanner = button1.isSelected();
        siemensScanner = button2.isSelected();
        
        if(scannerType.getSelection() == null) {
            MipavUtil.displayInfo("Please select a scanner type");
            dialog.dispose();
            return showHIFIDialog();
        }
        
        if (geScanner == true) {
            siemensScanner = false;
        }
        if (siemensScanner == true) {
            geScanner = false;
        }
        
        if (Nsa > wList.length) {
            MipavUtil.displayError("Please import all nessesary images first.");
            return false;
        }
        if (Nsa < 2) {
            MipavUtil.displayError("T1 and Mo calculations require at least two SPGR images.");
            return false;
        }
        if (Nti < 1) {
            MipavUtil.displayError("B1 correction requires at least one IR-SPGR image.");
            return false;
        }
        
        return true;
    }
    
    public boolean showConventionalDESPOT1Dialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1: General Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JTextField field1 = guiHelp.buildDecimalField("Number of SPGR Flip Angles:", Nsa);
        
        panel.add(field1.getParent(), panelLayout);
        
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
        
        Nsa = (int) Double.valueOf(field1.getText()).doubleValue();
        
        if (Nsa > wList.length) {
            MipavUtil.displayError("Please import all nessesary images first.");
            return false;
        }
        if (Nsa < 2) {
            MipavUtil.displayError("T1 and Mo calculations require at least two SPGR images.");
            return false;
        }
        if (performDESPOT1withPreCalculatedB1Map) {
            if (Nsa+1 > wList.length) {
                MipavUtil.displayError("Please import all nessesary images first.");
                return false;
            }
        }
        
        return true;
    }
    
     public boolean showSPGRDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1-HIFI: SPGR Image Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JComboBox[] comboArr = new JComboBox[Nsa];
        JTextField[] fieldArr = new JTextField[Nsa];
        for (int i=0; i<Nsa; i++) {
            comboArr[i] = guiHelp.buildComboBox("SPGR Image #"+i, titles, i);
            panel.add(comboArr[i].getParent(), panelLayout);
            
            fieldArr[i] = guiHelp.buildDecimalField("SPGR Flip Angle #"+i, despotFA[i]);
            panel.add(fieldArr[i].getParent(), panelLayout);
        }
        JTextField fieldTime = guiHelp.buildDecimalField("SPGR Repetition Time (ms):", despotTR);
        panel.add(fieldTime.getParent(), panelLayout);
        
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
        
        for(int i=0; i<Nsa; i++) {
            spgrImageIndex[i] = comboArr[i].getSelectedIndex();
            despotFA[i] = Float.valueOf(fieldArr[i].getText()).floatValue();
        }
        despotTR = Double.valueOf(fieldTime.getText()).doubleValue();
         
        return true;
     }
    
    public boolean showIRSPGRDialogGE() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1-HIFI: IR-SPGR Image Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JComboBox[] comboArr = new JComboBox[Nti];
        JTextField[] fieldArr = new JTextField[Nti];
        for (int i=0; i<Nti; i++) {
            //TODO: See if addition of nsa is possible bug
            comboArr[i] = guiHelp.buildComboBox("IR-SPGR Image #"+i, titles, Nsa+i);
            panel.add(comboArr[i].getParent(), panelLayout);
            
            fieldArr[i] = guiHelp.buildDecimalField("IR-SPGR TI #"+i, irspgrTI[i]);
            panel.add(fieldArr[i].getParent(), panelLayout);
        }
        
        JTextField field1 = guiHelp.buildDecimalField("IR-SPGR Repetition Time (ms)", irspgrTR);
        JTextField field2 = guiHelp.buildDecimalField("IR-SPGR Flip Angle", irspgrFA);
        JTextField field3 = guiHelp.buildDecimalField("Total Number of Acquired Slices", irspgrKy);
        JRadioButton radio1 = guiHelp.buildRadioButton("Double Inversion Regime", doubleInversion);
        JRadioButton radio2 = guiHelp.buildRadioButton("Single Inversion Regime", singleInversion);
        JRadioButton radio3 = guiHelp.buildRadioButton("1.5T Field Strength", onefiveTField);
        JRadioButton radio4 = guiHelp.buildRadioButton("3.0T Field Strength", threeTField);
        JCheckBox box1 = guiHelp.buildCheckBox("Smooth B1 Field Prior to T1 Calculations", smoothB1Field);
        
        ButtonGroup inversion = new ButtonGroup();
        inversion.add(radio1);
        inversion.add(radio2);
        
        ButtonGroup fieldStrength = new ButtonGroup();
        fieldStrength.add(radio3);
        fieldStrength.add(radio4);
        
        panel.add(field1.getParent(), panelLayout);
        panel.add(field2.getParent(), panelLayout);
        panel.add(field3.getParent(), panelLayout);
        panel.add(radio1.getParent(), panelLayout);
        panel.add(radio2.getParent(), panelLayout);
        panel.add(radio3.getParent(), panelLayout);
        panel.add(radio4.getParent(), panelLayout);
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
        
        for (int i=0; i<Nti; i++) {
            irspgrImageIndex[i] = comboArr[i].getSelectedIndex();
            irspgrTI[i] = Double.valueOf(fieldArr[i].getText()).doubleValue();
        }
        irspgrTR = Double.valueOf(field1.getText()).doubleValue();
        irspgrFA = Double.valueOf(field2.getText()).doubleValue();
        irspgrKy = Double.valueOf(field3.getText()).doubleValue();
        doubleInversion = radio1.isSelected();
        singleInversion = radio2.isSelected();
        onefiveTField = radio3.isSelected();
        threeTField = radio4.isSelected();
        smoothB1Field = box1.isSelected();
        
        if(inversion.getSelection() == null) {
            MipavUtil.displayInfo("Please choose either single or double inversion regime");
            dialog.dispose();
            return showIRSPGRDialogGE();
        }
        
        if(fieldStrength.getSelection() == null) {
            MipavUtil.displayInfo("Please choose field strength");
            dialog.dispose();
            return showIRSPGRDialogGE();
        }
        
        if (doubleInversion == true) {
            singleInversion = false;
        }
        if (singleInversion == true) {
            doubleInversion = false;
        }
        
        if (onefiveTField == true) {
            threeTField = false;
        }
        if (threeTField == true) {
            onefiveTField = false;
        }
        
        if (threeTField == true) {
            if (doubleInversion == true) {
                for (int i=0; i<Nti; i++) irspgrTI[i] = irspgrTI[i]*0.9;
                irspgrKy = (irspgrKy/2.00)+2.00;
            }
            else {
                for (int i=0; i<Nti; i++) irspgrTI[i] = irspgrTI[i]*0.9*0.93;
                irspgrKy = irspgrKy + 2.00;
            }
        }
        else {
            if (doubleInversion == true) {
                for (int i=0; i<Nti; i++) irspgrTI[i] = irspgrTI[i];
                irspgrKy = (irspgrKy/2.00)+2.00;
            }
            else {
                for (int i=0; i<Nti; i++) irspgrTI[i] = irspgrTI[i];
                irspgrKy = irspgrKy;
            }
        }
        
        for (int i=0; i<Nti; i++) irspgrTr[i] = irspgrTI[i] + irspgrTR*irspgrKy;
        
        return true;
    }
    
    public boolean showIRSPGRDialogSiemens() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1-HIFI: IR-SPGR Image Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JComboBox[] comboArr = new JComboBox[Nti];
        JTextField[] fieldArr = new JTextField[Nti];
        for (int i=0; i<Nti; i++) {
            //TODO: find out why this is i+2
            comboArr[i] = guiHelp.buildComboBox("IR-SPGR Image #"+i, titles, i+2);
            panel.add(comboArr[i].getParent(), panelLayout);
            
            fieldArr[i] = guiHelp.buildDecimalField("IR-SPGR TI #"+i, irspgrTI[i]);
            panel.add(fieldArr[i].getParent(), panelLayout);
        }
        
        JTextField field1 = guiHelp.buildDecimalField("IR-SPGR Repetition Time (ms)", irspgrTR);
        JTextField field2 = guiHelp.buildDecimalField("IR-SPGR Flip Angle", irspgrFA);
        JTextField field3 = guiHelp.buildDecimalField("Total Number of Acquired Slices", irspgrKy);
        JRadioButton radio1 = guiHelp.buildRadioButton("Double Inversion Regime", doubleInversion);
        JRadioButton radio2 = guiHelp.buildRadioButton("Single Inversion Regime", singleInversion);
        JRadioButton radio3 = guiHelp.buildRadioButton("1.5T Field Strength", onefiveTField);
        JRadioButton radio4 = guiHelp.buildRadioButton("3.0T Field Strength", threeTField);
        JCheckBox box1 = guiHelp.buildCheckBox("Smooth B1 Field Prior to T1 Calculations", smoothB1Field);
        
        ButtonGroup inversion = new ButtonGroup();
        inversion.add(radio1);
        inversion.add(radio2);
        
        ButtonGroup fieldStrength = new ButtonGroup();
        fieldStrength.add(radio3);
        fieldStrength.add(radio4);
        
        panel.add(field1.getParent(), panelLayout);
        panel.add(field2.getParent(), panelLayout);
        panel.add(field3.getParent(), panelLayout);
        panel.add(radio1.getParent(), panelLayout);
        panel.add(radio2.getParent(), panelLayout);
        panel.add(radio3.getParent(), panelLayout);
        panel.add(radio4.getParent(), panelLayout);
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
        
        for (int i=0; i<Nti; i++) {
            irspgrImageIndex[i] = comboArr[i].getSelectedIndex();
            irspgrTI[i] = Double.valueOf(fieldArr[i].getText()).doubleValue();
        }
        irspgrTR = Double.valueOf(field1.getText()).doubleValue();
        irspgrFA = Double.valueOf(field2.getText()).doubleValue();
        irspgrKy = Double.valueOf(field3.getText()).doubleValue();
        doubleInversion = radio1.isSelected();
        singleInversion = radio2.isSelected();
        onefiveTField = radio3.isSelected();
        threeTField = radio4.isSelected();
        smoothB1Field = box1.isSelected();
        
        if(inversion.getSelection() == null) {
            MipavUtil.displayInfo("Please choose either single or double inversion regime");
            dialog.dispose();
            return showIRSPGRDialogSiemens();
        }
        if(fieldStrength.getSelection() == null) {
            MipavUtil.displayInfo("Please select a field strength");
            dialog.dispose();
            return showIRSPGRDialogSiemens();
        }
        
        if (doubleInversion == true) {
            singleInversion = false;
        }
        if (singleInversion == true) {
            doubleInversion = false;
        }
        if (onefiveTField == true) {
            threeTField = false;
        }
        if (threeTField == true) {
            onefiveTField = false;
        }
        
        if (threeTField == true) {
            if (doubleInversion == true) {
                for (int i=0; i<Nti; i++) irspgrTI[i] = irspgrTI[i]*0.9;
                irspgrKy = (irspgrKy/2.00)+2.00;
            }
            else {
                for (int i=0; i<Nti; i++) irspgrTI[i] = irspgrTI[i]*0.9*0.93;
                irspgrKy = irspgrKy + 2.00;
            }
        }
        else {
            if (doubleInversion == true) {
                for (int i=0; i<Nti; i++) irspgrTI[i] = irspgrTI[i];
                irspgrKy = (irspgrKy/2.00)+2.00;
            }
            else {
                for (int i=0; i<Nti; i++) irspgrTI[i] = irspgrTI[i];
                irspgrKy = irspgrKy;
            }
        }
        
        for (int i=0; i<Nti; i++) irspgrTr[i] = irspgrTI[i] + irspgrTR*irspgrKy;
        
        return true;
    }
        
        

    public boolean showDESPOT1LongDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle(title);
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JComboBox combo1 = null;
        if(performDESPOT1withPreCalculatedB1Map) {
            combo1 = guiHelp.buildComboBox("B1 Field Map:", titles, 0);
            panel.add(combo1.getParent(), panelLayout);
        }
        
        JComboBox[] comboAr = new JComboBox[Nsa];
        JTextField[] fieldAr = new JTextField[Nsa];
        for(int i=0; i<Nsa; i++) {
            comboAr[i] = guiHelp.buildComboBox("Image #"+i, titles, i);
            panel.add(comboAr[i].getParent(), panelLayout);
            
            fieldAr[i] = guiHelp.buildDecimalField("Flip Angle #"+i, despotFA[i]);
            panel.add(fieldAr[i].getParent(), panelLayout);
        }
        JTextField field1 = guiHelp.buildDecimalField("Repetition Time (ms):", despotTR);
        panel.add(field1.getParent(), panelLayout);
        
        JCheckBox check1 = null;
        if(Nsa > 2) {
            check1 = guiHelp.buildCheckBox("Calculate T1 Using Weigthed Least-Squares", useWeights);
            panel.add(check1.getParent(), panelLayout);
        }
        
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
    
        if (performDESPOT1withPreCalculatedB1Map) {
            b1ImageIndex = combo1.getSelectedIndex();
        }
        
        for (int i=0; i<Nsa; i++) {
            spgrImageIndex[i] = comboAr[i].getSelectedIndex();
            despotFA[i] = Float.valueOf(fieldAr[i].getText()).floatValue();
        }
        
        despotTR = Double.valueOf(field1.getText()).doubleValue();
        if (Nsa > 2) {
            useWeights = check1.isSelected();
        }
        
        return true;
    }
        
    public boolean showDESPOT1SpecificsDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1-HIFI: IR-SPGR Image Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JTextField field1 = guiHelp.buildDecimalField("Maximum Allowable T1:", maxT1);
        JTextField field2 = guiHelp.buildDecimalField("Maximum Allowable Mo:", maxMo);
        JCheckBox check1 = guiHelp.buildCheckBox("Show T1 Map", calculateT1);
        JCheckBox check2 = guiHelp.buildCheckBox("Show Mo Map", calculateMo);
        JCheckBox check3 = guiHelp.buildCheckBox("Show R1 Map", invertT1toR1);
        JCheckBox check4 = guiHelp.buildCheckBox("Use Smart Thresholding?", useSmartThresholding);
        JCheckBox check5 = guiHelp.buildCheckBox("Use Hard Thresholding?", useHardThresholding);
        
        panel.add(field1.getParent(), panelLayout);
        panel.add(field2.getParent(), panelLayout);
        panel.add(check1.getParent(), panelLayout);
        panel.add(check2.getParent(), panelLayout);
        panel.add(check3.getParent(), panelLayout);
        panel.add(check4.getParent(), panelLayout);
        panel.add(check5.getParent(), panelLayout);
        
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
        
        maxT1 = Double.valueOf(field1.getText()).doubleValue();
        maxMo = Double.valueOf(field2.getText()).doubleValue();
        calculateT1 = check1.isSelected();
        calculateMo = check2.isSelected();
        invertT1toR1 = check3.isSelected();
        useSmartThresholding = check4.isSelected();
        useHardThresholding = check5.isSelected();
        if (useSmartThresholding) {
            useHardThresholding = false;
        }
        return true;
    }
    
   
    public boolean showDESPOT1HIFISpecificsDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1-HIFI: IR-SPGR Image Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JTextField field1 = guiHelp.buildDecimalField("Maximum Allowable T1:", maxT1);
        JTextField field2 = guiHelp.buildDecimalField("Maximum Allowable Mo:", maxMo);
        JCheckBox check1 = guiHelp.buildCheckBox("Show T1 Map", calculateT1);
        JCheckBox check2 = guiHelp.buildCheckBox("Show Mo Map", calculateMo);
        JCheckBox check3 = guiHelp.buildCheckBox("Show B1 Map", true);
        JCheckBox check4 = guiHelp.buildCheckBox("Show R1 Map", invertT1toR1);
        JCheckBox checkOpt = null; 
        if (Nsa > 2) { 
            checkOpt = guiHelp.buildCheckBox("Calculate T1 Using Weighted Least-Squares", useWeights);
        }
        JCheckBox check6 = guiHelp.buildCheckBox("Use Smart Thresholding?", useSmartThresholding);
        JCheckBox check7 = guiHelp.buildCheckBox("Use Hard Thresholding?", useHardThresholding);
        
        panel.add(field1.getParent(), panelLayout);
        panel.add(field2.getParent(), panelLayout);
        panel.add(check1.getParent(), panelLayout);
        panel.add(check2.getParent(), panelLayout);
        panel.add(check3.getParent(), panelLayout);
        panel.add(check4.getParent(), panelLayout);
        if(Nsa > 2) {
            panel.add(checkOpt.getParent(), panelLayout);
        }
        panel.add(check6.getParent(), panelLayout);
        panel.add(check7.getParent(), panelLayout);
        
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
        
        maxT1 = Double.valueOf(field1.getText()).doubleValue();
        maxMo = Double.valueOf(field2.getText()).doubleValue();
        calculateT1 = check1.isSelected();
        calculateMo = check2.isSelected();
        showB1Map = check3.isSelected();
        invertT1toR1 = check4.isSelected();
        if (Nsa > 2) {
            useWeights = checkOpt.isSelected();
        }
        useSmartThresholding = check6.isSelected();
        useHardThresholding = check7.isSelected();
        if (useSmartThresholding) {
            useHardThresholding = false;
        }
        
        return true;
    }
    
    public boolean showHardThresholdDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1 T1: Threshold Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JTextField field1 = guiHelp.buildDecimalField("Hard Noise Level", hardNoiseThreshold);
        
        panel.add(field1.getParent(), panelLayout);
        
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
        
        hardNoiseThreshold = Float.valueOf(field1.getText()).floatValue();
        
        return true;
    }
    
    public boolean showSmartThresholdDialog() {
        BorderLayout b = new BorderLayout();
        JDialog dialog = new JDialog();
        dialog.setLayout(b);
        GuiBuilder guiHelp = new GuiBuilder(dialog);
        dialog.setTitle("DESPOT1 T1: Threshold Information");
        JPanel panel = new JPanel();
        LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(panelLayout);
        
        JTextField field1 = guiHelp.buildDecimalField("Noise Level Scale", noiseScale);
        JCheckBox box1 = guiHelp.buildCheckBox("Calculate Noise from TOP LEFT Corner?", upperLeftCorner);
        JCheckBox box2 = guiHelp.buildCheckBox("Calculate Noise from TOP RIGHT Corner?", upperRightCorner);
        JCheckBox box3 = guiHelp.buildCheckBox("Calculate Noise from BOTTOM LEFT Corner?", lowerLeftCorner);
        JCheckBox box4 = guiHelp.buildCheckBox("Calculate Noise from BOTTOM RIGHT Corner?", lowerRightCorner);
        
        panel.add(field1.getParent(), panelLayout);
        panel.add(box1.getParent(), panelLayout);
        panel.add(box2.getParent(), panelLayout);
        panel.add(box3.getParent(), panelLayout);
        panel.add(box4.getParent(), panelLayout);
        
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
        
        noiseScale = Float.valueOf(field1.getText()).floatValue();
        upperLeftCorner = box1.isSelected();
        upperRightCorner = box2.isSelected();
        lowerLeftCorner = box3.isSelected();
        lowerRightCorner = box4.isSelected();
        return true;
    }
    
   
    public String itos(int num) {
        String str = new Integer(num).toString();
        return str;
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

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterDouble("despot_TR", despotTR));
            table.put(new ParameterDouble("irspgr_TR", irspgrTR));
            table.put(new ParameterDouble("irspgr_Ky", irspgrKy));
            table.put(new ParameterDouble("irspgr_FA", irspgrFA));
            table.put(new ParameterDouble("max_T1", maxT1));
            table.put(new ParameterDouble("max_Mo", maxMo));
            
            //double[], note all non-string arrays are stored as parameter lists
            table.put(new ParameterList("despot_FA", Parameter.PARAM_DOUBLE, "0,0,0"));
            table.put(new ParameterList("irspgr_Tr", Parameter.PARAM_DOUBLE, "0,0,0"));
            table.put(new ParameterList("irspgr_TI", Parameter.PARAM_DOUBLE, "0,0,0")); 
            table.put(new ParameterList("spgr_Data", Parameter.PARAM_DOUBLE, "0,0,0"));
            table.put(new ParameterList("irspgr_Data", Parameter.PARAM_DOUBLE, "0,0,0"));
            
            table.put(new ParameterDouble("scale", scale));
            table.put(new ParameterDouble("point_Scale", pointScale));
            table.put(new ParameterDouble("scale_Increment", scaleIncrement));

            //double[]
            table.put(new ParameterList("estimates", Parameter.PARAM_DOUBLE, "0,0,0"));
            table.put(new ParameterList("residuals", Parameter.PARAM_DOUBLE, "0,0,0"));

            //int[]
            table.put(new ParameterList("direction", Parameter.PARAM_INT, "0,0,0"));
            table.put(new ParameterList("spgr_Image_Index", Parameter.PARAM_INT, "0,0,0"));
            table.put(new ParameterList("irspgr_Image_Index", Parameter.PARAM_INT, "0,0,0"));
            
            table.put(new ParameterInt("b1_Image_Index", b1ImageIndex));
            
            table.put(new ParameterDouble("angle_Increment", angleIncrement));

            table.put(new ParameterInt("Nsa", Nsa));
            table.put(new ParameterInt("Nti", Nti));
            
            table.put(new ParameterDouble("max_Angle", maxAngle));
            
            table.put(new ParameterBoolean("smooth_B1_Field", smoothB1Field));
            table.put(new ParameterBoolean("perform_Straight_DESPOT1", performStraightDESPOT1));
            table.put(new ParameterBoolean("perform_DESPOT1_with_PreCalculatedB1Map", performDESPOT1withPreCalculatedB1Map));
            table.put(new ParameterBoolean("perform_DESPOT1_HIFI", performDESPOT1HIFI));
            table.put(new ParameterBoolean("double_Inversion", doubleInversion));
            table.put(new ParameterBoolean("single_Inversion", singleInversion));
            table.put(new ParameterBoolean("ge_Scanner", geScanner));
            table.put(new ParameterBoolean("siemens_Scanner", siemensScanner));
            table.put(new ParameterBoolean("three_T_Field", threeTField));
            table.put(new ParameterBoolean("one_five_TField", onefiveTField));
            table.put(new ParameterBoolean("calculate_T1", calculateT1));
            table.put(new ParameterBoolean("show_B1_Map", showB1Map));
            table.put(new ParameterBoolean("calculate_Mo", calculateMo));
            table.put(new ParameterBoolean("invert_T1_to_R1", invertT1toR1));
            table.put(new ParameterBoolean("use_Weights", useWeights));
            table.put(new ParameterBoolean("uniform_Angle_Spacing", uniformAngleSpacing));
            table.put(new ParameterBoolean("upper_Left_Corner", upperLeftCorner));
            table.put(new ParameterBoolean("upper_Right_Corner", upperRightCorner));
            table.put(new ParameterBoolean("lower_Left_Corner", lowerLeftCorner));
            table.put(new ParameterBoolean("lower_Right_Corner", lowerRightCorner));
            table.put(new ParameterBoolean("use_Smart_Thresholding", useSmartThresholding));
            table.put(new ParameterBoolean("use_Hard_Thresholding", useHardThresholding));
            
            table.put(new ParameterFloat("noise_Scale", noiseScale));
            table.put(new ParameterFloat("hard_Noise_Threshold", hardNoiseThreshold));
            
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            
            //need a count of wlist
            table.put(new ParameterInt("number_of_input_images", 1));
            
            Enumeration<String> imgNames = ViewUserInterface.getReference().getRegisteredImageNames();
            while(imgNames.hasMoreElements()) {
                table.put(new ParameterExternalImage(imgNames.nextElement()));
            }

            
            
            /*AlgorithmParameters.
            
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
            
            
            
            
            
            
            
            
            
            
          //need a count of wList
            scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_input_images", wList.length));
            //String[], are in fact ModelImage identifiers need to be stored in order, titles is presumed to be identical for scripting
            if(wList != null) {
                for(int i=0; i<wList.length; i++) {
                    scriptParameters.storeImage(ViewUserInterface.getReference().getRegisteredImageByName(wList[i]), wList[i]);
                }
            }
            
            if(cAlgo != null) {
                if(showB1Map && cAlgo.getB1ResultStack() != null) {
                    //scriptParameters.storeImageInRecorder(cAlgo.getB1ResultStack());
                    scriptParameters.storeOutputImageParams(cAlgo.getB1ResultStack(), true);
                }
                
                if(calculateMo && cAlgo.getMoResultStack() != null) {
                    //scriptParameters.storeImageInRecorder(cAlgo.getMoResultStack());
                    scriptParameters.storeOutputImageParams(cAlgo.getMoResultStack(), true);
                }
                
                if(invertT1toR1 && cAlgo.getR1ResultStack() != null) {
                    //scriptParameters.storeImageInRecorder(cAlgo.getR1ResultStack());
                    scriptParameters.storeOutputImageParams(cAlgo.getR1ResultStack(), true);
                }
                
                if(calculateT1 && cAlgo.getT1ResultStack() != null) {
                    //scriptParameters.storeImageInRecorder(cAlgo.getT1ResultStack());
                    scriptParameters.storeOutputImageParams(cAlgo.getT1ResultStack(), true);
                }
            }
            

            
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_SEPARABLE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterList(AlgorithmParameters.SIGMAS, Parameter.PARAM_FLOAT, "1.0,1.0,1.0"));
            table.put(new ParameterBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION, true));
            table.put(new ParameterList(AlgorithmParameters.DO_PROCESS_RGB, Parameter.PARAM_BOOLEAN, "true,true,true"));*/
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
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage("t1_results"));
            table.put(new ParameterImage("r1_results"));
            table.put(new ParameterImage("b1_results"));
            table.put(new ParameterImage("mo_results"));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                System.out.println("Here3");
                return new String("Algorithms.MRI");
            }

            public String getDescription() {
                return new String("Estimates T1 using two flip angles.");
            }

            public String getDescriptionLong() {
                return new String("Estimates T1 relaxation times using two flip angles with constant Tr.");
            }

            public String getShortLabel() {
                return new String("FlipCalcT1");
            }

            public String getLabel() {
                return new String("FlipCalcT1");
            }

            public String getName() {
                return new String("FlipCalcT1");
            }
        };
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(String imageParamName) {
        

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }

}
