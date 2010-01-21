package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
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
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;

public class JDialogDespotT1 extends JDialogScriptableBase implements AlgorithmInterface {

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
    private boolean performStraightDESPOT1 = true;
    private boolean performDESPOT1withPreCalculatedB1Map = false;
    private boolean performDESPOT1HIFI = false;
    private boolean doubleInversion = true;
    private boolean singleInversion = false;
    private boolean geScanner = true;
    private boolean siemensScanner = false;
    private boolean threeTField = true;
    private boolean onefiveTField = false;
    
    private boolean calculateT1 = true;
    private boolean showB1Map = true;
    private boolean calculateMo = false;
    private boolean invertT1toR1 = false;
    
    private boolean useWeights = true;
    
    private boolean uniformAngleSpacing = true;
    
    private boolean upperLeftCorner = true;
    private boolean upperRightCorner = false;
    private boolean lowerLeftCorner = false;
    private boolean lowerRightCorner = false;
    
    private boolean useSmartThresholding = true;
    private boolean useHardThresholding = false;
    private float noiseScale = (float) 4.00;
    private float hardNoiseThreshold = (float) 0.00;
    
    private String[] wList;
    private String[] titles;

    private AlgorithmDespotT1 cAlgo;
    
    public JDialogDespotT1() {
        System.out.println("Here");
        run();
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof AlgorithmDespotT1) {
            Preferences.debug("DespotT1: " + algorithm.getElapsedTime());
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
    
    protected void setGUIFromParams() {
        this.despotTR = despotTR;
        this.irspgrTR = irspgrTR;
        this.irspgrKy = irspgrKy;
        this.irspgrFA = irspgrFA;
        this.maxT1 = maxT1;
        this.maxMo = maxMo;
        this.despotFA = despotFA;
        irspgrTr = irspgrTr;
        this.irspgrTI = irspgrTI;
        this.spgrData = spgrData;
        this.irspgrData = irspgrData;
        this.scale = scale;
        this.pointScale = pointScale;
        this.scaleIncrement = scaleIncrement;
        this.estimates = estimates;
        this.residuals = residuals;
        this.direction = direction;
        this.spgrImageIndex = spgrImageIndex;
        this.irspgrImageIndex = irspgrImageIndex;
        this.b1ImageIndex = b1ImageIndex;
        this.angleIncrement = angleIncrement;
        Nsa = Nsa;
        Nti = Nti;
        this.maxAngle = maxAngle;
        this.smoothB1Field = smoothB1Field;
        this.performStraightDESPOT1 = performStraightDESPOT1;
        this.performDESPOT1withPreCalculatedB1Map = performDESPOT1withPreCalculatedB1Map;
        this.performDESPOT1HIFI = performDESPOT1HIFI;
        this.doubleInversion = doubleInversion;
        this.singleInversion = singleInversion;
        this.geScanner = geScanner;
        this.siemensScanner = siemensScanner;
        this.threeTField = threeTField;
        this.onefiveTField = onefiveTField;
        this.calculateT1 = calculateT1;
        this.showB1Map = showB1Map;
        this.calculateMo = calculateMo;
        this.invertT1toR1 = invertT1toR1;
        this.useWeights = useWeights;
        this.uniformAngleSpacing = uniformAngleSpacing;
        this.upperLeftCorner = upperLeftCorner;
        this.upperRightCorner = upperRightCorner;
        this.lowerLeftCorner = lowerLeftCorner;
        this.lowerRightCorner = lowerRightCorner;
        this.useSmartThresholding = useSmartThresholding;
        this.useHardThresholding = useHardThresholding;
        this.noiseScale = noiseScale;
        this.hardNoiseThreshold = hardNoiseThreshold;
        this.wList = wList;
        this.titles = titles;
        
    }

    protected void storeParamsFromGUI() throws ParserException {
        this.despotTR = despotTR;
        this.irspgrTR = irspgrTR;
        this.irspgrKy = irspgrKy;
        this.irspgrFA = irspgrFA;
        this.maxT1 = maxT1;
        this.maxMo = maxMo;
        this.despotFA = despotFA;
        irspgrTr = irspgrTr;
        this.irspgrTI = irspgrTI;
        this.spgrData = spgrData;
        this.irspgrData = irspgrData;
        this.scale = scale;
        this.pointScale = pointScale;
        this.scaleIncrement = scaleIncrement;
        this.estimates = estimates;
        this.residuals = residuals;
        this.direction = direction;
        this.spgrImageIndex = spgrImageIndex;
        this.irspgrImageIndex = irspgrImageIndex;
        this.b1ImageIndex = b1ImageIndex;
        this.angleIncrement = angleIncrement;
        Nsa = Nsa;
        Nti = Nti;
        this.maxAngle = maxAngle;
        this.smoothB1Field = smoothB1Field;
        this.performStraightDESPOT1 = performStraightDESPOT1;
        this.performDESPOT1withPreCalculatedB1Map = performDESPOT1withPreCalculatedB1Map;
        this.performDESPOT1HIFI = performDESPOT1HIFI;
        this.doubleInversion = doubleInversion;
        this.singleInversion = singleInversion;
        this.geScanner = geScanner;
        this.siemensScanner = siemensScanner;
        this.threeTField = threeTField;
        this.onefiveTField = onefiveTField;
        this.calculateT1 = calculateT1;
        this.showB1Map = showB1Map;
        this.calculateMo = calculateMo;
        this.invertT1toR1 = invertT1toR1;
        this.useWeights = useWeights;
        this.uniformAngleSpacing = uniformAngleSpacing;
        this.upperLeftCorner = upperLeftCorner;
        this.upperRightCorner = upperRightCorner;
        this.lowerLeftCorner = lowerLeftCorner;
        this.lowerRightCorner = lowerRightCorner;
        this.useSmartThresholding = useSmartThresholding;
        this.useHardThresholding = useHardThresholding;
        this.noiseScale = noiseScale;
        this.hardNoiseThreshold = hardNoiseThreshold;
        this.wList = wList;
        this.titles = titles;
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
        JCheckBox check3 = guiHelp.buildCheckBox("Show B1 Map", showB1Map);
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

}
