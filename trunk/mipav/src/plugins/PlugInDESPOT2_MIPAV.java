
import java.awt.BorderLayout;

import java.awt.FlowLayout;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
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

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.BundledPlugInInfo;
import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;

/** This plugin calculates the T2 relaxation time from any number of SSFP images acquired with phase-cycling and a known T1 and B1 map. **/
/* written: March 17, 2007 : S. Deoni
	status : development

	22/03/2007	Added ability to fully model the SSFP signal using a downhill simplex method
	5/07/2007	Refined search method to improve robustness to starting conditions
	7/07/2007	Refined search method into two parts: first an initial search for Mo, T2 and Bo.
				These maps are then smoothed and a two-parameter fit calculated for just Mo and T2.

*/

public class PlugInDESPOT2_MIPAV implements PlugInGeneric, BundledPlugInInfo {
	
    public static final String[] CATEGORY = {"DESPOT"};
    
    static String title = "DESPOT2 T2 Mapper";
    static double despotTR = 5.00;
	static double maxT2 = 1000;
	static double maxMo = 10000;
    double[] despotFA_phase0;
	double[] despotFA_phase180;
	
    int[] ssfpImageIndex_phase0;
	int[] ssfpImageIndex_phase180;
	int t1ImageIndex;
	int b1ImageIndex;
	
	double[] simplexLineValues, simplexResiduals, simplexCentre, reflection, expansion, contraction, shrink;
	double[][] simplex;
	double[] twoPSimplexLineValues, twoPSimplexResiduals, twoPSimplexCentre, twoPReflection, twoPExpansion, twoPContraction, twoPShrink;
	double[][] twoPSimplex;
	int[] bestToWorst;
	
    static int Nfa_phase0 = 0;
	static int Nfa_phase180 = 2;
	
    static boolean calculateT2 = true;
	static boolean includeB1Map = false;
	static boolean performConventionalModelling = true;
	static boolean performApproxModelling = false;
	static boolean performFullModelling = false;
    static boolean calculateMo = true;
	static boolean invertT2toR2 = false;
	static boolean calculateBo = false;
	
	static boolean performConventionalWith180Phase = true;
	static boolean performConventionalWith0Phase = false;
	
	static boolean geScanner = true;
	static boolean siemensScanner = false;
	
	static boolean upperLeftCorner = true;
	static boolean upperRightCorner = false;
	static boolean lowerLeftCorner = false;
	static boolean lowerRightCorner = false;
	
	static boolean useSmartThresholding = true;
	static boolean useHardThresholding = false;
	static float noiseScale = (float) 1.00;
	static float hardNoiseThreshold = (float) 0.00;
	
    String[] wList;
    private String[] titles;

    private long start;

    private ViewJProgressBar progressBar;
	
    public String[] getCategory() {
        return CATEGORY;
    }

    public String getName() {
        return "Despot2";
    }

    public void run() {
    	
    	Enumeration<String> imageEnum = ViewUserInterface.getReference().getRegisteredImageNames();
        ArrayList<String> imageList = new ArrayList<String>();
        while(imageEnum.hasMoreElements()) {
        	imageList.add(imageEnum.nextElement());
        }
        wList = new String[imageList.size()];
        wList = imageList.toArray(wList);
        if (wList==null || wList.length<3) {
            MipavUtil.displayWarning("Need at least 2 SSFP images and a pre-computed T1 map to use this Plugin.");
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
        
        start = System.currentTimeMillis();
        
        final Thread runLocal = new DespotPerform();
        
        progressBar = new ViewJProgressBar("Despot1", "   ", 0, 100, true);
        progressBar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(e.getActionCommand().equalsIgnoreCase("cancel")) {
                    progressBar.dispose();
                    runLocal.interrupt();
                }
            }
        });
        
        runLocal.start();
        
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
		
		while(dialog.isVisible()) {}
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
	
	public String itos(int num) {
    	String str = new Integer(num).toString();
	return str;
    }
	
	public void twoPDownHillSimplex(double[] optimization, double[] initialGuess, double Bo, double t1, double tr, double[] ssfpSampleData, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
		
		double RHO, CHI, PSI, SIGMA, maxError, usual_delta, zero_term_delta, residual;
		int best, worst, secondWorst;
		
		
		int NMAX, numParams, numVertices, iterations;
		
		double t2, mo;
		double rtol; 
		
		int i, j;
		
		// define algorithm-specific variables
		RHO = 1.00;
		CHI = 2.00;
		PSI = 0.50;
		SIGMA = 0.50;
		usual_delta = 0.05;
		zero_term_delta = .00025;
		
		maxError = 0.001;
		NMAX = 15000;
		
		numParams = 2;
		numVertices = numParams+1;
		
		// initialize the twoPSimplex
		twoPSimplexLineValues[0] = initialGuess[0]; 
		twoPSimplexLineValues[1] = initialGuess[1];
		residual = calculateTwoPResiduals(twoPSimplexLineValues, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
		
		for (i=0; i<numParams; i++) twoPSimplex[0][i] = twoPSimplexLineValues[i];
		twoPSimplex[0][numParams] = residual;
		
		for (i=0; i<numParams; i++) {
			twoPSimplexLineValues[0] = initialGuess[0];
			twoPSimplexLineValues[1] = initialGuess[1];
			
			if (twoPSimplexLineValues[i] != 0.00) twoPSimplexLineValues[i] = (1.00+usual_delta)*twoPSimplexLineValues[i];
			else twoPSimplexLineValues[i] = zero_term_delta;
			residual = calculateTwoPResiduals(twoPSimplexLineValues, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
			
			twoPSimplex[i+1][0] = twoPSimplexLineValues[0];
			twoPSimplex[i+1][1] = twoPSimplexLineValues[1];
			twoPSimplex[i+1][2] = residual;
		}
		
		// now calculate the best, worst and second worst vertices of the twoPSimplex
		for (i=0; i<numVertices; i++) twoPSimplexResiduals[i] = twoPSimplex[i][numParams];
		calculateBestToWorst(twoPSimplexResiduals, bestToWorst, numVertices);
		best = bestToWorst[0];
		worst = bestToWorst[2];
		secondWorst = bestToWorst[1];
		
		// define a fractional range from the best to worst twoPSimplex vertex
		rtol = twoPSimplex[best][numParams] - twoPSimplex[worst][numParams];
		if (rtol < 0.00) rtol = -1.00*rtol;
		
		// now, get on with the main algorithm
		iterations = 0;
		while (rtol > maxError && iterations < NMAX) {
			iterations++;
			
			// calculate the centre of the twoPSimplex, ignoring the worst point
			for (i=0; i<numParams; i++) twoPSimplexCentre[i] = 0.00;
			for (i=0; i<numVertices; i++) {
				if (i != bestToWorst[numParams-1]) {
					for (j=0; j<numParams; j++) twoPSimplexCentre[j] += twoPSimplex[i][j];
				}
			}
			for (i=0; i<numParams; i++) twoPSimplexCentre[i] = twoPSimplexCentre[i]/numParams;
			
			// reflect the twoPSimplex through the face of the high point
			//for (i=0; i<numVertices; i++) twoPReflection[i] = 0.00;
			for (i=0; i<numParams; i++) twoPReflection[i] = (1.00+RHO)*twoPSimplexCentre[i] - RHO*twoPSimplex[worst][i];
			twoPReflection[numParams] = calculateTwoPResiduals(twoPReflection, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
			
			
			// compare the twoPReflection vertex with the best vertex in the exisiting twoPSimplex
			if (twoPReflection[numParams] < twoPSimplex[best][numParams]) {
				// if the twoPReflection was better, try an twoPExpansion in this direction and see how that is
				//for (i=0; i<numVertices; i++) twoPExpansion[i] = 0.00;
				for (i=0; i<numParams; i++) twoPExpansion[i] = (1.00+RHO*CHI)*twoPSimplexCentre[i] - RHO*CHI*twoPSimplex[worst][i];
				twoPExpansion[numParams] = calculateTwoPResiduals(twoPExpansion, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
				
				if (twoPExpansion[numParams] < twoPReflection[numParams]) {
					for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPExpansion[i];
				}
				else {
					for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPReflection[i];
				}
			}
			else {
				if (twoPReflection[numParams] < twoPSimplex[secondWorst][numParams]) {
					for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPReflection[i];
				}
				else {
					if (twoPReflection[numParams] < twoPSimplex[worst][numParams]) {
						// perform an outside twoPContraction
						//for (i=0; i<numVertices; i++) twoPContraction[i] = 0.00;
						for (i=0; i<numParams; i++) twoPContraction[i] = (1.00+PSI*RHO)*twoPSimplexCentre[i] - RHO*PSI*twoPSimplex[worst][i];
						twoPContraction[numParams] = calculateTwoPResiduals(twoPContraction, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
						
						if (twoPContraction[numParams] <= twoPReflection[numParams]) {
							for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPContraction[i];
						}
						else {
							// perform a twoPShrink of all vertices except the best
							for (j=0; j<numVertices; j++) {
								if (j != best) {
									//for (i=0; i<numVertices; i++) twoPShrink[i] = 0.00;
									for (i=0; i<numParams; i++) twoPShrink[i] = twoPSimplex[best][i] + SIGMA*(twoPSimplex[j][i]-twoPSimplex[best][i]);
									twoPShrink[numParams] = calculateTwoPResiduals(twoPShrink, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
									for (i=0; i<numVertices; i++) twoPSimplex[j][i] = twoPShrink[i];
								}
							}
						}
					}
					else {
						// perform an inside twoPContraction
						//for (i=0; i<numVertices; i++) twoPContraction[i] = 0.00;
						for (i=0; i<numParams; i++) twoPContraction[i] = (1.00-PSI)*twoPSimplexCentre[i] + PSI*twoPSimplex[worst][i];
						twoPContraction[numParams] = calculateTwoPResiduals(twoPContraction, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
						
						if (twoPContraction[numParams] < twoPSimplex[worst][numParams]) {
							for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPContraction[i];
						}
						else {
							// perform a twoPShrink of all vertices except the best
							for (j=0; j<numVertices; j++) {
								if (j != best) {
									//for (i=0; i<numVertices; i++) twoPShrink[i] = 0.00;
									for (i=0; i<numParams; i++) twoPShrink[i] = twoPSimplex[best][i] + SIGMA*(twoPSimplex[j][i]-twoPSimplex[best][i]);
									twoPShrink[numParams] = calculateTwoPResiduals(twoPShrink, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
									for (i=0; i<numVertices; i++) twoPSimplex[j][i] = twoPShrink[i];
								}
							}
						}
					}
				}
				
			}
			
			// re-evaluate the twoPSimplex vertices from best to worst
			for (i=0; i<numVertices; i++) twoPSimplexResiduals[i] = twoPSimplex[i][numVertices-1];
			calculateBestToWorst(twoPSimplexResiduals, bestToWorst, numVertices);
			best = bestToWorst[0];
			worst = bestToWorst[2];
			secondWorst = bestToWorst[1];
			
			rtol = twoPSimplex[best][numParams] - twoPSimplex[worst][numParams];
			if (rtol < 0.00) rtol = -1.00*rtol;
			
		} // end of main algorithm loop
		
		mo = twoPSimplex[best][0];
		t2 = twoPSimplex[best][1];
		
		if (mo < 0.00) mo = -1.00*mo;
		if (t2 < 0.00) t2 = -1.00*t2;
		
		optimization[0] = mo;
		optimization[1] = t2;
		
		return;
	}
	
	public void threePDownHillSimplex(double[] optimization, double[] initialGuess, double t1, double tr, double[] ssfpSampleData, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
		
		double RHO, CHI, PSI, SIGMA, maxError, usual_delta, zero_term_delta, residual;
		int best, worst, secondWorst;
		
		
		int NMAX, numParams, numVertices, iterations;
		
		double t2, mo, bo;
		double rtol; 
		
		int i, j;
		
		// define algorithm-specific variables
		RHO = 1.00;
		CHI = 2.00;
		PSI = 0.50;
		SIGMA = 0.50;
		usual_delta = 0.05;
		zero_term_delta = .00025;
		
		maxError = 0.001;
		NMAX = 15000;
		
		numParams = 3;
		numVertices = numParams+1;
		
		// initialize the simplex
		simplexLineValues[0] = initialGuess[0]; 
		simplexLineValues[1] = initialGuess[1];
		simplexLineValues[2] = initialGuess[2]; 
		residual = calculateResiduals(simplexLineValues, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
		
		for (i=0; i<numParams; i++) simplex[0][i] = simplexLineValues[i];
		simplex[0][numParams] = residual;
		
		for (i=0; i<numParams; i++) {
			simplexLineValues[0] = initialGuess[0];
			simplexLineValues[1] = initialGuess[1];
			simplexLineValues[2] = initialGuess[2];
			
			if (simplexLineValues[i] != 0.00) simplexLineValues[i] = (1.00+usual_delta)*simplexLineValues[i];
			else simplexLineValues[i] = zero_term_delta;
			residual = calculateResiduals(simplexLineValues, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
			
			simplex[i+1][0] = simplexLineValues[0];
			simplex[i+1][1] = simplexLineValues[1];
			simplex[i+1][2] = simplexLineValues[2];
			simplex[i+1][3] = residual;
		}
		
		// now calculate the best, worst and second worst vertices of the simplex
		for (i=0; i<numVertices; i++) simplexResiduals[i] = simplex[i][numParams];
		calculateBestToWorst(simplexResiduals, bestToWorst, numVertices);
		best = bestToWorst[0];
		worst = bestToWorst[2];
		secondWorst = bestToWorst[1];
		
		// define a fractional range from the best to worst simplex vertex
		rtol = simplex[best][numParams] - simplex[worst][numParams];
		if (rtol < 0.00) rtol = -1.00*rtol;
		
		// now, get on with the main algorithm
		iterations = 0;
		while (rtol > maxError && iterations < NMAX) {
			iterations++;
			
			// calculate the centre of the simplex, ignoring the worst point
			for (i=0; i<numParams; i++) simplexCentre[i] = 0.00;
			for (i=0; i<numVertices; i++) {
				if (i != bestToWorst[numParams-1]) {
					for (j=0; j<numParams; j++) simplexCentre[j] += simplex[i][j];
				}
			}
			for (i=0; i<numParams; i++) simplexCentre[i] = simplexCentre[i]/numParams;
			
			// reflect the simplex through the face of the high point
			//for (i=0; i<numVertices; i++) reflection[i] = 0.00;
			for (i=0; i<numParams; i++) reflection[i] = (1.00+RHO)*simplexCentre[i] - RHO*simplex[worst][i];
			reflection[numParams] = calculateResiduals(reflection, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
			
			
			// compare the reflection vertex with the best vertex in the exisiting simplex
			if (reflection[numParams] < simplex[best][numParams]) {
				// if the reflection was better, try an expansion in this direction and see how that is
				//for (i=0; i<numVertices; i++) expansion[i] = 0.00;
				for (i=0; i<numParams; i++) expansion[i] = (1.00+RHO*CHI)*simplexCentre[i] - RHO*CHI*simplex[worst][i];
				expansion[numParams] = calculateResiduals(expansion, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
				
				if (expansion[numParams] < reflection[numParams]) {
					for (i=0; i<numVertices; i++) simplex[worst][i] = expansion[i];
				}
				else {
					for (i=0; i<numVertices; i++) simplex[worst][i] = reflection[i];
				}
			}
			else {
				if (reflection[numParams] < simplex[secondWorst][numParams]) {
					for (i=0; i<numVertices; i++) simplex[worst][i] = reflection[i];
				}
				else {
					if (reflection[numParams] < simplex[worst][numParams]) {
						// perform an outside contraction
						//for (i=0; i<numVertices; i++) contraction[i] = 0.00;
						for (i=0; i<numParams; i++) contraction[i] = (1.00+PSI*RHO)*simplexCentre[i] - RHO*PSI*simplex[worst][i];
						contraction[numParams] = calculateResiduals(contraction, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
						
						if (contraction[numParams] <= reflection[numParams]) {
							for (i=0; i<numVertices; i++) simplex[worst][i] = contraction[i];
						}
						else {
							// perform a shrink of all vertices except the best
							for (j=0; j<numVertices; j++) {
								if (j != best) {
									//for (i=0; i<numVertices; i++) shrink[i] = 0.00;
									for (i=0; i<numParams; i++) shrink[i] = simplex[best][i] + SIGMA*(simplex[j][i]-simplex[best][i]);
									shrink[numParams] = calculateResiduals(shrink, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
									for (i=0; i<numVertices; i++) simplex[j][i] = shrink[i];
								}
							}
						}
					}
					else {
						// perform an inside contraction
						//for (i=0; i<numVertices; i++) contraction[i] = 0.00;
						for (i=0; i<numParams; i++) contraction[i] = (1.00-PSI)*simplexCentre[i] + PSI*simplex[worst][i];
						contraction[numParams] = calculateResiduals(contraction, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
						
						if (contraction[numParams] < simplex[worst][numParams]) {
							for (i=0; i<numVertices; i++) simplex[worst][i] = contraction[i];
						}
						else {
							// perform a shrink of all vertices except the best
							for (j=0; j<numVertices; j++) {
								if (j != best) {
									//for (i=0; i<numVertices; i++) shrink[i] = 0.00;
									for (i=0; i<numParams; i++) shrink[i] = simplex[best][i] + SIGMA*(simplex[j][i]-simplex[best][i]);
									shrink[numParams] = calculateResiduals(shrink, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
									for (i=0; i<numVertices; i++) simplex[j][i] = shrink[i];
								}
							}
						}
					}
				}
				
			}
			
			// re-evaluate the simplex vertices from best to worst
			for (i=0; i<numVertices; i++) simplexResiduals[i] = simplex[i][numVertices-1];
			calculateBestToWorst(simplexResiduals, bestToWorst, numVertices);
			best = bestToWorst[0];
			worst = bestToWorst[2];
			secondWorst = bestToWorst[1];
			
			rtol = simplex[best][numParams] - simplex[worst][numParams];
			if (rtol < 0.00) rtol = -1.00*rtol;
			
		} // end of main algorithm loop
		
		mo = simplex[best][0];
		t2 = simplex[best][1];
		bo = simplex[best][2];
		
		if (mo < 0.00) mo = -1.00*mo;
		if (t2 < 0.00) t2 = -1.00*t2;
		if (bo < 0.00) bo = -1.00*bo;
		
		optimization[0] = mo;
		optimization[1] = t2;
		optimization[2] = bo;
		
		return;
	}
	
	
	public double calculateTwoPResiduals(double[] simplexLineValues, double bo, double t1, double tr, double[] Signal, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
		
		double mo, e1, e2, phasePrecession, pi, mx, my, guessSignal, residualValue;
		int i;
		
		pi = 3.14159265;
		mo = simplexLineValues[0];
		e1 = Math.exp(-tr/t1);
		e2 = Math.exp(-tr/simplexLineValues[1]);
		
		residualValue = 0.00;
		for (i=0; i<N; i++) {
			phasePrecession = phaseIncrements[i] + 2*pi*((tr/1000.00)*bo);
			
			mx = mo*(1.00-e1)*e2*sina[i]*(Math.cos(phasePrecession)-e2)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
			my = mo*(1.00-e1)*e2*sina[i]*Math.sin(phasePrecession)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
			
			guessSignal = Math.sqrt( Math.pow(mx,2.00) + Math.pow(my,2.00) );
			
			residualValue += Math.pow( (Signal[i]-guessSignal),2.00 );
		}
		
		return residualValue;
	}
	
	public double calculateResiduals(double[] simplexLineValues, double t1, double tr, double[] Signal, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
		
		double mo, e1, e2, phasePrecession, pi, mx, my, guessSignal, residualValue;
		int i;
		
		pi = 3.14159265;
		mo = simplexLineValues[0];
		e1 = Math.exp(-tr/t1);
		e2 = Math.exp(-tr/simplexLineValues[1]);
		
		residualValue = 0.00;
		for (i=0; i<N; i++) {
			phasePrecession = phaseIncrements[i] + 2*pi*((tr/1000.00)*simplexLineValues[2]);
			
			mx = mo*(1.00-e1)*e2*sina[i]*(Math.cos(phasePrecession)-e2)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
			my = mo*(1.00-e1)*e2*sina[i]*Math.sin(phasePrecession)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
			
			guessSignal = Math.sqrt( Math.pow(mx,2.00) + Math.pow(my,2.00) );
			
			residualValue += Math.pow( (Signal[i]-guessSignal),2.00 );
		}
		
		return residualValue;
	}
	
	public double calculate2PResiduals(double[] simplexLineValues, double bo, double t1, double tr, double[] Signal, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
		
		double mo, e1, e2, phasePrecession, pi, mx, my, guessSignal, residualValue;
		int i;
		
		pi = 3.14159265;
		mo = simplexLineValues[0];
		e1 = Math.exp(-tr/t1);
		e2 = Math.exp(-tr/simplexLineValues[1]);
		
		residualValue = 0.00;
		for (i=0; i<N; i++) {
			phasePrecession = phaseIncrements[i] + 2*pi*((tr/1000.00)*bo);
			
			mx = mo*(1.00-e1)*e2*sina[i]*(Math.cos(phasePrecession)-e2)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
			my = mo*(1.00-e1)*e2*sina[i]*Math.sin(phasePrecession)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
			
			guessSignal = Math.sqrt( Math.pow(mx,2.00) + Math.pow(my,2.00) );
			
			residualValue += Math.pow( (Signal[i]-guessSignal),2.00 );
		}
		
		return residualValue;
	}
	
	public void calculateBestToWorst(double[] simplexResiduals, int[] bestToWorst, int numVertices) {
		
		int i, best, worst, secondWorst;
		
		best = 0;
		worst = 0;
		secondWorst = 0;
		
		for (i=0; i<numVertices; i++) {
			if (simplexResiduals[i] < simplexResiduals[best]) best = i;
			if (simplexResiduals[i] > simplexResiduals[worst]) worst = i;
		}
		
		secondWorst = best;
		for (i=0; i<numVertices; i++) {
			if (i != worst) {
				if (simplexResiduals[i] > simplexResiduals[secondWorst]) secondWorst = i;
			}
		}
		bestToWorst[0] = best;
		bestToWorst[1] = secondWorst;
		bestToWorst[2] = worst;
		
		return;
	}
	
	/**
     * This method gives <code>clo</code> most of the image attributes of <code>orig</code> besides
     * data type to minimize rounding errors.
     * 
     * @param orig Original image
     * @param clo Near clone image
     */
    public ModelImage nearCloneImage(ModelImage orig, ModelImage clo) {
        FileInfoBase[] oArr = orig.getFileInfo();
        FileInfoBase[] cArr = clo.getFileInfo();
        if(cArr.length != oArr.length) {
            MipavUtil.displayError("Images are not same length");
            return clo;
        }
        for(int i=0; i<cArr.length; i++) {
            if(cArr == null) {
                return clo;
            }
            cArr[i].setOffset(oArr[i].getOffset());
            cArr[i].setEndianess(oArr[i].getEndianess());
            cArr[i].setResolutions(oArr[i].getResolutions().clone());
            cArr[i].setUnitsOfMeasure(oArr[i].getUnitsOfMeasure().clone());
            cArr[i].setOrigin(oArr[i].getOrigin().clone());
            cArr[i].setImageOrientation(cArr[i].getImageOrientation());
            cArr[i].setAxisOrientation(oArr[i].getAxisOrientation().clone());
            cArr[i].setDataType(ModelImage.DOUBLE);
        }
        
        return clo;
    }
	
	public void reduceBoField(double[][] boField, double resonancePeriod, int width, int height) {
		int i,j;
		double fraction, offResonanceMod;
		int repetitionCycle;
		
		for (i=0; i<width; i++) {
			for (j=0; j<height; j++) {
				
				repetitionCycle = 0;
				
				fraction = boField[i][j]/resonancePeriod;
				if (fraction >= 1) repetitionCycle = (int) (Math.floor(fraction));
				if (fraction < 1 && fraction >= 0.5) repetitionCycle = 1;
				if (fraction < 0.5) repetitionCycle = 0;
				
				offResonanceMod = boField[i][j] - (repetitionCycle*resonancePeriod);
				if (offResonanceMod < 0.00) offResonanceMod = -1.00*offResonanceMod;
				
				boField[i][j] = offResonanceMod;
				
				
			}
		}
		
		return;
	}
	
	
	public void smoothField(double[][] field, float[][] fieldValues, int width, int height, int k, double[][] Gaussian) {
		int x, y, p1, p2;
		int pixelIndex;
		double smoothedValue;
		
		pixelIndex = 0;
		
		for (x=0; x<width; x++) {
			for (y=0; y<height; y++) {
				
				if (y>2 && y<height-2 && x>2 && x<width-2) {
					smoothedValue = 0.00;
					for (p1=0; p1<5; p1++) {
						for (p2=0; p2<5; p2++) smoothedValue += field[x-2+p1][y-2+p2]*Gaussian[p1][p2];
					}
					smoothedValue = smoothedValue / 34.00;
					
					fieldValues[k-1][pixelIndex] = (float) smoothedValue;
				}
				else fieldValues[k-1][pixelIndex] = (float) 0.00;
				
				pixelIndex++;
				
				
			} // close y loop
		} // close x loop
		
		
		return;
	}
	
	public void smoothFieldB(double[][] field, int width, int height, double[][] Gaussian) {
		int x, y, p1, p2;
		double smoothedValue;
		double[][] smoothedField;
		
		smoothedField = new double[width][height];
		for (y=0; y<height; y++) {
			for (x=0; x<width; x++) {
				smoothedField[x][y] = 0.00;
			}
		}
		
		
		for (y=2; y<height-2; y++) {
			for (x=2; x<width-2; x++) {
				
				smoothedValue = 0.00;
				for (p1=0; p1<5; p1++) {
					for (p2=0; p2<5; p2++) smoothedValue += field[x-2+p1][y-2+p2]; //Gaussian[p1][p2]*;
				}
				smoothedValue = smoothedValue / 34.00;
				smoothedField[x][y] = smoothedValue;
				
			} // close y loop
		} // close x loop
		
		 
		
		for (y=0; y<height; y++) {
			for (x=0; x<width; x++) {
				field[x][y] = smoothedField[x][y];
			}
		}
		
		return;
	}
		
	
	public void resetSliceToZero(double[][] field, int width, int height) {
		int x, y;
		
		for (y=0; y<height; y++) {
			for (x=0; x<width; x++) {
				
				field[x][y] = 0.00;
				
				
			} // close y loop
		} // close x loop
		
		
		return;
	}
	
	public void swapMatrixForVector(double[][] fieldField, float[][] fieldValues, int k, int width, int height) {
		int x, y, pixelIndex;
		
		pixelIndex = 0;
		
		for (x=0; x<width; x++) {
			for (y=0; y<height; y++) {
				fieldValues[k-1][pixelIndex] = (float) fieldField[x][y];
				pixelIndex++;
			}
		}
		
		return;
	}
	
	public void swapMatrixForVectorB(double[][] fieldField, float[][] fieldValues, int k, int width, int height) {
		int x, y, pixelIndex;
		
		pixelIndex = 0;
		
		for (y=0; y<height; y++) {
			for (x=0; x<width; x++) {
				fieldValues[k-1][pixelIndex] = (float) fieldField[x][y];
				pixelIndex++;
			}
		}
		
		return;
	}
	
	private class DespotPerform extends Thread {
    
        private boolean hardInterrupt = false;
        private ModelImage t2ResultStack;
        private ModelImage moResultStack;
        private ModelImage r2ResultStack;
        private ModelImage boResultStack;
        
        public void run() {
            if (performFullModelling == true || performApproxModelling == true) {
                if (performFullModelling == true) calculateT2withFullModelling();
                else calculateT2withApproximateModelling();
            }
            else {
                if (performConventionalWith0Phase == true) calculateT2with0Phase();
                else calculateT2with180Phase();
            }
    
            if(progressBar != null) {
                progressBar.setMessage("That all took "+(System.currentTimeMillis()-start)/1000.0+" seconds");
                progressBar.updateValue(99);
                progressBar.dispose();
            }
            
            if(hardInterrupt) {
                if(t2ResultStack != null) {
                    t2ResultStack.disposeLocal();
                }
                
                if(moResultStack != null) {
                    moResultStack.disposeLocal();
                }
    
                if(r2ResultStack != null) {
                    r2ResultStack.disposeLocal();
                }
    
                if(boResultStack != null) {
                    boResultStack.disposeLocal();
                }
            }
        }
        
    	public void calculateT2with0Phase() {
    		ModelImage image;
    		float[] ctable;
    		
    		
    		double[] fa_phase0;
    		double[] scaledFA_phase0;
    		
    		double[][] ssfpPixelValues_phase0;
    		double[] t1PixelValues, b1PixelValues;
    		double[] phase0Data;
    		
    		float[][] t2Values, moValues, r2Values;
    		
    		double a, d, e2;
    		double sumX, sumY, sumXY, sumXX, slope, denominator, intercept, lnslope, t1, t2, e1, mo, r2;
    		double x1, x2, y1, y2;
    		double[] possibleT2s, possibleMos;
    		float noiseSum, threshold;
    		
    		int width, height, nSlices;
    		int x,y,i,j,k,angle, p, ti, p1, p2, pixelIndex, noiseIndex;
    		
    		image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[0]]);
    		width = image.getExtents()[0];
    		height = image.getExtents()[1];
    		if(image.getNDims() > 2) {
    			nSlices = image.getExtents()[2];
    		} else {
    			nSlices = 1;
    		}
    		
    		ssfpPixelValues_phase0 = new double[Nfa_phase0][width*height];
    		t1PixelValues = new double[width*height];
    		if (includeB1Map) b1PixelValues = new double[width*height];
    		else b1PixelValues = new double[1];
    		
    		if (calculateT2) t2Values = new float[nSlices][width*height];
    		else t2Values = new float[1][1];
    		if (calculateMo) moValues = new float[nSlices][width*height];
    		else moValues = new float[1][1];
    		if (invertT2toR2) r2Values = new float[nSlices][width*height];
    		else r2Values = new float[1][1];
    		
    		t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2 Results");
    		moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo Results");
    		r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2 Results");
    		
    		t2ResultStack = nearCloneImage(image, t2ResultStack);
            moResultStack = nearCloneImage(image, moResultStack);
            r2ResultStack = nearCloneImage(image, r2ResultStack);
    		
    		fa_phase0 = new double[Nfa_phase0];
    		scaledFA_phase0 = new double[Nfa_phase0];
    		for (angle=0; angle<Nfa_phase0; angle++) {
    			fa_phase0[angle] = Math.toRadians(despotFA_phase0[angle]);
    		}
    		
    		phase0Data = new double[Nfa_phase0];
    		
    		// Actually perform the T2 Calculations
    		for (k=0; k<nSlices; k++) {
    			progressBar.setMessage("calculating T2 values on slice: "+k+" of "+nSlices);
                progressBar.updateValue(0+(int)(((float)k+1.0)/(float)nSlices*90.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
    			// grab the ssfp pixel values from the phase = 0 data
    			for (angle=0; angle<Nfa_phase0; angle++) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// grab the T1 and B1 information
    			image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
    			
    			pixelIndex = 0;
    			for (y=0; y<height; y++) {
    				for (x=0; x<width; x++) {
    					t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
    					pixelIndex++;
    				}
    			}
    			
    			if (includeB1Map == true) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						b1PixelValues[pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// now that we have all the information, perform the calculate pixel-wise
    			pixelIndex = 0;
    			for (x=0; x<width; x++) {
    				for (y=0; y<height; y++) {
    					
    					if (t1PixelValues[pixelIndex] > 0.00) {
    						
    						e1 = Math.exp(-despotTR/t1PixelValues[pixelIndex]);
    						
    						// scale up (or down) the flip angles based on the calculated B1 if required
    						if (includeB1Map == true) {
    							for (p=0; p<Nfa_phase0; p++) scaledFA_phase0[p] = fa_phase0[p]*b1PixelValues[pixelIndex];
    						}
    						else {
    							for (p=0; p<Nfa_phase0; p++) scaledFA_phase0[p] = fa_phase0[p];
    						}
    						
    						// grab the SSFP values for this pixel
    						for (p=0; p<Nfa_phase0; p++) phase0Data[p] = ssfpPixelValues_phase0[p][pixelIndex];
    						
    						intercept = 1.00;
    						denominator = 1.00;
    						slope = 0.00;
    						e2 = 1.00;
    						t2 = 0.00;
    						
    						// calculate T2 first from the phase = 0 data
    						sumX = 0.00;
    						sumY = 0.00;
    						sumXY = 0.00;
    						sumXX = 0.00;
    						
    						for (p=0; p<Nfa_phase0; p++) {
    							sumX += phase0Data[p]/Math.tan(scaledFA_phase0[p]);
    							sumY += phase0Data[p]/Math.sin(scaledFA_phase0[p]);
    							sumXY += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.sin(scaledFA_phase0[p]));
    							sumXX += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.tan(scaledFA_phase0[p]));
    						}
    						
    						d = (Nfa_phase0*sumXX) - sumX*sumX;
    						a = (Nfa_phase0*sumXY) - (sumX*sumY);
    						
    						if (d != 0) {
    							slope = a/d;
    							denominator = (e1-slope)/(1.00-slope*e1);
    							intercept = (sumY-slope*sumX)/Nfa_phase0;
    							
    							if (denominator > 0.00 && denominator < 1.00) {
    								t2 = -despotTR/Math.log(denominator);
    								e2 = Math.exp(-despotTR/t2);
    								mo = intercept*(1.00-e1*e2)/(1.00-e1);
    							}
    							else {
    								mo = maxMo;
    								t2 = maxT2;
    							}
    						}
    						else {
    							mo = maxMo;
    							t2 = maxT2;
    						}
    						
    						
    						if (t2 < 0.00 || t2 > maxT2) {
    							t2 = maxT2;
    						}
    						if (mo < 0.00 || mo > maxMo) {
    							mo = maxMo;
    						}
    						
    						// invert to r2
    						if (t2 != 0.00) {
    							r2 = 1.00/t2;
    						}
    						else {
    							r2 = 0.00;
    						}
    						
    						
    						if (calculateT2) t2Values[k][pixelIndex] = (float) t2;
    						if (calculateMo) moValues[k][pixelIndex] = (float) mo;
    						if (invertT2toR2) r2Values[k][pixelIndex] = (float) r2;
    					}
    					else {
    						if (calculateT2) t2Values[k][pixelIndex] = (float) 0.00;
    						if (calculateMo) moValues[k][pixelIndex] = (float) 0.00;
    						if (invertT2toR2) r2Values[k][pixelIndex] = (float) 0.00;
    					}
    					pixelIndex++;
    				}
    			}
    			
    			try {
    				if (calculateT2) {
    				    t2ResultStack.importData(image.getSliceSize()*k, t2Values[k], true);
    				}
    				if (calculateMo) {
    				    moResultStack.importData(image.getSliceSize()*k, moValues[k], true);
    				}
    				if (invertT2toR2) {
    				    r2ResultStack.importData(image.getSliceSize()*k, r2Values[k], true);
    				}
    			} catch(IOException e) {
    				e.printStackTrace();
    				MipavUtil.displayError("Data could not be imported into result image");
    			}
    		}
    		
    		if (calculateT2) {
    			ViewJFrameImage t2ResultWindow = new ViewJFrameImage(t2ResultStack);
    			t2ResultWindow.setTitle("DESPOT2-T2_Map");
    			t2ResultWindow.setVisible(true);
    		} else if(t2ResultStack != null) {
    			t2ResultStack.disposeLocal();
    		}
    		
    		if (calculateMo) {
    			ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
    			moResultWindow.setTitle("DESPOT2_MoMap");
    			moResultWindow.setVisible(true);
    		} else if(moResultStack != null) {
    			moResultStack.disposeLocal();
    		}
    		
    		if (invertT2toR2) {
    			ViewJFrameImage r2ResultWindow = new ViewJFrameImage(r2ResultStack);
    			r2ResultWindow.setTitle("DESPOT2-R2Map");
    			r2ResultWindow.setVisible(true);
    		} else if(r2ResultStack != null) {
    			r2ResultStack.disposeLocal();
    		}
    		
    	}
    	
    	public void calculateT2with180Phase() {
    		ModelImage image;
    		float[] ctable;
    		
    		
    		double[] fa_phase180;
    		double[] scaledFA_phase180;
    		
    		double[][] ssfpPixelValues_phase180;
    		double[] t1PixelValues, b1PixelValues;
    		double[] phase180Data;
    		
    		float[][] t2Values, moValues, r2Values;
    		
    		double a, d, e2;
    		double sumX, sumY, sumXY, sumXX, slope, denominator, intercept, lnslope, t1, t2, e1, mo, r2;
    		double x1, x2, y1, y2;
    		double[] possibleT2s, possibleMos;
    		float noiseSum, threshold;
    		
    		int width, height, nSlices;
    		int x,y,i,j,k,angle, p, ti, p1, p2, pixelIndex, noiseIndex;
    
    		image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[0]]);
    		width = image.getExtents()[0];
    		height = image.getExtents()[1];
    		if(image.getNDims() > 2) {
    			nSlices = image.getExtents()[2];
    		} else {
    			nSlices = 1;
    		}
    		
    		ssfpPixelValues_phase180 = new double[Nfa_phase180][width*height];
    		t1PixelValues = new double[width*height];
    		if (includeB1Map) b1PixelValues = new double[width*height];
    		else b1PixelValues = new double[1];
    		
    		if (calculateT2) t2Values = new float[nSlices][width*height];
    		else t2Values = new float[1][1];
    		if (calculateMo) moValues = new float[nSlices][width*height];
    		else moValues = new float[1][1];
    		if (invertT2toR2) r2Values = new float[nSlices][width*height];
    		else r2Values = new float[1][1];
    		
    		t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2 Results");
    		moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo Results");
    		r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2 Results");
    		
    		t2ResultStack = nearCloneImage(image, t2ResultStack);
    		moResultStack = nearCloneImage(image, moResultStack);
    		r2ResultStack = nearCloneImage(image, r2ResultStack);
    		
    		fa_phase180 = new double[Nfa_phase180];
    		scaledFA_phase180 = new double[Nfa_phase180];
    		for (angle=0; angle<Nfa_phase180; angle++) {
    			fa_phase180[angle] = Math.toRadians(despotFA_phase180[angle]);
    		}
    		
    		phase180Data = new double[Nfa_phase180];
    		
    		// Actually perform the T2 Calculations
    		for (k=0; k<nSlices; k++) {
    		    progressBar.setMessage("calculating T2 values on slice: "+k+" of "+nSlices);
                progressBar.updateValue(0+(int)(((float)k+1.0)/(float)nSlices*90.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
    			// grab the ssfp pixel values from the phase = 180 data
    			for (angle=0; angle<Nfa_phase180; angle++) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x,y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// grab the T1 and B1 information
    			image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
    			
    			pixelIndex = 0;
    			for (y=0; y<height; y++) {
    				for (x=0; x<width; x++) {
    					t1PixelValues[pixelIndex] = image.getDouble(x,y, k);
    					pixelIndex++;
    				}
    			}
    			
    			if (includeB1Map == true) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						b1PixelValues[pixelIndex] = image.getDouble(x,y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// now that we have all the information, perform the calculate pixel-wise
    			pixelIndex = 0;
    			for (x=0; x<width; x++) {
    				for (y=0; y<height; y++) {
    					
    					if (t1PixelValues[pixelIndex] > 0.00) {
    						
    						e1 = Math.exp(-despotTR/t1PixelValues[pixelIndex]);
    						
    						// scale up (or down) the flip angles based on the calculated B1 if required
    						if (includeB1Map == true) {
    							for (p=0; p<Nfa_phase180; p++) scaledFA_phase180[p] = fa_phase180[p]*b1PixelValues[pixelIndex];
    						}
    						
    						else {
    							for (p=0; p<Nfa_phase180; p++) scaledFA_phase180[p] = fa_phase180[p];
    						}
    						
    						// grab the SSFP values for this pixel
    						for (p=0; p<Nfa_phase180; p++) phase180Data[p] = ssfpPixelValues_phase180[p][pixelIndex];
    						
    						intercept = 1.00;
    						denominator = 1.00;
    						slope = 0.00;
    						e2 = 1.00;
    						t2 = 0.00;
    						
    						// calculate T2 first from the phase = 180 data
    						sumX = 0.00;
    						sumY = 0.00;
    						sumXY = 0.00;
    						sumXX = 0.00;
    						
    						for (p=0; p<Nfa_phase180; p++) {
    							sumX += phase180Data[p]/Math.tan(scaledFA_phase180[p]);
    							sumY += phase180Data[p]/Math.sin(scaledFA_phase180[p]);
    							sumXY += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.sin(scaledFA_phase180[p]));
    							sumXX += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.tan(scaledFA_phase180[p]));
    						}
    						
    						d = (Nfa_phase180*sumXX) - sumX*sumX;
    						a = (Nfa_phase180*sumXY) - (sumX*sumY);
    						
    						if (d != 0) {
    							slope = a/d;
    							denominator = (slope-e1)/(slope*e1-1.00);
    							intercept = (sumY-slope*sumX)/Nfa_phase180;
    							
    							if (denominator > 0.00 && denominator < 1.00) {
    								t2 = -despotTR/Math.log(denominator);
    								e2 = Math.exp(-despotTR/t2);
    								mo = intercept*(1.00-e1*e2)/(1.00-e1);
    							}
    							else {
    								mo = maxMo;
    								t2 = maxT2;
    							}
    						}
    						else {
    							mo = maxMo;
    							t2 = maxT2;
    						}
    						
    						
    						if (t2 < 0.00 || t2 > maxT2) {
    							t2 = maxT2;
    						}
    						if (mo < 0.00 || mo > maxMo) {
    							mo = maxMo;
    						}
    												
    						// invert to r2
    						if (t2 != 0.00) {
    							r2 = 1.00/t2;
    						}
    						else {
    							r2 = 0.00;
    						}
    						
    						
    						if (calculateT2) t2Values[k][pixelIndex] = (float) t2;
    						if (calculateMo) moValues[k][pixelIndex] = (float) mo;
    						if (invertT2toR2) r2Values[k][pixelIndex] = (float) r2;
    					}
    					else {
    						if (calculateT2) t2Values[k][pixelIndex] = (float) 0.00;
    						if (calculateMo) moValues[k][pixelIndex] = (float) 0.00;
    						if (invertT2toR2) r2Values[k][pixelIndex] = (float) 0.00;
    					}
    					pixelIndex++;
    				}
    			}
    			
    			try {
    				if (calculateT2) t2ResultStack.importData(image.getSliceSize()*k, t2Values[k], true);
    				if (calculateMo) moResultStack.importData(image.getSliceSize()*k, moValues[k], true);
    				if (invertT2toR2) r2ResultStack.importData(image.getSliceSize()*k, r2Values[k], true);
    			} catch(IOException e) {
    				e.printStackTrace();
    				MipavUtil.displayError("Data could not be imported into result image");
    			}
    		}
    		
    		if (calculateT2) {
    			ViewJFrameImage t2ResultWindow = new ViewJFrameImage(t2ResultStack);
    			t2ResultWindow.setTitle("DESPOT2_T2_Map");
    			t2ResultWindow.setVisible(true);
    		} else if(t2ResultStack != null) {
    			t2ResultStack.disposeLocal();
    		}
    		
    		if (calculateMo) {
    			ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
    			moResultWindow.setTitle("DESPOT2_MoMap");
    			moResultWindow.setVisible(true);
    		} else if(moResultStack != null) {
    			moResultStack.disposeLocal();
    		}
    		
    		if (invertT2toR2) {
    			ViewJFrameImage r2ResultWindow = new ViewJFrameImage(r2ResultStack);
    			r2ResultWindow.setTitle("DESPOT2_R2Map");
    			r2ResultWindow.setVisible(true);
    		} else if(r2ResultStack != null) {
    			r2ResultStack.disposeLocal();
    		}
    	}
    	
    	
    
    	public void calculateT2withApproximateModelling() {
    		ModelImage image;
    		float[] ctable;
    		
    		
    		double[] fa_phase0, fa_phase180;
    		double[] scaledFA_phase0, scaledFA_phase180;
    		
    		double[][] ssfpPixelValues_phase0, ssfpPixelValues_phase180;
    		double[] t1PixelValues, b1PixelValues;
    		double[] phase0Data, phase180Data;
    		
    		float[][] t2Values, moValues, r2Values;
    		
    		double a, d, e2;
    		double sumX, sumY, sumXY, sumXX, slope, denominator, intercept, lnslope, t1, t2, e1, mo, r2;
    		double x1, x2, y1, y2;
    		double[] possibleT2s, possibleMos;
    		float noiseSum, threshold;
    		float Residuals, guessDiffence;
    		float [] lastGuess, recentGuess;
    		
    		int width, height, nSlices;
    		int x,y,i,j,k,angle, p, ti, p1, p2, pixelIndex, noiseIndex;
    
    		image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[0]]);
    		width = image.getExtents()[0];
    		height = image.getExtents()[1];
    		if(image.getNDims() > 2) {
    			nSlices = image.getExtents()[2];
    		} else {
    			nSlices = 1;
    		}
    		
    		ssfpPixelValues_phase0 = new double[Nfa_phase0][width*height];
    		ssfpPixelValues_phase180 = new double[Nfa_phase180][width*height];
    		t1PixelValues = new double[width*height];
    		if (includeB1Map) b1PixelValues = new double[width*height];
    		else b1PixelValues = new double[1];
    		
    		if (calculateT2) t2Values = new float[nSlices][width*height];
    		else t2Values = new float[1][1];
    		if (calculateMo) moValues = new float[nSlices][width*height];
    		else moValues = new float[1][1];
    		if (invertT2toR2) r2Values = new float[nSlices][width*height];
    		else r2Values = new float[1][1];
    		
    		t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2 Results");
    		moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo Results");
    		r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2 Results");
    		
    		t2ResultStack = nearCloneImage(image, t2ResultStack);
            moResultStack = nearCloneImage(image, moResultStack);
            r2ResultStack = nearCloneImage(image, r2ResultStack);
    		
    		fa_phase0 = new double[Nfa_phase0];
    		fa_phase180 = new double[Nfa_phase180];
    		scaledFA_phase0 = new double[Nfa_phase0];
    		scaledFA_phase180 = new double[Nfa_phase180];
    		for (angle=0; angle<Nfa_phase0; angle++) {
    			fa_phase0[angle] = Math.toRadians(despotFA_phase0[angle]);
    		}
    		for (angle=0; angle<Nfa_phase180; angle++) {
    			fa_phase180[angle] = Math.toRadians(despotFA_phase180[angle]);
    		}
    		
    		phase0Data = new double[Nfa_phase0];
    		phase180Data = new double[Nfa_phase180];
    		
    		possibleT2s = new double[2];
    		possibleMos = new double[2];
    		
    		// Actually perform the T2 Calculations
    		for (k=0; k<nSlices; k++) {
    			progressBar.setMessage("calculating T2 values on slice: "+k+" of "+nSlices);
                progressBar.updateValue(0+(int)(((float)k+1.0)/(float)nSlices*90.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
    			// grab the ssfp pixel values from the phase = 0 data
    			for (angle=0; angle<Nfa_phase0; angle++) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// grab the ssfp pixel values from the phase = 180 data
    			for (angle=0; angle<Nfa_phase180; angle++) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// grab the T1 and B1 information
    			image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
    			
    			pixelIndex = 0;
    			for (y=0; y<height; y++) {
    				for (x=0; x<width; x++) {
    					t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
    					pixelIndex++;
    				}
    			}
    			
    			if (includeB1Map == true) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						b1PixelValues[pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// now that we have all the information, perform the calculate pixel-wise
    			pixelIndex = 0;
    			for (x=0; x<width; x++) {
    				for (y=0; y<height; y++) {
    				
    					if (t1PixelValues[pixelIndex] > 0.00) {
    						
    						e1 = Math.exp(-despotTR/t1PixelValues[pixelIndex]);
    						
    						// scale up (or down) the flip angles based on the calculated B1 if required
    						if (includeB1Map == true) {
    							for (p=0; p<Nfa_phase0; p++) scaledFA_phase0[p] = fa_phase0[p]*b1PixelValues[pixelIndex];
    							for (p=0; p<Nfa_phase180; p++) scaledFA_phase180[p] = fa_phase180[p]*b1PixelValues[pixelIndex];
    						}
    						else {
    							for (p=0; p<Nfa_phase0; p++) {
    								scaledFA_phase0[p] = fa_phase0[p];
    								scaledFA_phase180[p] = fa_phase180[p];
    							}
    						}
    						
    						// grab the SSFP values for this pixel
    						for (p=0; p<Nfa_phase0; p++) phase0Data[p] = ssfpPixelValues_phase0[p][pixelIndex];
    						for (p=0; p<Nfa_phase180; p++) phase180Data[p] = ssfpPixelValues_phase180[p][pixelIndex];
    						
    						intercept = 1.00;
    						denominator = 1.00;
    						slope = 0.00;
    						e2 = 1.00;
    						t2 = 0.00;
    						
    						// calculate T2 first from the phase = 0 data
    						sumX = 0.00;
    						sumY = 0.00;
    						sumXY = 0.00;
    						sumXX = 0.00;
    					
    						for (p=0; p<Nfa_phase0; p++) {
    							sumX += phase0Data[p]/Math.tan(scaledFA_phase0[p]);
    							sumY += phase0Data[p]/Math.sin(scaledFA_phase0[p]);
    							sumXY += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.sin(scaledFA_phase0[p]));
    							sumXX += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.tan(scaledFA_phase0[p]));
    						}
    						
    						d = (Nfa_phase0*sumXX) - sumX*sumX;
    						a = (Nfa_phase0*sumXY) - (sumX*sumY);
    						
    						if (d != 0) {
    							slope = a/d;
    							denominator = (e1-slope)/(1.00-slope*e1);
    							intercept = (sumY-slope*sumX)/Nfa_phase0;
    							
    							if (denominator > 0.00 && denominator < 1.00) {
    								t2 = -despotTR/Math.log(denominator);
    								e2 = Math.exp(-despotTR/t2);
    								mo = intercept*(1.00-e1*e2)/(1.00-e1);
    							}
    							else {
    								mo = 0.00;
    								t2 = 0.00;
    							}
    						}
    						else {
    							mo = 0.00;
    							t2 = 0.00;
    						}
    						
    
    						if (t2 < 0.00 || t2 > maxT2) {
    							t2 = maxT2;
    						}
    						if (mo < 0.00 || mo > maxMo) {
    							mo = 0.00;
    						}
    					
    			
    						possibleT2s[0] = t2;
    						possibleMos[0] = mo;
    						
    						
    						// calculate T2 first from the phase = 180 data
    						sumX = 0.00;
    						sumY = 0.00;
    						sumXY = 0.00;
    						sumXX = 0.00;
    						
    						for (p=0; p<Nfa_phase180; p++) {
    							sumX += phase180Data[p]/Math.tan(scaledFA_phase180[p]);
    							sumY += phase180Data[p]/Math.sin(scaledFA_phase180[p]);
    							sumXY += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.sin(scaledFA_phase180[p]));
    							sumXX += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.tan(scaledFA_phase180[p]));
    						}
    						
    						d = (Nfa_phase180*sumXX) - sumX*sumX;
    						a = (Nfa_phase180*sumXY) - (sumX*sumY);
    						
    						if (d != 0) {
    							slope = a/d;
    							denominator = (slope-e1)/(slope*e1-1.00);
    							intercept = (sumY-slope*sumX)/Nfa_phase180;
    							
    							if (denominator > 0.00 && denominator < 1.00) {
    								t2 = -despotTR/Math.log(denominator);
    								e2 = Math.exp(-despotTR/t2);
    								mo = intercept*(1.00-e1*e2)/(1.00-e1);
    							}
    							else {
    								mo = 0.00;
    								t2 = 0.00;
    							}
    						}
    						else {
    							mo = 0.00;
    							t2 = 0.00;
    						}
    						
    						
    						if (t2 < 0.00 || t2 > maxT2) {
    							t2 = maxT2;
    						}
    						if (mo < 0.00 || mo > maxMo) {
    							mo = 0.00;
    						}
    						
    						possibleT2s[1] = t2;
    						possibleMos[1] = mo;
    					
    						
    						// now, choose the maximum T2 and the corresponding mo value
    						if (possibleT2s[0] >= possibleT2s[1]) {
    							t2 = possibleT2s[0];
    							mo = possibleMos[0];
    						}
    						else {
    							t2 = possibleT2s[1];
    							mo = possibleMos[1];
    						}
    						 
    						
    						//t2 = (possibleT2s[0]+possibleT2s[1])/2.00;
    						//mo = (possibleMos[0]+possibleMos[1])/2.00;
    						
    						
    						// invert to r2
    						if (t2 != 0.00) {
    							r2 = 1.00/t2;
    						}
    						else {
    							r2 = 0.00;
    						}
    					
    						
    						if (calculateT2) t2Values[k][pixelIndex] = (float) t2;
    						if (calculateMo) moValues[k][pixelIndex] = (float) mo;
    						if (invertT2toR2) r2Values[k][pixelIndex] = (float) r2;
    					}
    					else {
    						if (calculateT2) t2Values[k][pixelIndex] = (float) 0.00;
    						if (calculateMo) moValues[k][pixelIndex] = (float) 0.00;
    						if (invertT2toR2) r2Values[k][pixelIndex] = (float) 0.00;
    					}
    					pixelIndex++;
    				}
    			}
    			
    			try {
    				if (calculateT2) t2ResultStack.importData(image.getSliceSize()*k, t2Values[k], true);
    				if (calculateMo) moResultStack.importData(image.getSliceSize()*k, moValues[k], true);
    				if (invertT2toR2) r2ResultStack.importData(image.getSliceSize()*k, r2Values[k], true);
    			} catch(IOException e) {
    				e.printStackTrace();
    				MipavUtil.displayError("Data could not be imported into result image");
    			}
    		}
    		
    		if (calculateT2) {
    			ViewJFrameImage t2ResultWindow = new ViewJFrameImage(t2ResultStack);
    			t2ResultWindow.setTitle("CalculatedT2Map_AM");
    			t2ResultWindow.setVisible(true);
    		} else if(t2ResultStack != null) {
    			t2ResultStack.disposeLocal();
    		}
    		
    		if (calculateMo) {
    			ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
    			moResultWindow.setTitle("CalculatedMoMap_AM");
    			moResultWindow.setVisible(true);
    		} else if(moResultStack != null) {
    			moResultStack.disposeLocal();
    		}
    		
    		if (invertT2toR2) {
    			ViewJFrameImage r2ResultWindow = new ViewJFrameImage(r2ResultStack);
    			r2ResultWindow.setTitle("CalculatedR2Map_AM");
    			r2ResultWindow.setVisible(true);
    		} else if(r2ResultStack != null) {
    			r2ResultStack.disposeLocal();
    		}
    	}
    	
    	public void calculateT2withFullModelling() {
    		progressBar.setMessage("prepping data - hang on");
    		ModelImage image;
    		float[] ctable;
    		
    		double[] FA, scaledFA, phaseIncrements, sina, cosa;
    		
    		double[][] ssfpPixelValues_phase0, ssfpPixelValues_phase180;
    		double[] t1PixelValues, b1PixelValues;
    		double[] ssfpSampleData;
    		
    		float[][] boField, moField, t2Field, r2Field;
    		double[][][] t2Values, moValues, r2Values, boValues;
    		double[][] Gaussian;
    		double smoothedBo;
    		
    		
    		double[] optimization, initialGuess;
    		double[] twoPOptimization, twoPInitialGuess;
    		double Residuals, guessDifference, lowestResiduals;
    		double[] lastGuess, recentGuess, bestGuess;
    		int restartIndex;
    		int numParams, numVertices;
    		
    		double offResonanceMod, resonancePeriod;
    		int repetitionCycle;
    		
    		optimization = new double[3];
    		initialGuess = new double[3];
    		twoPOptimization = new double[2];
    		twoPInitialGuess = new double[2];
    		
    		lastGuess = new double[3];
    		recentGuess = new double[3];
    		bestGuess = new double[3];
    		
    		int iterations;
    		
    		double t2, mo, bo, r2;
    		double rtol; 
    		
    		double t1, tr;
    		
    		int width, height, nSlices;
    		int x,y,i,j,k,angle, p, pixelIndex, p1,p2;
    		
    		image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[0]]);
    		width = image.getExtents()[0];
    		height = image.getExtents()[1];
    		if(image.getNDims() > 2) {
    			nSlices = image.getExtents()[2];
    		} else {
    			nSlices = 1;
    		}
    		
    		ssfpPixelValues_phase0 = new double[Nfa_phase0][width*height];
    		ssfpPixelValues_phase180 = new double[Nfa_phase180][width*height];
    		t1PixelValues = new double[width*height];
    		if (includeB1Map) b1PixelValues = new double[width*height];
    		else b1PixelValues = new double[1];
    		
    		t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2 Results");
    		moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo Results");
    		r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2 Results");
    		boResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "bo Results");
    		
    		t2ResultStack = nearCloneImage(image, t2ResultStack);
            moResultStack = nearCloneImage(image, moResultStack);
            r2ResultStack = nearCloneImage(image, r2ResultStack);
    		boResultStack = nearCloneImage(image, boResultStack);
    		
    		FA = new double[Nfa_phase0 + Nfa_phase180];
    		scaledFA = new double[Nfa_phase0 + Nfa_phase180];
    		phaseIncrements = new double[Nfa_phase0 + Nfa_phase180];
    		sina = new double[Nfa_phase0 + Nfa_phase180];
    		cosa = new double[Nfa_phase0 + Nfa_phase180];
    		
    		for (angle=0; angle<Nfa_phase0; angle++) {
    			FA[angle] = Math.toRadians(despotFA_phase0[angle]);
    			phaseIncrements[angle] = 0.00;
    		}
    		for (angle=0; angle<Nfa_phase180; angle++) {
    			FA[angle+Nfa_phase0] = Math.toRadians(despotFA_phase180[angle]);
    			phaseIncrements[angle+Nfa_phase0] = 3.14159265;
    		}
    		
    		ssfpSampleData = new double[Nfa_phase0 + Nfa_phase180];
    		
    		tr = despotTR;
    		resonancePeriod = 1000.00/tr;
    		
    		t2Values = new double[nSlices][height][width];
    		moValues = new double[nSlices][height][width];
    		boValues = new double[nSlices][height][width];
    		r2Values = new double[nSlices][height][width];
    		
    		boField = new float[nSlices][width*height];
    		moField = new float[nSlices][width*height];
    		t2Field = new float[nSlices][width*height];
    		r2Field = new float[nSlices][width*height];
    		
    		Gaussian = new double[5][5];
    		
    		// define the Gaussian kernel
    		Gaussian[0][0] = 0;
    		Gaussian[0][1] = 0;
    		Gaussian[0][2] = 1;
    		Gaussian[0][3] = 0;
    		Gaussian[0][4] = 0;
    		Gaussian[1][0] = 0;
    		Gaussian[1][1] = 2;
    		Gaussian[1][2] = 4;
    		Gaussian[1][3] = 2;
    		Gaussian[1][4] = 0;
    		Gaussian[2][0] = 1;
    		Gaussian[2][1] = 4;
    		Gaussian[2][2] = 6;
    		Gaussian[2][3] = 4;
    		Gaussian[2][4] = 1;
    		Gaussian[3][0] = 0;
    		Gaussian[3][1] = 2;
    		Gaussian[3][2] = 4;
    		Gaussian[3][3] = 2;
    		Gaussian[3][4] = 0;
    		Gaussian[0][0] = 0;
    		Gaussian[4][1] = 0;
    		Gaussian[4][2] = 1;
    		Gaussian[4][3] = 0;
    		Gaussian[4][4] = 0;
    		
    		
    		// simplex - specific parameters
    		numParams = 3;
    		numVertices = numParams+1;
    		simplex = new double[numVertices][numVertices];
    		simplexLineValues = new double[numParams];
    		simplexResiduals = new double[numVertices];
    		bestToWorst = new int[3];
    		
    		simplexCentre = new double[numParams];
    		reflection = new double[numVertices];
    		expansion = new double[numVertices];
    		contraction = new double[numVertices];
    		shrink = new double[numVertices];
    		
    		
    		numParams = 2;
    		numVertices = numParams+1;
    		twoPSimplex = new double[numVertices][numVertices];
    		twoPSimplexLineValues = new double[numParams];
    		twoPSimplexResiduals = new double[numVertices];
    		bestToWorst = new int[3];
    		
    		twoPSimplexCentre = new double[numParams];
    		twoPReflection = new double[numVertices];
    		twoPExpansion = new double[numVertices];
    		twoPContraction = new double[numVertices];
    		twoPShrink = new double[numVertices];
    
    		// Perform an initial T2 Calculation to get the rough Bo field
    		for (k=0; k<nSlices; k++) {
    			progressBar.setMessage("calculating Initial Estimates for slice: "+k+" of "+nSlices);
                progressBar.updateValue(0+(int)(((float)k+1.0)/(float)nSlices*50.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
    			// grab the ssfp pixel values from the phase = 0 data
    			for (angle=0; angle<Nfa_phase0; angle++) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// grab the ssfp pixel values from the phase = 180 data
    			for (angle=0; angle<Nfa_phase180; angle++) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// grab the T1 and B1 information
    			image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
    			
    			pixelIndex = 0;
    			for (y=0; y<height; y++) {
    				for (x=0; x<width; x++) {
    					t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
    					pixelIndex++;
    				}
    			}
    			
    			if (includeB1Map == true) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						b1PixelValues[pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// now that we have all the information, perform the calculate the initial estimates of B1, T2 and Mo pixel-wise
    			pixelIndex = 0;
    			
    			for (y=0; y<height; y++) {
    				for (x=0; x<width; x++) {
    					
    					t1 = t1PixelValues[pixelIndex];
    					
    					if (t1 > 10.00) {
    						/*
    						if (t1 > 2500) {
    							boValues[k-1][y][x] = 0.00;
    							moValues[k-1][y][x] = maxMo;
    							t2Values[k-1][y][x] = maxT2;
    							r2Values[k-1][y][x] = 1.00/maxT2;
    							
    						}
    						 
    						else {
    						 */
    						
    						// scale up (or down) the flip angles based on the calculated B1 if required
    						if (includeB1Map == true) {
    							for (p=0; p<Nfa_phase0+Nfa_phase180; p++) scaledFA[p] = FA[p]*b1PixelValues[pixelIndex];
    						}
    						else {
    							for (p=0; p<Nfa_phase0+Nfa_phase180; p++) scaledFA[p] = FA[p];
    						}
    						
    						// calculate the sina and cosa matrices
    						for (p=0; p<Nfa_phase0+Nfa_phase180; p++) {
    							sina[p] = Math.sin(scaledFA[p]);
    							cosa[p] = Math.cos(scaledFA[p]);
    						}
    						
    						// grab the SSFP values for this pixel
    						for (p=0; p<Nfa_phase0; p++) ssfpSampleData[p] = ssfpPixelValues_phase0[p][pixelIndex];
    						for (p=0; p<Nfa_phase180; p++) ssfpSampleData[p+Nfa_phase0] = ssfpPixelValues_phase180[p][pixelIndex];
    						
    						// begin with the downhill simplex
    					
    						// calculate an initial guess for Mo, T2 and Bo
    						if (geScanner) {
    							initialGuess[0] = 10000.00;	// initial guess for mo
    							initialGuess[1] = 100.00;	// initial guess for t2
    							initialGuess[2] = 500.0;	// initial guess for off-resonance (Hz)
    						}
    						else {
    							initialGuess[0] = 1000.00;
    							initialGuess[1] = 100.00;
    							initialGuess[2] = 500.0;
    						}
    							
    						
    						threePDownHillSimplex(optimization, initialGuess, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, Nfa_phase0+Nfa_phase180);
    						
    						
    						mo = optimization[0];
    						t2 = optimization[1];
    						bo = optimization[2];
    						r2 = 0.00;
    						
    						if (mo < 0.00) mo = -1.00*mo;
    						if (t2 < 0.00) t2 = -1.00*t2;
    						if (bo < 0.00) bo = -1.00*bo;
    						
    						if (t2 > maxT2) {
    							t2 = maxT2;
    						}
    						
    						// invert to r2
    						if (t2 != 0.00) {
    							r2 = 1.00/t2;
    						}
    						else {
    							r2 = 0.00;
    						}
    						
    						
    						boValues[k][y][x] = bo;
    						moValues[k][y][x] = mo;
    						t2Values[k][y][x] = t2;
    						if (t1 > 0) r2Values[k][y][x] = 1.00/t2;
    						else r2Values[k][y][x] = 0.00;
    						//}
    					}
    					else {
    						boValues[k][y][x] = 0.00;
    						moValues[k][y][x] = 0.00;
    						t2Values[k][y][x] = 0.00;
    						r2Values[k][y][x] = 0.00;
    					}
    					
    					pixelIndex++;
    					
    				} // close y (height) loop
    			} // close x (width) loop
    		} // close the k (slice) loop
    
    
    		// now, go back through and smooth the Bo field 
    		for (k=0; k<nSlices; k++) {
    			progressBar.setMessage("smoothing B0 field on slice: "+k+" of "+nSlices);
                progressBar.updateValue(50+(int)(((float)k+1.0)/(float)nSlices*25.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
    			pixelIndex = 0;
    			for (y=0; y<height; y++) {
    				for (x=0; x<width; x++) {
    					
    					if (y>2 && y<height-2 && x>2 && x<width-2) {
    						
    						smoothedBo = 0.00;
    						for (p1=0; p1<5; p1++) {
    							for (p2=0; p2<5; p2++) smoothedBo += boValues[k][y-2+p2][x-2+p1]*Gaussian[p1][p2];
    						}
    						smoothedBo = smoothedBo / 34.00;
    						
    						boField[k][pixelIndex] = (float) smoothedBo;
    					}
    					else boField[k][pixelIndex] = (float) boValues[k][y][x];
    					
    					pixelIndex ++;
    				}
    			}
    			
    		}
    		
    		// now, go back through and calculate T2 and Mo again using the smoothed Bo value
    		for (k=0; k<nSlices; k++) {
    			progressBar.setMessage("recalculating values on slice: "+k+" of "+nSlices);
                progressBar.updateValue(75+(int)(((float)k+1.0)/(float)nSlices*15.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
    			// grab the ssfp pixel values from the phase = 0 data
    			for (angle=0; angle<Nfa_phase0; angle++) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// grab the ssfp pixel values from the phase = 180 data
    			for (angle=0; angle<Nfa_phase180; angle++) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			// grab the T1 and B1 information
    			image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
    			
    			pixelIndex = 0;
    			for (y=0; y<height; y++) {
    				for (x=0; x<width; x++) {
    					t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
    					pixelIndex++;
    				}
    			}
    			
    			if (includeB1Map == true) {
    				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
    				
    				pixelIndex = 0;
    				for (y=0; y<height; y++) {
    					for (x=0; x<width; x++) {
    						b1PixelValues[pixelIndex] = image.getDouble(x, y, k);
    						pixelIndex++;
    					}
    				}
    			}
    			
    			
    			pixelIndex = 0;
    			for (y=0; y<height; y++) {
    				for (x=0; x<width; x++) {
    					
    					if (boField[k][pixelIndex] > 0.00) {
    			
    					
    						bo = boField[k][pixelIndex];
    						t1 = t1PixelValues[pixelIndex];
    						
    								
    						// scale up (or down) the flip angles based on the calculated B1 if required
    						if (includeB1Map == true) {
    							for (p=0; p<Nfa_phase0+Nfa_phase180; p++) scaledFA[p] = FA[p]*b1PixelValues[pixelIndex];
    						}
    						else {
    							for (p=0; p<Nfa_phase0+Nfa_phase180; p++) scaledFA[p] = FA[p];
    						}
    						
    						// calculate the sina and cosa matrices
    						for (p=0; p<Nfa_phase0+Nfa_phase180; p++) {
    							sina[p] = Math.sin(scaledFA[p]);
    							cosa[p] = Math.cos(scaledFA[p]);
    						}
    						
    						// grab the SSFP values for this pixel
    						for (p=0; p<Nfa_phase0; p++) ssfpSampleData[p] = ssfpPixelValues_phase0[p][pixelIndex];
    						for (p=0; p<Nfa_phase180; p++) ssfpSampleData[p+Nfa_phase0] = ssfpPixelValues_phase180[p][pixelIndex];
    						
    						
    						twoPInitialGuess[0] = moValues[k][y][x];
    						twoPInitialGuess[1] = t2Values[k][y][x];
    						
    						twoPDownHillSimplex(twoPOptimization, twoPInitialGuess, bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, Nfa_phase0+Nfa_phase180);
    						
    						mo = twoPOptimization[0];
    						t2 = twoPOptimization[1];
    						
    						if (mo < 0.00) mo = -1.00*mo;
    						if (t2 < 0.00) t2 = -1.00*t2;
    						
    						if (t2 > maxT2) t2 = maxT2;
    						
    						if (t2 > 0.00) r2 = 1.00/t2;
    						else r2 = 0.00;
    						
    						t2Field[k][pixelIndex] = (float) t2;
    						moField[k][pixelIndex] = (float) mo;
    						r2Field[k][pixelIndex] = (float) r2;
    					}
    					else {
    						t2Field[k][pixelIndex] = (float) 0.00;
    						moField[k][pixelIndex] = (float) 0.00;
    						r2Field[k][pixelIndex] = (float) 0.00;
    					}
    					
    					pixelIndex++;
    					
    				} // close y (height) loop
    			} // close x (widht) loop
    			
    			try {
    				// add data to the final stacks
    				if (calculateT2) t2ResultStack.importData(image.getSliceSize()*k, t2Field[k], true);
    				if (calculateMo) moResultStack.importData(image.getSliceSize()*k, moField[k], true);
    				if (invertT2toR2) r2ResultStack.importData(image.getSliceSize()*k, r2Field[k], true);
    				if (calculateBo) boResultStack.importData(image.getSliceSize()*k, boField[k], true);
    			} catch(IOException e) {
    				e.printStackTrace();
    				MipavUtil.displayError("Data could not be imported into result image");
    			}// end the k (slice) loop 
    		}
    		
    		if (calculateT2) {
    			ViewJFrameImage t2ResultWindow = new ViewJFrameImage(t2ResultStack);
    			t2ResultWindow.setTitle("CalculatedT2Map_FM");
    			t2ResultWindow.setVisible(true);
    		} else if(t2ResultStack != null) {
    			t2ResultStack.disposeLocal();
    		}
    		
    		if (calculateMo) {
    			ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
    			moResultWindow.setTitle("CalculatedMoMap_FM");
    			moResultWindow.setVisible(true);
    		} else if(moResultStack != null) {
    			moResultStack.disposeLocal();
    		}
    		
    		if (invertT2toR2) {
    			ViewJFrameImage r2ResultWindow = new ViewJFrameImage(r2ResultStack);
    			r2ResultWindow.setTitle("CalculatedR2Map_FM");
    			r2ResultWindow.setVisible(true);
    		} else if(r2ResultStack != null) {
    			r2ResultStack.disposeLocal();
    		}
    		
    		
    		if (calculateBo) {
    			ViewJFrameImage boResultWindow = new ViewJFrameImage(boResultStack);
    			boResultWindow.setTitle("CalculatedOffResonanceMap_FM");
    			boResultWindow.setVisible(true);
    		} else if(boResultStack != null) {
    			boResultStack.disposeLocal();
    		}
    	}
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
