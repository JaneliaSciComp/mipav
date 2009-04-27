
import java.awt.BorderLayout;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

/** Swiss army knife for calculating the T1 relaxation time from any number of EFGRE (SPGR) images. **/
/* written: May 29, 2007 : S. Deoni
	status : in development

*/

public class PlugInDESPOT1_MIPAV implements PlugInGeneric {

    static String title = "DESPOT1 T1 Mapper";
	double despotTR = 5.00;
	double irspgrTR = 5.00;
	double irspgrKy = 96.00;
	double irspgrFA = 5.00;
	double maxT1 = 5000;
	double maxMo = 10000;
    double[] despotFA;
	double[] irspgrTr;
	double[] irspgrTI;
	
	double[] spgrData;
	double[] irspgrData;
	double scale, pointScale, scaleIncrement;
	double[] estimates, residuals;
	int[] direction;
    
    int[] spgrImageIndex;
	int[] irspgrImageIndex;
	int b1ImageIndex;
    double angleIncrement;
	int Nsa = 2;
	int Nti = 1;
	double maxAngle = 20;
	
	boolean smoothB1Field = true;
	boolean performStraightDESPOT1 = true;
	boolean performDESPOT1withPreCalculatedB1Map = false;
	boolean performDESPOT1HIFI = false;
	boolean doubleInversion = true;
	boolean singleInversion = false;
	boolean geScanner = true;
	boolean siemensScanner = false;
	boolean threeTField = true;
	boolean onefiveTField = false;
	
    boolean calculateT1 = true;
	boolean showB1Map = true;
    boolean calculateMo = false;
	boolean invertT1toR1 = false;
	
    boolean useWeights = true;
	
    boolean uniformAngleSpacing = true;
	
	boolean upperLeftCorner = true;
	boolean upperRightCorner = false;
	boolean lowerLeftCorner = false;
	boolean lowerRightCorner = false;
	
	boolean useSmartThresholding = true;
	boolean useHardThresholding = false;
	float noiseScale = (float) 4.00;
	float hardNoiseThreshold = (float) 0.00;
    
    String[] wList;
    private String[] titles;

    public void run() {
    	Enumeration<String> imageEnum = ViewUserInterface.getReference().getRegisteredImageNames();
        ArrayList<String> imageList = new ArrayList<String>();
        while(imageEnum.hasMoreElements()) {
        	imageList.add(imageEnum.nextElement());
        }
        wList = new String[imageList.size()];
        wList = imageList.toArray(wList);
        if (wList==null || wList.length<2) {
            MipavUtil.displayWarning("Need at least 2 SPGR images to use this Plugin.");
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
		
		if (useSmartThresholding) {
			if (!showSmartThresholdDialog()) return;
		}
		else {
			if (!showHardThresholdDialog()) return;
		}
		
        long start = System.currentTimeMillis();
		
		if (performDESPOT1HIFI == true) {
			if (!calculateT1UsingDESPOT1HIFI()) return;
		}
		else calculateT1UsingConventionalDESPOT1();
		
        System.out.println("That all took "+(System.currentTimeMillis()-start)/1000.0+" seconds");
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
		
		JCheckBox box1 = guiHelp.buildCheckBox("Perform Conventional DESPOT1 Processing", performStraightDESPOT1);
		JCheckBox box2 = guiHelp.buildCheckBox("Perform DESPOT1 with Pre-calculated B1 Map", performDESPOT1withPreCalculatedB1Map);
		JCheckBox box3 = guiHelp.buildCheckBox("Perform DESPOT1-HIFI Processing", performDESPOT1HIFI);
		
		panel.add(box1.getParent(), panelLayout);
		panel.add(box2.getParent(), panelLayout);
		panel.add(box3.getParent(), panelLayout);
		
		dialog.add(panel, BorderLayout.CENTER);
		dialog.add(guiHelp.buildOKCancelPanel(), BorderLayout.SOUTH);
		dialog.setLocationRelativeTo(null);
		dialog.pack();
		dialog.setModal(true);
		dialog.setVisible(true);
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
			return false;
		}
        
		performStraightDESPOT1 = box1.isSelected();
		performDESPOT1withPreCalculatedB1Map = box2.isSelected();
		performDESPOT1HIFI = box3.isSelected();
		
		
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
		JCheckBox box1 = guiHelp.buildCheckBox("Scan Performed on a GE Scanner", geScanner);
		JCheckBox box2 = guiHelp.buildCheckBox("Scan Performed on a Siemens Scanner", siemensScanner);
    	
		panel.add(field1.getParent(), panelLayout);
		panel.add(field2.getParent(), panelLayout);
		panel.add(box1.getParent(), panelLayout);
		panel.add(box2.getParent(), panelLayout);
		
		dialog.add(panel, BorderLayout.CENTER);
		dialog.add(guiHelp.buildOKCancelPanel(), BorderLayout.SOUTH);
		dialog.setLocationRelativeTo(null);
		dialog.pack();
		dialog.setModal(true);
		dialog.setVisible(true);
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
			return false;
		}
		
		Nsa = (int) Double.valueOf(field1.getText()).doubleValue();
		Nti = (int) Double.valueOf(field2.getText()).doubleValue();
	
		geScanner = box1.isSelected();
		siemensScanner = box2.isSelected();
		
		if (geScanner == true) siemensScanner = false;
		if (siemensScanner == true) geScanner = false;
		
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
		JCheckBox box1 = guiHelp.buildCheckBox("Double Inversion Regime", doubleInversion);
		JCheckBox box2 = guiHelp.buildCheckBox("Single Inversion Regime", singleInversion);
		JCheckBox box3 = guiHelp.buildCheckBox("1.5T Field Strength", onefiveTField);
		JCheckBox box4 = guiHelp.buildCheckBox("3.0T Field Strength", threeTField);
		JCheckBox box5 = guiHelp.buildCheckBox("Smooth B1 Field Prior to T1 Calculations", smoothB1Field);
		
		panel.add(field1.getParent(), panelLayout);
		panel.add(field2.getParent(), panelLayout);
		panel.add(field3.getParent(), panelLayout);
		panel.add(box1.getParent(), panelLayout);
		panel.add(box2.getParent(), panelLayout);
		panel.add(box3.getParent(), panelLayout);
		panel.add(box4.getParent(), panelLayout);
		panel.add(box5.getParent(), panelLayout);
		
		dialog.add(panel, BorderLayout.CENTER);
		dialog.add(guiHelp.buildOKCancelPanel(), BorderLayout.SOUTH);
		dialog.setLocationRelativeTo(null);
		dialog.pack();
		dialog.setModal(true);
		dialog.setVisible(true);
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
			return false;
		}
		
		for (int i=0; i<Nti; i++) {
			irspgrImageIndex[i] = comboArr[i].getSelectedIndex();
			irspgrTI[i] = Double.valueOf(fieldArr[i].getText()).doubleValue();
		}
		irspgrTR = Double.valueOf(field1.getText()).doubleValue();
		irspgrFA = Double.valueOf(field2.getText()).doubleValue();
		irspgrKy = Double.valueOf(field3.getText()).doubleValue();
		doubleInversion = box1.isSelected();
		singleInversion = box2.isSelected();
		onefiveTField = box3.isSelected();
		threeTField = box4.isSelected();
		smoothB1Field = box5.isSelected();
		if (doubleInversion == true) singleInversion = false;
		if (singleInversion == true) doubleInversion = false;
		if (onefiveTField == true) threeTField = false;
		if (threeTField == true) onefiveTField = false;
		
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
		JCheckBox box1 = guiHelp.buildCheckBox("Double Inversion Regime", doubleInversion);
		JCheckBox box2 = guiHelp.buildCheckBox("Single Inversion Regime", singleInversion);
		JCheckBox box3 = guiHelp.buildCheckBox("1.5T Field Strength", onefiveTField);
		JCheckBox box4 = guiHelp.buildCheckBox("3.0T Field Strength", threeTField);
		JCheckBox box5 = guiHelp.buildCheckBox("Smooth B1 Field Prior to T1 Calculations", smoothB1Field);
		
		panel.add(field1.getParent(), panelLayout);
		panel.add(field2.getParent(), panelLayout);
		panel.add(field3.getParent(), panelLayout);
		panel.add(box1.getParent(), panelLayout);
		panel.add(box2.getParent(), panelLayout);
		panel.add(box3.getParent(), panelLayout);
		panel.add(box4.getParent(), panelLayout);
		panel.add(box5.getParent(), panelLayout);
		
		dialog.add(panel, BorderLayout.CENTER);
		dialog.add(guiHelp.buildOKCancelPanel(), BorderLayout.SOUTH);
		dialog.setLocationRelativeTo(null);
		dialog.pack();
		dialog.setModal(true);
		dialog.setVisible(true);
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
			return false;
		}
		
		for (int i=0; i<Nti; i++) {
			irspgrImageIndex[i] = comboArr[i].getSelectedIndex();
			irspgrTI[i] = Double.valueOf(fieldArr[i].getText()).doubleValue();
		}
		irspgrTR = Double.valueOf(field1.getText()).doubleValue();
		irspgrFA = Double.valueOf(field2.getText()).doubleValue();
		irspgrKy = Double.valueOf(field3.getText()).doubleValue();
		doubleInversion = box1.isSelected();
		singleInversion = box2.isSelected();
		onefiveTField = box3.isSelected();
		threeTField = box4.isSelected();
		smoothB1Field = box5.isSelected();
		if (doubleInversion == true) singleInversion = false;
		if (singleInversion == true) doubleInversion = false;
		if (onefiveTField == true) threeTField = false;
		if (threeTField == true) onefiveTField = false;
		
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
		if (useSmartThresholding) useHardThresholding = false;
		
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
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
		
		if(guiHelp.getExitStatus().equals(ExitStatus.CANCEL)) {
			return false;
		}
		
		noiseScale = Float.valueOf(field1.getText()).floatValue();
		upperLeftCorner = box1.isSelected();
		upperRightCorner = box2.isSelected();
		lowerLeftCorner = box3.isSelected();
		lowerRightCorner = box4.isSelected();
		return true;
	}
	
   
	public boolean calculateT1UsingDESPOT1HIFI() {
		System.out.println("...prepping data..hang on");
		ModelImage image, irspgrImage;
		float[] ctable;
		double Inversion, ax, bx, cx, fax, fbx, fcx, R, C, precision, x0, x1, x2, x3, f1, f2, xmin;
		double smoothedB1;
		
		Inversion = 1+Math.cos(0.98*3.14159265/180.00);
		R = 0.61803399;
		C = 1.00-R;
		precision = 0.003;
		
		double[] fa;
		double[] scaledFA;
		double[][] spgrPixelValues;
		double[][] irspgrPixelValues;
		float[][] t1Values, moValues, r1Values, b1field;
		double[][][] b1Values;
		double[][] Gaussian;
		double[][][] smoothedB1Values;
		double sumX, sumY, sumXY, sumXX, slope, intercept, lnslope, t1, e1, mo, r1, d, a, b, b1;
		double ernstAngle, ernstSignal, collectedSignal, weight, sumWeights;
		float noiseSum, threshold;
		
		int width, height, nSlices, irspgrSlices;
		int irwidth, irheight;
		int x,y,i,j,k,angle, p, ti, p1, p2, pixelIndex, noiseIndex;
		int calculateB1 = 1;
		
		image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
		width = image.getExtents()[0];
		height = image.getExtents()[1];
		if(image.getNDims() > 2) {
			nSlices = image.getExtents()[2];
		} else {
			nSlices = 1;
		}
		
		irspgrImage = ViewUserInterface.getReference().getRegisteredImageByName(wList[irspgrImageIndex[0]]);
		if(image.getNDims() > 2) {
			irspgrSlices = irspgrImage.getExtents()[2];
		} else {
			irspgrSlices = 1;
		}
		
		// check to make sure the IR-SPGR and SPGR data have the same size
		irwidth = irspgrImage.getExtents()[0];
		irheight = irspgrImage.getExtents()[1];
		
		if (irwidth != width || irheight != height || irspgrSlices != nSlices) {
			MipavUtil.displayError("IR-SPGR and SPGR data must have the same image dimensions.");
			return false;
		}
		
		spgrPixelValues = new double[Nsa][width*height];
		irspgrPixelValues = new double[Nsa][width*height];
		if (calculateT1) t1Values = new float[irspgrSlices][width*height];
		else t1Values = new float[1][1];
		if (calculateMo) moValues = new float[irspgrSlices][width*height];
		else moValues = new float[1][1];
		if (invertT1toR1) r1Values = new float[irspgrSlices][width*height];
		else r1Values = new float[1][1];
		b1field = new float[irspgrSlices][width*height];
		b1Values = new double[irspgrSlices][height][width];
		
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
		
		ModelImage t1ResultStack = new ModelImage(image.getType(), image.getExtents(), "t1 Results");
		ModelImage moResultStack = new ModelImage(image.getType(), image.getExtents(), "mo Results");
		ModelImage r1ResultStack = new ModelImage(image.getType(), image.getExtents(), "r1 Results");
		ModelImage b1ResultStack = new ModelImage(image.getType(), image.getExtents(), "b1 Results");
		
		fa = new double[Nsa];
		scaledFA = new double[Nsa];
		for (angle=0; angle<Nsa;angle++) {
			fa[angle] = Math.toRadians(despotFA[angle]);
		}
		double irFA = Math.toRadians(irspgrFA);
		
		// start by calculaing the B1 field
		for (k=0; k<irspgrSlices; k++) {
			
			noiseSum = (float) 0.00;
			image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
			
			threshold = hardNoiseThreshold;
			if (useSmartThresholding) {
				noiseSum = (float) 0.00;
				noiseIndex = 0;
				if (upperLeftCorner) {
					for (y=20; y<30; y++) {
						for (x=20; x<30; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				if (upperRightCorner) {
					for (y=20; y<30; y++) {
						for (x=width-30; x<width-20; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				if (lowerLeftCorner) {
					for (y=height-30; y<height-20; y++) {
						for (x=20; x<30; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				if (lowerRightCorner) {
					for (y=height-30; y<height-20; y++) {
						for (x=width-30; x<width-20; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				
				threshold = (float) ( (noiseSum/noiseIndex + 1)*noiseScale );
			}
			else {
				threshold = (float) hardNoiseThreshold;
			}
			System.out.println("...calculating B1 field for slice: "+k+" of "+nSlices);
			// grab the ir-spgr pixel values
			for (ti=0; ti<Nti; ti++) {
				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[irspgrImageIndex[ti]]);				
				pixelIndex = 0;
				for (y=0; y<height; y++) {
					for (x=0; x<width; x++) {
						irspgrPixelValues[ti][pixelIndex] = image.getFloat(x, y, k);
						pixelIndex++;
					}
				}
			}
			// grab the spgr pixel values 
			for (angle=0; angle<Nsa; angle++) {
				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
				pixelIndex = 0;
				for (y=0; y<height; y++) {
					for (x=0; x<width; x++) {
						spgrPixelValues[angle][pixelIndex] = image.getFloat(x, y, k);
						pixelIndex++;
					}
				}
			}
			pixelIndex = 0;
			for (y=0; y<height; y++) {
				for (x=0; x<width; x++) {
					if (spgrPixelValues[0][pixelIndex] > (threshold)) {
						
						// group the spgr and ir-spagr data
						for (p=0; p<Nsa; p++) spgrData[p] = spgrPixelValues[p][pixelIndex];
						for (p=0; p<Nti; p++) irspgrData[p] = irspgrPixelValues[p][pixelIndex];
						
						// define the initial fitting points
						ax = 0.3; // lower bound
						cx = 1.5; // upper bound
						fax = signalResiduals(ax, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
						fcx = signalResiduals(cx, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
						
						// choose bx such that ax < bx < cx and f(bx) < f(ax) and f(bx) < f(cx)
						if (fax < fcx) bx = ax + 0.2;
						else bx = cx - 0.2;
						fbx = signalResiduals(bx, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
						
						x0 = ax;
						x3 = cx;
						if ( Math.abs(cx-bx) > Math.abs(bx-ax) ) {
							x1 = bx;
							x2 = bx + C*(cx-bx);
						}
						else {
							x2 = bx;
							x1 = bx - C*(bx-ax);
						}
						
						f1 = signalResiduals(x1, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
						f2 = signalResiduals(x2, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
						
						while ( Math.abs(x3-x0) > precision*(Math.abs(x1)+Math.abs(x2)) ) {
							if (f2 < f1) {
								x0 = x1;
								x1 = x2;
								x2 = R*x1 + C*x3;
								f1 = f2;
								f2 = signalResiduals(x2, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
							}
							else {
								x3 = x2;
								x2 = x1;
								x1 = R*x2 + C*x0;
								f2 = f1;
								f1 = signalResiduals(x1, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
							}
						}
						
						if (f1 < f2) xmin = x1;
						else xmin = x2;
						
						b1Values[k][y][x] = xmin;
					}
					pixelIndex ++;
				}
			}
		}
		
		// go back through and apply a single gaussian kernel to smooth the calculated B1 field
		if (smoothB1Field) {
			for (k=0; k<irspgrSlices; k++) {
				System.out.println("...smoothing B1 field on slice: "+k+" of "+nSlices);
				pixelIndex = 0;
				for (y=0; y<height; y++) {
					for (x=0; x<width; x++) {
						
						if (y>2 && y<height-2 && x>2 && x<width-2) {
							
							smoothedB1 = 0.00;
							for (p1=0; p1<5; p1++) {
								for (p2=0; p2<5; p2++) smoothedB1 += b1Values[k][y-2+p2][x-2+p1]*Gaussian[p1][p2];
							}
							smoothedB1 = smoothedB1 / 34.00;
							
							b1field[k][pixelIndex] = (float) smoothedB1;
						}
						else b1field[k][pixelIndex] = (float) b1Values[k][y][x];
						
						pixelIndex ++;
					}
				}
			}
		}
		else { // no smoothing option
			for (k=0; k<irspgrSlices; k++) {
				pixelIndex = 0;
				for (y=0; y<height; y++) {
					for (x=0; x<width; x++) {
						b1field[k][pixelIndex] = (float) b1Values[k][y][x];
						pixelIndex ++;
					}
				}
			}
			
		}
	
		// clear the b1Values matrix
		
		// finally, calculate the corrected T1 estimates, changed slice start 
		for (k=0; k<irspgrSlices; k++) {
			
			// first, recalculate the noise threshold
			noiseSum = (float) 0.00;
			image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
			
			threshold = hardNoiseThreshold;
			if (useSmartThresholding) {
				noiseSum = (float) 0.00;
				noiseIndex = 0;
				if (upperLeftCorner) {
					for (y=20; y<30; y++) {
						for (x=20; x<30; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				if (upperRightCorner) {
					for (y=20; y<30; y++) {
						for (x=width-30; x<width-20; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				if (lowerLeftCorner) {
					for (y=height-30; y<height-20; y++) {
						for (x=20; x<30; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				if (lowerRightCorner) {
					for (y=height-30; y<height-20; y++) {
						for (x=width-30; x<width-20; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				
				threshold = (float) ( (noiseSum/noiseIndex + 1)*noiseScale );
			}
			else {
				threshold = (float) hardNoiseThreshold;
			}
			
			
			System.out.println("...calculating T1 values on slice: "+k+" of "+nSlices);
			// grab the spgr pixel values 
			for (angle=0; angle<Nsa; angle++) {
				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
				
				pixelIndex = 0;
				for (y=0; y<height; y++) {
					for (x=0; x<width; x++) {
						spgrPixelValues[angle][pixelIndex] = image.getFloat(x, y, k);
						pixelIndex++;
					}
				}
			}
			
			pixelIndex = 0;
			//for (x=0; x<width; x++) {
			//	for (y=0; y<height; y++) {
			for (y=0; y<height; y++) {
				for (x=0; x<width; x++) {
					
					
					if (b1field[k][pixelIndex] > 0.00 && spgrPixelValues[0][pixelIndex] > threshold) {
						
						// scale up (or down) the flip angles based on the calculated B1
						for (p=0; p<Nsa; p++) scaledFA[p] = fa[p]*b1field[k][pixelIndex];
						
						// grab the SPGR values for this pixel
						for (p=0; p<Nsa; p++) spgrData[p] = spgrPixelValues[p][pixelIndex];
						
						// calculate T1
						sumX = 0.00;
						sumY = 0.00;
						sumXY = 0.00;
						sumXX = 0.00;
					
						for (p=0; p<Nsa; p++) {
							sumX += spgrData[p]/Math.tan(scaledFA[p]);
							sumY += spgrData[p]/Math.sin(scaledFA[p]);
							sumXY += (spgrData[p]/Math.tan(scaledFA[p]))*(spgrData[p]/Math.sin(scaledFA[p]));
							sumXX += (spgrData[p]/Math.tan(scaledFA[p]))*(spgrData[p]/Math.tan(scaledFA[p]));
						}
						
						d = (Nsa*sumXX) - sumX*sumX;
						a = (Nsa*sumXY) - (sumX*sumY);
						
						if (d != 0) {
							slope = a/d;
							intercept = (sumY-slope*sumX)/Nsa;
							lnslope = -1.00*Math.log(slope);
							if (lnslope > 0.00 && lnslope < 1.00) {
								t1 = despotTR/lnslope;
								mo = intercept/(1.00-Math.exp(-despotTR/t1));
							}
							else {
								mo = maxMo;
								t1 = maxT1;
							}
						}
						else {
							mo = maxMo;
							t1 = maxT1;
						}
						

						if (t1 < 0.00 || t1 > maxT1) {
							t1 = maxT1;
						}
						if (mo < 0.00 || mo > maxMo) {
							mo = maxMo;
						}
						
					
						if (t1 != 0.00) {
							r1 = 1.00/t1;
						}
						else {
							r1 = 0.00;
						}
					
						// if they wish to use weights, re-calculate using our non-linear weighting scheme
						if (useWeights || Nsa > 3) {
							sumX = 0.00;
							sumY = 0.00;
							sumXY = 0.00;
							sumXX = 0.00;
							sumWeights = 0.00;
							if (t1 == 0) {
								t1 = 0;
								mo = 0;
								r1 = 0;
							}
							else {
								e1 = Math.exp(-1.00*despotTR/t1);
								ernstAngle = Math.acos(e1);
								ernstSignal = (1.00-e1)*Math.sin(ernstAngle)/(1.00-e1*Math.cos(ernstAngle));
								for (angle=0; angle<Nsa; angle++) {
									collectedSignal = (1.00-e1)*Math.sin(scaledFA[angle])/(1.00-e1*Math.cos(scaledFA[angle]));
									weight = collectedSignal/ernstSignal;
									sumX+=weight * spgrPixelValues[angle][pixelIndex]/Math.tan(scaledFA[angle]);
									sumY+=weight * spgrPixelValues[angle][pixelIndex]/Math.sin(scaledFA[angle]);
									sumXY+=weight * spgrPixelValues[angle][pixelIndex]/Math.tan(scaledFA[angle]) * spgrPixelValues[angle][pixelIndex]/Math.sin(scaledFA[angle]);
									sumXX+=weight* spgrPixelValues[angle][pixelIndex]/Math.tan(scaledFA[angle]) * spgrPixelValues[angle][pixelIndex]/Math.tan(scaledFA[angle]);
									sumWeights+=weight;
								}
								d = (sumWeights*sumXX) - sumX*sumX;
								a = (sumWeights*sumXY)-(sumX*sumY);
								
								if (d != 0) {
									slope = a/d;
									intercept = (sumY - slope*sumX)/sumWeights;
									lnslope = -1.00*Math.log(slope);
									if (lnslope > 0.00 && lnslope < 1.00) {
										t1 = despotTR/lnslope;
										mo = intercept/(1.00-Math.exp(-despotTR/t1));
									}
									else {
										mo = maxMo;
										t1 = maxT1;
									}
								}
								else {
									mo = maxMo;
									t1 = maxT1;
								}
								
								
								if (t1 < 0.00 || t1 > maxT1) {
									t1 = maxT1;
								} 
								if (mo < 0.00 || mo > maxMo) {
									mo = maxMo;
								}
								if (t1 != 0.00) {
									r1 = 1.00/t1;
								}
								else {
									r1 = 0.00;
								}
							}
						}
						if (calculateT1) t1Values[k][pixelIndex] = (float) t1;
						if (calculateMo) moValues[k][pixelIndex] = (float) mo;
						if (invertT1toR1) r1Values[k][pixelIndex] = (float) r1;
					}
					else {
						if (calculateT1) t1Values[k][pixelIndex] = 0;
						if (calculateMo) moValues[k][pixelIndex] = 0;
						if (invertT1toR1) r1Values[k][pixelIndex] = 0;
					}
					pixelIndex++;
				}
			}
			
			//adds data to the end of an image
			try {
				if (calculateT1) t1ResultStack.importData(image.getSliceSize()*k, t1Values[k], true);
				if (calculateMo) moResultStack.importData(image.getSliceSize()*k, moValues[k], true);
				if (invertT1toR1) r1ResultStack.importData(image.getSliceSize()*k, r1Values[k], true);
				if (showB1Map) b1ResultStack.importData(image.getSliceSize()*k, b1field[k], true);
			} catch(IOException e) {
				e.printStackTrace();
				MipavUtil.displayError("Data could not be imported into result image");
			}
		}
		
		if (calculateT1) {
			ViewJFrameImage t1ResultWindow = new ViewJFrameImage(t1ResultStack);
			t1ResultWindow.setTitle("DESPOT1-HIFI_T1_Map");
			t1ResultWindow.setVisible(true);
		}
		
		if (calculateMo) {
			ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
			moResultWindow.setTitle("DESPOT1-HIFI_Mo_Map");
			moResultWindow.setVisible(true);
		}
		
		if (invertT1toR1) {
			ViewJFrameImage r1ResultWindow = new ViewJFrameImage(r1ResultStack);
			r1ResultWindow.setTitle("DESPOT1-HIFI_R1_Map");
			r1ResultWindow.setVisible(true);
		}
		
		
		if (showB1Map) {
			ViewJFrameImage b1ResultWindow = new ViewJFrameImage(b1ResultStack);
			b1ResultWindow.setTitle("DESPOT1-HIFI_B1_Map");
			b1ResultWindow.setVisible(true);
		}
		
		return true;
		
	}
	
	
	public boolean calculateT1UsingConventionalDESPOT1() {
		ModelImage image, b1FieldImage = null;
		float[] ctable, b1ctable;
		 
		double b1;
		double[] fa, b1Values;
		double[][] pixelValues;
		float[][] t1Values, moValues, r1Values;
		double sumX, sumY, sumXY, sumXX, slope, intercept, lnslope, t1, e1, mo, r1, d, a, b;
		double ernstAngle, ernstSignal, collectedSignal, weight, sumWeights;
		float noiseSum, threshold;
		
		int width, height, nSlices;
		int x,y,i,j,k,angle, pixelIndex, noiseIndex;
		
		image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
		width = image.getExtents()[0];
		height = image.getExtents()[1];
		if(image.getNDims() > 2) {
			nSlices = image.getExtents()[2];
		} else {
			nSlices = 1;
		}
		
		pixelValues = new double[Nsa][width*height];
		t1Values = new float[nSlices][width*height];
		moValues = new float[nSlices][width*height];
		r1Values = new float[nSlices][width*height];
		
		ModelImage t1ResultStack = new ModelImage(image.getType(), image.getExtents(), "t1 Results");
		ModelImage moResultStack = new ModelImage(image.getType(), image.getExtents(), "mo Results");
		ModelImage r1ResultStack = new ModelImage(image.getType(), image.getExtents(), "r1 Results");
		
		fa = new double[Nsa];
		for (angle=0; angle<Nsa;angle++) {
			fa[angle] = Math.toRadians(despotFA[angle]);
		}
		
		for (k=0; k<nSlices; k++) { //changed slice size
			noiseSum = (float) 0.00;
			image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
			
			threshold = hardNoiseThreshold;
			if (useSmartThresholding) {
				noiseSum = (float) 0.00;
				noiseIndex = 0;
				if (upperLeftCorner) {
					for (y=20; y<30; y++) {
						for (x=20; x<30; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				if (upperRightCorner) {
					for (y=20; y<30; y++) {
						for (x=width-30; x<width-20; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				if (lowerLeftCorner) {
					for (y=height-30; y<height-20; y++) {
						for (x=20; x<30; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				if (lowerRightCorner) {
					for (y=height-30; y<height-20; y++) {
						for (x=width-30; x<width-20; x++) {
							noiseSum += image.getFloat(x, y, k);
							noiseIndex++;
						}
					}
				}
				
				threshold = (float) ( (noiseSum/noiseIndex)*noiseScale );
			}
			else {
				threshold = (float) hardNoiseThreshold;
			}
			System.out.println("...working on slice: "+k+" of "+nSlices+". Noise Threshold = "+threshold);
			
			
			if (performDESPOT1withPreCalculatedB1Map) {
				b1FieldImage = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
				
				b1Values = new double[width*height]; 
			}
			else { // need to initialize something
				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
				
				b1Values = new double[1];
			}
			
			for (angle=0; angle<Nsa; angle++) {
				image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
				
				pixelIndex = 0;
				for (y=0; y<height; y++) {
					for (x=0; x<width; x++) {
						pixelValues[angle][pixelIndex] = image.getDouble(x, y, k);
						if (performDESPOT1withPreCalculatedB1Map) b1Values[pixelIndex] = b1FieldImage.getDouble(x, y, k);
						pixelIndex++;
					}
				}
			}
			
			for (pixelIndex=0; pixelIndex<width*height; pixelIndex++) {
				if (pixelValues[0][pixelIndex] > (threshold)) {
					sumX = 0.00;
					sumY = 0.00;
					sumXY = 0.00;
					sumXX = 0.00;
					
					if (performDESPOT1withPreCalculatedB1Map) {
						b1 = b1Values[pixelIndex];
						if (b1 == 0) b1 = 1.00;
					}
					else b1 = 1.00;
					
					for (angle=0; angle<Nsa; angle++) {
						sumX+=(pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]));
						sumY+=(pixelValues[angle][pixelIndex]/Math.sin(b1*fa[angle]));
						sumXY+=(pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle])) * (pixelValues[angle][pixelIndex]/Math.sin(b1*fa[angle]));
						sumXX+=(pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle])) * (pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]));
					}
					
					d = (Nsa*sumXX) - (sumX*sumX);
					a = (Nsa*sumXY) - (sumX*sumY);
					
					if (d != 0) {
						slope = a/d;
						intercept = (sumY-slope*sumX)/Nsa;
						lnslope = -1.00*Math.log(slope);
						if (lnslope > 0.00 && lnslope < 1.00) {
							t1 = despotTR/lnslope;
							mo = intercept/(1.00-Math.exp(-despotTR/t1));
						} 
						else {
							mo = maxMo;
							t1 = maxT1;
						}
					}
					else {
						mo = maxMo;
						t1 = maxT1;
					}
					
					if (t1 < 0 || t1 > maxT1) {
						t1 = maxT1;
					}
					if (mo < 0 || mo > maxMo) {
						mo = maxMo;
					}
					if (t1 != 0) {
						r1 = 1/t1;
					}
					else {
						r1 = 0;
					}
					
					if (useWeights) {
						sumX = 0.00;
						sumY = 0.00;
						sumXY = 0.00;
						sumXX = 0.00;
						sumWeights = 0.00;
						if (t1 == 0) {
							t1 = 0;
							mo = 0;
							r1 = 0;
						}
						else {
							e1 = Math.exp(-1.00*despotTR/t1);
							ernstAngle = Math.acos(e1);
							ernstSignal = (1.00-e1)*Math.sin(ernstAngle)/(1.00-e1*Math.cos(ernstAngle));
							for (angle=0; angle<Nsa; angle++) {
								collectedSignal = (1.00-e1)*Math.sin(b1*fa[angle])/(1.00-e1*Math.cos(b1*fa[angle]));
								weight = collectedSignal/ernstSignal;
								sumX+=weight * pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]);
								sumY+=weight * pixelValues[angle][pixelIndex]/Math.sin(b1*fa[angle]);
								sumXY+=weight * pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]) * pixelValues[angle][pixelIndex]/Math.sin(b1*fa[angle]);
								sumXX+=weight* pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]) * pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]);
								sumWeights+=weight;
							}
							d = (sumWeights*sumXX) - (sumX*sumX);
							a = (sumWeights*sumXY)-(sumX*sumY);
							
							if (d != 0) {
								slope = a/d;
								intercept = (sumY - slope*sumX)/sumWeights;
								lnslope = -1.00*Math.log(slope);
								if (lnslope > 0.00 && lnslope < 1.00) {
									t1 = despotTR/lnslope;
									mo = intercept/(1.00-Math.exp(-despotTR/t1));
								}
								else {
									mo = maxMo;
									t1 = maxT1;
								}
							}
							else {
								mo = maxMo;
								t1 = maxT1;
							}
							
							if (t1 < 0 || t1 > maxT1) {
								t1 = maxT1;
							} 
							if (mo < 0 || mo > maxMo) {
								mo = maxMo;
							}
							if (t1 != 0) {
								r1 = 1/t1;
							}
							else {
								r1 = 0;
							}
						}
					}
					
					t1Values[k][pixelIndex] = (float) t1;
					moValues[k][pixelIndex] = (float) mo;
					r1Values[k][pixelIndex] = (float) r1;
				}
				else {
					t1Values[k][pixelIndex] = 0;
					moValues[k][pixelIndex] = 0;
					r1Values[k][pixelIndex] = 0;
				}
			}
			
			try {
				t1ResultStack.importData(image.getSliceSize()*k, t1Values[k], true);
				moResultStack.importData(image.getSliceSize()*k, moValues[k], true);
				r1ResultStack.importData(image.getSliceSize()*k, r1Values[k], true);
			} catch (IOException e) {
				e.printStackTrace();
				MipavUtil.displayError("Could not import result image data.");
			}
		}
		
		if (calculateT1) {
			ViewJFrameImage t1ResultWindow = new ViewJFrameImage(t1ResultStack);
			t1ResultWindow.setTitle("DESPOT1_T1_Map");
			t1ResultWindow.setVisible(true);
		}
		
		if (calculateMo) {
			ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
			moResultWindow.setTitle("DESPOT1_Mo_Map");
			moResultWindow.setVisible(true);
		}
		
		if (invertT1toR1) {
			ViewJFrameImage r1ResultWindow = new ViewJFrameImage(r1ResultStack);
			r1ResultWindow.setTitle("DESPOT1_R1_Map");
			r1ResultWindow.setVisible(true);
		}
		
		return true;
	}
	
    public String itos(int num) {
    	String str = new Integer(num).toString();
	return str;
    }
	
	public double signalResiduals(double x, double[] spgrData, double[] irspgrData, double Inversion, int Nfa, int Nti, double[] despotFA, double despotTR, double[] irspgrTr, double[] irspgrTI, double irspgrFA) {
		
		double sumX, sumY, sumXY, sumXX, slope, intercept, residuals;
		double t1Guess, moGuess;
		double[] despotGuess, irspgrGuess, guess;
		
		double MoScale;
		
		int p, pulse, i,j;
		
		
		despotGuess = new double[Nfa];
		irspgrGuess = new double[Nti];
		
		sumX = 0.00;
		sumY = 0.00;
		sumXY = 0.00;
		sumXX = 0.00;
		for (p=0; p<Nfa; p++) {
			sumX += spgrData[p]/Math.tan(x*despotFA[p]*3.14159265/180.00);
			sumY += spgrData[p]/Math.sin(x*despotFA[p]*3.14159265/180.00);
			sumXY += spgrData[p]/Math.tan(x*despotFA[p]*3.14159265/180.00) * spgrData[p]/Math.sin(x*despotFA[p]*3.14159265/180.00);
			sumXX += spgrData[p]/Math.tan(x*despotFA[p]*3.14159265/180.00) * spgrData[p]/Math.tan(x*despotFA[p]*3.14159265/180.00);
		}
		slope = (Nfa*sumXY - sumX*sumY) / (Nfa*sumXX - sumX*sumX);
		intercept = (sumY - slope*sumX)/Nfa;
		
		if (slope > 0.00 && slope < 1.00) {
			t1Guess = -despotTR/Math.log(slope);
			moGuess = intercept/(1.00-slope);
		}
		else {
			t1Guess = 0.00;
			moGuess = 0.00;
		}
		
		
		if (t1Guess > 0) {
			for (p=0; p<Nfa; p++) despotGuess[p] = moGuess*(1.00-Math.exp(-despotTR/t1Guess))*Math.sin(x*despotFA[p]*3.14159265/180.00)/(1.00-Math.exp(-despotTR/t1Guess)*Math.cos(x*despotFA[p]*3.14159265/180.00));
		}
		else {
			for (p=0; p<Nfa; p++) despotGuess[p] = 0.00;
		}
		
		if (t1Guess > 0) {
			
			MoScale = 0.975;
			
			for (p=0; p<Nti; p++) irspgrGuess[p] = Math.abs( MoScale*moGuess*Math.sin(x*irspgrFA*3.14159265/180.00) * (1.00-Inversion*Math.exp(-irspgrTI[p]/t1Guess) + Math.exp(-irspgrTr[p]/t1Guess)) );
		}
		else {
			for (p=0; p<Nti; p++) irspgrGuess[p] = 0.00;
		}
		
		
		residuals = 0.00;
		for (p=0; p<Nfa; p++) residuals += Math.pow( (spgrData[p]-despotGuess[p]), 2.00);
		for (p=0; p<Nti; p++) residuals += Math.pow( (irspgrData[p]-irspgrGuess[p]), 2.00);
		
		return residuals;
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
		
		public JCheckBox buildCheckBox(String label, boolean selected) {
			JPanel checkPanel = new JPanel();
			JCheckBox checkBox = new JCheckBox(label);
			checkBox.setSelected(selected);
			checkPanel.add(checkBox);
			return checkBox;
		}
		
		public JTextField buildField(String labelText, String initText) {
			JPanel panel = new JPanel();
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
			JPanel panel = new JPanel();
			JLabel label = new JLabel(labelText);
			JComboBox comboBox = new JComboBox(options);
			panel.add(label);
			panel.add(comboBox);
			return comboBox;
		}
		
		public JComboBox buildComboBox(String labelText, Object[] options, int numDefault) {
			JComboBox comboBox = buildComboBox(labelText, options);
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
