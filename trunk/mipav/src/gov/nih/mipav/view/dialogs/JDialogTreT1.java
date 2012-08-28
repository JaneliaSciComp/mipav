package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
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
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmTreT1;
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
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewUserInterface;

public class JDialogTreT1 extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, AlgorithmTreParams {

    private static String title = "TRE T1 Mapper";
    
    private static int dialogWidthAdd = 0;
    
    private double treTR = 5.00;
    private double irspgrTR = 5.00;
    private double irspgrKy = 96.00;
    private double irspgrFA = 5.00;
    private double maxT1 = 5000;
    private double maxM0 = 10000;
    private double[] treFA;
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
    /**The following GUI choices change algorithm operation using enums*/
    private FieldStrength mriStrength;
	private ScannerType scannerType;
	private Threshold thresholdMethod;
	private InversionType inversionType;
    
    /**The list of possible maps that can be calculated*/
    private boolean calculateT1 = true;
    private boolean showB1Map = false;
    private boolean calculateM0 = false;
    private boolean invertT1toR1 = false;
    
    private boolean useWeights = true;
    
    private boolean uniformAngleSpacing = true;
    
    /**Whether, during "smart-thresholding", noise should be calculated from a given corner */
    private boolean upperLeftCorner = true;
    private boolean upperRightCorner = false;
    private boolean lowerLeftCorner = false;
    private boolean lowerRightCorner = false;
    
    private float noiseScale = (float) 4.00;
    private float hardNoiseThreshold = (float) 0.00;
    
    /**The ordered list of images that the algorithm is dependent on. */
    private String[] wList;
    
    /**The GUI list of images */
    private String[] titles;

    private AlgorithmTreT1 cAlgo;
	private JRadioButton doConvTre;
	private JRadioButton doHifiTre;
	private GuiBuilder guiBuilder;
	private JPanel hifiPanel;
	private JPanel straightPanel;
	private JScrollPane spgrPanel;
	private JPanel convSpec;
	private JPanel hifiSpec;
	private JScrollPane treLong;
	private JComboBox[] spgrImageComboBoxAr;
	private JTextField[] flipAngleAr;
	private JTextField spgrRepTime;
	private JTextField spgrNumFA;
	private JTextField irspgrNum;
	private JRadioButton isGEButton;
	private JRadioButton isSiemensButton;
	private JTextField irspgrTRField;
	private JTextField irspgrFAField;
	private JTextField numSlicesField;
	private JRadioButton doubleInvRadio;
	private JRadioButton singleInvRadio;
	private JRadioButton t15Radio;
	private JRadioButton t30Radio;
	private JCheckBox smoothB1Box;
	private JComboBox[] irspgrCombo;
	private JTextField[] irspgrField;
	private ButtonGroup inversionGroup;
	private ButtonGroup fieldStrengthGroup;
	private JCheckBox leastSquaresCheck;
	private JTextField convRepTime;
	private JTextField[] convFAFieldAr;
	private JComboBox[] convimageComboAr;
	private JComboBox b1Field;
	private JTextField maxT1Field;
	private JTextField maxM0Field;
	private JCheckBox showT1Map;
	private JCheckBox showM0Map;
	private JCheckBox showR1Map;
	private JRadioButton smartCheckBox;
	private JRadioButton hardCheckBox;
	private JCheckBox showB1Check;
	private JTextField hardNoiseField;
	private JTextField smartNoiseField;
	private JCheckBox topLeftBox;
	private JCheckBox topRightBox;
	private JCheckBox bottomLeftBox;
	private JCheckBox bottomRightBox;
	private ButtonGroup thresholdGroup;
	private JPanel totalThreshold;
	private JPanel generalThresholdPanel;
	private JTabbedPane tab;
	private JPanel irspgrGeneralPanel;
	//private JButton ok;
	//private JButton cancel;
	private JRadioButton noCheckBox;
	
	private JScrollPane irspgrPanel;

	private boolean performTreT1withPreCalculatedB1Map;

	private boolean performStraightTreT1;

	private boolean performTreT1HIFI;

	private JCheckBox useB1Map;
	private static final String SUCCESS = "Successful";
	
    /**
     * Blank constructor needed for dynamic instantiation.
     */
    public JDialogTreT1() { 
    	initParams();
    }

    /**
     * Construct the TRE dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogTreT1(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        initParams();
        run();
    }
    
    private void initParams() {
    	thresholdMethod = Threshold.NONE;
    	scannerType = ScannerType.GE;
    	mriStrength = FieldStrength.mri3T;
    	inversionType = InversionType.DOUBLE;
    	performStraightTreT1 = true;
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof AlgorithmTreT1) {
            Preferences.debug("TreT1: " + algorithm.getElapsedTime());
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

        //System.out.println("Pressed: "+command);
        
        if(command.equals("OK")) {
        	
    		String userError;
    		if((userError = setUI(true)).equals(SUCCESS)) {
    			if(validateUI()) {
    				callAlgorithm();
    			} else {
    				MipavUtil.displayError("Validation check failed, please check console output.");
    				this.setVisible(true);
    			}
    		} else {
    			MipavUtil.displayError(userError);
    			this.setVisible(true);
    		}
        } else if (command.equals("Cancel")) {
           if(cAlgo != null) {
        	   cAlgo.interrupt();
           } else {
        	   this.dispose();
           }
        }  else {
            super.actionPerformed(event);
        }
    }

    protected JPanel buildHIFIPanel() {
	    JPanel panel = new JPanel();
	    LayoutManager panelLayout = new GridBagLayout();
	    GridBagConstraints gbc = new GridBagConstraints();
	    panel.setBorder(MipavUtil.buildTitledBorder("HIFI information"));
	    panel.setLayout(panelLayout);
	    
	    spgrNumFA = guiBuilder.buildDecimalField("Number of SPGR Flip Angles:", Nsa);
	    irspgrNum = guiBuilder.buildDecimalField("Number of IR-SPGR TI Times:", Nti);
	    isGEButton = guiBuilder.buildRadioButton("Scan Performed on a GE Scanner", scannerType.equals(ScannerType.GE));
	    isSiemensButton = guiBuilder.buildRadioButton("Scan Performed on a Siemens Scanner", scannerType.equals(ScannerType.SIEMENS));
	    
	    ActionListener c = new ScannerChoiceListener();
	    isGEButton.addActionListener(c);
	    isSiemensButton.addActionListener(c);
	    
	    ButtonGroup scannerType = new ButtonGroup();
	    scannerType.add(isGEButton);
	    scannerType.add(isSiemensButton);
	    
	    //guiHelp envelopes elements in JPanels, so need to add parent
	    gbc.gridx = 0;
	    gbc.gridy = 0;
	    gbc.weighty = 0;
	    gbc.weightx = 1;
	    gbc.anchor = GridBagConstraints.WEST;
	    panel.add(spgrNumFA.getParent(), gbc);
	    gbc.gridy++;
	    panel.add(irspgrNum.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(isGEButton.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(isSiemensButton.getParent(), gbc);
	    
	 	//TODO: Remove dummy label for display
	    gbc.gridy++;
	    gbc.weighty = 1;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(new JLabel(""), gbc);
	    
	    return panel;
	}

	protected JPanel buildConventionalTreT1Panel() {
	    JPanel panel = new JPanel();
	    panel.setBorder(MipavUtil.buildTitledBorder("treT1: General Information"));
	    LayoutManager panelLayout = new GridBagLayout();
	    GridBagConstraints gbc = new GridBagConstraints();
	    panel.setLayout(panelLayout);
	    
	    spgrNumFA = guiBuilder.buildDecimalField("Number of SPGR Flip Angles:", Nsa);
	    useB1Map = guiBuilder.buildCheckBox("<html>Use B<sub>1</sub> Map</html>", performTreT1withPreCalculatedB1Map);
	    useB1Map.addActionListener(new ProcessChoiceListener());
	    
	    gbc.gridx = 0;
	    gbc.gridy = 0;
	    gbc.weightx = 1;
	    gbc.anchor = GridBagConstraints.WEST;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(spgrNumFA.getParent(), gbc);
	    
	    gbc.gridy++;
	    panel.add(useB1Map.getParent(), gbc);
	    
	    gbc.gridy++;
	    gbc.weighty = 1;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(new JLabel(""), gbc);
	    
	    return panel;
	}

	protected JScrollPane buildSPGRPanel() {
	    JPanel panel = buildSPGRPanelInner();
	    
	    JScrollPane scrollPane = new JScrollPane(panel);
	    scrollPane.setPreferredSize(new Dimension(420+dialogWidthAdd,405));
	    scrollPane.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: SPGR Image Information"));
	    scrollPane.addComponentListener(new FlipAngleChangeListener());
	
	    return scrollPane;
	 }
	
	private JPanel buildSPGRPanelInner() {
		JPanel panel = new JPanel();
	    LayoutManager panelLayout = new GridBagLayout();
	    GridBagConstraints gbc = new GridBagConstraints();
	    panel.setLayout(panelLayout);
	    
	    gbc.fill = GridBagConstraints.NONE;
	    gbc.anchor = GridBagConstraints.WEST;
	    gbc.gridy = 0;
	    gbc.weighty = 0;
	    gbc.weightx = 1;
	    
	    spgrImageComboBoxAr = new JComboBox[Nsa];
	    flipAngleAr = new JTextField[Nsa];
	    for (int i=0; i<Nsa; i++) {
	        spgrImageComboBoxAr[i] = guiBuilder.buildComboBox("SPGR Image #"+(i+1), titles, i);
	        gbc.gridy = i;
	        gbc.gridx = 0;
	        gbc.weightx = .9;
	        panel.add(spgrImageComboBoxAr[i].getParent(), gbc);
	       
	        double tiAdd = treFA != null && treFA.length > i ? treFA[i] : 0.0;
	        flipAngleAr[i] = guiBuilder.buildDecimalField("SPGR Flip Angle #"+(i+1), tiAdd);
	        gbc.gridx = 1;
	        gbc.weightx = .1;
	        panel.add(flipAngleAr[i].getParent(), gbc);
	    }
	    
	    spgrRepTime = guiBuilder.buildDecimalField("SPGR Repetition Time (ms):", treTR);
	    
	    gbc.weightx = 1;
	    gbc.gridy++;
	    gbc.gridwidth = 2;
	    gbc.gridx = 0;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(spgrRepTime.getParent(), gbc);
	    
	    //TODO: Remove dummy label for display
	    gbc.gridy++;
	    gbc.weighty = 1;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(new JLabel(""), gbc);
	    
	    return panel;
	}

	protected JScrollPane buildIRSPGRPanel() {
	    
		JPanel panel = null;
		if(scannerType.equals(ScannerType.GE)) {
			panel = buildIRSPGRPanelGEInner();
		} else {
			panel = buildIRSPGRPanelSiemensInner();
		}
		
	    JScrollPane scrollPane = new JScrollPane(panel);
	    if(scannerType.equals(ScannerType.GE)) {
	    	scrollPane.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: IR-SPGR GE Image Information"));
	    } else {
	    	scrollPane.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: IR-SPGR Siemens Image Information"));
	    }
	    scrollPane.setPreferredSize(new Dimension(420+dialogWidthAdd,405));
	    scrollPane.addComponentListener(new FlipAngleChangeListener());
	    
	    return scrollPane;
	}
	
	private JPanel buildIRSPGRPanelGEInner() {
		JPanel panel = new JPanel();
	    LayoutManager panelLayout = new GridBagLayout();
	    panel.setLayout(panelLayout);
	    GridBagConstraints gbc = new GridBagConstraints();
	    
	    gbc.fill = GridBagConstraints.NONE;
	    gbc.anchor = GridBagConstraints.WEST;
	    gbc.gridy = 0;
	    gbc.weighty = 0;
	    gbc.weightx = 1;
	    
	    irspgrCombo = new JComboBox[Nti];
	    irspgrField  = new JTextField[Nti];
	    for (int i=0; i<Nti; i++) {
	        
	    	irspgrCombo[i] = guiBuilder.buildComboBox("IR-SPGR Image #"+(i+1), titles, Nsa+i);
	    	gbc.gridy = i;
	        gbc.gridx = 0;
	        gbc.weightx = 1;
	    	panel.add(irspgrCombo[i].getParent(), gbc);
	        
	        double tiAdd = irspgrTI != null && irspgrTI.length > i ? irspgrTI[i] : 0.0;
	        irspgrField[i] = guiBuilder.buildDecimalField("IR-SPGR TI #"+(i+1), tiAdd);
	        
	        gbc.gridx = 1;
	        gbc.weightx = 0;
	        panel.add(irspgrField[i].getParent(), gbc);
	    }
	    
	    irspgrTRField = guiBuilder.buildDecimalField("IR-SPGR Repetition Time (ms)", irspgrTR);
	    irspgrFAField = guiBuilder.buildDecimalField("IR-SPGR Flip Angle", irspgrFA);
	    numSlicesField = guiBuilder.buildDecimalField("Total Number of Acquired Slices", irspgrKy);
	    doubleInvRadio = guiBuilder.buildRadioButton("Double Inversion Regime", inversionType.equals(InversionType.DOUBLE));
	    singleInvRadio = guiBuilder.buildRadioButton("Single Inversion Regime", inversionType.equals(InversionType.SINGLE));
	    t15Radio = guiBuilder.buildRadioButton("1.5T Field Strength", mriStrength.equals(FieldStrength.mri15T));
	    t30Radio = guiBuilder.buildRadioButton("3.0T Field Strength", mriStrength.equals(FieldStrength.mri3T));
	    smoothB1Box = guiBuilder.buildCheckBox("<html>Smooth B<sub>1</sub> Field Prior to T<sub>1</sub> Calculations</html>", smoothB1Field);
	    
	    inversionGroup = new ButtonGroup();
	    inversionGroup.add(doubleInvRadio);
	    inversionGroup.add(singleInvRadio);
	    
	    fieldStrengthGroup = new ButtonGroup();
	    fieldStrengthGroup.add(t15Radio);
	    fieldStrengthGroup.add(t30Radio);
	    
	    gbc.weightx = 1;
	    gbc.gridy++;
	    gbc.gridwidth = 2;
	    gbc.gridx = 0;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(irspgrTRField.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(irspgrFAField.getParent(), gbc);
	    gbc.gridy++;
	    panel.add(numSlicesField.getParent(), gbc);
	    gbc.insets = new Insets(10, 0, 0, 0);
	    gbc.gridy++;
	    panel.add(doubleInvRadio.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(singleInvRadio.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(t15Radio.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(t30Radio.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(smoothB1Box.getParent(), gbc);
	
	    //TODO: Remove dummy label for display
	    gbc.gridy++;
	    gbc.weighty = 1;
	    
	    
	    
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(new JLabel(""), gbc);
	    
	    return panel;
	}
	
	private JPanel buildIRSPGRPanelSiemensInner() {
		JPanel panel = new JPanel();
	    LayoutManager panelLayout = new GridBagLayout();
	    panel.setLayout(panelLayout);
	    GridBagConstraints gbc = new GridBagConstraints();
	    
	    gbc.fill = GridBagConstraints.NONE;
	    gbc.anchor = GridBagConstraints.WEST;
	    gbc.gridy = 0;
	    gbc.weighty = 0;
	    gbc.weightx = 1;
	    
	    irspgrCombo = new JComboBox[Nti];
	    irspgrField = new JTextField[Nti];
	    for (int i=0; i<Nti; i++) {
	        irspgrCombo[i] = guiBuilder.buildComboBox("IR-SPGR Image #"+(i+1), titles, i+2);
	        gbc.gridy = i;
	        gbc.gridx = 0;
	        gbc.weightx = .9;
	        panel.add(irspgrCombo[i].getParent(), gbc);
	        
	        double tiAdd = irspgrTI != null && irspgrTI.length > i ? irspgrTI[i] : 0.0;
	        irspgrField[i] = guiBuilder.buildDecimalField("IR-SPGR TI #"+(i+1), tiAdd);
	        
	        gbc.gridx = 1;
	        gbc.weightx = .1;
	        panel.add(irspgrField[i].getParent(), gbc);
	    }
	    
	    irspgrTRField = guiBuilder.buildDecimalField("IR-SPGR Repetition Time (ms)", irspgrTR);
	    irspgrFAField = guiBuilder.buildDecimalField("IR-SPGR Flip Angle", irspgrFA);
	    numSlicesField = guiBuilder.buildDecimalField("Total Number of Acquired Slices", irspgrKy);
	    doubleInvRadio = guiBuilder.buildRadioButton("Double Inversion Regime", inversionType.equals(InversionType.DOUBLE));
	    singleInvRadio = guiBuilder.buildRadioButton("Single Inversion Regime", inversionType.equals(InversionType.SINGLE));
	    t15Radio = guiBuilder.buildRadioButton("1.5T Field Strength", mriStrength.equals(FieldStrength.mri15T));
	    t30Radio = guiBuilder.buildRadioButton("3.0T Field Strength", mriStrength.equals(FieldStrength.mri3T));
	    smoothB1Box = guiBuilder.buildCheckBox("<html>Smooth B<sub>1</sub> Field Prior to T<sub>1</sub> Calculations</html>", smoothB1Field);
	    
	    inversionGroup = new ButtonGroup();
	    inversionGroup.add(doubleInvRadio);
	    inversionGroup.add(singleInvRadio);
	    
	    fieldStrengthGroup = new ButtonGroup();
	    fieldStrengthGroup.add(t15Radio);
	    fieldStrengthGroup.add(t30Radio);
	    
	    gbc.weightx = 1;
	    gbc.gridwidth = 2;
	    gbc.gridy++;
	    gbc.gridx = 0;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(irspgrTRField.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(irspgrFAField.getParent(), gbc);
	    gbc.gridy++;
	    panel.add(numSlicesField.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(doubleInvRadio.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(singleInvRadio.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(t15Radio.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(t30Radio.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(smoothB1Box.getParent(), gbc);
	
	    //TODO: Remove dummy label for display
	    gbc.gridy++;
	    gbc.weighty = 1;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(new JLabel(""), gbc);
	    
	    return panel;
	}

	protected JScrollPane buildTreT1LongPanel() {
	    JPanel panel = buildTreT1LongPanelInner();
	    
	    JScrollPane scrollPane = new JScrollPane(panel);
	    scrollPane.setPreferredSize(new Dimension(420+dialogWidthAdd,405));
	    scrollPane.setBorder(MipavUtil.buildTitledBorder("treT1-Conv: Long"));
	    
	    scrollPane.addComponentListener(new FlipAngleChangeListener());
	    
	    return scrollPane;
	}
	
	private class FlipAngleChangeListener implements ComponentListener {

		public void componentResized(ComponentEvent e) {}

		public void componentMoved(ComponentEvent e) {}

		public void componentShown(ComponentEvent e) {
			setFAValues();
			//System.out.println("Entered");
			if(e.getSource().equals(convSpec) && doSpec()) {
				convSpec.removeAll();
				convSpec.add(buildTreT1SpecificsPanelInner());
				
				convSpec.validate();
				convSpec.updateUI();
			} else if(e.getSource().equals(hifiSpec) && doSpec()) {
				hifiSpec.removeAll();
				hifiSpec.add(buildTreT1HIFISpecificsPanelInner());
				
				hifiSpec.validate();
				hifiSpec.updateUI();
			} else if(e.getSource().equals(spgrPanel) && doSpgr()) {
				spgrPanel.getViewport().removeAll();
				spgrPanel.setViewportView(buildSPGRPanelInner());

				spgrPanel.validate();
				spgrPanel.updateUI();
			} else if(e.getSource().equals(treLong) && doConv()) {
				treLong.getViewport().removeAll();
				treLong.setViewportView(buildTreT1LongPanelInner());

				treLong.validate();
				treLong.updateUI();
			} else if(e.getSource().equals(irspgrPanel) && doIRSPGR()) {
				irspgrPanel.getViewport().removeAll();
				
				if(scannerType.equals(ScannerType.GE)) {
					irspgrPanel.setViewportView(buildIRSPGRPanelGEInner());
					irspgrPanel.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: IR-SPGR GE Image Information"));
				} else {
					irspgrPanel.setViewportView(buildIRSPGRPanelSiemensInner());
					irspgrPanel.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: IR-SPGR Siemens Image Information"));
				}

				irspgrPanel.validate();
				irspgrPanel.updateUI();
			}
			tab.validate();
		    tab.updateUI();	
		}

		public void componentHidden(ComponentEvent e) {}
		
		private void setFAValues() {
			try {
				if(spgrNumFA != null && Nsa != Double.valueOf(spgrNumFA.getText()).intValue()) {
					Nsa = Double.valueOf(spgrNumFA.getText()).intValue();
				}
			} catch (NumberFormatException ex1) {
				//System.out.println(spgrNumFA.getText());
				MipavUtil.displayError("The number of flip angles in panel 1 is not a valid value.");
			}
			
			try {
				if(irspgrNum != null && Nti != Double.valueOf(irspgrNum.getText()).intValue()) {
					Nti = Double.valueOf(irspgrNum.getText()).intValue();
				}
			} catch (NumberFormatException ex1) {
				//System.out.println(irspgrNum.getText());
				MipavUtil.displayError("The number of irspgr images in panel 1 is not a valid value.");
			}
		}
		
		private boolean doSpec() {
			//when Nsa > 2, the leastSquares checkbox should not be null and should be visible
			//when Nsa <=2, the leastSquares check could be null, but if not, it shouldn't be visible
			return ((leastSquaresCheck == null || (leastSquaresCheck != null && !leastSquaresCheck.isVisible())) && Nsa > 2) 
						|| (Nsa <=2 && leastSquaresCheck != null && leastSquaresCheck.isVisible());
		}
		
		private boolean doSpgr() {
			//this array shouldn't be null and its length should equal Nsa
			return (spgrImageComboBoxAr == null || spgrImageComboBoxAr.length != Nsa);
		}
		
		private boolean doConv() {	
			//like spgr this array shouldn't be null and its length should equal Nsa
			return (convimageComboAr == null || convimageComboAr.length != Nsa);
		}
		
		private boolean doIRSPGR() {
			//for both he and siemens, this array shouldn't be null and its length should equal Nti
			return (irspgrCombo == null || irspgrCombo.length != Nti);
		}
	}
	
	private JPanel buildTreT1LongPanelInner() {
		JPanel panel = new JPanel();
	    LayoutManager panelLayout = new GridBagLayout();
	    panel.setLayout(panelLayout);
	    GridBagConstraints gbc = new GridBagConstraints();
	    
	    gbc.fill = GridBagConstraints.NONE;
	    gbc.anchor = GridBagConstraints.WEST;
	    gbc.gridy = 0;
	    gbc.weighty = 0;
	    gbc.weightx = 1;
	    
	    convimageComboAr = new JComboBox[Nsa];
	    convFAFieldAr = new JTextField[Nsa];
	    for(int i=0; i<Nsa; i++) {
	        convimageComboAr[i] = guiBuilder.buildComboBox("Image #"+(i+1), titles, i);
	        gbc.gridy = i;
	        gbc.gridx = 0;
	        gbc.weightx = .9;
	        panel.add(convimageComboAr[i].getParent(), gbc);
	        
	        double faAdd = treFA != null && treFA.length > i ? treFA[i] : 0.0;
	        convFAFieldAr[i] = guiBuilder.buildDecimalField("Flip Angle #"+(i+1), faAdd);
	        
	        gbc.gridx = 1;
	        gbc.weightx = .1;
	        panel.add(convFAFieldAr[i].getParent(), gbc);
	    }
	    
	    gbc.weightx = 1;
	    gbc.gridwidth = 2;
	    gbc.gridy++;
	    gbc.gridx = 0;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    convRepTime = guiBuilder.buildDecimalField("Repetition Time (ms):", treTR);
	    panel.add(convRepTime.getParent(), gbc);
	    
	    b1Field = null;
	    if(performTreT1withPreCalculatedB1Map) {
	    	gbc.gridy++;
	    	b1Field = guiBuilder.buildComboBox("<html>B<sub>1</sub> Field Map:</html>", titles, 0);
	        panel.add(b1Field.getParent(), gbc);
	    }
	    
	    //TODO: Remove dummy label for display
	    gbc.gridy++;
	    gbc.weighty = 1;
	    panel.add(new JLabel(""), gbc);
	    
	    return panel;
	}

	protected JPanel buildTreT1SpecificsPanel() {
		JPanel panel = new JPanel();
	    LayoutManager panelLayout = new BorderLayout();
	    panel.setLayout(panelLayout);
	    panel.addComponentListener(new FlipAngleChangeListener());
	    panel.setBorder(MipavUtil.buildTitledBorder("treT1: Specifics"));
	    
	    panel.add(buildTreT1SpecificsPanelInner());
	    
	    return panel;
	    
	}
	
	private JPanel buildTreT1SpecificsPanelInner() {
		JPanel panel = new JPanel();
		panel.addComponentListener(new FlipAngleChangeListener());
	    LayoutManager panelLayout = new GridBagLayout();
	    panel.setLayout(panelLayout);
	    GridBagConstraints gbc =  new GridBagConstraints();
		maxT1Field = guiBuilder.buildDecimalField("<html>Maximum Allowable T<sub>1</sub>:</html>", maxT1);
	    maxM0Field = guiBuilder.buildDecimalField("<html>Maximum Allowable M<sub>0</sub>:</html>", maxM0);
	    showT1Map = guiBuilder.buildCheckBox("<html>Show T<sub>1</sub> Map</html>", calculateT1);
	    showM0Map = guiBuilder.buildCheckBox("<html>Show M<sub>0</sub> Map</html>", calculateM0);
	    showR1Map = guiBuilder.buildCheckBox("<html>Show R<sub>1</sub> Map</html>", invertT1toR1);
	    leastSquaresCheck = null;
	    gbc.fill = GridBagConstraints.NONE;
	    gbc.anchor = GridBagConstraints.NORTHWEST;
	    gbc.gridy = 0;
	    gbc.weighty = 0;
	    gbc.weightx = 1;

	    panel.add(maxT1Field.getParent(), gbc);
	    gbc.gridy++;
	    panel.add(maxM0Field.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(10, 0, 0, 0);
	    panel.add(showT1Map.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(showM0Map.getParent(), gbc);
	    gbc.gridy++;
	    panel.add(showR1Map.getParent(), gbc);
	    
	    if(Nsa > 2) {
	        leastSquaresCheck = guiBuilder.buildCheckBox("Calculate T1 Using Weigthed Least-Squares", useWeights);
	        gbc.gridy++;
	        gbc.insets = new Insets(10, 0, 0, 0);
	        panel.add(leastSquaresCheck.getParent(), gbc);
	    }

	    gbc.gridy++;
	    gbc.weighty = 1;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(new JLabel(""), gbc);
	    
	    return panel;
	}

	protected JPanel buildThresholdPanel() {
		//System.out.println("The selected state: "+thresholdMethod);
		
		JPanel panel = new JPanel();
		panel.setName("Threshold total");
	    LayoutManager panelLayout = new GridBagLayout();
	    panel.setLayout(panelLayout);
	    GridBagConstraints gbc = new GridBagConstraints();
	    panel.setBorder(MipavUtil.buildTitledBorder("Thresholding"));
	    
	    JPanel methodPanel = new JPanel();
	    LayoutManager methodLayout = new BoxLayout(methodPanel, BoxLayout.Y_AXIS);
	    methodPanel.setLayout(methodLayout);
	    methodPanel.setBorder(MipavUtil.buildTitledBorder("Thresholding method"));
	    
	    smartCheckBox = guiBuilder.buildRadioButton("Use Smart Thresholding", thresholdMethod.equals(Threshold.SMART));
	    hardCheckBox = guiBuilder.buildRadioButton("Use Hard Thresholding", thresholdMethod.equals(Threshold.HARD));
	    noCheckBox = guiBuilder.buildRadioButton("No Thresholding", thresholdMethod.equals(Threshold.NONE));
	    
	    thresholdGroup = new ButtonGroup();
	    thresholdGroup.add(smartCheckBox);
	    thresholdGroup.add(hardCheckBox);
	    thresholdGroup.add(noCheckBox);
	    
	    if(thresholdMethod == null) {
	    	smartCheckBox.setSelected(true);
	    	thresholdMethod = Threshold.SMART;
	    }
	    
	    ActionListener c = new ThresholdChoiceListener();
	    smartCheckBox.addActionListener(c);
	    hardCheckBox.addActionListener(c);
	    noCheckBox.addActionListener(c);
	
	    methodPanel.add(smartCheckBox, methodLayout);
	    methodPanel.add(hardCheckBox, methodLayout);
	    methodPanel.add(noCheckBox, methodLayout);
	    
	    gbc.gridx = 0;
	    gbc.gridy = 0;
	    gbc.weightx = 1;
	    gbc.fill = GridBagConstraints.NONE;
	    gbc.anchor = GridBagConstraints.WEST;
	    panel.add(methodPanel, gbc);
	    
	    generalThresholdPanel = new JPanel();
	    
	    JPanel innerPanel = null;
	    
	    if(thresholdMethod.equals(Threshold.SMART)) {
	    	innerPanel = buildSmartThresholdPanel();
	    } else if(thresholdMethod.equals(Threshold.HARD)) {
	    	innerPanel = buildHardThresholdPanel();
	    } else {
	    	innerPanel = buildNoThresholdPanel();
	    	thresholdMethod = Threshold.NONE;
	    }
	    generalThresholdPanel.add(innerPanel);
	    
	    gbc.gridy = 1;
	    gbc.weighty = 0;
	    gbc.insets = new Insets(20, 0, 0, 0);
	    panel.add(generalThresholdPanel, gbc);
	    
	    gbc.gridy++;
	    gbc.weighty = 1;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(new JLabel(""), gbc);
	    
	    return panel;
	}

	protected JPanel buildTreT1HIFISpecificsPanel() {
	    JPanel panel = new JPanel();
	    LayoutManager panelLayout = new BorderLayout();
	    panel.addComponentListener(new FlipAngleChangeListener());
	    panel.setLayout(panelLayout);
	    panel.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: IR-SPGR Image Information"));
	    
	    panel.add(buildTreT1HIFISpecificsPanelInner());
	    
	    return panel;
	}
	
	private JPanel buildTreT1HIFISpecificsPanelInner() {
		JPanel panel = new JPanel();
	    LayoutManager panelLayout = new GridBagLayout();
	    panel.setLayout(panelLayout);
	    GridBagConstraints gbc =  new GridBagConstraints();
	    
	    maxT1Field = guiBuilder.buildDecimalField("<html>Maximum Allowable T<sub>1</sub>:</html>", maxT1);
	    maxM0Field = guiBuilder.buildDecimalField("<html>Maximum Allowable M<sub>0</sub>:</html>", maxM0);
	    showT1Map = guiBuilder.buildCheckBox("<html>Show T<sub>1</sub> Map</html>", calculateT1);
	    showM0Map = guiBuilder.buildCheckBox("<html>Show M<sub>0</sub> Map</html>", calculateM0);
	    showB1Check = guiBuilder.buildCheckBox("<html>Show B<sub>1</sub> Map</html>", true);
	    showR1Map = guiBuilder.buildCheckBox("<html>Show R<sub>1</sub> Map</html>", invertT1toR1);
	    leastSquaresCheck = null; 
	    if (Nsa > 2) { 
	    	leastSquaresCheck = guiBuilder.buildCheckBox("Calculate T1 Using Weighted Least-Squares", useWeights);
	    }
	    
	    gbc.fill = GridBagConstraints.NONE;
	    gbc.anchor = GridBagConstraints.WEST;
	    gbc.gridy = 0;
	    gbc.weighty = 0;
	    gbc.weightx = 1;
	    
	    panel.add(maxT1Field.getParent(), gbc);
	    gbc.gridy++;
	    panel.add(maxM0Field.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(20, 0, 0, 0);
	    panel.add(showT1Map.getParent(), gbc);
	    gbc.gridy++;
	    gbc.insets = new Insets(0, 0, 0, 0);
	    panel.add(showM0Map.getParent(), gbc);
	    gbc.gridy++;
	    panel.add(showB1Check.getParent(), gbc);
	    gbc.gridy++;
	    panel.add(showR1Map.getParent(), gbc);
	    if(Nsa > 2) {
	    	gbc.gridy++;
	    	gbc.insets = new Insets(10, 0, 0, 0);
	    	panel.add(leastSquaresCheck.getParent(), gbc);
	    }
	    //gbc.weighty = 1;
	    //gbc.insets = new Insets(0, 0, 0, 0);
	    //panel.add(new JLabel(""), gbc);
	    
	    return panel;
	}

	/**
	 * No border
	 * @return
	 */
	protected JPanel buildHardThresholdPanel() {
	    JPanel panel = new JPanel();
	    LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
	    panel.setLayout(panelLayout);
	    
	    hardNoiseField = guiBuilder.buildDecimalField("Hard Noise Level", hardNoiseThreshold);
	    
	    panel.add(hardNoiseField.getParent(), panelLayout);
	    
	    return panel;
	}

	protected JPanel buildSmartThresholdPanel() {
	    JPanel panel = new JPanel();
	    LayoutManager panelLayout = new BoxLayout(panel, BoxLayout.Y_AXIS);
	    panel.setLayout(panelLayout);
	    
	    smartNoiseField = guiBuilder.buildDecimalField("Noise Level Scale", noiseScale);
	    topLeftBox = guiBuilder.buildCheckBox("Calculate Noise from TOP LEFT Corner?", upperLeftCorner);
	    topRightBox = guiBuilder.buildCheckBox("Calculate Noise from TOP RIGHT Corner?", upperRightCorner);
	    bottomLeftBox = guiBuilder.buildCheckBox("Calculate Noise from BOTTOM LEFT Corner?", lowerLeftCorner);
	    bottomRightBox = guiBuilder.buildCheckBox("Calculate Noise from BOTTOM RIGHT Corner?", lowerRightCorner);
	    
	    panel.add(smartNoiseField.getParent(), panelLayout);
	    panel.add(topLeftBox.getParent(), panelLayout);
	    panel.add(topRightBox.getParent(), panelLayout);
	    panel.add(bottomLeftBox.getParent(), panelLayout);
	    panel.add(bottomRightBox.getParent(), panelLayout);
	
	    return panel;
	}
	
	protected JPanel buildNoThresholdPanel() {
		JPanel panel = new JPanel();
		return panel;
	}

	protected void callAlgorithm() {
        // Make algorithm
		boolean useHardThresholding = false, useSmartThresholding = false;
    	boolean doubleInversion = false, singleInversion = false;
    	boolean geScanner = false, siemensScanner = false;
    	boolean threeTField = false, onefiveTField = false;
        
        //convert enums to simple booleans
    	if(thresholdMethod.equals(Threshold.SMART)) {
    		useSmartThresholding = true;
    	} else if(thresholdMethod.equals(Threshold.HARD)) {
    		useHardThresholding = true;
    	} else {
    		useHardThresholding = true;
    		hardNoiseThreshold = 0.0f;
    	}
    	
    	if(inversionType.equals(InversionType.DOUBLE)) {
    		doubleInversion = true;
    	} else if(inversionType.equals(InversionType.SINGLE)){
    		singleInversion = true;
    	}
    	
    	if(scannerType.equals(ScannerType.GE)) {
    		geScanner = true;
    	} else {
    		siemensScanner = true;
    	}
    	
    	if(mriStrength.equals(FieldStrength.mri15T)) {
    		onefiveTField = true;
    	} else {
    		threeTField = true;
    	}
		
		cAlgo = new AlgorithmTreT1(treTR, irspgrTR,
                irspgrKy, irspgrFA, maxT1, maxM0,
                treFA,  irspgrTr,  irspgrTI,
                spgrData,  irspgrData, scale,
                pointScale, scaleIncrement,  estimates,
                residuals,  direction,  spgrImageIndex,
                irspgrImageIndex,  b1ImageIndex, angleIncrement,
                Nsa,  Nti, maxAngle,  smoothB1Field,
                performStraightTreT1,
                performTreT1withPreCalculatedB1Map,
                performTreT1HIFI,  doubleInversion,
                singleInversion,  geScanner,  siemensScanner,
                threeTField,  onefiveTField,  calculateT1,
                showB1Map,  calculateM0,  invertT1toR1,
                useWeights,  uniformAngleSpacing,
                upperLeftCorner,  upperRightCorner,
                lowerLeftCorner,  lowerRightCorner,
                useSmartThresholding,  useHardThresholding,
                noiseScale, hardNoiseThreshold, wList,
                titles);

        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed or failed. See algorithm performed event.
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
    
    protected void displayTotalDialog() {
	    GridBagLayout gb = new GridBagLayout();
	    setLayout(gb);
	    GridBagConstraints gbc = new GridBagConstraints();
	    guiBuilder = new GuiBuilder(this);
	    setTitle("treT1: Dialog");
	    setDialogWidthAdd();
	    setMinimumSize(new Dimension(450+dialogWidthAdd, 600));
	    setPreferredSize(new Dimension(450+dialogWidthAdd, 600));
	    setMaximumSize(new Dimension(800+dialogWidthAdd, 1200));
	    
	    JPanel methodPanel = new JPanel();
	    LayoutManager panelLayout = new BoxLayout(methodPanel, BoxLayout.X_AXIS);
	    methodPanel.setLayout(panelLayout);
	    methodPanel.setBorder(MipavUtil.buildTitledBorder("Processing method"));
	    
	    doConvTre = guiBuilder.buildRadioButton("Conventional TRE", performStraightTreT1);
	    doHifiTre = guiBuilder.buildRadioButton("TRE-HIFI Processing", performTreT1HIFI);
	    if(ViewUserInterface.getReference().getRegisteredImagesNum() < 3) {
	    	doHifiTre.setEnabled(false); //hifi is not possible without available irspgr images
	    
		    doHifiTre.addMouseListener(new MouseListener() {
				public void mouseClicked(MouseEvent e) {
					if(!doHifiTre.isEnabled()) {
						MipavUtil.displayInfo("Hifi processing requires irspgr images, load at least three images into MIPAV and restart TRE to enable this feature.");
					}
				}
				public void mousePressed(MouseEvent e) {}
				public void mouseReleased(MouseEvent e) {}
				public void mouseEntered(MouseEvent e) {}
				public void mouseExited(MouseEvent e) {}
		    });
	    }
	    
	    ActionListener c = new ProcessChoiceListener();
	    doConvTre.addActionListener(c);
	    doHifiTre.addActionListener(c);
	    
	    ButtonGroup processType = new ButtonGroup();
	    processType.add(doConvTre);
	    processType.add(doHifiTre);
	    
	    //guiHelp envelopes elements in JPanels, so need to add parent
	    methodPanel.add(doConvTre.getParent(), panelLayout);
	    methodPanel.add(doHifiTre.getParent(), panelLayout);
	    
	    gbc.gridx = 0;
	    gbc.gridy = 0;
	    gbc.fill = GridBagConstraints.HORIZONTAL;
	    gbc.weightx = 1;
	    gbc.weighty = 0;
	    add(methodPanel, gbc);
	
	    tab = new JTabbedPane();
	    buildConventionalTabs();
	    
	    //add both general and hifi at same location
	    gbc.gridx = 0;
	    gbc.gridy = 1;
	    gbc.fill = GridBagConstraints.BOTH;
	    gbc.weightx = 1;
	    gbc.weighty = 1;
	    add(tab, gbc);
	
	    gbc.gridx = 0;
	    gbc.gridy = 2;
	    gbc.weightx = 1;
	    gbc.weighty = 0;
	    
	    add(guiBuilder.buildOKCancelPanel(), gbc);
	    //ok.addActionListener(this);
	    //cancel.addActionListener(this);
	    setLocationRelativeTo(null);
	    pack();
	    setVisible(true);
	}

	/**
	 * Specifies an addendum to the preferred size of the dialog equal to the length of the longest image.
	 * 
	 */
	private void setDialogWidthAdd() {
		dialogWidthAdd = 0;
		if(doHifiTre != null && doHifiTre.isSelected()) {
			dialogWidthAdd = 80;
		}
		int longestLength = 0;
		Enumeration<String> list = ViewUserInterface.getReference().getRegisteredImageNames();
		String str = "";
		while(list.hasMoreElements()) {
			str = list.nextElement();
			if(str.length() > longestLength) {
				longestLength = str.length();
			}
		}
		if(longestLength > 15) {
			dialogWidthAdd += ((longestLength-15)*4.5);
		}
		//System.out.println(dialogWidthAdd);
	}

	/**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        
        if(cAlgo != null) {
            if(showB1Map && cAlgo.getB1ResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getB1ResultStack());
            }
            
            if(calculateM0 && cAlgo.getM0ResultStack() != null) {
                AlgorithmParameters.storeImageInRunner(cAlgo.getM0ResultStack());
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
        treTR = scriptParameters.getParams().getDouble("tre_TR");
        irspgrTR = scriptParameters.getParams().getDouble("irspgr_TR");
        irspgrKy = scriptParameters.getParams().getDouble("irspgr_Ky");
        irspgrFA = scriptParameters.getParams().getDouble("irspgr_FA");
        maxT1 = scriptParameters.getParams().getDouble("max_T1");
        maxM0 = scriptParameters.getParams().getDouble("max_M0");
        //double[], note all non-string arrays are stored as parameter lists
        if(scriptParameters.getParams().containsParameter("tre_FA")) {
            treFA = scriptParameters.getParams().getList("tre_FA").getAsDoubleArray();
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
        performStraightTreT1 = scriptParameters.getParams().getBoolean("perform_Straight_treT1");
        performTreT1withPreCalculatedB1Map = scriptParameters.getParams().getBoolean("perform_treT1_with_PreCalculatedB1Map");
        performTreT1HIFI = scriptParameters.getParams().getBoolean("perform_treT1_HIFI");
        boolean doubleInversion = scriptParameters.getParams().getBoolean("double_Inversion");
        @SuppressWarnings("unused")
        boolean singleInversion = scriptParameters.getParams().getBoolean("single_Inversion");
        boolean geScanner = scriptParameters.getParams().getBoolean("ge_Scanner");
        @SuppressWarnings("unused")
        boolean siemensScanner = scriptParameters.getParams().getBoolean("siemens_Scanner");
        boolean threeTField = scriptParameters.getParams().getBoolean("three_T_Field");
        @SuppressWarnings("unused")
        boolean onefiveTField = scriptParameters.getParams().getBoolean("one_five_TField");
        calculateT1 = scriptParameters.getParams().getBoolean("calculate_T1");
        showB1Map = scriptParameters.getParams().getBoolean("show_B1_Map");
        calculateM0 = scriptParameters.getParams().getBoolean("calculate_M0");
        invertT1toR1 = scriptParameters.getParams().getBoolean("invert_T1_to_R1");
        useWeights = scriptParameters.getParams().getBoolean("use_Weights");
        uniformAngleSpacing = scriptParameters.getParams().getBoolean("uniform_Angle_Spacing");
        upperLeftCorner = scriptParameters.getParams().getBoolean("upper_Left_Corner");
        upperRightCorner = scriptParameters.getParams().getBoolean("upper_Right_Corner");
        lowerLeftCorner = scriptParameters.getParams().getBoolean("lower_Left_Corner");
        lowerRightCorner = scriptParameters.getParams().getBoolean("lower_Right_Corner");
        boolean useSmartThresholding = scriptParameters.getParams().getBoolean("use_Smart_Thresholding");
        boolean useHardThresholding = scriptParameters.getParams().getBoolean("use_Hard_Thresholding");
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
        
        //TODO: Implement enumerations in JIST
        //set enumerations
        if(useSmartThresholding) {
        	thresholdMethod = Threshold.SMART;
        } else if(useHardThresholding) {
        	thresholdMethod = Threshold.HARD;
        } else {
        	thresholdMethod = Threshold.NONE;
        	hardNoiseThreshold = 0.0f;
        }
        
        if(doubleInversion) {
        	inversionType = InversionType.DOUBLE;
        } else {
        	inversionType = InversionType.SINGLE;
        }
        
        if(geScanner) {
        	scannerType = ScannerType.GE;
        } else {
        	scannerType = ScannerType.PHILIPS;
        }
        
        if(threeTField) {
        	mriStrength = FieldStrength.mri3T;
        } else {
        	mriStrength = FieldStrength.mri15T;
        }
    }

    protected void storeParamsFromGUI() throws ParserException {
        //TODO: Get JIST to understand enumerations
    	boolean useHardThresholding = false, useSmartThresholding = false;
    	boolean doubleInversion = false, singleInversion = false;
    	boolean geScanner = false, siemensScanner = false;
    	boolean threeTField = false, onefiveTField = false;
        
        //convert enums to simple booleans
    	if(thresholdMethod.equals(Threshold.SMART)) {
    		useSmartThresholding = true;
    	} else if(thresholdMethod.equals(Threshold.HARD)) {
    		useHardThresholding = true;
    	} else {
    		useHardThresholding = true;
    		hardNoiseThreshold = 0.0f;
    	}
    	
    	if(inversionType.equals(InversionType.DOUBLE)) {
    		doubleInversion = true;
    	} else if(inversionType.equals(InversionType.SINGLE)){
    		singleInversion = true;
    	}
    	
    	if(scannerType.equals(ScannerType.GE)) {
    		geScanner = true;
    	} else {
    		siemensScanner = true;
    	}
    	
    	if(mriStrength.equals(FieldStrength.mri15T)) {
    		onefiveTField = true;
    	} else {
    		threeTField = true;
    	}
    	
    	scriptParameters.getParams().put(ParameterFactory.newParameter("tre_TR", treTR));
        scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_TR", irspgrTR));
        scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_Ky", irspgrKy)); 
        scriptParameters.getParams().put(ParameterFactory.newParameter("irspgr_FA", irspgrFA));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_T1", maxT1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_M0", maxM0));
        //double[], note all non-string arrays are stored as parameter lists
        if(treFA != null) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("tre_FA", treFA));
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
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_Straight_treT1", performStraightTreT1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_treT1_with_PreCalculatedB1Map", performTreT1withPreCalculatedB1Map));
        scriptParameters.getParams().put(ParameterFactory.newParameter("perform_treT1_HIFI", performTreT1HIFI));
		scriptParameters.getParams().put(ParameterFactory.newParameter("double_Inversion", doubleInversion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("single_Inversion", singleInversion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("ge_Scanner", geScanner));  
        scriptParameters.getParams().put(ParameterFactory.newParameter("siemens_Scanner", siemensScanner));
        scriptParameters.getParams().put(ParameterFactory.newParameter("three_T_Field", threeTField));
        scriptParameters.getParams().put(ParameterFactory.newParameter("one_five_TField", onefiveTField));
        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_T1", calculateT1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("show_B1_Map", showB1Map));
        scriptParameters.getParams().put(ParameterFactory.newParameter("calculate_M0", calculateM0));
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
        	
        	if(calculateM0 && cAlgo.getM0ResultStack() != null) {
        		//scriptParameters.storeImageInRecorder(cAlgo.getM0ResultStack());
        		scriptParameters.storeOutputImageParams(cAlgo.getM0ResultStack(), true);
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
    
    /**
     * This method builds the conventional tabs based on possibly pre-defined values.  Each method places these tabs
     * in containers to allow for nice display.
     */
    private void buildConventionalTabs() {
	    
	    //construct general, show if performStraighttreT1
	    straightPanel = buildConventionalTreT1Panel();
	    straightPanel.setName("Conventional initPanel");
	    
	    //conventional panels for next section
	    treLong = buildTreT1LongPanel();
	    treLong.setName("Conventional long panel");
	    
	    //specifics section
	    convSpec = buildTreT1SpecificsPanel();
	    convSpec.setName("Conventional Specifics");
	
	    //threshold
	    totalThreshold = buildThresholdPanel();
	    
	    tab.add(straightPanel, "1: General");
	    tab.add(treLong, "2: Images");
	    tab.add(convSpec, "3: Output ");
	    tab.add(totalThreshold, "4: Threshold");
	}

	private void buildHIFITabs() {
	    
	    //construct Hifi, do not show
	    hifiPanel = buildHIFIPanel();
	    
	    //hifi panels for next section
	    spgrPanel = buildSPGRPanel();
	    
	    irspgrPanel = buildIRSPGRPanel();
	    
	    if(scannerType.equals(ScannerType.GE)) {
	    	irspgrPanel.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: IR-SPGR GE Image Information"));
		} else {
			irspgrPanel.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: IR-SPGR Siemens Image Information"));
		}
	    
	    //specifics section
	    hifiSpec = buildTreT1HIFISpecificsPanel();
	
	    //threshold
	    totalThreshold = buildThresholdPanel();
	    
	    
	    tab.add(hifiPanel, "1: General");
	    tab.add(spgrPanel, "2: Images");
	    if(scannerType.equals(ScannerType.GE)) {
	    	tab.add(irspgrPanel, "3: GE");
	    } else {
	    	tab.add(irspgrGeneralPanel, "3: Siemens");
	    }
	    tab.add(hifiSpec, "4: Output ");
	    tab.add(totalThreshold, "5: Threshold");
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
	    
	    displayTotalDialog();
	}

	private void setProcessConvUI() {
    	if(useB1Map != null && useB1Map.isSelected()) {
			performStraightTreT1 = false;
	    	performTreT1withPreCalculatedB1Map = true;
    	} else {
    		performStraightTreT1 = true;
	    	performTreT1withPreCalculatedB1Map = false;
    	}
        performTreT1HIFI = false;
    }
    
    private void setProcessHifiUI() {
    	performTreT1HIFI = true;
    	performStraightTreT1 = false;
        performTreT1withPreCalculatedB1Map = false;
    }
    
    /**
     * Sets UI variables for either saving between processing steps or later reuse.  When process is true, 
     * null values cause an issue to be returned, when process is false (indicating UI variables are just
     * being transferred between processing steps), no action is taken for null values
     * 
     * @param process
     * @return
     */
    private String setUI(boolean process) {
		//set conventional
    	ViewUserInterface.getReference().getMessageFrame().append("SETTING ALGORITHM: \n", ViewJFrameMessage.DEBUG);
    	try {
    		Nsa = Double.valueOf(spgrNumFA.getText()).intValue();
    		
    		treFA = new double[Nsa];

    	    spgrImageIndex = new int[Nsa];
    	    
    	    spgrData = new double[Nsa];
    	    
    	    ViewUserInterface.getReference().getMessageFrame().append("Nsa: "+Nsa+"\n", ViewJFrameMessage.DEBUG);
    	} catch(Exception e) {
    		if(process) {
    			return "Number of spgr flip angles has not been set.";
    		}
    	}
    	
    	if(!process || (process && performTreT1HIFI)) {
	    	try {
				//set spgr
				for(int i=0; i<Nsa; i++) {
			        spgrImageIndex[i] = spgrImageComboBoxAr[i].getSelectedIndex();
			        treFA[i] = Float.valueOf(flipAngleAr[i].getText()).floatValue();
			        
			        ViewUserInterface.getReference().getMessageFrame().append("spgrImageIndex: "+spgrImageIndex[i]+"\tFlip Angle: "+treFA[i]+"\n", ViewJFrameMessage.DEBUG);
			    }
			    treTR = Double.valueOf(spgrRepTime.getText()).doubleValue();
			    ViewUserInterface.getReference().getMessageFrame().append("TreTR: "+treTR+"\n", ViewJFrameMessage.DEBUG);
		    } catch(Exception e) {
		    	if(process && performTreT1HIFI) {
		    		return "Invalid spgr information entered";
		    	}
		    }
			
		    //set Hifi
	    	try {
	    		irspgrTI = new double[Nti];
	
		        irspgrTr = new double[Nti];
		        irspgrImageIndex = new int[Nti];
		        irspgrData = new double[Nti];
		        
		        //initial value in gui as well, but must be set explicitly by irspgrTRField
		        irspgrTR = treTR;
	    		
	    		Nti = Double.valueOf(irspgrNum.getText()).intValue();
	    		ViewUserInterface.getReference().getMessageFrame().append("Nti: "+Nti+"\n", ViewJFrameMessage.DEBUG);
	    	} catch(Exception e) { 
	    		if(process && performTreT1HIFI) {
	    			return "Number of irspgr images has not been set.";
	    		}
	    	}
	    	
	    	try {
			    //set irspgrGE and siemens
			    for (int i=0; i<Nti; i++) {
			        irspgrImageIndex[i] = irspgrCombo[i].getSelectedIndex();
			        irspgrTI[i] = Double.valueOf(irspgrField[i].getText()).doubleValue();
			        
			        ViewUserInterface.getReference().getMessageFrame().append("irspgrImageIndex: "+irspgrImageIndex[i]+"\tirspgrTI: "+irspgrTI[i]+"\n", ViewJFrameMessage.DEBUG);
			    }
			    irspgrTR = Double.valueOf(irspgrTRField.getText()).doubleValue();
			    irspgrFA = Double.valueOf(irspgrFAField.getText()).doubleValue();
			    irspgrKy = Double.valueOf(numSlicesField.getText()).doubleValue();
			    
			    ViewUserInterface.getReference().getMessageFrame().append("irspgrTR: "+irspgrTR+"\n", ViewJFrameMessage.DEBUG);
			    ViewUserInterface.getReference().getMessageFrame().append("irspgrFA: "+irspgrFA+"\n", ViewJFrameMessage.DEBUG);
			    ViewUserInterface.getReference().getMessageFrame().append("irspgrKy: "+irspgrKy+"\n", ViewJFrameMessage.DEBUG);
		    } catch(Exception e) {
		    	if(process && performTreT1HIFI) {
		    		return "Invalid irspgr information entered.";
		    	}
		    }
	    	
	    	try {
			    if(isGEButton.isSelected()) {
			    	scannerType = ScannerType.GE;
			    } else if(isGEButton.isSelected()) {
			    	scannerType = ScannerType.SIEMENS;
			    }
	    		
			    if(doubleInvRadio.isSelected()) {
			    	inversionType = InversionType.DOUBLE;
			    } else if(singleInvRadio.isSelected()) {
			    	inversionType = InversionType.SINGLE;
			    }
			    
			    if(t15Radio.isSelected()) {
			    	mriStrength = FieldStrength.mri15T;
			    } else if(t30Radio.isSelected()) {
			    	mriStrength = FieldStrength.mri3T;
			    }
			   
			    showB1Map = showB1Check.isSelected(); //this value is only used for HIFI, so should be left here
			    smoothB1Field = smoothB1Box.isSelected();
			    
			    ViewUserInterface.getReference().getMessageFrame().append("scanner typer: "+scannerType.toString()+"\n", ViewJFrameMessage.DEBUG);
			    ViewUserInterface.getReference().getMessageFrame().append("inversionType: "+inversionType.toString()+"\n", ViewJFrameMessage.DEBUG);
			    ViewUserInterface.getReference().getMessageFrame().append("fieldStrength: "+mriStrength.toString()+"\n", ViewJFrameMessage.DEBUG);
			    ViewUserInterface.getReference().getMessageFrame().append("showB1Map: "+showB1Map+"\n", ViewJFrameMessage.DEBUG);
			    ViewUserInterface.getReference().getMessageFrame().append("smoothB1Field: "+smoothB1Field+"\n", ViewJFrameMessage.DEBUG);
	    	} catch(Exception e) { 
	    		if(process && performTreT1HIFI) {
	    			return "Invalid scanner information entered";
	    		}
	    	}
    	}
	    
    	if(!process || (process && (performStraightTreT1 || performTreT1withPreCalculatedB1Map))) {
	    	//set t1Long
	    	try {
	    		if (performTreT1withPreCalculatedB1Map) {
			        b1ImageIndex = b1Field.getSelectedIndex();
			    }
			    
			    for (int i=0; i<Nsa; i++) {
			        spgrImageIndex[i] = convimageComboAr[i].getSelectedIndex();
			        treFA[i] = Float.valueOf(convFAFieldAr[i].getText()).floatValue();
			        
			        ViewUserInterface.getReference().getMessageFrame().append("spgrImageIndex: "+spgrImageIndex[i]+"\tFlip Angle: "+treFA[i]+"\n", ViewJFrameMessage.DEBUG);
			    }
			    
			    treTR = Double.valueOf(convRepTime.getText()).doubleValue();
			    if(process && (performStraightTreT1 || performTreT1withPreCalculatedB1Map)) {
			    	showB1Map = false; //the B1 map is not computed in the conventional case, this keeps TRE from trying to open this image
			    }
			    ViewUserInterface.getReference().getMessageFrame().append("TreTR: "+treTR+"\n", ViewJFrameMessage.DEBUG);
	    	} catch(Exception e) {
	    		if(process && (performStraightTreT1 || performTreT1withPreCalculatedB1Map)) {
	    			//System.out.println("In conventional");
	    			return "Conventional TRE variables not correctly entered.";
	    		}
	    	}
    	}
	    
	    //used for both generic and hiifi
	    try {
		    maxT1 = Double.valueOf(maxT1Field.getText()).doubleValue();
		    maxM0 = Double.valueOf(maxM0Field.getText()).doubleValue();
		    calculateT1 = showT1Map.isSelected();
		    calculateM0 = showM0Map.isSelected();
		    invertT1toR1 = showR1Map.isSelected();
		    
		    ViewUserInterface.getReference().getMessageFrame().append("maxT1: "+maxT1+"\n", ViewJFrameMessage.DEBUG);
		    ViewUserInterface.getReference().getMessageFrame().append("maxM0: "+maxM0+"\n", ViewJFrameMessage.DEBUG);
		    ViewUserInterface.getReference().getMessageFrame().append("calculateT1: "+calculateT1+"\n", ViewJFrameMessage.DEBUG);
		    ViewUserInterface.getReference().getMessageFrame().append("calculateM0: "+calculateM0+"\n", ViewJFrameMessage.DEBUG);
		    ViewUserInterface.getReference().getMessageFrame().append("invertT1toR1: "+invertT1toR1+"\n", ViewJFrameMessage.DEBUG);
		    
		    if (Nsa > 2) {
		        useWeights = leastSquaresCheck.isSelected();
		        ViewUserInterface.getReference().getMessageFrame().append("useWeights: "+useWeights+"\n", ViewJFrameMessage.DEBUG);
		    }
	    } catch(Exception e) {
	    	if(process) {
	    		return "Processing options have not been correctly set.";
	    	}
	    }
	    
	    if(smartCheckBox.isSelected()) {
	    	thresholdMethod = Threshold.SMART;
	    } else if(hardCheckBox.isSelected()) {
	    	thresholdMethod = Threshold.HARD;
	    } else {
	    	thresholdMethod = Threshold.NONE;
	    }
	    
	    ViewUserInterface.getReference().getMessageFrame().append("Threshold: "+thresholdMethod.toString()+"\n", ViewJFrameMessage.DEBUG);
	    
	    String threshOutput;
	    if(thresholdMethod.equals(Threshold.SMART)) {
	    	if((!(threshOutput = setSmartThresholdUI(process)).equals(SUCCESS)) && process) {
	    		return threshOutput;
	    	}
	    } else if(thresholdMethod.equals(Threshold.HARD)) {
	    	if((!(threshOutput = setHardThresholdUI(process)).equals(SUCCESS)) && process) {
	    		return threshOutput;
	    	}
	    } else if(thresholdMethod.equals(Threshold.NONE)) {
	    	hardNoiseThreshold = 0.0f;
	    }
	    
	    //pre-processing
	    if(process && performTreT1HIFI) {
	    	//System.out.println("In hifi");
	    	try {
			    if (mriStrength.equals(FieldStrength.mri3T)) {
			        if (inversionType.equals(InversionType.DOUBLE)) {
			            for (int i=0; i<Nti; i++) {
			            	irspgrTI[i] = irspgrTI[i]*0.9;
			            }
			            irspgrKy = (irspgrKy/2.00)+2.00;
			        }
			        else {
			            for (int i=0; i<Nti; i++) {
			            	irspgrTI[i] = irspgrTI[i]*0.9*0.93;
			            }
			            irspgrKy = irspgrKy + 2.00;
			        }
			    }
			    else {
			        if (inversionType.equals(InversionType.DOUBLE)) {
			            for (int i=0; i<Nti; i++) {
			            	irspgrTI[i] = irspgrTI[i];
			            }
			            irspgrKy = (irspgrKy/2.00)+2.00;
			        }
			        else {
			            for (int i=0; i<Nti; i++) {
			            	irspgrTI[i] = irspgrTI[i];
			            }
			        }
			    }
			    
			    for (int i=0; i<Nti; i++) {
			    	irspgrTr[i] = irspgrTI[i] + irspgrTR*irspgrKy;
			    }
			    
			    for(int i=0; i<Nti; i++) {
			    	ViewUserInterface.getReference().getMessageFrame().append("irspgrTr["+i+"]: "+irspgrTr[i]+"\n", ViewJFrameMessage.DEBUG);
			    	ViewUserInterface.getReference().getMessageFrame().append("irspgrTI["+i+"]: "+irspgrTI[i]+"\n", ViewJFrameMessage.DEBUG);
			    	ViewUserInterface.getReference().getMessageFrame().append("irspgrKy: "+irspgrKy+"\n", ViewJFrameMessage.DEBUG);
			    }
	    	} catch(Exception e) {
	    		return "IRSPGR information has not been fully entered.";
	    	}
	    }
	    
	    return SUCCESS;
	}

	private String setHardThresholdUI(boolean process) {
		try {
			//hard threshold
		    hardNoiseThreshold = Float.valueOf(hardNoiseField.getText()).floatValue();
		    ViewUserInterface.getReference().getMessageFrame().append("hardNoiseThreshold: "+hardNoiseThreshold+"\n", ViewJFrameMessage.DEBUG);
		} catch(Exception e) {
			if(process) {
				return "Invalid hard threshold noise entered";
			}
		}
		return SUCCESS;
	}

	private String setSmartThresholdUI(boolean process) {
		try {
			//smart noise threshold
		    noiseScale = Float.valueOf(smartNoiseField.getText()).floatValue();
		    upperLeftCorner = topLeftBox.isSelected();
		    upperRightCorner = topRightBox.isSelected();
		    lowerLeftCorner = bottomLeftBox.isSelected();
		    lowerRightCorner = bottomRightBox.isSelected();
		    
		    ViewUserInterface.getReference().getMessageFrame().append("noiseScale: "+noiseScale+"\n", ViewJFrameMessage.DEBUG);
		    ViewUserInterface.getReference().getMessageFrame().append("upperLeftCorner: "+upperLeftCorner+"\n", ViewJFrameMessage.DEBUG);
		    ViewUserInterface.getReference().getMessageFrame().append("upperRightCorner: "+upperRightCorner+"\n", ViewJFrameMessage.DEBUG);
		    ViewUserInterface.getReference().getMessageFrame().append("lowerLeftCorner: "+lowerLeftCorner+"\n", ViewJFrameMessage.DEBUG);
		    ViewUserInterface.getReference().getMessageFrame().append("lowerRightCorner: "+lowerRightCorner+"\n", ViewJFrameMessage.DEBUG);
		} catch(Exception e) {
			if(process) {
				return "Invalid smart thresholding noise entered";
			}
		}
		return SUCCESS;
	}

	private boolean validateUI() {
	
	    //generic
	    if (Nsa > wList.length) {
	        MipavUtil.displayError("Please import all nessesary images first.");
	        return false;
	    }
	    if (Nsa < 2) {
	        MipavUtil.displayError("T1 and M0 calculations require at least two SPGR images.");
	        return false;
	    }
	    if (performTreT1withPreCalculatedB1Map) {
	        if (Nsa+1 > wList.length) {
	            MipavUtil.displayError("Please import all nessesary images first.");
	            return false;
	        }
	    }
	    
	    if(performTreT1HIFI) {
		    if(performTreT1HIFI && (inversionType == null || mriStrength == null || scannerType == null)) {
		    	MipavUtil.displayError("Values for the inversion regime, field strength, and scanner type are required for hifi processing");
		    	return false;
		    }
		    
		    if (Nti < 1) {
		        MipavUtil.displayError("B1 correction requires at least one IR-SPGR image.");
		        return false;
		    }
		    
		    //validate ge and siemens irspgr
		    if(inversionGroup.getSelection() == null) {
		        MipavUtil.displayInfo("Please choose either single or double inversion regime");
		        return false;
		    }
		    
		    if(fieldStrengthGroup.getSelection() == null) {
		        MipavUtil.displayInfo("Please choose field strength");
		        return false;
		    }
	    }
	    
	    return true;
	}

	/**
	 * This listener focuses on the TRE processing choice chosen.  Relevant tabs
	 * are loaded as a result.
	 * 
	 * @author senseneyj
	 *
	 */
	private class ProcessChoiceListener implements ActionListener {
		
		private void varSet() {
			setDialogWidthAdd();
		    setMinimumSize(new Dimension(450+dialogWidthAdd, 600));
		    setPreferredSize(new Dimension(450+dialogWidthAdd, 600));
		    setMaximumSize(new Dimension(800+dialogWidthAdd, 1200));
		    
			if (doConvTre.isSelected()) {
				//this section may e
				setProcessHifiUI();
	            setUI(false);
	            
	            setProcessConvUI();
	            tab.removeAll();
	            buildConventionalTabs();
	            tab.updateUI();
	            
	        }
	        if (doHifiTre.isSelected()) {
	        	//showConventionalDialog();
	        	setProcessConvUI();
	            setUI(false);
	            
	            setProcessHifiUI();
	            tab.removeAll();
	            buildHIFITabs();
	            tab.updateUI();
	        }
		    validate();
		}

		public void actionPerformed(ActionEvent e) {
			varSet();
			//System.out.println("Action: "+e.getActionCommand()+"\t "+e.getSource());
		}
    }
    
	/**
	 * This listener focuses on the user's choice of hard or smart thresholding.
	 * A change will prompt a different panel to be displayed.
	 *
	 * @author senseneyj
	 *
	 */
    private class ThresholdChoiceListener implements ActionListener {
    	private void varSet() {
    		if(thresholdMethod.equals(Threshold.HARD)) {
    			setHardThresholdUI(false);
    		} else if(thresholdMethod.equals(Threshold.SMART)) {
    			setSmartThresholdUI(false);
    		} else {
    			hardNoiseThreshold = 0.0f;
    		}
    		
    		generalThresholdPanel.removeAll();
    		JPanel genericPanel = null;
    		
			if (hardCheckBox.isSelected()) {
				thresholdMethod = Threshold.HARD;
				genericPanel = buildHardThresholdPanel();
	        } else if (smartCheckBox.isSelected()) {
	        	thresholdMethod = Threshold.SMART;
				genericPanel = buildSmartThresholdPanel();
	        } else if(noCheckBox.isSelected()) {
	        	thresholdMethod = Threshold.NONE;
	        	genericPanel = buildNoThresholdPanel();
	        }
			generalThresholdPanel.add(genericPanel);
        	generalThresholdPanel.updateUI();
		}

		public void actionPerformed(ActionEvent e) {
			varSet();
			//System.out.println("Action: "+e.getActionCommand()+"\t "+e.getSource());
		}
    }
    
    /**
     * This listener focuses on the type of scanner used to acquire the images.
     * Currently, both GE and Philips scanners are available.
     * 
     * @author senseneyj
     *
     */
    private class ScannerChoiceListener implements ActionListener {
    	private int getChangeTab(String str) {
    		int changeTab = -1;
			for(int i=0; i<tab.getTabCount(); i++) {
				if(tab.getTitleAt(i).indexOf(str) != -1) {
					changeTab = i;
				}
			}
			return changeTab;
    	}
    	
    	private void varSet() {
    		int tabLoc = -1;
    		if(isGEButton.isSelected()) {
    			if((tabLoc = getChangeTab("Siemens")) != -1) {
    				tab.setTitleAt(tabLoc, (tabLoc+1)+": GE");
    				irspgrPanel.getViewport().removeAll();
					irspgrPanel.setViewportView(buildIRSPGRPanelSiemensInner());
					irspgrPanel.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: IR-SPGR GE Image Information"));
					irspgrPanel.validate();
					irspgrPanel.updateUI();
    			}
    		} else if(isSiemensButton.isSelected()) {
    			if((tabLoc = getChangeTab("GE")) != -1) {
    				tab.setTitleAt(tabLoc, (tabLoc+1)+": Siemens");
    				irspgrPanel.getViewport().removeAll();
					irspgrPanel.setViewportView(buildIRSPGRPanelSiemensInner());
					irspgrPanel.setBorder(MipavUtil.buildTitledBorder("treT1-HIFI: IR-SPGR Siemens Image Information"));
					irspgrPanel.validate();
					irspgrPanel.updateUI();
    			}
    		}
    	}
    	
    	public void actionPerformed(ActionEvent e) {
			varSet();
			//System.out.println("Action: "+e.getActionCommand()+"\t "+e.getSource());
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
            table.put(new ParameterDouble("tre_TR", treTR));
            table.put(new ParameterDouble("irspgr_TR", irspgrTR));
            table.put(new ParameterDouble("irspgr_Ky", irspgrKy));
            table.put(new ParameterDouble("irspgr_FA", irspgrFA));
            table.put(new ParameterDouble("max_T1", maxT1));
            table.put(new ParameterDouble("max_M0", maxM0));
            
            //double[], note all non-string arrays are stored as parameter lists
            table.put(new ParameterList("tre_FA", Parameter.PARAM_DOUBLE, "0,0,0"));
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
            table.put(new ParameterBoolean("perform_Straight_treT1", performStraightTreT1));
            table.put(new ParameterBoolean("perform_treT1_with_PreCalculatedB1Map", performTreT1withPreCalculatedB1Map));
            table.put(new ParameterBoolean("perform_treT1_HIFI", performTreT1HIFI));

            table.put(new ParameterBoolean("double_Inversion", performTreT1HIFI && inversionType.equals(InversionType.DOUBLE)));
            table.put(new ParameterBoolean("single_Inversion", performTreT1HIFI && inversionType.equals(InversionType.SINGLE)));
            table.put(new ParameterBoolean("ge_Scanner", performTreT1HIFI && scannerType.equals(ScannerType.GE)));
            table.put(new ParameterBoolean("siemens_Scanner", performTreT1HIFI && scannerType.equals(ScannerType.SIEMENS)));
            table.put(new ParameterBoolean("three_T_Field", performTreT1HIFI && mriStrength.equals(FieldStrength.mri3T)));
            table.put(new ParameterBoolean("one_five_TField", performTreT1HIFI && mriStrength.equals(FieldStrength.mri15T)));

            table.put(new ParameterBoolean("calculate_T1", calculateT1));
            table.put(new ParameterBoolean("show_B1_Map", showB1Map));
            table.put(new ParameterBoolean("calculate_M0", calculateM0));
            table.put(new ParameterBoolean("invert_T1_to_R1", invertT1toR1));
            table.put(new ParameterBoolean("use_Weights", useWeights));
            table.put(new ParameterBoolean("uniform_Angle_Spacing", uniformAngleSpacing));
            table.put(new ParameterBoolean("upper_Left_Corner", upperLeftCorner));
            table.put(new ParameterBoolean("upper_Right_Corner", upperRightCorner));
            table.put(new ParameterBoolean("lower_Left_Corner", lowerLeftCorner));
            table.put(new ParameterBoolean("lower_Right_Corner", lowerRightCorner));
            table.put(new ParameterBoolean("use_Smart_Thresholding", thresholdMethod.equals(Threshold.SMART)));
            table.put(new ParameterBoolean("use_Hard_Thresholding", thresholdMethod.equals(Threshold.NONE) || thresholdMethod.equals(Threshold.HARD)));
            
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
            for(int i=0; i<parTotal.length; i++) {w
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
                
                if(calculateM0 && cAlgo.getM0ResultStack() != null) {
                    //scriptParameters.storeImageInRecorder(cAlgo.getM0ResultStack());
                    scriptParameters.storeOutputImageParams(cAlgo.getM0ResultStack(), true);
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
            table.put(new ParameterImage("m0_results"));
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
