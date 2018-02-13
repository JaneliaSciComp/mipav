package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.IOException;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmPhaseCongruency;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class JDialogPhaseCongruency extends JDialogBase implements
		AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5977927697947830216L;
	
	private ModelImage srcImage;
	
	private JTextField[] fields;
	
	private JCheckBox edgeBox;
	
	private JCheckBox cornerBox;
	
	private JCheckBox orientationBox;
	
	private JCheckBox phaseBox;
	
	@SuppressWarnings("rawtypes")
	private JComboBox methodBox;
	
	private JDialog helpDialog;
	
	private Frame imFrame;
	
	public JDialogPhaseCongruency(Frame frame, ModelImage im){

		super(frame, false);
		srcImage = im;
		imFrame = frame;
		imFrame.addWindowListener(this);
		fields = new JTextField[9];

		for(int i=0;i<9;i++){
			fields[i] = new JTextField(4);
			fields[i].setFont(serif12);
			fields[i].setHorizontalAlignment(JTextField.RIGHT);
		}
		init();

	}
	
	public void actionPerformed(ActionEvent event){
		String command = event.getActionCommand();
		
		if(command.equals("OK"))
			callAlgorithm();
		else if(command.equals("Cancel")){
			dispose();
			helpDialog.dispose();
		}
		else if(command.equals("Help")){
			if(helpDialog == null)
				displayHelp();
		} else if(command.equals("Close")){
			helpDialog.dispose();
			helpDialog = null;
		}
	}
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		AlgorithmPhaseCongruency alg = (AlgorithmPhaseCongruency) algorithm;
		if(edgeBox.isSelected()){
			String name = srcImage.getImageName() + "_edges";
			ModelImage edgeImage = new ModelImage(ModelImage.DOUBLE, srcImage.getExtents(), name);
			try {
				edgeImage.importData(0, alg.getEdges(), true);
			} catch (IOException e) {
				e.printStackTrace();
			}
			new ViewJFrameImage(edgeImage);
		}
		if(cornerBox.isSelected()){
			String name = srcImage.getImageName() + "_corners";
			ModelImage cornerImage = new ModelImage(ModelImage.DOUBLE, srcImage.getExtents(), name);
			try {
				cornerImage.importData(0, alg.getCorners(), true);
			} catch (IOException e) {
				e.printStackTrace();
			}
			new ViewJFrameImage(cornerImage);
		}
		if(orientationBox.isSelected()){
			String name = srcImage.getImageName() + "_orientation";
			ModelImage cornerImage = new ModelImage(ModelImage.DOUBLE, srcImage.getExtents(), name);
			try {
				cornerImage.importData(0, alg.getOrientations(), true);
			} catch (IOException e) {
				e.printStackTrace();
			}
			new ViewJFrameImage(cornerImage);
		}
		if(phaseBox.isSelected()){
			String name = srcImage.getImageName() + "_phase";
			ModelImage cornerImage = new ModelImage(ModelImage.DOUBLE, srcImage.getExtents(), name);
			try {
				cornerImage.importData(0, alg.getFeatureType(), true);
			} catch (IOException e) {
				e.printStackTrace();
			}
			new ViewJFrameImage(cornerImage);
		}
		
		dispose();
	}
	
	protected void callAlgorithm(){
		
		int nscale, norient, minWaveLength, g, noiseMethod;
		double mult, sigmaOnf, k, cutOff;
		
		try{
			nscale = Integer.parseInt(fields[0].getText());
			norient = Integer.parseInt(fields[1].getText());
			minWaveLength = Integer.parseInt(fields[2].getText());
			mult = Double.parseDouble(fields[3].getText());
			sigmaOnf = Double.parseDouble(fields[4].getText());
			k = Double.parseDouble(fields[5].getText());
			cutOff = Double.parseDouble(fields[6].getText());
			g = Integer.parseInt(fields[7].getText());
			
			if(methodBox.getSelectedIndex() < 2){
				noiseMethod = -(methodBox.getSelectedIndex() + 1);
			} else noiseMethod = Integer.parseInt(fields[8].getText());
		} catch (NumberFormatException e){
			e.printStackTrace();
			MipavUtil.displayError("A number is formatted wrong");
			return;
		}
				
		AlgorithmPhaseCongruency alg = new AlgorithmPhaseCongruency(srcImage);
		alg.setArguments(nscale, norient, minWaveLength, mult, sigmaOnf, k, cutOff, g, noiseMethod);
		alg.addListener(this);
		
		createProgressBar("Phase Congruency", alg);
		progressBar.setVisible(true);
		
		if (isRunInSeparateThread()) {
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
		
		setVisible(false);
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void init(){
		setTitle("Phase Congruency");
		
		JPanel inputPanel = new JPanel(new GridBagLayout());
		inputPanel.setForeground(Color.black);
		inputPanel.setBorder(buildTitledBorder("Input options"));
		
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5,5,5,5);
		gbc.gridx = 0;
		gbc.gridy = 0;
		
		Dimension leftSize = new Dimension(85, 20);
		Dimension rightSize = new Dimension(75, 20);
		
		JLabel scaleLabel = new JLabel("# Scales");
		scaleLabel.setFont(serif12); 
		scaleLabel.setPreferredSize(leftSize);
		inputPanel.add(scaleLabel, gbc);
		
		gbc.gridx++;
		
		fields[0].setText("4");
		inputPanel.add(fields[0], gbc);
		
		gbc.gridx++;
		
		JLabel orientLabel = new JLabel("# Orientations");
		orientLabel.setFont(serif12);
		orientLabel.setPreferredSize(rightSize);
		inputPanel.add(orientLabel, gbc);
		
		gbc.gridx++;
		
		fields[1].setText("6");
		inputPanel.add(fields[1], gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		
		JLabel waveLabel = new JLabel("Min. wavelength");
		waveLabel.setFont(serif12);
		waveLabel.setPreferredSize(leftSize);
		inputPanel.add(waveLabel, gbc);
		
		gbc.gridx++;
		
		fields[2].setText("3");
		inputPanel.add(fields[2], gbc);
		
		gbc.gridx++;
		
		JLabel multLabel = new JLabel("Scaling factor");
		multLabel.setFont(serif12);
		multLabel.setPreferredSize(rightSize);
		inputPanel.add(multLabel, gbc);
		
		gbc.gridx++;
		
		fields[3].setText("2.1");
		inputPanel.add(fields[3], gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		
		JLabel sigmaLabel = new JLabel("Filter sigma");
		sigmaLabel.setFont(serif12);
		sigmaLabel.setPreferredSize(leftSize);
		inputPanel.add(sigmaLabel, gbc);
		
		gbc.gridx++;
		
		fields[4].setText("0.55");
		inputPanel.add(fields[4], gbc);
		
		gbc.gridx++;
		
		JLabel kLabel = new JLabel("k");
		kLabel.setFont(serif12);
		kLabel.setPreferredSize(rightSize);
		inputPanel.add(kLabel, gbc);
		
		gbc.gridx++;
		
		fields[5].setText("2.0");
		inputPanel.add(fields[5], gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		
		JLabel cutoffLabel = new JLabel("Cutoff");
		cutoffLabel.setFont(serif12);
		cutoffLabel.setPreferredSize(leftSize);
		inputPanel.add(cutoffLabel, gbc);
		
		gbc.gridx++;
		
		fields[6].setText("0.5");
		inputPanel.add(fields[6], gbc);
		
		gbc.gridx++;
		
		JLabel gLabel = new JLabel("g");
		gLabel.setFont(serif12);
		gLabel.setPreferredSize(rightSize);
		inputPanel.add(gLabel, gbc);
		
		gbc.gridx++;
		
		fields[7].setText("10");
		inputPanel.add(fields[7], gbc);
		
		gbc.gridx = 0;
		gbc.gridwidth = 2;
		gbc.gridy++;
		
		JLabel methodLabel = new JLabel("Noise estimation method");
		methodLabel.setFont(serif12);
		inputPanel.add(methodLabel, gbc);
		
		gbc.gridx = 2;
		gbc.gridwidth = 1;
		
		String[] methods = new String[]{"Median", "Mode", "Value"};
		
		DefaultListCellRenderer r = new DefaultListCellRenderer();
		r.setHorizontalAlignment(DefaultListCellRenderer.CENTER);
		
		methodBox = new JComboBox(methods);
		methodBox.setRenderer(r);
		methodBox.setPreferredSize(rightSize);
		methodBox.setFont(serif12);
		methodBox.setSelectedIndex(0);
		methodBox.addItemListener(this);
		inputPanel.add(methodBox, gbc);
		
		gbc.gridx++;
		
		fields[8].setText("0");
		fields[8].setEnabled(false);
		inputPanel.add(fields[8], gbc);
		
		getContentPane().add(inputPanel, BorderLayout.NORTH);	
		
		JPanel outputPanel = new JPanel(new GridBagLayout());
		outputPanel.setForeground(Color.black);
		outputPanel.setBorder(buildTitledBorder("Output options"));
		
		GridBagConstraints gbc2 = new GridBagConstraints();
		
		gbc2.anchor = GridBagConstraints.WEST;
		gbc2.gridx = 0;
		gbc2.gridy = 0;
		
		edgeBox = new JCheckBox("Edges");
		edgeBox.setFont(serif12);
		edgeBox.setSelected(true);
		outputPanel.add(edgeBox, gbc2);
		
		gbc2.gridx++;
		
		orientationBox = new JCheckBox("Orientation");
		orientationBox.setFont(serif12);
		outputPanel.add(orientationBox, gbc2);
		
		gbc2.gridx = 0;
		gbc2.gridy++;
		
		cornerBox = new JCheckBox("Corners");
		cornerBox.setFont(serif12);
		outputPanel.add(cornerBox, gbc2);
		
		gbc2.gridx++;
		
		phaseBox = new JCheckBox("Phase angle");
		phaseBox.setFont(serif12);
		outputPanel.add(phaseBox, gbc2);
		
		getContentPane().add(outputPanel);
		
		buildOKCancelButtons();
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.setForeground(Color.black);
		
		JButton helpButton = new JButton("Help");
		helpButton.setFont(serif12B);
		helpButton.setMinimumSize(MipavUtil.defaultButtonSize);
        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
		helpButton.addActionListener(this);
		
		buttonPanel.add(OKButton);
		buttonPanel.add(cancelButton);
		buttonPanel.add(helpButton);
		
		
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
		System.gc();
	}
	
	private void displayHelp(){
		
		helpDialog = new JDialog();
		helpDialog.setLayout(new BorderLayout());
		helpDialog.addWindowListener(this);
		helpDialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		helpDialog.setTitle("Phase Congruency Help");
		
		String helpText = "<html>"
				+ "As determined by Peter Kovesi: <br>"
				+ "<b># Scales:</b> Number of wavelet scales, try values 3-6<br>"
				+ "<b># Orientations:</b> Number of filter orientations.<br>"
				+ "<b>Min. wavelength:</b> Wavelength of smallest scale filter.<br>"
				+ "<b>Scaling factor:</b> Scaling factor between successive filters.<br>"
				+ "<b>Filter sigma:</b> Ratio of the standard deviation of the Gaussian<br>"
				+ "describing the log Gabor filter's transfer function<br>"
				+ "in the frequency domain to the filter center frequency<br>"
				+ "<b>k:</b> No of standard deviations of the noise energy beyond<br>"
				+ "the mean at which we set the noise threshold point.<br>"
				+ "You may want to vary this up to a value of 10 or<br>"
				+ "20 for noisy images<br>"
				+ "<b>Cutoff:</b> The fractional measure of frequency spread<br>"
				+ "below which phase congruency values get penalized.<br>"
				+ "<b>g:</b> Controls the sharpness of the transition in<br>"
				+ "the sigmoid function used to weight phase<br>"
				+ "congruency for frequency spread. <br><br>"
				+ "<b>Noise estimation method</b><br>"
				+ "<b>Median:</b> Should be used in most cases.<br>"
				+ "<b>Mode:</b> For noise that follows a Rayleigh distribution.<br>"
				+ "<b>Value:</b> For when you already know the level of noise.";
		
		JPanel textPanel = new JPanel();
		textPanel.setForeground(Color.black);
		
		JLabel helpLabel = new JLabel(helpText);
		helpLabel.setFont(serif12);
		textPanel.add(helpLabel);
		helpDialog.getContentPane().add(textPanel);
		
		JPanel closePanel = new JPanel();
		closePanel.setForeground(Color.black);
		
		JButton closeButton = new JButton("Close");
		closeButton.setFont(serif12);
		closeButton.addActionListener(this);
		closePanel.add(closeButton);
		
		helpDialog.getContentPane().add(closePanel, BorderLayout.SOUTH);
		
		helpDialog.pack();
		helpDialog.setVisible(true);
		
	}
	
	public void itemStateChanged(ItemEvent e){
		
		Object source = e.getSource();
		if(source instanceof JComboBox){
			int i = methodBox.getSelectedIndex();
			if(i==2){
				fields[8].setEnabled(true);
			} else fields[8].setEnabled(false);
		}
	}
	
	public void windowClosing(WindowEvent e){
		if(e.getSource() == helpDialog){
			helpDialog = null;
		}
	}

}
