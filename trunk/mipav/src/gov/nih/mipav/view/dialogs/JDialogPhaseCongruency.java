package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.io.IOException;

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
	
	@SuppressWarnings("rawtypes")
	private JComboBox methodBox;
	
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
		else if(command.equals("Cancel"))
			dispose();
		else if(command.equals("Help"))
			displayHelp();
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
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void init(){
		setTitle("Phase Congruency");
		
		JPanel inputPanel = new JPanel(new BorderLayout());
		inputPanel.setForeground(Color.black);
		inputPanel.setBorder(buildTitledBorder("Input options"));
		
		JPanel numbersPanel = new JPanel(new GridBagLayout());
		numbersPanel.setForeground(Color.black);
		
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(0,0,0,5);
		gbc.gridx = 0;
		gbc.gridy = 0;
		
		JLabel scaleLabel = new JLabel("# Scales");
		scaleLabel.setFont(serif12); 
		scaleLabel.setPreferredSize(new Dimension(90, 25));
		numbersPanel.add(scaleLabel, gbc);
		
		gbc.gridx++;
		
		fields[0].setText("4");
		numbersPanel.add(fields[0], gbc);
		
		gbc.gridx++;
		
		JLabel orientLabel = new JLabel("# Orientations");
		orientLabel.setFont(serif12);
		orientLabel.setPreferredSize(new Dimension(90, 25));
		numbersPanel.add(orientLabel, gbc);
		
		gbc.gridx++;
		
		fields[1].setText("6");
		numbersPanel.add(fields[1], gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		
		JLabel waveLabel = new JLabel("Min. wavelength");
		waveLabel.setFont(serif12);
		waveLabel.setPreferredSize(new Dimension(90, 25));
		numbersPanel.add(waveLabel, gbc);
		
		gbc.gridx++;
		
		fields[2].setText("3");
		numbersPanel.add(fields[2], gbc);
		
		gbc.gridx++;
		
		JLabel multLabel = new JLabel("Scaling factor");
		multLabel.setFont(serif12);
		multLabel.setPreferredSize(new Dimension(90, 25));
		numbersPanel.add(multLabel, gbc);
		
		gbc.gridx++;
		
		fields[3].setText("2.1");
		numbersPanel.add(fields[3], gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		
		JLabel sigmaLabel = new JLabel("Filter sigma");
		sigmaLabel.setFont(serif12);
		sigmaLabel.setPreferredSize(new Dimension(90, 25));
		numbersPanel.add(sigmaLabel, gbc);
		
		gbc.gridx++;
		
		fields[4].setText("0.55");
		numbersPanel.add(fields[4], gbc);
		
		gbc.gridx++;
		
		JLabel kLabel = new JLabel("k");
		kLabel.setFont(serif12);
		kLabel.setPreferredSize(new Dimension(90, 25));
		numbersPanel.add(kLabel, gbc);
		
		gbc.gridx++;
		
		fields[5].setText("2.0");
		numbersPanel.add(fields[5], gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		
		JLabel cutoffLabel = new JLabel("Cutoff");
		cutoffLabel.setFont(serif12);
		cutoffLabel.setPreferredSize(new Dimension(90, 25));
		numbersPanel.add(cutoffLabel, gbc);
		
		gbc.gridx++;
		
		fields[6].setText("0.5");
		numbersPanel.add(fields[6], gbc);
		
		gbc.gridx++;
		
		JLabel gLabel = new JLabel("g");
		gLabel.setFont(serif12);
		gLabel.setPreferredSize(new Dimension(90, 25));
		numbersPanel.add(gLabel, gbc);
		
		gbc.gridx++;
		
		fields[7].setText("10");
		numbersPanel.add(fields[7], gbc);

		inputPanel.add(numbersPanel);
		
		JPanel methodPanel = new JPanel();
		methodPanel.setForeground(Color.black);
		
		JLabel methodLabel = new JLabel("Noise Estimation");
		methodLabel.setFont(serif12);
		methodPanel.add(methodLabel);
		
		String[] methods = new String[]{"Median", "Mode", "Value"};
		
		methodBox = new JComboBox(methods);
		methodBox.setFont(serif12);
		methodBox.setSelectedIndex(0);
		methodBox.addItemListener(this);
		methodPanel.add(methodBox);
		
		fields[8].setText("0");
		fields[8].setEnabled(false);
		methodPanel.add(fields[8]);
		inputPanel.add(methodPanel, BorderLayout.SOUTH);
		
		getContentPane().add(inputPanel, BorderLayout.NORTH);	
		
		JPanel outputPanel = new JPanel(new GridLayout(0,2));
		outputPanel.setForeground(Color.black);
		outputPanel.setBorder(buildTitledBorder("Output options"));
		
		edgeBox = new JCheckBox("Edges");
		edgeBox.setFont(serif12);
		edgeBox.setSelected(true);
		outputPanel.add(edgeBox);
		
		cornerBox = new JCheckBox("Corners");
		cornerBox.setFont(serif12);
		outputPanel.add(cornerBox);
		
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
		
		JDialog helpDialog = new JDialog();
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
				+ "congruency for frequency spread. <br>";
		
		JPanel textPanel = new JPanel();
		textPanel.setForeground(Color.black);
		
		JLabel helpLabel = new JLabel(helpText);
		helpLabel.setFont(serif12);
		textPanel.add(helpLabel);
		helpDialog.getContentPane().add(textPanel);
		
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

}
