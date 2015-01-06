import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.filechooser.FileFilter;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;


public class PlugInDialogSWCVolume extends JDialogStandalonePlugin
		implements AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4480769994405721958L;

	private static int CHOOSE_IMAGE = 0;
	
	private static int CHOOSE_FIL = 1;
	
	private JTextField imageText;
	
	private JTextField filText;
	
	private JTextField volumeText;
	
	private ModelImage srcImage;
	
	public PlugInDialogSWCVolume(){
		
		super();
		
		init();
		
	}
	
	public PlugInDialogSWCVolume(Frame parentFrame, ModelImage image){
		super(parentFrame, false);
		
		srcImage = image;
		
		initAlg();
	}
	
	public void actionPerformed(ActionEvent e){
		String command = e.getActionCommand();
		
		if(command.equals("chooseIm")){
			chooser(CHOOSE_IMAGE);
		}else if(command.equals("chooseFil")){
			chooser(CHOOSE_FIL);
		}else if(command.equals("OK")){
			callAlgorithm();
		}else if(command.equals("Cancel")){
			if (isExitRequired()) {
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		}
	}
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(algorithm instanceof PlugInAlgorithmSWCVolume){
			PlugInAlgorithmSWCVolume alg = (PlugInAlgorithmSWCVolume) algorithm;
			volumeText.setText(alg.getVolume() + " um2");
		}
	}
	
	protected void callAlgorithm(){
		PlugInAlgorithmSWCVolume alg;
		
		if(srcImage == null){
			alg = new PlugInAlgorithmSWCVolume(
					imageText.getText(), filText.getText());
		}else{
			alg = new PlugInAlgorithmSWCVolume(
					srcImage, filText.getText());
		}
		alg.addListener(this);
		if (isRunInSeparateThread()) {
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
	}
	
	private void initAlg(){
		
		getContentPane().removeAll();
		
		JPanel topPanel = new JPanel(new GridBagLayout());
		topPanel.setForeground(Color.black);
		
		JLabel filLabel = new JLabel("Select filament");
		filLabel.setFont(serif12B);
		
		filText = new JTextField(30);
		filText.setFont(serif12);
		
		JButton filButton = new JButton("Choose");
		filButton.setActionCommand("chooseFil");
		filButton.addActionListener(this);
		filButton.setFont(serif12);
		
		GridBagConstraints gbc = new GridBagConstraints();
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(0, 5, 0, 5);
		
		topPanel.add(filLabel, gbc);
		gbc.gridy++;
		gbc.gridwidth = 4;
		topPanel.add(filText, gbc);
		gbc.gridx = 4;
		gbc.gridwidth = 1;
		topPanel.add(filButton, gbc);
		
		getContentPane().add(topPanel, BorderLayout.NORTH);
		
		buildOKCancelButtons();
		
		JPanel bottomPanel = new JPanel();
		bottomPanel.setForeground(Color.black);
		
		bottomPanel.add(OKButton);
		bottomPanel.add(cancelButton);
		
		getContentPane().add(bottomPanel, BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
		System.gc();
	}

	private void init(){
		
		getContentPane().removeAll();
		
		JPanel topPanel = new JPanel(new GridBagLayout());
		topPanel.setForeground(Color.black);
		
		JLabel imageLabel = new JLabel("Select image");
		imageLabel.setFont(serif12B);
		
		imageText = new JTextField(30);
		imageText.setFont(serif12);
		
		JButton imageButton = new JButton("Choose");
		imageButton.setActionCommand("chooseIm");
		imageButton.addActionListener(this);
		imageButton.setFont(serif12);
		
		JLabel filLabel = new JLabel("Select filament");
		filLabel.setFont(serif12B);
		
		filText = new JTextField(30);
		filText.setFont(serif12);
		
		JButton filButton = new JButton("Choose");
		filButton.setActionCommand("chooseFil");
		filButton.addActionListener(this);
		filButton.setFont(serif12);
		
		JLabel volumeLabel = new JLabel("Volume");
		volumeLabel.setFont(serif12B);
		
		volumeText = new JTextField(15);
		volumeText.setEditable(false);
		volumeText.setFont(serif12);
		
		GridBagConstraints gbc = new GridBagConstraints();
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(0, 5, 0, 5);
		
		topPanel.add(imageLabel, gbc);
		gbc.gridy++;
		gbc.gridwidth = 4;
		topPanel.add(imageText, gbc);
		gbc.gridx = 4;
		gbc.gridwidth = 1;
		topPanel.add(imageButton, gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		topPanel.add(filLabel, gbc);
		gbc.gridy++;
		gbc.gridwidth = 4;
		topPanel.add(filText, gbc);
		gbc.gridx = 4;
		gbc.gridwidth = 1;
		topPanel.add(filButton, gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		topPanel.add(volumeLabel, gbc);
		gbc.gridy++;
		gbc.gridwidth = 2;
		topPanel.add(volumeText, gbc);
		
		getContentPane().add(topPanel, BorderLayout.NORTH);
		
		buildOKCancelButtons();
		
		JPanel bottomPanel = new JPanel();
		bottomPanel.setForeground(Color.black);
		
		bottomPanel.add(OKButton);
		bottomPanel.add(cancelButton);
		
		getContentPane().add(bottomPanel, BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
		System.gc();
		
		
	}
	
	private void chooser(final int type){
		
		final JFileChooser chooser = new JFileChooser(Preferences.getImageDirectory());
		chooser.addActionListener(new ActionListener(){

			@Override
			public void actionPerformed(ActionEvent e) {
				String command = e.getActionCommand();
				if(command.equals(JFileChooser.APPROVE_SELECTION)){
					File chosenFile = chooser.getSelectedFile();
					String fileStr = chosenFile.getAbsolutePath();
					Preferences.setImageDirectory(chosenFile);
					
					if(type == CHOOSE_IMAGE){
						imageText.setText(fileStr);
					}else if(type == CHOOSE_FIL){
						filText.setText(fileStr);
					}
				}
			}
			
		});
		
		FileFilter filter = new FileFilter(){

			@Override
			public boolean accept(File f) {
				if(type == CHOOSE_IMAGE){
					if(f.isDirectory() || f.getName().toLowerCase().endsWith(".ics"))
						return true;
				}else if(type == CHOOSE_FIL){
					if(f.isDirectory() || f.getName().toLowerCase().endsWith(".iv"))
						return true;
				}
				return false;
			}

			@Override
			public String getDescription() {
				if(type == CHOOSE_IMAGE){
					return "Imaris image (.ics)";
				} else if(type == CHOOSE_FIL){
					return "Imaris filament (.iv)";
				}
				return null;
			}
			
		};
		
		chooser.addChoosableFileFilter(filter);
		chooser.setFileFilter(filter);
		chooser.showOpenDialog(this);
		
		
	}
	
}
