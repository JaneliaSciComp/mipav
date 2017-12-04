package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

/**
 * The class reads the nii binary masks and generates the comparison list for shell script. 
 * The "EvaluateSegmentation" software will run the script to generate the performance data. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateEvaluationSegmentation extends JDialogBase implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;


	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<String, Hashtable<String, Hashtable<String, String>>> fileNameTable =
			new Hashtable<String, Hashtable<String, Hashtable<String, String>>>();
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateEvaluationSegmentation(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
		setVisible(true);

	}

	/**
	 * dispose memory
	 * */
	public void disposeLocal() {

	}

	/**
	 * Dialog local actionPerformed handler.
	 */
	public void actionPerformed(ActionEvent event) {

		String command = event.getActionCommand();
		if (command.equals("OK")) {
			callAlgorithm();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			
		} else if (command.equals("ChooseKeyImageDir")) {
			readKeyImageDir();
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}
	

	/**
	 * Let user specify the saved 2D slices atlas, record the save directory.
	 */
	private void recordSaveImageDir() {
		String saveImageName;
		saveImageChooser.setDialogTitle("Open Saved Images Directory");
		saveImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = saveImageChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			saveImageName = saveImageChooser.getSelectedFile().getName();

			saveImageDirectory = String.valueOf(saveImageChooser.getCurrentDirectory()) + File.separatorChar + saveImageName + File.separatorChar;
			textFieldSaveImage.setText(saveImageDirectory);

		} else {
			return;
		}
	}

	/**
	 * Read 3D images atlas directory.
	 */
	private void readKeyImageDir() {
		File fileDir_1 = new File("/scratch/ISBI2017/result/fold4");
		traverse_Layer(fileDir_1);
	}

	
	
	private void traverse_Layer(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				fileNameTable.put(children[i], new Hashtable<String, Hashtable<String, String>>());
			    traverse_firstLayer(dir, children[i]);
			}
			
		}
	}

	private void traverse_firstLayer(File firstDir, String orientation) {
		
		File firstLayer = new File(firstDir, orientation);
		traverse_secondLayer(firstLayer, orientation);

	}

	

	private void printNIItable() {
		
		Set<String> orientationKeys = fileNameTable.keySet();
		
	    for ( String orientationKey : orientationKeys ) {
	    	
	    	Hashtable<String, Hashtable<String, String>> hashIDtable = fileNameTable.get(orientationKey);
	    	
	    	String saveShellScript = saveImageDirectory + File.separator + orientationKey + File.separator +
	    			   (orientationKey + ".sh");
	    	String saveShellScriptDir = saveImageDirectory + File.separator + orientationKey + File.separator;
	    	
	    	try {
	    	PrintWriter fileWriter = new PrintWriter(new FileWriter(saveShellScript));
	    	
	    	Set<String> hashIDKeys = hashIDtable.keySet();
	    	
	    	for ( String hashIDKey : hashIDKeys ) {
	    		String gtName = fileNameTable.get(orientationKey).get(hashIDKey).get("gt");
	    		String segName = fileNameTable.get(orientationKey).get(hashIDKey).get("seg");
	    		fileWriter.write("EvaluateSegmentation" + " " + gtName + " " + segName + " " +
	    				"-use all" + " " + 
	    				"-xml" + " " + saveShellScriptDir + "result_" + hashIDKey + ".xml" +
	    				"\n");
	    	}
	    
	    	fileWriter.close();
	    	} catch (IOException e) {
				e.printStackTrace();
			}
	    }
	        
	}
	
	private void traverse_nii(File secondLayer, String orientation, String hashID ) {
		
		String[] children = secondLayer.list();
		for ( int i = 0; i < children.length; i++ ) {
			
			if ( children[i].startsWith("gt") && children[i].endsWith(".nii")) {
				fileNameTable.get(orientation).get(hashID).put("gt",
						secondLayer.getAbsolutePath() +
						File.separator + children[i]);
				// System.err.println(secondLayer.getAbsolutePath() +
				//		File.separator + children[i]);
			}
			
			if ( children[i].startsWith("seg") && children[i].endsWith(".nii")) {
				fileNameTable.get(orientation).get(hashID).put("seg",
						secondLayer.getAbsolutePath() +
						File.separator + children[i]);
				 // System.err.println(secondLayer.getAbsolutePath() +
					//	File.separator + children[i]);
			}
			
		}
	}
	
	
	private void traverse_secondLayer(File firstLayer, String orientation) {

		String[] children = firstLayer.list();

		for (int i = 0; i < children.length; i++) {

			File dir = new File(firstLayer, children[i]);
			
			if ( dir.isDirectory() ) {
				fileNameTable.get(orientation).put(children[i], new Hashtable<String, String>());
				traverse_nii(dir, orientation, children[i]);
			}
			
		}
		
	}
	
	/**
	 * This method is required if the AlgorithmPerformed interface is
	 * implemented. It is called by the algorithms when it has completed or
	 * failed to to complete, so that the dialog can be display the result image
	 * and/or clean up.
	 * 
	 * @param algorithm
	 *            Algorithm that caused the event.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {

	}

	/**
	 * Driver function to read image and VOIs, and convert each 3D image to 2D
	 * slices.
	 */
	public void callAlgorithm() {
		
		long startTime = System.currentTimeMillis();
		
		printNIItable();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");


	}


	
	/**
	 * Initial panel
	 */
	public void init() {

		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		buildKeyImagePanel();

		mainPanel.add(imageSelectionPanel, BorderLayout.CENTER);
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);
	}

	/**
	 * Panel contains both the 3D image dir and saved 2D slices atlas dir.
	 */
	public void buildKeyImagePanel() {

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		imageSelectionPanel = new JPanel();
		imageSelectionPanel.setLayout(new GridLayout(2, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Auto Train"));


		// Key image directory
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelKeyImage = new JLabel("Key Image Directory: ");
		labelKeyImage.setFont(serif12);
		labelKeyImage.setForeground(Color.black);

		imageSelectionPanel.add(labelKeyImage, gbc);

		textFieldKeyImage = new JTextField(20);
		textFieldKeyImage.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldKeyImage, gbc);

		buttonKeyImage = new JButton("Choose");
		buttonKeyImage.addActionListener(this);
		buttonKeyImage.setActionCommand("ChooseKeyImageDir");
		buttonKeyImage.setFont(serif12B);
		buttonKeyImage.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonKeyImage, gbc);

		// Save image directory
		gbc.gridx = 0;
		gbc.gridy = 2;
		labelSaveImage = new JLabel("Saved Image Directory: ");
		labelSaveImage.setFont(serif12);
		labelSaveImage.setForeground(Color.black);

		imageSelectionPanel.add(labelSaveImage, gbc);

		textFieldSaveImage = new JTextField(20);
		textFieldSaveImage.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldSaveImage, gbc);

		buttonSaveImage = new JButton("Choose");
		buttonSaveImage.addActionListener(this);
		buttonSaveImage.setActionCommand("ChooseSaveImageDir");
		buttonSaveImage.setFont(serif12B);
		buttonSaveImage.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonSaveImage, gbc);

	}

}