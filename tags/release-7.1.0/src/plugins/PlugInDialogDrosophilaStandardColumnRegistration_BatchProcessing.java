import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.LineBorder;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ScrollCorrector;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogDrosophilaStandardColumnRegistration_BatchProcessing extends JDialogBase implements AlgorithmInterface {
	
	
	 private JTextField batchFilePathTextField;
	 
	 private JButton browseButton;
	
	 private JLabel batchLabel;
	 
	 private JPanel mainPanel;
	 
	 private GridBagConstraints gbc;
	 
	 private PlugInDialogDrosophilaStandardColumnRegistration plugin;
	 
	 private JTextArea batchOutputTextArea;
	 
	 private JScrollPane scrollPane;
	 
	 private File batchFile;
	 
	 ArrayList <String[]> plugInData = new ArrayList <String[]>();
	 
	 
	

	
	/**
	 * constructor
	 */
	public PlugInDialogDrosophilaStandardColumnRegistration_BatchProcessing() {
		
	}

	/**
	 * constructor
	 * @param modal
	 */
	public PlugInDialogDrosophilaStandardColumnRegistration_BatchProcessing(boolean modal) {
		super(modal);
		init();
	}
	
	
	
	
	
	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("Drosophila Standard Column Registration - Batch Processing   v2.1");
        
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        
        batchLabel = new JLabel("Batch File ");
        batchFilePathTextField = new JTextField(35);
        batchFilePathTextField.setEditable(false);
        batchFilePathTextField.setBackground(Color.white);
        browseButton = new JButton("Browse");
        browseButton.addActionListener(this);
        browseButton.setActionCommand("browse");
        
        
        
        batchOutputTextArea = new JTextArea();
        batchOutputTextArea.setRows(15);
        batchOutputTextArea.setEditable(false);
        batchOutputTextArea.setBackground(Color.lightGray);
        batchOutputTextArea.setBorder(new LineBorder(Color.black));
        batchOutputTextArea.setForeground(Color.black);
		scrollPane = new JScrollPane(batchOutputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
		scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
        
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15,5,5,15);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(batchLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(batchFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(browseButton,gbc);
        
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;
        
        gbc.gridy = 1;
        gbc.gridx = 0;
        gbc.gridwidth = 3;
        mainPanel.add(scrollPane,gbc);

        JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        
        pack();
        setMinimumSize(getSize());
        
        setVisible(true);
        setResizable(false);
        
	}
	
	
	

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}
	
	
	
	
	private boolean readBatchFile() {
		boolean success = true;
		String[] arr;
        String line;
		
		RandomAccessFile raFile = null;
		
		try {
			raFile = new RandomAccessFile(batchFile, "r");
			while((line = raFile.readLine()) != null) {
        		arr = line.trim().split(",");
        		plugInData.add(arr);
        		
			}
			
			
		}catch(Exception e) {
			e.printStackTrace();
			return false;
		}
		
		
		
		return success;
	}
	
	

	


	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("browse")) {
			 JFileChooser chooser = new JFileChooser();

		        chooser.setDialogTitle("Choose batch file");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	String currDir = chooser.getSelectedFile().getAbsolutePath();
		        	batchFile = new File(currDir);
		        	boolean success = readBatchFile();
		        	if(success) {
		        		batchFilePathTextField.setText(currDir);
		        	}else {
		        		MipavUtil.displayError("Error reading batch file");
		        		batchFilePathTextField.setText("");
		        	}
		        	
		        }
		 }else if(command.equals("ok")) {
			 if(batchFilePathTextField.getText().equals("")) {
				 MipavUtil.displayError("Batch file has not been read in yet");
				 return;
			 }
			 //for(int i=0;i<plugInData.size();i++) {
				 
				 Thread t = new StartDialog();
				 
				 try {
	    	    		t.start();
	    	    	}catch (Exception ex) {
	    				ex.printStackTrace();
	    				return;
	    			}
	    	    	
	    	    	
				 
				 
				 
				 
			 //}
			 
			 
			 
			 
			 
			 
			 
		 }else if(command.equals("cancel")) {
			 dispose();
		 } else {
             super.actionPerformed(e);
         }

	}
	
	
	
	public class StartDialog extends Thread {
		
		public void run() {
			
			 for(int i=0;i<plugInData.size();i++) {
			
				plugin = new PlugInDialogDrosophilaStandardColumnRegistration(false);
				 if(setVariables(plugInData.get(i))) {
					 batchOutputTextArea.append("Running plugin on " +  plugInData.get(i)[0] + "... \n");
					 plugin.callAlgorithm();
					 while(!plugin.isPlugInCompleted()) {
						 //do nothing
					 }
					 if(plugin.neuronImage != null) {
						 plugin.neuronImage.disposeLocal();
						 plugin.neuronImage = null;
					 }
					 plugin.dispose();
					 
					 System.gc();
				 }
			 }
			 
			 batchOutputTextArea.append("Done \n");
		}
		
		
		
		private boolean setVariables(String[] vars) {
			boolean success = true;
			
			plugin.imageFilePathTextField.setText(vars[0]);
			FileIO fileIO = new FileIO();
			String replaced = vars[0].replace("\\", File.separator);
			String name = vars[0].substring(vars[0].lastIndexOf(File.separator)+1, vars[0].length());
			String dir = vars[0].substring(0,vars[0].lastIndexOf(File.separator));
			
			plugin.neuronImage = fileIO.readImage(name, dir + File.separator, true, null);
	    	plugin.resols = plugin.neuronImage.getResolutions(0);
			
			
			
			
			
			
			
			
			
	    	replaced = vars[1].replace("\\", File.separator);
			name = vars[1].substring(vars[1].lastIndexOf(File.separator)+1, vars[1].length());
			dir = vars[1].substring(0,vars[1].lastIndexOf(File.separator));
			plugin.pointsFile = new File(replaced);
			if(!plugin.readPointsFile(plugin.pointsFile)) {
	    		MipavUtil.displayError("Error parsing points file");
	    		return false;
			}
			
			plugin.pointsFilePathTextField.setText(vars[1]);
			

	    	
	    	
			
			replaced = vars[2].replace("\\", File.separator);
			name = vars[2].substring(vars[2].lastIndexOf(File.separator)+1, vars[2].length());
			dir = vars[2].substring(0,vars[2].lastIndexOf(File.separator));
			plugin.surfaceFile = new File(replaced);
	    	if(!plugin.readSurfaceFile(plugin.surfaceFile)) {
	    		
	    		MipavUtil.displayError("Error parsing surface file");

	    		return false;
	    		
	    	}else {
	    		if(plugin.determineIfProperlyConnected()) {
	    			plugin.createCityBlockImage();
	    		}else {
	    			MipavUtil.displayError("Error parsing surface file");

	        		return false;
	    		}
	    		
	    		

	    		
	    	}
			plugin.surfaceFilePathTextField.setText(vars[2]);
			
			
			
			if(Float.valueOf(vars[3]) == 0.125) {
				plugin.surfaceFileSamplingCB.setSelectedIndex(3);
			}else if(Float.valueOf(vars[3]) == 0.25) {
				plugin.surfaceFileSamplingCB.setSelectedIndex(2);
			}else if(Float.valueOf(vars[3]) == 0.5) {
				plugin.surfaceFileSamplingCB.setSelectedIndex(1);
			}else if(Float.valueOf(vars[3]) == 1.0) {
				plugin.surfaceFileSamplingCB.setSelectedIndex(0);
			}
			
			
			if(vars[4].equalsIgnoreCase("flipNone")) {
				plugin.flipXCB.setSelected(false);
				plugin.flipYCB.setSelected(false);
				plugin.flipZCB.setSelected(false);
			}else if (vars[4].equalsIgnoreCase("flipX")) {
				plugin.flipXCB.setSelected(true);
				plugin.flipYCB.setSelected(false);
				plugin.flipZCB.setSelected(false);
			}else if (vars[4].equalsIgnoreCase("flipY")) {
				plugin.flipXCB.setSelected(false);
				plugin.flipYCB.setSelected(true);
				plugin.flipZCB.setSelected(false);
			}else if (vars[4].equalsIgnoreCase("flipZ")) {
				plugin.flipXCB.setSelected(false);
				plugin.flipYCB.setSelected(false);
				plugin.flipZCB.setSelected(true);
			}else if (vars[4].equalsIgnoreCase("flipXY")) {
				plugin.flipXCB.setSelected(true);
				plugin.flipYCB.setSelected(true);
				plugin.flipZCB.setSelected(false);
			}else if (vars[4].equalsIgnoreCase("flipXZ")) {
				plugin.flipXCB.setSelected(true);
				plugin.flipYCB.setSelected(false);
				plugin.flipZCB.setSelected(true);
			}else if (vars[4].equalsIgnoreCase("flipYZ")) {
				plugin.flipXCB.setSelected(false);
				plugin.flipYCB.setSelected(true);
				plugin.flipZCB.setSelected(true);
			}else if (vars[4].equalsIgnoreCase("flipXYZ")) {
				plugin.flipXCB.setSelected(true);
				plugin.flipYCB.setSelected(true);
				plugin.flipZCB.setSelected(true);
			}
			
			/*if(vars[5].equalsIgnoreCase("27_points")) {
				plugin._27PointsRadio.setSelected(true);
				plugin._75PointsRadio.setSelected(false);
				plugin._147PointsRadio.setSelected(false);
			}else if(vars[5].equalsIgnoreCase("75_points")) {
				plugin._27PointsRadio.setSelected(false);
				plugin._75PointsRadio.setSelected(true);
				plugin._147PointsRadio.setSelected(false);
			}else if(vars[5].equalsIgnoreCase("147_points")) {
				plugin._27PointsRadio.setSelected(false);
				plugin._75PointsRadio.setSelected(false);
				plugin._147PointsRadio.setSelected(true);
			}*/
			
			
			if(vars[5].equalsIgnoreCase("27A_points")) {
				plugin._27APointsRadio.setSelected(true);
				plugin._75APointsRadio.setSelected(false);
			}else if(vars[5].equalsIgnoreCase("75A_points")) {
				plugin._27APointsRadio.setSelected(false);
				plugin._75APointsRadio.setSelected(true);
			}
			
			/*if(plugin._27PointsRadio.isSelected()) {
				plugin.setNumPoints(PlugInDialogDrosophilaStandardColumnRegistration._27POINTS);
			}else if (plugin._75PointsRadio.isSelected()) {
				plugin.setNumPoints(PlugInDialogDrosophilaStandardColumnRegistration._75POINTS);
			}else if (plugin._147PointsRadio.isSelected()) {
				plugin.setNumPoints(PlugInDialogDrosophilaStandardColumnRegistration._147POINTS);
			}*/
			
			if(plugin._27APointsRadio.isSelected()) {
				plugin.setNumPointsString(PlugInDialogDrosophilaStandardColumnRegistration._27APOINTS);
			}else if (plugin._75APointsRadio.isSelected()) {
				plugin.setNumPointsString(PlugInDialogDrosophilaStandardColumnRegistration._75APOINTS);
			}
			
			
			
			
			
			
			if(vars[6].equalsIgnoreCase("lvrd")) {
				plugin.lvrdRadio.setSelected(true);
				plugin.rvldRadio.setSelected(false);
			}else if(vars[6].equalsIgnoreCase("rvld")) {
				plugin.lvrdRadio.setSelected(false);
				plugin.rvldRadio.setSelected(true);
			}
			
			if(vars[7].equalsIgnoreCase("rigidTPS")) {
				plugin.rigTPSRadio.setSelected(true);
				plugin.rigRadio.setSelected(false);
			}else if(vars[7].equalsIgnoreCase("rigidOnly")) {
				plugin.rigTPSRadio.setSelected(false);
				plugin.rigRadio.setSelected(true);
			}
			
			
			if(vars.length > 8) {
				plugin.greenValueRadiusThresholdTextField.setText(vars[8]);
				plugin.greenThreshold = Float.valueOf(vars[8]).floatValue();
				plugin.subsamplingDistanceTextField.setText(vars[9]);
				plugin.subsamplingDistance = Float.valueOf(vars[9]).floatValue();
				plugin.swcCB.setSelected(true);
				plugin.doSWC = true;
				
			}
			
			
			return success;
		}
	}

}
