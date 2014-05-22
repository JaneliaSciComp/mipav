import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.util.Enumeration;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogNeuronalOverlap extends JDialogBase implements AlgorithmInterface{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1691766330291274023L;
	
	@SuppressWarnings("rawtypes")
	private JComboBox dapiImages;
	
	@SuppressWarnings("rawtypes")
	private JComboBox neunImages;
	
	private JTextField overlapField;
	
	private JTextField dapiThreshField;
	
	private JTextField neunThreshField;
	
	public PlugInDialogNeuronalOverlap(){
		
		try{
		buildImageBox();
		
		if(dapiImages.getModel().getSize()<2){
			MipavUtil.displayError("Not enough images open");
			dispose();
			return;
		}
		
		init();
		} catch(Exception e){
			e.printStackTrace();
		}
	}
	
	public void actionPerformed(ActionEvent action){
		String command = action.getActionCommand();
		
		if(command.equals("OK")){
			callAlgorithm();
		} else if(command.equals("Cancel")){
			dispose();
		}
	}
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub
		if(algorithm.isCompleted()){
			ModelImage result = algorithm.getDestImage();
			new ViewJFrameImage(result);
		} else setVisible(true);
	}
	
	protected void callAlgorithm(){
		String dapiName = (String)dapiImages.getSelectedItem();
		String neunName = (String)neunImages.getSelectedItem();

		if(dapiName.equals(neunName)){
			MipavUtil.displayError("Please select different images");
			return;
		}
		
		String overlapStr = overlapField.getText();
		int overlapPct = 0;
		try{
			overlapPct = Integer.valueOf(overlapStr);
		}catch(NumberFormatException e){
			MipavUtil.displayError("Overlap % is not a number");
			return;
		}
		
		String dapiThreshStr = dapiThreshField.getText();
		int dapiThresh = 4;
		try{
			dapiThresh = Integer.valueOf(dapiThreshStr);
		}catch(NumberFormatException e){
			MipavUtil.displayError("DAPI threshold is not a number");
			return;
		}
		
		String neunThreshStr = neunThreshField.getText();
		int neunThresh = 65;
		try{
			neunThresh = Integer.valueOf(neunThreshStr);
		}catch(NumberFormatException e){
			MipavUtil.displayError("DAPI threshold is not a number");
			return;
		}
		
		ViewUserInterface UI = ViewUserInterface.getReference();
		ModelImage dapiImage = UI.getRegisteredImageByName(dapiName);
		ModelImage neunImage = UI.getRegisteredImageByName(neunName);
		
		int[] dapiExt = dapiImage.getExtents();
		int[] neunExt = neunImage.getExtents();
		if(dapiExt[0] != neunExt[0] || dapiExt[1] != neunExt[1]){
			MipavUtil.displayError("Dimensions do not match");
			return;
		}
		
		PlugInAlgorithmNeuronalOverlap alg = new PlugInAlgorithmNeuronalOverlap(dapiImage, neunImage, overlapPct);
		alg.setThresholds(dapiThresh, neunThresh);
		alg.addListener(this);
		if (isRunInSeparateThread()) {
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
				return;
			}
		} else {
			alg.run();
		}
		
		setVisible(false);
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void buildImageBox(){
		
		ViewUserInterface UI = ViewUserInterface.getReference();

		Enumeration<String> names = UI.getRegisteredImageNames();
		
        dapiImages = new JComboBox();
		neunImages = new JComboBox();

		while (names.hasMoreElements()){

			String name = names.nextElement();
			ModelImage img = UI.getRegisteredImageByName(name);
			
			if (UI.getFrameContainingImage(img) != null) {
				dapiImages.addItem(name);
				neunImages.addItem(name);
				
			}	
		}
	}
	
	private void init(){
		
		getContentPane().removeAll();
		
		setTitle("Neuronal Overlap");
		
		PanelManager manage = new PanelManager();
		
		JPanel dapiPanel = new JPanel();
		dapiPanel.setForeground(Color.black);
		
		JLabel dapiLabel = new JLabel("DAPI image");
		dapiLabel.setFont(serif12);
		dapiPanel.add(dapiLabel);
		dapiPanel.add(dapiImages);
		
		getContentPane().add(dapiPanel, BorderLayout.NORTH);
		
		JPanel neunPanel = new JPanel();
		neunPanel.setForeground(Color.black);
		
		JLabel neunLabel = new JLabel("NeuN image");
		neunLabel.setFont(serif12);
		neunPanel.add(neunLabel);
		neunPanel.add(neunImages);
		
		JPanel overlapPanel = new JPanel();
		overlapPanel.setForeground(Color.black);
		
		JLabel overlapLabel = new JLabel("Overlap: ");
		overlapLabel.setFont(serif12);
		overlapPanel.add(overlapLabel);
		
		overlapField = new JTextField(4);
		overlapField.setText("0");
		overlapField.setHorizontalAlignment(JTextField.RIGHT);
		overlapField.setFont(serif12);
		overlapPanel.add(overlapField);
		
		JLabel pctLabel = new JLabel("%");
		pctLabel.setFont(serif12);
		overlapPanel.add(pctLabel);
		
		JPanel dapiThreshPanel = new JPanel();
		dapiThreshPanel.setForeground(Color.black);
		
		JLabel dapiThreshLabel = new JLabel("DAPI Threshold");
		dapiThreshLabel.setFont(serif12);
		dapiThreshPanel.add(dapiThreshLabel);
		
		dapiThreshField = new JTextField(4);
		dapiThreshField.setText("4");
		dapiThreshField.setFont(serif12);
		dapiThreshPanel.add(dapiThreshField);
		
		JPanel neunThreshPanel = new JPanel();
		neunThreshPanel.setForeground(Color.black);
		
		JLabel neunThreshLabel = new JLabel("NeuN Threshold");
		neunThreshLabel.setFont(serif12);
		neunThreshPanel.add(neunThreshLabel);
		
		neunThreshField = new JTextField(4);
		neunThreshField.setText("65");
		neunThreshField.setFont(serif12);
		neunThreshPanel.add(neunThreshField);
		
		manage.add(neunPanel);
		manage.addOnNextLine(overlapPanel);
		manage.addOnNextLine(dapiThreshPanel);
		manage.addOnNextLine(neunThreshPanel);
		
		getContentPane().add(manage.getPanel(), BorderLayout.CENTER);
		
		getContentPane().add(buildOKCancelButtons(), BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
		System.gc();
	}
}
