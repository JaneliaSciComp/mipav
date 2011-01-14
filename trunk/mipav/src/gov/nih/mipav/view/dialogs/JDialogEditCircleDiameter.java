package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManager;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public class JDialogEditCircleDiameter extends JDialogScriptableBase {
	
	/** button group for radio buttons **/
    private ButtonGroup optionsGroup;
   
    private JRadioButton pixelRadio;

    private JRadioButton spaceRadio;
	
    private JTextField pixelField;
	
    private JTextField spaceField;
    
    private JLabel pixelLabel;
    
    private JLabel spaceLabel;
    
    private String widthString, measuredWidthString, unitsString;
    
    private VOIBase voi;
    
    private VOIManager voiManager;
    
    private float[] res;
    
    private Component parentComponent;
    
	
	
	public JDialogEditCircleDiameter(Component parentComponent, String widthString, String measuredWidthString, String unitsString, float[] res, VOIBase voi, VOIManager voiManager) {
		this.parentComponent = parentComponent;
		this.widthString = widthString;
		this.measuredWidthString = measuredWidthString;
		this.unitsString = unitsString;
		this.res = res;
		this.voi = voi;
		this.voiManager = voiManager;
		init();
	}
	
	
	private void init() {
	  setTitle("Enter new diameter");
	  JPanel mainPanel = new JPanel(new GridBagLayout());
	  GridBagConstraints gbc = new GridBagConstraints();
	  
	  optionsGroup = new ButtonGroup();
		
	  pixelRadio = new JRadioButton("");
	  pixelRadio.setSelected(true);
	  pixelRadio.addActionListener(this);
	  pixelRadio.setActionCommand("pixelRadio");
	  optionsGroup.add(pixelRadio);
			
	  spaceRadio = new JRadioButton("");
	  spaceRadio.setSelected(false);
	  spaceRadio.addActionListener(this);
	  spaceRadio.setActionCommand("spaceRadio");
	  optionsGroup.add(spaceRadio);	
	  
	  pixelField = new JTextField(5);
	  pixelField.setEnabled(true);
	  pixelField.setText(widthString);
	  
	  spaceField = new JTextField(5);
	  spaceField.setEnabled(false);
	  spaceField.setText(measuredWidthString);
	  
	  pixelLabel = new JLabel("pixels");
	  spaceLabel = new JLabel(unitsString);
	  spaceLabel.setEnabled(false);
	  
	  
	  gbc.gridx = 0;
	  gbc.gridy = 0;
	  gbc.insets = new Insets(15,5,15,0);
	  gbc.gridwidth = 3;
      JLabel topLabel = new JLabel("Enter new diameter : ");
      mainPanel.add(topLabel, gbc);
      
      gbc.gridx = 0;
	  gbc.gridy = 1;
	  gbc.insets = new Insets(15,5,15,0);
	  gbc.gridwidth = 1;
	  mainPanel.add(pixelRadio, gbc);
	  
	  gbc.gridx = 1;
	  gbc.gridy = 1;
	  gbc.insets = new Insets(15,5,15,0);
	  gbc.gridwidth = 1;
	  mainPanel.add(pixelField, gbc);
	  
	  gbc.gridx = 2;
	  gbc.gridy = 1;
	  gbc.insets = new Insets(15,5,15,5);
	  gbc.gridwidth = 1;
	  mainPanel.add(pixelLabel, gbc);
	  
	  gbc.gridx = 0;
	  gbc.gridy = 2;
	  gbc.insets = new Insets(15,5,15,0);
	  gbc.gridwidth = 1;
	  mainPanel.add(spaceRadio, gbc);
	  
	  gbc.gridx = 1;
	  gbc.gridy = 2;
	  gbc.insets = new Insets(15,5,15,0);
	  gbc.gridwidth = 1;
	  mainPanel.add(spaceField, gbc);
	  
	  gbc.gridx = 2;
	  gbc.gridy = 2;
	  gbc.insets = new Insets(15,5,15,5);
	  gbc.gridwidth = 1;
	  mainPanel.add(spaceLabel, gbc);
	  
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
      setVisible(true);
      
      MipavUtil.centerInComponent(parentComponent, this);
		
	}
	

	@Override
	protected void callAlgorithm() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub

	}

	@Override
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equals("pixelRadio")) {
			pixelField.setEnabled(true);
			pixelLabel.setEnabled(true);
			spaceField.setEnabled(false);
			spaceLabel.setEnabled(false);
		}else if(command.equals("spaceRadio")) {
			spaceField.setEnabled(true);
			spaceLabel.setEnabled(true);
			pixelField.setEnabled(false);
			pixelLabel.setEnabled(false);
		}else if(command.equals("cancel")) {
			this.dispose();
		}else if(command.equals("ok")) {
			if(editCircleDiameter()) {
				this.dispose();
			}
		}
	}
	
	private boolean editCircleDiameter() {
		
		float radius = 0;
		try {
			if(pixelRadio.isSelected()) {
				float diameter = Float.valueOf(pixelField.getText().trim()).floatValue();
				radius = diameter/2;
			}else {
				float diameter = Float.valueOf(spaceField.getText().trim()).floatValue()/res[0];
				radius = diameter/2;
				
			}
		}catch(NumberFormatException e) {
			MipavUtil.displayError("Invalid number(s) entered");
			return false;
		}
		voiManager.editCircleDiameter(voi, radius);
		return true;
	}

}
