import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.filechooser.FileFilter;

import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;


public class PlugInDialogPhenXToCDE extends JDialogStandalonePlugin {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1307622422039985285L;

	private JTextField dataField;
	
	private JTextField idField;
	
	public PlugInDialogPhenXToCDE(){
		super(false);
		
		init();
	}
	
	public void actionPerformed(ActionEvent event){
		String command = event.getActionCommand();
		if(command.equals("OK")){
			callAlgorithm();
		}else if(command.equals("Cancel")){
			if(isExitRequired()){
				ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
			}else{
				dispose();
			}
		}else if(command.startsWith("Choose")){
			String type = command.replace("Choose", "");
			chooseDir(type);
		}
	}
	
	protected void callAlgorithm(){
		File dataFile = new File(dataField.getText());
		
		if(!(dataFile.exists() || dataFile.isDirectory())){
			MipavUtil.displayError("Please select a proper data file");
			return;
		}
		
		File idFile = new File(idField.getText());
		
		if(!(idFile.exists() || idFile.isDirectory())){
			MipavUtil.displayError("Please select a proper data file");
			return;
		}
		
		PlugInAlgorithmPhenXToCDE alg = new PlugInAlgorithmPhenXToCDE(dataFile, idFile);
		
		if(isRunInSeparateThread()){
			if(!alg.startMethod(Thread.MIN_PRIORITY)){
				MipavUtil.displayError("A thread is already running on this object");
			}else{
				alg.run();
			}
		}
		
	}
	
	private void chooseDir(final String type){
		JFileChooser chooser = new JFileChooser(Preferences.getImageDirectory());
		chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		
		final JTextField field;
		if(type.equals("Data")){
			field = dataField;
		}else{
			field = idField;
		}
		
		FileFilter filter = new FileFilter(){

			@Override
			public boolean accept(File f) {
				if(f.isDirectory())
					return true;
				int extInd = f.getName().lastIndexOf(".");
				if(extInd > -1){
					String ext = f.getName().substring(extInd).toLowerCase();
					if(ext.equals(".csv"))
						return true;
				}
				return false;
			}

			@Override
			public String getDescription() {
				return "CSVs (.csv)";
			}
			
		};
		
		chooser.addChoosableFileFilter(filter);
		chooser.setFileFilter(filter);
		
		chooser.setDialogTitle("Select " + type + " file");
		final int choice = chooser.showOpenDialog(this);
		if(choice == JFileChooser.APPROVE_OPTION){
			File selected = chooser.getSelectedFile();
			field.setText(selected.getAbsolutePath());
			Preferences.setImageDirectory(selected);
		}
		
	}
	
	private void init(){
		
		JPanel topPanel = new JPanel(new GridBagLayout());
		topPanel.setForeground(Color.black);
		
		JLabel dataLabel = new JLabel("PhenX data file");
		dataLabel.setFont(serif12B);
		
		dataField = new JTextField(30);
		dataField.setFont(serif12);
		
		JButton dataButton = new JButton("Choose");
		dataButton.setFont(serif12);
		dataButton.addActionListener(this);
		dataButton.setActionCommand("ChooseData");
		
		JLabel idLabel = new JLabel("PhenX ID file");
		idLabel.setFont(serif12B);
		
		idField = new JTextField(30);
		idField.setFont(serif12);
		
		JButton idButton = new JButton("Choose");
		idButton.setFont(serif12);
		idButton.addActionListener(this);
		idButton.setActionCommand("ChooseID");
		
		GridBagConstraints gbc = new GridBagConstraints();
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(2, 5, 2, 5);
		
		topPanel.add(dataLabel, gbc);
		
		gbc.gridy++;
		gbc.gridwidth = 2;
		topPanel.add(dataField, gbc);
		gbc.gridx = 2;
		gbc.gridwidth = 1;
		topPanel.add(dataButton, gbc);
		
		gbc.gridx = 0;
		gbc.gridy++; 
		topPanel.add(idLabel, gbc);
		gbc.gridy++;
		gbc.gridwidth = 2;
		topPanel.add(idField, gbc);
		gbc.gridx = 2;
		gbc.gridwidth = 1;
		topPanel.add(idButton, gbc);
		
		getContentPane().add(topPanel, BorderLayout.NORTH);
		
		JPanel bottomPanel = new JPanel();
		bottomPanel.setForeground(Color.black);
		
		buildOKCancelButtons();
		bottomPanel.add(OKButton);
		bottomPanel.add(cancelButton);
		
		getContentPane().add(bottomPanel, BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
		System.gc();
		
		
	}

}
