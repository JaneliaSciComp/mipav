import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.filechooser.FileNameExtensionFilter;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;


public class PlugInDialogKeyChecker extends JDialogStandalonePlugin implements
		AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = -241725202791699791L;
	
	private JFileChooser fileChooser;
	
	private File keyFile;
	
	private ArrayList<File> dataFiles;
	
	private boolean chooseKey;
	
	private JTextField keyField;
	
	@SuppressWarnings("rawtypes")
	private JList dataList;
	
	private JRadioButton demoRB;
	
	public PlugInDialogKeyChecker(){
		dataFiles = new ArrayList<File>();
		
		init();
	}

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub
		if(algorithm instanceof PlugInAlgorithmKeyChecker && algorithm.isCompleted()){
			
			String keyDir = keyFile.getParent() + File.separator;
			String keyName = keyFile.getName();
			keyName = keyName.substring(0, keyName.lastIndexOf("."));
			keyName += "_keycompare.csv";
			String csvName = keyDir + keyName;
			
			MipavUtil.displayInfo("Differences saved to: " + csvName);
		}

	}
	
	public void actionPerformed(ActionEvent event){
		
		String command = event.getActionCommand();
		
		if(command.equals("ChooseKey")){
			chooseKey = true;
			chooseDir();
		} else if(command.equals("ChooseData")){
			chooseKey = false;
			chooseDir();
		} else if(command.equals("OK")){
			callAlgorithm();
		} else if(command.equals("Cancel")){
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		} else if(command.equals("ApproveSelection")){
			if(chooseKey){
				String text = fileChooser.getSelectedFile().toString();
				keyField.setText(text);
	        	Preferences.setImageDirectory(new File(text));
	        	keyFile = new File(text);
			} else {
				getDataFiles();
			}
		}
		
	}
	
	protected void callAlgorithm(){
		
		PlugInAlgorithmKeyChecker alg = new PlugInAlgorithmKeyChecker(keyFile, dataFiles, demoRB.isSelected());
		alg.addListener(this);
		if (isRunInSeparateThread()) {
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
		
	}
	
	@SuppressWarnings("rawtypes")
	private void init(){
		
		setTitle("Key Checker");
		getContentPane().removeAll();
		
		JPanel keyPanel = new JPanel(new GridBagLayout());
		keyPanel.setForeground(Color.black);
		
		GridBagConstraints gbc1 = new GridBagConstraints();
		
		gbc1.anchor = GridBagConstraints.FIRST_LINE_START;
		gbc1.insets = new Insets(5, 5, 5, 5);
		
		JLabel keyLabel = new JLabel("Key file");
		keyLabel.setFont(serif12B);
		keyPanel.add(keyLabel, gbc1);
		
		gbc1.gridy = 1;
		
		keyField = new JTextField(30);
		keyField.setFont(serif12);
		keyPanel.add(keyField, gbc1);
		
		gbc1.gridx = 1;
		
		JButton keyButton = new JButton("Choose");
		keyButton.setFont(serif12);
		keyButton.addActionListener(this);
		keyButton.setActionCommand("ChooseKey");
		keyPanel.add(keyButton, gbc1);
		
		JPanel rbPanel = new JPanel();
		rbPanel.setForeground(Color.black);
		
		demoRB = new JRadioButton("Demo GUID");
		demoRB.setFont(serif12);
		demoRB.setSelected(true);
		rbPanel.add(demoRB);
		
		JRadioButton prodRB = new JRadioButton("Production GUID");
		prodRB.setFont(serif12);
		rbPanel.add(prodRB);
		
		ButtonGroup group = new ButtonGroup();
		group.add(demoRB);
		group.add(prodRB);
		
		PanelManager manage = new PanelManager();
		manage.add(keyPanel);
		manage.addOnNextLine(rbPanel);
		
		getContentPane().add(manage.getPanel(), BorderLayout.NORTH);
		
		JPanel dataPanel = new JPanel(new GridBagLayout());
		dataPanel.setForeground(Color.black);
		
		GridBagConstraints gbc2 = new GridBagConstraints();
		
		gbc2.anchor = GridBagConstraints.FIRST_LINE_START;
		gbc2.insets = new Insets(5, 5, 5, 5);
		
		JLabel dataLabel = new JLabel("Data files");
		dataLabel.setFont(serif12B);
		dataPanel.add(dataLabel, gbc2);
		
		gbc2.gridy = 1;
		
		dataList = new JList();
		dataList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		dataList.setLayoutOrientation(JList.VERTICAL);
		dataList.setVisibleRowCount(0);
		
        JScrollPane listScroller = new JScrollPane(dataList);
        listScroller.setPreferredSize(new Dimension(275, 275));
        listScroller.setAlignmentX(LEFT_ALIGNMENT);
        listScroller.setFont(serif12);
        
        dataPanel.add(listScroller, gbc2);
        
        gbc2.gridx = 1;
        
        JButton dataButton = new JButton("Choose");
		dataButton.setFont(serif12);
		dataButton.addActionListener(this);
		dataButton.setActionCommand("ChooseData");
		dataPanel.add(dataButton, gbc2);

		getContentPane().add(dataPanel);
		
		buildOKCancelButtons();
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.setForeground(Color.black);
		
		buttonPanel.add(OKButton);
		buttonPanel.add(cancelButton);
		
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);
		
		pack();
		setVisible();
		System.gc();
	}
	
	private void chooseDir(){
		
		FileNameExtensionFilter csvFilter = new FileNameExtensionFilter("CSV file (.csv)", "csv");
		String dirText = Preferences.getImageDirectory();
		fileChooser = new JFileChooser(dirText);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.setMultiSelectionEnabled(!chooseKey);
		fileChooser.addActionListener(this);
		fileChooser.addChoosableFileFilter( csvFilter);
		fileChooser.setAcceptAllFileFilterUsed(false);
		fileChooser.setFileFilter(csvFilter);
		fileChooser.showOpenDialog(this);
	}
	
	@SuppressWarnings("unchecked")
	private void getDataFiles(){
		File[] files = fileChooser.getSelectedFiles();
		for(File f : files){
			if(f.isDirectory()){
				getFilesInDir(f);
			} else {
				dataFiles.add(f);
			}
		}
		
		Vector<String> fileVector = new Vector<String>();
		
		for(File f : dataFiles){
			String name = f.getName();
			fileVector.add(name);
		}
		
		dataList.setListData(fileVector);
	}
	
	private void getFilesInDir(File dir){
		FilenameFilter csvFilter = new FilenameFilter(){
			public boolean accept(File dir, String name) {
				return name.toLowerCase().endsWith("csv");
			}
		};
		File[] subFiles = dir.listFiles(csvFilter);
		for(File s : subFiles){
			dataFiles.add(s);
		}
		
		FileFilter dirFilter = new FileFilter(){
			@Override
			public boolean accept(File pathname) {
				return pathname.isDirectory();
			}
			
		};
		File[] dirFiles = dir.listFiles(dirFilter);
		for(File s : dirFiles){
			getFilesInDir(s);
		}
		
	}

}
