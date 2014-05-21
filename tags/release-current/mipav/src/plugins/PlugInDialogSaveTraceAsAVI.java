import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;


public class PlugInDialogSaveTraceAsAVI extends JDialogStandalonePlugin implements AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = 600685001478262276L;
	
	private JTextField dirText;
	
	private JTextField freqText;
	
	private JRadioButton swcRB;
	
	private JRadioButton imageRB;
	
	private JFileChooser fileChooser;
	
	private ArrayList<File> fileList;
	
	private String dir = Preferences.getImageDirectory();
	
	private JTextField nameText;
	
	public PlugInDialogSaveTraceAsAVI(){
		super();
		
		fileList = new ArrayList<File>();
		
		init();
	}
	
	public PlugInDialogSaveTraceAsAVI(String directory){
		super();
		
		fileList = new ArrayList<File>();
		dir = directory;
		
		init();
	}
	
	public void actionPerformed(ActionEvent event){
		String command = event.getActionCommand();
		
		if (command.equals("ApproveSelection")){
			dirText.setText(fileChooser.getSelectedFile().toString());
        	Preferences.setImageDirectory(fileChooser.getSelectedFile());
        }
		else if(command.equals("Cancel")){
			
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		}
		else if(command.equals("Choose")) chooseDir();
		else if(command.equals("OK")){
			if(populateImages(new File(dirText.getText()))){
				callAlgorithm();
			} else {
				MipavUtil.displayError("No files were found");
			}
		}
	}
	

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(algorithm.isCompleted()){
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		}
	}
	
	protected void callAlgorithm(){
		float frameFreq;
		try{
			frameFreq = Float.valueOf(freqText.getText());
		} catch(NumberFormatException e){
			MipavUtil.displayError("Slices per second is not a number");
			return;
		}
		
		PlugInAlgorithmSaveTraceAsAVI alg = new PlugInAlgorithmSaveTraceAsAVI(fileList, frameFreq, swcRB.isSelected(), nameText.getText());
		alg.addListener(this);
		if (isRunInSeparateThread()) {
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
		
		
	}
	
	private void chooseDir(){
		fileChooser = new JFileChooser(dir);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addActionListener(this);
		fileChooser.showOpenDialog(this);
	}
	
	private void init(){
		setForeground(Color.black);
        setTitle("Save Traces as AVI");

        JPanel dirPanel = new JPanel();
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose Image File/Directory"));
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please choose a folder that contains traces to convert<br>"
        		+ "to AVI. Choose whether to look for SWC or image files.</html>";

        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel);
        getContentPane().add(dirPanel, BorderLayout.NORTH);
        
        JPanel choosePanel = new JPanel();
        
        dirText = new JTextField(30);
        dirText.setText(dir);
        dirText.setFont(serif12);
        choosePanel.add(dirText);
        
        JButton dirButton = new JButton("Choose");
        dirButton.setFont(serif12);
        dirButton.addActionListener(this);
        choosePanel.add(dirButton);
        
        JPanel namePanel = new JPanel();
        namePanel.setForeground(Color.black);
        
        JLabel nameLabel = new JLabel("AVI Name: ");
        nameLabel.setFont(serif12);
        namePanel.add(nameLabel);
        
        nameText = new JTextField(19);
        nameText.setText("trace_movie");
        nameText.setFont(serif12);
        namePanel.add(nameText);
        
        JPanel radioPanel = new JPanel();
        radioPanel.setForeground(Color.black);
        
        JLabel radioLabel = new JLabel("Make AVI from: ");
        radioLabel.setFont(serif12);
        radioPanel.add(radioLabel);
        
        swcRB = new JRadioButton("SWC files");
        swcRB.setFont(serif12);
        swcRB.setSelected(true);
        
        imageRB = new JRadioButton("Image files");
        imageRB.setFont(serif12);
        
        ButtonGroup group = new ButtonGroup();
        group.add(swcRB);
        group.add(imageRB);
        radioPanel.add(swcRB);
        radioPanel.add(imageRB);
        
        JPanel rangePanel = new JPanel();
        rangePanel.setForeground(Color.black);
        
        JLabel rangeLabel = new JLabel("Slices per second");
        rangeLabel.setFont(serif12);
        rangePanel.add(rangeLabel);
        
        freqText = new JTextField(5);
        freqText.setText("10.0");
        rangePanel.add(freqText);
        
        PanelManager manage = new PanelManager();
        manage.add(choosePanel);
        manage.addOnNextLine(namePanel);
        manage.addOnNextLine(radioPanel);
        manage.addOnNextLine(rangePanel);
        getContentPane().add(manage.getPanel(), BorderLayout.CENTER);

        JPanel OKCancelPanel = new JPanel();

        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
	}
		
	private boolean populateImages(File dir){

		FilenameFilter imFilter;
		if(swcRB.isSelected()){
			imFilter = new FilenameFilter(){
				public boolean accept(File dir, String name) {
					return (name.toLowerCase().endsWith(".swc"));
				}
			};
		} else {
			imFilter = new FilenameFilter(){
				public boolean accept(File dir, String name) {
					return (name.toLowerCase().endsWith("_trace.tif"));
				}
			};
		}
			
		File[] files = dir.listFiles(imFilter);

		for(File im : files){
			fileList.add(im);
		}

		return fileList.size() > 0;
	}

		

}
