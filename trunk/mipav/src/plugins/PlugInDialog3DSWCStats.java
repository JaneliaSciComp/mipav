import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;

/**
 * New plugin for Akanni Clarke of the Giniger Lab. Conceptually similar to the 
 * DrosophilaCreateSWC plugin written by Nish Pandya, this one also includes
 * keeping track of statistics. These statistics are: branch order, number, and 
 * length as well as the distance along the axon/parent that the branch
 * originates from. 
 * @author wangvg
 *
 */
public class PlugInDialog3DSWCStats extends JDialogStandalonePlugin implements AlgorithmInterface
{

	/**
	 * 
	 */
	private static final long serialVersionUID = 2942624971551641398L;
	
	private JTextField textField;
	
	private JFileChooser fileChooser;
	
	@SuppressWarnings("rawtypes")
	private JComboBox resolutionUnits;
	
	private JTextPane textArea;
	
	private JRadioButton axonRB;

	public PlugInDialog3DSWCStats(){
		super();
		
		init();
	}
	
	public void actionPerformed(ActionEvent e){
		String command = e.getActionCommand();
		if(command.equals("ok")){
			callAlgorithm();
		}else if(command.equals("cancel")){
			if(isExitRequired()){
				ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
			}
		}else if(command.equals(JFileChooser.APPROVE_SELECTION)){
			File selected = fileChooser.getSelectedFile();
			String name = selected.getAbsolutePath();
			Preferences.setImageDirectory(selected);
			textField.setText(name);
		}else if(command.equals("Browse")){
			chooseDir();
		}else{
			super.actionPerformed(e);
		}
	}
	

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(algorithm instanceof PlugInAlgorithm3DSWCStats){
			if(algorithm.isCompleted()){
				String fileText = textField.getText();
				File file = new File(fileText);
				
				String message;
				
				if(file.isDirectory()){
					message = "Converted files have been saved to "
							+ fileText;
				}else{
					String parent = file.getParent();
					String name = file.getName();
					name = name.substring(0, name.lastIndexOf("."));
					name += ".swc";
					message = "Converted file has been saved to "
							+ parent + File.separator + name + ".";
				}
				MipavUtil.displayInfo(message);
			}else{
				String message = "One or more files were unable to "
						+ "complete conversion. Check debugging output"
						+ "for more information.";
				MipavUtil.displayError(message);
			}
			
		}
	}
	
	protected void callAlgorithm(){
		
		String fileName = textField.getText();
		File file = new File(fileName);
		if(!(fileName.endsWith(".iv") || file.isDirectory())){
			MipavUtil.displayError("This file is not the correct format");
			return;
		}
		
		if(!file.exists()){
			MipavUtil.displayError("This file or directory does not exist");
			return;
		}
		
		ArrayList<File> files = new ArrayList<File>();
		
		if(file.isFile()){
			files.add(file);
		}else{
			File[] array = file.listFiles(new FilenameFilter(){

				@Override
				public boolean accept(File dir, String name) {
					if(name.endsWith(".iv"))
						return true;
					else
						return false;
				}
			});
			for(File f : array){
				files.add(f);
			}
		}
		
		PlugInAlgorithm3DSWCStats alg = new PlugInAlgorithm3DSWCStats(files, (String) resolutionUnits.getSelectedItem(), textArea);
		alg.useAxonLength(axonRB.isSelected());
		alg.addListener(this);
		
		if(isRunInSeparateThread()){
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void init(){
		
		setTitle("Imaris to 3D SWC with stats");
		
		getContentPane().removeAll();
		getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.PAGE_AXIS));
		
		JPanel mainPanel = new JPanel(new GridBagLayout());
		mainPanel.setForeground(Color.black);
		
		JLabel fileLabel = new JLabel("Input Imaris filament");
		fileLabel.setFont(serif12B);
		
		textField = new JTextField(30);
		textField.setText(Preferences.getImageDirectory());
		textField.setFont(serif12);
		
		JButton browseButton = new JButton("Browse");
		browseButton.setFont(serif12);
		browseButton.addActionListener(this);
		
		JPanel rbPanel = new JPanel(new GridLayout(0, 2));
		rbPanel.setForeground(Color.black);
		rbPanel.setBorder(new TitledBorder(BorderFactory.createLineBorder(Color.black), "Axon determination"));
		
		ButtonGroup group = new ButtonGroup();
		
		axonRB = new JRadioButton("Use absolute length");
		axonRB.setFont(serif12);
		axonRB.setSelected(true);
		group.add(axonRB);
		
		JRadioButton imarisRB = new JRadioButton("Infer from file");
		imarisRB.setFont(serif12);
		group.add(imarisRB);
		
		rbPanel.add(axonRB);
		rbPanel.add(imarisRB);
		
		JLabel resLabel = new JLabel("SWC Resolution Units");
		resLabel.setFont(serif12);
		
		Unit[] allSame = UnitType.getUnitsOfType(UnitType.LENGTH);
        int[] allSameMeasure = new int[allSame.length]; 
        for(int i=0; i<allSameMeasure.length; i++) {
            allSameMeasure[i] = allSame[i].getLegacyNum();
        }
        String[] unitArr = new String[allSameMeasure.length];
        for(int i=0; i<allSameMeasure.length; i++) {
        	Unit unit = Unit.getUnitFromLegacyNum(allSameMeasure[i]);
        	unitArr[i] = unit.getAbbrev();
        }
		
		resolutionUnits = new JComboBox(unitArr);
		resolutionUnits.setSelectedItem("um");
		resolutionUnits.setFont(serif12);
		
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.weighty = 0;
		gbc.insets = new Insets(5,5,5,5);
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		
		mainPanel.add(fileLabel, gbc);
		
		gbc.gridy = 1;
		gbc.weightx = 1;
		gbc.gridwidth = 2;
		
		mainPanel.add(textField, gbc);
		
		gbc.gridx = 2;
		gbc.weightx = 0;
		gbc.gridwidth = 1;
		
		mainPanel.add(browseButton, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		mainPanel.add(rbPanel, gbc);
		
		getContentPane().add(mainPanel);
		
		JPanel resPanel = new JPanel();
		resPanel.setForeground(Color.black);
		
		resPanel.add(resLabel);
		resPanel.add(resolutionUnits);
		
		getContentPane().add(resPanel);
		
		buildOKCancelButtons();
		
		//OKButton.addActionListener(this);
		OKButton.setActionCommand("ok");
		//cancelButton.addActionListener(this);
		cancelButton.setActionCommand("cancel");
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.setForeground(Color.black);
		buttonPanel.add(OKButton, BorderLayout.WEST);
		buttonPanel.add(cancelButton, BorderLayout.EAST);
		
		getContentPane().add(buttonPanel);
		
		JPanel debugPanel = new JPanel();
		debugPanel.setLayout(new BoxLayout(debugPanel, BoxLayout.PAGE_AXIS));;
		debugPanel.setForeground(Color.black);
		debugPanel.setBorder(new TitledBorder(BorderFactory.createEmptyBorder(), "Debugging Output"));
		
		JPanel textPanel = new JPanel(new BorderLayout());
		textArea = new JTextPane();
		textPanel.add(textArea, BorderLayout.CENTER);
		JScrollPane scrollPane = new JScrollPane(textPanel);
		scrollPane.setPreferredSize(new Dimension(100,200));
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		debugPanel.add(scrollPane);
		
		getContentPane().add(debugPanel);
		
		pack();
		setVisible(true);
		System.gc();
	}
	
	private void chooseDir(){
		String dirText = Preferences.getImageDirectory();
		
		FileFilter filter = new FileFilter(){

			@Override
			public boolean accept(File pathname) {
				if(pathname.isDirectory())
					return true;
				String name = pathname.getName();
				if(name.endsWith(".iv"))
					return true;
				else
					return false;
			}
			
			@Override
			public String getDescription()
            {
                return "Imaris Filaments (.iv)";
            }
			
		};
		
		fileChooser = new JFileChooser(dirText);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addActionListener(this);
		fileChooser.addChoosableFileFilter(filter);
		fileChooser.setFileFilter(filter);
		fileChooser.showOpenDialog(this);
	}

	
}
