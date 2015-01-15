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
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
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
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

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
	
	private JTextField imageField;
	
	private JFileChooser fileChooser;
	
	@SuppressWarnings("rawtypes")
	private JComboBox resolutionUnits;
	
	private JTextPane textArea;
	
	private JRadioButton axonRB;
	
	private JRadioButton customRB;
	private JRadioButton densityRB;
	
	private boolean chooseIV;
	
	private PlugInAlgorithm3DSWCViewer alg;
	
	private boolean writeStep;
	
	private boolean locked;
	
	private File[] densityFiles;
	
	private int densityCount;
	
	public PlugInDialog3DSWCStats(){
		super();
		
		writeStep = false;
		locked = false;
		
		init();
	}
	
	public void actionPerformed(ActionEvent e){
		String command = e.getActionCommand();
		if(command.equals("ok")){
			if(!locked && (alg == null || !alg.isViewerOpen())){
				if(writeStep){
					locked = true;
					if(!alg.write())
						locked = false;
				}else{
					locked = true;
					callAlgorithm();
				}
			}
		}else if(command.equals("cancel")){
			if(isExitRequired()){
				ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
			}else{
				dispose();
			}
		}else if(command.equals(JFileChooser.APPROVE_SELECTION)){
			File selected = fileChooser.getSelectedFile();
			String name = selected.getAbsolutePath();
			Preferences.setImageDirectory(selected);
			if(chooseIV)
				textField.setText(name);
			else imageField.setText(name);
		}else if(command.equals("Browse")){
			chooseIV = true;
			chooseDir();
		}else if(command.equals("BrowseImage")){
			chooseIV = false;
			chooseDir();
		}else{
			super.actionPerformed(e);
		}
	}
	

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(algorithm instanceof PlugInAlgorithm3DSWCViewer){
			if(algorithm.isCompleted()){
				if(densityRB.isSelected() && writeStep){
					densityCount++;
					if(densityCount<densityFiles.length){
						alg = new PlugInAlgorithm3DSWCViewer(densityFiles[densityCount], textArea, (String) resolutionUnits.getSelectedItem());
						alg.addListener(this);
						new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);
						if(isRunInSeparateThread()){
							if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
								MipavUtil.displayError("A thread is already running on this object");
							}
						} else {
							alg.run();
						}
					}else{
						locked = false;
					}
				}else{
					locked = false;
				}
				writeStep ^= true;
				/*String fileText = textField.getText();
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
				MipavUtil.displayInfo(message);*/
				
			}else{
				locked = false;
				writeStep = false;
			}
			
		}
	}
	
	protected void callAlgorithm(){
		
		String fileName = textField.getText();
		File file = new File(fileName);
		
		if(densityRB.isSelected()){
			
			File parent = file.getParentFile();
			File[] list = parent.listFiles(new FilenameFilter(){

				@Override
				public boolean accept(File dir, String name) {
					int ind = name.lastIndexOf(".");
					if(ind < 0)
						return false;
					String ext = name.substring(ind);
					if(ext.equalsIgnoreCase(".iv")){
						return true;
					}else{
						return false;
					}
				}
			});
			
			String parentStr = parent.getAbsolutePath();
			File csvFile = new File(parentStr + File.separator + "branch_density.csv");
			try {
				FileWriter fw = new FileWriter(csvFile);
				fw.append("Branch Density Statistics\n");
				fw.close();
			} catch (IOException e) {
				String message = "Could not create a CSV file for writing. Make sure to close any open CSVs.";
				SimpleAttributeSet redText = new SimpleAttributeSet();
				StyleConstants.setFontFamily(redText, "Serif");
				StyleConstants.setFontSize(redText, 12);
				StyleConstants.setForeground(redText, Color.red.darker());
				Document doc = textArea.getDocument();
				try {
					doc.insertString(doc.getLength(), message + "\n", redText);
				} catch (BadLocationException ex) {
					ex.printStackTrace();
				}
				textArea.setCaretPosition(doc.getLength());
				return;
			}
			
			densityFiles = list;
			densityCount = 0;
			
			alg = new PlugInAlgorithm3DSWCViewer(list[densityCount], textArea, (String) resolutionUnits.getSelectedItem());
			alg.addListener(this);
			new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);
			
		}else{
			
			if(!fileName.endsWith(".iv")){
				MipavUtil.displayError("This file is not the correct format");
				return;
			}
			
			if(!file.exists()){
				MipavUtil.displayError("This file does not exist");
				return;
			}
			
			if(customRB.isSelected()){
				alg = new PlugInAlgorithm3DSWCViewer(imageField.getText(), file, textArea, (String) resolutionUnits.getSelectedItem(), false, true);
				alg.addListener(this);
				new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);
				
				//PlugInDialog3DSWCViewer viewer = new PlugInDialog3DSWCViewer(imageField.getText(), file, textArea, (String) resolutionUnits.getSelectedItem());
				//alg = new PlugInAlgorithm3DSWCViewer()
	
			}else{
			
				alg = new PlugInAlgorithm3DSWCViewer(imageField.getText(), file, textArea, 
						(String) resolutionUnits.getSelectedItem(), axonRB.isSelected(), false);
				alg.addListener(this);
				
			}
		}
		
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
		textField.setFont(serif12);
		
		JButton browseButton = new JButton("Browse");
		browseButton.setFont(serif12);
		browseButton.addActionListener(this);
		
		JLabel imLabel = new JLabel("Input Imaris image");
		imLabel.setFont(serif12B);
		
		imageField = new JTextField(30);
		imageField.setFont(serif12);
		
		JButton browseImage = new JButton("Browse");
		browseImage.setFont(serif12);
		browseImage.setActionCommand("BrowseImage");
		browseImage.addActionListener(this);
		
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
		
		customRB = new JRadioButton("Choose filament");
		customRB.setFont(serif12);
		group.add(customRB);
		
		densityRB = new JRadioButton("Branch Density");
		densityRB.setFont(serif12);
		group.add(densityRB);
		
		rbPanel.add(axonRB);
		rbPanel.add(imarisRB);
		rbPanel.add(customRB);
		rbPanel.add(densityRB);
		
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
		gbc.gridy++;
		
		mainPanel.add(imLabel, gbc);
		
		gbc.gridy++;
		gbc.weightx = 1;
		gbc.gridwidth = 2;
		
		mainPanel.add(imageField, gbc);
		
		gbc.gridx = 2;
		gbc.weightx = 0;
		gbc.gridwidth = 1;
		
		mainPanel.add(browseImage, gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
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
		
		FileFilter ivFilter = new FileFilter(){

			@Override
			public boolean accept(File pathname) {
				if(pathname.isDirectory())
					return true;
				String name = pathname.getName();
				int index = name.lastIndexOf(".");
				if(index < 0)
					return false;
				String fileExt = name.substring(index);
				if(fileExt.equalsIgnoreCase(".iv"))
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
		
		FileFilter imFilter = new FileFilter(){
			@Override
			public boolean accept(File pathname) {
				if(pathname.isDirectory())
					return true;
				String name = pathname.getName();
				int index = name.lastIndexOf(".");
				if(index < 0)
					return false;
				String fileExt = name.substring(index);
				if(fileExt.equalsIgnoreCase(".ics"))
					return true;
				else
					return false;
			}
			
			@Override
			public String getDescription()
            {
                return "Imaris Image (.ics)";
            }
		};
		
		FileFilter filter = chooseIV ? ivFilter : imFilter;
		
		fileChooser = new JFileChooser(dirText);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.addActionListener(this);
		fileChooser.addChoosableFileFilter(filter);
		fileChooser.setFileFilter(filter);
		fileChooser.showOpenDialog(this);
	}

	
}
