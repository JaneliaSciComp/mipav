import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.RandomAccessFile;
import java.io.Writer;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;

import gov.nih.mipav.model.file.DicomType;
import gov.nih.mipav.model.file.FileDicomTagInfo;
import gov.nih.mipav.model.file.FileDicomTagInfo.NumType;
import gov.nih.mipav.model.file.FileDicomTagInfo.StringType;
import gov.nih.mipav.model.file.FileDicomTagInfo.VR;

import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogDicomDataElementsGenerator extends JDialogBase implements AlgorithmInterface {

	
	
	
	
	/** grid bag constraints **/
	private GridBagConstraints gbc;
	
	/** main panel **/
	private JPanel mainPanel;
	
	private JTextField dicomDictFilePathTextField;
	
	private JButton dicomDictBrowseButton;
	
	private String currDir = null;
	
	private File dicomDictFile;
	
	
	
	public PlugInDialogDicomDataElementsGenerator() {
		
	}
	
	
	
	public PlugInDialogDicomDataElementsGenerator(boolean modal) {
		super(modal);
		init();
	}
	
	
	
	
	public void init() {
		setForeground(Color.black);
        setTitle("Dicom Data Elements Generator");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        
        JLabel dicomDictLabel = new JLabel("Dicom Dictonary File Path");
        dicomDictFilePathTextField = new JTextField(35);
        dicomDictFilePathTextField.setEditable(false);
        dicomDictFilePathTextField.setBackground(Color.white);
        dicomDictBrowseButton = new JButton("Browse");
        dicomDictBrowseButton.addActionListener(this);
        dicomDictBrowseButton.setActionCommand("dicomDictBrowse");
        

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15,5,5,15);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(dicomDictLabel,gbc);
        gbc.gridx = 1;
        mainPanel.add(dicomDictFilePathTextField,gbc);
        gbc.gridx = 2;
        mainPanel.add(dicomDictBrowseButton,gbc);
        
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
	}
	
	
	
	
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}

	
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("dicomDictBrowse")) {
			 JFileChooser chooser = new JFileChooser();
		        if (currDir != null) {
					chooser.setCurrentDirectory(new File(currDir));
		        }
		        chooser.setDialogTitle("Choose dicom dictionary file");
		        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		        int returnValue = chooser.showOpenDialog(this);
		        if (returnValue == JFileChooser.APPROVE_OPTION) {
		        	currDir = chooser.getSelectedFile().getAbsolutePath();
		        	dicomDictFile = new File(currDir);
		        	dicomDictFilePathTextField.setText(currDir);
		        }
		 }else if(command.equalsIgnoreCase("ok")) {
			 parseDicomDictFile();
			 
		 }else if(command.equalsIgnoreCase("cancel")) {
			 dispose();
			 
		 } else {
	            super.actionPerformed(e);
	        }

	}
	
	
	
	private boolean parseDicomDictFile() {
		boolean success = true;
		RandomAccessFile raFile = null;
		File to = new File(dicomDictFile.getParent() + File.separator + "dicomDataElements.txt");
		FileOutputStream fos = null;
		Writer out = null;
		try {
			fos = new FileOutputStream(to);
	        out = new OutputStreamWriter(fos);
			raFile = new RandomAccessFile(dicomDictFile, "r");
			String line;
			String elementName;
			String valueRep;
			DicomType type;
			String ndarType;
			String size = " ";
			String required = "Optional";
			String description;
			System.out.println("elementName" + "\t"  + "DataType" + "\t" + "Size" + "\t" + "Unit" + "\t" + "Required" + "\t" + "Condition" + "\t" + "elementDescription") ;
			out.write("elementName" + "\t"  + "DataType" + "\t" + "Size" + "\t" + "Unit" + "\t" + "Required" + "\t" + "Condition" + "\t" + "elementDescription" + "\n");
			out.flush();
			while((line = raFile.readLine()) != null) {
				elementName = line.substring(0, line.indexOf(")")+1);
				if(!elementName.startsWith("#") && !elementName.contains("xx")) {
					elementName = "dicom_" + elementName;
					valueRep = (line.substring(line.indexOf("VR=") + 3, line.indexOf("VM="))).trim();
					valueRep = valueRep.substring(valueRep.indexOf("\"") + 1, valueRep.lastIndexOf("\""));
					if(!valueRep.equals("SQ")) {

						type = FileDicomTagInfo.getType(VR.valueOf(valueRep));
						
						if(type.equals(NumType.LONG)) {
							ndarType = "Integer";
							size = " ";
						}else if(type.equals(NumType.SHORT)) {
							ndarType = "Integer";
							size = " ";
						}else if(type instanceof StringType) {
							ndarType = "String";
							size = "1024";
						}
						else if(type.equals(NumType.FLOAT)) {
							ndarType = "Float";
							size = " ";
						}
						else if(type.equals(NumType.DOUBLE)) {
							ndarType = "Float";
							size = " ";
						}else {
							ndarType = "String";
							size = "1024";
						}
						
						
						
						description = line.substring(line.indexOf("Name=") + 5);
						description = description.substring(description.indexOf("\"") + 1, description.lastIndexOf("\""));
						
						
						System.out.println(elementName + "\t"  + ndarType + "\t" + size + "\t" + " " + "\t" + required + "\t" + " " + "\t" + description) ;
						out.write(elementName + "\t"  + ndarType + "\t" + size + "\t" + " " + "\t" + required + "\t" + " " + "\t" + description + "\n");
						out.flush();
					}
				}
			}
			out.close();
			fos.close();
			raFile.close();
			return success;
			
		}catch (Exception e) {
			try {
				if(raFile != null) {
					raFile.close();
				}
				if (out != null) {
                    out.close();
                }

                if (fos != null) {
                    fos.close();
                }
			}catch(Exception ex) {
				
			}
			e.printStackTrace();
			return false;
		}
		
		
		
	}

}
