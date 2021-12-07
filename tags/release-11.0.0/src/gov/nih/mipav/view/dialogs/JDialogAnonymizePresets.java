package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dialog;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Set;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

public class JDialogAnonymizePresets extends JDialogBase {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1564864947768799239L;
	
	private static final String[] filenames = new String[] {
		"UID.txt", "device.txt", "patient.txt", "fullDate.txt",
		"modifDate.txt", "cleanDesc.txt", "cleanStruct.txt", "cleanGraph.txt"
	};
	
	private ArrayList<JCheckBox> boxes;
	
	private JCheckBox fullDatesBox;
	
	private JCheckBox modifDatesBox;
	
	private ModelImage im;

	public JDialogAnonymizePresets(Dialog parentDialog, boolean modal, ModelImage image){
		super(parentDialog, modal);
		
		im = image;
		init();
	}
	
	private void init(){
		setTitle("Anonymize from preset profiles");
		
		boxes = new ArrayList<JCheckBox>();
		
		JPanel descPanel = new JPanel();
		descPanel.setForeground(Color.black);
		descPanel.setBorder(new EmptyBorder(0, 0, 5, 0));
		String desc = "<html><b>"
				+ "This will enact anonymization rules as suggested<br>"
				+ "by the DICOM standards. Select extra profiles to<br>"
				+ "modify the basic rule set."
				+ "<br>"
				+ "<br>"
				+ "NOTE: Keys not in the profile rule set will not<br>"
				+ "be changed. Please make sure all necessary tags<br>"
				+ "are de-identified"
				+ "</b></html>";
		JLabel descLabel = new JLabel(desc);
		descLabel.setFont(serif12);
		descPanel.add(descLabel);
		getContentPane().add(descPanel, BorderLayout.NORTH);
		
		JPanel presetPanel = new JPanel(new GridLayout(0,2));
		presetPanel.setForeground(Color.black);
		
		
		JCheckBox uidBox = new JCheckBox("Retain UIDs");
		uidBox.setFont(serif12);
		presetPanel.add(uidBox);
		boxes.add(uidBox);
		
		JCheckBox deviceBox = new JCheckBox("Retain device ident.");
		deviceBox.setFont(serif12);
		presetPanel.add(deviceBox);
		boxes.add(deviceBox);
		
		JCheckBox patientBox = new JCheckBox("Retain patient chars");
		patientBox.setFont(serif12);
		presetPanel.add(patientBox);
		boxes.add(patientBox);
		
		fullDatesBox = new JCheckBox("Retain long. full dates");
		fullDatesBox.setFont(serif12);
		fullDatesBox.addActionListener(this);
		presetPanel.add(fullDatesBox);
		boxes.add(fullDatesBox);
		
		modifDatesBox = new JCheckBox("Retain long. modif. dates");
		modifDatesBox.setFont(serif12);
		modifDatesBox.addActionListener(this);
		presetPanel.add(modifDatesBox);
		boxes.add(modifDatesBox);
		
		JCheckBox descBox = new JCheckBox("Clean descriptions");
		descBox.setEnabled(false);
		descBox.setFont(serif12);
		presetPanel.add(descBox);
		boxes.add(descBox);
		
		JCheckBox structBox = new JCheckBox("Clean struct. content");
		structBox.setEnabled(false);
		structBox.setFont(serif12);
		presetPanel.add(structBox);
		boxes.add(structBox);
		
		JCheckBox graphBox = new JCheckBox("Clean graphics");
		graphBox.setEnabled(false);
		graphBox.setFont(serif12);
		presetPanel.add(graphBox);
		boxes.add(graphBox);
		
		getContentPane().add(presetPanel, BorderLayout.CENTER);
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.setForeground(Color.black);
		buildOKCancelButtons();
		buttonPanel.add(OKButton);
		buttonPanel.add(cancelButton);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
		System.gc();
	}
	
	private void runAnonymization(){
		
		//Start by building hash for the keys and corresponding anonymization command
		
		final URL fileURL = Thread.currentThread().getContextClassLoader().getResource("DICOM profiles/basic.txt");
		Hashtable<String, String> actionTable = new Hashtable<String, String>();
		
		
		try{
			BufferedReader basic = new BufferedReader(new InputStreamReader(fileURL.openStream()));
			String line;
			while((line = basic.readLine()) != null){
				String[] split = line.split(" ");
				String keyString = split[0];
				String actionString = split[1];
				actionTable.put(keyString, actionString);
			}
			
			basic.close();
			
			for(int i=0;i<boxes.size();i++){
				JCheckBox b = boxes.get(i);
				if(b.isSelected()){
					URL addURL = Thread.currentThread().getContextClassLoader().getResource("DICOM profiles/" + filenames[i]);
					BufferedReader add = new BufferedReader(new InputStreamReader(addURL.openStream()));
					while((line = add.readLine()) != null){
						String[] split = line.split(" ");
						String keyString = split[0];
						String actionString = split[1];
						actionTable.put(keyString, actionString);
					}
					add.close();
				}
			}
			
		} catch (IOException e){
			e.printStackTrace();
		}
		
		//Should now have complete action table, proceed to carrying out actions
		
		/**
		 * D - replace w/ non-zero length value
		 * Z - replace w/ zero length value or non-zero length value
		 * X - remove
		 * K - keep
		 * C - clean;
		 * U - replace w/ non-zero length UID
		 * 
		 * for now, if a key in the image does not have an action, assume K(eep)
		 */
		int depth = im.getNDims() == 2 ? 1 : im.getExtents()[2];
		
		for(int i=0;i<depth;i++){
		
			FileInfoDicom info = (FileInfoDicom) im.getFileInfo(i);
			
			FileDicomTagTable table = info.getTagTable();
			Set<FileDicomKey> keys = table.getTagList().keySet();
			for(FileDicomKey k : keys){
				String keyStr = k.getKey();
				if(k.getGroupNumber() % 2 == 1){
					table.removeTag(k); //remove private tags by default
					continue;
				}
				//Carry out the action given by the table
				String action = actionTable.get(keyStr);
				if(action == null || action.equals("K")){//Default to keep if not in table
					continue;
				} 
				//default to whatever is first for the multi-tags
				//since don't really know how to differentiate type 1, 2, 3
				action = action.substring(0, 1);
				
				if(action.equals("X")){
					table.removeTag(k);
				} else if(action.equals("C")){
					FileDicomTag t = table.get(k);
					VR vr = t.getValueRepresentation();
					if(vr == VR.DA){
						String date = (String)t.getValue(false);
						date = "1900" + date.substring(4);
						table.setValue(k, date);
					} else if (vr == VR.DT){
						String val = (String)t.getValue(false);
						val = "1900" + val.substring(4);//val.substring(4, 8); //what to do with time
						table.setValue(k,val);
					} else if (vr == VR.TM){
						//change it to what?
					} else {
						System.out.println("Clean does nothing yet");
						//not sure what to do with "clean" option otherwise
					}
				} else if(action.equals("Z") || action.equals("D") || action.equals("U")){
					FileDicomTag t = table.get(k);
					boolean blank = action.equals("Z");
					Object newTag = FileInfoDicom.generateNewTagValue(k.getKey(), (String)t.getValue(false),
							t.getValueRepresentation(), blank);
					table.setValue(k, newTag);
				} else {
					//Action is not correct for some reason. 
					System.out.println(action + " is not an action");
				}
				
			}
		}
		
		getOwner().getOwner().dispose();
		im.getParentFrame().about();
	}
	
	public void actionPerformed(ActionEvent e){
		Object source = e.getSource();
		String command = e.getActionCommand();
		if(source instanceof JCheckBox){
			JCheckBox box = (JCheckBox) source;
			if(box.isSelected()){
				if(box == fullDatesBox)//These two options are mutually exclusive
					modifDatesBox.setSelected(false);
				else if(box == modifDatesBox)
					fullDatesBox.setSelected(false);
			}
		} else if(command.equals("OK")){
			runAnonymization();
		} else if(command.equals("Cancel")){
			dispose();
		}
		
		
	}

}
