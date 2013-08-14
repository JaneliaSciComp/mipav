package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileInfoDicom;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Set;

public class JDialogDicomTagMultiEditor extends JDialogDicomTagSelector {

	protected File file;

	protected FileInfoDicom fileInfo;
	
	protected static final String SAVE = "Process";
	
	public JDialogDicomTagMultiEditor(Hashtable<FileDicomKey, FileDicomTag> tagList, JDialogBase parent, 
			boolean isStandalone, File file, FileInfoDicom fileInfo) {
		super(tagList, parent, isStandalone);
		
		this.file = file;
		this.fileInfo = fileInfo;
		
		closeButton.setActionCommand(SAVE);
		closeButton.setText(SAVE);
		closeButton.addActionListener(this);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if(e.getSource().equals(closeButton)) {
			try {
				FileDicom writeDicom = new FileDicom(file.getAbsolutePath());
				int length = tagsTable.getRowCount();
				FileDicomKey[] keyArray = new FileDicomKey[length];
				FileDicomTag[] tagArray = new FileDicomTag[length];
				Set<Entry<FileDicomKey, FileDicomTag>> entrySet = tagList.entrySet();
				for(int i=0; i<length; i++) {
					FileDicomKey key = new FileDicomKey(tagsTable.getValueAt(i, 0).toString());
					Iterator<Entry<FileDicomKey, FileDicomTag>> keyItr = entrySet.iterator();
					Entry<FileDicomKey, FileDicomTag> entry = null;
		keySearch:	while(keyItr.hasNext()) {
						entry = keyItr.next();
						if(entry.getKey().equals(key)) {
							keyArray[i] = key;
							tagArray[i] = entry.getValue();
							tagArray[i].setValue(tagsTable.getValueAt(i, 2));
							break keySearch;
						}
					}
				}
				
				RandomAccessFile raFile = new RandomAccessFile(file, "rw");
				
				writeDicom.writeTags(raFile, fileInfo, keyArray, tagArray);
			} catch(IOException ex) {
				System.err.println("Unable to write to file: "+file.getAbsolutePath());
			}
		} else {
			super.actionPerformed(e);
		}
	}

}
