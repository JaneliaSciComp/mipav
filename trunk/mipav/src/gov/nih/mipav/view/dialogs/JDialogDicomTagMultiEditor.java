package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.view.MipavUtil;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Set;

import javax.swing.table.DefaultTableModel;

public class JDialogDicomTagMultiEditor extends JDialogDicomTagSelector {

	protected File file;

	protected FileInfoDicom fileInfo;
	
	protected static final String SAVE = "Process";
	
	private boolean processed = false;

	private FileDicomKey[] keyArray;

	private FileDicomTag[] tagArray;
	
	public JDialogDicomTagMultiEditor(Hashtable<FileDicomKey, FileDicomTag> tagList, JDialogBase parent, 
			boolean isStandalone, File file, FileInfoDicom fileInfo) {
		super(tagList, parent, isStandalone);
		
		this.file = file;
		this.fileInfo = fileInfo;
		
		closeButton.setActionCommand(SAVE);
		closeButton.setText(SAVE);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if(e.getSource().equals(closeButton) && !processed) {
			callAlgorithm();
		} else {
			super.actionPerformed(e);
		}
	}

	@Override
	protected void callAlgorithm() {
		try {
			FileDicom writeDicom = new FileDicom(file.getAbsolutePath());
			int length = tagsTable.getRowCount();
			keyArray = new FileDicomKey[length];
			tagArray = new FileDicomTag[length];
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
			
			System.out.println("Here again");
			writeDicom.writeTags(raFile, fileInfo, keyArray, tagArray);
			
			this.dispose();
			
			processed = true;
			if(!isScriptRunning()) {
				MipavUtil.displayInfo("Tags processed successfully for image "+file.getAbsolutePath());
				insertScriptLine();
			}
		} catch(IOException ex) {
			System.err.println("Unable to write to file: "+file.getAbsolutePath());
		}
	}

	@Override
	protected void setGUIFromParams() {
		int numKey = 0;
		Iterator<String> itr = scriptParameters.getParams().keySet().iterator();
		String name = null;
		String keyName = "TagKey";
		while(itr.hasNext()) {
			name = itr.next();
			if(name.contains(keyName)) {
				numKey++;
			}
		}
		
		file = new File(scriptParameters.getParams().getString("DicomFile"));
		DefaultTableModel model = ((DefaultTableModel)tagsTable.getModel());
		for(int i=0; i<numKey; i++) {
			model.addRow(new String[]{
				scriptParameters.getParams().getString("TagKey"+i),
				" ",
				scriptParameters.getParams().getString("TagValue"+i)
			});
		}
	}

	@Override
	protected void storeParamsFromGUI() throws ParserException {
		super.setGUIFromParams();
		scriptParameters.getParams().put(ParameterFactory.newParameter("DicomFile", file.getAbsolutePath()));
		for(int i=0; i<keyArray.length; i++) {
			scriptParameters.getParams().put(ParameterFactory.newParameter("TagKey"+i, keyArray[i].toString()));
		}
		for(int i=0; i<tagArray.length; i++) {
			scriptParameters.getParams().put(ParameterFactory.newParameter("TagValue"+i, tagArray[i].getValue(true)));
		}
	}

}
