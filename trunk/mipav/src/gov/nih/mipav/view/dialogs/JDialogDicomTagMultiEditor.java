package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagInfo;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.PrivateDicomDictionary;
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
	
	/** Blank constructor needed for scripting */
	public JDialogDicomTagMultiEditor() {
		super(null, true);
		
		createFileInfo();
		
		closeButton.setActionCommand(SAVE);
		closeButton.setText(SAVE);
	}
	
	public JDialogDicomTagMultiEditor(Hashtable<FileDicomKey, FileDicomTag> tagList, JDialogBase parent, 
			boolean isStandalone, File file, FileInfoDicom fileInfo) {
		super(parent, isStandalone);
		
		this.file = file;
		this.fileInfo = fileInfo;
		
		this.setTagList(tagList);
		
		closeButton.setActionCommand(SAVE);
		closeButton.setText(SAVE);
	}
	
	private FileInfoDicom createFileInfo() {
		FileInfoDicom fileInfo = null;
		try {
			if(file != null) {
	        	FileDicom readDicom = new FileDicom(file.getAbsolutePath());
	        	boolean success = readDicom.readHeader(true);
	        	if(success) {
	        		fileInfo = (FileInfoDicom)readDicom.getFileInfo();
	        		Hashtable<FileDicomKey, FileDicomTag> tagList = fileInfo.getTagTable().getTagList();
	        		setTagList(tagList);
	        		
	        		this.fileInfo = fileInfo;
	        	}
			} else {
				tagList = new Hashtable<FileDicomKey, FileDicomTag>();
				setTagList(tagList);
			}
    	} catch(IOException e) {
    		System.err.println("Unable to read dicom file: "+file.getAbsolutePath());
    	}
		
		return fileInfo;
		
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
		System.out.println("Processing file: "+file.getAbsolutePath());
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
				keyArray[i] = key;
				
	
	keySearch:	while(keyItr.hasNext()) {
					entry = keyItr.next();
					if(entry.getKey().equals(key)) {
						
						tagArray[i] = entry.getValue();
						
						break keySearch;
					}
				}
				
				if(tagArray[i] == null) {
					FileDicomTagInfo tagInfo = DicomDictionary.getDicomTagTable().get(keyArray[i]);
					if(tagInfo == null) {
						//TODO: Implement method for getting unknown dicom tag info
					}
					tagArray[i] = new FileDicomTag(tagInfo);
				}
				
				tagArray[i].setValue(tagsTable.getValueAt(i, 2));
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
		try {
			FileDicom readDicom = new FileDicom(file.getAbsolutePath());
	    	boolean success = readDicom.readHeader(true);
	    	if(success) {
	    		FileInfoDicom fileInfo = (FileInfoDicom)readDicom.getFileInfo();
	    		
	    		this.fileInfo = fileInfo;
	    	}
		} catch(IOException io) {
			System.err.println("Unable to read file: "+file.getAbsolutePath());
		}
		
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
