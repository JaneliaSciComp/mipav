package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagInfo;
import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.PrivateDicomDictionary;
import gov.nih.mipav.model.file.PrivateFileDicomKey;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.view.MipavUtil;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Set;

import javax.swing.table.DefaultTableModel;

public class JDialogDicomTagMultiEditor extends JDialogDicomTagSelector {

	protected File file;

	protected FileInfoDicom fileInfo;
	
	public static final String SAVE = "Process";
	
	private boolean processed = false;

	private FileDicomKey[] keyArray;

	private FileDicomTag[] tagArray;
	
	/** Blank constructor needed for scripting */
	public JDialogDicomTagMultiEditor() {
		super(null, true);
		
		createFileInfo(null);
		
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
	
	private FileInfoDicom createFileInfo(File f) {
		FileInfoDicom fileInfo = null;
		try {
			if(f != null) {
	        	FileDicom readDicom = new FileDicom(f.getAbsolutePath());
	        	boolean success = readDicom.readHeader(true);
	        	if(success) {
	        		fileInfo = (FileInfoDicom)readDicom.getFileInfo();
	        		Hashtable<FileDicomKey, FileDicomTag> tagList = fileInfo.getTagTable().getTagList();
	        		this.tagList = tagList;
	        		
	        		this.fileInfo = fileInfo;
	        	}
			} else {
				tagList = new Hashtable<FileDicomKey, FileDicomTag>();
				setTagList(tagList);
			}
    	} catch(IOException e) {
    		System.err.println("Unable to read dicom file: "+f.getAbsolutePath());
    	}
		
		return fileInfo;
		
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals(closeButton.getActionCommand()) && !processed) {
			callAlgorithm();
		} else {
			super.actionPerformed(e);
		}
	}

	@Override
	protected void callAlgorithm() {
		processFile(file);
		
		if(!isScriptRunning()) {
			MipavUtil.displayInfo("Tags processed successfully for image/directory "+file.getAbsolutePath());
			insertScriptLine();
		}

	}
	
	private void processFile(File f) {
		if(f.isDirectory()) {
			for(File fSub : f.listFiles()) {
				processFile(fSub);
			}
		} else {
			processSlice(f);
		}
	}
	
	private void processSlice(File f) {
		System.out.println("Processing file: "+f.getAbsolutePath());
		createFileInfo(f);
		FileDicomKey key = null;
		try {
			FileDicom writeDicom = new FileDicom(f.getAbsolutePath());
			int length = tagsTable.getRowCount();
			keyArray = new FileDicomKey[length];
			tagArray = new FileDicomTag[length];
			Set<Entry<FileDicomKey, FileDicomTag>> entrySet = tagList.entrySet();
tableItr:	for(int i=0; i<length; i++) {
				key = new FileDicomKey(tagsTable.getValueAt(i, 0).toString());
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
						if(key.getGroupNumber() % 2 == 1) { //if dicom tag is private
							if(!key.getElement().equals("0010")) {
								String publisher = PrivateFileDicomKey.NO_PUBLISHER;
								String group = key.getGroup();
								for(int j=0; j<keyArray.length; j++) {
									if(keyArray[j].getGroup().equals(group) && keyArray[j].getElement().equals("0010")) {
										publisher = tagsTable.getValueAt(j, 2).toString();
										break;
									}
								}
								if(publisher.equals(PrivateFileDicomKey.NO_PUBLISHER)) {
									FileDicomTag pubTag = fileInfo.getTagTable().get(new FileDicomKey(group+",0010"));
									if(pubTag != null) {
										publisher = pubTag.getValue(true).toString();
									}
								}
								if(publisher.equals(PrivateFileDicomKey.NO_PUBLISHER)) {
									MipavUtil.displayError("Unable to add tag "+keyArray[i].toString()+", no pubilsher specified");
									continue tableItr;
								} else {
									keyArray[i] = new PrivateFileDicomKey(publisher, keyArray[i].toString());
									tagInfo = PrivateDicomDictionary.getInfo((PrivateFileDicomKey)keyArray[i]);
								}	
							} else { //is publisher tag
								tagInfo = new FileDicomTagInfo(key, VR.LO, 1, "Publisher", "Publisher");
							}
						}
						//TODO: Implement method for getting unknown dicom tag info, VR: UN?
					}
					tagArray[i] = new FileDicomTag(tagInfo);
				}
				
				tagArray[i].setValue(tagsTable.getValueAt(i, 2));
			}
			
			RandomAccessFile raFile = new RandomAccessFile(f, "rw");
		
			writeDicom.writeTags(raFile, fileInfo, keyArray, tagArray);
			
			this.dispose();
			
			processed = true;
			
		} catch(IOException ex) {
			MipavUtil.displayError("Unable to write to file: "+f.getAbsolutePath());
		} catch(Exception ex1) { 
			if(key != null) {
				MipavUtil.displayError("Key could not be saved: "+key);
			}
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
		
		if(!file.isDirectory()) {
			FileDicom readDicom = null;
			try {
        		if(file.isDirectory()) {
	        		readDicom = searchForDicom(file);
	        	} else {
	        		readDicom = new FileDicom(file.getAbsolutePath());
	        	}
		    	boolean success = readDicom.readHeader(true);
		    	if(success) {
		    		FileInfoDicom fileInfo = (FileInfoDicom)readDicom.getFileInfo();
		    		
		    		this.fileInfo = fileInfo;
		    	}
			} catch(IOException io) {
				System.err.println("Unable to read file: "+file.getAbsolutePath());
			}
		}
		
		DefaultTableModel model = ((DefaultTableModel)tagsTable.getModel());
		final FileDicomKey[] tagKeys = new FileDicomKey[numKey];
		FileDicomKey[] tagKeysBuffer = new FileDicomKey[numKey];
		String[] tagValue = new String[numKey];
		String[] tagValueBuffer = new String[numKey];
		Integer[] indexArray = new Integer[numKey];
		
		for(int i=0; i<numKey; i++) {
			tagKeys[i] = new FileDicomKey(scriptParameters.getParams().getString("TagKey"+i));
			tagKeysBuffer[i] = tagKeys[i];
			tagValue[i] = scriptParameters.getParams().getString("TagValue"+i);
			tagValueBuffer[i] = tagValue[i];
			indexArray[i] = i;
		}
		
		Arrays.sort(indexArray, new Comparator<Integer>() {
			@Override
			public int compare(Integer arg0, Integer arg1) {
				return tagKeys[arg0].compareTo(tagKeys[arg1]);
			}
		});
		
		for(int i=0; i<numKey; i++) {
			tagKeys[i] = tagKeysBuffer[indexArray[i]];
			tagValue[i] = tagValueBuffer[indexArray[i]];
		}
		
		for(int i=0; i<numKey; i++) {
			model.addRow(new String[]{
				tagKeys[i].toString(),
				" ",
				tagValue[i]
			});
		}
	}
	
	private void sortTags(final FileDicomKey[] keyArray, Object[] tagArray) {
		Integer[] indexArray = new Integer[keyArray.length];
		FileDicomKey[] keyArrayBuffer = new FileDicomKey[keyArray.length];
		Object[] tagArrayBuffer = new Object[tagArray.length];
		for(int i=0; i<keyArray.length; i++) {
			indexArray[i] = i;
			keyArrayBuffer[i] = keyArray[i];
			tagArrayBuffer[i] = tagArray[i];
		}
		
		Arrays.sort(indexArray, new Comparator<Integer>() {
			@Override
			public int compare(Integer arg0, Integer arg1) {
				return keyArray[arg0].compareTo(keyArray[arg1]);
			}
		});
		
		for(int i=0; i<keyArray.length; i++) {
			keyArray[i] = keyArrayBuffer[indexArray[i]];
			tagArray[i] = tagArrayBuffer[indexArray[i]];
		}
	}
	
	public static FileDicom searchForDicom(File file) throws IOException {
    	FileDicom readDicom = null;
    	for(File f : file.listFiles()) {
			if(!f.isDirectory() && FileUtility.getExtension(f.getAbsolutePath()).toLowerCase().equals(".dcm")) {
				readDicom = new FileDicom(f.getAbsolutePath());
				break;
			} else if(f.isDirectory()) {
				searchForDicom(f);
			}
		}
    	
    	return readDicom;
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

