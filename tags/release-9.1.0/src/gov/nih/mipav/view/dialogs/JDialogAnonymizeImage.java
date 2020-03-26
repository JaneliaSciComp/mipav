package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptRunner;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Properties;
import java.util.Set;
import java.util.Vector;

import javax.swing.*;


/**
 * REALLY like JDialogRemoveSlices except it does not have an "algorithm"
 *
 * <p>builds a simple dialog which imbeds a JPanelAnonymizeImage. The JDialogAnonymizeImage will gray-out all entries
 * which are not available in the FileInfoDicom that goes with the given image. "Okay" then proceeds to remove or filter
 * the selected tags from the image.</p>
 *
 * @author   parsonsd
 * @version  0.9
 * @see      JDialogRemoveSlices
 */
public class JDialogAnonymizeImage extends JDialogScriptableBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5845185607194185354L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JPanelAnonymizeImage checkboxPanel; //
    
    private JPanelAnonymizePrivateTags privateTagsPanel;
    
    private JPanelAnonymizePublicTags publicTagsPanel;

    /** DOCUMENT ME! */
    private ModelImage image;
    
    private Vector<FileDicomSQItem> seqTags;
    
    private JCheckBox removeBox;
    
    //Scripting variables
    
    private String scriptStringParams;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    public JDialogAnonymizeImage(){
    	super();
    };
    
    /**
     * constructor to build a dialog allowing user to find which tags are available to anonymize.
     *
     * @param  parent  Parent dialog
     * @param  img     Image to anonymize
     */
    public JDialogAnonymizeImage(Dialog parent, ModelImage img) {
        super(parent, true); // make modal

        if (!img.isDicomImage() && !img.isMincImage()) {
            return;
        }

        image = img;
        setTitle("Anonymize sensitive info");
        setForeground(Color.black);
        
        getSequenceTags();
        privateTagsPanel = new JPanelAnonymizePrivateTags(img, seqTags);
        publicTagsPanel = new JPanelAnonymizePublicTags(img, seqTags);

        if(privateTagsPanel.isEmpty()){
        	privateTagsPanel.removeAll();
        	JLabel label = new JLabel("No private tags");
        	privateTagsPanel.add(label);
        }
        if(publicTagsPanel.isEmpty()){
        	publicTagsPanel.removeAll();
        	JLabel label = new JLabel("No public tags");
        	publicTagsPanel.add(label);
        }

        JTabbedPane tabs = new JTabbedPane();

        // place a check-box list in here
        checkboxPanel = new JPanelAnonymizeImage();

        if (img.isDicomImage()) {
        	checkboxPanel.setDicomInfo((FileInfoDicom) img.getFileInfo(0));
        } else if (img.isMincImage()) {
        	checkboxPanel.setMincInfo((FileInfoMinc) img.getFileInfo(0));
        }

        mainDialogPanel.add(checkboxPanel, BorderLayout.CENTER);

        //getContentPane().add(mainDialogPanel);

        tabs.insertTab("Supp. 55 tag options", null, mainDialogPanel, "Supp. 55 Tag Selection", 0);
        tabs.insertTab("Public tag options", null, publicTagsPanel, "Public Tag Selection", 1);
        tabs.insertTab("Private tag options", null, privateTagsPanel, "Private Tag Selection", 2);
        //tabs.insertTab("Private tag options", null, privateTagsPanel, "Private Tag Selection", 2);
        
        JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;

        mainPanel.add(tabs, gbc);
        
        gbc.gridy = 1;
        gbc.weighty = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        
        JPanel boxPanel = new JPanel();
        removeBox = new JCheckBox("Remove public tag values");
        removeBox.setFont(serif12);
        removeBox.setForeground(Color.black);
        boxPanel.add(removeBox);
        mainPanel.add(boxPanel, gbc);
        
        gbc.gridy = 2;
        
        JPanel presetButtonPanel = new JPanel();
        presetButtonPanel.setForeground(Color.black);
        
        JButton presetButton = new JButton("Anonymize from presets");
        //presetButton.setPreferredSize(new Dimension(150, 35));
        presetButton.setActionCommand("preset");
        presetButton.setFont(serif12B);
        presetButton.addActionListener(this);
        presetButtonPanel.add(presetButton);
        mainPanel.add(presetButtonPanel, gbc);
        
        gbc.gridy = 3;
        mainPanel.add(getOKCancelPanel(), gbc);
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        
        setResizable(true); // since locations are hard-coded we are not checking for different sizes. prevent user from
                            // changing
        addWindowListener(this); // check for events
        pack();
        setSize(425, 825);
        setVisible(true); // let someone see the dialog.
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and does the routine.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource(); // whatever the user clicked on
        String command = event.getActionCommand();

        if (source == OKButton) { // if user pressed "expurgate" ...

            int anonymizeChoice;

            if (checkboxPanel.getNumberSelected() == 0 &&
            		privateTagsPanel.isEmpty() && publicTagsPanel.isEmpty()) {
                MipavUtil.displayError("No fields were selected!  Select a field.");
            } else {

                try {
                    JOptionPane.getRootFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                } catch (FileNotFoundException error) {
                    Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                                      ">.  Check that this file is available.\n");
                    System.err.println("Exception ocurred while getting <" + error.getMessage() +
                                       ">.  Check that this file is available.\n");
                }
                
                //Need to find sequence tags and use it for both the public and private tag methods

                anonymizeChoice = JOptionPane.showConfirmDialog(null,
                                                                "You are about to make a permament change to the entire image.\n" +
                                                                "Are you sure you want to remove the sensitive data?",
                                                                "Confirm for Anonymize", JOptionPane.YES_NO_OPTION,
                                                                JOptionPane.QUESTION_MESSAGE);

                if (anonymizeChoice == JOptionPane.YES_OPTION) {
                    image.anonymize(checkboxPanel.getSelectedList(), true, removeBox.isSelected()); // anonymize the image of sensitive data
                    image.anonymizeSequenceTags(checkboxPanel.getSelectedList(), seqTags, removeBox.isSelected());
                    
                    FileDicomKey[] keys = privateTagsPanel.getSelectedKeys();
                    if(keys != null){
                    	image.removePrivateTags(keys);
                    	image.removePrivateSequenceTags(keys, seqTags);
                    }
                    keys = publicTagsPanel.getSelectedKeys();
                    if(keys != null){
                    	image.anonymizePublicTags(keys, removeBox.isSelected());
                    	image.anonymizePublicSequenceTags(keys, seqTags, removeBox.isSelected());
                    }
                    
                    setVisible(false); // Hide dialog
                    insertScriptLine();

                    getOwner().dispose();
                    image.getParentFrame().about();

                }
            }
        } else if (source == cancelButton) {
            dispose();
        } else if (command.equals("LoadProfile")){
        	ArrayList<String> profiles = getProfiles();
        	if(profiles.size() > 0) {
                Object select = JOptionPane.showInputDialog(this, "Choose the profile to load", "Load profile", JOptionPane.INFORMATION_MESSAGE, null, profiles.toArray(), profiles.toArray()[0]);
                if(select != null) {
                    loadProfile(select.toString());
                }
            } else {
                JOptionPane.showMessageDialog(this, "No available profiles");
            }
        } else if (command.equals("SaveProfile")){
        	int doSave = JOptionPane.NO_OPTION;
            String str = String.valueOf(0);
            while(doSave == JOptionPane.NO_OPTION) {
                str = JOptionPane.showInputDialog(this, "Name the profile");
                if(str != null && str.length() == 0) {
                    doSave = JOptionPane.NO_OPTION;
                } else if(Preferences.getProperty("profileAnonymizeDICOMImage"+str) != null) {
                    doSave = JOptionPane.showConfirmDialog(this, "Profile "+str+" already exists.  Overwrite?", "Overwrite?", JOptionPane.YES_NO_CANCEL_OPTION);
                } else {
                    doSave = JOptionPane.YES_OPTION;
                }
            }
            if(doSave == JOptionPane.YES_OPTION)
            	saveProfile(str);
        } else if(command.equals("preset")){
        	new JDialogAnonymizePresets(this, true, image);
        }
        else {
            super.actionPerformed(event);
        }
    }

    /**
     * makes the panel which displays the ok and cancel buttons.
     *
     * @return  DOCUMENT ME!
     */
    protected JPanel getOKCancelPanel() {
        JPanel okCancelPanel = new JPanel();

        // Make & set the OK (purge) and Cancel buttons--place outside the border
        buildOKButton();

        // if there are no fields which the user may select, turn off the OK button
        if (checkboxPanel.getNumberVisible() == 0) { // if no tags are visible
            OKButton.setEnabled(false);
        }

        okCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        okCancelPanel.add(cancelButton, BorderLayout.EAST);

        return okCancelPanel;
    }
    
    /**
     * Method to retrieve the sequence tags from the file info. This will
     * be used later to check for the other tags when anonymizing/removing
     * information so that things aren't left in erroneously. 
     */
    private void getSequenceTags(){
    	
    	seqTags = new Vector<FileDicomSQItem>();
    	FileInfoDicom info = (FileInfoDicom)image.getFileInfo()[0];
    	Hashtable<FileDicomKey, FileDicomTag> hash = info.getTagTable().getTagList();
    	Set<FileDicomKey> keys = hash.keySet();
    	for(FileDicomKey k : keys){
    		Object obj = hash.get(k).getValue(false);
    		if(obj instanceof FileDicomSQ){
    			FileDicomSQ seq = (FileDicomSQ) obj;
    			Vector<FileDicomSQItem> vec = seq.getSequence();
    			seqTags.addAll(vec);
    		}
    	}
    	
    }
    
    /**
     * Parses through the profile in the MIPAV preferences to
     * determine which tags were selected and set them correctly
     * in the respective panels. Parsing occurs here while the
     * actual selection determination is within each panel. 
     * @param name
     */
    private void loadProfile(String name){
    	String profile = "profileAnonymizeDICOMImage" + name;
    	String value = Preferences.getProperty(profile);
    	String[] split = value.split(";");
    	int i;
    	boolean[] publicList = new boolean[FileInfoDicom.anonymizeTagIDs.length];
    	for(i=0;i<publicList.length;i++){
    		if(split[i].equals("t"))
    			publicList[i] = true;
    		else if(split[i].equals("f"))
    			publicList[i] = false;
    		else {
    			MipavUtil.displayWarning("Warning: standard anonymized tags may have changed. "
    					+ "There are fewer fields in profile than expected.");
    			publicList[i] = false;
    			break;
    		}
    	}
    	
    	if(split[i].equals("f") || split[i].equals("t")){
    		MipavUtil.displayWarning("Warning: standard anonymized tags may have changed. "
    				+ "There are more fields in profile than expected.");
    	}
    	while(split[i].equals("f") || split[i].equals("t"))
    		i++;
    	checkboxPanel.setSelectedList(publicList);
    	
    	ArrayList<FileDicomKey> publicKeys = new ArrayList<FileDicomKey>();
    	ArrayList<FileDicomKey> privateKeys = new ArrayList<FileDicomKey>();
    	
    	for(;i<split.length-1;i++){
    		String key = split[i];
    		String group = key.substring(0, key.indexOf(","));
    		int groupNum = Integer.valueOf(group, 0x10);
    		if(groupNum%2==0){
    			publicKeys.add(new FileDicomKey(key));
    		}else{
    			privateKeys.add(new FileDicomKey(key));
    		}
    	}
    	
    	/*ArrayList<FileDicomKey> keys = new ArrayList<FileDicomKey>();
    	ArrayList<FileDicomKey> publicKeys = new ArrayList<FileDicomKey>();
    	ArrayList<FileDicomKey> workingList = keys;
    	for(;i<split.length-1;i+=3){
    		String group = split[i].substring(0, split[i].indexOf(","));
    		int groupNum = Integer.valueOf(group, 0x10);
    		if(groupNum%2==0)
    			workingList = publicKeys;
    		if(split[i+2].equals("t")){
    			String keyString = split[i];
    			workingList.add(new FileDicomKey(keyString));
    		}
    	}*/
    	if(privateKeys.size()>0)
    		privateTagsPanel.setSelectedKeys(privateKeys);
    	if(publicKeys.size()>0)
    		publicTagsPanel.setSelectedKeys(publicKeys);
    	String boxChecked = split[i];
    	if(boxChecked.equals("t"))
    		removeBox.setSelected(true);
    	else removeBox.setSelected(false);
    }
    
    /**
     * Searches for all profiles in the MIPAV preferences that
     * start with profileAnonymizeDICOM. Works for both the
     * anonymize image dialog and the anonymize directory dialog.
     */
    private ArrayList<String> getProfiles(){
    	
    	ArrayList<String> profiles = new ArrayList<String>();
    	Properties props = Preferences.getMipavProps();
    	Set<Object> keys = props.keySet();
    	for(Object o : keys){
    		if(o instanceof String){
    			String s = (String) o;
    			if(s.startsWith("profileAnonymizeDICOMImage")){
    				profiles.add(s.substring(26));
    			}
    		}
    	}
    	return profiles;
    }
    
    /**
     * Saves off a profile into the preferences. This is basically
     * the exact same as what is seen in the anonymize directory
     * version. This is so that the profiles from one image can 
     * be applied across all images in the directory. 
     * @param name
     */
    private void saveProfile(String name){
    	
    	String profile = "profileAnonymizeDICOMImage" + name;
    	StringBuilder hashString = new StringBuilder();
    	String delimiter = ";";
    	
    	boolean[] standardKeys = checkboxPanel.getSelectedList();
    	FileDicomKey[] publicKeys = publicTagsPanel.getSelectedKeys();
    	FileDicomKey[] privateKeys = privateTagsPanel.getSelectedKeys();
    	
    	//For the supplement 55 tags, just save off "t" or "f" since we
    	//already know what it corresponds to
    	for(int i=0;i<FileInfoDicom.anonymizeTagIDs.length;i++){
    		if(standardKeys[i])
    			hashString.append("t");
    		else hashString.append("f");
    		hashString.append(delimiter);
    	}
    	
    	if(publicKeys != null){
	    	for(int i=0;i<publicKeys.length;i++){
	    		FileDicomKey k = publicKeys[i];
	    		hashString.append(k.getKey() + delimiter);
	    	}
    	}
    	
    	if(privateKeys != null){
    		for(int i=0;i<privateKeys.length;i++){
    			FileDicomKey k = privateKeys[i];
    			hashString.append(k.getKey() + delimiter);
    		}
    	}
    	
    	
    	/*
    	ArrayList<FileDicomKey> keyList = privateTagsPanel.getKeyList();
    	ArrayList<String> tagList = privateTagsPanel.getTagList();
    	ArrayList<FileDicomKey> publicKeyList = publicTagsPanel.getKeyList();
    	ArrayList<String> publicTagList = publicTagsPanel.getTagList();
    	
    	
    	//For other public and private tags, you need to save off
    	//the key numbers, name of the key, and whether or not
    	//it was selected
    	for(int i=0;i<privateSelected.length;i++){
    		FileDicomKey k = keyList.get(i);
    		String t = tagList.get(i);

    		hashString.append(k.getKey() + delimiter + t + delimiter);
    		
    		if(privateSelected[i])
    			hashString.append("t");
    		else hashString.append("f");
    		hashString.append(delimiter);
    	}
    	for(int i=0;i<publicSelected.length;i++){
    		FileDicomKey k = publicKeyList.get(i);
    		String t = publicTagList.get(i);

    		hashString.append(k.getKey() + delimiter + t + delimiter);
    		
    		if(publicSelected[i])
    			hashString.append("t");
    		else hashString.append("f");
    		hashString.append(delimiter);
    	}
    	*/
    	if(removeBox.isSelected())
    		hashString.append("t");
    	else hashString.append("f");
    	Preferences.setProperty(profile, hashString.toString());
    	
    }
    
    //Only for scripting, just basically what occurs in action performed, plus
    //a couple of extra steps to carry out scripting things
    
	@Override
	protected void callAlgorithm() {
		
		String oldName = image.getImageName();
		
		getSequenceTags();
    	String[] split = scriptStringParams.split(";");
    	int i = 0;
    	boolean[] publicList = new boolean[FileInfoDicom.anonymizeTagIDs.length];
    	for(int j = 0;j<publicList.length;j++){
    		if(split[i].equals(FileInfoDicom.anonymizeTagIDs[j])){
    			publicList[j] = true;
    			i++;
    		}
    	}
    	
    	ArrayList<FileDicomKey> keys = new ArrayList<FileDicomKey>();
    	ArrayList<FileDicomKey> publicKeys = new ArrayList<FileDicomKey>();
    	ArrayList<FileDicomKey> workingList = keys;
    	for(;i<split.length;i++){
    		String group = split[i].substring(0, split[i].indexOf(","));
    		int groupNum = Integer.valueOf(group, 0x10);
    		if(groupNum%2==0)
    			workingList = publicKeys;
    		workingList.add(new FileDicomKey(split[i]));
    	}

		image.anonymize(publicList, true, removeBox.isSelected()); // anonymize the image of sensitive data
        image.anonymizeSequenceTags(publicList, seqTags, removeBox.isSelected());
        
        if(keys.size()>0){
        	FileDicomKey[] privateKeys = new FileDicomKey[keys.size()];
        	keys.toArray(privateKeys);
        	image.removePrivateTags(privateKeys);
        	image.removePrivateSequenceTags(privateKeys, seqTags);
        }
    	if(publicKeys.size()>0){
    		FileDicomKey[] keysArray = new FileDicomKey[publicKeys.size()];
    		publicKeys.toArray(keysArray);
    		image.anonymizePublicTags(keysArray, removeBox.isSelected());
        	image.anonymizePublicSequenceTags(keysArray, seqTags, removeBox.isSelected());
    	}
    	
    	ScriptRunner.getReference().getImageTable().changeImageName(oldName, image.getImageName());
    	
    	//anonImage = (ModelImage)image.clone("Anonymous");
	}

	@Override
	protected void setGUIFromParams() {
		
		image = scriptParameters.retrieveInputImage();
		scriptStringParams = scriptParameters.getParams().getString("keys");
		
	}

	/**
	 * Stores all the keys that were selected, but in string
	 * form so that it is easier to pull up later (although
	 * it is not a pretty sight to see in the recorder)
	 */
	@Override
	protected void storeParamsFromGUI() throws ParserException {

		scriptParameters.storeInputImage(image);
		
		StringBuilder hashString = new StringBuilder();
    	String delimiter = ";";
    	
    	boolean[] standardKeys = checkboxPanel.getSelectedList();
    	boolean[] privateSelected = privateTagsPanel.getSelectedKeysBool();
    	boolean[] publicSelected = publicTagsPanel.getSelectedKeysBool();
    	ArrayList<FileDicomKey> keyList = privateTagsPanel.getKeyList();
    	ArrayList<FileDicomKey> publicKeyList = publicTagsPanel.getKeyList();
    	
    	//Simply just write all the key strings out to the parameters
    	for(int i=0;i<FileInfoDicom.anonymizeTagIDs.length;i++){
    		if(standardKeys[i])
    			hashString.append(FileInfoDicom.anonymizeTagIDs[i] + delimiter);
    	}
    	for(int i=0;i<privateSelected.length;i++){
    		FileDicomKey k = keyList.get(i);
    		
    		if(privateSelected[i])
    			hashString.append(k.getKey() + delimiter);
    	}
    	for(int i=0;i<publicSelected.length;i++){
    		FileDicomKey k = publicKeyList.get(i);
    		
    		if(publicSelected[i])
    			hashString.append(k.getKey() + delimiter);
    	}

    	hashString.deleteCharAt(hashString.length()-1);
		scriptParameters.getParams().put(ParameterFactory.newString("keys", hashString.toString()));
		
	}
	
    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {
    	//AlgorithmParameters.storeImageInRunner(anonImage);
    }
    
}
