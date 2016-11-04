package gov.nih.mipav.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQItem;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagInfo;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.CheckTreeManager.CheckTreeSelectionModel;
import gov.nih.mipav.view.dialogs.JDialogBase;

/**
 * The panel class used in anonymize image/directory that displays the public
 * tags in a tree format along with check boxes so that the organization between
 * groups is apparent. These tags don't show up in the DICOM Supplement 55 so
 * it is a separate object from that panel. 
 * 
 * A lot of the code in here is replicated from the Private tags version (which
 * was written first), so any key differences are pointed out. If there is
 * not enough information here, go to the private tags version to look for 
 * further comments.
 * 
 * For information on the tree classes used, see CheckTreeManager (and the URL 
 * in that file)
 * @see CheckTreeManager
 * @author wangvg
 *
 */

public class JPanelAnonymizePublicTags extends JPanel implements ActionListener{

	/**
	 * 
	 */
	private static final long serialVersionUID = -4032253276365568162L;

	private CheckTreeManager checkTree;
	
	private JTree tree;
	
	private ArrayList<FileDicomKey> keyList;
	
	private ArrayList<String> tagList;
	
	private HashSet<String> suppTags;
	
	/**
	 * The default constructor that occurs in the anonymize directory
	 * dialog before a profile has been loaded to populate the tree
	 */
	
	public JPanelAnonymizePublicTags(){
		super();
		//add(new JLabel("Load profile for public tags"));
		
		suppTags = new HashSet<String>();
		for(int i=0;i<FileInfoDicom.anonymizeTagIDs.length;i++){
			suppTags.add(FileInfoDicom.anonymizeTagIDs[i]);
		}
		
		tree = createTreeFromDictionary();
		
		checkTree = new CheckTreeManager(tree);
		
		//Build the layout
		removeAll();
		setLayout(new GridBagLayout());
		setBorder(JDialogBase.buildTitledBorder("Check the fields to anonymize:"));
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.fill = GridBagConstraints.BOTH;

		JScrollPane treeView = new JScrollPane(tree, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		treeView.setViewportView(tree);
		add(treeView, gbc);

		gbc.gridy = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;


		JPanel buttonPanel = new JPanel();
		//buttonPanel.setBorder(new EmptyBorder(0,0,3,0));
		JButton checkButton = new JButton("Select all");
		checkButton.setActionCommand("publicAll");
		checkButton.setFont(MipavUtil.font12B);
		checkButton.setPreferredSize(new Dimension(85, 30));
		buttonPanel.add(checkButton, BorderLayout.WEST);
		checkButton.addActionListener(this);

		JButton unCheckButton = new JButton("Clear");
		unCheckButton.setActionCommand("publicClear");
		unCheckButton.setFont(MipavUtil.font12B);
		unCheckButton.setPreferredSize(new Dimension(85, 30));
		unCheckButton.addActionListener(this);
		buttonPanel.add(unCheckButton, BorderLayout.EAST);

		add(buttonPanel, gbc);

		tree.expandRow(0);
		/*for (int i = 2; i < tree.getRowCount(); i++) {
			tree.collapseRow(i);
		}*/
		
	}
	
	/**
	 * The constructor used in the anonymize image dialog that will 
	 * populate the tree and assign the layout based on the file info.
	 * @param img
	 * @param seqTags
	 */
	public JPanelAnonymizePublicTags(ModelImage img, Vector<FileDicomSQItem> seqTags){
		super();
		
		suppTags = new HashSet<String>();
		for(int i=0;i<FileInfoDicom.anonymizeTagIDs.length;i++){
			suppTags.add(FileInfoDicom.anonymizeTagIDs[i]);
		}
		
		setLayout(new GridBagLayout());
		setBorder(JDialogBase.buildTitledBorder("Check the fields to anonymize:"));
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.fill = GridBagConstraints.BOTH;
		
		tree = createPublicKeyTree(img, seqTags);
		
		checkTree = new CheckTreeManager(tree);
		JScrollPane treeView = new JScrollPane(tree, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		treeView.setViewportView(tree);
		add(treeView, gbc);

		gbc.gridy = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;

		JPanel buttonPanel = new JPanel();
		JButton checkButton = new JButton("Select all");
		checkButton.setActionCommand("publicAll");
		checkButton.setFont(MipavUtil.font12B);
		checkButton.setPreferredSize(new Dimension(85, 30));
		buttonPanel.add(checkButton, BorderLayout.WEST);
		checkButton.addActionListener(this);

		JButton unCheckButton = new JButton("Clear");
		unCheckButton.setActionCommand("publicClear");
		unCheckButton.setFont(MipavUtil.font12B);
		unCheckButton.setPreferredSize(new Dimension(85, 30));
		unCheckButton.addActionListener(this);
		buttonPanel.add(unCheckButton, BorderLayout.EAST);

		add(buttonPanel, gbc);

		for (int i = 0; i < tree.getRowCount(); i++) {
			tree.expandRow(i);
		}
	}
	
	public void populateFromProfile(ArrayList<FileDicomKey> keys){
		
		if(keys.isEmpty())
			return;
		
		Collections.sort(keys);
		
		int offset = 1;
		int i = 0;
		
		String currGrp = "";
		
		ArrayList<Integer> selectedList = new ArrayList<Integer>();
		
		for(FileDicomKey k : keys){
			while(true){
				FileDicomKey treeKey = keyList.get(i);
				String treeGrp = treeKey.getGroup();
				if(!treeGrp.equals(currGrp)){
					currGrp = treeGrp;
					offset++;
				}
				int compare = k.compareTo(treeKey);
				if(compare == 0){
					selectedList.add(new Integer(i+offset));
					i++;
					break;
				}else if(compare > 0){
					i++;
				}else{//This key isn't in the list for some reason
					break;
				}
			}
		}
		
		TreePath[] paths = new TreePath[selectedList.size()];
		i=0;
		
		for(int j : selectedList){
			TreePath path = tree.getPathForRow(j);
			paths[i] = path;
			i++;
		}
		
		tree.setSelectionPaths(paths);
		
	}
	
	/**
	 * Method used in the anonymize dialog profile that generates the tree
	 * structure from key information passed into the class. Required to
	 * pass in the key string, key name, and whether it was not selected.
	 * This version differs slightly from the private keys version because
	 * of the way the tree is structured. 
	 * @param keys The list of keys to display
	 * @param tags The list of tags that map the to keys
	 * @param selected Which keys were selected
	 */
	public void populateFromProfile(ArrayList<FileDicomKey> keys, ArrayList<String> tags, boolean[] selected){
		
		
		
		if(keys.isEmpty())
			return;
		
		//Build the tree from the input keys
		//Somewhat similar to the "createPrivateKeyTree" method
		//so see that for comments
		keyList = keys;
		tagList = tags;
		String prevGroup = "";
		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Public keys");
		DefaultMutableTreeNode next = null;
		ArrayList<Integer> selectedRows = new ArrayList<Integer>();
		tree = new JTree(top);
		int offset = 1;
		for(int i=0;i<keys.size();i++){
			FileDicomKey k = keys.get(i);
			if(k.getKey().equals("0002,0010") ||
					k.getKey().equals("0018,1310") || //do NOT anonymize Transfer Syntax
					k.getGroup().equals("0028")) //This group contains a lot of image info
				continue; //Shouldn't let user remove these tags/groups
			String t = tags.get(i);
			if(!k.getGroup().equals(prevGroup)){
				String title = "Group (" + k.getGroup() +") ";
				if(next != null)
					top.add(next);
				next = new DefaultMutableTreeNode(title);
				prevGroup = k.getGroup();
				offset++;
			}

			String title = "(" + k.getElement() + ") " + t;
			DefaultMutableTreeNode child = new DefaultMutableTreeNode(title);
			next.add(child);
			
			if(selected[i])
				selectedRows.add(i+offset);
		}
		top.add(next);
		checkTree = new CheckTreeManager(tree);
		
		//Build the layout
		removeAll();
		setLayout(new GridBagLayout());
		setBorder(JDialogBase.buildTitledBorder("Check the fields to anonymize:"));
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.fill = GridBagConstraints.BOTH;

		JScrollPane treeView = new JScrollPane(tree, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		treeView.setViewportView(tree);
		add(treeView, gbc);

		gbc.gridy = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;


		JPanel buttonPanel = new JPanel();
		//buttonPanel.setBorder(new EmptyBorder(0,0,3,0));
		JButton checkButton = new JButton("Select all");
		checkButton.setActionCommand("privateAll");
		checkButton.setFont(MipavUtil.font12B);
		checkButton.setPreferredSize(new Dimension(85, 30));
		buttonPanel.add(checkButton, BorderLayout.WEST);
		checkButton.addActionListener(this);

		JButton unCheckButton = new JButton("Clear");
		unCheckButton.setActionCommand("privateClear");
		unCheckButton.setFont(MipavUtil.font12B);
		unCheckButton.setPreferredSize(new Dimension(85, 30));
		unCheckButton.addActionListener(this);
		buttonPanel.add(unCheckButton, BorderLayout.EAST);

		add(buttonPanel, gbc);

		for (int i = 0; i < tree.getRowCount(); i++) {
			tree.expandRow(i);
		}
		
		TreePath[] paths = new TreePath[selectedRows.size()];
		for(int i=0;i<selectedRows.size();i++){
			paths[i] = tree.getPathForRow(selectedRows.get(i));
		}
		removeAllPaths();
		checkTree.getSelectionModel().addSelectionPaths(paths);
	}
	
	private JTree createTreeFromDictionary(){
		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Public keys");
		JTree keyTree = new JTree(top);
		
		Hashtable<FileDicomKey, FileDicomTagInfo> table = DicomDictionary.getDicomTagTable();
		FileDicomKey[] keys = DicomDictionary.sortTagKeys(table);
		
		keyList = new ArrayList<FileDicomKey>();
		tagList = new ArrayList<String>();
		
		String currGrp = "";
		//String nodeStr = "Group (" + currGrp + ")";
		DefaultMutableTreeNode currNode = null; 
		//top.add(currNode);
		
		for(int i=0;i<keys.length;i++){
			FileDicomKey k = keys[i];
			if(k.getKey().equals("0002,0010") ||
					k.getKey().equals("0018,1310") || //do NOT anonymize Transfer Syntax
					k.getGroup().equals("0028") ||
					suppTags.contains(k.getKey())) //This group contains a lot of image info
				continue;
			keyList.add(k);
			if(!k.getGroup().equals(currGrp)){
				currGrp = k.getGroup();
				String nodeStr = "Group (" + currGrp + ")";
				currNode = new DefaultMutableTreeNode(nodeStr);
				top.add(currNode);
			}
			FileDicomTagInfo info = table.get(k);
			tagList.add(info.getName());
			String childStr = "(" + k.getElement() + ") " + info.getName();
			DefaultMutableTreeNode child = new DefaultMutableTreeNode(childStr);
			currNode.add(child);
			
		}
		
		return keyTree;
	}
	
	/**
	 * Method to populate the tree structure from the input image 
	 * and provided sequence tags. 
	 * @param image
	 * @param seqTags
	 * @return
	 */
	private JTree createPublicKeyTree(ModelImage image, Vector<FileDicomSQItem> seqTags){
	
		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Public keys");
		JTree keyTree = new JTree(top);
		
		FileInfoDicom info = (FileInfoDicom) image.getFileInfo(0);
		FileDicomTagTable table = info.getTagTable();
		//Place all the keys into a hashset
		Hashtable<FileDicomKey, FileDicomTag> hash = table.getTagList();
		Set<FileDicomKey> keys = new LinkedHashSet<FileDicomKey>( hash.keySet());
		//Add all the tags within a sequence into the hash set (so that any
				//keys already in the private list aren't added)
		for(FileDicomSQItem s : seqTags){
			keys.addAll(s.getTagList().keySet());
		}
		
		keyList = new ArrayList<FileDicomKey>();
		tagList = new ArrayList<String>();
		
		//Hash to separate out groups and elements out
		//Hash keys are the DICOM keys, while the values are lists
		//containing all the keys in that group
		Hashtable<String, ArrayList<FileDicomKey>> groups = new Hashtable<String, ArrayList<FileDicomKey>>();
		
		//No equivalent element number for group name in public keys, so instead
		//of making the root node include the group name, just use the group
		//number as there is no easy way to differentiate groups
		for(FileDicomKey k : keys){
			if(k.getKey().equals("0002,0010") ||
					k.getKey().equals("0018,1310") || //do NOT anonymize Transfer Syntax
					k.getGroup().equals("0028")) //This group contains a lot of image info
				continue; //Shouldn't let user remove these tags/groups
			String group = k.getGroup();
			int groupNum = k.getGroupNumber();
			if(groupNum%2 == 0){ //Public tags end in an even number
				//Only add the public tag if it isn't already in the supplement 55 list
				if(groups.containsKey(group)){
					ArrayList<FileDicomKey> keyArray = groups.get(group);
					if(!keyArray.contains(k) && !suppTags.contains(k.getKey()))
						keyArray.add(k);	
				}else{
					if(!suppTags.contains(k.getKey())){
						ArrayList<FileDicomKey> keyArray = new ArrayList<FileDicomKey>();
						keyArray.add(k);
						groups.put(group, keyArray);
					}
				}
			}
		}
		
		//Sort out the lists
		//Collections.sort(keyList);
		Set<String> groupKeys = groups.keySet();
		ArrayList<String> groupList = new ArrayList<String>(groupKeys);
		Collections.sort(groupList);
		
		for(String s : groupList){
			ArrayList<FileDicomKey> key = groups.get(s);			
			Collections.sort(key);
			
			//Add all the elements in the group under one node, but
			//without a title like in the private keys version
			String nodeTitle = "Group (" + key.get(0).getGroup() + ")";
			DefaultMutableTreeNode root = new DefaultMutableTreeNode(nodeTitle);
			top.add(root);
			for(FileDicomKey k: key){
				keyList.add(k);
				String subTitle = "(" + k.getElement() + ") ";
				FileDicomTag tag = hash.get(k);
				if(tag == null){
					for(FileDicomSQItem t : seqTags){
						tag = t.getTagList().get(k);
						if(tag != null) break;
					}
				}
				tagList.add(tag.getName());
				DefaultMutableTreeNode keyNode = new DefaultMutableTreeNode(subTitle + tag.getName());
				root.add(keyNode);
			}
			
		}
		
		return keyTree;
	}
	
	private void checkAllPaths(){
		TreePath[] root = new TreePath[1];
		root[0] = tree.getPathForRow(0);
		checkTree.getSelectionModel().addSelectionPaths(root);
		
	}
	
	private void removeAllPaths(){
		TreePath[] paths = new TreePath[tree.getRowCount()];
		for(int i=0;i<tree.getRowCount();i++){
			paths[i] = tree.getPathForRow(i);
		}
		checkTree.getSelectionModel().removeSelectionPaths(paths);
	}
	
	public boolean isEmpty(){
		return keyList.isEmpty();
	}
	
	public ArrayList<FileDicomKey> getKeyList(){
		return keyList;
	}
	
	public ArrayList<String> getTagList(){
		return tagList;
	}
	
	public FileDicomKey[] getSelectedKeys(){
		
		if(checkTree == null)
			return null;
		
		ArrayList<TreePath> collapsed = new ArrayList<TreePath>();
		
		for(int i=0;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			if(tree.isCollapsed(path)){
				collapsed.add(path);
			}
		}
		
		for(TreePath p : collapsed){
			tree.expandPath(p);
		}
		
		ArrayList<Integer> paths = new ArrayList<Integer>();
		CheckTreeSelectionModel model = checkTree.getSelectionModel();
		int offset = 1;
		for(int i=1;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			if(path.getPathCount() == 2){
				offset++;
				continue;
			}
			if(model.isPathSelected(path, true)){
				paths.add(i-offset);
			}
		}
		int length = paths.size();
		if(length == 0)
			return null;
		FileDicomKey[] keys = new FileDicomKey[length];
		for(int i=0;i<length;i++){
			keys[i] = keyList.get(paths.get(i));
		}
		
		for(TreePath p : collapsed){
			tree.collapsePath(p);
		}
		
		return keys;
	}

	public boolean[] getSelectedKeysBool(){
		boolean[] selected = new boolean[keyList.size()];
		int offset = 1;
		
		ArrayList<TreePath> collapsed = new ArrayList<TreePath>();
		
		for(int i=0;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			if(tree.isCollapsed(path)){
				collapsed.add(path);
			}
		}
		
		for(TreePath p : collapsed){
			tree.expandPath(p);
		}
		
		CheckTreeSelectionModel model = checkTree.getSelectionModel();
		for(int i=1;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			if(path.getPathCount() < 3){
				offset++;
				continue;
			}
			selected[i-offset] = model.isPathSelected(path, true);
		}
		
		for(TreePath p : collapsed){
			tree.collapsePath(p);
		}
		
		return selected;
	}
	
	public void setSelectedKeys(ArrayList<FileDicomKey> keys){
		ArrayDeque<FileDicomKey> keyStack = new ArrayDeque<FileDicomKey>(keys);
		ArrayList<Integer> selected = new ArrayList<Integer>();

		int offset = 1;
		String prevGroup = "";
		
		//Both lists should be sorted already
		for(int i=0;i<keyList.size();i++){
			if(keyStack.isEmpty())
				break;
			FileDicomKey k = keyList.get(i);
			
			FileDicomKey q = keyStack.peek(); //Keys made from strings
			
			if(!k.getGroup().equals(prevGroup)){
				prevGroup = k.getGroup();
				offset++;
			}
			
			while(compare(k,q) > 0){
				keyStack.poll();
				q = keyStack.peek();
			}
			if(k.equals(q)){
				keyStack.poll();
				selected.add(i+offset);
			}
		}
		
		ArrayList<TreePath> collapsed = new ArrayList<TreePath>();
		
		for(int i=0;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			if(tree.isCollapsed(path)){
				collapsed.add(path);
			}
		}
		
		for(TreePath p : collapsed){
			tree.expandPath(p);
		}
		
		TreePath[] selectedPaths = new TreePath[selected.size()];
		for(int i=0;i<selected.size();i++){
			selectedPaths[i] = tree.getPathForRow(selected.get(i));
		}
		
		removeAllPaths();
		checkTree.getSelectionModel().addSelectionPaths(selectedPaths);
		
		for(TreePath p : collapsed){
			tree.collapsePath(p);
		}
		
	}

	public int compare(FileDicomKey k, FileDicomKey q){
		int gdiff = k.getGroup().compareTo(q.getGroup());
		int ediff = k.getElement().compareTo(q.getElement());
		if(gdiff != 0)
			return gdiff;
		else return ediff;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub
		String command = e.getActionCommand();
		if(command.equals("publicAll"))
			checkAllPaths();
		else if(command.equals("publicClear"))
			removeAllPaths();
	}
}
