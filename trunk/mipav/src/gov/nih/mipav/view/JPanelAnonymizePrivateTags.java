package gov.nih.mipav.view;

import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQItem;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.CheckTreeManager.CheckTreeSelectionModel;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Hashtable;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.border.EmptyBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

public class JPanelAnonymizePrivateTags extends JPanel implements ActionListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = -410753580261870012L;
	
	private CheckTreeManager checkTree;
	
	private JTree tree;
	
	private ArrayList<FileDicomKey> keyList;
	
	private ArrayList<String> tagList;
	
	public JPanelAnonymizePrivateTags(){
		super();
		add(new JLabel("Load profile for private tags"));
	}
	
	public JPanelAnonymizePrivateTags(ModelImage img, Vector<FileDicomSQItem> seqTags){
		super();
		
		setLayout(new GridBagLayout());
		setBorder(JDialogBase.buildTitledBorder("Check the fields to anonymize:"));
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.fill = GridBagConstraints.BOTH;

		tree = createPrivateKeyTree(img, seqTags);

		checkTree = new CheckTreeManager(tree);
		JScrollPane treeView = new JScrollPane(tree, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		treeView.setViewportView(tree);
		//treeView.setPreferredSize(new Dimension(300,200));
		add(treeView, gbc);

		gbc.gridy = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;


		JPanel buttonPanel = new JPanel();
		buttonPanel.setBorder(new EmptyBorder(0,0,3,0));
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

		checkAllPaths();
		
		
	}
	
	public void populateFromProfile(ArrayList<FileDicomKey> keys, ArrayList<String> tags, boolean[] selected){
		
		if(keys.isEmpty())
			return;
		
		keyList = keys;
		tagList = tags;
		String prevGroup = "";
		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Private keys");
		DefaultMutableTreeNode next = null;
		ArrayList<Integer> selectedRows = new ArrayList<Integer>();
		tree = new JTree(top);
		for(int i=0;i<keys.size();i++){
			FileDicomKey k = keys.get(i);
			String t = tags.get(i);
			if(selected[i])
				selectedRows.add(i+1);
			if(k.getGroup().equals(prevGroup)){
				String title = "(" + k.getElement() + ") " + t;
				DefaultMutableTreeNode child = new DefaultMutableTreeNode(title);
				next.add(child);
			} else {
				String title = "(" + k.getGroup() +") " + t;
				if(next != null)
					top.add(next);
				next = new DefaultMutableTreeNode(title);
				prevGroup = k.getGroup();
			}
		}
		top.add(next);
		checkTree = new CheckTreeManager(tree);
		
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
		buttonPanel.setBorder(new EmptyBorder(0,0,3,0));
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
		checkTree.getSelectionModel().addSelectionPaths(paths);
	}
	
	public ArrayList<FileDicomKey> getKeyList(){
		return keyList;
	}
	
	public ArrayList<String> getTagList(){
		return tagList;
	}
	
	/*private void createPrivateKeyTree(ModelImage image){
		
		FileInfoDicom info = (FileInfoDicom) image.getFileInfo(0);
		FileDicomTagTable table = info.getTagTable();
		Hashtable<FileDicomKey, FileDicomTag> hash = table.getTagList();
		Set<FileDicomKey> keys = hash.keySet();
		Hashtable<String, ArrayList<FileDicomKey>> groups = new Hashtable<String, ArrayList<FileDicomKey>>();
		for(FileDicomKey k : keys){
			String group = k.getGroup();
			String last = group.substring(group.length() - 1);
			int lastNum = Integer.parseInt(last);
			if(lastNum%2 == 1){
				if(groups.containsKey(group)){
					String element = k.getElement();
					if(element.equals("0010"))//Group name
						groups.get(group).add(0, k);
					else 
						groups.get(group).add(k);
				}else{
					System.err.println(group);
					ArrayList<FileDicomKey> keyList = new ArrayList<FileDicomKey>();
					keyList.add(k);
					groups.put(group, keyList);
				}
			}
		}
		
		Set<String> groupKeys = groups.keySet();
	
		for(String s : groupKeys){
			ArrayList<FileDicomKey> key = groups.get(s);
			FileDicomKey groupNameKey = key.remove(0);
			FileDicomTag groupNameTag = hash.get(groupNameKey);
			String nodeTitle = "(" + groupNameKey.getGroup() + ") " 
					+ groupNameTag.getValue(false);
			//String groupName = groupNameTag.getName();
			ArrayList<String> elements = new ArrayList<String>();
			for(FileDicomKey k: key){
				String subTitle = "(" + k.getElement() + ") ";
				FileDicomTag tag = hash.get(k);
				elements.add(subTitle + tag.getName());
			}
			addGroup(nodeTitle, elements);
			
		}
		
		//Have tree, need to display to test it out first. DO ON MONDAY
	
	}*/
	
	
	
	/*private void createPrivateKeyTree(ModelImage image){
		
		FileInfoDicom info = (FileInfoDicom) image.getFileInfo(0);
		FileDicomTagTable table = info.getTagTable();
		Hashtable<FileDicomKey, FileDicomTag> hash = table.getTagList();
		Set<FileDicomKey> keys = hash.keySet();
		Hashtable<String, ArrayList<FileDicomKey>> groups = new Hashtable<String, ArrayList<FileDicomKey>>();
		for(FileDicomKey k : keys){
			String group = k.getGroup();
			String last = group.substring(group.length() - 1);
			int lastNum = Integer.parseInt(last);
			if(lastNum%2 == 1){
				if(groups.containsKey(group)){
					String element = k.getElement();
					if(element.equals("0010"))//Group name
						groups.get(group).add(0, k);
					else 
						groups.get(group).add(k);
				}else{
					System.err.println(group);
					ArrayList<FileDicomKey> keyList = new ArrayList<FileDicomKey>();
					keyList.add(k);
					groups.put(group, keyList);
				}
			}
		}
		
		Set<String> groupKeys = groups.keySet();
	
		for(String s : groupKeys){
			ArrayList<FileDicomKey> key = groups.get(s);
			FileDicomKey groupNameKey = key.remove(0);
			FileDicomTag groupNameTag = hash.get(groupNameKey);
			String nodeTitle = "(" + groupNameKey.getGroup() + ") " 
					+ groupNameTag.getValue(false);
			//String groupName = groupNameTag.getName();
			ArrayList<String> elements = new ArrayList<String>();
			for(FileDicomKey k: key){
				String subTitle = "(" + k.getElement() + ") ";
				FileDicomTag tag = hash.get(k);
				elements.add(subTitle + tag.getName());
			}
			addGroup(nodeTitle, elements);
			
		}
		
		//Have tree, need to display to test it out first. DO ON MONDAY
	
	}*/
	
	
	
	public FileDicomKey[] getSelectedKeys(){
		ArrayList<Integer> paths = new ArrayList<Integer>();
		CheckTreeSelectionModel model = checkTree.getSelectionModel();
		for(int i=1;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			if(model.isPathSelected(path, true)){
				paths.add(i-1);
			}
		}
		int length = paths.size();
		if(length == 0)
			return null;
		FileDicomKey[] keys = new FileDicomKey[length];
		for(int i=0;i<length;i++){
			keys[i] = keyList.get(paths.get(i));
		}
		
		return keys;
	}

	public boolean[] getSelectedKeysBool(){
		boolean[] selected = new boolean[keyList.size()];
		CheckTreeSelectionModel model = checkTree.getSelectionModel();
		for(int i=1;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			selected[i-1] = model.isPathSelected(path, true);
		}
		
		return selected;
	}

	public boolean isEmpty(){
		return keyList.isEmpty();
	}

	public void setSelectedKeys(ArrayList<FileDicomKey> keys){
		ArrayDeque<FileDicomKey> keyStack = new ArrayDeque<FileDicomKey>(keys);
		ArrayList<Integer> selected = new ArrayList<Integer>();

		//Both lists should be sorted already
		for(int i=0;i<keyList.size();i++){
			if(keyStack.isEmpty())
				break;
			FileDicomKey k = new FileDicomKey(keyList.get(i).getKey()); //Private tags, should convert to regular structure
			FileDicomKey q = keyStack.peek(); //Keys made from strings
			while(k.compareTo(q) > 0){
				keyStack.poll();
				q = keyStack.peek();
			}
			if(k.equals(q)){
				keyStack.poll();
				if(!k.getElement().equals("0010"))
					selected.add(i+1);
			}
			/*if(q.equals(k)){
				keyStack.poll();
				selected.add(i+1);
			} else if(k.compareTo(q) > 0){ //This key from the profile does not exist
				while(k.compareTo(q) > 0){
					keyStack.poll();
					q = keyStack.peek();
				}
			}*/
		}
		TreePath[] selectedPaths = new TreePath[selected.size()];
		for(int i=0;i<selected.size();i++){
			selectedPaths[i] = tree.getPathForRow(selected.get(i));
		}
		
		removeAllPaths();
		checkTree.getSelectionModel().addSelectionPaths(selectedPaths);
		
	}
	
	private void checkAllPaths(){
		/*TreePath[] paths = new TreePath[tree.getRowCount()];
		for(int i=0;i<tree.getRowCount();i++){
			paths[i] = tree.getPathForRow(i);
		}*/
		TreePath[] root = new TreePath[1];
		root[0] = tree.getPathForRow(0);
		checkTree.getSelectionModel().addSelectionPaths(root);
		
	}
	/*private void createPrivateKeyTree(ModelImage image){
		
		FileInfoDicom info = (FileInfoDicom) image.getFileInfo(0);
		FileDicomTagTable table = info.getTagTable();
		Hashtable<FileDicomKey, FileDicomTag> hash = table.getTagList();
		Set<FileDicomKey> keys = hash.keySet();
		Hashtable<String, ArrayList<FileDicomKey>> groups = new Hashtable<String, ArrayList<FileDicomKey>>();
		for(FileDicomKey k : keys){
			String group = k.getGroup();
			String last = group.substring(group.length() - 1);
			int lastNum = Integer.parseInt(last);
			if(lastNum%2 == 1){
				if(groups.containsKey(group)){
					String element = k.getElement();
					if(element.equals("0010"))//Group name
						groups.get(group).add(0, k);
					else 
						groups.get(group).add(k);
				}else{
					System.err.println(group);
					ArrayList<FileDicomKey> keyList = new ArrayList<FileDicomKey>();
					keyList.add(k);
					groups.put(group, keyList);
				}
			}
		}
		
		Set<String> groupKeys = groups.keySet();
	
		for(String s : groupKeys){
			ArrayList<FileDicomKey> key = groups.get(s);
			FileDicomKey groupNameKey = key.remove(0);
			FileDicomTag groupNameTag = hash.get(groupNameKey);
			String nodeTitle = "(" + groupNameKey.getGroup() + ") " 
					+ groupNameTag.getValue(false);
			//String groupName = groupNameTag.getName();
			ArrayList<String> elements = new ArrayList<String>();
			for(FileDicomKey k: key){
				String subTitle = "(" + k.getElement() + ") ";
				FileDicomTag tag = hash.get(k);
				elements.add(subTitle + tag.getName());
			}
			addGroup(nodeTitle, elements);
			
		}
		
		//Have tree, need to display to test it out first. DO ON MONDAY
	
	}*/
	
	
	
	private JTree createPrivateKeyTree(ModelImage image, Vector<FileDicomSQItem> seqTags){
		
		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Private keys");
		JTree keyTree = new JTree(top);
		
		FileInfoDicom info = (FileInfoDicom) image.getFileInfo(0);
		FileDicomTagTable table = info.getTagTable();
		Hashtable<FileDicomKey, FileDicomTag> hash = table.getTagList();
		Set<FileDicomKey> keys = new LinkedHashSet<FileDicomKey>( hash.keySet());
		for(FileDicomSQItem s : seqTags){
			keys.addAll(s.getTagList().keySet());
		}
		
		keyList = new ArrayList<FileDicomKey>();
		tagList = new ArrayList<String>();
		
		Hashtable<String, ArrayList<FileDicomKey>> groups = new Hashtable<String, ArrayList<FileDicomKey>>();
		for(FileDicomKey k : keys){
			String group = k.getGroup();
			int groupNum = k.getGroupNumber();
			if(groupNum%2 == 1){
				if(groups.containsKey(group)){
					String element = k.getElement();
					if(element.equals("0010"))//Group name
						groups.get(group).add(0, k);
					else{
						ArrayList<FileDicomKey> keyArray = groups.get(group);
						if(!keyArray.contains(k))
							keyArray.add(k);
					}	
				}else{
					ArrayList<FileDicomKey> keyArray = new ArrayList<FileDicomKey>();
					keyArray.add(k);
					groups.put(group, keyArray);
				}
			}
		}
		
		Collections.sort(keyList);
		Set<String> groupKeys = groups.keySet();
		ArrayList<String> groupList = new ArrayList<String>(groupKeys);
		Collections.sort(groupList);
		//List<String> groupList = asSortedList(groupKeys);
	
		for(String s : groupList){
			ArrayList<FileDicomKey> key = groups.get(s);
			FileDicomKey groupNameKey = key.remove(0);
			keyList.add(groupNameKey);
			Collections.sort(key);
			FileDicomTag groupNameTag = hash.get(groupNameKey);
			tagList.add(groupNameTag.getValue(false).toString());
			//System.out.println(groupNameKey.toString());
			String nodeTitle = "(" + groupNameKey.getGroup() + ") " 
					+ groupNameTag.getValue(false);
			//String groupName = groupNameTag.getName();
			DefaultMutableTreeNode root = new DefaultMutableTreeNode(nodeTitle);
			top.add(root);
			for(FileDicomKey k: key){
				keyList.add(k);
				//System.err.println(k.toString());
				String subTitle = "(" + k.getElement() + ") ";
				FileDicomTag tag = hash.get(k);
				tagList.add(tag.getName());
				DefaultMutableTreeNode keyNode = new DefaultMutableTreeNode(subTitle + tag.getName());
				root.add(keyNode);
			}
			
		}
		
		//Have tree, need to display to test it out first. DO ON MONDAY
		
		return keyTree;
	}

	private void removeAllPaths(){
		TreePath[] paths = new TreePath[tree.getRowCount()];
		for(int i=0;i<tree.getRowCount();i++){
			paths[i] = tree.getPathForRow(i);
		}
		checkTree.getSelectionModel().removeSelectionPaths(paths);
	}
	
	

	@Override
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equals("privateAll"))
			checkAllPaths();
		else if(command.equals("privateClear"))
			removeAllPaths();
		
	}
	

}
