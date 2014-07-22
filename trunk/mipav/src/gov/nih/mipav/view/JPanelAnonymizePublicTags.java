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
import javax.swing.border.EmptyBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQItem;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.CheckTreeManager.CheckTreeSelectionModel;
import gov.nih.mipav.view.dialogs.JDialogBase;

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
	
	//Should make some sort of hash/list that holds the Supp. 55 tags so that you don't duplicate them
	
	public JPanelAnonymizePublicTags(){
		super();
		add(new JLabel("Load profile for private tags"));
		
		suppTags = new HashSet<String>();
		for(int i=0;i<FileInfoDicom.anonymizeTagIDs.length;i++){
			suppTags.add(FileInfoDicom.anonymizeTagIDs[i]);
		}
	}
	
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
		//treeView.setPreferredSize(new Dimension(300,200));
		add(treeView, gbc);

		gbc.gridy = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;


		JPanel buttonPanel = new JPanel();
		buttonPanel.setBorder(new EmptyBorder(0,0,3,0));
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

		//checkAllPaths();
	}
	
	private JTree createPublicKeyTree(ModelImage image, Vector<FileDicomSQItem> seqTags){
	
		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Public keys");
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
		
		//No equivalent element number for group name in public keys, need to do something else for
		//group titles
		for(FileDicomKey k : keys){
			String group = k.getGroup();
			int groupNum = k.getGroupNumber();
			if(groupNum%2 == 0){
				if(groups.containsKey(group)){
					ArrayList<FileDicomKey> keyArray = groups.get(group);
					if(!keyArray.contains(k) && !suppTags.contains(k.getKey()))
						keyArray.add(k);	
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
		
		for(String s : groupList){
			ArrayList<FileDicomKey> key = groups.get(s);			
			Collections.sort(key);
			
			/*FileDicomKey groupNameKey = key.remove(0);
			keyList.add(groupNameKey);
			
			FileDicomTag groupNameTag = hash.get(groupNameKey);
			tagList.add((String) groupNameTag.getValue(false));
			String nodeTitle = "(" + groupNameKey.getGroup() + ") " 
					+ groupNameTag.getValue(false);*/
			
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
		/*TreePath[] paths = new TreePath[tree.getRowCount()];
		for(int i=0;i<tree.getRowCount();i++){
			paths[i] = tree.getPathForRow(i);
		}*/
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
		ArrayList<Integer> paths = new ArrayList<Integer>();
		CheckTreeSelectionModel model = checkTree.getSelectionModel();
		int offset = 1;
		for(int i=1;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			if(path.getPathCount() == 2)
				offset++;
			if(model.isPathSelected(path, true)){
				paths.add(i-offset);
			}
		}
		int length = paths.size();
		if(length == 0)
			return null;
		FileDicomKey[] keys = new FileDicomKey[length];
		for(int i=0;i<length;i++){
			keys[i] = keyList.get(i);
		}
		
		return keys;
	}

	public boolean[] getSelectedKeysBool(){
		boolean[] selected = new boolean[keyList.size()];
		int offset = 1;
		CheckTreeSelectionModel model = checkTree.getSelectionModel();
		for(int i=1;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			if(path.getPathCount() < 3){
				offset++;
				continue;
			}
			selected[i-offset] = model.isPathSelected(path, true);
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
			while(k.compareTo(q) > 0){
				keyStack.poll();
				q = keyStack.peek();
			}
			if(k.equals(q)){
				keyStack.poll();
				selected.add(i+offset);
			}
		}
		TreePath[] selectedPaths = new TreePath[selected.size()];
		for(int i=0;i<selected.size();i++){
			selectedPaths[i] = tree.getPathForRow(selected.get(i));
		}
		
		removeAllPaths();
		checkTree.getSelectionModel().addSelectionPaths(selectedPaths);
		
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
