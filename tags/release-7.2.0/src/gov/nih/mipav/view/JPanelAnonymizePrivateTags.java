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
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * The panel class used in anonymize image/directory that displays the private
 * tags in a tree format along with check boxes so that the organization between
 * groups is apparent. 
 * 
 * For information on the tree classes used, see CheckTreeManager (and the URL 
 * in that file)
 * @see CheckTreeManager
 * @author wangvg
 *
 */

public class JPanelAnonymizePrivateTags extends JPanel implements ActionListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = -410753580261870012L;
	
	/**
	 * The checkbox tree version of the tree in the panel. Used
	 * to actually display the information.
	 */
	private CheckTreeManager checkTree;
	
	/**
	 * The tree used to keep track of keys. Used to manipulate 
	 * the selections. 
	 */
	private JTree tree;
	
	/**
	 * The list of private keys in the image
	 */
	private ArrayList<FileDicomKey> keyList;
	
	/**
	 * The list of private tags that map to the
	 * keys in the previous list
	 */
	private ArrayList<String> tagList;
	
	/**
	 * The default constructor that occurs in the anonymize directory
	 * dialog before a profile has been loaded to populate the tree
	 */
	public JPanelAnonymizePrivateTags(){
		super();
		add(new JLabel("Load profile for private tags"));
	}
	
	/**
	 * The constructor used in the anonymize image dialog that will 
	 * populate the tree and assign the layout based on the file info.
	 * @param img
	 * @param seqTags
	 */
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
		add(treeView, gbc);

		gbc.gridy = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;


		JPanel buttonPanel = new JPanel();
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

		//Make sure the tree is completely expanded and
		//all selections are checked for private tags
		for (int i = 0; i < tree.getRowCount(); i++) {
			tree.expandRow(i);
		}

		checkAllPaths();
		
		
	}
	
	/**
	 * Method used in the anonymize dialog profile that generates the tree
	 * structure from key information passed into the class. Required to
	 * pass in the key string, key name, and whether it was not selected.
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
		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Private keys");
		DefaultMutableTreeNode next = null;
		ArrayList<Integer> selectedRows = new ArrayList<Integer>();
		tree = new JTree(top);
		for(int i=0;i<keys.size();i++){
			FileDicomKey k = keys.get(i);
			String t = tags.get(i);
			if(k.getGroup().equals(prevGroup)){
				String title = "(" + k.getElement() + ") " + t;
				DefaultMutableTreeNode child = new DefaultMutableTreeNode(title);
				next.add(child);
				if(selected[i])
				selectedRows.add(i+1);
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
	
	public ArrayList<FileDicomKey> getKeyList(){
		return keyList;
	}
	
	public ArrayList<String> getTagList(){
		return tagList;
	}
	
	/**
	 * Method to retrieve the private keys selected in this
	 * panel. In none are selected, the method returns null.
	 * @return
	 */
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

	/**
	 * Alternate method for retrieving private keys that were
	 * selected. When you already know which private keys are
	 * in the tree, you only need the booleans to work with.
	 * @return
	 */
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

	/**
	 * Method used for loading profiles into the panel. Keys
	 * passed in are compared to the current tree in order
	 * to select only the ones that are present.
	 * @param keys
	 */
	public void setSelectedKeys(ArrayList<FileDicomKey> keys){
		ArrayDeque<FileDicomKey> keyStack = new ArrayDeque<FileDicomKey>(keys);
		ArrayList<Integer> selected = new ArrayList<Integer>();

		//Both lists should be sorted already
		//Run a sorted comparison to determine which 
		//keys in the tree need to be selected
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

		}
		
		//Actually select the tree elements
		TreePath[] selectedPaths = new TreePath[selected.size()];
		for(int i=0;i<selected.size();i++){
			selectedPaths[i] = tree.getPathForRow(selected.get(i));
		}
		
		removeAllPaths();
		checkTree.getSelectionModel().addSelectionPaths(selectedPaths);
		
	}
	
	/**
	 * Selects all the paths in the tree. Since the tree
	 * automatically selects all children if the root is
	 * selected, this just requires you to select the
	 * root node.
	 */
	private void checkAllPaths(){

		TreePath[] root = new TreePath[1];
		root[0] = tree.getPathForRow(0);
		checkTree.getSelectionModel().addSelectionPaths(root);
		
	}
		
	/**
	 * Method to populate the tree structure from the input image 
	 * and provided sequence tags. 
	 * @param image
	 * @param seqTags
	 * @return
	 */
	private JTree createPrivateKeyTree(ModelImage image, Vector<FileDicomSQItem> seqTags){
		
		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Private keys");
		JTree keyTree = new JTree(top);
		
		FileInfoDicom info = (FileInfoDicom) image.getFileInfo(0);
		FileDicomTagTable table = info.getTagTable();
		Hashtable<FileDicomKey, FileDicomTag> hash = table.getTagList();
		//Place all the keys into a hashset
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
		for(FileDicomKey k : keys){
			String group = k.getGroup();
			int groupNum = k.getGroupNumber();
			if(groupNum%2 == 1){//Private tags end in an odd number
				if(groups.containsKey(group)){
					String element = k.getElement();
					if(element.equals("0010"))//Group name, add to the top of the list
						groups.get(group).add(0, k);
					else{//Not group name, add to the end of the list
						ArrayList<FileDicomKey> keyArray = groups.get(group);
						if(!keyArray.contains(k))
							keyArray.add(k);
					}	
				}else{//This group is not in the hash, make a new list
					ArrayList<FileDicomKey> keyArray = new ArrayList<FileDicomKey>();
					keyArray.add(k);
					groups.put(group, keyArray);
				}
			}
		}
		
		//Sort the lists so that they are displayed properly
		//Collections.sort(keyList); //why is this here? sorting an empty list.
		Set<String> groupKeys = groups.keySet();
		ArrayList<String> groupList = new ArrayList<String>(groupKeys);
		Collections.sort(groupList);
	
		for(String s : groupList){
			ArrayList<FileDicomKey> key = groups.get(s);
			
			//First element is group name, display at the root
			//of the group
			FileDicomKey groupNameKey = key.remove(0); 
			keyList.add(groupNameKey);
			Collections.sort(key);
			FileDicomTag groupNameTag = hash.get(groupNameKey);
			tagList.add(groupNameTag.getValue(false).toString());
			String nodeTitle = "(" + groupNameKey.getGroup() + ") " 
					+ groupNameTag.getValue(false);
			DefaultMutableTreeNode root = new DefaultMutableTreeNode(nodeTitle);
			top.add(root);
			for(FileDicomKey k: key){ //Add rest of the elements to the tree
				keyList.add(k);
				String subTitle = "(" + k.getElement() + ") ";
				FileDicomTag tag = hash.get(k);
				tagList.add(tag.getName());
				DefaultMutableTreeNode keyNode = new DefaultMutableTreeNode(subTitle + tag.getName());
				root.add(keyNode);
			}
			
		}
		
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
