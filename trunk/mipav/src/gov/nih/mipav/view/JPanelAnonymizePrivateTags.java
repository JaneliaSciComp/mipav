package gov.nih.mipav.view;

import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQ;
import gov.nih.mipav.model.file.FileDicomSQItem;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.border.EmptyBorder;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;


public class JPanelAnonymizePrivateTags extends JPanel implements ActionListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = -410753580261870012L;
	
	private CheckTreeManager checkTree;
	
	private JTree tree;
	
	private ArrayList<FileDicomKey> keyList;
	
	public JPanelAnonymizePrivateTags(){
		super();
		add(new JLabel("No private tags"));
	}
	
	public JPanelAnonymizePrivateTags(ModelImage img){
		super();
		
		setLayout(new GridBagLayout());
		setBorder(JDialogBase.buildTitledBorder("Check the fields to anonymize:"));
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.fill = GridBagConstraints.BOTH;
		
		/*createPrivateKeyTree(img);
		
		JScrollPane scrollPane = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollPane.setPreferredSize(new Dimension(375, 200));
		add(scrollPane, BorderLayout.NORTH);
		*/
		tree = createPrivateKeyTree(img);
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
        
        JButton testButton = new JButton("Test");
        testButton.addActionListener(this);;
        buttonPanel.add(testButton, BorderLayout.SOUTH);
		
		add(buttonPanel, gbc);
		
		for (int i = 0; i < tree.getRowCount(); i++) {
	         tree.expandRow(i);
		}
		
		checkAllPaths();
		
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
	
	
	
	public JTree createPrivateKeyTree(ModelImage image){
		
		/*FileDicomKey seqKey = new FileDicomKey("0008,1032");
		FileDicomTag seqTag = ((FileInfoDicom)image.getFileInfo()[0]).getTagTable().get(seqKey);
		//System.out.println(seqTag.toString());
		Object obj = seqTag.getValue(false);
		if(obj instanceof FileDicomSQ){
			FileDicomSQ sq = (FileDicomSQ) obj;
			System.out.println("Length" + sq.getDataLength());
			Vector<FileDicomSQItem> vec = sq.getSequence();
			System.out.println(vec.size());
		}*/
		
		DefaultMutableTreeNode top = new DefaultMutableTreeNode("Private keys");
		JTree keyTree = new JTree(top);
		
		FileInfoDicom info = (FileInfoDicom) image.getFileInfo(0);
		FileDicomTagTable table = info.getTagTable();
		Hashtable<FileDicomKey, FileDicomTag> hash = table.getTagList();
		Set<FileDicomKey> keys = hash.keySet();
		//Arrays.sort(keys.toArray());
		keyList = new ArrayList<FileDicomKey>();
		//Collections.sort(keyList);
		
		Hashtable<String, ArrayList<FileDicomKey>> groups = new Hashtable<String, ArrayList<FileDicomKey>>();
		for(FileDicomKey k : keys){
			String group = k.getGroup();
			int groupNum = k.getGroupNumber();
			if(groupNum%2 == 1){
				if(groups.containsKey(group)){
					String element = k.getElement();
					if(element.equals("0010"))//Group name
						groups.get(group).add(0, k);
					else 
						groups.get(group).add(k);
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
				DefaultMutableTreeNode keyNode = new DefaultMutableTreeNode(subTitle + tag.getName());
				root.add(keyNode);
			}
			
		}
		
		//Have tree, need to display to test it out first. DO ON MONDAY
		
		return keyTree;
	}
	
	private <T extends Comparable<? super T>> List<T> asSortedList(Collection<T> c){
		List<T> list = new ArrayList<T>(c);
		Collections.sort(list);
		return list;
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
	
	private void populateSeqTags(){
		
	}
	
	// @author Santhosh Kumar T - santhosh@in.fiorano.com 
	// Source: http://www.jroller.com/santhosh/date/20050610
	// Includes all components of the CheckTree class
	public class CheckTreeSelectionModel extends DefaultTreeSelectionModel{ 
	    /**
		 * 
		 */
		private static final long serialVersionUID = 8918452189008290261L;
		private TreeModel model; 
	 
	    public CheckTreeSelectionModel(TreeModel model){ 
	        this.model = model; 
	        setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION); 
	    } 
	 
	    // tests whether there is any unselected node in the subtree of given path 
	    public boolean isPartiallySelected(TreePath path){ 
	        if(isPathSelected(path, true)) 
	            return false; 
	        TreePath[] selectionPaths = getSelectionPaths(); 
	        if(selectionPaths==null) 
	            return false; 
	        for(int j = 0; j<selectionPaths.length; j++){ 
	            if(isDescendant(selectionPaths[j], path)) 
	                return true; 
	        } 
	        return false; 
	    } 
	 
	    // tells whether given path is selected. 
	    // if dig is true, then a path is assumed to be selected, if 
	    // one of its ancestor is selected. 
	    public boolean isPathSelected(TreePath path, boolean dig){ 
	        if(!dig) 
	            return super.isPathSelected(path); 
	        while(path!=null && !super.isPathSelected(path)) 
	            path = path.getParentPath(); 
	        return path!=null; 
	    } 
	 
	    // is path1 descendant of path2 
	    private boolean isDescendant(TreePath path1, TreePath path2){ 
	        Object obj1[] = path1.getPath(); 
	        Object obj2[] = path2.getPath(); 
	        for(int i = 0; i<obj2.length; i++){ 
	            if(obj1[i]!=obj2[i]) 
	                return false; 
	        } 
	        return true; 
	    } 
	 
	    public void setSelectionPaths(TreePath[] pPaths){ 
	        throw new UnsupportedOperationException("not implemented yet!!!"); 
	    } 
	 
	    public void addSelectionPaths(TreePath[] paths){ 
	        // unselect all descendants of paths[] 
	        for(int i = 0; i<paths.length; i++){ 
	            TreePath path = paths[i]; 
	            TreePath[] selectionPaths = getSelectionPaths(); 
	            if(selectionPaths==null) 
	                break; 
	            ArrayList<TreePath> toBeRemoved = new ArrayList<TreePath>(); 
	            for(int j = 0; j<selectionPaths.length; j++){ 
	                if(isDescendant(selectionPaths[j], path)) 
	                    toBeRemoved.add(selectionPaths[j]); 
	            } 
	            super.removeSelectionPaths((TreePath[])toBeRemoved.toArray(new TreePath[0])); 
	        } 
	 
	        // if all siblings are selected then unselect them and select parent recursively 
	        // otherwize just select that path. 
	        for(int i = 0; i<paths.length; i++){ 
	            TreePath path = paths[i]; 
	            TreePath temp = null; 
	            while(areSiblingsSelected(path)){ 
	                temp = path; 
	                if(path.getParentPath()==null) 
	                    break; 
	                path = path.getParentPath(); 
	            } 
	            if(temp!=null){ 
	                if(temp.getParentPath()!=null) 
	                    addSelectionPath(temp.getParentPath()); 
	                else{ 
	                    if(!isSelectionEmpty()) 
	                        removeSelectionPaths(getSelectionPaths()); 
	                    super.addSelectionPaths(new TreePath[]{temp}); 
	                } 
	            }else 
	                super.addSelectionPaths(new TreePath[]{ path}); 
	        } 
	    } 
	 
	    // tells whether all siblings of given path are selected. 
	    private boolean areSiblingsSelected(TreePath path){ 
	        TreePath parent = path.getParentPath(); 
	        if(parent==null) 
	            return true; 
	        Object node = path.getLastPathComponent(); 
	        Object parentNode = parent.getLastPathComponent(); 
	 
	        int childCount = model.getChildCount(parentNode); 
	        for(int i = 0; i<childCount; i++){ 
	            Object childNode = model.getChild(parentNode, i); 
	            if(childNode==node) 
	                continue; 
	            if(!isPathSelected(parent.pathByAddingChild(childNode))) 
	                return false; 
	        } 
	        return true; 
	    } 
	 
	    public void removeSelectionPaths(TreePath[] paths){ 
	        for(int i = 0; i<paths.length; i++){ 
	            TreePath path = paths[i]; 
	            if(path.getPathCount()==1) 
	                super.removeSelectionPaths(new TreePath[]{ path}); 
	            else 
	                toggleRemoveSelection(path); 
	        } 
	    } 
	 
	    // if any ancestor node of given path is selected then unselect it 
	    //  and selection all its descendants except given path and descendants. 
	    // otherwise just unselect the given path 
	    private void toggleRemoveSelection(TreePath path){ 
	        Stack<TreePath> stack = new Stack<TreePath>(); 
	        TreePath parent = path.getParentPath(); 
	        while(parent!=null && !isPathSelected(parent)){ 
	            stack.push(parent); 
	            parent = parent.getParentPath(); 
	        } 
	        if(parent!=null) 
	            stack.push(parent); 
	        else{ 
	            super.removeSelectionPaths(new TreePath[]{path}); 
	            return; 
	        } 
	 
	        while(!stack.isEmpty()){ 
	            TreePath temp = (TreePath)stack.pop(); 
	            TreePath peekPath = stack.isEmpty() ? path : (TreePath)stack.peek(); 
	            Object node = temp.getLastPathComponent(); 
	            Object peekNode = peekPath.getLastPathComponent(); 
	            int childCount = model.getChildCount(node); 
	            for(int i = 0; i<childCount; i++){ 
	                Object childNode = model.getChild(node, i); 
	                if(childNode!=peekNode) 
	                    super.addSelectionPaths(new TreePath[]{temp.pathByAddingChild(childNode)}); 
	            } 
	        } 
	        super.removeSelectionPaths(new TreePath[]{parent}); 
	    }
	}

	public class CheckTreeCellRenderer extends JPanel implements TreeCellRenderer{ 
	    /**
		 * 
		 */
		private static final long serialVersionUID = -8230844402715861092L;
		private CheckTreeSelectionModel selectionModel; 
	    private TreeCellRenderer delegate; 
	    private JCheckBox checkBox = new JCheckBox(); 
	 
	    public CheckTreeCellRenderer(TreeCellRenderer delegate, CheckTreeSelectionModel selectionModel){ 
	        this.delegate = delegate; 
	        this.selectionModel = selectionModel; 
	        setLayout(new BorderLayout()); 
	        setOpaque(false); 
	        checkBox.setOpaque(false); 
	    } 
	 
	 
	    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus){ 
	        Component renderer = delegate.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus); 
	 
	        TreePath path = tree.getPathForRow(row); 
	        if(path!=null){ 
	            if(selectionModel.isPathSelected(path, true)) {
	            	/*boolean checked = checkBox.isSelected();
	                checkBox.setSelected(!checked); */
	            	checkBox.setSelected(true);
	            }
	            else {
	                //if(selectionModel.isPartiallySelected(path))
	                	checkBox.setSelected(false);
	            	
	            	//checkBox.setSelected(selectionModel.isPartiallySelected(path) ? null : Boolean.FALSE); 
	        
	            }
	        }
	        removeAll(); 
	        add(checkBox, BorderLayout.WEST); 
	        add(renderer, BorderLayout.CENTER); 
	        return this; 
	    } 
	} 
	
	public class CheckTreeManager extends MouseAdapter implements TreeSelectionListener{ 
	    private CheckTreeSelectionModel selectionModel; 
	    private JTree tree = new JTree(); 
	    int hotspot = new JCheckBox().getPreferredSize().width; 
	 
	    public CheckTreeManager(JTree tree){ 
	        this.tree = tree; 
	        selectionModel = new CheckTreeSelectionModel(tree.getModel()); 
	        tree.setCellRenderer(new CheckTreeCellRenderer(tree.getCellRenderer(), selectionModel)); 
	        tree.addMouseListener(this); 
	        selectionModel.addTreeSelectionListener(this); 
	    } 
	 
	    public void mouseClicked(MouseEvent me){ 
	        TreePath path = tree.getPathForLocation(me.getX(), me.getY()); 
	        if(path==null) 
	            return; 
	        if(me.getX()>tree.getPathBounds(path).x+hotspot) 
	            return; 
	 
	        boolean selected = selectionModel.isPathSelected(path, true); 
	        selectionModel.removeTreeSelectionListener(this); 
	 
	        try{ 
	            if(selected) 
	                selectionModel.removeSelectionPath(path); 
	            else 
	                selectionModel.addSelectionPath(path); 
	        } finally{ 
	            selectionModel.addTreeSelectionListener(this); 
	            tree.treeDidChange(); 
	        } 
	    } 
	 
	    public CheckTreeSelectionModel getSelectionModel(){ 
	        return selectionModel; 
	    } 
	 
	    public void valueChanged(TreeSelectionEvent e){ 
	        tree.treeDidChange(); 
	    } 
	}
	
	

	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub
		String command = e.getActionCommand();
		if(command.equals("privateAll"))
			checkAllPaths();
		else if(command.equals("privateClear"))
			removeAllPaths();
		else if(command.equals("Test"))
			test();
		
	}
	
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
			keys[i] = keyList.get(i);
		}
		
		return keys;
	}
	
	private void test(){
		
		
		/*ArrayList<Integer> paths = new ArrayList<Integer>();
		CheckTreeSelectionModel model = checkTree.getSelectionModel();
		for(int i=1;i<tree.getRowCount();i++){
			TreePath path = tree.getPathForRow(i);
			if(model.isPathSelected(path, true)){
				paths.add(i-1);
				System.out.println(i-1);
			}
			
		}*/
	}
}
