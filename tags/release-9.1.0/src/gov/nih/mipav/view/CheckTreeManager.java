package gov.nih.mipav.view;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Stack;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;


//@author Santhosh Kumar T - santhosh@in.fiorano.com 
// Source: http://www.jroller.com/santhosh/date/20050610
// Includes all components of the CheckTree class

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
}
