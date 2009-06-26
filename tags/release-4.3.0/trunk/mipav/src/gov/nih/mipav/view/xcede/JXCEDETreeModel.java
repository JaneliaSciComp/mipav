package gov.nih.mipav.view.xcede;

import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeModelEvent;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.event.EventListenerList;
import javax.swing.Icon;

import java.util.EventListener;

import gov.nih.mipav.model.file.xcede.*;

public class JXCEDETreeModel implements TreeModel {

    public static Icon ICON_PROJECT = null;
    public static Icon ICON_SUBJECT = null;
    public static Icon ICON_VISIT   = null;
    public static Icon ICON_STUDY   = null;
    public static Icon ICON_SERIES  = null;
    public static Icon ICON_DATAREC_IMAGE = null;
    public static Icon ICON_DATAREC_TEXT = null;
    
    private Object root;
    
    protected EventListenerList listenerList = new EventListenerList();
  
    /***********************
     ***** Constructor *****
     ***********************
     */
    public JXCEDETreeModel(){
        this(null);
    }
    
    public JXCEDETreeModel(XCEDEElement root){
        this.root = root;
    }
    
    protected void finalize() throws Throwable{
        System.out.println("Entering JXCEDETreeModel's finalize() ... ");
        this.root = null;
        
        listenerList = null;
        super.finalize();
    }
    /**
     * Returns the root of the tree.
     */
    public Object getRoot() {
        return root;
    }

    /**
     * Sets the new root of the tree.
     * @param newRoot the new root of the tree.
     * @throws ClassCastException if new root is not a XCEDEElement.
     * @throws NullPonterException if newRoot is null.
     */
    public void setRoot(Object newRoot){
        Object oldRoot = root;
        if(newRoot != null){
            if(newRoot instanceof XCEDEElement){
                root = newRoot;
                fireTreeStructureChanged(oldRoot);
            }else{
                throw new ClassCastException("Root must be an instance of XCEDEElement.");
            }
        }else{
            throw new NullPointerException("Root can't be null!");
        }
    }
    
    /**
     * Returns the child of parent at index in the parent's child array.
     */
    public Object getChild(Object parent, int index) {
        if(parent instanceof XCEDEElement){
            return ((XCEDEElement)parent).getChildAt(index);
        }
        return null;
    }

    /**
     * Returns the number of children of the parent. Returns 0 if the
     * node is leaf.
     */
    public int getChildCount(Object parent) {
        if(parent instanceof XCEDEElement){
            return ((XCEDEElement)parent).getChildCount();
        }
        return 0;
    }

    /**
     * Returns true if the node is a leaf.
     */
    public boolean isLeaf(Object node) {
        if(node instanceof XCEDEElement){
            return ((XCEDEElement)node).isLeaf();
        }
        return true;
    }

    public void valueForPathChanged(TreePath path, Object newValue) {

    }

    /**
     * Returns the index of the child in parent.
     */
    public int getIndexOfChild(Object parent, Object child) {
        if(parent instanceof XCEDEElement && child instanceof XCEDEElement){
            int nchild = ((XCEDEElement)parent).getChildCount();
            for(int i = 0; i < nchild; i++){
                XCEDEElement xcedeElement = (XCEDEElement)((XCEDEElement)parent).getChildAt(i);
                if(xcedeElement.equals(child)){
                    return i;
                }
            }
        }
        return -1;
    }

    /**
     * Adds a listener for the TreeModelEvent posed after the tree changed.
     */
    public void addTreeModelListener(TreeModelListener l) {
        listenerList.add(TreeModelListener.class, l);
    }

    /**
     * Removes a listener previously added with addTreeModelListener().
     */
    public void removeTreeModelListener(TreeModelListener l) {
        listenerList.remove(TreeModelListener.class, l);
    }

    /**
     * Notifies all the listeners the tree structure was changed.
     * @param oldRoot the root node of the tree.
     */
    public void fireTreeStructureChanged(Object oldRoot){
        TreeModelEvent event = new TreeModelEvent(this, new Object[]{oldRoot});
        EventListener[] listeners = listenerList.getListeners(TreeModelListener.class);
        for(int i = 0; i < listeners.length; i++){
            ((TreeModelListener)listeners[i]).treeStructureChanged(event);
        }
    }
}
