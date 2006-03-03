package gov.nih.mipav.view;

import javax.swing.tree.*;
import java.util.*;
import gov.nih.mipav.model.structures.*;


/**
 *   This class is used to represent a ... in
 *   a tree.  This is a node of the .... .  It
 *   has a gov.nih.mipav.structure.VOI as its object.
 *   <p>
 *   Nodes in the tree are expanded by calling this
 *   class's explore method.
 *   <p>
 *
 *   @author David Parsons
 */
public class VOIGroupNode extends DefaultMutableTreeNode {

    /**
    *   Loads a File into the tree-leaf.
    *   @param node File for tree leaf.
    */
    public VOIGroupNode (VOI node) {
        setUserObject(node);
        setAllowsChildren(true);
        explore();
    }

    public VOIGroupNode(Object node) {
        super(node);
    }


    /**
    *   the string returned is the name of the VOI
    *   returns     the name of the VOI if it exists, or
    *       <code>null</code> if it doesn't.
    *   @return     Name of the file.
    *   @see javax.swing.tree.DefaultMutableTreeNode#toString()
    */
    public String toString() {
        return getName();
    }

    /** the string returned is the name of the VOI
    *   returns     the name of the VOI if the VOI exists, or
    *       <code>null</code> if it doesn't.
    *   <p>
    *   Explicitly calls DefaultMutableTreeNode.toString()
    *   @return     Name of the file.
    *   @see javax.swing.tree.DefaultMutableTreeNode#toString()
    */
    public String getName() {
        return super.toString();
    }

    /**
    *   Adds the children of this file to the this FileNode
    *   for display in a JTree. adjusts the <code>explored</code>
    *   variable.  Does nothing if the FileNode is not a
    *   directory or if the node has already been explored.
    *   If directories only are to be explored, then the
    *   only children to be added will be directories.  Otherwise,
    *   if the file filter is not null, only the files that satisfy the
    *   filter will be added.  If the filter is null, all files will be added.
    *   <p>
    *   Implementation of this is different from the Sun Books' code.
    */
    public void explore() {
        // load all contours into the Tree as children of the VOI node.
        Vector curveList[] = ((VOI)getUserObject()).getCurves();
        for (int i=0; i < curveList.length; i++) {
            int j = 0;
            while (j < curveList[i].size()) {
                add(new VOINode((VOIBase)curveList[i].get(j)));
                j++;
            }
        }

    }


    /** Returns this node's user object.
    *   <p>
    *   <b>Overrides:</b>
    *   <br>
    *    <code>getUserObject</code> in javax.swing.tree.DefaultMutableTree#getUserObject()
    *   @see gov.nih.mipav.structures.VOI
    */
    public VOI getVOIgroup() {return (VOI)super.getUserObject();}


}
