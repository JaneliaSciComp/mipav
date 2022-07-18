package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import javax.swing.tree.*;


/**
 * This class is used to represent a contour node in a tree. This is a node of the VOITreeNode . It has a gov.nih.mipav.structure.VOIBase as
 * its object.
 *
 * <p>Nodes in the tree are expanded by calling this class's explore method.</p>
 *
 * @author  Justin Senseney
 */
public class VOIContourNode extends DefaultMutableTreeNode {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1101994985217434811L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new VOINode object.
     *
     * @param  node  File for tree leaf.
     */
    public VOIContourNode(VOIBase node) {
        setUserObject(node);

        // unless we drop in information about the VOIbase here...
        setAllowsChildren(false);

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds the children of this file to the this FileNode for display in a JTree. adjusts the <code>explored</code>
     * variable. Does nothing if the FileNode is not a directory or if the node has already been explored. If
     * directories only are to be explored, then the only children to be added will be directories. Otherwise, if the
     * file filter is not null, only the files that satisfy the filter will be added. If the filter is null, all files
     * will be added.
     *
     * <p>Implementation of this is different from the Sun Books' code.</p>
     */
    public void explore() { }

    /**
     * the string returned is the name of the VOI returns the name of the VOI if the VOI exists, or <code>null</code> if
     * it doesn't.
     *
     * @return  The File's name.
     */
    public String getName() {

        try {
            return " " + ((VOIBase) getUserObject()).getLabel();
        } catch (NullPointerException npe) {
            return null;
        }
    }


    /**
     * Returns this node's user object.
     *
     * <p><b>Overrides:</b><br>
     * <code>getUserObject</code> in javax.swing.tree.DefaultMutableTree#getUserObject()</p>
     *
     * @see  gov.nih.mipav.structures.VOI
     */
    public VOIBase getVOI() {
        return (VOIBase) super.getUserObject();
    }


    /**
     * the string returned is the name of the VOI returns the name of the VOI if it exists, or <code>null</code> if it
     * doesn't.
     *
     * @return  The File's name.
     */
    public String toString() {
        return getName();
    }


}
