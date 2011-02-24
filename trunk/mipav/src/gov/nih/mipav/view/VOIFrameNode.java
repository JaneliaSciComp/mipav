package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.util.*;

import javax.swing.tree.*;


/**
 * This class is used to represent a VOIContour, VOIPoint, or other two-dimensional VOI object in a tree. 
 * This is a node of the overall VOI. It has a gov.nih.mipav.structure.VOI as its object.
 *
 * <p>Nodes in the tree are expanded by calling this class's explore method.</p>
 *
 * @author  David Parsons
 */
public class VOIFrameNode extends DefaultMutableTreeNode {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6795049606234900885L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The slice number of the VOI component */
    private int frameNumber;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new VOIFrameNode object.
     *
     * @param  node     File for tree leaf.
     * @param  fNumber  The slice number of this VOI component
     */
    @SuppressWarnings("unchecked")
    public VOIFrameNode(Vector node, int fNumber) {
        setUserObject(node);

        // unless we drop in information about the VOIbase here...
        setAllowsChildren(true);
        frameNumber = fNumber;
        explore();
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
    @SuppressWarnings("unchecked")
    public void explore() {
        Vector<VOIBase> curves = (Vector<VOIBase>) getUserObject();

        for (int i = 0; i < curves.size(); i++) {
            add(new VOINode(curves.elementAt(i)));
        }
    }

    /**
     * Gets the slice number of this VOI.  Note that slice number is zero based
     *
     * @return  DOCUMENT ME!
     */
    public int getFrameNumber() {
        return this.frameNumber;
    }

    /**
     * the string returned is the name of the VOI returns the name of the VOI if the VOI exists, or <code>null</code> if
     * it doesn't.
     *
     * @return  The slice number of this VOI.
     */
    public String getName() {

        return "Slice " + Integer.toString(frameNumber);
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
