package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.util.*;

import javax.swing.tree.*;


/**
 * This class is used to represent the orientation of a set of VOI curves. 
 * This is a node of the overall VOI. It has a gov.nih.mipav.structure.VOIBase set as its object.
 *
 * <p>Nodes in the tree are expanded by calling this class's explore method.</p>
 *
 * @author  Justin Senseney
 */
public class VOIOrientationNode extends DefaultMutableTreeNode {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6795049606234900885L;
    
    
    private String orientation;

    /** Name of parent voi */
    private String voiName;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new VOIFrameNode object.
     *
     * @param  node     File for tree leaf.
     * @param  fNumber  The slice number of this VOI component
     */
    public VOIOrientationNode(String voiName, String orientation,Vector<VOIBase>[] sortedCurves) {
    	this.voiName = voiName;
        this.orientation = orientation;
        setUserObject(sortedCurves);

        // unless we drop in information about the VOIbase here...
        setAllowsChildren(true);
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
    	Vector<VOIBase>[] sortedCurves = (Vector<VOIBase>[])getUserObject();
    	
    	
    	for(int i=0;i<sortedCurves.length;i++) {
    		Vector<VOIBase> contours = sortedCurves[i];
    		if(contours != null && contours.size() > 0) {
    			add(new VOIFrameNode(contours,i));
    			
    			
    		}
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
    @SuppressWarnings("unchecked")
    public Vector<VOIBase>[] getVOI() {
        return (Vector<VOIBase>[])super.getUserObject();
    }

    /**
     * Returns the name of the parent voi
     * 
     * @return
     */
    public String getVOIname() {
        return voiName;
    }

    /**
     * the string returned is the name of the VOI returns the name of the VOI if the VOI exists, or <code>null</code> if
     * it doesn't.
     *
     * <p>Explicitly calls DefaultMutableTreeNode.toString()</p>
     *
     * @return  Name of the file.
     *
     * @see     javax.swing.tree.DefaultMutableTreeNode#toString()
     */
    public String getName() {
        return orientation;
    }
    
    /**
     * the string returned is the name of the VOI returns the name of the VOI if it exists, or <code>null</code> if it
     * doesn't.
     *
     * @return  Name of the file.
     *
     * @see     javax.swing.tree.DefaultMutableTreeNode#toString()
     */
    public String toString() {
        return getName();
    }
    


}
