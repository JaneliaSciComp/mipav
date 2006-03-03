package gov.nih.mipav.view;

import javax.swing.*;

/** wrapper class for JTree
*/
public class VOIGroupTree   extends JTree {
    
    /** creates a new JTree as a VOIGroupTree
    */
    public VOIGroupTree() {
        super();
    }
    
    /** creates a new JTree as a VOIGroupTree
    *   with a DefaultMutableTreeNode.
    */
    public VOIGroupTree(VOIGroupNode VOIgn) {
        super(VOIgn);
    }
    
    // nothing really important to say here....
    public String toString() {return ("(VOI group tree) "+super.toString());}
}