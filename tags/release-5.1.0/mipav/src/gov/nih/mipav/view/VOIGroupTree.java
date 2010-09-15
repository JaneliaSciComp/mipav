package gov.nih.mipav.view;


import javax.swing.*;


/**
 * wrapper class for JTree.
 */
public class VOIGroupTree extends JTree {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6853661393602487962L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * creates a new JTree as a VOIGroupTree.
     */
    public VOIGroupTree() {
        super();
    }

    /**
     * creates a new JTree as a VOIGroupTree with a DefaultMutableTreeNode.
     *
     * @param  VOIgn  DOCUMENT ME!
     */
    public VOIGroupTree(VOIGroupNode VOIgn) {
        super(VOIgn);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * nothing really important to say here....
     *
     * @return  DOCUMENT ME!
     */
    public String toString() {
        return ("(VOI group tree) " + super.toString());
    }
}
