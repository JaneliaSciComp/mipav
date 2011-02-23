package gov.nih.mipav.view.dialogs;


import javax.swing.tree.DefaultMutableTreeNode;


/**
 * Class used as a DefaultMutableTreeNode, except it stores a default name that can be recalled at any time if the
 * users deletes the image/voi.
 */
public class ScriptTreeNode extends DefaultMutableTreeNode {

    private int nodeType;

    private String filePath;

    public ScriptTreeNode(Object userObject, int nodeType) {
        super();
        super.setUserObject(userObject);
        this.nodeType = nodeType;
    }

    public int getNodeType() {
        return nodeType;
    }

    public void setNodeType(int nodeType) {
        this.nodeType = nodeType;
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }
}
