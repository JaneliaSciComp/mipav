package gov.nih.mipav.view.dialogs;


import javax.swing.tree.DefaultMutableTreeNode;


/**
 * Class used as a DefaultMutableTreeNode, except it stores a default name that can be recalled at any time if the
 * users deletes the image/voi.
 */
public class ScriptTreeNode extends DefaultMutableTreeNode {
    private String defaultName;

    private String nodeType;

    private String filePath;

    public ScriptTreeNode(Object userObject, String nodeType) {
        super();
        super.setUserObject(userObject);
        this.nodeType = nodeType;
        setDefaultName((String) userObject);
    }

    public String getDefaultName() {
        return defaultName;
    }

    public void setDefaultName(String defaultName) {
        this.defaultName = defaultName;
    }

    public String getNodeType() {
        return nodeType;
    }

    public void setNodeType(String nodeType) {
        this.nodeType = nodeType;
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }
}
