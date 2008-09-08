package gov.nih.mipav.view;


import java.io.*;

import javax.swing.tree.*;


/**
 * This class is used to represent a file system in a tree. This is a node of the file system. It has a java.io.File as
 * its user object.
 *
 * <p>Nodes in the tree are expanded by calling this class's explore method. You can set the explore method to return
 * only directories by setting the directoriesOnly flag. Additionally you can call explore with a file filter, and only
 * nodes that satisfy that file filter will be returned. Directories are verified to contain valid files before they are
 * added to the tree.</p>
 *
 * <p>Basic structure of class taken from Sun's Graphic Java Swing "Trees" section.</p>
 *
 * @author  David Parsons
 * @author  Neva Cherniavsky
 * @see     ViewImageDirectory
 * @see     JDialogAnonymizeDirectory
 */
public class ViewFileTreeNode extends DefaultMutableTreeNode {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2761808148905905365L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean directoriesOnly = false;

    /** DOCUMENT ME! */
    private boolean explored = false;

    /** DOCUMENT ME! */
    private boolean rootfile = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Loads a File into the tree-leaf.
     *
     * @param  node  File for tree leaf.
     */
    public ViewFileTreeNode(File node) {
        setUserObject(node);
    }

    /**
     * Loads a File into a tree-leaf. This method permits telling the node that the file is a root for the filesystem.
     *
     * <p>It can be useful to remember whether or not the given file was a root. (which is not information normally
     * stored by the File.)</p>
     *
     * @param  node    File for tree leaf.
     * @param  fsRoot  Flag indicating if this is a root.
     */
    public ViewFileTreeNode(File node, boolean fsRoot) {
        this(node);
        rootfile = fsRoot;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds the children of this file to the this FileNode for display in a JTree. adjusts the <code>explored</code>
     * variable. Does nothing if the FileNode is not a directory or if the node has already been explored. If
     * directories only are to be explored, then the only children to be added will be directories; otherwise, all files
     * in the directory will be added to the node.
     *
     * <p>Implementation of this is different from the Sun Books' code.</p>
     */
    public void explore() {
        explore(null);
    }

    /**
     * Adds the children of this file to the this FileNode for display in a JTree. adjusts the <code>explored</code>
     * variable. Does nothing if the FileNode is not a directory or if the node has already been explored. If
     * directories only are to be explored, then the only children to be added will be directories. Otherwise, if the
     * file filter is not null, only the files that satisfy the filter will be added. If the filter is null, all files
     * will be added.
     *
     * <p>Implementation of this is different from the Sun Books' code.</p>
     *
     * @param  filter  File filter; can be null.
     */
    public void explore(ViewImageFileFilter filter) {

        if (!isDirectory()) {
            return;
        }

        if (!isExplored()) {
            File file = getFile();
            File[] children = file.listFiles();

            if (children == null) {
                explored = true;

                return;
            }

            // else
            for (int i = 0; i < children.length; i++) {

                if (directoriesOnly) {

                    if (children[i].isDirectory()) {
                        ViewFileTreeNode node = new ViewFileTreeNode(children[i]);

                        if (node.hasValidChildren(filter)) {
                            node.exploreDirectoriesOnly(true);
                            add(node);
                        }
                    }
                } else {

                    if (filter == null) {
                        add(new ViewFileTreeNode(children[i]));
                    } else {

                        if (children[i].isDirectory()) {
                            ViewFileTreeNode node = new ViewFileTreeNode(children[i]);

                            if (node.hasValidChildren(filter)) {
                                add(node);
                            }
                        } else {

                            if (filter.accept(children[i])) {
                                add(new ViewFileTreeNode(children[i]));
                            }
                        }
                    }
                }
            }

            explored = true;
        }
    }

    /**
     * Accessor to whether or not the FileNode is to only explore Directories.
     *
     * @return  <code>true</code> if FileNode only explores directories.
     */
    public boolean exploreDirectoriesOnly() {
        return directoriesOnly;
    }

    /**
     * Sets the FileNode to view only directories when it goes exploring.
     *
     * @param  directoryChildren  The permission to view only directories.
     */
    public void exploreDirectoriesOnly(boolean directoryChildren) {
        directoriesOnly = directoryChildren;
    }

    /**
     * The absolute path of the FileNode, as taken from its File.
     *
     * @return  The File's name.
     */
    public String getAbsolutePath() {
        return getFile().getAbsolutePath();
    }

    /**
     * Only directoies can allow children.
     *
     * @return  <code>true</code> if this is a directory.
     */
    public boolean getAllowsChildren() {
        return isDirectory();
    }


    /**
     * The directory of the FileNode, as taken from its File.
     *
     * @return  The directory's name.
     */
    public String getDirectory() {
        return getFile().getParent();
    }

    /**
     * Returns the File that is this node on the tree.
     *
     * @return  the File that is this node on the tree.
     */
    public File getFile() {
        return (File) getUserObject();
    }

    /**
     * The name of the FileNode, as taken from its File.
     *
     * @return  The File's name.
     */
    public String getName() {
        return getFile().getName();
    }

    /**
     * Returns <code>true</code> if this fileNode is a directory and not a normal file.
     *
     * @return  <code>true</code> if this fileNode is a directory.
     */
    public boolean isDirectory() {
        return getFile().isDirectory();
    }

    /**
     * Whether or not this node has been 'explored', and its subdirectories and file-children have been found.
     *
     * @return  whether or not this FileNode knows that is known.
     */
    public boolean isExplored() {
        return explored;
    }

    /**
     * Leaves are not directories. Directories can extend.
     *
     * @return  <code>true</code> if not directory.
     */
    public boolean isLeaf() {
        return !isDirectory();
    }

    /**
     * Gets the name of the File. When this FileNode is a FileNode for a root of the filesystem, the String returned is
     * the absolute path (that is so on Windows systems,the user sees "C:\"); otherwise, the string returned is the
     * filename.
     *
     * @return  Name of the file.
     */
    public String toString() {
        return (rootfile) ? getAbsolutePath() : getName();
    }

    /**
     * Checks if this directory ought to be added to the list based on whether it has any children that satisfy the
     * filter.
     *
     * @param   filter  Filter to check files against; if null returns true.
     *
     * @return  Flag indicating if this directory has valid children.
     */
    private boolean hasValidChildren(ViewImageFileFilter filter) {

        if (filter == null) {
            return true;
        }

        if (!isDirectory()) {

            if (filter.accept(getFile())) {
                return true;
            } else {
                return false;
            }
        }

        File file = getFile();
        File[] children = file.listFiles();

        if (children == null) {
            return false;
        }

        ViewFileTreeNode node;

        // else
        for (int i = 0; i < children.length; i++) {
            node = new ViewFileTreeNode(children[i]);

            if (node.hasValidChildren(filter)) {
                return true;
            }
        }

        return false;
    }
}
