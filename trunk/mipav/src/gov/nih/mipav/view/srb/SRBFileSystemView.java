package gov.nih.mipav.view.srb;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Vector;

import javax.swing.Icon;
import javax.swing.plaf.metal.MetalIconFactory;
import java.net.URI;

import edu.sdsc.grid.io.srb.SRBFileSystem;
import edu.sdsc.grid.io.srb.SRBAccount;
import edu.sdsc.grid.io.srb.SRBFile;
import edu.sdsc.grid.io.FileFactory;
import edu.sdsc.grid.io.GeneralFile;


/**
 * FileSystemView is JFileChooser's gateway to the file system. Since the JDK1.1
 * File API doesn't allow access to such information as root partitions, file
 * type information, or hidden file bits, this class is designed to intuit as
 * much OS-specific file system information as possible.
 * 
 * <p>
 * 
 * Java Licensees may want to provide a different implementation of
 * FileSystemView to better handle a given operating system.
 * 
 * @version 1.46 04/27/04
 * @author Jeff Dinkins
 */

public class SRBFileSystemView {
    private SRBFileSystem fileSystem;

    public SRBFileSystemView() throws FileNotFoundException, IOException {
        this((SRBFileSystem)null);
    }
    public SRBFileSystemView(SRBFileSystem fs) throws FileNotFoundException, IOException {
        if(fs == null){
            fs = new SRBFileSystem(new SRBAccount());
        }
        setFileSystem(fs);
    }

    public SRBFileSystemView(SRBFile f) throws FileNotFoundException, IOException {
        SRBFileSystem fs = null;
        if(f == null){
            fs = new SRBFileSystem(new SRBAccount());
        }else{
            fs = (SRBFileSystem)f.getFileSystem();
        }
        setFileSystem(fs);
    }
    
    public SRBFileSystemView(URI uri) throws IOException{
        SRBFile f = new SRBFile(uri);
        SRBFileSystem fs = null;
        if(f == null){
            fs = new SRBFileSystem(new SRBAccount());
        }else{
            fs = (SRBFileSystem)f.getFileSystem();
        }
        setFileSystem(fs);
    }

    public void setFileSystem(SRBFileSystem fs) throws NullPointerException{
        if(fs == null)
            throw new NullPointerException("The SRBFileSystem must not be null!");
        this.fileSystem = fs;
    }
    /**
     * Determines if the given file is a root in the navigatable tree(s).
     * Examples: Windows 98 has one root, the Desktop folder. DOS has one root
     * per drive letter, <code>C:\</code>, <code>D:\</code>, etc. Unix has
     * one root, the <code>"/"</code> directory.
     * 
     * The default implementation gets information from the
     * <code>ShellFolder</code> class.
     * 
     * @param f
     *            a <code>File</code> object representing a directory
     * @return <code>true</code> if <code>f</code> is a root in the
     *         navigatable tree.
     * @see #isFileSystemRoot
     */
    public boolean isRoot(SRBFile f) {
        if (f == null || !f.isAbsolute()) {
            return false;
        }

        SRBFile[] roots = getRoots();
        for (int i = 0; i < roots.length; i++) {
            if (roots[i].equals(f)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns true if the file (directory) can be visited. Returns false if the
     * directory cannot be traversed.
     * 
     * @param f
     *            the <code>SRBFile</code>
     * @return <code>true</code> if the file/directory can be traversed,
     *         otherwise <code>false</code>
     * @see JFileChooser#isTraversable
     * @see FileView#isTraversable
     */
    public Boolean isTraversable(SRBFile f) {
        return Boolean.valueOf(f.isDirectory());
    }

    /**
     * Name of a file, directory, or folder as it would be displayed in a system
     * file browser. Example from Windows: the "M:\" directory displays as
     * "CD-ROM (M:)"
     * 
     * The default implementation gets information from the ShellFolder class.
     * 
     * @param f
     *            a <code>SRBFile</code> object
     * @return the file name as it would be displayed by a native file chooser
     * @see JFileChooser#getName
     */
    public String getSystemDisplayName(SRBFile f) {
        String name = null;
        if (f != null) {
            name = f.getName();
        }
        return name;
    }

    /**
     * Type description for a file, directory, or folder as it would be
     * displayed in a system file browser. Example from Windows: the "Desktop"
     * folder is desribed as "Desktop".
     * 
     * Override for platforms with native ShellFolder implementations.
     * 
     * @param f
     *            a <code>SRBFile</code> object
     * @return the file type description as it would be displayed by a native
     *         file chooser or null if no native information is available.
     * @see JFileChooser#getTypeDescription
     */
    public String getSystemTypeDescription(SRBFile f) {
        return null;
    }

    /**
     * Icon for a file, directory, or folder as it would be displayed in a
     * system file browser. Example from Windows: the "M:\" directory displays a
     * CD-ROM icon.
     * 
     * The default implementation gets information from the ShellFolder class.
     * 
     * @param f
     *            a <code>SRBFile</code> object
     * @return an icon as it would be displayed by a native file chooser
     * @see JFileChooser#getIcon
     */
    public Icon getSystemIcon(SRBFile f) {
        Icon icon = null;
        if (f != null) {
            if(f.isDirectory()){
                icon = new MetalIconFactory.FolderIcon16();
            }else{
                icon = new MetalIconFactory.FileIcon16();
            }
        }
        return icon;
    }

    /**
     * On Windows, a file can appear in multiple folders, other than its parent
     * directory in the filesystem. Folder could for example be the "Desktop"
     * folder which is not the same as file.getParentFile().
     * 
     * @param folder
     *            a <code>SRBFile</code> object repesenting a directory or
     *            special folder
     * @param file
     *            a <code>SRBFile</code> object
     * @return <code>true</code> if <code>folder</code> is a directory or
     *         special folder and contains <code>file</code>.
     */
    public boolean isParent(SRBFile folder, SRBFile file) {
        if (folder == null || file == null) {
            return false;
        }

        return folder.equals(file.getParentFile());
    }

    /**
     * 
     * @param parent
     *            a <code>SRBFile</code> object repesenting a directory or
     *            special folder
     * @param fileName
     *            a name of a file or folder which exists in <code>parent</code>
     * @return a SRBFile object. This is normally constructed with <code>new
     * SRBFile(parent, fileName)</code>
     *         except when parent and child are both special folders, in which
     *         case the <code>SRBFile</code> is a wrapper containing a
     *         <code>ShellFolder</code> object.
     */
    public SRBFile getChild(SRBFile parent, String fileName) {
        String[] children = parent.list();
        for (int i = 0; i < children.length; i++) {
            if (children[i].equals(fileName)) {
                return new SRBFile(fileSystem, parent.getPath(), children[i]);
            }
        }

        return createFileObject(parent, fileName);
    }

    /**
     * Checks if <code>f</code> represents a real directory or file as opposed
     * to a special folder such as <code>"Desktop"</code>. Used by UI classes
     * to decide if a folder is selectable when doing directory choosing.
     * 
     * @param f
     *            a <code>SRBFile</code> object
     * @return <code>true</code> if <code>f</code> is a real file or
     *         directory.
     */
    public boolean isFileSystem(SRBFile f) {
        return true;
     }

    /**
     * Creates a new folder with a default folder name.
     */
    public SRBFile createNewFolder(SRBFile containingDir){
        SRBFile newFold = new SRBFile(containingDir, "newFold");
        return newFold;
    }

    /**
     * Returns whether a file is hidden or not.
     */
    public boolean isHiddenFile(SRBFile f) {
        return f.isHidden();
    }

    /**
     * Is dir the root of a tree in the file system, such as a drive or
     * partition. Example: Returns true for "C:\" on Windows 98.
     * 
     * @param f
     *            a <code>SRBFile</code> object representing a directory
     * @return <code>true</code> if <code>f</code> is a root of a filesystem
     * @see #isRoot
     */
    public boolean isFileSystemRoot(SRBFile dir) {
        return isRoot(dir);
    }

    /**
     * Used by UI classes to decide whether to display a special icon for drives
     * or partitions, e.g. a "hard disk" icon.
     * 
     * The default implementation has no way of knowing, so always returns
     * false.
     * 
     * @param dir
     *            a directory
     * @return <code>false</code> always
     */
    public boolean isDrive(SRBFile dir) {
        return false;
    }

    /**
     * Used by UI classes to decide whether to display a special icon for a
     * floppy disk. Implies isDrive(dir).
     * 
     * The default implementation has no way of knowing, so always returns
     * false.
     * 
     * @param dir
     *            a directory
     * @return <code>false</code> always
     */
    public boolean isFloppyDrive(SRBFile dir) {
        return false;
    }

    /**
     * Used by UI classes to decide whether to display a special icon for a
     * computer node, e.g. "My Computer" or a network server.
     * 
     * The default implementation has no way of knowing, so always returns
     * false.
     * 
     * @param dir
     *            a directory
     * @return <code>false</code> always
     */
    public boolean isComputerNode(SRBFile dir) {
        return false;
    }

    /**
     * Returns all root partitions on this system. For example, on Windows, this
     * would be the "Desktop" folder, while on DOS this would be the A: through
     * Z: drives.
     */
    public SRBFile[] getRoots() {
        SRBFile[] files = new SRBFile[1];
        files[0] = new SRBFile(fileSystem, "/");
        return files;
//        return (SRBFile[])SRBFile.listRoots();
    }

    // Providing default implementations for the remaining methods
    // because most OS file systems will likely be able to use this
    // code. If a given OS can't, override these methods in its
    // implementation.

    public SRBFile getHomeDirectory() {
        return (SRBFile)FileFactory.newFile(fileSystem, fileSystem.getHomeDirectory());
    }

    /**
     * Return the user's default starting directory for the file chooser.
     * 
     * @return a <code>SRBFile</code> object representing the default starting
     *         folder
     */
    public SRBFile getDefaultDirectory() {
        return getHomeDirectory();
    }

    /**
     * Returns a SRBFile object constructed in dir from the given filename.
     */
    public SRBFile createFileObject(SRBFile dir, String filename) {
        if (dir == null) {
            return new SRBFile(fileSystem, filename);
        } else {
            return new SRBFile(dir, filename);
        }
    }

    /**
     * Returns a SRBFile object constructed from the given path string.
     */
    public SRBFile createFileObject(String path) {
        SRBFile f = new SRBFile(fileSystem, path);
        if (isFileSystemRoot(f)) {
            f = createFileSystemRoot(f);
        }
        return f;
    }

    /**
     * Gets the list of shown (i.e. not hidden) files.
     */
    public SRBFile[] getFiles(SRBFile dir, boolean useFileHiding) {
        Vector files = new Vector();

        // add all files in dir
        GeneralFile[] names = dir.listFiles();
        SRBFile f;

        int nameCount = (names == null) ? 0 : names.length;
        for (int i = 0; i < nameCount; i++) {
            if (Thread.currentThread().isInterrupted()) {
                break;
            }
            f = (SRBFile)names[i];
            if (!useFileHiding || !isHiddenFile(f)) {
                files.addElement(f);
            }
        }

        return (SRBFile[]) files.toArray(new SRBFile[files.size()]);
    }

    /**
     * Returns the parent directory of <code>f</code>.
     * 
     * @param dir
     *            the <code>SRBFile</code> being queried
     * @return the parent directory of <code>dir</code>, or <code>null</code>
     *         if <code>dir</code> is <code>null</code>
     */
    public SRBFile getParentDirectory(SRBFile f) {
        if (f != null && f.exists()) {
            SRBFile parent = (SRBFile)f.getParentFile();
            if (parent == null) {
                parent = f;
            }
            return parent;
        }
        return null;
    }

    /**
     * Creates a new <code>SRBFile</code> object for <code>f</code> with
     * correct behavior for a file system root directory.
     * 
     * @param f
     *            a <code>SRBFile</code> object representing a file system root
     *            directory, for example "/" on Unix or "C:\" on Windows.
     * @return a new <code>SRBFile</code> object
     */
    protected SRBFile createFileSystemRoot(SRBFile f) {
        return new SRBFile((SRBFileSystem)f.getFileSystem(), "/");
    }
}
