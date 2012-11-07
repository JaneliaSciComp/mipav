package gov.nih.mipav.plugins;


/**
 * An interface for a plugin which transfers files between two file systems.
 *
 * @author  Hailong Wang
 */
public interface PlugInFileTransfer extends PlugIn {
	
	// ~ Static fields/initializers ------------------------------------------------------------------
	
	public static final String[] CATEGORY = {"File Transfer"};

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Transfers the files from one file system to the other file system.
     */
    void transferFiles();
}
