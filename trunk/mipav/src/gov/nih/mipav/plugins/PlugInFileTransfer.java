package gov.nih.mipav.plugins;

import edu.sdsc.grid.io.GeneralFile;

/**
 * An interface for a plugin which transfers files between two file systems.
 * @author Hailong Wang 04/18/2006
 * @version 1.0
 */
public interface PlugInFileTransfer extends PlugIn {
    /**
     * Returns whether this plugin supports the files transfering.
     * @return whether this plugin supports the files transfering.
     */
    public boolean canTransferFiles();

    /**
     * Transfers the files from one file system to the other file system.
     */
    public void transferFiles();
}
