package gov.nih.mipav.model.srb;

import edu.sdsc.grid.io.*;

/**
 * An interface for transfering files between different machines.
 * @author Hailong Wang
 * @version 1.0 05/10/06
 */
public interface FileTransferable {
    /**
     * Gets the source files which need to be transferred.
     * 
     * @return the source files which need to be transferred.
     */
    public GeneralFile[] getSourceFiles();
    
    /**
     * Returns the target files.
     * @return the target files.
     */
    public GeneralFile[] getTargetFiles();
    
    /**
     * Transfers the source files to the target files.
     * 
     * @param sourceFiles the source files.
     * @param targetFiles the target files.
     * @return true if the file was transferred successfully.
     */
    public boolean transfer(GeneralFile[] sourceFiles, GeneralFile[] targetFiles);
    
    public boolean transfer(GeneralFile sourceFile, GeneralFile targetFile);
}
