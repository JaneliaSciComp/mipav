package gov.nih.mipav.view;

import javax.swing.filechooser.FileFilter;

import java.awt.Component;

/**
 * The FileSelector interface defines the interface for file selector
 * which is used to select files.
 * 
 * @author Hailong Wang, Ph.D
 * 
 * @version 1.0 06/16/2006
 */
public interface FileSelector {
    /**
     * Returns the selected file names.
     * 
     * Notes: 
     * I want this interface to cover the SRB file selection. So I used
     * the string array as the return type.
     * 
     * @return the selected file names.
     */
    public String[] getSelectedFileNames();
    
    /**
     * Returns the file filter used by <code>FileSelector</code>.
     * @return the file filter used by <code>FileSelector</code>.
     */
    public FileFilter getFileFilter();
    
    /**
     * Sets the file filter to the <code>FileSelector</code>.
     * @param fileFilter the new file filter.
     */
    public void setFileFilter(FileFilter fileFilter);
    
    /**
     * Shows the open dialog to retrieve the user's input, the approve button
     * will be "Open", and title will be "Open Image", only files are displayed,
     * multi selection will be allowed.
     * 
     * @param parent  the parent component
     */
    public void showOpenDialog(Component parent);
    
    /**
     * Shows the open dialog to retrieve the user's input, the approve button
     * will be "Save", and title will be "Save Image", only directory are displayed,
     * multi selection is not allowed.
     * 
     * @param parent  the parent component
     */
    public void showSaveDialog(Component parent);
    
    /**
     * Shows the save as dialog to retrieve the user's input, the approve button
     * will be "Save", and title will be "Save Image As", only file are displayed,
     * multi selection is not allowed.
     * 
     * @param parent  the parent component
     */
    public void showSaveAsDialog(Component parent);
}
