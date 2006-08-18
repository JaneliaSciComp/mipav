package gov.nih.mipav.view;

import java.io.File;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;
import java.awt.Component;

/**
 * The implementation of the FileSelector interface for the local file
 * system, for the remote file system you should have different implementation,
 * basically this class is a wrapper of the JFileChooser.
 * 
 * @author Hailong Wang, Ph.D
 * @version 1.0, 06/21/2006
 */
public class FileSelectorImpl implements FileSelector {
    private JFileChooser fileChooser;
    private FileSelectorImpl(){
        fileChooser = new JFileChooser(new File(Preferences.getImageDirectory()));
        fileChooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
        fileChooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        fileChooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
        fileChooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));
        FileFilter currentFileFilter = getFileFilter(Preferences.getFileFilter());
        fileChooser.setFileFilter(currentFileFilter);
    }
    
    public static FileSelectorImpl newInstance(){
        return new FileSelectorImpl();
    }
    
    /**
     * @see FileSelector#getFileFilter()
     */
    public FileFilter getFileFilter(){
        if(fileChooser == null){
            return null;
        }
        
        return fileChooser.getFileFilter();
    }
    
    /**
     * @see FileSelector#setFileFilter(FileFilter)
     */
    public void setFileFilter(FileFilter fileFilter){
        if(fileChooser != null){
            fileChooser.setFileFilter(fileFilter);
        }
    }
    
    /**
     * @see FileSelector#getSelectedFileNames()
     */
    public String[] getSelectedFileNames() {
        if(fileChooser == null){
            return null;
        }
        
        File[] selectedFiles = fileChooser.getSelectedFiles();
        if(selectedFiles == null || selectedFiles.length == 0){
            return null;
        }
        
        String[] fileNames = new String[selectedFiles.length];
        for(int i = 0; i < selectedFiles.length; i++){
            fileNames[i] = selectedFiles[i].getAbsolutePath();
        }
        return fileNames;
    }

    /**
     * @see FileSelector#showOpenDialog(Component)
     */
    public void showOpenDialog(Component parent){
        showDialog(parent, "Open Image", "Open");
    }
    
    /**
     * @see FileSelector#showSaveDialog(Component)
     */
    public void showSaveDialog(Component parent){
        showDialog(parent, "Save Image", "Save");
    }
    
    /**
     * @see FileSelector#showSaveAsDailog(Component)
     */
    public void showSaveAsDialog(Component parent){
        showDialog(parent, "Save Image As", "Save");
    }
    
    /**
     * Sets up the parameters and show the dialog to let user make selection.
     * If user approves the selection, then stores the image directory.
     * 
     * @param parent              the parent component of the dialog.
     * @param title               the title of the dialog.
     * @param approveButtonName   the approve button name of the dialog.
     */
    private void showDialog(Component parent, String title, String approveButtonName){
        if(fileChooser == null){
            return;
        }
        fileChooser.setDialogTitle(title);
        if(approveButtonName.equals("Open")){
            fileChooser.setMultiSelectionEnabled(true);
            fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        }else{
            fileChooser.setMultiSelectionEnabled(false);
            fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        }
        int ret = fileChooser.showDialog(parent, approveButtonName);
        if(ret == JFileChooser.APPROVE_OPTION){
            File selectedFile = fileChooser.getSelectedFile();
            Preferences.setImageDirectory(selectedFile);
        }else{
            fileChooser.setSelectedFiles(null);
        }
    }
    
    /**
     * A private helper function to get the current used FileFilter from JFileChooser.
     *  
     * @param index  the index of the choosable file filters.
     * @return       the current used file filter.
     */
    private FileFilter getFileFilter(int index){
        FileFilter[] filters = fileChooser.getChoosableFileFilters();
        String[] descriptions = ViewImageFileFilter.getDescriptions();
        for(int i = 0; i < filters.length; i++){
            if(filters[i].getDescription().equals(descriptions[index])){
                return filters[i];
            }
        }
        return null;
    }
    
    public static void main(String[] args){
        FileSelector selector = FileSelectorImpl.newInstance();
        selector.showSaveDialog(null);
        selector.getSelectedFileNames();
    }
}
