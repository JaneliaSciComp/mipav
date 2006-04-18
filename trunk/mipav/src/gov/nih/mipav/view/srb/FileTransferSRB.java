package gov.nih.mipav.view.srb;

import edu.sdsc.grid.io.GeneralFile;
import edu.sdsc.grid.io.GeneralRandomAccessFile;
import edu.sdsc.grid.io.local.LocalFile;
import edu.sdsc.grid.io.local.LocalRandomAccessFile;
import edu.sdsc.grid.io.srb.SRBFile;
import edu.sdsc.grid.io.srb.SRBRandomAccessFile;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJProgressBar;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;
import java.util.Vector;

public class FileTransferSRB extends Thread implements ActionListener, WindowListener{
    /**
     * The source file list.
     */
    private Vector sourceFileList;
    
    /**
     * The target file list.
     */
    private Vector targetFileList;
    
    /**
     * The progress bar used to indicate the progress of reading or writing.
     */
    private ViewJProgressBar progressBar;
    
    /**
     * The location of the progress bar.
     */
    private Point progressBarLocation = new Point(500, 400);
    
    /**
     * The flag to indicate the visibility of the progress bar.
     */
    private boolean progressBarVisible = true;
    
    /**
     * The flag to indicate whether the reading or writing is completed
     */
    private boolean completed;
    
    private boolean threadSeperated = true;
    
    public FileTransferSRB(Vector sourceFileList, Vector targetFileList){
        this.sourceFileList = sourceFileList;
        this.targetFileList = targetFileList;
    }

    /**
     * Returns true if the function copyFiles is run in the seperate thread.
     */
    public boolean isThreadSeperated(){
        return threadSeperated;
    }
    
    /**
     * Sets the flag which indicates the function copyFiles is run in the seperate thread.
     * @param threadSeperated the flag to indicate the function copyFiles is run in the seperate thread.
     */
    public void setThreadSeperated(boolean threadSeperated){
        this.threadSeperated = threadSeperated;
    }
    /**
     * Returns the whether the progress bar should be visible.
     */
    public boolean isProgressBarVisible(){
        return progressBarVisible;
    }
    
    /**
     * Sets the progress bar's visibility.
     * @param visible the flag to set.
     */
    public void setProgressBarVisible(boolean visible){
        this.progressBarVisible = visible;
        if(progressBar != null){
            progressBar.setVisible(visible);
        }
    }
    
    /**
     * Returns the location of the progress bar.
     */
    public Point getProgressBarLocation(){
        return progressBarLocation;
    }
    
    /**
     *   Constructs progress bar
     *   @param imageName    title of the toolbar
     *   @param message      message to be displayed in the frame
     *   @param start        start (typical = 0)
     *   @param end          end   (typical = 100)
     */
    protected void buildProgressBar(String imageName, String message, int start, int end) {
        if (progressBarVisible == true) {
            progressBar = new ViewJProgressBar(imageName, message, start, end, true, this, this);
        }
    }

    /**
     *   Initializes progress bar
     */
    protected void initProgressBar() {
        if (progressBarVisible == true && progressBar != null) {
            if (progressBarLocation == null) {
                MipavUtil.centerOnScreen(progressBar);
            }
            else {
                progressBar.setLocation(progressBarLocation);
            }
            progressBar.setVisible(true);
        }
    }
    
    /**
     * Sets the location of the progress bar.
     */
    public void setProgressBarLocation(Point location){
        this.progressBarLocation = location;
        if(progressBar != null){
            progressBar.setLocation(location);
        }
    }

    /**
     * Returns the flag indicating if the file transfering has completed sucessfully.
     */
    public boolean isCompleted(){
        return completed;
    }

    /**
     *   Disposes of progress bar
     */
    protected void disposeProgressBar() {
        if (progressBar != null)
            progressBar.dispose();
    }

    /**
     * Copies the source files to the target files.
     */
    public void copyFiles(){
        if(sourceFileList == null || sourceFileList.size() == 0 || targetFileList == null || targetFileList.size() == 0){
            return;
        }
 
        /**
         * Builds a progress bar first.
         */
        int mod = 1;
        if(sourceFileList.size() > 100){
            buildProgressBar("Transfering " + ((GeneralFile)sourceFileList.get(0)).getName(), "Transfering files ...", 0, 100);
            mod = sourceFileList.size()/100;
        }else{
            buildProgressBar("Transfering " + ((GeneralFile)sourceFileList.get(0)).getName(), "Transfering files ...", 0, sourceFileList.size());
        }
        
 
        initProgressBar();
        for(int i = 0; i < sourceFileList.size(); i++){
            if ((i)%mod==0 && isProgressBarVisible()){
                progressBar.setTitle("Transfering " + ((GeneralFile)sourceFileList.get(i)).getName());
                if(sourceFileList.size() > 100){
                    progressBar.updateValue(Math.round((float)i/(sourceFileList.size()-1) * 100), isThreadSeperated());
                }else{
                    if(sourceFileList.size() == 1){
                        progressBar.updateValue(100, isThreadSeperated());
                    }else{
                        progressBar.updateValue(Math.round((float)i/(sourceFileList.size()-1) * 100), isThreadSeperated());
                    }
                }
            }
            GeneralFile sourceFile = (GeneralFile)sourceFileList.get(i);
            GeneralFile targetFile = (GeneralFile)targetFileList.get(i);
            copy(sourceFile, targetFile);
        }
        
        disposeProgressBar();
        completed = true;
    }

    /**
     * Copies the source file to the destination file.
     * @param sourceFile the source file
     * @param targetFile the target file
     * @return
     */
    public static boolean copy(GeneralFile sourceFile, GeneralFile targetFile){
        GeneralRandomAccessFile sourceRandomAccessFile;
        GeneralRandomAccessFile destinationRandomAccessFile;
        try {
            if (sourceFile instanceof LocalFile) {
                sourceRandomAccessFile = new LocalRandomAccessFile(
                        (LocalFile) sourceFile, "r");
            } else {
                sourceRandomAccessFile = new SRBRandomAccessFile(
                        (SRBFile) sourceFile, "r");
            }
            if (targetFile instanceof LocalFile) {
                destinationRandomAccessFile = new LocalRandomAccessFile(
                        (LocalFile) targetFile, "rw");
            } else {
                destinationRandomAccessFile = new SRBRandomAccessFile(
                        (SRBFile) targetFile, "rw");
            }
            long length = sourceRandomAccessFile.length();
            byte[] buffer = new byte[(int)length];
            sourceRandomAccessFile.read(buffer, 0, (int)length);
            destinationRandomAccessFile.write(buffer, 0, (int)length);

            sourceRandomAccessFile.close();
            destinationRandomAccessFile.close();
            return true;
        } catch (IOException e) {
            e.printStackTrace();
            MipavUtil.displayError("File I/O error: " + e.getMessage());
            return false;
        }
    }

    public void run(){
        copyFiles();
    }

    /*********************************
     ***** Window Event Listener *****
     *********************************/

    /**
     * Do nothing.
     * @param event  the window opened event
     */
    public void windowOpened(WindowEvent event) {}

    /**
     *   Sets completed to false, disposes the progress bar and notifies all
     *   listeners that the algorithm is stopped.
     *   @param event    event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        completed = false;
        if (progressBar != null)
            progressBar.dispose();
    }

    /**
     * Do nothing.
     * @param event  the window closed event
     */
    public void windowClosed(WindowEvent event) {
    }

    /**
     * Do nothing.
     * @param event  the window iconified event
     */
    public void windowIconified(WindowEvent event) {
    }

    /**
     * Do nothing.
     * @param event  the window deiconified event
     */
    public void windowDeiconified(WindowEvent event) {
    }

    /**
     * Do nothing.
     * @param event  the window activated event
     */
    public void windowActivated(WindowEvent event) {
    }

    /**
     * Do nothing.
     * @param event  the window deactivated event
     */
    public void windowDeactivated(WindowEvent event) {
    }
    
    /*********************************
     ***** Action Event Listener *****
     *********************************/
    
    /**
     * Stops the thread and destroys the progress bar.
     */
    public void actionPerformed(ActionEvent e){
        if (progressBar != null && !progressBar.isComplete()) {
            completed = false;
            progressBar.dispose();
        }
    }
}
