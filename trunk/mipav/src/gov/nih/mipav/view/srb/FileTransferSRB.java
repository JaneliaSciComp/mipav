package gov.nih.mipav.view.srb;


import gov.nih.mipav.view.*;

import edu.sdsc.grid.io.*;
import edu.sdsc.grid.io.local.*;
import edu.sdsc.grid.io.srb.*;

import java.awt.Point;
import java.awt.event.*;

import java.io.*;

import java.util.Vector;


/**
 * DOCUMENT ME!
 */
public class FileTransferSRB extends Thread implements ActionListener, WindowListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The flag to indicate whether the reading or writing is completed. */
    private boolean completed;

    /** The progress bar used to indicate the progress of reading or writing. */
    private ViewJProgressBar progressBar;

    /** The location of the progress bar. */
    private Point progressBarLocation = new Point(500, 400);

    /** The flag to indicate the visibility of the progress bar. */
    private boolean progressBarVisible = true;

    /** The source file list. */
    private Vector sourceFileList;

    /** The target file list. */
    private Vector targetFileList;

    /** DOCUMENT ME! */
    private boolean threadSeperated = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new FileTransferSRB object.
     *
     * @param  sourceFileList  DOCUMENT ME!
     * @param  targetFileList  DOCUMENT ME!
     */
    public FileTransferSRB(Vector sourceFileList, Vector targetFileList) {
        this.sourceFileList = sourceFileList;
        this.targetFileList = targetFileList;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Copies the source file to the destination file.
     *
     * @param   sourceFile  the source file
     * @param   targetFile  the target file
     *
     * @return  DOCUMENT ME!
     */
    public static boolean copy(GeneralFile sourceFile, GeneralFile targetFile) {
        GeneralRandomAccessFile sourceRandomAccessFile;
        GeneralRandomAccessFile destinationRandomAccessFile;

        try {

            if (sourceFile instanceof LocalFile) {
                sourceRandomAccessFile = new LocalRandomAccessFile((LocalFile) sourceFile, "r");
            } else {
                sourceRandomAccessFile = new SRBRandomAccessFile((SRBFile) sourceFile, "r");
            }

            if (targetFile instanceof LocalFile) {
                destinationRandomAccessFile = new LocalRandomAccessFile((LocalFile) targetFile, "rw");
            } else {
                destinationRandomAccessFile = new SRBRandomAccessFile((SRBFile) targetFile, "rw");
            }

            long length = sourceRandomAccessFile.length();
            byte[] buffer = new byte[(int) length];
            sourceRandomAccessFile.read(buffer, 0, (int) length);
            destinationRandomAccessFile.write(buffer, 0, (int) length);

            sourceRandomAccessFile.close();
            destinationRandomAccessFile.close();

            return true;
        } catch (IOException e) {
            e.printStackTrace();
            MipavUtil.displayError("File I/O error: " + e.getMessage());

            return false;
        }
    }

    public static boolean parallelCopy(GeneralFile sourceFile, GeneralFile targetFile){
        try{
            sourceFile.copyTo(targetFile);
            return true;
        }catch(IOException e){
            e.printStackTrace();
            MipavUtil.displayError("File I/O error: " + e.getMessage());
            return false;
        }
    }
    /**
     * **** Action Event Listener.*****
     *
     * @param  e  DOCUMENT ME!
     */

    /**
     * Stops the thread and destroys the progress bar.
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {

        if ((progressBar != null) && !progressBar.isComplete()) {
            completed = false;
            progressBar.dispose();
        }
    }

    /**
     * Copies the source files to the target files.
     */
    public void copyFiles() {

        if ((sourceFileList == null) || (sourceFileList.size() == 0) || (targetFileList == null) ||
                (targetFileList.size() == 0)) {
            return;
        }

        /**
         * Builds a progress bar first.
         */
        int mod = 1;
        buildProgressBar("Transfering " + ((GeneralFile) sourceFileList.get(0)).getName(), "Transfering files ...", 0,
                         100);

        if (sourceFileList.size() > 100) {
            mod = sourceFileList.size() / 100;
        }

        initProgressBar();

        for (int i = 0; i < sourceFileList.size(); i++) {

            if ((((i) % mod) == 0)) {
                progressBar.setTitle("Transfering " + ((GeneralFile) sourceFileList.get(i)).getName());

                if (sourceFileList.size() > 100) {
                    progressBar.updateValue(Math.round(((float) i / sourceFileList.size()) * 100), isThreadSeperated());
                } else {
                    progressBar.updateValue(Math.round(((float) i / sourceFileList.size()) * 100), isThreadSeperated());
                }
            }

            GeneralFile sourceFile = (GeneralFile) sourceFileList.get(i);
            GeneralFile targetFile = (GeneralFile) targetFileList.get(i);
            parallelCopy(sourceFile, targetFile);
        }

        progressBar.updateValue(100, isThreadSeperated());

        try {
            Thread.sleep(100);
        } catch (InterruptedException e) { }

        disposeProgressBar();
        completed = true;
    }

    /**
     * Returns the location of the progress bar.
     *
     * @return  DOCUMENT ME!
     */
    public Point getProgressBarLocation() {
        return progressBarLocation;
    }

    /**
     * Returns the flag indicating if the file transfering has completed sucessfully.
     *
     * @return  DOCUMENT ME!
     */
    public boolean isCompleted() {
        return completed;
    }

    /**
     * Returns the whether the progress bar should be visible.
     *
     * @return  DOCUMENT ME!
     */
    public boolean isProgressBarVisible() {
        return progressBarVisible;
    }

    /**
     * Returns true if the function copyFiles is run in the seperate thread.
     *
     * @return  DOCUMENT ME!
     */
    public boolean isThreadSeperated() {
        return threadSeperated;
    }

    /**
     * DOCUMENT ME!
     */
    public void run() {
        copyFiles();
    }

    /**
     * Sets the location of the progress bar.
     *
     * @param  location  DOCUMENT ME!
     */
    public void setProgressBarLocation(Point location) {
        this.progressBarLocation = location;

        if (progressBar != null) {
            progressBar.setLocation(location);
        }
    }

    /**
     * Sets the progress bar's visibility.
     *
     * @param  visible  the flag to set.
     */
    public void setProgressBarVisible(boolean visible) {
        this.progressBarVisible = visible;

        if (progressBar != null) {
            progressBar.setVisible(visible);
        }
    }

    /**
     * Sets the flag which indicates the function copyFiles is run in the seperate thread.
     *
     * @param  threadSeperated  the flag to indicate the function copyFiles is run in the seperate thread.
     */
    public void setThreadSeperated(boolean threadSeperated) {
        this.threadSeperated = threadSeperated;
    }

    /**
     * Do nothing.
     *
     * @param  event  the window activated event
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  the window closed event
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Sets completed to false, disposes the progress bar and notifies all listeners that the algorithm is stopped.
     *
     * @param  event  event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        completed = false;

        if (progressBar != null) {
            progressBar.dispose();
        }
    }

    /**
     * Do nothing.
     *
     * @param  event  the window deactivated event
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  the window deiconified event
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  the window iconified event
     */
    public void windowIconified(WindowEvent event) { }

    /**
     * **** Window Event Listener.*****
     *
     * @param  event  DOCUMENT ME!
     */

    /**
     * Do nothing.
     *
     * @param  event  the window opened event
     */
    public void windowOpened(WindowEvent event) { }

    /**
     * Constructs progress bar.
     *
     * @param  imageName  title of the toolbar
     * @param  message    message to be displayed in the frame
     * @param  start      start (typical = 0)
     * @param  end        end (typical = 100)
     */
    protected void buildProgressBar(String imageName, String message, int start, int end) {

        if (progressBarVisible == true) {
            progressBar = new ViewJProgressBar(imageName, message, start, end, true, this, this);
        }
    }

    /**
     * Disposes of progress bar.
     */
    protected void disposeProgressBar() {

        if (progressBar != null) {
            progressBar.dispose();
        }
    }

    /**
     * Initializes progress bar.
     */
    protected void initProgressBar() {

        if ((progressBarVisible == true) && (progressBar != null)) {

            if (progressBarLocation == null) {
                MipavUtil.centerOnScreen(progressBar);
            } else {
                progressBar.setLocation(progressBarLocation);
            }

            progressBar.setVisible(true);
        }
    }
}
