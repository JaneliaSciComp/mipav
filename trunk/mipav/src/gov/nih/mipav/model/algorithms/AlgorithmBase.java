package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.text.*;

import java.util.*;


/**
 * Base abstract class for algorithms.
 *
 * @version  0.1 Feburary 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public abstract class AlgorithmBase extends Thread implements ActionListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Show standard progress bar with cancel. */
    public static final int STANDARD = 0;

    /** Show progress bar without a cancel button. */
    public static final int NO_CANCEL = 1;

    /** Do no show progress bar. */
    public static final int NO_PROGRESS = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Should be set to true if NOT in a single thread - will NOT force a graphics update of the progress bar. Should be
     * set to false if in a single thread - will force a graphics update of the progress bar.
     */
    protected boolean activeImage = true;

    /** This flag will be set to true when the algorithm has completed. */
    protected boolean completed = false;

    /** Destination flag indicating if a destination (result) image is generated. */
    protected boolean destFlag = false;

    /** Destination image. */
    protected ModelImage destImage;

    /**
     * String for successfully completed algorithms that will go into the history of the model image that was changed
     * (based on destFlag).
     */
    protected String historyString = new String();

    /** If true process each image of a 3D volume independently. */
    protected boolean image25D = false;

    /** Mask indicating which voxels to process. If true process voxel else skip. */
    protected BitSet mask = null;

    /** Flag indicating if a whether of not the progress bar is visible. */
    protected boolean pBarVisible = true;

    /** Progress bar object. */
    protected ViewJProgressBar progressBar;

    /** Progress bar default location. */
    protected Point progressBarLocation = null;

    /** Progress mode - either standard, no cancel, or no progress bar. */
    protected int progressMode = STANDARD;

    /** Source image. */
    protected ModelImage srcImage;

    /** Flag indicating whether or not the thread is stopped. */
    protected boolean threadStopped = false;

    /** Elapsed time (in milliseconds) -- time it took for algorithm to run. */
    private double elapsedTime = 0;

    /** If true, keep do not dispose of progress bar when finalize() is called. */
    private boolean keepProgressBar = false;

    /**
     * Vector list of AlgorithmInterface objects. When the algorithm has been stopped or completed, all listeners in
     * this list are notified.
     */
    private Vector objectList = new Vector();

    /** Start time (in milliseconds) to be used to compute elapsed time. */
    private long startTime = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor which sets thread stopped to false, source and destination images to null, and destination
     * flag to false.
     */
    public AlgorithmBase() {
        this.destImage = null;
        this.srcImage = null;
        destFlag = false;
        threadStopped = false;
    }

    /**
     * Constructor which sets thread stopped to false and sets source and destination images.
     *
     * @param  destImage  Destination image, can be null.
     * @param  srcImage   Source image, should not be null.
     */
    public AlgorithmBase(ModelImage destImage, ModelImage srcImage) {
        this.destImage = destImage;
        this.srcImage = srcImage;

        if (destImage == null) {
            destFlag = false;
        } else {
            destFlag = true;
        }

        threadStopped = false;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Actually runs the algorithm. Implemented by inheriting algorithms.
     */
    public abstract void runAlgorithm();

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************
    /**
     * Sets completed to <code>false</code>, stops the thread, and disposes the progress bar.
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {

        if ((progressBar != null) && !progressBar.isComplete()) {
            completed = false;

            if (Preferences.is(Preferences.PREF_LOG)) {
                String tempStr = new String(this.getClass().getName().substring(this.getClass().getName().lastIndexOf(".") +
                                                                                1) + " failed.");

                if (srcImage != null) {
                    srcImage.getHistoryArea().append(tempStr);
                }
            }

            threadStopped = true;

            if (progressBar != null) {
                progressBar.dispose();
            }
        }
    }

    /**
     * Add a listener to this class so that when when the algorithm has completed processing it can use notifyListener
     * to notify all listeners that the algorithm has completed.
     *
     * @param  obj  AlgorithmInterface "object' to be added to the list
     */
    public void addListener(AlgorithmInterface obj) {
        objectList.addElement(obj);
    }

    /**
     * Computes the elapased time as the difference between the start time and the current time (both of which are in
     * milliseconds).
     *
     * @return  the elapsed time in seconds -- this is a double value
     */
    public double computeElapsedTime() {

        long now = System.currentTimeMillis();

        elapsedTime = (double) (now - startTime);

        // if elasedTime is invalid, then set it to 0
        if (elapsedTime <= 0) {
            elapsedTime = (double) 0.0;
        }

        return (double) (elapsedTime / 1000.0); // return in seconds!!
    }

    /**
     * Displays an error in a frame.
     *
     * @param  error  string that is displayed
     */
    public void displayError(String error) {
        MipavUtil.displayError(error);
    }

    /**
     * Display string pass into the method. If the string is null no message is displayed. The completed flag is set to
     * <code>false</code> and the progress bar is disposed;
     *
     * @param  strErr  the string that is to be displayed. If the string is null no message is displayed.
     * @param  gcFlag  if <code>true</code> call the garbage collector.
     */
    public void errorCleanUp(String strErr, boolean gcFlag) {

        if (gcFlag == true) {
            System.gc();
        }

        if (strErr != null) {
            displayError(strErr);
        }

        setCompleted(false);
        setThreadStopped(true);
        disposeProgressBar();

        if (destImage != null) {
            destImage.releaseLock();
        }
    }

    /**
     * Calls garbage collector to release system resources.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        mask = null;

        if (keepProgressBar == false) {
            disposeProgressBar();
        }

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /**
     * Returns the elapsed time in seconds. This does not compute a new elapsed time.
     *
     * @return  the elapsed time, in seconds computed earlier
     *
     * @see     #computeElapsedTime()
     */
    public double getElapsedTime() {
        return (double) (elapsedTime / 1000.0); // convert from milliseconds to seconds
    }

    /**
     * Gets the Bitset mask object.
     *
     * @return  The mask indicating which voxels of the image to process
     */
    public BitSet getMask() {
        return mask;
    }

    /**
     * Should be set to true if NOT in a single thread - will NOT force a graphics update of the progress bar. Should be
     * set to false if in a single thread - will force a graphics update of the progress bar.
     *
     * @return  boolean
     */
    public boolean isActiveImage() {
        return this.activeImage;
    }

    /**
     * Returns flag that indicates that the algorithm has been sucessfully completed.
     *
     * @return  <code>true</code> if algorithm completed successfully, <code>false</code> if it was stopped or had an
     *          error.
     */
    public boolean isCompleted() {
        return completed;
    }

    /**
     * Returns flag that indicates if the image should be processed slice by slice.
     *
     * @return  <code>true</code> if image should be processed slice by slice.
     */
    public boolean isImage25D() {
        return image25D;
    }

    /**
     * If false (default state) the progress bar is disposed in the finalized method.
     *
     * @return  boolean The state of the keepProgressBar flag.
     */
    public boolean isKeepProgressBar() {
        return keepProgressBar;
    }

    /**
     * Returns flag that indicates that the progressBar is visible.
     *
     * @return  <code>true</code> if progress bar is visible.
     */
    public final boolean isProgressBarVisible() {

        if ((pBarVisible == true) && (progressBar != null)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns flag that indicates that the algorithm thread has been stopped.
     *
     * @return  <code>true</code> if thread has stopped.
     */
    public boolean isThreadStopped() {
        return threadStopped;
    }

    /**
     * Appends the elapsed time in seconds to the log.
     */
    public void logElapsedTime() {

        NumberFormat nf = NumberFormat.getInstance();

        nf.setMaximumFractionDigits(2);
        nf.setMinimumFractionDigits(2);

        historyString = new String("Elapsed Time = " + nf.format(getElapsedTime()) + " sec.\n");
        writeLog();
    }

    /**
     * Used to notify all listeners that the algorithm has completed.
     *
     * @param  algorithm  algorithm class that has completed the function
     */
    public void notifyListeners(AlgorithmBase algorithm) {

        for (int i = 0; i < objectList.size(); i++) {
            ((AlgorithmInterface) objectList.elementAt(i)).algorithmPerformed(algorithm);
        }

    }

    /**
     * Remove a listener from the class.
     *
     * @param  obj  the algorithm listener to be removed from the list for this algo
     */
    public void removeListener(AlgorithmInterface obj) {
        objectList.removeElement(obj);
    }

    /**
     * Performs start-up and tear-down operations that should be done by all algorithms (timing, history log). Since
     * this class extends the Thread class it can be run in its own thread by invoking object.start(); It can also be
     * invoked without a new thread by calling the the run() method directly (ie. object.run()).
     */
    public void run() {
        setStartTime();
        ///if ( this instanceof AlgorithmHistoryInterface ) {
        ///    constructLog();
        ///}

        runAlgorithm();

        // write out anything that the algorithm put into the (history) logging string(s)
        writeLog();

        computeElapsedTime();

        if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
            logElapsedTime();
        }

        notifyListeners(this);

        /// finalize();
    }

    /**
     * Sets the active image flag which controls how the progress bar is updated If true the progress bar is not forced
     * to update.
     *
     * @param  activeFlag  Should be set to true if NOT in a single thread - will NOT force a graphics update of the
     *                     progress bar. Should be set to false if in a single thread - will force a graphics update of
     *                     the progress bar.
     */
    public void setActiveImage(boolean activeFlag) {
        this.activeImage = activeFlag;
    }


    /**
     * Sets completed to flag indicating if algorithm has sucessfully completed.
     *
     * @param  flag  Flag to set to <code>true</code> if the algorithm has completed.
     */
    public void setCompleted(boolean flag) {
        completed = flag;
    }

    /**
     * Sets a flag to indicate the image has dimensions of (i.e. spacial, spacial, time) and image processing routines
     * should apply 2.5D algorithm.
     *
     * @param  flag  flag to set to
     */
    public void setImage25D(boolean flag) {
        image25D = flag;
    }

    /**
     * Sets the state of the keepProgressBar flag. If false (default state) the progress bar is disposed in the
     * finalized method.
     *
     * @param  flag  The state of the keepProgressBar flag.
     */
    public void setKeepProgressBar(boolean flag) {
        this.keepProgressBar = flag;
    }

    /**
     * Sets the mask (BitSet object).
     *
     * @param  imageMask  BitSet object indicating which voxels to process
     */
    public void setMask(BitSet imageMask) {
        mask = imageMask;
    }

    /**
     * Sets the location of the progress bar to the specified x, y coordinate.
     *
     * @param  xCoord  int x coordinate of the new progress bar location
     * @param  yCoord  int y coordinate of the new progress bar location
     */
    public void setProgressBarInitLocation(int xCoord, int yCoord) {

        if (progressBarLocation == null) {
            progressBarLocation = new Point(xCoord, yCoord);
        } else {
            progressBarLocation.setLocation(xCoord, yCoord);
        }
    }

    /**
     * Sets Progress Bar visibility.
     *
     * @param  flag  flag to set to
     */
    public void setProgressBarVisible(boolean flag) {

        pBarVisible = flag;

        if (progressBar != null) {
            progressBar.setVisible(flag);
        }
    }

    /**
     * Sets the start time to the current time. This should be called at the beginning of the run() method.
     */
    public void setStartTime() {
        startTime = System.currentTimeMillis();
    }

    /**
     * Sets the thread stopped to flag indicating if algorithm has stopped.
     *
     * @param  flag  flag to set to
     */
    public void setThreadStopped(boolean flag) {
        threadStopped = flag;
    }

    /**
     * Checks to see if a thread is already running on this object. If so, it returns false, else it starts the thread
     * and returns true.
     *
     * @param   priority  thread priority
     *
     * @return  false if a thread is already running this algorithm, true otherwise
     */
    public final boolean startMethod(int priority) {

        if (this.isAlive() == true) {
            return false;
        } else {

            if ((priority < MAX_PRIORITY) && (priority > MIN_PRIORITY)) {
                setPriority(priority);
            } else {
                setPriority(MIN_PRIORITY);
            }

            this.start();
        }

        return true;
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
        threadStopped = true;

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

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

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

        if (pBarVisible == true) {

            if (progressMode == STANDARD) {
                progressBar = new ViewJProgressBar(imageName, message, start, end, true, this, this);
            } else if (progressMode == NO_CANCEL) {
                progressBar = new ViewJProgressBar(imageName, message, start, end, false, this, this);
            } else if (progressMode == NO_PROGRESS) {
                Preferences.debug("Tried to build a progress bar when progressMode == NO_PROGRESS.\n");
            }
        }
    }

    /**
     * Takes an array of strings and converts each entry into a float value. Useful for updating DICOM file information.
     *
     * @param   str  Some number of strings in an array
     *
     * @return  An array of floats; the same number of entries in str are returned in the array.
     */
    protected float[] convertIntoFloat(String[] str) {

        if (str == null) {
            return null;
        }

        float[] imagePositionCoords = new float[str.length];

        for (int i = 0; i < str.length; i++) { // convert all 3 axis coords from string into float.
            imagePositionCoords[i] = Float.valueOf(str[i]).floatValue();
        }

        return imagePositionCoords;
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

        if ((pBarVisible == true) && (progressBar != null)) {

            if (progressBarLocation == null) {
                MipavUtil.centerOnScreen(progressBar);
            } else {
                progressBar.setLocation(progressBarLocation);
            }

            progressBar.setVisible(true);
        }
    }

    /**
     * Writes the logString to the appropriate history area. If algorithm was completed successfully, write the history
     * string to the algorithm (and copy the source history into destination if there is a destination image)
     */
    protected void writeLog() {

        // get the current date/time
        DateFormat df1 = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM);

        // write to the history area
        if (Preferences.is(Preferences.PREF_LOG) && completed) {

            if (destImage != null) {

                if (srcImage != null) {
                    destImage.getHistoryArea().setText(srcImage.getHistoryArea().getText());
                }

                if (historyString != null) {
                    destImage.getHistoryArea().append("[" + df1.format(new Date()) + "] " + historyString);
                }
            } else if (srcImage != null) {

                if (historyString != null) {
                    srcImage.getHistoryArea().append("[" + df1.format(new Date()) + "] " + historyString);
                }
            }
        }
    }

}
