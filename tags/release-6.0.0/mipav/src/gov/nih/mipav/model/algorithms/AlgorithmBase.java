package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ProgressChangeEvent;
import gov.nih.mipav.view.ProgressChangeListener;
import gov.nih.mipav.view.ViewJProgressBar;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.event.EventListenerList;

import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Base abstract class for algorithms.
 *
 * @version  0.1 Feburary 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public abstract class AlgorithmBase extends Thread implements ActionListener, WindowListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Destination flag indicating if a destination (result) image is generated. */
    protected boolean destFlag = false;

    /** Destination image. */
    protected ModelImage destImage;

    /** If true process each image of a 3D volume independently. */
    protected boolean image25D = false;

    /** If true convolution kernels are separable. */
    protected boolean separable = false;

    /** Mask indicating which voxels to process. If true process voxel else skip. */
    protected BitSet mask = null;

    /**
     * Should be set to true if NOT in a single thread - will NOT force a graphics update of the progress bar. Should be
     * set to false if in a single thread - will force a graphics update of the progress bar. This defaults to false,
     * since we can automatically set it to true when startMethod() is called. When starting the algorithm with run(),
     * this boolean should remain false.
     *
     * @see  #startMethod(int)
     * @see  #run()
     */
    protected boolean runningInSeparateThread = false;

    /** Source image. */
    protected ModelImage srcImage;
    
    /** Flag indicating whether or not the thread is stopped. */
    protected volatile boolean threadStopped = false;

    /** This flag will be set to true when the algorithm has completed. */
    private boolean completed = false;

    /** Elapsed time (in milliseconds) -- time it took for algorithm to run. */
    private double elapsedTime = 0;

    /** A list of the ChangeListeners which are interested in the ChangeEvent. */
    private EventListenerList listenerList = new EventListenerList();

    /** Used to store the maximum value of the progress bar. */
    protected int maxProgressValue = 100;

    /** Used to store the minimum value of the progress bar. */
    protected int minProgressValue = 0;

    /**
     * Vector list of AlgorithmInterface objects. When the algorithm has been stopped or completed, all listeners in
     * this list are notified.
     */
    private Vector<AlgorithmInterface> objectList = new Vector<AlgorithmInterface>();

    /** Start time (in milliseconds) to be used to compute elapsed time. */
    private long startTime = 0;

    /**
     * Store the progress being made.
     */
    protected float progress;
    
    protected float progressStep;
    
    protected int progressModulus;
    
    /** Indicates if multi-threading will be used to optimize the algorithm. */
    protected boolean multiThreadingEnabled;
    
    /**
     * The number of threads will be used to execute the algorithm if multi-threading is enabled 
     */
    protected int nthreads;
    
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
        nthreads = ThreadUtil.getAvailableCores();
        multiThreadingEnabled = Preferences.isMultiThreadingEnabled();
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
        nthreads = ThreadUtil.getAvailableCores();
        multiThreadingEnabled = Preferences.isMultiThreadingEnabled();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Actually runs the algorithm. Implemented by inheriting algorithms.
     */
    public abstract void runAlgorithm();

    /**
     * Routine to catch action events
     *
     * @param  e  ActionEvent
     */
    public void actionPerformed(ActionEvent e) {

        // System.err.println("action performed!");
        if (e.getActionCommand().equalsIgnoreCase("cancel")) {
            setThreadStopped(true);
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
     * Adds the ProgressChangeListener to this FileBase object.
     *
     * @param  l  the ProgressChangeListener object
     */
    public void addProgressChangeListener(ProgressChangeListener l) {
        listenerList.add(ProgressChangeListener.class, l);

        if (l instanceof ViewJProgressBar) {
            ((ViewJProgressBar) l).addActionListener(this);
        }
    }

    /**
     * Working ...
     * This function is never used. It doesn't modify any parameters or data members and returns void.
     * @param  image    DOCUMENT ME!
     * @param  doColor  DOCUMENT ME!
     */
    public void calculatePrincipleAxis(ModelImage image, boolean doColor) {
        int x, y, z;
        int n = 0;
        Matrix3f mat2 = new Matrix3f(); // Row,Col
        Matrix3f meanProduct = new Matrix3f();
        Vector3f mean = new Vector3f(); // Column vector
        double voxVal = 0;
        double total = 0;
        double tot = 0;

        // Moments first and second order
        double mX = 0, mY = 0, mZ = 0, mXX = 0, mXY = 0, mXZ = 0, mYY = 0, mYZ = 0, mZZ = 0;

        float min = (float) image.getMin();

        int xEnd = image.getExtents()[0];
        int yEnd = image.getExtents()[1];
        int zEnd = image.getExtents()[2];

        int nLim = (int) Math.sqrt((double) xEnd * yEnd * zEnd);

        if (nLim < 1000) {
            nLim = 1000;
        }

        for (z = 0; z < zEnd; z++) {

            for (y = 0; y < yEnd; y++) {

                for (x = 0; x < xEnd; x++) {

                    if (doColor) {
                        voxVal = (double) (image.getFloatC(x, y, z, 1) + image.getFloatC(x, y, z, 2) +
                                           image.getFloatC(x, y, z, 3));
                    } else {
                        voxVal = (double) (image.getFloat(x, y, z) - min);
                    }

                    mX += voxVal * x;
                    mY += voxVal * y;
                    mZ += voxVal * z;
                    mXX += voxVal * x * x;
                    mXY += voxVal * x * y;
                    mXZ += voxVal * x * z;
                    mYY += voxVal * y * y;
                    mYZ += voxVal * y * z;
                    mZZ += voxVal * z * z;
                    tot += voxVal;
                    n++;

                    if (n > nLim) { // Lets not over run the buffers during summation
                        n = 0;
                        total += tot;
                        mat2.M00 = (float)(mat2.M00 + mXX);
                        mat2.M01 = (float)(mat2.M01 + mXY);
                        mat2.M02 = (float)(mat2.M02 + mXZ);
                        mat2.M11 = (float)(mat2.M11 + mYY);
                        mat2.M12 = (float)(mat2.M12 + mYZ);
                        mat2.M22 = (float)(mat2.M22 + mZZ);
                        mean.X = (float)(mean.X + mX);
                        mean.Y = (float)(mean.Y + mY);
                        mean.Z = (float)(mean.Z + mZ);
                        tot = 0;
                        mX = 0;
                        mY = 0;
                        mZ = 0;
                        mXX = 0;
                        mXY = 0;
                        mXZ = 0;
                        mYY = 0;
                        mYZ = 0;
                        mZZ = 0;
                    }
                }
            }
        }

        total += tot;

        if (Math.abs(total) < 1e-5) {
            total = 1.0f;
        }

        mat2.M00 = (float)((mat2.M00 + mXX) / total);
        mat2.M01 = (float)((mat2.M01 + mXY) / total);
        mat2.M02 = (float)((mat2.M02 + mXZ) / total);
        mat2.M11 = (float)((mat2.M11 + mYY) / total);
        mat2.M12 = (float)((mat2.M12 + mYZ) / total);
        mat2.M22 = (float)((mat2.M22 + mZZ) / total);
        mean.X = (float)((mean.X + mX) / total);
        mean.Y = (float)((mean.Y + mY) / total);
        mean.Z = (float)((mean.Z + mZ) / total);

        // Now make it central (taking off the Center of Mass)
        meanProduct.MakeTensorProduct(mean, mean);
        mat2.M00 -= meanProduct.M00;
        mat2.M01 -= meanProduct.M01;
        mat2.M02 -= meanProduct.M02;
        mat2.M10 -= meanProduct.M10;
        mat2.M11 -= meanProduct.M11;
        mat2.M12 -= meanProduct.M12;
        mat2.M20 -= meanProduct.M20;
        mat2.M21 -= meanProduct.M21;
        mat2.M22 -= meanProduct.M22;
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
        gov.nih.mipav.view.MipavUtil.displayError(error);
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


        if (destImage != null) {
            destImage.releaseLock();
        }

        if (srcImage != null) {
            srcImage.releaseLock();
        }
    }

    /**
     * Calls garbage collector to release system resources.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        mask = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /**
     * Helper function to determine which values to pass on to linked algorithms (that will fire progress changes to the
     * same progress bar). You pass in the current progress and then the max desired progress.... you do NOT adjust the
     * progress yourself, you assume a 0-100 range always, so if the current progress of a certain algorithm was 50%,
     * and you want the helper algorithm to run on an adjusted scale of 50% to 70%, then you pass 50 and 70 as current &
     * desiredMax respectively. if the algorithm passing this in was on a scale of 0->50, then the displayed progress
     * will range from 25% to 35% (50% of 50->70)
     *
     * @param   currentProgress     current progress of algorithm on a 0->100 scale
     * @param   desiredMaxProgress  max progress desired for linked algorithm
     *
     * @return  array of 2 ints [current, max]
     */
    public int[] generateProgressValues(int currentProgress, int desiredMaxProgress) {
        return new int[] {
                   ViewJProgressBar.getProgressFromInt(minProgressValue, maxProgressValue, currentProgress),
                   ViewJProgressBar.getProgressFromInt(minProgressValue, maxProgressValue, desiredMaxProgress)
               };
    }


    /**
     * Accessor to return the destination image
     *
     * @return  ModelImage destination image
     */
    public ModelImage getDestImage() {
        return destImage;
    }

    /**
     * Returns the elapsed time in seconds. This does not compute a new elapsed time.
     *
     * @return  the elapsed time, in seconds computed earlier
     *
     * @see     #computeElapsedTime()
     */
    public double getElapsedTime() {

        if (elapsedTime == 0) {
            computeElapsedTime();
        }

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
     * Returns the max progress value.
     *
     * @return  max progress value
     */
    public int getMaxProgressValue() {
        return maxProgressValue;
    }

    /**
     * Returns the min progress value.
     *
     * @return  min progress value
     */
    public int getMinProgressValue() {
        return minProgressValue;
    }

    /**
     * If there is a progress bar that is listening to the algorithm's progress change events, this will retrieve that
     * progress bar. This function is really only here to support AlgorithmExtractSurface, AlgorithmExtractSurfaceCubes,
     * and AlgorithmHeightFunction which use ModelQuadMesh and ModelTriangle Mesh (which do not follow the standard
     * algorithmbase setup). This function should not be used elsewhere and will probably be removed (and
     * modelquad/trianglemesh will be changed).
     *
     * @return  a progressbar if there is one listening, null otherwise.
     */
    public ViewJProgressBar getProgressChangeListener() {
        ProgressChangeListener[] pListeners = getProgressChangeListeners();
        ViewJProgressBar pBar = null;

        for (int p = 0; p < pListeners.length; p++) {

            if (pListeners[p] instanceof ViewJProgressBar) {
                pBar = (ViewJProgressBar) pListeners[p];
            }
        }

        return pBar;
    }

    /**
     * Returns the progress change event listener list.
     *
     * @return  the progress change event listener list.
     */
    public ProgressChangeListener[] getProgressChangeListeners() {
        return (ProgressChangeListener[]) listenerList.getListeners(ProgressChangeListener.class);
    }

    /**
     * Gets the current stored min and max progress values.
     *
     * @return  int [] { currentMin, currentMax}
     */
    public int[] getProgressValues() {
        return new int[] { minProgressValue, maxProgressValue };
    }
    
    /**
     * Gets the current source image for the algorithm
     * 
     * @return the source image
     */
    public ModelImage getSrcImage() {
        return srcImage;
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
     * Should be set to true if NOT in a single thread (i.e., the main gui and this algo are in different threads) -
     * will NOT force a graphics update of the progress bar. Should be set to false if in a single thread - will force a
     * graphics update of the progress bar.
     *
     * @return  boolean true if this algorithm is in a different thread from the main mipav gui thread.
     */
    public boolean isRunningInSeparateThread() {
        return this.runningInSeparateThread;
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
     * Removes the ChangeListener from the FileBase object.
     *
     * @param  l  the ProgressChangeListener object
     */
    public void removeProgressChangeListener(ProgressChangeListener l) {
        listenerList.remove(ProgressChangeListener.class, l);

        if (l instanceof ViewJProgressBar) {
            ((ViewJProgressBar) l).removeActionListener(this);
        }
    }

    /**
     * Performs start-up and tear-down operations that should be done by all algorithms (timing, history log). Since
     * this class extends the Thread class it can be run in its own thread by invoking object.start(); It can also be
     * invoked without a new thread by calling the the run() method directly (ie. object.run()).
     */
    public void run() {
        setStartTime();
        runAlgorithm();

        if(threadStopped){
        	return;
        }
        double timeinSec = computeElapsedTime();
        Preferences.debug( "Elapsed time: " + timeinSec + "\n", Preferences.DEBUG_ALGORITHM);
        notifyListeners(this);

        /// finalize();
    }


    /**
     * Sets completed to flag indicating if algorithm has sucessfully completed.
     *
     * @param  flag  Flag to set to <code>true</code> if the algorithm has completed.
     */
    public void setCompleted(boolean flag) {
        completed = flag;

        if (maxProgressValue == 100) {
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
        }
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
     * Sets the mask (BitSet object).
     *
     * @param  imageMask  BitSet object indicating which voxels to process
     */
    public void setMask(BitSet imageMask) {
        mask = imageMask;
    }

    /**
     * Sets the max progress value.
     *
     * @param  maxProgressValue  the maximum progress value
     */
    public void setMaxProgressValue(int maxProgressValue) {
        this.maxProgressValue = maxProgressValue;
    }

    /**
     * Sets the min progress value.
     *
     * @param  minProgressValue  the minimum progress value
     */
    public void setMinProgressValue(int minProgressValue) {
        this.minProgressValue = minProgressValue;
    }

    /**
     * Sets the min and max progress values from an array of two ints.
     *
     * @param  minmax  array of two ints [min, max]
     */
    public void setProgressValues(int[] minmax) {
        this.minProgressValue = minmax[0];
        this.maxProgressValue = minmax[1];
    }

    /**
     * Sets both the min and max progress values.
     *
     * @param  min  the min progress value
     * @param  max  the max progress value
     */
    public void setProgressValues(int min, int max) {
        this.minProgressValue = min;
        this.maxProgressValue = max;
    }

    /**
     * Sets the running in separate thread flag which controls how the progress bar is updated. If true the progress bar
     * is not forced to update.
     *
     * @param  separateThread  Should be set to true if NOT in a single thread (i.e., the main gui and this algo are in
     *                         different threads) - will NOT force a graphics update of the progress bar. Should be set
     *                         to false if in a single thread - will force a graphics update of the progress bar.
     */
    public void setRunningInSeparateThread(boolean separateThread) {
        this.runningInSeparateThread = separateThread;
    }
    
    /**
     * Sets the source image of the algorithm
     * 
     * @param srcImage the source image
     */
    public void setSrcImage(ModelImage srcImage) {
        this.srcImage = srcImage;
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

            setRunningInSeparateThread(true);
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

        setCompleted(false);
        threadStopped = true;
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
     * Notifies listeners that have registered interest for notification on this event type.
     *
     * @param  message  the new message to display on the progress bar
     */
    protected void fireProgressStateChanged(String message) {
        fireProgressStateChanged(ViewJProgressBar.PROGRESS_VALUE_UNCHANGED, null, message);
    }

    /**
     * Notifies all listeners that have registered interest for notification on this event type.
     *
     * @param  value  the value of the progress bar.
     */
    protected void fireProgressStateChanged(int value) {
        fireProgressStateChanged(value, null, null);
    }

    /**
     * Updates listeners of progress status. Without actually changing the numerical value
     *
     * @param  imageName  the name of the image
     * @param  message    the new message to display
     */
    protected void fireProgressStateChanged(String imageName, String message) {
        fireProgressStateChanged(ViewJProgressBar.PROGRESS_VALUE_UNCHANGED, imageName, message);
    }

    /**
     * Notifies all listeners that have registered interest for notification on this event type.
     *
     * @param  value    the value of the progress bar.
     * @param  title    the title of the progress dialog.
     * @param  message  the message for that specific progress value.
     */
    protected void fireProgressStateChanged(int value, String title, String message) {

        Object[] listeners = listenerList.getListenerList();
        if (listeners == null || listeners.length < 2) {
            if(title != null) {
                System.out.print(title+": ");
            }
            if(message != null) {
                System.out.println(message);
            }
            return;
        }

        // adjust value based on minProgress and maxProgress
        if ((value != ViewJProgressBar.PROGRESS_VALUE_UNCHANGED) &&
                (value != ViewJProgressBar.PROGRESS_WINDOW_CLOSING)) {
            value = ViewJProgressBar.getProgressFromInt(minProgressValue, maxProgressValue, value);
        }

        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == ProgressChangeListener.class) {
                ProgressChangeEvent event = new ProgressChangeEvent(this, value, title, message);
                ((ProgressChangeListener) listeners[i + 1]).progressStateChanged(event);
            }
        }
    }

    /**
     * Updates listeners of progress status.
     *
     * @param  fVal     the percent progress of the algorithm so far
     * @param  title    the title to send to the listeners
     * @param  message  the message to send to the listeners
     */
    protected void fireProgressStateChanged(float fVal, String title, String message) {
        fireProgressStateChanged(ViewJProgressBar.getProgressFromFloat(minProgressValue, maxProgressValue, fVal), title,
                                 message);
    }

    /**
     * Helper function to link the currently listening progress bar with an algorithm created within an algorithm. This
     * function should only be called when the other algorithm is expected to update the progress bar seamlessly. It
     * should be called in conjuction with AlgorithmBase's setProgressValues() so that the sub-algorithm will have know
     * the min and max progress values.
     *
     * <p>Generally this will be called with the following lines: linkProgressToAlgorithm(theSubAlgorithm);
     * theSubAlgorithm.setProgressValues(generateProgressValues(currentProgressOfAlgorithm,
     * desiredMaxProgressOfSubAlgorithm));</p>
     *
     * @param  baseAlgo  the subalgorithm created within current algorithm
     */
    protected void linkProgressToAlgorithm(AlgorithmBase baseAlgo) {
        ProgressChangeListener[] listeners = this.getProgressChangeListeners();

        if (listeners != null) {

            for (int i = 0; i < listeners.length; i++) {
                baseAlgo.addProgressChangeListener(listeners[i]);
            }
        }
    }

    /**
     * Disconnect this algorithm with the progress bar which holds a reference
     * to this algorithm. In order to release the memory of this algorithm,
     * this function should be called after the algorithm was finished.
     * @param baseAlgo
     */
    protected void delinkProgressToAlgorithm(AlgorithmBase baseAlgo){
        ProgressChangeListener[] listeners = this.getProgressChangeListeners();

        if (listeners != null) {

            for (int i = 0; i < listeners.length; i++) {
                baseAlgo.removeProgressChangeListener(listeners[i]);
            }
        }
    }

	public synchronized float getProgress() {
		return progress;
	}


	public synchronized void setProgress(int progress) {
		this.progress = progress;
	}
	
	public synchronized void makeProgress(float step){
		this.progress += step;
	}
	
	public float getProgressStep(){
		return this.progressStep;
	}
	public void setProgressStep(float progressStep){
		this.progressStep = progressStep;
	}


    public boolean isMultiThreadingEnabled() {
        return multiThreadingEnabled;
    }


    public void setMultiThreadingEnabled(boolean multiThreadingEnabled) {
        this.multiThreadingEnabled = multiThreadingEnabled;
    }


    public int getNumberOfThreads() {
        return nthreads;
    }


    public void setNumberOfThreads(int nthreads) {
        this.nthreads = nthreads;
    }


    public int getProgressModulus() {
        return progressModulus;
    }


    public void setProgressModulus(int progressModulus) {
        this.progressModulus = progressModulus;
    }


    /**
     * Calculate the size of the image.
     * 
     * @param dims
     * @return
     */
    public static int calculateImageSize(final int[] dims) {
        int size = 1;
        for (final int element : dims) {
            size *= element;
        }
        return size;
    }
}
