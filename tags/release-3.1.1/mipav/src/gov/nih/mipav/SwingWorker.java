package gov.nih.mipav;


import javax.swing.*;


/**
 * This is the 3rd version of SwingWorker (also known as SwingWorker 3), an abstract class that you subclass to perform
 * GUI-related work in a dedicated thread. For instructions on and examples of using this class, see:
 *
 * <p>http://java.sun.com/docs/books/tutorial/uiswing/misc/threads.html</p>
 *
 * <p>Note that the API changed slightly in the 3rd version: You must now invoke start() on the SwingWorker after
 * creating it.</p>
 */
public abstract class SwingWorker {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** An object used to manage the current worker thread reference. */
    private ThreadVar threadVar;

    /**
     * The value produced by the worker thread.
     *
     * @see  #getValue()
     * @see  #setValue(Object)
     */
    private Object value; // see getValue(), setValue()

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Start a thread that will call the <code>construct</code> method and then exit.
     */
    public SwingWorker() {
        final Runnable doFinished = new Runnable() {
            public void run() {
                finished();
            }
        };

        Runnable doConstruct = new Runnable() {
            public void run() {

                try {
                    setValue(construct());
                } finally {
                    threadVar.clear();
                }

                SwingUtilities.invokeLater(doFinished);
            }
        };

        Thread t = new Thread(doConstruct);
        threadVar = new ThreadVar(t);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Compute the value to be returned by the <code>get</code> method.
     *
     * @return  the value produced by this worker thread
     */
    public abstract Object construct();

    /**
     * Called on the event dispatching thread (not on the worker thread) after the <code>construct</code> method has
     * returned.
     */
    public void finished() { }

    /**
     * Return the value created by the <code>construct</code> method. Returns null if either the constructing thread or
     * the current thread was interrupted before a value was produced.
     *
     * @return  the value created by the <code>construct</code> method
     */
    public Object get() {

        while (true) {
            Thread t = threadVar.get();

            if (t == null) {
                return getValue();
            }

            try {
                t.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt(); // propagate

                return null;
            }
        }
    }

    /**
     * A new method that interrupts the worker thread. Call this method to force the worker to stop what it's doing.
     */
    public void interrupt() {
        Thread t = threadVar.get();

        if (t != null) {
            t.interrupt();
        }

        threadVar.clear();
    }

    /**
     * Start the worker thread.
     */
    public void start() {
        Thread t = threadVar.get();

        if (t != null) {
            t.start();
        }
    }

    /**
     * Get the value produced by the worker thread, or null if it hasn't been constructed yet.
     *
     * @return  the value produced by the worker thread
     */
    protected synchronized Object getValue() {
        return value;
    }

    /**
     * Set the value produced by worker thread.
     *
     * @param  x  the value which should be produced by this worker thread
     */
    private synchronized void setValue(Object x) {
        value = x;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Class to maintain reference to current worker thread under separate synchronization control.
     */
    private static class ThreadVar {

        /** The reference to the current worker thread. */
        private Thread thread;

        /**
         * Constructs the class to maintain the current worker thread reference.
         *
         * @param  t  DOCUMENT ME!
         */
        public ThreadVar(Thread t) {
            thread = t;
        }

        /**
         * Resets the current worker thread reference to <code>null</code>.
         */
        public synchronized void clear() {
            thread = null;
        }

        /**
         * Get the current worker thread reference.
         *
         * @return  the reference of the current thread
         */
        public synchronized Thread get() {
            return thread;
        }
    }
}
