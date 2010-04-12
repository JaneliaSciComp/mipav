package gov.nih.mipav;


import javax.swing.SwingUtilities;


/**
 * This is the 3rd version of SwingWorker (also known as SwingWorker 3), an abstract class that you subclass to perform
 * GUI-related work in a dedicated thread. For instructions on and examples of using this class, see:
 * 
 * <p>
 * http://java.sun.com/docs/books/tutorial/uiswing/misc/threads.html
 * </p>
 * 
 * <p>
 * Note that the API changed slightly in the 3rd version: You must now invoke start() on the SwingWorker after creating
 * it.
 * </p>
 * 
 * <hr>
 * From the SDN, where version 3 of SwingWorker was downloaded:
 * 
 * <blockquote>
 * <p>
 * Code Sample License
 * </p>
 * 
 * <p>
 * Copyright 1994-2009 Sun Microsystems, Inc. All Rights Reserved.
 * </p>
 * 
 * <p>
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 * </p>
 * 
 * <ul>
 * <li> Redistribution of source code must retain the above copyright notice, this list of conditions and the following
 * disclaimer. </li>
 * <li> Redistribution in binary form must reproduce the above copyright notice, this list of conditions and the
 * following disclaimer in the documentation and/or other materials provided with the distribution. </li>
 * </ul>
 * 
 * <p>
 * Neither the name of Sun Microsystems, Inc. or the names of contributors may be used to endorse or promote products
 * derived from this software without specific prior written permission.
 * </p>
 * 
 * <p>
 * This software is provided &quot;AS IS,&quot; without a warranty of any kind. ALL EXPRESS OR IMPLIED CONDITIONS,
 * REPRESENTATIONS AND WARRANTIES, INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE
 * OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN MICROSYSTEMS, INC. (&quot;SUN&quot;) AND ITS LICENSORS SHALL NOT BE
 * LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING THIS SOFTWARE OR ITS
 * DERIVATIVES. IN NO EVENT WILL SUN OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT,
 * INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF
 * LIABILITY, ARISING OUT OF THE USE OF OR INABILITY TO USE THIS SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGES.
 * </p>
 * 
 * <p>
 * You acknowledge that this software is not designed, licensed or intended for use in the design, construction,
 * operation or maintenance of any nuclear facility.
 * </p>
 * </blockquote>
 */
public abstract class SwingWorker {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** An object used to manage the current worker thread reference. */
    private final ThreadVar threadVar;

    /**
     * The value produced by the worker thread.
     * 
     * @see #getValue()
     * @see #setValue(Object)
     */
    private Object value; // see getValue(), setValue()

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Start a thread that will call the <code>construct</code> method and then exit.
     */
    public SwingWorker() {
        final Runnable doFinished = new Runnable() {
            public void run() {
                finished();
            }
        };

        final Runnable doConstruct = new Runnable() {
            public void run() {

                try {
                    setValue(construct());
                } finally {
                    threadVar.clear();
                }

                SwingUtilities.invokeLater(doFinished);
            }
        };

        final Thread t = new Thread(doConstruct);
        threadVar = new ThreadVar(t);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Compute the value to be returned by the <code>get</code> method.
     * 
     * @return the value produced by this worker thread
     */
    public abstract Object construct();

    /**
     * Called on the event dispatching thread (not on the worker thread) after the <code>construct</code> method has
     * returned.
     */
    public void finished() {}

    /**
     * Return the value created by the <code>construct</code> method. Returns null if either the constructing thread
     * or the current thread was interrupted before a value was produced.
     * 
     * @return the value created by the <code>construct</code> method
     */
    public Object get() {

        while (true) {
            final Thread t = threadVar.get();

            if (t == null) {
                return getValue();
            }

            try {
                t.join();
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt(); // propagate

                return null;
            }
        }
    }

    /**
     * A new method that interrupts the worker thread. Call this method to force the worker to stop what it's doing.
     */
    public void interrupt() {
        final Thread t = threadVar.get();

        if (t != null) {
            t.interrupt();
        }

        threadVar.clear();
    }

    /**
     * Start the worker thread.
     */
    public void start() {
        final Thread t = threadVar.get();

        if (t != null) {
            t.start();
        }
    }

    /**
     * Get the value produced by the worker thread, or null if it hasn't been constructed yet.
     * 
     * @return the value produced by the worker thread
     */
    protected synchronized Object getValue() {
        return value;
    }

    /**
     * Set the value produced by worker thread.
     * 
     * @param x the value which should be produced by this worker thread
     */
    private synchronized void setValue(final Object x) {
        value = x;
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Class to maintain reference to current worker thread under separate synchronization control.
     */
    private static class ThreadVar {

        /** The reference to the current worker thread. */
        private Thread thread;

        /**
         * Constructs the class to maintain the current worker thread reference.
         * 
         * @param t DOCUMENT ME!
         */
        public ThreadVar(final Thread t) {
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
         * @return the reference of the current thread
         */
        public synchronized Thread get() {
            return thread;
        }
    }
}
