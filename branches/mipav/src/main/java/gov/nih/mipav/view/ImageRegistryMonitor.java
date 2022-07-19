package gov.nih.mipav.view;


import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Tracks registered images, and notifies anybody who is interested in finding out about them.
 */
public class ImageRegistryMonitor implements Runnable, FocusListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ChangeEvent changeEvent;

    /** DOCUMENT ME! */
    private EventListenerList listenerList;

    /** DOCUMENT ME! */
    private Vector<String> registeredNames = new Vector<String>();

    /** DOCUMENT ME! */
    private long sleepAmount = 5000;

    /** DOCUMENT ME! */
    private volatile Thread thread;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor. Creates the list for listeners
     */
    public ImageRegistryMonitor() {
        listenerList = new EventListenerList();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * add a memory change listener.
     *
     * @param  l  DOCUMENT ME!
     */
    public void addImageRegistryChangeListener(ChangeListener l) {
        (listenerList).add(ChangeListener.class, l);
    }

    /**
     * focus gained.
     *
     * @param  fe  DOCUMENT ME!
     */
    public void focusGained(FocusEvent fe) { }

    /**
     * when focus is lost the source is assumed to be a JTextField, and it sets the sample period.
     *
     * @param  fe  DOCUMENT ME!
     */
    public void focusLost(FocusEvent fe) {
        Object source = fe.getSource();

        if (!fe.isTemporary()) {

            if (source instanceof JTextField) {
                sleepAmount = Long.parseLong(((JTextField) source).getText());
            }
        }
    }

    /**
     * The name of images currently registered.
     *
     * @return  Vector of image names
     */
    public Vector<String> getRegisteredNames() {
        return (Vector<String>) (registeredNames);
    }

    /**
     * removes the change listener.
     *
     * @param  l  DOCUMENT ME!
     */
    public void removeImageRegistryChangeListener(ChangeListener l) {
        (listenerList).remove(ChangeListener.class, l);
    }

    /**
     * when the thread wakes up, it collects information from the runtime on current memory status. It then notifies all
     * listeners.
     */
    public void run() {
        Thread me = Thread.currentThread();

        while (thread == me) {
            registeredNames.clear();

            Enumeration<String> e = ViewUserInterface.getReference().getRegisteredImageNames();

            while (e.hasMoreElements()) {
                registeredNames.addElement(e.nextElement());
            }

            fireImageRegistryChanged(); // too many elements and we waste
                                        // time here...and be thread-selfish

            try {
                Thread.sleep(sleepAmount);
            } catch (InterruptedException ie) {
                break;
            }
        }

        thread = null;
    }

    /**
     * Start the thread as a minimum priority thread.
     */
    public void start() {
        thread = new Thread(this);
        thread.setPriority(Thread.MIN_PRIORITY);
        thread.setName("ImageRegistryMonitor");
        thread.start();
    }

    /**
     * kill the thread.
     */
    public void stop() {
        thread = null;
    }

    /**
     * Notify all listeners that have registered interest for notification on this event type. The event instance is
     * lazily created using the parameters passed into the fire method.
     */
    protected void fireImageRegistryChanged() {

        // Guaranteed to return a non-null array
        Object[] listeners = (listenerList).getListenerList();

        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == ChangeListener.class) {

                // Lazily create the event:
                if (changeEvent == null) {
                    changeEvent = new ChangeEvent(this);
                }

                ((ChangeListener) listeners[i + 1]).stateChanged(changeEvent);

            }
        }
    }

}
