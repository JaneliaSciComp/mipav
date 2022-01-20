package gov.nih.mipav.model.structures;


import java.lang.reflect.*;

import java.util.*;


/**
 * Thread which sleeps for a pre-determined amount of time, then calls a method (if the thread hasn't had its shutdown
 * method called). This class differs from ReminderThread because it sleeps before invoking the subscriber's callback
 * method and because it will only invoke each subscriber's callback method once.
 */
public class TimeoutThread extends Thread {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected Hashtable<Object,Method> subscribersHashtable;

    /** DOCUMENT ME! */
    private volatile boolean isRunning;

    /** DOCUMENT ME! */
    private long sleeptime;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ReminderThread object.
     *
     * @param  sleeptime  DOCUMENT ME!
     */
    public TimeoutThread(long sleeptime) {
        subscribersHashtable = new Hashtable<Object,Method>();
        this.sleeptime = sleeptime;
        isRunning = false;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  object  DOCUMENT ME!
     * @param  method  DOCUMENT ME!
     */
    public void addSubscriber(Object object, Method method) {
        subscribersHashtable.put(object, method);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  object  DOCUMENT ME!
     */
    public void removeSubscriber(Object object) {
        subscribersHashtable.remove(object);
    }

    /**
     * DOCUMENT ME!
     */
    public void run() {
        isRunning = true;

        try {
            sleep(sleeptime);

            if (isRunning) {
                Enumeration<Object> e = subscribersHashtable.keys();

                while (e.hasMoreElements()) {
                    Object object = e.nextElement();

                    Object value = subscribersHashtable.get(object);

                    Method method = (Method) value;

                    try {
                        method.invoke(object, (Object[]) null);
                    } catch (Exception exception) {
                        subscribersHashtable.remove(object);
                    }
                }


            }
        } catch (InterruptedException ie) { }
    }

    /**
     * DOCUMENT ME!
     */
    public void shutdown() {
        isRunning = false;
    }
}
