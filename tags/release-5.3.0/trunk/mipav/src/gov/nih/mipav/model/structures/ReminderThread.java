package gov.nih.mipav.model.structures;


import java.lang.reflect.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class ReminderThread extends Thread {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected Hashtable<Object,Method> subscribersHashtable;

    /** DOCUMENT ME! */
    private boolean isRunning;

    /** DOCUMENT ME! */
    private long sleeptime;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ReminderThread object.
     *
     * @param  sleeptime  DOCUMENT ME!
     */
    public ReminderThread(long sleeptime) {
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

            while (isRunning) {
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

                sleep(sleeptime);
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
