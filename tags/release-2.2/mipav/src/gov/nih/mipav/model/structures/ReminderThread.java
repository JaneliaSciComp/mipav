package gov.nih.mipav.model.structures;

import java.lang.reflect.*;
import java.util.*;

public class ReminderThread extends Thread
{
    private boolean isRunning;
    private long sleeptime;
    protected Hashtable subscribersHashtable;

    public ReminderThread(long sleeptime)
    {
        subscribersHashtable = new Hashtable();
        this.sleeptime = sleeptime;
        isRunning = false;
    }

    public void addSubscriber(Object object, Method method)
    {
        subscribersHashtable.put(object, method);
    }

    public void removeSubscriber(Object object)
    {
        subscribersHashtable.remove(object);
    }

    public void run()
    {
        isRunning = true;

        try
        {
            while (isRunning)
            {
                Enumeration e = subscribersHashtable.keys();

                while (e.hasMoreElements())
                {
                    Object object = e.nextElement();

                    Object value = subscribersHashtable.get(object);

                    Method method = (Method) value;

                    try
                    {
                        method.invoke(object, null);
                    }
                    catch (Exception exception)
                    {
                        subscribersHashtable.remove(object);
                    }
                }

                sleep(sleeptime);
            }
        }
        catch (InterruptedException ie)
        {

        }
    }

    public void shutdown()
    {
        isRunning = false;
    }
}
