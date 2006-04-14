package gov.nih.mipav.model.structures;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.event.*;

import javax.swing.event.EventListenerList;

/** Class extends ViewVOIVector to finish cleaning up
*   access syntax to the volumes of interest vector.
*   <p>
*   In addition, it contains a listener, and permits
*   interested parties to request to be notified when
*   list of Volumes of Interest change.  That could be
*   when:
*   <ul>
*   <li>a VOI is added</li>
*   <li>a VOI is removed</li>
*   <li>et cetera</li>
*   </ul>
*   Those wishing to add more to notify on a VOI change
*   should add the appropriate fireVOI..method and the
*   appropriate corresponding method in <code>VOIVectorListener</code>
*   to enforce that listeners handle that change.
*   <p>
*   Notice none of the methods using java.util.Collection are
*   implemented here.
*		@version    Aug 2002
*		@author     David Parsons
*
*
*/

public class VOIVector extends ViewVOIVector {
    // holds all interfaces listening to this vector
    private EventListenerList listenerList;
    // an instnace of the VOI update event.
    private VOIVectorEvent voiUpdate;

    /** Constructs an empty vector so that
    *   its internal data array has size 10
    *   and its standard capacity increment
    *   is zero.
    *   <p>
    *   <i>Copied from the definition of
    *   <code>java.util.Vector</code></i>
    */
    public VOIVector() {
        super();
    }

    /** Constructs an empty vector with the
    *   specified initial capacity and with
    *   its capacity increment equal to zero.
    *   @param initialsize initial capacity of
    *           the vector
    *   @exception IllegalArgumentException -
    *           if the specified initial capacity
    *           is negative
    *   <p>
    *   <i>Copied from the definition of
    *   <code>java.util.Vector</code></i>
    */
    public VOIVector(int initialsize) {
        super(initialsize);
    }

    // --------------------Vector method over-rides:
    // Vector method over-rides ensure that :
    // *the only elements added are VOI, and that a notify
    //  takes place to all interested listeners.
    // *that outputs are all returning VOI as needed.
    //

    // note: none of the methods using java.util.Collection are
    // implemented here.

    /**
    *  Override the Vector method to ensure that
    *  object is a voi, and that the new voi's name is unique.
    *  @param i   index of the VOI
    *  @return    the VOI at the index
    *   @exception IllegalArgumentException for any argument <code>o</code>
    *               which is not an instance of
    *               <code>gov.nih.mipav.model.structures.VOI</code>
    */
    public void addElement(Object o) {
        add(o);   // add the voi to the vector
    }

    /**
    *  Override the Vector method to ensure that
    *  object is a voi, and that the new voi's name is unique.
    *  @param i   index of the VOI
    *  @return    the VOI at the index
    *  @exception IllegalArgumentException for any argument <code>o</code>
    *               which is not an instance of
    *               <code>gov.nih.mipav.model.structures.VOI</code>
    */
    public void insertElementAt(Object o, int index) {
        // check that object is a VOI
        if (!(o instanceof VOI)) {
            throw new IllegalArgumentException();
        }

        VOI voi = (VOI)o;
        // check the voi name, fix if necessary
        while (contains(voi)) {
            voi.setName(buildName(voi.getName()));
        }

        super.insertElementAt(voi, index);// add the voi to the vector
        fireVOIadded(voi);  // notify listeners of a change

        Preferences.debug ("Add voi: name = " + voi.getName() + "\n");
    }

    /**
    *  Override the Vector method to ensure that
    *  object is a voi, and that the new voi's name is unique.
    *  @param i   index of the VOI
    *  @return    the VOI at the index
    *   @exception IllegalArgumentException for any argument <code>o</code>
    *               which is not an instance of
    *               <code>gov.nih.mipav.model.structures.VOI</code>
    */
    public boolean add(Object o) {
        VOI voi = null;
        // check that object is a VOI
        if (!(o instanceof VOI))
            throw new IllegalArgumentException();

        voi = (VOI)o;
        // check the voi name, fix if necessary
        while (contains(voi)) {
            voi.setName(buildName(voi.getName()));
        }

        if (super.add(voi)) {// add the voi to the vector
            fireVOIadded(voi);  // notify listeners of a change
            Preferences.debug ("Add voi: name = " + voi.getName() + "\n");
            return true;
        }
        return false;
    }

    public boolean addVOI(VOI voi) {
        // check the voi name, fix if necessary
        while (contains(voi)) {
            voi.setName(buildName(voi.getName()));
        }

        if (super.add(voi)) {// add the voi to the vector
            fireVOIadded(voi);  // notify listeners of a change
            Preferences.debug ("Add voi: name = " + voi.getName() + "\n");
            return true;
        }
        return false;
    }


    /** <b>Overides</b> <code>Vector.remove()</code>
    *   <p>
    *   successful removal of the element results
    *   in the VOI listeners being notified.
    */
    public void removeElementAt(int index) {
        try {
            VOI voi = VOIAt(index);
            super.removeElementAt(index);
            fireVOIremoved(voi);
        }
        catch (ArrayIndexOutOfBoundsException badIndex) {
            throw badIndex;
        }
    }

    public void removeAllElements() {
        super.removeAllElements();
        //fireVOIremoved(null);
    }

    /** <b>Overides</b> <code>Vector.remove()</code>
    *   <p>
    *   successful removal of the element results
    *   in the VOI listeners being notified.
    */
    public Object remove(int index) {
        try {
            VOI voi = (VOI) super.remove(index);
            fireVOIremoved(voi);
            return voi;
        }
        catch (ArrayIndexOutOfBoundsException badIndex) {
            throw badIndex;
        }
    }

    /** <b>Overides</b> <code>Vector.removeRange()</code>
    *   <p>
    *   successfull removal of the range of elements results
    *   in the VOI listeners being notified.
    */
    public void removeRange(int first, int last) {
        try {
            super.removeRange(first, last);
            fireVOIremoved(null);
        }
        catch (ArrayIndexOutOfBoundsException badIndex) {
            throw badIndex;
        }
    }

	// --------- Event-handling routines:
	// to add this object to send out events for listening
	// objects, at least the following 3 methods must be
	// present: addListener, removeListener, fireEvent as
	// present below.
	/** adds the update listener */
    public void addVectorListener(VOIVectorListener listener) {
        if (listenerList == null) {
            listenerList = new EventListenerList();
        }
        if (listener == null) {
            throw new IllegalArgumentException("listener is null");
        }
        listenerList.add(VOIVectorListener.class, listener);
    }

	/** removes the update listener */
    public void removeVectorListener(VOIVectorListener listener) {
        if (listenerList == null) {
            listenerList = new EventListenerList();
        }
        if (listener == null) {
            throw new IllegalArgumentException("listener is null");
        }
        listenerList.remove(VOIVectorListener.class, listener);
    }

    // Notify all listeners that have registered interest for
    // notification on this event type.  The event instance
    // is lazily created using the parameters passed into
    // the fire method.


    /** Fires a VOI event based on the VOI.  calls the listener's
    *   <code>addedVOI()</code> method.
    */
    protected void fireVOIadded(VOI voi) {
        try {
            // only if there are listeners to send events to should we
            // bother with creating an event and bothering the event queue.
            if (listenerList.getListenerCount(VOIVectorListener.class) == 0) {
                return;
            }
        }
        catch (NullPointerException npe) {
            listenerList = new EventListenerList();
        }

        // always create a new Event, since we need to carry
        // the changed VOI around.
        voiUpdate = new VOIVectorEvent(this, voi);
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==VOIVectorListener.class) {
                ((VOIVectorListener)listeners[i+1]).addedVOI(voiUpdate);
            }
        }
    }
    /** Fires a VOI event based on the VOI.  calls the listener's
    *   <code>removedVOI()</code> method.
    */
    protected void fireVOIremoved(VOI voi) {
        try {
            // only if there are listeners to send events to should we
            // bother with creating an event and bothering the event queue.
            if (listenerList.getListenerCount(VOIVectorListener.class) == 0) {
                return;
            }
        }
        catch (NullPointerException npe) {
            listenerList = new EventListenerList();
        }

        // always create a new Event, since we need to carry
        // the changed VOI around.
        voiUpdate = new VOIVectorEvent(this, voi);
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==VOIVectorListener.class) {
                ((VOIVectorListener)listeners[i+1]).removedVOI(voiUpdate);
            }
        }
    }
}
