package gov.nih.mipav.model.structures;


import gov.nih.mipav.model.structures.event.VOIVectorEvent;
import gov.nih.mipav.model.structures.event.VOIVectorListener;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewVOIVector;

import javax.swing.event.EventListenerList;


/**
 * Class extends ViewVOIVector to finish cleaning up access syntax to the volumes of interest vector.
 *
 * <p>In addition, it contains a listener, and permits interested parties to request to be notified when list of Volumes
 * of Interest change. That could be when:</p>
 *
 * <ul>
 *   <li>a VOI is added</li>
 *   <li>a VOI is removed</li>
 *   <li>et cetera</li>
 * </ul>
 *
 * Those wishing to add more to notify on a VOI change should add the appropriate fireVOI..method and the appropriate
 * corresponding method in <code>VOIVectorListener</code> to enforce that listeners handle that change.
 *
 * <p>Notice none of the methods using java.util.Collection are implemented here.</p>
 *
 * @version  Aug 2002
 * @author   David Parsons
 */

public class VOIVector extends ViewVOIVector {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7391734288216133947L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** holds all interfaces listening to this vector. */
    private EventListenerList listenerList;

    /** an instnace of the VOI update event. */
    private VOIVectorEvent voiUpdate;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs an empty vector so that its internal data array has size 10 and its standard capacity increment is
     * zero.
     *
     * <p><i>Copied from the definition of <code>java.util.Vector</code></i></p>
     */
    public VOIVector() {
        super();
    }

    /**
     * Constructs an empty vector with the specified initial capacity and with its capacity increment equal to zero.
     *
     * @param  initialsize  initial capacity of the vector
     */
    public VOIVector(int initialsize) {
        super(initialsize);
    }
    
    public VOIVector(VOIVector voiVector)
    {
        super();
        int size = voiVector.size();
        for ( int i = 0; i < size; i++ )
        {
            add ( new VOI( voiVector.get(i) ) );
        }
        if ( voiVector.listenerList != null) {
            voiVector.listenerList.getListenerCount(VOIVectorListener.class);
            VOIVectorListener [] voiList = voiVector.listenerList.getListeners(VOIVectorListener.class);
            for (int i = 0; i < voiList.length; i++) {
                this.addVectorListener(voiList[i]);
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Override the Vector method to ensure that object is a voi, and that the new voi's name is unique.
     *
     * @param      o  index of the VOI
     *
     * @return     the VOI at the index
     *
     * @exception  IllegalArgumentException  for any argument <code>o</code> which is not an instance of <code>
     *                                       gov.nih.mipav.model.structures.VOI</code>
     */
    @Override
	public boolean add(VOI o) {
        VOI voi = null;

        // check that object is a VOI
        if (!(o instanceof VOI)) {
            throw new IllegalArgumentException();
        }

        voi = o;

        // check the voi name, fix if necessary
        if (contains(voi)) {
            voi.setName(buildName(voi.getName()));
        }

        if (super.add(voi)) { // add the voi to the vector
            fireVOIadded(voi); // notify listeners of a change
            Preferences.debug("Add voi: name = " + voi.getName() + "\n");

            return true;
        }

        return false;
    }

    // --------------------Vector method over-rides:
    // Vector method over-rides ensure that :
    // *the only elements added are VOI, and that a notify
    // takes place to all interested listeners.
    // *that outputs are all returning VOI as needed.
    //

    // note: none of the methods using java.util.Collection are
    // implemented here.

    /**
     * Override the Vector method to ensure that object is a voi, and that the new voi's name is unique.
     *
     * @param  o  index of the VOI
     */
    @Override
	public void addElement(VOI o) {
        add(o); // add the voi to the vector
    }

    // --------- Event-handling routines:
    // to add this object to send out events for listening
    // objects, at least the following 3 methods must be
    // present: addListener, removeListener, fireEvent as
    // present below.
    /**
     * adds the update listener.
     *
     * @param   listener  DOCUMENT ME!
     *
     * @throws  IllegalArgumentException  DOCUMENT ME!
     */
    public void addVectorListener(VOIVectorListener listener) {

        if (listenerList == null) {
            listenerList = new EventListenerList();
        }

        if (listener == null) {
            throw new IllegalArgumentException("listener is null");
        }
        
        
        if (listenerList.getListenerCount(VOIVectorListener.class) == 0)
        {
            listenerList.add(VOIVectorListener.class, listener);
            return;
        }        
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == VOIVectorListener.class && 
                    ((VOIVectorListener) listeners[i + 1]) == listener) {
                return;
            }
        }
        listenerList.add(VOIVectorListener.class, listener);
    }

    /**
     * Adds voi to the voi vector
     *
     * @param   voi  DOCUMENT ME!
     *
     * @return  true if successful, false if not
     */
    public boolean addVOI(VOI voi) {

        // check the voi name, fix if necessary
        if (contains(voi)) {
            voi.setName(buildName(voi.getName()));
        }

        if (super.add(voi)) { // add the voi to the vector
            fireVOIadded(voi); // notify listeners of a change
            Preferences.debug("Add voi: name = " + voi.getName() + "\n");

            return true;
        }

        return false;
    }

    /**
     * Override the Vector method to ensure that object is a voi, and that the new voi's name is unique.
     *
     * @param      o      index of the VOI
     * @param      index  DOCUMENT ME!
     *
     * @exception  IllegalArgumentException  for any argument <code>o</code> which is not an instance of <code>
     *                                       gov.nih.mipav.model.structures.VOI</code>
     */
    @Override
	public void insertElementAt(VOI o, int index) {

        // check that object is a VOI
        if (!(o instanceof VOI)) {
            throw new IllegalArgumentException();
        }

        VOI voi = o;

        // check the voi name, fix if necessary
        if (contains(voi)) {
            voi.setName(buildName(voi.getName()));
        }

        super.insertElementAt(voi, index); // add the voi to the vector
        fireVOIadded(voi); // notify listeners of a change

        Preferences.debug("Add voi: name = " + voi.getName() + "\n");
    }

    /**
     * <b>Overides</b> <code>Vector.remove().</code>
     *
     * <p>successful removal of the element results in the VOI listeners being notified.</p>
     *
     * @param   index  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    @Override
	public VOI remove(int index) {

        try {
            VOI voi = super.remove(index);
            fireVOIremoved(voi);

            return voi;
        } catch (ArrayIndexOutOfBoundsException badIndex) {
            throw badIndex;
        }
    }

    /**
     * DOCUMENT ME!
     */
    @Override
	public void removeAllElements() {
        super.removeAllElements();
        // fireVOIremoved(null);
    }


    public void removeAllVectorListeners() {
    	if ( listenerList == null )
    	{
    		return;
    	}
    	VOIVectorListener[] list = listenerList.getListeners(VOIVectorListener.class);
    	for ( int i = 0; i < list.length; i++ )
    	{
    		listenerList.remove(VOIVectorListener.class, list[i]);
    	}
    }

    /**
     * <b>Overides</b> <code>Vector.remove().</code>
     *
     * <p>successful removal of the element results in the VOI listeners being notified.</p>
     *
     * @param  index  DOCUMENT ME!
     */
    @Override
	public void removeElementAt(int index) {

        try {
            VOI voi = VOIAt(index);
            super.removeElementAt(index);
            fireVOIremoved(voi);
        } catch (ArrayIndexOutOfBoundsException badIndex) {
            throw badIndex;
        }
    }

    /**
     * <b>Overides</b> <code>Vector.removeRange().</code>
     *
     * <p>successfull removal of the range of elements results in the VOI listeners being notified.</p>
     *
     * @param  first  DOCUMENT ME!
     * @param  last   DOCUMENT ME!
     */
    @Override
	public void removeRange(int first, int last) {

        try {
            super.removeRange(first, last);
            fireVOIremoved(null);
        } catch (ArrayIndexOutOfBoundsException badIndex) {
            throw badIndex;
        }
    }

    /**
     * removes the update listener.
     *
     * @param   listener  DOCUMENT ME!
     *
     * @throws  IllegalArgumentException  DOCUMENT ME!
     */
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


    /**
     * Fires a VOI event based on the VOI. calls the listener's <code>addedVOI()</code> method.
     *
     * @param  voi  DOCUMENT ME!
     */
    protected void fireVOIadded(VOI voi) {

        try {

            // only if there are listeners to send events to should we
            // bother with creating an event and bothering the event queue.
            if (listenerList.getListenerCount(VOIVectorListener.class) == 0) {
                return;
            }
        } catch (NullPointerException npe) {
            listenerList = new EventListenerList();
        }

        // always create a new Event, since we need to carry
        // the changed VOI around.
        voiUpdate = new VOIVectorEvent(this, voi);

        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();

        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == VOIVectorListener.class) {
                ((VOIVectorListener) listeners[i + 1]).addedVOI(voiUpdate);
            }
        }
    }

    /**
     * Fires a VOI event based on the VOI. calls the listener's <code>removedVOI()</code> method.
     *
     * @param  voi  DOCUMENT ME!
     */
    protected void fireVOIremoved(VOI voi) {

        try {

            // only if there are listeners to send events to should we
            // bother with creating an event and bothering the event queue.
            if (listenerList.getListenerCount(VOIVectorListener.class) == 0) {
                return;
            }
        } catch (NullPointerException npe) {
            listenerList = new EventListenerList();
        }

        // always create a new Event, since we need to carry
        // the changed VOI around.
        voiUpdate = new VOIVectorEvent(this, voi);

        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();

        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == VOIVectorListener.class) {
                ((VOIVectorListener) listeners[i + 1]).removedVOI(voiUpdate);
            }
        }
    }
}
