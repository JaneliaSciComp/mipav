package gov.nih.mipav.view.renderer;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.event.*;


/**
 * This class holds the necessary information for each "mouse event" that shows up in the list. In reality, the "mouse
 * event" listed is a vector of mouse events that begins with a mousePressed and ends with a mouseReleased. In between
 * are mouseDragged events. In this way all events that happen on the canvas are accounted for. If we were to put every
 * event that actually happened in the list, the list would soon contain hundreds of elements. The mouseEvents vector
 * can be empty, as is the case when a view is saved. Then just the name and the view are saved; the vector holding
 * mouseEvents remains empty.
 */
public class MouseEventVector implements Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5183229064629196522L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** First time accessed. */
    public boolean first;

    /** Current mode. */
    public int mode;

    /** Vector to record mouse events. */
    public Vector mouseEvents;

    /** Name of the events. */
    public String name;

    /** State being recorded. */
    public Object state;

    /** Vector to record state changes. */
    public Vector stateVector;

    /** Parent frame transform3D view. */
    public double[] view;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new structure to hold necessary information, with the name in the list and the beginning view saved.
     * The Vector containing the rest of the mouseEvents is also created but begins empty. Elements are added with the
     * add method, below.
     *
     * @param  name   Name of this view or mouse event.
     * @param  view   Transform representing the view.
     * @param  first  If this is the first-time accessed use <code>true</code> otherwise, use <code>false</code>.
     * @param  state  The state being recorded.
     * @param  mode   Current mode.
     */
    public MouseEventVector(String name, double[] view, boolean first, Object state, int mode) {
        this.name = name;
        this.view = view;
        this.first = first;
        this.state = state;
        this.mode = mode;

        try {
            mouseEvents = new Vector();
            stateVector = new Vector();
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds a new mouse event to the end of the mouseEvents vector.
     *
     * @param  event  Event to add.
     */
    public void add(Object event) {
        mouseEvents.add(event);
    }

    /**
     * Adds a new mouse event to the end of the mouseEvents vector.
     *
     * @param  event  Event to add.
     * @param  state  State to add
     */
    public void add(Object event, Object state) {
        mouseEvents.add(event);
        stateVector.add(state);
    }

    /**
     * Returns the mouse pointer mode at the beginning of events.
     *
     * @return  mode The mode.
     */
    public int getMode() {
        return mode;
    }

    /**
     * Returns the mouse events vector.
     *
     * @return  Vector containing all the mouse events that happened after (and including) the mousePressed event.
     */
    public Vector getMouseEvents() {
        return mouseEvents;
    }

    /**
     * Returns the current vector title name.
     *
     * @return  name The name.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the state at the beginning of events.
     *
     * @return  state The state.
     */
    public Object getState() {
        return state;
    }

    /**
     * Returns the state vector.
     *
     * @return  Vector containing all the states parallel to mouse events.
     */
    public Vector getStateVector() {
        return stateVector;
    }

    /**
     * Returns the view stored in this structure.
     *
     * @return  The transform representing the view.
     */
    public double[] getView() {
        return view;
    }

    /**
     * Returns flag indicating if this is the first mouse event vector in a series.
     *
     * @return  <code>true</code> if the first mouse event vector, otherwise <code>false</code>.
     */
    public boolean isFirst() {
        return first;
    }

    /**
     * Set the mode at the beginning of the vector array.
     *
     * @param  _mode  The mode
     */
    public void setMode(int _mode) {
        this.mode = _mode;
    }

    /**
     * Set the current vector title name.
     *
     * @param  _name  The name.
     */
    public void setName(String _name) {
        this.name = _name;
    }

    /**
     * Set the state at the beginning of vector array.
     *
     * @param  _state  The state.
     */
    public void setState(Object _state) {
        this.state = _state;
    }

    /**
     * Set the current vector view - transform3D object.
     *
     * @param  _view  The present view transform.
     */
    public void setView(double[] _view) {
        this.view = _view;
    }

    /**
     * Called by some native invokeMethod. Provides a way to read this class out as a object stream. First, the size of
     * the vector is read, then each mouseEvent into the vector, then the name, then the view. The view is read in as a
     * String because it is not serializable. The read method corresponds to the write method, below.
     *
     * @param   stream  Object stream to read from.
     *
     * @throws  IOException             DOCUMENT ME!
     * @throws  ClassNotFoundException  DOCUMENT ME!
     */
    private void readObject(ObjectInputStream stream) throws IOException, ClassNotFoundException {
        double[] matrix;

        try {
            mouseEvents = new Vector();
            stateVector = new Vector();
            matrix = new double[16];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return;
        }

        int size = stream.readInt();

        for (int i = 0; i < size; i++) {

            try {
                // MouseEvent event =  (MouseEvent)stream.readObject();
                // this is a little strange; for some reason, the source was null when we read it in
                // from the stream, so in order for things to work properly we need to give it a non-null
                // source.  However just setting the source to parentScene.getCanvas() didn't work; that
                // also was null.  Perhaps it has to do with where this is called, which is unknown to us.

                Object eventRead = stream.readObject();

                if (eventRead instanceof MouseEvent) {
                    MouseEvent event = (MouseEvent) eventRead;
                    MouseEvent newEvent = new MouseEvent(new Container(), event.getID(), event.getWhen(),
                                                         event.getModifiers(), event.getX(), event.getY(),
                                                         event.getClickCount(), event.isPopupTrigger());
                    mouseEvents.add(newEvent);

                    SceneState vec = (SceneState) stream.readObject();
                    stateVector.add(vec);
                } else {
                    ChangeEvent newEvent = (ChangeEvent) eventRead;
                    mouseEvents.add(newEvent);

                    SceneState vec = (SceneState) stream.readObject();
                    stateVector.add(vec);
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in mouse recorder.");

                return;
            }

        }

        name = (String) stream.readObject();

        String viewName = (String) stream.readObject();

        StringTokenizer token = new StringTokenizer(viewName, "\n,");

        if (token.countTokens() != 16) {
            throw new IOException("Matrix is wrong size when trying to read object file in JDialogMouseRecorder.");
        }

        int i = 0;

        while (token.hasMoreTokens()) {
            matrix[i] = Double.valueOf(token.nextToken()).doubleValue();
            i++;
        }

        view = matrix;
        first = stream.readBoolean();
        state = stream.readObject();
        mode = stream.readInt();
    }

    /**
     * Called by some native invokeMethod. Provides a way to write this class out as a object stream. First, the size of
     * the vector is written, then each mouseEvent in the vector, then the name, then the view. The view is written out
     * as a String because it is not serializable. The write method corresponds to the read method, above.
     *
     * @param   stream  Object stream to write to.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeObject(ObjectOutputStream stream) throws IOException {
        stream.writeInt(mouseEvents.size());

        for (int i = 0; i < mouseEvents.size(); i++) {

            if (mouseEvents.elementAt(i) instanceof MouseEvent) {
                MouseEvent event = (MouseEvent) mouseEvents.elementAt(i);
                stream.writeObject(event);

                SceneState stateV = (SceneState) stateVector.elementAt(i);
                stream.writeObject(stateV);
            } else {
                ChangeEvent event = (ChangeEvent) mouseEvents.elementAt(i);
                stream.writeObject(event);

                SceneState stateV = (SceneState) stateVector.elementAt(i);
                stream.writeObject(stateV);
            }
        }

        stream.writeObject(name);
        stream.writeObject(view.toString());
        stream.writeBoolean(first);
        stream.writeObject(state);
        stream.writeInt(mode);
    }

}
