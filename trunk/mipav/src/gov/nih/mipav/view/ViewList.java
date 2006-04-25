package gov.nih.mipav.view;


import java.io.*;


/**
 * DOCUMENT ME!
 *
 * @version  1.0 Mar 1, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */

public class ViewList implements Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3493522274714412775L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean state;

    /** DOCUMENT ME! */
    private String string;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ViewList object.
     *
     * @param  _string  DOCUMENT ME!
     * @param  _state   DOCUMENT ME!
     */
    public ViewList(String _string, boolean _state) {

        string = _string;
        state = _state;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getState() {
        return state;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getString() {
        return string;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  value  DOCUMENT ME!
     */
    public void printSelf(float value) {

        if (state == true) {
            Preferences.debug(string + "\t\t" + value + "\n");
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  _state  DOCUMENT ME!
     */
    public void setState(boolean _state) {
        state = _state;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  _string  DOCUMENT ME!
     */
    public void setString(String _string) {
        string = _string;
    }


}
