package gov.nih.mipav.view;

import java.io.*;

/**
*		@version    1.0 Mar 1, 1999
*		@author     Matthew J. McAuliffe, Ph.D.
*
*/

public class ViewList implements Serializable {

    private String  string;
    private boolean state;

    public ViewList (String _string, boolean _state) {

        string  = _string;
        state   = _state;
    }

    public String getString() { return string;}

    public boolean getState() { return state; }

    public void setState(boolean _state) { state = _state;}

    public void setString(String _string) { string = _string;}

    public void printSelf(float value) {

        if (state == true) {
            Preferences.debug(string + "\t\t" + value + "\n");
        }
    }


}
