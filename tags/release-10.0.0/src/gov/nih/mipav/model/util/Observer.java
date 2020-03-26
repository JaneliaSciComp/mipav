package gov.nih.mipav.model.util;

/**
 * An interface for the Observer pattern.
 * @author Hailong Wang
 * @version 1.0 05/11/06
 */
public interface Observer {
    /**
     * This method was called whenever the observed object is changed.
     * @param o  the observerd object.
     * @param arg an argument passed by the notifyObservers method.
     */
    public void update(Observable o, Object arg);
}
