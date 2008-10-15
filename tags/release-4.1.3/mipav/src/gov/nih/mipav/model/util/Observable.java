package gov.nih.mipav.model.util;

/**
 * An interface for the Observable
 * @author Hailong Wang
 * @version 1.0 05/11/06
 */
public interface Observable {
    /**
     * Adds an observer to the set of observers for this object, 
     * provided that it is not the same as some observer already in the set.
     * @param o an observer object.
     */
    public void addObserver(Observer o);
    
    /**
     * Marks this object as no longer having been changed.  
     */
    public void clearChanged();
    
    /**
     * Returns the number of the observers of this Observable object.
     * @return the number of the observers of this Observable object.
     */
    public int countObservers();
    
    /**
     * Deletes an observer from the set of observers of this object.
     * @param o an observer object.
     */
    public void deleteObserver(Observer o);
    
    /**
     * Clears the observer list so that this object no longer has
     * any observer.
     */
    public void deleteObservers();
    
    /**
     * Test if this object has changed.
     * @return true if the object has changed.
     */
    public boolean hasChanged();
    
    /**
     * If this object has changed, as indicated by the hasChanged
     * method, then notify all of its observers and then call the
     * clearChanged method to indicate that this object has no 
     * longer changed.
     * @param obj the argument object
     */
    public void notifyObservers(Object obj);
    
    /**
     * Marks this object as having been changed.
     */
    public void setChanged();
}
