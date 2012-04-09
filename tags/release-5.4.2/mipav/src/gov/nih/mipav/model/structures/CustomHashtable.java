package gov.nih.mipav.model.structures;


import java.util.*;


/**
 * CustomHashtable extends the Hashtable class and adds some methods for generating keys for new elements to the
 * Hashtable. This is designed for storing the active images, but can also be used for image frames or any other list
 * that needs to enforce unique keys.
 * 
 * @version 0.1 November 16, 2001
 * @author Lynne M. Pusanik
 * @see java.util.Hashtable
 */
public class CustomHashtable<V> extends Hashtable<String, V> {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8592617112282655416L;

    /**
     * MAX_APPENDED_KEYS is the maximum number of keys that will* appended to a "keyname" in order to make a key unique.
     */
    public static final int MAX_APPENDED_KEYS = 500;

    /**
     * Constructs a new, empty hashtable with a default capacity and load factor, which is 0.75.
     */
    public CustomHashtable() {
        super();
    }

    /**
     * Constructs a new hashtable with the same mappings as the given Map. The hashtable is created with a capacity of
     * twice the number of entries in the given Map or 11 (whichever is greater), and a default load factor, which is
     * 0.75.
     * 
     * @param t Map to base the hashtable on
     */
    public CustomHashtable(Map<String, V> t) {
        super(t);
    }

    /**
     * Constructs a new, empty hashtable with the specified initial capacity and default load factor, which is 0.75.
     * 
     * @param capacity initial capacity of the hashtable
     */
    public CustomHashtable(int capacity) {
        super(capacity);
    }

    /**
     * Constructs a new, empty hashtable with the specified initial capacity and the specified load factor.
     * 
     * @param capacity initial capacity of the hashtable
     * @param loadFactor the load factor of the hashtable
     */
    public CustomHashtable(int capacity, float loadFactor) {
        super(capacity, loadFactor);
    }

    /**
     * Returns a unique key string for the hashtable based on a given key string.
     * 
     * @param key A string that we want to base a hashtable key on.
     * 
     * @return A unique key, which should be used as an input to the {@link put()} method.
     */
    public String makeUniqueKey(String key) {
        String newKey = key;

        if (this.containsKey(key)) {
            newKey = appendKey(key);
        }

        return newKey;
    }

    /**
     * Starts with a key that already exists in the hashtable and appends integers to its toString() value until
     * reaching MAX_APPENDED_KEYS. Throws a NullPointer Exception if a unique key can't be found after appending all the
     * integer values.
     * 
     * @param key DOCUMENT ME!
     * 
     * @return returns the newly formed key.
     * 
     * @exception NullPointerException thrown when key cannot be appended to make a unique key.
     */
    protected String appendKey(String key) throws NullPointerException {
        String keyname = key.toString();

        for (int i = 1; i <= MAX_APPENDED_KEYS; i++) {

            if ( !this.containsKey(new String(keyname + i))) {

                // we have a winner!
                System.gc();

                return new String(keyname + i);
            }
        }

        // if we get here, then we failed to find a unique key
        System.gc();
        throw new NullPointerException("Failed to create new key for " + keyname + " after " + MAX_APPENDED_KEYS
                + " iterations.");
    }
}
