package gov.nih.mipav.model.structures;

import  java.util.*;

/**
*  CustomHashtable extends the Hashtable class and adds some
*  methods for generating keys for new elements to the Hashtable.
*  This is designed for storing the active images, but can also
*  be used for image frames or any other list that needs to enforce
*  unique keys.
*
*       @version 0.1 November 16, 2001
*       @author Lynne M. Pusanik
*
*       @see java.util.Hashtable
*
*/


public class CustomHashtable extends java.util.Hashtable
{

    //--------------------------------------------------------
    //  Constants
    //--------------------------------------------------------
    
    /** MAX_APPENDED_KEYS is the maximum number of keys that will
    **  appended to a "keyname" in order to make a key unique.
    */
    public static final int MAX_APPENDED_KEYS   = 500;
    
    //--------------------------------------------------------
    //  Constructors
    //--------------------------------------------------------
    
    /**
    *   Constructs a new hashtable with the same mappings 
    *   as the given Map. The hashtable is created with a 
    *   capacity of twice the number of entries in the given 
    *   Map or 11 (whichever is greater), and a default load 
    *   factor, which is 0.75.
    *   @param map      Map to base the hashtable on
    */
    public CustomHashtable(Map t) {
        super(t);
    }

    /**
    *   Constructs a new, empty hashtable with the specified 
    *   initial capacity and the specified load factor.
    *
    *   @param capacity     initial capacity of the hashtable
    *   @param loadFactor   the load factor of the hashtable
    */
    public CustomHashtable(int capacity, float loadFactor){
        super (capacity, loadFactor);
    }

    /**
    *   Constructs a new, empty hashtable with the specified 
    *   initial capacity and default load factor, which is 0.75.
    *
    *   @param  capacity    initial capacity of the hashtable
    */
    public CustomHashtable(int capacity){
        super (capacity);
    }

    /**
    *   Constructs a new, empty hashtable with a default capacity 
    *   and load factor, which is 0.75.
    */
    public CustomHashtable(){
        super();
    }

    //--------------------------------------------------------
    //  Other Methods
    //--------------------------------------------------------
    
    /**
    *   Ensures that the key is unique for this hashtable, then
    *   invokes the super.put() method generate a key and add a value
    *   to the hashtable.
    *
    *   @param  value   value for the hashtable entry.
    *   @return         returns the key used for this hashtable entry.
    *   @exception  NullPointerException    thrown when a unique key cannot
    *           be found or generated for this hashtable.
   */
    public Object put (Object value)throws NullPointerException
    {
        return this.put (null, value);
        
    } // end put()
    
    /**
    *   Ensures that the key is unique for this hashtable, then
    *   invokes the super.put() method to add the key and value
    *   to the table.
    *
    *   @param  key     key for the hashtable entry.
    *   @param  value   value for the hashtable entry.
    *   @return         returns the key used for this hashtable entry.
    *   @exception  NullPointerException    thrown when a unique key cannot
    *           be found or generated for this hashtable.
    */
    public Object put (Object key, Object value)throws NullPointerException
    {
        Object newKey = key;
        
        // if key is null, then try generating one
        if (key == null)
        {
            newKey = makeKey(key, value);
        }
            
        // make sure the key is unique
        else if (this.containsKey(key))
        {
            // need to find a new key
            newKey = appendKey(key);
        }
        
        // add the newKey and value to the hashtable
        super.put (newKey, value);
        
        // return the newKey
        return newKey;
        
    } // end put()

    /**
    *   Creates a key for a value based on the value's hashcode.
    *   If the key exists in the hashtable already, then it is 
    *   appended to try and get to a unique key.
    *   Throws a NullPointer Exception if a unique key can't 
    *   be found after appending.
    *
    *   @return     returns the newly generated key.
    *   @exception  NullPointerException    thrown when a unique 
    *           key cannot be generated.
    *                   
    */
    protected Object makeKey (Object key, Object value)throws NullPointerException
    {
        String keyname = String.valueOf (value.hashCode());
        Object newKey = (Object)keyname;
        
        if (this.containsKey (keyname))
            newKey = appendKey (keyname);
            
        return newKey;
        
    } // end makeKey()
    
    /**
    *   Starts with a key that already exists in the hashtable
    *   and appends integers to its toString() value until
    *   reaching MAX_APPENDED_KEYS.  Throws a NullPointer Exception
    *   if a unique key can't be found after appending all 
    *   the integer values.
    *
    *   @return     returns the newly formed key.
    *   @exception  NullPointerException    thrown when key cannot be
    *                   appended to make a unique key.
    */
    protected Object appendKey (Object key)throws NullPointerException
    {
        String keyname = key.toString();
        
        for (int i = 1; i <= MAX_APPENDED_KEYS; i++)
        {
            if (!this.containsKey (new String(keyname + i)))
            {
                // we have a winner!
                System.gc();
                return (Object)new String(keyname + i);
            }
        }
        
        // if we get here, then we failed to find a unique
        // key
        System.gc();
        throw new NullPointerException("Failed to create new key for "
            + keyname + " after " + MAX_APPENDED_KEYS + " iterations.");

    } // end appendKey()
    
}  // end CustomHashtable class
