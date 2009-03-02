package gov.nih.mipav.model.file.xcede;

import java.util.*;

/**
 * This class represents the any level element of XCEDE(XML-based Clinical and Experimental Data Exchange) schema.
 * @author Hailong Wang
 * @version 1.0  04/21/2006
 */
public abstract class Element {
    public static final String LEVEL = "level";

    /**
     * The string constant which represents the key of the parent element.
     */
    public static final String PARENT_ELEMENT = "parent";
    
    /**
     * Uses the <code>Hashtable</code> to store the attributes information.
     */
    private Hashtable attributes;
    
    /**
     * Constructor
     */
    public Element() {
        this(null, null);
    }
    
    public Element(Element parent){
        this(null, parent);
    }
    
    public Element(String level){
        this(level, null);
    }
    
    public Element(String level, Element parent){
        attributes = new Hashtable();
        if(parent != null){
            setParent(parent);
        }
        if(level != null && level.length() > 0 ){
            setLevel(level);
        }
    }
    
    /**
     * Returns the number of the keys that this element includes.
     * @return the number of the keys that this element includes.
     */
    public int getKeyCount(){
        return attributes.size();
    }
    
    /**
     * Used to clean memory.
     */
    protected void finalize() throws Throwable{
        System.out.println("Entering the " + getLevel() + " Element's finalize() ... ");
        Enumeration keys = keys();
        String level = this.getLevel();
        while(keys.hasMoreElements()){
            Object key = keys.nextElement();
            Object value = get((String)key);
            if(value instanceof Element){
                ((Element)value).finalize();
            }else if(value instanceof Vector){
                for(int i = 0; i < ((Vector)value).size(); i++){
                    Object o = ((Vector)value).get(i);
                    if(o instanceof XCEDEElement){
                        ((XCEDEElement)o).finalize();
                    }
                }
                ((Vector)value).clear();
            }
            System.out.println(level + ": " + key + "--garbage collection.");
            attributes.remove(key);
        }
        attributes = null;
        super.finalize();
    }

    /**
     * Returns the level that this <code>XCEDEElement</code> locates.
     * @return the level that this <code>XCEDEElement</code> locates.
     */
    public String getLevel(){
        Object value = get(LEVEL);
        if(value instanceof String){
            return (String)value;
        }
        return null;
    }
    
    /**
     * Sets the level that this <code>XCEDEElement</code> locates.
     * @param level the level that this <code>XCEDEElement</code> locates.
     */
    public void setLevel(String level){
        if(level == null || level.length() == 0){
            return;
        }
        put(LEVEL, level);
    }

    /**
     * Returns the attribute value for the specified attribute name.
     * @param key the attribute name.
     * @return the attribute value.
     */
    public Object get(String key){
        return attributes.get(key);
    }
    
    /**
     * Returns the set of attribute names.
     */
    public Set keySet(){
        return attributes.keySet();
    }
    
    /**
     * Returns the attribute name list.
     */
    public Enumeration keys(){
        return attributes.keys();
    }
    
    /**
     * Adds the <code>key</code> attribute to the current <code>Element</code> 
     * @param key the attribute name.
     * @param value the attribute value.
     */
    public void put(String key, Object value){
        if(key == null){
            return ;
        }else if(value == null ||(value instanceof String && ((String)value).length() == 0)){
            attributes.remove(key);
            return;
        }
        attributes.put(key, value);
    }
    
    /**
     * Returns the parent element of this <code>Element</code>.
     * @return the parent element of this <code>Element</code>.
     */
    public Element getParentElement(){
        Object value = get(PARENT_ELEMENT);
        if(value == null){
            return null;
        }else if(value instanceof Element){
            return (Element)value;
        }else{
            return null;
        }
    }
    
    /**
     * Sets the parent element of this <code>Element</code>.
     * @param parent the parent element.
     */
    public void setParent(Element parent){
        if(parent == null){
            attributes.remove(Element.PARENT_ELEMENT);
        }else{
            put(PARENT_ELEMENT, parent);
        }
    }
    
    /********************************************************************
     ***** The functions used to handle multi-occured child element *****
     ********************************************************************/
    
    /**
     * Returns the child list corresponding to the specified key.
     * @param the specified child element key.
     * @return the child list.
     */
    public Vector getChildList(String key){
        Object value = get(key);
        if(value == null || value instanceof Vector){
            return (Vector)value;
        }
        return null;
    }
    
    /**
     * Returns the specified child element at the specified index.
     * @param index  the location index.
     * @param key    the child element key
     * @return       the specified child object.
     */
    public Object getChild(int index, String key){
        if(key == null || key.length() == 0){
            return null;
        }
        Vector childList = getChildList(key);
        if(childList == null){
            return null;
        }
        return childList.get(index);
    }
    
    /**
     * Sets the specified child element list.
     * @param key the child element key
     * @param childList the child list the specified child element. 
     */
    public void setChildList(String key, Vector childList){
        if(key != null && key.length() > 0 && childList != null){
            put(key, childList);
        }
    }
    
    /**
     * Adds the child element to this element.
     * @param key     the child element key.
     * @param child   the child element.
     * @return        true if it succeeds
     */
    public boolean addChild(String key, Object child){
        if(key == null || key.length() == 0 || child == null){
            return false;
        }
        Vector childList = getChildList(key);
        if(childList == null){
            childList = new Vector();
            put(key, childList);
        }
        boolean ret = childList.add(child);
        if(ret && child instanceof Element){
            ((Element)child).setParent(this);
        }
        return ret;
    }
}
