package gov.nih.mipav.model.structures;


import java.awt.Color;
import java.util.Vector;

import gov.nih.mipav.model.structures.event.*;

import gov.nih.mipav.view.*;

import javax.swing.event.*;


public class VOIBaseVector extends Vector<VOIBase> {

    /**  */
    private static final long serialVersionUID = 8244024438846673669L;
    public VOI parent;
    
    public VOIBaseVector(VOI parent) {
        super();
        this.parent = parent;
    }



    public boolean add(VOIBase e)
    {
        boolean result = super.add(e);
        parent.fireVOIBaseAdded(e);
        return result;
    }
    
    public void add( int index, VOIBase element )
    {
        super.add(index,element);
        parent.fireVOIBaseAdded(element);
    }
    
    //public boolean addAll( Collection<> c ){}
    
    //public boolean addAll( int index, Collection<> c ) {}
    
    public void addElement(VOIBase obj )
    {
        super.addElement(obj);
        parent.fireVOIBaseAdded(obj);
    }
    
    public void insertElementAt(VOIBase obj, int index)
    {
        super.insertElementAt(obj, index);
        parent.fireVOIBaseAdded(obj);
    }
    
    public VOIBase remove(int index)
    {
        VOIBase element = super.remove(index);
        parent.fireVOIBaseRemoved(element);
        return element;
    }
    
    public boolean remove(Object o)
    {
        boolean result = super.remove(o);
        parent.fireVOIBaseRemoved((VOIBase)o);
        return result;
    }
    
    //public boolean removeAll( Collection<> c ) {}
    
    //public void removeAllElements() {}
    
    public boolean removeElement(Object obj)
    {
        boolean result = super.removeElement(obj);
        parent.fireVOIBaseRemoved((VOIBase)obj);
        return result;
    }
    
    public void removeElementAt(int index)
    {
        VOIBase element = super.remove(index);
        parent.fireVOIBaseRemoved(element);
    }
    
    //protected void removeRange(int fromIndex, int toIndex) {}
    
    //public VOIBase set(int index, VOIBase element) {}
    
    //public void setElementAt( VOIBase obj, int index ) {}
    
    //public void setSize(int newSize) {}

}
