package gov.nih.mipav.model.structures;


import java.util.Vector;


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
        e.setGroup(parent);
        parent.fireVOIBaseAdded(e);
        return result;
    }
    
    public void add( int index, VOIBase element )
    {
        super.add(index,element);
        element.setGroup(parent);
        parent.fireVOIBaseAdded(element);
    }
    
    //public boolean addAll( Collection<> c ){}
    
    //public boolean addAll( int index, Collection<> c ) {}
    
    public void addElement(VOIBase obj )
    {
        super.addElement(obj);
        obj.setGroup(parent);
        parent.fireVOIBaseAdded(obj);
    }
    
    public void insertElementAt(VOIBase obj, int index)
    {
        super.insertElementAt(obj, index);
        obj.setGroup(parent);
        parent.fireVOIBaseAdded(obj);
    }
    
    public VOIBase remove(int index)
    {
        VOIBase element = super.remove(index);
        element.setGroup(null);
        parent.fireVOIBaseRemoved(element);
        return element;
    }
    
    public boolean remove(VOIBase o)
    {
        boolean result = super.remove(o);
        o.setGroup(null);
        parent.fireVOIBaseRemoved(o);
        return result;
    }
    
    //public boolean removeAll( Collection<> c ) {}
    
    //public void removeAllElements() {}
    
    public boolean removeElement(VOIBase obj)
    {
        boolean result = super.removeElement(obj);
        obj.setGroup(null);
        parent.fireVOIBaseRemoved(obj);
        return result;
    }
    
    public void removeElementAt(int index)
    {
        VOIBase element = super.remove(index);
        element.setGroup(null);
        parent.fireVOIBaseRemoved(element);
    }
    
    //protected void removeRange(int fromIndex, int toIndex) {}
    
    //public VOIBase set(int index, VOIBase element) {}
    
    //public void setElementAt( VOIBase obj, int index ) {}
    
    //public void setSize(int newSize) {}

}
