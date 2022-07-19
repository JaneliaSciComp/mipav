package gov.nih.mipav.model.structures;


import java.util.Vector;


public class VOIBaseVector extends Vector<VOIBase> {

    /**  */
    private static final long serialVersionUID = 8244024438846673669L;
    public VOI parent;
    
    public VOIBaseVector() {
        super();
        this.parent = null;
    }
    
    public VOIBaseVector(VOI parent) {
        super();
        this.parent = parent;
    }

    @Override
	public void add( int index, VOIBase element )
    {
        super.add(index,element);
        element.setGroup(parent);
        parent.fireVOIBaseAdded(element);
    }
    
    @Override
	public boolean add(VOIBase e)
    {
        boolean result = super.add(e);
        if ( parent != null )
        {
        	e.setGroup(parent);
        	parent.fireVOIBaseAdded(e);
        }
        return result;
    }
        
    @Override
	public void addElement(VOIBase obj )
    {
        super.addElement(obj);
        if ( parent != null )
        {
        	obj.setGroup(parent);
        	parent.fireVOIBaseAdded(obj);
        }
    }
    
    @Override
	public int indexOf(Object o)
    {
        for ( int i = 0; i < size(); i++ )
        {
            if ( o == elementAt(i) )
            {
                return i;
            }
        }
        return -1;
    }
    
    @Override
	public void insertElementAt(VOIBase obj, int index)
    {
        super.insertElementAt(obj, index);
        obj.setGroup(parent);
        parent.fireVOIBaseAdded(obj);
    }
    
    @Override
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
    
    public boolean removeElement(VOIBase obj)
    {
        boolean result = super.removeElement(obj);
        obj.setGroup(null);
        parent.fireVOIBaseRemoved(obj);
        return result;
    }
    
    @Override
	public void removeElementAt(int index)
    {
        VOIBase element = super.remove(index);
        element.setGroup(null);
        parent.fireVOIBaseRemoved(element);
    }    
}
