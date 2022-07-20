package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;


public class Entry implements Comparable {
	
	public Entry(Vertex _elem,float _dist,Point3 _intersection)
	{
		elem = _elem;
		dist=_dist;
		intersection = _intersection;
	}
	public final boolean lessThan( Entry l ) {
		return (dist > l.dist);                  // // Ruida >  ?????????????????
	}    
	
	public final boolean equals(Entry l) { return elem.equals(l.elem); }
	
	public final int compareTo(Object o) {
		Entry other = (Entry) o;
		if (lessThan(other))
			return -1;
		else if ( other.lessThan(this) )
		    return 1;
		else
		    return 0;
	}

	
	
	
	public Vertex elem;
	public float dist;
	public Point3 intersection;
	
	
}