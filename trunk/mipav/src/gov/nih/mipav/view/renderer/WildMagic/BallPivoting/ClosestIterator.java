package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

import java.util.*;
import java.lang.*;

public class ClosestIterator {
	
	protected Point3 p;							//initial point
	protected StaticGrid Si;		  //reference to spatial index algorithm
	protected boolean end;									//true if the scan is terminated
	protected float max_dist;		  //max distance when the scan terminate
	protected Box3 explored = new Box3();		  //current bounding box explored
	protected Box3 to_explore = new Box3();		//current bounding box explored
	protected float radius;			  //curret radius for sphere expansion
	protected float step_size;		  //radius step
	protected Vector<Entry> Elems = new Vector<Entry>(); //element loaded from the current sphere
	
	protected PointDistanceFunctor dist_funct;
	protected VertTmark tm;
	
	protected Entry CurrentElem;  // //iterator to current element
	
	

	///control the end of scanning
	public boolean  _EndGrid()
	{
		if (explored.min.equals(new Point3(0, 0, 0)) && explored.max.equals(Si.siz)) {
			end =true;
		}
		return end;
	}


	public void _UpdateRadius()
	{
		if (radius>=max_dist) {
			end=true;
		}
		radius+=step_size;
		//control bounds
		if (radius>max_dist)
			radius=max_dist;
	}

	///add cell to the curren set of explored cells
	public boolean _NextShell()
	{

		//then expand the box
        explored.assign(to_explore);          // Ruida ?????????????? shallow or deep
        //explored = to_explore;
		_UpdateRadius();
		Box3 b3d = new Box3(p,radius);
		Si.BoxToIBox(b3d,to_explore);
		Box3 ibox = new Box3(new Point3(0,0,0),Si.siz.sub(new Point3(1,1,1)));
		to_explore.Intersect(ibox);
		if (!to_explore.IsNull())
		{
			assert(!( to_explore.min.x<0 || to_explore.max.x>=Si.siz.x ||
				to_explore.min.y<0 || to_explore.max.y>=Si.siz.y ||  to_explore.min.z<0
				|| to_explore.max.z>=Si.siz.z ));
			return true;
		}
		return false;
	}
	
	
	public ClosestIterator(StaticGrid _Si,PointDistanceFunctor _dist_funct) {
		Si = _Si;
		dist_funct = _dist_funct;
	}
	

	///set the current spatial indexing structure used
	public void SetIndexStructure(StaticGrid _Si) {
		Si=_Si;
	}

	public void SetMarker(VertTmark _tm)
	{
		tm=_tm;
	}
	
	///initialize the Iterator
	public void Init(Point3 _p, float _max_dist)
	{
		explored.SetNull();
		to_explore.SetNull();
		p = _p;
		max_dist=_max_dist;
		Elems.clear();
		end=false;
		tm.UnMarkAll();
		//step_size=Si.voxel.X();
		step_size=Si.voxel.Norm();
		radius=0;

		///inflate the bbox until find a valid bbox
		while ((!_NextShell())&&(!End())) {}

		while ((!End())&& Refresh()&&(!_EndGrid()))
				_NextShell();
	}

	//return true if the scan is complete
	public boolean End() {
		return end;
	}

	// int count = 0;
	///refresh Object found	also considering current share radius,
	//and object comes from	previous that are already in	the	stack
	//return false if no elements find
	public boolean Refresh()
	{
	    int[] tempIndex = new int[1];
		int	ix,iy,iz;
		int i;
		for( iz = (int)to_explore.min.z;iz <=	(int)to_explore.max.z; ++iz)
			for(iy =(int)to_explore.min.y; iy	<= (int)to_explore.max.y; ++iy)
				for(ix = (int)to_explore.min.x; ix	<= (int)to_explore.max.x;++ix)
				{
					// this test is to avoid to re-process already analyzed cells.
					if((explored.IsNull())||
						(ix<(int)explored.min.x || ix>(int)explored.max.x ||
						iy< (int)explored.min.y || iy> (int)explored.max.y ||
						iz< (int)explored.min.z || iz> (int)explored.max.z ))
					{
						Link first = new Link();
						Link last = new Link();
						Link l;
                        // System.err.println("check");
						//Si.Grid(ix,iy,iz,first,last);
						
						first = Si.Grid(ix, iy, iz, tempIndex);
				        //last.i = first.i + 1;
				        int firstIndex = tempIndex[0];
				        int lastIndex = firstIndex+1;//Si.getIndex(last);
				        last = Si.get(lastIndex);
						
						// System.err.println("first = " + first);
						// System.err.println("last = " + last);

                        //System.err.println(  "                   : " + 
                        //        first.i + " " + last.i + "             "  + ix + " " + iy + " " + iz );

                        int lIndex = Si.links.indexOf(first);
						for(l = Si.getLink(lIndex); l != last; l = Si.getLink(lIndex++) )
						{

                            //System.err.println(  "                   : inside for " + 
                            //       first + " " + last + " " + l + " " + l.i );
                             
								// shallow  is correct here
								Vertex	elem=  l.get();
								// deep  ???  no.
								// System.err.print(" i = " + i + " elem.p() = ");  elem.P().print();
								
								if (!tm.IsMarked(elem))
								{
	                                // System.err.println("amaze");
									Point3 nearest = new Point3();
									float[] dist= new float[1];
									dist[0] = max_dist;
									if (dist_funct.call( l.get(),p,dist,nearest)) {
										// System.err.println("count = " + count++);
										Elems.add(new Entry(elem,(float)Math.abs(dist[0]),nearest));
									}
									tm.Mark(elem);
								}
						}
					}

				}

		    // System.err.println("Elems.size() = " + Elems.size());
			////sort the elements in Elems and take a iterator to the last one
			// std.sort(Elems.begin(),Elems.end());
		    Collections.sort(Elems);
		    
			// CurrentElem=Elems.rbegin();
		    if ( Elems.size() == 0 ) { 
		    	// CurrentElem = null;
		    } else {
		    	CurrentElem=Elems.lastElement();
		    }
		return((Elems.size()==0)||(Dist()>radius));
	}
	

	public boolean ToUpdate() {
		return ((Elems.size()==0)||(Dist()>radius));
	}

	 // operator ++
	public void plusplus()
	{
		if (!Elems.isEmpty()) Elems.remove(Elems.lastElement());
		// else return;
        // System.err.println("1. Elems.size() " + Elems.size());
		// if( Elems.size() == 0 ) return;
		// CurrentElem = Elems.rbegin();
		if ( Elems.size() == 0 ) {
			// CurrentElem = null;
		} else {
			CurrentElem = Elems.lastElement();
		}
		if ((!End())&& ToUpdate())
		{
		    //System.err.println( "ClosestIterator ++" );
            while ((!End())&& Refresh()&&(!_EndGrid()))
            {

                //System.err.println( "ClosestIterator ++ calling NS" );
                _NextShell();
            }
		}
	}


	// &operator *()
	public Vertex get() {
	    //System.err.println( "ClosestIterator *operator " + CurrentElem.elem._p.x + " " + CurrentElem.elem._p.y + " " + CurrentElem.elem._p.z );
		return CurrentElem.elem;
	}
	
	//&operator *()
	public Object dereference(){
		return CurrentElem.elem;
	}
	
	//return distance of the element form the point if no element
	//are in the vector then return max dinstance
	public float Dist()
	{
		if (Elems.size()>0)
			return (CurrentElem.dist);
		else
			return Float.MAX_VALUE;
	}

	public Point3 NearestPoint(){
		return CurrentElem.intersection;
	}
	
	
	
	
	

	
   
	
	
	
	

	
	
	
}