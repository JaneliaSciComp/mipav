package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

import java.util.*;

/* Static Uniform Grid
A spatial search structure for a accessing a container of objects. 
It is based on a uniform grid overlayed over a protion of space. 
The grid partion the space into cells. Cells contains just pointers 
to the object that are stored elsewhere.
The set of objects is meant to be static and pointer stable. 

Useful for situation were many space related query are issued over 
the same dataset (ray tracing, measuring distances between meshes, 
re-detailing ecc.). 
Works well for distribution that ar reasonably uniform.
How to use it:
ContainerType must have a 'value_type' typedef inside.
(stl containers already have it)

Objects pointed by cells (of kind 'value_type') must have
a 'ScalarType' typedef (float or double usually)
and a member function:

void GetBBox(Box3<ScalarType> &b)
which return the bounding box of the object

When using the GetClosest() method, the user must supply a functor object
(whose type is a method template argument) which expose the following
operator ():

bool operator () (const ObjType & obj, const Point3f & point, ScalarType & mindist, Point3f & result);
which return true if the distance from point to the object 'obj' is < mindist
and set mindist to said distance, and result must be set as the closest 
point of the object to point)
*/


public class StaticGrid extends BasicGrid {
	
	// Set of all links
	public Vector<Link> links = new Vector<Link>();
	
	// Grid real
	public Vector<Link> grid = new Vector<Link>();
	public BasicGrid BT = new BasicGrid();
	
	
	// This function automatically compute a reasonable size for the uniform grid such that the number of cells is
	// the same of the nubmer of elements to be inserted in the grid.
	//
	// Note that the bbox must be already 'inflated' so to be sure that no object will fall on the border of the grid.	
	public final void set(Box3 _bbox, Vector<Vertex> vertex)
	{

		// int _size=(int)std::distance<OBJITER>(_oBegin,_oEnd);
		int _size = vertex.size();
		Point3 _dim = _bbox.max.sub(_bbox.min);
		Point3 _siz = new Point3();
		bestDim( _size, _dim, _siz );
		
		set(vertex, _bbox,_siz);
	}			
	
	public final void set(Vector<Vertex> vertex, Box3 _bbox, Point3 _siz)
	{
			Vertex i;
			Iterator<Vertex> ii;
			
			
			this.bbox.assign(_bbox);
			this.siz.assign(_siz);
			
			BT.bbox.assign(this.bbox);
			BT.siz.assign(this.siz);
			
			// find voxel size starting from the provided bbox and grid size. 
			
			this.dim  = this.bbox.max.sub(this.bbox.min);
			this.voxel.x = this.dim.x/this.siz.x;
			this.voxel.y = this.dim.y/this.siz.y;
			this.voxel.z = this.dim.z/this.siz.z;	
			
			BT.dim.assign(this.dim);
			BT.voxel.assign(this.voxel);
			
			
				// Allocation grid: +1 for the sentinel
				grid.setSize( (int)(this.siz.x*this.siz.y*this.siz.z+1) );

				// Cycle entry of tetrahedra: creating links
				links.clear();
				// i = (Vertex)vertex.firstElement();
				ii = vertex.iterator();
				
				while ( ii.hasNext() )
				{
					i = ii.next();
					Box3 bb = new Box3();			// Boundig box del tetraedro corrente
					i.getBBox(bb);
					bb.intersect(this.bbox);
					if(! bb.isNull() )
					{

						Box3 ib = new Box3();		// Boundig box in voxels
						this.BoxToIBox( bb,ib );
						int x,y,z;
						for(z=(int)ib.min.z;z<=ib.max.z;++z)
						{
							int bz = (int)(z*this.siz.y);
							for(y=(int)(ib.min.y);y<=ib.max.y;++y)
							{
								int by = (int)((y+bz)*this.siz.x);
								for(x=(int)ib.min.x;x<=ib.max.x;++x)
									// Enter calculating current cell
									// if( pt.Intersect( ... )
									// links.push_back( Link(&(*i),by+x) );
									links.add( new Link(i,by+x) );
							}
						}
					}
					
					
				}
				// Push della sentinella
				/*links.push_back( Link((typename ContainerType::iterator)NULL,
				(grid.size()-1)));*/

				// links.push_back( Link( NULL,	int(grid.size())-1) );
				
				links.add( new Link( null,	(int)(grid.size())-1) );
				
				Collections.sort(links);
				
				int pg;
				Iterator<Link> pl = links.iterator();
				Link pl_elem = null; 
				if ( pl.hasNext() )
                {
                    pl_elem = pl.next();
                }
				
				for(pg=0;pg<grid.size();++pg)
				{
					assert ( pl_elem != null );
					grid.set(pg, pl_elem );   // Addressing issues here
					//System.err.println ("Grid  " + pg + " " + pl_elem + " " + pl_elem.Index() );
					while( (int)pg == pl_elem.index() )	// Found beginning
					{
					    //System.err.println("    while..."  + pl_elem + " " + pl_elem.Index() );
						if ( pl.hasNext() )
						{
	                        pl_elem = pl.next();
						}
						else
						{
	                        //System.err.println("    while...BREAK" );
						    break;
						}
						
					}
				}
				// System.err.println( " grid = " + grid);

	}		

	/// BY INTEGER COORDS
	public final Link grid( int x, int y, int z, int[] index )
	{
		assert(!( x<0 || x>=BT.siz.x || y<0 || y>=BT.siz.y || z<0 || z>=BT.siz.z ));
		assert(grid.size()>0);
        //int temp = ( x+(int)BT.siz.x*(y+(int)BT.siz.y*z));
        int temp = (int)BT.siz.y*z;
        temp += y;
        temp *= ((int)BT.siz.x);
        temp += x;
		//temp = getIndex(grid.firstElement()) + temp;
        index[0] = temp;
		return  grid.elementAt(temp);
	}

    
    public final Link get(int index) {
      return grid.elementAt(index);
    }
    
    public final Link getLink(int index) {
      return links.elementAt(index);
    }
	
	
	public final int getInSphere(PointDistanceFunctor _getPointDistance, VertTmark _marker, Point3 _p, float _r,
			Vector<Vertex> _objectPtrs, Vector<Float> _distances, Vector<Point3> _points) {
		return gridGetInSphere(this,_getPointDistance,_marker,_p,_r,_objectPtrs,_distances,_points);
	}
	
	public final int gridGetInSphere(StaticGrid _Si,
							   PointDistanceFunctor _getPointDistance, 
							   VertTmark _marker,
							   Point3 _p, 
							   float _r,
							   Vector<Vertex> _objectPtrs, 
							   Vector<Float> _distances, 
							   Vector<Point3> _points
							   ) {
		ClosestIterator Cli=new ClosestIterator(_Si,_getPointDistance);
		Cli.setMarker(_marker);
		Cli.init(_p,_r);
		_objectPtrs.clear();
		_distances.clear();
		_points.clear();
		while (!Cli.end())
		{
			
			_objectPtrs.add(Cli.get());          //  Ruida  address conversion????????
			_distances.add(Cli.dist());
			_points.add(Cli.nearestPoint());
			
			// ++Cli;
			Cli.plusplus();
			
		}
		return ((int)_objectPtrs.size());
		
	}
	
	
	
	
	
}