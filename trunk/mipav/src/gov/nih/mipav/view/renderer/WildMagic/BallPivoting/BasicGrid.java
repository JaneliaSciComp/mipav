package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

/** BasicGrid

Basic Class abstracting a gridded structure in a 3d space;
Usueful for having coherent float to integer conversion in a unique place:
Some Notes:
	- bbox is the real occupation of the box in the space;
	- siz is the number of cells for each side

OBJTYPE:      Type of the indexed objects.
SCALARTYPE:   Scalars type for structure's internal data (may differ from
              object's scalar type).

*/
public class BasicGrid {
	
	public Box3 bbox = new Box3();
	
	/// Spatial Dimention (edge legth) of the bounding box
	public Point3 dim = new Point3();
	/// Number of cells forming the grid
	public Point3 siz = new Point3();
	/// Dimensions of a single cell
	public Point3 voxel = new Point3();
	
	/*
	 Derives the right values of Dim and voxel starting
	 from the current values of siz and bbox
	*/
	public final void computeDimAndVoxel()
	{
			this.dim  = this.bbox.max.sub(this.bbox.min);
			this.voxel.x = this.dim.x/this.siz.x;
			this.voxel.y = this.dim.y/this.siz.y;
			this.voxel.z = this.dim.z/this.siz.z;
	}
	
	/* Given a 3D point, returns the coordinates of the cell where the point is
	 * @param p is a 3D point
	 * @return integer coordinates of the cell
	 */
	public final Point3 gridP( Point3 p )
	{
		Point3 pi = new Point3(); 
		PToIP(p, pi);
		return pi;
	}
	
	/* Given a 3D point p, returns the index of the corresponding cell
	 * @param p is a 3D point in the space
	 * @return integer coordinates pi of the cell
	 */
	public final void PToIP(final Point3 p, Point3 pi )
	{
		Point3 t = p.sub(bbox.min);
		pi.x = (int)( t.x / voxel.x );
		pi.y = (int)( t.y / voxel.y );
		pi.z = (int)( t.z / voxel.z );
	}
	
	/* Given a cell index return the lower corner of the cell
	 * @param integer coordinates pi of the cell
	 * @return p is a 3D point representing the lower corner of the cell
	 */
	public void IPToP(final Point3 pi, Point3 p )
	{
		p.x = ((float)pi.x)*voxel.x;
		p.y = ((float)pi.y)*voxel.y;
		p.z = ((float)pi.z)*voxel.z;
		p.add_into(bbox.min);
	}
	
	/* Given a cell in <ScalarType> coordinates, compute the corresponding cell in integer coordinates
	 * @param b is the cell in <ScalarType> coordinates
	 * @return ib is the correspondent box in integer coordinates
	 */
	public final void BoxToIBox(final Box3  b, Box3 ib )
	{
		PToIP(b.min, ib.min);
		PToIP(b.max, ib.max);
		//assert(ib.max[0]>=0 && ib.max[1]>=0 && ib.max[2]>=0);	
	}
	
	/* Given a cell in integer coordinates, compute the corresponding cell in <ScalarType> coordinates
	 * @param ib is the cell in integer coordinates
	 * @return b is the correspondent box in <ScalarType> coordinates
	 */
	/// Given a voxel box in the back ends of the box real
	public final void IBoxToBox( final Box3 ib, Box3 b )
	{
		IPToP(ib.min,b.min);
		IPToP(ib.max,b.max);
	}
	
	/* Calculation grid size
	 * Calculate the size of the grid function
	 * the ratio of the bounding box and the number of elements
	 */
	public final void bestDim( int elems, Point3 size, Point3 dim )
	{
		int mincells   = 1;		// Minimum number of cells
		double GFactor = 1;	// GridEntry = NumElem*GFactor
		double diag = size.norm();	// Diagonal of the box
		double eps  = diag*1e-4;		// 	Tolerance factor

		assert(elems>0);
		assert(size.x>=0.0f);
		assert(size.y>=0.0f);
		assert(size.z>=0.0f);


		int ncell = (int)(elems*GFactor);	// Calcolo numero di voxel
		if(ncell<mincells)
			ncell = mincells;

		dim.x = 1;
		dim.y = 1;
		dim.z = 1;

		if(size.x>eps)
		{
			if(size.y>eps)
			{
				if(size.z>eps)
				{
					double k = Math.pow((double)(ncell/(size.x*size.y*size.z)),(double)(1.0f/3.f));
					dim.x = (int)(size.x * k);
					dim.y = (int)(size.y * k);
					dim.z = (int)(size.z * k);
				} 
				else 
				{
					dim.x = (int)(Math.sqrt(ncell*size.x/size.y));
					dim.y = (int)(Math.sqrt(ncell*size.y/size.x));
				}
			}
			else
			{
				if(size.z>eps)
				{
					dim.x = (int)(Math.sqrt(ncell*size.x/size.z));
					dim.z = (int)(Math.sqrt(ncell*size.z/size.x));
				}
				else
					dim.x = (int)(ncell);
			}
		}
		else
		{
			if(size.y>eps)
			{
				if(size.z>eps)
				{
					dim.y = (int)(Math.sqrt(ncell*size.y/size.z));
					dim.z = (int)(Math.sqrt(ncell*size.z/size.y));
				}
				else
					dim.y = (int)(ncell);
			}
			else if(size.z>eps)
				dim.z = (int)(ncell);
		}
		dim.x = Math.max(dim.x,1);
		dim.y = Math.max(dim.y,1);
		dim.z = Math.max(dim.z,1);
	}
	
}