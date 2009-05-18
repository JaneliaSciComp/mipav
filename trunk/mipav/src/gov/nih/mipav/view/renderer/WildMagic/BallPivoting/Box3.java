package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

/* 
Templated class for 3D boxes.
  This is the class for definition of a axis aligned bounding box in 3D space. It is stored just as two Point3
	@param BoxScalarType (template parameter) Specifies the type of scalar used to represent coords.
*/
public class Box3 {
	
	/// min coordinate point
	public Point3 min = new Point3();
	
	/// max coordinate point
	public Point3 max = new Point3();
	
	/// The bounding box constructor
	public Box3() {
		min.x = 1f;
		max.x = -1f;
		min.y = 1f;
		max.y = -1f;
		min.z = 1f;
		max.z = -1f;
	}
	
	/// Copy constructor
	public Box3(Box3 b) {
		min.assign(b.min);
		max.assign(b.max);
	}
	
	public void assign(Box3 b) {
		min.assign(b.min);
		max.assign(b.max);
	}
	
	/// Min Max constructor
	public Box3(Point3 mi, Point3 ma) {
		min.assign(mi);
		max.assign(ma);
	}
	
	/// Point Radius Constructor
	public Box3(Point3 center, float radius ) {
		min = center.sub(new Point3(radius, radius, radius));
		max = center.add(new Point3(radius, radius, radius));
	}
	
	public boolean equals(Box3 p) {
		return min.equals(p.min) && max.equals(p.max);
	}
	
	public boolean noteqauls(Box3 p) {
		return min.notequals(p.min) || max.notequals(p.max);
	}
	
	public void Offset( float s )
	{
		Offset( new Point3(s,s,s));
	}
	
	public void Offset(Point3 delta )
	{
		min.sub_into(delta);
		max.add_into(delta);
	}
	
	/// Initializing the bounding box
	public void Set( Point3 p )
	{
		min.assign(p); 
		max.assign(p);
	}
	
	public void SetNull()
	{
		min.x= 1f; max.x= -1f;
		min.y= 1f; max.y= -1f;
		min.z= 1f; max.z= -1f;
	}
	
	public void Add( final Box3 b )
	{
		if(b.IsNull()) return; // Adding a null bbox should do nothing
		if(IsNull()) this.assign(b);
		else
		{
			if(min.x > b.min.x) min.x = b.min.x;
			if(min.y > b.min.y) min.y = b.min.y;
			if(min.z > b.min.z) min.z = b.min.z;
		
			if(max.x < b.max.x) max.x = b.max.x;
			if(max.y < b.max.y) max.y = b.max.y;
			if(max.z < b.max.z) max.z = b.max.z;
		}
	}
	
	public void Add( final Point3 p )
	{
		if(IsNull()) Set(p);
		else 
		{
			if(min.x > p.x) min.x = p.x;
			if(min.y > p.y) min.y = p.y;
			if(min.z > p.z) min.z = p.z;

			if(max.x < p.x) max.x = p.x;
			if(max.y < p.y) max.y = p.y;
			if(max.z < p.z) max.z = p.z;
		}
	}
	
	/*
	public void Add( final Matrix44 m, final Box3 b )
	{
			final Point3 mn = new Point3();
			mn.assign(b.min);
			final Point3 mx = new Point3();
			mx.assign(b.max);
			
			Add(m.mul(new Point3(mn.x,mn.y,mn.z)));
			Add(m.mul(new Point3(mx.x,mn.y,mn.z)));
			Add(m.mul(new Point3(mn.x,mx.y,mn.z)));
			Add(m.mul(new Point3(mx.x,mx.y,mn.z)));
			Add(m.mul(new Point3(mn.x,mn.y,mx.z)));
			Add(m.mul(new Point3(mx.x,mn.y,mx.z)));
			Add(m.mul(new Point3(mn.x,mx.y,mx.z)));
			Add(m.mul(new Point3(mx.x,mx.y,mx.z)));
	}
	*/
	public void Intersect( final Box3 b )
	{
		if(min.x < b.min.x) min.x = b.min.x;
		if(min.y < b.min.y) min.y = b.min.y;
		if(min.z < b.min.z) min.z = b.min.z;

		if(max.x > b.max.x) max.x = b.max.x;
		if(max.y > b.max.y) max.y = b.max.y;
		if(max.z > b.max.z) max.z = b.max.z;

		if(min.x>max.x || min.y>max.y || min.z>max.z) SetNull();
	}
	
	public void Translate( final Point3 p )
	{
		min.add_into(p);
		max.add_into(p);
	}
	
	public final boolean IsIn( final Point3 p )
	{
		return (
			min.x <= p.x && p.x <= max.x &&
			min.y <= p.y && p.y <= max.y &&
			min.z <= p.z && p.z <= max.z
		);
	}
	
	public final boolean IsInEx( final Point3 p )
	{
		return (
			min.x <= p.x && p.x < max.x &&
			min.y <= p.y && p.y < max.y &&
			min.z <= p.z && p.z < max.z
		);
	}
	
	public final boolean Collide(final Box3 b)
	{
		return b.min.x<max.x && b.max.x>min.x &&
			   b.min.y<max.y && b.max.y>min.y &&
			   b.min.z<max.z && b.max.z>min.z ;
	}
	
	public boolean IsNull() { 
		return min.x>max.x || min.y>max.y || min.z>max.z; 
	}
	
	public final boolean IsEmpty() { 
		return min.equals(max); 
	}
	
	public final float Diag()
	{
		return Distance(min,max);
	}
	
	public float SquaredDiag()
	{
		return SquaredDistance(min,max);
	}
	
	public final Point3 Center()
	{
		return (min.add(max)).div(2f);
	}
	
	

	public float Distance( Point3 p1, Point3 p2 )
	{
	    return (p1.sub(p2)).Norm();
	}

	public float SquaredDistance( Point3 p1, Point3 p2 )
	{
	    return (p1.sub(p2)).SquaredNorm();
	}
	
	
	
	
	public final Point3 Dim()
	{
		return max.sub(min);
	}
	
	public final Point3 LocalToGlobal(final Point3 p) {
		return new Point3( 
			min.x + p.x*(max.x-min.x), 
			min.y + p.y*(max.y-min.y),
			min.z + p.z*(max.z-min.z));
	}
	
	public final Point3 GlobalToLocal(final Point3 p) {
		return new Point3( 
		  (p.x-min.x)/(max.x-min.x), 
		  (p.y-min.y)/(max.y-min.y), 
		  (p.z-min.z)/(max.z-min.z)
			);
	}
	
	public float Volume()
	{
		return (max.x-min.x)*(max.y-min.y)*(max.z-min.z);
	}
	
	public final float DimX() { return max.x-min.x;}

    public final float DimY() { return max.y-min.y;}
	
    public final float DimZ() { return max.z-min.z;}
	
    public final int MaxDim() { 
		int i;
		Point3 diag = max.sub(min);
		if(diag.x>diag.y) i=0; else i=1;
		// return (diag[i]>diag.z)? i: 2;
		if ( i == 0 ) {
			return (diag.x>diag.z)? i: 2;
		} else { // i == 1
			return (diag.y>diag.z)? i: 2;
		}
	}
    
    public final int MinDim() { 
		int i;
		Point3 diag =  max.sub(min);
		if(diag.x<diag.y) i=0; else i=1;
		// return (diag[i]<diag[2])? i: 2;
		if ( i == 0 ) {
			return (diag.x<diag.z)? i: 2;
		} else {
			return (diag.y<diag.z)? i: 2;
		}
	}
    /*
    public void Import( final Box3 b )
	{
		min.Import(b.min);
		max.Import(b.max);
	}
    
    public static final Box3 Construct( final Box3 b )
	{
      return new Box3(Point3.Construct(b.min),Point3.Construct(b.max));
	}
    */
    public final Point3 P(final int i) {
		return new Point3(
			min.x+ (i%2) * DimX(),
			min.y+ ((i / 2)%2) * DimY(),
			min.z+ ((i>3)? 1 : 0) * DimZ());
    }
    
    
    
}