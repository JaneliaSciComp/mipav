package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class Vertex {
	
	protected Point3 _p = new Point3();
	
	public static int b;
	
	public int imark;
	
	// This bit indicate that the vertex is deleted from the mesh
	public static int DELETED    = 0x0001;		// cancellato
	// This bit indicate that the vertex of the mesh is not readable
	public static int NOTREAD    = 0x0002;		// non leggibile (ma forse modificabile) 
	// This bit indicate that the vertex is not modifiable
	public static int NOTWRITE   = 0x0004;		// non modificabile (ma forse leggibile) 
	// This bit indicate that the vertex is modified
	public static int MODIFIED   = 0x0008;		// modificato 
	// This bit can be used to mark the visited vertex
	public static int VISITED    = 0x0010;		// Visited  
	// This bit can be used to select 
	public static int SELECTED   = 0x0020;		// Selection flag
	// Border Flag
	public static int BORDER     = 0x0100;
	// First user bit
	public static int USER0      = 0x0200;			// Fisrt user bit
	
	protected Face _vfb;
	protected int _vfi;
	
	
	/** @name Vertex Flags
	For each vertex we store a set of boolean values packed in a int. 
	The default value for each flag is 0. Most commonly used flags are the \a deleted and the \a selected ones. 
	Users can ask and dispose for a bit for their own purposes with the  vcg::VertexFull::NewUserBit() and vcg::VertexFull::DeleteUserBit() functions. 
	The value returned by these functions has to be passed to the 
	vcg::VertexFull::SetUserBit() vcg::VertexFull::ClearUserBit() and vcg::VertexFull::IsUserBit() functions to check and modify the obtained bit flag.
	**/
	/// This are the flags of vertex, the default (reasonable) value is 0
	public int _flags;	

	protected Point3 _n = new Point3();
	
	public final Vertex copy() {
		Vertex v = new Vertex();
		v._p.assign(this._p);
		v._n.assign(this._n);
		v.b = this.b;
		v.imark = this.imark;
		v._flags = this._flags;
		return v;
	}
	
	/// Return the spatial coordinate of the vertex
	public final  Point3 P()
	{
	  assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		return _p;
	}
	
	public final void setP(Point3 p) {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		_p.assign(p);
	}
	
	/// Return the constant spatial coordinate of the vertex
	public final Point3 cP()
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		return _p;
	}
	
	
	
	public final int flags ()
	{
			assert( (_flags & DELETED) == 0 );
			assert( (_flags & NOTREAD) == 0 );
			return _flags;
	}
	
	public final void setFlags(int flags ) {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
	    _flags = flags;
	}
	
	
	///  checks if the vertex is deleted
	public final boolean isD() {return (_flags & DELETED) != 0;}
	///  checks if the vertex is readable
	public final boolean isR() {return (_flags & NOTREAD) == 0;}
	///  checks if the vertex is modifiable
	public final boolean isW() {return (_flags & NOTWRITE)== 0;}
	/// This funcion checks whether the vertex is both readable and modifiable
	public final boolean isRW() {return (_flags & (NOTREAD | NOTWRITE)) == 0;}
	///  checks if the vertex is Modified
	public final boolean isS() {return (_flags & SELECTED) != 0;}
	///  checks if the vertex is readable
	public final boolean isB() {return (_flags & BORDER) != 0;}
	///  checks if the vertex is visited
	public final boolean isV() {return (_flags & VISITED) != 0;}


	public final void clearFlags() {_flags=0;}
	
	///  deletes the vertex from the mesh
	public final void setD() {_flags |=DELETED;}
	///  un-delete a vertex
	public final void clearD() {_flags &=(~DELETED);}
	///  marks the vertex as readable
	public final void setR() {_flags &=(~NOTREAD);}
	///  marks the vertex as not readable
	public final void clearR() {_flags |=NOTREAD;}
	///  marks the vertex as writable
	public final void clearW() {_flags |=NOTWRITE;}
	///  marks the vertex as not writable
	public final void setW() {_flags &=(~NOTWRITE);}
	///  select the vertex
	public final void setS()		{_flags |=SELECTED;}
	/// Un-select a vertex
	public final void clearS()	{_flags &= ~SELECTED;}
	/// Set vertex as ob border
	public final void setB()		{_flags |=BORDER;}
	public final void clearB()	{_flags &=~BORDER;}
	///  checks if the vertex is visited
	public final void clearV()	{_flags &= ~VISITED;}
	///  checks if the vertex is visited
	public final void setV()		{_flags |=VISITED;}
	
	///  Return the first bit that is not still used
	public static final int lastBitFlag()
	{
				b =USER0;
				return b;
	}

	/// allocate a bit among the flags that can be used by user.
	public static final int newBitFlag()
	{
		b=b<<1;
		return b;
	}
	// de-allocate a bit among the flags that can be used by user.
	public static final boolean deleteBitFlag(int bitval) {
		if (b == bitval) {
			b = b >> 1;
			return true;
		}
		// assert(0);
		return false;
	}
	
	/// This function checks if the given user bit is true
	public final boolean isUserBit(int userBit){return (_flags & userBit) != 0;}
	/// This function set  the given user bit 
	public final void setUserBit(int userBit){_flags |=userBit;}
	/// This function clear the given user bit 
	public final void clearUserBit(int userBit){_flags &= (~userBit);}
	
	
	/*#*******************	
	*  Bounding box *
	**********************/
	public final void getBBox( Box3 bb ) 
	{
		bb.set( cP() );
	}
	
	
	/***********************************************/
	/** @name Vertex Incremental Mark
	   blah
	   blah
	 **/
	/// This function return the vertex incremental mark
	public final int  iMark()
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		return imark;
	}

	/// Initialize the _imark system of the vertex
	public final void initIMark()
	{
		imark = 0;
	}
	
	// & operator [] 
	public final float get(int i) {
		return P().V().get(i);
	}
	
	public final boolean equals(Vertex ve) {
		return _p.equals(ve._p);
	}
	
	/// Operator to compare two vertices using lexicographic order
	// operator < 
	public final boolean lessThan (Vertex ve) {
		return _p.lessThan(ve._p);
	}
	
	/// Operator to compare two vertices using lexicographic order
	public final boolean lessEqualThan (Vertex ve) {
		return _p.lessEqualThan(ve._p);
	}
	
	public final boolean greaterThan (Vertex ve) {
		return _p.greaterThan(ve._p);
	}
	
	
	 /***********************************************/
	 /** @name Vertex Normal 
	 **/
	public static final boolean hasNormal() {
		return true;
	}
	
	public final Point3 N() {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		return _n;
	}
	
	public final void setN(Point3 nv) {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		_n.assign(nv);
	}
	
	
	public final Face cVFp() 
	{
		  return _vfb;
	}
	
	public final Face VFp()
	{
		  return _vfb;
	}
	
	public final int VFi() {
		return _vfi;
	}
	
	
}