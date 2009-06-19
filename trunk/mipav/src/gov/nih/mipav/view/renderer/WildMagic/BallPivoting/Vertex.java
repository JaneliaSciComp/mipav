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
	
	public Vertex copy() {
		Vertex v = new Vertex();
		v._p.assign(this._p);
		v._n.assign(this._n);
		v.b = this.b;
		v.imark = this.imark;
		v._flags = this._flags;
		return v;
	}
	
	/// Return the spatial coordinate of the vertex
	public Point3 P()
	{
	  assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		return _p;
	}
	
	public void setP(Point3 p) {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		_p.assign(p);
	}
	
	/// Return the constant spatial coordinate of the vertex
	public Point3 cP()
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		return _p;
	}
	
	
	
	public int Flags ()
	{
			assert( (_flags & DELETED) == 0 );
			assert( (_flags & NOTREAD) == 0 );
			return _flags;
	}
	
	public void setFlags(int flags ) {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
	    _flags = flags;
	}
	
	
	///  checks if the vertex is deleted
	public boolean IsD() {return (_flags & DELETED) != 0;}
	///  checks if the vertex is readable
	public boolean IsR() {return (_flags & NOTREAD) == 0;}
	///  checks if the vertex is modifiable
	public boolean IsW() {return (_flags & NOTWRITE)== 0;}
	/// This funcion checks whether the vertex is both readable and modifiable
	public boolean IsRW() {return (_flags & (NOTREAD | NOTWRITE)) == 0;}
	///  checks if the vertex is Modified
	public boolean IsS() {return (_flags & SELECTED) != 0;}
	///  checks if the vertex is readable
	public boolean IsB() {return (_flags & BORDER) != 0;}
	///  checks if the vertex is visited
	public boolean IsV() {return (_flags & VISITED) != 0;}


	/** Set the flag value
		@param flagp Valore da inserire nel flag
	*/
	public void SetFlags(int flagp) {_flags=flagp;}


	/** Set the flag value
	@param flagp Valore da inserire nel flag
	*/
	public void ClearFlags() {_flags=0;}
	
	///  deletes the vertex from the mesh
	public void SetD() {_flags |=DELETED;}
	///  un-delete a vertex
	public void ClearD() {_flags &=(~DELETED);}
	///  marks the vertex as readable
	public void SetR() {_flags &=(~NOTREAD);}
	///  marks the vertex as not readable
	public void ClearR() {_flags |=NOTREAD;}
	///  marks the vertex as writable
	public void ClearW() {_flags |=NOTWRITE;}
	///  marks the vertex as not writable
	public void SetW() {_flags &=(~NOTWRITE);}
	///  select the vertex
	public void SetS()		{_flags |=SELECTED;}
	/// Un-select a vertex
	public void ClearS()	{_flags &= ~SELECTED;}
	/// Set vertex as ob border
	public void SetB()		{_flags |=BORDER;}
	public void ClearB()	{_flags &=~BORDER;}
	///  checks if the vertex is visited
	public void ClearV()	{_flags &= ~VISITED;}
	///  checks if the vertex is visited
	public void SetV()		{_flags |=VISITED;}
	
	///  Return the first bit that is not still used
	public static int LastBitFlag()
	{
				b =USER0;
				return b;
	}

	/// allocate a bit among the flags that can be used by user.
	public static int NewBitFlag()
	{
		b=b<<1;
		return b;
	}
	// de-allocate a bit among the flags that can be used by user.
	public static boolean DeleteBitFlag(int bitval) {
		if (b == bitval) {
			b = b >> 1;
			return true;
		}
		// assert(0);
		return false;
	}
	
	/// This function checks if the given user bit is true
	public boolean IsUserBit(int userBit){return (_flags & userBit) != 0;}
	/// This function set  the given user bit 
	public void SetUserBit(int userBit){_flags |=userBit;}
	/// This function clear the given user bit 
	public void ClearUserBit(int userBit){_flags &= (~userBit);}
	
	
	/*#*******************	
	*  Bounding box *
	**********************/
	public void GetBBox( Box3 bb ) 
	{
		bb.Set( cP() );
	}
	
	
	/***********************************************/
	/** @name Vertex Incremental Mark
	   blah
	   blah
	 **/
	/// This function return the vertex incremental mark
	public int  IMark()
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		return imark;
	}

	/// Initialize the _imark system of the vertex
	public void InitIMark()
	{
		imark = 0;
	}
	
	// & operator [] 
	public float get(int i) {
		return P().V().get(i);
	}
	
	public boolean equals(Vertex ve) {
		return _p.equals(ve._p);
	}
	
	/// Operator to compare two vertices using lexicographic order
	// operator < 
	public boolean lessThan (Vertex ve) {
		return _p.lessThan(ve._p);
	}
	
	/// Operator to compare two vertices using lexicographic order
	public boolean lessEqualThan (Vertex ve) {
		return _p.lessEqualThan(ve._p);
	}
	
	public boolean greaterThan (Vertex ve) {
		return _p.greaterThan(ve._p);
	}
	
	
	 /***********************************************/
	 /** @name Vertex Normal 
	 **/
	public static boolean HasNormal() {
		return true;
	}
	
	public Point3 N() {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		return _n;
	}
	
	public void setN(Point3 nv) {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		_n.assign(nv);
	}
	
	
	public Face cVFp() 
	{
		  return _vfb;
	}
	
	public Face VFp()
	{
		  return _vfb;
	}
	
	public int VFi() {
		return _vfi;
	}
	
	protected Face _vfb;
	protected int _vfi;
	
	/*#*******************	
	*  Bounding box *
	**********************/
    /*
	public void GetBBox( Box3 bb ) 
	{
		bb.Set( cP() );
	}
	*/
}