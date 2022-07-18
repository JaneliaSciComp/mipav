package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class Face {
	// This bit indicate that the face is deleted from the mesh
	public static int DELETED     = 0x00000001;		// cancellato
	// This bit indicate that the face of the mesh is not readable
	public static int NOTREAD     = 0x00000002;		// non leggibile (ma forse modificabile)
	// This bit indicate that the face is not modifiable
	public static int NOTWRITE    = 0x00000004;		// non modificabile (ma forse leggibile) 
	// This bit indicate that the face is modified
	public static int SELECTED    = 0x00000020;		// Selection _flags
	// Border _flags, it is assumed that BORDERi = BORDER0<<i 
	public static int BORDER0     = 0x00000040;
	public static int BORDER1     = 0x00000080;
	public static int BORDER2     = 0x00000100;
	// Face Orientation Flags, used efficiently compute point face distance  
	public static int NORMX		= 0x00000200;
	public static int NORMY		= 0x00000400;
	public static int NORMZ		= 0x00000800;
	// Crease _flags,  it is assumed that FEATUREi = FEATURE0<<i 
	public static int FEATURE0    = 0x00008000;
	public static int FEATURE1    = 0x00010000;
	public static int FEATURE2    = 0x00020000;
	// User bits
	public static int USER0       = 0x00040000;
	public static int USER1       = 0x00080000;
	public static int USER2       = 0x00100000;
	public static int USER3       = 0x00200000;
	
	/// This are the _flags of face, the default value is 0
	public int  _flags;	
	
	private static int b;
	
	public int imark;
	
	public Point3 n = new Point3();
	
	/// Vector of vertex pointer incident in the face
	protected Vertex v[] = new Vertex[3];
	
	protected int v_index[] = new int[3];
	
	// Vector of face pointer, it's used to indicate the adjacency relations (defines if FACE_A is defined)
	protected Face[] _ffp = new Face[3];
	// Index of the face in the arrival face
	protected int[] _ffi = new int[3];
	
	// Vector of pointers to face, used to denote the vertex adjacent face
	protected Face[] _fvp = new Face[3];
	protected int[] _fvi = new int[3];

	// Vector of pointers to face, used to denote the vertex adjacent face
	protected Face[] fs = new Face[3];
	protected int[] zs = new int[3];
	
	protected float _q;                            // Quality
	protected Point3[] _wn = new Point3[3];        // Vertex normal 
	
	
	public int VN() {
		return 3;
	}
	
	public int Prev(final int  i) { return (i+(3-1))%3;}
	public int Next(final int  i) { return (i+1)%3;}
	
	public Face assign(Face f ) {
		/// This are the _flags of face, the default value is 0
	    this._flags = f._flags;	
		this.b = f.b;
		this.imark = f.imark;
		this.n.assign(f.n);
		this.v[0] = f.v[0];
		this.v[1] = f.v[1];
		this.v[2] = f.v[2];
		return this;
		
	}
	
	/// Default Empty Constructor
	public Face() {
		
		_flags=0;
	}
	
	public final void init() {
		_ffp[0] = new Face();
		_ffp[1] = new Face();
		_ffp[2] = new Face();
		
		_fvp[0] = new Face();
		_fvp[1] = new Face();
		_fvp[2] = new Face();
		
		fs[0] = new Face();
		fs[1] = new Face();
		fs[2] = new Face();
		
		_wn[0] = new Point3();
		_wn[1] = new Point3();
		_wn[2] = new Point3();
	}
	
	 /// operator to compare two faces
	public final boolean equals( Face f ) {
		for(int i=0; i<3; ++i)
			if( !V(i).equals(f.V(0)) && !V(i).equals(f.V(1)) && !V(i).equals(f.V(2)) )
				return false;
		return true;
	}
	
	public final Point3 N() {
		return n;
	}
	
	public final void setN(Point3 _n) {
		n.assign(_n);
	}
	
	
	
	/** Return a boolean that indicate if the face is complex.
    @param j Index of the edge
	@return true se la faccia e' manifold, false altrimenti
	 */
	public final boolean IsManifold( Face f, int j ) 
	{
		assert(f.FFp(j) != null); // never try to use this on uncomputed topology
		if( HasFFAdjacency())
			return ( f.cFFp(j) == f || f == f.FFp(j).cFFp(f.FFi(j)) );
		else 
			return true;
	}

	/** Return a boolean that indicate if the j-th edge of the face is a border.
	@param j Index of the edge
	@return true if j is an edge of border, false otherwise
	 */
	public final boolean IsBorder(Face f, int j ) 
	{
		if( HasFFAdjacency())
			return f.FFp(j)==f;
			//return f.IsBorder(j);
  
		// assert(0);
		return true;
	}
	
	public static boolean HasFFAdjacency()  { 
		/*
		if ( __VCGLIB_FACE_AF == 1 || __VCGLIB_FACE_AS == 1))
		  return true;
	    else
		  return false;
		*/
		return true;
	}
	
	public final int getIndex(Vertex vertex) {
		int index = -1;
		for ( int i = 0; i < v.length; i++ ) {
			if ( v[i] == vertex ) {
				index = i;
				return index;
			}
		}
		return index;
	}

	public final Vertex V( int j )
	{	
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 ); 
		assert( (_flags & NOTWRITE) == 0 );
		assert(j >= 0);
		assert(j <  3);
		return v[j];
	}
	
	public final void setV(int j, Vertex vertex) {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 ); 
		assert( (_flags & NOTWRITE) == 0 );
		assert(j >= 0);
		assert(j <  3);
		v[j] = vertex;
	}
	
	public final void setV_index(int j, int idx) {
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 ); 
		assert( (_flags & NOTWRITE) == 0 );
		assert(j >= 0);
		assert(j <  3);
		v_index[j] = idx;
	}
	
	public final int getV_index(int idx) {
		return v_index[idx];
	}
	
	public final Vertex cV( int j )
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert(j>=0);
		assert(j<3);
		return v[j];
	}
	
	public final Point3 P( int j )
	{	
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 ); 
		assert( (_flags & NOTWRITE) == 0 );
		assert(j>=0);
		assert(j<3);
		return v[j].P();
	}
	
	public final Point3 cP( int j )
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert(j>=0);
		assert(j<3);
		return v[j].cP();
	}
	
	/** Return the pointer to the ((j+1)%3)-th vertex of the face.
	@param j Index of the face vertex.
	 */
	public final Vertex V0( int j ) { return V(j);}
	public final Vertex V1( int j ) { return V((j+1)%3);}
	public final Vertex V2( int j ) { return V((j+2)%3);}
	
	public final Vertex cV0( int j ) { return cV(j);}
	public final Vertex cV1( int j ) { return cV((j+1)%3);}
	public final Vertex cV2( int j ) { return cV((j+2)%3);}
	
	public final Point3 P0( int j ) { return V(j).P();}
	public final Point3 P1( int j ) { return V((j+1)%3).P();}
	public final Point3 P2( int j ) { return V((j+2)%3).P();}
	
	public final Point3 cP0( int j ) { return cV(j).P();}
	public final Point3 cP1( int j ) { return cV((j+1)%3).P();}
	public final Point3 cP2( int j ) { return cV((j+2)%3).P();}

	public final Vertex UberV( int j )
	{	
		assert(j>=0 && j < 3);
		return v[j];
	}
	
	public static boolean HasVertexRef()   { return true; }

	
	// support function
	public final void Nexts( Face f,int[] z )
	{
	    int t;
	    t = z[0];
	    z[0] = f.FFi(z[0]);
	    f = f.FFp(t);
	}
	
	
	public static final int LastBitFlag()
	{
		b =USER0;
		return b;
	}

	public static final int NewBitFlag()
	{
		b=b<<1;
		return b;
	}
	
	public static final boolean DeleteBitFlag(int bitval)
	{	
		if(b==bitval) {
				b = b >>1;
				return true;
		}
		// assert(0);
		return false;
	}

	public final void ClearFlags() {_flags=0;}
	
	/// Return the _flags.
	public final int Flags ()
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		return _flags;
	}

	public final  int UberFlags()
	{
		return _flags;
	}
	
	public final void SetFlags(int flagp) {
		_flags=flagp;
	}
	
	/// This function checks if the face is deleted 
	public final boolean IsD()  {return (_flags & DELETED) != 0;}
	/// This function mark the face as deleted
	public final void SetD()		{_flags |=DELETED;}
	/// This function mark the face as not deleted
	public final void ClearD()	{_flags &= (~DELETED);}
	/// This function checks if the face is deleted 
	public final boolean IsDeleted()  {return IsD();}
	
	/// This function checks if the face is readable 
	public final boolean IsR()  {return (_flags & NOTREAD) == 0;}
	/// This function marks the face as readable
	public final void SetR()		{_flags &= (~NOTREAD);}
	/// This function marks the face as not readable
	public final void ClearR() {_flags |=NOTREAD;}
	
	/// This function checks if the face is readable 
	public final boolean IsW()  {return (_flags & NOTWRITE)== 0;}
	/// This function marks the vertex as not writable
	public final void SetW() {_flags &=(~NOTWRITE);}
	/// This function marks the face as not writable
	public final void ClearW() {_flags |=NOTWRITE;}

	/// This funcion checks whether the face is both readable and modifiable
	public final boolean IsRW()  {return (_flags & (NOTREAD | NOTWRITE)) == 0;}

	
	/// This function checks if the face is selected
	public final boolean IsS()  {return (_flags & SELECTED) != 0;}
	/// This function select the face
	public final void SetS()		{_flags |=SELECTED;}
	/// This funcion execute the inverse operation of SetS()
	public final void ClearS()	{_flags &= (~SELECTED);}

	/// This function checks if the face is selected
	public final boolean IsB(int i)  {return (_flags & (BORDER0<<i)) != 0;}
	/// This function select the face
	public final void SetB(int i)		{_flags |=(BORDER0<<i);}
	/// This funcion execute the inverse operation of SetS()
	public final void ClearB(int i)	{_flags &= (~(BORDER0<<i));}

	/// This function checks if the face is Crease  on side i
	public final boolean IsFF(int i)  {return (_flags & (FEATURE0<<i)) != 0;}
	/// This function select the face flag
	public final void SetFF(int i)		{_flags |=(FEATURE0<<i);}
	/// This funcion execute the inverse operation of Set()
	public final void ClearFF(int i)	{_flags &= (~(FEATURE0<<i));}

	/// This function checks if the given user bit is true
	public final boolean IsUserBit(int userBit){return (_flags & userBit) != 0;}
	/// This function set  the given user bit 
	public final void SetUserBit(int userBit){_flags |=userBit;}
	/// This function clear the given user bit 
	public final void ClearUserBit(int userBit){_flags &= (~userBit);}

	public final void GetBBox( Box3 bb ) 
	  {
		
		 if(this.IsD()) 
			{
				bb.setNull();
				return;
			}
		  bb.set(this.P(0));
		  bb.add(this.P(1));
		  bb.add(this.P(2));
	  }

	
	public final int IMark()
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		return imark;
	}
	
	public final void InitIMark()
	{
		imark = 0;
	}
	
	/*
	/// Return the value of the face normal as it correspond to the current geometry.
	/// it is always computed and never stored. 
	public Point3 Normal()
	{
		return Normal(this);
	}

	/// Return the value of the face normal as it correspond to the current geometry.
	/// it is always computed and never stored. 
	public Point3 NormalizedNormal()
	{
		return NormalizedNormal(this);
	}
    */
	
	public static final boolean HasVFAdjacency()  { 
		return true;
	}

	public final Point3 WN( int i)
	{
		return _wn[i];
	}
	
	public final float Q() {
		return _q;
	}
	
	
	public final Face VFp( int j )
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		assert(j>=0);
		assert(j<3);
		return _fvp[j];
	}
	
	public final void setVFp( int j, Face f )
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		assert(j>=0);
		assert(j<3);
		_fvp[j] = f;
	}
	
	/** Return the pointer to the j-th adjacent face.
    @param j Index of the edge.
	 */
	public final Face FFp( int j )
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		assert(j>=0);
		assert(j<3);
		/*
		if (__VCGLIB_FACE_AF ) {
			return _ffp[j];
		}
		else if (__VCGLIB_FACE_AS) {
			return fs[j];
		} else 
			assert(0);
		static Face dum=null; 
		dum+=j;
		return dum;
		*/
		return _ffp[j];
	}
	
	public final void setFFp( int j , Face f)
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		assert(j>=0);
		assert(j<3);
		/*
		if (__VCGLIB_FACE_AF ) {
			return _ffp[j];
		}
		else if (__VCGLIB_FACE_AS) {
			return fs[j];
		} else 
			assert(0);
		static Face dum=null; 
		dum+=j;
		return dum;
		*/
		_ffp[j] = f;
	}

    public final Face cFFp( int j ) {
    	return FFp(j);
    }
    
    
    public final Face cVFp( int j ) {
    	return VFp(j);
    }
    
	
    public final  void setcFFp( int j, Face f ) {
    	 _ffp[j] = f;
    }
	
    
	public final Face FFp1( int j ) { 
		return FFp((j+1)%3);
	}
	
	public final void setFFp1( int j, Face f) { 
		_ffp[(j+1)%3] = f;
	}
	
	public final Face FFp2( int j) { 
		return FFp((j+2)%3);
	}
	
	public final void setFFp2( int j, Face f) { 
		_ffp[(j+2)%3] = f;
	}
	
	/** Return the index that the face have in the j-th adjacent face.
    @param j Index of the edge.
	 */
	public final int FFi( int j )
	{
		assert( (_flags & DELETED) == 0 );
		assert( (_flags & NOTREAD) == 0 );
		assert( (_flags & NOTWRITE) == 0 );
		assert(j>=0);
		assert(j<3);
		return _ffi[j];
		
	}
	
	public final void setFFi( int j, int v ) {
		_ffi[j] = v;
	}
	
	
	
	
}
