package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

import java.util.*;

public class TriMesh {
	
	/// Set of vertices 
	public Vector<Vertex> vert = new Vector<Vertex>();
	/// Real number of vertices
	public int vn;
	/// Set of faces
	public Vector<Face> face = new Vector<Face>();
	/// Real number of faces
	public int fn;
	/// Bounding box of the mesh
	public Box3 bbox = new Box3();
	
	/// The incremental mark
	public int imark;
	
	public TriMesh() {
		fn = 0;
		vn = 0;
		imark = 0;
	}
	
	public void setVertex( Vector<Point3> _vert){
		// vert = _vert;
		vn = _vert.size();
		vert.setSize(vn);
		for ( int i = 0; i < vn; i++ ) {
			float x, y, z;
			x = ((Point3)_vert.get(i)).x;
			y = ((Point3)_vert.get(i)).y;
			z = ((Point3)_vert.get(i)).z;
			Vertex v = new Vertex();
			v.setP(new Point3(x, y, z));
			vert.set(i, v);
		}
	}
	
	public int simplexNumber(){ 
		return fn;
	}
	
	public int vertexNumber(){ 
		return vn;
	}
	
	/// Initialize the imark-system of the faces
	public final void initFaceIMark()
	{
		Face f = null;
		int f_index = 0;
		if ( face.size() > 0 ) {
			for(f_index = 0;f_index <face.size();++f_index)
				f = face.get(f_index);
				if( !f.IsD() && f.IsR() && f.IsW() )
					f.InitIMark();
		}
	}

	/// Initialize the imark-system of the vertices
	public final void initVertexIMark()
	{
		Vertex vi = null;
        int v_index = 0;
		for(v_index = 0 ;v_index < vert.size();++v_index)
			vi = vert.get(v_index);
			if( !vi.isD() && vi.isRW() )
				vi.initIMark();
	}
	
	
	/** Access function to the incremental mark. 
	*/
	public final int iMark(){return imark;}
	/** Check if the vertex incremental mark matches the one of the mesh. 
		@param v Vertex pointer
	*/
	public final boolean isMarked( Vertex  v ) { return v.imark == imark; }
	/** Check if the face incremental mark matches the one of the mesh. 
		@param v Face pointer
	*/
	public final boolean isMarked( Face f ) { return f.imark == imark; }
	/** Set the vertex incremental mark of the vertex to the one of the mesh.
		@param v Vertex pointer
	*/
	public final void mark( Vertex v ) { v.imark = imark; }
	/** Set the face incremental mark of the vertex to the one of the mesh.
		@param v Vertex pointer
	*/
	public final void mark( Face f ) { f.imark = imark; }
	/// Unmark the mesh
	public final void unMarkAll() { ++imark; }
	
	public final boolean hasPerVertexNormal()  { return Vertex.hasNormal() ; }
	
}