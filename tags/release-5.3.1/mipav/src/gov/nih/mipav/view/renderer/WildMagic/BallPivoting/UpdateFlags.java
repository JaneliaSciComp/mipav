package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

import java.util.*;

public class UpdateFlags {
	
	public static final void vertexClearV(TriMesh m) { 
		vertexClear(m,Vertex.VISITED);
	}
	
	public static final void vertexClearB(TriMesh m) { 
		vertexClear(m,Vertex.BORDER);
	}
	
	// int FlagMask = 0xffffffff
	public static final void vertexClear(TriMesh m, int FlagMask)
	{
		if ( FlagMask == -1 ) {
			FlagMask = 0xffffffff;
		}
		Vertex vi;
		int flags;
		int i = 0;
		int andMask = ~FlagMask;
		
		i = 0;
		while ( i < m.vert.size()) {
			vi = m.vert.get(i);
			if(!(vi).isD()) {
				flags = vi.flags();
				flags &= andMask ;
				// (vi).Flags() &= andMask ;
				vi.setFlags(flags);
				
			}
			i++;
			
		}

	}
	
	/// Compute the PerVertex Border flag deriving it from the faces
	public static final void vertexBorderFromFace(TriMesh m)
	{
	  assert(hasPerFaceFlags(m));
		Vertex v;
		Face f;
        int i;
		
		i = 0;
		while ( i < m.vert.size()) {
			v = m.vert.get(i);
			v.clearB();
			i++;
		}
		
			i = 0;
	    	// f = (Face)m.face.firstElement();
		    while ( i < m.face.size() ) {
		    	 f = m.face.get(i);
		    	 if(!(f).IsD())
				 {
		    		 
		    		 for(int z=0;z< f.VN();++z)
				     if( (f).IsB(z) )
				     {
					     f.V(z).setB();
					     f.V(f.Next(z)).setB();          // Ruida,  Next() ?????????????????
				     }
		    		 
				 }
		    	
		    	i++;
		    }
	    
	
	
	}
	
	public static final boolean hasPerFaceFlags(TriMesh m ) {
		return hasMark();
	}
	
	public static final boolean hasMark() {
		return true;
	}
	
	/// Computes per-face border flags without requiring any kind of topology 
	/// It has a O(fn log fn) complexity. 
	public static final void faceBorderFromNone(TriMesh m)
	{
	  assert(hasPerFaceFlags(m));
	  Vector<EdgeSorter> e = new Vector<EdgeSorter>();
	  Iterator<Face> pf;
	  Iterator<EdgeSorter> p;
	  
      Iterator<Vertex> v = m.vert.iterator();
	  while ( v.hasNext() ) {
		  ((Vertex)v.next()).clearB();
	  }
	  
	  if( m.fn == 0 ) 
		return;

	  Iterator<Face> fi = m.face.iterator();
	  int n_edges = 0;
	
	  while ( fi.hasNext()) {
			Face f = fi.next();
			if(! f.IsD()) n_edges+=(f).VN();
	  }
		
	  e.setSize(n_edges);

	  
	  p = e.iterator();
	  EdgeSorter p_elem = e.firstElement();
	  pf = m.face.iterator();	
	  while ( pf.hasNext() ) {
			Face pff = pf.next();
			if( ! (pff).IsD() )
				
				for(int j=0;j<pff.VN();++j)
				{
					p_elem.Set(pff,j);
					pff.ClearB(j);
				    p_elem = p.next();	
				}
		}
		
		assert(p== null);

		Collections.sort(e);
		 
		Iterator<EdgeSorter> pe;
		Iterator<EdgeSorter> ps;
		ps = e.iterator();
		pe = e.iterator();
		EdgeSorter ps_elem = e.firstElement();
		EdgeSorter pe_elem = e.firstElement();
		do
		{	 
			if( !pe.hasNext() ||  pe_elem.notEquals(ps_elem) )
			{
				int value = e.indexOf(pe_elem) - e.indexOf(ps_elem);
				if(value==1) 	{	
						ps_elem.f.SetB(ps_elem.z);
				} else
					if(value!=2)  {  
					for ( ; ps_elem.notEquals(pe_elem); ps_elem = ps.next() ) {
							ps_elem.f.SetB(ps_elem.z); 
					}
				} 
				ps_elem = pe_elem;
			}
			if( !pe.hasNext() ) break;
			pe_elem = pe.next();

		} while(true);
	}
	
	
	class EdgeSorter implements Comparable
	{

		
		private Vertex v[] = new Vertex[2];		// Pointer to the two vertices (Sorted)
		public Face    f;				// Pointer to the face-generating
		public int      z;				// Index dell'edge in the face

	    public EdgeSorter() {} // Nothing to do


	    public void Set( Face pf, int nz )
	    {
			assert(pf!=null);
			assert(nz>=0);
			assert(nz<3);
				
			v[0] = pf.V(nz);
			v[1] = pf.V((nz+1)%3);
			
			assert(!v[0].equals(v[1]));
	
			if( v[0].greaterThan(v[1]) ) {
				Vertex temp;
				temp = v[0];
				v[0] = v[1];
				v[1] = temp;
			}
			f = pf;
			z = nz;
	    }
	    
	    public int compareTo(Object o) {
			EdgeSorter other = (EdgeSorter) o;
			if (lessThan(other))
				return -1;
			else if (equals(other))
				return 0;
			else
				return 1;
		}


		public boolean lessThan( EdgeSorter  pe ) {
			if( v[0].lessThan(pe.v[0]) ) return true;
			else if( v[0].greaterThan(pe.v[0]) ) return false;
			else return v[1].lessThan(pe.v[1]);
		}
		
		
	
		public boolean equals( EdgeSorter pe )
		{
			return v[0] == pe.v[0] && v[1] == pe.v[1];
		}
		
		public boolean notEquals( EdgeSorter pe )
		{
			return v[0] != pe.v[0] || v[1] != pe.v[1];
		}

	}
	
	
}