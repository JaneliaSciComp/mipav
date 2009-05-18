package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class Link implements Comparable {
	
	

		/// Costruttore di default
		public Link(){};
		/// Costruttore con inizializzatori
		public  Link(Vertex nt, int ni ){
			assert(ni>=0);
			t = nt;
			i = ni;
		};

		
	    public int compareTo(Object o) {
            Link other = (Link) o;
	        if (lessThan(other))
	            return -1;
	        else if ( other.lessThan(this) )
	            return 1;
	        else
	            return 0;
	    }
		
		public boolean lessThan ( Link  l ) { return i <   l.i; } 
		public boolean lessThan ( Object r, Object  l ) { return ((Link)r).i <   ((Link)l).i; } 
		public boolean lessEqualThan ( Link  l ) { return i <=  l.i; }
		public boolean greaterThan  ( Link  l ) { return i >   l.i; }
		public boolean greaterEqualThan ( Link  l ) { return i >=  l.i; }
		public boolean equals ( Link  l ) { return i ==  l.i; }
		public boolean notequals ( Link  l ) { return i !=  l.i; }

		public Vertex  Elem() {
			return t;
		}

		// ObjType operator *(){return *(t);}

		public Vertex get() {
			return t;
		}
		
		public  int  Index() {
			return i;
		}

		public void assign(Link in) {
			t = in.t;
			i = in.i;
		}
		

		/// Puntatore all'elemento T
		private Vertex t = null;   
		/// Indirizzo del voxel dentro la griglia
		public int i;

}