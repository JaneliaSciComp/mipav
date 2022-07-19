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

		
	    public final int compareTo(Object o) {
            Link other = (Link) o;
	        if (lessThan(other))
	            return -1;
	        else if ( other.lessThan(this) )
	            return 1;
	        else
	            return 0;
	    }
		
		public final boolean lessThan ( Link  l ) { return i <   l.i; } 
		public final boolean lessThan ( Object r, Object  l ) { return ((Link)r).i <   ((Link)l).i; } 
		public final boolean lessEqualThan ( Link  l ) { return i <=  l.i; }
		public final boolean greaterThan  ( Link  l ) { return i >   l.i; }
		public final boolean greaterEqualThan ( Link  l ) { return i >=  l.i; }
		public final boolean equals ( Link  l ) { return i ==  l.i; }
		public final boolean notequals ( Link  l ) { return i !=  l.i; }

		public final Vertex  elem() {
			return t;
		}

	
		public final Vertex get() {
			return t;
		}
		
		public final int  index() {
			return i;
		}

		public final void assign(Link in) {
			t = in.t;
			i = in.i;
		}
		

		private Vertex t = null;   
		public int i;

}