package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class FrontEdge { 
	      
	  public int v0, v1, v2;   //v0, v1 represent the FrontEdge, v2 the other vertex 
	                    //in the face this FrontEdge belongs to    
	  public int face;         //index of the face             
	  public boolean active; //keep tracks of whether it is in front or in deads
	    
	  //the loops in the front are maintained as a double linked list
	  // ListIterator<FrontEdge> next;            
	  // ListIterator<FrontEdge> previous;
	  public FrontEdge next; // new FrontEdge();            
	  public FrontEdge previous; // new FrontEdge();
	  public int assigned;
	  
	  public FrontEdge() {
		  assigned = -1;
	  }
	  
	  public FrontEdge(int _v0, int _v1, int _v2, int _face) { 
         v0 = _v0;
         v1 = _v1;
         v2 = _v2;
         face = _face;
         active = true; 
         // next = new FrontEdge();
         // previous = new FrontEdge();
         assert(v0 != v1 && v1 != v2 && v0 != v2);
	  }
	  
	  /*
	  public void assign(FrontEdge e) {
		  this.v0 = e.v0;
		  this.v1 = e.v1;
		  this.v2 = e.v2;
		  this.face = e.face;
		  this.active = e.active;
		  this.next = e.next;
		  this.previous = e.previous;
	  }
	  */
	  /*
	  public boolean equals(FrontEdge temp) {
		  return ( v0 == temp.v0 &&
				   v1 == temp.v1 &&
				   v2 == temp.v2 &&
				   face == temp.face &&
				   active == temp.active &&
				   next == temp.next &&
		           previous == temp.previous);
	  }
	  */
}  
