package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class FrontEdge { 
	      
	  public int v0, v1, v2;   //v0, v1 represent the FrontEdge, v2 the other vertex 
	                    //in the face this FrontEdge belongs to    
	  public int face;         //index of the face             
	  public boolean active; //keep tracks of whether it is in front or in deads
	    
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
         assert(v0 != v1 && v1 != v2 && v0 != v2);
	  }
	 
	  
}  
