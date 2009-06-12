package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

import java.util.*;

public abstract class AdvancingFront {
	public LinkedList<FrontEdge> front = new LinkedList<FrontEdge>();
	public LinkedList<FrontEdge> deads = new LinkedList<FrontEdge>();
	
	public Vector<Integer> nb = new Vector<Integer>(); //number of fronts a vertex is into,
    												   //this is used for the Visited and Border flags
    												   //but adding topology may not be needed anymore
	
	public TriMesh mesh = new TriMesh();        //this structure will be filled by the algorithm
	
	
	public AdvancingFront(TriMesh _mesh) { 
		   mesh = _mesh;
		    
		   UpdateFlags u = new UpdateFlags();
		   
		   u.FaceBorderFromNone(mesh);   
		   u.VertexBorderFromFace(mesh);     

		    nb.clear();
		    // nb.resize(mesh.vert.size(), 0);
		    nb.setSize(mesh.vert.size());
		    for ( int i = 0; i < mesh.vert.size(); i++ ) {
		    	nb.set(i, 0);
		    }
		    
		  
		    CreateLoops();
    }
	
	public abstract float radi();
	
	public void BuildMesh() {
		/// if ( interval == 0 )
		int 	interval = 512;
		for ( int k = 0; k <16; k++ ) {
	   // while(true) {
	       //  System.err.println( "BuildMesh while...");
	      // if(call) call(0, "Advancing front");
	      for(int i = 0; i < interval; i++) {
	            //System.err.println( "BuildMesh " + i);
	        if(front.isEmpty() && !SeedFace()) return;
	        	AddFace();
	      }
		}
	   // }
	 }
	
	//Implement these functions in your subclass 
	// int &v0, int &v1, int &v2
	protected abstract boolean Seed(int[] v0, int[] v1, int[] v2);
	protected abstract int Place(FrontEdge e, FrontEdge[] touch);  
	
	protected boolean CheckFrontEdge(int v0, int v1) {
	    int tot = 0;
	    //HACK to speed up things until i can use a sreach structure
//	    int i = mesh.face.size() - 4*(front.size());
//	    if(front.size() < 100) i = mesh.face.size() - 100;
	    int  i = 0;
	    if(i < 0) i = 0;
	    for(; i < mesh.face.size(); i++) { 
	      Face f = mesh.face.elementAt(i);
	      for(int k = 0; k < 3; k++) {
	        if(v1== f.getIndex(f.V(k)) && v0 == f.getIndex(f.V((k+1)%3)) ) ++tot;
	        else if(v0 == f.getIndex(f.V(k)) && v1 == f.getIndex(f.V((k+1)%3) )) { 
	        	//orientation not constistent
	           return false;
	        }
	      }
	      if(tot >= 2) { //non manifold
	        return false;
	      }
	    }
	    return true;
	  }        
	
	 //create the FrontEdge loops from seed faces
	 protected void CreateLoops() {   
	    Vertex start = mesh.vert.firstElement();
	    for(int i = 0; i < mesh.face.size(); i++) {
	      Face f = mesh.face.elementAt(i);     
	      if(f.IsD()) continue;
	       
	      for(int k = 0; k < 3; k++) {
	        if(f.IsB(k)) {
	          NewEdge(new FrontEdge(f.getIndex(f.V0(k)) - f.getIndex(start), 
	        		                f.getIndex(f.V1(k)) - f.getIndex(start), 
	        		                f.getIndex(f.V2(k)) - f.getIndex(start), 
	        		                i));     
	          int value = nb.get(f.getIndex(f.V0(k)) - f.getIndex(start));
	          value++;
	          nb.set(f.getIndex(f.V0(k)) - f.getIndex(start), value);
	        }          
	      }
	    }
	  
	    Iterator<FrontEdge> s = front.iterator();
	    while ( s.hasNext() ) {
	        FrontEdge elem = s.next();
	    	// elem.previous = null;
	    	// elem.next = null;
	    }
    
	  //now create loops:
	    s = front.iterator();
	    Iterator<FrontEdge> j = front.iterator();
	    while ( s.hasNext() ) {
	    	FrontEdge ss = s.next();
	    	while ( j.hasNext() ) {
	    		FrontEdge jj = j.next();
	    		if(ss == jj) continue;
		        if(ss.v1 != jj.v0) continue;
		        if(jj.previous != null) continue;
		        ss.next = jj;
		        jj.previous = ss;  
		        break;
	    	}
	    }
	    
	    s = front.iterator();  
	    while ( s.hasNext() ) {
	      FrontEdge ss = s.next();
	      assert(ss.next !=  null);
	      assert(ss.previous != null );      
	    }
	    
	  }   
	  
	  protected boolean SeedFace() {
		    int[]  v = new int[3];
		    int[] v00 = new int[1];
		    int[] v11 = new int[1];
		    int[] v22 = new int[1];
		    boolean success = Seed(v00, v11, v22);
            //System.err.println("SeedFace: " + v00[0] + " " +  v11[0] + " " +  v22[0]);
		    v[0] = v00[0];
		    v[1] = v11[0];
		    v[2] = v22[0];
		    if(!success)
		    {
		        System.err.println( "SeedFace return FALSE");
		        return false;
		    }
		    // nb.resize(mesh.vert.size(), 0);
		    nb.setSize(mesh.vert.size());
		    
		     //create the border of the first face  
		    FrontEdge e = ( front.isEmpty() ) ? null: front.getLast();
		    FrontEdge last = e;
		    FrontEdge first = null;
		  
		    for(int i = 0; i < 3; i++) {
		      int v0 = v[i];
		      int v1 = v[((i+1)%3)];
		      int v2 = v[((i+2)%3)];
		  
		      // System.err.println("v0 = " + v0 + " v1 = " + v1 + " v2 = " + v2);
		      
		      mesh.vert.get(v0).SetB();
		      
		      // nb[v[i]]++;
		      int value = nb.get(v[i]);
		      value++;
		      nb.set(v[i], value);
		  
		      
		      // e = front.insert(front.begin(), FrontEdge(v0, v1, v2, mesh.face.size()));
		      front.addFirst(new FrontEdge(v0, v1, v2, mesh.face.size()));
		      e = front.getFirst();
		      
		      if(i != 0) {
		        last.next = e;    
		        e.previous = last;
		      } else
		        first = e;
		  
		      last = e;
		    } 
		    //connect last and first
		    last.next = first;
		    first.previous = last;

            //System.err.println("SAddFace: " + v[0] + " " +  v[1] + " " +  v[2]);
		    AddFace(v[0], v[1], v[2]);
		    return true;
	}
	  
	public boolean AddFace() {
		    if(front.size() == 0) return false; 
		      
		    FrontEdge ei = front.getFirst();
		    FrontEdge current = ei;
		    FrontEdge previous = current.previous;           
		    FrontEdge next = current.next;  
		        
		    int v0 = current.v0, v1 = current.v1;
		    assert(nb.get(v0) < 10 && nb.get(v1) < 10);
		        
		    FrontEdge[] touch = new FrontEdge[1];
		    touch[0] = new FrontEdge();                 // here is the problem   !!!!!!!!!!!!!!
		    
		    int v2 = Place(current, touch);

		    if(v2 == -1) {
		      KillEdge(ei);
		      return false;
		    }
		    
		    assert(v2 != v0 && v2 != v1);  

            if(touch[0].assigned != -1) {  
		      //check for orientation and manifoldness    
		      
		      //touch == current.previous?  
		      if(v2 == previous.v0) {   
		        if(!CheckEdge(v2, v1)) {
		          KillEdge(ei);
		          return false;
		        }       
		          /*touching previous FrontEdge  (we reuse previous)        
		                                    next
		             ------->v2 -----> v1------>
		                      \       /
		                       \     /
		               previous \   / current
		                         \ /
		                          v0           */
		          
		        Detach(v0);
		  
		        FrontEdge up = NewEdge(new FrontEdge(v2, v1, v0, mesh.face.size()));
		        MoveFront(up);
		        up.previous = previous.previous;
		        up.next = current.next;
		        previous.previous.next = up;
		        next.previous = up;
		        Erase(current.previous);
		        Erase(ei);
		        Glue(up);

		      //touch == (*current.next).next         
		      } else if(v2 == next.v1) {    
		        if(!CheckEdge(v0, v2)) {
		          KillEdge(ei);
		          return false;
		        }     
		        /*touching next FrontEdge  (we reuse next)        
		          previous
		             ------->v0 -----> v2------>
		                      \       /
		                       \     /
		                        \   / next
		                         \ /
		                          v1           */      
		    
		        Detach(v1);
		        FrontEdge up = NewEdge(new FrontEdge(v0, v2, v1, mesh.face.size()));
		        MoveFront(up);
		        up.previous = current.previous;
		        up.next = current.next.next;
		        previous.next = up;
		        next.next.previous = up;
		        Erase(current.next);
		        Erase(ei);
		        Glue(up);
		      } else {
		        if(!CheckEdge(v0, v2) || !CheckEdge(v2, v1)) {
		          KillEdge(ei);
		          return false;
		        } 
		      //touching some loop: split (or merge it is local does not matter.
		      //like this 
		      /*                 
		                  left        right
		                <--------v2-<------
		                          /|\
		                         /   \
		                     up /     \ down
		                       /       \
		                      /         V
		                 ----v0 - - - > v1---------
		                        current                         */           
		        FrontEdge left = touch[0];
		        FrontEdge right = touch[0].previous;      
		        
		        //this would be a really bad join
		        if(v1 == right.v0 || v0 == left.v1) {
		          KillEdge(ei);
		          return false;
		        }
		        
		        // nb[v2]++;
		        int v = nb.get(v2);
		        v++;
		        nb.set(v2, v);
		            
		  
		        FrontEdge down = NewEdge(new FrontEdge(v2, v1, v0, mesh.face.size()));      
		        FrontEdge up = NewEdge(new FrontEdge(v0, v2, v1, mesh.face.size()));                            
		      
		        right.next = down;
		        down.previous = right;
		      
		        down.next = current.next;
		        next.previous = down;      
		      
		        left.previous = up;
		        up.next = left;
		  
		        up.previous = current.previous;
		        previous.next = up;
		        Erase(ei);
		      }                         
		              
		      
		    } else {
//		        assert(CheckEdge(v0, v2));
//		        assert(CheckEdge(v2, v1));
		        /*  adding a new vertex
		                 
		                           v2
		                          /|\
		                         /   \
		                     up /     \ down
		                       /       \
		                      /         V
		                 ----v0 - - - > v1--------- */
		        // assert(!mesh.vert[v2].IsB()); //fatal error! a new point is already a border?
		    	assert(!(mesh.vert.get(v2)).IsB());
		        // nb[v2]++;
		    	int v = nb.get(v2);
		    	v++;
		    	nb.set(v2, v);
		    	
		        (mesh.vert.get(v2)).SetB();

		        FrontEdge down = NewEdge(new FrontEdge(v2, v1, v0, mesh.face.size()));
		        FrontEdge up = NewEdge(new FrontEdge(v0, v2, v1, mesh.face.size()));                        
		  
		        down.previous = up;
		        up.next = down;
		        down.next = current.next;
		        next.previous = down;
		        up.previous = current.previous;
		        previous.next = up;
		        Erase(ei);
		      }

            //System.err.println("AddFace: " + v0 + " " +  v1 + " " +  v2);
		      AddFace(v0, v2, v1);
		      return false;
		  }       
	
	 protected void AddFace(int v0, int v1, int v2) {
		    assert(v0 < (int)mesh.vert.size() && v1 < (int)mesh.vert.size() && v2 < (int)mesh.vert.size());  
		    Face face = new Face();
		    // face.V(0) = mesh.vert.get(v0);
		    // face.V(1) = mesh.vert.get(v1);
		    // face.V(2) = mesh.vert.get(v2);
		    
		    face.setV_index(0, v0);
		    face.setV_index(1, v1);
		    face.setV_index(2, v2);
		    
		    face.setV(0, mesh.vert.get(v0));
		    face.setV(1, mesh.vert.get(v1));
		    face.setV(2, mesh.vert.get(v2));
		    
		    ComputeNormalizedNormal(face);
		    // mesh.face.push_back(face);
		    mesh.face.add(face);
		    mesh.fn++;
     }
		    
	 protected void ComputeNormalizedNormal(Face f) {	
		 // f.N() = NormalizedNormal(f);
		 f.setN(NormalizedNoraml(f));
	 }

	 protected Point3 NormalizedNoraml(Face f) {
		 Point3 p0 = f.P(0);
		 Point3 p1 = f.P(1);
		 Point3 p2 = f.P(2);
		 
		 return (( p1.sub(p0)).Cross(p2.sub(p0))).Normalize();
	 }
	 
	 
	 // VertexType &vertex
     protected void AddVertex(Vertex vertex) {
		    Vertex oldstart = null;
		    int old_index;
		    int current_index;
		    int new_index;
		    if(mesh.vert.size() != 0 ) oldstart = mesh.vert.firstElement();
		    // mesh.vert.push_back(vertex);
		    //System.err.println("add vertex");
		    mesh.vert.add(vertex);
		    mesh.vn++;
		    Vertex newstart = mesh.vert.firstElement();
		    if(oldstart != null && oldstart != newstart) { 
		      for(int i = 0; i < mesh.face.size(); i++) {
		        Face face = mesh.face.get(i);
		        for(int k = 0; k < 3; k++) {
		          current_index = face.getIndex(face.V(k));
		          old_index = face.getIndex(oldstart);
		          new_index = face.getIndex(newstart);
		          current_index = new_index + (current_index - old_index);
		          // face.V(k) = newstart + (face.V(k) - oldstart);
		          face.setV(k, face.V(current_index));    //   Addressing  ???????????????????  Ruida
		        }
		      }
		    }
		    // nb.push_back(0);
		    nb.add(0);
	 }
	
     protected boolean CheckEdge(int v0, int v1) {
    	    int tot = 0;
    	    //HACK to speed up things until i can use a seach structure
    	/*    int i = mesh.face.size() - 4*(front.size());
    	    if(front.size() < 100) i = mesh.face.size() - 100;
    	    if(i < 0) i = 0;*/
    	    Vertex vv0 = mesh.vert.get(v0);
    	    Vertex vv1 = mesh.vert.get(v1);    
    	    
    	    for(int i = 0; i < mesh.face.size(); i++) { 
    	      Face f = mesh.face.get(i);
    	      for(int k = 0; k < 3; k++) {
    	        if(vv0 == f.V0(k) && vv1 == f.V1(k))  //orientation not consistent    ruida equals ???
    	           return false;              
    	        else if(vv1 == f.V0(k) && vv0 == f.V1(k)) ++tot;
    	      }
    	      if(tot >= 2) { //non manifold
    	        return false;
    	      }
    	    }
    	    return true;
    	  }    

     //Add a new FrontEdge to the back of the queue
     protected FrontEdge NewEdge(FrontEdge e) {                  
       // return front.insert(front.end(), e);
    	 front.addLast(e);
    	 return front.getLast();
     }     
     
     //move an Edge among the dead ones
     protected void KillEdge(FrontEdge e) {
       e.active = false;
       // deads.splice(deads.getLast(), front, e);
       front.remove(e);
       deads.addLast(e);
     }
     
     protected void Erase(FrontEdge e) {
       if(e.active) front.remove(e);
       else deads.remove(e);
     }
     
     //move an FrontEdge to the back of the queue
     protected void MoveBack(FrontEdge e) {
       // front.splice(front.getLast(), front, e);  
    	 front.remove(e);
    	 front.addLast(e);
     }
     
     protected void MoveFront(FrontEdge e) {
       // front.splice(front.begin(), front, e);
    	 front.remove(e);
    	 front.addFirst(e);
     }
     
     //check if e can be sewed with one of its neighbors
     protected boolean Glue(FrontEdge e) {
       return Glue(e.previous, e) || Glue(e, e.next);
     }
     
   //Glue toghether a and b (where a.next = b
     protected boolean Glue(FrontEdge a, FrontEdge b) {
       if( a.v0 != b.v1) return false; 
       
       FrontEdge previous = a.previous;
       FrontEdge next = b.next;
       previous.next = next;
       next.previous = previous;
       Detach(a.v1);
       Detach(a.v0); 
       Erase(a);
       Erase(b);  
       return true;
     }
     
     protected void Detach(int v) {
       assert(nb.get(v) > 0);
       int value = nb.get(v);
       --value;
       nb.set(v, value);
       if( nb.get(v) == 0) {
         mesh.vert.get(v).ClearB();      
       }
     }      
     
     
     class AdvancingTest extends AdvancingFront {
    	 
    	 public float radi() {
    		 return 0;
    	 }
    	 
        	 public AdvancingTest(TriMesh _mesh) { 
    		  super(_mesh);
        	 }
    	 
    	 // int &v0, int &v1, int &v2
    	 public boolean Seed(int[] v0, int[] v1, int[] v2) {
    		
    	    Vertex[] v = new Vertex[3];
    	    v[0] = new Vertex();
    	    v[1] = new Vertex();
    	    v[2] = new Vertex();
    	    
    	    v[0].setP(new Point3(0, 0, 0));
    	    v[1].setP(new Point3(1, 0, 0));
    	    v[2].setP(new Point3(0, 1, 0));
    	    v[0].ClearFlags();
    	    v[1].ClearFlags();
    	    v[2].ClearFlags();

    	    v0[0] = this.mesh.vert.size();
    	    AddVertex(v[0]);
    	    v1[0] = this.mesh.vert.size();
    	    AddVertex(v[1]);
    	    v2[0] = this.mesh.vert.size();
    	    AddVertex(v[2]);
    	    return true;
    	  }
    	  
    	 // &touch
    	  public int Place(FrontEdge e, FrontEdge[] touch) {
    	     Point3[] p = new Point3[3];
    	     p[0] = new Point3();
    	     p[1] = new Point3();
    	     p[2] = new Point3();
    	     
    	     int k_index;
    	     
    	     p[0] = new Point3((this.mesh.vert.get(e.v0)).P());
    	     p[1] = new Point3((this.mesh.vert.get(e.v1)).P());
    	     p[2] = new Point3((this.mesh.vert.get(e.v2)).P());
    	     
    	     Point3 point = p[0].add(p[1]).sub(p[2]);

    	     int vn = this.mesh.vert.size();
    	     for(int i = 0; i < this.mesh.vert.size(); i++) {
    	       if((  this.mesh.vert.get(i).P().sub(point)).Norm() < 0.1f) {
    	         vn = i;
    	         //find the border
    	         k_index = 0;
    	         assert(((Vertex)(this.mesh.vert.get(i))).IsB());
    		     for(FrontEdge k = this.front.getFirst(); k_index < this.front.size(); k = this.front.get(++k_index))
    				if( k.v0 == i) touch[0] = k;
    		     
    		     k_index = 0;
    		     for(FrontEdge k = this.deads.getFirst(); k_index < this.deads.size(); k = this.deads.get(++k_index))
    			    if( k.v0 == i) touch[0] = k;              
    	         break;
    	       }
    	     }
    	     if(vn == this.mesh.vert.size()) {       
    	       Vertex v = new Vertex();
    	       v.setP(point);
    	       v.ClearFlags();
    	       AddVertex(v);
    	     }
    	     return vn;
    	  }
    	}
        
     
}
