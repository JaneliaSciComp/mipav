package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;



import java.util.*;

/* Ball pivoting algorithm:
1) the vertices used in the new mesh are marked as visited
2) the border vertices of the new mesh are marked as border
3) the vector nb is used to keep track of the number of borders a vertex belongs to
4) usedBit flag is used to select the points in the mesh already processed

*/   

public class BallPivoting extends AdvancingFront {
  
	public float radius;          // radius of the ball
	public float min_edge;        // min length of an edge
	public float max_edge;        // max length of an edge
	public float max_angle;       // max angle between 2 faces (cos(angle)
									// actually)
	
	private int last_seed;     // used for new seeds when front is empty
	private int usedBit;       // use to detect if a vertex has been already processed.
	private Point3 baricenter; // used for the first seed.
	private StaticGrid grid = new StaticGrid();  // lookup grid for points
	
	public float radi() { return radius; }        
	
	
	Vector<Vertex> left = new Vector<Vertex>();
	Vector<Vertex> leftUp = new Vector<Vertex>();
	Vector<Vertex> leftDown = new Vector<Vertex>();
	Vector<Vertex> right = new Vector<Vertex>();
	Vector<Vertex> rightUp = new Vector<Vertex>();
	Vector<Vertex> rightDown = new Vector<Vertex>();
	
	
	// MESH _mesh, float _radius = 0, float minr = 0.2, float angle = M_PI/2
	public BallPivoting(TriMesh _mesh, float _radius,  float minr, float angle) { 
	
		super(_mesh);

        radius = _radius; 
        min_edge = minr; 
		max_edge = 1.8f;
        max_angle = (float)Math.cos(angle);
		last_seed = -1;
		
		
		// compute bbox
		baricenter = new Point3(0, 0, 0);
		
		// UpdateBounding<TriMesh>.Box(_mesh);
		this.mesh.bbox.setNull();
		int i = 0;
		Vertex vi;
		Iterator<Vertex> vt = mesh.vert.iterator();
		while ( vt.hasNext() ) {
			  vi = vt.next();
          	  if( !vi.isD() ) {
          		  this.mesh.bbox.add(vi.cP()); 
          		  baricenter.add_into(vi.P());
          	  }
		}
		
		System.err.println("Test");
		baricenter.div_into(this.mesh.vn);
		
		
		/*
		vt = mesh.vert.iterator();
	
		System.err.println("mesh.vert.size = " + mesh.vert.size());
		
		while ( vt.hasNext() ) {
			  vi = vt.next();
			  if ( vi.P().x < baricenter.x ) {
				  left.add(vi.copy());
			  } else {
				  right.add(vi.copy());
			  }
		}
		
		vt = left.iterator();
		while ( vt.hasNext() ) {
			vi = vt.next();
			if ( vi.P().y < baricenter.y ) {
				  leftDown.add(vi.copy());
			} else {
				  leftUp.add(vi.copy());
			}   	  
		}
		
		vt = right.iterator();
		while ( vt.hasNext() ) {
			vi = vt.next();
			if ( vi.P().y < baricenter.y ) {
				  rightDown.add(vi.copy());
			} else {
				  rightUp.add(vi.copy());
			}   	  
		}
		
		
		System.err.println("leftUp.size = " + leftUp.size());
		System.err.println("leftDown.size = " + leftDown.size());
		System.err.println("rightUp.size = " + rightUp.size());
		System.err.println("rightDown.size = " + rightDown.size());
		System.err.println("total = " + (leftUp.size()+leftDown.size()+rightUp.size()+rightDown.size()));		
		
		
		this.mesh.vert.removeAllElements();		
	
		
		vt = leftUp.iterator();
		while ( vt.hasNext() ) {
			vi = vt.next();
			this.mesh.vert.add(vi);
		}
		
		
		vt = leftDown.iterator();
		while ( vt.hasNext() ) {
			vi = vt.next();
			this.mesh.vert.add(vi);
		}
		
		
		vt = rightUp.iterator();
		while ( vt.hasNext() ) {
			vi = vt.next();
			this.mesh.vert.add(vi);
		}
		
		
	    
		vt = rightDown.iterator();
		while ( vt.hasNext() ) {
			vi = vt.next();
			this.mesh.vert.add(vi);
		}
        */
		
		assert(this.mesh.vn > 3);
		if(radius == 0)
		{
		    // radius ==0 means that an auto guess should be attempted.
		    radius = (float)Math.sqrt((this.mesh.bbox.diag()*this.mesh.bbox.diag())/this.mesh.vn);
		    System.err.println( radius );
		    radius += (radius/1000.0f);
		    System.err.println( radius );
		}
		

        //radius = 0.902f;
        //min_edge = .3f;
        //max_angle = 0;
		
		min_edge *= radius;
		max_edge *= radius;    

		// enlarging the bbox for out-of-box queries
		Box3 BPbbox = this.mesh.bbox;
		BPbbox.offset(4*radius);
		// This function automatically compute a reasonable size for the uniform grid providing the side (radius) of the cell
		// Note that the bbox must be already 'inflated' so to be sure that no object will fall on the border of the grid.
		grid.set(BPbbox, this.mesh.vert);              // Ruida, need more checks ??????

		// mark visited points
		Vector<Vertex> targets = new Vector<Vertex>();      
		Vector<Point3> points = new Vector<Point3>();      
		Vector<Float> dists = new Vector<Float>();

		Vertex.lastBitFlag();
		usedBit = Vertex.newBitFlag();
		for(i = 0; i < (int)this.mesh.vert.size(); i++) {
			vi = this.mesh.vert.elementAt(i);
			vi.clearUserBit(usedBit);
		}
		
		UpdateFlags.vertexClearV(this.mesh);
		

		for(i = 0; i < this.mesh.face.size(); i++) {
			Face f = this.mesh.face.elementAt(i);
			if(f.IsD()) continue;
			for(int k = 0; k < 3; k++) {
				f.V(k).setV();
				int n = getInSphereVertex(this.mesh, grid, f.V(k).P(), min_edge, targets, dists, points);  // Ruida, Check???????
				for(int t = 0; t < n; t++) {
					targets.elementAt(t).setUserBit(usedBit);
					assert(targets.elementAt(t).isUserBit(usedBit));
				}
				assert(f.V(k).isUserBit(usedBit));
			}
		}    
	}
	
	public void dispose() {
		Vertex.deleteBitFlag(usedBit);
	}
	
	
	public final int getInSphereVertex(TriMesh mesh, StaticGrid gr, Point3 _p, float _r, Vector<Vertex> _objectPtrs, Vector<Float> _distances, Vector<Point3> _points) {
		VertTmark mv = new VertTmark();
		mv.setMesh(mesh);
		PointDistanceFunctor fn = new PointDistanceFunctor();
		return (gr.getInSphere/*<VDistFunct,MarkerVert,OBJPTRCONTAINER,DISTCONTAINER,POINTCONTAINER>*/
				(fn, mv,_p,_r,_objectPtrs,_distances,_points));
	}
	
	
	// int &v0, int &v1, int &v2
	public final boolean seed(int[] v0, int[] v1, int[] v2) {               
		// boolean use_normals = false;     
		//get a sphere of neighbours
		Vector<Vertex> targets = new Vector<Vertex>();      
		Vector<Point3> points = new Vector<Point3>();      
		Vector<Float> dists = new Vector<Float>();
		while( ++last_seed < this.mesh.vert.size()) {
	        //System.err.println( "Seed: " + last_seed);
			Vertex seed = this.mesh.vert.get(last_seed);
			if(seed.isD() || seed.isUserBit(usedBit)) continue;                      

			seed.setUserBit(usedBit);       

			int n = getInSphereVertex(this.mesh, grid, seed.P(), 2*radius, targets, dists, points);
			if(n < 3) {      
				continue;
			}                     

            Vertex vv0 = null, vv1 = null , vv2 = null;
			//for(int i = 0; i < n; i++) {
			//    vv0 = targets.get(i);
			//    System.err.println( "           tagets[" +i+"]   " + vv0._p.x + " " + vv0._p.y + " " + vv0._p.z );
            //}


			boolean success = true;
			//find the closest visited or boundary
			for(int i = 0; i < n; i++) {         
				Vertex v = targets.get(i);
				if(v.isV()) {        
					success = false;
					break;
				}
			}
			if(!success) continue;

			success = false;
			//find a triplet that does not contains any other point
			Point3 center = new Point3();
			for(int i = 0; i < n; i++) {
				vv0 = targets.get(i);
	            //System.err.println( "           vv0 " + i + " " + vv0._p.x + " " + vv0._p.y + " " + vv0._p.z );
				if(vv0.isD()) continue;
				Point3 p0 = vv0.P();        

				for(int k = i+1; k < n; k++) {
					vv1 = targets.get(k);            
		            //System.err.println( "            vv1 "  + k + " "+ vv1._p.x + " " + vv1._p.y + " " + vv1._p.z );
					if(vv1.isD()) continue;
					Point3 p1 = vv1.P();      
					float d2 = (p1.sub(p0)).norm();    
					if(d2 < min_edge || d2 > max_edge) continue;

					for(int j = k+1; j < n; j++) {
						vv2 = targets.get(j);
			            //System.err.println( "             vv2 "  + j + " " + vv2._p.x + " " + vv2._p.y + " " + vv2._p.z );
						if(vv2.isD()) continue;
						Point3 p2 = vv2.P();            
						float d1 = (p2.sub(p0)).norm();
						if(d1 < min_edge || d1 > max_edge) continue;            
						float d0 = (p2.sub(p1)).norm();
						if(d0 < min_edge || d0 > max_edge) continue;

						Point3 normal = (p1.sub(p0)).cross(p2.sub(p0));
						if(normal.dot(p0.sub(baricenter)) < 0) continue;
						/*            if(use_normals) {             
						if(normal * vv0->N() < 0) continue;
						if(normal * vv1->N() < 0) continue;
						if(normal * vv2->N() < 0) continue;
						}*/

						if(!findSphere(p0, p1, p2, center)) {
							continue;
						}
						//System.err.println( "CENTER  " + center.x + " " + center.y + " " + center.z );
                        
						//check no other point inside
						int t;
						for(t = 0; t < n; t++) {
                            Point3 kTP = targets.get(t).P();
                            //System.err.println( "TP " + kTP.x + " " + kTP.y + " " + kTP.z );
                            Point3 kPointDiff = center.sub(kTP);
                            //System.err.println( "vEC DIFF " + kPointDiff.x + " " + kPointDiff.y + " " + kPointDiff.z );
                            float Norm = kPointDiff.norm();
						    //float fDiff = radius - Norm;
						    //System.err.println( "NORM " + Norm + " radius " + radius + " diff " + fDiff );
						    //int iNorm = (int)(Norm * 1000000);
						    //Norm = iNorm/1000000.0f;
                            if ( Norm <= radius )
                            {
                                //System.err.println( "         BREAK " + i + " " + k + " " + j + " " + t );
                                
                                //System.err.println( "                center " + center.x + " " + center.y + " " + center.z );
                                //System.err.println( "                target " + targets.get(t).P().x + " " + targets.get(t).P().y + " " + targets.get(t).P().z );
                                //System.err.println( "                Norm " + center.sub(targets.get(t).P()).Norm() + " radius " + radius );
                                 
							//if(center.sub(targets.get(t).P()).Norm() <= radius)
								break;
                            }
						}
						if(t < n) {
							continue;                         
						}

						//check on the other side there is not a surface
						// Point3 opposite = center.add(normal.mul(((center.sub(p0)).dot(normal)) * 2/normal.SquaredNorm()));
						
						Point3 opposite =    center.add(   normal.mul( ( (center.sub(p0)).dot(normal) ) * 2 / normal.squaredNorm() ) );
						
						for(t = 0; t < n; t++) {
							Vertex v = targets.get(t);
							if((v.isV()) && (opposite.sub(v.P())).norm() <= radius) 
								break;              
						}
						if(t < n) {
							continue;                         
						}
						success = true;
						i = n;
						k = n;
						j = n;
					}
				}
			}

			if(!success) { //see bad luck above
				continue;
			}
            //System.err.println( "ball_pivoting " + vv0._p.x + " " + vv0._p.y + " " + vv0._p.z );
            //System.err.println( "ball_pivoting " + vv1._p.x + " " + vv1._p.y + " " + vv1._p.z );
            //System.err.println( "ball_pivoting " + vv2._p.x + " " + vv2._p.y + " " + vv2._p.z );
	        
			mark(vv0);
			mark(vv1);
			mark(vv2);            

			// Ruida, redundant addressing subtraction, check ??????????????????
			// v0[0] = this.mesh.vert.indexOf(vv0) - this.mesh.vert.indexOf(this.mesh.vert.firstElement());
			// v1[0] = this.mesh.vert.indexOf(vv1) - this.mesh.vert.indexOf(this.mesh.vert.firstElement());
			// v2[0] = this.mesh.vert.indexOf(vv2) - this.mesh.vert.indexOf(this.mesh.vert.firstElement());
	        v0[0] = this.mesh.vert.indexOf(vv0);
	        v1[0] = this.mesh.vert.indexOf(vv1);
	        v2[0] = this.mesh.vert.indexOf(vv2);

	        //System.err.println( "Seed: " + v0[0] + " " + v0[0] + " " + v2[0]);
			
	        //System.err.println("ruida exit");
			return true;      
		}
		
		System.err.println("out of while");
		return false;    
	}
	
	//select a new vertex, mark as Visited and mark as usedBit all neighbours (less than min_edge)
	// FrontEdge &edge, std::list<FrontEdge>::iterator &touch
	public final int place(FrontEdge edge, FrontEdge[] touch) {
		Point3 v0 = ((this.mesh.vert.get(edge.v0))).P();
		Point3 v1 = ((this.mesh.vert.get(edge.v1))).P();  
		Point3 v2 = ((this.mesh.vert.get(edge.v2))).P();  
		/* TODO why using the face normals everything goes wrong? should be
		exactly the same................................................

		Point3x &normal = mesh.face[edge.face].N(); ?
		*/

		Point3 normal = ((v1.sub(v0)).cross(v2.sub(v0))).normalize();        
		Point3 middle = (v0.add(v1)).div(2f);    
		Point3 center = new Point3();    

		if(!findSphere(v0, v1, v2, center)) {
			//      assert(0);
			return -1;
		}

		Point3 start_pivot = center.sub(middle);          
		Point3 axis = (v1.sub(v0));

		float axis_len = axis.squaredNorm();
		if(axis_len > 4*radius*radius) {
			return -1;
		}
		axis.normalize();

		// r is the radius of the thorus of all possible spheres passing through v0 and v1
		float r = (float)Math.sqrt(radius*radius - axis_len/4f);

		Vector<Vertex> targets = new Vector<Vertex>();
		Vector<Float> dists = new Vector<Float>();    
		Vector<Point3> points = new Vector<Point3>();

		// int n = GetInSphereVertex(this.mesh, grid, middle, r + radius, targets, dists, points);
		
		getInSphereVertex(this.mesh, grid, middle, r + radius, targets, dists, points);

		if(targets.size() == 0) {
			return -1; //this really would be strange but one never knows.
		}

		Vertex candidate = null;
		float min_angle = (float)Math.PI;

		for(int i = 0; i < targets.size(); i++) {      
			Vertex v = targets.get(i);
			// int id = v - (Vertex)this.mesh.vert.getFirst();
			// int id =  this.mesh.vert.indexOf(v) - this.mesh.vert.indexOf(this.mesh.vert.firstElement());
	        int id = this.mesh.vert.indexOf(v);
			
			if(v.isD()) continue; 

			// this should always be true IsB => IsV , IsV => IsU
			if(v.isB()) assert(v.isV());
			if(v.isV()) assert(v.isUserBit(usedBit));


			if(v.isUserBit(usedBit) && !(v.isB())) continue;
			if(id == edge.v0 || id == edge.v1 || id == edge.v2) continue;

			Point3 p = (this.mesh.vert.get(id)).P();

			/* Find the sphere through v0, p, v1 (store center on end_pivot */
			if(!findSphere(v0, p, v1, center)) {
				continue;      
			}

			/* Angle between old center and new center */
			float alpha = angle(start_pivot, center.sub(middle), axis);

			/* adding a small bias to already chosen vertices.
			doesn't solve numerical problems, but helps. */
			//          if(this->mesh.vert[id].IsB()) alpha -= 0.001;

			/* Sometimes alpha might be little less then M_PI while it should be 0,
			by numerical errors: happens for example pivoting 
			on the diagonal of a square. */

			/*      if(alpha > 2*M_PI - 0.8) {               
			// Angle between old center and new *point* 
			//TODO is this really overshooting? shouldbe enough to alpha -= 2*M_PI
			Point3x proj = p - axis * (axis * p - axis * middle);
			ScalarType beta = angle(start_pivot, proj - middle, axis);

			if(alpha > beta) alpha -= 2*M_PI; 
			} */
			if(candidate == null || alpha < min_angle) {
				candidate = v;                // shallow
				min_angle = alpha;
			} 
		}
		if(min_angle >= (float)Math.PI - 0.1f) {
			return -1;
		}

		if(candidate == null) {
			return -1;
		}
		if(!candidate.isB()) {
			assert((candidate.P().sub(v0)).norm() > min_edge);
			assert((candidate.P().sub(v1)).norm() > min_edge);    
		}

		// int id = this.mesh.vert.indexOf(candidate) - this.mesh.vert.indexOf(this.mesh.vert.firstElement());
	    int id = this.mesh.vert.indexOf(candidate);
		
		assert(id != edge.v0 && id != edge.v1);

		Point3 newnormal = ((candidate.P().sub(v0)).cross(v1.sub(v0))).normalize();
		float fDot = normal.dot(newnormal);
		int iVal = (Integer)(this.nb.get(id));
		if( fDot < max_angle || iVal >= 2) {
			return -1;
		}

		//test if id is in some border (to return touch
		int k_index = 0;
		FrontEdge k = null;
		for(k_index = 0; k_index < this.front.size(); k_index++) {
			k = this.front.get(k_index);
			// if ( touch == null ) touch = new FrontEdge();
			if(k.v0 == id) { 
				touch[0] = k;
				touch[0].assigned = 1;
			}
		}
		
		
		for(k_index = 0; k_index < this.deads.size(); k_index++) {
			k = this.deads.get(k_index);
			// if ( touch == null ) touch = new FrontEdge();
			if(k.v0 == id) { 
				touch[0] = k;
				touch[0].assigned = 1;
			}
		}
		
		//mark vertices close to candidate
		mark(candidate);
		return id;
	}
	
	/* returns the sphere touching p0, p1, p2 of radius r such that
	the normal of the face points toward the center of the sphere */

	// Point3x &p0, Point3x &p1, Point3x &p2, Point3x &center
	private final boolean findSphere(Point3 p0, Point3 p1, Point3 p2, Point3 center) {
		//we want p0 to be always the smallest one.
		Point3[] p = new Point3[3];
		p[0] = new Point3();
		p[1] = new Point3();
		p[2] = new Point3();
		

		if(p0.lessThan(p1) && p0.lessThan(p2)) {
			p[0].assign(p0);
			p[1].assign(p1);
			p[2].assign(p2);          
		} else if(p1.lessThan(p0) && p1.lessThan(p2)) {
			p[0].assign(p1);
			p[1].assign(p2);
			p[2].assign(p0);
		} else {
			p[0].assign(p2);
			p[1].assign(p0);
			p[2].assign(p1);
		}
		Point3 q1 = p[1].sub(p[0]);
		Point3 q2 = p[2].sub(p[0]);  

		Point3 up = q1.cross(q2);
		float uplen = up.norm();

		//the three points are aligned
		if(uplen < 0.001f*q1.norm()*q2.norm()) {
			return false;
		}
		up.div_into(uplen);


		float a11 = q1.dot(q1);
		float a12 = q1.dot(q2);
		float a22 = q2.dot(q2);

		float m = 4*(a11*a22 - a12*a12);
		float l1 = 2*(a11*a22 - a22*a12)/m;
		float l2 = 2*(a11*a22 - a12*a11)/m;

		Point3 centerTemp = q1.mul(l1).add(q2.mul(l2));
		float circle_r = centerTemp.norm();
		if(circle_r > radius) {
		    center.assign(centerTemp);
			return false; //need too big a sphere
		}

		float height = (float)Math.sqrt(radius*radius - circle_r*circle_r);
		centerTemp.add_into(p[0].add(up.mul(height)));

        center.assign(centerTemp);
		return true;
	}         

	/* compute angle from p to q, using axis for orientation */
	// Point3x p, Point3x q, Point3x &axis
	private float angle(Point3 p, Point3 q, Point3 axis) {
		p.normalize();
		q.normalize();
		Point3 vec = p.cross(q);
		float angle = (float)Math.acos(p.dot(q));
		if(vec.dot(axis) < 0) angle = -angle;
		if(angle < 0) angle += 2f*Math.PI;
		return angle;
	}          

	// VertexType *v
	private void mark(Vertex v) {
		Vector<Vertex> targets = new Vector<Vertex>();      
		Vector<Point3> points = new Vector<Point3>();      
		Vector<Float> dists = new Vector<Float>();       
		int n = getInSphereVertex(this.mesh, grid, v.P(), min_edge, targets, dists, points);
		for(int t = 0; t < n; t++) 
			(targets.get(t)).setUserBit(usedBit);
		v.setV();
	}
	
	
}