
package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;



public class RunBallPivoting {
	/*
	public static void main(String[] args) {
		float Radius = 4.82f;   //  = par.getAbsPerc("BallRadius");		
	    float Clustering = 10f; //  = par.getFloat("Clustering");		      
	    // float CreaseThr = (float)Math.PI/2f;   //  = math::ToRad(par.getFloat("CreaseThr"));
	    float CreaseThr = ToRad(90f);
		boolean DeleteFaces = true; //  = par.getBool("DeleteFaces");
		MeshModel m = new MeshModel();
		File file = null;
		
		PlyReader reader = new PlyReader();
		reader.readPlyAsciiMesh(m.cm, file);
		
	    if(DeleteFaces) {
					m.cm.fn=0;
					m.cm.face.setSize(0);
	    }
	
	    m.cm.InitFaceIMark();
		m.cm.InitVertexIMark();
		// int startingFn=m.cm.fn;			
		Clustering /= 100.0f;
		BallPivoting pivot = new BallPivoting(m.cm, Radius, Clustering, CreaseThr); 
	    // the main processing
		System.err.println("BuildMesh");
	    pivot.BuildMesh();
	    System.err.println("finish");
	    m.clearDataMask(MeshModel.MM_FACEFACETOPO | MeshModel.MM_FACEFLAGBORDER);
	    System.err.println("m.cm.fn = " + m.cm.fn);
	    
	    PlyWriter writer = new PlyWriter();
	    writer.writePlyAsciiMesh(m.cm);
	}
	*/
	public static float ToRad(float a){return (float)(Math.PI)*a/180.0f;}
	
}