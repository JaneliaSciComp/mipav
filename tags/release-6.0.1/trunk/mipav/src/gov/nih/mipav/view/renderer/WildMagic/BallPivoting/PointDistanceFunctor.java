package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class PointDistanceFunctor {
	
	public PointDistanceFunctor() {}
	
	public static boolean call(Vertex v, Point3 p, float[] minDist, Point3 q) 
	{
		// convert the coordinates of p from SCALARTYPE to VERTEXTYPE::ScalarType type
		// Point3 fp = new Point3(p);

		float md;		// distance between v and fp
		md = (v.P().sub(p)).norm();

		if (md <= minDist[0]) 
		{
			minDist[0] = (float)(md);		// minDist is updated to the closest distance
			// q = new Point3(v.P());						// q is the current closest point
            q.assign(v.P());
			return true;
		}

		return false;
	}
	
}