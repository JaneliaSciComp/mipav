
package gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry;

public class Point3D {
		public float[] coords = new float[3];
		
		public Point3D() {
			coords = new float[3];
		}
		
		public Point3D(float x, float y, float z) {
			coords = new float[3];
			coords[0] = x;
			coords[1] = y;
			coords[2] = z;
		}
}
