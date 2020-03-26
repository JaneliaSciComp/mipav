package gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry;

public class Edge {
	public double[][] p = new double[2][2];

	public double Length() {
		double[] d = new double[2];
		d[0] = p[0][0] - p[1][0];
		d[1] = p[0][1] - p[1][1];

		return Math.sqrt(d[0] * d[0] + d[1] * d[1]);
	}
}
