package gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry;

public class Triangle {

	public double[][] p = new double[3][3];

	public final double Area() {
		double[] v1 = new double[3];
		double[] v2 = new double[3];
		double[] v = new double[3];
		for (int d = 0; d < 3; d++) {
			v1[d] = p[1][d] - p[0][d];
			v2[d] = p[2][d] - p[0][d];
		}
		v[0] = v1[1] * v2[2] - v1[2] * v2[1];
		v[1] = -v1[0] * v2[2] + v1[2] * v2[0];
		v[2] = v1[0] * v2[1] - v1[1] * v2[0];
		return Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]) / 2d;
	}

	public final double AspectRatio() {
		double d = 0;
		int i, j;
		for (i = 0; i < 3; i++) {
			for (i = 0; i < 3; i++)
				for (j = 0; j < 3; j++) {
					d += (p[(i + 1) % 3][j] - p[i][j])
							* (p[(i + 1) % 3][j] - p[i][j]);
				}
		}
		return Area() / d;
	}

}