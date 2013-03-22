package gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry;

import java.util.*;

public class Geometry {

	int RAND_MAX = 0x7ffff;

	public float Random() {
		return (float) (Math.random()) / RAND_MAX;
	}

	public Point3D RandomBallPoint() {
		Point3D p = new Point3D();
		while (true) {
			p.coords[0] = (float) (1.0f - 2.0f * Random());
			p.coords[1] = (float) (1.0f - 2.0f * Random());
			p.coords[2] = (float) (1.0f - 2.0f * Random());
			double l = SquareLength(p);
			if (l <= 1) {
				return p;
			}
		}
	}

	public Point3D RandomSpherePoint() {
		Point3D p = RandomBallPoint();
		float l = (float) (Length(p));
		p.coords[0] /= l;
		p.coords[1] /= l;
		p.coords[2] /= l;
		return p;
	}

	public double SquareLength(final Point3D p) {
		return p.coords[0] * p.coords[0] + p.coords[1] * p.coords[1]
				+ p.coords[2] * p.coords[2];
	}

	public double Length(final Point3D p) {
		return Math.sqrt(SquareLength(p));
	}

	public double SquareDistance(final Point3D p1, final Point3D p2) {
		return (p1.coords[0] - p2.coords[0]) * (p1.coords[0] - p2.coords[0])
				+ (p1.coords[1] - p2.coords[1]) * (p1.coords[1] - p2.coords[1])
				+ (p1.coords[2] - p2.coords[2]) * (p1.coords[2] - p2.coords[2]);
	}

	public double Distance(final Point3D p1, final Point3D p2) {
		return Math.sqrt(SquareDistance(p1, p2));
	}

	public void CrossProduct(final Point3D p1, final Point3D p2, Point3D p) {
		p.coords[0] = p1.coords[1] * p2.coords[2] - p1.coords[2] * p2.coords[1];
		p.coords[1] = -p1.coords[0] * p2.coords[2] + p1.coords[2] * p2.coords[0];
		p.coords[2] = p1.coords[0] * p2.coords[1] - p1.coords[1] * p2.coords[0];
	}

	public void EdgeCollapse(final float edgeRatio, Vector<TriangleIndex> triangles,
			Vector<Point3D> positions, Vector<Point3D> normals) {
		int i, j;
		int[] remapTable;
		int[] pointCount;
		int[] idx = new int[3];
		Point3D[] p = new Point3D[3];
		Point3D[] q = new Point3D[2];
		Point3D c = new Point3D();
		double[] d = new double[3];
		double a;
		double Ratio = 12.0 / Math.sqrt(3.0); // (Sum of Squares Length /
												// Area) for and equilateral
												// triangle

		for (i = 0; i < 3; i++) {
			p[i] = new Point3D();
			if (i < 2)
				q[i] = new Point3D();
		}

		remapTable = new int[positions.size()];
		pointCount = new int[positions.size()];
		for (i = 0; i < (int) (positions.size()); i++) {
			remapTable[i] = i;
			pointCount[i] = 1;
		}
		for (i = (int) (triangles.size() - 1); i >= 0; i--) {
			for (j = 0; j < 3; j++) {
				idx[j] = triangles.get(i).idx[j];
				while (remapTable[idx[j]] < idx[j]) {
					idx[j] = remapTable[idx[j]];
				}
			}
			if (idx[0] == idx[1] || idx[0] == idx[2] || idx[1] == idx[2]) {
				triangles.set(i, triangles.get(triangles.size() - 1));
				triangles.remove(triangles.size() - 1);
				continue;
			}
			for (j = 0; j < 3; j++) {
				p[j].coords[0] = positions.get(idx[j]).coords[0]
						/ pointCount[idx[j]];
				p[j].coords[1] = positions.get(idx[j]).coords[1]
						/ pointCount[idx[j]];
				p[j].coords[2] = positions.get(idx[j]).coords[2]
						/ pointCount[idx[j]];
			}
			for (j = 0; j < 3; j++) {
				q[0].coords[j] = p[1].coords[j] - p[0].coords[j];
				q[1].coords[j] = p[2].coords[j] - p[0].coords[j];
				d[j] = SquareDistance(p[j], p[(j + 1) % 3]);
			}
			CrossProduct(q[0], q[1], c);
			a = Length(c) / 2;

			if ((d[0] + d[1] + d[2]) * edgeRatio > a * Ratio) {
				// Find the smallest edge
				j = 0;
				if (d[1] < d[j]) {
					j = 1;
				}
				if (d[2] < d[j]) {
					j = 2;
				}

				int idx1, idx2;
				if (idx[j] < idx[(j + 1) % 3]) {
					idx1 = idx[j];
					idx2 = idx[(j + 1) % 3];
				} else {
					idx2 = idx[j];
					idx1 = idx[(j + 1) % 3];
				}
				positions.get(idx1).coords[0] += positions.get(idx2).coords[0];
				positions.get(idx1).coords[1] += positions.get(idx2).coords[1];
				positions.get(idx1).coords[2] += positions.get(idx2).coords[2];
				if (normals != null) {
					normals.get(idx1).coords[0] += normals.get(idx2).coords[0];
					normals.get(idx1).coords[1] += normals.get(idx2).coords[1];
					normals.get(idx1).coords[2] += normals.get(idx2).coords[2];
				}
				pointCount[idx1] += pointCount[idx2];
				remapTable[idx2] = idx1;
				triangles.set(i, triangles.get(triangles.size() - 1));
				triangles.remove(triangles.size() - 1);
			}
		}
		int pCount = 0;
		for (i = 0; i < (int) (positions.size()); i++) {
			for (j = 0; j < 3; j++) {
				positions.get(i).coords[j] /= pointCount[i];
			}
			if (normals != null) {
				float l = (float) (Length(normals.get(i)));
				for (j = 0; j < 3; j++) {
					normals.get(i).coords[j] /= l;
				}
			}
			if (remapTable[i] == i) { // If vertex i is being used
				positions.set(pCount, positions.get(i));
				if (normals != null) {
					normals.set(pCount, normals.get(i));
				}
				pointCount[i] = pCount;
				pCount++;
			}
		}
		int pos = positions.size();
		positions.setSize(pCount);

		// if pCount is greater than positions size, allocate the memory for the
		// new added positions.
		if (pCount > pos) {
			for (i = pos - 1; i < pCount; i++) {
				positions.set(i, new Point3D());
			}
		}

		for (i = (int) (triangles.size() - 1); i >= 0; i--) {
			for (j = 0; j < 3; j++) {
				idx[j] = triangles.get(i).idx[j];
				while (remapTable[idx[j]] < idx[j]) {
					idx[j] = remapTable[idx[j]];
				}
				triangles.get(i).idx[j] = pointCount[idx[j]];
			}
			if (idx[0] == idx[1] || idx[0] == idx[2] || idx[1] == idx[2]) {
				triangles.set(i, triangles.get(triangles.size() - 1));
				triangles.remove(triangles.size() - 1);
			}
		}

		pointCount = null;
		remapTable = null;
	}

	public void TriangleCollapse(final float edgeRatio, Vector<TriangleIndex> triangles,
			Vector<Point3D> positions, Vector<Point3D> normals) {
		int i, j;
		int[] remapTable;
		int[] pointCount;
		int[] idx = new int[3];
		Point3D[] p = new Point3D[3];
		Point3D[] q = new Point3D[2];
		Point3D c = new Point3D();
		double[] d = new double[3];
		double a;
		double Ratio = 12.0d / Math.sqrt(3.0); // (Sum of Squares Length /
												// Area) for and equilateral
												// triangle

		for (i = 0; i < 3; i++) {
			p[i] = new Point3D();
			if (i < 2)
				q[i] = new Point3D();
		}

		remapTable = new int[positions.size()];
		pointCount = new int[positions.size()];
		for (i = 0; i < (int) (positions.size()); i++) {
			remapTable[i] = i;
			pointCount[i] = 1;
		}
		for (i = (int) (triangles.size() - 1); i >= 0; i--) {
			for (j = 0; j < 3; j++) {
				idx[j] = triangles.get(i).idx[j];
				while (remapTable[idx[j]] < idx[j]) {
					idx[j] = remapTable[idx[j]];
				}
			}
			if (idx[0] == idx[1] || idx[0] == idx[2] || idx[1] == idx[2]) {
				triangles.set(i, triangles.get(triangles.size() - 1));
				triangles.remove(triangles.size() - 1);
				continue;
			}
			for (j = 0; j < 3; j++) {
				p[j].coords[0] = positions.get(idx[j]).coords[0]
						/ pointCount[idx[j]];
				p[j].coords[1] = positions.get(idx[j]).coords[1]
						/ pointCount[idx[j]];
				p[j].coords[2] = positions.get(idx[j]).coords[2]
						/ pointCount[idx[j]];
			}
			for (j = 0; j < 3; j++) {
				q[0].coords[j] = p[1].coords[j] - p[0].coords[j];
				q[1].coords[j] = p[2].coords[j] - p[0].coords[j];
				d[j] = SquareDistance(p[j], p[(j + 1) % 3]);
			}
			CrossProduct(q[0], q[1], c);
			a = Length(c) / 2;

			if ((d[0] + d[1] + d[2]) * edgeRatio > a * Ratio) {
				// Find the smallest edge
				j = 0;
				if (d[1] < d[j]) {
					j = 1;
				}
				if (d[2] < d[j]) {
					j = 2;
				}

				int idx1, idx2, idx3;
				if (idx[0] < idx[1]) {
					if (idx[0] < idx[2]) {
						idx1 = idx[0];
						idx2 = idx[2];
						idx3 = idx[1];
					} else {
						idx1 = idx[2];
						idx2 = idx[0];
						idx3 = idx[1];
					}
				} else {
					if (idx[1] < idx[2]) {
						idx1 = idx[1];
						idx2 = idx[2];
						idx3 = idx[0];
					} else {
						idx1 = idx[2];
						idx2 = idx[1];
						idx3 = idx[0];
					}
				}
				positions.get(idx1).coords[0] += positions.get(idx2).coords[0]
						+ positions.get(idx3).coords[0];
				positions.get(idx1).coords[1] += positions.get(idx2).coords[1]
						+ positions.get(idx3).coords[1];
				positions.get(idx1).coords[2] += positions.get(idx2).coords[2]
						+ positions.get(idx3).coords[2];
				if (normals != null) {
					normals.get(idx1).coords[0] += normals.get(idx2).coords[0]
							+ normals.get(idx3).coords[0];
					normals.get(idx1).coords[1] += normals.get(idx2).coords[1]
							+ normals.get(idx3).coords[1];
					normals.get(idx1).coords[2] += normals.get(idx2).coords[2]
							+ normals.get(idx3).coords[2];
				}
				pointCount[idx1] += pointCount[idx2] + pointCount[idx3];
				remapTable[idx2] = idx1;
				remapTable[idx3] = idx1;
				triangles.set(i, triangles.get(triangles.size() - 1));
				triangles.remove(triangles.size() - 1);
			}
		}
		int pCount = 0;
		for (i = 0; i < (int) (positions.size()); i++) {
			for (j = 0; j < 3; j++) {
				positions.get(i).coords[j] /= pointCount[i];
			}
			if (normals != null) {
				float l = (float) (Length(normals.get(i)));
				for (j = 0; j < 3; j++) {
					normals.get(i).coords[j] /= l;
				}
			}
			if (remapTable[i] == i) { // If vertex i is being used
				positions.set(pCount, positions.get(i));
				if (normals != null) {
					normals.set(pCount, normals.get(i));
				}
				pointCount[i] = pCount;
				pCount++;
			}
		}

		int pos = positions.size();

		positions.setSize(pCount);

		// if pCount is greater than positions size, allocate the memory for the
		// new added positions.
		if (pCount > pos) {
			for (i = pos - 1; i < pCount; i++) {
				positions.set(i, new Point3D());
			}
		}

		for (i = (int) (triangles.size() - 1); i >= 0; i--) {
			for (j = 0; j < 3; j++) {
				idx[j] = triangles.get(i).idx[j];
				while (remapTable[idx[j]] < idx[j]) {
					idx[j] = remapTable[idx[j]];
				}
				triangles.get(i).idx[j] = pointCount[idx[j]];
			}
			if (idx[0] == idx[1] || idx[0] == idx[2] || idx[1] == idx[2]) {
				triangles.set(i, triangles.get(triangles.size() - 1));
				triangles.remove(triangles.size() - 1);
			}
		}
		pointCount = null;
		remapTable = null;
	}


}