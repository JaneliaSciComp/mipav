package gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry;

import java.util.HashMap;
import java.util.Vector;


public class Triangulation {
	public Vector<Point3D> points = new Vector<Point3D>();
	public Vector<TriangulationEdge> edges = new Vector<TriangulationEdge>();
	public Vector<TriangulationTriangle> triangles = new Vector<TriangulationTriangle>();
	protected HashMap<Long, Integer> edgeMap = new HashMap<Long, Integer>();

	public int factor(final int tIndex, int[] p1, int[] p2, int[] p3) {
		if (triangles.get(tIndex).eIndex[0] < 0
				|| triangles.get(tIndex).eIndex[1] < 0
				|| triangles.get(tIndex).eIndex[2] < 0) {
			return 0;
		}
		if (edges.get(triangles.get(tIndex).eIndex[0]).tIndex[0] == tIndex) {
			p1[0] = edges.get(triangles.get(tIndex).eIndex[0]).pIndex[0];
		} else {
			p1[0] = edges.get(triangles.get(tIndex).eIndex[0]).pIndex[1];
		}

		if (edges.get(triangles.get(tIndex).eIndex[1]).tIndex[0] == tIndex) {
			p2[0] = edges.get(triangles.get(tIndex).eIndex[1]).pIndex[0];
		} else {
			p2[0] = edges.get(triangles.get(tIndex).eIndex[1]).pIndex[1];
		}

		if (edges.get(triangles.get(tIndex).eIndex[2]).tIndex[0] == tIndex) {
			p3[0] = edges.get(triangles.get(tIndex).eIndex[2]).pIndex[0];
		} else {
			p3[0] = edges.get(triangles.get(tIndex).eIndex[2]).pIndex[1];
		}
		return 1;
	}

	public double area() {
		double a = 0;
		for (int i = 0; i < (int) (triangles.size()); i++) {
			a += area(i);
		}
		return a;
	}

	public double area(final int tIndex) {
		int[] p1 = new int[1];
		int[] p2 = new int[1];
		int[] p3 = new int[1];
		factor(tIndex, p1, p2, p3);
		return area(p1[0], p2[0], p3[0]);
	}

	public double area(final int p1, final int p2, final int p3) {
		Point3D q1 = new Point3D();
		Point3D q2 = new Point3D();
		Point3D q = new Point3D();
		for (int i = 0; i < 3; i++) {
			q1.coords[i] = points.get(p2).coords[i] - points.get(p1).coords[i];
			q2.coords[i] = points.get(p3).coords[i] - points.get(p1).coords[i];
		}
		CrossProduct(q1, q2, q);
		return Length(q);
	}

	public void CrossProduct(final Point3D p1, final Point3D p2, Point3D p) {
		p.coords[0] = p1.coords[1] * p2.coords[2] - p1.coords[2] * p2.coords[1];
		p.coords[1] = -p1.coords[0] * p2.coords[2] + p1.coords[2] * p2.coords[0];
		p.coords[2] = p1.coords[0] * p2.coords[1] - p1.coords[1] * p2.coords[0];
	}

	public double SquareLength(final Point3D p) {
		return p.coords[0] * p.coords[0] + p.coords[1] * p.coords[1]
				+ p.coords[2] * p.coords[2];
	}

	public double Length(final Point3D p) {
		return Math.sqrt(SquareLength(p));
	}

	
	public int flipMinimize(final int eIndex) {
		double oldArea, newArea;
		int[] oldP = new int[3];
		int[] oldQ = new int[3];
		int[] newP = new int[3];
		int[] newQ = new int[3];
		
		int result;
		
		TriangulationEdge newEdge = new TriangulationEdge();

		if (edges.get(eIndex).tIndex[0] < 0 || edges.get(eIndex).tIndex[1] < 0) {
			return 0;
		}

		int[] oldP0 = new int[1];
		int[] oldP1 = new int[1];
		int[] oldP2 = new int[1];
		
		result = factor(edges.get(eIndex).tIndex[0], oldP0, oldP1, oldP2);
		oldP[0] = oldP0[0]; oldP[1] = oldP1[0]; oldP[2] = oldP2[0];
		if (0 == result) {
			return 0;
		}
		
		int[] oldQ0 = new int[1];
		int[] oldQ1 = new int[1];
		int[] oldQ2 = new int[1];
		result = factor(edges.get(eIndex).tIndex[1], oldQ0, oldQ1, oldQ2);
		oldQ[0] = oldQ0[0]; oldQ[1] = oldQ1[0]; oldQ[2] = oldQ2[0];
		
		if (0 == result) {
			return 0;
		}

		oldArea = area(oldP[0], oldP[1], oldP[2])
				+ area(oldQ[0], oldQ[1], oldQ[2]);
		int idxP, idxQ;
		for (idxP = 0; idxP < 3; idxP++) {
			int i;
			for (i = 0; i < 3; i++) {
				if (oldP[idxP] == oldQ[i]) {
					break;
				}
			}
			if (i == 3) {
				break;
			}
		}
		for (idxQ = 0; idxQ < 3; idxQ++) {
			int i;
			for (i = 0; i < 3; i++) {
				if (oldP[i] == oldQ[idxQ]) {
					break;
				}
			}
			if (i == 3) {
				break;
			}
		}
		if (idxP == 3 || idxQ == 3) {
			return 0;
		}
		newP[0] = oldP[idxP];
		newP[1] = oldP[(idxP + 1) % 3];
		newP[2] = oldQ[idxQ];
		newQ[0] = oldQ[idxQ];
		newQ[1] = oldP[(idxP + 2) % 3];
		newQ[2] = oldP[idxP];

		newArea = area(newP[0], newP[1], newP[2])
				+ area(newQ[0], newQ[1], newQ[2]);
		if (oldArea <= newArea) {
			return 0;
		}

		// Remove the entry in the hash_table for the old edge
		edgeMap.remove(EdgeIndex(edges.get(eIndex).pIndex[0], edges.get(eIndex).pIndex[1]));
		// Set the new edge so that the zero-side is newQ
		edges.get(eIndex).pIndex[0] = newP[0];
		edges.get(eIndex).pIndex[1] = newQ[0];
		// Insert the entry into the hash_table for the new edge
		edgeMap.put(EdgeIndex(newP[0], newQ[0]), eIndex);
		// Update the triangle information
		for (int i = 0; i < 3; i++) {
			int idx;
			idx = edgeMap.get(EdgeIndex(newQ[i], newQ[(i + 1) % 3]));
			triangles.get(edges.get(eIndex).tIndex[0]).eIndex[i] = idx;
			if (idx != eIndex) {
				if (edges.get(idx).tIndex[0] == edges.get(eIndex).tIndex[1]) {
					edges.get(idx).tIndex[0] = edges.get(eIndex).tIndex[0];
				}
				if (edges.get(idx).tIndex[1] == edges.get(eIndex).tIndex[1]) {
					edges.get(idx).tIndex[1] = edges.get(eIndex).tIndex[0];
				}
			}

			idx = edgeMap.get(EdgeIndex(newP[i], newP[(i + 1) % 3]));
			triangles.get(edges.get(eIndex).tIndex[1]).eIndex[i] = idx;
			if (idx != eIndex) {
				if (edges.get(idx).tIndex[0] == edges.get(eIndex).tIndex[0]) {
					edges.get(idx).tIndex[0] = edges.get(eIndex).tIndex[1];
				}
				if (edges.get(idx).tIndex[1] == edges.get(eIndex).tIndex[0]) {
					edges.get(idx).tIndex[1] = edges.get(eIndex).tIndex[1];
				}
			}
		}
		return 1;
	}

	// need improvement. Also need to test EdgeIndex methods. Ruida
	public int addTriangle(final int p1, final int p2, final int p3) {
		// hash_map<long long,int>::iterator iter;
		int tIdx, eIdx;
		int[] p = new int[3];
		
		p[0] = p1;
		p[1] = p2;
		p[2] = p3;
		triangles.add(new TriangulationTriangle());
		tIdx = (int) (triangles.size()) - 1;

		for (int i = 0; i < 3; i++) {
			long e = EdgeIndex(p[i], p[(i + 1) % 3]);

			if (edgeMap.get(e) == null) {
				TriangulationEdge edge = new TriangulationEdge();
				edge.pIndex[0] = p[i];
				edge.pIndex[1] = p[(i + 1) % 3];
				edges.add(edge);
				eIdx = (int) (edges.size()) - 1;
				// edgeMap[e]=eIdx;
				edgeMap.put(e, eIdx);
				edges.get(eIdx).tIndex[0] = tIdx;
			} else {
				// eIdx=edgeMap[e];
				eIdx = edgeMap.get(e);
				if (edges.get(eIdx).pIndex[0] == p[i]) {
					if (edges.get(eIdx).tIndex[0] < 0) {
						edges.get(eIdx).tIndex[0] = tIdx;
					} else {
						System.err.println("Edge Triangle in use 1\n");
						return 0;
					}
				} else {
					if (edges.get(eIdx).tIndex[1] < 0) {
						edges.get(eIdx).tIndex[1] = tIdx;
					} else {
						System.err.println("Edge Triangle in use 2\n");
						return 0;
					}
				}

			}
			triangles.get(tIdx).eIndex[i] = eIdx;
		}
		return tIdx;
	}

	protected static long EdgeIndex(final int p1, final int p2) {
		if (p1 > p2) {
			return ((long) (p1) << 32) | ((long) (p2));
		} else {
			return ((long) (p2) << 32) | ((long) (p1));
		}
	}
	
	// protected static double area(Triangle t) {}
}
