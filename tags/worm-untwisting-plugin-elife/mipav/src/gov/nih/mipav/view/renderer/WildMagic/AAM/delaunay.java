package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
import java.util.*;

/**
 * Edgeset of Delaunay triangulation ( L2-norm). Steve J. Fortune (1987) A
 * Sweepline Algorithm for Voronoi Diagrams, Algorithmica 2, 153-174.
 * http://www.netlib.org/voronoi/sweep2
 * 
 * This is Java modified version of Steve J. Fortune's Sweepline algorithm for
 * Voronoi diagrams. It generates either Voronoi diagram or the Delaunay
 * triangulation with given set of points. The Delaunay triangulation is used in
 * the AAM model for the automatic prostate segmentation. It reads the double
 * VOIs contour from each 2D slice, and generate the Delaunay triangulation for
 * shape analysis.
 * 
 * The author of this software is Steven Fortune.  Copyright (c) 1994 by AT&T
 * Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 * 
 * @author Ruida Cheng
 * 
 */

public class delaunay {

	public static final int NULL = 0;

	public Edge DELETED = new Edge();

	public float xmin, xmax, ymin, ymax, deltax, deltay;

	public int triangulate, sorted, plot, debug;

	public Site[] sites;
	int nsites;
	int siteidx;
	int sqrt_nsites;
	int nvertices;

	public Site bottomsite;

	public static int le = 0;
	public static int re = 1;
	public int nedges;

	Halfedge ELleftend, ELrightend;
	int ELhashsize;
	Halfedge[] ELhash;

	int PQhashsize;
	Halfedge[] PQhash;

	int PQcount;
	int PQmin;

	float pxmin, pxmax, pymin, pymax, cradius;

	public FileReader reader;
	public Scanner in;

	class Point {
		float x, y;
	}

	class Site implements Comparator {
		public Point coord = new Point();
		int sitenbr;
		int refcnt;

		public int compare(final Object v1, final Object v2) {
			Site site1 = (Site) v1;
			Site site2 = (Site) v2;
			Point s1, s2;
			s1 = (Point) site1.coord;
			s2 = (Point) site2.coord;
			if (s1.y < s2.y)
				return (-1);
			if (s1.y > s2.y)
				return (1);
			if (s1.x < s2.x)
				return (-1);
			if (s1.x > s2.x)
				return (1);
			return (0);
		}

	}

	class Edge {
		float a, b, c;
		Site[] ep = new Site[2];
		Site[] reg = new Site[2];
		int edgenbr;

		public Edge() {
			ep[0] = new Site();
			ep[1] = new Site();
			reg[0] = new Site();
			reg[1] = new Site();
		}

	}

	class Halfedge {
		Halfedge ELleft, ELright;
		Edge ELedge;
		int ELrefcnt;
		int ELpm;
		Site vertex;
		float ystar;
		Halfedge PQnext;
	};

	class Triangle {
		int index1;
		int index2;
		int index3;

		public Triangle(int x, int y, int z) {
			index1 = x;
			index2 = y;
			index3 = z;
		}

	}

	Vector<Triangle> triangles = new Vector<Triangle>();
	private int outTriangles = 0;

	public delaunay() {

	}

	// ************************ edgelist ******************************

	int ntry, totalsearch;

	public void ELinitialize() {
		int i;
		// freeinit(hfl, sizeof **ELhash);
		ELhashsize = 2 * sqrt_nsites;
		ELhash = new Halfedge[ELhashsize];
		for (i = 0; i < ELhashsize; i += 1) {
			ELhash[i] = null;
		}
		ELleftend = HEcreate(null, 0);
		ELrightend = HEcreate(null, 0);
		ELleftend.ELleft = null;
		ELleftend.ELright = ELrightend;
		ELrightend.ELleft = ELleftend;
		ELrightend.ELright = null;
		ELhash[0] = ELleftend;
		ELhash[ELhashsize - 1] = ELrightend;
	}

	public Halfedge HEcreate(Edge e, int pm) {
		Halfedge answer;
		answer = new Halfedge();
		answer.ELedge = e;
		answer.ELpm = pm;
		answer.PQnext = null;
		answer.vertex = null;
		answer.ELrefcnt = 0;
		return (answer);
	}

	public void ELinsert(Halfedge lb, Halfedge newEdge) {
		newEdge.ELleft = lb;
		newEdge.ELright = lb.ELright;
		(lb.ELright).ELleft = newEdge;
		lb.ELright = newEdge;
	}

	// Get entry from hash table, pruning any deleted nodes
	public Halfedge ELgethash(int b) {
		Halfedge he;

		if (b < 0 || b >= ELhashsize)
			return null;
		he = ELhash[b];

		if (he == null || he.ELedge != DELETED) {
			return he;
		}

		/* Hash table points to deleted half edge. Patch as necessary. */
		ELhash[b] = null;
		if ((he.ELrefcnt -= 1) == 0)
			he = null;
		return null;
	}

	public Halfedge ELleftbnd(Point p) { // Point *p

		int i;
		int bucket;
		Halfedge he;

		/* Use hash table to get close to desired halfedge */
		bucket = (int) ((p.x - xmin) / deltax * ELhashsize);
		if (bucket < 0)
			bucket = 0;
		if (bucket >= ELhashsize)
			bucket = ELhashsize - 1;
		he = ELgethash(bucket);
		if (he == null) {
			for (i = 1; true; i += 1) {
				if ((he = ELgethash(bucket - i)) != null)
					break;
				if ((he = ELgethash(bucket + i)) != null)
					break;
			}
			totalsearch += i;
		}
		;
		ntry += 1;
		/* Now search linear list of halfedges for the correct one */
		if (he == ELleftend || (he != ELrightend && right_of(he, p) == 1)) {
			do {
				he = he.ELright;
			} while (he != ELrightend && right_of(he, p) == 1);
			he = he.ELleft;
		} else
			do {
				he = he.ELleft;
			} while (he != ELleftend && right_of(he, p) == 0);

		/* Update hash table and reference counts */
		if (bucket > 0 && bucket < ELhashsize - 1) {
			if (ELhash[bucket] != null)
				ELhash[bucket].ELrefcnt -= 1;
			ELhash[bucket] = he;
			ELhash[bucket].ELrefcnt += 1;
		}

		return (he);

	}

	public void ELdelete(Halfedge he) {
		(he.ELleft).ELright = he.ELright;
		(he.ELright).ELleft = he.ELleft;
		he.ELedge = DELETED;
	}

	public Halfedge ELright(Halfedge he) {
		return he.ELright;
	}

	public Halfedge ELleft(Halfedge he) {
		return he.ELleft;
	}

	public Site leftreg(Halfedge he) {
		if (he.ELedge == null)
			return (bottomsite);
		return (he.ELpm == le ? he.ELedge.reg[le] : he.ELedge.reg[re]);
	}

	public Site rightreg(Halfedge he) {
		if (he.ELedge == null)
			return (bottomsite);
		return (he.ELpm == le ? he.ELedge.reg[re] : he.ELedge.reg[le]);
	}

	// ******************** Heap - Priority Queue
	// ********************************
	public void PQinsert(Halfedge he, Site v, float offset) {
		Halfedge last, next;

		he.vertex = v;
		ref(v);
		he.ystar = v.coord.y + offset;
		last = PQhash[PQbucket(he)];
		while ((next = last.PQnext) != null
				&& (he.ystar > next.ystar || (he.ystar == next.ystar && v.coord.x > next.vertex.coord.x))) {
			last = next;
		}

		he.PQnext = last.PQnext;
		last.PQnext = he;
		PQcount += 1;
	}

	public void PQdelete(Halfedge he) {
		Halfedge last;

		if (he.vertex != null) {
			last = PQhash[PQbucket(he)];
			while (last.PQnext != he)
				last = last.PQnext;
			last.PQnext = he.PQnext;
			PQcount -= 1;
			deref(he.vertex);
			he.vertex = null;
		}
	}

	public int PQbucket(Halfedge he) {
		int bucket;

		bucket = (int) ((he.ystar - ymin) / deltay * PQhashsize);
		if (bucket < 0)
			bucket = 0;
		if (bucket >= PQhashsize)
			bucket = PQhashsize - 1;
		if (bucket < PQmin)
			PQmin = bucket;
		return (bucket);
	}

	public boolean PQempty() {
		return (PQcount == 0) ? true : false;
	}

	public Point PQ_min() {
		Point answer = new Point();

		while (PQhash[PQmin].PQnext == null) {
			PQmin += 1;
		}

		answer.x = PQhash[PQmin].PQnext.vertex.coord.x;
		answer.y = PQhash[PQmin].PQnext.ystar;
		return answer;
	}

	public Halfedge PQextractmin() {
		Halfedge curr;

		curr = PQhash[PQmin].PQnext;
		PQhash[PQmin].PQnext = curr.PQnext;
		PQcount -= 1;
		return (curr);
	}

	public void PQinitialize() {
		int i;
		Point s;

		PQcount = 0;
		PQmin = 0;
		PQhashsize = 4 * sqrt_nsites;

		PQhash = new Halfedge[PQhashsize];
		for (i = 0; i < PQhashsize; i += 1) {
			PQhash[i] = new Halfedge();
			PQhash[i].PQnext = null;
		}
	}

	// ******************** geometry **********************************
	public void geominit() {
		float sn;
		nvertices = 0;
		nedges = 0;
		sn = nsites + 4;
		sqrt_nsites = (int) Math.sqrt(sn);
		deltay = ymax - ymin;
		deltax = xmax - xmin;
	}

	public Edge bisect(Site s1, Site s2) {
		float dx, dy, adx, ady;
		Edge newedge;

		newedge = new Edge();

		newedge.reg[0] = s1;
		newedge.reg[1] = s2;
		ref(s1);
		ref(s2);
		newedge.ep[0] = null;
		newedge.ep[1] = null;

		dx = s2.coord.x - s1.coord.x;
		dy = s2.coord.y - s1.coord.y;
		adx = dx > 0 ? dx : -dx;
		ady = dy > 0 ? dy : -dy;
		newedge.c = (float) (s1.coord.x * dx + s1.coord.y * dy + (dx * dx + dy
				* dy) * 0.5);
		if (adx > ady) {
			newedge.a = 1.0f;
			newedge.b = dy / dx;
			newedge.c /= dx;
		} else {
			newedge.b = 1.0f;
			newedge.a = dx / dy;
			newedge.c /= dy;
		}
		;

		newedge.edgenbr = nedges;

		nedges += 1;
		return (newedge);
	}

	public Site intersect(Halfedge el1, Halfedge el2) {
		Edge e1, e2, e;
		Halfedge el;
		float d, xint, yint;
		int right_of_site;
		Site v;

		e1 = el1.ELedge;
		e2 = el2.ELedge;
		if (e1 == null || e2 == null)
			return null;
		if (e1.reg[1] == e2.reg[1])
			return null;

		d = e1.a * e2.b - e1.b * e2.a;
		if (-1.0e-10 < d && d < 1.0e-10)
			return null;

		xint = (e1.c * e2.b - e2.c * e1.b) / d;
		yint = (e2.c * e1.a - e1.c * e2.a) / d;

		if ((e1.reg[1].coord.y < e2.reg[1].coord.y)
				|| (e1.reg[1].coord.y == e2.reg[1].coord.y && e1.reg[1].coord.x < e2.reg[1].coord.x)) {
			el = el1;
			e = e1;
		} else {
			el = el2;
			e = e2;
		}
		;
		right_of_site = xint >= e.reg[1].coord.x ? 1 : 0;
		if ((right_of_site == 1 && el.ELpm == le)
				|| (right_of_site == 0 && el.ELpm == re))
			return null;

		v = new Site();
		v.refcnt = 0;
		v.coord.x = xint;
		v.coord.y = yint;
		return (v);
	}

	// returns 1 if p is to right of halfedge e
	public int right_of(Halfedge el, Point p) {
		Edge e;
		Site topsite;
		int right_of_site, above, fast;
		float dxp, dyp, dxs, t1, t2, t3, yl;

		e = el.ELedge;
		topsite = e.reg[1];
		right_of_site = p.x > topsite.coord.x ? 1 : 0;
		if (right_of_site == 1 && el.ELpm == le)
			return (1);
		if (right_of_site == 0 && el.ELpm == re)
			return (0);

		if (e.a == 1.0) {
			dyp = p.y - topsite.coord.y;
			dxp = p.x - topsite.coord.x;
			fast = 0;
			if ((right_of_site == 0 & e.b < 0.0)
					| (right_of_site == 1 & e.b >= 0.0)) {
				above = dyp >= e.b * dxp ? 1 : 0;
				fast = above;
			} else {
				above = p.x + p.y * e.b > e.c ? 1 : 0;
				if (e.b < 0.0)
					above = (above == 1 ? 0 : 1);
				if (above == 0)
					fast = 1;
			}
			if (fast == 0) {
				dxs = topsite.coord.x - (e.reg[0]).coord.x;
				above = e.b * (dxp * dxp - dyp * dyp) < dxs * dyp
						* (1.0 + 2.0 * dxp / dxs + e.b * e.b) ? 1 : 0;
				if (e.b < 0.0)
					above = (above == 1 ? 0 : 1);
			}
		} else { // e.b==1.0
			yl = e.c - e.a * p.x;
			t1 = p.y - yl;
			t2 = p.x - topsite.coord.x;
			t3 = yl - topsite.coord.y;
			above = (t1 * t1 > (t2 * t2 + t3 * t3) ? 1 : 0);
		}

		return (el.ELpm == le ? above : (above == 1 ? 0 : 1));
	}

	public void endpoint(Edge e, int lr, Site s) {
		e.ep[lr] = s;
		ref(s);
		if (e.ep[re - lr] == null)
			return;

		deref(e.reg[le]);
		deref(e.reg[re]);

		e = null;
	}

	public float dist(Site s, Site t) {
		float dx, dy;
		dx = s.coord.x - t.coord.x;
		dy = s.coord.y - t.coord.y;
		return (float) ((Math.sqrt(dx * dx + dy * dy)));
	}

	public void makevertex(Site v) {
		v.sitenbr = nvertices;
		nvertices += 1;
	}

	public void deref(Site v) {
		v.refcnt -= 1;
		if (v.refcnt == 0)
			v = null;
	}

	public void ref(Site v) {
		v.refcnt += 1;
	}

	// ************************** voronoi ********************************
	// implicit parameters: nsites, sqrt_nsites, xmin, xmax, ymin, ymax,
	// deltax, deltay (can all be estimates).
	// Performance suffers if they are wrong; better to make nsites,
	// deltax, and deltay too big than too small. (?)

	public void voronoi(int triangulate, boolean output) {
		Site newsite, bot, top, temp, p;
		Site v;
		Point newintstar = new Point();
		int pm;
		Halfedge lbnd, rbnd, llbnd, rrbnd, bisector;
		Edge e;

		PQinitialize();
		if (sorted == 1) {
			bottomsite = readone();
		} else {
			bottomsite = nextone();
		}

		ELinitialize();

		if (sorted == 1) {
			newsite = readone();
		} else {
			newsite = nextone();
		}
		while (true) {
			if (!PQempty())
				newintstar = PQ_min();

			if (newsite != null
					&& (PQempty() || newsite.coord.y < newintstar.y || (newsite.coord.y == newintstar.y && newsite.coord.x < newintstar.x))) {

				lbnd = ELleftbnd((newsite.coord));
				rbnd = ELright(lbnd);
				bot = rightreg(lbnd);
				e = bisect(bot, newsite);
				bisector = HEcreate(e, le);
				ELinsert(lbnd, bisector);
				if ((p = intersect(lbnd, bisector)) != null) {
					PQdelete(lbnd);
					PQinsert(lbnd, p, dist(p, newsite));
				}

				lbnd = bisector;
				bisector = HEcreate(e, re);
				ELinsert(lbnd, bisector);
				if ((p = intersect(bisector, rbnd)) != null) {
					PQinsert(bisector, p, dist(p, newsite));
				}

				if (sorted == 1) {
					newsite = readone();
				} else {
					newsite = nextone();
				}

			} else if (!PQempty()) {
				/* intersection is smallest */

				lbnd = PQextractmin();
				llbnd = ELleft(lbnd);
				rbnd = ELright(lbnd);
				rrbnd = ELright(rbnd);
				bot = leftreg(lbnd);
				top = rightreg(rbnd);

				if (output) {
					out_triple(bot, top, rightreg(lbnd));
				} else {
					addTriangle(bot, top, rightreg(lbnd));
				}

				v = lbnd.vertex;
				makevertex(v);
				endpoint(lbnd.ELedge, lbnd.ELpm, v);
				endpoint(rbnd.ELedge, rbnd.ELpm, v);
				ELdelete(lbnd);
				PQdelete(rbnd);
				ELdelete(rbnd);
				pm = le;
				if (bot.coord.y > top.coord.y) {
					temp = bot;
					bot = top;
					top = temp;
					pm = re;
				}
				e = bisect(bot, top);
				bisector = HEcreate(e, pm);
				ELinsert(llbnd, bisector);
				endpoint(e, re - pm, v);
				deref(v);
				if ((p = intersect(llbnd, bisector)) != null) {
					PQdelete(llbnd);
					PQinsert(llbnd, p, dist(p, bot));
				}

				if ((p = intersect(bisector, rrbnd)) != null) {
					PQinsert(bisector, p, dist(p, bot));
				}

			} else
				break;
		}

		for (lbnd = ELright(ELleftend); lbnd != ELrightend; lbnd = ELright(lbnd)) {
			e = lbnd.ELedge;

		}

	}

	// SCOMP sorts sites on the Y coordinate and then on the X coordinate.
	public int scomp(Point s1, Point s2) {
		if (s1.y < s2.y)
			return (-1);
		if (s1.y > s2.y)
			return (1);
		if (s1.x < s2.x)
			return (-1);
		if (s1.x > s2.x)
			return (1);
		return (0);
	}

	// NEXTONE returns a single in-storage site.
	public Site nextone() {
		Site s;
		if (siteidx < nsites) {
			s = sites[siteidx];
			siteidx += 1;
			return (s);
		} else
			return null;
	}

	public int[] ATTDelaunay(double[] xVec, double[] yVec, int nbPoints,
			int[] numTriangles) {
		int triangulate = 1;
		int trianglesIndexArray[];
		readSites(xVec, yVec, nbPoints);

		siteidx = 0;
		geominit();
		voronoi(triangulate, false);

		numTriangles[0] = outTriangles;
		trianglesIndexArray = new int[outTriangles * 3];

		int index = 0;
		for (int i = 0; i < outTriangles; i++) {
			Triangle t = triangles.get(i);
			trianglesIndexArray[index] = t.index1;
			trianglesIndexArray[index + 1] = t.index2;
			trianglesIndexArray[index + 2] = t.index3;
			index += 3;
		}

		return trianglesIndexArray;
	}

	public int readSites(double[] xVec, double[] yVec, int nbPoints) {
		int i;
		nsites = 0;
		sites = new Site[4000];
		for (i = 0; i < nbPoints; i++) {
			sites[nsites] = new Site();
			sites[nsites].coord.x = (float) xVec[i];
			sites[nsites].coord.y = (float) yVec[i];
			sites[nsites].sitenbr = nsites;
			sites[nsites].refcnt = 0;
			nsites += 1;

			if (nsites % 4000 == 0) {
				Site[] tempSites = new Site[sites.length];
				System.arraycopy(sites, 0, tempSites, 0, sites.length);
				sites = new Site[nsites + 4000];
				System.arraycopy(tempSites, 0, sites, 0, sites.length);
			}
		}

		// Trimm trailing empty slots.
		Site[] temp = new Site[nsites];
		System.arraycopy(sites, 0, temp, 0, nsites);
		sites = null;
		sites = new Site[nsites];
		// System.err.println("sites length = " + sites.length);

		System.arraycopy(temp, 0, sites, 0, sites.length);

		Site t = new Site();
		QuickSort qsort = new QuickSort(t);
		qsort.sort(sites);
		xmin = sites[0].coord.x;
		xmax = sites[0].coord.x;
		for (i = 1; i < nsites; i += 1) {
			if (sites[i].coord.x < xmin)
				xmin = sites[i].coord.x;
			if (sites[i].coord.x > xmax)
				xmax = sites[i].coord.x;
		}

		ymin = sites[0].coord.y;
		ymax = sites[nsites - 1].coord.y;
		return -1;
	}

	// READSITES reads all sites, sort, and compute xmin, xmax, ymin, ymax.
	public void readsites() {
		int i;

		nsites = 0;

		sites = new Site[4000];

		while (in.hasNextLine()) {
			String line = in.nextLine();
			StringTokenizer token = new StringTokenizer(line);
			sites[nsites] = new Site();
			sites[nsites].coord.x = Float.valueOf(token.nextToken());
			sites[nsites].coord.y = Float.valueOf(token.nextToken());
			sites[nsites].sitenbr = nsites;
			sites[nsites].refcnt = 0;
			nsites += 1;

			if (nsites % 4000 == 0) {
				Site[] tempSites = new Site[sites.length];
				System.arraycopy(sites, 0, tempSites, 0, sites.length);
				sites = new Site[nsites + 4000];
				System.arraycopy(tempSites, 0, sites, 0, sites.length);
			}
		}

		// Trimm trailing empty slots.
		Site[] temp = new Site[nsites];
		System.arraycopy(sites, 0, temp, 0, nsites);
		sites = null;
		sites = new Site[nsites];
		System.err.println("sites length = " + sites.length);

		System.arraycopy(temp, 0, sites, 0, sites.length);

		Site t = new Site();
		QuickSort qsort = new QuickSort(t);
		qsort.sort(sites);
		xmin = sites[0].coord.x;
		xmax = sites[0].coord.x;
		for (i = 1; i < nsites; i += 1) {
			if (sites[i].coord.x < xmin)
				xmin = sites[i].coord.x;
			if (sites[i].coord.x > xmax)
				xmax = sites[i].coord.x;
		}

		ymin = sites[0].coord.y;
		ymax = sites[nsites - 1].coord.y;

	}

	// READONE reads one site.
	public Site readone() {
		Site s;

		s = new Site();
		s.refcnt = 0;
		s.sitenbr = siteidx;
		siteidx += 1;

		if (in.hasNextLine()) {
			String line = in.nextLine();
			StringTokenizer token = new StringTokenizer(line);
			s.coord.x = Float.valueOf(token.nextToken());
			s.coord.y = Float.valueOf(token.nextToken());
		}

		return (s);

	}

	// ****************************** output
	// **********************************************
	// OUT_BISECTOR writes "L" records to the Voronoi diagram output file.
	public void out_bisector(Edge e) {

		if (triangulate == 1 & plot == 1 & debug == 0) {
			// line(e.reg[0].coord.x, e.reg[0].coord.y, e.reg[1].coord.x,
			// e.reg[1].coord.y);

		}
		if (triangulate == 0 & plot == 0 & debug == 0) {
			System.err.printf("l %f %f %f\n", e.a, e.b, e.c);
		}
		if (debug == 1) {
			System.err.printf("line(%d) %gx+%gy=%g, bisecting %d %d\n",
					e.edgenbr, e.a, e.b, e.c, e.reg[le].sitenbr,
					e.reg[re].sitenbr);
		}

	}

	// OUT_EP writes "E" records to the Voronoi diagram output file.
	public void out_ep(Edge e) {

		if (triangulate == 0 & plot == 1) {
			clip_line(e);
		}

		if (triangulate == 0 & plot == 0) {
			System.err.printf("e %d", e.edgenbr);
			System.err.printf(" %d ", e.ep[le] != null ? e.ep[le].sitenbr : -1);
			System.err.printf("%d\n", e.ep[re] != null ? e.ep[re].sitenbr : -1);
		}

	}

	// OUT_VERTEX writes "V" records to the Voronoi diagram output file.
	public void out_vertex(Site v) {
		if (triangulate == 0 & plot == 0 & debug == 0) {
			System.err.printf("v %f %f\n", v.coord.x, v.coord.y);
		}

		if (debug == 1) {
			System.err.printf("vertex(%d) at %f %f\n", v.sitenbr, v.coord.x,
					v.coord.y);
		}
	}

	// OUT_SITE writes "S" records to the Voronoi diagram output file.
	public void out_site(Site s) {
		if (triangulate == 0 & plot == 1 & debug == 0) {
			// circle (s.coord.x, s.coord.y, cradius);
		}

		if (triangulate == 0 & plot == 0 & debug == 0) {
			System.err.printf("s %f %f\n", s.coord.x, s.coord.y);
		}

		if (debug == 1) {
			System.err.printf("site (%d) at %f %f\n", s.sitenbr, s.coord.x,
					s.coord.y);
		}
	}

	// OUT_TRIPLE writes the triangle records to the Delaunay triangulation
	// output file.
	public void out_triple(Site s1, Site s2, Site s3) {

		if (triangulate == 1 & plot == 0 & debug == 0) {
			System.err.printf("%d %d %d\n", s1.sitenbr, s2.sitenbr, s3.sitenbr);
		}

		if (debug == 1) {
			System.err.printf("circle through left=%d right=%d bottom=%d\n",
					s1.sitenbr, s2.sitenbr, s3.sitenbr);
		}

	}

	public void addTriangle(Site s1, Site s2, Site s3) {
		triangles.add(new Triangle(s1.sitenbr, s2.sitenbr, s3.sitenbr));
		outTriangles++;
	}

	// PLOTINIT initializes plotting information.
	public void plotinit() {

		float dx;
		float dy;
		float d;

		dy = ymax - ymin;
		dx = xmax - xmin;
		d = (dx > dy ? dx : dy) * 1.1f;

		pxmin = xmin - (d - dx) / 2.0f;
		pxmax = xmax + (d - dx) / 2.0f;
		pymin = ymin - (d - dy) / 2.0f;
		pymax = ymax + (d - dy) / 2.0f;

		cradius = (pxmax - pxmin) / 350.0f;

	}

	public int clip_line(Edge e) {
		Site s1, s2;
		float x1, x2, y1, y2;

		if (e.a == 1.0 && e.b >= 0.0) {
			s1 = e.ep[1];
			s2 = e.ep[0];
		} else {
			s1 = e.ep[0];
			s2 = e.ep[1];
		}
		

		if (e.a == 1.0) {
			y1 = pymin;
			if (s1 != null && s1.coord.y > pymin)
				y1 = s1.coord.y;
			if (y1 > pymax)
				return 0;
			x1 = e.c - e.b * y1;
			y2 = pymax;
			if (s2 != null && s2.coord.y < pymax)
				y2 = s2.coord.y;
			if (y2 < pymin)
				return (0);
			x2 = e.c - e.b * y2;
			if ((x1 > pxmax & x2 > pxmax) | (x1 < pxmin & x2 < pxmin))
				return 0;
			if (x1 > pxmax) {
				x1 = pxmax;
				y1 = (e.c - x1) / e.b;
			}
			if (x1 < pxmin) {
				x1 = pxmin;
				y1 = (e.c - x1) / e.b;
			}
			if (x2 > pxmax) {
				x2 = pxmax;
				y2 = (e.c - x2) / e.b;
			}
			if (x2 < pxmin) {
				x2 = pxmin;
				y2 = (e.c - x2) / e.b;
			}
		} else {
			x1 = pxmin;
			if (s1 != null && s1.coord.x > pxmin)
				x1 = s1.coord.x;
			if (x1 > pxmax)
				return (0);
			y1 = e.c - e.a * x1;
			x2 = pxmax;
			if (s2 != null && s2.coord.x < pxmax)
				x2 = s2.coord.x;
			if (x2 < pxmin)
				return (0);
			y2 = e.c - e.a * x2;
			if ((y1 > pymax & y2 > pymax) | (y1 < pymin & y2 < pymin))
				return (0);
			if (y1 > pymax) {
				y1 = pymax;
				x1 = (e.c - y1) / e.a;
			}
			if (y1 < pymin) {
				y1 = pymin;
				x1 = (e.c - y1) / e.a;
			}
			if (y2 > pymax) {
				y2 = pymax;
				x2 = (e.c - y2) / e.a;
			}
			if (y2 < pymin) {
				y2 = pymin;
				x2 = (e.c - y2) / e.a;
			}
		}
		
		return 0;

	}

}
