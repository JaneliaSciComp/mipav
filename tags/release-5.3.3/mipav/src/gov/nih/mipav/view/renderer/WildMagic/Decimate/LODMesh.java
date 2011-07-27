package gov.nih.mipav.view.renderer.WildMagic.Decimate;

import WildMagic.LibFoundation.Mathematics.*;

public class LODMesh {
	int numOK = 0;
	int numCANCEL = 0;
	int deciV = 0;
	int deciF = 0;
	int numE = 0;
	int iterationnum = 0;
	Vertices[] initialV;
	double ThresholdGamma = 0.15;

	Vertices VHead;
	Vertices VTail;
	Triangle THead;
	Triangle TTail;
	NEList[] NEHead;
	NEList[] NETail;
	double maxenergy = 0.0;
	double realmax = 0.0;
	Edge[] Root;
	PriorityQueue pqueue;
	int HeapPointerN = 0;

	Neighbor[] NHead;
	Neighbor[] NTail;
	Vector3f[] point;

	int[] boundary;
	Quadric dummyQE;
	Vector3f dummyv1;
	Vector3f dummyv2;
	Vector3f dummyv3;
	Vector3f dummyv4;

	public LODMesh(int nv1, int nv2, int[][] dFace, Vector3f[] rpoint, int[] rboundary) {
		int i;
		dummyv1 = new Vector3f(0.0f, 0.0f, 0.0f);
		dummyv2 = new Vector3f(0.0f, 0.0f, 0.0f);
		dummyv3 = new Vector3f(0.0f, 0.0f, 0.0f);
		dummyv4 = new Vector3f(0.0f, 0.0f, 0.0f);
		dummyQE = new Quadric();
		deciV = nv1;
		deciF = nv2;
		numE = 0;
		InitV();
		InitT();

		/*
		 * point = rpoint; boundary = rboundary;
		 */
		point = new Vector3f[deciV];
		boundary = new int[deciV];

		for (i = 0; i < deciV; i++) {

			point[i] = new Vector3f(rpoint[i].X, rpoint[i].Y, rpoint[i].Z);
			boundary[i] = rboundary[i];

		}
		initialV = new Vertices[deciV];
		NHead = new Neighbor[deciV];
		NTail = new Neighbor[deciV];

		NEHead = new NEList[deciV];
		NETail = new NEList[deciV];

		Vertices now = VHead;

		for (i = 0; i < deciV; i++) {
			AppendV(i);
			now = next(now);
			initialV[i] = now;
			NHead[i] = new Neighbor();
			NTail[i] = new Neighbor();
			NHead[i].next = NTail[i];
			NTail[i].back = NHead[i];

			NEHead[i] = new NEList();
			NETail[i] = new NEList();
			NEHead[i].next = NETail[i];
			NETail[i].back = NEHead[i];

		}

		for (i = 0; i < deciF; i++) {
			if (dFace[i][0] > dFace[i][1])
				numE++;
			if (dFace[i][1] > dFace[i][2])
				numE++;
			if (dFace[i][2] > dFace[i][0])
				numE++;
		}

		Root = new Edge[numE];
		pqueue = new PriorityQueue(numE);
		HeapPointerN = 0;
		// System.out.println("Number of Edges: " + numE);
		Triangle nowt = THead;
		for (i = 0; i < deciF; i++) {
			AppendT(dFace[i][0], dFace[i][1], dFace[i][2]);
			nowt = next(nowt);
			AppendN(nowt, NTail[dFace[i][0]]);
			AppendN(nowt, NTail[dFace[i][1]]);
			AppendN(nowt, NTail[dFace[i][2]]);
		}

		double tarea = 0.0f;
		Neighbor nownt;
		for (i = 0; i < deciV; i++) {
			tarea = 0.0f;
			nownt = NHead[i];
			while (next(nownt) != NTail[i]) {
				nownt = next(nownt);
				tarea += nownt.dt.area;
			}
			initialV[i].error.normalizeQE(tarea);
		}

		for (i = 0; i < deciF; i++) {
			if (dFace[i][0] > dFace[i][1])
				AppendE(initialV[dFace[i][0]], initialV[dFace[i][1]]);
			if (dFace[i][1] > dFace[i][2])
				AppendE(initialV[dFace[i][1]], initialV[dFace[i][2]]);
			if (dFace[i][2] > dFace[i][0])
				AppendE(initialV[dFace[i][2]], initialV[dFace[i][0]]);
		}
		maxenergy = Math.abs(pqueue.getmax()) - 10.0f;
	}

	/**
     * Dispose the local memory.
     */
	public void dispose() {
		initialV = null ;
		VHead = null;
		VTail = null;
		THead = null;
		TTail = null;
		for (int i = 0; i < deciV; i++) {
			NHead[i] = null;
			NTail[i] = null;
			NEHead[i] = null;
			NETail[i] = null;
			point[i] = null;
		}
		NEHead = null;
		NETail = null;
		NHead = null;
		NTail = null;
		point = null;
		boundary = null;
	
		Root = null;
		pqueue = null;
		dummyQE = null;
		dummyv1 = null;
		dummyv2 = null;
		dummyv3 = null;
		dummyv4 = null;
	}
	
	
	public void getVFace(Vector3f[] subpoint, int[][] subFace) {
		int i = 0;
		Vertices now = VHead;
		while (next(now) != VTail) {
			now = next(now);
			if (now.EC) {
				subpoint[i] = new Vector3f(point[now.ID].X, point[now.ID].Y,
						point[now.ID].Z);
				now.ID = i;
				i++;
				/*
				 * if(i>=deciV){ // System.out.println("Break this process in V.");
				 * break; }
				 */
			}
		}
		i = 0;
		Triangle nowt = THead;
		while (next(nowt) != TTail) {

			nowt = next(nowt);
			if (nowt.EC) {

				// // System.out.println("i:"+i);
				// if(nowt.v1.ID<deciV &&nowt.v2.ID<deciV &&nowt.v3.ID<deciV){
				subFace[i][0] = nowt.v1.ID;
				subFace[i][1] = nowt.v2.ID;
				subFace[i][2] = nowt.v3.ID;
				i++;
				/*
				 * if(i>=deciF){ // System.out.println("Break this process in F.");
				 * break; }
				 */
				// }
			}
		}
	}

	public void getVFace(Vector3f[] subpoint, int[][] subFace, int[] ind) {
		int i = 0;
		Vertices now = VHead;
		while (next(now) != VTail) {
			now = next(now);
			if (now.EC) {
				subpoint[i] = new Vector3f(point[now.ID].X, point[now.ID].Y,
						point[now.ID].Z);
				ind[i] = now.ID;
				now.ID = i;
				i++;
				/*
				 * if(i>=deciV){ // System.out.println("Break this process in V.");
				 * break; }
				 */
			}
		}
		i = 0;
		Triangle nowt = THead;
		while (next(nowt) != TTail) {

			nowt = next(nowt);
			if (nowt.EC) {

				// // System.out.println("i:"+i);
				// if(nowt.v1.ID<deciV &&nowt.v2.ID<deciV &&nowt.v3.ID<deciV){
				subFace[i][0] = nowt.v1.ID;
				subFace[i][1] = nowt.v2.ID;
				subFace[i][2] = nowt.v3.ID;
				i++;
				/*
				 * if(i>=deciF){ // System.out.println("Break this process in F.");
				 * break; }
				 */
				// }
			}
		}
	}

	private void RemoveNeighbor(Triangle dt, int di, int dj) {
		Neighbor now;
		int dtarget = dt.getV(di, dj);
		if (dtarget == -1) {
			// System.out.println("Error !!!");

		} else {
			now = NHead[dtarget];
			while (next(now) != NTail[dtarget]) {
				now = next(now);
				if (!(now.dt.EC)) {
					Remove(now);
					break;
				}
			}
			NEList nowNE = NEHead[dtarget];
			while (next(nowNE) != NETail[dtarget]) {
				nowNE = next(nowNE);
				if (nowNE.de.vp1.ID == di || nowNE.de.vp2.ID == di) {
					Remove(nowNE);
					break;
				}
			}
		}
	}


	public void Decimation(int objnum) {
		Vector3f mynewp = new Vector3f(0.0f, 0.0f, 0.0f);

		numOK = 0;
		numCANCEL = 0;
		while (deciV > objnum) {

			if (Update(mynewp))
				break;
			iterationnum++;
			if (iterationnum >= numE) {
				break;
			}
		}

		// System.out.println("OK = " + numOK);
		// System.out.println("CANCEL = " + numCANCEL);

		int i = 0;
		int realV = 0;
		int realF = 0;

		Vertices now = VHead;
		while (next(now) != VTail) {
			now = next(now);
			if (now.EC) {
				realV++;
			}
		}
		Triangle nowt = THead;
		while (next(nowt) != TTail) {

			nowt = next(nowt);
			if (nowt.EC) {
				realF++;
			}
		}
		deciV = realV;
		deciF = realF;

	}

	private boolean Update(Vector3f dv) {
		int mypop = pqueue.Pop();

		if (mypop == -1)
			return true;
		Edge now = Root[mypop];
		if (now == null)
			return true;
		// // System.out.println(+now.ene);

		if (Consistency(now.vp1.ID, now.vp2.ID, dv)) {
			EdgeCollapse(now.vp1.ID, now.vp2.ID, dv);

			numOK++;
		} else {
			pqueue.Put(mypop, realmax);
			numCANCEL++;
		}

		return false;
	}

	private double Size(Vector3f dv) {
		return Math.sqrt((dv.X * dv.X + dv.Y * dv.Y + dv.Z * dv.Z));
	}

	private double DistanceS(Vector3f dv1, Vector3f dv2) {
		double dx = dv1.X - dv2.X;
		double dy = dv1.Y - dv2.Y;
		double dz = dv1.Z - dv2.Z;
		return (dx * dx + dy * dy + dz * dz);
	}

	private void EdgeCollapse(int i, int j, Vector3f dv) {

		Neighbor now;
		Neighbor tmp;
		NEList nowe;
		point[j].X = dv.X;
		point[j].Y = dv.Y;
		point[j].Z = dv.Z;
		if (boundary[i] == 1)
			boundary[j] = 1;

		initialV[j].error.addQE(initialV[i].error);

		/*
		 * If T_{i} has two vertices (di and dj) then it neighbor triangle is
		 * removed, and T_{i} real take EC = false.
		 */

		initialV[i].EC = false;
		now = NHead[j];
		while (next(now) != NTail[j]) {
			now = next(now);
			if (now.dt.HasTwoV(i, j)) {
				now.dt.EC = false;
				deciF--;
				RemoveNeighbor((now.dt), i, j);
				tmp = back(now);
				Remove(now);
				now = tmp;
			}
		}

		/*
		 * If T_{i} take EC = true then T_{around} is inserted to vertex (dj)
		 * neighbor, and index di of T_{i} is changed to index dj.
		 */

		now = NHead[i];
		while (next(now) != NTail[i]) {
			now = next(now);
			if (now.dt.EC) {
				now.dt.changeItoJ(initialV[i], initialV[j]);
				AppendN((now.dt), NTail[j]);
			}
		}

		/* Remove and Add Neighbor Edges of T_{i} to T_{j} */

		nowe = NEHead[j];
		while (next(nowe) != NETail[j]) {
			nowe = next(nowe);
			if (nowe.de.vp1.ID == i || nowe.de.vp2.ID == i) {
				Remove(nowe);
				break;
			}
		}

		nowe = NEHead[i];
		while (next(nowe) != NETail[i]) {
			nowe = next(nowe);
			// // System.out.println(+nowe.de.vp1.ID+" "+nowe.de.vp2.ID);
			if (nowe.getV(i) != j) {

				if (nowe.de.vp1 == initialV[i]) {
					nowe.de.vp1 = initialV[j];
				} else if (nowe.de.vp2 == initialV[i]) {
					nowe.de.vp2 = initialV[j];
				}
				AppendNE(nowe.de, NEHead[j], NETail[j]);
			} else {
				nowe.de.EC = false;
			}
		}

		/* Update HeapData */

		int dtarget;
		nowe = NEHead[j];
		while (next(nowe) != NETail[j]) {
			nowe = next(nowe);
			dtarget = nowe.getV(j);
			pqueue.HeapUpdate(nowe.de.idx, getEnergy(j, dtarget));
		}

		deciV--;

	}

	private void CrossVector(Vector3f out, Vector3f in1, Vector3f in2) {
		out.X = (in1.Y * in2.Z - in2.Y * in1.Z);
		out.Y = (in1.Z * in2.X - in2.Z * in1.X);
		out.Z = (in1.X * in2.Y - in2.X * in1.Y);
	}

	private void setNormal(Vector3f dln, int i, int j, Vector3f newp,
			Vector3f dv1, Vector3f dv2) {

		dv1.X = (point[i].X - newp.X);
		dv1.Y = (point[i].Y - newp.Y);
		dv1.Z = (point[i].Z - newp.Z);
		dv2.X = (point[j].X - newp.X);
		dv2.Y = (point[j].Y - newp.Y);
		dv2.Z = (point[j].Z - newp.Z);
		CrossVector(dln, dv1, dv2);
	}

	private boolean WillBeNonManifold(int i, int j) {
		int num2i = 0;
		int num2j = 0;
		int numFi = 0;
		int numFj = 0;

		Neighbor now = NHead[i];
		while (next(now) != NTail[i]) {
			now = next(now);
			if (now.dt.HasTwoV(i, j))
				num2i++;
			numFi++;
		}
		if (num2i >= 2)
			return true;
		now = NHead[j];
		while (next(now) != NTail[j]) {
			now = next(now);
			if (now.dt.HasTwoV(i, j))
				num2j++;
			numFj++;
		}
		if (num2j >= 2)
			return true;

		int numVi = 0;
		int numVj = 0;

		NEList nowe = NEHead[i];
		while (next(nowe) != NETail[i]) {
			nowe = next(nowe);
			numVi++;
		}
		nowe = NEHead[j];
		while (next(nowe) != NETail[j]) {
			nowe = next(nowe);
			numVj++;
		}
		int numFnew = (numFi + numFj - num2i - num2j);
		int numEnew = (numVi + numVj - (num2i + num2j) / 2 - 2);
		if (numFnew >= (numEnew - 1)) {
			return false;
		} else {
			return true;
		}
	}

	private boolean Consistency(int i, int j, Vector3f dv) {
		// if(boundary[i]==1||boundary[j]==1)return false;
		// if(boundary[i]==1&&boundary[j]==1)return false;
		// if(point[j].z<100.0f)return false;

		if (boundary[i] == 1 && boundary[j] == 1)
			if (WillBeNonManifold(i, j))
				return false;

		setNewPosition(i, j, dv);

		// // System.out.println(+dv.x+" "+dv.y+" "+dv.z);
		int dk1, dk2;

		dummyv4.X = 0.0f;
		dummyv4.Y = 0.0f;
		dummyv4.Z = 0.0f;

		double dsize = 0.0f;

		double Gamma = 0.0f;

		Neighbor now = NHead[i];
		while (next(now) != NTail[i]) {
			now = next(now);

			if (!(now.dt.HasTwoV(i, j))) {
				dk1 = now.dt.getV1(i);
				dk2 = now.dt.getV2(i);
				// // System.out.println("i :"+i+" j: "+j+" dk1: "+dk1+" dk2:
				// "+dk2);
				if (dk1 == -1 || dk2 == -1) {
					// System.out.println("i :" + i + " j: " + j);
					// System.out.println(+now.dt.v1.ID + " " + now.dt.v2.ID + " " + now.dt.v3.ID);
					if (now.dt.EC) {
						// System.out.println("TRUE");

					} else {
						// System.out.println("FALSE");
					}
					return false;
				}

				setNormal(dummyv3, dk1, dk2, dv, dummyv1, dummyv2);
				dummyv4.X = dummyv4.X + dummyv3.X;
				dummyv4.Y = dummyv4.Y + dummyv3.Y;
				dummyv4.Z = dummyv4.Z + dummyv3.Z;
				Gamma = ((2.0 * Math.sqrt(3.0) * Size(dummyv3)) / (DistanceS(
						dv, point[dk1])
						+ DistanceS(dv, point[dk2]) + DistanceS(point[dk1],
						point[dk2])));
				// if(Gamma<0.1)// System.out.println(+i+" "+j+" :"+Gamma);
				if (Gamma <= ThresholdGamma) {
					return false;
				}
			}
		}

		now = NHead[j];
		while (next(now) != NTail[j]) {
			now = next(now);

			if (!(now.dt.HasTwoV(i, j))) {
				dk1 = now.dt.getV1(j);
				dk2 = now.dt.getV2(j);
				if (dk1 == -1 || dk2 == -1) {
					// System.out.println("i :" + i + " j: " + j);
					// System.out.println(+now.dt.v1.ID + " " + now.dt.v2.ID + " " + now.dt.v3.ID);
					if (now.dt.EC) {
						// System.out.println("TRUE");

					} else {
						// System.out.println("FALSE");
					}
					return false;
				}

				setNormal(dummyv3, dk1, dk2, dv, dummyv1, dummyv2);
				dummyv4.X = dummyv4.X + dummyv3.X;
				dummyv4.Y = dummyv4.Y + dummyv3.Y;
				dummyv4.Z = dummyv4.Z + dummyv3.Z;
				Gamma = ((2.0 * Math.sqrt(3.0) * Size(dummyv3)) / (DistanceS(
						dv, point[dk1])
						+ DistanceS(dv, point[dk2]) + DistanceS(point[dk1],
						point[dk2])));
				if (Gamma <= ThresholdGamma) {
					return false;
				}
			}
		}
		dsize = Size(dummyv4);
		if (dsize == 0.0f)
			dsize = 1.0;
		dummyv4.X = (float)(dummyv4.X / dsize);
		dummyv4.Y = (float)(dummyv4.Y / dsize);
		dummyv4.Z = (float)(dummyv4.Z / dsize);

		now = NHead[i];
		while (next(now) != NTail[i]) {
			now = next(now);

			if (!(now.dt.HasTwoV(i, j))) {
				dk1 = now.dt.getV1(i);
				dk2 = now.dt.getV2(i);
				dummyv1.X =  0.5f * (point[dk1].X + point[dk2].X) + dummyv4.X;
				dummyv1.Y =  0.5f * (point[dk1].Y + point[dk2].Y) + dummyv4.Y;
				dummyv1.Z =  0.5f * (point[dk1].Z + point[dk2].Z) + dummyv4.Z;
				if (PlaneSize(point[dk1], point[dk2], dummyv1, dv))
					return false;
			}
		}
		now = NHead[j];
		while (next(now) != NTail[j]) {
			now = next(now);

			if (!(now.dt.HasTwoV(i, j))) {
				dk1 = now.dt.getV1(j);
				dk2 = now.dt.getV2(j);
				dummyv1.X =  0.5f * (point[dk1].X + point[dk2].X) + dummyv4.X;
				dummyv1.Y =  0.5f * (point[dk1].Y + point[dk2].Y) + dummyv4.Y;
				dummyv1.Z =  0.5f * (point[dk1].Z + point[dk2].Z) + dummyv4.Z;
				if (PlaneSize(point[dk1], point[dk2], dummyv1, dv))
					return false;
			}
		}

		return true;
	}

	private boolean PlaneSize(Vector3f p1, Vector3f p2, Vector3f p3, Vector3f ep) {
		double dv = (-ep.Z * p1.Y * p2.X + ep.Y * p1.Z * p2.X + ep.Z * p1.X
				* p2.Y - ep.X * p1.Z * p2.Y - ep.Y * p1.X * p2.Z + ep.X * p1.Y
				* p2.Z + ep.Z * p1.Y * p3.X - ep.Y * p1.Z * p3.X - ep.Z * p2.Y
				* p3.X + p1.Z * p2.Y * p3.X + ep.Y * p2.Z * p3.X - p1.Y * p2.Z
				* p3.X - ep.Z * p1.X * p3.Y + ep.X * p1.Z * p3.Y + ep.Z * p2.X
				* p3.Y - p1.Z * p2.X * p3.Y - ep.X * p2.Z * p3.Y + p1.X * p2.Z
				* p3.Y + ep.Y * p1.X * p3.Z - ep.X * p1.Y * p3.Z - ep.Y * p2.X
				* p3.Z + p1.Y * p2.X * p3.Z + ep.X * p2.Y * p3.Z - p1.X * p2.Y
				* p3.Z);

		// // System.out.println(+dv);
		if (dv > 0.0f)
			return true;
		return false;
	}

	private void setNewPosition(int i, int j, Vector3f dv) {

		if (boundary[i] == 1 && boundary[j] == 1) {
			dv.X = 0.5f * (point[i].X + point[j].X);
                        dv.Y = 0.5f * (point[i].Y + point[j].Y);
			dv.Z = 0.5f * (point[i].Z + point[j].Z);
		} else if (boundary[i] == 1 && boundary[j] != 1) {
			dv.X = point[i].X;
			dv.Y = point[i].Y;
			dv.Z = point[i].Z;

		} else if (boundary[i] != 1 && boundary[j] == 1) {
			dv.X = point[j].X;
                        dv.Y = point[j].Y;
			dv.Z = point[j].Z;
		} else {
			dummyQE.UpdateQuadric(initialV[i].error, initialV[j].error);
			if (!dummyQE.setNewPosition(dv)) {
				// // System.out.println("Error !!!");
				dv.X = 0.5f * (point[i].X + point[j].X);
				dv.Y = 0.5f * (point[i].Y + point[j].Y);
				dv.Z = 0.5f * (point[i].Z + point[j].Z);
			}
		}
	}

	private void InitV() {
		VHead = new Vertices();
		VTail = new Vertices();
		VHead.next = VTail;
		VTail.back = VHead;
	}

	private void InitT() {
		THead = new Triangle();
		TTail = new Triangle();
		THead.next = TTail;
		TTail.back = THead;
	}

	private void AppendV(int dv) {
		Vertices tmp = new Vertices(dv);
		Vertices tmp2 = VTail.back;
		tmp.back = tmp2;
		tmp2.next = tmp;
		tmp.next = VTail;
		VTail.back = tmp;
	}

	private double getEnergy(Vertices dv1, Vertices dv2) {
		double energy;
		dummyQE.UpdateQuadric(dv1.error, dv2.error);
		if (!dummyQE.setNewPosition(dummyv1)) {
			// // System.out.println("AAAA");
			dummyv1.X = (point[dv1.ID].X + point[dv2.ID].X) / 2.0f;
			dummyv1.Y = (point[dv1.ID].Y + point[dv2.ID].Y) / 2.0f;
			dummyv1.Z = (point[dv1.ID].Z + point[dv2.ID].Z) / 2.0f;
		}

		energy = dummyQE.Quadric(dummyv1);
		if (maxenergy > energy)
			maxenergy = energy - 10.0f;
		if (realmax < energy)
			realmax = energy + 10.0f;
		return energy;
	}

	private double getEnergy(int i, int j) {
		double energy;
		Vertices dv1 = initialV[i];
		Vertices dv2 = initialV[j];
		dummyQE.UpdateQuadric(dv1.error, dv2.error);
		if (!dummyQE.setNewPosition(dummyv1)) {
			dummyv1.X = (point[dv1.ID].X + point[dv2.ID].X) / 2.0f;
			dummyv1.Y = (point[dv1.ID].Y + point[dv2.ID].Y) / 2.0f;
			dummyv1.Z = (point[dv1.ID].Z + point[dv2.ID].Z) / 2.0f;
		}
		energy = dummyQE.Quadric(dummyv1);
		if (maxenergy > energy)
			maxenergy = energy - 10.0f;
		if (realmax < energy)
			realmax = energy + 10.0f;
		return energy;
	}

	private void AppendE(Vertices dv1, Vertices dv2) {
		Edge tmp = new Edge(dv1, dv2);
		tmp.ene = getEnergy(dv1, dv2);
		tmp.idx = HeapPointerN;
		Root[HeapPointerN] = tmp;
		pqueue.Put(HeapPointerN, tmp.ene);
		HeapPointerN++;
		AppendNE(tmp, NETail[dv1.ID]);
		AppendNE(tmp, NETail[dv2.ID]);
	}

	private void AppendNE(Edge dv, NEList dTail) {
		NEList tmp = new NEList(dv);
		NEList tmp2 = dTail.back;
		tmp.back = tmp2;
		tmp2.next = tmp;
		tmp.next = dTail;
		dTail.back = tmp;
	}

	private void AppendNE(Edge dv, NEList dHead, NEList dTail) {
		NEList now = dHead;
		boolean mycheck = true;

		while (next(now) != dTail) {
			now = next(now);
			if (((now.de.vp1 == dv.vp1) && (now.de.vp2 == dv.vp2))
					|| ((now.de.vp1 == dv.vp2) && (now.de.vp2 == dv.vp1))) {
				mycheck = false;
				double mymy = Root[dv.idx].ene;
				pqueue.HeapUpdate(dv.idx, maxenergy);
				int mymymy = pqueue.Pop();
				Edge dddd = Root[mymymy];

				if (dddd.idx != dv.idx) {
					// System.out.println("dv.id = " + dv.idx + " dddd.idx = " + dddd.idx + " mymymy = " + mymymy + " Old(dv).w = " + mymy);
					// System.out.println("maxenergy = " + maxenergy + " dddd.w = " + dddd.ene);
					// System.out.println("AAAAAAAAAAAAAAAAAAAAAAAAAA");
					// System.out.println("get: " + dddd.vp1.ID + " " + dddd.vp2.ID);
					// System.exit(0);
				}

				break;
			}
		}
		if (mycheck)
			AppendNE(dv, dTail);
	}

	private void AppendN(Triangle dv, Neighbor dTail) {
		Neighbor tmp = new Neighbor(dv);
		Neighbor tmp2 = dTail.back;
		tmp.back = tmp2;
		tmp2.next = tmp;
		tmp.next = dTail;
		dTail.back = tmp;
	}

	private void setTNormal(int dv1, int dv2, int dv3) {
		dummyv1.X = (point[dv2].X - point[dv1].X);
		dummyv1.Y = (point[dv2].Y - point[dv1].Y);
		dummyv1.Z = (point[dv2].Z - point[dv1].Z);
		dummyv2.X = (point[dv3].X - point[dv1].X);
		dummyv2.Y = (point[dv3].Y - point[dv1].Y);
		dummyv2.Z = (point[dv3].Z - point[dv1].Z);
		CrossVector(dummyv3, dummyv1, dummyv2);
	}

	private void AppendT(int dv1, int dv2, int dv3) {
		Triangle tmp = new Triangle(initialV[dv1], initialV[dv2], initialV[dv3]);
		setTNormal(dv1, dv2, dv3);
		tmp.setABCD(point[dv1], dummyv3);
		Triangle tmp2 = TTail.back;
		tmp.back = tmp2;
		tmp2.next = tmp;
		tmp.next = TTail;
		TTail.back = tmp;
	}

	private void Remove(Neighbor dv) {
		Neighbor tmp1 = dv.back;
		Neighbor tmp2 = dv.next;
		tmp2.back = tmp1;
		tmp1.next = tmp2;
	}

	private void Remove(NEList dv) {
		NEList tmp1 = dv.back;
		NEList tmp2 = dv.next;
		tmp2.back = tmp1;
		tmp1.next = tmp2;
	}

	private Vertices next(Vertices now) {
		return (now.next);
	}

	private Vertices back(Vertices now) {
		return (now.back);
	}

	private Triangle next(Triangle now) {
		return (now.next);
	}

	private Triangle back(Triangle now) {
		return (now.back);
	}

	private Neighbor next(Neighbor now) {
		return (now.next);
	}

	private Neighbor back(Neighbor now) {
		return (now.back);
	}

	private Edge next(Edge now) {
		return (now.next);
	}

	private Edge back(Edge now) {
		return (now.back);
	}

	private NEList next(NEList now) {
		return (now.next);
	}

	private NEList back(NEList now) {
		return (now.back);
	}

	class NEList implements java.io.Serializable {
		Edge de;
		NEList next;
		NEList back;

		public NEList() {
		}

		public NEList(Edge dv) {
			de = dv;
		}

		public int getV(int i) {
			if (de.vp1.ID == i) {
				return de.vp2.ID;
			} else if (de.vp2.ID == i) {
				return de.vp1.ID;
			} else {
				return -1;
			}
		}
	}

	class Vertices {

		Vertices next;
		Vertices back;
		int ID;
		boolean EC;
		Quadric error = null;

		public Vertices() {
			ID = -1;
			EC = false;
			error = new Quadric();
		}

		public Vertices(int dv) {
			ID = dv;
			EC = true;
			error = new Quadric();
		}

		public void addABCD(Quadric dv) {
			error.addQE(dv);
		}

		public double Quadric(Vector3f dv) {
			return error.Quadric(dv);
		}
	}

	class Quadric {
		double bx, by, bz;
		double dd;
		double a11, a12, a13, a22, a23, a33;

		public Quadric() {
			bx = 0.0f;
			by = 0.0f;
			bz = 0.0f;
			dd = 0.0f;
			a11 = 0.0f;
			a12 = 0.0f;
			a13 = 0.0f;
			a22 = 0.0f;
			a23 = 0.0f;
			a33 = 0.0f;
		}

		public Quadric(double dva, double dvb, double dvc, double dvd,
				double weight) {
			// weight = 1.0;
			bx = weight * dvd * dva;
			by = weight * dvd * dvb;
			bz = weight * dvd * dvc;
			dd = weight * dvd * dvd;
			a11 = weight * dva * dva;
			a12 = weight * dva * dvb;
			a13 = weight * dva * dvc;
			a22 = weight * dvb * dvb;
			a23 = weight * dvb * dvc;
			a33 = weight * dvc * dvc;
		}

		public boolean setNewPosition(Vector3f dv) {
			double dx, dy, dz;
			double divisor = (-a13 * a13 * a22 + 2.0 * a12 * a13 * a23 - a11
					* a23 * a23 - a12 * a12 * a33 + a11 * a22 * a33);
			// // System.out.println("divisor: "+divisor);
			// if(Math.abs(divisor)<=0.0f000001)return false;
			if (Math.abs(divisor) <= 0.000000000001)
				return false;

			dx = -((-a23 * a23 + a22 * a33) * bx + (a13 * a23 - a12 * a33) * by + (-a13
					* a22 + a12 * a23)
					* bz)
					/ divisor;
			dy = -((a13 * a23 - a12 * a33) * bx + (-a13 * a13 + a11 * a33) * by + (a12
					* a13 - a11 * a23)
					* bz)
					/ divisor;
			dz = -((-a13 * a22 + a12 * a23) * bx + (a12 * a13 - a11 * a23) * by + (-a12
					* a12 + a11 * a22)
					* bz)
					/ divisor;
			dv.X = (float)dx;
			dv.Y = (float)dy;
			dv.Z = (float)dz;
			return true;
		}

		public void normalizeQE(double dv) {
			bx /= dv;
			by /= dv;
			bz /= dv;
			dd /= dv;
			a11 /= dv;
			a12 /= dv;
			a13 /= dv;
			a22 /= dv;
			a23 /= dv;
			a33 /= dv;

		}

		public void UpdateQuadric(Quadric dv1, Quadric dv2) {
			bx = dv1.bx + dv2.bx;
			by = dv1.by + dv2.by;
			bz = dv1.bz + dv2.bz;
			dd = dv1.dd + dv2.dd;
			a11 = dv1.a11 + dv2.a11;
			a12 = dv1.a12 + dv2.a12;
			a13 = dv1.a13 + dv2.a13;
			a22 = dv1.a22 + dv2.a22;
			a23 = dv1.a23 + dv2.a23;
			a33 = dv1.a33 + dv2.a33;
		}

		public void addQE(Quadric dv) {
			bx += dv.bx;
			by += dv.by;
			bz += dv.bz;
			dd += dv.dd;
			a11 += dv.a11;
			a12 += dv.a12;
			a13 += dv.a13;
			a22 += dv.a22;
			a23 += dv.a23;
			a33 += dv.a33;
		}

		public double Quadric(Vector3f dv) {
			return (dd + 2.0 * bx * dv.X + a11 * dv.X * dv.X + 2.0 * by * dv.Y
					+ 2.0 * a12 * dv.X * dv.Y + a22 * dv.Y * dv.Y + 2.0 * bz
					* dv.Z + 2.0 * a13 * dv.X * dv.Z + 2.0 * a23 * dv.Y * dv.Z + a33
					* dv.Z * dv.Z);
		}

	}

	class PriorityQueue {
		HeapNode[] hlist;
		int[] hindex;
		int nodeN = 0;
		int hpointerM = 0;

		class HeapNode {
			double weight;
			int idx;

			public HeapNode() {
			}

			public HeapNode(int dv, double dw) {
				idx = dv;
				weight = dw;
			}
		}

		public PriorityQueue(int dv) {
			nodeN = dv;
			hlist = new HeapNode[nodeN];
			hindex = new int[nodeN];
			for (int i = 0; i < nodeN; i++) {
				hindex[i] = 0;
			}
		}

		public double getmax() {
			return hlist[0].weight;
		}

		private void swap(int i, int j) {
			HeapNode t;
			t = hlist[i];
			hlist[i] = hlist[j];
			hlist[j] = t;
			hindex[hlist[i].idx] = i;
			hindex[hlist[j].idx] = j;
		}

		private void upheap(int i) {
			int j;
			while (i != 0) {
				j = (((i) - 1) / 2);
				if (hlist[i].weight >= hlist[j].weight)
					break;
				swap(i, j);
				i = j;
			}
		}

		private void downheap(int i) {
			int j;
			while ((j = ((i) * 2 + 1)) < hpointerM) {
				if (j + 1 < hpointerM && hlist[j + 1].weight < hlist[j].weight)
					j++;
				if (hlist[j].weight >= hlist[i].weight)
					break;
				swap(i, j);
				i = j;
			}
		}

		public void Put(int idx, double w) {
			hlist[hpointerM] = new HeapNode(idx, w);
			hindex[idx] = hpointerM;
			hpointerM++;
			upheap((hpointerM - 1));
		}

		private HeapNode get() {
			HeapNode dv = hlist[0];
			hpointerM--;
			if (hpointerM < 0)
				return null;
			hlist[0] = hlist[hpointerM];
			hindex[hlist[0].idx] = 0;
			downheap(0);
			return dv;
		}

		public void HeapUpdate(int idx, double w) {
			int i;
			double oldv;
			i = hindex[idx];
			oldv = hlist[i].weight;
			hlist[i].weight = w;
			if (w < oldv) {
				upheap(i);
			} else {
				downheap(i);
			}
		}

		public int Pop() {
			HeapNode tmp = this.get();
			if (tmp == null)
				return -1;
			return tmp.idx;
		}
		/*
		 * public static void main(String[] args) { int i,j; double[] a =
		 * {0.0f,12.0,2.0,16.0,20.0f,8.0,28.0,4.0,10.0f,20.0f,6.0,18.0};
		 * PriorityQueue myheap = new PriorityQueue(12); HeapNode dum; for(i=0;i<12;i++){
		 * myheap.Put(i,a[i]); } myheap.HeapUpdate(3,1000.0f); a[3] = 10000.0f;
		 * for(i=0;i<12;i++){ dum = myheap.Pop(); // System.out.println(+i+"
		 * dum->idx = "+dum.idx+" dum->w "+dum.weight); } }
		 */
	}

	class Neighbor {
		Neighbor next;
		Neighbor back;
		Triangle dt;

		public Neighbor(Triangle dv) {
			dt = dv;

		}

		public Neighbor() {
			dt = null;
		}
	}

	class Triangle {

		Triangle next;
		Triangle back;
		boolean EC;
		Vertices v1, v2, v3;

		Quadric error;
		double area;

		public Triangle() {
			v1 = null;
			EC = false;
			v2 = null;
			v3 = null;
		}

		public Triangle(Vertices dv1, Vertices dv2, Vertices dv3) {
			v1 = dv1;
			EC = true;
			v2 = dv2;
			v3 = dv3;
		}

		public void setABCD(Vector3f dv1, Vector3f dn) {
			double size = Math.sqrt((dn.X * dn.X + dn.Y * dn.Y + dn.Z * dn.Z));
			if (size == 0.0f)
				size = 1.0;
			double dx = (dn.X / size);
			double dy = (dn.Y / size);
			double dz = (dn.Z / size);
			area = (size / 2.0);
			error = new Quadric(dx, dy, dz, (-(dv1.X * dx + dv1.Y * dy + dv1.Z
					* dz)), (size / 2.0));

			v1.addABCD(error);
			v2.addABCD(error);
			v3.addABCD(error);

		}

		public boolean HasTwoV(int di, int dj) {
			if ((v1.ID == di && v2.ID == dj) || (v1.ID == dj && v2.ID == di)
					|| (v1.ID == di && v3.ID == dj)
					|| (v1.ID == dj && v3.ID == di)
					|| (v2.ID == di && v3.ID == dj)
					|| (v2.ID == dj && v3.ID == di)) {
				return true;
			} else {
				return false;
			}
		}

		public int getV(int di, int dj) {
			if ((v1.ID == di && v2.ID == dj) || (v1.ID == dj && v2.ID == di)) {
				return v3.ID;
			} else if ((v1.ID == di && v3.ID == dj)
					|| (v1.ID == dj && v3.ID == di)) {
				return v2.ID;
			} else if ((v2.ID == di && v3.ID == dj)
					|| (v2.ID == dj && v3.ID == di)) {
				return v1.ID;
			} else {
				return -1;
			}
		}

		public int getV1(int di) {
			if (v1.ID == di) {
				return v2.ID;
			} else if (v2.ID == di) {
				return v3.ID;
			} else if (v3.ID == di) {
				return v1.ID;
			} else {
				return -1;
			}
		}

		public int getV2(int di) {
			if (v1.ID == di) {
				return v3.ID;
			} else if (v2.ID == di) {
				return v1.ID;
			} else if (v3.ID == di) {
				return v2.ID;
			} else {
				return -1;
			}
		}

		public void changeItoJ(Vertices dv1, Vertices dv2) {
			if (v1 == dv1) {
				v1 = dv2;
			} else if (v2 == dv1) {
				v2 = dv2;
			} else if (v3 == dv1) {
				v3 = dv2;
			} else {

				// System.out.println("Error 2 !!!!");
			}
		}
	}

	class Edge {
		Vertices vp1;
		Vertices vp2;
		double ene;
		int idx;
		boolean EC = true;
		Edge next = null;
		Edge back = null;

		public Edge() {
			vp1 = null;
			vp2 = null;
			ene = 0.0f;
		}

		public Edge(Vertices dv1, Vertices dv2) {
			vp1 = dv1;
			vp2 = dv2;
			ene = 0.0f;
		}

		public Edge(Vertices dv1, Vertices dv2, double dene) {
			vp1 = dv1;
			vp2 = dv2;
			ene = dene;
		}

	}
}
