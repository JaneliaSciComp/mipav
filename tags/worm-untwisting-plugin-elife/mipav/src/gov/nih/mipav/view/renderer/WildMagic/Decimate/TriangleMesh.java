package gov.nih.mipav.view.renderer.WildMagic.Decimate;

import java.io.*;

import WildMagic.LibFoundation.Mathematics.*;

import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public class TriangleMesh {
	double maxX = 0.0;
	double maxY = 0.0;
	double maxZ = 0.0;
	double minX = 0.0;
	double minY = 0.0;
	double minZ = 0.0;

	Mesh myobj = null;

	boolean dipli = false;
	Vector3f[] point = null;

    double aveH;
	double sdviH;
	double[] H = null;

	double minarea = 1000.0;
	double threshold = 1.5;
	int[] index = null;
	Vector3f[] laplace = null;

	Vector3f[] bilaplace = null;
	Vector3f[] KnotInit = null;

	int numberV = 0;
	int numberF = 0;
	int numberE = 0;
	int EulerC = 0;
	double scaler = 1.0;

	/* true is original, false is skeleton */
	IDList BEHead = null;
	IDList BETail = null;
	int[][] FaceArray;
	int[] neighborI = null;
	int[] neighborF = null;
	int[] neighborV = null;
	IDList[] FHead = null;
	IDList[] FTail = null;
	IDList[] VHead = null;
	IDList[] VTail = null;
	IDList[] IHead = null;
	IDList[] ITail = null;
	int[] boundary = null;
	int numboundary = 0;
	/* Subdivide Attribute */
	Vector3f[] subpoint = null;
	int[][] subFace = null;
	/* GH-Decimation Attribute */

	LODMesh mydecimate = null;

	Vector3f tmppoint3d1 = new Vector3f(0.0f, 0.0f, 0.0f);
	Vector3f tmppoint3d2 = new Vector3f(0.0f, 0.0f, 0.0f);
	Vector3f Wvex = new Vector3f(0.0f, 0.0f, 0.0f);

	int[] ffR = null;

	int numberVOrig, numberFOrig;
	Vector3f[] pointOrig = null;
	int[][] FaceOrig = null;
	String meshName = null;

	public TriangleMesh(VertexBuffer pkVBuffer, IndexBuffer pkIBuffer) {
		int dnumV = 0, dnumF = 0;

		dnumV = pkVBuffer.GetVertexQuantity();
		numberVOrig = dnumV;
		pointOrig = new Vector3f[numberVOrig];
		dnumF = pkIBuffer.GetIndexQuantity() / 3;
		numberFOrig = dnumF;
		FaceOrig = new int[numberFOrig][3];
		memoryallocation(dnumV, dnumF);
		ffR = new int[dnumF];
		int i;
		double dx, dy, dz;
		double maxvalue = 0.0;
		maxX = 0.0;
		maxY = 0.0;
		maxZ = 0.0;
		minX = 0.0;
		minY = 0.0;
		minZ = 0.0;
		boolean firstcheck = true;
		float[] vertex;
		for (i = 0; i < numberV; i++) {
			vertex = pkVBuffer.GetVertex(i);
			dx = vertex[0];
			dy = vertex[1];
			dz = vertex[2];
			InitializeData(i, dx, dy, dz);
			pointOrig[i] = new Vector3f((float)dx, (float)dy, (float)dz);
			if (firstcheck) {
				firstcheck = false;
				maxX = dx;
				maxY = dy;
				maxZ = dz;
				minX = dx;
				minY = dy;
				minZ = dz;

			} else {
				if (maxX < dx)
					maxX = dx;
				if (maxY < dy)
					maxY = dy;
				if (maxZ < dz)
					maxZ = dz;
				if (minX > dx)
					minX = dx;
				if (minY > dy)
					minY = dy;
				if (minZ > dz)
					minZ = dz;

			}
			if (maxvalue < Math.abs(dx))
				maxvalue = Math.abs(dx);
			if (maxvalue < Math.abs(dy))
				maxvalue = Math.abs(dy);
			if (maxvalue < Math.abs(dz))
				maxvalue = Math.abs(dz);
		}

		if (maxvalue != 0.0) {
			ReScale((1.0 / maxvalue));
		}

		int di, dj, dk;
		int[] indexData = pkIBuffer.GetData();
		for (i = 0; i < numberF; i++) {
			di = indexData[3 * i];
			dj = indexData[3 * i + 1];
			dk = indexData[3 * i + 2];
			ffR[i] = -1;
			MakeFace(i, di, dj, dk);
			FaceOrig[i][0] = di;
			FaceOrig[i][1] = dj;
			FaceOrig[i][2] = dk;

		}
		MakeArrayObj(3);
		SetBoundaryStrips();
		setEdgeN();
		PropertyUpdate();
	}

	public void SetName(String name) {
		meshName = name;
	}
	
	public String GetName() {
		return meshName;
	}
	
	public void doDecimation(double dv) throws Exception {
		try {
			Decimation(dv);
		} catch (Exception e) {

		}
		ffR = new int[numberF];
		for (int i = 0; i < numberF; i++)
			ffR[i] = -1;
	}

	public void ReScale(double dv) {
		// scaler = dv;
		scaler = 1.0;

	}

	public void setEdgeN() {
		int i = 0;
		numberE = 0;
		for (i = 0; i < numberV; i++) {
			numberE += (neighborI[i]);
		}
		numberE /= 2;
		EulerC = numberV - numberE + numberF;
		// System.out.println("V = " + numberV + ", F = " + numberF + ", E = " + numberE);
		// System.out.println("EulerC = " + EulerC);

	}

	public void InitializeData(int di, double dx, double dy, double dz) {
		index[di] = di;
		point[di] = new Vector3f((float)dx, (float)dy, (float)dz);

		laplace[di] = new Vector3f(0.0f, 0.0f, 0.0f);
		bilaplace[di] = new Vector3f(0.0f, 0.0f, 0.0f);
		H[di] = 0.0;

		neighborV[di] = 0;
		neighborF[di] = 0;
		neighborI[di] = 0;
		VHead[di] = new IDList();
		VTail[di] = new IDList();
		this.Init(VHead[di], VTail[di]);
		FHead[di] = new IDList();
		FTail[di] = new IDList();
		this.Init(FHead[di], FTail[di]);
		IHead[di] = new IDList();
		ITail[di] = new IDList();
		this.Init(IHead[di], ITail[di]);
		KnotInit[di] = new Vector3f((float)dx, (float)dy, (float)dz);

	}

	public void memoryallocation(int dnumberV, int dnumberF) {
		numberV = dnumberV;
		numberF = dnumberF;

		index = new int[numberV];
		point = new Vector3f[numberV];

		H = new double[numberV];

		boundary = new int[numberV];
		neighborV = new int[numberV];
		neighborF = new int[numberV];
		neighborI = new int[numberV];
		VHead = new IDList[numberV];
		VTail = new IDList[numberV];
		FHead = new IDList[numberV];
		FTail = new IDList[numberV];
		IHead = new IDList[numberV];
		ITail = new IDList[numberV];
		KnotInit = new Vector3f[numberV];

		laplace = new Vector3f[numberV];
		bilaplace = new Vector3f[numberV];
		FaceArray = new int[numberF][3];
	}
	
	/**
     * Dispose the local memory.
     */
	public void dispose() {
		for ( int i = 0; i < numberV; i++ ) {
			point[i] = null;
			laplace[i] = null;
			bilaplace[i] = null;
			VHead[i] = null;
			VTail[i] = null;
			FHead[i] = null;
			FTail[i] = null;
			IHead[i] = null;
			ITail[i] = null;
			KnotInit[i] = null;
		}
		index = null;
		point = null;
		H = null;
		boundary = null;
		neighborV = null;
		neighborF = null;
		neighborI = null;
		VHead = null;
		VTail = null;
		FHead = null;
		FTail = null;
		IHead = null;
		ITail = null;
		KnotInit = null;
		laplace = null;
		bilaplace = null;
		FaceArray = null;
		
		subpoint = null;
		subFace = null;
		
		if ( myobj != null ) {
			myobj.dispose();
			myobj = null;
		}
		
		if ( mydecimate != null ) {
			mydecimate.dispose();
			mydecimate = null;
		}
	}
	

	public void Init(IDList h, IDList t) {
		h.next = new IDList();
		t.back = new IDList();
		h.next = t;
		t.back = h;
	}

	public IDList next(IDList now) {
		return (now.next);
	}

	public IDList back(IDList now) {
		return (now.back);
	}

	public void AppendVF(int myID, IDList dVTail) {
		IDList now = new IDList(myID);

		IDList dummy = dVTail.back;
		now.next = dVTail;
		dVTail.back = now;
		now.back = dummy;
		dummy.next = now;
	}

	private boolean SearchI(int dID, IDList dIHead, IDList dITail) {

		IDList now = dIHead;
		while (next(now) != dITail) {
			now = next(now);
			if ((now.ID == dID)) {
				return true;
			}
		}
		return false;
	}

	public void AppendI(int dID, IDList dIHead, IDList dITail, int nowID,
			int[] dnum) {
		if (dID != nowID) {
			if (!SearchI(dID, dIHead, dITail)) {
				IDList now = new IDList(dID);
				IDList dummy = dITail.back;
				now.next = dITail;
				dITail.back = now;
				now.back = dummy;
				dummy.next = now;
				dnum[nowID]++;
			}
		} else {
			// // System.out.println("Check !!!");
		}
	}

	public void MakeFace(int i, int di, int dj, int dk) {
		FaceArray[i][0] = di;
		FaceArray[i][1] = dj;
		FaceArray[i][2] = dk;
		/* One */
		this.AppendVF(i, FTail[di]);
		neighborF[di]++;
		this.AppendI(dj, IHead[di], ITail[di], di, neighborI);
		this.AppendI(dk, IHead[di], ITail[di], di, neighborI);
		this.AppendVF(dj, VTail[di]);
		neighborV[di]++;
		this.AppendVF(dk, VTail[di]);
		neighborV[di]++;
		/* Two */
		this.AppendVF(i, FTail[dj]);
		neighborF[dj]++;
		this.AppendI(di, IHead[dj], ITail[dj], dj, neighborI);
		this.AppendI(dk, IHead[dj], ITail[dj], dj, neighborI);
		this.AppendVF(dk, VTail[dj]);
		neighborV[dj]++;
		this.AppendVF(di, VTail[dj]);
		neighborV[dj]++;
		/* Three */
		this.AppendVF(i, FTail[dk]);
		neighborF[dk]++;
		this.AppendI(di, IHead[dk], ITail[dk], dk, neighborI);
		this.AppendI(dj, IHead[dk], ITail[dk], dk, neighborI);
		this.AppendVF(di, VTail[dk]);
		neighborV[dk]++;
		this.AppendVF(dj, VTail[dk]);
		neighborV[dk]++;

	}

	public void SetBoundaryStrips() {
		int i = 0;
		IDList now;
		int[] dummyboundary = null;
		numboundary = 0;
		if (neighborI != null && neighborF != null && boundary != null) {
			dummyboundary = new int[numberV];
			for (i = 0; i < numberV; i++) {
				if (((neighborI[i]) == neighborF[i]) && (neighborF[i] != 0)
						&& (neighborI[i] != 0)) {
					dummyboundary[i] = 0;
				} else {
					dummyboundary[i] = 1;
				}
				boundary[i] = 0;
			}

			for (i = 0; i < numberV; i++) {
				if (dummyboundary[i] == 1) {
					boundary[i] = 1;
					// deeplevel[i] = 1;
					numboundary++;
					now = VHead[i];
					while (next(now) != VTail[i]) {
						now = next(now);
						if (boundary[now.ID] != 1)
							boundary[now.ID] = 2;
						now = next(now);
					}
				}
			}
		}
	}

	public void MakeArrayObj(int dv) {

		myobj = new Mesh(dv);

		this.MakeArrayData();
	}

	public void MakeArrayData() {
		int j = 0;
		// myobj.reNew(numberV, numberF, scaler);
		myobj.reNew(numberV, numberF, 1.0);

		/* old shadings */
		for (j = 0; j < numberF; j++) {
			myobj.MakeFace(j, FaceArray[j][0], FaceArray[j][1], FaceArray[j][2], point);

		}
		// end new shading

	}

	public void UpdatePolygon() {
		int j = 0;
		
		for (j = 0; j < numberF; j++) {
			myobj.MakeFace(j, FaceArray[j][0], FaceArray[j][1], FaceArray[j][2], point);

		}

	}

	public void PropertyUpdate() {
		int i, j;
		Vector3f[] dv = new Vector3f[9];
		IDList now;
		boolean secondfirst = true;
		for (i = 0; i < 9; i++)
			dv[i] = new Vector3f(0.0f, 0.0f, 0.0f);
		double tarea = 0.0;
		double area = 0.0;
		aveH = 0.0;
		sdviH = 0.0;
		double angle = 0.0;
		double anglesum = 0.0;
		double divisor = 0.0;
		double dummytemp = 0.0;
		double InnW1 = 0.0;
		double InnW2 = 0.0;
		double dl1 = 0.0;
		double dl2 = 0.0;
		for (i = 0; i < numberV; i++) {
			if (boundary[i] != 1) {
				divisor += 1.0;
				now = VHead[i];
				area = 0.0;
				area = 0.0;
				tarea = 0.0;
				anglesum = 0.0;
				for (j = 0; j < 9; j++) {
					dv[j].X = 0.0f;
					dv[j].Y = 0.0f;
					dv[j].Z = 0.0f;
				}

				while (next(now) != VTail[i]) {
					now = next(now);
					this.makeVector(dv[0], point[i], point[next(now).ID]);
					this.makeVector(dv[1], point[next(now).ID], point[now.ID]);
					this.makeVector(dv[2], point[i], point[now.ID]);
					this.makeVector(dv[3], point[now.ID], point[next(now).ID]);
					this.CrossVector(dv[4], dv[2], dv[0]);

					dl1 = Point3dSize(dv[2]);
					dl2 = Point3dSize(dv[0]);
					if (dl1 == 0.0)
						dl1 = 1.0;
					if (dl2 == 0.0)
						dl2 = 1.0;

					area = ((1.0 / 2.0) * (Point3dSize(dv[4])));
					angle = Math
							.acos((InnerProduct(dv[2], dv[0]) / (dl1 * dl2)));
					anglesum += angle;
					InnW1 = InnerProduct(dv[0], dv[1]);
					InnW2 = InnerProduct(dv[2], dv[3]);
					tarea += area;
					this.ScalarVector(dv[5], InnW1, dv[2]);
					this.ScalarVector(dv[6], InnW2, dv[0]);
					if (area != 0.0) {
						dv[7].X = (float)(dv[7].X + ((1.0 / (4.0 * area)) * (dv[5].X + dv[6].X)));
						dv[7].Y = (float)(dv[7].Y + ((1.0 / (4.0 * area)) * (dv[5].Y + dv[6].Y)));
						dv[7].Z = (float)(dv[7].Z + ((1.0 / (4.0 * area)) * (dv[5].Z + dv[6].Z)));
						dv[8].X = dv[8].X + dv[4].X;
						dv[8].Y = dv[8].Y + dv[4].Y;
						dv[8].Z = dv[8].Z + dv[4].Z;

					}
					now = next(now);
				}

				dummytemp = Point3dSize(dv[8]);
				if (dummytemp != 0.0) {
					tmppoint3d1.X = (float)(((dv[8].X) / dummytemp));
					tmppoint3d1.Y = (float)(((dv[8].Y) / dummytemp));
					tmppoint3d1.Z = (float)(((dv[8].Z) / dummytemp));
				}

				if (tarea != 0.0) {
					tmppoint3d2.X = (float)(-((dv[7].X) / (2.0 * tarea)));
					tmppoint3d2.Y = (float)(-((dv[7].Y) / (2.0 * tarea)));
					tmppoint3d2.Z = (float)(-((dv[7].Z) / (2.0 * tarea)));
				}
				// K[i] = 3.0*(2.0*Math.PI - anglesum)/(tarea);

				H[i] = 3.0 * InnerProduct(tmppoint3d1, tmppoint3d2);
			} else {
				H[i] = 0.0;
			}

			if (secondfirst) {
				secondfirst = false;

				aveH = H[i];
				sdviH = (H[i] * H[i]);
			} else {

				aveH += H[i];
				sdviH += (H[i] * H[i]);
			}
		}

		aveH /= divisor;
		sdviH -= (aveH * aveH);
		sdviH /= divisor;
		sdviH = Math.sqrt(sdviH);
	}

	private void UpdatePolyStructure() {
		memoryallocation(numberV, numberF);
		int i;
		// System.out.println("Init finish.");
		double dx = 0.0;
		double dy = 0.0;
		double dz = 0.0;
		double maxvalue = 0.0;
		for (i = 0; i < numberV; i++) {
			dx = subpoint[i].X;
			dy = subpoint[i].Y;
			dz = subpoint[i].Z;
			if (maxvalue < Math.abs(dx))
				maxvalue = Math.abs(dx);
			if (maxvalue < Math.abs(dy))
				maxvalue = Math.abs(dy);
			if (maxvalue < Math.abs(dz))
				maxvalue = Math.abs(dz);
			InitializeData(i, dx, dy, dz);
		}
		// System.out.println("Now Finish Read Vertices.");
		if (maxvalue != 0.0) {
			// scaler = 1.0/maxvalue;
			// System.out.println("scaler = " + scaler);
			/*
			 * for(i=0;i<numberV;i++){ point[i].x *= scaler; point[i].y *=
			 * scaler; point[i].z *= scaler; KnotInit[i].x *= scaler;
			 * KnotInit[i].y *= scaler; KnotInit[i].z *= scaler; }
			 */
		}
		// System.out.println("Finish Scaling");
		int di, dj, dk;

		for (i = 0; i < numberF; i++) {
			di = subFace[i][0];
			dj = subFace[i][1];
			dk = subFace[i][2];
			MakeFace(i, di, dj, dk);
		}

		// System.out.println("Finish Read Faces.");
		this.SetBoundaryStrips();

		MakeArrayData();
		// System.out.println("Finish Update Data Section.");

	}

	private void UpdatePolyStructureToOrig() {
		memoryallocation(numberVOrig, numberFOrig);
		int i;
		double dx = 0.0;
		double dy = 0.0;
		double dz = 0.0;
		double maxvalue = 0.0;
		for (i = 0; i < numberVOrig; i++) {
			dx = pointOrig[i].X;
			dy = pointOrig[i].Y;
			dz = pointOrig[i].Z;
			if (maxvalue < Math.abs(dx))
				maxvalue = Math.abs(dx);
			if (maxvalue < Math.abs(dy))
				maxvalue = Math.abs(dy);
			if (maxvalue < Math.abs(dz))
				maxvalue = Math.abs(dz);
			InitializeData(i, dx, dy, dz);
		}
		if (maxvalue != 0.0) {
			// scaler = 1.0/maxvalue;
			/*
			 * for(i=0;i<numberV;i++){ point[i].x *= scaler; point[i].y *=
			 * scaler; point[i].z *= scaler; KnotInit[i].x *= scaler;
			 * KnotInit[i].y *= scaler; KnotInit[i].z *= scaler; }
			 */
		}
		int di, dj, dk;

		for (i = 0; i < numberFOrig; i++) {
			di = FaceOrig[i][0];
			dj = FaceOrig[i][1];
			dk = FaceOrig[i][2];
			MakeFace(i, di, dj, dk);
		}

		// System.out.println("Finish Read Faces to Origin.");
		SetBoundaryStrips();

		MakeArrayData();
		// System.out.println("Finish Update to Origin Data Section .");

	}

	

	public void makeVector(Vector3f out, Vector3f in1, Vector3f in2) {
		out.X = (in2.X - in1.X);
		out.Y = (in2.Y - in1.Y);
		out.Z = (in2.Z - in1.Z);
	}

	public void CrossVector(Vector3f out, Vector3f in1, Vector3f in2) {
		out.X = (in1.Y * in2.Z - in2.Y * in1.Z);
		out.Y = (in1.Z * in2.X - in2.Z * in1.X);
		out.Z = (in1.X * in2.Y - in2.X * in1.Y);
	}

	public double InnerProduct(Vector3f in1, Vector3f in2) {
		return (in1.X * in2.X + in1.Y * in2.Y + in1.Z * in2.Z);
	}

	public void ScalarVector(Vector3f out, double dv, Vector3f in) {
		out.X = (float)(dv * in.X);
                out.Y = (float)(dv * in.Y);
		out.Z = (float)(dv * in.Z);
	}

	public double Point3dSize(Vector3f in) {
		return Math.sqrt(((in.X * in.X) + (in.Y * in.Y) + (in.Z * in.Z)));
	}

	public double Distance(Vector3f in1, Vector3f in2) {
		return Math
				.sqrt((((in1.X - in2.X) * (in1.X - in2.X))
						+ ((in1.Y - in2.Y) * (in1.Y - in2.Y)) + ((in1.Z - in2.Z) * (in1.Z - in2.Z))));

	}

	public void Decimation(double ddrp) throws IOException {
		int reduceN;
		int i;

		reduceN = ((int) ((double) ((ddrp / 100.0) * ((double) (numberV)))));

		if (numberV - reduceN <= 3)
			reduceN = numberV - 4;

		numberVOrig = numberV;
		numberFOrig = numberF;

		// System.out.println("Reduction Ratio: " + ddrp + " %");
		// System.out.println("May be V:" + numberV + " -> " + (numberV - reduceN));
		/* Make Intermediate Data structure and priority box */
		mydecimate = new LODMesh(numberV, numberF, FaceArray, point, boundary);
		mydecimate.Decimation((numberV - reduceN));

		/* Update Data Structure */
		subpoint = new Vector3f[mydecimate.deciV];
		subFace = new int[mydecimate.deciF][3];
		// System.out.println("mydecimate.deciF = " + mydecimate.deciF);
		// System.out.println("mydecimate.deciV = " + mydecimate.deciV);
		int[] dddindex = new int[mydecimate.deciV];
		for (i = 0; i < mydecimate.deciV; i++)
			dddindex[i] = 0;
		mydecimate.getVFace(subpoint, subFace, dddindex);

		numberV = mydecimate.deciV;
		numberF = mydecimate.deciF;
		numberE = numberV + numberF - EulerC;

		// System.out.println("");
		// System.out.println("New V:" + numberV + " F:" + numberF);
		UpdatePolyStructure();
		for (i = 0; i < numberV; i++)
			index[i] = dddindex[i];
		PropertyUpdate();
		dddindex = null;

		UpdatePolygon();
		UpdatePolyStructureToOrig();
	}

	public int getFaceQuatityOrig() {
		return numberFOrig;
	}

	public int getFaceQuatity() {
		return numberF;
	}

	public VertexBuffer getDecimatedVBuffer() {
		VertexBuffer pkVBuffer;

		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		kAttr.SetTChannels(0, 3);
		kAttr.SetCChannels(0, 4);

		pkVBuffer = new VertexBuffer(kAttr, mydecimate.deciV);

		for (int i = 0; i < mydecimate.deciV; i++) {
			pkVBuffer.SetPosition3(i, (float) subpoint[i].X,
					(float) subpoint[i].Y, (float) subpoint[i].Z);
			pkVBuffer.SetColor4(0, i, 1.0f, 1.0f, 1.0f, 1.0f);
		}
		return pkVBuffer;
	}

	public IndexBuffer getDecimatedIBuffer() {
		IndexBuffer pkIBuffer;
		int[] aiIndex = new int[mydecimate.deciF * 3];
		for (int i = 0; i < mydecimate.deciF; i++) {
			aiIndex[3 * i] = subFace[i][0];
			aiIndex[3 * i + 1] = subFace[i][1];
			aiIndex[3 * i + 2] = subFace[i][2];

		}
		pkIBuffer = new IndexBuffer(mydecimate.deciF * 3, aiIndex);
		return pkIBuffer;
	}

	public Vector3f[] getDecimatedPoints() {
		return subpoint;
	}

	public int[][] getDecimatedFace() {
		return subFace;
	}

	public int getFace(int i, int j, int fi) {
		IDList now = FHead[i];
		int tID = -1;
		while (next(now) != FTail[i]) {
			now = next(now);
			if (now.ID != fi) {
				if ((FaceArray[now.ID][0] == i && FaceArray[now.ID][1] == j)
						|| (FaceArray[now.ID][1] == i && FaceArray[now.ID][0] == j)
						|| (FaceArray[now.ID][0] == i && FaceArray[now.ID][2] == j)
						|| (FaceArray[now.ID][2] == i && FaceArray[now.ID][0] == j)
						|| (FaceArray[now.ID][2] == i && FaceArray[now.ID][1] == j)
						|| (FaceArray[now.ID][1] == i && FaceArray[now.ID][2] == j)) {

					tID = now.ID;
					break;
				}

			}

		}
		return tID;
	}

}
