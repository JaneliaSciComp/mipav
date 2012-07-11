package gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry;

import java.util.*;

public class CoredVectorMeshData extends CoredMeshData {

	public Vector<Point3D> oocPoints = new Vector<Point3D>();
	public Vector<TriangleIndex> triangles = new Vector<TriangleIndex>();
	public int oocPointIndex,triangleIndex;

	public CoredVectorMeshData() {
		oocPointIndex=triangleIndex=0;
	}

	public void resetIterator() {
		oocPointIndex=triangleIndex=0;
	}

	public int addOutOfCorePoint(final Point3D p){
		oocPoints.add(p);
		return (int)(oocPoints.size())-1;
	}
	
	public int addTriangle(final TriangleIndex t, final int coreFlag){
		TriangleIndex tt = new TriangleIndex();
		if((coreFlag & IN_CORE_FLAG[0]) != 0 )	{tt.idx[0]= t.idx[0];}
		else											{tt.idx[0]=-t.idx[0]-1;}
		if((coreFlag & IN_CORE_FLAG[1]) != 0 )	{tt.idx[1]= t.idx[1];}
		else											{tt.idx[1]=-t.idx[1]-1;}
		if((coreFlag & IN_CORE_FLAG[2]) != 0)	{tt.idx[2]= t.idx[2];}
		else											{tt.idx[2]=-t.idx[2]-1;}
		triangles.add(tt);
		return (int)(triangles.size())-1;
	}
	
	public int nextOutOfCorePoint(Point3D p){
		if(oocPointIndex<(int)(oocPoints.size())){
			// p=oocPoints.get(oocPointIndex++);
			p.coords[0]=oocPoints.get(oocPointIndex).coords[0];
			p.coords[1]=oocPoints.get(oocPointIndex).coords[1];
			p.coords[2]=oocPoints.get(oocPointIndex).coords[2];
			oocPointIndex++;
			return 1;
		}
		else{return 0;}
	}
	
	public int nextTriangle(TriangleIndex t,int[] inCoreFlag){
		inCoreFlag[0]=0;
		if(triangleIndex<(int)(triangles.size())){
			// t=triangles.get(triangleIndex++);
			t.idx[0]=triangles.get(triangleIndex).idx[0];
			t.idx[1]=triangles.get(triangleIndex).idx[1];
			t.idx[2]=triangles.get(triangleIndex).idx[2];
			triangleIndex++;
			if(t.idx[0]<0)	{t.idx[0]=-t.idx[0]-1;}
			else			{inCoreFlag[0]|=IN_CORE_FLAG[0];}
			if(t.idx[1]<0)	{t.idx[1]=-t.idx[1]-1;}
			else			{inCoreFlag[0]|=IN_CORE_FLAG[1];}
			if(t.idx[2]<0)	{t.idx[2]=-t.idx[2]-1;}
			else			{inCoreFlag[0]|=IN_CORE_FLAG[2];}
			return 1;
		}
		else{return 0;}
	}
	public int outOfCorePointCount() {
		return (int)(oocPoints.size());
	}
	
	public int triangleCount() {
		return (int)(triangles.size());
	}
	
	public int nextTriangle(TriangleIndex t, int inCoreFlag){return 0;};
}
