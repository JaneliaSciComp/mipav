package gov.nih.mipav.view.renderer.WildMagic.Poisson.MarchingCubes;




import gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry.*;

public class MarchingSquares {

	public static final int MAX_EDGES=2;
	public static double[][] vertexList = new double[Square.EDGES][2];
	// public double[][] vertexList[Square::EDGES][2];
	// public static int[] edgeMask = new int[1<<Square.CORNERS];
	/*
	0} // (0,0) -> (1,0)
	1} // (1,0) -> (1,1)
	2} // (0,1) -> (1,1)
	3} // (0,0) -> (0,1)
	*/
	public static final int[] edgeMask={
		    0, //  0 ->         ->                         ->
		    9, //  1 -> 0       -> (0,0)                   -> 0,3     ->  9 
		    3, //  2 -> 1       -> (1,0)                   -> 0,1     ->  3
		   10, //  3 -> 0,1     -> (0,0) (1,0)             -> 1,3     -> 10
		   12, //  4 -> 2       -> (0,1)                   -> 2,3     -> 12
		    5, //  5 -> 0,2     -> (0,0) (0,1)             -> 0,2     ->  5
		   15, //  6 -> 1,2     -> (1,0) (0,1)             -> 0,1,2,3 -> 15
		    6, //  7 -> 0,1,2   -> (0,0) (1,0) (0,1)       -> 1,2     ->  6
		    6, //  8 -> 3       -> (1,1)                   -> 1,2     ->  6
		   15, //  9 -> 0,3     -> (0,0) (1,1)             -> 0,1,2,3 -> 15 
		    5, // 10 -> 1,3     -> (1,0) (1,1)             -> 0,2     ->  5
		   12, // 11 -> 0,1,3   -> (0,0) (1,0) (1,1)       -> 2,3     -> 12
		   10, // 12 -> 2,3     -> (0,1) (1,1)             -> 1,3     -> 10
		    3, // 13 -> 0,2,3   -> (0,0) (0,1) (1,1)       -> 0,1     ->  3
		    9, // 14 -> 1,2,3   -> (1,0) (0,1) (1,1)       -> 0,3     ->  9
		    0, // 15 -> 0,1,2,3 -> (0,0) (1,0) (0,1) (1,1) -> 
	};
	
	// public static int[][] edges = new int[1<<Square.CORNERS][2*MAX_EDGES+1];
	public static final int[][] edges = {
			{ -1,  -1,  -1,  -1,  -1}, //
			{  3,   0,  -1,  -1,  -1}, // (0,0)
			{  0,   1,  -1,  -1,  -1}, // (1,0)
			{  3,   1,  -1,  -1,  -1}, // (0,0) (1,0)
			{  2,   3,  -1,  -1,  -1}, // (0,1)
			{  2,   0,  -1,  -1,  -1}, // (0,0) (0,1)
			{  0,   1,   2,   3,  -1}, // (1,0) (0,1)
			{  1,   2,  -1,  -1,  -1}, // (0,0) (1,0) (0,1)
			{  2,   1,  -1,  -1,  -1}, // (1,1)
			{  3,   0,   1,   2,  -1}, // (0,0) (1,1)
			{  0,   2,  -1,  -1,  -1}, // (1,0) (1,1)
			{  3,   2,  -1,  -1,  -1}, // (0,0) (1,0) (1,1)
			{  1,   3,  -1,  -1,  -1}, // (0,1) (1,1)
			{  1,   0,  -1,  -1,  -1}, // (0,0) (0,1) (1,1)
			{  0,   3,  -1,  -1,  -1}, // (1,0) (0,1) (1,1)
			{ -1,  -1,  -1,  -1,  -1}, // (0,0) (1,0) (0,1) (1,1)
		};
	
	public static int GetIndex(final double[] v, final double iso){
		int idx=0;
		for(int i=0;i<Square.CORNERS;i++) { 
			if(v[i]<iso) { 
				idx|=(1<<i);
			}
		}
		return idx;
	}

	public static boolean IsAmbiguous(final double v[], final double isoValue){
		int idx=GetIndex(v,isoValue);
		return (idx==5) || (idx==10);
	}
	
	public static int AddEdges(final double[] v, double iso, Edge[] isoEdges){
		int idx,nEdges=0;
		Edge e = new Edge();
		idx=GetIndex(v,iso);

		/* Cube is entirely in/out of the surface */
		if (edgeMask[idx] == 0) return 0;

		/* Find the vertices where the surface intersects the cube */
		int i,j,ii=1;
		for(i=0;i<12;i++){
			if( ( edgeMask[idx] & ii ) != 0 ){
				SetVertex(i,v,iso);}
			ii<<=1;
		}
		/* Create the triangle */
		for (i=0;edges[idx][i]!=-1;i+=2) {
			for(j=0;j<2;j++){
				e.p[0][j]=vertexList[edges[idx][i+0]][j];
				e.p[1][j]=vertexList[edges[idx][i+1]][j];
			}
			isoEdges[nEdges++]=e;
		}
		return nEdges;
	}
	
	public static int AddEdgeIndices(final double[] v, final double iso,int[] isoIndices){
		int idx,nEdges=0;
		idx=GetIndex(v,iso);

		/* Cube is entirely in/out of the surface */
		if (edgeMask[idx] == 0) return 0;

		/* Create the triangle */
		for(int i=0;edges[idx][i]!=-1;i+=2){
			for(int j=0;j<2;j++){isoIndices[i+j]=edges[idx][i+j];}
			nEdges++;
		}
		return nEdges;
	}
	
	public static void SetVertex(final int e, final double[] values, final double iso){
		int[] o = new int[1];
		int[] i = new int[1];
		int[] c1 = new int[1];
		int[] c2 = new int[1];
		Square.FactorEdgeIndex(e,o,i);
		Square.EdgeCorners(e,c1,c2);
		switch(o[0]){
			case 0:
				vertexList[e][0]=Interpolate(values[c1[0]]-iso,values[c2[0]]-iso);
				vertexList[e][1]=i[0];
				break;
			case 1:
				vertexList[e][1]=Interpolate(values[c1[0]]-iso,values[c2[0]]-iso);
				vertexList[e][0]=i[0];
				break;
		}
	}
	
	public static double Interpolate(final double v1, final double v2){ 
		return v1/(v1-v2);
	}
	
}


