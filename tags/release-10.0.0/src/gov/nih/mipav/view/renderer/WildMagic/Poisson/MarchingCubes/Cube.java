package gov.nih.mipav.view.renderer.WildMagic.Poisson.MarchingCubes;

public class Cube {
	
	public static int CORNERS=8,EDGES=12,NEIGHBORS=6;
	
	public static int CornerIndex(final int x, final int y, final int z) {
		return (z<<2)|(y<<1)|x;
	}

	public static void FactorCornerIndex(final int idx,int[] x,int[] y,int[] z){
		x[0]=(idx>>0)%2;
		y[0]=(idx>>1)%2;
		z[0]=(idx>>2)%2;
	}
	
	public static int EdgeIndex( final int orientation,  final int i,  final int j) { 
		return (i | (j<<1))|(orientation<<2);
	}
	
	public static void FactorEdgeIndex( final int idx, final int[] orientation,int[] i,int[] j){
		orientation[0]=idx>>2;
		i[0]=idx&1;
		j[0]=(idx&2)>>1;
	}
	
	public static int FaceIndex(final int x, final int y, final int z){
		if		(x<0)	{return  0;}
		else if	(x>0)	{return  1;}
		else if	(y<0)	{return  2;}
		else if	(y>0)	{return  3;}
		else if	(z<0)	{return  4;}
		else if	(z>0)	{return  5;}
		else			{return -1;}
	}
	
	public static int FaceIndex(final int dir, final int offSet) {
		return (dir<<1)|offSet;
	}

	public static void FactorFaceIndex(final int idx,int[] x,int[] y,int[] z){
		x[0]=y[0]=z[0]=0;
		switch(idx){
			case 0:		x[0]=-1;	break;
			case 1:		x[0]= 1;	break;
			case 2:		y[0]=-1;	break;
			case 3:		y[0]= 1;	break;
			case 4:		z[0]=-1;	break;
			case 5:		z[0]= 1;	break;
		};
	}
	
	public static void FactorFaceIndex(final int idx,int[] dir, int[] offSet){
		dir[0]  = idx>>1;
		offSet[0]=idx &1;
	}

	public static int FaceAdjacentToEdges(final int eIndex1, final int eIndex2){
		int[] f1 = new int[1];
		int[] f2 = new int[1];
		int[] g1 = new int[1];
		int[] g2 = new int[1];
		FacesAdjacentToEdge(eIndex1,f1,f2);
		FacesAdjacentToEdge(eIndex2,g1,g2);
		if(f1[0]==g1[0] || f1[0]==g2[0]){return f1[0];}
		if(f2[0]==g1[0] || f2[0]==g2[0]){return f2[0];}
		return -1;
	}
	
	public static void FacesAdjacentToEdge(final int eIndex,int[] f1Index,int[] f2Index){
		int[] orientation = new int[1];
		int[] i1 = new int[1];
		int[] i2 = new int[1];
		FactorEdgeIndex(eIndex,orientation,i1,i2);
		i1[0]<<=1;
		i2[0]<<=1;
		i1[0]--;
		i2[0]--;
		switch(orientation[0]){
			case 0:
				f1Index[0]=FaceIndex( 0,i1[0], 0);
				f2Index[0]=FaceIndex( 0, 0,i2[0]);
				break;
			case 1:
				f1Index[0]=FaceIndex(i1[0], 0, 0);
				f2Index[0]=FaceIndex( 0, 0,i2[0]);
				break;
			case 2:
				f1Index[0]=FaceIndex(i1[0], 0, 0);
				f2Index[0]=FaceIndex( 0,i2[0], 0);
				break;
		};
	}
	
	public static void EdgeCorners(final int idx, int[] c1, int[] c2){
		int[] orientation = new int[1];
		int[] i1 = new int[1];
		int[] i2 = new int[1];
		FactorEdgeIndex(idx,orientation,i1,i2);
		switch(orientation[0]){
			case 0:
				c1[0]=CornerIndex(0,i1[0],i2[0]);
				c2[0]=CornerIndex(1,i1[0],i2[0]);
				break;
			case 1:
				c1[0]=CornerIndex(i1[0],0,i2[0]);
				c2[0]=CornerIndex(i1[0],1,i2[0]);
				break;
			case 2:
				c1[0]=CornerIndex(i1[0],i2[0],0);
				c2[0]=CornerIndex(i1[0],i2[0],1);
				break;
		};
	}
	
	public static void FaceCorners(final int idx,int[] c1,int[] c2,int[] c3,int[] c4){
		int i=idx%2;
		switch(idx/2){
		case 0:
			c1[0]=CornerIndex(i,0,0);
			c2[0]=CornerIndex(i,1,0);
			c3[0]=CornerIndex(i,0,1);
			c4[0]=CornerIndex(i,1,1);
			return;
		case 1:
			c1[0]=CornerIndex(0,i,0);
			c2[0]=CornerIndex(1,i,0);
			c3[0]=CornerIndex(0,i,1);
			c4[0]=CornerIndex(1,i,1);
			return;
		case 2:
			c1[0]=CornerIndex(0,0,i);
			c2[0]=CornerIndex(1,0,i);
			c3[0]=CornerIndex(0,1,i);
			c4[0]=CornerIndex(1,1,i);
			return;
		}
	}
	
	public static int AntipodalCornerIndex(final int idx){
		int[] x = new int[1];
		int[] y = new int[1];
		int[] z = new int[1];
		FactorCornerIndex(idx,x,y,z);
		return CornerIndex((x[0]+1)%2,(y[0]+1)%2,(z[0]+1)%2);
	}
	
	public static int FaceReflectFaceIndex(final int idx, final int faceIndex){
		if(idx/2!=faceIndex/2){return idx;}
		else{
			if(idx%2 != 0)	{return idx-1;}
			else		{return idx+1;}
		}
	}
	
	public static int FaceReflectEdgeIndex(final int idx, final int faceIndex){
		int orientation=faceIndex/2;
		int[] o = new int[1];
		int[] i = new int[1];
		int[] j = new int[1];
		FactorEdgeIndex(idx,o,i,j);
		if(o[0]==orientation){return idx;}
		switch(orientation){
			case 0:	return EdgeIndex(o[0],(i[0]+1)%2,j[0]);
			case 1:
				switch(o[0]){
					case 0:	return EdgeIndex(o[0],(i[0]+1)%2,j[0]);
					case 2:	return EdgeIndex(o[0],i[0],(j[0]+1)%2);
				};
				break;
			case 2:	return EdgeIndex(o[0],i[0],(j[0]+1)%2);
		};
		return -1;
	}
	
	public static int FaceReflectCornerIndex(final int idx, final int faceIndex){
		int orientation=faceIndex/2;
		int[] x = new int[1];
		int[] y = new int[1];
		int[] z = new int[1];
		FactorCornerIndex(idx,x,y,z);
		switch(orientation){
			case 0:	return CornerIndex((x[0]+1)%2,y[0],z[0]);
			case 1:	return CornerIndex(x[0],(y[0]+1)%2,z[0]);
			case 2: return CornerIndex(x[0],y[0],(z[0]+1)%2);
		};
		return -1;
	}
	
	public static int EdgeReflectCornerIndex(final int idx,int edgeIndex){
		int[] orientation = new int[1];
		int[] x = new int[1];
		int[] y = new int[1];
		int[] z = new int[1];
		FactorEdgeIndex(edgeIndex,orientation,x,y);
		FactorCornerIndex(idx,x,y,z);
		switch(orientation[0]){
			case 0:	return CornerIndex( x[0]     ,(y[0]+1)%2,(z[0]+1)%2);
			case 1:	return CornerIndex((x[0]+1)%2, y[0]     ,(z[0]+1)%2);
			case 2:	return CornerIndex((x[0]+1)%2,(y[0]+1)%2, z[0]     );
		};
		return -1;
	}
	
	public static int EdgeReflectEdgeIndex(int edgeIndex){
		int[] o = new int[1];
		int[] i1 = new int[1];
		int[] i2 = new int[1];
		FactorEdgeIndex(edgeIndex,o,i1,i2);
		return EdgeIndex(o[0],(i1[0]+1)%2,(i2[0]+1)%2);
	}
	
}