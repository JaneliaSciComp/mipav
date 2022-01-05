package gov.nih.mipav.view.renderer.WildMagic.Poisson.MarchingCubes;

public class Square {
	
	public static final int CORNERS=4,EDGES=4,NEIGHBORS=4;
	
	public static int CornerIndex(final int x, final int y) {
		return (y<<1)|x;
	}
	
	public static void FactorCornerIndex(final int idx, int[] x, int[] y){
		x[0]=(idx>>0)%2;
		y[0]=(idx>>1)%2;
	}
	
	
	public static int EdgeIndex(final int orientation, final int i){
		switch(orientation){
			case 0: // x
				if( i == 0)	{return  0;} // (0,0) -> (1,0)
				else	{return  2;} // (0,1) -> (1,1)
			case 1: // y
				if(i == 0)	{return  3;} // (0,0) -> (0,1)
				else	{return  1;} // (1,0) -> (1,1)
		};
		return -1;
	}
	
	
	public static void FactorEdgeIndex(final int idx, int[] orientation,int[] i){
		switch(idx){
			case 0: case 2:
				orientation[0]=0;
				i[0]=idx/2;
				return;
			case 1: case 3:
				orientation[0]=1;
				i[0]=((idx/2)+1)%2;
				return;
		};
	}
	
	
	public static void EdgeCorners(final int idx,int[] c1,int[] c2){
		int[] orientation = new int[1];
		int[] i = new int[1];
		FactorEdgeIndex(idx,orientation,i);
		switch(orientation[0]){
			case 0:
				c1[0]=CornerIndex(0,i[0]);
				c2[0]=CornerIndex(1,i[0]);
				break;
			case 1:
				c1[0]=CornerIndex(i[0],0);
				c2[0]=CornerIndex(i[0],1);
				break;
		};
	}
	
	
	public static int ReflectEdgeIndex(final int idx, int edgeIndex){
		int orientation=edgeIndex%2;
		int[] o = new int[1];
		int[] i = new int[1];
		FactorEdgeIndex(idx,o,i);
		if(o[0]!=orientation){return idx;}
		else {
			return EdgeIndex(o[0],(i[0]+1)%2);
		}
	}
	
	public static int ReflectCornerIndex(final int idx, int edgeIndex){
		int orientation=edgeIndex%2;
		int[] x = new int[1];
		int[] y = new int[1];
		FactorCornerIndex(idx,x,y);
		switch(orientation){
			case 0:	return CornerIndex((x[0]+1)%2,y[0]);
			case 1:	return CornerIndex(x[0],(y[0]+1)%2);
		};
		return -1;
	}	
}