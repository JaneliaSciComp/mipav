package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.MarchingCubes.*;

public class VertexData {
	
	public static long CenterIndex(final OctNode node, final int maxDepth){
		int[] idx = new int[Octree.DIMENSION];
		return CenterIndex(node,maxDepth,idx);
	}
	
	public static long CenterIndex(final OctNode node, final int maxDepth,final int idx[]){
		int[] d = new int[1];
		int[] o = new int[3];
		node.depthAndOffset(d,o);
		for(int i=0;i<Octree.DIMENSION;i++){
			idx[i]=BinaryNode.CornerIndex(maxDepth+1,d[0]+1,o[i]<<1,1);}
		return (long)(idx[0]) | (long)(idx[1])<<15 | (long)(idx[2])<<30;
	}
	
	public static long CenterIndex(final int depth, final int offSet[], final int maxDepth,int idx[]){
		for(int i=0;i<Octree.DIMENSION;i++){
			idx[i]=BinaryNode.CornerIndex(maxDepth+1,depth+1,offSet[i]<<1,1);}
		return (long)(idx[0]) | (long)(idx[1])<<15 | (long)(idx[2])<<30;
	}
	
	public static long CornerIndex(final OctNode node, final int cIndex, final int maxDepth){
		int[] idx = new int[Octree.DIMENSION];
		return CornerIndex(node,cIndex,maxDepth,idx);
	}
	
	
	public static long CornerIndex(final OctNode node, final int cIndex, final int maxDepth,int idx[]){
		int[] x = new int[Octree.DIMENSION];
		int[] x0 = new int[1];
		int[] x1 = new int[1];
		int[] x2 = new int[1];
		
		Cube.FactorCornerIndex(cIndex,x0,x1,x2);
		x[0] = x0[0];
		x[1] = x1[0];
		x[2] = x2[0];
		int[] d = new int[1];
		int[] o = new int[3];
		node.depthAndOffset(d,o);
		for(int i=0;i<Octree.DIMENSION;i++){
			idx[i]=BinaryNode.CornerIndex(maxDepth+1,d[0],o[i],x[i]);}
		return (long)(idx[0]) | (long)(idx[1])<<15 | (long)(idx[2])<<30;
	}
	
	
	public static long CornerIndex(final int depth, final int offSet[], final int cIndex, final int maxDepth,int idx[]){
		int x[] = new int[Octree.DIMENSION];
		int[] x0 = new int[1];
		int[] x1 = new int[1];
		int[] x2 = new int[1];
		Cube.FactorCornerIndex(cIndex,x0,x1,x2);
		x[0] = x0[0];
		x[1] = x1[0];
		x[2] = x2[0];
		for(int i=0;i<Octree.DIMENSION;i++){idx[i]=BinaryNode.CornerIndex(maxDepth+1,depth,offSet[i],x[i]);}
		return (long)(idx[0]) | (long)(idx[1])<<15 | (long)(idx[2])<<30;
	}
	
	public static long FaceIndex(final OctNode node, final int fIndex, final int maxDepth){
		int[] idx = new int[Octree.DIMENSION];
		return FaceIndex(node,fIndex,maxDepth,idx);
	}
	
	
	public static long FaceIndex(final OctNode node, final int fIndex, final int maxDepth,int idx[]){
		int[] dir = new int[1];
		int[] offset = new int[1];
		Cube.FactorFaceIndex(fIndex,dir,offset);
		int[] d = new int[1];
		int[] o = new int[3];
		node.depthAndOffset(d,o);
		for(int i=0;i<Octree.DIMENSION;i++){idx[i]=BinaryNode.CornerIndex(maxDepth+1,d[0]+1,o[i]<<1,1);}
			idx[dir[0]]=BinaryNode.CornerIndex(maxDepth+1,d[0],o[dir[0]],offset[0]);
		return (long)(idx[0]) | (long)(idx[1])<<15 | (long)(idx[2])<<30;
	}
	
	
	public static long EdgeIndex(final OctNode node, final int eIndex, final int maxDepth){
		int[] idx = new int[Octree.DIMENSION];
		return EdgeIndex(node,eIndex,maxDepth,idx);
	}
	
	
	public static long EdgeIndex(final OctNode node, final int eIndex, final int maxDepth,int idx[]){
		int[] o = new int[1];
		int[] i1 = new int[1];
		int[] i2 = new int[1];
		int[] d= new int[1];
		int[] offf= new int[3];
		node.depthAndOffset(d,offf);
		for(int i=0;i<Octree.DIMENSION;i++){idx[i]=BinaryNode.CornerIndex(maxDepth+1,d[0]+1,offf[i]<<1,1);}
		Cube.FactorEdgeIndex(eIndex,o,i1,i2);
		switch(o[0]){
			case 0:
				idx[1]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[1],i1[0]);
				idx[2]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[2],i2[0]);
				break;
			case 1:
				idx[0]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[0],i1[0]);
				idx[2]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[2],i2[0]);
				break;
			case 2:
				idx[0]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[0],i1[0]);
				idx[1]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[1],i2[0]);
				break;
		};
		return (long)(idx[0]) | (long)(idx[1])<<15 | (long)(idx[2])<<30;
	}

	
}