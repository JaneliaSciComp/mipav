package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class LaplacianMatrixFunction extends TerminatingNodeAdjacencyFunction {
	
	int x2,y2,z2,d2;
	Octree ot;
	int[] index = new int[Octree.DIMENSION];
	int[] scratch = new int[Octree.DIMENSION];
	int elementCount,offset;
	MatrixEntry[] rowElements;
	
	public void print() {
		for ( int i = 0; i < elementCount; i++ ) {
			System.err.println("rowElment["+ i + "]= " + rowElements[i].Value);
		}
	}
	
	public int Function( final OctNode node1, final OctNode node2){
		float temp;
		int d1= (int)(node1.d[0]);
		int x1,y1,z1;
		x1= (int)(node1.off[0]);
		y1= (int)(node1.off[1]);
		z1= (int)(node1.off[2]);
		int dDepth=d2-d1;
		int d;
		d=(x2>>dDepth)-x1;
		if(d<0){return 0;}
		if(dDepth == 0){
			if( d == 0){
				d=y2-y1;
				if(d<0){return 0;}
				else if( d == 0){
					d=z2-z1;
					if(d<0){return 0;}
				}
			}
			scratch[0]=FunctionData.SymmetricIndex(index[0],x1);
			scratch[1]=FunctionData.SymmetricIndex(index[1],y1);
			scratch[2]=FunctionData.SymmetricIndex(index[2],z1);
			temp=ot.GetLaplacian(scratch);
			if(node1==node2){temp/=2;}
			if((float)Math.abs(temp)>Octree.EPSILON){
				rowElements[elementCount].Value=temp;
				rowElements[elementCount].N=node1.nodeData.nodeIndex-offset;
				/*
				System.err.println("node1.nodeData.nodeIndex = " + node1.nodeData.nodeIndex + " offset = " + offset);
				System.err.println("rowElements[" + elementCount + "].value = " + rowElements[elementCount].Value + 
						"   rowElements[" + elementCount + "].N = " + rowElements[elementCount].N);
				Octree.pause();
				*/
				elementCount++;
			}
			return 0;
		}
		return 1;
	}
	
	
}