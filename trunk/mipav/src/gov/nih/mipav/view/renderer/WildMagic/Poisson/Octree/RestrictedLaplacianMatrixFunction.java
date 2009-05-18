package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class RestrictedLaplacianMatrixFunction extends TerminatingNodeAdjacencyFunction{
	
	public int depth;
	public int[] offset = new int[3];
	public Octree ot;
	public float radius;
	public int[] index = new int[Octree.DIMENSION];
	public int[] scratch = new int[Octree.DIMENSION];
	public int elementCount;
	public MatrixEntry[] rowElements;
	
	public int Function(final OctNode node1, final OctNode node2){
		int[] d1 = new int[1];
		int[] d2 = new int[1];
		int[] off1 = new int[3];
		int[] off2 = new int[3];
		node1.depthAndOffset(d1,off1);
		node2.depthAndOffset(d2,off2);
		int dDepth=d2[0]-d1[0];
		int d;
		d=(off2[0]>>dDepth)-off1[0];
		if(d<0){return 0;}

		if( dDepth == 0){
			if( d == 0){
				d=off2[1]-off1[1];
				if(d<0){return 0;}
				else if( d == 0){
					d=off2[2]-off1[2];
					if(d<0){return 0;}
				}
			}
			// Since we are getting the restricted matrix, we don't want to propogate out to terms that don't contribute...
			if(!OctNode.Overlap2(depth,offset,0.5f,d1[0],off1,radius)){return 0;}
			scratch[0]=FunctionData.SymmetricIndex(index[0],BinaryNode.Index(d1[0],off1[0]));
			scratch[1]=FunctionData.SymmetricIndex(index[1],BinaryNode.Index(d1[0],off1[1]));
			scratch[2]=FunctionData.SymmetricIndex(index[2],BinaryNode.Index(d1[0],off1[2]));
			float temp=ot.GetLaplacian(scratch);
			if(node1==node2){temp/=2;}
			if((float)Math.abs(temp)> Octree.EPSILON){
				rowElements[elementCount].Value=temp;
				rowElements[elementCount].N=node1.nodeData.nodeIndex;
				elementCount++;
			}
			return 0;
		}
		return 1;
	}
	
}