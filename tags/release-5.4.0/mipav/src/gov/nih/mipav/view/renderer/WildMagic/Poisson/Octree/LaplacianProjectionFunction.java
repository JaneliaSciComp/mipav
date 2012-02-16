package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class LaplacianProjectionFunction extends NodeAdjacencyFunction {

	public double value;

	public Octree ot;
	public int[] index = new int[Octree.DIMENSION];
	public int[] scratch = new int[Octree.DIMENSION];

	public void Function(OctNode node1, final OctNode node2)  {
		scratch[0] = FunctionData.SymmetricIndex(index[0], (int)(node1.off[0]));
		scratch[1] = FunctionData.SymmetricIndex(index[1], (int)(node1.off[1]));
		scratch[2] = FunctionData.SymmetricIndex(index[2], (int)(node1.off[2]));
		node1.nodeData.value -= (float) (ot.GetLaplacian(scratch) * value);
		// System.err.println("LaplacianProjectionFunction: node1.nodeData.value = " + node1.nodeData.value);
		// System.err.println("ot.GetLaplacian(scratch) = " +  ot.GetLaplacian(scratch) + " value = " + value );
		// Octree.pause();
	}

}
