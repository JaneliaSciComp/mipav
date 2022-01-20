package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class PointIndexValueFunction extends PointAdjacencyFunction {

	public int res2;
	public float[] valueTables;
	public int[] index = new int[Octree.DIMENSION];
	public float value;

	public void Function(final OctNode node) {
		int[] idx = new int[Octree.DIMENSION];
		idx[0] = index[0] + (int)(node.off[0]);
		idx[1] = index[1] + (int)(node.off[1]);
		idx[2] = index[2] + (int)(node.off[2]);
		value += node.nodeData.value
				* (float) (valueTables[idx[0]] * valueTables[idx[1]] * valueTables[idx[2]]);
	}

}