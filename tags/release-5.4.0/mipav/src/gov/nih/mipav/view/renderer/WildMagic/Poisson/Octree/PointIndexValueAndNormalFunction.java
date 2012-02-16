package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry.*;

public class PointIndexValueAndNormalFunction extends PointAdjacencyFunction {

	public int res2;
	public float[] valueTables;
	public float[] dValueTables;
	public float[] value = new float[1];
	public Point3D normal = new Point3D();
	public int[] index = new int[Octree.DIMENSION];

	public void Function(final OctNode node) {
		int[] idx = new int[Octree.DIMENSION];
		idx[0] = index[0] + (int)(node.off[0]);
		idx[1] = index[1] + (int)(node.off[1]);
		idx[2] = index[2] + (int)(node.off[2]);
		value[0] += node.nodeData.value
				* (float) (valueTables[idx[0]] * valueTables[idx[1]] * valueTables[idx[2]]);
		normal.coords[0] += node.nodeData.value
				* (float) (dValueTables[idx[0]] * valueTables[idx[1]] * valueTables[idx[2]]);
		normal.coords[1] += node.nodeData.value
				* (float) (valueTables[idx[0]] * dValueTables[idx[1]] * valueTables[idx[2]]);
		normal.coords[2] += node.nodeData.value
				* (float) (valueTables[idx[0]] * valueTables[idx[1]] * dValueTables[idx[2]]);
	}

}