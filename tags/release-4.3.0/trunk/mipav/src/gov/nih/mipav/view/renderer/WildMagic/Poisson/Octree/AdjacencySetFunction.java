package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class AdjacencySetFunction  extends NodeAdjacencyFunction  {

	public int[] adjacencies;

	public int adjacencyCount;

	public void Function(final OctNode node1, final OctNode node2) {
		adjacencies[adjacencyCount++] = node1.nodeData.nodeIndex;
	}

}