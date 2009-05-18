package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class AdjacencyCountFunction extends NodeAdjacencyFunction {

	public int adjacencyCount;
	
	public void Function(final OctNode node1, final OctNode node2){
		adjacencyCount++;
	}
	
}