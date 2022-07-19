package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class RefineFunction extends NodeAdjacencyFunction {
	public int depth;
	public void Function(OctNode node1, final OctNode node2){
		if( node1.children == null && node1.depth()<depth){
			node1.initChildren();
		}
	}
	
}