package gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry;

public class TriangulationEdge {
	public TriangulationEdge() {
		pIndex[0]=pIndex[1]=tIndex[0]=tIndex[1]=-1;
	}

	public int[] pIndex = new int[2];
	public int[] tIndex = new int[2];
}
