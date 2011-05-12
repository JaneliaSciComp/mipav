package gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry;

public class CoredPointIndex {

	public int index;
	public int inCore;

	public CoredPointIndex() {
		
	}
	
	public CoredPointIndex(int x, int y) {
		index = x;
		inCore = y;
	}
	
	// operator ==
	public final boolean equals(CoredPointIndex cpi) {
		return (index == cpi.index) && (inCore == cpi.inCore);
	}

	// operator !=
	public final boolean notequals(CoredPointIndex cpi) {
		return (index != cpi.index) || (inCore != cpi.inCore);
	}
}