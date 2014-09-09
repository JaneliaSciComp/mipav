package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity;

public class poly {
	
	public static final int MAX_PTS = 100;
	
	public int n;
	public POINT pt[] = new POINT[MAX_PTS];
	
	public poly() {
	
	}
    
	public poly(int _n) {
		n = _n;
		pt = new POINT[_n];
	}
}