package gov.nih.mipav.view.renderer.WildMagic.AAM;

public class SIZE {
	
	public int x;
	public int y;
	
	public SIZE () { 
	
	}
	
	public SIZE(int initCX, int initCY) { 
		x = initCX; 
		y = initCY; 
	}
	
	public SIZE(SIZE initSize) { 
		x = initSize.x; 
		y = initSize.y; 
	}
	
	public SIZE(POINT initPt) { 
		x = initPt.x; 
		y = initPt.y; 
	}
	
	// 
	public SIZE assign(final SIZE refSize) { 
		x = refSize.x; 
		y = refSize.y; 
		return this;
	}
	
}