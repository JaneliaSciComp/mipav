package gov.nih.mipav.view.renderer.WildMagic.AAM;



public class POINT {
	
	public int x;
	public int y;
	
	public POINT() { 
		
	}
	
	public POINT(int initX, int initY) { 
		x = initX; 
		y = initY; 
	}
	
	public POINT(POINT initPt) { 
		x = initPt.x; 
		y = initPt.y; 
	}
	
	public POINT(SIZE initSize) { 
		x = initSize.x; 
		y = initSize.y; 
	}
	
	// operator=
	public POINT assign(final POINT refPoint) { 
		x = refPoint.x; 
		y = refPoint.y; 
		return this;
	}
	
	
}