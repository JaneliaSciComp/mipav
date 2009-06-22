package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class FaceTmark {
	
	TriMesh m = new TriMesh();
	
	public FaceTmark() {
		
	}
	
	public final void unMarkAll() {
		m.unMarkAll();
	}
	
	public final boolean isMarked(Face obj){
		return (m.isMarked(obj));
	}
	
	public final void mark(Face obj) {
		m.mark(obj);
	}
	
	public final void setMesh(TriMesh _m) {
		m=_m;
	}
	
}