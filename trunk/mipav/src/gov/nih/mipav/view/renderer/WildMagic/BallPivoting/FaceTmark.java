package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class FaceTmark {
	
	TriMesh m = new TriMesh();
	
	public FaceTmark() {
		
	}
	void UnMarkAll() {
		m.UnMarkAll();
	}
	
	public boolean IsMarked(Face obj){
		return (m.IsMarked(obj));
	}
	
	public void Mark(Face obj) {
		m.Mark(obj);
	}
	
	public void SetMesh(TriMesh _m) {
		m=_m;
	}
	
}