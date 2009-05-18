
package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class VertTmark {
	
	TriMesh m;
	
	public VertTmark() {
		
	}
	void UnMarkAll() {
		m.UnMarkAll();
	}
	
	public boolean IsMarked(Vertex obj){
		return (m.IsMarked(obj));
	}
	
	public void Mark(Vertex obj) {
		m.Mark(obj);
	}
	
	public void SetMesh(TriMesh _m) {
		m=_m;
	}
	
}