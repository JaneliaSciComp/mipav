
package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class VertTmark {
	
	TriMesh m;
	
	public VertTmark() {
		
	}
	
	public final void unMarkAll() {
		m.unMarkAll();
	}
	
	public final boolean isMarked(Vertex obj){
		return (m.isMarked(obj));
	}
	
	public final void mark(Vertex obj) {
		m.mark(obj);
	}
	
	public final void setMesh(TriMesh _m) {
		m=_m;
	}
	
}