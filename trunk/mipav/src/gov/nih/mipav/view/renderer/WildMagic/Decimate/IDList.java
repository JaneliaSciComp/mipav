package gov.nih.mipav.view.renderer.WildMagic.Decimate;


public class IDList implements java.io.Serializable {
	int ID;
	IDList next;
	IDList back;

	public IDList() {
	}

	public IDList(int dID) {
		ID = dID;
	}
}
