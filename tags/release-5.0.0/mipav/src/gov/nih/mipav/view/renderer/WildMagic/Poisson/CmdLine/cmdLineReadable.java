package gov.nih.mipav.view.renderer.WildMagic.Poisson.CmdLine;

public class cmdLineReadable {
	public int set;
	
	public cmdLineReadable() {
		set = 0;
	}
	
	public int read(String name, int t){
		set=1;
		return 0;
	}
}