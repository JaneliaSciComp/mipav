package gov.nih.mipav.view.renderer.WildMagic.Poisson.CmdLine;

public class cmdLineInt extends cmdLineReadable {
	
	public int value;
	
	public cmdLineInt(){
		value=0;
	}
	
	public cmdLineInt(int v){
		value=v;
	}
	
	public int read(String argv,int argc){
		if(argc>0){
			value= Integer.valueOf(argv);
			set=1;
			return 1;
		}
		else{return 0;}
	}
	
}