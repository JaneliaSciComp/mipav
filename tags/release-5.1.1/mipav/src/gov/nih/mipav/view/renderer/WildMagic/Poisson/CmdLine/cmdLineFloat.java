package gov.nih.mipav.view.renderer.WildMagic.Poisson.CmdLine;

public class cmdLineFloat extends cmdLineReadable {
	
	public float value;
	
	public cmdLineFloat(){
		value=0;
	}
	
	public cmdLineFloat(float v){
		value=v;
	}
	
	public int read(String argv,int argc){
		if(argc>0){
			value= Float.valueOf(argv);
			set=1;
			return 1;
		}
		else{return 0;}
	}
	
}