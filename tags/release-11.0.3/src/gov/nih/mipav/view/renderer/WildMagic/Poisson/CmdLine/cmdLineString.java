package gov.nih.mipav.view.renderer.WildMagic.Poisson.CmdLine;

public class cmdLineString extends cmdLineReadable {
	
	public String value;
	
	public cmdLineString(){
		value=null;
	}
	
	public void dispose(){
		if(value != null ){
			value=null;
		}
	}
	
	public int read(String argv,int argc){
		if(argc>0){
			// value=new char[strlen(argv[0])+1];
			
			// strcpy(value,argv[0]);
			value = new String();
			value = argv;
			set=1;
			return 1;
		}
		else{return 0;}
	}
	
}