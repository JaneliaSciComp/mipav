package gov.nih.mipav.view.renderer.WildMagic.Poisson.CmdLine;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry.*;

public class cmdLinePoint3D extends cmdLineReadable {
	
	public Point3D value = new Point3D();
	
	public cmdLinePoint3D(){
		value.coords[0]=value.coords[1]=value.coords[2]=0;
	}
	
	public cmdLinePoint3D(Point3D v){
		value.coords[0]=v.coords[0];value.coords[1]=v.coords[1];value.coords[2]=v.coords[2];
	}
	
	public cmdLinePoint3D(float v0, float v1, float v2){
		value.coords[0]=v0;value.coords[1]=v1;value.coords[2]=v2;
	}
	
	public int read(String[] argv,int argc){
		if(argc>2){
			value.coords[0]= Float.valueOf(argv[0]);
			value.coords[1]= Float.valueOf(argv[1]);
			value.coords[2]= Float.valueOf(argv[2]);
			set=1;
			return 3;
		}
		else{return 0;}
	}
	
}