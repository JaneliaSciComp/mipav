import gov.nih.mipav.model.algorithms.AlgorithmBase;

import java.util.ArrayList;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public class PlugInAlgorithm3DSpline extends AlgorithmBase {

	private ArrayList<Vector3f> splinePts;
	
	private ArrayList<Vector3f> gradients;
	
	private ArrayList<Vector3f> xBases;
	
	private ArrayList<Vector3f> yBases;
	
	private static ArrayList<Vector3f> searchFieldBase;
	
	private float xres = 0.211639F;
	
	//private float zres = 0.8e-3F;
	
	public PlugInAlgorithm3DSpline(ArrayList<Vector3f> nodes){
		super();
		
		splinePts = nodes;
		gradients = new ArrayList<Vector3f>();
		xBases = new ArrayList<Vector3f>();
		yBases = new ArrayList<Vector3f>();
		
		searchFieldBase = new ArrayList<Vector3f>();
		
		for(float y = -15F*xres;y<=15F*xres;y+=xres/2.0){
			for(float x = -15F*xres;x<=15F*xres;x+=xres/2.0){
				Vector3f pt = new Vector3f(x, y, 0);
				searchFieldBase.add(pt);
			}
		}
		
	}
	
	@Override
	public void runAlgorithm() {
		
		int n = splinePts.size() - 1;
		float[] gamma = new float[n+1];
		Vector3f[] intermediate = new Vector3f[n+1];
		Vector3f[] output = new Vector3f[n+1];
		
		gamma[0] = 0.5f;
		
		for(int i=1;i<n;i++){
			gamma[i] = 1.0f/(4.0f - gamma[i-1]);
		}
		
		gamma[n] = 1.0f/(2.0f - gamma[n-1]);
		Vector3f pt = Vector3f.sub(splinePts.get(1), splinePts.get(0));
		pt.scale(3.0f * gamma[0]);
		intermediate[0] = pt;
		
		for(int i=1;i<n;i++){
			pt = Vector3f.sub(splinePts.get(i+1), splinePts.get(i-1));
			pt.scale(3.0f);
			pt.sub(intermediate[i-1]);
			pt.scale(gamma[i]);
			intermediate[i] = pt;
			
		}
		
		pt = Vector3f.sub(splinePts.get(n), splinePts.get(n-1));
		pt.scale(3.0f);
		pt.sub(intermediate[n-1]);
		pt.scale(gamma[n]);
		intermediate[n] = pt;
		
		output[n] = intermediate[n];
		
		for(int i=n-1;i>=0;i--){
			Vector3f subtract = new Vector3f(gamma[i], gamma[i], gamma[i]);
			subtract.mult(output[i+1]);
			output[i] = Vector3f.sub(intermediate[i], subtract);
		}
		
		for(int i=0;i<n+1;i++){
			Vector3f v = output[i];
			v.normalize();
			
			gradients.add(v);
		}
		
		//Find x and y bases, as gradients holds z basis
		for(int i=0;i<n+1;i++){
			Vector3f aBase = new Vector3f(1,0,0);
			Vector3f zBase = output[i];
			if(zBase.angle(aBase) < Math.PI/90){
				aBase = new Vector3f(0,1,0);
			}
			
			Vector3f xBase = Vector3f.cross(zBase, aBase);
			xBase.normalize();
			Vector3f yBase = Vector3f.cross(zBase, xBase);
			yBase.normalize();
			xBases.add(xBase);
			yBases.add(yBase);
		}
		
		setCompleted(true);
		
	}
	
	public ArrayList<Vector3f> getZBases(){
		return gradients;
	}
	
	public ArrayList<Vector3f> getXBases(){
		return xBases;
	}
	
	public ArrayList<Vector3f> getYBases(){
		return yBases;
	}
	
	public ArrayList<Vector3f> rotatePlane(Vector3f xBase, Vector3f yBase, Vector3f zBase){
		ArrayList<Vector3f> rotated = new ArrayList<Vector3f>();
		for(Vector3f v : searchFieldBase){
			float x = xBase.X * v.X + yBase.X * v.Y + zBase.X * v.Z;
			float y = xBase.Y * v.X + yBase.Y * v.Y + zBase.Y * v.Z;
			float z = xBase.Z * v.X + yBase.Z * v.Y + zBase.Z * v.Z;
			//float x = xBase.dot(v);
			//float y = yBase.dot(v);
			//float z = zBase.dot(v);
			Vector3f pt = new Vector3f(x,y,z);
			rotated.add(pt);
		}
		
		return rotated;
	}

}
