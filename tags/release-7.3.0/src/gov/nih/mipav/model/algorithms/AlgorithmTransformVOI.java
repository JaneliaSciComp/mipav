package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIVector;

public class AlgorithmTransformVOI extends AlgorithmBase {

	public static final int VOICENTER = 0;
	
	public static final int ORIGIN = 1;
	
	public static final int IMCENTER = 2;
	
	private TransMatrix mat;
	
	private float t_x;
	
	private float t_y;
	
	private float t_z;
	
	private boolean allVOIs;
	
	private int transformCenter;
	
	public AlgorithmTransformVOI(ModelImage im, TransMatrix matrix){
		super(null, im);
		mat = matrix;
	}
	
	public void setCenter(int centerValue){
		transformCenter = centerValue;
	}
	
	public void setTranslation(float x, float y, float z){
		t_x = x;
		t_y = y;
		t_z = z;
	}
	
	public void setAllVOIs(boolean all){
		allVOIs = all;
	}
	
	@Override
	public void runAlgorithm() {
		
		//Start by grabbing active VOIs in this image
		VOIVector vois = srcImage.getVOIs();
		VOIBaseVector active = new VOIBaseVector();
		for(VOI v : vois){
			VOIBaseVector vec = v.getCurves();
			for(VOIBase b : vec){
				if(b.isActive() || allVOIs)
					active.add(b);
			}
		}
		
		/**
		 * RIGHT NOW ROTATIONS AREN'T ABOUT THE CENTER OF
		 * THE VOI, change to 3 steps:
		 * 
		 * translate to center
		 * perform transformation w/o translation
		 * translate to the new location
		 */
		
		if(srcImage.is2DImage()){
			for(VOIBase b : active){
				Vector3f center; 
				Vector3f newCenter; 
				Vector3f[] pts = new Vector3f[b.size()];
				Vector3f transVec = new Vector3f(t_x, t_y, t_z);
				switch(transformCenter){

				case ORIGIN: //Nothing fancy
					for(int i=0;i<b.size();i++){
						b.get(i).add(transVec);
						pts[i] = new Vector3f();
					}
					mat.transformAsVector3Df(b, pts);
					for(int i=0;i<b.size();i++){
						pts[i].Z = 0;
					}
					break;
				case IMCENTER: 
					int xCenter = srcImage.getWidth(0)/2;
					int yCenter = srcImage.getHeight(0)/2;
					
					Vector3f imCenter = new Vector3f(xCenter, yCenter, 0);
					
					for(int i=0;i<b.size();i++){
						b.get(i).add(transVec).sub(imCenter);
						pts[i] = new Vector3f();
					}
					mat.transformAsVector3Df(b, pts);
					for(int i=0;i<b.size();i++){
						pts[i].Z = 0;
						pts[i].add(imCenter);
					}
					break;
				default: 
					center = b.getGeometricCenter(); //default to VOI center
					newCenter = Vector3f.add(center, transVec);
					 
					for(int i=0;i<b.size();i++){
						b.get(i).add(Vector3f.neg(center));
						pts[i] = new Vector3f();
					}
					mat.transformAsVector3Df(b, pts);
					for(int i=0;i<b.size();i++){
						pts[i].Z = 0;
						pts[i].add(newCenter);
					}
				}

				
				b.clear();
				b.importPoints(pts);
				b.update();
				
			}
		} else {
			for(VOIBase b : active){
				for(Vector3f pt : b){
					Vector3f center = b.getGeometricCenter();
					Vector3f newCenter = center.add(t_x, t_y, t_z);
					Vector3f tPt = new Vector3f();
					pt.add(Vector3f.neg(center));
					mat.transformAsPoint3Df(pt, tPt);
					System.out.println(pt + " " + tPt);
					pt.copy(tPt);
					pt.add(newCenter);
				}
				b.update();
			}
		}
		
		
		

		srcImage.notifyImageDisplayListeners();

	}

}
