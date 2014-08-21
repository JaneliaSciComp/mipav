package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIVector;

public class AlgorithmTransformVOI extends AlgorithmBase {

	private TransMatrix mat;
	
	private float t_x;
	
	private float t_y;
	
	private float t_z;
	
	public AlgorithmTransformVOI(ModelImage im, TransMatrix matrix){
		super(null, im);
		mat = matrix;
	}
	
	public void setTranslation(float x, float y, float z){
		t_x = x;
		t_y = y;
		t_z = z;
	}
	
	@Override
	public void runAlgorithm() {
		
		//Start by grabbing active VOIs in this image
		VOIVector vois = srcImage.getVOIs();
		VOIBaseVector active = new VOIBaseVector();
		for(VOI v : vois){
			VOIBaseVector vec = v.getCurves();
			for(VOIBase b : vec){
				if(b.isActive())
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
				Vector3f center = b.getGeometricCenter();
				Vector3f newCenter = Vector3f.add(center, new Vector3f(t_x, t_y, t_z));//center.add(t_x, t_y, t_z);
				Vector3f[] pts = new Vector3f[b.size()]; 
				for(int i=0;i<b.size();i++){
					b.get(i).add(Vector3f.neg(center));
					pts[i] = new Vector3f();
				}
				mat.transformAsVector3Df(b, pts);
				for(int i=0;i<b.size();i++){
					pts[i].Z = 0;
					pts[i].add(newCenter);
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
