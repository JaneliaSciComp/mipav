import java.io.IOException;
import java.util.BitSet;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.structures.ModelImage;


public class PlugInAlgorithmNeuronalOverlap extends AlgorithmBase{
	
	private ModelImage dapiImage;
	
	private ModelImage dapiOriginal;
	
	private ModelImage neunImage;
	
	private int[] extents;
	
	private int width;
	
	private int height;
	
	private int length;
	
	private int overlap;
	
	private float greyThreshold = 4.0f;
	
	private float greenThreshold = 65.0f;

	public PlugInAlgorithmNeuronalOverlap(ModelImage dapiIm, ModelImage neunIm, int overlapPct){
		
		extents = dapiIm.getExtents();
		width = extents[0];
		height = extents[1];
		length = width*height;
		
		dapiOriginal = dapiIm;
		
		if(dapiIm.isColorImage()){
			dapiImage = new ModelImage(dapiIm.getType(), extents, "DAPI Gray");
			AlgorithmRGBtoGray convert = new AlgorithmRGBtoGray(dapiImage, dapiIm);
			convert.run();
		} else dapiImage = (ModelImage) dapiIm.clone();
		if(neunIm.isColorImage()){
			neunImage = new ModelImage(neunIm.getType(), extents, "NeuN Gray");
			int[] neunBuffer = new int[length];
			int[] colorBuffer = new int[4*length];
			try {
				neunIm.exportData(0, 4*length, colorBuffer);
				for(int i=0;i<length;i++){
					neunBuffer[i] = colorBuffer[4*i+2];
				}
				neunImage.importData(0, neunBuffer, true);
			} catch (IOException e) {
				e.printStackTrace();
			}
		} else neunImage = (ModelImage)neunIm.clone();
		
		overlap = overlapPct;
	}
	
	public void setThresholds(int dapi, int neun){
		greyThreshold = (float)dapi;
		greenThreshold = (float)neun;
	}
	
	@Override
	public void runAlgorithm() {

		float[] dThreshold = new float[]{greyThreshold, (float) dapiImage.getMax()};
		AlgorithmThresholdDual dThresh = new AlgorithmThresholdDual(dapiImage, dThreshold, 1, 1, true, false);
        dThresh.run();
        
        float[] nThreshold = new float[]{greenThreshold, (float) neunImage.getMax()};
        AlgorithmThresholdDual nThresh = new AlgorithmThresholdDual(neunImage, nThreshold, 1, 1, true, false);
        nThresh.run();
		
        AlgorithmMorphology2D nObj = new AlgorithmMorphology2D(dapiImage, AlgorithmMorphology2D.CONNECTED4,
        		1.0f, AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, true);
        nObj.setMinMax(2, length);
        nObj.run();
        
        int numObj = (int) dapiImage.getMax();
        
        int[] objArea = new int[numObj];
        int[] objOverlap = new int[numObj];
        
        int[] objBuffer = new int[length];
        
        BitSet neunSet = new BitSet(length);
        
        int colors = dapiOriginal.isColorImage() ? 4 : 1;
        int[] dapiBuffer = new int[colors*length];
        try {
        	dapiImage.exportData(0, length, objBuffer);
			neunImage.exportData(0, length, neunSet);
			dapiOriginal.exportData(0, colors*length, dapiBuffer);
		} catch (IOException e) {
			e.printStackTrace();
		}
        
        //dapiImage: objects
        //neunImage: occlusion objects
        //dapiOriginal: original image
        for(int i=0;i<length;i++){
        	if(objBuffer[i] == 0)
        		continue;
        	int value = objBuffer[i];
        	objArea[value-1]++;
        	if(neunSet.get(i))
        		objOverlap[value-1]++;
        }
        
        boolean[] occluded = new boolean[numObj];
        for(int i=0;i<numObj;i++){
        	int occlusion = (int) ((float)(objOverlap[i]*100)/(float)objArea[i]);
        	occluded[i] = occlusion > overlap;
        }
        
        /*for(int i=0;i<length;i++){
        	int value = objBuffer[i];
        	if(occluded[value-1])
        		objBuffer[i] = 0;
        	else if(objBuffer[i]>0)
        		objBuffer[i] = 1;
        }*/
        
        for(int i=0;i<length;i++){
        	int value = objBuffer[i];
        	if(value == 0 || occluded[value-1]){
        		objBuffer[i] = 0;
        		if(dapiOriginal.isColorImage()){
        			for(int k=1;k<4;k++){
        				dapiBuffer[4*i+k] = 0;
        			}
        		} else dapiBuffer[i] = 0;
        	}
        }
        
        destImage = new ModelImage(dapiOriginal.getType(), extents, dapiOriginal.getImageName() + "_removed");
        
        try {
			destImage.importData(0, dapiBuffer, true);
		} catch (IOException e) {
			e.printStackTrace();
		}     
        
        dapiImage.disposeLocal();
        neunImage.disposeLocal();
        
        setCompleted(true);
	}

}
