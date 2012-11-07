package gov.nih.mipav.model.algorithms;

import java.util.BitSet;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;


/**
 * Algorithm class for performing logical operations on VOIs
 * @author pandyan
 *
 */
public class AlgorithmVOILogicalOperations extends AlgorithmBase {
	
	/** Vector of all VOIs that will have calculations performed. */
    protected ViewVOIVector selectedVOIset;
    
    /** Model Images **/
    protected ModelImage clonedImage, finalMaskImage, tempMaskImage;
    
    /** operation type **/
    protected int operation;
    
    /** operation constants **/
    public static final int ADD = 0;

    /** operation constants **/
    public static final int OR = 1;
    
    /** operation constants **/
    public static final int XOR = 2;
    
    /** flag indicating whether output should be VOI image or mask image **/
    protected boolean doVoiImage;
    
    private AlgorithmMorphology2D alg2D;
    
    private AlgorithmMorphology3D alg3D;
    
    private int min = 0;
    
    private int max = 1000000000;
    
    private int kernel = 0;
    

     
    /**
     * constructor
     * @param img
     * @param clonedImage
     * @param selectedVOIset
     * @param operation
     * @param doVoiImage
     */
	public AlgorithmVOILogicalOperations(ModelImage clonedImage , ViewVOIVector selectedVOIset, int operation, boolean doVoiImage) {
		//this.img = img;
		this.clonedImage = clonedImage;
		this.selectedVOIset = selectedVOIset;
		this.operation = operation;
		this.doVoiImage = doVoiImage;
	}
	
	
	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		if (selectedVOIset.size() == 1) {
			//This means only 1 VOI is selected...so perform logical operations on its contours
			
			try {
				VOI voi = selectedVOIset.elementAt(0);
				
				for (int i=0; i<voi.getCurves().size(); i++) {
					VOIContour contour = (VOIContour) (voi.getCurves().elementAt(i));
					contour.setActive(false);
				}
				
				VOIContour contour = (VOIContour) (voi.getCurves().elementAt(0));

				finalMaskImage = new ModelImage(ModelStorageBase.BOOLEAN, clonedImage.getExtents(), "Final Mask Image");
				finalMaskImage.getMatrixHolder().replaceMatrices(clonedImage.getMatrixHolder().getMatrices());
				finalMaskImage.getFileInfo(0).setOrigin(clonedImage.getFileInfo(0).getOrigin());
				
				int extents[] = finalMaskImage.getExtents();
				int length = extents[0];
				for (int i=1;i<extents.length;i++) {
					length = length * extents[i];
				}
				BitSet finalBitSet = new BitSet(length);
				finalBitSet.clear();
				finalMaskImage.exportData(0, length, finalBitSet);

				JDialogBase.updateFileInfoOtherModality(clonedImage, finalMaskImage);

				contour.setMask( finalBitSet, extents[0], extents[1], false, 1 );     
				
				
				finalMaskImage.importData(0, finalBitSet, true);

				for (int i=1; i<voi.getCurves().size(); i++) {
					contour = (VOIContour) (voi.getCurves().elementAt(i));

					BitSet tempBitSet = new BitSet(length);
					
					contour.setMask( tempBitSet, extents[0], extents[1], false, 1 );    

					int size = tempBitSet.size();
	
					
					for(int k=0;k<size;k++) {
						boolean bool1 = finalBitSet.get(k);
						boolean bool2 = tempBitSet.get(k);
						if(operation == ADD) {
							if(bool1 == true && bool2 == true) {
								finalBitSet.set(k);
							}else {
								finalBitSet.clear(k);
							}
						}else if(operation == OR) {
							if(bool1 == true || bool2 == true) {
								finalBitSet.set(k);
							}else {
								finalBitSet.clear(k);
							}
						}else {
							if(bool1 == true && bool2 == true) {
								finalBitSet.clear(k);
							}else if (bool1 == false && bool2 == false){
								finalBitSet.clear(k);
							}else {
								finalBitSet.set(k);
							}
						}
					}
				}
				finalMaskImage.clearMask();
				finalMaskImage.importData(0, finalBitSet, true);
				
				if(doVoiImage) {
					
					if(operation == XOR) {
						
						if(finalMaskImage.getNDims() == 2) {
							 alg2D = new AlgorithmMorphology2D(finalMaskImage, kernel, 0, AlgorithmMorphology2D.ID_OBJECTS, 0,
                                     0, 0, 0, true);
							 alg2D.setMinMax(min, max);
							
							 alg2D.run();
							
						}else if (finalMaskImage.getNDims() == 3) {
							 alg3D = new AlgorithmMorphology3D(finalMaskImage, kernel, 0, AlgorithmMorphology3D.ID_OBJECTS, 0,
                                     0, 0, 0, true);
							 alg3D.setMinMax(min, max);
							 
							 alg3D.run();
							
						}
						
						finalMaskImage.notifyImageDisplayListeners(null, true);
						
					}

					AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(finalMaskImage);

		            VOIExtractionAlgo.run();
		            
		            finalMaskImage.groupVOIs();
		            
		            finalMaskImage.notifyImageDisplayListeners(null, true);
		            
		            VOIVector kVOIs = finalMaskImage.getVOIs();
		            
		            for ( int i = 0; i < kVOIs.size(); i++ ) {
		                VOI kCurrentGroup = kVOIs.get(i);
		                VOI clonedGroup = (VOI)kCurrentGroup.clone();
		                clonedImage.registerVOI(clonedGroup);
		            }
		            
		            finalMaskImage.disposeLocal();
				}else {
					
					clonedImage.disposeLocal();
				}

				setCompleted(true);
			}catch(Exception e) {
				e.printStackTrace();
			}

	
		}else {
			//This means multiple VOIs are selected
			try {
				
				for (int i=0; i<selectedVOIset.size(); i++) {
					VOI voi = selectedVOIset.elementAt(i);
					voi.setAllActive(false);
				}
				
				
				VOI voi = selectedVOIset.elementAt(0);
				voi.setAllActive(true);
				finalMaskImage = new ModelImage(ModelStorageBase.BOOLEAN, clonedImage.getExtents(), "Final Mask Image");
				JDialogBase.updateFileInfoOtherModality(clonedImage, finalMaskImage);
				voi.createBinaryImage(finalMaskImage, false, true);
				finalMaskImage.getMatrixHolder().replaceMatrices(clonedImage.getMatrixHolder().getMatrices());
				finalMaskImage.getFileInfo(0).setOrigin(clonedImage.getFileInfo(0).getOrigin());
				
				int extents[] = finalMaskImage.getExtents();
				int length = extents[0];
				for (int i=1;i<extents.length;i++) {
					length = length * extents[i];
				}
				BitSet finalBitSet = new BitSet(length);
				finalMaskImage.exportData(0, length, finalBitSet);
				voi.setAllActive(false);
				
				
				
				for (int i=1; i<selectedVOIset.size(); i++) {
					tempMaskImage = new ModelImage(ModelStorageBase.BOOLEAN, clonedImage.getExtents(), "Temp Image");
					JDialogBase.updateFileInfoOtherModality(clonedImage, tempMaskImage);
					tempMaskImage.getMatrixHolder().replaceMatrices(clonedImage.getMatrixHolder().getMatrices());
					tempMaskImage.getFileInfo(0).setOrigin(clonedImage.getFileInfo(0).getOrigin());
					
					voi = selectedVOIset.elementAt(i);
					voi.setAllActive(true);
					voi.createBinaryImage(tempMaskImage, false, true);
	
					BitSet tempBitSet = new BitSet(length);
					tempMaskImage.exportData(0, length, tempBitSet);

					int size = tempBitSet.size();
					
					for(int k=0;k<size;k++) {
						boolean bool1 = finalBitSet.get(k);
						boolean bool2 = tempBitSet.get(k);
						if(operation == ADD) {
							if(bool1 == true && bool2 == true) {
								finalBitSet.set(k);
							}else {
								finalBitSet.clear(k);
							}
						}else if(operation == OR) {
							if(bool1 == true || bool2 == true) {
								finalBitSet.set(k);
							}else {
								finalBitSet.clear(k);
							}
						}else {
							if(bool1 == true && bool2 == true) {
								finalBitSet.clear(k);
							}else if (bool1 == false && bool2 == false){
								finalBitSet.clear(k);
							}else {
								finalBitSet.set(k);
							}
						}
					}
					voi.setAllActive(false);
					tempMaskImage.disposeLocal();
				}
				
				finalMaskImage.clearMask();
				finalMaskImage.importData(0, finalBitSet, true);
				
				
				if(doVoiImage) {
					
					if(operation == XOR) {
						if(finalMaskImage.getNDims() == 2) {
							 alg2D = new AlgorithmMorphology2D(finalMaskImage, kernel, 0, AlgorithmMorphology2D.ID_OBJECTS, 0,
                                     0, 0, 0, true);
							 alg2D.setMinMax(min, max);
							
							 alg2D.run();
							
						}else if (finalMaskImage.getNDims() == 3) {
							 alg3D = new AlgorithmMorphology3D(finalMaskImage, kernel, 0, AlgorithmMorphology3D.ID_OBJECTS, 0,
                                     0, 0, 0, true);
							 alg3D.setMinMax(min, max);
							 
							 alg3D.run();
							
						}
						
						finalMaskImage.notifyImageDisplayListeners(null, true);
						
					}

					AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(finalMaskImage);

		            VOIExtractionAlgo.run();
		           
		            
		            VOIVector kVOIs = finalMaskImage.getVOIs();
		            
		            for ( int i = 0; i < kVOIs.size(); i++ ) {
		                VOI kCurrentGroup = kVOIs.get(i);
		                VOI clonedGroup = (VOI)kCurrentGroup.clone();
		                clonedImage.registerVOI(clonedGroup);
		            }
		            
		            finalMaskImage.disposeLocal();

				}else {
					
					clonedImage.disposeLocal();
				}
				
				
	
				setCompleted(true);
			}catch (Exception e) {
				e.printStackTrace();
			}
				
			
		}


	}


	/**
	 * returns the mask image
	 * @return
	 */
	public ModelImage getFinalMaskImage() {
		return finalMaskImage;
	}
	

	
	
	
	
	
	
	
	
	
	
	

}
