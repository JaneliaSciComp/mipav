import java.io.IOException;
import java.util.ArrayList;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewJComponentDTIImage;
import gov.nih.mipav.view.ViewJFrameImage;

/**
 * @author pandyan
 * 
 * This is the main dialog for the DTI Color Display
 * 
 * References: This algorithm was developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group and
 * Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * 
 * Mathematical and Statistical Computing Laboratory (MSCL)
 * Division of Cumputational Bioscience (DCB)
 * Center for Informational Technology (CIT)
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 *
 */
public class PlugInAlgorithmDTIColorDisplay extends AlgorithmBase {
	
	/** eigenvector src image **/
	private ModelImage eigvecSrcImage;
	
	/** anisotropy src Image     MAY NOT NEED THIS HERE**/
	private ModelImage anisotropyImage;
	
	/** result dec map image **/
    private ModelImage decImage;
    
    /** result image extents **/
    private int[] destExtents;
    
    /** extraced 3D Model Images from  image...red Image is [0], green image is [1], blue image is [2] **/
    private ModelImage[] channelImages;
    
    /** DOCUMENT ME **/
    private AlgorithmSubset subsetAlgo;
    
    /** result image **/
    private ModelImage resultImage;
    
    /** ref to AlgorithmRGBConcat**/
    private AlgorithmRGBConcat mathAlgo;
	
    /** boolean for remap **/
    private boolean remapMode = false;
    
    /** component image **/
    private ViewJComponentDTIImage componentImage;
	
    
    
    
	/** constructor **/
	public PlugInAlgorithmDTIColorDisplay(ModelImage eigvecSrcImage, ModelImage anisotropyImage) {
		this.eigvecSrcImage = eigvecSrcImage;
		this.anisotropyImage = anisotropyImage;
	}

	/** run algorithm **/
	public void runAlgorithm() {
	
		createModelImage();

		setCompleted(true);
	}
	
	
	
	
	
	/**
	 * create model image
	 *
	 */
	private void createModelImage() {
		//create the dest extents of the result image
		destExtents = new int[4];
        destExtents[0] = eigvecSrcImage.getExtents()[0];
        destExtents[1] = eigvecSrcImage.getExtents()[1];
        destExtents[2] = eigvecSrcImage.getExtents()[2];
        destExtents[3] = 3;
		
        //to do ...remove the EG part from name
        decImage = new ModelImage(ModelStorageBase.FLOAT, destExtents, eigvecSrcImage.getImageName() + "_DEC");
        
        //src buffer
        float[] srcBuffer;
        
        //result buffer
        float[] resultBuffer;
        
        int length = eigvecSrcImage.getExtents()[0] * eigvecSrcImage.getExtents()[1] * eigvecSrcImage.getExtents()[2] * 3;
        srcBuffer = new float[length];
        resultBuffer = new float[length];
        try {
        	eigvecSrcImage.exportData(0, length, srcBuffer);
        }
        catch (IOException error) {
        	System.out.println("IO exception");

            return;
        }
        
        
        for(int i=0;i<srcBuffer.length;i++) {

        	//int roundedInt = Math.round((float)srcBuffer[i] * 255);

        	resultBuffer[i] = srcBuffer[i];
        }
        
        
        try {
        	decImage.importData(0, resultBuffer, true);
        }
        catch (IOException error) {
        	System.out.println("IO exception");

            return;
        }
        
        //new ViewJFrameImage(resultImage);
        
        
        destExtents = new int[3];
        destExtents[0] = decImage.getExtents()[0];
        destExtents[1] = decImage.getExtents()[1];
        destExtents[2] = decImage.getExtents()[2];
        channelImages = new ModelImage[decImage.getExtents()[3]];
        for(int i=0;i<decImage.getExtents()[3];i++) {
			int num = i + 1;
			String resultString = decImage.getImageName() + "_Vol=" + num;
			channelImages[i] = new ModelImage(decImage.getType(), destExtents, resultString);
			subsetAlgo = new AlgorithmSubset(decImage, channelImages[i], AlgorithmSubset.REMOVE_T, i);
			subsetAlgo.setRunningInSeparateThread(false);
			subsetAlgo.run();
		}
  
        resultImage = new ModelImage(ModelImage.ARGB_FLOAT, channelImages[0].getExtents(),eigvecSrcImage.getImageName() + "_ColorDisplay");

        mathAlgo = new AlgorithmRGBConcat(channelImages[0], channelImages[1], channelImages[2], resultImage, remapMode, false);
        mathAlgo.setRunningInSeparateThread(false);
        mathAlgo.run();
        
        FileInfoBase[] fileInfoBases = new FileInfoBase[resultImage.getExtents()[2]];
        for (int i=0;i<fileInfoBases.length;i++) {
       	 	fileInfoBases[i] = new FileInfoImageXML(resultImage.getImageName(), null, FileUtility.XML);
        }
        FileInfoBase.copyCoreInfo(eigvecSrcImage.getFileInfo(), fileInfoBases);
        resultImage.setFileInfo(fileInfoBases);
        
        
        
        
        finalize();
        
	}

	
	
	public ArrayList getFractionalAnisotropyArrayList() {
		ArrayList faArrayList = new ArrayList();
		
		
		
		return faArrayList;
	}
	

	/**
	 * get Result Image
	 * @return resultImage
	 */
	public ModelImage getResultImage() {
		return resultImage;
	}
	
	
	

	
	/**
	 * get component image
	 * @return
	 */
	public ViewJComponentDTIImage getComponentImage() {
		return componentImage;
	}

	/**
	 * finalize
	 */
	public void finalize() {
		if(channelImages[0] != null) {
			channelImages[0].disposeLocal();
		}
		if(channelImages[1] != null) {
			channelImages[1].disposeLocal();
		}
		if(channelImages[2] != null) {
			channelImages[2].disposeLocal();
		}
		if(decImage != null) {
			decImage.disposeLocal();
		}
		
		channelImages[0] = null;
		channelImages[1] = null;
		channelImages[2] = null;
		decImage = null;
		
 	}
	
}
