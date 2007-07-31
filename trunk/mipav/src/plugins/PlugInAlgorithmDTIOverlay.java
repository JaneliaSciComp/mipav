import java.io.IOException;

import dtioverlay.utils.DTIStudioReader;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;


/**
 * @author pandyan
 * 
 * 
 * This is the algorithm for the DTI Overlay Plugin
 * 
 * References: Developed in concert with Bennett Landman from Johns Hopkins University
 *
 */
public class PlugInAlgorithmDTIOverlay extends AlgorithmBase {
	/** DTIStudioReader object for reading fiber file **/
	private DTIStudioReader reader;
	
	/** src image **/
	private ModelImage frecImage;
	
	/** fiber file path **/
	private String filepath;
	
	/** rgb fiber array **/
	private int[][][][] fiberArray;
	
	/** fiber array dimensions **/
	private int xdim, ydim, zdim;
	
	/** image b **/
	private ModelImage imageB;
	
	
	

	/** constructor **/
	public PlugInAlgorithmDTIOverlay(ModelImage frecImage, String filepath) {
		this.frecImage = frecImage;
		this.filepath = filepath;
		reader = new DTIStudioReader();
	}
	
	
	public void runAlgorithm() {
		try {
			fiberArray = reader.readColorVolumeMIPAV(filepath);
		}
		catch(IOException e) {
    		System.out.println(e.toString());
    	}
		
		//first lets make sure that the src image's extents and the fiber array's extents match up
		xdim = fiberArray.length;
		for(int i=0;i<fiberArray.length;i++) {
			int[][][] fiberArray2 = fiberArray[i];
			if(ydim == 0) {
				ydim = fiberArray2.length;
			}
			else {
				if(fiberArray2.length != ydim) {
					System.out.println("error....fiber file dimensions are not correct");
					return;
				}
			}
			for(int k=0;k<fiberArray2.length;k++) {
				int[][] fiberArray3 = fiberArray2[k];
				if(zdim == 0) {
					zdim = fiberArray3.length;
				}
				else {
					if(fiberArray3.length != zdim) {
						System.out.println("error....fiber file dimensions are not correct");
						return;
					}
				}
			}
		}
		if(xdim != frecImage.getExtents()[0] || ydim != frecImage.getExtents()[1] || zdim != frecImage.getExtents()[2]) {
			MipavUtil.displayError("Fiber file and src image to not have similar extents");
			return;
		}

		
		
		//ok now create a blank image b based on fiber array
		int bufferSize = 4 * xdim * ydim * zdim;
		int[] extents = new int[3];
		extents[0] = xdim;
		extents[1] = ydim;
		extents[2] = zdim;
        imageB = new ModelImage(ModelStorageBase.ARGB, extents, frecImage.getImageName() + "_imageB");
        
        //set up the data buffer
		short[] buffer = new short[bufferSize];
		int index=0;
		for(int z=0;z<zdim;z++) {
			for(int y=0;y<ydim;y++) {
				for(int x=0;x<xdim;x++) {
					buffer[index] = 1;
					buffer[index + 1] = (short)fiberArray[x][y][z][0];
					buffer[index + 2] = (short)fiberArray[x][y][z][1];
					buffer[index + 3] = (short)fiberArray[x][y][z][2];
					index = index + 4;
				}
			}
		}
		
		//import buffer into image b
		try {
            imageB.importData(0, buffer, false);
        } catch (IOException error) {
            errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", false);

            return;
        }

        FileInfoBase fileInfo = new FileInfoImageXML(frecImage.getImageName() + "_imageB", null, FileUtility.XML);
        fileInfo.setDataType(ModelStorageBase.ARGB);
        fileInfo.setExtents(extents);
        fileInfo.setUnitsOfMeasure(frecImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo.setResolutions(frecImage.getFileInfo()[0].getResolutions());
        fileInfo.setEndianess(frecImage.getFileInfo()[0].getEndianess());
        fileInfo.setOffset(frecImage.getFileInfo()[0].getOffset());
        for (int i = 0; i < fileInfo.getExtents()[2]; i++) {
            imageB.setFileInfo(fileInfo, i);
        }

        setCompleted(true);	

	}

	/** 
	 * get image b
	 * @return
	 */
	public ModelImage getImageB() {
		return imageB;
	}
	
	
	

}
