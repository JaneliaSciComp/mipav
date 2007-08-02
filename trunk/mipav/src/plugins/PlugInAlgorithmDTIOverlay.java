import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import dtioverlay.utils.DTIStudioReader;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
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
	
	/** LUT for imageB **/
	private ModelLUT lutb;
	
	/** Mask Colors added to LUT **/
	private ArrayList addedColors = new ArrayList();
	
	
	

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
		int bufferSize = xdim * ydim * zdim;
		int[] extents = new int[3];
		extents[0] = xdim;
		extents[1] = ydim;
		extents[2] = zdim;
        imageB = new ModelImage(ModelStorageBase.SHORT, extents, frecImage.getImageName() + "_imageB");
        
        //set up lutB
        int[] dimExtentsLUT = new int[2];
        dimExtentsLUT[0] = 4;
        dimExtentsLUT[1] = 256;
        lutb = new ModelLUT(ModelLUT.STRIPED, 256, dimExtentsLUT);

        
        //set up the data buffer
		short[] buffer = new short[bufferSize];
		int index=0;
		int k;
		boolean match = false;
		for(int z=0;z<zdim;z++) {
			for(int y=0;y<ydim;y++) {
				for(int x=0;x<xdim;x++) {
					short r = (short)fiberArray[x][y][z][0];
					short g = (short)fiberArray[x][y][z][1];
					short b = (short)fiberArray[x][y][z][2];
					if(r != 0 || g != 0 || b != 0) {
						Color maskColor = new Color(r,g,b);
						loop: for (k = 1; k < 256; k++) {
								Color currColor = lutb.getColor(k);
								if(currColor.getRed() == maskColor.getRed() && currColor.getGreen() == maskColor.getGreen() && currColor.getBlue() == maskColor.getBlue()) {
									buffer[index] = (short)k;
									break loop;
								}
								else {
									for(int i=0;i<addedColors.size();i++) {
										if(((Color)addedColors.get(i)).equals(currColor)) {
											continue loop;
										}
									}
									lutb.setColor(k, maskColor);
									addedColors.add(maskColor);
									buffer[index] = (short)k;
									break loop;
								}
	                    	}	
					}
					else {
						buffer[index] = 0;
					}
					
					++ index;
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

        
        FileInfoBase[] fileInfoBases = new FileInfoBase[imageB.getExtents()[2]];
        for (int i=0;i<fileInfoBases.length;i++) {

        	 fileInfoBases[i] = new FileInfoImageXML(frecImage.getImageName() + "_imageB", null, FileUtility.XML);
        }
        
        
        FileInfoBase.copyCoreInfo(frecImage.getFileInfo(), fileInfoBases);
        for (int i=0;i<fileInfoBases.length;i++) {
       	 	fileInfoBases[i].setDataType(ModelStorageBase.SHORT); 
       	 	fileInfoBases[i].setModality(FileInfoBase.UNKNOWN_MODALITY);
        }

        imageB.setFileInfo(fileInfoBases);
        setCompleted(true);	

	}

	/** 
	 * get image b
	 * @return
	 */
	public ModelImage getImageB() {
		return imageB;
	}


	/**
	 * get lutb
	 * @return
	 */
	public ModelLUT getLutb() {
		return lutb;
	}
	
	
	

}
