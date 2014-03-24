import java.io.File;
import java.io.IOException;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmExtractSlices;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;


public class PlugInAlgorithm4DMaxProject extends AlgorithmBase {

	//private ModelImage locationImage;
	
	private boolean save;
	
	public PlugInAlgorithm4DMaxProject(ModelImage sourceImage, ModelImage destImage, boolean saveImage){
		super(destImage, sourceImage);
		save = saveImage;
	}
	
	/*public ModelImage getLocationImage(){
		return locationImage;
	}*/
	@Override
	public void runAlgorithm() {
		// TODO Auto-generated method stub
		int[] extents = srcImage.getExtents();
		int[] extents3 = {extents[0], extents[1], extents[2]};
		int[] dstBuffer = new int[extents[0]*extents[1]*extents[3]];
		int[] projBuffer = new int[extents[0]*extents[1]];
		//int[] locBuffer = new int[extents[0]*extents[1]*extents[3]];
		//int[] volBuffer = new int[extents[0]*extents[1]*extents[2]];
		ModelImage projection;
		ModelImage volume = new ModelImage(srcImage.getDataType(), extents3, "Extracted Volume");
		ModelImage dest = new ModelImage(srcImage.getDataType(), new int[] {extents[0], extents[1]}, "Z_Projection");
		//locationImage = new ModelImage(ModelImage.UBYTE, destImage.getExtents(), "Z location");
		AlgorithmSubset extract;
		AlgorithmMaximumIntensityProjection project;
		AlgorithmExtractSlices slice;
		/*FileInfoBase info = srcImage.getFileInfo(0);
		float xRes = info.getResolution(0);
		float yRes = info.getResolution(1);
		int xUnit = info.getUnitsOfMeasure(0);
		int yUnit = info.getUnitsOfMeasure(1);*/
		String[] slices = new String[] {"0"};
		progress = 0;

		for(int i=0;i<extents[3];i++){
			
			progress = 100f * (float)i/(float)extents[3];
			fireProgressStateChanged((int)progress, null, "Converting to 3D: Max Projecting..." + (i+1) + " of " + extents[3]);
			System.out.println(String.valueOf(i));
			extract = new AlgorithmSubset(srcImage, volume, AlgorithmSubset.REMOVE_T, i);
			extract.run();
			/*try {
				volume.exportData(0, extents[0]*extents[1]*extents[2], volBuffer);
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}*/
			project = new AlgorithmMaximumIntensityProjection(volume, 0, extents[2]-1, extents[2]-1,
					volume.getMin(), volume.getMax(), true, false, AlgorithmMaximumIntensityProjection.Z_PROJECTION);
			project.run();
			projection = project.getResultImage().get(0);
			
			slice = new AlgorithmExtractSlices(projection, dest, slices);
			slice.run();
			projection.disposeLocal();
			
			if(save){
				/*FileInfoBase sInfo = dest.getFileInfo(0);
				sInfo.setResolutions(xRes, 0);
				sInfo.setResolutions(yRes, 1);
				sInfo.setUnitsOfMeasure(xUnit, 0);
				sInfo.setUnitsOfMeasure(yUnit, 1);*/
				String directory = srcImage.getImageDirectory() + File.separator 
						+ srcImage.getImageName() + "_Max_Project" + File.separator;
				File dirFile = new File(directory);
				if(!dirFile.exists())
					dirFile.mkdir();
				String fileName = srcImage.getImageName() + "_MIP_T=" + String.format("%03d", i);
				dest.saveImage(directory, fileName, FileUtility.TIFF, false, false);
			}
			try {
				dest.exportData(0, extents[0]*extents[1], projBuffer);
				for(int j=0;j<extents[0]*extents[1];j++){
					dstBuffer[j+i*extents[0]*extents[1]] = projBuffer[j];
					/*for(int k=0;k<extents[2];k++){
						if(projBuffer[j] == volBuffer[j+k*extents[0]*extents[1]]){
							locBuffer[j+i*extents[0]*extents[1]] = k;
						}
					}*/
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		try {
			destImage.importData(0, dstBuffer, true);
			//locationImage.importData(0, locBuffer, true);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		dest.disposeLocal();
		volume.disposeLocal();
		
		setCompleted(true);
	}

}
