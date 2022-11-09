import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmCoherenceEnhancingDiffusion;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.*;

import ar.com.hjg.pngj.ImageInfo;
import ar.com.hjg.pngj.ImageLineByte;
import ar.com.hjg.pngj.ImageLineHelper;
import ar.com.hjg.pngj.PngWriter;

/*
 * This file reads the SKI10 dataset, converts the 3D images and corresponding masks into 2D
 * png slices -- 10 folds cross-validation.  
 * 
 * No image preprocessing steps.
 * 
 * SKI10 dataset is given actually in sagittal view, however, the image orientation attribute 
 * of the SKI 10 dataset is axial.  We have to bare with it.    
 * 
 * @author Ruida Cheng
 */
public class PlugInAlgorithmKneesFemurSegmentationSKI10_nopre extends AlgorithmBase {

    
    /** image repository input directory. */
    private String inputDir;

    /** saved images and report output directory. */
    private String outputDir;
    
    
    /** Reference to the brain subcortical plugin dialog. */
    private PlugInDialogKneesFemurSegmentationSKI10_nopre parentDialog;
   
    Hashtable<String, String> nameTableImages = new Hashtable<String, String>();
	Hashtable<String, String> nameTableImagesCED = new Hashtable<String, String>();
	Hashtable<String, String> nameTableImagesMask = new Hashtable<String, String>();

	// During the preprocessing step, it occupies quite large amount of memory for pre-processing, 
	// i.e. CED filter.  In java environment, it is slow.  
	// One solution is to save the intermediate image into MIPAV .xml readable file format, such as
	// axial, sagittal, coronal MRI images, CED images and Ground Truth images. Then reload those images
	// and convert them into pnd 2D slices for traning.   
	private Hashtable<String, ModelImage> keyImagesOrientation = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> keyImagesOrientationMask = new Hashtable<String, ModelImage>();

	
	// large data processing occupy fair large amount of memory. 
	// 1) Specify which orientation to run. 
	// 2) set the corresponding index
	private int axial_index = 0;
	private int sagittal_index = 0;
	private int coronal_index = 0;

	
	// image table to hold the isotropic sampled images
	Hashtable<String, ModelImage> keyImagesTransform = new Hashtable<String, ModelImage>();
	// image mask table to hold the isotropic sampled image masks. 
	Hashtable<String, ModelImage> keyImageMasksTransform = new Hashtable<String, ModelImage>();
	
	
	// mask threshold table to hold the mask label from the ground truth.  
	// For the experiment, I only extract the femur part to compare with literature results. 
	Hashtable<String, ModelImage> maskThresAxial = new Hashtable<String, ModelImage>();
	
	
	// axial, sagittal, coronal lists to hold the converted images from SKI10 axial view. 
	private Hashtable<String, ModelImage> axialList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> axialMaskList = new Hashtable<String, ModelImage>();
	
	private String saveImageDirectory_train;
	private String saveImageDirectory_test;
	
	
    /**
     * Algorithm for brain subcortical registration
     * @param _inputDir    image repository input directory
     * @param _outputDir    saved images ( registered, comparison, and report) directory. 
     * @param _parentDialog    reference to parent dialog, for file info saving. 
     */
    public PlugInAlgorithmKneesFemurSegmentationSKI10_nopre(String _inputDir, String _outputDir, PlugInDialogKneesFemurSegmentationSKI10_nopre _parentDialog) {
    	inputDir = _inputDir;
    	outputDir = _outputDir;
    	parentDialog = _parentDialog;
    }
    
    /**
     * Processing comamnd line, construction image instances, then do registration and comparison. 
     */
	public void runAlgorithm() {
		
		File fileDir = new File(inputDir);
		traverse_folder(fileDir);
		
		long startTime = System.currentTimeMillis();

		readFile();
		System.err.println("finish image I/O");

		threshold();
		
		System.err.println("saveImage");
		
		saveMRI2Dslice();
		
		saveHED2Dslice_test();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.err.println("sagittal_index = " + sagittal_index);
		
	
		System.gc();
		System.out.println("PlugIn Knees femur segmentation 10-fold finish running.  MIPAV Quits. ");
		System.exit(0);
	}
    
	
	public void saveHED2Dslice_test() {

		Set<String> keys = keyImagesOrientation.keySet();

		int count = 0;
		

		System.err.println("saveOrthogonalCEDImage testing");

		String sliceDir = outputDir;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		saveImageDirectory_test = outputDir + File.separator + "test" + File.separator;
		sliceDir = saveImageDirectory_test;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
	
		for (String key : keys) {

			System.err.println("save HED slice key = " + key);
			axial_index = 0;
			coronal_index = 0;
			sagittal_index = 0;
			
				ModelImage axialImage = keyImagesOrientation.get(key);
				
				if (count >= 0 && count <= 10) {	
					saveImageSlice_test(axialImage, "sagittal", "fold1", key);
				} else if (count >= 11 && count <= 20 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold2", key);	
				} else if (count >= 21 && count <= 30 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold3", key);	
				} else if (count >= 31 && count <= 40 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold4", key);	
				} else if (count >= 41 && count <= 50 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold5", key);	
				} else if (count >= 51 && count <= 60 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold6", key);	
				} else if (count >= 61 && count <= 70 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold7", key);	
				} else if (count >= 71 && count <= 80 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold8", key);	
				} else if (count >= 81 && count <= 90 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold9", key);	
				} else if (count >= 91 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold10", key);	
				}
				
				axialImage = null;
			
			count++;
			
		}

	}
	
	public void saveImageSlice_test(ModelImage image, String orientation, String folder, String key) {

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];

		float[] res = image.getResolutions(0);
		float x_res = res[0];
		float y_res = res[1];
		
		float[] newRes = new float[2];
		newRes[0] = x_res;
		newRes[1] = y_res;

		int size_3D = xDim * yDim * zDim;

		int[] newExtents = new int[2];
		newExtents[0] = xDim;
		newExtents[1] = yDim;

		int size = xDim * yDim;

		float[] imageBuffer = new float[size_3D];

		try {
			image.exportData(0, imageBuffer.length, imageBuffer);
		} catch (Exception e) {
			e.printStackTrace();
		}

		float percentile_left = 0.01f;
		float percentile_right = 0.01f;

		int minIndex = (int) (size_3D * percentile_left);
		int maxIndex = (int) (size_3D * (1.0 - percentile_right));

		Arrays.sort(imageBuffer);

		float min = imageBuffer[minIndex];
		float max = imageBuffer[maxIndex];

		imageBuffer = null;
		
		String sliceDir = saveImageDirectory_test + File.separator + folder + File.separator;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		sliceDir = sliceDir +  orientation + File.separator;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();

		sliceDir += key + File.separator;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();

		for (int j = 0; j < zDim; j++) {

			try {

				ModelImage imageSlice = new ModelImage(ModelStorageBase.FLOAT,
						newExtents, "image" + j);
				imageSlice.getFileInfo(0).setResolutions(newRes);
				imageSlice.setResolutions(newRes);
				float[] imgBuffer = new float[size];
				image.exportData(j * size, size, imgBuffer);
				imageSlice.importData(0, imgBuffer, true);
			
				
				String imgName = null;

				if (orientation.equals("axial")) {
					imgName = "image_" + axial_index + ".png";
				} else if (orientation.equals("coronal")) {
					imgName = "image_" + coronal_index + ".png";
				} else if (orientation.equals("sagittal")) {
					imgName = "image_" + sagittal_index + ".png";
				}

				if (imgName != null) {

					savePNGfile_test(sliceDir, imgName, imageSlice, min, max, xDim, yDim, false);
					
					System.err.println("sliceDir = " + sliceDir);
					System.err.println("imgName = " + imgName);
					
					if (orientation.equals("axial")) {
						axial_index++;
					} else if (orientation.equals("coronal")) {
						coronal_index++;
					} else if (orientation.equals("sagittal")) {
						sagittal_index++;
					}
				}

				imgBuffer = null;
				imageSlice.disposeLocal();
				imageSlice = null;

			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		newExtents = null;
	}
	
	

	public void saveImageSlice_test(ModelImage image, ModelImage imageMask, String orientation, String folder, String key) {

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];

		float[] res = image.getResolutions(0);
		float x_res = res[0];
		float y_res = res[1];
		
		float[] newRes = new float[2];
		newRes[0] = x_res;
		newRes[1] = y_res;

		int size_3D = xDim * yDim * zDim;

		int[] newExtents = new int[2];
		newExtents[0] = xDim;
		newExtents[1] = yDim;

		int size = xDim * yDim;

		float[] imageBuffer = new float[size_3D];

		try {
			image.exportData(0, imageBuffer.length, imageBuffer);
		} catch (Exception e) {
			e.printStackTrace();
		}

		float percentile_left = 0.01f;
		float percentile_right = 0.01f;

		int minIndex = (int) (size_3D * percentile_left);
		int maxIndex = (int) (size_3D * (1.0 - percentile_right));

		Arrays.sort(imageBuffer);

		float min = imageBuffer[minIndex];
		float max = imageBuffer[maxIndex];

		imageBuffer = null;

		for (int j = 0; j < zDim; j++) {

			try {

				ModelImage imageSlice = new ModelImage(ModelStorageBase.FLOAT,
						newExtents, "image" + j);
				imageSlice.getFileInfo(0).setResolutions(newRes);
				imageSlice.setResolutions(newRes);
				float[] imgBuffer = new float[size];
				image.exportData(j * size, size, imgBuffer);
				imageSlice.importData(0, imgBuffer, true);

				// new ViewJFrameImage(imageSlice);

				ModelImage maskSlice = new ModelImage(ModelStorageBase.SHORT,
						newExtents, "mask" + j);
				maskSlice.setResolutions(newRes);
				maskSlice.getFileInfo(0).setResolutions(newRes);
				short[] maskBuffer = new short[size];
				imageMask.exportData(j * size, size, maskBuffer);
				maskSlice.importData(0, maskBuffer, true);

				// new ViewJFrameImage(maskSlice);

				String sliceDir = outputDir + File.separator + folder + File.separator +  orientation
						+ File.separator;
				File sliceDirFile = new File(sliceDir);
				if (!sliceDirFile.isDirectory())
					sliceDirFile.mkdir();

				sliceDir += key + File.separator;
				sliceDirFile = new File(sliceDir);
				if (!sliceDirFile.isDirectory())
					sliceDirFile.mkdir();
				
				String imgName = null;
				String maskName = null;

				if (orientation.equals("axial")) {
					imgName = "image_" + axial_index + ".png";
					maskName = "voi_" + axial_index + ".png";
				} else if (orientation.equals("coronal")) {
					imgName = "image_" + coronal_index + ".png";
					maskName = "voi_" + coronal_index + ".png";
				} else if (orientation.equals("sagittal")) {
					imgName = "image_" + sagittal_index + ".png";
					maskName = "voi_" + sagittal_index + ".png";
				}

				if (imgName != null && maskName != null) {

					savePNGfile_test(sliceDir, imgName, imageSlice, min, max, xDim, yDim, false);
					savePNGfile_test(sliceDir, maskName, maskSlice, min, max, xDim, yDim, true);
					System.err.println("sliceDir = " + sliceDir);
					System.err.println("imgName = " + imgName);
					
					if (orientation.equals("axial")) {
						axial_index++;
					} else if (orientation.equals("coronal")) {
						coronal_index++;
					} else if (orientation.equals("sagittal")) {
						sagittal_index++;
					}
				}

				imgBuffer = null;
				imageSlice.disposeLocal();
				imageSlice = null;

				maskBuffer = null;
				maskSlice.disposeLocal();
				maskSlice = null;

			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		newExtents = null;
	}
	
	private void savePNGfile_test(String dirName, String fileName,
			ModelImage srcImage, float minIntensity, float maxIntensity,
			int xDim, int yDim, boolean isMask) {
		File file = null;
		boolean alpha = false;
		boolean gray = true;
		boolean indexed = false;
		try {

			ImageInfo imi = new ImageInfo(xDim, yDim, 8, alpha, gray, indexed);
			ImageLineByte line = new ImageLineByte(imi);
			int yMin = 0, yMax = yDim;
			int xMin = 0, xMax = xDim;
			int x, y;
			file = new File(dirName + File.separator + fileName);
			if (!file.exists()) {
				file.createNewFile();
			}

			OutputStream os = new FileOutputStream(file);

			PngWriter pngw = new PngWriter(os, imi);

			// System.err.println("xMin = " + xMin + "  xMax = " + xMax);

			for (int j = yMin; j < yMax; j++) {

				y = j - yMin;

				for (int i = xMin; i < xMax; i++) {

					x = i - xMin;

					if (isMask == false) {
						float intensity = srcImage.getFloat(i, j);
						float r = 0;
						if (intensity >= minIntensity
								&& intensity <= maxIntensity) {
							r = (float) ((intensity - minIntensity) * 255d / (maxIntensity - minIntensity));
						} else if (intensity > maxIntensity)
							r = 255;
						else if (intensity < minIntensity)
							r = 0;

						ImageLineHelper.setPixelGray8(line, x, (int) r);
					} else {

						short intensity = srcImage.getShort(i, j);

						if (intensity == 1) {
							// System.err.println("intensity = " + intensity);
							ImageLineHelper.setPixelGray8(line, x, (int) 255);
						} else {
							ImageLineHelper.setPixelGray8(line, x, (int) 0);
						}

					}

				}
				// System.err.println();
				pngw.writeRow(line, y);
			}
			pngw.end();
			pngw.close();
			pngw = null;
			os.close();
			os = null;
			line = null;
			imi = null;
			// System.err.println("testing array");

		} catch (Exception e) {
			System.err.println("image find wrong : " + file.getAbsolutePath());
			e.printStackTrace();

			System.exit(0);
		}
	}
	
	 /*
     * Save ski10 dataset 3D images into png 2D slices. 
     */
	public void saveMRI2Dslice() {


		Set<String> keys = keyImagesOrientation.keySet();

		int count = 0;
		
		String sliceDir = outputDir;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		saveImageDirectory_train = outputDir + File.separator + "train" + File.separator;
		sliceDir = saveImageDirectory_train;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		for (String key : keys) {

			System.err.println("save HED slice key = " + key);
			
				ModelImage axialImage = keyImagesOrientation.get(key);
				ModelImage axialImageMask = keyImagesOrientationMask.get(key);
				
				if (count >= 0 && count <= 10) {	
				    saveImage(axialImage, axialImageMask, "sagittal", "fold1");
				} else if (count >= 11 && count <= 20 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold2");	
				} else if (count >= 21 && count <= 30 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold3");	
				} else if (count >= 31 && count <= 40 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold4");	
				} else if (count >= 41 && count <= 50 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold5");	
				} else if (count >= 51 && count <= 60 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold6");	
				} else if (count >= 61 && count <= 70 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold7");	
				} else if (count >= 71 && count <= 80 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold8");	
				} else if (count >= 81 && count <= 90 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold9");	
				} else if (count >= 91 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold10");	
				}
				
				axialImage = null;
				axialImageMask = null;

			
			count++;
			
		}

	
	}

	/**
	 * Save 3D image into 2D slice png files.  
	 * @param image  MRI source iamge
	 * @param imageMask   corresponding ground truth image
	 * @param orientation  orientation. 
	 * @param folder    indicate which fold in the 10-fold cross-validation. 
	 */
	public void saveImage(ModelImage image, ModelImage imageMask, String orientation, String folder) {


		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];

		float[] res = image.getResolutions(0);
		float x_res = res[0];
		float y_res = res[1];
		float z_res = res[2];

		float[] newRes = new float[2];
		newRes[0] = x_res;
		newRes[1] = y_res;

		int size_3D = xDim * yDim * zDim;

		int[] newExtents = new int[2];
		newExtents[0] = xDim;
		newExtents[1] = yDim;

		int size = xDim * yDim;

		float[] imageBuffer = new float[size_3D];

		try {
			image.exportData(0, imageBuffer.length, imageBuffer);
		} catch (Exception e) {
			e.printStackTrace();
		}

		float percentile_left = 0.01f;
		float percentile_right = 0.01f;

		int minIndex = (int) (size_3D * percentile_left);
		int maxIndex = (int) (size_3D * (1.0 - percentile_right));

		Arrays.sort(imageBuffer);

		float min = imageBuffer[minIndex];
		float max = imageBuffer[maxIndex];

		imageBuffer = null;
		

		String sliceDir = saveImageDirectory_train + File.separator + folder + File.separator;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		sliceDir = sliceDir +  orientation + File.separator;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();

		for (int j = 0; j < zDim; j++) {

			try {

				ModelImage imageSlice = new ModelImage(ModelStorageBase.FLOAT,
						newExtents, "image" + j);
				imageSlice.getFileInfo(0).setResolutions(newRes);
				imageSlice.setResolutions(newRes);
				float[] imgBuffer = new float[size];
				image.exportData(j * size, size, imgBuffer);
				imageSlice.importData(0, imgBuffer, true);

				// new ViewJFrameImage(imageSlice);

				ModelImage maskSlice = new ModelImage(ModelStorageBase.SHORT,
						newExtents, "mask" + j);
				maskSlice.setResolutions(newRes);
				maskSlice.getFileInfo(0).setResolutions(newRes);
				short[] maskBuffer = new short[size];
				imageMask.exportData(j * size, size, maskBuffer);
				maskSlice.importData(0, maskBuffer, true);

				// new ViewJFrameImage(maskSlice);


				String imgName = null;
				String maskName = null;

				if (orientation.equals("axial")) {
					imgName = "image_" + axial_index + ".png";
					maskName = "voi_" + axial_index + ".png";
				} else if (orientation.equals("coronal")) {
					imgName = "image_" + coronal_index + ".png";
					maskName = "voi_" + coronal_index + ".png";
				} else if (orientation.equals("sagittal")) {
					imgName = "image_" + sagittal_index + ".png";
					maskName = "voi_" + sagittal_index + ".png";
				}

				if (imgName != null && maskName != null) {

					savePNGfile(sliceDir, imgName, imageSlice, min, max, xDim, yDim, false);
					savePNGfile(sliceDir, maskName, maskSlice, min, max, xDim, yDim, true);
					System.err.println("sliceDir = " + sliceDir);
					System.err.println("imgName = " + imgName);
					

					if (orientation.equals("axial")) {
						axial_index++;
					} else if (orientation.equals("coronal")) {
						coronal_index++;
					} else if (orientation.equals("sagittal")) {
						sagittal_index++;
					}
				}

				imgBuffer = null;
				imageSlice.disposeLocal();
				imageSlice = null;

				maskBuffer = null;
				maskSlice.disposeLocal();
				maskSlice = null;

			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		newExtents = null;
	
	}
	
	private void savePNGfile(String dirName, String fileName,
			ModelImage srcImage, float minIntensity, float maxIntensity,
			int xDim, int yDim, boolean isMask) {
		File file = null;
		boolean alpha = false;
		boolean gray = true;
		boolean indexed = false;
		try {

			ImageInfo imi = new ImageInfo(xDim, yDim, 8, alpha, gray, indexed);
			ImageLineByte line = new ImageLineByte(imi);
			int yMin = 0, yMax = yDim;
			int xMin = 0, xMax = xDim;
			int x, y;
			file = new File(dirName + File.separator + fileName);
			if (!file.exists()) {
				file.createNewFile();
			}

			OutputStream os = new FileOutputStream(file);

			PngWriter pngw = new PngWriter(os, imi);

			// System.err.println("xMin = " + xMin + "  xMax = " + xMax);

			for (int j = yMin; j < yMax; j++) {

				y = j - yMin;

				for (int i = xMin; i < xMax; i++) {

					x = i - xMin;

					if (isMask == false) {
						float intensity = srcImage.getFloat(i, j);
						float r = 0;
						if (intensity >= minIntensity
								&& intensity <= maxIntensity) {
							r = (float) ((intensity - minIntensity) * 255d / (maxIntensity - minIntensity));
						} else if (intensity > maxIntensity)
							r = 255;
						else if (intensity < minIntensity)
							r = 0;

						ImageLineHelper.setPixelGray8(line, x, (int) r);
					} else {

						short intensity = srcImage.getShort(i, j);

						if (intensity == 1) {
							// System.err.println("intensity = " + intensity);
							ImageLineHelper.setPixelGray8(line, x, (int) 255);
						} else {
							ImageLineHelper.setPixelGray8(line, x, (int) 0);
						}

					}

				}
				// System.err.println();
				pngw.writeRow(line, y);
			}
			pngw.end();
			pngw.close();
			pngw = null;
			os.close();
			os = null;
			line = null;
			imi = null;
			// System.err.println("testing array");

		} catch (Exception e) {
			System.err.println("image find wrong : " + file.getAbsolutePath());
			e.printStackTrace();

			System.exit(0);
		}
	}

	
	
	/**
	 * Normalize the image intensity between 25% and 75% intensity range value, 
	 * then scale by a factor of 1000.  Histogram equalization alike normalization 
	 * in the MRM paper. 
	 * 
	 * @param image
	 * @return  normalized and scaled image.
	 */
	public ModelImage scaleIntensity(ModelImage image) {

		int[] destExtents = new int[3];

		destExtents[0] = image.getExtents()[0];
		destExtents[1] = image.getExtents()[1];
		destExtents[2] = image.getExtents()[2];

		ModelImage resultImage = (ModelImage) image.clone();

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];

		int size_3D = xDim * yDim * zDim;
		float[] imageBuffer = new float[size_3D];
		float[] origBuffer = new float[size_3D];

		try {
			image.exportData(0, imageBuffer.length, imageBuffer);
			image.exportData(0, imageBuffer.length, origBuffer);
		} catch (Exception e) {
			e.printStackTrace();
		}

		float percentile_10 = 0.05f;
		float percentile_75 = 0.75f;
		float percentile_25 = 0.25f;
		float percentile_50 = 0.50f;
		float percentile_90 = 0.95f;

		int index_50 = (int) (size_3D * percentile_50);
		int index_75 = (int) (size_3D * percentile_75);
		int index_25 = (int) (size_3D * percentile_25);
		int index_10 = (int) (size_3D * percentile_10);
		int index_90 = (int) (size_3D * percentile_90);

		Arrays.sort(imageBuffer);

		float inten_75 = imageBuffer[index_75];
		float inten_25 = imageBuffer[index_25];
		float median = imageBuffer[index_50];
		float min = imageBuffer[index_10];
		float max = imageBuffer[index_90];

		float contrast = median + 2f * (inten_75 - inten_25);

		for (int i = 0; i < size_3D; i++) {

			if (origBuffer[i] <= min) {
				origBuffer[i] = min;
			}

			if (origBuffer[i] >= max) {
				origBuffer[i] = max;
			}

			origBuffer[i] = 1000f * (origBuffer[i] / contrast);
		}

		try {
			resultImage.importData(0, origBuffer, true);
		} catch (IOException e) {
			e.printStackTrace();
		}

		return resultImage;
	}
	
	
	/**
     * Read the SKI10 dataset ( .MHD image and .MHD ground truth labels ).
     */
	public void readFile() {

		int index;
		Set<String> keys = nameTableImages.keySet();
		String dir;
		String directory;
		String fileName;
		FileIO keyImageIO;
		
		try {
			    
			for (String key : keys) {
				
				System.err.println("axial key = " + key);
				dir = nameTableImages.get(key);
				if (dir == null)
					continue;
				index = dir.lastIndexOf(File.separator);
				directory = new String(dir.substring(0, index + 1));
				fileName = new String(dir.substring(index + 1, dir.length()));
				keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					keyImagesOrientation.put(key, image);
					keyImagesOrientation.get(key).setImageName(key);
					
				}
				keyImageIO.dispose();
				keyImageIO = null;
			
			}
			
			
			keys = nameTableImagesMask.keySet();

			for (String key : keys) {
				System.err.println("axial mask key = " + key);
				dir = nameTableImagesMask.get(key);
				if (dir == null)
					continue;
				index = dir.lastIndexOf(File.separator);
				directory = new String(dir.substring(0, index + 1));
				fileName = new String(dir.substring(index + 1, dir.length()));

				keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					
					keyImagesOrientationMask.put(key, image);
					keyImagesOrientationMask.get(key).setImageName(key);
					
				}
				keyImageIO.dispose();
				keyImageIO = null;
			}
		


		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/*
	 * Extract the ground truth label for femur, tibia, femur cartilage, tibia cartilage, etc.  
	 * For MRM paper, just the femur. 
	 */
	public void threshold() {

		Set<String> keys = keyImagesOrientationMask.keySet();

		for (String key : keys) {

				System.err.println("key = " + key);
				ModelImage maskImage = keyImagesOrientationMask.get(key);
				ModelImage thresImage = calculateThreshold(maskImage);
				maskThresAxial.put(key, thresImage);
			
		}

	}
	
	
	/**
	 * Extract the theshold from grount truth label.  
	 * @param srcImage
	 * @return
	 */
	private ModelImage calculateThreshold(ModelImage srcImage) {
		
		float[] thresholds = new float[2];
		// femur 
    	thresholds[0] = 1.0f;
    	thresholds[1] = 1.0f;
    	
    	// femur cartilage
    	// thresholds[0] = 2.0f;
    	// thresholds[1] = 2.0f;
    	
    	// tibia cartilage
    	// thresholds[0] = 4.0f;
    	// thresholds[1] = 4.0f;
    	
    	ModelImage thresholdImage = new ModelImage(ModelImage.UBYTE, srcImage.getExtents(), srcImage.getImageName()+ "threshold");
    	(thresholdImage.getFileInfo(0)).setModality(FileInfoBase.OTHER);
    	// fillValue not used
    	float fillValue = 0.0f;
    	int outputType = AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE;
    	boolean wholeImage = true;
    	boolean isInverse = false;
    	AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(thresholdImage, srcImage, thresholds, fillValue, outputType,
                wholeImage, isInverse);
    	thresholdAlgo.run();
    	FileInfoBase[] fileInfo = thresholdImage.getFileInfo();
        fileInfo[0].setModality(FileInfoBase.OTHER);
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());

        fileInfo[0].setEndianess(srcImage.getFileInfo()[0].getEndianess());
        fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(thresholdImage.getExtents());
        fileInfo[0].setMax(thresholdImage.getMax());
        fileInfo[0].setMin(thresholdImage.getMin());
        fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        fileInfo[0].setPixelPadValue(srcImage.getFileInfo()[0].getPixelPadValue());
        fileInfo[0].setPhotometric(srcImage.getFileInfo()[0].getPhotometric());
        thresholdAlgo.finalize();
        thresholdAlgo = null;
		
		return thresholdImage;
	}
	

	/**
	 * Traverse the folder
	 * @param dir
	 */
	private void traverse_folder(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				read_image_name(new File(dir, children[i]));
			}
		}

	}

	/**
	 * Read image full path for SKI10 dataset.  
	 * @param dir
	 */
	private void read_image_name(File dir) {

		String dirName = dir.toString();

		String lowerCaseName;
		String fileName;
		String hashID;

		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		lowerCaseName = dirName.toLowerCase();
		fileName = lowerCaseName.substring(begin, end);
		begin = fileName.lastIndexOf("-") + 1;
		end = fileName.lastIndexOf(".");

		hashID = fileName.substring(begin, end);

		if (fileName.startsWith("image") && fileName.endsWith("mhd")) {
			// System.err.println("hashID = " + hashID + "  dirName = " + dirName);
			nameTableImages.put(hashID, dirName);
		}

		if (fileName.startsWith("labels") && fileName.endsWith("mhd")) {
			// System.err.println("hashID = " + hashID + "  dirName = " + dirName);
			nameTableImagesMask.put(hashID, dirName);
		}

	}


      
      
	
}

