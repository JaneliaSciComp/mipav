
//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
 ******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

 ******************************************************************
 ******************************************************************/

import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmConstrainedOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelImageToImageJConversion;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;
import ij.ImagePlus;
import ij.ImageStack;

import java.io.File;
import java.util.Vector;

import javax.swing.JProgressBar;

/**
 * Implements batch algorithms for worm untwisting:
 * 1. automatic seam cell detection
 * 2. automatic lattice-building
 * 3. untwisting the worm
 * 4. registering straightened images and calculating the maximum-intensity projectsions
 * Supports the user-interface for the user to view/edit the results of the automatic batch processes.
 */
public class PlugInAlgorithmWormUntwisting
{

	public static final String autoSeamCellSegmentationOutput = new String("seam_cells");
	public static final String editSeamCellOutput = new String("seam_cell_final");
	public static final String autoLatticeGenerationOutput = new String("lattice_");
	public static final String editLatticeOutput = new String("lattice_final");
	public static final String editAnnotationInput = new String("annotation");
	public static final String editAnnotationOutput = new String("annotation_final");
	public static final String outputImages = new String("output_images");
	public static final String straightenedLattice = new String("straightened_lattice");
	public static final String straightenedAnnotations = new String("straightened_annotations");
	
	/**
	 * Runs the automatic lattice-building algorithm on a batch of files.
	 * @param batchProgress progress bar (may be null).
	 * @param includeRange the list of file IDs to run the algorithm on.
	 * @param baseFileDir  the base file directory containing the volume data files.
	 * @param baseFileName  the base file name to which the file ID is added to generate the full file name.
	 */
	public static void buildLattice( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName )
	{
		ModelImage image = null;
		if ( includeRange != null )
		{
			int count = 0;
			int foundCount = 0;
			// Loop over the images in the list:
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				// build the full image file name:
				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
				FileIO fileIO = new FileIO();
				if ( image != null )
				{
					image.disposeLocal(false);
				}
				image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
				WormData wormData = new WormData(image);
				foundCount += wormData.generateLattice() > 0 ? 1 : 0;
				wormData.dispose();
				count++;
			}
			MipavUtil.displayInfo( "Found lattices for " + foundCount + " out of " + count + " volumes tested (" + (int)(100 * (float)foundCount/count) + "%)" );
		}

		if ( image != null )
		{
			image.disposeLocal(false);
			image = null;
		}		
	}
	
	/**
	 * Run the maximum projection algorithm on the batch of input files.
	 * @param batchProgress progress bar (may be null).
	 * @param includeRange the list of file IDs to run the algorithm on.
	 * @param baseFileDir  the base file directory containing the volume data files.
	 * @param baseFileName  the base file name to which the file ID is added to generate the full file name.
	 */
	public static void calcMaxProjection(JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName)
	{
		ModelImage image = null, maximumProjectionImage = null;
		int[] currentMPImage = new int[]{0};
		if ( includeRange != null )
		{
			int mpSliceCount = 0;
			// Count the number of input files (must exist in the file system):
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileName + "_" + includeRange.elementAt(i) + "_straight_register" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{					
					mpSliceCount++;
				}
			}
			// Calculate the maximum projection slices and store them in an image stack:
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileName + "_" + includeRange.elementAt(i) + "_straight_register" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);

//				System.err.println( fileName );
				if ( voiFile.exists() )
				{
					FileIO fileIO = new FileIO();
					if( image != null )
					{
						if ( (i%10) == 0 )
						{
							image.disposeLocal(true);
						}
						else
						{
							image.disposeLocal();
						}
						image = null;
					}
					image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
					image.calcMinMax();
					if ( maximumProjectionImage == null )
					{
						int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1; // dimX
						int dimY = image.getExtents().length > 2 ? image.getExtents()[2] : 1; // use dimZ for y projection
						maximumProjectionImage = new ModelImage( image.getType(), new int[]{ dimX, dimY, mpSliceCount}, baseFileName + "_MP_Y.tif" );
						currentMPImage[0] = 0;
					}
					calcMaximumProjectionY( image, maximumProjectionImage, currentMPImage );
				}
				if ( batchProgress != null )
				{
					batchProgress.setValue((int)(100 * (float)(i+1)/includeRange.size()));
					batchProgress.update(batchProgress.getGraphics());
				}
			}
		}
		
		if ( image != null )
		{
			image.disposeLocal();
			image = null;
		}

		// Write the image stack to file:
		if ( maximumProjectionImage != null )
		{
			String fileName = baseFileDir + File.separator;
//			System.err.println( "Saving mp image to : " + fileName + " " + maximumProjectionImage.getImageName() + ".tif" );
			ModelImage.saveImage( maximumProjectionImage, maximumProjectionImage.getImageName() + ".tif", fileName, false ); 

			maximumProjectionImage.calcMinMax();
//			new ViewJFrameImage(maximumProjectionImage);
			
			// Launch ImageJ for viewing:
            final ImageStack is = ModelImageToImageJConversion.convert3D(maximumProjectionImage);
            new ImagePlus("ImageJ:" + maximumProjectionImage.getImageName(), is).show();
            new ij.ImageJ();
            
			maximumProjectionImage.disposeLocal();
			maximumProjectionImage = null;
		}
	}

	/**
	 * Runs an algorithm that first registers all input images then generates the maximum-projection image stack for the list of input files.
	 * @param batchProgress progress bar (may be null).
	 * @param includeRange the list of file IDs to run the algorithm on.
	 * @param baseFileDir  the base file directory containing the volume data files.
	 * @param baseFileName  the base file name to which the file ID is added to generate the full file name.
	 */
	public static void createMaximumProjectionAVI( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName)
	{
		// register images and save output files:
		registerImages(batchProgress, includeRange, baseFileDir, baseFileName);
		// calculate the maximum projection of each image and save the image stack:
		calcMaxProjection(batchProgress, includeRange, baseFileDir, baseFileName);		
	}

	/**
	 * Run the lattice-based worm straightening algorithm for the list of input files.
	 * @param batchProgress progress bar (may be null).
	 * @param includeRange the list of file IDs to run the algorithm on.
	 * @param baseFileDir  the base file directory containing the volume data files.
	 * @param baseFileName  the base file name to which the file ID is added to generate the full file name.
	 */
	public static void latticeStraighten( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName )
	{
		ModelImage wormImage = null;
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				//    	    		String fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
				// Build the full image name:
				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
//					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					wormImage = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
					WormData wormData = new WormData(wormImage); 
					VOI lattice = wormData.readFinalLattice();
					if ( lattice == null )
					{
						MipavUtil.displayError( "Error in reading lattice file" );
					}

					ModelImage seamImage = wormData.readSeamSegmentation();					
					LatticeModel model = new LatticeModel(wormImage);
					model.setSeamCellImage(seamImage);
					model.setLattice(lattice);
					
//							String nucleiFileName = baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + File.separator + "marker" + File.separator + "marker.csv";
//							File nucleiFile = new File(nucleiFileName);
//							if ( nucleiFile.exists() )
//							{
//								VOIVector vois = readMarkerPositions( nucleiFileName, nucleiFile );
//								VOI nucleiVOIs = vois.elementAt(0);
//								if ( (nucleiVOIs != null) && (nucleiVOIs.getCurves().size() > 0) )
//								{
//									model.setMarkers(nucleiVOIs);
//								}
//							}

							model.interpolateLattice( false, false, true, false );
							ModelImage contourImage = null;
//							if ( segmentSkinSurface.isSelected() )
							{
								contourImage = model.segmentSkin(wormImage, 0);
							}
//							else if ( segmentLattice.isSelected() )
//							{
//								model.segmentLattice(wormImage, false);
//							}
							model.retwist(wormImage);
							wormData.readMarkers();
							VOI markers = wormData.getMarkerAnnotations();
							if ( markers != null )
							{
								model.setMarkers(markers);
								model.interpolateLattice( false, false, false, true );	
							}

//							if ( nucleiImage != null )
//							{					
//
//							if ( wormImage != null )
//							{
//								wormImage.unregisterAllVOIs();
//								wormImage.disposeLocal(false);
//							}
//								nucleiImage.setImageName(baseFileName2 + "_" + includeRange.elementAt(i) + ".tif");
//								model.setImage(nucleiImage);
//								model.interpolateLattice( false, false, straightenImageCheck.isSelected(), false );
//
//								if ( segmentSkinSurface.isSelected() )
//								{
//									contourImage = model.segmentSkin(nucleiImage, contourImage, paddingFactor);
//								}
//								else if ( segmentLattice.isSelected() )
//								{
//									model.segmentLattice(nucleiImage, false);
//								}
//								model.dispose();
//								model = null;
//
//								if ( nucleiImage != null )
//								{
//									nucleiImage.disposeLocal(false);
//								}
//							}						

							if ( contourImage != null )
							{
								contourImage.disposeLocal(false);
							}
							
						model.dispose();
						model = null;
						if ( wormImage != null )
						{
							wormImage.disposeLocal(false);
						}
						if ( wormData != null )
						{
							wormData.dispose();
						}
						
				}
				if ( batchProgress != null )
				{
					batchProgress.setValue((int)(100 * (float)(i+1)/includeRange.size()));
					batchProgress.update(batchProgress.getGraphics());
				}
			}
		}

		if ( wormImage != null )
		{
			wormImage.disposeLocal(false);
		}
		
		MipavUtil.displayInfo( "Lattice straightening complete." );
	}
	
	/**
	 * @param image the ModelImage for registering loaded VOIs.
	 * @param voiDir the directory to load voi's from.
	 * @param quietMode if true indicates that warnings should not be displayed.
	 * @param resultVector the result VOI Vector containing the loaded VOIs.
	 * @param registerVOIs when true the VOIs are registered in the input image.
	 */
	public static void loadAllVOIsFrom(ModelImage image, final String voiDir, boolean quietMode, VOIVector resultVector, boolean registerVOIs) {

		int i, j;
		VOI[] VOIs;
		FileVOI fileVOI;

		try {

			// if voiDir does not exist, then return
			// if voiDir exists, then get list of voi's from directory (*.voi)
			final File voiFileDir = new File(voiDir);
			final Vector<String> filenames = new Vector<String>();
			final Vector<Boolean> isLabel = new Vector<Boolean>();

			if (voiFileDir.exists() && voiFileDir.isDirectory()) {

				// get list of files
				final File[] files = voiFileDir.listFiles();

				for (final File element : files) {

					if (element.getName().endsWith(".voi") || element.getName().endsWith(".xml")) {
						filenames.add(element.getName());
						isLabel.add(false);
					} else if (element.getName().endsWith(".lbl")) {
						filenames.add(element.getName());
						isLabel.add(true);
					}
				}
			} else { // voiFileDir either doesn't exist, or isn't a directory

				if ( !quietMode) {
					MipavUtil.displayError("No VOIs are found in directory: " + voiDir);
				}

				return;
			}

			// open each voi array, then register voi array to this image
			for (i = 0; i < filenames.size(); i++) {

				fileVOI = new FileVOI( (filenames.elementAt(i)), voiDir, image);

				VOIs = fileVOI.readVOI(isLabel.get(i));

				for (j = 0; j < VOIs.length; j++) {

                    if ( registerVOIs )
                    {   	
                    	image.registerVOI(VOIs[j]);
                    }
					if ( resultVector != null )
					{
						resultVector.add(VOIs[j]);
					}
				}
			}
            // when everything's done, notify the image listeners
            if ( registerVOIs )
            {   	
            	image.notifyImageDisplayListeners();
            }

		} catch (final Exception error) {

			if ( !quietMode) {
				MipavUtil.displayError("Error loading all VOIs from " + voiDir + ": " + error);
			}
		}

	}
	

	/**
	 * Runs an algorithm that registers all input images in sequence, first padding each image so all are the same size.
	 * @param batchProgress progress bar (may be null).
	 * @param includeRange the list of file IDs to run the algorithm on.
	 * @param baseFileDir  the base file directory containing the volume data files.
	 * @param baseFileName  the base file name to which the file ID is added to generate the full file name.
	 */
	public static void registerImages( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName)
	{
		ModelImage image = null, prevImage = null;
		int[] maxExtents = new int[]{0,0,0};
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileName + "_" + includeRange.elementAt(i) + "_straight" + ".xml";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					FileIO fileIO = new FileIO();
					image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
					int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 0;  
					int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 0;  
					int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 0;
					maxExtents[0] = Math.max(maxExtents[0], dimX);
					maxExtents[1] = Math.max(maxExtents[1], dimY);
					maxExtents[2] = Math.max(maxExtents[2], dimZ);
					if ( image != null )
					{
						image.disposeLocal();
						image = null;
					}
				}
			}

			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileName + "_" + includeRange.elementAt(i) + "_straight" + ".xml";
				File voiFile = new File(baseFileDir + File.separator + fileName);

				File registrationDir = new File(baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images");
				if ( voiFile.exists() )
				{
//					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  

					ModelImage result = register( prevImage, image, registrationDir, maxExtents );
					if ( result == null )
					{
						prevImage = image;
					}
					else
					{
						if ( image != null )
						{
							image.disposeLocal();
							image = null;
						}
						if ( prevImage != null )
						{
							prevImage.disposeLocal();
							prevImage = null;
						}
						prevImage = result;
					}
				}

				if ( batchProgress != null )
				{
					batchProgress.setValue((int)(100 * (float)(i+1)/includeRange.size()));
					batchProgress.update(batchProgress.getGraphics());
				}
			}
		}

		if ( image != null )
		{
			image.disposeLocal();
			image = null;
		}
		if ( prevImage != null )
		{
			prevImage.disposeLocal();
			prevImage = null;
		}
	}
	
	/**
	 * Run the automatic seam-cell segmentation algorithm for the list of input files.
	 * @param batchProgress progress bar (may be null).
	 * @param includeRange the list of file IDs to run the algorithm on.
	 * @param baseFileDir  the base file directory containing the volume data files.
	 * @param baseFileName  the base file name to which the file ID is added to generate the full file name.
	 */
	public static void segmentSeamCells( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName )
	{
		ModelImage image = null; 
		if ( includeRange != null )
		{
			int numSteps = 6;
			int step = 0;
			int foundCount = 0;
			int count = 0;
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					step = 1;
					FileIO fileIO = new FileIO();
					if(image != null) {
						if ( (i%10) == 0 )
						{
							image.disposeLocal(true);
						}
						else
						{
							image.disposeLocal();
						}
						image = null;
					}
					image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null); 
					image.calcMinMax();
					String imageName = image.getImageName();
					if (imageName.contains("_clone")) {
						imageName = imageName.replaceAll("_clone", "");
					}
					if (imageName.contains("_laplace")) {
						imageName = imageName.replaceAll("_laplace", "");
					}
					if (imageName.contains("_gblur")) {
						imageName = imageName.replaceAll("_gblur", "");
					}
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
					if ( image.isColorImage() )
					{
						// Extract the green channel for segmentation:
						System.err.println("extracting green channel");
						ModelImage green = extractGreen(image);
						if ( green != null )
						{
							String name = image.getImageName();
							image.disposeLocal();
							image = green;
							image.setImageName(name);
						}
					}
					WormData wormData = new WormData(image); 
					wormData.segmentSeamCells(8, 25);
					if ( wormData.getSeamCells().size() > 0 )
					{
						foundCount++;
					}
					count++;
					wormData.dispose();
					
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
				}
			}
			if ( batchProgress != null )
			{
				batchProgress.setValue(100);
				batchProgress.update(batchProgress.getGraphics());
			}
			MipavUtil.displayInfo( "Finished seam cell segmentation. Segmented " + foundCount + " out of " + count + " volumes tested (" + (int)(100 * (float)foundCount/count) + "%)" );
		}
		
		if ( image != null )
		{
			image.disposeLocal();
			image = null;
		}
	}

	/**
	 * Calculate the maximum projection along the y-axis of the input image. The 2D projection is saved in the output image.
	 * @param image input image to project.
	 * @param maximumProjectionImage output image for saving the 2D projection slice.
	 * @param zSlice current slice to write into (will be incremented by this function if successful).
	 */
	private static void calcMaximumProjectionY( ModelImage image, ModelImage maximumProjectionImage, int[] zSlice )
	{
//		System.err.println( image.getExtents()[0] + " " + image.getExtents()[1] + " " + image.getExtents()[2] );
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		AlgorithmMaximumIntensityProjection mipAlgo = null;
		// Make algorithm
		if (image.isColorImage()) {
			mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
					0, dimY -1, dimY,
					image.getMinR(), image.getMaxR(),
					image.getMinG(), image.getMaxG(), 
					image.getMinB(), image.getMaxB(),
					true, false, 1);    
		}
		else {
			mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
					0, dimY -1, dimY,
					image.getMin(), image.getMax(),
					true, false, 1 );
		}
		mipAlgo.setRunningInSeparateThread(false);
		mipAlgo.run();

		Vector<ModelImage> resultImages = mipAlgo.getResultImage();
		if ( resultImages.size() > 0 )
		{
			ModelImage mp = resultImages.elementAt(0);
			int dimX = maximumProjectionImage.getExtents().length > 0 ? maximumProjectionImage.getExtents()[0] : 1;
			dimY = maximumProjectionImage.getExtents().length > 1 ? maximumProjectionImage.getExtents()[1] : 1;

			
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( image.isColorImage() )
					{
						int index = y*dimX + x;
						for ( int c = 0; c < 4; c++ )
						{
							maximumProjectionImage.setC(x, y, zSlice[0], c, mp.getC( index, c ));
						}
					}
					else
					{
						maximumProjectionImage.set(x, y, zSlice[0], mp.get(x, y) );
					}
				}
			}

			zSlice[0]++;
		}
		mipAlgo.finalize();
	}



	/**
	 * Calculates and returns the transformed image after the image registration algorithm completes:
	 * @param reg3
	 * @param refImage
	 * @param matchImage
	 * @return
	 */
	private static ModelImage getTransformedImage( AlgorithmConstrainedOAR3D reg3, ModelImage refImage, ModelImage matchImage )
	{
		// get the image extents and resolutions from the reference image:
		final int xdimA = refImage.getExtents()[0];
		final int ydimA = refImage.getExtents()[1];
		final int zdimA = refImage.getExtents()[2];
		final float xresA = refImage.getFileInfo(0).getResolutions()[0];
		final float yresA = refImage.getFileInfo(0).getResolutions()[1];
		final float zresA = refImage.getFileInfo(0).getResolutions()[2];
		if (reg3.isCompleted()) {
			// get the transformation from the registration algorithm:
			final TransMatrix finalMatrix = reg3.getTransform();
			// transform the input image (matchImage) based on the transformation:
			final String name = JDialogBase.makeImageName(matchImage.getImageName(), "_register");
			AlgorithmTransform transform = new AlgorithmTransform(matchImage, finalMatrix, 0, xresA, yresA, zresA, xdimA,
					ydimA, zdimA, true, false, false);

			transform.setUpdateOriginFlag(true);
			transform.setFillValue((float)matchImage.getMin());
			transform.run();
			ModelImage resultImage = transform.getTransformedImage();
			if ( resultImage != null )
			{
				resultImage.calcMinMax();
				resultImage.setImageName(name);

				resultImage.getMatrixHolder().replaceMatrices(refImage.getMatrixHolder().getMatrices());

				for (int i = 0; i < resultImage.getExtents()[2]; i++) {
					resultImage.getFileInfo(i).setOrigin(refImage.getFileInfo(i).getOrigin());
				}
			}

			transform.finalize();
			if (transform != null) {
				transform.disposeLocal();
				transform = null;
			}
			// return the transformed image:
			return resultImage;
		}
		return null;
	}
	


	/**
	 * Register the input image to the previous image in the sequence.
	 * @param prevImage image to register to.
	 * @param image input image to transform.
	 * @param registrationDir output directory for the registered image.
	 * @param maxExtents extents of the registered images in the sequence.
	 * @return the transformed image.
	 */
	private static ModelImage register( ModelImage prevImage, ModelImage image, final File registrationDir, final int[] maxExtents )
	{
		int[] marginX = new int[2];
		int[] marginY = new int[2];
		int[] marginZ = new int[2];

		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 0;  
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 0;  
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 0;

		ModelImage regInput;
		ModelImage regTo = prevImage;
		ModelImage result = null;
		ModelImage padResult = null;
		AlgorithmAddMargins pad = null;
		if ( dimX != maxExtents[0] || dimY != maxExtents[1] || dimZ != maxExtents[2] )
		{
			int diff = maxExtents[0] - dimX;
			marginX[0] = diff/2;
			marginX[1] = diff - marginX[0];

			diff = maxExtents[1] - dimY;
			marginY[0] = diff/2;
			marginY[1] = diff - marginY[0];

			diff = maxExtents[2] - dimZ;
			marginZ[0] = 0;
			marginZ[1] = diff;


			padResult = new ModelImage( image.getType(), maxExtents, image.getImageName() + "_pad" );
			JDialogBase.updateFileInfo( image, padResult );
			pad = new AlgorithmAddMargins(image, padResult, marginX, marginY, marginZ );
			pad.setRunningInSeparateThread(false);
			pad.run();
			regInput = padResult;

			pad.finalize();
			pad = null;
		}
		else
		{
			regInput = image;
		}
		if ( regTo == null )
		{
			result = regInput;
			saveTransformImage( registrationDir, result );
		}
		else
		{
//			AlgorithmConstrainedOAR3D reg = new AlgorithmConstrainedOAR3D( regTo, regInput, AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
//                    -10, 20, -10, 20, -10, 20, 3, 3, 3, new float[][]{{-30,-30,-30},{30,30,30}},                     
//                    true, true, false, true, 10, 10, 5);
			AlgorithmConstrainedOAR3D reg = new AlgorithmConstrainedOAR3D( regTo, regInput, AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
                    -10, 20, -10, 20, -10, 20, 3, 3, 3, new float[][]{{-30,-30,-30},{30,30,30}},                     
                    true, false, true, false, 10, 10, 5);
                    
//			AlgorithmRegOAR3D reg = new AlgorithmRegOAR3D(regTo, regInput,
//					AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
//					-10, 10, 5, 2, -10, 10, 5, 2, -15, 15, 7, 2, 
//					true, true, true, false, 2, 3);
			reg.setRunningInSeparateThread(false);
			reg.run();
			result = getTransformedImage( reg, regTo, regInput );
			if ( result != null )
			{
				saveTransformImage( registrationDir, result );
			}
			reg.finalize();
			reg = null;
		}
		if ( (result != padResult) && (padResult != null) )
		{
			padResult.disposeLocal();
			padResult = null;
		}
		
		return result;
	}
	
	/**
	 * Saves the input image to the specified output directory.
	 * @param dir output directory.
	 * @param image image to save.
	 */
	private static void saveTransformImage( File dir, ModelImage image  ) 
	{
		String imageName = image.getImageName();
		if ( imageName.contains("_pad") )
		{
			imageName = imageName.replaceAll("_pad", "" );
		}
		if ( imageName.contains("_register") )
		{
			imageName = imageName.replaceAll("_register", "" );
		}
		imageName = imageName + "_register";
		String voiDir = dir.getAbsolutePath() + File.separator;
		//        ModelImage.saveImage( image, image.getImageName() + ".xml", voiDir );
		//		System.err.println( imageName + ".tif" + "   " + voiDir );
		image.setImageName(imageName);
		ModelImage.saveImage( image, imageName + ".tif", voiDir, false ); 
	}
	

	private static ModelImage extractGreen(ModelImage image)
	{
		ModelImage resultImageG = null;
        if (image.getType() == ModelStorageBase.ARGB) {
            resultImageG = new ModelImage(ModelImage.UBYTE, image.getExtents(), "GrayG");
        } else if (image.getType() == ModelStorageBase.ARGB_USHORT) {
            resultImageG = new ModelImage(ModelImage.USHORT, image.getExtents(), "GrayG");
        } else if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
            resultImageG = new ModelImage(ModelImage.FLOAT, image.getExtents(), "GrayG");
        }
        if ( resultImageG == null )
        {
        	return null;
        }
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 0;  
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 0;  
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 0;
        for ( int z = 0; z < dimZ; z++ )
        {
        	for ( int y = 0; y < dimY; y++ )
        	{
        		for ( int x = 0; x < dimX; x++ )
        		{
        			int index = z * dimX*dimY + y * dimX + x;
        			resultImageG.set( index, image.getC(index, 2) );
        		}
        	}
        }
        resultImageG.calcMinMax();
        JDialogBase.updateFileInfo(image, resultImageG);
        return resultImageG;
	}


	public PlugInAlgorithmWormUntwisting()	{}


}
