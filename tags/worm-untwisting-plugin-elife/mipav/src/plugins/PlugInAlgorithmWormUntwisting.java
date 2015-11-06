
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

import gov.nih.mipav.model.algorithms.AlgorithmBase;
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
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogRGBtoGrays;
import gov.nih.mipav.view.renderer.WildMagic.Render.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentation;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentationKMeans;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentationLoG;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentationWindowing;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;
import ij.ImagePlus;
import ij.ImageStack;

import java.awt.Color;
import java.io.File;
import java.util.Arrays;
import java.util.Vector;

import javax.swing.JProgressBar;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;

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
	
	private static final int minPairDist = 5;
	private static final int maxPairDist = 15;		
	private static final float tenMinDist = 1;
	private static final float tenMaxDist = 5;  
	private static final float noseP1MinDist = 10;
	private static final float noseP1MaxDist = 30;
	private static final int minSequenceMidDist = 4;
	private static final int maxSequenceMidDist = 30;
	private static final int minSequenceDist = 4;
	private static final int maxSequenceDist = 25;
	private static final int sequenceDistDiffLimit = 12;
	private static final double sequenceTwistLimit = (Math.PI/2f);
	private static final int wormLengthMin = 100;
	private static final int wormLengthMax = 140;
	private static final int sequenceBendMin = 6;
	private static final int sequenceBendMax = 12;
	private static final int sequenceTimeLimit = 1000;

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
					image.disposeLocal();
				}
				image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
//				System.err.println( fileName );
				
				// Open the seam cells files as input to the algorithm:
				// an unordered list of up to 22 positions in the volume representing the seam cell locations
				VOIVector seamCells = new VOIVector();
				// start with the user edited version:
				fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + editSeamCellOutput;
				String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
				loadAllVOIsFrom(image, voiDir, true, seamCells, false);
				if ( seamCells.size() <= 0 )
				{
					// if no edited version, use the default automatically-segmented seam cell file:
					fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + autoSeamCellSegmentationOutput;
					voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(image, voiDir, true, seamCells, false);
				}
				if ( seamCells.size() == 0 )
				{
					continue;
				}
				// find the 'nose' position labeled by the user (may be excluded)
				VOI nose = new VOI( (short)1, "nose", VOI.ANNOTATION, 1);
				for ( int j = seamCells.elementAt(0).getCurves().size() - 1; j >= 0; j-- )
				{
					VOIText text = (VOIText) seamCells.elementAt(0).getCurves().elementAt(j);
					// skip extra annotations (origin, nose)
					if ( text.getText().contains("nose") || text.getText().contains("Nose") )
					{
						nose.getCurves().add(seamCells.elementAt(0).getCurves().remove(j));
					}
				}
				// build the lattices from input image and list of points:
				if ( buildLattice( batchProgress, i, includeRange.size(), image, seamCells.elementAt(0), nose, includeRange.elementAt(i), baseFileDir, baseFileName ) )
				{
					foundCount++;
				}

				count++;
			}
//			System.err.println( "Tested " + count + " success " + foundCount + " success rate = " + 100 * (float)foundCount/count +"%" );
			MipavUtil.displayInfo( "Found lattices for " + foundCount + " out of " + count + " volumes tested (" + (int)(100 * (float)foundCount/count) + "%)" );
		}

		if ( image != null )
		{
			image.disposeLocal();
			image = null;
		}
//		System.err.println( "Done lattice building" );
		
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

//		String fileName = baseFileName + "_MP_Y.tif";
//		File voiFile = new File(baseFileDir + File.separator + fileName);		
//		FileIO fileIO = new FileIO();
//		maximumProjectionImage = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
		
		// Write the image stack to file:
		if ( maximumProjectionImage != null )
		{
			String fileName = baseFileDir + File.separator;
//			System.err.println( "Saving mp image to : " + fileName + " " + maximumProjectionImage.getImageName() + ".tif" );
			ModelImage.saveImage( maximumProjectionImage, maximumProjectionImage.getImageName() + ".tif", fileName, false ); 
			
			// Launch ImageJ for viewing:
            final ImageStack is = ModelImageToImageJConversion.convert3D(maximumProjectionImage);
            new ImagePlus("ImageJ:" + maximumProjectionImage.getImageName(), is).show();
            new ij.ImageJ();
            
//			maximumProjectionImage.calcMinMax();
//			new ViewJFrameImage(maximumProjectionImage);
//			maximumProjectionImage.disposeLocal();
//			maximumProjectionImage = null;
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
		ModelImage image = null;
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

					// load the user-edited lattice if it exists:
					fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + editLatticeOutput;
					VOIVector lattice = new VOIVector();
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(image, voiDir, true, lattice, false);
					if ( lattice.size() == 0 )
					{
						// otherwise load the default automatically-generated lattice:
						fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + autoLatticeGenerationOutput + "1";
						lattice = new VOIVector();
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(image, voiDir, true, lattice, false);						
					}
					if ( lattice.size() == 0 )
					{
						continue;
					}
					// Load user-specified annotations, they are included in the straightening process:
					if ( (lattice.elementAt(0) != null) && (lattice.elementAt(0).getCurves().size() == 2) )
					{
						LatticeModel model = new LatticeModel( image, lattice.elementAt(0) );
						// load user-edited annotations:
						fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + editAnnotationOutput;            	    		
						VOIVector annotations = new VOIVector();
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(image, voiDir, true, annotations, false);
						if ( annotations.size() > 0 )
						{
							model.setAnnotations( annotations.elementAt(0) );
						}
						else
						{
							// otherwise load default annotation file:
							fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + editAnnotationInput;            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							loadAllVOIsFrom(image, voiDir, true, annotations, false);
							if ( annotations.size() > 0 )
							{
								model.setAnnotations( annotations.elementAt(0) );
							}							
						}
						// interpolate the lattice and untwist the worm, saving the output images and straightened lattice + annotations:
						model.interpolateLattice( false );
						model.dispose();
						model = null;
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
//					System.err.print( fileName + "   " );
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

					Vector<Vector3f> dataPoints = new Vector<Vector3f>();
					// calculate a robust histogram of the data values, used to find the left-right marker thresholds:
					float[] max_LR = WormSegmentationWindowing.robustHistogram(image, dataPoints);	
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
					// Calculate the K-Means segmentation:
					Vector<Vector3f> annotationskMeans = WormSegmentationKMeans.seamCellSegmentation(image);
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
					
					// Calculate the windowing segmentation:
					Vector<Vector3f> annotationsWindow = WormSegmentationWindowing.seamCellSegmentation(image, max_LR[0], max_LR[1]);
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
					
					// Calculate the Laplacian of Gaussian segmentation:
					Vector<Vector3f> annotationsLoG = WormSegmentationLoG.seamCellSegmentation(image);	
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
					
					Vector<Vector3f> seamCells = new Vector<Vector3f>();
					Vector<Vector3f> tempSeamCells = new Vector<Vector3f>();
					for ( int k = 0; k < annotationsWindow.size(); k++ )
					{
						tempSeamCells.add( new Vector3f(annotationsWindow.elementAt(k)) );
					}
					for ( int k = 0; k < annotationskMeans.size(); k++ )
					{
						tempSeamCells.add( new Vector3f(annotationskMeans.elementAt(k)) );
					}
					for ( int k = 0; k < annotationsLoG.size(); k++ )
					{
						tempSeamCells.add( new Vector3f(annotationsLoG.elementAt(k)) );						
					}
					int prevSize = tempSeamCells.size();
					int newSize = WormSegmentation.reduceDuplicates(image, tempSeamCells, 5, 15, false);
					while ( (prevSize > newSize) && (newSize > 22) )
					{
						prevSize = newSize;
						newSize = WormSegmentation.reduceDuplicates(image, tempSeamCells, 5, 15, false);
					}
					if ( newSize > 22 )
					{
						newSize = WormSegmentation.reduceDuplicates(image, tempSeamCells);
					}
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}

					Vector3f negCenter = new Vector3f(-1,-1,-1);
					for ( int j = 0; j < tempSeamCells.size(); j++ )
					{
						if ( !tempSeamCells.elementAt(j).equals(negCenter) )
						{
							seamCells.add(tempSeamCells.elementAt(j));
						}
					}
					if ( seamCells.size() > 0 )
					{
						foundCount++;
					}
					count++;
					WormSegmentation.saveAnnotations(image, seamCells, Color.blue );
					final File voiFileDir = new File(baseFileDir + File.separator +  baseFileName + "_" + includeRange.elementAt(i) + File.separator);
					if ( !voiFileDir.exists()) {
						voiFileDir.mkdir();
					}
					fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + autoSeamCellSegmentationOutput;  
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					WormSegmentation.saveAllVOIsTo(voiDir, image);
				}
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
	 * Automatically builds lattices for a given image and set of unordered seam cell positions.
	 * The lattices are tested and ranked based on measures of curvature and self-intersection.
	 * The top 5 lattices are saved to file for the user to evaluate and select.
	 * @param batchProgress progress bar (may be null).
	 * @param imageCount current image (for progress)
	 * @param numImages total images to process (for progress)
	 * @param image input image to build the lattice on
	 * @param annotations unordered list of seam cell positions
	 * @param nose list of nose positions
	 * @param time file ID
	 * @param baseFileDir  the base file directory containing the volume data files.
	 * @param baseFileName  the base file name to which the file ID is added to generate the full file name.
	 * @return true if the algorithm successfully build at least one lattice for the inputs.
	 */
	private static boolean buildLattice( JProgressBar batchProgress, int imageCount, int numImages, ModelImage image, VOI annotations, VOI nose, int time, String baseFileDir, String baseFileName )
	{
		int numSteps = 3;
		int step = 1;
		long startTime = System.currentTimeMillis();
		boolean print = false;
		/** Step (1) Attempt to find the tenth pair of seam cells in the lattice. 
		 * The 10th pair is distinct in that it has the smallest between-cell distance */
		Vector<int[]> tenthPairs = new Vector<int[]>();
		Vector<Vector3f> positions = new Vector<Vector3f>();
		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			// skip extra annotations (origin, nose)
			// use annotations list?
			if ( !text.getText().equalsIgnoreCase("origin") )
			{
				Vector3f pos = new Vector3f(text.elementAt(0));
				pos.scale( VOILatticeManagerInterface.VoxelSize );
				positions.add( pos );
			}
		}
		Vector<Vector3f> nosePts = new Vector<Vector3f>();
		if ( nose != null )
		{
			for ( int i = 0; i < nose.getCurves().size(); i++ )
			{
				VOIText text = (VOIText)nose.getCurves().elementAt(i);
				nosePts.add(new Vector3f(text.elementAt(0)));
				nosePts.lastElement().scale( VOILatticeManagerInterface.VoxelSize );
			}
		}

		// look for potential 10 pair:
		float maxDist = -1;
		int count = 0;
		float tempMin = tenMinDist;
		float tempMax = tenMaxDist;
		while ( (count == 0) && (tempMin > 0.5) )
		{
			count = 0;
			for ( int i = 0; i < positions.size(); i++ )
			{
				for ( int j = i + 1; j < positions.size(); j++ )
				{
					float distance = positions.elementAt(i).distance( positions.elementAt(j) );
					if ( distance > maxDist )
					{
						maxDist = distance;
					}
					if (  (distance > tempMin) && (distance < tempMax) )
					{
						tenthPairs.add( new int[]{i,j} );
						count++;
					}
				}
			}
			if ( count == 0 )
			{
				tempMin -= 0.1;
				tempMax += 0.1;
			}
		}

		// Given a set of potential tenth pairs, remove any that fail the mid-point test:
		System.err.println( tenthPairs.size() + " " + tempMin + " " + tempMax );
		if ( tenthPairs.size() > 1 )
		{
			for ( int i = tenthPairs.size() - 1; i >= 0; i-- )
			{
				if ( midPointFail( tenthPairs.elementAt(i), positions ) )
				{
					tenthPairs.remove(i);
				}
			}
		}
//		System.err.println( tenthPairs.size() );
		if ( batchProgress != null )
		{
			batchProgress.setValue((int)(100 * (float)(imageCount*numSteps + step++)/(numSteps*numImages)));
			batchProgress.update(batchProgress.getGraphics());
		}
		
		LatticeModel model = new LatticeModel( image );
		int[] total = new int[]{0};
		int[] max = new int[]{0};
		Vector<Vector<int[]>> sequenceList = new Vector<Vector<int[]>>();
		
		// Loop over all potential tenthPairs:
		for ( int i = 0; i < tenthPairs.size(); i++ )
		{
			int minMaxCount = 0;
			int[] tenthPair = tenthPairs.elementAt(i);
			int[][] pairs = new int[positions.size()][positions.size()];
			// Find potential pairs in the remaining set of points:
			for ( int j = 0; j < positions.size(); j++ )
			{
				int countJ = 0;
				for ( int k = 0; k < positions.size(); k++ )
				{
					pairs[j][k] = -1;
					if ( (j != tenthPair[0]) && (j != tenthPair[1]) )
					{
						if ( (k != j) && (k != tenthPair[0]) && (k != tenthPair[1]) )
						{
							float distance = positions.elementAt(j).distance( positions.elementAt(k) );
							// pairs must be within a minimum and maximum distance thresholds:
							if (  (distance >= minPairDist) && (distance <= maxPairDist) )
							{
								minMaxCount++;
								// only add pairs that pass the mid-point test:
								if ( !midPointFail(j,k, positions) )
								{
									pairs[j][k] = 1;
									countJ++;
								}
							}
						}
					}
				}
				if ( (j != tenthPair[0]) && (j != tenthPair[1]) )
				{
					if ( countJ == 0 )
					{
						if ( print ) System.err.println( "   No pairs found for " + j );
					}
				}
			}
			if ( print ) System.err.println( "   Initial Pairset " + countPairs(pairs, tenthPair) );

			Vector<int[]> pairLists = new Vector<int[]>();
			for ( int j = 0; j < pairs.length; j++ )
			{
				for ( int k = j+1; k < pairs[j].length; k++ )
				{
					if ( pairs[j][k] != -1 )
					{
						pairLists.add( new int[]{j,k} );
					}
				}
			}

//			System.err.println( pairLists.size() + " " + minMaxCount + " " + positions.size() );
			pairLists.add( tenthPair );

			// This step looks for seam cell pairs
			// where one member of the pair is only in
			// one potential pair. When that's the case
			// the pair is essential to the list
			// this step removes all other potential pairs
			// that link to the non-singleton member of the
			// essential pair:
			int prevSize = pairLists.size();
			if ( checkPairs( pairLists, positions.size() ) )
			{
				while ( prevSize > pairLists.size() )
				{
					prevSize = pairLists.size();
					checkPairs( pairLists, positions.size() );
				}
			}
			checkPairs( pairLists, positions.size() );
//			System.err.println( pairLists.size() );
			
			Vector<int[]> sequence = new Vector<int[]>();
			int targetLength = (positions.size() % 2) == 0 ? positions.size() : positions.size() -1;
			targetLength = Math.max(0, targetLength);
			targetLength = Math.min(20, targetLength);
			// Starting at the 10th pair, add pairs to the sequence building it up until the sequence contains 10 pairs:
			sequencePairs( startTime, nosePts, positions, sequence, pairLists, tenthPair, targetLength, total, max, sequenceList );
		}
		if ( batchProgress != null )
		{
			batchProgress.setValue((int)(100 * (float)(imageCount*numSteps + step++)/(numSteps*numImages)));
			batchProgress.update(batchProgress.getGraphics());
		}
//		System.err.println( "buildLattice time 1 = " + AlgorithmBase.computeElapsedTime(startTime) + " " + sequenceList.size() );
		startTime = System.currentTimeMillis();
//		System.err.println( sequenceList.size() + " " + max[0] );
		VOIVector finalLatticeList = new VOIVector();
		// Order the sequences based on how well they fit the lattice parameters:
		orderSequences( startTime, image, model, nosePts, positions, sequenceList, finalLatticeList );
//		System.err.println( "buildLattice time 2 = " + AlgorithmBase.computeElapsedTime(startTime) );

		if ( batchProgress != null )
		{
			batchProgress.setValue((int)(100 * (float)(imageCount*numSteps + step++)/(numSteps*numImages)));
			batchProgress.update(batchProgress.getGraphics());
		}
//		System.err.println( finalLatticeList.size() );
		// Save the top 5 lattices found for the user to select the best:
		for ( int j = 0; j < Math.min( 5, finalLatticeList.size()); j++ )
//			for ( int j = 0; j < finalLatticeList.size(); j++ )
		{
			image.unregisterAllVOIs();
			VOI lattice = finalLatticeList.elementAt(j);
			image.registerVOI(lattice);
			String fileName = baseFileDir + File.separator + baseFileName + "_"  + time + File.separator + autoLatticeGenerationOutput + (j+1) + File.separator;
			File outputFileDir = new File(fileName);
			if ( !outputFileDir.exists() )
			{
				outputFileDir.mkdir();
			}

			LatticeModel.saveAllVOIsTo(fileName, image);
		}
		
		model.dispose();
		model = null;
		return (finalLatticeList.size() > 0);
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
						int index = zSlice[0]*dimX*dimY + y * dimX + x;
						for ( int c = 0; c < 4; c++ )
						{
							maximumProjectionImage.set(index * 4 + c, mp.getC(index, c) );
						}
					}
					else
					{
						maximumProjectionImage.set(x, y, zSlice[0], mp.get(x, y, 0) );
					}
				}
			}

			zSlice[0]++;
		}
		mipAlgo.finalize();
	}


	/**
	 * @param pairs  list of potential seam cell pairs
	 * @param size   the total number of seam cells
	 * @return
	 */
	private static boolean checkPairs( Vector<int[]> pairs, int size )
	{
		boolean allFound = true;
		// if a positions only occurs in one pair, remove all other occurrences of its partner:
		int[] counts = new int[size];
		for ( int i = 0; i < size; i++ )
		{
			counts[i] = 0;
			for ( int j = 0; j < pairs.size(); j++ )
			{
				int[] pair = pairs.elementAt(j);
				if ( pair[0] == i || pair[1] == i )
				{
					counts[i]++;
				}
			}
			if ( counts[i] == 0 )
			{
				// seam cell is not in any pair:
				allFound = false;
			}
		}
		
		// Create the essential pair list of seam cell pairs where one of the
		// seam cells in the pair occurs in only one pair:
		Vector<int[]> essentialPairs = new Vector<int[]>();
		for ( int i = 0; i < size; i++ )
		{
			if ( counts[i] == 1 )
			{
				// found an essential pair:
				for ( int j = pairs.size() - 1; j >= 0; j-- )
				{
					int[] pair = pairs.elementAt(j);
					// remove essential pair from the pairs list and add
					// to essential list:
					if ( pair[0] == i || pair[1] == i )
					{
						essentialPairs.add(pairs.remove(j));
					}
				}				
			}
		}
		
		// When a seam cell is part of an essential pair both
		// seam cells are limited to that pair only
		// this removes pairs that contain the partner of the
		// singleton seam cell:
		for ( int i = 0; i < essentialPairs.size(); i++ )
		{
			// get the essential pair:
			int[] essentialPair = essentialPairs.elementAt(i);
			// look through remaining non-essential pairs:
			for ( int j = pairs.size() - 1; j >= 0; j-- )
			{
				int[] pair = pairs.elementAt(j);
				// if the non-essential pair contains the partner to the
				// singleton of the essential pair remove it:
				if ( pair[0] == essentialPair[0] || pair[1] == essentialPair[0] ||
					 pair[0] == essentialPair[1] || pair[1] == essentialPair[1] )
				{
					pairs.remove(j);
				}
			}				
		}

		// Add all essential pairs back in:
		pairs.addAll( essentialPairs );
		return allFound;
	}
	
	/**
	 * Checks the table with seam-cells pairs to make sure all seam cells are included in at least one pair:
	 * @param pairs
	 * @param tenthPair
	 * @return
	 */
	private static boolean countPairs( final int[][] pairs, final int[] tenthPair )
	{
		boolean allFound = true;
		for ( int i = 0; i < pairs.length; i++ )
		{
			int count = 0;
			for ( int j = 0; j < pairs[i].length; j++ )
			{
				if ( pairs[i][j] != -1 )
				{
					count++;
				}
			}
			if ( (i != tenthPair[0]) && (i != tenthPair[1]) )
			{
				if ( count == 0 )
				{
					allFound = false;
				}
			}
		}
		return allFound;
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
	 * Generates a measure of the amount of self-intersection for the given lattice:
	 * @param image input image for untwisting
	 * @param left left-hand side of the lattice:
	 * @param right right-hand side of the lattice:
	 * @return the intersection measure.
	 */
	private static int intersectionCount( ModelImage image, Vector<Vector3f> left, Vector<Vector3f> right )
	{
		// The amount of intersection is approximated using bounding boxes for each sequential pair
		// of points on the lattice. For a lattice with 10 pairs of points there are 9 bounding boxes:
		int intersectionCount = 0;
		Box3f[] boxes = new Box3f[left.size()-1];
		Vector<Vector3f> vertices = new Vector<Vector3f>();
		// Generate the set of bounding boxes for the lattice, around each sequential pair of
		// points on the lattice:
		for ( int i = 0; i < left.size()-1; i++ )
		{
			vertices.add( left.elementAt(i) );
			vertices.add( right.elementAt(i) );
			vertices.add( left.elementAt(i+1) );
			vertices.add( right.elementAt(i+1) );
			Vector3f mid0 = Vector3f.add( left.elementAt(i), right.elementAt(i) );
			mid0.scale(0.5f);
			Vector3f mid1 = Vector3f.add( left.elementAt(i+1), right.elementAt(i+1) );
			mid1.scale(0.5f);
			
			Vector3f horizontal = Vector3f.sub( right.elementAt(i), left.elementAt(i) );
			Vector3f length = Vector3f.sub( mid1, mid0 );
			horizontal.normalize();
			length.normalize();
			Vector3f up = Vector3f.cross(horizontal, length);
			up.normalize();
			float width = left.elementAt(i).distance(right.elementAt(i) );
			width /= 4f;
			vertices.add( Vector3f.scaleAddS(width, up, left.elementAt(i)) );
			vertices.add( Vector3f.scaleAddS(-width, up, left.elementAt(i)) );
			vertices.add( Vector3f.scaleAddS(width, up, right.elementAt(i)) );
			vertices.add( Vector3f.scaleAddS(-width, up, right.elementAt(i)) );

			
			horizontal = Vector3f.sub( right.elementAt(i+1), left.elementAt(i+1) );
			horizontal.normalize();
			up = Vector3f.cross(horizontal, length);
			up.normalize();
			width = left.elementAt(i+1).distance(right.elementAt(i+1) );
			width /= 4f;
			vertices.add( Vector3f.scaleAddS(width, up, left.elementAt(i+1)) );
			vertices.add( Vector3f.scaleAddS(-width, up, left.elementAt(i+1)) );
			vertices.add( Vector3f.scaleAddS(width, up, right.elementAt(i+1)) );
			vertices.add( Vector3f.scaleAddS(-width, up, right.elementAt(i+1)) );
			
			boxes[i] = ContBox3f.ContOrientedBox( vertices.size(), vertices );
			vertices.clear();
		}
		
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 0;  
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 0;  
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 0;
		Vector3f pt = new Vector3f();
		// Test the amount the bounding boxes overlap:
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					pt.set(x, y, z);
					int overlapCount = 0;
					for ( int b = 0; b < boxes.length; b++ )
					{
						if ( ContBox3f.InBox( pt, boxes[b]) )
						{
							overlapCount++;
						}
					}
					if ( overlapCount > 1 )
					{
						intersectionCount++;
					}
				}
			}
		}
		
		// return the intersection total:
		return intersectionCount;
	}
	
	/**
	 * Generate the VOI representing the lattice with the given sequence and set of positions.
	 * The nose is included if present.
	 * @param positions  3D positions of the unordered seam cells
	 * @param nose nose position
	 * @param index ID for the VOI
	 * @param sequence list of pair index-ids that define the sequence of pairs in the lattice
	 * @return VOI with two contours, one containing the left-edge of the lattice, the other containing the right-edge of the lattice.
	 */
	private static VOI makeLattice( Vector<Vector3f> positions, Vector<Vector3f> nose, int index, Vector<int[]> sequence )
	{
		VOIContour left = new VOIContour( false );
		VOIContour right = new VOIContour( false );
		VOI lattice = new VOI((short) index, "lattice", 1, VOI.POLYLINE );
		lattice.getCurves().add(left);
		lattice.getCurves().add(right);
		for ( int i = 0; i < sequence.size(); i++ )
		{
			int[] pair = sequence.elementAt(i);
			Vector3f p1 = new Vector3f(positions.elementAt(pair[0]));
			Vector3f p2 = new Vector3f(positions.elementAt(pair[1]));
			p1.scale(1f/VOILatticeManagerInterface.VoxelSize);
			p2.scale(1f/VOILatticeManagerInterface.VoxelSize);
			left.add(0, p1);
			right.add(0, p2);
		}
		if ( nose != null )
		{
			if ( nose.size() == 1 )
			{
				Vector3f nosePt = new Vector3f(nose.elementAt(0));
				nosePt.scale(1f/VOILatticeManagerInterface.VoxelSize);
				left.add(0, nosePt);
				right.add(0, nosePt);
			}
			else if ( nose.size() >= 2 )
			{
				Vector3f nosePt0 = new Vector3f(nose.elementAt(0));
				nosePt0.scale(1f/VOILatticeManagerInterface.VoxelSize);
				Vector3f nosePt1 = new Vector3f(nose.elementAt(1));
				nosePt1.scale(1f/VOILatticeManagerInterface.VoxelSize);

				float distanceL0 = left.elementAt(0).distance(nosePt0);
				float distanceL1 = left.elementAt(0).distance(nosePt1);
				if ( distanceL0 < distanceL1 )
				{
					left.add(0, nosePt0);
					right.add(0, nosePt1);
				}
				else if ( distanceL0 > distanceL1 )
				{
					left.add(0, nosePt1);
					right.add(0, nosePt0);					
				}
				else
				{
					float distanceR0 = right.elementAt(0).distance(nosePt0);
					float distanceR1 = right.elementAt(0).distance(nosePt1);
					if ( distanceR0 < distanceR1 )
					{
						right.add(0, nosePt0);
						left.add(0, nosePt1);
					}
					else
					{
						right.add(0, nosePt1);			
						left.add(0, nosePt0);		
					}					
				}
			}
		}
		return lattice;
	}
	
	/**
	 * Measures the total amount of folding in the lattice.
	 * @param left sequence of points along the left-edge of the lattice.
	 * @param right sequence of points along the right-edge of the lattice.
	 * @return the sum of the 'bend' in the lattice as measured from one mid-point to the next.
	 */
	private static float measureCurvature( Vector<Vector3f> left, Vector<Vector3f> right )
	{
		float bendAngles = 0;
		for ( int i = 0; i < left.size()-2; i++ )
		{
			// Calculate the mid point of the first pair in the sequence:
			Vector3f mid1 = Vector3f.add(left.elementAt(i), right.elementAt(i));
			mid1.scale(0.5f);			

			// Calculate the mid point of the second pair in the sequence:
			Vector3f mid2 = Vector3f.add(left.elementAt(i+1), right.elementAt(i+1));
			mid2.scale(0.5f);

			// Calculate the mid point of the third pair in the sequence:
			Vector3f mid3 = Vector3f.add(left.elementAt(i+2), right.elementAt(i+2));
			mid3.scale(0.5f);
			
			// The bend is calculated as the angle change from midpoint1 to
			// midpoint3 bending around midpoint 2.
			Vector3f vec1 = Vector3f.sub( mid2, mid1 );
			Vector3f vec2 = Vector3f.sub( mid3, mid2 );
			vec1.normalize();
			vec2.normalize();
			float angle1 = Math.abs(vec1.angle(vec2));
			float angle2 = Math.abs(vec2.angle(vec1));
			if ( angle1 < angle2 )
			{
				bendAngles += angle1;
			}
			else
			{
				bendAngles += angle2;
			}
		}
		// total bend (folding) along the sequence
		return bendAngles;
	}
	
	/**
	 * Checks two seam cells to see if they form a potential pair.
	 * The test looks at the mid point between the two seam cells
	 * and if there is another seam cell that is closer to the mid point
	 * than 1/2 the distances between the two seam cells the seam cells can
	 * be ruled out as a potential pair.
	 * @param pair
	 * @param positions
	 * @return
	 */
	private static boolean midPointFail( int pair0, int pair1, Vector<Vector3f> positions )
	{
		// Get the positions of the two seam cells:
		Vector3f p1 = positions.elementAt(pair0);
		Vector3f p2 = positions.elementAt(pair1);
		// calculate the mid point:
		Vector3f midPt = Vector3f.add(p1,p2);
		midPt.scale(0.5f);
		// calculate the distance between the two seam cells and divide by 2:
		float distance = p1.distance(p2);
		distance /= 2f;
		// Loop through all seam cells:
		for ( int i = 0; i < positions.size(); i++ )
		{
			if ( (i != pair0) && (i != pair1) )
			{
				if ( midPt.distance(positions.elementAt(i)) < distance )
				{
					// Fails to pass the mid-point test and 
					// can be ruled out as a potential pair:
					return true;
				}
			}
		}
		// MidPoint Test Pass:
		return false;
	}
	
	/**
	 * Checks two seam cells to see if they form a potential pair.
	 * The test looks at the mid point between the two seam cells
	 * and if there is another seam cell that is closer to the mid point
	 * than 1/2 the distances between the two seam cells the seam cells can
	 * be ruled out as a potential pair.
	 * @param pair
	 * @param positions
	 * @return
	 */
	private static boolean midPointFail( int[] pair, Vector<Vector3f> positions )
	{
		// Get the positions of the two seam cells:
		Vector3f p1 = positions.elementAt(pair[0]);
		Vector3f p2 = positions.elementAt(pair[1]);
		// calculate the mid point:
		Vector3f midPt = Vector3f.add(p1,p2);
		midPt.scale(0.5f);
		// calculate the distance between the two seam cells and divide by 2:
		float distance = p1.distance(p2);
		distance /= 2f;
		// Loop through all seam cells:
		for ( int i = 0; i < positions.size(); i++ )
		{
			if ( (i != pair[0]) && (i != pair[1]) )
			{
				if ( midPt.distance(positions.elementAt(i)) < distance )
				{
					// Fails to pass the mid-point test and 
					// can be ruled out as a potential pair:
					return true;
				}
			}
		}
		// MidPoint Test Pass:
		return false;
	}
	
	/**
	 * Returns an ordering of the input sequences based on measures of which lattice is judged to be a best fit.
	 * The lattice with the smoothest curvature and minimum self-intersections is scored higher than a lattice
	 * with a lot of curvature and high amount of self-intersection.
	 * @param startTime the algorithm total run-time, to limit the length spent optimizing the lattice search.
	 * @param image input worm image
	 * @param model model of the worm, based on the lattice.
	 * @param nose nose positions
	 * @param positions seam cell positions
	 * @param sequenceList list of sequences representing lattices
	 * @param finalLatticeList final list of output lattices.
	 */
	private static void orderSequences( long startTime, ModelImage image, LatticeModel model, Vector<Vector3f> nose, Vector<Vector3f> positions, Vector<Vector<int[]>> sequenceList, VOIVector finalLatticeList )
	{
		if ( sequenceList.size() <= 0 )
		{
			return;
		}
		// Turn the sequence lists into VOI lattices:
		VOIVector lattices = new VOIVector();
		for ( int i = 0; i < sequenceList.size(); i++ )
		{
			lattices.add( makeLattice( positions, nose, i, sequenceList.elementAt(i) ) );
		}
		// measure the curvatures of the lattices:
		Vector2d[] angles = new Vector2d[sequenceList.size()];
		int[] intersections = new int[sequenceList.size()];
		for ( int i = 0; i < sequenceList.size(); i++ )
		{
			VOI lattice = lattices.elementAt(i);
			angles[i] = new Vector2d(measureCurvature(lattice.getCurves().elementAt(0), lattice.getCurves().elementAt(1)), i);
			intersections[i] = -1;
		}
		Arrays.sort(angles);
		Vector3d[] minCount = new Vector3d[sequenceList.size()];
		
		// determine the self-intersections of the lattices:
		for ( int i = 0; i < sequenceList.size(); i++ )
		{
			int index = (int) angles[i].Y;
			VOI lattice = lattices.elementAt(index);
			int intersection = Integer.MAX_VALUE;

			double elapsedTime = AlgorithmBase.computeElapsedTime(startTime);
			if ( elapsedTime < sequenceTimeLimit )
			{
				intersection = intersectionCount( image, lattice.getCurves().elementAt(0), lattice.getCurves().elementAt(1) );
				minCount[i] = new Vector3d(Math.round(angles[i].X), intersection, index);
			}
			else
			{
				minCount[i] = new Vector3d( Double.MAX_VALUE, Double.MAX_VALUE, index);
			}
//			System.err.println( i + " " + Math.round(angles[i].X) + " " + intersection + " " + index );
		}
		Arrays.sort(minCount);
		for ( int i = 0; i < minCount.length; i++ )
		{
			if ( minCount[i].Z != -1 )
			{
				int minIndex = (int) minCount[i].Z;
				finalLatticeList.add(lattices.elementAt(minIndex));
//				VOIContour left = (VOIContour) lattices.elementAt(minIndex).getCurves().elementAt(0);
//				VOIContour right = (VOIContour) lattices.elementAt(minIndex).getCurves().elementAt(1);
//				Vector3f mid0 = Vector3f.add(left.elementAt(0), right.elementAt(0)); mid0.scale(0.5f);
//				Vector3f mid1 = Vector3f.add(left.elementAt(1), right.elementAt(1)); mid1.scale(0.5f);
//				System.err.println( i + " " + minIndex + " " + mid0.distance(mid1) );
			}
		}
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
	
	/**
	 * Generates sequences of pairs of seam cells to build a lattice. 
	 * @param startTime the algorithm total run-time, to limit the length spent optimizing the lattice search.
	 * @param nose user-specified nose positions
	 * @param positions 3D positions of the seam cells (unordered)
	 * @param sequence current sequence to be added to
	 * @param pairs remaining pairs that could be added to the sequence
	 * @param lastPair current pair being added to the sequence
	 * @param count maximum limit on the sequence length
	 * @param total number of sequences that form complete lattices
	 * @param max number of sequences that reach the maximum lattice length (not all pass the tests).
	 * @param sequenceList list of sequences representing potentially acceptable lattices.
	 */
	private static void sequencePairs( long startTime, Vector<Vector3f> nose, Vector<Vector3f> positions, Vector<int[]> sequence, Vector<int[]> pairs, int[] lastPair, int count, int[] total, int[] max, Vector<Vector<int[]>> sequenceList )
	{		
		Vector<int[]> newSequence = new Vector<int[]>();
		newSequence.addAll(sequence);
		newSequence.add(lastPair);
		
		// Test if the current sequence is the correct length:
		boolean allFound = (count == newSequence.size() * 2);
		if ( allFound )
		{
			// current sequence is the desired length make into a lattice:
			max[0]++;
			Vector<Vector3f> left = new Vector<Vector3f>();
			Vector<Vector3f> right = new Vector<Vector3f>();
			for ( int i = 0; i < newSequence.size(); i++ )
			{
				int[] pair = newSequence.elementAt(i);
				left.add(positions.elementAt(pair[0]));
				right.add(positions.elementAt(pair[1]));
			}
			// test the lattice, if it fits add to the list:
			if ( testLattice(left, right, nose) )
			{
				total[0]++;
				sequenceList.add(newSequence);				
			}
			// return, do not add more to this sequence:
			return;
		}
		if ( count <= (newSequence.size() * 2) )
		{
			// new sequence is too long, return failure:
			return;
		}
		double elapsedTime = AlgorithmBase.computeElapsedTime(startTime);
		if ( elapsedTime > sequenceTimeLimit )
		{
			// if too much time has elapsed, return:
			return;
		}
		
		// Add a new 'rung' to the lattice:
		Vector<int[]> newPairs = new Vector<int[]>();
		newPairs.addAll(pairs);
		newPairs.remove(lastPair);
		for ( int i = 0; i < newPairs.size(); i++ )
		{
			int[] pair = newPairs.elementAt(i);
			boolean found = false;
			for ( int j = 0; j < newSequence.size(); j++ )
			{
				int[] pair2 = newSequence.elementAt(j);
				if ( (pair2[0] == pair[0]) || (pair2[0] == pair[1]) || (pair2[1] == pair[0]) || (pair2[1] == pair[1]) )
				{
					found = true;
					break;
				}
			}
			if ( !found )
			{
				// Calculate angles from the last pair in the sequence to the new pair, this determines which side (left or right) to put
				// the points of the new pair on.
				Vector3f edge00 = Vector3f.sub( positions.elementAt(pair[0]), positions.elementAt(lastPair[0]) );  float L00 = edge00.normalize();
				Vector3f edge11 = Vector3f.sub( positions.elementAt(pair[1]), positions.elementAt(lastPair[1]) );  float L11 = edge11.normalize();
				float angle1 = edge00.angle(edge11);

				Vector3f edge01 = Vector3f.sub( positions.elementAt(pair[0]), positions.elementAt(lastPair[1]) );  float L01 = edge01.normalize();
				Vector3f edge10 = Vector3f.sub( positions.elementAt(pair[1]), positions.elementAt(lastPair[0]) );  float L10 = edge10.normalize();
				float angle2 = edge01.angle(edge10);
				float diff = Math.abs(L01-L10);
				if ( (angle1 < angle2) && (angle1 < sequenceTwistLimit) )
				{
					diff = Math.abs(L00-L11);
					if ( diff <= sequenceDistDiffLimit )
					{
						Vector3f midPt = Vector3f.add(positions.elementAt(pair[0]), positions.elementAt(pair[1]));
						midPt.scale(0.5f);
						Vector3f midPtSequence = Vector3f.add(positions.elementAt(lastPair[0]), positions.elementAt(lastPair[1]));
						midPtSequence.scale(0.5f);
						float midDistance = midPt.distance(midPtSequence);
						

						if ( (midDistance > minSequenceMidDist) && (midDistance < maxSequenceMidDist) && (L00 > minSequenceDist) && (L00 < maxSequenceDist) && (L11 > minSequenceDist) && (L11 < maxSequenceDist))
						{
							sequencePairs( startTime, nose, positions, newSequence, newPairs, pair, count, total, max, sequenceList );
						}
					}
				}
				else if ( angle2 < (Math.PI/2f) )
				{
					diff = Math.abs(L01-L10);
					if ( diff <= sequenceDistDiffLimit )
					{
						Vector3f midPt = Vector3f.add(positions.elementAt(pair[0]), positions.elementAt(pair[1]));
						midPt.scale(0.5f);
						Vector3f midPtSequence = Vector3f.add(positions.elementAt(lastPair[0]), positions.elementAt(lastPair[1]));
						midPtSequence.scale(0.5f);
						float midDistance = midPt.distance(midPtSequence);
						

						if ( (midDistance > minSequenceMidDist) && (midDistance < maxSequenceMidDist) && (L01 > minSequenceDist) && (L01 < maxSequenceDist) && (L10 > minSequenceDist) && (L10 < maxSequenceDist) )
						{
							sequencePairs( startTime, nose, positions, newSequence, newPairs, new int[]{pair[1],pair[0]}, count, total, max, sequenceList );
						}
					}
				}
			}
		}

	}


	/**
	 * Checks the lattice based on observations of the embryonic worm and compares the lattice to threshold values.
	 * Return true if the lattice fits within bounds.
	 * @param left the positions of the lattice along the left-edge
	 * @param right the positions of the lattice along the right-edge
	 * @param nose nose point of the worm (may be empty)
	 * @return true if the lattice fits within threshold, false otherwise.
	 */
	private static boolean testLattice( Vector<Vector3f> left, Vector<Vector3f> right, Vector<Vector3f> nose )
	{
		// Left and right sides must contain the same number of points:
		if ( (left.size() != right.size()) || (left.size() == 0) )
		{
			return false;
		}

		if ( nose != null )
		{
			Vector3f mid = Vector3f.add(left.lastElement(), right.lastElement());
			mid.scale(0.5f);
			
			float distance = -1;
			if ( nose.size() == 1 )
			{
				distance = mid.distance(nose.elementAt(0));
			}
			else if ( nose.size() >= 2 )
			{
				Vector3f midNose = Vector3f.add(nose.elementAt(0), nose.elementAt(1));
				midNose.scale(0.5f);
				distance = mid.distance(midNose);
			}
			if ( (distance != -1) && ((distance < noseP1MinDist) || (distance > noseP1MaxDist)) )
			{
//				System.err.println( distance );
				return false;
			}
		}
		
		for ( int i = 0; i < left.size(); i++ )
		{
			Vector3f mid1 = Vector3f.add(left.elementAt(i), right.elementAt(i));
			mid1.scale(0.5f);
			// distance measures the width of the worm at this point in the lattice model:
			float distance = left.elementAt(i).distance(right.elementAt(i));
			distance /= 2;

			// Check distance of sequential mid-points to make
			// sure they are not closer together than the 'width' of the worm
			// at this point:
			for ( int j = i+1; j < left.size(); j++ )
			{
				Vector3f mid2 = Vector3f.add(left.elementAt(j), right.elementAt(j));
				mid2.scale(0.5f);
				
				if ( mid1.distance(mid2) < distance)
				{
					return false;
				}
			}
		}
		
		float length = 0;
		// Check the sequence distances between midpoints
		// the distance expected ranges vary based on the pair number:
		for ( int i = 0; i < left.size() - 1; i++ )
		{
			Vector3f mid1 = Vector3f.add(left.elementAt(i), right.elementAt(i));     mid1.scale(0.5f);
			Vector3f mid2 = Vector3f.add(left.elementAt(i+1), right.elementAt(i+1)); mid2.scale(0.5f);
			float sequenceDistance = mid1.distance(mid2);
			length += sequenceDistance;
			if ( (i <= 3) && ((sequenceDistance < 5) || (sequenceDistance > 26)) )
			{
				return false;
			}
			if ( (i > 3) && (i <= 6) && ((sequenceDistance < 5) || (sequenceDistance > 16)) )
			{
				return false;
			}
			if ( (i == 7) && ((sequenceDistance < 10) || (sequenceDistance > 26)) )
			{
				return false;
			}
			if ( (i == 8) && ((sequenceDistance < 5) || (sequenceDistance > 16)) )
			{
				return false;
			}
		}
		
		// Check total length:
		if ( (length < wormLengthMin) || (length > wormLengthMax) )
		{
			return false;
		}		

		// Check the 'width' of the pairs:
		float maxWidth = -Float.MAX_VALUE;
		int maxIndex = -1;
		float minWidth = Float.MAX_VALUE;
		int minIndex = -1;
		int countFirst = 0;
		int countSecond = 0;
		float avgWidthFirst4 = 0;
		float avgWidthLast5 = 0;
		for ( int i = 0; i < left.size(); i++ )
		{
			float dist = left.elementAt(i).distance( right.elementAt(i) );
			if ( i < 5 )
			{
				avgWidthLast5 += dist;
				countSecond++;
			}
			else
			{
				avgWidthFirst4 += dist;	
				countFirst++;
			}
			if ( dist > maxWidth )
			{
				maxWidth = dist;
				maxIndex = i;
			}			
			if ( dist < minWidth )
			{
				minWidth = dist;
				minIndex = i;
			}				
		}
		avgWidthFirst4 /= countFirst;
		avgWidthLast5 /= countSecond;
		if ( (avgWidthFirst4 > avgWidthLast5) && (maxIndex >= 4) && (minIndex <= 4) )
		{			
			// Check the amount of 'bend' in the worm:
			if ( nose != null )
			{
				if ( nose.size() == 1 )
				{
					Vector3f nosePt = new Vector3f(nose.elementAt(0));
					left.add(nosePt);
					right.add(nosePt);
				}
				else if ( nose.size() >= 2 )
				{
					Vector3f nosePt0 = new Vector3f(nose.elementAt(0));
					Vector3f nosePt1 = new Vector3f(nose.elementAt(1));

					float distanceL0 = left.elementAt(0).distance(nosePt0);
					float distanceL1 = left.elementAt(0).distance(nosePt1);
					if ( distanceL0 < distanceL1 )
					{
						left.add(nosePt0);
						right.add(nosePt1);
					}
					else if ( distanceL0 > distanceL1 )
					{
						left.add(nosePt1);
						right.add(nosePt0);					
					}
					else
					{
						float distanceR0 = right.elementAt(0).distance(nosePt0);
						float distanceR1 = right.elementAt(0).distance(nosePt1);
						if ( distanceR0 < distanceR1 )
						{
							right.add(nosePt0);
							left.add(nosePt1);
						}
						else
						{
							right.add(nosePt1);			
							left.add(nosePt0);		
						}					
					}
				}
			}
			float bendSum = measureCurvature(left, right);
			if ( (bendSum < sequenceBendMin) || (bendSum > sequenceBendMax) )
			{
				return false;
			}
			
			return true;
		}		
		
		return false;
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
