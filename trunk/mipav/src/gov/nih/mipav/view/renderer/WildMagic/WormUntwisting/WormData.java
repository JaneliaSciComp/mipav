package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

import java.io.File;
import java.util.Vector;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

public class WormData
{
	public static float VoxelSize =  0.1625f;

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
	
	private String imageName;
	private ModelImage wormImage;
	private ModelImage nucleiImage;
	private ModelImage nucleiBlur;
	private ModelImage wormBlur;
	private ModelImage seamSegmentation;
	private ModelImage skinSegmentation;
	private ModelImage insideSegmentation;
	private ModelImage nucleiSegmentation;

	private Vector<Vector3f> seamCellPoints;
	private VOI seamAnnotations;
	private VOIVector autoLattice;
	private float minSeamCellSegmentationIntensity = -1;
	
	private String outputDirectory = null;
	private String outputImagesDirectory = null;
	
	public WormData( ModelImage image )
	{
		wormImage = image;

		if ( wormImage != null )
		{
			imageName = wormImage.getImageName();
			if (imageName.contains("_clone")) {
				imageName = imageName.replaceAll("_clone", "");
			}
			outputDirectory = new String(wormImage.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + JDialogBase.makeImageName(imageName, "_results") );
			outputImagesDirectory = outputDirectory + File.separator + "output_images" + File.separator;
			String parentDir = new String(wormImage.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator);
			checkParentDir(parentDir);
			
			float voxelResolution = wormImage.getResolutions(0)[0];
			if ( voxelResolution != VoxelSize )
			{
				VoxelSize = voxelResolution;
			}
			minSeamCellSegmentationIntensity = (float) (0.1 * wormImage.getMin());
//			System.err.println( minSeamCellSegmentationIntensity );
		}
	}
	
	public void dispose()
	{
		if ( nucleiSegmentation != null )
		{
			nucleiSegmentation.disposeLocal(false);
			nucleiSegmentation = null;
		}
		if ( skinSegmentation != null )
		{
			skinSegmentation.disposeLocal(false);
			skinSegmentation = null;
		}
		if ( insideSegmentation != null )
		{
			insideSegmentation.disposeLocal(false);
			insideSegmentation = null;
		}
		if ( seamSegmentation != null )
		{
			seamSegmentation.disposeLocal(false);
			seamSegmentation = null;
		}
		if ( wormBlur != null )
		{
			wormBlur.disposeLocal(false);
			wormBlur = null;
		}
	}

	/**
	 * Creates the parent directory for the output images and data created by worm segmentation and untwisting:
	 * @param parentDir
	 */
	private void checkParentDir( String parentDir )
	{
		File parentFileDir = new File(parentDir);
		if (parentFileDir.exists() && parentFileDir.isDirectory()) { // do nothing
		} else if (parentFileDir.exists() && !parentFileDir.isDirectory()) { // do nothing
		} else { // parentDir does not exist
			parentFileDir.mkdir();
		}
	}
	
	public Vector<Vector3f> readSeamCells()
	{	
		VOIVector annotationVector = new VOIVector();
		LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + autoSeamCellSegmentationOutput + File.separator, true, annotationVector, false);
		if ( annotationVector.size() > 0 )
		{
			seamAnnotations = annotationVector.elementAt(0);
		}
		if ( seamAnnotations == null )
		{
			return null;
		}
		seamCellPoints = new Vector<Vector3f>();
		for ( int i = 0; i < seamAnnotations.getCurves().size(); i++ )
		{
			VOIText text = (VOIText)seamAnnotations.getCurves().elementAt(i);
			seamCellPoints.add(new Vector3f(text.elementAt(0)));
		}
		return seamCellPoints;
	}
	
	public void segmentSeamCells(int minRadius, int maxRadius)
	{
//		System.err.println( "   segmentSeamCells: start" );
		if (seamCellPoints == null)
		{
			String seamCellDir = outputImagesDirectory + File.separator;
			File outputDir = new File(seamCellDir);
			if ( !outputDir.exists() )
			{
				outputDir.mkdir();
			}
			
			if ( wormBlur == null )
			{
				wormBlur = WormSegmentation.blur(wormImage, 3);
			}
			WormSegmentation segmentation = new WormSegmentation();
			seamCellPoints = segmentation.segmentSeamNew(wormBlur, minRadius, maxRadius, outputImagesDirectory);
			minSeamCellSegmentationIntensity = segmentation.getMinSegmentationIntensity();

			File inputFile = new File(seamCellDir + "seamCellImage.xml");
			if ( inputFile.exists() )
			{
				FileIO fileIO = new FileIO();
				seamSegmentation = fileIO.readImage( seamCellDir + "seamCellImage.xml" );
				
//				new ViewJFrameImage((ModelImage)seamSegmentation.clone());
			}
		}
		
		if ( seamAnnotations == null )
		{
			seamAnnotations = new VOI((short) 0, "seam cells", VOI.ANNOTATION, -1.0f);
			wormImage.registerVOI(seamAnnotations);
			for ( int i = 0; i < seamCellPoints.size(); i++ )
			{
				//			System.err.println( i );
				VOIText text = new VOIText();
				text.add(seamCellPoints.elementAt(i));
				text.add(seamCellPoints.elementAt(i));
				text.setText( "" + (i+1) );
				text.setUseMarker(false);
				text.update();

				//			int x = (int)seamCellPoints.elementAt(i).X;
				//			int y = (int)seamCellPoints.elementAt(i).Y;
				//			int z = (int)seamCellPoints.elementAt(i).Z;
				//			System.err.println( "Seam Cell " + (i+1) + "  " + wormImage.getFloat(x,y,z) );
				seamAnnotations.getCurves().add(text);
			}

			LatticeModel.saveAllVOIsTo(outputDirectory + File.separator + autoSeamCellSegmentationOutput + File.separator, wormImage);
		}
//		System.err.println( "   segmentSeamCells: end " + minSeamCellSegmentationIntensity );
	}
	
	public void setNucleiImage(ModelImage image)
	{
		nucleiImage = image;
		if ( nucleiBlur == null )
		{
			nucleiBlur = WormSegmentation.blur(nucleiImage, 3);
		}
		if ( wormBlur != null )
		{
			wormBlur = WormSegmentation.blur(wormImage, 3);
		}

		ModelImage insideOut = new ModelImage(ModelStorageBase.FLOAT, nucleiImage.getExtents(), "insideOut.tif");
		float min = Float.MAX_VALUE;
		float max = -Float.MAX_VALUE;
		for ( int i = 0; i < nucleiImage.getDataSize(); i++ )
		{
			float inside = insideSegmentation.getFloat(i);
			if ( inside > 0 )
			{
				float wormValue = wormBlur.getFloat(i);
				float nucleiValue = nucleiBlur.getFloat(i);
				if ( wormValue > nucleiValue )
				{
					insideOut.set(i, 10);
				}
			}
		}
		insideOut.setMax(10);
		insideOut.setMin(0);
		new ViewJFrameImage(insideOut);
	}
	
	public Vector<Vector3f> getSeamCells()
	{
		return seamCellPoints;
	}
	
	public ModelImage getSeamSegmentation()
	{
		return seamSegmentation;
	}
	
	public ModelImage readSeamSegmentation()
	{
		String seamCellDir = outputImagesDirectory + File.separator;
		File inputFile = new File(seamCellDir + "seamCellImage.xml");
		if ( inputFile.exists() )
		{
			FileIO fileIO = new FileIO();
			seamSegmentation = fileIO.readImage( seamCellDir + "seamCellImage.xml" );
		} 
		return seamSegmentation;
	}
	
	public void segmentSkin()
	{
		if ( skinSegmentation == null )
		{
			if ( wormBlur == null )
			{
				wormBlur = WormSegmentation.blur(wormImage, 3);
			}

//			ModelImage surface = WormSegmentation.outlineA( wormBlur, minSeamCellSegmentationIntensity );
//			ModelImage surface = new ModelImage( ModelStorageBase.ARGB_FLOAT, wormImage.getExtents(), imageName + "_skin" );
//			WormSegmentation.outline2( outputImagesDirectory, wormImage, surface, minSeamCellSegmentationIntensity);
			
			ModelImage[] outsideInside = WormSegmentation.outside(wormBlur, 5);
			skinSegmentation = outsideInside[0];
			insideSegmentation = outsideInside[1];
			
//			skinSegmentation.calcMinMax();
//			new ViewJFrameImage((ModelImage)skinSegmentation.clone());
//			
//			insideSegmentation.calcMinMax();
//			new ViewJFrameImage((ModelImage)insideSegmentation.clone());
			
//			if ( seamCellPoints != null )
//			{
//				// scan skinSegmentation image for nearest skin pt to each seam cell point...
//				int dimX = skinSegmentation.getExtents().length > 0 ? skinSegmentation.getExtents()[0] : 1;
//				int dimY = skinSegmentation.getExtents().length > 1 ? skinSegmentation.getExtents()[1] : 1;
//				int dimZ = skinSegmentation.getExtents().length > 2 ? skinSegmentation.getExtents()[2] : 1;
//
//				Vector<Vector3f> nearest = new Vector<Vector3f>();
//				Vector<Float>  distances = new Vector<Float>();
//				for ( int i = 0; i < seamCellPoints.size(); i++ )
//				{
//					nearest.add(new Vector3f());
//					distances.add( nearest.elementAt(i).distance(seamCellPoints.elementAt(i)));
//				}
//				Vector3f temp = new Vector3f();
//				for ( int z = 0; z < dimZ; z++ )
//				{
//					for ( int y = 0; y < dimY; y++ )
//					{
//						for ( int x = 0; x < dimX; x++ )
//						{
//							if ( skinSegmentation.getFloat(x, y, z ) > 0 )
//							{
//								temp.set(x, y, z);
//								for ( int i = 0; i < seamCellPoints.size(); i++ )
//								{
//									if ( temp.distance(seamCellPoints.elementAt(i)) < distances.elementAt(i) )
//									{
//										nearest.elementAt(i).copy(temp);
//										distances.remove(i);
//										distances.add(i, temp.distance(seamCellPoints.elementAt(i)));
//									}
//								}
//							}
//						}
//					}
//				}
//				for ( int i = 0; i < seamCellPoints.size(); i++ )
//				{
////					System.err.println( i + " " + distances.elementAt(i) );
////					if ( distances.elementAt(i) < 30 )
////					{
//						seamAnnotations.getCurves().elementAt(i).remove(1);
//						seamAnnotations.getCurves().elementAt(i).add(nearest.elementAt(i));
//						((VOIText)seamAnnotations.getCurves().elementAt(i)).setUseMarker(true);
//						seamAnnotations.getCurves().elementAt(i).update();
////					}
//				}
//			}
		}
	}
	
	public void testLattice()
	{
		LatticeBuilder builder = new LatticeBuilder();
		builder.setSeamImage(seamSegmentation);
		builder.setSkinImage(skinSegmentation);

		//40:
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(seamCellPoints.elementAt(16));
//		left.add(seamCellPoints.elementAt(8));
//		left.add(seamCellPoints.elementAt(1));
//		left.add(seamCellPoints.elementAt(2));
//		left.add(seamCellPoints.elementAt(9));
//		left.add(seamCellPoints.elementAt(12));
//		left.add(seamCellPoints.elementAt(20));
//		left.add(seamCellPoints.elementAt(23));
//		left.add(seamCellPoints.elementAt(17));
//		left.add(seamCellPoints.elementAt(11));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(seamCellPoints.elementAt(14));
//		right.add(seamCellPoints.elementAt(6));
//		right.add(seamCellPoints.elementAt(0));
//		right.add(seamCellPoints.elementAt(3));
//		right.add(seamCellPoints.elementAt(10));
//		right.add(seamCellPoints.elementAt(13));
//		right.add(seamCellPoints.elementAt(19));
//		right.add(seamCellPoints.elementAt(22));
//		right.add(seamCellPoints.elementAt(15));
//		right.add(seamCellPoints.elementAt(7));
		

//		//41:
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(seamCellPoints.elementAt(5));
//		left.add(seamCellPoints.elementAt(13));
//		left.add(seamCellPoints.elementAt(17));
//		left.add(seamCellPoints.elementAt(11));
//		left.add(seamCellPoints.elementAt(6));
//		left.add(seamCellPoints.elementAt(1));
//		left.add(seamCellPoints.elementAt(3));
//		left.add(seamCellPoints.elementAt(8));
//		left.add(seamCellPoints.elementAt(15));
//		left.add(seamCellPoints.elementAt(20));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(seamCellPoints.elementAt(7));
//		right.add(seamCellPoints.elementAt(12));
//		right.add(seamCellPoints.elementAt(14));
//		right.add(seamCellPoints.elementAt(10));
//		right.add(seamCellPoints.elementAt(4));
//		right.add(seamCellPoints.elementAt(0));
//		right.add(seamCellPoints.elementAt(2));
//		right.add(seamCellPoints.elementAt(9));
//		right.add(seamCellPoints.elementAt(16));
//		right.add(seamCellPoints.elementAt(18));

		
		//55:
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(seamCellPoints.elementAt(2));
//		left.add(seamCellPoints.elementAt(4));
//		left.add(seamCellPoints.elementAt(16));
//		left.add(seamCellPoints.elementAt(18));
//		left.add(seamCellPoints.elementAt(19));
//		left.add(seamCellPoints.elementAt(17));
//		left.add(seamCellPoints.elementAt(15));
//		left.add(seamCellPoints.elementAt(13));
//		left.add(seamCellPoints.elementAt(8));
//		left.add(seamCellPoints.elementAt(11));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(seamCellPoints.elementAt(5));
//		right.add(seamCellPoints.elementAt(6));
//		right.add(seamCellPoints.elementAt(12));
//		right.add(seamCellPoints.elementAt(10));
//		right.add(seamCellPoints.elementAt(9));
//		right.add(seamCellPoints.elementAt(7));
//		right.add(seamCellPoints.elementAt(3));
//		right.add(seamCellPoints.elementAt(0));
//		right.add(seamCellPoints.elementAt(1));
//		right.add(seamCellPoints.elementAt(14));

//		
//		//60:
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(seamCellPoints.elementAt(20));
//		left.add(seamCellPoints.elementAt(8));
//		left.add(seamCellPoints.elementAt(14));
//		left.add(seamCellPoints.elementAt(13));
//		left.add(seamCellPoints.elementAt(6));
//		left.add(seamCellPoints.elementAt(3));
//		left.add(seamCellPoints.elementAt(0));
//		left.add(seamCellPoints.elementAt(2));
//		left.add(seamCellPoints.elementAt(12));
//		left.add(seamCellPoints.elementAt(17));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(seamCellPoints.elementAt(9));
//		right.add(seamCellPoints.elementAt(11));
//		right.add(seamCellPoints.elementAt(16));
//		right.add(seamCellPoints.elementAt(15));
//		right.add(seamCellPoints.elementAt(7));
//		right.add(seamCellPoints.elementAt(5));
//		right.add(seamCellPoints.elementAt(1));
//		right.add(seamCellPoints.elementAt(4));
//		right.add(seamCellPoints.elementAt(10));
//		right.add(seamCellPoints.elementAt(18));
		
//		
//
//		//80:
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(seamCellPoints.elementAt(15));
//		left.add(seamCellPoints.elementAt(7));
//		left.add(seamCellPoints.elementAt(2));
//		left.add(seamCellPoints.elementAt(8));
//		left.add(seamCellPoints.elementAt(12));
//		left.add(seamCellPoints.elementAt(17));
//		left.add(seamCellPoints.elementAt(18));
//		left.add(seamCellPoints.elementAt(10));
//		left.add(seamCellPoints.elementAt(5));
//		left.add(seamCellPoints.elementAt(1));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(seamCellPoints.elementAt(14));
//		right.add(seamCellPoints.elementAt(9));
//		right.add(seamCellPoints.elementAt(3));
//		right.add(seamCellPoints.elementAt(6));
//		right.add(seamCellPoints.elementAt(11));
//		right.add(seamCellPoints.elementAt(16));
//		right.add(seamCellPoints.elementAt(19));
//		right.add(seamCellPoints.elementAt(13));
//		right.add(seamCellPoints.elementAt(4));
//		right.add(seamCellPoints.elementAt(0));
		


//		//95:
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(seamCellPoints.elementAt(5));
//		left.add(seamCellPoints.elementAt(12));
//		left.add(seamCellPoints.elementAt(15));
//		left.add(seamCellPoints.elementAt(14));
//		left.add(seamCellPoints.elementAt(8));
//		left.add(seamCellPoints.elementAt(7));
//		left.add(seamCellPoints.elementAt(0));
//		left.add(seamCellPoints.elementAt(2));
//		left.add(seamCellPoints.elementAt(13));
//		left.add(seamCellPoints.elementAt(18));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(seamCellPoints.elementAt(3));
//		right.add(seamCellPoints.elementAt(10));
//		right.add(seamCellPoints.elementAt(16));
//		right.add(seamCellPoints.elementAt(19));
//		right.add(seamCellPoints.elementAt(9));
//		right.add(seamCellPoints.elementAt(6));
//		right.add(seamCellPoints.elementAt(1));
//		right.add(seamCellPoints.elementAt(4));
//		right.add(seamCellPoints.elementAt(11));
//		right.add(seamCellPoints.elementAt(17));

//
//
//		//120:
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(seamCellPoints.elementAt(21));
//		left.add(seamCellPoints.elementAt(15));
//		left.add(seamCellPoints.elementAt(10));
//		left.add(seamCellPoints.elementAt(16));
//		left.add(seamCellPoints.elementAt(20));
//		left.add(seamCellPoints.elementAt(18));
//		left.add(seamCellPoints.elementAt(9));
//		left.add(seamCellPoints.elementAt(0));
//		left.add(seamCellPoints.elementAt(2));
//		left.add(seamCellPoints.elementAt(8));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(seamCellPoints.elementAt(23));
//		right.add(seamCellPoints.elementAt(13));
//		right.add(seamCellPoints.elementAt(5));
//		right.add(seamCellPoints.elementAt(17));
//		right.add(seamCellPoints.elementAt(22));
//		right.add(seamCellPoints.elementAt(19));
//		right.add(seamCellPoints.elementAt(12));
//		right.add(seamCellPoints.elementAt(1));
//		right.add(seamCellPoints.elementAt(3));
//		right.add(seamCellPoints.elementAt(11));

		//120:
		Vector<Vector3f> left = new Vector<Vector3f>();
		left.add(seamCellPoints.elementAt(21));
		left.add(seamCellPoints.elementAt(15));
		left.add(seamCellPoints.elementAt(10));
		left.add(seamCellPoints.elementAt(16));
		left.add(seamCellPoints.elementAt(20));
		left.add(seamCellPoints.elementAt(18));
		left.add(seamCellPoints.elementAt(9));
		left.add(seamCellPoints.elementAt(0));
		left.add(seamCellPoints.elementAt(2));
		left.add(seamCellPoints.elementAt(8));
		
		Vector<Vector3f> right = new Vector<Vector3f>();
		right.add(seamCellPoints.elementAt(23));
		right.add(seamCellPoints.elementAt(13));
		right.add(seamCellPoints.elementAt(5));
		right.add(seamCellPoints.elementAt(17));
		right.add(seamCellPoints.elementAt(22));
		right.add(seamCellPoints.elementAt(19));
		right.add(seamCellPoints.elementAt(12));
		right.add(seamCellPoints.elementAt(1));
		right.add(seamCellPoints.elementAt(3));
		right.add(seamCellPoints.elementAt(11));
		
		
		
		System.err.println( "lattice test " + builder.testLattice(wormImage, left, right) );
	}
	
	public void generateLattice()
	{
		LatticeBuilder builder = new LatticeBuilder();
		builder.setSeamImage(seamSegmentation);
		builder.setSkinImage(skinSegmentation);
		// build the lattices from input image and list of points:
		autoLattice = builder.buildLattice( null, 0, 0, wormImage, seamAnnotations, null, outputDirectory );
	}
	
	public VOIVector getAutoLattice()
	{
		return autoLattice;
	}

	private float surfaceCount( ModelImage skinImage, Vector3f pt1, Vector3f pt2 )
	{
		Vector3f dir = Vector3f.sub(pt2, pt1);
		float length = dir.normalize();
		
		Vector3f center = Vector3f.add(pt1, pt2);
		center.scale(0.5f);

		Vector3f basisVectorX = new Vector3f(1, 0, 0);
		Vector3f basisVectorY = new Vector3f(0, 1, 0);
		Vector3f basisVectorZ = new Vector3f(0, 0, 1);
		Vector3f rotationAxis = Vector3f.cross(basisVectorZ, dir);
		rotationAxis.normalize();
		float angle = basisVectorZ.angle(dir);
		Matrix3f mat = new Matrix3f(true);
		mat.fromAxisAngle(rotationAxis, angle);

		Vector3f rotatedX = mat.multRight(basisVectorX);
		Vector3f rotatedY = mat.multRight(basisVectorY);

		Box3f box = new Box3f( center, rotatedX, rotatedY, dir, 10, 10, length );

		int dimX = skinSegmentation.getExtents().length > 0 ? skinSegmentation.getExtents()[0] : 1;
		int dimY = skinSegmentation.getExtents().length > 1 ? skinSegmentation.getExtents()[1] : 1;
		int dimZ = skinSegmentation.getExtents().length > 2 ? skinSegmentation.getExtents()[2] : 1;
		float totalCount = 0;
		float skinCount = 0;
		Vector3f pt = new Vector3f();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					pt.set(x, y, z);
					if ( ContBox3f.InBox( pt, box ) )
					{
						totalCount++;
						if ( skinSegmentation.getFloat(x, y, z ) > 0 )
						{
							skinCount++;
						}
					}
				}
			}
		}
		return (skinCount/totalCount);
	}
}
