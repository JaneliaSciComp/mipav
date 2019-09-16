package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.Color;
import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Vector;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

public class WormData
{
	public static float VoxelSize =  0.1625f;

	public static final String autoSeamCellSegmentationOutput = new String("seam_cells");
	public static final String editSeamCellOutput = new String("seam_cell_final");
	public static final String namedSeamCellOutput = new String("named_seam_cells");
	public static final String autoLatticeGenerationOutput = new String("lattice_");
	public static final String editLatticeOutput = new String("lattice_final");
	public static final String editAnnotationInput = new String("annotation");
	public static final String editAnnotationOutput = new String("annotation_final");
	public static final String integratedAnnotationOutput = new String("integrated_annotation");
	public static final String outputImages = new String("output_images");
	public static final String straightenedLattice = new String("straightened_lattice");
	public static final String straightenedAnnotations = new String("straightened_annotations");
	public static final String straightenedSeamCells = new String("straightened_seamcells");
	public static final String straightenedNamedSeamCells = new String("straightened_named_seamcells");
	public static final String neuriteOutput = new String("neurite_final");


	private String imageName;
	private ModelImage wormImage;
	private ModelImage nucleiImage;
	private ModelImage seamSegmentation;
	private ModelImage skinSegmentation;
	private ModelImage insideSegmentation;
	private ModelImage nucleiSegmentation;

	private Vector<Vector3f> seamCellPoints;
	private VOI seamAnnotations;
	private VOIVector autoLattice;

	private String outputDirectory = null;
	private String outputImagesDirectory = null;

	private int minSeamRadius = 8;
	private int maxSeamRadius = 25;


	private boolean seamEdited = false;

	public static String getOutputDirectory( File imageFile, String imageName ) {
		return new String(imageFile.getParent() + File.separator + JDialogBase.makeImageName(imageName, "") + File.separator + JDialogBase.makeImageName(imageName, "_results") );
	}

	public static String getOutputDirectory( ModelImage image ) {

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		boolean isStraight = false;
		if (imageName.contains("_straight")) {
			imageName = imageName.replaceAll("_straight", "");
			isStraight = true;
		}
		String outputDirectory = new String(image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + JDialogBase.makeImageName(imageName, "_results") );
		String outputImagesDirectory = outputDirectory + File.separator + "output_images" + File.separator;
		if ( isStraight )
		{
			outputImagesDirectory = image.getImageDirectory();
			outputDirectory = outputImagesDirectory.replaceAll( "output_images", "");
			outputDirectory = outputDirectory.substring(0, outputDirectory.length() - 2);
		}
		return outputDirectory;
	}

	public WormData( ModelImage image )
	{
		wormImage = image;

		if ( wormImage != null )
		{
			imageName = wormImage.getImageName();
			if (imageName.contains("_clone")) {
				imageName = imageName.replaceAll("_clone", "");
			}
			boolean isStraight = false;
			if (imageName.contains("_straight")) {
				imageName = imageName.replaceAll("_straight", "");
				isStraight = true;
			}
			outputDirectory = new String(wormImage.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + JDialogBase.makeImageName(imageName, "_results") );
			outputImagesDirectory = outputDirectory + File.separator + "output_images" + File.separator;
			if ( isStraight )
			{
				outputImagesDirectory = wormImage.getImageDirectory();
				outputDirectory = outputImagesDirectory.replaceAll( "output_images", "");
				outputDirectory = outputDirectory.substring(0, outputDirectory.length() - 2);
			}
			String parentDir = new String(wormImage.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator);
			if ( !isStraight )
			{
				checkParentDir(parentDir);
			}
			File dir = new File(outputDirectory);
			if ( !dir.exists() )
			{
				//				System.err.println( "WormData " + outputDirectory);
				dir.mkdir();
			}
			dir = new File(outputImagesDirectory);
			if ( !dir.exists() )
			{
				//				System.err.println( "WormData " + outputImagesDirectory);
				dir.mkdir();
			}

			float voxelResolution = wormImage.getResolutions(0)[0];
			//			if ( voxelResolution != VoxelSize )
			//			{
			//				VoxelSize = voxelResolution;
			//			}
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
	}

	public int generateLattice()
	{
		if ( seamSegmentation == null )
		{
			readSeamSegmentation();
			if ( seamSegmentation == null )
			{
				segmentSeamCells(minSeamRadius, maxSeamRadius);
			}
		}
		if ( seamAnnotations == null )
		{
			readSeamCells();
			if ( seamAnnotations == null )
			{
				segmentSeamCells(minSeamRadius, maxSeamRadius);
			}
		}
		if ( skinSegmentation == null )
		{
			readSkinSegmentation();
			if ( skinSegmentation == null )
			{
				segmentSkin();
			}
		}
		LatticeBuilder builder = new LatticeBuilder();
		builder.setSeamImage(seamSegmentation);
		builder.setSkinImage(skinSegmentation);
		// build the lattices from input image and list of points:
		autoLattice = builder.buildLattice( null, 0, 0, wormImage, seamAnnotations, null, outputDirectory, seamEdited );
		builder.dispose();
		builder = null;
		return autoLattice.size();
	}

	public VOIVector getAutoLattice()
	{
		return autoLattice;
	}

	public boolean integratedExists() {
		File file = new File(outputDirectory + File.separator + integratedAnnotationOutput + File.separator + "annotations.csv");
		return file.exists();
	}
	
	public VOI getIntegratedMarkerAnnotations()
	{
		System.err.println("getIntegratedMarkerAnnotations");
		VOI markerAnnotations = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + integratedAnnotationOutput + File.separator + "annotations.csv");

		if ( markerAnnotations == null )
			markerAnnotations = new VOI( (short)0, "markers", VOI.ANNOTATION, 0 );
		return markerAnnotations;
	}

	public VOI getMarkerAnnotations()
	{
		System.err.println("getMarkerAnnotations");
		VOI markerAnnotations = readMarkers();
		if ( markerAnnotations == null )
			markerAnnotations = new VOI( (short)0, "markers", VOI.ANNOTATION, 0 );
		return markerAnnotations;
	}

	public VOI getMarkerAnnotations(String outputDir)
	{
		System.err.println("getMarkerAnnotations " + outputDir);
		VOI markerAnnotations = readMarkers(outputDir);
		if ( markerAnnotations == null )
			markerAnnotations = new VOI( (short)0, "markers", VOI.ANNOTATION, 0 );
		return markerAnnotations;
	}

	public String getOutputDirectory()
	{
		return outputDirectory;
	}

	public VOI getSeamAnnotations()
	{
		if ( seamAnnotations == null )
			seamAnnotations = new VOI( (short)0, "seam cells", VOI.ANNOTATION, 0 );
		return seamAnnotations;
	}

	public Vector<Vector3f> getSeamCells()
	{
		return seamCellPoints;
	}

	public ModelImage getSeamSegmentation()
	{
		return seamSegmentation;
	}

	public void openStraightAnnotations()
	{
		//		VOIVector annotationVector = new VOIVector();
		//		LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + straightenedAnnotations + File.separator, true, annotationVector, true);	
		//		if ( annotationVector.size() > 0 )
		//		{
		//			return annotationVector.elementAt(0);
		//		}
		//		return null;

		VOI voi = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + straightenedAnnotations + File.separator + "straightened_annotations.csv");
		if ( voi != null ) wormImage.registerVOI(voi);
	}

	public void openStraightAnnotations( String dir )
	{
		//		VOIVector annotationVector = new VOIVector();
		//		LatticeModel.loadAllVOIsFrom(wormImage, dir + File.separator + straightenedAnnotations + File.separator, true, annotationVector, true);	
		//		if ( annotationVector.size() > 0 )
		//		{
		//			return annotationVector.elementAt(0);
		//		}
		//		return null;


		VOI voi = LatticeModel.readAnnotationsCSV(dir + File.separator + straightenedAnnotations + File.separator + "straightened_annotations.csv");
		if ( voi != null ) wormImage.registerVOI(voi);
	}

	public void openStraightLattice()
	{

		//		VOIVector lattice = new VOIVector();
		//		LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + straightenedLattice + File.separator, true, lattice, true);
		//		return lattice;

		VOIVector voi = LatticeModel.readLatticeCSV(outputDirectory + File.separator + straightenedLattice + File.separator + "straightened_lattice.csv");
		if ( voi != null ) 
		{
			for ( int i = 0; i < voi.size(); i++ )
			{
				wormImage.registerVOI(voi.elementAt(i));
			}
		}
	}

	public void openStraightSeamCells()
	{
		//		VOIVector lattice = new VOIVector();
		//		LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + straightenedSeamCells + File.separator, true, lattice, true);
		//		return lattice;


		VOI voi = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + straightenedNamedSeamCells + File.separator + "straightened_seamcells.csv");
		if ( voi == null )
		{
			voi = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + straightenedSeamCells + File.separator + "straightened_seamcells.csv");
		}
		if ( voi != null ) wormImage.registerVOI(voi);
	}

	public VOIVector[] readAutoLattice()
	{
		VOIVector[] latticeList = new VOIVector[5];
		// read top 5 lattices:
		for ( int i = 0; i < 5; i++ )
		{
			String fileName = outputDirectory + File.separator + autoLatticeGenerationOutput + (i+1) + File.separator;
			File outputFileDir = new File(fileName);
			if ( outputFileDir.exists() )
			{
				//				latticeList[i] = new VOIVector();
				//				LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + autoLatticeGenerationOutput + (i+1) + File.separator, true, latticeList[i], false);
				latticeList[i] = LatticeModel.readLatticeCSV(outputDirectory + File.separator + autoLatticeGenerationOutput + (i+1) + File.separator + "lattice.csv");
			}
		}
		return latticeList;
	}

	public VOI readFinalLattice()
	{
		// try reading the edited lattice:
		String fileName = outputDirectory + File.separator + editLatticeOutput + File.separator;
		File outputFileDir = new File(fileName);
		if ( outputFileDir.exists() )
		{
			//			VOIVector finalLattice = new VOIVector();
			//			LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + editLatticeOutput + File.separator, true, finalLattice, false);
			VOIVector finalLattice = LatticeModel.readLatticeCSV(outputDirectory + File.separator + editLatticeOutput + File.separator + "lattice.csv");
			if ( finalLattice.size() > 0 )
			{
				return finalLattice.elementAt(0);
			}
		}
		// try reading one of the generated lattices:
		fileName = outputDirectory + File.separator + autoLatticeGenerationOutput + 1 + File.separator;
		outputFileDir = new File(fileName);
		if ( outputFileDir.exists() )
		{
			//			VOIVector lattice = new VOIVector();
			//			LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + autoLatticeGenerationOutput + 1 + File.separator, true, lattice, false);	
			VOIVector lattice = LatticeModel.readLatticeCSV(outputDirectory + File.separator + autoLatticeGenerationOutput + +1 + File.separator + "lattice.csv");
			if ( lattice.size() > 0 )
			{
				return lattice.elementAt(0);
			}
		}
		return null;
	}

	private VOI readMarkers()
	{
		//		VOI markerAnnotations = null;
		//		VOIVector annotationVector = new VOIVector();
		//		LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + editAnnotationOutput + File.separator, true, annotationVector, false);
		//		if ( annotationVector.size() > 0 )
		//		{
		//			markerAnnotations = annotationVector.elementAt(0);
		//		if ( markerAnnotations == null )
		//		{
		//			LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + editAnnotationInput + File.separator, true, annotationVector, false);
		//			if ( annotationVector.size() > 0 )
		//			{
		//				markerAnnotations = annotationVector.elementAt(0);
		//			}
		//		}
		//		return markerAnnotations;


		VOI annotations = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + editAnnotationOutput + File.separator + "annotations.csv");
		if ( annotations == null ) {
			annotations = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + editAnnotationInput + File.separator + "annotations.csv");
		}
		return annotations;
	}


	private VOI readMarkers(String outputDir)
	{
		VOI annotations = LatticeModel.readAnnotationsCSV(outputDir + File.separator + editAnnotationOutput + File.separator + "annotations.csv");
		if ( annotations == null ) {
			annotations = LatticeModel.readAnnotationsCSV(outputDir + File.separator + editAnnotationInput + File.separator + "annotations.csv");
		}
		return annotations;
	}

	public Vector<Vector3f> readSeamCells()
	{	
		//		seamEdited = false;
		//		VOIVector annotationVector = new VOIVector();
		//		LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + editSeamCellOutput + File.separator, true, annotationVector, false);
		//		if ( annotationVector.size() > 0 )
		//		{
		//			seamAnnotations = annotationVector.elementAt(0);
		//			seamEdited = true;
		//		}
		//		if ( seamAnnotations == null )
		//		{
		//			seamEdited = false;
		//			LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + autoSeamCellSegmentationOutput + File.separator, true, annotationVector, false);
		//			if ( annotationVector.size() > 0 )
		//			{
		//				seamAnnotations = annotationVector.elementAt(0);
		//			}
		//			if ( seamAnnotations == null )
		//			{
		//				return null;
		//			}
		//		}

		seamEdited = true;
		seamAnnotations = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + editSeamCellOutput + File.separator + "seam_cells.csv");
		if ( seamAnnotations == null ) {
			seamAnnotations = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + autoSeamCellSegmentationOutput + File.separator + "seam_cells.csv");
			seamEdited = false;
		}
		if ( seamAnnotations == null )
		{
			return null;
		}

		seamCellPoints = new Vector<Vector3f>();
		for ( int i = 0; i < seamAnnotations.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation)seamAnnotations.getCurves().elementAt(i);
			seamCellPoints.add(new Vector3f(text.elementAt(0)));
		}
		return seamCellPoints;
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
		else
		{
			return null;
		}
		return seamSegmentation;
	}

	public ModelImage readSkinSegmentation()
	{
		String seamCellDir = outputImagesDirectory + File.separator;
		File inputFile = new File(seamCellDir + "skinImage.xml");
		if ( inputFile.exists() )
		{
			FileIO fileIO = new FileIO();
			skinSegmentation = fileIO.readImage( seamCellDir + "skinImage.xml" );
		} 
		return skinSegmentation;
	}

	//	public void saveLattice()
	//	{
	//		LatticeModel.saveAllVOIsTo(outputDirectory + File.separator + editLatticeOutput + File.separator, wormImage);
	//	}

	public String getAnnotationsPath()
	{
		return outputDirectory + File.separator + editAnnotationOutput + File.separator;
	}

	public void saveMarkerAnnotations(VOI annotations)
	{		
		System.err.println("saveMarkerAnnotations");
		//		wormImage.unregisterAllVOIs();
		//		wormImage.registerVOI(annotations);
		//		annotations.setName("markers");
		//		System.err.println( outputDirectory + File.separator + editAnnotationOutput + File.separator );
		//		LatticeModel.saveAllVOIsTo(outputDirectory + File.separator + editAnnotationOutput + File.separator, wormImage);
		LatticeModel.saveAnnotationsAsCSV(outputDirectory + File.separator + editAnnotationOutput + File.separator, "annotations.csv", annotations);
		wormImage.unregisterAllVOIs();
	}

	public void saveIntegratedMarkerAnnotations(VOI annotations)
	{		
		System.err.println("saveIntegratedMarkerAnnotations");
		LatticeModel.saveAnnotationsAsCSV(outputDirectory + File.separator + integratedAnnotationOutput + File.separator, "annotations.csv", annotations);
	}


	public boolean checkSeamCells(VOI annotations)
	{
		if ( annotations == null ) {
			return false;
		}
		if ( annotations.getCurves().size() == 0 )
		{
			return false;
		}
		int count = 0;
		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation)annotations.getCurves().elementAt(i);
			if ( !(text.getText().equalsIgnoreCase("origin") || text.getText().contains("nose") || text.getText().contains("Nose")) )
			{
				count++;
			}
		}
		System.err.println("checkSeamCells " + count );
		return ((count%2) == 0);
	}

	public boolean checkHeadSeamCells(VOI annotations)
	{
		if ( annotations.getCurves().size() == 0 )
		{
			return false;
		}
		int count = 0;
		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation)annotations.getCurves().elementAt(i);
			if ( LatticeModel.match(text.getColor(), Color.green) )
			{
				count++;
			}
		}
		return (count == 2) || (count == 0);
	}

	public boolean checkTailSeamCells(VOI annotations)
	{
		if ( annotations.getCurves().size() == 0 )
		{
			return false;
		}
		int count = 0;
		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation)annotations.getCurves().elementAt(i);
			if ( LatticeModel.match(text.getColor(), Color.red) )
			{
				count++;
			}
		}
		return (count == 2) || (count == 0);
	}

	public void saveSeamAnnotations(VOI annotations, boolean rename, boolean checkImage)
	{		
		Vector<Vector3f> seamCellPoints = new Vector<Vector3f>();
		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation)annotations.getCurves().elementAt(i);
			if ( !(text.getText().equalsIgnoreCase("origin") || text.getText().contains("nose") || text.getText().contains("Nose")) )
			{
				Vector3f temp = new Vector3f(text.elementAt(0) );
				temp.X = Math.round(temp.X);
				temp.Y = Math.round(temp.Y);
				temp.Z = Math.round(temp.Z);
				seamCellPoints.add(temp);
				//				System.err.println( seamCellPoints.size() + "   " + text.getText() );
			}
		}
		//		System.err.println( "Seam Cells " + seamCellPoints.size() );

		if ( checkImage ) {
			// check that all seam cell points match the seam segmentation image...  redo image if necessary...
			if ( seamSegmentation == null )
			{
				readSeamSegmentation();
			}
			if ( seamSegmentation == null )
			{
				seamSegmentation = new ModelImage( ModelStorageBase.FLOAT, wormImage.getExtents(), "seamCellImage" );	
				seamSegmentation.setImageName("seamCellImage");
				JDialogBase.updateFileInfo(wormImage, seamSegmentation);
			}

			int dimX = seamSegmentation.getExtents().length > 0 ? seamSegmentation.getExtents()[0] : 1;
			int dimY = seamSegmentation.getExtents().length > 1 ? seamSegmentation.getExtents()[1] : 1;
			int dimZ = seamSegmentation.getExtents().length > 2 ? seamSegmentation.getExtents()[2] : 1;

			VOI clusterList = getSeamClusters();
			VOIContour[] clusters = new VOIContour[seamCellPoints.size()];
			for ( int i = 0; i < seamCellPoints.size(); i++ )
			{
				boolean found = false;
				for ( int j = 0; j < clusterList.getCurves().size(); j++ )
				{
					if ( clusterList.getCurves().elementAt(j).contains(seamCellPoints.elementAt(i)) )
					{
						System.err.println( "Found seam " + (i+1) );
						clusters[i] = (VOIContour) clusterList.getCurves().elementAt(j);
						found = true;
						break;
					}
				}
				if ( !found )
				{
					System.err.println( "new seam " + (i+1) );
					// New seam cell: put the seam cell at the center and use maxSeamRadius to generate a cluster:
					clusters[i] = new VOIContour(false);
					clusters[i].add(seamCellPoints.elementAt(i));
					Vector3f newPt = seamCellPoints.elementAt(i);
					int minX = (int) Math.max(0, newPt.X - minSeamRadius);
					int maxX = (int) Math.min(dimX, newPt.X + minSeamRadius + 1);

					int minY = (int) Math.max(0, newPt.Y - minSeamRadius);
					int maxY = (int) Math.min(dimY, newPt.Y + minSeamRadius + 1);

					int minZ = (int) Math.max(0, newPt.Z - minSeamRadius);
					int maxZ = (int) Math.min(dimZ, newPt.Z + minSeamRadius + 1);
					for ( int z = minZ; z < maxZ; z++ )
					{
						for ( int y = minY; y < maxY; y++ )
						{
							for ( int x = minX; x < maxX; x++ )
							{
								Vector3f temp = new Vector3f(x,y,z);
								if ( newPt.distance(temp) <= minSeamRadius )
								{
									clusters[i].add( temp );
								}
							}
						}
					}
				}
			}

			for ( int i = 0; i < clusters.length; i++ )
			{
				Vector<Vector3f> splitList = null;
				Vector<Integer> splitIndex = null;
				for ( int j = i+1; j < clusters.length; j++ )
				{
					if ( clusters[i] == clusters[j] )
					{
						if ( splitList == null )
						{
							splitList = new Vector<Vector3f>();
							splitList.add(seamCellPoints.elementAt(i));

							splitIndex = new Vector<Integer>();
							splitIndex.add(i);
						}
						splitList.add(seamCellPoints.elementAt(j));		
						splitIndex.add(j);			
					}
				}
				if ( splitList != null && splitIndex != null )
				{
					VOIContour splitContour = clusters[i];
					for ( int j = 0; j < splitIndex.size(); j++ )
					{
						int index = splitIndex.elementAt(j);
						clusters[index] = new VOIContour(false);
					}
					for ( int j = 0; j < splitContour.size(); j++ )
					{
						int minIndex = -1;
						float minDist = Float.MAX_VALUE;
						for ( int k = 0; k < splitList.size(); k++ )
						{
							float dist = splitContour.elementAt(j).distance(splitList.elementAt(k));
							if ( dist < minDist )
							{
								minDist = dist;
								minIndex = splitIndex.elementAt(k);
							}
						}
						if ( minIndex != -1 )
						{
							clusters[minIndex].add(splitContour.elementAt(j));
						}
					}
					for ( int j = 0; j < splitIndex.size(); j++ )
					{
						int index = splitIndex.elementAt(j);
						if ( !clusters[index].contains(splitContour.elementAt(j)) )
						{
							clusters[index].add(splitContour.elementAt(j));
						}
					}
				}
			}

			int clusterCount = 1;
			for ( int i = 0; i < seamSegmentation.getDataSize(); i++ )
			{
				seamSegmentation.set(i, 0);
			}
			for ( int i = 0; i < clusters.length; i++ )
			{
				VOIContour cluster = clusters[i];
				if ( cluster != null )
				{
					for ( int j = 0; j < cluster.size(); j++ )
					{
						int x = Math.round(cluster.elementAt(j).X);
						int y = Math.round(cluster.elementAt(j).Y);
						int z = Math.round(cluster.elementAt(j).Z);

						seamSegmentation.set(x, y, z, clusterCount);
					}
				}
				clusterCount++;
			}
			seamSegmentation.setImageName("seamCellImage");
			JDialogBase.updateFileInfo(wormImage, seamSegmentation);
			ModelImage.saveImage(seamSegmentation, seamSegmentation.getImageName() + ".xml", outputImagesDirectory, false);

			boolean allMatch = true;
			for ( int i = 0; i < seamCellPoints.size(); i++ )
			{
				int x = (int)seamCellPoints.elementAt(i).X;
				int y = (int)seamCellPoints.elementAt(i).Y;
				int z = (int)seamCellPoints.elementAt(i).Z;
				int id = seamSegmentation.getInt(x, y, z );
				if ( i != (id-1) )
				{
					//error:
					System.err.println( "error wrong id:   " + (i+1) + "   " + id );				
					allMatch = false;
				}
			}
			System.err.println( seamCellPoints.size() + "  allfound? " + allMatch );
		}
		if ( rename )
		{
			int id = 1;
			for ( int i = 0; i < annotations.getCurves().size(); i++ )
			{
				VOIWormAnnotation text = (VOIWormAnnotation)annotations.getCurves().elementAt(i);
				if ( !(text.getText().equalsIgnoreCase("origin") || text.getText().contains("nose") || text.getText().contains("Nose")) )
				{
					text.setText( "" + id++ );
				}
			}
		}
		annotations.setName("seam cells");
		//		LatticeModel.saveAllVOIsTo(outputDirectory + File.separator + editSeamCellOutput + File.separator, wormImage);
		if ( rename ) {
			LatticeModel.saveAnnotationsAsCSV(outputDirectory + File.separator + editSeamCellOutput + File.separator, "seam_cells.csv", annotations);
		}
		else {
			LatticeModel.saveAnnotationsAsCSV(outputDirectory + File.separator + namedSeamCellOutput + File.separator, "seam_cells.csv", annotations);			
		}
	}

	public void segmentSeamCells(int minRadius, int maxRadius)
	{
		minSeamRadius = minRadius;
		maxSeamRadius = minRadius;
		//		System.err.println( "   segmentSeamCells: start" );
		if ( (seamCellPoints == null) || (seamCellPoints.size() == 0) )
		{
			String seamCellDir = outputImagesDirectory + File.separator;
			File outputDir = new File(seamCellDir);
			if ( !outputDir.exists() )
			{
				//				System.err.println( "segmentSeamCells " + seamCellDir);
				outputDir.mkdir();
			}
			seamCellPoints = new Vector<Vector3f>();
			VOI finalClusterList = new VOI((short)0, "temp", VOI.POLYLINE, 0);

			seamSegmentation = new ModelImage( ModelStorageBase.FLOAT, wormImage.getExtents(), "seamCellImage" );	
			seamSegmentation.setImageName("seamCellImage");
			JDialogBase.updateFileInfo(wormImage, seamSegmentation);

			WormSegmentation segmentation = new WormSegmentation();
			boolean[] passTest = new boolean[6];
			VOIContour[] ptsTest = new VOIContour[6];
			VOI[] clusterTest = new VOI[6];
			int[] radiusTest = new int[6];

			int testCount = 0;
			ptsTest[testCount] = new VOIContour(false);
			clusterTest[testCount] = new VOI((short)0, "temp", VOI.POLYLINE, 0);			
			passTest[testCount] = segmentation.segmentSeamNew( wormImage, ptsTest[0], clusterTest[0], minRadius, maxRadius, outputImagesDirectory, 22, true );
			radiusTest[testCount] = minRadius;


			int diff = -2;

			testCount++;
			ptsTest[testCount] = new VOIContour(false);
			clusterTest[testCount] = new VOI((short)0, "temp", VOI.POLYLINE, 0);
			passTest[testCount] = segmentation.segmentSeamNew( wormImage, ptsTest[1], clusterTest[1], minRadius+diff, maxRadius, outputImagesDirectory, 22, true );
			radiusTest[testCount] = minRadius+diff;

			boolean found = false;
			if ( passTest[0] && passTest[1] && allMatch(ptsTest[0], clusterTest[0], ptsTest[1], clusterTest[1], minRadius, seamCellPoints, finalClusterList ) )
			{
				found = true;
			}

			if ( !found )
			{
				testCount++;
				ptsTest[testCount] = new VOIContour(false);
				clusterTest[testCount] = new VOI((short)0, "temp", VOI.POLYLINE, 0);
				passTest[testCount] = segmentation.segmentSeamNew( wormImage, ptsTest[2], clusterTest[2], minRadius, maxRadius, outputImagesDirectory, 20, true );
				radiusTest[testCount] = minRadius;

				for ( int i = 0; i <= testCount; i++ )
				{
					for ( int j = i+1; j <= testCount; j++ )
					{
						if ( passTest[i] && passTest[j] && (radiusTest[i] != radiusTest[j]) &&
								allMatch(ptsTest[i], clusterTest[i], ptsTest[j], clusterTest[j], minRadius, seamCellPoints, finalClusterList) )
						{
							found = true;
							break;
						}
					}
					if ( found )
					{
						break;
					}
				}

				if ( !found )
				{
					testCount++;
					ptsTest[testCount] = new VOIContour(false);
					clusterTest[testCount] = new VOI((short)0, "temp", VOI.POLYLINE, 0);
					passTest[testCount] = segmentation.segmentSeamNew( wormImage, ptsTest[3], clusterTest[3], minRadius+diff, maxRadius, outputImagesDirectory, 20, true );
					radiusTest[testCount] = minRadius+diff;

					for ( int i = 0; i <= testCount; i++ )
					{
						for ( int j = i+1; j <= testCount; j++ )
						{
							if ( passTest[i] && passTest[j] && (radiusTest[i] != radiusTest[j]) &&
									allMatch(ptsTest[i], clusterTest[i], ptsTest[j], clusterTest[j], minRadius, seamCellPoints, finalClusterList) )
							{
								found = true;
								break;
							}
						}
						if ( found )
						{
							break;
						}
					}


					if ( !found )
					{
						testCount++;
						ptsTest[testCount] = new VOIContour(false);
						clusterTest[testCount] = new VOI((short)0, "temp", VOI.POLYLINE, 0);
						passTest[testCount] = segmentation.segmentSeamNew( wormImage, ptsTest[4], clusterTest[4], minRadius, maxRadius, outputImagesDirectory, 18, false );
						radiusTest[testCount] = minRadius;
						for ( int i = 0; i <= testCount; i++ )
						{
							for ( int j = i+1; j <= testCount; j++ )
							{
								if ( passTest[i] && passTest[j] && (radiusTest[i] != radiusTest[j]) && 
										allMatch(ptsTest[i], clusterTest[i], ptsTest[j], clusterTest[j], minRadius, seamCellPoints, finalClusterList) )
								{
									found = true;
									break;
								}
							}
							if ( found )
							{
								break;
							}
						}

						if ( !found )
						{
							testCount++;
							ptsTest[testCount] = new VOIContour(false);
							clusterTest[testCount] = new VOI((short)0, "temp", VOI.POLYLINE, 0);
							passTest[testCount] = segmentation.segmentSeamNew( wormImage, ptsTest[4], clusterTest[4], minRadius+diff, maxRadius, outputImagesDirectory, 18, false );
							radiusTest[testCount] = minRadius+diff;
							for ( int i = 0; i <= testCount; i++ )
							{
								for ( int j = i+1; j <= testCount; j++ )
								{
									if ( passTest[i] && passTest[j] && (radiusTest[i] != radiusTest[j]) && 
											allMatch(ptsTest[i], clusterTest[i], ptsTest[j], clusterTest[j], minRadius, seamCellPoints, finalClusterList) )
									{
										found = true;
										break;
									}
								}
								if ( found )
								{
									break;
								}
							}
						}
					}
				}
			}



			System.err.println( found + "   " + seamCellPoints.size() );


			if ( !found )
			{
				for ( int i = 0; i < ptsTest.length; i++ )
				{
					if ( passTest[i] )
					{
						seamCellPoints.clear();
						seamCellPoints.addAll(ptsTest[i]);
						finalClusterList.getCurves().clear();
						finalClusterList.getCurves().addAll(clusterTest[i].getCurves());
						found = true;
						break;
					}
				}				
			}


			if ( found )
			{
				int clusterCount = 1;
				for ( int i = 0; i < seamSegmentation.getDataSize(); i++ )
				{
					seamSegmentation.set(i, 0);
				}
				for ( int i = 0; i < finalClusterList.getCurves().size(); i++ )
				{
					VOIContour cluster = (VOIContour) finalClusterList.getCurves().elementAt(i);
					for ( int j = 0; j < cluster.size(); j++ )
					{
						int x = Math.round(cluster.elementAt(j).X);
						int y = Math.round(cluster.elementAt(j).Y);
						int z = Math.round(cluster.elementAt(j).Z);

						seamSegmentation.set(x, y, z, clusterCount);
					}
					clusterCount++;
				}
				seamSegmentation.setImageName("seamCellImage");
				JDialogBase.updateFileInfo(wormImage, seamSegmentation);
				ModelImage.saveImage(seamSegmentation, seamSegmentation.getImageName() + ".xml", outputImagesDirectory, false);
			}

			//			minSeamCellSegmentationIntensity = segmentation.getMinSegmentationIntensity();
		}

		if ( seamCellPoints.size() > 0 )
		{
			seamAnnotations = new VOI((short) 0, "seam cells", VOI.ANNOTATION, -1.0f);
			wormImage.unregisterAllVOIs();
			wormImage.registerVOI(seamAnnotations);
			for ( int i = 0; i < seamCellPoints.size(); i++ )
			{
				//			System.err.println( i );
				VOIWormAnnotation text = new VOIWormAnnotation();
				text.add(seamCellPoints.elementAt(i));
				text.add(seamCellPoints.elementAt(i));
				text.setText( "" + (i+1) );
				text.setUseMarker(false);
				text.update();

				int x = (int)seamCellPoints.elementAt(i).X;
				int y = (int)seamCellPoints.elementAt(i).Y;
				int z = (int)seamCellPoints.elementAt(i).Z;
				if ( (i+1) != seamSegmentation.getFloat(x,y,z) )
				{
					System.err.println( "Seam Cell " + (i+1) + "  " + seamSegmentation.getFloat(x,y,z) );
				}
				seamAnnotations.getCurves().add(text);
			}

			String seamCellDir = outputDirectory + File.separator + autoSeamCellSegmentationOutput + File.separator;
			File outputFileDir = new File(seamCellDir);
			if ( !outputFileDir.exists() )
			{
				//			System.err.println( "segmentSeamCells " + seamCellDir);
				outputFileDir.mkdir();
			}

			//			LatticeModel.saveAllVOIsTo(outputDirectory + File.separator + autoSeamCellSegmentationOutput + File.separator, wormImage);
			LatticeModel.saveAnnotationsAsCSV(seamCellDir, "seam_cells.csv", seamAnnotations);
		}
		//		System.err.println( "   segmentSeamCells: end " + minSeamCellSegmentationIntensity );
	}

	public void readNamedSeamCells() 
	{
		String seamCellDir = outputDirectory + File.separator + namedSeamCellOutput + File.separator;
		File outputFileDir = new File(seamCellDir);
		if ( !outputFileDir.exists() )
		{
			segmentSeamFromLattice(null, null, true);
			return;
		}
		seamEdited = true;
		seamAnnotations = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + namedSeamCellOutput + File.separator + "seam_cells.csv");
		if ( seamAnnotations == null ) {
			segmentSeamFromLattice(null, null, true);
			return;
		}

		seamCellPoints = new Vector<Vector3f>();
		for ( int i = 0; i < seamAnnotations.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation)seamAnnotations.getCurves().elementAt(i);
			seamCellPoints.add(new Vector3f(text.elementAt(0)));
		}
	}

	public VOI segmentSeamFromLattice(VOI lattice, ModelImage image, boolean save)
	{

		System.err.println("segmentSeamFromLattice");
		if ( lattice == null )
		{
			lattice = readFinalLattice();
			if ( lattice == null ) return null;
		}
		VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
		VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);

		seamAnnotations = new VOI((short) 0, "seam cells", VOI.ANNOTATION, -1.0f);
		if ( image == null )
		{
			image = wormImage;
		}

		float[] pairSort = new float[left.size()];
		// decide which 10 points are seam cells by taking max pair values
		for ( int i = 0; i < left.size() -1; i++ )
		{
			int x = (int)left.elementAt(i).X;
			int y = (int)left.elementAt(i).Y;
			int z = (int)left.elementAt(i).Z;
			float value;
			if ( image.isColorImage() ) 
			{
				value = image.getFloatC(x,y,z,2);
			}
			else
			{
				value = image.getFloat(x,y,z);
			}

			x = (int)right.elementAt(i).X;
			y = (int)right.elementAt(i).Y;
			z = (int)right.elementAt(i).Z;
			if ( image.isColorImage() ) 
			{
				value += image.getFloatC(x,y,z,2);
			}
			else
			{
				value += image.getFloat(x,y,z);
			}
			pairSort[i] = value;
		}
		Arrays.sort(pairSort);

		int pairCount = 0;
		int extraCount = 0;
		for ( int i = 0; i < left.size(); i++ )
		{
			int x = (int)left.elementAt(i).X;
			int y = (int)left.elementAt(i).Y;
			int z = (int)left.elementAt(i).Z;
			float value;
			if ( image.isColorImage() ) 
			{
				value = image.getFloatC(x,y,z,2);
			}
			else
			{
				value = image.getFloat(x,y,z);
			}
			x = (int)right.elementAt(i).X;
			y = (int)right.elementAt(i).Y;
			z = (int)right.elementAt(i).Z;
			if ( image.isColorImage() ) 
			{
				value += image.getFloatC(x,y,z,2);
			}
			else
			{
				value += image.getFloat(x,y,z);
			}

			int segment = Math.min(left.size() - 1, i+1);
			boolean isSeamPair = (left.size() <= 10) || (i == left.size() -1);
			if ( !isSeamPair )
			{
				// check if this pair has a high enough value to be added to the list of seam cells:
				for ( int j = 0; j < Math.min(9, pairSort.length); j++ )
				{
					if ( value >= pairSort[(pairSort.length -1) - j] )
					{
						System.err.println( value + "  " + pairSort[(pairSort.length -1) - j]);
						isSeamPair = true;
						break;
					}
				}
			}
			if ( isSeamPair )
			{
				String name = pairCount < 3 ? ("H" + pairCount) : (pairCount < 9) ? ("V" + (pairCount - 2)) : "T";
				pairCount++;

				// left seam cell:
				VOIWormAnnotation text = new VOIWormAnnotation();
				text.add(left.elementAt(i));
				text.add(left.elementAt(i));
				text.setText( name + "L" );
				text.setLatticeSegment(segment);
				text.setUseMarker(false);
				text.update();

				//			int x = (int)seamCellPoints.elementAt(i).X;
				//			int y = (int)seamCellPoints.elementAt(i).Y;
				//			int z = (int)seamCellPoints.elementAt(i).Z;
				//			if ( (i+1) != seamSegmentation.getFloat(x,y,z) )
				//			{
				//				System.err.println( "Seam Cell " + (i+1) + "  " + seamSegmentation.getFloat(x,y,z) );
				//			}
				seamAnnotations.getCurves().add(text);

				// right seam cell:
				// left seam cell:
				text = new VOIWormAnnotation();
				text.add(right.elementAt(i));
				text.add(right.elementAt(i));
				text.setText( name + "R" );
				text.setLatticeSegment(segment);
				text.setUseMarker(false);
				text.update();
				seamAnnotations.getCurves().add(text);
			}
			else
			{
				String name = "a" + extraCount++;

				// left seam cell:
				VOIWormAnnotation text = new VOIWormAnnotation();
				text.add(left.elementAt(i));
				text.add(left.elementAt(i));
				text.setText( name + "L" );
				text.setLatticeSegment(segment);
				text.setUseMarker(false);
				text.update();

				//			int x = (int)seamCellPoints.elementAt(i).X;
				//			int y = (int)seamCellPoints.elementAt(i).Y;
				//			int z = (int)seamCellPoints.elementAt(i).Z;
				//			if ( (i+1) != seamSegmentation.getFloat(x,y,z) )
				//			{
				//				System.err.println( "Seam Cell " + (i+1) + "  " + seamSegmentation.getFloat(x,y,z) );
				//			}
				seamAnnotations.getCurves().add(text);

				// right seam cell:
				// left seam cell:
				text = new VOIWormAnnotation();
				text.add(right.elementAt(i));
				text.add(right.elementAt(i));
				text.setText( name + "R" );
				text.setLatticeSegment(segment);
				text.setUseMarker(false);
				text.update();
				seamAnnotations.getCurves().add(text);

			}
		}
		if ( save )
		{
			String seamCellDir = outputDirectory + File.separator + namedSeamCellOutput + File.separator;
			File outputFileDir = new File(seamCellDir);
			if ( !outputFileDir.exists() )
			{
				outputFileDir.mkdir();
			}
			LatticeModel.saveAnnotationsAsCSV(outputDirectory + File.separator + namedSeamCellOutput + File.separator, "seam_cells.csv", seamAnnotations);
		}

		seamCellPoints = new Vector<Vector3f>();
		for ( int i = 0; i < seamAnnotations.getCurves().size(); i++ )
		{
			VOIWormAnnotation text = (VOIWormAnnotation)seamAnnotations.getCurves().elementAt(i);
			seamCellPoints.add(new Vector3f(text.elementAt(0)));
		}

		return seamAnnotations;
	}

	public void segmentSkin()
	{
		if ( skinSegmentation == null )
		{
			ModelImage wormBlur = WormSegmentation.blur(wormImage, 3);

			//			ModelImage surface = WormSegmentation.outlineA( wormBlur, minSeamCellSegmentationIntensity );
			//			ModelImage surface = new ModelImage( ModelStorageBase.ARGB_FLOAT, wormImage.getExtents(), imageName + "_skin" );
			//			WormSegmentation.outline2( outputImagesDirectory, wormImage, surface, minSeamCellSegmentationIntensity);

			ModelImage[] outsideInside = WormSegmentation.outside(wormBlur, 5);
			skinSegmentation = outsideInside[0];
			insideSegmentation = outsideInside[1];
			skinSegmentation.setImageName( "skinImage" );
			ModelImage.saveImage(skinSegmentation, skinSegmentation.getImageName() + ".xml", outputImagesDirectory, false);

			wormBlur.disposeLocal(false);
			wormBlur = null;

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

	public void setNucleiImage(ModelImage image)
	{
		//		nucleiImage = image;
		//		if ( nucleiBlur == null )
		//		{
		//			nucleiBlur = WormSegmentation.blur(nucleiImage, 3);
		//		}
		//		if ( wormBlur != null )
		//		{
		//			wormBlur = WormSegmentation.blur(wormImage, 3);
		//		}
		//		float maxNuclei = (float) nucleiBlur.getMax();
		//		float maxWorm = (float) wormBlur.getMax();
		//
		//		ModelImage insideOut = new ModelImage(ModelStorageBase.FLOAT, nucleiImage.getExtents(), "insideOut.tif");
		//		float min = Float.MAX_VALUE;
		//		float max = -Float.MAX_VALUE;
		//		for ( int i = 0; i < nucleiImage.getDataSize(); i++ )
		//		{
		//			float inside = insideSegmentation.getFloat(i);
		//			if ( inside > 0 )
		//			{
		//				float wormValue = wormBlur.getFloat(i);
		//				float nucleiValue = nucleiBlur.getFloat(i);
		//				if ( (wormValue > minSeamCellSegmentationIntensity) && (nucleiValue < .1*maxNuclei) )
		//				{
		//					insideOut.set(i, 10);
		//				}
		//			}
		//		}
		//		insideOut.setMax(10);
		//		insideOut.setMin(0);
		//		new ViewJFrameImage(insideOut);
	}

	public boolean testLattice( Vector<Vector3f> left, Vector<Vector3f> right, boolean reorder )
	{
		if ( seamSegmentation == null )
		{
			readSeamCells();
			readSeamSegmentation();
		}
		if ( seamCellPoints == null )
		{
			segmentSeamCells(8,25);
		}
		if ( skinSegmentation == null )
		{
			segmentSkin();
		}
		LatticeBuilder builder = new LatticeBuilder();
		builder.setSeamImage(seamSegmentation);
		builder.setSkinImage(skinSegmentation);

		if ( reorder )
		{
			Vector3f[] leftTemp = new Vector3f[left.size()];
			Vector3f[] rightTemp = new Vector3f[right.size()];
			for ( int i = 0; i < left.size(); i++ )
			{
				leftTemp[i] = left.elementAt((left.size()-1)-i);
				rightTemp[i] = right.elementAt((right.size()-1)-i);
			}
			left.removeAllElements();
			right.removeAllElements();
			for ( int i = 0; i < leftTemp.length; i++ )
			{
				left.add(leftTemp[i]);
				right.add(rightTemp[i]);
			}
		}
		boolean pass = builder.testLattice(wormImage, left, right);
		System.err.println( "lattice test " + pass );
		return pass;
	}

	private boolean allMatch(VOIContour pts1, VOI clusterList1, VOIContour pts2, VOI clusterList2, float minDist, Vector<Vector3f> finalPtsList, VOI finalClusterList )
	{
		if ( pts1.size() > pts2.size() )
		{
			return allMatch(pts2, clusterList2, pts1, clusterList1, minDist, finalPtsList, finalClusterList);
		}

		for ( int i = 0; i < pts1.size(); i++ )
		{
			boolean foundMatch = false;
			float minDistTest = Float.MAX_VALUE;
			for ( int j = 0; j < pts2.size(); j++ )
			{
				float distance = pts1.elementAt(i).distance(pts2.elementAt(j));
				if ( distance < minDistTest )
				{
					minDistTest = distance;
				}
				if ( distance < minDist )
				{
					foundMatch = true;
					break;
				}
			}
			if ( !foundMatch )
			{
				//				System.err.println( i + "   " + minDistTest + "   " + minDist );
				return false;
			}
		}

		// Add larger list:
		for ( int i = 0; i < pts2.size(); i++ )
		{
			finalPtsList.add( pts2.elementAt(i) );
			finalClusterList.getCurves().add( clusterList2.getCurves().elementAt(i) );
		}
		return true;
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
			System.err.println( "WormData:checkParentDir " + parentDir);
			parentFileDir.mkdir();
		}
	}

	private VOI getSeamClusters()
	{
		if ( seamSegmentation == null )
		{
			readSeamSegmentation();
		}
		VOI clusterList = new VOI((short)0, "temp", VOI.POLYLINE, 0);
		HashMap<Integer, VOIContour> clusters = new HashMap<Integer, VOIContour>();
		int dimX = seamSegmentation.getExtents().length > 0 ? seamSegmentation.getExtents()[0] : 1;
		int dimY = seamSegmentation.getExtents().length > 1 ? seamSegmentation.getExtents()[1] : 1;
		int dimZ = seamSegmentation.getExtents().length > 2 ? seamSegmentation.getExtents()[2] : 1;
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					int id = seamSegmentation.getInt(x, y, z );
					if ( id > 0 )
					{
						if ( !clusters.containsKey(id) )
						{
							VOIContour cluster = new VOIContour(false);
							clusters.put(id, cluster);
							clusterList.getCurves().add(cluster);
							//							System.err.println( "Adding cluster " + id );
						}
						VOIContour cluster = clusters.get(id);
						cluster.add( new Vector3f(x,y,z));
					}
				}
			}
		}
		//		System.err.println( "Done reading clusters" );
		return clusterList;
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