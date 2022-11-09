package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public class WormData
{
	public static float VoxelSize =  0.1625f;

	public static final String editSeamCellOutput = new String("seam_cell_final");
	public static final String namedSeamCellOutput = new String("named_seam_cells");
	public static final String autoLatticeGenerationOutput = new String("lattice_");
	public static final String editLatticeOutput = new String("lattice_final");
	public static final String integratedAnnotationOutput = new String("integrated_annotation");
	public static final String straightenedLattice = new String("straightened_lattice");
	public static final String straightenedAnnotations = new String("straightened_annotations");
	public static final String neuriteOutput = new String("neurite_final");


	private String imageName;
	private ModelImage wormImage;
	
	private String outputDirectory = null;
	private String outputImagesDirectory = null;


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
				if ( outputImagesDirectory != null && outputImagesDirectory.contains( "output_images" ) ) {
					outputDirectory = outputImagesDirectory.replaceAll( "output_images", "");
				}
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
			if ( outputImagesDirectory != null ) {
				dir = new File(outputImagesDirectory);
				if ( !dir.exists() )
				{
					//				System.err.println( "WormData " + outputImagesDirectory);
					dir.mkdir();
				}
			}
		}
	}

	public static boolean integratedExists(String outputDirectory) {
		File file = new File(outputDirectory + File.separator + integratedAnnotationOutput + File.separator + "annotations.csv");
		return file.exists();
	}

	public static String getIntegratedMarkerAnnotationsPath(String outputDirectory) {
		return (outputDirectory + File.separator + integratedAnnotationOutput + File.separator + "annotations.csv");
	}
	
	public static String getStraightAnnotationsPath(String outputDirectory) {
		return (outputDirectory + File.separator + straightenedAnnotations + File.separator + "straightened_annotations.csv");
	}

	public static VOI getIntegratedMarkerAnnotations(String outputDirectory)
	{
		System.err.println("getIntegratedMarkerAnnotations");
		VOI markerAnnotations = LatticeModel.readAnnotationsCSV(outputDirectory + File.separator + integratedAnnotationOutput + File.separator + "annotations.csv");

		if ( markerAnnotations == null )
			markerAnnotations = new VOI( (short)0, "annotationVOIs", VOI.ANNOTATION, 0 );
		return markerAnnotations;
	}

	public static void openStraightAnnotations( String dir, ModelImage image)
	{
		VOI voi = LatticeModel.readAnnotationsCSV(dir + File.separator + straightenedAnnotations + File.separator + "straightened_annotations.csv");
		if ( voi != null ) image.registerVOI(voi);
	}
	
	public static VOIVector readStraightLattice(String outputDirectory)
	{
		VOIVector voi = LatticeModel.readLatticeCSV(outputDirectory + File.separator + straightenedLattice + File.separator + "straightened_lattice.csv");
		return voi;
	}

	public VOIVector readFinalLattice()
	{
		return readFinalLattice(false);
	}

	private VOIVector readFinalLattice(boolean convertLegacyXML) {
		return readFinalLattice(outputDirectory, convertLegacyXML, wormImage);
	}
	
	public static VOIVector readFinalLattice(String outputDirectory, boolean convertLegacyXML, ModelImage image)
	{
		// try reading the edited lattice:
		String fileName = outputDirectory + File.separator + editLatticeOutput + File.separator;
		File outputFileDir = new File(fileName);
		if ( outputFileDir.exists() )
		{
			VOIVector finalLattice = LatticeModel.readLatticeCSV(outputDirectory + File.separator + editLatticeOutput + File.separator + "lattice.csv");
			if ( (finalLattice == null) || (finalLattice.size() < 2) || convertLegacyXML) {
				finalLattice = readLegacyLattice(fileName, image);
				if ( finalLattice == null ) {
					String latticeDir = outputDirectory + File.separator + "lattice" + File.separator;
					outputFileDir = new File(fileName);
					if ( outputFileDir.exists() ) {
						finalLattice = readLegacyLattice(latticeDir, image);
					}
				}
			}
			if ( LatticeModel.renameLatticeOnLoad(image, finalLattice) || !checkSeamCells(outputDirectory) ) {
				LatticeModel.saveLattice(outputDirectory + File.separator, editLatticeOutput, finalLattice);
			}
			return finalLattice;
		}
		// try reading one of the generated lattices:
		fileName = outputDirectory + File.separator + autoLatticeGenerationOutput + 1 + File.separator;
		outputFileDir = new File(fileName);
		if ( outputFileDir.exists() )
		{
			//			VOIVector lattice = new VOIVector();
			//			LatticeModel.loadAllVOIsFrom(wormImage, outputDirectory + File.separator + autoLatticeGenerationOutput + 1 + File.separator, true, lattice, false);	
			VOIVector lattice = LatticeModel.readLatticeCSV(outputDirectory + File.separator + autoLatticeGenerationOutput + +1 + File.separator + "lattice.csv");
			return lattice;
		}
		return null;
	}

	private static VOIVector readLegacyLattice(String fileName, ModelImage wormImage) {

		File latticeFile = new File(fileName + "lattice.xml");
		if ( !latticeFile.exists() ) return null;
			
		VOIVector finalLattice = new VOIVector();
		
		short sID = 0;
		VOI left = new VOI(sID, "left", VOI.ANNOTATION, (float) Math.random());
		left.setColor(new Color(0, 0, 255));
		VOI right = new VOI(sID++, "right", VOI.ANNOTATION, (float) Math.random());
		right.setColor(new Color(0, 0, 255));
		finalLattice.add(left);
		finalLattice.add(right);
		// try reading .xml:
		FileVOI fileVOI;
		try {
			fileVOI = new FileVOI( "lattice.xml", fileName, wormImage);
			VOI[] vois = fileVOI.readVOI(false);
			if ( vois != null && vois[0] != null ) {
				if ( vois[0].getCurves() != null && vois[0].getCurves().size() == 2 ) {
					for ( int i = 0; i < vois[0].getCurves().elementAt(0).size(); i++ ) {
						VOIWormAnnotation annotation = new VOIWormAnnotation( vois[0].getCurves().elementAt(0).elementAt(i) );
						left.getCurves().add( annotation );

						annotation = new VOIWormAnnotation( vois[0].getCurves().elementAt(1).elementAt(i) );
						right.getCurves().add( annotation );
					}
				}
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return finalLattice;
	}

	private static boolean checkSeamCells(String outputDirectory) {

		File file = new File(outputDirectory + File.separator + editSeamCellOutput + File.separator + "seam_cells.csv");
		return file.exists();
	}

	public static void saveIntegratedMarkerAnnotations(String outputDirectory, VOI annotations)
	{		
//		System.err.println("saveIntegratedMarkerAnnotations");
		LatticeModel.saveAnnotationsAsCSV(outputDirectory + File.separator + integratedAnnotationOutput + File.separator, "annotations.csv", annotations);
	}

	public static void saveSeamAnnotations(String outputDirectory, VOI annotations, boolean rename, boolean checkImage)
	{
		if ( annotations == null ) return;
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

//		if ( checkImage ) {
//			// check that all seam cell points match the seam segmentation image...  redo image if necessary...
//			if ( seamSegmentation == null )
//			{
//				readSeamSegmentation();
//			}
//			if ( seamSegmentation == null )
//			{
//				seamSegmentation = new ModelImage( ModelStorageBase.FLOAT, wormImage.getExtents(), "seamCellImage" );	
//				seamSegmentation.setImageName("seamCellImage");
//				JDialogBase.updateFileInfo(wormImage, seamSegmentation);
//			}
//
//			int dimX = seamSegmentation.getExtents().length > 0 ? seamSegmentation.getExtents()[0] : 1;
//			int dimY = seamSegmentation.getExtents().length > 1 ? seamSegmentation.getExtents()[1] : 1;
//			int dimZ = seamSegmentation.getExtents().length > 2 ? seamSegmentation.getExtents()[2] : 1;
//
//			VOI clusterList = getSeamClusters();
//			VOIContour[] clusters = new VOIContour[seamCellPoints.size()];
//			for ( int i = 0; i < seamCellPoints.size(); i++ )
//			{
//				boolean found = false;
//				for ( int j = 0; j < clusterList.getCurves().size(); j++ )
//				{
//					if ( clusterList.getCurves().elementAt(j).contains(seamCellPoints.elementAt(i)) )
//					{
//						System.err.println( "Found seam " + (i+1) );
//						clusters[i] = (VOIContour) clusterList.getCurves().elementAt(j);
//						found = true;
//						break;
//					}
//				}
//				if ( !found )
//				{
//					System.err.println( "new seam " + (i+1) );
//					// New seam cell: put the seam cell at the center and use maxSeamRadius to generate a cluster:
//					clusters[i] = new VOIContour(false);
//					clusters[i].add(seamCellPoints.elementAt(i));
//					Vector3f newPt = seamCellPoints.elementAt(i);
//					int minX = (int) Math.max(0, newPt.X - minSeamRadius);
//					int maxX = (int) Math.min(dimX, newPt.X + minSeamRadius + 1);
//
//					int minY = (int) Math.max(0, newPt.Y - minSeamRadius);
//					int maxY = (int) Math.min(dimY, newPt.Y + minSeamRadius + 1);
//
//					int minZ = (int) Math.max(0, newPt.Z - minSeamRadius);
//					int maxZ = (int) Math.min(dimZ, newPt.Z + minSeamRadius + 1);
//					for ( int z = minZ; z < maxZ; z++ )
//					{
//						for ( int y = minY; y < maxY; y++ )
//						{
//							for ( int x = minX; x < maxX; x++ )
//							{
//								Vector3f temp = new Vector3f(x,y,z);
//								if ( newPt.distance(temp) <= minSeamRadius )
//								{
//									clusters[i].add( temp );
//								}
//							}
//						}
//					}
//				}
//			}
//
//			for ( int i = 0; i < clusters.length; i++ )
//			{
//				Vector<Vector3f> splitList = null;
//				Vector<Integer> splitIndex = null;
//				for ( int j = i+1; j < clusters.length; j++ )
//				{
//					if ( clusters[i] == clusters[j] )
//					{
//						if ( splitList == null )
//						{
//							splitList = new Vector<Vector3f>();
//							splitList.add(seamCellPoints.elementAt(i));
//
//							splitIndex = new Vector<Integer>();
//							splitIndex.add(i);
//						}
//						splitList.add(seamCellPoints.elementAt(j));		
//						splitIndex.add(j);			
//					}
//				}
//				if ( splitList != null && splitIndex != null )
//				{
//					VOIContour splitContour = clusters[i];
//					for ( int j = 0; j < splitIndex.size(); j++ )
//					{
//						int index = splitIndex.elementAt(j);
//						clusters[index] = new VOIContour(false);
//					}
//					for ( int j = 0; j < splitContour.size(); j++ )
//					{
//						int minIndex = -1;
//						float minDist = Float.MAX_VALUE;
//						for ( int k = 0; k < splitList.size(); k++ )
//						{
//							float dist = splitContour.elementAt(j).distance(splitList.elementAt(k));
//							if ( dist < minDist )
//							{
//								minDist = dist;
//								minIndex = splitIndex.elementAt(k);
//							}
//						}
//						if ( minIndex != -1 )
//						{
//							clusters[minIndex].add(splitContour.elementAt(j));
//						}
//					}
//					for ( int j = 0; j < splitIndex.size(); j++ )
//					{
//						int index = splitIndex.elementAt(j);
//						if ( !clusters[index].contains(splitContour.elementAt(j)) )
//						{
//							clusters[index].add(splitContour.elementAt(j));
//						}
//					}
//				}
//			}
//
//			int clusterCount = 1;
//			for ( int i = 0; i < seamSegmentation.getDataSize(); i++ )
//			{
//				seamSegmentation.set(i, 0);
//			}
//			for ( int i = 0; i < clusters.length; i++ )
//			{
//				VOIContour cluster = clusters[i];
//				if ( cluster != null )
//				{
//					for ( int j = 0; j < cluster.size(); j++ )
//					{
//						int x = Math.round(cluster.elementAt(j).X);
//						int y = Math.round(cluster.elementAt(j).Y);
//						int z = Math.round(cluster.elementAt(j).Z);
//
//						seamSegmentation.set(x, y, z, clusterCount);
//					}
//				}
//				clusterCount++;
//			}
//			seamSegmentation.setImageName("seamCellImage");
//			JDialogBase.updateFileInfo(wormImage, seamSegmentation);
//			ModelImage.saveImage(seamSegmentation, seamSegmentation.getImageName() + ".xml", outputImagesDirectory, false);
//
//			boolean allMatch = true;
//			for ( int i = 0; i < seamCellPoints.size(); i++ )
//			{
//				int x = (int)seamCellPoints.elementAt(i).X;
//				int y = (int)seamCellPoints.elementAt(i).Y;
//				int z = (int)seamCellPoints.elementAt(i).Z;
//				int id = seamSegmentation.getInt(x, y, z );
//				if ( i != (id-1) )
//				{
//					//error:
//					System.err.println( "error wrong id:   " + (i+1) + "   " + id );				
//					allMatch = false;
//				}
//			}
//			System.err.println( seamCellPoints.size() + "  allfound? " + allMatch );
//		}
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

}