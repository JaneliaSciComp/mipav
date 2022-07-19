package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import java.awt.Color;
import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;

import javax.swing.JProgressBar;

//import com.mentorgen.tools.profile.runtime.Profile;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Intersection.IntrSegment3Triangle3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Triangle3f;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;

public class LatticeBuilder {

	public LatticeBuilder() {}

	public LatticeBuilder( ModelImage image, ModelImage seam, ModelImage skin)
	{
		wormImage = image;
		seamSegmentation = seam;
		skinSegmentation = skin;
	}

	public void dispose()
	{
		wormImage = null;
		seamSegmentation = null;
		skinSegmentation = null;
	}

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
	private static final int maxPairDist = 20;		
	public static final float tenMinDist = 2;
	public static final float tenMaxDist = 5;  
	//	private static final float noseP1MinDist = 10;
	//	private static final float noseP1MaxDist = 30;
	private static final int minSequenceMidDist = 5;
	private static final int maxSequenceMidDist = 20;
	private static final int minSequenceDist = 4;
	private static final int maxSequenceDist = 28;
	private static final int sequenceDistDiffLimit = 16;
	//	private static final double sequenceTwistLimit = (Math.PI/2f);
	//	private static final int wormLengthMin = 100;
	//	private static final int wormLengthMax = 140;
	//	private static final int sequenceBendMin = 6;
	//	private static final int sequenceBendMax = 12;
	//	private static final int sequenceTimeLimit = 1000;
	private static final float parallelAngleErrorMax = 3.5f;

	private static final float midPointTestScale = 0.3f;

	private Vector<int []> testSequence = new Vector<int[]>();

	public class Sequence extends Vector<int[]> {}
	public class SequenceList extends Vector<Sequence> {}


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
	public Vector<VOIVector> buildLattice( JProgressBar batchProgress, int imageCount, int numImages, ModelImage image, VOI annotations, VOI nose, String outputDir, boolean wasEdited )
	{
		wormImage = image;

		Vector3f originPt = null;
		for ( int i = annotations.getCurves().size() - 1; i >= 0; i-- )
		{
			VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			// skip extra annotations (origin, nose)
			// use annotations list?
			if ( text.getText().equalsIgnoreCase("origin") || text.getText().contains("nose") || text.getText().contains("Nose") )
			{
				originPt = new Vector3f(text.elementAt(0));
				annotations.getCurves().remove(i);
			}
		}
		final Vector3f origin = (originPt == null) ? null : new Vector3f(originPt);

		//-javaagent:C:\GeometricToolsInc\mipav\src\lib\profile.jar
		//-Dprofile.properties=C:\GeometricToolsInc\mipav\src\lib\profile.properties
		//		Profile.clear();
		//		Profile.start();

		long startTime = System.currentTimeMillis();

		VOI finalPointSets = null;
		SequenceList[] finalSequenceList = null;

		if ( wasEdited )
		{
			finalPointSets = new VOI( (short)0, "clusterList", VOI.POLYLINE, 0);
			VOIContour newCombo = new VOIContour(false);
			VOIContour headPair = new VOIContour(false);
			VOIContour tailPair = new VOIContour(false);
			finalPointSets.getCurves().add(newCombo);
			for ( int i = 0; i < annotations.getCurves().size(); i++ )
			{
				VOIText text = (VOIText) annotations.getCurves().elementAt(i);
				if ( !LatticeModel.match(text.getColor(), Color.red) && !LatticeModel.match(text.getColor(), Color.green)  )
				{
					newCombo.add(new Vector3f(text.elementAt(0)));
				}
				if ( LatticeModel.match(text.getColor(), Color.red) )
				{
					tailPair.add(new Vector3f(text.elementAt(0)));
				}
				if ( LatticeModel.match(text.getColor(), Color.green) )
				{
					headPair.add(new Vector3f(text.elementAt(0)));
				}
			}			


			finalSequenceList = new SequenceList[1];
			LatticeBuilder builder = new LatticeBuilder(wormImage, seamSegmentation, skinSegmentation);				
			builder.setSeamIDs( newCombo );
			finalSequenceList[0] = builder.buildLattice(newCombo, origin, headPair, tailPair, newCombo.size() );	

			for ( int i = 0; i < tailPair.size(); i++ )
			{
				newCombo.add(tailPair.elementAt(i));
			}
			for ( int i = 0; i < headPair.size(); i++ )
			{
				newCombo.add(headPair.elementAt(i));
			}
		}
		else
		{
			final int targetLength = wasEdited ? ((annotations.getCurves().size()%2) == 0) ? annotations.getCurves().size() : (annotations.getCurves().size() - 1) : 20;

			final VOI pointSets = makeGroups(annotations, targetLength);
			System.err.println( "buildLattice outer: " + annotations.getCurves().size() + "   groups: " + pointSets.getCurves().size() );

			int nthreads = ThreadUtil.getAvailableCores();
			final CountDownLatch doneSignalx = new CountDownLatch(nthreads);
			float size = pointSets.getCurves().size();
			final float step2 = size / nthreads;

			final SequenceList[] sequenceList = new SequenceList[pointSets.getCurves().size()+1];
			for (int i = 0; i < nthreads; i++) {
				final int start2 = (int) (step2 * i);
				final int end2 = (int) (step2 * (i + 1));
				final Runnable task = new Runnable() {
					public void run() {
						for ( int j = start2; j < end2; j++ )
						{
							LatticeBuilder builder = new LatticeBuilder(wormImage, seamSegmentation, skinSegmentation);				
							builder.setSeamIDs( pointSets.getCurves().elementAt(j) );
							sequenceList[j] = builder.buildLattice(pointSets.getCurves().elementAt(j), origin, targetLength );

							System.err.println( (j+1) + " / " + pointSets.getCurves().size() + "   " + sequenceList[j].size() );
							//        				System.err.println( (j+1) + " / " + pointSets.getCurves().size() + "   " + sequenceList[j].size() + "                " +
							//        						builder.widthErrorCount + "  " + builder.parallelErrorCount + "  " + builder.midDistErrorCount + "  " +  
							//        						builder.avgWidthErrorCount + "  " + builder.bendSumErrorCount + "  " + 
							//        						builder.intersectionErrorCount + "   " +  builder.latticeImageErrorCount );

						}
						doneSignalx.countDown();
					}
				};
				ThreadUtil.mipavThreadPool.execute(task);
			}
			try {
				doneSignalx.await();
			} catch (final InterruptedException e) {
				gov.nih.mipav.view.MipavUtil.displayError(e.getMessage());
				return null;
			}

			finalPointSets = pointSets;
			finalSequenceList = sequenceList;
		}

		LatticeModel model = new LatticeModel( wormImage );
		model.setSeamCellImage(seamSegmentation);	

		Vector<VOIVector> finalLatticeList = new Vector<VOIVector>();
		// Order the sequences based on how well they fit the lattice parameters:		
		//		sequenceList.add(0, testSequence);

		orderSequences2( startTime, model, origin, finalPointSets, finalSequenceList, finalLatticeList );

		if ( batchProgress != null )
		{
			batchProgress.setValue((int)100);
			batchProgress.update(batchProgress.getGraphics());
		}

		//		System.err.println( finalLatticeList.size() );
		image.unregisterAllVOIs();
		// Save the top 5 lattices found for the user to select the best:
		for ( int i = 0; i < finalLatticeList.size(); i++ )
			//			for ( int i = 0; i < Math.min( 5, finalLatticeList.size()); i++ )
		{
			VOIVector lattice = finalLatticeList.elementAt(i);
			String fileName = outputDir + File.separator + autoLatticeGenerationOutput + (i+1) + File.separator;
			File outputFileDir = new File(fileName);
			if ( !outputFileDir.exists() )
			{
				//				System.err.println( "buildLattice " + fileName);
				outputFileDir.mkdir();
			}

			//			LatticeModel.saveAllVOIsTo(fileName, image);

			VOI latticePoints = new VOI( (short)0, "lattice" + (i+1), VOI.ANNOTATION, 0);
			VOI left = lattice.elementAt(0);
			VOI right = lattice.elementAt(1);
			for ( int j = 0; j < left.getCurves().size(); j++ )
			{
				latticePoints.getCurves().add(left.getCurves().elementAt(j));
				latticePoints.getCurves().add(right.getCurves().elementAt(j));
			}
			LatticeModel.saveAnnotationsAsCSV(fileName, "lattice.csv", latticePoints);
		}

		System.err.println( "done buildLattice = " + AlgorithmBase.computeElapsedTime(startTime) );

		//		Profile.stop();
		//		Profile.setFileName( "profile_out_build_lattice" );
		//		Profile.shutdown();


		model.dispose();
		model = null;
		return finalLatticeList;
	}



	private Vector3f[] midPts;
	private float[] widths;
	private Vector3f pL1;
	private Vector3f pL2;
	private Vector3f pR1;
	private Vector3f pR2;
	private Vector3f leftDir;
	private Vector3f rightDir;
	private Vector3f parallel1;
	private Vector3f parallel2;

	private SequenceList buildLattice( Vector<Vector3f> seamCells, Vector3f origin, int targetLength )
	{
		midPts = new Vector3f[targetLength];
		widths = new float[targetLength];
		pL1 = new Vector3f();
		pL2 = new Vector3f();
		pR1 = new Vector3f();
		pR2 = new Vector3f();
		leftDir = new Vector3f();
		rightDir = new Vector3f();
		parallel1 = new Vector3f();
		parallel2 = new Vector3f();



		int numSteps = 3;
		int step = 1;
		long startTime = System.currentTimeMillis();
		boolean print = false;
		/** Step (1) Attempt to find the tenth pair of seam cells in the lattice. 
		 * The 10th pair is distinct in that it has the smallest between-cell distance */
		Vector<int[]> tenthPairs = new Vector<int[]>();
		Vector<Vector3f> positions = new Vector<Vector3f>();
		for ( int i = 0; i < seamCells.size(); i++ )
		{
			Vector3f pos = new Vector3f(seamCells.elementAt(i));
			pos.scale( VOILatticeManagerInterface.VoxelSize );
			positions.add( pos );
		}

		System.err.println( "Start buildLattice inner " + positions.size() );

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

		if ( tenthPairs.size() == 0 )
		{
			return null;
		}

		// Given a set of potential tenth pairs, remove any that fail the mid-point test:
		//		System.err.println( tenthPairs.size() + " " + tempMin + " " + tempMax );
		if ( tenthPairs.size() > 1 )
		{
			for ( int i = tenthPairs.size() - 1; i >= 0; i-- )
			{
				if ( midPointFail( tenthPairs.elementAt(i), positions ) )
				{
					tenthPairs.remove(i);
				}
				else
				{
					int[] pair = tenthPairs.elementAt(i);
					int index1 = pair[0];
					int index2 = pair[1];
					Vector3f pt1 = positions.elementAt(index1);
					Vector3f pt2 = positions.elementAt(index2);
					if ( !checkSurface(pt1, pt2) )
					{
						if ( print ) System.err.println( "surface check removing " + index1 + " " + index2 );
						tenthPairs.remove(i);
					}
					else if ( !checkSeam(pt1, pt2) )
					{
						if ( print ) System.err.println( "seam check removing " + index1 + " " + index2 );
						tenthPairs.remove(i);
					}
				}
			}
		}



		//		if ( print ) System.err.println( "10th pair " + tenthPairs.size() );
		//		float minIntensity = Float.MAX_VALUE;
		//		int minIndex = -1;
		//		for ( int i = 0; i < tenthPairs.size(); i++ )
		//		{
		//			int[] pair = tenthPairs.elementAt(i);
		//			int index1 = pair[0];
		//			int index2 = pair[1];
		//			Vector3f pt1 = positions.elementAt(index1);
		//			Vector3f pt2 = positions.elementAt(index2);
		//
		//			Vector3f pt1Voxel = new Vector3f(pt1);
		//			pt1Voxel.scale(1f/VOILatticeManagerInterface.VoxelSize);
		//			Vector3f pt1Voxel2 = new Vector3f(pt2);
		//			pt1Voxel2.scale(1f/VOILatticeManagerInterface.VoxelSize);
		//			
		//			
		//			int x = (int) pt1Voxel.X;
		//			int y = (int) pt1Voxel.Y;
		//			int z = (int) pt1Voxel.Z;
		//			float value1 = wormImage.getFloat(x,y,z);
		//			x = (int) pt1Voxel2.X;
		//			y = (int) pt1Voxel2.Y;
		//			z = (int) pt1Voxel2.Z;
		//			float value2 = wormImage.getFloat(x,y,z);
		//			if ( print ) System.err.println( "10th " + (index1 + 1) + " " + (index2 + 1) + " " + value1 + " " + value2 );
		//			
		//			if ( (value1 + value2) < minIntensity )
		//			{
		//				minIntensity = (value1 + value2);
		//				minIndex = i;
		//			}
		//		}
		//
		//		if ( minIndex != -1 )
		//		{
		//			int[] pair = tenthPairs.elementAt(minIndex);
		//			tenthPairs.removeAllElements();
		//			tenthPairs.add(pair);
		//			
		//			if ( print ) System.err.println( "Tenth pair = [" + pair[0] + "," + pair[1] + "]" );
		//		}


		int[] total = new int[]{0};
		int[] max = new int[]{0};
		SequenceList sequenceList = new SequenceList();


		LatticeModel model = new LatticeModel( wormImage );
		model.setSeamCellImage(seamSegmentation);	

		// Loop over all potential tenthPairs:
		for ( int i = 0; i < tenthPairs.size(); i++ )
		{
			int minMaxCount = 0;
			int[] tenthPair = tenthPairs.elementAt(i);
			//			System.err.println( "Tenth pair = [" + tenthPair[0] + "," + tenthPair[1] + "]" );
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

							if ( print ) System.err.print( "Pair " + j + " " + k );
							if (  (distance >= minPairDist) && (distance <= maxPairDist) )
							{
								minMaxCount++;
								// only add pairs that pass the mid-point test:
								if ( !midPointFail(j,k, positions) )
								{
									Vector3f pt1 = positions.elementAt(j);
									Vector3f pt2 = positions.elementAt(k);
									if ( checkSurface(pt1, pt2) )
									{
										if ( checkSeam( pt1, pt2) )
										{
											if ( checkOrigin( pt1, pt2, origin ) )
											{
												pairs[j][k] = 1;
												countJ++;

												if ( print ) System.err.println(" added" );
											}
											else
											{
												if ( print ) System.err.println(" origin check fail");
											}
										}
										else
										{
											if ( print ) System.err.println(" checkSeam fail" );
										}
									}
									else
									{
										if ( print ) System.err.println(" checkSurface fail" );
									}
								}
								else
								{
									if ( print ) System.err.println(" midPointFail fail" );
								}
							}
							else
							{
								if ( print ) System.err.println(" distance fail " + distance + "  " + (distance >= minPairDist) + "  " + (distance <= maxPairDist) );
							}
						}
					}
				}
				if ( (j != tenthPair[0]) && (j != tenthPair[1]) )
				{
					if ( countJ == 0 )
					{
						if ( print ) System.err.println( "   No pairs found for " + j );
						return null;
					}
				}
			}

			boolean pairCount = countPairs(pairs, tenthPair);
			if ( !pairCount )
			{
				if ( print ) System.err.println( "   Initial Pairset " + countPairs(pairs, tenthPair) );
				return null;
			}

			Vector<int[]> pairLists = new Vector<int[]>();
			for ( int j = 0; j < pairs.length; j++ )
			{
				for ( int k = j+1; k < pairs[j].length; k++ )
				{
					if ( pairs[j][k] != -1 )
					{
						pairLists.add( new int[]{j,k} );

						if ( print ) System.err.println( pairLists.size() + "   [" + j + "," + k + "]" );
					}
				}
			}

			if ( print ) System.err.println( pairLists.size() + " " + minMaxCount + " " + positions.size() );
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
			if ( print ) System.err.println( pairLists.size() );

			Vector<int[]> sequence = new Vector<int[]>();
			//			int targetLength = (positions.size() % 2) == 0 ? positions.size() : positions.size() -1;
			//			targetLength = Math.max(0, targetLength);
			//			targetLength = Math.min(20, targetLength);

			//			savePairs(pairLists, positions);
			// Starting at the 10th pair, add pairs to the sequence building it up until the sequence contains 10 pairs:
			sequencePairs( startTime, model, origin, positions, sequence, pairLists, tenthPair, targetLength, total, max, sequenceList );
		}
		if ( print ) System.err.println( "buildLattice inner: time = " + AlgorithmBase.computeElapsedTime(startTime) + " " + sequenceList.size() );

		return sequenceList;
	}

	private SequenceList buildLattice( Vector<Vector3f> seamCells, Vector3f origin, Vector<Vector3f> headPair, Vector<Vector3f> tailPair, int targetLength )
	{
		midPts = new Vector3f[targetLength];
		widths = new float[targetLength];
		pL1 = new Vector3f();
		pL2 = new Vector3f();
		pR1 = new Vector3f();
		pR2 = new Vector3f();
		leftDir = new Vector3f();
		rightDir = new Vector3f();
		parallel1 = new Vector3f();
		parallel2 = new Vector3f();



		int numSteps = 3;
		int step = 1;
		long startTime = System.currentTimeMillis();
		boolean print = false;

		Vector<Vector3f> positions = new Vector<Vector3f>();
		for ( int i = 0; i < seamCells.size(); i++ )
		{
			Vector3f pos = new Vector3f(seamCells.elementAt(i));
			pos.scale( VOILatticeManagerInterface.VoxelSize );
			positions.add( pos );
		}

		System.err.println( "Start buildLattice inner " + positions.size() );

		int[] total = new int[]{0};
		int[] max = new int[]{0};
		SequenceList sequenceList = new SequenceList();


		LatticeModel model = new LatticeModel( wormImage );
		model.setSeamCellImage(seamSegmentation);	


		int minMaxCount = 0;
		//			System.err.println( "Tenth pair = [" + tenthPair[0] + "," + tenthPair[1] + "]" );
		int[][] pairs = new int[positions.size()][positions.size()];
		// Find potential pairs in the remaining set of points:
		for ( int j = 0; j < positions.size(); j++ )
		{
			int countJ = 0;
			for ( int k = 0; k < positions.size(); k++ )
			{
				pairs[j][k] = -1;
				float distance = positions.elementAt(j).distance( positions.elementAt(k) );
				// pairs must be within a minimum and maximum distance thresholds:

				if ( print ) System.err.print( "Pair " + j + " " + k );
				if (  (distance >= minPairDist) && (distance <= maxPairDist) )
				{
					minMaxCount++;
					// only add pairs that pass the mid-point test:
					if ( !midPointFail(j,k, positions) )
					{
						Vector3f pt1 = positions.elementAt(j);
						Vector3f pt2 = positions.elementAt(k);
						if ( checkSurface(pt1, pt2) )
						{
							if ( checkSeam( pt1, pt2) )
							{
								if ( checkOrigin( pt1, pt2, origin ) )
								{
									pairs[j][k] = 1;
									countJ++;

									if ( print ) System.err.println(" added" );
								}
								else
								{
									if ( print ) System.err.println(" origin check fail");
								}
							}
							else
							{
								if ( print ) System.err.println(" checkSeam fail" );
							}
						}
						else
						{
							if ( print ) System.err.println(" checkSurface fail" );
						}
					}
					else
					{
						if ( print ) System.err.println(" midPointFail fail" );
					}
				}
				else
				{
					if ( print ) System.err.println(" distance fail " + distance + "  " + (distance >= minPairDist) + "  " + (distance <= maxPairDist) );
				}
			}
			if ( countJ == 0 )
			{
				if ( print ) System.err.println( "   No pairs found for " + j );
				return null;
			}
		}

		boolean pairCount = countPairs(pairs, null);
		if ( !pairCount )
		{
			if ( print ) System.err.println( "   Initial Pairset " + countPairs(pairs, null) );
			return null;
		}

		Vector<int[]> pairLists = new Vector<int[]>();
		for ( int j = 0; j < pairs.length; j++ )
		{
			for ( int k = j+1; k < pairs[j].length; k++ )
			{
				if ( pairs[j][k] != -1 )
				{
					pairLists.add( new int[]{j,k} );

					if ( print ) System.err.println( pairLists.size() + "   [" + j + "," + k + "]" );
				}
			}
		}

		if ( print ) System.err.println( pairLists.size() + " " + minMaxCount + " " + positions.size() );

		int[] tenthPair = new int[2];
		for ( int i = 0; i < tailPair.size(); i++ )
		{
			Vector3f pos = new Vector3f(tailPair.elementAt(i));
			pos.scale( VOILatticeManagerInterface.VoxelSize );
			positions.add( pos );
			tenthPair[i] = positions.indexOf(pos);
		}
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
		if ( print ) System.err.println( pairLists.size() );

		Vector<int[]> sequence = new Vector<int[]>();
		//			int targetLength = (positions.size() % 2) == 0 ? positions.size() : positions.size() -1;
		//			targetLength = Math.max(0, targetLength);
		//			targetLength = Math.min(20, targetLength);

		//			savePairs(pairLists, positions);
		// Starting at the 10th pair, add pairs to the sequence building it up until the sequence contains 10 pairs:
		sequencePairs( startTime, model, origin, positions, sequence, pairLists, tenthPair, targetLength + 2, total, max, sequenceList );

		if (headPair.size() == 2)
		{
			int[] firstPair = new int[2];
			for ( int i = 0; i < headPair.size(); i++ )
			{
				Vector3f pos = new Vector3f(headPair.elementAt(i));
				pos.scale( VOILatticeManagerInterface.VoxelSize );
				positions.add( pos );
				firstPair[i] = positions.indexOf(pos);
			}
			int[] firstPair2 = new int[]{firstPair[1], firstPair[0]};
			SequenceList newSequenceList = new SequenceList();
			for ( int i = sequenceList.size() -1; i >= 0; i-- )
			{
				Sequence s = sequenceList.elementAt(i);
				s.add(firstPair);	
				Vector<Vector3f> left = new Vector<Vector3f>();
				Vector<Vector3f> right = new Vector<Vector3f>();
				for ( int j = 0; j < s.size(); j++ )
				{
					int[] pair = s.elementAt(j);
					left.add(positions.elementAt(pair[0]));
					right.add(positions.elementAt(pair[1]));
				}
				// test the lattice, if it fits add to the list:
				if ( !testLattice(model, left, right, origin) )
				{
					sequenceList.remove(s);
				}

				s.remove(s.lastElement());
				s.add(firstPair2);	
				left = new Vector<Vector3f>();
				right = new Vector<Vector3f>();
				for ( int j = 0; j < s.size(); j++ )
				{
					int[] pair = s.elementAt(j);
					left.add(positions.elementAt(pair[0]));
					right.add(positions.elementAt(pair[1]));
				}
				// test the lattice, if it fits add to the list:
				if ( testLattice(model, left, right, origin) )
				{
					newSequenceList.add(s);
				}
			}
			sequenceList.addAll(newSequenceList);
		}

		if ( print ) System.err.println( "buildLattice inner: time = " + AlgorithmBase.computeElapsedTime(startTime) + " " + sequenceList.size() );

		return sequenceList;
	}





	private VOI makeGroups( VOI annotations, int targetLength )
	{
		if ( seamSegmentation == null )
		{
			return null;
		}

		System.err.println( "Seam segmentation " + annotations.getCurves().size() );

		Vector<Vector3f> centerList = new Vector<Vector3f>();
		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			// skip extra annotations (origin, nose)
			// use annotations list?
			if ( !text.getText().equalsIgnoreCase("origin") )
			{
				Vector3f pos = new Vector3f(text.elementAt(0));
				centerList.add( pos );
			}
		}

		Vector2d[] values = new Vector2d[centerList.size()];
		int[] foundIDs = new int[centerList.size()];
		for ( int i = 0; i < centerList.size(); i++ )
		{
			foundIDs[i] = -1;
		}
		for ( int i = 0; i < centerList.size(); i++ )
		{
			int x = (int)centerList.elementAt(i).X;
			int y = (int)centerList.elementAt(i).Y;
			int z = (int)centerList.elementAt(i).Z;
			int id = seamSegmentation.getInt(x, y, z );
			float value = wormImage.getFloat(x, y, z );
			values[i] = new Vector2d(value, i);
			if ( foundIDs[i] == (id-1) )
			{
				//error:
				System.err.println( "error: " + x + " " + y + " " + z + "    " + id + " already found!");				
			}
			foundIDs[i] = (id-1);
		}

		Arrays.sort(values);
		//		float median = (float)values[values.length/2].X;
		//		System.err.println( median );

		Vector<Vector3f> coreGroup = new Vector<Vector3f>();
		Vector<Vector3f> extraGroup = new Vector<Vector3f>();
		for ( int i = values.length-1; i >= 0; i-- )
		{
			if ( coreGroup.size() < (targetLength - 2) )
			{
				coreGroup.add( centerList.elementAt( (int)(values[i].Y)) );
				//				System.err.println( "Core " + (int)values[i].Y + "   " + values[i].X );
			}
			else
			{
				extraGroup.add( centerList.elementAt( (int)(values[i].Y)) );
				//				System.err.println( "     " + (int)values[i].Y + "   " + values[i].X );
			}
		}

		VOI clusterLists = new VOI( (short)0, "clusterList", VOI.POLYLINE, 0);
		for ( int i = 0; i < extraGroup.size(); i++ )
		{
			for ( int j = i+1; j < extraGroup.size(); j++ )
			{
				VOIContour newCombo = new VOIContour(false);
				newCombo.add( new Vector3f(extraGroup.elementAt(i)) );
				newCombo.add( new Vector3f(extraGroup.elementAt(j)) );
				for ( int k = 0; k < coreGroup.size(); k++ )
				{
					newCombo.add( new Vector3f(coreGroup.elementAt(k)) );
				}
				setSeamIDs(newCombo);
				boolean pass = findTenthPair(wormImage, newCombo);
				if ( pass )
				{
					clusterLists.getCurves().add(newCombo);
					//					System.err.println( "New Group " + clusterLists.getCurves().size() + "   " + i + "   " + j + "   " + pass  + "   " + newCombo.size() );
				}
			}
		}
		return clusterLists;
	}



	/**
	 * @param pairs  list of potential seam cell pairs
	 * @param size   the total number of seam cells
	 * @return
	 */
	private boolean checkPairs( Vector<int[]> pairs, int size )
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
	private boolean countPairs( final int[][] pairs, final int[] tenthPair )
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
			if ( (tenthPair != null) && (i != tenthPair[0]) && (i != tenthPair[1]) )
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
	 * Measures the total amount of folding in the lattice.
	 * @param left sequence of points along the left-edge of the lattice.
	 * @param right sequence of points along the right-edge of the lattice.
	 * @return the sum of the 'bend' in the lattice as measured from one mid-point to the next.
	 */
	private float measureCurvature( Vector<Vector3f> left, Vector<Vector3f> right )
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


	private float measureParallel( VOI left, VOI right )
	{
		float pAngles = 0;
		for ( int i = 0; i < left.getCurves().size()-1; i++ )
		{
			Vector3f leftDir = Vector3f.sub(left.getCurves().elementAt(i+1).elementAt(0), left.getCurves().elementAt(i).elementAt(0));
			leftDir.normalize();

			Vector3f rightDir = Vector3f.sub(right.getCurves().elementAt(i+1).elementAt(0), right.getCurves().elementAt(i).elementAt(0));
			rightDir.normalize();
			pAngles += leftDir.angle(rightDir);
		}
		return pAngles;
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
	private boolean midPointFail( int pair0, int pair1, Vector<Vector3f> positions )
	{
		// Get the positions of the two seam cells:
		Vector3f p1 = positions.elementAt(pair0);
		Vector3f p2 = positions.elementAt(pair1);
		// calculate the mid point:
		Vector3f midPt = Vector3f.add(p1,p2);
		midPt.scale(0.5f);
		// calculate the distance between the two seam cells and scale:
		float distance = p1.distance(p2);
		distance *= midPointTestScale;
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
	private boolean midPointFail( int[] pair, Vector<Vector3f> positions )
	{
		// Get the positions of the two seam cells:
		Vector3f p1 = positions.elementAt(pair[0]);
		Vector3f p2 = positions.elementAt(pair[1]);
		// calculate the mid point:
		Vector3f midPt = Vector3f.add(p1,p2);
		midPt.scale(0.5f);
		// calculate the distance between the two seam cells and scale:
		float distance = p1.distance(p2);
		distance *= midPointTestScale;
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
	 * @param model model of the worm, based on the lattice.
	 * @param positions seam cell positions
	 * @param sequenceList list of sequences representing lattices
	 * @param finalLatticeList final list of output lattices.
	 */
	private void orderSequences( long startTime, LatticeModel model, VOI seamCellLists, SequenceList[] sequenceList, Vector<VOIVector> finalLatticeList )
	{
		int numSequences = 0;
		for ( int i = 0; i < sequenceList.length; i++ )
		{
			SequenceList sequences = sequenceList[i]; 
			if ( sequences == null )
				continue;
			numSequences += sequences.size();
		}
		System.err.println( "Order Sequences " + numSequences );


		Vector<VOIVector> lattices = new Vector<VOIVector>();
		Vector<Vector2d> latticeImageCounts = new Vector<Vector2d>();
		for ( int i = 0; i < sequenceList.length; i++ )
		{
			SequenceList sequences = sequenceList[i]; 
			if ( sequences == null )
				continue;
			Vector<Vector3f> positions = seamCellLists.getCurves().elementAt(i);

			for ( int j = 0; j < sequences.size(); j++ )
			{
				Vector<int[]> sequence = sequences.elementAt(j);

				VOI left = new VOI((short) 0, "left", VOI.ANNOTATION, (float) Math.random());
				VOI right = new VOI((short) 0, "right", VOI.ANNOTATION, (float) Math.random());
				for ( int k = 0; k < sequence.size(); k++ )
				{
					int[] pair = sequence.elementAt(k);
					Vector3f p1 = new Vector3f(positions.elementAt(pair[0]));
					Vector3f p2 = new Vector3f(positions.elementAt(pair[1]));
					left.getCurves().add(0, new VOIWormAnnotation(p1));
					right.getCurves().add(0, new VOIWormAnnotation(p2));
				}

				float[] intersectionCount = new float[1];
				model.testLatticeConflicts( left, right, intersectionCount);
				if ( intersectionCount[0] < .2 )
				{
					VOIVector lattice = new VOIVector();
					lattice.add(left);
					lattice.add(right);
					lattices.add(lattice);
					latticeImageCounts.add( new Vector2d(intersectionCount[0], lattices.size()-1) );
				}
			}
		}

		if ( latticeImageCounts.size() == 0 )
			return;

		Vector2d[] latticeSort = new Vector2d[latticeImageCounts.size()];
		for ( int i = 0; i < latticeImageCounts.size(); i++ )
		{
			latticeSort[i] = latticeImageCounts.elementAt(i);
		}
		Arrays.sort(latticeSort);
		for ( int i = latticeSort.length -1; i >= 0; i-- )
		{
			VOIVector lattice = lattices.elementAt((int)latticeSort[i].Y);
			finalLatticeList.add(lattice);
			if ( finalLatticeList.size() >= 5 )
			{
				break;
			}
		}
	}


	private void orderSequences2( long startTime, LatticeModel model, Vector3f origin, VOI seamCellLists, SequenceList[] sequenceList, Vector<VOIVector> finalLatticeList )
	{
		int numSequences = 0;
		for ( int i = 0; i < sequenceList.length; i++ )
		{
			SequenceList sequences = sequenceList[i]; 
			if ( sequences == null )
				continue;
			numSequences += sequences.size();
		}
		System.err.println( "Order Sequences 2 " + numSequences );


		Vector<VOIVector> lattices = new Vector<VOIVector>();
		Vector<Vector2d> latticeImageCounts = new Vector<Vector2d>();
		for ( int i = 0; i < sequenceList.length; i++ )
		{
			SequenceList sequences = sequenceList[i]; 
			if ( sequences == null )
				continue;
			Vector<Vector3f> positions = seamCellLists.getCurves().elementAt(i);

			for ( int j = 0; j < sequences.size(); j++ )
			{
				Vector<int[]> sequence = sequences.elementAt(j);

				VOI left = new VOI((short) 0, "left", VOI.ANNOTATION, (float) Math.random());
				VOI right = new VOI((short) 0, "right", VOI.ANNOTATION, (float) Math.random());
				for ( int k = 0; k < sequence.size(); k++ )
				{
					int[] pair = sequence.elementAt(k);
					Vector3f p1 = new Vector3f(positions.elementAt(pair[0]));
					Vector3f p2 = new Vector3f(positions.elementAt(pair[1]));
					left.getCurves().add(0, new VOIWormAnnotation(p1));
					right.getCurves().add(0, new VOIWormAnnotation(p2));
				}
				if ( origin != null )
				{
					Vector3f nose1 = Vector3f.add( origin, left.getCurves().firstElement().elementAt(0));   nose1.scale(0.5f);
					Vector3f nose2 = Vector3f.add( origin, right.getCurves().firstElement().elementAt(0));  nose2.scale(0.5f);					
					left.getCurves().add(0, new VOIWormAnnotation(nose1));
					right.getCurves().add(0, new VOIWormAnnotation(nose2));

					Vector3f leftDir = Vector3f.sub( left.getCurves().firstElement().elementAt(0), origin );     leftDir.normalize();   leftDir.scale(2);
					Vector3f rightDir = Vector3f.sub( right.getCurves().firstElement().elementAt(0), origin );   rightDir.normalize();  rightDir.scale(2);
					nose1 = Vector3f.add( origin, leftDir);
					nose2 = Vector3f.add( origin, rightDir);					
					left.getCurves().add(0, new VOIWormAnnotation(nose1));
					right.getCurves().add(0, new VOIWormAnnotation(nose2));
				}

				//				float[] avgVals = model.testLatticeImage(left, right, minSeamValue);
				//				if ( avgVals[0] != -1 )
				{
					VOIVector lattice = new VOIVector();
					lattice.add(left);
					lattice.add(right);
					lattices.add(lattice);

					latticeImageCounts.add( new Vector2d( measureParallel(left,right), lattices.size()-1) );


					//					latticeImageCounts.add( new Vector2d( avgVals[0], lattices.size()-1) );
					//					latticeImageCounts.add( new Vector2d( Math.abs(avgVals[0] - avgVals[1]), lattices.size()-1) );
					//					System.err.println( lattices.size() + "     " + avgVals[0] + "   " + avgVals[1] + "    " + (avgVals[0] + avgVals[1]) + "    " + Math.abs(avgVals[0] - avgVals[1]));
					//					latticeImageCounts.add( new Vector2d((avgVals[0] / avgVals[1]), lattices.size()-1) );

					//					finalLatticeList.add(lattice);
					//					if ( finalLatticeList.size() >= 5 )
					//					{
					//						return;
					//					}
				}
			}
		}

		if ( latticeImageCounts.size() == 0 )
			return;

		Vector2d[] latticeSort = new Vector2d[latticeImageCounts.size()];
		for ( int i = 0; i < latticeImageCounts.size(); i++ )
		{
			latticeSort[i] = latticeImageCounts.elementAt(i);
		}
		Arrays.sort(latticeSort);
		//		for ( int i = 0; i < latticeSort.length; i++ )
		//		{
		//			System.err.println( "Parallel sum: " + latticeSort[i].X );
		//		}
		for ( int i = 0; i < latticeSort.length; i++ )
			//		for ( int i = latticeSort.length -1; i >= 0; i-- )
		{
			VOIVector lattice = lattices.elementAt((int)latticeSort[i].Y);
			float[] intersectionCount = new float[1];
			model.testLatticeConflicts( lattice.elementAt(0), lattice.elementAt(1), intersectionCount);
			if ( intersectionCount[0] < .2 )
			{
				float[] avgVals = model.testLatticeImage(lattice.elementAt(0), lattice.elementAt(1));
				System.err.println( finalLatticeList.size() + "   " + (int)latticeSort[i].Y + "   " + latticeSort[i].X + "     " +  avgVals[0] + "     " + intersectionCount[0] );
				finalLatticeList.add(lattice);
			}


			if ( finalLatticeList.size() >= 15 )
			{
				break;
			}
		}
		if ( finalLatticeList.size() == 0 )
		{
			for ( int i = latticeSort.length -1; i >= 0; i-- )
			{
				VOIVector lattice = lattices.elementAt((int)latticeSort[i].Y);
				finalLatticeList.add(lattice);
				if ( finalLatticeList.size() >= 5 )
				{
					break;
				}
			}			
		}
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
	private void sequencePairs( long startTime, LatticeModel model, Vector3f origin, Vector<Vector3f> positions, Vector<int[]> sequence, Vector<int[]> pairs, int[] lastPair, int count, int[] total, int[] max, SequenceList sequenceList )
	{		
		Sequence newSequence = new Sequence();
		newSequence.addAll(sequence);
		newSequence.add(lastPair);

		if ( (newSequence.size() == 3) && !testLatticeInProgress(newSequence, positions) )
		{
			return;
		}
		if ( (newSequence.size() > 3) && !testLastPair(newSequence, positions) )
		{
			return;
		}

		// Test if the current sequence is the correct length:
		boolean allFound = (count == newSequence.size() * 2);
		if ( allFound )
		{
			// current sequence is the desired length make into a lattice:
			max[0]++;
			Vector<Vector3f> left = new Vector<Vector3f>();
			Vector<Vector3f> right = new Vector<Vector3f>();
			//			boolean testFound = true;
			for ( int i = 0; i < newSequence.size(); i++ )
			{
				int[] pair = newSequence.elementAt(i);
				left.add(positions.elementAt(pair[0]));
				right.add(positions.elementAt(pair[1]));

				//				if ( (pair[1] != testSequence.elementAt(i)[0]) || (pair[0] != testSequence.elementAt(i)[1]) )
				//				{
				//					testFound = false;
				//				}
			}
			//			if ( testFound )
			//			{
			//				System.err.println( testFound );
			//			}
			// test the lattice, if it fits add to the list:
			if ( testLattice(model, left, right, origin) )
			{
				total[0]++;
				sequenceList.add(newSequence);
				//				System.err.println( "    adding sequence " + total[0] + "  " + max[0]);
			}
			// return, do not add more to this sequence:
			return;
		}
		if ( count <= (newSequence.size() * 2) )
		{
			// new sequence is too long, return failure:
			return;
		}
		//		double elapsedTime = AlgorithmBase.computeElapsedTime(startTime);
		//		if ( elapsedTime > sequenceTimeLimit )
		//		{
		//			// if too much time has elapsed, return:
		////			System.err.println( "too much time" );
		//			return;
		//		}

		// Add a new 'rung' to the lattice:
		Vector<int[]> newPairs = new Vector<int[]>();
		newPairs.addAll(pairs);
		newPairs.remove(lastPair);
		for ( int i = newPairs.size() - 1; i >= 0; i-- )
		{
			int[] pair = newPairs.elementAt(i);
			if ( (lastPair[0] == pair[0]) || (lastPair[0] == pair[1]) || (lastPair[1] == pair[0]) || (lastPair[1] == pair[1]) )
			{
				newPairs.remove(pair);
			}			
		}
		int pairGoal = count / 2;
		if ( newSequence.size() + newPairs.size() < pairGoal )
		{
			return;
		}
		for ( int i = 0; i < newPairs.size(); i++ )
		{
			int[] pair = newPairs.elementAt(i);

			// Calculate angles from the last pair in the sequence to the new pair, this determines which side (left or right) to put
			// the points of the new pair on.
			Vector3f edge00 = Vector3f.sub( positions.elementAt(pair[0]), positions.elementAt(lastPair[0]) );  float L00 = edge00.normalize();
			Vector3f edge11 = Vector3f.sub( positions.elementAt(pair[1]), positions.elementAt(lastPair[1]) );  float L11 = edge11.normalize();
			//			float angle1 = edge00.angle(edge11);

			Vector3f edge01 = Vector3f.sub( positions.elementAt(pair[0]), positions.elementAt(lastPair[1]) );  float L01 = edge01.normalize();
			Vector3f edge10 = Vector3f.sub( positions.elementAt(pair[1]), positions.elementAt(lastPair[0]) );  float L10 = edge10.normalize();
			//			float angle2 = edge01.angle(edge10);
			float diff = Math.abs(L01-L10);


			boolean path1 = false;
			boolean path2 = false;
			if ( (L00 < L01) && (L11 < L10) )
			{
				path1 = true;
			}
			else if ( (L00 > L01) && (L11 > L10) )
			{
				path2 = true;
			}
			else
			{
				path1 = true;
				path2 = true;
			}
			//			float min = L00;
			//			if ( L11 < min )
			//			{
			//				min = L11;
			//			}
			//			if ( L01 < min )
			//			{
			//				min = L01;
			//				path1 = false;
			//			}
			//			if ( L10 < min )
			//			{
			//				min = L10;
			//				path1 = false;
			//			}

			if ( path1 )
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
						sequencePairs( startTime, model, origin, positions, newSequence, newPairs, pair, count, total, max, sequenceList );
					}
				}
			}
			if ( path2 )
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
						sequencePairs( startTime, model, origin, positions, newSequence, newPairs, new int[]{pair[1],pair[0]}, count, total, max, sequenceList );
					}
				}
			}
		}

	}

	public boolean testLattice( ModelImage image, Vector<Vector3f> leftIn, Vector<Vector3f> rightIn )
	{
		wormImage = image;


		Vector<Vector3f> left = new Vector<Vector3f>();
		Vector<Vector3f> right = new Vector<Vector3f>();
		for ( int i = 0; i < leftIn.size(); i++ )
		{
			Vector3f p1 = new Vector3f(leftIn.elementAt(i));
			Vector3f p2 = new Vector3f(rightIn.elementAt(i));
			p1.scale(VOILatticeManagerInterface.VoxelSize);
			p2.scale(VOILatticeManagerInterface.VoxelSize);
			left.add(p1);
			right.add(p2);
		}

		boolean pass = true;
		//		for ( int i = 2; i < left.size(); i++ )
		//		{
		//			float distLM2 = left.elementAt(i).distance(right.elementAt(i-2));
		//			float distLM1 = left.elementAt(i).distance(left.elementAt(i-1));
		//			if ( distLM2 < distLM1 )
		//			{
		//				System.err.println("right too close to left " + i+ "  " + distLM2 + "    " + distLM1 );
		//				pass = false;
		//			}
		//			
		//			float distRM2 = right.elementAt(i).distance(left.elementAt(i-2));
		//			float distRM1 = right.elementAt(i).distance(right.elementAt(i-1));
		//			if ( distRM2 < distRM1 )
		//			{
		//				System.err.println("left too close to right " + i + "  " + distRM2 + "    " + distRM1 );
		//				pass =  false;
		//			}
		//		}

		//		System.err.println( "pair" + "\t" + "diff" + "\t" + "midDistance" +  "\t" + "L00" + "\t" + "R00"   );
		for ( int i = 0; i < left.size()-1; i++ )
		{
			Vector3f ptL = left.elementAt(i);
			Vector3f ptL1 = left.elementAt(i+1);
			Vector3f ptR = right.elementAt(i);
			Vector3f ptR1 = right.elementAt(i+1);

			Vector3f edge00 = Vector3f.sub( ptL, ptL1 );  float L00 = edge00.normalize();
			Vector3f edge11 = Vector3f.sub( ptR, ptR1 );  float R00 = edge11.normalize();

			Vector3f edge00A = Vector3f.sub( ptL, ptR1 );  float L00A = edge00A.normalize();
			Vector3f edge11A = Vector3f.sub( ptR, ptL1 );  float R00A = edge11A.normalize();


			boolean path1 = false;
			boolean path2 = false;
			if ( (L00 < L00A) && (R00 < R00A) )
			{
				path1 = true;
			}
			else if ( (L00 > L00A) && (R00 > R00A) )
			{
				path2 = true;
			}
			else
			{
				path1 = true;
				path2 = true;
			}


			Vector3f edge1 = Vector3f.sub(ptR, ptL);      edge1.normalize();
			Vector3f edgeA = Vector3f.sub(ptR1, ptL1);    edgeA.normalize();
			Vector3f edgeB = Vector3f.sub(ptL1, ptR1);    edgeB.normalize();

			//			System.err.print( diff + "  " + angle1 + "  " + angle2 + "     " + angleA + "  " + angleB + "  " );

			//			float path1 = L00 + R00;
			//			float path2 = L00A + R00A;

			Vector3f midPt = Vector3f.add(ptL, ptR);
			midPt.scale(0.5f);
			Vector3f midPtSequence = Vector3f.add(ptL1, ptR1);
			midPtSequence.scale(0.5f);
			float midDistance = midPt.distance(midPtSequence);
			float diff = Math.abs(L00-R00);

			//			System.err.println( i + "\t" + diff + "\t" + midDistance +  "\t" + L00 + "\t" + R00   );



			if ( path1 && (diff <= sequenceDistDiffLimit) && (midDistance > minSequenceMidDist) && (midDistance < maxSequenceMidDist) && (L00 > minSequenceDist) && (L00 < maxSequenceDist) && (R00 > minSequenceDist) && (R00 < maxSequenceDist))
			{		
			}
			else
			{
				System.err.println( i + "  " + path1 + "   " + (diff <= sequenceDistDiffLimit) + "   " + (midDistance > minSequenceMidDist) + "   " + (midDistance < maxSequenceMidDist) + "   " +
						(L00 > minSequenceDist) + "   " + (L00 < maxSequenceDist) + "   " + (R00 > minSequenceDist) + "    " +  (R00 < maxSequenceDist) );

				pass = false;	
			}

		}



		Vector3f pL1 = new Vector3f();
		Vector3f pL2 = new Vector3f();
		Vector3f pR1 = new Vector3f();
		Vector3f pR2 = new Vector3f();
		for ( int i = 0; i < left.size()-1; i++ )
		{
			pL1.copy(left.elementAt(i));       pL1.scale(1f/VOILatticeManagerInterface.VoxelSize);
			pL2.copy(left.elementAt(i+1));     pL2.scale(1f/VOILatticeManagerInterface.VoxelSize);
			pR1.copy(right.elementAt(i));      pR1.scale(1f/VOILatticeManagerInterface.VoxelSize);
			pR2.copy(right.elementAt(i+1));    pR2.scale(1f/VOILatticeManagerInterface.VoxelSize);

			Vector3f edgeL = Vector3f.sub( pL2, pL1 );  edgeL.normalize();
			Vector3f edgeR = Vector3f.sub( pR2, pR1 );  edgeR.normalize();
			float angle = edgeL.angle(edgeR);
			if ( angle > sixtyRad )
			{
				parallelErrorCount++;
				pass = false;
			}

			Vector3f parallel1 = Vector3f.sub(pL1, pR1); parallel1.normalize();
			Vector3f parallel2 = Vector3f.sub(pL2, pR2); parallel2.normalize();
			angle = parallel1.angle(parallel2);
			if ( angle > onetwentyRad )
			{
				parallelErrorCount++;
				pass = false;
			}
		}

		for ( int i = 1; i < left.size()-1; i++ )
		{
			float distM1 =   Math.round(left.elementAt(i-1).distance(right.elementAt(i-1)));
			float dist = 2 + Math.round(left.elementAt(i).distance(right.elementAt(i)));
			float distP1 =   Math.round(left.elementAt(i+1).distance(right.elementAt(i+1)));
			//			System.err.println( i + " " + dist + " " + distM1 + " " + distP1 );
			if ( (dist < distM1) && (dist < distP1) )
			{
				System.err.println( i + " widthError " + dist + " " + distM1 + " " + distP1 );
				pass = false;
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
					System.err.println( i + " midDistErrorCount " +  mid1.distance(mid2) + " " + distance );
					pass = false;
				}
			}
		}


		//		float length = 0;
		//		// Check the sequence distances between midpoints
		//		// the distance expected ranges vary based on the pair number:
		//		for ( int i = 0; i < left.size() - 1; i++ )
		//		{
		//			Vector3f mid1 = Vector3f.add(left.elementAt(i), right.elementAt(i));     mid1.scale(0.5f);
		//			Vector3f mid2 = Vector3f.add(left.elementAt(i+1), right.elementAt(i+1)); mid2.scale(0.5f);
		//			float sequenceDistance = mid1.distance(mid2);
		//			length += sequenceDistance;
		//			if ( (i <= 3) && ((sequenceDistance < 5) || (sequenceDistance > 26)) )
		//			{
		//				System.err.println( i + " sequenceDistErrorCount " + sequenceDistance );
		//				pass = false;
		//			}
		//			if ( (i > 3) && (i <= 6) && ((sequenceDistance < 5) || (sequenceDistance > 16)) )
		//			{
		//				System.err.println( i + " sequenceDistErrorCount " + sequenceDistance );
		//				pass = false;
		//			}
		//			if ( (i == 7) && ((sequenceDistance < 10) || (sequenceDistance > 26)) )
		//			{
		//				System.err.println( i + " sequenceDistErrorCount " + sequenceDistance );
		//				pass = false;
		//			}
		//			if ( (i == 8) && ((sequenceDistance < 5) || (sequenceDistance > 16)) )
		//			{
		//				System.err.println( i + " sequenceDistErrorCount " + sequenceDistance );
		//				pass = false;
		//			}
		//		}
		//		
		//		// Check total length:
		//		if ( (length < wormLengthMin) || (length > wormLengthMax) )
		//		{
		//			System.err.println( "wormLengthErrorCount " + length + "  " + wormLengthMin + "   " + wormLengthMax );
		//			pass = false;
		//		}		

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
		if ( !((avgWidthFirst4 > avgWidthLast5) && (maxIndex >= 4) && (minIndex <= 4)) )
		{			
			System.err.println( "avgWidthErrorCount " + avgWidthFirst4 + "  " + avgWidthLast5  );
			pass = false;
		}

		//		float bendSum = measureCurvature(left, right);
		//		if ( (bendSum < sequenceBendMin) || (bendSum > sequenceBendMax) )
		//		{
		//			System.err.println( "bendSumErrorCount " + bendSum + "  " + sequenceBendMin + "  " + sequenceBendMax  );
		//			pass = false;
		//		}





		VOI leftImage = new VOI((short) 0, "left", VOI.ANNOTATION, (float) Math.random());
		VOI rightImage = new VOI((short) 0, "right", VOI.ANNOTATION, (float) Math.random());
		float totalAngle = 0;
		for ( int i = 0; i < left.size(); i++ )
		{
			Vector3f p1 = new Vector3f(left.elementAt(i));
			Vector3f p2 = new Vector3f(right.elementAt(i));
			p1.scale(1f/VOILatticeManagerInterface.VoxelSize);
			p2.scale(1f/VOILatticeManagerInterface.VoxelSize);
			leftImage.getCurves().add(0, new VOIWormAnnotation(p1));
			rightImage.getCurves().add(0, new VOIWormAnnotation(p2));

			if ( i > 0 )
			{
				Vector3f dirL = Vector3f.sub(leftImage.getCurves().elementAt(0).elementAt(0), leftImage.getCurves().elementAt(1).elementAt(0));
				Vector3f dirR = Vector3f.sub(rightImage.getCurves().elementAt(0).elementAt(0), rightImage.getCurves().elementAt(1).elementAt(0));
				dirL.normalize();
				dirR.normalize();
				float angle = dirL.angle(dirR);
				totalAngle += angle;
			}
		}
		if ( totalAngle > parallelAngleErrorMax )
		{
			return false;
		}


		wormImage.unregisterAllVOIs();
		LatticeModel model = new LatticeModel(wormImage);
		model.setSeamCellImage(seamSegmentation);
		float[] avgVals = model.testLatticeImage(leftImage, rightImage);
		wormImage.unregisterAllVOIs();
		if ( avgVals[0] == -1)
		{
			System.err.println( "latticeImageErrorCount " + avgVals[0] + "  " + avgVals[1] + "  " + (avgVals[0] / avgVals[1])  );
			pass = false;
		}
		//			float diff = Math.abs(avgVals[0] - avgVals[1]);
		//			if ( (diff > avgVals[0]) || (diff > avgVals[1]) )
		//			{
		//				pass = false;
		//			}

		float[] intersectionCount = new float[1];
		model.testLatticeConflicts( leftImage, rightImage, intersectionCount);
		if ( intersectionCount[0] >= .2 )
		{
			pass = false;
		}


		return pass;
	}

	int widthErrorCount = 0;
	int parallelErrorCount = 0;
	int midDistErrorCount = 0;
	int sequenceDistErrorCount = 0;
	int wormLengthErrorCount = 0;
	int avgWidthErrorCount = 0;
	int bendSumErrorCount = 0;
	int intersectionErrorCount = 0;
	int latticeImageErrorCount = 0;


	private static float inverseVoxel = 1f/VOILatticeManagerInterface.VoxelSize;
	private static float sixtyRad = (float)Math.PI/3f;
	private static float onetwentyRad = (float)(120f*(Math.PI/180f));
	private boolean testLatticeInProgress( Vector<int[]> newSequence, Vector<Vector3f> positions )
	{

		//		for ( int i = 2; i < newSequence.size(); i++ )
		//		{
		//			int[] pairM2 = newSequence.elementAt(i-2);
		//			int[] pairM1 = newSequence.elementAt(i-1);
		//			int[] pair   = newSequence.elementAt(i);
		//			float distLM2 = positions.elementAt(pair[0]).distance(positions.elementAt(pairM2[0]));
		//			float distLM1 = positions.elementAt(pair[0]).distance(positions.elementAt(pairM1[0]));
		//			if ( distLM2 < distLM1 )
		//			{
		//				return false;
		//			}
		//			
		//			float distRM2 = positions.elementAt(pair[1]).distance(positions.elementAt(pairM2[1]));
		//			float distRM1 = positions.elementAt(pair[1]).distance(positions.elementAt(pairM1[1]));
		//			if ( distRM2 < distRM1 )
		//			{
		//				return false;
		//			}
		//		}

		for ( int i = 1; i < newSequence.size()-1; i++ )
		{
			int[] pairM1 = newSequence.elementAt(i-1);
			int[] pair   = newSequence.elementAt(i);
			int[] pairP1 = newSequence.elementAt(i+1);
			float distM1 =   Math.round(positions.elementAt(pairM1[0]).distance(positions.elementAt(pairM1[1])));
			float dist = 2 + Math.round(positions.elementAt(  pair[0]).distance(positions.elementAt(  pair[1])));
			float distP1 =   Math.round(positions.elementAt(pairP1[0]).distance(positions.elementAt(pairP1[1])));
			if ( (dist < distM1) && (dist < distP1) )
			{
				widthErrorCount++;
				return false;
			}
		}

		for ( int i = 0; i < newSequence.size(); i++ )
		{
			int[] pair = newSequence.elementAt(i);
			midPts[i] = Vector3f.add(positions.elementAt(pair[0]), positions.elementAt(pair[1])); midPts[i].scale(0.5f);
			// distance measures the width of the worm at this point in the lattice model:
			widths[i] = positions.elementAt(pair[0]).distance(positions.elementAt(pair[1]));
			widths[i] /= 2;
		}

		for ( int i = 0; i < newSequence.size(); i++ )
		{
			for ( int j = i+1; j < newSequence.size(); j++ )
			{
				float midDist = midPts[i].distance(midPts[j]);
				if ( (midDist < widths[i]) || (midDist < widths[j]) )
				{
					midDistErrorCount++;
					return false;
				}
			}
		}

		float totalPAngle = 0;
		for ( int i = 0; i < newSequence.size()-1; i++ )
		{
			int[] pair = newSequence.elementAt(i);
			int[] pairP1 = newSequence.elementAt(i+1);
			pL1.copy(positions.elementAt(  pair[0]));       pL1.scale(inverseVoxel);
			pL2.copy(positions.elementAt(pairP1[0]));       pL2.scale(inverseVoxel);
			pR1.copy(positions.elementAt(  pair[1]));       pR1.scale(inverseVoxel);
			pR2.copy(positions.elementAt(pairP1[1]));       pR2.scale(inverseVoxel);

			leftDir.copy( pL2);  leftDir.sub( pL1); leftDir.normalize();
			rightDir.copy(pR2);  rightDir.sub(pR1); rightDir.normalize();
			float angle = leftDir.angle(rightDir);
			if ( angle > sixtyRad )
			{
				parallelErrorCount++;
				return false;
			}
			totalPAngle += angle;
			parallel1.copy(pL1); parallel1.sub(pR1); parallel1.normalize();
			parallel2.copy(pL2); parallel2.sub(pR2); parallel2.normalize();
			angle = parallel1.angle(parallel2);
			if ( angle > onetwentyRad )
			{
				parallelErrorCount++;
				return false;
			}
		}
		if ( totalPAngle > parallelAngleErrorMax )
		{
			return false;
		}
		return true;
	}


	private boolean testLastPair( Vector<int[]> newSequence, Vector<Vector3f> positions )
	{

		//		for ( int i = newSequence.size()-1; i < newSequence.size(); i++ )
		//		{
		//			int[] pairM2 = newSequence.elementAt(i-2);
		//			int[] pairM1 = newSequence.elementAt(i-1);
		//			int[] pair   = newSequence.elementAt(i);
		//			float distLM2 = positions.elementAt(pair[0]).distance(positions.elementAt(pairM2[0]));
		//			float distLM1 = positions.elementAt(pair[0]).distance(positions.elementAt(pairM1[0]));
		//			if ( distLM2 < distLM1 )
		//			{
		//				return false;
		//			}
		//			
		//			float distRM2 = positions.elementAt(pair[1]).distance(positions.elementAt(pairM2[1]));
		//			float distRM1 = positions.elementAt(pair[1]).distance(positions.elementAt(pairM1[1]));
		//			if ( distRM2 < distRM1 )
		//			{
		//				return false;
		//			}
		//		}

		int start = Math.max(0, newSequence.size()-3);		
		for ( int i = start; i < newSequence.size()-1; i++ )
		{
			int[] pairM1 = newSequence.elementAt(i-1);
			int[] pair   = newSequence.elementAt(i);
			int[] pairP1 = newSequence.elementAt(i+1);
			float distM1 =   Math.round(positions.elementAt(pairM1[0]).distance(positions.elementAt(pairM1[1])));
			float dist = 2 + Math.round(positions.elementAt(  pair[0]).distance(positions.elementAt(  pair[1])));
			float distP1 =   Math.round(positions.elementAt(pairP1[0]).distance(positions.elementAt(pairP1[1])));
			if ( (dist < distM1) && (dist < distP1) )
			{
				widthErrorCount++;
				return false;
			}
		}

		for ( int i = 0; i < newSequence.size(); i++ )
		{
			int[] pair = newSequence.elementAt(i);
			midPts[i] = Vector3f.add(positions.elementAt(pair[0]), positions.elementAt(pair[1])); midPts[i].scale(0.5f);
			// distance measures the width of the worm at this point in the lattice model:
			widths[i] = positions.elementAt(pair[0]).distance(positions.elementAt(pair[1]));
			widths[i] /= 2;
		}
		for ( int i = 0; i < newSequence.size()-1; i++ )
		{
			float midDist = midPts[i].distance(midPts[newSequence.size()-1]);
			if ( (midDist < widths[i]) || (midDist < widths[newSequence.size()-1]) )
			{
				midDistErrorCount++;
				return false;
			}				
		}


		start = Math.max(0, newSequence.size()-2);
		for ( int i = start; i < newSequence.size()-1; i++ )
		{
			int[] pair   = newSequence.elementAt(i);
			int[] pairP1 = newSequence.elementAt(i+1);
			pL1.copy(positions.elementAt(  pair[0]));       pL1.scale(inverseVoxel);
			pL2.copy(positions.elementAt(pairP1[0]));       pL2.scale(inverseVoxel);
			pR1.copy(positions.elementAt(  pair[1]));       pR1.scale(inverseVoxel);
			pR2.copy(positions.elementAt(pairP1[1]));       pR2.scale(inverseVoxel);

			leftDir.copy( pL2);  leftDir.sub( pL1); leftDir.normalize();
			rightDir.copy(pR2);  rightDir.sub(pR1); rightDir.normalize();
			float angle = leftDir.angle(rightDir);
			if ( angle > sixtyRad )
			{
				parallelErrorCount++;
				return false;
			}
			parallel1.copy(pL1); parallel1.sub(pR1); parallel1.normalize();
			parallel2.copy(pL2); parallel2.sub(pR2); parallel2.normalize();
			angle = parallel1.angle(parallel2);
			if ( angle > onetwentyRad )
			{
				parallelErrorCount++;
				return false;
			}
		}

		return true;
	}



	/**
	 * Checks the lattice based on observations of the embryonic worm and compares the lattice to threshold values.
	 * Return true if the lattice fits within bounds.
	 * @param left the positions of the lattice along the left-edge
	 * @param right the positions of the lattice along the right-edge
	 * @param nose nose point of the worm (may be empty)
	 * @return true if the lattice fits within threshold, false otherwise.
	 */
	private boolean testLattice( LatticeModel model, Vector<Vector3f> left, Vector<Vector3f> right, Vector3f origin )
	{
		// Left and right sides must contain the same number of points:
		if ( (left.size() != right.size()) || (left.size() == 0) )
		{
			return false;
		}


		//		float length = 0;
		//		// Check the sequence distances between midpoints
		//		// the distance expected ranges vary based on the pair number:
		//		for ( int i = 0; i < left.size() - 1; i++ )
		//		{
		//			Vector3f mid1 = Vector3f.add(left.elementAt(i), right.elementAt(i));     mid1.scale(0.5f);
		//			Vector3f mid2 = Vector3f.add(left.elementAt(i+1), right.elementAt(i+1)); mid2.scale(0.5f);
		//			float sequenceDistance = mid1.distance(mid2);
		//			length += sequenceDistance;
		//			if ( (i <= 3) && ((sequenceDistance < 5) || (sequenceDistance > 26)) )
		//			{
		//				sequenceDistErrorCount++;
		//				return false;
		//			}
		//			if ( (i > 3) && (i <= 6) && ((sequenceDistance < 5) || (sequenceDistance > 16)) )
		//			{
		//				sequenceDistErrorCount++;
		//				return false;
		//			}
		//			if ( (i == 7) && ((sequenceDistance < 10) || (sequenceDistance > 26)) )
		//			{
		//				sequenceDistErrorCount++;
		//				return false;
		//			}
		//			if ( (i == 8) && ((sequenceDistance < 5) || (sequenceDistance > 16)) )
		//			{
		//				sequenceDistErrorCount++;
		//				return false;
		//			}
		//		}
		//		
		//		// Check total length:
		//		if ( (length < wormLengthMin) || (length > wormLengthMax) )
		//		{
		//			wormLengthErrorCount++;
		//			return false;
		//		}		

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
		if ( !((avgWidthFirst4 > avgWidthLast5) && (maxIndex >= 4) && (minIndex <= 4)) )
		{			
			avgWidthErrorCount++;
			return false;
		}

		//		float bendSum = measureCurvature(left, right);
		//		if ( (bendSum < sequenceBendMin) || (bendSum > sequenceBendMax) )
		//		{
		//			bendSumErrorCount++;
		//			return false;
		//		}

		// convert to voxel space:
		VOI leftImage = new VOI((short) 0, "left", VOI.ANNOTATION, (float) Math.random());
		VOI rightImage = new VOI((short) 0, "right", VOI.ANNOTATION, (float) Math.random());
		float totalAngle = 0;
		for ( int i = 0; i < left.size(); i++ )
		{
			Vector3f p1 = new Vector3f(left.elementAt(i));
			Vector3f p2 = new Vector3f(right.elementAt(i));
			p1.scale(1f/VOILatticeManagerInterface.VoxelSize);
			p2.scale(1f/VOILatticeManagerInterface.VoxelSize);
			leftImage.getCurves().add(0, new VOIWormAnnotation(p1));
			rightImage.getCurves().add(0, new VOIWormAnnotation(p2));

			if ( i > 0 )
			{
				Vector3f dirL = Vector3f.sub(leftImage.getCurves().elementAt(0).elementAt(0), leftImage.getCurves().elementAt(1).elementAt(0));
				Vector3f dirR = Vector3f.sub(rightImage.getCurves().elementAt(0).elementAt(0), rightImage.getCurves().elementAt(1).elementAt(0));
				dirL.normalize();
				dirR.normalize();
				float angle = dirL.angle(dirR);
				totalAngle += angle;
			}
		}
		if ( totalAngle > parallelAngleErrorMax )
		{
			return false;
		}

		if ( origin != null )
		{
			Vector3f nose1 = Vector3f.add( origin, leftImage.getCurves().firstElement().elementAt(0));   nose1.scale(0.5f);
			Vector3f nose2 = Vector3f.add( origin, rightImage.getCurves().firstElement().elementAt(0));  nose2.scale(0.5f);

			leftImage.getCurves().add(0, new VOIWormAnnotation(nose1));
			rightImage.getCurves().add(0, new VOIWormAnnotation(nose2));
		}

		IntrSegment3Triangle3f intersectionTest = new IntrSegment3Triangle3f();
		for ( int i = 0; i < left.size() -1; i++ )
		{
			Triangle3f tri1 = new Triangle3f(left.elementAt(i), right.elementAt(i), right.elementAt(i+1) );
			Triangle3f tri2 = new Triangle3f(left.elementAt(i), right.elementAt(i+1), left.elementAt(i+1) );

			for ( int j = 0; j < left.size() - 1; j++ )
			{
				if ( (j != i) && (j != i+1) && (j != i-1) )
				{
					Segment3f seg1 = new Segment3f(left.elementAt(j), left.elementAt(j+1) );
					intersectionTest.Segment = seg1;
					intersectionTest.Triangle = tri1;
					if ( intersectionTest.Test() )
					{
						intersectionErrorCount++;
						return false;
					}
					intersectionTest.Triangle = tri2;
					if ( intersectionTest.Test() )
					{
						intersectionErrorCount++;
						return false;
					}

					Segment3f seg2 = new Segment3f(right.elementAt(j), right.elementAt(j+1) );
					intersectionTest.Segment = seg2;
					intersectionTest.Triangle = tri1;
					if ( intersectionTest.Test() )
					{
						intersectionErrorCount++;
						return false;
					}
					intersectionTest.Triangle = tri2;
					if ( intersectionTest.Test() )
					{
						intersectionErrorCount++;
						return false;
					}
				}
			}
		}


		// test against seam segmentation image:
		wormImage.unregisterAllVOIs();
		float[] avgVals = model.testLatticeImage(leftImage, rightImage);
		wormImage.unregisterAllVOIs();
		if ( avgVals[0] == -1)
		{
			latticeImageErrorCount++;
			return false;
		}
		//		float diff = Math.abs(avgVals[0] - avgVals[1]);
		//		if ( (diff > avgVals[0]) || (diff > avgVals[1]) )
		//		{
		//			return false;
		//		}

		return true;
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
	private VOI makeLattice( Vector<Vector3f> positions, Vector<Vector3f> nose, int index, Vector<int[]> sequence )
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

	private ModelImage wormImage = null;
	private ModelImage seamSegmentation = null;
	private ModelImage skinSegmentation = null;

	public void setSeamImage(ModelImage image)
	{
		seamSegmentation = image;
	}

	public void setSkinImage(ModelImage image)
	{
		skinSegmentation = image;
	}

	private boolean checkSurface( Vector3f pt1, Vector3f pt2 )
	{
		if ( skinSegmentation == null )
			return true;

		Vector3f pt1Voxel = new Vector3f(pt1);
		pt1Voxel.scale(1f/VOILatticeManagerInterface.VoxelSize);
		Vector3f pt1Voxel2 = new Vector3f(pt2);
		pt1Voxel2.scale(1f/VOILatticeManagerInterface.VoxelSize);

		Vector3f dir = Vector3f.sub(pt1Voxel2, pt1Voxel);
		float length = dir.normalize();
		Vector3f startPt = new Vector3f(pt1Voxel);
		startPt.add(dir);
		for ( int i = 0; i < length; i++ )
		{
			int x = (int) startPt.X;
			int y = (int) startPt.Y;
			int z = (int) startPt.Z;
			if ( skinSegmentation.getFloat(x, y, z) != 0 )
			{
				return false;
			}
			startPt.add(dir);
		}
		return true;
	}

	private boolean checkSeam( Vector3f pt1, Vector3f pt2 )
	{
		if ( seamSegmentation == null )
			return true;

		Vector3f ptVoxel1 = new Vector3f(pt1);
		ptVoxel1.scale(1f/VOILatticeManagerInterface.VoxelSize);
		Vector3f ptVoxel2 = new Vector3f(pt2);
		ptVoxel2.scale(1f/VOILatticeManagerInterface.VoxelSize);

		int x = (int) ptVoxel1.X;
		int y = (int) ptVoxel1.Y;
		int z = (int) ptVoxel1.Z;
		int seamID1 = seamSegmentation.getInt(x, y, z);
		x = (int) ptVoxel2.X;
		y = (int) ptVoxel2.Y;
		z = (int) ptVoxel2.Z;
		int seamID2 = seamSegmentation.getInt(x, y, z);
		Vector3f dir = Vector3f.sub(ptVoxel2, ptVoxel1);
		float length = dir.normalize();
		Vector3f startPt = new Vector3f(ptVoxel1);
		startPt.add(dir);
		for ( int i = 0; i < length; i++ )
		{
			x = (int) startPt.X;
			y = (int) startPt.Y;
			z = (int) startPt.Z;
			int id = seamSegmentation.getInt(x, y, z);
			if ( (id != 0) && (id != seamID1) && (id != seamID2) && seamCellIds.contains(id) )
			{
				//				System.err.println( seamID1 + " " + seamID2 + " " + id);
				return false;
			}
			startPt.add(dir);
		}
		return true;
	}

	private boolean checkOrigin( Vector3f pt1, Vector3f pt2, Vector3f origin )
	{
		if ( origin == null )
		{
			return true;
		}

		// calculate the mid point:
		Vector3f midPt = Vector3f.add(pt1, pt2);
		midPt.scale(0.5f);
		// calculate the distance between the two seam cells and scale:
		float distance = pt1.distance(pt2);
		distance *= midPointTestScale;		
		if ( midPt.distance(origin) < distance )
		{
			// Fails to pass the mid-point test and 
			// can be ruled out as a potential pair:
			return false;
		}
		return true;
	}

	Vector<Integer> seamCellIds;
	public void setSeamIDs(Vector<Vector3f> seamCells)
	{
		if ( seamSegmentation == null )
		{
			return;
		}
		if ( seamCellIds != null )
		{
			seamCellIds = null;
		}
		seamCellIds = new Vector<Integer>();
		for ( int i = 0; i < seamCells.size(); i++ )
		{
			Vector3f pt = seamCells.elementAt(i);

			int id = seamSegmentation.getInt( (int)pt.X, (int)pt.Y, (int)pt.Z );
			if ( id != 0 )
			{
				if ( !seamCellIds.contains(id) )
				{
					seamCellIds.add(id);
				}
			}
		}
	}

	private void savePairs( Vector<int[]> pairLists, Vector<Vector3f> positions )
	{
		System.err.println( "" );
		VOI pairs = new VOI( (short)0, "all pairs", VOI.POLYLINE, 0);
		for ( int i = 0; i < pairLists.size(); i++ )
		{
			int[] pair = pairLists.elementAt(i);
			System.err.println( (pair[0]+1) + "   " + (pair[1]+1) );
			Vector3f pt1 = new Vector3f(positions.elementAt(pair[0]));
			Vector3f pt2 = new Vector3f(positions.elementAt(pair[1]));
			pt1.scale(1f/VOILatticeManagerInterface.VoxelSize);
			pt2.scale(1f/VOILatticeManagerInterface.VoxelSize);

			VOIContour link = new VOIContour(false);
			link.add(pt1);
			link.add(pt2);
			pairs.getCurves().add(link);
		}

		ModelImage wormClone = (ModelImage)wormImage.clone();
		wormClone.unregisterAllVOIs();
		wormClone.registerVOI(pairs);
		wormClone.calcMinMax();
		new ViewJFrameImage(wormClone);

		System.err.println( "Num pairs " + pairLists.size() );
	}

	public boolean findTenthPair( ModelImage image, Vector<Vector3f> seamCells )
	{
		wormImage = image;

		/** Attempt to find the tenth pair of seam cells in the lattice. 
		 * The 10th pair is distinct in that it has the smallest between-cell distance */
		Vector<int[]> tenthPairs = new Vector<int[]>();
		Vector<Vector3f> positions = new Vector<Vector3f>();
		for ( int i = 0; i < seamCells.size(); i++ )
		{
			Vector3f pos = new Vector3f(seamCells.elementAt(i));
			pos.scale( VOILatticeManagerInterface.VoxelSize );
			positions.add( pos );
		}		

		// look for potential 10 pair:
		float maxDist = -1;
		int count = 0;
		float tempMin = tenMinDist;
		float tempMax = tenMaxDist;
		//		while ( (count == 0) && (tempMin > 0.5) )
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
			//			if ( count == 0 )
			//			{
			//				tempMin -= 0.1;
			//				tempMax += 0.1;
			//			}
		}

		// Given a set of potential tenth pairs, remove any that fail the mid-point test:
		//		System.err.println( tenthPairs.size() + " " + tempMin + " " + tempMax );
		//		if ( tenthPairs.size() > 1 )
		{
			for ( int i = tenthPairs.size() - 1; i >= 0; i-- )
			{
				if ( midPointFail( tenthPairs.elementAt(i), positions ) )
				{
					tenthPairs.remove(i);
				}
				else
				{
					int[] pair = tenthPairs.elementAt(i);
					int index1 = pair[0];
					int index2 = pair[1];
					Vector3f pt1 = positions.elementAt(index1);
					Vector3f pt2 = positions.elementAt(index2);
					if ( !checkSurface(pt1, pt2) )
					{
						tenthPairs.remove(i);
					}
					else if ( !checkSeam(pt1, pt2) )
					{
						tenthPairs.remove(i);
					}
				}
			}
		}


		//		float minIntensity = Float.MAX_VALUE;
		//		int minIndex = -1;
		//		for ( int i = 0; i < tenthPairs.size(); i++ )
		//		{
		//			int[] pair = tenthPairs.elementAt(i);
		////			System.err.println( "     Tenth pair = [" + i + "]   =   [" + pair[0] + "," + pair[1] + "]" );
		//			int index1 = pair[0];
		//			int index2 = pair[1];
		//			Vector3f pt1 = positions.elementAt(index1);
		//			Vector3f pt2 = positions.elementAt(index2);
		//
		//			Vector3f pt1Voxel = new Vector3f(pt1);
		//			pt1Voxel.scale(1f/VOILatticeManagerInterface.VoxelSize);
		//			Vector3f pt1Voxel2 = new Vector3f(pt2);
		//			pt1Voxel2.scale(1f/VOILatticeManagerInterface.VoxelSize);
		//			
		//			
		//			int x = (int) pt1Voxel.X;
		//			int y = (int) pt1Voxel.Y;
		//			int z = (int) pt1Voxel.Z;
		//			float value1 = image.getFloat(x,y,z);
		//			x = (int) pt1Voxel2.X;
		//			y = (int) pt1Voxel2.Y;
		//			z = (int) pt1Voxel2.Z;
		//			float value2 = image.getFloat(x,y,z);
		////			System.err.println( "10th " + (index1 + 1) + " " + (index2 + 1) + " " + value1 + " " + value2 );
		//			
		//			if ( (value1 + value2) < minIntensity )
		//			{
		//				minIntensity = (value1 + value2);
		//				minIndex = i;
		//			}
		//		}
		//
		//		if ( minIndex != -1 )
		//		{
		//			int[] pair = tenthPairs.elementAt(minIndex);
		//			tenthPairs.removeAllElements();
		//			tenthPairs.add(pair);
		//			
		////			System.err.println( "     Tenth pair = [" + pair[0] + "," + pair[1] + "]" );
		//		}


		return (tenthPairs.size() > 0);
	}

}
