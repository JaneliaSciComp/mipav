package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import java.io.File;
import java.util.Arrays;
import java.util.Vector;

import javax.swing.JProgressBar;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;

public class LatticeBuilder {
	
	public LatticeBuilder() {}

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
	public static final float tenMinDist = 1;
	public static final float tenMaxDist = 5;  
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

	private Vector<int []> testSequence = new Vector<int[]>();
	
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
	public boolean buildLattice( JProgressBar batchProgress, int imageCount, int numImages, ModelImage image, VOI annotations, VOI nose, String outputDir )
	{
		wormImage = image;
		int numSteps = 3;
		int step = 1;
		long startTime = System.currentTimeMillis();
		boolean print = true;
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
		
		System.err.println( positions.size() );

		// 100
//		testSequence.add( new int[]{5,7} );
//		testSequence.add( new int[]{14,13} );
//		testSequence.add( new int[]{15,16} );
//		testSequence.add( new int[]{9,11} );
//		testSequence.add( new int[]{2,3} );
//		testSequence.add( new int[]{1,0} );
//		testSequence.add( new int[]{6,4} );
//		testSequence.add( new int[]{12,10} );
//		testSequence.add( new int[]{19,20} );
//		testSequence.add( new int[]{18,21} );
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(positions.elementAt(5));
//		left.add(positions.elementAt(14));
//		left.add(positions.elementAt(16));
//		left.add(positions.elementAt(9));
//		left.add(positions.elementAt(2));
//		left.add(positions.elementAt(1));
//		left.add(positions.elementAt(6));
//		left.add(positions.elementAt(12));
//		left.add(positions.elementAt(19));
//		left.add(positions.elementAt(18));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(positions.elementAt(7));
//		right.add(positions.elementAt(13));
//		right.add(positions.elementAt(16));
//		right.add(positions.elementAt(11));
//		right.add(positions.elementAt(3));
//		right.add(positions.elementAt(0));
//		right.add(positions.elementAt(4));
//		right.add(positions.elementAt(10));
//		right.add(positions.elementAt(20));
//		right.add(positions.elementAt(21));
		
		
		// 40
		testSequence.add( new int[]{12,14} );
		testSequence.add( new int[]{4,6} );
		testSequence.add( new int[]{0,1} );
		testSequence.add( new int[]{3,2} );
		testSequence.add( new int[]{8,7} );
		testSequence.add( new int[]{11,10} );
		testSequence.add( new int[]{16,17} );
		testSequence.add( new int[]{19,20} );
		testSequence.add( new int[]{13,15} );
		testSequence.add( new int[]{5,9} );
//		
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(positions.elementAt(14));
//		left.add(positions.elementAt(6));
//		left.add(positions.elementAt(1));
//		left.add(positions.elementAt(2));
//		left.add(positions.elementAt(7));
//		left.add(positions.elementAt(10));
//		left.add(positions.elementAt(17));
//		left.add(positions.elementAt(20));
//		left.add(positions.elementAt(15));
//		left.add(positions.elementAt(9));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(positions.elementAt(12));
//		right.add(positions.elementAt(4));
//		right.add(positions.elementAt(0));
//		right.add(positions.elementAt(3));
//		right.add(positions.elementAt(8));
//		right.add(positions.elementAt(11));
//		right.add(positions.elementAt(16));
//		right.add(positions.elementAt(19));
//		right.add(positions.elementAt(13));
//		right.add(positions.elementAt(5));

		// 41
//		testSequence.add( new int[]{7,5} );
//		testSequence.add( new int[]{12,13} );
//		testSequence.add( new int[]{14,17} );
//		testSequence.add( new int[]{10,11} );
//		testSequence.add( new int[]{4,6} );
//		testSequence.add( new int[]{0,1} );
//		testSequence.add( new int[]{2,3} );
//		testSequence.add( new int[]{9,8} );
//		testSequence.add( new int[]{16,15} );
//		testSequence.add( new int[]{18,19} );
//		
//		Vector<Vector3f> left = new Vector<Vector3f>();
//		left.add(positions.elementAt(7));
//		left.add(positions.elementAt(12));
//		left.add(positions.elementAt(14));
//		left.add(positions.elementAt(10));
//		left.add(positions.elementAt(4));
//		left.add(positions.elementAt(0));
//		left.add(positions.elementAt(2));
//		left.add(positions.elementAt(9));
//		left.add(positions.elementAt(16));
//		left.add(positions.elementAt(18));
//		
//		Vector<Vector3f> right = new Vector<Vector3f>();
//		right.add(positions.elementAt(5));
//		right.add(positions.elementAt(13));
//		right.add(positions.elementAt(17));
//		right.add(positions.elementAt(11));
//		right.add(positions.elementAt(6));
//		right.add(positions.elementAt(1));
//		right.add(positions.elementAt(3));
//		right.add(positions.elementAt(8));
//		right.add(positions.elementAt(15));
//		right.add(positions.elementAt(19));

//		testLattice(left, right, null);
		
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
		


		if ( print ) System.err.println( "10th pair " + tenthPairs.size() );
		float minIntensity = Float.MAX_VALUE;
		int minIndex = -1;
		for ( int i = 0; i < tenthPairs.size(); i++ )
		{
			int[] pair = tenthPairs.elementAt(i);
			int index1 = pair[0];
			int index2 = pair[1];
			Vector3f pt1 = positions.elementAt(index1);
			Vector3f pt2 = positions.elementAt(index2);

			Vector3f pt1Voxel = new Vector3f(pt1);
			pt1Voxel.scale(1f/VOILatticeManagerInterface.VoxelSize);
			Vector3f pt1Voxel2 = new Vector3f(pt2);
			pt1Voxel2.scale(1f/VOILatticeManagerInterface.VoxelSize);
			
			
			int x = (int) pt1Voxel.X;
			int y = (int) pt1Voxel.Y;
			int z = (int) pt1Voxel.Z;
			float value1 = image.getFloat(x,y,z);
			x = (int) pt1Voxel2.X;
			y = (int) pt1Voxel2.Y;
			z = (int) pt1Voxel2.Z;
			float value2 = image.getFloat(x,y,z);
			if ( print ) System.err.println( "10th " + (index1 + 1) + " " + (index2 + 1) + " " + value1 + " " + value2 );
			
			if ( (value1 + value2) < minIntensity )
			{
				minIntensity = (value1 + value2);
				minIndex = i;
			}
		}

		if ( minIndex != -1 )
		{
			int[] pair = tenthPairs.elementAt(minIndex);
			tenthPairs.removeAllElements();
			tenthPairs.add(pair);
			
			System.err.println( "Tenth pair = [" + pair[0] + "," + pair[1] + "]" );
		}
		
		
		
		
//		if ( print ) System.err.println( tenthPairs.size() );
		if ( batchProgress != null )
		{
			batchProgress.setValue((int)(100 * (float)(imageCount*numSteps + step++)/(numSteps*numImages)));
			batchProgress.update(batchProgress.getGraphics());
		}
		
		LatticeModel model = new LatticeModel( image );
		int[] total = new int[]{0};
		int[] max = new int[]{0};
		Vector<Vector<int[]>> sequenceList = new Vector<Vector<int[]>>();

		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			Vector3f pos = text.elementAt(0);
			int x = (int)pos.X;
			int y = (int)pos.Y;
			int z = (int)pos.Z;
			float val = wormImage.getFloat(x,y,z);
			System.err.println( i + "   " + val );
		}
		
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
							
							System.err.print( "Pair " + j + " " + k );
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
											pairs[j][k] = 1;
											countJ++;
											
											System.err.println(" added" );
										}
										else
										{
											System.err.println(" checkSeam fail" );
										}
									}
									else
									{
										System.err.println(" checkSurface fail" );
									}
								}
								else
								{
									System.err.println(" midPointFail fail" );
								}
							}
							else
							{
								System.err.println(" distance fail " + distance + "  " + (distance >= minPairDist) + "  " + (distance <= maxPairDist) );
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
						
						System.err.println( pairLists.size() + "   [" + j + "," + k + "]" );
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
//			if ( print ) System.err.println( pairLists.size() );
			
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
		if ( print ) System.err.println( "buildLattice time 1 = " + AlgorithmBase.computeElapsedTime(startTime) + " " + sequenceList.size() );
		long startTime2 = System.currentTimeMillis();
//		System.err.println( sequenceList.size() + " " + max[0] );
		VOIVector finalLatticeList = new VOIVector();
		// Order the sequences based on how well they fit the lattice parameters:
		
		sequenceList.add(0, testSequence);
		orderSequences( startTime2, image, model, nosePts, positions, sequenceList, finalLatticeList );

		if ( batchProgress != null )
		{
			batchProgress.setValue((int)(100 * (float)(imageCount*numSteps + step++)/(numSteps*numImages)));
			batchProgress.update(batchProgress.getGraphics());
		}
		if ( print ) System.err.println( finalLatticeList.size() );
		// Save the top 5 lattices found for the user to select the best:
		for ( int j = 0; j < Math.min( 5, finalLatticeList.size()); j++ )
//			for ( int j = 0; j < finalLatticeList.size(); j++ )
		{
			image.unregisterAllVOIs();
			VOI lattice = finalLatticeList.elementAt(j);
			image.registerVOI(lattice);
			String fileName = outputDir + File.separator + autoLatticeGenerationOutput + (j+1) + File.separator;
			File outputFileDir = new File(fileName);
			if ( !outputFileDir.exists() )
			{
				outputFileDir.mkdir();
			}

			LatticeModel.saveAllVOIsTo(fileName, image);
		}

		if ( print ) System.err.println( "done buildLattice = " + AlgorithmBase.computeElapsedTime(startTime) );
		
		model.dispose();
		model = null;
		return (finalLatticeList.size() > 0);
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
	private boolean midPointFail( int[] pair, Vector<Vector3f> positions )
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
	private void orderSequences( long startTime, ModelImage image, LatticeModel model, Vector<Vector3f> nose, Vector<Vector3f> positions, Vector<Vector<int[]>> sequenceList, VOIVector finalLatticeList )
	{
		if ( sequenceList.size() <= 0 )
		{
			return;
		}

		model.setSeamCellImage(seamSegmentation);	
		
		VOIVector lattices = new VOIVector();
		Vector2d[] latticeImageCounts = new Vector2d[sequenceList.size()];
		for ( int i = 0; i < sequenceList.size(); i++ )
		{
			Vector<int[]>sequence = sequenceList.elementAt(i); 
			
			VOIContour left = new VOIContour( false );
			VOIContour right = new VOIContour( false );
			VOI lattice = new VOI((short) i, "lattice", 1, VOI.POLYLINE );
			lattice.getCurves().add(left);
			lattice.getCurves().add(right);
			for ( int j = 0; j < sequence.size(); j++ )
			{
				int[] pair = sequence.elementAt(j);
				Vector3f p1 = new Vector3f(positions.elementAt(pair[0]));
				Vector3f p2 = new Vector3f(positions.elementAt(pair[1]));
				p1.scale(1f/VOILatticeManagerInterface.VoxelSize);
				p2.scale(1f/VOILatticeManagerInterface.VoxelSize);
				left.add(0, p1);
				right.add(0, p2);
			}
			lattices.add(lattice);
			
			model.setLattice(lattice);		
			float[] avgVals = model.testLatticeImage();
			latticeImageCounts[i] = new Vector2d( avgVals[0], i );
		}
				
		Arrays.sort(latticeImageCounts);
		for ( int i = latticeImageCounts.length -1; i >= 0; i-- )
		{
			if ( latticeImageCounts[i].X != -1 )
			{
				VOI lattice = lattices.elementAt((int)latticeImageCounts[i].Y);
				model.setLattice(lattice);
				float[] length2 = new float[1];
				float[] curvature = new float[1];
				float[] intersectionCount = new float[1];
				model.testLatticeConflicts(length2, curvature, intersectionCount);
				if ( intersectionCount[0] < .2 )
				{
					finalLatticeList.add(lattice);
				}
				
				System.err.println( latticeImageCounts[i].X + "   curve vals: " + latticeImageCounts[i].Y + "     " +  intersectionCount[0] );
			}
			
			if ( finalLatticeList.size() >= 5 )
			{
				return;
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
	private void sequencePairs( long startTime, Vector<Vector3f> nose, Vector<Vector3f> positions, Vector<int[]> sequence, Vector<int[]> pairs, int[] lastPair, int count, int[] total, int[] max, Vector<Vector<int[]>> sequenceList )
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
//		double elapsedTime = AlgorithmBase.computeElapsedTime(startTime);
//		if ( elapsedTime > sequenceTimeLimit )
//		{
//			// if too much time has elapsed, return:
//			System.err.println( "too much time" );
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
			

			boolean path1 = true;
			float min = L00;
			if ( L11 < min )
			{
				min = L11;
			}
			if ( L01 < min )
			{
				min = L01;
				path1 = false;
			}
			if ( L10 < min )
			{
				min = L10;
				path1 = false;
			}
			
			if ( path1 )
//			if ( angle1 < sequenceTwistLimit )
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
			else
//			if ( angle2 < sequenceTwistLimit )
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


	/**
	 * Checks the lattice based on observations of the embryonic worm and compares the lattice to threshold values.
	 * Return true if the lattice fits within bounds.
	 * @param left the positions of the lattice along the left-edge
	 * @param right the positions of the lattice along the right-edge
	 * @param nose nose point of the worm (may be empty)
	 * @return true if the lattice fits within threshold, false otherwise.
	 */
	private boolean testLattice( Vector<Vector3f> left, Vector<Vector3f> right, Vector<Vector3f> nose )
	{
		// Left and right sides must contain the same number of points:
		if ( (left.size() != right.size()) || (left.size() == 0) )
		{
			return false;
		}
		

//		for ( int i = 0; i < left.size()-1; i++ )
//		{
//			Vector3f ptL = left.elementAt(i);
//			Vector3f ptL1 = left.elementAt(i+1);
//			Vector3f ptR = right.elementAt(i);
//			Vector3f ptR1 = right.elementAt(i+1);
//			
//			Vector3f edge00 = Vector3f.sub( ptL, ptL1 );  float L00 = edge00.normalize();
//			Vector3f edge11 = Vector3f.sub( ptR, ptR1 );  float R00 = edge11.normalize();
//			float angle1 = edge00.angle(edge11);
//			
//			Vector3f edge00A = Vector3f.sub( ptL, ptR1 );  float L00A = edge00A.normalize();
//			Vector3f edge11A = Vector3f.sub( ptR, ptL1 );  float R00A = edge11A.normalize();
//			float angle2 = edge00A.angle(edge11A);
//			
//			float diff = Math.abs(L00 - R00);
//			
//			boolean path1 = true;
//			float min = L00;
//			if ( R00 < min )
//			{
//				min = R00;
//			}
//			if ( L00A < min )
//			{
//				min = L00A;
//				path1 = false;
//			}
//			if ( R00A < min )
//			{
//				path1 = false;
//			}
//			
//			
//			Vector3f edge1 = Vector3f.sub(ptR, ptL);      edge1.normalize();
//			Vector3f edgeA = Vector3f.sub(ptR1, ptL1);    edgeA.normalize();
//			Vector3f edgeB = Vector3f.sub(ptL1, ptR1);    edgeB.normalize();
//			
//			float angleA = edge1.angle(edgeA);
//			float angleB = edge1.angle(edgeB);
//			
//			System.err.print( diff + "  " + angle1 + "  " + angle2 + "     " + angleA + "  " + angleB + "  " );
//			
////			float path1 = L00 + R00;
////			float path2 = L00A + R00A;
//
//			Vector3f midPt = Vector3f.add(ptL, ptR);
//			midPt.scale(0.5f);
//			Vector3f midPtSequence = Vector3f.add(ptL1, ptR1);
//			midPtSequence.scale(0.5f);
//			float midDistance = midPt.distance(midPtSequence);
//
//			if ( path1 && (midDistance > minSequenceMidDist) && (midDistance < maxSequenceMidDist) && (L00 > minSequenceDist) && (L00 < maxSequenceDist) && (R00 > minSequenceDist) && (R00 < maxSequenceDist))
//			{
//				System.err.println( true );				
//			}
//			System.err.println( "" );
//		}
		
		
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
		if ( !((avgWidthFirst4 > avgWidthLast5) && (maxIndex >= 4) && (minIndex <= 4)) )
		{			
			return false;
		}
//		{
//			// Check the amount of 'bend' in the worm:
//			if ( nose != null )
//			{
//				if ( nose.size() == 1 )
//				{
//					Vector3f nosePt = new Vector3f(nose.elementAt(0));
//					left.add(nosePt);
//					right.add(nosePt);
//				}
//				else if ( nose.size() >= 2 )
//				{
//					Vector3f nosePt0 = new Vector3f(nose.elementAt(0));
//					Vector3f nosePt1 = new Vector3f(nose.elementAt(1));
//
//					float distanceL0 = left.elementAt(0).distance(nosePt0);
//					float distanceL1 = left.elementAt(0).distance(nosePt1);
//					if ( distanceL0 < distanceL1 )
//					{
//						left.add(nosePt0);
//						right.add(nosePt1);
//					}
//					else if ( distanceL0 > distanceL1 )
//					{
//						left.add(nosePt1);
//						right.add(nosePt0);					
//					}
//					else
//					{
//						float distanceR0 = right.elementAt(0).distance(nosePt0);
//						float distanceR1 = right.elementAt(0).distance(nosePt1);
//						if ( distanceR0 < distanceR1 )
//						{
//							right.add(nosePt0);
//							left.add(nosePt1);
//						}
//						else
//						{
//							right.add(nosePt1);			
//							left.add(nosePt0);		
//						}					
//					}
//				}
//			}
			float bendSum = measureCurvature(left, right);
			if ( (bendSum < sequenceBendMin) || (bendSum > sequenceBendMax) )
			{
				return false;
			}
//			
//			return true;
//		}		
				
			
			
			
			

			VOIContour leftImage = new VOIContour( false );
			VOIContour rightImage = new VOIContour( false );
			VOI lattice = new VOI((short) 0, "lattice", 1, VOI.POLYLINE );
			lattice.getCurves().add(leftImage);
			lattice.getCurves().add(rightImage);
			for ( int i = 0; i < left.size(); i++ )
			{
				Vector3f p1 = new Vector3f(left.elementAt(i));
				Vector3f p2 = new Vector3f(right.elementAt(i));
				p1.scale(1f/VOILatticeManagerInterface.VoxelSize);
				p2.scale(1f/VOILatticeManagerInterface.VoxelSize);
				leftImage.add(0, p1);
				rightImage.add(0, p2);
			}
			wormImage.unregisterAllVOIs();
			LatticeModel model = new LatticeModel(wormImage);
			model.setSeamCellImage(seamSegmentation);
			model.setLattice(lattice);
			float[] avgVals = model.testLatticeImage();
			wormImage.unregisterAllVOIs();
			if ( avgVals[1] > avgVals[0] )
			{
				return false;
			}
			
			
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
		
		Vector3f pt1Voxel = new Vector3f(pt1);
		pt1Voxel.scale(1f/VOILatticeManagerInterface.VoxelSize);
		Vector3f pt1Voxel2 = new Vector3f(pt2);
		pt1Voxel2.scale(1f/VOILatticeManagerInterface.VoxelSize);
		
		int x = (int) pt1Voxel.X;
		int y = (int) pt1Voxel.Y;
		int z = (int) pt1Voxel.Z;
		int seamID1 = seamSegmentation.getInt(x, y, z);
		x = (int) pt1Voxel2.X;
		y = (int) pt1Voxel2.Y;
		z = (int) pt1Voxel2.Z;
		int seamID2 = seamSegmentation.getInt(x, y, z);
		Vector3f dir = Vector3f.sub(pt1Voxel2, pt1Voxel);
		float length = dir.normalize();
		Vector3f startPt = new Vector3f(pt1Voxel);
		startPt.add(dir);
		for ( int i = 0; i < length; i++ )
		{
			x = (int) startPt.X;
			y = (int) startPt.Y;
			z = (int) startPt.Z;
			int id = seamSegmentation.getInt(x, y, z);
			if ( (id != 0) && (id != seamID1) && (id != seamID2) )
			{
				System.err.println( seamID1 + " " + seamID2 + " " + id);
				return false;
			}
			startPt.add(dir);
		}
		return true;
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
						tenthPairs.remove(i);
					}
					else if ( !checkSeam(pt1, pt2) )
					{
						tenthPairs.remove(i);
					}
				}
			}
		}
		

		float minIntensity = Float.MAX_VALUE;
		int minIndex = -1;
		for ( int i = 0; i < tenthPairs.size(); i++ )
		{
			int[] pair = tenthPairs.elementAt(i);
			int index1 = pair[0];
			int index2 = pair[1];
			Vector3f pt1 = positions.elementAt(index1);
			Vector3f pt2 = positions.elementAt(index2);

			Vector3f pt1Voxel = new Vector3f(pt1);
			pt1Voxel.scale(1f/VOILatticeManagerInterface.VoxelSize);
			Vector3f pt1Voxel2 = new Vector3f(pt2);
			pt1Voxel2.scale(1f/VOILatticeManagerInterface.VoxelSize);
			
			
			int x = (int) pt1Voxel.X;
			int y = (int) pt1Voxel.Y;
			int z = (int) pt1Voxel.Z;
			float value1 = image.getFloat(x,y,z);
			x = (int) pt1Voxel2.X;
			y = (int) pt1Voxel2.Y;
			z = (int) pt1Voxel2.Z;
			float value2 = image.getFloat(x,y,z);
			System.err.println( "10th " + (index1 + 1) + " " + (index2 + 1) + " " + value1 + " " + value2 );
			
			if ( (value1 + value2) < minIntensity )
			{
				minIntensity = (value1 + value2);
				minIndex = i;
			}
		}

		if ( minIndex != -1 )
		{
			int[] pair = tenthPairs.elementAt(minIndex);
			tenthPairs.removeAllElements();
			tenthPairs.add(pair);
			
			System.err.println( "Tenth pair = [" + pair[0] + "," + pair[1] + "]" );
		}
		
		
		return (tenthPairs.size() > 0);
	}
	
}
